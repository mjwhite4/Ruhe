#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <string.h>
#include <unistd.h>

#define CAPACITY 8192

static char buffer[CAPACITY];
static size_t used = 0;
static int eof_seen = 0;
static int saved_flags = -1;

enum {
    READ_COMPLETE_LINE   =  1,
    READ_NO_LINE =  0,
    READ_EOF             = -1,
    READ_IOS_ERROR       = -2,
    READ_OVERFLOW        = -3
};

/* Enable nonblocking stdin. Call before any stdin reading. */
int uci_input_unit(void)
{
    if (saved_flags != -1)
        return READ_NO_LINE;

    int flags = fcntl(STDIN_FILENO, F_GETFL);
    if (flags == -1)
        return READ_EOF;

    if (fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK) == -1)
        return READ_EOF;

    saved_flags = flags;
    used = 0;
    eof_seen = 0;
    return READ_NO_LINE;
}

/* Restore stdin flags before normal program exit. */
int uci_input_close(void)
{
    if (saved_flags == -1)
        return READ_NO_LINE;

    if (fcntl(STDIN_FILENO, F_SETFL, saved_flags) == -1)
        return READ_EOF;

    saved_flags = -1;
    return READ_NO_LINE;
}

/*
 * Return values:
 *   1 : complete line returned
 *   0 : no complete line available yet
 *  -1 : EOF, with no buffered input remaining
 *  -2 : input/system error
 *  -3 : command too long
 *
 * out receives bytes, without a newline or terminating NUL.
 * length receives the number of bytes returned.
 */
int uci_input_next(char *out, int capacity, int *length)
{
    *length = 0;

    for (;;) {
        char *newline = memchr(buffer, '\n', used);

        if (newline != NULL || (eof_seen && used > 0)) {
            size_t consumed = newline != NULL
                ? (size_t)(newline - buffer) + 1
                : used;

            size_t n = newline != NULL ? consumed - 1 : consumed;

            /* Accept Windows-style CRLF too. */
            if (n > 0 && buffer[n - 1] == '\r')
                --n;

            if (n > (size_t)capacity)
                return READ_OVERFLOW;

            memcpy(out, buffer, n);
            memmove(buffer, buffer + consumed, used - consumed);
            used -= consumed;

            *length = (int)n;
            return READ_COMPLETE_LINE;
        }

        if (eof_seen)
            return READ_EOF;

        if (used == sizeof buffer)
            return READ_OVERFLOW;

        ssize_t n = read(STDIN_FILENO,
                         buffer + used,
                         sizeof buffer - used);

        if (n > 0) {
            used += (size_t)n;
            continue;
        }

        if (n == 0) {
            eof_seen = 1;
            continue;
        }

        if (errno == EINTR ||
            errno == EAGAIN || 
            errno == EWOULDBLOCK
           )
            return READ_NO_LINE;  /* Retry on a later call. */

        return READ_IOS_ERROR;
    }
}

/* Used only when idle, to avoid spinning while waiting for commands. */
int uci_input_wait(int milliseconds)
{
    struct pollfd pfd = {STDIN_FILENO, POLLIN, 0};
    int rc = poll(&pfd, 1, milliseconds);

    if (rc < 0 && errno == EINTR)
        return READ_NO_LINE;

    return rc;
}
