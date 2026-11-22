#!/usr/bin/env python3
"""Cumulative UCI smoke tests; standard library only (Linux and macOS).

Usage: python3 test/test_uci.py ./ruhe-debug [--verbose] [--timeout 10]
Each case owns a fresh process. Search move legality remains the responsibility
of the internal tests and the separate integration suite.
"""
import argparse
import os
from pathlib import Path
import queue
import re
import subprocess
import sys
import threading
import time

GREEN = "\033[32m"
RED = "\033[31m"
RESET = "\033[0m"

class Engine:
    def __init__(self, path, timeout=10.0, verbose=False):
        self.timeout = timeout
        self.verbose = verbose
        self.responses = queue.Queue()
        self.lines = []
        self.expected = []
        env = os.environ.copy()
        env['GFORTRAN_UNBUFFERED_PRECONNECTED'] = 'y'
        self.process = subprocess.Popen(
            [path], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=None, text=True, encoding='utf-8', errors='replace',
            bufsize=1, env=env)
        self.reader = threading.Thread(target=self._read, daemon=True)
        self.reader.start()

    def close_input_and_wait(self, allow_bestmove=False):
        """Require clean exit within timeout after closing engine stdin."""
        started = time.monotonic()
        deadline = started + self.timeout

        self.process.stdin.close()

        try:
            status = self.process.wait(
                timeout=max(0.001, deadline - time.monotonic())
            )
        except subprocess.TimeoutExpired:
            raise RuntimeError("engine did not exit after stdin EOF")

        self.reader.join(
            timeout=max(0.0, deadline - time.monotonic())
        )
        if self.reader.is_alive():
            raise RuntimeError("stdout did not close after stdin EOF")
        if status != 0:
            raise RuntimeError(f"EOF shutdown returned status {status}")

        # All output is now collected. Preserve strict duplicate detection.
        actual = [
            line.strip() for line in self.lines
            if self._is_reply(line.strip())
        ]
        if actual[:len(self.expected)] != self.expected:
            raise RuntimeError("protocol reply mismatch before EOF")

        extra = actual[len(self.expected):]
        if extra:
            if not allow_bestmove or len(extra) != 1:
                raise RuntimeError(f"unexpected replies after EOF: {extra!r}")
            if not re.fullmatch(
                r"bestmove [a-h][1-8][a-h][1-8][qrbn]?"
                r"(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?",
                extra[0],
            ):
                raise RuntimeError(f"invalid final bestmove: {extra[0]!r}")
            self.expected.extend(extra)

        if self.verbose:
            print(f"  EOF shutdown: {time.monotonic() - started:.3f}s")

    def _read(self):
        try:
            for line in self.process.stdout:
                line = line.rstrip('\r\n')
                self.lines.append(line)
                self.responses.put(line)
        finally:
            self.responses.put(None)

    def __enter__(self):
        return self

    def __exit__(self, kind, value, traceback):
        try:
            if kind is None:

                # prevent sending quit after eof test closed stdin
                if not self.process.stdin.closed:
                    self.send('quit')
                try:
                    status = self.process.wait(timeout=self.timeout)
                except subprocess.TimeoutExpired:
                    raise RuntimeError('engine did not exit after quit')
                self.reader.join(timeout=self.timeout)
                if self.reader.is_alive():
                    raise RuntimeError('stdout reader did not reach EOF')
                if status != 0:
                    raise RuntimeError(f'engine exited with status {status}')
                actual = [line.strip() for line in self.lines
                          if self._is_reply(line.strip())]
                if actual != self.expected:
                    raise RuntimeError(f'extra/missing protocol replies: expected '
                                       f'{self.expected!r}, received {actual!r}')
        finally:
            if self.process.poll() is None:
                self.process.terminate()
                try:
                    self.process.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    self.process.kill()
                    self.process.wait(timeout=2)
            self.reader.join(timeout=2)
            self.process.stdin.close()
            if not self.reader.is_alive():
                self.process.stdout.close()
        return False

    @staticmethod
    def _is_reply(line):
        return line in ('uciok', 'readyok') or line.split()[:1] == ['bestmove']

    def write(self, text):
        if self.verbose:
            print(f'  --> {text!r}')
        self.process.stdin.write(text)
        self.process.stdin.flush()

    def send(self, command):
        self.write(command + '\n')

    def expect(self, wanted, deadline=None):
        if deadline is None:
            deadline = time.monotonic() + self.timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise RuntimeError(f'timeout waiting for {wanted}')
            try:
                line = self.responses.get(timeout=remaining)
            except queue.Empty:
                raise RuntimeError(f'timeout waiting for {wanted}')
            if line is None:
                raise RuntimeError(f'EOF while waiting for {wanted}')
            line = line.strip()
            if self.verbose:
                print(f'  <-- {line}')
            if not self._is_reply(line):
                continue  # Identification, info, and diagnostic lines.
            matches = (line.startswith('bestmove ') if wanted == 'bestmove'
                       else line == wanted)
            if not matches:
                raise RuntimeError(f'expected {wanted}, received {line!r}')
            self.expected.append(line)
            return line

    def expect_rejection(self):
        deadline = time.monotonic() + self.timeout

        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise RuntimeError("timeout waiting for go rejection")

            try:
                line = self.responses.get(timeout=remaining)
            except queue.Empty:
                raise RuntimeError("timeout waiting for go rejection")

            if line is None:
                raise RuntimeError("EOF while waiting for go rejection")

            line = line.strip()
            if self.verbose:
                print(f"  <-- {line}")

            if line.startswith("info string rejected go command;"):
                return

            if self._is_reply(line):
                raise RuntimeError(
                    f"expected go rejection before protocol reply: {line!r}"
                )

    def handshake(self):
        self.send('uci')
        self.expect('uciok')
        self.send('isready')
        self.expect('readyok')

    def no_reply(self, duration):
        """Observe a bounded window before completing a fragmented command."""
        deadline = time.monotonic() + duration
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return
            try:
                line = self.responses.get(timeout=remaining)
            except queue.Empty:
                return
            if line is None:
                raise RuntimeError('EOF before fragmented command was completed')
            if self.verbose:
                print(f'  <-- {line}')
            if self._is_reply(line.strip()):
                raise RuntimeError(f'protocol reply before newline: {line!r}')


def test_basic_handshake(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()


def test_batched_commands(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.write('uci\nisready\nisready\n')
        deadline = time.monotonic() + engine.timeout
        for reply in ('uciok', 'readyok', 'readyok'):
            engine.expect(reply, deadline)


def test_fragmented_command(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.write('isrea')
        engine.no_reply(0.2)
        engine.write('dy\n')
        engine.expect('readyok')
        # Confirm continued operation after reconstructing the command.
        engine.send('isready')
        engine.expect('readyok')


def test_crlf_input(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.write('uci\r\nisready\r\n')
        engine.expect('uciok')
        engine.expect('readyok')


def test_eof_idle(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.close_input_and_wait()


def test_eof_during_search(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.send("position startpos")

        # go inifinite avoids the search completing naturally before eof.
        engine.send("go infinite")

        # readiness confirms that the engine is responding before close
        engine.send("isready")
        engine.expect("readyok")

        # allow zero or one final best move satisfies shutdown or move reply
        engine.close_input_and_wait(allow_bestmove=True)


def test_position_and_search(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.send('ucinewgame')
        engine.send('isready')
        engine.expect('readyok')
        engine.send('position startpos moves e2e4 e7e5')
        engine.send('isready')
        engine.expect('readyok')
        engine.send('go depth 3')
        reply = engine.expect('bestmove')
        if not re.fullmatch(r'bestmove [a-h][1-8][a-h][1-8][qrbn]?'
                            r'(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?', reply):
            raise RuntimeError(f'invalid bestmove syntax for nonterminal root: {reply!r}')


def test_unknown_command_recovery(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()

        engine.send("ruhe_unknown_command")
        engine.send("isready")
        engine.expect("readyok")

        engine.send("position startpos")
        engine.send("go depth 3")
        reply = engine.expect("bestmove")

        if not re.fullmatch(
            r"bestmove [a-h][1-8][a-h][1-8][qrbn]?"
            r"(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?",
            reply,
        ):
            raise RuntimeError(f"invalid bestmove after unknown command: {reply!r}")


def test_combined_search_limits(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.send("position startpos")
        engine.send("go depth 20 nodes 1")
        reply = engine.expect("bestmove")

        if not re.fullmatch(
            r"bestmove [a-h][1-8][a-h][1-8][qrbn]?"
            r"(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?",
            reply,
        ):
            raise RuntimeError(f"invalid fallback bestmove: {reply!r}")

        # A fresh search must clear the previous interruption and budget.
        engine.send("isready")
        engine.expect("readyok")

        # record the output position
        start = len(engine.lines)

        engine.send("go depth 3")
        reply = engine.expect("bestmove")

        if not re.fullmatch(
            r"bestmove [a-h][1-8][a-h][1-8][qrbn]?"
            r"(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?",
            reply,
        ):
            raise RuntimeError(f"invalid bestmove after limited search: {reply!r}")

        # prove that depth 3 completes, by examining info depth outputs
        completed_depths = []

        for line in engine.lines[start:]:
            match = re.match(r"^info depth (\d+)(?:\s|$)", line.strip())
            if match:
                completed_depths.append(int(match.group(1)))

        if 3 not in completed_depths:
            raise RuntimeError(
                "search after node exhaustion did not report completed depth 3"
            )


def test_combined_limits_depth_first(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.send("position startpos")

        start = len(engine.lines)
        engine.send("go depth 2 nodes 1000000")
        reply = engine.expect("bestmove")

        if not re.fullmatch(
            r"bestmove [a-h][1-8][a-h][1-8][qrbn]?"
            r"(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?",
            reply,
        ):
            raise RuntimeError(f"invalid bestmove: {reply!r}")

        depths = []
        for line in engine.lines[start:]:
            match = re.match(
                r"^info depth (\d+)(?:\s|$)", line.strip()
            )
            if match:
                depths.append(int(match.group(1)))

        if not depths or max(depths) != 2:
            raise RuntimeError(
                f"expected completed depth 2, received depths {depths!r}"
            )


def test_rejected_go_recovery(engine_path, **options):
    with Engine(engine_path, **options) as engine:
        engine.handshake()
        engine.send("position startpos")

        for command in (
                "go depth 20 nodes 0", 
                "go ruhe_unknown_parameter",
                "go depth", 
                "go nodes", 
                "go depth abc", 
                "go nodes abc", 
                ):
            engine.send(command)
            engine.send("isready")
            engine.expect_rejection()
            engine.expect("readyok")

        # record the output position
        start = len(engine.lines)

        engine.send("go depth 3")
        reply = engine.expect("bestmove")

        if not re.fullmatch(
            r"bestmove [a-h][1-8][a-h][1-8][qrbn]?"
            r"(?: ponder [a-h][1-8][a-h][1-8][qrbn]?)?",
            reply,
        ):
            raise RuntimeError(f"invalid bestmove after rejected go commands: {reply!r}")

        # prove that depth 3 completes, by examining info depth outputs
        completed_depths = []

        for line in engine.lines[start:]:
            match = re.match(r"^info depth (\d+)(?:\s|$)", line.strip())
            if match:
                completed_depths.append(int(match.group(1)))

        if 3 not in completed_depths:
            raise RuntimeError(
                "search after rejected go commands did not report completed depth 3"
            )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('engine', nargs='?', default='./ruhe')
    parser.add_argument('--timeout', type=float, default=10.0,
                        help='response/shutdown timeout in seconds (default: 10)')
    parser.add_argument('--verbose', action='store_true')
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error('--timeout must be positive')
    path = str(Path(args.engine).expanduser().resolve())
    if not os.path.isfile(path) or not os.access(path, os.X_OK):
        parser.error(f'engine is missing or not executable: {path}')
    failures = 0
    tests = (test_basic_handshake, test_batched_commands, test_crlf_input,
             test_eof_idle, test_eof_during_search, test_fragmented_command,
             test_position_and_search, test_unknown_command_recovery,
             test_combined_search_limits, test_combined_limits_depth_first,
             test_rejected_go_recovery)
    for test in tests:
        try:
            test(path, timeout=args.timeout, verbose=args.verbose)
        except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
            failures += 1
            print(f'{RED}[FAIL]{RESET} {test.__name__}: {exc}', flush=True)
        else:
            print(f'{GREEN}[PASS]{RESET} {test.__name__}', flush=True)
    print(f'{len(tests) - failures}/{len(tests)} tests passed')
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())
