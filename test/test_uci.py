#!/usr/bin/env python3

import os
import subprocess
import time
import sys
import select

import queue
import threading


def test_uci_engine(engine_path):

    env = os.environ.copy()
    env["GFORTRAN_UNBUFFERED_PRECONNECTED"] = "y"

    # Launch engine process with unbuffered I/O
    process = subprocess.Popen(
        [engine_path],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=None,
        text=True,
        bufsize=1,
        env=env
    )

    responses = queue.Queue()

    def read_stdout(pipe, q):
        for line in pipe:
            q.put(line.rstrip("\r\n"))
        q.put(None) # EOF


    reader_thread = threading.Thread(
        target=read_stdout,
        args=(process.stdout, responses),
        daemon=True
    )
    reader_thread.start()


    def send_command(cmd, wait_time=0.1, timeout=1.0):
        print(f"\n---> Sent: {cmd}")
        process.stdin.write(f"{cmd}\n")
        process.stdin.flush()

        if cmd.strip() == "quit":
            try:
                status = process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                raise RuntimeError("Engine did not exit after quit")

            reader_thread.join(timeout=1.0)

            # Display and final output captured by the reader.
            while True:
                try:
                    line = responses.get_nowait()
                except queue.Empty:
                    break
                if line is not None:
                    print(f"<--- Received: {line}")

            if status != 0:
                raise RuntimeError(
                        f"Engine failed during shutdown: status {status}")

            print("<--- Engine exited normally (status 0).")
            return []
        
        # Give engine time to process
        time.sleep(wait_time)
        
        # Silent commands produce no expected terminal output; return immediately
        cmd_name = cmd.split()[0]
        if cmd_name in ("ucinewgame", "position"):
            # Drain any debug output currently sitting in the queue without blocking
            lines = []
            while not responses.empty():
                try:
                    line = responses.get_nowait()
                    if line:
                        print(f"<--- Received: {line.strip()}")
                        lines.append(line.strip())
                except queue.Empty:
                    break
            return lines

        lines = []
        start_time = time.time()

        while True:
            status = process.poll()
            if status is not None:
                raise RuntimeError(
                        f"Engine exited unexpectedly while handling {cmd!r}: "
                        f"status {status}"
                        )

            remaining = timeout - (time.time() - start_time)
            if remaining <= 0:
                print("<--- <timeout>")
                break

            try:
                line = responses.get(timeout=max(0.01, remaining))
            except queue.Empty:
                print("<--- <timeout>")
                break

            if line is None:
                print("<--- Engine closed stdout")
                break

            cleaned = line.strip()
            print(f"<--- Received: {cleaned}")
            lines.append(cleaned)
                
            # Exit criteria for interactive UCI commands
            if cmd == "uci" and cleaned == "uciok":
                break
            elif cmd == "isready" and cleaned == "readyok":
                break
            elif cmd_name == "go" and cleaned.startswith("bestmove "):
                break
            elif "done" in cleaned:
                break

        return lines


    try:
        # Handshake
        send_command("uci", wait_time=0.1, timeout=1.0)
        send_command("isready", wait_time=0.1, timeout=1.0)
        
        # Silent game reset
        send_command("ucinewgame", wait_time=0.1, timeout=0.3)
        send_command("isready", wait_time=0.1, timeout=1.0)
        
        # Silent position setup
        send_command("position startpos moves e2e4 e7e5", wait_time=0.1,
                     timeout=0.5)
        
        # Calculation search
        send_command("go depth 7", wait_time=0.5, timeout=5.0)
        
        # Shutdown
        send_command("quit", wait_time=0.1, timeout=1.0)

    finally:
        if process.poll() is None:
            process.terminate()
            process.wait()

if __name__ == "__main__":
    path = sys.argv[1] if len(sys.argv) > 1 else "./ruhe"
    test_uci_engine(path)



