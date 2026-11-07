#!/usr/bin/env python3
"""Process-level Ruhe UCI tests. Requires: python3 -m pip install python-chess.
Run: python3 test_extra/test_uci_integration.py ./ruhe-debug
Timing uses host monotonic timestamps, not engine-reported time.
"""
import argparse
from collections import deque
from pathlib import Path
import queue
import subprocess
import sys
import threading
import time

import chess

GREEN = "\x1b[32m"
RED = "\x1b[31m"
RESET = "\x1b[0m"
PASS = f"{GREEN}[PASS]{RESET}" if sys.stdout.isatty() else "[PASS]"
FAIL = f"{RED}[FAIL]{RESET}" if sys.stderr.isatty() else "[FAIL]"


class Engine:
    def __init__(self, path, timeout, verbose):
        self.timeout = timeout
        self.verbose = verbose
        self.events = queue.Queue()
        self.stderr = deque(maxlen=60)
        # Inherit the environment: do not hide engine flush defects with a
        # GFORTRAN_UNBUFFERED_PRECONNECTED override.
        self.process = subprocess.Popen([str(Path(path).resolve())],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, text=True, bufsize=1)
        self.threads = [threading.Thread(target=self.read_stdout, daemon=True),
                        threading.Thread(target=self.read_stderr, daemon=True)]
        for thread in self.threads:
            thread.start()

    def read_stdout(self):
        for line in self.process.stdout:
            self.events.put((time.monotonic(), line.rstrip('\r\n')))
        self.events.put((time.monotonic(), None))

    def read_stderr(self):
        for line in self.process.stderr:
            self.stderr.append(line.rstrip())
            if self.verbose:
                print('stderr:', line.rstrip())

    def send(self, command):
        if self.process.poll() is not None:
            raise AssertionError(f'Engine exited before {command!r}')
        if self.verbose:
            print('-->', command)
        sent = time.monotonic()
        self.process.stdin.write(command + '\n')
        self.process.stdin.flush()
        return sent

    def wait(self, predicate, timeout=None, on_line=None):
        deadline = time.monotonic() + (self.timeout if timeout is None else timeout)
        lines = []
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError(f'Response timeout; last lines: {lines[-8:]}')
            try:
                stamp, line = self.events.get(timeout=remaining)
            except queue.Empty:
                raise AssertionError(f'Response timeout; last lines: {lines[-8:]}')
            if line is None:
                raise AssertionError(f'Unexpected stdout EOF; status={self.process.poll()}')
            if self.verbose:
                print('<--', line)
            lines.append(line)
            if on_line:
                on_line(line)
            if predicate(line):
                return stamp, lines

    @staticmethod
    def no_bestmove(line):
        if line.startswith('bestmove '):
            raise AssertionError('Unexpected or duplicate bestmove: ' + line)

    def ready(self):
        self.send('isready')
        self.wait(lambda line: line == 'readyok', on_line=self.no_bestmove)

    def position(self, board):
        self.send('ucinewgame')
        self.ready()
        self.send('position fen ' + board.fen())
        self.ready()

    def bestmove(self, board, timeout=None):
        stamp, lines = self.wait(lambda line: line.startswith('bestmove '), timeout)
        fields = lines[-1].split()
        if len(fields) not in (2, 4) or (len(fields) == 4 and fields[2] != 'ponder'):
            raise AssertionError('Malformed bestmove: ' + lines[-1])
        legal = list(board.legal_moves)
        if not legal:
            if fields[1] != '0000':
                raise AssertionError('Terminal position requires bestmove 0000')
        else:
            move = chess.Move.from_uci(fields[1])
            if move not in legal:
                raise AssertionError('Illegal bestmove: ' + fields[1])
        if len(fields) == 4:
            child = board.copy()
            child.push(chess.Move.from_uci(fields[1]))
            if chess.Move.from_uci(fields[3]) not in child.legal_moves:
                raise AssertionError('Illegal ponder move: ' + fields[3])
        return stamp, lines

    def quiet_search(self, seconds):
        deadline = time.monotonic() + seconds
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return
            try:
                _, line = self.events.get(timeout=remaining)
            except queue.Empty:
                return
            if line is None:
                raise AssertionError('Engine exited during infinite search')
            self.no_bestmove(line)
            if self.verbose:
                print('<--', line)

    def quit(self, active_board=None):
        self.send('quit')
        try:
            status = self.process.wait(timeout=self.timeout)
        except subprocess.TimeoutExpired:
            raise AssertionError('Engine did not exit after quit')
        for thread in self.threads:
            thread.join(timeout=1)
        bestmoves = 0
        for thread in self.threads:
            if thread.is_alive():
                raise AssertionError('Output reader did not finish after quit')
        while not self.events.empty():
            _, line = self.events.get_nowait()
            if line is not None and line.startswith('bestmove '):
                if active_board is None:
                    self.no_bestmove(line)
                bestmoves += 1
                fields = line.split()
                if len(fields) != 2 or chess.Move.from_uci(fields[1]) not in active_board.legal_moves:
                    raise AssertionError('Invalid bestmove during quit: ' + line)
        if bestmoves > 1:
            raise AssertionError('Duplicate bestmove during quit')
        if status != 0:
            raise AssertionError(f'Nonzero quit status: {status}')

    def close(self):
        if self.process.poll() is None:
            self.process.terminate()
            try:
                self.process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()
        for thread in self.threads:
            thread.join(timeout=1)
        for pipe in (self.process.stdin, self.process.stdout, self.process.stderr):
            pipe.close()


def info_values(lines, key):
    values = []
    for line in lines:
        fields = line.split()
        if fields and fields[0] == 'info' and 'string' not in fields:
            for i in range(1, len(fields) - 1):
                if fields[i] == key:
                    try:
                        values.append(int(fields[i + 1]))
                    except ValueError:
                        raise AssertionError(f'Invalid info {key}: {line}')
    return values


def run(args):
    engine = Engine(args.engine, args.timeout, args.verbose)
    board = chess.Board()
    board.push_uci('e2e4')
    board.push_uci('e7e5')
    try:
        engine.send('uci')
        _, lines = engine.wait(lambda line: line == 'uciok',
                               on_line=engine.no_bestmove)
        assert any(line.startswith('id name ') for line in lines), 'Missing id name'
        assert any(line.startswith('id author ') for line in lines), 'Missing id author'
        engine.ready()
        print(f'{PASS} handshake')

        engine.position(board)
        engine.send('go depth 3')
        _, lines = engine.bestmove(board)
        depths = info_values(lines, 'depth')
        assert depths and max(depths) == 3, f'Expected completed depth 3, saw {depths}'
        engine.ready()
        print(f'{PASS} go depth 3: legal bestmove, depth reported')

        engine.position(board)
        started = engine.send('go nodes 1000')
        stamp, lines = engine.bestmove(board)
        nodes = info_values(lines, 'nodes')
        # Iteration reports can precede the node limit; do not require the
        # last completed iteration to have exactly 1000 nodes.
        assert all(n <= 1000 for n in nodes), f'Reported node overshoot: {nodes}'
        engine.ready()
        print(f'{PASS} go nodes 1000: legal bestmove ({stamp-started:.3f} s)')
        print('       Exact node enforcement requires the internal node-limit test.')

        engine.position(board)
        started = engine.send(f'go movetime {args.movetime}')
        stamp, _ = engine.bestmove(board,
            timeout=args.movetime / 1000 + args.slack + args.timeout)
        elapsed = stamp - started
        assert elapsed <= args.movetime / 1000 + args.slack, \
            f'movetime overrun: {elapsed:.3f}s (slack {args.slack}s)'
        # No strict lower bound: stopping early can be a valid engine policy.
        engine.ready()
        print(f'{PASS} go movetime {args.movetime}: legal bestmove ({elapsed:.3f} s)')

        engine.position(board)
        engine.send('go infinite')
        engine.quiet_search(args.hold)
        engine.ready()  # Must respond while searching, without bestmove.
        engine.quiet_search(args.hold)
        stopped = engine.send('stop')
        stamp, _ = engine.bestmove(board, timeout=args.stop_timeout)
        assert stamp >= stopped, 'bestmove preceded stop'
        engine.ready()
        print(f'{PASS} infinite / isready / stop: legal bestmove ({stamp-stopped:.3f} s)')

        # Reuse the process after interruption: catches stale stopped flags.
        engine.position(board)
        engine.send('go depth 1')
        engine.bestmove(board)
        engine.ready()
        print(f'{PASS} search after stop')
        # Terminal positions must produce exactly one null bestmove. The
        # readiness barrier and quiet interval catch a duplicate response.
        for name, fen in (
            ('checkmate', '7k/6Q1/5K2/8/8/8/8/8 b - - 0 1'),
            ('stalemate', '7k/5Q2/6K1/8/8/8/8/8 b - - 0 1'),
        ):
            terminal = chess.Board(fen)
            assert terminal.is_checkmate() if name == 'checkmate' else terminal.is_stalemate()
            engine.position(terminal)
            engine.send('go depth 3')
            _, lines = engine.bestmove(terminal)
            assert lines[-1].split() == ['bestmove', '0000'], lines[-1]
            engine.ready()
            engine.quiet_search(args.hold)
            print(f'{PASS} {name}: exactly one bestmove 0000')

        # Integration bounds follow Ruhe's current ClockM policy, not a UCI
        # requirement: reserve=50 ms, soft=available/moves+4*increment/5,
        # hard=min(available,3*soft). Exact arithmetic belongs in test_clock.
        for turn in (chess.WHITE, chess.BLACK):
            clock_board = chess.Board()
            if turn == chess.BLACK:
                clock_board.push_uci('e2e4')
            for increment in (0, 100):
                own_ms, other_ms, moves = 1000, 20000, 10
                wtime, btime = (own_ms, other_ms) if turn else (other_ms, own_ms)
                winc, binc = (increment, 500) if turn else (500, increment)
                available = own_ms - 50
                target = available // moves + increment * 4 // 5
                hard_ms = min(available, 3 * target)
                engine.position(clock_board)
                command = (f'go wtime {wtime} btime {btime} '
                           f'winc {winc} binc {binc} movestogo {moves}')
                started = engine.send(command)
                stamp, _ = engine.bestmove(clock_board,
                    timeout=hard_ms / 1000 + args.slack + args.timeout)
                elapsed = stamp - started
                assert elapsed <= hard_ms / 1000 + args.slack, (
                    f'Clock overrun: {elapsed:.3f} s; hard={hard_ms} ms')
                engine.ready()
                color = 'white' if turn else 'black'
                print(f'{PASS} {color} clock, increment {increment} ms: '
                      f'legal bestmove ({elapsed:.3f} s; hard {hard_ms} ms)')
        print('       Exact allocation and side selection remain covered by test_clock.')
        engine.quit()
        print(f'{PASS} quit: exit status 0')
    except Exception:
        print('\nRecent engine stderr:', file=sys.stderr)
        for line in engine.stderr:
            print(line, file=sys.stderr)
        raise
    finally:
        engine.close()


def test_active_quit(args):
    # Fresh process because this test deliberately ends the engine.
    engine = Engine(args.engine, args.timeout, args.verbose)
    try:
        engine.send('uci')
        engine.wait(lambda line: line == 'uciok', on_line=engine.no_bestmove)
        engine.ready()
        board = chess.Board()
        engine.position(board)
        engine.send('go infinite')
        # Observe progress before sending quit, rather than assuming that a
        # fixed delay guarantees the search has started.
        engine.wait(lambda line: line.startswith('info depth '),
                    on_line=engine.no_bestmove)
        engine.ready()
        engine.quiet_search(args.hold)
        started = time.monotonic()
        engine.quit(active_board=board)
        print(f'{PASS} quit during search: exit status 0 '
              f'({time.monotonic()-started:.3f} s)')
    except Exception:
        print('Recent engine stderr:', file=sys.stderr)
        for line in engine.stderr:
            print(line, file=sys.stderr)
        raise
    finally:
        engine.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('engine', nargs='?', default='./ruhe')
    parser.add_argument('--timeout', type=float, default=10,
                        help='general response timeout in seconds')
    parser.add_argument('--movetime', type=int, default=500,
                        help='requested movetime in milliseconds')
    parser.add_argument('--slack', type=float, default=1,
                        help='allowed movetime overrun in seconds')
    parser.add_argument('--stop-timeout', type=float, default=2)
    parser.add_argument('--hold', type=float, default=0.25,
                        help='infinite-search observation interval in seconds')
    parser.add_argument('--verbose', action='store_true')
    args = parser.parse_args()
    if min(args.timeout, args.movetime, args.slack, args.stop_timeout, args.hold) <= 0:
        parser.error('all numeric controls must be positive')
    try:
        run(args)
        test_active_quit(args)
    except (AssertionError, OSError, ValueError) as exc:
        print(f'{FAIL} {exc}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
