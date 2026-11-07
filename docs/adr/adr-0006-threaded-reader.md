# ADR-0006: Transition to an OpenMP threaded UCI reader

- **Status:** Proposed; implementation deferred. The existing nonblocking input remains the production solution.
- **Date:** 2026-10-20
- **Project:** ruhe
- **Scope:** UCI input and search coordination; initial search remains serial.
- **Intended repository location:** `docs/adr/adr-0006-threaded-reader.md`

## Context

ruhe currently uses a working POSIX nonblocking stdin implementation: a C input buffer reads bytes, assembles complete lines, retains incomplete input, and exposes commands through `NonBlockingM`. The main loop and search must use that single input path. This solution is functional and satisfactory; there is no immediate requirement to replace it.

OpenMP offers an opportunity to learn shared-memory concurrency in Fortran before undertaking a search that uses multiple processors. A dedicated command reader could wait in an ordinary blocking Fortran `read` while a separate worker searches. This separates command reception from recursive search and may eventually remove the custom POSIX input code from the default build.

The exploratory two-thread prototype demonstrated the intended responsibilities, but is not a production implementation. Its single mailbox, idle busy-wait, placeholder board commands, and limited failure handling must be replaced before migration.

OpenMP is a separate standard supported by GNU Fortran, not a facility provided by the Fortran language standard itself. The target environments are Linux, Linux under WSL, and macOS using GCC/gfortran.

## Decision

Explore an OpenMP backend with one dedicated stdin reader and one engine worker. Retain the existing backend until the threaded backend meets the acceptance criteria below. Switching the production default is a later implementation milestone, not an effect of recording this ADR.

The first implementation will have exactly two cooperating roles:

| Role | Responsibilities | Ownership |
| --- | --- | --- |
| Reader | Read complete command lines; publish ordered commands; signal urgent stop/shutdown requests; service eligible readiness requests | stdin and its input buffer |
| Engine worker | Apply position/configuration changes; initialize a game; run search; unwind interrupted search; publish results | Mutable board, move stack, serial search state, evaluation state, and transposition-table operations |

Shared state is restricted to a synchronized command queue, request identifiers, control flags, lifecycle state, and output coordination. The reader must never modify the board while search is active.

This decision does not parallelize negamax. Multiple search workers, parallel root search, and shared transposition-table policies require a subsequent ADR.

## Command ordering and handoff

Replace the prototype mailbox with a bounded FIFO of owned command records. Each record contains the complete command text and a monotonically increasing sequence number. Copy text before releasing the queue lock; never share a mutable parser buffer between threads.

Preserve the controller's order for `setoption`, `ucinewgame`, `position`, and `go`. For example, `position ...` followed immediately by `go depth 5` must search the newly supplied position, even when both lines arrive in a single pipe write.

The queue has an explicit capacity and overflow policy. Do not silently overwrite or drop commands. For the first implementation, overflow is a diagnosed fatal input failure that requests shutdown. Blocking the sole reader indefinitely on a full queue would prevent it from receiving `stop` or `quit`.

Use search request identifiers so a stop cannot be lost while a queued `go` is being claimed. The reader assigns an identifier when accepting `go`; a subsequent `stop` records the applicable accepted request. Starting that request must not blindly clear a stop already recorded for it. Reject overlapping search requests with a diagnostic during the initial implementation; never silently replace a pending search.

### Commands received during search

| Command/event | Required treatment |
| --- | --- |
| `stop` | Signal the applicable search request without waiting behind ordinary queued commands. The worker unwinds and emits one `bestmove`. |
| `quit` | Publish a persistent shutdown request, stop search, and let the reader leave its loop. Do not start queued work afterward. |
| stdin EOF | Request orderly shutdown and stop active search. |
| Input error or command overflow | Record failure, request shutdown, and preserve a nonzero final exit status. |
| `isready` | Respond during search without stopping it when prior setup work is complete; otherwise defer acknowledgment until earlier setup work is complete. |
| `position`, `ucinewgame`, configuration changes | Queue for the worker; do not alter the active search position. |
| Another `go` while busy/pending | Diagnose and reject under the initial policy. |

Readiness needs an ordering fence, not an unconditional reader-side `readyok`: a readiness request following queued initialization must wait for that initialization. An active search alone must not delay `readyok`. Track setup completion separately from search completion.

## Synchronization and output

Begin with OpenMP locks protecting compound queue and lifecycle updates. All concurrent reads, writes, and resets of shared flags must follow the same synchronization discipline. `volatile` is not a replacement for synchronization.

Never hold the state/queue lock during blocking input, search, output, allocation-heavy work, or waiting. Copy the required state under the lock, release it, and then act. Avoid nested locks; define an order if nesting later becomes necessary.

Use a single output routine with a named OpenMP critical region. Serialize complete protocol lines and flush standard output after protocol responses. Diagnostics go to stderr. A Fortran `flush(output_unit)` drains an I/O buffer; it does not synchronize shared search state. OpenMP memory synchronization and Fortran I/O flushing serve different purposes.

Prefer `default(none)` in production parallel regions to make data ownership explicit. Review module variables, `save` variables, initialized locals, pointers, and helper routines for hidden shared state. Reader-local and worker-local parser state must remain separate.

## Search integration

Give each search its own control object and request identifier. The worker checks synchronized stop/shutdown state periodically, while enforcing node and time limits independently. Measure the check interval against stop-latency requirements rather than assuming a fixed node interval is always fast enough.

Every recursive exit restores the board by unmaking outstanding moves. An interrupted iteration must not replace the last completed iteration's result. Maintain a legal fallback move if interruption precedes completion of depth one. Emit exactly one `bestmove` for normal search completion or `stop`; suppress it on immediate program shutdown under the selected policy.

The worker alone resets per-search state. Shutdown and input-failure flags remain persistent and must never be cleared by a new search. Remove stdin reads and command polling from the recursive search once the threaded backend owns input.

## Idle waiting and shutdown

The reader can block in ordinary formatted Fortran input. Use bounded line assembly that detects overlong records and handles EOF explicitly; a fixed-length advancing read must not silently truncate a command. Do not also initialize or call the previous nonblocking stdin reader in the same execution.

The worker must not spin indefinitely when idle. OpenMP `taskyield` is not a sleeping notification primitive, and `OMP_WAIT_POLICY` alone does not fix a user-written polling loop. Select and validate a wakeup mechanism before promotion. A short timed OS sleep with synchronized queue checks is acceptable initially if measured idle CPU and response latency meet the criteria. Such a helper may retain a small platform dependency; eliminating every C/POSIX call is not a requirement.

Normal shutdown is cooperative: the reader receives `quit` or EOF, publishes shutdown, and returns; the worker observes it, unwinds, and returns; both leave the parallel region before locks and queues are destroyed.

A worker cannot portably cancel another thread's blocking Fortran stdin read using ordinary OpenMP constructs. Therefore unrecoverable worker failure must follow a documented process-wide fatal-termination path with a nonzero status, rather than waiting forever at the parallel-region barrier. Recoverable search errors may report failure and return the worker to idle. Do not rely on closing stdin from another thread to wake a read portably.

## Build and portability

- Add `-fopenmp` to compilation and final linking for the threaded backend in both debug and release builds. Import runtime procedures through `omp_lib`.
- Keep backend selection explicit in the build. Exactly one backend owns stdin. A non-OpenMP build must select the existing backend, not execute the threaded design with directives ignored.
- Request two threads, disable dynamic team reduction for this design, and verify the actual team size before either role starts. Fail clearly if the runtime cannot supply the required team.
- Test supported compiler/runtime versions on Linux, WSL, and macOS. Record versions rather than assuming all installed gfortran releases behave identically.
- Audit recursive and automatic-array storage. GNU Fortran's `-fopenmp` implies `-frecursive`, which can increase stack use; verify search stack requirements for worker threads.

## Migration plan

1. Preserve baseline protocol tests and a reproducible build using the existing backend.
2. Build the standalone two-thread demonstration; verify locks, blocking reader behavior, and team-size checks on the target systems.
3. Implement and test the ordered queue, request identifiers, lifecycle state, readiness fences, and serialized output without changing chess search behavior.
4. Move board-changing commands to the engine worker. Connect real serial search with stop/quit propagation and existing depth/node/time limits.
5. Add idle waiting, bounded input records, error handling, and process-wide fatal shutdown. Exercise piped input and interactive terminal input.
6. Compare both backends with the same tests and positions. Promote the threaded backend only after acceptance; keep the old implementation available for rollback during stabilization.
7. Retire `uci_input.c` and `NonBlockingM` from the default build only when their input responsibilities have been replaced. Document any remaining platform helper.

## Acceptance criteria

- Handshake and license/help behavior remain correct; each response is flushed and protocol lines never interleave.
- Batched and fragmented pipe writes preserve complete commands and command order. Empty lines, CRLF, long records, EOF, and input errors have defined outcomes.
- Repeated `go`/`stop` cycles produce one `bestmove` per stopped search, including `stop` immediately after `go` before worker pickup.
- `isready` responds during search and respects preceding initialization/configuration work.
- `quit` and EOF work while idle, with pending work, and during deep search; no thread remains stranded at a barrier.
- Ordinary searches preserve legal moves, board restoration, and existing depth/node/time behavior. Timing differences are assessed separately from correctness.
- As initial engineering targets, stop/readiness response latency is under 100 ms on an otherwise idle test machine, and idle consumption is under 1% of one logical CPU over 30 seconds. Record hardware and revise targets explicitly if necessary.
- Queue overflow, unavailable second thread, and injected worker/input failures produce explicit diagnostics and expected exit statuses rather than hangs.
- Both debug and release configurations pass on Linux, WSL, and macOS before the default changes.

## Consequences and alternatives

The transition introduces OpenMP team management, synchronization, queue semantics, and shutdown complexity. In return, input reception becomes independent of recursive search and provides practical experience with shared-memory concurrency.

Keeping the existing nonblocking backend indefinitely remains a valid alternative. Direct Fortran bindings to POSIX calls would change source language boundaries but would not provide threading experience. Coarrays and separate processes introduce different communication models and are not selected for this transition.

Future parallel search can reuse lifecycle and output coordination, but will require private board/search stacks, synchronized shared tables, global node/time accounting, and a deliberate thread budget that includes the reader. Two-thread input does not by itself make current search code safe for multiple workers or guarantee a speed improvement.

## References

- [GNU Fortran: OpenMP support and build options](https://gcc.gnu.org/onlinedocs/gfortran/OpenMP.html)
- [OpenMP API Specification 5.2](https://www.openmp.org/spec-html/5.2/openmp.html)

This ADR records a proposed architecture and validation plan. It does not claim the threaded prototype has been compiled or qualified across these platforms.
