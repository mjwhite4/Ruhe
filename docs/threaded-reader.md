The threaded reader would behave correctly under Linux. A useful first prototype has **one thread that owns stdin** and **one thread that owns the board and search**. They communicate through a small mailbox protected by an OpenMP lock.

The example below is a standalone Fortran program. Its search is simulated so you can test `uci`, `isready`, `go`, `stop`, and `quit` before connecting negamax.

It deliberately allows **one pending search**, rejects another `go` while busy, and uses the same lock for all shared state.

```fortran
program threaded_uci
  use iso_fortran_env, only: input_unit, output_unit, int64
  use omp_lib
  implicit none

  integer(omp_lock_kind) :: state_lock

  logical :: request_pending = .false.
  logical :: search_busy = .false.
  logical :: stop_requested = .false.
  logical :: quit_requested = .false.

  character(len=8192) :: pending_command = ""

  call omp_init_lock(state_lock)
  call omp_set_dynamic(.false.)

  !$omp parallel num_threads(2) default(shared)
  !$omp single
  if (omp_get_num_threads() /= 2) then
    error stop "This prototype requires two OpenMP threads"
  end if
  !$omp end single

  if (omp_get_thread_num() == 0) then
    call command_reader()
  else
    call search_worker()
  end if

  !$omp end parallel

  call omp_destroy_lock(state_lock)

contains

  ! Serialize complete output lines from both threads.
  subroutine emit(line)
    character(len=*), intent(in) :: line

    !$omp critical(uci_output)
    write(output_unit, '(a)') trim(line)
    flush(output_unit)
    !$omp end critical(uci_output)
  end subroutine emit


  subroutine command_reader()
    character(len=8192) :: command
    character(len=32) :: verb
    integer :: ios
    logical :: accepted

    do
      ! Only this thread ever reads stdin.
      read(input_unit, '(a)', iostat=ios) command

      if (ios /= 0) then
        call request_shutdown()
        exit
end if

      if (len_trim(command) == 0) cycle

      verb = ""
      read(command, *, iostat=ios) verb
      if (ios /= 0) cycle

      select case (trim(verb))
      case ("uci")
        call emit("id name ruhe threading prototype")
        call emit("id author Michael White")
        call emit("uciok")

      case ("isready")
        ! This prototype has no pending initialization to wait for.
        call emit("readyok")

      case ("go")
        call omp_set_lock(state_lock)

        accepted = .not. search_busy .and. .not. request_pending

        if (accepted) then
          pending_command = command
          stop_requested = .false.
          request_pending = .true.
          search_busy = .true.
        end if

        call omp_unset_lock(state_lock)

        if (.not. accepted) then
          call emit("info string Search already active")
        end if

      case ("stop")
        call omp_set_lock(state_lock)
        if (search_busy) stop_requested = .true.
        call omp_unset_lock(state_lock)

      case ("quit")
        call request_shutdown()
        exit

      case ("position", "ucinewgame")
        call emit("info string Board commands not implemented in prototype")

      case default
        ! Ignore unsupported commands.
      end select
    end do
  end subroutine command_reader


  subroutine request_shutdown()
    call omp_set_lock(state_lock)
    quit_requested = .true.
    stop_requested = .true.
    call omp_unset_lock(state_lock)
  end subroutine request_shutdown


  subroutine search_worker()
    character(len=8192) :: command
    logical :: have_request, quitting

    do
      call omp_set_lock(state_lock)

      quitting = quit_requested
      have_request = request_pending

      if (have_request) then
        command = pending_command
        request_pending = .false.
      end if

      call omp_unset_lock(state_lock)

      if (quitting) exit

      if (.not. have_request) then
        ! Prototype only: this is still a busy-wait loop.
        !$omp taskyield
        cycle
      end if

      call simulated_search(command)

      call omp_set_lock(state_lock)
      search_busy = .false.
      quitting = quit_requested
      call omp_unset_lock(state_lock)

      if (quitting) exit
    end do
  end subroutine search_worker


  logical function search_should_stop() result(stopping)
    call omp_set_lock(state_lock)
    stopping = stop_requested .or. quit_requested
    call omp_unset_lock(state_lock)
  end function search_should_stop


  subroutine simulated_search(command)
    character(len=*), intent(in) :: command

    integer(int64) :: nodes
    integer :: depth, max_depth, ios
    character(len=32) :: verb, option
    character(len=160) :: message
    real(real64_kind) :: iteration_start
    logical :: quitting

    ! Supported here: "go", "go infinite", "go depth N".
    max_depth = huge(0)
    verb = ""
    option = ""

    read(command, *, iostat=ios) verb, option

    if (trim(option) == "depth") then
      read(command, *, iostat=ios) verb, option, max_depth
      if (ios /= 0) max_depth = 1
      max_depth = max(1, max_depth)
    end if

    nodes = 0
    depth = 1

    search_loop: do
      if (search_should_stop()) exit

      ! Simulate one iteration taking about 250 ms.
      iteration_start = omp_get_wtime()

      do while (omp_get_wtime() - iteration_start < 0.25d0)
        nodes = nodes + 1

        if (mod(nodes, 1024_int64) == 0) then
          if (search_should_stop()) exit search_loop
        end if
      end do

      write(message, '(a,i0,a,i0)') &
        "info depth ", depth, " nodes ", nodes
      call emit(message)

      if (depth >= max_depth) exit
      depth = depth + 1
    end do search_loop

    call omp_set_lock(state_lock)
    quitting = quit_requested
    call omp_unset_lock(state_lock)

    ! Placeholder only: there is no board in this demonstration.
    if (.not. quitting) call emit("bestmove 0000")
  end subroutine simulated_search

end program threaded_uci
```

One declaration needs the real kind available at program scope. Change the opening import to:

```fortran
use iso_fortran_env, only: input_unit, output_unit, int64, real64
```

and use this declaration in `simulated_search`:

```fortran
real(real64) :: iteration_start
```

Compile with:

```sh
gfortran -std=f2018 -Wall -Wextra -fopenmp \
  threaded_uci.f90 -o threaded_uci
```

Then run interactively:

```text
uci
isready
go infinite
isready
stop
go depth 3
quit
```

While the simulated search runs, `isready` should receive an immediate `readyok`; `stop` should end the search; `quit` should let both threads finish.

For integration into ruhe:

- **Board ownership:** the search worker should execute `position`, `ucinewgame`, and `go`. The reader should enqueue these commands in order, rather than directly modify the board.
- **Search interruption:** negamax checks `search_should_stop()` periodically, unmakes moves while unwinding, and returns the last completed iteration’s result.
- **Output:** route both threads’ output through a common serialization mechanism such as `emit`.
- **Idle waiting:** replace the worker’s busy-wait with a notification mechanism before production use. `taskyield` does not guarantee sleeping.
- **Shutdown:** this works because `quit` or EOF releases the blocking reader. An internal worker failure also needs a policy for releasing or terminating that reader.

This is a threading demonstration, not a complete UCI engine; it omits a command queue, real board/search operations, and time controls. I haven’t compiled it in this environment.

---

Yes—**the threading model should work on Linux with gfortran and OpenMP**, including Linux under WSL.

Apply the declaration correction from the previous answer:

```fortran
use iso_fortran_env, only: input_unit, output_unit, int64, real64
```

```fortran
real(real64) :: iteration_start
```

Then compile:

```sh
gfortran -std=f2018 -Wall -Wextra -fcheck=all -g -fopenmp \
  threaded_uci.f90 -o threaded_uci
```

The blocking stdin read occupies only the reader thread; the worker can continue searching. OpenMP locks protect the shared flags and mailbox.

The limitations remain the same on Linux:

- The idle worker **busy-waits**, consuming CPU.
- `position` and `ucinewgame` are placeholders.
- An internal worker failure cannot automatically release the reader from its blocking `read`.
- `bestmove 0000` is a demonstration response.

One further detail: **do not initialize your previous nonblocking C input layer in this version**. The reader expects ordinary blocking stdin, and it must be the only stdin reader.

So the prototype is suitable for testing the Linux threading behavior, with those limitations; it is not yet a complete replacement for ruhe’s input and search handling.
