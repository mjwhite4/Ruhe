! ruhe — A UCI chess engine written in Fortran.
! Copyright (C) 2026 Michael White
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!-
!! @brief CLI top-level command processing.
module UciM

  use, intrinsic :: iso_fortran_env, only: int64, real64
  use PieceM, only: PCK, BLACK, WHITE
  use BoardM, only: BoardT
  use StateM, only: StateT
  use PerftM, only: run_perft
  implicit none

  public :: &
    format_pv_line, &
    mainloop, &
    init_new_game, &
    print_result, &
    print_uci_info, &
    set_control

contains

  !! @brief prepare formatted pv sequence
  pure function format_pv_line(pv_moves, num_moves) result(pv_str)

    use MoveStackM, only: MVK ! get_uci returns move as string
    use MoveM, only: get_uci

    integer(kind=MVK), dimension(:), intent(in) :: pv_moves
    integer, intent(in) :: num_moves
    character(len=:), allocatable :: pv_str
    integer :: i

    pv_str = "" ! allocates pv_str to len 0

    do i = 1, num_moves
      if (i > 1) pv_str = pv_str//" "
      pv_str = pv_str//get_uci(pv_moves(i))
    end do
  end function format_pv_line

  !! @brief set board to initial game state
  subroutine init_new_game(board)

    use MoveStackM, only: reset_movestack
    use BoardM, only: get_fen, calculate_full_hash, set_fen, STARTING_FEN
    use BaseM, only: full_score

    type(BoardT), intent(inout) :: board

    call set_fen(board, STARTING_FEN)
    board%state%h = calculate_full_hash(board)
    call board%stack%reset_movestack()
  end subroutine init_new_game

  !! @brief no moves left; print game result.
  subroutine print_result(board)

    use PieceM, only: XCOLOR
    use BaseM, only: is_square_attacked

    type(BoardT), intent(inout) :: board
    integer(kind=PCK) :: co, xco

    co = board%state%turn
    xco = XCOLOR(co)

    if (co == WHITE .and. board%is_check()) then
      print "(a)", "0-1 {Black mates}"
    else if (co == BLACK .and. board%is_check()) then
      print "(a)", "1-0 {White mates}"
    else
      print "(a)", "1/2-1/2 {Stalemate}"
    end if
  end subroutine print_result

  !! @brief console user interface mainloop
  subroutine mainloop()

    use, intrinsic :: iso_fortran_env, only: &
      input_unit, output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_int

    use BaseM, only: full_score, print_mailbox
    use BoardM, only: &
      BoardT, calculate_full_hash, generate_legal_moves, get_board, &
      get_fen, parse_uci, print_move_list, reset_board, unmake_move
    use BuildInfoM, only: BUILD_NAME, BUILD_VERSION, print_build
    use MoveM, only: get_uci
    use MoveStackM, only: reset_movestack
    use NonBlockingM
    use SearchM, only: search_deeper, SearchResultT
    use StringUtilsM, only: next_token
    use ZobristM, only: zhash_to_hexstr
    implicit none

    intrinsic :: isatty

    character(len=8192) :: command
    character(len=:), allocatable :: token
    integer :: pos, nmoves
    integer(c_int) :: status
    logical :: input_failed, interactive, quit_requested
    type(BoardT), pointer :: board => null()
    type(SearchResultT) :: search_out

    board => get_board()
    call board%reset_board()
    call init_new_game(board)
    call generate_legal_moves(board, nmoves)

    status = uci_input_unit()
    if (status /= 0) error stop "cannot initialize stdin"

    quit_requested = .false.

    interactive = isatty(input_unit) .and. isatty(output_unit)

    if (interactive) then
      write (output_unit, "(a)") &
        "ruhe — Copyright (C) 2026 Michael White", &
        "Free software under GNU GPLv3 or later; no warranty.", &
        "Type 'license' or 'warranty' for details."
      flush (output_unit)
    end if

    ! Enter the usual UCI command loop

    input_failed = .false.
    quit_requested = .false.

    !! @brief prompt & get command.
    do while (.not. quit_requested)

      ! read (unit=*, fmt="(a)", iostat=ios) command
      call uci_next_command(command, status)
#ifdef DEBUG
      write (error_unit, "(a,i0,a,a,a)") &
        "status=", status, " command=[", trim(command), "]"
      call flush (error_unit)
#endif

      select case (status)

      case (READ_NO_LINE)
        ! idle: sleep until input arrives, or 100 ms elapses
        status = uci_input_wait(100_c_int)
        if (status < 0) exit
        cycle

      case (READ_EOF)
        ! controller closed stdin
        quit_requested = .true.

      case (READ_IOS_ERROR, READ_OVERFLOW)
        quit_requested = .true.
        input_failed = .true.

      case (READ_COMPLETE_LINE)
        if (len_trim(command) == 0) cycle

        pos = 1
        call next_token(command, " ", pos, token)

        select case (token)

         !!--- Nonstandard commands

        case ("board") !! @brief show mailbox board
          call print_mailbox(board)

        case ("build") !! @brief display version and compiler flags info
          call print_build()

        case ("eval")
           !! @brief show static evaluation this position
          write (output_unit, "(a, i0)") &
            "Static eval (ce) = ", board%score, &
            "(a, i0)", "Phase 24->0 (ce) = ", board%phase, &
            "(a, i0)", "Middle game (ce) = ", board%score_mg
          call flush (output_unit)

        case ("fen") !! @brief show the fen string
          print "(a)", trim(get_fen(board))

        case ("hash") !! @brief show hash key for this position
          print "(a, a17)", &
            "Hash: ", zhash_to_hexstr(board%state%h)

        case ("license", "l")
          write (output_unit, "(a,a)") &
            "info string ", BUILD_NAME//" - GNU General Public License v3.0", &
            "info string ", "This program is free software: you can "// &
            "redistribute it and/or modify", &
            "info string ", &
            "it under the terms of the GNU General Public License as "// &
            "published by", &
            "info string ", "the Free Software "// &
            "Foundation, either version 3 of the License, or", &
            "info string ", "(at your option) any later version."
          call flush (output_unit)

        case ("moves")
          !! @brief show cui moves for this position
          call print_move_list(board)

        case ("warranty", "w")
          write (unit=output_unit, fmt="(a,a)") &
            "info string ", &
            "This program is distributed in the hope that it will be useful,", &
            "info string ", &
            "but WITHOUT ANY WARRANTY; without even the implied warranty of", &
            "info string ", &
            "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the", &
            "info string ", &
            "GNU General Public License for more details."
          flush (output_unit)

        !!--- UCI commands

        case ("isready")
          !! @brief nondestructive state checks, no structural reset
          write (output_unit, "(a)") "readyok"
          flush (output_unit)

        case ("position")
          !! @brief set position and optional move sequence
          call parse_position(board, command, pos)

        case ("go")
          !! @brief search according to parameters and yield bestmove

          ! set_control
          call set_control(board, command)

          ! search
          call search_deeper(board, search_out)

          write (output_unit, "(a)") "bestmove "//get_uci(search_out%move)
          flush (output_unit)
          quit_requested = board%control%quit_requested

        !! @brief uci
        case ("uci")
          write (output_unit, "(a)") &
            "id name "//BUILD_NAME//" (C) 2026 Michael White", &
            "id author Michael White", &
            "info string "//BUILD_NAME//" version "//BUILD_VERSION//" ", &
            "Copyright (C) 2026 Michael White", &
            "info string This program comes with ABSOLUTELY NO WARRANTY.", &
            "info string This is free software, licensed under GPLv3.", &
            "uciok"
          flush (output_unit)

        !! @brief clears, resets internal game state, no response
        case ("ucinewgame")
          call board%reset_board()
          call init_new_game(board)
          call generate_legal_moves(board, nmoves)

        !! @brief display help
        case ("help", "h", "?")
          write (output_unit, "(a)") &
            "", &
            "UCI commands:", &
            "  * isready; reset/initialize tables", &
            "  * go [perft <n>]; search bestmove /or/ count nodes at depth", &
            "  * position fen <fen> [moves ...]; set fen, apply moves", &
            "  * position startpos [moves ...]; set STARTPOS, apply moves", &
            "  * uci; establish uci protocol mode", &
            "  * ucinewgame; clear, reset internal game state, no response", &
            "  * (q)uit; quit", &
            "", &
            "Non-UCI commands: ", &
            "  * board; show board", &
            "  * build; show compilation flags", &
            "  * edit; edit position", &
            "  * fen; show FEN", &
            "  * hash; show hash key", &
            "  * (h)elp; show this message", &
            "  * (l)icense; show license", &
            "  * moves; show move list", &
            "  * score; show positional score", &
            "  * (w)arranty; show warranty", &
            ""
          flush (output_unit)

        case ("quit", "q", "exit", "end", "bye") !! @brief quit
          write (error_unit, "(a)") "quit!"
          quit_requested = .true.

        case default
          write (error_unit, "(a)") &
            ">>> Command not recognized:"//trim(token)//"<<<", &
            ">>> Type 'h' for help"
          flush (error_unit)

        end select
        call flush (output_unit)
        call flush (error_unit)
      end select
      call flush (output_unit)
      call flush (error_unit)
    end do
    call flush (output_unit)

    ! clean up the input unit before shutdown
    status = uci_input_close()
    if (status /= 0) input_failed = .true.
    if (input_failed) error stop "UCI input or cleanup failed"

  end subroutine mainloop

  !! @brief parse "position" command
  subroutine parse_position(board, cmd_str, ipos)

    use, intrinsic :: iso_fortran_env, only: output_unit
    use BaseM, only: print_mailbox
    use BoardM, only: STARTING_FEN
    use MoveM, only: MVK
    use StringUtilsM, only: next_token

    type(BoardT), intent(inout) :: board
    character(len=*), intent(in) :: cmd_str
    integer, intent(inout) :: ipos

    character(len=:), allocatable :: fen, token
    integer :: itok
    integer(kind=MVK) :: mv

    call next_token(cmd_str, " ", ipos, token)

    if (token /= "startpos" .and. token /= "fen") then
      print "(a)", "Error: expected STARTPOS or FEN: "//trim(cmd_str)
      call flush (output_unit)
      return

    elseif (token == "startpos") then
      call board%set_fen(STARTING_FEN)

    elseif (token == "fen") then
      ! FEN has exactly six non-empty items, else Warning
      fen = ""
      do itok = 1, 6
        call next_token(cmd_str, " ", ipos, token)
        if (len_trim(token) == 0) then
          print *, "Error: FEN incomplete:"//trim(fen)
          call flush (output_unit)
          return
        end if
        fen = trim(fen)//" "//trim(token)
      end do
      call board%set_fen(trim(fen))
      ! call print_mailbox(board)
      ! call flush (output_unit)
    end if

    call next_token(cmd_str, " ", ipos, token) ! for "moves"

    if (token /= "moves" .and. token /= "") then
      print "(a)", "Error: expected token 'moves', saw:"//trim(token)
      call flush (output_unit)
      return

    else if (token == "moves") then
      do while (ipos <= len_trim(cmd_str))
        call next_token(cmd_str, " ", ipos, token)
        if (len_trim(token) == 0) exit ! end of cmd_str

        call board%parse_uci(trim(token), mv)
        if (mv /= 0) then
          call board%make_move(mv) ! apply move and push state
        else
          print "(a)", "Error: illegal/unrecognized move:"//trim(token)
          call flush (output_unit)
          exit
        end if
      end do
      ! call print_mailbox(board)
      ! call flush (output_unit)
    end if

  end subroutine parse_position

  !! @brief emit UCI search info output (e.g. info depth 12 score cp 35 ...)
  subroutine print_uci_info(depth, score, nodes, pv_moves, num_moves)

    use, intrinsic :: iso_fortran_env, only: output_unit
    use MoveStackM, only: MVK

    integer, intent(in) :: depth, score, num_moves
    integer(kind=int64), intent(in) :: nodes
    integer(kind=MVK), intent(in) :: pv_moves(:)

    character(len=:), allocatable :: pv_str

    pv_str = format_pv_line(pv_moves, num_moves)

    ! Clean output formatted with exact lengths, no trailing spaces
    write (output_unit, "(a, i0, a, i0, a, i0, a, a)") &
      "info depth ", depth, &
      " score cp ", score, &
      " nodes ", nodes, &
      " pv ", pv_str
  end subroutine print_uci_info

  !! @brief parse uci "go" parameters and populate control and limits
  subroutine set_control(bd, cmd_str, status_opt)

    use, intrinsic :: iso_fortran_env, only: output_unit

    use BoardM, only: BoardT
    use ClockM, only: LimitsT
    use StringUtilsM, only: next_token, to_int, to_str

    type(BoardT), intent(inout) :: bd
    character(len=*), intent(in) :: cmd_str
    integer, intent(out), optional :: status_opt

    character(len=:), allocatable :: token
    integer :: ipos, status

    bd%limits = LimitsT()

    status = 0
    if (present(status_opt)) status_opt = 0

    ipos = 1
    call next_token(cmd_str, " ", ipos, token) ! skip initial 'go' token
    ! print "(a)", "token="//trim(token)

    do while (ipos < len_trim(cmd_str))

      call next_token(cmd_str, " ", ipos, token)
      ! print "(a)", "token="//trim(token)
      if (len_trim(token) == 0) exit

      select case (token)

      case ("binc")
        ! @brief black's time increment (ms) per move
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 1
        else
          call to_int(token, bd%limits%binc, status)
          if (status == 0) then
            if (bd%limits%binc < 0) status = 2
          end if
        end if

      case ("btime")
        ! @brief black's present time remaining (ms)
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 3
        else
          call to_int(token, bd%limits%btime, status)
          if (status == 0) then
            if (bd%limits%btime < 1) status = 4
          end if
        end if

      case ("depth")
        ! @brief limit search to depth plies
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 5
        else
          call to_int(token, bd%limits%depth, status)
          if (status == 0) then
            if (bd%limits%depth < 1) status = 6
          end if
        end if

      case ("infinite")
        ! @brief whether to calculate forever (until quit/stop)
        bd%limits%infinite = .true.

      case ("nodes")
        ! @brief limit search according to visited nodes
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 7
        else
          call to_int(token, bd%limits%nodes, status)
          if (status == 0) then
            if (bd%limits%nodes < 1) status = 8
          end if
        end if

      case ("ponder")
        ! @brief whether permissible to search on opponent's time
        bd%limits%ponder = .true.

      case ("movetime")
        ! @brief limit search time to exactly N milliseconds
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 9
        else
          call to_int(token, bd%limits%movetime, status)
          if (status == 0) then
            if (bd%limits%movetime < 1) status = 10
          end if
        end if

      case ("movestogo")
        ! @brief moves left until the next time control
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 11
        else
          call to_int(token, bd%limits%movestogo, status)
          if (status == 0) then
            if (bd%limits%movestogo < 1) status = 12
          end if
        end if

      case ("winc")
        ! @brief white's time increment (ms) per move
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 13
        else
          call to_int(token, bd%limits%winc, status)
          if (status == 0) then
            if (bd%limits%winc < 0) status = 14
          end if
        end if

      case ("wtime")
        ! @brief white's present time remaining (ms)
        call next_token(cmd_str, " ", ipos, token)

        if (len_trim(token) == 0) then
          status = 15
        else
          call to_int(token, bd%limits%wtime, status)
          if (status /= 0) then
            if (bd%limits%wtime < 1) status = 16
          end if
        end if

      case default
        ! TODO ignore perft for now
        error stop "set_control, unknown go:"//trim(cmd_str(ipos:))
        call flush (output_unit)

      end select

      if (status /= 0) then
      if (present(status_opt)) then
        status_opt = status
        return
      end if
      error stop "set_control, status="//to_str(status)
      end if

    end do
  end subroutine set_control

end module UciM
