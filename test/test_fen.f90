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
program test_fen

  use TestUtilsM, only: begin_suite, begin_test, pass, fail
  use BoardM, only: BoardT, get_board, reset_board, set_fen, get_fen
  implicit none

  type(BoardT), pointer :: board
  character(len=:), allocatable :: original_fen
  integer :: i

  character(len=128), parameter :: TEST_FENS(5) = [ &
                                   character(len=128) :: &
                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", &
       "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", &
                                  "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", &
           "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1PPP2/R2Q1RK1 w kq - 0 1", &
                   "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" &
                                   ]

  call begin_suite("test_fen")
  board => get_board()

  call begin_test("FEN round-trip serialization")
  do i = 1, size(TEST_FENS)
    original_fen = trim(TEST_FENS(i))

    call board%reset_board()
    call set_fen(board, original_fen)

    if (original_fen /= get_fen(board)) then
      print *, "get_fen:", get_fen(board)
      call fail("Mismatch on FEN: "//trim(original_fen))
    end if
  end do
  call pass("All FEN round-trips matched perfectly")

end program test_fen
