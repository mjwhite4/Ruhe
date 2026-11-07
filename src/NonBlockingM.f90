! nonblocking I/O
module NonBlockingM

  use, intrinsic :: iso_c_binding, only: c_char, c_int
  implicit none
  private

  public :: &
    uci_input_unit, &
    uci_input_close, &
    uci_input_wait, &
    uci_next_command

  integer(c_int), parameter, public :: &
    READ_COMPLETE_LINE = 1_c_int, &
    READ_NO_LINE = 0_c_int, &
    READ_EOF = -1_c_int, &
    READ_IOS_ERROR = -2_c_int, &
    READ_OVERFLOW = -3_c_int

  interface
    function uci_input_unit() bind(C, &
                                   name="uci_input_unit") result(status)
      import :: c_int
      integer(kind=c_int) :: status
    end function uci_input_unit

    function uci_input_close() bind(C, &
                                    name="uci_input_close") result(status)
      import :: c_int
      integer(kind=c_int) :: status
    end function uci_input_close

    function uci_input_wait(milliseconds) bind(C, &
                                           name="uci_input_wait") result(status)
      import :: c_int
      integer(kind=c_int), value :: milliseconds
      integer(kind=c_int) :: status
    end function uci_input_wait

    function uci_input_next(bytes, capacity, length) bind(C, &
                                           name="uci_input_next") result(status)
      import :: c_char, c_int
      character(kind=c_char), dimension(*), intent(out) :: bytes
      integer(kind=c_int), value :: capacity
      integer(kind=c_int), intent(out) :: length
      integer(kind=c_int) :: status
    end function uci_input_next
  end interface

contains

  ! @brief get the next full \n-terminated command from the input
  subroutine uci_next_command(command, status)

    character(len=*), intent(out) :: command
    integer(c_int), intent(out) :: status

    character(kind=c_char) :: bytes(len(command))
    integer(c_int) :: length
    integer :: i

    command = ""
    status = uci_input_next(bytes, int(len(command), c_int), length)

    if (status /= READ_COMPLETE_LINE) return

    do i = 1, length
      command(i:i) = bytes(i)
    end do
  end subroutine uci_next_command

end module NonBlockingM
