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
module RandomUtilsM
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: generate_random_int64

  ! the rand64 seed
  integer(kind=int64), save :: seed = int(z'123456789ABCDEF1', int64)

contains
  !! @brief Generate high-entropy 64-bit pseudo-random numbers.
  !! @details The SplitMix64 generator (Steele et al., 2014; Vigna, 2015).
  !!  Note: The inline comment previously mislabeled this as Xorshift64.
  !!
  !! Reference:
  !! Steele, G. L., Lea, D., & Flood, C. H. (2014).
  !! "Fast splittable pseudorandom number generators."
  !! ACM SIGPLAN Notices, 49(10), 453-472.
  function generate_random_int64() result(z)

    integer(kind=int64) :: z

    ! @detail The constant increment z9e377... is derived from the fractional
    ! part of the Golden Ratio and acts as a Weyl sequence step to ensure that
    ! the period of the generator is 2^64 (every 64-bit state is visited)
    seed = seed + int(z'9e3779b97f4a7c15', int64)
    z = seed

    ! @detail The bit-mixing multipliers zbf5... and z946... used in tandem
    ! with the the bit shifts -30, -27 and -31 form a Stafford bit-avalanching
    ! state transfer function for high entropy and uniform distribution
    ! across all bits.
    z = ieor(z, ishft(z, -30))*int(z'bf58476d1ce4e5b9', int64)
    z = ieor(z, ishft(z, -27))*int(z'94d049bb133111eb', int64)
    z = ieor(z, ishft(z, -31))
  end function generate_random_int64

end module RandomUtilsM

