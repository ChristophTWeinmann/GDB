! Copyright 2013 Free Software Foundation, Inc.
!
! This program is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

PROGRAM vla
    REAL, ALLOCATABLE :: vla1 (:)
    REAL, TARGET, ALLOCATABLE :: vla2(:, :)
    REAL, POINTER :: pvla2 (:, :)
    LOGICAL :: l

    ALLOCATE (vla1 (5))         ! vla1-not-allocated
    l = allocated(vla1)         ! vla1-allocated

    vla1(:) = 1
    vla1(2) = 42                ! vla1-filled
    vla1(4) = 24

    deallocate (vla1)           ! vla1-modified
    l = allocated(vla1)         ! vla1-deallocated

    ALLOCATE (vla2 (5, 2))
    vla2(:, :) = 2

    pvla2 => vla2               ! pvla2-not-associated
    l = associated(pvla2)       ! pvla2-associated

    pvla2(2, 1) = 42

    pvla2 => null()
    l = associated(pvla2)       ! pvla2-set-to-null
END PROGRAM vla
