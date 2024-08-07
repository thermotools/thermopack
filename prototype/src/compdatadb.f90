! This is the module that holds the entire fluid database in the form of the "datadb" array
! The array should not be accessed directly, but through the get_compdata subroutine.
! This file is intended to be automatically generated from the json files in the 'fluids' directory.

module compdatadb
use compdata_mod
implicit none

public :: datadb_len, get_compdata, str_eq

integer :: datadb_len=2
logical, save :: datadb_is_initialized = .false.
type(CompData), dimension(2), save :: datadb

contains

! Method to retrieve the CompData object corresponding to a specific fluid
! Ensures that the compdatadb array is only initialized once.
subroutine get_compdata(ident, data)
    character(len=10), intent(in) :: ident
    type(Compdata), intent(out) :: data
    integer :: i

    if (.not. datadb_is_initialized) then
        call init_datadb()
    endif

    do i = 1, datadb_len
        if (str_eq(ident, datadb(i)%ident)) then
            data = datadb(i)
            return
        endif
    enddo

    print*, "No data found for ", ident
    error stop
end subroutine get_compdata

function str_eq(str1, str2) result(equal)
    character(len=*), intent(in) :: str1, str2
    logical :: equal
    integer :: i

    equal = .true.

    do i = 1, len(str1)
        if (str1(i:i) /= str2(i:i)) then
            equal = .false.
            exit
        end if
        if (str1(i:i) == char(0)) exit
    end do

    if (len(str1) /= len(str2)) equal = .false.

end function str_eq

subroutine init_datadb()
datadb(1)%ident = "speciesA" // char(0)
    datadb(1)%base%Tc = 100
    datadb(1)%base%Vc = 10
    datadb(1)%base%Tt = 30
    datadb(1)%base%pt = 15
    allocate(datadb(1)%variantdatadb(2))
        datadb(1)%variantdatadb(1)%ref = "default" // char(0)
        datadb(1)%variantdatadb(1)%p1 = 10
        datadb(1)%variantdatadb(1)%p2 = 25

        datadb(1)%variantdatadb(2)%ref = "ref2" // char(0)
        datadb(1)%variantdatadb(2)%p1 = 100
        datadb(1)%variantdatadb(2)%p2 = 250

datadb(2)%ident = "speciesB" // char(0)
    datadb(2)%base%Tc = 500
    datadb(2)%base%Vc = 30
    datadb(2)%base%Tt = 300
    datadb(2)%base%pt = 150

    allocate(datadb(2)%variantdatadb(2))
        datadb(2)%variantdatadb(1)%ref = "default" // char(0)
        datadb(2)%variantdatadb(1)%p1 = -10
        datadb(2)%variantdatadb(1)%p2 = -25

        datadb(2)%variantdatadb(2)%ref = "ref2" // char(0)
        datadb(2)%variantdatadb(2)%p1 = -100
        datadb(2)%variantdatadb(2)%p2 = -250

datadb_is_initialized = .true.
end subroutine init_datadb

end module compdatadb