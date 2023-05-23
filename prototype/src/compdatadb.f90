! This is the module that holds the entire fluid database in the form of the "datadb" array
! The array should not be accessed directly, but through the get_compdata subroutine.
! This file is intended to be automatically generated from the json files in the 'fluids' directory.

module compdatadb
use compdata_mod
implicit none

public :: datadb_len, get_compdata

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
        if (ident == datadb(i)%ident) then
            data = datadb(i)
            return
        endif
    enddo
end subroutine get_compdata

subroutine init_datadb()
datadb(1)%ident = "speciesA"
    datadb(1)%base%Tc = 100
    datadb(1)%base%Tt = 30
    datadb(1)%base%pt = 15

    allocate(datadb(1)%variantdatadb(2))
        datadb(1)%variantdatadb(1)%ref = "default"
        datadb(1)%variantdatadb(1)%p1 = 10
        datadb(1)%variantdatadb(1)%p2 = 25

        datadb(1)%variantdatadb(2)%ref = "ref2"
        datadb(1)%variantdatadb(2)%p1 = 100
        datadb(1)%variantdatadb(2)%p2 = 250

datadb(2)%ident = "speciesB"
    datadb(2)%base%Tc = 500
    datadb(2)%base%Tt = 300
    datadb(2)%base%pt = 150

    allocate(datadb(2)%variantdatadb(2))
        datadb(2)%variantdatadb(1)%ref = "default"
        datadb(2)%variantdatadb(1)%p1 = -10
        datadb(2)%variantdatadb(1)%p2 = -25

        datadb(2)%variantdatadb(2)%ref = "ref2"
        datadb(2)%variantdatadb(2)%p1 = -100
        datadb(2)%variantdatadb(2)%p2 = -250

datadb_is_initialized = .true.
end subroutine init_datadb

end module compdatadb