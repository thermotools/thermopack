! Functions and subroutines for accessing the datadb array in compdatadb.f90
! Specifically, to retrieve the BaseData struct for a specific fluid, and the VariantData struct for a
! Specific fluid and parameter reference
module compdatautils

use compdata_mod, only : CompData, VariantData, BaseData
use compdatadb, only : get_datadb, datadb_len
implicit none

private

contains

! Get the BaseData struct corresponding to a specific fluid
function get_base_data(ident) result(bdata)
    character(len=10), intent(in) :: ident
    type(BaseData) :: bdata
    type(CompData) :: cdata

    call get_compdata(ident, cdata)
    bdata = cdata%base
end function get_base_data

! Get the VariantData struct corresponding to a specific fluid and parameter reference
function get_variant_compdata(ident, ref) result(data)
    character(len=10), intent(in) :: ident
    character(len=10), intent(in) :: ref
    type(VariantData) :: data

    type(CompData) :: cdata
    integer :: i

    call get_compdata(ident, cdata)
    do i = 1, size(cdata%VariantDataDb)
        if (ref == cdata%VariantDataDb(i)%ref) then
            data = cdata%VariantDataDb(i)
            return
        endif
    enddo
end function get_variant_compdata

end module compdatautils