module compdata_mod

implicit none

! Struct to hold the data all fluids require
type BaseData
    real :: Tc
    real :: Tt
    real :: pt
    real :: dipole
end type BaseData

! Struct to hold parameters for VariantEoS models
type VariantData
    character(len=10) :: ref
    real :: p1
    real :: p2
end type VariantData

! Top level database structure
! The fluid database consists of an array of CompData objects, found in compdatadb
! To retrieve the compdata object corresponding to a specific identifier, use the methods found there
type CompData
    character(len=10) :: ident

    class(BaseData), allocatable :: base
    class(VariantData), allocatable, dimension(:) :: VariantDatadb
end type CompData

end module compdata_mod