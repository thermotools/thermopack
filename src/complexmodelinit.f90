!> Initialization of complex models.
!! These models are typically comprised of several
!! specific sub-models, and when used together they
!! define a known model.
!!
!! \author MH, 2016-12
module complexmodelinit
  !
  use eoslibinit, only: init_thermo
  implicit none
  save
  !
  private
  public :: init_VTPR, init_UMR
  !
contains

  !----------------------------------------------------------------------
  !> Initialize VTPR unifac-pr based model
  !> See Schmid 2014 (10.1021/ie404118f) or later
  !> \author MH, 2016-12
  !----------------------------------------------------------------------
  subroutine init_VTPR(ncomp,comp_string,nphases,&
       kij_setno,alpha_setno)
    use parameters, only: clen
    use volume_shift, only: InitVolumeShift
    use tpvar, only: cbeos, comp
    implicit none
    integer, intent(in) :: ncomp !< Number of components
    character(len=*), intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
    integer, intent(in) :: nphases !< Number of phases
    integer, optional, intent(in) :: kij_setno, alpha_setno !< Data set numbers
    ! Locals
    character(len=clen) :: eosLib !< String defining eos library (TP, DTU, TREND, ThermoPack)
    character(len=clen) :: eos    !< String defining equation of state
    character(len=clen) :: mixing !< String defining mixing rules
    character(len=clen) :: alpha  !< String defining alpha correlation
    integer :: ic, volumeShiftId, ncbeos
    real :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)

    eosLib = 'Thermopack'
    eos = 'PR'
    mixing = 'VTPR'
    alpha = 'TWU'
    b_exponent = 4.0/3.0
    !
    ! Initialize thermopack
    call init_thermo(eosLib,eos,mixing,alpha,ncomp,comp_string,nphases,&
       kij_setno=kij_setno,alpha_setno=alpha_setno,b_exponent=b_exponent)

    ! Enable volume-shift
    volumeShiftId = InitVolumeShift(ncomp,comp,'Peneloux','PR')
    ncbeos = size(cbeos)
    do ic=1,ncbeos
      cbeos(ic)%volumeShiftId = volumeShiftId
    enddo

    print *, 'Warning! Should add VTPR specific UNIFAC parameterters...'
    print *, 'See Schmid 2014 (10.1021/ie404118f) or later.'
  end subroutine init_VTPR

  !----------------------------------------------------------------------
  !> Initialize UMR unifac-pr based model
  !> See: 10.1021/ie049580p
  !> \author MH, 2016-12
  !----------------------------------------------------------------------
  subroutine init_UMR(ncomp,comp_string,nphases,&
       kij_setno,alpha_setno)
    use parameters, only: clen
    use volume_shift, only: InitVolumeShift
    use tpvar, only: cbeos, comp
    implicit none
    integer, intent(in) :: ncomp !< Number of components
    character(len=*), intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
    integer, intent(in) :: nphases !< Number of phases
    integer, optional, intent(in) :: kij_setno, alpha_setno !< Data set numbers
    ! Locals
    character(len=clen) :: eosLib !< String defining eos library (TP, DTU, TREND, ThermoPack)
    character(len=clen) :: eos    !< String defining equation of state
    character(len=clen) :: mixing !< String defining mixing rules
    character(len=clen) :: alpha  !< String defining alpha correlation
    integer :: ic, volumeShiftId, ncbeos
    real :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)

    eosLib = 'Thermopack'
    eos = 'PR'
    mixing = 'UMR'
    ! UMR uses MC alpha correlation with polar components,
    ! and otherwise a specialized alpha
    alpha = 'ALPHA_UMR'
    b_exponent = 2.0
    !
    ! Initialize thermopack
    call init_thermo(eosLib,eos,mixing,alpha,ncomp,comp_string,nphases,&
       kij_setno=kij_setno,alpha_setno=alpha_setno,b_exponent=b_exponent)

    ! Enable volume-shift
    volumeShiftId = InitVolumeShift(ncomp,comp,'Peneloux','PR')
    ncbeos = size(cbeos)
    do ic=1,ncbeos
      cbeos(ic)%volumeShiftId = volumeShiftId
    enddo

  end subroutine init_UMR

end module complexmodelinit
