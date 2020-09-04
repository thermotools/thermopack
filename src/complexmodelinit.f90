!> Initialization of complex models.
!! These models are typically comprised of several
!! specific sub-models, and when used together they
!! define a known model.
!!
!! \author MH, 2016-12
module complexmodelinit
  !
  use eoslibinit, only: init_thermo
  use thermopack_var, only: nce, get_active_thermo_model, thermo_model
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
       kij_ref,alpha_ref)
    use thermopack_constants, only: clen
    use volume_shift, only: InitVolumeShift
    implicit none
    integer, intent(in) :: ncomp !< Number of components
    character(len=*), intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
    integer, intent(in) :: nphases !< Number of phases
    character(len=*), optional, intent(in) :: kij_ref, alpha_ref !< Data set numbers
    ! Locals
    character(len=clen) :: eos    !< String defining equation of state
    character(len=clen) :: mixing !< String defining mixing rules
    character(len=clen) :: alpha  !< String defining alpha correlation
    integer :: ic, volumeShiftId, ncbeos
    real :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    eos = 'PR'
    mixing = 'VTPR'
    alpha = 'TWU'
    b_exponent = 4.0/3.0
    !
    ! Initialize thermopack
    call init_thermo(eos,mixing,alpha,comp_string,nphases,&
       kij_ref=kij_ref,alpha_ref=alpha_ref,b_exponent=b_exponent)

    ! Enable volume-shift
    volumeShiftId = InitVolumeShift(ncomp,act_mod_ptr%comps,'Peneloux','PR')
    ncbeos = size(act_mod_ptr%eos)
    do ic=1,ncbeos
      act_mod_ptr%eos(ic)%p_eos%volumeShiftId = volumeShiftId
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
       kij_ref,alpha_ref)
    use thermopack_constants, only: clen
    use volume_shift, only: InitVolumeShift
    implicit none
    integer, intent(in) :: ncomp !< Number of components
    character(len=*), intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
    integer, intent(in) :: nphases !< Number of phases
    character(len=*), optional, intent(in) :: kij_ref, alpha_ref !< Data set numbers
    ! Locals
    character(len=clen) :: eos    !< String defining equation of state
    character(len=clen) :: mixing !< String defining mixing rules
    character(len=clen) :: alpha  !< String defining alpha correlation
    integer :: ic, volumeShiftId, ncbeos
    real :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    eos = 'PR'
    mixing = 'UMR'
    ! UMR uses MC alpha correlation with polar components,
    ! and otherwise a specialized alpha
    alpha = 'ALPHA_UMR'
    b_exponent = 2.0
    !
    ! Initialize thermopack
    call init_thermo(eos,mixing,alpha,comp_string,nphases,&
       kij_ref=kij_ref,alpha_ref=alpha_ref,b_exponent=b_exponent)

    ! Enable volume-shift
    volumeShiftId = InitVolumeShift(ncomp,act_mod_ptr%comps,'Peneloux','PR')
    ncbeos = size(act_mod_ptr%eos)
    do ic=1,ncbeos
      act_mod_ptr%eos(ic)%p_eos%volumeShiftId = volumeShiftId
    enddo

  end subroutine init_UMR

end module complexmodelinit
