module gergmix
  !> The GERG-2008 Wide-Range Equation of State for Natural Gases and Other Mixtures:
  !! An Expansion of GERG-2004
  !! O. Kunz and W. Wagner
  !! Journal of Chemical & Engineering Data 2012 57 (11), 3032-3091
  !! DOI: 10.1021/je300655b
  use multiparameter_base, only: meos
  use gerg, only: meos_gerg
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH
  use thermopack_var, only: Rgas
  use saturated_densities, only: sat_densities
  implicit none
  save
  private

  real, parameter :: Rgas_star = 8.314510 !< J/mol/K

  !> GERG-2008 multiparameter equations of state.
  type, extends(meos) :: meos_gergmix

  contains

    procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_GERGMIX
    procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_GERGMIX
    procedure, public :: satDeltaEstimate => satDeltaEstimate_GERGMIX
    procedure, public :: init => init_GERGMIX
    procedure, private :: allocate_param
    procedure, public :: alpha0Derivs_hd_taudelta => alpha0Derivs_hd_GERGMIX
    procedure, public :: alphaResDerivs_hd_taudelta => alphaResDerivs_hd_GERGMIX

    ! Assignment operator
    procedure, pass(This), public :: assign_meos => assign_meos_gergmix
  end type meos_gergmix

  public :: meos_gergmix, constructor_gergmix

contains

  function constructor_GERGMIX(comp_name) result(gerg_comp)
    use stringmod, only: str_eq
    use gergmixdb, only: max_gerg_mix_reducing, gerg_mix_reducingdb, &
         max_gerg_mix_data, gerg_mix_datadb
    use thermopack_var, only: get_active_thermo_model, thermo_model
    character(len=*), intent(in) :: comp_name
    type(meos_gergmix) :: gerg_comp
    ! Locals
    type(thermo_model), pointer :: p_thermo
    !integer :: i_comp, i!, j

    ! i_comp = -1
    ! do i=1,maxgerg
    !   if (str_eq(comp_name, gergdb(i)%ident)) then
    !     i_comp = i
    !     exit
    !   endif
    ! enddo
    ! if (i_comp > 0) then

    ! else
    !   print *,"No parameters for component ",trim(comp_name)
    ! endif

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = gerg_comp%Rgas_meos
    p_thermo%kRgas = 1000.0*gerg_comp%Rgas_meos !< J/kmol/K
  end function constructor_GERGMIX

  subroutine init_GERGMIX(this, use_Rgas_fit)
    class(meos_gergmix) :: this
    logical, optional, intent(in) :: use_Rgas_fit
    ! DUMMY
  end subroutine init_GERGMIX

  subroutine allocate_param(this)
    class(meos_gergmix), intent(inout) :: this
    !
  end subroutine allocate_param

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  subroutine alpha0Derivs_GERGMIX(this, delta, tau, alp0)
    class(meos_gergmix) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    !
    ! Internals

    alp0 = 0.0

  end subroutine alpha0Derivs_GERGMIX

    ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  function alpha0Derivs_hd_GERGMIX(this, delta, tau) result(alp0)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alp0 !< alp0
    ! Internals
    !integer :: i
    !real :: RR
    alp0 = 0.0
  end function alpha0Derivs_hd_GERGMIX

  subroutine alphaResDerivs_GERGMIX (this, delta, tau, alpr)
    class(meos_gergmix) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    ! Internal
    alpr = 0
  end subroutine alphaResDerivs_GERGMIX

  function alphaResDerivs_hd_GERGMIX(this, delta, tau) result(alpr)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alpr !< alpr
    ! Internal
    !integer :: i

    alpr = 0.0_dp

  end function alphaResDerivs_hd_GERGMIX

  function satDeltaEstimate_GERGMIX(this,tau,phase) result(deltaSat)
    use thermopack_constants, only: LIQPH, VAPPH
    class(meos_gergmix) :: this
    real, intent(in) :: tau
    integer, intent(in) :: phase
    real :: deltaSat
    ! Internals
    !real :: T

    deltaSat = 0.0
    ! T = this%tc/tau
    ! if (tau<1.0) then
    !    deltaSat = 1.0
    ! else if ( phase == LIQPH ) then
    !   deltaSat = this%dl%density(T)/this%rc
    ! else if ( phase == VAPPH ) then
    !   deltaSat = this%dv%density(T)/this%rc
    ! else
    !   call stoperror("satDeltaEstimate_GERGMIX: only LIQPH and VAPPH allowed!")
    ! end if

  end function satDeltaEstimate_GERGMIX

  subroutine assign_meos_gergmix(this,other)
    class(meos_gergmix), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_gergmix)
      call this%assign_meos_base(other)
      !
    class default
      call stoperror("assign_meos_gergmix: Should not be here....")
    end select
  end subroutine assign_meos_gergmix

end module gergmix
