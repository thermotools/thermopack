module gerg
  !> The GERG-2008 Wide-Range Equation of State for Natural Gases and Other Mixtures:
  !! An Expansion of GERG-2004
  !! O. Kunz and W. Wagner
  !! Journal of Chemical & Engineering Data 2012 57 (11), 3032-3091
  !! DOI: 10.1021/je300655b
  !!
  !! \author Morten Hammer, 2023
  use multiparameter_base, only: meos
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH
  use thermopack_var, only: Rgas
  use saturated_densities, only: sat_densities
  implicit none
  save
  private

  real, parameter :: Rgas_star = 8.314510 !< J/mol/K

  !> GERG-2008 multiparameter equations of state.
  type, extends(meos) :: meos_gerg
    !
    integer :: i_comp = 0 !> index of component in gergdatadb
    !
    ! Parameters for the ideal gas part alpha^0
    integer :: n_cosh = 0 !> Number of cosh terms +3
    integer :: n_sinh = 0 !> Number of cosh and sinh terms + 3
    real :: n(3)  !> Constants
    real, allocatable, dimension(:) :: v !> Prefactor ideal terms
    real, allocatable, dimension(:) :: b !> Cosh/sinh parameter for ideal terms
    !
    ! Parameters for the residual gas part alpha^r
    integer :: upPol = 0 !> Number of polynomial terms
    integer :: upExp = 0 !> Number of polynomial and exponential terms
    !
    real, allocatable, dimension(:) :: N_pol !> Prefactor polynomial terms
    real, allocatable, dimension(:) :: N_exp !> Prefactor exponential terms
    !
    real, allocatable, dimension(:) :: t_pol !> Tau exponent polynomial terms
    real, allocatable, dimension(:) :: t_exp !> Tau exponent for prefactor in exponential terms
    !
    integer, allocatable, dimension(:) :: d_pol !> Delta exponent polynomial terms
    integer, allocatable, dimension(:) :: d_exp !> Delta exponent for prefactor in exponential terms
    !
    integer, allocatable, dimension(:) :: l_exp !> Exponent of delta in exponential term

    ! Ancillary equations to compute saturation densities
    type(sat_densities) :: dl !> Parameters liquid saturated density
    type(sat_densities) :: dv !> Parameters vapor saturated density

  contains

    procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_GERG
    procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_GERG
    procedure, public :: satDeltaEstimate => satDeltaEstimate_GERG
    procedure, public :: init => init_GERG
    procedure, private :: alphaResPrefactors => alphaResPrefactors_GERG
    procedure, public :: allocate_param => allocate_param_GERG
    procedure, public :: alpha0_hd_taudelta => alpha0_hd_GERG
    procedure, public :: alphaRes_hd_taudelta => alphaRes_hd_GERG

    ! Assignment operator
    procedure, pass(This), public :: assign_meos => assign_meos_gerg
  end type meos_gerg

  public :: meos_gerg, constructor_gerg

contains

  !> Contruct pure fluid gerg eos
  function constructor_GERG(comp_name) result(gerg_comp)
    use stringmod, only: str_eq
    use gergdatadb, only: maxgerg, gergdb
    use thermopack_var, only: get_active_thermo_model, thermo_model
    character(len=*), intent(in) :: comp_name
    type(meos_gerg) :: gerg_comp
    ! Locals
    type(thermo_model), pointer :: p_thermo
    integer :: i_comp, i, j

    i_comp = -1
    do i=1,maxgerg
      if (str_eq(comp_name, gergdb(i)%ident)) then
        i_comp = i
        exit
      endif
    enddo
    if (i_comp > 0) then
      gerg_comp%i_comp = i_comp
      gerg_comp%Rgas_meos = gergdb(i_comp)%Rgas
      gerg_comp%Rgas_fit = gergdb(i_comp)%Rgas
      gerg_comp%compName = comp_name
      gerg_comp%molarMass = gergdb(i_comp)%mw
      gerg_comp%t_triple = gergdb(i_comp)%ttr
      gerg_comp%p_triple = gergdb(i_comp)%ptr*1.0e-3 ! kPa -> Pa

      gerg_comp%tc = gergdb(i_comp)%tc
      gerg_comp%rc = gergdb(i_comp)%rhoc*1.0e3 !  -> mol/l -> mol/m3
      gerg_comp%acf = gergdb(i_comp)%acf !< Acentric factor

      ! Set indices
      gerg_comp%n_cosh = 3 + gergdb(i_comp)%n_cosh
      gerg_comp%n_sinh = gerg_comp%n_cosh + gergdb(i_comp)%n_sinh
      gerg_comp%upExp = gergdb(i_comp)%n_eos
      j = 0
      do i=1,gergdb(i_comp)%n_eos
        if (gergdb(i_comp)%l_eos(i) /= 0) then
          j = i - 1
          exit
        endif
      enddo
      gerg_comp%upPol = j

      call gerg_comp%allocate_param()
      gerg_comp%n = gergdb(i_comp)%n_id(1:3)
      gerg_comp%v = gergdb(i_comp)%n_id(4:gerg_comp%n_sinh)
      gerg_comp%b = gergdb(i_comp)%t_id(4:gerg_comp%n_sinh)
      !
      gerg_comp%N_pol = gergdb(i_comp)%a_eos(1:gerg_comp%upPol)
      gerg_comp%N_exp = gergdb(i_comp)%a_eos(gerg_comp%upPol+1:gerg_comp%upExp)
      !
      gerg_comp%t_pol = gergdb(i_comp)%t_eos(1:gerg_comp%upPol)
      gerg_comp%t_exp = gergdb(i_comp)%t_eos(gerg_comp%upPol+1:gerg_comp%upExp)
      !
      gerg_comp%d_pol = gergdb(i_comp)%d_eos(1:gerg_comp%upPol)
      gerg_comp%d_exp = gergdb(i_comp)%d_eos(gerg_comp%upPol+1:gerg_comp%upExp)
      !
      gerg_comp%l_exp = gergdb(i_comp)%l_eos(gerg_comp%upPol+1:gerg_comp%upExp)
      !
      call gerg_comp%dl%init(comp_name, LIQPH)
      call gerg_comp%dv%init(comp_name, VAPPH)

      ! Calculate critcal pressure
      call gerg_comp%mp_pressure(gerg_comp%rc, gerg_comp%tc, gerg_comp%pc)
      ! Estimats for triple densities
      gerg_comp%rhoLiq_triple = gerg_comp%dl%density(gerg_comp%t_triple)
      gerg_comp%rhoVap_triple = gerg_comp%dv%density(gerg_comp%t_triple)
    else
      print *,"No parameters for component ",trim(comp_name)
      call stoperror("")
    endif

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = gerg_comp%Rgas_meos
    p_thermo%kRgas = 1000.0*gerg_comp%Rgas_meos !< J/kmol/K
  end function constructor_GERG

  subroutine init_GERG(this, use_Rgas_fit)
    class(meos_gerg) :: this
    logical, optional, intent(in) :: use_Rgas_fit
    ! DUMMY
  end subroutine init_GERG

  subroutine allocate_param_GERG(this)
    class(meos_gerg), intent(inout) :: this
    !
    if (allocated(this%v)) deallocate(this%v)
    if (allocated(this%b)) deallocate(this%b)
    !
    if (allocated(this%N_pol)) deallocate(this%N_pol)
    if (allocated(this%N_exp)) deallocate(this%N_exp)
    !
    if (allocated(this%t_pol)) deallocate(this%t_pol)
    if (allocated(this%t_exp)) deallocate(this%t_exp)
    !
    if (allocated(this%d_pol)) deallocate(this%d_pol)
    if (allocated(this%d_exp)) deallocate(this%d_exp)
    !
    if (allocated(this%l_exp)) deallocate(this%l_exp)
    !
    allocate(this%v(4:this%n_sinh))
    allocate(this%b(4:this%n_sinh))
    !
    allocate(this%N_pol(1:this%upPol))
    allocate(this%N_exp(this%upPol+1:this%upExp))
    !
    allocate(this%t_pol(1:this%upPol))
    allocate(this%t_exp(this%upPol+1:this%upExp))
    !
    allocate(this%d_pol(1:this%upPol))
    allocate(this%d_exp(this%upPol+1:this%upExp))
    !
    allocate(this%l_exp(this%upPol+1:this%upExp))

  end subroutine allocate_param_GERG

  !> Specific reduced Helmholtz energy - ideal gas contributuion
  subroutine alpha0Derivs_GERG(this, delta, tau, alp0)
    class(meos_gerg) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    !
    ! Internals
    real, dimension(4:this%n_sinh) :: exps, sinhx, coshx, a0i, dlna0i, d2lna0i2
    real :: RR
    ! Precalculate exponentials
    exps = exp(this%b*tau)
    sinhx = exps/2 - 1/(2*exps)
    coshx = exps/2 + 1/(2*exps)

    a0i(4:this%n_cosh) = coshx(4:this%n_cosh)
    a0i(this%n_cosh+1:this%n_sinh) = sinhx(this%n_cosh+1:this%n_sinh)

    dlna0i(4:this%n_cosh) = sinhx(4:this%n_cosh)
    dlna0i(this%n_cosh+1:this%n_sinh) = coshx(this%n_cosh+1:this%n_sinh)
    dlna0i = dlna0i/a0i

    d2lna0i2(4:this%n_cosh) = 1/coshx(4:this%n_cosh)**2
    d2lna0i2(this%n_cosh+1:this%n_sinh) = -1/sinhx(this%n_cosh+1:this%n_sinh)**2

    RR = Rgas_star/this%Rgas_fit
    alp0 = 0.0
    alp0(0,0) = log(delta) + RR*(this%n(1) + this%n(2)*tau + this%n(3)*log(tau) + dot_product(this%v, log(a0i)))
    alp0(1,0) = 1.0
    alp0(2,0) = -1.0
    alp0(0,1) = RR*(this%n(2)*tau + this%n(3) + tau*dot_product(this%v*this%b, dlna0i))
    alp0(0,2) = RR*(-this%n(3) + tau*tau*dot_product(this%v*this%b**2, d2lna0i2))

  end subroutine alpha0Derivs_GERG

  !> Specific reduced Helmholtz energy - ideal gas contributuion. Hyperdual numbers.
  function alpha0_hd_GERG(this, delta, tau) result(alp0)
    use hyperdual_mod
    class(meos_gerg) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alp0 !< alp0
    ! Internals
    integer :: i
    real :: RR
    type(hyperdual) :: exps
    RR = Rgas_star/this%Rgas_fit
    alp0 = log(delta) + RR*(this%n(1) + this%n(2)*tau + this%n(3)*log(tau))
    do i=4,this%n_cosh
      exps = cosh(this%b(i)*tau)
      alp0 = alp0 + RR*this%v(i)*log(exps)
    enddo
    do i=this%n_cosh+1,this%n_sinh
      exps = abs(sinh(this%b(i)*tau))
      alp0 = alp0 + RR*this%v(i)*log(exps)
    enddo
  end function alpha0_hd_GERG

  ! Supplies all prefactors that do not depend on delta. Prefactors are cached.
  subroutine alphaResPrefactors_GERG (this, tau, prefactors_pol, prefactors_exp)
    class(meos_gerg) :: this
    real, intent(in) :: tau
    real, intent(out) :: prefactors_pol(1:this%upPol)
    real, intent(out) :: prefactors_exp(this%upPol+1:this%upExp)
    !
    prefactors_pol = this%N_pol * tau**this%t_pol
    prefactors_exp = this%N_exp * tau**this%t_exp

  end subroutine alphaResPrefactors_GERG

  !> Specific residual reduced Helmholtz energy
  subroutine alphaResDerivs_GERG (this, delta, tau, alpr)
    class(meos_gerg) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    ! Internal
    real :: deltaL(this%upPol+1:this%upExp)
    real :: prefactors_pol(1:this%upPol)
    real :: prefactors_exp(this%upPol+1:this%upExp)
    real :: polTerms(1:this%upPol), expTerms(this%upPol+1:this%upExp)
    call this%alphaResPrefactors(tau, prefactors_pol, prefactors_exp)

    ! Precalculate polynomial terms
    polTerms(1:this%upPol) = prefactors_pol*delta**this%d_pol
    ! Precalculate single-exponential terms
    deltaL(this%upPol+1:this%upExp) = delta**this%l_exp
    expTerms(this%upPol+1:this%upExp) = prefactors_exp * delta**this%d_exp * exp(-deltaL)
    ! alpha
    alpr(0,0) = sum(polTerms) + sum(expTerms)

    ! delta*alpha_delta
    alpr(1,0) = dot_product(polTerms, this%d_pol) + dot_product(expTerms, this%d_exp - this%l_exp*deltaL)

    ! delta**2 * alpha_deltadelta
    alpr(2,0) = dot_product(polTerms, this%d_pol*(this%d_pol-1)) + &
         dot_product(expTerms, (this%d_exp - &
         this%l_exp*deltaL)*(this%d_exp-1-this%l_exp*deltaL) - &
         this%l_exp*this%l_exp*deltaL )

    ! tau*alpha_tau
    alpr(0,1) = dot_product(polTerms, this%t_pol) + dot_product(expTerms, this%t_exp)

    ! tau**2 * alpha_tautau
    alpr(0,2) = dot_product(polTerms, this%t_pol*(this%t_pol-1)) + &
         dot_product(expTerms, this%t_exp*(this%t_exp-1))

    ! delta*tau*alpha_deltatau
    alpr(1,1) = dot_product(polTerms, this%d_pol*this%t_pol) + &
         dot_product(expTerms, (this%d_exp - this%l_exp*deltaL)*this%t_exp )

  end subroutine alphaResDerivs_GERG

  !> Specific residual reduced Helmholtz energy. Hyperdual numbers.
  function alphaRes_hd_GERG(this, delta, tau) result(alpr)
    use hyperdual_mod
    class(meos_gerg) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alpr !< alpr
    ! Internal
    integer :: i

    alpr = 0.0_dp
    do i=1,this%upPol
      alpr = alpr + this%N_pol(i) * tau**this%t_pol(i)*delta**this%d_pol(i)
    enddo

    do i=this%upPol+1,this%upExp
      alpr = alpr + this%N_exp(i) * tau**this%t_exp(i) * delta**this%d_exp(i) * exp(-delta**this%l_exp(i))
    enddo

  end function alphaRes_hd_GERG

  !> Estimate saturated densities for density solver
  function satDeltaEstimate_GERG(this,tau,phase) result(deltaSat)
    use thermopack_constants, only: LIQPH, VAPPH
    class(meos_gerg) :: this
    real, intent(in) :: tau
    integer, intent(in) :: phase
    real :: deltaSat
    ! Internals
    real :: T

    T = this%tc/tau
    if (tau<1.0) then
       deltaSat = 1.0
    else if ( phase == LIQPH ) then
      deltaSat = this%dl%density(T)/this%rc
    else if ( phase == VAPPH ) then
      deltaSat = this%dv%density(T)/this%rc
    else
      call stoperror("satDeltaEstimate_GERG: only LIQPH and VAPPH allowed!")
    end if

  end function satDeltaEstimate_GERG

  subroutine assign_meos_gerg(this,other)
    class(meos_gerg), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_gerg)
      call this%assign_meos_base(other)
      !
      this%i_comp = other%i_comp
      !
      this%n_cosh = other%n_cosh
      this%n_sinh = other%n_sinh
      !
      this%n = other%n
      this%v = other%v
      this%b = other%b
      !
      this%upPol = other%upPol
      this%upExp = other%upExp
      !
      this%N_pol = other%N_pol
      this%N_exp = other%N_exp
      !
      this%t_pol = other%t_pol
      this%t_exp = other%t_exp
      !
      this%d_pol = other%d_pol
      this%d_exp = other%d_exp
      !
      this%l_exp = other%l_exp
      !
      this%dl = other%dl
      this%dv = other%dv
    class default
      call stoperror("assign_meos_gerg: Should not be here....")
    end select
  end subroutine assign_meos_gerg

end module gerg
