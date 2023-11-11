module pure_fluid_meos
  !> Multiparameter pure fluid EOS. Functional forms include polinomial, exponential,
  !! Gauss (bell shaped) and non-analytical terms.
  !!
  !! \author Morten Hammer, 2023
  use multiparameter_base, only: meos, REF_NO_SOLVE, REF_EVALUATE_ID, &
       REF_SOLVE_FOR_T, REF_SOLVE_FOR_P
  use gerg, only: meos_gerg
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH, Rgas_default, comp_name_len
  use thermopack_var, only: Rgas
  use hyperdual_mod
  use stringmod, only: str_eq
  implicit none
  save
  private

  !> Constructor for generic multiparameter equations of state.
  type, extends(meos_gerg) :: meos_pure
    !
    real :: t_nbp = 0.0 !> Normal boiling point temperature
    character(len=comp_name_len) :: ref_state !> Reference state
    !
    ! Parameters for the ideal gas part alpha^0
    integer :: n1_id = 0 !> Number of polynomial terms in ideal gas Cp
    integer :: n_id = 0 !> Number of polynomial terms and number of Einstein terms  in ideal gas Cp
    real :: a1_id = 0 !> Integration constant entropy
    real :: a2_id = 0 !> Integration constant enthalpy
    real, allocatable, dimension(:) :: c_id !> Prefactor parameter
    real, allocatable, dimension(:) :: t_id !> Tau exponent
    !
    ! Parameters for the residual gas part alpha^r
    integer :: n_gauss = 0 !> Number of Gauss terms
    integer :: n_nona = 0 !> Number of non-analytical terms
    integer :: n_assoc = 0 !> Number of association terms
    !
    real, allocatable, dimension(:) :: g_exp !> Scaling inside exponential term
    !
    real, allocatable, dimension(:) :: n_g !> Prefactor for Gauss terms
    real, allocatable, dimension(:) :: t_g !> Tau exponent for polynomial prefactor in Gauss terms
    integer, allocatable, dimension(:) :: d_g !> Delta exponent for polynomial prefactor in Gauss terms
    real, allocatable, dimension(:) :: eta_g !> Delta term prefactor inside exponential function of the Gauss terms
    real, allocatable, dimension(:) :: beta_g !> Tau term prefactor inside exponential function of the Gauss terms
    real, allocatable, dimension(:) :: gamma_g !> Tau offset inside exponential function of the Gauss terms
    real, allocatable, dimension(:) :: epsilon_g !> Delta offset inside exponential function of the Gauss terms
    integer, allocatable, dimension(:) :: tauexp_g !> Tau term exponent inside exponential function of the Gauss terms
    integer, allocatable, dimension(:) :: delexp_g !> Delta term exponent inside exponential function of the Gauss terms
    real, allocatable, dimension(:) :: b_assoc_g !> b parameter for modefied Gauss terms used with associating fluids
    !
    real, allocatable, dimension(:) :: n_na !> Prefactor for non-analytical terms
    real, allocatable, dimension(:) :: a_na !> a-parameter non-analytical terms
    real, allocatable, dimension(:) :: b_na !> b-parameter non-analytical terms
    real, allocatable, dimension(:) :: beta_na !> beta-parameter non-analytical terms
    real, allocatable, dimension(:) :: big_a_na !> A-parameter non-analytical terms
    real, allocatable, dimension(:) :: big_b_na !> B-parameter non-analytical terms
    real, allocatable, dimension(:) :: big_c_na !> C-parameter non-analytical terms
    real, allocatable, dimension(:) :: big_d_na !> D-parameter non-analytical terms
    !
  contains

    procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_meos_pure
    procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_meos_pure
    procedure, public :: init => init_meos_pure
    procedure, private :: alphaResPrefactors => alphaResPrefactors_meos_pure
    procedure, public :: allocate_param => allocate_param_meos_pure
    procedure, public :: alpha0_hd_taudelta => alpha0_hd_meos_pure
    procedure, public :: alphaRes_hd_taudelta => alphaRes_hd_meos_pure
    procedure, public :: mp_pressure => mp_pressure_meos_pure
    procedure, public :: satDeltaEstimate => satDeltaEstimate_meos_pure
    procedure, public :: get_ref_state_spec => get_ref_state_spec_meos_pure
    procedure, public :: set_ref_state => set_ref_state_meos_pure
    ! Assignment operator
    procedure, pass(This), public :: assign_meos => assign_meos_pure
  end type meos_pure

  public :: meos_pure, constructor_meos_pure

contains

  !> meos_pure constructor
  function constructor_meos_pure(comp_name) result(meos_comp)
    use stringmod, only: str_eq
    use meosdatadb, only: maxmeos, meosdb
    use thermopack_var, only: get_active_thermo_model, thermo_model
    character(len=*), intent(in) :: comp_name
    type(meos_pure) :: meos_comp
    ! Locals
    type(thermo_model), pointer :: p_thermo
    integer :: i_comp, i
    i_comp = -1
    do i=1,maxmeos
      if (str_eq(comp_name, meosdb(i)%ident)) then
        i_comp = i
        exit
      endif
    enddo
    if (i_comp > 0) then
      meos_comp%i_comp = i_comp
      meos_comp%Rgas_meos = meosdb(i_comp)%Rgas
      meos_comp%Rgas_fit = meosdb(i_comp)%Rgas
      meos_comp%compName = comp_name
      meos_comp%molarMass = meosdb(i_comp)%mw
      meos_comp%t_triple = meosdb(i_comp)%ttr
      meos_comp%p_triple = meosdb(i_comp)%ptr*1.0e-3 ! kPa -> Pa

      meos_comp%tr = meosdb(i_comp)%tr
      meos_comp%rhor = meosdb(i_comp)%rhor*1.0e3 !  -> mol/l -> mol/m3

      meos_comp%tc = meosdb(i_comp)%tc
      meos_comp%rc = meosdb(i_comp)%rhoc*1.0e3 !  -> mol/l -> mol/m3
      meos_comp%acf = meosdb(i_comp)%acf !< Acentric factor

      meos_comp%t_nbp = meosdb(i_comp)%t_nbp
      meos_comp%ref_state = meosdb(i_comp)%default_ref_state

      ! Set indices
      meos_comp%n_cosh = 0
      meos_comp%n_sinh = 4 ! Avoid error allocating GERG arrays
      meos_comp%upPol = meosdb(i_comp)%n_poly_eos
      meos_comp%upExp = meosdb(i_comp)%n_exp_eos + meosdb(i_comp)%n_poly_eos
      meos_comp%n_gauss = meosdb(i_comp)%n_gauss_eos
      meos_comp%n_nona = meosdb(i_comp)%n_nona_eos
      meos_comp%n_assoc = meosdb(i_comp)%n_assoc_eos
      meos_comp%n1_id = meosdb(i_comp)%n1_id
      meos_comp%n_id = meosdb(i_comp)%n_id

      call meos_comp%allocate_param()
      ! Read CP paramaters
      meos_comp%c_id = meosdb(i_comp)%c_id(1:meos_comp%n_id)
      meos_comp%t_id = meosdb(i_comp)%t_id(1:meos_comp%n_id)
      !
      meos_comp%N_pol = meosdb(i_comp)%n_eos(1:meos_comp%upPol)
      meos_comp%N_exp = meosdb(i_comp)%n_eos(meos_comp%upPol+1:meos_comp%upExp)
      !
      meos_comp%t_pol = meosdb(i_comp)%t_eos(1:meos_comp%upPol)
      meos_comp%t_exp = meosdb(i_comp)%t_eos(meos_comp%upPol+1:meos_comp%upExp)
      !
      meos_comp%d_pol = meosdb(i_comp)%d_eos(1:meos_comp%upPol)
      meos_comp%d_exp = meosdb(i_comp)%d_eos(meos_comp%upPol+1:meos_comp%upExp)
      !
      meos_comp%l_exp = meosdb(i_comp)%l_eos(meos_comp%upPol+1:meos_comp%upExp)
      meos_comp%g_exp = meosdb(i_comp)%g_eos(meos_comp%upPol+1:meos_comp%upExp)
      !
      meos_comp%n_g = meosdb(i_comp)%n_eos(meos_comp%upExp + 1:meos_comp%upExp + meos_comp%n_gauss)
      meos_comp%t_g = meosdb(i_comp)%t_eos(meos_comp%upExp + 1:meos_comp%upExp + meos_comp%n_gauss)
      meos_comp%d_g = meosdb(i_comp)%d_eos(meos_comp%upExp + 1:meos_comp%upExp + meos_comp%n_gauss)
      meos_comp%eta_g = meosdb(i_comp)%eta_eos(1:meos_comp%n_gauss)
      meos_comp%beta_g = meosdb(i_comp)%beta_eos(1:meos_comp%n_gauss)
      meos_comp%gamma_g = meosdb(i_comp)%gamma_eos(1:meos_comp%n_gauss)
      meos_comp%epsilon_g = meosdb(i_comp)%epsilon_eos(1:meos_comp%n_gauss)
      meos_comp%tauexp_g = meosdb(i_comp)%tau_exp_eos(1:meos_comp%n_gauss)
      meos_comp%delexp_g = meosdb(i_comp)%del_exp_eos(1:meos_comp%n_gauss)
      meos_comp%b_assoc_g = meosdb(i_comp)%b_assoc_eos(1:meos_comp%n_assoc)
      !
      meos_comp%n_na = meosdb(i_comp)%n_na(1:meos_comp%n_nona)
      meos_comp%a_na = meosdb(i_comp)%a_na(1:meos_comp%n_nona)
      meos_comp%b_na = meosdb(i_comp)%b_na(1:meos_comp%n_nona)
      meos_comp%beta_na = meosdb(i_comp)%beta_na(1:meos_comp%n_nona)
      meos_comp%big_a_na = meosdb(i_comp)%big_a_na(1:meos_comp%n_nona)
      meos_comp%big_b_na = meosdb(i_comp)%big_b_na(1:meos_comp%n_nona)
      meos_comp%big_c_na = meosdb(i_comp)%big_c_na(1:meos_comp%n_nona)
      meos_comp%big_d_na = meosdb(i_comp)%big_d_na(1:meos_comp%n_nona)
      !
      ! Set Rgas
      p_thermo => get_active_thermo_model()
      p_thermo%Rgas = meos_comp%Rgas_meos
      p_thermo%kRgas = 1000.0*p_thermo%Rgas !< J/kmol/K

      ! Calculate critcal pressure
      call meos_comp%mp_pressure(meos_comp%rc, meos_comp%tc, meos_comp%pc)

      ! Set reference entropy/enthalpy
      meos_comp%a1_id = meosdb(i_comp)%a1_id
      meos_comp%a2_id = meosdb(i_comp)%a2_id

    else
      print *,"No parameters for component ",trim(comp_name)
      call stoperror("")
    endif

  end function constructor_meos_pure

  !> Get information on reference state
  subroutine get_ref_state_spec_meos_pure(this, ref_state, T, P, phase, solve)
    class(meos_pure) :: this
    character(len=*), intent(in) :: ref_state
    real, intent(out) :: T, P
    integer, intent(out) :: phase
    integer, intent(out) :: solve
    ! Locals
    if (.not. str_eq(ref_state,"DEFAULT")) then
      this%ref_state = ref_state
    endif
    P = -1.0
    if (str_eq(this%ref_state, "IIR")) then
      ! The value of specific enthalpy is set to 200 kJ/kg and the value
      ! of specific entropy is set to 1.0 kJ/kg-K for saturated liquid at 0 C (273.15 K).
      ! This is the standard reference state for the
      ! International Institute of Refrigeration (IIR).
      T = 273.15
      solve = REF_SOLVE_FOR_P
    else if (str_eq(this%ref_state, "NBP")) then
      ! Enthalpy and entropy to zero for the saturated liquid at the normal boiling point (NBP)
      P = 1.01325e5
      T = this%t_nbp
      solve = REF_SOLVE_FOR_T
    else if (str_eq(this%ref_state, "ASHRAE")) then
      ! Setting enthalpy and entropy to zero for the saturated liquid at -40 °C.
      T = 273.15 - 40.0
      solve = REF_SOLVE_FOR_P
    else if (str_eq(this%ref_state, "IDGAS")) then
      ! Setting the gas enthalpy and entropy to zero at 298.15 K and 1 atm
      P = 1.01325e5
      T = 298.15
      solve = REF_EVALUATE_ID
    else if (str_eq(this%ref_state, "TRIPLE_POINT")) then
      ! Setting internal energy and entropy to zero for liquid at the triple point
      T = this%t_triple
      P = this%p_triple
      solve = REF_SOLVE_FOR_P
    else
      call stoperror("pure_fluid_meos: Wrong reference state")
    endif

  end subroutine get_ref_state_spec_meos_pure

  !> Set calculated reference state
  subroutine set_ref_state_meos_pure(this, T, P, v, h, s)
    class(meos_pure) :: this
    real, intent(in) :: T, P, v, h, s
    ! Locals
    real :: s_ref, h_ref

    if (str_eq(this%ref_state, "IIR")) then
      ! The value of specific enthalpy is set to 200 kJ/kg and the value
      ! of specific entropy is set to 1.0 kJ/kg-K for saturated liquid at 0 C (273.15 K).
      ! This is the standard reference state for the
      ! International Institute of Refrigeration (IIR).
      s_ref = 1.0_dp*this%molarMass ! kJ/kg-K * g/mol = J/mol/K
      h_ref = 200.0_dp*this%molarMass ! kJ/kg * g/mol = J/mol
    else if (str_eq(this%ref_state, "NBP")) then
      ! Enthalpy and entropy to zero for the saturated liquid at the normal boiling point (NBP)
      s_ref = 0
      h_ref = 0
    else if (str_eq(this%ref_state, "ASHRAE")) then
      ! Setting enthalpy and entropy to zero for the saturated liquid at -40 °C.
      s_ref = 0
      h_ref = 0
    else if (str_eq(this%ref_state, "IDGAS")) then
      ! Setting the gas enthalpy and entropy to zero at 298.15 K and 1 atm
      s_ref = 0
      h_ref = 0
      ! Legacy support for AR, N2 and O2
      if (str_eq(this%compName, "AR")) then
        h_ref = 6197.0
        s_ref = 154.737
      else if (str_eq(this%compName, "N2")) then
        h_ref = 8670.0
        s_ref = 191.5
      else if (str_eq(this%compName, "O2")) then
        h_ref = 8680.0
        s_ref = 205.043
      endif
    else if (str_eq(this%ref_state, "TRIPLE_POINT")) then
      ! Setting internal energy and entropy to zero for liquid at the triple point
      s_ref = 0
      h_ref = P*v
    else
      call stoperror("pure_fluid_meos: Wrong reference state")
    endif

    ! Correct entropy
    this%a1_id = this%a1_id - (s_ref-s)/this%Rgas_meos

    ! Correct enthalpy
    this%a2_id = this%a2_id + (h_ref-h)/(this%Rgas_meos*this%tc)

  end subroutine set_ref_state_meos_pure

  subroutine init_meos_pure(this, use_Rgas_fit)
    class(meos_pure) :: this
    logical, optional, intent(in) :: use_Rgas_fit
    ! DUMMY
  end subroutine init_meos_pure

  subroutine allocate_param_meos_pure(this)
    class(meos_pure), intent(inout) :: this
    !
    call this%meos_gerg%allocate_param()
    !
    if (allocated(this%g_exp)) deallocate(this%g_exp)
    !
    if (allocated(this%n_g)) deallocate(this%n_g)
    if (allocated(this%t_g)) deallocate(this%t_g)
    if (allocated(this%d_g)) deallocate(this%d_g)
    if (allocated(this%eta_g)) deallocate(this%eta_g)
    if (allocated(this%beta_g)) deallocate(this%beta_g)
    if (allocated(this%gamma_g)) deallocate(this%gamma_g)
    if (allocated(this%epsilon_g)) deallocate(this%epsilon_g)
    if (allocated(this%tauexp_g)) deallocate(this%tauexp_g)
    if (allocated(this%delexp_g)) deallocate(this%delexp_g)
    if (allocated(this%b_assoc_g)) deallocate(this%b_assoc_g)
    !
    if (allocated(this%n_na)) deallocate(this%n_na)
    if (allocated(this%a_na)) deallocate(this%a_na)
    if (allocated(this%b_na)) deallocate(this%b_na)
    if (allocated(this%beta_na)) deallocate(this%beta_na)
    if (allocated(this%big_a_na)) deallocate(this%big_a_na)
    if (allocated(this%big_b_na)) deallocate(this%big_b_na)
    if (allocated(this%big_c_na)) deallocate(this%big_c_na)
    if (allocated(this%big_d_na)) deallocate(this%big_d_na)
    !
    if (allocated(this%c_id)) deallocate(this%c_id)
    if (allocated(this%c_id)) deallocate(this%t_id)
    !
    allocate(this%g_exp(this%upPol+1:this%upExp))
    !
    allocate(this%n_g(1:this%n_gauss))
    allocate(this%t_g(1:this%n_gauss))
    allocate(this%d_g(1:this%n_gauss))
    allocate(this%eta_g(1:this%n_gauss))
    allocate(this%beta_g(1:this%n_gauss))
    allocate(this%gamma_g(1:this%n_gauss))
    allocate(this%epsilon_g(1:this%n_gauss))
    allocate(this%tauexp_g(1:this%n_gauss))
    allocate(this%delexp_g(1:this%n_gauss))
    allocate(this%b_assoc_g(1:this%n_assoc))
    !
    allocate(this%n_na(1:this%n_nona))
    allocate(this%a_na(1:this%n_nona))
    allocate(this%b_na(1:this%n_nona))
    allocate(this%beta_na(1:this%n_nona))
    allocate(this%big_a_na(1:this%n_nona))
    allocate(this%big_b_na(1:this%n_nona))
    allocate(this%big_c_na(1:this%n_nona))
    allocate(this%big_d_na(1:this%n_nona))
    !
    allocate(this%c_id(this%n_id))
    allocate(this%t_id(this%n_id))
    !
  end subroutine allocate_param_meos_pure

  !> Specific reduced Helmholtz energy - ideal gas contributuion
  subroutine alpha0Derivs_meos_pure(this, delta, tau, alp0)
    class(meos_pure) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    !
    alp0 = 0
    call stoperror("alpha0Derivs not implemented")
  end subroutine alpha0Derivs_meos_pure

  !> Specific reduced Helmholtz energy - ideal gas contributuion. HYperdual numbers.
  function alpha0_hd_meos_pure(this, delta, tau) result(alp0)
    use hyperdual_mod
    class(meos_pure) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alp0 !< alp0
    ! Internals
    integer :: i, i_start
    alp0 = log(delta) + this%a1_id + this%a2_id*tau - log(tau)

    if (this%t_id(1) < 1.0e-12) then
      i_start = 2
      alp0 = alp0 + this%c_id(1)*log(tau)
    else
      i_start = 1
     endif
    do i=i_start,this%n1_id
      alp0 = alp0 - this%c_id(i) * this%tc**this%t_id(i) * tau**(-this%t_id(i)) / &
           (this%t_id(i)*(this%t_id(i) + 1.0_dp))
    enddo
    do i=this%n1_id+1,this%n_id
      alp0 = alp0 + this%c_id(i)*log(1.0_dp - exp(-tau*this%t_id(i)/this%tc))
    enddo
  end function alpha0_hd_meos_pure

  ! Supplies all prefactors that do not depend on delta. Prefactors are cached.
  subroutine alphaResPrefactors_meos_pure (this, tau, prefactors_pol, prefactors_exp)
    class(meos_pure) :: this
    real, intent(in) :: tau
    real, intent(out) :: prefactors_pol(1:this%upPol)
    real, intent(out) :: prefactors_exp(this%upPol+1:this%upExp)
    !
    prefactors_pol = 0
    prefactors_exp = 0
    call stoperror("alphaResPrefactors not implemented")
  end subroutine alphaResPrefactors_meos_pure

  !> Specific reduced residual Helmholtz energy
  subroutine alphaResDerivs_meos_pure (this, delta, tau, alpr)
    class(meos_pure) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    !
    alpr = 0
    call stoperror("alphaResDerivs not implemented")
  end subroutine alphaResDerivs_meos_pure

  !> Specific reduced residual Helmholtz energy - hyperdual numbers
  function alphaRes_hd_meos_pure(this, delta, tau) result(alpr)
    use hyperdual_mod
    class(meos_pure) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alpr !< alpr
    ! Internal
    integer :: i, j
    type(hyperdual) :: del
    !
    alpr = 0.0_dp
    do i=1,this%upPol
      alpr = alpr + this%N_pol(i) * tau**this%t_pol(i)*delta**this%d_pol(i)
    enddo

    do i=this%upPol+1,this%upExp
      alpr = alpr + this%N_exp(i) * tau**this%t_exp(i) * delta**this%d_exp(i) * exp(-this%g_exp(i)*delta**this%l_exp(i))
    enddo

    do i=1,this%n_gauss-this%n_assoc
      alpr = alpr + this%N_g(i) * tau**this%t_g(i) * delta**this%d_g(i) * &
           exp(this%eta_g(i)*(delta-this%epsilon_g(i))**this%delexp_g(i) + &
           this%beta_g(i)*(tau-this%gamma_g(i))**this%tauexp_g(i))
    enddo

    do i=1,this%n_assoc
      j = this%n_gauss-this%n_assoc + i
      alpr = alpr + this%N_g(j) * tau**this%t_g(j) * delta**this%d_g(j) * &
           exp(this%eta_g(j)*(delta-this%epsilon_g(j))**this%delexp_g(j) + &
           1.0_dp/(this%beta_g(j)*(tau-this%gamma_g(j))**this%tauexp_g(j) + this%b_assoc_g(i)))
    enddo

    do i=1,this%n_nona
      del = ((1.0_dp - tau) + this%big_a_na(i)*((delta-1.0_dp)**2)**(1.0_dp/(2.0_dp*this%beta_na(i))))**2 + &
           this%big_b_na(i)*((delta-1.0_dp)**2)**this%a_na(i)
      alpr = alpr + this%n_na(i) * del**this%b_na(i) * delta * &
           exp(-this%big_c_na(i)*(delta-1.0_dp)**2 - &
           this%big_d_na(i)*(tau-1.0_dp)**2)
    enddo

  end function alphaRes_hd_meos_pure

  subroutine assign_meos_pure(this,other)
    class(meos_pure), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_pure)
      call this%meos_gerg%assign_meos(other)
      !
      this%n1_id = other%n1_id
      this%n_id = other%n_id
      this%a1_id = other%a1_id
      this%a2_id = other%a2_id
      this%c_id = other%c_id
      this%t_id = other%t_id
      !
      this%n_gauss = other%n_gauss
      this%n_nona = other%n_nona
      this%n_assoc = other%n_assoc
      !
      this%g_exp = other%g_exp
      !
      this%n_g = other%n_g
      this%t_g = other%t_g
      this%d_g = other%d_g
      this%eta_g = other%eta_g
      this%beta_g = other%beta_g
      this%gamma_g = other%gamma_g
      this%epsilon_g = other%epsilon_g
      this%tauexp_g = other%tauexp_g
      this%delexp_g = other%delexp_g
      this%b_assoc_g = other%b_assoc_g
      !
      this%n_na = other%n_na
      this%a_na = other%a_na
      this%b_na = other%b_na
      this%beta_na = other%beta_na
      this%big_a_na = other%big_a_na
      this%big_b_na = other%big_b_na
      this%big_c_na = other%big_c_na
      this%big_d_na = other%big_d_na
      !
    class default
      call stoperror("assign_meos_pure: Should not be here....")
    end select
  end subroutine assign_meos_pure

  !> Pressure and (optionally) its derivatives
  subroutine mp_pressure_meos_pure(this, rho, T, p, p_rho, p_T)
    use hyperdual_mod
    class(meos_pure) :: this
    real, intent(in) :: rho, T
    real, intent(out) :: p
    real, optional, intent(out) :: p_rho, p_T
    ! Internal
    type(hyperdual) :: T_hd, v_hd, f_res_hd, delta_hd, tau_hd
    T_hd = T
    v_hd%f0 = 1.0/rho
    v_hd%f1 = 1.0_dp
    v_hd%f2 = 1.0_dp
    delta_hd = 1.0_dp/(v_hd*this%rc)
    tau_hd = this%tc/T_hd
    f_res_hd = this%alphaRes_hd_taudelta(delta_hd, tau_hd)
    p = -this%Rgas_meos*T*(f_res_hd%f1 - rho)
    if (present(p_rho)) p_rho = this%Rgas_meos*T*(f_res_hd%f12/rho**2 + 1.0_dp)
    if (present(p_T)) then
      v_hd%f2 = 0.0_dp
      delta_hd = 1.0_dp/(v_hd*this%rc)
      T_hd%f2 = 1.0_dp
      tau_hd = this%tc/T_hd
      f_res_hd = this%alphaRes_hd_taudelta(delta_hd, tau_hd)
      p_T = -this%Rgas_meos*(T*f_res_hd%f12 + f_res_hd%f1 - rho)
    endif
  end subroutine mp_pressure_meos_pure

  !> Provide initial guess for liquid and vapor densities
  function satDeltaEstimate_meos_pure(this,tau,phase) result(deltaSat)
    use thermopack_constants, only: LIQPH, VAPPH
    class(meos_pure) :: this
    real, intent(in) :: tau
    integer, intent(in) :: phase
    real :: deltaSat
    ! Internals
    real :: b, rho

    if (tau<1.0) then
       deltaSat = 1.0
     else if ( phase == LIQPH ) then
       b = 0.0778*this%Rgas_meos*this%tc/this%pc ! Peng-Robinson
       rho = 1.0/(1.01 * b)
       deltaSat = rho/this%rc
    else if ( phase == VAPPH ) then
      deltaSat = 0.1
    else
      call stoperror("satDeltaEstimate_meos_pure: only LIQPH and VAPPH allowed!")
    end if

  end function satDeltaEstimate_meos_pure

end module pure_fluid_meos
