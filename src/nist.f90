module nist
  !>
  use multiparameter_base, only: meos
  use gerg, only: meos_gerg
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH, Rgas_default
  use thermopack_var, only: Rgas
  implicit none
  save
  private

  !> Generig multiparameter equations of state.
  type, extends(meos_gerg) :: meos_nist
    !
    ! Parameters for the ideal gas part alpha^0
    integer :: n1_id = 0
    integer :: n_id = 0
    real :: a1_id = 0
    real :: a2_id = 0
    real, allocatable, dimension(:) :: c_id
    real, allocatable, dimension(:) :: t_id
    !
    ! Parameters for the residual gas part alpha^r
    integer :: n_gauss = 0
    integer :: n_nona = 0
    !
    real, allocatable, dimension(:) :: n_g
    real, allocatable, dimension(:) :: t_g
    integer, allocatable, dimension(:) :: d_g
    real, allocatable, dimension(:) :: eta_g
    real, allocatable, dimension(:) :: beta_g
    real, allocatable, dimension(:) :: gamma_g
    real, allocatable, dimension(:) :: epsilon_g
    !
    real, allocatable, dimension(:) :: n_na
    real, allocatable, dimension(:) :: a_na
    real, allocatable, dimension(:) :: b_na
    real, allocatable, dimension(:) :: beta_na
    real, allocatable, dimension(:) :: big_a_na
    real, allocatable, dimension(:) :: big_b_na
    real, allocatable, dimension(:) :: big_c_na
    real, allocatable, dimension(:) :: big_d_na
    !
  contains

    procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_NIST
    procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_NIST
    procedure, public :: init => init_NIST
    procedure, private :: alphaResPrefactors => alphaResPrefactors_NIST
    procedure, public :: allocate_param => allocate_param_nist
    procedure, public :: alpha0_hd_taudelta => alpha0_hd_NIST
    procedure, public :: alphaRes_hd_taudelta => alphaRes_hd_NIST
    procedure, public :: mp_pressure => mp_pressure_NIST

    ! Assignment operator
    procedure, pass(This), public :: assign_meos => assign_meos_nist
  end type meos_nist

  public :: meos_nist, constructor_nist

contains

  function constructor_NIST(comp_name) result(nist_comp)
    use stringmod, only: str_eq
    use nistdatadb, only: maxnist, nistdb
    use thermopack_var, only: get_active_thermo_model, thermo_model
    character(len=*), intent(in) :: comp_name
    type(meos_nist) :: nist_comp
    ! Locals
    type(thermo_model), pointer :: p_thermo
    integer :: i_comp, i
    real :: p1, p2, p_rho, p_T
    i_comp = -1
    do i=1,maxnist
      if (str_eq(comp_name, nistdb(i)%ident)) then
        i_comp = i
        exit
      endif
    enddo
    if (i_comp > 0) then
      nist_comp%i_comp = i_comp
      nist_comp%Rgas_meos = nistdb(i_comp)%Rgas
      nist_comp%Rgas_fit = nistdb(i_comp)%Rgas
      nist_comp%compName = comp_name
      nist_comp%molarMass = nistdb(i_comp)%mw
      nist_comp%t_triple = nistdb(i_comp)%ttr
      nist_comp%p_triple = nistdb(i_comp)%ptr*1.0e-3 ! kPa -> Pa

      nist_comp%tc = nistdb(i_comp)%tc
      nist_comp%rc = nistdb(i_comp)%rhoc*1.0e3 !  -> mol/l -> mol/m3
      nist_comp%acf = nistdb(i_comp)%acf !< Acentric factor

      ! Set indices
      nist_comp%n_cosh = 0
      nist_comp%n_sinh = 4 ! Avoid error allocating GERG arrays
      nist_comp%upPol = nistdb(i_comp)%n_poly_eos
      nist_comp%upExp = nistdb(i_comp)%n_exp_eos + nistdb(i_comp)%n_poly_eos
      nist_comp%n_gauss = nistdb(i_comp)%n_gauss_eos
      nist_comp%n_nona = nistdb(i_comp)%n_nona_eos
      nist_comp%n1_id = nistdb(i_comp)%n1_id
      nist_comp%n_id = nistdb(i_comp)%n_id

      call nist_comp%allocate_param()
      ! Read CP paramaters
      nist_comp%c_id = nistdb(i_comp)%c_id(1:nist_comp%n_id)
      nist_comp%t_id = nistdb(i_comp)%t_id(1:nist_comp%n_id)
      !
      nist_comp%N_pol = nistdb(i_comp)%n_eos(1:nist_comp%upPol)
      nist_comp%N_exp = nistdb(i_comp)%n_eos(nist_comp%upPol+1:nist_comp%upExp)
      !
      nist_comp%t_pol = nistdb(i_comp)%t_eos(1:nist_comp%upPol)
      nist_comp%t_exp = nistdb(i_comp)%t_eos(nist_comp%upPol+1:nist_comp%upExp)
      !
      nist_comp%d_pol = nistdb(i_comp)%d_eos(1:nist_comp%upPol)
      nist_comp%d_exp = nistdb(i_comp)%d_eos(nist_comp%upPol+1:nist_comp%upExp)
      !
      nist_comp%l_exp = nistdb(i_comp)%l_eos(nist_comp%upPol+1:nist_comp%upExp)
      !
      nist_comp%n_g = nistdb(i_comp)%n_eos(nist_comp%upExp + 1:nist_comp%upExp + nist_comp%n_gauss)
      nist_comp%t_g = nistdb(i_comp)%t_eos(nist_comp%upExp + 1:nist_comp%upExp + nist_comp%n_gauss)
      nist_comp%d_g = nistdb(i_comp)%d_eos(nist_comp%upExp + 1:nist_comp%upExp + nist_comp%n_gauss)
      nist_comp%eta_g = nistdb(i_comp)%eta_eos(1:nist_comp%n_gauss)
      nist_comp%beta_g = nistdb(i_comp)%beta_eos(1:nist_comp%n_gauss)
      nist_comp%gamma_g = nistdb(i_comp)%gamma_eos(1:nist_comp%n_gauss)
      nist_comp%epsilon_g = nistdb(i_comp)%epsilon_eos(1:nist_comp%n_gauss)
      !
      nist_comp%n_na = nistdb(i_comp)%n_na(1:nist_comp%n_nona)
      nist_comp%a_na = nistdb(i_comp)%a_na(1:nist_comp%n_nona)
      nist_comp%b_na = nistdb(i_comp)%b_na(1:nist_comp%n_nona)
      nist_comp%beta_na = nistdb(i_comp)%beta_na(1:nist_comp%n_nona)
      nist_comp%big_a_na = nistdb(i_comp)%big_a_na(1:nist_comp%n_nona)
      nist_comp%big_b_na = nistdb(i_comp)%big_b_na(1:nist_comp%n_nona)
      nist_comp%big_c_na = nistdb(i_comp)%big_c_na(1:nist_comp%n_nona)
      nist_comp%big_d_na = nistdb(i_comp)%big_d_na(1:nist_comp%n_nona)
      !
      ! Calculate critcal pressure
      call nist_comp%mp_pressure(nist_comp%rc, nist_comp%tc, nist_comp%pc) !, p_rho, p_T

      ! Set reference entropy/enthalpy
      !nist_comp%a1_id = 0.0_dp
      !nist_comp%a2_id = 0.0_dp

    else
      print *,"No parameters for component ",trim(comp_name)
    endif

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = Rgas_default
    p_thermo%kRgas = 1000.0*Rgas_default !< J/kmol/K
  end function constructor_NIST

  subroutine init_NIST(this, use_Rgas_fit)
    class(meos_nist) :: this
    logical, optional, intent(in) :: use_Rgas_fit
    ! DUMMY
  end subroutine init_NIST

  subroutine allocate_param_nist(this)
    class(meos_nist), intent(inout) :: this
    !
    call this%meos_gerg%allocate_param()
    !
    if (allocated(this%n_g)) deallocate(this%n_g)
    if (allocated(this%t_g)) deallocate(this%t_g)
    if (allocated(this%d_g)) deallocate(this%d_g)
    if (allocated(this%eta_g)) deallocate(this%eta_g)
    if (allocated(this%beta_g)) deallocate(this%beta_g)
    if (allocated(this%gamma_g)) deallocate(this%gamma_g)
    if (allocated(this%epsilon_g)) deallocate(this%epsilon_g)
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
    allocate(this%n_g(1:this%n_gauss))
    allocate(this%t_g(1:this%n_gauss))
    allocate(this%d_g(1:this%n_gauss))
    allocate(this%eta_g(1:this%n_gauss))
    allocate(this%beta_g(1:this%n_gauss))
    allocate(this%gamma_g(1:this%n_gauss))
    allocate(this%epsilon_g(1:this%n_gauss))
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
  end subroutine allocate_param_nist

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  subroutine alpha0Derivs_NIST(this, delta, tau, alp0)
    class(meos_nist) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    !
    alp0 = 0
    call stoperror("alpha0Derivs not implemented")
  end subroutine alpha0Derivs_NIST

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  function alpha0_hd_NIST(this, delta, tau) result(alp0)
    use hyperdual_mod
    class(meos_nist) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alp0 !< alp0
    ! Internals
    integer :: i
    alp0 = log(delta) + this%a1_id + this%a2_id*tau + (this%c_id(1) - 1.0_dp)*log(tau)
    do i=2,this%n1_id
      alp0 = alp0 - this%c_id(i) * this%tc**this%t_id(i) * tau**(this%t_id(i)) / &
           (this%t_id(i)*(this%t_id(i) + 1.0_dp))
    enddo
    do i=this%n1_id+1,this%n_id
      alp0 = alp0 + this%c_id(i)*log(1.0_dp - exp(-tau*this%t_id(i)/this%tc))
    enddo
  end function alpha0_hd_NIST

  ! Supplies all prefactors that do not depend on delta. Prefactors are cached.
  subroutine alphaResPrefactors_NIST (this, tau, prefactors_pol, prefactors_exp)
    class(meos_nist) :: this
    real, intent(in) :: tau
    real, intent(out) :: prefactors_pol(1:this%upPol)
    real, intent(out) :: prefactors_exp(this%upPol+1:this%upExp)
    !
    prefactors_pol = 0
    prefactors_exp = 0
    call stoperror("alphaResPrefactors not implemented")
  end subroutine alphaResPrefactors_NIST

  subroutine alphaResDerivs_NIST (this, delta, tau, alpr)
    class(meos_nist) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    !
    alpr = 0
    call stoperror("alphaResDerivs not implemented")
  end subroutine alphaResDerivs_NIST

  function alphaRes_hd_NIST(this, delta, tau) result(alpr)
    use hyperdual_mod
    class(meos_nist) :: this
    type(hyperdual), intent(in) :: delta, tau
    type(hyperdual) :: alpr !< alpr
    ! Internal
    integer :: i
    type(hyperdual) :: del
    !
    alpr = 0.0_dp
    do i=1,this%upPol
      alpr = alpr + this%N_pol(i) * tau**this%t_pol(i)*delta**this%d_pol(i)
    enddo

    do i=this%upPol+1,this%upExp
      alpr = alpr + this%N_exp(i) * tau**this%t_exp(i) * delta**this%d_exp(i) * exp(-delta**this%l_exp(i))
    enddo

    do i=1,this%n_gauss
      alpr = alpr + this%N_g(i) * tau**this%t_g(i) * delta**this%d_g(i) * &
           exp(this%eta_g(i)*(delta-this%epsilon_g(i))**2 + &
           this%beta_g(i)*(tau-this%gamma_g(i))**2)
    enddo

    do i=1,this%n_nona
      del = ((1.0_dp - tau) + this%big_a_na(i)*((delta-1.0_dp)**2)**(1.0_dp/(2.0_dp*this%beta_na(i))))**2 + &
           this%big_b_na(i)*((delta-1.0_dp)**2)**this%a_na(i)
      alpr = alpr + this%n_na(i) * del**this%b_na(i) * delta * &
           exp(-this%big_c_na(i)*(delta-1.0_dp)**2 - &
           this%big_d_na(i)*(tau-1.0_dp)**2)
    enddo
  end function alphaRes_hd_NIST

  subroutine assign_meos_nist(this,other)
    class(meos_nist), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_nist)
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
      !
      this%n_g = other%n_g
      this%t_g = other%t_g
      this%d_g = other%d_g
      this%eta_g = other%eta_g
      this%beta_g = other%beta_g
      this%gamma_g = other%gamma_g
      this%epsilon_g = other%epsilon_g
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
      call stoperror("assign_meos_nist: Should not be here....")
    end select
  end subroutine assign_meos_nist

  !> Pressure and (optionally) its derivatives
  subroutine mp_pressure_NIST(this, rho, T, p, p_rho, p_T)
    use hyperdual_mod
    class(meos_nist) :: this
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
  end subroutine mp_pressure_NIST

end module nist
