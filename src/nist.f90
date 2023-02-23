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
    integer :: n_gao = 0
    !
    real, allocatable, dimension(:) :: n_crit
    real, allocatable, dimension(:) :: t_crit
    integer, allocatable, dimension(:) :: d_crit
    real, allocatable, dimension(:) :: eta_crit
    real, allocatable, dimension(:) :: beta_crit
    real, allocatable, dimension(:) :: gamma_crit
    real, allocatable, dimension(:) :: epsilon_crit
    !
    real, allocatable, dimension(:) :: n_cd
    real, allocatable, dimension(:) :: a_cd
    real, allocatable, dimension(:) :: b_cd
    real, allocatable, dimension(:) :: beta_cd
    real, allocatable, dimension(:) :: big_a_cd
    real, allocatable, dimension(:) :: big_b_cd
    real, allocatable, dimension(:) :: big_c_cd
    real, allocatable, dimension(:) :: big_d_cd
    !
  contains

    procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_NIST
    procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_NIST
    procedure, public :: init => init_NIST
    procedure, private :: alphaResPrefactors => alphaResPrefactors_NIST
    procedure, public :: allocate_param
    procedure, public :: alpha0_hd_taudelta => alpha0_hd_NIST
    procedure, public :: alphaRes_hd_taudelta => alphaRes_hd_NIST

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

    print *,"Entering nist init"
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
      nist_comp%n_sinh = 0
      nist_comp%upPol = nistdb(i_comp)%n_poly_eos
      nist_comp%upExp = nistdb(i_comp)%n_exp_eos + nistdb(i_comp)%n_poly_eos
      nist_comp%n_gauss = nistdb(i_comp)%n_gauss_eos
      nist_comp%n_gao = nistdb(i_comp)%n_gao_eos
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
      nist_comp%n_crit = nistdb(i_comp)%n_eos(nist_comp%upExp + 1:nist_comp%upExp + nist_comp%n_gauss)
      nist_comp%t_crit = nistdb(i_comp)%t_eos(nist_comp%upExp + 1:nist_comp%upExp + nist_comp%n_gauss)
      nist_comp%d_crit = nistdb(i_comp)%d_eos(nist_comp%upExp + 1:nist_comp%upExp + nist_comp%n_gauss)
      nist_comp%eta_crit = nistdb(i_comp)%eta_eos(1:nist_comp%n_gauss)
      nist_comp%beta_crit = nistdb(i_comp)%beta_eos(1:nist_comp%n_gauss)
      nist_comp%gamma_crit = nistdb(i_comp)%gamma_eos(1:nist_comp%n_gauss)
      nist_comp%epsilon_crit = nistdb(i_comp)%epsilon_eos(1:nist_comp%n_gauss)
      !
      nist_comp%n_cd = nistdb(i_comp)%n_cd(1:nist_comp%n_gao)
      nist_comp%a_cd = nistdb(i_comp)%a_cd(1:nist_comp%n_gao)
      nist_comp%b_cd = nistdb(i_comp)%b_cd(1:nist_comp%n_gao)
      nist_comp%beta_cd = nistdb(i_comp)%beta_cd(1:nist_comp%n_gao)
      nist_comp%big_a_cd = nistdb(i_comp)%big_a_cd(1:nist_comp%n_gao)
      nist_comp%big_b_cd = nistdb(i_comp)%big_b_cd(1:nist_comp%n_gao)
      nist_comp%big_c_cd = nistdb(i_comp)%big_c_cd(1:nist_comp%n_gao)
      nist_comp%big_d_cd = nistdb(i_comp)%big_d_cd(1:nist_comp%n_gao)
      !
      ! Calculate critcal pressure
      !nist_comp%rc
      call nist_comp%mp_pressure(nist_comp%rc, nist_comp%tc, nist_comp%pc)
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

  subroutine allocate_param(this)
    class(meos_nist), intent(inout) :: this
    !
    call this%meos_gerg%allocate_param()
    !
    if (allocated(this%n_crit)) deallocate(this%n_crit)
    if (allocated(this%t_crit)) deallocate(this%t_crit)
    if (allocated(this%d_crit)) deallocate(this%d_crit)
    if (allocated(this%eta_crit)) deallocate(this%eta_crit)
    if (allocated(this%beta_crit)) deallocate(this%beta_crit)
    if (allocated(this%gamma_crit)) deallocate(this%gamma_crit)
    if (allocated(this%epsilon_crit)) deallocate(this%epsilon_crit)
    !
    if (allocated(this%n_cd)) deallocate(this%n_cd)
    if (allocated(this%a_cd)) deallocate(this%a_cd)
    if (allocated(this%b_cd)) deallocate(this%b_cd)
    if (allocated(this%beta_cd)) deallocate(this%beta_cd)
    if (allocated(this%big_a_cd)) deallocate(this%big_a_cd)
    if (allocated(this%big_b_cd)) deallocate(this%big_b_cd)
    if (allocated(this%big_c_cd)) deallocate(this%big_c_cd)
    if (allocated(this%big_d_cd)) deallocate(this%big_d_cd)
    !
    if (allocated(this%c_id)) deallocate(this%c_id)
    if (allocated(this%c_id)) deallocate(this%t_id)
    !
    allocate(this%n_crit(1:this%n_gauss))
    allocate(this%t_crit(1:this%n_gauss))
    allocate(this%d_crit(1:this%n_gauss))
    allocate(this%eta_crit(1:this%n_gauss))
    allocate(this%beta_crit(1:this%n_gauss))
    allocate(this%gamma_crit(1:this%n_gauss))
    allocate(this%epsilon_crit(1:this%n_gauss))
    !
    allocate(this%n_cd(1:this%n_gao))
    allocate(this%a_cd(1:this%n_gao))
    allocate(this%b_cd(1:this%n_gao))
    allocate(this%beta_cd(1:this%n_gao))
    allocate(this%big_a_cd(1:this%n_gao))
    allocate(this%big_b_cd(1:this%n_gao))
    allocate(this%big_c_cd(1:this%n_gao))
    allocate(this%big_d_cd(1:this%n_gao))
    !
    allocate(this%c_id(this%n_id))
    allocate(this%t_id(this%n_id))
    !
  end subroutine allocate_param

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
    integer :: i, j
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
      j = i + this%upExp
      alpr = alpr + this%N_exp(j) * tau**this%t_exp(j) * delta**this%d_exp(j) * &
           exp(-this%eta_crit(i)*(delta-this%epsilon_crit(i))**2 - &
           this%beta_crit(i)*(delta-this%gamma_crit(i))**2)
    enddo

    do i=1,this%n_gao
      del = ((1.0_dp - tau) + this%big_a_cd(i)*((delta-1.0_dp)**2)**(1.0_dp/(2.0_dp*this%beta_cd(i))))**2 + &
           this%big_b_cd(i)*((delta-1.0_dp)**2)**this%a_cd(i)
      alpr = alpr + this%n_cd(i) * del**this%b_cd(i) * delta * &
           exp(-this%big_c_cd(i)*(delta-1.0_dp)**2 - &
           this%big_d_cd(i)*(tau-1.0_dp)**2)
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
      this%n_gao = other%n_gao
      !
      this%n_crit = other%n_crit
      this%t_crit = other%t_crit
      this%d_crit = other%d_crit
      this%eta_crit = other%eta_crit
      this%beta_crit = other%beta_crit
      this%gamma_crit = other%gamma_crit
      this%epsilon_crit = other%epsilon_crit
      !
      this%n_cd = other%n_cd
      this%a_cd = other%a_cd
      this%b_cd = other%b_cd
      this%beta_cd = other%beta_cd
      this%big_a_cd = other%big_a_cd
      this%big_b_cd = other%big_b_cd
      this%big_c_cd = other%big_c_cd
      this%big_d_cd = other%big_d_cd
      !
    class default
      call stoperror("assign_meos_nist: Should not be here....")
    end select
  end subroutine assign_meos_nist

end module nist
