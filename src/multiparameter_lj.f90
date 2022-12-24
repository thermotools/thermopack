module multiparameter_lj
  !> Equation of State for the Lennard-Jones Fluid
  !! See doi:10.1063/1.4945000
  !! and doi:10.1080/00268976.2016.1246760
  use multiparameter_base, only: meos
  use thermopack_constants, only: N_Avogadro, kB_const, &
       VAPPH, LIQPH
  use thermopack_var, only: Rgas
  implicit none
  save
  public :: meos_lj, lj_param, constructor_lj, init_LJTS
  private

  ! Parameters for the ideal gas part alpha^0
  real, parameter, dimension(1:2) :: a_lj = (/ -1.515151515, 6.262265814 /)

  ! upPol is the upper index for polynomial terms, upExp the same for
  ! single-expontential terms, upExpExp for double-exponential terms.
  integer, parameter :: upPol_lj = 6
  integer, parameter :: upExp_lj = 12
  integer, parameter :: upExpExp_lj = 23

  real, parameter, dimension(1:upPol_lj) :: N_pol_lj = (/0.52080730e-2, &
       0.21862520e+1, -0.21610160e+1, 0.14527000e+1, -0.20417920e+1, &
       0.18695286 /)
  real, parameter, dimension(upPol_lj+1:upExp_lj) :: N_exp_lj =(/ -0.90988445e-1, &
       -0.49745610, 0.10901431, -0.80055922, -0.56883900, -0.62086250 /)
  real, parameter, dimension(upExp_lj+1:upExpExp_lj) :: N_expexp_lj = (/ -0.14667177e1, &
       0.18914690e1, -0.13837010, -0.38696450, 0.12657020, 0.60578100, &
       0.11791890e1, -0.47732679, -0.99218575e1, -0.57479320, 0.37729230e-2/)

  real, parameter, dimension(1:upPol_lj) :: t_pol_lj = (/1.0, 0.32, 0.505, 0.672, &
       0.843, 0.898 /)
  real, parameter, dimension(upPol_lj+1:upExp_lj) :: t_exp_lj = (/ 1.294, 2.590, 1.786, &
       2.770, 1.786, 1.205/)
  real, parameter, dimension(upExp_lj+1:upExpExp_lj) :: t_expexp_lj = (/ 2.830, 2.548, &
       4.650, 1.385, 1.460, 1.351, 0.660, 1.496, 1.830, 1.616, 4.970/)

  integer, parameter, dimension(1:upPol_lj) :: d_pol_lj = (/ 4, 1, 1, 2, 2, 3/)
  integer, parameter, dimension(upPol_lj+1:upExp_lj) :: d_exp_lj = (/ 5, 2, 2, 3, 1, 1 /)
  integer, parameter, dimension(upExp_lj+1:upExpExp_lj) :: d_expexp_lj = (/ 1, 1, 2, 3, &
       3, 2, 1, 2, 3, 1, 1 /)

  integer, parameter, dimension(upPol_lj+1:upExp_lj) :: l_exp_lj = (/ 1, 2, 1, 2, 2, 1 /)
  real, parameter, dimension(upExp_lj+1:upExpExp_lj) :: eta_expexp_lj = (/ 2.067, 1.522, &
       8.820, 1.722, 0.679, 1.883, 3.925, 2.461, 28.20, 0.753, 0.820 /)
  real, parameter, dimension(upExp_lj+1:upExpExp_lj) :: beta_expexp_lj = (/ 0.625, &
       0.638, 3.910, 0.156, 0.157, 0.153, 1.160, 1.730, 383.0, 0.112, 0.119 /)
  real, parameter, dimension(upExp_lj+1:upExpExp_lj) :: gam_expexp_lj = (/ 0.710, 0.860, &
       1.940, 1.480, 1.490, 1.945, 3.020, 1.110, 1.170, 1.330, 0.240 /)
  real, parameter, dimension(upExp_lj+1:upExpExp_lj) :: eps_expexp_lj = (/ 0.2053, &
       0.4090, 0.6000, 1.2030, 1.8290, 1.3970, 1.3900, 0.5390, 0.9340, &
       2.3690, 2.4300/)

  ! Parameters for the ancillary equations to compute saturation densities
  integer, parameter :: satp_liq_lj = 5
  integer, parameter :: satp_vap_lj = 6

  real, parameter, dimension(5) :: N_liqsat_lj = (/ 0.1362e1, 0.2093e1, &
       -0.2110e1, 0.3290, 0.1410e1 /)
  real, parameter, dimension(5) :: expo_liqsat_lj = (/ 0.313, 0.940, 1.630, &
       17.00, 2.400/)
  real, parameter, dimension(6) :: N_vapsat_lj = (/ -0.69655e1, &
       -0.10331e3, -0.20325e1, -0.44481e2, -0.18463e2, -0.26070e-3/)
  real, parameter, dimension(6) :: expo_vapsat_lj = (/ 1.320, 19.24, &
       0.360, 8.780, 4.040, 41.60/)

  !**************************************************
  ! Parameters for the ideal gas part alpha^0
  real, parameter, dimension(1:2) :: a_ljts = (/ -2.5*0.8/1.086, 0.0 /)

  ! upPol is the upper index for polynomial terms, upExp the same for
  ! single-expontential terms, upExpExp for double-exponential terms.
  integer, parameter :: upPol_ljts = 6
  integer, parameter :: upExp_ljts = 12
  integer, parameter :: upExpExp_ljts = 21

  real, parameter, dimension(1:upPol_ljts) :: N_pol_ljts = (/ 0.15606084e-1, &
       0.17917527e1, -0.19613228e1, 0.13045604e1, -0.18117673e1, &
       0.15483997 /)
  real, parameter, dimension(upPol_ljts+1:upExp_ljts) :: N_exp_ljts =(/ -0.94885204e-1, &
        -0.20092412, 0.11639644, -0.50607364, -0.58422807, -0.47510982/)
  real, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: N_expexp_ljts = (/ 0.94333106e-2, &
       0.30444628, -0.10820946e-2, -0.99693391e-1, 0.91193522e-2, 0.12970543, &
       0.23036030e-1, -0.82671073e-1, -0.22497821e1/)

  real, parameter, dimension(1:upPol_ljts) :: t_pol_ljts = (/ 1.000, 0.304, 0.583, &
        0.662, 0.870, 0.870 /)
  real, parameter, dimension(upPol_ljts+1:upExp_ljts) :: t_exp_ljts = (/ 1.250, 3.000, &
       1.700, 2.400, 1.960, 1.286 /)
  real, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: t_expexp_ljts = (/ 3.600, 2.080, &
       5.240, 0.960, 1.360, 1.655, 0.900, 0.860, 3.950/)

  integer, parameter, dimension(1:upPol_ljts) :: d_pol_ljts = (/ 4, 1, 1, 2, 2, 3 /)
  integer, parameter, dimension(upPol_ljts+1:upExp_ljts) :: d_exp_ljts = (/ 5, 2, 2, 3, 1, 1 /)
  integer, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: d_expexp_ljts = (/ 1, 1, 2, &
        3, 3, 2, 1, 2, 3/)

  integer, parameter, dimension(upPol_ljts+1:upExp_ljts) :: l_exp_ljts = (/ 1, 2, 1, 2, 2, 1 /)
  real, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: eta_expexp_ljts = (/ 4.70, 1.92, &
        2.70, 1.49, 0.65, 1.73, 3.70, 1.90, 13.2/)
  real, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: beta_expexp_ljts = (/ 20.0, 0.77, &
        0.50, 0.80, 0.40, 0.43, 8.00, 3.30, 114.0 /)
  real, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: gam_expexp_ljts = (/ 1.0, 0.5, &
        0.8, 1.5, 0.7, 1.6, 1.3, 0.6, 1.3/)
  real, parameter, dimension(upExp_ljts+1:upExpExp_ljts) :: eps_expexp_ljts = (/ 0.55, 0.70, &
       2.00, 1.14, 1.20, 1.31, 1.14, 0.53, 0.96 /)

  ! Parameters for the ancillary equations to compute saturation densities
  integer, parameter :: satp_liq_ljts = 4
  integer, parameter :: satp_vap_ljts = 4

  real, parameter, dimension(4) :: N_liqsat_ljts = (/ 1.45, -0.172, -0.298, 0.295 /)
  real, parameter, dimension(4) :: expo_liqsat_ljts = (/ 0.334, 0.667, 1.25, 1.92 /)
  real, parameter, dimension(4) :: N_vapsat_ljts = (/ 1.59809, -0.09975, -0.4774, &
       -2.33736 /)
  real, parameter, dimension(4) :: expo_vapsat_ljts = (/ 1.0, 1.5, 5.94, 0.41452 /)
  !********************************************************************

  !> Noble gas parameters for LJ EOS
  type :: lj_param
    character(len=20) :: comp_name
    real :: eps_divk !(K)
    real :: sigma ! (m)
  end type lj_param

  integer, parameter :: nLJmodels = 4
  type(lj_param), dimension(nLJmodels), parameter :: LJarray = (/&
       lj_param(comp_name="NE", eps_divk=33.921, sigma=2.801e-10), &
       lj_param(comp_name="AR", eps_divk=116.79, sigma=3.3952e-10), &
       lj_param(comp_name="KR", eps_divk=162.58, sigma=3.6274e-10), &
       lj_param(comp_name="XE", eps_divk=226.14, sigma=3.949e-10) &
       /)

  integer, parameter :: nLJTSmodels = 4
  type(lj_param), dimension(nLJTSmodels), parameter :: LJTSarray = (/&
       lj_param(comp_name="NE", eps_divk=39.83, sigma=2.800e-10), &
       lj_param(comp_name="AR", eps_divk=137.90, sigma=3.3916e-10), &
       lj_param(comp_name="KR", eps_divk=191.52, sigma=3.6233e-10), &
       lj_param(comp_name="XE", eps_divk=274.86, sigma=3.946e-10) &
       /)

  !> LJ multiparameter equations of state (Thol, Vrabec and Span).
  type, extends(meos) :: meos_lj
    real :: sigma ! (m)
    real :: eps_divk !(K)
    !
    ! Parameters for the ideal gas part alpha^0
    real, allocatable, dimension(:) :: a
    !
    integer :: upPol = 0
    integer :: upExp = 0
    integer :: upExpExp = 0
    !
    real, allocatable, dimension(:) :: N_pol
    real, allocatable, dimension(:) :: N_exp
    real, allocatable, dimension(:) :: N_expexp
    !
    real, allocatable, dimension(:) :: t_pol
    real, allocatable, dimension(:) :: t_exp
    real, allocatable, dimension(:) :: t_expexp
    !
    integer, allocatable, dimension(:) :: d_pol
    integer, allocatable, dimension(:) :: d_exp
    integer, allocatable, dimension(:) :: d_expexp
    !
    integer, allocatable, dimension(:) :: l_exp
    real, allocatable, dimension(:) :: eta_expexp
    real, allocatable, dimension(:) :: beta_expexp
    real, allocatable, dimension(:) :: gam_expexp
    real, allocatable, dimension(:) :: eps_expexp
    ! Allocatables for the ancillary equations to compute saturation densities
    integer :: satp_liq = 0
    integer :: satp_vap = 0
    !
    real, allocatable, dimension(:) :: N_liqsat
    real, allocatable, dimension(:) :: expo_liqsat
    real, allocatable, dimension(:) :: N_vapsat
    real, allocatable, dimension(:) :: expo_vapsat

   contains
     procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_LJ
     procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_LJ
     procedure, public :: satDeltaEstimate => satDeltaEstimate_LJ
     procedure, public :: init => init_dummy
     procedure, private :: alphaResPrefactors => alphaResPrefactors_LJ
     procedure, private :: allocate_param
     ! Assignment operator
     procedure, pass(This), public :: assign_meos => assign_meos_lj
   end type meos_lj

contains

  function constructor_LJ(comp_name, shift_and_truncate) result(lj)
    use stringmod, only: str_eq
    use compdata_init, only: getCompDBindex, compdb
    character(len=*), intent(in) :: comp_name
    logical, intent(in) :: shift_and_truncate
    type(meos_lj) :: lj
    ! Locals
    integer :: i_comp, i, comp_id
    real :: tau_triple
    integer :: nModels
    type(lj_param), dimension(:), allocatable :: param_array
    if (shift_and_truncate) then
      call init_LJTS(lj)
      nModels = nLJTSmodels
      allocate(param_array(nModels))
      param_array = LJTSarray
    else
      call init_LJ(lj)
      nModels = nLJmodels
      allocate(param_array(nModels))
      param_array = LJarray
    endif
    i_comp = -1
    do i=1,nModels
      if (str_eq(comp_name, param_array(i)%comp_name)) then
        i_comp = i
        exit
      endif
    enddo
    if (i_comp > 0) then
      lj%eps_divk = param_array(i_comp)%eps_divk
      lj%sigma = param_array(i_comp)%sigma
    else
      print *,"No LJ parameters for component ",trim(comp_name)
    endif
    lj%tc = lj%tc*lj%eps_divk
    lj%rc = lj%rc/(N_Avogadro*lj%sigma**3)
    lj%t_triple = lj%t_triple*lj%eps_divk
    lj%maxT = lj%maxT*lj%eps_divk
    lj%maxP = lj%maxP*lj%eps_divk*kB_const/lj%sigma**3
    lj%Rgas_fit = Rgas
    lj%Rgas_meos = Rgas
    comp_id = getCompDBindex(comp_name)
    lj%molarMass = compdb(comp_id)%mw
    ! Calculate critcal pressure
    call lj%mp_pressure(lj%rc, lj%tc, lj%pc)
    ! Estimats for triple densities
    tau_triple = lj%tc/lj%t_triple
    lj%rhoLiq_triple = lj%rc*lj%satDeltaEstimate(tau_triple, LIQPH)
    lj%rhoVap_triple = lj%rc*lj%satDeltaEstimate(tau_triple, VAPPH)

    deallocate(param_array)
  end function constructor_LJ

  subroutine init_dummy(this, use_Rgas_fit)
    class(meos_lj) :: this
    logical, optional, intent(in) :: use_Rgas_fit
  end subroutine init_dummy

  subroutine init_LJ (this)
    class(meos_lj) :: this

    this%compName = "LJ"
    this%tc = 1.32  !< (-)
    this%rc = 0.31    !< (-)
    this%pc = 0.0 !< (-)
    this%acf = -4.2286E-002

    this%t_triple = 0.661  !< (-)
    this%p_triple = 0.0  !< ()
    this%rhoLiq_triple = 0.0  !< (-)
    this%rhoVap_triple = 0.0  !< (-)

    this%molarMass = 1.0  !< (kg/mol)
    this%Rgas_fit = 1.0 !< (J/(mol*K))

    this%maxT = 9.0 ! (-)
    this%maxP = 65.0 ! (-)

    this%Rgas_meos = 1.0

    ! Set LJ parameters
    this%upPol = upPol_lj
    this%upExp = upExp_lj
    this%upExpExp = upExpExp_lj
    !
    this%satp_liq = satp_liq_lj
    this%satp_vap = satp_vap_lj
    !
    call this%allocate_param()
    !
    this%a(1:2) = a_lj
    !
    this%N_pol(1:this%upPol) = N_pol_lj
    this%N_exp(this%upPol+1:this%upExp) = N_exp_lj
    this%N_expexp(this%upExp+1:this%upExpExp) = N_expexp_lj
    !
    this%t_pol(1:this%upPol) = t_pol_lj
    this%t_exp(this%upPol+1:this%upExp) = t_exp_lj
    this%t_expexp(this%upExp+1:this%upExpExp) = t_expexp_lj
    !
    this%d_pol(1:this%upPol) = d_pol_lj
    this%d_exp(this%upPol+1:this%upExp) = d_exp_lj
    this%d_expexp(this%upExp+1:this%upExpExp) = d_expexp_lj
    !
    this%l_exp(this%upPol+1:this%upExp) = l_exp_lj
    this%eta_expexp(this%upExp+1:this%upExpExp) = eta_expexp_lj
    this%beta_expexp(this%upExp+1:this%upExpExp) = beta_expexp_lj
    this%gam_expexp(this%upExp+1:this%upExpExp) = gam_expexp_lj
    this%eps_expexp(this%upExp+1:this%upExpExp) = eps_expexp_lj
    !
    this%N_liqsat(1:this%satp_liq) = N_liqsat_lj
    this%expo_liqsat(1:this%satp_liq) = expo_liqsat_lj
    this%N_vapsat(1:this%satp_vap) = N_vapsat_lj
    this%expo_vapsat(1:this%satp_vap) = expo_vapsat_lj

  end subroutine init_LJ

  subroutine init_LJTS(this)
    class(meos_lj) :: this
    ! Loclas
    real :: alp0(0:2,0:2), s0, tau, delta
    !
    this%compName = "LJTS"
    this%tc = 1.086  !< (-)
    this%rc = 0.319    !< (-)
    this%pc = 0.0 !< (-)
    this%acf = 2.44417E-002

    this%t_triple = 0.56  !< (-)
    this%p_triple = 0.0  !< ()
    this%rhoLiq_triple = 0.0  !< (-)
    this%rhoVap_triple = 0.0  !< (-)

    this%molarMass = 1.0  !< (kg/mol)
    this%Rgas_fit = 1.0 !< (J/(mol*K))

    this%maxT = 11.0 ! (-)
    this%maxP = 70.0 ! (-)

    this%Rgas_meos = 1.0

    ! Set LJTS parameters
    this%upPol = upPol_ljts
    this%upExp = upExp_ljts
    this%upExpExp = upExpExp_ljts
    !
    this%satp_liq = satp_liq_ljts
    this%satp_vap = satp_vap_ljts
    !
    call this%allocate_param()
    !
    this%a(1:2) = a_ljts
    ! Set a(2) to get s0
    delta = 0.001d0/(0.8*this%rc)
    tau = this%tc/0.8
    call this%alpha0Derivs_taudelta(delta, tau, alp0)
    s0 = -(alp0(0,0) - alp0(0,1))
    this%a(2) = s0
    !
    this%N_pol(1:this%upPol) = N_pol_ljts
    this%N_exp(this%upPol+1:this%upExp) = N_exp_ljts
    this%N_expexp(this%upExp+1:this%upExpExp) = N_expexp_ljts
    !
    this%t_pol(1:this%upPol) = t_pol_ljts
    this%t_exp(this%upPol+1:this%upExp) = t_exp_ljts
    this%t_expexp(this%upExp+1:this%upExpExp) = t_expexp_ljts
    !
    this%d_pol(1:this%upPol) = d_pol_ljts
    this%d_exp(this%upPol+1:this%upExp) = d_exp_ljts
    this%d_expexp(this%upExp+1:this%upExpExp) = d_expexp_ljts
    !
    this%l_exp(this%upPol+1:this%upExp) = l_exp_ljts
    this%eta_expexp(this%upExp+1:this%upExpExp) = eta_expexp_ljts
    this%beta_expexp(this%upExp+1:this%upExpExp) = beta_expexp_ljts
    this%gam_expexp(this%upExp+1:this%upExpExp) = gam_expexp_ljts
    this%eps_expexp(this%upExp+1:this%upExpExp) = eps_expexp_ljts
    !
    this%N_liqsat(1:this%satp_liq) = N_liqsat_ljts
    this%expo_liqsat(1:this%satp_liq) = expo_liqsat_ljts
    this%N_vapsat(1:this%satp_vap) = N_vapsat_ljts
    this%expo_vapsat(1:this%satp_vap) = expo_vapsat_ljts

  end subroutine init_LJTS

  subroutine allocate_param(this)
    class(meos_lj), intent(inout) :: this
    !
    if (allocated(this%a)) deallocate(this%a)
    !
    if (allocated(this%N_pol)) deallocate(this%N_pol)
    if (allocated(this%N_exp)) deallocate(this%N_exp)
    if (allocated(this%N_expexp)) deallocate(this%N_expexp)
    !
    if (allocated(this%t_pol)) deallocate(this%t_pol)
    if (allocated(this%t_exp)) deallocate(this%t_exp)
    if (allocated(this%t_expexp)) deallocate(this%t_expexp)
    !
    if (allocated(this%d_pol)) deallocate(this%d_pol)
    if (allocated(this%d_exp)) deallocate(this%d_exp)
    if (allocated(this%d_expexp)) deallocate(this%d_expexp)
    !
    if (allocated(this%l_exp)) deallocate(this%l_exp)
    if (allocated(this%eta_expexp)) deallocate(this%eta_expexp)
    if (allocated(this%beta_expexp)) deallocate(this%beta_expexp)
    if (allocated(this%gam_expexp)) deallocate(this%gam_expexp)
    if (allocated(this%eps_expexp)) deallocate(this%eps_expexp)
    ! Allocatables for the ancillary equations to compute saturation densities
    if (allocated(this%N_liqsat)) deallocate(this%N_liqsat)
    if (allocated(this%expo_liqsat)) deallocate(this%expo_liqsat)
    if (allocated(this%N_vapsat)) deallocate(this%N_vapsat)
    if (allocated(this%expo_vapsat)) deallocate(this%expo_vapsat)
    !
    allocate(this%a(1:2))
    !
    allocate(this%N_pol(1:this%upPol))
    allocate(this%N_exp(this%upPol+1:this%upExp))
    allocate(this%N_expexp(this%upExp+1:this%upExpExp))
    !
    allocate(this%t_pol(1:this%upPol))
    allocate(this%t_exp(this%upPol+1:this%upExp))
    allocate(this%t_expexp(this%upExp+1:this%upExpExp))
    !
    allocate(this%d_pol(1:this%upPol))
    allocate(this%d_exp(this%upPol+1:this%upExp))
    allocate(this%d_expexp(this%upExp+1:this%upExpExp))
    !
    allocate(this%l_exp(this%upPol+1:this%upExp))
    allocate(this%eta_expexp(this%upExp+1:this%upExpExp))
    allocate(this%beta_expexp(this%upExp+1:this%upExpExp))
    allocate(this%gam_expexp(this%upExp+1:this%upExpExp))
    allocate(this%eps_expexp(this%upExp+1:this%upExpExp))
    !
    allocate(this%N_liqsat(1:this%satp_liq))
    allocate(this%expo_liqsat(1:this%satp_liq))
    allocate(this%N_vapsat(1:this%satp_vap))
    allocate(this%expo_vapsat(1:this%satp_vap))

  end subroutine allocate_param

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  subroutine alpha0Derivs_LJ(this, delta, tau, alp0)
    class(meos_lj) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    !
    alp0 = 0.0
    alp0(0,0) = log(delta) + 1.5*log(tau) + this%a(1)*tau + this%a(2)
    alp0(1,0) = 1.0
    alp0(2,0) = -1.0
    alp0(0,1) = 1.5 + this%a(1)*tau
    alp0(0,2) = -1.5
  end subroutine alpha0Derivs_LJ

  ! Supplies all prefactors that do not depend on delta. Prefactors are cached.
  subroutine alphaResPrefactors_LJ (this, tau, prefactors_pol, prefactors_exp, prefactors_expexp)
    class(meos_lj) :: this
    real, intent(in) :: tau
    real, intent(out) :: prefactors_pol(1:this%upPol)
    real, intent(out) :: prefactors_exp(this%upPol+1:this%upExp)
    real, intent(out) :: prefactors_expexp(this%upExp+1:this%upExpExp)
    !
    prefactors_pol = this%N_pol * tau**this%t_pol
    prefactors_exp = this%N_exp * tau**this%t_exp
    prefactors_expexp = this%N_expexp * tau**this%t_expexp

  end subroutine alphaResPrefactors_LJ


  subroutine alphaResDerivs_LJ (this, delta, tau, alpr)
    class(meos_lj) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    ! Internal
    real :: deltaL(this%upPol+1:this%upExp)
    real :: prefactors_pol(1:this%upPol)
    real :: prefactors_exp(this%upPol+1:this%upExp)
    real :: prefactors_expexp(this%upExp+1:this%upExpExp)
    real :: polTerms(1:this%upPol), expTerms(this%upPol+1:this%upExp)
    real :: expExpTerms(this%upExp+1:this%upExpExp)
    call this%alphaResPrefactors(tau, prefactors_pol, prefactors_exp, prefactors_expexp)

    ! Precalculate polynomial terms
    polTerms(1:this%upPol) = prefactors_pol*delta**this%d_pol

    ! Precalculate single-exponential terms
    deltaL(this%upPol+1:this%upExp) = delta**this%l_exp
    expTerms(this%upPol+1:this%upExp) = prefactors_exp * delta**this%d_exp * exp(-deltaL)

    ! Precalculate double-exponential terms
    expExpTerms(this%upExp+1:this%upExpExp) = prefactors_expexp * delta**this%d_expexp &
         * exp(-this%eta_expexp*(delta-this%eps_expexp)**2 - &
         this%beta_expexp*(tau-this%gam_expexp)**2)

    ! alpha
    alpr(0,0) = sum(polTerms) + sum(expTerms) + sum(expExpTerms)

    ! delta*alpha_delta
    alpr(1,0) = dot_product(polTerms, this%d_pol) + dot_product(expTerms, this%d_exp - this%l_exp*deltaL) &
         + dot_product(expExpTerms, this%d_expexp - 2*this%eta_expexp*delta*(delta-this%eps_expexp))

    ! delta**2 * alpha_deltadelta
    alpr(2,0) = dot_product(polTerms, this%d_pol*(this%d_pol-1)) + &
         dot_product(expTerms, (this%d_exp - &
         this%l_exp*deltaL)*(this%d_exp-1-this%l_exp*deltaL) - &
         this%l_exp*this%l_exp*deltaL ) + &
         dot_product(expExpTerms, (this%d_expexp - &
         2*this%eta_expexp*delta*(delta-this%eps_expexp))**2 - &
         this%d_expexp - 2*this%eta_expexp*delta**2 )

    ! tau*alpha_tau
    alpr(0,1) = dot_product(polTerms, this%t_pol) + dot_product(expTerms, this%t_exp) &
         + dot_product(expExpTerms, this%t_expexp - 2*this%beta_expexp*tau*(tau-this%gam_expexp))

    ! tau**2 * alpha_tautau
    alpr(0,2) = dot_product(polTerms, this%t_pol*(this%t_pol-1)) + &
         dot_product(expTerms, this%t_exp*(this%t_exp-1)) &
         + dot_product(expExpTerms, (this%t_expexp - &
         2*this%beta_expexp*tau*(tau-this%gam_expexp))**2 - &
         this%t_expexp - 2*this%beta_expexp*tau**2 )

    ! delta*tau*alpha_deltatau
    alpr(1,1) = dot_product(polTerms, this%d_pol*this%t_pol) + &
         dot_product(expTerms, (this%d_exp - this%l_exp*deltaL)*this%t_exp ) &
         + dot_product(expExpTerms, &
         (this%d_expexp - 2*this%eta_expexp*delta*(delta-this%eps_expexp))*&
         (this%t_expexp - 2*this%beta_expexp*tau*(tau-this%gam_expexp)))

  end subroutine alphaResDerivs_LJ

  function satDeltaEstimate_LJ(this,tau,phase) result(deltaSat)
    class(meos_lj) :: this
    real, intent(in) :: tau
    integer, intent(in) :: phase
    real :: deltaSat
    ! Internals
    real :: theta

    theta = 1-1/tau
    if (theta<0.0) then
       deltaSat = 1.0
    else if ( phase == LIQPH ) then
      deltaSat = 1 + dot_product(this%N_liqsat,theta**this%expo_liqsat)
    else if ( phase == VAPPH ) then
      deltaSat = exp(dot_product(this%N_vapsat,theta**this%expo_vapsat))
    else
      call stoperror("satDeltaEstimate_LJ: only LIQPH and VAPPH allowed!")
    end if

  end function satDeltaEstimate_LJ

  subroutine assign_meos_lj(this,other)
    class(meos_lj), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_lj)
      call this%assign_meos_base(other)
      !
      this%sigma = other%sigma
      this%eps_divk = other%eps_divk
      !
      this%a = other%a
      !
      this%upPol = other%upPol
      this%upExp = other%upExp
      this%upExpExp = other%upExpExp
      !
      this%N_pol = other%N_pol
      this%N_exp = other%N_exp
      this%N_expexp = other%N_expexp
      !
      this%t_pol = other%t_pol
      this%t_exp = other%t_exp
      this%t_expexp = other%t_expexp
      !
      this%d_pol = other%d_pol
      this%d_exp = other%d_exp
      this%d_expexp = other%d_expexp
      !
      this%l_exp = other%l_exp
      this%eta_expexp = other%eta_expexp
      this%beta_expexp = other%beta_expexp
      this%gam_expexp = other%gam_expexp
      this%eps_expexp = other%eps_expexp
      this%satp_liq = other%satp_liq
      this%satp_vap = other%satp_vap
      !
      this%N_liqsat = other%N_liqsat
      this%expo_liqsat = other%expo_liqsat
      this%N_vapsat = other%N_vapsat
      this%expo_vapsat = other%expo_vapsat
    class default
      call stoperror("assign_meos_lj: Should not be here....")
    end select
  end subroutine assign_meos_lj

end module multiparameter_lj
