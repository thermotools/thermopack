!---------------------------------------------------------------------
! Module and subroutines for the Lennard-Jones splined
! Equation of State implmented in Thermopack.
! Programmed by: M. Hammer and Ø. Wilhelmsen
! Spring 2019
! © SINTEF Energy Research. All rights reserved.
!---------------------------------------------------------------------
module lj_splined
  use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_var_container, &
       saftvrmie_param, saftvrmie_zeta, saftvrmie_dhs, svrm_opt
  use saftvrmie_options, only: saftvrmie_opt
  use saftvrmie_utils, only: calc_a_zeta_product, convert_zeta_x_to_TVn, &
       calc_a0_a_product
  use numconstants, only: pi
  use eosdata, only: eosLJS_BH, eosLJS_WCA, eosLJS_UF, eosLJS_UV, eosLJ_UF
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
  use thermopack_var, only: base_eos_param, get_active_eos, base_eos_dealloc, &
       Rgas, kRgas, get_active_thermo_model, thermo_model
  use hardsphere_wca, only: calc_dhs_WCA, calc_cavity_integral_LJ_Fres, &
       calcZetaX_vdW_no_segments
  implicit none
  private
  save

  ! Van der Waals alpha for ljs potential
  real, parameter :: alpha_ljs = 0.5357287350120993

  type, extends(saftvrmie_eos) :: ljs_bh_eos
    ! Model control
    logical :: use_Lafitte_a3 = .false.
    logical :: enable_chi_correction = .true.
    logical :: enable_hs = .true.
    logical :: enable_a1 = .true.
    logical :: enable_a2 = .true.
    logical :: enable_a3 = .true.
  contains
    procedure, public :: set_sigma_eps => ljs_bh_set_sigma_eps
    ! Assignment operator
    procedure, pass(This), public :: assign_eos => assign_ljs_bh_eos
  end type ljs_bh_eos

  type, extends(base_eos_param) :: ljs_wca_eos
    real :: sigma
    real :: eps_divk
    type(saftvrmie_dhs) :: dhs !< The hard-sphere diameter
    type(saftvrmie_zeta) :: eta_hs !< Packing fraction
    ! Model control
    logical :: enable_cavity = .true.
    logical :: enable_hs = .true.
    logical :: enable_a1 = .true.
    logical :: enable_a2 = .true.
    logical :: enable_a3 = .true.
    logical :: enable_a4 = .true.
    ! Need hard-sphere options from SAFT-VR Mie
    logical :: ovner_of_svrm_opt = .false.
    type(saftvrmie_opt), pointer :: svrm_opt => NULL()
  contains
    procedure, public :: set_sigma_eps => ljs_wca_set_sigma_eps
    !
    procedure, public :: dealloc => wca_dealloc
    procedure, public :: allocate_and_init => wca_allocate_and_init
    ! Assignment operator
    procedure, pass(This), public :: assign_eos => assign_ljs_wca_eos
  end type ljs_wca_eos

  type, extends(ljs_wca_eos) :: ljx_ux_eos
    ! Model control
    logical :: lj_potential
    logical :: is_uf_theory
    logical :: enable_virial_term = .true.
    logical :: use_temperature_dependent_u_fraction = .false.
  contains
    ! Assignment operator
    procedure, pass(This), public :: assign_eos => assign_ljx_ux_eos
  end type ljx_ux_eos

  !> PURE COMPONENT PARAMETERS.
  ! ---------------------------------------------------------------------------
  type :: ljs_data
    integer :: eosidx
    character(len=uid_len) :: compName
    ! Pure component parameters.
    real :: sigma    !< [m]. Temperature-independent segment diameter.
    real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.
    ! Parameter set
    character(len=ref_len) :: ref
  end type ljs_data

  integer, parameter :: nLJS = 5
  type(ljs_data), dimension(nLJS), parameter :: LJSarray = (/ &
       ljs_data(eosidx = eosLJS_BH, &
       compName="AR", &
       sigma=3.42E-10, &
       eps_depth_divk=124.0, &
       ref="DEFAULT"), &
       ljs_data(eosidx = eosLJS_WCA, &
       compName="AR", &
       sigma=3.42E-10, &
       eps_depth_divk=124.0, &
       ref="DEFAULT"), &
       ljs_data(eosidx = eosLJS_UV, &
       compName="AR", &
       sigma=3.42E-10, &
       eps_depth_divk=124.0, &
       ref="DEFAULT"), &
       ljs_data(eosidx = eosLJS_UF, &
       compName="AR", &
       sigma=3.42E-10, &
       eps_depth_divk=124.0, &
       ref="DEFAULT"), &
       ljs_data(eosidx = eosLJ_UF, &
       compName="AR", &
       sigma=3.42E-10, &
       eps_depth_divk=124.0, &
       ref="DEFAULT")/)

  public :: ljs_bh_eos
  public :: init_LJs_bh, calc_LJs_bh_zeta, calcFresLJs_bh
  public :: calc_LJs_a1, calc_LJs_a2, calc_a3_LJs_ex
  public :: calc_ai_LJs_ex, calc_a1_LJs_ex, calc_a2_LJs_ex
  public :: calc_ais_LJs_ex, calc_chi_LJs_ex
  public :: ljs_bh_model_control, ljs_bh_set_pure_params, ljs_bh_get_pure_params
  public :: calc_ai_reduced_LJs_ex, ljs_bh_get_bh_diameter_div_sigma

  public :: calcFres_WCA, ljs_wca_eos, init_ljs_wca
  public :: calc_ljx_wca_zeta, ljx_ux_eos_constructor
  public :: calc_ljs_wca_ai_tr, calcFresLJs_WCA
  public :: ljs_uv_model_control, ljs_wca_model_control
  public :: ljs_wca_set_pure_params, ljs_wca_get_pure_params
  public :: calc_ljs_dispersion, calc_wca_soft_repulsion
  public :: calc_ljs_hard_sphere
  public :: ljs_potential_reduced
  public :: alpha_ljs

  ! Testing
  public :: calc_uf_wca, calc_uf_wca_tvn, ljx_ux_eos
  public :: calcFresLJ_uf_theory, calcFresLJs_uv_theory
  public :: uv_q_of_t, uv_hs_b2, ljs_b2, uv_a1_u_mult_t
  public :: uv_a1_b2, uv_phi, calc_ljs_wca_ai
  public :: get_bh_ljs_eos_pointer
  public :: uv_delta_b2_overall, calc_uv_wca, uv_a1_u
  public :: calc_uv_wca_tvn

contains

  !> Calculate the Wilhelmsen polynomials for the LJ/s
  !! dispersion terms.
  !!
  !! i = 4 gives chi (Correction in a2)
  !! \author Morten Hammer, Spring 2019
  subroutine calc_ai_LJs_ex(i,tau,rho,prefac,a,a_r,a_x,a_rr,a_xx,a_rx)
    ! Input
    integer, intent(in) :: i !< Calculate a_i
    real, intent(in) :: tau !< Temperature dependence (1- d/sigma)
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    real, intent(in) :: prefac !< Constants
    ! Output
    real, intent(out) :: a,a_r,a_x,a_rr,a_xx,a_rx
    ! Locals
    real, parameter :: p(4,15) = reshape(&
         (/0.039813685,  -0.106920241,  1.291256810,  0.000000000, &
         0.469671161,  -0.295578203,  -2.763380451,  21.299989640, &
         -0.346120315,  0.343721691,  1.416116081,  -11.284586250, &
         -0.344778684,  0.248954221,  0.331595545,  -0.258500011, &
         -0.535278660,  0.358663104,  -0.248745451,  0.996821621, &
         0.000000000,  0.000000000,  0.007804235,  0.000000000, &
         -7.861555167,  7.084518499,  2.799119819,  -540.982471100, &
         10.084157010,  -9.814532406,  -4.455465501,  599.182974200, &
         0.518650855,  0.172122707,  1.855450051,  -116.677210100, &
         0.000000000,  0.000000000,  -0.350181778,  3.804716369, &
         0.000000000,  0.000000000,  0.000000000,  0.000000000, &
         32.718743790,  -28.998566930,  0.000000000,  3131.407636000, &
         -51.573804390,  47.108198520,  0.000000000,  -4126.665342000, &
         6.550964581,  -7.263318791,  0.000000000,  1013.877746000, &
         0.000000000,  0.000000000,  0.000000000,  -70.079287230 /), (/4,15/))
    real :: f(3,0:2), g(3,0:2), rhov(5,0:2)
    integer :: j
    rhov(5,0) = rho
    do j=2,5
      rhov(5-j+1,0) = rhov(5-j+2,0)*rho
    enddo
    rhov(5,1) = 1.0
    rhov(4,1) = 2*rho
    rhov(3,1) = 3*rhov(4,0)
    rhov(2,1) = 4*rhov(3,0)
    rhov(1,1) = 5*rhov(2,0)

    rhov(5,2) = 0.0
    rhov(4,2) = 2.0
    rhov(3,2) = 6*rho
    rhov(2,2) = 12*rhov(4,0)
    rhov(1,2) = 20*rhov(3,0)

    do j=0,2
      f(1,j) = sum(p(i,1:5)*rhov(:,j))
      f(2,j) = sum(p(i,6:10)*rhov(:,j))
      f(3,j) = sum(p(i,11:15)*rhov(:,j))
    enddo

    g(1,0) = 1
    g(1,1) = 0
    g(1,2) = 0
    g(2,0) = tau
    g(2,1) = 1
    g(2,2) = 0
    g(3,0) = tau**2
    g(3,1) = 2*tau
    g(3,2) = 2

    a = 0.0
    a_x = 0.0
    a_r = 0.0
    a_xx = 0.0
    a_rr = 0.0
    a_rx = 0.0
    do j=1,3
      a = a + f(j,0)*g(j,0)
      a_x = a_x + f(j,0)*g(j,1)
      a_r = a_r + f(j,1)*g(j,0)
      a_xx = a_xx + f(j,0)*g(j,2)
      a_rr = a_rr + f(j,2)*g(j,0)
      a_rx = a_rx + f(j,1)*g(j,1)
    enddo
    a = prefac*a
    a_x = prefac*a_x
    a_r = prefac*a_r
    a_xx = prefac*a_xx
    a_rr = prefac*a_rr
    a_rx = prefac*a_rx
  end subroutine calc_ai_LJs_ex

  !> Calculate LJ/s a1
  !! Based on derivation by Ø. Wilhelmsen
  !! \author Morten Hammer, March 2019
  subroutine calc_LJs_a1(nc,T,V,n,s_vc,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn
    ! Locals
    integer :: difflevel
    real :: rho !< Reduced density
    real :: a_r,a_x,a_rr,a_xx,a_rx
    real :: tau, tau_T, tau_TT !<
    real :: eps !< Well depth div. temperature (K)
    real :: s !< Sigma
    real :: fac
    if ( present(a1_TT) .or. present(a1_VV) .or. present(a1_TV) .or. &
         present(a1_Tn) .or. present(a1_Vn) .or. present(a1_nn)) then
      difflevel = 2
    else if (present(a1_T) .or. present(a1_V) .or. present(a1_n)) then
      difflevel = 1
    else
      difflevel = 0
    endif
    s = saftvrmie_param%sigma_ij(1,1)
    tau = 1 - s_vc%dhs%d(1,1)/s
    tau_T = -s_vc%dhs%d_T(1,1)/s
    tau_TT = -s_vc%dhs%d_TT(1,1)/s
    rho = s_vc%rho_star%zx
    eps = saftvrmie_param%eps_divk_ij(1,1)
    fac = 2*pi*eps
    call calc_ai_LJs_ex(1,tau,rho,fac,a1,a_r,a_x,a_rr,a_xx,a_rx)
    call convert_zeta_x_to_TVn(nc,tau,tau_T,tau_TT,s_vc%rho_star,&
         a1,a_r,a_x,a_rr,a_xx,a_rx,0.0,0.0,0.0,&
         a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         difflevel=difflevel)

  end subroutine calc_LJs_a1

  !> Calculate LJ/s a2
  !! Based on derivation by Ø. Wilhelmsen
  !! \author Morten Hammer, March 2019
  subroutine calc_LJs_a2(eos,nc,T,V,n,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    class(ljs_bh_eos), intent(in) :: eos
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn
    ! Locals
    integer :: difflevel
    real :: rho !< Reduced density
    real :: a_r,a_x,a_rr,a_xx,a_rx
    real :: tau, tau_T, tau_TT !<
    real :: eps !< Well depth div. temperature (K)
    real :: s !< Sigma
    real :: fac
    real :: c2,c2_T,c2_V,c2_TT,c2_VV,c2_TV,c2_r,c2_x,c2_rr,c2_xx,c2_rx
    real, dimension(nc) :: c2_n,c2_Tn,c2_Vn
    real, dimension(nc,nc) :: c2_nn
    if ( present(a2_TT) .or. present(a2_VV) .or. present(a2_TV) .or. &
         present(a2_Tn) .or. present(a2_Vn) .or. present(a2_nn)) then
      difflevel = 2
    else if (present(a2_T) .or. present(a2_V) .or. present(a2_n)) then
      difflevel = 1
    else
      difflevel = 0
    endif
    s = saftvrmie_param%sigma_ij(1,1)
    tau = 1 - eos%saftvrmie_var%dhs%d(1,1)/s
    tau_T = -eos%saftvrmie_var%dhs%d_T(1,1)/s
    tau_TT = -eos%saftvrmie_var%dhs%d_TT(1,1)/s
    rho =  eos%saftvrmie_var%rho_star%zx
    eps = saftvrmie_param%eps_divk_ij(1,1)
    fac = -pi*eps**2
    call calc_ai_LJs_ex(2,tau,rho,fac,a2,a_r,a_x,a_rr,a_xx,a_rx)
    call convert_zeta_x_to_TVn(nc,tau,tau_T,tau_TT,eos%saftvrmie_var%rho_star,&
         a2,a_r,a_x,a_rr,a_xx,a_rx,0.0,0.0,0.0,&
         a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         difflevel=difflevel)

    ! Multiply with (1 + chi)?
    if (eos%enable_chi_correction) then
      fac = 1.0
      call calc_ai_LJs_ex(4,tau,rho,fac,c2,c2_r,c2_x,c2_rr,c2_xx,c2_rx)
      c2 = 1 + c2
      call convert_zeta_x_to_TVn(nc,tau,tau_T,tau_TT,eos%saftvrmie_var%rho_star,&
           c2,c2_r,c2_x,c2_rr,c2_xx,c2_rx,0.0,0.0,0.0,&
           c2_T,c2_V,c2_n,c2_TT,c2_VV,c2_TV,c2_Tn,c2_Vn,c2_nn,&
           difflevel=difflevel)
      call calc_a0_a_product(nc,c2,a2,&
           c2_T,c2_V,c2_n,&
           a2_T,a2_V,a2_n,&
           c2_TT,c2_VV,c2_TV,c2_Tn,c2_Vn,c2_nn,&
           a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)
    endif
    ! Multiply a2 with Khs
    call calc_a_zeta_product(nc, eos%saftvrmie_var%Khs,a2,a2_T,a2_V,a2_n,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)

  end subroutine calc_LJs_a2

  !> Calculate LJ/s a3
  !! Based on derivation by Ø. Wilhelmsen
  !! \author Morten Hammer, March 2019
  subroutine calc_LJs_a3_Lafitte(eos,nc,T,V,n,&
       a3,a3_T,a3_V,a3_n,a3_TT,a3_VV,a3_TV,a3_Tn,a3_Vn,a3_nn)
    use saftvrmie_dispersion, only: calcA3zeta
    use saftvrmie_containers, only: calcFunAlpha
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    class(ljs_bh_eos), intent(in) :: eos
    ! Output
    real, intent(out) :: a3
    real, optional, intent(out) ::  a3_T,a3_V,a3_TT,a3_VV,a3_TV
    real, optional, dimension(nc), intent(out) :: a3_n,a3_Tn,a3_Vn
    real, optional, dimension(nc,nc), intent(out) :: a3_nn
    ! Locals
    real :: z !< Packing fraction
    real :: a3_z,a3_a,a3_zz,a3_aa,a3_az
    real :: eps !< Well depth div. temperature (K)
    real :: f_alpha(6)
    integer :: difflevel
    if ( present(a3_TT) .or. present(a3_VV) .or. present(a3_TV) .or. &
         present(a3_Tn) .or. present(a3_Vn) .or. present(a3_nn)) then
      difflevel = 2
    else if (present(a3_T) .or. present(a3_V) .or. present(a3_n)) then
      difflevel = 1
    else
      difflevel = 0
    endif
    eps = saftvrmie_param%eps_divk_ij(1,1)
    z =  eos%saftvrmie_var%zeta_bar%zx
    call calcFunAlpha(alpha_ljs, f_alpha)
    call calcA3zeta(eps,z,alpha_ljs,f_alpha,a3,a3_z,a3_zz,&
         a3_a,a3_aa,a3_az)
    call convert_zeta_x_to_TVn(nc,alpha_ljs,0.0,0.0, eos%saftvrmie_var%zeta_bar,&
         a3,a3_z,a3_a,a3_zz,a3_aa,a3_az,0.0,0.0,0.0,&
         a1_T=a3_T,a1_V=a3_V,a1_n=a3_n,a1_TT=a3_TT,a1_VV=a3_VV,&
         a1_TV=a3_TV,a1_Tn=a3_Tn,a1_Vn=a3_Vn,a1_nn=a3_nn,&
         difflevel=difflevel)
  end subroutine calc_LJs_a3_Lafitte

  !> Calculate LJ/s a3
  !! Based on derivation by Ø. Wilhelmsen
  !! \author Morten Hammer, May 2019
  subroutine calc_LJs_a3(eos,nc,T,V,n,&
       a3,a3_T,a3_V,a3_n,a3_TT,a3_VV,a3_TV,a3_Tn,a3_Vn,a3_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    class(ljs_bh_eos), intent(inout) :: eos
    ! Output
    real, intent(out) :: a3
    real, optional, intent(out) ::  a3_T,a3_V,a3_TT,a3_VV,a3_TV
    real, optional, dimension(nc), intent(out) :: a3_n,a3_Tn,a3_Vn
    real, optional, dimension(nc,nc), intent(out) :: a3_nn
    ! Locals
    real :: a3_r,a3_x,a3_rr,a3_xx,a3_rx,fac,rho
    real :: eps !< Well depth div. kB (K)
    real :: tau, tau_T, tau_TT !<
    real :: s !< Sigma
    integer :: difflevel
    if (eos%use_Lafitte_a3) then
      call calc_LJs_a3_Lafitte(eos,nc,T,V,n,&
       a3,a3_T,a3_V,a3_n,a3_TT,a3_VV,a3_TV,a3_Tn,a3_Vn,a3_nn)
    else
      if ( present(a3_TT) .or. present(a3_VV) .or. present(a3_TV) .or. &
           present(a3_Tn) .or. present(a3_Vn) .or. present(a3_nn)) then
        difflevel = 2
      else if (present(a3_T) .or. present(a3_V) .or. present(a3_n)) then
        difflevel = 1
      else
        difflevel = 0
      endif
      s = saftvrmie_param%sigma_ij(1,1)
      tau = 1 - eos%saftvrmie_var%dhs%d(1,1)/s
      tau_T = -eos%saftvrmie_var%dhs%d_T(1,1)/s
      tau_TT = -eos%saftvrmie_var%dhs%d_TT(1,1)/s
      rho = eos%saftvrmie_var%rho_star%zx
      eps = saftvrmie_param%eps_divk_ij(1,1)
      fac = eps**3
      call calc_ai_LJs_ex(3,tau,rho,fac,a3,a3_r,a3_x,a3_rr,a3_xx,a3_rx)
      call convert_zeta_x_to_TVn(nc,tau,tau_T,tau_TT,eos%saftvrmie_var%rho_star,&
           a3,a3_r,a3_x,a3_rr,a3_xx,a3_rx,0.0,0.0,0.0,&
           a3_T,a3_V,a3_n,a3_TT,a3_VV,a3_TV,a3_Tn,a3_Vn,a3_nn,&
           difflevel=difflevel)
    endif
  end subroutine calc_LJs_a3

  !> Calculate a1 for LJ/s
  !! \author Morten Hammer, March 2019
  subroutine calc_a1_LJs_ex(x0,rho,eps,a1)
    ! Input
    real, intent(in) :: eps !< Epsilon divided by kB
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density
    ! Output
    real, intent(out) :: a1
    ! Locals
    real :: a1_r,a1_x,a1_rr,a1_xx,a1_rx
    real :: fac, tau
    fac = 2*pi*eps
    tau = 1 - 1/x0
    call calc_ai_LJs_ex(1,tau,rho,fac,a1,a1_r,a1_x,a1_rr,a1_xx,a1_rx)
  end subroutine calc_a1_LJs_ex

  !> Calculate a2 for LJ/s
  !! \author Morten Hammer, March 2019
  subroutine calc_a2_LJs_ex(x0,rho,eps,a2)
    use saftvrmie_dispersion, only: calcKhs
    ! Input
    real, intent(in) :: eps !< Epsilon divided by kB
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density
    ! Output
    real, intent(out) :: a2
    ! Locals
    real :: eta
    real :: a_r,a_x,a_rr,a_xx,a_rx
    real :: fac, tau
    real :: chi, chi_r, chi_rr, chi_rx, chi_x, chi_xx
    real :: Khs, Khs_e, Khs_ee, Khs_eee
    fac = -pi*eps**2
    tau = 1 - 1/x0
    call calc_ai_LJs_ex(2,tau,rho,fac,a2,a_r,a_x,a_rr,a_xx,a_rx)
    ! Multiply with (1 + chi)
    fac = 1.0
    call calc_ai_LJs_ex(4,tau,rho,fac,chi,chi_r,chi_x,chi_rr,chi_xx,chi_rx)
    a2 = a2*(1.0 + chi)

    eta = pi*rho/(6*x0**3)
    call calcKhs(eta, Khs, Khs_e, Khs_ee, Khs_eee)
    ! Multiply a2 with Khs
    a2 = a2*Khs

  end subroutine calc_a2_LJs_ex

  !> Calculate a3 for LJ/s
  !! \author Morten Hammer, March 2019
  subroutine calc_a3_LJs_ex(x0,rho,eps,a3)
    ! Input
    real, intent(in) :: eps !< Epsilon divided by kB
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density
    ! Output
    real, intent(out) :: a3
    ! Locals
    real :: fac,a3_r,a3_rr,a3_x,a3_xx,a3_rx,tau

    fac = eps**3
    tau = 1 - 1/x0
    call calc_ai_LJs_ex(3,tau,rho,fac,a3,a3_r,a3_x,a3_rr,a3_xx,a3_rx)

  end subroutine calc_a3_LJs_ex

  !> Calculate chi (a2 correction) for LJ/s
  !! \author Morten Hammer, Mai 2019
  subroutine calc_chi_LJs_ex(x0,rho,eps,chi)
    ! Input
    real, intent(in) :: eps !< Epsilon divided by kB
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density
    ! Output
    real, intent(out) :: chi
    ! Locals
    real :: fac,chi_r,chi_rr,chi_x,chi_xx,chi_rx, tau

    fac = 1.0
    tau = 1 - 1/x0
    call calc_ai_LJs_ex(4,tau,rho,fac,chi,chi_r,chi_x,chi_rr,chi_xx,chi_rx)

  end subroutine calc_chi_LJs_ex

  !> Calculate ais for LJ/s
  !! \author Morten Hammer, March 2019
  subroutine calc_ais_LJs_ex(i,x0,rho,ai)
    ! Input
    integer, intent(in) :: i !<
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density
    ! Output
    real, intent(out) :: ai
    ! Locals
    real :: fac, tau
    real :: a_r,a_x,a_rr,a_xx,a_rx
    if (i <= 2) then
      fac = 1.0/rho
    else
      fac = 1.0
    endif
    tau = 1 - 1/x0
    call calc_ai_LJs_ex(i,tau,rho,fac,ai,a_r,a_x,a_rr,a_xx,a_rx)
  end subroutine calc_ais_LJs_ex

  !> Calculate a1-a3 for LJ/s
  !! \author Morten Hammer, February 2020
  subroutine calc_ai_reduced_LJs_ex(T_star,rho_star,a1,a2,a3)
    use thermopack_var, only: base_eos_param, get_active_eos, nc
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter
    ! Input
    real, intent(in) :: T_star !< Reduced temperature
    real, intent(in) :: rho_star !< Reduced density
    ! Output
    real, intent(out) :: a1, a2, a3
    ! Locals
    real :: x0, T, s, d, eps
    class(base_eos_param), pointer :: eos
    eps = saftvrmie_param%eps_divk_ij(1,1)
    T = T_star*eps
    eos => get_active_eos()
    select type( p_eos => eos )
    class is ( ljs_bh_eos )
      call calc_hardsphere_diameter(nc,T,p_eos%saftvrmie_var,&
           p_eos%saftvrmie_var%sigma_eff%d,&
           p_eos%saftvrmie_var%sigma_eff%d_T,&
           p_eos%saftvrmie_var%sigma_eff%d_TT,&
           p_eos%saftvrmie_var%dhs%d,&
           p_eos%saftvrmie_var%dhs%d_T,&
           p_eos%saftvrmie_var%dhs%d_TT)
      s = saftvrmie_param%sigma_ij(1,1)
      d = p_eos%saftvrmie_var%dhs%d(1,1)
      x0 = s/d
      eps = 1
      call calc_a1_LJs_ex(x0,rho_star,eps,a1)
      call calc_a2_LJs_ex(x0,rho_star,eps,a2)
      call calc_a3_LJs_ex(x0,rho_star,eps,a3)
    class default
      print *,"calc_a_Tstar_LJs_ex: Wrong active model...."
      a1 = 0
      a2 = 0
      a3 = 0
    end select
  end subroutine calc_ai_reduced_LJs_ex

  !> Initialize the LJs model
  !!
  !! \author Morten Hammer, March 2019
  subroutine init_LJs_bh(nc,comp,ljs,ref)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: N_Avogadro, kB_const
    use saftvrmie_containers, only: cleanup_saftvrmie_param_container, &
         cleanup_saftvrmie_var_container, allocate_saftvrmie_zeta, &
         allocate_saftvrmie_param_container, allocate_saftvrmie_dhs, &
         calcFunAlpha, svrm_opt
    integer, intent(in) :: nc          !< Number of components.
    type(gendata_pointer), intent(inout) :: comp(nc)    !< Component vector.
    class(ljs_bh_eos), intent(inout) :: ljs
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    ! Locals
    real :: f_alpha(6)
    real :: sigma, eps_depth_divk
    integer :: idx
    type(thermo_model), pointer :: p_thermo

    ! Deallocate old memory and init new memory
    call ljs%allocate_and_init(nc,"LJS-BH")
    saftvrmie_param => ljs%saftvrmie_param
    svrm_opt => ljs%svrm_opt

    idx = getLJSdataIdx(ljs%subeosidx,trim(comp(1)%p_comp%ident),ref)
    sigma = LJSarray(idx)%sigma
    eps_depth_divk = LJSarray(idx)%eps_depth_divk

    ! Set component data
    ljs%saftvrmie_param%lambda_a_ij(1,1) = 6
    ljs%saftvrmie_param%lambda_r_ij(1,1) = 12
    call ljs%set_sigma_eps(sigma, eps_depth_divk)
    ljs%saftvrmie_param%alpha_ij(1,1) = alpha_ljs
    call calcFunAlpha(ljs%saftvrmie_param%alpha_ij(1,1), f_alpha)
    ljs%saftvrmie_param%f_alpha_ij(:,1,1) = f_alpha
    ljs%saftvrmie_param%Cij(1,1) = 4
    ljs%saftvrmie_param%ms(1) = 1

    ! Set consistent Rgas
    Rgas = N_Avogadro*kB_const
    kRgas = Rgas*1.0e3

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = Rgas
    p_thermo%kRgas = kRgas !< J/kmol/K

    ! Set ideal gas Cp
    comp(1)%p_comp%id_cp%cptype = 8
    comp(1)%p_comp%id_cp%cp(:) = 0.0
    comp(1)%p_comp%id_cp%cp(1) = 2.5
  end subroutine init_LJs_bh

  !! \author Morten Hammer, January 2021
  subroutine ljs_bh_set_sigma_eps(ljs, sigma, eps_depth_divk)
    class(ljs_bh_eos), intent(inout) :: ljs
    real, intent(in) :: sigma, eps_depth_divk
    !
    ljs%saftvrmie_param%sigma_ij(1,1) = sigma
    ljs%saftvrmie_var%sigma_eff%d(1,1) = sigma
    ljs%saftvrmie_var%sigma_eff%d_T(1,1) = 0
    ljs%saftvrmie_var%sigma_eff%d_TT(1,1) = 0
    ljs%saftvrmie_param%sigma_ij_cube(1,1) = sigma**3
    ljs%saftvrmie_param%eps_divk_ij(1,1) = eps_depth_divk
  end subroutine ljs_bh_set_sigma_eps

  subroutine get_ljs_db_entry(idx, eosidx, comp_name, ref)
    use thermopack_constants, only: ref_len, uid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eosidx !< Index of EOS
    character(len=uid_len), intent(out) :: comp_name !< Component name
    character(len=ref_len), intent(out) :: ref !< Reference string
    eosidx = LJSarray(idx)%eosidx
    comp_name = LJSarray(idx)%compName
    ref = LJSarray(idx)%ref
  end subroutine get_ljs_db_entry

  !> Get the index in the LJSarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getLJSdataIdx(eosidx,comp_name,ref) result(idx)
    use parameters, only: get_pure_data_db_idx
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: comp_name, ref
    ! Locals
    integer :: idx, idx_default
    call get_pure_data_db_idx(get_ljs_db_entry,nLJS,"LJS",&
         eosidx,comp_name,ref,.true.,idx,idx_default)
  end function getLJSdataIdx

  !> Calculate hypotetical pure fluid packing fraction
  !!
  !! \author Morten Hammer, March 2018
  function calc_LJs_bh_zeta(eos,nc,T,V,n) result(zeta)
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter
    use saftvrmie_dispersion, only: calcZetaX
    ! Input/Output
    class(ljs_bh_eos), intent(inout) :: eos
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real :: zeta
    !
    ! Calculate hard-sphere diameter
    call calc_hardsphere_diameter(nc,T,eos%saftvrmie_var,eos%saftvrmie_var%sigma_eff%d,&
         eos%saftvrmie_var%sigma_eff%d_T,eos%saftvrmie_var%sigma_eff%d_TT,eos%saftvrmie_var%dhs%d,&
         eos%saftvrmie_var%dhs%d_T,eos%saftvrmie_var%dhs%d_TT)

    ! Calculate hypotetical pure fluid packing fraction
    call calcZetaX(nc,T,V,n,0,eos%saftvrmie_var%dhs,eos%saftvrmie_var%zeta)
    zeta = eos%saftvrmie_var%zeta%zx
  end function calc_LJs_bh_zeta

  !> Calculate common variable properties for LJs
  !!
  !! \author Morten Hammer, March 2019
  subroutine preCalcLJs(eos,nc,T,V,n,difflevel)
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
         calc_binary_effective_sigma, calc_binary_effective_eps_divk, &
         calc_d_pure
    use saftvrmie_dispersion, only: calcZetaX, &
         calcKhsTVn, calcAlpha
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< Level of differentials
    ! Input/Output
    class(ljs_bh_eos), intent(inout) :: eos
    !
    ! Calculate hard-sphere diameter
    call calc_hardsphere_diameter(nc,T,eos%saftvrmie_var,eos%saftvrmie_var%sigma_eff%d,&
         eos%saftvrmie_var%sigma_eff%d_T,eos%saftvrmie_var%sigma_eff%d_TT,eos%saftvrmie_var%dhs%d,&
         eos%saftvrmie_var%dhs%d_T,eos%saftvrmie_var%dhs%d_TT)
    ! Calculate hypotetical pure fluid packing fraction
    call calcZetaX(nc,T,V,n,difflevel+1,eos%saftvrmie_var%dhs,eos%saftvrmie_var%zeta)
    ! Calculate isothermal hard sphere compressibillity factor
    call calcKhsTVn(nc,T,V,n,difflevel+1,eos%saftvrmie_var)
    !> Calculate packing fraction
    call calcZetaX(nc,T,V,n,difflevel,eos%saftvrmie_var%sigma_eff,&
         eos%saftvrmie_var%zeta_bar, zetaxbar=.true.)
    !> Calculate rho_star
    call calcRhoStarLJs(nc,T,V,n,difflevel+1,eos%saftvrmie_param%sigma_ij(1,1),&
         eos%saftvrmie_var%rho_star)
  end subroutine preCalcLJs

  !> Calculate rho^*
  !! Store results in saftvrmie_var
  !!
  !! \author Morten Hammer, May 2019
  subroutine calcRhoStarLJs(nc,T,V,n,difflevel,sigma,rho_star)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< 0-2. Differential order
    real, intent(in) :: sigma
    ! Output
    type(saftvrmie_zeta), intent(inout) :: rho_star
    ! Locals
    real :: prefactor
    prefactor = N_AVOGADRO*sigma**3/V
    rho_star%zx = prefactor*n(1)
    if (difflevel > 0) then
      rho_star%zx_T = 0.0
      rho_star%zx_V = -rho_star%zx/V
      rho_star%zx_n = prefactor
    endif
    if (difflevel > 1) then
      rho_star%zx_TT = 0
      rho_star%zx_VV = 2*rho_star%zx/V**2
      rho_star%zx_TV = 0
      rho_star%zx_Vn = -rho_star%zx_n/V
      rho_star%zx_Tn = 0
      rho_star%zx_nn = 0
      rho_star%zx_VVV = -6.0*rho_star%zx/V**3
      rho_star%zx_VVT = 0
      rho_star%zx_VTn = 0
      rho_star%zx_VVn = 2.0*rho_star%zx_n/V**2
      rho_star%zx_Vnn = 0
      rho_star%zx_VTT = 0
    endif
  end subroutine calcRhoStarLJs

  !> Calculate residual reduced Helmholts free energy
  !!
  !! \author Morten Hammer, March 2019
  subroutine calcFresLJs_bh(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_hardsphere, only: calc_hardsphere_helmholtzenergy
    ! Input
    class(ljs_bh_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    real :: beta(3),nsum
    real :: Fhs,Fhs_T,Fhs_V,Fhs_TT,Fhs_VV,Fhs_TV
    real, dimension(nc) :: Fhs_n,Fhs_Tn,Fhs_Vn
    real, dimension(nc,nc) :: Fhs_nn
    real :: F1,F1_T,F1_V,F1_TT,F1_VV,F1_TV
    real, dimension(nc) :: F1_n,F1_Tn,F1_Vn
    real, dimension(nc,nc) :: F1_nn
    real :: F2,F2_T,F2_V,F2_TT,F2_VV,F2_TV
    real, dimension(nc) :: F2_n,F2_Tn,F2_Vn
    real, dimension(nc,nc) :: F2_nn
    real :: F3,F3_T,F3_V,F3_TT,F3_VV,F3_TV
    real, dimension(nc) :: F3_n,F3_Tn,F3_Vn
    real, dimension(nc,nc) :: F3_nn
    integer :: l, difflevel
    real :: am, am_T, am_V, am_n(nc)

    if ( present(F_TT) .or. present(F_VV) .or. present(F_TV) .or. &
         present(F_Tn) .or. present(F_Vn) .or. present(F_nn)) then
      difflevel = 2
    else if (present(F_T) .or. present(F_V) .or. present(F_n)) then
      difflevel = 1
    else
      difflevel = 0
    endif

    ! Precalculate common variables
    call preCalcLJs(eos,nc,T,V,n,difflevel)

    ! Calculate hard-sphere term
    if (eos%enable_hs) then
      call calc_hardsphere_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,&
           Fhs,a_T=Fhs_T,a_V=Fhs_V,a_n=Fhs_n,&
           a_TT=Fhs_TT,a_VV=Fhs_VV,a_TV=Fhs_TV,a_Tn=Fhs_Tn,a_Vn=Fhs_Vn,a_nn=Fhs_nn)
    else
      Fhs = 0.0
      Fhs_T = 0.0
      Fhs_V = 0.0
      Fhs_TT = 0.0
      Fhs_VV = 0.0
      Fhs_TV = 0.0
      Fhs_n = 0.0
      Fhs_Tn = 0.0
      Fhs_Vn = 0.0
      Fhs_nn = 0.0
    endif

    ! Calculate first order monomer term
    if (eos%enable_A1) then
      call calc_LJs_a1(nc,T,V,n,eos%saftvrmie_var,&
           F1,a1_T=F1_T,a1_V=F1_V,a1_n=F1_n,a1_TT=F1_TT,a1_VV=F1_VV,&
           a1_TV=F1_TV,a1_Tn=F1_Tn,a1_Vn=F1_Vn,a1_nn=F1_nn)
    else
      F1 = 0.0
      F1_T = 0.0
      F1_V = 0.0
      F1_TT = 0.0
      F1_VV = 0.0
      F1_TV = 0.0
      F1_n = 0.0
      F1_Tn = 0.0
      F1_Vn = 0.0
      F1_nn = 0.0
    endif

    ! Calculate second order monomer term
    if (eos%enable_A2) then
      call calc_LJs_a2(eos,nc,T,V,n,&
           F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n,a2_TT=F2_TT,&
           a2_VV=F2_VV,a2_TV=F2_TV,a2_Tn=F2_Tn,a2_Vn=F2_Vn,a2_nn=F2_nn)
        else
      F2 = 0.0
      F2_T = 0.0
      F2_V = 0.0
      F2_TT = 0.0
      F2_VV = 0.0
      F2_TV = 0.0
      F2_n = 0.0
      F2_Tn = 0.0
      F2_Vn = 0.0
      F2_nn = 0.0
    endif

    ! Calculate third order monomer term
    if (eos%enable_A3) then
      call calc_LJs_a3(eos,nc,T,V,n,&
           F3,a3_T=F3_T,a3_V=F3_V,a3_n=F3_n,a3_TT=F3_TT,&
           a3_VV=F3_VV,a3_TV=F3_TV,a3_Tn=F3_Tn,a3_Vn=F3_Vn,a3_nn=F3_nn)
    else
      F3 = 0.0
      F3_T = 0.0
      F3_V = 0.0
      F3_TT = 0.0
      F3_VV = 0.0
      F3_TV = 0.0
      F3_n = 0.0
      F3_Tn = 0.0
      F3_Vn = 0.0
      F3_nn = 0.0
    endif

    beta(1) = 1.0/T
    beta(2) = beta(1)*beta(1)
    beta(3) = beta(1)*beta(2)
    nsum = sum(n)
    am = Fhs + beta(1)*F1 + beta(2)*F2 + beta(3)*F3
    F = nsum*am
    if (present(F_T) .or. present(F_Tn)) then
      am_T = Fhs_T + beta(1)*F1_T + beta(2)*F2_T + beta(3)*F3_T &
           -(beta(1)*F1 + 2.0*beta(2)*F2 + 3.0*beta(3)*F3)/T
    endif
    if (present(F_T)) then
      F_T = nsum*am_T
    endif
    if (present(F_V) .or. present(F_Vn)) then
      am_V = Fhs_V + beta(1)*F1_V + beta(2)*F2_V + beta(3)*F3_V
    endif
    if (present(F_V)) then
      F_V = nsum*am_V
    endif
    if (present(F_TT)) then
      F_TT = nsum*(Fhs_TT + beta(1)*F1_TT + beta(2)*F2_TT + beta(3)*F3_TT) &
           +nsum*(2.0*beta(1)*F1 + 6.0*beta(2)*F2 + 12.0*beta(3)*F3)/T**2 &
           -2.0*nsum*(beta(1)*F1_T + 2.0*beta(2)*F2_T + 3.0*beta(3)*F3_T)/T
    endif
    if (present(F_VV)) then
      F_VV = nsum*(Fhs_VV + beta(1)*F1_VV + beta(2)*F2_VV + beta(3)*F3_VV)
    endif
    if (present(F_TV)) then
      F_TV = nsum*(Fhs_TV + beta(1)*F1_TV + beta(2)*F2_TV + beta(3)*F3_TV) &
           -nsum*(beta(1)*F1_V + 2.0*beta(2)*F2_V + 3.0*beta(3)*F3_V)/T
    endif
    if (present(F_Tn)) then
      F_Tn = nsum*(Fhs_Tn + beta(1)*F1_Tn + beta(2)*F2_Tn + beta(3)*F3_Tn) &
           -nsum*(beta(1)*F1_n + 2.0*beta(1)**2*F2_n + 3.0*beta(3)*F3_n)/T &
           + am_T
    endif
    if (present(F_Vn)) then
      F_Vn = nsum*(Fhs_Vn + beta(1)*F1_Vn + beta(2)*F2_Vn + beta(3)*F3_Vn) &
           + am_V
    endif
    if (present(F_n) .or. present(F_nn)) then
      am_n = Fhs_n + beta(1)*F1_n + beta(2)*F2_n + beta(3)*F3_n
    endif
    if (present(F_n)) then
      F_n = nsum*am_n + am
    endif
    if (present(F_nn)) then
      F_nn = nsum*(Fhs_nn + beta(1)*F1_nn + beta(1)**2*F2_nn + beta(1)**3*F3_nn)
      do l=1,nc
        F_nn(:,l) = F_nn(:,l) + am_n(l) + am_n
      enddo
    endif

  end subroutine calcFresLJs_bh

  subroutine assign_ljs_bh_eos(this, other)
    class(ljs_bh_eos), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (ljs_bh_eos)
      call this%saftvrmie_eos%assign_base_eos_param(other)
      ! Model control
      this%use_Lafitte_a3 = other%use_Lafitte_a3
      this%enable_chi_correction = other%enable_chi_correction
      this%enable_hs = other%enable_hs
      this%enable_a1 = other%enable_a1
      this%enable_a2 = other%enable_a2
      this%enable_a3 = other%enable_a3
    class default
    end select
  end subroutine assign_ljs_bh_eos

  subroutine ljs_bh_model_control(use_Lafitte_a3, enable_chi_correction,&
       enable_hs, enable_a1, enable_a2, enable_a3)
    use thermopack_var, only: base_eos_param, thermo_model, get_active_thermo_model
    logical, intent(in) :: use_Lafitte_a3, enable_chi_correction,&
         enable_hs, enable_a1, enable_a2, enable_a3
    ! Locals
    class(base_eos_param), pointer :: eos
    type(thermo_model), pointer :: p_eos_cont
    integer :: i
    p_eos_cont => get_active_thermo_model()
    if (allocated(p_eos_cont%eos)) then
      do i=1,size(p_eos_cont%eos)
        if (associated(p_eos_cont%eos(i)%p_eos)) then
          eos => p_eos_cont%eos(i)%p_eos
          select type( p_eos => eos )
          class is ( ljs_bh_eos )
            p_eos%use_Lafitte_a3 = use_Lafitte_a3
            p_eos%enable_chi_correction = enable_chi_correction
            p_eos%enable_hs = enable_hs
            p_eos%enable_a1 = enable_a1
            p_eos%enable_a2 = enable_a2
            p_eos%enable_a3 = enable_a3
          end select
        else
           print *,"ljs_bh_model_control: eos not acociated"
         endif
       enddo
     else
       print *,"ljs_bh_model_control: eos array not allocted found"
     endif
  end subroutine ljs_bh_model_control

  subroutine ljs_bh_set_pure_params(sigma, eps_depth_divk)
    use thermopack_var, only: base_eos_param, get_active_eos
    real, intent(in) :: sigma, eps_depth_divk ! sigma/m, eps_depth_divk/K
    ! Locals
    class(base_eos_param), pointer :: eos
    type(thermo_model), pointer :: p_eos_cont
    integer :: i
    eos => get_active_eos()
    select type( p_eos => eos )
    class is ( ljs_bh_eos )
      p_eos%saftvrmie_param%sigma_ij(1,1) = sigma
      p_eos%saftvrmie_param%eps_divk_ij(1,1) = eps_depth_divk
    class default
      print*, "ljs_bh_set_pure_params wrong active model ..."
    end select
  end subroutine ljs_bh_set_pure_params

  subroutine ljs_bh_get_pure_params(sigma, eps_depth_divk)
    use thermopack_var, only: base_eos_param, get_active_eos
    real, intent(out) :: sigma, eps_depth_divk ! sigma/m, eps_depth_divk/K
    !
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type( p_eos => eos )
    class is ( ljs_bh_eos )
      sigma = p_eos%saftvrmie_param%sigma_ij(1,1)
      eps_depth_divk = p_eos%saftvrmie_param%eps_divk_ij(1,1)
    class default
      sigma = 0
      eps_depth_divk = 0
    end select
  end subroutine ljs_bh_get_pure_params

  ! Calculate the Barker--Henderson diameter (d/s)
  subroutine ljs_bh_get_bh_diameter_div_sigma(T_star,d_bh)
    use thermopack_var, only: base_eos_param, get_active_eos, nc
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter
    implicit none
    ! Input
    real, intent(in) :: T_star !< Reduced temperature [-]
    ! Output
    real, intent(out) :: d_bh !< Barker--Henderson hard-sphere diameter
    ! Locals
    real :: T, eps, s, d
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type( p_eos => eos )
    class is ( ljs_bh_eos )
      eps = saftvrmie_param%eps_divk_ij(1,1)
      T = T_star*eps
      ! Calculate hard-sphere diameter
      call calc_hardsphere_diameter(nc,T,p_eos%saftvrmie_var,&
           p_eos%saftvrmie_var%sigma_eff%d,&
           p_eos%saftvrmie_var%sigma_eff%d_T,&
           p_eos%saftvrmie_var%sigma_eff%d_TT,&
           p_eos%saftvrmie_var%dhs%d,&
           p_eos%saftvrmie_var%dhs%d_T,&
           p_eos%saftvrmie_var%dhs%d_TT)
      s = saftvrmie_param%sigma_ij(1,1)
      d = p_eos%saftvrmie_var%dhs%d(1,1)
      d_bh = d/s
    class default
      print *,"get_bh_diameter: Wrong active model...."
      d_bh = 0
    end select
  end subroutine ljs_bh_get_bh_diameter_div_sigma

  !# WCA #######################################

  !> Initialize the LJs WCA model
  !!
  !! \author Morten Hammer, March 2021
  subroutine init_LJs_WCA(nc,comp,ljs,ref)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: N_Avogadro, kB_const
    use saftvrmie_containers, only: cleanup_saftvrmie_zeta, &
         cleanup_saftvrmie_dhs, allocate_saftvrmie_zeta, &
         allocate_saftvrmie_dhs
    integer, intent(in) :: nc          !< Number of components.
    type(gendata_pointer), intent(inout) :: comp(nc)    !< Component vector.
    class(ljs_wca_eos), intent(inout) :: ljs
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    ! Locals
    real :: sigma, eps_depth_divk
    integer :: idx
    type(thermo_model), pointer :: p_thermo

    call ljs%allocate_and_init(nc,"WCA")
    svrm_opt => ljs%svrm_opt

    idx = getLJSdataIdx(ljs%subeosidx,trim(comp(1)%p_comp%ident),ref)
    sigma = LJSarray(idx)%sigma
    eps_depth_divk = LJSarray(idx)%eps_depth_divk

    ! Set component data
    call ljs%set_sigma_eps(sigma, eps_depth_divk)

    ! Set consistent Rgas
    Rgas = N_Avogadro*kB_const
    kRgas = Rgas*1.0e3

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = Rgas
    p_thermo%kRgas = kRgas !< J/kmol/K

    ! Set ideal gas Cp
    comp(1)%p_comp%id_cp%cptype = 8
    comp(1)%p_comp%id_cp%cp(:) = 0.0
    comp(1)%p_comp%id_cp%cp(1) = 2.5
  end subroutine init_LJs_WCA

  !! \author Morten Hammer, October 2021
  subroutine ljs_wca_set_sigma_eps(ljs, sigma, eps_depth_divk)
    class(ljs_wca_eos), intent(inout) :: ljs
    real, intent(in) :: sigma, eps_depth_divk
    !
    ljs%sigma = sigma
    ljs%eps_divk = eps_depth_divk
  end subroutine ljs_wca_set_sigma_eps

  subroutine assign_ljs_wca_eos(this, other)
    class(ljs_wca_eos), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (ljs_wca_eos)
      call this%assign_base_eos_param(other)
      ! Structures
      this%dhs = other%dhs
      this%eta_hs = other%eta_hs
      ! Parameters
      this%sigma = other%sigma
      this%eps_divk = other%eps_divk
      ! Model control
      this%enable_cavity = other%enable_cavity
      this%enable_hs = other%enable_hs
      this%enable_a1 = other%enable_a1
      this%enable_a2 = other%enable_a2
      this%enable_a3 = other%enable_a3
      this%enable_a4 = other%enable_a4
      ! HS options
      this%svrm_opt => other%svrm_opt
    class default
    end select
  end subroutine assign_ljs_wca_eos

  subroutine wca_dealloc(eos)
    use saftvrmie_containers, only: cleanup_saftvrmie_dhs, cleanup_saftvrmie_zeta
    class(ljs_wca_eos), intent(inout) :: eos
    ! Locals
    integer :: stat
    call base_eos_dealloc(eos)
    call cleanup_saftvrmie_dhs(eos%dhs)
    call cleanup_saftvrmie_zeta(eos%eta_hs)
    if (eos%ovner_of_svrm_opt .and. &
         associated(eos%svrm_opt)) then
      eos%ovner_of_svrm_opt = .false.
      deallocate(eos%svrm_opt,stat=stat)
      if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate svrm_opt")
      eos%svrm_opt => NULL()
    endif
   end subroutine wca_dealloc

  subroutine wca_allocate_and_init(eos,nc,eos_label)
    use saftvrmie_containers, only: allocate_saftvrmie_dhs,&
         allocate_saftvrmie_zeta
    class(ljs_wca_eos), intent(inout) :: eos
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label !< EOS label
    ! Locals
    integer :: stat
    !
    call eos%dealloc()
    ! Allocate structures
    call allocate_saftvrmie_dhs(nc,eos%dhs)
    call allocate_saftvrmie_zeta(nc,eos%eta_hs)

    if (eos%ovner_of_svrm_opt .and. &
         associated(eos%svrm_opt)) then
      eos%ovner_of_svrm_opt = .false.
      deallocate(eos%svrm_opt,stat=stat)
      if (stat /= 0) call stoperror("wca_allocate_and_init: Not able to deallocate svrm_opt")
      eos%svrm_opt => NULL()
    endif
    allocate(eos%svrm_opt,stat=stat)
    if (stat /= 0) call stoperror("wca_allocate_and_init: Not able to allocate eos%svrm_opt")
    eos%ovner_of_svrm_opt = .true.
  end subroutine wca_allocate_and_init

  subroutine ljs_wca_model_control(enable_cavity,&
       enable_hs, enable_a1, enable_a2, enable_a3, enable_a4)
    use thermopack_var, only: base_eos_param, thermo_model, get_active_thermo_model
    logical, intent(in) :: enable_cavity,&
         enable_hs, enable_a1, enable_a2, enable_a3, enable_a4
    ! Locals
    class(base_eos_param), pointer :: eos
    type(thermo_model), pointer :: p_eos_cont
    integer :: i
    p_eos_cont => get_active_thermo_model()
    if (allocated(p_eos_cont%eos)) then
      do i=1,size(p_eos_cont%eos)
        if (associated(p_eos_cont%eos(i)%p_eos)) then
          eos => p_eos_cont%eos(i)%p_eos
          select type( p_eos => eos )
          class is ( ljs_wca_eos )
            p_eos%enable_cavity = enable_cavity
            p_eos%enable_hs = enable_hs
            p_eos%enable_a1 = enable_a1
            p_eos%enable_a2 = enable_a2
            p_eos%enable_a3 = enable_a3
            p_eos%enable_a4 = enable_a4
          end select
        else
           print *,"ljs_wca_model_control: eos not acociated"
         endif
       enddo
     else
       print *,"ljs_wca_model_control: eos array not allocted found"
     endif
   end subroutine ljs_wca_model_control

  subroutine ljs_wca_set_pure_params(sigma, eps_depth_divk)
    use thermopack_var, only: base_eos_param, thermo_model, get_active_thermo_model
    real, intent(in) :: sigma, eps_depth_divk ! sigma/m, eps_depth_divk/K
    ! Locals
    class(base_eos_param), pointer :: eos
    type(thermo_model), pointer :: p_eos_cont
    integer :: i
    p_eos_cont => get_active_thermo_model()
    if (allocated(p_eos_cont%eos)) then
      do i=1,size(p_eos_cont%eos)
        if (associated(p_eos_cont%eos(i)%p_eos)) then
          eos => p_eos_cont%eos(i)%p_eos
          select type( p_eos => eos )
          class is ( ljs_wca_eos )
            call p_eos%set_sigma_eps(sigma, eps_depth_divk)
          end select
        else
           print *,"ljs_wca_set_pure_params: eos not acociated"
         endif
       enddo
     else
       print *,"ljs_wca_set_pure_params: eos array not allocted found"
     endif
   end subroutine ljs_wca_set_pure_params

  subroutine ljs_wca_get_pure_params(sigma, eps_depth_divk)
    use thermopack_var, only: base_eos_param, get_active_eos
    real, intent(out) :: sigma, eps_depth_divk ! sigma/m, eps_depth_divk/K
    !
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type( p_eos => eos )
    class is ( ljs_wca_eos )
      sigma = p_eos%sigma
      eps_depth_divk = p_eos%eps_divk
    class default
      sigma = 0
      eps_depth_divk = 0
    end select
  end subroutine ljs_wca_get_pure_params

  function calc_ljx_WCA_zeta(eos,nc,T,V,n) result(zeta)
    use hardsphere_wca, only: calc_dhs_WCA
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Input/Output
    class(ljs_wca_eos), intent(inout) :: eos
    ! Output
    real :: zeta
    ! Locals
    real :: sigma, eps_divk
    sigma = eos%sigma
    eps_divk = eos%eps_divk
    ! The hard-sphere diameter
    call calc_dhs_WCA(nc,sigma,eps_divk,T,eos%dhs)
    ! Packing fraction
    call calcZetaX_vdW_no_segments(nc,T,V,n,eos%dhs,eos%eta_hs)
    zeta = eos%eta_hs%zx
  end function calc_ljx_WCA_zeta

  !> Calculate residual reduced Helmholts free energy
  !! for WCA based perturbation theory
  !!
  !! \author Morten Hammer, April 2021
  subroutine calcFres_WCA(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    ! Input
    class(ljs_wca_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn

    select type(p_eos => eos)
    class is(ljx_ux_eos)
      if (p_eos%is_uf_theory) then
        call calcFresLJ_uf_theory(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
             F_VV,F_TV,F_Tn,F_Vn,F_nn)
      else
        call calcFresLJs_uv_theory(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
             F_VV,F_TV,F_Tn,F_Vn,F_nn)
      endif
    class default
      call calcFresLJs_WCA(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
    end select
  end subroutine calcFres_WCA

    !> Calculate residual reduced Helmholts free energy
  !! for WCA perturbation theory
  !!
  !! \author Morten Hammer, April 2021
  subroutine calcFresLJs_WCA(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_hardsphere, only: calc_pure_ahs_div_nRT, calc_hardsphere_diameter
    use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta, &
         allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs, &
         allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta
    ! Input
    class(ljs_wca_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    real :: beta(4,0:2),nsum
    !
    real :: Fhs,Fhs_T,Fhs_V,Fhs_TT,Fhs_VV,Fhs_TV
    real, dimension(nc) :: Fhs_n,Fhs_Tn,Fhs_Vn
    real, dimension(nc,nc) :: Fhs_nn
    !
    real :: FhsC,FhsC_T,FhsC_V,FhsC_TT,FhsC_VV,FhsC_TV
    real, dimension(nc) :: FhsC_n,FhsC_Tn,FhsC_Vn
    real, dimension(nc,nc) :: FhsC_nn
    !
    real :: F1,F1_T,F1_V,F1_TT,F1_VV,F1_TV
    real, dimension(nc) :: F1_n,F1_Tn,F1_Vn
    real, dimension(nc,nc) :: F1_nn
    !
    real :: F2,F2_T,F2_V,F2_TT,F2_VV,F2_TV
    real, dimension(nc) :: F2_n,F2_Tn,F2_Vn
    real, dimension(nc,nc) :: F2_nn
    !
    real :: F3,F3_T,F3_V,F3_TT,F3_VV,F3_TV
    real, dimension(nc) :: F3_n,F3_Tn,F3_Vn
    real, dimension(nc,nc) :: F3_nn
    !
    real :: F4,F4_T,F4_V,F4_TT,F4_VV,F4_TV
    real, dimension(nc) :: F4_n,F4_Tn,F4_Vn
    real, dimension(nc,nc) :: F4_nn
    !
    real :: sigma, eps_divk
    integer :: i, k
    !
    sigma = eos%sigma
    eps_divk = eos%eps_divk
    ! The hard-sphere diameter
    call calc_dhs_WCA(nc,sigma,eps_divk,T,eos%dhs)
    ! Calculate hard-sphere term
    if (eos%enable_hs) then
      ! Packing fraction
      call calcZetaX_vdW_no_segments(nc,T,V,n,eos%dhs,eos%eta_hs)
      ! Get pure fluid Helmholtz energy
      call calc_pure_ahs_div_nRT(nc,eos%eta_hs,a=Fhs,a_T=Fhs_T,a_V=Fhs_V,a_n=Fhs_n,&
           a_TT=Fhs_TT,a_TV=Fhs_TV,a_Tn=Fhs_Tn,a_VV=Fhs_VV,a_Vn=Fhs_Vn,a_nn=Fhs_nn)
      nsum = sum(n)
      Fhs_nn(1,1) = nsum*Fhs_nn(1,1) + 2*Fhs_n(1)
      Fhs_n = nsum*Fhs_n + Fhs
      Fhs = nsum*Fhs
      Fhs_Tn = nsum*Fhs_Tn + Fhs_T
      Fhs_T = nsum*Fhs_T
      Fhs_TT = nsum*Fhs_TT
      Fhs_Vn = nsum*Fhs_Vn + Fhs_V
      Fhs_V = nsum*Fhs_V
      Fhs_VV = nsum*Fhs_VV
      Fhs_TV = nsum*Fhs_TV
      !
      if (eos%enable_cavity) then
        call calc_cavity_integral_LJ_Fres(nc,sigma,eps_divk,eos%dhs,eos%eta_hs,T,V,n,&
             FhsC,FhsC_T,FhsC_V,FhsC_n,FhsC_TT,FhsC_VV,FhsC_TV,FhsC_Tn,FhsC_Vn,&
             FhsC_nn)
      else
        FhsC = 0
        FhsC_T = 0
        FhsC_V = 0
        FhsC_TT = 0
        FhsC_VV = 0
        FhsC_TV = 0
        FhsC_n = 0
        FhsC_Tn = 0
        FhsC_Vn = 0
        FhsC_nn = 0
      endif
    else
      Fhs = 0.0
      Fhs_T = 0.0
      Fhs_V = 0.0
      Fhs_TT = 0.0
      Fhs_VV = 0.0
      Fhs_TV = 0.0
      Fhs_n = 0.0
      Fhs_Tn = 0.0
      Fhs_Vn = 0.0
      Fhs_nn = 0.0
      !
      FhsC = 0.0
      FhsC_T = 0.0
      FhsC_V = 0.0
      FhsC_TT = 0.0
      FhsC_VV = 0.0
      FhsC_TV = 0.0
      FhsC_n = 0.0
      FhsC_Tn = 0.0
      FhsC_Vn = 0.0
      FhsC_nn = 0.0
    endif

    ! Calculate first order term
    if (eos%enable_A1) then
      call calc_LJs_WCA_ai_TVN(nc,sigma,eps_divk,1,eos%dhs,T,V,n,a=F1,a_t=F1_T,a_V=F1_V,a_n=F1_n,&
           a_tt=F1_TT,a_vv=F1_VV,a_tv=F1_TV,a_tn=F1_Tn,a_vn=F1_Vn,a_nn=F1_nn)
    else
      F1 = 0.0
      F1_T = 0.0
      F1_V = 0.0
      F1_TT = 0.0
      F1_VV = 0.0
      F1_TV = 0.0
      F1_n = 0.0
      F1_Tn = 0.0
      F1_Vn = 0.0
      F1_nn = 0.0
    endif

    ! Calculate second order term
    if (eos%enable_A2) then
      call calc_LJs_WCA_ai_TVN(nc,sigma,eps_divk,2,eos%dhs,T,V,n,a=F2,a_t=F2_T,a_V=F2_V,a_n=F2_n,&
           a_tt=F2_TT,a_vv=F2_VV,a_tv=F2_TV,a_tn=F2_Tn,a_vn=F2_Vn,a_nn=F2_nn)
    else
      F2 = 0.0
      F2_T = 0.0
      F2_V = 0.0
      F2_TT = 0.0
      F2_VV = 0.0
      F2_TV = 0.0
      F2_n = 0.0
      F2_Tn = 0.0
      F2_Vn = 0.0
      F2_nn = 0.0
    endif

    ! Calculate third order term
    if (eos%enable_A3) then
      call calc_LJs_WCA_ai_TVN(nc,sigma,eps_divk,3,eos%dhs,T,V,n,a=F3,a_t=F3_T,a_V=F3_V,a_n=F3_n,&
           a_tt=F3_TT,a_vv=F3_VV,a_tv=F3_TV,a_tn=F3_Tn,a_vn=F3_Vn,a_nn=F3_nn)
    else
      F3 = 0.0
      F3_T = 0.0
      F3_V = 0.0
      F3_TT = 0.0
      F3_VV = 0.0
      F3_TV = 0.0
      F3_n = 0.0
      F3_Tn = 0.0
      F3_Vn = 0.0
      F3_nn = 0.0
    endif

    ! Calculate forth order term
    if (eos%enable_A4) then
      call calc_LJs_WCA_ai_TVN(nc,sigma,eps_divk,4,eos%dhs,T,V,n,a=F4,a_t=F4_T,a_V=F4_V,a_n=F4_n,&
           a_tt=F4_TT,a_vv=F4_VV,a_tv=F4_TV,a_tn=F4_Tn,a_vn=F4_Vn,a_nn=F4_nn)
    else
      F4 = 0.0
      F4_T = 0.0
      F4_V = 0.0
      F4_TT = 0.0
      F4_VV = 0.0
      F4_TV = 0.0
      F4_n = 0.0
      F4_Tn = 0.0
      F4_Vn = 0.0
      F4_nn = 0.0
    endif

    ! F = A/(RT)
    beta(1,0) = 1.0/T
    beta(2,0) = beta(1,0)*beta(1,0)
    beta(3,0) = beta(1,0)*beta(2,0)
    beta(4,0) = beta(2,0)*beta(2,0)
    do k=1,4
      do i=1,2
        beta(k,i) = -(i+k-1)*beta(k,i-1)*beta(1,0)
      enddo
    enddo
    F = Fhs + FhsC + beta(1,0)*F1 + beta(2,0)*F2 + beta(3,0)*F3 + beta(4,0)*F4
    if (present(F_T)) then
      F_T = Fhs_T + FhsC_T + beta(1,1)*F1 + beta(2,1)*F2 + beta(3,1)*F3 + beta(4,1)*F4 &
           + beta(1,0)*F1_T + beta(2,0)*F2_T + beta(3,0)*F3_T + beta(4,0)*F4_T
    endif
    if (present(F_V)) then
      F_V = Fhs_V + FhsC_V + beta(1,0)*F1_V + beta(2,0)*F2_V + beta(3,0)*F3_V + beta(4,0)*F4_V
    endif
    if (present(F_TT)) then
      F_TT = Fhs_TT + FhsC_TT + beta(1,2)*F1 + beta(2,2)*F2 + beta(3,2)*F3 + beta(4,2)*F4 &
           + beta(1,0)*F1_TT + beta(2,0)*F2_TT + beta(3,0)*F3_TT + beta(4,0)*F4_TT &
           + 2*(beta(1,1)*F1_T + beta(2,1)*F2_T + beta(3,1)*F3_T + beta(4,1)*F4_T)
    endif
    if (present(F_VV)) then
      F_VV = Fhs_VV + FhsC_VV + beta(1,0)*F1_VV + beta(2,0)*F2_VV + beta(3,0)*F3_VV + beta(4,0)*F4_VV
    endif
    if (present(F_TV)) then
      F_TV = Fhs_TV + FhsC_TV + beta(1,1)*F1_V + beta(2,1)*F2_V + beta(3,1)*F3_V + beta(4,1)*F4_V &
           + beta(1,0)*F1_TV + beta(2,0)*F2_TV + beta(3,0)*F3_TV + beta(4,0)*F4_TV
    endif
    if (present(F_Tn)) then
      F_Tn = Fhs_Tn + FhsC_Tn + beta(1,0)*F1_Tn + beta(2,0)*F2_Tn + beta(3,0)*F3_Tn + beta(4,0)*F4_Tn &
           + beta(1,1)*F1_n + beta(2,1)*F2_n + beta(3,1)*F3_n + beta(4,1)*F4_n
    endif
    if (present(F_Vn)) then
      F_Vn = Fhs_Vn + FhsC_Vn + beta(1,0)*F1_Vn + beta(2,0)*F2_Vn + beta(3,0)*F3_Vn + beta(4,0)*F4_Vn
    endif
    if (present(F_n)) then
      F_n = Fhs_n + FhsC_n + beta(1,0)*F1_n + beta(2,0)*F2_n + beta(3,0)*F3_n + beta(4,0)*F4_n
    endif
    if (present(F_nn)) then
      F_nn = Fhs_nn + FhsC_nn + beta(1,0)*F1_nn + beta(2,0)*F2_nn + beta(3,0)*F3_nn + beta(4,0)*F4_nn
    endif
  end subroutine calcFresLJs_WCA

  !> Calculate a1 of WCA LJs
  !!
  !! \author Morten Hammer, 2021-04
  subroutine calc_LJs_WCA_ai_TVN(nc,sigma,eps_divk,i,dhs,T,V,n,a,a_t,a_V,a_n,a_tt,&
       a_vv,a_tv,a_tn,a_vn,a_nn)
    use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta, &
         allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta
    use saftvrmie_dispersion, only: calcXDifferentials
    ! Input
    integer, intent(in) :: i !< Calculate a_i
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: sigma
    real, intent(in) :: eps_divk !< Well depth div. Boltzmann constant
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: V !< Volume (m3)
    real, intent(in) :: n(nc) !< Mol number (mol)
    ! Output
    real, intent(out) :: a
    real, optional, intent(out) :: a_V,a_t,a_n(nc),a_vv,a_tt,a_tv
    real, optional, intent(out) :: a_nn(nc,nc),a_tn(nc),a_vn(nc)
    ! Locals
    integer :: difflevel
    real :: rho_star !< Reduced density
    real :: a_r,a_y,a_rr,a_yy,a_ry
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s_T, s_TT !< Sigma
    real :: y, y_T, y_TT
    real :: a_T_local, a_V_local, nsum
    type(saftvrmie_zeta) :: rho_s
    if ( present(a_TT) .or. present(a_VV) .or. present(a_TV) .or. &
         present(a_Tn) .or. present(a_Vn) .or. present(a_nn)) then
      difflevel = 2
    else if (present(a_T) .or. present(a_V) .or. present(a_n)) then
      difflevel = 1
    else
      difflevel = 0
    endif
    call allocate_saftvrmie_zeta(nc,rho_s)
    call calcRhoStarLJs(nc,T,V,n,difflevel,sigma,rho_s)
    s_T = 0.0
    s_TT = 0.0
    d = dhs%d(1,1)
    d_T = dhs%d_T(1,1)
    d_TT = dhs%d_TT(1,1)
    call calcXDifferentials(d,d_T,d_TT,sigma,s_T,s_TT,x0,x0_T,x0_TT,assmue_s_of_T=.true.)
    y = 2**(real(1)/6)-x0
    y_T = -x0_T
    y_TT = -x0_TT
    rho_star = rho_s%zx
    call calc_LJs_WCA_ai(i,eps_divk,y,rho_star,a,a_r,a_y,a_rr,a_yy,a_ry)
    call convert_zeta_x_to_TVn(nc,y,y_T,y_TT,rho_s,&
         a,a_r,a_y,a_rr,a_yy,a_ry,0.0,0.0,0.0,&
         a_T_local,a_V_local,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
         difflevel=difflevel)
    call cleanup_saftvrmie_zeta(rho_s)

    ! Multiply by n
    nsum = sum(n)
    if (present(a_nn)) a_nn(1,1) = nsum*a_nn(1,1) + 2*a_n(1)
    if (present(a_n)) a_n = nsum*a_n + a
    a = nsum*a
    if (present(a_Tn)) a_Tn = nsum*a_Tn + a_T_local
    if (present(a_T)) a_T = nsum*a_T_local
    if (present(a_TT)) a_TT = nsum*a_TT
    if (present(a_Vn)) a_Vn = nsum*a_Vn + a_V_local
    if (present(a_V)) a_V = nsum*a_V_local
    if (present(a_VV)) a_VV = nsum*a_VV
    if (present(a_TV)) a_TV = nsum*a_TV

  end subroutine calc_LJs_WCA_ai_TVN

  !> Calculate the a1-a4 for LJ/s
  !!
  !! \author Morten Hammer, April 2021
  subroutine calc_LJs_WCA_ai(i,eps_divk,y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    ! Input
    integer, intent(in) :: i !< Calculate a_i
    real, intent(in) :: eps_divk !< Well depth div. Boltzmann constant
    real, intent(in) :: y !< Unity minus reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_y,a_rr,a_yy,a_ry
    ! Locals
    real :: fac
    select case(i)
    case(1)
      fac = 2*pi*eps_divk
      call calc_LJs_WCA_a1(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    case(2)
      fac = eps_divk**2
      call calc_LJs_WCA_a2(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    case(3)
      fac = eps_divk**3
      call calc_LJs_WCA_a3(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    case(4)
      fac = eps_divk**4
      call calc_LJs_WCA_a4(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    end select
    a = fac*a
    a_y = fac*a_y
    a_r = fac*a_r
    a_yy = fac*a_yy
    a_rr = fac*a_rr
    a_ry = fac*a_ry
  end subroutine calc_LJs_WCA_ai

  !> Calculate the a1 for LJ/s WCA
  !!
  !! \author Morten Hammer, April 2021
  subroutine calc_LJs_WCA_a1(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    ! Input
    real, intent(in) :: y !< Unity minus reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_y,a_rr,a_yy,a_ry
    ! Locals
    real, parameter :: c(12) = (/ &
         -4.359342008565890E-01, -1.010684675331760, &
         5.545568513310810E-01, -4.948459993882990E-01, &
         -2.175255239422550E-02, 2.784218910493600, &
         -9.960315909054440E-01, 7.619772900616760, &
         -1.399502159877310E+01, 8.957336641810310E-01, &
         -5.449504201398070, 8.809619552329060 /)
    real :: p(4,0:2), yv(3,0:2), rhov(4,0:2)
    integer :: j

    yv = 0
    yv(1,0) = 1
    yv(2,0) = y
    yv(3,0) = y**2
    yv(2,1) = 1
    yv(3,1) = 2*y
    yv(3,2) = 2

    rhov(1,0) = rho
    rhov(1,1) = 1
    rhov(1,2) = 0
    rhov(2,2) = 2
    do j=2,4
      rhov(j,0) = rhov(j-1,0)*rho
      rhov(j,1) = j*rhov(j-1,0)
      if (j > 2) rhov(j,2) = j*(j-1)*rhov(j-2,0)
    enddo
    do j=0,2
      p(1,j) = sum(c(1:3)*yv(:,j))
      p(2,j) = sum(c(4:6)*yv(:,j))
      p(3,j) = sum(c(7:9)*yv(:,j))
      p(4,j) = sum(c(10:12)*yv(:,j))
    enddo

    a = 0
    a_y = 0
    a_r = 0
    a_yy = 0
    a_rr = 0
    a_ry = 0
    do j=1,4
      a = a + p(j,0)*rhov(j,0)
      a_y = a_y + p(j,1)*rhov(j,0)
      a_r = a_r + p(j,0)*rhov(j,1)
      a_yy = a_yy + p(j,2)*rhov(j,0)
      a_rr = a_rr + p(j,0)*rhov(j,2)
      a_ry = a_ry + p(j,1)*rhov(j,1)
    enddo
  end subroutine calc_LJs_WCA_a1

  !> Calculate the a2 for LJ/s WCA
  !!
  !! \author Morten Hammer, April 2021
  subroutine calc_LJs_WCA_a2(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    ! Input
    real, intent(in) :: y !< Unity minus reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_y,a_rr,a_yy,a_ry
    ! Locals
    real, parameter :: c(14) = (/ &
         -8.549712604490140E-01, -3.309657322845890, &
         2.150262062951070, 1.819290452429280, &
         4.881241094741120, -1.266600852072320E+01, &
         2.720757103054650, -2.012625560669800E+01, &
         3.826002401416580E+01, 2.528308672696750, &
         4.436429616364860,-2.714948088128380E+01, &
         4.413607271085820E-02, 9.921300491978420E-01 /)
    real :: p(4,0:2), yv(3,0:2), u(0:2,0:2), g(0:2,0:2)
    real :: log_rho, rho_p4(0:3), tanhu(0:2), e2u
    integer :: j

    yv = 0
    yv(1,0) = 1
    yv(2,0) = y
    yv(3,0) = y**2
    yv(2,1) = 1
    yv(3,1) = 2*y
    yv(3,2) = 2

    do j=0,2
      p(1,j) = sum(c(1:3)*yv(:,j))
      p(2,j) = sum(c(4:6)*yv(:,j))
      p(3,j) = sum(c(7:9)*yv(:,j))
      p(4,j) = sum(c(10:12)*yv(:,j))
    enddo

    log_rho = log(rho)
    rho_p4(0) = rho**p(4,0)
    rho_p4(1) = rho**p(4,0)*p(4,1)*log_rho
    rho_p4(2) = rho**p(4,0)*p(4,2)*log_rho + rho**p(4,0)*p(4,1)**2*log_rho**2
    ! 3: t,rho differential
    rho_p4(3) = p(4,1)*rho**(p(4,0)-1)*(p(4,0)*log_rho + 1)
    u(2,2) = 0
    u(1,2) = 0
    u(2,1) = 0
    u(0,0) = p(2,0)*(rho**c(14) + p(3,0)*rho_p4(0))
    u(0,1) = p(2,0)*(c(14)*rho**(c(14)-1) + p(3,0)*p(4,0)*rho**(p(4,0)-1))
    u(0,2) = p(2,0)*(c(14)*(c(14)-1)*rho**(c(14)-2) + &
         p(3,0)*p(4,0)*(p(4,0)-1)*rho**(p(4,0)-2))
    u(1,0) = p(2,1)*(rho**c(14) + p(3,0)*rho_p4(0)) + &
         p(2,0)*(p(3,1)*rho_p4(0) + p(3,0)*rho_p4(1))
    u(2,0) = p(2,2)*(rho**c(14) + p(3,0)*rho_p4(0)) + &
         2*p(2,1)*(p(3,1)*rho_p4(0) + p(3,0)*rho_p4(1)) + &
         p(2,0)*(p(3,2)*rho_p4(0) + 2*p(3,1)*rho_p4(1) + p(3,0)*rho_p4(2))
    u(1,1) = p(2,1)*(c(14)*rho**(c(14)-1) + p(3,0)*p(4,0)*rho**(p(4,0)-1)) + &
         p(2,0)*(p(3,1)*p(4,0)*rho**(p(4,0)-1) + p(3,0)*rho_p4(3))
    !
    e2u = exp(2*u(0,0))
    tanhu(0) = (e2u-1)/(e2u+1)
    tanhu(1) = 1 - tanhu(0)**2
    tanhu(2) = -2*tanhu(0)*tanhu(1)
    !
    g = 0
    g(0,0) = -rho*(p(1,0) + c(13))
    g(0,1) = -p(1,0) - c(13)
    g(1,0) = -rho*p(1,1)
    g(2,0) = -rho*p(1,2)
    g(1,1) = -p(1,1)
    !
    a = p(1,0)*rho + g(0,0)*tanhu(0)
    a_y = p(1,1)*rho + g(1,0)*tanhu(0) + g(0,0)*tanhu(1)*u(1,0)
    a_r = p(1,0) + g(0,1)*tanhu(0) + g(0,0)*tanhu(1)*u(0,1)
    a_yy = p(1,2)*rho + g(2,0)*tanhu(0) + 2*g(1,0)*tanhu(1)*u(1,0) +&
         g(0,0)*tanhu(1)*u(2,0) + g(0,0)*tanhu(2)*u(1,0)**2
    a_rr = 2*g(0,1)*tanhu(1)*u(0,1) + g(0,0)*tanhu(1)*u(0,2) + g(0,0)*tanhu(2)*u(0,1)**2
    a_ry = p(1,1) + g(1,1)*tanhu(0) + g(1,0)*tanhu(1)*u(0,1) + &
         g(0,1)*tanhu(1)*u(1,0) + g(0,0)*tanhu(2)*u(1,0)*u(0,1) + &
         g(0,0)*tanhu(1)*u(1,1)
  end subroutine calc_LJs_WCA_a2

  !> Calculate the a3 for LJ/s WCA
  !!
  !! \author Morten Hammer, April 2021
  subroutine calc_LJs_WCA_a3(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    ! Input
    real, intent(in) :: y !< Unity minus reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_y,a_rr,a_yy,a_ry
    ! Locals
    real, parameter :: c(13) = (/ &
         -2.299701237101790E-01, -9.378822827801790E-01, &
         2.598656446923390E-01, -2.508338793969640, &
         4.339750952753030, -1.048143274408200E+01, &
         4.115203492416100E+01, -3.548798441767570E+01, &
         6.040169542924050, -9.971355447734860, &
         1.004780979021150, 7.443292068847610E-01, &
         3.431138067711600E-01 /)
    real :: yv(3,0:2), p(5,0:2), r(0:2,0:2), g(0:2,0:2)
    real :: log_rho, tanhr(0:2), e2r, rho_p(4:5,0:3)
    integer :: j, k

    yv = 0
    yv(1,0) = 1
    yv(2,0) = y
    yv(3,0) = y**2
    yv(2,1) = 1
    yv(3,1) = 2*y
    yv(3,2) = 2

    do j=0,2
      p(1,j) = sum(c(1:3)*yv(:,j))
      p(3,j) = sum(c(4:5)*yv(1:2,j))
      p(2,j) = sum(c(6:8)*yv(:,j))
      p(4,j) = sum(c(9:10)*yv(1:2,j))
      p(5,j) = sum(c(12:13)*yv(1:2,j))
    enddo

    log_rho = log(rho)
    do j=4,5
      rho_p(j,0) = rho**p(j,0)
      rho_p(j, 1) = rho**p(j,0)*p(j,1)*log_rho
      rho_p(j, 2) = rho**p(j,0)*p(j,2)*log_rho + rho**p(j,0)*p(j,1)**2*log_rho**2
      ! 3: t,rho differential
      rho_p(j, 3) = p(j,1)*rho**(p(j,0)-1)*(p(j,0)*log_rho + 1)
    enddo
    r = 0
    do j=4,5
      k = j - 2
      r(0,0) = r(0,0) + p(k,0)*rho_p(j,0)
      r(0,1) = r(0,1) + p(k,0)*p(j,0)*rho**(p(j,0)-1)
      r(0,2) = r(0,2) + p(k,0)*p(j,0)*(p(j,0)-1)*rho**(p(j,0)-2)
      r(1,0) = r(1,0) + p(k,1)*rho_p(j,0) + p(k,0)*rho_p(j,1)
      r(2,0) = r(2,0) + 2*p(k,1)*rho_p(j,1) + p(k,2)*rho_p(j,0) + &
           p(k,0)*rho_p(j,2)
      r(1,1) = r(1,1) + p(k,1)*p(j,0)*rho**(p(j,0)-1) + &
           + p(k,0)*p(j,1)*rho**(p(j,0)-1) + p(k,0)*p(j,0)*rho_p(j,1)/rho
    enddo
    !
    e2r = exp(2*r(0,0))
    tanhr(0) = (e2r-1)/(e2r+1)
    tanhr(1) = (1 - tanhr(0)**2)
    tanhr(2) = -2*tanhr(0)*tanhr(1)
    tanhr(0) = 1 + c(11)*tanhr(0)
    tanhr(1) = c(11)*tanhr(1)
    tanhr(2) = c(11)*tanhr(2)
    !
    g = 0
    g(0,0) = rho*p(1,0)
    g(0,1) = p(1,0)
    g(1,0) = rho*p(1,1)
    g(2,0) = rho*p(1,2)
    g(1,1) = p(1,1)
    !
    a = g(0,0)*tanhr(0)
    a_y = g(1,0)*tanhr(0) + g(0,0)*tanhr(1)*r(1,0)
    a_r = g(0,1)*tanhr(0) + g(0,0)*tanhr(1)*r(0,1)
    a_yy = g(2,0)*tanhr(0) + 2*g(1,0)*tanhr(1)*r(1,0) +&
         g(0,0)*tanhr(1)*r(2,0) + g(0,0)*tanhr(2)*r(1,0)**2
    a_rr = 2*g(0,1)*tanhr(1)*r(0,1) + g(0,0)*tanhr(1)*r(0,2) + g(0,0)*tanhr(2)*r(0,1)**2
    a_ry = g(1,1)*tanhr(0) + g(1,0)*tanhr(1)*r(0,1) + &
         g(0,1)*tanhr(1)*r(1,0) + g(0,0)*tanhr(2)*r(1,0)*r(0,1) + &
         g(0,0)*tanhr(1)*r(1,1)
  end subroutine calc_LJs_WCA_a3

  !> Calculate the a4 for LJ/s WCA
  !!
  !! \author Morten Hammer, April 2021
  subroutine calc_LJs_WCA_a4(y,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
    ! Input
    real, intent(in) :: y !< Unity minus reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_y,a_rr,a_yy,a_ry
    ! Locals
    real, parameter :: c(14) = (/ &
         -3.887755601943300E-03, 7.088380007122160E-01, &
         -5.061184486003090, -5.601386794807250E-02, &
         -1.855154519243070E-01, 2.148962864405380E-01, &
         6.920413400878820E-02, -6.212126555148940E-01, &
         5.101710025798300, 6.495708980657240, &
         -2.494763026982940E+01, 3.710757600727180E+01, &
         3.683587731520040E-01, 1.903500562425810 /)
    real :: yv(3,0:2), p(4,0:2), k(0:2,0:2), g(0:2,0:1)
    real :: tanhk(0:2), e2k
    integer :: j

    yv = 0
    yv(1,0) = 1
    yv(2,0) = y
    yv(3,0) = y**2
    yv(2,1) = 1
    yv(3,1) = 2*y
    yv(3,2) = 2

    do j=0,2
      p(1,j) = sum(c(1:3)*yv(:,j))
      p(2,j) = sum(c(4:6)*yv(:,j))
      p(3,j) = sum(c(7:9)*yv(:,j))
      p(4,j) = sum(c(10:12)*yv(:,j))
    enddo

    k = 0
    k(0,0) = p(4,0)*rho**c(14)
    k(0,1) = c(14)*p(4,0)*rho**(c(14)-1)
    k(0,2) = (c(14)-1)*c(14)*p(4,0)*rho**(c(14)-2)
    k(1,0) = p(4,1)*rho**c(14)
    k(2,0) = p(4,2)*rho**c(14)
    k(1,1) = c(14)*p(4,1)*rho**(c(14)-1)
    !
    e2k = exp(2*k(0,0))
    tanhk(0) = (e2k-1)/(e2k+1)
    tanhk(1) = (1 - tanhk(0)**2)
    tanhk(2) = -2*tanhk(0)*tanhk(1)
    !
    g = 0
    g(0,0) = rho*p(3,0)
    g(0,1) = p(3,0)
    g(1,0) = rho*p(3,1)
    g(1,1) = p(3,1)
    g(2,0) = rho*p(3,2)
    !
    a = p(1,0)*rho**(c(13)+1) + p(2,0)*rho
    a_y = p(1,1)*rho**(c(13)+1) + p(2,1)*rho
    a_yy = p(1,2)*rho**(c(13)+1) + p(2,2)*rho
    a_r = (c(13)+1)*p(1,0)*rho**c(13) + p(2,0)
    a_rr = c(13)*(c(13)+1)*p(1,0)*rho**(c(13)-1)
    a_ry = (c(13)+1)*p(1,1)*rho**c(13) + p(2,1)
    !
    a = a + g(0,0)*tanhk(0)
    a_y = a_y + g(1,0)*tanhk(0) + g(0,0)*tanhk(1)*k(1,0)
    a_r = a_r + g(0,1)*tanhk(0) + g(0,0)*tanhk(1)*k(0,1)
    a_yy = a_yy  + g(2,0)*tanhk(0) + 2*g(1,0)*tanhk(1)*k(1,0) &
         + g(0,0)*tanhk(2)*k(1,0)**2 + g(0,0)*tanhk(1)*k(2,0)
    a_rr = a_rr + 2*g(0,1)*tanhk(1)*k(0,1) + g(0,0)*tanhk(1)*k(0,2) + g(0,0)*tanhk(2)*k(0,1)**2
    a_ry = a_ry + g(1,1)*tanhk(0) + g(1,0)*tanhk(1)*k(0,1) + &
         g(0,1)*tanhk(1)*k(1,0) + g(0,0)*tanhk(2)*k(1,0)*k(0,1) + &
         g(0,0)*tanhk(1)*k(1,1)
  end subroutine calc_LJs_WCA_a4

  !> Calculate the a1-a4 for LJ/s
  !!
  !! \author Morten Hammer, June 2021
  subroutine calc_LJs_WCA_ai_tr(i,T_star,rho_star,ai)
    ! Input
    integer, intent(in) :: i !< Calculate a_i
    real, intent(in) :: T_star !< Reduced temperature
    real, intent(in) :: rho_star !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: ai
    ! Locals
    real :: y, x0
    real :: fac, a_r,a_y,a_rr,a_yy,a_ry
    !
    x0 = (2.0/(1 + sqrt(t_star)))**(real(1)/6)
    y = 2.0**(real(1)/6) - x0
    !
    fac = 1
    select case(i)
    case(1)
      fac = 2.0*atan(1.0)
      call calc_LJs_WCA_a1(y,rho_star,ai,a_r,a_y,a_rr,a_yy,a_ry)
    case(2)
      call calc_LJs_WCA_a2(y,rho_star,ai,a_r,a_y,a_rr,a_yy,a_ry)
    case(3)
      call calc_LJs_WCA_a3(y,rho_star,ai,a_r,a_y,a_rr,a_yy,a_ry)
    case(4)
      call calc_LJs_WCA_a4(y,rho_star,ai,a_r,a_y,a_rr,a_yy,a_ry)
    end select
    ai = fac*ai
  end subroutine calc_LJs_WCA_ai_tr

  !###################################
  !> ljx_ux parameters
  function ljx_ux_eos_constructor(eos_label) result(p_eos)
    ! Input:
    character(len=*), intent(in) :: eos_label
    ! Created object:
    type(ljx_ux_eos) :: p_eos
    !
    p_eos%lj_potential = (INDEX("LJS",eos_label) <= 0)
    p_eos%is_uf_theory = (INDEX("UF",eos_label) > 0)
  end function ljx_ux_eos_constructor

  subroutine assign_ljx_ux_eos(this, other)
    class(ljx_ux_eos), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (ljx_ux_eos)
      call this%ljs_wca_eos%assign_base_eos_param(other)
      ! Model control
      this%lj_potential = other%lj_potential
      this%is_uf_theory = other%is_uf_theory
      this%enable_virial_term = other%enable_virial_term
      this%use_temperature_dependent_u_fraction = other%use_temperature_dependent_u_fraction
    class default
    end select
  end subroutine assign_ljx_ux_eos

  subroutine ljs_uv_model_control(use_temperature_dependent_u_fraction)
    use thermopack_var, only: base_eos_param, thermo_model, get_active_thermo_model
    logical, intent(in) :: use_temperature_dependent_u_fraction
    ! Locals
    class(base_eos_param), pointer :: eos
    type(thermo_model), pointer :: p_eos_cont
    integer :: i
    p_eos_cont => get_active_thermo_model()
    if (allocated(p_eos_cont%eos)) then
      do i=1,size(p_eos_cont%eos)
        if (associated(p_eos_cont%eos(i)%p_eos)) then
          eos => p_eos_cont%eos(i)%p_eos
          select type( p_eos => eos )
          class is ( ljx_ux_eos )
            p_eos%use_temperature_dependent_u_fraction = use_temperature_dependent_u_fraction
          end select
        else
           print *,"ljs_uv_model_control: eos not acociated"
         endif
       enddo
     else
       print *,"ljs_uv_model_control: eos array not allocted found"
     endif
   end subroutine ljs_uv_model_control

  !> Calculate WCA a1 for LJs. U-expansion.
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_u_a1_LJs_ex(rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real, parameter :: param(5,4) = reshape(&
         (/-4.19307460e-01, 2.63021608e+01, 6.14703510e+01, -2.82732512,  &
         6.09001685e-01, &
         2.00550080e+00, -1.04365573e-01, 5.73140940e-01, -1.61216849, &
         5.06954476e+01, &
         -2.13577726e+00, -2.66579307e+01, -6.23488657e+01, 4.93092625, &
         -5.14171759e+01, &
         4.88149448e-01, -6.41433360e-03, 7.42870835e-03,  2.23270821e-01, &
         4.95777156e-01 /), (/5,4/))

    call calc_WCA_u_a1_LJ(param,rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)

  end subroutine calc_u_a1_LJs_ex

  !> Calculate WCA a1 for LJ. U-expansion.
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_u_a1_LJ_ex(rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real, parameter :: param(5,4) = reshape(&
         (/-0.37447216,-0.01631296,-0.11968918, 0.42113581,-0.13660913, &
         -0.00081593, 0.0173549,  0.06900995,-0.11298284, 0.02263133, &
         -0.53140336,-0.48321813,-0.09297811, 0.06698083, 0.00933081, &
         0.06813345,-0.10811097,-0.05479239, 0.13611539, 0.21970435 /), (/5,4/))

    call calc_WCA_u_a1_LJ(param,rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)

  end subroutine calc_u_a1_LJ_ex

  !> Calculate WCA a1 for LJs/LJs. U-expansion.
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_WCA_u_a1_LJ(param,rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: param(5,4)
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real :: ai(5),ai_t(5),ai_tt(5)
    integer :: i
    real :: rhoa(5)
    real :: rhoa_r(5)
    real :: rhoa_rr(5)
    call calc_ai(param, T, ai, ai_T, ai_TT)
    rhoa(1) = rho
    rhoa_r(1) = 1
    rhoa_rr(1) = 0
    do i=2,5
      rhoa(i) = rhoa(i-1)*rho
      rhoa_r(i) = i*rhoa(i-1)
      rhoa_rr(i) = i*rhoa_r(i-1)
    enddo
    a = 2*pi*sum(ai*rhoa)
    a_r = 2*pi*sum(ai*rhoa_r)
    a_t = 2*pi*sum(ai_t*rhoa)
    a_rr = 2*pi*sum(ai*rhoa_rr)
    a_tt = 2*pi*sum(ai_tt*rhoa)
    a_rt = 2*pi*sum(ai_t*rhoa_r)

  contains
    subroutine calc_ai(param, T, ai, ai_T, ai_TT)
      real, intent(in) :: param(5,4)
      real, intent(in) :: T !< Reduced temperature
      ! Output
      real, intent(out) :: ai(5),ai_t(5),ai_tt(5)
      ! Locals
      real :: sqrt_T
      sqrt_T = sqrt(T)
      do i=1,5
        ai(i) = param(i,1) + param(i,2)*sqrt_t &
             + param(i,3)*t**param(i,4)
        ai_t(i) = 0.5*param(i,2)/sqrt_t &
             + param(i,3)*param(i,4)*t**(param(i,4)-1.0)
        ai_tt(i) = -0.25*param(i,2)/(sqrt_t*t) &
             + param(i,3)*param(i,4)*(param(i,4)-1.0)*t**(param(i,4)-2.0)
      enddo
    end subroutine calc_ai
  end subroutine calc_WCA_u_a1_LJ

  !> Calculate WCA a1 for LJs. Mayer-f-expansion.
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_mf_a1_LJs_ex(rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real, parameter :: param(5,5) = reshape(&
         (/ -1.38956236e-03, 7.21649732e-01,-4.46006380e-01, 1.02494323e+00,-3.98081890e-01, &
         4.39757272e-02,-6.53666725e-01, 6.21534008e+00,-1.04680007e+01, 4.57504276e+00, &
         -1.77080263e-01, 3.60375455e+00,-2.00895809e+01, 3.64523410e+01,-1.57757874e+01, &
         2.46524435e-01,-5.16905418e+00, 2.80637697e+01,-5.00655126e+01, 2.15679397e+01, &
         -1.14440939e-01, 2.39307542e+00,-1.34547589e+01, 2.34538041e+01,-1.00079543e+01 /), (/5,5/))

    call calc_WCA_mf_a1_LJ(param,rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)

  end subroutine calc_mf_a1_LJs_ex

  !> Calculate WCA a1 for LJ. Mayer-f-expansion.
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_mf_a1_LJ_ex(rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real, parameter :: param(5,5) = reshape(&
         (/ 0.00326607, 0.98177233, 0.09812966, 0.1091711,  0.00246771, &
         -0.00631528, 0.37428833, 0.4494362, -0.10567363, 0.06797539, &
         -0.00134063,-0.0261357,  0.18812166,-0.01328055,-0.0037673, &
         0.00803155,-0.23353006,-0.0407614,  0.04651761, 0.08330469, &
         -0.0041289,  0.11235306,-0.191682,  0.01508,   -0.06683759  /), (/5,5/))

    call calc_WCA_mf_a1_LJ(param,rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)

  end subroutine calc_mf_a1_LJ_ex

  !> Calculate WCA a1 for LJs/LJs. Mayer-f-expansion.
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_WCA_mf_a1_LJ(param,rho,T,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: param(5,5)
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    integer :: i, j
    real :: rhoa(5)
    real :: rhoa_r(5)
    real :: rhoa_rr(5)
    real :: beta(5), b
    real :: beta_t(5)
    real :: beta_tt(5)

    rhoa(1) = rho
    rhoa_r(1) = 1
    rhoa_rr(1) = 0
    do i=2,5
      rhoa(i) = rhoa(i-1)*rho
      rhoa_r(i) = i*rhoa(i-1)
      rhoa_rr(i) = i*rhoa_r(i-1)
    enddo
    b = 1.0/t
    beta(1) = t
    beta(2) = 1
    beta_t(1) = 1
    beta_t(2) = 0
    beta_tt(1) = 0
    beta_tt(2) = 0
    do i=3,5
      beta(i) = beta(i-1)*b
      beta_t(i) = -(i-2)*beta(i)*b
      beta_tt(i) = -(i-1)*beta_t(i)*b
    enddo

    a = 0
    a_r = 0
    a_t = 0
    a_rr = 0
    a_tt = 0
    a_rt = 0
    do i=1,5
      do j=1,5
        a = a + param(i, j)*beta(i)*rhoa(j)
        a_r = a_r + param(i, j)*beta(i)*rhoa_r(j)
        a_rr = a_rr + param(i, j)*beta(i)*rhoa_rr(j)
        a_rt = a_rt + param(i, j)*beta_t(i)*rhoa_r(j)
        a_t = a_t + param(i, j)*beta_t(i)*rhoa(j)
        a_tt = a_tt + param(i, j)*beta_tt(i)*rhoa(j)
      enddo
    enddo
    a = -2*pi*a
    a_r = -2*pi*a_r
    a_t = -2*pi*a_t
    a_rr = -2*pi*a_rr
    a_tt = -2*pi*a_tt
    a_rt = -2*pi*a_rt
  end subroutine calc_WCA_mf_a1_LJ

  !> Calculate phi-u for uf-theory of WCA LJs
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_WCA_phiu_LJs(rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt
    ! Locals
    real, parameter :: param(2) = (/2.47, 1.4/)
    !
    call calc_WCA_phiu_2param(param,rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
  end subroutine calc_WCA_phiu_LJs

  !> Calculate phi-u for uf-theory of WCA LJ
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_WCA_phiu_LJ(rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt
    ! Locals
    real, parameter :: param(2) = (/2.215, 2.033/)
    !
    call calc_WCA_phiu_2param(param,rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
  end subroutine calc_WCA_phiu_LJ

  !> Calculate phi-u for uf-theory of WCA using 2-parameter
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_WCA_phiu_2param(param,rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
    ! Input
    real, intent(in) :: param(2)
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    ! Output
    real, intent(out) :: phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt
    ! Locals
    real :: a, b
    real :: e2x, x, dxdr, d2xdr2, denum
    !
    a = param(1)
    b = param(2)
    x = a*rho + b*rho**2
    dxdr = a + 2*b*rho
    d2xdr2 = 2*b
    e2x = exp(2*x)
    phi_u = (e2x-1)/(e2x+1)
    phi_u_t = 0
    phi_u_tt = 0
    phi_u_rt = 0
    denum = (e2x+1)**2
    phi_u_r = 4*e2x*dxdr/denum
    phi_u_rr = 2*phi_u_r*dxdr + 4*e2x*d2xdr2/denum &
        - 16*e2x*e2x*dxdr**2/(denum*(e2x+1))
  end subroutine calc_WCA_phiu_2param

  !> Calculate uf-theory a1 of WCA LJ
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_uf_WCA(rho,T,is_lj,a,a_r,a_t,a_rr,a_tt,a_rt)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    logical, intent(in) :: is_lj !< Is Lennard-Lones fluid
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real :: phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt
    real :: au,au_r,au_t,au_rr,au_tt,au_rt
    real :: amf,amf_r,amf_t,amf_rr,amf_tt,amf_rt
    !
    if (is_LJ) then
      call calc_WCA_phiu_LJ(rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
      call calc_mf_a1_LJ_ex(rho,T,amf,amf_r,amf_t,amf_rr,amf_tt,amf_rt)
      call calc_u_a1_LJ_ex(rho,T,au,au_r,au_t,au_rr,au_tt,au_rt)
    else
      call calc_WCA_phiu_LJs(rho,T,phi_u,phi_u_r,phi_u_t,phi_u_rr,phi_u_tt,phi_u_rt)
      call calc_mf_a1_LJs_ex(rho,T,amf,amf_r,amf_t,amf_rr,amf_tt,amf_rt)
      call calc_u_a1_LJs_ex(rho,T,au,au_r,au_t,au_rr,au_tt,au_rt)
    endif
    !
    ! phi_u = 1
    ! phi_u_r = 0
    ! phi_u_t = 0
    ! phi_u_rr = 0
    ! phi_u_tt = 0
    ! phi_u_rt = 0
    !
    a = phi_u*au + (1-phi_u)*amf
    a_r = phi_u*au_r + (1-phi_u)*amf_r + phi_u_r*(au - amf)
    a_rr = phi_u_r*au_r + phi_u*au_rr &
         - phi_u_r*amf_r + (1-phi_u)*amf_rr &
         + phi_u_r*(au_r - amf_r) + phi_u_rr*(au - amf)
    ! No temperature dependdence in phi_u assumed
    a_t = phi_u*au_t + (1-phi_u)*amf_t
    a_tt = phi_u*au_tt + (1-phi_u)*amf_tt
    a_rt = phi_u*au_rt + (1-phi_u)*amf_rt + phi_u_r*(au_t - amf_t)
  end subroutine calc_uf_WCA


  !> Calculate uf-theory a1 of WCA LJ
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_uf_WCA_TVN(eos,nc,sigma,eps_divk,T,V,n,a,a_t,a_V,a_n,a_tt,&
       a_vv,a_tv,a_tn,a_vn,a_nn)
    ! Input
    class(ljx_ux_eos), intent(in) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: sigma, eps_divk
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: V !< Volume (m3)
    real, intent(in) :: n(nc) !< Mol number (mol)
    ! Output
    real, optional, intent(out) :: a,a_V,a_t,a_n(nc),a_vv,a_tt,a_tv
    real, optional, intent(out) :: a_nn(nc,nc),a_tn(nc),a_vn(nc)
    ! Locals
    real :: ar,arr,art, att
    real :: al, at, av, an(nc)
    real :: t_star, rho_star, prefac, sumn
    !
    sumn = sum(n)
    prefac = N_AVOGADRO*sigma**3
    rho_star = prefac*sumn/V
    t_star = T/eps_divk
    call calc_uf_WCA(rho_star,t_star,eos%lj_potential,al,ar,at,arr,att,art)
    ! Convert to TVn differentials
    an = al + sumn*ar*prefac/V
    al = al*sumn
    at = at*sumn/eps_divk
    av = -sumn*ar*prefac*sumn/V**2
    !
    if (present(a)) a = eps_divk*al
    if (present(a_t)) a_t = eps_divk*at
    if (present(a_tt)) a_tt = eps_divk*att*sumn/eps_divk**2
    !
    if (present(a_n)) a_n = eps_divk*an
    if (present(a_nn)) a_nn(1,1) = eps_divk*(2*ar*prefac/V + n(1)*arr*(prefac/V)**2)
    if (present(a_tn)) a_tn = eps_divk*(at/sumn + n*art*prefac/V/eps_divk)
    !
    if (present(a_v)) a_v = eps_divk*av
    if (present(a_tv)) a_tv = eps_divk*(-sumn*art*prefac*sumn/V**2/eps_divk)
    if (present(a_vv)) a_vv = eps_divk*(-2*av/V + sumn*arr*(prefac*sumn/V**2)**2)
    if (present(a_vn)) a_vn = eps_divk*(2*av/sumn - sumn*arr*prefac**2*sumn/V**3)
  end subroutine calc_uf_WCA_TVN

  !> Calculate residual reduced Helmholts free energy
  !!
  !! \author Morten Hammer, Noember 2020
  subroutine calcFresLJ_uf_theory(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_hardsphere, only: calc_pure_ahs_div_nRT
    ! Input
    class(ljx_ux_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    real :: beta,nsum
    real :: Fhs,Fhs_T,Fhs_V,Fhs_TT,Fhs_VV,Fhs_TV
    real, dimension(nc) :: Fhs_n,Fhs_Tn,Fhs_Vn
    real, dimension(nc,nc) :: Fhs_nn
    real :: FhsC,FhsC_T,FhsC_V,FhsC_TT,FhsC_VV,FhsC_TV
    real, dimension(nc) :: FhsC_n,FhsC_Tn,FhsC_Vn
    real, dimension(nc,nc) :: FhsC_nn
    real :: F1,F1_T,F1_V,F1_TT,F1_VV,F1_TV
    real, dimension(nc) :: F1_n,F1_Tn,F1_Vn
    real, dimension(nc,nc) :: F1_nn
    real :: sigma, eps_divk

    sigma = eos%sigma
    eps_divk = eos%eps_divk
    ! Calculate hard-sphere term
    if (eos%enable_hs) then
      ! The hard-sphere diameter
      call calc_dhs_WCA(nc,sigma,eps_divk,T,eos%dhs)
      ! Packing fraction
      call calcZetaX_vdW_no_segments(nc,T,V,n,eos%dhs,eos%eta_hs)
      ! Get pure fluid Helmholtz energy
      call calc_pure_ahs_div_nRT(nc,eos%eta_hs,a=Fhs,a_T=Fhs_T,a_V=Fhs_V,a_n=Fhs_n,&
           a_TT=Fhs_TT,a_TV=Fhs_TV,a_Tn=Fhs_Tn,a_VV=Fhs_VV,a_Vn=Fhs_Vn,a_nn=Fhs_nn)
      nsum = sum(n)
      Fhs_nn(1,1) = nsum*Fhs_nn(1,1) + 2*Fhs_n(1)
      Fhs_n = nsum*Fhs_n + Fhs
      Fhs = nsum*Fhs
      Fhs_Tn = nsum*Fhs_Tn + Fhs_T
      Fhs_T = nsum*Fhs_T
      Fhs_TT = nsum*Fhs_TT
      Fhs_Vn = nsum*Fhs_Vn + Fhs_V
      Fhs_V = nsum*Fhs_V
      Fhs_VV = nsum*Fhs_VV
      Fhs_TV = nsum*Fhs_TV
      !
      if (eos%enable_cavity) then
        call calc_cavity_integral_LJ_Fres(nc,sigma,eps_divk,eos%dhs,eos%eta_hs,T,V,n,&
             FhsC,FhsC_T,FhsC_V,FhsC_n,FhsC_TT,FhsC_VV,FhsC_TV,FhsC_Tn,FhsC_Vn,&
             FhsC_nn)
      else
        FhsC = 0
        FhsC_T = 0
        FhsC_V = 0
        FhsC_TT = 0
        FhsC_VV = 0
        FhsC_TV = 0
        FhsC_n = 0
        FhsC_Tn = 0
        FhsC_Vn = 0
        FhsC_nn = 0
      endif
    else
      Fhs = 0.0
      Fhs_T = 0.0
      Fhs_V = 0.0
      Fhs_TT = 0.0
      Fhs_VV = 0.0
      Fhs_TV = 0.0
      Fhs_n = 0.0
      Fhs_Tn = 0.0
      Fhs_Vn = 0.0
      Fhs_nn = 0.0
      !
      FhsC = 0.0
      FhsC_T = 0.0
      FhsC_V = 0.0
      FhsC_TT = 0.0
      FhsC_VV = 0.0
      FhsC_TV = 0.0
      FhsC_n = 0.0
      FhsC_Tn = 0.0
      FhsC_Vn = 0.0
      FhsC_nn = 0.0
    endif

    ! Calculate first order term
    if (eos%enable_A1) then
      call calc_uf_WCA_TVN(eos,nc,sigma,eps_divk,T,V,n,a=F1,a_t=F1_T,a_V=F1_V,a_n=F1_n,&
           a_tt=F1_TT,a_vv=F1_VV,a_tv=F1_TV,a_tn=F1_Tn,a_vn=F1_Vn,a_nn=F1_nn)
    else
      F1 = 0.0
      F1_T = 0.0
      F1_V = 0.0
      F1_TT = 0.0
      F1_VV = 0.0
      F1_TV = 0.0
      F1_n = 0.0
      F1_Tn = 0.0
      F1_Vn = 0.0
      F1_nn = 0.0
    endif

    ! F = A/(RT)
    beta = 1.0/T
    F = Fhs + FhsC + beta*F1
    if (present(F_T)) then
      F_T = Fhs_T + FhsC_T + beta*F1_T -beta*F1/T
    endif
    if (present(F_V)) then
      F_V = Fhs_V + FhsC_V + beta*F1_V
    endif
    if (present(F_TT)) then
      F_TT = Fhs_TT + FhsC_TT + beta*F1_TT + 2.0*beta*F1/T**2 - 2.0*beta*F1_T/T
    endif
    if (present(F_VV)) then
      F_VV = Fhs_VV + FhsC_VV + beta*F1_VV
    endif
    if (present(F_TV)) then
      F_TV = Fhs_TV + FhsC_TV + beta*F1_TV - beta*F1_V/T
    endif
    if (present(F_Tn)) then
      F_Tn = Fhs_Tn + FhsC_Tn + beta*F1_Tn - beta*F1_n/T
    endif
    if (present(F_Vn)) then
      F_Vn = Fhs_Vn + FhsC_Vn + beta*F1_Vn
    endif
    if (present(F_n)) then
      F_n = Fhs_n + FhsC_n + beta*F1_n
    endif
    if (present(F_nn)) then
      F_nn = Fhs_nn + FhsC_nn + beta*F1_nn
    endif

  end subroutine calcFresLJ_uf_theory

  !> Calculate residual reduced Helmholts free energy
  !!
  !! \author Morten Hammer, March 2021
  subroutine calcFresLJs_uv_theory(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_hardsphere, only: calc_pure_ahs_div_nRT
    ! Input
    class(ljx_ux_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    real :: nsum
    real :: Fhs,Fhs_T,Fhs_V,Fhs_TT,Fhs_VV,Fhs_TV
    real, dimension(nc) :: Fhs_n,Fhs_Tn,Fhs_Vn
    real, dimension(nc,nc) :: Fhs_nn
    real :: FhsC,FhsC_T,FhsC_V,FhsC_TT,FhsC_VV,FhsC_TV
    real, dimension(nc) :: FhsC_n,FhsC_Tn,FhsC_Vn
    real, dimension(nc,nc) :: FhsC_nn
    real :: F1,F1_T,F1_V,F1_TT,F1_VV,F1_TV
    real, dimension(nc) :: F1_n,F1_Tn,F1_Vn
    real, dimension(nc,nc) :: F1_nn
    real :: sigma, eps_divk

    sigma = eos%sigma
    eps_divk = eos%eps_divk
    ! The hard-sphere diameter
    call calc_dhs_WCA(nc,sigma,eps_divk,T,eos%dhs)
    ! Calculate hard-sphere term
    if (eos%enable_hs) then
      ! Packing fraction
      call calcZetaX_vdW_no_segments(nc,T,V,n,eos%dhs,eos%eta_hs)
      ! Get pure fluid Helmholtz energy
      call calc_pure_ahs_div_nRT(nc,eos%eta_hs,a=Fhs,a_T=Fhs_T,a_V=Fhs_V,a_n=Fhs_n,&
           a_TT=Fhs_TT,a_TV=Fhs_TV,a_Tn=Fhs_Tn,a_VV=Fhs_VV,a_Vn=Fhs_Vn,a_nn=Fhs_nn)
      nsum = sum(n)
      Fhs_nn(1,1) = nsum*Fhs_nn(1,1) + 2*Fhs_n(1)
      Fhs_n = nsum*Fhs_n + Fhs
      Fhs = nsum*Fhs
      Fhs_Tn = nsum*Fhs_Tn + Fhs_T
      Fhs_T = nsum*Fhs_T
      Fhs_TT = nsum*Fhs_TT
      Fhs_Vn = nsum*Fhs_Vn + Fhs_V
      Fhs_V = nsum*Fhs_V
      Fhs_VV = nsum*Fhs_VV
      Fhs_TV = nsum*Fhs_TV
      !
      if (eos%enable_cavity) then
        call calc_cavity_integral_LJ_Fres(nc,sigma,eps_divk,eos%dhs,eos%eta_hs,T,V,n,&
             FhsC,FhsC_T,FhsC_V,FhsC_n,FhsC_TT,FhsC_VV,FhsC_TV,FhsC_Tn,FhsC_Vn,&
             FhsC_nn)
      else
        FhsC = 0
        FhsC_T = 0
        FhsC_V = 0
        FhsC_TT = 0
        FhsC_VV = 0
        FhsC_TV = 0
        FhsC_n = 0
        FhsC_Tn = 0
        FhsC_Vn = 0
        FhsC_nn = 0
      endif
    else
      Fhs = 0.0
      Fhs_T = 0.0
      Fhs_V = 0.0
      Fhs_TT = 0.0
      Fhs_VV = 0.0
      Fhs_TV = 0.0
      Fhs_n = 0.0
      Fhs_Tn = 0.0
      Fhs_Vn = 0.0
      Fhs_nn = 0.0
      !
      FhsC = 0.0
      FhsC_T = 0.0
      FhsC_V = 0.0
      FhsC_TT = 0.0
      FhsC_VV = 0.0
      FhsC_TV = 0.0
      FhsC_n = 0.0
      FhsC_Tn = 0.0
      FhsC_Vn = 0.0
      FhsC_nn = 0.0
    endif

    ! Calculate first order term
    if (eos%enable_A1) then
      call calc_uv_WCA_TVN(eos,nc,sigma,eps_divk,T,V,n,eos%dhs,a=F1,a_t=F1_T,a_V=F1_V,a_n=F1_n,&
           a_tt=F1_TT,a_vv=F1_VV,a_tv=F1_TV,a_tn=F1_Tn,a_vn=F1_Vn,a_nn=F1_nn)
    else
      F1 = 0.0
      F1_T = 0.0
      F1_V = 0.0
      F1_TT = 0.0
      F1_VV = 0.0
      F1_TV = 0.0
      F1_n = 0.0
      F1_Tn = 0.0
      F1_Vn = 0.0
      F1_nn = 0.0
    endif

    ! F = A/(RT)
    F = Fhs + FhsC + F1
    if (present(F_T)) then
      F_T = Fhs_T + FhsC_T + F1_T
    endif
    if (present(F_V)) then
      F_V = Fhs_V + FhsC_V + F1_V
    endif
    if (present(F_TT)) then
      F_TT = Fhs_TT + FhsC_TT + F1_TT
    endif
    if (present(F_VV)) then
      F_VV = Fhs_VV + FhsC_VV + F1_VV
    endif
    if (present(F_TV)) then
      F_TV = Fhs_TV + FhsC_TV + F1_TV
    endif
    if (present(F_Tn)) then
      F_Tn = Fhs_Tn + FhsC_Tn + F1_Tn
    endif
    if (present(F_Vn)) then
      F_Vn = Fhs_Vn + FhsC_Vn + F1_Vn
    endif
    if (present(F_n)) then
      F_n = Fhs_n + FhsC_n + F1_n
    endif
    if (present(F_nn)) then
      F_nn = Fhs_nn + FhsC_nn + F1_nn
    endif

  end subroutine calcFresLJs_uv_theory

  !> Calculate uv-theory a1 of WCA LJ
  !!
  !! \author Morten Hammer, 2021-03
  subroutine calc_uv_WCA_TVN(eos,nc,sigma,eps_divk,T,V,n,dhs,a,a_t,a_V,a_n,a_tt,&
       a_vv,a_tv,a_tn,a_vn,a_nn)
    ! Input
    class(ljx_ux_eos), intent(in) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: sigma, eps_divk
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: V !< Volume (m3)
    real, intent(in) :: n(nc) !< Mol number (mol)
    ! Output
    real, optional, intent(out) :: a,a_V,a_t,a_n(nc),a_vv,a_tt,a_tv
    real, optional, intent(out) :: a_nn(nc,nc),a_tn(nc),a_vn(nc)
    ! Locals
    real :: ar, arr, art, att
    real :: al, at, av, an(nc)
    real :: rho_star, prefac, sumn
    !
    sumn = sum(n)
    prefac = N_AVOGADRO*sigma**3
    rho_star = prefac*sumn/V
    call calc_uv_WCA(sigma,eps_divk,rho_star,T,dhs,al,ar,at,arr,att,art,&
         eos%enable_virial_term, eos%use_temperature_dependent_u_fraction)
    ! Convert to TVn differentials (F = n*a)
    an = al + sumn*ar*prefac/V
    al = al*sumn
    at = at*sumn
    av = -sumn*ar*prefac*sumn/V**2
    !
    if (present(a)) a = al
    if (present(a_t)) a_t = at
    if (present(a_tt)) a_tt = att*sumn
    !
    if (present(a_n)) a_n = an
    if (present(a_nn)) a_nn(1,1) = (2*ar*prefac/V + n(1)*arr*(prefac/V)**2)
    if (present(a_tn)) a_tn = (at/sumn + n*art*prefac/V)
    !
    if (present(a_v)) a_v = av
    if (present(a_tv)) a_tv = (-sumn*art*prefac*sumn/V**2)
    if (present(a_vv)) a_vv = (-2*av/V + sumn*arr*(prefac*sumn/V**2)**2)
    if (present(a_vn)) a_vn = (2*av/sumn - sumn*arr*prefac**2*sumn/V**3)
  end subroutine calc_uv_WCA_TVN

  ! Alternative hard sphere diameter
  subroutine uv_q_of_T(T, sigma, eps_divk, q, q_T, q_TT)
    real, intent(in) :: T !
    real, intent(in) :: sigma !
    real, intent(in) :: eps_divk !
    real, intent(out) :: q, q_T, q_TT
    !
    real :: C(0:3)
    integer, parameter :: m = 12, n = 6
    real, parameter :: m_matrix(4) = (/ 1, (m-7), (m-7)**2, (m-7)**3/)
    real :: c_matrix(3,4) = reshape((/&
           1.92840364363978E+00, 5.20120816141761E-01, 0.0, &
           4.43165896265079E-01, 1.82526759234412E-01, 1.29885156087242E-02, &
           0.0, 1.10319989659929E-02, 6.41039871789327E-03, &
           0.0, -7.97813995328348E-05, 1.85866741090323E-05 &
           /), (/3, 4 /))
    real :: rs, sqrt_T, f, f_T, f_TT, ex, Ts
    Ts = T/eps_divk
    rs = 2**(real(1)/real(6))
    C(0) = sqrt(2*pi*m/n)
    C(1:3) =  matmul(c_matrix,m_matrix)
    sqrt_T = sqrt(Ts)
    ex = - real(1)/(2*m)
    f = 1 + C(0)*sqrt_T + C(1)*Ts + C(2)*Ts*sqrt_T + C(3)*Ts**2
    f_T = 0.5*C(0)/sqrt_T + C(1) + 1.5*C(2)*sqrt_T + 2*C(3)*Ts
    f_TT = -0.25*C(0)/(Ts*sqrt_T) + 0.75*C(2)/sqrt_T + 2*C(3)
    q = f**ex
    q_T = ex*f**(ex-1)*f_T
    q_TT = ex*(ex-1)*f**(ex-2)*f_T**2 + ex*f**(ex-1)*f_TT
    !
    q = rs*q
    q_T = rs*q_T/eps_divk
    q_TT = rs*q_TT/eps_divk**2
  end subroutine uv_q_of_T

  ! Hardsphere second virial
  subroutine uv_hs_b2(T, sigma, eps_divk, b2, b2_T, b2_TT)
    real, intent(in) :: T !
    real, intent(in) :: sigma !
    real, intent(in) :: eps_divk !
    real, intent(out) :: b2, b2_T, b2_TT
    !
    real :: q, q_T, q_TT, prefac
    call uv_q_of_T(T, sigma, eps_divk, q, q_T, q_TT)
    prefac = 2*pi/3
    b2 = prefac*q**3
    b2_T = 3*prefac*q**2*q_T
    b2_TT = 6*prefac*q*q_T**2 + 3*prefac*q**2*q_TT
  end subroutine uv_hs_b2

  ! Lennard-Jones spline second virial
  subroutine ljs_b2(T, eps_divk, b2, b2_T, b2_TT)
    real, intent(in) :: T !
    real, intent(in) :: eps_divk !
    real, intent(out) :: b2, b2_T, b2_TT
    !
    real, parameter :: BI(0:4) = (/1.345, -1.336, -3.85, 1.295, -0.416/)
    real :: bs, bs_T, bs_TT
    bs = eps_divk/T
    bs_T = -eps_divk/T**2
    bs_TT = 2*eps_divk/T**3
    b2 = BI(0) + BI(1)*bs + BI(2)*bs**2 + BI(3)*bs**3 + BI(4)*bs**4
    b2_T = BI(1)*bs_T + 2*BI(2)*bs*bs_T + 3*BI(3)*bs**2*bs_T + 4*BI(4)*bs**3*bs_T
    b2_TT = BI(1)*bs_TT + 2*BI(2)*bs_T**2 + 2*BI(2)*bs*bs_TT + &
         6*BI(3)*bs*bs_T**2 + 3*BI(3)*bs**2*bs_TT + &
         12*BI(4)*bs**2*bs_T**2 + 4*BI(4)*bs**3*bs_TT
  end subroutine ljs_b2

  !
  subroutine uv_a1_u_mult_T(eps_divk,d,rho,a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd)
    real, intent(in) :: eps_divk !
    real, intent(in) :: d !
    real, intent(in) :: rho !
    real, intent(out) :: a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd
    !
    real :: c(8) = (/-0.8513603, 0.2977649, -0.1287404, &
         -0.3377645, 0.7343770, -0.9839062, -0.4592915, 0.7963438/)
    real :: a1_div_r, a1_div_r_r
    a1_div_r = 2*pi*((c(1) + c(2)*d**3) + (c(3) + c(4)*d**3)*rho + &
         (c(5) + c(6)*d**3)*rho**2 + (c(7) + c(8)*d**3)*rho**3)
    a1_div_r_r = 2*pi*((c(3) + c(4)*d**3) + &
         2*(c(5) + c(6)*d**3)*rho + 3*(c(7) + c(8)*d**3)*rho**2)
    a1 = a1_div_r*rho
    a1_r = a1_div_r + a1_div_r_r*rho
    a1_rr = 2*a1_div_r_r + 2*pi*rho*(2*(c(5) + c(6)*d**3) + 6*(c(7) + c(8)*d**3)*rho)
    a1_d = 6*pi*rho*(c(2) + c(4)*rho + c(6)*rho**2 + c(8)*rho**3)*d**2
    a1_dd = 12*pi*rho*(c(2) + c(4)*rho + c(6)*rho**2 + c(8)*rho**3)*d
    a1_rd = 6*pi*d**2*((c(2) + c(4)*rho + c(6)*rho**2 + c(8)*rho**3) + &
         rho*(c(4) + 2*c(6)*rho + 3*c(8)*rho**2))
    a1_rdd = 12*pi*d*((c(2) + c(4)*rho + c(6)*rho**2 + c(8)*rho**3) + &
         rho*(c(4) + 2*c(6)*rho + 3*c(8)*rho**2))
    !
    a1 = a1*eps_divk
    a1_r = a1_r*eps_divk
    a1_rr = a1_rr*eps_divk
    a1_d = a1_d*eps_divk
    a1_dd = a1_dd*eps_divk
    a1_rd = a1_rd*eps_divk
    a1_rdd = a1_rdd*eps_divk
  end subroutine uv_a1_u_mult_T

  !
  subroutine uv_a1_u(sigma,eps_divk,T,dhs,rho,a1,a1_r,a1_T,a1_rr,a1_TT,a1_rT)
    real, intent(in) :: sigma !
    real, intent(in) :: eps_divk !
    real, intent(in) :: T !
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    real, intent(in) :: rho !
    real, intent(out) :: a1,a1_r,a1_T,a1_rr,a1_TT,a1_rT
    !
    real :: a1_d,a1_dd,a1_rd,a1_rdd
    real :: d, d_T, d_TT
    d = dhs%d(1,1)/sigma
    d_T = dhs%d_T(1,1)/sigma
    d_TT = dhs%d_TT(1,1)/sigma
    call uv_a1_u_mult_T(eps_divk,d,rho,a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd)
    !
    a1_T = a1_d*d_T/T - a1/T**2
    a1_rT = a1_rd*d_T/T - a1_r/T**2
    a1_TT = a1_dd*d_T**2/T + a1_d*d_TT/T - a1_d*d_T/T**2 - a1_d*d_T/T**2  + 2*a1/T**3
    !
    a1 = a1/T
    a1_r = a1_r/T
    a1_rr = a1_rr/T
  end subroutine uv_a1_u

  !
  subroutine uv_a1_b2(dhs,sigma,eps_divk,T,b2,b2_T,b2_TT)
    real, intent(in) :: sigma !
    real, intent(in) :: eps_divk !
    real, intent(in) :: T !
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    real, intent(out) :: b2,b2_T,b2_TT
    !
    real :: rho_virial
    real :: a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd
    real :: d, d_T, d_TT
    d = dhs%d(1,1)/sigma
    d_T = dhs%d_T(1,1)/sigma
    d_TT = dhs%d_TT(1,1)/sigma
    rho_virial = 0
    call uv_a1_u_mult_T(eps_divk,d,rho_virial,a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd)
    b2 = a1_r/T
    b2_T = a1_rd*d_T/T - a1_r/T**2
    b2_TT = a1_rd*d_TT/T + a1_rdd*d_T**2/T - 2*a1_rd*d_T/T**2 &
         + 2*a1_r/T**3
  end subroutine uv_a1_b2

  ! u-fraction uv theory - Temperature dependent fraction
  subroutine uv_phi_temp(eps_divk,T,rho,u,u_r,u_T,u_rr,u_TT,u_rT)
    real, intent(in) :: eps_divk !
    real, intent(in) :: T !
    real, intent(in) :: rho !
    real, intent(out) :: u,u_r,u_T,u_rr,u_TT,u_rT
    ! Locals
    real, parameter :: C(7) = (/1.01236, 0.99164, 2.63226, 1.21219, &
         2.25005, 3.91411, 0.436593/)
    real :: sigma_beta, sigma_beta_b, sigma_beta_bb, denum, sqrt_C7, denum_sq
    real :: tanhx, tanhx_r, tanhx_rr
    real :: x, x_r, x_rr, e2x, prefac
    real :: u_b,u_bb,u_rb,bs,bs_T,bs_TT
    bs = eps_divk/T
    bs_T = -eps_divk/T**2
    bs_TT = 2*eps_divk/T**3
    sqrt_C7 = sqrt(C(7))
    denum_sq = 1 + C(7)*bs**2
    denum = sqrt(denum_sq)
    sigma_beta = sqrt_C7*bs/denum
    sigma_beta_b = sqrt_C7/denum - sqrt_C7*C(7)*bs**2/(denum*denum_sq)
    sigma_beta_bb = -3*sqrt_C7*C(7)*bs/(denum*denum_sq) + &
         3*sqrt_C7*C(7)**2*bs**3/(denum*denum_sq**2)
    prefac = (C(1) + sigma_beta*(C(2) - C(1)))
    x = C(3)*rho**C(4) + C(5)*rho**C(6)
    x_r = C(3)*C(4)*rho**(C(4)-1) + C(5)*C(6)*rho**(C(6)-1)
    x_rr = C(3)*C(4)*(C(4)-1)*rho**(C(4)-2) + C(5)*C(6)*(C(6)-1)*rho**(C(6)-2)
    e2x = exp(2*x)
    tanhx = (e2x-1)/(e2x+1)
    denum = (e2x+1)**2
    tanhx_r = 4*e2x*x_r/denum
    tanhx_rr = 2*tanhx_r*x_r + 4*e2x*x_rr/denum &
         - 16*e2x*e2x*x_r**2/(denum*(e2x+1))
    u = prefac*tanhx
    u_b = sigma_beta_b*(C(2) - C(1))*tanhx
    u_bb = sigma_beta_bb*(C(2) - C(1))*tanhx
    u_r = prefac*tanhx_r
    u_rr = prefac*tanhx_rr
    u_rb = sigma_beta_b*(C(2) - C(1))*tanhx_r
    !
    u_T = u_b*bs_T
    u_TT = u_bb*bs_T**2 + u_b*bs_TT
    u_rT = u_rb*bs_T
  end subroutine uv_phi_temp

  ! u-fraction uv theory
  subroutine uv_phi(eps_divk,T,rho,u,u_r,u_T,u_rr,u_TT,u_rT)
    real, intent(in) :: eps_divk !
    real, intent(in) :: T !
    real, intent(in) :: rho !
    real, intent(out) :: u,u_r,u_T,u_rr,u_TT,u_rT
    ! Locals
    real, parameter :: C(5) = (/0.11072527, 2.5809656, 1.2333385, &
         2.3534453, 4.1241290/)
    real :: tanhx, tanhx_r, tanhx_rr
    real :: x, x_r, x_rr, e2x, denum
    x = C(1)*rho + C(2)*rho**C(3) + C(4)*rho**C(5)
    x_r = C(1) + C(2)*C(3)*rho**(C(3)-1) + C(4)*C(5)*rho**(C(5)-1)
    x_rr = C(2)*C(3)*(C(3)-1)*rho**(C(3)-2) + C(4)*C(5)*(C(5)-1)*rho**(C(5)-2)
    e2x = exp(2*x)
    tanhx = (e2x-1)/(e2x+1)
    denum = (e2x+1)**2
    tanhx_r = 4*e2x*x_r/denum
    tanhx_rr = 2*tanhx_r*x_r + 4*e2x*x_rr/denum &
         - 16*e2x*e2x*x_r**2/(denum*(e2x+1))
    u = tanhx
    u_T = 0
    u_TT = 0
    u_r = tanhx_r
    u_rr = tanhx_rr
    u_rT = 0
  end subroutine uv_phi

  ! u-fraction uv theory
  subroutine uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho,b2,b2_r,b2_T,b2_rr,b2_TT,b2_rT)
    real, intent(in) :: sigma !
    real, intent(in) :: eps_divk !
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    real, intent(in) :: T !
    real, intent(in) :: rho !
    real, intent(out) :: b2,b2_r,b2_T,b2_rr,b2_TT,b2_rT
    !
    real :: b2_a1,b2_a1_T,b2_a1_TT
    real :: b2_ljs,b2_ljs_T,b2_ljs_TT
    real :: b2_hs,b2_hs_T,b2_hs_TT
    call uv_a1_b2(dhs,sigma,eps_divk,T,b2_a1,b2_a1_T,b2_a1_TT)
    call ljs_b2(T, eps_divk, b2_ljs, b2_ljs_T, b2_ljs_TT)
    call uv_hs_b2(T, sigma, eps_divk, b2_hs, b2_hs_T, b2_hs_TT)
    b2 = b2_ljs - b2_hs - b2_a1
    b2_T = b2_ljs_T - b2_hs_T - b2_a1_T
    b2_TT = b2_ljs_TT - b2_hs_TT - b2_a1_TT
    !
    b2_r = b2
    b2_rr = 0
    b2_rT = b2_T
    b2 = b2*rho
    b2_T = b2_T*rho
    b2_TT = b2_TT*rho
  end subroutine uv_delta_b2_overall

  !> Calculate uv-theory a1
  !!
  !! \author Morten Hammer, 2021-03
  subroutine calc_uv_WCA(sigma,eps_divk,rho,T,dhs,a,a_r,a_t,a_rr,a_tt,a_rt,&
       enable_virial_term, use_temperature_dependent_u_fraction)
    ! Input
    real, intent(in) :: sigma !
    real, intent(in) :: eps_divk !
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    logical, intent(in) :: enable_virial_term, use_temperature_dependent_u_fraction
    ! Output
    real, intent(out) :: a,a_r,a_t,a_rr,a_tt,a_rt
    ! Locals
    real :: u,u_r,u_t,u_rr,u_tt,u_rt
    real :: a1,a1_r,a1_t,a1_rr,a1_tt,a1_rt
    real :: b2,b2_r,b2_t,b2_rr,b2_tt,b2_rt
    !
    if (enable_virial_term) then
      call uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho,b2,b2_r,b2_T,b2_rr,b2_TT,b2_rT)
      if (use_temperature_dependent_u_fraction) then
        call uv_phi_temp(eps_divk,T,rho,u,u_r,u_T,u_rr,u_TT,u_rT)
      else
        call uv_phi(eps_divk,T,rho,u,u_r,u_T,u_rr,u_TT,u_rT)
      endif
    else
      b2 = 0
      b2_r = 0
      b2_T = 0
      b2_rr = 0
      b2_TT = 0
      b2_rT = 0
      u = 0
      u_r = 0
      u_T = 0
      u_rr = 0
      u_TT = 0
      u_rT = 0
    endif
    call uv_a1_u(sigma,eps_divk,T,dhs,rho,a1,a1_r,a1_T,a1_rr,a1_TT,a1_rT)
    !
    a = a1 + (1-u)*b2
    a_r = a1_r + (1-u)*b2_r - u_r*b2
    a_rr = a1_rr + (1-u)*b2_rr - 2*u_r*b2_r - u_rr*b2
    a_t = a1_t + (1-u)*b2_t - u_t*b2
    a_tt = a1_tt + (1-u)*b2_tt - 2*u_t*b2_t - u_tt*b2
    a_rt = a1_rt + (1-u)*b2_rt - u_rt*b2 - u_t*b2_r - u_r*b2_t
  end subroutine calc_uv_WCA

  !> Get pointer to LJS BH eos
  !!
  !! \author Morten Hammer, 2021-12
  function get_bh_ljs_eos_pointer(base_eos) result(eos)
    use thermopack_var, only: base_eos_param
    class(base_eos_param), pointer, intent(in) :: base_eos
    class(ljs_bh_eos), pointer :: eos
    eos => NULL()
    if (.not. associated(base_eos)) return
    select type(p_eos => base_eos)
    type is (ljs_bh_eos)
      eos => p_eos
    class default
      call stoperror("Error casting to saftvrmie_eos")
    end select
  end function get_bh_ljs_eos_pointer


  !> Calculate reduced molar dispersion contribution to Helmholts free energy
  !!
  !! \author Morten Hammer, September 2022
  subroutine calc_ljs_dispersion(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    class(base_eos_param), pointer, intent(in) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    logical :: enable_hs
    real :: nsum
    select type ( p_eos => eos )
    class is (ljs_wca_eos)
      ! Disable hard-sphere term
      enable_hs = p_eos%enable_hs
      p_eos%enable_hs = .false.
      call calcFres_WCA(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      p_eos%enable_hs = enable_hs
    class is (ljs_bh_eos)
      ! Disable hard-sphere term
      enable_hs = p_eos%enable_hs
      p_eos%enable_hs = .false.
      call calcFresLJs_bh(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      p_eos%enable_hs = enable_hs
    class default
      call stoperror("calc_ljs_dispersion: Wrong eos...")
    end select

    nsum = sum(n)
    if (present(F_Tn)) F_Tn(1) = F_Tn(1)/nsum - F_T/nsum**2
    if (present(F_Vn)) F_Vn(1) = F_Vn(1)/nsum - F_V/nsum**2
    if (present(F_nn)) F_nn(1,1) = F_nn(1,1)/nsum - 2*F_n(1)/nsum**2 + 2*F/nsum**3
    if (present(F_n)) F_n(1) = F_n(1)/nsum - F/nsum**2
    if (present(F_TT)) F_TT = F_TT/nsum
    if (present(F_VV)) F_VV = F_VV/nsum
    if (present(F_T)) F_T = F_T/nsum
    if (present(F_V)) F_V = F_V/nsum
    if (present(F_TV)) F_TV = F_TV/nsum
    F = F/nsum
  end subroutine calc_ljs_dispersion

  !> Calculate reduced molar hard-sphere contribution to Helmholts free energy
  !!
  !! \author Morten Hammer, October 2022
  subroutine calc_ljs_hard_sphere(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    class(base_eos_param), pointer, intent(in) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    logical :: enable_hs, enable_cavity, enable_a1, enable_a2, enable_a3, enable_a4
    real :: nsum
    select type ( p_eos => eos )
    class is (ljs_wca_eos)
      ! Enable only hard-sphere term
      enable_hs = p_eos%enable_hs
      enable_cavity = p_eos%enable_cavity
      enable_a1 = p_eos%enable_a1
      enable_a2 = p_eos%enable_a2
      enable_a3 = p_eos%enable_a3
      enable_a4 = p_eos%enable_a4
      !
      p_eos%enable_hs = .true.
      p_eos%enable_cavity = .false.
      p_eos%enable_a1 = .false.
      p_eos%enable_a2 = .false.
      p_eos%enable_a3 = .false.
      p_eos%enable_a4 = .false.
      call calcFres_WCA(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      p_eos%enable_hs = enable_hs
      p_eos%enable_cavity = enable_cavity
      p_eos%enable_a1 = enable_a1
      p_eos%enable_a2 = enable_a2
      p_eos%enable_a3 = enable_a3
      p_eos%enable_a4 = enable_a4
    class is (ljs_bh_eos)
      ! Enable only hard-sphere term
      enable_hs = p_eos%enable_hs
      enable_a1 = p_eos%enable_a1
      enable_a2 = p_eos%enable_a2
      enable_a3 = p_eos%enable_a3
      !
      p_eos%enable_hs = .true.
      p_eos%enable_a1 = .false.
      p_eos%enable_a2 = .false.
      p_eos%enable_a3 = .false.
      call calcFresLJs_bh(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      p_eos%enable_hs = enable_hs
      p_eos%enable_a1 = enable_a1
      p_eos%enable_a2 = enable_a2
      p_eos%enable_a3 = enable_a3
    class default
      call stoperror("calc_ljs_hard_sphere: Wrong eos...")
    end select

    nsum = sum(n)
    if (present(F_Tn)) F_Tn(1) = F_Tn(1)/nsum - F_T/nsum**2
    if (present(F_Vn)) F_Vn(1) = F_Vn(1)/nsum - F_V/nsum**2
    if (present(F_nn)) F_nn(1,1) = F_nn(1,1)/nsum - 2*F_n(1)/nsum**2 + 2*F/nsum**3
    if (present(F_n)) F_n(1) = F_n(1)/nsum - F/nsum**2
    if (present(F_TT)) F_TT = F_TT/nsum
    if (present(F_VV)) F_VV = F_VV/nsum
    if (present(F_T)) F_T = F_T/nsum
    if (present(F_V)) F_V = F_V/nsum
    if (present(F_TV)) F_TV = F_TV/nsum
    F = F/nsum
  end subroutine calc_ljs_hard_sphere

  !> Calculate reduced dispersion contribution to Helmholts free energy
  !!
  !! \author Morten Hammer, September 2022
  subroutine calc_wca_soft_repulsion(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    class(ljs_wca_eos), pointer, intent(in) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    !
    ! The hard-sphere diameter
    call calc_dhs_WCA(nc,eos%sigma,eos%eps_divk,T,eos%dhs)
    ! Packing fraction
    call calcZetaX_vdW_no_segments(nc,T,V,n,eos%dhs,eos%eta_hs)
    ! Soft repulsion term
    call calc_cavity_integral_LJ_Fres(nc,eos%sigma,eos%eps_divk,eos%dhs,eos%eta_hs,T,V,n,&
         F,F_T,F_V,F_n,F_TT,F_VV,F_TV,F_Tn,F_Vn,F_nn,disable_n_multiplication=.true.)
  end subroutine calc_wca_soft_repulsion

  !> Return interaction potential
  !!
  !! \author Morten Hammer, October 2022
  subroutine ljs_potential_reduced(n, r_div_sigma, pot)
    ! Input
    integer, intent(in) :: n !< Array size
    real, intent(in) :: r_div_sigma(n) !< Intermolecular separation reduced by sigma (-)
    real, intent(out) :: pot(n) !< Potential divided by Boltzmann constant
    !
    ! Locals
    integer :: i
    real :: xs, xc, a, b, irm
    xs = (26.0/7.0)**(1.0/6.0)
    xc = 67.0/48.0 * xs
    a = -24192.0/3211.0/xs**2
    b = -387072.0/61009.0/xs**3
    pot = 0
    do i=1,n
      if (r_div_sigma(i) < xs) then
        irm = 1.0/r_div_sigma(i)**6
        pot(i) = 4.0*(irm*irm-irm)
      else if (r_div_sigma(i) < xc) then
        pot(i) = a*(r_div_sigma(i)-xc)**2+b*(r_div_sigma(i)-xc)**3
      else
        exit
      endif
    enddo
  end subroutine ljs_potential_reduced

end module lj_splined

subroutine testing_uf()
  use hardsphere_wca
  use lj_splined
  use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta, &
       allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs, &
       allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta
  implicit none
  real :: n(1),n0(1),n2(1),n1(1)
  real :: eps, T, V, sigma, eps_divk, d
  real :: rho,a_r,a_rr,a_rt
  real :: a1_r,a2_r,ax_rr,ax_tt,ax_rt
  real :: a,a_V,a_t,a_n(1),a_VV,a_tt,a_Vt,a_nn(1,1),a_nT(1),a_nV(1)
  real :: a1,a1_V,a1_t,a1_n(1),a1_VV,a1_tt,a1_Vt,a1_nn(1,1),a1_nT(1),a1_nV(1)
  real :: a2,a2_V,a2_t,a2_n(1),a2_VV,a2_tt,a2_Vt,a2_nn(1,1),a2_nT(1),a2_nV(1)
  integer :: nc1
  class(ljx_ux_eos), pointer :: eos
  real, parameter :: sigma_param = 2.801e-10, eps_divk_param = 33.921
  allocate(eos, source=ljx_ux_eos_constructor("LJS-UF"))
  T = 2.0
  rho = 0.25
  d = 1.0e-5
  call calc_uf_WCA(rho,T,eos%lj_potential,a,a_r,a_t,a_rr,a_tt,a_rt)
  call calc_uf_WCA(rho-d,T,eos%lj_potential,a1,a1_r,a1_t,ax_rr,ax_tt,ax_rt)
  call calc_uf_WCA(rho+d,T,eos%lj_potential,a2,a2_r,a2_t,ax_rr,ax_tt,ax_rt)
  print *,"LJ",a
  print *,"LJ",a_r,(a2-a1)/(2*d)
  print *,"LJ",a_rr,(a2_r-a1_r)/(2*d), (a2-2*a+a1)/d**2
  print *,"LJ",a_rt,(a2_t-a1_t)/(2*d)
  !
  call calc_uf_WCA(rho,T-d,eos%lj_potential,a1,a1_r,a1_t,ax_rr,ax_tt,ax_rt)
  call calc_uf_WCA(rho,T+d,eos%lj_potential,a2,a2_r,a2_t,ax_rr,ax_tt,ax_rt)
  print *,"LJ",a_t,(a2-a1)/(2*d)
  print *,"LJ",a_tt,(a2_t-a1_t)/(2*d)
  print *,"LJ",a_rt,(a2_r-a1_r)/(2*d)

  sigma = sigma_param
  eps_divk = eps_divk_param
  n = (/1.2/)
  n0 = n
  V = 5.0e-5*sum(n)
  T = 45.0
  eps = 1.0e-5
  nc1=1
  d = 1.0e-5
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T,V,n,a,a_t,a_v,a_n,a_tt,a_vv,a_Vt,a_nT,a_nV,a_nn)
  eps = T*d
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T-eps,V,n,a1,a1_t,a1_v,a1_n,a1_tt,a1_vv,a1_Vt,a1_nT,a1_nV,a1_nn)
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T+eps,V,n,a2,a2_t,a2_v,a2_n,a2_tt,a2_vv,a2_Vt,a2_nT,a2_nV,a2_nn)
  print *,"LJ",a
  print *,"LJ",a_t,(a2-a1)/(2*eps)
  print *,"LJ",a_tt,(a2_t-a1_t)/(2*eps), (a2-2*a+a1)/eps**2
  print *,"LJ",a_vt,(a2_v-a1_v)/(2*eps)
  print *,"LJ",a_nt,(a2_n-a1_n)/(2*eps)
  eps = V*d
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T,V-eps,n,a1,a1_t,a1_v,a1_n,a1_tt,a1_vv,a1_Vt,a1_nT,a1_nV,a1_nn)
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T,V+eps,n,a2,a2_t,a2_v,a2_n,a2_tt,a2_vv,a2_Vt,a2_nT,a2_nV,a2_nn)
  print *,"LJ",a_v,(a2-a1)/(2*eps)
  print *,"LJ",a_vv,(a2_v-a1_v)/(2*eps), (a2-2*a+a1)/eps**2
  print *,"LJ",a_vt,(a2_t-a1_t)/(2*eps)
  print *,"LJ",a_nv,(a2_n-a1_n)/(2*eps)

  eps = n(1)*d
  n1(1) = n(1) - eps
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T,V,n1,a1,a1_t,a1_v,a1_n,a1_tt,a1_vv,a1_Vt,a1_nT,a1_nV,a1_nn)
  n2(1) = n(1) + eps
  call calc_uf_WCA_TVN(eos,nc1,sigma,eps_divk,T,V,n2,a2,a2_t,a2_v,a2_n,a2_tt,a2_vv,a2_Vt,a2_nT,a2_nV,a2_nn)
  print *,"LJ",a_n,(a2-a1)/(2*eps)
  print *,"LJ",a_nn,(a2_n-a1_n)/(2*eps), (a2-2*a+a1)/eps**2
  print *,"LJ",a_nt,(a2_t-a1_t)/(2*eps)
  print *,"LJ",a_nv,(a2_v-a1_v)/(2*eps)

end subroutine testing_uf

subroutine test_LJs()
  use lj_splined, only: calc_ai_LJs_ex, calc_a1_LJs_ex, &
       calc_a2_LJs_ex, calc_a3_LJs_ex, calc_chi_LJs_ex
  real :: x0,rho,fac,a,a_e,a_x,a_ee,a_xx,a_ex
  real :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex
  integer :: i
  real :: eps, denum
  x0 = 1.0
  rho = 0.5
  fac = 1.0
  i = 4
  eps = 1.0e-6
  call calc_ai_LJs_ex(i,x0,rho,fac,a,a_e,a_x,a_ee,a_xx,a_ex)
  denum = 1.0
  if (i <= 2) denum = rho
  print *,"rho",rho
  print *,"as",a/denum
  call calc_ai_LJs_ex(i,x0,rho+eps,fac,a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex)
  print *,"a_e ",a_e,(a1-a)/eps
  print *,"a_ee",a_ee,(a1_e-a_e)/eps
  print *,"a_ex",a_ex,(a1_x-a_x)/eps
  call calc_ai_LJs_ex(i,x0+eps,rho,fac,a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex)
  print *,"a_x ",a_x,(a1-a)/eps
  print *,"a_xx",a_xx,(a1_x-a_x)/eps
  print *,"a_ex",a_ex,(a1_e-a_e)/eps
  !
  call calc_a1_LJs_ex(x0,rho,1.0,a1)
  print *,"a1",a1
  call calc_a2_LJs_ex(x0,rho,1.0,a1)
  print *,"a2",a1
  call calc_a3_LJs_ex(x0,rho,1.0,a1)
  print *,"a3",a1
  call calc_chi_LJs_ex(x0,rho,1.0,a1)
  print *,"chi",a1

end subroutine test_LJs

subroutine test_LJs_Fres(nc,Ti,Vi,ni)
  use thermopack_var, only: thermo_model, get_active_thermo_model
  use lj_splined
  use saftvrmie_options
  implicit none
  integer, intent(in) :: nc
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  ! Locals
  real :: n(nc),n0(nc),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(nc) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(nc,nc) :: Fp_nn
  class(ljs_bh_eos), pointer :: p_eos => NULL()
  type(thermo_model), pointer :: act_mod_ptr
  act_mod_ptr => get_active_thermo_model()
  p_eos => get_bh_ljs_eos_pointer(act_mod_ptr%eos(1)%p_eos)
  !allocate(ljs_bh_eos :: p_eos)
  !call init_LJs_bh(nc,act_mod_ptr%comps,p_eos,"DEFAULT")

  if (present(ni)) then
    n = ni
  else
    n = (/1.2/)
  endif
  n0 = n
  if (present(Vi)) then
    V = Vi
  else
    V = 1.0e-4
  endif
  if (present(Ti)) then
    T = Ti
  else
    T = 100.0
  endif

  eps = 1.0e-8

  p_eos%enable_hs = .true.  !> Option to enable/disable hard-sphere contribution
  p_eos%enable_A1 = .true.  !> Option to enable/disable A1 contribution
  p_eos%enable_A3 = .true.  !> Option to enable/disable A3 contribution
  p_eos%enable_A2 = .true.  !> Option to enable/disable A2 contribution

  call calcFresLJs_bh(p_eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcFresLJs_bh(p_eos,nc,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - F)/(V*eps)
  print *,F_VV,(Fp_V - F_V)/(V*eps)
  print *,F_TV,(Fp_T - F_T)/(V*eps)
  print *,F_Vn,(Fp_n - F_n)/(V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call calcFresLJs_bh(p_eos,nc,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  print *,F_n(1),(Fp - F)/eps
  print *,F_Tn(1),(Fp_T - F_T)/eps
  print *,F_Vn(1),(Fp_V - F_V)/eps
  print *,F_nn(1,:),(Fp_n - F_n)/eps

  print *,"T"
  n = n0
  call calcFresLJs_bh(p_eos,nc,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,F_T,(Fp - F)/(T*eps)
  print *,F_TT,(Fp_T - F_T)/(T*eps)
  print *,F_TV,(Fp_V - F_V)/(T*eps)
  print *,F_Tn,(Fp_n - F_n)/(T*eps)

  stop
end subroutine test_LJs_Fres

subroutine test_uf_LJ_Fres()
  use lj_splined
  implicit none
  ! Locals
  real :: sigma, eps_divk
  real :: n(1),n0(1),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(1) :: F_n,F_Tn,F_Vn
  real, dimension(1,1) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(1) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(1,1) :: Fp_nn
  class(ljx_ux_eos), pointer :: eos
  allocate(eos, source=ljx_ux_eos_constructor("LJS-UF"))
  n = (/1.2/)
  n0 = n
  V = 1.0e-4
  T = 30.0
  sigma = 3.5e-10
  eps_divk = 30.0
  eps = 1.0e-5

  T=41.986558345917246
  V=4.6966099964624520E-005
  eos%enable_hs = .true.  !> Option to enable/disable hard-sphere contribution
  eos%enable_cavity = .true.
  eos%enable_A1 = .true.  !> Option to enable/disable A1 contribution


  call calcFresLJ_uf_theory(eos,1,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcFresLJ_uf_theory(eos,1,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - F)/(V*eps)
  print *,F_VV,(Fp_V - F_V)/(V*eps)
  print *,F_TV,(Fp_T - F_T)/(V*eps)
  print *,F_Vn,(Fp_n - F_n)/(V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call calcFresLJ_uf_theory(eos,1,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  print *,F_n(1),(Fp - F)/eps
  print *,F_Tn(1),(Fp_T - F_T)/eps
  print *,F_Vn(1),(Fp_V - F_V)/eps
  print *,F_nn(1,:),(Fp_n - F_n)/eps

  print *,"T"
  n = n0
  call calcFresLJ_uf_theory(eos,1,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,F_T,(Fp - F)/(T*eps)
  print *,F_TT,(Fp_T - F_T)/(T*eps)
  print *,F_TV,(Fp_V - F_V)/(T*eps)
  print *,F_Tn,(Fp_n - F_n)/(T*eps)

  stop
end subroutine test_uf_LJ_Fres

subroutine test_uv_LJs()
  use saftvrmie_containers, only: saftvrmie_var_container, &
       saftvrmie_zeta, saftvrmie_dhs, allocate_saftvrmie_dhs
  use lj_splined
  use hardsphere_wca, only: calc_dhs_WCA
  implicit none
  ! Locals
  real :: sigma, eps_divk
  real :: n(1),n0(1),T,V,eps,n1(1),n2(1)
  type(saftvrmie_dhs) :: dhs !< The hard-sphere diameter
  ! real :: F,F_T,F_V,F_TT,F_VV,F_TV
  ! real, dimension(1) :: F_n,F_Tn,F_Vn
  ! real, dimension(1,1) :: F_nn
  ! real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  ! real, dimension(1) :: Fp_n,Fp_Tn,Fp_Vn
  ! real, dimension(1,1) :: Fp_nn
  ! real :: Fm,Fm_T,Fm_V,Fm_TT,Fm_VV,Fm_TV
  ! real, dimension(1) :: Fm_n,Fm_Tn,Fm_Vn
  ! real, dimension(1,1) :: Fm_nn
  real :: dT, T0, dV, V0
  real :: q, q_T, q_TT, q1, q1_T, q1_TT, q2, q2_T, q2_TT
  real :: b, b_T, b_TT, b1, b1_T, b1_TT, b2, b2_T, b2_TT
  real :: d,rho,a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd
  real :: a12,a12_r,a12_d,a12_rr,a12_dd,a12_rd,a12_rdd
  real :: a12_T,a12_TT,a12_rT,a11_T,a11_TT,a11_rT,a1_T,a1_TT,a1_rT
  real :: a11,a11_r,a11_d,a11_rr,a11_dd,a11_rd,a11_rdd
  real :: u,u_r,u_T,u_rr,u_TT,u_rT
  real :: u1,u1_r,u1_T,u1_rr,u1_TT,u1_rT
  real :: u2,u2_r,u2_T,u2_rr,u2_TT,u2_rT
  real :: b_r,b_rr,b_rT,b1_r,b1_rr,b1_rT,b2_r,b2_rr,b2_rT
  real :: a,a_V,a_t,a_n(1),a_VV,a_tt,a_Vt,a_nn(1,1),a_nT(1),a_nV(1)
  real :: a1_V,a1_n(1),a1_VV,a1_Vt,a1_nn(1,1),a1_nT(1),a1_nV(1)
  real :: a2,a2_V,a2_t,a2_n(1),a2_VV,a2_tt,a2_Vt,a2_nn(1,1),a2_nT(1),a2_nV(1)
  class(ljx_ux_eos), pointer :: eos
  allocate(eos, source=ljx_ux_eos_constructor("LJS-UV"))
  n = (/1.2/)
  n0 = n
  V = 1.0e-4
  V0 = V
  T = 39.0
  T0 = T
  sigma = 3.5e-10
  eps_divk = 30.0
  eps = 1.0e-5

  dT = T*eps
  call uv_q_of_T(T, sigma, eps_divk, q, q_T, q_TT)
  call uv_q_of_T(T + dT, sigma, eps_divk, q2, q2_T, q2_TT)
  call uv_q_of_T(T - dT, sigma, eps_divk, q1, q1_T, q1_TT)
  print *,q
  print *,q_T,(q2-q1)/(2*dT)
  print *,q_TT,(q2_T-q1_T)/(2*dT)

  call uv_hs_b2(T, sigma, eps_divk, b, b_T, b_TT)
  call uv_hs_b2(T + dT, sigma, eps_divk, b2, b2_T, b2_TT)
  call uv_hs_b2(T - dT, sigma, eps_divk, b1, b1_T, b1_TT)
  print *,b
  print *,b_T,(b2-b1)/(2*dT)
  print *,b_TT,(b2_T-b1_T)/(2*dT)

  call ljs_b2(T, eps_divk, b, b_T, b_TT)
  call ljs_b2(T + dT, eps_divk, b2, b2_T, b2_TT)
  call ljs_b2(T - dT, eps_divk, b1, b1_T, b1_TT)
  print *,b
  print *,b_T,(b2-b1)/(2*dT)
  print *,b_TT,(b2_T-b1_T)/(2*dT)

  d = (2/(1+sqrt(T)))**(real(1)/6)
  rho = 0.7
  call uv_a1_u_mult_T(eps_divk,d,rho,a1,a1_r,a1_d,a1_rr,a1_dd,a1_rd,a1_rdd)
  call uv_a1_u_mult_T(eps_divk,d+eps,rho,a12,a12_r,a12_d,a12_rr,a12_dd,a12_rd,a12_rdd)
  call uv_a1_u_mult_T(eps_divk,d-eps,rho,a11,a11_r,a11_d,a11_rr,a11_dd,a11_rd,a11_rdd)
  print *,a1
  print *,a1_d,(a12-a11)/(2*eps)
  print *,a1_dd,(a12_d-a11_d)/(2*eps)
  print *,a1_rd,(a12_r-a11_r)/(2*eps)

  call uv_a1_u_mult_T(eps_divk,d,rho+eps,a12,a12_r,a12_d,a12_rr,a12_dd,a12_rd,a12_rdd)
  call uv_a1_u_mult_T(eps_divk,d,rho-eps,a11,a11_r,a11_d,a11_rr,a11_dd,a11_rd,a11_rdd)
  print *,a1_r,(a12-a11)/(2*eps)
  print *,a1_rr,(a12_r-a11_r)/(2*eps)
  print *,a1_rd,(a12_d-a11_d)/(2*eps)

  call allocate_saftvrmie_dhs(1,dhs)
  !call allocate_saftvrmie_zeta(nc,eta_hs)
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_b2(dhs,sigma,eps_divk,T,b,b_T,b_TT)
  T0 = T
  T = T0 + dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_b2(dhs,sigma,eps_divk,T,b2,b2_T,b2_TT)
  T = T0 - dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_b2(dhs,sigma,eps_divk,T,b1,b1_T,b1_TT)

  print *,b
  print *,b_T,(b2-b1)/(2*dT)
  print *,b_TT,(b2_T-b1_T)/(2*dT)

  rho = 0.5
  T = T0
  call uv_phi(eps_divk,T,rho,u,u_r,u_T,u_rr,u_TT,u_rT)
  call uv_phi(eps_divk,T+dT,rho,u2,u2_r,u2_T,u2_rr,u2_TT,u2_rT)
  call uv_phi(eps_divk,T-dT,rho,u1,u1_r,u1_T,u1_rr,u1_TT,u1_rT)
  print *,u
  print *,u_T,(u2-u1)/(2*dT)
  print *,u_TT,(u2_T-u1_T)/(2*dT)
  print *,u_rT,(u2_r-u1_r)/(2*dT)
  !
  call uv_phi(eps_divk,T,rho+eps,u2,u2_r,u2_T,u2_rr,u2_TT,u2_rT)
  call uv_phi(eps_divk,T,rho-eps,u1,u1_r,u1_T,u1_rr,u1_TT,u1_rT)
  print *,u_r,(u2-u1)/(2*eps)
  print *,u_rr,(u2_r-u1_r)/(2*eps)
  print *,u_rT,(u2_T-u1_T)/(2*eps)
  stop
  rho = 0.3
  T = T0
  dT = T*eps
  call allocate_saftvrmie_dhs(1,dhs)
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho,b,b_r,b_T,b_rr,b_TT,b_rT)
  T = T0 + dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho,b2,b2_r,b2_T,b2_rr,b2_TT,b2_rT)
  T = T0 - dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho,b1,b1_r,b1_T,b1_rr,b1_TT,b1_rT)
  print *,b
  print *,b_T,(b2-b1)/(2*dT)
  print *,b_TT,(b2_T-b1_T)/(2*dT)
  print *,b_rT,(b2_r-b1_r)/(2*dT)
  !
  T = T0
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho+eps,b2,b2_r,b2_T,b2_rr,b2_TT,b2_rT)
  call uv_delta_b2_overall(sigma,eps_divk,T,dhs,rho-eps,b1,b1_r,b1_T,b1_rr,b1_TT,b1_rT)
  print *,b_r,(b2-b1)/(2*eps)
  print *,b_rr,(b2_r-b1_r)/(2*eps)
  print *,b_rT,(b2_T-b1_T)/(2*eps)

  rho = 0.3
  T = T0
  dT = T*eps
  call allocate_saftvrmie_dhs(1,dhs)
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_u(sigma,eps_divk,T,dhs,rho,a1,a1_r,a1_T,a1_rr,a1_TT,a1_rT)
  T = T0 + dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_u(sigma,eps_divk,T,dhs,rho,a12,a12_r,a12_T,a12_rr,a12_TT,a12_rT)
  T = T0 - dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_u(sigma,eps_divk,T,dhs,rho,a11,a11_r,a11_T,a11_rr,a11_TT,a11_rT)
  print *,a1
  print *,a1_T,(a12-a11)/(2*dT)
  print *,a1_TT,(a12_T-a11_T)/(2*dT)
  print *,a1_rT,(a12_r-a11_r)/(2*dT)
  T = T0
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call uv_a1_u(sigma,eps_divk,T,dhs,rho+eps,a12,a12_r,a12_T,a12_rr,a12_TT,a12_rT)
  call uv_a1_u(sigma,eps_divk,T,dhs,rho-eps,a11,a11_r,a11_T,a11_rr,a11_TT,a11_rT)
  print *,a1_r,(a12-a11)/(2*eps)
  print *,a1_rr,(a12_r-a11_r)/(2*eps)
  print *,a1_rT,(a12_T-a11_T)/(2*eps)

  rho = 0.3
  T = T0
  dT = T*eps
  call allocate_saftvrmie_dhs(1,dhs)
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA(sigma,eps_divk,rho,T,dhs,a1,a1_r,a1_t,a1_rr,a1_tt,a1_rt,&
    enable_virial_term=.true., use_temperature_dependent_u_fraction=.false.)
  T = T0 + dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA(sigma,eps_divk,rho,T,dhs,a12,a12_r,a12_t,a12_rr,a12_tt,a12_rt,&
    enable_virial_term=.true., use_temperature_dependent_u_fraction=.false.)
  T = T0 - dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA(sigma,eps_divk,rho,T,dhs,a11,a11_r,a11_t,a11_rr,a11_tt,a11_rt,&
    enable_virial_term=.true., use_temperature_dependent_u_fraction=.false.)
  print *,a1
  print *,a1_T,(a12-a11)/(2*dT)
  print *,a1_TT,(a12_T-a11_T)/(2*dT)
  print *,a1_rT,(a12_r-a11_r)/(2*dT)
  T = T0
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA(sigma,eps_divk,rho+eps,T,dhs,a12,a12_r,a12_t,a12_rr,a12_tt,a12_rt,&
    enable_virial_term=.true., use_temperature_dependent_u_fraction=.false.)
  call calc_uv_WCA(sigma,eps_divk,rho-eps,T,dhs,a11,a11_r,a11_t,a11_rr,a11_tt,a11_rt,&
    enable_virial_term=.true., use_temperature_dependent_u_fraction=.false.)
  print *,a1_r,(a12-a11)/(2*eps)
  print *,a1_rr,(a12_r-a11_r)/(2*eps)
  print *,a1_rT,(a12_T-a11_T)/(2*eps)

  rho = 0.3
  T = T0
  dT = T*eps
  call allocate_saftvrmie_dhs(1,dhs)
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V,n,dhs,a,a_t,a_v,a_n,a_tt,a_vv,a_Vt,a_nT,a_nV,a_nn)
  T = T0 + dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V,n,dhs,a2,a2_t,a2_v,a2_n,a2_tt,a2_vv,a2_Vt,a2_nT,a2_nV,a2_nn)
  T = T0 - dT
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V,n,dhs,a1,a1_t,a1_v,a1_n,a1_tt,a1_vv,a1_Vt,a1_nT,a1_nV,a1_nn)

  print *,"LJs",a
  print *,"LJs",a_t,(a2-a1)/(2*dT)
  print *,"LJs",a_tt,(a2_t-a1_t)/(2*dT), (a2-2*a+a1)/dT**2
  print *,"LJs",a_vt,(a2_v-a1_v)/(2*dT)
  print *,"LJs",a_nt,(a2_n-a1_n)/(2*dT)

  print *,"V"
  T = T0
  call calc_dhs_WCA(1,sigma,eps_divk,T,dhs)
  dV = V*eps
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V+dv,n,dhs,a2,a2_t,a2_v,a2_n,a2_tt,a2_vv,a2_Vt,a2_nT,a2_nV,a2_nn)
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V-dv,n,dhs,a1,a1_t,a1_v,a1_n,a1_tt,a1_vv,a1_Vt,a1_nT,a1_nV,a1_nn)
  print *,"LJs",a_v,(a2-a1)/(2*dV)
  print *,"LJs",a_vv,(a2_v-a1_v)/(2*dV), (a2-2*a+a1)/dV**2
  print *,"LJs",a_vt,(a2_t-a1_t)/(2*dV)
  print *,"LJs",a_nv,(a2_n-a1_n)/(2*dV)

  print *,"n"
  n2(1) = n(1) + eps
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V,n2,dhs,a2,a2_t,a2_v,a2_n,a2_tt,a2_vv,a2_Vt,a2_nT,a2_nV,a2_nn)
  n1(1) = n(1) - eps
  call calc_uv_WCA_TVN(eos,1,sigma,eps_divk,T,V,n1,dhs,a1,a1_t,a1_v,a1_n,a1_tt,a1_vv,a1_Vt,a1_nT,a1_nV,a1_nn)
  print *,"LJs",a_n,(a2-a1)/(2*eps)
  print *,"LJs",a_nn,(a2_n-a1_n)/(2*eps), (a2-2*a+a1)/eps**2
  print *,"LJs",a_nt,(a2_t-a1_t)/(2*eps)
  print *,"LJs",a_nv,(a2_v-a1_v)/(2*eps)

end subroutine test_uv_LJs

subroutine test_uv_LJs_Fres()
  use lj_splined
  use thermopack_var
  use thermopack_constants
  implicit none
  ! Locals
  !real :: sigma, eps_divk
  real :: n(1),n0(1),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(1) :: F_n,F_Tn,F_Vn
  real, dimension(1,1) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(1) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(1,1) :: Fp_nn
  real :: Fm,Fm_T,Fm_V,Fm_TT,Fm_VV,Fm_TV
  real, dimension(1) :: Fm_n,Fm_Tn,Fm_Vn
  real, dimension(1,1) :: Fm_nn
  class(ljx_ux_eos), pointer :: eos
  class(base_eos_param), pointer :: xxeos
  !allocate(eos, source=ljx_ux_eos_constructor("LJS-UF"))
  n = (/1.0/)
  n0 = n
  ! T*=125.67
  ! rho_s=0.6
  V = 4.0149299295600481E-005
  T = 15583.080000000000
  ! T*=10.0
  ! rho_s=0.9
  ! T=1240.0000000000000
  ! V=2.6766199530400317E-005

  ! T*=0.5
  ! rho_s=0.9
  ! T=124.0*0.5
  ! V=2.6766199530400317E-005

  !sigma = 3.5e-10
  !eps_divk = 30.0
  eps = 1.0e-5

  xxeos => get_active_eos()
  select type(p_eos => xxeos)
  class is(ljx_ux_eos)
    eos => p_eos
  class default
    call stoperror("Wrong model initilized")
  end select

  eos%enable_hs = .true.  !> Option to enable/disable hard-sphere contribution
  eos%enable_cavity = .true.
  eos%enable_A1 = .true.  !> Option to enable/disable A1 contribution
  eos%use_temperature_dependent_u_fraction = .false.
  !print *,eos%eps_divk, eos%sigma
  call calcFresLJs_uv_theory(eos,1,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcFresLJs_uv_theory(eos,1,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calcFresLJs_uv_theory(eos,1,T,V-V*eps,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,"Fres",F
  print *,F_V,(Fp - Fm)/(2*V*eps)
  print *,F_VV,(Fp_V - Fm_V)/(2*V*eps)
  print *,F_TV,(Fp_T - Fm_T)/(2*V*eps)
  print *,F_Vn,(Fp_n - Fm_n)/(2*V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call calcFresLJs_uv_theory(eos,1,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  n(1) = n0(1) - eps
  call calcFresLJs_uv_theory(eos,1,T,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  print *,F_n(1),(Fp - Fm)/eps/2
  print *,F_Tn(1),(Fp_T - Fm_T)/eps/2
  print *,F_Vn(1),(Fp_V - Fm_V)/eps/2
  print *,F_nn(1,:),(Fp_n - Fm_n)/eps/2

  print *,"T"
  n = n0
  call calcFresLJs_uv_theory(eos,1,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calcFresLJs_uv_theory(eos,1,T-T*eps,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *,F_T,(Fp - Fm)/(2*T*eps)
  print *,F_TT,(Fp_T - Fm_T)/(2*T*eps)
  print *,F_TV,(Fp_V - Fm_V)/(2*T*eps)
  print *,F_Tn,(Fp_n - Fm_n)/(2*T*eps)

  stop
end subroutine test_uv_LJs_Fres

subroutine test_ai_LJs_WCA()
  use lj_splined
  implicit none
  ! Locals
  real :: eps_divk
  real :: tau,rho,eps, d_star, t_star
  real :: a,a_r,a_y,a_rr,a_yy,a_ry
  real :: a_p,a_p_r,a_p_y,a_p_rr,a_p_yy,a_p_ry
  real :: a_n,a_n_r,a_n_y,a_n_rr,a_n_yy,a_n_ry
  integer :: i
  rho = 0.1
  eps_divk = 1.0
  eps = 1.0e-5
  t_star = 3.5
  d_star = (2.0/(1 + sqrt(t_star)))**(1.0/6.0)
  tau = 2**(real(1)/6) - d_star
  i = 1
  call calc_LJs_WCA_ai(i,eps_divk,tau,rho,a,a_r,a_y,a_rr,a_yy,a_ry)
  call calc_LJs_WCA_ai(i,eps_divk,tau,rho+eps,a_p,a_p_r,a_p_y,a_p_rr,a_p_yy,a_p_ry)
  call calc_LJs_WCA_ai(i,eps_divk,tau,rho-eps,a_n,a_n_r,a_n_y,a_n_rr,a_n_yy,a_n_ry)

  print *,"Testing ai_LJs_WCA"
  print *,"rho"
  print *,"a:   ",a
  print *,"a_r: ",a_r,(a_p - a_n)/(2*eps)
  print *,"a_rr:",a_rr,(a_p_r - a_n_r)/(2*eps)
  print *,"a_ry:",a_ry,(a_p_y - a_n_y)/(2*eps)

  print *,"tau"
  call calc_LJs_WCA_ai(i,eps_divk,tau+eps,rho,a_p,a_p_r,a_p_y,a_p_rr,a_p_yy,a_p_ry)
  call calc_LJs_WCA_ai(i,eps_divk,tau-eps,rho,a_n,a_n_r,a_n_y,a_n_rr,a_n_yy,a_n_ry)
  print *,"a_y: ",a_y,(a_p - a_n)/(2*eps)
  print *,"a_yy:",a_yy,(a_p_y - a_n_y)/(2*eps)
  print *,"a_ry:",a_ry,(a_p_r - a_n_r)/(2*eps)

  stop
end subroutine test_ai_LJs_WCA

subroutine test_LJs_WCA_Fres()
  use lj_splined
  use thermopack_constants, only: N_AVOGADRO
  implicit none
  ! Locals
  integer :: nc=1
  real :: n(1),n0(1),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(1) :: F_n,F_Tn,F_Vn
  real, dimension(1,1) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(1) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(1,1) :: Fp_nn
  real :: Fm,Fm_T,Fm_V,Fm_TT,Fm_VV,Fm_TV
  real, dimension(1) :: Fm_n,Fm_Tn,Fm_Vn
  real, dimension(1,1) :: Fm_nn
  class(ljs_wca_eos), pointer :: eos
  real, parameter :: sigma_param = 2.801e-10, eps_divk_param = 33.921
  allocate(ljs_wca_eos :: eos)
  call eos%allocate_and_init(nc,"LJS-WCA")
  eps = 1.0e-5
  eos%enable_A1 = .true.
  eos%enable_A2 = .true.
  eos%enable_A3 = .true.
  eos%enable_A4 = .true.
  eos%enable_hs = .true.
  eos%enable_cavity = .true.

  T = eps_divk_param
  n = 1.2
  n0 = n
  V = n(1)*N_AVOGADRO*sigma_param**3/0.5
  V = 1.0
  print *,V
  call calcFresLJs_WCA(eos,1,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcFresLJs_WCA(eos,1,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calcFresLJs_WCA(eos,1,T,V-V*eps,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - Fm)/(2*V*eps)
  print *,F_VV,(Fp_V - Fm_V)/(2*V*eps)
  print *,F_TV,(Fp_T - Fm_T)/(2*V*eps)
  print *,F_Vn,(Fp_n - Fm_n)/(2*V*eps)

  print *,"n1"
  n(1) = n0(1) + eps
  call calcFresLJs_WCA(eos,1,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  n(1) = n0(1) - eps
  call calcFresLJs_WCA(eos,1,T,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  print *,F_n(1),(Fp - Fm)/(2*eps)
  print *,F_Tn(1),(Fp_T - Fm_T)/(2*eps)
  print *,F_Vn(1),(Fp_V - Fm_V)/(2*eps)
  print *,F_nn(1,:),(Fp_n - Fm_n)/(2*eps)

  print *,"T"
  n = n0
  call calcFresLJs_WCA(eos,1,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calcFresLJs_WCA(eos,1,T-T*eps,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *,F_T,(Fp - Fm)/(2*T*eps)
  print *,F_TT,(Fp_T - Fm_T)/(2*T*eps)
  print *,F_TV,(Fp_V - Fm_V)/(2*T*eps)
  print *,F_Tn,(Fp_n - Fm_n)/(2*T*eps)
  stop
end subroutine test_LJs_WCA_Fres
