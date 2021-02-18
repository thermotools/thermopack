!---------------------------------------------------------------------
! Module and subroutines for the Lennard-Jones splined
! Equation of State implmented in Thermopack.
! Programmed by: M. Hammer and Ø. Wilhelmsen
! Spring 2019
! © SINTEF Energy Research. All rights reserved.
!---------------------------------------------------------------------
module lj_splined
  use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_var_container, &
       saftvrmie_param, saftvrmie_zeta
  use saftvrmie_utils, only: calc_a_zeta_product, convert_zeta_x_to_TVn, &
       calc_a0_a_product
  use numconstants, only: pi
  use eosdata, only: eosLJS_BH
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
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

  integer, parameter :: nLJS = 1
  type(ljs_data), dimension(nLJS), parameter :: LJSarray = (/ &
       ljs_data(eosidx = eosLJS_BH, &
       compName="AR", &
       sigma=3.42E-10, &
       eps_depth_divk=124.0, &
       ref="DEFAULT") /)

  public :: ljs_bh_eos
  public :: init_LJs_bh, calc_LJs_bh_zeta, calcFresLJs_bh
  public :: calc_LJs_a1, calc_LJs_a2, calc_a3_LJs_ex
  public :: calc_ai_LJs_ex, calc_a1_LJs_ex, calc_a2_LJs_ex
  public :: calc_ais_LJs_ex, calc_chi_LJs_ex
  public :: ljs_bh_model_control, ljs_bh_set_pure_params, ljs_bh_get_pure_params
  public :: calc_ai_reduced_LJs_ex, ljs_bh_get_bh_diameter_div_sigma

contains

  !> Calculate the Wilhelmsen polynomials for the LJ/s
  !! dispersion terms.
  !!
  !! i = 4 gives chi (Correction in a2)
  !! \author Morten Hammer, Spring 2019
  subroutine calc_ai_LJs_ex(i,x0,rho,prefac,a,a_r,a_x,a_rr,a_xx,a_rx)
    ! Input
    integer, intent(in) :: i !< Calculate a_i
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: rho !< Reduced density rho^* = N*sigma^3/V
    real, intent(in) :: prefac !< Constants
    ! Output
    real, intent(out) :: a,a_r,a_x,a_rr,a_xx,a_rx
    ! Locals
    real, parameter :: p(4,15) = reshape(&
         (/  0.046050136, -0.112418762,  0.887791946,  0.000000000, &
             0.455352603, -0.283029169, -1.832901216,  20.55766323, &
            -0.332821197,  0.331754004,  0.68629618,  -10.29116877, &
            -0.346431499,  0.250722039,  0.550930018, -0.506055887, &
            -0.535062927,  0.358472912, -0.268692395,  1.013467495, &
             0.000000000,  0.000000000,  3.09335097,   0.000000000, &
            -7.528877435,  6.793646216, -4.579486347, -485.1658101, &
             9.488730198, -9.266048284,  1.56856135,   525.2464035, &
             0.533745989,  0.133598017, -0.014515242, -98.35952841, &
             0.000000000,  0.000000000, -0.163269272,  2.572362466, &
             0.000000000,  0.000000000,  0.000000000,  0.000000000, &
             30.72700396, -27.3726249 ,  0.000000000,  2584.954799, &
            -45.87690548,  42.42520027,  0.000000000, -3300.865903, &
             4.626543007, -5.654632657,  0.000000000,  781.9992393, &
             0.000000000,  0.000000000,  0.000000000, -50.47603682  /), (/4,15/))
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
    g(2,0) = x0 - 1
    g(2,1) = 1
    g(2,2) = 0
    g(3,0) = x0**2 - 2*x0 + 1
    g(3,1) = 2*x0 - 2
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
    use saftvrmie_dispersion, only: calcXDifferentials
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
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: eps !< Well depth div. temperature (K)
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Sigma
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
    s_T = 0.0
    s_TT = 0.0
    d = s_vc%dhs%d(1,1)
    d_T = s_vc%dhs%d_T(1,1)
    d_TT = s_vc%dhs%d_TT(1,1)
    call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    rho = s_vc%rho_star%zx
    eps = saftvrmie_param%eps_divk_ij(1,1)
    fac = 2*pi*eps
    call calc_ai_LJs_ex(1,x0,rho,fac,a1,a_r,a_x,a_rr,a_xx,a_rx)
    call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,s_vc%rho_star,&
         a1,a_r,a_x,a_rr,a_xx,a_rx,0.0,0.0,0.0,&
         a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         difflevel=difflevel)

  end subroutine calc_LJs_a1

  !> Calculate LJ/s a2
  !! Based on derivation by Ø. Wilhelmsen
  !! \author Morten Hammer, March 2019
  subroutine calc_LJs_a2(eos,nc,T,V,n,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)
    use saftvrmie_dispersion, only: calcXDifferentials
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
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: eps !< Well depth div. temperature (K)
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Sigma
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
    s_T = 0.0
    s_TT = 0.0
    d = eos%saftvrmie_var%dhs%d(1,1)
    d_T =  eos%saftvrmie_var%dhs%d_T(1,1)
    d_TT =  eos%saftvrmie_var%dhs%d_TT(1,1)
    call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    rho =  eos%saftvrmie_var%rho_star%zx
    eps = saftvrmie_param%eps_divk_ij(1,1)
    fac = -pi*eps**2
    call calc_ai_LJs_ex(2,x0,rho,fac,a2,a_r,a_x,a_rr,a_xx,a_rx)
    call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT, eos%saftvrmie_var%rho_star,&
         a2,a_r,a_x,a_rr,a_xx,a_rx,0.0,0.0,0.0,&
         a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         difflevel=difflevel)

    ! Multiply with (1 + chi)?
    if (eos%enable_chi_correction) then
      fac = 1.0
      call calc_ai_LJs_ex(4,x0,rho,fac,c2,c2_r,c2_x,c2_rr,c2_xx,c2_rx)
      c2 = 1 + c2
      call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT, eos%saftvrmie_var%rho_star,&
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
    use saftvrmie_dispersion, only: calcXDifferentials
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
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Sigma
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
      s_T = 0.0
      s_TT = 0.0
      d = eos%saftvrmie_var%dhs%d(1,1)
      d_T = eos%saftvrmie_var%dhs%d_T(1,1)
      d_TT = eos%saftvrmie_var%dhs%d_TT(1,1)
      call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
      rho = eos%saftvrmie_var%rho_star%zx
      eps = saftvrmie_param%eps_divk_ij(1,1)
      fac = eps**3
      call calc_ai_LJs_ex(3,x0,rho,fac,a3,a3_r,a3_x,a3_rr,a3_xx,a3_rx)
      call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,eos%saftvrmie_var%rho_star,&
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
    real :: fac
    fac = 2*pi*eps
    call calc_ai_LJs_ex(1,x0,rho,fac,a1,a1_r,a1_x,a1_rr,a1_xx,a1_rx)
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
    real :: fac
    real :: chi, chi_r, chi_rr, chi_rx, chi_x, chi_xx
    real :: Khs, Khs_e, Khs_ee, Khs_eee
    fac = -pi*eps**2
    call calc_ai_LJs_ex(2,x0,rho,fac,a2,a_r,a_x,a_rr,a_xx,a_rx)
    ! Multiply with (1 + chi)
    fac = 1.0
    call calc_ai_LJs_ex(4,x0,rho,fac,chi,chi_r,chi_x,chi_rr,chi_xx,chi_rx)
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
    real :: fac,a3_r,a3_rr,a3_x,a3_xx,a3_rx

    fac = eps**3
    call calc_ai_LJs_ex(3,x0,rho,fac,a3,a3_r,a3_x,a3_rr,a3_xx,a3_rx)

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
    real :: fac,chi_r,chi_rr,chi_x,chi_xx,chi_rx

    fac = 1.0
    call calc_ai_LJs_ex(4,x0,rho,fac,chi,chi_r,chi_x,chi_rr,chi_xx,chi_rx)

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
    real :: fac
    real :: a_r,a_x,a_rr,a_xx,a_rx
    if (i <= 2) then
      fac = 1.0/rho
    else
      fac = 1.0
    endif
    call calc_ai_LJs_ex(i,x0,rho,fac,ai,a_r,a_x,a_rr,a_xx,a_rx)
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
    use thermopack_constants, only: Rgas, kRgas, N_Avogadro, kB_const
    use saftvrmie_containers, only: cleanup_saftvrmie_param_container, &
         cleanup_saftvrmie_var_container, allocate_saftvrmie_zeta, &
         allocate_saftvrmie_param_container, allocate_saftvrmie_dhs, &
         calcFunAlpha
    integer, intent(in) :: nc          !< Number of components.
    type(gendata_pointer), intent(inout) :: comp(nc)    !< Component vector.
    class(ljs_bh_eos), intent(inout) :: ljs
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    ! Locals
    real :: f_alpha(6)
    real :: sigma, eps_depth_divk
    integer :: idx

    ! Deallocate old memory and init new memory
    call ljs%allocate_and_init(nc,"LJS-BH")
    saftvrmie_param => ljs%saftvrmie_param

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

  !> Get the index in the LJSarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getLJSdataIdx(eosidx,compName,ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, ref
    integer :: idx, idx_default
    logical :: found

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nLJS)
      if ((eosidx == LJSarray(idx)%eosidx) .and. &
           str_eq(compName, LJSarray(idx)%compName)) then
        if (string_match(ref,LJSarray(idx)%ref)) then
          found = .true.
          exit
        else if (string_match("DEFAULT",LJSarray(idx)%ref)) then
          idx_default = idx
        endif
      endif
      idx = idx + 1
    enddo

    if (.not. found .and. idx_default > 0) then
      idx = idx_default
      found = .true.
    endif
    if (.not. found) then
       print *, "ERROR FOR COMPONENT ", compname
       call stoperror("The LJS parameters don't exist.")
    end if

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
          class is ( ljs_bh_eos )
            call p_eos%set_sigma_eps(sigma, eps_depth_divk)
          end select
        else
           print *,"ljs_bh_set_pure_params: eos not acociated"
         endif
       enddo
     else
       print *,"ljs_bh_set_pure_params: eos array not allocted found"
     endif
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

end module lj_splined

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
  allocate(ljs_bh_eos :: p_eos)
  call init_LJs_bh(nc,act_mod_ptr%comps,p_eos,"DEFAULT")

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
