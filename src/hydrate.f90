!---------------------------------------------------------------------
! Module for calculating Hydrate fugacities
! Programmed by: M. Hammer
!---------------------------------------------------------------------
module hydrate
  use thermopack_constants, only: kB_const, N_Avogadro, VAPPH, LIQPH, SOLIDPH, &
       ref_len
  use thermopack_var, only: nc, complist, Rgas
  use eostv, only: thermo_tv, pressure
  implicit none
  private

  ! SI Hydrate cavities
  integer, parameter          :: n_cavities = 2
  integer, parameter          :: SMALL_CAVITY = 1
  integer, parameter          :: LARGE_CAVITY = 2
  ! Number of type cavities per water molecule
  real, parameter             :: v_cav(n_cavities) = (/1.0/23.0, 3.0/23.0 /)
  ! Coordination number of the cavity (Avalonitis 1994: 10.1016/0009-2509(94)85087-9)
  real, parameter             :: z_cav(n_cavities) = (/20.0, 24.0 /)
  ! Averge cavity radius (Avalonitis 1994: 10.1016/0009-2509(94)85087-9)
  real, parameter             :: R_cav(n_cavities) = (/3.95, 4.33 /) !Å

  ! Triple point
  real :: Ptriple = 611.73
  real, parameter :: Ttriple = 273.16
  integer :: water_idx = -1
  logical :: use_solid_ice_eos = .true.
  logical, parameter :: verbose = .false.

  !> This data structure stores parameters for the
  !> Kihara potenital
  ! ---------------------------------------------------------------------------
  type :: hydrate_data
     character (len=10) :: compName
     real :: sigma    !< [Å]. Temperature-independent segment diameter.
     real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.
     real :: alpha !< [Å]
     ! Parameter set
     character(len=ref_len) :: ref
   end type hydrate_data

  !  Avalonitis 1994: 10.1016/0009-2509(94)85087-9
  type(hydrate_data), parameter :: Hyd1 = hydrate_data("CO2", &
       2.349, 420.3, 0.7530, "AVALONITIS")
  type(hydrate_data), parameter :: Hyd2 = hydrate_data("C1", &
       3.067, 199.5, 0.295, "AVALONITIS")

  ! B.T. Kalorazi “Gas hydrate equilibria in the presence of electrolyte solutions”, PhD-thesis, Heriot-
  ! Watt University, 1995
  type(hydrate_data), parameter :: Hyd3 = hydrate_data("CO2", &
       2.9040, 171.97, 0.7530, "KALORAZI/DEFAULT")
  type(hydrate_data), parameter :: Hyd4 = hydrate_data("C1", &
       3.2512, 153.69, 0.2950, "KALORAZI/DEFAULT")

  ! E.D. Sloan and C.A. Koh “ Clathrate Hydrates of Natural Gases” Third edition, CRC Press, 2008
  ! ISBN: 9780849390784
  type(hydrate_data), parameter :: Hyd5 = hydrate_data("CO2", &
       2.97638, 175.405, 0.6805, "SLOAN")
  type(hydrate_data), parameter :: Hyd6 = hydrate_data("C1", &
       3.14393, 155.593, 0.3834, "SLOAN")

  ! Jager et al. 2013: 10.1016/j.fluid.2012.10.017
  type(hydrate_data), parameter :: Hyd7 = hydrate_data("CO2", &
       2.9463252, 175.405, 0.6805, "JAGER")

  integer, parameter :: n_hyd_db = 7
  type(hydrate_data), dimension(n_hyd_db), parameter :: hyd_array = (/ &
       Hyd1, &
       Hyd2, &
       Hyd3, &
       Hyd4, &
       Hyd5, &
       Hyd6, &
       Hyd7/)

  type :: active_kihara_data
    logical :: comp_has_kihara_data = .false.
    real :: sigma
    real :: eps_depth_divk
    real :: alpha
  end type active_kihara_data

  type :: active_hydrate_param
    type(active_kihara_data), allocatable :: act_kihara(:)
    logical :: use_poynting_correction = .true.
  end type active_hydrate_param

  type(active_hydrate_param) :: act_hyd_param

  public :: init_hydrate_model
  public :: fugacity_water_in_hydrate_TPx
  public :: water_idx
  public :: fugacity_water_in_hydrate_TVn

  ! Debug:
  public :: poynting_water_ice, Ptriple
  public :: pure_water_melting_temperature
  public :: fugacity, get_langmuir_constants_kihara
  public :: kihara_integrand_diff, kihara_w_over_kT
  public :: calc_zero_kihara_integrand
  public :: test_fugacity_water_in_hydrate_TVn

contains

  !-----------------------------------------------------------------!
  subroutine init_hydrate_model(param_ref, use_solid_eos)
  !-----------------------------------------------------------------!
  ! Initialize hydrate model
    use saturation, only: safe_bubP
    use stringmod, only: str_eq
    use solideos, only: solid_init
    implicit none
    character(len=*), intent(in) :: param_ref
    logical, optional, intent(in) :: use_solid_eos
    ! Locals
    real, dimension(nc) :: x,y
    integer :: ierr, i
    write(*,*) "Initializing hydrate model."

    water_idx = -1
    do i=1,nc
      if (str_eq(complist(i),"H2O")) then
        water_idx = i
        exit
      end if
    end do
    if (water_idx < 0) then
      call stoperror('init_hydrate_model: Water not found among components.')
    end if

    ! Make pure component sublimation line meet saturation line
    ! in triple point. That is; triple point pressure modified.
    x = 0.0
    x(water_idx) = 1.0
    y = x
    Ptriple = safe_bubP(Ttriple,x,y,ierr)
    if (ierr /= 0) then
      call stoperror('init_hydrate_model: Not able to calculate triple pressure for water')
    end if

    ! Set Kihara potential parameters
    call set_kihara_potential_parameters(param_ref)

    ! Solid model
    if (present(use_solid_eos)) then
      use_solid_ice_eos = use_solid_eos
    else
      use_solid_ice_eos = .true.
    endif
    if (use_solid_ice_eos) then
      call solid_init("H2O")
    endif

    write(*,*) "Initialization done."
  end subroutine init_hydrate_model

  !-----------------------------------------------------------------!
  subroutine fugacity_water_in_hydrate(T,P,fug,fug_wh)
  !-----------------------------------------------------------------!
  ! Equation 4 in memo (doc/memo/hydrate)
  !
  ! Input:
  !   T [K]
  !   P [Pa]
  !   fug [Pa]
  ! Output:
  !   fug_wh [Pa]
    implicit none
    ! Input:
    real, intent(in)            :: T,P
    real, intent(in)            :: fug(nc)
    ! Output:
    real, intent(out)           :: fug_wh
    ! Internal:
    real                        :: fug_T(nc),fug_v(nc),fug_n(nc,nc)
    real                        :: dmu_wbh_over_RT,dmu_wbh_over_RT_T,dmu_wbh_over_RT_v,dmu_wbh_over_RT_n(nc)
    real                        :: fug_wb,fug_wb_T,fug_wb_P
    real                        :: exp_dmu
    real                        :: fug_w,fug_w_T,fug_w_P
    integer                     :: phase_w

    ! Get pure water (liquid/solid) fugacity
    call fug_pure_water(T,P,phase_w,fug_w,fug_w_T,fug_w_P)
    ! Getting fugacity of empty hydrate.
    call fugacity_empty_hydrate(T,P,phase_w,fug_wb,fug_wb_T,fug_wb_P)
    ! Getting difference in chemical potential between empty hydrate and filled hydrate.
    call delta_mu_wbh_over_RT(T,fug,fug_T,fug_v,fug_n,dmu_wbh_over_RT,dmu_wbh_over_RT_T,dmu_wbh_over_RT_v,dmu_wbh_over_RT_n)
    ! Get fugacity of hydrate
    exp_dmu = exp(-dmu_wbh_over_RT) ! Negative sign: Need hb, not bh.
    fug_wh = fug_w * fug_wb * exp_dmu

  end subroutine fugacity_water_in_hydrate

  !-----------------------------------------------------------------!
  subroutine fugacity_water_in_hydrate_TPx(T,P,x_fluid,phase_fluid,fug_wh)
  !-----------------------------------------------------------------!
  ! Equation 4 in memo (doc/memo/hydrate)
  !
  ! Evaluate  according to fluid variables TPx
  !
  ! Input:
  !   T [K]
  !   P [Pa]
  !   x_fluid [mol/mol]
  !   phase_fluid Fluid phase indicator
  ! Output:
  !   fug_wh [Pa]
  !
    implicit none
    ! Input:
    real, intent(in)            :: T,P,x_fluid(nc)
    integer, intent(in)         :: phase_fluid
    ! Output:
    real, intent(out)           :: fug_wh
    ! Internal:
    real                        :: fug(nc)

    ! Fluid phase fugacity
    call fugacity(T,P,x_fluid,phase_fluid,fug)
    !
    call fugacity_water_in_hydrate(T,P,fug,fug_wh)
  end subroutine fugacity_water_in_hydrate_TPx

  !-----------------------------------------------------------------!
  subroutine fugacity_water_in_hydrate_TVn(T,v,n_fluid,fug_wh,fug_wh_T,fug_wh_v,fug_wh_n)
  !-----------------------------------------------------------------!
  ! Equation 4 in memo (doc/memo/hydrate)
  !
  ! Evaluate and calculate differentials according to fluid variables TVn
  !
  ! Input:
  !   T [K]
  !   V [m3]
  !   n_fluid [mol]
  ! Output:
  !   fug_wh [Pa]
  !   fug_wh_T [Pa/K]
  !   fug_wh_v [Pa/m3]
  !   fug_wh_n(:) [Pa/mol]
  !
    use eostv, only: pressure, thermo_tv
    implicit none
    ! Input:
    real, intent(in)            :: T,V,n_fluid(nc)
    ! Output:
    real, intent(out) :: fug_wh
    real, optional, intent(out) :: fug_wh_T
    real, optional, intent(out) :: fug_wh_v
    real, optional, intent(out) :: fug_wh_n(nc)
    ! Internal:
    real :: fug(nc), fug_w, fug_wb, P
    real :: fug_T(nc), fug_w_T, fug_wb_T, P_T, fug_w_P, fug_wb_P
    real :: fug_v(nc), fug_w_v, fug_wb_v, P_v
    real :: fug_n(nc,nc), fug_w_n(nc), fug_wb_n(nc), P_n(nc)
    !
    real    :: dmu_wbh_over_RT,dmu_wbh_over_RT_T,dmu_wbh_over_RT_v
    real    :: dmu_wbh_over_RT_n(nc)
    real    :: exp_dmu, exp_dmu_T, exp_dmu_v, exp_dmu_n(nc)
    integer :: phase_w, i

    ! Fluid phase fugacity
    call thermo_tv(T,V,n_fluid,fug,fug_T,fug_v,fug_n)
    fug = exp(fug)
    fug_T = fug*fug_T
    fug_v = fug*fug_v
    do i=1,nc
      fug_n(i,:) = fug(i)*fug_n(i,:)
    enddo
    P = pressure(T,V,n_fluid,dpdv=P_v,dpdt=P_T,dpdn=P_n)
    if (P < 0.0) then
      fug_wh = 1000.0 ! Force solver to reduce step
      if (present(fug_wh_T)) fug_wh_T = 0
      if (present(fug_wh_v)) fug_wh_v = 0
      if (present(fug_wh_n)) fug_wh_n = 0
      return
    endif
    !
    ! Get pure water (liquid/solid) fugacity
    call fug_pure_water(T,P,phase_w,fug_w,fug_w_T,fug_w_P)
    fug_w_T = fug_w_T + fug_w_P*P_T
    fug_w_v = fug_w_P*P_v
    fug_w_n = fug_w_P*P_n
    !print *,"fug_w",fug_w
    !
    ! Getting fugacity of empty hydrate.
    call fugacity_empty_hydrate(T,P,phase_w,fug_wb,fug_wb_T,fug_wb_P)
    fug_wb_T = fug_wb_T + fug_wb_P*P_T
    fug_wb_v = fug_wb_P*P_v
    fug_wb_n = fug_wb_P*P_n
    !print *,"fug_wb",fug_wb
    !
    ! Getting difference in chemical potential between empty hydrate and filled hydrate.
    call delta_mu_wbh_over_RT(T,fug,fug_T,fug_v,fug_n,dmu_wbh_over_RT,&
         dmu_wbh_over_RT_T,dmu_wbh_over_RT_v,dmu_wbh_over_RT_n)
    ! Get fugacity of hydrate
    exp_dmu = exp(-dmu_wbh_over_RT) ! Negative sign: Need hb, not bh.
    exp_dmu_T = -exp_dmu*dmu_wbh_over_RT_T
    exp_dmu_v = -exp_dmu*dmu_wbh_over_RT_v
    exp_dmu_n = -exp_dmu*dmu_wbh_over_RT_n
    !print *,"exp_dmu",exp_dmu
    !
    fug_wh = fug_w * fug_wb * exp_dmu
    if (present(fug_wh_T)) then
      fug_wh_T = fug_w_T * fug_wb * exp_dmu + &
           fug_w * fug_wb_T * exp_dmu + &
           fug_w * fug_wb * exp_dmu_T
    endif
    if (present(fug_wh_v)) then
      fug_wh_v = fug_w_v * fug_wb * exp_dmu + &
           fug_w * fug_wb_v * exp_dmu + &
           fug_w * fug_wb * exp_dmu_v
    endif
    if (present(fug_wh_n)) then
      fug_wh_n = fug_w_n * fug_wb * exp_dmu + &
           fug_w * fug_wb_n * exp_dmu + &
           fug_w * fug_wb * exp_dmu_n
    endif
  end subroutine fugacity_water_in_hydrate_TVn

  !-----------------------------------------------------------------!
  subroutine test_fugacity_water_in_hydrate_TVn(T,v,n0)
  !-----------------------------------------------------------------!
  ! Test analytical fifferentials of fugacity_water_in_hydrate_TVn
  ! numerically
    implicit none
    ! Input:
    real, intent(in)            :: T,V,n0(nc)
    !
    real :: fug1_wh, fug2_wh,fug0_wh
    real :: fug1_wh_T, fug2_wh_T,fug0_wh_T
    real :: fug1_wh_v, fug2_wh_v,fug0_wh_v
    real :: fug1_wh_n(nc),fug2_wh_n(nc),fug0_wh_n(nc)
    real :: eps,dT,dV,n1(nc),dn
    integer :: i
    eps = 1.0e-5

    call fugacity_water_in_hydrate_TVn(T,v,n0,&
         fug0_wh,fug0_wh_T,fug0_wh_v,fug0_wh_n)
    dT = T*eps
    call fugacity_water_in_hydrate_TVn(T-dT,v,n0,&
         fug1_wh,fug1_wh_T,fug1_wh_v,fug1_wh_n)
    call fugacity_water_in_hydrate_TVn(T+dT,v,n0,&
         fug2_wh,fug2_wh_T,fug2_wh_v,fug2_wh_n)
    print *,"T"
    print *,"f_T",(fug2_wh-fug1_wh)/(2*dT), fug0_wh_T

    dv = v*eps
    call fugacity_water_in_hydrate_TVn(T,v-dv,n0,&
         fug1_wh,fug1_wh_T,fug1_wh_v,fug1_wh_n)
    call fugacity_water_in_hydrate_TVn(T,v+dv,n0,&
         fug2_wh,fug2_wh_T,fug2_wh_v,fug2_wh_n)
    print *,"v"
    print *,"f_v",(fug2_wh-fug1_wh)/(2*dv), fug0_wh_v

    do i=1,nc
      dn = n0(i)*eps
      n1 = n0
      n1(i) = n1(i) - dn
      call fugacity_water_in_hydrate_TVn(T,v,n1,&
           fug1_wh,fug1_wh_T,fug1_wh_v,fug1_wh_n)
      n1 = n0
      n1(i) = n1(i) + dn
      call fugacity_water_in_hydrate_TVn(T,v,n1,&
           fug2_wh,fug2_wh_T,fug2_wh_v,fug2_wh_n)
      print *,"n",i
      print *,"f_n",(fug2_wh-fug1_wh)/(2*dn), fug0_wh_n(i)
    enddo

  end subroutine test_fugacity_water_in_hydrate_TVn

  !-----------------------------------------------------------------!
  subroutine set_kihara_potential_parameters(param_ref)
  !-----------------------------------------------------------------!
  ! Find and set Kihara potential parameters according to reference input.
    use stringmod, only: str_eq, string_match_val
    implicit none
    ! Input:
    character(len=*), intent(in) :: param_ref
    ! Locals
    logical :: ref_match
    integer :: idx_lowest, match_val
    integer :: i, ki, j

    if (allocated(act_hyd_param%act_kihara)) deallocate(act_hyd_param%act_kihara)
    allocate(act_hyd_param%act_kihara(nc))

    do j=1,nc
      idx_lowest = 100000
      ki = 0
      do i=1,n_hyd_db
        if (str_eq(hyd_array(i)%compname, complist(j))) then
          call string_match_val(param_ref,hyd_array(i)%ref,ref_match,match_val)
          if (ref_match .and. match_val<idx_lowest) then ! the match takes precedence
            idx_lowest = match_val
            ki = i
          else if (ki < 1) then
            ! Set default values if no match is found
            call string_match_val("DEFAULT",hyd_array(i)%ref,ref_match,match_val)
            ki = i
          endif
        endif
      enddo
      if (ki > 0) then
        act_hyd_param%act_kihara(j)%comp_has_kihara_data = .true.
        act_hyd_param%act_kihara(j)%sigma = hyd_array(ki)%sigma
        act_hyd_param%act_kihara(j)%alpha = hyd_array(ki)%alpha
        act_hyd_param%act_kihara(j)%eps_depth_divk = hyd_array(ki)%eps_depth_divk
      else
        act_hyd_param%act_kihara(j)%comp_has_kihara_data = .false.
        act_hyd_param%act_kihara(j)%sigma = 0
        act_hyd_param%act_kihara(j)%alpha = 0
        act_hyd_param%act_kihara(j)%eps_depth_divk = 0
      endif
    enddo
  end subroutine set_kihara_potential_parameters

  !-----------------------------------------------------------------!
  subroutine kihara_integrand_diff(r,n,param,fun,fun_T,fun_r)
  !-----------------------------------------------------------------!
  ! Interface to calculate r squared multiplied with the exponential
  ! of Kihara cell potential divided by kT
  !
  ! Input:
  !   r [m]
  !   n Size of param array
  !   param(:) Parameters
  ! Output:
  !   fun [-]
  !   fun_T [1/K]
  !   fun_r [1/m]
  !
    implicit none
    ! Input:
    real, intent(in)    :: r
    integer, intent(in) :: n
    real, intent(in)    :: param(n)
    ! Output:
    real, intent(out)   :: fun, fun_T, fun_r
    ! Internal:
    real                :: w_over_kT,w_over_kT_r
    real                :: T_par

    T_par = param(6)
    call kihara_w_over_kT(r,n,param,w_over_kT,w_over_kT_r)
    fun = exp(-w_over_kT) * r**2
    fun_T = fun*w_over_kT/T_par
    fun_r = exp(-w_over_kT) * r * (2 - w_over_kT_r * r)
  end subroutine kihara_integrand_diff

  !-----------------------------------------------------------------!
  subroutine kihara_w_over_kT(r,n,param,w_over_kT,w_over_kT_r)
    !-----------------------------------------------------------------!
    ! Kihara cell potential divided by kT
    !
    ! Theory of Dissociation Pressures of Some Gas Hydrates
    ! V. McKoy and O. Sinanoğlu
    ! The Journal of Chemical Physics 38, 2946 (1963); doi: 10.1063/1.1733625
    !
    ! Input:
    !   r [m]
    !   n Size of param array
    !   param(:) Parameters
    ! Output:
    !   w_over_kT [-]
    !   w_over_kT_r [1/m]
    !
    implicit none
    ! Input:
    real, intent(in)    :: r
    integer, intent(in) :: n
    real, intent(in)    :: param(n)
    ! Output:
    real, intent(out)   :: w_over_kT
    real, optional, intent(out)   :: w_over_kT_r
    ! Internal:
    real                :: z_par,R_par,alpha_par,sigma_par,eps_over_k_par,T_par
    real                :: prefac
    z_par = param(1)
    R_par = param(2)
    alpha_par = param(3)
    sigma_par = param(4)
    eps_over_k_par = param(5)
    T_par = param(6)

    w_over_kT = 2.0*z_par*eps_over_k_par* ( &
         ( (sigma_par/r)*(sigma_par/R_par)**11 )*( delta(10,r,R_par,alpha_par) + (alpha_par/R_par)*delta(11,r,R_par,alpha_par) ) - &
         ( (sigma_par/r)*(sigma_par/R_par)**5  )*( delta( 4,r,R_par,alpha_par) + (alpha_par/R_par)*delta( 5,r,R_par,alpha_par) ) &
         )/T_par

    if (present(w_over_kT_r)) then
      prefac = 2.0*z_par*eps_over_k_par/T_par
      w_over_kT_r = &
           ( -(sigma_par/r**2)*(sigma_par/R_par)**11 )*( delta(10,r,R_par,alpha_par) +&
           (alpha_par/R_par)*delta(11,r,R_par,alpha_par) ) - &
           ( -(sigma_par/r**2)*(sigma_par/R_par)**5  )*( delta( 4,r,R_par,alpha_par) +&
           (alpha_par/R_par)*delta( 5,r,R_par,alpha_par) ) + &
           ( (sigma_par/r)*(sigma_par/R_par)**11 )*( delta_r(10,r,R_par,alpha_par) +&
           (alpha_par/R_par)*delta_r(11,r,R_par,alpha_par) ) - &
           ( (sigma_par/r)*(sigma_par/R_par)**5  )*( delta_r( 4,r,R_par,alpha_par) +&
           (alpha_par/R_par)*delta_r( 5,r,R_par,alpha_par) )
      w_over_kT_r = prefac*w_over_kT_r
    endif

  contains
    function delta(N,r,Rlarge,alpha)
      implicit none
      integer, intent(in) :: N
      real, intent(in) :: r, Rlarge, alpha
      real             :: delta
      delta = (   (1.0 - r/Rlarge - alpha/Rlarge)**(-N) &
           - (1.0 + r/Rlarge - alpha/Rlarge)**(-N)   )/N
    end function delta
    function delta_r(N,r,Rlarge,alpha)
      implicit none
      integer, intent(in) :: N
      real, intent(in) :: r, Rlarge, alpha
      real             :: delta_r
      delta_r = (   (1.0 - r/Rlarge - alpha/Rlarge)**(-N-1) &
           + (1.0 + r/Rlarge - alpha/Rlarge)**(-N-1)   )/Rlarge
    end function delta_r
  end subroutine kihara_w_over_kT

  subroutine calc_zero_kihara_integrand(n,param,r0,r0_T)
    !--------------------------------------------------------------------
    ! Calculate point where exp(-w(r))*r**2 = 0.0, numerically     !
    !
    !! \author Morten Hammer, October 2021
    !---------------------------------------------------------------------
    use nonlinear_solvers, only: nonlinear_solver, limit_dx, premReturn, &
         setXv, nonlinear_solve
    integer, intent(in) :: n
    real, intent(inout)    :: param(n)
    real, intent(out) :: r0                  !< r0 [m]
    real, intent(out) :: r0_T                !< T-deriv of r0 [m/K]
    ! Locals
    type(nonlinear_solver) :: solver
    real :: r(1), rmin(1), rmax(1), alpha, Rlarge, T
    real :: dfdr, dfdt, w_over_kT,w_over_kT_r
    r = 1.0
    rmin = 0.1
    alpha = param(3)
    Rlarge = param(2)
    rmax = Rlarge-2.0*alpha
    solver%rel_tol = 1.0e-13
    solver%max_it = 20
    solver%ls_max_it = 3

    Solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%limit_x_values = .true.
    solver%max_it = 20
    solver%ls_max_it = 3
    call nonlinear_solve(solver,zero_kihara_fun_newton,&
         zero_kihara_diff_newton,zero_kihara_diff_newton,&
         limit_dx,premReturn,setXv,r,rmin,rmax,param)
    if (solver%exitflag /= 0) then
       call stoperror("Not able to solve for point where d-integrand becomes zero")
    else
      r0 = r(1)
      call kihara_w_over_kT(r0,size(param),param,w_over_kT,w_over_kT_r)
      dfdr = -w_over_kT_r + 2/r0
      T = param(6)
      dfdT = w_over_kT/T
      r0_T = -dfdT/dfdr
    endif
  end subroutine calc_zero_kihara_integrand

  !-----------------------------------------------------------------------------
  !> \author Morten Hammer, 2021
  !-----------------------------------------------------------------------------
  subroutine zero_kihara_fun_newton(F,rvar,param)
    use numconstants, only: almost_zero
    implicit none
    real, dimension(1), intent(out) :: F !< Function values
    real, dimension(1), intent(in) :: Rvar !< Variable vector
    real, dimension(6), intent(in) :: param !< Parameter vector
    ! Locals
    real :: w_over_kT, r
    r = rvar(1)
    call kihara_w_over_kT(r,size(param),param,w_over_kT)
    F = -w_over_kT + 2*log(r) - log(almost_zero)
  end subroutine zero_kihara_fun_newton

  !-----------------------------------------------------------------------------
  !> \author Morten Hammer, 2021
  !-----------------------------------------------------------------------------
  subroutine zero_kihara_diff_newton(J,rvar,param)
    implicit none
    real, dimension(1,1), intent(out) :: J !< Function differentials
    real, dimension(1), intent(in) :: Rvar !< Variable vector
    real, dimension(6), intent(in) :: param !< Parameter vector
    ! Locals
    real :: w_over_kT, w_over_kT_r, r
    r = rvar(1)

    call kihara_w_over_kT(r,size(param),param,w_over_kT,w_over_kT_r)
    J = -w_over_kT_r + 2/r
  end subroutine zero_kihara_diff_newton

  !-----------------------------------------------------------------!
  subroutine get_langmuir_constants_kihara(T,C,C_T)
  !-----------------------------------------------------------------!
  ! Kihara potential function is used as described in
  ! McKoy and Sinanoglu 1963 (10.1063/1.1733625).
  !
  ! Input:
  !   T [K]
  ! Output:
  !   C(:,:) [1/Pa]
  !   C_T(:,:) [1/Pa/K]
  !
    use numconstants, only: pi
    use quadratures, only: integrate_with_differential_const_bound, &
         GAUSS_KRONROD_61, integrate_with_differential
    implicit none
    ! Input:
    real, intent(in)              :: T
    ! Output:
    real, intent(out)             :: C(:,:), C_T(:,:)
    ! Internal:
    integer                       :: i,j
    integer, parameter            :: n_param = 6
    real                          :: param(n_param), alpha, R, integral, I_T
    real                          :: r0,r0_T
    logical, parameter            :: estimate_quadrature_error = .false.
    !real :: integral2,I2_T
    do j=1,nc
      do i=1,n_cavities
        if (act_hyd_param%act_kihara(j)%comp_has_kihara_data) then
          param(1) = z_cav(i)
          R = R_cav(i)
          param(2) = R
          alpha = act_hyd_param%act_kihara(j)%alpha
          param(3) = alpha
          param(4) = act_hyd_param%act_kihara(j)%sigma
          param(5) = act_hyd_param%act_kihara(j)%eps_depth_divk
          param(6) = T
          call calc_zero_kihara_integrand(n_param,param,r0,r0_T)
          call integrate_with_differential(GAUSS_KRONROD_61,kihara_integrand_diff,n_param,param,&
               0.0,r0,0.0,r0_T,estimate_quadrature_error,integral,I_T)
          C(j,i) = (4.0*pi)/(kB_const*T)*integral*(1e-30) !Last factor: Å^3/J -> m^3/J = 1/Pa
          C_T(j,i) = (4.0*pi*1e-30/kB_const)*(-integral/T**2 + I_T/T)
        else
          C(j,i) = 0
          C_T(j,i) = 0
        end if
      end do
    end do

  end subroutine get_langmuir_constants_kihara

  !-----------------------------------------------------------------!
  subroutine fugacity(T,P,x,phase,fug,fug_T,fug_P)
  !-----------------------------------------------------------------!
  !
  ! Returns fluid fugacity of components, given that the phase is known.
  ! Input:
  !   T [K]
  !   P [Pa]
  !   x(:) []
  !   phase []
  ! Output:
  !   fug(:) [Pa]
  !   fug_T(:) [Pa/K]
  !   fug_P(:) [-]
  !
    use eos, only: thermo
    implicit none
    ! Input:
    real, intent(in)              :: T,p,x(nc)
    integer, intent(in)           :: phase
    ! Output:
    real, intent(out)             :: fug(nc)
    real, optional, intent(out)   :: fug_T(nc), fug_P(nc)
    ! Internal:
    real :: lnphi(nc),lnphi_t(nc)
    real :: lnphi_p(nc)

    call thermo(t,p,x,phase,lnphi,lnphi_t,lnphi_p)
    fug = exp(lnphi + log(P))
    if (present(fug_T)) then
      fug_T = fug*lnphi_T
    endif
    if (present(fug_P)) then
      fug_P = fug*(lnphi_P + 1/P)
    endif

  end subroutine fugacity

  !-----------------------------------------------------------------!
  subroutine fug_pure_water(T,P,phase,fug_w,fug_w_T,fug_w_P)
  !-----------------------------------------------------------------!
  ! Equation 5 in memo (doc/memo/hydrate)
  !
  ! Input:
  !   T [K]
  !   P [Pa]
  ! Output:
  !   phase Stable water phase indicator
  !   fug_w [Pa]
  !   fug_w_T [Pa/K]
  !   fug_w_P [-]
  !
    implicit none
    ! Input:
    real, intent(in)            :: T,P
    ! Output:
    real, intent(out)           :: fug_w
    integer, intent(out)        :: phase
    real, optional, intent(out) :: fug_w_T
    real, optional, intent(out) :: fug_w_P
    ! Internal:
    real                        :: x(nc), fug(nc), fug_T(nc), fug_P(nc)
    real                        :: fug_ws, fug_ws_T,fug_ws_P

    if (T > Ttriple - 10.0) then
      ! Liquid water fugacity
      x = 0
      x(water_idx) = 1
      call fugacity(T,P,x,LIQPH,fug,fug_T,fug_P)
      fug_w = fug(water_idx)
      if (present(fug_w_T)) then
        fug_w_T = fug_T(water_idx)
      endif
      if (present(fug_w_P)) then
        fug_w_P = fug_P(water_idx)
      endif
      phase = LIQPH
    else
      fug_w = 1e10
    endif
    !
    if (T < Ttriple + 10.0) then
      ! Solid fugacity
      call solid_fugacity_water(T,P,fug_ws,fug_ws_T,fug_ws_P)
      if (fug_ws < fug_w) then
        phase = SOLIDPH
        fug_w = fug_ws
        if (present(fug_w_T)) then
          fug_w_T = fug_ws_T
        endif
        if (present(fug_w_P)) then
          fug_w_P = fug_ws_P
        endif
      endif
    endif
  end subroutine fug_pure_water

  !-----------------------------------------------------------------!
  subroutine solid_fugacity_water(T,P,fug_w,fug_w_T,fug_w_P)
  !-----------------------------------------------------------------!
  ! Input:
  !   T [K]
  !   P [Pa]
  ! Output:
  !   fug_w [Pa]
  !   fug_w_T [Pa/K]
  !   fug_w_P [-]
  !
    use solideos, only: solid_thermo
    implicit none
    ! Input:
    real, intent(in)            :: T,P
    ! Output:
    real, intent(out)           :: fug_w
    real, optional, intent(out) :: fug_w_T
    real, optional, intent(out) :: fug_w_P
    ! Internal:
    real :: x(nc)

    if (use_solid_ice_eos) then
      x = 0
      x(water_idx) = 1
      call solid_thermo(T,P,x,fug_w,fug_w_T,fug_w_P)
      fug_w = exp(fug_w)*P
      if (present(fug_w_T)) then
        fug_w_T = fug_w*fug_w_T
      endif
      if (present(fug_w_P)) then
        fug_w_P = fug_w*(fug_w_P + 1/P)
      endif
    else
      call poynting_water_ice(T,P,fug_w,fug_w_T,fug_w_P)
    endif

  end subroutine solid_fugacity_water

  !-----------------------------------------------------------------!
  subroutine poynting_water_ice(T,P,fug_w,fug_w_T,fug_w_P)
  !-----------------------------------------------------------------!
  ! Input:
  !   T [K]
  !   P [Pa]
  ! Output:
  !   fug_w [Pa]
  !   fug_w_T [Pa/K]
  !   fug_w_P [-]
  !
    implicit none
    ! Input:
    real, intent(in)            :: T,P
    ! Output:
    real, intent(out)           :: fug_w
    real, optional, intent(out) :: fug_w_T
    real, optional, intent(out) :: fug_w_P
    ! Internal:
    real, parameter             :: a1=-13.9281690, a2=34.7078238
    real                        :: x(nc),fug(nc),fug_T(nc),fug_P(nc)
    real                        :: t_s,P_sat,coeff_pure_water,vi
    real                        :: P0, T0
    real                        :: C_T, C_P, vi_T, P_sat_T
    real                        :: exp_pdiff, exp_pdiff_T, exp_pdiff_P
    x = 0
    x(water_idx) = 1
    ! Poynting correction using sublimation curve from
    ! Wagner (1994): 10.1063/1.555947
    T0 = Ttriple
    P0 = Ptriple
    t_s = T/T0
    P_sat = P0*exp(a1*(1-t_s**(-1.5))+a2*(1-t_s**(-1.25)))
    P_sat_T = P_sat*(1.5*a1*t_s**(-2.5)+a2*+1.25*t_s**(-2.25))/T0
    vi_T = 2.2364*10.0**(-9.0)
    vi = 19.629*10.0**(-6.0)+vi_T*(T-T0)
    if(T < 223.15) then
      coeff_pure_water = 1.0  ! Almost ideal
      C_T = 0
      C_P = 0
    else
      call fugacity(T,P_sat,x,VAPPH,fug,fug_T,fug_P)
      coeff_pure_water = fug(water_idx)/P_sat
      fug_T = fug_T + fug_P*P_sat_T
      C_T = fug_T(water_idx)/P_sat - P_sat_T*coeff_pure_water/P_sat
      C_P = 0
    endif
    exp_pdiff = exp(vi*(P-P_sat)/(Rgas*T))
    fug_w=coeff_pure_water*P_sat*exp_pdiff

    if (present(fug_w_T)) then
      exp_pdiff_T = -exp_pdiff*(vi*(P-P_sat)/(Rgas*T**2))
      fug_w_T = C_T*P_sat*exp_pdiff + &
           coeff_pure_water*P_sat_T*exp_pdiff + &
           coeff_pure_water*P_sat*exp_pdiff_T
    endif
    if (present(fug_w_P)) then
      exp_pdiff_P = exp_pdiff*(vi/(Rgas*T))
      fug_w_P = C_P*P_sat*exp_pdiff + &
           coeff_pure_water*P_sat*exp_pdiff_P
    endif
  end subroutine poynting_water_ice

  !-----------------------------------------------------------------!
  subroutine pure_water_melting_temperature(P,T,ierr)
  !-----------------------------------------------------------------!
    use nonlinear_solvers, only: NS_PEGASUS, bracketing_solver, &
         nonlinear_solver
    implicit none
    ! Input:
    real, intent(in)            :: P
    ! Output:
    real, intent(out)           :: T
    integer, intent(out)        :: ierr
    ! Internal:
    type(nonlinear_solver)      :: solver
    real, dimension(1) :: param
    param(1) = P
    solver%abs_tol = 1.0e-8
    solver%max_it = 1000
    solver%isolver = NS_PEGASUS
    call bracketing_solver(Ttriple-50.0,Ttriple+5.0,fun_melting_pure_water,T,&
         solver,param)
  end subroutine pure_water_melting_temperature

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on melting curve
  !>
  !> \author MH 2021
  !-----------------------------------------------------------------------------
  function fun_melting_pure_water(T,param) result(fun)
    implicit none
    real, intent(in) :: T
    real, dimension(1), intent(in) :: param
    real :: fun
    ! Locals:
    real :: P,fug_ws,fug_wl(nc),x(nc)
    P = param(1)
    x = 0
    x(water_idx) = 1
    call fugacity(T,P,x,LIQPH,fug_wl)
    call solid_fugacity_water(T,P,fug_ws)
    fun = log(fug_wl(water_idx)) - log(fug_ws)
  end function fun_melting_pure_water

  !-----------------------------------------------------------------!
  subroutine fugacity_empty_hydrate(T,P,phase_w,fug_wb,fug_wb_T,fug_wb_P)
  !-----------------------------------------------------------------!
  ! Equation 6 in memo (doc/memo/hydrate)
  !
  ! Input:
  !   T [K]
  !   P [Pa]
  !   phase_w Stable phase for pure water
  ! Output:
  !   fug_wb [Pa]
  !   fug_wb_T [Pa/K]
  !   fug_wb_P [-]
  !
    implicit none
    ! Input:
    real, intent(in)            :: T,P
    integer, intent(in)         :: phase_w
    ! Output:
    real, intent(out)           :: fug_wb
    real, optional, intent(out) :: fug_wb_T, fug_wb_P
    ! Internal:
    real                        :: dmu_over_RT,&
                                   dmu0, dH0, dV,dCp_coeff1,dCp_coeff2,&
                                   T0,P0,int_dH_RT2,dH_RT2

    ! Reference states for term 2,
    ! and heat capacity difference (Holder 1980).
    T0 = Ttriple
    P0 = Ptriple
    dmu0 = 1297.0
    if (T < T0) then ! SOLID - Not consistent with pure water fugacity
      dH0 = 1389.0
      dV = 3.0e-6
      dCp_coeff1 = 0.565
      dCp_coeff2 = 0.002
    else
      dH0 = -4620.5
      dV = 4.601e-6
      dCp_coeff1 = -37.32
      dCp_coeff2 = 0.179
    endif
    ! The change in dV give rise to a discontinuity of the fugacity of
    ! the hydrate

    call int_dH_over_RT2(T,T0,Rgas,dCp_coeff1,dCp_coeff2,dH0,int_dH_RT2,dH_RT2)

    dmu_over_RT = dmu0/(Rgas*T0) &
                  - int_dH_RT2 &
                  + (dV/(Rgas*T))*(P-P0)

    fug_wb = exp(dmu_over_RT)
    fug_wb_T = fug_wb*(-dH_RT2 - (dV/(Rgas*T**2))*(P-P0))
    fug_wb_P = fug_wb*dV/(Rgas*T)

    contains
      subroutine int_dH_over_RT2(T,T0,R,C1,C2,dH0,int_dH_RT2,dH_RT2)
        implicit none
        real, intent(in)  :: T,T0,R,C1,C2,dH0
        real, intent(out) :: int_dH_RT2, dH_RT2
        real              :: T_term,T0_term

        T_term  = -dH0/T  + (C1-C2*T0)*log(T) + 0.5*C2*T + C1*T0/T - 0.5*C2*(T0**2)/(T)
        T0_term = -dH0/T0 + (C1-C2*T0)*log(T0)           + C1

        int_dH_RT2 = (T_term-T0_term)/R

        dH_RT2 = (dH0/T**2  + (C1-C2*T0)/T + 0.5*C2 - C1*T0/T**2 + 0.5*C2*(T0**2)/T**2)/R

      end subroutine int_dH_over_RT2

  end subroutine fugacity_empty_hydrate

  !-----------------------------------------------------------------!
  subroutine delta_mu_wbh_over_RT(T,fug,fug_T,fug_v,fug_n,dmu_wbh_over_RT,&
       dmu_wbh_over_RT_T,dmu_wbh_over_RT_v,dmu_wbh_over_RT_n)
  !-----------------------------------------------------------------!
  ! Equation 9 (divided by RT) in memo doc/memo/hydrate
  !
  ! Input:
  !   T [K]
  !   fug(:) [Pa]
  !   fug_T(:) [Pa/K]
  !   fug_v(:) [Pa/m3]
  !   fug_n(:,:) [Pa/mol]
  ! Output:
  !   dmu_wbh_over_RT [-]
  !   dmu_wbh_over_RT_T [1/K]
  !   dmu_wbh_over_RT_v [1/m3]
  !   dmu_wbh_over_RT_n(:) [1/mol]
  !
    implicit none
    ! Input:
    real, intent(in)            :: T,fug(nc),fug_T(nc),fug_v(nc),fug_n(nc,nc)
    ! Output:
    real, intent(out)           :: dmu_wbh_over_RT, dmu_wbh_over_RT_T
    real, intent(out)           :: dmu_wbh_over_RT_v, dmu_wbh_over_RT_n(nc)
    ! Internal:
    integer                     :: i,j,k
    real           :: theta(nc,n_cavities), denom(n_cavities), sum_theta(n_cavities), C(nc,n_cavities)
    real           :: theta_T(nc,n_cavities), denom_T(n_cavities), sum_theta_T(n_cavities), C_T(nc,n_cavities)
    real           :: theta_v(nc,n_cavities), denom_v(n_cavities), sum_theta_v(n_cavities)
    real           :: theta_n(nc,n_cavities,nc), denom_n(n_cavities,nc), sum_theta_n(n_cavities,nc)

    ! Get Langmuir constants
    call get_langmuir_constants_kihara(T,C,C_T)

    ! Get denominator of theta
    denom = 1.0
    denom_T = 0
    denom_v = 0
    denom_n = 0
    do i=1,n_cavities
      do j=1,nc
        denom(i) = denom(i) + C(j,i)*fug(j)
        denom_T(i) = denom_T(i) + C_T(j,i)*fug(j) + C(j,i)*fug_T(j)
        denom_v(i) = denom_v(i) + C(j,i)*fug_v(j)
        denom_n(i,:) = denom_n(i,:) + C(j,i)*fug_n(j,:)
      end do
    end do

    ! Get theta
    do j=1,nc
      if (j==water_idx) then
        theta(j,:) = 0
        theta_T(j,:) = 0
        theta_v(j,:) = 0
        theta_n(j,:,:) = 0
      else
        do i=1,n_cavities
          theta(j,i) = C(j,i)*fug(j)/denom(i)
          theta_T(j,i) = (C_T(j,i)*fug(j)+C(j,i)*fug_T(j))/denom(i) - theta(j,i)*denom_T(i)/denom(i)
          theta_v(j,i) = C(j,i)*fug_v(j)/denom(i) - theta(j,i)*denom_v(i)/denom(i)
          theta_n(j,i,:) = C(j,i)*fug_n(j,:)/denom(i) - theta(j,i)*denom_n(i,:)/denom(i)
        end do
      end if
    end do

    ! Get sum of theta for each cavity, excluding water molecules (which are zero)
    do i=1,n_cavities
      sum_theta(i) = sum(theta(:,i))
      sum_theta_T(i) = sum(theta_T(:,i))
      sum_theta_v(i) = sum(theta_v(:,i))
      do k=1,nc
        sum_theta_n(i,k) = sum(theta_n(:,i,k))
      enddo
    end do

    ! Get chemical potential difference between empty hydrate and filled hydrate.
    dmu_wbh_over_RT = 0
    dmu_wbh_over_RT_T = 0
    dmu_wbh_over_RT_v = 0
    dmu_wbh_over_RT_n = 0
    do i=1,n_cavities
      dmu_wbh_over_RT = dmu_wbh_over_RT - v_cav(i)*log(1-sum_theta(i))
      dmu_wbh_over_RT_T = dmu_wbh_over_RT_T + v_cav(i)*sum_theta_T(i)/(1-sum_theta(i))
      dmu_wbh_over_RT_v = dmu_wbh_over_RT_v + v_cav(i)*sum_theta_v(i)/(1-sum_theta(i))
      dmu_wbh_over_RT_n(:) = dmu_wbh_over_RT_n(:) + v_cav(i)*sum_theta_n(i,:)/(1-sum_theta(i))
    end do

  end subroutine delta_mu_wbh_over_RT

end module hydrate
