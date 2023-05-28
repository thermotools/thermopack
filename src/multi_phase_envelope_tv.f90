!-------------------------------------------------------------------------
!> Module for plotting LLVE phase lines
!>
!> \author Morten Hammer
!-------------------------------------------------------------------------
module multi_phase_envelope_tv
  use eos, only: thermo, specificvolume
  use eostv, only: thermo_tv, pressure
  use thermopack_constants, only: LIQPH, VAPPH, SINGLEPH, &
       MINGIBBSPH, WATER, NONWATER, clen, verbose
  use nonlinear_solvers
  use thermopack_var, only: nc, thermo_model, get_active_eos, &
       get_active_thermo_model, Rgas, tpTmax, tpTmin
  use numconstants, only: machine_prec, small
  use thermo_utils, only: isSingleComp, wilsonK, wilsonKdiff, &
       calcLnPhiOffset, waterComponentFraction
  use saturation, only: sat_WilsonK, sat_newton, sat_successive
  !     , ER_NMAX, ER_NOSOL, ER_NOINITIALPOINT
  use saturation_tv, only: envelope_plot_control_tv, debug_tv, &
       get_step_direction_tv
  use saturation_curve, only: ER_STABILITY, ER_PMAX_TMIN, isStable, &
       envelope_plot_control
  use cubic_eos, only: get_b_linear_mix
implicit none
private
save

integer, parameter :: max_iter = 20!, max_nr_line_s = 10
integer, parameter :: numberOfPartialSteps = 5
integer, parameter :: nmax_static = 10000
logical, parameter :: mpe_tv_verbose = .false.

type :: three_phase_line_point
  real :: T
  real :: p
  real :: beta
  real :: vy, vx, vw
  real, allocatable, dimension(:) :: W, X, Y
contains
  procedure, public :: print => three_phase_line_point_print
end type three_phase_line_point

type :: three_phase_line
  type(three_phase_line_point), allocatable, dimension(:) :: tpl
  integer :: n = 0
  integer :: nAlloc = 0
  logical :: end_in_tpp = .false.
  logical :: start_in_tpp = .false.
  integer :: i_tpp_end = 0
  integer :: i_tpp_start = 0
  logical :: inverted_print = .false.
contains
  procedure, public :: map_to_output => three_phase_line_map_to_output
  procedure, public :: dealloc => three_phase_line_dealloc
  procedure, public :: push_back => three_phase_line_push_back
end type three_phase_line

type :: three_phase_line_pointer
  class(three_phase_line), pointer :: tplp => NULL()
end type three_phase_line_pointer

type, extends(three_phase_line) :: two_phase_line
  logical :: is_LL = .false.
  real, dimension(:), allocatable :: T,P, beta1
  real, dimension(:,:), allocatable :: X1, X2
  real, dimension(:,:), allocatable :: K
  real, dimension(:), allocatable :: v1a,v2a
contains
  procedure, public :: alloc => two_phase_line_alloc
  procedure, public :: dealloc => two_phase_line_dealloc
  procedure, public :: set_x_from_K => two_phase_line_set_x_from_K
  procedure, public :: copy_to_three_phase_structure => two_phase_line_copy_to_three_phase_structure
  procedure, public :: append => two_phase_line_append
  procedure, public :: overwrite_last_point => two_phase_line_overwrite_last_point
end type two_phase_line

type :: three_phase_point
  real, dimension(:), allocatable :: W,X
  real :: t, p
  real :: vz, vx, vw
contains
  procedure, public :: print => three_phase_point_print
end type three_phase_point

public :: multi_phase_envelope_plot_tv
public :: three_ph_line_newton, read_Xvar_and_param_tv
public :: extrapolate_three_ph_line_tv
public :: fill_Xvar_and_param_tv, print_Xvar_tv
! Debugging
public :: three_ph_newton_tv, three_ph_newton_tv_extrapolate_test
contains

  ! subroutine test()
  !   implicit none
  !   !
  !   real :: XX(2*nc+3), XXold(2*nc+3), dXds(2*nc+3), param(nc+6), sgn, dlns, lns
  !   integer :: ierr, s
  !   XXold = (/10.812020730895949,       -13.026717633881230,&
  !        -25.252845401603654,       -31.699094587384796,&
  !        4.5611024609059951,       -1.0976359645190774,&
  !        4.9956411425992915,        6.2652983887393665,&
  !        5.5056366599729003,        15.346929715509246,&
  !        0.75144875730835614/)
  !   dXds = (/-0.45382005187542762,       0.33178950690963260,&
  !        0.77701853042843805,        1.0000000000000000,&
  !        -0.22966005184467531,        3.1890966690849873E-004,&
  !        -0.24576137083873748,      -0.29906015759261573,&
  !        2.0885304211484053E-002,   5.9022088889071560E-002,&
  !        3.2532224972435595E-004/)
  !   sgn = -1.0000000000000000
  !   dlns = 4.4999999999999998E-002
  !   param = (/0.25000000000000000,       0.25000000000000000,&
  !        0.25000000000000000,       0.25000000000000000,&
  !        -31.789094587384795,        4.0000000000000000,&
  !        0.0000000000000000,        2.0000000000000000,&
  !        1.0000000000000000,        1.0000000000000000/)
  !   XX = XXold + dXds*sgn*dlns
  !   s = 4
  !   lns = XX(s)
  !   call three_ph_line_point(param,XXold,XX,dXds,dlns,s,lns,sgn,ierr)
  !   print *,ierr
  !   stop
  ! end subroutine test

  !-----------------------------------------------------------------------------
  !> Wrapper for solver of three phase point
  !> Do iterations with partial jacobian before using full jacobian
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine wrap_three_ph_point(Z,W,X,t,p,vZ,vX,vW)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(inout) :: W,X
    real, intent(inout) :: vZ,vX,vW
    real, intent(inout) :: p
    real, intent(inout) :: t
    !
    integer :: ierr
    ierr = 0
    call three_ph_newton_tv(Z,W,X,t,p,vZ,vX,vW,&
         .true.,numberOfPartialSteps,ierr)
    if (ierr /= 0) then
      call three_ph_newton_tv(Z,W,X,t,p,vZ,vX,vW,&
           .false.,max_iter,ierr)
    endif
    if (ierr /= 0) then
      call stoperror('wrap_three_ph_point: Not converged')
    endif
  end subroutine wrap_three_ph_point

  !-----------------------------------------------------------------------------
  !> Solve for three phase point using NR solver
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_newton_tv(Z,W,X,t,p,vZ,vX,vW,&
       partial,niter,ierr)
    use utilities, only: isXwithinBounds
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(inout) :: W,X
    real, intent(inout) :: p
    real, intent(inout) :: t
    real, intent(inout) :: vZ,vX,vW
    logical, intent(in) :: partial
    integer, intent(in) :: niter
    integer, intent(out) :: ierr
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(nc+1) :: param
    real, dimension(2*nc+4) :: Ymax, Ymin, Y
    ! real :: G(2*nc+4), J(2*nc+4,2*nc+4)
    ! real :: G1(2*nc+4), Y0(2*nc+4), dY
    ! integer :: i
    Y(1:nc) = W
    Y(1+nc:2*nc) = X
    Y(2*nc+1) = log(t)
    Y(2*nc+2) = log(vZ)
    Y(2*nc+3) = log(vX)
    Y(2*nc+4) = log(vW)

    param(1:nc) = Z
    if (partial) then
      param(nc+1) = 1.0
    else
      param(nc+1) = 0.0
    endif

    ! Test differentials
    ! Y0 = Y
    ! call three_ph_fun_newton(G,Y0,param)
    ! call three_ph_diff_newton(J,Y0,param)
    ! print *,""
    ! !T
    ! Y(2*nc+1) = Y(2*nc+1) + 1.0e-5
    ! call three_ph_fun_newton(G1,Y,param)
    ! print *,'dGdT',(G1-G)/1.0e-5
    ! print *,'dGdT',J(:,2*nc+1)
    !vz
    ! Y = Y0
    ! Y(2*nc+2) = Y(2*nc+2) + 1.0e-5
    ! call three_ph_fun_newton(G1,Y,param)
    ! print *,'dGdlnvz',(G1-G)/1.0e-5
    ! print *,'dGdlnvz',J(:,2*nc+2)
    !vx
    ! Y = Y0
    ! Y(2*nc+3) = Y(2*nc+3) + 1.0e-5
    ! call three_ph_fun_newton(G1,Y,param)
    ! print *,'dGdlnvx',(G1-G)/1.0e-5
    ! print *,'dGdlnvx',J(:,2*nc+3)
    !vw
    ! Y = Y0
    ! Y(2*nc+4) = Y(2*nc+4) + 1.0e-5
    ! call three_ph_fun_newton(G1,Y,param)
    ! print *,'dGdlnvy',(G1-G)/1.0e-5
    ! print *,'dGdlnvy',J(:,2*nc+4)
    !n1
    ! do i=1,nc
    !   Y = Y0
    !   dY = Y(i)*1.0e-5
    !   Y(i) = Y(i) + dY
    !   call three_ph_fun_newton(G1,Y,param)
    !   print *,'dGdn1',(G1-G)/dY
    !   print *,'dGdn1',J(:,i)
    ! enddo
    !n1+nc
    ! do i=1,nc
    !   Y = Y0
    !   dY = Y(i+nc)*1.0e-5
    !   Y(i+nc) = Y(i+nc) + dY
    !   call three_ph_fun_newton(G1,Y,param)
    !   print *,'dGdn1',(G1-G)/dY
    !   print *,'dGdn1',J(:,i+nc)
    ! enddo
    ! stop

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%limit_x_values = .true.
    solver%max_it = niter
    solver%ls_max_it = 3

    call sat_var_tv_limits(Z,X,W,Ymin,Ymax,include_beta=.false.)
    call isXwithinBounds(2*nc+4,Y,Ymin,Ymax,"",&
         "three_ph_newton_tv: Initial values not within bounds!!")
    call nonlinear_solve(solver,three_ph_fun_newton,three_ph_diff_newton,&
         three_ph_diff_newton,limit_dn,premReturn,setXv,Y,Ymin,Ymax,param)
    ierr = solver%exitflag
    !print *,ierr,solver%error_on_exit,solver%iter
    W = Y(1:nc)
    X = Y(1+nc:2*nc)
    t = exp(Y(2*nc+1))
    vZ = exp(Y(2*nc+2))
    vX = exp(Y(2*nc+3))
    vW = exp(Y(2*nc+4))
    p = pressure(T,vZ,Z)
  end subroutine three_ph_newton_tv

  !> Saturation variable limits
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine sat_var_tv_limits(Z1,Z2,Z3,Xmin,Xmax,include_beta)
    use numconstants, only: expMax, expMin
    use eosdata, only: eosCPA
    implicit none
    real, dimension(nc), intent(in) :: Z1, Z2, Z3 !< Composition
    logical, intent(in) :: include_beta
    real, dimension(:), intent(out) :: Xmin, Xmax !< Variable vector
    ! Locals
    real :: b(3)
    integer :: n_nin_v, n
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    if (include_beta) then
      Xmin = expMin
      Xmax = expMax
    else
      Xmin = 0
      Xmax = 2.0
    endif
    Xmin(2*nc+1) = log(tpTmin) !Tmin
    Xmax(2*nc+1) = log(tpTmax) !Tmax
    !
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      b = 1.0e-7
    else
      b(1) = get_b_linear_mix(z1) + Small ! m3/mol
      b(2) = get_b_linear_mix(z2) + Small ! m3/mol
      b(3) = get_b_linear_mix(z3) + Small ! m3/mol
    endif
    if (include_beta) then
      Xmin(2*nc+2) =  0 !beta min
      Xmax(2*nc+2) =  1 !beta max
      n_nin_v = 2*nc+3
    else
      n_nin_v = 2*nc+2
    endif
    n = size(Xmin)
    Xmin(n_nin_v:n) = log(b) !v min
    Xmax(n_nin_v:n) = log(100.0) !v max
  end subroutine sat_var_tv_limits

  !-----------------------------------------------------------------------------
  !> Three phase point function values for non-linear solver
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_fun_newton(G,Yvar,param)
    implicit none
    real, dimension(2*nc+4), intent(out) :: G !< Function values
    real, dimension(2*nc+4), intent(in) :: Yvar !< Variable vector
    real, dimension(nc+1) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, X, W
    real, dimension(nc) :: lnFugZ, lnFugX, lnFugW
    integer :: i
    real :: t, vZ, vX, vW, pZ, pX, pW

    Z = param(1:nc)
    W = Yvar(1:nc)
    X = Yvar(1+nc:2*nc)
    t = exp(Yvar(2*nc+1))
    vZ = exp(Yvar(2*nc+2))
    vX = exp(Yvar(2*nc+3))
    vW = exp(Yvar(2*nc+4))

    call thermo_tv(t,vZ,Z,lnFugZ)
    call thermo_tv(t,vW,W,lnFugW)
    call thermo_tv(t,vX,X,lnFugX)
    ! Function value
    do i=1,nc
      if (Z(i) > 0.0) then
        G(i) = lnFugW(i) - lnFugZ(i)
        G(nc+i) = lnFugX(i) - lnFugZ(i)
      else
        G(i) = 0.0
        G(nc+i) = 0.0
      endif
    enddo
    G(2*nc+1) = sum(W) - 1.0
    G(2*nc+2) = sum(X) - 1.0
    !
    pZ = pressure(t,vZ,Z)
    pW = pressure(t,vW,W)
    pX = pressure(t,vX,X)
    G(2*nc+3) = vZ*(pW-pZ)/(Rgas*T)
    G(2*nc+4) = vZ*(pX-pZ)/(Rgas*T)

  end subroutine three_ph_fun_newton

  !-------------------------------------------------------------------------
  !> Limit change in mole numbers, to maintain positive mole numbers.
  !>
  !> \author Morten Hammer, 2016
  !-------------------------------------------------------------------------
  subroutine limit_dn(n,x,xmin,xmax,dx,np,param)
    implicit none
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: x,xmin,xmax
    real, dimension(n),     intent(inout) :: dx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: param
    ! Locals
    real :: scaling
    integer :: i

    scaling = 1.0
    do i=1,n
      if (x(i)+dx(i) < xmin(i) .AND. abs(dx(i)) > 1.0e-9) then
        scaling = min(scaling,(xmin(i)-x(i))/dx(i))
      endif
      if (x(i)+dx(i) > xmax(i) .AND. abs(dx(i)) > 1.0e-9) then
        scaling = min(scaling,(xmax(i)-x(i))/dx(i))
      endif
    enddo
    !
    if (scaling < 1.0) then
      dx = dx * scaling * (1.0 - small)
    endif

  end subroutine limit_dn

  !-----------------------------------------------------------------------------
  !> Differentials for three phase point function values for non-linear solver
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_diff_newton(Jac,Yvar,param)
    implicit none
    real, dimension(2*nc+4), intent(in) :: Yvar !< Variable vector
    real, dimension(2*nc+4,2*nc+4), intent(out) :: Jac !< Function differentials
    real, dimension(nc+1) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, X, W
    real, dimension(nc) :: lnFugZ, lnFugX, lnFugW
    real, dimension(nc) :: lnFugXT, lnFugWT, lnFugXv, lnFugWv
    real, dimension(nc) :: lnFugZT, lnFugZv
    real, dimension(nc,nc) :: lnFugXn, lnFugWn
    real :: dpdvZ,dpdtZ,dpdnZ(nc)
    real :: dpdvX,dpdtX,dpdnX(nc)
    real :: dpdvW,dpdtW,dpdnW(nc)
    integer :: i
    real :: t, vZ, vX, vW, Pz, Px, Pw, fac
    logical :: partial
    Z = param(1:nc)
    partial = (int(param(1+nc)) /= 0)
    W = Yvar(1:nc)
    X = Yvar(1+nc:2*nc)
    t = exp(Yvar(2*nc+1))
    vZ = exp(Yvar(2*nc+2))
    vX = exp(Yvar(2*nc+3))
    vW = exp(Yvar(2*nc+4))

    call thermo_tv(t,vZ,Z,lnFugZ,lnFugZT,lnFugZv)
    call thermo_tv(t,vW,W,lnFugW,lnFugWT,lnFugWv,lnFugWn)
    call thermo_tv(t,vX,X,lnFugX,lnFugXT,lnFugXv,lnFugXn)

    Jac = 0.0
    ! W and X differentials
    if (partial) then
      do i=1,nc
        if (Z(i) > 0.0) then
          Jac(i,i) = Jac(i,i) + 1.0/W(i)
          Jac(nc+i,nc+i) = Jac(nc+i,nc+i) + 1.0/X(i)
        else
          Jac(i,i) = 1.0
          Jac(nc+i,nc+i) = 1.0
        endif
      enddo
    else
      Jac(1:nc,1:nc) = lnFugWn
      Jac(nc+1:2*nc,nc+1:2*nc) = lnFugXn
      ! Dummy equations?
      do i=1,nc
        if (Z(i) == 0.0) then
          Jac(i,:) = 0
          Jac(:,i) = 0
          Jac(i,i) = 1
          !
          Jac(nc+i,:) = 0
          Jac(:,nc+i) = 0
          Jac(nc+i,nc+i) = 1
        endif
      enddo
    endif
    Jac(2*nc+1,1:nc) = 1.0
    Jac(2*nc+2,1+nc:2*nc) = 1.0

    ! Temperature differential
    Jac(1:nc,2*nc+1) = T*(lnFugWT-lnFugZT)
    Jac(1+nc:2*nc,2*nc+1) = T*(lnFugXT-lnFugZT)

    ! Volume differential
    Jac(1:nc,2*nc+2) = -vz*lnFugZv
    Jac(1+nc:2*nc,2*nc+2) = -vz*lnFugZv
    Jac(1+nc:2*nc,2*nc+3) = vx*lnFugXv
    Jac(1:nc,2*nc+4) = vw*lnFugWv

    pZ = pressure(t,vZ,Z,dpdv=dpdvZ,dpdt=dpdtZ,dpdn=dpdnZ)
    pW = pressure(t,vW,W,dpdv=dpdvW,dpdt=dpdtW,dpdn=dpdnW)
    pX = pressure(t,vX,X,dpdv=dpdvX,dpdt=dpdtX,dpdn=dpdnX)

    ! Pressure equation differential
    fac = vz/Rgas/T
    Jac(2*nc+3,1:nc) = dpdnW*fac
    Jac(2*nc+4,nc+1:2*nc) = dpdnX*fac
    Jac(2*nc+3,2*nc+1) = T*(dpdtW-dpdtZ)*fac
    if (debug_tv) Jac(2*nc+3,2*nc+1) = Jac(2*nc+3,2*nc+1) - (pW-pZ)*fac
    Jac(2*nc+4,2*nc+1) = T*(dpdtX-dpdtZ)*fac
    if (debug_tv) Jac(2*nc+4,2*nc+1) = Jac(2*nc+4,2*nc+1) - (pX-pZ)*fac
    Jac(2*nc+3,2*nc+2) = -vZ*dpdvZ*fac
    if (debug_tv) Jac(2*nc+3,2*nc+2) = Jac(2*nc+3,2*nc+2) + (pW-pZ)*fac
    Jac(2*nc+4,2*nc+2) = -vZ*dpdvZ*fac
    if (debug_tv) Jac(2*nc+4,2*nc+2) = Jac(2*nc+4,2*nc+2) + (pX-pZ)*fac
    Jac(2*nc+4,2*nc+3) = vX*dpdvX*fac
    Jac(2*nc+3,2*nc+4) = vW*dpdvW*fac

  end subroutine three_ph_diff_newton

  !-----------------------------------------------------------------------------
  !> Plot multi-phase saturation curves
  !>
  !> \author Morten Hammer
  !-----------------------------------------------------------------------------
  subroutine multi_phase_envelope_plot_tv(Z,p_init,Pmax,Tmin,nmax,&
       n,nw,Ta,Pa,beta_YXW,Y,X,W,v_YXW,print_to_file)
    implicit none
    ! Input:
    real,           intent(in)  :: Z(nc)            ! Total molar comp. (-)
    real,           intent(in)  :: p_init           ! p-guess initial point (Pa)
    real,           intent(in)  :: Tmin             ! Exit at low temperatures
    real,           intent(in)  :: Pmax             ! Maximum pressure (Pa)
    integer,        intent(in)  :: nmax             ! Size of allocate memory
    logical,        intent(in)  :: print_to_file    ! Print to text file?
    ! Output
    integer,        intent(out) :: n                ! Number of output points
    integer,        intent(out) :: nw               ! Number of output points for water apperance curve
    real,           intent(out) :: Ta(nmax)         ! Temperature (K)
    real,           intent(out) :: Pa(nmax)         ! Pressure (Pa)
    real,           intent(out) :: beta_YXW(3,nmax) ! Phase molar fraction (-)
    real,           intent(out) :: v_YXW(3,nmax)    ! Phase volume (-)
    real,           intent(out) :: Y(nc,nmax)       ! Phase Y composition (-)
    real,           intent(out) :: X(nc,nmax)       ! Phase X composition (-)
    real,           intent(out) :: W(nc,nmax)       ! Phase W composition (-)
    ! Locals
    real :: K(nc), p, t, Pmin, X_tph(nc), t_HC,t_WATER, lnfug(nc), v1, v2
    integer :: i
    type(three_phase_line), dimension(11) :: threepls
    type(two_phase_line), dimension(10) :: twopls
    type(three_phase_point), dimension(10) :: tps
    integer :: n_threepls, n_twopls, n_tps, stepVar
    real :: beta, sgn_in
    integer :: Yphase
    logical :: isWaterLine !doNotSwitchFormulation
    type(envelope_plot_control_tv) :: epc
    character(len=9) :: fname = "multi.dat"
    call epc%allocate(nc)
    n_threepls = 0
    n_tps = 0
    call two_phase_lines_alloc(twopls, nmax)
    call three_phase_points_alloc(tps)
    Pmin = p_init
    ! Find initial point on envelope, and disable formulation switching for water-gas line
    isWaterLine = initialPoint(Z,t,p_init,K,t_HC,t_WATER)
    p = p_init
    sgn_in = 1.0
    beta = 1.0
    stepVar = 1
    Yphase = VAPPH
    do n_twopls=1,10
      if (mpe_tv_verbose) print *,"Mapping two-phase line from (T, p): ",T,p
      call epc%unset_all()
      call epc%set_minimum_pressure(min(p_init,1.0e5))
      !call epc%set_minimum_temperature(tmin)
      call epc%set_ds_override(dS=0.05)
      call epc%set_second_phase(Yphase)
      call epc%set_K_init(K)
      epc%do_stability_check = .true.
      epc%Pmax = Pmax
      epc%sgn_in = sgn_in
      epc%beta_in = beta
      epc%spec = stepVar
      !epc%do_not_switch_formulation = doNotSwitchFormulation
      twopls(n_twopls)%is_LL = .false. !(Yphase /= VAPPH)
      ! Pass v1 and v2 ?
      call epc%envelope_plot(Z,T,p,nmax,twopls(n_twopls)%T,&
           twopls(n_twopls)%P,twopls(n_twopls)%v1a,twopls(n_twopls)%v2a,&
           twopls(n_twopls)%K(1:nmax,1:nc),twopls(n_twopls)%beta1,twopls(n_twopls)%n)

      !doNotSwitchFormulation = .false.
      call twopls(n_twopls)%copy_to_three_phase_structure(Z)
      if (mpe_tv_verbose) print *,"Return cause: ",epc%get_return_cause_txt()
      !print *,"twopls(n_twopls)%n",twopls(n_twopls)%n
      !print *,twopls(n_twopls)%T(1:twopls(n_twopls)%n)
      !stop
      if (epc%returnCause == ER_STABILITY) then
        X_tph = epc%Wstab
        n_tps = n_tps + 1
        tps(n_tps)%W = twopls(n_twopls)%X2(twopls(n_twopls)%n,:)
        tps(n_tps)%t = twopls(n_twopls)%t(twopls(n_twopls)%n)
        tps(n_tps)%p = twopls(n_twopls)%p(twopls(n_twopls)%n)
        tps(n_tps)%vZ = twopls(n_twopls)%v1a(twopls(n_twopls)%n)
        tps(n_tps)%vX = epc%vW
        tps(n_tps)%vW = twopls(n_twopls)%v2a(twopls(n_twopls)%n)
        tps(n_tps)%X = X_tph
        if (mpe_tv_verbose) call tps(n_tps)%print(NEW_LINE("a")//"Initial three-phase point:")
        call wrap_three_ph_point(Z,tps(n_tps)%W,tps(n_tps)%X,&
             tps(n_tps)%t,tps(n_tps)%p,tps(n_tps)%vZ,&
             tps(n_tps)%vX,tps(n_tps)%vW)
        if (mpe_tv_verbose) call tps(n_tps)%print()
        ! Overwrite last unstable point of two phase line
        call twopls(n_twopls)%overwrite_last_point(tps(n_tps),Z)
        twopls(n_twopls)%end_in_tpp = .true.
        twopls(n_twopls)%i_tpp_end = n_tps

        ! Continue mapping
        K = Z/tps(n_tps)%X
        beta = 1.0
        t = tps(n_tps)%T
        P = tps(n_tps)%P
        v1 = tps(n_tps)%vZ
        v2 = tps(n_tps)%vX
        call thermo(T,P,Z,phase=MINGIBBSPH,lnfug=lnfug,ophase=Yphase)
        if (Yphase == SINGLEPH) Yphase = LIQPH
        call get_step_direction_tv(t,Z,K,v1,v2,beta,stepVar,sgn_in)
        twopls(n_twopls+1)%start_in_tpp = .true.
        twopls(n_twopls+1)%i_tpp_start = n_tps
      else if (epc%returnCause == ER_PMAX_TMIN) then
        exit ! Saturation line complete
      else
        print *,'Termination reason for saturation line: ', trim(epc%get_return_cause_txt())
        exit
      endif
    enddo

    if (n_tps == 0) then
      if (mpe_tv_verbose) print *,"Mapping isolated three-phase region (T, p): ",t_HC,p_init
      if (waterComponentFraction(Z) > 1.0e-9) then
        ! Isolated three-phase region
        n_threepls = 1
        call mapIsolatedThreePhRegion(Z,t_HC,p_init,Pmin,Pmax,Tmin,threepls(n_threepls))
      else
        n_threepls = 0
      endif
    else
      ! Loop triple phase points
      do i=1,n_tps
        if (mpe_tv_verbose) print *,"Mapping three-phase line from (T, p): ",tps(i)%T,tps(i)%p
        n_threepls = n_threepls + 1
        threepls(n_threepls)%i_tpp_start = i
        threepls(n_threepls)%start_in_tpp = .true.
        call map_three_ph_line(threepls(n_threepls),tps(i),Z,&
             Pmin,Pmax,Tmin,.true.,tps,n_tps)
        if (i == 1) then
          if (mpe_tv_verbose) print *,"Mapping three-phase line from (T, p): ",tps(i)%T,tps(i)%p
          n_threepls = n_threepls + 1
          threepls(n_threepls)%i_tpp_start = i
          threepls(n_threepls)%start_in_tpp = .true.
          call map_three_ph_line(threepls(n_threepls),tps(i),Z,&
               Pmin,Pmax,Tmin,.false.,tps,n_tps)
        endif
      enddo
    endif

    ! Print results
    if (print_to_file) &
         call printTPtoFile(tps,n_tps,threepls,n_threepls,twopls,n_twopls,&
         Z,fname,.true.)

    ! Map data-structures to optput arrays
    call write_output_data(threepls,n_threepls,twopls,n_twopls,&
         Z,nmax,n,nw,Ta,Pa,beta_YXW,v_YXW,Y,X,W)

    ! Clean memory
    call three_phase_lines_dealloc(threepls)
    call two_phase_lines_dealloc(twopls)
    call three_phase_points_dealloc(tps)
  end subroutine multi_phase_envelope_plot_tv

  !-----------------------------------------------------------------------------
  !> Map data-structures to optput arrays
  !>
  !> \author Morten Hammer, 2021
  !-----------------------------------------------------------------------------
  subroutine write_output_data(threepls,n_threepls,twopls,n_twopls,&
         Z,nmax,n,nw,Ta,Pa,beta_YXW,phase_YXW,Y,X,W)
    implicit none
    ! Input:
    class(three_phase_line), target, dimension(:), intent(in) :: threepls
    class(two_phase_line), target, dimension(:), intent(in) :: twopls
    integer, intent(in) :: n_threepls, n_twopls
    real,           intent(in)  :: Z(nc)            ! Total molar comp. (-)
    integer,        intent(in)  :: nmax             ! Size of allocate memory
    ! Output:
    integer,        intent(out) :: n                ! Number of points
    integer,        intent(out) :: nw               ! Number of points on incipient water line
    real,           intent(out) :: Ta(nmax)         ! Temperature (K)
    real,           intent(out) :: Pa(nmax)         ! Pressure (Pa)
    real,           intent(out) :: beta_YXW(3,nmax) ! Phase molar fraction (-)
    real,           intent(out) :: phase_YXW(3,nmax)! Phase flags (-)
    real,           intent(out) :: Y(nc,nmax)       ! Phase Y composition (-)
    real,           intent(out) :: X(nc,nmax)       ! Phase X composition (-)
    real,           intent(out) :: W(nc,nmax)       ! Phase W composition (-)
    ! Locals
    type(three_phase_line_pointer), allocatable :: water_incip_lines(:), non_water_incip_lines(:)
    integer :: curves(n_threepls+n_twopls), nc1, nc2, n_water_curves, n_non_water_curves
    integer :: i, istat, k

    if (mpe_tv_verbose) then
      print *,"Writing output to 1D arrays"
      print *,"n_twopls:",n_twopls
      do i=1,n_twopls
        print *,"Two phase: i, i_tpp_start, i_tpp_end", i, twopls(i)%i_tpp_start, twopls(i)%i_tpp_end
      enddo
      print *,"n_threepls:",n_threepls
      do i=1,n_threepls
        print *,"Three phase: i, i_tpp_start, i_tpp_end", i, threepls(i)%i_tpp_start, threepls(i)%i_tpp_end
      enddo
    endif

    curves = 0
    ! Locate water apperance curve data
    do i=1,n_twopls
      if (waterComponentFraction(twopls(i)%X2(1,:)) > 0.8) then
        curves(i) = 1
      endif
    enddo
    do i=1,n_threepls
      if (waterComponentFraction(threepls(i)%tpl(1)%W(:)) > 0.8) then
        curves(n_twopls+i) = 1
      endif
    enddo

    if (mpe_tv_verbose) then
      print *,"Water incipient curves:",curves
    endif

    n_water_curves = sum(curves)
    n_non_water_curves = n_threepls + n_twopls - sum(curves)
    if (n_water_curves > 0) then
      allocate(water_incip_lines(n_water_curves), stat=istat)
      if (istat /= 0) call stoperror('Not able to allocate water_incip_lines')
    endif
    if (n_non_water_curves > 0) then
      allocate(non_water_incip_lines(n_non_water_curves), stat=istat)
      if (istat /= 0) call stoperror('Not able to allocate non_water_incip_lines')
    endif
    nc1 = 0
    nc2 = 0
    do i=1,n_twopls
      if (curves(i) > 0) then
        nc1 = nc1 + 1
        water_incip_lines(nc1)%tplp => twopls(i)
      else
        nc2 = nc2 + 1
        non_water_incip_lines(nc2)%tplp => twopls(i)
      endif
    enddo
    do i=1,n_threepls
      if (curves(n_twopls+i) > 0) then
        nc1 = nc1 + 1
        water_incip_lines(nc1)%tplp => threepls(i)
      else
        nc2 = nc2 + 1
        non_water_incip_lines(nc2)%tplp => threepls(i)
      endif
    enddo

    ! Order water curves
    if (n_water_curves > 1) then
      do i=1,n_water_curves
        if (.not. water_incip_lines(i)%tplp%start_in_tpp) exit
        if (.not. water_incip_lines(i)%tplp%end_in_tpp) then
          water_incip_lines(i)%tplp%inverted_print = .true.
          exit
        endif
      enddo
      if (i /= 1) call swap_lines(water_incip_lines,1,i)
      do k=2,n_water_curves-1
        !if (water_incip_lines()%tplp%end_in_tpp) then
        do i=k,n_water_curves
          if (water_incip_lines(k-1)%tplp%i_tpp_end == water_incip_lines(i)%tplp%i_tpp_start) then
            water_incip_lines(i)%tplp%inverted_print = water_incip_lines(k-1)%tplp%inverted_print
            exit
          else if (water_incip_lines(k-1)%tplp%i_tpp_end == water_incip_lines(i)%tplp%i_tpp_end) then
            water_incip_lines(i)%tplp%inverted_print = .not. water_incip_lines(k-1)%tplp%inverted_print
            exit
          endif
        enddo
        if (i /= k .and. i <= n_water_curves) call swap_lines(water_incip_lines,k,i)
      enddo
    endif

    ! Map water curves
    nw = 0
    do i=1,n_water_curves
      !print *,"i,associated(water_incip_lines(i)%tplp)",i,associated(water_incip_lines(i)%tplp)
      call water_incip_lines(i)%tplp%map_to_output(Z,nmax,nw,Ta,Pa,beta_YXW,phase_YXW,Y,X,W)
    enddo

    ! Order non-water curves
    if (n_non_water_curves > 1) then
      do i=1,n_non_water_curves
        if (.not. non_water_incip_lines(i)%tplp%start_in_tpp) exit
        if (.not. non_water_incip_lines(i)%tplp%end_in_tpp) then
          non_water_incip_lines(i)%tplp%inverted_print = .true.
          exit
        endif
      enddo
      if (i /= 1) call swap_lines(non_water_incip_lines,1,i)
      do k=2,n_non_water_curves-1
        !if (non_water_incip_lines()%tplp%end_in_tpp) then
        do i=k,n_non_water_curves
          if (non_water_incip_lines(k-1)%tplp%i_tpp_end == non_water_incip_lines(i)%tplp%i_tpp_start) then
            non_water_incip_lines(i)%tplp%inverted_print = non_water_incip_lines(k-1)%tplp%inverted_print
            exit
          else if (non_water_incip_lines(k-1)%tplp%i_tpp_end == non_water_incip_lines(i)%tplp%i_tpp_end) then
            non_water_incip_lines(i)%tplp%inverted_print = .not. non_water_incip_lines(k-1)%tplp%inverted_print
            exit
          else if (non_water_incip_lines(k-1)%tplp%i_tpp_start == non_water_incip_lines(i)%tplp%i_tpp_start) then
            non_water_incip_lines(i)%tplp%inverted_print = .not. non_water_incip_lines(k-1)%tplp%inverted_print
            exit
          else if (non_water_incip_lines(k-1)%tplp%i_tpp_start == non_water_incip_lines(i)%tplp%i_tpp_end) then
            non_water_incip_lines(i)%tplp%inverted_print = non_water_incip_lines(k-1)%tplp%inverted_print
            exit
          endif
        enddo
        if (i /= k) call swap_lines(non_water_incip_lines,k,i)
      enddo
    endif

    ! Map non-water curves
    n = nw
    do i=1,n_water_curves
      call non_water_incip_lines(i)%tplp%map_to_output(Z,nmax,n,Ta,Pa,beta_YXW,phase_YXW,Y,X,W)
    enddo

    istat = 0
    if (allocated(water_incip_lines))  deallocate(water_incip_lines, stat=istat)
    if (istat /= 0) call stoperror('Not able to deallocate water_incip_lines')
    if (allocated(non_water_incip_lines))  deallocate(non_water_incip_lines, stat=istat)
    if (istat /= 0) call stoperror('Not able to deallocate non_water_incip_lines')

  contains
    subroutine swap_lines(phase_curves,j,i)
      type(three_phase_line_pointer), intent(inout) :: phase_curves(:)
      integer, intent(in) :: i, j
      ! Locals
      class(three_phase_line), pointer :: p_curve => NULL()
      !print *,"swapping",j,i
      p_curve => phase_curves(i)%tplp
      phase_curves(i)%tplp => phase_curves(j)%tplp
      phase_curves(j)%tplp => p_curve
    end subroutine swap_lines
  end subroutine write_output_data

  !-----------------------------------------------------------------------------
  !> Map two_phase_line data to output data arrays
  !>
  !> \author Morten Hammer, 2021
  !-----------------------------------------------------------------------------
  subroutine three_phase_line_map_to_output(threepl,&
         Z,nmax,n,Ta,Pa,beta_YXW,v_YXW,Y,X,W)
    implicit none
    ! Input:
    class(three_phase_line), intent(in) :: threepl
    real,           intent(in)  :: Z(nc)            ! Total molar comp. (-)
    integer,        intent(in)  :: nmax             ! Size of allocate memory
    ! Output:
    integer,        intent(inout) :: n              ! Index in output array
    real,           intent(out) :: Ta(nmax)         ! Temperature (K)
    real,           intent(out) :: Pa(nmax)         ! Pressure (Pa)
    real,           intent(out) :: beta_YXW(3,nmax) ! Phase molar fraction (-)
    real,           intent(out) :: v_YXW(3,nmax)    ! Phase flags (-)
    real,           intent(out) :: Y(nc,nmax)       ! Phase Y composition (-)
    real,           intent(out) :: X(nc,nmax)       ! Phase X composition (-)
    real,           intent(out) :: W(nc,nmax)       ! Phase W composition (-)
    ! Locals:
    integer :: i
    if (threepl%inverted_print) then
      do i=threepl%n,1,-1
        call set_point()
      enddo
    else
      do i=1,threepl%n
        call set_point()
      enddo
    endif

  contains

    subroutine set_point()
      n = n + 1
      Ta(n) = threepl%tpl(i)%T
      Pa(n) = threepl%tpl(i)%P
      v_YXW(1,n) = threepl%tpl(i)%vY
      v_YXW(2,n) = threepl%tpl(i)%vX
      v_YXW(3,n) = threepl%tpl(i)%vW
      beta_YXW(1,n) = threepl%tpl(i)%beta
      beta_YXW(2,n) = 1-threepl%tpl(i)%beta
      beta_YXW(3,n) = 0
      Y(:,n) = threepl%tpl(i)%Y
      X(:,n) = threepl%tpl(i)%X
      W(:,n) = threepl%tpl(i)%W
   end subroutine set_point
  end subroutine three_phase_line_map_to_output


  !-----------------------------------------------------------------------------
  !> Map three phase line
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine map_three_ph_line(threepl,tpp,Z,Pmin,Pmax,Tmin,WisIncipient,tps,n_tps)
    implicit none
    type(three_phase_line),  intent(inout) :: threepl
    type(three_phase_point), intent(in)  :: tpp
    real,      intent(in)  :: Z(nc)        ! Total molar comp. (-)
    real,      intent(in)  :: Pmin         ! Minimum pressure (Pa)
    real,      intent(in)  :: Pmax         ! Maximum pressure (Pa)
    real,      intent(in)  :: Tmin         ! Minimum temperature (K)
    logical,   intent(in)  :: WisIncipient ! W is Incipent?
    type(three_phase_point), dimension(:) :: tps
    integer,   intent(in)  :: n_tps        ! Number of three phase points
    ! Locals
    real :: beta, W(nc), X(nc), Y(nc), lns, t, p, dlns, sgn
    integer :: s, ierr, iter, i
    real :: vW, vX, vY
    real :: XX(2*nc+5), param(nc+3)
    real :: error_on_exit
    !real, dimension(nc) :: lnFug
    !integer :: phase

    ! Initial point on three phase line
    beta = 1.0
    s = 2*nc+2 ! Specify beta
    lns = 1.0 ! Specify beta
    if (WisIncipient) then
      W = tpp%W
      X = tpp%X
      vW = tpp%vW
      vX = tpp%vX
    else
      W = tpp%X
      X = tpp%W
      vW = tpp%vX
      vX = tpp%vW
    endif
    Y = Z
    t = tpp%t
    p = tpp%p
    vY = tpp%vZ

    call fill_Xvar_and_param_tv(XX,param,Z,W,X,Y,beta,t,p,vW,vX,&
         vY,s,lns)
    call wrap_three_ph_line_solver(param,XX,s,lns,ierr,error_on_exit,iter)
    call set_three_ph_line_point(threepl,Z,XX,param)
    dlns = 0.01
    sgn = -1.0
    call loop_three_ph_line(threepl,Z,XX,param,Pmin,Pmax,Tmin,s,sgn,dlns,lns)

    if (threepl%end_in_tpp) then
      do i=1,n_tps
        if (three_phase_line_end_point_is_tpp(threepl%tpl(threepl%n),tps(i))) then
          threepl%i_tpp_end = i
          threepl%end_in_tpp = .true.
          exit
        endif
      enddo
    endif
  end subroutine map_three_ph_line

  function three_phase_line_end_point_is_tpp(tpl,tpp) result(is_same_point)
    type(three_phase_line_point), intent(in) :: tpl
    type(three_phase_point), intent(in) :: tpp
    logical :: is_same_point

    is_same_point = (abs(tpl%p - tpp%p)/tpp%p < 1.0e-5 .and. &
         abs(tpl%T - tpp%T)/tpp%T < 1.0e-5)
    !if (.not. is_same_point) exit
    !if (tpl%beta > 0.5) then
    !  tpl%W
    !  tpl%X
    ! tpp%W,tpp%X
    !else
    !  tpl%W
    !  tpl%Y
    ! endif

  end function three_phase_line_end_point_is_tpp

  !-----------------------------------------------------------------------------
  !> Map three phase line
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine loop_three_ph_line(threepl,Z,XX,param,Pmin,Pmax,Tmin,s,sgn,dlns,lns)
    implicit none
    type(three_phase_line),  intent(inout) :: threepl
    real,      intent(in)     :: Z(nc)        ! Overall composition
    real,      intent(inout)  :: XX(2*nc+5)   ! Variables (-)
    real,      intent(inout)  :: param(nc+3)  ! Paramaters (-)
    real,      intent(in)     :: Pmin         ! Minimum pressure (Pa)
    real,      intent(in)     :: Pmax         ! Maximum pressure (Pa)
    real,      intent(in)     :: Tmin         ! Minimum temperature (K)
    integer,   intent(inout)  :: s            ! Specification
    real,      intent(inout)  :: sgn          ! Step direction
    real,      intent(inout)  :: dlns         ! Inital step size
    real,      intent(inout)  :: lns          ! Inital specification
    ! Locals
    integer :: ierr, i
    real :: XXold(2*nc+5), dXds(2*nc+5)
    logical :: isS, crossed_temp_limit, zero_beta, crossed_pressure_limit
    !logical :: swappedPhases
    real :: WW(nc), p
    real :: error_on_exit
    !
    !swappedPhases = .false.
    XXold = 0
    do i=1,nmax_static
      call extrapolate_three_ph_line_tv(param,XXold,XX,dXdS,dlns,s,sgn,&
           Pmin,Pmax,Tmin,lns,.false.,(i==1))
      !call print_Xvar_tv(XX,param,"Entering point solver")
      call three_ph_line_point(param,XXold,XX,dXds,dlns,s,lns,sgn,ierr,error_on_exit)
      ! if (ierr /= 0) then
      !   swappedPhases = .true. ! Disable alternative phase swapping
      !   ! Swap gas and liquid phase flags
      !   call swap_gas_and_liquid_phase_flags(XXold,Z,param,s,lns,sgn,ierr)
      !   XX = XXold
      !   ! Run extrapolation again
      !   call extrapolate_three_ph_line_tv(param,XXold,XX,dXdS,dlns,s,sgn,&
      !        Pmin,Pmax,Tmin,lns)
      !   call three_ph_line_point(param,XXold,XX,dXds,dlns,s,lns,sgn,ierr)
      ! endif
      if (ierr /= 0) then
        ! print *,XXold
        ! print *,dXds
        ! print *,sgn,dlns
        ! print *,param
        ! print *,s
        ! stop
        ! XX = XXold + dXds*sgn*dlns*10.0
        ! call read_Xvar_and_param(XX,param,W,X,Y,beta,t,p,phaseW,phaseX,phaseY)
        ! print *,'PP',t,p
        ! call thermo(t,p,Y,MINGIBBSPH,lnFug,ophase=phase)
        ! print *,'phaseY',phaseY,phase
        ! call thermo(t,p,W,MINGIBBSPH,lnFug,ophase=phase)
        ! print *,'phaseW',phaseW,phase
        ! call thermo(t,p,X,MINGIBBSPH,lnFug,ophase=phase)
        ! print *,'phaseX',phaseX,phase
        ! stop

        ! ! Swap phases
        ! if (int(param(nc+4)) == VAPPH) then
        !   print *,'swap'
        !   param(nc+4) = real(LIQPH)
        !   param(nc+6) = real(VAPPH)
        ! else
        !   param(nc+4) = real(VAPPH)
        !   param(nc+6) = real(LIQPH)
        ! endif
        ! call three_ph_line_point(param,XXold,XX,dXds,dlns,s,lns,sgn,ierr)
        print *,'Not able to solve for point on three phase line, giving up'
        exit
      endif
      call set_three_ph_line_point(threepl,Z,XX,param,isS,WW)
      ! Swap phase flags for liquid phases?
      !if (.not. swappedPhases) then
      !  swappedPhases = three_ph_line_swap_phase(XX,param)
      !endif
      crossed_temp_limit = (XX(2*nc+1) < XXold(2*nc+1)) .and. (XX(2*nc+1) - log(Tmin) <= 0.0)
      zero_beta = (XX(2*nc+2)-1.0 >= 0.0 .OR. XX(2*nc+2) <= 0.0)
      p = pressure_Xvar_tv(XX,param)
      crossed_pressure_limit = ((p > Pmax*(1-small)) .or. (p <= Pmin*(1+small)))

      ! print *,"p",p,XX(2*nc+2)
      ! print *,"crossed_pressure_limit",crossed_pressure_limit
      ! print *,"crossed_temp_limit",crossed_temp_limit
      ! print *,"zero_beta",zero_beta
      ! print *,"iss",iss
      if (zero_beta .OR. &
           crossed_pressure_limit .OR. &
           crossed_temp_limit .OR. &
           .not. isS) then
        if (mpe_tv_verbose) call threepl%tpl(threepl%n)%print("Three phase line point at termination:")
        if (zero_beta .OR. .not. isS) then
          !print *,"zero_beta", zero_beta, crossed_temp_limit
          threepl%end_in_tpp = .true.
          !call threepl%tpl(threepl%n)%print()
          ! Locate three-phase point
          !print *,threepl%tpl(threepl%n)%beta
          if (threepl%tpl(threepl%n)%beta > 0.5) then
            threepl%tpl(threepl%n)%beta = 1
            ! Y is Z
            !wrap_three_ph_point(Z,W,X,t,p,phaseZ,phaseX,phaseW)
            call wrap_three_ph_point(Z,threepl%tpl(threepl%n)%W,threepl%tpl(threepl%n)%X,&
                 threepl%tpl(threepl%n)%T,threepl%tpl(threepl%n)%p,threepl%tpl(threepl%n)%vY,&
                 threepl%tpl(threepl%n)%vX,threepl%tpl(threepl%n)%vW)
          else
            threepl%tpl(threepl%n)%beta = 0
            ! X is Z
            call wrap_three_ph_point(Z,threepl%tpl(threepl%n)%W,threepl%tpl(threepl%n)%Y,&
                 threepl%tpl(threepl%n)%T,threepl%tpl(threepl%n)%p,threepl%tpl(threepl%n)%vX,&
                 threepl%tpl(threepl%n)%vY,threepl%tpl(threepl%n)%vW)
          endif
        endif
        ! print *,"p",p,XX(2*nc+2),exp(XX(2*nc+1))
        ! print *,"crossed_pressure_limit",crossed_pressure_limit
        ! print *,"crossed_temp_limit",crossed_temp_limit
        ! print *,"zero_beta",zero_beta
        ! print *,"iss",iss
        exit
      endif
    enddo
  end subroutine loop_three_ph_line

  !-----------------------------------------------------------------------------
  !> Test if new liquid phase should be introduced
  !>
  !> \author Morten Hammer, 2016-08
  !-----------------------------------------------------------------------------
  function isStableThreePh(t,p,Z,Y,X,W,beta,vY,vX,vW,WW) result(isS)
    use stability, only: stabcalcW, stabilityLimit
    use critical, only: calcStabMinEig, calcStabMinEigTV
    !use thermo_utils, only: getWaterFraction
    implicit none
    real, intent(in) :: t,p,beta
    real, intent(in) :: vY,vX,vW
    real, dimension(nc), intent(in) :: X,Y,W,Z
    real, dimension(nc), intent(out) :: WW
    logical :: isS
    ! Locals
    integer :: nd, phase, i
    real :: XX(3,nc),lnFugY(nc),lnFugW(nc),tpd, stab_min_eig(4)
    real, parameter :: stab_lim = stabilityLimit*5.0e4
    call thermo_tv(t,vY,Y,lnFugY)
    lnFugY = lnFugY - log(p) - log(Y)
    nd = 3
    XX(1,:) = Y
    XX(2,:) = W
    XX(3,:) = X
    phase = LIQPH

    ! Calculate starting values
    !call wilsonK(t,p,K)
    !WW = K*Z
    !tpd = stabcalcW(nd,1,t,p,XX,WW,VAPPH,isTrivial,lnFugY,lnFugW)
    !print *,"tpd gass",tpd
    do i=1,nc
      WW = 0.0
      WW(i) = 1.0
      tpd = stabcalcW(nd,1,t,p,XX,WW,phase,lnFugY,lnFugW)
      isS = (tpd > stab_lim)
      if (.not. isS) then
        !print *,"tpd",tpd, stabilityLimit*500.0
        stab_min_eig(1) = calcStabMinEigTV(t,vY,Y)
        stab_min_eig(2) = calcStabMinEigTV(t,vX,X)
        stab_min_eig(3) = calcStabMinEigTV(t,vW,W)
        stab_min_eig(4) = calcStabMinEig(t,p,WW,phase)
        ! Force tangent plane to converge
        tpd = stabcalcW(nd,1,t,p,XX,WW,phase,lnFugY,lnFugW,-1000.0)
        print *,'Not stable',tpd,stab_lim
        print *,"beta",beta
        call thermo(t,p,Y,MINGIBBSPH,lnfugW,ophase=phase)
        print *,'phase-Y',phase
        print *,'Y',Y
        call thermo(t,p,X,MINGIBBSPH,lnfugW,ophase=phase)
        print *,'phase-X',phase
        print *,'X',X
        call thermo(t,p,W,MINGIBBSPH,lnfugW,ophase=phase)
        print *,'phase-W',phase
        print *,'W',W
        call thermo(t,p,WW,MINGIBBSPH,lnfugW,ophase=phase)
        print *,'phase-WW',phase
        print *,'WW',WW
        print *,'Min.Eig.-Y',stab_min_eig(1)
        print *,'Min.Eig.-X',stab_min_eig(2)
        print *,'Min.Eig.-W',stab_min_eig(3)
        print *,'Min.Eig.-WW',stab_min_eig(4)
        call exit(1)
      endif
    enddo
  end function isStableThreePh

  !-----------------------------------------------------------------------------
  !> Map isolated three phase line
  !>
  !> \author Morten Hammer, 2015-11
  !-----------------------------------------------------------------------------
  subroutine mapIsolatedThreePhRegion(Z,t_HC,p_init,Pmin,Pmax,Tmin,threepl)
    type(three_phase_line),  intent(out) :: threepl
    real,           intent(in)  :: Z(nc)       !< Total molar comp. (-)
    real,           intent(in)  :: T_HC       !< Initial guess for temperature (K)
    real,           intent(in)  :: p_init     !< Initial point pressure (Pa)
    real,           intent(in)  :: Pmin       !< Minimum pressure (Pa)
    real,           intent(in)  :: Pmax       !< Maximum pressure (Pa)
    real,           intent(in)  :: Tmin       !< Minimum temperature (K)
    ! Locals
    real    :: XX(2*nc+5)   ! Variables (-)
    real    :: param(nc+3)  ! Paramaters (-)
    integer :: s            ! Specification
    real    :: sgn          ! Step direction
    real    :: dlns         ! Inital step size
    real    :: lns          ! Inital specification

    call initialIsolatedRegionPoint(Z,t_HC,p_init,XX,param,lns,s,sgn,dlns)
    call set_three_ph_line_point(threepl,Z,XX,param)
    call loop_three_ph_line(threepl,Z,XX,param,Pmin,Pmax,Tmin,s,sgn,dlns,lns)
  end subroutine mapIsolatedThreePhRegion

  !-----------------------------------------------------------------------------
  !> Determine initial point for isolated three phase line
  !> Solve for T and water phase fraction using K-ideal
  !> \author Morten Hammer, 2015-11
  !-----------------------------------------------------------------------------
  subroutine initialIsolatedRegionPoint(Z,t_HC,p_init,XXF,paramF,lns,s,sgn,dlns)
    use thermo_utils, only: isWaterComponent
    implicit none
    ! Input:
    real,           intent(in)  :: Z(nc)        !< Total molar comp. (-)
    real,           intent(in)  :: T_HC         !< Initial guess for temperature (K)
    real,           intent(in)  :: p_init       !< Initial point pressure (Pa)
    real,           intent(out) :: XXF(2*nc+5)  !< Variables
    real,           intent(out) :: paramF(nc+3) !< Parameters
    real,           intent(out) :: lns          !< Specification
    real,           intent(out) :: dlns         !< Step size
    real,           intent(out) :: sgn          !< Step direction
    integer,        intent(out) :: s            !< Specification variable
    ! Locals
    real :: param(3*nc+1), lnPhi_offset(nc), p
    integer :: ierr, iter, i
    real :: T,betaW,X_HC(nc),X_WATER(nc),Y_HC(nc)
    type(nonlinear_solver) :: solver
    real, dimension(2) :: XX, XXmax, XXmin
    real :: Tmin,Tmax
    real :: betaY, vW, vY, vX
    real :: error_on_exit
    !real :: Jac(2,2), G(2), G1(2)
    T = T_HC
    betaW = 0.0
    do i=1,nc
      if (isWaterComponent(i)) then
        betaW = betaW + Z(i)
      endif
    enddo
    p = p_init
    param(1) = p
    param(2:nc+1) = Z
    XX(1) = T
    XX(2) = betaW ! Assume pure phase
    call calcLnPhiOffset(WATER,lnPhi_offset)
    param(nc+2:2*nc+1) = lnPhi_offset
    call calcLnPhiOffset(NONWATER,lnPhi_offset)
    param(2*nc+2:3*nc+1) = lnPhi_offset

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! print *
    ! print *,'Numerical test'
    ! call isolated_three_ph_jac_newton(Jac,XX,param)
    ! call isolated_three_ph_fun_newton(G,XX,param)
    ! XX(2) = XX(2) + 1.0e-5
    ! call isolated_three_ph_fun_newton(G1,XX,param)
    ! print *,'betaW'
    ! print *,(G1-G)/1.0e-5
    ! print *,Jac(:,2)
    ! XX(1) = T + 1.0e-3
    ! XX(2) = betaW
    ! call isolated_three_ph_fun_newton(G1,XX,param)
    ! print *,'T'
    ! print *,(G1-G)/1.0e-3
    ! print *,Jac(:,1)
    ! stop
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-12
    solver%limit_x_values = .true.
    solver%max_it = 300
    solver%ls_max_it = 3

    call get_templimits(Tmin,Tmax)
    XXmin(1) = Tmin
    XXmax(1) = Tmax
    XXmin(2) = 0.0 !beta min
    XXmax(2) = 1.0 !beta max
    call nonlinear_solve(solver,isolated_three_ph_fun_newton,&
         isolated_three_ph_jac_newton,isolated_three_ph_jac_newton,&
         limit_dx,premReturn,setXv,XX,XXmin,XXmax,param)
    ierr = solver%exitflag
    T = XX(1)
    betaW = XX(2)
    call getIdealIsolatedRegionComp(param,Z,T,p,betaW,X_HC,Y_HC,X_WATER)

    ! Full newton search
    s = 2*nc+6 ! Fixed pressure value
    lns = log(p)
    call specificvolume(T,p,X_HC,LIQPH,vW)
    call specificvolume(T,p,Y_HC,VAPPH,vY)
    call specificvolume(T,p,X_WATER,LIQPH,vX)
    betaY = 1 - betaW
    call fill_Xvar_and_param_tv(XXF,paramF,Z,X_HC,X_WATER,Y_HC,&
         betaY,t,p,vW,vX,vY,s,lns)
    call wrap_three_ph_line_solver(paramF,XXF,s,lns,ierr,error_on_exit,iter)
    !call read_Xvar_and_param_tv(XXF,paramF,X_HC,Y_HC,X_WATER,&
    !     betaW,t,p,vW,vX,vY)
    ! Set initial step for three phase line
    dlns = 0.01
    sgn = 1.0
  end subroutine initialIsolatedRegionPoint

  !-----------------------------------------------------------------------------
  !> Calculate composition from Z, T, p, betaW and K-values
  !>
  !> \author Morten Hammer, 2015-11
  !-----------------------------------------------------------------------------
  subroutine getIdealIsolatedRegionComp(param,Z,T,p,betaW,X_NONWATER,Y,X_WATER)
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(in) :: betaW !< Water phase fractions
    real, intent(in) :: T !< Temperature
    real, intent(in) :: p !< Pressure
    real, dimension(3*nc+1) :: param !< Parameter vector
    real, dimension(nc), intent(out) :: Y, X_NONWATER, X_WATER !< Composition

    ! Locals
    real, dimension(nc) :: lnPhi_offset_W, dKdt_W, dKdp
    real, dimension(nc) :: lnPhi_offset_NW, dKdT_NW, K_W, K_NW
    integer :: i
    real :: beta, denum

    lnPhi_offset_W = param(nc+2:2*nc+1)
    lnPhi_offset_NW = param(2*nc+2:3*nc+1)
    call wilsonKdiff(t,p,K_W,dKdp,dKdt_W,lnPhi_offset_W)
    call wilsonKdiff(t,p,K_NW,dKdp,dKdt_NW,lnPhi_offset_NW)
    K_W = K_NW/K_W
    beta = 1.0

    do i=1,nc
      denum = betaW*K_W(i)+(1.0-betaW)*(beta*K_NW(i)+1.0-beta)
      Y(i) = K_NW(i)*Z(i)/denum
      X_NONWATER(i) = Z(i)/denum
      X_WATER(i) = K_W(i)*Z(i)/denum
    enddo
  end subroutine getIdealIsolatedRegionComp

  !-----------------------------------------------------------------------------
  !> Isolated three phase point function values for non-linear solver
  !>
  !> \author Morten Hammer, 2015-11
  !-----------------------------------------------------------------------------
  subroutine isolated_three_ph_fun_newton(G,Yvar,param)
    implicit none
    real, dimension(2), intent(out) :: G !< Function values
    real, dimension(2), intent(in) :: Yvar !< Variable vector
    real, dimension(3*nc+1) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, lnPhi_offset_W, dKdt_W, dKdp
    real, dimension(nc) :: lnPhi_offset_NW, dKdT_NW, K_W, K_NW
    integer :: i
    real :: p, t, betaW, beta, denum

    p = param(1)
    Z = param(2:nc+1)
    lnPhi_offset_W = param(nc+2:2*nc+1)
    lnPhi_offset_NW = param(2*nc+2:3*nc+1)
    T = Yvar(1)
    betaW = Yvar(2)
    call wilsonKdiff(t,p,K_W,dKdp,dKdt_W,lnPhi_offset_W)
    call wilsonKdiff(t,p,K_NW,dKdp,dKdt_NW,lnPhi_offset_NW)
    K_W = K_NW/K_W
    beta = 1.0

    G = 0.0
    do i=1,nc
      denum = betaW*K_W(i)+(1.0-betaW)*(beta*K_NW(i)+1.0-beta)
      G(1) = G(1) + (K_NW(i)-1.0)*Z(i)/denum
      G(2) = G(2) + K_W(i)*Z(i)/denum
    enddo
    G(2) = G(2) - 1.0
  end subroutine isolated_three_ph_fun_newton

  !-----------------------------------------------------------------------------
  !> Isolated three phase point function values for non-linear solver
  !>
  !> \author Morten Hammer, 2015-11
  !-----------------------------------------------------------------------------
  subroutine isolated_three_ph_jac_newton(Jac,Yvar,param)
    implicit none
    real, dimension(2,2), intent(out) :: Jac !< Function differential values
    real, dimension(2), intent(in) :: Yvar !< Variable vector
    real, dimension(3*nc+1) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, lnPhi_offset_W, dKdt_W, dKdp
    real, dimension(nc) :: lnPhi_offset_NW, dKdT_NW, K_W, K_NW
    integer :: i
    real :: denum
    real :: p, t, betaW, beta, d_denum_dbetaW, d_denum_dT, denum2

    p = param(1)
    Z = param(2:nc+1)
    lnPhi_offset_W = param(nc+2:2*nc+1)
    lnPhi_offset_NW = param(2*nc+2:3*nc+1)
    T = Yvar(1)
    betaW = Yvar(2)
    call wilsonKdiff(t,p,K_W,dKdp,dKdt_W,lnPhi_offset_W)
    call wilsonKdiff(t,p,K_NW,dKdp,dKdt_NW,lnPhi_offset_NW)
    dKdt_W = dKdt_NW/K_W - dKdt_W*K_NW/K_W**2
    K_W = K_NW/K_W
    beta = 1.0

    Jac = 0.0
    do i=1,nc
      denum = betaW*K_W(i)+(1.0-betaW)*(beta*K_NW(i)+1.0-beta)
      denum2 = denum**2
      ! Temperature differentials
      d_denum_dT = betaW*dKdt_W(i)+(1.0-betaW)*beta*dKdT_NW(i)
      Jac(1,1) = Jac(1,1) - d_denum_dT*(K_NW(i)-1.0)*Z(i)/denum2 + dKdT_NW(i)*Z(i)/denum
      Jac(2,1) = Jac(2,1) - d_denum_dT*K_W(i)*Z(i)/denum2 + dKdt_W(i)*Z(i)/denum
      ! BetaW differentials
      d_denum_dbetaW = K_W(i)-(beta*K_NW(i)+1.0-beta)
      Jac(1,2) = Jac(1,2) - d_denum_dbetaW*(K_NW(i)-1.0)*Z(i)/denum2
      Jac(2,2) = Jac(2,2) - d_denum_dbetaW*K_W(i)*Z(i)/denum2
    enddo
  end subroutine isolated_three_ph_jac_newton

  !-----------------------------------------------------------------------------
  !> Determine initial point for phase envelope
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  function initialPoint(Z,t,p_init,K,t_HC,t_WATER) result(isWaterLine)
    use thermo_utils, only: waterComponentFraction
    use saturation, only: specP
    implicit none
    ! Input:
    real,           intent(in)  :: Z(nc)     ! Total molar comp. (-)
    real,           intent(out) :: T         ! T-guess initial point (K)
    real,           intent(in)  :: p_init    ! Initial point pressure (Pa)
    real,           intent(out) :: K(nc)     ! Initial K factor
    real,           intent(out) :: T_HC      ! T-guess HC (K)
    real,           intent(out) :: T_WATER   ! T-guess WATER (K)
    logical :: isWaterLine ! Is it a waterline?
    ! Locals
    real :: K_WATER(nc), K_HC(nc), p, lns, beta, W(nc)
    integer :: ierr, iter, Wphase
    logical :: isS

    t_HC = 300.0 ! Initial value
    p = p_init
    call sat_wilsonK(Z,K_HC,t_HC,p,specP,.false.,NONWATER,ierr)
    if (mpe_tv_verbose) print *,'T HC',t_HC
    if (waterComponentFraction(Z) > small) then
      t_WATER = 300.0 ! Initial value
      call sat_wilsonK(Z,K_WATER,t_WATER,p,specP,.false.,WATER,ierr)
      if (mpe_tv_verbose) print *,'T WATER',t_WATER
    else
      t_WATER = t_HC - 1.0
    endif

    if (t_WATER > T_HC) then ! Solve for water saturation line
      t = t_WATER
      K = K_WATER
      isWaterLine = .true.
    else ! Solve for HC saturation line
      t = t_HC
      K = K_HC
      isWaterLine = .false.
    endif

    ! Find actual saturation point
    ! Set early return off successive substitution approach
    call sat_successive(Z,K,t,p,1,.false.,10)

    beta = 1.0
    lns = log(p)
    iter = sat_newton(Z,K,t,p,beta,nc+2,lns,VAPPH)
    if (mpe_tv_verbose) print *,'Actual T',t,iter
    isS = isStable(t,p,Z,K,VAPPH,beta,W,Wphase)
    if (mpe_tv_verbose) print *,'isStable',isS
    if (.not. isS) then
      ! Swap to other liquid phase
      if (isWaterLine) then ! Solve for water saturation line
        t = t_HC
        K = K_HC
        isWaterLine = .false.
      else ! Solve for HC saturation line
        t = t_WATER
        K = K_WATER
        isWaterLine = .true.
      endif
      iter = sat_newton(Z,K,t,p,beta,nc+2,lns,VAPPH)
      if (mpe_tv_verbose) print *,'Actual T',t,iter
      isS = isStable(t,p,Z,K,VAPPH,beta,W,Wphase)
      if (mpe_tv_verbose) print *,'isStable',isS
    endif
  end function initialPoint

  !-----------------------------------------------------------------------------
  !> Set solution
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine set_three_ph_line_point(tpline,Z,XX,param,isS,WW)
    implicit none
    type(three_phase_line), intent(inout) :: tpline
    real, dimension(nc), intent(in) :: Z
    real, dimension(2*nc+5), intent(in) :: XX
    real, dimension(nc+3), intent(in) :: param
    logical, optional, intent(out) :: isS
    real, optional, dimension(nc), intent(out) :: WW
    ! Locals
    real, dimension(nc) :: W, X, Y, WWl
    real :: vW,vX,vY
    real :: t, p, beta
    !logical :: isStab
    call read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY)
    !print *,"TP",T,P
    !print *,"Y",Y
    !print *,"X",X
    !print *,"W",W
    !print *,"YXW",phaseY,phaseX,phaseW
    !print *,"beta",beta
    call tpline%push_back(T,p,beta,W,X,Y,vW,vX,vY)
    if (present(isS)) then
      isS = isStableThreePh(t,p,Z,Y,X,W,beta,vY,vX,vW,WWl)
      if (present(WW)) then
        WW = WWl
      endif
    endif
    !print *,'point',T,p,beta
  end subroutine set_three_ph_line_point

  !-----------------------------------------------------------------------------
  !> Wrapper for solver of three phase line
  !> Do iterations with partial jacobian before using full jacobian
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine wrap_three_ph_line_solver(param,XX,s,lns,ierr,error_on_exit,iter)
    implicit none
    real, dimension(2*nc+5), intent(inout) :: XX
    real, dimension(nc+3), intent(inout) :: param
    real, intent(inout)    :: lns
    real, intent(out)    :: error_on_exit
    integer, intent(in) :: s
    integer, intent(out) :: ierr, iter
    !
    ierr = 0
    call three_ph_line_newton(param,XX,s,lns,.true.,numberOfPartialSteps,ierr,3,&
         error_on_exit)
    iter = 1
    if (ierr /= 0) then
      call three_ph_line_newton(param,XX,s,lns,.false.,max_iter,ierr,3,&
           error_on_exit,iter)
      iter = iter + 1
    endif
  end subroutine wrap_three_ph_line_solver

  !-----------------------------------------------------------------------------
  !> Solve for point on three phase line
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_line_point(param,XXold,XX,dXds,dlns,s,lns,sgn,ierr,error_on_exit)
    implicit none
    real, dimension(2*nc+5), intent(in) :: XXold, dXds
    real, dimension(2*nc+5), intent(inout) :: XX
    real, dimension(nc+3), intent(inout) :: param
    real, intent(inout)    :: lns, dlns, sgn
    integer, intent(in) :: s
    integer, intent(out) :: ierr
    real, intent(out)    :: error_on_exit
    ! Locals
    real, parameter :: tuning = 1.2
    integer :: i, iter
    real :: halff, dlns_in, p_old, p, dpds
    real, dimension(2*nc+5) :: XXtemp, G
    dlns_in = dlns
    if (s == 2*nc+6) then
      call calc_dpds(XXold,dXds,param,p_old,dpds)
    endif
    halff = 1.0
    do i=1,4 ! Try 4 times before giving up
      call wrap_three_ph_line_solver(param,XX,s,lns,ierr,error_on_exit,iter)
      if (ierr == 0) then
        dlns = halff*dlns
        exit
      endif
      halff = 0.5**i
      XX = XXold + dXds*sgn*dlns*halff
      if (s == 2*nc+6) then
        p = p_old + dpds*sgn*dlns*halff
        lns = log(p)
      else
        lns = XX(s)
      endif
    enddo
    if (ierr /= 0) then ! Try doubling initial step
      XX = XXold + dXds*sgn*dlns*2.0
      if (s == 2*nc+6) then
        p = p_old + dpds*sgn*dlns*2.0
        lns = log(p)
      else
        lns = XX(s)
      endif
      call wrap_three_ph_line_solver(param,XX,s,lns,ierr,error_on_exit,iter)
      if (ierr == 0) then
        dlns = dlns*2.0
      endif
    endif
    if (ierr == 0) then
      ! Tune dS up or down based on how fast sat_newton converged
      if (iter < 3) then
        dlns = dlns * tuning
      else if (iter > 5) then
        dlns = dlns / tuning
      endif
      dlns = max(min(dlns,0.1),0.005)
    else
      if (error_on_exit < 5.0e-8) then
        ! Water pressure error at low temperature?
        ! Try to continue
        ierr = 0
        return
      endif
      call print_Xvar_tv(XXold,param,"Old state")
      print *,"s",s
      print *,"dlns",dlns_in
      print *,"sgn",sgn
      call three_ph_line_fun_newton(G,XX,param)
      print *,"G exit",G
      print *,"error_on_exit",error_on_exit
      XXtemp = XXold + dXds*sgn*dlns_in
      call print_Xvar_tv(XXtemp,param,"Initial state")
    endif
  end subroutine three_ph_line_point

  !-----------------------------------------------------------------------------
  !> Solve for three phase line using NR solver
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_line_newton(param,XX,s,lns,partial,niter,ierr,&
       nlines,error_on_exit,iter)
    use utilities, only: isXwithinBounds
    implicit none
    real, dimension(2*nc+5), intent(inout) :: XX
    real, dimension(nc+3), intent(inout) :: param
    logical, intent(in) :: partial
    real, intent(in)    :: lns
    integer, intent(in) :: niter, s, nlines
    integer, intent(out) :: ierr
    real, intent(out) ::  error_on_exit
    integer, optional, intent(out) :: iter
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(2*nc+5) :: XXmax, XXmin
    real :: W(nc),X(nc),Y(nc),beta,t,p,vW,vX,vY
    ! real :: G(2*nc+5), J(2*nc+5,2*nc+5), dXX
    ! real :: G1(2*nc+5), J1(2*nc+5,2*nc+5), XX0(2*nc+5)
    ! integer :: i
    param(nc+1) = lns
    param(nc+2) = real(s)
    if (partial) then
      param(nc+3) = 1.0
    else
      param(nc+3) = 0.0
    endif

    ! XX =(/14.664418513028755, -10.398985162227861, 14.664448971237785,&
    !      -18.998242274339034, 5.1081310265597253, 1.5213907961904378E-002,&
    !      -2.9016523680710287,-10.245805777882209,-10.715270524425001/)
    ! param = (/0.99997000000000003, 3.0000000000000001E-005,  -18.996395345023217,&
    !      4.0000000000000000,        0.0000000000000000/)
    ! Test differentials
    ! XX(2*nc+2) = 1.00
    ! param(nc+3) = 0.0
    ! param(nc+2) = real(2*nc+2)
    ! XX0 = XX
    ! call three_ph_line_fun_newton(G,XX0,param)
    ! call three_ph_line_diff_newton(J,XX0,param)
    ! J1 = J
    ! n = 2*nc+5
    ! call DGEEV('N', 'N', n, J1, n, WR, WI, VL, n, VR, n, WORK, 3*n, INFO )
    ! print *,'Eigenvalues, real part: ',wr
    ! print *,'Eigenvalues, imaginary part: ',maxval(abs(wi))

    ! do i=1,2*nc+5
    !   XX = XX0
    !   dXX = XX0(i)*1.0e-5
    !   XX(i) = XX0(i) + dXX
    !   call three_ph_line_fun_newton(G1,XX,param)
    !   XX(i) = XX0(i) - dXX
    !   call three_ph_line_fun_newton(G,XX,param)
    !   print *,'*************************************'
    !   print *,'dGdx 1:nc',(G1(1:nc)-G(1:nc))/dXX/2
    !   print *,'dGdx 1:nc',J(1:nc,i)
    !   print *,'dGdx nc+1:2*nc',(G1(nc+1:2*nc)-G(nc+1:2*nc))/dXX/2
    !   print *,'dGdx nc+1:2*nc',J(nc+1:2*nc,i)
    !   print *,'dGdx 2*nc+1:2*nc+2',(G1(2*nc+1:2*nc+2)-G(2*nc+1:2*nc+2))/dXX/2
    !   print *,'dGdx 2*nc+1:2*nc+2',J(2*nc+1:2*nc+2,i)
    !   print *,'dGdx 2*nc+3:2*nc+5',(G1(2*nc+3:2*nc+5)-G(2*nc+3:2*nc+5))/dXX/2
    !   print *,'dGdx 2*nc+3:2*nc+5',J(2*nc+3:2*nc+5,i)
    !   !stop
    ! enddo
    ! stop

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 2.0e-9
    solver%limit_x_values = .true.
    solver%max_it = niter
    solver%ls_max_it = nlines

    call read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY)
    call sat_var_tv_limits(Y,X,W,XXmin,XXmax,include_beta=.true.)
    call isXwithinBounds(2*nc+5,XX,XXmin,XXmax,"",&
         "three_ph_line_newton: Initial values not within bounds!!")
    call nonlinear_solve(solver,three_ph_line_fun_newton,&
         three_ph_line_diff_newton,three_ph_line_diff_newton,&
         limit_dx_line,premReturn,setXv,XX,XXmin,XXmax,param)
    ierr = solver%exitflag
    error_on_exit = solver%error_on_exit
    !print *,'ierr',ierr,solver%error_on_exit
    !stop
    if (present(iter)) then
      iter = solver%iter
    endif
  end subroutine three_ph_line_newton

  !-----------------------------------------------------------------------------
  !> Limit change in x, dx, to keep x between extreme values:
  !> xmin <= x <= xmax
  !>
  !> \author MH, Dec 2021
  !-----------------------------------------------------------------------------
  subroutine limit_dx_line(n,xx,xxmin,xxmax,dxx,np,param)
    use eosdata, only: eosCPA
    implicit none
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: xx,xxmin,xxmax
    real, dimension(n),     intent(inout) :: dxx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: param
    real :: scaling
    logical :: needalt, isCPA
    real :: W(nc),X(nc),Y(nc),beta,t,p,vW,vX,vY,b
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    !
    call limit_dx(n,xx,xxmin,xxmax,dxx,np,param)
    ! Additional test for minimum volume?
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (.not. (needalt .and. .not. isCPA)) then
      if (minval(abs(Xx(2*nc+2:2*nc+4)+dxx(2*nc+2:2*nc+4)-Xxmin(2*nc+2:2*nc+4))) < 0.05) then
        call read_Xvar_and_param_tv(Xx+dxx,param,W,X,Y,beta,t,p,vW,vX,vY,no_press_calc=.true.)
        b = get_b_linear_mix(Y) + Small ! m3/mol
        scaling = 1.0
        if (vY < b) scaling = 0.5
        b = get_b_linear_mix(X) + Small ! m3/mol
        if (vX < b) scaling = 0.5
        b = get_b_linear_mix(W) + Small ! m3/mol
        if (vW < b) scaling = 0.5
        if (scaling < 1.0) then
          dxx = dxx * scaling
        endif
      endif
    endif

  end subroutine limit_dx_line

  !-----------------------------------------------------------------------------
  !> Three phase point function values for non-linear solver
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_line_fun_newton(G,Yvar,param)
    use eos
    implicit none
    real, dimension(2*nc+5), intent(out) :: G !< Function values
    real, dimension(3*nc+5), intent(in) :: Yvar !< Variable vector
    real, dimension(nc+3) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, X, W, Y!, Ky, Kx, lnKy, lnKx
    real, dimension(nc) :: lnFugY, lnFugX, lnFugW
    integer :: i, s
    real :: vY, vX, vW, vS
    real :: pY, pX, pW, t, lns, beta, p

    !Z = param(1:nc)
    lns = param(nc+1)
    s = nint(param(nc+2))
    Z = param(1:nc)
    ! lnKx = Yvar(1:nc)
    ! lnKy = Yvar(1+nc:2*nc)
    ! t = exp(Yvar(2*nc+1))
    ! beta = Yvar(2*nc+2)
    ! vY = exp(Yvar(2*nc+3))
    ! vX = exp(Yvar(2*nc+4))
    ! vW = exp(Yvar(2*nc+5))
    ! !
    ! Kx = exp(lnKx)
    ! Ky = exp(lnKy)
    ! ! Incipient phase W
    ! W = 0.0
    ! do i=1,nc
    !   if (Z(i) > 0.0) then
    !     W(i) = Z(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
    !   endif
    ! enddo
    ! Y = Ky*W
    ! X = Kx*W
    call read_Xvar_and_param_tv(Yvar,param,W,X,Y,beta,t,p,vW,vX,vY)
    call thermo_tv(t,vY,Y,lnFugY)
    call thermo_tv(t,vW,W,lnFugW)
    call thermo_tv(t,vX,X,lnFugX)
    pY = pressure(t,vY,Y)
    pW = pressure(t,vW,W)
    pX = pressure(t,vX,X)
    ! Volume average for pressure error scaling
    vS = 3/(1/vW + 1/vX + 1/vY)

    ! Function value
    do i=1,nc
      if (Z(i) > 0.0) then
        G(nc+i) = lnFugY(i) - lnFugW(i)
        G(i) = lnFugX(i) - lnFugW(i)
      else
        G(i) = 0.0
        G(nc+i) = 0.0
      endif
    enddo
    G(2*nc+1) = sum(W) - 1.0
    G(2*nc+2) = sum(Y-X)
    if (s <= 2*nc+5) then
      G(2*nc+3) = Yvar(s) - lns
    else if (s == 2*nc+6) then
      ! Fixate pressure
      p = exp(lns)
      G(2*nc+3) = vS*(P-Pw)/(Rgas*T)
    endif
    !
    G(2*nc+4) = vS*(Px-Pw)/(Rgas*T)
    G(2*nc+5) = vS*(Py-Pw)/(Rgas*T)

    !print *,"G",G
  end subroutine three_ph_line_fun_newton

  !-----------------------------------------------------------------------------
  !> Differentials for three phase point function values for non-linear solver
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine three_ph_line_diff_newton(Jac,Yvar,param)
    implicit none
    real, dimension(2*nc+5), intent(in) :: Yvar !< Variable vector
    real, dimension(2*nc+5,2*nc+5), intent(out) :: Jac !< Function differentials
    real, dimension(nc+3) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, X, W, Y, lnKx, lnKy, Ky, Kx
    real, dimension(nc) :: lnFugY, lnFugX, lnFugW
    real, dimension(nc) :: lnFugXT, lnFugWT, lnFugXv, lnFugWv
    real, dimension(nc) :: lnFugYT, lnFugYv
    real, dimension(nc,nc) :: lnFugXn, lnFugWn, lnFugYn
    integer :: i, s
    real :: t, beta, vY, vX, vW, pY, pX, pW, fac, p, lns
    real :: vS, dvSdvW_div_vS, dvSdvX_div_vS, dvSdvY_div_vS
    logical :: partial
    real, dimension(nc) :: dWdlnKX, dWdlnKY, dXdlnKX, dXdlnKY
    real, dimension(nc) :: dYdlnKX, dYdlnKY, dWdbeta, dXdbeta
    real, dimension(nc) :: dYdbeta
    real :: dpdvY,dpdTY,dpdnY(nc)
    real :: dpdvW,dpdTW,dpdnW(nc)
    real :: dpdvX,dpdTX,dpdnX(nc)
    !
    Z = param(1:nc)
    s = nint(param(nc+2))
    lns = param(nc+1)
    partial = (nint(param(3+nc)) /= 0)
    lnKx = Yvar(1:nc)
    lnKy = Yvar(1+nc:2*nc)
    t = exp(Yvar(2*nc+1))
    beta = Yvar(2*nc+2)
    vY = exp(Yvar(2*nc+3))
    vX = exp(Yvar(2*nc+4))
    vW = exp(Yvar(2*nc+5))
    Kx = exp(lnKx)
    Ky = exp(lnKy)
    W = 0.0
    do i=1,nc
      if (Z(i) > 0.0) then
        W(i) = Z(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
      endif
    enddo
    Y = Ky*w
    X = Kx*w

    call thermo_tv(t,vY,Y,lnFugY,lnFugYT,lnFugYv,lnFugYn)
    call thermo_tv(t,vW,W,lnFugW,lnFugWT,lnFugWv,lnFugWn)
    call thermo_tv(t,vX,X,lnFugX,lnFugXT,lnFugXv,lnFugXn)
    pY = pressure(t,vY,Y,dpdv=dpdvY,dpdT=dpdTY,dpdn=dpdnY)
    pW = pressure(t,vW,W,dpdv=dpdvW,dpdT=dpdTW,dpdn=dpdnW)
    pX = pressure(t,vX,X,dpdv=dpdvX,dpdT=dpdTX,dpdn=dpdnX)

    do i=1,nc
      if (Z(i) > 0.0) then
        dWdlnKX(i) = - Kx(i)*(1.0-beta)*W(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
        dWdlnKY(i) = - Ky(i)*beta*W(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
        dXdlnKX(i) = Kx(i)*W(i) + Kx(i)*dWdlnKX(i)
        dXdlnKY(i) = Kx(i)*dWdlnKY(i)
        dYdlnKX(i) = Ky(i)*dWdlnKX(i)
        dYdlnKY(i) = Ky(i)*W(i) + Ky(i)*dWdlnKY(i)
        dWdbeta(i) = - (Ky(i)-Kx(i))*W(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
        dXdbeta(i) = Kx(i)*dWdbeta(i)
        dYdbeta(i) = Ky(i)*dWdbeta(i)
      else
        dWdlnKX(i) = 0.0
        dWdlnKY(i) = 0.0
        dXdlnKX(i) = 0.0
        dXdlnKY(i) = 0.0
        dYdlnKX(i) = 0.0
        dYdlnKY(i) = 0.0
        dWdbeta(i) = 0.0
        dXdbeta(i) = 0.0
        dYdbeta(i) = 0.0
      endif
    enddo

    Jac = 0
    ! W and X differentials
    if (.not. partial) then
      do i=1,nc
        Jac(1:nc,i+nc) = lnFugXn(:,i)*dXdlnKY(i) - lnFugWn(:,i)*dWdlnKY(i)
        Jac(1:nc,i) = lnFugXn(:,i)*dXdlnKX(i) - lnFugWn(:,i)*dWdlnKX(i)
        Jac(nc+1:2*nc,i) = lnFugYn(:,i)*dYdlnKX(i) - lnFugWn(:,i)*dWdlnKX(i)
        Jac(nc+1:2*nc,i+nc) = lnFugYn(:,i)*dYdlnKY(i) - lnFugWn(:,i)*dWdlnKY(i)
      enddo
    else
      do i=1,nc
        Jac(i,i) = 1
        Jac(nc+i,nc+i) = 1
      enddo
    endif
    Jac(2*nc+1,1:nc) = dWdlnKX
    Jac(2*nc+1,1+nc:2*nc) = dWdlnKY
    Jac(2*nc+2,1:nc) = dYdlnKX - dXdlnKX
    Jac(2*nc+2,1+nc:2*nc) = dYdlnKY - dXdlnKY

    ! Temperature differential
    Jac(1:nc,2*nc+1) = T*(lnFugXT-lnFugWT)
    Jac(1+nc:2*nc,2*nc+1) = T*(lnFugYT-lnFugWT)

    ! Volume differential
    Jac(1:nc,2*nc+4) = vX*lnFugXv
    Jac(1:nc,2*nc+5) = -vW*lnFugWv
    Jac(1+nc:2*nc,2*nc+3) = vY*lnFugYv
    Jac(1+nc:2*nc,2*nc+5) = -vW*lnFugWv

    ! Beta
    do i=1,nc
      Jac(1:nc,2*nc+2) = Jac(1:nc,2*nc+2) + &
           lnFugXn(:,i)*dXdbeta(i) - lnFugWn(:,i)*dWdbeta(i)
      Jac(1+nc:2*nc,2*nc+2) = Jac(1+nc:2*nc,2*nc+2) + &
           lnFugYn(:,i)*dYdbeta(i) - lnFugWn(:,i)*dWdbeta(i)
    enddo
    Jac(2*nc+1,2*nc+2) = sum(dWdbeta)
    Jac(2*nc+2,2*nc+2) = sum(dYdbeta-dXdbeta)

    ! Volume average for pressure error scaling
    vS = 3/(1/vW + 1/vX + 1/vY)
    if (debug_tv) dvSdvW_div_vS = 1/(1/vW + 1/vX + 1/vY)/vW**2
    if (debug_tv) dvSdvX_div_vS = 1/(1/vW + 1/vX + 1/vY)/vX**2
    if (debug_tv) dvSdvY_div_vS = 1/(1/vW + 1/vX + 1/vY)/vY**2

    ! Factor scaling pressure equations
    fac = vS/(Rgas*T)
    ! Specification
    if (s <= 2*nc+5) then
      Jac(2*nc+3,s) = 1.0
    else if (s == 2*nc+6) then
      ! Fixate pressure
      p = exp(lns)
      Jac(2*nc+3,1:nc) = -dPdnW*dWdlnKx*fac
      Jac(2*nc+3,nc+1:2*nc) = -dPdnW*dWdlnKy*fac
      Jac(2*nc+3,2*nc+1) = -T*dPdTw*fac
      if (debug_tv) Jac(2*nc+3,2*nc+1) = Jac(2*nc+3,2*nc+1) - (P-Pw)*fac
      Jac(2*nc+3,2*nc+2) = -sum(dPdnW*dWdbeta)*fac
      Jac(2*nc+3,2*nc+5) = -vW*dpdvW*fac
      if (debug_tv) Jac(2*nc+3,2*nc+3) = Jac(2*nc+3,2*nc+3) + vY*dvSdvY_div_vS*(P-Pw)*fac
      if (debug_tv) Jac(2*nc+3,2*nc+4) = Jac(2*nc+3,2*nc+4) + vX*dvSdvX_div_vS*(P-Pw)*fac
      if (debug_tv) Jac(2*nc+3,2*nc+5) = Jac(2*nc+3,2*nc+5) + vW*dvSdvW_div_vS*(P-Pw)*fac
    endif

    ! Pressure differentials
    Jac(2*nc+4,1:nc) = (dPdnX*dXdlnKx-dPdnW*dWdlnKx)*fac
    Jac(2*nc+4,nc+1:2*nc) = (dPdnX*dXdlnKy-dPdnW*dWdlnKy)*fac
    Jac(2*nc+5,1:nc) = (dPdnY*dYdlnKx-dPdnW*dWdlnKx)*fac
    Jac(2*nc+5,nc+1:2*nc) = (dPdnY*dYdlnKy-dPdnW*dWdlnKy)*fac
    !
    Jac(2*nc+4,2*nc+2) = sum(dPdnX*dXdbeta-dPdnW*dWdbeta)*fac
    Jac(2*nc+5,2*nc+2) = sum(dPdnY*dYdbeta-dPdnW*dWdbeta)*fac
    !
    Jac(2*nc+4,2*nc+1) = T*(dPdTx-dPdTw)*fac
    if (debug_tv) Jac(2*nc+4,2*nc+1) = Jac(2*nc+4,2*nc+1) - (Px-Pw)*fac
    Jac(2*nc+5,2*nc+1) = T*(dPdTy-dPdTw)*fac
    if (debug_tv) Jac(2*nc+5,2*nc+1) = Jac(2*nc+5,2*nc+1) - (Py-Pw)*fac
    Jac(2*nc+4,2*nc+4) = vX*dpdvX*fac
    Jac(2*nc+4,2*nc+5) = -vW*dpdvW*fac
    if (debug_tv) Jac(2*nc+4,2*nc+3) = Jac(2*nc+4,2*nc+3) + vY*dvSdvY_div_vS*(Px-Pw)*fac
    if (debug_tv) Jac(2*nc+4,2*nc+4) = Jac(2*nc+4,2*nc+4) + vX*dvSdvX_div_vS*(Px-Pw)*fac
    if (debug_tv) Jac(2*nc+4,2*nc+5) = Jac(2*nc+4,2*nc+5) + vW*dvSdvW_div_vS*(Px-Pw)*fac
    Jac(2*nc+5,2*nc+3) = vY*dpdvY*fac
    Jac(2*nc+5,2*nc+5) = -vW*dpdvW*fac
    if (debug_tv) Jac(2*nc+5,2*nc+3) = Jac(2*nc+5,2*nc+3) + vY*dvSdvY_div_vS*(Py-Pw)*fac
    if (debug_tv) Jac(2*nc+5,2*nc+4) = Jac(2*nc+5,2*nc+4) + vX*dvSdvX_div_vS*(Py-Pw)*fac
    if (debug_tv) Jac(2*nc+5,2*nc+5) = Jac(2*nc+5,2*nc+5) + vW*dvSdvW_div_vS*(Py-Pw)*fac
  end subroutine three_ph_line_diff_newton

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along the saturation line
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine extrapolate_three_ph_line_tv(param,XXold,XX,dXdS,dlns,s,sgn,&
       Pmin,Pmax,Tmin,lns,only_calc_dxds,is_first_iteration)
    implicit none
    real, dimension(nc+3), intent(inout) :: param
    real, dimension(2*nc+5), intent(out) :: dXdS
    real, dimension(2*nc+5), intent(inout) :: XXold
    real, dimension(2*nc+5), intent(inout) :: XX
    integer, intent(inout) :: s
    real, intent(inout) :: sgn, dlns, lns
    real, intent(in) :: Pmin,Pmax,Tmin
    logical :: only_calc_dxds, is_first_iteration
    ! Locals
    real, dimension(2*nc+5,2*nc+5) :: Jac
    integer, dimension(2*nc+5) :: INDX
    integer :: INFO, snew
    real, parameter :: betaLimit = 1.0e-4
    integer :: beta_idx
    real :: dpds, vS
    real :: W(nc),X(nc),Y(nc),beta,t,p,vW,vX,vY,p_new
    param(nc+2) = real(s)
    param(nc+3) = 0.0 ! Partial off

    call three_ph_line_diff_newton(Jac,XX,param)
    dXdS = 0.0
    if (s <= 2*nc+5) then
      dXdS(2*nc+3) = 1.0
    else
      ! Specified pressure
      call read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY)
      ! Volume average for pressure error scaling
      vS = 3/(1/vW + 1/vX + 1/vY)
      ! dlnP
      dXdS(2*nc+3) = -p*vS/(Rgas*T)
    endif

    ! Solve equation system
    call DGESV( 2*nc+5, 1, Jac, 2*nc+5, INDX, dXdS, 2*nc+5, INFO )
    if (only_calc_dxds) return

    ! Change variable?
    snew = maxloc(abs(dXdS),dim=1)
    if ((.not. snew == s)) then
      ! Rescaling the sensitivities
      if (s == 2*nc+6) then
        if (.not. is_first_iteration) then
          sgn = sign(1.0,XX(snew) - XXold(snew))
          s = snew
          dXdS = dXdS / dXdS(s)
        endif
      else
        sgn = sign(1.0,dXdS(snew)*dXdS(s))*sgn
        s = snew
        dXdS = dXdS / dXdS(s)
      endif
    endif

    ! Update XX
    XXold = XX
    XX = XX + dXds*sgn*dlns

    if (s == 2*nc+6) then
      lns = log(p) + dlns*sgn
    else
      lns = XX(s)
    endif

    beta_idx = 2*nc+2
    ! Test for: 0.0 <= beta <= 1.0
    if (XX(beta_idx) > 1.0) then
      dlns = (1.0 - XXold(beta_idx))/dXds(beta_idx)*sgn
      s = beta_idx
      XX = XXold + dXds*sgn*dlns
      lns = 1.0
    else if (XX(beta_idx) < 0.0) then
      dlns = XX(beta_idx)/dXds(beta_idx)*sgn
      s = beta_idx
      XX = XXold + dXds*sgn*dlns
      lns = 0.0
    endif

    ! Test for: Pmin <= P <= Pmax
    call calc_dpds(XXold,dXds,param,p,dpds)
    p_new = p + dpds*sgn*dlns
    if (p_new > Pmax) then
      dlns = (Pmax - p)/(dpds*sgn)
      s = 2*nc+6
      XX = XXold + dXds*sgn*dlns
      lns = log(Pmax)
    else if (p_new < Pmin) then
      dlns = (Pmin - p)/(dpds*sgn)
      s = 2*nc+6
      XX = XXold + dXds*sgn*dlns
      lns = log(Pmin)
    endif

    ! Test for: Tmin <= T
    if (XX(2*nc+1) < log(Tmin) .and. dXds(2*nc+1)*sgn < 0.0) then
      dlns = (log(Tmin) - XXold(2*nc+1))/dXds(2*nc+1)*sgn
      s = 2*nc+1
      XX = XXold + dXds*sgn*dlns
      lns = log(Tmin)
    endif

    ! Test for decreasing beta and beta < betaLimit
    if (XX(beta_idx) < betaLimit .and. dXds(beta_idx)*sgn < -betaLimit) then
      dlns = -XXold(beta_idx)/dXds(beta_idx)*sgn
      s = beta_idx
      XX = XXold + dXds*sgn*dlns
      lns = 0.0
    endif

    ! Test for increasing beta and beta > 1.0-betaLimit
    if (XX(beta_idx) > 1.0-betaLimit .and. dXds(beta_idx)*sgn > betaLimit) then
      dlns = (1.0-XXold(beta_idx))/dXds(beta_idx)*sgn
      s = beta_idx
      XX = XXold + dXds*sgn*dlns
      lns = 1.0
    endif

    !call print_Xvar_tv(XX,param,"Extrapolated state")
  end subroutine extrapolate_three_ph_line_tv

  !-----------------------------------------------------------------------------
  !> Test variable sensitivities along the saturation line
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine three_ph_newton_tv_extrapolate_test()
    use saturation, only: safe_dewT
    use eos, only: specificvolume
    implicit none
    ! Locals
    real, dimension(2*nc+5) :: Xvar, Xvar1, Xvar2
    real, parameter :: Xvar0(11) = (/8.2094431303631286, 16.558563369597643, &
         -6.0789515586576170, 8.0622774660287710, 19.007741713255712, -9.2100724083490260, &
         5.5735578415932991, 1.0000000000000000, -7.6018922636034283, -9.8878584799437164, &
         -10.666046734192035/)
    real, dimension(nc+3) :: param, param1
    real, dimension(2*nc+5) :: dXdS
    real :: p, T, beta, ln_s, dpds, p1
    integer :: s, ierr, iter
    real :: Y(nc), X(nc), Z(nc), dlns, sgn, W(nc)
    real :: vY, vX, vW
    real :: error_on_exit

    !call init_cubic("CO2,N2,H2O","SRK")
    z = (/0.85,0.1499,0.0001/)
    param(1:nc) = Z
    param(nc+3) = 0

    ! Fixate beta
    s = 2*nc + 2
    ln_s = 1
    param(nc+1) = ln_s
    param(nc+2) = real(s)
    Xvar = Xvar0
    call three_ph_line_newton(param,Xvar,s,ln_s,.false.,50,ierr,3,error_on_exit,iter)
    dXds = 0
    call calc_dpds(Xvar,dXds,param,p,dpds)

    ! Fixate pressure
    s = 2*nc+6
    ln_s = log(p)
    param(1:nc) = Z
    param(nc+2) = real(s)
    param(nc+1) = ln_s

    dlns=0.0
    sgn = 1
    call three_ph_line_newton(param,Xvar,s,ln_s,.false.,50,ierr,3,error_on_exit,iter)
    Xvar2 = Xvar
    call extrapolate_three_ph_line_tv(param,Xvar,Xvar2,dXdS,dlns=dlns,s=s,sgn=sgn,&
         Pmin=1.0e5,Pmax=1.0e7,Tmin=180.0,lns=ln_s,only_calc_dxds=.true.,&
         is_first_iteration=.false.)
    call calc_dpds(Xvar,dXds,param,p,dpds)

    ! Perturb P
    ln_s = log(p-1.0)
    param1 = param
    param1(nc+1) = ln_s
    Xvar1 = Xvar
    call three_ph_line_newton(param1,Xvar1,s,ln_s,.false.,50,ierr,3,error_on_exit,iter)

    print *,"Pressure:"
    print *,(Xvar1-Xvar)/(param1(nc+1)-param(nc+1))
    print *,dXdS

    s = 2*nc + 1
    ln_s = Xvar(s)
    param(nc+2) = real(s)
    param(nc+1) = ln_s
    Xvar2 = Xvar
    call extrapolate_three_ph_line_tv(param,Xvar,Xvar2,dXdS,dlns=dlns,s=s,sgn=sgn,&
         Pmin=1.0e5,Pmax=1.0e7,Tmin=180.0,lns=ln_s,only_calc_dxds=.true.,&
         is_first_iteration=.false.)
    call calc_dpds(Xvar,dXds,param,p,dpds)

    ! Perturb T
    param1 = param
    ln_s = ln_s - 1.0e-3
    param1(nc+1) = ln_s
    Xvar1 = Xvar
    call three_ph_line_newton(param1,Xvar1,s,ln_s,.false.,50,ierr,3,error_on_exit,iter)
    call read_Xvar_and_param_tv(Xvar1,param1,W,X,Y,beta,t,p1,vW,vX,vY)

    print *,"Temperature:"
    print *,(Xvar1-Xvar)/(param1(nc+1)-param(nc+1))
    print *,dXdS

    print *,"dpds:"
    print *,dpds,(p1-p)/(param1(nc+1)-param(nc+1))

  end subroutine three_ph_newton_tv_extrapolate_test

  !-----------------------------------------------------------------------------
  !> Fill variable vector
  !>
  !> \author Morten Hammer, 2021-11
  !-----------------------------------------------------------------------------
  subroutine calc_dpds(XX,dXds,param,p,dpds)
    implicit none
    real, dimension(2*nc+5), intent(in) :: XX, dXds
    real, dimension(nc+3), intent(in) :: param
    real, intent(out) :: p, dpds
    ! Locals
    integer :: i
    real, dimension(nc) :: dWdlnKX, dWdlnKY, dWdbeta, Ky, Kx
    real :: W(nc),X(nc),Y(nc),beta,t,vW,vX,vY,Z(nc)
    real :: dpdv,dpdt,dpdn(nc)
    !
    call read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY)
    p = pressure(T,vW,W,dpdv=dpdv,dpdt=dpdt,dpdn=dpdn)
    Z = param(1:nc)

    Kx = exp(XX(1:nc))
    Ky = exp(XX(1+nc:2*nc))
    do i=1,nc
      if (Z(i) > 0.0) then
        dWdlnKX(i) = - Kx(i)*(1.0-beta)*W(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
        dWdlnKY(i) = - Ky(i)*beta*W(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
        dWdbeta(i) = - (Ky(i)-Kx(i))*W(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
      else
        dWdlnKX(i) = 0.0
        dWdlnKY(i) = 0.0
      endif
    enddo

    dpds = vW*dpdv*dxds(2*nc+5) + T*dpdt*dxds(2*nc+1) + &
         sum(dpdn*dWdlnKx*dxds(1:nc)) + sum(dpdn*dWdlnKy*dxds(nc+1:2*nc)) + &
         sum(dpdn*dWdbeta*dxds(2*nc+2))

  end subroutine calc_dpds

  !-----------------------------------------------------------------------------
  !> Fill variable vector
  !>
  !> \author Morten Hammer, 2015-09
  !-----------------------------------------------------------------------------
  subroutine fill_Xvar_and_param_tv(XX,param,Z,W,X,Y,beta,t,p,vW,vX,&
       vY,s,lns)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(in) :: W,X,Y
    real, intent(in) :: p
    real, intent(in) :: t
    real, intent(in) :: beta
    real, intent(in)    :: lns
    real, intent(in) :: vX, vW, vY
    integer, intent(in) :: s
    real, dimension(nc+3), intent(out) :: param
    real, dimension(2*nc+5), intent(out) :: XX
    ! Locals
    integer :: i

    do i=1,nc
      if (Z(i) > 0.0) then
        XX(i) = log(X(i)/W(i))
        XX(i+nc) = log(Y(i)/W(i))
      else
        XX(i) = 0.0
        XX(i+nc) = 0.0
      endif
    enddo
    XX(2*nc+1) = log(t)
    XX(2*nc+2) = beta
    XX(2*nc+3) = log(vY)
    XX(2*nc+4) = log(vX)
    XX(2*nc+5) = log(vW)

    param(1:nc) = Z
    param(nc+1) = lns
    param(nc+2) = real(s)
    param(nc+3) = 0.0 ! Default not partial
  end subroutine fill_Xvar_and_param_tv

  !-----------------------------------------------------------------------------
  !> Read variable vector
  !>
  !> \author Morten Hammer, 2021-11
  !-----------------------------------------------------------------------------
  subroutine read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY,no_press_calc)
    implicit none
    real, dimension(nc), intent(out) :: W,X,Y
    real, intent(out) :: p
    real, intent(out) :: t
    real, intent(out) :: beta
    real, intent(out) :: vX, vW, vY
    real, dimension(nc+3), intent(in) :: param
    real, dimension(2*nc+5), intent(in) :: XX
    logical, optional, intent(in) :: no_press_calc
    ! Locals
    integer :: i
    real, dimension(nc) :: Kx, Ky, Z
    logical :: no_press_calc_local
    Z = param(1:nc)
    t = exp(XX(2*nc+1))
    beta = XX(2*nc+2)
    vY = exp(XX(2*nc+3))
    vX = exp(XX(2*nc+4))
    vW = exp(XX(2*nc+5))
    Kx = exp(XX(1:nc))
    Ky = exp(XX(1+nc:2*nc))
    W = 0.0
    do i=1,nc
      if (Z(i) > 0.0) then
        W(i) = Z(i)/(beta*Ky(i)+(1.0-beta)*Kx(i))
      endif
    enddo
    Y = Ky*w
    X = Kx*w
    no_press_calc_local = .false.
    if (present(no_press_calc)) then
      no_press_calc_local = no_press_calc
    endif
    if (no_press_calc_local) then
      p = 0
    else
      p = pressure(t,vY,Y)
    endif
  end subroutine read_Xvar_and_param_tv

  !-----------------------------------------------------------------------------
  !> Get pressure
  !>
  !> \author Morten Hammer, 2021-11
  !-----------------------------------------------------------------------------
  function pressure_Xvar_tv(XX,param) result(p)
    implicit none
    real, dimension(nc+3), intent(in) :: param
    real, dimension(2*nc+5), intent(in) :: XX
    real :: p
    ! Locals
    real, dimension(nc) :: W,X,Y
    real :: t
    real :: beta
    real :: vX, vW, vY

    call read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY)
  end function pressure_Xvar_tv

  !-----------------------------------------------------------------------------
  !> Print variable vector
  !>
  !> \author Morten Hammer, 2021-11
  !-----------------------------------------------------------------------------
  subroutine print_Xvar_tv(XX,param,message)
    implicit none
    real, dimension(nc+3), intent(in) :: param
    real, dimension(2*nc+5), intent(in) :: XX
    character(len=*), intent(in) :: message
    ! Locals
    real, dimension(nc) :: W,X,Y
    real :: p
    real :: t
    real :: beta
    real :: vX, vW, vY

    call read_Xvar_and_param_tv(XX,param,W,X,Y,beta,t,p,vW,vX,vY)
    print *,""
    print *,"Three-phase state: ",message
    print *,"T: ",T
    print *,"p: ",p
    print *,"vY: ",vY
    print *,"vX: ",vX
    print *,"vW: ",vW
    print *,"beta: ",beta
    print *,"vY: ",vY
    print *,"vX: ",vX
    print *,"vW: ",vW
    print *,"Y: ",Y
    print *,"X: ",X
    print *,"W: ",W
  end subroutine print_Xvar_tv

  subroutine three_phase_line_point_print(tplp, message)
    implicit none
    class(three_phase_line_point), intent(in) :: tplp
    character(len=*), intent(in), optional :: message
    !
    print *,""
    if (present(message)) then
      print *,trim(message)
    else
      print *,"Three-phase line point:"
    endif

    print *,"T: ",tplp%T
    print *,"p: ",tplp%p
    print *,"vY: ",tplp%vY
    print *,"vX: ",tplp%vX
    print *,"vW: ",tplp%vW
    print *,"beta: ",tplp%beta
    print *,"vY: ",tplp%vY
    print *,"vX: ",tplp%vX
    print *,"vW: ",tplp%vW
    print *,"Y: ",tplp%Y
    print *,"X: ",tplp%X
    print *,"W: ",tplp%W
  end subroutine three_phase_line_point_print

  subroutine three_phase_line_push_back(tpline,T,p,beta,W,X,Y,&
       vW,vX,vY)
    implicit none
    class(three_phase_line), intent(inout) :: tpline
    real, intent(in) :: T
    real, intent(in) :: p
    real, intent(in) :: beta
    real, intent(in) :: W(nc), X(nc), Y(nc)
    real, intent(in) :: vW, vX, vY
    ! Locals
    type(three_phase_line) :: tpline_cpy
    integer :: ierr, newStorage, i
    newStorage = 0
    if (allocated(tpline%tpl)) then
      newStorage = tpline%n + 1
    else
      tpline%nAlloc = 0
      newStorage = 1
    endif
    if (newStorage > tpline%nAlloc) then
      allocate(tpline_cpy%tpl(newStorage),STAT=ierr)
      tpline_cpy%nAlloc = newStorage
      if (ierr /= 0) then
        call stoperror('Not able to allocate tpline_cpy%tpl')
      endif
      do i=1,newStorage
        allocate(tpline_cpy%tpl(i)%W(nc),&
             tpline_cpy%tpl(i)%X(nc),&
             tpline_cpy%tpl(i)%Y(nc),stat=ierr)
        if (ierr /= 0) then
          call stoperror('Not able to allocate tpline_cpy%tpl%W/X/Y')
        endif
      enddo
      do i=1,newStorage-1
        tpline_cpy%tpl(i)%T = tpline%tpl(i)%T
        tpline_cpy%tpl(i)%p = tpline%tpl(i)%p
        tpline_cpy%tpl(i)%vY = tpline%tpl(i)%vY
        tpline_cpy%tpl(i)%vX = tpline%tpl(i)%vX
        tpline_cpy%tpl(i)%vW = tpline%tpl(i)%vW
        tpline_cpy%tpl(i)%beta = tpline%tpl(i)%beta
        tpline_cpy%tpl(i)%W = tpline%tpl(i)%W
        tpline_cpy%tpl(i)%X = tpline%tpl(i)%X
        tpline_cpy%tpl(i)%Y = tpline%tpl(i)%Y
        tpline_cpy%tpl(i)%vW = tpline%tpl(i)%vW
        tpline_cpy%tpl(i)%vX = tpline%tpl(i)%vX
        tpline_cpy%tpl(i)%vY = tpline%tpl(i)%vY
      enddo
      do i=1,tpline%nAlloc
        deallocate(tpline%tpl(i)%W,&
             tpline%tpl(i)%X,&
             tpline%tpl(i)%Y,stat=ierr)
        if (ierr /= 0) then
          call stoperror('Not able to deallocate tpline%tpl%W/X/Y')
        endif
      enddo
      if (allocated(tpline%tpl)) then
        deallocate(tpline%tpl,STAT=ierr)
        if (ierr /= 0) then
          call stoperror('Not able to deallocate tpline')
        endif
      endif
      tpline%nAlloc = tpline%nAlloc + 10
      allocate(tpline%tpl(tpline%nAlloc),STAT=ierr)
      if (ierr /= 0) then
        call stoperror('Not able to allocate tpline')
      endif
      do i=1,newStorage-1
        tpline%tpl(i)%T = tpline_cpy%tpl(i)%T
        tpline%tpl(i)%p = tpline_cpy%tpl(i)%p
        tpline_cpy%tpl(i)%vY = tpline_cpy%tpl(i)%vY
        tpline_cpy%tpl(i)%vX = tpline_cpy%tpl(i)%vX
        tpline_cpy%tpl(i)%vW = tpline_cpy%tpl(i)%vW
        tpline%tpl(i)%beta = tpline_cpy%tpl(i)%beta
        tpline%tpl(i)%W = tpline_cpy%tpl(i)%W
        tpline%tpl(i)%X = tpline_cpy%tpl(i)%X
        tpline%tpl(i)%Y = tpline_cpy%tpl(i)%Y
        tpline%tpl(i)%vW = tpline_cpy%tpl(i)%vW
        tpline%tpl(i)%vX = tpline_cpy%tpl(i)%vX
        tpline%tpl(i)%vY = tpline_cpy%tpl(i)%vY
      enddo
      do i=1,tpline_cpy%nAlloc
        deallocate(tpline_cpy%tpl(i)%W,&
             tpline_cpy%tpl(i)%X,&
             tpline_cpy%tpl(i)%Y,stat=ierr)
        if (ierr /= 0) then
          call stoperror('Not able to deallocate tpline_cpy%tpl%W/X/Y')
        endif
      enddo
    endif
    tpline%tpl(newStorage)%T = T
    tpline%tpl(newStorage)%p = p
    tpline%tpl(newStorage)%vY = vY
    tpline%tpl(newStorage)%vX = vX
    tpline%tpl(newStorage)%vW = vW
    tpline%tpl(newStorage)%beta = beta
    tpline%tpl(newStorage)%W = W
    tpline%tpl(newStorage)%X = X
    tpline%tpl(newStorage)%Y = Y
    tpline%tpl(newStorage)%vW = vW
    tpline%tpl(newStorage)%vX = vX
    tpline%tpl(newStorage)%vY = vY
    tpline%n = newStorage
  end subroutine three_phase_line_push_back

  subroutine three_phase_line_dealloc(tplines)
    implicit none
    class(three_phase_line), intent(inout) :: tplines
    !
    integer :: ierr
    if (allocated(tplines%tpl)) then
      deallocate(tplines%tpl,stat=ierr)
      if (ierr /= 0) then
        call stoperror('three_phase_line_dealloc failed to deallocate memory')
      endif
    endif
  end subroutine three_phase_line_dealloc

  subroutine two_phase_line_dealloc(tplines)
    implicit none
    class(two_phase_line), intent(inout) :: tplines
    !
    integer :: ierr
    call tplines%three_phase_line%dealloc()
    ierr = 0
    if (allocated(tplines%X1)) deallocate(tplines%X1,stat=ierr)
    if (allocated(tplines%X2)) deallocate(tplines%X2,stat=ierr)
    if (allocated(tplines%K)) deallocate(tplines%K,stat=ierr)
    if (allocated(tplines%T)) deallocate(tplines%T,stat=ierr)
    if (allocated(tplines%P)) deallocate(tplines%P,stat=ierr)
    if (allocated(tplines%v1a)) deallocate(tplines%v1a,stat=ierr)
    if (allocated(tplines%v2a)) deallocate(tplines%v2a,stat=ierr)
    if (allocated(tplines%beta1)) deallocate(tplines%beta1,stat=ierr)
    if (ierr /= 0) then
      call stoperror('two_phase_line_dealloc failed to deallocate memory')
    endif
  end subroutine two_phase_line_dealloc

  subroutine three_phase_lines_dealloc(tplines)
    implicit none
    type(three_phase_line), dimension(:), intent(inout) :: tplines
    !
    integer :: i
    do i=1,size(tplines)
      call tplines(i)%dealloc()
    enddo
  end subroutine three_phase_lines_dealloc

  subroutine two_phase_lines_dealloc(tplines)
    implicit none
    type(two_phase_line), dimension(:), intent(inout) :: tplines
    !
    integer :: i
    do i=1,size(tplines)
      call tplines(i)%dealloc()
    enddo
  end subroutine two_phase_lines_dealloc

  subroutine two_phase_lines_alloc(tplines, nmax)
    implicit none
    type(two_phase_line), dimension(:), intent(inout) :: tplines
    integer, intent(in) :: nmax
    !
    integer :: i
    do i=1,size(tplines)
      call tplines(i)%alloc(nmax)
    enddo
  end subroutine two_phase_lineS_alloc

  subroutine two_phase_line_alloc(tplines, nmax)
    implicit none
    class(two_phase_line), intent(inout) :: tplines
    integer, intent(in) :: nmax
    !
    integer :: ierr
    ierr = 0
    allocate(tplines%X1(nmax,nc), tplines%X2(nmax,nc), &
         tplines%K(nmax,nc), tplines%T(nmax), tplines%P(nmax),&
         tplines%beta1(nmax), tplines%v1a(nmax), tplines%v2a(nmax), &
         stat=ierr)
    tplines%n = 0
    if (ierr /= 0) then
      call stoperror('two_phase_line_alloc failed to allocate memory')
    endif
  end subroutine two_phase_line_alloc

  subroutine two_phase_line_set_x_from_K(tpline, Z)
    implicit none
    class(two_phase_line), intent(inout) :: tpline
    real, intent(in) :: z(:)
    !
    integer :: i
    do i=1,tpline%n
      tpline%x2(i,:) = Z/(1-tpline%beta1(i)+tpline%beta1(i)*tpline%K(i,:))
      tpline%x1(i,:) = tpline%K(i,:)*Z/(1-tpline%beta1(i)+tpline%beta1(i)*tpline%K(i,:))
    enddo
  end subroutine two_phase_line_set_x_from_K

  subroutine two_phase_line_copy_to_three_phase_structure(tpline, Z)
    implicit none
    class(two_phase_line), intent(inout) :: tpline
    real, intent(in) :: z(:)
    ! Locals
    real :: W(nc),X(nc),Y(nc),beta
    real :: vW,vX,vY
    integer :: i, n
    call tpline%set_X_from_K(Z)
    n = tpline%n ! Will be overwritten by three_phase_line push_back
    tpline%n = 0
    do i=1,n
      W = tpline%X2(i,:)
      vW = tpline%v2a(i)
      if (.not. tpline%is_LL) then
        beta = 1
        Y = tpline%X1(i,:)
        X = 0
        vY = tpline%v1a(i)
        vX = -1
      else
        beta = 0
        Y = 0
        X = tpline%X1(i,:)
        vY = -1
        vX = tpline%v1a(i)
      endif
      call tpline%push_back(tpline%T(i),tpline%p(i),beta,W,X,Y,&
           vW,vX,vY)
    enddo
  end subroutine two_phase_line_copy_to_three_phase_structure

  subroutine two_phase_line_append(tpline,tpps,Z)
    implicit none
    class(two_phase_line), intent(inout) :: tpline
    type(three_phase_point), intent(in) :: tpps
    real, intent(in) :: Z(nc)
    ! Locals
    integer :: n

    n = tpline%n + 1 ! Will be updated by three_phase_line push_back

    tpline%X1(n,:) = Z
    tpline%X2(n,:) = tpps%W
    tpline%t(n) = tpps%t
    tpline%p(n) = tpps%p
    tpline%beta1(n) = 1
    tpline%v1a(n) = tpps%vZ
    tpline%v2a(n) = tpps%vW
    tpline%K(n,:) = tpline%X1(n,:)/tpline%X2(n,:)
    call tpline%push_back(tpline%T(n),tpline%p(n),1.0,tpps%W,tpps%X,Z,&
         tpps%vW,tpps%vX,tpps%vZ)
  end subroutine two_phase_line_append

  subroutine two_phase_line_overwrite_last_point(tpline,tpps,Z)
    implicit none
    class(two_phase_line), intent(inout) :: tpline
    type(three_phase_point), intent(in) :: tpps
    real, intent(in) :: Z(nc)
    ! Locals
    integer :: n

    n = tpline%n

    tpline%X1(n,:) = Z
    tpline%X2(n,:) = tpps%W
    tpline%t(n) = tpps%t
    tpline%p(n) = tpps%p
    tpline%beta1(n) = 1
    tpline%v1a(n) = tpps%vZ
    tpline%v2a(n) = tpps%vW
    tpline%K(n,:) = tpline%X1(n,:)/tpline%X2(n,:)
    !
    tpline%n = tpline%n - 1  ! Will be advanced by one by three_phase_line push_back
    call tpline%push_back(tpline%T(n),tpline%p(n),1.0,tpps%W,tpps%X,Z,&
         tpps%vW,tpps%vX,tpps%vZ)
  end subroutine two_phase_line_overwrite_last_point

  subroutine three_phase_points_dealloc(tpps)
    implicit none
    type(three_phase_point), dimension(:), intent(inout) :: tpps
    !
    integer :: i, ierr
    do i=1,size(tpps)
      if (allocated(tpps(i)%W)) then
        deallocate(tpps(i)%W,tpps(i)%X,stat=ierr)
        if (ierr /= 0) then
          call stoperror('three_phase_points_dealloc failed to deallocate memory')
        endif
      endif
    enddo
  end subroutine three_phase_points_dealloc

  subroutine three_phase_points_alloc(tpps)
    implicit none
    type(three_phase_point), dimension(:), intent(inout) :: tpps
    !
    integer :: i, ierr
    do i=1,size(tpps)
      if (allocated(tpps(i)%W)) then
        allocate(tpps(i)%W(nc),tpps(i)%X(nc),stat=ierr)
        if (ierr /= 0) then
          call stoperror('three_phase_points_dealloc failed to deallocate memory')
        endif
      endif
    enddo
  end subroutine three_phase_points_alloc

  subroutine three_phase_point_print(tpp, message)
    implicit none
    class(three_phase_point), intent(in) :: tpp
    character(len=*), intent(in), optional :: message
    !
    print *,""
    if (present(message)) then
      print *,trim(message)
    else
      print *,"Three-phase point:"
    endif
    print *,"T: ",tpp%T
    print *,"P: ",tpp%P
    print *,"vZ: ",tpp%vZ
    print *,"vX: ",tpp%vX
    print *,"vW: ",tpp%vW
    print *,"X: ",tpp%X
    print *,"W: ",tpp%W
  end subroutine three_phase_point_print

  subroutine print_z_to_string(Z,Z_str)
    implicit none
    real,                intent(in)  :: Z(nc)       ! Total molar comp. (-)
    character(len=*),    intent(out) :: Z_str
    ! Locals
    integer :: i
    character(len=20) :: Zi_str
    write(Zi_str,'(es13.4e3)') Z(1)
    Z_str = "Z = ["//trim(Zi_str)
    do i=2,nc
      write(Zi_str,'(es13.4e3)') Z(i)
      Z_str = trim(Z_str)//","//trim(Zi_str)
    enddo
    Z_str = trim(Z_str)//"]"
  end subroutine print_z_to_string

  subroutine printTPtoFile(tps,n_tps,threepls,n_threepls,twopls,n_twopls,&
       Z,fname,printProperties)
    use thermopack_var, only: get_active_thermo_model, thermo_model
    implicit none
    type(three_phase_line), dimension(:), intent(in) :: threepls
    type(two_phase_line), dimension(:), intent(in) :: twopls
    type(three_phase_point), dimension(:), intent(in) :: tps
    integer, intent(in) :: n_threepls, n_twopls, n_tps
    real,           intent(in)  :: Z(nc)       ! Total molar comp. (-)
    character(len=*), intent(in) :: fname
    logical, optional, intent(in) :: printProperties
    ! Locals
    integer :: i, maxEntries, j
    integer, parameter :: fint = 12
    character(len=*), parameter :: sep = '  '
    character(len=*), parameter :: non = 'NaN' !'.' for gnu, 'NaN' for python
    character(len=clen) :: line, addon
    character(len=100) :: phaseline, emptyAddon
    character(len=19) :: number
    logical :: printProp
    real :: e, v, s
    character(len=clen) :: Z_str, model_str
    type(thermo_model), pointer :: p_eos
    ! Should properites be printed
    printProp = .false.
    if (present(printProperties)) then
      printProp = printProperties
    endif
    ! Determine maximum number of rows
    maxEntries = 0
    do i=1,n_threepls
      maxEntries = max(maxEntries,threepls(i)%n)
    enddo
    do i=1,n_twopls
      maxEntries = max(maxEntries,twopls(i)%n)
    enddo
    call print_z_to_string(Z,Z_str)
    p_eos => get_active_thermo_model()
    model_str = trim(p_eos%label) // " ("
    do i=1,nc
      if (i > 1) model_str = trim(model_str) // ","
      model_str = trim(model_str) // trim(p_eos%complist(i))
    enddo
    model_str = trim(model_str) // ")"
    ! Open file and write heading
    open(file=fname,unit=fint)
    write(fint,'(A)') '# Three phase lines, for model '//trim(model_str)
    write(fint,'(A)') '# Compozition '//trim(Z_str)
    do i=1,n_tps
      write(fint,'(A,2es19.10e3)') '# Three phase point (T K,P bar): ',&
           tps(i)%T,tps(i)%p*1.0e-5
    enddo
    phaseline = 'T (K)'//sep//'P (bar)'
    emptyAddon = non // sep // non
    if (printProp) then
      phaseline = trim(phaseline)//sep//'u (J/mol)'//sep//'s (J/mol/K)'//sep//'v (m3/mol)'
      emptyAddon = non // sep // non // sep // non // sep // non // sep // non
    endif
    line = '# '//trim(phaseline)
    do i=1,n_threepls+n_twopls-1
      line = trim(line)//','//trim(phaseline)
    enddo
    write(fint,'(A)') trim(line)
    ! Write lines
    do i=1,maxEntries
      line = ''
      do j=1,n_threepls
        if (threepls(j)%n >= i) then
          write(number,'(es19.10e3)') threepls(j)%tpl(i)%T
          addon = number
          write(number,'(es19.10e3)') threepls(j)%tpl(i)%p*1.0e-5
          addon = trim(addon)//sep//number
          if (printProp) then
            call getESV(threepls(j)%tpl(i)%T,threepls(j)%tpl(i)%p,&
                 threepls(j)%tpl(i)%X,threepls(j)%tpl(i)%Y,Z,&
                 threepls(j)%tpl(i)%beta,threepls(j)%tpl(i)%vX,&
                 threepls(j)%tpl(i)%vY,e,s,v)
            write(number,'(es19.10e3)') e
            addon = trim(addon)//sep//number
            write(number,'(es19.10e3)') s
            addon = trim(addon)//sep//number
            write(number,'(es19.10e3)') v
            addon = trim(addon)//sep//number
          endif
        else
          addon = trim(emptyAddon)
        endif
        if (len_trim(line) == 0) then
          line = trim(addon)
        else
          line = trim(line)//sep//trim(addon)
        endif
      enddo
      do j=1,n_twopls
        if (twopls(j)%n >= i) then
          write(number,'(es19.10e3)') twopls(j)%t(i)
          addon = number
          write(number,'(es19.10e3)') twopls(j)%p(i)*1.0e-5
          addon = trim(addon)//sep//number
          if (printProp) then
            call getESV(twopls(j)%t(i),twopls(j)%p(i),Z,Z,Z,&
                 twopls(j)%beta1(i),twopls(j)%v2a(i),twopls(j)%v1a(i),e,s,v)
            write(number,'(es19.10e3)') e
            addon = trim(addon)//sep//number
            write(number,'(es19.10e3)') s
            addon = trim(addon)//sep//number
            write(number,'(es19.10e3)') v
            addon = trim(addon)//sep//number
          endif
        else
          addon = trim(emptyAddon)
        endif
        if (len_trim(line) == 0) then
          line = trim(addon)
        else
          line = trim(line)//sep//trim(addon)
        endif
      enddo
      write(fint,'(A)') trim(line)
    enddo
    close(fint)
  end subroutine printTPtoFile

  subroutine getESV(T,P,X,Y,Z,beta,vX,vY,e,s,v)
    use eosTV, only: enthalpy_tv, entropy_tv
    implicit none
    ! Input:
    real,           intent(in)    :: T           !> T-guess and solution T (K)
    real,           intent(in)    :: p           !> Solution pressure (Pa)
    real,           intent(in)    :: Z(nc)       !> Total molar comp. (-)
    real,           intent(in)    :: beta        !> Y phase fraction
    real,           intent(in)    :: vY          !> Volume
    real,           intent(in)    :: vX          !> Volume
    real,           intent(in)    :: X(nc)       !> Phase composition
    real,           intent(in)    :: Y(nc)       !> Phase composition
    ! Output:
    real,           intent(out)   :: e           !> Internal energy (J/mol)
    real,           intent(out)   :: s           !> Specific entropy (J/mol/K)
    real,           intent(out)   :: v           !> Specific volume (m3/mol)
    ! Internal:
    real :: hY, hX, sY, sX, h

    if (beta == 0.0) then
      call enthalpy_tv(t,vX,Z,h)
      call entropy_tv(t,vX,Z,s)
      v = vX
    else if (beta == 1.0) then
      call enthalpy_tv(t,vY,Z,h)
      call entropy_tv(t,vY,Z,s)
      v = vY
    else
      ! Two phases
      call enthalpy_tv(t,vY,Y,hY)
      call entropy_tv(t,vY,Y,sY)
      call enthalpy_tv(t,vX,X,hX)
      call entropy_tv(t,vX,X,sX)
      h = beta*hY + (1.0-beta)*hX
      s = beta*sY + (1.0-beta)*sX
      v = beta*vY + (1.0-beta)*vX
    endif
    e = h - p*v
  end subroutine getESV

end module multi_phase_envelope_tv
