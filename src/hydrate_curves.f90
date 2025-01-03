!---------------------------------------------------------------------
! Module for calculating Hydrate appearance curves
! Programmed by: M. Hammer
!---------------------------------------------------------------------
module hydrate_curves
  use hydrate
  use thermopack_constants, only: VAPPH, LIQPH, SOLIDPH
  use thermopack_var, only: Rgas, nc, tpTmin, tpTmax
  use eostv, only: thermo_tv, pressure
  implicit none
  private

  logical, parameter :: debug_hyd = .false.

  type :: multi_phase_point
    real, allocatable :: X1(:), X2(:), X3(:), K1(:), K2(:)
    real :: beta1, beta2, beta3
    real :: v1, v2, v3
    real :: T, P
    real :: nph
  contains
    procedure, public :: alloc => multi_phase_point_alloc
    procedure, public :: dealloc => multi_phase_point_dealloc
  end type multi_phase_point

  type :: multi_phase_point_list
    type(multi_phase_point), allocatable :: list(:)
    real, allocatable :: Z(:)
    integer :: n ! Points stored in array
  contains
    procedure, public :: add_point_twoph => multi_phase_point_list_add_point_twoph
    procedure, public :: add_point_threeph => multi_phase_point_list_add_point_threeph
    procedure, public :: add_single_phase_curve => multi_phase_point_list_add_single_phase_curve
    procedure, public :: add_tppl => multi_phase_point_list_add_tppl
    procedure, public :: write_file => multi_phase_point_list_write_file
    procedure, public :: write_arrays => multi_phase_point_list_write_arrays
    procedure, public :: init => multi_phase_point_list_init
    procedure, public :: dealloc => multi_phase_point_list_dealloc
    procedure, public :: print => multi_phase_point_list_print_arrays
    procedure, public :: reset => multi_phase_point_list_reset
  end type multi_phase_point_list

  public :: map_hydrate_appearance_curve
  ! Testing
  public :: newton_single_phase_fluid_hydrate_curve
  public :: hyd_newton_tv_extrapolate_test
  public :: newton_two_phase_fluid_hydrate_curve
  public :: hyd_twoph_newton_tv_extrapolate_test
  public :: hyd_twoph_fun_newton_tv

contains

  !-----------------------------------------------------------------!
  subroutine map_hydrate_appearance_curve(z, p_dew_start, p_max, T_min, &
       n_max_hyd, n_hyd, T_hyd, P_hyd, print_to_file)
  !-----------------------------------------------------------------!
    use saturation, only: specP
    use multi_phase_envelope_tv, only: multi_phase_envelope_plot_tv, &
         fill_Xvar_and_param_tv, print_Xvar_tv
    use eostv, only: thermo_tv, pressure
    use saturation_tv, only: set_variables_tv, print_envelope_point_tv
    implicit none
    real, intent(in)  :: Z(nc)       ! Total molar comp. (-)
    real, intent(in) :: p_dew_start, p_max, T_min
    integer, intent(in) :: n_max_hyd
    integer, intent(out) :: n_hyd
    real, intent(out) :: T_hyd(n_max_hyd), P_hyd(n_max_hyd)
    logical, optional, intent(in) :: print_to_file
    ! Internal
    integer, parameter :: nmx = 50000, nmxs = 1000
    integer        :: nwm               ! Number of output points for water appearance curve
    real           :: Tam(nmx)          ! Temperature (K)
    real           :: Pam(nmx)          ! Pressure (Pa)
    real           :: betam_YXW(3,nmx)  ! Phase molar fraction (-)
    real           :: v_YXW(3,nmx)      ! Molar volume (-)
    real           :: Ym(nc,nmx)        ! Phase Y composition (-)
    real           :: Xm(nc,nmx)        ! Phase X composition (-)
    real           :: Wm(nc,nmx)        ! Phase W composition (-)
    integer :: n_sat, i, ns, n_cross
    real :: fug_h_a(nmx), fug_w_a(nmx)
    real :: fug(nc)
    real :: X1(nc+3), X2(nc+3), param(nc+3), Xsol(nc+3)
    real :: X1_3ph(2*nc+5), X2_3ph(2*nc+5), Xsol_3ph(2*nc+5)
    real :: Ta(nmxs),va(nmxs)
    real :: T,P,v,beta
    type(multi_phase_point_list) :: tppl, tppl_comprised
    call tppl%init(1000,nc,Z)
    call tppl_comprised%init(4000,nc,Z)
    !
    call multi_phase_envelope_plot_tv(Z,p_dew_start,p_max,T_min,&
         nmx,n_sat,nwm,Tam,Pam,betam_YXW,Ym,Xm,Wm,v_YXW,print_to_file=.false.)
    do i=nwm+1,n_sat
      ! print *,"***********************"
      ! print *,"i",i,n_sat
      ! print *,"T,P",Tam(i),Pam(i)
      ! print *,"beta",betam_YXW(:,i)
      ! print *,"v",v_YXW(:,i)
      ! print *,"Y",Ym(:,i)
      ! print *,"X",Xm(:,i)
      ! print *,"W",Wm(:,i)
      call thermo_tv(Tam(i),v_YXW(1,i),Ym(:,i),fug)
      fug_w_a(i) = exp(fug(water_idx))
      call fugacity_water_in_hydrate_TVn(Tam(i),v_YXW(1,i),Ym(:,i),fug_h_a(i))
    enddo
    n_cross = 0
    do i=nwm+1,n_sat-1
      if ((fug_h_a(i)-fug_w_a(i))*(fug_h_a(i+1)-fug_w_a(i+1)) .LE. 0.0) then
        n_cross = n_cross + 1
        ! print *,"Crossing",Tam(i), pressure(Tam(i),v_YXW(1,i),Ym(:,i))
        ! print *,""
        ! print *,"i"
        ! print *,"T,P",Tam(i),Pam(i)
        ! print *,"beta",betam_YXW(:,i)
        ! print *,"v",v_YXW(:,i)
        ! print *,"Y",Ym(:,i)
        ! print *,"X",Xm(:,i)
        ! print *,"W",Wm(:,i)
        ! print *,""

        ! print *,""
        ! print *,"i+1"
        ! print *,"T,P",Tam(i+1),Pam(i+1)
        ! print *,"beta",betam_YXW(:,i+1)
        ! print *,"v",v_YXW(:,i+1)
        ! print *,"Y",Ym(:,i+1)
        ! print *,"X",Xm(:,i+1)
        ! print *,"W",Wm(:,i+1)

        if (betam_YXW(1,i) > 0.0 .and. betam_YXW(1,i) < 1.0) then
          ! Three phase solution
          call fill_Xvar_and_param_tv(X1_3ph,param,Z,Wm(:,i),Xm(:,i),Ym(:,i),&
               betam_YXW(1,i),Tam(i),Pam(i),v_YXW(3,i),v_YXW(2,i),&
               v_YXW(1,i),s=1,lns=0.0)
          !call print_Xvar_tv(X1_3ph,param,"X1_3ph")
          call fill_Xvar_and_param_tv(X2_3ph,param,Z,Wm(:,i+1),Xm(:,i+1),Ym(:,i+1),&
               betam_YXW(1,i+1),Tam(i+1),Pam(i+1),v_YXW(3,i+1),v_YXW(2,i+1),&
               v_YXW(1,i+1),s=1,lns=0.0)
          !call print_Xvar_tv(X2_3ph,param,"X2_3ph")
          call locate_hydrate_envelope_crossing_3ph(X1_3ph,X2_3ph,param,Xsol_3ph)
          !call print_Xvar_tv(Xsol_3ph,param,"Xsol_3ph")
          call copy_three_phase_crossing_to_two_phase_struct(Z,Xsol_3ph,Xsol,beta)

          call tppl%reset()
          call map_hydrate_curve_two_phase_fluid(Xsol,param,p_dew_start,p_max,T_min,tppl,beta)
          !call tppl%print()
          call tppl_comprised%add_tppl(tppl,inverted=(n_cross == 1))

          if (n_cross == 1) then
            call tppl%reset()
            call map_hydrate_curve_three_phase_fluid(Xsol_3ph,param,p_dew_start,p_max,T_min,tppl)
            call tppl_comprised%add_tppl(tppl,inverted=.false.)
            !call tppl%print()
          endif
        else
          call set_variables_tv(Tam(i),v_YXW(1,i),v_YXW(3,i),Ym(:,i),Wm(:,i),Z,1.0,X1,param)
          !call print_envelope_point_tv(X1,param)
          call set_variables_tv(Tam(i+1),v_YXW(1,i+1),v_YXW(3,i+1),Ym(:,i+1),Wm(:,i+1),Z,1.0,X2,param)
          !call print_envelope_point_tv(X2,param)

          call locate_hydrate_envelope_crossing_2ph(X1,X2,param,Xsol)
          !call print_envelope_point_tv(Xsol,param)
          call map_hydrate_curve_single_phase_fluid(Xsol,param,p_dew_start,p_max,T_min,nmxs,Ta,va,ns)
          if (n_cross == 1) &
               call tppl_comprised%add_single_phase_curve(Ta,va,ns,inverted=(n_cross == 1))
          if (n_cross == 1) then
            call tppl%reset()
            call map_hydrate_curve_two_phase_fluid(Xsol,param,p_dew_start,p_max,T_min,tppl)
            !call tppl%print()
            call tppl_comprised%add_tppl(tppl,inverted=.false.)
          else
            T = exp(Xsol(nc+1))
            v = exp(Xsol(nc+2))
            p = pressure(T,v,z)
            !print *,abs(T-tppl%list(tppl%n)%T)/T, abs(P-tppl%list(tppl%n)%P)/P
            if (abs(T-tppl%list(tppl%n)%T)/T > 1.0e-5 .or. abs(P-tppl%list(tppl%n)%P)/P  > 1.0e-5) then
              call tppl%reset()
              call map_hydrate_curve_two_phase_fluid(Xsol,param,p_dew_start,p_max,T_min,tppl)
              !call tppl%print()
              call tppl_comprised%add_tppl(tppl,inverted=.true.)
            endif
            if (n_cross > 1) &
               call tppl_comprised%add_single_phase_curve(Ta,va,ns,inverted=.false.)
          endif
        endif
      endif
    enddo
    if (print_to_file) call tppl_comprised%write_file("hydrate.dat")
    call tppl_comprised%write_arrays(nmax=n_max_hyd, n=n_hyd, T=T_hyd, P=P_hyd)
  end subroutine map_hydrate_appearance_curve
  !-----------------------------------------------------------------!

  !-----------------------------------------------------------------!
  subroutine copy_three_phase_crossing_to_two_phase_struct(Z,Xsol_3ph,Xsol,beta)
  !-----------------------------------------------------------------!
    implicit none
    real, intent(in)  :: z(nc)
    real, intent(in)  :: Xsol_3ph(2*nc+5)
    real, intent(out)  :: Xsol(nc+3)
    real, intent(out)  :: beta
    ! Internal
    real :: Xsol_3ph_ext(2*nc+7)
    real, dimension(nc) :: Ky, Kx, Y, X, W
    real :: vY, vX, vW, t, p, betaL, betaW
    integer :: i
    !real :: Xsol_ext(nc+4)
    !
    Xsol_3ph_ext(1:2*nc+5) = Xsol_3ph
    Xsol_3ph_ext(2*nc+6) = 0
    Xsol_3ph_ext(2*nc+7) = 1-Xsol_3ph_ext(2*nc+2)
    call get_three_phase_variables_tv(Xsol_3ph_ext,Z,T,P,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW)
    !
    do i=1,nc
      if (Z(i) > 0) then
        Xsol(i) = Y(i)/X(i)
      else
        Xsol(i) = 1
      endif
    enddo
    Xsol(1:nc) = log(Xsol(1:nc))
    Xsol(nc+1) = Xsol_3ph(2*nc+1) ! ln(T)
    beta = Xsol_3ph(2*nc+2)
    Xsol(nc+2) = Xsol_3ph(2*nc+3) ! ln(v1)
    Xsol(nc+3) = Xsol_3ph(2*nc+4) ! ln(v2)

    ! Debug
    ! Xsol_ext(1:nc+3) = Xsol
    ! Xsol_ext(nc+4) = beta
    ! call print_X_state_twoph(Xsol_ext,Z)
    ! print *,""
    ! call print_X_state_threeph(Xsol_3ph_ext,Z)
   end subroutine copy_three_phase_crossing_to_two_phase_struct

  !-----------------------------------------------------------------!
  subroutine locate_hydrate_envelope_crossing_2ph(X1,X2,param,Xsol)
  !-----------------------------------------------------------------!
    use saturation_tv, only: newton_tv_extrapolate, sat_newton_tv_x, &
         print_envelope_point_tv
    use nonlinear_solvers, only: NS_PEGASUS, bracketing_solver, &
         nonlinear_solver
    implicit none
    real, intent(in)  :: X1(nc+3), X2(nc+3)
    real, intent(inout) :: param(nc+3)
    real, intent(out)  :: Xsol(nc+3)
    ! Internal
    integer :: s, ierr, iter
    real :: Xmax, Xmin, X
    real :: dXdS(nc+3), dpds, param_locate(3*(nc+3)), p, ds
    type(nonlinear_solver) :: solver
    s = maxloc(abs(X1-X2),dim=1)
    param(nc+2) = real(s)
    if (X1(s) < X2(s)) then
      Xsol = X1
      Xmin = X1(s)
      Xmax = X2(s)
    else
      Xsol = X2
      Xmin = X2(s)
      Xmax = X1(s)
    endif
    call newton_tv_extrapolate(Xsol,param,dXdS,dpds)
    param_locate(1:nc+3) = param
    param_locate(nc+4:2*(nc+3)) = Xsol
    param_locate(2*(nc+3)+1:3*(nc+3)) = dXdS

    solver%abs_tol = 1.0e-8
    solver%max_it = 100
    solver%isolver = NS_PEGASUS
    call bracketing_solver(Xmin,Xmax,fun_hydrate_envelope_crossing,x,&
         solver,param_locate)
    if (solver%exitflag /= 0) then
      call stoperror("locate_hydrate_envelope_crossing: Not able to solve")
    else
      param = param_locate(1:nc+3)
      param(nc+3) = x
      ds = x - Xsol(s)
      Xsol = Xsol + dXdS*ds
      iter = sat_newton_tv_x(Xsol,param,p,ierr)
      !print *,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      !call print_envelope_point_tv(Xsol,param)
    endif
  end subroutine locate_hydrate_envelope_crossing_2ph
  !-----------------------------------------------------------------!

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on melting curve
  !>
  !> \author MH 2021
  !-----------------------------------------------------------------------------
  function fun_hydrate_envelope_crossing(x,param) result(fun)
    use eostv, only: thermo_tv
    use saturation_tv, only: sat_newton_tv_x, get_variables_tv
    implicit none
    real, intent(in) :: x
    real, dimension((nc+3)*3), intent(in) :: param
    real :: fun
    ! Locals:
    real, dimension(nc+3) :: param_tv
    real :: Xsol(nc+3), dXds(nc+3), ds, p, fug_w, fug_wh
    integer :: s, ierr, iter
    real :: T,v1,v2,X1(nc),X2(nc),K(nc), fug(nc)
    param_tv = param(1:nc+3)
    s = nint(param(nc+2))
    param_tv(nc+3) = x
    Xsol = param(nc+4:2*(nc+3))
    dXdS = param(2*(nc+3)+1:3*(nc+3))
    ds = x - Xsol(s)
    Xsol = Xsol + dXdS*ds
    iter = sat_newton_tv_x(Xsol,param_tv,p,ierr)

    call get_variables_tv(Xsol,param_tv,T,P,v1,v2,X1,X2,K)
    call thermo_tv(T,v1,X1,fug)
    fug_w = fug(water_idx)
    call fugacity_water_in_hydrate_TVn(T,v1,X1,fug_wh)
    fun = (fug_w - log(fug_wh))/max(abs(fug_w),1.0)
    !print *,"T,v1,X1",T,pressure(T,v1,X1),v1,X1
    !print *,"fun cross fug",fun, fug_w,log(fug_wh)
  end function fun_hydrate_envelope_crossing

  !-----------------------------------------------------------------!
  subroutine locate_hydrate_envelope_crossing_3ph(X1,X2,param,Xsol)
  !-----------------------------------------------------------------!
    use multi_phase_envelope_tv, only: three_ph_line_newton
    use nonlinear_solvers, only: NS_PEGASUS, bracketing_solver, &
         nonlinear_solver
    implicit none
    real, intent(in)  :: X1(2*nc+5), X2(2*nc+5)
    real, intent(inout) :: param(nc+3)
    real, intent(out)  :: Xsol(2*nc+5)
    ! Internal
    integer :: s, ierr, iter
    real :: Xmax, Xmin, X, error_on_exit
    real :: dXdS(2*nc+5), param_locate((nc+3)+2*(2*nc+5)), ds
    type(nonlinear_solver) :: solver
    s = maxloc(abs(X1-X2),dim=1)
    param(nc+2) = real(s)
    param(nc+3) = 0.0 ! Partial off
    if (X1(s) < X2(s)) then
      Xsol = X1
      Xmin = X1(s)
      Xmax = X2(s)
    else
      Xsol = X2
      Xmin = X2(s)
      Xmax = X1(s)
    endif
    dXdS = (X1-X2)/(X1(s)-X2(s))
    param_locate(1:nc+3) = param
    param_locate(nc+4:(nc+3)+(2*nc+5)) = Xsol
    param_locate((nc+3)+(2*nc+5)+1:(nc+3)+2*(2*nc+5)) = dXdS

    solver%abs_tol = 1.0e-8
    solver%max_it = 100
    solver%isolver = NS_PEGASUS
    call bracketing_solver(Xmin,Xmax,fun_hydrate_envelope_crossing_3ph,x,&
         solver,param_locate)
    if (solver%exitflag /= 0) then
      call stoperror("locate_hydrate_envelope_crossing_3ph: Not able to solve")
    else
      param = param_locate(1:nc+3)
      param(nc+1) = x
      ds = x - Xsol(s)
      Xsol = Xsol + dXdS*ds
      call three_ph_line_newton(param,Xsol,s,x,partial=.false.,niter=100,ierr=ierr,&
         nlines=3,error_on_exit=error_on_exit,iter=iter)
    endif
  end subroutine locate_hydrate_envelope_crossing_3ph
  !-----------------------------------------------------------------!

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on hydrate curve
  !>
  !> \author MH 2022
  !-----------------------------------------------------------------------------
  function fun_hydrate_envelope_crossing_3ph(xx,param) result(fun)
    use eostv, only: thermo_tv
    use multi_phase_envelope_tv, only: three_ph_line_newton, read_Xvar_and_param_tv
    implicit none
    real, intent(in) :: xx
    real, dimension(nc+3+2*(2*nc+5)), intent(in) :: param
    real :: fun
    ! Locals:
    real, dimension(nc+3) :: param_tv
    real :: Xsol(2*nc+5), dXds(2*nc+5), ds, fug_w, fug_wh
    integer :: s, ierr, iter
    real :: fug(nc), error_on_exit
    real :: W(nc),X(nc),Y(nc),beta,T,P,vW,vX,vY,Ky(nc),Kx(nc)
    param_tv = param(1:nc+3)
    s = nint(param(nc+2))
    param_tv(nc+1) = xx
    Xsol = param(nc+4:(nc+3)+(2*nc+5))
    dXdS = param((nc+3)+(2*nc+5)+1:(nc+3)+2*(2*nc+5))
    ds = xx - Xsol(s)
    Xsol = Xsol + dXdS*ds
    call three_ph_line_newton(param_tv,Xsol,s,xx,partial=.false.,niter=100,ierr=ierr,&
         nlines=3,error_on_exit=error_on_exit,iter=iter)
    call read_Xvar_and_param_tv(Xsol,param_tv,&
         W,X,Y,beta,t,p,vW,vX,vY,Kx,Ky,no_press_calc=.true.)
    call thermo_tv(T,vY,Y,fug)
    fug_w = exp(fug(water_idx))
    call fugacity_water_in_hydrate_TVn(T,vY,Y,fug_wh)
    fun = (fug_w - fug_wh)/max(abs(fug_w),1.0)
  end function fun_hydrate_envelope_crossing_3ph

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Methods for single phase curves
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------!
  subroutine map_hydrate_curve_single_phase_fluid(Xsol_tv_sat,param_tv,p_min,p_max,T_min,nmx,Ta,va,n)
  !-----------------------------------------------------------------!
    use saturation_tv, only: get_variables_tv
    use eostv, only: pressure
    implicit none
    real, intent(in) :: Xsol_tv_sat(nc+3)
    real, intent(in) :: param_tv(nc+3)
    real, intent(in) :: p_min,p_max,T_min
    integer, intent(in) :: nmx
    real, intent(out) :: Ta(nmx), va(nmx)
    integer, intent(out) :: n
    ! Internal
    integer :: s,ierr,iter
    real :: T,P,v,v2,X1(nc),X2(nc),K(nc),Pold,dpdv
    real :: X(2), Xold(2), dXds(2), dXdsOld(2)
    real :: Z(nc), param(nc+2), sgn, Told
    real :: dS, tuning, dP, dPds, dS_max, dS_min, ln_spec
    integer :: smax
    logical :: excessive_T_jump
    logical :: recalculate
    logical :: exit_after_saving
    real, parameter :: maxdT = 25.0, maxdP = 15.0
    !
    exit_after_saving = .false.
    recalculate = .false.
    Z = param_tv(1:nc)
    param(1:nc) = Z
    s = 2 ! Start by fixating volume
    param(nc+1) = real(s)
    !
    call get_variables_tv(Xsol_tv_sat,param_tv,T,P,v,v2,X1,X2,K)
    X(1) = log(T)
    X(2) = log(v)
    param(nc+2) = X(2)
    iter = newton_single_phase_fluid_hydrate_curve(X,param,ierr)
    Xold = X
    ! Determine step direction
    call hydrate_single_initial_step(X,param,sgn)
    !
    X = Xold
    n = 0
    call set_hydrate_solution_single(X,Z,nmx,Ta,va,n,p)
    dXdS = 0
    !
    ! Adapt initial step to pressure sensitivity
    p=pressure(Ta(1),va(1),z,dpdv=dpdv)
    !
    dS_max = 0.25
    dS_min = 0.01/min(max(abs(dpdv*1e-9), 1.0), 1e5)
    dS = dS_min
    tuning = 1.2
    do while (n < nmx)
      dXdSold = dXdS
      call hyd_newton_tv_extrapolate(X,param,dXdS,dPds)
      smax = maxloc(abs(dXdS),dim=1)
      if ((.not. smax == s) .and. n > 1) then
        s = smax
        ! Rescaling the sensitivities
        sgn = sign(1.0,X(s) - Xold(s))
        dPds = dPds / dXdS(s)
        dXdS = dXdS / dXdS(s)
        dXdSold = dXdSold / dXdSold(s)
        param(nc+1) = real(s)
      endif

      dP = abs(dPds*ds)/1e5
      if (dP > maxdP) then
        ! Limit step in pressure
        dS = max(min(dS*maxdP/dP,dS_max),dS_min*0.05)
      endif
      Pold = P
      Xold = X
      Told = exp(Xold(1))
      X = Xold + dXdS*dS*sgn
      if (s == 3) then
        param(nc+2) = log(p) + dS*sgn
      else
        param(nc+2) = X(s)
      endif
      iter = newton_single_phase_fluid_hydrate_curve(X,param,ierr)
      T = exp(X(1))
      excessive_T_jump = (abs(Told - T) > maxdT)
      if (ierr /= 0 .OR. excessive_T_jump) then
        ! Something went wrong.
        ! Attempt to make the step shorter.
        X = Xold + dXdS*dS*sgn*0.5
        if (s == 3) then
          param(nc+2) = log(Pold) + dS*sgn*0.5
        else
          param(nc+2) = X(s)
        endif
        iter = newton_single_phase_fluid_hydrate_curve(X,param,ierr)
        T = exp(X(1))
        excessive_T_jump = (abs(Told - T) > maxdT)
      endif
      if (ierr /= 0 .OR. excessive_T_jump) then
        ! Something went wrong.
        ! Attempt to make the step longer.
        X = Xold + dXdS*dS*sgn*2.0
        if (s == 3) then
          param(nc+2) = log(Pold) + dS*sgn*2.0
        else
          param(nc+2) = X(s)
        endif
        iter = newton_single_phase_fluid_hydrate_curve(X,param,ierr)
        T = exp(X(1))
        excessive_T_jump = (abs(Told - T) > maxdT)
      endif

      v = exp(X(2))
      p = pressure(T,v,Z)
      !Exit at thermo limit or defined pressure
      if (p < p_min) then
        s = 3
        recalculate = .true.
        ln_spec = log(p_min)
      else if (p >= p_max) then
        s = 3
        recalculate = .true.
        ln_spec = log(p_max)
      endif

      ! Is temperature decreasing? - And below Tmin + safety limit?
      if (X(1) - Xold(1) < 0.0 .and. T < T_min + 0.01) then
        ! Exit at temperature
        s = 1
        ln_spec = log(T_min)
        recalculate = .true.
      endif

      if (recalculate) then
        ! Extrapolate from previous point
        param(nc+1) = real(s)
        param(nc+2) = ln_spec
        if (s <= 2) then
          dS = ln_spec - Xold(s) ! Sign included
        else if (s == 3) then
          if (abs(dpds) > 10.0) ds = (exp(ln_spec) - Pold)/dpds
        endif
        X = Xold + dXdS*dS
        iter = newton_single_phase_fluid_hydrate_curve(X,param,ierr)
        exit_after_saving = .true.
      endif

      v = exp(X(1))
      call set_hydrate_solution_single(X,Z,nmx,Ta,va,n,P)

      if (exit_after_saving) then
        exit
      endif

      ! Tune dS up or down based on how fast newton_single_phase_fluid_hydrate_curve converged
      if (iter < 3) then
        dS = dS * tuning
      else if (iter > 5) then
        dS = dS/tuning
      endif
      dS = max(min(dS,dS_max),dS_min)

    enddo
  contains
    subroutine set_hydrate_solution_single(X,Z,nmx,Ta,va,n,p)
      real, intent(in) :: X(2)
      real, intent(in) :: Z(nc)
      integer, intent(in) :: nmx
      real, intent(inout) :: Ta(nmx), va(nmx)
      integer, intent(inout) :: n
      real, intent(out) :: P
      ! Locals
      real :: T, v
      T = exp(X(1))
      v = exp(X(2))
      n = n + 1
      Ta(n) = T
      va(n) = v
      P = pressure(T,v,z)
      !print *,"T,v,P",T,v,P
    end subroutine set_hydrate_solution_single
  end subroutine map_hydrate_curve_single_phase_fluid

  !-----------------------------------------------------------------!
  !> Single phase hydrate curve initial step
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hydrate_single_initial_step(X,param,sgn)
    use stability, only: checkVLEstability
    implicit none
    real, dimension(nc+2), intent(inout) :: param
    real, dimension(2), intent(in) :: X !< Variable vector
    real, intent(out) :: sgn !< direction
    ! Locals
    real :: Xl(2) !< Local variable vector
    real :: Z(nc), Wsol(nc), P, T, v
    logical :: isStable
    integer :: new_phase, ierr, iter, s
    Xl = X
    T = exp(Xl(1))
    v = exp(Xl(2))
    Z = param(1:nc)

    ! Make sure we increase when already at low pressure
    p=pressure(T,v,z)
    if (p < 2.0e5) then
      sgn = -1 ! Increase pressure
    else
      sgn = 1
    endif

    ! Perturbation in volume
    s = 2
    param(nc+1) = real(s)
    param(nc+2) = log(v*(1.0+sgn*1.0e-4))
    iter = newton_single_phase_fluid_hydrate_curve(Xl,param,ierr)
    if (ierr /= 0) call stoperror("Hydrate curve: Not able to determine initial step direction")

    ! Test stabillity
    T = exp(Xl(1))
    v = exp(Xl(2))
    P = pressure(T,v,Z)
    call checkVLEstability(t,p,Z,isStable,Wsol,new_phase)
    if (.not. isStable) then
      sgn = -sgn
    endif
  end subroutine hydrate_single_initial_step

  !-----------------------------------------------------------------!
  !> Hydrate curve mapping limits
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hydrate_var_tv_limits(Z,Xsol,Xmin,Xmax)
    use thermopack_var, only: thermo_model, get_active_thermo_model
    use numconstants, only: expMax, expMin, Small
    use eosdata, only: eosCPA
    use cubic_eos, only: get_b_linear_mix
    use volume_shift, only: get_c_mix
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(2), intent(in) :: Xsol !< Variable vector
    real, dimension(2), intent(out) :: Xmin, Xmax !< Variable vector
    ! Locals
    real :: b, T
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    Xmin = expMin
    Xmax = expMax
    Xmin(1) = log(tpTmin) !Tmin
    Xmax(1) = log(tpTmax) !Tmax
    T = exp(Xsol(1))
    !
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      b = 1.0e-7
    else
      b = get_b_linear_mix(Z) + get_c_mix(T,Z) + Small ! m3/mol
    endif
    Xmin(2) = log(b) !v min
    Xmax(2) = log(100.0) !v max

  end subroutine hydrate_var_tv_limits

  !-----------------------------------------------------------------!
  function newton_single_phase_fluid_hydrate_curve(Xsol,param,ierr) result(iter)
  !-----------------------------------------------------------------!
    use nonlinear_solvers, only: nonlinear_solver,limit_dx,premReturn,setXv,&
         nonlinear_solve, test_differentials
    use utilities, only: isXwithinBounds
    implicit none
    real, intent(inout) :: Xsol(2)
    real, intent(inout) :: param(nc+2)
    integer, intent(out) :: ierr
    integer :: iter
    ! Internal
    real :: Z(nc)
    real :: Xmax(2), Xmin(2)
    type(nonlinear_solver) :: solver
    ! Testing
    !call test_differentials(Xsol,param,hyd_single_fun_newton_tv,&
    !     hyd_single_diff_newton_tv)

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-9
    solver%max_it = 50
    solver%ls_max_it = 3

    Z = param(1:nc)
    call hydrate_var_tv_limits(Z,Xsol,Xmin,Xmax)
    call isXwithinBounds(2,Xsol,Xmin,Xmax,"",&
         "newton_single_phase_fluid_hydrate_curve: Initial values not within bounds!!")
    call nonlinear_solve(solver,hyd_single_fun_newton_tv,hyd_single_diff_newton_tv,&
         hyd_single_diff_newton_tv,limit_dx,premReturn,setXv,Xsol,Xmin,Xmax,param)
    iter = solver%iter
    ierr = solver%exitflag

  end function newton_single_phase_fluid_hydrate_curve
  !-----------------------------------------------------------------!

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-12
  !-----------------------------------------------------------------------------
  subroutine hyd_single_fun_newton_tv(G,Xvar,param)
    use eos, only: entropy, enthalpy
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(2), intent(out) :: G !< Function values
    real, dimension(2), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2), intent(inout) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: lnfug
    integer :: s
    real :: v, t, ln_s, p, ps, fug_wh

    Z = param(1:nc)
    s = nint(param(nc+1))
    ln_s = param(nc+2)

    t = exp(Xvar(1))
    v = exp(Xvar(2))

    call thermo_tv(t,v,Z,lnfug)
    call fugacity_water_in_hydrate_TVn(T,v,z,fug_wh)
    ! Function value
    G(1) = lnfug(water_idx) - log(fug_wh)
    if (s <= 2) then
      G(2) = Xvar(s) - ln_s
    else if (s == 3) then
      ! Pressure
      p = pressure(t,v,Z)
      ps = exp(ln_s)
      G(2) = (p-ps)*v/(Rgas*T)
    endif

    !print *,"G",G
  end subroutine hyd_single_fun_newton_tv

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hyd_single_diff_newton_tv(J,Xvar,param)
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(2), intent(in) :: Xvar !< Variable vector
    real, dimension(2,2), intent(out) :: J !< Function differentials
    real, dimension(nc+2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: lnfug, lnfug_T, lnfug_V
    real :: p, dpdv, dpdt
    real :: fug_wh, fug_wh_T, fug_wh_v, lnfug_wh_T, lnfug_wh_v
    integer :: s
    real :: v, t, fac, ln_s, ps

    Z = param(1:nc)
    s = nint(param(nc+1))
    ln_s = param(nc+2)

    t = exp(Xvar(1))
    v = exp(Xvar(2))

    call thermo_tv(t,v,z,lnfug,lnphit=lnfug_T,lnphiv=lnfug_V)
    call fugacity_water_in_hydrate_TVn(T,v,z,fug_wh,fug_wh_T,fug_wh_v)
    lnfug_wh_T = fug_wh_T/fug_wh
    lnfug_wh_v = fug_wh_v/fug_wh

    ! Temperature differential
    J(1,1) = t*(lnfug_T(water_idx) - lnfug_wh_T)

    ! Volume differential
    J(1,2) = v*(lnfug_V(water_idx) - lnfug_wh_V)

    ! Specification row
    if (s <= 2) then
      J(2,:) = 0.0
      J(2,s) = 1.0
    else if (s == 3) then
      ! Pressure
      ps = exp(ln_s)
      p = pressure(t,v,z,dpdv=dpdv,dpdt=dpdt)
      fac = v/(Rgas*T)
      J(2,1) = T*dpdT*fac
      if (debug_hyd) J(2,1) = J(2,1) - (p-ps)*fac
      J(2,2) = v*dpdv*fac
      if (debug_hyd) J(2,2) = J(2,2) - (p-ps)*fac
    endif

  end subroutine hyd_single_diff_newton_tv

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along the hydrate line
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hyd_newton_tv_extrapolate(Xvar,param,dXdS,dpds)
    implicit none
    real, dimension(2), intent(in) :: Xvar
    real, dimension(nc+2), intent(inout) :: param
    real, dimension(2), intent(out) :: dXdS
    real, intent(out) :: dPdS
    ! Locals
    real :: p, T, v, Z(nc)
    real :: dpdv, dpdt
    integer :: s
    real, dimension(2,2) :: Jac
    integer, dimension(2) :: INDX
    integer :: INFO

    call hyd_single_diff_newton_tv(Jac,Xvar,param)
    t = exp(Xvar(1))
    v = exp(Xvar(2))
    Z = param(1:nc)
    p = pressure(T,v,Z,dpdv=dpdv,dpdt=dpdt)
    s = nint(param(nc+1))
    dXdS = 0
    if (s <= 2) then
      dXdS(2) = 1
    else
      ! Specified pressure
      ! dlnP
      dXdS(2) = p*v/(Rgas*T)
    endif

    ! Solve equation system
    call DGESV( 2, 1, Jac, 2, INDX, dXdS, 2, INFO )

    dpds = v*dpdv*dxds(2) + T*dpdt*dxds(1)

  end subroutine hyd_newton_tv_extrapolate

  !-----------------------------------------------------------------------------
  !> Test variable sensitivities along the hydrate curve
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hyd_newton_tv_extrapolate_test()
    implicit none
    ! Locals
    real, dimension(2) :: Xvar1, Xvar2, Xvar0
    real, dimension(nc+2) :: param
    real, dimension(2) :: dXdS
    real :: T, ln_s, dpds, p1, p2, ds, p
    integer :: s, ierr, iter
    real :: Z(nc)
    real :: v

    !call init_cubic("CO2,H2O","SRK")
    z = (/1-100.0e-6,100.0e-6/)
    param(1:nc) = Z
    T = 263.94514142386441
    v = 6.1210979165482525E-004
    Xvar0 = (/log(T), log(v)/)
    s = 1
    param(nc+1) = real(s)
    ln_s = Xvar0(1)
    param(nc+2) = ln_s
    iter = newton_single_phase_fluid_hydrate_curve(Xvar0,param,ierr)
    T = exp(Xvar0(1))
    v = exp(Xvar0(2))
    P = pressure(T,v,z)
    ds = 1.0e-4

    ! Fixate temperature and volume
    do s=1,3
      param(nc+1) = real(s)
      call hyd_newton_tv_extrapolate(Xvar0,param,dXdS,dpds)
      if (s <= 2) then
        param(nc+2) = Xvar0(s) - ds
      else
        param(nc+2) = log(p) - ds
      endif
      Xvar1 = Xvar0
      iter = newton_single_phase_fluid_hydrate_curve(Xvar1,param,ierr)
      T = exp(Xvar1(1))
      v = exp(Xvar1(2))
      P1 = pressure(T,v,z)
      if (s <= 2) then
        param(nc+2) = Xvar0(s) + ds
      else
        param(nc+2) = log(p) + ds
      endif
      Xvar2 = Xvar0
      iter = newton_single_phase_fluid_hydrate_curve(Xvar2,param,ierr)
      T = exp(Xvar2(1))
      v = exp(Xvar2(2))
      P2 = pressure(T,v,z)
      if (s == 1) print *,"Temperature:"
      if (s == 2) print *,"Volume:"
      if (s == 3) print *,"Pressure:"
      print *,(Xvar2-Xvar1)/(2*ds)
      print *,dXdS
      print *,"dpds:"
      print *,dpds,(p2-p1)/(2*ds)
    enddo
  end subroutine hyd_newton_tv_extrapolate_test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Methods for two phase curves
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------!
  subroutine map_hydrate_curve_two_phase_fluid(Xsol_tv_sat,param_tv,p_min,p_max,T_min,tppl,beta)
  !-----------------------------------------------------------------!
    implicit none
    real, intent(in) :: Xsol_tv_sat(nc+3)
    real, intent(in) :: param_tv(nc+3)
    real, intent(in) :: p_min,p_max,T_min
    type(multi_phase_point_list), intent(inout) :: tppl
    real, optional, intent(in) :: beta
    ! Internal
    integer :: s,ierr,iter
    real :: T,P,Pold
    real :: X(nc+4), Xold(nc+4), dXds(nc+4), dXdsOld(nc+4)
    real :: Z(nc), param(nc+2), sgn, Told, beta_lin, dbeta
    real :: dS, tuning, dP, dPds, dS_max, dS_min, ln_spec, dT
    real :: X1(nc), X2(nc), K(nc), T_test, P_test, v3, X3(nc)
    real :: X_3ph(2*nc+7)
    integer :: smax
    logical :: recalculate
    logical :: exit_after_saving
    logical :: set_beta_lin
    real, parameter :: maxdT = 10.0, maxdP = 15.0, maxdbeta = 0.05
    real, parameter :: excessive_dT = 25.0
    real, parameter :: beta_limit = 0.025
    real :: v1,v2,K2(nc),bG,bL,bW
    !
    exit_after_saving = .false.
    recalculate = .false.
    set_beta_lin = .false.
    Z = param_tv(1:nc)
    param(1:nc) = Z
    X(1:nc+3) = Xsol_tv_sat
    !
    if (present(beta)) then
      X(nc+4) = beta
    else
      X(nc+4) = 1.0
    endif
    s = nc+4 ! Start by fixating beta
    sgn = -1 ! Start by decreaseing beta
    !
    param(nc+2) = X(s)
    param(nc+1) = real(s)
    iter = newton_two_phase_fluid_hydrate_curve(X,param,T,P,ierr)
    Xold = X
    !
    X = Xold
    call tppl%add_point_twoph(X)
    P = tppl%list(tppl%n)%P
    dXdS = 0
    !
    if (present(beta)) then
      s = nc+5 ! Specify pressure
      sgn = -1 ! Try decreaseing pressure
      param(nc+1) = real(s)
      call hyd_twoph_newton_tv_extrapolate(X,param,dXdS,dPds)
      X = Xold + dXdS*0.01*sgn
      param(nc+2) = log(p) + 0.01*sgn
      iter = newton_two_phase_fluid_hydrate_curve(X,param,T_test,P_test,ierr)
      ! Stabillity?
      K = exp(X(1:nc))
      X2 = Z/(1-X(nc+4)+X(nc+4)*K)
      X1 = K*X2
      if (.not. isStable2ph(t_test,p_test,X(nc+4),X1,exp(X(nc+2)),X2,exp(X(nc+3)),X3,v3)) then
        sgn = 1 ! Go oposite direction
      endif
      X = Xold ! Reset X
    endif
    !
    dS_max = 0.25
    dS_min = 0.01
    dS = dS_min
    tuning = 1.2
    do while (tppl%n < size(tppl%list))
      dXdSold = dXdS
      call hyd_twoph_newton_tv_extrapolate(X,param,dXdS,dPds)
      smax = maxloc(abs(dXdS),dim=1)
      s = nint(param(nc+1))
      if ((.not. smax == s) .and. tppl%n > 1) then
        !if (s == nc+5) then ! Pressure specification
          s = smax
          sgn = sign(1.0,X(s) - Xold(s))
        !else
        !  sgn = sign(1.0,dXdS(smax)*dXdS(s))*sgn
        !  s = smax
        !endif
        dPds = dPds / dXdS(s)
        dXdS = dXdS / dXdS(s)
        dXdSold = dXdSold / dXdSold(s)
        param(nc+1) = real(s)
      endif

      dT = abs(exp(X(nc+1) + dXdS(nc+1)*dS*sgn) - exp(X(nc+1)))
      if (dT > maxdT) then
        ! Limit step in temperature
        dS = max(min(maxdT/dT,dS_max),dS_min)
      endif
      dP = abs(dPds*ds)/1e5
      if (dP > maxdP) then
        ! Limit step in pressure
        dS = max(min(dS*maxdP/dP,dS_max),dS_min)
      endif
      ! Limit step in beta
      if (abs(dXdS(nc+4)*dS) > maxdbeta) then
        dS = max(min(maxdbeta/abs(dXdS(nc+4)),dS_max),dS_min)
      endif
      beta_lin = X(nc+4) + dXdS(nc+4)*dS*sgn
      if ((dXdS(nc+4)*sgn < -1.0e-5 .and. beta_lin < beta_limit) .or. &
           (dXdS(nc+4)*sgn > 1.0e-5 .and. beta_lin > 1-beta_limit)) then
        s = nc+4
        param(nc+1) = real(s)
      endif
      if (beta_lin <= 0.0) then
        set_beta_lin = .true.
        beta_lin =  0
        dS = X(nc+4)/abs(dXdS(nc+4))
        s = nc+4
        param(nc+1) = real(s)
      else if (beta_lin >= 1.0) then
        set_beta_lin = .true.
        beta_lin = 1
        dbeta = abs(1 - X(nc+4))
        dS = dbeta/abs(dXdS(nc+4))
        s = nc+4
        param(nc+1) = real(s)
      endif

      Pold = P
      Xold = X
      Told = T
      X = Xold + dXdS*dS*sgn
      ! Make sure beta is between zero and one
      X(nc+4) = min(max(X(nc+4),real(0)),real(1))
      if (s == nc+5) then
        param(nc+2) = log(p) + dS*sgn
      else if (s == nc+4 .and. set_beta_lin) then
        param(nc+2) = beta_lin
      else
        param(nc+2) = X(s)
      endif

      call wrap_newton_two_phase_fluid_hydrate_curve(param,&
           Pold,dpds,Xold,X,dXds,sgn,s,ds,T,P,ierr,iter)

      ! Stabillity test
      K = exp(X(1:nc))
      X2 = Z/(1-X(nc+4)+X(nc+4)*K)
      X1 = K*X2
      if (.not. isStable2ph(T,P,X(nc+4),X1,exp(X(nc+2)),X2,exp(X(nc+3)),X3,v3)) then
        ! Locate endpoint
        call set_three_phase_variables_tv(Z,T,exp(X(nc+2)),exp(X(nc+3)),v3,X1,X2,X3,X(nc+4),1-X(nc+4),0.0,X_3ph)
        param(nc+1) = 2*nc+6
        param(nc+2) = 0.0
        iter = newton_three_phase_fluid_hydrate_curve(X_3ph,param,T,P,ierr)
        if (ierr == 0) then
          call get_three_phase_variables_tv(X_3ph,Z,T,P,v1,v2,v3,X1,X2,X3,K,K2,bG,bL,bW)
          X(1:nc) = log(X1/X2)
          X(nc+1) = log(T)
          X(nc+2) = log(v1)
          X(nc+3) = log(v2)
          X(nc+4) = bG
        endif
        exit_after_saving = .true.
      endif

      !Exit at thermo limit or defined pressure
      if (p < p_min) then
        s = nc+5
        recalculate = .true.
        ln_spec = log(p_min)
      else if (p >= p_max) then
        s = nc+5
        recalculate = .true.
        ln_spec = log(p_max)
      endif

      ! Is temperature decreasing? - And below Tmin + safety limit?
      if (X(nc+1) - Xold(nc+1) < 0.0 .and. T < T_min + 0.01) then
        ! Exit at temperature
        s = nc+1
        ln_spec = log(T_min)
        recalculate = .true.
      endif

      ! Is beta zero/one
      if (X(nc+4) <= 0.0 .or. X(nc+4) >= 1.0) then
        ! Exit
        exit_after_saving = .true.
      endif

      if (recalculate) then
        ! Extrapolate from previous point
        param(nc+1) = real(s)
        param(nc+2) = ln_spec
        if (s <= nc+4) then
          dS = ln_spec - Xold(s) ! Sign included
        else if (s == nc+5) then
          if (abs(dpds) > 10.0) ds = (exp(ln_spec) - Pold)/dpds
        endif
        X = Xold + dXdS*dS
        iter = newton_two_phase_fluid_hydrate_curve(X,param,T,P,ierr)
        exit_after_saving = .true.
      endif

      ! Add point to list
      call tppl%add_point_twoph(X)

      if (exit_after_saving) then
        exit
      endif

      ! Tune dS up or down based on how fast newton_single_phase_fluid_hydrate_curve converged
      if (iter < 3) then
        dS = dS * tuning
      else if (iter > 5) then
        dS = dS/tuning
      endif
      dS = max(min(dS,dS_max),dS_min)

    enddo
  end subroutine map_hydrate_curve_two_phase_fluid

  !-----------------------------------------------------------------------------
  !> Test if new phase should be introduced to two-phase mixture
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  function isStable2ph(t,p,beta1,X1,v1,X2,v2,X3,v3) result(isS)
    use eostv, only: thermo_tvp
    use eos, only: specificvolume
    use thermopack_constants, only: LIQPH, VAPPH, WATER, NONWATER
    use stability, only: stabcalcW, stabilityLimit
    use thermo_utils, only: waterComponentFraction, wilsonK
    implicit none
    real, intent(in) :: t,p,beta1
    real, dimension(nc), intent(in) :: X1,X2
    real, intent(in) :: v1, v2
    real, dimension(nc), intent(out) :: X3
    real, intent(out) :: v3
    logical :: isS
    ! Locals
    integer :: nd, X3phase
    real :: Z(nc), XX(2,nc),lnFug1(nc),lnFug3(nc),tpd
    real, dimension(nc) :: Kw,dKwdp,dKwdt
    real, parameter :: stab_lim_test = stabilityLimit*1.0e5
    !
    call thermo_tvp(T,v1,X1,lnfug1)
    !
    nd = 2
    XX(1,:) = X1
    XX(2,:) = X2
    Z = beta1*X1 + (1-beta1)*X2

    ! Test water-like liquid phase
    call wilsonK(t,p,Kw,dKwdp,dKwdt,WATER)
    X3 = Z/Kw
    X3phase = LIQPH
    tpd = stabcalcW(nd,1,t,p,XX,X3,X3phase,lnFug1,lnFug3,preTermLim=-1000.0)
    isS = (tpd > stab_lim_test)
    if (.not. isS) then
      call specificvolume(t,p,X3,X3phase,v3)
      return
    endif

    ! Test vapour
    call wilsonK(t,p,Kw,dKwdp,dKwdt,NONWATER)
    X3 = Kw*Z
    X3phase = VAPPH
    tpd = stabcalcW(nd,1,t,p,XX,X3,X3phase,lnFug1,lnFug3,preTermLim=-1000.0)
    isS = (tpd > stab_lim_test)
    if (.not. isS) then
      call specificvolume(t,p,X3,X3phase,v3)
      return
    endif

    ! Test liquid phase
    X3 = Z/Kw
    X3phase = LIQPH
    tpd = stabcalcW(nd,1,t,p,XX,X3,X3phase,lnFug1,lnFug3,preTermLim=-1000.0)
    isS = (tpd > stab_lim_test)
    if (.not. isS) then
      call specificvolume(t,p,X3,X3phase,v3)
    endif
  end function isStable2ph

  !-----------------------------------------------------------------------------
  !> Solve for hydrate appearance point in equilibrium with two fluid phases
  !>
  !> \author Morten Hammer, 2022-01
  !-----------------------------------------------------------------------------
  subroutine wrap_newton_two_phase_fluid_hydrate_curve(param,&
       Pold,dpds,XXold,XX,dXds,sgn,s,ds,T,P,ierr,iter)
    use nonlinear_solvers, only: test_differentials
    implicit none
    real, intent(in) :: Pold, dpds
    real, dimension(nc+4), intent(in) :: XXold, dXds
    real, dimension(nc+4), intent(inout) :: XX
    real, dimension(nc+2), intent(inout) :: param
    real, intent(in)     :: sgn
    integer, intent(in)  :: s
    real, intent(inout)  :: ds
    integer, intent(out) :: ierr
    integer, intent(out) :: iter
    real, intent(out)    :: T,P
    ! Locals
    integer :: i
    real :: halff, ds_in, Told
    real, parameter :: excessive_dT = 25.0
    logical :: excessive_T_jump
    Told = exp(XXold(nc+1))
    ds_in = ds
    halff = 1.0
    do i=1,3 ! Try 3 times before trying to double
      iter = newton_two_phase_fluid_hydrate_curve(XX,param,T,P,ierr)
      excessive_T_jump = (abs(Told - T) > excessive_dT)
      if (ierr == 0 .and. .not. excessive_T_jump) then
        ds = halff*ds
        exit
      endif
      halff = 0.5**i
      call update_step(halff)
    enddo
    if (ierr /= 0 .or. excessive_T_jump) then ! Try doubling initial step
      call update_step(2.0)
      iter = newton_two_phase_fluid_hydrate_curve(XX,param,T,P,ierr)
      excessive_T_jump = (abs(Told - T) > excessive_dT)
      if (ierr == 0) then
        ds = ds*2.0
      endif
    endif
    do i=1,2 ! Try 2 more times before giving up
      iter = newton_two_phase_fluid_hydrate_curve(XX,param,T,P,ierr)
      excessive_T_jump = (abs(Told - T) > excessive_dT)
      if (ierr == 0 .and. .not. excessive_T_jump) then
        ds = halff*ds
        exit
      endif
      halff = 0.5*halff
      call update_step(halff)
    enddo
    if (ierr /= 0 .or. excessive_T_jump) then ! Giving up
      halff = 2.0*halff
      call update_step(halff)
      print *,"XX",XX
      print *,"param",param
      call print_X_state_twoph(XX,param(1:nc))
      call test_differentials(XX,param,hyd_twoph_fun_newton_tv,&
           hyd_twoph_diff_newton_tv)
      call stoperror("wrap_newton_two_phase_fluid_hydrate_curve: Giving up")
    endif
  contains
    subroutine update_step(fac)
      real, intent(in) :: fac
      XX = XXold + dXds*sgn*ds*fac
      if (s == nc+5) then
        P = Pold + dpds*sgn*ds*fac
        param(nc+2) = log(P)
      else
        param(nc+2) = XX(s)
      endif
    end subroutine update_step
  end subroutine wrap_newton_two_phase_fluid_hydrate_curve

  !-----------------------------------------------------------------!
  function newton_two_phase_fluid_hydrate_curve(Xsol,param,T,P,ierr) result(iter)
  !-----------------------------------------------------------------!
    use nonlinear_solvers, only: nonlinear_solver,premReturn,setXv,&
         nonlinear_solve, test_differentials
    use utilities, only: isXwithinBounds
    implicit none
    real, intent(inout) :: Xsol(nc+4)
    real, intent(inout) :: param(nc+2)
    real, intent(out) :: T, P
    integer, intent(out) :: ierr
    integer :: iter
    ! Internal
    real :: Z(nc), b(nc)
    real :: Xmax(nc+4), Xmin(nc+4)
    type(nonlinear_solver) :: solver
    real :: v1,v2,X1(nc),X2(nc),K(nc),beta
    real :: param_ext(2*nc+2)
    ! Testing
    !call test_differentials(Xsol,param,hyd_twoph_fun_newton_tv,&
    !     hyd_twoph_diff_newton_tv)

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-9
    solver%max_it = 50
    solver%ls_max_it = 3

    Z = param(1:nc)
    call hydrate_var_twoph_tv_limits(Xsol,Xmin,Xmax,b)
    call isXwithinBounds(nc+4,Xsol,Xmin,Xmax,"",&
         "newton_two_phase_fluid_hydrate_curve: Initial values not within bounds!!")
    param_ext(1:nc+2) = param
    param_ext(nc+3:2*nc+2) = b
    call nonlinear_solve(solver,hyd_twoph_fun_newton_tv,hyd_twoph_diff_newton_tv,&
         hyd_twoph_diff_newton_tv,limit_twoph_dx_line,premReturn,&
         setXv,Xsol,Xmin,Xmax,param_ext)
    iter = solver%iter
    ierr = solver%exitflag

    if (ierr == 0) then
      call get_two_phase_variables_tv(Xsol,Z,T,P,v1,v2,X1,X2,K,beta)
    endif
  end function newton_two_phase_fluid_hydrate_curve

  !-----------------------------------------------------------------------------
  !> Limit change in x, dx, to keep x between extreme values:
  !> xmin <= x <= xmax
  !>
  !> \author MH, Dec 2021
  !-----------------------------------------------------------------------------
  subroutine limit_twoph_dx_line(n,xx,xxmin,xxmax,dxx,np,param)
    use nonlinear_solvers, only: limit_dx
    implicit none
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: xx,xxmin,xxmax
    real, dimension(n),     intent(inout) :: dxx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: param
    real :: scaling
    real :: Z(nc),X1(nc),X2(nc),beta,t,p,v1,v2,K(nc)
    real :: b(nc),bx1,bx2
    !
    call limit_dx(n,xx,xxmin,xxmax,dxx,np,param)
    ! Additional test for minimum volume?
    if (minval(abs(Xx(nc+2:nc+3)+dxx(nc+2:nc+3)-Xxmin(nc+2:nc+3))) < 0.05) then
      b = param(nc+3:2*nc+2)
      Z = param(1:nc)
      call get_two_phase_variables_tv(Xx+dxx,Z,T,P,v1,v2,X1,X2,K,beta,&
           no_press_calc=.true.)
      bx1 = sum(X1*b)
      bx2 = sum(X2*b)
      scaling = 1.0
      do while (v1 < bx1 .or. v2 < bx2)
        scaling = scaling*0.5
        call get_two_phase_variables_tv(Xx+scaling*dxx,&
             Z,T,P,v1,v2,X1,X2,K,beta,&
             no_press_calc=.true.)
        bx1 = sum(X1*b)
        bx2 = sum(X2*b)
      enddo
      if (scaling < 1.0) then
        dxx = dxx * scaling
      endif
    endif
  end subroutine limit_twoph_dx_line

  !-----------------------------------------------------------------!
  !> Hydrate curve mapping limits
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hydrate_var_twoph_tv_limits(Xsol,Xmin,Xmax,b)
    use thermopack_var, only: thermo_model, get_active_thermo_model
    use numconstants, only: expMax, expMin, Small
    use eosdata, only: eosCPA
    use cubic_eos, only: get_b_linear_mix
    use volume_shift, only: get_c_mix
    implicit none
    real, dimension(nc+4), intent(in) :: Xsol !< Variable vector
    real, dimension(nc+4), intent(out) :: Xmin, Xmax !< Variable vector
    real, dimension(nc), intent(out) :: b
    ! Locals
    real :: Z(nc), T
    logical :: needalt, isCPA
    integer :: i
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    Xmin = expMin
    Xmax = expMax
    Xmin(nc+1) = log(tpTmin) !Tmin
    Xmax(nc+1) = log(tpTmax) !Tmax
    T = exp(Xsol(nc+1))
    !
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      b = 1.0e-7
    else
      do i=1,nc
        Z = 0
        Z(i) = 1
        b(i) = get_b_linear_mix(Z) + get_c_mix(T,Z) + Small ! m3/mol
      enddo
    endif
    Xmin(nc+2:nc+3) = log(1.0e-7) !v min
    Xmax(nc+2:nc+3) = log(100.0) !v max
    Xmin(nc+4) = 0
    Xmax(nc+4) = 1
  end subroutine hydrate_var_twoph_tv_limits

  subroutine get_two_phase_variables_tv(Xvar,Z,T,P,v1,v2,X1,X2,K,beta,no_press_calc)
    use saturation_tv, only: get_variables_tv
    ! Input
    real, intent(in) :: Xvar(nc+4)
    real, intent(in) :: Z(nc)
    logical, optional, intent(in) :: no_press_calc
    ! Output
    real, dimension(nc), intent(out) :: K, X1, X2
    real, intent(out) :: v1, v2, t, p, beta
    ! Locals
    real :: param(nc+3)
    beta = Xvar(nc+4)
    param(1:nc) = Z
    param(nc+1) = beta
    call get_variables_tv(Xvar,param,T,P,v1,v2,X1,X2,K,no_press_calc)
  end subroutine get_two_phase_variables_tv

  !-----------------------------------------------------------------------------
  !> Print X state
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine print_X_state_twoph(X,Z)
    implicit none
    real, dimension(nc+3), intent(in) :: X
    real, dimension(nc), intent(in) :: Z
    ! Locals
    real :: beta, K(nc), T, v1, v2, X1(nc), X2(nc), p
    !
    call get_two_phase_variables_tv(X,Z,T,P,v1,v2,X1,X2,K,beta)
    print *,"K: ",K
    print *,"Temperature: ",T
    print *,"v1: ",v1
    print *,"v2: ",v2
    print *,"beta: ",beta
    print *,"X1: ",X1
    print *,"X2: ",X2
    print *,"pressure: ",p
  end subroutine print_X_state_twoph

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-12
  !-----------------------------------------------------------------------------
  subroutine hyd_twoph_fun_newton_tv(G,Xvar,param)
    use saturation_tv, only: sat_fun_newton_tv
    implicit none
    real, dimension(nc+4), intent(out) :: G !< Function values
    real, dimension(nc+4), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2), intent(inout) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: lnfug
    integer :: s
    real :: param_sat_tv(nc+3)
    real :: T,P1,v1,v2,X1(nc),X2(nc),K(nc),beta
    real :: ln_s, ps, fug_wh, vS

    Z = param(1:nc)
    s = nint(param(nc+1))
    ln_s = param(nc+2)

    call get_two_phase_variables_tv(Xvar,Z,T,P1,v1,v2,X1,X2,K,beta)
    param_sat_tv(1:nc) = Z
    param_sat_tv(nc+1) = beta
    param_sat_tv(nc+2) = 1 ! Set dummy value and handle specification here
    param_sat_tv(nc+3) = ln_s
    call sat_fun_newton_tv(G(1:nc+3),Xvar(1:nc+3),param_sat_tv)
    !
    call thermo_tv(t,v1,X1,lnfug)
    call fugacity_water_in_hydrate_TVn(T,v1,X1,fug_wh)
    ! Function value
    G(nc+4) = lnfug(water_idx) - log(fug_wh)
    ! Sepcification
    if (s <= nc+4) then
      G(nc+2) = Xvar(s) - ln_s
    else if (s == nc+5) then
      ! Pressure
      ps = exp(ln_s)
      ! Volume average for pressure error scaling
      vS = 2/(1/v1+1/v2)
      G(nc+2) = (p1-ps)*vS/(Rgas*T)
    endif

    !print *,"Xvar",Xvar
    !print *,"G",G
  end subroutine hyd_twoph_fun_newton_tv

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hyd_twoph_diff_newton_tv(J,Xvar,param)
    use thermo_utils, only: isSingleComp
    use saturation_tv, only: sat_diff_newton_tv
    implicit none
    real, dimension(nc+4), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+4,nc+4), intent(out) :: J !< Function differentials
    real, dimension(nc+2) :: param !< Parameter vector
    ! Locals
    real :: Z(nc)
    real :: lnfug1(nc), lnfug1_T(nc), lnfug1_V(nc), lnfug1_n(nc,nc)
    real :: lnfug2(nc), lnfug2_T(nc), lnfug2_V(nc), lnfug2_n(nc,nc)
    real :: p1, dp1dv, dp1dt, dp1dn(nc)
    real :: p2, dp2dv, dp2dt, dp2dn(nc)
    real :: fug_wh, fug_wh_T, fug_wh_v, fug_wh_n(nc)
    real :: lnfug_wh_T, lnfug_wh_v, lnfug_wh_n(nc)
    integer :: s, i
    real :: param_sat_tv(nc+3)
    real :: T,v1,v2,X1(nc),X2(nc),K(nc),beta,fac
    real :: ln_s, ps, vS, dvSdv1_div_vS, dvSdv2_div_vS
    real :: dX1dbeta(nc), dX2dbeta(nc)
    !
    Z = param(1:nc)
    s = nint(param(nc+1))
    ln_s = param(nc+2)

    call get_two_phase_variables_tv(Xvar,Z,T,P1,v1,v2,X1,X2,K,beta)
    param_sat_tv(1:nc) = Z
    param_sat_tv(nc+1) = beta
    param_sat_tv(nc+2) = 1 ! Set dummy value and handle specification here
    param_sat_tv(nc+3) = ln_s
    J = 0
    call sat_diff_newton_tv(J(1:nc+3,1:nc+3),Xvar(1:nc+3),param_sat_tv)
    ! Need differentials wrpt. beta
    do i=1,nc
      if (Z(i) > 0.0) then
        dX2dbeta(i) = -(X1(i)-X2(i))/Z(i)
      else
        dX2dbeta(i) = 0
      endif
    enddo
    dX1dbeta = X1*dX2dbeta
    dX2dbeta = X2*dX2dbeta
    !
    ! Volume average for pressure error scaling
    vS = 2/(1/v1+1/v2)
    if (debug_hyd) dvSdv1_div_vS = 1/(1/v1 + 1/v2)/v1**2
    if (debug_hyd) dvSdv2_div_vS = 1/(1/v1 + 1/v2)/v2**2
    fac = vS/(Rgas*T)
    !
    call thermo_tv(t,v2,X2,lnfug2,lnphit=lnfug2_t,lnphiv=lnfug2_v,lnphin=lnfug2_n)
    call thermo_tv(t,v1,X1,lnfug1,lnphit=lnfug1_t,lnphiv=lnfug1_V,lnphin=lnfug1_n)
    p2 = pressure(t,v2,X2,dpdv=dp2dv,dpdt=dp2dt,dpdn=dp2dn)
    p1 = pressure(t,v1,X1,dpdv=dp1dv,dpdt=dp1dt,dpdn=dp1dn)
    call fugacity_water_in_hydrate_TVn(T,v1,X1,fug_wh,fug_wh_T,fug_wh_v,fug_wh_n)
    lnfug_wh_T = fug_wh_T/fug_wh
    lnfug_wh_v = fug_wh_v/fug_wh
    lnfug_wh_n = fug_wh_n/fug_wh

    ! beta differential of sat_diff_newton_tv equations
    do i=1,nc
      J(i,nc+4) = sum(lnfug1_n(i,:)*dX1dbeta) - sum(lnfug2_n(i,:)*dX2dbeta)
    enddo
    J(nc+1,nc+4) = sum(dX1dbeta-dX2dbeta)
    J(nc+3,nc+4) = sum(dX1dbeta*dp1dn-dX2dbeta*dp2dn)*fac

    ! K differential
    do i=1,nc
      if (Z(i) > 0.0) then
        J(nc+4,i) = (lnfug1_n(water_idx,i)-lnfug_wh_n(i))*(1.0-beta)*X2(i)*X1(i)/Z(i)
      endif
    enddo

    ! Temperature differential
    J(nc+4,nc+1) = T*(lnfug1_T(water_idx) - lnfug_wh_T)

    ! Volume differential
    J(nc+4,nc+2) = v1*(lnfug1_V(water_idx) - lnfug_wh_V)

    ! beta differential
    J(nc+4,nc+4) = sum(dX1dbeta*lnfug1_n(water_idx,:) - dX1dbeta*lnfug_wh_n)

    ! Specification row/column
    if (s <= nc+4) then
      J(nc+2,:) = 0.0
      J(nc+2,s) = 1.0
    else if (s == nc+5) then
      ! Pressure
      ps = exp(ln_s)
      ! K differentials
      do i=1,nc
        if (Z(i) > 0.0) then
          J(nc+2,i) = fac*((1.0-beta)*dp1dn(i))*X2(i)*X1(i)/Z(i)
        else
          J(nc+2,i) = 0
        endif
      enddo
      J(nc+2,nc+1) = T*dp1dT*fac
      if (debug_hyd) J(nc+2,nc+1) = J(nc+2,nc+1) - (p1-ps)*fac
      J(nc+2,nc+2) = dp1dv*v1*fac
      if (debug_hyd) J(nc+2,nc+2) = J(nc+2,nc+2) + v1*dvSdv1_div_vS*(p1-ps)*fac
      J(nc+2,nc+3) =0
      if (debug_hyd) J(nc+2,nc+3) = J(nc+2,nc+3) + v2*dvSdv2_div_vS*(p1-ps)*fac
      !
      J(nc+2,nc+4) = sum(dp1dn*dX1dbeta)*fac
    endif

  end subroutine hyd_twoph_diff_newton_tv

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along the hydrate line
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hyd_twoph_newton_tv_extrapolate(Xvar,param,dXdS,dpds)
    implicit none
    real, dimension(nc+4), intent(in) :: Xvar
    real, dimension(nc+2), intent(inout) :: param
    real, dimension(nc+4), intent(out) :: dXdS
    real, intent(out) :: dPdS
    ! Locals
    real :: Z(nc), vS
    real :: T,v1,v2,X1(nc),X2(nc),K(nc),beta
    real :: p1, dp1dv, dp1dt, dp1dn(nc)
    real :: dX1dlnK(nc), dX1dbeta(nc)
    integer :: s, i
    real, dimension(nc+4,nc+4) :: Jac
    integer, dimension(nc+4) :: INDX
    integer :: INFO

    call hyd_twoph_diff_newton_tv(Jac,Xvar,param)
    Z = param(1:nc)
    call get_two_phase_variables_tv(Xvar,Z,T,P1,v1,v2,X1,X2,K,beta)

    p1 = pressure(T,v1,X1,dpdv=dp1dv,dpdt=dp1dt,dpdn=dp1dn)
    s = nint(param(nc+1))
    dXdS = 0
    if (s <= nc+4) then
      dXdS(nc+2) = 1
    else
      ! Specified pressure
      vS = 2/(1/v1+1/v2)
      ! dlnp
      dXdS(nc+2) = p1*vS/(Rgas*T)
    endif

    ! Solve equation system
    call DGESV( nc+4, 1, Jac, nc+4, INDX, dXdS, nc+4, INFO )

    ! Mol number differentials
    do i=1,nc
      if (Z(i) > 0.0) then
        dX1dlnK(i) = (1.0-beta)*X2(i)*X1(i)/Z(i)
        dX1dbeta(i) = -X1(i)*(X1(i)-X2(i))/Z(i)
      else
        dX1dlnK(i) = 0
        dX1dbeta(i) = 0
      endif
    enddo

    dpds = v1*dp1dv*dxds(nc+2) + T*dp1dt*dxds(nc+1) + &
         sum(dp1dn*dX1dlnK*dxds(1:nc)) + &
         sum(dp1dn*dX1dbeta*dxds(nc+4))
  end subroutine hyd_twoph_newton_tv_extrapolate

  !-----------------------------------------------------------------------------
  !> Test variable sensitivities along the hydrate curve for two-phase fluid
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hyd_twoph_newton_tv_extrapolate_test(Xin,Zin)
    implicit none
    real, optional, dimension(nc+4) :: Xin
    real, optional, dimension(nc) :: Zin
    ! Locals
    real, dimension(nc+4) :: Xvar1, Xvar2, Xvar0
    real, parameter, dimension(6) :: XvarInit = (/1.9652658382049047E-003, &
         -3.0268140104256873, 5.5757412839164209, -7.3985988934060671, &
         -9.8895239674956770, 0.99 /)
    character(len=15), dimension(nc+5) :: var_names
    real, dimension(nc+2) :: param
    real, dimension(nc+4) :: dXdS
    real :: T, ln_s, dpds, p1, p2, ds, p, eps
    integer :: s, ierr, iter
    real :: Z(nc)
    ! Debug
    !real :: v1,v2,X1(nc),X2(nc),K(nc),beta
    !real :: T_2,v1_2,v2_2,X1_2(nc),X2_2(nc),K_2(nc),beta_2

    var_names(1) = "lnK1"
    var_names(2) = "lnK2"
    var_names(3) = "ln(T)"
    var_names(4) = "ln(v1)"
    var_names(5) = "ln(v2)"
    var_names(6) = "beta"
    var_names(7) = "dlnP"
    !call init_cubic("CO2,H2O","SRK")
    if (present(Xin) .and. present(Zin)) then
      Xvar0 = Xin
      z = Zin
    else
      Xvar0 = XvarInit
      z = (/1-100.0e-6,100.0e-6/)
    endif
    param(1:nc) = Z
    s = nc+4
    param(nc+1) = real(s)
    ln_s = Xvar0(s)
    param(nc+2) = ln_s
    iter = newton_two_phase_fluid_hydrate_curve(Xvar0,param,T,P,ierr)
    !call get_two_phase_variables_tv(Xvar0,Z,T,P,v1,v2,X1,X2,K,beta)
    !print *,"iter,ierr",iter,ierr
    eps = 1.0e-4

    ! Loop
    do s=1,nc+5
      print *,"s",s
      ds = eps
      param(nc+1) = real(s)
      call hyd_twoph_newton_tv_extrapolate(Xvar0,param,dXdS,dpds)
      if (s <= nc+4) then
        ds = ds/maxval(abs(dXdS))
        param(nc+2) = Xvar0(s) - ds
      else
        param(nc+2) = log(p) - ds
      endif
      Xvar1 = Xvar0 - dXdS*ds
      iter = newton_two_phase_fluid_hydrate_curve(Xvar1,param,T,P1,ierr)
      !print *,"iter,ierr",iter,ierr
      !call get_two_phase_variables_tv(Xvar1,Z,T,P1,v1,v2,X1,X2,K,beta)
      if (s <= nc+4) then
        param(nc+2) = Xvar0(s) + ds
      else
        param(nc+2) = log(p) + ds
      endif
      Xvar2 = Xvar0 + dXdS*ds
      iter = newton_two_phase_fluid_hydrate_curve(Xvar2,param,T,P2,ierr)
      !print *,"iter,ierr",iter,ierr
      !call get_two_phase_variables_tv(Xvar2,Z,T_2,P2,v1_2,v2_2,X1_2,X2_2,K_2,beta_2)
      print *,trim(var_names(s))//":"
      print *,(Xvar2-Xvar1)/(2*ds)
      print *,dXdS
      print *,"dpds:"
      print *,dpds,(p2-p1)/(2*ds)
    enddo
  end subroutine hyd_twoph_newton_tv_extrapolate_test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Methods for three phase curves
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------!
  subroutine map_hydrate_curve_three_phase_fluid(Xsol_tv_sat,param_tv,p_min,p_max,T_min,tppl)
  !-----------------------------------------------------------------!
    implicit none
    real, intent(in) :: Xsol_tv_sat(2*nc+5)
    real, intent(in) :: param_tv(nc+4)
    real, intent(in) :: p_min,p_max,T_min
    type(multi_phase_point_list), intent(inout) :: tppl
    ! Internal
    integer :: s,ierr,iter
    real :: T,P,Pold
    real :: X(2*nc+7), Xold(2*nc+7), dXds(2*nc+7), dXdsOld(2*nc+7)
    real :: Z(nc), param(nc+2), sgn, Told, beta_lin, dbeta
    real :: dS, tuning, dP, dPds, dS_max, dS_min, ln_spec, dT
    real :: beta_lin_vec(3)
    integer :: smax, beta_lin_idx
    logical :: recalculate
    logical :: exit_after_saving
    logical :: set_beta_lin
    real, parameter :: maxdT = 10.0, maxdP = 15.0, maxdbeta = 0.05
    real, parameter :: excessive_dT = 25.0
    real, parameter :: beta_limit = 0.025
    !
    exit_after_saving = .false.
    recalculate = .false.
    set_beta_lin = .false.
    Z = param_tv(1:nc)
    param(1:nc) = Z
    s = 2*nc+6 ! Start by fixating betaW
    param(nc+1) = real(s)
    !
    X(1:2*nc+5) = Xsol_tv_sat
    X(2*nc+6) = 0.0 ! betaW
    X(2*nc+7) = 1.0 - X(2*nc+2) ! betaL

    param(nc+2) = X(s)
    iter = newton_three_phase_fluid_hydrate_curve(X,param,T,P,ierr)
    Xold = X
    sgn = 1 ! Start by increaseing betaW
    !
    !call print_X_state_threeph(X,Z)
    !call test_hyd_threeph_diff_newton_tv(X,param)
    !stop
    !
    X = Xold
    call tppl%add_point_threeph(X)
    P = tppl%list(tppl%n)%P
    dXdS = 0
    !
    dS_max = 0.25
    dS_min = 0.01
    dS = dS_min
    tuning = 1.2
    do while (tppl%n < size(tppl%list))
      dXdSold = dXdS
      call hyd_threeph_newton_tv_extrapolate(X,param,dXdS,dPds)
      smax = maxloc(abs(dXdS),dim=1)
      if ((.not. smax == s) .and. tppl%n > 1) then
        s = smax
        ! Rescaling the sensitivities
        sgn = sign(1.0,X(s) - Xold(s))
        dPds = dPds / dXdS(s)
        dXdS = dXdS / dXdS(s)
        dXdSold = dXdSold / dXdSold(s)
        param(nc+1) = real(s)
      endif
      dT = abs(exp(X(2*nc+1) + dXdS(2*nc+1)*dS*sgn) - exp(X(2*nc+1)))
      if (dT > maxdT) then
        ! Limit step in temperature
        dS = max(min(maxdT/dT,dS_max),dS_min)
      endif
      dP = abs(dPds*ds)/1e5
      if (dP > maxdP) then
        ! Limit step in pressure
        dS = max(min(dS*maxdP/dP,dS_max),dS_min)
      endif
      ! Limit step in beta
      if (abs(dXdS(2*nc+6)*dS) > maxdbeta) then
        dS = max(min(maxdbeta/abs(dXdS(2*nc+6)),dS_max),dS_min)
      endif
      ! Beta limit
      beta_lin_vec(1) = X(2*nc+2) + dXdS(2*nc+2)*dS*sgn
      beta_lin_vec(2) = X(2*nc+6) + dXdS(2*nc+6)*dS*sgn
      beta_lin_vec(3) = X(2*nc+7) + dXdS(2*nc+7)*dS*sgn
      beta_lin_idx = minloc(min(beta_lin_vec,1-beta_lin_vec), dim=1)
      beta_lin = beta_lin_vec(beta_lin_idx)
      if (beta_lin_idx == 1) then
        beta_lin_idx = 2*nc+2
      else if (beta_lin_idx == 2) then
        beta_lin_idx = 2*nc+6
      else
        beta_lin_idx = 2*nc+7
      endif
      if (dXdS(beta_lin_idx)*sgn < 0.0 .and. beta_lin < beta_limit) then
        if (beta_lin <= 0.0) then
          set_beta_lin = .true.
          beta_lin =  0
          dS = X(beta_lin_idx)/abs(dXdS(beta_lin_idx))
        endif
        s = beta_lin_idx
        param(nc+1) = real(s)
      else if (dXdS(beta_lin_idx)*sgn > 0.0 .and. beta_lin > 1-beta_limit) then
        if (beta_lin >= 1.0) then
          set_beta_lin = .true.
          beta_lin = 1
          dbeta = abs(1 - X(beta_lin_idx))
          dS = dbeta/abs(dXdS(beta_lin_idx))
        endif
        s = beta_lin_idx
        param(nc+1) = real(s)
      endif

      Pold = P
      Xold = X
      Told = T
      !dX = dXdS*dS*sgn
      ! Make sure beta is between zero and one
      !call limit_threeph_dx(Xold,dX,Z,scaling)
      X = Xold + dXdS*dS*sgn
      ! Make sure beta is between zero and one
      X(2*nc+2) = min(max(X(2*nc+2),real(0)),real(1))
      X(2*nc+6) = min(max(X(2*nc+6),real(0)),real(1))
      X(2*nc+7) = min(max(X(2*nc+7),real(0)),real(1))
      if (s == 2*nc+8) then
        param(nc+2) = log(p) + dS*sgn
      else if (s == 2*nc+6 .and. set_beta_lin) then
        param(nc+2) = beta_lin
      else
        param(nc+2) = X(s)
      endif

      call wrap_newton_three_phase_fluid_hydrate_curve(param,&
           Pold,dpds,Xold,X,dXds,sgn,s,ds,T,P,ierr,iter)

      !Exit at thermo limit or defined pressure
      if (p < p_min) then
        s = 2*nc+8
        recalculate = .true.
        ln_spec = log(p_min)
      else if (p >= p_max) then
        s = 2*nc+8
        recalculate = .true.
        ln_spec = log(p_max)
      endif

      ! Is temperature decreasing? - And below Tmin + safety limit?
      if (X(2*nc+1) - Xold(2*nc+1) < 0.0 .and. T < T_min + 0.01) then
        ! Exit at temperature
        s = 2*nc+1
        ln_spec = log(T_min)
        recalculate = .true.
      endif

      ! Is beta, betaL or betaW zero
      if (X(2*nc+2) <= 0.0 .or. X(2*nc+7) <= 0.0 .or. X(2*nc+6) < 0.0) then
        ! Exit
        exit_after_saving = .true.
      endif

      if (recalculate) then
        ! Extrapolate from previous point
        param(nc+1) = real(s)
        param(nc+2) = ln_spec
        if (s <= 2*nc+7) then
          dS = ln_spec - Xold(s) ! Sign included
        else if (s == 2*nc+8) then
          if (abs(dpds) > 10.0) ds = (exp(ln_spec) - Pold)/dpds
        endif
        X = Xold + dXdS*dS
        iter = newton_three_phase_fluid_hydrate_curve(X,param,T,P,ierr)
        exit_after_saving = .true.
      endif

      ! Add point to list
      call tppl%add_point_threeph(X)

      if (exit_after_saving) then
        exit
      endif

      ! Tune dS up or down based on how fast newton_single_phase_fluid_hydrate_curve converged
      if (iter < 3) then
        dS = dS * tuning
      else if (iter > 5) then
        dS = dS/tuning
      endif
      dS = max(min(dS,dS_max),dS_min)

    enddo
  end subroutine map_hydrate_curve_three_phase_fluid

  !-----------------------------------------------------------------------------
  !> Limit change in x, dx, to keep x between extreme values:
  !> xmin <= x <= xmax
  !>
  !> \author MH, Dec 2024
  !-----------------------------------------------------------------------------
  subroutine limit_threeph_dx(X,dX,Z,scaling)
    use nonlinear_solvers, only: limit_dx
    implicit none
    real, intent(in) :: Z(nc)
    real, intent(in) :: X(2*nc+7), dX(2*nc+7)
    real, intent(out) :: scaling
    ! Locals
    real :: xxmin(2*nc+7),xxmax(2*nc+7),dxx(2*nc+7),param(2*nc+2)
    real :: b(nc)
    integer :: n, np, i
    !
    call hydrate_var_threeph_tv_limits(X,xxmin,xxmax,b)
    param(nc+3:2*nc+2) = b
    param(1:nc) = Z
    dxx = dX
    n = 2*nc + 7
    np = 2*nc + 2
    call limit_threeph_dx_line(n,X,xxmin,xxmax,dxx,np,param)
    i = maxloc(abs(dX), dim=1)
    if (abs(dX(i)) > 0.0) then
      scaling = dxx(i)/dX(i)
    else
      scaling = 1
    endif
  end subroutine limit_threeph_dx

  !-----------------------------------------------------------------------------
  !> Solve for hydrate appearance point in equilibrium with three fluid phases
  !>
  !> \author Morten Hammer, 2024-7
  !-----------------------------------------------------------------------------
  subroutine wrap_newton_three_phase_fluid_hydrate_curve(param,&
       Pold,dpds,XXold,XX,dXds,sgn,s,ds,T,P,ierr,iter)
    use nonlinear_solvers, only: test_differentials
    implicit none
    real, intent(in) :: Pold, dpds
    real, dimension(2*nc+7), intent(in) :: XXold, dXds
    real, dimension(2*nc+7), intent(inout) :: XX
    real, dimension(nc+2), intent(inout) :: param
    real, intent(in)     :: sgn
    integer, intent(in)  :: s
    real, intent(inout)  :: ds
    integer, intent(out) :: ierr
    integer, intent(out) :: iter
    real, intent(out)    :: T,P
    ! Locals
    integer :: i
    real :: halff, ds_in, Told
    real, parameter :: excessive_dT = 25.0
    logical :: excessive_T_jump
    Told = exp(XXold(2*nc+1))
    ds_in = ds
    halff = 1.0
    do i=1,3 ! Try 3 times before trying to double
      iter = newton_three_phase_fluid_hydrate_curve(XX,param,T,P,ierr)
      excessive_T_jump = (abs(Told - T) > excessive_dT)
      if (ierr == 0 .and. .not. excessive_T_jump) then
        ds = halff*ds
        exit
      endif
      halff = 0.5**i
      call update_step(halff)
    enddo
    if (ierr /= 0 .or. excessive_T_jump) then ! Try doubling initial step
      call update_step(2.0)
      iter = newton_three_phase_fluid_hydrate_curve(XX,param,T,P,ierr)
      excessive_T_jump = (abs(Told - T) > excessive_dT)
      if (ierr == 0) then
        ds = ds*2.0
      endif
    endif
    do i=1,2 ! Try 2 more times before giving up
      iter = newton_three_phase_fluid_hydrate_curve(XX,param,T,P,ierr)
      excessive_T_jump = (abs(Told - T) > excessive_dT)
      if (ierr == 0 .and. .not. excessive_T_jump) then
        ds = halff*ds
        exit
      endif
      halff = 0.5*halff
      call update_step(halff)
    enddo
    if (ierr /= 0 .or. excessive_T_jump) then ! Giving up
      halff = 2.0*halff
      call update_step(halff)
      print *,"s",nint(param(nc+1))
      print *,"XX",XX
      print *,"param",param
      call print_X_state_threeph(XX,param(1:nc))
      call test_differentials(XX,param,hyd_threeph_fun_newton_tv,&
           hyd_threeph_diff_newton_tv)
      call stoperror("wrap_newton_three_phase_fluid_hydrate_curve: Giving up")
    endif
  contains
    subroutine update_step(fac)
      real, intent(in) :: fac
      XX = XXold + dXds*sgn*ds*fac
      if (s == 2*nc+8) then
        P = Pold + dpds*sgn*ds*fac
        param(nc+2) = log(P)
      else
        param(nc+2) = XX(s)
      endif
    end subroutine update_step
  end subroutine wrap_newton_three_phase_fluid_hydrate_curve

  !-----------------------------------------------------------------!
  !> Hydrate curve mapping limits
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine hydrate_var_threeph_tv_limits(Xsol,Xmin,Xmax,b)
    use thermopack_var, only: thermo_model, get_active_thermo_model
    use numconstants, only: expMax, expMin, Small
    use eosdata, only: eosCPA
    use cubic_eos, only: get_b_linear_mix
    use volume_shift, only: get_c_mix
    implicit none
    real, dimension(2*nc+7), intent(in) :: Xsol !< Variable vector
    real, dimension(2*nc+7), intent(out) :: Xmin, Xmax !< Variable vector
    real, dimension(nc), intent(out) :: b
    ! Locals
    real :: Z(nc), T
    logical :: needalt, isCPA
    integer :: i
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    Xmin = expMin
    Xmax = expMax
    Xmin(2*nc+1) = log(tpTmin) !Tmin
    Xmax(2*nc+1) = log(tpTmax) !Tmax
    T = exp(Xsol(2*nc+1))
    !
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      b = 1.0e-7
    else
      do i=1,nc
        Z = 0
        Z(i) = 1
        b(i) = get_b_linear_mix(Z) + get_c_mix(T,Z) + Small ! m3/mol
      enddo
    endif
    Xmin(2*nc+3:2*nc+5) = log(1.0e-7) !v min
    Xmax(2*nc+3:2*nc+5) = log(100.0) !v max
    Xmin(2*nc+2) = 0 ! beta
    Xmax(2*nc+2) = 1 ! beta
    Xmin(2*nc+6:2*nc+7) = 0 ! betaW
    Xmax(2*nc+6:2*nc+7) = 1 ! betaL
  end subroutine hydrate_var_threeph_tv_limits

  subroutine get_three_phase_variables_tv(Xvar,Z,T,P,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW,no_press_calc)
    ! Input
    real, intent(in) :: Xvar(2*nc+7)
    real, intent(in) :: Z(nc)
    logical, optional, intent(in) :: no_press_calc
    ! Output
    real, dimension(nc), intent(out) :: Ky, Kx, Y, X, W
    real, intent(out) :: vY, vX, vW, t, p, beta, betaL, betaW
    ! Locals
    integer :: i
    logical :: no_press_calc_local
    betaW = Xvar(2*nc+6)
    betaL = Xvar(2*nc+7)
    t = exp(Xvar(2*nc+1))
    beta = Xvar(2*nc+2)
    vY = exp(Xvar(2*nc+3))
    vX = exp(Xvar(2*nc+4))
    vW = exp(Xvar(2*nc+5))
    Kx = exp(Xvar(1:nc))
    Ky = exp(Xvar(1+nc:2*nc))
    W = 0.0
    do i=1,nc
      if (Z(i) > 0.0) then
        W(i) = Z(i)/(betaW + beta*Ky(i) + betaL*Kx(i))
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
  end subroutine get_three_phase_variables_tv

  subroutine set_three_phase_variables_tv(Z,T,vY,vX,vW,Y,X,W,beta,betaL,betaW,Xvar)
    ! Input
    real, dimension(nc), intent(in) :: Z, Y, X, W
    real, intent(in) :: T, vY, vX, vW, beta, betaL, betaW
    ! Output
    real, intent(out) :: Xvar(2*nc+7)
    ! Locals
    integer :: i
    real :: Kx(nc), Ky(nc)
    Xvar(2*nc+6) = betaW
    Xvar(2*nc+7) = betaL
    Xvar(2*nc+1) = log(t)
    Xvar(2*nc+2) = beta
    Xvar(2*nc+3) = log(vY)
    Xvar(2*nc+4) = log(vX)
    Xvar(2*nc+5) = log(vW)
    do i=1,nc
      if (Z(i) > 0.0) then
        Ky(i) = Y(i)/W(i)
        Kx(i) = X(i)/W(i)
      else
        Ky(i) = 1
        Kx(i) = 1
      endif
    enddo
    Xvar(nc+1:2*nc) = log(Ky)
    Xvar(1:nc) = log(Kx)
  end subroutine set_three_phase_variables_tv

  !-----------------------------------------------------------------------------
  !> Print X state
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  subroutine print_X_state_threeph(Xvar,Z)
    implicit none
    real, dimension(2*nc+7), intent(in) :: Xvar
    real, dimension(nc), intent(in) :: Z
    ! Locals
    real, dimension(nc) :: Ky, Kx, Y, X, W
    real :: vY, vX, vW, t, p, beta, betaL, betaW
    !
    call get_three_phase_variables_tv(Xvar,Z,T,P,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW)
    print *,"Ky: ",Ky
    print *,"Kx: ",Kx
    print *,"Temperature: ",T
    print *,"beta: ",beta
    print *,"vY: ",vY
    print *,"vX: ",vX
    print *,"vW: ",vW
    print *,"betaW: ",betaW
    print *,"betaL: ",betaL
    print *,"Y: ",Y
    print *,"X: ",X
    print *,"W: ",W
    print *,"Pressure: ",p
  end subroutine print_X_state_threeph

  !-----------------------------------------------------------------------------
  !> Saturation curve function values for non-linear solver
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  subroutine hyd_threeph_fun_newton_tv(G,Xvar,param)
    use multi_phase_envelope_tv, only: three_ph_line_fun_newton
    implicit none
    real, dimension(2*nc+7), intent(out) :: G !< Function values
    real, dimension(2*nc+7), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+3), intent(inout) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    integer :: s, i
    real :: T,P1,vY,vX,vW,Y(nc),X(nc),W(nc),Ky(nc),Kx(nc),beta,betaW,betaL
    real :: ln_s, ps, fug_wh, vS, pY, pX, pW
    real, dimension(nc) :: lnFugY, lnFugX, lnFugW

    Z = param(1:nc)
    s = nint(param(nc+1))
    ln_s = param(nc+2)

    call get_three_phase_variables_tv(Xvar,Z,T,P1,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW)
    ! Volume average for pressure error scaling
    vS = 3/(1/vW + 1/vX + 1/vY)

    call thermo_tv(t,vY,Y,lnFugY)
    call thermo_tv(t,vW,W,lnFugW)
    call thermo_tv(t,vX,X,lnFugX)
    pY = pressure(t,vY,Y)
    pW = pressure(t,vW,W)
    pX = pressure(t,vX,X)

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
    !
    G(2*nc+4) = vS*(Px-Pw)/(Rgas*T)
    G(2*nc+5) = vS*(Py-Pw)/(Rgas*T)

    call fugacity_water_in_hydrate_TVn(T,vW,W,fug_wh)
    ! Hydrate function value
    G(2*nc+6) = lnfugW(water_idx) - log(fug_wh)
    ! Phase fraction values
    G(2*nc+7) = beta + betaL + betaW - 1
    ! Specification
    if (s <= 2*nc+7) then
      G(2*nc+3) = Xvar(s) - ln_s
    else if (s == 2*nc+8) then
      ! Pressure
      ps = exp(ln_s)
      G(2*nc+3) = (pW-ps)*vS/(Rgas*T)
    endif

    !print *,"Xvar",Xvar
    !print *,"G",G
  end subroutine hyd_threeph_fun_newton_tv

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  subroutine hyd_threeph_diff_newton_tv(J,Xvar,param)
    use thermo_utils, only: isSingleComp
    use multi_phase_envelope_tv, only: three_ph_line_diff_newton
    implicit none
    real, dimension(2*nc+7), intent(in) :: Xvar !< Variable vector
    real, dimension(2*nc+7,2*nc+7), intent(out) :: J !< Function differentials
    real, dimension(nc+3) :: param !< Parameter vector
    ! Locals
    real :: Z(nc)
    real :: lnfugY(nc), lnfugY_T(nc), lnfugY_V(nc), lnfugY_n(nc,nc)
    real :: lnfugX(nc), lnfugX_T(nc), lnfugX_V(nc), lnfugX_n(nc,nc)
    real :: lnfugW(nc), lnfugW_T(nc), lnfugW_V(nc), lnfugW_n(nc,nc)
    real :: pY, dpYdv, dpYdt, dpYdn(nc)
    real :: pX, dpXdv, dpXdt, dpXdn(nc)
    real :: pW, dpWdv, dpWdt, dpWdn(nc)
    real :: fug_wh, fug_wh_T, fug_wh_v, fug_wh_n(nc)
    real :: lnfug_wh_T, lnfug_wh_v, lnfug_wh_n(nc)
    integer :: s, i
    real :: T,P1,vY,vX,vW,Y(nc),X(nc),W(nc),Ky(nc),Kx(nc),beta,betaL,betaW
    real :: ln_s, ps, vS, dvSdvY_div_vS, dvSdvX_div_vS, dvSdvW_div_vS
    real :: dYdbetaW(nc), dXdbetaW(nc), dWdbetaW(nc), dWdbeta(nc)
    real :: dWdbetaL(nc), dYdbetaL(nc), dXdbetaL(nc)
    real :: dWdlnKX(nc), dWdlnKY(nc), dXdlnKX(nc), dXdlnKY(nc)
    real :: dYdlnKX(nc), dYdlnKY(nc), dXdbeta(nc), dYdbeta(nc)
    real :: fac, fac_vec(nc)
    logical, parameter :: partial = .false.
    !
    Z = param(1:nc)
    s = nint(param(nc+1))
    ln_s = param(nc+2)

    call get_three_phase_variables_tv(Xvar,Z,T,P1,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW)

    J = 0
    ! Differentials of W
    do i=1,nc
      if (Z(i) > 0.0) then
        fac_vec(i) = -W(i)/(betaW + beta*Ky(i) + betaL*Kx(i))
        dWdbetaW(i) = fac_vec(i)
        dWdbeta(i) = Ky(i)*fac_vec(i)
        dWdbetaL(i) = Kx(i)*fac_vec(i)
        dWdlnKX(i) = Kx(i)*betaL*fac_vec(i)
        dWdlnKY(i) = Ky(i)*beta*fac_vec(i)
      else
        dWdbetaW(i) = 0
        dWdbeta(i) = 0
        dWdbetaL(i) = 0
        dWdlnKX(i) = 0
        dWdlnKY(i) = 0
      endif
    enddo
    dXdlnKX = Kx*W + Kx*dWdlnKX
    dXdlnKY = Kx*dWdlnKY
    dYdlnKX = Ky*dWdlnKX
    dYdlnKY = Ky*W + Ky*dWdlnKY
    dXdbeta = Kx*dWdbeta
    dYdbeta = Ky*dWdbeta
    dXdbetaL = Kx*dWdbetaL
    dYdbetaL = Ky*dWdbetaL
    dYdbetaW = Ky*dWdbetaW
    dXdbetaW = Kx*dWdbetaW
    !
    ! Volume average for pressure error scaling
    vS = 3/(1/vY+1/vX+1/vW)
    if (debug_hyd) dvSdvY_div_vS = 1/(1/vY+1/vX+1/vW)/vY**2
    if (debug_hyd) dvSdvX_div_vS = 1/(1/vY+1/vX+1/vW)/vX**2
    if (debug_hyd) dvSdvW_div_vS = 1/(1/vY+1/vX+1/vW)/vW**2
    fac = vS/(Rgas*T)
    !
    call thermo_tv(t,vX,X,lnfugX,lnphit=lnfugX_t,lnphiv=lnfugX_v,lnphin=lnfugX_n)
    call thermo_tv(t,vY,Y,lnfugY,lnphit=lnfugY_t,lnphiv=lnfugY_V,lnphin=lnfugY_n)
    call thermo_tv(t,vW,W,lnfugW,lnphit=lnfugW_t,lnphiv=lnfugW_V,lnphin=lnfugW_n)
    pX = pressure(t,vX,X,dpdv=dpXdv,dpdt=dpXdt,dpdn=dpXdn)
    pY = pressure(t,vY,Y,dpdv=dpYdv,dpdt=dpYdt,dpdn=dpYdn)
    pW = pressure(t,vW,W,dpdv=dpWdv,dpdt=dpWdt,dpdn=dpWdn)
    call fugacity_water_in_hydrate_TVn(T,vW,W,fug_wh,fug_wh_T,fug_wh_v,fug_wh_n)
    lnfug_wh_T = fug_wh_T/fug_wh
    lnfug_wh_v = fug_wh_v/fug_wh
    lnfug_wh_n = fug_wh_n/fug_wh

    if (.not. partial) then
      do i=1,nc
        J(1:nc,i+nc) = lnFugX_n(:,i)*dXdlnKY(i) - lnFugW_n(:,i)*dWdlnKY(i)
        J(1:nc,i) = lnFugX_n(:,i)*dXdlnKX(i) - lnFugW_n(:,i)*dWdlnKX(i)
        J(nc+1:2*nc,i) = lnFugY_n(:,i)*dYdlnKX(i) - lnFugW_n(:,i)*dWdlnKX(i)
        J(nc+1:2*nc,i+nc) = lnFugY_n(:,i)*dYdlnKY(i) - lnFugW_n(:,i)*dWdlnKY(i)
      enddo
    else
      do i=1,nc
        J(i,i) = 1
        J(nc+i,nc+i) = 1
      enddo
    endif
    J(2*nc+1,1:nc) = dWdlnKX
    J(2*nc+1,1+nc:2*nc) = dWdlnKY
    J(2*nc+2,1:nc) = dYdlnKX - dXdlnKX
    J(2*nc+2,1+nc:2*nc) = dYdlnKY - dXdlnKY

    ! Temperature differential
    J(1:nc,2*nc+1) = T*(lnFugX_T-lnFugW_T)
    J(1+nc:2*nc,2*nc+1) = T*(lnFugY_T-lnFugW_T)

    ! Volume differential
    J(1:nc,2*nc+4) = vX*lnFugX_v
    J(1:nc,2*nc+5) = -vW*lnFugW_v
    J(1+nc:2*nc,2*nc+3) = vY*lnFugY_v
    J(1+nc:2*nc,2*nc+5) = -vW*lnFugW_v

    ! Beta
    do i=1,nc
      J(1:nc,2*nc+2) = J(1:nc,2*nc+2) + &
           lnFugX_n(:,i)*dXdbeta(i) - lnFugW_n(:,i)*dWdbeta(i)
      J(1+nc:2*nc,2*nc+2) = J(1+nc:2*nc,2*nc+2) + &
           lnFugY_n(:,i)*dYdbeta(i) - lnFugW_n(:,i)*dWdbeta(i)
    enddo
    J(2*nc+1,2*nc+2) = sum(dWdbeta)
    J(2*nc+2,2*nc+2) = sum(dYdbeta-dXdbeta)

    ! Pressure differentials
    J(2*nc+4,1:nc) = (dPXdn*dXdlnKx-dPWdn*dWdlnKx)*fac
    J(2*nc+4,nc+1:2*nc) = (dPXdn*dXdlnKy-dPWdn*dWdlnKy)*fac
    J(2*nc+5,1:nc) = (dPYdn*dYdlnKx-dPWdn*dWdlnKx)*fac
    J(2*nc+5,nc+1:2*nc) = (dPYdn*dYdlnKy-dPWdn*dWdlnKy)*fac
    !
    J(2*nc+4,2*nc+2) = sum(dPXdn*dXdbeta-dPWdn*dWdbeta)*fac
    J(2*nc+5,2*nc+2) = sum(dPYdn*dYdbeta-dPWdn*dWdbeta)*fac
    !
    J(2*nc+4,2*nc+7) = sum(dPXdn*dXdbetaL-dPWdn*dWdbetaL)*fac
    J(2*nc+5,2*nc+7) = sum(dPYdn*dYdbetaL-dPWdn*dWdbetaL)*fac
    !
    J(2*nc+4,2*nc+1) = T*(dPxdT-dPwdT)*fac
    if (debug_hyd) J(2*nc+4,2*nc+1) = J(2*nc+4,2*nc+1) - (Px-Pw)*fac
    J(2*nc+5,2*nc+1) = T*(dPydT-dPwdT)*fac
    if (debug_hyd) J(2*nc+5,2*nc+1) = J(2*nc+5,2*nc+1) - (Py-Pw)*fac
    J(2*nc+4,2*nc+4) = vX*dpXdv*fac
    J(2*nc+4,2*nc+5) = -vW*dPwdv*fac
    if (debug_hyd) J(2*nc+4,2*nc+3) = J(2*nc+4,2*nc+3) + vY*dvSdvY_div_vS*(Px-Pw)*fac
    if (debug_hyd) J(2*nc+4,2*nc+4) = J(2*nc+4,2*nc+4) + vX*dvSdvX_div_vS*(Px-Pw)*fac
    if (debug_hyd) J(2*nc+4,2*nc+5) = J(2*nc+4,2*nc+5) + vW*dvSdvW_div_vS*(Px-Pw)*fac
    J(2*nc+5,2*nc+3) = vY*dPydv*fac
    J(2*nc+5,2*nc+5) = -vW*dPwdv*fac
    if (debug_hyd) J(2*nc+5,2*nc+3) = J(2*nc+5,2*nc+3) + vY*dvSdvY_div_vS*(Py-Pw)*fac
    if (debug_hyd) J(2*nc+5,2*nc+4) = J(2*nc+5,2*nc+4) + vX*dvSdvX_div_vS*(Py-Pw)*fac
    if (debug_hyd) J(2*nc+5,2*nc+5) = J(2*nc+5,2*nc+5) + vW*dvSdvW_div_vS*(Py-Pw)*fac

    ! beta differential of three_ph_line_diff_newton equations
    do i=1,nc
      J(i+nc,2*nc+6) = sum(lnfugY_n(i,:)*dYdbetaW) - sum(lnfugW_n(i,:)*dWdbetaW)
      J(i,2*nc+6) = sum(lnfugX_n(i,:)*dXdbetaW) - sum(lnfugW_n(i,:)*dWdbetaW)
    enddo
    J(2*nc+1,2*nc+6) = sum(dWdbetaW)
    J(2*nc+2,2*nc+6) = sum(dWdbetaW-dXdbetaW)
    J(2*nc+4,2*nc+6) = sum(dXdbetaW*dpXdn-dWdbetaW*dpWdn)*fac
    J(2*nc+5,2*nc+6) = sum(dYdbetaW*dpYdn-dWdbetaW*dpWdn)*fac

    ! K differentials
    do i=1,nc
      if (Z(i) > 0.0) then
        J(2*nc+6,i+nc) = (lnfugW_n(water_idx,i)-lnfug_wh_n(i))*beta*Ky(i)*fac_vec(i)
        J(2*nc+6,i) = (lnfugW_n(water_idx,i)-lnfug_wh_n(i))*betaL*Kx(i)*fac_vec(i)
      endif
    enddo

    ! Temperature differential
    J(2*nc+6,2*nc+1) = T*(lnfugW_T(water_idx) - lnfug_wh_T)

    ! Volume differential
    J(2*nc+6,2*nc+5) = vW*(lnfugW_V(water_idx) - lnfug_wh_V)

    ! beta differential
    J(2*nc+6,2*nc+2) = sum(dWdbeta*lnfugW_n(water_idx,:) - dWdbeta*lnfug_wh_n)

    ! betaW differential
    J(2*nc+6,2*nc+6) = sum(dWdbetaW*lnfugW_n(water_idx,:) - dWdbetaW*lnfug_wh_n)

    ! betaL differential
    J(2*nc+6,2*nc+7) = sum(dWdbetaL*lnfugW_n(water_idx,:) - dWdbetaL*lnfug_wh_n)

    ! Specification row/column
    if (s <= 2*nc+7) then
      J(2*nc+3,:) = 0.0
      J(2*nc+3,s) = 1.0
    else if (s == 2*nc+8) then
      ! Pressure
      ps = exp(ln_s)
      ! Ky and Kx differentials
      do i=1,nc
        if (Z(i) > 0.0) then
          J(2*nc+3,i+nc) = fac*dpWdn(i)*beta*Ky(i)*fac_vec(i)
          J(2*nc+3,i) = fac*dpWdn(i)*betaL*Kx(i)*fac_vec(i)
        else
          J(2*nc+3,i) = 0
          J(2*nc+3,i+nc) = 0
        endif
      enddo
      J(2*nc+3,2*nc+1) = T*dpWdT*fac
      if (debug_hyd) J(2*nc+3,2*nc+1) = J(2*nc+3,2*nc+1) - (pW-ps)*fac
      J(2*nc+3,2*nc+5) = dpWdv*vW*fac
      if (debug_hyd) J(2*nc+3,2*nc+5) = J(2*nc+3,2*nc+5) + vW*dvSdvW_div_vS*(pW-ps)*fac
      J(2*nc+3,2*nc+3) = 0
      if (debug_hyd) J(2*nc+3,2*nc+3) = J(2*nc+2,2*nc+3) + vY*dvSdvY_div_vS*(pW-ps)*fac
      J(2*nc+3,2*nc+4) = 0
      if (debug_hyd) J(2*nc+3,2*nc+4) = J(2*nc+2,2*nc+4) + vX*dvSdvX_div_vS*(pW-ps)*fac
      !
      J(2*nc+3,2*nc+2) = sum(dpWdn*dWdbeta)*fac
      J(2*nc+3,2*nc+6) = sum(dpWdn*dWdbetaW)*fac
      J(2*nc+3,2*nc+7) = sum(dpWdn*dWdbetaL)*fac
    endif

    ! beta differentials
    J(2*nc+7,2*nc+2) = 1
    J(2*nc+7,2*nc+6) = 1
    J(2*nc+7,2*nc+7) = 1

    ! BetaL
    do i=1,nc
      J(1:nc,2*nc+7) = J(1:nc,2*nc+7) + &
           lnFugX_n(:,i)*dXdbetaL(i) - lnFugW_n(:,i)*dWdbetaL(i)
      J(1+nc:2*nc,2*nc+7) = J(1+nc:2*nc,2*nc+7) + &
           lnFugY_n(:,i)*dYdbetaL(i) - lnFugW_n(:,i)*dWdbetaL(i)
    enddo
    J(2*nc+1,2*nc+7) = sum(dWdbetaL)
    J(2*nc+2,2*nc+7) = sum(dYdbetaL-dXdbetaL)

  end subroutine hyd_threeph_diff_newton_tv

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  subroutine test_hyd_threeph_diff_newton_tv(Xvar,param)
    implicit none
    real, dimension(2*nc+7), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2) :: param !< Parameter vector
    ! Locals
    integer :: i
    real, dimension(2*nc+7) :: X, F1, F2, dFdx
    real :: ds, dx, J(2*nc+7,2*nc+7)

    call print_X_state_threeph(Xvar,param(1:nc))
    !param(nc+1) = 2*nc+8 ! Fixate pressure
    param(nc+1) = 2*nc+7 ! Fixate betaL
    call hyd_threeph_diff_newton_tv(J,Xvar,param)
    ds = 1.0e-5
    do i=1,2*nc+7 !2*nc+7
      X = Xvar
      dx = sign(1.0, X(i))*max(ds*abs(X(i)), ds)
      print *,"dx",i,dx
      X(i) = Xvar(i) + dx
      call hyd_threeph_fun_newton_tv(F2,X,param)
      X(i) = Xvar(i) - dx
      call hyd_threeph_fun_newton_tv(F1,X,param)
      dFdx = (F2-F1)/(2*dx)
      print *,"i",i
      !print *,dFdx(2*nc+7)
      !print *,J(2*nc+7,i)
      print *,dFdx(:)
      print *,J(:,i)
      !stop
    enddo
    stop
  end subroutine test_hyd_threeph_diff_newton_tv

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along the hydrate line
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  subroutine hyd_threeph_newton_tv_extrapolate(Xvar,param,dXdS,dpds)
    implicit none
    real, dimension(2*nc+7), intent(in) :: Xvar
    real, dimension(nc+2), intent(inout) :: param
    real, dimension(2*nc+7), intent(out) :: dXdS
    real, intent(out) :: dPdS
    ! Locals
    real :: Z(nc), vS
    real :: T,vY,vX,vW,Y(nc),X(nc),W(nc),Ky(nc),Kx(nc),beta,betaL,betaW
    real :: pW, dpWdv, dpWdt, dpWdn(nc), fac
    real :: dWdlnKy(nc), dWdlnKx(nc), dWdbeta(nc), dWdbetaW(nc)
    integer :: s, i
    real, dimension(2*nc+7,2*nc+7) :: Jac
    integer, dimension(2*nc+7) :: INDX
    integer :: INFO

    call hyd_threeph_diff_newton_tv(Jac,Xvar,param)
    Z = param(1:nc)
    call get_three_phase_variables_tv(Xvar,Z,T,pW,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW)

    pW = pressure(T,vW,W,dpdv=dpWdv,dpdt=dpWdt,dpdn=dpWdn)
    s = nint(param(nc+1))
    dXdS = 0
    if (s <= 2*nc+7) then
      dXdS(2*nc+3) = 1
    else
      ! Specified pressure
      vS = 3/(1/vY+1/vX+1/vW)
      fac = vS/(Rgas*T)
      dXdS(2*nc+3) = pW*fac
    endif

    ! Solve equation system
    call DGESV( 2*nc+7, 1, Jac, 2*nc+7, INDX, dXdS, 2*nc+7, INFO )

    do i=1,nc
      if (Z(i) > 0.0) then
        fac = -W(i)/(betaW + beta*Ky(i)+(1.0-beta-betaW)*Kx(i))
        dWdlnKX(i) = Kx(i)*(1.0-beta-betaW)*fac
        dWdlnKY(i) = Ky(i)*beta*fac
        dWdbeta(i) = (Ky(i)-Kx(i))*fac
        dWdbetaW(i) = (1-Kx(i))*fac
      else
        dWdlnKX(i) = 0
        dWdlnKY(i) = 0
        dWdbeta(i) = 0
        dWdbetaW(i) = 0
      endif
    enddo

    dpds = vW*dpWdv*dxds(2*nc+5) + T*dpWdt*dxds(2*nc+1) + &
         sum(dpWdn*dWdlnKx*dxds(1:nc)) + sum(dpWdn*dWdlnKy*dxds(nc+1:2*nc)) + &
         sum(dpWdn*dWdbeta*dxds(2*nc+2)) + sum(dpWdn*dWdbetaW*dxds(2*nc+6))

  end subroutine hyd_threeph_newton_tv_extrapolate

  !-----------------------------------------------------------------!
  function newton_three_phase_fluid_hydrate_curve(Xsol,param,T,P,ierr) result(iter)
  !-----------------------------------------------------------------!
    use nonlinear_solvers, only: nonlinear_solver,premReturn,setXv,&
         nonlinear_solve, test_differentials
    use utilities, only: isXwithinBounds
    implicit none
    real, intent(inout) :: Xsol(2*nc+7)
    real, intent(inout) :: param(nc+2)
    real, intent(out) :: T, P
    integer, intent(out) :: ierr
    integer :: iter
    ! Internal
    real :: Z(nc), b(nc)
    real :: Xmax(2*nc+7), Xmin(2*nc+7)
    type(nonlinear_solver) :: solver
    real :: vY,vX,vW,Y(nc),X(nc),W(nc),Ky(nc),Kx(nc),beta,betaL,betaW
    real :: param_ext(2*nc+2)
    ! Testing
    !call test_differentials(Xsol,param,hyd_twoph_fun_newton_tv,&
    !     hyd_twoph_diff_newton_tv)

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-9
    solver%max_it = 50
    solver%ls_max_it = 3

    Z = param(1:nc)
    call hydrate_var_threeph_tv_limits(Xsol,Xmin,Xmax,b)
    call isXwithinBounds(2*nc+7,Xsol,Xmin,Xmax,"",&
         "newton_three_phase_fluid_hydrate_curve: Initial values not within bounds!!")
    param_ext(1:nc+2) = param
    param_ext(nc+3:2*nc+2) = b
    call nonlinear_solve(solver,hyd_threeph_fun_newton_tv,hyd_threeph_diff_newton_tv,&
         hyd_threeph_diff_newton_tv,limit_threeph_dx_line,premReturn,&
         setXv,Xsol,Xmin,Xmax,param_ext)
    iter = solver%iter
    ierr = solver%exitflag

    if (ierr == 0) then
      call get_three_phase_variables_tv(Xsol,Z,T,p,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW)
    endif
  end function newton_three_phase_fluid_hydrate_curve

  !-----------------------------------------------------------------------------
  !> Limit change in x, dx, to keep x between extreme values:
  !> xmin <= x <= xmax
  !>
  !> \author MH, Dec 2021
  !-----------------------------------------------------------------------------
  subroutine limit_threeph_dx_line(n,xx,xxmin,xxmax,dxx,np,param)
    use nonlinear_solvers, only: limit_dx
    implicit none
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: xx,xxmin,xxmax
    real, dimension(n),     intent(inout) :: dxx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: param
    real :: scaling
    real :: vY,vX,vW,Y(nc),X(nc),W(nc),Ky(nc),Kx(nc),beta,betaL,betaW,T,P,z(nc)
    real :: b(nc),bY,bX,bW
    !
    call limit_dx(n,xx,xxmin,xxmax,dxx,np,param)
    ! Additional test for minimum volume?
    if (minval(abs(Xx(2*nc+3:2*nc+5)+dxx(2*nc+3:2*nc+5)-Xxmin(2*nc+3:2*nc+5))) < 0.05) then
      b = param(nc+3:2*nc+2)
      Z = param(1:nc)
      call get_three_phase_variables_tv(Xx+dxx,Z,T,p,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW,&
           no_press_calc=.true.)
      bY = sum(Y*b)
      bX = sum(X*b)
      bW = sum(W*b)
      scaling = 1.0
      do while (vY < bY .or. vX < bX .or. vW < bW)
        scaling = scaling*0.5
        call get_three_phase_variables_tv(Xx+scaling*dxx,&
             Z,T,p,vY,vX,vW,Y,X,W,Ky,Kx,beta,betaL,betaW,&
             no_press_calc=.true.)
        bY = sum(Y*b)
        bX = sum(X*b)
        bW = sum(W*b)
      enddo
      if (scaling < 1.0) then
        dxx = dxx * scaling
      endif
    endif
    ! Additional test for beta
    if (Xx(2*nc+2)+dxx(2*nc+2) < 0.0) then
      if (Xx(2*nc+2) == 0.0) then
        dxx(2*nc+2) = 0
      else
        scaling = -Xx(2*nc+2)/dxx(2*nc+2)
        dxx = dxx * scaling
      endif
    endif
    ! Additional test for betaW
    if (Xx(2*nc+6)+dxx(2*nc+6) < 0.0) then
      if (Xx(2*nc+6) == 0.0) then
        dxx(2*nc+6) = 0
      else
        scaling = -Xx(2*nc+6)/dxx(2*nc+6)
        dxx = dxx * scaling
      endif
    endif
    ! Additional test for betaL
    if (xx(2*nc+7) + dxx(2*nc+7) < 0.0) then
      if (Xx(2*nc+7) == 0.0) then
        dxx(2*nc+7) = 0
      else
        scaling =  xx(2*nc+7)/dxx(2*nc+7)
        dxx = dxx * scaling
      endif
    endif
  end subroutine limit_threeph_dx_line

  !-----------------------------------------------------------------------------
  !> Test variable sensitivities along the hydrate curve for two-phase fluid
  !>
  !> \author MH, 2024-7
  !-----------------------------------------------------------------------------
  subroutine hyd_threeph_newton_tv_extrapolate_test()
    implicit none
    ! Locals
    real, dimension(2*nc+7) :: Xvar1, Xvar2, Xvar0
    real, parameter, dimension(11) :: XvarInit = (/1.0,1.0,1.0,1.0,1.0,&
         1.0,1.0,1.0,1.0,1.0,1.0 /)
    character(len=15), dimension(12) :: var_names
    real, dimension(nc+2) :: param
    real, dimension(2*nc+7) :: dXdS
    real :: T, ln_s, dpds, p1, p2, ds, p
    integer :: s, ierr, iter
    real :: Z(nc)
    ! Debug
    !real :: v1,v2,X1(nc),X2(nc),K(nc),beta
    !real :: T_2,v1_2,v2_2,X1_2(nc),X2_2(nc),K_2(nc),beta_2

    var_names(1) = "lnKy1"
    var_names(2) = "lnKy2"
    var_names(3) = "lnKx1"
    var_names(4) = "lnKx2"
    var_names(5) = "ln(T)"
    var_names(6) = "beta"
    var_names(7) = "ln(v1)"
    var_names(8) = "ln(v2)"
    var_names(9) = "ln(v2)"
    var_names(10) = "betaW"
    var_names(11) = "betaL"
    var_names(12) = "dlnP"
    !call init_cubic("CO2,H2O","SRK")
    z = (/0.5,0.5/)
    param(1:nc) = Z
    Xvar0 = XvarInit
    s = 2*nc+6
    param(nc+1) = real(s)
    ln_s = Xvar0(s)
    param(nc+2) = ln_s
    iter = newton_three_phase_fluid_hydrate_curve(Xvar0,param,T,P,ierr)
    !call get_two_phase_variables_tv(Xvar0,Z,T,P,v1,v2,X1,X2,K,beta)
    !print *,"iter,ierr",iter,ierr
    ds = 1.0e-4

    ! Loop
    do s=1,nc+5
      param(nc+1) = real(s)
      call hyd_twoph_newton_tv_extrapolate(Xvar0,param,dXdS,dpds)
      if (s <= 2*nc+7) then
        param(nc+2) = Xvar0(s) - ds
      else
        param(nc+2) = log(p) - ds
      endif
      Xvar1 = Xvar0 - dXdS*ds
      iter = newton_three_phase_fluid_hydrate_curve(Xvar1,param,T,P1,ierr)
      !print *,"iter,ierr",iter,ierr
      !call get_two_phase_variables_tv(Xvar1,Z,T,P1,v1,v2,X1,X2,K,beta)
      if (s <= 2*nc+7) then
        param(nc+2) = Xvar0(s) + ds
      else
        param(nc+2) = log(p) + ds
      endif
      Xvar2 = Xvar0 + dXdS*ds
      iter = newton_three_phase_fluid_hydrate_curve(Xvar2,param,T,P2,ierr)
      !print *,"iter,ierr",iter,ierr
      !call get_two_phase_variables_tv(Xvar2,Z,T_2,P2,v1_2,v2_2,X1_2,X2_2,K_2,beta_2)
      print *,trim(var_names(s))//":"
      print *,(Xvar2-Xvar1)/(2*ds)
      print *,dXdS
      print *,"dpds:"
      print *,dpds,(p2-p1)/(2*ds)
    enddo
  end subroutine hyd_threeph_newton_tv_extrapolate_test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Methods for storage structure
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine multi_phase_point_alloc(tpp,nc)
    class(multi_phase_point), intent(inout) :: tpp
    integer, intent(in) :: nc
    !
    integer :: istat
    call tpp%dealloc()
    allocate(tpp%X1(nc), tpp%X2(nc), tpp%X3(nc), tpp%K1(nc), tpp%K2(nc), stat=istat)
    if (istat /= 0) call stoperror("Error allocating multi_phase_point")
  end subroutine multi_phase_point_alloc

  subroutine multi_phase_point_dealloc(tpp)
    class(multi_phase_point), intent(inout) :: tpp
    !
    integer :: istat
    istat = 0
    if (allocated(tpp%X1)) deallocate(tpp%X1, stat=istat)
    if (istat /= 0) call stoperror("Error deallocating multi_phase_point X1")
    if (allocated(tpp%X2)) deallocate(tpp%X2, stat=istat)
    if (istat /= 0) call stoperror("Error deallocating multi_phase_point X2")
    if (allocated(tpp%X3)) deallocate(tpp%X3, stat=istat)
    if (istat /= 0) call stoperror("Error deallocating multi_phase_point X3")
    if (allocated(tpp%K1)) deallocate(tpp%K1, stat=istat)
    if (istat /= 0) call stoperror("Error deallocating multi_phase_point K1")
    if (allocated(tpp%K2)) deallocate(tpp%K2, stat=istat)
    if (istat /= 0) call stoperror("Error deallocating multi_phase_point K2")
  end subroutine multi_phase_point_dealloc

  subroutine multi_phase_point_list_init(tppl,nmax,nc,Z)
    class(multi_phase_point_list), intent(inout) :: tppl
    integer, intent(in) :: nmax, nc
    real, intent(in) :: Z(nc)
    !
    integer :: istat, i
    call tppl%dealloc()
    allocate(tppl%list(nmax), stat=istat)
    if (istat /= 0) call stoperror("Error allocating multi_phase_point_list")
    allocate(tppl%z(nc), stat=istat)
    if (istat /= 0) call stoperror("Error allocating multi_phase_point_list Z")
    tppl%z = Z
    do i=1,size(tppl%list)
      call tppl%list(i)%alloc(nc)
    enddo
    tppl%n = 0
  end subroutine multi_phase_point_list_init

  subroutine multi_phase_point_list_dealloc(tppl)
    class(multi_phase_point_list), intent(inout) :: tppl
    !
    integer :: istat, i
    if (allocated(tppl%list)) then
      do i=1,size(tppl%list)
        call tppl%list(i)%dealloc()
      enddo
      deallocate(tppl%list, stat=istat)
      if (istat /= 0) call stoperror("Error deallocating multi_phase_point_list")
    endif
    if (allocated(tppl%Z)) then
      deallocate(tppl%Z, stat=istat)
      if (istat /= 0) call stoperror("Error deallocating multi_phase_point_list Z")
    endif
  end subroutine multi_phase_point_list_dealloc

  subroutine multi_phase_point_list_add_point_twoph(tppl,Xvar)
    class(multi_phase_point_list), intent(inout) :: tppl
    real, intent(in) :: Xvar(nc+4)
    !
    tppl%n = tppl%n + 1
    call get_two_phase_variables_tv(Xvar,tppl%Z,&
         tppl%list(tppl%n)%T,&
         tppl%list(tppl%n)%P,&
         tppl%list(tppl%n)%v1,&
         tppl%list(tppl%n)%v2,&
         tppl%list(tppl%n)%X1,&
         tppl%list(tppl%n)%X2,&
         tppl%list(tppl%n)%K1,&
         tppl%list(tppl%n)%beta1)
    tppl%list(tppl%n)%nph = 2
    tppl%list(tppl%n)%v3 = 0
    tppl%list(tppl%n)%X3 = 0
    tppl%list(tppl%n)%K2 = 1
    tppl%list(tppl%n)%beta3 = 0
    if (.false.) then
      print *,"T",tppl%list(tppl%n)%T
      print *,"P",tppl%list(tppl%n)%P
      print *,"v1",tppl%list(tppl%n)%v1
      print *,"v2",tppl%list(tppl%n)%v2
      print *,"X1",tppl%list(tppl%n)%X1
      print *,"X2",tppl%list(tppl%n)%X2
      print *,"K1",tppl%list(tppl%n)%K1
      print *,"beta1",tppl%list(tppl%n)%beta1
    endif
  end subroutine multi_phase_point_list_add_point_twoph

  subroutine multi_phase_point_list_add_point_threeph(tppl,Xvar)
    class(multi_phase_point_list), intent(inout) :: tppl
    real, intent(in) :: Xvar(2*nc+7)
    !
    tppl%n = tppl%n + 1
    call get_three_phase_variables_tv(Xvar,tppl%Z,&
         tppl%list(tppl%n)%T,&
         tppl%list(tppl%n)%P,&
         tppl%list(tppl%n)%v1,&
         tppl%list(tppl%n)%v2,&
         tppl%list(tppl%n)%v3,&
         tppl%list(tppl%n)%X1,&
         tppl%list(tppl%n)%X2,&
         tppl%list(tppl%n)%X3,&
         tppl%list(tppl%n)%K1,&
         tppl%list(tppl%n)%K2,&
         tppl%list(tppl%n)%beta1,&
         tppl%list(tppl%n)%beta2,&
         tppl%list(tppl%n)%beta3)
    tppl%list(tppl%n)%nph = 3
    if (.false.) then
      print *,"T",tppl%list(tppl%n)%T
      print *,"P",tppl%list(tppl%n)%P
      print *,"v1",tppl%list(tppl%n)%v1
      print *,"v2",tppl%list(tppl%n)%v2
      print *,"v3",tppl%list(tppl%n)%v3
      print *,"X1",tppl%list(tppl%n)%X1
      print *,"X2",tppl%list(tppl%n)%X2
      print *,"X3",tppl%list(tppl%n)%X3
      print *,"K1",tppl%list(tppl%n)%K1
      print *,"K2",tppl%list(tppl%n)%K2
      print *,"beta1",tppl%list(tppl%n)%beta1
      print *,"beta2",tppl%list(tppl%n)%beta2
      print *,"beta3",tppl%list(tppl%n)%beta3
    endif
  end subroutine multi_phase_point_list_add_point_threeph

  subroutine multi_phase_point_list_add_single_phase_curve(tppl,T,v,n,inverted)
    class(multi_phase_point_list), intent(inout) :: tppl
    integer, intent(in) :: n
    real, intent(in) :: T(n), v(n)
    logical, intent(in) :: inverted
    !
    integer :: i, imin, imax, step
    if (inverted) then
      imin = n
      imax = 1
      step = -1
    else
      imin = 1
      imax = n
      step = 1
    endif
    do i=imin,imax,step
      tppl%n = tppl%n + 1
      tppl%list(tppl%n)%T = T(i)
      tppl%list(tppl%n)%v1 = v(i)
      tppl%list(tppl%n)%P = pressure(T(i),v(i),tppl%Z)
      tppl%list(tppl%n)%v2 = 0
      tppl%list(tppl%n)%X1 = tppl%Z
      tppl%list(tppl%n)%X2 = 0
      tppl%list(tppl%n)%X3 = 0
      tppl%list(tppl%n)%K1 = 1
      tppl%list(tppl%n)%K2 = 1
      tppl%list(tppl%n)%beta1 = 1
      tppl%list(tppl%n)%beta3 = 1
      tppl%list(tppl%n)%nph = 1
    enddo
  end subroutine multi_phase_point_list_add_single_phase_curve

  subroutine multi_phase_point_list_add_tppl(tppl,tppl_to_add,inverted)
    class(multi_phase_point_list), intent(inout) :: tppl
    class(multi_phase_point_list), intent(in) :: tppl_to_add
    logical, intent(in) :: inverted
    !
    integer :: i, imin, imax, step
    if (inverted) then
      imin = tppl_to_add%n
      imax = 1
      step = -1
    else
      imin = 1
      imax = tppl_to_add%n
      step = 1
    endif
    do i=imin,imax,step
      tppl%n = tppl%n + 1
      tppl%list(tppl%n)%T = tppl_to_add%list(i)%T
      tppl%list(tppl%n)%v1 = tppl_to_add%list(i)%v1
      tppl%list(tppl%n)%P = tppl_to_add%list(i)%P
      tppl%list(tppl%n)%v2 = tppl_to_add%list(i)%v2
      tppl%list(tppl%n)%v3 = tppl_to_add%list(i)%v3
      tppl%list(tppl%n)%X1 = tppl_to_add%list(i)%X1
      tppl%list(tppl%n)%X2 = tppl_to_add%list(i)%X2
      tppl%list(tppl%n)%X3 = tppl_to_add%list(i)%X3
      tppl%list(tppl%n)%K1 = tppl_to_add%list(i)%K1
      tppl%list(tppl%n)%K2 = tppl_to_add%list(i)%K2
      tppl%list(tppl%n)%beta1 = tppl_to_add%list(i)%beta1
      tppl%list(tppl%n)%beta3 = tppl_to_add%list(i)%beta3
      tppl%list(tppl%n)%nph = tppl_to_add%list(i)%nph
    enddo
  end subroutine multi_phase_point_list_add_tppl

  subroutine multi_phase_point_list_write_file(tppl,filename)
    use utilities, only: newunit
    class(multi_phase_point_list), intent(in) :: tppl
    character(len=*), intent(in) :: filename
    !
    integer :: i, i_unit
    i_unit = newunit()
    open(unit=i_unit,file=filename)
    do i=1,tppl%n
      write(i_unit,*) tppl%list(i)%T, tppl%list(i)%P
    enddo
    close(i_unit)
  end subroutine multi_phase_point_list_write_file

  subroutine multi_phase_point_list_write_arrays(tppl,nmax,n,T,P)
    class(multi_phase_point_list), intent(in) :: tppl
    integer, intent(in) :: nmax
    integer, intent(out) :: n
    real, intent(out) :: T(nmax), P(nmax)
    !
    integer :: i
    n = min(tppl%n, nmax)
    do i=1,n
      T(i) = tppl%list(i)%T
      P(i) = tppl%list(i)%P
    enddo
  end subroutine multi_phase_point_list_write_arrays

  subroutine multi_phase_point_list_print_arrays(tppl)
    class(multi_phase_point_list), intent(in) :: tppl
    !
    integer :: i
    do i=1,tppl%n
      print *,tppl%list(i)%T, tppl%list(i)%P, tppl%list(i)%beta1, tppl%list(i)%beta2, tppl%list(i)%beta3
    enddo
  end subroutine multi_phase_point_list_print_arrays

  subroutine multi_phase_point_list_reset(tppl)
    class(multi_phase_point_list), intent(inout) :: tppl
    !
    tppl%list(:)%T = 0
    tppl%list(:)%P = 0
    tppl%list(:)%beta1 = 0
    tppl%list(:)%beta2 = 0
    tppl%list(:)%beta3 = 0
    tppl%list(:)%v1 = 0
    tppl%list(:)%v2 = 0
    tppl%list(:)%v3 = 0
    ! tppl%list(:)%X1 = 0
    ! tppl%list(:)%X2 = 0
    ! tppl%list(:)%X3 = 0
    ! tppl%list(:)%K1 = 0
    ! tppl%list(:)%K2 = 0
    tppl%list(:)%nph = 0
    tppl%n = 0
  end subroutine multi_phase_point_list_reset

end module hydrate_curves
