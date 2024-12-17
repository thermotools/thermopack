module saturation_tv
  use eostv, only: thermo_tv,pressure
  use thermopack_constants, only: clen, LIQPH, VAPPH, verbose
  use thermopack_var, only: nc, thermo_model, get_active_eos, &
       get_active_thermo_model, Rgas, tpTmax, tpTmin
  use nonlinear_solvers
  use numconstants, only: machine_prec, Small
  use saturation, only: specP, specT, sat_limitEps, sat_max_iter, &
       sat_max_nr_line_s, ispec, sat_newton, sat_wilsonK, sat_successive
  use saturation_curve, only: envelope_plot_control, ER_OK, &
       ER_NOINITIALPOINT, ER_PMAX_TMIN, ER_STABILITY, ER_NMAX, &
       ER_NOSOL, ER_TRIPLE
  use puresaturation, only: puresat
  use cubic_eos, only: get_b_linear_mix
  implicit none
  private
  save

  logical, parameter :: debug_tv = .false.
  integer, parameter :: specV1 = 3
  integer, parameter :: specV2 = 4

  type, extends(envelope_plot_control) :: envelope_plot_control_tv
    real :: vW
  contains
    procedure, public :: envelope_plot => envelope_plot_tv_epc
  end type envelope_plot_control_tv

  public :: specV1, specV2, ispec_tv
  public :: sat_newton_tv, sat_fun_newton_tv, sat_diff_newton_tv
  public :: newton_tv_extrapolate_test
  public :: envelope_plot_tv, envelope_plot_control_tv
  public :: get_step_direction_tv, debug_tv
  public :: set_variables_tv, newton_tv_extrapolate
  public :: sat_newton_tv_x, get_variables_tv
  public :: print_envelope_point_tv

contains

  !-----------------------------------------------------------------------------
  !> Solve for saturation line using NR solver.
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  function sat_newton_tv(Z,K,t,v1,v2,beta1,s,ln_s,p,ierr) result(iter)
    implicit none
    real, dimension(nc), intent(in) :: Z    ! total composition
    real, dimension(nc), intent(inout) :: K ! equilibrium factors; K_i = y_i/x_i
    real, intent(inout) :: t                ! temperature [K]
    real, intent(inout) :: v1,v2            ! molar volume [m3/mol]
    real, intent(in) :: beta1               ! phase 1 quality
    real, intent(in) :: ln_s                ! logarithm of the fixed variable
    integer, intent(in) :: s                ! index of fixed variable in X
    real, intent(out) :: p                  ! pressure [Pa]
    integer, intent(out) :: ierr            ! error flag
    integer :: iter
    ! Locals
    real :: X(nc+3), param(nc+3)
    X(1:nc) = log(K)
    X(nc+1) = log(t)
    X(nc+2) = log(v1)
    X(nc+3) = log(v2)

    param(1:nc) = Z
    param(nc+1) = beta1
    param(nc+2) = real(s) ! typecast since param can only contain reals..
    param(nc+3) = ln_s

    iter = sat_newton_tv_x(X,param,p,ierr)

    if (ierr == 0) then
      K = exp(X(1:nc))
      t = exp(X(nc+1))
      v1 = exp(X(nc+2))
      v2 = exp(X(nc+3))
      if (verbose) then
        print *,'sat_newton_tv: converged after ', iter, ' number of iterations'
      end if
    endif

  end function sat_newton_tv

  !-----------------------------------------------------------------------------
  !> Solve for saturation line using NR solver.
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  function sat_newton_tv_x(X,param,p,ierr) result(iter)
    use utilities, only: isXwithinBounds
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(nc+3), intent(inout) :: X    !
    real, dimension(nc+3), intent(inout) :: param    !
    real, intent(out) :: p                  ! pressure [Pa]
    integer, intent(out) :: ierr            ! error flag
    integer :: iter
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(nc+3) :: xmax, xmin
    real, dimension(nc) :: K, Z, X1
    real :: beta1, t, v1
    !
    !Testing
    ! integer :: i
    ! real :: eps
    ! real, dimension(nc+3) :: XX1, G0, G1, G2
    ! real, dimension(nc+3,nc+3) :: J0


    ! !*********************************************
    ! eps = 1.0e-5
    ! param(nc+2) = real(nc + 4)
    ! call sat_fun_newton_tv(G0,X,param)
    ! print *,G0
    ! !stop
    ! call sat_diff_newton_tv(J0,X,param)
    ! do i=1,nc+3
    !   XX1 = X
    !   XX1(i) = X(i) - eps
    !   call sat_fun_newton_tv(G1,XX1,param)
    !   XX1(i) = X(i) + eps
    !   call sat_fun_newton_tv(G2,XX1,param)
    !   print *,"i",i
    !   print *,(G2-G1)/eps/2
    !   print *,J0(:,i)
    ! enddo
    ! stop
    ! ! *********************************************

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-9
    solver%limit_x_values = .false.
    solver%max_it = sat_max_iter
    solver%ls_max_it = sat_max_nr_line_s

    Z = param(1:nc)
    call sat_var_tv_limits(param,X,Xmin,Xmax)
    call isXwithinBounds(nc+3,X,Xmin,Xmax,"",&
         "sat_newton_tv_x: Initial values not within bounds!!")
    call nonlinear_solve(solver,sat_fun_newton_tv,sat_diff_newton_tv,&
         sat_diff_newton_tv,limit_dx,premReturn,setXv,X,Xmin,Xmax,param)
    iter = solver%iter
    ierr = solver%exitflag

    ! print *,"X",X
    ! print *,"Xmin",Xmin
    ! print *,"Xmax",Xmax
    if (sat_var_tv_at_limit(param,X)) ierr = -1
    if (ierr == 0) then
      K = exp(X(1:nc))
      if (.not. isSingleComp(Z)) then
        if (maxval(abs(K-1))<1e-8) ierr = -2
      endif
      Z = param(1:nc)
      beta1 = param(nc+1)
      X1 = K*Z/(1-beta1+beta1*K)
      t = exp(X(nc+1))
      v1 = exp(X(nc+2))
      p = pressure(t,v1,X1)
    endif
  end function sat_newton_tv_x

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  subroutine sat_fun_newton_tv(G,Xvar,param)
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(nc+3), intent(out) :: G !< Function values
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+3) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: K, lnfug1, lnfug2, X1, X2
    integer :: s, i
    logical :: is_single
    real :: v1, v2, t,ln_s, beta1, p, p1, p2
    real :: vS

    Z = param(1:nc)
    beta1 = param(nc+1)
    s = nint(param(nc+2))
    ln_s = param(nc+3)
    is_single  = isSingleComp(Z)

    if (sat_var_tv_at_limit(param,Xvar)) then
      G = 0.0 ! Terminate solver
      return
    endif
    call get_variables_tv(Xvar,param,T,P,v1,v2,X1,X2,K)
    if (.not. is_single) then
      if (maxval(abs(K-1))<1e-8) then
        ! Converging to trivial solution
        G = 0.0 ! Terminate solver
        return
      endif
    endif
    ! Volume average for pressure error scaling
    vS = 2/(1/v1+1/v2)

    call thermo_tv(t,v1,X1,lnfug1)
    call thermo_tv(t,v2,X2,lnfug2)
    p1 = pressure(t,v1,X1)
    p2 = pressure(t,v2,X2)
    ! Function value
    do i=1,nc
      if (Z(i) > 0.0) then
        G(i) = lnfug1(i) - lnfug2(i)
      else
        G(i) = 0.0
      endif
    enddo
    if (is_single) then
      G(nc+1) = 0.0
    else
      G(nc+1) = sum(X1-X2)
    endif
    if (s <= nc+3) then
      G(nc+2) = Xvar(s) - ln_s
    else if (s == nc+4) then
      ! Pressure
      p = exp(ln_s)
      G(nc+2) = (p1-p)*vS/(Rgas*T)
    endif
    G(nc+3) = (p1-p2)*vS/(Rgas*T)

    !print *,"G",G
  end subroutine sat_fun_newton_tv

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along the saturation line
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine newton_tv_extrapolate(Xvar,param,dXdS,dpds)
    implicit none
    real, dimension(nc+3), intent(in) :: Xvar
    real, dimension(nc+3), intent(in) :: param
    real, dimension(nc+3), intent(out) :: dXdS
    real, intent(out) :: dPdS
    ! Locals
    real :: p, T, v1, v2, X1(nc), X2(nc), K(nc), Z(nc)
    real :: dpdv, dpdt, dpdn(nc), dndlnK(nc), beta, vS
    integer :: s
    real, dimension(nc+3,nc+3) :: Jac
    integer, dimension(nc+3) :: INDX
    integer :: INFO, j

    call sat_diff_newton_tv(Jac,Xvar,param)
    call get_variables_tv(Xvar,param,T,P,v1,v2,X1,X2,K)
    s = nint(param(nc+2))
    dXdS = 0
    if (s <= nc+3) then
      dXdS(nc+2) = 1.0
    else
      ! Specified pressure
      ! Volume average for pressure error scaling
      vS = 2/(1/v1+1/v2)
      ! dlnP
      dXdS(nc+2) = p*vS/(Rgas*T)
    endif

    ! Solve equation system
    call DGESV( nc+3, 1, Jac, nc+3, INDX, dXdS, nc+3, INFO )

    p = pressure(T,v1,X1,dpdv=dpdv,dpdt=dpdt,dpdn=dpdn)
    Z = param(1:nc)
    beta = param(nc+1)
    do j=1,nc
      if (Z(j) > 0.0) then
        dndlnK(j) = (1.0-beta)*X2(j)*X1(j)/Z(j)
      else
        dndlnK(j) = 0.0
      endif
    enddo
    dpds = v1*dpdv*dxds(nc+2) + T*dpdt*dxds(nc+1) + sum(dpdn*dndlnK*dxds(1:nc))

  end subroutine newton_tv_extrapolate

  !-----------------------------------------------------------------------------
  !> Test variable sensitivities along the saturation line
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine newton_tv_extrapolate_test()
    use saturation, only: safe_dewT
    use eos, only: specificvolume
    implicit none
    ! Locals
    real, dimension(nc+3) :: Xvar, Xvar1
    real, dimension(nc+3) :: param, param1
    real, dimension(nc+3) :: dXdS
    real :: p, T, v1, v2, beta1, ln_s, dpds, p1
    integer :: s, ierr, iter
    real :: Y(nc), X(nc), Z(nc), K(nc), xx

    !call init_cubic("CO2,N2,C1","SRK")
    !z = (/0.85,0.1,0.05/)
    ! p = 5.0e5
    ! X = Z
    ! Y = Z
    ! T = safe_dewT(P,X,Y,ierr)
    ! K = Y/X
    ! call specificvolume(T,P,x,LIQPH,v2)
    ! call specificvolume(T,P,y,VAPPH,v1)
    ! beta1 = 1

    !call init_cubic("CO2,H2O","SRK")
    xx = 30.0e-6
    z = (/1.0-xx,xx/)
    p = 1.0e4
    X = Z
    Y = Z
    T = safe_dewT(P,X,Y,ierr)
    K = Y/X
    call specificvolume(T,P,x,LIQPH,v2)
    call specificvolume(T,P,y,VAPPH,v1)
    beta1 = 1

    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(v1)
    Xvar(nc+3) = log(v2)

    s = nc+4
    ln_s = log(p)
    param(1:nc) = Z
    param(nc+1) = beta1
    param(nc+2) = real(s)
    param(nc+3) = ln_s

    iter = sat_newton_tv_x(Xvar,param,p,ierr)
    call newton_tv_extrapolate(Xvar,param,dXdS,dpds)

    ln_s = log(p+1.0)
    param1 = param
    param1(nc+3) = ln_s
    Xvar1 = Xvar
    iter = sat_newton_tv_x(Xvar1,param1,p,ierr)

    print *,"Pressure:"
    print *,(Xvar1-Xvar)/(param1(nc+3)-param(nc+3))
    print *,dXdS

    s = nc + 1
    ln_s = Xvar(s)
    param(nc+2) = real(s)
    param(nc+3) = ln_s
    iter = sat_newton_tv_x(Xvar,param,p,ierr)
    call newton_tv_extrapolate(Xvar,param,dXdS,dpds)

    param1 = param
    param1(nc+3) = ln_s + 1.0e-3
    Xvar1 = Xvar
    iter = sat_newton_tv_x(Xvar1,param1,p1,ierr)

    print *,"Temperature:"
    print *,(Xvar1-Xvar)/(param1(nc+3)-param(nc+3))
    print *,dXdS

    print *,"dpds:"
    print *,dpds,(p1-p)/(param1(nc+3)-param(nc+3))

  end subroutine newton_tv_extrapolate_test

  !> Saturation variable limits
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine sat_var_tv_limits(param,X,Xmin,Xmax)
    use numconstants, only: expMax, expMin
    use eosdata, only: eosCPA
    use volume_shift, only: get_c_mix
    implicit none
    real, dimension(nc+3), intent(in) :: X
    real, dimension(nc+3), intent(in) :: param
    real, dimension(nc+3), intent(out) :: Xmin, Xmax !< Variable vector
    ! Locals
    real :: b1,b2
    real :: T,P,v1,v2,X1(nc),X2(nc),K(nc)
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    Xmin = expMin
    Xmax = expMax
    Xmin(nc+1) = log(tpTmin) !Tmin
    Xmax(nc+1) = log(tpTmax) !Tmax
    !
    call get_variables_tv(X,param,T,P,v1,v2,X1,X2,K)
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      b1 = 1.0e-7
      b2 = 1.0e-7
    else
      b1 = get_b_linear_mix(X1) + get_c_mix(T,X1) + Small ! m3/mol
      b2 = get_b_linear_mix(X2) + get_c_mix(T,X2) + Small ! m3/mol
    endif
    Xmin(nc+2) = log(b1) !v min
    Xmin(nc+3) = log(b2) !v min
    Xmax(nc+2:nc+3) = log(100.0) !v max

  end subroutine sat_var_tv_limits

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  function sat_var_tv_at_limit(param,Xvar,eps) result(atLimit)
    implicit none
    real, dimension(nc+3), intent(in) :: param
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, optional, intent(in) :: eps !< Safety margin
    logical :: atLimit
    ! Locals
    integer :: i
    real :: eps_local
    real :: lnMin(nc+3), lnMax(nc+3)
    atLimit = .false.
    call sat_var_tv_limits(param,Xvar,lnMin,lnMax)
    eps_local = sat_limitEps
    if (present(eps)) then
      eps_local = eps
    endif
    do i=1,nc+3
      if ( Xvar(i) >= lnMax(i) - eps_local .or. &
           Xvar(i) <= lnMin(i) + eps_local) then
        atLimit = .true.
      endif
    enddo
  end function sat_var_tv_at_limit

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine sat_diff_newton_tv(Jac,Xvar,param)
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+3,nc+3), intent(out) :: Jac !< Function differentials
    real, dimension(nc+3) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: K, lnphi1, lnphi1T, lnphi1V, lnphi2, lnphi2T, lnphi2V, X1, X2
    real, dimension(nc,nc) :: lnphi1n, lnphi2n
    real :: dp1dv,dp1dt,dp2dv,dp2dt
    real, dimension(nc) :: dp1dn, dp2dn
    integer :: s, i, j
    logical :: is_single
    real :: v1, v2, t, beta1, fac, p, p1, p2
    real :: vS, dvSdv1_div_vS, dvSdv2_div_vS

    Z = param(1:nc)
    beta1 = param(nc+1)
    s = nint(param(nc+2))
    is_single  = isSingleComp(Z)

    call get_variables_tv(Xvar,param,T,P,v1,v2,X1,X2,K)
    ! Volume average for pressure error scaling
    vS = 2/(1/v1+1/v2)
    if (debug_tv) dvSdv1_div_vS = 1/(1/v1 + 1/v2)/v1**2
    if (debug_tv) dvSdv2_div_vS = 1/(1/v1 + 1/v2)/v2**2
    fac = vS/(Rgas*T)

    call thermo_tv(t,v2,X2,lnphi2,lnphit=lnphi2t,lnphiv=lnphi2v,lnphin=lnphi2n)
    call thermo_tv(t,v1,X1,lnphi1,lnphit=lnphi1t,lnphiv=lnphi1V,lnphin=lnphi1n)
    p2 = pressure(t,v2,X2,dpdv=dp2dv,dpdt=dp2dt,dpdn=dp2dn)
    p1 = pressure(t,v1,X1,dpdv=dp1dv,dpdt=dp1dt,dpdn=dp1dn)

    ! Temperature differential
    Jac(1:nc,nc+1) = t*(lnphi1T-lnphi2T)

    ! Volume differential
    Jac(1:nc,nc+2) = v1*lnphi1V
    Jac(1:nc,nc+3) = -v2*lnphi2V

    ! K differentials
    do i=1,nc
      if (Z(i) > 0.0) then
        do j=1,nc
          if (Z(j) > 0.0) then
            Jac(i,j) = ((1.0-beta1)*lnphi1n(i,j)+beta1*lnphi2n(i,j))*X2(j)*X1(j)/Z(j)
          else
            Jac(i,j) = 0.0
          endif
        enddo
      else
        Jac(i,:) = 0.0
      endif
    enddo

    ! nc+1 row
    if (is_single) then
      Jac(nc+1,:) = 0
      i = maxloc(Z,dim=1)
      Jac(nc+1,i) = 1
    else
      do i=1,nc
        if (Z(i) > 0.0) then
          Jac(nc+1,i) = X2(i)*X1(i)/Z(i)
        else
          Jac(nc+1,i) = 0.0
        endif
      enddo
      Jac(nc+1,nc+1:nc+3) = 0
    endif

    ! Specification row
    if (s <= nc+3) then
      Jac(nc+2,:) = 0.0
      Jac(nc+2,s) = 1.0
    else if (s == nc+4) then
      ! Pressure
      p = exp(param(nc+3))
      ! K differentials
      Jac(nc+2,nc+1:nc+3) = 0
      do i=1,nc
        if (Z(i) > 0.0) then
          Jac(nc+2,i) = fac*((1.0-beta1)*dp1dn(i))*X2(i)*X1(i)/Z(i)
        else
          Jac(nc+2,i) = 0
        endif
      enddo
      Jac(nc+2,nc+1) = T*dp1dT*fac
      if (debug_tv) Jac(nc+2,nc+1) = Jac(nc+2,nc+1) - (p1-p)*fac
      Jac(nc+2,nc+2) = dp1dv*v1*fac
      if (debug_tv) Jac(nc+2,nc+2) = Jac(nc+2,nc+2) + v1*dvSdv1_div_vS*(p1-p)*fac
      Jac(nc+2,nc+3) = 0
      if (debug_tv) Jac(nc+2,nc+3) = Jac(nc+2,nc+3) + v2*dvSdv2_div_vS*(p1-p)*fac
    endif

    ! Bottom row - pressure
    ! K differentials
    Jac(nc+3,nc+1:nc+3) = 0
    do i=1,nc
      if (Z(i) > 0.0) then
        Jac(nc+3,i) = fac*((1.0-beta1)*dp1dn(i)+beta1*dp2dn(i))*X2(i)*X1(i)/Z(i)
      else
        Jac(nc+3,i) = 0
      endif
    enddo

    Jac(nc+3,nc+1) = T*(dp1dT-dp2dT)*fac
    if (debug_tv) Jac(nc+3,nc+1) = Jac(nc+3,nc+1) - (p1-p2)*fac
    Jac(nc+3,nc+2) = dp1dv*v1*fac
    if (debug_tv) Jac(nc+3,nc+2) = Jac(nc+3,nc+2) + v1*dvSdv1_div_vS*(p1-p2)*fac
    Jac(nc+3,nc+3) = -dp2dv*v2*fac
    if (debug_tv) Jac(nc+3,nc+3) = Jac(nc+3,nc+3) + v2*dvSdv2_div_vS*(p1-p2)*fac

  end subroutine sat_diff_newton_tv

  !-----------------------------------------------------------------------------
  !> Get variables from Xvar
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine get_variables_tv(Xvar,param,T,P,v1,v2,X1,X2,K,no_press_calc)
    implicit none
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+3), intent(in) :: param !< Parameter vector
    logical, optional, intent(in) :: no_press_calc
    real, dimension(nc), intent(out) :: K, X1, X2
    real, intent(out) :: v1, v2, t, p
    ! Locals
    real, dimension(nc) :: Z
    real :: beta1
    logical :: no_press_calc_local
    !
    Z = param(1:nc)
    beta1 = param(nc+1)
    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    v1 = exp(Xvar(nc+2))
    v2 = exp(Xvar(nc+3))
    X2 = Z/(1-beta1+beta1*K)
    X1 = K*Z/(1-beta1+beta1*K)
    if (present(no_press_calc)) then
      no_press_calc_local = no_press_calc
    else
      no_press_calc_local = .false.
    endif
    if (no_press_calc_local) then
      p = 0
    else
      p = pressure(t,v1,X1)
    endif
  end subroutine get_variables_tv

  !-----------------------------------------------------------------------------
  !> Set variables and param
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine set_variables_tv(T,v1,v2,X1,X2,Z,beta1,Xvar,param)
    implicit none
    real, dimension(nc), intent(in) :: X1, X2, Z
    real, intent(in) :: v1, v2, t
    real, dimension(nc+3), intent(out) :: Xvar !< Variable vector
    real, dimension(nc+3), intent(out) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: K
    real :: beta1
    integer :: i
    !
    param(1:nc) = Z
    param(nc+1) = beta1
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(v1)
    Xvar(nc+3) = log(v2)
    do i=1,nc
      if (Z(i) > 0.0) then
        K(i) = X1(i)/X2(i)
      else
        K(i) = 1
      endif
    enddo
    Xvar(1:nc) = log(K)
  end subroutine set_variables_tv

  !-----------------------------------------------------------------------------
  !> Get specification for saturation point solvers
  !> specification = 1 ! 1:P, 2:T, 3:V1, 4:V2, 5:nc+4:ln K_i
  !> \author MH, 2011
  !-----------------------------------------------------------------------------
  function ispec_tv(specification) result(s)
    implicit none
    integer, intent(in) :: specification
    integer :: s
    select case(specification)
    case(specP) ! Pressure
      s = nc + 4
    case(specV1) ! Volume 1
      s = nc + 2
    case(specV2) ! Volume 2
      s = nc + 3
    case(specT) ! Temperature
      s = nc + 1
    case default ! ln K
      s = specification-4
    end select
  end function ispec_tv

  !-----------------------------------------------------------------------------
  !> Plot saturation curve in TP space - interface without type epc
  !>
  !> \author MH, 2020-11
  !-----------------------------------------------------------------------------
  subroutine envelope_plot_tv(Z,T_init,p_init,spec,beta_in,Pmax,nmax,&
       Ta,Pa,v1a,v2a,Ka,betaa,n,dS_override,Tme)
    implicit none
    ! Input:
    real,           intent(in)  :: Z(nc)       ! Total molar comp. (-)
    real,           intent(in)  :: T_init      ! T-guess initial point (K)
    real,           intent(in)  :: p_init      ! p-guess initial point (Pa)
    integer,        intent(in)  :: spec        ! 1: Specify P, T Varies
    ! 2: Specify T: P varies
    real,           intent(in)  :: beta_in     ! Initial beta (bub or dew)
    real,           intent(in)  :: Pmax        ! Maximum pressure (Pa)
    integer,        intent(in)  :: nmax        ! Maximum number of points
    ! Output:
    real,           intent(out) :: Ta(nmax)    ! Sat. temp. values (K)
    real,           intent(out) :: pa(nmax)    ! Sat. pres. values (Pa)
    real,           intent(out) :: v1a(nmax)   ! Sat. vol. values (m3/mol)
    real,           intent(out) :: v2a(nmax)   ! Sat. vol. values (m3/mol)
    real,           intent(out) :: Ka(nmax,nc) ! Equilibrium constants
    real,           intent(out) :: betaa(nmax) ! Phase fraction
    integer,        intent(out) :: n           ! Number of points found
    real, optional, intent(in)  :: dS_override ! Override step length
    real, optional, intent(in)  :: Tme ! Exit on temperature
    ! Locals
    type(envelope_plot_control_tv) :: epc
    ! Map inputs to epc type
    epc%spec = spec
    epc%beta_in = beta_in
    epc%Pmax = Pmax
    !
    if (present(dS_override)) call epc%set_ds_override(dS_override)
    !
    if (present(Tme)) call epc%set_minimum_temperature(Tme)
    !
    call epc%envelope_plot(Z,T_init,p_init,nmax,Ta,Pa,v1a,v2a,Ka,betaa,n)
    !
    if (epc%returnCause /= ER_OK .and. verbose) print *,trim(epc%get_return_cause_txt())
  end subroutine envelope_plot_tv

  !-----------------------------------------------------------------------------
  !> Plot saturation curve in TP space
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine envelope_plot_tv_epc(epc,Z,T_init,p_init,nmax,Ta,Pa,v1a,v2a,Ka,betaa,n)
    use eos, only: thermo, specificvolume
    use thermo_utils, only: isSingleComp
    use critical, only: calcStabMinEigTV
    implicit none
    class(envelope_plot_control_tv), intent(inout) :: epc
    ! Input:
    real,           intent(in)  :: Z(nc)       ! Total molar comp. (-)
    real,           intent(in)  :: T_init      ! T-guess initial point (K)
    real,           intent(in)  :: p_init      ! p-guess initial point (Pa)
    integer,        intent(in)  :: nmax        ! Maximum number of points
    ! Output:
    real,           intent(out) :: Ta(nmax)    ! Sat. temp. values (K)
    real,           intent(out) :: pa(nmax)    ! Sat. pres. values (Pa)
    real,           intent(out) :: v1a(nmax)   ! Sat. vol. values (m3/mol)
    real,           intent(out) :: v2a(nmax)   ! Sat. vol. values (m3/mol)
    real,           intent(out) :: Ka(nmax,nc) ! Equilibrium constants
    real,           intent(out) :: betaa(nmax) ! Phase fraction
    integer,        intent(out) :: n           ! Number of points found
    ! Internal:
    real  :: T,p
    real, dimension(nc) :: K, lnfugG, lnfugL, X1, X2
    real, dimension(nc+3) :: dXdS, dXdSold
    real, dimension(nc+3) :: Xvar ,Xold, param
    real :: dS, tuning, sgn, Pstart, Tmin, Tmax, dP
    integer :: iter,s,return_iter,ierr
    integer :: smax
    real :: ln_spec,beta
    logical :: doBub
    logical :: excessive_T_jump, excessive_P_jump
    !logical :: have_switched_formulation
    !logical :: should_switch_formulation
    logical :: recalculate
    logical :: exit_after_saving
    real, parameter :: maxdT = 25.0, maxdP = 15.0
    real :: dS_max, dS_min, v1, v2, p_old, dpds
    integer :: Yphase
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    ! Set initial guess for first point
    T = T_init
    p = p_init

    ! have_switched_formulation = epc%do_not_switch_formulation
    ! should_switch_formulation = .false.
    recalculate = .false.
    exit_after_saving = .false.

    epc%criconden = 0
    if (epc%calc_cricond) then
      epc%calc_cricond = .false.
      print *,"envelope_plot_tv_epc: Calculation of cricondenbar not supported"
    endif

    if (epc%calc_critical) then
      print *,"envelope_plot_tv_epc: Calculation of critical point not supported"
      epc%crit = 0.0
      epc%calc_critical = .false.
    endif

    n = 0
    beta = epc%beta_in

    if (epc%override_ds) then
      dS_max = epc%dS_override
      dS_min = min(0.25*epc%dS_override, 0.03)
    else
      dS_max = 0.25
      dS_min = 0.03
    endif
    dS = dS_min
    tuning = 1.2
    sgn = epc%sgn_in

    ! Temperature limits
    Tmin = tpTmin
    Tmax = tpTmax
    !Exit at specified temperature
    if (epc%override_exit_temperature) then
      Tmin = max(epc%Tme, Tmin)
    endif

    if (.not. epc%override_K_init) then
      ! Generate initial K values
      if (epc%beta_in < 0.5) then
        doBub = .true.
      else
        doBub = .false.
      endif
      ! Find temperature/pressure in correct range
      if (epc%spec == specP) then
        call PureSat(T,P,Z,.true.,ierr=ierr)
        ln_spec = log(p)
      else
        ln_spec = log(T)
        call PureSat(T,P,Z,.false.,ierr=ierr)
      endif
      call thermo(t,p,z,VAPPH,lnfugG)
      call thermo(t,p,z,LIQPH,lnfugL)
      if (abs(sum(lnfugG-lnfugL)) > 1.0e-3) then
        K = exp(lnfugL-lnfugG)
      else
        call sat_wilsonK(Z,K,t,p,epc%spec,doBub)
      endif
      ! Set early return
      return_iter = 200
      call sat_successive(Z,K,t,p,epc%spec,doBub,return_iter)
      s = ispec(epc%spec)
      iter = sat_newton(Z,K,t,p,beta,s,ln_spec)
      if (iter >= sat_max_iter) then
        print *,'envelopePlot: Initial point not found.'
        epc%returnCause = ER_NOINITIALPOINT
        return
      endif
      if (verbose) then
        print *,'*****************************'
        print *,'Phase envelope:'
        print *,'T,P:',t,p
      endif
    else
      K = epc%K_init
    endif
    if (isSingleComp(Z)) then
      K = 1
    endif

    if (epc%set_y_phase) then
      Yphase = epc%Yph
    else
      Yphase = VAPPH
    endif
    X2 = Z/(1-beta+beta*K)
    X1 = K*Z/(1-beta+beta*K)
    call specificvolume(T,P,X1,Yphase,v1)
    call specificvolume(T,P,X2,LIQPH,v2)
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(v1)
    Xvar(nc+3) = log(v2)
    s = ispec_tv(epc%spec)
    param(1:nc) = Z
    param(nc+1) = beta
    param(nc+2) = real(s)
    call set_ln_spec(Xvar,p,s,param,ln_spec)
    if (epc%set_pmin) then
      Pstart = epc%Pmin
    else
      Pstart = p
    endif

    ! Set output
    n = 1
    call setEnvelopePoint_tv(Xvar,param,p,n,Ta,Pa,v1a,v2a,Ka,betaa,nmax)

    do while (n < nmax)
      dXdSold = dXdS
      call newton_tv_extrapolate(Xvar,param,dXdS,dpds)
      smax = maxloc(abs(dXdS),dim=1)
      if ((.not. smax == s) .and. n > 1) then
        s = smax
        ! Rescaling the sensitivities
        sgn = sign(1.0,Xvar(s) - Xold(s))
        dpds = dpds / dXdS(s)
        dXdS = dXdS / dXdS(s)
        dXdSold = dXdSold / dXdSold(s)
        param(nc+2) = real(s)
      endif

      dP = abs(dpds*ds)/1e5
      if (dP > maxdP) then
        ! Limit step in pressure
        dS = max(min(dS*maxdP/dP,dS_max),dS_min)
      endif
      p_old = p
      Xold = Xvar
      Xvar = Xold + dXdS*dS*sgn
      if (s == nc+4) p = exp(log(p) + dS*sgn)
      call set_ln_spec(Xvar,p,s,param,ln_spec)
      iter = sat_newton_tv_x(Xvar,param,p,ierr)
      T = exp(Xvar(nc+1))
      excessive_T_jump = (abs(exp(Xold(nc+1)) - T) > maxdT)
      excessive_P_jump = (abs(p_old - p)/1e5 > maxdP)
      if (ierr /= 0 .OR. excessive_T_jump .OR. excessive_P_jump) then
        ! Something went wrong.
        ! Attempt to make the step shorter.
        Xvar = Xold + dXdS*dS*sgn*0.5
        if (s == nc+4) p = exp(log(p_old) + dS*sgn*0.5)
        call set_ln_spec(Xvar,p,s,param,ln_spec)
        iter = sat_newton_tv_x(Xvar,param,p,ierr)
        T = exp(Xvar(nc+1))
        excessive_T_jump = (abs(exp(Xold(nc+1)) - T) > maxdT)
      endif
      if (ierr /= 0 .OR. excessive_T_jump) then
        ! Something went wrong.
        ! Attempt to make the step longer.
        Xvar = Xold + dXdS*dS*sgn*2.0
        if (s == nc+4) p = exp(log(p_old) + dS*sgn*2.0)
        call set_ln_spec(Xvar,p,s,param,ln_spec)
        iter = sat_newton_tv_x(Xvar,param,p,ierr)
        T = exp(Xvar(nc+1))
        excessive_T_jump = (abs(exp(Xold(nc+1)) - T) > maxdT)
      endif

      if (ierr == 0 .and. .not. excessive_T_jump) then
        ! Test if state is stable and not too close to the critical point
        call get_variables_tv(Xvar,param,T,P,v1,v2,X1,X2,K)
        if (calcStabMinEigTV(T,v1,X1) < 1.0e-8) then
          ! Try to avoid meta-stable states
          Xvar = Xold + dXdS*dS*sgn*2.0
          if (s == nc+4) p = exp(log(p_old) + dS*sgn*2.0)
          call set_ln_spec(Xvar,p,s,param,ln_spec)
          iter = sat_newton_tv_x(Xvar,param,p,ierr)
          T = exp(Xvar(nc+1))
          excessive_T_jump = (abs(exp(Xold(nc+1)) - T) > maxdT)
        endif
      endif

      if (ierr /= 0 .OR. excessive_T_jump) then
        ! Give up
        epc%returnCause = ER_NOSOL
        exit
      endif

      !Exit at thermo limit or defined pressure
      if (p < Pstart) then
        s = nc+4
        recalculate = .true.
        ln_spec = log(Pstart)
      else if (p >= epc%Pmax) then
        s = nc+4
        recalculate = .true.
        ln_spec = log(epc%Pmax)
      endif

      ! Is temperature decreasing? - And below Tmin + safety limit?
      if (Xvar(nc+1) - Xold(nc+1) < 0.0 .and. T < Tmin + 0.01) then
        ! Exit at temperature
        s = nc+1
        ln_spec = log(Tmin)
        recalculate = .true.
      endif

      if (recalculate) then
        ! Extrapolate from previous point
        if (s <= nc+3) then
          param(nc+2) = real(s)
          dS = ln_spec - Xold(s) ! Sign included
        endif
        call newton_tv_extrapolate(Xold,param,dXdS,dpds)
        if (s == nc+4) then
          param(nc+2) = real(s)
          if (abs(dpds) > 10.0) ds = (exp(ln_spec) - p_old)/dpds
        endif
        Xvar = Xold + dXdS*dS
        param(nc+3) = ln_spec
        iter = sat_newton_tv_x(Xvar,param,p,ierr)
        exit_after_saving = .true.
      endif

      ! Set output
      n = n + 1
      call setEnvelopePoint_tv(Xvar,param,p,n,Ta,Pa,v1a,v2a,Ka,betaa,nmax)
      if (n == nmax) then
        epc%returnCause = ER_NMAX
        exit ! Exit before cricon calculations
      endif
      if (exit_after_saving) then
        epc%returnCause = ER_PMAX_TMIN
        exit ! Exit before cricon calculations
      endif

      ! if (epc%exit_on_triple_point) then
      !   ! Have we passed a triple point?
      !   if (passedTriplePoint(Xold,Xvar,Z,beta)) then
      !     epc%returnCause = ER_TRIPLE
      !     return
      !   endif
      ! endif

      if (epc%do_stability_check) then
        if (.not. isStable_tv(t,v1,Z,K,beta,epc%Wstab,epc%vW)) then
          if (verbose) then
            print *,'Not stable any more....',t,p
          endif
          epc%returnCause = ER_STABILITY
          return
        endif
      endif

      ! Tune dS up or down based on how fast sat_newton_tv converged
      if (iter < 3) then
        dS = dS * tuning
      else if (iter > 5) then
        dS = dS/tuning
      endif
      dS = max(min(dS,dS_max),dS_min)
    enddo

  end subroutine envelope_plot_tv_epc

  !-----------------------------------------------------------------------------
  !> Set solution
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine setEnvelopePoint_tv(X,param,p,i,Ta,Pa,v1a,v2a,Ka,betaa,nmax)
    implicit none
    real, dimension(nc+3), intent(in) :: X
    real, dimension(nc+3), intent(in) :: param
    real, intent(in) :: p
    integer, intent(in) :: i, nmax
    real, dimension(nmax), intent(inout) :: Ta,Pa
    real, dimension(nmax), intent(inout) :: v1a,v2a
    real, dimension(nmax,nc), intent(inout) :: Ka
    real, dimension(nmax), intent(inout) :: betaa
    ! Locals
    real, dimension(nc) :: K
    !real, dimension(nc), intent(in) :: Z
    real :: beta
    !
    K = exp(X(1:nc))
    beta = param(nc+1)
    !
    Ta(i) = exp(X(nc+1))
    v1a(i) = exp(X(nc+2))
    v2a(i) = exp(X(nc+3))
    betaa(i) = beta
    Ka(i,:) = K
    Pa(i) = p
    !print *,"Point number ",i
    !call print_envelope_point_tv(X,param)
    !print *,Ta(i),Pa(i),betaa(i),Ka(i,:),v1a(i),v2a(i)
    !print *,Ta(i),Pa(i)
  end subroutine setEnvelopePoint_tv

  !-----------------------------------------------------------------------------
  !> Print envelope-point
  !>
  !> \author MH, 2021-12
  !-----------------------------------------------------------------------------
  subroutine print_envelope_point_tv(X,param)
    implicit none
    real, dimension(nc+3), intent(in) :: X
    real, dimension(nc+3), intent(in) :: param
    ! Locals
    real :: beta, K(nc), T, v1, v2, Z(nc), X1(nc), X2(nc), p1, p2
    !
    beta = param(nc+1)
    Z = param(1:nc)
    K = exp(X(1:nc))
    X2 = Z/(1-beta+beta*K)
    X1 = K*Z/(1-beta+beta*K)
    print *,"K: ",K
    T = exp(X(nc+1))
    print *,"Temperature: ",T
    v1 = exp(X(nc+2))
    v2 = exp(X(nc+3))
    print *,"v1: ",v1
    print *,"v2: ",v2
    print *,"beta: ",beta
    print *,"X1: ",X1
    print *,"X2: ",X2
    p1 = pressure(T,v1,X1)
    p2 = pressure(T,v2,X2)
    print *,"pressure: ",p1,p2
  end subroutine print_envelope_point_tv

  !-----------------------------------------------------------------------------
  !> Set specification
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine set_ln_spec(Xvar,p,s,param,ln_spec)
    implicit none
    real, dimension(nc+3), intent(in) :: Xvar
    real, dimension(nc+3), intent(inout) :: param
    real, intent(in) :: p
    integer, intent(in) :: s
    real, intent(out) :: ln_spec
    !
    if (s == nc+4) then
      ln_spec = log(p)
    else
      ln_spec = Xvar(s)
    endif
    param(nc+3) = ln_spec
  end subroutine set_ln_spec

  !-----------------------------------------------------------------------------
  !> Test if new liquid phase should be introduced
  !>
  !> \author MH, 2015-09
  !-----------------------------------------------------------------------------
  function isStable_tv(t,v1,Z,K,beta1,W,vW) result(isS)
    use eos, only: thermo, specificvolume
    use thermopack_constants, only: LIQPH, VAPPH, WATER, NONWATER
    use stability, only: stabcalcW, stabilityLimit
    use thermo_utils, only: waterComponentFraction, wilsonK
    implicit none
    real, intent(in) :: t,v1,beta1
    real, dimension(nc), intent(in) :: Z,K
    real, dimension(nc), intent(out) :: W
    real, intent(out) :: vW
    logical :: isS
    ! Locals
    integer :: nd, phase, pid, i
    real :: XX(2,nc),lnFug1(nc),lnFugW(nc),tpd,p
    real, dimension(nc) :: Kw,dKwdp,dKwdt,WW
    real :: tpd_fac = 5.0e4
    logical, parameter :: debug_stability = .false.
    isS = .true.
    XX(2,:) = Z/(1-beta1+beta1*K)
    XX(1,:) = K*XX(2,:)
    p = pressure(t,v1,XX(1,:))
    call thermo_tv(t,v1,XX(1,:),lnFug1)
    lnFug1 = lnFug1 - log(P*XX(1,:))
    nd = 2

    ! Look for water or liquid phase
    if (waterComponentFraction(XX(2,:)) > 0.8) then
      pid = NONWATER
      phase = VAPPH
    else
      pid = WATER
      phase = LIQPH
    endif

    call wilsonK(t,p,Kw,dKwdp,dKwdt,pid)
    if (phase == LIQPH) then
      W = Z/Kw
    else
      W = Kw*Z
    endif
    tpd = stabcalcW(nd,1,t,p,XX,W,phase,lnFug1,lnFugW,preTermLim=-1000.0)
    isS = (tpd > stabilityLimit*tpd_fac)
    if (.not. isS) then
      call specificvolume(t,p,W,phase,vW)
      if (debug_stability) call print_stab_res()
      return
    endif
    if (pid == NONWATER .and. phase == VAPPH) then
      ! Test also for liquid phase
      phase = LIQPH
      W = Z/Kw
      tpd = stabcalcW(nd,1,t,p,XX,W,phase,lnFug1,lnFugW,preTermLim=-1000.0)
      isS = (tpd > stabilityLimit*tpd_fac)
      if (.not. isS) then
        call specificvolume(t,p,W,phase,vW)
        if (debug_stability) call print_stab_res()
        return
      endif
    endif
    ! Loop all possible liquid phases
    do i=1,nc
      WW = 0.0
      WW(i) = 1.0
      tpd = stabcalcW(nd,1,t,p,XX,WW,LIQPH,lnFug1,lnFugW,preTermLim=-1000.0)
      if (tpd < stabilityLimit*tpd_fac) then
        isS = .false.
        W = WW
        call specificvolume(t,p,W,LIQPH,vW)
        if (debug_stability) call print_stab_res()
        exit
      endif
    enddo
  contains
    subroutine print_stab_res()
      print *,"t,p",t,p*1e-6
      print *,"tpd",tpd,stabilityLimit*5.0e4
      print *,"W",W
      print *,"Z",Z
      print *,"X2",XX(2,:)
    end subroutine print_stab_res
  end function isStable_tv

  !-----------------------------------------------------------------------------
  !> Get step direction K-value starting point
  !>
  !> \author MH, 2021-11
  !-----------------------------------------------------------------------------
  subroutine get_step_direction_tv(t,Z,K,v1,v2,beta1,stepVar,sgn)
    implicit none
    real, intent(in) :: t,beta1,v1,v2
    real, dimension(nc), intent(in) :: Z,K
    ! Output
    real, intent(out) :: sgn
    integer, intent(out) :: stepVar
    ! Locals
    real, dimension(nc) :: Wstab, Kl
    real, dimension(nc+3) :: Xvar, dXdS, param
    real :: ds, ln_spec, tl, pl, v1l, v2l, vW, dpds
    integer :: s, iter, ierr
    ! Set variables
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(T)
    Xvar(nc+2) = log(v1)
    Xvar(nc+3) = log(v2)
    ! Extrapolate
    s = nc+1
    stepVar = specT
    ln_spec = Xvar(s)
    param(1:nc) = Z
    param(nc+1) = beta1
    param(nc+2) = real(s) ! typecast since param can only contain reals..
    param(nc+3) = ln_spec
    call newton_tv_extrapolate(Xvar,param,dXdS,dpds)
    s = maxloc(abs(dXdS),dim=1)
    dXdS = dXdS / dXdS(s)
    if (s < nc+1) then
      stepVar = s + 4
    else if (s == nc+1) then
      stepVar = specT
    else if (s == nc+2) then
      stepVar = specV1
    else if (s == nc+3) then
      stepVar = specV2
    else if (s == nc+4) then
      stepVar = specP
    endif

    ! Small perturbation
    ds = 1.0e-3
    sgn = 1.0
    Xvar = Xvar + dXdS*ds*sgn
    Kl = exp(Xvar(1:nc))
    tl = exp(Xvar(nc+1))
    v1l = exp(Xvar(nc+2))
    v2l = exp(Xvar(nc+3))
    ln_spec = Xvar(s)
    iter = sat_newton_tv(Z,Kl,tl,v1l,v2l,beta1,s,ln_spec,pl,ierr)
    if (ierr /= 0) then
      sgn = -1.0
    else
      if (.not. isStable_tv(tl,v1l,Z,Kl,beta1,Wstab,vW)) then
        sgn = -1.0
      endif
    endif
  end subroutine get_step_direction_tv

end module saturation_tv
