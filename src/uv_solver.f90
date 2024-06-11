!--------------------------------------------------------------------------
!> Calculate solve UV-flash for single phase gas/liquid or a
!! gas-liquid mixture.
!!
!! \todo Need trace-component functionallity.
!!
!-------------------------------------------------------------------------
module uv_solver
  !
  !
  use thermopack_constants, only: LIQPH, VAPPH, continueOnError, zLimit, FAKEPH, &
       SINGLEPH, SOLIDPH, TWOPH, VAPSOLPH, MINGIBBSPH
  use numconstants, only: machine_prec, small
  use thermopack_var, only: nc, nph, get_active_thermo_model, thermo_model, &
       tpPmax, tpPmin, Rgas, tpTmin, tpTmax, robustness_level
  use eos
  use tp_solver, only: twoPhaseTPflash, rr_solve
  use state_functions
  use stability, only : stabcalc, stabilityLimit
  implicit none
  private
  save

  ! Option to initialize tangen plane search with custom composition
  logical :: doCustomStabCheck = .false.
  integer :: custumPhase = VAPPH
  real, allocatable, dimension(:) :: wInitial

  !> When to test if phase should be dropped?
  real, parameter :: dropPhaseTestBeta = 1.0e-10

  !> Accept two-phase solution even though its Gibbs energy
  !> is slightly larger than that of the single-phase feed
  real :: g_tolerance = machine_prec * 10.0

  !> Properties for nesed loop
  real :: nested_tolerance = 1.0e7*machine_prec
  integer :: nested_line_searches = 3
  integer :: nested_nmax = 200

  !> Properties for full equation system solver
  real :: fulleq_tolerance = 1.0e4*machine_prec
  integer :: fulleq_line_searches = 4
  integer :: fulleq_nmax = 100

  !> Single component solver
  real :: singleuv_tolerance = 1.0e4*machine_prec
  integer :: singleuv_line_searches = 3
  integer :: singleuv_nmax = 100

  public :: twoPhaseUVflash, twoPhaseUVflashNested, twoPhaseUVflashFull
  public :: enableCustumStabCalc, disableCustumStabCalc
  public :: setNestedUVtolerance, setFullEqUVtolerance
  public :: getNestedUVtolerance, getFullEqUVtolerance
  public :: setSingleCompUVtolerance, getSingleCompUVtolerance
  public :: twoPhaseUVsingleComp, jac_1ph, fun_1ph

contains

  !-------------------------------------------------------------------------
  !> Do UV-flash:
  !> Switch for multicomponent (mc) and single component solver.
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine twoPhaseUVflash(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase)
    use thermo_utils, only: isSingleComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    !
    if (isSingleComp(Z)) then
      call twoPhaseUVsingleComp(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase)
    else
      call twoPhaseUVflash_mc(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase)
    endif
    !
  end subroutine twoPhaseUVflash

  !-------------------------------------------------------------------------
  !> Do multicomponent UV-flash:
  !> Try solving all equations at once, for given initial values.
  !> Switch to nested loop if it fails to converge.
  !>
  !> Assume initial values for specified phase.
  !>
  !> \author MH, 2012-08-15
  !-------------------------------------------------------------------------
  subroutine twoPhaseUVflash_mc(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase)
    use thermo_utils, only: wilsonK
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    ! Locals
    logical :: converged, tryNewPhase, nestedIsConverged
    real :: beta0, t0, p0, betaL0
    real, dimension(nc) :: X0, Y0, K
    integer :: ph0, ph1
    !
    ph0 = phase
    ph1 = phase
    beta0 = beta
    betaL0 = betaL
    t0 = t
    p0 = p
    Y0 = Y
    X0 = X
    !
    tryNewPhase = .true.
    converged = .false.
    if (robustness_level > 0) then
      call twoPhaseUVflashNested(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,converged)
    else
      call twoPhaseUVflashFull(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,converged)
    end if
    !
    if (.not. converged) then
      if (phase == TWOPH) then
        if (beta <= 0.0 .or. beta >= 1.0) then
          ! Calculate starting values
          call wilsonK(t,p,K)
          tryNewPhase = rr_solve(nc,Z,K,beta,X,Y,.false.,betaL)
          if (beta < 0.0 .or. betaL < 0.0) then
            ! Invalid beta
            tryNewPhase = .false.
          endif
        endif
      endif
      if (tryNewPhase) then
        ph1 = phase
        call twoPhaseUVflashFull(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,&
             converged)
        if (.not. converged .and. ph0 /= TWOPH .and. ph1 /= TWOPH) then
          phase = TWOPH
          tryNewPhase = .true.
          if (beta <= 0.0 .or. beta <= 1.0) then
            ! Calculate starting values
            call wilsonK(t,p,K)
            tryNewPhase = rr_solve(nc,Z,K,beta,X,Y,.false.,betaL)
            if (beta < 0.0 .or. betaL < 0.0) then
              ! Invalid beta
              tryNewPhase = .false.
            endif
          endif
          if (tryNewPhase) then
            call twoPhaseUVflashFull(t,p,Z,beta,betaL,X,Y,uspec,vspec,&
                 phase,converged)
          endif
        endif
      endif
    endif

    if (.not. converged) then
      t = t0
      p = p0
      call twoPhaseUVflashNested(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,&
           nestedIsConverged)
      if (.not. nestedIsConverged) then
        ! Too slow convergence in netsed loop iteration. Restart full solver.
        call twoPhaseUVflashFull(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,&
             converged)
        if (.not. converged) then
          print *,'Temperature',t
          print *,'Pressure',p
          print *,'Initial temperature',t0
          print *,'Initial pressure',p0
          print *,'Internal energy',uspec
          print *,'Specific volume',vspec
          print *,'Comp.',Z
          print *,'X0 ', X0
          print *,'Y0 ', Y0
          print *,'Phase0 ', ph0
          print *,'beta0 ', beta0
          print *,'betaL0 ', betaL0
          call stoperror('uv_solver::twoPhaseUVflash: UV-flash did not converge.')
        endif
      endif
    endif
    if (phase /= TWOPH) then
      beta = 0.0
      betaL = 0.0
      Y = 0.0
      X = 0.0
    endif
    !
  end subroutine twoPhaseUVflash_mc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Nested-loop !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-------------------------------------------------------------------------
  !> Do UV-flash using PT-flash in nested loop
  !>
  !> Function to minimize:
  !>  \f$ -\frac{g_{min} - u_{spec} - p v_{spec}}{T} \f$.
  !>
  !> \author MH, 2012-07-05
  !-------------------------------------------------------------------------
  subroutine twoPhaseUVflashNested(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,isConverged)
    use optimizers, only: optimize, optim_param, setX
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    logical, optional, intent(out) :: isConverged
    ! Locals
    type(optim_param) :: optim
    real, dimension(2) :: var
    real, dimension(nc+2) :: param
    real :: tInitial, pInitial, Tmin, Tmax
    !
    if (present(isConverged)) then
      isConverged = .true.
    endif
    Tmin = tpTmin
    Tmax = tpTmax
    if (t > Tmax .OR. t < Tmin) then
      t = 0.5*(Tmax+Tmin)
    endif
    if (p > tpPmax .OR. p < tpPmin) then
      p = 0.1*(tpPmax+tpPmin)
    endif
    tInitial = t
    pInitial = p
    !
    ! Do modified newton search
    param(1) = uspec
    param(2) = vspec
    param(3:nc+2) = Z
    var(1) = log(t) ![K]
    var(2) = log(p) ![Pa]
    optim%rel_tol = nested_tolerance
    optim%max_line_search_iter = nested_line_searches
    optim%gradient_termination = .true.
    optim%max_iter = nested_nmax
    call optimize(optim,objective,differentials,var,param,&
         limitDvar,prematureReturn,get_problem_size,setX)
    if (optim%exitflag > 0) then
      if (present(isConverged)) then
        isConverged = .false.
      else
        print *,'Error: ',optim%error
        print *,'Tolerance: ',optim%rel_tol
        print *,'Temperature',var(1)
        print *,'Pressure',var(2)
        print *,'Initial temperature',tInitial
        print *,'Initial pressure',pInitial
        print *,'Internal energy',uspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        print *,'Iterations used: ',optim%iter
        call stoperror('uv_solver::twoPhaseUVflashNested: The nonlinear optimizer did not converge')
      endif
    endif
    !
    ! Set solution
    t = exp(var(1))
    p = exp(var(2))
    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)

  end subroutine twoPhaseUVflashNested

  !-------------------------------------------------------------------------
  !> Calculate state function for UV system
  !>
  !> \author MH, 2012-07-06
  !-------------------------------------------------------------------------
  function objective(var,param) result(of)
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    real :: of !< Objective function value
    ! Locals
    real:: beta, t, p, g, s, h, uspec, vspec, sg, sl, hg, hl, betaL
    real, dimension(nc) :: Z,X,Y
    integer :: phase, sphase
    !
    t = exp(var(1))
    p = exp(var(2))
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    !
    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)
    !
    if (phase == TWOPH) then
      call entropy(t,p,x,LIQPH,sl)
      call entropy(t,p,y,VAPPH,sg)
      call enthalpy(t,p,x,LIQPH,hl)
      call enthalpy(t,p,y,VAPPH,hg)
      h = beta*hg+betaL*hl
      s = beta*sg+betaL*sl
    else
      sphase = LIQPH
      if (phase == VAPPH) then
        sphase = VAPPH
      endif
      call entropy(t,p,z,sphase,s)
      call enthalpy(t,p,z,sphase,h)
    endif
    g = h - t*s
    of = -(g - uspec - p*vspec)/(T*Rgas)
  end function objective

  !-------------------------------------------------------------------------
  !> Calculate state function for PH system and its
  !> differentials.
  !>
  !> \author MH, 2012-07-06
  !-------------------------------------------------------------------------
  subroutine differentials(var,param,of,dOFdvar,Hof)
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: Hof !< Hessian matrix of objective function
    real, dimension(2), intent(out) :: dOFdvar !< Differential of objective function with respect to temperature
    real, intent(out) :: of !< Objective function value
    ! Locals
    real :: beta, t, p, g, s, h, v, uspec, vspec, sg, sl, hg, hl, vg, vl, betaL
    real :: dvdp, dvdt, dhdt
    real, dimension(nc) :: Z,X,Y
    integer :: phase, sphase
    !real :: det, landa_min, tau, tr
    !real, parameter :: delta = machine_prec*5.0e3
    t = exp(var(1))
    p = exp(var(2))
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)

    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)

    if (phase == TWOPH) then
      call entropy(t,p,x,LIQPH,sl)
      call entropy(t,p,y,VAPPH,sg)
      call enthalpy(t,p,x,LIQPH,hl)
      call enthalpy(t,p,y,VAPPH,hg)
      call specificVolume(t,p,x,LIQPH,vl)
      call specificVolume(t,p,y,VAPPH,vg)
      h = beta*hg+betaL*hl
      s = beta*sg+betaL*sl
      v = beta*vg+betaL*vl
      dhdt = dhdt_twoPhase(t,p,Z,beta,betaL,X,Y)
      dvdt = dvdt_twoPhase(t,p,Z,beta,betaL,X,Y)
      dvdp = dvdp_twoPhase(t,p,Z,beta,betaL,X,Y)
    else
      sphase = LIQPH
      if (phase == VAPPH) then
        sphase = VAPPH
      endif
      call entropy(t,p,z,sphase,s)
      call enthalpy(t,p,z,sphase,h,dhdt=dhdt)
      call specificVolume(t,p,z,sphase,v,dvdt=dvdt,dvdp=dvdp)
    endif
    g = h - t*s
    of = -(g - uspec - p*vspec)/(T*Rgas)
    dOFdvar(1) = (h - uspec - p*vspec)/(T*Rgas)
    dOFdvar(2) = -p*(v - vspec)/(T*Rgas)
    Hof(1,1) = dhdt/Rgas
    Hof(1,2) = -p*dvdt/Rgas
    Hof(2,1) = Hof(1,2)
    Hof(2,2) = -p**2*dvdp/(T*Rgas)
    !
    ! det < 0 -> indefinite hessian, can not use newton step (not decent)
    !   modify the hessian, so it has positive eigenvalues
    ! Should be handled by the modified Cholesky factorization
    !det = Hof(1,1)*Hof(2,2)-Hof(1,2)*Hof(2,1)
    !tr = Hof(1,1)+Hof(2,2)
    !print *,(tr-sqrt(tr**2-4*det))/2.0
    !if (det <= delta) then
    !  landa_min = (tr-sqrt(tr**2-4*det))/2.0
    !  tau = delta - landa_min
    !  Hof(1,1) = Hof(1,1) + tau
    !  Hof(2,2) = Hof(2,2) + tau
    !endif
  end subroutine differentials

  !-------------------------------------------------------------------------
  !> Limit change in temperature.
  !>
  !> \author MH, 2012-07-06
  !-------------------------------------------------------------------------
  subroutine limitDvar(var,param,dvar)
    use numconstants, only: expMax
    implicit none
    real, dimension(2), intent(in)    :: var !< Variable
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    real, dimension(2), intent(inout) :: dvar !< Calculated change in variables
    ! Locals
    real, parameter :: maxstep_t = 25.0
    real, parameter :: maxstep_p = 5.0e5
    real :: factor, T0, P0, T1, P1, Tmin, Tmax
    real, dimension(2) :: dvar_tp
    Tmin = tpTmin
    Tmax = tpTmax
    if (var(1) + dvar(1) > 0.99*expMax) then
      factor = 1.0/dvar(1)
      dvar(1) = 0.99*expMax - var(1)
      factor = factor * dvar(1)
      dvar(2) = dvar(2)*factor
    endif
    if (var(2) + dvar(2) > 0.99*expMax) then
      factor = 1.0/dvar(2)
      dvar(2) = 0.99*expMax - var(2)
      factor = factor * dvar(2)
      dvar(1) = dvar(1)*factor
    endif
    factor = 1.0
    T0 = exp(var(1))
    T1 = exp(var(1) + dvar(1))
    P0 = exp(var(2))
    dvar_tp(1) = T1 - T0
    if (abs(dvar_tp(1)) > maxstep_t) then
      factor = 1.0/dvar(1)
      if (dvar_tp(1) > 0.0) then
        dvar(1) = min(dvar(1),log(T0 + maxstep_t) - var(1))
      else
        dvar(1) = max(dvar(1),log(T0 - maxstep_t) - var(1))
      endif
      factor = factor*dvar(1)
      T1 = exp(var(1) + dvar(1))
    endif
    if (dvar(1) > 0.0 .AND. T1 > Tmax) then
      factor = 1.0/dvar(1)
      dvar(1) = log(Tmax) - var(1)
      factor = factor*dvar(1)
    endif
    if (dvar(1) < 0.0 .AND. T1 < Tmin) then
      factor = 1.0/dvar(1)
      dvar(1) = log(Tmin) - var(1)
      factor = factor*dvar(1)
    endif
    !
    if (factor /= 1.0) then
      dvar(2) = dvar(2)*factor
      factor = 1.0
    endif
    P1 = exp(var(2) + dvar(2))
    dvar_tp(2) = P1 - P0
    if (abs(dvar_tp(2)) > maxstep_p) then
      factor = 1.0/dvar(2)
      if (dvar_tp(2) > 0.0) then
        dvar(2) = min(dvar(2),log(P0 + maxstep_p) - var(2))
      else
        dvar(2) = max(dvar(2),log(P0 - maxstep_p) - var(2))
      endif
      factor = factor*dvar(2)
      P1 = exp(var(2) + dvar(2))
    endif
    if (dvar(2) > 0.0 .AND. P1 > tpPmax) then
      factor = 1.0/dvar(2)
      dvar(2) = log(tpPmax) - var(2)
      factor = factor*dvar(2)
    endif
    if (dvar(2) < 0.0 .AND. P1 < tpPmin) then
      factor = 1.0/dvar(2)
      dvar(2) = log(tpPmin) - var(2)
      factor = factor*dvar(2)
    endif
    !
    if (factor /= 1.0) then
      dvar(1) = dvar(1)*factor
    endif
  end subroutine limitDvar

  !-------------------------------------------------------------------------
  !> Premature termination when solution is outside valid temperature region.
  !>
  !> \author MH, 2012-07-06
  !-------------------------------------------------------------------------
  function prematureReturn(var,param,of,dofdvar) result(doReturn)
    implicit none
    real, dimension(2), intent(in)      :: var !< Variables
    real, dimension(nc+2), intent(in)   :: param !< Parameter vector
    real, intent(in)                    :: of !< Objective function value
    real, dimension(2), intent(in)      :: dofdvar !< Differential of objective function
    logical                             :: doReturn !< Terminate minimization?
    ! Locals

    doReturn = .false.

  end function prematureReturn

  !-------------------------------------------------------------------------
  !> Support for variable size in problem
  !>
  !> \author MH, 2012-07-06
  !-------------------------------------------------------------------------
  function get_problem_size(param) result(nvar)
    implicit none
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    integer :: nvar

    nvar = 2

  end function get_problem_size


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Full NR-flash !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-------------------------------------------------------------------------
  !> Do UV-flash using full equation system
  !>
  !> Assume initial values for specified phase.
  !>
  !> \author MH, 2012-08-15
  !-------------------------------------------------------------------------
  subroutine twoPhaseUVflashFull(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,converged)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, limit_dx, &
         premterm_at_dx_zero, setXv
    use thermo_utils, only: isSingleComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    logical, intent(out) :: converged
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(nc+2) :: var, xmin, xmax
    real, dimension(nc+3) :: param
    real, dimension(2*nc+2) :: paramTwo
    real, dimension(nc) :: FUGZ, FUGL, FUGG, K, L
    integer :: minGphase, nov, i
    real :: g_feed, tpd, g_mix, Tmin, Tmax
    logical :: liq_stab_negative, gas_stab_negative
    !
    converged = .false.
    Tmin = tpTmin
    Tmax = tpTmax
    if (t > Tmax .OR. t < Tmin) then
      t = 0.5*(Tmax+Tmin)
    endif
    if (p > tpPmax .OR. p < tpPmin) then
      p = 0.1*(tpPmax+tpPmin)
    endif
    !
    ! Do modified newton search
    param(1) = uspec
    param(2) = vspec
    param(3:nc+2) = Z
    if (phase == TWOPH) then
      var(1:nc) = beta*Y
      if (minval(Z-var(1:nc),1) <= 0.0) then
        ! Modify beta to get valid initial variables
        do i=1,nc
          if (Z(i) > zLimit) then
            beta = min(beta,Z(i)/Y(i))
          endif
        enddo
        beta = beta*0.95
        var(1:nc) = beta*Y
      endif
      var(nc+1) = log(t)
      var(nc+2) = log(p)
      nov = nc+2
    else if (phase == LIQPH .OR. phase == VAPPH) then
      var(1) = log(t)
      var(2) = log(p)
      nov = 2
    else if (phase == SINGLEPH) then
      phase = LIQPH
      var(1) = log(t)
      var(2) = log(p)
      nov = 2
    else
      write(*,*) "phase=",phase
      call stoperror('uv_solver::twoPhaseUVflashFull: Wrong phase indicator.')
    endif

    ! Set solver parameters
    solver%abs_tol = fulleq_tolerance
    solver%ls_max_it = fulleq_line_searches
    solver%max_it = fulleq_nmax
    solver%symmetric_jac = .true.
    if (nov == 2) then ! Try single phase
      param(nc+3) = phase
      xmin(1) = log(Tmin)
      xmax(1) = log(Tmax)
      xmin(2) = log(tpPmin)
      xmax(2) = log(tpPmax)
      call nonlinear_solve(solver,fun_1ph,jac_1ph,jac_1ph,limit_dx,&
           premterm_at_dx_zero,setXv,var(1:2),xmin(1:2),xmax(1:2),param)
      if (solver%exitflag == 0) then
        ! Test solution for stability
        ! Get minimum gibbs solution for single phase
        t = exp(var(1))
        p = exp(var(2))
        call thermo(t,p,Z,MINGIBBSPH,FUGZ,ophase=minGphase)
        if (isSingleComp(Z)) then
          if (minGphase == phase .OR. minGphase == SINGLEPH) then
            converged = .true.
          else
            converged = .false.
          endif
        else
          if (minGphase == phase .OR. minGphase == SINGLEPH) then
            tpd = stabcalc(t,p,Z,LIQPH,FUGZ,FUGL)
            liq_stab_negative = (tpd < stabilityLimit)
            tpd = stabcalc(t,p,Z,VAPPH,FUGZ,FUGG)
            gas_stab_negative = (tpd < stabilityLimit)
            ! Do we need to try another phase?
            if (liq_stab_negative .or. gas_stab_negative) then
              if (liq_stab_negative .and. gas_stab_negative) then
                K = exp(FUGL-FUGG)
              else if(liq_stab_negative) then
                K = exp(FUGL-FUGZ)
              else if (gas_stab_negative) then
                K = exp(FUGZ-FUGG)
              endif
              phase = TWOPH
              if (.not. rr_solve(nc,Z,K,beta,X,Y,.false.,betaL)) then
                beta = -1.0
                betaL = -1.0
              endif
            else
              converged = custumStabCalc(t,p,Z,FUGZ,X,Y,beta,betaL,phase)
              phase = minGphase
            endif
          else if (minGphase == FAKEPH) then
            !Feed is FAKEPH at current T and P -> Unstable
            phase = TWOPH
            if (.not. rr_solve(nc,Z,K,beta,X,Y,.false.,betaL)) then
              beta = -1.0
              betaL = -1.0
            endif
          else
            phase = minGphase
          endif
        endif
      else
        if (phase == LIQPH) then
          phase = VAPPH
        else
          phase = LIQPH
        endif
      endif
    else ! Try two-phase
      xmin(1:nc) = 0.0
      xmax(1:nc) = Z
      xmin(1+nc) = log(Tmin)
      xmax(1+nc) = log(Tmax)
      xmin(2+nc) = log(tpPmin)
      xmax(2+nc) = log(tpPmax)

      paramTwo(1:nc+2) = param(1:nc+2)
      paramTwo(nc+3:2*nc+2) = Z - var(1:nc)
      call nonlinear_solve(solver,fun_2ph,jac_2ph,jac_2ph,limit_2ph,&
           premterm_2ph,setV_2ph,var,xmin,xmax,paramTwo)
      if (solver%exitflag == 0) then
        ! Test solution for stability
        ! Get minimum gibbs solution for single phase
        t = exp(var(nc+1))
        p = exp(var(nc+2))
        beta = sum(var(1:nc))
        L = paramTwo(nc+3:2*nc+2)
        betaL = sum(L)
        Y = var(1:nc)/beta
        X = L/betaL
        call thermo(t,p,Z,MINGIBBSPH,FUGZ,ophase=minGphase)
        if (minGphase == FAKEPH) then
          ! Two phase solution is always considered stable if feed phase is FAKEPH
          converged = .true.
        else
          g_feed = 0.0
          do i=1,nc
            if (Z(i) > zLimit) then
              g_feed = g_feed + Z(i)*(log(Z(i))+FUGZ(i))
            endif
          enddo
          call thermo(t,p,X,LIQPH,FUGL)
          call thermo(t,p,Y,VAPPH,FUGG)
          g_mix = 0.0
          do i=1,nc
            if (Z(i) > zLimit) then
              g_mix = g_mix + beta*Y(i)*(log(Y(i))+FUGG(i)) + betaL*X(i)*(log(X(i))+FUGL(i))
            endif
          enddo
          ! Compare single phase gibbs energy to mixture gibbs energy
          if (g_mix - g_feed < g_tolerance) then
            converged = .true.
          else
            phase = minGphase
          endif
        endif
      else
        beta = sum(var(1:nc))
        betaL = sum(paramTwo(nc+3:2*nc+2))
        if (beta < 0.5) then
          phase = LIQPH
        else
          phase = VAPPH
        endif
      endif
    endif
    !
  end subroutine twoPhaseUVflashFull

  !-------------------------------------------------------------------------
  !> Calculate state function for UV system
  !>
  !> \author MH, 2012-08-15
  !-------------------------------------------------------------------------
  subroutine fun_1ph(f,var,param)
    implicit none
    real, dimension(2), intent(out) :: f !< Function values
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+3), intent(in) :: param !< Parameter vector
    ! Locals
    real:: t, p, h, uspec, vspec, v
    real, dimension(nc) :: Z
    integer :: phase
    !
    t = exp(var(1))
    p = exp(var(2))
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = INT(param(nc+3))
    !
    call specificVolume(t,p,z,phase,v)
    call enthalpy(t,p,z,phase,h)
    f(1)= (uspec+p*vspec-h)/(Rgas*t)
    f(2)= p*(v-vspec)/(Rgas*t)
  end subroutine fun_1ph

  !-------------------------------------------------------------------------
  !> Calculate state function for PH system and its
  !> differentials.
  !>
  !> \author MH, 2012-08-15
  !-------------------------------------------------------------------------
  subroutine jac_1ph(Jac,var,param)
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+3), intent(in) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: t, p, h, v, uspec, vspec
    real :: dvdp, dvdt, dhdt
    real, dimension(nc) :: Z
    integer :: phase
    t = exp(var(1))
    p = exp(var(2))
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = INT(param(nc+3))
    !
    call specificVolume(t,p,z,phase,v,dvdt=dvdt,dvdp=dvdp)
    call enthalpy(t,p,z,phase,h,dhdt=dhdt)
    !
    Jac(1,1) = -dhdt/Rgas
    Jac(1,2) = p*dvdt/Rgas
    Jac(2,1) = Jac(1,2)
    Jac(2,2) = p*p*dvdp/(Rgas*T)
  end subroutine jac_1ph

  !-------------------------------------------------------------------------
  !> Calculate state function for UV system
  !>
  !> \author MH, 2012-08-15
  !-------------------------------------------------------------------------
  subroutine fun_2ph(f,var,param)
    implicit none
    real, dimension(nc+2), intent(out) :: f !< Function values
    real, dimension(nc+2), intent(in) :: var !< Variable vector
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    ! Locals
    real:: betaV, t, p, uspec, vspec, hg, hl, vl, vg, h, v, betaL
    real, dimension(nc) :: Z,X,Y,FUGV,FUGL,L
    integer :: i
    real, parameter :: log_eps = machine_prec**5
    !
    t = exp(var(nc+1))
    p = exp(var(nc+2))
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    betaV = sum(var(1:nc))
    Y = var(1:nc)/betaV
    L = param(nc+3:2*nc+2)
    betaL = sum(L)
    X = L/betaL
    !
    call specificVolume(t,p,Y,VAPPH,vg)
    call enthalpy(t,p,Y,VAPPH,hg)
    call specificVolume(t,p,X,LIQPH,vl)
    call enthalpy(t,p,X,LIQPH,hl)
    h = betaV*hg+betaL*hl
    v = betaV*vg+betaL*vl
    f(nc+1)= (uspec+p*vspec-h)/(Rgas*t)
    f(nc+2)= p*(v-vspec)/(Rgas*t)
    !
    call thermo(t,p,X,LIQPH,FUGL)
    call thermo(t,p,Y,VAPPH,FUGV)
    do i=1,nc
      if (Z(i) > zLimit) then
        if (X(i) < log_eps) then
          X(i) = log_eps
        endif
        if (Y(i) < log_eps) then
          Y(i) = log_eps
        endif
        f(i) = log(Y(i)) - log(X(i)) + FUGV(i) - FUGL(i)
      else
        f(i) = 0.0
      endif
    enddo
  end subroutine fun_2ph

  !-------------------------------------------------------------------------
  !> Calculate state function for PH system and its
  !> differentials.
  !>
  !> \author MH, 2012-08-15
  !-------------------------------------------------------------------------
  subroutine jac_2ph(Jac,var,param)
    implicit none
    real, dimension(nc+2), intent(in) :: var !< Variable vector
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    real, dimension(nc+2,nc+2), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: betaV, t, p, uspec, vspec, betaL
    real, dimension(2) :: spec
    real, dimension(nc+2) :: RHS
    real, dimension(nc) :: Z,X,Y,L
    !
    t = exp(var(nc+1))
    p = exp(var(nc+2))
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    betaV = sum(var(1:nc))
    Y = var(1:nc)/betaV
    L = param(nc+3:2*nc+2)
    betaL = sum(L)
    X = L/betaL
    spec(1) = uspec
    spec(2) = vspec
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'UV',spec,Jac,RHS)
  end subroutine jac_2ph

  !-------------------------------------------------------------------------
  !> Terminate search for two-phase solution
  !>
  !> \author MH, 2012-08-16
  !-------------------------------------------------------------------------
  function premterm_2ph(var,dvar,param,n,np) result(premature_return)
    implicit none
    integer, intent(in)                :: n !< Dimension of X
    integer, intent(in)                :: np !< Dimension of param
    real, dimension(n), intent(in)     :: var !< Variable vector
    real, dimension(n), intent(inout)  :: dvar !< Change in variable vector
    real, dimension(np), intent(in)    :: param !< Parameter vector
    logical :: premature_return
    ! Locals
    real :: t,p,beta,betaL,Stwo,Ssingle,sl,sg
    integer :: phase
    real, dimension(nc) :: Z,L,X,Y
    !real, dimension(nc) :: fugZ,fugL,fugG
    !real :: gMix, gSingle

    beta = sum(var(1:nc))
    premature_return = .false.
    if (beta < dropPhaseTestBeta .or. beta > 1.0 - dropPhaseTestBeta) then
      ! Try removing phase - do we get an increase in S?
      Z(1:nc) = param(3:nc+2)
      if (beta > 0.5) then
        phase = VAPPH
      else
        phase = LIQPH
      endif
      Y = var(1:nc)/beta
      L = param(nc+3:2*nc+2)
      betaL = sum(L)
      X = L/betaL
      t = exp(var(nc+1))
      p = exp(var(nc+2))
      call entropy(t,p,y,VAPPH,sg,residual=.true.)
      call entropy(t,p,x,LIQPH,sl,residual=.true.)
      Stwo = beta*sg+betaL*sl
      call entropy(t,p,z,phase,Ssingle,residual=.true.)

      ! call thermo(T,P,Z,phase,FUGZ)
      ! gSingle = sum(Z*(log(Z)+FUGZ))
      ! call thermo(t,p,y,VAPPH,fugG)
      ! call thermo(t,p,x,LIQPH,fugL)
      ! gMix = beta*sum(Y*(log(Y)+FUGG)) + betaL*sum(X*(log(X)+FUGL))

      if (Ssingle > Stwo) then
        !print *,'Entropy',Stwo,Ssingle,phase,Stwo-Ssingle
        !print *,'Gibbs: ',gMix,gSingle,gMix-gSingle
        !print *,'Premature two phase UV termination'
        premature_return = .true.
      endif
    endif
    !
  end function premterm_2ph

  !-------------------------------------------------------------------------
  !> Limit change in variables
  !>
  !> \author MH, 2013-04-18
  !-------------------------------------------------------------------------
  subroutine limit_2ph(n,var,varmin,varmax,dvar,np,param)
    implicit none
    integer, intent(in) :: n
    real, dimension(n), intent(in) :: var !< Variable vector
    real, dimension(n), intent(inout) :: dvar !< Change in variable vector
    real, dimension(n), intent(in) :: varmax, varmin !< Parameter vector
    integer, intent(in) :: np
    real, dimension(np), intent(in)    :: param !< Parameter vector
    ! Locals
    integer :: i
    real :: scale
    real, dimension(nc) :: V,L,Z,DV

    scale = 1.0
    ! Temperature and pressure limits
    do i=nc+1,nc+2
      if (var(i)+dvar(i) < varmin(i) .AND. abs(dvar(i)) > 1.0e-9) then
        scale = min(scale,(varmin(i)-var(i))/dvar(i))
      endif
      if (var(i)+dvar(i) > varmax(i) .AND. abs(dvar(i)) > 1.0e-9) then
        scale = min(scale,(varmax(i)-var(i))/dvar(i))
      endif
    enddo

    Z(1:nc) = param(3:nc+2)
    L = param(nc+3:2*nc+2)
    V = var(1:nc)
    DV = dvar(1:nc)
    do i=1,nc
      if (V(i) < L(i)) then
        if (V(i) + DV(i) < 0.0) then
          scale = min(scale, -V(i)/DV(i))
        else if (Z(i) - V(i) - DV(i) < 0.0) then
          scale = min(scale, (Z(i)-V(i))/DV(i))
        endif
      else
        if (L(i) - DV(i) < 0.0) then
          scale = min(scale, L(i)/DV(i))
        else if (Z(i) - L(i) + DV(i) < 0.0) then
          scale = min(scale, (L(i)-Z(i))/DV(i))
        endif
      endif
    enddo
    if (scale < 1.0) then
      scale = scale * (1.0 - small)
      dvar = dvar*scale
    endif
    !
  end subroutine limit_2ph

  !-------------------------------------------------------------------------
  !> Set mole numbers. Try to avoid truncation error.
  !>
  !> \author MH, 2013-10-15
  !-------------------------------------------------------------------------
  subroutine setV_2ph(n,nparam,X,dX,varmin,varmax,param,alpha)
    implicit none
    integer, intent(in) :: n, nparam !< Problem dimension
    real, dimension(n), intent(inout) :: X !< Variables []
    real, dimension(n), intent(inout) :: dX !< Change in variables []
    real, dimension(n), intent(in) :: varmax, varmin !< Variable limit
    real, dimension(nparam), intent(inout) :: param !< Parameter vector
    real, intent(in) :: alpha !< dV scaling
    ! Locals
    real, dimension(nc) :: L,Z,V,dV
    integer :: i

    ! Temperature
    X(nc+1) = X(nc+1) + alpha*dX(nc+1)
    ! Pressure
    X(nc+2) = X(nc+2) + alpha*dX(nc+2)

    ! Mole numbers
    Z = param(3:nc+2)
    L = param(nc+3:2*nc+2)
    V = X(1:nc)
    dV = dX(1:nc)
    do i=1,nc
      if (V(i) >= L(i)) then
        L(i) = L(i) - alpha*dV(i)
        V(i) = Z(i) - L(i)
      else
        V(i) = V(i) + alpha*dV(i)
        L(i) = Z(i) - V(i)
      end if
    enddo

    X(1:nc) = V
    param(nc+3:2*nc+2) = L

    ! print *,'beta: ',sum(V)+sum(L)
    ! print *,'V: ',V
    ! print *,'L: ',L
    ! print *,'Z-L-V: ',Z-L-V

  end subroutine setV_2ph

  !-------------------------------------------------------------------------
  !> Enable additional phase stability check
  !>
  !> \author MH, 2014-01
  !-------------------------------------------------------------------------
  subroutine enableCustumStabCalc(w,phase)
    implicit none
    integer, intent(in) :: phase !< Phase identifyer
    real, dimension(nc), intent(in) :: W !< Initial comosition in stability calculation
    ! Locals
    integer :: ierr

    doCustomStabCheck = .true.
    custumPhase = phase
    if (phase /= VAPPH .OR. phase /= LIQPH) then
      call StopError('Wrong phase specified for custom phase stability in uv_solver')
    endif
    if (allocated(wInitial)) then
      deallocate(wInitial, STAT=ierr)
      if (ierr /= 0) &
           call StopError('Not able to de-allocate wInitial in uv_solver')
    endif
    allocate(wInitial(nc), STAT=ierr)
    if (ierr /= 0) &
         call StopError('Not able to allocate wInitial in uv_solver')

    wInitial = w
  end subroutine enableCustumStabCalc

  !-------------------------------------------------------------------------
  !> Disable additional phase stability check
  !>
  !> \author MH, 2014-01
  !-------------------------------------------------------------------------
  subroutine disableCustumStabCalc()
    implicit none
    ! Locals
    integer :: ierr

    doCustomStabCheck = .false.
    if (allocated(wInitial)) then
      deallocate(wInitial, STAT=ierr)
      if (ierr /= 0) &
           call StopError('Not able to de-allocate wInitial in uv_solver')
    endif
  end subroutine disableCustumStabCalc

  !-------------------------------------------------------------------------
  !> Additional phase stability check
  !>
  !> \author MH, 2014-01
  !-------------------------------------------------------------------------
  function custumStabCalc(t,p,Z,FUGZ,X,Y,beta,betaL,phase) result(isStable)
    use stability, only: stabcalcW
    implicit none
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< Overall composition [mol/mol]
    real, dimension(nc), intent(in) :: FUGZ !< Single phase fugacities [-]
    real, dimension(nc), intent(out) :: X !< Liquid composition [mol/mol]
    real, dimension(nc), intent(out) :: Y !< Gas composition [mol/mol]
    real, intent(out) :: beta !< Gas phase fraction [mol/mol]
    real, intent(out) :: betaL !< Liquid phase fraction [mol/mol]
    integer, intent(out) :: phase !< Phase identifyer [-]
    logical :: isStable
    ! Locals
    real :: tpd
    logical :: phase_stab_negative
    real, dimension(nc) :: K,FUG,W
    integer, parameter :: nd = 1, j = 1
    real, dimension(nd,nc) :: XX
    isStable = .true.
    if (doCustomStabCheck) then
      XX(1,:) = Z
      W = wInitial
      tpd = stabcalcW(nd,j,t,p,XX,W,custumPhase,FUGZ,FUG)
      phase_stab_negative = (tpd < stabilityLimit)
      ! Do we need to introduce another phase?
      if (phase_stab_negative) then
        if (custumPhase == VAPPH) then
          K = exp(FUGZ-FUG)
        else ! LIQPH
          K = exp(FUG-FUGZ)
        endif
        phase = TWOPH
        isStable = .false.
        if (.not. rr_solve(nc,Z,K,beta,X,Y,.false.,betaL)) then
          beta = -1.0
          betaL = -1.0
        endif
      endif
    endif
  end function custumStabCalc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Single-Phase flash!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-------------------------------------------------------------------------
  !> Do UV-flash for single component
  !> \todo Need handling of solutions close to critical point
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine twoPhaseUVsingleComp(t,p,Z,beta,betaL,X,Y,uspec,vspec,&
       phase,ierr)
    use thermo_utils, only: maxComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr !< Phase identifier
    ! Locals
    real :: t0, p0, beta0, betaL0, Tmax, Tmin
    integer :: phaseVec(2), i, imax(1)
    logical :: isConverged
    type(thermo_model), pointer :: act_mod_ptr
    !
    act_mod_ptr => get_active_thermo_model()
    if (present(ierr)) then
      ierr = 0
    endif
    Tmin = tpTmin
    Tmax = tpTmax
    imax = maxloc(Z)
    Tmin = max(act_mod_ptr%comps(imax(1))%p_comp%ttr, Tmin) ! Limit to triple point
    if (t > Tmax .OR. t < Tmin) then
      t = 0.5*(Tmax+Tmin)
    endif
    if (p > tpPmax .OR. p < tpPmin) then
      p = 0.1*(tpPmax+tpPmin)
    endif
    t0 = t
    p0 = p
    beta0 = beta
    betaL0 = betaL
    if (phase == SINGLEPH) then
      phase = LIQPH
    endif

    phaseVec(1) = phase
    if (phase == TWOPH) then
      phaseVec(2) = SINGLEPH
    else
      phaseVec(2) = TWOPH
    endif
    isConverged = .false.
    do i=1,2
      phase = phaseVec(i)
      if (phase == TWOPH) then
        t = t0
        p = p0
        call singleCompUV(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,isConverged)
        if (isConverged) then
          exit ! exit loop
        endif
      else
        ! Single phase
        t = t0
        p = p0
        call singleCompUV_Tv(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,isConverged)
        if (isConverged) then
          exit ! exit loop
        endif
      endif
    enddo
    if (.not. isConverged) then
      if (present(ierr)) then
        ierr = 1
      else
        print *,'Initial temperature',t0
        print *,'Initial pressure',p0
        print *,'Internal energy',uspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        print *,'Gas phase mol fraction',beta0
        print *,'Initial phase',phaseVec(1)
        call stoperror('uv_solver::twoPhaseUVsingleComp: No convergence')
      endif
    endif
  end subroutine twoPhaseUVsingleComp

  !-------------------------------------------------------------------------
  !> Do UV-flash for single component
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine singleCompUV(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,isConverged)
    use eos, only: getCriticalParam, specificVolume, enthalpy
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         premterm_at_dx_zero, setXv, limit_dx
    use thermo_utils, only: maxComp
    use saturation, only: safe_dewP, dewP
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    logical, optional, intent(out) :: isConverged
    ! Locals
    real, dimension(nc+7) :: param !< Parameter vector
    real :: tci,pci,oi,dpdt,hg,hl,vg,vl, Tmin, Tmax
    real :: t0, x0, var(2), xmin(2), xmax(2), F(2)
    type(nonlinear_solver) :: solver

    isConverged = .true.
    param(1) = uspec
    param(2) = vspec
    param(3:nc+2) = Z(1:nc)
    ! Get critical point
    call getCriticalParam(maxComp(Z),tci,pci,oi)
    param(nc+3) = pci
    param(nc+4) = tci
    Tmin = tpTmin
    Tmax = tpTmax
    !t = max(min(t, tci - 20.0), tpTmin + 50.0)
    if (t > tci - 5.0 .OR. t < Tmin + 10.0) then
      t = (tci + Tmin + 50.0)*0.5
    endif
    var(1) = t
    var(2) = min(max(beta,0.0),1.0)
    t0 = t
    x0 = var(2)
    p = safe_dewP(t,X,Z)
    param(nc+5) = p
    param(nc+6) = t
    call specificVolume(t,p,z,VAPPH,vg)
    call enthalpy(t,p,z,VAPPH,hg)
    call specificVolume(t,p,z,LIQPH,vl)
    call enthalpy(t,p,z,LIQPH,hl)
    ! Use Clapeyron's equation
    dpdt = (hg-hl)/((vg-vl)*T)
    param(nc+7) = dpdt

    solver%abs_tol = singleuv_tolerance
    solver%ls_max_it = singleuv_line_searches
    solver%max_it = singleuv_nmax

    xmin(1) = Tmin
    xmax(1) = tci - 1.0e-2
    xmin(2) = -2.0
    xmax(2) = 3.0
    call nonlinear_solve(solver,fun_two_single,jac_two_single,jac_two_single,limit_dx_two,&
         premterm_at_dx_zero,setXv,var,xmin,xmax,param)
    beta = var(2)
    phase = TWOPH
    if (beta < 0.0 .or. beta > 1.0) then
      ! Test for single phase solution at saturation line
      if (beta < 0.0) then
        var(2) = 0.0
        phase = LIQPH
      else
        var(2) = 1.0
        phase = VAPPH
      endif
      call fun_two_single(F,var,param)
      if (sqrt(dot_product(F,F)) > singleuv_tolerance) then
        solver%exitflag = -1
      else
        beta = var(2)
      endif
    endif
    if (solver%exitflag == 0) then
      t = var(1)
      p = dewP(T,param(nc+5),X,Z)
      Y = Z
      X = Z
      betaL = 1.0 - beta
    else
      if (present(isConverged)) then
        isConverged = .false.
        if (var(2) > 0.5) then
          phase = VAPPH
        else
          phase = LIQPH
        endif
      else
        print *,'Tolerance: ',solver%abs_tol
        print *,'Temperature',var(1)
        print *,'beta',var(2)
        print *,'Initial temperature',t0
        print *,'Initial gas phase fraction',x0
        print *,'Internal energy',uspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        call stoperror('uv_solver::twoPhaseUVsingleComp: The nonlinear optimizer did not converge')
      endif
    endif
  end subroutine singleCompUV

  !-------------------------------------------------------------------------
  !> Limit change in x, dx, to keep x between extreme values:
  !> xmin <= x <= xmax
  !> |dx| <= 50.0
  !> \author MH, October 2015
  !-------------------------------------------------------------------------
  subroutine limit_dx_two(n,x,xmin,xmax,dx,np,param)
    implicit none
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: x,xmin,xmax
    real, dimension(n),     intent(inout) :: dx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: param
    real :: scaling
    integer :: i
    !
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
    ! Avoid going to limits in T, when not required
    if (abs(dx(1)) > 50.0) then
      scaling = min(scaling,50.0/abs(dx(1)))
    endif
    if (scaling < 1.0) then
      dx = dx * scaling
    endif
    !
  end subroutine limit_dx_two

  !-------------------------------------------------------------------------
  !> Calculate residual for single component two-phase UV system
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine fun_two_single(f,var,param)
    use eos, only: specificVolume, enthalpy
    use saturation, only: safe_dewP, dewP
    implicit none
    real, dimension(2), intent(out) :: f !< Function values
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+7), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: t, p, h, uspec, vspec, v, beta, hg, hl, vg, vl
    real :: t_old, p_old, p_c, t_c, dpdt, u
    real, dimension(nc) :: Z, X
    !
    t = var(1)
    beta = var(2)
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    p_c = param(nc+3)
    t_c = param(nc+4)
    p_old = param(nc+5)
    t_old = param(nc+6)
    dpdt = param(nc+7)
    ! Extrapolate along saturation line
    p = p_old + (t-t_old)*dpdt
    p = min(p,p_c)
    ! Find saturation pressure
    if (t > 0.95*t_c .or. abs(t-t_old) > 10.0) then
      p = safe_dewP(T,X,Z)
    else
      p = dewP(T,p,X,Z)
    endif
    call specificVolume(t,p,z,VAPPH,vg)
    call enthalpy(t,p,z,VAPPH,hg)
    call specificVolume(t,p,z,LIQPH,vl)
    call enthalpy(t,p,z,LIQPH,hl)
    ! Use Clapeyron's equation
    dpdt = (hg-hl)/((vg-vl)*T)
    param(nc+5) = p
    param(nc+6) = t
    param(nc+7) = dpdt
    !
    h = beta*hg+(1.0-beta)*hl
    v = beta*vg+(1.0-beta)*vl

    u = h - p*v
    f(1) = (u - uspec)/(T)
    f(2) = (v - vspec)/vspec

    !f(1)= (uspec+p*vspec-h)/(Rgas*t)
    !f(2)= p*(v-vspec)/(Rgas*t)
  end subroutine fun_two_single

  !-------------------------------------------------------------------------
  !> Calculate differential for single component two-phase UV system.
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine jac_two_single(Jac,var,param)
    use saturation, only: safe_dewP, dewP
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+7), intent(inout) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: t, p, uspec, vspec, beta, hg, hl, vg, vl, ul, ug
    real :: dvdtg, dvdpg, dvdtl, dvdpl
    real :: dhdtg, dhdpg, dhdtl, dhdpl, dpdt, dugdt, duldt
    real, dimension(nc) :: Z, X
    real :: t_old, p_old, p_c, t_c
    !real :: dvdt, dhdt, denumFact, h, v

    t = var(1)
    beta = var(2)
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    p_c = param(nc+3)
    t_c = param(nc+4)
    p_old = param(nc+5)
    t_old = param(nc+6)
    dpdt = param(nc+7)
    ! Extrapolate along saturation line
    p = p_old + (t-t_old)*dpdt
    p = min(p,p_c)
    ! Find saturation pressure
    if (t > 0.95*t_c .or. abs(t-t_old) > 10.0) then
      p = safe_dewP(T,X,Z)
    else
      p = dewP(T,p,X,Z)
    endif
    call specificVolume(t,p,z,VAPPH,vg,dvdt=dvdtg,dvdp=dvdpg)
    call enthalpy(t,p,z,VAPPH,hg,dhdt=dhdtg,dhdp=dhdpg)
    call specificVolume(t,p,z,LIQPH,vl,dvdt=dvdtl,dvdp=dvdpl)
    call enthalpy(t,p,z,LIQPH,hl,dhdt=dhdtl,dhdp=dhdpl)
    ! Use Clapeyron's equation
    dpdt = (hg-hl)/((vg-vl)*T)
    param(nc+5) = p
    param(nc+6) = t
    param(nc+7) = dpdt
    !
    ! h = beta*hg+(1.0-beta)*hl
    ! v = beta*vg+(1.0-beta)*vl
    ! dhdt = beta*(dhdtg + dhdpg*dpdt)+(1.0-beta)*(dhdtl + dhdpl*dpdt)
    ! dvdt = beta*(dvdtg + dvdpg*dpdt)+(1.0-beta)*(dvdtl + dvdpl*dpdt)
    !f(1)= (uspec+p*vspec-h)/(Rgas*t)
    !f(2)= p*(v-vspec)/(Rgas*t)
    !
    ! denumFact = 1.0/(T*Rgas)
    ! Jac(1,1) = (dpdt*vspec-dhdt)*denumFact
    ! Jac(1,2) = -(hg-hl)*denumFact
    ! Jac(2,1) = p*dvdt*denumFact
    ! Jac(2,2) = p*(vg-vl)*denumFact

    dhdtg = dhdtg + dhdpg*dpdt
    dhdtl = dhdtl + dhdpl*dpdt
    dvdtg = dvdtg + dvdpg*dpdt
    dvdtl = dvdtl + dvdpl*dpdt

    dugdt = dhdtg - p*dvdtg - dpdt*vg
    duldt = dhdtl - p*dvdtl - dpdt*vl
    ul = hl - p*vl
    ug = hg - p*vg

    Jac(1,1) = (beta*dugdt + (1.0-beta)*duldt)/(T)
    Jac(1,2) = (ug-ul)/(T)
    Jac(2,1) = (beta*dvdtg + (1.0-beta)*dvdtl)/vspec
    Jac(2,2) = (vg-vl)/vspec

  end subroutine jac_two_single

  !-------------------------------------------------------------------------
  !> Do UV-flash for single component single phase. Use U(T,v)
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine singleCompUV_Tv(t,p,Z,beta,betaL,X,Y,uspec,vspec,phase,isConverged)
    use eos, only: getCriticalParam, specificVolume, enthalpy
    use eosTV, only: pressure
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         premterm_at_dx_zero, setXv, limit_dx
    use thermo_utils, only: maxComp
    use saturation, only: safe_dewP, dewP
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: uspec !< Specified internal energy [J/mol]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    logical, optional, intent(out) :: isConverged
    ! Locals
    real, dimension(nc+2) :: param !< Parameter vector
    real, dimension(nc) :: FUGZ
    real :: t0, var(1), xmin(1), xmax(1), v
    type(nonlinear_solver) :: solver
    integer :: sphase

    if (present(isConverged)) then
      isConverged = .true.
    endif
    param(1) = uspec
    param(2) = vspec
    param(3:nc+2) = Z(1:nc)
    var(1) = t

    solver%abs_tol = singleuv_tolerance
    solver%ls_max_it = singleuv_line_searches
    solver%max_it = singleuv_nmax

    xmin(1) = tpTmin
    xmax(1) = tpTmax
    call nonlinear_solve(solver,fun_Tv_single,jac_Tv_single,jac_Tv_single,limit_dx,&
         premterm_at_dx_zero,setXv,var,xmin,xmax,param)

    t = var(1)
    if (solver%exitflag == 0) then
      ! Check for positive pressure
      p = pressure(t,vspec,Z)
      if (p < tpPmin .or. p > tpPmax) then
        solver%exitflag = 1 ! False solution
      else
        call thermo(t,p,Z,MINGIBBSPH,FUGZ,ophase=phase,v=v)
        if (phase == VAPPH) then
          beta = 1.0
          betaL = 0.0
        else if (phase == LIQPH) then
          beta = 0.0
          betaL = 1.0
        else
          beta = -1.0
          betaL = -1.0
        endif
        X = Z
        Y = Z
        sphase = phase
        if (sphase == SINGLEPH) then
          sphase = LIQPH
        endif
        if (abs(v-vspec)/vspec > solver%abs_tol) then
          solver%exitflag = 1 ! False solution?
        endif
      endif
    endif
    if (solver%exitflag /= 0) then
      if (present(isConverged)) then
        isConverged = .false.
      else
        print *,'Tolerance: ',solver%abs_tol
        print *,'Temperature',var(1)
        print *,'Initial temperature',t0
        print *,'Internal energy',uspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        call stoperror('uv_solver::singleCompUV_Tv: The nonlinear optimizer did not converge')
      endif

    endif
  end subroutine singleCompUV_Tv

  !-------------------------------------------------------------------------
  !> Calculate residual for one-phase UV system. U(T,v) formulation.
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine fun_Tv_single(f,var,param)
    use eosTV, only: internal_energy_tv
    implicit none
    real, dimension(1), intent(out) :: f !< Function values
    real, dimension(1), intent(in) :: var !< Variable vector
    real, dimension(nc+2), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: t, u, uspec, vspec
    real, dimension(nc) :: Z
    !
    t = var(1)
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)

    call internal_energy_tv(t,vspec,Z,u)

    f(1)= (u-uspec)/max(1.0,abs(uspec))
  end subroutine fun_Tv_single

  !-------------------------------------------------------------------------
  !> Calculate differential for one-phase UV system. U(T,v) formulation.
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  subroutine jac_Tv_single(Jac,var,param)
    use eosTV, only: internal_energy_tv
    implicit none
    real, dimension(1), intent(in) :: var !< Variable vector
    real, dimension(nc+2), intent(inout) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: t, u, uspec, dudt, vspec
    real, dimension(nc) :: Z

    t = var(1)
    uspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    call internal_energy_tv(t,vspec,Z,u,dudt)
    !
    !f(1)=  (u-uspec)/max(1.0,abs(uspec))
    !
    Jac(1,1) =  dudt/max(1.0,abs(uspec))
  end subroutine jac_Tv_single

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Solver properties !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-------------------------------------------------------------------------
  !> Set tolerance for nested loop UV flash.
  !>
  !> \author MH, 2015
  !-------------------------------------------------------------------------
  subroutine setNestedUVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(in) :: tolerance !< Solver tolerance
    integer, intent(in) :: nmax !< Maximum number of iteretions
    integer, intent(in) :: linesearch_nmax !< Maximum number of line-searches
    !
    nested_tolerance = tolerance
    nested_line_searches = linesearch_nmax
    nested_nmax = nmax
  end subroutine setNestedUVtolerance

  !-------------------------------------------------------------------------
  !> Get tolerance for nested loop UV flash.
  !>
  !> \author MH, 2015
  !-------------------------------------------------------------------------
  subroutine getNestedUVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(out) :: tolerance !< Solver tolerance
    integer, intent(out) :: nmax !< Maximum number of iteretions
    integer, intent(out) :: linesearch_nmax !< Maximum number of line-searches
    !
    tolerance = nested_tolerance
    linesearch_nmax = nested_line_searches
    nmax = nested_nmax
  end subroutine getNestedUVtolerance

  !-------------------------------------------------------------------------
  !> Set tolerance for fulleq. UV flash.
  !>
  !> \author MH, 2015
  !-------------------------------------------------------------------------
  subroutine setFullEqUVtolerance(tolerance,nmax,linesearch_nmax,gibbs_tolerance)
    implicit none
    real, intent(in) :: tolerance !< Solver tolerance
    integer, intent(in) :: nmax !< Maximum number of iteretions
    integer, intent(in) :: linesearch_nmax !< Maximum number of line-searches
    real, optional, intent(in) :: gibbs_tolerance !< Tolerance for when to accept two-phase solutions
    !
    fulleq_tolerance = tolerance
    fulleq_line_searches = linesearch_nmax
    fulleq_nmax = nmax
    if (present(gibbs_tolerance)) then
      g_tolerance = gibbs_tolerance
    endif
  end subroutine setFullEqUVtolerance

  !-------------------------------------------------------------------------
  !> Get tolerance for fulleq. UV flash.
  !>
  !> \author MH, 2015
  !-------------------------------------------------------------------------
  subroutine getFullEqUVtolerance(tolerance,nmax,linesearch_nmax,gibbs_tolerance)
    implicit none
    real, intent(out) :: tolerance !< Solver tolerance
    integer, intent(out) :: nmax !< Maximum number of iteretions
    integer, intent(out) :: linesearch_nmax !< Maximum number of line-searches
    real, optional, intent(out) :: gibbs_tolerance !< Tolerance for when to accept two-phase solutions
    !
    tolerance = fulleq_tolerance
    linesearch_nmax = fulleq_line_searches
    nmax = fulleq_nmax
    if (present(gibbs_tolerance)) then
      gibbs_tolerance = g_tolerance
    endif
  end subroutine getFullEqUVtolerance

  !-------------------------------------------------------------------------
  !> Set tolerance for single component UV flash.
  !>
  !> \author MH, 2015
  !-------------------------------------------------------------------------
  subroutine setSingleCompUVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(in) :: tolerance !< Solver tolerance
    integer, intent(in) :: nmax !< Maximum number of iteretions
    integer, intent(in) :: linesearch_nmax !< Maximum number of line-searches
    !
    singleuv_tolerance = tolerance
    singleuv_line_searches = linesearch_nmax
    singleuv_nmax = nmax
  end subroutine setSingleCompUVtolerance

  !-------------------------------------------------------------------------
  !> Get tolerance for single component UV flash.
  !>
  !> \author MH, 2015
  !-------------------------------------------------------------------------
  subroutine getSingleCompUVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(out) :: tolerance !< Solver tolerance
    integer, intent(out) :: nmax !< Maximum number of iteretions
    integer, intent(out) :: linesearch_nmax !< Maximum number of line-searches
    !
    tolerance = singleuv_tolerance
    linesearch_nmax = singleuv_line_searches
    nmax = singleuv_nmax
  end subroutine getSingleCompUVtolerance

end module uv_solver
