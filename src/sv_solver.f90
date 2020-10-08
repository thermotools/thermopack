!-----------------------------------------------------------------------------
!> Calculate solve SV-flash for single phase gas/liquid or a
!! gas-liquid mixture.
!!
!! \todo Need trace-component functionallity.
!! \todo Consider merging with UV-flash
!!
!-----------------------------------------------------------------------------
module sv_solver
  !
  !
  use numconstants, only: machine_prec, small
  use thermopack_constants
  use thermopack_var, only: nc, nph, get_active_thermo_model, thermo_model
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
  real :: nested_tolerance = 5.0e7*machine_prec
  integer :: nested_line_searches = 5
  integer :: nested_nmax = 200

  !> Properties for full equation system solver
  real :: fulleq_tolerance = 1.0e4*machine_prec
  integer :: fulleq_line_searches = 4
  integer :: fulleq_nmax = 100

  !> Single component solver
  real :: singlesv_tolerance = 5.0e5*machine_prec
  integer :: singlesv_line_searches = 3
  integer :: singlesv_nmax = 100

  public :: twoPhaseSVflash, twoPhaseSVflashNested, twoPhaseSVflashFull
  public :: enableCustumStabCalc, disableCustumStabCalc
  public :: setNestedSVtolerance, setFullEqSVtolerance
  public :: getNestedSVtolerance, getFullEqSVtolerance
  public :: setSingleCompSVtolerance, getSingleCompSVtolerance
  public :: twoPhaseSVsingleComp
  public :: singleCompSV_Tv, fun_1ph_sv, jac_1ph_sv

contains

  !-----------------------------------------------------------------------------
  !> Do SV-flash:
  !> Switch for multicomponent (mc) and single component solver.
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine twoPhaseSVflash(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,ierr)
    use thermo_utils, only: isSingleComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr !< Integer error flag
    !
    if (isSingleComp(Z)) then
      if (present(ierr)) then
        ierr = 0
      endif
      call twoPhaseSVsingleComp(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase)
    else
      call twoPhaseSVflash_mc(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,ierr)
    endif
    !
  end subroutine twoPhaseSVflash

  !-----------------------------------------------------------------------------
  !> Do multicomponent SV-flash:
  !> Try solving all equations at once, for given initial values.
  !> Switch to nested loop if it fails to converge.
  !>
  !> Assume initial values for specified phase.
  !>
  !> \author MH, 2012-08-15
  !-----------------------------------------------------------------------------
  subroutine twoPhaseSVflash_mc(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,ierr)
    use thermo_utils, only: wilsonK
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr !< Integer error flag
    ! Locals
    logical :: converged, tryNewPhase, nestedIsConverged
    real :: beta0, t0, p0, betaL0
    real, dimension(nc) :: X0, Y0, K
    integer :: ph0, ph1
    !
    if (present(ierr)) then
      ierr = 0
    endif
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
    call twoPhaseSVflashFull(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,converged)
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
        call twoPhaseSVflashFull(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,&
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
            call twoPhaseSVflashFull(t,p,Z,beta,betaL,X,Y,sspec,vspec,&
                 phase,converged)
          endif
        endif
      endif
    endif

    if (.not. converged) then
      t = t0
      p = p0
      call twoPhaseSVflashNested(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,&
           nestedIsConverged)
      if (.not. nestedIsConverged) then
        ! Too slow convergence in netsed loop iteration. Restart full solver.
        call twoPhaseSVflashFull(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,&
             converged)
        ! Solve for single phase at large pressure
        if (.not. converged) then
          call singleCompSV_Tv(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,converged,pmax=1.0e13)
          if (p < tpPmax - 1.0e5) then
            print *,'sv_solver::twoPhaseSVflash_mc - '//&
                 'singleCompSV_Tv solution used without stability check'
          endif
        endif
        if (.not. converged) then
          if (present(ierr)) then
            ierr = 1
          else
            print *,'Temperature',t
            print *,'Pressure',p
            print *,'Initial temperature',t0
            print *,'Initial pressure',p0
            print *,'Entropy',sspec
            print *,'Specific volume',vspec
            print *,'Comp.',Z
            print *,'X0 ', X0
            print *,'Y0 ', Y0
            print *,'Phase0 ', ph0
            print *,'beta0 ', beta0
            print *,'betaL0 ', betaL0
            call stoperror('sv_solver::twoPhaseSVflash: SV-flash did not converge.')
          endif
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
  end subroutine twoPhaseSVflash_mc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Nested-loop !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !> Do SV-flash using PT-flash in nested loop
  !>
  !> Function to minimize:
  !>  \f$ -\frac{g_{min} + T S_{spec} - p v_{spec}}{R} \f$.
  !>
  !> \author MH, 2015-02
  !-----------------------------------------------------------------------------
  subroutine twoPhaseSVflashNested(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,isConverged)
    use optimizers, only: optimize, optim_param
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    logical, optional, intent(out) :: isConverged
    ! Locals
    type(optim_param) :: optim
    real, dimension(2) :: var
    real, dimension(nc+6) :: param
    real :: tInitial, pInitial
    !
    if (present(isConverged)) then
      isConverged = .true.
    endif
    if (t > tpTmax .OR. t < tpTmin) then
      t = 0.5*(tpTmax+tpTmin)
    endif
    if (p > tpPmax .OR. p < tpPmin) then
      p = 0.1*(tpPmax+tpPmin)
    endif
    tInitial = t
    pInitial = p
    !
    ! Do modified newton search
    param(1) = sspec
    param(2) = vspec
    param(3:nc+2) = Z
    param(nc+3:nc+4) = 1.0 ! Scaling
    param(nc+5) = 0.0 ! Iteration index
    param(nc+6) = 1.0 ! Enable line search
    var(1) = log(t) ![K]
    var(2) = log(p) ![Pa]
    optim%rel_tol = nested_tolerance
    optim%max_line_search_iter = nested_line_searches
    optim%gradient_termination = .true.
    optim%max_iter = nested_nmax
    optim%line_search_control = .true.
    call optimize(optim,objective,differentials,var,param,&
         limitDvar,prematureReturn,get_problem_size,setX_nested,&
         error_fun=error_fun_nested)
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
        print *,'Entropy',sspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        print *,'Iterations used: ',optim%iter
        call stoperror('sv_solver::twoPhaseSVflashNested: The nonlinear optimizer did not converge')
      endif
    endif
    !
    ! Set solution
    t = exp(var(1))
    p = exp(var(2))
    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)

  end subroutine twoPhaseSVflashNested

  subroutine setX_nested(n,nparam,X,dX,param,alpha)
    implicit none
    integer, intent(in) :: n, nparam !< Problem dimension
    real, dimension(n), intent(inout) :: X !< Variables
    real, dimension(n), intent(in)    :: dX !< Change in variables
    real, dimension(nparam), intent(inout) :: param !< Parameter vector
    real, intent(in) :: alpha !< dX scaling
    !
    real :: r !< random number
    real :: base_step
    !
    r = modulo(exp(X(1))*1.0e4, 300.0)/300.0 ! pseudo random number
    if (int(param(nparam)) .eq. 0) then ! Override line-search
      if (param(nc+5) > 50.0) then
        ! Reduce step after 50 iterations
        base_step = 0.5 - 0.4*max(0.0,min(1.0,(param(nc+5)-50.0)/50.0))
      else
        base_step = 0.5
      endif
      X(1:n) = X(1:n) + dX(1:n)*(base_step+0.5*r)
    else
      X(1:n) = X(1:n) + alpha*dX(1:n)*(0.80+0.20*r)
    endif
  end subroutine setX_nested

  !-----------------------------------------------------------------------------
  !> Calculate state function for SV system
  !>
  !> \author MH, 2012-07-06
  !-----------------------------------------------------------------------------
  function objective(var,param) result(of)
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+6), intent(in) :: param !< Parameter vector
    real :: of !< Objective function value
    ! Locals
    real:: beta, t, p, g, s, h, sspec, vspec, sg, sl, hg, hl, betaL
    real, dimension(nc) :: Z,X,Y
    integer :: phase, sphase
   !
    t = exp(var(1))
    p = exp(var(2))
    sspec = param(1)
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
    of = -(g + T*sspec - p*vspec)/Rgas
  end function objective

  !-----------------------------------------------------------------------------
  !> Calculate state function for PH system and its
  !> differentials.
  !>
  !> \author MH, 2012-07-06
  !-----------------------------------------------------------------------------
  subroutine differentials(var,param,of,dOFdvar,Hof)
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+6), intent(inout) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: Hof !< Hessian matrix of objective function
    real, dimension(2), intent(out) :: dOFdvar !< Differential of objective function with respect to temperature
    real, intent(out) :: of !< Objective function value
    ! Locals
    real :: beta, t, p, g, s, h, v, sspec, vspec, sg, sl, hg, hl, vg, vl, betaL
    real :: dvdp, dvdt, dhdt, scaling(2)
    real, dimension(nc) :: Z,X,Y
    integer :: phase, sphase
    !real :: det, landa_min, tau, tr
    !real, parameter :: delta = machine_prec*5.0e3
    t = exp(var(1))
    p = exp(var(2))
    sspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    param(nc+5) = param(nc+5) + 1.0 ! Increase iteration count

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
    of = -(g + T*sspec - p*vspec)/Rgas
    dOFdvar(1) = T*(s - sspec)/Rgas
    dOFdvar(2) = -p*(v - vspec)/Rgas
    scaling(1) = T*max(abs(sspec),1.0)/Rgas
    scaling(2) = p*vspec/Rgas
    Hof(1,1) = T*dhdt/Rgas
    Hof(1,2) = -p*T*dvdt/Rgas
    Hof(2,1) = Hof(1,2)
    Hof(2,2) = -p**2*dvdp/Rgas
    param(nc+3:nc+4)=scaling
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
    real, dimension(nc+6), intent(inout) :: param !< Parameter vector
    real, dimension(2), intent(inout) :: dvar !< Calculated change in variables
    ! Locals
    real :: maxstep_t
    real :: maxstep_p
    real :: factor, T0, P0, T1, P1
    real, dimension(2) :: dvar_tp
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
    maxstep_t = min(25.0, T0*0.7)
    maxstep_p = min(1.0e6, P0*0.7)
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
    if (dvar(1) > 0.0 .AND. T1 > tpTmax*2.0) then
      factor = 1.0/dvar(1)
      dvar(1) = log(tpTmax) - var(1)
      factor = factor*dvar(1)
    endif
    if (dvar(1) < 0.0 .AND. T1 < tpTmin) then
      factor = 1.0/dvar(1)
      dvar(1) = log(tpTmin) - var(1)
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
    if (dvar(2) > 0.0 .AND. P1 > tpPmax*2.0) then
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
    if ( (abs(dvar_tp(1)) .gt. maxstep_t) .or. &
         (abs(dvar_tp(2)) .gt. maxstep_p)) then
      param(nc+6) = 0.0 ! Disable line-search
    else
      param(nc+6) = 1.0 ! Do line search
    endif
  end subroutine limitDvar

  !-----------------------------------------------------------------------------
  !> Premature termination when solution is outside valid temperature region.
  !>
  !> \author MH, 2012-07-06
  !-----------------------------------------------------------------------------
  function prematureReturn(var,param,of,dofdvar) result(doReturn)
    implicit none
    real, dimension(2), intent(in)      :: var !< Variables
    real, dimension(nc+6), intent(in)   :: param !< Parameter vector
    real, intent(in)                    :: of !< Objective function value
    real, dimension(2), intent(in)      :: dofdvar !< Differential of objective function
    logical                             :: doReturn !< Terminate minimization?
    ! Locals

    doReturn = .false.

  end function prematureReturn

  !----------------------------------------------------------------------------
  !> Support for variable size in problem
  !>
  !> \author MH, 2012-07-06
  !-----------------------------------------------------------------------------
  function get_problem_size(param) result(nvar)
    implicit none
    real, dimension(nc+6), intent(in) :: param !< Parameter vector
    integer :: nvar

    nvar = 2

  end function get_problem_size

  !-----------------------------------------------------------------------------
  !> Calculated scaled error
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  function error_fun_nested(n,X,nparam,param,of,dofdX,of_old) result(error)
    implicit none
    integer, intent(in)                 :: n !< Dimension of X
    real, dimension(n), intent(in)      :: X !< Variables
    integer, intent(in)                 :: nparam !< Dimension of param
    real, dimension(nparam), intent(in) :: param !< Parameter vector
    real, intent(in)                    :: of !< Objective function value
    real, intent(in)                    :: of_old !< Old objective function value
    real, dimension(n), intent(in)      :: dofdX !< Differential of objective function
    real                                :: error !< Calculated error
    ! Locals
    real :: abs_dofdX(n), scaling(n)
    integer :: imax(1)
    abs_dofdX = abs(dOFdX)
    scaling = param(nc+3:nc+4)
    abs_dofdX = abs_dofdX/scaling
    imax = maxloc(abs_dOFdX)
    error = abs_dofdX(imax(1))
  end function error_fun_nested


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Full NR-flash !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !> Do SV-flash using full equation system
  !>
  !> Assume initial values for specified phase.
  !>
  !> \author MH, 2012-08-15
  !-----------------------------------------------------------------------------
  subroutine twoPhaseSVflashFull(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,converged)
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
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
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
    real :: g_feed, tpd, g_mix
    logical :: liq_stab_negative, gas_stab_negative
    !
    converged = .false.
    if (t > tpTmax .OR. t < tpTmin) then
      t = 0.5*(tpTmax+tpTmin)
    endif
    if (p > tpPmax .OR. p < tpPmin) then
      p = 0.1*(tpPmax+tpPmin)
    endif
    !
    ! Do modified newton search
    param(1) = sspec
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
      call stoperror('sv_solver::twoPhaseSVflashFull: Wrong phase indicator.')
    endif

    ! Set solver parameters
    solver%abs_tol = fulleq_tolerance
    solver%ls_max_it = fulleq_line_searches
    solver%max_it = fulleq_nmax
    solver%symmetric_jac = .true.
    if (nov == 2) then ! Try single phase
      param(nc+3) = phase
      xmin(1) = log(tpTmin)
      xmax(1) = log(tpTmax)
      xmin(2) = log(tpPmin)
      xmax(2) = log(tpPmax)
      call nonlinear_solve(solver,fun_1ph_sv,jac_1ph_sv,jac_1ph_sv,limit_dx,&
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
      xmin(1+nc) = log(tpTmin)
      xmax(1+nc) = log(tpTmax)
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
  end subroutine twoPhaseSVflashFull

  !-----------------------------------------------------------------------------
  !> Calculate state function for SV system
  !>
  !> \author MH, 2012-08-15
  !-----------------------------------------------------------------------------
  subroutine fun_1ph_sv(f,var,param)
    implicit none
    real, dimension(2), intent(out) :: f !< Function values
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+3), intent(in) :: param !< Parameter vector
    ! Locals
    real:: t, p, s, sspec, vspec, v
    real, dimension(nc) :: Z
    integer :: phase
    !
    t = exp(var(1))
    p = exp(var(2))
    sspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = INT(param(nc+3))
    !
    call specificVolume(t,p,z,phase,v)
    call entropy(t,p,z,phase,s)
    f(1)= (sspec-s)/Rgas
    f(2)= p*(v-vspec)/(Rgas*t)
  end subroutine fun_1ph_sv

  !-----------------------------------------------------------------------------
  !> Calculate state function for PH system and its
  !> differentials.
  !>
  !> \author MH, 2012-08-15
  !-----------------------------------------------------------------------------
  subroutine jac_1ph_sv(Jac,var,param)
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+3), intent(in) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: t, p, h, v, sspec, vspec
    real :: dvdp, dvdt, dhdt
    real, dimension(nc) :: Z
    integer :: phase
    t = exp(var(1))
    p = exp(var(2))
    sspec = param(1)
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
  end subroutine jac_1ph_sv

  !-----------------------------------------------------------------------------
  !> Calculate state function for SV system
  !>
  !> \author MH, 2012-08-15
  !-----------------------------------------------------------------------------
  subroutine fun_2ph(f,var,param)
    implicit none
    real, dimension(nc+2), intent(out) :: f !< Function values
    real, dimension(nc+2), intent(in) :: var !< Variable vector
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    ! Locals
    real:: betaV, t, p, sspec, vspec, sg, sl, vl, vg, s, v, betaL
    real, dimension(nc) :: Z,X,Y,FUGV,FUGL,L
    integer :: i
    real, parameter :: log_eps = machine_prec**5
    !
    t = exp(var(nc+1))
    p = exp(var(nc+2))
    sspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    betaV = sum(var(1:nc))
    Y = var(1:nc)/betaV
    L = param(nc+3:2*nc+2)
    betaL = sum(L)
    X = L/betaL
    !
    call specificVolume(t,p,Y,VAPPH,vg)
    call entropy(t,p,Y,VAPPH,sg)
    call specificVolume(t,p,X,LIQPH,vl)
    call entropy(t,p,X,LIQPH,sl)
    s = betaV*sg+betaL*sl
    v = betaV*vg+betaL*vl
    f(nc+1)= (sspec-s)/Rgas
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

  !-----------------------------------------------------------------------------
  !> Calculate state function for PH system and its
  !> differentials.
  !>
  !> \author MH, 2012-08-15
  !-----------------------------------------------------------------------------
  subroutine jac_2ph(Jac,var,param)
    implicit none
    real, dimension(nc+2), intent(in) :: var !< Variable vector
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    real, dimension(nc+2,nc+2), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: betaV, t, p, sspec, vspec, betaL
    real, dimension(2) :: spec
    real, dimension(nc+2) :: RHS
    real, dimension(nc) :: Z,X,Y,L
    !
    t = exp(var(nc+1))
    p = exp(var(nc+2))
    sspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    betaV = sum(var(1:nc))
    Y = var(1:nc)/betaV
    L = param(nc+3:2*nc+2)
    betaL = sum(L)
    X = L/betaL
    spec(1) = sspec
    spec(2) = vspec
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'SV',spec,Jac,RHS)
  end subroutine jac_2ph

  !-----------------------------------------------------------------------------
  !> Terminate search for two-phase solution
  !>
  !> \author MH, 2012-08-16
  !-----------------------------------------------------------------------------
  function premterm_2ph(var,dvar,param,n,np) result(premature_return)
    implicit none
    integer, intent(in)                :: n !< Dimension of X
    integer, intent(in)                :: np !< Dimension of param
    real, dimension(n), intent(in)     :: var !< Variable vector
    real, dimension(n), intent(inout)  :: dvar !< Change in variable vector
    real, dimension(np), intent(in)    :: param !< Parameter vector
    logical :: premature_return
    ! Locals
    real :: t,p,beta,betaL,Utwo,Usingle
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
      Utwo = twoPhaseInternalEnergy(t,p,z,x,y,beta,TWOPH,betaL)
      Usingle = twoPhaseInternalEnergy(t,p,z,x,y,beta,phase,betaL)

      ! call thermo(T,P,Z,phase,FUGZ)
      ! gSingle = sum(Z*(log(Z)+FUGZ))
      ! call thermo(t,p,y,VAPPH,fugG)
      ! call thermo(t,p,x,LIQPH,fugL)
      ! gMix = beta*sum(Y*(log(Y)+FUGG)) + betaL*sum(X*(log(X)+FUGL))

      if (Usingle < Utwo) then
        !print *,'Entropy',Stwo,Ssingle,phase,Stwo-Ssingle
        !print *,'Gibbs: ',gMix,gSingle,gMix-gSingle
        !print *,'Premature two phase SV termination'
        premature_return = .true.
      endif
    endif
    !
  end function premterm_2ph

  !-----------------------------------------------------------------------------
  !> Limit change in variables
  !>
  !> \author MH, 2013-04-18
  !-----------------------------------------------------------------------------
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

  !-----------------------------------------------------------------------------
  !> Set mole numbers. Try to avoid truncation error.
  !>
  !> \author MH, 2013-10-15
  !-----------------------------------------------------------------------------
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

  !-----------------------------------------------------------------------------
  !> Enable additional phase stability check
  !>
  !> \author MH, 2014-01
  !-----------------------------------------------------------------------------
  subroutine enableCustumStabCalc(w,phase)
    implicit none
    integer, intent(in) :: phase !< Phase identifyer
    real, dimension(nc), intent(in) :: W !< Initial comosition in stability calculation
    ! Locals
    integer :: ierr

    doCustomStabCheck = .true.
    custumPhase = phase
    if (phase /= VAPPH .OR. phase /= LIQPH) then
      call StopError('Wrong phase specified for custom phase stability in sv_solver')
    endif
    if (allocated(wInitial)) then
      deallocate(wInitial, STAT=ierr)
      if (ierr /= 0) &
           call StopError('Not able to de-allocate wInitial in sv_solver')
    endif
    allocate(wInitial(nc), STAT=ierr)
    if (ierr /= 0) &
         call StopError('Not able to allocate wInitial in sv_solver')

    wInitial = w
  end subroutine enableCustumStabCalc

  !-----------------------------------------------------------------------------
  !> Disable additional phase stability check
  !>
  !> \author MH, 2014-01
  !-----------------------------------------------------------------------------
  subroutine disableCustumStabCalc()
    implicit none
    ! Locals
    integer :: ierr

    doCustomStabCheck = .false.
    if (allocated(wInitial)) then
      deallocate(wInitial, STAT=ierr)
      if (ierr /= 0) &
           call StopError('Not able to de-allocate wInitial in sv_solver')
    endif
  end subroutine disableCustumStabCalc

  !-----------------------------------------------------------------------------
  !> Additional phase stability check
  !>
  !> \author MH, 2014-01
  !-----------------------------------------------------------------------------
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

  !-----------------------------------------------------------------------------
  !> Do SV-flash for single component
  !> \todo Need handling of solutions close to critical point
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine twoPhaseSVsingleComp(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,ierr)
    use thermo_utils, only: maxComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr !< Phase identifier
    ! Locals
    real :: t0, p0, beta0, betaL0
    integer :: phaseVec(2), i
    logical :: isConverged
    !
    if (present(ierr)) then
      ierr = 0
    endif
    if (t > tpTmax .OR. t < tpTmin) then
      t = 0.5*(tpTmax+tpTmin)
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
        if (beta < 0.0 .or. beta > 1.0 .or. betaL < 0.0 .or. betaL > 1.0) then
          beta = 0.5
          betaL = 1.0-beta
        endif
        call singleCompSV(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,isConverged=isConverged)
        if (isConverged) then
          exit ! exit loop
        endif
      else
        ! Single phase
        t = t0
        p = p0
        call singleCompSV_Tv(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,isConverged)
        if (isConverged) then
          exit ! exit loop
        else
          beta = 0.5
          betaL = 1.0-beta
        endif
      endif
    enddo

    if (.not. isConverged) then
      ! Try close to critical point
      t = t0
      p = p0
      call singleCompSV(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,small,isConverged=isConverged)
    endif
    if (.not. isConverged) then
      if (present(ierr)) then
        ierr = 1
      else
        print *,'Initial temperature',t0
        print *,'Initial pressure',p0
        print *,'Entropy',sspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        print *,'Gas phase mol fraction',beta0
        print *,'Initial phase',phaseVec(1)
        call stoperror('sv_solver::twoPhaseSVsingleComp: No convergence')
      endif
    endif
  end subroutine twoPhaseSVsingleComp

  !-----------------------------------------------------------------------------
  !> Do SV-flash for single component
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine singleCompSV(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,tcOffset,isConverged)
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
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    real, optional, intent(in) :: tcOffset
    logical, optional, intent(out) :: isConverged
    ! Locals
    real, dimension(nc+7) :: param !< Parameter vector
    real :: tci,pci,oi,dpdt,hg,hl,vg,vl
    real :: t0, x0, var(2), xmin(2), xmax(2)
    type(nonlinear_solver) :: solver

    isConverged = .true.
    param(1) = sspec
    param(2) = vspec
    param(3:nc+2) = Z(1:nc)
    ! Get critical point
    call getCriticalParam(maxComp(Z),tci,pci,oi)
    param(nc+3) = pci
    param(nc+4) = tci
    xmin(1) = tpTmin + 50.0
    if (present(tcOffset)) then
      xmax(1) = tci - tcOffset
    else
      xmax(1) = tci - 1.0e-2
    endif
    xmin(2) = -2.0
    xmax(2) = 3.0
    ! Allow starting close to critical point
    ! Should test for closeness to sc and vc
    !t = max(min(t, xmax(1)), tpTmin + 50.0)
    t = (tci + tpTmin + 50.0)*0.5
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

    solver%abs_tol = singlesv_tolerance
    solver%ls_max_it = singlesv_line_searches
    solver%max_it = singlesv_nmax

    call nonlinear_solve(solver,fun_two_single,jac_two_single,jac_two_single,limit_dx,&
         premterm_at_dx_zero,setXv,var,xmin,xmax,param)
    if (solver%exitflag == 0) then
      beta = var(2)
      if (beta < 0.0 .or. beta > 1.0) then
        solver%exitflag = -1
      else
        t = var(1)
        p = dewP(T,param(nc+5),X,Z)
        Y = Z
        X = Z
        betaL = 1.0 - beta
        phase = TWOPH
      endif
    endif

    if (solver%exitflag /= 0) then
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
        print *,'Entropy',sspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        call stoperror('sv_solver::twoPhaseSVsingleComp: The nonlinear optimizer did not converge')
      endif

    endif
  end subroutine singleCompSV

  !-----------------------------------------------------------------------------
  !> Calculate residual for single component two-phase SV system
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine fun_two_single(f,var,param)
    use eos, only: specificVolume, enthalpy
    use saturation, only: safe_dewP, dewP
    implicit none
    real, dimension(2), intent(out) :: f !< Function values
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+7), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: t, p, s, sspec, vspec, v, beta, sg, sl, vg, vl
    real :: t_old, p_old, p_c, t_c, dpdt
    real, dimension(nc) :: Z, X
    !
    t = var(1)
    beta = var(2)
    sspec = param(1)
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
    call entropy(t,p,z,VAPPH,sg)
    call specificVolume(t,p,z,LIQPH,vl)
    call entropy(t,p,z,LIQPH,sl)
    ! Use Clapeyron's equation
    if (abs(vg-vl) /= 0.0) then
      dpdt = (sg-sl)/(vg-vl)
      param(nc+7) = dpdt
    endif
    param(nc+5) = p
    param(nc+6) = t
    !
    s = beta*sg+(1.0-beta)*sl
    v = beta*vg+(1.0-beta)*vl

    f(1) = (s - sspec)/max(abs(sspec),1.0)
    f(2) = (v - vspec)/vspec

    !f(1)= (sspec+p*vspec-h)/(Rgas*t)
    !f(2)= p*(v-vspec)/(Rgas*t)
  end subroutine fun_two_single

  !-----------------------------------------------------------------------------
  !> Calculate differential for single component two-phase SV system.
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine jac_two_single(Jac,var,param)
    use saturation, only: safe_dewP, dewP
    implicit none
    real, dimension(2), intent(in) :: var !< Variable vector
    real, dimension(nc+7), intent(inout) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: t, p, sspec, vspec, beta, sg, sl, vg, vl, scale
    real :: dvdtg, dvdpg, dvdtl, dvdpl
    real :: dsdtg, dsdpg, dsdtl, dsdpl, dpdt
    real, dimension(nc) :: Z, X
    real :: t_old, p_old, p_c, t_c
    !real :: dvdt, dhdt, denumFact, h, v

    t = var(1)
    beta = var(2)
    sspec = param(1)
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
    call entropy(t,p,z,VAPPH,sg,dsdt=dsdtg,dsdp=dsdpg)
    call specificVolume(t,p,z,LIQPH,vl,dvdt=dvdtl,dvdp=dvdpl)
    call entropy(t,p,z,LIQPH,sl,dsdt=dsdtl,dsdp=dsdpl)
    ! Use Clapeyron's equation
    if (abs(vg-vl) /= 0.0) then
      dpdt = (sg-sl)/(vg-vl)
      param(nc+7) = dpdt
    else
      dpdt = 0.0
    endif
    param(nc+5) = p
    param(nc+6) = t
    !
    dsdtg = dsdtg + dsdpg*dpdt
    dsdtl = dsdtl + dsdpl*dpdt
    dvdtg = dvdtg + dvdpg*dpdt
    dvdtl = dvdtl + dvdpl*dpdt

    scale = 1.0/max(abs(sspec),1.0)
    Jac(1,1) = scale*(beta*dsdtg + (1.0-beta)*dsdtl)
    Jac(1,2) = scale*(sg-sl)
    Jac(2,1) = (beta*dvdtg + (1.0-beta)*dvdtl)/vspec
    Jac(2,2) = (vg-vl)/vspec

  end subroutine jac_two_single

  !-----------------------------------------------------------------------------
  !> Do SV-flash for single component single phase. Use U(T,v)
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine singleCompSV_Tv(t,p,Z,beta,betaL,X,Y,sspec,vspec,phase,isConverged,pmin,pmax)
    use eos, only: getCriticalParam, specificVolume, enthalpy
    use eosTV, only: pressure
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         premterm_at_dx_zero, setXv, limit_dx
    use thermo_utils, only: maxComp, isSingleComp
    use saturation, only: safe_dewP, dewP
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    real, intent(in) :: vspec !< Specified specific volume [m3/mol]
    integer, intent(inout) :: phase !< Phase identifier
    logical, optional, intent(out) :: isConverged
    real, optional, intent(in) :: pmin,pmax
    ! Locals
    real, dimension(nc+2) :: param !< Parameter vector
    real, dimension(nc) :: FUGZ
    real :: t0, var(1), xmin(1), xmax(1), v, pminimum, pmaximum
    type(nonlinear_solver) :: solver
    integer :: sphase, is, imax(1)
    logical :: testSpecVolume
    type(thermo_model), pointer :: act_mod_ptr
    !
    act_mod_ptr => get_active_thermo_model()
    isConverged = .true.
    param(1) = sspec
    param(2) = vspec
    param(3:nc+2) = Z(1:nc)
    var(1) = t

    solver%abs_tol = singlesv_tolerance
    solver%ls_max_it = singlesv_line_searches
    solver%max_it = singlesv_nmax

    xmin(1) = tpTmin
    xmax(1) = tpTmax*2.0
    call nonlinear_solve(solver,fun_Tv_single,jac_Tv_single,jac_Tv_single,limit_dx,&
         premterm_at_dx_zero,setXv,var,xmin,xmax,param)

    t = var(1)
    if (solver%exitflag == 0) then
      ! Check for positive pressure
      p = pressure(t,vspec,Z)
      if (present(pmin)) then
        pminimum = pmin
      else
        pminimum = tpPmin
      endif
      if (present(pmax)) then
        pmaximum = pmax
      else
        pmaximum = 1.0e100
      endif
      if (p < pminimum .or. p > pmaximum) then
        solver%exitflag = 1 ! False solution
      else
        call thermo(t,p,Z,MINGIBBSPH,FUGZ,ophase=phase)
        sphase = phase
        if (sphase == SINGLEPH) then
          sphase = LIQPH
        endif
        if (sphase == VAPPH) then
          beta = 1.0
          betaL = 0.0
        else if (sphase == LIQPH) then
          beta = 0.0
          betaL = 1.0
        endif
        X = Z
        Y = Z
        ! Only test if not at single component critical point
        if (isSingleComp(Z)) then
          imax = maxloc(Z)
          is = imax(1)
          testSpecVolume = (abs((1.0 - t/act_mod_ptr%comps(is)%p_comp%tc)*(1.0 - p/act_mod_ptr%comps(is)%p_comp%pc)) > small)
        else
          testSpecVolume = .true.
        endif
        if (testSpecVolume) then
          call specificVolume(t,p,Z,sphase,v)
          if (abs(v-vspec)/vspec > solver%abs_tol) then
            solver%exitflag = 1 ! False solution?
          endif
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
        print *,'Entropy',sspec
        print *,'Specific volume',vspec
        print *,'Comp.',Z
        call stoperror('sv_solver::singleCompSV_Tv: The nonlinear optimizer did not converge')
      endif

    endif
  end subroutine singleCompSV_Tv

  !-----------------------------------------------------------------------------
  !> Calculate residual for one-phase SV system. S(T,v) formulation.
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine fun_Tv_single(f,var,param)
    use eosTV, only: entropyTV
    implicit none
    real, dimension(1), intent(out) :: f !< Function values
    real, dimension(1), intent(in) :: var !< Variable vector
    real, dimension(nc+2), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: t, s, sspec, vspec
    real, dimension(nc) :: Z
    !
    t = var(1)
    sspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)

    call entropyTV(t,vspec,Z,s)

    f(1)= (s-sspec)/max(1.0,abs(sspec))
  end subroutine fun_Tv_single

  !-----------------------------------------------------------------------------
  !> Calculate differential for one-phase SV system. S(T,v) formulation.
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  subroutine jac_Tv_single(Jac,var,param)
    use eosTV, only: entropyTV
    implicit none
    real, dimension(1), intent(in) :: var !< Variable vector
    real, dimension(nc+2), intent(inout) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: Jac !< Jacobian objective function
    ! Locals
    real :: t, s, sspec, dsdt, vspec
    real, dimension(nc) :: Z

    t = var(1)
    sspec = param(1)
    vspec = param(2)
    Z(1:nc) = param(3:nc+2)
    call entropyTV(t,vspec,Z,s,dsdt)
    !
    !f(1)=  (u-sspec)/max(1.0,abs(sspec))
    !
    Jac(1,1) =  dsdt/max(1.0,abs(sspec))
  end subroutine jac_Tv_single

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Solver properties !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  !> Set tolerance for nested loop SV flash.
  !>
  !> \author MH, 2015
  !-----------------------------------------------------------------------------
  subroutine setNestedSVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(in) :: tolerance !< Solver tolerance
    integer, intent(in) :: nmax !< Maximum number of iteretions
    integer, intent(in) :: linesearch_nmax !< Maximum number of line-searches
    !
    nested_tolerance = tolerance
    nested_line_searches = linesearch_nmax
    nested_nmax = nmax
  end subroutine setNestedSVtolerance

  !-----------------------------------------------------------------------------
  !> Get tolerance for nested loop SV flash.
  !>
  !> \author MH, 2015
  !-----------------------------------------------------------------------------
  subroutine getNestedSVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(out) :: tolerance !< Solver tolerance
    integer, intent(out) :: nmax !< Maximum number of iteretions
    integer, intent(out) :: linesearch_nmax !< Maximum number of line-searches
    !
    tolerance = nested_tolerance
    linesearch_nmax = nested_line_searches
    nmax = nested_nmax
  end subroutine getNestedSVtolerance

  !-----------------------------------------------------------------------------
  !> Set tolerance for fulleq. SV flash.
  !>
  !> \author MH, 2015
  !-----------------------------------------------------------------------------
  subroutine setFullEqSVtolerance(tolerance,nmax,linesearch_nmax,gibbs_tolerance)
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
  end subroutine setFullEqSVtolerance

  !-----------------------------------------------------------------------------
  !> Get tolerance for fulleq. SV flash.
  !>
  !> \author MH, 2015
  !-----------------------------------------------------------------------------
  subroutine getFullEqSVtolerance(tolerance,nmax,linesearch_nmax,gibbs_tolerance)
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
  end subroutine getFullEqSVtolerance

  !-----------------------------------------------------------------------------
  !> Set tolerance for single component SV flash.
  !>
  !> \author MH, 2015
  !-----------------------------------------------------------------------------
  subroutine setSingleCompSVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(in) :: tolerance !< Solver tolerance
    integer, intent(in) :: nmax !< Maximum number of iteretions
    integer, intent(in) :: linesearch_nmax !< Maximum number of line-searches
    !
    singlesv_tolerance = tolerance
    singlesv_line_searches = linesearch_nmax
    singlesv_nmax = nmax
  end subroutine setSingleCompSVtolerance

  !-----------------------------------------------------------------------------
  !> Get tolerance for single component SV flash.
  !>
  !> \author MH, 2015
  !-----------------------------------------------------------------------------
  subroutine getSingleCompSVtolerance(tolerance,nmax,linesearch_nmax)
    implicit none
    real, intent(out) :: tolerance !< Solver tolerance
    integer, intent(out) :: nmax !< Maximum number of iteretions
    integer, intent(out) :: linesearch_nmax !< Maximum number of line-searches
    !
    tolerance = singlesv_tolerance
    linesearch_nmax = singlesv_line_searches
    nmax = singlesv_nmax
  end subroutine getSingleCompSVtolerance

end module sv_solver
