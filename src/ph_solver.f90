!-----------------------------------------------------------------------------
!> Solve the two-phase PH flash.
!-----------------------------------------------------------------------------
module ph_solver
  !
  !
  use numconstants, only: small, machine_prec
  use thermopack_var, only: nc, get_templimits
  use tp_solver, only: twoPhaseTPflash
  use state_functions
  use thermopack_constants, only: VAPPH, LIQPH, TWOPH, SINGLEPH
  use thermo_utils, only: isSingleComp, maxComp
  use eos, only: enthalpy, getcriticalparam, pseudo_safe, twophaseenthalpy
  use saturation, only: safe_dewT
  use puresaturation, only: puresat
  use stability, only: checkVLEstability
  implicit none
  private
  save

  !> Number of iteration steps with simplified Jacobian
  integer, parameter :: nSimpIter = 0
  !> When to test if phase should be dropped?
  real, parameter :: dropPhaseTestBeta = 1.0e-10
  !> Tolerance for ph-flash
  real :: tolerance = 1.0e6*machine_prec
  logical :: verbose_loc = .false.

  !> Flash-mode. Some functions allow multiple specifications
  integer, parameter :: PH_MODE=1, PS_MODE=2

  public :: twoPhasePHflash, getPHtolerance, setPHtolerance
  public :: singleComponentTwoPhasePHflash
  public :: singlePhasePXflash, PH_MODE, PS_MODE

contains

  !-----------------------------------------------------------------------------
  !> Interface for PH flash
  !>
  !> \author MH, 2012-03-20
  !-----------------------------------------------------------------------------
  subroutine twoPhasePHflash(t,p,Z,beta,betaL,X,Y,hspec,phase,ierr_out)
    use thermo_utils, only: isSingleComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: hspec !< Specified enthalpy [J/mol]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr_out !< Error flag (ierr==0 means everything vent well. ierr==-2 out of temperature range, ierr==-1 tolerance not met)
    ! Locals
    real :: tInitial,Tmin,Tmax
    real :: tpc,ppc,zpc,vpc
    real :: Tpuresat, hvap_puresat, hliq_puresat
    real :: p_inout

    integer :: new_phase, ierr, initial_phase
    logical :: isStable
    real :: Wsol(nc)
    ! Initialize error flag
    if (present(ierr_out)) then
      ierr_out = 0
    endif
    isStable=.true.
    tInitial = t
    call get_templimits(Tmin,Tmax)
    if (t > Tmax .OR. t < Tmin .OR. T /= T) then
      t = 0.5*(Tmax+Tmin)
    endif

    ! Single-component algorithm.
    if (isSingleComp(Z)) then
      call singleComponentTwoPhasePHflash(t,p,Z,beta,betaL,hspec,Tmin,Tmax,phase,ierr_out)
      X=Z
      Y=Z
      return
    end if

    ! Start of multicomponent algorithm. If we know we are in the two-phase
    ! region, we will call the twoPhaseDirectPHsolver. If not, we will first do
    ! a singlePhasePHflash, and then analyze if the feed is stable at the
    ! resulting temperature.
    !
    ! Note that it is OK that puresat and pseudo are only approximate, because
    ! they are only used to generate a GUESS for the single phase solver. The
    ! single phase solution will be checked for stability anyway.
    call pseudo_safe(Z,tpc,ppc,zpc,vpc)
    if (p>ppc) then
      phase = VAPPH ! arbitrary choice
      if (t /= tInitial) then
        t = tpc ! hopefully a good initial estimate
      end if
    else
      p_inout = p ! puresat uses an inout variable for pressure
      call PureSat(Tpuresat,p_inout,Z,solveForT=.true.,&
           ierr=ierr) ! puresat accurate enough?
      call enthalpy(Tpuresat,p,Z,phase=LIQPH,h=hliq_puresat)
      if (hspec < hliq_puresat) then
        phase = LIQPH
        Tmax = Tpuresat
      else
        call enthalpy(Tpuresat,p,Z,phase=VAPPH,h=hvap_puresat)
        if (hspec > hvap_puresat) then
          phase = VAPPH
          Tmin = Tpuresat
        else
          phase = TWOPH
          T = Tpuresat
        end if
      end if
    end if

    initial_phase = phase
    if (phase == VAPPH .or. phase == LIQPH) then
      call singlePhasePXflash(t,p,Z,beta,betaL,hspec,Tmin,Tmax,phase,PH_MODE,ierr)
      call checkVLEstability(t,p,Z,isStable,Wsol,new_phase) ! Fails for Lee-Kesler close to or at saturated liquid

      if (isStable) then
        if (p>ppc) then
          ! (Does not need to be supercritical, since the real ppc can be higher
          ! than the ppc estimate from pseudo_safe.)
          phase = SINGLEPH
          beta = -1.0
        end if
        return
      else
        phase = TWOPH
      end if
    end if

    if (phase==TWOPH) then
      call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y) ! return phase
      ! if y=0 or X = 0 the twoPhaseDirectPHsolver will fail ..had a tendency to fail with saturated liquid for Lee-Kesler - should not be here
      ! Still TWOPH?
      if (phase == TWOPH) then
        call twoPhaseDirectPHsolver(t,p,Z,beta,betaL,X,Y,hspec,phase,Tmax,Tmin,ierr)
        if (maxval(abs(X-Y)) < 5.0*tolerance) then
          ! Might be trivial solution. Flag using ierr
          ierr = 1
        endif
        !      else ! ... not again
        !         call singlePhasePHflash(t,p,Z,beta,betaL,hspec,Tmin,Tmax,phase,ierr)
      else
        ! Close to pure fluid
        X = Z
        Y = Z
        phase = TWOPH
        t = Tpuresat
        if (initial_phase == TWOPH) then
          beta = (hspec-hliq_puresat)/(hvap_puresat-hliq_puresat)
        else
          beta = 0.5
        endif
        betaL = 1.0 - beta
        call twoPhaseDirectPHsolver(t,p,Z,beta,betaL,X,Y,hspec,phase,Tmax,Tmin,ierr)
      endif
    end if

    if (ierr/=0) then
      call get_templimits(Tmin,Tmax)
      if (t > Tmax .OR. t < Tmin .OR. T /= T) then
        t = 0.5*(Tmax+Tmin)
      endif
      call nestedMultiCompTwoPhasePHflash(t,p,Z,beta,betaL,X,Y,hspec,phase,ierr,Tmin,Tmax)
      if (present(ierr_out)) then
        ierr_out = ierr
      else if (ierr/=0 .and. verbose_loc) then
        print *, "nestedMultiCompTwoPhasePHflash FAILED"
        print *,'Temperature at exit',t
        print *,'Temperature upper limit',Tmax
        print *,'Temperature lower limit',Tmin
        print *,'Initial temperature',tInitial
        print *,'Pspec',p
        print *,'hspec, h',hspec, twoPhaseEnthalpy(t,p,z,x,y,beta,phase=TWOPH,betaL=betaL)
        print *,'Z', Z
        print *,'X', X
        print *,'Y', Y
      end if
    end if
  end subroutine twoPhasePHflash

  !-----------------------------------------------------------------------------
  !> Do single component PH-flash
  !>
  !> \author MH, 2014-10-17
  !> \author Ailo 2016-12-21
  !-----------------------------------------------------------------------------
  subroutine singleComponentTwoPhasePHflash(t,p,Z,beta,betaL,hspec,Tmin,Tmax,phase,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, setXv
    use eos, only: enthalpy
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: hspec !< Specified entropy [J/mol]
    real, intent(inout) :: Tmin,Tmax !< Temperature limits [K]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr
    ! Locals
    real :: tInitial, hg, hl, X(nc)
    real :: tci, pci, oi

    tInitial = t


    ! Notes on the safe_dewt call:
    ! 1. Profiling shows that it is time-consuming
    ! 2. Convergence in the critical region isn't guaranteed for EoS that
    !    overestimate the critical point, e.g. SAFT and CPA.
    call getCriticalParam(maxComp(Z),tci,pci,oi)
    if (p>pci) then
      phase = SINGLEPH
    else
      T = safe_dewT(P,X,Y=Z,ierr=ierr)
      call enthalpy(T,P,Z,LIQPH,hl)
      if (hspec < hl) then
        phase = LIQPH
        Tmax = T
      else
        call enthalpy(T,P,Z,VAPPH,hg)
        if (hspec > hg) then
          phase = VAPPH
          Tmin = T
        else
          phase = TWOPH
          betaL = (hspec - hg)/(hl - hg)
          beta = (hspec - hl)/(hg - hl)
          return
        endif
      endif
    end if

    call singlePhasePXflash(t,p,Z,beta,betaL,hspec,Tmin,Tmax,phase,PH_MODE,ierr)

  end subroutine singleComponentTwoPhasePHflash

  !-----------------------------------------------------------------------------
  !> Do PH/PS flash, assuming we only have one phase.
  !>
  !> \author Ailo, 2016-12-21
  !> \author MH, 2018-10
  !-----------------------------------------------------------------------------
  subroutine singlePhasePXflash(t,p,Z,beta,betaL,xspec,Tmin,Tmax,phase,mode,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, setXv
    use eos, only: enthalpy
    implicit none
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, intent(in) :: xspec !< Specified enthalpy/entropy [J/mol(/K)]
    real, intent(in) :: Tmin,Tmax !< Temperature limits [K]
    integer, intent(in) :: phase !< Phase identifier
    integer, intent(in) :: mode ! PH_MODE=1, PS_MODE=2
    integer, optional, intent(out) :: ierr
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(1) :: Tv,Tvmin,Tvmax
    real, dimension(nc+6) :: param
    param(1) = xspec
    param(2) = p
    param(3:nc+2) = Z
    param(nc+3) = Tmin
    param(nc+4) = Tmax
    if (phase == SINGLEPH) then
      param(nc+5) = real(LIQPH)
    else
      param(nc+5) = real(phase)
    endif
    param(nc+6) = mode
    Tv = T
    Tvmin = Tmin
    Tvmax = Tmax
    solver%ls_max_it = 3
    solver%abs_tol = tolerance
    call nonlinear_solve(solver,xfun,xdiff,xdiff,limitTv,xpremReturn,setXv,&
         Tv,Tvmin,Tvmax,param)
    if (present(ierr)) ierr = solver%exitflag

    if (solver%exitflag /= 0 .and. verbose_loc) then
      print *,'Temperature at exit',Tv(1)
      print *,'Temperature upper limit',Tmax
      print *,'Temperature lower limit',Tmin
      print *,'Pressure',p
      if (mode == PH_MODE) then
        print *,'Enthalpy',xspec
      else !mode == PS_MODE
        print *,'Entropy',xspec
      endif
      print *,'Comp.',Z
      print *,'Number of iterations',solver%iter
      print *,'Error on exit',solver%error_on_exit
      print *,'Tolerance',tolerance
    endif
    ! Set solution
    t = Tv(1)
    if (phase == VAPPH) then
      beta = 1.0
      betaL = 0.0
    else if (phase == LIQPH) then
      beta = 0.0
      betaL = 1.0
    else if (phase == SINGLEPH) then
      beta = -1.0
      betaL = -1.0
    endif
  end subroutine singlePhasePXflash

  !-----------------------------------------------------------------------------
  !> Do PH-flash using PT-flash in nested loop
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  subroutine nestedMultiCompTwoPhasePHflash(t,p,Z,beta,betaL,X,Y,hspec,phase,ierr,Tmin,Tmax)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, setXv
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: hspec !< Specified enthalpy [J/mol]
    integer, intent(inout) :: phase !< Phase identifier
    integer, intent(out)   :: ierr !< Error flag
    real, intent(inout) :: Tmax, Tmin !< Temperature limit [K]
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(1) :: Tv,Tvmin,Tvmax
    real, dimension(nc+5) :: param

    ierr = 0

    param(1) = hspec
    param(2) = p
    param(3:nc+2) = Z
    Tv = T
    Tvmin = Tmin
    Tvmax = Tmax
    param(nc+3) = Tmin
    param(nc+4) = Tmax
    solver%ls_max_it = 3
    solver%abs_tol = tolerance
    param(nc+5) = solver%abs_tol*2.0
    call nonlinear_solve(solver,fun,diff,diff,limitTv,premReturn,setXv,&
         Tv,Tvmin,Tvmax,param)
    ierr = solver%exitflag
    if (solver%exitflag == -1 .and. param(nc+4)-param(nc+3) < param(nc+5)) then
      ! Premature termination
      solver%exitflag = 0
      if (abs(param(nc+4)-Tmin) > 0.01 .or. abs(param(nc+3)-Tmax) > 0.01) then
        ! Out of temperature range
        ierr = -2
      else
        ! Not able to meet tolerance
        ierr = -1
      endif
    endif

    if (solver%exitflag == 0) then
      ! Set solution
      Tmin = param(nc+3)
      Tmax = param(nc+4)
      t = Tv(1)
      call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)
    endif
  end subroutine nestedMultiCompTwoPhasePHflash

  !-----------------------------------------------------------------------------
  !> Calculate residual for PH system
  !>
  !> \author MHA, 2012-01-30
  !-----------------------------------------------------------------------------
  subroutine fun(f,Tv,param)
    implicit none
    real, dimension(1), intent(out) :: f !< Residual [-]
    real, dimension(1), intent(in) :: Tv !< Temperature [K]
    real, dimension(nc+5), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: beta, t, p, h, hspec, betaL
    real, dimension(nc) :: Z,X,Y
    integer :: phase

    t = Tv(1)
    hspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    beta = 0.5

    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)
    h = twoPhaseEnthalpy(t,p,z,x,y,beta,phase,betaL=betaL)
    f = (h - hspec)/max(abs(hspec),1.0)
    if (f(1) > 0.0) then
      param(nc+4) = t
    else
      param(nc+3) = t
    endif
  end subroutine fun

  !-----------------------------------------------------------------------------
  !> Calculate differential for PH system.
  !>
  !> \author MHA, 2012-01-30
  !-----------------------------------------------------------------------------
  subroutine diff(J,Tv,param)
    use eos, only: enthalpy
    implicit none
    real, dimension(1), intent(in)    :: Tv !< Temperature [K]
    real, dimension(nc+5), intent(in) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: J !< Jacobean matrix
    ! Locals
    real:: beta, t, p, h, dhdt, hspec, betaL
    real, dimension(nc) :: Z,X,Y
    integer :: phase, sphase

    t = Tv(1)
    hspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    beta = 0.5

    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)

    if (phase == TWOPH) then
      dhdt = dhdt_twoPhase(t,p,Z,beta,betaL,X,Y)
    else
      sphase = LIQPH
      if (phase == VAPPH) then
        sphase = VAPPH
      endif
      call enthalpy(t,p,z,sphase,h,dhdt=dhdt)
    endif
    J(1,1) = dhdt/(max(abs(hspec),1.0))
  end subroutine diff

  !-----------------------------------------------------------------------------
  !> Limit change in temperature.
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  subroutine limitTv(n,Tv,Tvmin,Tvmax,dTv,np,param)
    implicit none
    integer, intent(in)               :: n !< Size of Tv, dTv, Tvmin and Tvmax
    integer, intent(in)               :: np !< Size of param
    real, dimension(n), intent(in)    :: Tv,Tvmin,Tvmax !< Temperature [K]
    real, dimension(np), intent(in)   :: param !< Parameter vector
    real, dimension(n), intent(inout) :: dTv !< Calculated change in temperature [K]
    !
    real :: tMax, tMin
    call get_templimits(tMin,tMax)
    tMin = max(tMin,param(nc+3))
    tMax = min(tMax,param(nc+4))
    if (Tv(1) + dTv(1) < tMin) then
      dTv(1) = Tmin - Tv(1)
    else if (Tv(1) + dTv(1) > tMax) then
      dTv(1) = Tmax - Tv(1)
    endif
  end subroutine limitTv

  !-----------------------------------------------------------------------------
  !> Premature termination when solution is outside valid temperature region.
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  function premReturn(Tv,dTv,param,n,np) result(doReturn)
    implicit none
    integer, intent(in)               :: n !< Size of Tv and dTv
    integer, intent(in)               :: np !< Size of param
    real, dimension(n), intent(in)    :: Tv !< Inverse temperature [K]
    real, dimension(np), intent(in)   :: param !< Parameter vector
    real, dimension(n), intent(in)    :: dTv !< Differential of objective function
    logical                           :: doReturn !< Terminate minimization?
    ! Locals
    real                              :: Tmin,Tmax
    call get_templimits(Tmin,Tmax)
    doReturn = .false.
    if (Tv(1) < Tmin + small .and. dTv(1) > 0.0) then ! s(Tmin) - hspec > 0
      doReturn = .true.
    endif
    if (Tv(1) > Tmax - small .and. dTv(1) < 0.0) then ! s(Tmax) - hspec < 0
      doReturn = .true.
    endif
    if (param(nc+4)-param(nc+3) < param(nc+5)) then ! Not able to meet tolerance
      doReturn = .true.
    endif
  end function premReturn

  !-----------------------------------------------------------------------------
  !> Full newton solver. Only use when we know we are in the two-phase regime.
  !> Needs initial guesses for temperature and gas and liquid phase composition.
  !> \author MH, 2014-10-23
  !> \author Ailo, 2016-12-21
  !-----------------------------------------------------------------------------
  subroutine twoPhaseDirectPHsolver(t,p,Z,beta,betaL,X,Y,hspec,phase,Tmax,Tmin,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: hspec !< Specified entropy [J/mol/K]
    integer, intent(inout) :: phase !< Phase identifier
    real, intent(in) :: Tmax, Tmin !< Temperature limits [K]
    integer, intent(out) :: ierr ! Error message
    ! Locals
    type(nonlinear_solver) :: solver
    real :: t0, X0(nc), Y0(nc)
    real, dimension(1+nc) :: Xv,Xvmin,Xvmax
    real, dimension(2*nc+3) :: param
    real :: betaUpper
    t0 = t
    X0 = X
    Y0 = Y

    ! Ensure that beta is not so large as to give negative liquid amounts
    betaUpper = minval(Z/Y)*0.9
    beta = min(beta,betaUpper)

    param(1) = hspec
    param(2) = p
    param(3:nc+2) = Z
    param(nc+3) = 0 ! iteration counter
    Xv(1:nc) = beta*Y ! vapor amounts
    param(nc+4:2*nc+3) = Z - Xv(1:nc) ! liquid amounts
    Xv(nc+1) = log(T)
    Xvmin(1:nc) = 0.0
    Xvmax(1:nc) = 1.0
    Xvmin(nc+1) = log(Tmin)
    Xvmax(nc+1) = log(Tmax)
    solver%ls_max_it = 3
    solver%abs_tol = tolerance
    solver%symmetric_jac = .true.
    call nonlinear_solve(solver,fullfun,fulldiff,fulldiff,fullLimitXv,fullPremReturn,&
         fullSetXv,Xv,Xvmin,Xvmax,param)

    ! Set solution
    ierr = solver%exitflag
    t = exp(Xv(nc+1))
    beta = sum(Xv(1:nc))
    Y = Xv(1:nc)/beta
    betaL = sum(param(nc+4:2*nc+3))
    X = param(nc+4:2*nc+3)/betaL
  end subroutine twoPhaseDirectPHsolver

  !-----------------------------------------------------------------------------
  !> Calculate residual for full PH system
  !>
  !> \author MH, 2014-10-17
  !-----------------------------------------------------------------------------
  subroutine fullfun(f,Xv,param)
    use state_functions, only: getStateFunc
    implicit none
    real, dimension(1+nc), intent(out) :: f !< Temperature [K]
    real, dimension(1+nc), intent(in) :: Xv !< Temperature [K]
    real, dimension(2*nc+3), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: t, p, hspec(2), beta, betaL
    real, dimension(nc) :: X,Y,Z

    t = exp(Xv(1+nc))
    hspec(1) = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)

    beta = sum(Xv(1:nc))
    Y = Xv(1:nc)/beta
    betaL = sum(param(nc+4:2*nc+3))
    X = param(nc+4:2*nc+3)/betaL

    call getStateFunc(t,p,Z,beta,betaL,X,Y,'PH',hspec,f)
  end subroutine fullfun

  !-----------------------------------------------------------------------------
  !> Calculate differential for full PH system.
  !>
  !> \author MH, 2014-10-17
  !-----------------------------------------------------------------------------
  subroutine fulldiff(J,Xv,param)
    use state_functions, only: getStateFuncMatrix
    implicit none
    real, dimension(1+nc,1+nc), intent(out) :: J !< Jacobean matrix
    real, dimension(1+nc), intent(in) :: Xv !< Temperature [K]
    real, dimension(2*nc+3), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: t, p, hspec(2), beta, betaL, f(nc+1)
    real, dimension(nc) :: X,Y,Z
    integer :: iter
    logical :: simpleMatrix

    t = exp(Xv(1+nc))
    hspec(1) = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    iter = int(param(nc+3))
    if (iter < nSimpIter) then
      simpleMatrix = .true.
    else
      simpleMatrix = .false.
    endif
    param(nc+3) = real(iter+1)
    beta = sum(Xv(1:nc))
    Y = Xv(1:nc)/beta
    betaL = sum(param(nc+4:2*nc+3))
    X = param(nc+4:2*nc+3)/betaL

    call getStateFuncMatrix(t,p,Z,beta,betaL,X,Y,'PH',hspec,J,f,simpleMatrix=simpleMatrix)
  end subroutine fulldiff

  !-----------------------------------------------------------------------------
  !> Premature termination for full SP solver.
  !>
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  function fullPremReturn(Xv,dXv,param,n,np) result(premature_return)
    use eos, only: entropy
    implicit none
    integer, intent(in)               :: n !< Size of Tv and dTv
    integer, intent(in)               :: np !< Size of param
    real, dimension(n), intent(in)    :: Xv !< Variable vector
    real, dimension(np), intent(in)   :: param !< Parameter vector
    real, dimension(n), intent(in)    :: dXv !< Change in variable vector
    logical                           :: premature_return !< Terminate minimization?
    ! Locals
    real :: t,p,beta,betaL,Stwo,Ssingle,sl,sg
    integer :: phase
    real, dimension(nc) :: Z,L,X,Y

    beta = sum(Xv(1:nc))
    premature_return = .false.
    if (beta < dropPhaseTestBeta .or. beta > 1.0 - dropPhaseTestBeta) then
      ! Try removing phase - do we get an increase in S?
      Z(1:nc) = param(3:nc+2)
      if (beta > 0.5) then
        phase = VAPPH
      else
        phase = LIQPH
      endif
      Y = Xv(1:nc)/beta
      L = param(nc+4:2*nc+3)
      betaL = sum(L)
      X = L/betaL
      t = exp(Xv(nc+1))
      p = param(2)
      call entropy(t,p,y,VAPPH,sg,residual=.true.)
      call entropy(t,p,x,LIQPH,sl,residual=.true.)
      Stwo = beta*sg+betaL*sl
      call entropy(t,p,z,phase,Ssingle,residual=.true.)

      if (Ssingle > Stwo) then ! Maximize entropy
        premature_return = .true.
      endif
    endif
  end function fullPremReturn

  !-----------------------------------------------------------------------------
  !> Limit change in variables
  !>
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  subroutine fullLimitXv(n,var,varmin,varmax,dvar,np,param)
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
    ! Temperature limit
    if (var(nc+1)+dvar(nc+1) < varmin(nc+1) .AND. abs(dvar(nc+1)) > 1.0e-9) then
      scale = (varmin(nc+1)-var(nc+1))/dvar(nc+1)
    endif

    Z(1:nc) = param(3:nc+2)
    L = param(nc+4:2*nc+3)
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
  end subroutine fullLimitXv

  !-----------------------------------------------------------------------------
  !> Set mole numbers. Try to avoid truncation error.
  !>
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  subroutine fullSetXv(n,nparam,X,dX,varmin,varmax,param,alpha)
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

    ! Mole numbers
    Z = param(3:nc+2)
    L = param(nc+4:2*nc+3)
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
    param(nc+4:2*nc+3) = L

  end subroutine fullSetXv

  !-----------------------------------------------------------------------------
  !> Calculate residual for single component PH/PS system
  !>
  !> \author MH, 2014-10-17
  !-----------------------------------------------------------------------------
  subroutine xfun(f,Tv,param)
    use eos, only: enthalpy, entropy
    implicit none
    real, dimension(1), intent(out) :: f !< Temperature [K]
    real, dimension(1), intent(in) :: Tv !< Temperature [K]
    real, dimension(nc+6), intent(inout) :: param !< Parameter vector
    !real :: of !< Objective function value
    ! Locals
    real:: t, p, x, xspec
    real, dimension(nc) :: Z
    integer :: phase, mode

    t = Tv(1)
    xspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = int(param(nc+5))
    mode = int(param(nc+6))
    if (mode == PH_MODE) then
      call enthalpy(T,P,Z,phase,x)
    else !mode == PS_MODE
      call entropy(T,P,Z,phase,x)
    endif
    f = (x - xspec)/max(abs(xspec),1.0)
  end subroutine xfun

  !-----------------------------------------------------------------------------
  !> Calculate differential for single component PH/PS system.
  !>
  !> \author MH, 2012-01-30
  !-----------------------------------------------------------------------------
  subroutine xdiff(J,Tv,param)
    use eos, only: enthalpy, entropy
    implicit none
    real, dimension(1), intent(in)    :: Tv !< Temperature [K]
    real, dimension(nc+6), intent(in) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: J !< Jacobean matrix
    ! Locals
    real:: t, p, x, dxdt, xspec
    real, dimension(nc) :: Z
    integer :: phase, mode

    t = Tv(1)
    xspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = int(param(nc+5))
    mode = int(param(nc+6))
    if (mode == PH_MODE) then
      call enthalpy(t,p,z,phase,x,dhdt=dxdt)
    else !mode == PS_MODE
      call entropy(T,P,Z,phase,x,dsdt=dxdt)
    endif
    J(1,1) = dxdt/max(abs(xspec),1.0)
  end subroutine xdiff

  !-----------------------------------------------------------------------------
  !> Premature termination when solution is outside valid temperature region.
  !>
  !> \author MH, 2012-03-20
  !-----------------------------------------------------------------------------
  function xpremReturn(Tv,dTv,param,n,np) result(doReturn)
    implicit none
    integer, intent(in)               :: n !< Size of Tv and dTv
    integer, intent(in)               :: np !< Size of param
    real, dimension(n), intent(in)    :: Tv !< Inverse temperature [K]
    real, dimension(np), intent(in)   :: param !< Parameter vector
    real, dimension(n), intent(in)    :: dTv !< Differential of objective function
    logical                           :: doReturn !< Terminate minimization?
    ! Locals
    real                              :: Tmin,Tmax
    call get_templimits(Tmin,Tmax)
    doReturn = .false.
    if (Tv(1) < Tmin + small .and. dTv(1) > 0.0) then ! s(Tmin) - hspec > 0
      doReturn = .true.
    endif
    if (Tv(1) > Tmax - small .and. dTv(1) < 0.0) then ! s(Tmax) - hspec < 0
      doReturn = .true.
    endif
  end function xpremReturn

  !-----------------------------------------------------------------------------
  !> Set PH-flash tolerance
  !> Caution, write to module variable, and is not thread safe
  !>
  !> \author MH, 2015-03
  !-----------------------------------------------------------------------------
  subroutine setPHtolerance(tol)
    implicit none
    real, intent(in) :: tol !< Tolerance for hp-flash [-]
    !
    tolerance = tol
  end subroutine setPHtolerance

  !-----------------------------------------------------------------------------
  !> Get PH-flash tolerance
  !>
  !> \author MH, 2015-03
  !-----------------------------------------------------------------------------
  function getPHtolerance() result(tol)
    implicit none
    real :: tol !< Tolerance of hp-flash [-]
    !
    tol = tolerance
  end function getPHtolerance

end module ph_solver
