!-----------------------------------------------------------------------------
!> Solve PS-flash specification
!>
!> \todo Need trace-component functionallity.
!>
!-----------------------------------------------------------------------------
module ps_solver
  !
  !
  use numconstants, only: small, machine_prec
  use thermopack_var, only: nc, nph, get_active_eos_container, eos_container
  use thermopack_constants, only: get_templimits, LIQPH, VAPPH, continueOnError, &
       SINGLEPH, SOLIDPH, TWOPH, VAPSOLPH, MINGIBBSPH
  use tp_solver, only: twoPhaseTPflash
  use state_functions
  implicit none
  private
  save

  !> Number of iteration steps with simplified Jacobian
  integer, parameter :: nSimpIter = 3
  !> When to test if phase should be dropped?
  real, parameter :: dropPhaseTestBeta = 1.0e-10
  !> Tolerance for ph-flash
  real :: tolerance = 2.0e4*machine_prec

  public :: twoPhasePSflash, getPStolerance, setPStolerance
  public :: singleComponentTwoPhasePSflash

contains

  !-----------------------------------------------------------------------------
  !> Do PS-flash using PT-flash in nested loop
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  subroutine twoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase)
    use thermo_utils, only: isSingleComp
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    integer, intent(inout) :: phase !< Phase identifier
    ! Locals
    real :: tInitial,Tmin,Tmax
    integer :: ierr

    tInitial = t
    if (isSingleComp(Z)) then
      call singleComponentTwoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase)
    else
     call nestedMultiCompTwoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr,Tmin,Tmax)
      !if (ierr == -1) then
      !  call fullMultiCompTwoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr,Tmin,Tmax)
      !endif
      if (ierr /= 0 .and. .not. continueOnError) then
        print *,'Temperature at exit',t
        print *,'Temperature upper limit',Tmax
        print *,'Temperature lower limit',Tmin
        print *,'Initial temperature',tInitial
        print *,'Pressure',p
        print *,'Entropy',sspec
        print *,'Comp.',Z
        call stoperror('ps_solver::twoPhasePSflash: The nonlinear solver did not converge')
      endif
    endif
  end subroutine twoPhasePSflash

  !-----------------------------------------------------------------------------
  !> Full newton solver
  !> Require initial guess for temperature and gas and liquid phase composition.
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  subroutine fullMultiCompTwoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr,Tmin,Tmax)
    use stability, only: stabilityLimit, stabcalc
    use tp_solver, only: rr_solve
    use eos, only: thermo
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    integer, intent(inout) :: phase !< Phase identifier
    real, intent(in) :: Tmax, Tmin !< Temperature limits [K]
    integer, intent(out) :: ierr ! Error message 
    ! Locals
    real, dimension(nc)   :: FUGZ, FUGL, FUGG, K
    real                  :: tpd
    logical               :: gas_stab_negative, liq_stab_negative
    logical               :: isTrivialL, isTrivialV
    integer :: ophase
    ! Run full solver for two-phase
    ierr = 0
    if (phase /= TWOPH) then
      call thermo(t,p,Z,MINGIBBSPH,FUGZ,ophase=ophase)
      if (.not. ophase == SINGLEPH) then
        beta = 0.5
        betaL = 1.0 - beta
        ! if (ophase == LIQPH) then
        !   FUGL = FUGZ
        !   call thermo(t,p,Z,VAPPH,FUGG)
        ! else
        !   FUGG = FUGZ
        !   call thermo(t,p,Z,LIQPH,FUGL)
        ! endif
        ! K = exp(FUGL-FUGG)
      else
        tpd = stabcalc(t,p,Z,LIQPH,isTrivialL,FUGZ,FUGL)
        liq_stab_negative = (.not. isTrivialL .and. tpd < stabilityLimit)
        tpd = stabcalc(t,p,Z,VAPPH,isTrivialV,FUGZ,FUGG)
        gas_stab_negative = (.not. isTrivialV .and. tpd < stabilityLimit)
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
            ierr = 1
          endif
        else
          ierr = 1
        endif
      endif
    endif
    if (ierr /= 0) then
      call twoPhasePSflashFull(t,p,Z,beta,betaL,X,Y,sspec,phase,Tmax,Tmin,ierr)          
    endif
  end subroutine fullMultiCompTwoPhasePSflash

  !-----------------------------------------------------------------------------
  !> Do PS-flash using PT-flash in nested loop
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  subroutine nestedMultiCompTwoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr,Tmin,Tmax)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, setXv
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    integer, intent(inout) :: phase !< Phase identifier
    integer, intent(out)   :: ierr !< Error flag
    real, intent(out) :: Tmax, Tmin !< Temperature limit [K]
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(1) :: Tv,Tvmin,Tvmax
    real, dimension(nc+5) :: param

    ierr = 0
    call get_templimits(Tmin,Tmax)
    if (t > Tmax .OR. t < Tmin .OR. T /= T) then
      t = 0.5*(Tmax+Tmin)
    endif
    param(1) = sspec
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
    if (solver%exitflag == -1 .and. param(nc+4)-param(nc+3) < param(nc+5)) then
      ! Not able to meet tolerance
      solver%exitflag = 0
    endif
    ierr = solver%exitflag

    if (solver%exitflag == -1) then
      call stoperror('ps_solver::nestedMultiCompTwoPhasePSflash: Temperature out of range')
    endif

    ! Set solution
    Tmin = param(nc+3)
    Tmax = param(nc+4)
    t = Tv(1)
    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)
  end subroutine nestedMultiCompTwoPhasePSflash

  !-----------------------------------------------------------------------------
  !> Calculate residual for PS system
  !>
  !> \author MHA, 2012-01-30
  !-----------------------------------------------------------------------------
  subroutine fun(f,Tv,param)
    use eos, only: twoPhaseEntropy
    implicit none
    real, dimension(1), intent(out) :: f !< Temperature [K]
    real, dimension(1), intent(in) :: Tv !< Temperature [K]
    real, dimension(nc+5), intent(inout) :: param !< Parameter vector
    ! Locals
    real:: beta, t, p, s, sspec, betaL
    real, dimension(nc) :: Z,X,Y
    integer :: phase

    t = Tv(1)
    sspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    beta = 0.5

    call twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)
    s = twoPhaseEntropy(t,p,z,x,y,beta,phase,betaL=betaL)
    f = (s - sspec)/max(abs(sspec),1.0)
    if (f(1) > 0.0) then
      param(nc+4) = t
    else
      param(nc+3) = t
    endif
  end subroutine fun

  !-----------------------------------------------------------------------------
  !> Calculate differential for PS system.
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
    real:: beta, t, p, h, dhdt, sspec, betaL
    real, dimension(nc) :: Z,X,Y
    integer :: phase, sphase

    t = Tv(1)
    sspec = param(1)
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
    J(1,1) = dhdt/(t*max(abs(sspec),1.0))
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
    if (Tv(1) < Tmin + small .and. dTv(1) > 0.0) then ! s(Tmin) - sspec > 0
      doReturn = .true.
    endif
    if (Tv(1) > Tmax - small .and. dTv(1) < 0.0) then ! s(Tmax) - sspec < 0
      doReturn = .true.
    endif
    if (param(nc+4)-param(nc+3) < param(nc+5)) then ! Not able to meet tolerance
      doReturn = .true.
    endif
  end function premReturn

  !-----------------------------------------------------------------------------
  !> Do single component PS-flash
  !>
  !> \author MH, 2014-10-17
  !-----------------------------------------------------------------------------
  subroutine singleComponentTwoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, setXv
    use numconstants, only: machine_prec
    use saturation, only: safe_dewT, specP
    use solid_saturation, only: solidFluidEqSingleComp
    use eos, only: entropy, getCriticalParam
    use thermo_utils, only: maxComp
    use solideos, only: nsolid, solidComp, solid_entropy
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    integer, intent(inout) :: phase !< Phase identifier
    integer, optional, intent(out) :: ierr
    ! Locals
    type(nonlinear_solver) :: solver
    real :: tInitial
    real, dimension(1) :: Tv,Tvmin,Tvmax
    real, dimension(nc+6) :: param
    real                  :: Tmin, Tmax, sl, sg, ptr, plocal
    real                  :: tci,pci,oi
    logical               :: lookForSolid
    type(eos_container), pointer :: p_act_eosc

    p_act_eosc => get_active_eos_container()
    call get_templimits(Tmin,Tmax)
    if (t > Tmax .OR. t < Tmin .OR. T /= T) then
      t = 0.5*(Tmax+Tmin)
    endif
    tInitial = t
    X = Z
    Y = Z

    ! Get critical point
    call getCriticalParam(maxComp(Z),tci,pci,oi)
    if (p > pci) then
      ! Single phase solution
      phase = SINGLEPH
    else
      lookForSolid = .false.
      if (nSolid == 1) then
        if (solidComp(1) == maxComp(Z)) then
          lookForSolid = .true.
          ptr = p_act_eosc%comps(maxComp(Z))%p_comp%ptr
        endif
      else
        ptr = 0.0
      endif
      if (lookForSolid .AND. p <= ptr) then
        if (p == ptr) then
           call stoperror('singleComponentTwoPhasePSflash: PS flash undefined for triple point pressure')
        else
          ! Get sublimation temperature
          plocal = p
          call solidFluidEqSingleComp(Z,Y,X,t,plocal,specP,VAPPH)
          call solid_entropy(T,P,Z,sl)
          if (sspec < sl) then
            phase = SOLIDPH
            Tmax = T
            call stoperror('singleComponentTwoPhasePSflash: PS flash will not look for pure solid phase')
          else
            call entropy(T,P,Z,VAPPH,sg)
            if (sspec > sg) then
              phase = VAPPH
              Tmin = T
            else
              phase = VAPSOLPH
              betal = (sspec - sg)/(sl - sg)
              beta  = (sspec - sl)/(sg - sl)
              if (present(ierr)) ierr = 0
            endif
          endif
        endif
      else
        ! Get saturaton temperature
        T = safe_dewT(P,X,Y,ierr=ierr)
        call entropy(T,P,Z,LIQPH,sl)
        if (sspec < sl) then
          phase = LIQPH
          Tmax = T
        else
          call entropy(T,P,Z,VAPPH,sg)
          if (sspec > sg) then
            phase = VAPPH
            Tmin = T
          else
            phase = TWOPH
            betal = (sspec - sg)/(sl - sg)
            beta  = (sspec - sl)/(sg - sl)
            if (present(ierr)) ierr = 0
          endif
        endif
      endif
    endif

    if (phase == VAPPH .OR. phase == LIQPH .OR. phase == SINGLEPH) then
      param(1) = sspec
      param(2) = p
      param(3:nc+2) = Z
      param(nc+3) = Tmin
      param(nc+4) = Tmax
      if (phase == SINGLEPH) then
        param(nc+5) = real(LIQPH)
      else
        param(nc+5) = real(phase)
      endif
      Tv = T
      Tvmin = Tmin
      Tvmax = Tmax
      solver%ls_max_it = 3
      solver%abs_tol = 2.0e4*machine_prec
      param(nc+6) = solver%abs_tol*2.0
      call nonlinear_solve(solver,sfun,sdiff,sdiff,limitTv,&
           spremReturn,setXv,Tv,Tvmin,Tvmax,param)
      if (present(ierr)) then
        ierr = solver%exitflag
      else if (solver%exitflag /= 0 .and. .not. continueOnError) then
        print *,'Temperature at exit',Tv(1)
        print *,'Temperature upper limit',param(nc+4)
        print *,'Temperature lower limit',param(nc+3)
        print *,'Initial temperature',tInitial
        print *,'Pressure',p
        print *,'Entropy',sspec
        print *,'Comp.',Z
        print *,'Number of iterations',solver%iter
        print *,"Error on exit",solver%error_on_exit
        print *,"phase",phase
        if (solver%exitflag > 0) then
          call stoperror('ps_solver::singleComponentTwoPhasePSflash: The nonlinear solver did not converge')
        else if (solver%exitflag == -1) then
          call stoperror('ps_solver::singleComponentTwoPhasePSflash: Temperature out of range')
        endif
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
    endif
  end subroutine singleComponentTwoPhasePSflash

  !-----------------------------------------------------------------------------
  !> Calculate residual for single component PS system
  !>
  !> \author MH, 2014-10-17
  !-----------------------------------------------------------------------------
  subroutine sfun(f,Tv,param)
    use eos, only: entropy
    implicit none
    real, dimension(1), intent(out) :: f !< Temperature [K]
    real, dimension(1), intent(in) :: Tv !< Temperature [K]
    real, dimension(nc+6), intent(inout) :: param !< Parameter vector
    !real :: of !< Objective function value 
    ! Locals
    real:: t, p, s, sspec
    real, dimension(nc) :: Z
    integer :: phase

    t = Tv(1)
    sspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = int(param(nc+5))

    call entropy(T,P,Z,phase,s)
    f = (s - sspec)/max(abs(sspec),1.0)
    if (f(1) > 0.0) then
      param(nc+4) = t ! Tmax
    else
      param(nc+3) = t ! Tmin
    endif
  end subroutine sfun

  !-----------------------------------------------------------------------------
  !> Calculate differential for single component PS system.
  !> 
  !> \author MH, 2012-01-30
  !-----------------------------------------------------------------------------
  subroutine sdiff(J,Tv,param)
    use eos, only: enthalpy
    implicit none
    real, dimension(1), intent(in)    :: Tv !< Temperature [K]
    real, dimension(nc+6), intent(in) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: J !< Jacobean matrix
    ! Locals
    real:: t, p, h, dhdt, sspec
    real, dimension(nc) :: Z
    integer :: phase

    t = Tv(1)
    sspec = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    phase = int(param(nc+5))

    call enthalpy(t,p,z,phase,h,dhdt=dhdt)
    J(1,1) = dhdt/(t*max(abs(sspec),1.0))
  end subroutine sdiff

  !-----------------------------------------------------------------------------
  !> Premature termination when solution is outside valid temperature region.
  !> 
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  function spremReturn(Tv,dTv,param,n,np) result(doReturn)
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
    if (Tv(1) < Tmin + small .and. dTv(1) > 0.0) then ! s(Tmin) - sspec > 0
      doReturn = .true.
    endif
    if (Tv(1) > Tmax - small .and. dTv(1) < 0.0) then ! s(Tmax) - sspec < 0
      doReturn = .true.
    endif
    if (param(nc+4)-param(nc+3) < param(nc+6)) then ! Not able to meet tolerance
      doReturn = .true.
    endif
  end function spremReturn

  !-----------------------------------------------------------------------------
  !> Full newton solver
  !> Require initial guess for temperature and gas and liquid phase composition.
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  subroutine twoPhasePSflashFull(t,p,Z,beta,betaL,X,Y,sspec,phase,Tmax,Tmin,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve
    use numconstants, only: machine_prec
    implicit none
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(inout) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: sspec !< Specified entropy [J/mol/K]
    integer, intent(inout) :: phase !< Phase identifier
    real, intent(in) :: Tmax, Tmin !< Temperature limits [K]
    integer, intent(out) :: ierr ! Error message 
    ! Locals
    type(nonlinear_solver) :: solver
    real :: t0, X0(nc), Y0(nc), beta0, betaL0
    real, dimension(1+nc) :: Xv,Xvmin,Xvmax
    real, dimension(2*nc+3) :: param
    t0 = t
    X0 = X
    Y0 = Y
    beta0 = beta
    betaL0 = betaL
    !
    param(1) = sspec
    param(2) = p
    param(3:nc+2) = Z
    param(nc+3) = 0 ! Iteration
    Xv(1:nc) = beta*Y
    param(nc+4:2*nc+3) = Z - Xv(1:nc)
    Xv(nc+1) = T
    Xvmin(1:nc) = 0.0
    Xvmax(1:nc) = 1.0
    Xvmin(nc+1) = Tmin
    Xvmax(nc+1) = Tmax
    solver%ls_max_it = 3
    solver%abs_tol = 2.0e4*machine_prec
    solver%symmetric_jac = .true.
    call nonlinear_solve(solver,fullfun,fulldiff,fulldiff,fullLimitXv,fullPremReturn,&
         fullSetXv,Xv,Xvmin,Xvmax,param)
    if (solver%exitflag /= 0) then
      ! print *,'Temperature at exit',Tv(1)
      ! print *,'Temperature upper limit',Tmax
      ! print *,'Temperature lower limit',Tmin
      ! print *,'Initial temperature',tInitial
      ! print *,'Pressure',p
      ! print *,'Entropy',sspec
      ! print *,'Comp.',Z
      ! print *,'Number of iterations',solver%iter
      ierr = 1
      if (beta > 0.5) then
        phase = VAPPH
      else
        phase = LIQPH
      endif
    else
      ierr = 0
      ! Set solution
      t = Xv(nc+1)
      beta = sum(Xv(1:nc))
      Y = Xv(1:nc)/beta
      betaL = sum(param(nc+4:2*nc+3))
      X = param(nc+4:2*nc+3)/betaL
    endif
  end subroutine twoPhasePSflashFull
  
  !-----------------------------------------------------------------------------
  !> Calculate residual for full PS system
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
    real:: t, p, sspec(2), beta, betaL
    real, dimension(nc) :: X,Y,Z

    t = exp(Xv(1+nc))
    sspec(1) = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)

    beta = sum(Xv(1:nc))
    Y = Xv(1:nc)/beta
    betaL = sum(param(nc+4:2*nc+3))
    X = param(nc+4:2*nc+3)/betaL

    call getStateFunc(t,p,Z,beta,betaL,X,Y,'PS',sspec,f)
  end subroutine fullfun

  !-----------------------------------------------------------------------------
  !> Calculate differential for full PS system.
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
    real:: t, p, sspec(2), beta, betaL, f(nc+1)
    real, dimension(nc) :: X,Y,Z
    integer :: iter
    logical :: simpleMatrix

    t = Xv(1+nc)
    sspec(1) = param(1)
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

    call getStateFuncMatrix(t,p,Z,beta,betaL,X,Y,'PS',sspec,J,f,simpleMatrix=simpleMatrix)
  end subroutine fulldiff

  !-----------------------------------------------------------------------------
  !> Premature termination for full SP solver.
  !> 
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  function fullPremReturn(Xv,dXv,param,n,np) result(premature_return)
    use eos, only: enthalpy
    implicit none
    integer, intent(in)               :: n !< Size of Tv and dTv
    integer, intent(in)               :: np !< Size of param
    real, dimension(n), intent(in)    :: Xv !< Variable vector
    real, dimension(np), intent(in)   :: param !< Parameter vector
    real, dimension(n), intent(in)    :: dXv !< Change in variable vector
    logical                           :: premature_return !< Terminate minimization?
    ! Locals
    real :: t,p,beta,betaL,Htwo,Hsingle,hl,hg
    integer :: phase
    real, dimension(nc) :: Z,L,X,Y

    beta = sum(Xv(1:nc))
    premature_return = .false.
    if (beta < dropPhaseTestBeta .or. beta > 1.0 - dropPhaseTestBeta) then
      ! Try removing phase - do we get an decrease in H?
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
      call enthalpy(t,p,y,VAPPH,hg,residual=.true.)
      call enthalpy(t,p,x,LIQPH,hl,residual=.true.)
      Htwo = beta*hg+betaL*hl
      call enthalpy(t,p,z,phase,Hsingle,residual=.true.)

      if (Hsingle < Htwo) then ! Minimize enthalpy
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

    ! print *,'beta: ',sum(V)+sum(L)
    ! print *,'V: ',V
    ! print *,'L: ',L
    ! print *,'Z-L-V: ',Z-L-V
    
  end subroutine fullSetXv

  !-----------------------------------------------------------------------------
  !> Set PS-flash tolerance
  !> Caution, write to module variable, and is not thread safe
  !>
  !> \author MH, 2015-03
  !-----------------------------------------------------------------------------
  subroutine setPStolerance(tol)
    implicit none
    real, intent(in) :: tol !< Tolerance for ps-flash [-]
    !
    tolerance = tol
  end subroutine setPStolerance

  !-----------------------------------------------------------------------------
  !> Get PS-flash tolerance
  !>
  !> \author MH, 2015-03
  !-----------------------------------------------------------------------------
  function getPStolerance() result(tol)
    implicit none
    real :: tol !< Tolerance of ps-flash [-]
    !
    tol = tolerance
  end function getPStolerance

end module ps_solver
