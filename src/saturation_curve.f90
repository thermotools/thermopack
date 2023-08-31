module saturation_curve
  use eos, only: thermo, entropy, specificvolume
  use thermopack_constants, only: clen, LIQPH, VAPPH, MINGIBBSPH, SINGLEPH, verbose
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model, &
       get_active_eos, base_eos_param, tpTmax, tpTmin
  use numconstants, only: machine_prec
  use puresaturation, only: puresat
  use saturation
  use nonlinear_solvers
  implicit none
  private
  save

  integer, parameter :: ISO_P=1, ISO_T=2, ISO_S=3, ISO_H=4
  character(len=1), parameter, dimension(4) :: ISO_LABEL = (/ "P", "T", "S", "H" /)

  integer, parameter :: AZ_NONE = 0, AZ_PAEP = 1, AZ_CAEP = 2, AZ_HAEP = 3
  ! Azeotropic end point
  type aep
    integer :: type = AZ_NONE
    logical :: found = .false.
    real :: t = 0
    real :: p = 0
    real :: vg = 0
    real :: vl = 0
    real :: x(2) = 0
  contains
    procedure, public :: print => aep_print
  end type aep

  ! TODO:
  ! sat_successive need to use beta_in, nbot only beta=0 and beta=1

  public :: aep,AZ_NONE,AZ_PAEP,AZ_CAEP,AZ_HAEP
  public :: envelopePlot,singleCompSaturation
  public :: envelope_isentrope_cross, envelope_isentrope_cross_single
  public :: newton_extrapolate, changeformulation
  public :: extrapolate_to_saturation_line
  public :: extrapolate_beta
  public :: ISO_P, ISO_T, ISO_S, ISO_H, ISO_LABEL
  public :: pure_fluid_saturation_wrapper

contains

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along the saturation line
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  subroutine newton_extrapolate(Z,Xvar,dXdS,beta,s,ln_s)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc+2), intent(out) :: dXdS
    real, dimension(nc+2), intent(in) :: Xvar
    real, intent(in) :: beta
    real, intent(in) :: ln_s
    integer, intent(in) :: s
    ! Locals
    real, dimension(nc+2,nc+2) :: Jac
    integer, dimension(nc+2) :: INDX
    integer :: INFO
    real, dimension(nc+3) :: param

    param(1:nc) = Z
    param(nc+1) = beta
    param(nc+2) = real(s)
    param(nc+3) = ln_s

    call sat_diff_newton(Jac,Xvar,param)
    dXdS(1:nc+1) = 0.0
    dXdS(nc+2) = 1.0

    ! Solve equation system
    call DGESV( nc+2, 1, Jac, nc+2, INDX, dXdS, nc+2, INFO )

  end subroutine newton_extrapolate

  !-----------------------------------------------------------------------------
  !> Extrapolate in beta
  !>
  !> \author MH, 2017-06
  !-----------------------------------------------------------------------------
  subroutine extrapolate_beta(Z,Xvar,beta,s,ln_s,dXvardBeta)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc+2), intent(out) :: dXvardBeta
    real, dimension(nc+2), intent(in) :: Xvar
    real, intent(in) :: beta
    real, intent(in) :: ln_s
    integer, intent(in) :: s
    ! Locals
    real, dimension(nc) :: dXdBeta, dYdBeta, X, Y, K, lnfugL, lnfugV
    real, dimension(nc,nc) :: lnfugnL, lnfugnV
    real :: t, p
    real, dimension(nc+2,nc+2) :: Jac
    integer, dimension(nc+2) :: INDX
    integer :: INFO, j, i
    real, dimension(nc+3) :: param

    param(1:nc) = Z
    param(nc+1) = beta
    param(nc+2) = real(s)
    param(nc+3) = ln_s
    call sat_diff_newton(Jac,Xvar,param)

    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))
    X = Z/(1-beta+beta*K)
    Y = K*Z/(1-beta+beta*K)
    call thermo(t,p,X,LIQPH,lnfugL,lnfugx=lnfugnL)
    call thermo(t,p,Y,VAPPH,lnfugV,lnfugx=lnfugnV)

    ! Need differentials wrpt. beta
    dYdbeta = 0.0
    dXdbeta = 0.0
    do j=1,nc
      if (Z(j) > 0.0) then
        dXdbeta(j) = -(Y(j)-X(j))/Z(j)
      endif
    enddo
    dYdbeta = Y*dXdbeta
    dXdbeta = X*dXdbeta
    dXvardBeta = 0.0
    do i=1,nc
      dXvardBeta(i) = sum(lnfugnV(i,:)*dYdbeta) - sum(lnfugnL(i,:)*dXdbeta)
    enddo
    dXvardBeta(nc+1) = sum(dYdbeta-dXdbeta)
    dXvardBeta = -dXvardBeta

    ! Solve equation system
    call DGESV( nc+2, 1, Jac, nc+2, INDX, dXvardBeta, nc+2, INFO )

  end subroutine extrapolate_beta

  !-----------------------------------------------------------------------------
  !> Extrapolate two-phase flash solution to phase boundary
  !! and solve for saturation point
  !!
  !! \author MH, 2017-06
  !-----------------------------------------------------------------------------
  subroutine extrapolate_to_saturation_line(t,p,X,Y,Z,beta,extrap,ierr,dtds_sat,dpds_sat)
    use numconstants, only: machine_prec
    use eos, only: twoPhaseEnthalpy, twoPhaseEntropy
    use thermopack_constants, only: TWOPH
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, intent(inout) :: t, p, beta
    real, dimension(nc), intent(inout) :: X, Y
    integer, intent(in) :: extrap ! 1 = isobaric, 2 = isothermal
    integer, intent(out) :: ierr
    real, intent(out) :: dpds_sat !< dpds along saturation line
    real, intent(out) :: dtds_sat !< dtds along saturation line
    ! Locals
    real :: ln_s, dBeta, iter, dBeta_sub, p0, t0, dS, dBetaMax
    integer :: s, i, smax(1)
    real, dimension(nc+2) :: Xvar, dXvardBeta, dXvardS, Xold
    real, dimension(nc) :: K
    real :: hs
    p0 = p
    t0 = t
    ! How to extrapolate?
    if (extrap == ISO_P) then
      s = nc + 2
      ln_s = log(p)
    else if (extrap == ISO_T) then
      s = nc + 1
      ln_s = log(t)
    else if (extrap == ISO_S) then
      s = nc + 3
      hs = twoPhaseEntropy(t,p,z,x,y,beta,TWOPH)
      ln_s = hs
    else if (extrap == ISO_H) then
      s = nc + 4
      hs = twoPhaseEnthalpy(t,p,z,x,y,beta,TWOPH)
      ln_s = hs
    endif

    ! Set variable vector
    do i=1,nc
      if (Z(i) > 0.0) then
        K(i) = Y(i)/X(i)
      else
        K(i) = 1.0
      endif
    enddo
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)

    ! Extrapolate
    if (beta > 0.5) then
      dBeta = 1.0 - beta
    else
      dBeta = - beta
    endif
    dBetaMax = 0.1
    do while (abs(dBeta) > 0.0)
      Xold = Xvar
      dBeta_sub = sign(min(dBetaMax, abs(dBeta)), dBeta)
      call extrapolate_beta(Z,Xvar,beta,s,ln_s,dXvardBeta)
      beta = beta + dBeta_sub
      dBeta = dBeta - dBeta_sub
      if (beta < machine_prec*5.0) then
        beta = 0.0
        dBeta = 0.0
      else if (beta > 1.0 - machine_prec*5.0) then
        beta = 1.0
        dBeta = 0.0
      endif
      Xvar = Xvar + dXvardBeta*dBeta_sub
      if (maxval(abs(Xvar(1:nc))) < 0.05) then
        ! Avoid getting too close to critical point
        ! Fixate K-value
        smax = maxloc(abs(Xvar(1:nc)))
        s = smax(1)
        ln_s = sign(max(Xvar(s),0.02),Xvar(s))
      endif
      K = exp(Xvar(1:nc))
      t = exp(Xvar(nc+1))
      p = exp(Xvar(nc+2))
      ! Solve for saturation point
      iter = sat_newton(Z,K,t,p,beta,s,ln_s,ierr)
      if (ierr /= 0) then
        ! Are we shooting out of the two-phase region
        ! Try reducing step size
        dBetaMax = max(abs(dBeta_sub), dBetaMax) / 4.0
        Xvar = Xold
        beta = beta - dBeta_sub
        dBeta = dBeta + dBeta_sub
        if (ierr /= 0 .and. dBetaMax < 1.0e-3) then
          exit ! Not able to extrapolate
        endif
      endif
    enddo
    if (ierr == 0 .and. s <= nc) then
      ! Try solving for T or P
      Xvar(1:nc) = log(K)
      Xvar(nc+1) = log(t)
      Xvar(nc+2) = log(p)
      Xold = Xvar
      s = nc + extrap
      ! if (extrap == ISO_P) then
      !   s = nc+2
      !   ln_s = log(p)
      ! else if (extrap == ISO_T) then
      !   s = nc+1
      !   ln_s = log(t)
      ! else if (extrap == ISO_S) then
      !   s = nc+3
      !   ln_s = hs
      ! else if (extrap == ISO_H) then
      !   s = nc+4
      !   ln_s = hs
      ! endif
      call newton_extrapolate(Z,Xvar,dXvardS,beta,s,ln_s)
      if (extrap == ISO_P) then
        ln_s = log(p0)
        dS = ln_s - Xvar(s)
      else if (extrap == ISO_T) then
        ln_s = log(t0)
        dS = ln_s - Xvar(s)
      else !ISO_S/ISO_H
        ln_s = hs
        if (extrap == ISO_S) then
          hs = twoPhaseEntropy(t,p,z,x,y,beta,TWOPH)
        else if (extrap == ISO_H) then
          hs = twoPhaseEnthalpy(t,p,z,x,y,beta,TWOPH)
        endif
        dS = (ln_s - hs)/max(abs(ln_s),1.0)
      endif
      Xvar = Xvar + dXvardS*dS
      ! Solve for saturation point
      iter = sat_newton(Z,K,t,p,beta,s,ln_s,ierr)
      if (ierr /= 0) then
        ierr = -100 ! Trace envelope to find crossing
        ! Reset K, T and P
        K = exp(Xold(1:nc))
        T = exp(Xold(1+nc))
        P = exp(Xold(2+nc))
      endif
    endif
    ! Get differentials along saturation line
    if (ierr == 0 .OR. ierr == -100) then
      Xvar(1:nc) = log(K)
      Xvar(nc+1) = log(t)
      Xvar(nc+2) = log(p)
      X = Z/(1-beta+beta*K)
      Y = K*Z/(1-beta+beta*K)
      call newton_extrapolate(Z,Xvar,dXvardS,beta,s,ln_s)
      dpds_sat = p*dXvardS(nc+2)
      dtds_sat = t*dXvardS(nc+1)
    else
      dpds_sat = 0.0
      dtds_sat = 0.0
    endif
  end subroutine extrapolate_to_saturation_line

  !-----------------------------------------------------------------------------
  !> Plot saturation line in TP space
  !>
  !> \author MH, 2012-03-05, Modified EA, 2014-10, 2015-01
  !-----------------------------------------------------------------------------
  subroutine envelopePlot(Z,T_init,p_init,spec,beta_in,Pmax,nmax,&
       Ta,Pa,Ki,betai,n,criconden,crit,dS_override,&
       exitOnTriplePoint,Tme,step_size_factor)
    use critical, only: calcCritical, calcStabMinEig, calcCriticalTV
    use thermo_utils, only: isSingleComp
    use eosTV, only: pressure
    use eosdata, only: eosLK
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
    real,           intent(out) :: Ki(nmax,nc) ! Equilibrium constants
    real,           intent(out) :: betai(nmax) ! Phase fraction
    integer,        intent(out) :: n           ! Number of points found
    real, optional, intent(out) :: criconden(4)! tcb,pcb,tct,pct
    real, optional, intent(out) :: crit(2)     ! tc, pc
    real, optional, intent(in)  :: dS_override ! Override step length
    logical, optional, intent(in) :: exitOnTriplePoint ! Exit if passing triple point
    real, optional, intent(in)  :: Tme ! Exit on temperature
    real, optional, intent(in)  :: step_size_factor ! Scale the default step size limits, and in effect the step size
    ! Internal:
    real  :: T,p
    real, dimension(nc) :: K, lnfugG, lnfugL
    real, dimension(nc+2) :: dXdS, dXdSold
    real, dimension(nc+2) :: Xvar ,Xold, XvarCricon
    real :: dS, tuning, sgn, Pstart, Tmin, Tmax
    integer :: iter,s,return_iter,ierr,phase
    integer :: smax,zmax
    real :: ln_spec,beta,lambda,lambda_old,lambda_min
    real :: dT,dP,dTold,dPold,dPdT,dTdP,dPdTold,dTdPold
    logical :: cricon, doBub,calcCrit
    logical :: excessive_jump
    logical :: have_switched_formulation
    logical :: should_switch_formulation
    logical :: recalculate
    logical :: exit_after_saving
    real, parameter :: maxdT = 25.0, maxdP = 10.0
    real :: dS_max, dS_min, v
    integer :: n_crit
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    ! Set initial guess for first point
    T = T_init
    p = p_init

    have_switched_formulation = .false.
    should_switch_formulation = .false.
    recalculate = .false.
    exit_after_saving = .false.

    cricon = .false.
    if (present(criconden) .and. (beta_in == 0.0 .or. beta_in == 1.0)) then
      cricon = .true.
      criconden = 0.0
    endif

    calcCrit = .false.
    if (present(crit) .and. (beta_in == 0.0 .or. beta_in == 1.0)) then
      calcCrit = .true.
      crit = 0.0
      lambda_old = 500.0
      lambda_min = 400.0
      lambda = lambda_old
      n_crit = 0
    endif

    n = 0
    beta = beta_in

    if (spec == specP) then
      ln_spec = log(p)
    else if (spec == specT) then
      ln_spec = log(t)
    else
      print *,'envelopePlot: Error in initial spec'
      call exit(1)
    endif

    zmax = maxloc(Z,dim=1)
    if (isSingleComp(Z)) then
      dS_max = 0.2e5
      if (present(step_size_factor)) then
        dS_max = dS_max*step_size_factor
      endif
      call singleCompSaturation(Z,t,p,spec,Ta,Pa,nmax,n,maxDeltaP=dS_max)
      Ki = 1.0
      betai = 0.0
      return
    endif

    if (present(dS_override)) then
      dS_max = dS_override
    else
      dS_max = 0.25
      if (present(step_size_factor)) then
        dS_max = dS_max*step_size_factor
      endif
    endif
    dS_min = min(0.25*dS_max, 0.05)
    dS = dS_min
    tuning = 1.2
    sgn = 1.0

    ! Temperature limits
    Tmin = tpTmin
    Tmax = tpTmax
    if (present(Tme)) Tmin = Tme

    ! Generate initial K values
    if (beta_in < 0.5) then
      doBub = .true.
    else
      doBub = .false.
    endif

    ! Find temperature/pressure in correct range
    if (spec == specP) then
      call PureSat(T,P,Z,.true.)
    else
      call PureSat(T,P,Z,.false.)
    endif
    call thermo(t,p,z,VAPPH,lnfugG)
    call thermo(t,p,z,LIQPH,lnfugL)
    if (abs(sum(lnfugG-lnfugL)) > 1.0e-3) then
      K = exp(lnfugL-lnfugG)
    else
      call sat_wilsonK(Z,K,t,p,spec,doBub)
    endif
    ! Set early return
    return_iter = 200
    call sat_successive(Z,K,t,p,spec,doBub,return_iter)
    s = ispec(spec)
    iter = sat_newton(Z,K,t,p,beta,s,ln_spec)
    if (iter >= sat_max_iter) then
      print *,'envelopePlot: Initial point not found.'
      call exit(1)
    endif
    if (verbose) then
      print *,'*****************************'
      print *,'Phase envelope:'
      print *,'T,P:',t,p
    endif

    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)
    Pstart = p

    ! Set output
    n = 1
    call setEnvelopePoint(Xvar,Z,beta,n,Ta,Pa,Ki,betai,nmax)

    if (cricon) then
      call dPdTcalc(dPold,dTold,Z,K,beta,t,p)
    endif

    do while (n < nmax)
      dXdSold = dXdS
      call newton_extrapolate(Z,Xvar,dXdS,beta,s,ln_spec)
      smax = maxloc(abs(dXdS),dim=1)
      if ((.not. smax == s) .and. n > 1) then
        s = smax
        ! Rescaling the sensitivities
        sgn = sign(1.0,Xvar(s) - Xold(s))
        dXdS = dXdS / dXdS(s)
        dXdSold = dXdSold / dXdSold(s)
      endif

      !if (i > 1) then
      !  snew = Xvar(s) + dS
      !  call poly(nc+2,Xold(s),Xvar(s),Xold,Xvar,dXdSold,dXdS,snew,Xnew)
      !else

      Xold = Xvar
      Xvar = Xold + dXdS*dS*sgn
      if (s == nc+2 .and. Xvar(s) > log(Pmax)) then
        ! Solve for Pmax
        Xvar = Xold + dXdS*(log(Pmax) - Xold(s))
      endif
      K = exp(Xvar(1:nc))
      t = exp(Xvar(nc+1))
      p = exp(Xvar(nc+2))
      ln_spec = Xvar(s)

      iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
      excessive_jump = (abs(exp(Xold(nc+1)) - t) > maxdT) .OR. &
           (abs(exp(Xold(nc+2)) - p)/1e5 > maxdP)
      if (ierr /= 0 .OR. excessive_jump) then
        ! Something went wrong.
        ! Attempt to make the step shorter.
        Xvar = Xold + dXdS*dS*sgn*0.5
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        ln_spec = Xvar(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        excessive_jump = (abs(exp(Xold(nc+1)) - t) > maxdT) .OR. &
             (abs(exp(Xold(nc+2)) - p)/1e5 > maxdP)
      endif
      if (ierr /= 0 .OR. excessive_jump) then
        ! Something went wrong.
        ! Attempt to make the step longer.
        Xvar = Xold + dXdS*dS*sgn*2.0
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        ln_spec = Xvar(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        excessive_jump = (abs(exp(Xold(nc+1)) - t) > maxdT) .OR. &
             (abs(exp(Xold(nc+2)) - p)/1e5 > maxdP)
      endif
      if (ierr /= 0 .OR. excessive_jump) then
        ! Could be close to single component, and have trouble
        ! passing the critical point.
        if (s > nc) then
          ! K-value not fixed close to critical point
          smax = maxloc(abs(dXdS(1:nc)),dim=1)
          sgn = sign(1.0,sgn*dXdS(s)*dXdS(smax))
          s = smax ! Lock K-value
          dXdS = dXdS / dXdS(s) ! Rescale dXds
          ! Keep pressure and temperature constant
          dXdS(nc+1:nc+2) = 0.0
        endif
        if (abs(Xold(s) + dS*sgn) < 1.0e-4) then
          ! Adapt dS to overshoot critical point
          dS = 1.0e-4 + abs(Xold(s))
        endif
        Xvar = Xold + dXdS*dS*sgn
        K = exp(Xvar(1:nc))
        ln_spec = Xvar(s)
        if (.not. have_switched_formulation) then
          ! Need to force change of formulation after extrapolation
          call changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
        endif
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        excessive_jump = (abs(exp(Xold(nc+1)) - t) > maxdT) .OR. &
             (abs(exp(Xold(nc+2)) - p)/1e5 > maxdP)
        if (.not. have_switched_formulation) then
          if (ierr == 0 .AND. .not. excessive_jump) then
            ! Successfully switched formulation
            have_switched_formulation = .true.
            should_switch_formulation = .false.
          else
            ! Revert to original formulation
            call changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
          endif
        endif
      endif
      if (ierr /= 0 .OR. excessive_jump) then
        ! Give up
        exit
      endif

      Xvar(1:nc) = log(K)
      Xvar(nc+1) = log(t)
      Xvar(nc+2) = log(p)

      !Exit at thermo limit or defined pressure
      if (p < Pstart) then
        s = nc+2
        recalculate = .true.
        ln_spec = log(Pstart)
      else if (p > Pmax) then
        s = nc+2
        recalculate = .true.
        ln_spec = log(Pmax)
      endif
      !Exit at temperature specified temperature
      if (present(Tme)) then
        ! Is temperature decreasing? - And below Tme?
        if (Xvar(nc+1) - Xold(nc+1) < 0.0 .and. T < Tme) then
          s = nc+1
          ln_spec = log(Tme)
          recalculate = .true.
        endif
      else if (T < Tmin) then
        ! Exit at thermo temperature limit
        s = nc+1
        ln_spec = log(Tmin)
        recalculate = .true.
      endif

      if (recalculate) then
        ! Extrapolate from previous point
        call newton_extrapolate(Z,Xold,dXdS,beta,s,ln_spec)
        dS = ln_spec - Xold(s) ! Sign included
        Xvar = Xold + dXdS*dS
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        Xvar(1:nc) = log(K)
        Xvar(nc+1) = log(t)
        Xvar(nc+2) = log(p)
        exit_after_saving = .true.
      endif

      ! Set output
      n = n + 1
      call setEnvelopePoint(Xvar,Z,beta,n,Ta,Pa,Ki,betai,nmax)
      if (n == nmax .OR. exit_after_saving) then
        exit ! Exit before cricon calculations
      endif

      if (present(exitOnTriplePoint)) then
        if (exitOnTriplePoint) then
          ! Have we passed a triple point?
          if (passedTriplePoint(Xold,Xvar,Z,beta)) then
            return
          endif
        endif
      endif

      !Exit at  defined pressure
      if (p >= Pmax*(1.0 - machine_prec*100)) then
        ! Attempt to solve for Pmax
        ln_spec = log(Pmax)
        s = nc+2
        ! Rescaling the sensitivities
        dXdS = dXdS / dXdS(s)
        Xvar = Xold + dXdS*(ln_spec-Xold(s))
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        if (ierr == 0) then
          ! Replace last point
          Xvar(1:nc) = log(K)
          Xvar(nc+1) = log(t)
          Xvar(nc+2) = log(p)
          call setEnvelopePoint(Xvar,Z,beta,n,Ta,Pa,Ki,betai,nmax)
        endif
        exit
      endif

      !Exit at thermo temperature limit, or defined pressure
      if (p < Pstart .OR. T < Tmin) then
        call newton_extrapolate(Z,Xvar,dXdS,beta,s,ln_spec)
        if (p < Pstart) then
          s = nc + 2
          ln_spec = log(Pstart)
        else if (T < Tmin) then
          s = nc + 1
          ln_spec = log(Tmin)
        endif
        ! Rescaling the sensitivities
        dXdS = dXdS / dXdS(s)
        Xvar = Xvar + dXdS*(ln_spec-Xold(s))
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        if (ierr == 0) then
          ! Replace last point
          Xvar(1:nc) = log(K)
          Xvar(nc+1) = log(t)
          Xvar(nc+2) = log(p)
          call setEnvelopePoint(Xvar,Z,beta,n,Ta,Pa,Ki,betai,nmax)
        endif
        exit
      endif

      if (calcCrit) then
        lambda_old = lambda
        if (beta > 0.5) then
          phase = VAPPH
        else
          phase = LIQPH
        endif
        lambda = calcStabMinEig(t,p,z,phase)
        lambda_min = min(lambda,lambda_min)
        if (lambda > lambda_old .and. lambda_min == lambda_old) then
          crit(1) = Ta(n-1)
          crit(2) = Pa(n-1)
          n_crit = n-1
        endif
      endif

      if (cricon) then
        call dPdTcalc(dP,dT,Z,K,beta,t,p)
        if (abs(dT) > abs(dP)) then
          ! Look for cricondenbar
          dPdT = dP/dT
          dPdTold = dPold/dTold
          if (dPdT*dPdTold < 0.0) then
            XvarCricon = Xvar
            call criconden_solve(Z,XvarCricon,beta,max(Pa(n),Pa(n-1)),&
                 min(Pa(n),Pa(n-1)),max(Ta(n),Ta(n-1)),min(Ta(n),Ta(n-1)),&
                 .false.,ierr)
            if (ierr == 0) then
              if (criconden(2) < exp(XvarCricon(nc+2))) then
                criconden(1) = exp(XvarCricon(nc+1))
                criconden(2) = exp(XvarCricon(nc+2))
              endif
              ! Add point to envelope
              Ta(n+1) = Ta(n)
              Pa(n+1) = Pa(n)
              Ki(n+1,:) = Ki(n,:)
              betai(n+1) = betai(n)
              call setEnvelopePoint(XvarCricon,Z,beta,n,Ta,Pa,Ki,betai,nmax)
              n = n + 1
            endif
          endif
        else
          ! Look for cricondentherm
          dTdP = dT/dP
          dTdPold = dTold/dPold
          if (dTdP*dTdPold < 0.0) then
            XvarCricon = Xvar
            call criconden_solve(Z,XvarCricon,beta,max(Pa(n),Pa(n-1)),&
                 min(Pa(n),Pa(n-1)),max(Ta(n),Ta(n-1)),min(Ta(n),Ta(n-1)),&
                 .true.,ierr)
            if (ierr == 0) then
              if (criconden(3) < exp(XvarCricon(nc+1))) then
                criconden(3) = exp(XvarCricon(nc+1))
                criconden(4) = exp(XvarCricon(nc+2))
              endif
              ! Add point to envelope
              Ta(n+1) = Ta(n)
              Pa(n+1) = Pa(n)
              Ki(n+1,:) = Ki(n,:)
              betai(n+1) = betai(n)
              call setEnvelopePoint(XvarCricon,Z,beta,n,Ta,Pa,Ki,betai,nmax)
              n = n + 1
            endif
          endif
        endif
        dTold = dT
        dPold = dP
      endif

      ! While the possibility to switch formulation may have arisen,
      ! it is not necessarily yet possible to do it error-free.
      ! If the flag should_switch_formulation has been raised, attempt
      ! to switch until it can be done error-free.
      if (should_switch_formulation) then
        call changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        ! If error-free, change phase now. If not, try again next time.
        if (ierr == 0 .and. &
             abs(Ta(n)-t)/max(Ta(n),t) < 1.0e-5 .and. &
             abs(Pa(n)-p)/max(Pa(n),p) < 1.0e-5) then
          have_switched_formulation = .true.
          should_switch_formulation = .false.
        else
          call changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
        endif
      endif

      if (.not. have_switched_formulation) then
        ! Check if we have entered the SINGLEPH-area for both phases.
        ! If so, we can change formulation (beta)
        should_switch_formulation = shouldSwitchFormulation(T,P,beta,Z,K)
      endif

      ! Tune dS up or down based on how fast sat_newton converged
      if (iter < 3) then
        dS = dS * tuning
      else if (iter > 5) then
        dS = dS * (1.0 - tuning)
      endif
      dS = max(min(dS,dS_max),dS_min)
    enddo

    if (calcCrit) then
      if (crit(1) > 0.0 .and. crit(2) > 0.0) then
        ! Use initial guess from envelope mapping
        T = crit(1)
        P = crit(2)
        call specificvolume(t,p,Z,LIQPH,v)
        call calcCritical(crit(1),crit(2),Z,VAPPH,ierr)
      else
        ! Force calculation using calcCriticalTV, without
        ! an initial guess
        ierr = -1
        T = -1.0
        v = -1.0
      endif
      if (ierr /= 0 .and. act_mod_ptr%eosidx /= eosLK) then
        ! MH: Should eventually use only this method for all models but eosLK
        call calcCriticalTV(t,v,Z,ierr,1.0e-8)
        if (ierr == 0) then
          p = pressure(t,v,Z)
          crit(1) = t
          crit(2) = p
        endif
      endif
      if (ierr /= 0) then
        crit = -1.0
      else if (n < nmax) then
        ! Add critical point to envelope output
        call insertCriticalPoint(crit(1),crit(2),Ta,Pa,Ki,betai,nmax,n_crit,n)
      endif
    endif

  end subroutine envelopePlot

  !-----------------------------------------------------------------------------
  !> Add critical point to envelope points
  !>
  !> \author MH, 2017-06.
  !-----------------------------------------------------------------------------
  subroutine insertCriticalPoint(Tc,Pc,Ta,Pa,Ki,betai,nmax,n_crit,n)
    implicit none
    real, intent(in) :: Tc, Pc
    integer, intent(in) :: nmax
    integer, intent(inout) :: n, n_crit
    real, dimension(nmax), intent(inout) :: Ta,Pa
    real, dimension(nmax,nc), intent(inout) :: Ki
    real, dimension(nmax), intent(inout) :: betai
    ! Locals
    real, dimension(nc) :: K, Km1, Kp1
    integer             :: i
    real, dimension(nmax) :: Ta_cpy,Pa_cpy
    real, dimension(nmax,nc) :: Ki_cpy
    real, dimension(nmax) :: betai_cpy
    ! Store copy of output
    Ta_cpy(1:n) = Ta(1:n)
    Pa_cpy(1:n) = Pa(1:n)
    Ki_cpy(1:n,:) = Ki(1:n,:)
    betai_cpy(1:n) = betai(1:n)
    ! Critical point located between n_crit and n_crit + 1
    ! or between n_crit - 1 and n_crit
    K = Ki(n_crit,:) - 1.0
    Km1 = Ki(n_crit - 1,:) - 1.0
    Kp1 = Ki(n_crit + 1,:) - 1.0
    if (maxval(K*Kp1) <= 0.0) then
      n_crit = n_crit + 1
    else if (maxval(K*Km1) <= 0.0) then
      n_crit = n_crit
    else
      n_crit = 0
    endif
    if (n_crit > 0) then
      Ta(n_crit) = Tc
      Pa(n_crit) = Pc
      Ki(n_crit,:) = 1.0
      !betai(n_crit) Intentionally left as is
      do i=n_crit,n
        Ta(i+1) = Ta_cpy(i)
        Pa(i+1) = Pa_cpy(i)
        Ki(i+1,:) = Ki_cpy(i,:)
        betai(i+1) = betai_cpy(i)
      enddo
      n = n + 1
    endif
  end subroutine insertCriticalPoint

  !-----------------------------------------------------------------------------
  !> Change phase to solve for
  !>
  !> \author MH, 2014-03
  !-----------------------------------------------------------------------------
  subroutine changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
    implicit none
    real, intent(inout) :: beta
    real, dimension(nc+2), intent(inout) :: Xvar, Xold
    real, dimension(nc), intent(out) :: K
    integer, intent(inout) :: s
    real, intent(inout) :: ln_spec,sgn
    !
    ! Change formulation (Swap phases)
    beta = 1.0 - beta
    Xvar(1:nc) = -Xvar(1:nc) ! K = 1.0/K
    Xold(1:nc) = -Xold(1:nc) ! K = 1.0/K
    K = exp(Xvar(1:nc))
    ln_spec = Xvar(s)
    if (s <= nc) then
      sgn = -sgn
    endif
  end subroutine changeFormulation

  !-----------------------------------------------------------------------------
  !> Check if it is possible to swap phase flags
  !>
  !> \author MH, 2014-03
  !-----------------------------------------------------------------------------
  function shouldSwitchFormulation(T,P,beta,Z,K) result(should_switch)
    implicit none
    real, intent(in) :: T,P
    real, intent(in) :: beta
    real, dimension(nc), intent(in) :: Z, K
    logical :: should_switch
    ! Locals
    integer :: ophase
    real, dimension(nc) :: lnfug, X, Y
    should_switch = .false.
    call thermo(t,p,z,MINGIBBSPH,lnfug,ophase=ophase)
    if (ophase == SINGLEPH) then
      if (beta > 0.5) then
        X = Z/(1-beta+beta*K)
        call thermo(t,p,X,MINGIBBSPH,lnfug,ophase=ophase)
      else
        Y = K*Z/(1-beta+beta*K)
        call thermo(t,p,Y,MINGIBBSPH,lnfug,ophase=ophase)
      endif
      if (ophase == SINGLEPH) then
        ! Wait until next iteration to actually switch formulation.
        should_switch = .true.
      endif
    endif
  end function shouldSwitchFormulation

  !-----------------------------------------------------------------------------
  !> Set solution
  !>
  !> \author MH, 2013-10-10. Modified: EA, 2015-01
  !-----------------------------------------------------------------------------
  subroutine setEnvelopePoint(X,Z,beta,i,Ta,Pa,Ki,betai,nmax)
    implicit none
    real, intent(in) :: beta
    real, dimension(nc+2), intent(in) :: X
    real, dimension(nc), intent(in) :: Z
    integer, intent(in) :: i, nmax
    real, dimension(nmax), intent(inout) :: Ta,Pa
    real, dimension(nmax,nc), intent(inout) :: Ki
    real, dimension(nmax), intent(inout) :: betai
    ! Locals
    real, dimension(nc) :: K
    !
    K = exp(X(1:nc))
    Ta(i) = exp(X(nc+1))
    Pa(i) = exp(X(nc+2))
    betai(i) = beta
    Ki(i,:) = K
  end subroutine setEnvelopePoint

  !-----------------------------------------------------------------------------
  !> Envelope-entropy solver
  !>
  !> \author MH, 2013-10-10
  !-----------------------------------------------------------------------------
  subroutine entropySolver(Z,beta,X,Xold,dXdS,sspec,s,ierr)
    implicit none
    real, intent(in) :: beta,sspec
    real, dimension(nc+2), intent(out) :: X
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc+2), intent(in) :: Xold,dXdS
    integer, intent(in) :: s
    integer, optional, intent(out) :: ierr
    ! Locals
    real, dimension(4*nc+9) :: param
    type(nonlinear_solver) :: solver
    real :: Xs,Xsmax,Xsmin
    !
    if (verbose) then
      print *,'Solving for isentropic envelope point....'
    endif
    param(1) = sspec
    param(2) = beta
    param(3) = real(s)
    param(4:nc+3) = Z
    param(nc+4:2*nc+5) = Xold
    param(2*nc+6:3*nc+7) = dXdS
    param(3*nc+8:4*nc+9) = X
    solver%abs_tol = 1e-8
    if (X(s) > Xold(s)) then
      Xsmax = X(s)
      Xsmin = Xold(s)
    else
      Xsmax = Xold(s)
      Xsmin = X(s)
    endif
    Xs = X(s)
    call pegasus(Xsmin,Xsmax,sdewfun,Xs,solver,param)
    if (present(ierr)) then
      ierr = solver%exitflag
    else
      if (solver%exitflag /= 0) then
        write(*,*) "Exitflag: ",solver%exitflag
        call stoperror("entropySolver failed.")
      endif
    endif
    X = param(3*nc+8:4*nc+9)
  end subroutine entropySolver

  !-----------------------------------------------------------------------------
  !> Function used to solve for isentropic point on phase envelope
  !>
  !> \author MH, 2013
  !-----------------------------------------------------------------------------
  function sdewfun(Xs,param) result(fun)
    implicit none
    real, intent(inout) :: Xs
    real, dimension(4*nc+9), intent(inout) :: param
    real :: fun
    ! Locals
    integer :: iter, s, phase
    real, dimension(nc) :: Z,K
    real, dimension(nc+2) :: X,Xold,dXdS
    real :: t, ent, entspec, beta, ln_spec, p

    entspec = param(1)
    beta = param(2)
    s = int(param(3))
    Z = param(4:nc+3)
    Xold = param(nc+4:2*nc+5)
    dXdS = param(2*nc+6:3*nc+7)
    X = param(3*nc+8:4*nc+9)
    ln_spec = Xs
    ! Extrapolate for better initial values
    X = Xold + dXdS*(Xs-Xold(s))
    K = exp(X(1:nc))
    t = exp(X(nc+1))
    p = exp(X(nc+2))

    iter = sat_newton(Z,K,t,p,beta,s,ln_spec)
    X(1:nc) = log(K)
    X(nc+1) = log(T)
    X(nc+2) = log(p)
    param(3*nc+8:4*nc+9) = X
    if (beta > 0.5) then
      phase = VAPPH
    else
      phase = LIQPH
    endif
    call entropy(t,p,Z,phase,ent)
    fun = ent - entspec
  end function sdewfun

  !-----------------------------------------------------------------------------
  !> Calculate dP and dT along saturation line.
  !> They only give meaning as dPdT or dTdP
  !> \author MH, 2013-10-08
  !-----------------------------------------------------------------------------
  subroutine dPdTcalc(dP,dT,Z,K,beta,t,p)
    implicit none
    real, dimension(nc), intent(in) :: Z, K
    real, intent(in) :: t, p
    real, intent(in) :: beta
    real, intent(out) :: dP, dT
    ! Locals
    real, dimension(nc) :: X,Y,fugL,fugV,fugtV,fugpV,fugtL,fugpL

    X = Z/(1.0-beta+beta*K)
    Y = K*Z/(1.0-beta+beta*K)

    call thermo(t,p,X,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL)
    call thermo(t,p,Y,VAPPH,fugV,lnfugt=fugtV,lnfugp=fugpV)

    if (beta == 1.0) then
      dT = -sum(X*(fugpL-fugpV))*1.0e5
      dP = sum(X*(fugtL-fugtV))
    else
      dT = -sum(Y*(fugpV-fugpL))*1.0e5
      dP = sum(Y*(fugtV-fugtL))
    endif

  end subroutine dPdTcalc

  !-----------------------------------------------------------------------------
  !> Numerical test for sat_diff_newton
  !>
  !> \author MH, 2011
  !-----------------------------------------------------------------------------
  subroutine testMatrix(Z,K,t,p,beta,s,ln_s)
    implicit none
    real, dimension(nc), intent(in) :: Z, K
    real, intent(in) :: t, p
    real, intent(in) :: beta
    real, intent(in) :: ln_s
    integer, intent(in) :: s
    ! Locals
    real, dimension(nc+2) :: Xvar, XvarOrg, G0, G1
    real, dimension(nc+2,nc+2) :: Jac, JacNum, Jac0
    integer :: i
    real, dimension(nc+3) :: param

    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)
    XvarOrg = Xvar
    param(1:nc) = Z
    param(nc+1) = beta
    param(nc+2) = real(s)
    param(nc+3) = ln_s

    call sat_fun_newton(G0,Xvar,param)
    call sat_diff_newton(Jac0,Xvar,param)

    do i=1,nc+2
      Xvar = XvarOrg
      Xvar(i) = XvarOrg(i) + 1.0e-4
      call sat_fun_newton(G1,Xvar,param)
      call sat_diff_newton(Jac,Xvar,param)
      JacNum(:,i) = (G1-G0)/1.0e-4
    enddo
    do i=1,nc+2
      print *
      print *,Jac0(i,:)
      print *,JacNum(i,:)
    enddo
  end subroutine testMatrix

  !-----------------------------------------------------------------------------
  !> Polynomial fit for saturation line used for extrapolation
  !>
  !> \author MH, 2011
  !-----------------------------------------------------------------------------
  subroutine poly(n,s0,s1,X0,X1,dXdS0,dXdS1,s2,X2)
    implicit none
    integer, intent(in) :: n
    real, dimension(n), intent(in) :: X0,X1,dXdS0,dXdS1
    real, dimension(n), intent(out) :: X2
    real, intent(in) :: s0, s1, s2
    ! Locals
    real, dimension(4) :: A
    real, dimension(4,4) :: JA
    integer :: i, INFO
    integer, dimension(n) :: INDX

    JA = 0.0
    do i=1,n
      JA(1,1) = s0**3
      JA(1,2) = s0**2
      JA(1,3) = s0
      JA(1,4) = 1.0
      A(1) = X0(i)

      JA(2,1) = s1**3
      JA(2,2) = s1**2
      JA(2,3) = s1
      JA(2,4) = 1.0
      A(2) = X1(i)

      JA(3,1) = 3.0*s0**2
      JA(3,2) = 2.0*s0
      JA(3,3) = 1.0
      A(3) = dXdS0(i)

      JA(4,1) = 3.0*s1**2
      JA(4,2) = 2.0*s1
      JA(4,3) = 1.0
      A(4) = dXdS1(i)

      ! Solve equation system
      call DGESV( n, 1, JA, n, INDX, A, n, INFO )

      ! Extrapolate
      X2(i) = A(1)*s2**3 + A(2)*s2**2 + A(3)*s2 + A(4)
    enddo

  end subroutine poly

  !-----------------------------------------------------------------------------
  !> Subroutine used to solve for cricondenbar/cricondenterm
  !> beta = 1 or beta = 0 is assumed
  !> Phase enveloe code should provide good initial guess.
  !> \author MH, 2013-10-07
  !-----------------------------------------------------------------------------
  subroutine criconden_fun_newton(F,Xvar,param)
    implicit none
    real, dimension(nc+2), intent(out) :: F !< Function value
    real, dimension(nc+2), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: K, FUGV, FUGTV, FUGPV, FUGL, FUGTL, FUGPL, Y, X
    real :: p, t, b
    logical :: condentherm, yvar
    integer :: i
    Z = param(1:nc)
    b = param(nc+1)
    condentherm = (param(nc+2) == 1.0)

    if (b == 0.0) then
      yvar = .true.
    else if (b == 1.0) then
      yvar = .false.
    else
      call Stoperror('saturation::criconden_fun_newton: Beta must be 1.0 or 0.0.')
    endif

    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))

    X = Z/(1-b+b*K)
    Y = K*Z/(1-b+b*K)

    call thermo(t,p,X,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL)
    call thermo(t,p,Y,VAPPH,fugV,lnfugt=fugtV,lnfugp=fugpV)
    ! Function value
    do i=1,nc
      if (Z(i) > 0.0) then
        F(i) = Xvar(i) + FUGV(i) - FUGL(i)
      else
        F(i) = 0.0
      endif
    enddo
    F(nc+1) = sum(Y-X)
    if (condentherm) then
      if (yvar) then
        F(nc+2) = sum(y*(fugpV-fugpL))*1.0e5
      else
        F(nc+2) = sum(x*(fugpL-fugpV))*1.0e5
      endif
    else ! cricondenbar
      if (yvar) then
        F(nc+2) = sum(y*(fugtV-fugtL))
      else
        F(nc+2) = sum(x*(fugtL-fugtV))
      endif
    endif
  end subroutine criconden_fun_newton

  !-----------------------------------------------------------------------------
  !> Subroutine used to calculate differentals when solving for
  !> cricondenbar/cricondenterm. Numerical perturbation is used.
  !> beta = 1 or beta = 0 is assumed
  !> Phase enveloe code should provide good initial guess.
  !> \author MH, 2013-10-07
  !-----------------------------------------------------------------------------
  subroutine criconden_diff_newton(Jac,Xvar,param)
    implicit none
    real, dimension(nc+2,nc+2), intent(out) :: Jac !< Differentials
    real, dimension(nc+2), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Paramater vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: K, FUGV, FUGTV, FUGPV, FUGL, FUGTL, FUGPL, Y, X
    real, dimension(nc) :: FUGTV2, FUGPV2, FUGTL2, FUGPL2
    real, dimension(nc,nc) :: FUGXV, FUGXL
    integer :: i,j
    real :: p, t, b, releps
    real, parameter :: eps = 1.0e-6
    logical :: condentherm, yvar
    Z = param(1:nc)
    b = param(nc+1)
    condentherm = (param(nc+2) == 1.0)

    if (b == 0.0) then
      yvar = .true.
    else if (b == 1.0) then
      yvar = .false.
    else
      call Stoperror('saturation::criconden_diff_newton: Beta must be 1.0 or 0.0.')
    endif

    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))

    X = Z/(1-b+b*K)
    Y = K*Z/(1-b+b*K)

    call thermo(t,p,X,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL,lnfugx=fugxL)
    call thermo(t,p,Y,VAPPH,fugV,lnfugt=fugtV,lnfugp=fugpV,lnfugx=fugxV)
    ! K differentials
    if (condentherm) then
      do j=1,nc
        if (Z(j) > 0.0) then
          if (yvar) then
            Jac(nc+2,j) = (fugpV(j)-fugpL(j))/Z(j)
          else
            Jac(nc+2,j) = -(fugpL(j)-fugpV(j))*Z(j)/K(j)
          endif
        else
          Jac(nc+2,1:nc) = 0.0
        endif
      enddo
      ! Perturbate pressure
      releps = p * eps
      p = p + releps
    else ! cricondenbar
      do j=1,nc
        if (Z(j) > 0.0) then
          if (yvar) then
            Jac(nc+2,j) = (fugtV(j)-fugtL(j))/Z(j)
          else
            Jac(nc+2,j) = -(fugtL(j)-fugtV(j))*Z(j)/K(j)
          endif
        else
          Jac(nc+2,j) = 0.0
        endif
      enddo
      ! Perturbate temperature
      releps = t * eps
      t = t + releps
    endif

    ! Temperature differential
    Jac(1:nc,nc+1) = t*(FUGTV-FUGTL)

    ! Pressure differential
    Jac(1:nc,nc+2) = p*(FUGPV-FUGPL)

    ! K differentials
    do i=1,nc
      if (Z(i) > 0.0) then
        do j=1,nc
          if (Z(j) > 0.0) then
            Jac(i,j) = ((1.0-b)*FUGXV(i,j)+b*FUGXL(i,j))*X(j)*Y(j)/Z(j)
          else
            Jac(i,j) = 0.0
          endif
        enddo
      else
        Jac(i,:) = 0.0
      endif
      Jac(i,i) = Jac(i,i) + 1.0
    enddo

    ! nc+1 row
    do j=1,nc
      if (Z(j) > 0.0) then
        Jac(nc+1,j) = X(j)*Y(j)/Z(j)
      else
        Jac(nc+1,i) = 0.0
      endif
    enddo
    Jac(nc+1,nc+1) = 0
    Jac(nc+1,nc+2) = 0

    ! Perturbated state
    call thermo(t,p,X,LIQPH,fugL,lnfugt=fugtL2,lnfugp=fugpL2)
    call thermo(t,p,Y,VAPPH,fugV,lnfugt=fugtV2,lnfugp=fugpV2)
    fugtL2 = (fugtL2 - fugtL)/releps
    fugtV2 = (fugtV2 - fugtV)/releps
    fugpL2 = (fugpL2 - fugpL)/releps
    fugpV2 = (fugpV2 - fugpV)/releps

    ! K differentials
    if (yvar) then
      Jac(nc+2,nc+1) = T*sum(y*(fugtV2-fugtL2)) ! dF(nc+2)/dlnT
      Jac(nc+2,nc+2) = P*sum(y*(fugpV2-fugpL2)) ! dF(nc+2)/dlnP
    else
      Jac(nc+2,nc+1) = T*sum(x*(fugtL2-fugtV2)) ! dF(nc+2)/dlnT
      Jac(nc+2,nc+2) = P*sum(x*(fugpL2-fugpV2)) ! dF(nc+2)/dlnP
    endif

    if (condentherm) then
      Jac(nc+2,:) = Jac(nc+2,:)*1.0e5
    endif

  end subroutine criconden_diff_newton

  !-----------------------------------------------------------------------------
  !> Solve cricondenbar/cricondenterm equations.
  !> Phase enveloe code should provide good initial guess.
  !> \author MH, 2013-10-07
  !-----------------------------------------------------------------------------
  subroutine criconden_solve(Z,Xvar,beta,Pmax,Pmin,Tmax,Tmin,cricondentherm,ierr)
    implicit none
    logical, intent(in) :: cricondentherm !< True if cricondenterm is to be calculate
    !< False if cricondenbar is to be calculated
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(in) :: Pmax,Pmin,Tmax,Tmin !< Pressure and temperature limiting solution space
    real, intent(in) :: beta !< Specified vapour mole fraction
    real, dimension(nc+2), intent(inout) :: Xvar !< Variables, ln K, ln T, ln P
    integer, intent(out) :: ierr !< Error indicator
    ! Locals
    real, dimension(nc+2) :: param,xmax,xmin
    type(nonlinear_solver) :: solver
    !     real, dimension(nc+2) :: XvarPert,F0,F
    !     real, dimension(nc+2,nc+2) :: Jac,numJac
    !     integer :: i

    param(1:nc) = Z
    param(nc+1) = beta
    if (cricondentherm) then
      param(nc+2) = 1.0 ! Logical flag for cricondentherm
    else
      param(nc+2) = 0.0 ! Logical flag for cricondenbar
    endif

    xmax(nc+1) = log(Tmax + 10.0)
    xmin(nc+1) = log(Tmin - 10.0)
    xmax(nc+2) = log(Pmax + 5.0e5)
    xmin(nc+2) = log(Pmin - 5.0e5)

    ! Test Jacobean numerically
    !     call criconden_fun_newton(F0,Xvar,param)
    !     call criconden_diff_newton(Jac,Xvar,param)
    !     do i=1,nc+2
    !       XvarPert = Xvar
    !       XvarPert(i) = XvarPert(i)*(1.0-1.0e-5)
    !       call criconden_fun_newton(F,XvarPert,param)
    !       numJac(:,i) = (F-F0)/(XvarPert(i)-Xvar(i))
    !     enddo
    !     print *,'Jac1',Jac(1,:)
    !     print *,'numJac1',numJac(1,:)
    !     print *,'Jac2',Jac(2,:)
    !     print *,'numJac2',numJac(2,:)
    !     print *,'Jac3',Jac(3,:)
    !     print *,'numJac3',numJac(3,:)
    !     print *,'Jac4',Jac(4,:)
    !     print *,'numJac4',numJac(4,:)

    solver%rel_tol = 1e-20
    solver%abs_tol = 1e-10
    solver%limit_x_values = .false.
    xmax(1:nc) = 1.0e100
    xmin(1:nc) = -1.0e100
    solver%max_it = 100
    solver%ls_max_it = 5

    call nonlinear_solve(solver,criconden_fun_newton,criconden_diff_newton,&
         criconden_diff_newton,limit_dx,premReturn,setXv,Xvar,xmin,xmax,param)
    ierr = solver%exitflag
    if (solver%exitflag /= 0) then
      print *,'saturation::criconden_solve: Not able to solve for criconden'
    endif
  end subroutine criconden_solve

  !-----------------------------------------------------------------------------
  !> Plot saturation line for single component (wrapper for external calls)
  !>
  !> \author MH, 2022-07-15
  !-----------------------------------------------------------------------------
  subroutine pure_fluid_saturation_wrapper(Z,t_or_p,start_from_temp,max_delta_p,&
       log_linear,Ta,Pa,val,vag,nmax,n)
    use eos, only: specificvolume
    implicit none
    logical, intent(in) :: start_from_temp
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: t_or_p
    integer, intent(in) :: nmax
    real, dimension(nmax), intent(out) :: Ta,Pa,val,vag
    integer, intent(out) :: n
    real, intent(in) :: max_delta_p !< Maximum Delta P between points
    logical, intent(in) :: log_linear !< Distribute values lineary based on natural logarithm
    ! Locals
    real :: t, p
    integer :: i, spec
    if (start_from_temp) then
      spec = SPECT
      t = t_or_p
      p = 0
    else
      spec = SPECP
      p = t_or_p
      t = 0
    endif
    call singleCompSaturation(Z,t,p,spec,Ta,Pa,nmax,n,&
         maxDeltaP=max_delta_p,log_linear=log_linear)
    do i=1,n
      call specificvolume(Ta(i), Pa(i), Z, VAPPH, vag(i))
      call specificvolume(Ta(i), Pa(i), Z, LIQPH, val(i))
    enddo
  end subroutine pure_fluid_saturation_wrapper

  !-----------------------------------------------------------------------------
  !> Plot saturation line for single component
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  subroutine singleCompSaturation(Z,t,p,specification,Ta,Pa,nmax,n,maxDeltaP,paep,log_linear)
    use eos, only: getCriticalParam
    use critical, only: calcCriticalTV
    use eosTV, only: pressure
    implicit none
    integer, intent(in) :: specification
    real, dimension(nc), intent(in) :: Z
    real, intent(inout) :: t, p
    integer, intent(in) :: nmax
    real, dimension(nmax), intent(out) :: Ta,Pa
    integer, intent(out) :: n
    real, optional, intent(in) :: maxDeltaP !< Maximum Delta P between points
    type(aep), optional, intent(out) :: paep
    logical, optional, intent(in) :: log_linear !< Distribute values lineary based on natural logarithm
    ! Locals
    integer, dimension(1) :: zmax
    real, dimension(nc) :: X,Y
    integer :: i_pure, iter, ierr, i_insip
    real :: f,dfdt,dfdp,tci,pci,oi,vci,dpdt,dP,dT,p0,t0
    real :: dlnp, dlnT
    logical :: log_linear_loc
    real, dimension(4) :: param
    real, dimension(1) :: XX,Xmax,Xmin
    type(nonlinear_solver) :: solver
    real, parameter   :: reltol_close_to_crit = 1e-2
    real, parameter   :: maxdP = 0.2e5, max_dlnp = 0.02
    real              :: relerr_crit
    real              :: maxDelP
    real              :: lnfugl(nc), lnfugg(nc), dg_insip, dg_insip_old
    !
    if (present(log_linear)) then
      log_linear_loc = log_linear
    else
      log_linear_loc = .false.
    endif
    if (present(maxDeltaP)) then
      maxDelP = maxDeltaP
    else
      maxDelP = maxdP
    endif
    X = 0.0
    zmax = maxloc(Z)
    i_pure = zmax(1)
    X(i_pure) = 1.0
    if (present(paep)) then
      if (nc /= 2) then
        call stoperror("Need nc=2 in singleCompSaturation to look for PAEP.")
      endif
      i_insip = minloc(X,dim=1)
      paep%found = .false.
      paep%type = AZ_PAEP
    endif
    call getCriticalParam(i_pure,tci,pci,oi,vci)
    ! Find initlal point
    if (specification == specP) then
      p0 = p
      T = safe_bubT(P,X,Y,ierr)
      if (log_linear_loc) then
        n = min(int((log(pci)-log(P))/max_dlnp)+1,nmax)
        dlnp = (log(pci)-log(P))/(n-1)
        do iter=1,n
          pa(iter) = exp(log(p0) + dlnp*(iter-1))
        enddo
      else
        n = min(int((pci-P)/maxDelP)+1,nmax)
        dP = (pci-P)/(n-1)
        do iter=1,n
          pa(iter) = p0 + dP*(iter-1)
        enddo
      endif
      xmax = log(tci)
      xmin = log(t)
    else if (specification == specT) then
      T0 = T
      P = safe_bubP(T,X,Y,ierr)
      if (log_linear_loc) then
        n = min(int((log(pci)-log(P))/max_dlnp)+1,nmax)
        dlnT = (log(tci)-log(T))/(n-1)
        do iter=1,n
          ta(iter) = exp(log(T0) + dlnT*(iter-1))
        enddo
      else
        n = min(int((pci-P)/maxDelP)+1,nmax)
        dT = (tci-T)/(n-1)
        do iter=1,n
          ta(iter) = T0 + dT*(iter-1)
        enddo
      endif
      xmax = log(pci)
      xmin = log(p)
    endif
    if (ierr /= 0) then
      call stoperror('saturation_curve::singleCompSaturation solve for initial point')
    endif
    Ta(1) = t
    Pa(1) = p
    Ta(n) = tci
    Pa(n) = pci
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%max_it = 1000
    param(4) = 0.0
    do iter = 2,n-1
      call sat_fun_single(i_pure,t,p,f,dfdt,dfdp,.false.)
      if (dfdp == 0.0) then
        if (verbose) then
          print *,"singleCompSaturation: Density solver did not find two different roots."// &
               " Attemting to continue"
        endif
        dpdt = 1.0 ! Dummy value
      else
        dpdt = -dfdt/dfdp
        if (verbose .and. dpdt < 0.0) then
          print *,"singleCompSaturation: Negative sign of dpdt. Check density solver."
        endif
      endif
      param(1) = specification
      param(2) = i_pure
      if (specification == specP) then
        dP = pa(iter) - pa(iter-1)
        param(3) = pa(iter)
        XX(1) = log(t+dP/dpdt)
      else
        dT = ta(iter) - ta(iter-1)
        param(3) = ta(iter)
        XX(1) = log(p+dpdt*dT)
      endif
      ! Limit to bounds
      XX(1) = max(min(XX(1),xmax(1)),xmin(1))
      call nonlinear_solve(solver,sat_fun_single_if,sat_diff_single,&
           sat_diff_single,limit_dx,premReturn,setXv,XX,xmin,xmax,param)
      if (solver%exitflag == 0) then
        select case(specification)
        case (1)
          T = exp(XX(1))
          P = param(3)
        case (2)
          P = exp(XX(1))
          T = param(3)
        end select
      else if (solver%exitflag == 2) then
        ! Could not invert Jacobian. Are we close to critical point?
        select case(specification)
        case (1)
          P = param(3)
          relerr_crit =  abs(P-Pci)/Pci
        case (2)
          T = param(3)
          relerr_crit =  abs(T-Tci)/Tci
        end select
        if (relerr_crit < reltol_close_to_crit) then
          ! Close enough to critical point. Set final point to critical point.
          n = iter
          Ta(n) = tci
          Pa(n) = pci
          return
        else
          if (specification == specP) then
            P = param(3)
            T = safe_bubT(P,X,Y,ierr)
          else if (specification == specT) then
            T = param(3)
            P = safe_bubP(T,X,Y,ierr)
          endif
          if (ierr /= 0) then
            call stoperror('saturation::singleCompSaturation could not invert J')
          endif
        endif
      else
        write(*,*) "Error flag: ",solver%exitflag
        call stoperror('saturation::singleCompSaturation did not converge')
      endif
      Ta(iter) = t
      Pa(iter) = p

      if (present(paep)) then
        if (.not. paep%found) then
          call thermo(t,p,x,LIQPH,lnfugl)
          call thermo(t,p,x,VAPPH,lnfugg)
          dg_insip = lnfugl(i_insip) - lnfugg(i_insip)
          if (iter /= 2) then
            if (dg_insip*dg_insip_old < 0) then
              ! Solve for PAEP
              call solve_for_paep(Ta(iter-1),Pa(iter-1),t,p,i_pure,i_insip,paep,ierr)
              if (ierr == 0) then
                ! Make sure paep is on saturation curve
                if (abs(Ta(iter-1)-paep%t) < abs(Ta(iter)-paep%t)) then
                  Ta(iter-1) = paep%t
                  Pa(iter-1) = paep%p
                else
                  Ta(iter) = paep%t
                  Pa(iter) = paep%p
                endif
              endif
            endif
          endif
          dg_insip_old = dg_insip
        endif
      endif
    end do
  end subroutine singleCompSaturation

  !-----------------------------------------------------------------------------
  !> Calculate PAEP
  !>
  !> \author MH, 2019-09
  !-----------------------------------------------------------------------------
  subroutine solve_for_paep(t0,p0,t1,p1,i_pure,i_insip,paep,ierr)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver,&
         NS_PEGASUS
    implicit none
    real, intent(in) :: t0, p0            ! init. point has pressure p0
    real, intent(in) :: t1, p1            ! t and p
    integer, intent(in) :: i_pure,i_insip ! component indices
    type(aep), intent(inout) :: paep        ! pure fluid azeotrope end point
    integer, intent(out) :: ierr          ! error flag
    ! Locals
    real :: T_guess, zpure(nc), y(nc), param(4), p, a, b
    real :: lnfugg(nc), lnfugl(nc)
    type(nonlinear_solver) :: solver_cfg
    real, parameter :: p_rel_tol = machine_prec*1000
    ! Configure solver
    solver_cfg%rel_tol = p_rel_tol
    solver_cfg%max_it = 1000
    solver_cfg%isolver = NS_PEGASUS
    ! Set the constant parameters of the objective function.
    param(1) = real(i_pure)
    param(2) = real(i_insip)
    a = (t1-t0)/(p1-p0)
    b = t0 - a*p0
    param(3) = a
    param(4) = b
    ! Find f=0 inside the bracket.
    call bracketing_solver(p0,p1,fun_paep,p,solver_cfg,param)
    ! Check for successful convergence
    if (solver_cfg%exitflag /= 0) then
      if (verbose) write(*,*) "PAEP: Bracketing solver failed."
      ierr = solver_cfg%exitflag
      paep%found = .false.
      return
    else
      ierr = 0
    endif
    paep%p = p
    zpure = 0
    zpure(i_pure) = 1
    T_guess = a*p + b
    paep%t = bubT(T_guess,paep%p,zpure,y)
    paep%found = .true.
    paep%type = AZ_PAEP
    paep%x = zpure
    call thermo(paep%t,paep%p,zpure,LIQPH,lnfugl,v=paep%vl)
    call thermo(paep%t,paep%p,zpure,VAPPH,lnfugg,v=paep%vg)
  end subroutine solve_for_paep

  function fun_paep(p,param) result(f)
    ! Objective function for lnphi_l - lnphi_g = 0
    !
    implicit none
    ! Input:
    real,     intent(in)  :: p !< Pressure (Pa)
    real,     intent(in)  :: param(4) !< Parameters
    ![i_pure,i_insip,T_guess]
    ! Output:
    real                  :: f !< s_sat - sspec
    ! Internal:
    integer               :: i_pure,i_insip
    real                  :: zpure(nc),y(nc),T0,T_sat,p_arg
    real                  :: lnfugl(nc), lnfugg(nc), a, b
    ! Read from param
    i_pure = nint(param(1))
    i_insip = nint(param(2))
    a = param(3)
    b = param(4)
    ! Make pure composition vector
    zpure(:) = 0.0
    zpure(i_pure) = 1.0
    ! Find saturation temperature at p
    p_arg = p
    T0 = a*p + b
    T_sat = bubT(T0,p_arg,zpure,y)
    ! Find single-phase entropy at saturation
    call thermo(T_sat,p,zpure,LIQPH,lnfugl)
    call thermo(T_sat,p,zpure,VAPPH,lnfugg)
    ! Make objective function
    f = lnfugl(i_insip) - lnfugg(i_insip)
  end function fun_paep

  !-----------------------------------------------------------------------------
  !> Calculate crossing of isentropic decompression path and phase envelope
  !>
  !> \author MH, 2014-03
  !-----------------------------------------------------------------------------
  function envelope_isentrope_cross(Z,t0,p0,x0,y0,Pmax,sspec,t,p,phase,w,&
       dS_override, ierr_out) result(hasCrossing)
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(nc), intent(in) :: Z     ! total composition
    real, dimension(nc), intent(in) :: x0,y0 ! init. liq./vap. composition guess
    real, intent(in) :: t0, p0               ! init. point has pressure p0
    real, intent(out) :: t,p                 ! t and p at crossing point
    real, intent(in) :: Pmax                 ! upper pressure limit on envelope
    real, intent(in) :: sspec                ! isentrope entropy
    integer, intent(out) :: phase            ! phase on incoming side of cross
    real, dimension(nc), intent(out) :: w    ! composition of incipient phase
    logical :: hasCrossing                   ! crossing between p0 and pmax?
    real, optional, intent(in) :: dS_override! step length along envelope
    integer, optional, intent(out) :: ierr_out ! error flag; nonzero if error
    ! Locals
    integer :: specification
    real, dimension(nc) :: K, lnfug, lnfugG, lnfugL
    real, dimension(nc+2) :: dXdS, dXdSold
    real, dimension(nc+2) :: Xvar ,Xold
    real :: dS, tuning, sgn, Pstart
    integer :: iter,s,n,ophase,ierr
    integer, dimension(1) :: smax,zmax
    real :: ent,ent_old,ln_spec,beta
    logical :: have_switched_formulation
    logical :: should_switch_formulation

    ! Special routine for pure fluids
    zmax = maxloc(Z)
    if (isSingleComp(Z)) then
      call envelope_isentrope_cross_single(p0,sspec,z,p0,hasCrossing,&
           p,T,phase,ierr)
      w = z
      if (present(ierr_out)) then
        ierr_out = ierr
      endif
      return
    endif

    ! Initialize error flag. If something went wrong, set ierr_out /= 0.
    if (present(ierr_out)) ierr_out = 0

    ! Initialize variables needed to solve for initial point on envelope
    beta = 1.0
    ln_spec = log(p0)
    t = t0
    p = p0
    call thermo(t0,p0,y0,VAPPH,lnfugG)
    call thermo(t0,p0,x0,LIQPH,lnfugL)
    K = exp(lnfugL-lnfugG)

    ! Solve for initial saturation point temperature and equilibrium factors K
    tuning = 1.2
    sgn = 1.0
    specification = specP
    s = ispec(specification) ! index of fixed variable in Xvar below (here nc+2)
    iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
    if (ierr /= 0) then
      if (present(ierr_out)) then
        ierr_out = ierr
        return
      else
        call stoperror('envelope_isentrope_cross: Initial point not found.')
      endif
    endif

    ! Retrieve solved variables
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)
    Pstart = p

    ! Compute entropy of initial point on phase envelope
    call entropy(t,p,Z,VAPPH,ent_old)
    if (ent_old < sspec) then
      hasCrossing = .false.
      return
    endif

    ! When traversing the phase envelope, we may eventually go
    ! from the dew curve to the bubble curve, forcing us to
    ! changevariables. These variables control this switch.
    have_switched_formulation = .false.
    should_switch_formulation = .false.

    ! Set steplength along phase envelope
    if (present(dS_override)) then
      dS = dS_override
    else
      dS = 0.15
    endif

    ! Step along the phase envelope until isentrope entropy is reached
    w = 0.0
    hasCrossing = .true.
    n = 1
    do while (.true.)
      dXdSold = dXdS
      call newton_extrapolate(Z,Xvar,dXdS,beta,s,ln_spec)
      smax = maxloc(abs(dXdS))
      if ((.not. smax(1) == s) .and. n > 1) then
        s = smax(1)
        ! Rescaling the sensitivities
        sgn = sign(1.0,Xvar(s) - Xold(s))
        dXdS = dXdS / dXdS(s)
        dXdSold = dXdSold / dXdSold(s)
      endif

      Xold = Xvar
      Xvar = Xold + dXdS*dS*sgn
      K = exp(Xvar(1:nc))
      t = exp(Xvar(nc+1))
      p = exp(Xvar(nc+2))
      ln_spec = Xvar(s)
      if (p < Pstart .OR. p > Pmax) then
        hasCrossing = .false.
        exit
      endif

      iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
      if (ierr /= 0) then
        ! Something went wrong.
        ! Attempt to make the step shorter.
        Xvar = Xold + dXdS*dS*sgn*0.5
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        ln_spec = Xvar(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        if (ierr /= 0) then
          ! Something went wrong.
          ! Attempt to make the step longer.
          Xvar = Xold + dXdS*dS*sgn*2.0
          K = exp(Xvar(1:nc))
          t = exp(Xvar(nc+1))
          p = exp(Xvar(nc+2))
          ln_spec = Xvar(s)
          iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
          if (ierr /= 0) then
            if (present(ierr_out)) then
              ierr_out = ierr
              return
            else
              call stoperror("envelope_isentrope_cross: Neither decreasing nor &
                   &increasing the step helped")
            endif
          endif
        endif

        ! Reset dS to an intermediate value
        if (.not. present(dS_override)) then
          dS = 0.15
        endif
      endif

      Xvar(1:nc) = log(K)
      Xvar(nc+1) = log(t)
      Xvar(nc+2) = log(p)
      n = n + 1

      if (beta > 0.5) then
        phase = VAPPH
      else
        phase = LIQPH
      endif
      call entropy(t,p,Z,phase,ent)

      ! Check to see if sspec is bracketed.
      if (sspec >= ent .and. sspec <= ent_old) then
        ! sspec is bracketed, so solve for exact entropy and return

        call entropySolver(Z,beta,Xvar,Xold,dXdS,sspec,s, ierr)

        if (ierr /= 0) then
          if (present(ierr_out)) then
            ierr_out = ierr
            return
          else
            if (ierr == 100) then
              print *,'No solution for entropy solver'
            else
              print *,'ierr',ierr
            endif
            call stoperror("entropySolver failed!")
          endif
        endif

        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))

        if (phase == VAPPH) then
          w = Z/(1.0-beta+beta*K)
        else
          w = K*Z/(1.0-beta+beta*K)
        endif
        return
      else
        ! sspec not bracketed, so update ent_old and continue
        ent_old = ent
      endif

      ! While the possibility to switch formulation may have arisen,
      ! it is not necessarily yet possible to do it error-free.
      ! If the flag should_switch_formulation has been raised, attempt
      ! to switch until it can be done error-free.
      if (should_switch_formulation) then
        call changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        ! If error-free, change phase now. If not, try again next time.
        if (ierr == 0) then
          Xvar(1:nc) = log(K)
          Xvar(nc+1) = log(t)
          Xvar(nc+2) = log(p)
          have_switched_formulation = .true.
          should_switch_formulation = .false.
        else
          call changeFormulation(beta,Xvar,Xold,K,s,ln_spec,sgn)
        endif
      endif

      if (.not. have_switched_formulation) then
        ! Check if we have entered the SINGLEPH-area.
        ! If so, we can change formulation (beta)
        call thermo(t,p,z,MINGIBBSPH,lnfug,ophase=ophase)
        if (ophase == SINGLEPH) then
          ! Wait until next iteration to actually switch formulation.
          should_switch_formulation = .true.
        endif
      endif

      ! Tune dS up or down based on how fast sat_newton converged
      if (.not. present(dS_override)) then
        if (iter < 3) then
          dS = dS * tuning
        else if (iter > 5) then
          dS = dS * (1.0 - tuning)
        endif
        dS = max(min(dS,0.25),0.05)
      endif
    enddo

  end function envelope_isentrope_cross


  subroutine envelope_isentrope_cross_single(p0,s_spec,z,p_low,has_crossing,&
       p_crossing,T_crossing,phase_crossing, ierr)
    !> Find if, and if so where, the isentrope from a point p0,s_spec
    !> meets the saturation line before reaching pressure p_low.
    !>
    !> This is for single-component calculation only.
    !> The maximum value in composition-vector z defines which component
    !> is used.
    !>
    !> \author EA, 2014-09
    !>
    use eos, only: getCriticalParam
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver,&
         NS_RIDDERS
    implicit none
    ! Input:
    real,         intent(in)    :: p0       !< Initial pressure (Pa)
    real,         intent(in)    :: s_spec   !< Isentrope entropy (J/(mol*K))
    real,         intent(in)    :: z(nc)    !< Molar composition (-)
    real,         intent(in)    :: p_low

    ! Output:
    logical,      intent(out)   :: has_crossing !< If isentrope meets sat. line
    real,         intent(out)   :: p_crossing !< Pressure when meeting sat. line
    real,         intent(out)   :: T_crossing !< Temp. when meeting sat. line
    integer,      intent(out)   :: phase_crossing !< Phase on incoming side
    integer,      intent(out)   :: ierr         !< Error if not zero.
    ! Internal:
    integer                     :: zmax(1), icomp
    real                        :: Tci, pci, oi, zpure(nc), s_crit, T_guess,&
         T_low,p,T,s,y(nc),s_low_liq,s_low_vap,&
         param(6),p_1,p_2,T_1,T_2,f,dfdt,dfdp,dtdp,&
         p_prev,XX(1),Xmin(1),Xmax(1),dp,p_close_crit,T_prev
    logical                     :: is_close_crit,found_bracket
    logical, parameter          :: debug=.false.
    type(nonlinear_solver)      :: solver_cfg
    real, parameter             :: p_rel_tol = machine_prec*1000,&
         close_crit_fac=0.99
    integer, parameter          :: nsteps = 100

    ! No error unless set otherwise
    ierr = 0

    ! Find the component to use
    zmax = maxloc(z)
    icomp = zmax(1)
    zpure(:) = 0.0
    zpure(icomp) = 1.0

    ! Find the cricital parameters of the component
    call getCriticalParam(icomp,Tci,pci,oi)
    if (debug) write(*,*) "pci, Tci = ",pci,Tci

    ! Find critical entropy
    call entropy(tci,pci,zpure,LIQPH,s_crit)

    ! Find the saturation-temperature at p_low
    T_guess = 0.5*Tci
    p = p_low
    T_low = safe_bubT(p,zpure,y)
    if (debug) write(*,*) "p_low, T_low = ",p_low,T_low

    ! Find LIQ and VAP saturation-entropies at p_low
    call entropy(T_low, p_low, zpure, LIQPH, s_low_liq)
    call entropy(T_low, p_low, zpure, VAPPH, s_low_vap)

    if (debug) then
      write(*,*) "s_spec: ", s_spec
      write(*,*) "s_low_liq, s_crit, s_low_vap: ", s_low_liq,s_crit,s_low_vap
    endif

    ! Can now decide if it will hit the line, and if so, on which side.
    if (s_spec < s_low_liq) then
      ! Will not cross before p_low (passes above line)
      has_crossing = .false.

    elseif (s_spec > s_low_vap) then
      ! Will not cross before p_low (passes below line)
      has_crossing = .false.

    elseif (s_spec < s_crit) then
      ! Will meet the line from above (liquid side)
      has_crossing = .true.
      phase_crossing = LIQPH
      s = s_low_liq

    else
      ! Will meet the line from below (vapor side)
      has_crossing = .true.
      phase_crossing = VAPPH
      s = s_low_vap
    endif

    ! Solve for s_sat = s_spec
    if (has_crossing) then
      p = p_low
      T = T_low
      ! Walk along saturation line until s crosses s_spec
      ! If liquid, we start from an s below s_spec.
      ! If vapor, we start from an s above s_spec.
      param(1) = 1 ! Specifies p
      param(2) = icomp ! The pure component
      param(4) = 1.0 ! Allow meta-extremum
      solver_cfg%rel_tol = 1.0e-15
      solver_cfg%abs_tol = 1.0e-10
      solver_cfg%max_it = 10000
      dp = (pci-p_low)/nsteps
      p_close_crit = p_low + close_crit_fac*(pci-p_low)
      is_close_crit = .false.
      found_bracket = .false.
      do
        ! Store previous p and T, for when we make a bracket.
        p_prev = p
        T_prev = T
        ! Make extrapolation to T_guess for p+dp
        call sat_fun_single(icomp,T,p,f,dfdt,dfdp,.false.)
        dtdp = -dfdp/dfdt
        p = p + dp
        if (p>p_close_crit) then
          p = p_close_crit
          is_close_crit = .true.
        endif
        T_guess = T + dp*dtdp

        ! Solve for T_sat at the new p
        param(3) = p
        XX(1) = log(T_guess)
        Xmin = log(T)
        Xmax = log(Tci)
        call nonlinear_solve(solver_cfg,sat_fun_single_if,sat_diff_single,&
             sat_diff_single,limit_dx,premReturn,setXv,XX,Xmin,Xmax,param)
        if (solver_cfg%exitflag /=0) then
          if (debug) write(*,*) "Could not solve sat_fun_single"
          ierr = solver_cfg%exitflag
          return
        endif
        T = exp(XX(1))

        ! Find single-phase entropy at the new point
        call entropy(T,p,zpure,phase_crossing,s)
        if (debug) write(*,*) "New point: T,p,s,s_spec = ",T,p,s,s_spec

        ! Exit if we passed s_spec.
        select case(phase_crossing)
        case(LIQPH)
          ! Liquid starts with an s below. Passed if suddenly above.
          if (s>s_spec) then
            found_bracket = .true.
          endif
        case (VAPPH)
          ! Vapor starts with an s above. Passed if suddenly below.
          if (s<s_spec) then
            found_bracket = .true.
          endif
        end select
        if (found_bracket) then
          ! s_sat crosses s_spec between the last p and the new one.
          p_1 = p_prev
          p_2 = p
          T_1 = T_prev
          T_2 = T
          if (debug) write(*,*) "Found bracket: ",T_1,p_1,T_2,p_2
          exit
        endif

        ! Reached critical point without finding passing s_spec?
        if (is_close_crit) then
          if (debug) write(*,*) &
               "Could not pass s_spec before reaching critical point."
          ierr = 1
          return
        endif
      end do

      ! Using the found bracket, find the point where s_sat = s_spec
      T_guess = 0.5*(T_1 + T_2)
      ! Configure solver
      solver_cfg%rel_tol = p_rel_tol
      solver_cfg%max_it = 1000
      solver_cfg%isolver = NS_RIDDERS
      ! Set the constant parameters of the objective function.
      param(1) = s_spec
      param(2) = real(icomp)
      param(3) = real(phase_crossing)
      param(4) = p_prev
      param(5) = T_prev
      param(6) = (T-T_prev)/(p-p_prev)
      ! Find f=0 inside the bracket.
      call bracketing_solver(p_1,p_2,fun_ssat_single,p_crossing,&
           solver_cfg,param)
      ! Check for successful convergence
      if (solver_cfg%exitflag /= 0) then
        if (debug) write(*,*) "Bracketing solver failed."
        ierr = solver_cfg%exitflag
        return
      endif
      ! Find the temperature where the isentrope met the saturation line.
      T_crossing = bubT(T_guess,p_crossing,zpure,y)
      if (debug) write(*,*) "Crossing at: ", T_crossing,p_crossing

    endif

  end subroutine envelope_isentrope_cross_single

  function fun_ssat_single(p,param) result(f)
    ! Objective function for s_sat(phase) = s_spec
    !
    ! a and b are parameters for a linear function for T_guess
    ! T_guess = a*p + b
    !
    implicit none
    ! Input:
    real,     intent(in)  :: p !< Pressure (Pa)
    real,     intent(in)  :: param(6) !< Parameters
    ![s_spec,ipure,phase,T_guess]
    ! Output:
    real                  :: f !< s_sat - sspec
    ! Internal:
    integer               :: ipure,phase
    real                  :: zpure(nc),y(nc),T0,s,s_spec,T_sat,p_arg,p0,T,dtdp

    ! Read from param
    s_spec = param(1)
    ipure = int(param(2)+0.1)
    phase = int(param(3)+0.1)
    p0 = param(4)
    T0 = param(5)
    dtdp = param(6)
    T = T0 + (p-p0)*dtdp
    ! Make pure composition vector
    zpure(:) = 0.0
    zpure(ipure) = 1.0
    ! Find saturation temperature at p
    p_arg = p
    T_sat = bubT(T,p_arg,zpure,y)
    ! Find single-phase entropy at saturation
    call entropy(T_sat,p,zpure,phase,s)
    ! Make objective function
    f = s - s_spec

  end function fun_ssat_single

  !-----------------------------------------------------------------------------
  !> Check if triple point have been passed by envelope mapper
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  function passedTriplePoint(Xold,Xvar,Z,beta) result(passed)
    use solideos, only: solidForming, nSolid, solidComp
    use eos, only: thermo
    implicit none
    real, dimension(nc+2), intent(in) :: Xold, Xvar
    real, intent(in) :: beta
    real, dimension(nc), intent(in) :: Z
    logical :: passed
    ! Locals
    integer :: is
    real :: t,p,fugs,sf_old, sf
    real, dimension(nc) :: lnfug, Y, X, K
    integer, parameter :: nd = 2
    real :: B(nd), IFUGAC(nd,nc)

    if (nSolid /= 1) then
      call stoperror('saturation::passesTriplePoint: No solid model initialized!')
    else
      is = solidComp(1)
    endif
    B(1) = beta
    B(2) = 1.0-beta

    K = exp(Xold(1:nc))
    t = exp(Xold(nc+1))
    p = exp(Xold(nc+2))
    X = Z/(1-beta+beta*K)
    Y = K*Z/(1-beta+beta*K)
    call thermo(t,p,Y,VAPPH,lnfug)
    IFUGAC(1,:) = 1.0/exp(lnfug)
    call thermo(t,p,X,LIQPH,lnfug)
    IFUGAC(2,:) = 1.0/exp(lnfug)
    sf_old = solidForming(t,p,is,Z,nd,B,IFUGAC,fugs)

    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))
    X = Z/(1-beta+beta*K)
    Y = K*Z/(1-beta+beta*K)
    call thermo(t,p,Y,VAPPH,lnfug)
    IFUGAC(1,:) = 1.0/exp(lnfug)
    call thermo(t,p,X,LIQPH,lnfug)
    IFUGAC(2,:) = 1.0/exp(lnfug)
    sf = solidForming(t,p,is,Z,nd,B,IFUGAC,fugs)

    passed = (sf*sf_old <= 0.0)

  end function passedTriplePoint

  subroutine aep_print(a)
    class(aep), intent(inout) :: a
    ! Locals
    character(len=4) :: type_str
    select case(a%type)
    case(AZ_NONE)
      type_str = "NONE"
    case(AZ_PAEP)
      type_str = "PAEP"
    case(AZ_CAEP)
      type_str = "CAEP"
    case(AZ_HAEP)
      type_str = "HAEP"
    end select
    print *,"Azeotropic end point type: ",type_str
    print *,"T",a%T
    print *,"x",a%x
    print *,"P",a%P
    print *,"vg",a%vg
    print *,"vl",a%vl
  end subroutine aep_print

end module saturation_curve
