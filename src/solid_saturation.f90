module solid_saturation
  use saturation
  use saturation_curve
  use eos, only: thermo
  use thermopack_constants, only: clen, LIQPH, VAPPH, SOLIDPH, verbose
  use thermopack_var, only: nc, nph, get_active_thermo_model, thermo_model, &
       tpTmin, tpTmax, tpPmin, tpPmax
  use nonlinear_solvers
  !use nonlinear_solvers
  !use numconstants, only: machine_prec
  !use puresaturation, only: puresat
  implicit none
  private
  save

  integer, parameter :: &
       TRIPLE         = 1, &
       VARSOLID       = 2, &
       VARFLUID       = 3, &
       VARFLUID_DUMMY = 4

  logical, parameter :: debug=.false.

  public :: solidEnvelopePlot, solidFluidEqSingleComp
  public :: TRIPLE, VARSOLID, VARFLUID, VARFLUID_DUMMY
  public :: tripleLineDiff, threePhaseLineTemperature
  public :: solidPointOnEnvelopeThreePh
  public :: sat_diff_newton_threePh, solidPointOnEnvelope
  public :: newton_extrapolate_solid, tripleAreaEdge
  public :: newton_extrapolate_threePh
  public :: solidEnvelopePlotSingle

contains

  !-----------------------------------------------------------------------------
  !> Solid-fluid line, specified pressure, solve for temperature
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine satSolid(Z,Zsolid,T,p,betaSol,fluidPhase,f,dfdt,dfdp,dfdbs)
    use solideos, only: solid_thermo
    implicit none
    real, intent(inout) :: T
    real, dimension(nc), intent(in) :: Z, Zsolid
    real, intent(in) :: p
    real, intent(in) :: betaSol
    integer, intent(in) :: fluidPhase
    real, intent(out) :: f
    real, optional, intent(out) :: dfdt
    real, optional, intent(out) :: dfdp
    real, optional, intent(out) :: dfdbs
    ! Locals
    real, dimension(nc) :: lnfug, lnfugT, lnfugP
    real, dimension(nc) :: Zstar, dZdbs
    real, dimension(nc,nc) :: lnfugn
    real :: lnfugs, lnfugsT, lnfugsP
    integer :: is
    integer, dimension(1) :: imax

    imax = maxloc(Zsolid)
    is = imax(1)
    ! if (nSolid == 1) then
    !   is = solidComp(1)
    ! else
    !   is = -1
    ! endif
    ! if (is < 1 .and. is > nc) then
    !   call stoperror("saturation::satSolid - No solid component found")
    ! endif
    Zstar = Z
    Zstar(is) = Z(is) - betaSol
    if (Zstar(is) <= 0.0) then
      call stoperror("saturation::satSolid - Negative component fraction. Solid fraction too big.")
    endif
    ! Normalize
    Zstar = Zstar/(1.0-betaSol)
    call thermo(t,p,Zstar,fluidPhase,lnfug,lnfugt,lnfugp,lnfugn)
    call solid_thermo(t,p,Zsolid,lnfugs,lnfugst,lnfugsp)
    f = lnfugs - lnfug(is) - log(Zstar(is))
    if (present(dfdt)) then
      dfdt = lnfugst - lnfugt(is)
    endif
    if (present(dfdp)) then
      dfdp = lnfugsp - lnfugp(is)
    endif
    if (present(dfdbs)) then
      dZdbs = Zstar
      dZdbs(is) = dZdbs(is) - 1.0
      dZdbs(is) = dZdbs(is)/(1.0-betaSol)
      dfdbs = -dZdbs(is)/Zstar(is) - sum(lnfugn(is,:)*dZdbs)
    endif

  end subroutine satSolid

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !> Three phase line or triple point with solid
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine sat_fun_newton_threePh(G,Xvar,param)
    use solideos, only: solid_thermo
    implicit none
    real, dimension(nc+3), intent(out) :: G !< Function values
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+5) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, Zsolid
    real, dimension(nc) :: K, lnfugV, lnfugL, Y, X
    integer :: s, i, is, mode
    real :: p, t,ln_s, beta, lnfugs, betaSol

    Z = param(1:nc)
    !beta = param(nc+1)
    s = int(param(nc+2))
    ln_s = param(nc+3)
    mode = int(param(nc+4))
    is = int(param(nc+5))

    call read_Xvar_threePh(Xvar,mode,K,t,p,beta,betaSol)
    select case (mode)
    case (TRIPLE)
      betaSol = 0.0
      beta = param(nc+1)
    case (VARSOLID)
      beta = param(nc+1)
    case (VARFLUID)
      betaSol = param(nc+1)
    case default
      call stoperror('saturation::sat_diff_newton_threePh: Wrong mode!')
    end select

    ! Get composition
    call getCompositionThreePh(Z,K,beta,betaSol,is,X,Y,Zsolid)

    if (P >= tpPmax - sat_limitEps .or. P <= tpPmin + sat_limitEps) then
      if (verbose) then
        print *,'sat_fun_newton:P (Bar):',P/1E5
      endif
      G = 0.0 ! Terminate solver
      return
    endif

    call thermo(t,p,X,LIQPH,lnfugL)
    call thermo(t,p,Y,VAPPH,lnfugV)
    call solid_thermo(t,p,Zsolid,lnfugs)
    ! Function value
    do i=1,nc
      if (Z(i) > 0.0) then
        G(i) = Xvar(i) + lnfugV(i) - lnfugL(i)
      else
        G(i) = 0.0
      endif
    enddo
    G(nc+1) = sum(Y-X)
    if (mode == TRIPLE) then
      G(nc+2) = 0.0
    else
      G(nc+2) = Xvar(s) - ln_s
    endif

    if (beta > 0.5) then
      G(nc+3) = lnfugV(is) + log(Y(is)) - lnfugs
    else
      G(nc+3) = lnfugL(is) + log(X(is)) - lnfugs
    endif

  end subroutine sat_fun_newton_threePh

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !> Three phase line or triple point with solid
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine sat_diff_newton_threePh(Jac,Xvar,param)
    use solideos, only: solid_thermo
    implicit none
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+3,nc+3), intent(out) :: Jac !< Function differentials
    real, dimension(nc+5) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, Y, X, Zstar, Zsolid
    real, dimension(nc) :: K, lnfugv, lnfugtv, lnfugpv, lnfugl
    real, dimension(nc) :: lnfugtl, lnfugpl
    real, dimension(nc,nc) :: lnfugnv, lnfugnl
    real, dimension(nc) :: dXdbeta, dYdBeta
    integer :: s, i, j, is
    real :: p, t, beta, betaSol ,lnfugs,lnfugst,lnfugsp
    integer :: mode ! TRIPLE, SOLID, FLUID
    logical :: calcSolid
    Z = param(1:nc)
    s = int(param(nc+2))
    !ln_s = param(nc+3)
    mode = int(param(nc+4))
    if (mode == VARFLUID_DUMMY) then
      mode = VARFLUID
      calcSolid = .false.
    else
      calcSolid = .true.
    endif
    is = int(param(nc+5))

    call read_Xvar_threePh(Xvar,mode,K,t,p,beta,betaSol)
    select case (mode)
    case (TRIPLE)
      betaSol = 0.0
      beta = param(nc+1)
    case (VARSOLID)
      beta = param(nc+1)
    case (VARFLUID)
      betaSol = param(nc+1)
    case default
      call stoperror('saturation::sat_diff_newton_threePh: Wrong mode!')
    end select

    ! Get composition
    call getCompositionThreePh(Z,K,beta,betaSol,is,X,Y,Zsolid,Zstar)

    call thermo(t,p,X,LIQPH,lnfugL,lnfugt=lnfugtL,lnfugp=lnfugpL,lnfugx=lnfugnL)
    call thermo(t,p,Y,VAPPH,lnfugV,lnfugt=lnfugtV,lnfugp=lnfugpV,lnfugx=lnfugnV)
    if (calcSolid) then
      call solid_thermo(t,p,Zsolid,lnfugs,lnfugst,lnfugsp)
    else
      lnfugs=0.0
      lnfugst=0.0
      lnfugsp=0.0
    endif
    ! Temperature differential
    Jac(1:nc,nc+1) = t*(lnfugtV-lnfugtL)

    ! Pressure differential
    Jac(1:nc,nc+2) = p*(lnfugpV-lnfugpL)

    ! K differentials
    do i=1,nc
      if (Z(i) > 0.0) then
        do j=1,nc
          if (Z(j) > 0.0) then
            Jac(i,j) = ((1.0-beta)*lnfugnV(i,j)+beta*lnfugnL(i,j))*X(j)*Y(j)/Zstar(j)
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
    do i=1,nc
      if (Z(i) > 0.0) then
        Jac(nc+1,i) = X(i)*Y(i)/Zstar(i)
      else
        Jac(nc+1,i) = 0.0
      endif
    enddo
    Jac(nc+1,nc+1) = 0
    Jac(nc+1,nc+2) = 0

    Jac(nc+2,:) = 0.0
    if (mode == TRIPLE) then
      Jac(:,nc+3) = 0.0
      Jac(nc+2,nc+3) = 1.0
    else
      Jac(nc+2,s) = 1.0
    endif

    Jac(nc+3,:) = 0.0
    if (beta > 0.5) then
      Jac(nc+3,1:nc) = lnfugnV(is,:)
      Jac(nc+3,is) = Jac(nc+3,is) + 1.0/Y(is)
      do j=1,nc
        if (Z(j) > 0.0) then
          Jac(nc+3,j) = Jac(nc+3,j)*(1.0-beta)*X(j)*Y(j)/Zstar(j)
        else
          Jac(nc+3,j) = 0.0
        endif
      enddo
      Jac(nc+3,nc+1) = T*(lnfugtV(is) - lnfugst)
      Jac(nc+3,nc+2) = P*(lnfugpV(is) - lnfugsp)
    else
      Jac(nc+3,1:nc) = lnfugnL(is,:)
      Jac(nc+3,is) = Jac(nc+3,is) + 1.0/X(is)
      do j=1,nc
        if (Z(j) > 0.0) then
          Jac(nc+3,j) = -Jac(nc+3,j)*beta*X(j)*Y(j)/Zstar(j)
        else
          Jac(nc+3,j) = 0.0
        endif
      enddo
      Jac(nc+3,nc+1) = T*(lnfugtL(is) - lnfugst)
      Jac(nc+3,nc+2) = P*(lnfugpL(is) - lnfugsp)
    endif

    dYdbeta = 0.0
    dXdbeta = 0.0
    if (mode == VARSOLID) then
      ! Need differentials wrpt. betaSol
      dXdbeta = X/(1.0-betaSol)
      dYdbeta = Y/(1.0-betaSol)
      dXdbeta(is) = dXdbeta(is)*(1.0 - 1.0/Zstar(is))
      dYdbeta(is) = dYdbeta(is)*(1.0 - 1.0/Zstar(is))
    else if (mode == VARFLUID) then
      ! Need differentials wrpt. beta
      do j=1,nc
        if (Z(j) > 0.0) then
          dXdbeta(j) = -(Y(j)-X(j))/Zstar(j)
        endif
      enddo
      dYdbeta = Y*dXdbeta
      dXdbeta = X*dXdbeta
    end if
    do i=1,nc
      Jac(i,nc+3) = sum(lnfugnV(i,:)*dYdbeta) - sum(lnfugnL(i,:)*dXdbeta)
    enddo
    Jac(nc+1,nc+3) = sum(dYdbeta-dXdbeta)
    if (beta > 0.5) then
      Jac(nc+3,nc+3) = sum(lnfugnV(is,:)*dYdbeta) + dYdbeta(is)/Y(is)
    else
      Jac(nc+3,nc+3) = sum(lnfugnL(is,:)*dXdbeta) + dXdbeta(is)/X(is)
    endif
  end subroutine sat_diff_newton_threePh

  function solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
       mode,ierr) result (iter)
    use nonlinear_solvers, only: nonlinear_solver,limit_dx,premReturn,setXv, &
         nonlinear_solve
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, dimension(nc), intent(inout) :: K !< Equilibrium constants
    real, intent(inout) :: t !< Temperature
    real, intent(inout) :: p !< Pressure
    real, intent(inout) :: beta !< Gas fluid fraction
    real, intent(inout) :: betaSol !< Solid fraction
    real, intent(in) :: ln_spec !< Variable specification
    integer, intent(in) :: s !< Specification variable
    integer, intent(in) :: is !< Solid component index
    integer, intent(in) :: mode !< Mode: TRIPLE, VARSOLID, VARFLUID
    integer, optional, intent(out) :: ierr !< Error flag
    integer :: iter
    ! Locals
    real, dimension(nc+5) :: param !< Parameter vector
    real, dimension(nc+3) :: Xvar !< Variable vector
    real, dimension(nc+3) :: Xmin, Xmax !< Variable limits
    real :: Tmin, Tmax
    type(nonlinear_solver) :: solver
    ! Debug
    ! real, dimension(nc+3,nc+3) :: Jac !< Function differentials
    ! real, dimension(nc+3) :: Xvar1, Fun, Fun1
    ! real, parameter :: eps = 1.0e-3
    ! integer :: ip
    param(1:nc) = Z
    param(nc+2) = real(s)
    param(nc+3) = ln_spec
    param(nc+4) = real(mode)
    param(nc+5) = real(is)
    select case (mode)
    case (TRIPLE)
      param(nc+1) = beta
      Xvar(nc+3) = 0.0
      Xmin(nc+3) = -100.0
      Xmax(nc+3) = 100.0
    case (VARSOLID)
      param(nc+1) = beta
      Xvar(nc+3) = betaSol
      Xmin(nc+3) = 0.0  ! betaSol
      Xmax(nc+3) = Z(is)*0.99 ! betaSol
    case (VARFLUID)
      param(nc+1) = betaSol
      Xvar(nc+3) = beta
      Xmin(nc+3) = -1.0 ! beta
      Xmax(nc+3) = 2.0 ! beta
    case default
      call stoperror('saturation::solidPointOnEnvelope: Wrong mode!')
    end select
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)
    Tmin = tpTmin
    Tmax = tpTmax
    Xmin(1:nc) = -1.0e100
    Xmax(1:nc) = 1.0e100
    Xmin(nc+1) = log(Tmin) !Tmin
    Xmax(nc+1) = log(Tmax) !Tmax
    Xmin(nc+2) = log(tpPmin) !Pmin
    Xmax(nc+2) = log(tpPmax) !Pmax

    Solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%limit_x_values = .true.
    solver%max_it = sat_max_iter
    solver%ls_max_it = sat_max_nr_line_s
    call nonlinear_solve(solver,sat_fun_newton_threePh,&
         sat_diff_newton_threePh,sat_diff_newton_threePh,&
         limit_dx,premReturn,setXv,Xvar,Xmin,Xmax,param)
    iter = solver%iter
    if (solver%exitflag == 0) then
      K = exp(Xvar(1:nc))
      t = exp(Xvar(nc+1))
      p = exp(Xvar(nc+2))
      select case (mode)
      case (VARSOLID)
        betaSol = Xvar(nc+3)
      case (VARFLUID)
        beta = Xvar(nc+3)
      end select
      if (verbose) then
        print *,'solidPointOnEnvelopeThreePh: converged after ', iter, &
             ' number of iterations'
      endif
    endif
    if (present(ierr)) then
      if (solver%exitflag /= 0) then
        ierr = solver%exitflag
      else
        if (T<=Tmin .or.  T>=Tmax .or. p<=tpPmin .or. p>=tpPmax) then
          ierr = -1
        else
          ierr = 0
        endif
      endif
    endif
  end function solidPointOnEnvelopeThreePh

  !-----------------------------------------------------------------------------
  !> Get phase compositions in a three phase system
  !>
  !> \author MH, 2016-03
  !-----------------------------------------------------------------------------
  subroutine getCompositionThreePh(Z,K,beta,betaSol,is,X,Y,Zsol,Zstar)
    implicit none
    ! Input
    real, dimension(nc), intent(in) :: K !< Equilibrium constants
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(in) :: beta !< Gas fluid fraction
    real, intent(in) :: betaSol !< Solid fraction
    integer, intent(in) :: is !< Solid component index
    ! Output
    real, dimension(nc), intent(out) :: X, Y !< Composition of fluid phases
    real, dimension(nc), intent(out) :: Zsol !< Solid composition
    real, dimension(nc), optional, intent(out) :: Zstar !< Corrected fluid composition
    ! Locals
    real :: localZstar(nc)
    localZstar = Z
    localZstar(is) = Z(is) - betaSol
    if (localZstar(is) <= 0.0) then
      call stoperror("saturation::getCompositionThreePh - Negative component fraction. Solid fraction too big.")
    endif
    ! Normalize
    localZstar = localZstar/(1.0-betaSol)

    X = localZstar/(1-beta+beta*K)
    Y = K*localZstar/(1-beta+beta*K)
    Zsol = 0.0
    Zsol(is) = 1.0
    if (present(Zstar)) then
      Zstar = localZstar
    endif
  end subroutine getCompositionThreePh

  !-----------------------------------------------------------------------------
  !> Set three-phase variable vector depending on mode
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine set_Xvar_threePh(Xvar,mode,K,t,p,beta,betaSol)
    implicit none
    real, dimension(nc+3), intent(out) :: Xvar !< Variable vector
    real, dimension(nc), intent(in) :: K !< Equilibrium constants
    real, intent(in) :: t !< Temperature
    real, intent(in) :: p !< Pressure
    real, intent(in) :: beta !< Gas fluid fraction
    real, intent(in) :: betaSol !< Solid fraction
    integer, intent(in) :: mode !< Mode: TRIPLE, VARSOLID, VARFLUID
    !
    select case (mode)
    case (TRIPLE)
      Xvar(nc+3) = 0.0
    case (VARSOLID)
      Xvar(nc+3) = betaSol
    case (VARFLUID)
      Xvar(nc+3) = beta
    case default
      call stoperror('saturation::sat_diff_newton_threePh: Wrong mode!')
    end select

    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)

  end subroutine set_Xvar_threePh

  !-----------------------------------------------------------------------------
  !> Set variables from three-phase variable vector depending on mode
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine read_Xvar_threePh(Xvar,mode,K,t,p,beta,betaSol)
    implicit none
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc), intent(out) :: K !< Equilibrium constants
    real, intent(out) :: t !< Temperature
    real, intent(out) :: p !< Pressure
    real, intent(out) :: beta !< Gas fluid fraction
    real, intent(out) :: betaSol !< Solid fraction
    integer, intent(in) :: mode !< Mode: TRIPLE, VARSOLID, VARFLUID
    !
    select case (mode)
    case (TRIPLE)
      beta = 0.0
      betaSol = 0.0
    case (VARSOLID)
      betaSol = Xvar(nc+3)
    case (VARFLUID)
      beta = Xvar(nc+3)
    case default
      call stoperror('saturation::read_Xvar_threePh: Wrong mode!')
    end select

    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))

  end subroutine read_Xvar_threePh

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along three phase line
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine newton_extrapolate_threePh(Z,K,t,p,beta,betaSol,s,is,&
       ln_s,dln_s,mode,dXds,dln_s_max)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(inout) :: K
    real, intent(inout) :: t,p
    real, intent(inout) :: beta,betaSol
    real, intent(in) :: ln_s
    real, intent(inout) :: dln_s
    integer, intent(in) :: s,mode,is
    real, optional, dimension(nc+3), intent(inout) :: dXds
    real, optional, intent(in) :: dln_s_max
    ! Locals
    real, dimension(nc+3,nc+3) :: Jac
    real, dimension(nc+3) :: X, dXdS_local, dX
    integer, dimension(nc+3) :: INDX
    integer :: INFO, imax(1)
    real, dimension(nc+5) :: param
    real :: scaling
    ! Debug
    !real, dimension(nc+3) :: G0, G1
    !integer :: i
    !
    call set_Xvar_threePh(X,mode,K,t,p,beta,betaSol)

    param(1:nc) = Z
    param(nc+2) = real(s)
    param(nc+3) = ln_s
    param(nc+4) = real(mode)
    param(nc+5) = real(is)
    select case (mode)
    case (VARSOLID)
      param(nc+1) = beta
    case (VARFLUID)
      param(nc+1) = betaSol
    case default
      call stoperror('saturation::newton_extrapolate_threePh: Wrong mode!')
    end select

    call sat_diff_newton_threePh(Jac,X,param)
    if (present(dXds)) then
      dXdS_local = dXdS
    else
      dXdS_local = 0.0
      dXdS_local(nc+2) = 1.0
    endif
    ! Solve equation system
    call DGESV(nc+3, 1, Jac, nc+3, INDX, dXdS_local, nc+3, INFO )
    ! Extrapolate
    dX = dXds_local*dln_s
    if (present(dln_s_max)) then
      imax = maxloc(abs(dX))
      if (abs(dX(imax(1))) > dln_s_max) then
        dln_s = sign(1.0,dln_s)*dln_s_max/abs(dXds_local(imax(1)))
        dX = dXds_local*dln_s
      endif
      if (dX(nc+3) > 0.999999*Z(is) - X(nc+3)) then
        dX(nc+3) = 0.0
      endif
    else
      if (dX(nc+3) > 0.999999*Z(is) - X(nc+3)) then
        if (dX(nc+3) > 1.0e-25) then
          scaling = (0.9999999*Z(is) - X(nc+3))/dX(nc+3)
          dX = dX*scaling
        endif
      endif
      if (abs(dX(nc+3)) > 0.05) then
        scaling = 0.05/abs(dX(nc+3))
        dX = dX*scaling
      endif
    ! else
    !   call sat_fun_newton_threePh(G0,X,param)
    !   call sat_diff_newton_threePh(Jac,X,param)
    !   i = 5
    !   X(i) = X(i) + 1.0e-5
    !   call sat_fun_newton_threePh(G1,X,param)
    !   print *,'Num',(G1-G0)/1.0e-5
    !   print *,'Ana',Jac(:,i)
    !   stop
    endif
    X = X + dX
    call read_Xvar_threePh(X,mode,K,t,p,beta,betaSol)
    if (present(dXds)) then
      dXdS = dXdS_local
    endif

  end subroutine newton_extrapolate_threePh

  !-----------------------------------------------------------------------------
  !> Solid-fluid line function values for non-linear solve
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine sat_fun_newton_solid(Fun,Xvar,param)
    implicit none
    real, dimension(2), intent(out) :: Fun !< Function values
    real, dimension(2), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+5) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, Zsolid
    integer :: s, is, fluidPhase
    real :: p, t, ln_s, betaSol, f
    t = exp(Xvar(1))
    p = exp(Xvar(2))
    Z = param(1:nc)
    betaSol = param(nc+1)
    s = int(param(nc+2))
    ln_s = param(nc+3)
    is = int(param(nc+4))
    fluidPhase = int(param(nc+5))
    Zsolid = 0.0
    Zsolid(is) = 1.0

    call satSolid(Z,Zsolid,T,p,betaSol,fluidPhase,f)
    Fun(1) = f
    Fun(2) = Xvar(s) - ln_s

  end subroutine sat_fun_newton_solid

  !-----------------------------------------------------------------------------
  !> Differentials for solid-fluid line function values for non-linear solver
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine sat_diff_newton_solid(Jac,Xvar,param)
    implicit none
    real, dimension(2), intent(in) :: Xvar !< Variable vector
    real, dimension(2,2), intent(out) :: Jac !< Function differentials
    real, dimension(nc+5) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, Zsolid
    integer :: s, is, fluidPhase
    real :: p, t, ln_s, betaSol, f, dfdt, dfdp
    t = exp(Xvar(1))
    p = exp(Xvar(2))
    Z = param(1:nc)
    betaSol = param(nc+1)
    s = int(param(nc+2))
    ln_s = param(nc+3)
    is = int(param(nc+4))
    fluidPhase = int(param(nc+5))
    Zsolid = 0.0
    Zsolid(is) = 1.0

    call satSolid(Z,Zsolid,T,p,betaSol,fluidPhase,f,dfdt,dfdp)
    Jac(1,1) = t*dfdt
    Jac(1,2) = p*dfdp

    Jac(2,:) = 0.0
    Jac(2,s) = 1.0

  end subroutine sat_diff_newton_solid

  function solidPointOnEnvelope(Z,t,p,fluidPhase,betaSol,is,s,&
       ln_spec,ierr) result (iter)
    use nonlinear_solvers, only: nonlinear_solver,limit_dx,premReturn,setXv, &
         nonlinear_solve
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(inout) :: t !< Temperature
    real, intent(inout) :: p !< Pressure
    integer, intent(in) :: fluidPhase !< Fluid phase flag
    real, intent(inout) :: betaSol !< Solid fraction
    real, intent(in) :: ln_spec !< Variable specification
    integer, intent(in) :: s !< Specification variable
    integer, intent(in) :: is !< Solid component index
    integer, optional, intent(out) :: ierr !< Error flag
    integer :: iter
    ! Locals
    real, dimension(nc+5) :: param !< Parameter vector
    real, dimension(2) :: Xvar !< Variable vector
    real, dimension(2) :: Xmin, Xmax !< Variable limits
    real :: Tmin, Tmax
    type(nonlinear_solver) :: solver
    ! Debug
    ! real, dimension(2,2) :: Jac !< Function differentials
    ! real, dimension(2) :: Xvar1, Fun, Fun1
    ! real, parameter :: eps = 1.0e-5
    ! integer :: ip

    Xvar(1) = log(t)
    Xvar(2) = log(p)
    param(1:nc) = Z
    param(nc+1) = betaSol
    param(nc+2) = real(s)
    param(nc+3) = ln_spec
    param(nc+4) = real(is)
    param(nc+5) = real(fluidPhase)

    Tmin = tpTmin
    Tmax = tpTmax
    Xmin(1) = log(Tmin) !Tmin
    Xmax(1) = log(Tmax) !Tmax
    Xmin(2) = log(tpPmin) !Pmin
    Xmax(2) = log(tpPmax) !Pmax

    ! Debug
    ! call sat_fun_newton_threePh(Fun,Xvar,param)
    ! call sat_diff_newton_threePh(Jac,Xvar,param)
    ! Xvar1 = Xvar
    ! ip = 1
    ! Xvar1(ip) = Xvar1(ip) + Xvar1(ip)*eps
    ! call sat_fun_newton_threePh(Fun1,Xvar1,param)
    ! print *,'numerical',(Fun1-Fun)/(Xvar(ip)*eps)
    ! print *,'analytical',Jac(:,ip)
    ! stop

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-8
    solver%limit_x_values = .false.
    solver%max_it = sat_max_iter
    solver%ls_max_it = sat_max_nr_line_s
    call nonlinear_solve(solver,sat_fun_newton_solid,&
         sat_diff_newton_solid,sat_diff_newton_solid,&
         limit_dx,premReturn,setXv,Xvar,Xmin,Xmax,param)
    iter = solver%iter
    if (solver%exitflag == 0) then
      t = exp(Xvar(1))
      p = exp(Xvar(2))
      if (verbose) then
        print *,'solidPointOnEnvelope: converged after ', iter, &
             ' number of iterations'
      endif
    endif
    if (present(ierr)) then
      if (solver%exitflag /= 0) then
        ierr = solver%exitflag
      else
        if (T<=Tmin .or.  T>=Tmax .or. p<=tpPmin .or. p>=tpPmax) then
          ierr = -1
        else
          ierr = 0
        endif
      endif
    endif

  end function solidPointOnEnvelope

  !-----------------------------------------------------------------------------
  !> Find variable sensitivities along solid-fluid line
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine newton_extrapolate_solid(Z,t,p,betaSol,fluidPhase,s,is,ln_s,dln_s,dXds_out)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, intent(inout) :: t,p
    real, intent(in) :: betaSol
    real, intent(in) :: ln_s,dln_s
    integer, intent(in) :: s,is,fluidPhase
    real, optional, intent(out) :: dXds_out(2)
    ! Locals
    real, dimension(2,2) :: Jac
    real, dimension(2) :: X,dXdS
    integer, dimension(2) :: INDX
    integer :: INFO
    real, dimension(nc+5) :: param

    X(1) = log(t)
    X(2) = log(p)
    param(1:nc) = Z
    param(nc+1) = betaSol
    param(nc+2) = real(s)
    param(nc+3) = ln_s
    param(nc+4) = real(is)
    param(nc+5) = real(fluidPhase)

    call sat_diff_newton_solid(Jac,X,param)
    dXdS(1) = 0.0
    dXdS(2) = 1.0

    ! Solve equation system
    call DGESV(2, 1, Jac, 2, INDX, dXdS, 2, INFO )

    ! Extrapolate
    X = X + dXds*dln_s
    t = exp(X(1))
    p = exp(X(2))
    if (present(dXds_out)) then
      dXds_out = dXds
    endif

  end subroutine newton_extrapolate_solid

  !-----------------------------------------------------------------------------
  !> Plot saturation line in TP space for VLSE
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine solidEnvelopePlot(Z,T_init,p_init,spec,Pmax,filename,plotESV)
    use solideos, only: nSolid, solidComp
    use utilities, only: newunit
    use thermo_utils, only: isSingleComp
    implicit none
    ! Input:
    real,              intent(in)  :: Z(nc)       ! Total molar comp. (-)
    real,              intent(in)  :: T_init      ! T-guess initial point (K)
    real,              intent(in)  :: p_init      ! p-guess initial point (Pa)
    integer,           intent(in)  :: spec        ! 1: Specify P, T Varies
                                                  ! 2: Specify T: P varies
    real,              intent(in)  :: Pmax        ! Maximum pressure (Pa)
    character(len=*),  intent(in)  :: filename    ! File for output
    logical, optional, intent(in)  :: plotESV     ! Plot energy, entropy and density
    ! Internal:
    integer,        parameter   :: nmax = 1000 ! Maximum number of points
    real                        :: Ta(nmax)    ! Sat. temp. values (K)
    real                        :: pa(nmax)    ! Sat. pres. values (Pa)
    integer                     :: n           ! Number of points found
    real                        :: Ki(nmax,nc) ! K factors
    real                        :: betai(nmax) ! Gas phase fraction
    real :: T, p, beta, betaSol, ln_spec, K(nc), p0, e, v, ent, dln_s
    real :: pn, pcrit, tcrit, critLV(2)
    real :: betaMin, point(5), betaSolCrit
    integer :: s, is, iter, ierr, i, n_vap_line
    integer :: ifile
    logical :: plotEnergiesEtc, closing, has_crit_vls
    character(len=*), parameter :: sep = '  '
    character(len=*), parameter :: non = 'NaN' ! '.' for gnuplot, NaN for numpy
    character(len=clen) :: line, mergedlines
    ! Triple points
    real :: Ttr, Ptr, Ktr(nc)
    real :: Ttr2, Ptr2, Ktr2(nc)
    ! Three phase lines
    integer, parameter :: nsol = 250
    real :: Kl(nsol,nc), bl(nsol), Tl(nsol), Pl(nsol)
    real :: bs(2*nsol), Ks(2*nsol,nc), Ts(2*nsol), Ps(2*nsol)
    real :: bs2(nsol), Ks2(nsol,nc), Ts2(nsol), Ps2(nsol)
    ! Solid gas line
    real :: tsg(nsol),psg(nsol)
    ! Solid liquid line
    real :: tsl(nsol),psl(nsol)
    ! Number of points in each line
    integer :: n_sol_lines(5)

    if (nSolid /= 1) then
      call stoperror('saturation::solidEnvelopePlot: No solid model initialized!')
    else
      is = solidComp(1)
    endif
    plotEnergiesEtc = .true.
    if (present(plotESV)) then
      plotEnergiesEtc = plotESV
    endif
    has_crit_vls = .false.
    if (isSingleComp(z)) then
      n = 100
      call solidEnvelopePlotSingle(Z,T_init,p_init,spec,Pmax,n,Ta,Pa,tsg,psg,tsl,psl,ierr)
      n_sol_lines = (/n, n, 0, 0, 0/)
      Ki(1:n,:) = 1.0
      betai(1:n) = 1.0
      Ttr = Ta(1)
      Ptr = Pa(1)
      Ktr = 1.0
      Ptr2 = 0.0
      Ttr2 = 0.0
      critLV(1) = Ta(n)
      critLV(2) = Pa(n)
      tcrit = 0
      pcrit = 0
      if (plotEnergiesEtc) then
        Ki(n+1:2*n,:) = 1.0
        betai(n+1:2*n) = 0.0
        do i=1,n
          Ta(n+i) = Ta(n+1-i)
          Pa(n+i) = Pa(n+1-i)
        enddo
        n_sol_lines(4) = n
        Ts(1:n) = tsl(1:n)
        Ps(1:n) = psl(1:n)
        Ks(1:n,:) = 1.0
        bs(1:n) = 1.0
        n_sol_lines(5) = n
        Ts2(1:n) = tsg(1:n)
        Ps2(1:n) = psg(1:n)
        Ks2(1:n,:) = 1.0
        bs2(1:n) = 1.0
        n = n*2
      endif
    else
      ! Default number of points in lines
      n_sol_lines = (/nsol, nsol, nsol, 2*nsol, nsol/)
      !              Tsg,   Tsl,  Tl,   Ts,    Ts2
      if (nc == 2) n_sol_lines(4) = 0

      ! Initialize critical point values
      tcrit = 0
      pcrit = 0

      beta = 1.0
      call envelopePlot(Z,T_init,p_init,spec,beta,Pmax,nmax,&
           Ta,Pa,Ki,betai,n,exitOnTriplePoint=.true.)

      ! Initial pressure
      p0 = Pa(1)

      ! Locate triple point
      ! Initial values
      t=Ta(n)
      p=Pa(n)
      betaSol = 0.0
      K = Ki(n,:)
      iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
           TRIPLE,ierr)
      if (ierr /= 0) then
        print *,"Not able to solve for triple line edge where sublimation "//&
             "and saturation curve meets"
      endif
      if (verbose) then
        print *
        print *,'Triple point for mixture'
        print *,'Triple point temperature (K): ',T
        print *,'Triple point pressure    (Pa): ',p
      endif
      Ttr = T
      Ptr = p
      Ktr = K

      ! Map solid appearance line
      if (verbose) then
        print *
        print *,'Mapping solid appearance line'
      endif
      closing = .true.
      Kl(1,:) = K
      bl(1) = 1.0
      Tl(1) = t
      Pl(1) = p
      betaSol = 0.0
      betaMin = 0.0
      s = nc+3
      do i=2,nsol-1
        beta = 1.0 - real(i-1)/real(nsol-1)
        ln_spec = beta
        iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
             VARFLUID,ierr)
        if (p > Pmax) then
          closing = .false.
          ln_spec = log(Pmax)
          s = nc+2
          iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
               VARFLUID,ierr)
          betaMin = beta
          exit
        endif
        if (ierr /= 0) then
          print *,"Not able to solve for point on triple line (solid apperance)."
          n_sol_lines(3) = i - 1
          exit
        endif
        if (verbose) then
          print *,t,p
        endif
        bl(i) = beta
        Kl(i,:) = K
        Tl(i) = t
        Pl(i) = p
      enddo

      if (.not. closing) then
        ! Not all lines will be mapped:
        n_sol_lines = (/nsol, 0, nsol, 0, 0/)
        ! Remap
        K=Kl(1,:)
        t=Tl(1)
        p=Pl(1)
        s = nc+3
        do i=2,nsol
          beta = 1.0 - (1.0 - betaMin)*real(i-1)/real(nsol-1)
          ln_spec = beta
          iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
               VARFLUID,ierr)
          if (ierr /= 0) then
            print *,"Not able to solve for point on triple line (solid apperance)."
            n_sol_lines(3) = i - 1
            exit
          endif
          if (verbose) then
            print *,t,p
          endif
          bl(i) = beta
          Kl(i,:) = K
          Tl(i) = t
          Pl(i) = p
        enddo
      endif

      if (closing) then
        ! Locate triple point
        beta = 0.0
        iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
             TRIPLE,ierr)
        if (verbose) then
          print *
          print *,'Triple point 2 for mixture'
          print *,'Triple point temperature (K): ',T
          print *,'Triple point pressure    (Pa): ',p
        endif
        Ttr2 = T
        Ptr2 = p
        Ktr2 = K
        Kl(nsol,:) = Ktr2
        bl(nsol) = 0.0
        Tl(nsol) = Ttr2
        Pl(nsol) = Ptr2
      else
        Ptr2 = Pmax
        Ttr2 = Ttr
      endif

      ! Mapping solid-gas line starting at triple point
      if (verbose) then
        print *
        print *,'Mapping solid-gas line'
      endif
      t = Ttr
      p = Ptr
      betaSol = 0.0
      s = 2 ! Specify pressure
      do i=1,nsol
        pn = Ptr - (Ptr-p0)*real(i-1)/real(nsol-1)
        ln_spec = log(pn)
        dln_s = ln_spec - log(p)
        call newton_extrapolate_solid(Z,t,p,betaSol,VAPPH,s,is,ln_spec,dln_s)
        iter = solidPointOnEnvelope(Z,t,p,VAPPH,betaSol,is,s,&
             ln_spec,ierr)
        if (ierr /= 0) then
          print *,"Not able to solve for point on sublimation line."
          n_sol_lines(1) = i - 1
          exit
        endif
        tsg(i) = t
        psg(i) = p
        if (verbose) then
          print *,t,p
        endif
      enddo

      ! Mapping solid-liquid line starting at second triple point
      if (closing) then
        if (verbose) then
          print *
          print *,'Mapping solid-liquid line'
        endif
        t = Ttr2
        p = Ptr2
        betaSol = 0.0
        s = 2 ! Specify pressure
        do i=1,nsol
          pn = Ptr2 + (Pmax-Ptr2)*real(i-1)/real(nsol-1)
          ln_spec = log(pn)
          dln_s = ln_spec - log(p)
          call newton_extrapolate_solid(Z,t,p,betaSol,LIQPH,s,is,ln_spec,dln_s)
          iter = solidPointOnEnvelope(Z,t,p,LIQPH,betaSol,is,s,&
               ln_spec,ierr)
          if (ierr /= 0) then
            print *,"Not able to solve for point on melting line."
            n_sol_lines(2) = i - 1
            exit
          endif
          tsl(i) = t
          psl(i) = p
          if (verbose) then
            print *,t,p
          endif
        enddo
      endif

      if (closing .and. nc > 2) then
        if (verbose) then
          print *,'Zero liquid line'
        endif
        Ts(1) = Tl(1)
        Ps(1) = Pl(1)
        bs(1) = 0.0
        Ks(1,:) = Kl(1,:)
        s = nc+2
        beta = 1.0
        p = Ps(1)
        t = Ts(1)
        K = Ks(1,:)
        betaSol = bs(1)
        do i=2,nsol
          ln_spec = log(Pl(i))
          dln_s = ln_spec - log(Ps(i-1))
          call newton_extrapolate_threePh(Z,K,t,p,beta,betaSol,s,is,ln_spec,&
               dln_s,VARSOLID)
          iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
               VARSOLID,ierr)
          if (ierr /= 0) then
            print *,"Not able to solve for point on triple line (zero liquid)."
            n_sol_lines(4) = i - 1
            exit
          endif
          if (verbose) then
            print *,t,p, betaSol
          endif
          Ks(i,:) = K
          Ts(i) = t
          Ps(i) = p
          bs(i) = betaSol
        enddo

        do i=nsol+1,2*nsol
          beta = 1.0
          pn = Ptr2 + (Pmax-Ptr2)*real(i-1-nsol)/real(nsol-1)
          ln_spec = log(pn)
          dln_s = ln_spec - log(Ps(i-1))
          call newton_extrapolate_threePh(Z,K,t,p,beta,betaSol,s,is,ln_spec,&
               dln_s,VARSOLID)
          iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
               VARSOLID,ierr)
          if (ierr /= 0) then
            print *,"Not able to solve for point on zero liquid."
            if (i > nsol + 1) n_sol_lines(4) = i - 1
            exit
          endif
          if (verbose) then
            print *,t,p, betaSol
          endif
          Ks(i,:) = K
          Ts(i) = t
          Ps(i) = p
          bs(i) = betaSol
        enddo
      endif

      if (closing) then
        if (verbose) then
          print *
          print *,'Mapping vapour appearance line'
        endif
        beta = 0.0
        Ts2(1) = Ttr2
        Ps2(1) = Ptr2
        Ks2(1,:) = Ktr2
        bs2(1) = 0.0
        s = nc+2 ! Specify pressure
        t = Ttr2
        K = Ktr2
        p = Ptr2
        betaSol = 0.0
        n_vap_line = nsol
        do i=2,nsol
          pn = Ptr2 + (Pmax-Ptr2)*real(i-1)/real(nsol-1)
          ln_spec = log(pn)
          dln_s = ln_spec - log(Ps2(i-1))
          call newton_extrapolate_threePh(Z,K,t,p,beta,betaSol,s,is,ln_spec,&
               dln_s,VARSOLID)
          iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
               VARSOLID,ierr)
          if (ierr /= 0) then
            print *,"Not able to solve for point on vapour appearance line."
            n_sol_lines(5) = i - 1
            exit
          endif
          if (isCriticalThreePhase(Z,K,t,p,beta,betaSol,is,tcrit,pcrit,ierr)) then
            betaSolCrit = betaSol
            has_crit_vls = .true.
            n_vap_line = i
          endif
          if (verbose) then
            print *,t,p, betaSol
          endif
          Ks2(i,:) = K
          Ts2(i) = t
          Ps2(i) = p
          bs2(i) = betaSol
          if (has_crit_vls) exit
        enddo
        n_sol_lines(5) = n_vap_line
      endif

      if (plotEnergiesEtc .and. nc == 2) then
        ! Calculate energies for curve where liquid disapear
        n_sol_lines(4) = n_sol_lines(3) + n_sol_lines(5)
        Ks(1:n_sol_lines(3),:) = Kl(1:n_sol_lines(3),:)
        Ts(1:n_sol_lines(3)) = Tl(1:n_sol_lines(3))
        Ps(1:n_sol_lines(3)) = Pl(1:n_sol_lines(3))
        ! Triple line
        do i=1,n_sol_lines(3)
          bs(i) = Z(is)*(1-Ks(i,is))/(1-Z(is)*Ks(i,is))
        enddo
        ! Vapour-liquid line in coexistence with solid
        Ks(n_sol_lines(3)+1:n_sol_lines(4),:) = Ks2(1:n_sol_lines(5),:)
        Ts(n_sol_lines(3)+1:n_sol_lines(4)) = Ts2(1:n_sol_lines(5))
        Ps(n_sol_lines(3)+1:n_sol_lines(4)) = Ps2(1:n_sol_lines(5))
        bs(n_sol_lines(3)+1:n_sol_lines(4)) = bs2(1:n_sol_lines(5))
        n_vap_line = n_sol_lines(5)
        if (has_crit_vls) then
          n_vap_line = n_vap_line - 1
          bs(n_sol_lines(4)) = betaSolCrit
        endif
        do i=1,n_vap_line
          bs(n_sol_lines(3)+i) = Z(is)*(1-Ks(n_sol_lines(3)+i,is))/(1-Z(is)*Ks(n_sol_lines(3)+i,is))
        enddo
      endif

      if (verbose) then
        print *,'Mapping liquid-vapour line'
      endif
      beta = 1.0
      call envelopePlot(Z,Ttr,Ptr,1,beta,Pmax,nmax,&
           Ta,Pa,Ki,betai,n,Tme=Ttr2,crit=critLV,&
           dS_override=0.05)
      if (verbose) then
        do i=1,n
          print *,Ta(i),Pa(i)
        enddo
      endif

      if (.not. closing) then
        Ptr2 = 0.0
        Ttr2 = 0.0
      endif
      ! TODO: Map solid iso-fraction lines
      ! betaSol = 0.5*Z(is)
      ! call solveForZeroBeta(Z,Pl(i-1),Pl(i),p,t,K,betaSol,is)
      ! print *,'t,p',t,p

    endif

    ifile = newunit()
    open(file=trim(filename),unit=ifile)
    write(ifile,'(A)') '#Solid-liquid-gas equilibrium line plot:'
    if (plotEnergiesEtc) then
      call getESV(Ttr,Ptr,Z,1.0,0.0,is,e,ent,v,Ktr)
      write(ifile,'(A,5es19.10e3)') '#Triple point 1: ',Ttr,Ptr,e,ent,v
      if (Ttr2 > 0) then
        call getESV(Ttr2,Ptr2,Z,0.0,0.0,is,e,ent,v,Ktr2)
        point = (/Ttr2,Ptr2,e,ent,v/)
      else
        point = 0
      endif
      write(ifile,'(A,5es19.10e3)') '#Triple point 2: ', point
      if (critLV(1) > 0)  then
        call getESV(critLV(1),critLV(2),Z,0.0,0.0,is,e,ent,v)
        point = (/critLV(1),critLV(2),e,ent,v/)
      else
        point = 0
      endif
      write(ifile,'(A,5es19.10e3)') '#Critical point 1: ', point
      if (has_crit_vls)  then
        call getESV(tcrit,pcrit,Z,0.0,betaSolCrit,is,e,ent,v)
        point = (/tcrit,pcrit,e,ent,v/)
      else
        point = 0
      endif
      write(ifile,'(A,5es19.10e3)') '#Critical point 2: ', point
    else
      write(ifile,'(A,2es19.10e3)') '#Triple point 1: ',Ttr,Ptr
      write(ifile,'(A,2es19.10e3)') '#Triple point 2: ',Ttr2,Ptr2
      write(ifile,'(A,2es19.10e3)') '#Critical point 1: ',critLV
      write(ifile,'(A,2es19.10e3)') '#Critical point 2: ', tcrit, pcrit
    endif
    if (plotEnergiesEtc) then
      line = '#Tgl (K) [1]'//sep//'Pgl (Pa) [2]'//&
           sep//'egl (J/mol) [3]'//sep//'sgl (J/mol/K) [4]'//&
           sep//'vgl (m3/mol) [5]'//&
           sep//'Tgs (K) [6]'//sep//'Pgs (Pa) [7]'//&
           sep//'egs (J/mol) [8]'//sep//'sgs (J/mol/K) [9]'//&
           sep//'vgs (m3/mol) [10]'//&
           sep//'Tls (K) [11]'//sep//'Pls (Pa) [12]'//&
           sep//'els (J/mol) [13]'//sep//'sls (J/mol/K) [14]'//&
           sep//'vls (m3/mol) [15]'//&
           sep//'Tgls1 (K) [16]'//sep//'Pgls1 (Pa) [17]'//&
           sep//'egls1 (J/mol) [18]'//sep//'sgls1 (J/mol/K) [19]'//&
           sep//'vgls1 (m3/mol) [20]'//&
           sep//'Tgls2 (K) [21]'//sep//'Pgls2 (Pa) [22]'//&
           sep//'egls2 (J/mol) [23]'//sep//'sgls2 (J/mol/K) [24]'//&
           sep//'vgls2 (m3/mol) [25]'//sep//'Tgls3 (K) [26]'//&
           sep//'Pgls3 (Pa) [27]'//sep//'egls3 (J/mol) [28]'//&
           sep//'sgls3 (J/mol/K) [29]'//sep//'vgls3 (m3/mol) [30]'
      write(ifile,'(A)') trim(line)
      do i=1,max(n,maxval(n_sol_lines))
        if (i > n) then
          line = non // sep // non // sep // non // sep // non // sep // non
        else
          call getESV(Ta(i),Pa(i),Z,betai(i),0.0,is,e,ent,v,Ki(i,:))
          write(line,'(5es19.10e3)') Ta(i), Pa(i), e, ent, v
        endif
        mergedlines = trim(line)
        if (i > n_sol_lines(1)) then
          line = non // sep // non // sep // non // sep // non // sep // non
        else
          call getESV(Tsg(i),Psg(i),Z,1.0,0.0,is,e,ent,v)
          write(line,'(5es19.10e3)') Tsg(i), Psg(i), e, ent, v
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(2)) then
          line = non // sep // non // sep // non // sep // non // sep // non
        else
          call getESV(Tsl(i),Psl(i),Z,0.0,0.0,is,e,ent,v)
          write(line,'(5es19.10e3)') Tsl(i), Psl(i), e, ent, v
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(3)) then
          line = non // sep // non // sep // non // sep // non // sep // non
        else
          call getESV(Tl(i),Pl(i),Z,bl(i),0.0,is,e,ent,v,Kl(i,:))
          write(line,'(5es19.10e3)') Tl(i), Pl(i), e, ent, v
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(4)) then
          line = non // sep // non // sep // non // sep // non // sep // non
        else
          call getESV(Ts(i),Ps(i),Z,1.0,bs(i),is,e,ent,v,Ks(i,:))
          write(line,'(5es19.10e3)') Ts(i), Ps(i), e, ent, v
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(5)) then
          line = non // sep // non // sep // non // sep // non // sep // non
        else
          call getESV(Ts2(i),Ps2(i),Z,0.0,bs2(i),is,e,ent,v,Ks2(i,:))
          write(line,'(5es19.10e3)') Ts2(i), Ps2(i), e, ent, v
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        write(ifile,'(A)') trim(mergedlines)
      enddo
    else
      line = '#Tgl (K) [1]'//sep//'Pgl (Pa) [2]'//&
           sep//'Tgs (K) [3]'//sep//'Pgs (Pa) [4]'//&
           sep//'Tls (K) [5]'//sep//'Pls (Pa) [6]'//&
           sep//'Tgls1 (K) [7]'//sep//'Pgls1 (Pa) [8]'//&
           sep//'Tgls2 (K) [9]'//sep//'Pgls2 (Pa) [10]'//&
           sep//'Tgls3 (K) [11]'//sep//'Pgls3 (Pa) [12]'
      write(ifile,'(A)') trim(line)

      do i=1,max(n,maxval(n_sol_lines))
        if (i > n) then
          line = non // sep // non
        else
          write(line,'(2es19.10e3)') Ta(i), Pa(i)
        endif
        mergedlines = trim(line)
        if (i > n_sol_lines(1)) then
          line = non // sep // non
        else
          write(line,'(2es19.10e3)') Tsg(i), Psg(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(2)) then
          line = non // sep // non
        else
          write(line,'(2es19.10e3)') Tsl(i), Psl(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(3)) then
          line = non // sep // non
        else
          write(line,'(2es19.10e3)') Tl(i), Pl(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(4)) then
          line = non // sep // non
        else
          write(line,'(2es19.10e3)') Ts(i), Ps(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        if (i > n_sol_lines(5)) then
          line = non // sep // non
        else
          write(line,'(2es19.10e3)') Ts2(i), Ps2(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(line)
        write(ifile,'(A)') trim(mergedlines)
      enddo
    endif
    close(ifile)
  end subroutine solidEnvelopePlot

  !-----------------------------------------------------------------------------
  !> Get energy, entropy, and specific volume for envelope plotting
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine getESV(T,P,Z,beta,betaSol,is,e,s,v,K)
    use vls, only: mpEntropy, mpEnthalpy, mpSpecificVolume
    use numconstants, only: machine_prec
    implicit none
    ! Input:
    real,           intent(in)    :: T           !> T-guess and solution T (K)
    real,           intent(in)    :: p           !> Solution pressure (Pa)
    real,           intent(in)    :: Z(nc)       !> Total molar comp. (-)
    real,           intent(in)    :: beta        !> Gas fraction in liquid-vapour mixture
    real,           intent(in)    :: betaSol     !> Solid fraction
    integer,        intent(in)    :: is          !> Solid component index
    real, optional, intent(in)    :: K(nc)       !> Guess for K-values
    ! Output:
    real,           intent(out)   :: e           !> Internal energy (J/mol)
    real,           intent(out)   :: s           !> Specific entropy (J/mol/K)
    real,           intent(out)   :: v           !> Specific volume (m3/mol)
    ! Internal:
    real, dimension(nc) :: X, Y, Zstar, Zsolid, Kl
    real, dimension(nph) :: B
    integer, dimension(nph) :: phaseVec
    real, dimension(nph,nc) :: XX
    integer :: nd
    real :: h

    Kl = 1.0 ! X = Y = Z
    if (present(K)) then
      Kl = K
    endif
    nd = 3
    Zstar = Z
    Zstar(is) = max(machine_prec, Z(is) - betaSol)
    Zstar = Zstar/max(machine_prec, (1.0-betaSol))
    Zsolid = 0.0
    Zsolid(is) = 1.0
    X = Zstar/(1-beta+beta*Kl)
    Y = Kl*Zstar/(1-beta+beta*Kl)
    phaseVec(1) = VAPPH
    phaseVec(2) = LIQPH
    phaseVec(3) = SOLIDPH
    B(1) = beta*(1.0-betaSol)
    B(2) = (1.0-beta)*(1.0-betaSol)
    B(3) = betaSol
    XX(1,:) = Y
    XX(2,:) = X
    XX(3,:) = Zsolid

    s = mpEntropy(nd,t,p,B,XX,phaseVec)
    h = mpEnthalpy(nd,t,p,B,XX,phaseVec)
    v = mpSpecificVolume(nd,t,p,B,XX,phaseVec)
    e = h - p*v
  end subroutine getESV

  !-----------------------------------------------------------------------------
  !> Plot saturation line in TP space for VLSE
  !>
  !> \author MH, 2016-02
  !-----------------------------------------------------------------------------
  subroutine solveForZeroBeta(Z,p0,p1,p,t,Ktr,betaSolFix,is)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver,&
         NS_PEGASUS
    implicit none
    ! Input:
    real,           intent(in)    :: Z(nc)       ! Total molar comp. (-)
    real,           intent(in)    :: Ktr(nc)     ! Guess for K-values
    real,           intent(inout) :: T           ! T-guess and solution T (K)
    real,           intent(out)   :: p           ! Solution pressure (Pa)
    real,           intent(in)    :: betaSolFix  ! Fixated solid fraction (-)
    real,           intent(in)    :: p0,p1       ! Triple point pressures (Pa)
    integer,        intent(in)    :: is          ! Solid component index
    ! Internal:
    type(nonlinear_solver)      :: solver
    real, dimension(2*nc+3) :: param
    real :: betaSol, beta, ln_spec, K(nc)
    integer :: s, ierr, iter
    param(1:nc) = Z
    param(nc+1:2*nc) = Ktr
    param(2*nc+1) = t
    param(2*nc+2) = real(is)
    param(2*nc+3) = betaSol
    solver%abs_tol = 1.0e-8
    solver%max_it = 1000
    solver%isolver = NS_PEGASUS
    call bracketing_solver(p0,p1,zeroBetaFraction,p,&
         solver,param)
    s = nc+2
    ln_spec = log(p)
    beta = 1.0
    betaSol = betaSolFix
    K = Ktr
    iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,&
         VARSOLID,ierr)
  end subroutine solveForZeroBeta


  !-----------------------------------------------------------------------------
  !> Function used to solve for point on phase envelope having solid
  !> in equilibrium with fluid phase and incipient fluid phase
  !>
  !> \author MH 2016-02
  !-----------------------------------------------------------------------------
  function zeroBetaFraction(P,param) result(fun)
    implicit none
    real, intent(in) :: P
    real, dimension(2*nc+3), intent(in) :: param
    real :: fun
    ! Locals:
    real :: t, K(nc), Z(nc), beta, betaSol, betaSolFix, ln_spec, pvar
    integer :: s, iter, ierr, is
    s = nc + 2
    ln_spec = log(p)
    Z = param(1:nc)
    K = param(nc+1:2*nc)
    t = param(2*nc+1)
    is = int(param(2*nc+2))
    betaSolFix = param(2*nc+3)
    betaSol = 0.0
    beta = 1.0
    pvar = p

    iter = solidPointOnEnvelopeThreePh(Z,K,t,pvar,beta,betaSol,&
         is,s,ln_spec,VARSOLID,ierr)
    if (ierr /= 0) then
      print *,'saturation::zeroBetaFraction failed at pressure (Pa): ',p
      stop
    endif
    fun = betaSolFix - betaSol
  end function zeroBetaFraction

  !-----------------------------------------------------------------------------
  !> Calculate solid-fluid equilibrium for single component,
  !> given pressure or temperature
  !>
  !> \author MH, 2016-03
  !-----------------------------------------------------------------------------
  subroutine solidFluidEqSingleComp(Z,Y,X,t,p,specification,phase,ierr,dTdP)
    implicit none
    integer, intent(in) :: specification     ! Indicates whether T or P is fixed
    real, dimension(nc), intent(in) :: Z     ! Total composition
    real, dimension(nc), intent(out) :: Y,X  ! Composition of vapor and liquid
    real, intent(inout) :: t, p              ! Temperature [K], pressure [Pa]
    integer, intent(in) :: phase             ! Phase flag
    integer, optional, intent(out) :: ierr   ! Optional error flag
    real, optional, intent(out) :: dTdP      ! Extrapolation along line
    ! Locals
    integer, dimension(1) :: imax
    integer :: is
    real, dimension(1) :: xx,xmax,xmin
    real, dimension(5) :: param
    type(nonlinear_solver) :: solver
    real :: Tmin, Pmin, Tmax, Pmax, Jac(1,1)
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    if (specification /= specT .and. specification /= specP) then
      print *,'sol_fluid_eq_single_comp: Only possible to call with spec=1 and spec=2'
      call exit(1)
    endif

    if (present(ierr)) then
      ierr = 0
    endif

    ! Single component index
    imax = maxloc(Z)
    is = imax(1)
    X = 0.0
    X(is) = 1.0
    Y = X
    param(1) = real(specification)
    param(2) = real(is)
    param(4) = real(phase)

    if (phase == VAPPH) then
      Tmax = act_mod_ptr%comps(is)%p_comp%ttr
      Tmin = tpTmin
      Pmax = act_mod_ptr%comps(is)%p_comp%ptr
      Pmin = tpPmin
    else
      Tmax = tpTmax
      Tmin = act_mod_ptr%comps(is)%p_comp%ttr
      Pmax = tpPmax
      Pmin = act_mod_ptr%comps(is)%p_comp%ptr
    endif

    if (specification == specP) then
      param(3) = p
      xmax = log(Tmax)
      xmin = log(Tmin)
      if (p > Pmax .OR. p < Pmin) then
        print *,'Pressure:',p
        print *,'Minimum pressure:',Pmin
        call stoperror('sol_fluid_eq_single_comp: Pressure specification out of range')
      endif
      if (t > Tmax .OR. t < Tmin) then
       if (phase == VAPPH) then
         t = 0.95*Tmax
       else
         t = 1.05*Tmin
       endif
      endif
      XX(1) = log(t)
    else
      param(3) = t
      xmax = log(Pmax)
      xmin = log(Pmin)
      if (t > Tmax .OR. t < Tmin) then
        call stoperror('sol_fluid_eq_single_comp: Temperature specification out of range')
      endif
      if (p > Pmax .OR. p < Pmin) then
       if (phase == VAPPH) then
         p = 0.95*Pmax
       else
         p = 1.05*Pmin
       endif
      endif
      X(1) = log(p)
    endif

    ! Attempt to solve for single-comp saturation point
    solver%max_it = 200
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-12
    solver%limit_x_values = .true.
    solver%ls_max_it = 3
    call nonlinear_solve(solver,sol_fluid_fun_single,sol_fluid_jac_single,&
         sol_fluid_jac_single,limit_dx,premReturn,setXv,XX,xmin,xmax,param)
    if (specification == specP) then
      T = exp(XX(1))
    else
      P = exp(XX(1))
    endif
    if (present(dTdP)) then
      call sol_fluid_jac_single(Jac,XX,param)
      dTdP = param(5)
    endif
    if (solver%exitflag /= 0) then
      ! Something went wrong.
      if (present(ierr)) then
        ierr = solver%exitflag
      else
        ! Failed...
        if (specification == specP) then
          print *,'Specified pressure (bar): ',p/1.0e5
        else
          print *,'Specified temperature (K): ',T
        endif
        write(*,*) "Computed value for T/K or P/Pa", exp(XX(1))
        write(*,*) "ttr, ptr=", act_mod_ptr%comps(is)%p_comp%ttr, &
             act_mod_ptr%comps(is)%p_comp%ptr
        write(*,*) "Exit flag: ", solver%exitflag
        write(*,*) "Error on exit: ", solver%error_on_exit
        call stoperror('sol_fluid_eq_single_comp::sat did not converge')
      endif
    endif
  end subroutine solidFluidEqSingleComp

  !-----------------------------------------------------------------------------
  !> Single component solid-fluid eqiulibrium error
  !>
  !> \author MH, 2016-03
  !-----------------------------------------------------------------------------
  subroutine sol_fluid_fun_single(F,X,param)
    use solideos, only: solid_thermo
    use eos, only: thermo
    implicit none
    real, dimension(1), intent(in) :: X
    real, dimension(1), intent(out) :: F
    real, dimension(5), intent(inout) :: param
    ! Locals
    integer :: spec, is, phase
    real :: p, t, Z(nc), lnfug(nc), lnfugs
    spec = int(param(1))
    is = int(param(2))
    phase = int(param(4))
    if (spec == specP) then
      p = param(3)
      t = exp(X(1))
    else
      p = exp(X(1))
      t = param(3)
    endif
    Z = 0.0
    Z(is) = 1.0
    call thermo(t,p,Z,phase,lnfug)
    call solid_thermo(t,p,Z,lnfugs)
    f = lnfug(is)-lnfugs
  end subroutine sol_fluid_fun_single

  !-----------------------------------------------------------------------------
  !> Single component solid-fluid equilibrium differential
  !>
  !> \author MH, 2016-03
  !-----------------------------------------------------------------------------
  subroutine sol_fluid_jac_single(Jac,X,param)
    use solideos, only: solid_thermo
    use eos, only: thermo
    implicit none
    real, dimension(1), intent(in) :: X
    real, dimension(1,1), intent(out) :: Jac
    real, dimension(5), intent(inout) :: param
    ! Locals
    integer :: spec, is, phase
    real :: p, t, lnfugs, dlnfugsdt, dlnfugsdp
    real, dimension(nc) :: Z(nc), lnfug(nc), dlnfugdt(nc), dlnfugdp(nc)
    spec = int(param(1))
    is = int(param(2))
    phase = int(param(4))
    if (spec == specP) then
      p = param(3)
      t = exp(X(1))
    else
      p = exp(X(1))
      t = param(3)
    endif
    Z = 0.0
    Z(is) = 1.0
    call thermo(t,p,Z,phase,lnfug,dlnfugdt,dlnfugdp)
    call solid_thermo(t,p,Z,lnfugs,dlnfugsdt,dlnfugsdp)
    if (spec == specP) then
      Jac(1,1) = T*(dlnfugdt(is)-dlnfugsdt)
    else
      Jac(1,1) = P*(dlnfugdp(is)-dlnfugsdp)
    endif
    ! Extrapolation of solution
    param(5) = -(dlnfugdp(is)-dlnfugsdp)/(dlnfugdt(is)-dlnfugsdt)
  end subroutine sol_fluid_jac_single

  !-------------------------------------------------------------------------
  !> Three-phase line for two component mixture.
  !! Differentials along line:
  !!  -pressure differentials at constant solid fraction
  !!  -solid fraction differentials at constant pressure
  !!
  !! \author MH, 2016
  !-------------------------------------------------------------------------
  subroutine tripleLineDiff(Z,K,t,p,beta,is,&
       dbetadlnp,dlntdlnp,dngdlnp,dbdbs,dngdbs,dnldbs)
    use eos, only: enthalpy, specificVolume
    use solideos, only: solid_enthalpy, solid_specificVolume
    implicit none
    real, intent(in) :: t, p
    real, intent(in) :: beta
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(out) :: dngdlnp,dngdbs,dnldbs !< Function values
    real, intent(out) :: dlntdlnp !< Temperature differential wrpt. pressure
    real, intent(out) :: dbetadlnp !< Vapor phase fraction differential wrpt. pressure
    real, intent(out) :: dbdbs !< Vapor phase fraction differential wrpt. solid fraction
    integer, intent(in) :: is !< Solid component index
    real, dimension(nc), intent(in) :: K !< Variable vector
    ! Locals
    real, dimension(nc+3) :: dXds !< Pressure differentials
    real:: ln_spec, dln_s
    real :: bs, b, tl, pl
    real, dimension(nc) :: Y, X, dlnKdlnp, dxdlnK, dxdbeta
    real, dimension(nc) :: dydlnK, dydbeta, Kl
    integer :: s
    !
    s = nc+2
    dXds = 0.0
    dXds(s) = 1.0
    bs = 0.0
    Kl = K
    ln_spec = log(p)
    dln_s = 0.0
    b = beta
    tl = t
    pl = p
    call newton_extrapolate_threePh(Z,Kl,tl,pl,b,bs,s,is,ln_spec,&
         dln_s,VARFLUID,dXds)
    dlnKdlnp = dXds(1:nc)
    dbetadlnp = dXds(nc+3)
    X = Z/(1-beta+beta*K)
    Y = K*X
    dxdlnK = -Y*beta/(1-beta+beta*K)
    dxdbeta = -X*(K-1)/(1-beta+beta*K)
    dydlnK = -K*Y*beta/(1-beta+beta*K) + Y
    dydbeta = K*dxdbeta
    dngdlnp = dbetadlnp*Y + beta*dydlnK*dlnKdlnp &
         + beta*dydbeta*dbetadlnp
    !
    ! beta = (Z(is) - X(is) + betaSol*(X(is) - 1.0))/(Y(is)-X(is))
    dbdbs = (X(is)-1.0)/(Y(is)-X(is))
    dngdbs = Y*dbdbs
    !betaL = 1 - b - bs
    dnldbs = X*(-dbdbs - 1.0)
    dlntdlnp = dXds(nc+1)
  end subroutine tripleLineDiff

  !> Calculate point where gas-liquid saturation line meats
  !! triple line/area.
  subroutine tripleAreaEdge(Z,Ttr,Ptr,is,lower,ierr,Ktr)
    use thermo_utils, only: isSingleComp
    use tp_solver, only: twoPhaseTPflash
    use thermopack_constants, only: TWOPH
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(out) :: Ttr !< Temperature
    real, intent(out) :: Ptr !< Pressure
    integer, intent(in) :: is !< Solid component index
    logical, intent(in) :: lower !< lower=.true. calculates lower pressure endpoint
    integer, intent(out) :: ierr !< Error flag
    real, dimension(nc), optional, intent(out) :: Ktr !< Equilibrium constants
    ! Locals
    real, dimension(nc) :: X, Y, lnfugV, lnfugL, K
    real :: betaGas, betaSol, betaLiq
    integer :: iter, phase
    type(thermo_model), pointer :: act_mod_ptr
    ! Locale triple points
    act_mod_ptr => get_active_thermo_model()
    Ttr = act_mod_ptr%comps(is)%p_comp%ttr
    if (isSingleComp(Z)) then
      if (Z(is) < 0.5) then
        ierr = 1
      else
        if (present(Ktr)) then
          Ktr = 1.0
        endif
        ierr = 0
        Ptr = act_mod_ptr%comps(is)%p_comp%Ptr
      endif
      return
    endif
    if (lower) then
      Y = Z
      Ptr = safe_dewP(Ttr,X,Y,ierr)
      betaGas = 1.0
    else ! upper
      X = Z
      Ptr = safe_bubP(Ttr,X,Y,ierr)
      betaGas = 0.0
      if (ierr /= 0) then
        Ptr = 200.0e5
        call twoPhaseTPflash(Ttr,Ptr,Z,betaGas,betaLiq,phase,X,Y)
        if (phase == TWOPH) then
          ierr = 0
        endif
      else
        betaGas = 0.0
      endif
    endif
    if (ierr == 0) then
      call thermo(Ttr,Ptr,Y,VAPPH,lnfugV)
      call thermo(Ttr,Ptr,X,LIQPH,lnfugL)
      K = exp(lnfugL-lnfugV)
      betaSol = 0.0
      iter = solidPointOnEnvelopeThreePh(Z,K,Ttr,Ptr,betaGas,&
           betaSol,is,0,0.0,TRIPLE,ierr)
      if (present(Ktr)) then
        Ktr = K
      endif
    else
      Ptr = 0.0
      Ttr = 0.0
      if (present(Ktr)) then
        Ktr = 0.0
      endif
    endif
  end subroutine tripleAreaEdge

  !-------------------------------------------------------------------------
  !> Get triple line temperature given pressure
  !>
  !> \author MH, 2016-03
  !-------------------------------------------------------------------------
  subroutine threePhaseLineTemperature(t,pspec,Z,X,Y,beta,is,isConverged)
    use thermopack_constants, only: VAPPH, LIQPH
    use numconstants, only: machine_prec
    use eos, only: thermo
    implicit none
    real, intent(out) :: beta !< Phase molar fractions [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(out) :: X, Y !< Molar compozition [-]
    real, intent(out) :: t !< Temperature [K]
    real, intent(in) :: pspec !< Pressure [Pa]
    integer, intent(in) :: is !< Solid component
    logical, intent(out) :: isConverged
    ! Locals
    real :: p, ln_spec, betaSol, p_last, dln_s, ln_p_spec
    real :: Ptr, Ttr, Ptr2, Ttr2
    real, dimension(nc) :: K
    integer :: s, ierr, iter, i
    !
    if (is < 1 .or. is > nc) then
      call stoperror('threePhaseLineTemperature: Solid component index is out of bounds')
    endif
    isConverged = .false.
    ! Locate triple points
    call tripleAreaEdge(Z,Ttr,Ptr,is,.true.,ierr,K)
    if (pspec >= Ptr) then
      call tripleAreaEdge(Z,Ttr2,Ptr2,is,.false.,ierr)
      if (ierr /= 0) then
        Ptr2 = 1.0e8
      endif
      if (pspec <= Ptr2) then
        ! Solve for point on line
        ln_spec = log(pspec)
        ln_p_spec = ln_spec
        s = nc+2
        T = Ttr
        p_last = Ptr
        beta = 1.0
        dln_s = ln_spec - log(p_last)
        p = p_last
        betaSol = 0.0
        i = 1
        ! Step along line to find solution
        do while (abs(dln_s) > machine_prec)
          call newton_extrapolate_threePh(Z,K,t,p,beta,betaSol,s,is,ln_spec,&
               dln_s,VARFLUID,dln_s_max=0.15)
          ln_spec = log(p)
          iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,&
               ln_spec,VARFLUID,ierr)
          if (ierr /= 0) then
            call stoperror('solidPointOnEnvelopeThreePh failed in tripleLineTemperature')
          endif
          dln_s = ln_p_spec - ln_spec
          i = i + 1
          if (i > 200) then
            call stoperror('Too many iterations in tripleLineTemperature')
          endif
        end do
        ! Mass balance give composition:
        X = Z/(1.0-beta+beta*K)
        Y = K*X
        isConverged = .true.
      endif
    endif

  end subroutine threePhaseLineTemperature

  !-------------------------------------------------------------------------
  !> Test for critical phase in equilibrim with solid phase
  !>
  !> \author MH, 2020-06
  !-------------------------------------------------------------------------
  function isCriticalThreePhase(Z,K,t,p,beta,betaSol,is,tcrit,pcrit,ierr) result(isCrit)
    use thermopack_constants, only: LIQPH, VAPPH, MINGIBBSPH
    use thermopack_var, only: nc
    use eos!, only: specificvolume
    use critical!, only: calcCriticalTV
    implicit none
    real, intent(out) :: t !< Temperature [K]
    real, intent(out) :: p !< Pressure [Pa]
    real, intent(in) :: beta !< Gas mole fraction in gas-liquid [-]
    real, intent(inout) :: betaSol !< Solid phase fractions [-]
    real, dimension(nc), intent(inout) :: K !< Gas liquid equilibrium constants [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, intent(out) :: tcrit !< Temperature [K]
    real, intent(out) :: pcrit !< Pressure [Pa]
    integer, intent(in) :: is !< Solid component
    integer, intent(out) :: ierr
    logical :: isCrit
    ! Locals
    real :: K1max, betaSolCrit

    isCrit = .false.
    K1max = maxval(abs(K-1))
    if (K1max < 5.0e-4) then
      ! Look for critical point
      isCrit = .true.
      ! MH: Should distinguish between critical points and azeotropes
      tcrit = t
      pcrit = p
      betaSolCrit = betaSol
      call threePhaseCriticalPoint(tcrit,pcrit,Z,betaSolCrit,is,ierr)
      if (ierr /= 0) then
        t = tcrit
        p = pcrit
        betaSol = betaSolCrit
        K = 1.0
      else
        tcrit = -1.0
        pcrit = -1.0
        betaSolCrit = -1.0
      endif
    endif
  end function isCriticalThreePhase

  !-------------------------------------------------------------------------
  !> Get critical point ending triple line/area
  !>
  !> \author MH, 2017-12
  !-------------------------------------------------------------------------
  subroutine threePhaseCriticalPoint(tcrit,pcrit,Z,betaSol,is,ierr)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver,&
         NS_PEGASUS
    use thermopack_var, only: nc
    use eosTV, only: pressure
    use critical!, only: calcCriticalTV
    implicit none
    real, intent(inout) :: betaSol !< Solid phase fractions [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, intent(inout) :: tcrit !< Temperature [K]
    real, intent(inout) :: pcrit !< Pressure [Pa]
    integer, intent(in) :: is !< Solid component
    integer, intent(out) :: ierr
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(nc+3) :: param
    real :: localZstar(nc), dbeta
    real :: betaSolMin, betaSolMax, funMin, funMax, vcrit
    integer :: i
    integer, parameter :: nbeta = 25
    !
    if (is < 1 .or. is > nc) then
      call stoperror('threePhaseCriticalPoint: Solid component index is out of bounds')
    endif
    ierr = 0

    ! Generate initial volume
    localZstar = Z
    localZstar(is) = Z(is) - betaSol
    ! Normalize
    localZstar = localZstar/(1.0-betaSol)
    !call specificVolume(tcrit,pcrit,localZstar,ph,vcrit)
    !print *,vcrit
    ! Set param vector
    param(1:nc) = Z
    !param(nc+1) = tcrit
    !param(nc+2) = vcrit
    param(nc+3) = real(is)

    ! Get search range
    betaSolMin = max(0.0,betaSol - 0.05)
    funMin = criticalError(betaSolMin,param)
    dbeta = (Z(is)*(1.0-1.0e-7) - betaSol)/(nbeta-1)
    do i=1,nbeta
      betaSolMax = betaSol + dbeta*real(i-1)
      funMax = criticalError(betaSolMax,param)
      if (funMin*funMax < 0.0) exit
    enddo

    if (funMin*funMax > 0.0) then
      ! Not able to locate critical point
      ierr = 1
      return
    endif

    ! Locate critical point
    solver%abs_tol = 1.0e-6
    solver%max_it = 1000
    solver%isolver = NS_PEGASUS
    call bracketing_solver(betaSolMin,betaSolMax,criticalError,betaSol,&
         solver,param)

    ! Check for successful convergence
    if (solver%exitflag /= 0) then
      if (verbose) then
        print *, "Bracketing solver for critical point"
        print *, "ending triple line/area failed."
      endif
      ierr = solver%exitflag
      return
    endif

    ! Get results
    localZstar = Z
    localZstar(is) = Z(is) - betaSol
    ! Normalize
    localZstar = localZstar/(1.0-betaSol)
    tcrit = -1.0
    vcrit = -1.0
    call calcCriticalTV(tcrit,vcrit,localZstar,ierr,1.0e-8)
    !tcrit = param(nc+1)
    !vcrit = param(nc+2)
    pcrit = pressure(tcrit,vcrit,localZstar)
  end subroutine threePhaseCriticalPoint

  !-----------------------------------------------------------------------------
  !> Function used to solve for critical point ending three phase line/area
  !>
  !> \author MH 2017-12
  !-----------------------------------------------------------------------------
  function criticalError(betaSol,param) result(fun)
    use critical, only: calcCriticalTV
    use eosTV, only: pressure
    use solideos, only: solid_thermo
    use eos, only: thermo
    implicit none
    real, intent(in) :: betaSol
    real, dimension(nc+3), intent(inout) :: param
    real :: fun
    ! Locals:
    real :: tcrit, vcrit, pcrit, lnfug(nc), lnfugs
    real :: localZstar(nc), Zs(nc), Z(nc)
    integer :: ierr, is
    Z = param(1:nc)
    vcrit = param(nc+1)
    tcrit = param(nc+2)
    is = int(param(nc+3))
    localZstar = Z
    localZstar(is) = Z(is) - betaSol
    ! Normalize
    localZstar = localZstar/(1.0-betaSol)
    tcrit = -1.0
    vcrit = -1.0
    call calcCriticalTV(tcrit,vcrit,localZstar,ierr,1.0e-8)
    if (debug) then
      print *,"calcCriticalTV ierr",ierr
    endif
    param(nc+1) = vcrit
    param(nc+2) = tcrit
    pcrit = pressure(tcrit,vcrit,localZstar)
    call thermo(tcrit,pcrit,localZstar,LIQPH,lnfug,v=vcrit)
    Zs = 0.0
    Zs(is) = 1.0
    call solid_thermo(tcrit,pcrit,Zs,lnfugs)
    fun = lnfugs-lnfug(is)-log(localZstar(is))
  end function criticalError

  !-------------------------------------------------------------------------
  !> Get critical point ending triple line/area
  !> Used for debugging
  !> \author MH, 2017-12
  !-------------------------------------------------------------------------
  subroutine printCriticalRange(Z,betaSolMin,betaSolMax,is)
    use thermopack_var, only: nc
    use eosTV, only: pressure
    use critical, only: calcCriticalTV
    implicit none
    real, intent(in) :: betaSolMin, betaSolMax !< Solid phase fractions [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    integer, intent(in) :: is !< Solid component
    ! Locals
    real :: localZstar(nc)
    real :: vcrit, betaSol
    real :: tcrit !< Temperature [K]
    real :: pcrit !< Pressure [Pa]
    integer :: i, n, ierr
    !
    if (is < 1 .or. is > nc) then
      call stoperror('printCriticalRange: Solid component index is out of bounds')
    endif

    n = 1000
    do i=1,n
      betaSol = betaSolMin + (betaSolMax-betaSolMin)*real(i-1)/real(n-1)
      ! Generate initial volume
      localZstar = Z
      localZstar(is) = Z(is) - betaSol
      ! Normalize
      localZstar = localZstar/(1.0-betaSol)
      tcrit = -1.0
      vcrit = -1.0
      call calcCriticalTV(tcrit,vcrit,localZstar,ierr,1.0e-8)
      pcrit = pressure(tcrit,vcrit,localZstar)
      print *,ierr,tcrit,pcrit
    enddo
  end subroutine printCriticalRange

  !-----------------------------------------------------------------------------
  !> Plot saturation line in TP space for VLSE of single component
  !>
  !> \author MH, 2022-11
  !-----------------------------------------------------------------------------
  subroutine solidEnvelopePlotSingle(Z,T_init,p_init,spec,Pmax,nmax,TaSat,PaSat,TaSubl,PaSubl,TaFus,PaFus,ierr)
    use solideos, only: nSolid, solidComp
    use utilities, only: newunit
    use thermo_utils, only: isSingleComp
    use thermopack_var, only: nce
    implicit none
    ! Input:
    real,              intent(in)  :: Z(nc)       ! Total molar comp. (-)
    real,              intent(in)  :: T_init      ! T-guess initial point (K)
    real,              intent(in)  :: p_init      ! p-guess initial point (Pa)
    integer,           intent(in)  :: spec        ! 1: Specify P, T Varies
                                                  ! 2: Specify T: P varies
    real,              intent(in)  :: Pmax        ! Maximum pressure (Pa)
    integer,           intent(in)  :: nmax          ! Maximum number of points
    real,              intent(out) :: TaSat(nmax)    ! Sat. temp. values (K)
    real,              intent(out) :: PaSat(nmax)    ! Sat. pres. values (Pa)
    real,              intent(out) :: TaSubl(nmax)   ! Sublimation temp. values (K)
    real,              intent(out) :: PaSubl(nmax)   ! Sublimation pres. values (Pa)
    real,              intent(out) :: TaFus(nmax)    ! Fusion temp. values (K)
    real,              intent(out) :: PaFus(nmax)    ! Fusion pres. values (Pa)
    integer,           intent(out) :: ierr           ! Error flag
    ! Internal:
    real :: T_tr, P_tr, T, P, dTdP, dT, dP, P_crit
    real :: X(nce), Y(nce)
    integer :: is, n, i
    logical :: found
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    found = .false.
    is = maxloc(Z, dim=1)
    do i=1,nSolid
      if (is == solidComp(i)) found = .true.
    enddo

    ! Triple point
    T_tr = act_mod_ptr%comps(is)%p_comp%ttr
    P_tr = act_mod_ptr%comps(is)%p_comp%ptr
    P_crit = act_mod_ptr%comps(is)%p_comp%pc

    ! Map saturation curve
    T = T_tr
    P = P_tr
    call singleCompSaturation(Z,T,P,spec,TaSat,PaSat,nmax,n,maxDeltaP=1.0)

    ! Map sublimation curve
    TaSubl(1) = T_tr
    PaSubl(1) = P_tr
    call solidFluidEqSingleComp(Z,Y,X,TaSubl(1),PaSubl(1),spec,VAPPH,ierr=ierr,dTdP=dTdP)
    if (spec == specP) then
      dP = (P_tr - min(P_init, 0.95*P_tr))/(nmax+1)
    else
      dT = (T_tr - min(T_init, 0.95*T_tr))/(nmax+1)
    endif
    do i=2,nmax
      if (spec == specP) then
        TaSubl(i) = TaSubl(i-1) - dP*dTdP
        PaSubl(i) = PaSubl(i-1) - dP
      else
        TaSubl(i) = TaSubl(i-1) - dT
        PaSubl(i) = PaSubl(i-1) - dT/dTdP
      endif
      call solidFluidEqSingleComp(Z,Y,X,TaSubl(i),PaSubl(i),spec,VAPPH,ierr=ierr,dTdP=dTdP)
    enddo

    ! Map fusion curve
    TaFus(1) = T_tr
    PaFus(1) = P_tr
    call solidFluidEqSingleComp(Z,Y,X,TaFus(1),PaFus(1),specP,LIQPH,ierr=ierr,dTdP=dTdP)
    dP = (max(Pmax, P_crit) - P_tr)/(nmax+1)
    do i=2,nmax
      TaFus(i) = TaFus(i-1) + dP*dTdP
      PaFus(i) = PaFus(i-1) + dP
      call solidFluidEqSingleComp(Z,Y,X,TaFus(i),PaFus(i),specP,LIQPH,ierr=ierr,dTdP=dTdP)
    enddo
  end subroutine solidEnvelopePlotSingle


end module solid_saturation
