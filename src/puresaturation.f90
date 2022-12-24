module puresaturation
  use nonlinear_solvers
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model, &
       base_eos_param, get_active_alt_eos, tpTmin
!  use utilities, only: get_thread_index
  implicit none
  private
  save

  public :: PureSat, PureSatLine

contains

  !-----------------------------------------------------------------------------
  !> Calculate saturation temperature (or pressure) as if the
  !! mixture was a single component (i.e. both phases are assumed to have
  !! composition Z). Intended for initial values in bubT, dewT etc.
  !!
  !! \author MH, 2013-10-17
  !-----------------------------------------------------------------------------
  subroutine PureSat(T,P,Z,solveForT,ierr,meta,pseudo_crit_TP)
    use eos, only: pseudo_safe
    use single_phase, only: TP_CalcPseudo
    use numconstants, only: Small
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, intent(inout) :: P, T
    logical, intent(in) :: solveForT
    integer, optional, intent(out) :: ierr
    logical, optional, intent(in) :: meta !< Use meta-stable minimum when solving for sat. point
    real, optional, intent(in) :: pseudo_crit_TP(2) !< Override pseudo critical point
    ! Locals
    real :: tpc,ppc,zpc,vpc
    real, dimension(nc+4) :: param
    real, dimension(1) :: X, Xmin, Xmax, dFdX
    type(nonlinear_solver) :: solver
    logical :: alt_eos
    type(thermo_model), pointer :: act_mod_ptr

    if (present(ierr)) ierr = 0

    act_mod_ptr => get_active_thermo_model()
    alt_eos = act_mod_ptr%need_alternative_eos

    ! Start with establishment of pseudocritical properties
    if (present(pseudo_crit_TP)) then
      ! Use user given pseudo critical point
      Ppc = pseudo_crit_TP(2)
      Tpc = pseudo_crit_TP(1)
    else
      call pseudo_safe(z,tpc,ppc,zpc,vpc)
    endif

    if (solveForT) then
      if (P > Ppc) then
        if (solver%verbose) print *,'saturation::PureSat: P > estimated pseudo critical pressure', Ppc
        T = Tpc
        return
      endif
    else ! Solve for pressure
      if (T > Tpc) then
        if (solver%verbose) print *,'saturation::PureSat: T > estimated pseudo critical temperature', Tpc
        P = Ppc
        return
      endif
    endif

    ! Solve for pure component saturation line
    ! Initiate from below the pseudo-critical point
    param(1:nc) = z
    if (alt_eos) then
      param(nc+3) = 1.0
    else
      param(nc+3) = 0.0
    endif
    param(nc+4) = 1.0
    if (present(meta)) then
      if (.not. meta) then
        param(nc+4) = 0.0
      endif
    endif
    if (solveForT) then
      param(nc+2) = 1.0
      param(nc+1) = p
      X(1) = Tpc
      Xmin = min(tpTmin,20.0)
      Xmax = Tpc
    else
      param(nc+2) = 0.0
      param(nc+1) = T
      Xmin = 1.0
      X(1) = max(0.1*Ppc*T/Tpc,Xmin(1))
      call PureSatDiff(dFdX,X,param)
      if (abs(dFdX(1)) < Small) then ! Same root for both phases
        X(1) = Xmin(1)+1.0e-12
        call PureSatDiff(dFdX,X,param)
        if (abs(dFdX(1)) < Small) then ! Still same root for both phases
          X(1) = Ppc*T/Tpc
        endif
      endif
      Xmax = min(1.5*Ppc*T/Tpc,Ppc)
    endif

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-4 ! No need to solve very accurately
    solver%max_it = 30
    solver%ls_max_it = 3

    call nonlinear_solve(solver,PureSatFun,PureSatDiff,&
         PureSatDiff,limit_dx,premReturn,setXv,X,Xmin,Xmax,param)
    if (solver%exitflag /= 0) then
      ! Can be too noisy .. use solver%verbose (GS)
      if (solver%verbose) print *, "solver%exitflag:",solver%exitflag
      if (present(ierr)) then
        ierr = 1
      else
        call stoperror('saturation::PureSat did not converge')
      end if
    endif
    if (solveForT) then
      T = X(1)
    else
      P = X(1)
    endif
  end subroutine PureSat

  subroutine PureSatFun(F,X,param)
    use utilities, only: boolean
    use single_phase, only: TP_CalcGibbs
    use eos, only: residualGibbs
    use thermopack_constants, only: VAPPH, LIQPH
    implicit none
    real, dimension(1), intent(in) :: X
    real, dimension(1), intent(out) :: F
    real, dimension(nc+4), intent(in) :: param
    ! Locals
    logical :: tempIsVar, alt_eos, meta
    real :: P, T, gL, gG
    real, dimension(nc) :: Z
    integer :: gflag_opt
    type(thermo_model), pointer :: act_mod_ptr
    class(base_eos_param), pointer :: act_eos_ptr

    alt_eos = boolean(param(nc+3))
    tempIsVar = boolean(param(nc+2))
    if (tempIsVar) then
      T = X(1)
      P = param(nc+1)
    else
      P = X(1)
      T = param(nc+1)
    endif
    Z=param(1:nc)
    if (nint(param(nc+4))==1) then
      meta = .true.
      gflag_opt = 2
    else
      meta = .false.
      gflag_opt = 1
    endif

    if (alt_eos) then
      act_mod_ptr => get_active_thermo_model()
      act_eos_ptr => get_active_alt_eos()
      !
      call TP_CalcGibbs(nc,act_mod_ptr%comps,act_eos_ptr,&
           T,P,Z,VAPPH,residual=.true.,g=gG,gflag_opt=gflag_opt)
      call TP_CalcGibbs(nc,act_mod_ptr%comps,act_eos_ptr,&
           T,P,Z,LIQPH,residual=.true.,g=gL,gflag_opt=gflag_opt)
    else
      call residualGibbs(t,p,z,VAPPH,gG,metaExtremum=meta)
      call residualGibbs(t,p,z,LIQPH,gL,metaExtremum=meta)
    endif
    F = gG - gL
  end subroutine PureSatFun

  subroutine PureSatDiff(dFdX,X,param)
    use utilities, only: boolean
    use single_phase, only: TP_CalcGibbs
    use eos, only: residualGibbs
    use thermopack_constants, only: VAPPH, LIQPH
    implicit none
    real, dimension(1), intent(in) :: X
    real, dimension(1), intent(out) :: dFdX
    real, dimension(nc+4), intent(in) :: param
    ! Locals
    logical :: tempIsVar, alt_eos, meta
    real :: P, T, gG, gL, dgL, dgG, dgrdt_G, dgrdt_L, dgrdp_G, dgrdp_L
    real, dimension(nc) :: Z
    integer :: gflag_opt
    type(thermo_model), pointer :: act_mod_ptr
    class(base_eos_param), pointer :: act_eos_ptr

    alt_eos = boolean(param(nc+3))
    tempIsVar = boolean(param(nc+2))
    Z=param(1:nc)
    if (tempIsVar) then
      T = X(1)
      P = param(nc+1)
    else
      P = X(1)
      T = param(nc+1)
    endif
    if (nint(param(nc+4))==1) then
      meta = .true.
      gflag_opt = 2
    else
      meta = .false.
      gflag_opt = 1
    endif

    if (alt_eos) then
      act_mod_ptr => get_active_thermo_model()
      act_eos_ptr => get_active_alt_eos()
      !
      call TP_CalcGibbs(nc,act_mod_ptr%comps,act_eos_ptr,&
           T,P,Z,VAPPH,residual=.true.,g=gG,dgdt=dgrdt_G,dgdp=dgrdp_G,gflag_opt=gflag_opt)
      call TP_CalcGibbs(nc,act_mod_ptr%comps,act_eos_ptr,&
           T,P,Z,LIQPH,residual=.true.,g=gL,dgdt=dgrdt_L,dgdp=dgrdp_L,gflag_opt=gflag_opt)
    else
      call residualGibbs(t,p,z,VAPPH,gG,dgrdt_G,dgrdp_G,metaExtremum=meta)
      call residualGibbs(t,p,z,LIQPH,gL,dgrdt_L,dgrdp_L,metaExtremum=meta)
    endif

    if (tempIsVar) then
      dgG = dgrdt_G
      dgL = dgrdt_L
    else
      dgG = dgrdp_G
      dgL = dgrdp_L
    endif

    dFdX = dgG - dgL
  end subroutine PureSatDiff

  function PureSatExtrapol(t,p,Z) result(dpdt)
    use eos, only: residualGibbs
    use thermopack_constants, only: VAPPH, LIQPH
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: P, T
    real :: dpdt
    ! Locals
    real :: gL, gG, dgLdT, dgGdT, dgLdP, dgGdP
    real :: dFdP, dFdT
    !
    call residualGibbs(t,p,z,VAPPH,gG,dgrdT=dgGdT,dgrdp=dgGdP)
    call residualGibbs(t,p,z,LIQPH,gL,dgrdT=dgLdT,dgrdp=dgLdP)
    dFdT = dgGdT - dgLdT
    dFdP = dgGdP - dgLdP
    dpdT=-dFdT/dFdP
  end function PureSatExtrapol

  !-----------------------------------------------------------------------------
  !> Calculate saturation line as if the
  !! mixture was a single component (i.e. both phases are assumed to have
  !! composition Z).
  !! TODO - Add proper solver for pseudo-critical point
  !! \author MH, 2018-10
  !-----------------------------------------------------------------------------
  subroutine PureSatLine(P_start,Z,Ta,Pa,na,ierr)
    use eos, only: pseudo_safe
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: P_start
    real, intent(out) :: Pa(na), Ta(na)
    integer, intent(in) :: na
    integer, optional, intent(out) :: ierr
    ! Locals
    real :: tpc,ppc,zpc,vpc,pseudo_crit_TP(2)
    real :: dP, dpdT
    integer :: i
    call pseudo_safe(Z,tpc,ppc,zpc,vpc)
    pseudo_crit_TP(1) = tpc
    pseudo_crit_TP(2) = ppc
    Pa(na) = ppc
    Ta(na) = Tpc
    Ta(1) = Tpc
    dP = (ppc-P_start)/(na-1)
    do i=1,na-1
      Pa(i) = P_start + dP*(i-1)
      !print *,Ta(i),Pa(i)
      call PureSat(Ta(i),Pa(i),Z,.true.,ierr=ierr,meta=.true.,&
           pseudo_crit_TP=pseudo_crit_TP)
      dpdt = PureSatExtrapol(Ta(i),Pa(i),Z)
      Ta(i+1) = Ta(i) + dP/dpdt
    enddo
  end subroutine PureSatLine
end module puresaturation
