module saturation
  use eos, only: thermo
  use thermopack_constants, only: clen, LIQPH, VAPPH, verbose
  use thermopack_var, only: nc, thermo_model, get_active_eos, &
       Rgas, tptmin, tppmin, tppmax, tpTmax
  use nonlinear_solvers
  use numconstants, only: machine_prec
  use puresaturation, only: puresat
  use saturation_point_locators, only: locate_sat_prop, locate_from_pressure, locate_from_temperature
  implicit none
  private
  save

  real, parameter :: sat_rel_tol = 1.0e-7
  integer, parameter :: sat_max_iter = 500
  integer, parameter :: sat_max_nr_line_s = 10
  real, parameter :: sat_limitEps = 5.0e3*machine_prec
  integer, parameter :: specP = 1
  integer, parameter :: specT = 2
  logical, parameter :: testK = .false.

  public :: specT, specP, ispec
  public :: sat_rel_tol, sat_max_iter, sat_limitEps, sat_max_nr_line_s
  public :: bubT,bubP,dewT,dewP
  public :: sat_newton, sat_fun_newton, sat_diff_newton
  public :: sat_wilsonK, sat_successive
  public :: safe_bubT,safe_bubP,safe_dewT,safe_dewP
  public :: sat_fun_single_if,sat_diff_single,sat_fun_single
  public :: acentricFactorEos

contains

  !-----------------------------------------------------------------------------
  !> Calculate bubble point temperature given pressure
  !> Use PureSat to calculate initial value
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function safe_bubT(P,X,Y,ierr) result(Tbub)
    implicit none
    real, dimension(nc), intent(in) :: X
    real, dimension(nc), intent(out) :: Y
    real, intent(in) :: P
    integer, optional, intent(out) :: ierr
    real :: Tbub !< K
    !
    real :: Pcpy
    Pcpy = P
    ! Initial temperature
    call PureSat(Tbub,Pcpy,X,.true.,ierr=ierr)

    ! Find real bubble point temperature
    Tbub = bubT(Tbub,Pcpy,X,Y,ierr)
  end function safe_bubT

  !-----------------------------------------------------------------------------
  !> Calculate bubble point pressure given temperature
  !> Use PureSat to calculate initial value
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function safe_bubP(T,X,Y,ierr) result(Pbub)
    implicit none
    real, dimension(nc), intent(in) :: X
    real, dimension(nc), intent(out) :: Y
    real, intent(in) :: T
    integer, optional, intent(out) :: ierr
    real :: Pbub !< Pa
    !
    real :: Tcpy
    Tcpy = T
    ! Initial pressure
    call PureSat(Tcpy,Pbub,X,.false.,ierr=ierr)

    ! Find real bubble point temperature
    Pbub = bubP(Tcpy,Pbub,X,Y,ierr)
  end function safe_bubP

  !-----------------------------------------------------------------------------
  !> Calculate dew point temperature given pressure
  !> Use PureSat to calculate initial value
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function safe_dewT(P,X,Y,ierr) result(Tdew)
    use thermo_utils, only: maxComp, isSingleComp
    use eos, only: getCriticalParam, specificVolume
    use eosTV, only: entropy_tv
    implicit none
    real, dimension(nc), intent(out) :: X
    real, dimension(nc), intent(in) :: Y
    real, intent(in) :: P
    integer, optional, intent(out) :: ierr
    real :: Tdew !< K
    !
    real :: Pcpy,tci,pci,oi,p_red,dP,vG,vL,sG,sL,dPdT
    real :: dT
    integer :: ierr_local, i
    integer, parameter :: n = 2
    real, parameter :: safe_rel_press = 0.02
    Pcpy = P
    ! Initial temperature
    call PureSat(Tdew,Pcpy,Y,.true.,ierr=ierr_local)
    ! Find real bubble point temperature
    Tdew = dewT(Tdew,Pcpy,X,Y,ierr_local)
    if (ierr_local /= 0 .and. isSingleComp(Y)) then
      ! Get critical point
      call getCriticalParam(maxComp(Y),tci,pci,oi)
      if ((pci-p)/pci < safe_rel_press) then
        ! Reduce p and try controlled steps tovards critical point
        p_red = pci*(1.0-safe_rel_press)
        call PureSat(Tdew,P_red,Y,.true.,ierr=ierr_local)
        Tdew = dewT(Tdew,P_red,X,Y,ierr_local)
        dP = pci*safe_rel_press/n
        do i=1,n
          call specificVolume(Tdew,P_red,X,LIQPH,vL)
          call specificVolume(Tdew,P_red,Y,VAPPH,vG)
          call entropy_tv(Tdew,vL,X,sL)
          call entropy_tv(Tdew,vG,Y,sG)
          dPdT = (sG-sL)/(vG-vL)
          dP = min(dP,p-P_red)
          P_red = P_red + dP
          dT = dP/dPdT
          Tdew = Tdew + dT
          Tdew = dewT(Tdew,P_red,X,Y,ierr_local)
          if (abs(P_red-p)/p < machine_prec) exit
        enddo
      endif
    endif
    if (present(ierr)) then
      ierr = ierr_local
    else if (ierr_local /= 0) then
      print *,'Specified pressure (bar): ',p/1.0e5
      write(*,*) "Computed value for T/K or P/Pa", Tdew
      write(*,*) "Exit flag: ", ierr
      call stoperror('safe_dewT::No convergece')
    endif

  end function safe_dewT

  !-----------------------------------------------------------------------------
  !> Calculate dew point pressure given temperature
  !> Use PureSat to calculate initial value
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function safe_dewP(T,X,Y,ierr) result(Pdew)
    implicit none
    real, dimension(nc), intent(out) :: X
    real, dimension(nc), intent(in) :: Y
    real, intent(in) :: T
    integer, optional, intent(out) :: ierr
    real :: Pdew !< Pa
    !
    real :: Tcpy
    Tcpy = T
    ! Initial pressure
    call PureSat(Tcpy,Pdew,Y,.false.,ierr=ierr)

    ! Find real bubble point temperature
    Pdew = dewP(Tcpy,Pdew,X,Y,ierr)
  end function safe_dewP

  !-----------------------------------------------------------------------------
  !> Calculate bubble point temperature given pressure
  !>
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function bubT(T,P,X,Y,ierr) result(Tbub)
    implicit none
    real, dimension(nc), intent(in) :: X
    real, dimension(nc), intent(out) :: Y
    real, intent(inout) :: T, P
    integer, optional, intent(out) :: ierr
    !
    logical :: doBubIn
    real, dimension(nc) :: Z
    integer :: spec
    real :: Tbub
    Z = X
    doBubIn = .true.
    spec = specP
    call sat(X,Y,Z,T,P,spec,doBubIn,ierr)
    Tbub = T
  end function bubT

  !-----------------------------------------------------------------------------
  !> Calculate bubble point pressure given temperature
  !>
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function bubP(T,P,X,Y,ierr) result(Pbub)
    implicit none
    real, dimension(nc), intent(in) :: X
    real, dimension(nc), intent(out) :: Y
    real, intent(inout) :: T, P
    integer, optional, intent(out) :: ierr
    !
    logical :: doBubIn
    real, dimension(nc) :: Z
    integer :: spec
    real :: Pbub
    Z = X
    doBubIn = .true.
    spec = specT
    call sat(X,Y,Z,T,P,spec,doBubIn,ierr)
    Pbub = P
  end function bubP

  !-----------------------------------------------------------------------------
  !> Calculate dew point temperature given pressure
  !>
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function dewT(T,P,X,Y,ierr) result(Tdew)
    implicit none
    real, dimension(nc), intent(out) :: X
    real, dimension(nc), intent(in) :: Y
    real, intent(inout) :: T, P
    integer, optional, intent(out) :: ierr
    !
    logical :: doBubIn
    real, dimension(nc) :: Z
    integer :: spec
    real :: Tdew
    Z = Y
    doBubIn = .false.
    spec = specP
    call sat(Y,Z,X,T,P,spec,doBubIn,ierr)
    Tdew = T
  end function dewT

  !-----------------------------------------------------------------------------
  !> Calculate dew point pressure given temperature
  !>
  !> \author MH, 2013-03-05
  !-----------------------------------------------------------------------------
  function dewP(T,P,X,Y,ierr) result(Pdew)
    implicit none
    real, dimension(nc), intent(out) :: X
    real, dimension(nc), intent(in) :: Y
    real, intent(inout) :: T, P
    integer, optional, intent(out) :: ierr
    !
    logical :: doBubIn
    real, dimension(nc) :: Z
    integer :: spec
    real :: Pdew
    Z = Y
    doBubIn = .false.
    spec = specT
    call sat(Y,Z,X,T,P,spec,doBubIn,ierr)
    Pdew = P
  end function dewP

  !-----------------------------------------------------------------------------
  !> Calculate saturation value, pressure or temperature
  !>
  !> \author MH, 2012, EA, 2014
  !-----------------------------------------------------------------------------
  subroutine sat(Z,Y,X,t,p,specification,doBubIn,ierr_out)
    use eos, only: getCriticalParam, specificVolume
    implicit none
    integer, intent(in) :: specification     ! Indicates whether T or P is fixed
    real, dimension(nc), intent(in) :: Z     ! Total composition
    real, dimension(nc), intent(out) :: Y,X  ! Composition of vapor and liquid
    real, intent(inout) :: t, p              ! Temperature [K], pressure [Pa]
    logical, intent(in) :: doBubIn
    integer, optional, intent(out) :: ierr_out
    ! Locals
    real, dimension(nc) :: K
    integer, dimension(1) :: imax
    real, dimension(1) :: xx,xmax,xmin
    real, dimension(4) :: param
    real :: tci,pci,oi
    type(nonlinear_solver) :: solver
    integer :: return_iter, ierr
    real    :: vL, vG, Tmin, Tmax
    real :: beta, ln_s, tin, pin, P0, T0
    character(len=clen) :: errorMessage

    if (specification /= specT .and. specification /= specP) then
      print *,'sat: Only possible to call with spec=1 and spec=2'
      call exit(1)
    endif

    if (present(ierr_out)) then
      ierr_out = 0
    endif

    ! Test for single component
    imax = maxloc(Z)
    if (Z(imax(1)) == sum(Z)) then
      param(1) = specification
      param(2) = imax(1)
      call getCriticalParam(imax(1),tci,pci,oi)
      if (specification == specP) then
        if (p > pci) then
          call stoperror('saturation::sat: p > critical pressure')
        endif
        param(3) = p
        T0 = T
        XX(1) = log(min(t,tci))
        xmax = log(tci)
        Tmin = tpTmin
        Tmax = tpTmax
        xmin = log(Tmin)
      else
        if (t > tci) then
          print *, "T=", t
          call stoperror('saturation::sat: t > critical temperature')
        endif
        param(3) = t
        XX(1) = log(min(p,pci))
        P0 = P
        xmax = log(pci)
        xmin = log(tpPmin)
      endif

      ! Attempt to solve for single-comp saturation point
      solver%max_it = 200

      param(4) = 0.0
      call nonlinear_solve(solver,sat_fun_single_if,sat_diff_single,&
           sat_diff_single,limit_dx,premReturn,setXv,XX,xmin,xmax,param)
      if (specification == specP) then
        T = exp(XX(1))
      else
        P = exp(XX(1))
      endif

      if (solver%exitflag /= 0) then
        ! Something went wrong.
        if (solver%exitflag == 2) then
          ! Could not invert Jacobian
          ! If close to the critical point, we may have fallen out of
          ! the area with two density solutions.
          if (abs(P-pci)/pci < 0.01 .and. abs(T-tci)/tci < 0.01) then
            ! Close to the critical point
            ! Check if we have only one root here
            call specificVolume(T,P,Z,LIQPH,vL)
            call specificVolume(T,P,Z,VAPPH,vG)
            if (abs(vL-vG)/(vL+vG) < 1e-12) then
              param(4) = 1.0
              call nonlinear_solve(solver,sat_fun_single_if,sat_diff_single,&
                   sat_diff_single,limit_dx,premReturn,setXv,XX,xmin,xmax,param)
            endif
          endif
        endif

        if (solver%exitflag /= 0) then
          if (present(ierr_out)) then
            ierr_out = solver%exitflag
          else
            ! Failed...
            if (specification == specP) then
              print *,'Specified pressure (bar): ',p/1.0e5
              print *,'Initial temperature (K): ',T0
            else
              print *,'Specified temperature (K): ',T
              print *,'Initial pressure (bar): ',P0/1.0e5
            endif
            write(*,*) "Computed value for T/K or P/Pa", exp(XX(1))
            write(*,*) "tci, pci=", tci, pci
            write(*,*) "Exit flag: ", solver%exitflag
            write(*,*) "Error on exit: ", solver%error_on_exit
            call stoperror('saturation::sat did not converge')
          endif
        endif
      endif
      X = Z
      Y = Z
    else ! We are dealing with a mixture

      pin = p
      tin = t
      call sat_wilsonK(Z,K,t,p,specification,doBubIn,ierr=ierr)
      ! For stabillity do some successive substitution iterations before full NR solver...
      return_iter = 5
      call sat_successive(Z,K,t,p,specification,doBubIn,return_iter,ierr)
      ! ...but guard against divergence
      if (t /= t .or. p /= p) then
         p = pin
         t = tin
         call sat_wilsonK(Z,K,t,p,specification,doBubIn,ierr=ierr)
      end if

      beta = 1.0
      if (doBubIn) beta = 0.0
      if (specification==specT) then
        ln_s = log(T)
      else
        ln_s = log(P)
      end if

      return_iter = sat_newton(Z,K,t,p,beta,ispec(specification),ln_s,ierr=ierr)

      if (ierr /= 0 .or. maxval(abs(K-1)) < 1e-8) then
        p = pin
        t = tin
        return_iter = 200
        ! Restart using successive substitution approach
        call sat_successive(Z,K,t,p,specification,doBubIn,return_iter,ierr)
        if (ierr /= 0) then
          errorMessage = "sat failed"
        endif
        if (maxval(abs(K-1)) < 1e-8) then
          ierr = 2
          errorMessage = "sat converged to trivial solution"
        end if
      endif

      if (present(ierr_out)) then
        ierr_out = ierr
      else if (ierr /= 0) then
        call stoperror(trim(errorMessage))
      endif

      if (doBubIn) then
        Y = K * X
        X  = Z
      else
        Y = Z
        X = Z / K
      endif
    endif
  end subroutine sat

  !-----------------------------------------------------------------------------
  !> Determine saturation point based on Wilson K-factors
  !>
  !> \author MH
  !-----------------------------------------------------------------------------
  subroutine sat_wilsonK(Z,K,t,p,specification,doBub,pid,ierr)
    use thermo_utils, only: wilsonK
    use thermopack_constants, only: STDLIQ
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(out) :: K
    real, intent(inout) :: t, p
    logical, intent(in) :: doBub
    integer, intent(in) :: specification
    integer, optional, intent(in) :: pid
    integer, optional, intent(out) :: ierr
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(nc+4) :: param
    real, dimension(1) :: xmax, xmin, x
    integer :: iter
    integer, parameter :: max_iter = 100, max_nr_line_s = 3

    param(1) = real(specification)
    if (doBub) then
      param(2) = 1.0
    else
      param(2) = 0.0
    endif
    if (specification == specP) then ! Solve for T, P given
      X(1) = t
      param(3) = p
      Xmin = tpTmin
      Xmax = tpTmax
    else
      param(3) = t
      x(1) = p
      Xmin = tpPmin
      Xmax = tpPmax
    endif
    param(4:nc+3) = z
    if (present(pid)) then
      param(nc+4) = real(pid)
    else
      param(nc+4) = real(STDLIQ)
    endif

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-8
    solver%limit_x_values = .true.
    solver%max_it = max_iter
    solver%ls_max_it = max_nr_line_s
    call nonlinear_solve(solver,wilsonK_fun,wilsonK_diff,&
         wilsonK_diff,limit_dx,premReturn,setXv,X,Xmin,Xmax,param)
    iter = solver%iter
    if (solver%exitflag == 0) then
      if (specification == specP) then ! Solve for T, P given
        t = X(1)
      else
        p = x(1)
      endif
      ! Update K
      call wilsonK(t,p,K,liqType=pid)
    endif
    if (present(ierr)) then
      if (solver%exitflag /= 0) then
        ierr = solver%exitflag
      else
        if (T<=tpTmin .or.  T>=tpTmax .or. p<=tpPmin .or. p>=tpPmax) then
          ierr = -1
        else
          ierr = 0
        endif
      endif
    else if (solver%exitflag /= 0) then
      call stoperror('saturation::sat_wilsonK did not converge')
    endif
  end subroutine sat_wilsonK

  !-----------------------------------------------------------------------------
  !> Calculate function value for saturation point
  !> based on Wilson K-factors
  !>
  !> \author MH
  !-----------------------------------------------------------------------------
  subroutine wilsonK_fun(F,X,param)
    use thermo_utils, only: wilsonK
    implicit none
    real, dimension(1), intent(out) :: F
    real, dimension(1), intent(in) :: X
    real, dimension(nc+4), intent(in) :: param
    ! Locals
    integer :: spec, pid
    logical :: doBub
    real :: t, p
    real, dimension(nc) :: Z, K

    spec = nint(param(1))
    doBub = (nint(param(2)) /= 0)
    if (spec == specP) then ! Solve for T, P given
      t = X(1)
      p = param(3)
    else
      t = param(3)
      p = x(1)
    endif
    z = param(4:nc+3)
    pid = nint(param(nc+4))
    call wilsonK(t,p,K,liqType=pid)
    if (doBub) then
      ! Bubble point
      f = sum(K*Z) - 1.0
    else
      ! Dew point
      f = sum(Z/K) - 1.0
    endif
  end subroutine wilsonK_fun

  !-----------------------------------------------------------------------
  !> Calculate differentials for saturation point
  !> based on Wilson K-factors
  !>
  !> \author MH
  !-----------------------------------------------------------------------
  subroutine wilsonK_diff(J,X,param)
    use thermo_utils, only: wilsonK
    implicit none
    real, dimension(1,1), intent(out) :: J
    real, dimension(1), intent(in) :: X
    real, dimension(nc+4), intent(in) :: param
    ! Locals
    integer :: spec, pid
    logical :: doBub
    real, dimension(nc) :: Z, K, dKdp, dKdt
    real :: dfdt, dfdp, p, t

    spec = nint(param(1))
    doBub = (nint(param(2)) /= 0)
    if (spec == specP) then ! Solve for T, P given
      t = X(1)
      p = param(3)
    else
      t = param(3)
      p = x(1)
    endif
    z = param(4:nc+3)
    pid = nint(param(nc+4))
    call wilsonK(t,p,K,dKdt,dKdp,liqType=pid)
    if (doBub) then
      ! Bubble point
      dfdt = sum(dKdt*Z)
      dfdp = sum(dKdp*Z)
    else
      ! Dew point
      dfdt = -sum(dKdt*Z/K**2)
      dfdp = -sum(dKdp*Z/K**2)
    endif
    if (spec == specP) then ! Solve for T, P given
      J = dfdt
    else
      J = dfdp
    endif
  end subroutine wilsonK_diff

  !-----------------------------------------------------------------------------
  !> Function value and differentials for saturation point.
  !> Used in successive substitution solver
  !>
  !> \author MH
  !-----------------------------------------------------------------------------
  subroutine sat_fun(Z,K,t,p,f,dfdt,dfdp,doBub)
    implicit none
    real, dimension(nc), intent(inout) :: K
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: p, t
    real, intent(out) :: dfdt, dfdp, f
    logical, intent(in) :: doBub
    ! Locals
    real, dimension(nc) :: FUGV, FUGTV, FUGPV, FUGL, FUGTL, FUGPL, Y, X
    if (doBub) then
      ! Bubble point
      Y = K*Z
      call thermo(t,p,Z,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL)
      call thermo(t,p,Y,VAPPH,fugV,lnfugt=fugtV,lnfugp=fugpV)
      K = exp(FUGL-FUGV)
      f = sum(K*Z) - 1.0
      dfdt = sum(K*Z*(FUGTL-FUGTV))
      dfdp = sum(K*Z*(FUGPL-FUGPV))
    else
      ! Dew point
      X = Z/K
      call thermo(t,p,X,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL)
      call thermo(t,p,Z,VAPPH,fugV,lnfugt=fugtV,lnfugp=fugpV)
      K = exp(FUGL-FUGV)
      f = sum(Z/K) - 1.0
      dfdt = -sum(Z*(FUGTL-FUGTV)/K)
      dfdp = -sum(Z*(FUGPL-FUGPV)/K)
    endif
  end subroutine sat_fun

  !-----------------------------------------------------------------------------
  !> Interface to sat_fun_single
  !>
  !> \author MH
  !-----------------------------------------------------------------------------
  subroutine sat_fun_single_if(F,X,param)
    implicit none
    real, dimension(1), intent(in) :: X
    real, dimension(1), intent(out) :: F
    real, dimension(4), intent(inout) :: param
    ! Locals
    integer :: spec,i
    real :: p, t
    real :: dfdt, dfdp
    logical :: metaExtr

    spec = int(param(1))
    i = int(param(2))
    if (param(4) == 0.0) then
      metaExtr = .false.
    else
      metaExtr = .true.
    endif
    if (spec == specP) then
      p = param(3)
      t = exp(X(1))
    else
      p = exp(X(1))
      t = param(3)
    endif
    call sat_fun_single(i,t,p,F(1),dfdt,dfdp,metaExtr)
  end subroutine sat_fun_single_if

  !-----------------------------------------------------------------------------
  !> Differentials for single component saturation point
  !>
  !> \author MH
  !-----------------------------------------------------------------------------
  subroutine sat_diff_single(Jac,X,param)
    implicit none
    real, dimension(1), intent(in) :: X
    real, dimension(1,1), intent(out) :: Jac
    real, dimension(4), intent(inout) :: param
    ! Locals
    integer :: spec,i
    real :: p, t
    real :: dfdt, dfdp, f
    logical :: metaExtr
    spec = int(param(1))
    i = int(param(2))
    if (param(4) == 0.0) then
      metaExtr = .false.
    else
      metaExtr = .true.
    endif
    if (spec == specP) then
      p = param(3)
      t = exp(X(1))
      call sat_fun_single(i,t,p,f,Jac(1,1),dfdp,metaExtr)
      Jac(1,1) = Jac(1,1)*T
    else
      p = exp(X(1))
      t = param(3)
      call sat_fun_single(i,t,p,f,dfdt,Jac(1,1),metaExtr)
      Jac(1,1) = Jac(1,1)*P
    endif
  end subroutine sat_diff_single

  !-----------------------------------------------------------------------------
  !> Function value and differentials for single component saturation point
  !>
  !> \author MH
  !-----------------------------------------------------------------------------
  subroutine sat_fun_single(i,t,p,f,dfdt,dfdp,metaExtr)
    implicit none
    integer, intent(in) :: i
    real, intent(in) :: p, t
    real, intent(out) :: dfdt, dfdp, f
    logical, intent(in)  :: metaExtr
    ! Locals
    real, dimension(nc) :: Z, FUGV, FUGTV, FUGPV, FUGL, FUGTL, FUGPL
    Z = 0.0
    Z(i) = 1.0
    call thermo(t,p,Z,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL,&
         metaExtremum=metaExtr)
    call thermo(t,p,Z,VAPPH,fugV,lnfugt=fugtV,lnfugp=fugpV,&
         metaExtremum=metaExtr)
    f = fugV(i)-fugL(i)
    dfdt = FUGTV(i)-FUGTL(i)
    dfdp = FUGPV(i)-FUGPL(i)
  end subroutine sat_fun_single

    !-----------------------------------------------------------------------------
  !> Solve for saturation line using NR solver.
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  function sat_newton(Z,K,t,p,beta,s,ln_s,Yphase,ierr) result(iter)
    use numconstants, only: expMax, expMin
    implicit none
    real, dimension(nc), intent(in) :: Z    ! total composition
    real, dimension(nc), intent(inout) :: K ! equilibrium factors; K_i = y_i/x_i
    real, intent(inout) :: p                ! pressure [Pa]
    real, intent(inout) :: t                ! temperature [K]
    real, intent(in) :: beta                ! vapor quality
    real, intent(in) :: ln_s                ! logarithm of the fixed variable
    integer, intent(in) :: s                ! index of fixed variable in X
    integer, intent(in), optional :: Yphase ! Phase flag for Y-phase
    integer, intent(out), optional :: ierr  ! error flag
    integer :: iter
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(nc+4) :: param
    real, dimension(nc+2) :: xmax, xmin, X
    X(1:nc) = log(K)
    X(nc+1) = log(t)
    X(nc+2) = log(p)

    param(1:nc) = Z
    param(nc+1) = beta
    param(nc+2) = real(s) ! typecast since param can only contain reals..
    param(nc+3) = ln_s
    if (present(Yphase)) then
      param(nc+4) = Yphase
    else
      param(nc+4) = VAPPH
    endif

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-8
    solver%limit_x_values = .false.
    solver%max_it = sat_max_iter
    solver%ls_max_it = sat_max_nr_line_s

    Xmin = expMin
    Xmax = expMax
    Xmin(nc+1) = log(tpTmin) !Tmin
    Xmax(nc+1) = log(tpTmax) !Tmax
    Xmin(nc+2) = log(tpPmin) !Pmin
    Xmax(nc+2) = log(tpPmax) !Pmax

    call nonlinear_solve(solver,sat_fun_newton,sat_diff_newton,&
         sat_diff_newton,limit_dx,premReturn,setXv,X,Xmin,Xmax,param)
    iter = solver%iter
    if (solver%exitflag == 0) then
      K = exp(X(1:nc))
      t = exp(X(nc+1))
      p = exp(X(nc+2))
      if (verbose) then
        print *,'sat_newton: converged after ', iter, ' number of iterations'
      end if
    endif
    if (present(ierr)) then
      ierr = solver%exitflag
      if (sat_var_at_limit(X)) ierr = -1
      if (maxval(abs(K-1))<1e-8) ierr = -2
    endif

  end function sat_newton

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  subroutine sat_fun_newton(G,Xvar,param)
    use eos, only: entropy, enthalpy
    implicit none
    real, dimension(nc+2), intent(out) :: G !< Function values
    real, dimension(nc+2), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+4) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: K, FUGV, FUGL, Y, X
    real :: hsl,hsg,hs,denum
    integer :: s, i
    real :: p, t,ln_s, beta

    Z = param(1:nc)
    beta = param(nc+1)
    s = int(param(nc+2))
    ln_s = param(nc+3)

    if (sat_var_at_limit(Xvar)) then
      G = 0.0 ! Terminate solver
      return
    endif
    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))
    if (maxval(abs(K-1))<1e-8) then
      ! Converging to trivial solution
      G = 0.0 ! Terminate solver
      return
    endif

    X = Z/(1-beta+beta*K)
    Y = K*Z/(1-beta+beta*K)

    call thermo(t,p,X,LIQPH,fugL)
    call thermo(t,p,Y,VAPPH,fugV)
    ! Function value
    do i=1,nc
      if (Z(i) > 0.0) then
        G(i) = Xvar(i) + FUGV(i) - FUGL(i)
      else
        G(i) = 0.0
      endif
    enddo
    G(nc+1) = sum(Y-X)
    if (s <= nc + 2) then
      G(nc+2) = Xvar(s) - ln_s
    else
      if (s == nc + 3) then
        ! Entropy
        call entropy(t,p,X,LIQPH,hsl)
        call entropy(t,p,Y,VAPPH,hsg)
      else if (s == nc + 4) then
        ! Enthalpy
        call enthalpy(t,p,X,LIQPH,hsl)
        call enthalpy(t,p,Y,VAPPH,hsg)
      endif
      denum = max(abs(ln_s), 1.0)
      !
      hs = beta*hsg + (1-beta)*hsl
      G(nc+2) = (hs - ln_s)/denum
    endif

  end subroutine sat_fun_newton

  !-----------------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  function sat_var_at_limit(Xvar,eps) result(atLimit)
    use numconstants, only: expMax, expMin
    implicit none
    real, dimension(nc+2), intent(in) :: Xvar !< Variable vector
    real, optional, intent(in) :: eps !< Safety margin
    logical :: atLimit
    ! Locals
    integer :: i
    real :: eps_local
    real :: lnMin(nc+2), lnMax(nc+2)
    atLimit = .false.
    lnMin(1:nc) = expMin
    lnMax(1:nc) = expMax
    lnMin(nc+1) = log(tpTmin)
    lnMax(nc+1) = log(tpTmax)
    lnMin(nc+2) = log(tpPmin)
    lnMax(nc+2) = log(tpPmax)

    eps_local = sat_limitEps
    if (present(eps)) then
      eps_local = eps
    endif
    do i=1,nc+2
      if ( Xvar(i) >= lnMax(i) - eps_local .or. &
           Xvar(i) <= lnMin(i) + eps_local) then
        atLimit = .true.
      endif
    enddo
  end function sat_var_at_limit

  !-----------------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-03-05
  !-----------------------------------------------------------------------------
  subroutine sat_diff_newton(Jac,Xvar,param)
    use eos, only: entropy, enthalpy
    implicit none
    real, dimension(nc+2), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2,nc+2), intent(out) :: Jac !< Function differentials
    real, dimension(nc+4) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real, dimension(nc) :: K, FUGV, FUGTV, FUGPV, FUGL, FUGTL, FUGPL, Y, X
    real, dimension(nc,nc) :: FUGXV, FUGXL
    real :: hsl,hslt,hslp,hsln(nc),hsg,hsgt,hsgp,hsgn(nc),hs
    integer :: s, i, j, Yphase
    real :: p, t, beta

    Z = param(1:nc)
    beta = param(nc+1)
    s = nint(param(nc+2))
    Yphase = int(param(nc+4))

    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))

    X = Z/(1-beta+beta*K)
    Y = K*Z/(1-beta+beta*K)

    call thermo(t,p,X,LIQPH,fugL,lnfugt=fugtL,lnfugp=fugpL,lnfugx=fugxL)
    call thermo(t,p,Y,Yphase,fugV,lnfugt=fugtV,lnfugp=fugpV,lnfugx=fugxV)

    ! Temperature differential
    Jac(1:nc,nc+1) = t*(FUGTV-FUGTL)

    ! Pressure differential
    Jac(1:nc,nc+2) = p*(FUGPV-FUGPL)

    ! K differentials
    do i=1,nc
      if (Z(i) > 0.0) then
        do j=1,nc
          if (Z(j) > 0.0) then
            Jac(i,j) = ((1.0-beta)*FUGXV(i,j)+beta*FUGXL(i,j))*X(j)*Y(j)/Z(j)
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
        Jac(nc+1,i) = X(i)*Y(i)/Z(i)
      else
        Jac(nc+1,i) = 0.0
      endif
    enddo
    Jac(nc+1,nc+1) = 0
    Jac(nc+1,nc+2) = 0

    ! Bottom row
    if (s <= nc + 2) then
      Jac(nc+2,:) = 0.0
      Jac(nc+2,s) = 1.0
    else
      if (s == nc + 3) then
        ! Entropy
        call entropy(t,p,X,LIQPH,hsl,hslt,hslp,hsln)
        call entropy(t,p,Y,VAPPH,hsg,hsgt,hsgp,hsgn)
      else if (s == nc + 4) then
        ! Enthalpy
        call enthalpy(t,p,X,LIQPH,hsl,hslt,hslp,hsln)
        call enthalpy(t,p,Y,VAPPH,hsg,hsgt,hsgp,hsgn)
      endif
      hs = param(nc+3)
      hs = max(abs(hs), 1.0)
      !
      ! nc+2 row
      do i=1,nc
        if (Z(i) > 0.0) then
          Jac(nc+2,i) = (1-beta)*beta*(hsgn(i)+hsln(i))*X(i)*Y(i)/(Z(i)*hs)
        else
          Jac(nc+2,i) = 0.0
        endif
      enddo
      Jac(nc+2,nc+1) = T*(beta*hsgt+(1-beta)*hslt)/hs
      Jac(nc+2,nc+2) = P*(beta*hsgp+(1-beta)*hslp)/hs
    endif

  end subroutine sat_diff_newton

  !-----------------------------------------------------------------------------
  !> Solve for saturation point using successive substitution
  !! Note that after updating T(k+1), we approximate y(k+1)=K(k)*Z,
  !! and does not solve for an exact y(k+1).
  !! \author MH
  !-----------------------------------------------------------------------------
  subroutine sat_successive(Z,K,t,p,specification,doBub,return_iter,ierr)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, dimension(nc), intent(inout) :: K
    real, intent(inout) :: p
    real, intent(inout) :: t
    integer, intent(in) :: specification
    logical, intent(in) :: doBub
    integer, intent(in) :: return_iter
    integer, optional, intent(out) :: ierr
    ! Locals
    real, dimension(nc) :: K_old
    real :: f, dfdx, dx, x_old, f_old, dfdt, dfdp, x
    real :: Tmin, Tmax
    integer :: i
    character(len=*), parameter :: outfile='out_lnT.dat'

    if (present(ierr)) then
      ierr = 0
    endif
    if (testK .and. specification == specP) then
      open(file=outfile,unit=12)
      write(12,*) 'TITLE=Error in lnT and lnK6'
      write(12,*) 'VARIABLES=iter,lnT,lnK6'
    endif

    if (specification == specP) then
      Tmin = tpTmin
      Tmax = tpTmax
    endif

    f = 1.0
    ! Solve for t or p
    do i=1,sat_max_iter
      f_old = f
      K_old = K
      call sat_fun(Z,K,t,p,f,dfdt,dfdp,doBub)

      if (specification == specP) then
        dfdx = dfdt
        x = t
      else if (specification == specT) then
        dfdx = dfdp
        x = p
      endif
      dx = -f / dfdx
      x_old = x
      x = x + dx

      if (specification == specP) then
        t = x
        if (t > Tmax) then
          t = Tmax
          if (present(ierr)) ierr = 1
        endif
        if (t < Tmin) then
          t = Tmin
          if (present(ierr)) ierr = 1
        endif
      else if (specification == specT) then
        p = x
        if (p > tpPmax) then
          p = tpPmax
          if (present(ierr)) ierr = 1
        endif
        if (p < tpPmin) then
          p = tpPmin
          if (present(ierr)) ierr = 1
        endif
      endif
      if (x /= x) then
        if (present(ierr)) ierr = 1
        return
      end if

      if (testK .and. specification == specP) then
        print *,'sat_successive: ln T err ', log(x)-log(x_old)
        print *,'sat_successive: ln K6 ', log(K(6))-log(K_old(6))
        write(12,*) i,abs(log(x)-log(x_old)),abs(log(K(6))-log(K_old(6)))
      endif
      if (abs(f - f_old) < sat_rel_tol .and. abs(x_old - x)/x_old < sat_rel_tol) then
        if (verbose) then
          print *,'sat_successive: Converged in ', i, ' iterations'
        endif
        if (testK .and. specification == specP) then
          close(12)
        endif
        return
      endif
      if (i > return_iter) then
        return ! Use together with Newton method
      endif
    enddo

    if (testK .and. specification == specP) then
      close(12)
    endif
    if (present(ierr)) then
      ierr = 1
    else
      call stoperror('sat_successive: Did not converge')
    endif
  end subroutine sat_successive

  !> Calculate the acentric factor from the EoS.
  !
  !> \author Ailo Aasen
  function acentricFactorEos(i,ierr) result(acf)
    use eos, only: getCriticalParam
    integer, intent(in) :: i !< Component index
    integer, optional, intent(out) :: ierr !< Error flag; 0 iff success
    real :: acf !< Acentric factor [-]
    ! Locals
    real :: Tci, Pci, dummy
    real :: P, T, X(nc), Y(nc)
    real :: Pr

    call getCriticalParam(i,Tci,Pci,dummy)
    T = 0.7*Tci

    X = 0.0
    X(i) = 1.0
    P = safe_bubP(T,X,Y,ierr)
    Pr = P/pci

    acf = -log10(Pr)-1
  end function acentricFactorEos

  !-----------------------------------------------------------------------------
  !> Get specification for saturation point solvers
  !> specification = 1 ! 1:P, 2:T, 3:ln K_1 etc.
  !> \author MH, 2011
  !-----------------------------------------------------------------------------
  function ispec(specification) result(s)
    implicit none
    integer, intent(in) :: specification
    integer :: s
    if (specification == specP) then ! Pressure
      s = nc + 2
    else if (specification == specT) then ! Temperature
      s = nc + 1
    else ! ln K
      s = specification-2
    endif
  end function ispec

  !-----------------------------------------------------------------------------
  !> Test if new liquid phase should be introduced
  !>
  !> \author MH, 2015-09
  !-----------------------------------------------------------------------------
  function isStable(t,p,Z,K,W,Yphase,beta) result(isS)
    use eos, only: thermo
    use thermopack_constants, only: LIQPH, VAPPH, WATER, NONWATER
    use stability, only: stabcalcW, stabilityLimit
    use thermo_utils, only: waterComponentFraction, wilsonK
    implicit none
    real, intent(in) :: t,p,beta
    integer, intent(in) :: Yphase
    real, dimension(nc), intent(in) :: Z,K
    real, dimension(nc), intent(out) :: W
    logical :: isS
    ! Locals
    integer :: nd, phase, Zphase, pid, i
    real :: XX(2,nc),lnFugZ(nc),lnFugW(nc),tpd
    real, dimension(nc) :: Kw,dKwdp,dKwdt,WW

    if (beta > 0.5) then
      Zphase = Yphase
      XX(2,:) = Z/K
    else
      Zphase = LIQPH
      XX(2,:) = K*Z
    endif

    call thermo(t,p,Z,zPhase,lnFugZ)
    nd = 2
    XX(1,:) = Z

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
    tpd = stabcalcW(nd,1,t,p,XX,W,phase,lnFugZ,lnFugW,preTermLim=-1000.0)
    isS = (tpd > -1.0e-9)
    !if (.not. isS) return
    ! Loop all possible phases
    do i=1,nc
      WW = 0.0
      WW(i) = 1.0
      tpd = stabcalcW(nd,1,t,p,XX,WW,LIQPH,lnFugZ,lnFugW,preTermLim=-1000.0)
      if (tpd < -1.0e-9) then
        isS = .false.
        W = WW
        exit
      endif
    enddo
  end function isStable

end module saturation
