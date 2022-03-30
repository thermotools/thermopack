module joule_thompson_inversion
  use eos, only: specificvolume, getCriticalParam, thermo
  use thermopack_constants, only: LIQPH, VAPPH, verbose
  use thermopack_var, only: nc
  use eosTV, only: virial_coefficients, pressure
  use numconstants, only: machine_prec
  use nonlinear_solvers
  implicit none
  private
  save

  integer, parameter :: JTI_TERM_NONE = 0, JTI_TERM_VIRIAL = 1, JTI_TERM_LOWP = 2, &
       JTI_TERM_SOLVER = 3, JTI_TERM_BUBP = 4, JTI_TERM_SATCURVE = 5

  public :: map_jt_inversion, print_JTI_TERM

contains

  !-----------------------------------------------------------------------
  function print_JTI_TERM(iTerm) result(charTerm)
    integer, intent(in) :: iTerm
    character(len=68) :: charTerm
    select case(iTerm)
    case(JTI_TERM_NONE)
      charTerm = "JT inversion mapper succseeded"
    case(JTI_TERM_VIRIAL)
      charTerm = "JT inversion mapper failed to calculate zero pressure solution"
    case(JTI_TERM_LOWP)
      charTerm = "JT inversion mapper failed to calculate low pressure solution"
    case(JTI_TERM_SOLVER)
      charTerm = "JT inversion solver failed"
    case(JTI_TERM_BUBP, JTI_TERM_SATCURVE)
      charTerm = "JT inversion mapper failed to terminate properly at saturation curve"
    case default
      call stoperror("print_JTI_TERM: Wrong error flag")
    end select
  end function print_JTI_TERM

  !-----------------------------------------------------------------------------
  !> Map JT inversion curve
  !>
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  subroutine map_jt_inversion(z,Ta,va,Pa,nmax,n,ierr)
    use saturation, only: safe_bubP, specT
    use saturation_point_locators, only: bracketSolveForPropertySingle, &
         locate_from_joule_thompson, sat_points_based_on_prop
    use thermo_utils, only: isSingleComp
    implicit none
    real, intent(out) :: Ta(nmax)     ! Temperature (K)
    real, intent(out) :: va(nmax)     ! Specific volume (m3/mol)
    real, intent(out) :: Pa(nmax)     ! Pressure (K)
    integer, intent(in) :: nmax       ! Array size
    integer, intent(out) :: n          ! Number of points calculated
    real, intent(in) :: z(nc)         ! Mol fractions
    integer, intent(out) :: ierr      ! error flag
    ! Locals
    integer :: i
    real :: p, v, T, dlnv, dlnTdlnv, dlnT, lnT, lnv, Ylow(nc)
    real :: Plow, Tlow
    logical :: isStable
    character(len=68) :: error_message
    integer, parameter :: n_grid = 1       !< number of grid points
    real :: prop_grid(n_grid)        !< property grid
    real :: T_grid(n_grid)        !< t at the grid points
    real :: P_grid(n_grid)        !< p at the grid points
    integer :: phase_grid(n_grid) !< incumbent phase at grid points
    real :: wi_grid(nc,n_grid)    !< Incipient phase at boundary
    integer :: n_grid_found       !< Number of grid points found
    va = 0
    Ta = 0
    Pa = 0
    n = 0
    call solve_for_zero_p_jt(z,T,ierr)
    if (ierr /= 0) then
      ierr = JTI_TERM_VIRIAL
      error_message = print_JTI_TERM(ierr)
      if (verbose) print *,trim(error_message)
      return
    endif
    v = 1.0e2
    T = T
    call add_point(T,v,z,n,Ta,va,Pa,nmax)
    p = 1.0e4
    call specificvolume(T,P,z,VAPPH,v)
    call jt_inv_solve(z,T,v,ierr)
    if (ierr /= 0) then
      ierr = JTI_TERM_LOWP
      error_message = print_JTI_TERM(ierr)
      if (verbose) print *,trim(error_message)
      return
    endif
    call add_point(T,v,z,n,Ta,va,Pa,nmax)
    dlnv = -0.025
    lnv = log(v)
    lnT = log(T)
    do i=1,nmax-2
      call jt_newton_extrapolate(Z,T,v,dlnTdlnv)
      dlnT = dlnTdlnv*dlnv
      lnT = lnT + dlnT
      T = exp(lnT)
      lnv = lnv + dlnv
      v = exp(lnv)
      call jt_inv_solve(z,T,v,ierr)
      if (ierr /= 0) then
        ierr = JTI_TERM_SOLVER
        error_message = print_JTI_TERM(ierr)
        if (verbose) print *,trim(error_message)
        return
      endif
      P = pressure(T, v, z)
      if (P < 1.0e4) then
        isStable = .false.
      else
        isStable = single_phase_is_stable(T, P, v, z)
      endif
      if (.not. isStable) then
        Tlow = T
        Plow = safe_bubP(Tlow,Z,Ylow,ierr)
        if (ierr /= 0) then
          ierr = JTI_TERM_BUBP
          error_message = print_JTI_TERM(ierr)
          if (verbose) print *,trim(error_message)
          return
        endif
        prop_grid = 0
        ! Search towards higher temperatures
        call sat_points_based_on_prop(Z,Tlow,Plow,z,Ylow,n_grid,&
             locate_from_joule_thompson,prop_grid,&
             T_grid,P_grid,phase_grid,wi_grid,n_grid_found,&
             phase_in=LIQPH,spec_in=specT,ierr_out=ierr)
        if (ierr /= 0) then
          ierr = JTI_TERM_SATCURVE
          error_message = print_JTI_TERM(ierr)
          if (verbose) print *,trim(error_message)
          return
        endif

        T = T_grid(1)
        P = P_grid(1)
        call specificvolume(T,P,z,LIQPH,v)
      endif
      call add_point(T,v,z,n,Ta,va,Pa,nmax)
      ierr = JTI_TERM_NONE
      if (.not. isStable) return
    enddo

  contains
    subroutine add_point(T,v,z,n,Ta,va,Pa,nmax)
      real, intent(in) :: T     ! Temperature (K)
      real, intent(in) :: v     ! Specific volume (m3/mol)
      real, intent(in) :: z(nc)         ! Mol fractions
      real, intent(inout) :: Ta(nmax)     ! Temperature (K)
      real, intent(inout) :: va(nmax)     ! Specific volume (m3/mol)
      real, intent(inout) :: Pa(nmax)     ! Pressure (K)
      integer, intent(in) :: nmax       ! Array size
      integer, intent(inout) :: n          ! Number of points calculated
      n = n + 1
      va(n) = v
      Ta(n) = T
      Pa(n) = pressure(T, v, z)
    end subroutine add_point
  end subroutine map_jt_inversion

  !-----------------------------------------------------------------------------
  !> Calculate zero pressure JT inversion
  !>
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  subroutine solve_for_zero_p_jt(z,T,ierr)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver,&
         NS_PEGASUS
    implicit none
    real, intent(out) :: t            ! Temperature (K)
    real, intent(in) :: z(nc)         ! Mol fractions
    integer, intent(out) :: ierr      ! error flag
    ! Locals
    real :: param(nc), Tmin, Tmax, Tc
    real :: tci,pci,oi,vci,tnbi
    integer :: i
    type(nonlinear_solver) :: solver_zp
    ! Configure solver
    solver_zp%abs_tol = 1e-9
    solver_zp%max_it = 100
    solver_zp%isolver = NS_PEGASUS
    ! Set the constant parameters of the objective function.
    param = z
    ! Estimate critical temperature
    Tc = 0
    do i=1,nc
      call getCriticalParam(i,tci,pci,oi,vci,tnbi)
      Tc = z(i)*tci
    enddo
    Tmin = 1.5*Tc
    Tmax = 20.0*Tc
    ! Find f=0 inside the bracket.
    call bracketing_solver(Tmin,Tmax,zero_press_jt_inversion,T,solver_zp,param)
    ierr = solver_zp%exitflag
    ! Check for successful convergence
    if (ierr /= 0) then
      if (verbose) write(*,*) "solve_for_zero_p_jt: Bracketing solver failed."
    endif
  end subroutine solve_for_zero_p_jt

  !-----------------------------------------------------------------------------
  !> Test stability of single phase
  !>
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  function single_phase_is_stable(T, P, v, z) result(isStable)
    use stability, only: stabcalc, stabilityLimit
    use eostv, only: thermo_tvp
    implicit none
    real, intent(in) :: T, P, v, z(nc)
    logical :: isStable
    ! Locals
    real :: lnFugZ(nc), lnFug(nc), tpd
    call thermo_tvp(T,v,z,lnfugZ)
    tpd = stabcalc(t,p,Z,LIQPH,lnFugZ,lnFug)
    isStable = (tpd > stabilityLimit)
    if (.not. isStable) return

    tpd = stabcalc(t,p,Z,VAPPH,lnFugZ,lnFug)
    isStable = (tpd > stabilityLimit)

  end function single_phase_is_stable

  !-----------------------------------------------------------------------------
  !> Zero-pressure inversion temperature
  !>
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  function zero_press_jt_inversion(T, param) result(f)
    implicit none
    real, intent(in) :: T
    real, intent(in) :: param(nc)
    real :: f
    ! Locals
    real :: Z(nc), B0, B1, B2, C, dT, dBdT
    Z = param
    call virial_coefficients(T,z,B0,C)
    dT = 1.0e-5*T
    call virial_coefficients(T-dT,z,B1,C)
    call virial_coefficients(T+dT,z,B2,C)
    dBdT = (B2-B1)/(2*dT)
    f = (dBdT*T - B0)/max(abs(B0),1.0e-6)
    !print *,"T,f,B0,dBdT, dBdT*T", T,f,B0,dBdT, dBdT*T
  end function zero_press_jt_inversion

  !-----------------------------------------------------------------------------
  !> Solve for JT inversion point
  !>
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  subroutine jt_inv_solve(Z,T,v,ierr)
    use thermopack_constants, only: get_templimits
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(inout) :: T !< Temperature
    real, intent(in) :: v !< Specified volume
    integer, intent(out) :: ierr !< Error indicator
    ! Locals
    real :: param(nc+2), xmax(1), xmin(1), X(1), Tmin, Tmax, dpdt, P
    type(nonlinear_solver) :: solver
    procedure(jacobian_template), pointer :: jt_jac

    jt_jac => NULL()
    param(1:nc) = Z
    param(nc+1) = v
    P = pressure(T, v, z, dpdt=dpdt)
    param(nc+2) = max(abs(T*dpdt),1.0)
    call get_templimits(Tmin,Tmax)
    xmax(1) = log(Tmax + 10.0)
    xmin(1) = log(max(Tmin - 10.0, 2.0))
    x(1) = log(T)

    solver%rel_tol = 1e-20
    solver%abs_tol = 1e-10
    solver%limit_x_values = .true.
    solver%analyt_jac = .false.
    solver%analyt_jac_order = 2
    solver%max_it = 100
    solver%ls_max_it = 5

    call nonlinear_solve(solver,jt_inv_fun,jt_jac,jt_jac,limit_dx,&
         premReturn,setXv,X,xmin,xmax,param)
    ierr = solver%exitflag
    if (solver%exitflag /= 0) then
      print *,'joule_thompson_inversion::jt_inv_solve: Not able to solve for JT inversion'
    endif
    T = exp(X(1))
  end subroutine jt_inv_solve

  !-----------------------------------------------------------------------------
  !> JT inversion function
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  subroutine jt_inv_fun(F,Xvar,param)
    implicit none
    real, dimension(1), intent(out) :: F !< Function value
    real, dimension(1), intent(in) :: Xvar !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z
    real :: T, v, P, dpdv, dpdt, denum
    Z = param(1:nc)
    v = param(nc+1)
    denum = param(nc+2)
    T = exp(Xvar(1))
    P = pressure(T, v, z, dpdv, dpdt)
    f(1) = (T*dpdt + v*dpdv)/denum
    !print *,T*dpdt, v*dpdv, f
  end subroutine jt_inv_fun

  !-----------------------------------------------------------------------------
  !> Extrapolate along JT inversion curve
  !>
  !> \author MH, 2021-04
  !-----------------------------------------------------------------------------
  subroutine jt_newton_extrapolate(Z,T,v,dlnTdlnv)
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall composition
    real, intent(in) :: T !< Temperature
    real, intent(in) :: v !< Specified volume
    real, intent(out) :: dlnTdlnv !< Sensitivity
    ! Locals
    real, dimension(1) :: F1, F2 !< Function value
    real, dimension(1) :: Xvar !< Variable vector
    real, dimension(nc+2) :: param !< Parameter vector
    real :: eps, dv, dfdlnT, dfdv
    param(1:nc) = Z
    param(nc+1) = v
    param(nc+2) = 1
    eps = 1.0e-5
    Xvar = log(T) - eps
    call jt_inv_fun(F1,Xvar,param)
    Xvar = log(T) + eps
    call jt_inv_fun(F2,Xvar,param)
    dfdlnT = (F2(1) - F1(1))/(2*eps)
    Xvar = log(T)
    dv = eps*v
    param(nc+1) = v - dv
    call jt_inv_fun(F1,Xvar,param)
    param(nc+1) = v + dv
    call jt_inv_fun(F2,Xvar,param)
    dfdv = (F2(1) - F1(1))/(2*dv)
    dlnTdlnv = -v*dfdv/dfdlnT
  end subroutine jt_newton_extrapolate

end module joule_thompson_inversion
