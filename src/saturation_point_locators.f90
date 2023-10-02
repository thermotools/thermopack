!> Functionality for locating points on the saturation curve based on
!> auxiliary properties such as specific volume or entropy.
!> The isentropeEnvelopeCross routines should eventually be moved from
!> saturation to saturation_point_locators
!>
!> \author Ailo 2015-12
!> \author MH 2018-06
module saturation_point_locators ! Give a more descriptive name..
  use saturation
  use saturation_curve
  use solid_saturation
  use eos, only: thermo, entropy, specificVolume
  use thermopack_constants
  use thermopack_var, only: nc, nph, get_active_thermo_model, thermo_model
  use nonlinear_solvers
  use numconstants, only: machine_prec
  use puresaturation, only: puresat
  implicit none
  save
  private

  integer, parameter, public :: locate_from_entropy = 1
  integer, parameter, public :: locate_from_lnvol = 2
  integer, parameter, public :: locate_from_enthalpy = 3
  integer, parameter, public :: locate_from_temperature = 4
  integer, parameter, public :: locate_from_pressure = 5
  integer, parameter, public :: locate_from_joule_thompson = 6

  public :: sat_points_based_on_prop
  public :: bracketSolveForPropertySingle
  public :: iso_cross_saturation_line

contains

  !-----------------------------------------------------------------------------
  !> Bracket solver for finding the exact point on the phase envelope
  !>
  !> What property is is determined by propflag.
  !>
  !> \author MH 2013-10, Ailo 2015-12
  !-----------------------------------------------------------------------------
  subroutine bracketSolveForProperty(n,Z,beta,propflag,propspec,X,Xold,dXdS,s,ierr,mode)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: Z(nc)
    real, intent(in) :: beta
    integer, intent(in) :: propflag
    real, intent(in) :: propspec
    real, intent(out) :: X(n)
    real, intent(in) :: Xold(n)
    real, intent(in) :: dXdS(n)
    integer, intent(in) :: s
    integer, optional, intent(out) :: ierr
    integer, optional, intent(in) :: mode
    ! Locals
    real, dimension(nc+3*n+5) :: param
    type(nonlinear_solver) :: solver
    real :: Xs,Xsmax,Xsmin
    !
    if (verbose) then
      print *,'In bracketSolveForProperty....'
    endif
    param(1) = propspec
    param(2) = beta
    param(3) = real(s)
    param(4:nc+3) = Z
    param(nc+4:nc+n+3) = Xold
    param(nc+n+4:nc+2*n+3) = dXdS
    param(nc+2*n+4:nc+3*n+3) = X
    param(nc+3*n+4) = real(propflag)
    if (present(mode)) then
      param(nc+3*n+5) = real(mode)
    endif
    solver%abs_tol = 1e-8
    solver%max_it = 30
    if (X(s) > Xold(s)) then
      Xsmax = X(s)
      Xsmin = Xold(s)
    else
      Xsmax = Xold(s)
      Xsmin = X(s)
    endif
    Xs = X(s)
    if (n == nc + 2) then
      call pegasus(Xsmin,Xsmax,propertyFunctionWrapper,Xs,solver,param)
    else if (n == 2) then
      call pegasus(Xsmin,Xsmax,propertyFunctionWrapperSubl,Xs,solver,param)
    else if (n == nc + 3) then
      call pegasus(Xsmin,Xsmax,propertyFunctionWrapperTriple,Xs,solver,param)
    endif
    if (present(ierr)) then
      ierr = solver%exitflag
    else
      if (solver%exitflag /= 0) then
        write(*,*) "Exitflag: ",solver%exitflag
        call stoperror("bracketSolveForProperty failed.")
      endif
    endif
    X = param(nc+2*n+4:nc+3*n+3)
  end subroutine bracketSolveForProperty

  !-----------------------------------------------------------------------------
  !> Bracket solver for finding the exact point on saturation line
  !>
  !> What property is is determined by propflag.
  !>
  !> \author MH 2019-05
  !-----------------------------------------------------------------------------
  subroutine bracketSolveForPropertySingle(ic,propflag,propspec,phase,T1,P1,dPdT1,Ts,Ps,ierr)
    implicit none
    integer, intent(in) :: ic, phase
    real, intent(in) :: T1,P1,dPdT1
    real, intent(inout) :: Ts,Ps
    integer, intent(in) :: propflag
    real, intent(in) :: propspec
    integer, optional, intent(out) :: ierr
    ! Locals
    real, dimension(9) :: param
    type(nonlinear_solver) :: solver
    real :: Xs,Xsmax,Xsmin
    !
    if (verbose) then
      print *,'In bracketSolveForPropertySingle....'
    endif
    param(1) = propspec
    param(2) = real(phase)
    param(3) = real(ic)
    param(4) = dPdT1
    param(5) = T1
    param(6) = P1
    param(7) = Ts
    param(8) = real(propflag)
    param(9) = 0.0
    solver%abs_tol = 1e-8
    Xsmax = max(Ps,P1)
    Xsmin = min(Ps,P1)
    Xs = P1
    call pegasus(Xsmin,Xsmax,propertyFunctionWrapperSingle,Xs,solver,param)
    if (present(ierr)) then
      ierr = solver%exitflag
    else
      if (solver%exitflag /= 0) then
        write(*,*) "Exitflag: ",solver%exitflag
        call stoperror("bracketSolveForProperty failed.")
      endif
    endif
    Ps = Xs
    Ts = param(9)
  end subroutine bracketSolveForPropertySingle

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on saturation line having property value
  !> propspec.
  !>
  !> \author MH 2019-05
  !-----------------------------------------------------------------------------
  function propertyFunctionWrapperSingle(Xs,param) result(fun)
    implicit none
    real, intent(inout) :: Xs
    real, dimension(9), intent(inout) :: param
    real :: fun
    ! Locals:
    integer :: phase
    real, dimension(nc) :: Z
    real, dimension(1) :: X, Xmin, Xmax
    real :: t, prop, propspec, p, T0, P0, dPdT, Tmax
    integer :: propflag, ic
    type(nonlinear_solver) :: solver
    real :: paramSat(4)

    ! Unpack the param vector
    propspec = param(1)
    phase = nint(param(2))
    ic = nint(param(3))
    dPdT = param(4)
    T0 = param(5)
    P0 = param(6)
    Tmax = param(7)
    propflag = nint(param(8))

    ! Composition
    Z = 0
    Z(ic) = 1

    ! Extrapolate for better initial values
    p = Xs
    T = T0 + (P-P0)/dPdT
    X = log(T)

    ! Find point on sat line corresponding to P equal to Xs
    paramSat(1) = specP
    paramSat(3) = Xs
    paramSat(2) = real(ic)
    paramSat(4) = 0.0
    Xmin = log(min(T0,Tmax))
    Xmax = log(max(T0,Tmax))
    X = min(max(X,Xmin),Xmax)
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%max_it = 100
    call nonlinear_solve(solver,sat_fun_single_if,sat_diff_single,&
         sat_diff_single,limit_dx,premReturn,setXv,X,xmin,xmax,paramSat)
    t = exp(X(1))

    ! Update t-slot in param vector with new t value
    param(9) = t

    ! Calculate the property value at the new saturation point
    call genericProperty(t,p,Z,phase,propflag,prop)

    ! Compute new value of objective function
    fun = (prop - propspec)/max(abs(propspec), 1.0)
  end function propertyFunctionWrapperSingle

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on phase envelope having property value
  !> propspec.
  !>
  !> \author MH 2013-10, Ailo 2015-12
  !-----------------------------------------------------------------------------
  function propertyFunctionWrapper(Xs,param) result(fun)
    implicit none
    real, intent(inout) :: Xs
    real, dimension(4*nc+10), intent(inout) :: param
    real :: fun
    ! Locals:
    integer :: iter, s, phase
    real, dimension(nc) :: Z,K
    real, dimension(nc+2) :: X,Xold,dXdS
    real :: t, prop, propspec, beta, ln_spec, p
    integer :: propflag

    ! Unpack the param vector
    propspec = param(1)
    beta = param(2)
    s = int(param(3))
    Z = param(4:nc+3)
    Xold = param(nc+4:2*nc+5)
    dXdS = param(2*nc+6:3*nc+7) ! is constant each time function is called
    X = param(3*nc+8:4*nc+9)
    propflag = int(param(4*nc+10))

    ! Extrapolate for better initial values in sat_newton
    X = Xold + dXdS*(Xs-Xold(s))
    K = exp(X(1:nc))
    t = exp(X(nc+1))
    p = exp(X(nc+2))

    ! Find point on envelope corresponding to ln(property value) equal to Xs
    ln_spec = Xs
    iter = sat_newton(Z,K,t,p,beta,s,ln_spec)
    X(1:nc) = log(K)
    X(nc+1) = log(T)
    X(nc+2) = log(p)

    ! Update Xold-slot in param vector with new X value
    param(3*nc+8:4*nc+9) = X

    ! Get correct phase
    if (beta > 0.5) then
      phase = VAPPH
    else
      phase = LIQPH
    endif

    ! Calculate the property value at the new saturation point
    call genericProperty(t,p,Z,phase,propflag,prop)

    ! Compute new value of objective function
    fun = (prop - propspec)/max(abs(propspec), 1.0)
  end function propertyFunctionWrapper

  subroutine genericProperty(t,p,Z,phase,propflag,prop)
    use eos
    use eosTV, only: pressure
    use trend_solver, only: trend_density
    !$ use omp_lib
    implicit none
    integer, intent(in) :: phase !< Phase identifier
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: Z !< Compozition
    integer, intent(in) :: propflag !< Flag determining what property to return
    real, intent(out) :: prop !< Property
    ! Locals
    real :: v, dpdt, dpdv, p_dummy

    select case(propflag)
    case(locate_from_entropy)
      call entropy(t,p,Z,phase,prop)
    case(locate_from_lnvol)
      call specificVolume(t,p,Z,phase,prop)
      prop = log(prop)
    case(locate_from_enthalpy)
      call enthalpy(t,p,Z,phase,prop)
    case(locate_from_temperature)
      prop = T
    case(locate_from_pressure)
      prop = P
    case(locate_from_joule_thompson)
      call specificVolume(t,p,Z,phase,v)
      p_dummy = pressure(T, v, z, dpdv, dpdt)
      prop = (T*dpdt + v*dpdv)/max(abs(T*dpdt), 1.0)
    end select

  end subroutine genericProperty


  !-----------------------------------------------------------------------------
  !> Locate the points on the phase envelope corresponding to the property values
  !> in prop_grid. The grid must be sorted according to whether one
  !> starts on the dew curve or the bubble curve.
  !>
  !> The routine starts at the dew point at given initial pressure, and then
  !> traverses the phase envelope until it has bracketed a grid value; it then
  !> locates the exact saturation point corresponding to this value, before
  !> continuing.
  !>
  !> This routine can perhaps be generalized to subsume the routines
  !> envelopePlot and isentropeEnvelopeCross.
  !>
  !> To do: test the routine when starting from bubble curve.
  !>
  !> \author Ailo 2015-12
  !> \author Morten 2018-06
  !-----------------------------------------------------------------------------
  subroutine sat_points_based_on_prop(Z,T0,P0,x0,y0,n_grid,propflag,prop_grid,&
       T_grid,P_grid,phase_grid,wi_grid,n_grid_found,&
       dS_override,phase_in,ierr_out,sgn_in,spec_in,&
       tMin,tMax,pMin,pMax,normal_grid,sequential_mode)
    use thermo_utils, only: isSingleComp
    implicit none
    ! Input:
    real, intent(in) :: Z(nc)                  !< total composition
    real, intent(in) :: t0, p0                 !< init. point has pressure p0
    real, dimension(nc), intent(in) :: x0,y0   !< init. liq./vap. compo. guess
    integer, intent(in) :: n_grid              !< number of grid points
    integer, intent(in) :: propflag            !< specified property 1:s, 2:lnv
    real, intent(inout) :: prop_grid(n_grid)   !< property grid (non sequential mode may reorder array)
    real, optional, intent(in) :: dS_override  !< step length along envelope
    integer, optional, intent(in) :: phase_in  !< start search from this phase
    integer, optional, intent(out) :: ierr_out !< error flag; nonzero if error
    real, optional, intent(in) :: sgn_in       !< Override search direction
    integer, optional, intent(in) :: spec_in   !< Override initial search direction
    real, optional, intent(in) :: tMin, tMax   !< Temperature limits (K)
    real, optional, intent(in) :: pMin, pMax   !< Pressure limits (Pa)
    logical, optional, intent(in) :: sequential_mode !< Default sequential
    ! Output:
    real, intent(out) :: T_grid(n_grid)        !< t at the grid points
    real, intent(out) :: P_grid(n_grid)        !< p at the grid points
    integer, intent(out) :: phase_grid(n_grid) !< incumbent phase at grid points
    real, intent(out) :: wi_grid(nc,n_grid)    !< Incipient phase at boundary
    integer, intent(out) :: n_grid_found       !< Number of grid points found
    real, optional, intent(out) :: normal_grid(2,n_grid) !< Normal vector (from tangent) pointing into two-phase region
    ! Locals:
    real :: t, p
    integer :: specification
    real, dimension(nc) :: K, lnfug, lnfugG, lnfugL
    real, dimension(nc+2) :: dXdS, dXdSold
    real, dimension(nc+2) :: Xvar, Xold, Xtemp
    real :: dS, tuning, sgn, Pstart
    integer :: iter,s,n,ophase,ierr,j
    integer, dimension(1) :: smax,zmax
    real :: curr_prop_val, old_prop_val, ln_spec, beta
    logical :: have_switched_formulation, seq_mode
    logical :: should_switch_formulation, reset_dS
    integer :: grid_idx, phase, start_phase, n_grid_not_found, n_found
    real :: sspec ! property value to bracket
    real :: dSmin, dSmax
    real :: prop_grid_local(n_grid), prop_grid_list(n_grid)      !< property grids
    logical :: exit_after_testing, recalculate
    real :: t_c,v_c,p_c,s_c,lnv_c,h_c,crit_prop

    ! Start envelope traversal from dew or bubble curve?
    if ( present(phase_in) ) then
      start_phase = phase_in
    else
      start_phase = VAPPH ! dew curve is default
    end if

    ! Locate critical point and set property at critical point
    call get_crit_prop(Z,t_c,v_c,p_c,s_c,h_c,lnv_c,propflag,crit_prop)

    ! Set mode of operation
    if (present(sequential_mode)) then
      seq_mode = sequential_mode
    else
      seq_mode = .true.
    endif
    if (.not. seq_mode) then
      ! Copy to local prop_grid
      prop_grid_local = prop_grid
      n_grid_not_found = n_grid
    endif

    ! Special consideration for pure fluids
    zmax = maxloc(Z)
    if (isSingleComp(Z)) then
      call sat_points_based_on_prop_single(Z,P0,n_grid,propflag,prop_grid,&
           T_grid,P_grid,phase_grid,n_grid_found,ierr_out)
      do j=1,n_grid_found
        wi_grid(:,j) = Z
      enddo
      return
    endif

    ! Initialize error flag. If something went wrong, set ierr_out /= 0.
    if (present(ierr_out)) ierr_out = 0

    ! Initialize variables needed to solve for initial point on envelope
    if (start_phase == VAPPH) then
      beta = 1.0
    else
      beta = 0.0
    end if
    t = t0
    p = p0
    call thermo(t0,p0,y0,VAPPH,lnfugG)
    call thermo(t0,p0,x0,LIQPH,lnfugL)
    K = exp(lnfugL-lnfugG)

    ! Solve for initial saturation point temperature and equilibrium factors K
    tuning = 1.2
    if (present(sgn_in)) then
      sgn = sgn_in
    else
      sgn = 1.0
    endif
    if (present(spec_in)) then
      specification = spec_in
    else
      specification = specP
    endif
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)
    s = ispec(specification) ! index of fixed variable in Xvar below (here nc+2)
    ln_spec = Xvar(s)
    iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
    if (ierr /= 0) then
      if (present(ierr_out)) then
        ierr_out = ierr
        return
      else
        call stoperror('sat grid: Initial point not found.')
      endif
    endif

    ! Retrieve solved variables
    Xvar(1:nc) = log(K)
    Xvar(nc+1) = log(t)
    Xvar(nc+2) = log(p)
    Pstart = p

    ! Compute property value of initial point on phase envelope
    ! lnv usually decreases
    call genericProperty(t,p,Z,start_phase,propflag,old_prop_val)
    if (.not. present(sgn_in) .and. seq_mode) then
      if ((start_phase == VAPPH .and. old_prop_val < prop_grid(1))) then
        print *, "Initial saturation point T, P0, phase:", t, p, start_phase
        print *, "Initial property value:               ", old_prop_val
        print *, "First property value in grid:         ", prop_grid(1)
        call stoperror("sat grid: Adjust P0 so grid_prop(1) is on dew curve.")
      else if ((start_phase == LIQPH .and. old_prop_val > prop_grid(1))) then
        print *, "Initial saturation point T and P0:", t, p
        print *, "Initial property value: \t", old_prop_val
        print *, "First property value in grid: \t", prop_grid(1)
        call stoperror("sat grid: Adjust P0 so grid_prop(1) is on bubble curve.")
      endif
    endif

    ! When traversing the phase envelope, we may eventually go from the dew
    ! curve to the bubble curve, forcing us to invert what is incumbent and
    ! incipient phase. These variables control this switch.
    have_switched_formulation = .false.
    should_switch_formulation = .false.

    ! Set steplength along phase envelope
    if (present(dS_override)) then
      dSmax = dS_override
      dSmin = min(0.25*dSmax,0.025)
    else
      dSmax = 0.15
      dSmin = 0.025
    endif
    dS = dSmin

    ! Traverse the phase envelope and bracket the property grid values.
    n = 1
    grid_idx = 1
    n_grid_found = 0
    sspec = prop_grid(grid_idx)
    dXdS = 0.0
    exit_after_testing = .false.
    recalculate = .false.
    do while (.true.)
      dXdSold = dXdS
      call newton_extrapolate(Z,Xvar,dXdS,beta,s,ln_spec)
      smax = maxloc(abs(dXdS))
      ! Rescaling the sensitivities
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
      iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
      reset_dS = .false.
      if (ierr /= 0) then
        ! Something went wrong.
        ! Attempt to make the step shorter.
        Xvar = Xold + dXdS*dS*sgn*0.5
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        ln_spec = Xvar(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        reset_dS = .true.
      endif
      if (ierr /= 0) then
        ! Something went wrong.
        ! Attempt to make the step longer.
        Xvar = Xold + dXdS*dS*sgn*2.0
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        ln_spec = Xvar(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        reset_dS = .true.
      endif
      if (ierr /= 0 .and. s > nc) then
        ! Set K-value to try passing critical point
        smax = maxloc(abs(dXdS(1:nc)))
        sgn = sign(1.0,sgn*dXdS(s)*dXdS(smax(1)))
        s = smax(1)
        dXdS = dXdS / dXdS(s)
        if (abs(Xold(s) + dS*sgn) < 1.0e-4) then
          ! Adapt dS to overshoot critical point
          dS = 1.0e-4 + abs(Xold(s))
        endif
        ! Keep pressure and temperature constant
        dXdS(nc+1:nc+2) = 0.0
        Xvar = Xold + dXdS*dS*sgn
        K = exp(Xvar(1:nc))
        t = exp(Xvar(nc+1))
        p = exp(Xvar(nc+2))
        ln_spec = Xvar(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        reset_dS = .true.
      endif
      if (ierr /= 0) then
        if (present(ierr_out)) then
          ierr_out = ierr
          return
        else
          call stoperror("grid error: Neither decreasing nor &
               &increasing the step helped")
        endif
      endif

      if (reset_dS) then
        ! Reset dS
        dS = dSmin
      endif
      Xvar(1:nc) = log(K)
      Xvar(nc+1) = log(t)
      Xvar(nc+2) = log(p)
      n = n + 1

      !Exit at specified minimum temperature
      if (present(tMin)) then
        ! Is temperature decreasing? - And below Tmin?
        if (Xvar(nc+1) - Xold(nc+1) < 0.0 .and. T < Tmin) then
          s = nc+1
          ln_spec = log(Tmin)
          recalculate = .true.
          if (present(ierr_out)) then
            ierr_out = -1
          endif
        endif
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
        exit_after_testing = .true.
      endif

      if (beta > 0.5) then
        phase = VAPPH
      else
        phase = LIQPH
      endif

      call genericProperty(t,p,Z,phase,propflag,curr_prop_val)
      if (seq_mode) then
        ! Check to see if sspec is bracketed.
        if ( sspec >= curr_prop_val .and. sspec <= old_prop_val .OR.&
             sspec <= curr_prop_val .and. sspec >= old_prop_val) then
          call locate_sat_prop(propflag,Z,s,sspec,Xvar,Xold,dXds,beta,ierr)
          if (ierr_demands_return(ierr,ierr_out)) return
          call store_point(Z,Xvar,phase,beta,grid_idx,n_grid,&
               T_grid,P_grid,phase_grid,wi_grid,n_grid_found,&
               normal_grid)
          if ( grid_idx > n_grid ) then
            return
          else
            curr_prop_val = sspec
            sspec = prop_grid(grid_idx)
          endif
        endif
      else ! .not. seq_mode
        ! Check to see if some of the specifications are bracketed.
        if ( isAnyPropBracketed(curr_prop_val,old_prop_val,n_grid,prop_grid_local,&
             n_grid_not_found,prop_grid_list,n_found)) then
          do j=1,n_found
            sspec = prop_grid_list(j)
            call locate_saturation_prop_point(Z,phase,s,sspec,&
                 propflag,crit_prop,curr_prop_val,old_prop_val,&
                 Xvar,Xold,dXds,beta,t_c,p_c,Xtemp,ierr)
            if (ierr_demands_return(ierr,ierr_out)) return
            prop_grid(grid_idx) = sspec ! Reorder array
            call store_point(Z,Xtemp,phase,beta,grid_idx,n_grid,&
                 T_grid,P_grid,phase_grid,wi_grid,n_grid_found,&
                 normal_grid)
          enddo
          if ( n_grid_found == n_grid ) then
            return
          endif
        endif
      endif
      ! update old_prop_val and continue
      old_prop_val = curr_prop_val

      if (exit_after_testing) then
        return
      endif
      ! Abort if we are outside boundaries
      if (present(tMax)) then
        if (t > tMax) then
          if (present(ierr_out)) then
            ierr_out = -1
          endif
          return
        endif
      endif
      if (present(pMin)) then
        if (p < pMin) then
          if (present(ierr_out)) then
            ierr_out = -1
          endif
          return
        endif
      endif
      if (present(pMax)) then
        if (p > pMax) then
          if (present(ierr_out)) then
            ierr_out = -1
          endif
          return
        endif
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
      if (iter < 3) then
        dS = dS * tuning
      else if (iter > 5) then
        dS = dS * (1.0 - tuning)
      endif
      dS = max(min(dS,dSmax),dSmin)
    enddo
  contains
    function ierr_demands_return(ierr,ierr_out) result(do_return)
      integer, intent(in) :: ierr
      integer, optional, intent(out) :: ierr_out
      !
      Logical :: do_return
      do_return = .false.
      if (ierr /= 0) then
        if (present(ierr_out)) then
          if (ierr == 100) then
            ierr_out = ierr
            do_return = .true.
          else
            ! Accept tolerance error:
            ierr_out = 0
            if (ierr /= 100) then
              print *,'sat_points_based_on_prop:: Tolerance not met for bracket solver. Continuing'
            endif
          endif
        else
          if (ierr == 100) then
            print *,'sat_points_based_on_prop:: No solution for bracket solver'
            call stoperror("grid error: Bracket solver failed!")
          else
            print *,"grid error: Bracket solver failed! ierr=",ierr
          endif
        endif
      endif
    end function ierr_demands_return
  end subroutine sat_points_based_on_prop

  !-----------------------------------------------------------------------------
  !> Locate property on saturation line
  !!
  !! \author Morten Hammer 2020-01
  !-----------------------------------------------------------------------------
  subroutine locate_saturation_prop_point(Z,phase,s,sspec,&
       propflag,crit_prop,curr_prop_val,old_prop_val,&
       Xvar,Xold,dXds_in,beta,T_c,p_c,Xtemp,ierr)
    ! Input
    real, intent(in) :: Z(nc)                  !< total composition
    integer, intent(in) :: propflag            !< specified property 1:s, 2:lnv
    integer, intent(in) :: phase   !<
    real, intent(in) :: sspec  !<
    integer, intent(in) :: s   !<
    real, intent(in) :: crit_prop,curr_prop_val,old_prop_val !<
    real, intent(in) :: beta  !<
    real, intent(in) :: Xvar(nc+2), Xold(nc+2), dXdS_in(nc+2)  !<
    real, intent(in) :: t_c  !<
    real, intent(in) :: p_c  !<
    ! Output
    real, intent(out) :: Xtemp(nc+2)
    integer, intent(out) :: ierr   !<
    ! Locals
    integer :: l, iter
    real :: dsc, Xc, iter_prop_val, temp_prop_val, ln_spec
    logical :: critIsCrossed, brackets_solution
    real, dimension(nc+2) :: XtempOld, dXds
    real :: t, p
    real, dimension(nc) :: K
    critIsCrossed = ((crit_prop-curr_prop_val)*(crit_prop-old_prop_val) < 0)
    !critIsCrossed .false.
    if (critIsCrossed) then
      if (s <= nc) then
        Xc = 0.0
      elseif (s == nc + 1) then
        Xc = log(T_c)
      else!if (s == nc + 2) then
        Xc = log(P_c)
      endif
      if ((crit_prop-curr_prop_val)*(crit_prop-sspec) > 0) then
        ! Located on curr_prop_val side
        Xtemp = Xvar
        temp_prop_val = curr_prop_val
      else
        ! Located on old_prop_val side
        Xtemp = Xold
        temp_prop_val = old_prop_val
      endif
      XtempOld = Xtemp
      ! Approach critical point
      brackets_solution = .false.
      do l=1,5
        call newton_extrapolate(Z,Xtemp,dXdS,beta,s,ln_spec)
        dsc = (Xc - Xtemp(s))*0.5**l
        Xtemp = Xtemp + dXdS*dSc
        K = exp(Xtemp(1:nc))
        t = exp(Xtemp(nc+1))
        p = exp(Xtemp(nc+2))
        ln_spec = Xtemp(s)
        iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
        if (ierr /= 0) then
          ! Reset and include critical point in search
          Xtemp = XtempOld
          exit
        endif
        Xtemp(1:nc) = log(K)
        Xtemp(nc+1) = log(t)
        Xtemp(nc+2) = log(p)
        call genericProperty(t,p,Z,phase,propflag,iter_prop_val)
        if ((temp_prop_val-sspec)*(iter_prop_val-sspec) < 0) then
          brackets_solution = .true.
          exit
        endif
      enddo
      if (.not. brackets_solution) then
        ! Fallback to search between Xold and Xvar
        XtempOld = Xold
        Xtemp = Xvar
      endif
    else
      Xtemp = Xvar
      XtempOld = Xold
      dXds = dXds_in
    endif
    call locate_sat_prop(propflag,Z,s,sspec,Xtemp,XtempOld,dXds,beta,ierr)
  end subroutine locate_saturation_prop_point

  !-----------------------------------------------------------------------------
  !> Locate critical point and set property at critical point
  !!
  !! \author Morten Hammer 2020-01
  !-----------------------------------------------------------------------------
  subroutine get_crit_prop(Z,t_c,v_c,p_c,s_c,h_c,lnv_c,propflag,crit_prop)
    use critical, only: calcCriticalTV, calcCritical
    use eosTV, only: enthalpy_tv, entropy_tv
    use eos, only: specificvolume
    ! Input
    real, intent(in) :: Z(nc)
    integer, intent(in) :: propflag
    ! Output
    real, intent(out) :: t_c,v_c,p_c,s_c,h_c,lnv_c,crit_prop
    ! Locals
    integer :: nenv, ierr
    integer, parameter :: n_max = 500
    real :: Ta(n_max), pa(n_max), Ki(n_max,nc)
    real :: betai(n_max)
    real :: crit(2)
    type(thermo_model), pointer :: act_mod_ptr

    act_mod_ptr => get_active_thermo_model()
    ! Locate critical point and set property at critical point
    if (act_mod_ptr%EoSlib /= TREND) then
      t_c = 0.0
      v_c = 0.0
      call calcCriticalTV(t_c,v_c,Z,ierr,tol=1.0e-7,p=p_c)
    else
      ierr = 0
      ! Get phase envelope, including critical point in T,p
      call envelopePlot(z,300.0,1.0e5,spec=1,beta_in=1.0,Pmax=2.0e7,nmax=n_max,&
           Ta=Ta,pa=pa,Ki=Ki,betai=betai,n=nenv,crit=crit)
      T_c = crit(1)
      p_c = crit(2)
      call specificvolume(t_c,p_c,Z,LIQPH,v_c)
    endif
    if (ierr == 0) then
      lnv_c = log(v_c)
      call entropy_tv(t_c,v_c,z,s_c)
      call enthalpy_tv(t_c,v_c,z,h_c)
    else
      ! Set un-physical values
      lnv_c = 0.0
      s_c = 1.0e20
      h_c = 1.0e20
      p_c = 0.0
      t_c = 0.0
    endif
    select case(propflag)
    case(locate_from_entropy)
      crit_prop = s_c
    case(locate_from_lnvol)
      crit_prop = lnv_c
    case(locate_from_enthalpy)
      crit_prop = h_c
    case(locate_from_temperature)
      crit_prop = t_c
    case(locate_from_pressure)
      crit_prop = p_c
    end select
  end subroutine get_crit_prop

  !-----------------------------------------------------------------------------
  !> Locate the points on pure fluid saturation line corresponding to
  !! the property values in prop_grid.
  !!
  !! \author Morten 2019-05
  !-----------------------------------------------------------------------------
  subroutine sat_points_based_on_prop_single(Z,P0,n_grid,propflag,prop_grid,&
       T_grid,P_grid,phase_grid,n_grid_found,ierr_out)
    use eos, only: getCriticalParam
    implicit none
    ! Input:
    real, intent(in) :: Z(nc)                  !< total composition
    real, intent(in) :: p0                     !< init. point has pressure p0
    integer, intent(in) :: n_grid              !< number of grid points
    integer, intent(in) :: propflag            !< specified property 1:s, 2:lnv
    real, intent(inout) :: prop_grid(n_grid)   !< property grid (non sequential mode may reorder array)
    integer, optional, intent(out) :: ierr_out !< error flag; nonzero if error
    ! Output:
    real, intent(out) :: T_grid(n_grid)        !< t at the grid points
    real, intent(out) :: P_grid(n_grid)        !< p at the grid points
    integer, intent(out) :: phase_grid(n_grid) !< incumbent phase at grid points
    integer, intent(out) :: n_grid_found       !< Number of grid points found
    ! Locals:
    real :: t, p, Ps, Ts
    integer, dimension(1) :: zmax
    real, dimension(nc) :: X,Y
    real, dimension(4) :: param
    real, dimension(1) :: XX,Xmax,Xmin
    type(nonlinear_solver) :: solver
    integer :: ic, iter, ierr, j
    real :: f,dfdt,dfdp,tci,pci,oi,dpdt,dP
    integer , parameter :: nmax = 200
    real :: Ta(nmax*2), Pa(nmax*2), propa(nmax*2)
    integer :: phasea(nmax*2)
    real :: curr_prop_val, old_prop_val, crit_prop_val
    integer :: grid_idx, n_grid_not_found, n_found
    real :: sspec ! property value to bracket
    real :: prop_grid_local(n_grid), prop_grid_list(n_grid)      !< property grids

    ! Copy to local prop_grid
    prop_grid_local = prop_grid
    n_grid_not_found = n_grid

    ! Initialize error flag. If something went wrong, set ierr_out /= 0.
    if (present(ierr_out)) ierr_out = 0

    ! Initialize composition
    X = 0.0
    zmax = maxloc(Z)
    ic = zmax(1)
    X(ic) = 1.0
    Y=X

    ! Initialize variables needed to solve for initial point on envelope
    t = safe_bubT(p0,X,Y,ierr_out)

    call getCriticalParam(ic,tci,pci,oi)
    ! Find initlal point
    p = p0
    dP = (pci-P)/nmax
    xmax = log(tci)
    xmin = log(t)

    ! Initial point
    Ta(1) = t
    Pa(1) = p
    phasea(1) = VAPPH
    call genericProperty(t,p,Z,VAPPH,propflag,old_prop_val)
    propa(1) = old_prop_val
    Ta(nmax*2) = t
    Pa(nmax*2) = p
    phasea(nmax*2) = LIQPH
    call genericProperty(t,p,Z,LIQPH,propflag,curr_prop_val)
    propa(nmax*2) = curr_prop_val

    ! Critical point
    call genericProperty(tci,pci,X,LIQPH,propflag,crit_prop_val)
    Ta(nmax) = tci
    Pa(nmax) = pci
    phasea(nmax) = VAPPH
    propa(nmax) = crit_prop_val
    Ta(nmax+1) = tci
    Pa(nmax+1) = pci
    phasea(nmax+1) = LIQPH
    propa(nmax+1) = crit_prop_val

    ! Solver paramaters
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%max_it = 10000
    param(1) = specP
    param(2) = ic
    param(4) = 0.0
    do iter = 2,nmax-1
      call sat_fun_single(ic,t,p,f,dfdt,dfdp,.false.)
      dpdt = -dfdt/dfdp
      param(3) = p0 + dP*(iter-1)
      XX(1) = log(t+dP/dpdt)
      call nonlinear_solve(solver,sat_fun_single_if,sat_diff_single,&
           sat_diff_single,limit_dx,premReturn,setXv,XX,xmin,xmax,param)
      if (solver%exitflag == 0) then
        T = exp(XX(1))
        P = param(3)
      else
        if (present(ierr_out)) then
          ierr_out = solver%exitflag
          return
        else
          call stoperror("sat_points_based_on_prop_single: Not able to locate point on saturation line")
        endif
      endif
      Ta(iter) = t
      Pa(iter) = p
      phasea(iter) = VAPPH
      call genericProperty(t,p,X,VAPPH,propflag,curr_prop_val)
      propa(iter) = curr_prop_val
      Ta(2*nmax-iter+1) = t
      Pa(2*nmax-iter+1) = p
      phasea(2*nmax-iter+1) = LIQPH
      call genericProperty(t,p,X,LIQPH,propflag,curr_prop_val)
      propa(2*nmax-iter+1) = curr_prop_val
    enddo

    ! Traverse the saturation line and bracket the property grid values.
    grid_idx = 1
    n_grid_found = 0
    old_prop_val = propa(1)
    do iter = 2,2*nmax
      curr_prop_val = propa(iter)
      ! Check to see if some of the specifications are bracketed.
      if ( isAnyPropBracketed(curr_prop_val,old_prop_val,n_grid,prop_grid_local,&
           n_grid_not_found,prop_grid_list,n_found)) then
        do j=1,n_found
          sspec = prop_grid_list(j)
          Ts=Ta(iter)
          Ps=Pa(iter)
          call locate_sat_prop_single(propflag,ic,sspec,&
               Ta(iter-1),Pa(iter-1),phasea(iter-1),Ts,Ps,ierr)
          !print *,iter,ierr,Ts,Ps,phasea(iter-1)
          if (ierr /= 0) then
            if (present(ierr_out)) then
              ierr_out = ierr
              return
            else
              if (ierr == 100) then
                print *,'No solution for bracket solver'
              else
                print *,'ierr',ierr
              endif
              call stoperror("grid error: Bracket solver failed!")
            endif
          endif
          prop_grid(grid_idx) = sspec ! Reorder array
          call store_point_single(Z,Ts,Ps,phasea(iter),grid_idx,n_grid,&
               T_grid,P_grid,phase_grid,n_grid_found)
          if ( n_grid_found == n_grid ) then
            return
          end if
        enddo
      endif
      old_prop_val = curr_prop_val
    enddo
  end subroutine sat_points_based_on_prop_single

  !-----------------------------------------------------------------------------
  !> Locate property - envelope intersect
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine locate_sat_prop(propflag,Z,s,sspec,Xvar,Xold,dXds,beta,ierr)
    implicit none
    integer, intent(in) :: propflag
    integer, intent(in) :: s
    real, intent(in) :: Z(nc)
    real, intent(in) :: sspec
    real, intent(in) :: beta
    real, intent(inout) :: Xvar(nc+2),Xold(nc+2),dXds(nc+2)
    integer, intent(out) :: ierr
    ! Locals
    integer :: sl, iter
    real :: K(nc), ln_spec, p, t
    if ( propflag == locate_from_temperature .OR. &
         propflag == locate_from_pressure) then
      if (propflag == locate_from_temperature) then
        sl = nc+1
      else
        sl = nc+2
      endif
      ln_spec = log(sspec)
      Xvar = Xold + dXdS*(ln_spec - Xold(sl))
      K = exp(Xvar(1:nc))
      t = exp(Xvar(nc+1))
      p = exp(Xvar(nc+2))
      iter = sat_newton(Z,K,t,p,beta,sl,ln_spec,ierr)
    else
      ! sspec is bracketed, so solve for exact property value and return
      call bracketSolveForProperty(n=nc+2,Z=Z,beta=beta,propflag=propflag,&
           propspec=sspec,X=Xvar,Xold=Xold,dXdS=dXdS,s=s,ierr=ierr)
      ! Restart at located point
      !dS = dSmin
    endif
  end subroutine locate_sat_prop

  !-----------------------------------------------------------------------------
  !> Locate property - saturation line intersect
  !>
  !> \author MH, 2019-05
  !-----------------------------------------------------------------------------
  subroutine locate_sat_prop_single(propflag,ic,sspec,T1,P1,phase,Ts,Ps,ierr)
    implicit none
    integer, intent(in) :: propflag
    integer, intent(in) :: ic, phase
    real, intent(in) :: T1,P1
    real, intent(in) :: sspec
    real, intent(inout) :: Ts,Ps
    integer, intent(out) :: ierr
    ! Locals
    real :: f,dfdt,dfdp, p, t, dpdt, dT, dP
    real :: XX(1), param(4), xmax(1), xmin(1)
    type(nonlinear_solver) :: solver
    t = T1
    p = p1
    call sat_fun_single(ic,t,p,f,dfdt,dfdp,.false.)
    dpdt = -dfdt/dfdp
    if ( propflag == locate_from_temperature .OR. &
         propflag == locate_from_pressure) then
      if (propflag == locate_from_temperature) then
        param(1) = specT
        param(3) = sspec
        dT = sspec - t
        xmin(1) = log(min(P1,Ps))
        xmax(1) = log(max(P1,Ps))
        XX(1) = log(p+dT*dpdt)
      else
        param(1) = specP
        param(3) = sspec
        dP = sspec - p
        xmin(1) = log(min(T1,Ts))
        xmax(1) = log(max(T1,Ts))
        XX(1) = log(t+dP/dpdt)
      endif
      XX(1) = min(max(XX(1),xmin(1)),xmax(1))
      param(2) = ic
      param(4) = 0.0
      ! Solver paramaters
      solver%rel_tol = 1.0e-20
      solver%abs_tol = 1.0e-10
      solver%max_it = 100
      call nonlinear_solve(solver,sat_fun_single_if,sat_diff_single,&
           sat_diff_single,limit_dx,premReturn,setXv,XX,xmin,xmax,param)
      ierr = solver%exitflag
      if (ierr == 0) then
        if (propflag == locate_from_temperature) then
          Ts = sspec
          Ps = exp(XX(1))
        else
          Ps = sspec
          Ts = exp(XX(1))
        endif
      endif
    else
      ! sspec is bracketed, so solve for exact property value and return
      call bracketSolveForPropertySingle(ic,propflag,sspec,phase,&
           T1,P1,dPdT,Ts,Ps,ierr)
    endif
  end subroutine locate_sat_prop_single

  !-----------------------------------------------------------------------------
  !> Store point in output vector
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine store_point(Z,Xvar,phase,beta,grid_idx,n_grid,&
       T_grid,P_grid,phase_grid,wi_grid,n_grid_found,&
       normal_grid)
    use solid_saturation, only: sat_diff_newton_threePh
    implicit none
    ! Input:
    real, dimension(nc), intent(in) :: Z       !< Overall composition
    real, dimension(nc+2) :: Xvar              !< Variable vector
    real, intent(in) :: beta                   !< Two-phase gas phase fraction (mol/mol)
    integer, intent(in) :: phase               !< Single phase at envelope crossing
    integer, intent(inout) :: grid_idx         !< Current index, increased by one before exit
    integer, intent(in) :: n_grid              !< number of grid points
    ! Output:
    real, intent(out) :: T_grid(n_grid)        !< t at the grid points
    real, intent(out) :: P_grid(n_grid)        !< p at the grid points
    integer, intent(out) :: phase_grid(n_grid) !< incumbent phase at grid points
    real, intent(out) :: wi_grid(nc,n_grid)    !< Incipient phase at boundary
    integer, intent(out) :: n_grid_found       !< Number of grid points found
    real, optional, intent(out) :: normal_grid(2,n_grid) !< Normal vector (from tangent) pointing into two-phase region
    ! Locals:
    real :: K(nc), tangent(2), normal(2), dlnTdX(2), dlnPdX(2), dbeta(2)
    real, dimension(nc+3) :: Xvar3ph !< Variable vector
    real, dimension(nc+3,nc+3) :: Jac3ph !< Function differentials
    real, dimension(nc+5) :: param3ph !< Parameter vector
    real, dimension(nc+2,2) :: rhs !< Variable vector
    real, dimension(nc+2,nc+2) :: Jac2ph !< Function differentials
    !real, dimension(nc+5) :: param3ph !< Parameter vector
    integer, dimension(nc+2) :: INDX
    integer :: INFO

    K = exp(Xvar(1:nc))
    T_grid(grid_idx) = exp(Xvar(nc+1))
    P_grid(grid_idx) = exp(Xvar(nc+2))
    phase_grid(grid_idx) = phase
    if (beta > 0.5) then
      wi_grid(:,grid_idx) = Z/(1-beta+beta*K)
    else
      wi_grid(:,grid_idx) = K*Z/(1-beta+beta*K)
    endif
    if (present(normal_grid)) then
      Xvar3ph(1:nc+2) = Xvar
      Xvar3ph(nc+3) = beta
      param3ph(1:nc) = z
      param3ph(nc+1) = 0.0  ! Solid fraction
      param3ph(nc+2) = nc+2 ! Const P
      param3ph(nc+3) = 0.0  ! Dummy spec.
      param3ph(nc+4) = real(VARFLUID_DUMMY)
      param3ph(nc+5) = 1.0 ! Dummy solid component
      call sat_diff_newton_threePh(Jac3ph,Xvar3ph,param3ph)
      Jac2ph = Jac3ph(1:nc+2,1:nc+2)
      rhs(:,1) = -Jac3ph(1:nc+2,nc+3)
      rhs(:,2) = 0.0
      rhs(nc+2,2) = 1.0 ! Perturbation in pressure
      ! Solve equation system
      call DGESV( nc+2, 2, Jac2ph, nc+2, INDX, rhs, nc+2, INFO )
      dlnTdX(1) = rhs(nc+1,2) ! dlnT_dlnP
      dlnTdX(2) = rhs(nc+1,1) ! dlnT_dbeta
      !
      Jac2ph = -Jac3ph(1:nc+2,1:nc+2)
      Jac2ph(nc+2,nc+2) = 0.0
      Jac2ph(nc+2,nc+1) = 1.0 ! Fixate temperature
      rhs(:,1) = Jac3ph(1:nc+2,nc+3)
      rhs(:,2) = 0.0
      rhs(nc+2,2) = 1.0 ! Perturbation in temperature
      ! Solve equation system
      call DGESV( nc+2, 2, Jac2ph, nc+2, INDX, rhs, nc+2, INFO )
      dlnPdX(1) = rhs(nc+2,2) ! dlnP_dlnT
      dlnPdX(2) = rhs(nc+2,1) ! dlnP_dbeta
      !
      if (dlnPdX(1) > dlnTdX(1)) then
        tangent = (/dlnTdX(1), 1.0/)
      else
        tangent = (/1.0, dlnPdX(1)/)
      endif
      normal(1) = - tangent(2)
      normal(2) = tangent(1)
      dbeta(1) = 1.0/dlnTdX(2)
      dbeta(2) = 1.0/dlnPdX(2)
      if (beta > 0.5 .and. sum(normal*dbeta) > 0.0) then
        normal = - normal
      else if (beta < 0.5 .and. sum(normal*dbeta) < 0.0) then
        normal = - normal
      endif
      normal = normal/sqrt(dot_product(normal,normal))
      normal_grid(:,grid_idx) = normal
    endif
    n_grid_found = grid_idx
    grid_idx = grid_idx + 1
  end subroutine store_point

  !-----------------------------------------------------------------------------
  !> Store point in output vector for pure fluid
  !>
  !> \author MH, 2019-05
  !-----------------------------------------------------------------------------
  subroutine store_point_single(Z,T,P,phase,grid_idx,n_grid,&
       T_grid,P_grid,phase_grid,n_grid_found)
    implicit none
    ! Input:
    real, dimension(nc), intent(in) :: Z       !< Overall composition
    real, intent(in)                :: T,P     !<
    integer, intent(in) :: phase               !< Single phase at envelope crossing
    integer, intent(inout) :: grid_idx         !< Current index, increased by one before exit
    integer, intent(in) :: n_grid              !< number of grid points
    ! Output:
    real, intent(out) :: T_grid(n_grid)        !< t at the grid points
    real, intent(out) :: P_grid(n_grid)        !< p at the grid points
    integer, intent(out) :: phase_grid(n_grid) !< incumbent phase at grid points
    integer, intent(out) :: n_grid_found       !< Number of grid points found
    ! Locals:

    T_grid(grid_idx) = T
    P_grid(grid_idx) = P
    phase_grid(grid_idx) = phase
    n_grid_found = grid_idx
    grid_idx = grid_idx + 1
  end subroutine store_point_single

  !-----------------------------------------------------------------------------
  !> Test numdiff beta
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine num_diff_beta(Z,Xvar,beta,dbeta)
    implicit none
    ! Input:
    real, dimension(nc), intent(in) :: Z       !< Overall composition
    real, dimension(nc+2) :: Xvar              !< Variable vector
    real, intent(in) :: beta                   !< Two-phase gas phase fraction (mol/mol)
    ! Output:
    real, intent(out) :: dbeta(2)              !< dbeta
    ! Locals:
    real :: K(nc), t0, p0, t, p, beta_eps, db, ln_spec
    real, parameter :: eps = 1.0e-5
    integer :: ierr, iter, s
    K = exp(Xvar(1:nc))
    t0 = exp(Xvar(nc+1))
    p0 = exp(Xvar(nc+2))
    ln_spec = Xvar(nc+2)
    s = nc+2
    iter = sat_newton(Z,K,t0,p0,beta,s,ln_spec,ierr)
    t = t0
    p = p0
    if (beta < 0.5) then
      db = eps
    else
      db = - eps
    endif
    beta_eps = beta + db
    iter = sat_newton(Z,K,t,p,beta_eps,s,ln_spec,ierr)
    dbeta(1) = db/(log(t) - log(t0))
    ln_spec = Xvar(nc+1)
    s = nc+1
    iter = sat_newton(Z,K,t,p,beta_eps,s,ln_spec,ierr)
    dbeta(2) = db/(log(p) - log(p0))
  end subroutine num_diff_beta

  !-----------------------------------------------------------------------------
  !> TNumerical dlntdlnp
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine num_diff_dlntdlnp(Z,Xvar,beta,dlntdlnp)
    implicit none
    ! Input:
    real, dimension(nc), intent(in) :: Z       !< Overall composition
    real, dimension(nc+2) :: Xvar              !< Variable vector
    real, intent(in) :: beta                   !< Two-phase gas phase fraction (mol/mol)
    ! Output:
    real, intent(out) :: dlntdlnp              !< dlntdlnp
    ! Locals:
    real :: K(nc), t0, p0, t, p, ln_spec
    real, parameter :: eps = 1.0e-5
    integer :: ierr, iter, s
    K = exp(Xvar(1:nc))
    t0 = exp(Xvar(nc+1))
    p0 = exp(Xvar(nc+2))
    ln_spec = Xvar(nc+2)
    s = nc+2
    iter = sat_newton(Z,K,t0,p0,beta,s,ln_spec,ierr)
    p = p0 + p0*eps
    ln_spec = log(p)
    t = t0
    iter = sat_newton(Z,K,t,p,beta,s,ln_spec,ierr)
    dlntdlnp = (log(t) - log(t0))/(log(p) - log(p0))
  end subroutine num_diff_dlntdlnp

  !-----------------------------------------------------------------------------
  !> Iso-line crossing of saturation line
  !>
  !> \author MH, 2017-06
  !-----------------------------------------------------------------------------
  subroutine iso_cross_saturation_line(ts,ps,t,p,X,Y,Z,beta,&
       prop_spec,prop_specID,phase,ierr)
    use stringmod, only: str_eq
    use eos, only: enthalpy, entropy
    use saturation_curve, only: extrapolate_to_saturation_line
    implicit none
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: ts !< Single phase temperature (K)
    real, intent(in) :: ps !< Single phase pressure (Pa)
    real, intent(inout) :: t !< Two-phase temperature (K)
    real, intent(inout) :: p !< Two-phase pressure (Pa)
    real, intent(inout) :: beta !< Two-phase gas phase fraction (mol/mol)
    real, dimension(nc), intent(in) :: X !< Liquid composition
    real, dimension(nc), intent(in) :: Y !< Gas composition
    real, intent(in) :: prop_spec ! Enthalpy, entropy or temperature
    character, intent(in) :: prop_specID !< "h", "s", "t", "p"
    integer, intent(out) :: phase !< Single phase at envelope crossing
    integer, intent(out) :: ierr !< Error indicator, zero on success
    ! Locals
    real :: dpds_sat, dtds_sat, h, dhdt, dhdp, s, dsdt, dsdp
    real :: dtdp, dpdt, dh, ds, dT, dP, sgn
    integer :: extrap ! 1 = isobaric, 2 = isothermal
    integer :: spec, propflag
    integer, parameter :: n_grid = 1
    real :: T_grid(n_grid), P_grid(n_grid), prop_grid(n_grid)
    real :: wi_grid(nc,n_grid)
    integer :: phase_grid(n_grid), n_grid_found
    real, dimension(nc) :: X_cpy, Y_cpy
    real :: tMax, tMin, pMax, pMin
    ! How to extrapolate?
    if (str_eq(prop_specID,"T")) then
      extrap = ISO_T ! isothermal
    else if (str_eq(prop_specID,"P")) then
      extrap = ISO_P ! isobaric
    else if (str_eq(prop_specID,"S")) then
      extrap = ISO_S ! isentropic
    else if (str_eq(prop_specID,"H")) then
      extrap = ISO_H ! isenthalpic
    else
      print *,"Wrong extrapolation variable specified"
      return
    endif
    X_cpy = X
    Y_cpy = Y
    call extrapolate_to_saturation_line(t,p,X_cpy,Y_cpy,Z,beta,&
         extrap,ierr,dtds_sat,dpds_sat)
    if (ierr == 0) then
      if (abs(dtds_sat)/t > abs(dpds_sat)/p) then
        spec = specT
        dpdt = dpds_sat/dtds_sat
      else
        spec = specP
        dtdp = dtds_sat/dpds_sat
      endif
      if (beta > 0.5) then
        phase = VAPPH
      else
        phase = LIQPH
      endif
      if (prop_specID == "h" .or. prop_specID == "H") then
        propflag = locate_from_enthalpy
        call enthalpy(T,P,Z,phase,h,dhdt,dhdp)
        if (spec == specT) then
          dh = dhdt + dhdp*dpdt
          dT = (prop_spec-h)/dh
          sgn = sign(1.0,dT)
        else ! spec == specP
          dh = dhdt*dtdp + dhdp
          dP = (prop_spec-h)/dh
          sgn = sign(1.0,dP)
        endif
      else if (prop_specID == "s" .or. prop_specID == "S") then
        propflag = locate_from_entropy
        call entropy(T,P,Z,phase,s,dsdt,dsdp)
        if (spec == specT) then
          ds = dsdt + dsdp*dpdt
          dT = (prop_spec-s)/ds
          sgn = sign(1.0,dT)
        else ! spec == specP
          ds = dsdt*dtdp + dsdp
          dP = (prop_spec-s)/ds
          sgn = sign(1.0,dP)
        endif
      else
        ! Isotherm and isobar
        if ( (extrap == 2 .and. abs(prop_spec - T)/prop_spec < machine_prec*1.0e7) .or.&
             (extrap == 1 .and. abs(prop_spec - P)/prop_spec < machine_prec*1.0e7)) then
          ! Converged
          return
        else
          ! Need to search for T or P
          if (extrap == 2) then
            propflag = locate_from_temperature
            if (spec == specT) then
              sgn = sign(1.0, prop_spec - T)
            else !spec == specP
              sgn = sign(1.0, (prop_spec - T)*dpdt)
            endif
          else !extrap == 1
            propflag = locate_from_pressure
            if (spec == specP) then
              sgn = sign(1.0, prop_spec - P)
            else !spec == specT
              sgn = sign(1.0, (prop_spec - P)*dtdp)
            endif
          endif
        endif
      endif
      ! Search for property on envelope
      prop_grid = prop_spec
      tMax = ts + 5.0
      tMin = ts - 5.0
      pMax = ps + 5.0e5
      pMin = max(ps - 5.0e5, 1.0e5)
      call sat_points_based_on_prop(Z,T,P,X_cpy,Y_cpy,n_grid,&
           propflag,prop_grid,T_grid,P_grid,phase_grid,&
           wi_grid,n_grid_found,&
           0.05,phase,ierr,sgn,spec,tMin=tMin,tMax=tMax,&
           pMin=pMin,pMax=Pmax)
      T = T_grid(1)
      P = P_grid(1)
      phase = phase_grid(1)
    endif
  end subroutine iso_cross_saturation_line


  !-----------------------------------------------------------------------------
  !> Locate the points on the sublimation line corresponding to the property values
  !! in prop_grid.
  !!
  !! The routine starts at the "triple" point, and then
  !! traverses the sublimation line until it has bracketed one or more grid value; it then
  !! locates the exact sublimation point corresponding to this value, before
  !! continuing.
  !!
  !! \author Morten 2018-06
  !-----------------------------------------------------------------------------
  subroutine subl_points_based_on_prop(Z,Ttr,Ptr,Pmin,n_grid,propflag,prop_grid,&
       T_grid,P_grid,n_grid_found,ierr_out)
    use solideos, only: solidComp, nSolid
    implicit none
    ! Input:
    real, intent(in) :: Z(nc)                  !< total composition
    real, intent(in) :: ttr, ptr               !< triple point
    integer, intent(in) :: n_grid              !< number of grid points
    integer, intent(in) :: propflag            !< specified property 1:s, 2:lnv
    real, intent(inout) :: prop_grid(n_grid)   !< property grid
    real, intent(in) :: pMin                   !< Pressure limit (Pa)
    ! Output:
    real, intent(out) :: T_grid(n_grid)        !< t at the grid points
    real, intent(out) :: P_grid(n_grid)        !< p at the grid points
    integer, intent(out) :: n_grid_found       !< gird points found
    integer, optional, intent(out) :: ierr_out !< error flag; nonzero if error
    ! Locals:
    real :: t, p
    integer :: i,iter,is,ierr,s,phase,j
    real :: curr_prop_val, old_prop_val, ln_spec, betaSol, dln_s, pn, sspec
    real, dimension(2) :: Xvar, Xold, dXds, Xtemp
    integer :: n_found, nsol, n_grid_not_found
    real, parameter :: dPmax = 0.2e5
    real :: prop_grid_local(n_grid), prop_grid_list(n_grid)      !< property grids
    ! Mapping solid-gas line starting at triple point
    if (nSolid /= 1) then
      call stoperror('saturation::solidEnvelopePlot: No solid model initialized!')
    else
      is = solidComp(1)
    endif
    ! Initial state
    t = Ttr
    p = Ptr
    phase = VAPPH

    ! Compute property value of initial point on sublimation line
    call genericProperty(t,p,Z,phase,propflag,old_prop_val)
    prop_grid_local = prop_grid
    n_grid_found = 0
    n_grid_not_found = n_grid

    betaSol = 0.0
    s = 2 ! Specify pressure
    nsol = max(int((Ptr-Pmin)/dPmax)+1,50)
    do i=1,nsol
      Xold = (/ log(t), log(p)/)
      pn = Ptr - (Ptr-Pmin)*real(i-1)/real(nsol-1)
      ln_spec = log(pn)
      dln_s = ln_spec - log(p)
      call newton_extrapolate_solid(Z,t,p,betaSol,VAPPH,s,is,ln_spec,dln_s,dXds)
      iter = solidPointOnEnvelope(Z,t,p,phase,betaSol,is,s,ln_spec,ierr)
      if (ierr /= 0) then
        if (present(ierr_out)) then
          ierr_out = ierr
          return
        else
          call stoperror("grid error: Not able to solve for sublimation line point")
        endif
      endif
      Xvar = (/ log(t), log(p)/)
      call genericProperty(t,p,Z,phase,propflag,curr_prop_val)

      ! Check to see if sspec is bracketed.
      if ( isAnyPropBracketed(curr_prop_val,old_prop_val,n_grid,prop_grid_local,&
           n_grid_not_found,prop_grid_list,n_found)) then
        do j=1,n_found
          sspec = prop_grid_list(j)
          if ( propflag == locate_from_temperature .OR. &
               propflag == locate_from_pressure) then
            if (propflag == locate_from_temperature) then
              s = 1
            else
              s = 2
            endif
            ln_spec = log(sspec)
            Xtemp = Xold + dXdS*(ln_spec - Xold(s))
            t = exp(Xtemp(1))
            p = exp(Xtemp(2))
            iter = solidPointOnEnvelope(Z,t,p,phase,betaSol,is,s,ln_spec,ierr)
          else
            Xtemp = Xvar
            ! sspec is bracketed, so solve for exact property value and return
            call bracketSolveForProperty(n=2,Z=Z,beta=1.0,propflag=propflag,&
                 propspec=sspec,X=Xtemp,Xold=Xold,dXdS=dXdS,s=s,ierr=ierr)
            t = exp(Xtemp(1))
            p = exp(Xtemp(2))
          endif
          if (ierr /= 0) then
            if (present(ierr_out)) then
              ierr_out = ierr
              return
            else
              if (ierr == 100) then
                print *,'No solution for bracket solver'
              else
                print *,'ierr',ierr
              endif
              call stoperror("grid error: Bracket solver failed!")
            endif
          endif

          call store_subl_point(Z,t,p,sspec,n_grid,T_grid,P_grid,n_grid_found,prop_grid)
          if ( n_grid_found == n_grid ) then
            return
          end if
        enddo
      else
        ! sspec not bracketed, so update old_prop_val and continue
        old_prop_val = curr_prop_val
      endif

    enddo

  end subroutine subl_points_based_on_prop

  !-----------------------------------------------------------------------------
  !> Store point in output vector
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine store_subl_point(Z,t,p,prop,n_grid,T_grid,P_grid,n_grid_found,prop_grid)
    implicit none
    ! Input:
    real, dimension(nc), intent(in) :: Z       !< Overall composition
    real, intent(in) :: t,p                    !< Two-phase gas phase fraction (mol/mol)
    integer, intent(in) :: n_grid              !< number of grid points
    real, intent(in) :: prop                   !< Proporty at point
    ! Output:
    real, intent(inout) :: T_grid(n_grid)        !< t at the grid points
    real, intent(inout) :: P_grid(n_grid)        !< p at the grid points
    integer, intent(inout) :: n_grid_found       !< Number of grid points found
    real, intent(inout) :: prop_grid(n_grid)     !< property grid
    ! Locals:
    n_grid_found = n_grid_found + 1
    T_grid(n_grid_found) = t
    P_grid(n_grid_found) = p
    prop_grid(n_grid_found) = prop
  end subroutine store_subl_point

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on sublimation line having property value
  !> propspec.
  !>
  !> \author MH 2018-6
  !-----------------------------------------------------------------------------
  function propertyFunctionWrapperSubl(Xs,param) result(fun)
    use solideos, only: solidComp
    implicit none
    real, intent(inout) :: Xs
    real, dimension(nc+10), intent(inout) :: param
    real :: fun
    ! Locals:
    integer :: iter, s, phase, is, ierr
    real, dimension(nc) :: Z
    real, dimension(2) :: X,Xold,dXdS
    real :: t, prop, propspec, ln_spec, p, beta, betaSol
    integer :: propflag

    ! Unpack the param vector
    propspec = param(1)
    beta = param(2)
    s = int(param(3))
    Z = param(4:nc+3)
    Xold = param(nc+4:nc+5)
    dXdS = param(nc+6:nc+7) ! is constant each time function is called
    X = param(nc+8:nc+9)
    propflag = int(param(nc+10))

    ! Extrapolate for better initial values in sat_newton
    X = Xold + dXdS*(Xs-Xold(s))
    t = exp(X(1))
    p = exp(X(2))

    ! Get correct phase
    if (beta > 0.5) then
      phase = VAPPH
    else
      phase = LIQPH
    endif
    betaSol = 0.0
    is = solidComp(1)

    ! Find point on envelope corresponding to ln(property value) equal to Xs
    ln_spec = Xs
    iter = solidPointOnEnvelope(Z,t,p,phase,betaSol,is,s,ln_spec,ierr)
    X(1) = log(T)
    X(2) = log(p)

    ! Update Xold-slot in param vector with new X value
    param(nc+8:nc+9) = X

    ! Calculate the property value at the new saturation point
    call genericProperty(t,p,Z,phase,propflag,prop)

    ! Compute new value of objective function
    fun = (prop - propspec)/max(abs(propspec), 1.0)
  end function propertyFunctionWrapperSubl

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on sublimation line having property value
  !> propspec.
  !>
  !> \author MH 2018-6
  !-----------------------------------------------------------------------------
  function isAnyPropBracketed(curr_prop_val,old_prop_val,n_grid,prop_grid,&
       n_grid_not_found,prop_list,n_found) result(isBracketed)
    implicit none
    real, intent(in) :: curr_prop_val,old_prop_val
    integer, intent(in) :: n_grid
    real, dimension(n_grid), intent(inout) :: prop_grid
    integer, intent(inout) :: n_grid_not_found
    real, dimension(n_grid), intent(out) :: prop_list
    integer, intent(out) :: n_found
    logical :: isBracketed
    ! Locals:
    integer :: i,j,k
    isBracketed = .false.
    n_found = 0
    do i=1,n_grid_not_found
      ! Check to see if specification is bracketed.
      if ( prop_grid(i) >= curr_prop_val .and. prop_grid(i) <= old_prop_val .OR. &
           prop_grid(i) <= curr_prop_val .and. prop_grid(i) >= old_prop_val) then
        isBracketed = .true.
        n_found = n_found + 1
        prop_list(n_found) = prop_grid(i)
      endif
    enddo
    ! Remove bracketed property specifications
    if (isBracketed) then
      do i=1,n_found
        do j=1,n_grid_not_found
          if (prop_list(i) == prop_grid(j)) then
            do k=j,n_grid_not_found-1
              prop_grid(k) = prop_grid(k+1)
            enddo
            exit
          endif
        enddo
        n_grid_not_found = n_grid_not_found - 1
      enddo
    endif
  end function isAnyPropBracketed

  !-----------------------------------------------------------------------------
  !> Locate the points on the triple line corresponding to the property values
  !! in prop_grid.
  !!
  !! The routine starts at the "triple" point, and then
  !! traverses the triple area line until it has bracketed one or more grid value; it then
  !! locates the exact sublimation point corresponding to this value, before
  !! continuing.
  !!
  !! Solid appearance line is specified, setting betaSol=0.0
  !! Zero liquid line is specified, setting betaSol<0.0
  !!
  !! \author Morten 2018-06
  !-----------------------------------------------------------------------------
  subroutine triple_area_points_based_on_prop(Z,Ttr,Ptr,Pmax,betaSol,n_grid,&
       propflag,prop_grid,T_grid,P_grid,beta_grid,X_grid,Y_grid,n_grid_found,ierr_out)
    use solideos, only: solidComp, nSolid
    implicit none
    ! Input:
    real, intent(in) :: Z(nc)                  !< total composition
    real, intent(in) :: ttr, ptr               !< triple point
    integer, intent(in) :: n_grid              !< number of grid points
    integer, intent(in) :: propflag            !< specified property 1:s, 2:lnv
    real, intent(inout) :: prop_grid(n_grid)   !< property grid
    real, intent(in) :: pMax                   !< Pressure limit (Pa)
    real, intent(in) :: betaSol                !< -1.0 interpreted as maximum solid amount
    ! Output:
    real, intent(out) :: T_grid(n_grid)        !< t at the grid points
    real, intent(out) :: P_grid(n_grid)        !< p at the grid points
    real, intent(out) :: beta_grid(2,n_grid)   !< gas and solid fraction
    real, intent(out) :: X_grid(nc,n_grid)     !< Liquid composition
    real, intent(out) :: Y_grid(nc,n_grid)     !< Gas composition
    integer, intent(out) :: n_grid_found       !< gird points found
    integer, optional, intent(out) :: ierr_out !< error flag; nonzero if error
    ! Locals:
    real :: t, p
    integer :: i,iter,is,ierr,s,j,sl
    real :: curr_prop_val, old_prop_val, ln_spec, dln_s, pn, sspec
    real, dimension(nc+3) :: Xvar, Xold, dXds, Xtemp
    integer :: n_found, nsol, n_grid_not_found, mode
    real, parameter :: dPmax = 0.2e5
    real :: prop_grid_local(n_grid), prop_grid_list(n_grid)      !< property grids
    real :: Pdew, beta
    integer, parameter :: nd = 3
    real :: XX(nph,nc), betaVec(nph), K(nc), bval, bs
    integer :: phaseVec(nph)
    if (present(ierr_out)) ierr_out = 0
    ! Mapping solid-gas line starting at triple point
    if (nSolid /= 1) then
      call stoperror('saturation::solidEnvelopePlot: No solid model initialized!')
    else
      is = solidComp(1)
    endif
    ! Initial state
    t = Ttr
    p = Ptr
    phaseVec(1:nd) = (/VAPPH, LIQPH, SOLIDPH/)
    XX(3,:) = 0.0
    XX(3,is) = 1.0
    betaVec(1:nd) = (/1.0,0.0,0.0/)
    XX(1,:) = Z
    Pdew = safe_dewP(T,XX(2,:),XX(1,:),ierr)
    K = XX(1,:)/XX(2,:)

    ! Compute property value of initial point on sublimation line
    call genericPropertyMP(nd,t,p,Z,XX,betaVec,phaseVec,propflag,old_prop_val)
    prop_grid_local = prop_grid
    n_grid_found = 0
    n_grid_not_found = n_grid
    if (betaSol < 0.0) then
      mode = VARSOLID
      s = nc+2 ! Specify pressure
      nsol = max(int((Pmax-Ptr)/dPmax)+1,50)
    else
      mode = VARFLUID
      s = nc+3 ! Specify beta
      nsol = 100
    endif
    ! Start from lower triple area edge
    beta = 1.0
    bs = 0.0
    do i=1,nsol
      call fillXvar(mode,bs,beta,t,p,K,Xold)
      if (mode == VARSOLID) then
        pn = Ptr + (Pmax-Ptr)*real(i-1)/real(nsol-1)
        ln_spec = log(pn)
        dln_s = ln_spec - log(p)
        !call newton_extrapolate_threePh(Z,K,t,p,beta,bs,s,is,ln_spec,dln_s,mode)
      else ! mode == VARFLUID
        beta = 1.0 - real(i-1)/real(nsol-1)
        ln_spec = beta
      endif
      iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,bs,is,s,ln_spec,mode,ierr)
      if (ierr /= 0) then
        if (present(ierr_out)) then
          ierr_out = ierr
          return
        else
          call stoperror("grid error: Not able to solve for triple area-line point")
        endif
      endif
      call fillXvar(mode,bs,beta,t,p,K,Xvar)
      call propFromXvar(Xvar,Z,is,mode,bs,beta,t,p,K,betaVec,XX)
      call genericPropertyMP(nd,t,p,Z,XX,betaVec,phaseVec,propflag,curr_prop_val)
      ! Check to see if sspec is bracketed.
      if ( isAnyPropBracketed(curr_prop_val,old_prop_val,n_grid,prop_grid_local,&
           n_grid_not_found,prop_grid_list,n_found)) then
        do j=1,n_found
          sspec = prop_grid_list(j)
          if ( propflag == locate_from_temperature .OR. &
               propflag == locate_from_pressure) then
            if (propflag == locate_from_temperature) then
              sl = nc + 1
            else
              sl = nc + 2
            endif
            ln_spec = log(sspec)
            Xtemp = Xold + dXdS*(ln_spec - Xold(sl))
            call propFromXvar(Xtemp,Z,is,mode,bs,beta,t,p,K,betaVec,XX)
            iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,bs,is,sl,ln_spec,mode,ierr)
          else
            Xtemp = Xvar
            ! sspec is bracketed, so solve for exact property value and return
            if (mode == VARSOLID) then
              bval = beta
            else ! mode == VARFLUID
              bval = betaSol
            endif
            call bracketSolveForProperty(n=nc+3,Z=Z,beta=bval,propflag=propflag,&
                 propspec=sspec,X=Xtemp,Xold=Xold,dXdS=dXdS,s=s,ierr=ierr,mode=mode)
            call propFromXvar(Xtemp,Z,is,mode,bs,beta,t,p,K,betaVec,XX)
          endif
          if (ierr /= 0) then
            if (present(ierr_out)) then
              ierr_out = ierr
              return
            else
              if (ierr == 100) then
                print *,'No solution for bracket solver'
              else
                print *,'ierr',ierr
              endif
              call stoperror("grid error: Bracket solver failed!")
            endif
          endif

          call store_point_triple(nd,t,p,XX,beta,bs,sspec,n_grid,T_grid,P_grid,beta_grid,&
               X_grid,Y_grid,n_grid_found,prop_grid)
          if ( n_grid_found == n_grid ) then
            return
          end if
        enddo
      endif
      ! Update old_prop_val and continue
      old_prop_val = curr_prop_val

    enddo

  end subroutine triple_area_points_based_on_prop

  !-----------------------------------------------------------------------------
  !> Store point in output vector for triple area line
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine store_point_triple(nd,t,p,XX,beta,betaSol,prop,n_grid,T_grid,P_grid,&
       beta_grid,X_grid,Y_grid,n_grid_found,prop_grid)
    implicit none
    ! Input:
    integer, intent(in) :: nd                  !< number of phases
    real, intent(in) :: t,p                    !< Two-phase gas phase fraction (mol/mol)
    real, intent(in) :: beta,betaSol           !< Phase fractions
    real, intent(in) :: XX(nph,nc)             !< Phase compositions
    integer, intent(in) :: n_grid              !< number of grid points
    real, intent(in) :: prop                   !< Property located
    ! Output:
    real, intent(inout) :: T_grid(n_grid)        !< t at the grid points
    real, intent(inout) :: P_grid(n_grid)        !< p at the grid points
    real, intent(inout) :: beta_grid(2,n_grid)   !< gas and solid fraction
    real, intent(inout) :: X_grid(nc,n_grid)     !< Liquid composition
    real, intent(inout) :: Y_grid(nc,n_grid)     !< Gas composition
    integer, intent(inout) :: n_grid_found       !< Number of grid points found
    real, intent(inout) :: prop_grid(n_grid)     !< Property grid
    ! Locals:
    n_grid_found = n_grid_found + 1
    T_grid(n_grid_found) = t
    P_grid(n_grid_found) = p
    beta_grid(:,n_grid_found) = (/ (1.0-betaSol)*beta, betaSol/)
    Y_grid(:,n_grid_found) = XX(1,:)
    X_grid(:,n_grid_found) = XX(2,:)
    prop_grid(n_grid_found) = prop
  end subroutine store_point_triple

  !-----------------------------------------------------------------------------
  !> Get properties from Xvar
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine propFromXvar(Xvar,Z,is,mode,betaSol,beta,t,p,K,betaVec,XX)
    implicit none
    ! Input:
    real, dimension(nc+3), intent(in) :: Xvar !< Variable vector
    real, dimension(nc), intent(in) :: Z      !< Overall composition
    real, intent(inout) :: betaSol,beta       !< Solid and gas phase fraction (mol/mol)
    integer, intent(in) :: mode               !< VARSOLID or VALFLUID
    integer, intent(in) :: is                 !< solid index
    ! Output:
    real, intent(out) :: t                    !< temperature
    real, intent(out) :: p                    !< pressure
    real, intent(out) :: K(nc)                !< equilibrium constants
    real, intent(out) :: betaVec(nph)         !< Phase mol fractions
    real, intent(inout) :: XX(nph,nc)         !< Phase compositions
    !
    real :: Zstar(nc)
    K = exp(Xvar(1:nc))
    t = exp(Xvar(nc+1))
    p = exp(Xvar(nc+2))
    if (mode == VARSOLID) then
      betaSol = Xvar(nc+3)
      betaVec(1) = (1.0-betaSol)*beta
      betaVec(3) = betaSol
    else ! mode == VARFLUID
      beta = Xvar(nc+3)
      betaVec(1) = (1.0-betaSol)*beta
      betaVec(3) = betaSol
    endif
    betaVec(2) = max(0.0,1.0 - betaVec(1) - betaVec(3))
    Zstar = Z
    Zstar(is) = Z(is) - betaSol
    Zstar = Zstar/(1.0-betaSol)
    XX(2,:) = Zstar/(1-beta+beta*K)
    XX(1,:) = K*XX(2,:)
  end subroutine propFromXvar

  !-----------------------------------------------------------------------------
  !> Fill Xvar from properties
  !>
  !> \author MH, 2018-06
  !-----------------------------------------------------------------------------
  subroutine fillXvar(mode,betaSol,beta,t,p,K,Xvar)
    implicit none
    ! Input:
    real, intent(in) :: t                      !< temperature
    real, intent(in) :: p                      !< pressure
    real, intent(in) :: K(nc)                  !< equilibrium constants
    real, intent(in) :: betaSol,beta           !< Solid and gas phase fraction (mol/mol)
    integer, intent(in) :: mode                !< VARSOLID or VALFLUID
    ! Output:
    real, dimension(nc+3), intent(out) :: Xvar !< Variable vector
    !
    if (mode == VARSOLID) then
      Xvar = (/ log(K), log(t), log(p), betaSol/)
    else ! mode == VARFLUID
      Xvar = (/ log(K), log(t), log(p), beta/)
    endif
  end subroutine fillXvar

  subroutine genericPropertyMP(nd,t,p,Z,XX,betaVec,phaseVec,propflag,prop)
    use vls, only: mpSpecificVolume, mpEnthalpy, mpEntropy
    implicit none
    integer, intent(in) :: nd !< Phase identifier
    integer, intent(in) :: phaseVec(nph) !< Phase identifier
    real, intent(in) :: betaVec(nph) !< Phase fractions
    real, intent(in) :: XX(nph,nc) !< Phase compositions
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: Z !< Compozition
    integer, intent(in) :: propflag !< Flag determining what property to return
    real, intent(out) :: prop !< Property

    if (propflag == locate_from_entropy) then
      prop = mpEntropy(nd,t,p,betaVec,XX,phaseVec)
    else if (propflag == locate_from_lnvol) then
      prop = mpSpecificVolume(nd,t,p,betaVec,XX,phaseVec)
      prop = log(prop)
    else if (propflag == locate_from_enthalpy) then
      prop = mpEnthalpy(nd,t,p,betaVec,XX,phaseVec)
    else if (propflag == locate_from_temperature) then
      prop = T
    else if (propflag == locate_from_pressure) then
      prop = P
    end if

  end subroutine genericPropertyMP

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on sublimation line having property value
  !> propspec.
  !>
  !> \author MH 2018-6
  !-----------------------------------------------------------------------------
  function propertyFunctionWrapperTriple(Xs,param) result(fun)
    use solideos, only: solidComp
    implicit none
    real, intent(inout) :: Xs
    real, dimension(4*nc+14), intent(inout) :: param
    real :: fun
    ! Locals:
    integer :: iter, s, is, ierr
    real, dimension(nc) :: Z, K
    real, dimension(nc+3) :: X,Xold,dXdS
    real :: t, prop, propspec, ln_spec, p, beta, betaSol
    integer :: propflag, mode
    real :: XX(nph,nc), betaVec(nph)
    integer :: phaseVec(nph)
    integer, parameter :: nd = 3
    ! Unpack the param vector
    propspec = param(1)
    mode = int(param(4*nc+14))
    s = int(param(3))
    Z = param(4:nc+3)
    Xold = param(nc+4:2*nc+6)
    dXdS = param(2*nc+7:3*nc+9) ! is constant each time function is called
    X = param(3*nc+10:4*nc+12)
    propflag = int(param(4*nc+13))
    !
    is = solidComp(1)

    ! Extrapolate for better initial values in sat_newton
    X = Xold !+ dXdS*(Xs-Xold(s))
    if (mode == VARSOLID) then
      beta = param(2)
      betaSol = X(nc+3)
    else
      betaSol = param(2)
      beta = X(nc+3)
    endif
    call propFromXvar(X,Z,is,mode,betaSol,beta,t,p,K,betaVec,XX)

    ! Find point on envelope corresponding to ln(property value) equal to Xs
    ln_spec = Xs
    iter = solidPointOnEnvelopeThreePh(Z,K,t,p,beta,betaSol,is,s,ln_spec,mode,ierr)
    call fillXvar(mode,betaSol,beta,t,p,K,X)
    call propFromXvar(X,Z,is,mode,betaSol,beta,t,p,K,betaVec,XX)
    phaseVec(1:nd) = (/VAPPH, LIQPH, SOLIDPH/)
    XX(3,:) = 0.0
    XX(3,is) = 1.0

    ! Update Xold-slot in param vector with new X value
    param(3*nc+10:4*nc+12) = X

    ! Calculate the property value at the new saturation point
    call genericPropertyMP(nd,t,p,Z,XX,betaVec,phaseVec,propflag,prop)

    ! Compute new value of objective function
    fun = (prop - propspec)/max(abs(propspec), 1.0)
  end function propertyFunctionWrapperTriple

end module saturation_point_locators
