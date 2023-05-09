!-------------------------------------------------------------------------
!> Solve mu-T problem. Look for single phase solution given initial guess.
!>
!>
!-------------------------------------------------------------------------
module mut_solver
  !
  !
  use numconstants, only: machine_prec, small
  use thermopack_constants
  use thermopack_var, only: nce, Rgas
  use eosTV, only: thermo_tv, chemical_potential_tv
  implicit none
  private
  save

  !> Maximum number of iterations used in nonlinear solvers
  integer, parameter :: max_iter = 200

  public :: solve_mu_t, solve_lnf_t
  public :: map_meta_isotherm

contains

  !-----------------------------------------------------------------------------
  !> Solve mu(T,rho) = mu for a given temperature
  !>
  !> \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine solve_mu_t(mu,T,rho,ierr)
    use nonlinear_solvers
    implicit none
    real, dimension(nce),         intent(in)    :: mu !< Chemical potential
    real,                         intent(in)    :: T !< K - Temperature
    real, dimension(nce),         intent(inout) :: rho !< mol/m3 - Specific mole numbers
    integer,                      intent(out)   :: ierr !< Error flag
    ! Locals
    real :: lnf(nce), muc(nce), lnfc(nce), V
    !
    V = 1.0
    call thermo_tv(T,V,rho,lnfc)
    call chemical_potential_tv(t, v, rho, muc)
    lnf = (mu - muc)/(Rgas*T) + lnfc
    call solve_lnf_t(lnf,T,rho,ierr)
    ! print *,ierr
    ! call chemical_potential_tv(t, v, rho, muc)
    ! print *,"mu-muc",mu-muc
  end subroutine solve_mu_t

  !-----------------------------------------------------------------------------
  !> Solve lnf(T,rho) = lnf for a given temperature
  !>
  !> \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine solve_lnf_t(lnf,T,rho,ierr)
    use nonlinear_solvers
    implicit none
    real, dimension(nce),         intent(in)    :: lnf !< Fugacity coefficient
    real,                         intent(in)    :: T !< K - Temperature
    real, dimension(nce),         intent(inout) :: rho !< mol/m3 - Specific mole numbers
    integer,                      intent(out)   :: ierr !< Error flag
    ! Locals
    real :: rhomax(nce), rhomin(nce)
    real, dimension(nce+1) :: param
    type(nonlinear_solver) :: solver
    ! Testing
    !real, dimension(nce) :: F1, F2
    !real, dimension(nce, nce) :: J
    !real :: rho1(nce), eps
    !integer :: i
    !
    ierr = 0
    !
    param(1) = T
    param(2:nce+1) = lnf
    !
    ! Testing
    ! call lnf_t_function(F1, rho, param)
    ! print *,"F1",F1
    ! call lnf_t_jacobian(J, rho, param)
    ! eps = 1.0e-5
    ! do i=1,nce
    !   print *,"Comp i=",i
    !   rho1 = rho
    !   rho1(i) = rho(i)*(1+eps)
    !   call lnf_t_function(F2, rho1, param)
    !   rho1(i) = rho(i)*(1-eps)
    !   call lnf_t_function(F1, rho1, param)
    !   print *,(F2-F1)/(2*rho(i)*eps)
    !   print *,J(:,i)
    ! enddo
    ! stop
    ! Attempt to solve for point on interface
    solver%max_it = max_iter
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-12
    solver%limit_x_values = .true.
    solver%ls_max_it = 5
    rhomin = 0
    rhomax = rho*2

    call nonlinear_solve(solver,lnf_t_function,lnf_t_jacobian,&
         lnf_t_jacobian,limit_dx,premReturn,setXv,rho,rhomin,rhomax,param)
    if (solver%exitflag /= 0) then
      print *,"Not able to solve for lnf-T, exit flag: ",solver%exitflag
      ierr = solver%exitflag
      return
    endif
  end subroutine solve_lnf_t

  !-----------------------------------------------------------------------------
  !> Calculate residual lnf(T,rho) - lnf
  !>
  !> \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine lnf_t_function(F, rho, param)
    implicit none
    real, dimension(nce),     intent(in)  :: rho !< Component density
    real, dimension(nce+1),   intent(in)  :: param
    real, dimension(nce),     intent(out) :: F
    ! Locals
    real :: T, V
    real :: lnf_spec(nce), lnf(nce)
    T = param(1)
    lnf_spec = param(2:nce+1)
    V = 1
    call thermo_tv(T,V,rho,lnf)
    F = lnf - lnf_spec
  end subroutine lnf_t_function

  !-----------------------------------------------------------------------------
  !> Calculate Jacobian for lnf(T,rho) - lnf wrt. rho
  !>
  !> \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine lnf_t_jacobian(J, rho, param)
    implicit none
    real, dimension(nce),     intent(in)  :: rho !< Component density
    real, dimension(nce+1),   intent(in)  :: param
    real, dimension(nce, nce), intent(out) :: J
    ! Locals
    real :: T, V
    real :: lnf(nce)
    T = param(1)
    V = 1
    call thermo_tv(T,v,rho,lnf,lnphin=J)
  end subroutine lnf_t_jacobian

  !-------------------------------------------------------------------------
  !> Map isotherm from saturation line to spinodal
  !>
  !> \author MH, 2022-11
  !-------------------------------------------------------------------------
  subroutine map_meta_isotherm(t,Z,n,phase,v,rho_other,ierr)
    use saturation, only: safe_bubP, safe_dewP
    use spinodal, only: initial_stab_limit_point
    use eos, only: specificvolume
    implicit none
    real, dimension(nce), intent(in) :: Z !< Composition
    real, intent(in) :: t !< Temperature [K]
    integer, intent(in) :: n !< Number of points on isotherm
    integer, intent(in) :: phase !< Phase identifer
    integer, intent(out) :: ierr !< Error flag
    real, intent(out) :: v(n) !< Specific volume (m3/mol)
    real, intent(out) :: rho_other(n,nce) !< Specific volume of other phase (-)
    ! Locals
    real :: vz, vo, p_sat, mu(nce), dmu(nce)
    real :: T_spin, vz_spin, P0
    real :: Xo(nce), dv, rho(nce)
    real :: mu_old(nce), mu_other(nce), dmudrho(nce)
    integer :: o_phase, i
    ierr = 0
    ! Locate saturation state
    if (phase == LIQPH) then
      p_sat = safe_bubP(T,Z,Xo,ierr)
      o_phase = VAPPH
    else
      p_sat = safe_dewP(T,Xo,Z,ierr)
      o_phase = LIQPH
    endif
    if (ierr /= 0) then
      ! Need to trace envelope to find saturation pressure
      call stoperror("map_meta_isotherm::Enable evelope tracing")
    endif
    ! Get volumes at saturation curve
    call specificvolume(T,p_sat,Z,phase,vz)
    call specificvolume(T,p_sat,Xo,o_phase,vo)
    call chemical_potential_tv(T, vz, Z, mu_old)
    rho = xo/vo
    v(1) = vz
    rho_other(1,:) = rho

    ! Locate spinodal
    P0 = 1.0e5
    call initial_stab_limit_point(P0,z,vz_spin,T_spin,phase,ierr,Tmin=T)
    if (ierr /= 0) then
      call stoperror("map_meta_isotherm::Point location on spinodal failed")
    endif
    dv = (vz_spin - vz)/(n-1)

    ! Map from saturation curve to spinodal
    do i=2,n
      vz = vz + dv
      ! Calculate mu
      call chemical_potential_tv(T, vz, Z, mu)
      ! Extrapolate density of other phase
      dmu = mu - mu_old
      call chemical_potential_tv(T, 1.0, rho, mu_other, dmudn=dmudrho)
      rho = rho + dmu/dmudrho
      call solve_mu_t(mu,T,rho,ierr)
      if (ierr /= 0) return
      v(i) = vz
      rho_other(i,:) = rho
      mu_old = mu
    enddo
  end subroutine map_meta_isotherm

end module mut_solver
