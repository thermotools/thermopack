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
  use thermopack_var, only: nce
  use eosTV, only: thermo_tv, chemical_potential_tv
  !use thermo_utils, only: wilsonK, phase_Is_fake
  !use stability, only : stabcalc, stabilityLimit
  !use utilities, only: safe_exp
  implicit none
  private
  save

  !> Maximum number of iterations used in nonlinear solvers
  integer, parameter :: max_iter = 200

  integer, parameter :: SPHERICAL=1, CYLINDRICAL=2

  public :: solve_mu_t, solve_lnf_t
  public :: extrapolate_mu_in_inverse_radius

contains

  !-----------------------------------------------------------------------------
  !> Solve mu(T,rho) = mu for a given temperature
  !>
  !> \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine solve_mu_t(mu,T,rho,ierr,phase)
    use nonlinear_solvers
    implicit none
    real, dimension(nce),         intent(in)    :: mu !< Chemical potential
    real,                         intent(in)    :: T !< K - Temperature
    real, dimension(nce),         intent(inout) :: rho !< mol/m3 - Specific mole numbers
    integer,                      intent(out)   :: ierr !< Error flag
    integer, optional,            intent(in)    :: phase !< Generate initial guess if rho is unavailable
    ! Locals
    real :: lnf(nce), muc(nce), lnfc(nce), V
    !
    V = 1.0
    call thermo_tv(T,V,rho,lnfc)
    call chemical_potential_tv(t, v, rho, muc)
    lnf = (mu - muc)/(Rgas*T) + lnfc
    call solve_lnf_t(lnf,T,rho,ierr,phase)
    ! print *,ierr
    ! call chemical_potential_tv(t, v, rho, muc)
    ! print *,"mu-muc",mu-muc
  end subroutine solve_mu_t

  !-----------------------------------------------------------------------------
  !> Solve lnf(T,rho) = lnf for a given temperature
  !>
  !> \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine solve_lnf_t(lnf,T,rho,ierr,phase)
    use nonlinear_solvers
    implicit none
    real, dimension(nce),         intent(in)    :: lnf !< Fugacity coefficient
    real,                         intent(in)    :: T !< K - Temperature
    real, dimension(nce),         intent(inout) :: rho !< mol/m3 - Specific mole numbers
    integer,                      intent(out)   :: ierr !< Error flag
    integer, optional,            intent(in)    :: phase !< Generate initial guess if rho is unavailable
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

  !-----------------------------------------------------------------------------
  !> Corrections for curved surfaces:
  !! Assuming constant temperature extrapolate mu in 1/R to first order,
  !! and calculate bulk phase properties. A constant compoision path is taken.
  !! On entry rho_l and rho_g represent the bulk equilibrium for wich sigma_0 is calculated
  !! For details see Aasen et al. 2018, doi: 10.1063/1.5026747
  !!
  !! \author MH, July 2022
  !-----------------------------------------------------------------------------
  subroutine extrapolate_mu_in_inverse_radius(sigma_0,T,rho_g,rho_l,phase,radius,geometry,mu,ierr)
    use nonlinear_solvers
    use linear_numerics, only: null_space, outer_product
    implicit none
    real,                         intent(in)    :: sigma_0 !< N/m - Surface tension of planar surface
    real,                         intent(in)    :: T !< K - Temperature
    real, dimension(nce),         intent(inout) :: rho_g, rho_l !< mol/m3 - Specific mole numbers
    integer,                      intent(in)    :: phase !< Extrapolate ho lding this phase composition constant
    real,                         intent(in)    :: radius !< m - Radius
    integer,                      intent(in)    :: geometry !< Enumerator (SPHERICAL=1, CYLINDRICAL=2)
    real, dimension(nce),         intent(out)   :: mu !< Chemical potential and logarithm of fugacity coefficient
    integer,                      intent(out)   :: ierr !< Error flag
    ! Locals
    real :: mu_0(nce), V
    real :: g, rho_tot, fac
    real :: M(nce,nce), w(nce), unity(nce)
    real :: rho_g_1(nce), rho_l_1(nce), rho(nce)
    real :: mu_1(nce) !< J/m^3
    real :: mu_rho(nce,nce), mu_rho_l(nce,nce), mu_rho_g(nce,nce) !< J/mol^2
    integer :: i
    !
    unity = 1
    V = 1.0
    call chemical_potential_tv(t, v, rho_l, mu=mu_0, dmudn=mu_rho_l)
    call chemical_potential_tv(t, v, rho_g, mu=mu_0, dmudn=mu_rho_g)
    if (phase == LIQPH) then
      rho = rho_l
      mu_rho = mu_rho_l
      !call thermo_tv(T,V,rho_l,lnf_0)
    else if (phase == VAPPH) then
      rho = rho_g
      mu_rho = mu_rho_g
    else
      call stoperror("Wrong phase identifyer in extrapolate_mu_in_inverse_radius")
    endif

    ! Equation S6
    rho_tot = sum(rho)
    call outer_product(rho,nce,unity,nce,M)
    do i=1,nce
      M(i,i) = M(i,i) + rho_tot
    enddo
    ! Find null-space defining mu_1
    call null_space(M,nce,w,ierr)
    if (ierr /= 0) then
      print *,"Error determening null-space of M matrix in extrapolate_mu_in_inverse_radius"
      return
    endif
    mu_1 = matmul(mu_rho,w)

    if (geometry == SPHERICAL) then
      g = 2
    else if (geometry == CYLINDRICAL) then
      g = 1
    else
      call stoperror("Wrong geometry in extrapolate_mu_in_inverse_radius")
    endif

    ! Equation 14
    fac = g*sigma_0 / dot_product(mu_1,(rho_l-rho_g))
    mu_1 = mu_1*fac

    ! Equation 8 truncated after first order correction
    mu = mu_0 + mu_1/radius

    ! Solve for "bulk" states
    ! Equation 9 truncated after first order correction
    rho_l_1 = rho_l + matmul(1.0/mu_rho_l,mu_1)/radius
    if (minval(rho_l_1) < 0.0) rho_l_1 = rho_l
    call solve_mu_t(mu,T,rho_l_1,ierr,phase=LIQPH)
    if (ierr == 0) then
      rho_l = rho_l_1
    else
      return
    endif
    ! Equation 9 truncated after first order correction
    rho_g_1 = rho_g + matmul(1.0/mu_rho_g,mu_1)/radius
    if (minval(rho_g_1) < 0.0) rho_g_1 = rho_g
    call solve_mu_t(mu,T,rho_g_1,ierr,phase=VAPPH)
    if (ierr == 0) then
      rho_g = rho_g_1
    endif
  end subroutine extrapolate_mu_in_inverse_radius

end module mut_solver
