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
  use eosTV, only: thermo_tv, chemical_potential_tv, pressure
  implicit none
  private
  save

  !> Maximum number of iterations used in nonlinear solvers
  integer, parameter :: max_iter = 200

  integer, parameter :: SPHERICAL=1, CYLINDRICAL=2

  public :: solve_mu_t, solve_lnf_t
  public :: extrapolate_mu_in_inverse_radius
  public :: solve_laplace

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

  !-----------------------------------------------------------------------------
  !> Corrections for curved surfaces:
  !! Assuming constant temperature calculate meta-stable states
  !! where pressure difference is given by the Laplace relation and equality in chemical potential
  !! A constant compoision path is taken.
  !! On entry rho_l and rho_g are initial guess
  !!
  !! \author MH, August 2022
  !-----------------------------------------------------------------------------
  subroutine solve_laplace(sigma_0,T,rho_g,rho_l,phase,radius,geometry,mu,ierr)
    use nonlinear_solvers
    use numconstants, only: expMax, expMin
    implicit none
    real,                         intent(in)    :: sigma_0 !< N/m - Surface tension of planar surface
    real,                         intent(in)    :: T !< K - Temperature
    real, dimension(nce),         intent(inout) :: rho_g, rho_l !< mol/m3 - Specific mole numbers
    integer,                      intent(in)    :: phase !< Extrapolate holding this phase composition constant
    real,                         intent(in)    :: radius !< m - Radius
    integer,                      intent(in)    :: geometry !< Enumerator (SPHERICAL=1, CYLINDRICAL=2)
    real, dimension(nce),         intent(out)   :: mu !< Chemical potential and logarithm of fugacity coefficient
    integer,                      intent(out)   :: ierr !< Error flag
    ! Locals
    real :: V, g, dp, sgn
    real :: n(nce), rho(nce), param(nce+2), x(nce+1), xmax(nce+1), xmin(nce+1)
    type(nonlinear_solver) :: solver
    ! Testing
    ! real, dimension(nce+1) :: F1, F2
    ! real, dimension(nce+1, nce+1) :: J
    ! real :: x1(nce+1), eps
    ! integer :: i
    !
    ierr = 0
    if (phase == LIQPH) then
      V = 1.0/sum(rho_l)
      n = rho_l*V
      rho = rho_g
      sgn = 1
    else if (phase == VAPPH) then
      V = 1.0/sum(rho_g)
      n = rho_g*V
      rho = rho_l
      sgn = -1
    else
      call stoperror("Wrong phase identifyer in solve_laplace")
    endif

    if (geometry == SPHERICAL) then
      g = 2
    else if (geometry == CYLINDRICAL) then
      g = 1
    else
      call stoperror("Wrong geometry in solve_laplace")
    endif

    dp = g*sigma_0/radius

    ! Parameters
    param(1) = T
    param(2) = sgn*dp
    param(3:nce+2) = n

    ! Variables
    x(1:nce) = log(rho)
    x(nce+1) = log(V)
    xmax = expMax
    xmin = expMin

    !
    ! Testing
    ! call laplace_function(F1, x, param)
    ! print *,"F1",F1
    ! call laplace_jacobian(J, x, param)
    ! eps = 1.0e-5
    ! do i=1,nce+1
    !   print *,"Var i=",i
    !   x1 = x
    !   x1(i) = x(i)*(1+eps)
    !   call laplace_function(F2, x1, param)
    !   x1(i) = x(i)*(1-eps)
    !   call laplace_function(F1, x1, param)
    !   print *,(F2-F1)/(2*x(i)*eps)
    !   print *,J(:,i)
    ! enddo
    ! stop

    ! Attempt to solve for point on interface
    solver%max_it = max_iter
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-12
    solver%limit_x_values = .true.
    solver%ls_max_it = 5

    call nonlinear_solve(solver,laplace_function,laplace_jacobian,&
         laplace_jacobian,limit_dx,premReturn,setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    if (solver%exitflag /= 0) then
      if (verbose) print *,"Not able to solve for laplace, exit flag: ",solver%exitflag
    else
      ! Calculate mu
      V = exp(x(nce+1))
      call thermo_tv(T,V,n,mu)
      ! Set densities
      if (phase == LIQPH) then
        rho_l = n/V
        rho_g = exp(x(1:nce))
      else if (phase == VAPPH) then
        rho_g = n/V
        rho_l = exp(x(1:nce))
      endif
    endif
  end subroutine solve_laplace

  !-----------------------------------------------------------------------------
  !> Calculate Laplace error function
  !>
  !> \author MH, August 2022
  !-----------------------------------------------------------------------------
  subroutine laplace_function(F, x, param)
    implicit none
    real, dimension(nce+1),     intent(in)  :: x
    real, dimension(nce+2),     intent(in)  :: param
    real, dimension(nce+1),     intent(out) :: F
    ! Locals
    real :: T, V1, V2, P_1, P_2
    real :: n(nce), rho(nce), dp_spec, lnf_1(nce), lnf_2(nce)
    ! Parameters
    T = param(1)
    dp_spec = param(2)
    n = param(3:nce+2)
    ! Variables
    rho = exp(x(1:nce))
    V1 = exp(x(nce+1))
    call thermo_tv(T,V1,n,lnf_1)
    p_1 = pressure(T,V1,n)
    V2 = 1
    call thermo_tv(T,V2,rho,lnf_2)
    p_2 = pressure(T,V2,rho)
    F(1:nce) = lnf_1 - lnf_2
    F(nce+1) = (P_1 - P_2 - dp_spec)/dp_spec
  end subroutine laplace_function

  !-----------------------------------------------------------------------------
  !> Calculate Laplace Jacobian
  !>
  !> \author MH, August 2022
  !-----------------------------------------------------------------------------
  subroutine laplace_jacobian(J, x, param)
    implicit none
    real, dimension(nce+1),        intent(in)  :: x
    real, dimension(nce+2),        intent(in)  :: param
    real, dimension(nce+1, nce+1), intent(out) :: J
    ! Locals
    real :: T, V1, V2, P_1, P_2, PV_1, Pn_2(nce)
    real :: n(nce), rho(nce), dp_spec, lnf_1(nce), lnf_2(nce)
    real :: lnfV_1(nce), lnfn_2(nce,nce)
    integer :: i
    ! Parameters
    T = param(1)
    dp_spec = param(2)
    n = param(3:nce+2)
    ! Variables
    rho = exp(x(1:nce))
    V1 = exp(x(nce+1))
    ! Phase 1
    call thermo_tv(T,V1,n,lnf_1,lnphiv=lnfV_1)
    p_1 = pressure(T,V1,n,dpdv=PV_1)
    ! Phase 2
    V2 = 1
    call thermo_tv(T,V2,rho,lnf_2,lnphin=lnfn_2)
    p_2 = pressure(T,V2,rho,dpdn=Pn_2)

    do i=1,nce
      J(i,1:nce) = - rho*lnfn_2(i,1:nce)
    enddo
    J(1:nce,nce+1) = V1*lnfV_1
    J(nce+1, 1:nce) = - rho*Pn_2/dp_spec
    J(nce+1,nce+1) = V1*PV_1/dp_spec

  end subroutine laplace_jacobian

end module mut_solver
