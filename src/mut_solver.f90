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

  public :: solve_mu_t, solve_lnf_t

contains

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
    real, dimension(nce) :: F1, F2
    real, dimension(nce, nce) :: J
    real :: rho1(nce), eps
    integer :: i
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

end module mut_solver
