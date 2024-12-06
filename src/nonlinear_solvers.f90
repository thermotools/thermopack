!-----------------------------------------------------------------------------
!> This module contains generic methods for solving systems of non-linear equations.
!>
!> \author MH, August 2012
!-----------------------------------------------------------------------------
module nonlinear_solvers
  use linear_numerics, only: solveLU
  implicit none
  private

  !> Solver aliases
  integer, parameter :: NS_NEWTON    = 1 !< NR solver
  integer, parameter :: NS_SUCCSUB   = 2
  integer, parameter :: NS_NEWTON_LS = 3 !< Include line search in NR iteration

  !> Bracketing solver aliases
  integer, parameter :: NS_PEGASUS    = 4
  integer, parameter :: NS_RIDDERS    = 5

  ! Constants
  real, parameter :: NS_LARGE = 1e20

  !> A type that contains solver information
  type :: nonlinear_solver
    !> Solver choice
    integer :: isolver = NS_NEWTON_LS
    !> Maximum number of iterations, if applicable
    integer :: max_it = 100
    !> Number of iterations to solution
    integer :: iter = 0
    !> Relative tolerance
    real :: rel_tol = 1e-20
    !> Absolute tolerance.
    real :: abs_tol = 1e-10
    !> Whether the Jacobian should be computed analytically with a user-provided
    !! procedure, or approximated using approximate_jacobian
    logical :: analyt_jac  = .true.
    integer :: analyt_jac_order  = 1
    !> Whether the inverse of the Hessian should be computed analytically with a user-provided
    !! procedure, or approximated by inverting the Jacobian
    logical :: analyt_hess = .false.
    !> Whether to output information about the solution
    logical :: verbose = .false.
    !> Number of line search trials
    integer :: ls_max_it = 10
    !> Limit x to make sure: xmin <= x <= xmax
    logical :: limit_x_values = .true.
    !> Is the Jacobean symmetric?
    logical :: symmetric_jac = .false.

    !> Status flag.
    !> 0 - Solver converged
    !> 1 - No solution found after max_it iterations
    !> 2 - Could not invert jacobian
    !> 3 - Floating point error, probably divergence
    integer :: exitflag = 0
    real :: error_on_exit = 0.0
  end type nonlinear_solver

  abstract interface
    subroutine function_template(f,x,p)
      real, intent(out) :: f(:)
      real, intent(in) :: x(:)
      real, intent(inout) :: p(:)
    end subroutine function_template
  end interface

  abstract interface
    subroutine jacobian_template(J,x,p)
      real, intent(out) :: J(:,:)
      real, intent(in) :: x(:)
      real, intent(inout) :: p(:)
    end subroutine jacobian_template
  end interface

  public :: NS_NEWTON, NS_SUCCSUB,NS_NEWTON_LS
  public :: nonlinear_solve,nonlinear_solver
  public :: NS_PEGASUS, NS_RIDDERS
  public :: pegasus, ridders_method, bracketing_solver
  public :: newton_secondorder_singlevar
  public :: limit_dx, premReturn, setXv, premterm_at_dx_zero
  public :: approximate_jacobian, approximate_jacobian_2nd, &
       approximate_jacobian_4th
  public :: function_template, jacobian_template
  public :: test_differentials
  public :: newton_1d
contains

   !-----------------------------------------------------------------------------
   !> Interface for solver
   !>
   !> \author MH, August 2012
   !-----------------------------------------------------------------------------
   subroutine nonlinear_solve(solver,fun,jac,hess,limit,premterm,setXvar,&
       x,xmin,xmax,param,dim)
    implicit none
    !> Solver information
    type(nonlinear_solver), intent(inout) :: solver
    !> The non-linear function F(x)=0
    external                        :: fun
    !> Function to evaluate the Jacobian of F.
    !> This can be approximate_jacobian, in which the
    !> jacobian is evaluated approximately
    external                        :: jac
    !> Function to evaluate the Hessian of F.
    !> If analyt_hess=false, then this will be
    !> found by inverting J.
    external                        :: hess
    !> Limit step
    external                        :: limit
    !> Test for premature termination
    logical, external               :: premterm
    !> Set variables
    external                        :: setXvar
    !> On entry, initial condition. On exit, solution
    real, dimension(:), intent(inout)       :: x
    !> Minimum and maximum limit for x
    real, dimension(:), intent(in)       :: xmin, xmax
    !> Paramaters
    real, dimension(:), intent(inout)    :: param
    !> Problem size
    integer, optional, intent(in)    :: dim
    !
    select case(solver%isolver)
    case(NS_NEWTON,NS_NEWTON_LS)
      call newton(solver,fun,jac,hess,limit,premterm,setXvar,&
           x,xmin,xmax,param,dim)
    case(NS_SUCCSUB)
      call successive_substitution(solver,fun,x)
    case default
      stop "No such nonlinear solver!"
    end select

  end subroutine nonlinear_solve

  !-----------------------------------------------------------------------------
  !> NR solver
  !>
  !> \author MH, August 2012
  !-----------------------------------------------------------------------------
  subroutine newton(solver,fun,jac,hess,limit,premterm,setXvar,&
       x,xmin,xmax,param,dim)
    !> Solve F(x)=0 using the Newton-Raphson method,
    !> x1 = x0 - J^-1 F(x1)
    !> where J is the Jacobian of F.
    implicit none
    type(nonlinear_solver), intent(inout) :: solver
    external                              :: fun
    external                              :: jac
    external                              :: hess
    external                              :: limit  !< limit step
    logical, external                     :: premterm  !< Test for premature termination
    external                              :: setXvar !< Set variables
    real, dimension(:),     intent(inout) :: x
    real, dimension(:),     intent(in)    :: xmin,xmax
    real, dimension(:),     intent(inout) :: param
    !> Problem size
    integer, optional, intent(in)         :: dim
    !
    ! Other stuff
    real, dimension(size(param)) :: param_old !< Copy of param
    real, dimension(size(x),size(x)) :: J, Jinv
    real, dimension(size(x))         :: f, resid0, resid, dx, x_old
    real    :: norm_resid0,norm_resid,norm_resid_old,alpha
    integer :: i,n,k,np

    if (present(dim)) then
      n=dim
      if (size(x) < dim) then
        call stoperror("libnum::newton: dim > size(x)")
      endif
    else
      n=size(x)
    endif
    np=size(param)

    ! Initial residual
    call fun(resid0,x,param)
    norm_resid0 = sqrt(dot_product(resid0(1:n),resid0(1:n)))
    solver%exitflag = 0
    solver%error_on_exit = norm_resid0

    do i=1,solver%max_it
      ! Find the Hessian
      if (solver%analyt_hess) then
        ! Calculate Hessian analytically
        call hess(Jinv(1:n,1:n),x,param)
        ! Find the next approximation
        call fun(f,x,param)
        dx(1:n) = - matmul(Jinv(1:n,1:n),f(1:n))
      else
        ! Calculate Hessian by finding and inverting the Jacobian
        if (solver%analyt_jac) then
          call jac(J,x,param)
        else
          if (solver%analyt_jac_order == 1) then
            call approximate_jacobian(fun,J,x,n,param,xmax,xmin)
          else if (solver%analyt_jac_order == 2) then
            call approximate_jacobian_2nd(fun,J,x,n,param,xmax,xmin)
          else if (solver%analyt_jac_order == 4) then
            call approximate_jacobian_4th(fun,J,x,n,param,xmax,xmin)
          else
            call stoperror("Newton solver: Error in order for analytical jacobian")
          endif
        endif

        call fun(f,x,param)
        dx(1:n) = -f(1:n)
        call solveLU(n,dx(1:n),j(1:n,1:n),solver%symmetric_jac,solver%exitflag)
        if (solver%exitflag /= 0) then
          return
        endif
      endif

      if (solver%limit_x_values) then
        call limit(n,x,xmin,xmax,dx,np,param)
      endif

      x_old = x
      alpha = 1.0
      param_old = param
      call setXvar(n,np,X,dX,xmin,xmax,param,alpha)
      !x = x + dx
      call fun(resid,x,param)

      ! "Require" reduction in function value
      if (solver%isolver == NS_NEWTON_LS) then
        norm_resid_old = sqrt(dot_product(f(1:n),f(1:n)))
        do k=1,solver%ls_max_it
          norm_resid = sqrt(dot_product(resid(1:n),resid(1:n)))
          if (norm_resid < norm_resid_old) then
            exit
          endif
          alpha = alpha*0.5
          x = x_old
          param = param_old
          call setXvar(n,np,X,dX,xmin,xmax,param,alpha)
          !x = x_old + dx*0.5**k
          call fun(resid,x,param)
        enddo
      endif

      ! Check for convergence
      norm_resid = sqrt(dot_product(resid(1:n),resid(1:n)))
      solver%error_on_exit = norm_resid
      if (norm_resid > 1e20) then
        solver%exitflag = 3
        return
      endif

      if (converged(solver,i,norm_resid0,norm_resid)) exit
      norm_resid0 = norm_resid

      ! Premature return?
      if (solver%isolver == NS_NEWTON_LS) dx = dx*0.5**k ! k is undefined if this is not the case..
      if (premterm(x,dx,param,n,np)) then
        if (solver%verbose) then
          print *,'nonlinear_solvers::newton: Premature return'
        endif
        solver%iter = i
        solver%exitflag = -1
        return
      endif
    enddo

    solver%iter=i

    if(i>=solver%max_it) then
      if(solver%verbose) then
        print *, 'Warning, solver did not converge after maximum number of iterations.'
      endif
      solver%exitflag=1
    endif

  end subroutine newton

  !-----------------------------------------------------------------------------
  !> Solve F(x)=x using the successive substitution method,
  !> x1 = F(x0)
  !> \author KEGT
  !-----------------------------------------------------------------------------
  subroutine successive_substitution(solver,fun,x)
    implicit none
    type(nonlinear_solver), intent(inout) :: solver
    external                              :: fun
    real, dimension(:),     intent(inout) :: x
    real, dimension(size(x)) :: x0,resid
    integer :: i
    real :: norm_resid0,norm_resid

    x0=x
    call fun(x,x0)
    resid=x-x0
    norm_resid0 = sqrt(dot_product(resid,resid))

    do i=1,solver%max_it
      call fun(x,x0)
      resid = x-x0
      norm_resid = sqrt(dot_product(resid,resid))
      if (converged(solver,i,norm_resid0,norm_resid)) exit
      if (norm_resid > NS_LARGE) then
        solver%exitflag=3
        exit
      endif
      norm_resid0=norm_resid
      x0=x
    enddo

    if(i>=solver%max_it) then
      if(solver%verbose) then
        print *, 'Warning, solver did not converge after maximum number of iterations.'
      endif
      solver%exitflag=1
    endif

  end subroutine successive_substitution

  !-----------------------------------------------------------------------------
  !> Test for convergence
  !>
  !> \author MH, August 2012
  !-----------------------------------------------------------------------------
  logical function converged(solver,iterations,resid0,resid)
    !> A convenience routine for checking convergence
    implicit none
    type(nonlinear_solver), intent(inout) :: solver
    integer,                intent(in)    :: iterations
    real,                   intent(in)    :: resid0,resid

    if (resid<solver%rel_tol*resid0 + solver%abs_tol) then
      if (solver%verbose) then
        write(*,100) solver%isolver, iterations, resid
      endif
      solver%exitflag=0
      converged = .true.
    else
      if (solver%verbose) then
        write(*,90) iterations, resid
      endif
      converged = .false.
    endif

90  format (1x,'Iteration ',i3,', residual ',es12.5)
100 format (1x,'Solver', i2, ' converged after', i4,' iterations with residual ',es12.5)

  end function converged

  !-----------------------------------------------------------------------------
  !> Approximate Jacobian numerically using forward differences.
  !>
  !> \author KEGT
  !-----------------------------------------------------------------------------
  subroutine approximate_jacobian(fun,jac,x0,n,param,xmax,xmin)
    implicit none
    external                          :: fun
    real, dimension(n), intent(in)    :: x0
    real, dimension(n,n), intent(out) :: jac
    integer, intent(in)               :: n
    real, dimension(:), intent(inout) :: param !< Paramaters
    real, dimension(n), optional, intent(in) :: xmax,xmin
    !
    real, parameter :: eps = 1e-7
    real, dimension(n) :: dx,f0,f,x
    integer :: i

    dx=0
    call fun(f0,x0,param)
    do i=1,n
      dx(i) = max(1.,abs(x0(i)))*eps
      if (present(xmax)) then
        if (x0(i)+dx(i) > xmax(i)) then
          dx(i) = -dx(i) ! Perturbate into valid region
        endif
      endif
      x = x0+dx
      call fun(f,x,param)
      jac(1:n,i) = (f - f0)/dx(i)
      dx(i) = 0
    enddo
  end subroutine approximate_jacobian
  !-----------------------------------------------------------------------------
  !> Approximate Jacobian numerically using central difference. 2nd order.
  !>
  !> \author GL
  !-----------------------------------------------------------------------------
  subroutine approximate_jacobian_2nd(fun,jac,x0,n,param,xmax,xmin)
    implicit none
    external                          :: fun
    real, dimension(n), intent(in)    :: x0
    real, dimension(n,n), intent(out) :: jac
    integer, intent(in)               :: n
    real, dimension(:), intent(in) :: param !< Paramaters
    real, dimension(n), optional, intent(in) :: xmax,xmin
    !
    real, parameter :: eps = 1e-7
    real, dimension(n) :: dx,f_m1,f_p1,x_m1,x_p1
    real, dimension(:), allocatable :: param_loc
    integer :: i
    real :: xeps

    allocate(param_loc(size(param)))
    param_loc(:) = param(:)
    dx=0.
    do i=1,n
      xeps = max(1.,abs(x0(i)))*eps
      dx(i) = xeps
      x_m1 = x0-dx
      x_p1 = x0+dx
      if (present(xmin) .and. present(xmax)) then
        if (x0(i)+dx(i) > xmax(i)) then
          x_p1(i) = x0(i)
          xeps = xeps*0.5
        else if (x0(i)-dx(i) < xmin(i)) then
          x_m1(i) = x0(i)
          xeps = xeps*0.5
        endif
      endif
      call fun(f_m1,x_m1,param_loc)
      call fun(f_p1,x_p1,param_loc)
      jac(1:n,i) = (f_p1-f_m1)/(2*xeps)
      dx = 0.
    enddo
  end subroutine approximate_jacobian_2nd

  !-----------------------------------------------------------------------------
  !> Approximate Jacobian numerically using central difference. 4th order.
  !>
  !> \author GL
  !-----------------------------------------------------------------------------
  subroutine approximate_jacobian_4th(fun,jac,x0,n,param,xmax,xmin)
    implicit none
    external                          :: fun
    real, dimension(n), intent(in)    :: x0
    real, dimension(n,n), intent(out) :: jac
    integer, intent(in)               :: n
    real, dimension(:), intent(in) :: param !< Paramaters
    real, dimension(n), optional, intent(in) :: xmax,xmin
    !
    real, parameter :: eps = 1e-7
    real, dimension(n) :: dx,f_m2,f_m1,f_p1,f_p2,&
         x_m2,x_m1,x_p1,x_p2
    real, dimension(:), allocatable :: param_loc
    integer :: i
    real :: xeps
    logical :: do_first_order

    allocate(param_loc(size(param)))
    param_loc(:) = param(:)
    dx=0.
    do i=1,n
      xeps = max(1.,abs(x0(i)))*eps
      dx(i) = xeps
      x_m1 = x0-dx
      x_p1 = x0+dx
      do_first_order = .false.
      if (present(xmin) .and. present(xmax)) then
        if (x0(i)+2*dx(i) > xmax(i)) then
          x_p1(i) = x0(i)
          do_first_order = .true.
        else if (x0(i)-2*dx(i) < xmin(i)) then
          x_m1(i) = x0(i)
          do_first_order = .true.
        endif
      endif
      if (do_first_order) then
        call fun(f_m1,x_m1,param_loc)
        call fun(f_p1,x_p1,param_loc)
        jac(1:n,i) = (f_p1-f_m1)/xeps
      else
        x_m2 = x0-2*dx
        x_p2 = x0+2*dx
        call fun(f_m2,x_m2,param_loc)
        call fun(f_m1,x_m1,param_loc)
        call fun(f_p1,x_p1,param_loc)
        call fun(f_p2,x_p2,param_loc)
        jac(1:n,i) = (f_m2 - 8*f_m1 + 8*f_p1 - f_p2)/(12*xeps)
      endif
      dx = 0.
    enddo
  end subroutine approximate_jacobian_4th

  !-----------------------------------------------------------------------------
  !> Limit change in x, dx, to keep x between extreme values:
  !> xmin <= x <= xmax
  !>
  !> \author MH, August 2012
  !-----------------------------------------------------------------------------
  subroutine limit_dx(n,x,xmin,xmax,dx,np,param)
    implicit none
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: x,xmin,xmax
    real, dimension(n),     intent(inout) :: dx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: param
    real :: scaling
    integer :: i
    !
    scaling = 1.0
    do i=1,n
      if (x(i)+dx(i) < xmin(i) .AND. abs(dx(i)) > 1.0e-9) then
        scaling = min(scaling,(xmin(i)-x(i))/dx(i))
      endif
      if (x(i)+dx(i) > xmax(i) .AND. abs(dx(i)) > 1.0e-9) then
        scaling = min(scaling,(xmax(i)-x(i))/dx(i))
      endif
    enddo
    !
    if (scaling < 1.0) then
      dx = dx * scaling
    endif
    !
  end subroutine limit_dx

  !-----------------------------------------------------------------------------
  !> No premature return
  !>
  !> \author MH, 2013-10-08
  !-----------------------------------------------------------------------------
  function premReturn(X,dX,param,n,np) result(doReturn)
    implicit none
    integer, intent(in)                :: n !< Dimension of X
    integer, intent(in)                :: np !< Dimension of param
    real, dimension(n), intent(in)     :: X !< Vapour mole numbers [mole]
    real, dimension(np), intent(in)    :: param !< Parameter vector
    real, dimension(n), intent(in)     :: dX !<
    logical                            :: doReturn !< Terminate minimization?
    !
    doReturn = .false.
  end function premReturn

  !-------------------------------------------------------------------------
  !> Terminate search when step size is zero
  !>
  !> \author MH, 2014
  !-------------------------------------------------------------------------
  function premterm_at_dx_zero(var,dvar,param,n,np) result(premature_return)
    implicit none
    integer, intent(in)                :: n !< Dimension of X
    integer, intent(in)                :: np !< Dimension of param
    real, dimension(n), intent(in)     :: var !< Variable vector
    real, dimension(n), intent(inout)  :: dvar !< Change in variable vector
    real, dimension(np), intent(in)    :: param !< Parameter vector
    logical :: premature_return
    ! Locals
    premature_return = .false.
    if (sum(abs(dvar)) == 0.0) then
      premature_return = .true. ! Stuck at max/min
    endif
  end function premterm_at_dx_zero

  !-----------------------------------------------------------------------------
  !> Set variables
  !>
  !> \author MH, 2013-02-27
  !-----------------------------------------------------------------------------
  subroutine setXv(n,nparam,X,dX,Xmin,Xmax,param,alpha)
    implicit none
    integer, intent(in) :: n, nparam !< Problem dimension
    real, dimension(n), intent(inout) :: X !< Variables
    real, dimension(n), intent(in)    :: dX !< Change in variables
    real, dimension(n), intent(in)    :: Xmin,Xmax !< Variable limits
    real, dimension(nparam), intent(inout) :: param !< Parameter vector
    real, intent(in) :: alpha !< dX scaling
    !
    integer :: i
    X(1:n) = X(1:n) + alpha*dX(1:n)
    do i=1,n
      X(i) = min(max(X(i),Xmin(i)),Xmax(i))
    enddo
    !
  end subroutine setXv

  !-----------------------------------------------------------------------------
  !> Interface for bracketing methods
  !! Solve: f(x)=0 for xlow <= x <= xhigh
  !!
  !! \author MH, Januar 2014
  !-----------------------------------------------------------------------------
  subroutine bracketing_solver(xmin,xmax,fun,x,solver,param)
    implicit none
    real, external    :: fun
    real, intent(in)  :: xmin, xmax
    real, intent(out) :: x
    real, dimension(:), optional, intent(inout) :: param
    type(nonlinear_solver), intent(inout) :: solver

    select case(solver%isolver)
    case(NS_PEGASUS)
       call pegasus(xmin,xmax,fun,x,solver,param)
    case(NS_RIDDERS)
       call ridders_method(fun,param,xmin,xmax,solver,x)
    case default
      stop "No such bracketing solver!"
    end select

  end subroutine bracketing_solver

  !-----------------------------------------------------------------------------
  !> A method of Regula Falsi type for finding a simple root of a non-linear equation.
  !! Solve: f(x)=0 for xlow <= x <= xhigh
  !! REF:
  !! AN IMPROVED PEGASUS METHOD FOR ROOT FINDING
  !! RICHARD F. KING
  !! BIT Numerical Mathematics, 13 (1973), 423-427
  !! \author MH, August 2012
  !-----------------------------------------------------------------------------
  subroutine pegasus(xlow,xhigh,fun,x,solver,param)
    implicit none
    real, external    :: fun
    real, intent(in)  :: xlow, xhigh
    real, intent(out) :: x
    real, dimension(:), optional, intent(inout) :: param
    !real, dimension(:), intent(inout) :: param
    ! Solver information
    type(nonlinear_solver), intent(inout) :: solver
    ! Internal variables
    real :: x0, x1, x2, xtmp, f0, f1, f2, ftmp, tol
    logical :: pass_through_to_step_5
    integer :: i
    tol = solver%abs_tol
    solver%exitflag = 0
    solver%error_on_exit = 1.0

    x0 = xlow
    f0 = eval(x0,fun,param)
    x1 = xhigh
    f1 = eval(x1,fun,param)

    ! Step 2
    if(f0*f1 > 0.0) then
      if (solver%verbose) then
        print *,'PEGASUS solver: No solution exist on search region.'
      endif
      solver%exitflag = 100
      return
    endif

    pass_through_to_step_5 = .false.
    do i=1,solver%max_it
      if (.not.pass_through_to_step_5) then
        ! Step 3
        x2 = x1 - (x0-x1)*f1/(f0-f1)
        f2 = eval(x2,fun,param)
        if (abs(f2) < tol) then
          x = x2
          solver%error_on_exit = abs(f2)
          return
        endif

        ! Step 4
        if (f2*f1 < 0.0) then
          xtmp = x0
          ftmp = f0
          x0 = x1
          f0 = f1
          x1 = xtmp
          f1 = ftmp
        endif
      endif

      pass_through_to_step_5 = .false.

      ! Step 5
      if (f2*f1 > 0.0) then
        f0 = f0*f1/(f1+f2)
        x1 = x2
        f1 = f2
        x2 = x1 - (x0-x1)*f1/(f0-f1)
        f2 = eval(x2,fun,param)
        if (abs(f2) < tol) then
          x = x2
          solver%error_on_exit = abs(f2)
          return
        endif
      endif

      ! Step 6
      if (f2*f1 < 0.0) then
        x0 = x1
        f0 = f1
        x1 = x2
        f1 = f2
      else
        pass_through_to_step_5 = .true.
      endif

    enddo

    if (solver%verbose) then
      print *,'PEGASUS solver failed to converge to a solution. f=',f2
    endif
    solver%error_on_exit = abs(f2)
    x = x2
    solver%exitflag = 1

  end subroutine pegasus

  !> Make param optional for lazy programmers
  function eval(x,fun,param) result(f)
    real, external    :: fun
    real, intent(in) :: x
    real, dimension(:), optional, intent(inout) :: param
    real :: f
    if (present(param)) then
       f = fun(x,param)
    else
       f = fun(x)
    endif
  end function eval

  !-----------------------------------------------------------------------------
  !> Ridders' method for root finding.
  ! Numerical Recipies, Sec. 9.2.1
  ! Solve: f(x)=0 for xlow <= x <= xhigh
  !
  ! Convergence: Uses solver%rel_tol on dx.
  !
  ! solver%exitflag:
  ! 0 - Solver converged
  ! 1 - No solution found after max_it iterations
  ! 4 - Interval does not bracket a root.
  !
  !! \author EA, August 2013
  !-----------------------------------------------------------------------------
  subroutine ridders_method(func,param,xmin,xmax,solver,x)
  !-----------------------------------------------------------------!
    implicit none
    ! Input:
    real, external                        :: func
    real, dimension(:), optional, intent(inout) :: param
    real, intent(in)                      :: xmin,xmax
    ! In/Out:
    type(nonlinear_solver), intent(inout) :: solver
    ! Output:
    real, intent(out)             :: x
    ! Internal:
    real                          :: xl,xh,xm,fl,fh,fm,s,xnew,fnew


    if (solver%verbose) then
      write(*,*) "Entering Ridders' method"
      write(*,*) "Bracket: ",xmin,xmax
    endif

    solver%error_on_exit = 0.0
    fl = eval(xmin,func,param)
    fh = eval(xmax,func,param)
    if (fl == 0.0) then
      x = xmin
      solver%exitflag = 0
      return
    elseif (fh == 0.0) then
      x = xmax
      solver%exitflag = 0
      return
    endif

    xl = xmin
    xh = xmax
    x = 2.0*xmax
    if (fl*fh < 0) then ! If f1 and f2 have opposite signs.
      do
        solver%iter = solver%iter + 1

        xm = 0.5*(xl+xh)
        fm = eval(xm,func,param)
        s = sqrt(fm*fm-fl*fh)
        if (s==0.0) then
          call stoperror("Ridders' method: This should not happen..")
        end if

        xnew = xm+(xm-xl)*sign(1.0,fl-fh)*fm/s
        fnew = eval(xnew,func,param)

        if (solver%verbose) then
          write(*,*) "x=",xnew, " fnew=",fnew
          write(*,*) "Rel. error in x: ",abs((xnew-x)/xnew)
        endif

        if ((abs((xnew-x)/xnew) < solver%rel_tol) .or. (fnew==0.0)) then
          if (solver%verbose) write(*,*) "Converged"
          x = xnew
          solver%exitflag = 0
          solver%error_on_exit = abs(fnew)
          exit
        end if
        x = xnew

        if (fm*fnew < 0.0) then ! If fm and fnew have opposite signs.
          xl = xm
          fl = fm
          xh = xnew
          fh = fnew
        elseif (fl*fnew < 0.0) then ! If fl and fnew have opposite signs.
          xh = xnew
          fh = fnew
        elseif (fh*fnew < 0.0) then ! If fh and fnew have opposite signs.
          xl = xnew
          fl = fnew
        else
          call stoperror("Ridders' method: Error...")
        end if
        if (solver%iter >= solver%max_it) then
          solver%exitflag = 1
          solver%error_on_exit = abs(fnew)
          return
          !call stoperror("Ridders' method: Did not converge to specified accuracy in specified number of iterations.")
        end if

      end do
    else
      if (solver%verbose) then
        write(*,*) "fl = ",fl
        write(*,*) "fh = ",fh
        write(*,*) "Ridders' method: Root is not bracketed in interval"
      endif
      solver%exitflag = 4
      solver%error_on_exit = 1.0
    end if
  end subroutine ridders_method
  !-----------------------------------------------------------------!


  !-----------------------------------------------------------------!
  !> Second order Newton solver (cubic convergence).
  !
  ! See interface block for "func" below.
  ! Convergence: Uses solver%rel_tol on dx.
  !
  ! solver%exitflag:
  ! 0 - Solver converged
  ! 1 - No solution found after max_it iterations
  !
  !! \author EA, 2014-02
  !-----------------------------------------------------------------!
  subroutine newton_secondorder_singlevar(func,xinit,xmin,xmax,solver,x,param)
  !-----------------------------------------------------------------!
    implicit none
    ! Input:
    external                              :: func
    real, intent(in)                      :: xinit
    real, intent(in)                      :: xmin,xmax
    real, intent(in)                      :: param(:)
    ! In/Out:
    type(nonlinear_solver), intent(inout) :: solver
    ! Output:
    real, intent(out)                     :: x
    ! Internal:
    integer                               :: iter,k
    real                                  :: f,dfdx,d2fdx2
    real                                  :: dx,x_old,f_old
    real                                  :: correction
    x = xinit
    x_old = x
    call func(x,f,param,dfdx,d2fdx2)
    f_old = f
    do iter=1,solver%max_it
      correction = 0.5*f*d2fdx2/(dfdx**2)
      if (abs(correction) > 0.25) correction = 0.0
      dx = -(f/dfdx)*(1.0 + correction)
      x = x + dx

      ! Do not allow going outside the bracket.
      if (x > xmax) then
        x = 0.5*(x-dx+xmax)
      elseif (x < xmin) then
        x = 0.5*(x-dx+xmin)
      elseif (abs(dx/x) < solver%rel_tol) then
        ! Check for convergence:
        solver%iter = iter
        solver%exitflag = 0
        return
      endif
      call func(x,f,param,dfdx,d2fdx2)

      ! "Require" reduction in function value
      if (solver%isolver == NS_NEWTON_LS) then
        dx = x - x_old
        do k=1,solver%ls_max_it
          if (abs(f) < abs(f_old)) then
            exit
          endif
          x = x_old + dx*0.5**k
          call func(x,f,param,dfdx,d2fdx2)
        enddo
      endif
      x_old = x
      f_old = f
    end do

    write(*,*) "newton_secondorder_singlevar could not converge in max_it"
    solver%exitflag = 1

  end subroutine newton_secondorder_singlevar
  !-----------------------------------------------------------------!


  !-----------------------------------------------------------------------------
  !> A clean, minimal implemention of Newton's method for solving fun(x) = 0 in
  !> 1D. Quadratic convergence.
  !>
  !> \author Ailo A, December 2014
  !-----------------------------------------------------------------------------
  function newton_1d(fun,x0,param,xmin,xmax) result(r)
    implicit none
    interface
       subroutine fun(x, param, f, df)
         implicit none
         real, intent(in)  :: x                     !< function argument x
         real, dimension(:), intent(in) :: param    !< additional parameters
         real, intent(out) :: f                     !< function value f(x)
         real, intent(out), optional :: df          !< derivative value df(x)/dx
       end subroutine fun
    end interface
    real, intent(in) :: x0                     !< initial guess
    real, dimension(:), intent(inout) :: param !< additional parameters for fun
    real, optional, intent(in) :: xmin, xmax   !< limits
    real :: r                                  !< the "root"
    !locals
    real :: f, df
    real :: rel_tol, abs_tol
    integer :: iter, max_iter

    max_iter = 20
    rel_tol = 1e-10
    abs_tol = 1e-8
    r = x0
    iter = 0
    do
       call fun(x=r,f=f,df=df,param=param)
       r = r - f/df
       if (present(xmin)) then
         r = max(xmin,r)
       endif
       if (present(xmax)) then
         r = min(xmax,r)
       endif
       iter = iter + 1
       if ( iter > max_iter .or. (abs(f/df) < rel_tol*r + abs_tol) ) exit
    end do
    if (iter > max_iter) call stoperror("newton_1d method: Error...")
  end function newton_1d

  subroutine test_differentials(X,param,fun,jac)
    implicit none
    real, intent(in) :: X(:)
    real, intent(inout) :: param(:)
    !procedure(function_template) :: fun
    !procedure(jacobian_template) :: jac
    external :: fun
    external :: jac
    ! Internal
    integer :: i
    real, parameter :: eps = 1.0e-5
    real, dimension(size(X)) :: XX1, G0, G1, G2
    real, dimension(size(X),size(X)) :: J0

    call fun(G0,X,param)
    print *,G0
    call jac(J0,X,param)
    do i=1,size(X)
      XX1 = X
      XX1(i) = X(i) - eps
      call fun(G1,XX1,param)
      XX1(i) = X(i) + eps
      call fun(G2,XX1,param)
      print *,"i",i
      print *,(G2-G1)/eps/2
      print *,J0(:,i)
    enddo
    stop
  end subroutine test_differentials

end module nonlinear_solvers
