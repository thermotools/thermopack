!-----------------------------------------------------------------------------
!> This module contains generic methods for minimizing systems of non-linear equations.
!>
!> \todo Add special treatment for n=2 systems. Analytical modification of eigenvalues.
!>
!> \author MHA, February 2012
!-----------------------------------------------------------------------------
module optimizers
  !
  !
  !
  use numconstants, only: machine_prec
  use thermopack_constants, only: verbose
  implicit none
  private

  !> Optimizer aliases
  integer, parameter :: NO_MOD_NEWTON = 1 !< Modefied newton optimizer

  !> A type that contains solver information
  type :: optim_param
    !> Optimizer choice
    integer :: ioptim = NO_MOD_NEWTON
    !> Maximum number of iterations, if applicable
    integer :: max_iter = 1000
    !> Relative tolerance
    real :: rel_tol = 1.0e5*machine_prec
    !> Line search parameter
    real :: wolfe = 1.0e-3
    !> Number of line search trials
    integer :: max_line_search_iter = 10
    !> Terminate besed on gradient infinity norm
    logical :: gradient_termination = .false.
    !> Turn off/on line search control (line search controlled by last param element)
    logical :: line_search_control = .false.
    !
    ! Output:
    !
    !> Number of iterations to solution
    integer :: iter = 0
    !> Objective function value at return
    real :: of = 0.0
    !> Error at return
    real :: error = 0.0
    !> Status flag.
    !> 0 - Solver converged
    !> 1 - No solution found after max_it iterations
    !>-1 - Premature return
    integer :: exitflag = 0
    !> Test eigenvalues of the Jacobean
    logical :: testEigenvalues = .false.
  end type optim_param

  abstract interface
    function error_function(n,X,nparam,param,of,dofdX,of_old) result(error)
      implicit none
      integer, intent(in)                 :: n !< Dimension of X
      real, dimension(n), intent(in)      :: X !< Variables
      integer, intent(in)                 :: nparam !< Dimension of param
      real, dimension(nparam), intent(in) :: param !< Parameter vector
      real, intent(in)                    :: of !< Objective function value
      real, dimension(n), intent(in)      :: dofdX !< Differential of objective function
      real, intent(in)                    :: of_old !< Old objective function value
      real                                :: error !< Calculated error
    end function error_function
  end interface

  public :: NO_MOD_NEWTON
  public :: optim_param
  public :: optimize, setX, prematureReturn
  public :: nelmin
  public :: error_function

contains

  !-----------------------------------------------------------------------------
  !> Interface for optimizer
  !>
  !> \author MHA, February 2012
  !-----------------------------------------------------------------------------
  subroutine optimize(optim,objective,diff,x,param,limit,premterm,getSize,setXvar,&
       error_fun)
    implicit none
    !> Solver information
    type(optim_param), intent(inout)   :: optim
    !> The non-linear function F(x)
    real, external                     :: objective
    !> Function to evaluate the differentials.
    external                           :: diff
    !> Function to limit search
    external                           :: limit
    !> Test for premature termination
    logical, external                  :: premterm
    !> On entry, initial condition. On exit, solution
    real, dimension(:), intent(inout)  :: x
    !> Paramaters used by objective, diff, limit and premterm
    real, dimension(:), intent(inout)  :: param
    !> Get size of problem (Usually size(x))
    integer, external                  :: getSize
    !> Function to update variables
    external                           :: setXvar
    !< Function to calculate error
    procedure(error_function), optional:: error_fun
    !
    select case(optim%ioptim)
    case(NO_MOD_NEWTON)
      call mod_newton(optim,objective,diff,x,param,limit,premterm,getSize,&
           setXvar,error_fun)
    case default
      stop "optimizers::optimize: No such optimizer!"
    end select

  end subroutine optimize

  !-----------------------------------------------------------------------------
  !> Solve min F(x) using modefied Newton method,
  !!
  !!  \f$ x^{n+1} = x^n - H(x^n)^{-1} dF(x^n) \f$.
  !!
  !! where H is the Hessian of F, and dF is the derivative of F.
  !!
  !! \author MHA, February 2012
  !-----------------------------------------------------------------------------
  subroutine mod_newton(optim,objective,diff,x,param,limit,premterm,getSize,setXvar,error_fun)
    implicit none
    type(optim_param), intent(inout)      :: optim     !< Solver information
    real, dimension(:),     intent(inout) :: x         !< Variables
    real, dimension(:),     intent(inout) :: param     !< Paramaters used by objective, diff, limit and premterm
    real, external                        :: objective !< The non-linear function F(x)
    external                              :: diff      !< Function to evaluate the differentials.
    external                              :: limit     !< Function to limit search
    logical, external                     :: premterm  !< Test for premature termination
    integer, external                     :: getSize   !< Get size of problem (size(x)?)
    external                              :: setXvar   !< Function to update variables
    procedure(error_function), optional   :: error_fun !< Function to calculate error
    !
    ! Locals
    real, dimension(size(param)) :: param_old !< Copy of param
    real, dimension(size(x),size(x)) :: H, Hcpy
    real, dimension(size(x))         :: dOFdX,dX,X_old
    real, dimension(3*size(x))         :: work
    integer, dimension(size(x)) :: ipiv
    integer :: iter,i,info,n,ii,nparam
    integer, dimension(1) :: imax
    real :: of, of_old, error, dum
    real :: alpha, wolfe
    real, dimension(size(x),size(x)) :: VL, VR
    real, dimension(size(x)) :: WI, WR
    real, dimension(size(x),size(x)) :: L !Lower triangular matrix
    real, dimension(size(x)) :: D !Diagonal matrix
    real, dimension(size(x)) :: E !Diagonal of mofification matrix
    logical :: call_error_fun
    if (present(error_fun)) then
      call_error_fun = .true.
    else
      call_error_fun = .false.
    endif
    n=size(x)
    nparam=size(param)
    optim%exitflag = 0
    error = -1.0
    do iter=1,optim%max_iter
      call diff(x,param,of,dOFdX,H)
      n = getSize(param)
      if (premterm(x,param,of,dOFdX)) then
        if (verbose) then
          print *,'optimizers::mod_newton: Premature return'
        endif
        optim%iter = iter
        optim%of = of
        optim%error = error
        optim%exitflag = -1
        return
      endif
      Hcpy(1:n,1:n) = H(1:n,1:n)

      if (optim%testEigenvalues) then
        call DGEEV('N', 'N', n, Hcpy, n, WR, WI, VL, n, VR, n, WORK, 3*N, INFO )
        print *,'Eigenvalues, real part: ',wr
        print *,'Eigenvalues, imaginary part: ',wi
        Hcpy(1:n,1:n) = H(1:n,1:n)
      end if
      ! Solve equation system
      dX(1:n) = -dOFdX(1:n)
      if (n > 1) then
        call dsysv('u', n, 1, H(1:n,1:n), n, ipiv, DX, n, work, 3*n, info)
        if (info.ne.0 .OR. sum(DX(1:n)*dOFdX(1:n)) >= 0.0) then
          if (verbose) then
            print *,'mod_newton: Need to do modefied Cholesky'
          endif
          ! Need to ensure a decent direction
          DX(1:n) = -dOFdX(1:n)
          H(1:n,1:n) = Hcpy(1:n,1:n)
          call solveModefiedCholesky(n,H(1:n,1:n),DX)
          if (optim%testEigenvalues) then
            call modefiedCholeskyDecompNoPerm(Hcpy,n,L,D,E)
            do ii = 1, n
              Hcpy(ii,ii) =  Hcpy(ii,ii) + E(ii)
            enddo
            call DGEEV('N', 'N', n, Hcpy, n, WR, WI, VL, n, VR, n, WORK, 3*N, INFO )
            print *,'Eigenvalues modified'
            print *,'Eigenvalues, real part: ',wr
            print *,'Eigenvalues, imaginary part: ',wi
          end if
        endif
      else
        ! Single variable
        if (H(1,1) <= 0.0) then
          ! Need positive definite matrix
          H(1,1) = machine_prec*1000.0
        endif
        DX = -dOFdX/H(1,1)
      endif
      !
      ! Limit search
      call limit(X,param,dX)
      !
      X_old(1:n) = X(1:n)
      param_old(1:nparam) = param(1:nparam)
      alpha = 1.0
      call setXvar(n,nparam,X(1:n),dX(1:n),param,alpha)
      of_old = of
      of = objective(x,param)
      ! Do simple line search to "ensure" reduction in objective function
      wolfe = optim%wolfe*sum(DX(1:n)*dOFdX(1:n))
      do i=1,optim%max_line_search_iter
        if (optim%line_search_control) then
          if (int(param(nparam)) .eq. 0) then
            exit ! Exit from do loop. Line search skipped.
          endif
        endif
        if (of >= of_old + alpha*wolfe) then
          alpha = alpha*0.5**i
          X(1:n) = X_old(1:n)
          param(1:nparam) = param_old(1:nparam)
          call setXvar(n,nparam,X(1:n),dX(1:n),param,alpha)
          of = objective(X,param)
        else
          exit ! Exit from do loop
        endif
      enddo

      ! Calculate error
      if (call_error_fun) then
        error = error_fun(n,X(1:n),nparam,param,of,dofdX(1:n),of_old)
      else
        if (optim%gradient_termination) then
          dOFdX(1:n) = abs(dOFdX(1:n))
          imax = maxloc(dOFdX(1:n))
          error = dOFdX(imax(1))
        else
          if (abs(of_old) < 0.99*optim%rel_tol) then
            dum = 1.0
          else
            dum = 0.0
          endif
          error = abs((of - of_old + dum)/(of_old + dum) - dum)
        endif
      endif

      ! Test for convergence
      if (error < optim%rel_tol) then
        if (verbose) then
          print *,'optimizers::mod_newton: Converged. Number of iterations: ', iter
        endif
        optim%iter = iter
        optim%of = of
        optim%error = error
        return
      endif
    enddo

    if (verbose) then
      print *,'optimizers::mod_newton:  The modified Newton minimizer did not converge.'
      print *,'Differential ',dOFdX
      !call exit(1)
    endif
    optim%exitflag = 1
    optim%of = of
    optim%error = error
    optim%iter = optim%max_iter
  end subroutine mod_newton

  !-----------------------------------------------------------------------------
  !> Set variables
  !>
  !> \author MH, 2013-02-27
  !-----------------------------------------------------------------------------
  subroutine setX(n,nparam,X,dX,param,alpha)
    implicit none
    integer, intent(in) :: n, nparam !< Problem dimension
    real, dimension(n), intent(inout) :: X !< Variables
    real, dimension(n), intent(in)    :: dX !< Change in variables
    real, dimension(nparam), intent(inout) :: param !< Parameter vector
    real, intent(in) :: alpha !< dX scaling
    !
    X(1:n) = X(1:n) + alpha*dX(1:n)
    !
  end subroutine setX

  !-----------------------------------------------------------------------------
  !> No premature return.
  !>
  !> \author MH, 2013-02-27
  !-----------------------------------------------------------------------------
  function prematureReturn(X,param,of,dofdX) result(doReturn)
    implicit none
    real, dimension(:), intent(in)    :: X !< Vapour mole numbers [mole]
    real, dimension(:), intent(in)    :: param !< Parameter vector
    real, intent(in)                  :: of !< Objective function value
    real, dimension(:), intent(in)    :: dofdX !< Differential of objective function
    logical                           :: doReturn !< Terminate minimization?
    !
    doReturn = .false.
  end function prematureReturn

  !-----------------------------------------------------------------------------
  !> Solve \f$ A X = B \f$ using Gill-Murray modified Cholesky factorization of A
  !>
  !> \author MHA, January 2012
  !-----------------------------------------------------------------------------
  subroutine solveModefiedCholesky(n,A,B)
    implicit none
    integer, intent(in) :: n ! Matrix dimension
    real, dimension(n,n), intent(inout) :: A ! Symmetric matrix to be factorized
    real, dimension(n), intent(inout) :: B ! Right hand side
    ! Locals
    real, dimension(n,n) :: L ! Lower triangular matrix
    real, dimension(n) :: D ! Diagonal matrix
    real, dimension(n) :: E ! Diagonal of mofification matrix
    real, parameter :: one = 1.0E+0
    integer :: i

    call modefiedCholeskyDecompNoPerm(A,n,L,D,E)
    ! Solve: L*D*L**T*X = B
    ! Solve L*X = B, overwriting B with X.
    call dtrsm( 'Left', 'Lower', 'No transpose', 'Non-unit', n, &
         1, one, L, n, B, n )
    ! Solve D*X = B, overwriting B with X.
    do i=1,n
      B(i) = B(i)/D(i)
    enddo
    ! Solve L**T*X = B, overwriting B with X.
    call dtrsm( 'Left', 'Lower', 'Transpose', 'Non-unit', n, 1, &
         one, L, n, B, n )

  end subroutine solveModefiedCholesky

  !-----------------------------------------------------------------------------
  !> Method for testing Gill-Murray modified Cholesky (LDL) factorization of A
  !>
  !> \author MHA, January 2012
  !-----------------------------------------------------------------------------
  subroutine testModefiedCholeskyDecomp()
    implicit none
    integer, parameter :: n = 5 ! Matrix dimension
    real, dimension(n,n) :: L, LT ! Lower triangular matrix
    real, dimension(n) :: D ! Diagonal matrix
    real, dimension(n) :: E ! Diagonal of mofification matrix
    real, dimension(n,n) :: DD ! Diagonal matrix
    real, dimension(n,n) :: EE ! Mofification matrix
    real, dimension(n) :: EIG ! Eigenvalues
    real, dimension(n) :: B1=(/1.0, 2.0, 3.0, 4.0, 5.0/) ! RHS1
    real, dimension(n) :: B2=(/1.0, 2.0, 3.0, 4.0, 5.0/) ! RHS2
    real, dimension(3*n) :: work ! Work array
    real, dimension(n,n) :: Acpy ! Copy of A
    integer, dimension(n) :: ipiv
    real :: temp
    integer :: i,j, LWORK, INFO
    ! Symmetric matrix to be factorized
    real, dimension(n,n) :: A
    A(1,:) =  (/36.0, 12.0, 30.0, 6.0, 18.0/)
    A(2,:) = (/12.0, 20.0, 2.0, 10.0, 22.0/)
    A(3,:) = (/30.0, 2.0, 29.0, 1.0, 7.0/)
    A(4,:) = (/6.0, 10.0, 1.0, 14.0, 20.0/)
    A(5,:) = (/18.0, 22.0, 7.0, 20.0, 40.0/)

    Acpy = A
    LWORK=3*n
    INFO = 0
    call DSYEV('N', 'U', N, Acpy, N, EIG, WORK, LWORK, INFO )
    print *,EIG

    call modefiedCholeskyDecompNoPerm(A,n,L,D,E)

    print *,L(1,:)
    print *,L(2,:)
    print *,L(3,:)
    print *,L(4,:)
    print *,L(5,:)
    print *,D
    print *,E

    EE = 0.0
    DD = 0.0

    ! Transpose
    LT = L
    do j = 1,n
      DD(j,j) = D(j)
      EE(j,j) = E(j)
      do i = 1,j-1
        temp = LT(i,j)
        LT(i,j) = LT(j,i)
        LT(j,i) = temp
      end do
    end do

    L = MATMUL(L,DD)
    L = MATMUL(L,LT)

    Acpy = L
    INFO = 0
    call DSYEV('N', 'U', N, Acpy, N, EIG, WORK, LWORK, INFO )
    print *,EIG

    L = A - L
    print *,L(1,:)
    print *,L(2,:)
    print *,L(3,:)
    print *,L(4,:)
    print *,L(5,:)

    Acpy = A + EE
    call dsysv('u', n, 1, Acpy, n, ipiv, B1, n, work, 3*n, info)
    print *,B1

    Acpy = A
    call solveModefiedCholesky(n,Acpy,B2)
    print *,B2

  end subroutine testModefiedCholeskyDecomp

  !-----------------------------------------------------------------------------
  !> Compute Gill-Murray modified Cholesky (LDL) factorization of A.
  !!
  !! If A is negative definite, or singular, the matrix is modified:
  !!
  !! \f$ A + E = L D L^T \f$
  !!
  !! Where E is a diagonal small norm matrix making A+E positive definite.
  !!
  !! \author MHA, January 2012
  !-----------------------------------------------------------------------------
  subroutine modefiedCholeskyDecomp(A,n,L,D,P)
    implicit none
    integer, intent(in) :: n !< Symmetric matrix dimension
    real, dimension(n,n), intent(inout) :: A !< Symmetric matrix to be factorized
    real, dimension(n,n), intent(out) :: L !< Lower triangular matrix
    real, dimension(n), intent(out) :: D !< Diagonal matrix
    integer, dimension(n), intent(out) :: P !< Permutations
    !
    ! Locals
    real, dimension(n,n) :: C
    real, parameter :: mu = machine_prec*1.0e3
    real :: gamma, delta, beta, xi, theta, temp, maxVal
    integer :: i,j,s,q,iTemp

    if (n <= 2) then
      write(*,*) 'util::modefiedCholeskyDecomp - Intended for matrices with n>2'
      call stoperror('')
    endif

    ! Initialize matrices and vectors
    L(:,:) = 0.0
    D(:) = 0.0
    C(:,:) = 0.0
    gamma = 0.0
    do i = 1,n
      L(i,i) = 1.0
      c(i,i) = A(i,i)
      gamma = max(gamma, abs(A(i,i)))
      P(i) = i
    enddo

    ! Compute modification parameters
    xi = 0.0
    do i = 1,n
      do j = 1,n
        if (j /= i) then
          xi = max(xi, abs(A(i,j)))
        endif
      end do
    enddo
    delta = mu*max(gamma+xi,1.0)
    beta = max(gamma, mu)
    beta = max(beta, xi/sqrt(n**2-1.0))
    beta = sqrt(beta)

    do j = 1,n
      ! Find q that results in Best Permutation with j
      maxVal = abs(C(j,j))
      q = j
      do i = j+1,n
        if (maxVal < abs(C(i,i))) then
          maxVal = abs(C(i,i))
          q = i
        endif
      end do

      ! Permute d,c,l,a
      temp = d(q)
      d(q) = d(j)
      d(j) = temp
      iTemp = P(q)
      P(q) = P(j)
      P(j) = iTemp
      call interChangeRow(C,n,j,q)
      call interChangeColumn(L,n,j,q)
      call interChangeRow(L,n,j,q)
      call interChangeColumn(L,n,j,q)
      call interChangeRow(A,n,j,q)
      call interChangeColumn(A,n,j,q)

      do s = 1,j-1
        l(j,s) = c(j,s)/d(s)
      enddo
      do i = j+1,n
        c(i,j) = A(i,j) - sum(l(j,1:j-1)*c(i,1:j-1))
      enddo
      theta = 0.0
      if (j < n) then
        do s = j+1,n
          theta = max(theta, abs(c(s,j)))
        enddo
      endif
      d(j) = max(abs(c(j,j)), (theta/beta)**2)
      d(j) = max(d(j), delta)
      if (j < n) then
        do i = j+1,n
          c(i,i) = c(i,i) - (c(i,j)**2)/d(j)
        end do
      end if
    end do

  end subroutine modefiedCholeskyDecomp

  !-----------------------------------------------------------------------------
  !> Swap row j and q in matrix A
  !>
  !> \author MHA, January 2012
  !-----------------------------------------------------------------------------
  subroutine interChangeRow(A,n,j,q)
    implicit none
    integer, intent(in) :: n !< Symmetric matrix dimension
    integer, intent(in) :: j !< Row index
    integer, intent(in) :: q !< Row index
    real, dimension(n,n), intent(inout) :: A !< Symmetric matrix
    !
    ! Locals
    real, dimension(n) :: row
    row(:) = A(j,:)
    A(j,:) = A(q,:)
    A(q,:) = row(:)
  end subroutine interChangeRow

  !-----------------------------------------------------------------------------
  !> Swap column j and q in matrix A
  !>
  !> \author MHA, January 2012
  !-----------------------------------------------------------------------------
  subroutine interChangeColumn(A,n,j,q)
    implicit none
    integer, intent(in) :: n !< Symmetric matrix dimension
    integer, intent(in) :: j !< Column index
    integer, intent(in) :: q !< Column index
    real, dimension(n,n), intent(inout) :: A !< Symmetric matrix
    !
    ! Locals
    real, dimension(n,1) :: column
    column(:,1) = A(:,j)
    A(:,j) = A(:,q)
    A(:,q) = column(:,1)
  end subroutine interChangeColumn

  !-----------------------------------------------------------------------------
  !> Compute Gill-Murray modified Cholesky (LDL) factorization of A.
  !!
  !! If A is negative definite, or singular, the matrix is modified:
  !!
  !! \f$ A + E = L D L^T \f$
  !!
  !! Where E is a diagonal small norm matrix making A+E positive definite.
  !!
  !! \author MHA, January 2012
  !-----------------------------------------------------------------------------
  subroutine modefiedCholeskyDecompNoPerm(A,n,L,D,E)
    implicit none
    integer, intent(in) :: n !< Symmetric matrix dimension
    real, dimension(n,n), intent(in) :: A !< Symmetric matrix to be factorized
    real, dimension(n,n), intent(out) :: L !< Lower triangular matrix
    real, dimension(n), intent(out) :: D !< Diagonal matrix
    real, dimension(n), intent(out) :: E !< Diagonal of modification matrix
    !
    ! Locals
    real, dimension(n,n) :: C
    real, parameter :: mu = machine_prec*1.0e2
    real :: gamma, delta, beta, xi, theta
    integer :: i,j,s

    if (n < 2) then
      write(*,*) 'util::modefiedCholeskyDecompNoPerm - Intended for matrices with n>=2'
      call stoperror('')
    endif

    ! Initialize matrices and vectors
    L(:,:) = 0.0
    D(:) = 0.0
    C(:,:) = 0.0
    E(:) = 0.0
    gamma = 0.0
    do i = 1,n
      L(i,i) = 1.0
      C(i,i) = A(i,i)
      gamma = max(gamma, abs(A(i,i)))
    enddo

    ! Compute modification parameters
    xi = 0.0
    do i = 1,n
      do j = 1,n
        if (j /= i) then
          xi = max(xi, abs(A(i,j)))
        endif
      end do
    enddo
    delta = mu*max(gamma+xi,1.0)
    beta = max(gamma, mu)
    beta = max(beta, xi/sqrt(n**2-1.0))
    beta = sqrt(beta)

    do j = 1,n
      do s = 1,j-1
        L(j,s) = C(j,s)/D(s)
      enddo
      do i = j+1,n
        C(i,j) = A(i,j) - sum(L(j,1:j-1)*C(i,1:j-1))
      enddo
      theta = 0.0
      if (j < n) then
        do s = j+1,n
          theta = max(theta, abs(c(s,j)))
        enddo
      endif
      D(j) = max(abs(C(j,j)), (theta/beta)**2)
      D(j) = max(D(j), delta)

      E(j)=D(j)-C(j,j)
      if (j < n) then
        do i = j+1,n
          C(i,i) = C(i,i) - (C(i,j)**2)/D(j)
        end do
      end if
    end do

  end subroutine modefiedCholeskyDecompNoPerm


  !-----------------------------------------------------------------------------
  !> NELMIN minimizes a function using the Nelder-Mead algorithm
  !!
  !! \author KEGT
  !-----------------------------------------------------------------------------
  subroutine nelmin ( fn, n, start, xmin, ynewlo, reqmin, step, konvge, kcount, &
       icount, numres, ifault )

    !*****************************************************************************80
    !
    !! NELMIN minimizes a function using the Nelder-Mead algorithm.
    !
    !  Discussion:
    !
    !    This routine seeks the minimum value of a user-specified function.
    !
    !    Simplex function minimisation procedure due to Nelder+Mead(1965),
    !    as implemented by O'Neill(1971, Appl.Statist. 20, 338-45), with
    !    subsequent comments by Chambers+Ertel(1974, 23, 250-1), Benyon(1976,
    !    25, 97) and Hill(1978, 27, 380-2)
    !
    !    The function to be minimized must be defined by a function of
    !    the form
    !
    !      function fn ( x, f )
    !      real ( kind = 8 ) fn
    !      real ( kind = 8 ) x(*)
    !
    !    and the name of this subroutine must be declared EXTERNAL in the
    !    calling routine and passed as the argument FN.
    !
    !    This routine does not include a termination test using the
    !    fitting of a quadratic surface.
    !
    !  Modified:
    !
    !    27 February 2008
    !
    !  Author:
    !
    !    FORTRAN77 version by R ONeill
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    John Nelder, Roger Mead,
    !    A simplex method for function minimization,
    !    Computer Journal,
    !    Volume 7, 1965, pages 308-313.
    !
    !    R ONeill,
    !    Algorithm AS 47:
    !    Function Minimization Using a Simplex Procedure,
    !    Applied Statistics,
    !    Volume 20, Number 3, 1971, pages 338-345.
    !
    !  Parameters:
    !
    !    Input, external FN, the name of the function which evaluates
    !    the function to be minimized.
    !
    !    Input, integer ( kind = 4 ) N, the number of variables.
    !
    !    Input/output, real ( kind = 8 ) START(N).  On input, a starting point
    !    for the iteration.  On output, this data may have been overwritten.
    !
    !    Output, real ( kind = 8 ) XMIN(N), the coordinates of the point which
    !    is estimated to minimize the function.
    !
    !    Output, real ( kind = 8 ) YNEWLO, the minimum value of the function.
    !
    !    Input, real ( kind = 8 ) REQMIN, the terminating limit for the variance
    !    of function values.
    !
    !    Input, real ( kind = 8 ) STEP(N), determines the size and shape of the
    !    initial simplex.  The relative magnitudes of its elements should reflect
    !    the units of the variables.
    !
    !    Input, integer ( kind = 4 ) KONVGE, the convergence check is carried out
    !    every KONVGE iterations.
    !
    !    Input, integer ( kind = 4 ) KCOUNT, the maximum number of function
    !    evaluations.
    !
    !    Output, integer ( kind = 4 ) ICOUNT, the number of function evaluations
    !    used.
    !
    !    Output, integer ( kind = 4 ) NUMRES, the number of restarts.
    !
    !    Output, integer ( kind = 4 ) IFAULT, error indicator.
    !    0, no errors detected.
    !    1, REQMIN, N, or KONVGE has an illegal value.
    !    2, iteration terminated because KCOUNT was exceeded without convergence.
    !
    implicit none

    integer ( kind = 4 ) n

    real    ( kind = 8 ), parameter :: ccoeff = 0.5D+00
    real    ( kind = 8 ) del
    real    ( kind = 8 ) dn
    real    ( kind = 8 ) dnn
    real    ( kind = 8 ), parameter :: ecoeff = 2.0D+00
    real    ( kind = 8 ), parameter :: eps = 0.001D+00
    real    ( kind = 8 ), external :: fn
    integer ( kind = 4 ) i
    integer ( kind = 4 ) icount
    integer ( kind = 4 ) ifault
    integer ( kind = 4 ) ihi
    integer ( kind = 4 ) ilo
    integer ( kind = 4 ) j
    integer ( kind = 4 ) jcount
    integer ( kind = 4 ) kcount
    integer ( kind = 4 ) konvge
    integer ( kind = 4 ) l
    integer ( kind = 4 ) nn
    integer ( kind = 4 ) numres
    real    ( kind = 8 ) p(n,n+1)
    real    ( kind = 8 ) p2star(n)
    real    ( kind = 8 ) pbar(n)
    real    ( kind = 8 ) pstar(n)
    real    ( kind = 8 ), parameter :: rcoeff = 1.0D+00
    real    ( kind = 8 ) reqmin
    real    ( kind = 8 ) rq
    real    ( kind = 8 ) start(n)
    real    ( kind = 8 ) step(n)
    real    ( kind = 8 ) x
    real    ( kind = 8 ) xmin(n)
    real    ( kind = 8 ) y(n+1)
    real    ( kind = 8 ) y2star
    real    ( kind = 8 ) ylo
    real    ( kind = 8 ) ynewlo
    real    ( kind = 8 ) ystar
    real    ( kind = 8 ) z
    !
    !  Check the input parameters.
    !
    if ( reqmin <= 0.0D+00 ) then
       ifault = 1
       return
    end if

    if ( n < 1 ) then
       ifault = 1
       return
    end if

    if ( konvge < 1 ) then
       ifault = 1
       return
    end if

    icount = 0
    numres = 0

    jcount = konvge
    dn = real ( n, kind = 8 )
    nn = n + 1
    dnn = real ( nn, kind = 8 )
    del = 1.0D+00
    rq = reqmin * dn
    !
    !  Initial or restarted loop.
    !
    do

       do i = 1, n
          p(i,nn) = start(i)
       end do

       y(nn) = fn ( start )
       icount = icount + 1

       do j = 1, n
          x = start(j)
          start(j) = start(j) + step(j) * del
          do i = 1, n
             p(i,j) = start(i)
          end do
          y(j) = fn ( start )
          icount = icount + 1
          start(j) = x
       end do
       !
       !  The simplex construction is complete.
       !
       !  Find highest and lowest Y values.  YNEWLO = Y(IHI) indicates
       !  the vertex of the simplex to be replaced.
       !
       ylo = y(1)
       ilo = 1

       do i = 2, nn
          if ( y(i) < ylo ) then
             ylo = y(i)
             ilo = i
          end if
       end do
       !
       !  Inner loop.
       !
       do

          if ( kcount <= icount ) then
             exit
          end if

          ynewlo = y(1)
          ihi = 1

          do i = 2, nn
             if ( ynewlo < y(i) ) then
                ynewlo = y(i)
                ihi = i
             end if
          end do
          !
          !  Calculate PBAR, the centroid of the simplex vertices
          !  excepting the vertex with Y value YNEWLO.
          !
          do i = 1, n
             z = 0.0D+00
             do j = 1, nn
                z = z + p(i,j)
             end do
             z = z - p(i,ihi)
             pbar(i) = z / dn
          end do
          !
          !  Reflection through the centroid.
          !
          do i = 1, n
             pstar(i) = pbar(i) + rcoeff * ( pbar(i) - p(i,ihi) )
          end do

          ystar = fn ( pstar )
          icount = icount + 1
          !
          !  Successful reflection, so extension.
          !
          if ( ystar < ylo ) then

             do i = 1, n
                p2star(i) = pbar(i) + ecoeff * ( pstar(i) - pbar(i) )
             end do

             y2star = fn ( p2star )
             icount = icount + 1
             !
             !  Check extension.
             !
             if ( ystar < y2star ) then

                do i = 1, n
                   p(i,ihi) = pstar(i)
                end do

                y(ihi) = ystar
                !
                !  Retain extension or contraction.
                !
             else

                do i = 1, n
                   p(i,ihi) = p2star(i)
                end do

                y(ihi) = y2star

             end if
             !
             !  No extension.
             !
          else

             l = 0
             do i = 1, nn
                if ( ystar < y(i) ) then
                   l = l + 1
                end if
             end do

             if ( 1 < l ) then

                do i = 1, n
                   p(i,ihi) = pstar(i)
                end do

                y(ihi) = ystar
                !
                !  Contraction on the Y(IHI) side of the centroid.
                !
             else if ( l == 0 ) then

                do i = 1, n
                   p2star(i) = pbar(i) + ccoeff * ( p(i,ihi) - pbar(i) )
                end do

                y2star = fn ( p2star )
                icount = icount + 1
                !
                !  Contract the whole simplex.
                !
                if ( y(ihi) < y2star ) then

                   do j = 1, nn
                      do i = 1, n
                         p(i,j) = ( p(i,j) + p(i,ilo) ) * 0.5D+00
                         xmin(i) = p(i,j)
                      end do
                      y(j) = fn ( xmin )
                      icount = icount + 1
                   end do

                   ylo = y(1)
                   ilo = 1

                   do i = 2, nn
                      if ( y(i) < ylo ) then
                         ylo = y(i)
                         ilo = i
                      end if
                   end do

                   cycle
                   !
                   !  Retain contraction.
                   !
                else

                   do i = 1, n
                      p(i,ihi) = p2star(i)
                   end do
                   y(ihi) = y2star

                end if
                !
                !  Contraction on the reflection side of the centroid.
                !
             else if ( l == 1 ) then

                do i = 1, n
                   p2star(i) = pbar(i) + ccoeff * ( pstar(i) - pbar(i) )
                end do

                y2star = fn ( p2star )
                icount = icount + 1
                !
                !  Retain reflection?
                !
                if ( y2star <= ystar ) then

                   do i = 1, n
                      p(i,ihi) = p2star(i)
                   end do
                   y(ihi) = y2star

                else

                   do i = 1, n
                      p(i,ihi) = pstar(i)
                   end do
                   y(ihi) = ystar

                end if

             end if

          end if
          !
          !  Check if YLO improved.
          !
          if ( y(ihi) < ylo ) then
             ylo = y(ihi)
             ilo = ihi
          end if

          jcount = jcount - 1

          if ( 0 < jcount ) then
             cycle
          end if
          !
          !  Check to see if minimum reached.
          !
          if ( icount <= kcount ) then

             jcount = konvge

             z = 0.0D+00
             do i = 1, nn
                z = z + y(i)
             end do
             x = z / dnn

             z = 0.0D+00
             do i = 1, nn
                z = z + ( y(i) - x )**2
             end do

             if ( z <= rq ) then
                exit
             end if

          end if

       end do
       !
       !  Factorial tests to check that YNEWLO is a local minimum.
       !
       do i = 1, n
          xmin(i) = p(i,ilo)
       end do

       ynewlo = y(ilo)

       if ( kcount < icount ) then
          ifault = 2
          exit
       end if

       ifault = 0

       do i = 1, n
          del = step(i) * eps
          xmin(i) = xmin(i) + del
          z = fn ( xmin )
          icount = icount + 1
          if ( z < ynewlo ) then
             ifault = 2
             exit
          end if
          xmin(i) = xmin(i) - del - del
          z = fn ( xmin )
          icount = icount + 1
          if ( z < ynewlo ) then
             ifault = 2
             exit
          end if
          xmin(i) = xmin(i) + del
       end do

       if ( ifault == 0 ) then
          exit
       end if
       !
       !  Restart the procedure.
       !
       do i = 1, n
          start(i) = xmin(i)
       end do

       del = eps
       numres = numres + 1

    end do

    return
  end subroutine nelmin


end module optimizers
