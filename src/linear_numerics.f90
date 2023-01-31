!-----------------------------------------------------------------------------
!> This module contains methods for solving systems of linear equations.
!>
!> \author MH, August 2022
!-----------------------------------------------------------------------------
module linear_numerics
  !
  implicit none
  private

  public :: cg, solveLU, inverse, null_space
  public :: outer_product
  public :: solve_lu_hd

contains

  !-----------------------------------------------------------------------------
  !> A simple CG solver, can be used instead of LAPACK's dgesv for solving linear
  !> systems
  !> \author KEGT
  !-----------------------------------------------------------------------------
  subroutine cg(A,x,b)
    implicit none
    real, intent(in),    dimension(:,:) :: A ! Values of the coefficient matrix
    real, intent(inout), dimension(:)   :: x ! Initial guess and solution
    real, intent(in),    dimension(:)   :: b ! Right hand side

    real :: rho, rho_old,alpha,beta,gamma ! Temporary storage scalars
    real :: norm ! The norm of the error
    real, dimension(size(x)) :: p,q ! Temporary storage vectors
    real, dimension(size(x)) :: r ! Resudial vector
    integer :: iteration=0 ! Iteration counter
    real,parameter :: resid=1e-9 ! Wanted residual
    integer,parameter :: max_iter=1000 ! Maximum iterations
    real, dimension(size(x)) :: Ax ! Matrix-vector product a*x
    integer :: neq,i

    neq = size(x)
    ! Initial residual
    Ax = matmul(A,x)
    r = b - Ax
    ! Main loop
    do
      rho = 0.0
      gamma = 0.0
      iteration = iteration + 1

      do i = 1, neq
        rho = rho + r(i)*r(i)
      end do

      if (iteration > 1) then
        beta = rho / rho_old
        p = r + beta*p
      else
        p = r
      end if

      q = matmul(A,p)

      do i=1,neq
        gamma = gamma  + p(i)*q(i)
      enddo

      alpha = rho / gamma

      x = x + alpha * p
      r = r - alpha * q

      ! Convergence check
      norm = maxval(abs(r))/real(neq)
      if( (norm <= resid) .or. (iteration >= max_iter) ) exit
      rho_old = rho
    end do

  end subroutine cg

  !----------------------------------------------------------------------
  subroutine solveLU(neq,X,A,symmetric,ierr)
    ! Solve using lapack: A*X = B
    ! X = B on entry
    implicit none
    integer, intent(in) :: neq
    real, dimension(neq), intent(inout) :: X
    real, dimension(neq,neq), intent(inout) :: A
    integer, intent(inout) :: ierr
    logical, intent(in) :: symmetric
    ! Lapack variables
    integer, dimension(neq) :: IPIV
    integer :: info
    real, dimension(3*neq) :: work

    if (symmetric) then
      call dsysv('u', neq, 1, A, neq, ipiv, &
           X, neq, work, 3*neq, info)
      if (info /= 0) then
        ierr=2
        return
      endif
    else
      ! Do LU factorization
      call dgetrf(neq,neq,A,neq,ipiv,info)
      if(info /= 0) then
        ierr=2
        return
      endif
      ! backsubstitute
      call dgetrs('n',neq,1,A,neq,ipiv,X,neq,info)
      if(info /= 0) then
        ierr=2
        return
      endif
    endif

  end subroutine solveLU

  !-----------------------------------------------------------------------------
  !> Subroutine to find the inverse of a square matrix
  !> Modified after freely available routine written by Ashwith J. Rego
  !>
  !> \author MH, August 2012
  !-----------------------------------------------------------------------------
  subroutine inverse(matrix, inv, n, errorflag)
    implicit none
    integer, intent(in)  :: n
    integer, intent(out) :: errorflag  !Return error status. -1 for error, 0 for normal
    real, intent(in), dimension(n,n)  :: matrix  !Input matrix
    real, intent(out), dimension(n,n) :: inv     !Inverted matrix

    logical :: flag = .true.
    integer :: i, j, k
    real :: m
    real, dimension(n,2*n) :: augmatrix !augmented matrix

    !Augment input matrix with an identity matrix
    do i = 1, n
      do j = 1, 2*n
        if (j <= n ) then
          augmatrix(i,j) = matrix(i,j)
        else if ((i+n) == j) then
          augmatrix(i,j) = 1
        else
          augmatrix(i,j) = 0
        endif
      end do
    end do

    !Reduce augmented matrix to upper traingular form
    do k =1, n-1
      if (augmatrix(k,k) == 0) then
        flag = .false.
        do i = k+1, n
          if (augmatrix(i,k) /= 0) then
            do j = 1,2*n
              augmatrix(k,j) = augmatrix(k,j)+augmatrix(i,j)
            end do
            flag = .true.
            exit
          endif
          if (flag .eqv. .false.) then
            !print*, "Matrix is non - invertible"
            inv = 0
            errorflag = -1
            return
          endif
        end do
      endif
      do j = k+1, n
        m = augmatrix(j,k)/augmatrix(k,k)
        do i = k, 2*n
          augmatrix(j,i) = augmatrix(j,i) - m*augmatrix(k,i)
        end do
      end do
    end do

    !Test for invertibility
    do i = 1, n
      if (augmatrix(i,i) == 0) then
        !print*, "Matrix is non - invertible"
        inv = 0
        errorflag = -1
        return
      endif
    end do

    !Make diagonal elements as 1
    do i = 1 , n
      m = augmatrix(i,i)
      do j = i , (2 * n)
        augmatrix(i,j) = (augmatrix(i,j) / m)
      end do
    end do

    !Reduced right side half of augmented matrix to identity matrix
    do k = n-1, 1, -1
      do i =1, k
        m = augmatrix(i,k+1)
        do j = k, (2*n)
          augmatrix(i,j) = augmatrix(i,j) -augmatrix(k+1,j) * m
        end do
      end do
    end do

    !store answer
    do i =1, n
      do j = 1, n
        inv(i,j) = augmatrix(i,j+n)
      end do
    end do
    errorflag = 0

  end subroutine inverse

  !-----------------------------------------------------------------------------
  !> Calculate null space of A using SVD
  !>
  !> \author MH, August 2022
  !-----------------------------------------------------------------------------
  subroutine null_space(A,n,x_null,ierr)
    implicit none
    real, dimension(n,n),         intent(inout) :: A !< Symmetric matrix
    integer,                      intent(in)    :: n !< Dimension of A
    real, dimension(n),           intent(inout) :: x_null !< Null space of A
    integer,                      intent(out)   :: ierr !< Error flag
    ! Locals
    real :: S(n), U(n,n), WORK(5*n), VT(n,n)
    integer :: info
    !
    call dgesvd('A','N',n,n,A,n,S,U,n,VT,n,WORK,5*n,info)
    if (info == 0) then
      ierr = 0
      x_null = U(:,n)
    else
      ierr = 1
      x_null = 0
    endif
  end subroutine null_space

  !-----------------------------------------------------------------------------
  !> Calculate outer product
  !>
  !> \author MH, August 2022
  !-----------------------------------------------------------------------------
  subroutine outer_product(A,nA,B,nB,AB)
    real, intent(in) :: A(nA),B(nB)
    real, intent(out) :: AB(nA,nB)
    integer, intent(in) :: nA,nB
    AB = spread(source = A, dim = 2, ncopies = nB) * &
         spread(source = B, dim = 1, ncopies = nA)
  end subroutine outer_product

  !-----------------------------------------------------------------------------
  !> LU decomposition of a N x N matrix A
  !! Numerical Recipes in C++
  !! Second edition,
  !! By W.H. Press, S.A. Teukolsky, W.T. Vetterling and B. P. Flannery,
  !! Cambridge University Press, 2002
  !!
  !! \author MH, January 2023
  !-----------------------------------------------------------------------------
  subroutine ludcmp_hd(a,n,indx,d,ierr)
    use hyperdual_mod
    use numconstants, only: machine_prec
    implicit none
    type(hyperdual), intent(inout), dimension(n,n) :: a
    integer, intent(in) :: n
    integer, intent(out) :: d, ierr
    integer, intent(out), dimension(n) :: indx
    ! Locals
    type(hyperdual) :: amax, dum, summ, vv(n)
    integer :: i, j, k, imax

    !d=1
    ierr=0

    do i=1,n
      amax=0.0_dp
      do j=1,n
        if (abs(a(i,j)) > amax) amax = abs(a(i,j))
      enddo
      if(amax < machine_prec) then
        ierr = 1
        return
      end if
      vv(i) = 1.0_dp / amax
    enddo

    do j=1,n
      do i=1,j-1
        summ = a(i,j)
        do k=1,i-1
          summ = summ - a(i,k)*a(k,j)
        enddo
        a(i,j) = summ
      enddo
      amax = 0.0_dp
      do i=j,n
        summ = a(i,j)
        do k=1,j-1
          summ = summ - a(i,k)*a(k,j)
        enddo
        a(i,j) = summ
        dum = vv(i)*abs(summ)
        if(dum >= amax) then
          imax = i
          amax = dum
        end if
      enddo

      if(j /= imax) then
        do k=1,n
          dum = a(imax,k)
          a(imax,k) = a(j,k)
          a(j,k) = dum
        enddo
        !d = -d
        vv(imax) = vv(j)
      end if

      indx(j) = imax
      if(abs(a(j,j)) < machine_prec) a(j,j) = machine_prec

      if(j /= n) then
        dum = 1.0_dp / a(j,j)
        do i=j+1,n
          a(i,j) = a(i,j)*dum
        enddo
      end if
    enddo

  end subroutine ludcmp_hd

  !-----------------------------------------------------------------------------
  !> Back substitute for LU of a x = b
  !! Numerical Recipes in C++
  !! Second edition,
  !! By W.H. Press, S.A. Teukolsky, W.T. Vetterling and B. P. Flannery,
  !! Cambridge University Press, 2002
  !!
  !! \author MH, January 2023
  !-----------------------------------------------------------------------------
  subroutine lubksb_hd(a, n, indx, b)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: n
    type(hyperdual), intent(in), dimension(n,n) :: a
    integer, intent(in), dimension(n) :: indx
    type(hyperdual), intent(inout), dimension(n) :: b
    ! Locals
    integer :: i, j, ii, ll
    type(hyperdual) ::  summ

    ii = 0
    do i=1,n
      ll = indx(i)
      summ = b(ll)
      b(ll) = b(i)
      if(ii /= 0) then
        do j=ii,i-1
          summ = summ - a(i,j)*b(j)
        enddo
      else if(summ%f0 /= 0.0_dp .or. &
           summ%f1 /= 0.0_dp .or. &
           summ%f2 /= 0.0_dp .or. &
           summ%f12 /= 0.0_dp) then ! Make sure differentials are propagated correctly
        ii = i
      end if
      b(i) = summ
    enddo

    do i=n,1,-1
      summ = b(i)
      if(i < n) then
        do j=i+1,n
          summ = summ - a(i,j)*b(j)
        enddo
      end if
      b(i) = summ / a(i,i)
    enddo

  end subroutine lubksb_hd

  !-----------------------------------------------------------------------------
  !> Solve a x = b
  !!
  !! \author MH, January 2023
  !-----------------------------------------------------------------------------
  subroutine solve_lu_hd(a, n, b, ierr)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: n
    type(hyperdual), intent(inout), dimension(n,n) :: a
    type(hyperdual), intent(inout), dimension(n) :: b
    integer, intent(out) :: ierr
    ! Locals
    integer, dimension(n) :: indx
    integer :: d
    call ludcmp_hd(a,n,indx,d,ierr)
    if (ierr /= 0) return
    call lubksb_hd(a, n, indx, b)

  end subroutine solve_lu_hd

end module linear_numerics
