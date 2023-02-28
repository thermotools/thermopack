module utilities
  ! Contains various utility functions
  ! It can typically be used for functions/subroutines that can be useful
  ! throughout the program, to make it easier to follow the DRY principle
  ! = (Don't Repeat Yourself)
  !
  ! HL, 2013-11-08
  ! MH,
  ! EA
  implicit none
  private
  save

  public :: safe_exp
  public :: rand_seed, random, newunit, linspace, is_numerically_equal
  public :: normalize, boolean, isXwithinBounds
  public :: allocate_nc, allocate_nc_x_nc, deallocate_real, deallocate_real_2
  public :: get_thread_index, test_tvn_method

  !-----------------------------------------------------------------------------
  !> Exponential function which will return "huge" instead of overflowing.
  !! Generic interface for both reals and arrays of reals.
  !! \author EA, 2013-07-19
  !-----------------------------------------------------------------------------
  interface safe_exp
    module procedure safe_exp_real, safe_exp_array
  end interface safe_exp

contains

  subroutine rand_seed(random)
    ! Seeds the random generator
    ! The seed is random unless random==.false.
    ! by default random==.true.
    implicit none
    logical, intent(in), optional :: random
    integer, allocatable :: seed(:)
    integer :: i, n, un, istat, dt(8), pid, t(2), s
    integer(8) :: count, tms
    ! Seed without random seed
    if(present(random)) then
      if(random .eqv. .false.) then
        call random_seed()
        return
      end if
    end if
    ! Continue to create random seed
    call random_seed(size = n)
    allocate(seed(n))
    ! First try if the OS provides a random number generator
    open(unit=newunit(un), file="/dev/urandom", access="stream", &
         form="unformatted", action="read", status="old", iostat=istat)
    if (istat == 0) then
      read(un) seed
      close(un)
    else
      ! Fallback to XOR:ing the current time and pid. The PID is
      ! useful in case one launches multiple instances of the same
      ! program in parallel.
      call system_clock(count)
      if (count /= 0) then
        t = transfer(count, t)
      else
        call date_and_time(values=dt)
        tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
             + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
             + dt(3) * 24 * 60 * 60 * 60 * 1000 &
             + dt(5) * 60 * 60 * 1000 &
             + dt(6) * 60 * 1000 + dt(7) * 1000 &
             + dt(8)
        t = transfer(tms, t)
      end if
      s = ieor(t(1), t(2))
      !pid = getpid() + 1099279 ! getpid() doesn't work with ifort
      pid = 3854 + 1099279 ! Add a prime
      s = ieor(s, pid)
      if (n >= 3) then
        seed(1) = t(1) + 36269
        seed(2) = t(2) + 72551
        seed(3) = pid
        if (n > 3) then
          seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
        end if
      else
        seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
      end if
    end if
    call random_seed(put=seed)
  end subroutine rand_seed

  real function random()
    ! Returns a random number between 0 and 1.
    ! Simply a useful wrapper for the random_number subroutine
    call random_number(random)
  end function random

  integer function newunit(unit)
    ! This is a simple function to search for an available unit.
    ! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
    ! The UNIT value is returned by the function, and also by the optional
    ! argument. This allows the function to be used directly in an OPEN
    ! statement, and optionally save the result in a local variable.
    ! If no units are available, -1 is returned.
    ! Copied from http://fortranwiki.org/fortran/show/newunit
    integer, intent(out), optional :: unit
    ! local
    integer, parameter :: LUN_MIN=10, LUN_MAX=1000
    logical :: opened
    integer :: lun
    ! begin
    newunit=-1
    do lun=LUN_MIN,LUN_MAX
      inquire(unit=lun,opened=opened)
      if (.not. opened) then
        newunit=lun
        exit
      end if
    end do
    if (present(unit)) unit=newunit
  end function newunit

  subroutine linspace(a,b,n,array)
    ! Make linearly spaced array of n points between a and b,
    ! with the first point being exactly a, and the second being exactly b.
    ! Input:
    real,     intent(in)      :: a
    real,     intent(in)      :: b
    integer,  intent(in)      :: n
    ! Output:
    real,     intent(out)     :: array(n)
    ! Internal:
    real                      :: dx
    integer                   :: i

    dx = (b-a)/real(n-1)
    array(1) = a
    array(n) = b
    do i=2,n-1
      array(i) = array(i-1)+dx
    end do

  end subroutine linspace

  !> True if a=b within tolerance
  logical function is_numerically_equal(a,b,tolerance)
    use numconstants, only: machine_prec
    real, intent(in) :: a,b
    real, optional, intent(in) :: tolerance
    ! Locals:
    real, parameter :: defaultTol = 10.0*machine_prec
    real :: tol
    if (present(tolerance)) then
      tol = tolerance
    else
      tol = abs(a)*defaultTol
    endif
    is_numerically_equal = (abs(a-b) < tol)
  end function is_numerically_equal

    !-----------------------------------------------------------------------------
  !> Exponential function which will return "huge" instead of overflowing.
  !! Version for reals. Called through the generic interface safe_exp.
  !! \author EA, 2013-07-19
  !-----------------------------------------------------------------------------
  function safe_exp_real(x)
    use numconstants, only: expMax
    implicit none
    real, intent(in)  :: x
    real              :: safe_exp_real
    safe_exp_real = exp(min(expMax,x))
  end function safe_exp_real

  !-----------------------------------------------------------------------------
  !> Exponential function which will return "huge" instead of overflowing.
  !! Version for arrays of reals. Called through the generic interface safe_exp.
  !! \author EA, 2013-07-19
  !-----------------------------------------------------------------------------
  function safe_exp_array(x)
    use numconstants, only: expMax
    implicit none
    real, intent(in)  :: x(:)
    real              :: safe_exp_array(size(x))
    safe_exp_array = exp(min(expMax,x))
  end function safe_exp_array

  subroutine normalize(Z)
    implicit none
    real, dimension(:), intent(inout) :: Z
    Z = Z/sum(Z)
  end subroutine normalize

  !< Convert from real to logical
  function boolean(real_bool) result (bool)
    implicit none
    real, intent (in):: real_bool
    logical :: bool
    if (real_bool > 0.0) then
      bool = .true.
    else
      bool = .false.
    endif
  end function boolean

  !> Test: Xmin <= X <= Xmax
  !!
  !! \author MH, 2019-05
  !-------------------------------------------------------------------------
  subroutine isXwithinBounds(n,X,Xmin,Xmax,varlist,errmess,ierr)
    integer, intent(in) :: n
    real, intent(in) :: X(n),Xmin(n),Xmax(n)
    character(len=*), intent(in) :: errmess
    character(len=*), intent(in) :: varlist
    integer, optional, intent(out) :: ierr
    ! Locals
    integer :: i
    logical :: isWithin
    isWithin = .true.
    if (present(ierr)) then
      ierr = 0
    endif
    do i=1,n
      isWithin = (isWithin .and. X(i) >= Xmin(i) .and. X(i) <= Xmax(i))
    enddo
    if (.not. isWithin) then
      print *,trim(varlist)
      print *,"Xmin",Xmin
      print *,"X   ",X
      print *,"Xmax",Xmax
      if (present(ierr)) then
        ierr = 1
        print *,trim(errmess)
      else
        call stoperror(errmess)
      endif
    endif
  end subroutine isXwithinBounds

  !> deallocate real variable
  subroutine deallocate_real(var,var_name)
    real, allocatable, intent(inout) :: var(:)
    character(len=*), intent(in) :: var_name
    ! Locals
    integer :: err
    err = 0
    if (allocated (var)) deallocate (var, stat=err)
    if (err /= 0) call stoperror('deallocate_real: could not deallocate array: '//var_name)
  end subroutine deallocate_real

  !> deallocate real variable
  subroutine deallocate_real_2(var,var_name)
    real, allocatable, intent(inout) :: var(:,:)
    character(len=*), intent(in) :: var_name
    ! Locals
    integer :: err
    err = 0
    if (allocated (var)) deallocate (var, stat=err)
    if (err /= 0) call stoperror('deallocate_real: could not deallocate array: '//var_name)
  end subroutine deallocate_real_2

  !> Allocate nc matrix
  subroutine allocate_nc(var,nc,var_name)
    real, allocatable, intent(inout) :: var(:)
    integer, intent(in) :: nc
    character(len=*), intent(in) :: var_name
    ! Locals
    integer :: err
    err = 0
    if (allocated (var)) deallocate (var, stat=err)
    if (err /= 0) call stoperror('allocate_nc: could not deallocate array: '//var_name)
    allocate (var(nc), stat=err)
    if (err /= 0) call stoperror('allocate_nc: could not allocate array: '//var_name)
  end subroutine allocate_nc

  !> Allocate nc x nc matrix
  subroutine allocate_nc_x_nc(var,nc,var_name)
    real, allocatable, intent(inout) :: var(:,:)
    integer, intent(in) :: nc
    character(len=*), intent(in) :: var_name
    ! Locals
    integer :: err
    err = 0
    if (allocated (var)) deallocate (var, stat=err)
    if (err /= 0) call stoperror('allocate_nc_x_nc: could not deallocate array: '//var_name)
    allocate (var(nc,nc), stat=err)
    if (err /= 0) call stoperror('allocate_nc_x_nc: could not allocate array: '//var_name)
  end subroutine allocate_nc_x_nc

  !< Get thread index when performing OpenMP parallel code execution
  function get_thread_index() result(i_thread)
    !$ use omp_lib, only: omp_get_thread_num
    integer :: i_thread
    i_thread = 1
    !$ i_thread = 1 + omp_get_thread_num()
  end function get_thread_index

  !> Test differentials by two-sided numerical differentiation. External subroutine
  !! assumed to take the following input/output:
  !! fun(real: T, real: V, real: n(:), real: a, real: a_T,
  !!     real: a_V, real: a_n(:), real: a_TT, real: a_TV, real: a_VV,
  !!     real: a_Tn(:), real: a_Vn(:), real: a_nn(:,:))
  !!
  !! \author Morten Hammer
  subroutine test_tvn_method(fun,nc,T,V,n)
    external :: fun
    integer, intent(in) :: nc
    real, intent(in) :: T,V,n(nc)
    ! Locals
    real, parameter :: rel_eps = 1.0e-5
    integer :: i
    real :: a,a_T,a_V,a_TT,a_TV,a_VV
    real :: a_n(nc),a_Tn(nc),a_Vn(nc),a_nn(nc,nc)
    real :: a1,a1_T,a1_V,a1_n(nc)
    real :: a2,a2_T,a2_V,a2_n(nc)
    real :: dT, dV, dn, n1(nc)
    real :: d_TT,d_TV,d_VV
    real :: d_Tn(nc),d_Vn(nc),d_nn(nc,nc)

    call fun(T,V,n,a,a_T,&
         a_V,a_n,a_TT,a_TV,a_VV,a_Tn,a_Vn,a_nn)

    ! Temperature differentials
    dT = T*rel_eps
    call fun(T+dT,V,n,a2,a2_T,a2_V,a2_n,d_TT,d_TV,d_VV,d_Tn,d_Vn,d_nn)
    call fun(T-dT,V,n,a1,a1_T,a1_V,a1_n,d_TT,d_TV,d_VV,d_Tn,d_Vn,d_nn)
    print *,"a_T",a_T, (a2 - a1)/(2*dT)
    print *,"a_TT",a_TT, (a2_T - a1_T)/(2*dT)
    print *,"a_TV",a_TV, (a2_V - a1_V)/(2*dT)
    print *,"a_Tn",a_Tn, (a2_n - a1_n)/(2*dT)

    ! Volume differentials
    dV = V*rel_eps
    call fun(T,V+dV,n,a2,a2_T,a2_V,a2_n,d_TT,d_TV,d_VV,d_Tn,d_Vn,d_nn)
    call fun(T,V-dV,n,a1,a1_T,a1_V,a1_n,d_TT,d_TV,d_VV,d_Tn,d_Vn,d_nn)
    print *,"a_V",a_V, (a2 - a1)/(2*dV)
    print *,"a_VV",a_VV, (a2_V - a1_V)/(2*dV)
    print *,"a_TV",a_TV, (a2_T - a1_T)/(2*dV)
    print *,"a_Vn",a_Vn, (a2_n - a1_n)/(2*dV)

    ! Mol number differentials
    do i=1,nc
      n1 = n
      dn = n(i)*rel_eps
      n1(i) = n(i) + dn
      call fun(T,V,n1,a2,a2_T,a2_V,a2_n,d_TT,d_TV,d_VV,d_Tn,d_Vn,d_nn)
      n1(i) = n(i) - dn
      call fun(T,V,n1,a1,a1_T,a1_V,a1_n,d_TT,d_TV,d_VV,d_Tn,d_Vn,d_nn)
      print *,"i=",i
      print *,"a_n",a_n(i), (a2 - a1)/(2*dn)
      print *,"a_Vn",a_Vn(i), (a2_V - a1_V)/(2*dn)
      print *,"a_Tn",a_Tn(i), (a2_T - a1_T)/(2*dn)
      print *,"a_nn",a_nn(:,i), (a2_n - a1_n)/(2*dn)
    enddo
  end subroutine test_tvn_method

end module utilities
