module hyperdual_utility
  use hyperdual_mod
  use thermopack_var, only: base_eos_param
  implicit none

  abstract interface
    function hyperdual_fres(p_eos,nc,T,V,n) result(f)
      use hyperdual_mod
      use thermopack_var, only: base_eos_param
      implicit none
      class(base_eos_param), intent(inout) :: p_eos
      integer, intent(in) :: nc
      type(hyperdual), intent(in) :: T, V, n(nc)
      type(hyperdual) :: f
    end function hyperdual_fres
  end interface

  class(base_eos_param), pointer :: p_eos_dummy => NULL()

  public :: hyperdual_fres
  public :: hyperdual_fres_wrapper
  ! Testing only:
  public :: test_hyperdual_numbers

contains

  subroutine hyperdual_fres_wrapper(fun,p_eos,nc,T,V,n,f,f_T,f_V,f_n,&
       f_TT,f_VV,f_TV,f_Tn,f_Vn,f_nn)
    use hyperdual_mod
    implicit none
    procedure(hyperdual_fres) :: fun
    class(base_eos_param), intent(inout) :: p_eos
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc)
    real, intent(out) :: f
    real, optional, intent(inout) :: f_T,f_V,f_TT,f_VV,f_TV
    real, optional, intent(inout) :: f_n(nc),f_Tn(nc),f_Vn(nc),f_nn(nc,nc)
    ! Locals
    type(hyperdual) :: T_hd
    type(hyperdual) :: n_hd(nc)
    type(hyperdual) :: V_hd
    type(hyperdual) :: f_hd
    logical :: f_T_calculated, f_V_calculated, f_n_calculated
    integer :: i, j
    T_hd%f0 = T
    V_hd%f0 = V
    n_hd(:)%f0 = n
    f_T_calculated = .false.
    f_V_calculated = .false.
    f_n_calculated = .false.

    if (present(f_TT)) then
      T_hd%f1 = 1
      T_hd%f2 = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%f2 = 0
      f_TT = f_hd%f12
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
    endif
    if (present(f_VV)) then
      V_hd%f1 = 1
      V_hd%f2 = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      V_hd%f2 = 0
      f_VV = f_hd%f12
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_TV)) then
      V_hd%f1 = 1
      T_hd%f2 = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      T_hd%f2 = 0
      f_TV = f_hd%f12
      if (present(f_T)) then
        f_T = f_hd%f2
        f_T_calculated = .true.
      endif
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_Tn)) then
      T_hd%f1 = 1
      do i=1,nc
        n_hd(i)%f2 = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_Tn(i) = f_hd%f12
        n_hd(i)%f2 = 0
        if (present(f_T)) then
          f_T = f_hd%f1
          f_T_calculated = .true.
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f2
          f_n_calculated = .true.
        endif
      enddo
      T_hd%f1 = 0
    endif
    if (present(f_Vn)) then
      V_hd%f1 = 1
      do i=1,nc
        n_hd(i)%f2 = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_Vn(i) = f_hd%f12
        n_hd(i)%f2 = 0
        if (present(f_V)) then
          f_V = f_hd%f1
          f_V_calculated = .true.
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f2
          f_n_calculated = .true.
        endif
      enddo
      V_hd%f1 = 0
    endif
    if (present(f_nn)) then
      do i=1,nc
        n_hd(i)%f1 = 1
        do j=i,nc
          n_hd(j)%f2 = 1
          f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
          f_nn(i,j) = f_hd%f12
          f_nn(j,i) = f_nn(i,j)
          n_hd(j)%f2 = 0
        enddo
        n_hd(i)%f1 = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f1
          f_n_calculated = .true.
        endif
      enddo
    endif
    if ( present(f_T) .and. .not. f_T_calculated .and. &
         present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      T_hd%f2 = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      T_hd%f2 = 0
      f_T = f_hd%f2
      f_T_calculated = .true.
      f_V = f_hd%f1
      f_V_calculated = .true.
    else if (present(f_n) .and. .not. f_n_calculated .and. &
         present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      do i=1,nc
        n_hd(i)%f2 = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f2 = 0
        f_V = f_hd%f1
        f_V_calculated = .true.
        f_n(i) = f_hd%f2
        f_n_calculated = .true.
      enddo
      V_hd%f1 = 0
    else if (present(f_n) .and. .not. f_n_calculated .and. &
         present(f_T) .and. .not. f_T_calculated) then
      T_hd%f1 = 1
      do i=1,nc
        n_hd(i)%f2 = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f2 = 0
        f_T = f_hd%f1
        f_T_calculated = .true.
        f_n(i) = f_hd%f2
        f_n_calculated = .true.
      enddo
      T_hd%f1 = 0
    endif
    if (present(f_T) .and. .not. f_T_calculated) then
      T_hd%f1 = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      f_T = f_hd%f1
      f_T_calculated = .true.
    endif
    if (present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      f_V = f_hd%f1
      f_V_calculated = .true.
    endif
    if (present(f_n) .and. .not. f_n_calculated) then
      do i=1,nc
        n_hd(i)%f1 = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f1 = 0
        f_n(i) = f_hd%f1
        f_n_calculated = .true.
      enddo
    endif

    if (.not. (present(f_TT) .or. &
         present(f_VV) .or. &
         present(f_TV) .or. &
         present(f_Tn) .or. &
         present(f_Vn) .or. &
         present(f_nn) .or. &
         present(f_T) .or. &
         present(f_V) .or. &
         present(f_n))) then
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
    endif
    f = f_hd%f0

  end subroutine hyperdual_fres_wrapper

  subroutine test_hyperdual_numbers(fun,p_eos,T_r,V_r,n_r)
    procedure(hyperdual_fres) :: fun
    real, intent(in) :: T_r, V_r, n_r(2)
    class(base_eos_param), intent(inout) :: p_eos
    !
    type(hyperdual) :: T, V, n(2), ff
    real :: f_r, n1_r(2)
    real :: f_T,f_V,f_TT,f_VV,f_TV
    real :: f_n(2),f_Tn(2),f_Vn(2),f_nn(2,2)
    real :: eps = 1.0e-5, dT, dV, dn
    real :: f1_r, f1_T, f1_V, f1_n(2)
    real :: f2_r, f2_T, f2_V, f2_n(2)
    integer :: i
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r,n_r,f_r,f_T=f_T,f_V=f_V,f_n=f_n,&
         f_TT=f_TT,f_VV=f_VV,f_TV=f_TV,f_Tn=f_Tn,f_Vn=f_Vn,f_nn=f_nn)
    dT = T_r*eps
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r-dT,V_r,n_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r+dT,V_r,n_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_r",f_r
    print *,"f_T",f_T, (f2_r - f1_r)/(2*dT)
    print *,"f_TT",f_TT, (f2_T - f1_T)/(2*dT)
    print *,"f_TV",f_TV, (f2_V - f1_V)/(2*dT)
    print *,"f_Tn",f_Tn, (f2_n - f1_n)/(2*dT)

    dV = V_r*eps
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r-dV,n_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r+dV,n_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_V",f_V, (f2_r - f1_r)/(2*dV)
    print *,"f_VV",f_VV, (f2_V - f1_V)/(2*dV)
    print *,"f_TV",f_TV, (f2_T - f1_T)/(2*dV)
    print *,"f_Vn",f_Vn, (f2_n - f1_n)/(2*dV)

    dn = eps
    i = 1
    n1_r = n_r
    n1_r(i) = n1_r(i) - dn
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r,n1_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    n1_r = n_r
    n1_r(i) = n1_r(i) + dn
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r,n1_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_n",f_n(i), (f2_r - f1_r)/(2*dn)
    print *,"f_Tn",f_Tn(i), (f2_T - f1_T)/(2*dn)
    print *,"f_Vn",f_Vn(i), (f2_V - f1_V)/(2*dn)
    print *,"f_nn",f_nn(:,i), (f2_n - f1_n)/(2*dn)

    i = 2
    n1_r = n_r
    n1_r(i) = n1_r(i) - dn
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r,n1_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    n1_r = n_r
    n1_r(i) = n1_r(i) + dn
    call hyperdual_fres_wrapper(fun,p_eos,2,T_r,V_r,n1_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_n",f_n(i), (f2_r - f1_r)/(2*dn)
    print *,"f_Tn",f_Tn(i), (f2_T - f1_T)/(2*dn)
    print *,"f_Vn",f_Vn(i), (f2_V - f1_V)/(2*dn)
    print *,"f_nn",f_nn(:,i), (f2_n - f1_n)/(2*dn)
  end subroutine test_hyperdual_numbers

  subroutine play_with_hyperdual_numbers()
    type(hyperdual) :: T, V, n(2), ff
    real :: T_r, V_r, n_r(2), f_r, n1_r(2)
    real :: f_T,f_V,f_TT,f_VV,f_TV
    real :: f_n(2),f_Tn(2),f_Vn(2),f_nn(2,2)
    real :: eps = 1.0e-5, dT, dV, dn
    real :: f1_r, f1_T, f1_V, f1_n(2)
    real :: f2_r, f2_T, f2_V, f2_n(2)
    integer :: i
    T%f0 = 3.0
    T%f1 = 1.0
    T%f2 = 0.0
    T%f12 = 0.0
    V%f0 = 2.0
    V%f1 = 0.0
    V%f2 = 1.0
    V%f12 = 0.0
    n(:)%f0 = (/1.5, 0.5/)
    n(:)%f1 = 0.0
    n(:)%f2 = 0.0
    n(:)%f12 = 0.0
    print *,"T",T
    print *,"V",V
    ff = test_fun(p_eos_dummy,2,T,V, n)
    print *,"ff",ff%f0, ff%f1, ff%f2, ff%f12
    V%f2 = 0.0
    n(1)%f1 = 1.0
    ff = test_fun(p_eos_dummy,2,T,V, n)
    print *,"ff",ff%f0, ff%f1, ff%f2, ff%f12
    T%f2 = 1.0
    n(1)%f1 = 0.0
    ff = test_fun(p_eos_dummy,2,T,V, n)
    print *,"ff",ff%f0, ff%f1, ff%f2, ff%f12
    T%f2 = 1.0
    n(1)%f1 = 0.0
    ff = test_fun(p_eos_dummy,2,T,V, n)
    print *,"ff",ff%f0, ff%f1, ff%f2, ff%f12
  end subroutine play_with_hyperdual_numbers

  function test_fun(p_eos,nc,T,V,n) result(ff)
    use hyperdual_mod
    implicit none
    class(base_eos_param), intent(inout) :: p_eos
    integer, intent(in) :: nc
    type(hyperdual), intent(in) :: T, V, n(nc)
    type(hyperdual) :: ff
    ff = log(V) * T**2 * (n(1)**2 + n(2)**2)*sum(n)
  end function test_fun

end module hyperdual_utility
