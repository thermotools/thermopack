module hyperdual_utility
  use hyperdual_mod
  use thermopack_var, only: base_eos_param
  implicit none
  private

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
  public :: hyperdual_fres_wrapper, hyperdual_fres_wrapper_third_order
  ! Testing only:
  public :: test_hyperdual_numbers, test_fun, test_hyperdual_numbers_third_order

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
      T_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%f2 = 0
      T_hd%order = 0
      f_TT = f_hd%f12
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
    endif
    if (present(f_VV)) then
      V_hd%f1 = 1
      V_hd%f2 = 1
      V_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      V_hd%f2 = 0
      V_hd%order = 0
      f_VV = f_hd%f12
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_TV)) then
      V_hd%f1 = 1
      T_hd%f2 = 1
      T_hd%order = 2
      V_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      T_hd%f2 = 0
      T_hd%order = 0
      V_hd%order = 0
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
      T_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_Tn(i) = f_hd%f12
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
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
      T_hd%order = 0
    endif
    if (present(f_Vn)) then
      V_hd%f1 = 1
      V_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_Vn(i) = f_hd%f12
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f2
        endif
      enddo
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
      V_hd%f1 = 0
      V_hd%order = 0
      f_n_calculated = .true.
    endif
    if (present(f_nn)) then
      do i=1,nc
        n_hd(i)%f1 = 1
        n_hd(i)%order = 2
        do j=i,nc
          n_hd(j)%f2 = 1
          n_hd(j)%order = 2
          f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
          f_nn(i,j) = f_hd%f12
          f_nn(j,i) = f_nn(i,j)
          n_hd(j)%f2 = 0
          if (i /= j) n_hd(j)%order = 0
        enddo
        n_hd(i)%f1 = 0
        n_hd(i)%order = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f1
        endif
      enddo
      f_n_calculated = .true.
    endif
    if ( present(f_T) .and. .not. f_T_calculated .and. &
         present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      V_hd%order = 2
      T_hd%f2 = 1
      T_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      T_hd%f2 = 0
      V_hd%order = 0
      T_hd%order = 0
      f_T = f_hd%f2
      f_T_calculated = .true.
      f_V = f_hd%f1
      f_V_calculated = .true.
    else if (present(f_n) .and. .not. f_n_calculated .and. &
         present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      V_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        f_n(i) = f_hd%f2
      enddo
      V_hd%f1 = 0
      V_hd%order = 0
      f_V = f_hd%f1
      f_V_calculated = .true.
      f_n_calculated = .true.
    else if (present(f_n) .and. .not. f_n_calculated .and. &
         present(f_T) .and. .not. f_T_calculated) then
      T_hd%f1 = 1
      T_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        f_n(i) = f_hd%f2
      enddo
      T_hd%f1 = 0
      T_hd%order = 0
      f_T = f_hd%f1
      f_T_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_T) .and. .not. f_T_calculated) then
      T_hd%f1 = 1
      T_hd%order = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%order = 0
      f_T = f_hd%f1
      f_T_calculated = .true.
    endif
    if (present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      V_hd%order = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      V_hd%order = 0
      f_V = f_hd%f1
      f_V_calculated = .true.
    endif
    if (present(f_n) .and. .not. f_n_calculated) then
      do i=1,nc
        n_hd(i)%f1 = 1
        n_hd(i)%order = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f1 = 0
        n_hd(i)%order = 0
        f_n(i) = f_hd%f1
      enddo
      f_n_calculated = .true.
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

  subroutine hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T,V,n,f,f_T,f_V,f_n,&
       f_TT,f_VV,f_TV,f_Tn,f_Vn,f_nn,f_TTT,f_VVV,f_TTV,f_TVV,f_TTn,f_TVn,f_VVn,f_Tnn,f_Vnn,f_nnn)
    use hyperdual_mod
    implicit none
    procedure(hyperdual_fres) :: fun
    class(base_eos_param), intent(inout) :: p_eos
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc)
    real, intent(out) :: f
    real, optional, intent(inout) :: f_T,f_V,f_TT,f_VV,f_TV,f_TTT,f_VVV,f_TTV,f_TVV
    real, optional, intent(inout) :: f_n(nc),f_Tn(nc),f_Vn(nc),f_nn(nc,nc)
    real, optional, intent(inout) :: f_TTn(nc),f_TVn(nc),f_VVn(nc)
    real, optional, intent(inout) :: f_Tnn(nc,nc),f_Vnn(nc,nc),f_nnn(nc,nc,nc)
    ! Locals
    type(hyperdual) :: T_hd
    type(hyperdual) :: n_hd(nc)
    type(hyperdual) :: V_hd
    type(hyperdual) :: f_hd
    logical :: f_T_calculated, f_V_calculated, f_n_calculated
    logical :: f_TT_calculated, f_VV_calculated, f_TV_calculated
    logical :: f_Tn_calculated, f_Vn_calculated, f_nn_calculated
    integer :: i, j, k
    T_hd%f0 = T
    V_hd%f0 = V
    n_hd(:)%f0 = n
    f_T_calculated = .false.
    f_V_calculated = .false.
    f_n_calculated = .false.
    f_TT_calculated = .false.
    f_VV_calculated = .false.
    f_Tv_calculated = .false.
    f_Tn_calculated = .false.
    f_Vn_calculated = .false.
    f_nn_calculated = .false.

    if (present(f_TTT)) then
      T_hd%f1 = 1
      T_hd%f2 = 1
      T_hd%f3 = 1
      T_hd%order = 3
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%f2 = 0
      T_hd%f3 = 0
      T_hd%order = 0
      f_TTT = f_hd%f123
      if (present(f_TT)) then
        f_TT = f_hd%f12
        f_TT_calculated = .true.
      endif
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
    endif
    if (present(f_VVV)) then
      V_hd%f1 = 1
      V_hd%f2 = 1
      V_hd%f3 = 1
      V_hd%order = 3
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      V_hd%f2 = 0
      V_hd%f3 = 0
      V_hd%order = 0
      f_VVV = f_hd%f123
      if (present(f_VV)) then
        f_VV = f_hd%f12
        f_VV_calculated = .true.
      endif
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_TTV)) then
      T_hd%f1 = 1
      T_hd%f2 = 1
      V_hd%f3 = 1
      T_hd%order = 3
      V_hd%order = 3
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%f2 = 0
      V_hd%f3 = 0
      T_hd%order = 0
      V_hd%order = 0
      f_TTV = f_hd%f123
      if (present(f_TT)) then
        f_TT = f_hd%f12
        f_TT_calculated = .true.
      endif
      if (present(f_TV)) then
        f_TV = f_hd%f13
        f_TV_calculated = .true.
      endif
      if (present(f_T)) then
        f_T = f_hd%f2
        f_T_calculated = .true.
      endif
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_TVV)) then
      T_hd%f1 = 1
      V_hd%f2 = 1
      V_hd%f3 = 1
      T_hd%order = 3
      V_hd%order = 3
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      V_hd%f2 = 0
      V_hd%f3 = 0
      T_hd%order = 0
      V_hd%order = 0
      f_TVV = f_hd%f123
      if (present(f_VV)) then
        f_VV = f_hd%f23
        f_VV_calculated = .true.
      endif
      if (present(f_TV)) then
        f_TV = f_hd%f13
        f_TV_calculated = .true.
      endif
      if (present(f_T)) then
        f_T = f_hd%f2
        f_T_calculated = .true.
      endif
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_TTn)) then
      T_hd%f1 = 1
      T_hd%f2 = 1
      T_hd%order = 3
      do i=1,nc
        n_hd(i)%f3 = 1
        n_hd(i)%order = 3
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_TTn(i) = f_hd%f123
        n_hd(i)%f3 = 0
        n_hd(i)%order = 0
        if (present(f_Tn)) then
          f_Tn(i) = f_hd%f13
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f3
        endif
      enddo
      T_hd%f1 = 0
      T_hd%f2 = 0
      T_hd%order = 0
      if (present(f_TT)) then
        f_TT = f_hd%f12
        f_TT_calculated = .true.
      endif
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
      f_Tn_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_Tnn)) then
      T_hd%f1 = 1
      T_hd%order = 3
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 3
        do j=i,nc
          n_hd(j)%f3 = 1
          n_hd(j)%order = 3
          f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
          f_Tnn(i,j) = f_hd%f123
          f_Tnn(j,i) = f_Tnn(i,j)
          n_hd(j)%f3 = 0
          if (i /= j) n_hd(j)%order = 0
          if (present(f_nn)) then
            f_nn(i,j) = f_hd%f23
            f_nn(j,i) = f_nn(i,j)
          endif
        enddo
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        if (present(f_Tn)) then
          f_Tn(i) = f_hd%f12
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f1
        endif
      enddo
      T_hd%f1 = 0
      T_hd%order = 0
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
      f_Tn_calculated = .true.
      f_nn_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_Vnn)) then
      V_hd%f1 = 1
      V_hd%order = 3
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 3
        do j=i,nc
          n_hd(j)%f3 = 1
          n_hd(j)%order = 3
          f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
          f_Vnn(i,j) = f_hd%f123
          f_Vnn(j,i) = f_Vnn(i,j)
          n_hd(j)%f3 = 0
          if (i /= j) n_hd(j)%order = 0
          if (present(f_nn)) then
            f_nn(i,j) = f_hd%f23
            f_nn(j,i) = f_nn(i,j)
          endif
        enddo
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        if (present(f_Vn)) then
          f_Vn(i) = f_hd%f12
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f1
        endif
      enddo
      V_hd%f1 = 0
      V_hd%order = 0
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
      f_Vn_calculated = .true.
      f_nn_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_TVn)) then
      T_hd%f1 = 1
      V_hd%f2 = 1
      T_hd%order = 3
      V_hd%order = 3
      do i=1,nc
        n_hd(i)%f3 = 1
        n_hd(i)%order = 3
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_TVn(i) = f_hd%f123
        n_hd(i)%f3 = 0
        n_hd(i)%order = 0
        if (present(f_Tn)) then
          f_Tn(i) = f_hd%f13
        endif
        if (present(f_Vn)) then
          f_Vn(i) = f_hd%f23
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f3
        endif
      enddo
      T_hd%f1 = 0
      V_hd%f2 = 0
      T_hd%order = 0
      V_hd%order = 0
      if (present(f_TV)) then
        f_TV = f_hd%f12
        f_TV_calculated = .true.
      endif
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
      if (present(f_V)) then
        f_V = f_hd%f12
        f_V_calculated = .true.
      endif
      f_Tn_calculated = .true.
      f_Vn_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_VVn)) then
      V_hd%f1 = 1
      V_hd%f2 = 1
      V_hd%order = 3
      do i=1,nc
        n_hd(i)%f3 = 1
        n_hd(i)%order = 3
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_VVn(i) = f_hd%f123
        n_hd(i)%f3 = 0
        n_hd(i)%order = 0
        if (present(f_Vn)) then
          f_Vn(i) = f_hd%f13
        endif
        if (present(f_n)) then
          f_n(i) = f_hd%f3
        endif
      enddo
      V_hd%f1 = 0
      V_hd%f2 = 0
      V_hd%order = 0
      if (present(f_VV)) then
        f_VV = f_hd%f12
        f_VV_calculated = .true.
      endif
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
      f_Vn_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_nnn)) then
      do i=1,nc
        n_hd(i)%f1 = 1
        n_hd(i)%order = 3
        do j=i,nc
          n_hd(j)%f2 = 1
          n_hd(j)%order = 3
          do k=j,nc
            n_hd(k)%f3 = 1
            n_hd(k)%order = 3
            f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
            f_nnn(i,j,k) = f_hd%f123
            f_nnn(j,i,k) = f_nnn(i,j,k)
            f_nnn(i,k,j) = f_nnn(i,j,k)
            f_nnn(j,k,i) = f_nnn(i,j,k)
            f_nnn(k,j,i) = f_nnn(i,j,k)
            f_nnn(k,i,j) = f_nnn(i,j,k)
            n_hd(k)%f3 = 0
            if (i /= k .and. j /= k) n_hd(k)%order = 0
          enddo
          n_hd(j)%f2 = 0
          if (i /= j) n_hd(j)%order = 0
          if (present(f_nn)) then
            f_nn(i,j) = f_hd%f12
            f_nn(j,i) = f_nn(i,j)
          endif
        enddo
        n_hd(i)%f1 = 0
        n_hd(i)%order = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f1
        endif
      enddo
      f_nn_calculated = .true.
      f_n_calculated = .true.
    endif

    if (present(f_TT) .and. .not. f_TT_calculated) then
      T_hd%f1 = 1
      T_hd%f2 = 1
      T_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%f2 = 0
      T_hd%order = 0
      f_TT = f_hd%f12
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
    endif
    if (present(f_VV) .and. .not. f_VV_calculated) then
      V_hd%f1 = 1
      V_hd%f2 = 1
      V_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      V_hd%f2 = 0
      V_hd%order = 0
      f_VV = f_hd%f12
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
    endif
    if (present(f_TV) .and. .not. f_TV_calculated) then
      V_hd%f1 = 1
      T_hd%f2 = 1
      T_hd%order = 2
      V_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      T_hd%f2 = 0
      T_hd%order = 0
      V_hd%order = 0
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
    if (present(f_Tn) .and. .not. f_Tn_calculated) then
      T_hd%f1 = 1
      T_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_Tn(i) = f_hd%f12
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f2
        endif
      enddo
      T_hd%f1 = 0
      T_hd%order = 0
      if (present(f_T)) then
        f_T = f_hd%f1
        f_T_calculated = .true.
      endif
      f_n_calculated = .true.
    endif
    if (present(f_Vn) .and. .not. f_Vn_calculated) then
      V_hd%f1 = 1
      V_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        f_Vn(i) = f_hd%f12
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f2
        endif
      enddo
      V_hd%f1 = 0
      V_hd%order = 0
      if (present(f_V)) then
        f_V = f_hd%f1
        f_V_calculated = .true.
      endif
      f_n_calculated = .true.
    endif
    if (present(f_nn) .and. .not. f_nn_calculated) then
      do i=1,nc
        n_hd(i)%f1 = 1
        n_hd(i)%order = 2
        do j=i,nc
          n_hd(j)%f2 = 1
          n_hd(j)%order = 2
          f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
          f_nn(i,j) = f_hd%f12
          f_nn(j,i) = f_nn(i,j)
          n_hd(j)%f2 = 0
          if (i /= j) n_hd(j)%order = 0
        enddo
        n_hd(i)%f1 = 0
        n_hd(i)%order = 0
        if (present(f_n)) then
          f_n(i) = f_hd%f1
          f_n_calculated = .true.
        endif
      enddo
    endif
    if ( present(f_T) .and. .not. f_T_calculated .and. &
         present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      V_hd%order = 2
      T_hd%f2 = 1
      T_hd%order = 2
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      T_hd%f2 = 0
      V_hd%order = 0
      T_hd%order = 0
      f_T = f_hd%f2
      f_T_calculated = .true.
      f_V = f_hd%f1
      f_V_calculated = .true.
    else if (present(f_n) .and. .not. f_n_calculated .and. &
         present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      V_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        f_n(i) = f_hd%f2
      enddo
      V_hd%f1 = 0
      V_hd%order = 0
      f_V = f_hd%f1
      f_V_calculated = .true.
      f_n_calculated = .true.
    else if (present(f_n) .and. .not. f_n_calculated .and. &
         present(f_T) .and. .not. f_T_calculated) then
      T_hd%f1 = 1
      T_hd%order = 2
      do i=1,nc
        n_hd(i)%f2 = 1
        n_hd(i)%order = 2
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f2 = 0
        n_hd(i)%order = 0
        f_n(i) = f_hd%f2
      enddo
      T_hd%f1 = 0
      T_hd%order = 0
      f_T = f_hd%f1
      f_T_calculated = .true.
      f_n_calculated = .true.
    endif
    if (present(f_T) .and. .not. f_T_calculated) then
      T_hd%f1 = 1
      T_hd%order = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      T_hd%f1 = 0
      T_hd%order = 0
      f_T = f_hd%f1
      f_T_calculated = .true.
    endif
    if (present(f_V) .and. .not. f_V_calculated) then
      V_hd%f1 = 1
      V_hd%order = 1
      f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
      V_hd%f1 = 0
      V_hd%order = 0
      f_V = f_hd%f1
      f_V_calculated = .true.
    endif
    if (present(f_n) .and. .not. f_n_calculated) then
      do i=1,nc
        n_hd(i)%f1 = 1
        n_hd(i)%order = 1
        f_hd = fun(p_eos,nc,T_hd,V_hd,n_hd)
        n_hd(i)%f1 = 0
        n_hd(i)%order = 0
        f_n(i) = f_hd%f1
      enddo
      f_n_calculated = .true.
    endif

    if (.not. (present(f_TTT) .or. &
         present(f_VVV) .or. &
         present(f_TTV) .or. &
         present(f_TVV) .or. &
         present(f_TTn) .or. &
         present(f_TVn) .or. &
         present(f_VVn) .or. &
         present(f_Tnn) .or. &
         present(f_Vnn) .or. &
         present(f_nnn) .or. &
         present(f_TT) .or. &
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

  end subroutine hyperdual_fres_wrapper_third_order

  subroutine test_hyperdual_numbers(fun,p_eos,nc,T_r,V_r,n_r)
    procedure(hyperdual_fres) :: fun
    integer, intent(in) :: nc
    real, intent(in) :: T_r, V_r, n_r(nc)
    class(base_eos_param), intent(inout) :: p_eos
    !
    type(hyperdual) :: T, V, n(nc), ff
    real :: f_r, n1_r(nc)
    real :: f_T,f_V,f_TT,f_VV,f_TV
    real :: f_n(nc),f_Tn(nc),f_Vn(nc),f_nn(nc,nc)
    real :: eps = 1.0e-5, dT, dV, dn
    real :: f1_r, f1_T, f1_V, f1_n(nc)
    real :: f2_r, f2_T, f2_V, f2_n(nc)
    integer :: i
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r,n_r,f_r,f_T=f_T,f_V=f_V,f_n=f_n,&
         f_TT=f_TT,f_VV=f_VV,f_TV=f_TV,f_Tn=f_Tn,f_Vn=f_Vn,f_nn=f_nn)
    dT = T_r*eps
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r-dT,V_r,n_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r+dT,V_r,n_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_r",f_r
    print *,"f_T",f_T, (f2_r - f1_r)/(2*dT)
    print *,"f_TT",f_TT, (f2_T - f1_T)/(2*dT)
    print *,"f_TV",f_TV, (f2_V - f1_V)/(2*dT)
    print *,"f_Tn",f_Tn, (f2_n - f1_n)/(2*dT)

    dV = V_r*eps
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r-dV,n_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r+dV,n_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_V",f_V, (f2_r - f1_r)/(2*dV)
    print *,"f_VV",f_VV, (f2_V - f1_V)/(2*dV)
    print *,"f_TV",f_TV, (f2_T - f1_T)/(2*dV)
    print *,"f_Vn",f_Vn, (f2_n - f1_n)/(2*dV)

    dn = eps
    i = 1
    n1_r = n_r
    n1_r(i) = n1_r(i) - dn
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r,n1_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    n1_r = n_r
    n1_r(i) = n1_r(i) + dn
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r,n1_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_n",f_n(i), (f2_r - f1_r)/(2*dn)
    print *,"f_Tn",f_Tn(i), (f2_T - f1_T)/(2*dn)
    print *,"f_Vn",f_Vn(i), (f2_V - f1_V)/(2*dn)
    print *,"f_nn",f_nn(:,i), (f2_n - f1_n)/(2*dn)

    i = 2
    n1_r = n_r
    n1_r(i) = n1_r(i) - dn
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r,n1_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n)
    n1_r = n_r
    n1_r(i) = n1_r(i) + dn
    call hyperdual_fres_wrapper(fun,p_eos,nc,T_r,V_r,n1_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n)
    print *,"f_n",f_n(i), (f2_r - f1_r)/(2*dn)
    print *,"f_Tn",f_Tn(i), (f2_T - f1_T)/(2*dn)
    print *,"f_Vn",f_Vn(i), (f2_V - f1_V)/(2*dn)
    print *,"f_nn",f_nn(:,i), (f2_n - f1_n)/(2*dn)
  end subroutine test_hyperdual_numbers

  subroutine test_hyperdual_numbers_third_order(fun,p_eos,nc,T_r,V_r,n_r)
    procedure(hyperdual_fres) :: fun
    integer, intent(in) :: nc
    real, intent(in) :: T_r, V_r, n_r(nc)
    class(base_eos_param), intent(inout) :: p_eos
    !
    type(hyperdual) :: T, V, n(nc), ff
    real :: f_r, n1_r(nc)
    real :: f_T,f_V,f_TT,f_VV,f_TV,f_TTT,f_TTV,f_TVV,F_VVV
    real :: f_n(nc),f_Tn(nc),f_Vn(nc),f_nn(nc,nc)
    real :: f_TTn(nc),f_VVn(nc),f_TVn(nc),f_Tnn(nc,nc),f_Vnn(nc,nc)
    real :: f_nnn(nc,nc,nc)
    real :: eps = 1.0e-5, dT, dV, dn
    real :: f1_r, f1_T, f1_V, f1_n(nc), f1_TT, f1_VV, f1_TV, f1_nn(nc,nc)
    real :: f2_r, f2_T, f2_V, f2_n(nc), f2_TT, f2_VV, f2_TV, f2_nn(nc,nc)
    real :: f1_Tn(nc), f1_Vn(nc)
    real :: f2_Tn(nc), f2_Vn(nc)
    integer :: i, j
    call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r,V_r,n_r,f_r,f_T=f_T,f_V=f_V,f_n=f_n,&
         f_TT=f_TT,f_VV=f_VV,f_TV=f_TV,f_Tn=f_Tn,f_Vn=f_Vn,f_nn=f_nn,f_TTT=f_TTT,&
         f_TTV=f_TTV,f_TVV=f_TVV,f_VVV=f_VVV,f_TTn=f_TTn,f_TVn=f_TVn,f_VVn=f_VVn,f_Tnn=f_Tnn,f_Vnn=f_Vnn,f_nnn=f_nnn)
    dT = T_r*eps
    call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r-dT,V_r,n_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n, &
         f_TT=f1_TT,f_VV=f1_VV,f_TV=f1_TV,f_Tn=f1_Tn,f_Vn=f1_Vn,f_nn=f1_nn)
    call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r+dT,V_r,n_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n, &
         f_TT=f2_TT,f_VV=f2_VV,f_TV=f2_TV,f_Tn=f2_Tn,f_Vn=f2_Vn,f_nn=f2_nn)
    print *,"f_r",f_r
    print *,"f_T",f_T, (f2_r - f1_r)/(2*dT)
    print *,"f_TT",f_TT, (f2_T - f1_T)/(2*dT)
    print *,"f_TV",f_TV, (f2_V - f1_V)/(2*dT)
    print *,"f_Tn",f_Tn, (f2_n - f1_n)/(2*dT)
    print *,"f_TTT",f_TTT, (f2_TT - f1_TT)/(2*dT)
    print *,"f_TTV",f_TTV, (f2_TV - f1_TV)/(2*dT)
    print *,"f_TVV",f_TVV, (f2_VV - f1_VV)/(2*dT)
    print *,"f_TVn",f_TVn, (f2_Vn - f1_Vn)/(2*dT)
    print *,"f_TTn",f_TTn, (f2_Tn - f1_Tn)/(2*dT)
    do i=1,nc
      print *,"f_Tnn",f_Tnn(i,:), (f2_nn(i,:) - f1_nn(i,:))/(2*dT)
    enddo

    dV = V_r*eps
    call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r,V_r-dV,n_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n, &
         f_TT=f1_TT,f_VV=f1_VV,f_TV=f1_TV,f_Tn=f1_Tn,f_Vn=f1_Vn,f_nn=f1_nn)
    call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r,V_r+dV,n_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n, &
         f_TT=f2_TT,f_VV=f2_VV,f_TV=f2_TV,f_Tn=f2_Tn,f_Vn=f2_Vn,f_nn=f2_nn)
    print *,"f_V",f_V, (f2_r - f1_r)/(2*dV)
    print *,"f_VV",f_VV, (f2_V - f1_V)/(2*dV)
    print *,"f_TV",f_TV, (f2_T - f1_T)/(2*dV)
    print *,"f_Vn",f_Vn, (f2_n - f1_n)/(2*dV)
    print *,"f_VVV",f_VVV, (f2_VV - f1_VV)/(2*dV)
    print *,"f_TTV",f_TTV, (f2_TT - f1_TT)/(2*dV)
    print *,"f_TVV",f_TVV, (f2_TV - f1_TV)/(2*dV)
    print *,"f_VVn",f_VVn, (f2_Vn - f1_Vn)/(2*dV)
    print *,"f_TVn",f_TVn, (f2_Tn - f1_Tn)/(2*dV)
    do i=1,nc
      print *,"f_Vnn",f_Vnn(i,:), (f2_nn(i,:) - f1_nn(i,:))/(2*dV)
    enddo

    dn = eps
    do i=1,nc
      print *,"i=",i
      n1_r = n_r
      n1_r(i) = n1_r(i) - dn
      call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r,V_r,n1_r,f1_r, f_T=f1_T, f_V=f1_V, f_n=f1_n, &
           f_TT=f1_TT,f_VV=f1_VV,f_TV=f1_TV,f_Tn=f1_Tn,f_Vn=f1_Vn,f_nn=f1_nn)
      n1_r = n_r
      n1_r(i) = n1_r(i) + dn
      call hyperdual_fres_wrapper_third_order(fun,p_eos,nc,T_r,V_r,n1_r,f2_r, f_T=f2_T, f_V=f2_V, f_n=f2_n, &
           f_TT=f2_TT,f_VV=f2_VV,f_TV=f2_TV,f_Tn=f2_Tn,f_Vn=f2_Vn,f_nn=f2_nn)
      print *,"f_n",f_n(i), (f2_r - f1_r)/(2*dn)
      print *,"f_Tn",f_Tn(i), (f2_T - f1_T)/(2*dn)
      print *,"f_Vn",f_Vn(i), (f2_V - f1_V)/(2*dn)
      print *,"f_nn",f_nn(:,i), (f2_n - f1_n)/(2*dn)
      print *,"f_TTn",f_TTn(i), (f2_TT - f1_TT)/(2*dn)
      print *,"f_TVn",f_TVn(i), (f2_TV - f1_TV)/(2*dn)
      print *,"f_VVn",f_VVn(i), (f2_VV - f1_VV)/(2*dn)
      print *,"f_Tnn",f_Tnn(:,i), (f2_Tn - f1_Tn)/(2*dn)
      print *,"f_Vnn",f_Vnn(:,i), (f2_Vn - f1_Vn)/(2*dn)
      do j=1,nc
        print *,"f_nnn",f_nnn(j,:,i), (f2_nn(j,:) - f1_nn(j,:))/(2*dn)
      enddo
    enddo
  end subroutine test_hyperdual_numbers_third_order

  function test_fun(p_eos,nc,T,V,n) result(ff)
    use hyperdual_mod
    implicit none
    class(base_eos_param), intent(inout) :: p_eos
    integer, intent(in) :: nc
    type(hyperdual), intent(in) :: T, V, n(nc)
    type(hyperdual) :: ff
    ff = log(V) * T**3 * sum(n**2)*sum(n)*sum(n) * exp(T*V)
  end function test_fun

end module hyperdual_utility
