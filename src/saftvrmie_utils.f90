!---------------------------------------------------------------------
! Utility module for SAFT-VRQ Mie.
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Autumn 2018
!---------------------------------------------------------------------

module saftvrmie_utils
  use saftvrmie_containers, only: saftvrmie_zeta
  implicit none
  private
  save

  public :: calc_a_zeta_product
  public :: convert_zeta_x_to_TVn
  public :: convert_zeta_zeta_to_TVn
  public :: calc_a0_a_product
  public :: calc_a0_plus_a1

contains

  !> Calculate a*zeta and differentials
  !! Store result in a
  !!
  !! \author Morten Hammer, February 2018
  subroutine calc_a_zeta_product(nc,zeta,a,a_T,a_V,a_n,&
       a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_zeta), intent(in) :: zeta !< zeta and differentials
    ! Output
    real, intent(inout) ::  a
    real, optional, intent(inout) ::  a_T,a_V,a_TT,a_VV,a_TV,a_VVV,a_VVT,a_VTT
    real, optional, dimension(nc), intent(inout) :: a_n,a_Tn,a_Vn,a_VVn,a_VTn
    real, optional, dimension(nc,nc), intent(inout) :: a_nn,a_Vnn

    call calc_a0_a_product(nc,zeta%zx,a,&
         a0_T=zeta%zx_T,a0_V=zeta%zx_V,a0_n=zeta%zx_n,&
         a_T=a_T,a_V=a_V,a_n=a_n,&
         a0_TT=zeta%zx_TT,a0_VV=zeta%zx_VV,a0_TV=zeta%zx_TV,&
         a0_Tn=zeta%zx_Tn,a0_Vn=zeta%zx_Vn,a0_nn=zeta%zx_nn,&
         a_TT=a_TT,a_VV=a_VV,a_TV=a_TV,a_Tn=a_Tn,a_Vn=a_Vn,a_nn=a_nn,&
         a0_VVV=zeta%zx_VVV,a0_VVT=zeta%zx_VVT,a0_VTT=zeta%zx_VTT,&
         a0_VVn=zeta%zx_VVn,a0_Vnn=zeta%zx_Vnn,a0_VTn=zeta%zx_VTn,&
         a_VVV=a_VVV,a_VVT=a_VVT,a_VTT=a_VTT,a_VVn=a_VVn,a_Vnn=a_Vnn,a_VTn=a_VTn)
  end subroutine calc_a_zeta_product

  !> Calculate a0*a and differentials
  !! Store result in a
  !!
  !! \author Morten Hammer, February 2018
  subroutine calc_a0_a_product(nc,a0,a,&
       a0_T,a0_V,a0_n,&
       a_T,a_V,a_n,&
       a0_TT,a0_VV,a0_TV,a0_Tn,a0_Vn,a0_nn,&
       a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       a0_VVV,a0_VVT,a0_VTT,a0_VVn,a0_Vnn,a0_VTn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: a0
    real, optional, intent(in) :: a0_T,a0_V,a0_TT,a0_VV,a0_TV,a0_VVV,a0_VVT,a0_VTT
    real, optional, dimension(nc), intent(in) :: a0_n,a0_Tn,a0_Vn,a0_VVn,a0_VTn
    real, optional, dimension(nc,nc), intent(in) :: a0_nn,a0_Vnn
    ! Output
    real, intent(inout) ::  a
    real, optional, intent(inout) ::  a_T,a_V,a_TT,a_VV,a_TV,a_VVV,a_VVT,a_VTT
    real, optional, dimension(nc), intent(inout) :: a_n,a_Tn,a_Vn,a_VVn,a_VTn
    real, optional, dimension(nc,nc), intent(inout) :: a_nn,a_Vnn
    ! Locals
    integer :: k

    if (present(a_Vnn)) then
       do k=1,nc
          a_Vnn(:,k) = a*a0_Vnn(:,k) + a_Vn(k)*a0_n + a_n*a0_Vn(k) &
               + a_nn(:,k)*a0_V + a_V*a0_nn(:,k) + a_n(k)*a0_Vn &
               + a_Vn*a0_n(k) + a_Vnn(:,k)*a0
       enddo
    endif
    if (present(a_nn)) then
       do k=1,nc
          a_nn(:,k) = a*a0_nn(:,k) + a_n(k)*a0_n + a_n*a0_n(k) + a_nn(:,k)*a0
       enddo
    endif
    if (present(a_VTn)) then
       a_VTn = a*a0_VTn + a_V*a0_Tn + a_Tn*a0_V + a_Vn*a0_T &
            + a_T*a0_Vn + a_TV*a0_n + a_n*a0_TV + a_VTn*a0
    endif
    if (present(a_VVn)) then
       a_VVn = a*a0_VVn + a_VV*a0_n + 2.0*a_Vn*a0_V&
            + 2.0*a_V*a0_Vn + a_n*a0_VV + a_VVn*a0
    endif
    if (present(a_Vn)) then
       a_Vn = a*a0_Vn + a_V*a0_n + a_n*a0_V + a_Vn*a0
    endif
    if (present(a_Tn)) then
       a_Tn = a*a0_Tn + a_T*a0_n + a_n*a0_T + a_Tn*a0
    endif
    if (present(a_n)) then
       a_n = a*a0_n + a_n*a0
    endif
    if (present(a_VTT)) then
       a_VTT = a*a0_VTT + 2.0*a_TV*a0_T + 2.0*a_T*a0_TV + a_TT*a0_V &
            + a_V*a0_TT + a_VTT*a0
    endif
    if (present(a_VVT)) then
       a_VVT = a*a0_VVT + 2.0*a_TV*a0_V + 2.0*a_V*a0_TV + a_VV*a0_T &
            + a_T*a0_VV + a_VVT*a0
    endif
    if (present(a_TV)) then
       a_TV = a*a0_TV + a_T*a0_V + a_V*a0_T + a_TV*a0
    endif
    if (present(a_VVV)) then
       a_VVV = a*a0_VVV + 3.0*a_VV*a0_V + 3.0*a_V*a0_VV + a_VVV*a0
    endif
    if (present(a_VV)) then
       a_VV = a*a0_VV + 2.0*a_V*a0_V + a_VV*a0
    endif
    if (present(a_TT)) then
       a_TT = a*a0_TT + 2.0*a_T*a0_T + a_TT*a0
    endif
    if (present(a_T)) then
       a_T = a*a0_T + a_T*a0
    endif
    if (present(a_V)) then
       a_V = a*a0_V + a_V*a0
    endif
    a = a*a0
  end subroutine calc_a0_a_product

  !> Calculate a0 + a1 and differentials
  !! Store result in a1
  !!
  !! \author Morten Hammer, March 2018
  subroutine calc_a0_plus_a1(nc,a0,a1,&
       a0_T,a0_V,a0_n,&
       a1_T,a1_V,a1_n,&
       a0_TT,a0_VV,a0_TV,a0_Tn,a0_Vn,a0_nn,&
       a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a0_VVV,a0_VVT,a0_VTT,a0_VVn,a0_Vnn,a0_VTn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: a0
    real, optional, intent(in) :: a0_T,a0_V,a0_TT,a0_VV,a0_TV,a0_VVV,a0_VVT,a0_VTT
    real, optional, dimension(nc), intent(in) :: a0_n,a0_Tn,a0_Vn,a0_VVn,a0_VTn
    real, optional, dimension(nc,nc), intent(in) :: a0_nn,a0_Vnn
    ! Output
    real, intent(inout) ::  a1
    real, optional, intent(inout) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(inout) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(inout) :: a1_nn,a1_Vnn
    ! Locals
    integer :: k

    if (present(a1_Vnn)) then
       do k=1,nc
          a1_Vnn(:,k) = a0_Vnn(:,k) + a1_Vnn(:,k)
       enddo
    endif
    if (present(a1_nn)) then
       do k=1,nc
          a1_nn(:,k) = a0_nn(:,k) + a1_nn(:,k)
       enddo
    endif
    if (present(a1_VTn)) then
       a1_VTn = a0_VTn + a1_VTn
    endif
    if (present(a1_VVn)) then
       a1_VVn = a0_VVn + a1_VVn
    endif
    if (present(a1_Vn)) then
       a1_Vn = a0_Vn + a1_Vn
    endif
    if (present(a1_Tn)) then
       a1_Tn = a0_Tn + a1_Tn
    endif
    if (present(a1_n)) then
       a1_n = a0_n + a1_n
    endif
    if (present(a1_VTT)) then
       a1_VTT = a0_VTT + a1_VTT
    endif
    if (present(a1_VVT)) then
       a1_VVT = a0_VVT + a1_VVT
    endif
    if (present(a1_TV)) then
       a1_TV = a0_TV + a1_TV
    endif
    if (present(a1_VVV)) then
       a1_VVV = a0_VVV + a1_VVV
    endif
    if (present(a1_VV)) then
       a1_VV = a0_VV + a1_VV
    endif
    if (present(a1_TT)) then
       a1_TT = a0_TT + a1_TT
    endif
    if (present(a1_T)) then
       a1_T = a0_T + a1_T
    endif
    if (present(a1_V)) then
       a1_V = a0_V + a1_V
    endif
    a1 = a0 + a1
  end subroutine calc_a0_plus_a1

  !!
  !! \author Morten Hammer, February 2018
  subroutine convert_zeta_x_to_TVn(nc,x,x_T,x_TT,zb,&
       a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx,&
       a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: x, x_T, x_TT !< Reduced center-center hard sphere distance
    real, intent(in) :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx
    type(saftvrmie_zeta), intent(in) :: zb
    integer, optional, intent(in) :: difflevel
    ! Output
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    integer :: k,dl
    dl = 3
    if (present(difflevel)) then
       dl = difflevel
    endif
    if (present(a1_T)) then
       a1_T = a1_e*zb%zx_T + a1_x*x_T
    endif
    if (present(a1_TT) .and. dl > 1) then
       a1_TT = a1_e*zb%zx_TT + a1_x*x_TT &
            + a1_ee*zb%zx_T**2 + a1_xx*x_T**2 &
            + 2.0*a1_ex*x_T*zb%zx_T
    endif
    if (present(a1_V)) then
       a1_V = a1_e*zb%zx_V
    endif
    if (present(a1_VV) .and. dl > 1) then
       a1_VV = a1_e*zb%zx_VV + a1_ee*zb%zx_V**2
    endif
    if (present(a1_VVV) .and. dl > 2) then
       a1_VVV = a1_eee*zb%zx_V**3 &
            + 3.0*a1_ee*zb%zx_V*zb%zx_VV &
            + a1_e*zb%zx_VVV
    endif
    if (present(a1_TV) .and. dl > 1) then
       a1_TV = a1_ee*zb%zx_T*zb%zx_V &
            + a1_ex*x_T*zb%zx_V &
            + a1_e*zb%zx_TV
    endif
    if (present(a1_VVT) .and. dl > 2) then
       a1_VVT = a1_eee*zb%zx_V**2*zb%zx_T &
            + a1_ee*zb%zx_VV*zb%zx_T &
            + a1_ex*x_T*zb%zx_VV + a1_eex*x_T*zb%zx_V**2 &
            + a1_e*zb%zx_VVT &
            + 2.0*a1_ee*zb%zx_V*zb%zx_TV
    endif
    if (present(a1_VTT) .and. dl > 2) then
       a1_VTT = a1_ee*zb%zx_V*zb%zx_TT &
            + a1_ex*zb%zx_V*x_TT &
            + a1_eee*zb%zx_V*zb%zx_T**2 &
            + a1_exx*zb%zx_V*x_T**2 &
            + a1_e*zb%zx_VTT &
            + 2.0*a1_ee*zb%zx_T*zb%zx_TV &
            + 2.0*a1_eex*zb%zx_V*x_T*zb%zx_T &
            + 2.0*a1_ex*zb%zx_TV*x_T
    endif
    if (present(a1_n) .and. dl > 0) then
       a1_n = a1_e*zb%zx_n
    endif
    if (present(a1_nn) .and. dl > 1) then
       do k=1,nc
          a1_nn(:,k) = a1_ee*zb%zx_n(:)*zb%zx_n(k) &
               + a1_e*zb%zx_nn(:,k)
       enddo
    endif
    if (present(a1_Tn) .and. dl > 1) then
       a1_Tn = a1_ee*zb%zx_T*zb%zx_n &
            + a1_ex*x_T*zb%zx_n &
            + a1_e*zb%zx_Tn
    endif
    if (present(a1_Vn) .and. dl > 1) then
       a1_Vn = a1_ee*zb%zx_V*zb%zx_n &
            + a1_e*zb%zx_Vn
    endif
    if (present(a1_VVn) .and. dl > 2) then
       a1_VVn = a1_eee*zb%zx_V**2*zb%zx_n &
            + 2.0*a1_ee*zb%zx_V*zb%zx_Vn + a1_ee*zb%zx_VV*zb%zx_n &
            + a1_e*zb%zx_VVn
    endif
    if (present(a1_VTn) .and. dl > 2) then
       a1_VTn = a1_eee*zb%zx_V*zb%zx_T*zb%zx_n &
            + a1_ee*zb%zx_T*zb%zx_Vn &
            + a1_ee*zb%zx_TV*zb%zx_n &
            + a1_eex*x_T*zb%zx_V*zb%zx_n &
            + a1_ex*x_T*zb%zx_Vn &
            + a1_ee*zb%zx_V*zb%zx_Tn &
            + a1_e*zb%zx_VTn
    endif
    if (present(a1_Vnn) .and. dl > 2) then
       do k=1,nc
          a1_Vnn(:,k) = a1_eee*zb%zx_V*zb%zx_n(:)*&
               zb%zx_n(k) &
               + a1_ee*zb%zx_Vn(:)*zb%zx_n(k) &
               + a1_ee*zb%zx_n(:)*zb%zx_Vn(k) &
               + a1_ee*zb%zx_V*zb%zx_nn(:,k) &
               + a1_e*zb%zx_Vnn(:,k)
       enddo
    endif
  end subroutine convert_zeta_x_to_TVn

  !!
  !! \author Morten Hammer, February 2019
  subroutine convert_zeta_zeta_to_TVn(nc,x,ze,&
       a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_xxx,a1_eex,a1_exx,&
       a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx,a1_xxx
    type(saftvrmie_zeta), intent(in) :: ze,x
    integer, optional, intent(in) :: difflevel
    ! Output
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    integer :: k,dl
    dl = 3
    if (present(difflevel)) then
       dl = difflevel
    endif
    if (present(a1_T)) then
       a1_T = a1_e*ze%zx_T + a1_x*x%zx_T
    endif
    if (present(a1_TT) .and. dl > 1) then
       a1_TT = a1_e*ze%zx_TT + a1_x*x%zx_TT &
            + a1_ee*ze%zx_T**2 + a1_xx*x%zx_T**2 &
            + 2.0*a1_ex*x%zx_T*ze%zx_T
    endif
    if (present(a1_V)) then
       a1_V = a1_e*ze%zx_V + a1_x*x%zx_V
    endif
    if (present(a1_VV) .and. dl > 1) then
       a1_VV = a1_e*ze%zx_VV + a1_x*x%zx_VV &
            + a1_ee*ze%zx_V**2 + a1_xx*x%zx_V**2 &
            + 2.0*a1_ex*x%zx_V*ze%zx_V
    endif
    if (present(a1_VVV) .and. dl > 2) then
       a1_VVV = a1_eee*ze%zx_V**3 &
            + a1_xxx*x%zx_V**3 &
            + 3.0*a1_ee*ze%zx_V*ze%zx_VV &
            + 3.0*a1_xx*x%zx_V*x%zx_VV &
            + 3.0*a1_ex*ze%zx_V*x%zx_VV &
            + 3.0*a1_exx*x%zx_V**2*ze%zx_V &
            + 3.0*a1_ex*x%zx_V*ze%zx_VV &
            + 3.0*a1_eex*x%zx_V*ze%zx_V**2 &
            + a1_e*ze%zx_VVV &
            + a1_x*x%zx_VVV
    endif
    if (present(a1_TV) .and. dl > 1) then
       a1_TV = a1_ee*ze%zx_T*ze%zx_V &
            + a1_ex*x%zx_T*ze%zx_V &
            + a1_e*ze%zx_TV &
            + a1_xx*x%zx_T*x%zx_V &
            + a1_ex*x%zx_V*ze%zx_T &
            + a1_x*x%zx_TV
    endif
    if (present(a1_VVT) .and. dl > 2) then
       a1_VVT = a1_eee*ze%zx_V**2*ze%zx_T &
            + a1_ee*ze%zx_VV*ze%zx_T &
            + a1_ex*x%zx_T*ze%zx_VV + a1_eex*x%zx_T*ze%zx_V**2 &
            + a1_e*ze%zx_VVT &
            + 2.0*a1_ee*ze%zx_V*ze%zx_TV &
            + a1_x*x%zx_VVT &
            + a1_xx*x%zx_T*x%zx_VV &
            + a1_ex*ze%zx_T*x%zx_VV &
            + 2.0*a1_xx*x%zx_V*x%zx_TV &
            + a1_xxx*x%zx_T*x%zx_V**2 &
            + a1_exx*ze%zx_T*x%zx_V**2 &
            + 2.0*a1_ex*x%zx_TV*ze%zx_V &
            + 2.0*a1_ex*x%zx_V*ze%zx_TV &
            + 2.0*a1_exx*x%zx_T*x%zx_V*ze%zx_V &
            + 2.0*a1_eex*ze%zx_T*x%zx_V*ze%zx_V
    endif
    if (present(a1_VTT) .and. dl > 2) then
       a1_VTT = a1_ee*ze%zx_V*ze%zx_TT &
            + a1_ex*ze%zx_V*x%zx_TT &
            + a1_eee*ze%zx_V*ze%zx_T**2 &
            + a1_exx*ze%zx_V*x%zx_T**2 &
            + a1_e*ze%zx_VTT &
            + 2.0*a1_ee*ze%zx_T*ze%zx_TV &
            + 2.0*a1_eex*ze%zx_V*x%zx_T*ze%zx_T &
            + 2.0*a1_ex*ze%zx_TV*x%zx_T &
            + a1_x*x%zx_VTT &
            + 2.0*a1_xx*x%zx_TV*x%zx_T &
            + 2.0*a1_ex*x%zx_TV*ze%zx_T &
            + a1_xxx*x%zx_T**2*x%zx_V &
            + a1_xx*x%zx_TT*x%zx_V &
            + 2.0*a1_exx*x%zx_T*x%zx_V*ze%zx_T &
            + a1_ex*x%zx_V*ze%zx_TT &
            + a1_eex*x%zx_V*ze%zx_T**2
    endif
    if (present(a1_n) .and. dl > 0) then
       a1_n = a1_e*ze%zx_n + a1_x*x%zx_n
    endif
    if (present(a1_nn) .and. dl > 1) then
       do k=1,nc
          a1_nn(:,k) = a1_ee*ze%zx_n(:)*ze%zx_n(k) &
               + a1_e*ze%zx_nn(:,k) + a1_ex*ze%zx_n(k)*x%zx_n(:) &
               + a1_xx*x%zx_n(:)*x%zx_n(k) &
               + a1_x*x%zx_nn(:,k) + a1_ex*ze%zx_n(:)*x%zx_n(k)
       enddo
    endif
    if (present(a1_Tn) .and. dl > 1) then
       a1_Tn = a1_ee*ze%zx_T*ze%zx_n &
            + a1_ex*x%zx_T*ze%zx_n &
            + a1_e*ze%zx_Tn &
            + a1_xx*x%zx_T*x%zx_n &
            + a1_ex*x%zx_n*ze%zx_T &
            + a1_x*x%zx_Tn
    endif
    if (present(a1_Vn) .and. dl > 1) then
       a1_Vn = a1_ee*ze%zx_V*ze%zx_n &
            + a1_ex*x%zx_V*ze%zx_n &
            + a1_e*ze%zx_Vn &
            + a1_xx*x%zx_V*x%zx_n &
            + a1_ex*x%zx_n*ze%zx_V &
            + a1_x*x%zx_Vn
    endif
    if (present(a1_VVn) .and. dl > 2) then
       a1_VVn = a1_eee*ze%zx_V**2*ze%zx_n &
            + a1_ee*ze%zx_VV*ze%zx_n &
            + a1_ex*x%zx_n*ze%zx_VV + a1_eex*x%zx_n*ze%zx_V**2 &
            + a1_e*ze%zx_VVn &
            + 2.0*a1_ee*ze%zx_V*ze%zx_Vn &
            + a1_x*x%zx_VVn &
            + a1_xx*x%zx_n*x%zx_VV &
            + a1_ex*ze%zx_n*x%zx_VV &
            + 2.0*a1_xx*x%zx_V*x%zx_Vn &
            + a1_xxx*x%zx_n*x%zx_V**2 &
            + a1_exx*ze%zx_n*x%zx_V**2 &
            + 2.0*a1_ex*x%zx_Vn*ze%zx_V &
            + 2.0*a1_ex*x%zx_V*ze%zx_Vn &
            + 2.0*a1_exx*x%zx_n*x%zx_V*ze%zx_V &
            + 2.0*a1_eex*ze%zx_n*x%zx_V*ze%zx_V
    endif
    if (present(a1_VTn) .and. dl > 2) then
       a1_VTn = a1_eee*ze%zx_V*ze%zx_T*ze%zx_n &
            + a1_ee*ze%zx_T*ze%zx_Vn &
            + a1_ee*ze%zx_V*ze%zx_Tn &
            + a1_ee*ze%zx_TV*ze%zx_n &
            + a1_eex*x%zx_T*ze%zx_V*ze%zx_n &
            + a1_ex*x%zx_T*ze%zx_Vn &
            + a1_e*ze%zx_VTn &
            + a1_eex*x%zx_n*ze%zx_V*ze%zx_T &
            + a1_exx*x%zx_n*ze%zx_V*x%zx_T &
            + a1_ex*x%zx_Tn*ze%zx_V &
            + a1_ex*x%zx_n*ze%zx_TV &
            + a1_exx*x%zx_n*x%zx_V*ze%zx_T &
            + a1_eex*ze%zx_n*x%zx_V*ze%zx_T &
            + a1_ex*x%zx_V*ze%zx_Tn &
            + a1_ex*x%zx_Vn*ze%zx_T &
            + a1_x*x%zx_VTn &
            + a1_xx*x%zx_TV*x%zx_n &
            + a1_ex*x%zx_TV*ze%zx_n &
            + a1_xxx*x%zx_T*x%zx_n*x%zx_V &
            + a1_exx*x%zx_T*ze%zx_n*x%zx_V &
            + a1_xx*x%zx_V*ze%zx_Tn &
            + a1_xx*x%zx_Vn*ze%zx_T
    endif
    if (present(a1_Vnn) .and. dl > 2) then
       do k=1,nc
          a1_Vnn(:,k) = a1_eee*ze%zx_V*ze%zx_n(:)*ze%zx_n(k) &
               + a1_eex*x%zx_V*ze%zx_n(:)*ze%zx_n(k) &
               + a1_ee*ze%zx_Vn(:)*ze%zx_n(k) &
               + a1_ee*ze%zx_n(:)*ze%zx_Vn(k) &
               + a1_e*ze%zx_Vnn(:,k) &
               + a1_ee*ze%zx_V*ze%zx_nn(:,k) &
               + a1_ex*x%zx_V*ze%zx_nn(:,k) &
               + a1_eex*ze%zx_V*ze%zx_n(k)*x%zx_n(:) &
               + a1_exx*x%zx_V*ze%zx_n(k)*x%zx_n(:) &
               + a1_ex*ze%zx_Vn(k)*x%zx_n(:) &
               + a1_ex*ze%zx_n(k)*x%zx_Vn(:) &
               + a1_exx*ze%zx_V*x%zx_n(:)*x%zx_n(k) &
               + a1_xxx*x%zx_V*x%zx_n(:)*x%zx_n(k) &
               + a1_xx*x%zx_Vn(:)*x%zx_n(k) &
               + a1_xx*x%zx_n(:)*x%zx_Vn(k) &
               + a1_x*x%zx_Vnn(:,k) &
               + a1_ex*ze%zx_V*x%zx_nn(:,k) &
               + a1_xx*x%zx_V*x%zx_nn(:,k) &
               + a1_eex*ze%zx_V*ze%zx_n(:)*x%zx_n(k) &
               + a1_exx*x%zx_V*ze%zx_n(:)*x%zx_n(k) &
               + a1_ex*ze%zx_Vn(:)*x%zx_n(k) &
               + a1_ex*ze%zx_n(:)*x%zx_Vn(k)
       enddo
    endif
  end subroutine convert_zeta_zeta_to_TVn

end module saftvrmie_utils
