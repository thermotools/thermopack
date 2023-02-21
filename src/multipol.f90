!> Residual reduced Helmholtz energies from dipol amd quadrupol interactions:
!! Gross 2005: 10.1002/aic.10502
!! Gross and Vrabec 2006: 10.1002/aic.10683
!! Vrabec and Gross 2008: 10.1021/jp072619u
module multipol
  use hyperdual_mod
  use pc_saft_nonassoc, only: sPCSAFT_eos
  use thermopack_var, only: base_eos_param, get_active_eos, nce
  use thermopack_constants, only: N_AVOGADRO, kB_const
  use numconstants, only: PI, small
  use multipol_var, only: multipol_param
  implicit none

  real, parameter :: ALPHA = 1.1937350

  public :: hyperdual_fres_multipol
  public :: add_hyperdual_fres_multipol
  public :: fres_multipol

contains

  ! The segment diameter d_i.
  function hyperdual_calc_d_hs_pc_saft(eos,nce,T) result(d_hs)
    class(sPCSAFT_eos), intent(in) :: eos
    integer, intent(in) :: nce
    type(hyperdual), intent(in) :: T   !< [K]
    type(hyperdual) :: d_hs(nce)       !< [m]
    ! Locals.
    integer :: i
    do i=1,nce
      d_hs(i) = eos%sigma(i,i)*(1.0 - 0.12*exp(-3*eos%eps_depth_divk(i,i)/T))
    end do
  end function hyperdual_calc_d_hs_pc_saft

  function hyperdual_packing_fraction_pc_saft(eos,nce,V,n,d_hs) result(eta)
    use hyperdual_mod
    implicit none
    class(sPCSAFT_eos), intent(in) :: eos
    integer, intent(in) :: nce
    type(hyperdual), intent(in) :: V, n(nce)
    type(hyperdual), intent(in) :: d_hs(nce)
    type(hyperdual) :: eta
    !
    eta = N_AVOGADRO*PI/6/V*sum(eos%m*n*d_hs**3)
  end function hyperdual_packing_fraction_pc_saft

  function hyperdual_fres_multipol(p_eos,nce,T,V,n) result(f)
    use hyperdual_mod
    use thermopack_var, only: base_eos_param
    implicit none
    class(base_eos_param), intent(inout), pointer :: p_eos
    integer, intent(in) :: nce
    type(hyperdual), intent(in) :: T, V, n(nce)
    type(hyperdual) :: f
    ! Locals
    type(hyperdual) :: d_hs(nce)
    type(hyperdual) :: eta
    type(hyperdual) :: f_QQ, f_DD, f_DQ
    f = 0.0
    if (.not. associated(p_eos%mpol_param)) return
    if (p_eos%mpol_param%num_mu == 0 .and. p_eos%mpol_param%num_Q == 0) return
    select type(eos => p_eos)
    class is(sPCSAFT_eos)
      d_hs = hyperdual_calc_d_hs_pc_saft(eos,nce,T)
      eta = hyperdual_packing_fraction_pc_saft(eos,nce,V,n,d_hs)
    class default
      call stoperror("multipol:: Wrong type!")
    end select

    if ( p_eos%mpol_param%num_mu > 0 .and. &
         p_eos%mpol_param%enable_DD) then
      f_DD = hyperdual_f_dd(nce,T,V,n,eta,p_eos%mpol_param)
    else
      f_DD = 0.0
    endif
    if ( p_eos%mpol_param%num_Q > 0 .and. &
         p_eos%mpol_param%enable_QQ) then
      f_QQ = hyperdual_f_qq(nce,T,V,n,eta,p_eos%mpol_param)
    else
      f_QQ = 0.0
    endif
    if ( p_eos%mpol_param%num_mu > 0 .and. &
         p_eos%mpol_param%num_Q > 0  .and. &
         p_eos%mpol_param%enable_DQ) then
      f_DQ = hyperdual_f_dq(nce,T,V,n,eta,p_eos%mpol_param)
    else
      f_DQ = 0.0
    endif
    f = f_DD + f_QQ + f_DQ
  end function hyperdual_fres_multipol

  function hyperdual_j2_ij(nce,T,eta,a_ij,b_ij,i,j,lmax) result(j2_ij)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nce,i,j,lmax
    real, intent(in) :: a_ij(0:lmax,nce,nce),b_ij(0:lmax,nce,nce)
    type(hyperdual), intent(in) :: T, eta
    type(hyperdual) :: j2_ij
    ! Locals
    integer :: l
    J2_ij = 0.0
    do l=0,lmax
      J2_ij = J2_ij + (a_ij(l,i,j) + b_ij(l,i,j)/T)*eta**l
    enddo
  end function hyperdual_j2_ij

  function hyperdual_j3_ijk(nce,eta,c_ijk,i,j,k,lmax) result(j3_ijk)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nce,i,j,k,lmax
    real, intent(in) :: c_ijk(0:lmax,nce,nce,nce)
    type(hyperdual), intent(in) :: eta
    type(hyperdual) :: j3_ijk
    ! Locals
    integer :: l
    j3_ijk = 0.0
    do l=0,lmax
      j3_ijk = j3_ijk + c_ijk(l,i,j,k)*eta**l
    enddo
  end function hyperdual_j3_ijk

  !> Quadrupol-quadrupol interaction
  !! Gross 2005: 10.1002/aic.10502
  function hyperdual_f_qq(nce,T,V,n,eta,mpol_param) result(f_QQ)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nce
    type(hyperdual), intent(in) :: T, V, n(nce), eta
    class(multipol_param), intent(in) :: mpol_param
    type(hyperdual) :: f_QQ
    ! Locals
    type(hyperdual) :: a_2, a_3, prefactor_2, prefactor_3
    integer :: i, j, k, ii, jj, kk
    prefactor_2 = -PI*0.75**2*N_AVOGADRO/V/T**2
    prefactor_3 = (PI*0.75)**2*N_AVOGADRO**2/V**2/T**3
    a_2 = 0.0
    a_3 = 0.0
    do ii=1,mpol_param%num_Q
      i = mpol_param%Q_indices(ii)
      call add_to_a2(i,i,fac=1.0)
      call add_to_a3(i,i,i,fac=1.0)
      do jj=ii+1,mpol_param%num_Q
        j = mpol_param%Q_indices(jj)
        call add_to_a2(i,j,fac=2.0)
        call add_to_a3(i,i,j,fac=3.0)
        call add_to_a3(i,j,j,fac=3.0)
        do kk=jj+1,mpol_param%num_Q
          k = mpol_param%Q_indices(kk)
          call add_to_a3(i,j,k,fac=6.0)
        enddo
      enddo
    enddo
    a_2 = prefactor_2 * a_2
    a_3 = prefactor_3 * a_3
    if (a_2%f0 - a_3%f0 /= 0.0) then
      f_QQ = a_2 * a_2 /(a_2 - a_3)
    else
      f_QQ = a_2
    endif
  contains
    subroutine add_to_a2(is,js,fac)
      integer :: is, js
      real :: fac
      type(hyperdual) :: J2_ij
      J2_ij = hyperdual_j2_ij(nce,T,eta,mpol_param%a_ij_QQ,mpol_param%b_ij_QQ,is,js,4)
      a_2 = a_2 + fac*n(is)*n(js)*mpol_param%l_Q(is)*mpol_param%l_Q(js)&
           *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)&
           *mpol_param%sigma_ij_5(is,is)*mpol_param%sigma_ij_5(js,js)&
           /(mpol_param%sigma_ij_5(is,js)*mpol_param%sigma_ij(is,js)**2)&
           *mpol_param%Q_star_2(is)*mpol_param%Q_star_2(js)*J2_ij
    end subroutine add_to_a2
    subroutine add_to_a3(is,js,ks,fac)
      integer :: is, js, ks
      real :: fac
      type(hyperdual) :: J3_ijk
      J3_ijk = hyperdual_j3_ijk(nce,eta,mpol_param%c_ijk_QQ,is,js,ks,3)
      a_3 = a_3 + fac*n(is)*n(js)*n(ks)*mpol_param%l_Q(is)*mpol_param%l_Q(js)*mpol_param%l_Q(ks)&
           *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)*mpol_param%eps_divk_ij(ks,ks)&
           *mpol_param%sigma_ij_5(is,is)*mpol_param%sigma_ij_5(js,js)*mpol_param%sigma_ij_5(ks,ks)&
           /(mpol_param%sigma_ij_3(is,js)*mpol_param%sigma_ij_3(is,ks)*mpol_param%sigma_ij_3(js,ks))&
           *mpol_param%Q_star_2(is)*mpol_param%Q_star_2(js)*mpol_param%Q_star_2(ks)*J3_ijk
    end subroutine add_to_a3
  end function hyperdual_f_qq

  !> Dipol-dipol interaction
  !! Gross and Vrabec 2006: 10.1002/aic.10683
  function hyperdual_f_dd(nce,T,V,n,eta,mpol_param) result(f_DD)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nce
    type(hyperdual), intent(in) :: T, V, n(nce), eta
    class(multipol_param), intent(in) :: mpol_param
    type(hyperdual) :: f_DD
    ! Locals
    type(hyperdual) :: a_2, a_3, prefactor_2, prefactor_3
    integer :: i, j, k, ii, jj, kk
    prefactor_2 = -PI*N_AVOGADRO/V/T**2
    prefactor_3 = -(4.0/3.0)*(PI*N_AVOGADRO)**2/V**2/T**3
    a_2 = 0.0
    a_3 = 0.0
    do ii=1,mpol_param%num_mu
      i = mpol_param%mu_indices(ii)
      call add_to_a2(i,i,fac=1.0)
      call add_to_a3(i,i,i,fac=1.0)
      do jj=ii+1,mpol_param%num_mu
        j = mpol_param%mu_indices(jj)
        call add_to_a2(i,j,fac=2.0)
        call add_to_a3(i,i,j,fac=3.0)
        call add_to_a3(i,j,j,fac=3.0)
        do kk=jj+1,mpol_param%num_mu
          k = mpol_param%mu_indices(kk)
          call add_to_a3(i,j,k,fac=6.0)
        enddo
      enddo
    enddo
    a_2 = prefactor_2 * a_2
    a_3 = prefactor_3 * a_3
    if (a_2%f0 - a_3%f0 /= 0.0) then
      f_DD = a_2 * a_2 /(a_2 - a_3)
    else
      f_DD = a_2
    endif
  contains
    subroutine add_to_a2(is,js,fac)
      integer :: is, js
      real :: fac
      type(hyperdual) :: J2_ij
      J2_ij = hyperdual_j2_ij(nce,T,eta,mpol_param%a_ij_DD,mpol_param%b_ij_DD,is,js,4)
      a_2 = a_2 + fac*n(is)*n(js)*mpol_param%l_mu(is)*mpol_param%l_mu(js)&
           *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)&
           *mpol_param%sigma_ij_3(is,is)*mpol_param%sigma_ij_3(js,js)&
           /mpol_param%sigma_ij_3(is,js)&
           *mpol_param%mu_star_2(is)*mpol_param%mu_star_2(js)*J2_ij
    end subroutine add_to_a2
    subroutine add_to_a3(is,js,ks,fac)
      integer :: is, js, ks
      real :: fac
      type(hyperdual) :: J3_ijk
      J3_ijk = hyperdual_j3_ijk(nce,eta,mpol_param%c_ijk_DD,is,js,ks,3)
      a_3 = a_3 + fac*n(is)*n(js)*n(ks)*mpol_param%l_mu(is)*mpol_param%l_mu(js)*mpol_param%l_mu(ks)&
           *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)*mpol_param%eps_divk_ij(ks,ks)&
           *mpol_param%sigma_ij_3(is,is)*mpol_param%sigma_ij_3(js,js)*mpol_param%sigma_ij_3(ks,ks)&
           /(mpol_param%sigma_ij(is,js)*mpol_param%sigma_ij(is,ks)*mpol_param%sigma_ij(js,ks))&
           *mpol_param%mu_star_2(is)*mpol_param%mu_star_2(js)*mpol_param%mu_star_2(ks)*J3_ijk
    end subroutine add_to_a3
  end function hyperdual_f_dd

  !> Quadrupol-dipol interaction
  !! Vrabec and Gross 2008: 10.1021/jp072619u
  function hyperdual_f_dq(nce,T,V,n,eta,mpol_param) result(f_DQ)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nce
    type(hyperdual), intent(in) :: T, V, n(nce), eta
    class(multipol_param), intent(in) :: mpol_param
    type(hyperdual) :: f_DQ
    ! Locals
    type(hyperdual) :: a_2, a_3, prefactor_2, prefactor_3
    integer :: i, j, k, ii, jj, kk
    prefactor_2 = -(9.0/4.0)*PI*N_AVOGADRO/V/T**2
    prefactor_3 = -(PI*N_AVOGADRO)**2/V**2/T**3
    a_2 = 0.0
    a_3 = 0.0
    do ii=1,mpol_param%num_mu
      i = mpol_param%mu_indices(ii)
      do jj=1,mpol_param%num_Q
        j = mpol_param%Q_indices(jj)
        call add_to_a2(i,j)
        do kk=1,mpol_param%num_mu
          k = mpol_param%mu_indices(kk)
          call add_to_a3_mu(i,k,j)
        enddo
        do kk=1,mpol_param%num_Q
          k = mpol_param%Q_indices(kk)
          call add_to_a3_Q(i,j,k)
        enddo
      enddo
    enddo
    a_2 = prefactor_2 * a_2
    a_3 = prefactor_3 * a_3

    if (a_2%f0 - a_3%f0 /= 0.0) then
      f_DQ = a_2 * a_2 /(a_2 - a_3)
    else
      f_DQ = a_2
    endif
  contains
    subroutine add_to_a2(is,js)
      integer :: is, js
      type(hyperdual) :: J2_ij
        J2_ij = hyperdual_j2_ij(nce,T,eta,mpol_param%a_ij_DQ,mpol_param%b_ij_DQ,is,js,3)
        a_2 = a_2 + n(is)*n(js)*mpol_param%l_mu(is)*mpol_param%l_Q(js)&
             *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)&
             *mpol_param%sigma_ij_3(is,is)*mpol_param%sigma_ij_5(js,js)&
             /mpol_param%sigma_ij_5(is,js)&
             *mpol_param%mu_star_2(is)*mpol_param%Q_star_2(js)*J2_ij
    end subroutine add_to_a2
    subroutine add_to_a3_mu(is,js,ks)
      integer :: is, js, ks
      type(hyperdual) :: J3_ijk
      J3_ijk = hyperdual_j3_ijk(nce,eta,mpol_param%c_ijk_DQ,is,js,ks,2)
      a_3 = a_3 + n(is)*n(js)*n(ks)*mpol_param%l_mu(is)*mpol_param%l_Q(js)*mpol_param%l_Q(ks)&
           *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)*mpol_param%eps_divk_ij(ks,ks)&
           *(mpol_param%sigma_ij(is,is)*mpol_param%sigma_ij(js,js)*mpol_param%sigma_ij(ks,ks))**4&
           /(mpol_param%sigma_ij(is,js)*mpol_param%sigma_ij(is,ks)*mpol_param%sigma_ij(js,ks))**2&
           *mpol_param%mu_star_2(js)*mpol_param%mu_star_2(is)*mpol_param%Q_star_2(ks)*J3_ijk
    end subroutine add_to_a3_mu
    subroutine add_to_a3_Q(is,js,ks)
      integer :: is, js, ks
      type(hyperdual) :: J3_ijk
      J3_ijk = hyperdual_j3_ijk(nce,eta,mpol_param%c_ijk_DQ,is,js,ks,2)
      a_3 = a_3 + n(is)*n(js)*n(ks)*mpol_param%l_mu(is)*mpol_param%l_Q(js)*mpol_param%l_Q(ks)&
           *mpol_param%eps_divk_ij(is,is)*mpol_param%eps_divk_ij(js,js)*mpol_param%eps_divk_ij(ks,ks)&
           *(mpol_param%sigma_ij(is,is)*mpol_param%sigma_ij(js,js)*mpol_param%sigma_ij(ks,ks))**4&
           /(mpol_param%sigma_ij(is,js)*mpol_param%sigma_ij(is,ks)*mpol_param%sigma_ij(js,ks))**2&
           *ALPHA*mpol_param%Q_star_2(js)*mpol_param%mu_star_2(is)*mpol_param%Q_star_2(ks)*J3_ijk
    end subroutine add_to_a3_Q
  end function hyperdual_f_dq

  subroutine add_hyperdual_fres_multipol(eos,nc,T,V,n,F,F_T,&
       F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use hyperdual_utility, only: hyperdual_fres_wrapper
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in) :: T,V,n(nc)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    ! Locals
    real :: Fmp
    real, target :: Fmp_T,Fmp_V,Fmp_n(nc)
    real, target :: Fmp_TT,Fmp_TV,Fmp_Tn(nc),Fmp_VV,Fmp_Vn(nc),Fmp_nn(nc,nc)
    real, pointer :: Fmp_T_p,Fmp_V_p,Fmp_n_p(:)
    real, pointer :: Fmp_TT_p,Fmp_TV_p,Fmp_Tn_p(:),Fmp_VV_p,Fmp_Vn_p(:),Fmp_nn_p(:,:)

    if (present(F_T)) then
      Fmp_T_p => Fmp_T
    else
      Fmp_T_p => NULL()
    endif
    if (present(F_V)) then
      Fmp_V_p => Fmp_V
    else
      Fmp_V_p => NULL()
    endif
    if (present(F_n)) then
      Fmp_n_p => Fmp_n
    else
      Fmp_n_p => NULL()
    endif
    if (present(F_TT)) then
      Fmp_TT_p => Fmp_TT
    else
      Fmp_TT_p => NULL()
    endif
    if (present(F_VV)) then
      Fmp_VV_p => Fmp_VV
    else
      Fmp_VV_p => NULL()
    endif
    if (present(F_TV)) then
      Fmp_TV_p => Fmp_TV
    else
      Fmp_TV_p => NULL()
    endif
    if (present(F_Tn)) then
      Fmp_Tn_p => Fmp_Tn
    else
      Fmp_Tn_p => NULL()
    endif
    if (present(F_Vn)) then
      Fmp_Vn_p => Fmp_Vn
    else
      Fmp_Vn_p => NULL()
    endif
    if (present(F_nn)) then
      Fmp_nn_p => Fmp_nn
    else
      Fmp_nn_p => NULL()
    endif
    call hyperdual_fres_wrapper(hyperdual_fres_multipol,eos,nc,T,V,n,f=Fmp,f_T=Fmp_T_p,&
         f_V=Fmp_V_p,f_n=Fmp_n_p,f_TT=Fmp_TT_p,f_VV=Fmp_VV_p,f_TV=Fmp_TV_p,f_Tn=Fmp_Tn_p,&
         f_Vn=Fmp_Vn_p,f_nn=Fmp_nn_p)
    if (present(F)) then
      F = F + Fmp
    endif
    if (present(F_T)) then
      F_T = F_T + Fmp_T
    endif
    if (present(F_V)) then
      F_V = F_V + Fmp_V
    endif
    if (present(F_n)) then
      F_n = F_n + Fmp_n
    endif
    if (present(F_TT)) then
      F_TT = F_TT + Fmp_TT
    endif
    if (present(F_VV)) then
      F_VV = F_VV + Fmp_VV
    endif
    if (present(F_TV)) then
      F_TV = F_TV + Fmp_TV
    endif
    if (present(F_Tn)) then
      F_Tn = F_Tn + Fmp_Tn
    endif
    if (present(F_Vn)) then
      F_Vn = F_Vn + Fmp_Vn
    endif
    if (present(F_nn)) then
      F_nn = F_nn + Fmp_nn
    endif
  end subroutine add_hyperdual_fres_multipol

  subroutine fres_multipol(T,V,n,QQ,DD,DQ,F)
    ! Input.
    real, intent(in) :: T,V,n(nce)
    logical, intent(in) :: QQ,DD,DQ ! Control what terms are calculated
    ! Output.
    real, intent(out) :: F
    ! Locals
    class(base_eos_param), pointer :: eos

    eos => get_active_eos()

    if (associated(eos%mpol_param)) then
      eos%mpol_param%enable_QQ = QQ
      eos%mpol_param%enable_DD = DD
      eos%mpol_param%enable_DQ = DQ
    endif

    F = 0
    call add_hyperdual_fres_multipol(eos,nce,T,V,n,F)

  end subroutine fres_multipol

end module multipol
