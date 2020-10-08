!---------------------------------------------------------------------
! Module and subroutines for the chain part of SAFT-VR Mie
! Programmed by: M. Hammer
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------

module saftvrmie_chain
  use saftvrmie_containers, only: saftvrmie_zeta, saftvrmie_dhs, &
       saftvrmie_aij, saftvrmie_param, init_saftvrmie_containers, &
       saftvrmie_param_container, saftvrmie_var_container, allocate_saftvrmie_zeta, &
       cleanup_saftvrmie_zeta
  use thermopack_constants, only: N_AVOGADRO
  use numconstants, only: pi, machine_prec
  use saftvrmie_dispersion, only: calcA1Sutherland, calcBTilde, &
       calcXDifferentials, calcXDifferentialsPureHSRef
  use saftvrmie_utils, only: calc_a_zeta_product, convert_zeta_x_to_TVn, &
       calc_a0_a_product, calc_a0_plus_a1, convert_zeta_zeta_to_TVn
  use saftvrmie_options
  implicit none
  !private
  save

  public :: calcAchain, rdf_at_contact

contains

  !> Calculate chain contribution to Helmholtz energy
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcAchain(nc,T,V,n,saftvrmie_vc,ach,ach_T,ach_V,ach_n,ach_TT,&
       ach_VV,ach_TV,ach_Tn,ach_Vn,ach_nn,returnF)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    logical, optional, intent(in) :: returnF !< Return A/RT instead of default A/(nRT)
    ! Output
    real, intent(out) :: ach
    real, optional, intent(out) :: ach_T,ach_V,ach_TT,ach_VV,ach_TV
    real, optional, dimension(nc), intent(out) :: ach_n,ach_Tn,ach_Vn
    real, optional, dimension(nc,nc), intent(out) :: ach_nn
    ! Locals
    integer :: k,l
    real :: nsum,temp_sum,ach_T_l,ach_V_l
    real, dimension(nc) :: g,g_T,g_V,g_TT,g_VV,g_TV,ach_n_l
    real, dimension(nc,nc) :: g_n,g_Tn,g_Vn
    real, dimension(nc,nc,nc) :: g_nn
    logical :: returnFl
    real :: factor

    if (sum(abs(saftvrmie_param%ms-1.0)) < machine_prec) then
       ach = 0.0
       if (present(ach_T)) then
          ach_T = 0.0
       endif
       if (present(ach_V)) then
          ach_V = 0.0
       endif
       if (present(ach_TV)) then
          ach_TT = 0.0
       endif
       if (present(ach_TV)) then
          ach_TV = 0.0
       endif
       if (present(ach_VV)) then
          ach_VV = 0.0
       endif
       if (present(ach_Tn)) then
          ach_Tn = 0.0
       endif
       if (present(ach_Vn)) then
          ach_Vn = 0.0
       endif
       if (present(ach_n)) then
          ach_n = 0.0
       endif
       if (present(ach_nn)) then
          ach_nn = 0.0
       endif
       return
    endif
    if (present(returnF)) then
       returnFl = returnF
    else
       returnFl = .false.
    endif
    if (returnFl) then
       nsum = 1.0
       factor = 0.0
    else
       nsum = sum(n)
       factor = 1.0
    endif

    if ( present(ach_TT) .or. present(ach_VV) .or. present(ach_TV) .or. &
         present(ach_Tn) .or. present(ach_Vn) .or. present(ach_nn)) then
       call rdf_at_contact(nc,T,V,n,saftvrmie_vc,g,g_T=g_T,g_V=g_V,g_n=g_n,&
            g_TT=g_TT,g_VV=g_VV,g_TV=g_TV,g_Tn=g_Tn,g_Vn=g_Vn,g_nn=g_nn)
    else if (present(ach_T) .or. present(ach_V) .or. present(ach_n)) then
       call rdf_at_contact(nc,T,V,n,saftvrmie_vc,g,g_T=g_T,g_V=g_V,g_n=g_n)
    else
       call rdf_at_contact(nc,T,V,n,saftvrmie_vc,g)
    endif
    ach = -sum(n*(saftvrmie_param%ms-1.0)*log(g))/nsum
    if (present(ach_T) .or. present(ach_Tn)) then
       ach_T_l = -sum(n*(saftvrmie_param%ms-1.0)*g_T/g)/nsum
       if (present(ach_T)) then
          ach_T = ach_T_l
       endif
    endif
    if (present(ach_V) .or. present(ach_VV)) then
       ach_V_l = -sum(n*(saftvrmie_param%ms-1.0)*g_V/g)/nsum
       if (present(ach_V)) then
          ach_V = ach_V_l
       endif
    endif
    if (present(ach_n) .or. present(ach_nn)) then
       do k=1,nc
          temp_sum = sum(n*(saftvrmie_param%ms-1.0)*g_n(k,:)/g)
          ach_n_l(k) = -((saftvrmie_param%ms(k)-1.0)*log(g(k)) + &
               temp_sum + factor*ach)/nsum
       enddo
       if (present(ach_n)) then
          ach_n = ach_n_l
       endif
    endif
    if (present(ach_TT)) then
       ach_TT = sum(n*(saftvrmie_param%ms-1.0)*((g_T/g)**2-g_TT/g))/nsum
    endif
    if (present(ach_VV)) then
       ach_VV = sum(n*(saftvrmie_param%ms-1.0)*((g_V/g)**2-g_VV/g))/nsum
    endif
    if (present(ach_TV)) then
       ach_TV = sum(n*(saftvrmie_param%ms-1.0)*((g_V/g)*(g_T/g)-g_TV/g))/nsum
    endif
    if (present(ach_Tn)) then
       do k=1,nc
          temp_sum = sum(n*(saftvrmie_param%ms-1.0)*((g_n(k,:)/g)*(g_T/g)-g_Tn(k,:)/g))
          ach_Tn(k) = (-(saftvrmie_param%ms(k)-1.0)*g_T(k)/g(k) + &
               temp_sum - factor*ach_T_l)/nsum
       enddo
    endif
    if (present(ach_Vn)) then
       do k=1,nc
          temp_sum = sum(n*(saftvrmie_param%ms-1.0)*((g_n(k,:)/g)*(g_V/g)-g_Vn(k,:)/g))
          ach_Vn(k) = (-(saftvrmie_param%ms(k)-1.0)*g_V(k)/g(k) + &
               temp_sum - factor*ach_V_l)/nsum
       enddo
    endif
    if (present(ach_nn)) then
       do k=1,nc
          do l=1,nc
             temp_sum = sum(n*(saftvrmie_param%ms-1.0)*((g_n(k,:)/g)*(g_n(l,:)/g) - g_nn(k,l,:)/g))
             ach_nn(k,l) = (-(saftvrmie_param%ms(k)-1.0)*g_n(l,k)/g(k) &
                  -(saftvrmie_param%ms(l)-1.0)*g_n(k,l)/g(l) &
                  + temp_sum - factor*ach_n_l(l) &
                  - factor*ach_n_l(k))/nsum
          enddo
       enddo
    endif
  end subroutine calcAchain

  !> Calculate radial-distribution-function at contact
  !!
  !! \author Morten Hammer, February 2018
  subroutine rdf_at_contact(nc,T,V,n,saftvrmie_vc,&
       g,g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,&
       order)
    use numconstants, only: expMax
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    integer, optional, intent(in) :: order !< Order of expansion. Intended only for plotting and testing at difflevel=0
    ! Output
    real, dimension(nc), intent(out) :: g
    real, optional, dimension(nc), intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc,nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc,nc), intent(out) :: g_nn
    ! Locals
    real, dimension(nc) :: gd,gd_T,gd_V,gd_TT,gd_VV,gd_TV
    real, dimension(nc,nc) :: gd_n,gd_Tn,gd_Vn
    real, dimension(nc,nc,nc) :: gd_nn
    real, dimension(nc) :: g1,g1_T,g1_V,g1_TT,g1_VV,g1_TV
    real, dimension(nc,nc) :: g1_n,g1_Tn,g1_Vn
    real, dimension(nc,nc,nc) :: g1_nn
    real, dimension(nc) :: g2,g2_T,g2_V,g2_TT,g2_VV,g2_TV
    real, dimension(nc,nc) :: g2_n,g2_Tn,g2_Vn
    real, dimension(nc,nc,nc) :: g2_nn
    real, dimension(nc) :: w,w_T,w_V,w_TT,w_VV,w_TV
    real, dimension(nc,nc) :: w_n,w_Tn,w_Vn
    real, dimension(nc,nc,nc) :: w_nn
    real, dimension(nc) :: beta_eps
    real, parameter :: w_lim = 0.9*expMax
    integer :: i,k,l,difflevel
    logical :: no_correction
    no_correction = .false.
    if (present(order)) then
       if (order == -2) then
          no_correction = .true.
       endif
    endif
    do i=1,nc
       beta_eps(i) = saftvrmie_param%eps_divk_ij(i,i)/T
    enddo
    if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
         present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
       difflevel = 2
       call rdf_at_contact_zeroth_order_TVn(nc,T,V,n,saftvrmie_vc,&
            gd,g_T=gd_T,g_V=gd_V,g_n=gd_n,&
            g_TT=gd_TT,g_VV=gd_VV,g_TV=gd_TV,g_Tn=gd_Tn,g_Vn=gd_Vn,g_nn=gd_nn)
       call rdf_at_contact_first_order_TVn(nc,T,V,n,saftvrmie_vc,&
            g1,g_T=g1_T,g_V=g1_V,g_n=g1_n,&
            g_TT=g1_TT,g_VV=g1_VV,g_TV=g1_TV,g_Tn=g1_Tn,g_Vn=g1_Vn,g_nn=g1_nn)
       call rdf_at_contact_second_order_TVn(nc,T,V,n,saftvrmie_vc,no_correction,&
            g2,g_T=g2_T,g_V=g2_V,g_n=g2_n,&
            g_TT=g2_TT,g_VV=g2_VV,g_TV=g2_TV,g_Tn=g2_Tn,g_Vn=g2_Vn,g_nn=g2_nn)
    else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
       difflevel = 1
       call rdf_at_contact_zeroth_order_TVn(nc,T,V,n,saftvrmie_vc,&
            gd,g_T=gd_T,g_V=gd_V,g_n=gd_n)
       call rdf_at_contact_first_order_TVn(nc,T,V,n,saftvrmie_vc,&
            g1,g_T=g1_T,g_V=g1_V,g_n=g1_n)
       call rdf_at_contact_second_order_TVn(nc,T,V,n,saftvrmie_vc,no_correction,&
            g2,g_T=g2_T,g_V=g2_V,g_n=g2_n)
    else
       difflevel = 0
       call rdf_at_contact_zeroth_order_TVn(nc,T,V,n,saftvrmie_vc,gd)
       call rdf_at_contact_first_order_TVn(nc,T,V,n,saftvrmie_vc,g1)
       call rdf_at_contact_second_order_TVn(nc,T,V,n,saftvrmie_vc,no_correction,g2)
     endif
     w = (beta_eps*g1 + beta_eps**2*g2)/gd
    if (difflevel > 0) then
       w_T = (-w*gd_T + beta_eps*(-g1/T + g1_T) + beta_eps**2*(-2.0*g2/T + g2_T))/gd
       w_V = (-w*gd_V + beta_eps*g1_V + beta_eps**2*g2_V)/gd
       do i=1,nc
          do k=1,nc
             w_n(k,i) = (-w(i)*gd_n(k,i) + beta_eps(i)*g1_n(k,i) &
                  + beta_eps(i)**2*g2_n(k,i))/gd(i)
          enddo
       enddo
    endif
    if (difflevel > 1) then
       w_VV = (beta_eps*g1_VV + beta_eps**2*g2_VV - 2.0*gd_V*w_V - w*gd_VV)/gd
       w_TV = (beta_eps*(-g1_V/T + g1_TV) + beta_eps**2*(-2.0*g2_V/T + g2_TV)&
            - gd_V*w_T - gd_T*w_V - w*gd_TV)/gd
       w_TT = (beta_eps*(2.0*g1/T**2 - 2.0*g1_T/T + g1_TT) &
            + beta_eps**2*(6.0*g2/T**2 - 4.0*g2_T/T + g2_TT) - 2.0*gd_T*w_T - w*gd_TT)/gd
       if (present(g_Vn)) then
          do i=1,nc
             do k=1,nc
                w_Vn(k,i) = (beta_eps(i)*g1_Vn(k,i)&
                     + beta_eps(i)**2*g2_Vn(k,i)&
                     - w(i)*gd_Vn(k,i) - w_n(k,i)*gd_V(i) - w_V(i)*gd_n(k,i))/gd(i)
             enddo
          enddo
       endif
       if (present(g_Tn)) then
          do i=1,nc
             do k=1,nc
                w_Tn(k,i) = ((beta_eps(i)*(-g1_n(k,i)/T + g1_Tn(k,i))&
                     + beta_eps(i)**2*(-2.0*g2_n(k,i)/T + g2_Tn(k,i)))&
                     - w(i)*gd_Tn(k,i) - w_n(k,i)*gd_T(i) - w_T(i)*gd_n(k,i))/gd(i)
             enddo
          enddo
       endif
       if (present(g_nn)) then
          do i=1,nc
             do k=1,nc
                do l=1,nc
                   w_nn(k,l,i) = ((beta_eps(i)*g1_nn(k,l,i) + beta_eps(i)**2*g2_nn(k,l,i)) &
                        -gd_n(l,i)*w_n(k,i)-gd_n(k,i)*w_n(l,i)-gd_nn(k,l,i)*w(i))/gd(i)
                enddo
             enddo
          enddo
       endif
    endif
    do i=1,nc
      w(i) = min(max(-w_lim,w(i)),w_lim)
      g(i) = gd(i)*exp(w(i))
    enddo
    if (present(g_V)) then
       g_V = g*(gd_V/gd+w_V)
    endif
    if (present(g_T)) then
       g_T = g*(gd_T/gd+w_T)
    endif
    if (present(g_n)) then
       do i=1,nc
          do k=1,nc
             g_n(k,i) = g(i)*(gd_n(k,i)/gd(i)+w_n(k,i))
          enddo
       enddo
    endif
    if (present(g_VV)) then
       g_VV = g*((gd_VV+2.0*gd_V*w_V)/gd+w_VV+w_V**2)
    endif
    if (present(g_TT)) then
       g_TT = g*((gd_TT+2.0*gd_T*w_T)/gd+w_TT+w_T**2)
    endif
    if (present(g_TV)) then
       g_TV = g*((gd_TV+gd_T*w_V+gd_V*w_T)/gd+w_TV+w_T*w_V)
    endif
    if (present(g_Tn)) then
       do i=1,nc
          do k=1,nc
             g_Tn(k,i) = g(i)*((gd_Tn(k,i)+gd_T(i)*w_n(k,i)+gd_n(k,i)*w_T(i))/gd(i)&
                  +w_Tn(k,i)+w_T(i)*w_n(k,i))
          enddo
       enddo
    endif
    if (present(g_Vn)) then
       do i=1,nc
          do k=1,nc
             g_Vn(k,i) = g(i)*((gd_Vn(k,i)+gd_V(i)*w_n(k,i)+gd_n(k,i)*w_V(i))/gd(i)&
                  +w_Vn(k,i)+w_V(i)*w_n(k,i))
          enddo
       enddo
    endif
    if (present(g_nn)) then
       do i=1,nc
          do k=1,nc
             do l=1,nc
                g_nn(k,l,i) = g(i)*((gd_nn(k,l,i)+gd_n(k,i)*w_n(l,i)+gd_n(l,i)*w_n(k,i))/gd(i)&
                     +w_nn(k,l,i)+w_n(k,i)*w_n(l,i))
             enddo
          enddo
       enddo
    endif

    if (present(order)) then
       if (difflevel > 0) then
          call stoperror("Expansion order of g only intended for testing and plotting")
       endif
       if (order == 0) then
          g = gd
       else if (order == 1) then
          w = beta_eps*g1/gd
          g = gd*exp(w)
       else if (order == 2) then
          ! As is
       else if (order == -2) then ! Dummy for non-corrected second order expansion
          ! Already correted
       endif
    endif
  end subroutine rdf_at_contact

  !> Calculate radial-distribution-function at contact, zeroth order expansion
  !! calculate from TVn
  !!
  !! \author Morten Hammer, February 2018
  subroutine rdf_at_contact_zeroth_order_TVn(nc,T,V,n,s_vc,g,g_T,g_V,g_n,g_TT,&
       g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, dimension(nc), intent(out) :: g
    real, optional, dimension(nc), intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc,nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc,nc), intent(out) :: g_nn
    ! Locals
    integer :: i !< Component index
    !
    do i=1,nc
       if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
            present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
          call rdf_at_contact_zeroth_order_TVn_i(nc,T,V,n,i,s_vc,g(i),g_T(i),&
               g_V(i),g_n(:,i),g_TT(i),g_VV(i),g_TV(i),g_Tn(:,i),g_Vn(:,i),g_nn(:,:,i))
       else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
          call rdf_at_contact_zeroth_order_TVn_i(nc,T,V,n,i,s_vc,g(i),g_T(i),&
               g_V(i),g_n(:,i))
       else
          call rdf_at_contact_zeroth_order_TVn_i(nc,T,V,n,i,s_vc,g(i))
       endif
    enddo
  end subroutine rdf_at_contact_zeroth_order_TVn

  !> Calculate radial-distribution-function at contact, zeroth order expansion
  !! calculate from TVn
  !!
  !! \author Morten Hammer, March 2019
  subroutine rdf_at_contact_zeroth_order_TVn_i(nc,T,V,n,i,s_vc,g,g_T,g_V,g_n,g_TT,&
       g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(inout) :: g
    real, optional, intent(inout) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(inout) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(inout) :: g_nn
    ! Locals
    real :: g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,x0_ii,e,x0_ii_T,x0_ii_TT
    type(saftvrmie_zeta) :: x0z
    integer :: difflevel
    !
    if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
         present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
       difflevel = 2
    else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
       difflevel = 1
    else
       difflevel = 0
    endif
    e = s_vc%zeta%zx
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,saftvrmie_param%sigma_ij(i,i),0.0,0.0,&
            s_vc%d_pure,x0z)
       x0_ii = x0z%zx
    else
       call calcXDifferentials(saftvrmie_param%sigma_ij(i,i),0.0,0.0,&
            s_vc%dhs%d(i,i),s_vc%dhs%d_T(i,i),s_vc%dhs%d_TT(i,i),x0_ii,x0_ii_T,x0_ii_TT)
    endif
    call rdf_at_contact_zeroth_order(e,x0_ii,g,g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,&
         difflevel_in=difflevel)
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
            g,g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,0.0,0.0,0.0,0.0,&
            g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,&
            difflevel=difflevel)
       call cleanup_saftvrmie_zeta(x0z)
    else
       call convert_zeta_x_to_TVn(nc,x0_ii,x0_ii_T,x0_ii_TT,s_vc%zeta,&
            g,g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,0.0,0.0,0.0,&
            g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,&
            difflevel=difflevel)
    endif
  end subroutine rdf_at_contact_zeroth_order_TVn_i

  !> Calculate radial-distribution-function at contact, zeroth order expansion
  !!
  !! \author Morten Hammer, February 2018
  subroutine rdf_at_contact_zeroth_order(eta,x0,g,g_e,g_x,g_ee,g_xx,g_ex,difflevel_in)
    ! Input
    real, intent(in) :: eta
    real, intent(in) :: x0
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_e,g_x,g_ee,g_xx,g_ex
    integer, optional :: difflevel_in
    ! Locals
    integer :: i, difflevel
    real :: k0(0:2), k1(0:2), k2(0:2), k3(0:2)
    real :: exparg(0:2,0:2)
    if (present(difflevel_in)) then
       difflevel = difflevel_in
    else
       if (present(g_ee) .or. present(g_xx) .or. present(g_ex)) then
          difflevel = 2
       else if (present(g_e) .or. present(g_x)) then
          difflevel = 1
       else
          difflevel = 0
       endif
    endif
    call k_rdf_at_contact_zeroth_order(eta,k0,k1,k2,k3,difflevel)
    do i=0,difflevel
       exparg(i,0) = k0(i) + k1(i)*x0 + k2(i)*x0**2 + k3(i)*x0**3
    enddo
    if (difflevel > 0) then
       exparg(0,1) = k1(0) + 2.0*k2(0)*x0 + 3.0*k3(0)*x0**2
    endif
    if (difflevel > 1) then
       exparg(0,2) = 2.0*k2(0) + 6.0*k3(0)*x0
       exparg(1,1) = k1(1) + 2.0*k2(1)*x0 + 3.0*k3(1)*x0**2
       !exparg(2,2) = 2.0*k2(2) + 6.0*k3(2)*x0
    endif
    g = exp(exparg(0,0))
    if (present(g_e)) then
       g_e = g*exparg(1,0)
    endif
    if (present(g_ee)) then
       g_ee = g*(exparg(1,0)**2 + exparg(2,0))
    endif
    if (present(g_x)) then
       g_x = g*exparg(0,1)
    endif
    if (present(g_xx)) then
       g_xx = g*(exparg(0,1)**2 + exparg(0,2))
    endif
    if (present(g_ex)) then
       g_ex = g*(exparg(1,0)*exparg(0,1) + exparg(1,1))
    endif
  end subroutine rdf_at_contact_zeroth_order

  !> Calculate k vlaues for the radial-distribution-function at contact zeroth order expansion
  !!
  !! \author Morten Hammer, February 2018
  subroutine k_rdf_at_contact_zeroth_order(eta,k0,k1,k2,k3,difflevel)
    ! Input
    integer, intent(in) :: difflevel
    real, intent(in) :: eta
    ! Output
    real, intent(out) :: k0(0:2), k1(0:2), k2(0:2), k3(0:2)
    ! Locals
    real :: one_min_eta(2:5)
    one_min_eta(2) = (1.0-eta)**2
    one_min_eta(3) = (1.0-eta)*one_min_eta(2)
    one_min_eta(4) = (1.0-eta)*one_min_eta(3)
    one_min_eta(5) = (1.0-eta)*one_min_eta(4)
    k0(0) = - log(1.0-eta) + (42.0*eta - 39.0*eta**2 + 9.0*eta**3 - 2.0*eta**4)/&
         (6.0*one_min_eta(3))
    k1(0) = (eta**4 + 6.0*eta**2 - 12.0*eta)/(2.0*one_min_eta(3))
    k2(0) = (-3.0*eta**2)/(8.0*one_min_eta(2))
    k3(0) = (-eta**4 + 3.0*eta**2 +  3.0*eta)/(6.0*one_min_eta(3))
    if (difflevel > 0) then
       k0(1) =  (eta**4 - 7.0*eta**3 + 3.0*eta**2 - 6.0*eta + 24.0)/(3.0*one_min_eta(4))
       k1(1) = (-12.0 - 12.0*eta + 6.0*eta**2 + 4.0*eta**3 - eta**4)/(2.0*one_min_eta(4))
       k2(1) = (-3.0*eta)/(4.0*one_min_eta(3))
       k3(1) = (3.0 + 12.0*eta + 3.0*eta**2 - 4.0*eta**3 + eta**4)/(6.0*one_min_eta(4))
    endif
    if (difflevel > 1) then
       k0(2) = - (eta**3 + 5.0*eta**2 + 4.0*eta - 30.0)/one_min_eta(5)
       k1(2) = -6.0*(5.0 + 2.0*eta - 2.0*eta**2)/one_min_eta(5)
       k2(2) = -3.0*(2.0*eta + 1.0)/(4.0*one_min_eta(4))
       k3(2) = -(-4.0 - 7.0*eta + eta**2)/one_min_eta(5)
    endif
  end subroutine k_rdf_at_contact_zeroth_order

  !> Calculate radial-distribution-function at contact, first order expansion
  !! calculate from TVn
  !!
  !! \author Morten Hammer, February 2018
  subroutine rdf_at_contact_first_order_TVn(nc,T,V,n,s_vc,&
       g,g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, dimension(nc), intent(out) :: g
    real, optional, dimension(nc), intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc,nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc,nc), intent(out) :: g_nn
    ! Locals
    real :: g12, g12_T,g12_V,g12_TT,g12_VV,g12_TV
    real, dimension(nc) :: g12_n,g12_Tn,g12_Vn
    real, dimension(nc,nc) :: g12_nn
    integer :: i !< Component index
    if (enable_A1) then
       do i=1,nc
          if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
               present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
             call calcG12(nc,T,V,n,i,s_vc,g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,g12_Tn,g12_Vn,g12_nn)
             call calcG11_if(nc,T,V,n,i,s_vc,g(i),g_T(i),g_V(i),g_n(:,i),g_TT(i),g_VV(i),g_TV(i),&
                  g_Tn(:,i),g_Vn(:,i),g_nn(:,:,i))
          else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
             call calcG12(nc,T,V,n,i,s_vc,g12,g12_T,g12_V,g12_n)
             call calcG11_if(nc,T,V,n,i,s_vc,g(i),g_T(i),g_V(i),g_n(:,i))
          else
             call calcG12(nc,T,V,n,i,s_vc,g12)
             call calcG11_if(nc,T,V,n,i,s_vc,g(i))
          endif
          ! Add g11 and g12
          g(i) = g(i) + g12
          if (present(g_TT)) then
             g_TT(i) = g_TT(i) + g12_TT
          endif
          if (present(g_VV)) then
             g_VV(i) = g_VV(i) + g12_VV
          endif
          if (present(g_TV)) then
             g_TV(i) = g_TV(i) + g12_TV
          endif
          if (present(g_Tn)) then
             g_Tn(:,i) = g_Tn(:,i) + g12_Tn
          endif
          if (present(g_Vn)) then
             g_Vn(:,i) = g_Vn(:,i) + g12_Vn
          endif
          if (present(g_nn)) then
             g_nn(:,:,i) = g_nn(:,:,i) + g12_nn(:,:)
          endif
          if (present(g_T)) then
             g_T(i) = g_T(i) + g12_T
          endif
          if (present(g_V)) then
             g_V(i) = g_V(i) + g12_V
          endif
          if (present(g_n)) then
             g_n(:,i) = g_n(:,i) + g12_n
          endif
       enddo
    else ! Disable g1
       g = 0
       g_TT = 0
       g_VV = 0
       g_TV = 0
       g_Tn = 0
       g_Vn = 0
       g_nn = 0
       g_T = 0
       g_V = 0
       g_n = 0
    endif
  end subroutine rdf_at_contact_first_order_TVn

  !> Calculate part of first order RDF perturbation (g12)
  !!
  !! \author Morten Hammer, March 2019
  subroutine calcG12(nc,T,V,n,i,s_vc,g,g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(out) :: g_nn
    ! Locals
    real :: g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,x0_ii,e,x0_ii_T,x0_ii_TT
    type(saftvrmie_zeta) :: x0z
    integer :: difflevel

    if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
         present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
       difflevel = 2
    else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
       difflevel = 1
    else
       difflevel = 0
    endif

    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,saftvrmie_param%sigma_ij(i,i),0.0,0.0,s_vc%d_pure,x0z)
       x0_ii = x0z%zx
    else
       call calcXDifferentials(saftvrmie_param%sigma_ij(i,i),0.0,0.0,&
            s_vc%dhs%d(i,i),s_vc%dhs%d_T(i,i),s_vc%dhs%d_TT(i,i),x0_ii,x0_ii_T,x0_ii_TT)
    endif
    e = s_vc%zeta%zx
    call calcG12_ex(x0_ii,e,saftvrmie_param%lambda_a_ij(i,i),&
         saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
         saftvrmie_param%Cij(i,i),g,g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex)
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
            g,g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,0.0,0.0,0.0,0.0,&
            g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,&
            difflevel=difflevel)
       call cleanup_saftvrmie_zeta(x0z)
    else
       call convert_zeta_x_to_TVn(nc,x0_ii,x0_ii_T,x0_ii_TT,s_vc%zeta,&
            g,g_ii_e,g_ii_x,g_ii_ee,g_ii_xx,g_ii_ex,0.0,0.0,0.0,&
            g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,difflevel=difflevel)
    endif

  end subroutine calcG12

  !> Calculate part of first order RDF perturbation (g12)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcG12_ex(x0,eta,lambda_a,lambda_r,eps,C,&
       g1,g1_e,g1_x,g1_ee,g1_xx,g1_ex)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    ! Output
    real, intent(out) :: g1,g1_e,g1_x,g1_ee,g1_xx,g1_ex
    ! Locals
    real :: Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx
    real :: Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx
    real :: asr,asr_e,asr_ee,asr_eee
    real :: asa,asa_e,asa_ee,asa_eee
    real :: prefactor
    call calcA1Sutherland(eta,lambda_r,eps,asr,asr_e,asr_ee,asr_eee)
    call calcA1Sutherland(eta,lambda_a,eps,asa,asa_e,asa_ee,asa_eee)
    if (x0 > 1) then
       call calcBtilde(x0,eta,lambda_r,eps,Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx)
       call calcBtilde(x0,eta,lambda_a,eps,Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx)
    else
       Br = 0.0
       Br_e = 0.0
       Br_x = 0.0
       Br_ee = 0.0
       Br_xx = 0.0
       Br_ex = 0.0
       Br_eee = 0.0
       Br_eex = 0.0
       Br_exx = 0.0
       Ba = 0.0
       Ba_e = 0.0
       Ba_x = 0.0
       Ba_ee = 0.0
       Ba_xx = 0.0
       Ba_ex = 0.0
       Ba_eee = 0.0
       Ba_eex = 0.0
       Ba_exx = 0.0
    endif
    prefactor = C/(12.0*eps)
    g1 = prefactor*(-lambda_a*x0**(lambda_a)*(asa+Ba) + lambda_r*x0**(lambda_r)*(asr+Br))
    g1_x = prefactor*(-lambda_a**2*x0**(lambda_a-1.0)*(asa+Ba) &
         + lambda_r**2*x0**(lambda_r-1.0)*(asr+Br) &
         - lambda_a*x0**lambda_a*Ba_x + lambda_r*x0**lambda_r*Br_x)
    g1_xx = prefactor*(-(lambda_a-1.0)*lambda_a**2*x0**(lambda_a-2.0)*(asa+Ba)&
         + (lambda_r-1.0)*lambda_r**2*x0**(lambda_r-2.0)*(asr+Br) &
         - 2.0*lambda_a**2*x0**(lambda_a-1.0)*Ba_x + 2.0*lambda_r**2*x0**(lambda_r-1.0)*Br_x &
         -lambda_a*x0**lambda_a*Ba_xx + lambda_r*x0**lambda_r*Br_xx)
    g1_e = prefactor*(-lambda_a*x0**lambda_a*(asa_e+Ba_e) + lambda_r*x0**lambda_r*(asr_e+Br_e))
    g1_ee = prefactor*(-lambda_a*x0**lambda_a*(asa_ee+Ba_ee) &
         + lambda_r*x0**lambda_r*(asr_ee+Br_ee))
    g1_ex = prefactor*(-lambda_a**2*x0**(lambda_a-1.0)*(asa_e+Ba_e) &
         + lambda_r**2*x0**(lambda_r-1.0)*(asr_e+Br_e) &
         - lambda_a*x0**lambda_a*Ba_ex + lambda_r*x0**lambda_r*Br_ex)
  end subroutine calcG12_ex

  !> Calculate g11 form TVn
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcG11_if(nc,T,V,n,i,saftvrmie_vc,g,g_T,g_V,g_n,g_TT,&
       g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(out) :: g_nn
    ! Locals
    real :: eps, K, d, d_T, d_TT
    type(saftvrmie_zeta) :: pf

    eps = saftvrmie_param%eps_divk_ij(i,i)
    K = -3.0/(2.0*eps*pi*N_AVOGADRO)
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call set_gx_to_Kax_v(nc,i,K,saftvrmie_vc%a1ij,g,g_T,g_V,g_n,g_TT,&
            g_VV,g_TV,g_Tn,g_Vn,g_nn)
       call allocate_saftvrmie_zeta(nc,pf)
       call calcGX1_prefac_pure_ref(nc,T,V,n,saftvrmie_vc%d_pure,pf)
       call calc_a_zeta_product(nc,pf,g,g_T,g_V,g_n,&
            g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
       call cleanup_saftvrmie_zeta(pf)
    else
       d = saftvrmie_vc%dhs%d(i,i)
       d_T = saftvrmie_vc%dhs%d_T(i,i)
       d_TT = saftvrmie_vc%dhs%d_TT(i,i)
       call calcGX1(nc,T,V,n,i,K,d,d_T,d_TT,saftvrmie_vc%a1ij,g,g_T,&
            g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
    endif
  end subroutine calcG11_if

  !> Calculate radial-distribution-function at contact, second order expansion
  !! calculate from TVn
  !!
  !! \author Morten Hammer, February 2018
  subroutine rdf_at_contact_second_order_TVn(nc,T,V,n,saftvrmie_vc,no_correction,&
       g,g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    logical, intent(in) :: no_correction !< Disregard correction
    ! Output
    real, dimension(nc), intent(out) :: g
    real, optional, dimension(nc), intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc,nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc,nc), intent(out) :: g_nn
    ! Locals
    real :: g22,g22_T,g22_V,g22_TT,g22_VV,g22_TV
    real, dimension(nc) :: g22_n,g22_Tn,g22_Vn
    real, dimension(nc,nc) :: g22_nn
    integer :: i
    real :: gam,gam_T,gam_V,gam_TT,gam_VV,gam_TV
    real, dimension(nc) :: gam_n,gam_Tn,gam_Vn
    real, dimension(nc,nc) :: gam_nn

    if (enable_A2) then
       do i=1,nc
          if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
               present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
             call calcG22MCA_TVn(nc,T,V,n,i,saftvrmie_vc,&
                  g22,g22_T,g22_V,g22_n,g22_TT,g22_VV,g22_TV,g22_Tn,g22_Vn,g22_nn)
             call calcG21_if(nc,T,V,n,i,saftvrmie_vc,&
                  g(i),g_T(i),g_V(i),g_n(:,i),g_TT(i),g_VV(i),g_TV(i),&
                  g_Tn(:,i),g_Vn(:,i),g_nn(:,:,i))
          else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
             call calcG22MCA_TVn(nc,T,V,n,i,saftvrmie_vc,g22,g22_T,g22_V,g22_n)
             call calcG21_if(nc,T,V,n,i,saftvrmie_vc,g(i),g_T(i),g_V(i),g_n(:,i))
          else
             call calcG22MCA_TVn(nc,T,V,n,i,saftvrmie_vc,g22)
             call calcG21_if(nc,T,V,n,i,saftvrmie_vc,g(i))
           endif
          ! Add g21 and g22
          g(i) = g(i) + g22
          if (present(g_TT)) then
             g_TT(i) = g_TT(i) + g22_TT
          endif
          if (present(g_VV)) then
             g_VV(i) = g_VV(i) + g22_VV
          endif
          if (present(g_TV)) then
             g_TV(i) = g_TV(i) + g22_TV
          endif
          if (present(g_Tn)) then
             g_Tn(:,i) = g_Tn(:,i) + g22_Tn
          endif
          if (present(g_Vn)) then
             g_Vn(:,i) = g_Vn(:,i) + g22_Vn
          endif
          if (present(g_nn)) then
             g_nn(:,:,i) = g_nn(:,:,i) + g22_nn(:,:)
          endif
          if (present(g_T)) then
             g_T(i) = g_T(i) + g22_T
          endif
          if (present(g_V)) then
             g_V(i) = g_V(i) + g22_V
          endif
          if (present(g_n)) then
             g_n(:,i) = g_n(:,i) + g22_n
          endif

          ! Calculate g2 = (1+gammaC)*g2MCA
          if (.not. no_correction) then
             call calcGammaC_TVn(nc,T,V,n,i,saftvrmie_vc%zeta_bar,&
                  gam,gam_T,gam_V,gam_n,&
                  gam_TT,gam_VV,gam_TV,gam_Tn,gam_Vn,gam_nn)
             gam = 1.0 + gam
             ! Combine
             if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
                  present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
                call calc_a0_a_product(nc,gam,g(i),&
                     a0_T=gam_T,a0_V=gam_V,a0_n=gam_n,&
                     a_T=g_T(i),a_V=g_V(i),a_n=g_n(:,i),&
                     a0_TT=gam_TT,a0_VV=gam_VV,a0_TV=gam_TV,&
                     a0_Tn=gam_Tn,a0_Vn=gam_Vn,a0_nn=gam_nn,&
                     a_TT=g_TT(i),a_VV=g_VV(i),a_TV=g_TV(i),&
                     a_Tn=g_Tn(:,i),a_Vn=g_Vn(:,i),a_nn=g_nn(:,:,i))
             else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
                call calc_a0_a_product(nc,gam,g(i),&
                     a0_T=gam_T,a0_V=gam_V,a0_n=gam_n,&
                     a_T=g_T(i),a_V=g_V(i),a_n=g_n(:,i))
             else
                g(i) = g(i)*gam
             endif
          endif
       enddo
    else ! Disable g2
       g = 0
       g_TT = 0
       g_VV = 0
       g_TV = 0
       g_Tn = 0
       g_Vn = 0
       g_nn = 0
       g_T = 0
       g_V = 0
       g_n = 0
    endif
  end subroutine rdf_at_contact_second_order_TVn

  !> Calculate part of second order RDF perturbation (g22)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcG22MCA_TVn(nc,T,V,n,i,saftvrmie_vc,&
       g,g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(out) :: g_nn
    ! Locals
    real :: x0,x0_T,x0_TT !< Reduced center-center hard sphere distance
    real :: eta !< Packing fraction
    real :: lambda_a !< Mie potential attractive exponent
    real :: lambda_r !< Mie potential repulsive exponent
    real :: eps !< Well depth div. temperature (K)
    real :: C !< Mie potential prefactor
    real :: d,d_T,d_TT
    real :: s,s_T,s_TT
    real :: g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex
    type(saftvrmie_zeta) :: x0z
    integer :: difflevel

    if ( present(g_TT) .or. present(g_VV) .or. present(g_TV) .or. &
         present(g_Tn) .or. present(g_Vn) .or. present(g_nn)) then
       difflevel = 2
    else if (present(g_T) .or. present(g_V) .or. present(g_n)) then
       difflevel = 1
    else
       difflevel = 0
    endif

    lambda_a = saftvrmie_param%lambda_a_ij(i,i)
    lambda_r = saftvrmie_param%lambda_r_ij(i,i)
    eps = saftvrmie_param%eps_divk_ij(i,i)
    C = saftvrmie_param%Cij(i,i)
    eta = saftvrmie_vc%zeta%zx

    s = saftvrmie_param%sigma_ij(i,i) !saftvrmie_vc%sigma_eff%d(i,i)
    s_T = 0.0 !saftvrmie_vc%sigma_eff%d_T(i,i)
    s_TT = 0.0 !saftvrmie_vc%sigma_eff%d_TT(i,i)
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,saftvrmie_vc%d_pure,x0z)
       x0 = x0z%zx
    else
       d = saftvrmie_vc%dhs%d(i,i)
       d_T = saftvrmie_vc%dhs%d_T(i,i)
       d_TT = saftvrmie_vc%dhs%d_TT(i,i)
       call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    endif
    call calcG22MCA(x0,eta,lambda_a,lambda_r,eps,C,&
         g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
    g = g2
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call convert_zeta_zeta_to_TVn(nc,x0z,saftvrmie_vc%zeta,&
            g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex,0.0,0.0,0.0,0.0,&
            g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,&
            difflevel=difflevel)
       call cleanup_saftvrmie_zeta(x0z)
    else
       call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,&
            saftvrmie_vc%zeta,&
            g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex,0.0,0.0,0.0,&
            g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,difflevel=difflevel)
    endif
    ! Multiply g with Khs
    call calc_a_zeta_product(nc,saftvrmie_vc%Khs,g,g_T,g_V,g_n,&
         g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)

  end subroutine calcG22MCA_TVn

  !> Calculate part of second order RDF perturbation (g22)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcG22MCA(x0,eta,lambda_a,lambda_r,eps,C,&
       g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    ! Output
    real, intent(out) :: g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex
    ! Locals
    real :: Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx
    real :: Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx
    real :: Bar,Bar_e,Bar_x,Bar_ee,Bar_xx,Bar_ex,Bar_eee,Bar_eex,Bar_exx
    real :: asr,asr_e,asr_ee,asr_eee
    real :: asa,asa_e,asa_ee,asa_eee
    real :: asar,asar_e,asar_ee,asar_eee
    real :: prefactor,lambda_2a,lambda_2r,lambda_ar
    lambda_2a = 2.0*lambda_a
    lambda_2r = 2.0*lambda_r
    lambda_ar = lambda_a + lambda_r
    call calcA1Sutherland(eta,lambda_2r,eps,asr,asr_e,asr_ee,asr_eee)
    call calcA1Sutherland(eta,lambda_2a,eps,asa,asa_e,asa_ee,asa_eee)
    call calcA1Sutherland(eta,lambda_ar,eps,asar,asar_e,asar_ee,asar_eee)
    if (x0 > 1) then
       call calcBtilde(x0,eta,lambda_2r,eps,Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx)
       call calcBtilde(x0,eta,lambda_2a,eps,Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx)
       call calcBtilde(x0,eta,lambda_ar,eps,Bar,Bar_e,Bar_x,&
            Bar_ee,Bar_xx,Bar_ex,Bar_eee,Bar_eex,Bar_exx)
    else
       Br = 0.0
       Br_e = 0.0
       Br_x = 0.0
       Br_ee = 0.0
       Br_xx = 0.0
       Br_ex = 0.0
       Br_eee = 0.0
       Br_eex = 0.0
       Br_exx = 0.0
       Ba = 0.0
       Ba_e = 0.0
       Ba_x = 0.0
       Ba_ee = 0.0
       Ba_xx = 0.0
       Ba_ex = 0.0
       Ba_eee = 0.0
       Ba_eex = 0.0
       Ba_exx = 0.0
       Bar = 0.0
       Bar_e = 0.0
       Bar_x = 0.0
       Bar_ee = 0.0
       Bar_xx = 0.0
       Bar_ex = 0.0
       Bar_eee = 0.0
       Bar_eex = 0.0
       Bar_exx = 0.0
    endif
    prefactor = C**2/(12.0*eps)
    g2 = prefactor*(-lambda_a*x0**(lambda_2a)*(asa+Ba)&
         + lambda_ar*x0**(lambda_ar)*(asar+Bar)&
         - lambda_r*x0**(lambda_2r)*(asr+Br))
    g2_x = prefactor*(-lambda_2a*lambda_a*x0**(lambda_2a-1.0)*(asa+Ba) &
         + lambda_ar**2*x0**(lambda_ar-1.0)*(asar+Bar) &
         - lambda_r*lambda_2r*x0**(lambda_2r-1.0)*(asr+Br) &
         - lambda_a*x0**lambda_2a*Ba_x &
         + lambda_ar*x0**(lambda_ar)*Bar_x &
         - lambda_r*x0**lambda_2r*Br_x)
    g2_xx = prefactor*(-(lambda_2a-1.0)*lambda_2a*lambda_a*x0**(lambda_2a-2.0)*(asa+Ba) &
         + (lambda_ar-1.0)*lambda_ar**2*x0**(lambda_ar-2.0)*(asar+Bar) &
         - (lambda_2r-1.0)*lambda_r*lambda_2r*x0**(lambda_2r-2.0)*(asr+Br) &
         - 2.0*lambda_2a*lambda_a*x0**(lambda_2a-1.0)*Ba_x &
         + 2.0*lambda_ar**2*x0**(lambda_ar-1.0)*Bar_x &
         - 2.0*lambda_r*lambda_2r*x0**(lambda_2r-1.0)*Br_x &
         - lambda_a*x0**lambda_2a*Ba_xx &
         + lambda_ar*x0**(lambda_ar)*Bar_xx &
         - lambda_r*x0**lambda_2r*Br_xx)
    g2_e = prefactor*(-lambda_a*x0**lambda_2a*(asa_e+Ba_e) &
         + lambda_ar*x0**(lambda_ar)*(asar_e+Bar_e)&
         - lambda_r*x0**lambda_2r*(asr_e+Br_e))
    g2_ee = prefactor*(-lambda_a*x0**lambda_2a*(asa_ee+Ba_ee) &
         + lambda_ar*x0**(lambda_ar)*(asar_ee+Bar_ee)&
         - lambda_r*x0**lambda_2r*(asr_ee+Br_ee))
    g2_ex = prefactor*(-lambda_a*lambda_2a*x0**(lambda_2a-1.0)*(asa_e+Ba_e) &
         + lambda_ar**2*x0**(lambda_ar-1.0)*(asar_e+Bar_e) &
         - lambda_r*lambda_2r*x0**(lambda_2r-1.0)*(asr_e+Br_e) &
         - lambda_a*x0**lambda_2a*Ba_ex &
         + lambda_ar*x0**lambda_ar*Bar_ex &
         - lambda_r*x0**lambda_2r*Br_ex)
  end subroutine calcG22MCA

  !> Calculate g21 form TVn
  !!
  !! \author Morten Hammer, March 2019
  subroutine calcG21_if(nc,T,V,n,i,saftvrmie_vc,g,g_T,g_V,g_n,g_TT,&
       g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(out) :: g_nn
    ! Locals
    real :: eps, K, d, d_T, d_TT
    type(saftvrmie_zeta) :: pf

    eps = saftvrmie_param%eps_divk_ij(i,i)
    K = -3.0/(2.0*eps**2*pi*N_AVOGADRO)
    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call set_gx_to_Kax_v(nc,i,K,saftvrmie_vc%a2chij,g,g_T,g_V,g_n,g_TT,&
            g_VV,g_TV,g_Tn,g_Vn,g_nn)
       call allocate_saftvrmie_zeta(nc,pf)
       call calcGX1_prefac_pure_ref(nc,T,V,n,saftvrmie_vc%d_pure,pf)
       call calc_a_zeta_product(nc,pf,g,g_T,g_V,g_n,&
            g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
       call cleanup_saftvrmie_zeta(pf)
    else
       d = saftvrmie_vc%dhs%d(i,i)
       d_T = saftvrmie_vc%dhs%d_T(i,i)
       d_TT = saftvrmie_vc%dhs%d_TT(i,i)
       call calcGX1(nc,T,V,n,i,K,d,d_T,d_TT,saftvrmie_vc%a2chij,g,g_T,&
            g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn)
    endif
  end subroutine calcG21_if

  !> Set gx to ax_V
  !!
  !! \author Morten Hammer, March 2019
  subroutine set_gx_to_Kax_v(nc,i,K,ax,g,g_T,g_V,g_n,g_TT,&
       g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    integer, intent(in) :: i !< Component index
    real, intent(in) :: K !< Constant
    type(saftvrmie_aij), intent(in) :: ax
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(out) :: g_nn
    ! Locals
    integer :: j,l
    g = K*ax%am_V(i,i)
    if (present(g_V)) then
       g_V = K*ax%am_VV(i,i)
    endif
    if (present(g_VV)) then
       g_VV = K*ax%am_VVV(i,i)
    endif
    if (present(g_T)) then
       g_T = K*ax%am_TV(i,i)
    endif
    if (present(g_TT)) then
       g_TT = K*ax%am_VTT(i,i)
    endif
    if (present(g_TV)) then
       g_TV = K*ax%am_VVT(i,i)
    endif
    if (present(g_n)) then
       g_n = K*ax%am_Vn(:,i,i)
    endif
    if (present(g_Vn)) then
       g_Vn = K*ax%am_VVn(:,i,i)
    endif
    if (present(g_Tn)) then
       g_Tn = K*ax%am_VTn(:,i,i)
    endif
    if (present(g_nn)) then
       do j=1,nc
          do l=1,nc
             g_nn(j,l) = K*ax%am_Vnn(j,l,i,i)
          enddo
       enddo
    endif
  end subroutine set_gx_to_Kax_v

  !> Calculate gX1 form TVn
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcGX1(nc,T,V,n,i,K,d,d_T,d_TT,ax,g,g_T,g_V,g_n,g_TT,&
       g_VV,g_TV,g_Tn,g_Vn,g_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_aij), intent(in) :: ax
    real, intent(in) :: K, d, d_T, d_TT
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_T,g_V,g_TT,g_VV,g_TV
    real, optional, dimension(nc), intent(out) :: g_n,g_Tn,g_Vn
    real, optional, dimension(nc,nc), intent(out) :: g_nn
    ! Locals
    real :: ns, prefactor
    integer :: j,l

    ns = sum(saftvrmie_param%ms*n)
    prefactor = K*V**2/(ns*d**3)
    g = prefactor*ax%am_V(i,i)
    if (present(g_V)) then
       g_V = prefactor*(2.0*ax%am_V(i,i)/V + ax%am_VV(i,i))
    endif
    if (present(g_VV)) then
       g_VV = prefactor*(2.0*ax%am_V(i,i)/V**2 &
            + 4.0*ax%am_VV(i,i)/V + ax%am_VVV(i,i))
    endif
    if (present(g_T)) then
       g_T = prefactor*ax%am_TV(i,i) - 3.0*g*d_T/d
    endif
    if (present(g_TT)) then
       g_TT = prefactor*ax%am_VTT(i,i) - 6.0*g*d_T**2/d**2 &
            - 3.0*g*d_TT/d - 6.0*d_T*g_T/d
    endif
    if (present(g_TV)) then
       g_TV = prefactor*(2.0*ax%am_TV(i,i)/V &
            + ax%am_VVT(i,i)) - 3.0*d_T*g_V/d
    endif
    if (present(g_n)) then
       g_n = prefactor*ax%am_Vn(:,i,i) -g*saftvrmie_param%ms/ns
    endif
    if (present(g_Vn)) then
       g_Vn = prefactor*(2.0*ax%am_Vn(:,i,i)/V &
            + ax%am_VVn(:,i,i)) - g_V*saftvrmie_param%ms/ns
    endif
    if (present(g_Tn)) then
       g_Tn = prefactor*ax%am_VTn(:,i,i) - g_T*saftvrmie_param%ms/ns &
            - 3.0*g*saftvrmie_param%ms*d_T/(ns*d) - 3.0*g_n*d_T/d
    endif
    if (present(g_nn)) then
       do j=1,nc
          do l=1,nc
             g_nn(j,l) = prefactor*ax%am_Vnn(j,l,i,i) &
                  - g_n(l)*saftvrmie_param%ms(j)/ns &
                  - g_n(j)*saftvrmie_param%ms(l)/ns
          enddo
       enddo
    endif
  end subroutine calcGX1

  !> Calculate gX1 form TVn
  !!
  !! \author Morten Hammer, March 2019
  subroutine calcGX1_prefac_pure_ref(nc,T,V,n,d_pure,pf)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_zeta), intent(in) :: d_pure
    ! Output
    type(saftvrmie_zeta), intent(inout) :: pf
    ! Locals
    real :: ns
    integer :: k,l
    ns = sum(saftvrmie_param%ms*n)
    pf%zx = V**2/(ns*d_pure%zx**3)
    pf%zx_V = 2*pf%zx/V
    pf%zx_VV = pf%zx_V/V
    pf%zx_T = -3*pf%zx*d_pure%zx_T/d_pure%zx
    pf%zx_TT = 12*pf%zx*(d_pure%zx_T/d_pure%zx)**2 - 3*pf%zx*d_pure%zx_TT/d_pure%zx
    pf%zx_TV = -6*pf%zx*d_pure%zx_T/(d_pure%zx*V)
    pf%zx_n = -3*pf%zx*d_pure%zx_n/d_pure%zx - pf%zx*saftvrmie_param%ms/ns
    pf%zx_Vn = -6*pf%zx*d_pure%zx_n/(d_pure%zx*V) - 2*pf%zx*saftvrmie_param%ms/(ns*V)
    pf%zx_Tn = 3*pf%zx*d_pure%zx_T*saftvrmie_param%ms/(ns*d_pure%zx) &
         + 12*pf%zx*d_pure%zx_n*d_pure%zx_T/d_pure%zx**2 - 3*pf%zx*d_pure%zx_Tn/d_pure%zx
    do k=1,nc
       do l=1,nc
          pf%zx_nn(k,l) = 12*pf%zx*d_pure%zx_n(k)*d_pure%zx_n(l)/d_pure%zx**2 &
               + 3*pf%zx*saftvrmie_param%ms(l)*d_pure%zx_n(k)/(ns*d_pure%zx) &
               - 3*pf%zx*d_pure%zx_nn(k,l)/d_pure%zx &
               + 2*pf%zx*saftvrmie_param%ms(k)*saftvrmie_param%ms(l)/ns**2 &
               + 3*pf%zx*saftvrmie_param%ms(k)*d_pure%zx_n(l)/(ns*d_pure%zx)
       enddo
    enddo
    !
    pf%zx_VVV = 0.0
    pf%zx_VVT = 0.0
    pf%zx_VTn = 0.0
    pf%zx_VVn = 0.0
    pf%zx_Vnn = 0.0
    pf%zx_VTT = 0.0
  end subroutine calcGX1_prefac_pure_ref

  !> Calculate part of second order RDF perturbation (g22)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcGammaC_TVn(nc,T,V,n,i,zeta_bar,&
       gam,gam_T,gam_V,gam_n,gam_TT,gam_VV,gam_TV,gam_Tn,gam_Vn,gam_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i !< Component index
    type(saftvrmie_zeta), intent(in) :: zeta_bar
    ! Output
    real, intent(out) :: gam
    real, optional, intent(out) :: gam_T,gam_V,gam_TT,gam_VV,gam_TV
    real, optional, dimension(nc), intent(out) :: gam_n,gam_Tn,gam_Vn
    real, optional, dimension(nc,nc), intent(out) :: gam_nn
    ! Locals
    real :: zeta !< Packing fraction
    real :: alpha,theta,theta_T,theta_TT,expT
    real :: eps !< Well depth div. temperature (K)
    real :: gam_z, gam_zz, gam_x, gam_zx

    zeta = zeta_bar%zx
    eps = saftvrmie_param%eps_divk_ij(i,i)
    alpha = saftvrmie_param%alpha_ij(i,i)
    call calcGammaXDivTheta(zeta, alpha, gam, gam_z, gam_zz)
    !call combineZetaXBarDifferentials(nc,gam,gam_z,gam_zz,0.0,&
    !     zeta_bar,a_V=gam_V,a_n=gam_n,&
    !     a_VV=gam_VV,a_Vn=gam_Vn,a_nn=gam_nn)

    ! Multipy (gam/theta)*theta
    expT = exp(eps/T)
    theta = expT - 1.0
    theta_T = -expT*eps/T**2
    theta_TT = expT*(eps*eps/T**4 + 2.0*eps/T**3)
    gam_x = gam
    gam_zx = gam_z
    gam_z = gam_z*theta
    gam_zz = gam_zz*theta
    gam = gam*theta

    call convert_zeta_x_to_TVn(nc,theta,theta_T,theta_TT,zeta_bar,&
         gam,gam_z,gam_x,gam_zz,0.0,gam_zx,0.0,0.0,0.0,&
         a1_T=gam_T,a1_V=gam_V,a1_n=gam_n,a1_TT=gam_TT,a1_VV=gam_VV,a1_TV=gam_TV,&
         a1_Tn=gam_Tn,a1_Vn=gam_Vn,a1_nn=gam_nn)

    ! if (present(gam_T)) then
    !   gam_T = gam*theta_T
    ! endif
    ! if (present(gam_TT)) then
    !   gam_TT = gam*theta_TT
    ! endif
    ! if (present(gam_Tn)) then
    !   gam_Tn = gam_n*theta_T
    ! endif
    ! if (present(gam_TV)) then
    !   gam_TV = gam_V*theta_T
    ! endif
    ! gam = gam*theta
    ! if (present(gam_V)) then
    !   gam_V = gam_V*theta
    ! endif
    ! if (present(gam_VV)) then
    !   gam_VV = gam_VV*theta
    ! endif
    ! if (present(gam_Vn)) then
    !   gam_Vn = gam_Vn*theta
    ! endif
    ! if (present(gam_n)) then
    !   gam_n = gam_n*theta
    ! endif
    ! if (present(gam_nn)) then
    !   gam_nn = gam_nn*theta
    ! endif
  end subroutine calcGammaC_TVn

  !> Calculate GammaC/theta
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcGammaXDivTheta(zeta, alpha, gam, gam_z, gam_zz)
    ! Input
    real, intent(in) :: zeta
    real, intent(in) :: alpha
    ! Output
    real, intent(out) :: gam, gam_z, gam_zz
    ! Locals
    real, parameter :: phi(0:4) = (/10.0, 10.0, 0.57, -6.7, -8.0 /)
    real :: f,f_z,f_zz
    f = phi(3)*zeta + phi(4)*zeta**2
    f_z = phi(3) + 2.0*phi(4)*zeta
    f_zz = 2.0*phi(4)
    gam = phi(0)*(1.0 - tanh(phi(1)*(phi(2) - alpha)))*zeta*exp(f)
    gam_z = gam*(1.0/zeta + f_z)
    gam_zz = gam*(2.0*f_z/zeta + f_z**2 + f_zz)
  end subroutine calcGammaXDivTheta

end module saftvrmie_chain
