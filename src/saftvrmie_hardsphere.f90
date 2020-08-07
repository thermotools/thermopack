!--------------------------------------------------------------------------------
! Module, subroutines and functions for the hard-sphere part of SAFT-VRQ Mie
! Implemented by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!
! The module contains the following subroutines (S) and functions (F):
! -------------------------------------------------------------------------------
!            Name                                   Description
! -------------------------------------------------------------------------------
! (F)  1. calc_mie_prefactor       | Prefactor of Mie-potential.
! (S)  2. calc_hardsphere_diameter | Obtains the hard-sphere diameter and derivs.
! (S)  3. calc_quantumparameter_D  | Obtains the quantum D-parameter and derivs.
! (S)  4. calc_effective_sigma     | Obtain effective sigmae, U(sigma)=0 and derivs.
! (S)  5. sigmaeff_U               | Scaled potential, used by S3.
! (S)  6. sigmaeff_dUdz            | r-derivative of scaled potential. Used only by S3.
! (S)  8. calc_hardsphere_zeta_and_derivatives | Variable used in hard-sphere a.
! (S)  9. calc_hardsphere_dalpha_dzeta         | Derivs. used in hard-sphere a.
! (S) 10. calc_hardsphere_helmholtzenergy      | Hard-sphere Helmholtz and derivs.
! (S) 11. calc_hardsphere_rdf_and_U            | Pair correlation function and U
!---------------------------------------------------------------------------------
module saftvrmie_hardsphere
  use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_param,&
       mie_c_factor, saftvrmie_var, get_DFeynHibbsPower,&
       saftvrmie_zeta, saftvrmie_dhs, &
       saftvrmie_var_container, saftvrmie_zeta_hs
  use thermopack_constants, only: h_const,kB_const,N_AVOGADRO
  use numconstants, only: pi
  use thermopack_constants, only: verbose
  use quadratures
  use saftvrmie_options
  implicit none
  private
  save

  ! Quadratue information
  integer :: hs_diam_quadrature = GAUSS_KRONROD_21 ! Modify to improve accuarcy
  logical :: estimate_quadrature_error = .false. ! Modify to disable error print. Only acrive for K21 and K31
  logical :: only_integrate_active_hs_area = .true.

  public :: calc_hardsphere_diameter
  public :: calc_hardsphere_zeta_and_derivatives, calc_hardsphere_dalpha_dzeta
  public :: calc_hardsphere_helmholtzenergy
  public :: calc_binary_effective_sigma
  public :: calc_binary_effective_eps_divk
  public :: calc_hardsphere_rdf_and_U
  public :: calc_binary_Z_Santos
  public :: calc_hardsphere_extra_helmholtzenergy, calc_gij_boublik
  public :: calc_d_pure
  ! Exported for testing
  public :: calc_mie_potential_quantumcorrected, epseff_Ux, epseff_Uxx
  public :: calc_hardsphere_virial_Bijk, calc_Santos_eta
  public :: calc_ahs_div_nRT_Santos_part, calc_ahs_div_nRT_CSK, calc_ahs_div_nRT_CS
  public :: calc_pure_ahs_div_nRT, calc_F11_Santos
  public :: calc_hardsphere_virial_B2, calc_hardsphere_virial_B3
  public :: calc_Santos_F12_or_F22, calc_hardsphere_helmholtzenergy_santos
  public :: calc_hardsphere_helmholtzenergy_pure, calc_eta_dij
  public :: calc_hardsphere_mixture_gij
  public :: calc_hardsphere_dgij_dzeta
  public :: calc_hardsphere_extra_zeta_and_derivatives
  public :: hs_diam_quadrature, estimate_quadrature_error, only_integrate_active_hs_area
  public :: calc_hardsphere_helmholtzenergy_original

Contains

  subroutine calc_hardsphere_extra_helmholtzenergy(nc,T,V,n,s_vc,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    !------------------------------------------------------------------------
    !  2019-01-14 Oivind Wilhelmsen
    ! The reduced Helmholtz energy of the extra term in the perturbation theory
    ! that is present for mixtures. We have used the term derived by
    ! Leonard et al., " Perturbation Theory and Liquid Mixtures",
    ! Transactions of the Faraday Society, 66:2439-2452, 1969
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    type(saftvrmie_var_container), intent(inout) :: s_vc
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !<derivatives
    real :: g, g_T,g_V,g_n(nc)        !< rdf+ derivatives
    real :: g_VV,g_TV,g_Vn(nc)        !< rdf+derivatives
    real :: g_TT,g_Tn(nc),g_nn(nc,nc) !< rdf+derivatives
    logical :: storage_container_dhs  !< To store the dij flag
    type(saftvrmie_dhs) :: dhs !< Hard-sphere diameter and differentials
    type(saftvrmie_dhs) :: dhs_exact    !< Container for the exact dij's
    type(saftvrmie_zeta_hs) :: zeta               !< Container for zetas
    integer :: i, j, k
    real :: two_pi_div_V
    real :: prefactor
    real :: term1, term3

    ! Allocate variable containers to ease notation
    zeta=s_vc%zeta_hs
    dhs=s_vc%dhs                            ! Store diameters
    dhs_exact=s_vc%dhs                      ! Use allocated variables

    ! Obtain the zeta variables and its derivatives
    ! We will in the following map mu_ij into the first index of
    ! zeta and must therefore set all these contributions to zero
    call calc_hardsphere_extra_zeta_and_derivatives(nc,T,V,n,&
         dhs%d,dhs%d_T,dhs%d_TT,zeta%zet,zeta%zet_V,&
         zeta%zet_VV,zeta%zet_T,zeta%zet_TT,&
         zeta%zet_n,zeta%zet_Vn,zeta%zet_TV,zeta%zet_Tn)

    ! Initialize a
    a=0.0

    ! Initialize derivatives
    if (present(a_TT)) then
       a_TT=0.0
       g_TT=0.0
    endif
    if (present(a_T)) then
       a_T=0.0
       g_T=0.0
    endif
    if (present(a_V)) then
       a_V=0.0
       g_V=0.0
    endif
    if (present(a_VV)) then
       a_VV=0.0
       g_VV=0.0
    endif
    if (present(a_TV)) then
       a_TV=0.0
       g_TV=0.0
    endif
    if (present(a_Tn)) then
       a_Tn=0.0
       g_Tn=0.0
    endif
    if (present(a_Vn)) then
       a_Vn=0.0
       g_Vn=0.0
    endif
    if (present(a_n)) then
       a_n=0.0
       g_n=0.0
    endif
    if (present(a_nn)) then
       a_nn=0.0
       g_nn=0.0
    endif

    ! Compute the exact d_ij's according to BH-theory
    storage_container_dhs=exact_binary_dhs  ! Store logical
    exact_binary_dhs=.true.                 ! Reset logical

    ! Calculate exact hard-sphere diameters
    call calc_hardsphere_diameter(nc,T,s_vc,s_vc%sigma_eff%d,&
         s_vc%sigma_eff%d_T,s_vc%sigma_eff%d_TT,dhs_exact%d,&
         dhs_exact%d_T,dhs_exact%d_TT)

    exact_binary_dhs=storage_container_dhs  ! Reset logical

    ! Pre-calculate
    two_pi_div_V=2.0*pi/V

    if (nc>1) then
       ! We make the sum using appropriate symmetry properties
       ! of the variables, i.e. a_ij=a_ji. We have multiplied the
       ! contributions from the double sum with 2.0 to account for that.
       do j= 1,nc
          do i= 1,nc
             ! Compute the pair-correlation function and derivatives:
             call calc_hardsphere_mixture_gij(nc,T,V,n,i,j,dhs,zeta,g,g_T,g_V,g_n,&
                  g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)

             prefactor=two_pi_div_V*n(i)*n(j)
             term1=(dhs%d(i,j)**2)
             term3=(dhs%d(i,j)-dhs_exact%d(i,j))

             ! The reduced Helmholtz energy of the extra term A*beta
             a=a-prefactor*term1*g*term3

             ! First order derivatives
             if (present(a_T)) then
                a_T=a_T-prefactor*(2.0*dhs%d(i,j)*dhs%d_T(i,j)*g*term3+ &
                     term1*g_T*term3+term1*g*(dhs%d_T(i,j)-dhs_exact%d_T(i,j)))
             endif
             if (present(a_V)) then
                a_V=a_V-prefactor*term1*g_V*term3
             endif
             if (present(a_n)) then
                a_n(j)=a_n(j)-2.0*two_pi_div_V*n(i)*term1*term3*g
                a_n=a_n-prefactor*term1*term3*g_n
             endif

             ! Second order derivatives
             if (present(a_Tn)) then
                a_Tn(j)=a_Tn(j)-2.0*two_pi_div_V*n(i)*(&
                     2.0*dhs%d(i,j)*dhs%d_T(i,j)*term3*g+ &
                     term1*(dhs%d_T(i,j)-dhs_exact%d_T(i,j))*g+&
                     term1*term3*(g_T))
                a_Tn=a_Tn-prefactor*(2.0*dhs%d(i,j)*dhs%d_T(i,j)*g_n*term3+&
                     term1*term3*g_Tn+term1*(dhs%d_T(i,j)-dhs_exact%d_T(i,j))*g_n)
             endif
             if (present(a_TT)) then
                a_TT=a_TT-prefactor*(2.0*g*term3*((dhs%d_T(i,j)**2)+dhs%d_TT(i,j)*dhs%d(i,j))+&
                     4.0*dhs%d(i,j)*dhs%d_T(i,j)*g_T*term3+(dhs%d(i,j)**2)*g_TT*term3+&
                     4.0*dhs%d(i,j)*dhs%d_T(i,j)*g*(dhs%d_T(i,j)-dhs_exact%d_T(i,j))+&
                     2.0*(dhs%d(i,j)**2)*g_T*(dhs%d_T(i,j)-dhs_exact%d_T(i,j))+&
                     (dhs%d(i,j)**2)*g*(dhs%d_TT(i,j)-dhs_exact%d_TT(i,j)))
             endif
             if (present(a_TV)) then
                a_TV=a_TV-prefactor*(2*dhs%d(i,j)*dhs%d_T(i,j)*g_V*term3+ &
                     term1*g_TV*term3+term1*g_V*(dhs%d_T(i,j)-dhs_exact%d_T(i,j)))
             endif
             if (present(a_VV)) then
                a_VV=a_VV+(2.0*pi/(V**2))*n(i)*n(j)*(term1*g_V*term3+term1*g*term3)&
                     -prefactor*term1*g_VV*term3
             endif
             if (present(a_Vn)) then
                a_Vn(j)=a_Vn(j)-2.0*two_pi_div_V*n(i)*term1*term3*g_V
                a_Vn=a_Vn-prefactor*term1*term3*g_Vn
             endif
             if (present(a_nn)) then
                ! NB: These two terms must come first!
                a_nn(i,j)=a_nn(i,j)-2.0*two_pi_div_V*term1*term3*g
                a_nn=a_nn-prefactor*term1*term3*g_nn

                ! Then the single-sum terms (This can probably be written more effectively to
                ! avoid the sum below. Did not pursue that avenue (OW)
                do k=1,nc
                   call calc_hardsphere_mixture_gij(nc,T,V,n,i,k,dhs,zeta,g,g_T,g_V,g_n,&
                        g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
                   a_nn(i,j)=a_nn(i,j)-2.0*two_pi_div_V*n(k)*(dhs%d(i,k)**2)*g_n(j)*(dhs%d(i,k)-dhs_exact%d(i,k))
                   call calc_hardsphere_mixture_gij(nc,T,V,n,j,k,dhs,zeta,g,g_T,g_V,g_n,&
                        g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
                   a_nn(i,j)=a_nn(i,j)-2.0*two_pi_div_V*n(k)*(dhs%d(j,k)**2)*g_n(i)*(dhs%d(j,k)-dhs_exact%d(j,k))
                end do

             endif
          end do
       end do

       ! Add additional terms for volume derivatives:
       ! NB: We here assume that if we need second order
       ! derivatives, then we already have the first order
       ! derivatives available.
       if (present(a_V)) then
          a_V=a_V-a/V
       endif
       if (present(a_TV)) then
          a_TV=a_TV-a_T/V
       endif
       if (present(a_VV)) then
          a_VV=a_VV-a_V/V+(a/(V**2))
       endif
       if (present(a_Vn)) then
          a_Vn=a_Vn-a_n/V
       endif
    endif

    ! Add the extra double-sum terms to the first order n-derivatives
    if (present(a_V)) then
       a_n=a_n
    endif
    ! Add the extra double-sum terms to the second order n-derivatives
    if (present(a_V)) then
       a_nn=a_nn
    endif

    ! Multiply with Avogadros number to have the right
    ! dimensions (a=A/RT)
    a=a*N_AVOGADRO

    ! Initialize derivatives
    if (present(a_TT)) then
       a_TT=a_TT*N_AVOGADRO
    endif
    if (present(a_T)) then
       a_T=a_T*N_AVOGADRO
    endif
    if (present(a_V)) then
       a_V=a_V*N_AVOGADRO
    endif
    if (present(a_VV)) then
       a_VV=a_VV*N_AVOGADRO
    endif
    if (present(a_TV)) then
       a_TV=a_TV*N_AVOGADRO
    endif
    if (present(a_Tn)) then
       a_Tn=a_Tn*N_AVOGADRO
    endif
    if (present(a_Vn)) then
       a_Vn=a_Vn*N_AVOGADRO
    endif
    if (present(a_n)) then
       a_n=a_n*N_AVOGADRO
    endif
    if (present(a_nn)) then
       a_nn=a_nn*N_AVOGADRO
    endif
  end subroutine calc_hardsphere_extra_helmholtzenergy

  subroutine calc_gij_boublik(nc,T,V,n,i,j,s_vc,g,g_T,g_V,g_n,&
       g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
    !------------------------------------------------------------------------
    ! Ailo Aasen, March 2019
    ! Boublik hardsphere rdf.
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    integer, intent(in) :: i, j
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(out) :: g         !< pair radial distribution function [-]
    real, intent(out), optional :: g_T,g_V,g_n(nc)           !< derivatives
    real, intent(out), optional :: g_VV,g_TV,g_Vn(nc)        !< derivatives
    real, intent(out), optional :: g_TT,g_Tn(nc),g_nn(nc,nc) !<derivatives
    logical :: storage_container_dhs  !< To store the dij flag
    type(saftvrmie_dhs) :: dhs !< Hard-sphere diameter and differentials
    type(saftvrmie_dhs) :: dhs_exact    !< Container for the exact dij's
    type(saftvrmie_zeta_hs) :: zeta               !< Container for zetas

    ! Allocate variable containers to ease notation
    zeta=s_vc%zeta_hs
    dhs=s_vc%dhs                            ! Store diameters
    dhs_exact=s_vc%dhs                      ! Use allocated variables

    ! Obtain the zeta variables and its derivatives
    ! We will in the following map mu_ij into the first index of
    ! zeta and must therefore set all these contributions to zero
    call calc_hardsphere_extra_zeta_and_derivatives(nc,T,V,n,&
         dhs%d,dhs%d_T,dhs%d_TT,zeta%zet,zeta%zet_V,&
         zeta%zet_VV,zeta%zet_T,zeta%zet_TT,&
         zeta%zet_n,zeta%zet_Vn,zeta%zet_TV,zeta%zet_Tn)

    ! Initialize derivatives
    if (present(g_TT)) then
       g_TT=0.0
    endif
    if (present(g_T)) then
       g_T=0.0
    endif
    if (present(g_V)) then
       g_V=0.0
    endif
    if (present(g_VV)) then
       g_VV=0.0
    endif
    if (present(g_TV)) then
       g_TV=0.0
    endif
    if (present(g_Tn)) then
       g_Tn=0.0
    endif
    if (present(g_Vn)) then
       g_Vn=0.0
    endif
    if (present(g_n)) then
       g_n=0.0
    endif
    if (present(g_nn)) then
       g_nn=0.0
    endif

    ! Compute the exact d_ij's according to BH-theory
    storage_container_dhs=exact_binary_dhs  ! Store logical
    exact_binary_dhs=.true.                 ! Reset logical

    ! Calculate exact hard-sphere diameters
    call calc_hardsphere_diameter(nc,T,s_vc,s_vc%sigma_eff%d,&
         s_vc%sigma_eff%d_T,s_vc%sigma_eff%d_TT,dhs_exact%d,&
         dhs_exact%d_T,dhs_exact%d_TT)

    exact_binary_dhs=storage_container_dhs  ! Reset logical

    ! Compute the pair-correlation function and derivatives:
    call calc_hardsphere_mixture_gij(nc,T,V,n,i,j,dhs,zeta,g,g_T,g_V,g_n,&
         g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
  end subroutine calc_gij_boublik


  subroutine calc_hardsphere_mixture_gij(nc,T,V,n,i,j,dhs,zeta, &
       g,g_T,g_V,g_n,g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
    !------------------------------------------------------------------------
    !  2019-01-10, Oivind Wilhelmsen
    ! The pair-correlation function of an additive mixture and derivatives.
    ! We have used the expression from Boublik, T. "Hard-Sphere Equation of State"
    ! J. Chem. Phys. 53, 471 (1970). All derivatives checked numerically.
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    integer, intent(in) :: i, j    ! The pair-correlation of the pair i,j
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    type(saftvrmie_zeta_hs), intent(inout) :: zeta      !< Zetas and derivatives
    real, intent(out) :: g         !< reduced helmholtz energy [-]
    real, intent(out), optional :: g_T,g_V,g_n(nc)           !< derivatives
    real, intent(out), optional :: g_TT,g_TV,g_Tn(nc)        !< derivatives
    real, intent(out), optional :: g_VV,g_Vn(nc),g_nn(nc,nc) !<derivatives
    real :: dg_dzeta(3)      !< First order deriv. of the rdf
    real :: d2g_dzeta2(3,3)  !< Second order deriv. of the rdf
    integer :: k, comp1, comp2, l
    real :: mu_up, mu_down, mu
    real :: mu_TT_up1, mu_TT_up2

    ! Compute the mus
    mu_up=dhs%d(i,i)*dhs%d(j,j)
    mu_down=dhs%d(i,i)+dhs%d(j,j)
    mu=mu_up/mu_down

    ! We map mu_ij and its derivatives into zeta%zet(2)
    zeta%zet(1)=mu

    ! First order T-derivative
    if (present(g_T)) then
       g_T=0.0

       ! We map dmu_ij/dT into zeta%zet_T(2)
       zeta%zet_T(1)=((dhs%d(i,i)**2)*dhs%d_T(j,j)+(dhs%d(j,j)**2)*dhs%d_T(i,i))/(mu_down**2)
    endif

    ! Second order T-derivative
    if (present(g_TT)) then
       g_TT=0.0

       ! We map d2mu_ij/dT2 into zeta%zet_TT(2)
       mu_TT_up1=2.0*(dhs%d(i,i)+dhs%d(j,j))*dhs%d_T(i,i)*dhs%d_T(j,j)+&
            ((dhs%d(i,i)**2)*dhs%d_TT(j,j)+(dhs%d(j,j)**2)*dhs%d_TT(i,i))
       mu_TT_up2=((dhs%d(i,i)**2)*dhs%d_T(j,j)+(dhs%d(j,j)**2)*dhs%d_T(i,i))*&
            (dhs%d_T(i,i)+dhs%d_T(j,j))
       zeta%zet_TT(1)=mu_TT_up1/(mu_down**2)-2.0*mu_TT_up2/(mu_down**3)
    endif

    ! Initialize derivatives
    if (present(g_V)) then
       g_V=0.0
    endif
    if (present(g_VV)) then
       g_VV=0.0
    endif
    if (present(g_TV)) then
       g_TV=0.0
    endif
    if (present(g_Tn)) then
       g_Tn=0.0
    endif
    if (present(g_Vn)) then
       g_Vn=0.0
    endif
    if (present(g_n)) then
       g_n=0.0
    endif
    if (present(g_nn)) then
       g_nn=0.0
    endif

    ! Obtain the reduced Helholtz energy and the derivatives with
    ! respect to the zeta-variables
    call calc_hardsphere_dgij_dzeta(zeta,g,dg_dzeta,d2g_dzeta2)

    do comp1= 1,nc
       do comp2= 1,nc
          do l = 1,3   ! Loop over mu, eta_2 and eta_3
             ! Start single-sum over l part of the derivatives
             if (comp2==1) then  ! Compute this only once for each "l"
                if (comp1==1) then  ! Compute this only once for each "l"
                   if (present(g_T)) then
                      g_T=g_T+dg_dzeta(l)*zeta%zet_T(l)
                   endif
                   if (present(g_V)) then
                      g_V=g_V+dg_dzeta(l)*zeta%zet_V(l)
                   endif
                   if (present(g_VV)) then
                      g_VV=g_VV+dg_dzeta(l)*zeta%zet_VV(l)
                   endif
                   if (present(g_TV)) then
                      g_TV=g_TV+dg_dzeta(l)*zeta%zet_TV(l)
                   endif
                   if (present(g_TT)) then
                      g_TT=g_TT+dg_dzeta(l)*zeta%zet_TT(l)
                   endif
                end if

                ! The composition first order derivative of g
                if (present(g_n)) then
                   g_n(comp1)=g_n(comp1)+dg_dzeta(l)*zeta%zet_n(comp1,l)
                endif
                if (present(g_Tn)) then
                   g_Tn(comp1)=g_Tn(comp1)+dg_dzeta(l)*zeta%zet_Tn(comp1,l)
                endif
                if (present(g_Vn)) then
                   g_Vn(comp1)=g_Vn(comp1)+dg_dzeta(l)*zeta%zet_Vn(comp1,l)
                endif
             end if

             ! End single sum over l part of the derivatives
             ! Start double sum over l and k part of the derivatives

             do k = 1,3
                if (comp2==1) then ! Compute this only once for each "l" and "k"
                   if (comp1==1) then ! Compute this only once for each "l" and "k"
                      if (present(g_VV)) then
                         g_VV=g_VV+d2g_dzeta2(l,k)*zeta%zet_V(l)*zeta%zet_V(k)
                      endif
                      if (present(g_TV)) then
                         g_TV=g_TV+d2g_dzeta2(l,k)*zeta%zet_T(l)*zeta%zet_V(k)
                      endif
                      if (present(g_TT)) then
                         g_TT=g_TT+d2g_dzeta2(l,k)*zeta%zet_T(l)*zeta%zet_T(k)
                      endif
                   end if

                   ! The composition first order derivative of a
                   if (present(g_Tn)) then
                      g_Tn(comp1)=g_Tn(comp1)+d2g_dzeta2(l,k)*&
                           zeta%zet_T(l)*zeta%zet_n(comp1,k)
                   endif
                   if (present(g_Vn)) then
                      g_Vn(comp1)=g_Vn(comp1)+d2g_dzeta2(l,k)*&
                           zeta%zet_V(l)*zeta%zet_n(comp1,k)
                   endif
                end if

                ! Second order mole number derivative
                if (present(g_nn)) then
                   g_nn(comp1,comp2)=g_nn(comp1,comp2)+d2g_dzeta2(l,k)*&
                        zeta%zet_n(comp1,l)*zeta%zet_n(comp2,k)
                endif
             end do
          end do
       end do
    end do
  end subroutine calc_hardsphere_mixture_gij

  subroutine calc_hardsphere_dgij_dzeta(zeta,g,dg_dzeta,&
       d2g_dzeta2)
    !---------------------------------------------------------------------
    !  2019-01-15, Oivind Wilhelmsen
    ! Obtain the derivatives of the pair correlation function expression with
    ! respect to mu (first index), zeta2 (second index) and zeta3
    ! (third index). All derivatives have been checked numerically.
    !---------------------------------------------------------------------
    type(saftvrmie_zeta_hs), intent(in) :: zeta            !< Zetas and derivatives
    real, intent(out) :: g                       !< rdf
    real, intent(out) :: dg_dzeta(3)             !< rdf 1-deriv
    real, intent(out) :: d2g_dzeta2(3,3)         !< rdf 2-deriv
    real :: mu, zeta_2, zeta_3                   !< variables
    real :: div_diff_1, div_diff_2, div_diff_3, div_diff_4
    real :: mu_2, zeta_2_2

    ! First allocate zero to all vectors
    dg_dzeta=0.0
    d2g_dzeta2=0.0

    ! Extracting variables from vector to enhance readability
    mu=zeta%zet(1)
    zeta_2=zeta%zet(2)
    zeta_3=zeta%zet(3)


    ! Prefactors that go into many of the expressions
    div_diff_1=1.0/(1-zeta_3)
    div_diff_2=div_diff_1**2
    div_diff_3=div_diff_1*div_diff_2
    div_diff_4=div_diff_2*div_diff_2
    mu_2=mu*mu
    zeta_2_2=zeta_2*zeta_2

    g=div_diff_1+3.0*zeta_2*div_diff_2*mu+2.0*(zeta_2_2)*mu_2*div_diff_3

    ! The first order derivatives
    dg_dzeta(1)=3.0*zeta_2*div_diff_2+4.0*zeta_2_2*div_diff_3*mu                 ! dg_dmu
    dg_dzeta(2)=3.0*mu*div_diff_2+4.0*div_diff_3*mu_2*zeta_2                     ! dg_zeta_2
    dg_dzeta(3)=div_diff_2+6.0*zeta_2*div_diff_3*mu+6.0*zeta_2_2*div_diff_4*mu_2 !dg_dzeta_3

    ! The second order derivatives [diagonal elements]
    d2g_dzeta2(1,1)=4.0*zeta_2_2*div_diff_3                                      ! d2g_dmu2
    d2g_dzeta2(2,2)=4.0*div_diff_3*mu_2                                          ! d2g_dzeta_2_2
    d2g_dzeta2(3,3)=2.0*div_diff_3+18.0*zeta_2*mu*div_diff_4+24.0*zeta_2_2*mu_2*div_diff_3*div_diff_2  ! d2g_dzeta_3_2

    ! The second order derivatives [off-diagonal components]
    d2g_dzeta2(1,2)=3.0*div_diff_2+8.0*zeta_2*mu*div_diff_3                      !d2g_dmudzeta2
    d2g_dzeta2(2,3)=6.0*div_diff_3*mu+12.0*zeta_2*div_diff_4*mu_2                !d2g_dzeta2zeta3
    d2g_dzeta2(1,3)=6.0*zeta_2*div_diff_3+12.0*zeta_2_2*div_diff_4*mu            !d2g_dmudzeta3

    ! Complete the matrix of second derivatives
    d2g_dzeta2(2,1)=d2g_dzeta2(1,2)
    d2g_dzeta2(3,2)=d2g_dzeta2(2,3)
    d2g_dzeta2(3,1)=d2g_dzeta2(1,3)

  end subroutine calc_hardsphere_dgij_dzeta

  function calc_mie_prefactor(mie_c_factor, epsilon) result(prefactor)
    !> Gives the full prefactor for the Mie potential
    implicit none
    real, intent(in) :: mie_c_factor !< the C in front of the Mie potential
    real, intent(in) :: epsilon      !< well depth
    real :: prefactor                !< prefactor for Mie potential

    prefactor = epsilon*mie_c_factor
  end function calc_mie_prefactor

  subroutine calc_hardsphere_diameter(nc,T,s_vc,sigma_eff,sigma_eff_T,sigma_eff_TT,&
       d_mat,d_T_mat,d_TT_mat)
    !--------------------------------------------------------------------
    !  2018-02-14, Oivind Wilhelmsen
    !  2018-10-01, Ailo Aasen
    !  2018-11,    Morten Hammer
    !
    !  Calulates the Barker--Henderson diameter of each binary pair, and its
    !  first and second order temperature derivatives.
    !  ---------------------------------------------------------------------
    integer, intent(in) :: nc               !< number of components
    real, intent(in) :: T                   !< temperature [K]
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(in) :: sigma_eff(nc,nc)    !< Effective sigma
    real, intent(in) :: sigma_eff_T(nc,nc)  !< Effective sigma temperature differetial
    real, intent(in) :: sigma_eff_TT(nc,nc) !< Effective sigma second temperature differetial
    real, intent(out) :: d_mat(nc,nc)       !< hard-sphere diameter [m]
    real, intent(out) :: d_T_mat(nc,nc)     !< T_derivative [m/K]
    real, intent(out) :: d_TT_mat(nc,nc)    !< T_derivative [m/K]
    integer :: i,j,k,n_quad
    real :: x_vec(max_n_quadrature), w_vec(max_n_quadrature)
    real :: f_vec(max_n_quadrature), quad_error
    real :: r, U_divk,U_divk_T,U_divk_TT,sigmaj_2
    real :: prefactor
    real :: sigmaj_2_T, sigmaj_2_TT, r_T, r_TT
    real :: prefactor_T, prefactor_TT
    real :: U_divk_r, U_divk_Tr, U_divk_rr
    real :: minus_exp_term
    real :: d_T1, d_T2, d_T3, d_T4, d_TT1, d_TT2, d_TT3, d_TT4
    real :: d_T5, d_TT5
    real :: r0,r0_T,r0_TT

    ! Get quadrature points
    call get_quadrature_positions(hs_diam_quadrature,x_vec,n_quad)
    call get_quadrature_weights(hs_diam_quadrature,w_vec,n_quad)

    ! Set hard-sphere diameter matrix variables to zero:
    ! The "_mat" variables also considers binary combinations
    d_mat=0.0
    d_T_mat=0.0
    d_TT_mat=0.0

    ! Loop over all components and obtain the pure component values
    do j = 1,nc
       do k = 1,nc
          !
          if (.not. exact_binary_dhs .and. k/=j) then
             cycle
          end if

          if (only_integrate_active_hs_area) then
             ! Solve for exp(-beta*u(r0)) = machine_prec
             call calc_zero_d_integrand(j,k,T,s_vc,sigma_eff(j,k),r0,r0_T,r0_TT)
             ! Use 1-exp(-beta*u(r)) = 1 for [0,r0]:
             d_mat(j,k) = r0
             d_T_mat(j,k) = r0_T
             d_TT_mat(j,k) = r0_TT
          else
             r0 = 0.0
             r0_T = 0.0
             r0_TT = 0.0
          endif
          ! Loop quadrature points
          do i = 1,n_quad

             ! Obtain the position to evaluate r [m] and T-derivs
             sigmaj_2=0.5*(sigma_eff(j,k) - r0)
             sigmaj_2_T=0.5*(sigma_eff_T(j,k) - r0_T)
             sigmaj_2_TT=0.5*(sigma_eff_TT(j,k) - r0_TT)

             r=sigmaj_2*x_vec(i)+sigmaj_2 + r0
             r_T=sigmaj_2_T*x_vec(i)+sigmaj_2_T + r0_T
             r_TT=sigmaj_2_TT*x_vec(i)+sigmaj_2_TT + r0_TT

             ! Obtain the interaction potential and T-derivatives
             call calc_mie_potential_quantumcorrected(j,k,s_vc,&
                  saftvrmie_param%sigma_ij(j,k),saftvrmie_param%eps_divk_ij(j,k),&
                  saftvrmie_param%lambda_a_ij(j,k),saftvrmie_param%lambda_r_ij(j,k),&
                  saftvrmie_param%Cij(j,k),&
                  saftvrmie_param%Quantum_const_1a_ij(j,k),&
                  saftvrmie_param%Quantum_const_1r_ij(j,k),&
                  saftvrmie_param%Quantum_const_2a_ij(j,k),&
                  saftvrmie_param%Quantum_const_2r_ij(j,k),&
                  r,U_divk,U_divk_T,U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr)

             ! Calculate the hard-sphere diameter through approximating
             ! the integral through a 10-point Gauss-Legendre quadrature

             ! The prefactor and its Temperature derivatives
             prefactor=sigmaj_2*w_vec(i)
             prefactor_T=sigmaj_2_T*w_vec(i)
             prefactor_TT=sigmaj_2_TT*w_vec(i)

             ! Obtain the hahrd-sphere diameter and T-derivatives
             minus_exp_term=-1.0*exp(-U_divk/T)

             ! Hard sphere diameter [m]
             d_mat(j,k)=d_mat(j,k)+prefactor*(1.0+minus_exp_term)

             if (estimate_quadrature_error) then
                ! Store function evaluations for post-processing error estimate
                f_vec(i) = sigmaj_2*(1.0+minus_exp_term)
             endif

             ! Intermediate variables for the first order T-derivative of d
             d_T1=prefactor
             d_T2=minus_exp_term
             d_T3=(U_divk/(T**2)-U_divk_T/T-U_divk_r*r_T/T)
             d_T4=prefactor_T
             d_T5=(1.0+minus_exp_term)

             ! The first order T-derivative of the hard-sphere diameter
             d_T_mat(j,k)=d_T_mat(j,k)+d_T1*d_T2*d_T3+d_T4*d_T5

             ! Intermediate variables for the second order T-derivative of d
             d_TT1=prefactor_T
             d_TT2=minus_exp_term*d_T3
             d_TT3=(U_divk_T/(T**2)+U_divk_r*r_T/(T**2)-2.0*U_divk/(T**3)& ! First term
                  -U_divk_TT/T-U_divk_Tr*r_T/T+U_divk_T/(T**2)&
                  -U_divk_Tr*r_T/T-U_divk_rr*(r_T**2)/T-U_divk_r*r_TT/T+U_divk_r*r_T/(T**2))
             d_TT4=prefactor_TT
             d_TT5=d_TT2

             ! The second order TT-derivative of the hard-sphere diameter
             d_TT_mat(j,k)=d_TT_mat(j,k)+d_TT1*d_T2*d_T3+d_T1*d_TT2*d_T3+&
                  d_T1*d_T2*d_TT3+d_TT4*d_T5+d_T4*d_TT5
          end do
          if (estimate_quadrature_error) then
             quad_error = calc_quadrature_error(f_vec,d_mat(j,k)-r0,hs_diam_quadrature)
             print *,"Estimated relative quadrature error in hard "//&
                  "sphere diameter calculation: ",quad_error
          endif
       end do
    end do

    if (.not. exact_binary_dhs) then
       ! Loop over all components and obtain the mixture values
       do i=1,nc
          do j=i+1,nc
             d_mat(i,j)=0.5*(d_mat(i,i)+d_mat(j,j))
             d_T_mat(i,j)=0.5*(d_T_mat(i,i)+d_T_mat(j,j))
             d_TT_mat(i,j)=0.5*(d_TT_mat(i,i)+d_TT_mat(j,j))

             d_mat(j,i)=d_mat(i,j)
             d_T_mat(j,i)=d_T_mat(i,j)
             d_TT_mat(j,i)=d_TT_mat(i,j)
          end do
       end do
    end if

  end subroutine calc_hardsphere_diameter

  subroutine calc_zero_d_integrand(j,k,T,s_vc,sigma_eff,r0,r0_T,r0_TT)
    !--------------------------------------------------------------------
    ! Calculate point where 1-exp(beta u(r)) = 0.0, numerically     !
    !
    !! \author Morten Hammer, November 2018
    !---------------------------------------------------------------------
    use nonlinear_solvers, only: nonlinear_solver, newton_secondorder_singlevar
    use numconstants, only: machine_prec
    integer, intent(in) :: j, k              !< Current binary
    real, intent(in) :: T                    !< Temperature [K]
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(in) :: sigma_eff            !< Effective sigma [m]
    real, intent(out) :: r0                  !< r0 [m]
    real, intent(out) :: r0_T                !< T-deriv of r0 [m/K]
    real, intent(out) :: r0_TT               !< TT-deriv of r0 [m/K^2]
    ! Locals
    type(nonlinear_solver) :: solver
    real :: param(4)
    real :: rs, rsmin, rsmax
    real :: U_divk,U_divk_T,U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr
    !real :: f,dfdr,d2fdr2,f1,df1dr,d2f1dr2,eps
    param(1) = real(j)
    param(2) = real(k)
    param(3) = T
    param(4) = sigma_eff
    rs = 0.9
    rsmin = 0.0
    rsmax = 1.0
    solver%rel_tol = 1.0e-13
    solver%max_it = 20
    solver%ls_max_it = 3
    !call zero_integrand(rs,f,param,dfdr,d2fdr2)
    !eps = 1.0e-5
    !call zero_integrand(rs+eps,f1,param,df1dr,d2f1dr2)
    !print *,(f1-f)/eps,dfdr
    !print *,(df1dr-dfdr)/eps,d2fdr2
    !stop
    call newton_secondorder_singlevar(zero_integrand,0.7,rsmin,rsmax,solver,rs,param)
    if (solver%exitflag /= 0) then
       call stoperror("Not able to solve for point where d-integrand becomes zero")
    else
       r0 = rs*sigma_eff
       call calc_mie_potential_quantumcorrected(j,k,s_vc,&
            saftvrmie_param%sigma_ij(j,k),saftvrmie_param%eps_divk_ij(j,k),&
            saftvrmie_param%lambda_a_ij(j,k),saftvrmie_param%lambda_r_ij(j,k),&
            saftvrmie_param%Cij(j,k),&
            saftvrmie_param%Quantum_const_1a_ij(j,k),&
            saftvrmie_param%Quantum_const_1r_ij(j,k),&
            saftvrmie_param%Quantum_const_2a_ij(j,k),&
            saftvrmie_param%Quantum_const_2r_ij(j,k),&
            r0,U_divk,U_divk_T,U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr)
       r0_T=-(U_divk_T + log(machine_prec))/U_divk_r
       r0_TT= -(U_divk_TT+2.0*U_divk_Tr*r0_T+U_divk_rr*r0_T**2)/U_divk_r
    endif
  end subroutine calc_zero_d_integrand

  subroutine zero_integrand(rs,f,param,dfdr,d2fdr2)
    use numconstants, only: machine_prec
    use saftvrmie_containers, only: saftvrmie_var
    !$ use omp_lib, only: omp_get_thread_num
    real, intent(in) :: rs
    real, intent(in) :: param(4)
    real, intent(out) :: f,dfdr,d2fdr2
    ! Locals
    integer :: j,k
    real :: U_divk,U_divk_T,U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr
    real :: T, sigma_eff, r
    integer :: iTh
    iTh = 1
    !$ iTh = omp_get_thread_num() + 1
    j = int(param(1))
    k = int(param(2))
    T = param(3)
    sigma_eff = param(4)
    r = rs*sigma_eff
    call calc_mie_potential_quantumcorrected(j,k,saftvrmie_var(iTh),&
         saftvrmie_param%sigma_ij(j,k),saftvrmie_param%eps_divk_ij(j,k),&
         saftvrmie_param%lambda_a_ij(j,k),saftvrmie_param%lambda_r_ij(j,k),&
         saftvrmie_param%Cij(j,k),&
         saftvrmie_param%Quantum_const_1a_ij(j,k),&
         saftvrmie_param%Quantum_const_1r_ij(j,k),&
         saftvrmie_param%Quantum_const_2a_ij(j,k),&
         saftvrmie_param%Quantum_const_2r_ij(j,k),&
         r,U_divk,U_divk_T,U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr)
    f = U_divk/T + log(machine_prec)
    dfdr = sigma_eff*U_divk_r/T
    d2fdr2 = sigma_eff**2*U_divk_rr/T
  end subroutine zero_integrand

  subroutine calc_binary_effective_sigma(nc,T,s_vc,sigma_eff,sigma_eff_T,sigma_eff_TT)
    !--------------------------------------------------------------------
    ! This subroutine computes the effective sigma, defined as the position
    ! where the interaction potential equals zero
    !
    !! \author Øivind Wilhelmsen, March 2018
    !! \author Ailo Aasen, October 2018
    !---------------------------------------------------------------------
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, limit_dx, &
         premReturn, setXv
    use thermopack_var, only: get_active_eos_container, eos_container
    integer, intent(in) :: nc                        !< Number of components
    real, intent(in) :: T                            !< Temperature [K]
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(out) :: sigma_eff(nc,nc)               !< Effective sigma [m]
    real, intent(out) :: sigma_eff_T(nc,nc)             !< T-deriv of effective sigma [m/K]
    real, intent(out) :: sigma_eff_TT(nc,nc)            !< TT-deriv of effective sigma [m/K^2]
    type(nonlinear_solver) :: solver
    real :: D, D_T, D_TT, D2, D2_T, D2_TT            !< Quantum parameters and derivatives
    real :: U_divk, U_divk_T, U_divk_TT
    real :: param(10)
    real, dimension(1) :: sigma_scaled, xmin, xmax
    integer :: i, j
    real :: U_divk_r, U_divk_Tr, U_divk_rr
    type(eos_container), pointer :: p_act_eosc
    p_act_eosc => get_active_eos_container()

    if (quantum_correction_hs==0) then ! With no quantum corrections
       sigma_eff=saftvrmie_param%sigma_ij
       sigma_eff_T=0.0
       sigma_eff_TT=0.0
       return
    else
       do i=1,nc
          do j=i,nc
             if ((.not. exact_crosspot_eff) .and. i/=j) then
                cycle
             end if

             ! Obtain the quantum-parameters
             call get_DFeynHibbsPower(i,j,D,D_T,D_TT,s_vc,power_in=1)
             call get_DFeynHibbsPower(i,j,D2,D2_T,D2_TT,s_vc,power_in=2)

             ! Construct the parameter vector
             param(1)=saftvrmie_param%lambda_r_ij(i,j)
             param(2)=saftvrmie_param%lambda_a_ij(i,j)
             param(3)=saftvrmie_param%sigma_ij(i,j)
             param(4)=mie_c_factor(saftvrmie_param%lambda_r_ij(i,j),&
                  saftvrmie_param%lambda_a_ij(i,j))
             param(5)=D
             param(6)=D2
             param(7)=saftvrmie_param%Quantum_const_1a_ij(i,j)
             param(8)=saftvrmie_param%Quantum_const_1r_ij(i,j)
             param(9)=saftvrmie_param%Quantum_const_2a_ij(i,j)
             param(10)=saftvrmie_param%Quantum_const_2r_ij(i,j)

             ! Set the limits and the initial condition
             xmin=0.001
             xmax=10
             sigma_scaled=1.0
             solver%abs_tol = 1e-12

             ! NB, the variable we iterate on is u=r/sigma, i.e. a scaled radius
             ! The interaction potential is scaled with epsilon
             call nonlinear_solve(solver,sigmaeff_U,sigmaeff_dUdz,sigmaeff_dUdz,limit_dx,&
                  premReturn,setXv,sigma_scaled,xmin,xmax,param)
             if (solver%exitflag /= 0 .and. verbose) then
                print *,"Not able to solve for effective sigma"
                print *,"T = ",T
                print *,"comp = ",trim(p_act_eosc%comps(i)%p_comp%ident)
                print *,"lambda_r = ",saftvrmie_param%lambda_r_ij(i,j)
                print *,"lambda_a = ",saftvrmie_param%lambda_a_ij(i,j)
                print *,"sigma    = ",saftvrmie_param%sigma_ij(i,j)
                print *,"eps_div_k    = ",saftvrmie_param%eps_divk_ij(i,j)
                print *,"D    = ",D
                print *,"D2    = ",D2
                print *,"Quantum_const_1a    = ",saftvrmie_param%Quantum_const_1a_ij(i,j)
                print *,"Quantum_const_1r    = ",saftvrmie_param%Quantum_const_1r_ij(i,j)
                print *,"Quantum_const_2a    = ",saftvrmie_param%Quantum_const_2a_ij(i,j)
                print *,"Quantum_const_2r    = ",saftvrmie_param%Quantum_const_2r_ij(i,j)
             endif

             sigma_eff(i,j)=sigma_scaled(1)*saftvrmie_param%sigma_ij(i,j)
             call calc_mie_potential_quantumcorrected(i,j,s_vc,&
                  saftvrmie_param%sigma_ij(i,j),saftvrmie_param%eps_divk_ij(i,j),&
                  saftvrmie_param%lambda_a_ij(i,j),saftvrmie_param%lambda_r_ij(i,j),&
                  saftvrmie_param%Cij(i,j),&
                  saftvrmie_param%Quantum_const_1a_ij(i,j),&
                  saftvrmie_param%Quantum_const_1r_ij(i,j),&
                  saftvrmie_param%Quantum_const_2a_ij(i,j),&
                  saftvrmie_param%Quantum_const_2r_ij(i,j),&
                  sigma_eff(i,j),U_divk,U_divk_T,U_divk_TT, U_divk_r, U_divk_Tr, U_divk_rr)

             ! The temperature deriavtives of sigma have been found through
             ! implicit derivation of U/epsilon to first and second order in T
             ! of the equation U(sigma(T),T)=0
             sigma_eff_T(i,j)=-1.0*U_divk_T/U_divk_r
             sigma_eff_TT(i,j)=-1.0*(U_divk_TT+&
                  U_divk_rr*(sigma_eff_T(i,j)**2)+&
                  2.0*U_divk_Tr*(sigma_eff_T(i,j)))/U_divk_r

             sigma_eff(j,i) = sigma_eff(i,j)
             sigma_eff_T(j,i) = sigma_eff_T(i,j)
             sigma_eff_TT(j,i) = sigma_eff_TT(i,j)
          end do
       end do
       if (.not. exact_crosspot_eff) then
          ! Use simplified combining rule for effective sigma
          do i=1,nc
             do j=i+1,nc
                sigma_eff(i,j) = 0.5*(sigma_eff(i,i) + sigma_eff(j,j))
                sigma_eff_T(i,j) = 0.5*(sigma_eff_T(i,i) + sigma_eff_T(j,j))
                sigma_eff_TT(i,j) = 0.5*(sigma_eff_TT(i,i) + sigma_eff_TT(j,j))

                sigma_eff(j,i) = sigma_eff(i,j)
                sigma_eff_T(j,i) = sigma_eff_T(i,j)
                sigma_eff_TT(j,i) = sigma_eff_TT(i,j)
             end do
          end do
       end if
    end if


  end subroutine calc_binary_effective_sigma

  subroutine sigmaeff_U(f,var,param)

    !--------------------------------------------------------------------
    !  2018-03-06, Oivind Wilhelmsen
    !
    !  This subroutine computes the scaled interaction potential U/epsilon,
    !  for use in the subroutine to find the effective sigma, i.e. U(r=sigma_eff)=0
    !
    !---------------------------------------------------------------------

    implicit none

    real, intent(out) :: f !< Function values
    real, intent(in) :: var !< Variable vector
    real, dimension(10), intent(in) :: param !< Parameter vector

    ! Local variables
    real :: n, m
    real :: mie_c_factor_par, D, D2, sigma_Mie
    real :: sigma_r, r_n, r_m, U_divk
    real :: Q1_n, Q2_n, Q1_m, Q2_m
    real :: r_2, r_4, product_q1, product_q2, en_div_r

    ! Extract the parameters
    n=param(1)                  ! Repulsive exponent of Mie potential
    m=param(2)                  ! Attractive exponent of Mie potential
    sigma_Mie=param(3)          ! The sigma from the Mie potential
    mie_c_factor_par=param(4)   ! Prefactor
    D=param(5)                  ! Quantum parameter
    D2=param(6)                 ! Quantum parameter squared
    Q1_m=param(7)               ! Prefactor 1-order quantum correction
    Q1_n=param(8)               ! Prefactor 1-order quantum correction
    Q2_m=param(9)               ! Prefactor 2-order quantum correction
    Q2_n=param(10)              ! Prefactor 2-order quantum correction

    ! The inverse radii
    sigma_r=1.0/var
    en_div_r=1.0/(sigma_Mie*var)
    r_n=(sigma_r)**n
    r_m=(sigma_r)**m

    ! Zeroth order contribution
    U_divk=r_n-r_m

    ! Add the first order quantum correction
    if (quantum_correction_hs>0) then

       r_2=(en_div_r)**2
       product_q1=r_2*(Q1_n*r_n-Q1_m*r_m)
       U_divk=U_divk+D*product_q1

    end if

    ! Add the second order quantum correction
    if (quantum_correction_hs>1) then

       r_4=r_2**2
       product_q2=r_4*(Q2_n*r_n-Q2_m*r_m)
       U_divk=U_divk+D2*product_q2
    end if

    ! Multiply everything by the prefactor:
    f=U_divk*mie_c_factor_par                    ! U/epsilon
  end subroutine sigmaeff_U

  subroutine sigmaeff_dUdz(f,var,param)

    !--------------------------------------------------------------------
    !  2018-03-06, Oivind Wilhelmsen
    !
    !  This subroutine computes the scaled radial derivative of the interaction potential
    !  dU/dz*(1/epsilon), where z=r/sigma_Mie, for use in the subroutine
    !  to find the effective sigma, i.e. U(r=sigma_eff)=0
    !
    !---------------------------------------------------------------------

    implicit none

    real, intent(out) :: f !< Function values
    real, intent(in) :: var !< Variable vector
    real, dimension(10), intent(in) :: param !< Parameter vector

    ! Local variables
    real :: n, m
    real :: mie_c_factor_par, D, D2, sigma_Mie
    real :: sigma_r, r, dr_n, dr_m, dU_divk, r_n, r_m
    real :: Q1_n, Q2_n, Q1_m, Q2_m
    real :: dr_2, dr_4, dproduct_q1, dproduct_q2, r_2, r_4, en_div_r

    ! Extract the parameters
    n=param(1)                  ! Repulsive exponent of Mie potential
    m=param(2)                  ! Attractive exponent of Mie potential
    sigma_Mie=param(3)          ! The sigma from the Mie potential
    mie_c_factor_par=param(4)   ! Prefactor
    D=param(5)                  ! Quantum parameter
    D2=param(6)                 ! Quantum parameter squared
    Q1_m=param(7)               ! Prefactor 1-order quantum correction
    Q1_n=param(8)               ! Prefactor 1-order quantum correction
    Q2_m=param(9)               ! Prefactor 2-order quantum correction
    Q2_n=param(10)              ! Prefactor 2-order quantum correction

    ! The inverse radii
    sigma_r=1.0/var
    r=var*sigma_Mie
    en_div_r=1.0/r

    ! Calculate variables
    r_n=(sigma_r)**n
    r_m=(sigma_r)**m
    dr_n=-1.0*n*(r_n)*en_div_r
    dr_m=-1.0*m*(r_m)*en_div_r

    ! Zeroth order contribution
    dU_divk=dr_n-dr_m

    ! Add the first order quantum correction
    if (quantum_correction_hs>0) then

       r_2=(en_div_r)**2
       dr_2=-2.0*(en_div_r**3)

       ! The modified interaction potential and derivatives
       dproduct_q1=dr_2*(Q1_n*r_n-Q1_m*r_m)+&
            r_2*(Q1_n*dr_n-Q1_m*dr_m)

       dU_divk=dU_divk+D*dproduct_q1

    end if

    ! Add the second order quantum correction
    if (quantum_correction_hs>1) then

       r_4=en_div_r**4
       dr_4=-4.0*((en_div_r)**5)

       ! The modified interaction potential and its T-derivatives
       dproduct_q2=dr_4*(Q2_n*r_n-Q2_m*r_m)+&
            r_4*(Q2_n*dr_n-Q2_m*dr_m)
       dU_divk=dU_divk+D2*dproduct_q2
    end if

    ! Multiply everything by the prefactor, NB: This is a scaled derivative:

    f=dU_divk*mie_c_factor_par*sigma_Mie      ! dU_d(r/sigma_Mie)/epsilon
  end subroutine sigmaeff_dUdz

  subroutine calc_mie_potential_quantumcorrected(comp1,comp2,s_vc,sigma,eps_depth_divk,&
       lambda_a,lambda_r,c_fac,Q1_m,Q1_n,Q2_m,Q2_n,r,U_divk,U_divk_T,&
       U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr,U_divk_TTr,U_divk_Trr,U_divk_rrr)
    !--------------------------------------------------------------------
    !  2018-02-14, Oivind Wilhelmsen
    !
    !  This subroutine calulates the magntiude of the quantum corrected
    !  Mie poential at position r [m]
    !---------------------------------------------------------------------
    integer, intent(in) :: comp1,comp2         !< Component indices
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(in) :: sigma,eps_depth_divk   !< potential parameters
    real, intent(in) :: lambda_a,lambda_r      !< potential parameters
    real, intent(in) :: c_fac, Q1_n, Q1_m      !< potential parameters
    real, intent(in) :: Q2_n, Q2_m             !< potential parameters
    real, intent(in) :: r                      !< particle distances [m]
    real, intent(out) :: U_divk                !< value of interaction div. by kB
    real, intent(out), optional :: U_divk_T, U_divk_TT   !< T-derivatives
    real, intent(out), optional :: U_divk_r, U_divk_rr, U_divk_rrr     !< r-derivatives
    real, intent(out), optional :: U_divk_Tr, U_divk_Trr, U_divk_TTr !< Cross-derivatives
    ! Locals
    real :: m, n, r_n, r_m, r_1                !< Parameters and radii [m]
    real :: product_q1, product_q2             !< intermediate variables
    real :: D, D_T, D_TT                       !< Quantum par. and deriv.
    real :: D2, D2_T, D2_TT                    !< Quantum par. squared
    !real :: D3, D3_T, D3_TT                    !< Quantum par. power of 3
    !real :: D4, D4_T, D4_TT                    !< Quantum par. power of 4
    real :: Mie_pref                           !< Prefactor Mie potential
    real :: en_r, dr_n, dr_m, d2r_n, d2r_m     !< Intermediate variables
    real :: d3r_n, d3r_m, en_r2, en_r4
    real :: product_q1_r, product_q1_rr, product_q1_rrr
    real :: product_q2_r, product_q2_rr, product_q2_rrr

    ! The repulsive and attracive exponents
    n=lambda_r
    m=lambda_a

    ! The inverse radii
    en_r=1/r
    r_1=sigma*en_r

    ! Powers of the inverse radii and derivs
    r_n=(r_1)**n
    r_m=(r_1)**m
    dr_n=-1.0*n*(r_n)*en_r
    dr_m=-1.0*m*(r_m)*en_r
    d2r_n=-1.0*(n+1.0)*dr_n*en_r
    d2r_m=-1.0*(m+1.0)*dr_m*en_r
    d3r_n=-1.0*(n+2.0)*d2r_n*en_r
    d3r_m=-1.0*(m+2.0)*d2r_m*en_r

    ! Zeroth order contributions (only from the Mie-potential)
    U_divk=r_n-r_m
    if (present(U_divk_T)) then
       U_divk_T=0.0
    endif
    if (present(U_divk_TT)) then
       U_divk_TT=0.0
    endif
    if (present(U_divk_r)) then
       U_divk_r=dr_n-dr_m
    endif
    if (present(U_divk_rr)) then
       U_divk_rr=d2r_n-d2r_m
    endif
    if (present(U_divk_rrr)) then
       U_divk_rrr=d3r_n-d3r_m
    endif
    if (present(U_divk_Tr)) then
       U_divk_Tr=0.0
    endif
    if (present(U_divk_Trr)) then
       U_divk_Trr=0.0
    endif
    if (present(U_divk_TTr)) then
       U_divk_TTr=0.0
    endif
    ! Compute the quantum parameter D and derivatives to a
    ! suitable order decided by the quantum correction flag
    if (quantum_correction_hs==0) then

       D=0.0
       D_T=0.0
       D_TT=0.0
       D2=0.0
       D2_T=0.0
       D2_TT=0.0

    elseif ((quantum_correction_hs==1) .or. (quantum_correction_hs==2)) then
       call get_DFeynHibbsPower(comp1,comp2,D,D_T,D_TT,s_vc,power_in=1)
       call get_DFeynHibbsPower(comp1,comp2,D2,D2_T,D2_TT,s_vc,power_in=2)
    else
       call stoperror("saftvrmie_hardsphere :: calc_mie_potential_quantumcorrected : errornous quantum parameter chosen")
    end if

    ! Add the first order quantum correction
    if (quantum_correction_hs>0) then
       en_r2 = en_r**2
       ! The modified interaction potential and derivatives
       product_q1=en_r2*(Q1_n*r_n-Q1_m*r_m)
       product_q1_r= -en_r2*((n+2)*Q1_n*r_n-(m+2)*Q1_m*r_m)*en_r
       product_q1_rr= en_r2*((n+3)*(n+2)*Q1_n*r_n-(m+3)*(m+2)*Q1_m*r_m)*en_r2
       product_q1_rrr= -en_r2*((n+4)*(n+3)*(n+2)*Q1_n*r_n-(m+2)*(m+4)*(m+3)*Q1_m*r_m)*en_r*en_r2
       U_divk=U_divk+D*product_q1
       if (present(U_divk_T)) then
          U_divk_T=U_divk_T+D_T*product_q1
       endif
       if (present(U_divk_TT)) then
          U_divk_TT=U_divk_TT+D_TT*product_q1
       endif
       if (present(U_divk_r)) then
          U_divk_r=U_divk_r+D*product_q1_r
       endif
       if (present(U_divk_rr)) then
          U_divk_rr=U_divk_rr+D*product_q1_rr
       endif
       if (present(U_divk_rrr)) then
          U_divk_rrr=U_divk_rrr+D*product_q1_rrr
       endif
       if (present(U_divk_Tr)) then
          U_divk_Tr=U_divk_Tr+D_T*product_q1_r
       endif
       if (present(U_divk_Trr)) then
          U_divk_Trr=U_divk_Trr+D_T*product_q1_rr
       endif
       if (present(U_divk_TTr)) then
          U_divk_TTr=U_divk_TTr+D_TT*product_q1_r
       endif
    end if

    ! Add the second order quantum correction
    if (quantum_correction_hs>1) then

       en_r4 = en_r**4

       ! The modified interaction potential and its T-derivatives
       product_q2=en_r4*(Q2_n*r_n-Q2_m*r_m)
       product_q2_r=-en_r4*((n+4)*Q2_n*r_n-(m+4)*Q2_m*r_m)*en_r
       product_q2_rr=en_r4*((n+5)*(n+4)*Q2_n*r_n-(m+5)*(m+4)*Q2_m*r_m)*en_r2
       product_q2_rrr=-en_r4*((n+6)*(n+5)*(n+4)*Q2_n*r_n-(m+6)*(m+5)*(m+4)*Q2_m*r_m)*en_r2*en_r

       U_divk=U_divk+D2*product_q2
       if (present(U_divk_T)) then
          U_divk_T=U_divk_T+D2_T*product_q2
       endif
       if (present(U_divk_TT)) then
          U_divk_TT=U_divk_TT+D2_TT*product_q2
       endif
       if (present(U_divk_r)) then
          U_divk_r=U_divk_r+D2*product_q2_r
       endif
       if (present(U_divk_rr)) then
          U_divk_rr=U_divk_rr+D2*product_q2_rr
       endif
       if (present(U_divk_rrr)) then
          U_divk_rrr=U_divk_rrr+D2*product_q2_rrr
       endif
       if (present(U_divk_Tr)) then
          U_divk_Tr=U_divk_Tr+D2_T*product_q2_r
       endif
       if (present(U_divk_Trr)) then
          U_divk_Trr=U_divk_Trr+D2_T*product_q2_rr
       endif
       if (present(U_divk_TTr)) then
          U_divk_TTr=U_divk_TTr+D2_TT*product_q2_r
       endif
    end if

    ! The Mie potential prefactor, divided by kB
    Mie_pref = calc_mie_prefactor(mie_c_factor=c_fac, epsilon=eps_depth_divk)

    ! Multiply everything by the appropriate prefactor:
    U_divk=U_divk*Mie_pref
    if (present(U_divk_T)) then
       U_divk_T=U_divk_T*Mie_pref
    endif
    if (present(U_divk_TT)) then
       U_divk_TT=U_divk_TT*Mie_pref
    endif
    if (present(U_divk_r)) then
       U_divk_r=U_divk_r*Mie_pref
    endif
    if (present(U_divk_rr)) then
       U_divk_rr=U_divk_rr*Mie_pref
    endif
    if (present(U_divk_rrr)) then
       U_divk_rrr=U_divk_rrr*Mie_pref
    endif
    if (present(U_divk_Tr)) then
       U_divk_Tr=U_divk_Tr*Mie_pref
    endif
    if (present(U_divk_Trr)) then
       U_divk_Trr=U_divk_Trr*Mie_pref
    endif
    if (present(U_divk_TTr)) then
       U_divk_TTr=U_divk_TTr*Mie_pref
    endif
  end subroutine calc_mie_potential_quantumcorrected

  subroutine calc_hardsphere_zeta_and_derivatives(nc,T,V,n,&
       d_mat,d_T_mat,d_TT_mat,zeta,zeta_V,&
       zeta_VV,zeta_T,zeta_TT,zeta_n,zeta_Vn,zeta_VT,zeta_Tn)
    !---------------------------------------------------------------------
    !  2018-02-24, Oivind Wilhelmsen
    ! Obtain the zeta-variable and derivatives, used in the hard-sphere
    ! contribution and derivatives. Derivatives checked numerically.
    !---------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    real, intent(in) :: d_mat(nc,nc),d_T_mat(nc,nc), d_TT_mat(nc,nc)
    real, intent(out) :: zeta(4)    !< zeta variable (zeta_0,...,zeta_3)
    real, intent(out) :: zeta_V(4),zeta_VV(4)        !< zeta derivatives
    real, intent(out) :: zeta_T(4),zeta_TT(4)        !< zeta derivatives
    real, intent(out) :: zeta_n(nc,4),zeta_Vn(nc,4)  !< zeta derivatives
    real, intent(out) :: zeta_VT(4),zeta_Tn(nc,4)    !< zeta derivatives
    integer :: i, l, index_l
    real :: constant_piN

    ! First allocate zero to all vectors
    zeta=0.0
    zeta_V=0.0
    zeta_VV=0.0
    zeta_T=0.0
    zeta_TT=0.0
    zeta_n=0.0
    zeta_Vn=0.0
    zeta_VT=0.0
    zeta_Tn=0.0

    ! Loop over the number of indexes of the zeta (l=0,1,2,3) and
    ! the number of component (i) where (index_l=1,2,3,4)

    do l = 0, 3

       index_l=l+1

       do i = 1, nc
          ! The zeta variable and its derivatives:
          zeta(index_l)=zeta(index_l)+saftvrmie_param%ms(i)*n(i)*d_mat(i,i)**(l)
          zeta_n(i,index_l)=saftvrmie_param%ms(i)*(d_mat(i,i)**l)
          zeta_Vn(i,index_l)=-1.0*saftvrmie_param%ms(i)*(d_mat(i,i)**l)/V

          if (l>0) then

             zeta_T(index_l)=zeta_T(index_l)+&
                  l*saftvrmie_param%ms(i)*n(i)*(d_mat(i,i)**(l-1))*d_T_mat(i,i)

             zeta_TT(index_l)=zeta_TT(index_l)+l*saftvrmie_param%ms(i)*n(i)*(d_mat(i,i)**(l-1))*&
                  d_TT_mat(i,i)

             zeta_VT(index_l)=zeta_VT(index_l)-(1.0*l/V)*saftvrmie_param%ms(i)*n(i)*&
                  (d_mat(i,i)**(l-1))*d_T_mat(i,i)

             zeta_Tn(i,index_l)=l*saftvrmie_param%ms(i)*(d_mat(i,i)**(l-1))*d_T_mat(i,i)
          end if
          if (l>1) then

             zeta_TT(index_l)=zeta_TT(index_l)+l*(l-1)*saftvrmie_param%ms(i)*n(i)*&
                  (d_mat(i,i)**(l-2))*(d_T_mat(i,i)**2)

          end if
       end do
    end do

    ! The prefactor of the expressions
    constant_piN=(pi*N_AVOGADRO/(6.0*V))

    ! Multiply all terms with the prefactor
    zeta=zeta*constant_piN
    zeta_T=zeta_T*constant_piN
    zeta_TT=zeta_TT*constant_piN
    zeta_V=-1.0*zeta/V
    zeta_VV=2.0*zeta/(V**2)
    zeta_n=zeta_n*constant_piN
    zeta_Vn=zeta_Vn*constant_piN
    zeta_VT=zeta_VT*constant_piN
    zeta_Tn=zeta_Tn*constant_piN
  end subroutine calc_hardsphere_zeta_and_derivatives

  subroutine calc_hardsphere_extra_zeta_and_derivatives(nc,T,V,n,&
       d_mat,d_T_mat,d_TT_mat,zeta,zeta_V,&
       zeta_VV,zeta_T,zeta_TT,zeta_n,zeta_Vn,zeta_VT,zeta_Tn)
    !---------------------------------------------------------------------
    ! 2019-01-16, Oivind Wilhelmsen
    ! Obtain the zeta-variable and derivatives, used in the hard-sphere
    ! contribution and derivatives. Derivatives checked numerically.
    ! We will map mu into zeta(1)
    !---------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    real, intent(in) :: d_mat(nc,nc),d_T_mat(nc,nc), d_TT_mat(nc,nc)
    real, intent(out) :: zeta(3)    !< zeta variable (zeta_0,...,zeta_3)
    real, intent(out) :: zeta_V(3),zeta_VV(3)        !< zeta derivatives
    real, intent(out) :: zeta_T(3),zeta_TT(3)        !< zeta derivatives
    real, intent(out) :: zeta_n(nc,3),zeta_Vn(nc,3)  !< zeta derivatives
    real, intent(out) :: zeta_VT(3),zeta_Tn(nc,3)    !< zeta derivatives
    integer :: i, l
    real :: constant_piN

    ! First allocate zero to all vectors
    zeta=0.0
    zeta_V=0.0
    zeta_VV=0.0
    zeta_T=0.0
    zeta_TT=0.0
    zeta_n=0.0
    zeta_Vn=0.0
    zeta_VT=0.0
    zeta_Tn=0.0

    do l = 2, 3 ! All indexes that belong to l=1 should be zero
       do i = 1, nc
          ! The zeta variable and its derivatives:
          zeta(l)=zeta(l)+saftvrmie_param%ms(i)*n(i)*d_mat(i,i)**(l)
          zeta_n(i,l)=saftvrmie_param%ms(i)*(d_mat(i,i)**l)
          zeta_Vn(i,l)=-1.0*saftvrmie_param%ms(i)*(d_mat(i,i)**l)/V

          zeta_T(l)=zeta_T(l)+&
               l*saftvrmie_param%ms(i)*n(i)*(d_mat(i,i)**(l-1))*d_T_mat(i,i)

          zeta_TT(l)=zeta_TT(l)+l*saftvrmie_param%ms(i)*n(i)*(d_mat(i,i)**(l-1))*&
               d_TT_mat(i,i)
          zeta_VT(l)=zeta_VT(l)-(1.0*l/V)*saftvrmie_param%ms(i)*n(i)*&
               (d_mat(i,i)**(l-1))*d_T_mat(i,i)

          zeta_Tn(i,l)=l*saftvrmie_param%ms(i)*(d_mat(i,i)**(l-1))*d_T_mat(i,i)

          zeta_TT(l)=zeta_TT(l)+l*(l-1)*saftvrmie_param%ms(i)*n(i)*&
               (d_mat(i,i)**(l-2))*(d_T_mat(i,i)**2)
       end do
    end do

    ! The prefactor of the expressions
    constant_piN=(pi*N_AVOGADRO/(6.0*V))

    ! Multiply all terms with the prefactor
    zeta=zeta*constant_piN
    zeta_T=zeta_T*constant_piN
    zeta_TT=zeta_TT*constant_piN
    zeta_V=-1.0*zeta/V
    zeta_VV=2.0*zeta/(V**2)
    zeta_n=zeta_n*constant_piN
    zeta_Vn=zeta_Vn*constant_piN
    zeta_VT=zeta_VT*constant_piN
    zeta_Tn=zeta_Tn*constant_piN
  end subroutine calc_hardsphere_extra_zeta_and_derivatives

  subroutine calc_hardsphere_dalpha_dzeta(zeta,alpha,dalpha_dzeta,&
       d2alpha_dzeta2)
    !---------------------------------------------------------------------
    !  2018-02-27, Oivind Wilhelmsen
    ! Obtain the zeta-variable and derivatives, used in the hard-sphere
    ! contribution and derivatives. Derivatives checked numerically.
    !---------------------------------------------------------------------
    real, intent(in)  :: zeta(4)              !< zeta variable
    real, intent(out) :: alpha                !< reduced Helholtz energy
    real, intent(out) :: dalpha_dzeta(4)      !< reduced Helhmholtz 1-deriv
    real, intent(out) :: d2alpha_dzeta2(4,4)  !< reduced Helholtz 2-deriv
    real :: zeta_0, zeta_1, zeta_2, zeta_3    !< variables
    real :: pref,ln_term, en_m_zeta_3         !< Intermediate variables

    ! First allocate zero to all vectors
    dalpha_dzeta=0.0
    d2alpha_dzeta2=0.0

    ! Extracting variables from vector to enhance readability
    zeta_0=zeta(1)
    zeta_1=zeta(2)
    zeta_2=zeta(3)
    zeta_3=zeta(4)

    ! Prefactor that goes into many of the expressions
    pref=(1.0/zeta_0)
    en_m_zeta_3=1.0-zeta_3
    ln_term=log(abs(en_m_zeta_3))

    ! The reduced Helholtz energy
    alpha=pref*((3.0*zeta_1*zeta_2)/en_m_zeta_3+&
         (zeta_2**3)/(zeta_3*(en_m_zeta_3**2))+&
         (((zeta_2**3)/(zeta_3**2))-zeta_0)*ln_term)

    ! The first order derivatives
    dalpha_dzeta(1)=-1.0*pref*(alpha+ln_term)                  ! dalpha_dzeta_0
    dalpha_dzeta(2)=pref*3.0*zeta_2/(en_m_zeta_3)              ! dalpha_dzeta_1
    dalpha_dzeta(3)=pref*((3.0*zeta_1)/en_m_zeta_3+ &          ! dalpha_dzeta_2
         (3.0*zeta_2**2)/(zeta_3*(en_m_zeta_3**2))+&
         ((3*zeta_2**2)/(zeta_3**2))*ln_term)
    dalpha_dzeta(4)=pref*((3*zeta_1*zeta_2)/(en_m_zeta_3**2)+& !dalpha_dzeta_3
         ((zeta_2**3)*(3.0*zeta_3-1.0))/((zeta_3**2)*(en_m_zeta_3**3))-&
         ((2.0*zeta_2**3)/(zeta_3**3))*ln_term-&
         (zeta_2**3-zeta_0*zeta_3**2)/((zeta_3**2)*en_m_zeta_3))

    ! The second order derivatives [the diagonal elements]
    d2alpha_dzeta2(1,1)=(pref**2)*(2.0*alpha+2.0*ln_term)   ! d2alpha_dzeta0_2
    d2alpha_dzeta2(2,2)=0.0                                 ! d2alpha_dzeta1_2
    d2alpha_dzeta2(3,3)=(pref*6.0*zeta_2/zeta_3)*&          ! d2alpha_dzeta2_2
         (1.0/(en_m_zeta_3**2)+(1.0/zeta_3)*ln_term)
    d2alpha_dzeta2(4,4)=pref*((6.0*zeta_1*zeta_2)/(en_m_zeta_3**3)+&       ! 1
         (3.0*zeta_2**3)/((zeta_3**2)*(en_m_zeta_3**3))+&                  ! 2a
         (3.0*(zeta_2**3)*(3*zeta_3-1.0))/((zeta_3**2)*(en_m_zeta_3**4))-& ! 2b
         (2.0*(zeta_2**3)*(3*zeta_3-1.0))/((zeta_3**3)*(en_m_zeta_3**3))+& ! 2c
         (2.0*zeta_2**3)/((zeta_3**3)*(en_m_zeta_3))+&                     ! 3a
         (6.0*(zeta_2**3)*ln_term)/(zeta_3**4)+&                           ! 3b
         (2.0*zeta_0*zeta_3)/((zeta_3**2)*en_m_zeta_3)+&                   ! 4a
         (2.0*(zeta_2**3-zeta_0*(zeta_3**2)))/((zeta_3**3)*en_m_zeta_3)-&  ! 4b
         (zeta_2**3-zeta_0*(zeta_3**2))/((zeta_3**2)*(en_m_zeta_3**2)))    ! 4c

    ! The second order derivatives [off-diagonal components]
    d2alpha_dzeta2(1,2)=-1.0*pref*dalpha_dzeta(2)
    d2alpha_dzeta2(1,3)=-1.0*pref*dalpha_dzeta(3)
    d2alpha_dzeta2(1,4)=-1.0*pref*(dalpha_dzeta(4)-1.0/(en_m_zeta_3))
    d2alpha_dzeta2(2,3)=3.0*pref/en_m_zeta_3
    d2alpha_dzeta2(3,4)=pref*((3.0*zeta_1)/(en_m_zeta_3**2)+&
         (3.0*(zeta_2**2)*(3.0*zeta_3-1.0))/((zeta_3**2)*(en_m_zeta_3**3))-&
         ((6.0*zeta_2**2)/(zeta_3**3))*ln_term-&
         (3.0*zeta_2**2)/((zeta_3**2)*en_m_zeta_3))
    d2alpha_dzeta2(2,4)=dalpha_dzeta(2)/en_m_zeta_3

    ! Complete the matrix of second derivatives
    d2alpha_dzeta2(2,1)=d2alpha_dzeta2(1,2)
    d2alpha_dzeta2(3,1)=d2alpha_dzeta2(1,3)
    d2alpha_dzeta2(4,1)=d2alpha_dzeta2(1,4)
    d2alpha_dzeta2(3,2)=d2alpha_dzeta2(2,3)
    d2alpha_dzeta2(4,3)=d2alpha_dzeta2(3,4)
    d2alpha_dzeta2(4,2)=d2alpha_dzeta2(2,4)

  end subroutine calc_hardsphere_dalpha_dzeta

  subroutine calc_hardsphere_helmholtzenergy(nc,T,V,n,s_vc,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    !------------------------------------------------------------------------
    !  2018-12-04 Aasmund Ervik
    ! The reduced Helmholtz energy of the hard-sphere term and its derivatives
    ! This routine is just a wrapper that calls the appropriate specialised routine
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    type(saftvrmie_var_container), intent(inout) :: s_vc
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !<derivatives

    ! Initialize differentials
    if (present(a_TT)) then
       a_TT=0.0
    endif
    if (present(a_T)) then
       a_T=0.0
    endif
    if (present(a_V)) then
       a_V=0.0
    endif
    if (present(a_VV)) then
       a_VV=0.0
    endif
    if (present(a_TV)) then
       a_TV=0.0
    endif
    if (present(a_Tn)) then
       a_Tn=0.0
    endif
    if (present(a_Vn)) then
       a_Vn=0.0
    endif
    if (present(a_n)) then
       a_n=0.0
    endif
    if (present(a_nn)) then
       a_nn=0.0
    endif

    select case(hardsphere_EoS)
    case(HS_EOS_ORIGINAL)
       call calc_hardsphere_helmholtzenergy_original(nc,T,V,n,s_vc%dhs,&
            a,a_T,a_V,a_n,a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    case(HS_EOS_SANTOS)
       call calc_hardsphere_helmholtzenergy_santos(nc,T,V,n,s_vc,&
            a,a_T,a_V,a_n,a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    case(HS_EOS_PURE_DIJ)
       if (.not. exact_binary_dhs) then
          call stoperror("exact_binary_dhs must be true when using HS_EOS_PURE_DIJ")
       end if
       call calc_hardsphere_helmholtzenergy_pure(nc,T,V,n,s_vc,&
            a,a_T,a_V,a_n,a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    end select

  end subroutine calc_hardsphere_helmholtzenergy

  subroutine calc_hardsphere_helmholtzenergy_original(nc,T,V,n,dhs,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    !------------------------------------------------------------------------
    !  2018-02-27, Oivind Wilhelmsen
    ! The reduced Helmholtz energy of the hard-sphere term and its derivatives,
    ! as it stands in the Lafitte paper
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !<derivatives
    real :: zeta(4), zeta_V(4),zeta_VV(4),zeta_T(4),zeta_TT(4)
    real :: zeta_n(nc,4),zeta_Vn(nc,4),zeta_VT(4),zeta_Tn(nc,4)
    real :: alpha                !< reduced Helholtz energy
    real :: dalpha_dzeta(4)      !< reduced Helhmholtz 1-deriv
    real :: d2alpha_dzeta2(4,4)  !< reduced Helholtz 2-deriv
    integer :: k, comp1, comp2, l

    ! Obtain the zeta variables and its derivatives
    call calc_hardsphere_zeta_and_derivatives(nc,T,V,n,&
         dhs%d,dhs%d_T,dhs%d_TT,zeta,zeta_V,&
         zeta_VV,zeta_T,zeta_TT,zeta_n,zeta_Vn,zeta_VT,zeta_Tn)

    ! Obtain the reduced Helholtz energy and the derivatives with
    ! respect to the zeta-variables
    call calc_hardsphere_dalpha_dzeta(zeta,alpha,dalpha_dzeta,&
         d2alpha_dzeta2)

    ! The reduced Helholtz energy
    a=alpha

    do comp1= 1,nc
       do comp2= 1,nc
          do l = 1,4

             ! Start single-sum over l part of the derivatives
             if (comp2==1) then  ! Compute this only once for each "l"
                if (comp1==1) then  ! Compute this only once for each "l"
                   if (present(a_T)) then
                      a_T=a_T+dalpha_dzeta(l)*zeta_T(l)
                   endif
                   if (present(a_V)) then
                      a_V=a_V+dalpha_dzeta(l)*zeta_V(l)
                   endif
                   if (present(a_VV)) then
                      a_VV=a_VV+dalpha_dzeta(l)*zeta_VV(l)
                   endif
                   if (present(a_TV)) then
                      a_TV=a_TV+dalpha_dzeta(l)*zeta_VT(l)
                   endif
                   if (present(a_TT)) then
                      a_TT=a_TT+dalpha_dzeta(l)*zeta_TT(l)
                   endif
                end if

                ! The composition first order derivative of a
                if (present(a_n)) then
                   a_n(comp1)=a_n(comp1)+dalpha_dzeta(l)*zeta_n(comp1,l)
                endif
                if (present(a_Tn)) then
                   a_Tn(comp1)=a_Tn(comp1)+dalpha_dzeta(l)*zeta_Tn(comp1,l)
                endif
                if (present(a_Vn)) then
                   a_Vn(comp1)=a_Vn(comp1)+dalpha_dzeta(l)*zeta_Vn(comp1,l)
                endif
             end if

             ! End single sum over l part of the derivatives
             ! Start double sum over l and k part of the derivatives

             do k = 1,4
                if (comp2==1) then ! Compute this only once for each "l" and "k"
                   if (comp1==1) then ! Compute this only once for each "l" and "k"
                      if (present(a_VV)) then
                         a_VV=a_VV+d2alpha_dzeta2(l,k)*zeta_V(l)*zeta_V(k)
                      endif
                      if (present(a_TV)) then
                         a_TV=a_TV+d2alpha_dzeta2(l,k)*zeta_T(l)*zeta_V(k)
                      endif
                      if (present(a_TT)) then
                         a_TT=a_TT+d2alpha_dzeta2(l,k)*zeta_T(l)*zeta_T(k)
                      endif
                   end if

                   ! The composition first order derivative of a
                   if (present(a_Tn)) then
                      a_Tn(comp1)=a_Tn(comp1)+d2alpha_dzeta2(l,k)*&
                           zeta_T(l)*zeta_n(comp1,k)
                   endif
                   if (present(a_Vn)) then
                      a_Vn(comp1)=a_Vn(comp1)+d2alpha_dzeta2(l,k)*&
                           zeta_V(l)*zeta_n(comp1,k)
                   endif
                end if

                ! Second order mole number derivative
                if (present(a_nn)) then
                   a_nn(comp1,comp2)=a_nn(comp1,comp2)+d2alpha_dzeta2(l,k)*&
                        zeta_n(comp1,l)*zeta_n(comp2,k)
                endif
             end do
          end do
       end do
    end do
  end subroutine calc_hardsphere_helmholtzenergy_original

  subroutine calc_hardsphere_helmholtzenergy_santos(nc,T,V,n,s_vc,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    use saftvrmie_utils, only: calc_a0_a_product, calc_a0_plus_a1
    !------------------------------------------------------------------------
    !  2018-12-04 Morten Hammer, Aasmund Ervik
    ! The residual reduced Helmholtz energy of the hard-sphere term and its derivatives,
    ! following the book (2008) and paper (2005) by Santos and coworkers.
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    type(saftvrmie_var_container), intent(inout) :: s_vc
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !< derivatives
    ! Locals
    integer :: difflevel, i, j
    real :: aP1, aP2, ns
    real, target :: aL1_T,aL1_V,aL1_n(nc)
    real, target :: aL1_VV,aL1_TV,aL1_Vn(nc)
    real, target :: aL1_TT,aL1_Tn(nc),aL1_nn(nc,nc)
    real, pointer :: aP1_T,aP1_V,aP1_n(:)
    real, pointer :: aP1_VV,aP1_TV,aP1_Vn(:)
    real, pointer :: aP1_TT,aP1_Tn(:),aP1_nn(:,:)
    real, target :: aL2_T,aL2_V,aL2_n(nc)
    real, target :: aL2_VV,aL2_TV,aL2_Vn(nc)
    real, target :: aL2_TT,aL2_Tn(nc),aL2_nn(nc,nc)
    real, pointer :: aP2_T,aP2_V,aP2_n(:)
    real, pointer :: aP2_VV,aP2_TV,aP2_Vn(:)
    real, pointer :: aP2_TT,aP2_Tn(:),aP2_nn(:,:)
    real :: B2s,B2s_T,B2s_TT,B2s_Tn(nc),B2s_n(nc),B2s_nn(nc,nc)
    real :: B3s,B3s_T,B3s_TT,B3s_Tn(nc),B3s_n(nc),B3s_nn(nc,nc)

    aP1_T => NULL()
    aP1_V => NULL()
    aP1_n => NULL()
    aP1_VV => NULL()
    aP1_TV => NULL()
    aP1_Vn => NULL()
    aP1_TT => NULL()
    aP1_Tn => NULL()
    aP1_nn => NULL()
    aP2_T => NULL()
    aP2_V => NULL()
    aP2_n => NULL()
    aP2_VV => NULL()
    aP2_TV => NULL()
    aP2_Vn => NULL()
    aP2_TT => NULL()
    aP2_Tn => NULL()
    aP2_nn => NULL()

    if ( present(a_TT) .or. present(a_VV).or. present(a_TV) .or. &
         present(a_nn) .or. present(a_Tn).or. present(a_Vn)) then
       difflevel = 2
       aP1_T => aL1_T
       aP1_V => aL1_V
       aP1_n => aL1_n
       aP1_VV => aL1_VV
       aP1_TV => aL1_TV
       aP1_Vn => aL1_Vn
       aP1_TT => aL1_TT
       aP1_Tn => aL1_Tn
       aP1_nn => aL1_nn
       aP2_T => aL2_T
       aP2_V => aL2_V
       aP2_n => aL2_n
       aP2_VV => aL2_VV
       aP2_TV => aL2_TV
       aP2_Vn => aL2_Vn
       aP2_TT => aL2_TT
       aP2_Tn => aL2_Tn
       aP2_nn => aL2_nn
    else if ( present(a_T) .or. present(a_V).or. present(a_n)) then
       difflevel = 1
       aP1_T => aL1_T
       aP1_V => aL1_V
       aP1_n => aL1_n
       aP2_T => aL2_T
       aP2_V => aL2_V
       aP2_n => aL2_n
    else
       difflevel = 0
    endif
    ! Calculate packing fraction
    call calc_Santos_eta(nc,n,V,difflevel,s_vc%dhs,s_vc%eta_hs)

    ! Calculate virial coefficients
    call calc_hardsphere_virial_B2(nc,n,s_vc%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
    call calc_hardsphere_virial_B3(nc,n,s_vc%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)

    ! Get first part of Santos Helmholtz energy (F11)
    call calc_F11_Santos(nc,s_vc%eta_hs,a,a_T,a_V,a_n,&
         a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)

    ! Second term (F12)
    call calc_Santos_F12_or_F22(nc,n,s_vc%dhs,.true.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
         B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
         aP1,F12_T=aP1_T,F12_V=aP1_V,F12_n=aP1_n,&
         F12_TT=aP1_TT,F12_TV=aP1_TV,F12_Tn=aP1_Tn,F12_VV=aP1_VV,&
         F12_Vn=aP1_Vn,F12_nn=aP1_nn)

    ! Combine terms
    call calc_a0_a_product(nc,aP1,a,&
         a0_T=aP1_T,a0_V=aP1_V,a0_n=aP1_n,&
         a_T=a_T,a_V=a_V,a_n=a_n,&
         a0_TT=aP1_TT,a0_VV=aP1_VV,a0_TV=aP1_TV,a0_Tn=aP1_Tn,a0_Vn=aP1_Vn,a0_nn=aP1_nn,&
         a_TT=a_TT,a_VV=a_VV,a_TV=a_TV,a_Tn=a_Tn,a_Vn=a_Vn,a_nn=a_nn)

    ! Get pure fluid Helmholtz energy (F21)
    call calc_pure_ahs_div_nRT(nc,s_vc%eta_hs,aP1,aP1_T,aP1_V,aP1_n,&
         aP1_TT,aP1_TV,aP1_Tn,aP1_VV,aP1_Vn,aP1_nn)

    ! Fourth term (F22)
    call calc_Santos_F12_or_F22(nc,n,s_vc%dhs,.false.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
         B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
         aP2,F12_T=aP2_T,F12_V=aP2_V,F12_n=aP2_n,&
         F12_TT=aP2_TT,F12_TV=aP2_TV,F12_Tn=aP2_Tn,F12_VV=aP2_VV,&
         F12_Vn=aP2_Vn,F12_nn=aP2_nn)

    ! Combine terms
    call calc_a0_a_product(nc,aP1,aP2,&
         a0_T=aP1_T,a0_V=aP1_V,a0_n=aP1_n,&
         a_T=aP2_T,a_V=aP2_V,a_n=aP2_n,&
         a0_TT=aP1_TT,a0_VV=aP1_VV,a0_TV=aP1_TV,a0_Tn=aP1_Tn,a0_Vn=aP1_Vn,a0_nn=aP1_nn,&
         a_TT=aP2_TT,a_VV=aP2_VV,a_TV=aP2_TV,a_Tn=aP2_Tn,a_Vn=aP2_Vn,a_nn=aP2_nn)

    call calc_a0_plus_a1(nc,aP2,a,&
         a0_T=aP2_T,a0_V=aP2_V,a0_n=aP2_n,&
         a1_T=a_T,a1_V=a_V,a1_n=a_n,&
         a0_TT=aP2_TT,a0_VV=aP2_VV,a0_TV=aP2_TV,a0_Tn=aP2_Tn,a0_Vn=aP2_Vn,a0_nn=aP2_nn,&
         a1_TT=a_TT,a1_VV=a_VV,a1_TV=a_TV,a1_Tn=a_Tn,a1_Vn=a_Vn,a1_nn=a_nn)

    ! Divide by sum n*ms, to counteract multiplication in saftvrmie_interface
    ns = sum(n*saftvrmie_param%ms)
    a = a/ns
    if (present(a_TT)) then
       a_TT=a_TT/ns
    endif
    if (present(a_T)) then
       a_T=a_T/ns
    endif
    if (present(a_V)) then
       a_V=a_V/ns
    endif
    if (present(a_VV)) then
       a_VV=a_VV/ns
    endif
    if (present(a_TV)) then
       a_TV=a_TV/ns
    endif
    if (present(a_Tn)) then
       a_Tn=(a_Tn-saftvrmie_param%ms*a_T)/ns
    endif
    if (present(a_Vn)) then
       a_Vn=(a_Vn-saftvrmie_param%ms*a_V)/ns
    endif
    if (present(a_n)) then
       a_n=(a_n-saftvrmie_param%ms*a)/ns
    endif
    if (present(a_nn)) then
       do i=1,nc
          do j=1,nc
             a_nn(i,j)=(a_nn(i,j)-saftvrmie_param%ms(i)*a_n(j)-saftvrmie_param%ms(j)*a_n(i))/ns
          enddo
       enddo
    endif

  end subroutine calc_hardsphere_helmholtzenergy_santos

  subroutine calc_Santos_F12_or_F22(nc,n,dhs,is21,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       F12,F12_T,F12_V,F12_n,&
       F12_TT,F12_TV,F12_Tn,F12_VV,F12_Vn,F12_nn)
    !------------------------------------------------------------------------
    !  2018-12-06 Morten Hammer
    ! Combine second and third virial coeffs
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    real, intent(in) :: n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    logical, intent(in) :: is21
    real, intent(in) :: B2s,B2s_T,B2s_TT,B2s_Tn(nc),B2s_n(nc),B2s_nn(nc,nc)
    real, intent(in) :: B3s,B3s_T,B3s_TT,B3s_Tn(nc),B3s_n(nc),B3s_nn(nc,nc)
    real, intent(out) :: F12         !< reduced helmholtz energy [-]
    real, intent(out), optional :: F12_T,F12_V,F12_n(nc)           !< derivatives
    real, intent(out), optional :: F12_VV,F12_TV,F12_Vn(nc)        !< derivatives
    real, intent(out), optional :: F12_TT,F12_Tn(nc),F12_nn(nc,nc) !< derivatives
    ! Locals
    real, parameter :: b2_p = 4, b3_p = 10
    real :: b2, b3
    real :: b3mb2, nd3(0:2), denum, d3(nc), d(nc), d_T(nc)
    integer :: i, j
    if (is21) then
       b2 = b2_p
       b3 = b3_p
    else
       b2 = -1
       b3 = -1
    endif
    b3mb2 = b3_p - b2_p
    nd3 = 0
    do i=1,nc
       d(i) = dhs%d(i,i)
       d3(i) = d(i)**3
       nd3(0) = nd3(0) + n(i)*d3(i)
       d_T(i) = dhs%d_T(i,i)
       nd3(1) = nd3(1) + 3*n(i)*dhs%d(i,i)**2*dhs%d_T(i,i)
       nd3(2) = nd3(2) + 3*n(i)*(2*dhs%d(i,i)*dhs%d_T(i,i)**2 + dhs%d(i,i)**2*dhs%d_TT(i,i))
    enddo
    denum = b3mb2*nd3(0)**2
    F12 = (b3*nd3(0)*B2s-b2*B3s)/denum
    if (present(F12_n)) then
       F12_n = (b3*d3*B2s + b3*nd3(0)*B2s_n - b2*B3s_n - 2*b3mb2*nd3(0)*d3*F12)/denum
    endif
    if (present(F12_T)) then
       F12_T = (b3*nd3(1)*B2s + b3*nd3(0)*B2s_T - b2*B3s_T - 2*b3mb2*nd3(0)*nd3(1)*F12)/denum
    endif
    if (present(F12_TT)) then
       F12_TT = (b3*nd3(2)*B2s + 2*b3*nd3(1)*B2s_T + b3*nd3(0)*B2s_TT - b2*B3s_TT &
            - 2*b3mb2*nd3(1)**2*F12 - 2*b3mb2*nd3(0)*nd3(2)*F12 &
            - 4*b3mb2*nd3(0)*nd3(1)*F12_T)/denum
    endif
    if (present(F12_Tn)) then
       F12_Tn = (3*b3*d**2*d_T*B2s + b3*d3*B2s_T + b3*nd3(1)*B2s_n + b3*nd3(0)*B2s_Tn &
            - b2*B3s_Tn - 2*b3mb2*nd3(1)*d3*F12 - 6*b3mb2*nd3(0)*d**2*d_T*F12 &
            - 2*b3mb2*nd3(0)*d3*F12_T - 2*b3mb2*nd3(0)*nd3(1)*F12_n)/denum
    endif
    if (present(F12_nn)) then
       do i=1,nc
          do j=1,nc
             F12_nn(i,j) = (b3*d3(i)*B2s_n(j) + b3*d3(j)*B2s_n(i) + b3*nd3(0)*B2s_nn(i,j) &
                  - b2*B3s_nn(i,j) - 2*b3mb2*d3(i)*d3(j)*F12 &
                  - 2*b3mb2*nd3(0)*d3(j)*F12_n(i) &
                  - 2*b3mb2*nd3(0)*d3(i)*F12_n(j))/denum
          enddo
       enddo
    endif

    if (present(F12_V)) then
       F12_V = 0.0
    endif
    if (present(F12_VV)) then
       F12_VV = 0.0
    endif
    if (present(F12_TV)) then
       F12_TV = 0.0
    endif
    if (present(F12_Vn)) then
       F12_Vn = 0.0
    endif
  end subroutine calc_Santos_F12_or_F22

  subroutine calc_hardsphere_virial_B2(nc,n,dhs,B2s,B2s_T,B2s_TT,B2s_Tk,B2s_k,B2s_kl)
    !------------------------------------------------------------------------
    !  2018-12-05 Aasmund Ervik
    !  The B3-star term in the Santos NAHS EoS
    !------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: n(nc)
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    real, intent(out) :: B2s,B2s_T,B2s_TT
    real, dimension(nc), intent(out) :: B2s_Tk,B2s_k
    real, dimension(nc,nc), intent(out) :: B2s_kl
    ! Locals
    real, parameter :: prefactor = 4
    real, dimension(nc,nc) :: d, d_T, d_TT
    integer :: i,j

    ! Copy these guys into arrays with shorter names for convenience/readability
    d(:,:) = dhs%d(:,:) !* 1e10
    d_T(:,:) = dhs%d_T(:,:) !* 1e10
    d_TT(:,:) = dhs%d_TT(:,:) !* 1e10

    ! Zero out the things, then add up the terms in the sums
    B2s = 0.0
    B2s_T = 0.0
    B2s_TT = 0.0
    B2s_Tk = 0.0
    B2s_k = 0.0
    B2s_kl = 0.0
    do i=1,nc
       do j=1,nc
          B2s = B2s + prefactor*n(i)*n(j)*d(i,j)**3
          b2s_T = B2s_T + 3*prefactor*n(i)*n(j)*d(i,j)**2*d_T(i,j)
          B2s_TT = B2s_TT + 3*prefactor*n(i)*n(j)*(2.0*d(i,j)*d_T(i,j)**2 + d(i,j)**2*d_TT(i,j))
          ! We can do a shortcut here and use i for the k which is in the memo, etc.
          B2s_Tk(i) = B2s_Tk(i) + 6*prefactor*n(j)*d(i,j)**2*d_T(i,j)
          B2s_k(i) = B2s_k(i) + 2*prefactor*n(j)*d(i,j)**3
          B2s_kl(i,j) = 2*prefactor*d(i,j)**3
       enddo
    enddo

  end subroutine calc_hardsphere_virial_B2

  subroutine calc_hardsphere_virial_B3(nc,n,dhs,B3s,B3s_T,B3s_TT,B3s_Tl,B3s_l,B3s_lm)
    !------------------------------------------------------------------------
    !  2018-12-05 Aasmund Ervik
    !  The B3-star term in the Santos NAHS EoS
    !------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: n(nc)
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    real, intent(out) :: B3s,B3s_T,B3s_TT
    real, dimension(nc), intent(out) :: B3s_Tl,B3s_l
    real, dimension(nc,nc), intent(out) :: B3s_lm
    ! Locals
    real, dimension(nc,nc,nc) :: B_ijk, B_ijk_T, B_ijk_TT
    real, parameter :: prefactor = 1.0
    integer :: i,j,k,l,m

    ! Get the Bijks
    call calc_hardsphere_virial_Bijk(nc,dhs,B_ijk,B_ijk_T,B_ijk_TT)

    ! Zero out the things, then add up the terms in the sums
    B3s = 0.0
    B3s_T = 0.0
    B3s_TT = 0.0
    B3s_Tl = 0.0
    B3s_l = 0.0
    B3s_lm = 0.0
    do i=1,nc
       do j=1,nc
          do k=1,nc
             B3s = B3s + prefactor*n(i)*n(j)*n(k)*B_ijk(i,j,k)
             B3s_T = B3s_T + prefactor*n(i)*n(j)*n(k)*B_ijk_T(i,j,k)
             B3s_TT = B3s_TT + prefactor*n(i)*n(j)*n(k)*B_ijk_TT(i,j,k)
          enddo
       enddo
    enddo
    do l=1,nc
       do j=1,nc
          do k=1,nc
             B3s_Tl(l) = B3s_Tl(l) + prefactor*n(j)*n(k)*B_ijk_T(l,j,k)
             B3s_l(l) = B3s_l(l) + prefactor*n(j)*n(k)*B_ijk(l,j,k)
          enddo
       enddo
       do i=1,nc
          do k=1,nc
             B3s_Tl(l) = B3s_Tl(l) + prefactor*n(i)*n(k)*B_ijk_T(i,l,k)
             B3s_l(l) = B3s_l(l) + prefactor*n(i)*n(k)*B_ijk(i,l,k)
          enddo
       enddo
       do i=1,nc
          do j=1,nc
             B3s_Tl(l) = B3s_Tl(l) + prefactor*n(i)*n(j)*B_ijk_T(i,j,l)
             B3s_l(l) = B3s_l(l) + prefactor*n(i)*n(j)*B_ijk(i,j,l)
          enddo
       enddo
    enddo
    do m=1,nc
       do l=1,nc
          do k=1,nc
             B3s_lm(l,m) = B3s_lm(l,m) + prefactor*(n(k)*B_ijk(l,m,k) + n(k)*B_ijk(m,l,k))
          enddo
          do j=1,nc
             B3s_lm(l,m) = B3s_lm(l,m) + prefactor*(n(j)*B_ijk(m,j,l) + n(j)*B_ijk(l,j,m))
          enddo
          do i=1,nc
             B3s_lm(l,m) = B3s_lm(l,m) + prefactor*(n(i)*B_ijk(i,l,m) + n(i)*B_ijk(i,m,l))
          enddo
       enddo
    enddo

  end subroutine calc_hardsphere_virial_B3

  subroutine calc_hardsphere_virial_Bijk(nc,dhs,B_ijk,B_ijk_T,B_ijk_TT)
    !------------------------------------------------------------------------
    !  2018-12-04 Aasmund Ervik
    ! The temperature-dependent contribution to the sum which gives the third
    ! virial coefficient in the Santos hard sphere EoS, and the first and
    ! second temperature derivatives
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    real,dimension(nc,nc,nc), intent(out) :: B_ijk !< The temperature dependent part of the third virial coeff
    real,dimension(nc,nc,nc), intent(out) :: B_ijk_T !< Its first derivate wrt T
    real,dimension(nc,nc,nc), intent(out) :: B_ijk_TT !< Its second derivate wrt T

    real, dimension(nc,nc,nc) :: d3, d3_T, d3_TT
    real, dimension(nc,nc,nc) :: c3, c3_T, c3_TT
    real, dimension(nc,nc) :: d, d_T, d_TT
    integer :: i,j,k

    ! Copy these guys into arrays with shorter names for convenience/readability
    d(:,:) = dhs%d(:,:) !* 1e10
    d_T(:,:) = dhs%d_T(:,:) !* 1e10
    d_TT(:,:) = dhs%d_TT(:,:) !* 1e10

    ! Compute the d3 (and derivs wrt T) which enter into the coefficients c3
    do i=1,nc
       do j=1,nc
          do k=1,nc
             d3(k,i,j)    = d(i,k)    + d(j,k)    - d(i,j)
             if (d3(k,i,j) > 0) then
                d3_T(k,i,j)  = d_T(i,k)  + d_T(j,k)  - d_T(i,j)
                d3_TT(k,i,j) = d_TT(i,k) + d_TT(j,k) - d_TT(i,j)
             else
                d3(k,i,j)    = 0.0
                d3_T(k,i,j)  = 0.0
                d3_TT(k,i,j) = 0.0
             endif
          enddo
       enddo
    enddo

    ! Compute the c3 coefficents (and derivs wrt T) given by the formula
    ! c_kij = d_kij^3 + 1.5 d_kij^2/d_ij * d_ijk * d_jik
    do i=1,nc
       do j=1,nc
          do k=1,nc
             c3(k,i,j) = d3(k,i,j)**3 + 1.5*d3(k,i,j)**2*d3(i,j,k)*d3(j,i,k)/d(i,j)

             c3_T(k,i,j) = 3.0*d3(k,i,j)**2*d3_T(k,i,j) + &
                  1.5*( ( 2.0*d3(k,i,j)*d3_T(k,i,j)*d(i,j) - d3(k,i,j)**2*d_T(i,j) )&
                  *d3(i,j,k)*d3(j,i,k)/d(i,j)**2 &
                  + d3(k,i,j)**2/d(i,j)*( d3_T(i,j,k)*d3(j,i,k) + d3(i,j,k)*d3_T(j,i,k) ) &
                  )

             c3_TT(k,i,j) = 6.0*d3(k,i,j)*d3_T(k,i,j)**2 + 3.0*d3(k,i,j)**2*d3_TT(k,i,j) + &
                  1.5*(  ( 2.0*d3(k,i,j)*d3_TT(k,i,j) + 2.0*d3_T(k,i,j)**2 - &
                  2.0*d_T(i,j)*(  2.0*d3(k,i,j)*d3_T(k,i,j)*d(i,j) - d3(k,i,j)**2*d_T(i,j) )/d(i,j)**2 - &
                  d3(k,i,j)**2/d(i,j)*d_TT(i,j) &
                  )*d3(i,j,k)*d3(j,i,k)/d(i,j) &
                  + 2.0*( 2.0*d3(k,i,j)*d3_T(k,i,j)*d(i,j) - d3(k,i,j)**2*d_T(i,j) )/d(i,j)**2&
                  *( d3_T(i,j,k)*d3(j,i,k) + d3(i,j,k)*d3_T(j,i,k) ) &
                  + d3(k,i,j)**2/d(i,j)&
                  *( d3_TT(i,j,k)*d3(j,i,k) + 2.0*d3_T(i,j,k)*d3_T(j,i,k) + d3(i,j,k)*d3_TT(j,i,k)) &
                  )

          enddo
       enddo
    enddo

    ! Sum up the c3 coefficients to get B_ijk (and derivs wrt T)
    ! B_ijk = 4/3 (c_kij d_ij^3 + c_jik d_ik^3 + c_ijk d_jk^3)
    ! NB! Note that the d's in this formula are not the d3's, but the dhs%d etc.
    do i=1,nc
       do j=1,nc
          do k=1,nc
             B_ijk(i,j,k) = 4.0/3.0 *&
                  (c3(k,i,j)*d(i,j)**3 + &
                  c3(j,i,k)*d(i,k)**3 + &
                  c3(i,j,k)*d(j,k)**3)

             B_ijk_T(i,j,k) = 4.0/3.0 *&
                  (c3(k,i,j)*3.0*d(i,j)**2*d_T(i,j) + &
                  c3(j,i,k)*3.0*d(i,k)**2*d_T(i,k) + &
                  c3(i,j,k)*3.0*d(j,k)**2*d_T(j,k) + &
                  c3_T(k,i,j)*d(i,j)**3 + &
                  c3_T(j,i,k)*d(i,k)**3 + &
                  c3_T(i,j,k)*d(j,k)**3)

             B_ijk_TT(i,j,k) = 4.0/3.0 *&
                  (c3(k,i,j)*(3.0*d(i,j)**2*d_TT(i,j) + 6.0*d(i,j)*d_T(i,j)**2) + &
                  c3(j,i,k)*(3.0*d(i,k)**2*d_TT(i,k) + 6.0*d(i,k)*d_T(i,k)**2) + &
                  c3(i,j,k)*(3.0*d(j,k)**2*d_TT(j,k) + 6.0*d(j,k)*d_T(j,k)**2) + &
                  c3_T(k,i,j)*6.0*d(i,j)**2*d_T(i,j) + &
                  c3_T(j,i,k)*6.0*d(i,k)**2*d_T(i,k) + &
                  c3_T(i,j,k)*6.0*d(j,k)**2*d_T(j,k) + &
                  c3_TT(k,i,j)*d(i,j)**3 + &
                  c3_TT(j,i,k)*d(i,k)**3 + &
                  c3_TT(i,j,k)*d(j,k)**3)
          enddo
       enddo
    enddo

    ! Et fine.
  end subroutine calc_hardsphere_virial_Bijk

  subroutine calc_hardsphere_helmholtzenergy_pure(nc,T,V,n,s_vc,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    !------------------------------------------------------------------------
    !> 2018-12-10 Morten Hammer
    !! Pure HS reference using double mol fraction sum of dij for d in the packing fraction
    !!
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    type(saftvrmie_var_container), intent(inout) :: s_vc
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !< derivatives
    ! Locals
    integer :: difflevel

    if ( present(a_TT) .or. present(a_VV).or. present(a_TV) .or. &
         present(a_nn) .or. present(a_Tn).or. present(a_Vn)) then
       difflevel = 2
    else if ( present(a_T) .or. present(a_V).or. present(a_n)) then
       difflevel = 1
    else
       difflevel = 0
    endif
    ! Calculate packing fraction
    call calc_eta_dij(nc,n,V,difflevel,s_vc%dhs,s_vc%eta_hs)

    ! Get pure fluid Helmholtz energy
    call calc_pure_ahs_div_nRT(nc,s_vc%eta_hs,a,a_T,a_V,a_n,&
         a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)

  end subroutine calc_hardsphere_helmholtzenergy_pure

  subroutine calc_eta_dij(nc,n,V,difflevel,dhs,eta)
    !------------------------------------------------------------------------
    !> 2018-12-04 Morten Hammer
    !! Packing fraction using double sum of dij for d
    !!
    !----------------------------------------------------------------------------
    type(saftvrmie_zeta), intent(inout) :: eta !< The packing fraction with differentials
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    integer, intent(in) :: nc
    real, intent(in) :: n(nc) !< Mol numbers
    real, intent(in) :: V !< Volume
    integer, intent(in) :: difflevel !< Level of differentials (0-2)
    ! Locals
    real :: prefactor, d(0:2), d3(0:2), ns, di(nc,0:1)
    integer :: i, j
    ns = sum(n*saftvrmie_param%ms)
    di = 0.0
    d = 0.0
    do i=1,nc
       do j=1,nc
          d(0) = d(0) + saftvrmie_param%ms(i)*n(i)*saftvrmie_param%ms(j)*n(j)*dhs%d(i,j)
          d(1) = d(1) + saftvrmie_param%ms(i)*n(i)*saftvrmie_param%ms(j)*n(j)*dhs%d_T(i,j)
          d(2) = d(2) + saftvrmie_param%ms(i)*n(i)*saftvrmie_param%ms(j)*n(j)*dhs%d_TT(i,j)
          di(i,0) = di(i,0) + 2*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*n(j)*dhs%d(i,j)
          di(i,1) = di(i,1) + 2*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*n(j)*dhs%d_T(i,j)
       enddo
    enddo
    d3(0) = d(0)**3
    d3(1) = 3*d(0)**2*d(1)
    d3(2) = 3*d(0)*(2.0*d(1)**2 + d(0)*d(2))
    prefactor = pi*N_AVOGADRO/(6.0*V)
    eta%zx = prefactor*d3(0)/ns**5
    if (difflevel > 0) then
       eta%zx_T = prefactor*d3(1)/ns**5
       eta%zx_V = -eta%zx/V
       eta%zx_n = (prefactor*3*d(0)**2*di(:,0) - 5*saftvrmie_param%ms*ns**4*eta%zx)/ns**5
    endif
    if (difflevel > 1) then
       eta%zx_TT = prefactor*d3(2)/ns**5
       eta%zx_VV = 2.0*eta%zx/V**2
       eta%zx_TV = -eta%zx_T/V
       eta%zx_Vn = -eta%zx_n/V
       eta%zx_Tn = (prefactor*3*d(0)*(2*di(:,0)*d(1) + d(0)*di(:,1))  &
            - 5*saftvrmie_param%ms*ns**4*eta%zx_T)/ns**5
       do i=1,nc
          do j=1,nc
             eta%zx_nn(i,j) = (prefactor*6*d(0)*(di(i,0)*di(j,0) &
                  + d(0)*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*dhs%d(i,j)) &
                  - 20*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*ns**3*eta%zx &
                  - 5*saftvrmie_param%ms(i)*ns**4*eta%zx_n(j)  &
                  - 5*saftvrmie_param%ms(j)*ns**4*eta%zx_n(i))/ns**5
          enddo
       enddo
       !
       eta%zx_VVV = -6*eta%zx/V**3
       eta%zx_VVT = 2*eta%zx_T/V**2
       eta%zx_VTn = -eta%zx_Tn/V
       eta%zx_VVn = 2*eta%zx_n/V**2
       eta%zx_Vnn = -eta%zx_nn/V
       eta%zx_VTT = -eta%zx_TT/V
    endif
  end subroutine calc_eta_dij

  subroutine calc_d_pure(nc,n,V,difflevel,dhs,d_pure)
    !------------------------------------------------------------------------
    !> 2019-02-26 Morten Hammer
    !! Pure fluid hard-sphere diameter calculated as double sum of dij
    !!
    !----------------------------------------------------------------------------
    type(saftvrmie_zeta), intent(inout) :: d_pure !< The packing fraction with differentials
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    integer, intent(in) :: nc
    real, intent(in) :: n(nc) !< Mol numbers
    real, intent(in) :: V !< Volume
    integer, intent(in) :: difflevel !< Level of differentials (0-2)
    ! Locals
    real :: d(0:2), ns, di(nc,0:1)
    integer :: i, j
    ns = sum(n*saftvrmie_param%ms)
    di = 0.0
    d = 0.0
    do i=1,nc
       do j=1,nc
          d(0) = d(0) + saftvrmie_param%ms(i)*n(i)*saftvrmie_param%ms(j)*n(j)*dhs%d(i,j)
          d(1) = d(1) + saftvrmie_param%ms(i)*n(i)*saftvrmie_param%ms(j)*n(j)*dhs%d_T(i,j)
          d(2) = d(2) + saftvrmie_param%ms(i)*n(i)*saftvrmie_param%ms(j)*n(j)*dhs%d_TT(i,j)
          di(i,0) = di(i,0) + 2*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*n(j)*dhs%d(i,j)
          di(i,1) = di(i,1) + 2*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*n(j)*dhs%d_T(i,j)
       enddo
    enddo
    d_pure%zx = d(0)/ns**2
    if (difflevel > 0) then
       d_pure%zx_T = d(1)/ns**2
       d_pure%zx_V = 0.0
       d_pure%zx_n = (di(:,0) - 2*saftvrmie_param%ms(:)*ns*d_pure%zx)/ns**2
    endif
    if (difflevel > 1) then
       d_pure%zx_TT = d(2)/ns**2
       d_pure%zx_VV = 0.0
       d_pure%zx_TV = 0.0
       d_pure%zx_Vn = 0.0
       d_pure%zx_Tn = (di(:,1) - 2*saftvrmie_param%ms(:)*ns*d_pure%zx_T)/ns**2
       do i=1,nc
          do j=1,nc
             d_pure%zx_nn(i,j) = 2*(saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*dhs%d(i,j) &
                  - saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*d_pure%zx &
                  - saftvrmie_param%ms(i)*ns*d_pure%zx_n(j)  &
                  - saftvrmie_param%ms(j)*ns*d_pure%zx_n(i))/ns**2
          enddo
       enddo
       !
       d_pure%zx_VVV = 0.0
       d_pure%zx_VVT = 0.0
       d_pure%zx_VTn = 0.0
       d_pure%zx_VVn = 0.0
       d_pure%zx_Vnn = 0.0
       d_pure%zx_VTT = 0.0
    endif
  end subroutine calc_d_pure

  subroutine calc_hardsphere_rdf_and_U(nc,T,n,V,r,s_vc,g,U_divk)
    !------------------------------------------------------------------------
    !  2018-02-27, Oivind Wilhelmsen
    ! The reduced Helmholtz energy of the hard-sphere term and its derivatives
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,n(nc),V  !< temperature [K]
    real, intent(in) :: r  ! radial position
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(out) :: g  ! pair-correlation function
    real, intent(out) :: U_divk  ! intermolecular potential

    ! Variables used internally
    real :: U_divk_T,U_divk_TT
    real :: U_divk_r, U_divk_Tr, U_divk_rr
    real :: eta
    integer :: j

    if (nc>1) then
       call stoperror("This test is only made for one component")
    end if

    j=1
    call calc_mie_potential_quantumcorrected(j,j,s_vc,&
         saftvrmie_param%comp(j)%sigma,saftvrmie_param%comp(j)%eps_depth_divk,&
         saftvrmie_param%comp(j)%lambda_a,saftvrmie_param%comp(j)%lambda_r,&
         saftvrmie_param%Cij(j,j),&
         saftvrmie_param%Quantum_const_1a_ij(j,j),&
         saftvrmie_param%Quantum_const_1r_ij(j,j),&
         saftvrmie_param%Quantum_const_2a_ij(j,j),&
         saftvrmie_param%Quantum_const_2r_ij(j,j),&
         r,U_divk,U_divk_T,U_divk_TT,U_divk_r,U_divk_Tr,U_divk_rr)

    eta= pi*N_AVOGADRO*n(1)*(r**3)*saftvrmie_param%ms(1)/(6.0*V)
    g=(1.0-0.5*eta)/((1.0-eta)**3)

  end subroutine calc_hardsphere_rdf_and_U

  !--------------------------------------------------------------------
  !> This subroutine computes the radius differential of the
  !! scaled interaction potential U/epsilon (dUdx(r=x sigma)/eps)
  !! \author Morten Hammer, March 2018
  !---------------------------------------------------------------------
  subroutine epseff_Ux(f,x,param)
    implicit none
    real, intent(out) :: f(1) !< Function values
    real, intent(in) :: x(1) !< Variable vector
    real, dimension(10), intent(in) :: param !< Parameter vector
    ! Local variables
    real :: n, m
    real :: mie_c_factor_par, D, D2
    real :: s_1, s_n, s_m, Ux
    real :: Q1_n, Q2_n, Q1_m, Q2_m
    real :: s_2, s_4, U_q1_x, U_q2_x

    ! Extract the parameters
    n=param(1)                  ! Repulsive exponent of Mie potential
    m=param(2)                  ! Attractive exponent of Mie potential
    !sigma_Mie=param(3)          ! The sigma from the Mie potential
    mie_c_factor_par=param(4)   ! Prefactor
    D=param(5)                  ! Quantum parameter
    D2=param(6)                 ! Quantum parameter squared
    Q1_m=param(7)               ! Prefactor 1-order quantum correction
    Q1_n=param(8)               ! Prefactor 1-order quantum correction
    Q2_m=param(9)               ! Prefactor 2-order quantum correction
    Q2_n=param(10)              ! Prefactor 2-order quantum correction

    ! The inverse radii
    s_1=1.0/x(1)
    s_n=(s_1)**n
    s_m=(s_1)**m

    ! Zeroth order contribution
    Ux=(-n*s_n+m*s_m)*s_1

    ! Add the first order quantum correction
    if (quantum_correction_hs>0) then
       s_2=(s_1)**2
       U_q1_x=D*s_1*s_2*(-(n+2)*Q1_n*s_n+(m+2)*Q1_m*s_m)
       Ux=Ux+U_q1_x
    end if

    ! Add the second order quantum correction
    if (quantum_correction_hs>1) then
       s_4=s_2**2
       U_q2_x=D2*s_1*s_4*(-(n+4)*Q2_n*s_n+(m+4)*Q2_m*s_m)
       Ux=Ux+U_q2_x
    end if

    ! Multiply everything by the prefactor:
    f=Ux*mie_c_factor_par                    ! U/epsilon
  end subroutine epseff_Ux

  !--------------------------------------------------------------------
  !> This subroutine computes the second radius differential of the
  !! scaled interaction potential U/epsilon (d2Udx2(r=x sigma)/eps)
  !! \author Morten Hammer, March 2018
  !---------------------------------------------------------------------
  subroutine epseff_Uxx(J,x,param)
    implicit none
    real, intent(out) :: J(1,1) !< Differential value
    real, intent(in) :: x(1) !< Variable vector
    real, dimension(10), intent(in) :: param !< Parameter vector
    ! Local variables
    real :: n, m
    real :: mie_c_factor_par, D, D2
    real :: s_1, s_n, s_m, Ux
    real :: Q1_n, Q2_n, Q1_m, Q2_m
    real :: s_2, s_4, U_q1_x, U_q2_x

    ! Extract the parameters
    n=param(1)                  ! Repulsive exponent of Mie potential
    m=param(2)                  ! Attractive exponent of Mie potential
    !sigma_Mie=param(3)          ! The sigma from the Mie potential
    mie_c_factor_par=param(4)   ! Prefactor
    D=param(5)                  ! Quantum parameter
    D2=param(6)                 ! Quantum parameter squared
    Q1_m=param(7)               ! Prefactor 1-order quantum correction
    Q1_n=param(8)               ! Prefactor 1-order quantum correction
    Q2_m=param(9)               ! Prefactor 2-order quantum correction
    Q2_n=param(10)              ! Prefactor 2-order quantum correction

    ! The inverse radii
    s_1=1.0/x(1)
    s_2=s_1**2
    s_n=(s_1)**n
    s_m=(s_1)**m

    ! Zeroth order contribution
    Ux=(n*(n+1.0)*s_n-m*(m+1.0)*s_m)*s_2

    ! Add the first order quantum correction
    if (quantum_correction_hs>0) then
       U_q1_x=D*s_2*s_2*((n+2)*(n+3)*Q1_n*s_n-(m+2)*(m+3)*Q1_m*s_m)
       Ux=Ux+U_q1_x
    end if

    ! Add the second order quantum correction
    if (quantum_correction_hs>1) then
       s_4=s_2**2
       U_q2_x=D2*s_2*s_4*((n+4)*(n+5)*Q2_n*s_n-(m+4)*(m+5)*Q2_m*s_m)
       Ux=Ux+U_q2_x
    end if

    ! Multiply everything by the prefactor:
    J=Ux*mie_c_factor_par                    ! U/epsilon
  end subroutine epseff_Uxx

  !--------------------------------------------------------------------
  !> This subroutine solves for the effective epsilon, which is defined as the
  !! minimum of the potential. Epsilon divided by Boltzmanns constant returned.
  !! Based on calc_effective_sigma
  !! \author Morten Hammer, March 2018
  !! \author Ailo Aasen,  October 2018
  !---------------------------------------------------------------------
  subroutine calc_binary_effective_eps_divk(nc,T,s_vc,eps_divk_eff,eps_divk_eff_T,eps_divk_eff_TT)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, limit_dx, &
         premReturn, setXv
    integer, intent(in) :: nc                        !< Number of components
    real, intent(in) :: T                            !< Temperature [K]
    type(saftvrmie_var_container), intent(in) :: s_vc
    real, intent(out) :: eps_divk_eff(nc,nc)            !< Effective epsilon [1/T]
    real, intent(out) :: eps_divk_eff_T(nc,nc)          !< T-deriv of effective epsilon [m/K]
    real, intent(out) :: eps_divk_eff_TT(nc,nc)         !< TT-deriv of effective sigma [m/K^2]
    type(nonlinear_solver) :: solver
    real :: D, D_T, D_TT, D2, D2_T, D2_TT            !< Quantum parameters and derivatives
    real :: U, U_T, U_TT, U_r, U_Tr, U_rr, U_rrr, U_Trr, U_TTr
    real :: param(10)
    real, dimension(1) :: x, xmin, xmax
    integer :: i, j
    real :: z, z_T, z_TT

    if (quantum_correction_hs==0) then ! With no quantum corrections
       eps_divk_eff=saftvrmie_param%eps_divk_ij
       eps_divk_eff_T=0.0
       eps_divk_eff_TT=0.0
       return
    else
       do i=1,nc
          do j=i,nc
             if (.not. exact_crosspot_eff .and. i/=j) then
                cycle
             end if

             ! Obtain the quantum-parameters
             call get_DFeynHibbsPower(i,j,D,D_T,D_TT,s_vc,power_in=1,divideBySigmaMie=.true.)
             call get_DFeynHibbsPower(i,j,D2,D2_T,D2_TT,s_vc,power_in=2,divideBySigmaMie=.true.)

             ! Construct the parameter vector
             param(1)=saftvrmie_param%lambda_r_ij(i,j)
             param(2)=saftvrmie_param%lambda_a_ij(i,j)
             param(3)=saftvrmie_param%sigma_ij(i,j)
             param(4)=saftvrmie_param%Cij(i,j)
             param(5)=D
             param(6)=D2
             param(7)=saftvrmie_param%Quantum_const_1a_ij(i,j)
             param(8)=saftvrmie_param%Quantum_const_1r_ij(i,j)
             param(9)=saftvrmie_param%Quantum_const_2a_ij(i,j)
             param(10)=saftvrmie_param%Quantum_const_2r_ij(i,j)

             ! Set the limits and the initial condition
             xmin=1.0
             xmax=10
             x=1.0
             solver%abs_tol = 1e-12

             ! NB, the variable we iterate on is x=r/sigma, i.e. a scaled radius
             ! The interaction potential is scaled with epsilon
             call nonlinear_solve(solver,epseff_Ux,epseff_Uxx,epseff_Uxx,limit_dx,&
                  premReturn,setXv,x,xmin,xmax,param)
             if (solver%exitflag /= 0 .and. verbose) then
                print *,"Not able to solve for effective epsilon"
             endif
             z=x(1)*saftvrmie_param%sigma_ij(i,j)

             call calc_mie_potential_quantumcorrected(i,j,s_vc,&
                  saftvrmie_param%sigma_ij(i,j),saftvrmie_param%eps_divk_ij(i,j),&
                  saftvrmie_param%lambda_a_ij(i,j),saftvrmie_param%lambda_r_ij(i,j),&
                  saftvrmie_param%Cij(i,j),&
                  saftvrmie_param%Quantum_const_1a_ij(i,j),&
                  saftvrmie_param%Quantum_const_1r_ij(i,j),&
                  saftvrmie_param%Quantum_const_2a_ij(i,j),&
                  saftvrmie_param%Quantum_const_2r_ij(i,j),&
                  z,U,U_divk_T=U_T,U_divk_TT=U_TT,&
                  U_divk_r=U_r,U_divk_Tr=U_Tr,&
                  U_divk_rr=U_rr,U_divk_rrr=U_rrr,&
                  U_divk_Trr=U_Trr,U_divk_TTr=U_TTr)

             z_T = -U_Tr/U_rr
             z_TT = -(U_TTr+U_rrr*z_T**2+2.0*U_Trr*z_T)/U_rr
             eps_divk_eff(i,j)=-U
             eps_divk_eff_T(i,j)=-(U_r*z_T+U_T)
             eps_divk_eff_TT(i,j)=-(U_rr*z_T**2+2.0*U_Tr*z_T+U_r*z_TT+U_TT)

             eps_divk_eff(j,i) = eps_divk_eff(i,j)
             eps_divk_eff_T(j,i) = eps_divk_eff_T(i,j)
             eps_divk_eff_TT(j,i) = eps_divk_eff_TT(i,j)
          end do
       end do

       if (.not. exact_crosspot_eff) then
          ! Use simplified combining rule for effective eps_divk. NOTE: this is
          ! probably a very bad simplified combining rule
          do i=1,nc
             do j=i+1,nc
                eps_divk_eff(i,j) = 0.5*(eps_divk_eff(i,i) + eps_divk_eff(j,j))
                eps_divk_eff_T(i,j) = 0.5*(eps_divk_eff_T(i,i) + eps_divk_eff_T(j,j))
                eps_divk_eff_TT(i,j) = 0.5*(eps_divk_eff_TT(i,i) + eps_divk_eff_TT(j,j))
                eps_divk_eff(j,i) = eps_divk_eff(i,j)
                eps_divk_eff_T(j,i) = eps_divk_eff_T(i,j)
                eps_divk_eff_TT(j,i) = eps_divk_eff_TT(i,j)
             end do
          end do
       end if

    end if
  end subroutine calc_binary_effective_eps_divk

  subroutine calc_Santos_eta(nc,n,V,difflevel,dhs,eta)
    !------------------------------------------------------------------------
    !> 2018-12-04 Morten Hammer
    !! Santos 2005 specific packing fraction
    !!
    !----------------------------------------------------------------------------
    type(saftvrmie_zeta), intent(inout) :: eta !< The packing fraction with differentials
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    integer, intent(in) :: nc
    real, intent(in) :: n(nc) !< Mol numbers
    real, intent(in) :: V !< Volume
    integer, intent(in) :: difflevel !< Level of differentials (0-2)
    ! Locals
    real :: nsum_d3(3), prefactor, d3(nc,2)
    integer :: i
    nsum_d3 = 0.0
    do i=1,nc
       d3(i,1) = dhs%d(i,i)**3
       d3(i,2) = 3.0*dhs%d(i,i)**2*dhs%d_T(i,i)
       nsum_d3(1) = nsum_d3(1) + n(i)*d3(i,1)
       nsum_d3(2) = nsum_d3(2) + n(i)*d3(i,2)
       nsum_d3(3) = nsum_d3(3) + 3.0*n(i)*(2.0*dhs%d(i,i)*dhs%d_T(i,i)**2 + dhs%d(i,i)**2*dhs%d_TT(i,i))
    enddo
    prefactor = pi*N_AVOGADRO/(6.0*V)
    eta%zx = prefactor*nsum_d3(1)
    if (difflevel > 0) then
       eta%zx_T = prefactor*nsum_d3(2)
       eta%zx_V = -eta%zx/V
       eta%zx_n = prefactor*d3(:,1)
    endif
    if (difflevel > 1) then
       eta%zx_TT = prefactor*nsum_d3(3)
       eta%zx_VV = 2.0*eta%zx/V**2
       eta%zx_TV = -eta%zx_T/V
       eta%zx_Vn = -eta%zx_n/V
       eta%zx_Tn = prefactor*d3(:,2)
       eta%zx_nn = 0.0
       ! Should not be used
       eta%zx_VVV = 0.0
       eta%zx_VVT = 0.0
       eta%zx_VTn = 0.0
       eta%zx_VVn = 0.0
       eta%zx_Vnn = 0.0
       eta%zx_VTT = 0.0
    endif
  end subroutine calc_Santos_eta

  subroutine calc_ahs_div_nRT_Santos_part(eta,ahs,ahs_e,ahs_ee)
    !------------------------------------------------------------------------
    !> 2018-12-04 Morten Hammer
    !! Helmholtz energy divided by n*R*T for the Santos 2005 specific
    !! eos for hard spheres
    !----------------------------------------------------------------------------
    real, intent(in) :: eta !< The packing fraction
    real, intent(out) :: ahs, ahs_e, ahs_ee !< The Helmholtz energy, packing fraction differentials
    ! Locals
    real :: one_m_eta
    one_m_eta = 1.0 - eta
    ahs = - log(one_m_eta)
    ahs_e = 1/one_m_eta
    ahs_ee = 1/one_m_eta**2
  end subroutine calc_ahs_div_nRT_Santos_part

  subroutine calc_ahs_div_nRT_CSK(eta,ahs,ahs_e,ahs_ee)
    !------------------------------------------------------------------------
    !> 2018-12-04 Morten Hammer
    !! Helmholtz energy divided by n*R*T for the Carnahan-Starling-Kolafa
    !! eos for hard spheres
    !----------------------------------------------------------------------------
    real, intent(in) :: eta !< The packing fraction
    real, intent(out) :: ahs, ahs_e, ahs_ee !< The Helmholtz energy, packing fraction differentials
    ! Locals
    real :: one_m_eta
    one_m_eta = 1.0 - eta
    ahs = (1.0/3.0)*(2.5/one_m_eta**2 + 10/one_m_eta + 2*eta + 5*log(one_m_eta))
    ahs_e = (1.0/3.0)*(12 -6*eta + eta**2 - 2*eta**3)/one_m_eta**3
    ahs_ee = -(5.0/3.0)*(-6+2*eta+eta**2)/one_m_eta**4
  end subroutine calc_ahs_div_nRT_CSK

  subroutine calc_ahs_div_nRT_CS(eta,ahs,ahs_e,ahs_ee)
    !------------------------------------------------------------------------
    !> 2018-12-04 Morten Hammer
    !! Helmholtz energy divided by n*R*T for the Carnahan-Starling
    !! eos for hard spheres
    !----------------------------------------------------------------------------
    real, intent(in) :: eta !< The packing fraction
    real, intent(out) :: ahs, ahs_e, ahs_ee !< The Helmholtz energy, packing fraction differentials
    ! Locals
    real :: one_m_eta
    one_m_eta = 1.0 - eta
    !ahs = (3-2*eta)/one_m_eta**2
    ahs = (4*eta-3*eta**2)/one_m_eta**2
    ahs_e = 2*(2-eta)/one_m_eta**3
    ahs_ee = (10-4*eta)/one_m_eta**4
  end subroutine calc_ahs_div_nRT_CS

  subroutine calc_pure_ahs_div_nRT_eta(y,a,a_y,a_yy)
    real, intent(in) :: y
    real, intent(out) :: a,a_y,a_yy
    if (pure_hs_EoS == PURE_HS_CSK) then
       call calc_ahs_div_nRT_CSK(y,a,a_y,a_yy)
    else if (pure_hs_EoS == PURE_HS_CS) then
       call calc_ahs_div_nRT_CS(y,a,a_y,a_yy)
    else
       call stoperror("Wrong pure hard-sphere model")
    endif
  end subroutine calc_pure_ahs_div_nRT_eta

  !> Intended for debugging
  function calc_z_ahs_pure(y) result(Zp)
    real, intent(in) :: y
    real :: Zp
    if (pure_hs_EoS == PURE_HS_CSK) then
       Zp = (1+y+y**2-2*y**3*(1+y)/3)/(1-y)**3
    else if (pure_hs_EoS == PURE_HS_CS) then
       Zp = (1+y+y**2-y**3)/(1-y)**3
    else
       Zp = 0.0
    endif
  end function calc_z_ahs_pure

  subroutine calc_pure_ahs_div_nRT(nc,eta,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    use saftvrmie_utils, only: convert_zeta_x_to_TVn
    integer, intent(in) :: nc
    type(saftvrmie_zeta), intent(in) :: eta !< Packing fraction
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !<derivatives
    ! Locals
    real :: ahs_e, ahs_ee
    call calc_pure_ahs_div_nRT_eta(eta%zx,a,ahs_e,ahs_ee)

    call convert_zeta_x_to_TVn(nc,1.0,0.0,0.0,eta,&
         a,ahs_e,0.0,ahs_ee,0.0,0.0,0.0,0.0,0.0,&
         a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn)
  end subroutine calc_pure_ahs_div_nRT

  subroutine calc_F11_Santos(nc,eta,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    use saftvrmie_utils, only: convert_zeta_x_to_TVn
    integer, intent(in) :: nc
    type(saftvrmie_zeta), intent(in) :: eta !< Packing fraction
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !<derivatives
    ! Locals
    real :: ahs_e, ahs_ee
    call calc_ahs_div_nRT_Santos_part(eta%zx,a,ahs_e,ahs_ee)

    call convert_zeta_x_to_TVn(nc,1.0,0.0,0.0,eta,&
         a,ahs_e,0.0,ahs_ee,0.0,0.0,0.0,0.0,0.0,&
         a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn)
  end subroutine calc_F11_Santos

  !> Function intended to reproduce plots of Santos 2005
  function calc_binary_Z_Santos(nc,x,eta,d1,d2,delta,s_vc) result(Z)
    integer, intent(in) :: nc
    real, intent(in) :: x(nc) ! Mol fracions
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: d1, d2
    real, intent(in) :: delta !<
    type(saftvrmie_var_container), intent(inout) :: s_vc
    ! Output
    real :: Z
    ! Locals
    real :: V, T, a, a_V
    if (nc /= 2) call stoperror("calc_binary_Z_Santos only intentded for nc=2")
    T = 1.0 ! Dummy
    ! Initialize dhs
    s_vc%dhs%d(1,1) = d1
    s_vc%dhs%d(2,2) = d2
    s_vc%dhs%d(1,2) = 0.5*(d1+d2)*(1+delta)
    s_vc%dhs%d(2,1) = s_vc%dhs%d(1,2)
    s_vc%dhs%d_T = 0.0
    s_vc%dhs%d_TT = 0.0
    ! Calculate V and eta differentials
    V = 1.0
    call calc_Santos_eta(nc,x,V,0,s_vc%dhs,s_vc%eta_hs)
    V = s_vc%eta_hs%zx/eta
    call calc_Santos_eta(nc,x,V,1,s_vc%dhs,s_vc%eta_hs)

    ! Calculate residual contribution to Z
    call calc_hardsphere_helmholtzenergy_santos(nc,T,V,x,s_vc,a,a_V=a_V)
    Z = 1 - a_V*V
  end function calc_binary_Z_Santos

end module saftvrmie_hardsphere

!subroutine test_hardsphere_helmholtzenergy
!  !use saftvrmie_hardsphere
!  use thermopack_var, only: nc
!  use saftvrmie_containers, only: saftvrmie_dhs, &
!       allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs
!  implicit none
!  real :: p,Z(nc),a
!  integer :: i, j
!  real :: T, T_p, T_m, dT, V, V_p, V_m, dV, num(nc),dn,num_p(nc),num_m(nc)
!  real :: zeta(4)
!  real :: a_T,a_V,a_n(nc), a_VV,a_TV,a_Vn(nc),a_TT,a_Tn(nc),a_nn(nc,nc)
!  real :: a_T_p,a_V_p,a_n_p(nc), a_p, a_m
!  real :: a_T_m,a_V_m,a_n_m(nc)
!  type(saftvrmie_dhs) :: dhs,sigma
!  Z = (/ 0.9, 0.1 /)
!  num = (/ 2.0, 3.0 /)
!  zeta = (/ 1.3, 1.1, 2.3, 0.2 /)
!  T = 20.0
!  dT =2E-5
!  T_p = T+dT
!  T_m = T-dT
!  p = 1.0e6
!  V = 1000
!  dV=1E-3
!  V_p=V+dV
!  V_m=V-dV
!  dN=dT
!  num_p=num
!  num_p(1)=num_p(1)+dN
!  num_m=num
!  num_m(1)=num_m(1)-dN
!  call allocate_saftvrmie_dhs(nc,dhs)
!  call allocate_saftvrmie_dhs(nc,sigma)
!  call calc_binary_effective_sigma(nc,T,sigma%d,sigma%d_T,sigma%d_TT)
!  call calc_hardsphere_diameter(nc,T,sigma%d,sigma%d_T,sigma%d_TT,dhs%d,dhs%d_T,dhs%d_TT)
!  call calc_hardsphere_helmholtzenergy(nc,T,V_p,num,dhs,sigma,a_p,a_T_p,a_V_p,a_n_p,&
!       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
!  call calc_hardsphere_helmholtzenergy(nc,T,V_m,num,dhs,sigma,a_m,a_T_m,a_V_m,a_n_m,&
!       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
!  call calc_hardsphere_helmholtzenergy(nc,T,V,num,dhs,sigma,a,a_T,a_V,a_n,&
!       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
!
!  do i=1,nc
!    do j=1,nc
!      num_p=num
!      num_p(j)=num_p(j)+dN
!      num_m=num
!      num_m(j)=num_m(j)-dN
!
!      call calc_hardsphere_helmholtzenergy(nc,T,V,num_p,dhs,sigma,a_p,&
!           a_T_p,a_V_p,a_n_p,a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
!      call calc_hardsphere_helmholtzenergy(nc,T,V,num_m,dhs,sigma,a_m,&
!           a_T_m,a_V_m,a_n_m,a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
!      call calc_hardsphere_helmholtzenergy(nc,T,V,num,dhs,sigma,a,a_T,a_V,&
!           a_n,a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
!
!      print *, (a_nn(i,j)-((a_n_p(i)-a_n_m(i))/(2*dN)))/a_nn(i,j)
!    end do
!  end do
!  call cleanup_saftvrmie_dhs(dhs)
!  call cleanup_saftvrmie_dhs(sigma)
!end subroutine test_hardsphere_helmholtzenergy

subroutine test_effective_eps_divk()
  use thermopack_constants
  use saftvrmie_hardsphere
  use saftvrmie_containers
  use thermopack_var
  implicit none
  integer :: i
  real :: T, eps_divk_eff(nc,nc),eps_divk_eff_T(nc,nc),eps_divk_eff_TT(nc,nc)
  real :: eps_divk_eff2(nc,nc),eps_divk_eff2_T(nc,nc),eps_divk_eff2_TT(nc,nc)
  real :: param(10),D,D_T,D_TT,D2,D2_T,D2_TT,f(1),f2(1),J(1,1), x(1),eps
  real :: r, r2
  real :: U,U_T,U_TT,U_r,U_Tr,U_rr,U_rrr,U_Trr,U_TTr
  real :: U2,U2_T,U2_TT,U2_r,U2_Tr,U2_rr,U2_rrr,U2_Trr,U2_TTr
  T = 10.0
  i=1
  call update_temp_variables(nc,T,saftvrmie_var(1))
  ! Obtain the quantum-parameters
  call get_DFeynHibbsPower(i,i,D,D_T,D_TT,saftvrmie_var(1),power_in=1,divideBySigmaMie=.true.)
  call get_DFeynHibbsPower(i,i,D2,D2_T,D2_TT,saftvrmie_var(1),power_in=2,divideBySigmaMie=.true.)

  ! Construct the parameter-file
  param(1)=saftvrmie_param%comp(i)%lambda_r
  param(2)=saftvrmie_param%comp(i)%lambda_a
  param(3)=saftvrmie_param%comp(i)%sigma
  param(4)=saftvrmie_param%Cij(i,i)
  param(5)=D
  param(6)=D2
  param(7)=saftvrmie_param%Quantum_const_1a_ij(i,i)
  param(8)=saftvrmie_param%Quantum_const_1r_ij(i,i)
  param(9)=saftvrmie_param%Quantum_const_2a_ij(i,i)
  param(10)=saftvrmie_param%Quantum_const_2r_ij(i,i)

  x = 1.2
  eps = 1.0e-8
  call epseff_Ux(f,x,param)
  !print *,x,f
  call epseff_Uxx(J,x,param)
  x = x + eps
  call epseff_Ux(f2,x,param)
  print *,J,(f2-f)/eps


  r=4.0105664157835918E-010*0.95
  call calc_mie_potential_quantumcorrected(i,i,saftvrmie_var(1),&
       saftvrmie_param%comp(i)%sigma,saftvrmie_param%comp(i)%eps_depth_divk,&
       saftvrmie_param%comp(i)%lambda_a,saftvrmie_param%comp(i)%lambda_r,&
       saftvrmie_param%Cij(i,i),&
       saftvrmie_param%Quantum_const_1a_ij(i,i),&
       saftvrmie_param%Quantum_const_1r_ij(i,i),&
       saftvrmie_param%Quantum_const_2a_ij(i,i),&
       saftvrmie_param%Quantum_const_2r_ij(i,i),&
       r,U,U_divk_T=U_T,U_divk_TT=U_TT,&
       U_divk_r=U_r,U_divk_Tr=U_Tr,&
       U_divk_rr=U_rr,U_divk_rrr=U_rrr,&
       U_divk_Trr=U_Trr,U_divk_TTr=U_TTr)
  r2=r+r*eps
  call calc_mie_potential_quantumcorrected(i,i,saftvrmie_var(1),&
       saftvrmie_param%comp(i)%sigma,saftvrmie_param%comp(i)%eps_depth_divk,&
       saftvrmie_param%comp(i)%lambda_a,saftvrmie_param%comp(i)%lambda_r,&
       saftvrmie_param%Cij(i,i),&
       saftvrmie_param%Quantum_const_1a_ij(i,i),&
       saftvrmie_param%Quantum_const_1r_ij(i,i),&
       saftvrmie_param%Quantum_const_2a_ij(i,i),&
       saftvrmie_param%Quantum_const_2r_ij(i,i),&
       r2,U2,U_divk_T=U2_T,U_divk_TT=U2_TT,&
       U_divk_r=U2_r,U_divk_Tr=U2_Tr,&
       U_divk_rr=U2_rr,U_divk_rrr=U2_rrr,&
       U_divk_Trr=U2_Trr,U_divk_TTr=U2_TTr)
  print *,"r"
  print *,U_r,(U2-U)/(r*eps)
  print *,U_rr,(U2_r-U_r)/(r*eps)
  print *,U_rrr,(U2_rr-U_rr)/(r*eps)
  print *,U_Tr,(U2_T-U_T)/(r*eps)
  print *,U_Trr,(U2_Tr-U_Tr)/(r*eps)
  print *,U_TTr,(U2_TT-U_TT)/(r*eps)

  call update_temp_variables(nc,T+T*eps,saftvrmie_var(1))
  call calc_mie_potential_quantumcorrected(i,i,saftvrmie_var(1),&
       saftvrmie_param%comp(i)%sigma,saftvrmie_param%comp(i)%eps_depth_divk,&
       saftvrmie_param%comp(i)%lambda_a,saftvrmie_param%comp(i)%lambda_r,&
       saftvrmie_param%Cij(i,i),&
       saftvrmie_param%Quantum_const_1a_ij(i,i),&
       saftvrmie_param%Quantum_const_1r_ij(i,i),&
       saftvrmie_param%Quantum_const_2a_ij(i,i),&
       saftvrmie_param%Quantum_const_2r_ij(i,i),&
       r,U2,U_divk_T=U2_T,U_divk_TT=U2_TT,&
       U_divk_r=U2_r,U_divk_Tr=U2_Tr,&
       U_divk_rr=U2_rr,U_divk_rrr=U2_rrr,&
       U_divk_Trr=U2_Trr,U_divk_TTr=U2_TTr)
  print *,"T"
  print *,U_T,(U2-U)/(T*eps)
  print *,U_TT,(U2_T-U_T)/(T*eps)
  print *,U_Tr,(U2_r-U_r)/(T*eps)
  print *,U_TTr,(U2_Tr-U_Tr)/(T*eps)
  print *,U_Trr,(U2_rr-U_rr)/(T*eps)

  print *,"Test eps_divk differentials"
  call update_temp_variables(nc,T,saftvrmie_var(1))
  call calc_binary_effective_eps_divk(nc,T,saftvrmie_var(1),eps_divk_eff,eps_divk_eff_T,eps_divk_eff_TT)
  print *,eps_divk_eff,eps_divk_eff/saftvrmie_param%comp(i)%eps_depth_divk

  call update_temp_variables(nc,T+T*eps,saftvrmie_var(1))
  call calc_binary_effective_eps_divk(nc,T+T*eps,saftvrmie_var(1),eps_divk_eff2,eps_divk_eff2_T,eps_divk_eff2_TT)
  print *,eps_divk_eff_T,(eps_divk_eff2-eps_divk_eff)/(T*eps)
  print *,eps_divk_eff_TT,(eps_divk_eff2_T-eps_divk_eff_T)/(T*eps)

contains
  subroutine update_temp_variables(nc,T,saftvrmie_vc)
    implicit none
    real, intent(in) :: T
    integer, intent(in) :: nc
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc
    ! Calculate Feynman--Hibbs D parameter
    call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
         saftvrmie_vc%DFeynHibbsij, saftvrmie_vc%D2FeynHibbsij)
    ! Calculate effective sigma
    call calc_binary_effective_sigma(nc,T,saftvrmie_vc,saftvrmie_vc%sigma_eff%d,&
         saftvrmie_vc%sigma_eff%d_T,saftvrmie_vc%sigma_eff%d_TT)
    ! Calculate effective epsilon divided by k
    call calc_binary_effective_eps_divk(nc,T,saftvrmie_vc,saftvrmie_vc%eps_divk_eff%d,&
         saftvrmie_vc%eps_divk_eff%d_T,saftvrmie_vc%eps_divk_eff%d_TT)
    ! Calculate hard-sphere diameter
    call calc_hardsphere_diameter(nc,T,saftvrmie_vc,saftvrmie_vc%sigma_eff%d,&
         saftvrmie_vc%sigma_eff%d_T,saftvrmie_vc%sigma_eff%d_TT,saftvrmie_vc%dhs%d,&
         saftvrmie_vc%dhs%d_T,saftvrmie_vc%dhs%d_TT)
  end subroutine update_temp_variables
end subroutine test_effective_eps_divk
