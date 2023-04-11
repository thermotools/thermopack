!--------------------------------------------------------------------------------
! Subroutines and functions for various the Boublík (10.1063/1.1673824) and
! Mansoori et al. (10.1063/1.1675048) (BMCSL) hard-sphere model
! Implemented by: M. Hammer, A. Aasen and Mr. Wilhelmsen
!
!---------------------------------------------------------------------------------
module hardsphere_bmcsl
  use thermopack_constants, only: h_const,kB_const,N_AVOGADRO
  use numconstants, only: pi
  use thermopack_constants, only: verbose
  implicit none
  private
  save

  !> Container for temperature dependent hard-sphere diameter and differentials
  type :: hs_diameter
    !> Hard sphere diameter
    real, allocatable, dimension(:) :: d
    !> Temperature differential of hard sphere diameter
    real, allocatable, dimension(:) :: d_T
    !> Second temperature differential of hard sphere diameter
    real, allocatable, dimension(:) :: d_TT
    !> d calculated for T:
    real :: T_update = 0
  contains
    procedure, public :: allocate => allocate_hs_diameter
    procedure, public :: deallocate => cleanup_hs_diameter
    ! Assignment operator
    procedure, public :: assign_hs_diameter
    generic, public :: assignment(=) => assign_hs_diameter
  end type hs_diameter

  !> Container for zeta's (0, 1, 2, 3 and mu). These are moments of the number density
  type :: packing_fraction_hs
    !> Moments of the number density
    real, dimension(5) :: zet
    !> Temperature differential of the moments of the number density and mu
    real, dimension(5) :: zet_T
    !> Second temperature differential of the moments of the number density and mu
    real, dimension(5) :: zet_TT
    !> Volume differential of the moments of the number density and mu
    real, dimension(5) :: zet_V
    !> Second volume differential of the moments of the number density and mu
    real, dimension(5) :: zet_VV
    !> Temperature and volume differential of the moments of the number density and mu
    real, dimension(5) :: zet_TV
    !> Mol number differential of the moments of the number density and mu
    real, allocatable, dimension(:,:) :: zet_n
    !> Mol number and volume differential of the moments of the number density and mu
    real, allocatable, dimension(:,:) :: zet_Vn
    !> Mol number and temperature differential of the moments of the number density and mu
    real, allocatable, dimension(:,:) :: zet_Tn
  contains
    procedure, public :: allocate => allocate_packing_fraction_hs
    procedure, public :: deallocate => cleanup_packing_fraction_hs
    ! Assignment operator
    procedure, public :: assign_packing_fraction_hs
    generic, public :: assignment(=) => assign_packing_fraction_hs
  end type packing_fraction_hs

  public :: calc_bmcsl_helmholtzenergy
  public :: calc_bmcsl_gij
  public :: packing_fraction_hs, hs_diameter
  public :: calc_bmcsl_zeta_and_derivatives
  public :: calc_bmcsl_lngij
  public :: calc_bmcsl_gij_FMT
  public :: calc_bmcsl_gij_FMT_hd

contains

  subroutine calc_bmcsl_lngij(nc,T,V,n,i,j,dhs,zeta, &
       lng,lng_T,lng_V,lng_n,lng_TT,lng_TV,lng_Tn,lng_VV,lng_Vn,lng_nn)
    !------------------------------------------------------------------------
    !> Calculate log value of g (RDF) at contact using
    !! the expression of Boublik (10.1063/1.1673824).
    !!
    !! \author Morten Hammer, 2022-04
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    integer, intent(in) :: i, j    ! The pair-correlation of the pair i,j
    type(hs_diameter), intent(in) :: dhs !< Hard-sphere diameter and differentials
    type(packing_fraction_hs), intent(inout) :: zeta      !< Zetas and derivatives
    real, intent(out) :: lng         !< reduced helmholtz energy [-]
    real, intent(out), optional :: lng_T,lng_V,lng_n(nc)           !< derivatives
    real, intent(out), optional :: lng_TT,lng_TV,lng_Tn(nc)        !< derivatives
    real, intent(out), optional :: lng_VV,lng_Vn(nc),lng_nn(nc,nc) !<derivatives
    ! Locals
    real :: g         !< reduced helmholtz energy [-]
    real, target :: g_T,g_V,g_n(nc)           !< derivatives
    real, target :: g_TT,g_TV,g_Tn(nc)        !< derivatives
    real, target :: g_VV,g_Vn(nc),g_nn(nc,nc) !<derivatives
    real, pointer :: p_g_T,p_g_V,p_g_n(:)           !< derivatives
    real, pointer :: p_g_TT,p_g_TV,p_g_Tn(:)        !< derivatives
    real, pointer :: p_g_VV,p_g_Vn(:),p_g_nn(:,:) !<derivatives
    integer :: k

    if ( present(lng_T) .or. &
         present(lng_TT) .or. &
         present(lng_TV)  .or. &
         present(lng_Tn)) then
      p_g_T => g_T
    else
      p_g_T => NULL()
    endif
    if ( present(lng_V) .or. &
         present(lng_VV) .or. &
         present(lng_TV)  .or. &
         present(lng_Vn)) then
      p_g_V => g_V
    else
      p_g_V => NULL()
    endif
    if ( present(lng_n) .or. &
         present(lng_nn) .or. &
         present(lng_Tn)  .or. &
         present(lng_Vn)) then
      p_g_n => g_n
    else
      p_g_n => NULL()
    endif
    if (present(lng_TT)) then
      p_g_TT => g_TT
    else
      p_g_TT => NULL()
    endif
    if (present(lng_VV)) then
      p_g_VV => g_VV
    else
      p_g_VV => NULL()
    endif
    if (present(lng_Tn)) then
      p_g_Tn => g_Tn
    else
      p_g_Tn => NULL()
    endif
    if (present(lng_Vn)) then
      p_g_Vn => g_Vn
    else
      p_g_Vn => NULL()
    endif
    if (present(lng_TV)) then
      p_g_TV => g_TV
    else
      p_g_TV => NULL()
    endif
    if (present(lng_nn)) then
      p_g_nn => g_nn
    else
      p_g_nn => NULL()
    endif

    call calc_bmcsl_gij(nc,T,V,n,i,j,dhs,zeta, &
         g,p_g_T,p_g_V,p_g_n,p_g_TT,p_g_TV,p_g_Tn,p_g_VV,p_g_Vn,p_g_nn)

    lng = log(g)
    if (present(lng_T)) then
      lng_T = g_T/g
    endif
    if (present(lng_V)) then
      lng_V = g_V/g
    endif
    if (present(lng_n)) then
      lng_n = g_n/g
    endif
    if (present(lng_TT)) then
      lng_TT = g_TT/g - g_T**2/g**2
    endif
    if (present(lng_VV)) then
      lng_VV = g_VV/g - g_V**2/g**2
    endif
    if (present(lng_TV)) then
      lng_TV = g_TV/g - g_T*g_V/g**2
    endif
    if (present(lng_Tn)) then
      lng_Tn = g_Tn/g - g_T*g_n/g**2
    endif
    if (present(lng_Vn)) then
      lng_Vn = g_Vn/g - g_V*g_n/g**2
    endif
    if (present(lng_nn)) then
      do k=1,nc
        lng_nn(:,k) = g_nn(:,k)/g - g_n*g_n(k)/g**2
      enddo
    endif

  end subroutine calc_bmcsl_lngij

  subroutine calc_bmcsl_gij_FMT(n_alpha,mu_ij,mu_ij_T,g,g_n,g_T)
    !------------------------------------------------------------------------
    !>  FMT model for associating fluids
    !! 2022-04, Morten Hammer
    !! We have used the expression from Yang-Xin Yu and Jianzhong Wu
    !! "A fundamental-measure theory for inhomogeneous associating fluids"
    !! J. Chem. Phys., Vol. 116, No. 16 (2002).  All derivatives checked numerically.
    !! doi: 10.1063/1.1463435
    !----------------------------------------------------------------------------
    real, intent(in) :: n_alpha(0:5)  !< temperature [K], n_alpha
    !type(hs_diameter), intent(in) :: dhs !< Hard-sphere diameter and differentials
    real, intent(in) :: mu_ij, mu_ij_T !< mu=(d(i)*d(j))/(d(i)+d(j)) and temperature differential
    real, intent(out) :: g         !< reduced helmholtz energy [-]
    real, intent(out), optional :: g_n(0:5) !< derivatives
    real, intent(out), optional :: g_T !< derivative wrpt. temperature
    integer, parameter :: n2V = 5
    real :: mu, mu_2, xi, xi_n2, xi_n2V, g_xi
    real :: div_diff_1, div_diff_2, div_diff_3, div_diff_4

    ! Compute mu
    !mu=(dhs%d(i)*dhs%d(j))/(dhs%d(i)+dhs%d(j))
    xi = 1 - n_alpha(n2V)**2/n_alpha(2)**2
    xi_n2 = 2*n_alpha(n2V)**2/n_alpha(2)**3
    xi_n2V = -2*n_alpha(n2V)/n_alpha(2)**2

    ! Prefactors that go into many of the expressions
    div_diff_1=1.0/(1-n_alpha(3))
    div_diff_2=div_diff_1**2
    div_diff_3=div_diff_1*div_diff_2
    div_diff_4=div_diff_2*div_diff_2
    mu = mu_ij
    mu_2=mu*mu

    g_xi = mu*n_alpha(2)*div_diff_2/2 + n_alpha(2)**2*mu_2*div_diff_3/18
    g = div_diff_1 + g_xi*xi

    if (present(g_n)) then
      g_n=0
      g_n(2) = xi*(mu*div_diff_2/2 + n_alpha(2)*mu_2*div_diff_3/9) +&
           g_xi*xi_n2
      g_n(3) = div_diff_2 + n_alpha(2)*div_diff_3*mu*xi &
           + n_alpha(2)**2*div_diff_4*mu_2*xi/6
      g_n(n2V) = g_xi*xi_n2V
    endif
    if (present(g_T)) then
      g_T=xi*mu_ij_T*(n_alpha(2)*div_diff_2/2 + n_alpha(2)**2*mu*div_diff_3/9)
    endif

  end subroutine calc_bmcsl_gij_FMT

  subroutine calc_bmcsl_gij_FMT_hd(nc,n_alpha,dhs,i,j,g)
    !------------------------------------------------------------------------
    !>  FMT model for associating fluids
    !! 2022-04, Morten Hammer
    !! We have used the expression from Yang-Xin Yu and Jianzhong Wu
    !! "A fundamental-measure theory for inhomogeneous associating fluids"
    !! J. Chem. Phys., Vol. 116, No. 16 (2002).  All derivatives checked numerically.
    !! doi: 10.1063/1.1463435
    !----------------------------------------------------------------------------
    use hyperdual_mod
    integer, intent(in) :: nc, i, j
    type(hyperdual), intent(in) :: n_alpha(0:5) !< temperature [K], n_alpha
    type(hyperdual), intent(in) :: dhs(nc)      !< Hard-sphere diameter
    type(hyperdual), intent(out) :: g           !< reduced helmholtz energy [-]
    ! Locals
    integer, parameter :: n2V = 5
    type(hyperdual) :: mu, xi

    mu=(dhs(i)*dhs(j))/(dhs(i)+dhs(j))
    xi = 1.0_dp - n_alpha(n2V)**2/n_alpha(2)**2
    g = 1.0_dp/(1.0_dp-n_alpha(3)) + xi*n_alpha(2)*mu*(1.0_dp/(2.0_dp*(1.0_dp-n_alpha(3))**2) &
         + n_alpha(2)*mu/(18.0_dp*(1.0_dp-n_alpha(3))**3))

  end subroutine calc_bmcsl_gij_FMT_hd

  subroutine calc_bmcsl_gij(nc,T,V,n,i,j,dhs,zeta, &
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
    type(hs_diameter), intent(in) :: dhs !< Hard-sphere diameter and differentials
    type(packing_fraction_hs), intent(inout) :: zeta      !< Zetas and derivatives
    real, intent(out) :: g         !< reduced helmholtz energy [-]
    real, intent(out), optional :: g_T,g_V,g_n(nc)           !< derivatives
    real, intent(out), optional :: g_TT,g_TV,g_Tn(nc)        !< derivatives
    real, intent(out), optional :: g_VV,g_Vn(nc),g_nn(nc,nc) !<derivatives
    real :: dg_dzeta(3:5)      !< First order deriv. of the rdf
    real :: d2g_dzeta2(3:5,3:5)  !< Second order deriv. of the rdf
    integer :: k, comp1, comp2, l
    real :: mu, mu_up, mu_down
    real :: mu_TT_up1, mu_TT_up2

    ! Compute mu
    mu_up=dhs%d(i)*dhs%d(j)
    mu_down=dhs%d(i)+dhs%d(j)
    mu=mu_up/mu_down
    zeta%zet(5) = mu
    if (present(g_T)) then
      g_T = 0
      zeta%zet_T(5)=((dhs%d(i)**2)*dhs%d_T(j)+(dhs%d(j)**2)*dhs%d_T(i))/(mu_down**2)
    endif
    if (present(g_TT)) then
      g_TT = 0
      mu_TT_up1=2.0*(dhs%d(i)+dhs%d(j))*dhs%d_T(i)*dhs%d_T(j)+&
           ((dhs%d(i)**2)*dhs%d_TT(j)+(dhs%d(j)**2)*dhs%d_TT(i))
      mu_TT_up2=((dhs%d(i)**2)*dhs%d_T(j)+(dhs%d(j)**2)*dhs%d_T(i))*&
           (dhs%d_T(i)+dhs%d_T(j))
      zeta%zet_TT(5)=mu_TT_up1/(mu_down**2)-2.0*mu_TT_up2/(mu_down**3)
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
    call calc_bmcsl_dgij_dzeta(zeta,g,dg_dzeta,d2g_dzeta2)

    do comp1= 1,nc
      do comp2= 1,nc
        do l = 3,5   ! Loop over mu, eta_2 and eta_3
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

          do k = 3,5
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
  end subroutine calc_bmcsl_gij

  subroutine calc_bmcsl_dgij_dzeta(zeta,g,dg_dzeta,&
       d2g_dzeta2)
    !---------------------------------------------------------------------
    !  2019-01-15, Oivind Wilhelmsen
    ! Obtain the derivatives of the pair correlation function expression with
    ! respect to mu (first index), zeta2 (second index) and zeta3
    ! (third index). All derivatives have been checked numerically.
    !---------------------------------------------------------------------
    type(packing_fraction_hs), intent(in) :: zeta            !< Zetas and derivatives
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
    zeta_2=zeta%zet(3)
    zeta_3=zeta%zet(4)
    mu=zeta%zet(5)

    ! Prefactors that go into many of the expressions
    div_diff_1=1.0/(1-zeta_3)
    div_diff_2=div_diff_1**2
    div_diff_3=div_diff_1*div_diff_2
    div_diff_4=div_diff_2*div_diff_2
    mu_2=mu*mu
    zeta_2_2=zeta_2*zeta_2

    g=div_diff_1+3.0*zeta_2*div_diff_2*mu+2.0*(zeta_2_2)*mu_2*div_diff_3

    ! The first order derivatives
    dg_dzeta(3)=3.0*zeta_2*div_diff_2+4.0*zeta_2_2*div_diff_3*mu  ! dg_dmu
    dg_dzeta(1)=3.0*mu*div_diff_2+4.0*div_diff_3*mu_2*zeta_2      ! dg_zeta_2
    dg_dzeta(2)=div_diff_2+6.0*zeta_2*div_diff_3*mu+6.0*zeta_2_2*div_diff_4*mu_2 !dg_dzeta_3

    ! The second order derivatives [diagonal elements]
    d2g_dzeta2(3,3)=4.0*zeta_2_2*div_diff_3 ! d2g_dmu2
    d2g_dzeta2(1,1)=4.0*div_diff_3*mu_2     ! d2g_dzeta_2_2
    d2g_dzeta2(2,2)=2.0*div_diff_3+18.0*zeta_2*mu*div_diff_4+24.0*zeta_2_2*mu_2*div_diff_3*div_diff_2  ! d2g_dzeta_3_2

    ! The second order derivatives [off-diagonal components]
    d2g_dzeta2(3,1)=3.0*div_diff_2+8.0*zeta_2*mu*div_diff_3 !d2g_dmudzeta2
    d2g_dzeta2(1,2)=6.0*div_diff_3*mu+12.0*zeta_2*div_diff_4*mu_2 !d2g_dzeta2zeta3
    d2g_dzeta2(3,2)=6.0*zeta_2*div_diff_3+12.0*zeta_2_2*div_diff_4*mu !d2g_dmudzeta3

    ! Complete the matrix of second derivatives
    d2g_dzeta2(1,3)=d2g_dzeta2(3,1)
    d2g_dzeta2(2,1)=d2g_dzeta2(1,2)
    d2g_dzeta2(2,3)=d2g_dzeta2(3,2)

  end subroutine calc_bmcsl_dgij_dzeta

  subroutine calc_bmcsl_zeta_and_derivatives(nc,V,n,dhs,zeta,ms)
    !---------------------------------------------------------------------
    !  2018-02-24, Oivind Wilhelmsen
    ! Obtain the zeta-variable and derivatives, used in the hard-sphere
    ! contribution and derivatives. Derivatives checked numerically.
    !---------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: V,n(nc)  !< volume [m^3], N_i [mol]
    type(hs_diameter), intent(in) :: dhs
    type(packing_fraction_hs), intent(inout) :: zeta    !< zeta variable (zeta_0,...,zeta_3)
    real, intent(in) :: ms(nc)  !< Monomer segments
    integer :: i, l, index_l
    real :: constant_piN

    ! First allocate zero to all vectors
    zeta%zet=0.0
    zeta%zet_V=0.0
    zeta%zet_VV=0.0
    zeta%zet_T=0.0
    zeta%zet_TT=0.0
    zeta%zet_n=0.0
    zeta%zet_Vn=0.0
    zeta%zet_TV=0.0
    zeta%zet_Tn=0.0

    ! Loop over the number of indexes of the zeta (l=0,1,2,3) and
    ! the number of component (i) where (index_l=1,2,3,4)

    do l = 0, 3

      index_l=l+1

      do i = 1, nc
        ! The zeta variable and its derivatives:
        zeta%zet(index_l)=zeta%zet(index_l)+ms(i)*n(i)*dhs%d(i)**(l)
        zeta%zet_n(i,index_l)=ms(i)*(dhs%d(i)**l)
        zeta%zet_Vn(i,index_l)=-1.0*ms(i)*(dhs%d(i)**l)/V

        if (l>0) then

          zeta%zet_T(index_l)=zeta%zet_T(index_l)+&
               l*ms(i)*n(i)*(dhs%d(i)**(l-1))*dhs%d_T(i)

          zeta%zet_TT(index_l)=zeta%zet_TT(index_l)+l*ms(i)*n(i)*(dhs%d(i)**(l-1))*&
               dhs%d_TT(i)

          zeta%zet_TV(index_l)=zeta%zet_TV(index_l)-(1.0*l/V)*ms(i)*n(i)*&
               (dhs%d(i)**(l-1))*dhs%d_T(i)

          zeta%zet_Tn(i,index_l)=l*ms(i)*(dhs%d(i)**(l-1))*dhs%d_T(i)
        end if
        if (l>1) then

          zeta%zet_TT(index_l)=zeta%zet_TT(index_l)+l*(l-1)*ms(i)*n(i)*&
               (dhs%d(i)**(l-2))*(dhs%d_T(i)**2)

        end if
      end do
    end do

    ! The prefactor of the expressions
    constant_piN=(pi*N_AVOGADRO/(6.0*V))

    ! Multiply all terms with the prefactor
    zeta%zet=zeta%zet*constant_piN
    zeta%zet_T=zeta%zet_T*constant_piN
    zeta%zet_TT=zeta%zet_TT*constant_piN
    zeta%zet_V=-1.0*zeta%zet/V
    zeta%zet_VV=2.0*zeta%zet/(V**2)
    zeta%zet_n=zeta%zet_n*constant_piN
    zeta%zet_Vn=zeta%zet_Vn*constant_piN
    zeta%zet_TV=zeta%zet_TV*constant_piN
    zeta%zet_Tn=zeta%zet_Tn*constant_piN

  end subroutine calc_bmcsl_zeta_and_derivatives

  subroutine calc_bmcsl_dalpha_dzeta(zeta,alpha,dalpha_dzeta,&
       d2alpha_dzeta2)
    !---------------------------------------------------------------------
    !  2018-02-27, Oivind Wilhelmsen
    ! Obtain the zeta-variable and derivatives, used in the hard-sphere
    ! contribution and derivatives. Derivatives checked numerically.
    !---------------------------------------------------------------------
    type(packing_fraction_hs), intent(in)  :: zeta !< zeta variable
    real, intent(out) :: alpha                !< reduced Helholtz energy
    real, intent(out) :: dalpha_dzeta(4)      !< reduced Helhmholtz 1-deriv
    real, intent(out) :: d2alpha_dzeta2(4,4)  !< reduced Helholtz 2-deriv
    real :: zeta_0, zeta_1, zeta_2, zeta_3    !< variables
    real :: pref,ln_term, en_m_zeta_3         !< Intermediate variables

    ! First allocate zero to all vectors
    dalpha_dzeta=0.0
    d2alpha_dzeta2=0.0

    ! Extracting variables from vector to enhance readability
    zeta_0=zeta%zet(1)
    zeta_1=zeta%zet(2)
    zeta_2=zeta%zet(3)
    zeta_3=zeta%zet(4)

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

  end subroutine calc_bmcsl_dalpha_dzeta

  subroutine calc_bmcsl_helmholtzenergy(nc,T,V,n,zeta,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
    !------------------------------------------------------------------------
    !  2018-02-27, Oivind Wilhelmsen
    ! Boublík (10.1063/1.1673824) and Mansoori et al. (10.1063/1.1675048)
    ! The reduced Helmholtz energy of the hard-sphere term and its derivatives,
    !----------------------------------------------------------------------------
    integer, intent(in) :: nc      !< number of components
    real, intent(in) :: T,V,n(nc)  !< temperature [K], volume [m^3], N_i [mol]
    type(packing_fraction_hs), intent(in) :: zeta    !< zeta variable (zeta_0,...,zeta_3)
    real, intent(out) :: a         !< reduced helmholtz energy [-]
    real, intent(out), optional :: a_T,a_V,a_n(nc)           !< derivatives
    real, intent(out), optional :: a_VV,a_TV,a_Vn(nc)        !< derivatives
    real, intent(out), optional :: a_TT,a_Tn(nc),a_nn(nc,nc) !<derivatives
    ! Locals
    real :: alpha                !< reduced Helholtz energy
    real :: dalpha_dzeta(4)      !< reduced Helhmholtz 1-deriv
    real :: d2alpha_dzeta2(4,4)  !< reduced Helholtz 2-deriv
    integer :: k, comp1, comp2, l

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

    ! Obtain the reduced Helholtz energy and the derivatives with
    ! respect to the zeta-variables
    call calc_bmcsl_dalpha_dzeta(zeta,alpha,dalpha_dzeta,&
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
                a_T=a_T+dalpha_dzeta(l)*zeta%zet_T(l)
              endif
              if (present(a_V)) then
                a_V=a_V+dalpha_dzeta(l)*zeta%zet_V(l)
              endif
              if (present(a_VV)) then
                a_VV=a_VV+dalpha_dzeta(l)*zeta%zet_VV(l)
              endif
              if (present(a_TV)) then
                a_TV=a_TV+dalpha_dzeta(l)*zeta%zet_TV(l)
              endif
              if (present(a_TT)) then
                a_TT=a_TT+dalpha_dzeta(l)*zeta%zet_TT(l)
              endif
            end if

            ! The composition first order derivative of a
            if (present(a_n)) then
              a_n(comp1)=a_n(comp1)+dalpha_dzeta(l)*zeta%zet_n(comp1,l)
            endif
            if (present(a_Tn)) then
              a_Tn(comp1)=a_Tn(comp1)+dalpha_dzeta(l)*zeta%zet_Tn(comp1,l)
            endif
            if (present(a_Vn)) then
              a_Vn(comp1)=a_Vn(comp1)+dalpha_dzeta(l)*zeta%zet_Vn(comp1,l)
            endif
          end if

          ! End single sum over l part of the derivatives
          ! Start double sum over l and k part of the derivatives

          do k = 1,4
            if (comp2==1) then ! Compute this only once for each "l" and "k"
              if (comp1==1) then ! Compute this only once for each "l" and "k"
                if (present(a_VV)) then
                  a_VV=a_VV+d2alpha_dzeta2(l,k)*zeta%zet_V(l)*zeta%zet_V(k)
                endif
                if (present(a_TV)) then
                  a_TV=a_TV+d2alpha_dzeta2(l,k)*zeta%zet_T(l)*zeta%zet_V(k)
                endif
                if (present(a_TT)) then
                  a_TT=a_TT+d2alpha_dzeta2(l,k)*zeta%zet_T(l)*zeta%zet_T(k)
                endif
              end if

              ! The composition first order derivative of a
              if (present(a_Tn)) then
                a_Tn(comp1)=a_Tn(comp1)+d2alpha_dzeta2(l,k)*&
                     zeta%zet_T(l)*zeta%zet_n(comp1,k)
              endif
              if (present(a_Vn)) then
                a_Vn(comp1)=a_Vn(comp1)+d2alpha_dzeta2(l,k)*&
                     zeta%zet_V(l)*zeta%zet_n(comp1,k)
              endif
            end if

            ! Second order mole number derivative
            if (present(a_nn)) then
              a_nn(comp1,comp2)=a_nn(comp1,comp2)+d2alpha_dzeta2(l,k)*&
                   zeta%zet_n(comp1,l)*zeta%zet_n(comp2,k)
            endif
          end do
        end do
      end do
    end do
  end subroutine calc_bmcsl_helmholtzenergy

  subroutine assign_hs_diameter(this,other)
    class(hs_diameter), intent(inout) :: this
    class(hs_diameter), intent(in) :: other
    !
    this%d = other%d
    this%d_T = other%d_T
    this%d_TT = other%d_TT
    this%T_update = other%T_update
  end subroutine assign_hs_diameter

  !> Free allocated hs_diameter memory
  subroutine cleanup_hs_diameter(dhs)
    ! Passed object
    class(hs_diameter), intent(inout) :: dhs
    ! Locals
    integer :: ierr
    if (allocated(dhs%d)) then
      deallocate (dhs%d, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("hardsphere_bmcsl::cleanup_hs_diameter: Not able to deallocate dhs%d")
      endif
    endif
    if (allocated(dhs%d_T)) then
      deallocate (dhs%d_T, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("hardsphere_bmcsl::cleanup_hs_diameter: Not able to deallocate dhs%d_T")
      endif
    endif
    if (allocated(dhs%d_TT)) then
      deallocate (dhs%d_TT, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("hardsphere_bmcsl::cleanup_hs_diameter: Not able to deallocate dhs%d_TT")
      endif
    endif
  end subroutine cleanup_hs_diameter

  !> Allocated hs_diameter memory
  subroutine allocate_hs_diameter(dhs, nc)
    ! Passed object
    class(hs_diameter), intent(inout) :: dhs
    ! Input
    integer, intent(in) :: nc
    ! Locals
    integer :: ierr
    call dhs%deallocate()
    dhs%T_update = 0
    allocate (dhs%d(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("hardsphere_bmcsl::allocate_hs_diameter: Not able to allocate dhs%d")
    endif
    allocate (dhs%d_T(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("hardsphere_bmcsl::allocate_hs_diameter: Not able to allocate dhs%d_T")
    endif
    allocate (dhs%d_TT(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("hardsphere_bmcsl::allocate_hs_diameter: Not able to allocate dhs%d_TT")
    endif
  end subroutine allocate_hs_diameter

  subroutine assign_packing_fraction_hs(this,other)
    class(packing_fraction_hs), intent(inout) :: this
    class(packing_fraction_hs), intent(in) :: other
    !
    this%zet = other%zet
    this%zet_T = other%zet_T
    this%zet_TT = other%zet_TT
    this%zet_V = other%zet_V
    this%zet_VV = other%zet_VV
    this%zet_TV = other%zet_TV
    this%zet_n = other%zet_n
    this%zet_Vn = other%zet_Vn
    this%zet_Tn = other%zet_Tn
  end subroutine assign_packing_fraction_hs

  !> Free allocated packing_fraction_hs memory
  subroutine cleanup_packing_fraction_hs(zeta)
    ! Passed object
    class(packing_fraction_hs), intent(inout) :: zeta
    ! Locals
    integer :: ierr
    if (allocated(zeta%zet_n)) then
      deallocate (zeta%zet_n, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("hardsphere_bmcsl::cleanup_packing_fraction_hs: Not able to deallocate zeta%zet_n")
      endif
    endif
    if (allocated(zeta%zet_Vn)) then
      deallocate (zeta%zet_Vn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("hardsphere_bmcsl::cleanup_packing_fraction_hs: Not able to deallocate zeta%zet_Vn")
      endif
    endif
    if (allocated(zeta%zet_Tn)) then
      deallocate (zeta%zet_Tn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("hardsphere_bmcsl::cleanup_packing_fraction_hs: Not able to deallocate zeta%zet_Tn")
      endif
    endif
  end subroutine cleanup_packing_fraction_hs

  !> Allocated zetaeta memory and initialize to zero
  subroutine allocate_packing_fraction_hs(zeta,nc)
    ! Passed object
    class(packing_fraction_hs), intent(inout) :: zeta
    ! Input
    integer, intent(in) :: nc
    ! Locals
    integer :: ierr
    call zeta%deallocate()
    zeta%zet = 0.0
    zeta%zet_T = 0.0
    zeta%zet_TT = 0.0
    zeta%zet_V = 0.0
    zeta%zet_VV = 0.0
    zeta%zet_TV = 0.0
    allocate (zeta%zet_n(nc,5), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("hardsphere_bmcsl::allocate_packing_fraction: Not able to allocate zeta%zet_n")
    endif
    zeta%zet_n = 0.0
    allocate (zeta%zet_Tn(nc,5), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("hardsphere_bmcsl::allocate_packing_fraction: Not able to allocate zeta%zet_Tn")
    endif
    zeta%zet_Tn = 0.0
    allocate (zeta%zet_Vn(nc,5), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("hardsphere_bmcsl::allocate_packing_fraction: Not able to allocate zeta%zet_Vn")
    endif
    zeta%zet_Vn = 0.0
  end subroutine allocate_packing_fraction_hs

end module hardsphere_bmcsl
