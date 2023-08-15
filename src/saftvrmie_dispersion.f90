!---------------------------------------------------------------------
! Module and subroutines for the dispersion part of SAFT-VRQ Mie
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------

module saftvrmie_dispersion
  use saftvrmie_containers
  ! , only: saftvrmie_zeta, saftvrmie_dhs, &
  !      saftvrmie_aij, saftvrmie_var, saftvrmie_param, init_saftvrmie_containers, &
  !      saftvrmie_param_container, saftvrmie_var_container, quantum_correction, &
  !      add_second_saftvrmieaij_to_first, calcFunAlpha, quantum_correction_hs
  use saftvrmie_utils, only: calc_a_zeta_product, convert_zeta_x_to_TVn, &
       calc_a0_a_product, calc_a0_plus_a1, convert_zeta_zeta_to_TVn
  use thermopack_constants, only: kB_const,N_AVOGADRO
  use numconstants, only: pi
  use saftvrmie_options
  implicit none
  !private
  save

  public :: calcA1, calcA2, calcA3
  public :: calc_delta_Ac, calc_alpha_ts

contains

  !> Add aQ*DFH to a
  !! \author Ailo Aasen, March 2018
  subroutine add_aQ_DFH_product(nc,DFH,DFH_T,DFH_TT,&
       aQ,a,&
       aQ_T,aQ_V,aQ_n,&
       a_T,a_V,a_n,&
       aQ_TT,aQ_VV,aQ_TV,aQ_Tn,aQ_Vn,aQ_nn,&
       a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       aQ_VVV,aQ_VVT,aQ_VTT,aQ_VVn,aQ_Vnn,aQ_VTn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: DFH, DFH_T, DFH_TT !< DFH and differentials
    real, intent(in) ::  aQ
    real, optional, intent(in) ::  aQ_T,aQ_V,aQ_TT,aQ_VV,aQ_TV,aQ_VVV,aQ_VVT,aQ_VTT
    real, optional, dimension(nc), intent(in) :: aQ_n,aQ_Tn,aQ_Vn,aQ_VVn,aQ_VTn
    real, optional, dimension(nc,nc), intent(in) :: aQ_nn,aQ_Vnn

    ! Output
    real, intent(inout) ::  a
    real, optional, intent(inout) ::  a_T,a_V,a_TT,a_VV,a_TV,a_VVV,a_VVT,a_VTT
    real, optional, dimension(nc), intent(inout) :: a_n,a_Tn,a_Vn,a_VVn,a_VTn
    real, optional, dimension(nc,nc), intent(inout) :: a_nn,a_Vnn

    a = a + aQ*DFH
    if (present(a_V)) a_V = a_V + aQ_V*DFH
    if (present(a_n)) a_n = a_n + aQ_n*DFH
    if (present(a_T)) a_T = a_T + aQ_T*DFH + aQ*DFH_T
    if (present(a_TT)) a_TT = a_TT + aQ_TT*DFH + 2*aQ_T*DFH_T + aQ*DFH_TT
    if (present(a_VV)) a_VV = a_VV + aQ_VV*DFH
    if (present(a_TV)) a_TV = a_TV + aQ_TV*DFH + aQ_V*DFH_T
    if (present(a_Tn)) a_Tn = a_Tn + aQ_Tn*DFH + aQ_n*DFH_T
    if (present(a_Vn)) a_Vn = a_Vn + aQ_Vn*DFH
    if (present(a_nn)) a_nn = a_nn + aQ_nn*DFH
    if (present(a_VVV)) a_VVV = a_VVV + aQ_VVV*DFH
    if (present(a_VVT)) a_VVT = a_VVT + aQ_VVT*DFH + aQ_VV*DFH_T
    if (present(a_VTT)) a_VTT = a_VTT + aQ_VTT*DFH + 2*aQ_TV*DFH_T + aQ_V*DFH_TT
    if (present(a_VVn)) a_VVn = a_VVn + aQ_VVn*DFH
    if (present(a_Vnn)) a_Vnn = a_Vnn + aQ_Vnn*DFH
    if (present(a_VTn)) a_VTn = a_VTn + aQ_VTn*DFH + aQ_Vn*DFH_T
  end subroutine add_aQ_DFH_product

  !> Calculate a*d(T)^3 and differentials
  !! Store result in a
  !!
  !! \author Morten Hammer, February 2018
  subroutine calc_a_d3_product(nc,a,i,j,d,d_T,d_TT,difflevel,already_multiplied)
    ! Input
    integer, intent(in) :: nc !< Number of components
    integer, intent(in) :: i,j !< Index of a
    real, intent(in) :: d, d_T, d_TT !< d(T)
    integer, intent(in) :: difflevel !< Level of differentiation
    logical, intent(in) :: already_multiplied !< Is d already multiplied with all a terms
    ! Inout
    type(saftvrmie_aij), intent(inout) :: a !< a
    ! Locals
    real :: d3
    if (.not. already_multiplied) then
       d3 = d**3
       a%am(i,j) = a%am(i,j)*d3
       if (difflevel > 0) then
          a%am_T(i,j) = a%am_T(i,j)*d3
          a%am_V(i,j) = a%am_V(i,j)*d3
          a%am_n(:,i,j) = a%am_n(:,i,j)*d3
       endif
       if (difflevel > 1) then
          a%am_TT(i,j) = a%am_TT(i,j)*d3
          a%am_VV(i,j) = a%am_VV(i,j)*d3
          a%am_TV(i,j) = a%am_TV(i,j)*d3
          a%am_Tn(:,i,j) = a%am_Tn(:,i,j)*d3
          a%am_Vn(:,i,j) = a%am_Vn(:,i,j)*d3
          a%am_nn(:,:,i,j) = a%am_nn(:,:,i,j)*d3
       endif
    endif

    if (difflevel > 1) then
       a%am_TT(i,j) = a%am_TT(i,j) + 6.0*a%am_T(i,j)*d_T/d &
            + 6.0*a%am(i,j)*(d_T/d)**2 + 3.0*a%am(i,j)*d_TT/d
       a%am_TV(i,j) = a%am_TV(i,j) + 3.0*a%am_V(i,j)*d_T/d
       a%am_Tn(:,i,j) = a%am_Tn(:,i,j) + 3.0*a%am_n(:,i,j)*d_T/d
    endif
    if (difflevel > 0) then
       a%am_T(i,j) = a%am_T(i,j) + 3.0*a%am(i,j)*d_T/d
    endif

  end subroutine calc_a_d3_product

  !> Calculate hypotetical pure fluid packing fraction
  !! Store results in saftvrmie_var
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcZetaX(nc,T,V,n,difflevel,dhs,zeta,zetaxbar)
    use saftvrmie_options, only: ZETA_LAFITTE, ZETA_LINEAR, ZETA_LEONARD
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< 0-2. Differential order
    type(saftvrmie_dhs), intent(in) :: dhs
    type(saftvrmie_zeta), intent(inout) :: zeta
    logical, optional, intent(in) :: zetaxbar


    ! if (present(zetaxbar)) then
    !    ! Only the vdw mixing rule allowed
    !    call calcZetaX_vdW(nc,T,V,n,difflevel,dhs,zeta)
    !    return
    ! end if

    ! For the zeta_x parameter (Lafitte Eq. (A13)), we allow different definitions
    select case(svrm_opt%zeta_mixing_rule)
    case(ZETA_LAFITTE)
       call calcZetaX_vdW(nc,T,V,n,difflevel+1,dhs,zeta)
    case(ZETA_LINEAR)
       call calcZetaX_linear(nc,T,V,n,difflevel+1,dhs,zeta)
    case(ZETA_LEONARD)
       call calcZetaX_leonard(nc,T,V,n,difflevel+1,dhs,zeta)
    case default
       call stoperror("Wrong mixing rule for packing fraction")
    end select
  end subroutine calcZetaX

  !> Calculate hypotetical pure fluid packing fraction
  !! Store results in saftvrmie_var
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcZetaX_vdW(nc,T,V,n,difflevel,dhs,zeta,is_rho)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< 0-2. Differential order
    type(saftvrmie_dhs), intent(in) :: dhs
    type(saftvrmie_zeta), intent(inout) :: zeta
    logical, optional, intent(in) :: is_rho
    ! Output
    ! Locals
    integer :: i,k,l
    real :: prefactor, ns
    ns = sum(saftvrmie_param%ms*n)
    prefactor = pi*N_AVOGADRO/(6.0*V*ns)
    if (present(is_rho)) then
      if (is_rho) prefactor = N_AVOGADRO/(V*ns)
    endif
    zeta%zx = 0.0
    do i=1,nc
       zeta%zx = zeta%zx + saftvrmie_param%ms(i)*n(i)*&
            sum(saftvrmie_param%ms*n*dhs%d(:,i)**3)
    enddo
    zeta%zx = prefactor*zeta%zx
    if (difflevel > 0) then
       zeta%zx_T = 0.0
       do i=1,nc
          zeta%zx_T = zeta%zx_T + saftvrmie_param%ms(i)*n(i)*&
               sum(saftvrmie_param%ms*n*dhs%d(:,i)**2*dhs%d_T(:,i))
       enddo
       zeta%zx_T = 3.0*prefactor*zeta%zx_T
       zeta%zx_V = -zeta%zx/V
       do k=1,nc
          ! Assuming d_kl = d_lk
          zeta%zx_n(k) = saftvrmie_param%ms(k)*(&
               2.0*prefactor*sum(saftvrmie_param%ms*n*dhs%d(:,k)**3) &
               - zeta%zx/ns)
       enddo
    endif
    if (difflevel > 1) then
       zeta%zx_TT = 0.0
       do i=1,nc
          zeta%zx_TT = zeta%zx_TT + prefactor*saftvrmie_param%ms(i)*n(i)*(&
               6.0*sum(saftvrmie_param%ms*n*dhs%d(:,i)*dhs%d_T(:,i)**2) &
               + 3.0*sum(saftvrmie_param%ms*n*dhs%d(:,i)**2*dhs%d_TT(:,i)))
       enddo
       zeta%zx_VV = 2.0*zeta%zx/V**2
       zeta%zx_TV = -zeta%zx_T/V
       zeta%zx_Vn = -zeta%zx_n/V
       do k=1,nc
          ! Assuming d_kl = d_lk
          zeta%zx_Tn(k) = saftvrmie_param%ms(k)*(&
               6.0*prefactor*sum(saftvrmie_param%ms*n*&
               dhs%d(:,k)**2*dhs%d_T(:,k)) &
               - zeta%zx_T/ns)
       enddo
       do k=1,nc
          do l=1,nc
             ! Assuming d_kl = d_lk
             zeta%zx_nn(k,l) = 2.0*prefactor*saftvrmie_param%ms(k)*&
                  saftvrmie_param%ms(l)*dhs%d(k,l)**3&
                  -(saftvrmie_param%ms(k)*zeta%zx_n(l)&
                  +saftvrmie_param%ms(l)*zeta%zx_n(k))/ns
          enddo
       enddo
       zeta%zx_VVV = -6.0*zeta%zx/V**3
       zeta%zx_VVT = 2.0*zeta%zx_T/V**2
       zeta%zx_VTn = -zeta%zx_Tn/V
       zeta%zx_VVn = 2.0*zeta%zx_n/V**2
       zeta%zx_Vnn = -zeta%zx_nn/V
       zeta%zx_VTT = -zeta%zx_TT/V
    endif
  end subroutine calcZetaX_vdW

  !> Calculate hypotetical pure fluid packing fraction
  !! Store results in saftvrmie_var
  !!
  !! \author Ailo Aasen, January 2019
  subroutine calcZetaX_leonard(nc,T,V,n,difflevel,dhs,zeta)
    use saftvrmie_hardsphere, only: calc_eta_dij
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< 0-2. Differential order
    type(saftvrmie_dhs), intent(in) :: dhs
    type(saftvrmie_zeta), intent(inout) :: zeta

    call calc_eta_dij(nc,n,V,difflevel,dhs,eta=zeta)

  end subroutine calcZetaX_leonard


  !> Calculate hypotetical pure fluid packing fraction
  !! Store results in saftvrmie_var
  !!
  !! \author Morten Hammer, January 2019
  subroutine calcZetaX_linear(nc,T,V,n,difflevel,dhs,zeta)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< 0-2. Differential order
    type(saftvrmie_dhs), intent(in) :: dhs
    type(saftvrmie_zeta), intent(inout) :: zeta
    ! Output
    ! Locals
    integer :: i,k
    real :: prefactor
    prefactor = pi*N_AVOGADRO/(6.0*V)
    zeta%zx = 0.0
    do i=1,nc
       zeta%zx = zeta%zx + saftvrmie_param%ms(i)*n(i)*dhs%d(i,i)**3
    enddo
    zeta%zx = prefactor*zeta%zx
    if (difflevel > 0) then
       zeta%zx_T = 0.0
       do i=1,nc
          zeta%zx_T = zeta%zx_T + saftvrmie_param%ms(i)*n(i)*dhs%d(i,i)**2*dhs%d_T(i,i)
       enddo
       zeta%zx_T = 3.0*prefactor*zeta%zx_T
       zeta%zx_V = -zeta%zx/V
       do k=1,nc
          zeta%zx_n(k) = prefactor*saftvrmie_param%ms(k)*dhs%d(k,k)**3
       enddo
    endif
    if (difflevel > 1) then
       zeta%zx_TT = 0.0
       do i=1,nc
          zeta%zx_TT = zeta%zx_TT + prefactor*saftvrmie_param%ms(i)*n(i)*(&
               6.0*dhs%d(i,i)*dhs%d_T(i,i)**2 &
               + 3.0*dhs%d(i,i)**2*dhs%d_TT(i,i))
       enddo
       zeta%zx_VV = 2.0*zeta%zx/V**2
       zeta%zx_TV = -zeta%zx_T/V
       zeta%zx_Vn = -zeta%zx_n/V
       do k=1,nc
          zeta%zx_Tn(k) = 3.0*prefactor*saftvrmie_param%ms(k)*&
               dhs%d(k,k)**2*dhs%d_T(k,k)
       enddo
       zeta%zx_nn = 0.0
       !
       zeta%zx_VVV = -6.0*zeta%zx/V**3
       zeta%zx_VVT = 2.0*zeta%zx_T/V**2
       zeta%zx_VTn = -zeta%zx_Tn/V
       zeta%zx_VVn = 2.0*zeta%zx_n/V**2
       zeta%zx_Vnn = -zeta%zx_nn/V
       zeta%zx_VTT = -zeta%zx_TT/V
    endif
  end subroutine calcZetaX_linear

  !> Calculate zeta prefactor
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcZetaPreFactor(nc,T,V,n,difflevel,zeta)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< 0-2. Differential order
    type(saftvrmie_zeta), intent(inout) :: zeta
    ! Output
    ! Locals
    real :: prefactor, ns
    ns = sum(saftvrmie_param%ms*n)
    prefactor = pi*N_AVOGADRO*ns/(6.0*V)
    zeta%zx = prefactor
    if (difflevel > 0) then
       zeta%zx_T = 0.0
       zeta%zx_V = -zeta%zx/V
       zeta%zx_n = saftvrmie_param%ms*prefactor/ns
    endif
    if (difflevel > 1) then
       zeta%zx_TT = 0.0
       zeta%zx_VV = 2.0*zeta%zx/V**2
       zeta%zx_TV = 0.0
       zeta%zx_Vn = -zeta%zx_n/V
       zeta%zx_Tn = 0.0
       zeta%zx_nn = 0.0
       zeta%zx_VVV = -6.0*zeta%zx/V**3
       zeta%zx_VVT = 0.0
       zeta%zx_VTn = 0.0
       zeta%zx_VVn = 2.0*zeta%zx_n/V**2
       zeta%zx_Vnn = 0.0
       zeta%zx_VTT = 0.0
    endif
  end subroutine calcZetaPreFactor

  !> Calculate Helmholtz free energy of hard-core Sutherland particle
  !! The potential is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA1Sutherland(eta,lambda,eps,a1s,a1s_e,a1s_ee,a1s_eee)
    ! Input
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda !< Mie potential exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    ! Output
    real, intent(out) :: a1s,a1s_e,a1s_ee,a1s_eee
    ! Locals
    real :: ef,ef_e,ef_ee,ef_eee
    real :: prefactor, denum, temp1, temp2, temp3
    call calcEffEta(eta,lambda,ef,ef_e,ef_ee,ef_eee)
    prefactor = 6.0*eps/(lambda - 3.0)
    denum = (1.0 - ef)**3
    a1s = -2.0*prefactor*(1.0 - 0.5*ef)/denum
    temp1 = -prefactor*(5.0 - 2.0*ef)/denum/(1.0 - ef)
    a1s_e = temp1*ef_e
    temp2 = 6.0*prefactor*(ef - 3.0)/denum/(1.0 - ef)**2
    a1s_ee = temp1*ef_ee + temp2*ef_e**2
    temp3 = 12.0*prefactor*(2.0*ef - 7.0)/denum**2
    a1s_eee = temp1*ef_eee + 3.0*temp2*ef_e*ef_ee + temp3*ef_e**3
  end subroutine calcA1Sutherland

  !> Calculate utility function for the
  !! Helmholtz free energy of hard-core Sutherland particle
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcEffEta(eta,lambda,ef,ef_e,ef_ee,ef_eee)
    ! Input
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda !< Mie potential exponent
    ! Output
    real, intent(out) :: ef,ef_e,ef_ee,ef_eee
    ! Locals
    real, parameter :: lam_coeff(4,4) = reshape((/ 0.81096, 1.7888, -37.578, &
         92.284, 1.0205, -19.341, 151.26, -463.50, -1.9057, 22.845, -228.14, 973.92, &
         1.0885, -6.1962, 106.98, -677.64 /), (/4,4/))
    real :: c(4), inv_lam(4)
    integer :: i
    inv_lam(1) = 1.0
    do i=2,4
       inv_lam(i) = inv_lam(i-1)/lambda
    enddo
    do i=1,4
       c(i) = sum(lam_coeff(:,i)*inv_lam)
    enddo
    ef = eta*(c(1) + eta*(c(2) + eta*(c(3) + eta*c(4))))
    ef_e = c(1) + eta*(2.0*c(2) + eta*(3.0*c(3) + 4.0*eta*c(4)))
    ef_ee = 2.0*c(2) + eta*(6.0*c(3) + 12.0*c(4)*eta)
    ef_eee = 6.0*c(3) + 24.0*c(4)*eta
  end subroutine calcEffEta

  !> Calculate utility function for B
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcILambda(x0,lambda,I,I_x,I_xx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: lambda !< Mie potential exponent
    ! Output
    real, intent(out) :: I,I_x,I_xx
    !
    I = - (x0**(3.0 - lambda) - 1.0)/(lambda - 3.0)
    I_x = x0**(2.0 - lambda)
    I_xx = (2.0 - lambda)*x0**(1.0 - lambda)
  end subroutine calcILambda

  !> Calculate utility function for B
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcJLambda(x0,lambda,J,J_x,J_xx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: lambda !< Mie potential exponent
    ! Output
    real, intent(out) :: J,J_x,J_xx
    !
    J = - ((lambda - 3.0)*x0**(4.0 - lambda) - (lambda - 4.0)*x0**(3.0 - lambda) - 1.0)/&
         ((lambda - 3.0)*(lambda - 4.0))
    J_x = x0**(3.0 - lambda) - x0**(2.0 - lambda)
    J_xx = (3.0 - lambda)*x0**(2.0 - lambda) - (2.0 - lambda)*x0**(1.0 - lambda)
  end subroutine calcJLambda

  !> Calculate equation 33 of Lafitte 2013
  !! B is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcBtilde(x0,eta,lambda,eps,B,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx,fac_in)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda !< Mie potential exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in), optional :: fac_in !< Prefactor needed for quantum corrections
    ! Output
    real, intent(out) :: B,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx
    ! Locals
    real :: J,J_x,J_xx,I,I_x,I_xx, keta(2,0:3)
    real :: denum(3:6)
    real :: fac
    if (present(fac_in)) then
       fac = fac_in
    else
       fac = 1.0
    end if

    call calcJLambda(x0,lambda,J,J_x,J_xx)
    call calcILambda(x0,lambda,I,I_x,I_xx)
    denum(3) = (1.0-eta)**3
    denum(4) = (1.0-eta)*denum(3)
    denum(5) = (1.0-eta)*denum(4)
    denum(6) = denum(3)*denum(3)
    keta(1,0) = (2.0-eta)/denum(3)
    keta(1,1) = (5.0-2.0*eta)/denum(4)
    keta(1,2) = 6.0*(3.0-eta)/denum(5)
    keta(1,3) = 12.0*(7.0-2.0*eta)/denum(6)
    keta(2,0) = -9.0*eta*(1.0+eta)/denum(3)
    keta(2,1) = -9.0*(eta**2 + 4.0*eta + 1.0)/denum(4)
    keta(2,2) = -18.0*(eta**2 + 7.0*eta + 4.0)/denum(5)
    keta(2,3) = -54.0*(eta**2 + 10.0*eta + 9.0)/denum(6)
    B = 6.0*eps*(keta(1,0)*I + keta(2,0)*J)*fac
    B_e = 6.0*eps*(keta(1,1)*I + keta(2,1)*J)*fac
    B_ee = 6.0*eps*(keta(1,2)*I + keta(2,2)*J)*fac
    B_eee = 6.0*eps*(keta(1,3)*I + keta(2,3)*J)*fac
    B_x = 6.0*eps*(keta(1,0)*I_x + keta(2,0)*J_x)*fac
    B_xx = 6.0*eps*(keta(1,0)*I_xx + keta(2,0)*J_xx)*fac
    B_ex = 6.0*eps*(keta(1,1)*I_x + keta(2,1)*J_x)*fac
    B_eex = 6.0*eps*(keta(1,2)*I_x + keta(2,2)*J_x)*fac
    B_exx = 6.0*eps*(keta(1,1)*I_xx + keta(2,1)*J_xx)*fac
  end subroutine calcBtilde

  !> Calculate the quantity (x0^lambda)(a1stilde(eta,lambda)+Btilde(eta,lambda))
  !! \author Ailo Aasen, March 2018
  subroutine calcX0AplusBtilde(x0,eta,lambda,eps,&
       u1,u1_e,u1_x,u1_ee,u1_xx,u1_ex,u1_eee,u1_eex,u1_exx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda !< Mie potential attractive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    ! Output
    real, intent(out) :: u1,u1_e,u1_x,u1_ee,u1_xx,u1_ex,u1_eee,u1_eex,u1_exx
    ! Locals
    real :: B,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx
    real :: as,as_e,as_ee,as_eee
    call calcA1Sutherland(eta,lambda,eps,as,as_e,as_ee,as_eee)
    if (svrm_opt%quantum_correction_hs <= 0 .and. x0 > 1) then
       ! Calculate integral from d to sigma
       call calcBtilde(x0,eta,lambda,eps,B,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx)
    else
       ! Calculate integral from d to sigma_eff elsewhere
       B=0.0
       B_e=0.0
       B_x=0.0
       B_ee=0.0
       B_xx=0.0
       B_ex=0.0
       B_eee=0.0
       B_eex=0.0
       B_exx=0.0
    endif
    u1 = x0**lambda*(as+B)
    u1_x = lambda*x0**(lambda-1.0)*(as+B) + x0**lambda*B_x
    u1_xx = (lambda-1.0)*lambda*x0**(lambda-2.0)*(as+B)&
         + 2.0*lambda*x0**(lambda-1.0)*B_x &
         + x0**lambda*B_xx
    u1_e = x0**lambda*(as_e+B_e)
    u1_ee = x0**lambda*(as_ee+B_ee)
    u1_eee = x0**lambda*(as_eee+B_eee)
    u1_ex = lambda*x0**(lambda-1.0)*(as_e+B_e) + x0**lambda*B_ex
    u1_exx = ((lambda-1.0)*lambda*x0**(lambda-2.0)*(as_e+B_e)&
         + 2.0*lambda*x0**(lambda-1.0)*B_ex &
         + x0**lambda*B_exx )
    u1_eex = lambda*x0**(lambda-1.0)*(as_ee+B_ee) + x0**lambda*B_eex
  end subroutine calcX0AplusBtilde

  !> Calculate a1Q, multiplied by T/eta (and kB?)
  !!
  !! \author Ailo Aasen, March 2018
  subroutine calcA1QTilde(x0,eta,lambda_a,lambda_r,eps,C,&
       Q1_r,Q1_a,a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,&
       a1_eex,a1_exx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    real, intent(in) :: Q1_r !< 1-st order repulsive q-correction par.
    real, intent(in) :: Q1_a !< 1-st order attractive q-correction par.
    ! Output
    real, intent(out) :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx
    ! Locals
    real :: a1a,a1a_e,a1a_x,a1a_ee,a1a_xx,a1a_ex,a1a_eee,a1a_eex,a1a_exx
    real :: a1r,a1r_e,a1r_x,a1r_ee,a1r_xx,a1r_ex,a1r_eee,a1r_eex,a1r_exx
    real :: afac, rfac

    if (svrm_opt%quantum_correction<=0) then
       call stoperror("calcA1QTilde: only for quantum_correction>0")
    end if

    call calcX0AplusBtilde(x0,eta,lambda_r+2,eps,&
         a1r,a1r_e,a1r_x,a1r_ee,a1r_xx,a1r_ex,a1r_eee,a1r_eex,a1r_exx)
    call calcX0AplusBtilde(x0,eta,lambda_a+2,eps,&
         a1a,a1a_e,a1a_x,a1a_ee,a1a_xx,a1a_ex,a1a_eee,a1a_eex,a1a_exx)
    rfac = C*Q1_r
    afac = C*Q1_a
    a1 = afac*a1a - rfac*a1r
    a1_e = afac*a1a_e - rfac*a1r_e
    a1_x = afac*a1a_x - rfac*a1r_x
    a1_ee = afac*a1a_ee - rfac*a1r_ee
    a1_xx = afac*a1a_xx - rfac*a1r_xx
    a1_ex = afac*a1a_ex - rfac*a1r_ex
    a1_eee = afac*a1a_eee - rfac*a1r_eee
    a1_eex = afac*a1a_eex - rfac*a1r_eex
    a1_exx = afac*a1a_exx - rfac*a1r_exx
  end subroutine calcA1QTilde

  !> Calculate a1QQ, multiplied by T/eta (and kB?)
  !!
  !! \author Ailo Aasen, March 2018
  subroutine calcA1QQTilde(x0,eta,lambda_a,lambda_r,eps,C,&
       Q2_r,Q2_a,a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,&
       a1_eex,a1_exx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    real, intent(in) :: Q2_r !< 2-st order repulsive q-correction par.
    real, intent(in) :: Q2_a !< 2-st order attractive q-correction par.
    ! Output
    real, intent(out) :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx
    ! Locals
    real :: a1a,a1a_e,a1a_x,a1a_ee,a1a_xx,a1a_ex,a1a_eee,a1a_eex,a1a_exx
    real :: a1r,a1r_e,a1r_x,a1r_ee,a1r_xx,a1r_ex,a1r_eee,a1r_eex,a1r_exx
    real :: afac, rfac

    if (svrm_opt%quantum_correction<=1) then
       call stoperror("calcA1QQTilde: need quantum correction >= 2")
    end if

    call calcX0AplusBtilde(x0,eta,lambda_r+4,eps,&
         a1r,a1r_e,a1r_x,a1r_ee,a1r_xx,a1r_ex,a1r_eee,a1r_eex,a1r_exx)
    call calcX0AplusBtilde(x0,eta,lambda_a+4,eps,&
         a1a,a1a_e,a1a_x,a1a_ee,a1a_xx,a1a_ex,a1a_eee,a1a_eex,a1a_exx)
    rfac = C*Q2_r
    afac = C*Q2_a
    a1 = afac*a1a - rfac*a1r
    a1_e = afac*a1a_e - rfac*a1r_e
    a1_x = afac*a1a_x - rfac*a1r_x
    a1_ee = afac*a1a_ee - rfac*a1r_ee
    a1_xx = afac*a1a_xx - rfac*a1r_xx
    a1_ex = afac*a1a_ex - rfac*a1r_ex
    a1_eee = afac*a1a_eee - rfac*a1r_eee
    a1_eex = afac*a1a_eex - rfac*a1r_eex
    a1_exx = afac*a1a_exx - rfac*a1r_exx
  end subroutine calcA1QQTilde


  !> Calculate first order monomer perturbation (a1)
  !! a1 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA1Tilde(x0,eta,lambda_a,lambda_r,eps,C,&
       a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    ! Output
    real, intent(out) :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx
    ! Locals
    real :: Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx
    real :: Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx
    real :: asr,asr_e,asr_ee,asr_eee
    real :: asa,asa_e,asa_ee,asa_eee
    call calcA1Sutherland(eta,lambda_r,eps,asr,asr_e,asr_ee,asr_eee)
    call calcA1Sutherland(eta,lambda_a,eps,asa,asa_e,asa_ee,asa_eee)
    if (svrm_opt%quantum_correction_hs <= 0 .and. x0 > 1) then
       ! Calculate integral form d to sigma
       call calcBtilde(x0,eta,lambda_r,eps,Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx)
       call calcBtilde(x0,eta,lambda_a,eps,Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx)
    else
       ! Calculate integral form d to sigma_eff elsewhere
       Br=0.0
       Br_e=0.0
       Br_x=0.0
       Br_ee=0.0
       Br_xx=0.0
       Br_ex=0.0
       Br_eee=0.0
       Br_eex=0.0
       Br_exx=0.0
       !
       Ba=0.0
       Ba_e=0.0
       Ba_x=0.0
       Ba_ee=0.0
       Ba_xx=0.0
       Ba_ex=0.0
       Ba_eee=0.0
       Ba_eex=0.0
       Ba_exx=0.0
    endif
    a1 = C*(x0**lambda_a*(asa+Ba) - x0**lambda_r*(asr+Br))
    a1_x = C*(lambda_a*x0**(lambda_a-1.0)*(asa+Ba) - lambda_r*x0**(lambda_r-1.0)*(asr+Br) &
         +x0**lambda_a*Ba_x - x0**lambda_r*Br_x)
    a1_xx = C*((lambda_a-1.0)*lambda_a*x0**(lambda_a-2.0)*(asa+Ba)&
         - (lambda_r-1.0)*lambda_r*x0**(lambda_r-2.0)*(asr+Br) &
         + 2.0*lambda_a*x0**(lambda_a-1.0)*Ba_x - 2.0*lambda_r*x0**(lambda_r-1.0)*Br_x &
         +x0**lambda_a*Ba_xx - x0**lambda_r*Br_xx)
    a1_e = C*(x0**lambda_a*(asa_e+Ba_e) - x0**lambda_r*(asr_e+Br_e))
    a1_ee = C*(x0**lambda_a*(asa_ee+Ba_ee) - x0**lambda_r*(asr_ee+Br_ee))
    a1_eee = C*(x0**lambda_a*(asa_eee+Ba_eee) - x0**lambda_r*(asr_eee+Br_eee))
    a1_ex = C*(lambda_a*x0**(lambda_a-1.0)*(asa_e+Ba_e) &
         - lambda_r*x0**(lambda_r-1.0)*(asr_e+Br_e) &
         + x0**lambda_a*Ba_ex - x0**lambda_r*Br_ex)
    a1_exx = C*((lambda_a-1.0)*lambda_a*x0**(lambda_a-2.0)*(asa_e+Ba_e)&
         - (lambda_r-1.0)*lambda_r*x0**(lambda_r-2.0)*(asr_e+Br_e) &
         + 2.0*lambda_a*x0**(lambda_a-1.0)*Ba_ex - 2.0*lambda_r*x0**(lambda_r-1.0)*Br_ex &
         +x0**lambda_a*Ba_exx - x0**lambda_r*Br_exx)
    a1_eex = C*(lambda_a*x0**(lambda_a-1.0)*(asa_ee+Ba_ee) &
         - lambda_r*x0**(lambda_r-1.0)*(asr_ee+Br_ee) &
         +x0**lambda_a*Ba_eex - x0**lambda_r*Br_eex)
  end subroutine calcA1Tilde

  !> Calculate integral from d  to sigma_eff for a1.
  !! a1 is divided by the packing fraction
  !!
  !! \author Morten Hammer, March 2018
  subroutine calcIcTilde(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,eps,C,&
       a1,a1_e,a1_T,a1_ee,a1_TT,a1_eT,a1_eee,a1_eeT,a1_eTT,fac_r_in,fac_a_in)
    ! Input
    real, intent(in) :: x,x_T,x_TT !< Reduced center-center hard sphere distance (sigma/d)
    real, intent(in) :: y,y_T,y_TT !< Reduced center-center hard sphere distance (sigma_eff/d)
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    real, intent(in), optional :: fac_r_in, fac_a_in !< Prefactors needed for quantum corrections
    ! Output
    real, intent(out) :: a1,a1_e,a1_T,a1_ee,a1_TT,a1_eT,a1_eee,a1_eeT,a1_eTT
    ! Locals
    real :: a1a,a1a_e,a1a_T,a1a_ee,a1a_TT,a1a_eT,a1a_eee,a1a_eeT,a1a_eTT
    real :: a1r,a1r_e,a1r_T,a1r_ee,a1r_TT,a1r_eT,a1r_eee,a1r_eeT,a1r_eTT
    real :: fac_r, fac_a

    if ( present(fac_r_in) .and. present(fac_a_in) ) then
       fac_r = fac_r_in
       fac_a = fac_a_in
    else
       fac_r = 1.0
       fac_a = 1.0
    end if

    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_a,eps,C,&
         a1a,a1a_e,a1a_T,a1a_ee,a1a_TT,a1a_eT,a1a_eee,a1a_eeT,a1a_eTT,fac_in=fac_a)
    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_r,eps,C,&
         a1r,a1r_e,a1r_T,a1r_ee,a1r_TT,a1r_eT,a1r_eee,a1r_eeT,a1r_eTT,fac_in=fac_r)

    a1 = a1a - a1r
    a1_e = a1a_e - a1r_e
    a1_ee = a1a_ee - a1r_ee
    a1_eee = a1a_eee - a1r_eee
    a1_T = a1a_T - a1r_T
    a1_TT = a1a_TT - a1r_TT
    a1_eT = a1a_eT - a1r_eT
    a1_eeT = a1a_eeT - a1r_eeT
    a1_eTT = a1a_eTT - a1r_eTT
  end subroutine calcIcTilde

  !> Calculate integral from sigma_eff to sigma for single term in potential.
  !! a1 is divided by the packing fraction
  !!
  !! \author Morten Hammer, March 2018
  subroutine calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda,eps,C,&
       a1,a1_e,a1_T,a1_ee,a1_TT,a1_eT,a1_eee,a1_eeT,a1_eTT,fac_in)
    ! Input
    real, intent(in) :: x,x_T,x_TT !< Reduced center-center hard sphere distance (sigma/d)
    real, intent(in) :: y,y_T,y_TT !< Reduced center-center hard sphere distance (sigma_eff/d)
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda !< Mie potential exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    real, intent(in), optional :: fac_in !< Prefactors needed for quantum corrections
    ! Output
    real, intent(out) :: a1,a1_e,a1_T,a1_ee,a1_TT,a1_eT,a1_eee,a1_eeT,a1_eTT
    ! Locals
    real :: Bea,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,Bea_eee,Bea_eey,Bea_eyy
    real :: a1_x,a1_xx,a1_ex,a1_eex,a1_exx,a1_y,a1_yy,a1_ey,a1_eey,a1_eyy
    real :: a1_xy,a1_exy
    real :: fac

    if (present(fac_in)) then
       fac = fac_in
    else
       fac = 1.0
    end if
    call calcBtilde(y,eta,lambda,eps,Bea,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,&
         Bea_eee,Bea_eey,Bea_eyy,fac_in=fac)

    a1 = C*(x**lambda*Bea)

    a1_x = C*lambda*x**(lambda-1.)*Bea
    a1_y = C*(x**lambda*Bea_y)
    a1_T = a1_x*x_T + a1_y*y_T

    a1_xx = C*(lambda-1.0)*lambda*x**(lambda-2.0)*Bea
    a1_yy = C*(x**lambda*Bea_yy)
    a1_xy = C*(lambda*x**(lambda-1.)*Bea_y)
    a1_TT = a1_xx*x_T**2 + a1_x*x_TT + 2.0*a1_xy*x_T*y_T + a1_yy*y_T**2 + a1_y*y_TT

    a1_e = C*(x**lambda*Bea_e)
    a1_ee = C*(x**lambda*Bea_ee)
    a1_eee = C*(x**lambda*Bea_eee)

    a1_ex = C*(lambda*x**(lambda-1.0)*Bea_e)
    a1_ey = C*(x**lambda*Bea_ey)
    a1_eT = a1_ex*x_T + a1_ey*y_T

    a1_exx = C*(lambda-1.0)*lambda*x**(lambda-2.0)*Bea_e
    a1_exy = C*(lambda*x**(lambda-1.0)*Bea_ey)
    a1_eyy = C*(x**lambda*Bea_eyy)
    a1_eTT = a1_exx*x_T**2 + a1_ex*x_TT + 2.0*a1_exy*x_T*y_T + a1_eyy*y_T**2 + a1_ey*y_TT

    a1_eex = C*lambda*x**(lambda-1.0)*Bea_ee
    a1_eey = C*(x**lambda*Bea_eey)
    a1_eeT = a1_eex*x_T + a1_eey*y_T

  end subroutine calcIcTildeSingleTerm

  subroutine calcIcTilde_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_a,lambda_r,eps,C,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel,fac_r_in,fac_a_in)
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_zeta), intent(in) :: eta
    type(saftvrmie_zeta), intent(in) :: x0z,x1z
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    integer, optional, intent(in) :: difflevel
    real, intent(in), optional :: fac_r_in, fac_a_in !< Prefactors needed for quantum corrections
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    real :: fac_r, fac_a
    real :: a1r, a1r_T,a1r_V,a1r_TT,a1r_VV,a1r_TV,a1r_VVV,a1r_VVT,a1r_VTT
    real, dimension(nc) :: a1r_n,a1r_Tn,a1r_Vn,a1r_VVn,a1r_VTn
    real, dimension(nc,nc) :: a1r_nn,a1r_Vnn

    if ( present(fac_r_in) .and. present(fac_a_in) ) then
       fac_r = -fac_r_in
       fac_a = fac_a_in
    else
       fac_r = -1.0
       fac_a = 1.0
    end if

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_a,eps,C,fac_a,&
         a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel)

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_r,eps,C,fac_r,&
         a1r,a1r_T,a1r_V,a1r_n,a1r_TT,a1r_VV,a1r_TV,a1r_Tn,a1r_Vn,a1r_nn,&
         a1r_VVV,a1r_VVT,a1r_VTT,a1r_VVn,a1r_Vnn,a1r_VTn,difflevel)

    call calc_a0_plus_a1(nc,a1r,a1,&
         a1r_T,a1r_V,a1r_n,&
         a1_T,a1_V,a1_n,&
         a1r_TT,a1r_VV,a1r_TV,a1r_Tn,a1r_Vn,a1r_nn,&
         a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1r_VVV,a1r_VVT,a1r_VTT,a1r_VVn,a1r_Vnn,a1r_VTn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

  end subroutine calcIcTilde_TVn_PureHSRef

  !> Calculate integral from sigma_eff to sigma for single term in potential.
  !! a1 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2019
  subroutine calcIcTildeSingleTerm_TVn_PureHSRef(nc,x,y,eta,lambda,eps,C,fac,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel)
    ! Input
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_zeta), intent(in) :: x !< Reduced center-center hard sphere distance (sigma/d)
    type(saftvrmie_zeta), intent(in) :: y !< Reduced center-center hard sphere distance (sigma_eff/d)
    type(saftvrmie_zeta), intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda !< Mie potential exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: C !< Mie potential prefactor
    integer, optional, intent(in) :: difflevel
    real, intent(in) :: fac !< Prefactors needed for quantum corrections
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    real :: Bea,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,Bea_eee,Bea_eey,Bea_eyy
    real :: fac_c, x0, lx0l, llx0ll, y0, e0
    real ::  a0, a0_T,a0_V,a0_TT,a0_VV,a0_TV,a0_VVV,a0_VVT,a0_VTT
    real, dimension(nc) :: a0_n,a0_Tn,a0_Vn,a0_VVn,a0_VTn
    real, dimension(nc,nc) :: a0_nn,a0_Vnn
    integer :: k
    fac_c = C*fac
    y0 = y%zx
    e0 = eta%zx
    call calcBtilde(y0,e0,lambda,eps,Bea,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,&
         Bea_eee,Bea_eey,Bea_eyy,fac_in=fac_c)

    ! First calculate TVn for Bea
    a1 = Bea
    call convert_zeta_zeta_to_TVn(nc,y,eta,&
         a1,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,Bea_eee,0.0,Bea_eey,Bea_eyy,&
         a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel=difflevel)

    ! a0 = x^lambda
    x0 = x%zx
    lx0l = lambda*x0**(lambda-1.)
    llx0ll = (lambda-1.)*lambda*x0**(lambda-2.)
    a0 = x0**lambda
    a0_T = lx0l*x%zx_T
    a0_n = lx0l*x%zx_n
    a0_TT = lx0l*x%zx_TT + llx0ll*x%zx_T**2
    a0_Tn = lx0l*x%zx_Tn + llx0ll*x%zx_T*x%zx_n
    do k=1,nc
       a0_nn(:,k) = lx0l*x%zx_nn(:,k) + llx0ll*x%zx_n(:)*x%zx_n(k)
    enddo
    !
    a0_VVV = 0
    a0_VVT = 0
    a0_VTT = 0
    a0_VVn = 0
    a0_Vnn = 0
    a0_VTn = 0
    a0_Vn = 0
    a0_V = 0
    a0_VV = 0
    a0_TV = 0

    call calc_a0_a_product(nc,a0,a1,&
         a0_T,a0_V,a0_n,&
         a1_T,a1_V,a1_n,&
         a0_TT,a0_VV,a0_TV,a0_Tn,a0_Vn,a0_nn,&
         a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a0_VVV,a0_VVT,a0_VTT,a0_VVn,a0_Vnn,a0_VTn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

  end subroutine calcIcTildeSingleTerm_TVn_PureHSRef

  !> Calculate first order monomer perturbation (a1)
  !! a1 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA1TildeTVn(nc,i,j,s_vc,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    real :: a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: eta !< Packing fraction
    real :: lambda_a !< Mie potential attractive exponent
    real :: lambda_r !< Mie potential repulsive exponent
    real :: eps !< Well depth div. temperature (K)
    real :: C !< Mie potential prefactor
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Sigma
    real :: y, y_T, y_TT !< Reduced center-center hard sphere distance at sigma_eff
    real :: a1C_T,a1C_V,a1C_TT,a1C_VV,a1C_TV,a1C_VVV,a1C_VVT,a1C_VTT
    real, dimension(nc) :: a1C_n,a1C_Tn,a1C_Vn,a1C_VVn,a1C_VTn
    real, dimension(nc,nc) :: a1C_nn,a1C_Vnn
    real :: a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT
    integer :: difflevel
    type(saftvrmie_zeta) :: x0z, x1z
    difflevel = 1
    if (present(a1_VVV) .or. present(a1_VVT) .or. present(a1_VTT) .or. &
         present(a1_VVn) .or. present(a1_Vnn) .or. present(a1_VTn)) then
       difflevel = 3
    else if (present(a1_VV) .or. present(a1_TV) .or. present(a1_TT) .or. &
         present(a1_Vn) .or. present(a1_nn) .or. present(a1_Tn)) then
       difflevel = 2
    endif
    s = saftvrmie_param%sigma_ij(i,j)
    s_T = 0.0
    s_TT = 0.0
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       d = s_vc%d_pure%zx
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x0z)
       x0 = x0z%zx
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    endif
    eta = s_vc%zeta%zx
    lambda_a = saftvrmie_param%lambda_a_ij(i,j)
    lambda_r = saftvrmie_param%lambda_r_ij(i,j)
    eps = saftvrmie_param%eps_divk_ij(i,j)
    C = saftvrmie_param%Cij(i,j)
    call calcA1Tilde(x0,eta,lambda_a,lambda_r,eps,C,&
         a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx)
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
            a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,0.0,a1_eex,a1_exx,&
            a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel=difflevel)
    else
       call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,s_vc%zeta,&
            a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx,&
            a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel=difflevel)
    endif
    if (svrm_opt%quantum_correction_hs > 0) then
       ! Shift integral limits to sigma_eff instead of sigma
       s = s_vc%sigma_eff%d(i,j)
       s_T = s_vc%sigma_eff%d_T(i,j)
       s_TT = s_vc%sigma_eff%d_TT(i,j)
       if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
          call allocate_saftvrmie_zeta(nc,x1z)
          call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x1z)
          y = x1z%zx
       else
          call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
       endif
       if (y > 1) then
          if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
             call calcIcTilde_TVn_PureHSRef(nc,x0z,x1z,s_vc%zeta,lambda_a,lambda_r,eps,C,&
                  a1Cc,a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                  a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=difflevel)
          else
             call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,eps,C,&
                  a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT)
             call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,s_vc%zeta,&
                  a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
                  a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                  a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=difflevel)
          endif
          call calc_a0_plus_a1(nc,a1Cc,a1,&
               a1C_T,a1C_V,a1C_n,&
               a1_T,a1_V,a1_n,&
               a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
               a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
               a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,&
               a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
       endif
    endif
    call cleanup_saftvrmie_zeta(x0z)
    call cleanup_saftvrmie_zeta(x1z)
  end subroutine calcA1TildeTVn


  !> Calculate a1 quantum correction. a1 is divided by the packing fraction
  !!
  !! \author Ailo Aasen, March 2018
  subroutine calcA1TildeTVnQuantumCorrection(nc,T,V,n,i,j,s_vc, &
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    real :: a1Q, a1Q_e,a1Q_x,a1Q_ee,a1Q_xx,a1Q_ex,a1Q_eee,a1Q_eex,a1Q_exx
    real :: a1Q_T,a1Q_V,a1Q_TT,a1Q_VV,a1Q_TV,a1Q_VVV,a1Q_VVT,a1Q_VTT
    real, dimension(nc) :: a1Q_n,a1Q_Tn,a1Q_Vn,a1Q_VVn,a1Q_VTn
    real, dimension(nc,nc) :: a1Q_nn,a1Q_Vnn
    real :: a1QQ, a1QQ_e,a1QQ_x,a1QQ_ee,a1QQ_xx,a1QQ_ex,a1QQ_eee,a1QQ_eex,a1QQ_exx
    real :: a1QQ_T,a1QQ_V,a1QQ_TT,a1QQ_VV,a1QQ_TV,a1QQ_VVV,a1QQ_VVT,a1QQ_VTT
    real, dimension(nc) :: a1QQ_n,a1QQ_Tn,a1QQ_Vn,a1QQ_VVn,a1QQ_VTn
    real, dimension(nc,nc) :: a1QQ_nn,a1QQ_Vnn
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: eta !< Packing fraction
    real :: lambda_a, lambda_r !< Mie potential attractive and repulsive exponent
    real :: eps !< Well depth div. temperature (K)
    real :: C !< Mie potential prefactor
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Sigma effective
    real :: Q1_r !< 1st order repulsive q-correction par.
    real :: Q1_a !< 1st order attractive q-correction par.
    real :: Q2_r !< 2nd order repulsive q-correction par.
    real :: Q2_a !< 2nd order attractive q-correction par.
    real :: DFH, DFH_T, DFH_TT, DFH2, DFH2_T, DFH2_TT !< The Feynman--Hibbs D and D^2
    real :: y, y_T, y_TT !< Reduced center-center hard sphere distance at sigma_eff
    real :: a1C_T,a1C_V,a1C_TT,a1C_VV,a1C_TV,a1C_VVV,a1C_VVT,a1C_VTT
    real, dimension(nc) :: a1C_n,a1C_Tn,a1C_Vn,a1C_VVn,a1C_VTn
    real, dimension(nc,nc) :: a1C_nn,a1C_Vnn
    real :: a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT
    integer :: difflevel
    type(saftvrmie_zeta) :: x0z, x1z

    difflevel = 1
    if (present(a1_VVV) .or. present(a1_VVT) .or. present(a1_VTT) .or. &
         present(a1_VVn) .or. present(a1_Vnn) .or. present(a1_VTn)) then
       difflevel = 3
    else if (present(a1_VV) .or. present(a1_TV) .or. present(a1_TT) .or. &
         present(a1_Vn) .or. present(a1_nn) .or. present(a1_Tn)) then
       difflevel = 2
    endif

    ! Null out the variables, as we want to successively ADD contributions to them
    a1 = 0.0
    if (present(a1_V)) a1_V = 0.0
    if (present(a1_n)) a1_n = 0.0
    if (present(a1_T)) a1_T = 0.0
    if (present(a1_TT)) a1_TT = 0.0
    if (present(a1_VV)) a1_VV = 0.0
    if (present(a1_TV)) a1_TV = 0.0
    if (present(a1_Tn)) a1_Tn = 0.0
    if (present(a1_Vn)) a1_Vn = 0.0
    if (present(a1_nn)) a1_nn = 0.0
    if (present(a1_VVV)) a1_VVV = 0.0
    if (present(a1_VVT)) a1_VVT = 0.0
    if (present(a1_VTT)) a1_VTT = 0.0
    if (present(a1_VVn)) a1_VVn = 0.0
    if (present(a1_Vnn)) a1_Vnn = 0.0
    if (present(a1_VTn)) a1_VTn = 0.0

    s = saftvrmie_param%sigma_ij(i,j)
    s_T = 0.0
    s_TT = 0.0
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       d = s_vc%d_pure%zx
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x0z)
       x0 = x0z%zx
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    endif
    eta = s_vc%zeta%zx
    lambda_a = saftvrmie_param%lambda_a_ij(i,j)
    lambda_r = saftvrmie_param%lambda_r_ij(i,j)
    eps = saftvrmie_param%eps_divk_ij(i,j)
    C = saftvrmie_param%Cij(i,j)
    Q1_r = saftvrmie_param%Quantum_const_1r_ij(i,j)
    Q1_a = saftvrmie_param%Quantum_const_1a_ij(i,j)

    call calcA1QTilde(x0,eta,lambda_a,lambda_r,eps,C,&
         Q1_r,Q1_a,a1Q,a1Q_e,a1Q_x,a1Q_ee,a1Q_xx,a1Q_ex,&
         a1Q_eee,a1Q_eex,a1Q_exx)
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
            a1Q,a1Q_e,a1Q_x,a1Q_ee,a1Q_xx,a1Q_ex,a1Q_eee,0.0,a1Q_eex,a1Q_exx,&
            a1Q_T,a1Q_V,a1Q_n,a1Q_TT,a1Q_VV,a1Q_TV,a1Q_Tn,a1Q_Vn,a1Q_nn,&
            a1Q_VVV,a1Q_VVT,a1Q_VTT,a1Q_VVn,a1Q_Vnn,a1Q_VTn,difflevel=difflevel)
    else
       call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,s_vc%zeta,&
            a1Q,a1Q_e,a1Q_x,a1Q_ee,a1Q_xx,a1Q_ex,a1Q_eee,a1Q_eex,a1Q_exx,&
            a1Q_T,a1Q_V,a1Q_n,a1Q_TT,a1Q_VV,a1Q_TV,a1Q_Tn,a1Q_Vn,a1Q_nn,&
            a1Q_VVV,a1Q_VVT,a1Q_VTT,a1Q_VVn,a1Q_Vnn,a1Q_VTn,difflevel=difflevel)
    endif

    if (svrm_opt%quantum_correction_hs > 0) then
       ! Shift integral limits to sigma_eff instead of sigma
       s = s_vc%sigma_eff%d(i,j)
       s_T = s_vc%sigma_eff%d_T(i,j)
       s_TT = s_vc%sigma_eff%d_TT(i,j)
       if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
          call allocate_saftvrmie_zeta(nc,x1z)
          call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x1z)
          y = x1z%zx
       else
          call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
       endif
       if (y > 1) then
          if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
             call calcIcTilde_TVn_PureHSRef(nc,x0z,x1z,s_vc%zeta,lambda_a+2,lambda_r+2,eps,C,&
                  a1Cc,a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                  a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=difflevel,&
                  fac_r_in=Q1_r,fac_a_in=Q1_a)
          else
             call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a+2,lambda_r+2,eps,C,&
                  a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
                  fac_r_in=Q1_r,fac_a_in=Q1_a)
             call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,s_vc%zeta,&
                  a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
                  a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                  a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=difflevel)
          endif
          call calc_a0_plus_a1(nc,a1Cc,a1Q,&
               a1C_T,a1C_V,a1C_n,&
               a1Q_T,a1Q_V,a1Q_n,&
               a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
               a1Q_TT,a1Q_VV,a1Q_TV,a1Q_Tn,a1Q_Vn,a1Q_nn,&
               a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,&
               a1Q_VVV,a1Q_VVT,a1Q_VTT,a1Q_VVn,a1Q_Vnn,a1Q_VTn)
       endif
    endif

    ! Add a1Q*D to a1
    call get_DFeynHibbsPower(i,j,DFH,DFH_T,DFH_TT,s_vc,power_in=1,divideBySigmaMie=.true.)
    call add_aQ_DFH_product(nc,DFH,DFH_T,DFH_TT,&
         a1Q,a1,&
         a1Q_T,a1Q_V,a1Q_n,&
         a1_T,a1_V,a1_n,&
         a1Q_TT,a1Q_VV,a1Q_TV,a1Q_Tn,a1Q_Vn,a1Q_nn,&
         a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1Q_VVV,a1Q_VVT,a1Q_VTT,a1Q_VVn,a1Q_Vnn,a1Q_VTn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

    if (svrm_opt%quantum_correction>=2) then
       ! Add FH correction proportional to (D^2)
       Q2_r = saftvrmie_param%Quantum_const_2r_ij(i,j)
       Q2_a = saftvrmie_param%Quantum_const_2a_ij(i,j)

       call calcA1QQTilde(x0,eta,lambda_a,lambda_r,eps,C,&
            Q2_r,Q2_a,a1QQ,a1QQ_e,a1QQ_x,a1QQ_ee,a1QQ_xx,a1QQ_ex,&
            a1QQ_eee,a1QQ_eex,a1QQ_exx)
       if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
          call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
               a1QQ,a1QQ_e,a1QQ_x,a1QQ_ee,a1QQ_xx,a1QQ_ex,a1QQ_eee,0.0,a1QQ_eex,a1QQ_exx,&
               a1QQ_T,a1QQ_V,a1QQ_n,a1QQ_TT,a1QQ_VV,a1QQ_TV,a1QQ_Tn,a1QQ_Vn,a1QQ_nn,&
               a1QQ_VVV,a1QQ_VVT,a1QQ_VTT,a1QQ_VVn,a1QQ_Vnn,a1QQ_VTn,difflevel=difflevel)
       else
          call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,s_vc%zeta,&
               a1QQ,a1QQ_e,a1QQ_x,a1QQ_ee,a1QQ_xx,a1QQ_ex,a1QQ_eee,a1QQ_eex,a1QQ_exx,&
               a1QQ_T,a1QQ_V,a1QQ_n,a1QQ_TT,a1QQ_VV,a1QQ_TV,a1QQ_Tn,a1QQ_Vn,a1QQ_nn,&
               a1QQ_VVV,a1QQ_VVT,a1QQ_VTT,a1QQ_VVn,a1QQ_Vnn,a1QQ_VTn,difflevel=difflevel)
       endif

       if (svrm_opt%quantum_correction_hs > 0) then
          ! Shift integral limits to sigma_eff instead of sigma
          if (y > 1) then
             if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
                call calcIcTilde_TVn_PureHSRef(nc,x0z,x1z,s_vc%zeta,lambda_a+4,lambda_r+4,eps,C,&
                     a1Cc,a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                     a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=difflevel,&
                     fac_r_in=Q2_r,fac_a_in=Q2_a)
             else
                call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a+4,lambda_r+4,eps,C,&
                     a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
                     fac_r_in=Q2_r,fac_a_in=Q2_a)
                call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,s_vc%zeta,&
                     a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
                     a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                     a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=difflevel)
             endif
             call calc_a0_plus_a1(nc,a1Cc,a1QQ,&
                  a1C_T,a1C_V,a1C_n,&
                  a1QQ_T,a1QQ_V,a1QQ_n,&
                  a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
                  a1QQ_TT,a1QQ_VV,a1QQ_TV,a1QQ_Tn,a1QQ_Vn,a1QQ_nn,&
                  a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,&
                  a1QQ_VVV,a1QQ_VVT,a1QQ_VTT,a1QQ_VVn,a1QQ_Vnn,a1QQ_VTn)
          endif
       endif

       ! Add a1QQ*D**2 to a1
       call get_DFeynHibbsPower(i,j,DFH2,DFH2_T,DFH2_TT,s_vc,power_in=2,divideBySigmaMie=.true.)
       call add_aQ_DFH_product(nc,DFH2,DFH2_T,DFH2_TT,&
            a1QQ,a1,&
            a1QQ_T,a1QQ_V,a1QQ_n,&
            a1_T,a1_V,a1_n,&
            a1QQ_TT,a1QQ_VV,a1QQ_TV,a1QQ_Tn,a1QQ_Vn,a1QQ_nn,&
            a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1QQ_VVV,a1QQ_VVT,a1QQ_VTT,a1QQ_VVn,a1QQ_Vnn,a1QQ_VTn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

    end if
    call cleanup_saftvrmie_zeta(x0z)
    call cleanup_saftvrmie_zeta(x1z)
  end subroutine calcA1TildeTVnQuantumCorrection

  !> Calculate x=r/d differentials
  !!
  !! \author Morten Hammer, March 2018
  subroutine calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x,x_T,x_TT,assmue_s_of_T)
    ! Input
    real, intent(in) :: d, d_T, d_TT !< Hard sphere diameter
    real, intent(in) :: s, s_T, s_TT !< Effective sigma
    logical, optional, intent(in) :: assmue_s_of_T
    ! Output
    real, intent(out) :: x !< Reduced center-center hard sphere distance
    real, intent(out) :: x_T !< Reduced center-center hard sphere distance temperature differential
    real, intent(out) :: x_TT !< Reduced center-center hard sphere distance second temperature differential
    ! Locals
    logical :: assmue_s_of_T_local
    x = s/d
    x_T = -(x/d)*d_T
    x_TT = -2.0*x_T*d_T/d - (x/d)*d_TT
    assmue_s_of_T_local = .false.
    if (present(assmue_s_of_T)) assmue_s_of_T_local = assmue_s_of_T
    if (svrm_opt%quantum_correction_hs > 0 .OR. assmue_s_of_T_local) then
      x_T = x_T + s_T/d
      x_TT = x_TT + s_TT/d - 2.0*s_T*d_T/d**2
    endif
  end subroutine calcXDifferentials

  !> Calculate x=s(T)/d(T,n) with differentials for pure hard-sphere reference
  !!
  !! \author Morten Hammer, February 2019
  subroutine calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x)
    ! Input
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_zeta), intent(in) :: d_pure !< Hard sphere diameter
    real, intent(in) :: s, s_T, s_TT !< Sigma and differentials
    ! Output
    type(saftvrmie_zeta), intent(inout) :: x !< Reduced center-center hard sphere distance
    ! Locals
    real :: d, d_T, d_TT
    integer :: l,k
    d = d_pure%zx
    d_T = d_pure%zx_T
    d_TT = d_pure%zx_TT
    x%zx = s/d
    x%zx_T = -(x%zx/d)*d_T
    x%zx_TT = -2.0*x%zx_T*d_T/d - (x%zx/d)*d_TT
    x%zx_n = -(x%zx/d)*d_pure%zx_n
    do l=1,nc
       do k=1,nc
          x%zx_nn(k,l) = (x%zx/d)*(2*d_pure%zx_n(k)*d_pure%zx_n(l)/d - d_pure%zx_nn(k,l))
       enddo
    enddo
    x%zx_Tn = (x%zx/d)*(2*d_pure%zx_n(:)*d_T/d - d_pure%zx_Tn(:))
    if (svrm_opt%quantum_correction_hs > 0 .and. (s_T /= 0 .or. s_TT /= 0)) then
       x%zx_T = x%zx_T + s_T/d
       x%zx_TT = x%zx_TT + s_TT/d - 2.0*s_T*d_T/d**2
       x%zx_Tn = x%zx_Tn - s_T*d_pure%zx_n(:)/d**2
    endif
    x%zx_V = 0
    x%zx_VV = 0
    x%zx_TV = 0
    x%zx_Vn = 0
    x%zx_VVV = 0
    x%zx_VVT = 0
    x%zx_VTn = 0
    x%zx_VVn = 0
    x%zx_Vnn = 0
    x%zx_VTT = 0
  end subroutine calcXDifferentialsPureHSRef

  !****************************************************************************************
  ! FIRST ORDER TERM
  !****************************************************************************************

  !> Calculate first order monomer perturbation (a1)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA1ij(nc,T,V,n,i,j,s_vc,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    real :: d, d_T, d_TT !< Hard sphere diameter

    call calcA1TildeTVn(nc,i,j,s_vc,&
         a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call calc_a_zeta_product(nc,s_vc%zeta,a1,a1_T,a1_V,a1_n,&
            a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call eta_a_product(nc,T,V,n,d,d_T,d_TT,&
            a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    endif
  end subroutine calcA1ij

  !> Calculate a1 quantum correction.
  !!
  !! \author Ailo Aasen, March 2018
  subroutine calcA1ijQuantumCorrection(nc,T,V,n,i,j,s_vc,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    real :: d, d_T, d_TT !< Hard sphere diameter

    call calcA1TildeTVnQuantumCorrection(nc,T,V,n,i,j,s_vc,&
         a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call calc_a_zeta_product(nc,s_vc%zeta,a1,a1_T,a1_V,a1_n,&
            a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call eta_a_product(nc,T,V,n,d,d_T,d_TT,&
            a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
            a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    endif
  end subroutine calcA1ijQuantumCorrection


  !> Calculate a*eta and differentials
  !!
  !! \author Morten Hammer, February 2018
  subroutine eta_a_product(nc,T,V,n,d,d_T,d_TT,&
       a,a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    real, intent(in) :: d,d_T,d_TT !< Hard sphere diameter and differentials
    ! Output
    real, intent(out) :: a
    real, optional, intent(inout) ::  a_T,a_V,a_TT,a_VV,a_TV,a_VVV,a_VVT,a_VTT
    real, optional, dimension(nc), intent(inout) :: a_n,a_Tn,a_Vn,a_VVn,a_VTn
    real, optional, dimension(nc,nc), intent(inout) :: a_nn,a_Vnn
    ! Locals
    real :: at,at_T,at_V,at_TT,at_VV,at_TV
    real, dimension(nc) :: at_n,at_Tn,at_Vn
    real, dimension(nc,nc) :: at_nn
    real :: eta,eta_T,eta_V,eta_TT,eta_VV,eta_TV,eta_VVV,eta_VVT,eta_VTT
    real, dimension(nc) :: eta_n,eta_Tn,eta_Vn,eta_VVn,eta_VTn
    logical :: first_order_present, second_order_present
    logical :: third_order_present
    integer :: k
    real :: ns

    first_order_present = (present(a_T) .and. present(a_n) .and. present(a_V))
    second_order_present = (present(a_TT) .or. present(a_TV) .or. present(a_Tn) .or. &
         present(a_Vn) .or. present(a_VV) .or. present(a_nn))
    third_order_present = (present(a_VTT) .or. present(a_VTn) .or. present(a_VVn) .or. &
         present(a_Vnn) .or. present(a_VVT))
    ! Input logics
    if (second_order_present) then
       ! First order differentials required
       if (.not. first_order_present) then
          call stoperror("eta_a_product: First order differentials required")
       endif
    endif
    if (third_order_present) then
       ! Second order differentials required
       if (.not. second_order_present) then
          call stoperror("eta_a_product: First order differentials required")
       endif
    endif

    ns = sum(saftvrmie_param%ms*n)
    eta = pi*N_AVOGADRO*ns*d**3/(6.0*V)
    at = a
    a = at*eta
    if (present(a_T)) then
       at_T = a_T
       eta_T = 3.0*eta*d_T/d
       a_T = eta*at_T + eta_T*at
    endif
    if (present(a_TT)) then
       at_TT = a_TT
       eta_TT = 6.0*eta*d_T**2/d**2 + 3.0*eta*d_TT/d
       a_TT = eta*at_TT + 2.0*eta_T*at_T + eta_TT*at
    endif
    if (present(a_V)) then
       at_V = a_V
       eta_V = -eta/V
       a_V = eta*at_V + eta_V*at
    endif
    if (present(a_VV)) then
       at_VV = a_VV
       eta_VV = 2.0*eta/V**2
       a_VV = eta*at_VV + 2.0*eta_V*at_V + eta_VV*at
    endif
    if (present(a_TV)) then
       at_TV = a_TV
       eta_TV = -eta_T/V
       a_TV = eta*at_TV + eta_T*at_V + eta_V*at_T + eta_TV*at
    endif
    if (present(a_n)) then
       at_n = a_n
       eta_n = eta*saftvrmie_param%ms/ns
       a_n = eta*at_n + eta_n*at
    endif
    if (present(a_nn)) then
       at_nn = a_nn
       do k=1,nc
          a_nn(:,k) = eta*at_nn(:,k) + eta_n(k)*at_n + eta_n*at_n(k)
       enddo
    endif
    if (present(a_Tn)) then
       at_Tn = a_Tn
       eta_Tn = eta_T*saftvrmie_param%ms/ns
       a_Tn = eta*at_Tn + eta_T*at_n + eta_n*at_T + eta_Tn*at
    endif
    if (present(a_Vn)) then
       at_Vn = a_Vn
       eta_Vn = eta_V*saftvrmie_param%ms/ns
       a_Vn = eta*a_Vn + eta_V*at_n + eta_n*at_V + eta_Vn*at
    endif
    if (present(a_Vnn)) then
       do k=1,nc
          a_Vnn(:,k) = eta_V*at_nn(:,k) + eta*a_Vnn(:,k) + eta_Vn(k)*at_n &
               + eta_n(k)*at_Vn + eta_Vn*at_n(k) + eta_n*at_Vn(k)
       enddo
    endif
    if (present(a_VVV)) then
       eta_VVV = -6.0*eta/V**3
       a_VVV = eta_VVV*at + 3.0*eta_VV*at_V + 3.0*eta_V*at_VV + eta*a_VVV
    endif
    if (present(a_VVT)) then
       eta_VVT = 2.0*eta_T/V**2
       a_VVT = eta_VVT*at + 2.0*eta_TV*at_V + eta_VV*at_T &
            + 2.0*eta_V*at_TV + eta_T*at_VV + eta*a_VVT
    endif
    if (present(a_VTT)) then
       eta_VTT = -eta_TT/V
       a_VTT = eta_VTT*at + 2.0*eta_TV*at_T + eta_TT*at_V &
            + 2.0*eta_T*at_TV + eta_V*at_TT + eta*a_VTT
    endif
    if (present(a_VVn)) then
       eta_VVn = -2.0*eta_Vn/V
       a_VVn = eta_VVn*at + 2.0*eta_Vn*at_V + eta_VV*at_n &
            + 2.0*eta_V*at_Vn + eta_n*at_VV + eta*a_VVn
    endif
    if (present(a_VTn)) then
       eta_VTn = -eta_Tn/V
       a_VTn = eta_VTn*at + eta_Vn*at_T + eta_Tn*at_V + eta_TV*at_n &
            + eta_V*at_Tn + eta_T*at_Vn + eta_n*at_TV + eta*a_VTn
    endif
  end subroutine eta_a_product

  !> Calculate first order monomer perturbation (a1) and its quantum corrections
  !!
  !! \author Morten Hammer, February 2018
  !! \author Ailo Aasen, March 2018
  subroutine calcA1(nc,T,V,n,saftvrmie_vc,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc
    ! Output
    real, intent(out) :: a1
    real, optional, intent(out) ::  a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, optional, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, optional, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    integer :: i,j !< Binary of interest
    real :: ns, ns2
    ns = sum(saftvrmie_param%ms*n)
    ns2 = ns**2
    do i=1,nc
      do j=i,nc
        if (present(a1_nn) .or. present(a1_Tn) .or. present(a1_Vn) .or. &
             present(a1_TT) .or. present(a1_VV) .or. present(a1_TV)) then
          call calcA1ij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a1ij%am(i,j),saftvrmie_vc%a1ij%am_T(i,j),&
               saftvrmie_vc%a1ij%am_V(i,j),saftvrmie_vc%a1ij%am_n(:,i,j),&
               saftvrmie_vc%a1ij%am_TT(i,j),saftvrmie_vc%a1ij%am_VV(i,j),&
               saftvrmie_vc%a1ij%am_TV(i,j),saftvrmie_vc%a1ij%am_Tn(:,i,j),&
               saftvrmie_vc%a1ij%am_Vn(:,i,j),saftvrmie_vc%a1ij%am_nn(:,:,i,j),&
               saftvrmie_vc%a1ij%am_VVV(i,j),saftvrmie_vc%a1ij%am_VVT(i,j),&
               saftvrmie_vc%a1ij%am_VTT(i,j),saftvrmie_vc%a1ij%am_VVn(:,i,j),&
               saftvrmie_vc%a1ij%am_Vnn(:,:,i,j),saftvrmie_vc%a1ij%am_VTn(:,i,j))
          if (svrm_opt%quantum_correction>0) then
            call calcA1ijQuantumCorrection(nc,T,V,n,i,j,saftvrmie_vc,&
                 saftvrmie_vc%a1ijQCorr%am(i,j),saftvrmie_vc%a1ijQCorr%am_T(i,j),&
                 saftvrmie_vc%a1ijQCorr%am_V(i,j),saftvrmie_vc%a1ijQCorr%am_n(:,i,j),&
                 saftvrmie_vc%a1ijQCorr%am_TT(i,j),saftvrmie_vc%a1ijQCorr%am_VV(i,j),&
                 saftvrmie_vc%a1ijQCorr%am_TV(i,j),saftvrmie_vc%a1ijQCorr%am_Tn(:,i,j),&
                 saftvrmie_vc%a1ijQCorr%am_Vn(:,i,j),saftvrmie_vc%a1ijQCorr%am_nn(:,:,i,j),&
                 saftvrmie_vc%a1ijQCorr%am_VVV(i,j),saftvrmie_vc%a1ijQCorr%am_VVT(i,j),&
                 saftvrmie_vc%a1ijQCorr%am_VTT(i,j),saftvrmie_vc%a1ijQCorr%am_VVn(:,i,j),&
                 saftvrmie_vc%a1ijQCorr%am_Vnn(:,:,i,j),saftvrmie_vc%a1ijQCorr%am_VTn(:,i,j))
          end if
        else if (present(a1_T) .or. present(a1_V) .or. present(a1_n)) then
          call calcA1ij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a1ij%am(i,j),saftvrmie_vc%a1ij%am_T(i,j),&
               saftvrmie_vc%a1ij%am_V(i,j),saftvrmie_vc%a1ij%am_n(:,i,j),&
               saftvrmie_vc%a1ij%am_TT(i,j),saftvrmie_vc%a1ij%am_VV(i,j),&
               saftvrmie_vc%a1ij%am_TV(i,j),saftvrmie_vc%a1ij%am_Tn(:,i,j),&
               saftvrmie_vc%a1ij%am_Vn(:,i,j),saftvrmie_vc%a1ij%am_nn(:,:,i,j))
          if (svrm_opt%quantum_correction>0) then
            call calcA1ijQuantumCorrection(nc,T,V,n,i,j,saftvrmie_vc,&
                 saftvrmie_vc%a1ijQCorr%am(i,j),saftvrmie_vc%a1ijQCorr%am_T(i,j),&
                 saftvrmie_vc%a1ijQCorr%am_V(i,j),saftvrmie_vc%a1ijQCorr%am_n(:,i,j),&
                 saftvrmie_vc%a1ijQCorr%am_TT(i,j),saftvrmie_vc%a1ijQCorr%am_VV(i,j),&
                 saftvrmie_vc%a1ijQCorr%am_TV(i,j),saftvrmie_vc%a1ijQCorr%am_Tn(:,i,j),&
                 saftvrmie_vc%a1ijQCorr%am_Vn(:,i,j),saftvrmie_vc%a1ijQCorr%am_nn(:,:,i,j))
          end if
        else
          call calcA1ij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a1ij%am(i,j),saftvrmie_vc%a1ij%am_T(i,j),&
               saftvrmie_vc%a1ij%am_V(i,j),saftvrmie_vc%a1ij%am_n(:,i,j))
          if (svrm_opt%quantum_correction>0) then
            call calcA1ijQuantumCorrection(nc,T,V,n,i,j,saftvrmie_vc,&
                 saftvrmie_vc%a1ijQCorr%am(i,j),saftvrmie_vc%a1ijQCorr%am_T(i,j),&
                 saftvrmie_vc%a1ijQCorr%am_V(i,j),saftvrmie_vc%a1ijQCorr%am_n(:,i,j))
          end if
        endif
      enddo
    enddo
    call saftvrmie_vc%a1ij%mirror()

    if (svrm_opt%quantum_correction>0) then
      call saftvrmie_vc%a1ijQCorr%mirror()
      ! Add the quantum corrections a1ijQCorr to a1ij
      call add_second_saftvrmieaij_to_first(saftvrmie_vc%a1ij, saftvrmie_vc%a1ijQCorr)
    end if

    ! Average over mole fractions
    call calc_double_sum(nc,n,saftvrmie_vc%a1ij,&
         a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
  end subroutine calcA1

  !> Calculate double sum of ms(i)*x(i)*ms(j)*x(j)*a_ij
  !! and differentials
  !!
  !! \author Morten Hammer, February 2018
  subroutine calc_double_sum(nc,n,saftvrmie_a,&
       a,a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_aij), intent(in) :: saftvrmie_a
    ! Output
    real, intent(out) :: a
    real, optional, intent(out) ::  a_T,a_V,a_TT,a_VV,a_TV,a_VVV,a_VVT,a_VTT
    real, optional, dimension(nc), intent(out) :: a_n,a_Tn,a_Vn,a_VVn,a_VTn
    real, optional, dimension(nc,nc), intent(out) :: a_nn,a_Vnn
    ! Locals
    integer :: i,j,k,l !< Binary of interest
    real :: ns, ns2
    ns = sum(saftvrmie_param%ms*n)
    ns2 = ns**2
    a = 0.0
    do k=1,nc
       a = a + saftvrmie_param%ms(k)*n(k)*&
            sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am(:,k))
    enddo
    a = a/ns2
    if (present(a_T)) then
       a_T = 0.0
       do k=1,nc
          a_T = a_T + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_T(:,k))
       enddo
       a_T = a_T/ns2
    endif
    if (present(a_TT)) then
       a_TT = 0.0
       do k=1,nc
          a_TT = a_TT + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_TT(:,k))
       enddo
       a_TT = a_TT/ns2
    endif
    if (present(a_V)) then
       a_V = 0.0
       do k=1,nc
          a_V = a_V + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_V(:,k))
       enddo
       a_V = a_V/ns2
    endif
    if (present(a_VV)) then
       a_VV = 0.0
       do k=1,nc
          a_VV = a_VV + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_VV(:,k))
       enddo
       a_VV = a_VV/ns2
    endif
    if (present(a_TV)) then
       a_TV = 0.0
       do k=1,nc
          a_TV = a_TV + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_TV(:,k))
       enddo
       a_TV = a_TV/ns2
    endif
    if (present(a_n)) then
       a_n = 0.0
       do k=1,nc
          do i=1,nc
             do j=1,nc
                a_n(k) = a_n(k) + saftvrmie_param%ms(i)*n(i)*&
                     saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_n(k,i,j)
             enddo
          enddo
          a_n(k) = a_n(k) + saftvrmie_param%ms(k)*(&
               sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am(:,k)+saftvrmie_a%am(k,:))) &
               - 2.0*ns*a)
       enddo
       a_n = a_n/ns2
    endif
    if (present(a_Tn)) then
       a_Tn = 0.0
       do k=1,nc
          do i=1,nc
             do j=1,nc
                a_Tn(k) = a_Tn(k) + saftvrmie_param%ms(i)*n(i)*&
                     saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_Tn(k,i,j)
             enddo
          enddo
          a_Tn(k) = a_Tn(k) + saftvrmie_param%ms(k)*(&
               sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_T(:,k) + saftvrmie_a%am_T(k,:))) - 2.0*ns*a_T)
       enddo
       a_Tn = a_Tn/ns2
    endif
    if (present(a_Vn)) then
       a_Vn = 0.0
       do k=1,nc
          do i=1,nc
             do j=1,nc
                a_Vn(k) = a_Vn(k) + saftvrmie_param%ms(i)*n(i)*&
                     saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_Vn(k,i,j)
             enddo
          enddo
          a_Vn(k) = a_Vn(k) + saftvrmie_param%ms(k)*(&
               sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_V(:,k) + saftvrmie_a%am_V(k,:))) &
               - 2.0*ns*a_V)
       enddo
       a_Vn = a_Vn/ns2
    endif
    if (present(a_VVV)) then
       a_VVV = 0.0
       do k=1,nc
          a_VVV = a_VVV + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_VVV(:,k))
       enddo
       a_VVV = a_VVV/ns2
    endif
    if (present(a_VVT)) then
       a_VVT = 0.0
       do k=1,nc
          a_VVT = a_VVT + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_VVT(:,k))
       enddo
       a_VVT = a_VVT/ns2
    endif
    if (present(a_VTT)) then
       a_VTT = 0.0
       do k=1,nc
          a_VTT = a_VTT + saftvrmie_param%ms(k)*n(k)*&
               sum(saftvrmie_param%ms(:)*n(:)*saftvrmie_a%am_VTT(:,k))
       enddo
       a_VTT = a_VTT/ns2
    endif
    if (present(a_VVn)) then
       a_VVn = 0.0
       do k=1,nc
          do i=1,nc
             do j=1,nc
                a_VVn(k) = a_VVn(k) + saftvrmie_param%ms(i)*n(i)*&
                     saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_VVn(k,i,j)
             enddo
          enddo
          a_VVn(k) = a_VVn(k) + saftvrmie_param%ms(k)*(&
               sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_VV(:,k) + saftvrmie_a%am_VV(k,:)))&
               - 2.0*ns*a_VV)
       enddo
       a_VVn = a_VVn/ns2
    endif
    if (present(a_VTn)) then
       a_VTn = 0.0
       do k=1,nc
          do i=1,nc
             do j=1,nc
                a_VTn(k) = a_VTn(k) + saftvrmie_param%ms(i)*n(i)*&
                     saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_VTn(k,i,j)
             enddo
          enddo
          a_VTn(k) = a_VTn(k) + saftvrmie_param%ms(k)*(&
               sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_TV(:,k) + saftvrmie_a%am_TV(k,:)))&
               - 2.0*ns*a_TV)
       enddo
       a_VTn = a_VTn/ns2
    endif
    if (present(a_nn)) then
       a_nn = 0.0
       do k=1,nc
          do l=1,nc
             do i=1,nc
                do j=1,nc
                   a_nn(k,l) = a_nn(k,l) + saftvrmie_param%ms(i)*n(i)*&
                        saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_nn(k,l,i,j)
                enddo
             enddo
             a_nn(k,l) = a_nn(k,l) + saftvrmie_param%ms(k)*saftvrmie_param%ms(l)*(&
                  saftvrmie_a%am(k,l) + saftvrmie_a%am(l,k) - 2.0*a) + saftvrmie_param%ms(k)*(&
                  sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_n(l,k,:) &
                  + saftvrmie_a%am_n(l,:,k))) - 2.0*ns*a_n(l)) &
                  + saftvrmie_param%ms(l)*(&
                  sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_n(k,l,:) &
                  + saftvrmie_a%am_n(k,:,l))) - 2.0*ns*a_n(k))
          enddo
       enddo
       a_nn = a_nn/ns2
    endif
    if (present(a_Vnn)) then
       a_Vnn = 0.0
       do k=1,nc
          do l=1,nc
             do i=1,nc
                do j=1,nc
                   a_Vnn(k,l) = a_Vnn(k,l) + saftvrmie_param%ms(i)*n(i)*&
                        saftvrmie_param%ms(j)*n(j)*saftvrmie_a%am_Vnn(k,l,i,j)
                enddo
             enddo
             a_Vnn(k,l) = a_Vnn(k,l) + saftvrmie_param%ms(k)*saftvrmie_param%ms(l)*(&
                  saftvrmie_a%am_V(k,l) + saftvrmie_a%am_V(l,k) - 2.0*a_V) &
                  + saftvrmie_param%ms(k)*(&
                  sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_Vn(l,:,k) &
                  + saftvrmie_a%am_Vn(l,:,k))) - 2.0*ns*a_Vn(l)) &
                  + saftvrmie_param%ms(l)*(&
                  sum(saftvrmie_param%ms(:)*n(:)*(saftvrmie_a%am_Vn(k,:,l) &
                  + saftvrmie_a%am_Vn(k,:,l))) - 2.0*ns*a_Vn(k))
          enddo
       enddo
       a_Vnn = a_Vnn/ns2
    endif
  end subroutine calc_double_sum

  !****************************************************************************************
  ! SECOND ORDER TERM
  !****************************************************************************************

  !> Calculate second order monomer perturbation (a2)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA2(nc,T,V,n,saftvrmie_vc,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc

    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn
    ! Locals
    integer :: i,j !< Binary of interest
    do i=1,nc
      do j=i,nc
        if ( present(a2_nn) .or. present(a2_Tn) .or. present(a2_Vn) .or. &
             present(a2_TT) .or. present(a2_VV) .or. present(a2_TV)) then
          call calcA2chij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a2chij%am(i,j),&
               saftvrmie_vc%a2chij%am_T(i,j),&
               saftvrmie_vc%a2chij%am_V(i,j),saftvrmie_vc%a2chij%am_n(:,i,j),&
               saftvrmie_vc%a2chij%am_TT(i,j),saftvrmie_vc%a2chij%am_VV(i,j),&
               saftvrmie_vc%a2chij%am_TV(i,j),saftvrmie_vc%a2chij%am_Tn(:,i,j),&
               saftvrmie_vc%a2chij%am_Vn(:,i,j),saftvrmie_vc%a2chij%am_nn(:,:,i,j),&
               saftvrmie_vc%a2chij%am_VVV(i,j),saftvrmie_vc%a2chij%am_VVT(i,j),&
               saftvrmie_vc%a2chij%am_VTT(i,j),saftvrmie_vc%a2chij%am_VVn(:,i,j),&
               saftvrmie_vc%a2chij%am_Vnn(:,:,i,j),saftvrmie_vc%a2chij%am_VTn(:,i,j))
          if ( svrm_opt%quantum_correction>0 .and. &
               svrm_opt%quantum_correct_A2) then
            call calcA2chijQuantumCorrection(nc,T,V,n,i,j,saftvrmie_vc,&
                 saftvrmie_vc%a2chijQcorr%am(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_T(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_V(i,j),saftvrmie_vc%a2chijQcorr%am_n(:,i,j),&
                 saftvrmie_vc%a2chijQcorr%am_TT(i,j),saftvrmie_vc%a2chijQcorr%am_VV(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_TV(i,j),saftvrmie_vc%a2chijQcorr%am_Tn(:,i,j),&
                 saftvrmie_vc%a2chijQcorr%am_Vn(:,i,j),saftvrmie_vc%a2chijQcorr%am_nn(:,:,i,j),&
                 saftvrmie_vc%a2chijQcorr%am_VVV(i,j),saftvrmie_vc%a2chijQcorr%am_VVT(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_VTT(i,j),saftvrmie_vc%a2chijQcorr%am_VVn(:,i,j),&
                 saftvrmie_vc%a2chijQcorr%am_Vnn(:,:,i,j),saftvrmie_vc%a2chijQcorr%am_VTn(:,i,j))
            call add_second_saftvrmieaij_to_first(saftvrmie_vc%a2chij, saftvrmie_vc%a2chijQcorr,i,j)
          end if
          call calcA2ij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a2ij%am(i,j),&
               saftvrmie_vc%a2ij%am_T(i,j),saftvrmie_vc%a2ij%am_V(i,j),&
               saftvrmie_vc%a2ij%am_n(:,i,j),saftvrmie_vc%a2ij%am_TT(i,j),&
               saftvrmie_vc%a2ij%am_VV(i,j),saftvrmie_vc%a2ij%am_TV(i,j),&
               saftvrmie_vc%a2ij%am_Tn(:,i,j),saftvrmie_vc%a2ij%am_Vn(:,i,j),&
               saftvrmie_vc%a2ij%am_nn(:,:,i,j))
        else if (present(a2_T) .or. present(a2_V) .or. present(a2_n)) then
          call calcA2chij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a2chij%am(i,j),&
               saftvrmie_vc%a2chij%am_T(i,j),&
               saftvrmie_vc%a2chij%am_V(i,j),saftvrmie_vc%a2chij%am_n(:,i,j),&
               saftvrmie_vc%a2chij%am_TT(i,j),saftvrmie_vc%a2chij%am_VV(i,j),&
               saftvrmie_vc%a2chij%am_TV(i,j),saftvrmie_vc%a2chij%am_Tn(:,i,j),&
               saftvrmie_vc%a2chij%am_Vn(:,i,j),saftvrmie_vc%a2chij%am_nn(:,:,i,j))
          if ( svrm_opt%quantum_correction>0 .and. &
               svrm_opt%quantum_correct_A2) then
            call calcA2chijQuantumCorrection(nc,T,V,n,i,j,saftvrmie_vc,&
                 saftvrmie_vc%a2chijQcorr%am(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_T(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_V(i,j),saftvrmie_vc%a2chijQcorr%am_n(:,i,j),&
                 saftvrmie_vc%a2chijQcorr%am_TT(i,j),saftvrmie_vc%a2chijQcorr%am_VV(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_TV(i,j),saftvrmie_vc%a2chijQcorr%am_Tn(:,i,j),&
                 saftvrmie_vc%a2chijQcorr%am_Vn(:,i,j),saftvrmie_vc%a2chijQcorr%am_nn(:,:,i,j))
            call add_second_saftvrmieaij_to_first(saftvrmie_vc%a2chij, saftvrmie_vc%a2chijQcorr,i,j)
          endif
          call calcA2ij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a2ij%am(i,j),saftvrmie_vc%a2ij%am_T(i,j),&
               saftvrmie_vc%a2ij%am_V(i,j),saftvrmie_vc%a2ij%am_n(:,i,j))
        else
          call calcA2chij(nc,T,V,n,i,j,saftvrmie_vc,&
               saftvrmie_vc%a2chij%am(i,j),&
               saftvrmie_vc%a2chij%am_T(i,j),&
               saftvrmie_vc%a2chij%am_V(i,j),saftvrmie_vc%a2chij%am_n(:,i,j))
          if ( svrm_opt%quantum_correction>0 .and. &
               svrm_opt%quantum_correct_A2) then
            call calcA2chijQuantumCorrection(nc,T,V,n,i,j,saftvrmie_vc,&
                 saftvrmie_vc%a2chijQcorr%am(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_T(i,j),&
                 saftvrmie_vc%a2chijQcorr%am_V(i,j),saftvrmie_vc%a2chijQcorr%am_n(:,i,j))
            call add_second_saftvrmieaij_to_first(saftvrmie_vc%a2chij, saftvrmie_vc%a2chijQcorr,i,j)
          end if
          call calcA2ij(nc,T,V,n,i,j,saftvrmie_vc,saftvrmie_vc%a2ij%am(i,j))
        endif
      enddo
    enddo
    call saftvrmie_vc%a2ij%mirror()
    call calc_double_sum(nc,n,saftvrmie_vc%a2ij,&
         a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)
  end subroutine calcA2

  !> Calculate second order monomer perturbation
  !! a2 = a2ch*(1+chi)
  !! Where (1+chi) is a correction factor
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA2ij(nc,T,V,n,i,j,s_vc,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn
    ! Locals
    real :: c2,c2_V,c2_T
    real, dimension(nc) :: c2_n
    integer :: k

    call calcA2CorrectionVn(nc,V,n,i,j,s_vc%alpha,s_vc%zeta_bar,&
         c2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn)

    !a2ch*(1+chi)
    a2 = c2*s_vc%a2chij%am(i,j)
    if (present(a2_T)) then
       c2_T = a2_T
       a2_T = c2*s_vc%a2chij%am_T(i,j) + c2_T*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_TT)) then
       a2_TT = c2*s_vc%a2chij%am_TT(i,j) + 2.0*c2_T*s_vc%a2chij%am_T(i,j) &
            + a2_TT*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_V)) then
       c2_V = a2_V
       a2_V = c2*s_vc%a2chij%am_V(i,j) + c2_V*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_VV)) then
       a2_VV = c2*s_vc%a2chij%am_VV(i,j) + 2.0*c2_V*s_vc%a2chij%am_V(i,j) &
            + a2_VV*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_TV)) then
       a2_TV = c2*s_vc%a2chij%am_TV(i,j) + c2_T*s_vc%a2chij%am_V(i,j) &
            + c2_V*s_vc%a2chij%am_T(i,j) + a2_TV*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_n)) then
       c2_n = a2_n
       a2_n = c2*s_vc%a2chij%am_n(:,i,j) + c2_n*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_Tn)) then
       a2_Tn = c2*s_vc%a2chij%am_Tn(:,i,j) + c2_n*s_vc%a2chij%am_T(i,j) &
            + c2_T*s_vc%a2chij%am_n(:,i,j) + a2_Tn*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_Vn)) then
       a2_Vn = c2*s_vc%a2chij%am_Vn(:,i,j) + c2_n*s_vc%a2chij%am_V(i,j) &
            + c2_V*s_vc%a2chij%am_n(:,i,j) + a2_Vn*s_vc%a2chij%am(i,j)
    endif
    if (present(a2_nn)) then
       do k=1,nc
          a2_nn(:,k) = c2*s_vc%a2chij%am_nn(:,k,i,j) &
               + c2_n*s_vc%a2chij%am_n(k,i,j) &
               + c2_n(k)*s_vc%a2chij%am_n(:,i,j) &
               + a2_nn(:,k)*s_vc%a2chij%am(i,j)
       enddo
    endif
  end subroutine calcA2ij

  !> Calculate second order monomer perturbation (a2/(1+chi))
  !! a2 is divided by the correction fraction (1+chi)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA2chij(nc,T,V,n,i,j,s_vc,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV,a2_VVV,a2_VVT,a2_VTT
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn,a2_VVn,a2_VTn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn,a2_Vnn
    ! Locals
    real :: d,d_T,d_TT,prefactor
    call calcA22TildeTVn(nc,i,j,s_vc,&
         a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call calc_a_zeta_product(nc,s_vc%zeta,a2,a2_T,a2_V,a2_n,&
            a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call eta_a_product(nc,T,V,n,d,d_T,d_TT,&
            a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    endif
    ! Multiply a22 with 0.5*eps*C²
    prefactor = 0.5*saftvrmie_param%eps_divk_ij(i,j)*saftvrmie_param%Cij(i,j)**2
    a2 = prefactor*a2
    if (present(a2_T)) then
       a2_T = prefactor*a2_T
    endif
    if (present(a2_TT)) then
       a2_TT = prefactor*a2_TT
    endif
    if (present(a2_V)) then
       a2_V = prefactor*a2_V
    endif
    if (present(a2_VV)) then
       a2_VV = prefactor*a2_VV
    endif
    if (present(a2_VVV)) then
       a2_VVV = prefactor*a2_VVV
    endif
    if (present(a2_TV)) then
       a2_TV = prefactor*a2_TV
    endif
    if (present(a2_VVT)) then
       a2_VVT = prefactor*a2_VVT
    endif
    if (present(a2_VTT)) then
       a2_VTT = prefactor*a2_VTT
    endif
    if (present(a2_n)) then
       a2_n = prefactor*a2_n
    endif
    if (present(a2_nn)) then
       a2_nn = prefactor*a2_nn
    endif
    if (present(a2_Tn)) then
       a2_Tn = prefactor*a2_Tn
    endif
    if (present(a2_Vn)) then
       a2_Vn = prefactor*a2_Vn
    endif
    if (present(a2_VTn)) then
       a2_VTn = prefactor*a2_VTn
    endif
    if (present(a2_Vnn)) then
       a2_Vnn = prefactor*a2_Vnn
    endif
    if (present(a2_VVn)) then
       a2_VVn = prefactor*a2_VVn
    endif

    ! Multiply 0.5*eps*C²*a22 with Khs
    call calc_a_zeta_product(nc,s_vc%Khs,a2,a2_T,a2_V,a2_n,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

  end subroutine calcA2chij

  !> Calculate second order monomer perturbation (a2Q/(1+chi))
  !! a2Q is divided by the correction fraction (1+chi)
  !!
  !! \author Ailo Aasen, March 2018
  subroutine calcA2chijQuantumCorrection(nc,T,V,n,i,j,s_vc,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV,a2_VVV,a2_VVT,a2_VTT
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn,a2_VVn,a2_VTn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn,a2_Vnn
    ! Locals
    real :: d,d_T,d_TT,prefactor

    call calcA22TildeTVnQuantumCorrection(nc,T,V,n,i,j,s_vc,&
         a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call calc_a_zeta_product(nc,s_vc%zeta,a2,a2_T,a2_V,a2_n,&
            a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call eta_a_product(nc,T,V,n,d,d_T,d_TT,&
            a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    endif

    ! Multiply a22 with 0.5*eps*C²
    prefactor = 0.5*saftvrmie_param%eps_divk_ij(i,j)*saftvrmie_param%Cij(i,j)**2
    a2 = prefactor*a2
    if (present(a2_T)) then
       a2_T = prefactor*a2_T
    endif
    if (present(a2_TT)) then
       a2_TT = prefactor*a2_TT
    endif
    if (present(a2_V)) then
       a2_V = prefactor*a2_V
    endif
    if (present(a2_VV)) then
       a2_VV = prefactor*a2_VV
    endif
    if (present(a2_VVV)) then
       a2_VVV = prefactor*a2_VVV
    endif
    if (present(a2_TV)) then
       a2_TV = prefactor*a2_TV
    endif
    if (present(a2_VVT)) then
       a2_VVT = prefactor*a2_VVT
    endif
    if (present(a2_VTT)) then
       a2_VTT = prefactor*a2_VTT
    endif
    if (present(a2_n)) then
       a2_n = prefactor*a2_n
    endif
    if (present(a2_nn)) then
       a2_nn = prefactor*a2_nn
    endif
    if (present(a2_Tn)) then
       a2_Tn = prefactor*a2_Tn
    endif
    if (present(a2_Vn)) then
       a2_Vn = prefactor*a2_Vn
    endif
    if (present(a2_VTn)) then
       a2_VTn = prefactor*a2_VTn
    endif
    if (present(a2_Vnn)) then
       a2_Vnn = prefactor*a2_Vnn
    endif
    if (present(a2_VVn)) then
       a2_VVn = prefactor*a2_VVn
    endif

    ! Multiply 0.5*eps*C²*a22 with Khs
    call calc_a_zeta_product(nc,s_vc%Khs,a2,a2_T,a2_V,a2_n,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

  end subroutine calcA2chijQuantumCorrection

  !> Calculate the contribution to x0AplusBtilde that is proportional to D^(DFHPower).
  !! \author Ailo Aasen, March 2018
  subroutine calcA22QTilde_DFHPower(x0,eta,lambda_a,lambda_r,eps,Q1r,Q1a,Q2r,Q2a,DFHPower,&
       a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: Q1r, Q1a, Q2r, Q2a !< Parameters in the quantum correction
    integer, intent(in) :: DFHPower
    ! Output
    real, intent(out) :: a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx
    ! Locals
    real :: a1a,a1a_e,a1a_x,a1a_ee,a1a_xx,a1a_ex,a1a_eee,a1a_eex,a1a_exx
    real :: a1r,a1r_e,a1r_x,a1r_ee,a1r_xx,a1r_ex,a1r_eee,a1r_eex,a1r_exx
    real :: a1c,a1c_e,a1c_x,a1c_ee,a1c_xx,a1c_ex,a1c_eee,a1c_eex,a1c_exx
    real :: rfac, afac, cfac

    if (.not. svrm_opt%quantum_correct_A2) then
       call stoperror("calcA22QTilde_D2: you need to set quantum_correct_A2==.true.")
    end if

    call calcX0AplusBtilde(x0,eta,2*lambda_r+2*DFHPower,eps,&
         a1r,a1r_e,a1r_x,a1r_ee,a1r_xx,a1r_ex,a1r_eee,a1r_eex,a1r_exx)
    call calcX0AplusBtilde(x0,eta,2*lambda_a+2*DFHPower,eps,&
         a1a,a1a_e,a1a_x,a1a_ee,a1a_xx,a1a_ex,a1a_eee,a1a_eex,a1a_exx)
    call calcX0AplusBtilde(x0,eta,lambda_r+lambda_a+2*DFHPower,eps,&
         a1c,a1c_e,a1c_x,a1c_ee,a1c_xx,a1c_ex,a1c_eee,a1c_eex,a1c_exx)

    if (DFHPower==1) then
       rfac = 2*Q1r
       afac = 2*Q1a
       cfac = -(rfac+afac)
    else if (DFHPower==2) then
       rfac = Q1r*Q1r
       afac = Q1a*Q1a
       cfac = -2*Q1r*Q1a
       if (svrm_opt%quantum_correction==2) then
          ! The extra contributions proportional to D2 if we use the FH2 potential instead of the FH1 potential
          rfac = rfac + 2*Q2r
          afac = afac + 2*Q2a
          cfac = cfac - 2*(Q2r+Q2a)
       end if
    else if (DFHPower==3) then
       rfac = 2*Q2r*Q1r
       afac = 2*Q2a*Q1a
       cfac = -2*(Q2r*Q1a+Q2a*Q1r)
    else if (DFHPower==4) then
       rfac = Q2r*Q2r
       afac = Q2a*Q2a
       cfac = -2*Q2r*Q2a
    else
       call stoperror("Wrong DFHPower")
    end if

    a2 = afac*a1a + rfac*a1r + cfac*a1c
    a2_e = afac*a1a_e + rfac*a1r_e + cfac*a1c_e
    a2_x = afac*a1a_x + rfac*a1r_x + cfac*a1c_x
    a2_ee = afac*a1a_ee + rfac*a1r_ee + cfac*a1c_ee
    a2_xx = afac*a1a_xx + rfac*a1r_xx + cfac*a1c_xx
    a2_ex = afac*a1a_ex + rfac*a1r_ex + cfac*a1c_ex
    a2_eee = afac*a1a_eee + rfac*a1r_eee + cfac*a1c_eee
    a2_eex = afac*a1a_eex + rfac*a1r_eex + cfac*a1c_eex
    a2_exx = afac*a1a_exx + rfac*a1r_exx + cfac*a1c_exx

  end subroutine calcA22QTilde_DFHPower


  !> Calculate part of second order monomer perturbation (a22)
  !! a22 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA22TildeTVnQuantumCorrection(nc,T,V,n,i,j,s_vc,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV,a2_VVV,a2_VVT,a2_VTT
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn,a2_VVn,a2_VTn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn,a2_Vnn
    ! Locals
    real :: a2Q, a2Q_e,a2Q_x,a2Q_ee,a2Q_xx,a2Q_ex,a2Q_eee,a2Q_eex,a2Q_exx
    real :: a2Q_T,a2Q_V,a2Q_TT,a2Q_VV,a2Q_TV,a2Q_VVV,a2Q_VVT,a2Q_VTT
    real, dimension(nc) :: a2Q_n,a2Q_Tn,a2Q_Vn,a2Q_VVn,a2Q_VTn
    real, dimension(nc,nc) :: a2Q_nn,a2Q_Vnn
    real :: a2C_T,a2C_V,a2C_TT,a2C_VV,a2C_TV,a2C_VVV,a2C_VVT,a2C_VTT
    real, dimension(nc) :: a2C_n,a2C_Tn,a2C_Vn,a2C_VVn,a2C_VTn
    real, dimension(nc,nc) :: a2C_nn,a2C_Vnn
    real :: a2Cc,a2Cc_e,a2Cc_T,a2Cc_ee,a2Cc_TT,a2Cc_eT,a2Cc_eee,a2Cc_eeT,a2Cc_eTT
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: eta !< Packing fraction
    real :: lambda_a !< Mie potential attractive exponent
    real :: lambda_r !< Mie potential repulsive exponent
    real :: eps !< Well depth div. temperature (K)
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Effective sigma
    real :: y, y_T, y_TT !< Effective sigma
    real :: DFH,DFH_T,DFH_TT
    real :: Q1a,Q1r,Q2a,Q2r
    integer :: power_iterator, difflevel
    type(saftvrmie_zeta) :: x0z, x1z

    difflevel = 1
    if (present(a2_VVV) .or. present(a2_VVT) .or. present(a2_VTT) .or. &
         present(a2_VVn) .or. present(a2_Vnn) .or. present(a2_VTn)) then
       difflevel = 3
    else if (present(a2_VV) .or. present(a2_TV) .or. present(a2_TT) .or. &
         present(a2_Vn) .or. present(a2_nn) .or. present(a2_Tn)) then
       difflevel = 2
    endif

    a2 = 0.0
    if (present(a2_V)) a2_V = 0.0
    if (present(a2_n)) a2_n = 0.0
    if (present(a2_T)) a2_T = 0.0
    if (present(a2_TT)) a2_TT = 0.0
    if (present(a2_VV)) a2_VV = 0.0
    if (present(a2_TV)) a2_TV = 0.0
    if (present(a2_Tn)) a2_Tn = 0.0
    if (present(a2_Vn)) a2_Vn = 0.0
    if (present(a2_nn)) a2_nn = 0.0
    if (present(a2_VVV)) a2_VVV = 0.0
    if (present(a2_VVT)) a2_VVT = 0.0
    if (present(a2_VTT)) a2_VTT = 0.0
    if (present(a2_VVn)) a2_VVn = 0.0
    if (present(a2_Vnn)) a2_Vnn = 0.0
    if (present(a2_VTn)) a2_VTn = 0.0

    s = saftvrmie_param%sigma_ij(i,j)
    s_T = 0.0
    s_TT = 0.0
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       d = s_vc%d_pure%zx
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x0z)
       x0 = x0z%zx
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    endif
    eta = s_vc%zeta%zx
    lambda_a = saftvrmie_param%lambda_a_ij(i,j)
    lambda_r = saftvrmie_param%lambda_r_ij(i,j)
    eps = saftvrmie_param%eps_divk_ij(i,j)
    Q1a = saftvrmie_param%Quantum_const_1a_ij(i,j)
    Q1r = saftvrmie_param%Quantum_const_1r_ij(i,j)
    Q2a = saftvrmie_param%Quantum_const_2a_ij(i,j)
    Q2r = saftvrmie_param%Quantum_const_2r_ij(i,j)

    if (svrm_opt%quantum_correction_hs > 0) then
       ! Shift integral limits to sigma_eff instead of sigma
       s = s_vc%sigma_eff%d(i,j)
       s_T = s_vc%sigma_eff%d_T(i,j)
       s_TT = s_vc%sigma_eff%d_TT(i,j)
       if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
          call allocate_saftvrmie_zeta(nc,x1z)
          call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x1z)
          y = x1z%zx
       else
          call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
       endif
    endif

    do power_iterator=1,(2*svrm_opt%quantum_correction)
       call calcA22QTilde_DFHPower(x0,eta,lambda_a,lambda_r,eps,Q1r,Q1a,Q2r,Q2a,power_iterator,&
            a2Q,a2Q_e,a2Q_x,a2Q_ee,a2Q_xx,a2Q_ex,a2Q_eee,a2Q_eex,a2Q_exx)
       if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
          call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
               a2Q,a2Q_e,a2Q_x,a2Q_ee,a2Q_xx,a2Q_ex,a2Q_eee,0.0,a2Q_eex,a2Q_exx,&
               a2Q_T,a2Q_V,a2Q_n,a2Q_TT,a2Q_VV,a2Q_TV,a2Q_Tn,a2Q_Vn,a2Q_nn,&
               a2Q_VVV,a2Q_VVT,a2Q_VTT,a2Q_VVn,a2Q_Vnn,a2Q_VTn,difflevel=difflevel)
       else
          call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,s_vc%zeta,&
               a2Q,a2Q_e,a2Q_x,a2Q_ee,a2Q_xx,a2Q_ex,a2Q_eee,a2Q_eex,a2Q_exx,&
               a2Q_T,a2Q_V,a2Q_n,a2Q_TT,a2Q_VV,a2Q_TV,a2Q_Tn,a2Q_Vn,a2Q_nn,&
               a2Q_VVV,a2Q_VVT,a2Q_VTT,a2Q_VVn,a2Q_Vnn,a2Q_VTn,difflevel=difflevel)
       endif

       if (svrm_opt%quantum_correction_hs > 0) then
          ! Shift integral limits to sigma_eff instead of sigma
          if (y > 1) then
             if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
                call calcA22TildeSigmaCorr_Q_TVn_PureHSRef(nc,x0z,x1z,s_vc%zeta,lambda_a,lambda_r,eps,&
                     Q1r,Q1a,Q2r,Q2a,power_iterator,&
                     a2Cc,a2C_T,a2C_V,a2C_n,a2C_TT,a2C_VV,a2C_TV,a2C_Tn,a2C_Vn,a2C_nn,&
                     a2C_VVV,a2C_VVT,a2C_VTT,a2C_VVn,a2C_Vnn,a2C_VTn,difflevel=difflevel)
             else
                call calcA22TildeSigmaCorrection_Q(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,eps,&
                     Q1r,Q1a,Q2r,Q2a,power_iterator,&
                     a2Cc,a2Cc_e,a2Cc_T,a2Cc_ee,a2Cc_TT,a2Cc_eT,a2Cc_eee,a2Cc_eeT,a2Cc_eTT)
                call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,s_vc%zeta,&
                     a2Cc,a2Cc_e,a2Cc_T,a2Cc_ee,a2Cc_TT,a2Cc_eT,a2Cc_eee,a2Cc_eeT,a2Cc_eTT,&
                     a2C_T,a2C_V,a2C_n,a2C_TT,a2C_VV,a2C_TV,a2C_Tn,a2C_Vn,a2C_nn,&
                     a2C_VVV,a2C_VVT,a2C_VTT,a2C_VVn,a2C_Vnn,a2C_VTn,difflevel=difflevel)
             endif
             call calc_a0_plus_a1(nc,a2Cc,a2Q,&
                  a2C_T,a2C_V,a2C_n,&
                  a2Q_T,a2Q_V,a2Q_n,&
                  a2C_TT,a2C_VV,a2C_TV,a2C_Tn,a2C_Vn,a2C_nn,&
                  a2Q_TT,a2Q_VV,a2Q_TV,a2Q_Tn,a2Q_Vn,a2Q_nn,&
                  a2C_VVV,a2C_VVT,a2C_VTT,a2C_VVn,a2C_Vnn,a2C_VTn,&
                  a2Q_VVV,a2Q_VVT,a2Q_VTT,a2Q_VVn,a2Q_Vnn,a2Q_VTn)
          endif
       endif

       call get_DFeynHibbsPower(i,j,DFH,DFH_T,DFH_TT,s_vc,power_in=power_iterator,divideBySigmaMie=.true.)
       call add_aQ_DFH_product(nc,DFH,DFH_T,DFH_TT,&
            a2Q,a2,&
            a2Q_T,a2Q_V,a2Q_n,&
            a2_T,a2_V,a2_n,&
            a2Q_TT,a2Q_VV,a2Q_TV,a2Q_Tn,a2Q_Vn,a2Q_nn,&
            a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2Q_VVV,a2Q_VVT,a2Q_VTT,a2Q_VVn,a2Q_Vnn,a2Q_VTn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    end do
    call cleanup_saftvrmie_zeta(x0z)
    call cleanup_saftvrmie_zeta(x1z)
  end subroutine calcA22TildeTVnQuantumCorrection

  !> Calculate part of second order monomer perturbation (a22)
  !! a22 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA22Tilde(x0,eta,lambda_a,lambda_r,eps,&
       a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx)
    ! Input
    real, intent(in) :: x0 !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    ! Output
    real, intent(out) :: a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx
    ! Locals
    real :: Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,Br_eee,Br_eex,Br_exx
    real :: Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,Ba_eee,Ba_eex,Ba_exx
    real :: Bar,Bar_e,Bar_x,Bar_ee,Bar_xx,Bar_ex,Bar_eee,Bar_eex,Bar_exx
    real :: asr,asr_e,asr_ee,asr_eee
    real :: asa,asa_e,asa_ee,asa_eee
    real :: asar,asar_e,asar_ee,asar_eee
    real :: lambda_2r,lambda_2a,lambda_ar
    lambda_2r = 2.0*lambda_r
    lambda_2a = 2.0*lambda_a
    lambda_ar = lambda_a + lambda_r
    call calcA1Sutherland(eta,lambda_2r,eps,asr,asr_e,asr_ee,asr_eee)
    call calcA1Sutherland(eta,lambda_2a,eps,asa,asa_e,asa_ee,asa_eee)
    call calcA1Sutherland(eta,lambda_ar,eps,asar,asar_e,asar_ee,asar_eee)
    if (svrm_opt%quantum_correction_hs <= 0 .and. x0 > 1) then
       ! Calculate integral form d to sigma
       call calcBtilde(x0,eta,lambda_2r,eps,Br,Br_e,Br_x,Br_ee,Br_xx,Br_ex,&
            Br_eee,Br_eex,Br_exx)
       call calcBtilde(x0,eta,lambda_2a,eps,Ba,Ba_e,Ba_x,Ba_ee,Ba_xx,Ba_ex,&
            Ba_eee,Ba_eex,Ba_exx)
       call calcBtilde(x0,eta,lambda_ar,eps,Bar,Bar_e,Bar_x,&
            Bar_ee,Bar_xx,Bar_ex,Bar_eee,Bar_eex,Bar_exx)
    else
       ! Calculate integral form d to sigma_eff elsewhere
       Br=0.0
       Br_e=0.0
       Br_x=0.0
       Br_ee=0.0
       Br_xx=0.0
       Br_ex=0.0
       Br_eee=0.0
       Br_eex=0.0
       Br_exx=0.0
       !
       Ba=0.0
       Ba_e=0.0
       Ba_x=0.0
       Ba_ee=0.0
       Ba_xx=0.0
       Ba_ex=0.0
       Ba_eee=0.0
       Ba_eex=0.0
       Ba_exx=0.0
       !
       Bar=0.0
       Bar_e=0.0
       Bar_x=0.0
       Bar_ee=0.0
       Bar_xx=0.0
       Bar_ex=0.0
       Bar_eee=0.0
       Bar_eex=0.0
       Bar_exx=0.0
    endif
    a2 = x0**(lambda_2a)*(asa+Ba) - 2.0*x0**(lambda_ar)*(asar+Bar) + x0**(lambda_2r)*(asr+Br)
    a2_x = lambda_2a*x0**(lambda_2a-1.0)*(asa+Ba) &
         - 2.0*lambda_ar*x0**(lambda_ar-1.0)*(asar+Bar) &
         + lambda_2r*x0**(lambda_2r-1.0)*(asr+Br) &
         + x0**(lambda_2a)*Ba_x - 2.0*x0**(lambda_ar)*Bar_x + x0**(lambda_2r)*Br_x
    a2_xx = (lambda_2a-1.0)*lambda_2a*x0**(lambda_2a-2.0)*(asa+Ba) &
         - 2.0*(lambda_ar-1.0)*lambda_ar*x0**(lambda_ar-2.0)*(asar+Bar) &
         + (lambda_2r-1.0)*lambda_2r*x0**(lambda_2r-2.0)*(asr+Br) &
         + 2.0*(lambda_2a*x0**(lambda_2a-1.0)*Ba_x &
         - 2.0*lambda_ar*x0**(lambda_ar-1.0)*Bar_x &
         + lambda_2r*x0**(lambda_2r-1.0)*Br_x) &
         + x0**(lambda_2a)*Ba_xx - 2.0*x0**(lambda_ar)*Bar_xx + x0**(lambda_2r)*Br_xx
    a2_e = x0**(lambda_2a)*(asa_e+Ba_e) - 2.0*x0**(lambda_ar)*(asar_e+Bar_e) &
         + x0**(lambda_2r)*(asr_e+Br_e)
    a2_ee = x0**(lambda_2a)*(asa_ee+Ba_ee) - 2.0*x0**(lambda_ar)*(asar_ee+Bar_ee) &
         + x0**(lambda_2r)*(asr_ee+Br_ee)
    a2_eee = x0**(lambda_2a)*(asa_eee+Ba_eee) - 2.0*x0**(lambda_ar)*(asar_eee+Bar_eee) &
         + x0**(lambda_2r)*(asr_eee+Br_eee)
    a2_ex = lambda_2a*x0**(lambda_2a-1.0)*(asa_e+Ba_e) &
         - 2.0*lambda_ar*x0**(lambda_ar-1.0)*(asar_e+Bar_e) &
         + lambda_2r*x0**(lambda_2r-1.0)*(asr_e+Br_e) &
         + x0**(lambda_2a)*Ba_ex - 2.0*x0**(lambda_ar)*Bar_ex + x0**(lambda_2r)*Br_ex
    a2_exx = (lambda_2a-1.0)*lambda_2a*x0**(lambda_2a-2.0)*(asa_e+Ba_e) &
         - 2.0*(lambda_ar-1.0)*lambda_ar*x0**(lambda_ar-2.0)*(asar_e+Bar_e) &
         + (lambda_2r-1.0)*lambda_2r*x0**(lambda_2r-2.0)*(asr_e+Br_e) &
         + 2.0*(lambda_2a*x0**(lambda_2a-1.0)*Ba_ex &
         - 2.0*lambda_ar*x0**(lambda_ar-1.0)*Bar_ex &
         + lambda_2r*x0**(lambda_2r-1.0)*Br_ex) &
         + x0**(lambda_2a)*Ba_exx - 2.0*x0**(lambda_ar)*Bar_exx + x0**(lambda_2r)*Br_exx
    a2_eex = lambda_2a*x0**(lambda_2a-1.0)*(asa_ee+Ba_ee) &
         - 2.0*lambda_ar*x0**(lambda_ar-1.0)*(asar_ee+Bar_ee) &
         + lambda_2r*x0**(lambda_2r-1.0)*(asr_ee+Br_ee) &
         + + x0**(lambda_2a)*Ba_eex - 2.0*x0**(lambda_ar)*Bar_eex + x0**(lambda_2r)*Br_eex
  end subroutine calcA22Tilde

  !> Correct a22 by integral for y=1 to y=sigma_eff/d
  !! a22 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2019
  subroutine calcA22TildeSigmaCorr_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_a,lambda_r,eps,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn,difflevel)
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_zeta), intent(in) :: eta
    type(saftvrmie_zeta), intent(in) :: x0z,x1z
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    integer, optional, intent(in) :: difflevel
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV,a2_VVV,a2_VVT,a2_VTT
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn,a2_VVn,a2_VTn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn,a2_Vnn
    ! Locals
    real :: a2r, a2r_T,a2r_V,a2r_TT,a2r_VV,a2r_TV,a2r_VVV,a2r_VVT,a2r_VTT
    real, dimension(nc) :: a2r_n,a2r_Tn,a2r_Vn,a2r_VVn,a2r_VTn
    real, dimension(nc,nc) :: a2r_nn,a2r_Vnn
    real :: lambda_2r,lambda_2a,lambda_ar,C
    lambda_2r = 2.0*lambda_r
    lambda_2a = 2.0*lambda_a
    lambda_ar = lambda_a + lambda_r
    C = 1.0

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_2a,eps,C,1.0,&
         a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn,difflevel)

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_2r,eps,C,1.0,&
         a2r,a2r_T,a2r_V,a2r_n,a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,difflevel)

    call calc_a0_plus_a1(nc,a2r,a2,&
         a2r_T,a2r_V,a2r_n,&
         a2_T,a2_V,a2_n,&
         a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_ar,eps,C,-2.0,&
         a2r,a2r_T,a2r_V,a2r_n,a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,difflevel)

    call calc_a0_plus_a1(nc,a2r,a2,&
         a2r_T,a2r_V,a2r_n,&
         a2_T,a2_V,a2_n,&
         a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

  end subroutine calcA22TildeSigmaCorr_TVn_PureHSRef

  !> Correct a22 by integral for y=1 to y=sigma_eff/d
  !! a22 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2019
  subroutine calcA22TildeSigmaCorr_Q_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_a,lambda_r,eps,&
       Q1r,Q1a,Q2r,Q2a,DFHPower,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn,difflevel)
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_zeta), intent(in) :: eta
    type(saftvrmie_zeta), intent(in) :: x0z,x1z
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: Q1r, Q1a, Q2r, Q2a !< Parameters in the quantum correction
    integer, intent(in) :: DFHPower
    integer, optional, intent(in) :: difflevel
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV,a2_VVV,a2_VVT,a2_VTT
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn,a2_VVn,a2_VTn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn,a2_Vnn
    ! Locals
    real :: a2r, a2r_T,a2r_V,a2r_TT,a2r_VV,a2r_TV,a2r_VVV,a2r_VVT,a2r_VTT
    real, dimension(nc) :: a2r_n,a2r_Tn,a2r_Vn,a2r_VVn,a2r_VTn
    real, dimension(nc,nc) :: a2r_nn,a2r_Vnn
    real :: lambda_2r,lambda_2a,lambda_ar,C
    real :: rfac, afac, cfac
    lambda_2r = 2.0*lambda_r
    lambda_2a = 2.0*lambda_a
    lambda_ar = lambda_a + lambda_r
    C = 1.0

    if (DFHPower==1) then
       rfac = 2*Q1r
       afac = 2*Q1a
       cfac = -(rfac+afac)
    else if (DFHPower==2) then
       rfac = Q1r*Q1r
       afac = Q1a*Q1a
       cfac = -2*Q1r*Q1a
       if (svrm_opt%quantum_correction==2) then
          ! The extra contributions proportional to D2 if we use the FH2 potential instead of the FH1 potential
          rfac = rfac + 2*Q2r
          afac = afac + 2*Q2a
          cfac = cfac - 2*(Q2r+Q2a)
       end if
    else if (DFHPower==3) then
       rfac = 2*Q2r*Q1r
       afac = 2*Q2a*Q1a
       cfac = -2*(Q2r*Q1a+Q2a*Q1r)
    else if (DFHPower==4) then
       rfac = Q2r*Q2r
       afac = Q2a*Q2a
       cfac = -2*Q2r*Q2a
    else
       call stoperror("Wrong DFHPower")
    end if


    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_2a+2*DFHPower,eps,C,afac,&
         a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn,difflevel)

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_2r+2*DFHPower,eps,C,rfac,&
         a2r,a2r_T,a2r_V,a2r_n,a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,difflevel)

    call calc_a0_plus_a1(nc,a2r,a2,&
         a2r_T,a2r_V,a2r_n,&
         a2_T,a2_V,a2_n,&
         a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

    call calcIcTildeSingleTerm_TVn_PureHSRef(nc,x0z,x1z,eta,lambda_ar+2*DFHPower,eps,C,cfac,&
         a2r,a2r_T,a2r_V,a2r_n,a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,difflevel)

    call calc_a0_plus_a1(nc,a2r,a2,&
         a2r_T,a2r_V,a2r_n,&
         a2_T,a2_V,a2_n,&
         a2r_TT,a2r_VV,a2r_TV,a2r_Tn,a2r_Vn,a2r_nn,&
         a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
         a2r_VVV,a2r_VVT,a2r_VTT,a2r_VVn,a2r_Vnn,a2r_VTn,&
         a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

  end subroutine calcA22TildeSigmaCorr_Q_TVn_PureHSRef

  !> Correct a22 by integral for y=1 to y=sigma_eff/d
  !! a22 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA22TildeSigmaCorrection(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,eps,& !! This will be used as before
       a2,a2_e,a2_T,a2_ee,a2_TT,a2_eT,a2_eee,a2_eeT,a2_eTT)
    ! Input
    real, intent(in) :: x,x_T,x_TT !< Reduced center-center hard sphere distance
    real, intent(in) :: y,y_T,y_TT !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    ! Output
    real, intent(out) :: a2,a2_e,a2_T,a2_ee,a2_TT,a2_eT,a2_eee,a2_eeT,a2_eTT
    ! Locals
    real :: a21,a21_e,a21_T,a21_ee,a21_TT,a21_eT,a21_eee,a21_eeT,a21_eTT
    real :: a22,a22_e,a22_T,a22_ee,a22_TT,a22_eT,a22_eee,a22_eeT,a22_eTT
    real :: a23,a23_e,a23_T,a23_ee,a23_TT,a23_eT,a23_eee,a23_eeT,a23_eTT
    real :: lambda_2r,lambda_2a,lambda_ar,C
    lambda_2r = 2.0*lambda_r
    lambda_2a = 2.0*lambda_a
    lambda_ar = lambda_a + lambda_r
    C = 1.0
    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_2r,eps,C,& !! do-loop here, can multiply prefactor in thus func
         a21,a21_e,a21_T,a21_ee,a21_TT,a21_eT,a21_eee,a21_eeT,a21_eTT)
    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_2a,eps,C,&
         a22,a22_e,a22_T,a22_ee,a22_TT,a22_eT,a22_eee,a22_eeT,a22_eTT)
    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_ar,eps,C,&
         a23,a23_e,a23_T,a23_ee,a23_TT,a23_eT,a23_eee,a23_eeT,a23_eTT,fac_in=-2.0)
    a2 = a21 + a22 + a23
    a2_e = a21_e + a22_e + a23_e
    a2_ee = a21_ee + a22_ee + a23_ee
    a2_eee = a21_eee + a22_eee + a23_eee
    a2_T = a21_T + a22_T + a23_T
    a2_TT = a21_TT + a22_TT + a23_TT
    a2_eT = a21_eT + a22_eT + a23_eT
    a2_eeT = a21_eeT + a22_eeT + a23_eeT
    a2_eTT = a21_eTT + a22_eTT + a23_eTT
  end subroutine calcA22TildeSigmaCorrection


  !> Correct a22 by integral for y=sigma_eff/d to x=sigma/d
  !! a22 is divided by the packing fraction
  !!
  !! \author Ailo Aasen, March 2018
  subroutine calcA22TildeSigmaCorrection_Q(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,eps,&
       Q1r,Q1a,Q2r,Q2a,DFHPower,&
       a2,a2_e,a2_T,a2_ee,a2_TT,a2_eT,a2_eee,a2_eeT,a2_eTT)
    ! Input
    real, intent(in) :: x,x_T,x_TT !< Reduced center-center hard sphere distance
    real, intent(in) :: y,y_T,y_TT !< Reduced center-center hard sphere distance
    real, intent(in) :: eta !< Packing fraction
    real, intent(in) :: lambda_a !< Mie potential attractive exponent
    real, intent(in) :: lambda_r !< Mie potential repulsive exponent
    real, intent(in) :: eps !< Well depth div. temperature (K)
    real, intent(in) :: Q1r, Q1a, Q2r, Q2a !< Parameters in the quantum correction
    integer, intent(in) :: DFHPower
    ! Output
    real, intent(out) :: a2,a2_e,a2_T,a2_ee,a2_TT,a2_eT,a2_eee,a2_eeT,a2_eTT
    ! Locals
    real :: a21,a21_e,a21_T,a21_ee,a21_TT,a21_eT,a21_eee,a21_eeT,a21_eTT
    real :: a22,a22_e,a22_T,a22_ee,a22_TT,a22_eT,a22_eee,a22_eeT,a22_eTT
    real :: a23,a23_e,a23_T,a23_ee,a23_TT,a23_eT,a23_eee,a23_eeT,a23_eTT
    real :: lambda_2r,lambda_2a,lambda_ar,C
    real :: rfac, afac, cfac
    lambda_2r = 2.0*lambda_r
    lambda_2a = 2.0*lambda_a
    lambda_ar = lambda_a + lambda_r
    C = 1.0

    if (DFHPower==1) then
       rfac = 2*Q1r
       afac = 2*Q1a
       cfac = -(rfac+afac)
    else if (DFHPower==2) then
       rfac = Q1r*Q1r
       afac = Q1a*Q1a
       cfac = -2*Q1r*Q1a
       if (svrm_opt%quantum_correction==2) then
          ! The extra contributions proportional to D2 if we use the FH2 potential instead of the FH1 potential
          rfac = rfac + 2*Q2r
          afac = afac + 2*Q2a
          cfac = cfac - 2*(Q2r+Q2a)
       end if
    else if (DFHPower==3) then
       rfac = 2*Q2r*Q1r
       afac = 2*Q2a*Q1a
       cfac = -2*(Q2r*Q1a+Q2a*Q1r)
    else if (DFHPower==4) then
       rfac = Q2r*Q2r
       afac = Q2a*Q2a
       cfac = -2*Q2r*Q2a
    else
       call stoperror("Wrong DFHPower")
    end if

    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_2r+2*DFHPower,eps,C,&
         a21,a21_e,a21_T,a21_ee,a21_TT,a21_eT,a21_eee,a21_eeT,a21_eTT, fac_in=rfac)
    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_2a+2*DFHPower,eps,C,&
         a22,a22_e,a22_T,a22_ee,a22_TT,a22_eT,a22_eee,a22_eeT,a22_eTT, fac_in=afac)
    call calcIcTildeSingleTerm(x,x_T,x_TT,y,y_T,y_TT,eta,lambda_ar+2*DFHPower,eps,C,&
         a23,a23_e,a23_T,a23_ee,a23_TT,a23_eT,a23_eee,a23_eeT,a23_eTT, fac_in=cfac)
    a2 = a21 + a22 + a23
    a2_e = a21_e + a22_e + a23_e
    a2_ee = a21_ee + a22_ee + a23_ee
    a2_eee = a21_eee + a22_eee + a23_eee
    a2_T = a21_T + a22_T + a23_T
    a2_TT = a21_TT + a22_TT + a23_TT
    a2_eT = a21_eT + a22_eT + a23_eT
    a2_eeT = a21_eeT + a22_eeT + a23_eeT
    a2_eTT = a21_eTT + a22_eTT + a23_eTT
  end subroutine calcA22TildeSigmaCorrection_Q


  !> Calculate part of second order monomer perturbation (a22)
  !! a22 is divided by the packing fraction
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA22TildeTVn(nc,i,j,s_vc,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a2
    real, optional, intent(out) ::  a2_T,a2_V,a2_TT,a2_VV,a2_TV,a2_VVV,a2_VVT,a2_VTT
    real, optional, dimension(nc), intent(out) :: a2_n,a2_Tn,a2_Vn,a2_VVn,a2_VTn
    real, optional, dimension(nc,nc), intent(out) :: a2_nn,a2_Vnn
    ! Locals
    real :: a2C_T,a2C_V,a2C_TT,a2C_VV,a2C_TV,a2C_VVV,a2C_VVT,a2C_VTT
    real, dimension(nc) :: a2C_n,a2C_Tn,a2C_Vn,a2C_VVn,a2C_VTn
    real, dimension(nc,nc) :: a2C_nn,a2C_Vnn
    real :: a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx
    real :: a2Cc,a2Cc_e,a2Cc_T,a2Cc_ee,a2Cc_TT,a2Cc_eT,a2Cc_eee,a2Cc_eeT,a2Cc_eTT
    real :: x0, x0_T, x0_TT !< Reduced center-center hard sphere distance
    real :: y, y_T, y_TT !< Reduced center-center hard sphere distance (sigma_eff)
    real :: eta !< Packing fraction
    real :: lambda_a !< Mie potential attractive exponent
    real :: lambda_r !< Mie potential repulsive exponent
    real :: eps !< Well depth div. temperature (K)
    real :: d, d_T, d_TT !< Hard sphere diameter
    real :: s, s_T, s_TT !< Sigma
    integer :: difflevel
    type(saftvrmie_zeta) :: x0z, x1z

    difflevel = 1
    if (present(a2_VVV) .or. present(a2_VVT) .or. present(a2_VTT) .or. &
         present(a2_VVn) .or. present(a2_Vnn) .or. present(a2_VTn)) then
       difflevel = 3
    else if (present(a2_VV) .or. present(a2_TV) .or. present(a2_TT) .or. &
         present(a2_Vn) .or. present(a2_nn) .or. present(a2_Tn)) then
       difflevel = 2
    endif

    s = saftvrmie_param%sigma_ij(i,j)
    s_T = 0.0
    s_TT = 0.0
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       d = s_vc%d_pure%zx
       call allocate_saftvrmie_zeta(nc,x0z)
       call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x0z)
       x0 = x0z%zx
    else
       d = s_vc%dhs%d(i,j)
       d_T = s_vc%dhs%d_T(i,j)
       d_TT = s_vc%dhs%d_TT(i,j)
       call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,x0,x0_T,x0_TT)
    endif
    eta = s_vc%zeta%zx
    lambda_a = saftvrmie_param%lambda_a_ij(i,j)
    lambda_r = saftvrmie_param%lambda_r_ij(i,j)
    eps = saftvrmie_param%eps_divk_ij(i,j)
    call calcA22Tilde(x0,eta,lambda_a,lambda_r,eps,&
         a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx)
    if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
       call convert_zeta_zeta_to_TVn(nc,x0z,s_vc%zeta,&
            a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,0.0,a2_eex,a2_exx,&
            a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn,difflevel=difflevel)
    else
       call convert_zeta_x_to_TVn(nc,x0,x0_T,x0_TT,s_vc%zeta,&
            a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx,&
            a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
            a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn,difflevel=difflevel)
    endif

    if (svrm_opt%quantum_correction_hs > 0) then
       ! Shift integral limits to sigma_eff instead of sigma
       s = s_vc%sigma_eff%d(i,j)
       s_T = s_vc%sigma_eff%d_T(i,j)
       s_TT = s_vc%sigma_eff%d_TT(i,j)
       if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
          call allocate_saftvrmie_zeta(nc,x1z)
          call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,s_vc%d_pure,x1z)
          y = x1z%zx
       else
          call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
       endif
       if (y > 1) then
          if (svrm_opt%hardsphere_EoS == HS_EOS_PURE_DIJ) then
             call calcA22TildeSigmaCorr_TVn_PureHSRef(nc,x0z,x1z,s_vc%zeta,lambda_a,lambda_r,eps,&
                  a2Cc,a2C_T,a2C_V,a2C_n,a2C_TT,a2C_VV,a2C_TV,a2C_Tn,a2C_Vn,a2C_nn,&
                  a2C_VVV,a2C_VVT,a2C_VTT,a2C_VVn,a2C_Vnn,a2C_VTn,difflevel=difflevel)
          else
             call calcA22TildeSigmaCorrection(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,eps,&
                  a2Cc,a2Cc_e,a2Cc_T,a2Cc_ee,a2Cc_TT,a2Cc_eT,a2Cc_eee,a2Cc_eeT,a2Cc_eTT)
             call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,s_vc%zeta,&
                  a2Cc,a2Cc_e,a2Cc_T,a2Cc_ee,a2Cc_TT,a2Cc_eT,a2Cc_eee,a2Cc_eeT,a2Cc_eTT,&
                  a2C_T,a2C_V,a2C_n,a2C_TT,a2C_VV,a2C_TV,a2C_Tn,a2C_Vn,a2C_nn,&
                  a2C_VVV,a2C_VVT,a2C_VTT,a2C_VVn,a2C_Vnn,a2C_VTn,difflevel=difflevel)
          endif
          call calc_a0_plus_a1(nc,a2Cc,a2,&
               a2C_T,a2C_V,a2C_n,&
               a2_T,a2_V,a2_n,&
               a2C_TT,a2C_VV,a2C_TV,a2C_Tn,a2C_Vn,a2C_nn,&
               a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
               a2C_VVV,a2C_VVT,a2C_VTT,a2C_VVn,a2C_Vnn,a2C_VTn,&
               a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
       endif
    endif
    call cleanup_saftvrmie_zeta(x0z)
    call cleanup_saftvrmie_zeta(x1z)
  end subroutine calcA22TildeTVn

  !> Calculate 1+chi and differentials
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA2CorrectionVn(nc,V,n,i,j,alpha,zeta_bar,&
       c2,c2_T,c2_V,c2_n,c2_TT,c2_VV,c2_TV,c2_Tn,c2_Vn,c2_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: i,j !< Binary of interest
    type(saftvrmie_zeta), intent(in) :: zeta_bar
    type(saftvrmie_dhs), intent(in) :: alpha
    ! Output
    real, intent(out) :: c2
    real, optional, intent(out) ::  c2_V,c2_VV,c2_T,c2_TT,c2_TV
    real, optional, dimension(nc), intent(out) :: c2_n,c2_Vn,c2_Tn
    real, optional, dimension(nc,nc), intent(out) :: c2_nn
    ! Locals
    real :: zeta, alp, alp_T, alp_TT
    real :: chi, chi_z, chi_zz, chi_zzz
    real :: chi_a, chi_aa, chi_az, chi_aaz, chi_azz
    zeta = zeta_bar%zx
    alp = alpha%d(i,j)
    alp_T = alpha%d_T(i,j)
    alp_TT = alpha%d_TT(i,j)
    call calcCorrectionFactorA2(zeta, alp, saftvrmie_param%f_alpha_ij(:,i,j), &
         chi, chi_z, chi_zz, chi_zzz, chi_a, chi_aa, chi_az, chi_aaz, chi_azz)
    c2 = 1.0 + chi
    ! if ( present(c2_VV) .or. present(c2_Vn) .or. present(c2_nn)) then
    !   call combineZetaXBarDifferentials(nc,chi,chi_z,chi_zz,chi_zzz,&
    !        zeta_bar,a_V=c2_V,a_n=c2_n,&
    !        a_VV=c2_VV,a_Vn=c2_Vn,a_nn=c2_nn)
    ! else if (present(c2_V) .or. present(c2_n)) then
    !   call combineZetaXBarDifferentials(nc,chi,chi_z,chi_zz,chi_zzz,&
    !        zeta_bar,a_V=c2_V,a_n=c2_n)
    ! endif
    if ( present(c2_VV) .or. present(c2_Vn) .or. present(c2_nn) .or.&
         present(c2_TT) .or. present(c2_Tn) .or. present(c2_TV)) then
       call convert_zeta_x_to_TVn(nc,alp,alp_T,alp_TT,zeta_bar,&
            chi,chi_z,chi_a,chi_zz,chi_aa,chi_az,chi_zzz,chi_zzz,chi_aaz,&
            a1_T=c2_T,a1_V=c2_V,a1_n=c2_n,a1_TT=c2_TT,a1_VV=c2_VV,a1_TV=c2_TV,&
            a1_Tn=c2_Tn,a1_Vn=c2_Vn,a1_nn=c2_nn)
    else if (present(c2_V) .or. present(c2_n) .or. present(c2_T)) then
       call convert_zeta_x_to_TVn(nc,alp,alp_T,alp_TT,zeta_bar,&
            chi,chi_z,chi_a,chi_zz,chi_aa,chi_az,chi_zzz,chi_zzz,chi_aaz,&
            a1_T=c2_T,a1_V=c2_V,a1_n=c2_n)
    endif

  end subroutine calcA2CorrectionVn

  !> Calculate mix-Khs and differentials
  !!
  !! \author Morten Hammer, October 2019
  subroutine calcMixKhsTVn(nc,T,V,n,difflevel,s_vc)
    use saftvrmie_containers, only: allocate_saftvrmie_zeta, &
         cleanup_saftvrmie_zeta, saftvrmie_zeta, saftvrmie_var_container
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< Level of differentials
    type(saftvrmie_var_container), intent(inout) :: s_vc
    ! Locals
    type(saftvrmie_zeta) :: L, K, e
    real :: B,Be,Bee,Bk,Bkk,Bkl,Bke,Bl,Bll,Ble
    integer :: i, j
    !
    call allocate_saftvrmie_zeta(nc,L)
    call allocate_saftvrmie_zeta(nc,K)
    call allocate_saftvrmie_zeta(nc,e)
    call calcZetaX_linear(nc,T,V,n,difflevel=2,dhs=s_vc%dhs,zeta=e)
    call KandL(nc,n,s_vc%dhs,K,L)
    call betabar(e%zx,K%zx,L%zx,B,Be,Bee,Bk,Bkk,Bkl,Bke,Bl,Bll,Ble)
    ! Combine to TVn differential
    s_vc%Khs%zx = B
    s_vc%Khs%zx_T = Bk*K%zx_T + Bl*L%zx_T + Be*e%zx_T
    s_vc%Khs%zx_TT = (Bkk*K%zx_T + Bkl*L%zx_T + Bke*e%zx_T)*K%zx_T &
         + (Bkl*K%zx_T + Bll*L%zx_T + Ble*e%zx_T)*L%zx_T &
         + (Bke*K%zx_T + Ble*L%zx_T + Bee*e%zx_T)*e%zx_T &
         + Bk*K%zx_TT + Bl*L%zx_TT + Be*e%zx_TT
    s_vc%Khs%zx_TV = (Bke*K%zx_T + Ble*L%zx_T + Bee*e%zx_T)*e%zx_V + Be*e%zx_TV
    s_vc%Khs%zx_V = Be*e%zx_V
    s_vc%Khs%zx_VV = Bee*e%zx_V**2 + Be*e%zx_VV
    s_vc%Khs%zx_n = Bk*K%zx_n + Bl*L%zx_n + Be*e%zx_n
    s_vc%Khs%zx_Vn = (Bke*K%zx_n + Ble*L%zx_n + Bee*e%zx_n)*e%zx_V + Be*e%zx_Vn
    s_vc%Khs%zx_Tn = (Bkk*K%zx_n + Bkl*L%zx_n + Bke*e%zx_n)*K%zx_T &
         + (Bkl*K%zx_n + Bll*L%zx_n + Ble*e%zx_n)*L%zx_T &
         + (Bke*K%zx_n + Ble*L%zx_n + Bee*e%zx_n)*e%zx_T &
         + Bk*K%zx_Tn + Bl*L%zx_Tn + Be*e%zx_Tn
    do i=1,nc
       do j=1,nc
          s_vc%Khs%zx_nn(i,j) = (Bkk*K%zx_n(j) + Bkl*L%zx_n(j) + Bke*e%zx_n(j))*K%zx_n(i) &
               + (Bkl*K%zx_n(j) + Bll*L%zx_n(j) + Ble*e%zx_n(j))*L%zx_n(i) &
               + (Bke*K%zx_n(j) + Ble*L%zx_n(j) + Bee*e%zx_n(j))*e%zx_n(i) &
               + Bk*K%zx_nn(i,j) + Bl*L%zx_nn(i,j) + Be*e%zx_nn(i,j)
       enddo
    enddo
    call cleanup_saftvrmie_zeta(L)
    call cleanup_saftvrmie_zeta(K)
    call cleanup_saftvrmie_zeta(e)
  end subroutine calcMixKhsTVn

  subroutine betabar(e,k,l,B,Be,Bee,Bk,Bkk,Bkl,Bke,Bl,Bll,Ble)
    real, intent(in) :: e
    real, intent(in) :: K,L
    real, intent(out) :: B,Be,Bee,Bk,Bkk,Bkl,Bke,Bl,Bll,Ble
    ! Locals
    real :: N,Ne,Nee,D,De,Dee,Dk,Dke,Dl,Dle
    call NandD(e,k,l,N,Ne,Nee,D,De,Dee,Dk,Dke,Dl,Dle)
    B = N/D
    Bk = -N*Dk/D**2
    Bkk = 2*N*Dk**2/D**3
    Bke = (-Ne*D*Dk-N*D*Dke+2*N*Dk*De)/D**3
    Bl = -N*Dl/D**2
    Bll = 2*N*Dl**2/D**3
    Ble = (-Ne*D*Dl-N*D*Dle+2*N*Dl*De)/D**3
    Bkl = 2*N*Dk*Dl/D**3
    Be = (Ne*D - N*De)/D**2
    Bee = (Nee*D**2 - N*D*Dee - 2*D*Ne*De + 2*N*De**2)/D**3
  end subroutine betabar

  subroutine NandD(e,k,l,N,Ne,Nee,D,De,Dee,Dk,Dke,Dl,Dle)
    real, intent(in) :: e
    real, intent(in) :: K,L
    real :: N,Ne,Nee,D,De,Dee,Dk,Dke,Dl,Dle
    ! Locals
    N = (1-e)**4
    Ne = -4*(1-e)**3
    Nee = 12*(1-e)**2
    !
    D =  l*e**4 - 4*l*e**3 + (9*l - 6*k + 1 )*e**2 + (6*k - 2)*e + 1
    De =  4*l*e**3 - 12*l*e**2 + 2*(9*l - 6*k + 1 )*e + 6*k - 2
    Dee =  12*l*e**2 - 24*l*e + 2*(9*l - 6*k + 1 )
    Dk = -6*e**2 + 6*e
    Dke = -12*e + 6
    Dl =  e**4 - 4*e**3 + 9*e**2
    Dle =  4*e**3 - 12*e**2 + 18*e
  end subroutine NandD

  subroutine mbar(nc,n,dhs,M)
    use saftvrmie_containers, only: saftvrmie_zeta, saftvrmie_dhs
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: n(nc)
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    type(saftvrmie_zeta), intent(inout) :: M(3)
    ! Locals
    integer :: l, i
    do l=1,3
       M(l)%zx = 0
       M(l)%zx_T = 0
       M(l)%zx_TT = 0
       M(l)%zx_V = 0
       M(l)%zx_VV = 0
       M(l)%zx_TV = 0
       M(l)%zx_Vn = 0
       M(l)%zx_nn = 0
       do i=1,nc
          M(l)%zx = M(l)%zx + saftvrmie_param%ms(i)*n(i)*dhs%d(i,i)**l
          M(l)%zx_T = M(l)%zx_T + l*saftvrmie_param%ms(i)*n(i)*dhs%d(i,i)**(l-1)*dhs%d_T(i,i)
          if (l == 1) then
             M(l)%zx_TT = M(l)%zx_TT + saftvrmie_param%ms(i)*n(i)*dhs%d_TT(i,i)
          else
             M(l)%zx_TT = M(l)%zx_TT + l*saftvrmie_param%ms(i)*n(i)*dhs%d(i,i)**(l-2)*&
                  ((l-1)*dhs%d_T(i,i)**2 + dhs%d(i,i)*dhs%d_TT(i,i))
          endif
          M(l)%zx_Tn(i) = saftvrmie_param%ms(i)*l*dhs%d(i,i)**(l-1)*dhs%d_T(i,i)
          M(l)%zx_n(i) = saftvrmie_param%ms(i)*dhs%d(i,i)**l
       enddo
    enddo
  end subroutine mbar

  subroutine KandL(nc,n,dhs,K,L)
    use saftvrmie_containers, only: allocate_saftvrmie_zeta, &
         cleanup_saftvrmie_zeta, saftvrmie_zeta
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: n(nc)
    type(saftvrmie_dhs), intent(in) :: dhs !< Hard-sphere diameter and differentials
    type(saftvrmie_zeta), intent(inout) :: K,L
    ! Locals
    integer :: j, i
    type(saftvrmie_zeta) :: M(3)
    real :: ns, denum
    ns = sum(saftvrmie_param%ms*n)
    do i=1,3
       call allocate_saftvrmie_zeta(nc,M(i))
    enddo
    call mbar(nc,n,dhs,M)
    denum = M(3)%zx*ns
    K%zx = M(1)%zx*M(2)%zx/denum
    K%zx_T = (M(1)%zx_T*M(2)%zx + M(1)%zx*M(2)%zx_T - M(3)%zx_T*ns*K%zx)/denum
    K%zx_TT = (M(1)%zx_TT*M(2)%zx + M(1)%zx*M(2)%zx_TT + 2*M(1)%zx_T*M(2)%zx_T &
         - M(3)%zx_TT*ns*K%zx - 2*M(3)%zx_T*ns*K%zx_T)/denum
    K%zx_n = (M(1)%zx_n*M(2)%zx + M(1)%zx*M(2)%zx_n - M(3)%zx_n*ns*K%zx &
         - saftvrmie_param%ms*M(3)%zx*K%zx)/denum
    K%zx_Tn = (M(1)%zx_Tn*M(2)%zx + M(1)%zx_T*M(2)%zx_n + M(1)%zx_n*M(2)%zx_T +&
         M(1)%zx*M(2)%zx_Tn - saftvrmie_param%ms*M(3)%zx_T*K%zx - M(3)%zx_Tn*ns*K%zx - M(3)%zx_T*ns*K%zx_n&
         - saftvrmie_param%ms*M(3)%zx*K%zx_T - M(3)%zx_n*ns*K%zx_T)/denum
    do i=1,nc
       do j=1,nc
          K%zx_nn(i,j) = (M(1)%zx_n(i)*M(2)%zx_n(j) + M(1)%zx_n(j)*M(2)%zx_n(i) &
               - saftvrmie_param%ms(i)*M(3)%zx*K%zx_n(j) - saftvrmie_param%ms(j)*M(3)%zx*K%zx_n(i) &
               - saftvrmie_param%ms(j)*M(3)%zx_n(i)*K%zx - M(3)%zx_n(i)*ns*K%zx_n(j) - M(3)%zx_n(j)*ns*K%zx_n(i) &
               - saftvrmie_param%ms(i)*M(3)%zx_n(j)*K%zx - M(3)%zx_n(j)*ns*K%zx_nn(i,j))/denum
       enddo
    enddo
    !
    K%zx_V = 0
    K%zx_VV = 0
    K%zx_TV = 0
    K%zx_Vn = 0
    !
    denum = M(3)%zx**2*ns
    L%zx = M(2)%zx**3/denum
    L%zx_T = (3*M(2)%zx**2*M(2)%zx_T - 2*M(3)%zx*M(3)%zx_T*ns*L%zx)/denum
    L%zx_TT = (6*M(2)%zx*M(2)%zx_T**2 + 3*M(2)%zx**2*M(2)%zx_TT - 2*M(3)%zx_T**2*ns*L%zx &
         - 2*M(3)%zx*M(3)%zx_TT*ns*L%zx - 4*M(3)%zx*M(3)%zx_T*ns*L%zx_T)/denum
    L%zx_n = (3*M(2)%zx**2*M(2)%zx_n - saftvrmie_param%ms*M(3)%zx**2*L%zx &
         - 2*M(3)%zx*M(3)%zx_n*ns*L%zx)/denum
    L%zx_Tn = (6*M(2)%zx*M(2)%zx_n*M(2)%zx_T + 3*M(2)%zx**2*M(2)%zx_Tn &
         - 2*saftvrmie_param%ms*M(3)%zx*M(3)%zx_T*L%zx&
         - 2*ns*M(3)%zx_n*M(3)%zx_T*L%zx - 2*ns*M(3)%zx*M(3)%zx_Tn*L%zx &
         - 2*ns*M(3)%zx*M(3)%zx_T*L%zx_n - saftvrmie_param%ms*M(3)%zx**2*L%zx_T&
         - 2*ns*M(3)%zx*M(3)%zx_n*L%zx_T)/denum
    do i=1,nc
       do j=1,nc
          L%zx_nn(i,j) = (6*M(2)%zx*M(2)%zx_n(j)*M(2)%zx_n(i) + 3*M(2)%zx**2*M(2)%zx_nn(i,j) &
               - 2*saftvrmie_param%ms(i)*M(3)%zx*M(3)%zx_n(j)*L%zx &
               - saftvrmie_param%ms(i)*M(3)%zx**2*L%zx_n(j) &
               - 2*saftvrmie_param%ms(j)*M(3)%zx*M(3)%zx_n(i)*L%zx&
               - 2*ns*M(3)%zx_n(i)*M(3)%zx_n(j)*L%zx - 2*ns*M(3)%zx*M(3)%zx_n(i)*L%zx_n(j) &
               - saftvrmie_param%ms(j)*M(3)%zx**2*L%zx_n(i) &
               - 2*ns*M(3)%zx*M(3)%zx_n(j)*L%zx_n(i))/denum
       enddo
    enddo
    !
    L%zx_V = 0
    L%zx_VV = 0
    L%zx_TV = 0
    L%zx_Vn = 0

    do i=1,3
       call cleanup_saftvrmie_zeta(M(i))
    enddo
  end subroutine KandL

  !> Calculate chi
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcCorrectionFactorA2(zeta, alpha, f_alpha, chi, chi_z, chi_zz, chi_zzz,&
       chi_a, chi_aa, chi_az, chi_aaz, chi_azz)
    ! Input
    real, intent(in) :: zeta
    real, intent(in) :: alpha
    real, intent(in) :: f_alpha(6)
    ! Output
    real, intent(out) :: chi, chi_z, chi_zz, chi_zzz
    real, intent(out) :: chi_a, chi_aa, chi_az, chi_aaz, chi_azz
    ! Locals
    real :: zeta_2, zeta_3, zeta_4, zeta_7
    real :: f(6), f_a(6), f_aa(6)
    if (svrm_opt%quantum_correction_hs > 0) then
       call calcFunAlpha(alpha, f, f_a, f_aa)
    else
       f = f_alpha
    endif
    zeta_2 = zeta*zeta
    zeta_3 = zeta*zeta_2
    zeta_4 = zeta_3*zeta
    zeta_7 = zeta_4*zeta_3
    chi = zeta*(f(1) + f(2)*zeta_4 + f(3)*zeta_7)
    chi_z = f(1) + 5.0*f(2)*zeta_4 + 8.0*f(3)*zeta_7
    chi_zz = zeta_3*(20.0*f(2) + 56.0*f(3)*zeta_3)
    chi_zzz = zeta_2*(60.0*f(2) + 336.0*f(3)*zeta_3)
    if (svrm_opt%quantum_correction_hs > 0) then
       chi_a = zeta*(f_a(1) + f_a(2)*zeta_4 + f_a(3)*zeta_7)
       chi_aa = zeta*(f_aa(1) + f_aa(2)*zeta_4 + f_aa(3)*zeta_7)
       chi_az = f_a(1) + 5.0*f_a(2)*zeta_4 + 8.0*f_a(3)*zeta_7
       chi_aaz = f_aa(1) + 5.0*f_aa(2)*zeta_4 + 8.0*f_aa(3)*zeta_7
       chi_azz = zeta_3*(20.0*f_a(2) + 56.0*f_a(3)*zeta_3)
    else
       chi_a = 0.0
       chi_aa = 0.0
       chi_az = 0.0
       chi_aaz = 0.0
       chi_azz = 0.0
    endif
  end subroutine calcCorrectionFactorA2

  !> Calculate Khs and differentials
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcKhsTVn(nc,T,V,n,difflevel,s_vc)
    ! Input
    use saftvrmie_options, only: KHS_EOS_LAFITTE, &
         KHS_EOS_BMCSL, KHS_EOS_SANTOS
    use utilities, only: is_numerically_equal
    use numconstants, only: machine_prec
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< Level of differentials
    type(saftvrmie_var_container), intent(inout) :: s_vc
    !
    select case(svrm_opt%Khs_EoS)
    case(KHS_EOS_LAFITTE)
       call calcKhsTVnCS(nc,T,V,n,difflevel,s_vc)
    case(KHS_EOS_BMCSL)
       if (.not. is_numerically_equal(maxval(saftvrmie_param%ms),1.0,1e6*machine_prec)) then
          call stoperror("KHS_EOS_BMCSL: Not yet implemented for chain molecules (ms>1).")
       endif
       call calcMixKhsTVn(nc,T,V,n,difflevel,s_vc)
    case(KHS_EOS_SANTOS)
       call stoperror("KHS_EOS_SANTOS not yet implemented")
    case default
       call stoperror("Wrong KHS EOS.")
    end select
  end subroutine calcKhsTVn

  !> Calculate Khs and differentials
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcKhsTVnCS(nc,T,V,n,difflevel,s_vc)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< Level of differentials
    type(saftvrmie_var_container), intent(inout) :: s_vc
    ! Locals
    real :: eta
    real :: K, K_e, K_ee, K_eee
    !
    eta = s_vc%zeta%zx
    call calcKhs(eta, K, K_e, K_ee, K_eee)
    s_vc%Khs%zx = K
    if (difflevel > 1) then
       call convert_zeta_x_to_TVn(nc,1.0,0.0,0.0,s_vc%zeta,&
            K,K_e,0.0,K_ee,0.0,0.0,K_eee,0.0,0.0,&
            s_vc%Khs%zx_T,s_vc%Khs%zx_V,s_vc%Khs%zx_n,s_vc%Khs%zx_TT,&
            s_vc%Khs%zx_VV,s_vc%Khs%zx_TV,&
            s_vc%Khs%zx_Tn,s_vc%Khs%zx_Vn,s_vc%Khs%zx_nn,s_vc%Khs%zx_VVV,&
            s_vc%Khs%zx_VVT,s_vc%Khs%zx_VTT,&
            s_vc%Khs%zx_VVn,s_vc%Khs%zx_Vnn,s_vc%Khs%zx_VTn)
    else if (difflevel > 0) then
       call convert_zeta_x_to_TVn(nc,1.0,0.0,0.0,s_vc%zeta,&
            K,K_e,0.0,K_ee,0.0,0.0,K_eee,0.0,0.0,&
            s_vc%Khs%zx_T,s_vc%Khs%zx_V,s_vc%Khs%zx_n,s_vc%Khs%zx_TT,&
            s_vc%Khs%zx_VV,s_vc%Khs%zx_TV,&
            s_vc%Khs%zx_Tn,s_vc%Khs%zx_Vn,s_vc%Khs%zx_nn)
    else
       call convert_zeta_x_to_TVn(nc,1.0,0.0,0.0,s_vc%zeta,&
            K,K_e,0.0,K_ee,0.0,0.0,K_eee,0.0,0.0,&
            s_vc%Khs%zx_T,s_vc%Khs%zx_V,s_vc%Khs%zx_n)
    endif
  end subroutine calcKhsTVnCS

  !> Calculate isothermal hard sphere compressibillity factor
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcKhs(eta, K, K_e, K_ee, K_eee)
    ! Input
    real, intent(in) :: eta
    ! Output
    real, intent(out) :: K, K_e, K_ee, K_eee
    ! Locals
    real :: one_min_eta
    real :: eta_n(10)
    real :: denum
    integer :: i
    eta_n(1) = eta
    do i=2,10
       eta_n(i) = eta_n(i-1)*eta
    enddo
    one_min_eta = 1.0 - eta
    denum = 1.0 + 4.0*eta + 4.0*eta_n(2) - 4.0*eta_n(3) + eta_n(4)
    K = one_min_eta**4/denum
    K_e = 4.0*(eta_n(2) - 5.0*eta - 2.0)*one_min_eta**3/denum**2
    K_ee = 4.0*(3.0*eta_n(6) - 30.0*eta_n(5) + 77.0*eta_n(4) - 80.0*eta_n(3)&
         + 39.0*eta_n(2) + 82.0*eta + 17.0)*one_min_eta**2/denum**3
    K_eee = 48.0*(eta_n(10) - 15.0*eta_n(9) + 77.0*eta_n(8) - 210.0*eta_n(7)&
         + 372.0*eta_n(6) - 352.0*eta_n(5) + 238.0*eta_n(3)&
         - 109.0*eta_n(2) - 97.0*eta - 13.0)*one_min_eta/denum**4
  end subroutine calcKhs

  !****************************************************************************************
  ! THIRD ORDER TERM
  !****************************************************************************************

  !> Calculate a3(zeta)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA3(nc,T,V,n,s_vc,&
       a3,a3_T,a3_V,a3_n,a3_TT,a3_VV,a3_TV,a3_Tn,a3_Vn,a3_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_var_container), intent(inout) :: s_vc
    ! Output
    real, intent(out) :: a3
    real, optional, intent(out) :: a3_T,a3_V,a3_TT,a3_VV,a3_TV
    real, optional, dimension(nc), intent(out) :: a3_n,a3_Tn,a3_Vn
    real, optional, dimension(nc,nc), intent(out) :: a3_nn
    ! Locals
    integer :: difflevel,i,j
    real :: a3_z,a3_zz,z,a3_a,a3_aa,a3_az
    real :: eps,eps_T,eps_TT,d,d_T,d_TT
    if ( present(a3_TT) .or. present(a3_VV) .or. present(a3_TV) .or. &
         present(a3_Tn) .or. present(a3_Vn) .or. present(a3_nn)) then
       difflevel = 2
    else if (present(a3_T) .or. present(a3_V) .or. present(a3_n)) then
       difflevel = 1
    else
       difflevel = 0
    endif
    if (svrm_opt%a3_model == A3_SIJ_PREFAC) then
       !> Calculate dummy prefactor
       call calcZetaPreFactor(nc,T,V,n,difflevel,s_vc%zeta_a3)
    endif
    z = s_vc%zeta_bar%zx
    do i=1,nc
       do j=i,nc
          call calcA3zeta(s_vc%eps_divk_eff%d(i,j),z,s_vc%alpha%d(i,j), &
               saftvrmie_param%f_alpha_ij(:,i,j),s_vc%a3ij%am(i,j),a3_z,a3_zz,&
               a3_a,a3_aa,a3_az)
          select case(difflevel)
          case(2)
             call convert_zeta_x_to_TVn(nc,s_vc%alpha%d(i,j),s_vc%alpha%d_T(i,j),&
                  s_vc%alpha%d_TT(i,j),s_vc%zeta_bar,&
                  a3,a3_z,a3_a,a3_zz,a3_aa,a3_az,0.0,0.0,0.0,&
                  a1_T=s_vc%a3ij%am_T(i,j),a1_V=s_vc%a3ij%am_V(i,j),a1_n=s_vc%a3ij%am_n(:,i,j),&
                  a1_TT=s_vc%a3ij%am_TT(i,j),a1_VV=s_vc%a3ij%am_VV(i,j),&
                  a1_TV=s_vc%a3ij%am_TV(i,j),a1_Tn=s_vc%a3ij%am_Tn(:,i,j),&
                  a1_Vn=s_vc%a3ij%am_Vn(:,i,j),a1_nn=s_vc%a3ij%am_nn(:,:,i,j))
          case(1)
             call convert_zeta_x_to_TVn(nc,s_vc%alpha%d(i,j),s_vc%alpha%d_T(i,j),&
                  s_vc%alpha%d_TT(i,j),s_vc%zeta_bar,&
                  a3,a3_z,a3_a,a3_zz,a3_aa,a3_az,0.0,0.0,0.0,&
                  a1_T=s_vc%a3ij%am_T(i,j),a1_V=s_vc%a3ij%am_V(i,j),a1_n=s_vc%a3ij%am_n(:,i,j))
          end select
          if (svrm_opt%a3_model == A3_SIJ_PREFAC) then
             select case(difflevel)
             case(2)
                call calc_a_zeta_product(nc,s_vc%zeta_a3,a=s_vc%a3ij%am(i,j),&
                     a_T=s_vc%a3ij%am_T(i,j),a_V=s_vc%a3ij%am_V(i,j),&
                     a_n=s_vc%a3ij%am_n(:,i,j),a_TT=s_vc%a3ij%am_TT(i,j),&
                     a_VV=s_vc%a3ij%am_VV(i,j),a_TV=s_vc%a3ij%am_TV(i,j),&
                     a_Tn=s_vc%a3ij%am_Tn(:,i,j),a_Vn=s_vc%a3ij%am_Vn(:,i,j),&
                     a_nn=s_vc%a3ij%am_nn(:,:,i,j))
             case(1)
                call calc_a_zeta_product(nc,s_vc%zeta_a3,a=s_vc%a3ij%am(i,j),&
                     a_T=s_vc%a3ij%am_T(i,j),a_V=s_vc%a3ij%am_V(i,j),&
                     a_n=s_vc%a3ij%am_n(:,i,j))
             case(0)
                s_vc%a3ij%am(i,j) = s_vc%a3ij%am(i,j)*s_vc%zeta_a3%zx
             end select
             d = s_vc%sigma_eff%d(i,j)
             d_T = s_vc%sigma_eff%d_T(i,j)
             d_TT = s_vc%sigma_eff%d_TT(i,j)
             call calc_a_d3_product(nc,s_vc%a3ij,i,j,d,d_T,d_TT,difflevel,.false.)
          endif
          if (svrm_opt%quantum_correction_hs > 0) then
             ! Correct for temperature dependence in eps_div_k
             eps_TT = s_vc%eps_divk_eff%d_TT(i,j)
             eps_T = s_vc%eps_divk_eff%d_T(i,j)
             eps = s_vc%eps_divk_eff%d(i,j)
             call calc_a_d3_product(nc,s_vc%a3ij,i,j,eps,eps_T,eps_TT,difflevel,.true.)
          endif
       enddo
    enddo
    call s_vc%a3ij%mirror()
    call calc_double_sum(nc,n,s_vc%a3ij,&
         a3,a_T=a3_T,a_V=a3_V,a_n=a3_n,a_TT=a3_TT,a_VV=a3_VV,a_TV=a3_TV,&
         a_Tn=a3_Tn,a_Vn=a3_Vn,a_nn=a3_nn)

  end subroutine calcA3

  !> Calculate a3(zeta,alpha)
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcA3zeta(eps, zeta, alpha, f_alpha, a3, a3_z, a3_zz, a3_a, a3_aa, a3_az)
    ! Input
    real, intent(in) :: eps
    real, intent(in) :: zeta
    real, intent(in) :: alpha
    real, intent(in) :: f_alpha(6)
    ! Output
    real, intent(out) :: a3, a3_z, a3_zz, a3_a, a3_aa, a3_az
    ! Locals
    real :: exponent, exp_diff_zeta, exp_diff_alpha(1:2)
    real :: f(6), f_a(6), f_aa(6), exp_exponent
    real :: exp_diff_alpha_zeta,zeta_pre
    if (svrm_opt%quantum_correction_hs > 0) then
       call calcFunAlpha(alpha, f, f_a, f_aa)
    else
       f = f_alpha
    endif
    exponent = f(5)*zeta + f(6)*zeta**2
    exp_diff_zeta = f(5) + 2.0*f(6)*zeta
    exp_exponent = exp(exponent)
    select case(svrm_opt%a3_model)
    case(A3_LAFITTE)
       zeta_pre = zeta
       a3 = - eps**3*f(4)*zeta*exp_exponent
       a3_z = a3*(1.0/zeta + exp_diff_zeta)
       a3_zz = a3*(2.0*f(5)/zeta + 6.0*f(6) + exp_diff_zeta**2)
    case(A3_SIJ_PREFAC)
       zeta_pre = 1.0 ! Handled elsewhere
       a3 = - eps**3*f(4)*exp_exponent
       a3_z = a3*exp_diff_zeta
       a3_zz = a3*(2.0*f(6) + exp_diff_zeta**2)
    case default
       call stoperror("Wrong A3 model parameter")
    end select
    if (svrm_opt%quantum_correction_hs > 0) then
       exp_diff_alpha(1) = f_a(5)*zeta + f_a(6)*zeta**2
       exp_diff_alpha(2) = f_aa(5)*zeta + f_aa(6)*zeta**2
       exp_diff_alpha_zeta = f_a(5) + 2.0*f_a(6)*zeta
       a3_a = -eps**3*zeta_pre*exp_exponent*(f_a(4) + f(4)*exp_diff_alpha(1))
       a3_aa = -eps**3*zeta_pre*exp_exponent*(f_aa(4) + 2.0*f_a(4)*exp_diff_alpha(1) &
            + f(4)*exp_diff_alpha(1)**2 + f(4)*exp_diff_alpha(2))
       a3_az = -eps**3*zeta_pre*exp_exponent*(f_a(4)*exp_diff_zeta &
            + f(4)*exp_diff_zeta*exp_diff_alpha(1) + f(4)*exp_diff_alpha_zeta)
       if (svrm_opt%a3_model == A3_LAFITTE) then
          a3_az = a3_az + a3_a/zeta
       endif
    else
       a3_a = 0.0
       a3_aa = 0.0
       a3_az = 0.0
    endif
  end subroutine calcA3zeta

  ! !> Calculate zeta x bar
  ! !!
  ! !! \author Morten Hammer, February 2018
  ! subroutine calcZetaXBar(nc,T,V,n,difflevel,saftvrmie_sigma_eff,saftvrmie_zb)
  !   implicit none
  !   ! Input
  !   integer, intent(in) :: nc !< Number of components
  !   real, intent(in) :: T !< Temperature [K]
  !   real, intent(in) :: V !< Volume [m3]
  !   real, intent(in) :: n(nc) !< Mol numbers [mol]
  !   integer, intent(in) :: difflevel !< Level of differentilas
  !   type(saftvrmie_dhs), intent(in) :: saftvrmie_sigma_eff
  !   ! Output
  !   type(saftvrmie_zeta), intent(inout) :: saftvrmie_zb
  !   ! Locals
  !   integer :: i,j
  !   real :: ns, single_sum_ms_sigma3(nc), sum_sum_sigma3
  !   real :: factor
  !   ns = sum(saftvrmie_param%ms*n)
  !   do i=1,nc
  !     single_sum_ms_sigma3(i) = sum(saftvrmie_param%ms*n*saftvrmie_param%sigma_ij_cube(i,:))
  !   enddo
  !   sum_sum_sigma3 = sum(saftvrmie_param%ms*n*single_sum_ms_sigma3)
  !   factor = pi*N_AVOGADRO/(6.0*V*ns)
  !   saftvrmie_zb%zx = factor*sum_sum_sigma3
  !   if (difflevel > 0) then
  !     saftvrmie_zb%zx_v = -saftvrmie_zb%zx/V
  !     ! Note that we assume sigma_ij = sigma_ji
  !     saftvrmie_zb%zx_n = factor*2.0*saftvrmie_param%ms*single_sum_ms_sigma3&
  !          - saftvrmie_param%ms*saftvrmie_zb%zx/ns
  !   endif

  !   if (difflevel > 1) then
  !     saftvrmie_zb%zx_vv = 2.0*saftvrmie_zb%zx/V**2
  !     saftvrmie_zb%zx_Vn = -saftvrmie_zb%zx_n/V
  !     do i=1,nc
  !       do j=1,nc
  !         saftvrmie_zb%zx_nn(i,j) = factor*2.0*saftvrmie_param%ms(i)*saftvrmie_param%ms(j)*&
  !              saftvrmie_param%sigma_ij_cube(i,j) &
  !              - (saftvrmie_param%ms(i)*saftvrmie_zb%zx_n(j) &
  !              + saftvrmie_param%ms(j)*saftvrmie_zb%zx_n(i))/ns
  !       enddo
  !     enddo
  !   endif

  !   if (difflevel > 2) then
  !     saftvrmie_zb%zx_vvv = -6.0*saftvrmie_zb%zx/V**3
  !     saftvrmie_zb%zx_VVn = 2.0*saftvrmie_zb%zx_n/V**2
  !     saftvrmie_zb%zx_Vnn = -saftvrmie_zb%zx_nn/V
  !   endif
  ! end subroutine calcZetaXBar

  !> Calculate volume and mol number differentials from
  !! zeta differentias
  !!
  !! \author Morten Hammer, February 2018
  subroutine combineZetaXBarDifferentials(nc,a,a_z,a_zz,a_zzz,&
       saftvrmie_zb,&
       a_V,a_n,a_VV,a_VVV,a_Vn,a_nn,a_VVn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: a,a_z,a_zz,a_zzz !<
    type(saftvrmie_zeta), intent(in) :: saftvrmie_zb
    ! Output
    real, optional, intent(out) :: a_V,a_VV,a_VVV
    real, optional, dimension(nc), intent(out) :: a_n,a_Vn,a_VVn
    real, optional, dimension(nc,nc), intent(out) :: a_nn
    ! Locals
    integer :: i,j
    if (present(a_V)) then
       a_v = a_z*saftvrmie_zb%zx_V
    endif
    if (present(a_VV)) then
       a_vv = a_zz*saftvrmie_zb%zx_V**2 + a_z*saftvrmie_zb%zx_VV
    endif
    if (present(a_VVV)) then
       a_vvv = a_zzz*saftvrmie_zb%zx_V**3 + 3.0*a_zz*saftvrmie_zb%zx_V*saftvrmie_zb%zx_VV &
            + a_z*saftvrmie_zb%zx_VVV
    endif
    if (present(a_n)) then
       a_n = a_z*saftvrmie_zb%zx_n
    endif
    if (present(a_Vn)) then
       a_Vn = a_zz*saftvrmie_zb%zx_V*saftvrmie_zb%zx_n + a_z*saftvrmie_zb%zx_Vn
    endif
    if (present(a_VVn)) then
       a_VVn =(a_zzz*saftvrmie_zb%zx_V**2 + a_zz*saftvrmie_zb%zx_VV)*saftvrmie_zb%zx_n &
            + 2.0*a_zz*saftvrmie_zb%zx_V*saftvrmie_zb%zx_Vn + a_z*saftvrmie_zb%zx_VVn
    endif
    if (present(a_nn)) then
       do i=1,nc
          do j=1,nc
             a_nn(i,j) = a_zz*saftvrmie_zb%zx_n(i)*saftvrmie_zb%zx_n(j) &
                  + a_z*saftvrmie_zb%zx_nn(i,j)
          enddo
       enddo
    endif
  end subroutine combineZetaXBarDifferentials

  !> Calculate dimensionless van der Waals enery and differentials
  !!
  !! \author Morten Hammer, March 2018
  subroutine calcAlpha(nc,sigma_eff,eps_divk_eff,T,s_vc,a,a_T,a_TT)
    ! Input
    integer, intent(in) :: nc !< Number of components
    type(saftvrmie_dhs), intent(in) :: sigma_eff !<
    type(saftvrmie_dhs), intent(in) :: eps_divk_eff !<
    real, intent(in) :: T !< Temperature
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: a(nc,nc) !<
    real, optional, intent(out) :: a_T(nc,nc),a_TT(nc,nc)
    ! Locals
    integer :: i,j
    real :: sigma_ratio,C,lama,lamr,DmT,se,se_T,se_TT
    real :: sigma_ratio_a,sigma_ratio_r,sigma_ratio_2
    real :: Ma,Mr,Q1a,Q1r,Q2a,Q2r
    real :: eps_dk,eps_dk_T,eps_dk_TT,eps_divk_Mie
    if (svrm_opt%quantum_correction_hs > 2) then
       call stoperror("saftvrmie_dispersion::calcAlpha: quantum correction not implemented beyond second order")
    endif
    if (svrm_opt%quantum_correction_hs == 0) then
       a = saftvrmie_param%alpha_ij
       if (present(a_T)) then
          a_T = 0.0
       endif
       if (present(a_TT)) then
          a_TT = 0.0
       endif
    else
       ! Add Mie contribution
       do i=1,nc
          do j=1,nc
             se = sigma_eff%d(i,j)
             se_T = sigma_eff%d_T(i,j)
             se_TT = sigma_eff%d_TT(i,j)
             lama = saftvrmie_param%lambda_a_ij(i,j)
             lamr = saftvrmie_param%lambda_r_ij(i,j)
             sigma_ratio = saftvrmie_param%sigma_ij(i,j)/se
             sigma_ratio_a = sigma_ratio**(lama)
             sigma_ratio_r = sigma_ratio**(lamr)
             sigma_ratio_2 = sigma_ratio**2
             DmT = s_vc%DFeynHibbsij%D(i,j)/saftvrmie_param%sigma_ij(i,j)**2
             Ma = sigma_ratio_a/(lama - 3.0)
             Mr = sigma_ratio_r/(lamr - 3.0)
             Q1a = saftvrmie_param%Quantum_const_1a_ij(i,j)
             Q1a = Q1a*sigma_ratio_a*sigma_ratio_2/(lama - 1.0)
             Q1r = saftvrmie_param%Quantum_const_1r_ij(i,j)
             Q1r = Q1r*sigma_ratio_r*sigma_ratio_2/(lamr - 1.0)
             a(i,j) = Ma - Mr + DmT*(Q1a - Q1r)
             if (present(a_T)) then
                a_T(i,j) =  - (Ma*lama - Mr*lamr &
                     + DmT*(Q1a*(lama + 2.0) - Q1r*(lamr + 2.0)))*(se_T/se) &
                     - DmT*(Q1a - Q1r)/T
             endif
             if (present(a_TT)) then
                a_TT(i,j) =  (Ma*lama*(lama + 1.0) - Mr*lamr*(lamr + 1.0) &
                     + DmT*(Q1a*(lama + 2.0)*(lama + 3.0) &
                     - Q1r*(lamr + 2.0)*(lamr + 3.0)))*(se_T/se)**2 &
                     - (Ma*lama - Mr*lamr &
                     + DmT*(Q1a*(lama + 2.0) - Q1r*(lamr + 2.0)))*(se_TT/se) &
                     + 2.0*DmT*(Q1a*(lama + 2.0) - Q1r*(lamr + 2.0))*(se_T/(se*T)) &
                     + 2.0*DmT*(Q1a - Q1r)/T**2
             endif
             if (svrm_opt%quantum_correction_hs == 2) then
                Q2a = saftvrmie_param%Quantum_const_2a_ij(i,j)
                Q2a = Q2a*sigma_ratio_a*sigma_ratio_2**2/(lama + 1.0)
                Q2r = saftvrmie_param%Quantum_const_2r_ij(i,j)
                Q2r = Q2r*sigma_ratio_r*sigma_ratio_2**2/(lamr + 1.0)
                a(i,j) = a(i,j) + DmT**2*(Q2a - Q2r)
                if (present(a_T)) then
                   a_T(i,j) = a_T(i,j) - DmT**2*(Q2a*(lama + 4.0) - Q2r*(lamr + 4.0))*(se_T/se)&
                        - 2.0*DmT**2*(Q2a - Q2r)/T
                endif
                if (present(a_TT)) then
                   a_TT(i,j) = a_TT(i,j) + DmT**2*(Q2a*(lama + 4.0)*(lama + 5.0) &
                        - Q2r*(lamr + 4.0)*(lamr + 5.0))*(se_T/se)**2 &
                        + 4.0*DmT**2*(Q2a*(lama + 4.0) - Q2r*(lamr + 4.0))*(se_T/(se*T)) &
                        - DmT**2*(Q2a*(lama + 4.0) - Q2r*(lamr + 4.0))*(se_TT/se) &
                        + 6.0*DmT**2*(Q2a - Q2r)/T**2
                endif
             endif
             ! Correct for effective epsilon
             eps_dk = eps_divk_eff%d(i,j)
             eps_dk_T = eps_divk_eff%d_T(i,j)
             eps_dk_TT = eps_divk_eff%d_TT(i,j)
             eps_divk_Mie = saftvrmie_param%eps_divk_ij(i,j)
             C = saftvrmie_param%Cij(i,j)
             C = C*eps_divk_Mie/eps_dk
             if (present(a_TT)) then
                a_TT(i,j) = C*(a_TT(i,j) - 2.0*a_T(i,j)*eps_dk_T/eps_dk &
                     + 2.0*a(i,j)*(eps_dk_T/eps_dk)**2 - a(i,j)*(eps_dk_TT/eps_dk))
             endif
             if (present(a_T)) then
                a_T(i,j) = C*(a_T(i,j) - a(i,j)*eps_dk_T/eps_dk)
             endif
             a(i,j) = C*a(i,j)
          enddo
       enddo
    endif
  end subroutine calcAlpha

  !> Calculate truncation (and shifted) correction for quantum-corrected Mie fluid
  !! dAc = A - Ac
  !! \author Morten Hammer, November 2018
  subroutine calc_delta_Ac(nc,T,V,n,r_c,saftvrmie_vc,divide_by_n,&
       a,a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn)
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    real, intent(in) :: r_c !< Reduced cut off distance (actual cut off: r_c*sigma)  [-]
    type(saftvrmie_var_container), intent(in) :: saftvrmie_vc
    logical, intent(in) :: divide_by_n
    ! Output
    real, intent(out) :: a
    real, optional, intent(out) ::  a_T,a_V,a_TT,a_VV,a_TV
    real, optional, dimension(nc), intent(out) :: a_n,a_Tn,a_Vn
    real, optional, dimension(nc,nc), intent(out) :: a_nn
    ! Locals
    integer :: i !<
    real :: kc, L, Lq1, Lq2, inv_rc, inv_sigma, lamr, lama
    real :: Q1a, Q1r, Q2a, Q2r
    real :: D, D_T, D_TT          !< Quantum par. and derivatives
    real :: D2, D2_T, D2_TT       !< Quantum par. and derivatives

    if (nc /= 1) stop "calc_delta_Ac: Currently only for pure fluids"

    inv_rc = 1.0/r_c
    i = 1
    inv_sigma = 1.0/saftvrmie_param%sigma_ij(i,i)
    kc = 2.0*pi*N_AVOGADRO*saftvrmie_param%Cij(i,i)*&
         saftvrmie_param%sigma_ij_cube(i,i)*saftvrmie_param%eps_divk_ij(i,i)

    lamr = saftvrmie_param%lambda_r_ij(i,i)
    lama = saftvrmie_param%lambda_a_ij(i,i)
    L = inv_rc**(lamr-3)/(lamr-3) - inv_rc**(lama-3)/(lama-3)
    if (svrm_opt%quantum_correction > 0) then
       call get_DFeynHibbsPower(i,i,D,D_T,D_TT,saftvrmie_vc,power_in=1,divideBySigmaMie=.false.)
       Lq1 = (lamr*inv_rc**(lamr-1) - lama*inv_rc**(lama-1))*inv_sigma**2
    else
       Lq1 = 0.0
       D = 0.0
       D_T = 0.0
       D_TT = 0.0
    endif
    if (svrm_opt%quantum_correction > 1) then
       call get_DFeynHibbsPower(i,i,D2,D2_T,D2_TT,saftvrmie_vc,power_in=2,divideBySigmaMie=.false.)
       Q2a = saftvrmie_param%Quantum_const_2a_ij(i,i)
       Q2r = saftvrmie_param%Quantum_const_2r_ij(i,i)
       Lq2 = (Q2r*inv_rc**(lamr+1)/(lamr+1) &
            - Q2a*inv_rc**(lama+1)/(lama+1))*inv_sigma**4
    else
       Lq2 = 0.0
       D2 = 0.0
       D2_T = 0.0
       D2_TT = 0.0
    endif
    ! Initialize
    a = 0.0
    if (present(a_n)) then
       a_n = 0.0
    endif
    if (present(a_nn)) then
       a_nn = 0.0
    endif
    if (present(a_V)) then
       a_V = 0.0
    endif
    if (present(a_VV)) then
       a_VV = 0.0
    endif
    if (present(a_Vn)) then
       a_Vn = 0.0
    endif
    if (present(a_T)) then
       a_T = 0.0
    endif
    if (present(a_Tn)) then
       a_Tn = 0.0
    endif
    if (present(a_TV)) then
       a_TV = 0.0
    endif
    if (present(a_TT)) then
       a_TT = 0.0
    endif
    ! Calculate correction
    if (divide_by_n) then
      call calc_a_correction_div_n()
    else
      call calc_a_correction()
    endif
    if (svrm_opt%enable_shift_correction) then
       kc = kc/3.0
       L = inv_rc**(lamr-3) - inv_rc**(lama-3)
       if (svrm_opt%quantum_correction > 0) then
          Q1a = saftvrmie_param%Quantum_const_1a_ij(i,i)
          Q1r = saftvrmie_param%Quantum_const_1r_ij(i,i)
          Lq1 = (Q1r*inv_rc**(lamr-1) - Q1a*inv_rc**(lama-1))*inv_sigma**2
       endif
       if (svrm_opt%quantum_correction > 1) then
          Lq2 = (Q2r*inv_rc**(lamr+1) &
               - Q2a*inv_rc**(lama+1))*inv_sigma**4
        endif
        if (divide_by_n) then
          call calc_a_correction_div_n()
        else
          call calc_a_correction()
        endif
    endif
  contains
    subroutine calc_a_correction()
      real :: a_local, a_T_l
      a_local = kc*(n(i)**2/V)*(1/T)*(L+D*Lq1+D2*Lq2)
      a = a + a_local
      if (present(a_n)) then
         a_n = a_n + 2.0*a_local/n(i)
      endif
      if (present(a_nn)) then
         a_nn = a_nn + 2.0*a_local/n(i)**2
      endif
      if (present(a_V)) then
         a_V = a_V - a_local/V
      endif
      if (present(a_VV)) then
         a_VV = a_VV + 2.0*a_local/V**2
      endif
      if (present(a_Vn)) then
         a_Vn = a_Vn - 2.0*a_local/(V*n(i))
      endif
      if (present(a_T) .or. present(a_TV) .or. present(a_Tn) .or. present(a_TT)) then
         a_T_l = -a_local/T + kc*(n(i)**2/V)*(1/T)*(D_T*Lq1+D2_T*Lq2)
         if (present(a_T)) then
            a_T = a_T + a_T_l
         endif
      endif
      if (present(a_Tn)) then
         a_Tn = a_Tn + 2.0*a_T_l/n(i)
      endif
      if (present(a_TV)) then
         a_TV = a_TV - a_T_l/V
      endif
      if (present(a_TT)) then
         a_TT = a_TT + 2.0*a_local/T**2 - 2.0*kc*(n(i)**2/V)*(1/T**2)*(D_T*Lq1+D2_T*Lq2)&
              + kc*(n(i)**2/V)*(1/T)*(D_TT*Lq1+D2_TT*Lq2)
      endif
    end subroutine calc_a_correction
    subroutine calc_a_correction_div_n()
      real :: a_local, a_T_l
      a_local = kc*(1/V)*(1/T)*(L+D*Lq1+D2*Lq2)
      a = a + a_local*n(i)
      if (present(a_n)) then
         a_n = a_n + a_local
      endif
      if (present(a_V)) then
         a_V = a_V - a_local*n(i)/V
      endif
      if (present(a_VV)) then
         a_VV = a_VV + 2.0*a_local*n(i)/V**2
      endif
      if (present(a_Vn)) then
         a_Vn = a_Vn - a_local/V
      endif
      if (present(a_T) .or. present(a_TV) .or. present(a_Tn) .or. present(a_TT)) then
         a_T_l = -a_local*n(i)/T + kc*(n(i)/V)*(1/T)*(D_T*Lq1+D2_T*Lq2)
         if (present(a_T)) then
            a_T = a_T + a_T_l
         endif
      endif
      if (present(a_Tn)) then
         a_Tn = a_Tn + a_T_l/n(i)
      endif
      if (present(a_TV)) then
         a_TV = a_TV - a_T_l/V
      endif
      if (present(a_TT)) then
         a_TT = a_TT + 2.0*a_local*n(i)/T**2 - 2.0*kc*(n(i)/V)*(1/T**2)*(D_T*Lq1+D2_T*Lq2)&
              + kc*(n(i)/V)*(1/T)*(D_TT*Lq1+D2_TT*Lq2)
      endif
    end subroutine calc_a_correction_div_n
  end subroutine calc_delta_Ac

  !> Calculate truncation (and shifted) alpha
  !! \author Morten Hammer, June 2023
  subroutine calc_alpha_ts(i, j, s_vc, alpha_ij)
    use saftvrmie_hardsphere, only: calc_zero_for_shifted_potential
    ! Input
    integer, intent(in) :: i, j !< Interaction pair
    type(saftvrmie_var_container), intent(in) :: s_vc
    ! Output
    real, intent(out) :: alpha_ij  !< Truncated and shifted alpha
    ! Locals
    real :: lamr, lama, C
    real :: r_cut, u_cut, u_shift_divk
    real :: eps_eff, sigma_eff

    if (svrm_opt%quantum_correction > 0) &
         call stoperror("Alpha correction not yet implemented for quantum potentials")

    lamr = saftvrmie_param%lambda_r_ij(i,j)
    lama = saftvrmie_param%lambda_a_ij(i,j)
    C = saftvrmie_param%Cij(i,j)
    r_cut = svrm_opt%r_cut

    ! Get integration limits
    if (svrm_opt%enable_shift_correction) then
      u_cut = C*(1/r_cut**lama - 1/r_cut**lamr) ! Absolute value of u
      eps_eff = 1 - u_cut
      u_shift_divk = -u_cut*saftvrmie_param%eps_divk_ij(i,j)
      call calc_zero_for_shifted_potential(i,j,s_vc,saftvrmie_param%sigma_ij(i,j),u_shift_divk,sigma_eff)
      sigma_eff = sigma_eff/saftvrmie_param%sigma_ij(i,j)
    else
      sigma_eff = 1.0
      eps_eff = 1.0
    endif
    alpha_ij = C/sigma_eff**(lama-3)/(lama-3) - C/sigma_eff**(lamr-3)/(lamr-3) &
         + C/r_cut**(lamr-3)/(lamr-3) - C/r_cut**(lama-3)/(lama-3)
    if (svrm_opt%enable_shift_correction) then
      alpha_ij = alpha_ij - u_cut/3.0*(r_cut**3-sigma_eff**3)
    endif
    alpha_ij = alpha_ij/eps_eff/sigma_eff**3
  end subroutine calc_alpha_ts

end module saftvrmie_dispersion
