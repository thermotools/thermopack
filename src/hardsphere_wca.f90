!---------------------------------------------------------------------
! Module and subroutines for calculating Hard-Sphere contribution
! using perturbation theory and the WCA reference
! M. Hammer, 2019-2021
!---------------------------------------------------------------------
module hardsphere_wca
  use saftvrmie_containers, only: saftvrmie_zeta, saftvrmie_dhs
  implicit none
  private
  save
  real, parameter :: r_split = 2**(1.0/6.0)
  real, parameter :: eta_c = 0.52

  public :: calc_dhs_WCA, calcZetaX_vdW_no_segments
  public :: calc_cavity_integral_LJ_Fres
  !
  public :: cavity_distribution_function, lj_boltzmann_factor
  public :: minimum_r_cavity_int_LJ, calc_cavity_integral_LJ_Fres_test
  public :: calc_cavity_integral_LJ_Fres_test_TVN

contains

  !> Calculate WCA hard-sphere diameter
  !!
  !! \author Morten Hammer, 2020-11
  subroutine calc_dhs_WCA(nc,sigma,eps_divk,T,n,dhs)
    use saftvrmie_containers, only: saftvrmie_dhs
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: sigma, eps_divk
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: n(nc) !< Mol number (mol)
    ! Output
    type(saftvrmie_dhs), intent(inout) :: dhs !< The hard-sphere diameter
    ! Locals
    real :: t_star, d_star, denum, num_16, sqrt_t_star, prefac
    real :: d_star_t, d_star_tt
    !
    t_star = T/eps_divk
    sqrt_t_star = sqrt(t_star)
    denum = 1 + sqrt_t_star
    num_16 = denum**(-1.0/6.0)
    prefac = 2.0**(1.0/6.0)
    d_star = prefac*num_16
    d_star_t = -d_star/(12.0*denum*sqrt_t_star)
    d_star_tt = -d_star_t*(7.0/(12.0*denum*sqrt_t_star) + 1.0/(2.0*t_star))
    !
    dhs%d = sigma*d_star
    dhs%d_T = sigma*d_star_t/eps_divk
    dhs%d_TT = sigma*d_star_tt/eps_divk**2
  end subroutine calc_dhs_WCA

  !> Calculate cavity contribution
  !!
  !! \author Morten Hammer, Noember 2020
  subroutine calc_cavity_integral_LJ_Fres(nc,sigma,eps_divk,dhs,eta_hs,T,V,n,F,&
       F_T,F_V,F_n,F_TT,F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use quadratures
    use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta
    use saftvrmie_utils, only: convert_zeta_x_to_TVn
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    real, intent(in) :: sigma, eps_divk
    type(saftvrmie_dhs), intent(in) :: dhs !< The hard-sphere diameter
    type(saftvrmie_zeta), intent(in) :: eta_hs !< Hard sphere diameter packing fraction
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    real :: I1, I1_T, I1_TT, I1_e, I1_ee, I1_eT
    real :: I2, I2_T, I2_TT, I2_e, I2_ee, I2_eT
    integer :: cavity_quadrature = GAUSS_KRONROD_61 ! Modify to improve accuarcy
    integer :: i,n_quad
    real :: x_vec(max_n_quadrature), w_vec(max_n_quadrature)
    !real :: f_vec(max_n_quadrature), quad_error
    real :: r_split, d_star, d_star_T, d_star_TT, T_star
    real :: r_hs, r_hs_T, r_hs_TT
    real :: r, r_T, r_TT, r_diff_2, r_diff_2_T, r_diff_2_TT
    real :: r_min, r_min_T, r_min_TT
    real :: e_lj_wca, e_lj_wca_T, e_lj_wca_TT, eta, sumn
    real :: prefactor, prefactor_T, prefactor_TT
    real :: y_hs, y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT
    real :: Fn_e,Fn_ee,Fn_eT,Fn_xT,Fn_xTT
    real :: Fn,Fn_T,Fn_V,Fn_n(nc),Fn_TT,Fn_VV,Fn_TV,Fn_Tn(nc),Fn_Vn(nc),Fn_nn(nc,nc)
    ! Get quadrature points
    call get_quadrature_positions(cavity_quadrature,x_vec,n_quad)
    call get_quadrature_weights(cavity_quadrature,w_vec,n_quad)

    T_star = T/eps_divk
    d_star = dhs%d(1,1)/sigma
    d_star_t = dhs%d_T(1,1)/sigma
    d_star_tt = dhs%d_TT(1,1)/sigma
    r_split = 2**(1.0/6.0)
    call minimum_r_cavity_int_LJ(T, eps_divk, r_min, r_min_T, r_min_TT)
    sumn = sum(n)
    !eta_hs = pi*N_Avogadro*sumn*(d_star*sigma)**3/6/V
    !
    r_diff_2 = 0.5*(d_star - r_min)
    r_diff_2_T = 0.5*(d_star_T - r_min_T)
    r_diff_2_TT = 0.5*(d_star_TT - r_min_TT)

    I1 = 0
    I1_T = 0
    I1_TT = 0
    I1_e = 0
    I1_ee = 0
    I1_eT = 0
    ! Loop quadrature points
    do i = 1,n_quad
      r = r_diff_2*x_vec(i) + r_diff_2 + r_min
      r_T = r_diff_2_T*x_vec(i) + r_diff_2_T + r_min_T
      r_TT = r_diff_2_TT*x_vec(i) + r_diff_2_TT + r_min_TT
      prefactor = r_diff_2*w_vec(i)
      prefactor_T = r_diff_2_T*w_vec(i)
      prefactor_TT = r_diff_2_TT*w_vec(i)
      r_hs = r/d_star
      r_hs_T = r_T/d_star - d_star_T*r/d_star**2
      r_hs_TT = r_TT/d_star - 2*d_star_T*r_T/d_star**2 &
           - d_star_TT*r/d_star**2 + 2*d_star_T**2*r/d_star**3
      call cavity_distribution_function(eta_hs%zx, r_hs, r_hs_T, r_hs_TT, y_hs, &
           y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT)
      call lj_boltzmann_factor(r, r_T, r_TT, T, eps_divk, e_lj_wca, &
           e_lj_wca_T, e_lj_wca_TT)
      I1 = I1 + prefactor*y_hs*e_lj_wca*r**2
      I1_e = I1_e + prefactor*y_hs_e*e_lj_wca*r**2
      I1_ee = I1_ee + prefactor*y_hs_ee*e_lj_wca*r**2
      I1_T = I1_T + prefactor_T*y_hs*e_lj_wca*r**2 &
           + prefactor*y_hs_T*e_lj_wca*r**2 &
           + prefactor*y_hs*e_lj_wca_T*r**2 &
           + prefactor*y_hs*e_lj_wca*r*r_T*2
      I1_eT = I1_eT + prefactor_T*y_hs_e*e_lj_wca*r**2 &
           + prefactor*y_hs_eT*e_lj_wca*r**2 &
           + prefactor*y_hs_e*e_lj_wca_T*r**2 &
           + prefactor*y_hs_e*e_lj_wca*r*r_T*2
      I1_TT = I1_TT + prefactor_TT*y_hs*e_lj_wca*r**2 &
           + prefactor_T*y_hs_T*e_lj_wca*r**2 &
           + prefactor_T*y_hs*e_lj_wca_T*r**2 &
           + prefactor_T*y_hs*e_lj_wca*r*r_T*2 &
           + prefactor_T*y_hs_T*e_lj_wca*r**2 &
           + prefactor*y_hs_TT*e_lj_wca*r**2 &
           + prefactor*y_hs_T*e_lj_wca_T*r**2 &
           + prefactor*y_hs_T*e_lj_wca*r*r_T*2 &
           + prefactor_T*y_hs*e_lj_wca_T*r**2 &
           + prefactor*y_hs_T*e_lj_wca_T*r**2 &
           + prefactor*y_hs*e_lj_wca_TT*r**2 &
           + prefactor*y_hs*e_lj_wca_T*r*r_T*2 &
           + prefactor_T*y_hs*e_lj_wca*r*r_T*2 &
           + prefactor*y_hs_T*e_lj_wca*r*r_T*2 &
           + prefactor*y_hs*e_lj_wca_T*r*r_T*2 &
           + prefactor*y_hs*e_lj_wca*r_T**2*2 &
           + prefactor*y_hs*e_lj_wca*r*r_TT*2
    enddo

    !
    r_min = d_star
    r_min_T = d_star_T
    r_min_TT = d_star_TT
    r_diff_2 = 0.5*(r_split - r_min)
    r_diff_2_T = -0.5*r_min_T
    r_diff_2_TT = -0.5*r_min_TT

    I2 = 0
    I2_T = 0
    I2_TT = 0
    I2_e = 0
    I2_ee = 0
    I2_eT = 0
    ! Loop quadrature points
    do i = 1,n_quad
      r = r_diff_2*x_vec(i) + r_diff_2 + r_min
      r_T = r_diff_2_T*x_vec(i) + r_diff_2_T + r_min_T
      r_TT = r_diff_2_TT*x_vec(i) + r_diff_2_TT + r_min_TT

      prefactor = r_diff_2*w_vec(i)
      prefactor_T = r_diff_2_T*w_vec(i)
      prefactor_TT = r_diff_2_TT*w_vec(i)
      r_hs = r/d_star
      r_hs_T = r_T/d_star - d_star_T*r/d_star**2
      r_hs_TT = r_TT/d_star - 2*d_star_T*r_T/d_star**2 &
           - d_star_TT*r/d_star**2 + 2*d_star_T**2*r/d_star**3
      call cavity_distribution_function(eta_hs%zx, r_hs, r_hs_T, r_hs_TT, y_hs, &
           y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT)
      call lj_boltzmann_factor(r, r_T, r_TT, T, eps_divk, e_lj_wca, &
           e_lj_wca_T, e_lj_wca_TT)
      I2 = I2 + prefactor*y_hs*(e_lj_wca - 1)*r**2
      I2_e = I2_e + prefactor*y_hs_e*(e_lj_wca - 1)*r**2
      I2_ee = I2_ee + prefactor*y_hs_ee*(e_lj_wca - 1)*r**2
      I2_T = I2_T + prefactor_T*y_hs*(e_lj_wca - 1)*r**2 &
           + prefactor*y_hs_T*(e_lj_wca - 1)*r**2 &
           + prefactor*y_hs*e_lj_wca_T*r**2 &
           + prefactor*y_hs*(e_lj_wca - 1)*r*r_T*2
      I2_eT = I2_eT + prefactor_T*y_hs_e*(e_lj_wca - 1)*r**2 &
           + prefactor*y_hs_eT*(e_lj_wca - 1)*r**2 &
           + prefactor*y_hs_e*e_lj_wca_T*r**2 &
           + prefactor*y_hs_e*(e_lj_wca - 1)*r*r_T*2
      I2_TT = I2_TT + prefactor_TT*y_hs*(e_lj_wca - 1)*r**2 &
           + prefactor_T*y_hs_T*(e_lj_wca - 1)*r**2 &
           + prefactor_T*y_hs*e_lj_wca_T*r**2 &
           + prefactor_T*y_hs*(e_lj_wca - 1)*r*r_T*2 &
           + prefactor_T*y_hs_T*(e_lj_wca - 1)*r**2 &
           + prefactor*y_hs_TT*(e_lj_wca - 1)*r**2 &
           + prefactor*y_hs_T*e_lj_wca_T*r**2 &
           + prefactor*y_hs_T*(e_lj_wca - 1)*r*r_T*2 &
           + prefactor_T*y_hs*e_lj_wca_T*r**2 &
           + prefactor*y_hs_T*e_lj_wca_T*r**2 &
           + prefactor*y_hs*e_lj_wca_TT*r**2 &
           + prefactor*y_hs*e_lj_wca_T*r*r_T*2 &
           + prefactor_T*y_hs*(e_lj_wca - 1)*r*r_T*2 &
           + prefactor*y_hs_T*(e_lj_wca - 1)*r*r_T*2 &
           + prefactor*y_hs*e_lj_wca_T*r*r_T*2 &
           + prefactor*y_hs*(e_lj_wca - 1)*r_T**2*2 &
           + prefactor*y_hs*(e_lj_wca - 1)*r*r_TT*2
    enddo

    ! ! ! ! TESTING
    ! I2 = 0
    ! I2_T = 0
    ! I2_TT = 0
    ! I2_e = 0
    ! I2_ee = 0
    ! I2_eT = 0


    eta = eta_hs%zx
    Fn = -12*eta*(I1 + I2)/d_star**3
    ! Get overall differentials in eta and T
    Fn_e = -12*((I1 + I2) + eta*(I1_e + I2_e))/d_star**3
    Fn_ee = -12*(2*(I1_e + I2_e) + eta*(I1_ee + I2_ee))/d_star**3
    Fn_eT = -12*((I1_T + I2_T) + eta*(I1_eT + I2_eT))/d_star**3 &
         +12*((I1 + I2) + eta*(I1_e + I2_e))*3*d_star_t/d_star**4
    Fn_xT = -12*eta*((I1_T + I2_T) - (I1 + I2)*3*d_star_t/d_star)/d_star**3
    Fn_xTT = -12*eta*(I1_TT + I2_TT)/d_star**3 &
         +12*2*eta*(I1_T + I2_T)*3*d_star_t/d_star**4 &
         +12*eta*(I1 + I2)*3*d_star_tt/d_star**4 &
         -12*eta*(I1 + I2)*3*4*d_star_t**2/d_star**5
    !
    call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,eta_hs,&
         Fn,Fn_e,Fn_xT,Fn_ee,Fn_xTT,Fn_eT,0.0,0.0,0.0,&
         Fn_T,Fn_V,Fn_n,Fn_TT,Fn_VV,Fn_TV,Fn_Tn,Fn_Vn,Fn_nn,&
         difflevel=2)
    ! Get Fn*n
    Fn_nn(1,1) = n(1)*Fn_nn(1,1) + 2*Fn_n(1)
    Fn_n = n(1)*Fn_n + Fn
    Fn_Tn = n(1)*Fn_Tn + Fn_T
    Fn_T = n(1)*Fn_T
    Fn_TT = n(1)*Fn_TT
    Fn_Vn = n(1)*Fn_Vn + Fn_V
    Fn_V = n(1)*Fn_V
    Fn_VV = n(1)*Fn_VV
    Fn_TV = n(1)*Fn_TV

    F = n(1)*Fn
    if (present(F_T)) then
      F_T = Fn_T
    endif
    if (present(F_V)) then
      F_V = Fn_V
    endif
    if (present(F_TT)) then
      F_TT = Fn_TT
    endif
    if (present(F_VV)) then
      F_VV = Fn_VV
    endif
    if (present(F_TV)) then
      F_TV = Fn_TV
    endif
    if (present(F_Tn)) then
      F_Tn = Fn_Tn
    endif
    if (present(F_Vn)) then
      F_Vn = Fn_Vn
    endif
    if (present(F_n)) then
      F_n = Fn_n
    endif
    if (present(F_nn)) then
      F_nn = Fn_nn
    endif
    ! ! ! ! TESTING
    ! F = n(1)*Fn
    ! if (present(F_T)) then
    !   F_T = Fn_xT
    ! endif
    ! if (present(F_V)) then
    !   F_V = Fn_e
    ! endif
    ! if (present(F_TT)) then
    !   F_TT = Fn_xTT
    ! endif
    ! if (present(F_VV)) then
    !   F_VV = Fn_ee
    ! endif
    ! if (present(F_TV)) then
    !   F_TV = Fn_eT
    ! endif
  end subroutine calc_cavity_integral_LJ_Fres

  !> Test function
  !!
  !! \author Morten Hammer, November 2020
  subroutine calc_cavity_integral_LJ_Fres_test(nc,sigma,eps_divk,T,eta,F,&
       F_T,F_e,F_TT,F_ee,F_Te)
    use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta, &
         allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs, &
         allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: eta !<
    real, intent(in) :: sigma, eps_divk
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_e,F_TT,F_ee,F_Te
    ! Locals
    type(saftvrmie_dhs) :: dhs !< The hard-sphere diameter
    type(saftvrmie_zeta) :: eta_hs !< Hard sphere diameter packing fraction
    real :: V !< Volume [m3]
    real :: n(nc) !< Mol numbers [mol]
    V = 0
    n = 1
    call allocate_saftvrmie_dhs(nc,dhs)
    call allocate_saftvrmie_zeta(nc,eta_hs)
    ! The hard-sphere diameter
    call calc_dhs_WCA(nc,sigma,eps_divk,T,n,dhs)
    ! Packing fraction
    !call calcZetaX_vdW_no_segments(nc,T,V,n,dhs,eta_hs)
    eta_hs%zx = eta

    call calc_cavity_integral_LJ_Fres(nc,sigma,eps_divk,dhs,eta_hs,T,V,n,F,&
         F_T=F_T,F_V=F_e,F_TT=F_TT,F_VV=F_ee,F_TV=F_Te)
    call cleanup_saftvrmie_dhs(dhs)
    call cleanup_saftvrmie_zeta(eta_hs)
  end subroutine calc_cavity_integral_LJ_Fres_test

  !> Test function
  !!
  !! \author Morten Hammer, November 2020
  subroutine calc_cavity_integral_LJ_Fres_test_TVN(nc,sigma,eps_divk,T,V,n,F,&
       F_T,F_V,F_n,F_TT,F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta, &
         allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs, &
         allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    real, intent(in) :: sigma, eps_divk
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    type(saftvrmie_dhs) :: dhs !< The hard-sphere diameter
    type(saftvrmie_zeta) :: eta_hs !< Hard sphere diameter packing fraction
    call allocate_saftvrmie_dhs(nc,dhs)
    call allocate_saftvrmie_zeta(nc,eta_hs)
    ! The hard-sphere diameter
    call calc_dhs_WCA(nc,sigma,eps_divk,T,n,dhs)
    ! Packing fraction
    call calcZetaX_vdW_no_segments(nc,T,V,n,dhs,eta_hs)

    call calc_cavity_integral_LJ_Fres(nc,sigma,eps_divk,dhs,eta_hs,T,V,n,F,&
         F_T,F_V,F_n,F_TT,F_VV,F_TV,F_Tn,F_Vn,F_nn)
    call cleanup_saftvrmie_dhs(dhs)
    call cleanup_saftvrmie_zeta(eta_hs)
  end subroutine calc_cavity_integral_LJ_Fres_test_TVN

  ! Minimum radius for cavity function integration
  subroutine minimum_r_cavity_int_LJ(T, eps_divk, r, r_T, r_TT)
    use numconstants, only: expMin
    real, intent(in) :: T
    real, intent(in) :: eps_divk
    real, intent(out) :: r, r_T, r_TT! r/sigma_hs
    !
    real :: denum, sqr, C
    ! -beta*u = expMin
    ! 1/r**12 - 1/r**6 = -expMin/(4*beta_star)
    ! x**2 - x + expMin/(4*beta_star) = 0
    ! x = (1 + sqrt(1 - expMin/beta_star))/2
    C = expMin*eps_divk
    sqr = sqrt(1.0-C/T)
    denum = 1+sqr
    r = (2/denum)**(1.0/6.0)
    r_T = -(1.0/12.0)*r/denum/sqr*C/T**2
    r_TT = -(7.0/12.0)*r_T/denum/sqr*C/T**2 - r_T/2/sqr**2*C/T**2 &
         -2*r_T/T
  end subroutine minimum_r_cavity_int_LJ

  ! Cavity-correlation function of hard spheres
  subroutine cavity_distribution_function(eta, r_star, r_star_T, r_star_TT, y_hs, &
       y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT)
    real, intent(in) :: eta ! Hard sphere diameter packing fraction
    real, intent(in) :: r_star, r_star_T, r_star_TT! r/sigma_hs
    real, intent(out) :: y_hs !
    real, intent(out) :: y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT
    !
    real :: denum, A, B, C, ln_y_hs
    real :: A_e, A_ee, B_e, B_ee, C_e, C_ee
    real :: y_hs_r, y_hs_rr, y_hs_er, y_hs_c, y_hs_e_c, y_hs_ee_c
    real :: y_hs_r_c, y_hs_er_c, y_hs_eer_c
    real :: y_hs_rr_c, y_hs_err_c, y_hs_eerr_c

    if (eta <= eta_c) then
      call cavity_parameters(eta,A,B,C,A_e,B_e,C_e,A_ee,B_ee,C_ee)
      ln_y_hs = A + B*r_star + C*r_star**3
      y_hs = exp(ln_y_hs)
      !
      y_hs_e = y_hs*(A_e + B_e*r_star + C_e*r_star**3)
      y_hs_ee = y_hs*((A_e + B_e*r_star + C_e*r_star**3)**2 + &
           (A_ee + B_ee*r_star + C_ee*r_star**3))
      y_hs_er = y_hs*((A_e + B_e*r_star + C_e*r_star**3)*(B + 3*C*r_star**2) + &
           (B_e + 3*C_e*r_star**2))
      y_hs_r = y_hs*(B + 3*C*r_star**2)
      y_hs_rr = y_hs*((B + 3*C*r_star**2)**2 + 6*C*r_star)
    else
      call cavity_parameters(eta_c,A,B,C,A_e,B_e,C_e,A_ee,B_ee,C_ee)
      y_hs_c = exp(A + B*r_star + C*r_star**3)
      y_hs_e_c = y_hs*(A_e + B_e*r_star + C_e*r_star**3)
      y_hs_ee_c = y_hs*((A_e + B_e*r_star + C_e*r_star**3)**2 + &
           (A_ee + B_ee*r_star + C_ee*r_star**3))
      y_hs_r_c = y_hs*(B + 3*C*r_star**2)
      y_hs_rr_c = y_hs*((B + 3*C*r_star**2)**2 + 6*C*r_star)
      y_hs_er_c = y_hs*((A_e + B_e*r_star + C_e*r_star**3)*(B + 3*C*r_star**2) + &
           (B_e + 3*C_e*r_star**2))
      y_hs_err_c = y_hs*(&
           2*(B + 3*C*r_star**2)*(B_e + 3*C_e*r_star**2) + &
           (A_e + B_e*r_star + C_e*r_star**3)*(6*C*r_star + (B + 3*C*r_star**2)**2) + &
           6*C_e*r_star)
      y_hs_eer_c = y_hs_ee_c*(B + 3*C*r_star**2) + y_hs*(&
           2*(A_e + B_e*r_star + C_e*r_star**3)*(B_e + 3*C_e*r_star**2) + &
           (B_ee + 3*C_ee*r_star**2))
      y_hs_eerr_c = y_hs_eer_c*(B + 3*C*r_star**2) + y_hs*(&
             (A_e + B_e*r_star + C_e*r_star**3)**2*6*C*r_star + &
             2*(B + 3*C*r_star**2)*(A_e + B_e*r_star + C_e*r_star**3)*(B_e + 3*C_e*r_star**2) + &
            6*C*r_star*(A_ee + B_ee*r_star + C_ee*r_star**3) + &
            (B + 3*C*r_star**2)*(B_ee + 3*C_ee*r_star**2) + &
            2*(B_e + 3*C_e*r_star**2)**2 + &
            2*(A_e + B_e*r_star + C_e*r_star**3)*6*C_e*r_star + &
            6*C_ee*r_star)
      y_hs = y_hs_c + y_hs_e_c*(eta-eta_c) + 0.5*y_hs_ee_c*(eta-eta_c)**2
      y_hs_e = y_hs_e_c + y_hs_ee_c*(eta-eta_c)
      y_hs_ee = y_hs_ee_c
      y_hs_er = y_hs_er_c + y_hs_eer_c*(eta-eta_c)
      y_hs_r = y_hs_r_c + y_hs_er_c*(eta-eta_c) + 0.5*y_hs_eer_c*(eta-eta_c)**2
      y_hs_rr = y_hs_rr_c + y_hs_err_c*(eta-eta_c) + 0.5*y_hs_eerr_c*(eta-eta_c)**2
    endif
    !
    y_hs_eT = y_hs_er*r_star_T
    y_hs_T = y_hs_r*r_star_T
    y_hs_TT = y_hs_rr*r_star_T**2 + y_hs_r*r_star_TT

  contains
    subroutine cavity_parameters(eta,A,B,C,A_e,B_e,C_e,A_ee,B_ee,C_ee)
      real, intent(in) :: eta
      real, intent(out) :: A,B,C,A_e,B_e,C_e,A_ee,B_ee,C_ee
      denum = (1-eta)**3
      A = (3.0 - eta)/denum - 3.0
      B = -3.0*eta*(2.0 - eta)/denum
      C = log((2.0 - eta)/denum/2.0) - eta*(2.0 - 6*eta + 3*eta**2)/denum
      ln_y_hs = A + B*r_star + C*r_star**3
      y_hs = exp(ln_y_hs)
      !
      denum = (1-eta)*denum
      A_e = -(2*(eta - 4))/denum
      B_e = (3*(eta**2 - 2*eta - 2))/denum
      C_e = -((2*(eta - 4)*eta + 7)*eta**2 + eta + 1)/((eta - 2)*denum)
      denum = (1-eta)*denum
      A_ee = -(6*(eta - 5))/denum
      B_ee = (6*(eta**2 - 2*eta - 5))/denum
      C_ee = -(2*eta**5 - 10*eta**4 + 5*eta**3 + 31*eta**2 - 29*eta - 11)/&
           ((eta - 2)**2*denum)
    end subroutine cavity_parameters
  end subroutine cavity_distribution_function

  ! LJ boltzmann factor
  subroutine lj_boltzmann_factor(r, r_T, r_TT, T, eps_divk, &
       e_lj_wca, e_lj_wca_T, e_lj_wca_TT)
    real, intent(in) :: T !
    real, intent(in) :: eps_divk !
    real, intent(in) :: r, r_T, r_TT ! r/sigma
    real, intent(out) :: e_lj_wca, e_lj_wca_T, e_lj_wca_TT !
    !
    real :: u, u_r, u_rr, ir6, ir12, ir
    if (r > r_split) then
      u = 0
    else
      ir = 1/r
      ir6 = ir**6
      ir12 = ir6*ir6
      u = 4*(ir12 - ir6) + 1.0
      u_r = -4*(12*ir12 - 6*ir6)*ir
      u_rr = 4*(12*13*ir12 - 6*7*ir6)*ir*ir
    endif
    e_lj_wca = exp(-u*eps_divk/T)
    e_lj_wca_T = -e_lj_wca*(u_r*r_T*eps_divk/T - u*eps_divk/T**2)
    e_lj_wca_TT = e_lj_wca*((u_r*r_T*eps_divk/T - u*eps_divk/T**2)**2 -&
         (u_rr*r_T**2*eps_divk/T + u_r*r_TT*eps_divk/T - &
         2*u_r*r_T*eps_divk/T**2 + 2*u*eps_divk/T**3))
  end subroutine lj_boltzmann_factor

  !> Calculate hypotetical pure fluid packing fraction
  !!
  !! \author Morten Hammer, November 2020
  subroutine calcZetaX_vdW_no_segments(nc,T,V,n,dhs,zeta)
    use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta
    use numconstants, only: pi
    use thermopack_constants, only: N_AVOGADRO
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    type(saftvrmie_dhs), intent(in) :: dhs
    type(saftvrmie_zeta), intent(inout) :: zeta
    ! Locals
    integer :: i,k,l
    real :: prefactor, ns
    ns = sum(n)
    prefactor = pi*N_AVOGADRO/(6.0*V*ns)
    zeta%zx = 0.0
    do i=1,nc
      zeta%zx = zeta%zx + n(i)*sum(n*dhs%d(:,i)**3)
    enddo
    zeta%zx = prefactor*zeta%zx
    zeta%zx_T = 0.0
    do i=1,nc
      zeta%zx_T = zeta%zx_T + n(i)*sum(n*dhs%d(:,i)**2*dhs%d_T(:,i))
    enddo
    zeta%zx_T = 3.0*prefactor*zeta%zx_T
    zeta%zx_V = -zeta%zx/V
    do k=1,nc
      ! Assuming d_kl = d_lk
      zeta%zx_n(k) = (2.0*prefactor*sum(n*dhs%d(:,k)**3) - zeta%zx/ns)
    enddo
    zeta%zx_TT = 0.0
    do i=1,nc
      zeta%zx_TT = zeta%zx_TT + prefactor*n(i)*(&
           6.0*sum(n*dhs%d(:,i)*dhs%d_T(:,i)**2) &
           + 3.0*sum(n*dhs%d(:,i)**2*dhs%d_TT(:,i)))
    enddo
    zeta%zx_VV = 2.0*zeta%zx/V**2
    zeta%zx_TV = -zeta%zx_T/V
    zeta%zx_Vn = -zeta%zx_n/V
    do k=1,nc
      ! Assuming d_kl = d_lk
      zeta%zx_Tn(k) = (6.0*prefactor*sum(n*&
           dhs%d(:,k)**2*dhs%d_T(:,k)) &
           - zeta%zx_T/ns)
    enddo
    do k=1,nc
      do l=1,nc
        ! Assuming d_kl = d_lk
        zeta%zx_nn(k,l) = 2.0*prefactor*dhs%d(k,l)**3&
             -(zeta%zx_n(l) + zeta%zx_n(k))/ns
      enddo
    enddo
    zeta%zx_VVV = -6.0*zeta%zx/V**3
    zeta%zx_VVT = 2.0*zeta%zx_T/V**2
    zeta%zx_VTn = -zeta%zx_Tn/V
    zeta%zx_VVn = 2.0*zeta%zx_n/V**2
    zeta%zx_Vnn = -zeta%zx_nn/V
    zeta%zx_VTT = -zeta%zx_TT/V
  end subroutine calcZetaX_vdW_no_segments

end module hardsphere_wca

subroutine testing_WCA_lj()
  use hardsphere_wca
  use saftvrmie_containers, only: saftvrmie_dhs, saftvrmie_zeta, &
       allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs, &
       allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta
  implicit none
  real :: n(1),n0(1)
  real :: eps, T, V, sigma, eps_divk, d
  type(saftvrmie_zeta) :: zeta, zeta1, zeta2
  type(saftvrmie_dhs) :: dhs, dhs1, dhs2
  real :: T0
  real :: y_hs, y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT
  real :: y_hs1, y_hs_e1, y_hs_ee1, y_hs_T1, y_hs_TT1, y_hs_eT1
  real :: y_hs2, y_hs_e2, y_hs_ee2, y_hs_T2, y_hs_TT2, y_hs_eT2
  real :: eta, e_lj_wca, e_lj_wca_T, e_lj_wca_TT
  real :: e_lj_wca1, e_lj_wca1_T, e_lj_wca1_TT
  real :: e_lj_wca2, e_lj_wca2_T, e_lj_wca2_TT
  real :: d_star, d_star_t, d_star_tt, r, r_T, r_TT, r_hs, r_hs_T, r_hs_TT
  real :: r1, r2, r1_T, r2_T, r1_TT, r2_TT
  real, parameter :: sigma_param = 2.801e-10, eps_divk_param = 33.921

  sigma = sigma_param
  eps_divk = eps_divk_param
  n = (/1.2/)
  n0 = n
  V = 5.0e-5*sum(n)
  T = 45.0
  d = 1.0e-5

  call allocate_saftvrmie_dhs(1,dhs)
  call allocate_saftvrmie_dhs(1,dhs1)
  call allocate_saftvrmie_dhs(1,dhs2)
  T = 45.0
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs)
  eps = T*d
  call calc_dhs_WCA(1,sigma,eps_divk,T-eps,n,dhs1)
  call calc_dhs_WCA(1,sigma,eps_divk,T+eps,n,dhs2)
  print *,"d",dhs%d
  print *,"d_T",dhs%d_T,(dhs2%d-dhs1%d)/(2*eps)
  print *,"d_TT",dhs%d_TT,(dhs2%d_T-dhs1%d_T)/(2*eps)

  call allocate_saftvrmie_zeta(1,zeta)
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs)
  call calcZetaX_vdW_no_segments(1,T,V,n,dhs,zeta)
  call allocate_saftvrmie_zeta(1,zeta1)
  call allocate_saftvrmie_zeta(1,zeta2)
  !
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs1)
  call calcZetaX_vdW_no_segments(1,T,V-V*eps,n,dhs1,zeta1)
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs2)
  call calcZetaX_vdW_no_segments(1,T,V+V*eps,n,dhs2,zeta2)
  print *,"Testing hypotetical pure fluid packing fraction"
  print *,"Volume"
  print *,zeta%zx
  print *,zeta%zx_V,(zeta2%zx - zeta1%zx)/(2*V*eps)
  print *,zeta%zx_VV,(zeta2%zx_V - zeta1%zx_V)/(2*V*eps)
  print *,zeta%zx_TV,(zeta2%zx_T - zeta1%zx_T)/(2*V*eps)
  print *,zeta%zx_Vn,(zeta2%zx_n - zeta1%zx_n)/(2*V*eps)
  print *,zeta%zx_VVV,(zeta2%zx_VV - zeta1%zx_VV)/(2*V*eps)
  print *,zeta%zx_VVT,(zeta2%zx_TV - zeta1%zx_TV)/(2*V*eps)
  print *,zeta%zx_VTT,(zeta2%zx_TT - zeta1%zx_TT)/(2*V*eps)
  print *,zeta%zx_VTn,(zeta2%zx_Tn - zeta1%zx_Tn)/(2*V*eps)
  print *,zeta%zx_VVn,(zeta2%zx_Vn - zeta1%zx_Vn)/(2*V*eps)
  print *,zeta%zx_Vnn(1,:),(zeta2%zx_nn(1,:) - zeta1%zx_nn(1,:))/(2*V*eps)

  n(1) = n0(1) - eps
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs1)
  call calcZetaX_vdW_no_segments(1,T,V,n,dhs1,zeta1)
  n(1) = n0(1) + eps
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs2)
  call calcZetaX_vdW_no_segments(1,T,V,n,dhs2,zeta2)
  print *,"n(1)"
  print *,zeta%zx_n(1),(zeta2%zx - zeta1%zx)/(2*eps)
  print *,zeta%zx_Tn(1),(zeta2%zx_T - zeta1%zx_T)/(2*eps)
  print *,zeta%zx_Vn(1),(zeta2%zx_V - zeta1%zx_V)/(2*eps)
  print *,zeta%zx_nn(1,:),(zeta2%zx_n - zeta1%zx_n)/(2*eps)
  print *,zeta%zx_VVn(1),(zeta2%zx_VV - zeta1%zx_VV)/(2*eps)
  print *,zeta%zx_Vnn(1,:),(zeta2%zx_Vn - zeta1%zx_Vn)/(2*eps)
  print *,zeta%zx_VTn(1),(zeta2%zx_TV - zeta1%zx_TV)/(2*eps)

  n = n0
  eps = T*1.0e-5
  call calc_dhs_WCA(1,sigma,eps_divk,T-eps,n,dhs1)
  call calcZetaX_vdW_no_segments(1,T-eps,V,n,dhs1,zeta1)
  call calc_dhs_WCA(1,sigma,eps_divk,T+eps,n,dhs2)
  call calcZetaX_vdW_no_segments(1,T+eps,V,n,dhs2,zeta2)
  print *,"Temperature"
  print *,zeta%zx_T,(zeta2%zx - zeta1%zx)/(2*eps)
  print *,zeta%zx_TT,(zeta2%zx_T - zeta1%zx_T)/(2*eps)
  print *,zeta%zx_TV,(zeta2%zx_V - zeta1%zx_V)/(2*eps)
  print *,zeta%zx_Tn,(zeta2%zx_n - zeta1%zx_n)/(2*eps)
  print *,zeta%zx_VVT,(zeta2%zx_VV - zeta1%zx_VV)/(2*eps)
  print *,zeta%zx_VTT,(zeta2%zx_TV - zeta1%zx_TV)/(2*eps)
  print *,zeta%zx_VTn,(zeta2%zx_Vn - zeta1%zx_Vn)/(2*eps)

  !
  print *,"Cavity function"
  eta = 0.53
  T = eps_divk*1.5
  T0 = T
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs)
  d_star = dhs%d(1,1)/sigma
  d_star_t = dhs%d_T(1,1)/sigma
  d_star_tt = dhs%d_TT(1,1)/sigma
  r = 0.8*(T-eps_divk*1.5) + 1.0e-3*T**2
  r_T = 0.8 + 2.0e-3*T
  r_TT = 2.0e-3
  r_hs = r/d_star
  r_hs_T = r_T/d_star - d_star_T*r/d_star**2
  r_hs_TT = r_TT/d_star - 2*d_star_T*r_T/d_star**2 &
       - d_star_TT*r/d_star**2 + 2*d_star_T**2*r/d_star**3

  call cavity_distribution_function(eta, r_hs, r_hs_T, r_hs_TT, y_hs, &
       y_hs_e, y_hs_ee, y_hs_T, y_hs_TT, y_hs_eT)
  call cavity_distribution_function(eta-d, r_hs, r_hs_T, r_hs_TT, y_hs1, &
       y_hs_e1, y_hs_ee1, y_hs_T1, y_hs_TT1, y_hs_eT1)
  call cavity_distribution_function(eta+d, r_hs, r_hs_T, r_hs_TT, y_hs2, &
       y_hs_e2, y_hs_ee2, y_hs_T2, y_hs_TT2, y_hs_eT2)
  print *,y_hs
  print *,y_hs_e,(y_hs2-y_hs1)/(2*d)
  print *,y_hs_ee,(y_hs_e2-y_hs_e1)/(2*d), (y_hs2-2*y_hs+y_hs1)/d**2
  print *,y_hs_eT,(y_hs_T2-y_hs_T1)/(2*d)

  eps = d*T
  T = T0 - eps
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs)
  d_star = dhs%d(1,1)/sigma
  d_star_t = dhs%d_T(1,1)/sigma
  d_star_tt = dhs%d_TT(1,1)/sigma
  r = 0.8*(T-eps_divk*1.5) + 1.0e-3*T**2
  r_T = 0.8 + 2.0e-3*T
  r_TT = 2.0e-3
  r_hs = r/d_star
  r_hs_T = r_T/d_star - d_star_T*r/d_star**2
  r_hs_TT = r_TT/d_star - 2*d_star_T*r_T/d_star**2 &
       - d_star_TT*r/d_star**2 + 2*d_star_T**2*r/d_star**3
  call cavity_distribution_function(eta, r_hs, r_hs_T, r_hs_TT, y_hs1, &
       y_hs_e1, y_hs_ee1, y_hs_T1, y_hs_TT1, y_hs_eT1)
  T = T0 + eps
  call calc_dhs_WCA(1,sigma,eps_divk,T,n,dhs)
  d_star = dhs%d(1,1)/sigma
  d_star_t = dhs%d_T(1,1)/sigma
  d_star_tt = dhs%d_TT(1,1)/sigma
  r = 0.8*(T-eps_divk*1.5) + 1.0e-3*T**2
  r_T = 0.8 + 2.0e-3*T
  r_TT = 2.0e-3
  r_hs = r/d_star
  r_hs_T = r_T/d_star - d_star_T*r/d_star**2
  r_hs_TT = r_TT/d_star - 2*d_star_T*r_T/d_star**2 &
       - d_star_TT*r/d_star**2 + 2*d_star_T**2*r/d_star**3

  call cavity_distribution_function(eta, r_hs, r_hs_T, r_hs_TT, y_hs2, &
       y_hs_e2, y_hs_ee2, y_hs_T2, y_hs_TT2, y_hs_eT2)
  print *,y_hs_T,(y_hs2-y_hs1)/(2*eps)
  print *,y_hs_TT,(y_hs_T2-y_hs_T1)/(2*eps), (y_hs2-2*y_hs+y_hs1)/eps**2
  print *,y_hs_eT,(y_hs_e2-y_hs_e1)/(2*eps)

  print *,"e0 function"
  T = eps_divk*1.5
  T0 = T
  r = 0.8 + 0.8*(T-eps_divk*1.5) + 1.0e-4*T**2
  print *,"r",r
  r_T = 0.8 + 2.0e-4*T
  r_TT = 2.0e-4
  call lj_boltzmann_factor(r, r_T, r_TT, T, eps_divk, &
       e_lj_wca, e_lj_wca_T, e_lj_wca_TT)

  eps = T0*d
  T = T0 - eps
  r = 0.8 + 0.8*(T-eps_divk*1.5) + 1.0e-4*T**2
  print *,"r",r
  r_T = 0.8 + 2.0e-4*T
  r_TT = 2.0e-4
  call lj_boltzmann_factor(r, r_T, r_TT, T, eps_divk, &
       e_lj_wca1, e_lj_wca1_T, e_lj_wca1_TT)
  T = T0 + eps
  r = 0.8 + 0.8*(T-eps_divk*1.5) + 1.0e-4*T**2
  r_T = 0.8 + 2.0e-4*T
  r_TT = 2.0e-4
  call lj_boltzmann_factor(r, r_T, r_TT, T, eps_divk, &
       e_lj_wca2, e_lj_wca2_T, e_lj_wca2_TT)

  print *,e_lj_wca_T,(e_lj_wca2-e_lj_wca1)/(2*eps)
  print *,e_lj_wca_TT,(e_lj_wca2_T-e_lj_wca1_T)/(2*eps), (e_lj_wca2-2*e_lj_wca+e_lj_wca1)/eps**2

  print *,"Minimum r"
  call minimum_r_cavity_int_LJ(T, eps_divk, r, r_T, r_TT)
  call minimum_r_cavity_int_LJ(T-eps, eps_divk, r1, r1_T, r1_TT)
  call minimum_r_cavity_int_LJ(T+eps, eps_divk, r2, r2_T, r2_TT)
  print *,r
  print *,r_T,(r2-r1)/(2*eps)
  print *,r_TT,(r2_T-r1_T)/(2*eps), (r2-2*r+r1)/eps**2
  stop
end subroutine testing_WCA_lj

subroutine test_uf_LJ_cavity_Fres()
  use saftvrmie_options
  use hardsphere_wca
  implicit none
  ! Locals
  integer, parameter :: nc = 1
  real :: sigma, eps_divk
  real :: T,eps,eta
  real :: F,F_T,F_e,F_TT,F_ee,F_Te
  real :: Fp,Fp_T,Fp_e,Fp_TT,Fp_ee,Fp_Te
  real :: Fm,Fm_T,Fm_e,Fm_TT,Fm_ee,Fm_Te
  ! n = (/1.2/)
  ! n0 = n
  ! V = 1.0e-4
  ! T = 30.0
  sigma = 3.5e-10
  eps_divk = 30.0
  eps = 1.0e-5

  T=39.0
  eta = 0.3


  call calc_cavity_integral_LJ_Fres_test(nc,sigma,eps_divk,T,eta,F,&
       F_T,F_e,F_TT,F_ee,F_Te)
  call calc_cavity_integral_LJ_Fres_test(nc,sigma,eps_divk,T,eta-eps,Fm,&
       Fm_T,Fm_e,Fm_TT,Fm_ee,Fm_Te)
  call calc_cavity_integral_LJ_Fres_test(nc,sigma,eps_divk,T,eta+eps,Fp,&
       Fp_T,Fp_e,Fp_TT,Fp_ee,Fp_Te)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"eta"
  print *,F
  print *,F_e,(Fp - Fm)/(2*eps)
  print *,F_ee,(Fp_e - Fm_e)/(2*eps)
  print *,F_Te,(Fp_T - Fm_T)/(2*eps)

  print *,"T"
  call calc_cavity_integral_LJ_Fres_test(nc,sigma,eps_divk,T-T*eps,eta,Fm,&
       Fm_T,Fm_e,Fm_TT,Fm_ee,Fm_Te)
  call calc_cavity_integral_LJ_Fres_test(nc,sigma,eps_divk,T+T*eps,eta,Fp,&
       Fp_T,Fp_e,Fp_TT,Fp_ee,Fp_Te)

  print *,F_T,(Fp - Fm)/(2*T*eps)
  print *,F_TT,(Fp_T - Fm_T)/(2*T*eps)
  print *,F_Te,(Fp_e - Fm_e)/(2*T*eps)

  stop
end subroutine test_uf_LJ_cavity_Fres


subroutine test_uf_LJ_cavity_Fres_TVN()
  use saftvrmie_options
  use hardsphere_wca
  implicit none
  ! Locals
  real :: sigma, eps_divk
  real :: n(1),n0(1),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(1) :: F_n,F_Tn,F_Vn
  real, dimension(1,1) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(1) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(1,1) :: Fp_nn
  real :: Fm,Fm_T,Fm_V,Fm_TT,Fm_VV,Fm_TV
  real, dimension(1) :: Fm_n,Fm_Tn,Fm_Vn
  real, dimension(1,1) :: Fm_nn
  n = (/1.2/)
  n0 = n
  V = 1.0e-4
  T = 39.0
  sigma = 3.5e-10
  eps_divk = 30.0
  eps = 1.0e-5

  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T,V-V*eps,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - Fm)/(2*V*eps)
  print *,F_VV,(Fp_V - Fm_V)/(2*V*eps)
  print *,F_TV,(Fp_T - Fm_T)/(2*V*eps)
  print *,F_Vn,(Fp_n - Fm_n)/(2*V*eps)

  print *,"n1"
  n(1) = n0(1) - eps
  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  n(1) = n0(1) + eps
  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  print *,F_n(1),(Fp - Fm)/(2*eps)
  print *,F_Tn(1),(Fp_T - Fm_T)/(2*eps)
  print *,F_Vn(1),(Fp_V - Fm_V)/(2*eps)
  print *,F_nn(1,:),(Fp_n - Fm_n)/(2*eps)

  print *,"T"
  n = n0
  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T-T*eps,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  call calc_cavity_integral_LJ_Fres_test_TVN(1,sigma,eps_divk,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,F_T,(Fp - Fm)/(2*T*eps)
  print *,F_TT,(Fp_T - Fm_T)/(2*T*eps)
  print *,F_TV,(Fp_V - Fm_V)/(2*T*eps)
  print *,F_Tn,(Fp_n - Fm_n)/(2*T*eps)

  stop
end subroutine test_uf_LJ_cavity_Fres_TVN
