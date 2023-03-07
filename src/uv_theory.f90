!---------------------------------------------------------------------
! Module implementing uv-theory equation of state for Mie fluids.
!
! REFERENCES
!
! [1] van Westen and Gross 2021, Accurate thermodynamics of simple
! fluids and chain fluids based on first-order perturbation theory and
! second virial coefficients: uv-theory. J Chem Phys 155
! (10.1063/5.0073572)
! ---------------------------------------------------------------------
module uv_theory
  use hyperdual_mod
  use pair_potentials
  use numconstants, only: PI
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
  use eosdata, only: eosMie_UV_WCA, eosMie_UV_BH!, eosMie_UV_LAITTE
  use thermopack_var, only: base_eos_param, get_active_eos, base_eos_dealloc
  use hardsphere_bmcsl
  implicit none
  save

  !logical :: LAFITTE = .False.
  logical :: LAFITTE = .True.

  ! Coefficient vectors for u fraction correlation (Table I in [1])
  real, parameter :: C_PHI_WCA_LJ(2) = (/ 1.5661, 2.5422 /)
  real, parameter :: C_PHI_WCA_MIE(3) = (/ 1.4419, 1.1169, 16.8810 /)
  real, parameter :: C_PHI_BH_LJ(4) = (/ 0.74158, 0.14102, 2.3966, 4.6984 /)
  real, parameter :: C_PHI_BH_MIE(4) = (/ 0.72188, -0.0059822, 2.2919, 5.1647 /)
  real, parameter :: C1_PHI_BH_MIE(4) = (/ 0.72188, -0.0059822, 2.2919, 5.1647 /)
  real, parameter :: C2_PHI_BH_MIE(4) = (/ 0.0, 2.4676, 14.9735, 2.4017 /)
  real, parameter :: A_PHI_BH_MIE = 1.2187
  real, parameter :: B_PHI_BH_MIE = 4.2773

  ! Coefficient vectors for hard sphere diameter d (Table S23 in [1])
  real, parameter :: C_DHS_WCA_MIE(9) = (/ &
       1.92840364363978E+00, 4.43165896265079E-01, 5.20120816141761E-01 , &
       1.82526759234412E-01, 1.10319989659929E-02, -7.97813995328348E-05 , &
       1.29885156087242E-02, 6.41039871789327E-03, 1.85866741090323E-05 /)
  real, parameter :: C_DHS_BH_MIE(9) = (/ &
       1.09360455168912E-02, -2.00897880971934E-01, -1.27074910870683E-02 ,&
       1.40422470174053E-02, 7.35946850956932E-02, 1.28463973950737E-02 ,&
       3.71527116894441E-03, 5.05384813757953E-03, 4.91003312452622E-02 /)

  ! Coefficient vectors for effective packing fraction \eta_A (Table S2 in [1])
  real, parameter :: C_ETA_A_WCA_MIE(4,4) = RESHAPE( (/ &
       -0.888512176, 0.265207151, -0.851803291, -1.380304110, &
       -0.395548410, -0.626398537, -1.484059291, -3.041216688, &
       -2.905719617, -1.778798984, -1.556827067, -4.308085347, &
       0.429154871, 20.765871545, 9.341250676, -33.787719418 /), &
       shape=(/4,4/), order=(/2,1/))
  real, parameter :: C_ETA_A_BH_MIE(4,4) = RESHAPE( (/ &
       -1.217417282, 6.754987582, -0.5919326153, -28.99719604, &
       1.579548775, -26.93879416, 0.3998915410, 106.9446266, &
       -1.993990512, 44.11863355, -40.10916106, -29.61308489, &
       0.0, 0.0, 0.0, 0.0/), &
       shape=(/4,4/), order=(/2,1/))

  ! Coefficient matrices for effective packing fraction \eta_B (Eqs S35, S36 in [1])
  real, parameter :: C_ETA_B_WCA_MIE(3,2) = RESHAPE( (/ &
       -0.883143456, -0.618156214,&
       -0.589914255, -3.015264636,&
       -2.152046477, 4.7038689542/), shape=(/3,2/), order=(/2,1/))
  real, parameter :: C_ETA_B_BH_MIE(3,2) = RESHAPE( (/ &
       -0.960919783, -0.921097447, &
       -0.547468020, -3.508014069, &
       -2.253750186, 3.581161364/), shape=(/3,2/), order=(/2,1/))

  ! Coefficient matrix for the dimensionless correlation integral I^u_BH (Table S3 in [1])
  real, parameter :: C_IU_BH_MIE(3,4) = RESHAPE( (/ &
       0.0, 0.0, 0.0, 0.0, &
       0.1689669965, -0.9915458191, 0.7431421806, -4.323495934, &
       -0.5326281629, 2.660390140, -1.950702799, -0.000137219512 &
       /), shape=(/3,4/), order=(/2,1/))

  ! Coefficient matrix for the dimensionless correlation integral I^u_WCA (Table S4 in [1])
  real, parameter :: C_IU_WCA_MIE(6,6) = RESHAPE( (/ &
       -0.2622378162, 0.6585817423, 5.5318022309, 0.6902354794, -3.6825190645, -1.7263213318, &
       -0.1899241690, -0.5555205158, 9.1361398949, 0.7966155658, -6.1413017045 , 4.9553415149, &
       0.1169786415, -0.2216804790, -2.0470861617, -0.3742261343, 0.9568416381 , 10.1401796764, &
       0.5852642702, 2.0795520346, 19.0711829725, -2.3403594600, 2.5833371420, 432.3858674425, &
       -0.6084232211, -7.2376034572, 19.0412933614, 3.2388986513, 75.4442555789, -588.3837110653, &
       0.0512327656, 6.6667943569, 47.1109947616, -0.5011125797, -34.8918383146, 189.5498636006 &
       /), shape=(/6,6/), order=(/2,1/))

  ! Coefficients for calculation of DeltaB2 (Table S5 in [1])
  real, parameter :: C_DELTAB2_WCA(6) = (/ 1.45805207053190E-03, 3.57786067657446E-02, &
       1.25869266841313E-04, 1.79889086453277E-03, 0.0, 0.0 /)
  real, parameter :: A_DELTAB2_WCA = 1.05968091375869
  real, parameter :: B_DELTAB2_WCA = 3.41106168592999
  real, parameter :: C_DELTAB2_BH(6) = (/ 1.50542979585173E-03, 3.90426109607451E-02, &
       3.23388827421376E-04, 1.29508541592689E-02, 5.25749466058948E-05, 5.26748277148572E-04 /)
  integer, parameter :: A_DELTAB2_BH = 1
  integer, parameter :: B_DELTAB2_BH = 2
  integer, parameter :: C_EXPONENT_DELTAB2_BH = 4

  type, extends(base_eos_param) :: uv_theory_eos
     ! Mie potential parameters
     type(mie_potential_hd), allocatable :: mie(:,:)
     type(hyperdual), allocatable :: dhs(:,:)
     type(hyperdual), allocatable :: qhs(:,:)
     type(sutherlandsum), allocatable :: sutsum(:,:)
     !class(pair_potential), allocatable :: pot(:,:)
     type(hyperdual) :: epsdivk_x, sigma_x, sigma3_single, sigma3_double, dhs_x, dhs_x_adim

     ! Model control
   contains
     procedure, public :: dealloc => uv_dealloc
     procedure, public :: allocate_and_init => uv_allocate_and_init
     ! Assignment operator
     procedure, pass(this), public :: assign_eos => assign_uv_eos
  end type uv_theory_eos

  public :: uv_theory_eos
  public :: calcFres_uv
  public :: set_mie_parameters
  public :: calc_uv_mie_eta

contains

  subroutine assign_uv_eos(this, other)
    class(uv_theory_eos), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (uv_theory_eos)
       call this%assign_base_eos_param(other)
       this%mie = other%mie
    class default
    end select
  end subroutine assign_uv_eos

  subroutine uv_dealloc(eos)
    ! integer, intent(in) :: nc
    class(uv_theory_eos), intent(inout) :: eos
    ! Locals
    integer :: stat!, i, j
    call base_eos_dealloc(eos)

    select type (p_eos => eos)
    class is (uv_theory_eos)

       if (allocated(eos%mie)) then
          deallocate(eos%mie,stat=stat)
          if (stat /= 0) call stoperror("uv_dealloc: Not able to deallocate mie")
       end if

       if (allocated(eos%sutsum)) then
          !do i=1,nc
          !   do j=1,nc
          !call eos%sutsum%dealloc()
           !  end do
          !end do
          deallocate(eos%sutsum,stat=stat)
          if (stat /= 0) call stoperror("uv_dealloc: Not able to deallocate pot")
       end if

       if (allocated(eos%dhs)) then
          deallocate(eos%dhs,stat=stat)
          if (stat /= 0) call stoperror("uv_dealloc: Not able to deallocate dhs")
       end if

       if (allocated(eos%qhs)) then
          deallocate(eos%qhs,stat=stat)
          if (stat /= 0) call stoperror("uv_dealloc: Not able to deallocate qhs")
       end if

    end select
  end subroutine uv_dealloc

  subroutine uv_allocate_and_init(eos,nc,eos_label)
    class(uv_theory_eos), intent(inout) :: eos
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label !< EOS label
    call eos%dealloc()
    allocate(eos%mie(nc,nc))
    allocate(eos%sutsum(nc,nc))
    allocate(eos%dhs(nc,nc))
    allocate(eos%qhs(nc,nc))
  end subroutine uv_allocate_and_init

  !> Allocate memory for uv-theory eos
  function uv_eos_constructor(nc,eos_label) result(uv)
    ! Input:
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label
    ! Created object:
    type(uv_theory_eos) :: uv
    ! Locals

    call uv%allocate_and_init(nc,eos_label)
  end function uv_eos_constructor

  function calc_uv_Mie_eta(eos,nc,T,V,n) result(eta)
    ! Input
    type(uv_theory_eos), intent(in) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real :: eta !< Packing fraction [-]
    ! Locals
    real :: sumn
    type(hyperdual) :: eta_hd, rhonum_hd, T_hd, z_hd(nc)

    z_hd = 0.0
    T_hd = 0.0
    rhonum_hd = 0.0
    sumn = sum(n)
    z_hd%f0 = n/sumn
    T_hd%f0 = T
    rhonum_hd%f0 = sumn*N_AVOGADRO/V
    call calc_eta_dhs(eos, nc, T=T_hd, rho=rhonum_hd, z=z_hd, eta=eta_hd)
    eta = eta_hd%f0
  end function calc_uv_Mie_eta


  subroutine init_uv_theory(nc,comp,eos,ref)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas, kRgas, N_Avogadro, kB_const
    integer, intent(in) :: nc                           !< Number of components
    type(gendata_pointer), intent(inout) :: comp(nc)    !< Component vector
    class(uv_theory_eos), intent(inout) :: eos !< Equation of state
    character(len=*), intent(in) :: ref        !< Parameter sets to use for components
    ! Locals
    type(hyperdual) :: lamr_vec(nc), sigma_vec(nc), epsdivk_vec(nc)
    type(hyperdual) :: lam_mat(nc,2), C_mat(nc,2)
    integer :: i, idx

    ! Deallocate old memory and init new memory
    call eos%allocate_and_init(nc, eos_label="UV-MIE")

    ! Set pure component data from database
    do i=1,nc
       ! Assign pure Mie potential
       idx = getMiedataIdx(eos%subeosidx,trim(comp(i)%p_comp%ident),ref)
       lamr_vec(i) = MieArray(idx)%lamr
       sigma_vec(i) = MieArray(idx)%sigma
       epsdivk_vec(i) = MieArray(idx)%eps_divk

       ! Set ideal gas Cp
       comp(i)%p_comp%id_cp%cptype = 8
       comp(i)%p_comp%id_cp%cp(:) = 0.0
       comp(i)%p_comp%id_cp%cp(1) = 2.5
    end do

    ! Ensure consistent gas constants
    Rgas = N_Avogadro*kB_const
    kRgas = Rgas*1.0e3

    ! Set Mie potentials, including cross potential parameters
    call set_mie_parameters(eos,nc,lamr_vec,sigma_vec,epsdivk_vec)

    ! Set SutherlandSum potentials, including cross potential parameters
    do i=1,nc
       C_mat(i,1) = eos%mie(i,i)%Cmie
       C_mat(i,2) = -C_mat(i,1)
       lam_mat(i,1) = lamr_vec(i)
       lam_mat(i,2) = 6.0
    end do
    call set_sutsum_parameters(eos,nc,nt=2,C_mat=C_mat,lam_mat=lam_mat, &
         sigma_vec=sigma_vec,epsdivk_vec=epsdivk_vec)
  end subroutine init_uv_theory

  subroutine set_mie_parameters(eos,nc,lamr_vec,sigma_vec,epsdivk_vec)
    class(uv_theory_eos), intent(inout) :: eos !< Equation of state
    integer, intent(in)         :: nc                           !< Number of components
    type(hyperdual), intent(in) :: lamr_vec(nc), sigma_vec(nc), epsdivk_vec(nc) !< Mie parameters
    ! Locals
    type(mie_potential_hd) :: mie
    type(hyperdual) :: lama, lamr, sigma, epsdivk
    integer :: i, j

    ! Set pure component potentials
    do i=1,nc
       ! Assign pure Mie potential
       lama = 6.0
       lamr = lamr_vec(i)
       call mie%init(lama=lama, lamr=lamr, sigma=sigma_vec(i), epsdivk=epsdivk_vec(i))
       eos%mie(i,i) = mie

       ! Set cross potentials using standard, Lorentz-Berthelot combining rules
       do j = i+1,nc
          sigma = (sigma_vec(i) + sigma_vec(j))/2.0
          epsdivk = sqrt(epsdivk_vec(i) * epsdivk_vec(j))
          call mie%init(lama=lama, lamr=lamr, sigma=sigma, epsdivk=epsdivk)
          eos%mie(i,j) = mie !call uv_set_mie_pot(eos, i,j, mie)
          eos%mie(j,i) = mie !call uv_set_mie_pot(eos, j,i, mie)
       end do
    end do
  end subroutine set_mie_parameters

  subroutine set_sutsum_parameters(eos,nc,nt,C_mat,lam_mat,sigma_vec,epsdivk_vec)
    class(uv_theory_eos), intent(inout) :: eos !< Equation of state
    integer, intent(in)         :: nc, nt      !< Number of components, number of Sutherland terms
    type(hyperdual), intent(in) :: C_mat(nc, nt), lam_mat(nc,nt), sigma_vec(nc), epsdivk_vec(nc) !< Potential parameters
    ! Locals
    type(Sutherlandsum) :: sutsum
    type(hyperdual) :: sigma, epsdivk
    integer :: i, j
    ! Set pure component potentials
    do i=1,nc
       ! Assign pure SutherlandSum potential
       call sutsum%init(nt=nt, C=C_mat(i,:), lam=lam_mat(i,:), sigma=sigma_vec(i), epsdivk=epsdivk_vec(i))
       eos%sutsum(i,i) = sutsum
       ! Set cross potentials using standard, Lorentz-Berthelot combining rules
       do j = i+1,nc
          sigma = (sigma_vec(i) + sigma_vec(j))/2.0
          epsdivk = sqrt(epsdivk_vec(i) * epsdivk_vec(j))
          call sutsum%init(nt=nt, C=C_mat(i,:), lam=lam_mat(i,:), sigma=sigma_vec(i), epsdivk=epsdivk_vec(i))
          eos%sutsum(i,j) = sutsum
          eos%sutsum(j,i) = sutsum
       end do
    end do
  end subroutine set_sutsum_parameters

  subroutine calcFres_uv(eos,nc,T,V,n, f,f_T,f_V,f_n,&
       f_TT,f_VV,f_TV,f_Tn,f_Vn,f_nn)
    use hyperdual_utility, only: hyperdual_fres_wrapper
    !> The Fres function from uv-theory
    type(uv_theory_eos), intent(inout) :: eos
    integer, intent(in)                :: nc
    real, intent(in)  :: T             !< temperature (K)
    real, intent(in)  :: V             !< volume (m^3)
    real, intent(in)  :: n(nc)         !< mole numbers (mol)
    real, intent(out) :: f             !< Fres=Ares/RT (mol)
    real, optional, intent(inout) :: f_T,f_V,f_TT,f_VV,f_TV
    real, optional, intent(inout) :: f_n(nc),f_Tn(nc),f_Vn(nc),f_nn(nc,nc)
    call hyperdual_fres_wrapper(fun,eos,nc,T,V,n,f,f_T,f_V,f_n,&
         f_TT,f_VV,f_TV,f_Tn,f_Vn,f_nn)

  contains

    function fun(eos,nc,T,V,n) result(f)
      !class(uv_theory_eos), intent(inout) :: eos
      class(base_eos_param), intent(inout) :: eos
      integer, intent(in)                :: nc
      type(hyperdual), intent(in)  :: T             !< temperature (K)
      type(hyperdual), intent(in)  :: V             !< volume (m^3)
      type(hyperdual), intent(in)  :: n(nc)         !< mole numbers (mol)
      type(hyperdual) :: f                          !< Fres=Ares/RT (mol)
      ! Locals
      type(hyperdual) :: rhovec(nc)

      rhovec = N_Avogadro*n/V
      select type ( p_eos => eos )
      type is ( uv_theory_eos )
         call calc_ares_uv(p_eos, nc, T, rhovec=rhovec, a_res=f)
      end select
      f = f*sum(n)
    end function fun
  end subroutine calcFres_uv

  subroutine preCalcUVTheory(eos, nc, T, z)
    !> The reduced helmholtz energy from uv-theory
    integer, intent(in)                 :: nc
    class(uv_theory_eos), intent(inout) :: eos
    type(hyperdual), intent(in)  :: T             !< temperature (K)
    type(hyperdual), intent(in)  :: z(nc)         !< mole fractions (1/m^3)
    ! Locals
    type(hyperdual) :: sigma3_single   !< Cubed one-fluid diameter
    type(hyperdual) :: sigma3_double   !< Cubed one-fluid diameter
    type(hyperdual) :: epsdivk_x       !< One-fluid energy scale
    integer :: i,j

    ! Precalculate diameters for the additive hard-sphere reference
    ! system
    do i=1,nc
       if (eos%subeosidx == eosMie_UV_WCA) then
          LAFITTE = .FALSE.
          call dhs_WCA_Mie(T/eos%mie(i,i)%epsdivk, eos%mie(i,i), eos%dhs(i,i))
       else if (eos%subeosidx == eosMie_UV_BH) then
          if (LAFITTE) then
             call eos%sutsum(i,i)%update_beta(beta=1.0/T)
             call eos%sutsum(i,i)%calc_bh_diameter(beta=1.0/T, dhs=eos%dhs(i,i))
             call eos%mie(i,i)%calc_bh_diameter(beta=1.0/T, dhs=eos%dhs(i,i))
          else
             call dhs_BH_Mie(T/eos%mie(i,i)%epsdivk, eos%mie(i,i), eos%dhs(i,i))
          end if
       end if
    end do

    do i=1,nc
       do j=1,nc
          eos%dhs(i,j) = (eos%dhs(i,i) + eos%dhs(j,j))/2.0
       end do
    end do

    ! Calculate one-fluid Mie parameters
    epsdivk_x = 0.0
    sigma3_single = 0.0
    sigma3_double = 0.0
    eos%dhs_x = 0.0
    do i=1,nc
       eos%dhs_x = eos%dhs_x + z(i)*eos%dhs(i,i)**3
       sigma3_single = sigma3_single + z(i) * eos%mie(i,i)%sigma**3.0
       do j=1,nc
          sigma3_double = sigma3_double + z(i)*z(j) * eos%mie(i,j)%sigma**3.0
          epsdivk_x = epsdivk_x + eos%mie(i,j)%epsdivk * z(i)*z(j) * eos%mie(i,j)%sigma**3.0
       end do
    end do
    eos%epsdivk_x = epsdivk_x / sigma3_double
    eos%sigma_x = sigma3_single**(1.0/3)
    eos%sigma3_single = sigma3_single
    eos%sigma3_double = sigma3_double
    eos%dhs_x = (eos%dhs_x)**(1.0/3) !< Eq S46 in [1]
    eos%dhs_x_adim = eos%dhs_x/eos%sigma_x
  end subroutine preCalcUVTheory


  subroutine calc_ares_uv(eos, nc, T, rhovec, a_res)
    !> The reduced, residual helmholtz energy from uv-theory
    integer, intent(in)                :: nc
    class(uv_theory_eos), intent(inout) :: eos
    type(hyperdual), intent(in)  :: T             !< temperature (K)
    type(hyperdual), intent(in)  :: rhovec(nc)    !< number density (1/m^3)
    type(hyperdual), intent(out) :: a_res         !< a=Ares/NRT (-)
    ! Locals
    type(hyperdual) :: z(nc), rho, rho_r, T_x
    type(hyperdual) :: a0_res, a_hs_res, delta_a0, Delta_a1u
    type(hyperdual) :: phi, Delta_B2, Delta_B2u
    type(hyperdual) :: lama, lamr, dhs_i(nc)
    integer :: i

    ! Precalculations
    rho = sum(rhovec)
    z = rhovec/rho

    call preCalcUVTheory(eos,nc,T,z)
    lamr = eos%mie(1,1)%lamr
    lama = eos%mie(1,1)%lama
    do i = 1,nc
       dhs_i(i) = eos%dhs(i,i)
       if (eos%mie(i,i)%lamr /= lamr .or. eos%mie(i,i)%lama /= lama) then
          stop "Invalid Mie exponents in uv-theory"
       end if
    end do

    ! TODO: fix this for SutherlandSum
    ! Calculate the u-fraction phi
    T_x = T/eos%epsdivk_x
    rho_r = rho*eos%sigma3_single
    if (eos%subeosidx == eosMie_UV_WCA) then
       call phi_WCA_Mie(rho_r, lamr, phi)
    else if (eos%subeosidx == eosMie_UV_BH) then
       call phi_BH_Mie(T_x, rho_r, eos%mie(1,1)%lamr, phi)
    end if

    call calc_ares_hardsphere_bmcsl(nc, rhovec, dhs_i, a_hs_res)

    ! Calculate perturbation part of reference system
    if (LAFITTE) then
       delta_a0 = 0.0
    else
       call Delta_a0_Mie(eos, nc, T, rho, z, delta_a0)
    end if

    ! Calculate contribution from reference system
    a0_res = a_hs_res + delta_a0

    ! Calculate Delta a1u and Delta B2u
    if (LAFITTE) then
       call delta_a1u_b2u_sutsum(eos, nc, T, rho, z, delta_a1u, delta_b2u)
    else
       call delta_a1u_b2u_Mie(eos, nc, T, rho, z, delta_a1u, delta_b2u)
    end if

    ! Calculate Delta B2
    if (LAFITTE) then
       call DeltaB2_quadrature(eos,nc,T,z, Delta_B2)
    else
       call DeltaB2_Mie(eos,nc,T,z, Delta_B2)
    end if

    ! Calculate total helmholtz energy
    a_res = a0_res + Delta_a1u + (1.0-phi)*(Delta_B2 - Delta_B2u)*rho
  end subroutine calc_ares_uv

  subroutine delta_a1u_b2u_sutsum(eos, nc, T, rho, z, delta_a1u, delta_b2u)
    use sutherland_a1tilde
    !> The u contribution computed directly according to BH, without the one-fluid approximation
    type(uv_theory_eos), intent(inout) :: eos ! uv-theory eos
    integer, intent(in) :: nc ! number of components
    type(hyperdual), intent(in) :: T      ! temperature (K)
    type(hyperdual), intent(in) :: rho    ! density (1/m^3)
    type(hyperdual), intent(in) :: z(nc)    ! mole fractions
    type(hyperdual), intent(out) :: delta_a1u ! a1 (-)
    type(hyperdual), intent(out) :: delta_b2u ! B2 contribution from a1 (m^3)
    ! Locals
    type(hyperdual) :: lamij, epsij, zetax, prefac, x0ij
    type(hyperdual) :: a1til, a1til0, a1ij, a1ij0
    type(hyperdual) :: dhs_bh(nc,nc)
    integer :: i, j, k

    ! Calculate zetax and eta0 using Barker-Henderson HS diameters
    zetax = 0.0
    do i=1,nc
       do j=1,nc
          call calc_bh_diameter(eos%sutsum(i,j),beta=1.0/T,dhs=dhs_bh(i,j))
          zetax = zetax + z(i)*z(j)*dhs_bh(i,j)**3
       end do
    end do
    zetax = (PI/6) * rho * zetax

    ! Calculate double-sum of a1 contributions
    delta_a1u = 0.0
    delta_b2u = 0.0
    do i=1,nc
       do j=1,nc
          x0ij = eos%sutsum(i,j)%sigma / dhs_bh(i,j)
          epsij = eos%sutsum(i,j)%epsdivk
          a1ij0 = 0.0
          a1ij = 0.0
          prefac = - (PI/6) * dhs_bh(i,j)**3
          do k=1, eos%sutsum(i,j)%nt
             lamij = eos%sutsum(i,j)%lam(k)
             call calc_a1tilde_sutherland(x0ij,zetax,lamij,epsij, a1til,a1til0)
             a1ij0 = a1ij0 + eos%sutsum(i,j)%C(k)* prefac      * a1til0
             a1ij = a1ij  + eos%sutsum(i,j)%C(k) *(prefac*rho) * a1til
          end do
          delta_a1u = delta_a1u + z(i)*z(j)*a1ij
          delta_b2u = delta_b2u + z(i)*z(j)*a1ij0
       end do
    end do

    ! uv-theory incorporates the beta factor directly into a1
    delta_a1u = delta_a1u/T
    delta_b2u = delta_b2u/T
  end subroutine delta_a1u_b2u_sutsum


  subroutine delta_a1u_b2u_lafitte(eos, nc, T, rho, z, delta_a1u, delta_b2u)
    use sutherland_a1tilde
    !> The u contribution computed directly according to BH, without the one-fluid approximation
    type(uv_theory_eos), intent(inout) :: eos ! uv-theory eos
    integer, intent(in) :: nc ! number of components
    type(hyperdual), intent(in) :: T      ! temperature (K)
    type(hyperdual), intent(in) :: rho    ! density (1/m^3)
    type(hyperdual), intent(in) :: z(nc)    ! mole fractions
    type(hyperdual), intent(out) :: delta_a1u ! a1 (-)
    type(hyperdual), intent(out) :: delta_b2u ! B2 contribution from a1 (m^3)
    ! Locals
    type(hyperdual) :: lrij, laij, epsij, zetax, prefac, x0ij
    type(hyperdual) :: a1til_att, a1til_rep, a1til_att0, a1til_rep0, a1ij, a1ij0
    type(hyperdual) :: dhs_bh(nc,nc)
    integer :: i, j

    ! Calculate zetax and eta0 using Barker-Henderson HS diameters
    zetax = 0.0
    do i=1,nc
       do j=1,nc
          call calc_bh_diameter(eos%mie(i,j),beta=1.0/T,dhs=dhs_bh(i,j))
          zetax = zetax + z(i)*z(j)*dhs_bh(i,j)**3
       end do
    end do
    zetax = (PI/6) * rho * zetax

    ! Calculate double-sum of a1 contributions
    delta_a1u = 0.0
    delta_b2u = 0.0
    do i=1,nc
       do j=1,nc
          x0ij = eos%mie(i,j)%sigma / dhs_bh(i,j)
          prefac = eos%mie(i,j)%Cmie * (PI/6) * dhs_bh(i,j)**3
          lrij = eos%mie(i,j)%lamr
          laij = eos%mie(i,j)%lama
          epsij = eos%mie(i,j)%epsdivk
          call calc_a1tilde_sutherland(x0ij,zetax,lrij,epsij, a1til_rep,a1til_rep0)
          call calc_a1tilde_sutherland(x0ij,zetax,laij,epsij, a1til_att,a1til_att0)
          a1ij0 = prefac      * (a1til_att0 - a1til_rep0)
          a1ij = (prefac*rho) * (a1til_att  - a1til_rep)
          delta_a1u = delta_a1u + z(i)*z(j)*a1ij
          delta_b2u = delta_b2u + z(i)*z(j)*a1ij0
       end do
    end do

    ! uv-theory incorporates the beta factor directly into a1
    delta_a1u = delta_a1u/T
    delta_b2u = delta_b2u/T
  end subroutine delta_a1u_b2u_lafitte

  subroutine delta_a1u_b2u_Mie(eos, nc, T, rho, z, delta_a1u, delta_b2u)
    !> Pertubation contribution to a1u going into the u-term in uv-theory (Eq S42 in [1])
    type(uv_theory_eos), intent(inout) :: eos ! uv-theory eos
    integer, intent(in) :: nc ! number of components
    type(hyperdual), intent(in) :: T      ! temperature (K)
    type(hyperdual), intent(in) :: rho    ! density (1/m^3)
    type(hyperdual), intent(in) :: z(nc)    ! mole fractions
    type(hyperdual), intent(out) :: delta_a1u ! a1 (-)
    type(hyperdual), intent(out) :: delta_b2u ! B2 contribution from a1 (m^3)
    ! Locals
    type(hyperdual) :: beta, lamr, lama
    type(hyperdual) :: Iu, Iu_zerodensity, rho_r, T_r
    type(mie_potential_hd) :: mie_x

    ! Calc effective one-fluid parameters
    lamr = eos%mie(1,1)%lamr
    lama = eos%mie(1,1)%lama
    call mie_x%init(lama=lama, lamr=lamr, sigma=eos%sigma_x, epsdivk=eos%epsdivk_x)
    rho_r = rho*eos%sigma3_single
    T_r = T/eos%epsdivk_x

    ! Calc correlation integral of effective pure fluid
    if (eos%subeosidx == eosMie_UV_WCA) then
       call Iu_WCA_Mie(T_r, rho_r, mie_x, Iu_zerodensity, Iu, dhs=eos%dhs_x_adim)
    else if (eos%subeosidx == eosMie_UV_BH) then
       call Iu_BH_Mie(T_r, rho_r, mie_x, Iu_zerodensity, Iu, dhs=eos%dhs_x_adim)
    end if

    ! Calculate Delta_a1u (Eq S42 in [1])
    beta = 1.0/T_r ! the kB factor is subsumed in epsdivk
    Delta_b2u = 2*PI * beta * eos%sigma3_double * Iu_zerodensity
    Delta_a1u = Delta_b2u * rho * Iu/Iu_zerodensity
  end subroutine delta_a1u_b2u_Mie


  subroutine Iu_WCA_Mie(T_r, rho_r, mie, Iu_zerodensity, Iu, dhs)
    !> Correlation integral of pure fluids going into the u-term in uv-theory (Eq S54 in [1])
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(in) :: T_r      ! reduced temperature (-)
    type(hyperdual), intent(in) :: rho_r    ! reduced density (-)
    type(hyperdual), intent(out) :: Iu_zerodensity, Iu ! correlation integral and its zero density limit (-)
    type(hyperdual), intent(in) :: dhs      ! hard-sphere diameter (-)
    ! Locals
    type(hyperdual) :: n, nu, sigma, eps_divk
    type(hyperdual) :: rm, rs, alpha_rm
    type(hyperdual) :: C(6), pade_num, pade, qhs, tau, tauvec(6)
    integer :: i, j

    ! Extract Mie parameters for component ic
    n = mie%lama
    nu = mie%lamr
    sigma = mie%sigma
    eps_divk = mie%epsdivk
    rm = mie%rmin_adim
    rs = rm

    ! Calculate zero-density limit of Iu (Eq S54 in [1])
    call qhs_WCA_Mie(T_r=T_r, mie=mie, qhs=qhs)
    qhs = qhs/sigma
    alpha_rm = mie%alpha_x(rm)
    Iu_zerodensity = (qhs**3-rm**3)/3.0 - alpha_rm

    ! Calculate length scale tau (Eq S33)
    tau = rs - dhs

    ! Calculate remaining Padé approximation for the density-dependent term
    tauvec(1) = 1.0
    tauvec(2) = 1.0/nu
    tauvec(3) = 1.0/nu**2
    tauvec(4:6) = tauvec(1:3)*tau
    C = 0.0
    do i=1,6
       do j=1,6
          C(i) = C(i) + C_IU_WCA_MIE(i,j)*tauvec(j)
       end do
    end do
    pade_num = mie%cmie*(C(1)*rho_r + C(2)*rho_r**2 + C(3)*rho_r**3)
    pade = pade_num/(1.0 + C(4)*rho_r + C(5)*rho_r**2 + C(6)*rho_r**3)

    Iu = Iu_zerodensity + pade
  end subroutine Iu_WCA_Mie

  subroutine Iu_BH_Mie(T_r, rho_r, mie, Iu_zerodensity, Iu, dhs)
    !> Correlation integral of pure fluids going into the u-term in uv-theory (Eq S54 in [1])
    type(hyperdual), intent(in) :: T_r      ! reduced temperature (-)
    type(hyperdual), intent(in) :: rho_r    ! reduced density (-)
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(out) :: Iu_zerodensity, Iu ! correlation integral and its zero density limit (-)
    type(hyperdual) :: dhs
    ! Locals
    type(hyperdual) :: n, nu, sigma, eps_divk
    type(hyperdual) :: rs, one
    type(hyperdual) :: C(3), C11, C12, C13, tau

    ! Extract Mie parameters for component ic
    n = mie%lama
    nu = mie%lamr
    sigma = mie%sigma
    eps_divk = mie%epsdivk
    rs = 1.0

    ! Calculate length scale tau (Eq S33)
    tau = rs - dhs

    ! Calculate C2 and C3 (Eq. S50)
    C = (C_IU_BH_MIE(:,1) + C_IU_BH_MIE(:,2)/nu) &
         - (C_IU_BH_MIE(:,3) + C_IU_BH_MIE(:,4)/nu)*tau

    ! Calculate C1 (Eq. S49)
    C11 = dhs**(6.0-nu) * ((2**(3-nu%f0)-dhs**(nu%f0-3.0))/(3.0 - nu))  + (1.0-8.0*dhs**3)/24.0
    C12 = -0.75 * ((dhs**(6.0-nu) * (2.0**(4-nu%f0)-dhs**(nu-4.0))/(4.0 - nu))  + (1.0-4.0*dhs**2)/8.0)
    C13 = 1/16.0 * ( ((2*dhs)**(6.0-nu) - 1.0)/(6.0 - nu) - log(2*dhs) )
    C(1) = (4*PI/3)*(C11 + C12 + C13)

    ! Calculate correlation integral (Eq S48)
    one = 1.0
    Iu_zerodensity = - mie%alpha_x(x=one)
    Iu = Iu_zerodensity + mie%Cmie*(C(1)*rho_r + C(2)*rho_r**2)/(1.0+C(3)*rho_r)**2

  end subroutine Iu_BH_Mie

  subroutine DeltaB2_quadrature(eos,nc,T,z, DeltaB2)
    !> Perturbation contribution to the v-term in uv-theory (Eq S57 in [1])
    integer, intent(in) :: nc !< Number of components
    type(uv_theory_eos), intent(in) :: eos
    type(hyperdual), intent(in) :: T         ! temperature (K)
    type(hyperdual), intent(in) :: z(nc)     ! mole fractions (-)
    type(hyperdual), intent(out) :: DeltaB2  ! Perturbation contribution to B2 (m^3)
    ! Locals
    integer :: i, j
    DeltaB2 = 0.0
    do i=1,nc
       do j=1,nc
          DeltaB2 = DeltaB2 + z(i)*z(j)*eos%sutsum(i,j)%B2(beta=1.0/T) - (2*PI/3 * eos%dhs(i,j)**3)
       end do
    end do
  end subroutine DeltaB2_quadrature

  subroutine DeltaB2_Mie(eos,nc,T,z, DeltaB2)
    !> Perturbation contribution to the v-term in uv-theory (Eq S57 in [1])
    integer, intent(in) :: nc !< Number of components
    type(uv_theory_eos), intent(in) :: eos
    type(hyperdual), intent(in) :: T         ! temperature (K)
    type(hyperdual), intent(in) :: z(nc)     ! mole fractions (-)
    type(hyperdual), intent(out) :: DeltaB2  ! Perturbation contribution to B2 (m^3)
    ! Locals
    type(hyperdual) :: T_r, DeltaB2ij
    integer :: i, j
    logical :: is_BH

    is_BH = (eos%subeosidx==eosMie_UV_BH)
    DeltaB2 = 0.0
    do i=1,nc
       do j=1,nc
          T_r = T/eos%mie(i,j)%epsdivk
          call DeltaB2_pure_Mie(is_BH, T_r=T_r, mie=eos%mie(i,j), DeltaB2=DeltaB2ij)
          DeltaB2 = DeltaB2 + z(i)*z(j)*DeltaB2ij
       end do
    end do
  end subroutine DeltaB2_Mie

  subroutine DeltaB2_pure_Mie(is_BH, T_r, mie, DeltaB2)
    !> Perturbation contribution to the v-term in uv-theory (Eq S57 in [1])
    logical, intent(in) :: is_BH              ! True for BH, False for WCA
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(in) :: T_r        ! reduced temperature (-)
    type(hyperdual), intent(out) :: DeltaB2   ! Perturbation contribution to B2 (m^3)
    ! Locals
    type(hyperdual) :: C0, C1, C2, C3
    real :: a_expo, b_expo, c_expo, C_DELTAB2(6)
    type(hyperdual) :: rm, rc, rs, alpha_rs, alpha_rc, beta, beta_eff, qhs
    type(hyperdual) :: sigma, eps_divk, n,nu, wca_term

    ! Extract Mie parameters
    n = mie%lama
    nu = mie%lamr
    sigma = mie%sigma
    eps_divk = mie%epsdivk
    rm = mie%rmin_adim
    rc = 5.0

    ! Precalculate quantities
    beta = 1.0/T_r
    if (is_BH) then
       rs = 1.0
       call dhs_BH_Mie(T_r, mie, qhs)
       qhs = qhs/mie%sigma
       C_DELTAB2 = C_DELTAB2_BH
       a_expo = A_DELTAB2_BH
       b_expo = B_DELTAB2_BH
       c_expo = C_EXPONENT_DELTAB2_BH
       wca_term = 0.0

    else ! wca
       rs = rm
       call qhs_WCA_Mie(T_r, mie, qhs)
       qhs = qhs/mie%sigma
       C_DELTAB2 = C_DELTAB2_WCA
       a_expo = A_DELTAB2_WCA
       b_expo = B_DELTAB2_WCA
       c_expo = 1.0 ! this value won't matter
       wca_term = (rm**3-qhs**3)/3.0*(exp(beta)-1.0)
    end if
    alpha_rs = mie%alpha_x(rs)
    alpha_rc = mie%alpha_x(rc)
    C0 = 1.0 - 3*(alpha_rs-alpha_rc)/(rc**3-rs**3)

    ! Calculate beta_eff (Eq S58 in [1])
    C1 = C_DELTAB2(1) + C_DELTAB2(2)/nu
    C2 = C_DELTAB2(3) + C_DELTAB2(4)/nu
    C3 = C_DELTAB2(5) + C_DELTAB2(6)/nu
    beta_eff = beta*(1.0 - C0/(1.0 + C1*beta**a_expo + C2*beta**b_expo + C3*beta**c_expo))

    DeltaB2 = -2*PI*sigma**3*(wca_term + (rc**3-rs**3)/3.0*(exp(beta_eff)-1.0) + beta*alpha_rc)
  end subroutine DeltaB2_pure_Mie

  subroutine calc_eta_dhs(eos, nc, T, rho, z, eta)
    !> Hard-sphere packing fraction
    integer, intent(in) :: nc !< Number of components
    type(uv_theory_eos), intent(in) :: eos
    type(hyperdual), intent(in) :: T, rho, z(nc)
    type(hyperdual), intent(out) :: eta ! packing fraction (-)
    ! Locals
    type(hyperdual) :: dhs
    integer :: i
    eta = 0.0
    do i=1,nc
       if (eos%subeosidx==eosMie_UV_BH) then
          call dhs_BH_Mie(T/eos%mie(i,i)%epsdivk, eos%mie(i,i), dhs)
       else
          call dhs_WCA_Mie(T/eos%mie(i,i)%epsdivk, eos%mie(i,i), dhs)
       end if
       eta = eta + z(i)*dhs**3
    end do
    eta = pi/6*rho*eta
  end subroutine calc_eta_dhs

  subroutine Delta_a0_mie(eos, nc, T, rho, x, delta_a0)
    !> Perturbation contribution delta_a0 to the reference Helmholtz
    !> energy a0, defined by a_0 = ahs_d + delta_a0. See Eq S40 in [1].
    integer, intent(in) :: nc !< Number of components
    type(uv_theory_eos), intent(in) :: eos
    type(hyperdual), intent(in) :: T         ! temperature (K)
    type(hyperdual), intent(in) :: rho       ! number density (1/m3)
    type(hyperdual), intent(in) :: x(nc)     ! mole fractions (-)
    type(hyperdual), intent(out) :: delta_a0 ! perturbation contribution of a0 (-)
    ! Locals
    type(hyperdual) :: IA, IB, I0f, eta, etaA, etaB, qhs_r, dhs_r, Tij_r, tau, rs_r
    integer :: i, j

    call calc_eta_dhs(eos, nc, T, rho, x, eta)

    delta_a0 = 0.0
    do i=1,nc
       do j=1,nc
          Tij_r = T/eos%mie(i,j)%epsdivk
          dhs_r = eos%dhs(i,j)/eos%mie(i,j)%sigma
          !dhs_r = 0.5*(eos%dhs(i,i) + eos%dhs(j,j))
          if (eos%subeosidx == eosMie_UV_WCA) then
             rs_r = eos%mie(i,j)%rmin_adim
             call qhs_WCA_Mie(Tij_r, eos%mie(i,j), qhs_r)
             qhs_r = qhs_r/eos%mie(i,j)%sigma

             tau = rs_r - dhs_r ! adimensional length scale
             call etaA_Mie(eta, tau, eos%mie(i,j), etaA, C_ETA_A_WCA_MIE)
             call etaB_Mie(eta, tau, etaB, C_ETA_B_WCA_MIE)
          else if (eos%subeosidx == eosMie_UV_BH) then
             rs_r = 1.0
             dhs_r = 0.5*(eos%dhs(i,i)/eos%mie(i,i)%sigma + eos%dhs(j,j)/eos%mie(j,j)%sigma)
             tau = rs_r - dhs_r ! adimensional length scale
             call etaA_Mie(eta, tau, eos%mie(i,j), etaA, C_ETA_A_BH_MIE)
             call etaB_Mie(eta, tau, etaB, C_ETA_B_BH_MIE)

             ! feos uses different definitions of tau for BH and WCA...
             dhs_r = eos%dhs(i,j)/eos%mie(i,j)%sigma
             qhs_r = dhs_r
          end if

          IA = (1.0-etaA/2.0)/(1.0-etaA)**3 * (rs_r**3-qhs_r**3)
          IB = (1.0-etaB/2.0)/(1.0-etaB)**3 * (rs_r**3-dhs_r**3)
          I0f = IA-IB
          delta_a0 = delta_a0 + x(i)*x(j)*eos%mie(i,j)%sigma**3 * I0f
       end do
    end do
    delta_a0 = -(2*PI/3)*rho*delta_a0
  end subroutine Delta_a0_mie

  subroutine etaA_Mie(eta, tau, mie, etaA, C_ETA_A_MIE)
    !> Intermediate quantity to calculate the soft-repulsive
    !> perturbation term \Delta a0. The effective packing fraction
    !> used to calculate I_A
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(in) :: tau   ! adimensional length scale
    type(hyperdual), intent(in) :: eta   ! packing fraction
    real, intent(in) :: C_ETA_A_MIE(4,4) ! coefficientS for BH or WCA
    type(hyperdual), intent(out) :: etaA ! effective packing fraction A
    ! Locals
    type(hyperdual) :: C(4), nu
    nu = mie%lamr
    C =     (C_ETA_A_MIE(:,1)+C_ETA_A_MIE(:,2)/nu)*tau
    C = C + (C_ETA_A_MIE(:,3)+C_ETA_A_MIE(:,4)/nu)*tau**2

    etaA = eta + C(1)*eta + C(2)*eta**2 + C(3)*eta**3 + C(4)*eta**4
  end subroutine etaA_Mie


  subroutine etaB_Mie(eta, tau, etaB, C_ETA_B_MIE)
    !> Intermediate quantity to calculate the soft-repulsive
    !> perturbation term \Delta a0. The effective packing fraction
    !> used to calculate I_A
    type(hyperdual), intent(in) :: tau   ! adimensional length scale
    type(hyperdual), intent(in) :: eta   ! packing fraction
    real, intent(in) :: C_ETA_B_MIE(3,2) ! coefficients for BH or WCA
    type(hyperdual), intent(out) :: etaB ! effective packing fraction B

    ! Locals
    type(hyperdual) :: C(3)

    ! Eq S36 in [1]
    C = C_ETA_B_MIE(:,1)*tau + C_ETA_B_MIE(:,2)*tau**2

    etaB = eta + C(1)*eta + C(2)*eta**2 + C(3)*eta**3
  end subroutine etaB_Mie

  subroutine dhs_WCA_Mie(T_r,mie,dhs)
    !> The effective hard sphere diameter used for WCA
    type(hyperdual), intent(in) :: T_r        ! reduced temperature (-)
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(out) :: dhs       ! hard-sphere diameter (m)
    ! Locals
    type(hyperdual) :: c, nu

    ! Hard-sphere diameter (Eq S19)
    nu = mie%lamr
    c = (nu/6.0)**(-nu/(12.0-2*nu)) - 1.0
    dhs = mie%rmin*(1.0/(1.0+c*T_r**0.5))**(2.0/nu)
  end subroutine dhs_WCA_Mie

  subroutine dhs_BH_Mie(T_r,mie,dhs)
    !> The effective hard sphere diameter used for BH
    type(hyperdual), intent(in) :: T_r        ! reduced temperature (-)
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(out) :: dhs       ! hard-sphere diameter (m)
    ! Locals
    type(hyperdual) :: ap, nu, T14, C0, C1, C2, C3, C4, D, one, six, seven
    type(mie_potential_hd) :: mie_76 ! Mie potential

    ! Assemble C coefficients (Eqs S24 and S26)
    nu = mie%lamr
    C0 = -2*nu/((mie%lama-nu)*mie%cmie)
    one = 1.0
    six = 6.0
    seven = 7.0
    call mie_76%init(lama=six, lamr=seven, sigma=mie%sigma, epsdivk=mie%epsdivk)
    ap = 1.0/mie%alpha_x(one) - 1.0/mie_76%alpha_x(one)
    C1 =                   C_DHS_BH_MIE(1)*ap
    C2 = C_DHS_BH_MIE(2) + C_DHS_BH_MIE(3)*ap
    C3 = C_DHS_BH_MIE(4) + C_DHS_BH_MIE(5)*ap + C_DHS_BH_MIE(6)*ap*ap
    C4 = C_DHS_BH_MIE(7) + C_DHS_BH_MIE(8)*ap + C_DHS_BH_MIE(9)*ap*ap

    ! Eq S24
    T14 = T_r**(1.0/4)
    D = C1*T14 + C2*T_r/T14 + C3*T_r*T14

    ! Hard-sphere diameter (S23)
    dhs = mie%sigma*(1.0 + C0*T_r + D*log(1.0+T_r) + C4*T_r*T_r)**(-1.0/(2.0*nu))
  end subroutine dhs_BH_Mie


  subroutine qhs_WCA_Mie(T_r,mie,qhs)
    !> The effective hard sphere diameter used for the WCA perturbation contribution to DeltaB2
    type(hyperdual), intent(in) :: T_r         ! reduced temperature (-)
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(out) :: qhs       ! hard-sphere diameter (m)
    ! Locals
    type(hyperdual) :: n, nu, nu7, C0, C1, C2, C3

    ! Assemble C coefficients (Eqs S24 and S26)
    n = mie%lama
    nu = mie%lamr
    nu7 = nu-7.0
    C0 = sqrt(2*PI*nu/n)
    C1 = C_DHS_WCA_MIE(1) + C_DHS_WCA_MIE(2)*nu7
    C2 = C_DHS_WCA_MIE(3) + C_DHS_WCA_MIE(4)*nu7 + C_DHS_WCA_MIE(5)*nu7**2 + C_DHS_WCA_MIE(6)*nu7**3
    C3 =                    C_DHS_WCA_MIE(7)*nu7 + C_DHS_WCA_MIE(8)*nu7**2 + C_DHS_WCA_MIE(9)*nu7**3

    ! Hard-sphere diameter (S28)
    qhs = mie%rmin*(1.0 + C0*T_r**0.5 + C1*T_r + C2*T_r**1.5 + C3*T_r*T_r)**(-1.0/(2*nu))
  end subroutine qhs_WCA_Mie


  subroutine phi_WCA_Mie(rho_r,lamr, phi)
    !> Eq 13 in [1]
    type(hyperdual), intent(in) :: rho_r   ! reduced density (-)
    type(hyperdual), intent(in) :: lamr    ! repulsive exponent (-)
    type(hyperdual), intent(out) :: phi    ! u fraction (-)
    ! Locals
    type(hyperdual) :: x, e2x, tanhx
    real :: C(3)

    ! Coefficients (Eq 16)
    if (lamr%f0==12.0) then
       C(1) = C_PHI_WCA_LJ(1)
       C(2) = C_PHI_WCA_LJ(2)
       C(3) = 0.0
    else
       C = C_PHI_WCA_MIE
    end if

    ! Calculate tanh factor
    x = (C(1) + (C(2) + C(3)/lamr)*rho_r)*rho_r
    e2x = exp(2*x)
    tanhx = (e2x-1.0)/(e2x+1.0)

    ! Calculate phi
    phi = tanhx
  end subroutine phi_WCA_Mie

  subroutine phi_BH_Mie(T_r,rho_r, lamr, phi)
    !> Eq 14 in [1]
    type(hyperdual), intent(in) :: T_r        ! reduced temperature (-)
    type(hyperdual), intent(in) :: rho_r      ! reduced density (-)
    type(hyperdual), intent(in) :: lamr    ! repulsive exponent (-)
    type(hyperdual), intent(out) :: phi       ! u fraction (-)
    ! Locals
    type(hyperdual) :: beta, sigma_beta, prefac
    type(hyperdual) :: x, e2x, tanhx
    real :: C(4)
    real :: a_expo, b_expo

    ! Coefficients (Eq 16)
    if (lamr%f0==12.0) then
       a_expo = 1.0
       b_expo = 3.0
       C = C_PHI_BH_LJ
    else
       a_expo = A_PHI_BH_MIE
       b_expo = B_PHI_BH_MIE
       C = C1_PHI_BH_MIE + C2_PHI_BH_MIE/lamr%f0
    end if

    ! Calculate prefactor
    beta = 1.0/T_r
    sigma_beta = sqrt(C(2)*beta**2 / (1.0 + C(2)*beta**2))
    prefac = C(1) + sigma_beta*(1.0-C(1))

    ! Calculate tanh factor
    x = C(3)*rho_r**a_expo + C(4)*rho_r**b_expo
    e2x = exp(2*x)
    tanhx = (e2x-1.0)/(e2x+1.0)

    ! Calculate phi
    phi = prefac*tanhx
  end subroutine phi_BH_Mie

  subroutine phi_BH_alphavdw(T_r,rho_r, alpha, phi)
    type(hyperdual), intent(in) :: T_r        ! reduced temperature (-)
    type(hyperdual), intent(in) :: rho_r      ! reduced density (-)
    type(hyperdual), intent(in) :: alpha      ! vdw energy (-)
    type(hyperdual), intent(out) :: phi       ! u fraction (-)
    ! Locals
    type(hyperdual) :: lamr
    lamr = approx_lamr_from_alpha(alpha)
    call phi_BH_Mie(T_r, rho_r, lamr, phi)
  end subroutine phi_BH_alphavdw


end module uv_theory
