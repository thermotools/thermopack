!---------------------------------------------------------------------
! Module for holding parameters of the SAFT-VR Mie EoS
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------

module saftvrmie_containers
  use saftvrmie_parameters, only: saftvrmie_data
  use saftvrmie_options
  use thermopack_var, only: base_eos_param, get_active_eos
  implicit none
  private
  save

  !> Container for SAFT-VR Mie static parameters
  type :: saftvrmie_param_container
    !> Component parameters
    type(saftvrmie_data), allocatable, dimension(:) :: comp
    !> Binary interaction parameters for the well depth
    real, allocatable, dimension(:,:) :: kij
    !> Binary interaction parameters for the repulsive exponent
    real, allocatable, dimension(:,:) :: gamma_ij
    !> Binary interaction parameters for sigma
    real, allocatable, dimension(:,:) :: lij
    !> van der Waals-like attractive constant
    real, allocatable, dimension(:,:) :: alpha_ij
    !> Function of alpha
    real, allocatable, dimension(:,:,:) :: f_alpha_ij
    !> Binary attractive exponent of the Mie potential
    real, allocatable, dimension(:,:) :: lambda_a_ij
    !> Binary repulsive exponent of the Mie potential
    real, allocatable, dimension(:,:) :: lambda_r_ij
    !> Temperature-independent segment diameter (m)
    real, allocatable, dimension(:,:) :: sigma_ij
    !> Binary well depth divided by Boltzmann's k (K)
    real, allocatable, dimension(:,:) :: eps_divk_ij
    !> Binary Mie C factor
    real, allocatable, dimension(:,:) :: Cij
    !> T-independent part of binary Feynman--Hibbs D parameter
    real, allocatable, dimension(:,:) :: DFeynHibbsParam_ij
    !***** Duplicates included for speed and conveniance *****
    !> Copy of comp(:)%ms for easy looping
    real, allocatable, dimension(:) :: ms
    !> Cube of temperature-independent segment diameter (m3)
    real, allocatable, dimension(:,:) :: sigma_ij_cube
    !> Parameters in the quantum correction, first order - attractive
    real, allocatable, dimension(:,:) :: Quantum_const_1a_ij
    !> Parameters in the quantum correction, first order - repulsive
    real, allocatable, dimension(:,:) :: Quantum_const_1r_ij
    !> Parameters in the quantum correction, second order - attractive
    real, allocatable, dimension(:,:) :: Quantum_const_2a_ij
    !> Parameters in the quantum correction, second order - repulsive
    real, allocatable, dimension(:,:) :: Quantum_const_2r_ij
  contains
    ! Assignment operator
    procedure, public :: assign_saftvrmie_param_container
    generic, public :: assignment(=) => assign_saftvrmie_param_container
  end type saftvrmie_param_container

  !> Container for a_ij and differentials
  type :: saftvrmie_aij
    real, allocatable, dimension(:,:) :: am,am_T,am_V,am_TT,am_VV,am_TV
    real, allocatable, dimension(:,:) :: am_VVV,am_VVT,am_VTT
    real, allocatable, dimension(:,:,:) :: am_n,am_Tn,am_Vn,am_VVn,am_VTn
    real, allocatable, dimension(:,:,:,:) :: am_nn,am_Vnn
  contains
    ! Assignment operator
    procedure, public :: assign_saftvrmie_aij
    generic, public :: assignment(=) => assign_saftvrmie_aij
  end type saftvrmie_aij

  !> Container for hard-sphere diameter and differentials
  !> Also used for the Feynman--Hibbs D variable
  type :: saftvrmie_dhs
    !> Hard sphere diameter
    real, allocatable, dimension(:,:) :: d
    !> Temperature differential of hard sphere diameter
    real, allocatable, dimension(:,:) :: d_T
    !> Second temperature differential of hard sphere diameter
    real, allocatable, dimension(:,:) :: d_TT
  contains
    ! Assignment operator
    procedure, public :: assign_saftvrmie_dhs
    generic, public :: assignment(=) => assign_saftvrmie_dhs
  end type saftvrmie_dhs

  !> Container for zeta and differentials (also used for functions of zeta)
  type :: saftvrmie_zeta
    !> Hypotetical pure fluid packing fraction
    real :: zx
    !> Temperature differential of hypotetical pure fluid packing fraction
    real :: zx_T
    !> Second temperature differential of hypotetical pure fluid packing fraction
    real :: zx_TT
    !> Volume differential of hypotetical pure fluid packing fraction
    real :: zx_V
    !> Second volume differential of hypotetical pure fluid packing fraction
    real :: zx_VV
    !> Temperature and volume differential of hypotetical pure fluid packing fraction
    real :: zx_TV
    !> Mol number differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:) :: zx_n
    !> Mol number and volume differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:) :: zx_Vn
    !> Mol number and temperature differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:) :: zx_Tn
    !> Second mol number differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:,:) :: zx_nn
    !> Three time volume differential of hypotetical pure fluid packing fraction
    real :: zx_VVV
    !> Temperature and twice volume differential of hypotetical pure fluid packing fraction
    real :: zx_VVT
    !> Twice temperature and volume differential of hypotetical pure fluid packing fraction
    real :: zx_VTT
    !> Mol number, temperature and volume differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:) :: zx_VTn
    !> Mol number and twice volume and volume differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:) :: zx_VVn
    !> Volume and twice mol number and temperature differential of hypotetical pure fluid packing fraction
    real, allocatable, dimension(:,:) :: zx_Vnn
  contains
    ! Assignment operator
    procedure, public :: assign_saftvrmie_zeta
    generic, public :: assignment(=) => assign_saftvrmie_zeta
  end type saftvrmie_zeta

  !> Container for mu and zeta's (2 and 3). These are moments of the number density (2,3) and mu (1)
  type :: saftvrmie_zeta_hs
    !> Moments of the number density and mu
    real, dimension(3) :: zet
    !> Temperature differential of the moments of the number density and mu
    real, dimension(3) :: zet_T
    !> Second temperature differential of the moments of the number density and mu
    real, dimension(3) :: zet_TT
    !> Volume differential of the moments of the number density and mu
    real, dimension(3) :: zet_V
    !> Second volume differential of the moments of the number density and mu
    real, dimension(3) :: zet_VV
    !> Temperature and volume differential of the moments of the number density and mu
    real, dimension(3) :: zet_TV
    !> Mol number differential of the moments of the number density and mu
    real, allocatable, dimension(:,:) :: zet_n
    !> Mol number and volume differential of the moments of the number density and mu
    real, allocatable, dimension(:,:) :: zet_Vn
    !> Mol number and temperature differential of the moments of the number density and mu
    real, allocatable, dimension(:,:) :: zet_Tn
  contains
    ! Assignment operator
    procedure, public :: assign_saftvrmie_zeta_hs
    generic, public :: assignment(=) => assign_saftvrmie_zeta_hs
  end type saftvrmie_zeta_hs

  !> Container for SAFT-VR Mie common variables
  !! To be claculated only once per state
  type :: saftvrmie_var_container
    !> Hard sphere diameter
    type(saftvrmie_dhs) :: dhs
    !> Effective sigma for the quantum corrected potential
    type(saftvrmie_dhs) :: sigma_eff
    !> Effective epsilon for the quantum corrected potential
    type(saftvrmie_dhs) :: eps_divk_eff
    !> Feynman--Hibbs D variable
    type(saftvrmie_dhs) :: DFeynHibbsij
    !> Feynman--Hibbs D variable squared
    type(saftvrmie_dhs) :: D2FeynHibbsij
    !> Dimensionless van der Waals energy
    type(saftvrmie_dhs) :: alpha
    !> Moments of the number density for hs-module
    type(saftvrmie_zeta_hs) :: zeta_hs
    !> Hypotetical pure fluid packing fraction
    type(saftvrmie_zeta) :: zeta
    !> Packing fraction
    type(saftvrmie_zeta) :: zeta_bar
    !> Zeta used as prefactor in a3
    type(saftvrmie_zeta) :: zeta_a3
    !> Isothermal hard sphere compressibillity factor
    type(saftvrmie_zeta) :: Khs
    !> Hard sphere packing fraction
    type(saftvrmie_zeta) :: eta_hs
    !> Pure fluid reference HS diameter
    type(saftvrmie_zeta) :: d_pure
    !> Rho star
    type(saftvrmie_zeta) :: rho_star
    !> A1_ij
    type(saftvrmie_aij) :: a1ij
    !> Additive quantum corrections to A1_ij
    type(saftvrmie_aij) :: a1ijQCorr
    !> (A2/(1-chi))_ij
    type(saftvrmie_aij) :: a2chij
    !> Additive quantum corrections to A2chij
    type(saftvrmie_aij) :: a2chijQCorr
    !> A2_ij
    type(saftvrmie_aij) :: a2ij
    !> A3_ij
    type(saftvrmie_aij) :: a3ij
  contains
    ! Assignment operator
    procedure, public :: assign_saftvrmie_var_container
    generic, public :: assignment(=) => assign_saftvrmie_var_container
  end type saftvrmie_var_container

  type(saftvrmie_param_container), pointer :: saftvrmie_param => NULL()
  type(saftvrmie_var_container) :: svrmie_var ! ONLY USED FOR TESTING

  type, extends(base_eos_param) :: saftvrmie_eos
    type(saftvrmie_param_container), pointer :: saftvrmie_param => NULL()
    type(saftvrmie_var_container), pointer :: saftvrmie_var => NULL()
  contains
    procedure, public :: dealloc => saftvrmie_dealloc
    procedure, public :: allocate_and_init => saftvrmie_allocate_and_init
    ! Assignment operator
    procedure, pass(This), public :: assign_eos => assign_saftvrmie_eos
  end type saftvrmie_eos

  public :: saftvrmie_param_container, saftvrmie_var_container
  public :: saftvrmie_zeta, saftvrmie_dhs, saftvrmie_aij
  public :: svrmie_var, saftvrmie_param, saftvrmie_zeta_hs
  public :: init_saftvrmie_containers
  public :: mie_c_factor
  public :: add_second_saftvrmieaij_to_first
  public :: calcFunAlpha, calcVdWAlpha
  public :: allocate_saftvrmie_zeta, allocate_saftvrmie_zeta_hs, cleanup_saftvrmie_zeta, cleanup_saftvrmie_zeta_hs
  public :: allocate_saftvrmie_dhs, cleanup_saftvrmie_dhs
  public :: cleanup_saftvrmie_var_container, allocate_saftvrmie_var_container
  public :: get_saftvrmie_pure_fluid_param, set_saftvrmie_pure_fluid_param
  public :: get_saftvrmie_pure_fluid_deBoer, set_saftvrmie_pure_fluid_deBoer
  public :: get_saftvrmie_eps_kij, set_saftvrmie_eps_kij
  public :: get_saftvrmie_lr_gammaij, set_saftvrmie_lr_gammaij
  public :: set_saftvrmie_sigma_lij, get_saftvrmie_sigma_lij
  public :: calc_DFeynHibbsij, get_DFeynHibbsPower
  public :: calcBinaryMieParmeters
  public :: cleanup_saftvrmie_param_container
  public :: allocate_saftvrmie_param_container
  public :: saftvrmie_eos, get_saftvrmie_eos_pointer, get_saftvrmie_var

Contains

  !> Add variables of Y to those of X
  !! \author Ailo Aasen, March 2018
  subroutine add_second_saftvrmieaij_to_first(X, Y, i, j)
    type(saftvrmie_aij), intent(inout) :: X
    type(saftvrmie_aij), intent(in) :: Y
    integer, intent(in), optional :: i, j

    if (present(i) .and. present(j)) then
      X%am(i,j) = X%am(i,j) + Y%am(i,j)
      X%am_T(i,j) = X%am_T(i,j) + Y%am_T(i,j)
      X%am_V(i,j) = X%am_V(i,j) + Y%am_V(i,j)
      X%am_TT(i,j) = X%am_TT(i,j) + Y%am_TT(i,j)
      X%am_VV(i,j) = X%am_VV(i,j) + Y%am_VV(i,j)
      X%am_TV(i,j) = X%am_TV(i,j) + Y%am_TV(i,j)
      X%am_VVV(i,j) = X%am_VVV(i,j) + Y%am_VVV(i,j)
      X%am_VVT(i,j) = X%am_VVT(i,j) + Y%am_VVT(i,j)
      X%am_VTT(i,j) = X%am_VTT(i,j) + Y%am_VTT(i,j)
      X%am_n(:,i,j) = X%am_n(:,i,j) + Y%am_n(:,i,j)
      X%am_Tn(:,i,j) = X%am_Tn(:,i,j) + Y%am_Tn(:,i,j)
      X%am_Vn(:,i,j) = X%am_Vn(:,i,j) + Y%am_Vn(:,i,j)
      X%am_VVn(:,i,j) = X%am_VVn(:,i,j) + Y%am_VVn(:,i,j)
      X%am_VTn(:,i,j) = X%am_VTn(:,i,j) + Y%am_VTn(:,i,j)
      X%am_nn(:,:,i,j) = X%am_nn(:,:,i,j) + Y%am_nn(:,:,i,j)
      X%am_Vnn(:,:,i,j) = X%am_Vnn(:,:,i,j) + Y%am_Vnn(:,:,i,j)
    else
      X%am = X%am + Y%am
      X%am_T = X%am_T + Y%am_T
      X%am_V = X%am_V + Y%am_V
      X%am_TT = X%am_TT + Y%am_TT
      X%am_VV = X%am_VV + Y%am_VV
      X%am_TV = X%am_TV + Y%am_TV
      X%am_VVV = X%am_VVV + Y%am_VVV
      X%am_VVT = X%am_VVT + Y%am_VVT
      X%am_VTT = X%am_VTT + Y%am_VTT
      X%am_n = X%am_n + Y%am_n
      X%am_Tn = X%am_Tn + Y%am_Tn
      X%am_Vn = X%am_Vn + Y%am_Vn
      X%am_VVn = X%am_VVn + Y%am_VVn
      X%am_VTn = X%am_VTn + Y%am_VTn
      X%am_nn = X%am_nn + Y%am_nn
      X%am_Vnn = X%am_Vnn + Y%am_Vnn
    end if
  end subroutine add_second_saftvrmieaij_to_first

  !> Initialize the SAFT-VR Mie model
  !! See Lafitte 2013 (doi:10.1063/1.4819786)
  !! for model description
  subroutine init_saftvrmie_containers(nc,comp,eos,ref,mixing)
    use eosdata, only: eosSAFT_VR_MIE
    use thermopack_var, only: gendata_pointer
    use saftvrmie_parameters, only: getMieKij_allComps,getSaftVrMieParams
    integer, intent(in)           :: nc          !< Number of components.
    type(gendata_pointer), intent(inout)  :: comp(nc)    !< Component vector.
    class(saftvrmie_eos), intent(inout) :: eos
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    integer, intent(in), optional :: mixing      !< Binary combination rule id
    ! Locals
    integer :: fh_orders(nc), i, nthreads, err

    ! Deallocate old memory
    call eos%allocate_and_init(nc,"SAFT-VR Mie")
    saftvrmie_param => eos%saftvrmie_param
    call cleanup_saftvrmie_var_container(svrmie_var)
    call allocate_saftvrmie_var_container(nc,svrmie_var)
    ! Get interaction parameters
    call getMieKij_allComps(nc,comp,eosSAFT_VR_MIE,eos%saftvrmie_param%kij)
    eos%saftvrmie_param%gamma_ij = 0.0
    eos%saftvrmie_param%lij = 0.0
    ! Get component data
    call getSaftVrMieParams(nc,comp,eosSAFT_VR_MIE,ref,&
         eos%saftvrmie_param%comp,fh_orders=fh_orders)
    ! Set the correct Feynman--Hibbs order for the quantum corrections
    do i = 1, nc-1
      if (.not. fh_orders(i)==fh_orders(i+1)) call stoperror("init_saftvrmie_containers::fh_order must be equal for components")
    end do
    quantum_correction = fh_orders(1)
    quantum_correction_hs = fh_orders(1)
    ! Calculate mix parameters
    call calcBinaryMieParmeters(eos%saftvrmie_param,mixing)
    ! Add association.....

  end subroutine init_saftvrmie_containers

  !> Set pure fluid paramaters for SAFT-VR Mie
  subroutine set_saftvrmie_pure_fluid_param(ic,m,sigma,eps_depth_divk,lambda_a,lambda_r)
    integer, intent(in) :: ic          !< Component index
    real, intent(in)    :: m        !< [-]. Mean number of segments.
    real, intent(in)    :: sigma    !< [m]. Temperature-independent segment diameter.
    real, intent(in)    :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.
    real, intent(in)    :: lambda_a !< [] attractive exponent of the Mie potential
    real, intent(in)    :: lambda_r !< [] repulsive exponent of the Mie potential
    ! Locals
    saftvrmie_param%comp(ic)%m = m
    saftvrmie_param%comp(ic)%lambda_r = lambda_r
    saftvrmie_param%comp(ic)%lambda_a = lambda_a
    saftvrmie_param%comp(ic)%eps_depth_divk = eps_depth_divk
    saftvrmie_param%comp(ic)%sigma = sigma
    call calcBinaryMieParmeters(saftvrmie_param,1)
  end subroutine set_saftvrmie_pure_fluid_param

  !> Get pure fluid paramaters for SAFT-VR Mie
  subroutine get_saftvrmie_pure_fluid_param(ic,m,sigma,eps_depth_divk,lambda_a,lambda_r)
    integer, intent(in)  :: ic          !< Component index
    real, intent(out)    :: m        !< [-]. Mean number of segments.
    real, intent(out)    :: sigma    !< [m]. Temperature-independent segment diameter.
    real, intent(out)    :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.
    real, intent(out)    :: lambda_a !< [] attractive exponent of the Mie potential
    real, intent(out)    :: lambda_r !< [] repulsive exponent of the Mie potential
    ! Locals
    m = saftvrmie_param%comp(ic)%m
    lambda_r = saftvrmie_param%comp(ic)%lambda_r
    lambda_a = saftvrmie_param%comp(ic)%lambda_a
    eps_depth_divk = saftvrmie_param%comp(ic)%eps_depth_divk
    sigma = saftvrmie_param%comp(ic)%sigma
  end subroutine get_saftvrmie_pure_fluid_param


  !> Set the de Boer parameter Lambda, by adjusting the mass of the component
  !> \author Ailo Aasen, May 2018
  subroutine set_saftvrmie_pure_fluid_deBoer(ic,Lambda)
    use thermopack_constants, only: kB_const, h_const
    use numconstants, only: machine_prec
    integer, intent(in) :: ic          !< Component index
    real, intent(in)    :: Lambda        !< de Boer parameter [-]
    ! Locals
    real :: mass, sigma, eps
    if (Lambda<machine_prec) then
      call stoperror("set_saftvrmie_pure_fluid_deBoer::instead of setting Lambda=0, turn off FH corrections")
    end if
    eps = (saftvrmie_param%comp(ic)%eps_depth_divk) * kB_const
    sigma = saftvrmie_param%comp(ic)%sigma
    mass = h_const**2/((sigma**2)*(Lambda**2)*eps)
    saftvrmie_param%comp(ic)%mass = mass
    call calcBinaryMieParmeters(saftvrmie_param,1)
  end subroutine set_saftvrmie_pure_fluid_deBoer

  !> Calculate the de Boer parameter Lambda
  !> \author Ailo Aasen, May 2018
  subroutine get_saftvrmie_pure_fluid_deBoer(ic, Lambda)
    use thermopack_constants, only: kB_const, h_const
    integer, intent(in) :: ic          !< Component index
    real, intent(out)    :: Lambda        !< de Boer parameter [-]
    ! Locals
    real :: mass, sigma, eps
    mass = saftvrmie_param%comp(ic)%mass
    eps = (saftvrmie_param%comp(ic)%eps_depth_divk) * kB_const
    sigma = saftvrmie_param%comp(ic)%sigma
    Lambda = h_const/(sigma*sqrt(mass*eps))
  end subroutine get_saftvrmie_pure_fluid_deBoer

  !> Set the interaction parameter kij for the dispersive combining rule
  !> \author Ailo Aasen, October 2018
  subroutine set_saftvrmie_eps_kij(i,j,kij)
    integer, intent(in) :: i,j !< Component indices
    real, intent(in)    :: kij !< Dispersive interaction parameter [-]
    ! Locals
    saftvrmie_param%kij(i,j) = kij
    saftvrmie_param%kij(j,i) = saftvrmie_param%kij(i,j)
    call calcBinaryMieParmeters(saftvrmie_param,1)
  end subroutine set_saftvrmie_eps_kij

  !> Get the interaction parameter kij for the dispersive combining rule
  !> \author Ailo Aasen, October 2018
  subroutine get_saftvrmie_eps_kij(i,j,kij)
    integer, intent(in) :: i,j !< Component indices
    real, intent(out)   :: kij !< Dispersive interaction parameter [-]
    ! Locals
    kij = saftvrmie_param%kij(i,j)
  end subroutine get_saftvrmie_eps_kij

  !> Set the interaction parameter lij for the sigma combining rule
  subroutine set_saftvrmie_sigma_lij(i,j,lij)
    integer, intent(in) :: i,j !< Component indices
    real, intent(in)    :: lij !< sigma interaction parameter [-]
    ! Locals
    saftvrmie_param%lij(i,j) = lij
    saftvrmie_param%lij(j,i) = saftvrmie_param%lij(i,j)
    call calcBinaryMieParmeters(saftvrmie_param,1)
  end subroutine set_saftvrmie_sigma_lij

  !> Get the interaction parameter lij for the sigma combining rule
  subroutine get_saftvrmie_sigma_lij(i,j,lij)
    integer, intent(in) :: i,j !< Component indices
    real, intent(out)    :: lij !< sigma interaction parameter [-]
    ! Locals
    lij = saftvrmie_param%lij(i,j)
  end subroutine get_saftvrmie_sigma_lij

  !> Set the interaction parameter gammaij for the lambda_r combining rule
  !> \author Ailo Aasen, October 2018
  subroutine set_saftvrmie_lr_gammaij(i,j,gammaij)
    integer, intent(in) :: i,j !< Component indices
    real, intent(in)    :: gammaij !< lambda_r interaction parameter [-]
    ! Locals
    saftvrmie_param%gamma_ij(i,j) = gammaij
    saftvrmie_param%gamma_ij(j,i) = saftvrmie_param%gamma_ij(i,j)
    call calcBinaryMieParmeters(saftvrmie_param,1)
  end subroutine set_saftvrmie_lr_gammaij

  !> Get the interaction parameter gammaij for the lambda_r combining rule
  !> \author Ailo Aasen, October 2018
  subroutine get_saftvrmie_lr_gammaij(i,j,gammaij)
    integer, intent(in) :: i,j     !< Component indices
    real, intent(out)   :: gammaij !< lambda_r interaction parameter [-]
    ! Locals
    gammaij = saftvrmie_param%gamma_ij(i,j)
  end subroutine get_saftvrmie_lr_gammaij

  function mie_c_factor(lambda_r, lambda_a) result(C)
    !> Gives the part of the prefactor that depends on the exponent. The full
    !> prefactor is obtained by multiplying in the well-depth epsilon.
    implicit none
    real, intent(in) :: lambda_r !< repulsive exponent
    real, intent(in) :: lambda_a !< attractive exponent
    real :: C                    !< part of the prefactor
    ! locals
    real :: exponent

    exponent = lambda_a/(lambda_r-lambda_a)
    C = lambda_r/(lambda_r-lambda_a) * (lambda_r/lambda_a)**exponent
  end function mie_c_factor

  !> Calculate binary Mie parameters
  subroutine calcBinaryMieParmeters(saftvrmie_param,mixing)
    ! Input
    integer, intent(in) :: mixing
    ! In-out
    type(saftvrmie_param_container), intent(inout) :: saftvrmie_param
    ! Locals
    integer :: i, j, nc
    real :: f_alpha(6)
    nc = size(saftvrmie_param%comp)
    do i=1,nc
      do j=1,nc
        saftvrmie_param%lambda_a_ij(i,j) = calcBinaryLambda(saftvrmie_param%comp(i)%lambda_a,&
             saftvrmie_param%comp(j)%lambda_a,0.0)
        saftvrmie_param%lambda_r_ij(i,j) = calcBinaryLambda(saftvrmie_param%comp(i)%lambda_r,&
             saftvrmie_param%comp(j)%lambda_r,saftvrmie_param%gamma_ij(i,j))
        saftvrmie_param%sigma_ij(i,j) = calcBinarySigma(saftvrmie_param%comp(i)%sigma,&
             saftvrmie_param%comp(j)%sigma,saftvrmie_param%lij(i,j))
        saftvrmie_param%sigma_ij_cube(i,j) = saftvrmie_param%sigma_ij(i,j)**3
        saftvrmie_param%eps_divk_ij(i,j) = calcBinaryEps(&
             saftvrmie_param%comp(i)%eps_depth_divk,&
             saftvrmie_param%comp(j)%eps_depth_divk,saftvrmie_param%comp(i)%sigma,&
             saftvrmie_param%comp(j)%sigma,saftvrmie_param%sigma_ij(i,j),&
             saftvrmie_param%kij(i,j))
        saftvrmie_param%alpha_ij(i,j) = calcVdWAlpha(saftvrmie_param%lambda_a_ij(i,j),&
             saftvrmie_param%lambda_r_ij(i,j))
        call calcFunAlpha(saftvrmie_param%alpha_ij(i,j), f_alpha)
        saftvrmie_param%f_alpha_ij(:,i,j) = f_alpha
        saftvrmie_param%Cij(i,j) = mie_c_factor(saftvrmie_param%lambda_r_ij(i,j), &
             saftvrmie_param%lambda_a_ij(i,j))
        saftvrmie_param%DFeynHibbsParam_ij(i,j) = calcBinaryDFeynHibbsParam(saftvrmie_param%comp(i)%mass, &
             saftvrmie_param%comp(j)%mass)
        saftvrmie_param%Quantum_const_1a_ij(i,j)=calcBinary_Quantum_const_firstorder(&
             saftvrmie_param%lambda_a_ij(i,j))
        saftvrmie_param%Quantum_const_1r_ij(i,j)=calcBinary_Quantum_const_firstorder(&
             saftvrmie_param%lambda_r_ij(i,j))
        saftvrmie_param%Quantum_const_2a_ij(i,j)=calcBinary_Quantum_const_secondorder(&
             saftvrmie_param%lambda_a_ij(i,j))
        saftvrmie_param%Quantum_const_2r_ij(i,j)=calcBinary_Quantum_const_secondorder(&
             saftvrmie_param%lambda_r_ij(i,j))
      enddo
      saftvrmie_param%ms(i) = saftvrmie_param%comp(i)%m
    enddo
  end subroutine calcBinaryMieParmeters

  !> Calculate terms in second order quantum corrections
  function calcBinary_Quantum_const_secondorder(lambda) result(Q2)
    ! Input
    real, intent(in) :: lambda
    ! Output
    real :: Q2     ! Prefactor of terms in second order q-corr.
    ! locals
    real :: prefactor

    if (quantum_correction_spec==0) then        ! Feynman-Hibbs
      prefactor=0.5
    elseif (quantum_correction_spec==1) then    ! Jaen-Kahn
      prefactor=9.0/10.0
    else
      call stoperror("saftvrmie_containers::calcBinary_Quantum_const_secondorder: "//&
           "Specified undefined spec. for the second order quantum"//&
           "correction (0: Feynman Hibbs, 1: Jaen-Kahn)")
    end if

    Q2 = prefactor*lambda*(lambda-1.0)*(lambda+1.0)*(lambda+2.0)
  end function calcBinary_Quantum_const_secondorder

  !> Calculate terms in first order quantum corrections
  function calcBinary_Quantum_const_firstorder(lambda) result(Q1)
    ! Input
    real, intent(in) :: lambda
    ! Output
    real :: Q1     ! Prefactor of terms in first order q-corr.

    Q1= lambda*(lambda-1.0)
  end function calcBinary_Quantum_const_firstorder

  !> Calculate binary DFeynHibbsParam
  function calcBinaryDFeynHibbsParam(mass_i, mass_j) result(DParam)
    use numconstants, only: PI
    use thermopack_constants, only: kB_const, h_const
    ! Input
    real, intent(in) :: mass_i
    real, intent(in) :: mass_j
    ! Output
    real :: DParam
    ! Locals
    real :: reduced_mass, mass_product
    !
    mass_product = mass_i*mass_j
    if (mass_product > 0.0) then
      reduced_mass = mass_product/(mass_i+mass_j)
      DParam = (h_const**2)/(96*reduced_mass*kB_const*PI**2)
    else
      DParam = 0.0
    endif
  end function calcBinaryDFeynHibbsParam

  !> Calculate binary lambda
  function calcBinaryLambda(lambda_i, lambda_j, gamma_ij) result(lambda_ij)
    ! Input
    real, intent(in) :: lambda_i
    real, intent(in) :: lambda_j
    real, intent(in) :: gamma_ij
    ! Output
    real :: lambda_ij
    !
    lambda_ij = 3.0 + (1.0 - gamma_ij)*sqrt((lambda_i - 3.0)*(lambda_j - 3.0))
  end function calcBinaryLambda

  !> Calculate binary sigma
  function calcBinarySigma(sigma_i, sigma_j, lij) result(sigma_ij)
    ! Input
    real, intent(in) :: sigma_i
    real, intent(in) :: sigma_j
    real, intent(in) :: lij
    ! Output
    real :: sigma_ij
    !
    sigma_ij = 0.5*(sigma_i + sigma_j)*(1.0-lij)
  end function calcBinarySigma

  !> Calculate binary eps
  function calcBinaryEps(eps_i, eps_j, sigma_i, sigma_j, sigma_ij, kij) result(eps_ij)
    ! Input
    real, intent(in) :: eps_i
    real, intent(in) :: eps_j
    real, intent(in) :: sigma_i
    real, intent(in) :: sigma_j
    real, intent(in) :: sigma_ij
    real, intent(in) :: kij
    ! Output
    real :: eps_ij
    !
    if (use_epsrule_Lafitte) then ! standard combining rule of SAFT-VR Mie
      eps_ij = (1.0-kij)*sqrt(sigma_i**3*sigma_j**3)*sqrt(eps_i*eps_j)/sigma_ij**3
    else ! geometric mixing rule
      eps_ij = (1.0-kij)*sqrt(eps_i*eps_j)
    end if
  end function calcBinaryEps

  !> Calculate van der Waals attractive constant
  function calcVdWAlpha(lambda_a, lambda_r) result(alpha)
    !use saftvrmie_subroutines, only: mie_c_factor
    ! Input
    real, intent(in) :: lambda_a
    real, intent(in) :: lambda_r
    ! Output
    real :: alpha, C
    ! Locals
    C = mie_c_factor(lambda_r, lambda_a)
    alpha = C*(1.0/(lambda_a - 3.0) - 1.0/(lambda_r - 3.0))
  end function calcVdWAlpha

  !> Calculate f(alpha)
  subroutine calcFunAlpha(alpha, f, f_a, f_aa)
    ! Input
    real, intent(in) :: alpha !< van der Waals attarctive constant
    ! Output
    real, intent(out) :: f(6)
    real, optional, intent(out) :: f_a(6), f_aa(6)
    ! Locals
    real, parameter, dimension(0:6,1:6) :: phi = &
         reshape((/7.5365557, -37.60463, 71.745953, -46.83552, -2.467982, -0.50272, 8.0956883, &
         -359.44, 1825.6, -3168.0, 1884.2, -0.82376, -3.1935, 3.709, &
         1550.9, -5070.1, 6534.6, -3288.7, -2.7171, 2.0883, 0.0, &
         -1.19932, 9.063632, -17.9482, 11.34027, 20.52142, -56.6377, 40.53683, &
         -1911.28, 21390.175, -51320.7, 37064.54, 1103.742, -3264.61, 2556.181, &
         9236.9, -129430.0, 357230.0, -315530.0, 1390.2, -4518.2, 4241.6/), (/7,6/))
    integer :: i,j
    real :: alpha_n(0:3)
    real :: f_i_num, f_i_denum
    real :: m_a_i,m_aa_i,p_a(6),p_aa_i
    if (present(f_aa) .and. .not. present(f_a)) then
      call stoperror("calcFunAlpha: f_aa requires f_a")
    endif
    alpha_n(0) = 1.0
    do i=1,3
      alpha_n(i) = alpha*alpha_n(i-1)
    enddo
    do i=1,6
      f_i_num = sum(phi(0:3,i)*alpha_n(0:3))
      f_i_denum = 1.0 + sum(phi(4:6,i)*alpha_n(1:3))
      f(i) = f_i_num/f_i_denum
      if (present(f_a)) then
        m_a_i = 0.0
        do j=1,3
          m_a_i = m_a_i + j*phi(j,i)*alpha_n(j-1)
        enddo
        p_a(i) = 0.0
        do j=4,6
          p_a(i) = p_a(i) + (j-3)*phi(j,i)*alpha_n(j-4)
        enddo
        f_a(i) = (m_a_i - f(i)*p_a(i))/f_i_denum
      endif
      if (present(f_aa)) then
        m_aa_i = 0.0
        do j=2,3
          m_aa_i = m_aa_i + j*(j-1)*phi(j,i)*alpha_n(j-2)
        enddo
        p_aa_i = 0.0
        do j=5,6
          p_aa_i = p_aa_i + (j-3)*(j-4)*phi(j,i)*alpha_n(j-5)
        enddo
        f_aa(i) = (m_aa_i - 2.0*f_a(i)*p_a(i) - f(i)*p_aa_i)/f_i_denum
      endif
    enddo
  end subroutine calcFunAlpha

  !> Free allocated memory
  subroutine cleanup_saftvrmie_param_container(saftvrmie_pc)
    type(saftvrmie_param_container), intent(inout) :: saftvrmie_pc
    ! Locals
    integer :: ierr
    ! deallocate saftvrmie_pc
    if (allocated(saftvrmie_pc%comp)) then
      deallocate (saftvrmie_pc%comp, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%comp")
      endif
    endif
    if (allocated(saftvrmie_pc%kij)) then
      deallocate (saftvrmie_pc%kij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%kij")
      endif
    endif
    if (allocated(saftvrmie_pc%gamma_ij)) then
      deallocate (saftvrmie_pc%gamma_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%gamma_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%lij)) then
      deallocate (saftvrmie_pc%lij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%lij")
      endif
    endif
    if (allocated(saftvrmie_pc%alpha_ij)) then
      deallocate (saftvrmie_pc%alpha_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%alpha_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%f_alpha_ij)) then
      deallocate (saftvrmie_pc%f_alpha_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%f_alpha_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%lambda_a_ij)) then
      deallocate (saftvrmie_pc%lambda_a_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%lambda_a_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%lambda_r_ij)) then
      deallocate (saftvrmie_pc%lambda_r_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%lambda_r_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%sigma_ij)) then
      deallocate (saftvrmie_pc%sigma_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%sigma_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%sigma_ij_cube)) then
      deallocate (saftvrmie_pc%sigma_ij_cube, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%sigma_ij_cube")
      endif
    endif
    if (allocated(saftvrmie_pc%eps_divk_ij)) then
      deallocate (saftvrmie_pc%eps_divk_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%eps_divk_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%Cij)) then
      deallocate (saftvrmie_pc%Cij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%Cij")
      endif
    endif
    if (allocated(saftvrmie_pc%DFeynHibbsParam_ij)) then
      deallocate (saftvrmie_pc%DFeynHibbsParam_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%DFeynHibbsParam_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%Quantum_const_1a_ij)) then
      deallocate (saftvrmie_pc%Quantum_const_1a_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%Quantum_const_1a_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%Quantum_const_1r_ij)) then
      deallocate (saftvrmie_pc%Quantum_const_1r_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%Quantum_const_1r_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%Quantum_const_2a_ij)) then
      deallocate (saftvrmie_pc%Quantum_const_2a_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%Quantum_const_2a_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%Quantum_const_2r_ij)) then
      deallocate (saftvrmie_pc%Quantum_const_2r_ij, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%Quantum_const_2r_ij")
      endif
    endif
    if (allocated(saftvrmie_pc%ms)) then
      deallocate (saftvrmie_pc%ms, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_pc%ms")
      endif
    endif
  end subroutine cleanup_saftvrmie_param_container

  !> Free allocated saftvrmie_var_container memory
  subroutine cleanup_saftvrmie_var_container(saftvrmie_vc)
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc
    ! Locals
    call cleanup_saftvrmie_dhs(saftvrmie_vc%dhs)
    call cleanup_saftvrmie_dhs(saftvrmie_vc%alpha)
    call cleanup_saftvrmie_dhs(saftvrmie_vc%sigma_eff)
    call cleanup_saftvrmie_dhs(saftvrmie_vc%eps_divk_eff)
    call cleanup_saftvrmie_dhs(saftvrmie_vc%DFeynHibbsij)
    call cleanup_saftvrmie_dhs(saftvrmie_vc%D2FeynHibbsij)
    call cleanup_saftvrmie_zeta_hs(saftvrmie_vc%zeta_hs)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%zeta)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%zeta_bar)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%zeta_a3)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%Khs)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%eta_hs)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%d_pure)
    call cleanup_saftvrmie_zeta(saftvrmie_vc%rho_star)
    call cleanup_saftvrmie_aij(saftvrmie_vc%a1ij)
    call cleanup_saftvrmie_aij(saftvrmie_vc%a1ijQCorr)
    call cleanup_saftvrmie_aij(saftvrmie_vc%a2chij)
    call cleanup_saftvrmie_aij(saftvrmie_vc%a2chijQCorr)
    call cleanup_saftvrmie_aij(saftvrmie_vc%a2ij)
    call cleanup_saftvrmie_aij(saftvrmie_vc%a3ij)
  end subroutine cleanup_saftvrmie_var_container

  !> Free allocated saftvrmie_dhs memory
  subroutine cleanup_saftvrmie_dhs(saftvrmie_d)
    type(saftvrmie_dhs), intent(inout) :: saftvrmie_d
    ! Locals
    integer :: ierr
    if (allocated(saftvrmie_d%d)) then
      deallocate (saftvrmie_d%d, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_d%d")
      endif
    endif
    if (allocated(saftvrmie_d%d_T)) then
      deallocate (saftvrmie_d%d_T, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_d%d_T")
      endif
    endif
    if (allocated(saftvrmie_d%d_TT)) then
      deallocate (saftvrmie_d%d_TT, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_d%d_TT")
      endif
    endif
  end subroutine cleanup_saftvrmie_dhs

  !> Free allocated saftvrmie_zeta memory
  subroutine cleanup_saftvrmie_zeta(saftvrmie_z)
    type(saftvrmie_zeta), intent(inout) :: saftvrmie_z
    ! Locals
    integer :: ierr
    if (allocated(saftvrmie_z%zx_n)) then
      deallocate (saftvrmie_z%zx_n, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_n")
      endif
    endif
    if (allocated(saftvrmie_z%zx_Vn)) then
      deallocate (saftvrmie_z%zx_Vn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_Vn")
      endif
    endif
    if (allocated(saftvrmie_z%zx_Tn)) then
      deallocate (saftvrmie_z%zx_Tn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_Tn")
      endif
    endif
    if (allocated(saftvrmie_z%zx_nn)) then
      deallocate (saftvrmie_z%zx_nn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_nn")
      endif
    endif
    if (allocated(saftvrmie_z%zx_VTn)) then
      deallocate (saftvrmie_z%zx_VTn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_VTn")
      endif
    endif
    if (allocated(saftvrmie_z%zx_VVn)) then
      deallocate (saftvrmie_z%zx_VVn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_VVn")
      endif
    endif
    if (allocated(saftvrmie_z%zx_Vnn)) then
      deallocate (saftvrmie_z%zx_Vnn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zx_Vnn")
      endif
    endif
  end subroutine cleanup_saftvrmie_zeta

  !> Free allocated saftvrmie_zeta memory
  subroutine cleanup_saftvrmie_zeta_hs(saftvrmie_z)
    type(saftvrmie_zeta_hs), intent(inout) :: saftvrmie_z
    ! Locals
    integer :: ierr
    if (allocated(saftvrmie_z%zet_n)) then
      deallocate (saftvrmie_z%zet_n, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zet_n")
      endif
    endif
    if (allocated(saftvrmie_z%zet_Vn)) then
      deallocate (saftvrmie_z%zet_Vn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zet_Vn")
      endif
    endif
    if (allocated(saftvrmie_z%zet_Tn)) then
      deallocate (saftvrmie_z%zet_Tn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_z%zet_Tn")
      endif
    endif
  end subroutine cleanup_saftvrmie_zeta_hs

  !> Free allocated saftvrmie_aij memory
  subroutine cleanup_saftvrmie_aij(saftvrmie_a)
    type(saftvrmie_aij), intent(inout) :: saftvrmie_a
    ! Locals
    integer :: ierr
    if (allocated(saftvrmie_a%am)) then
      deallocate (saftvrmie_a%am, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am")
      endif
    endif
    if (allocated(saftvrmie_a%am_T)) then
      deallocate (saftvrmie_a%am_T, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_T")
      endif
    endif
    if (allocated(saftvrmie_a%am_V)) then
      deallocate (saftvrmie_a%am_V, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_V")
      endif
    endif
    if (allocated(saftvrmie_a%am_TT)) then
      deallocate (saftvrmie_a%am_TT, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_TT")
      endif
    endif
    if (allocated(saftvrmie_a%am_VV)) then
      deallocate (saftvrmie_a%am_VV, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_VV")
      endif
    endif
    if (allocated(saftvrmie_a%am_TV)) then
      deallocate (saftvrmie_a%am_TV, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_TV")
      endif
    endif
    if (allocated(saftvrmie_a%am_n)) then
      deallocate (saftvrmie_a%am_n, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_n")
      endif
    endif
    if (allocated(saftvrmie_a%am_Vn)) then
      deallocate (saftvrmie_a%am_Vn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_Vn")
      endif
    endif
    if (allocated(saftvrmie_a%am_Tn)) then
      deallocate (saftvrmie_a%am_Tn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_Tn")
      endif
    endif
    if (allocated(saftvrmie_a%am_nn)) then
      deallocate (saftvrmie_a%am_nn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_nn")
      endif
    endif
    if (allocated(saftvrmie_a%am_VTn)) then
      deallocate (saftvrmie_a%am_VTn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_VTn")
      endif
    endif
    if (allocated(saftvrmie_a%am_VVn)) then
      deallocate (saftvrmie_a%am_VVn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_VVn")
      endif
    endif
    if (allocated(saftvrmie_a%am_Vnn)) then
      deallocate (saftvrmie_a%am_Vnn, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_Vnn")
      endif
    endif
    if (allocated(saftvrmie_a%am_VVV)) then
      deallocate (saftvrmie_a%am_VVV, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_VVV")
      endif
    endif
    if (allocated(saftvrmie_a%am_VVT)) then
      deallocate (saftvrmie_a%am_VVT, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_VVT")
      endif
    endif
    if (allocated(saftvrmie_a%am_VTT)) then
      deallocate (saftvrmie_a%am_VTT, STAT=ierr)
      if (ierr /= 0) then
        call stoperror("saftvrmie_interface::cleanup_saftvrmie: Not able to deallocate saftvrmie_a%am_VTT")
      endif
    endif
  end subroutine cleanup_saftvrmie_aij

  !> Allocated memory structures
  subroutine allocate_saftvrmie_param_container(nc,saftvrmie_pc)
    ! Input
    integer, intent(in) :: nc
    type(saftvrmie_param_container), intent(inout) :: saftvrmie_pc
    ! Locals
    integer :: ierr
    allocate (saftvrmie_pc%comp(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%comp")
    endif
    allocate (saftvrmie_pc%kij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%kij")
    endif
    allocate (saftvrmie_pc%gamma_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%gamma_ij")
    endif
    allocate (saftvrmie_pc%lij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%lij")
    endif
    allocate (saftvrmie_pc%alpha_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%alpha_ij")
    endif
    allocate (saftvrmie_pc%f_alpha_ij(6,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%f_alpha_ij")
    endif
    allocate (saftvrmie_pc%lambda_a_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%lambda_a_ij")
    endif
    allocate (saftvrmie_pc%lambda_r_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%lambda_r_ij")
    endif
    allocate (saftvrmie_pc%sigma_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%sigma_ij")
    endif
    allocate (saftvrmie_pc%sigma_ij_cube(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%sigma_ij_cube")
    endif
    allocate (saftvrmie_pc%eps_divk_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%eps_divk_ij")
    endif
    allocate (saftvrmie_pc%Cij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%Cij")
    endif
    allocate (saftvrmie_pc%DFeynHibbsParam_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%DFeynHibbsParam_ij")
    endif
    allocate (saftvrmie_pc%Quantum_const_1a_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%Quantum_const_1a_ij")
    endif
    allocate (saftvrmie_pc%Quantum_const_1r_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%Quantum_const_1r_ij")
    endif
    allocate (saftvrmie_pc%Quantum_const_2a_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%Quantum_const_2a_ij")
    endif
    allocate (saftvrmie_pc%Quantum_const_2r_ij(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%Quantum_const_2r_ij")
    endif
    allocate (saftvrmie_pc%ms(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_pc%ms")
    endif
  end subroutine allocate_saftvrmie_param_container

  !> Allocated saftvrmie_var_container memory
  subroutine allocate_saftvrmie_var_container(nc,saftvrmie_vc)
    ! Input
    integer, intent(in) :: nc
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc
    ! Locals
    call allocate_saftvrmie_dhs(nc,saftvrmie_vc%dhs)
    call allocate_saftvrmie_dhs(nc,saftvrmie_vc%sigma_eff)
    call allocate_saftvrmie_dhs(nc,saftvrmie_vc%eps_divk_eff)
    call allocate_saftvrmie_dhs(nc,saftvrmie_vc%DFeynHibbsij)
    call allocate_saftvrmie_dhs(nc,saftvrmie_vc%D2FeynHibbsij)
    call allocate_saftvrmie_dhs(nc,saftvrmie_vc%alpha)
    call allocate_saftvrmie_zeta_hs(nc,saftvrmie_vc%zeta_hs)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%zeta)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%zeta_bar)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%zeta_a3)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%Khs)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%eta_hs)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%d_pure)
    call allocate_saftvrmie_zeta(nc,saftvrmie_vc%rho_star)
    call allocate_saftvrmie_aij(nc,saftvrmie_vc%a1ij)
    call allocate_saftvrmie_aij(nc,saftvrmie_vc%a1ijQCorr)
    call allocate_saftvrmie_aij(nc,saftvrmie_vc%a2chij)
    call allocate_saftvrmie_aij(nc,saftvrmie_vc%a2chijQCorr)
    call allocate_saftvrmie_aij(nc,saftvrmie_vc%a2ij)
    call allocate_saftvrmie_aij(nc,saftvrmie_vc%a3ij)
  end subroutine allocate_saftvrmie_var_container

  !> Allocated saftvrmie_var_container memory
  subroutine allocate_saftvrmie_dhs(nc,saftvrmie_d)
    ! Input
    integer, intent(in) :: nc
    type(saftvrmie_dhs), intent(inout) :: saftvrmie_d
    ! Locals
    integer :: ierr
    allocate (saftvrmie_d%d(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_d%d")
    endif
    allocate (saftvrmie_d%d_T(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_d%d_T")
    endif
    allocate (saftvrmie_d%d_TT(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_d%d_TT")
    endif
  end subroutine allocate_saftvrmie_dhs

  !> Allocated saftvrmie_zeta memory and initialize to zero
  subroutine allocate_saftvrmie_zeta(nc,saftvrmie_z)
    ! Input
    integer, intent(in) :: nc
    type(saftvrmie_zeta), intent(inout) :: saftvrmie_z
    ! Locals
    integer :: ierr
    saftvrmie_z%zx = 0.0
    saftvrmie_z%zx_T = 0.0
    saftvrmie_z%zx_TT = 0.0
    saftvrmie_z%zx_V = 0.0
    saftvrmie_z%zx_VV = 0.0
    saftvrmie_z%zx_TV = 0.0
    saftvrmie_z%zx_VVV = 0.0
    saftvrmie_z%zx_VVT = 0.0
    saftvrmie_z%zx_VTT = 0.0
    allocate (saftvrmie_z%zx_n(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_n")
    endif
    saftvrmie_z%zx_n = 0.0
    allocate (saftvrmie_z%zx_Tn(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_Tn")
    endif
    saftvrmie_z%zx_Tn = 0.0
    allocate (saftvrmie_z%zx_Vn(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_Vn")
    endif
    saftvrmie_z%zx_Vn = 0.0
    allocate (saftvrmie_z%zx_nn(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_nn")
    endif
    saftvrmie_z%zx_nn = 0.0
    allocate (saftvrmie_z%zx_VVn(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_VVn")
    endif
    saftvrmie_z%zx_VVn = 0.0
    allocate (saftvrmie_z%zx_VTn(nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_VTn")
    endif
    saftvrmie_z%zx_VTn = 0.0
    allocate (saftvrmie_z%zx_Vnn(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zx_Vnn")
    endif
    saftvrmie_z%zx_Vnn = 0.0
  end subroutine allocate_saftvrmie_zeta

  !> Allocated saftvrmie_zeta memory and initialize to zero
  subroutine allocate_saftvrmie_zeta_hs(nc,saftvrmie_z)
    ! Input
    integer, intent(in) :: nc
    type(saftvrmie_zeta_hs), intent(inout) :: saftvrmie_z
    ! Locals
    integer :: ierr
    saftvrmie_z%zet = 0.0
    saftvrmie_z%zet_T = 0.0
    saftvrmie_z%zet_TT = 0.0
    saftvrmie_z%zet_V = 0.0
    saftvrmie_z%zet_VV = 0.0
    saftvrmie_z%zet_TV = 0.0
    allocate (saftvrmie_z%zet_n(nc,3), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zet_n")
    endif
    saftvrmie_z%zet_n = 0.0
    allocate (saftvrmie_z%zet_Tn(nc,3), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zet_Tn")
    endif
    saftvrmie_z%zet_Tn = 0.0
    allocate (saftvrmie_z%zet_Vn(nc,3), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie: Not able to allocate saftvrmie_z%zet_Vn")
    endif
    saftvrmie_z%zet_Vn = 0.0
  end subroutine allocate_saftvrmie_zeta_hs

  !> Allocat saftvrmie_aij memory
  subroutine allocate_saftvrmie_aij(nc,saftvrmie_a)
    ! Input
    integer, intent(in) :: nc
    type(saftvrmie_aij), intent(inout) :: saftvrmie_a
    ! Locals
    integer :: ierr
    allocate (saftvrmie_a%am(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am")
    endif
    allocate (saftvrmie_a%am_T(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_T")
    endif
    allocate (saftvrmie_a%am_V(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_V")
    endif
    allocate (saftvrmie_a%am_TT(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_TT")
    endif
    allocate (saftvrmie_a%am_VV(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_VV")
    endif
    allocate (saftvrmie_a%am_TV(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_TV")
    endif
    allocate (saftvrmie_a%am_n(nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_n")
    endif
    allocate (saftvrmie_a%am_Vn(nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_Vn")
    endif
    allocate (saftvrmie_a%am_Tn(nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_Tn")
    endif
    allocate (saftvrmie_a%am_nn(nc,nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_nn")
    endif
    allocate (saftvrmie_a%am_VTn(nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_VTn")
    endif
    allocate (saftvrmie_a%am_VVn(nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_VVn")
    endif
    allocate (saftvrmie_a%am_Vnn(nc,nc,nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_Vnn")
    endif
    allocate (saftvrmie_a%am_VVV(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_VVV")
    endif
    allocate (saftvrmie_a%am_VVT(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_VVT")
    endif
    allocate (saftvrmie_a%am_VTT(nc,nc), STAT=ierr)
    if (ierr /= 0) then
      call stoperror("saftvrmie_interface::allocate_saftvrmie_aij: Not able to allocate saftvrmie_a%am_VTT")
    endif
  end subroutine allocate_saftvrmie_aij

  subroutine calc_DFeynHibbsij(nc, T, DParam, D, D2)
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: DParam(nc, nc)
    type(saftvrmie_dhs), intent(inout) :: D, D2
    ! Locals
    real :: Tinv, dTinv, ddTinv

    Tinv = 1/T ! =1/T
    dTinv = -Tinv*Tinv ! =-1/T^2
    ddTinv = -2*Tinv*dTinv ! =2/T^3

    D%d   = DParam*Tinv
    D%d_T = DParam*dTinv
    D%d_TT= DParam*ddTinv

    D2%d   = (D%d)*(D%d)
    D2%d_T = 2*(D%d)*(D%d_T)
    D2%d_TT= 2*((D%d)*(D%d_TT) + (D%d_T)*(D%d_T))
  end subroutine calc_DFeynHibbsij

  !> Retrieve the (T-dependent) Feynman--Hibbs D quantity, or D**power, optionally
  !> divided by sigma^(2*power).
  !> \author Ailo Aasen, March 2018
  subroutine get_DFeynHibbsPower(i,j,D,D_T,D_TT,s_vc,power_in,divideBySigmaMie)
    integer, intent(in) :: i,j
    real, intent(out) :: D, D_T, D_TT          !< Quantum par. and derivatives
    type(saftvrmie_var_container), intent(in) :: s_vc
    integer, intent(in), optional :: power_in
    logical, intent(in), optional :: divideBySigmaMie !< Scale D by sigma^2 or not
    ! Locals
    real :: sigmaMieInv, power

    if (present(power_in)) then
      power = power_in
    else
      power = 1
    end if

    if (power==1) then
      D = s_vc%DFeynHibbsij%D(i, j)
      D_T = s_vc%DFeynHibbsij%D_T(i, j)
      D_TT = s_vc%DFeynHibbsij%D_TT(i, j)
    else if (power==2) then
      D = s_vc%D2FeynHibbsij%D(i, j)
      D_T = s_vc%D2FeynHibbsij%D_T(i, j)
      D_TT = s_vc%D2FeynHibbsij%D_TT(i, j)
    else if (power==3) then
      D = s_vc%D2FeynHibbsij%D(i, j)*s_vc%DFeynHibbsij%D(i, j)
      D_T = s_vc%D2FeynHibbsij%D_T(i, j)*s_vc%DFeynHibbsij%D(i, j) &
           + s_vc%D2FeynHibbsij%D(i, j)*s_vc%DFeynHibbsij%D_T(i, j)
      D_TT = s_vc%D2FeynHibbsij%D_TT(i, j)*s_vc%DFeynHibbsij%D(i, j) &
           + 2*s_vc%D2FeynHibbsij%D_T(i, j)**2 &
           + s_vc%D2FeynHibbsij%D_TT(i, j)*s_vc%DFeynHibbsij%D(i, j)
    else if (power==4) then
      D = s_vc%D2FeynHibbsij%D(i, j)**2
      D_T = 2*s_vc%D2FeynHibbsij%D_T(i, j)*s_vc%D2FeynHibbsij%D(i, j)
      D_TT = 2*s_vc%D2FeynHibbsij%D_TT(i, j)*s_vc%D2FeynHibbsij%D(i, j) &
           + 2*s_vc%D2FeynHibbsij%D_T(i, j)**2
    else
      call stoperror("Wrong power")
    end if

    if (present(divideBySigmaMie)) then
      if (divideBySigmaMie) then
        ! Returns (D/sigma^2)^power, also known as (Dmod/T)^power
        sigmaMieInv = saftvrmie_param%sigma_ij(i, j)**(-2*power)
        D = D*sigmaMieInv
        D_T = D_T*sigmaMieInv
        D_TT = D_TT*sigmaMieInv
      end if
    end if

  end subroutine get_DFeynHibbsPower

  subroutine saftvrmie_allocate_and_init(eos,nc,eos_label)
    class(saftvrmie_eos), intent(inout) :: eos
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label !< EOS label
    ! Locals
    integer :: stat
    call eos%dealloc()
    allocate(eos%saftvrmie_param,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to allocate saftvrmie_param")
    call allocate_saftvrmie_param_container(nc,eos%saftvrmie_param)
    allocate(eos%saftvrmie_var,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to allocate saftvrmie_var")
    call allocate_saftvrmie_var_container(nc,eos%saftvrmie_var)
  end subroutine saftvrmie_allocate_and_init

  subroutine saftvrmie_dealloc(eos)
    class(saftvrmie_eos), intent(inout) :: eos
    ! Locals
    integer :: stat
    if (associated(eos%saftvrmie_param)) then
      call cleanup_saftvrmie_param_container(eos%saftvrmie_param)
      deallocate(eos%saftvrmie_param,stat=stat)
      if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate saftvrmie_param")
      eos%saftvrmie_param => NULL()
    endif
    if (associated(eos%saftvrmie_var)) then
      call cleanup_saftvrmie_var_container(eos%saftvrmie_var)
      deallocate(eos%saftvrmie_var,stat=stat)
      if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate saftvrmie_var")
      eos%saftvrmie_var => NULL()
    endif
  end subroutine saftvrmie_dealloc

  subroutine assign_saftvrmie_eos(this,other)
    class(saftvrmie_eos), intent(out) :: this
    class(*), intent(in) :: other
    ! Locals
    integer :: nc
    select type (other)
    class is (saftvrmie_eos)
      call this%assign_base_eos_param(other)
      if (allocated(other%saftvrmie_param%comp)) then
        nc = size(other%saftvrmie_param%comp)
      else
        return
      endif
      call this%allocate_and_init(nc,"SAFT-VR Mie")
      this%saftvrmie_param => other%saftvrmie_param
      this%saftvrmie_var = other%saftvrmie_var
    class default
    end select

  end subroutine assign_saftvrmie_eos

  subroutine assign_saftvrmie_param_container(this,other)
    class(saftvrmie_param_container), intent(inout) :: this
    class(saftvrmie_param_container), intent(in) :: other
    !
    this%comp = other%comp
    this%kij = other%kij
    this%gamma_ij = other%gamma_ij
    this%lij = other%lij
    this%alpha_ij = other%alpha_ij
    this%f_alpha_ij = other%f_alpha_ij
    this%lambda_a_ij = other%lambda_a_ij
    this%lambda_r_ij = other%lambda_r_ij
    this%sigma_ij = other%sigma_ij
    this%eps_divk_ij = other%eps_divk_ij
    this%Cij = other%Cij
    this%DFeynHibbsParam_ij = other%DFeynHibbsParam_ij
    this%ms = other%ms
    this%sigma_ij_cube = other%sigma_ij_cube
    this%Quantum_const_1a_ij = other%Quantum_const_1a_ij
    this%Quantum_const_1r_ij = other%Quantum_const_1r_ij
    this%Quantum_const_2a_ij = other%Quantum_const_2a_ij
    this%Quantum_const_2r_ij = other%Quantum_const_2r_ij
  end subroutine assign_saftvrmie_param_container

  subroutine assign_saftvrmie_var_container(this,other)
    class(saftvrmie_var_container), intent(inout) :: this
    class(saftvrmie_var_container), intent(in) :: other
    !
    this%dhs = this%dhs
    this%sigma_eff = this%sigma_eff
    this%eps_divk_eff = this%eps_divk_eff
    this%DFeynHibbsij = this%DFeynHibbsij
    this%D2FeynHibbsij = this%D2FeynHibbsij
    this%alpha = this%alpha
    this%zeta_hs = this%zeta_hs
    this%zeta = this%zeta
    this%zeta_bar = this%zeta_bar
    this%zeta_a3 = this%zeta_a3
    this%Khs = this%Khs
    this%eta_hs = this%eta_hs
    this%d_pure = this%d_pure
    this%rho_star = this%rho_star
    this%a1ij = this%a1ij
    this%a1ijQCorr = this%a1ijQCorr
    this%a2chij = this%a2chij
    this%a2chijQCorr = this%a2chijQCorr
    this%a2ij = this%a2ij
    this%a3ij = this%a3ij
  end subroutine assign_saftvrmie_var_container

  subroutine assign_saftvrmie_aij(this,other)
    class(saftvrmie_aij), intent(inout) :: this
    class(saftvrmie_aij), intent(in) :: other
    !
    this%am = other%am
    this%am_T = other%am_T
    this%am_V = other%am_V
    this%am_TT = other%am_TT
    this%am_VV = other%am_VV
    this%am_TV = other%am_TV
    this%am_VVV = other%am_VVV
    this%am_VVT = other%am_VVT
    this%am_VTT = other%am_VTT
    this%am_n = other%am_n
    this%am_Tn = other%am_Tn
    this%am_Vn = other%am_Vn
    this%am_VVn = other%am_VVn
    this%am_VTn = other%am_VTn
    this%am_nn = other%am_nn
    this%am_Vnn = other%am_Vnn
  end subroutine assign_saftvrmie_aij

  subroutine assign_saftvrmie_dhs(this,other)
    class(saftvrmie_dhs), intent(inout) :: this
    class(saftvrmie_dhs), intent(in) :: other
    !
    this%d = other%d
    this%d_T = other%d_T
    this%d_TT = other%d_TT
  end subroutine assign_saftvrmie_dhs

  subroutine assign_saftvrmie_zeta(this,other)
    class(saftvrmie_zeta), intent(inout) :: this
    class(saftvrmie_zeta), intent(in) :: other
    !
    this%zx = other%zx
    this%zx_T = other%zx_T
    this%zx_TT = other%zx_TT
    this%zx_V = other%zx_V
    this%zx_VV = other%zx_VV
    this%zx_TV = other%zx_TV
    this%zx_n = other%zx_n
    this%zx_Vn = other%zx_Vn
    this%zx_Tn = other%zx_Tn
    this%zx_nn = other%zx_nn
    this%zx_VVV = other%zx_VVV
    this%zx_VVT = other%zx_VVT
    this%zx_VTT = other%zx_VTT
    this%zx_VTn = other%zx_VTn
    this%zx_VVn = other%zx_VVn
    this%zx_Vnn = other%zx_Vnn
  end subroutine assign_saftvrmie_zeta

  subroutine assign_saftvrmie_zeta_hs(this,other)
    class(saftvrmie_zeta_hs), intent(inout) :: this
    class(saftvrmie_zeta_hs), intent(in) :: other
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
  end subroutine assign_saftvrmie_zeta_hs

  function get_saftvrmie_eos_pointer(base_eos) result(eos)
    class(base_eos_param), pointer, intent(in) :: base_eos
    class(saftvrmie_eos), pointer :: eos
    eos => NULL()
    if (.not. associated(base_eos)) return
    select type(p_eos => base_eos)
    type is (saftvrmie_eos)
      eos => p_eos
    class default
      call stoperror("Error casting to saftvrmie_eos")
    end select
  end function get_saftvrmie_eos_pointer

  function get_saftvrmie_var() result(svrm_var)
    type(saftvrmie_var_container), pointer :: svrm_var
    !
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type(p_eos => eos)
    type is (saftvrmie_eos)
      svrm_var => p_eos%saftvrmie_var
    class default
      svrm_var => NULL()
    end select
  end function get_saftvrmie_var

end module saftvrmie_containers
