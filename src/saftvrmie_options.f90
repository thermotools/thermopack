!---------------------------------------------------------------------
! Module and subroutines for the Quatum-SAFT-VR-Mie (QSAFT-VR-MIE)
! Equation of State implmented in Thermopack.
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
! © SINTEF Energy Research. All rights reserved.
!---------------------------------------------------------------------

module saftvrmie_options
  implicit none
  public
  save

  !-------------------------------------------------------------------
  ! Quantum correction parameters
  integer :: quantum_correction = 0       ! Specifies order of quantum corr. included
  integer :: quantum_correction_hs = 0    ! Specifies the Q-corr of the reference hard-sphere
  integer :: quantum_correction_spec = 0  ! 0: Feynman-Hibbs, 1: Jaen-Kahn
  logical :: quantum_correct_A2 = .true.  ! Include quantum correction to A2


  logical :: use_epsrule_Lafitte = .true. ! Use the Lafitte combining rule for epsilon
  logical :: exact_binary_dhs=.false.     ! If false, use the Lafitte approximation dij=(di+dj)/2
  logical :: exact_crosspot_eff=.true.    ! Use exact sigma_eff and eps_eff for cross interaction (doesn't seem to improve much, and slows the code down)
  integer, parameter :: ZETA_LAFITTE=1, ZETA_LINEAR=2, ZETA_LEONARD=3
  integer :: zeta_mixing_rule = ZETA_LAFITTE   ! Specifies packing fraction treatement for mixtures (LAFITTE or LINEAR)

  !-------------------------------------------------------------------
  ! Flag for switching on hard-sphere EoS
  integer, parameter :: HS_EOS_ORIGINAL = 0, &
       HS_EOS_SANTOS = 1, &
       HS_EOS_PURE_DIJ = 2
  integer :: hardsphere_EoS = HS_EOS_ORIGINAL
  integer, parameter :: KHS_EOS_LAFITTE = 0, & ! Carnahan-Starling
       KHS_EOS_BMCSL = 1, & ! Boublik-Mansoori-Carnahan-Starling-Leland
       KHS_EOS_SANTOS = 2 ! Non-additive Santos
  integer :: Khs_EoS = KHS_EOS_LAFITTE

  ! Flag for switching on which pure hard-sphere EoS is used with HS_EOS_SANTOS
  integer, parameter :: PURE_HS_CSK = 0, PURE_HS_CS = 1
  integer :: pure_hs_EoS = PURE_HS_CS
  logical :: enable_hs_extra = .false.  !> Option to enable/disable extra-term to hard-sphere reference

  !-------------------------------------------------------------------
  ! Dispersion terms
  integer, parameter :: A3_LAFITTE = 1, A3_SIJ_PREFAC = 2
  integer :: a3_model = A3_LAFITTE

  logical :: enable_hs = .true.  !> Option to enable/disable hard-sphere contribution
  logical :: enable_A1 = .true.  !> Option to enable/disable A1 contribution
  logical :: enable_A3 = .true.  !> Option to enable/disable A3 contribution
  logical :: enable_A2 = .true.  !> Option to enable/disable A2 contribution
  logical :: enable_chain = .true. !> Option to enable/disable chain contribution

  !-------------------------------------------------------------------
  ! Truncation and shift corrections
  logical :: enable_truncation_correction = .false. !> Option to enable/disable truncation correction
  logical :: enable_shift_correction = .false. !> Option to enable/disable shift correction
  real    :: r_cut = 3.5 !> Truncation radius

  !-------------------------------------------------------------------
  ! Overall model selections
  integer, parameter :: LAFITTE=1, QSAFT_FH1=2, QSAFT_FH2=3
  integer, parameter :: LAFITTE_HS_REF=1, SINGLE_COMP_HS_REF=2
  integer, parameter :: ADDITIVE_HS_REF=3, NON_ADD_HS_REF=4

contains

  !> Model options....
  !! \author Morten Hammer, January 2019
  subroutine saftvrmieaij_model_options(model, hs_reference)
    integer, intent(in) :: model
    integer, optional, intent(in) :: hs_reference
    select case(model)
    case(LAFITTE)
      call set_Lafitte_option()
    case(QSAFT_FH1)
      call set_Lafitte_option()
      quantum_correction = 1
      quantum_correction_hs = 1
      quantum_correction_spec = 0
    case(QSAFT_FH2)
      call set_Lafitte_option()
      quantum_correction = 2
      quantum_correction_hs = 2
      quantum_correction_spec = 0
    case default
      call stoperror("Unknown model options for SAFT-VR Mie")
    end select
    if (present(hs_reference)) then
      ! Override HS reference
      select case(hs_reference)
      case(LAFITTE_HS_REF)
        ! As already set
      case(SINGLE_COMP_HS_REF)
        hardsphere_EoS = HS_EOS_PURE_DIJ
        zeta_mixing_rule = ZETA_LEONARD
        exact_binary_dhs = .true.
      case(ADDITIVE_HS_REF)
        hardsphere_EoS = HS_EOS_ORIGINAL
        zeta_mixing_rule = ZETA_LAFITTE
        exact_binary_dhs = .false.
        enable_hs_extra = .true.
      case(NON_ADD_HS_REF)
        hardsphere_EoS = HS_EOS_SANTOS
        pure_hs_EoS = PURE_HS_CS
        exact_binary_dhs = .true.
      case default
        call stoperror("Unknown HS model options for SAFT-VR Mie")
      end select
    endif
  contains
    subroutine set_Lafitte_option()
      use_epsrule_Lafitte = .true.
      exact_binary_dhs = .false.
      zeta_mixing_rule = ZETA_LAFITTE
      exact_crosspot_eff = .true.
      hardsphere_EoS = HS_EOS_ORIGINAL
      a3_model = A3_LAFITTE
      enable_hs = .true.
      enable_A1 = .true.
      enable_A3 = .true.
      enable_A2 = .true.
      enable_chain = .true.
      enable_truncation_correction = .false.
      enable_hs_extra = .false.
      quantum_correction = 0
      quantum_correction_hs = 0
      quantum_correction_spec = 0
      quantum_correct_A2 = .true.
      Khs_EoS = KHS_EOS_LAFITTE
    end subroutine set_Lafitte_option
  end subroutine saftvrmieaij_model_options

  !> Set r_cut, and enable truncation correction
  !!
  !! \author Morten Hammer, November 2018
  subroutine set_r_cut(r_c)
    real, intent(in) :: r_c ! Potential cut-off radius
    r_cut = r_c
    enable_truncation_correction = .true.
  end subroutine set_r_cut

  !> Enable/disable truncation and shift correction
  !!
  !! \author Morten Hammer, November 2018
  subroutine truncation_correction_model_control(truncation,shift)
    logical, intent(in) :: truncation,shift
    enable_truncation_correction = truncation
    enable_shift_correction = shift
  end subroutine truncation_correction_model_control

  !> Check that model parameters are consistent
  !!
  !! \author Morten Hammer, March 2019
  subroutine check_model_consitency()
    !SINGLE_COMP_HS_REF
    if (  hardsphere_EoS == HS_EOS_PURE_DIJ .and. &
         (zeta_mixing_rule /= ZETA_LEONARD .or. .not. exact_binary_dhs)) then
      call stoperror("Single component HS reference requires ZETA_LEONARD mixing rules and exact_binary_dhs")
    endif

  end subroutine check_model_consitency

end module saftvrmie_options
