!---------------------------------------------------------------------
! Module for choosing submodels used in SAFT-VRQ Mie
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------

module saftvrmie_options
  implicit none
  public
  save

  !-------------------------------------------------------------------
  ! Flags for how to mix ZETA
  integer, parameter :: ZETA_LAFITTE=1, ZETA_LINEAR=2, ZETA_LEONARD=3

  !-------------------------------------------------------------------
  ! Flag for switching on hard-sphere EoS
  integer, parameter :: HS_EOS_ORIGINAL = 0, &
       HS_EOS_SANTOS = 1, &
       HS_EOS_PURE_DIJ = 2
  integer, parameter :: KHS_EOS_LAFITTE = 0, & ! Carnahan-Starling
       KHS_EOS_BMCSL = 1, & ! Boublik-Mansoori-Carnahan-Starling-Leland
       KHS_EOS_SANTOS = 2 ! Non-additive Santos

  ! Flag for switching on which pure hard-sphere EoS is used with HS_EOS_SANTOS
  integer, parameter :: PURE_HS_CSK = 0, PURE_HS_CS = 1

  !-------------------------------------------------------------------
  ! Dispersion terms
  integer, parameter :: A3_LAFITTE = 1, A3_SIJ_PREFAC = 2

  !-------------------------------------------------------------------
  ! Overall model selections
  integer, parameter :: LAFITTE=1, QSAFT_FH1=2, QSAFT_FH2=3
  integer, parameter :: LAFITTE_HS_REF=1, SINGLE_COMP_HS_REF=2
  integer, parameter :: ADDITIVE_HS_REF=3, NON_ADD_HS_REF=4

  type saftvrmie_opt
    ! Quantum correction parameters
    integer :: quantum_correction = 0       ! Specifies order of quantum corr. included
    integer :: quantum_correction_hs = 0    ! Specifies the Q-corr of the reference hard-sphere
    integer :: quantum_correction_spec = 0  ! 0: Feynman-Hibbs, 1: Jaen-Kahn
    logical :: quantum_correct_A2 = .true.  ! Include quantum correction to A2


    ! Mixing rules
    logical :: use_epsrule_Lafitte = .true. ! Use the Lafitte combining rule for epsilon
    logical :: exact_binary_dhs=.false.     ! If false, use the Lafitte approximation dij=(di+dj)/2
    logical :: exact_crosspot_eff=.true.    ! Use exact sigma_eff and eps_eff for cross interaction (doesn't seem to improve much, and slows the code down)
    integer :: zeta_mixing_rule = ZETA_LAFITTE   ! Specifies packing fraction treatement for mixtures (LAFITTE or LINEAR)

    ! Flag for switching on hard-sphere EoS
    integer :: hardsphere_EoS = HS_EOS_ORIGINAL
    integer :: Khs_EoS = KHS_EOS_LAFITTE

    ! Flag for switching on which pure hard-sphere EoS is used with HS_EOS_SANTOS
    integer :: pure_hs_EoS = PURE_HS_CS
    logical :: enable_hs_extra = .false.  !> Option to enable/disable extra-term to hard-sphere reference

    !-------------------------------------------------------------------
    ! Dispersion terms
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
  contains
    procedure, public :: saftvrmieaij_model_options
    procedure, public :: set_r_cut
    procedure, public :: truncation_correction_model_control
    procedure, public :: check_model_consitency
    procedure, public :: set_Lafitte_option
    procedure, public :: set_hs_reference
    procedure, public :: print
    ! Assignment operator
    procedure, public :: assign_saftvrmie_model_options
    generic, public :: assignment(=) => assign_saftvrmie_model_options
  end type saftvrmie_opt


contains

  !> Model options....
  !! \author Morten Hammer, January 2019
  subroutine saftvrmieaij_model_options(svrm_o, model, hs_reference)
    class(saftvrmie_opt), intent(inout) :: svrm_o
    integer, intent(in) :: model
    integer, optional, intent(in) :: hs_reference
    select case(model)
    case(LAFITTE)
       call svrm_o%set_Lafitte_option()
    case(QSAFT_FH1)
       call svrm_o%set_Lafitte_option()
       svrm_o%quantum_correction = 1
       svrm_o%quantum_correction_hs = 1
       svrm_o%quantum_correction_spec = 0
    case(QSAFT_FH2)
       call svrm_o%set_Lafitte_option()
       svrm_o%quantum_correction = 2
       svrm_o%quantum_correction_hs = 2
       svrm_o%quantum_correction_spec = 0
    case default
       call stoperror("Unknown model options for SAFT-VR Mie")
    end select
    if (present(hs_reference)) then
      ! Override HS reference
      call svrm_o%set_hs_reference(hs_reference)
    endif
  end subroutine saftvrmieaij_model_options

  subroutine set_Lafitte_option(svrm_o)
    class(saftvrmie_opt), intent(inout) :: svrm_o
    svrm_o%use_epsrule_Lafitte = .true.
    svrm_o%exact_binary_dhs = .false.
    svrm_o%zeta_mixing_rule = ZETA_LAFITTE
    svrm_o%exact_crosspot_eff = .true.
    svrm_o%hardsphere_EoS = HS_EOS_ORIGINAL
    svrm_o%a3_model = A3_LAFITTE
    svrm_o%enable_hs = .true.
    svrm_o%enable_A1 = .true.
    svrm_o%enable_A3 = .true.
    svrm_o%enable_A2 = .true.
    svrm_o%enable_chain = .true.
    svrm_o%enable_truncation_correction = .false.
    svrm_o%enable_hs_extra = .false.
    svrm_o%quantum_correction = 0
    svrm_o%quantum_correction_hs = 0
    svrm_o%quantum_correction_spec = 0
    svrm_o%quantum_correct_A2 = .true.
    svrm_o%Khs_EoS = KHS_EOS_LAFITTE
  end subroutine set_Lafitte_option

  subroutine set_hs_reference(svrm_o, hs_reference)
    class(saftvrmie_opt), intent(inout) :: svrm_o
    integer, intent(in) :: hs_reference
    ! Override HS reference
    select case(hs_reference)
    case(LAFITTE_HS_REF)
      svrm_o%hardsphere_EoS = HS_EOS_ORIGINAL
      svrm_o%zeta_mixing_rule = ZETA_LAFITTE
      svrm_o%exact_binary_dhs = .false.
      svrm_o%enable_hs_extra = .false.
    case(SINGLE_COMP_HS_REF)
      svrm_o%hardsphere_EoS = HS_EOS_PURE_DIJ
      svrm_o%zeta_mixing_rule = ZETA_LEONARD
      svrm_o%exact_binary_dhs = .true.
      svrm_o%enable_hs_extra = .false.
    case(ADDITIVE_HS_REF)
      svrm_o%hardsphere_EoS = HS_EOS_ORIGINAL
      svrm_o%zeta_mixing_rule = ZETA_LAFITTE
      svrm_o%exact_binary_dhs = .false.
      svrm_o%enable_hs_extra = .true.
    case(NON_ADD_HS_REF)
      svrm_o%hardsphere_EoS = HS_EOS_SANTOS
      svrm_o%pure_hs_EoS = PURE_HS_CS
      svrm_o%exact_binary_dhs = .true.
      svrm_o%enable_hs_extra = .false.
    case default
      call stoperror("Unknown HS model options for SAFT-VR Mie")
    end select
  end subroutine set_hs_reference

  !> Set r_cut, and enable truncation correction
  !!
  !! \author Morten Hammer, November 2018
  subroutine set_r_cut(svrm_o, r_c)
    class(saftvrmie_opt), intent(inout) :: svrm_o
    real, intent(in) :: r_c ! Potential cut-off radius
    svrm_o%r_cut = r_c
    svrm_o%enable_truncation_correction = .true.
  end subroutine set_r_cut

  !> Enable/disable truncation and shift correction
  !!
  !! \author Morten Hammer, November 2018
  subroutine truncation_correction_model_control(svrm_o, truncation,shift)
    class(saftvrmie_opt), intent(inout) :: svrm_o
    logical, intent(in) :: truncation,shift
    svrm_o%enable_truncation_correction = truncation
    svrm_o%enable_shift_correction = shift
  end subroutine truncation_correction_model_control

  !> Check that model parameters are consistent
  !!
  !! \author Morten Hammer, March 2019
  subroutine check_model_consitency(svrm_o)
    class(saftvrmie_opt), intent(in) :: svrm_o
    !SINGLE_COMP_HS_REF
    if (  svrm_o%hardsphere_EoS == HS_EOS_PURE_DIJ .and. &
         (svrm_o%zeta_mixing_rule /= ZETA_LEONARD .or. .not. svrm_o%exact_binary_dhs)) then
       call stoperror("Single component HS reference requires ZETA_LEONARD mixing rules and exact_binary_dhs")
    endif

  end subroutine check_model_consitency

  subroutine assign_saftvrmie_model_options(this,other)
    class(saftvrmie_opt), intent(inout) :: this
    class(saftvrmie_opt), intent(in) :: other
    !
    this%use_epsrule_Lafitte = other%use_epsrule_Lafitte
    this%exact_binary_dhs = other%exact_binary_dhs
    this%zeta_mixing_rule = other%zeta_mixing_rule
    this%exact_crosspot_eff = other%exact_crosspot_eff
    this%hardsphere_EoS = other%hardsphere_EoS
    this%a3_model = other%a3_model
    this%enable_hs = other%enable_hs
    this%enable_A1 = other%enable_A1
    this%enable_A3 = other%enable_A3
    this%enable_A2 = other%enable_A2
    this%enable_chain = other%enable_chain
    this%enable_truncation_correction = other%enable_truncation_correction
    this%enable_hs_extra = other%enable_hs_extra
    this%quantum_correction = other%quantum_correction
    this%quantum_correction_hs = other%quantum_correction_hs
    this%quantum_correction_spec = other%quantum_correction_spec
    this%quantum_correct_A2 = other%quantum_correct_A2
    this%Khs_EoS = other%Khs_EoS
    this%enable_shift_correction = other%enable_shift_correction
    this%r_cut = other%r_cut
    this%pure_hs_EoS = other%pure_hs_EoS

  end subroutine assign_saftvrmie_model_options

  subroutine print(this)
    class(saftvrmie_opt), intent(in) :: this
    !
    print *,"saftvrmie options:"
    print *, "use_epsrule_Lafitte:", this%use_epsrule_Lafitte
    print *, "exact_binary_dhs:", this%exact_binary_dhs
    print *, "zeta_mixing_rule:", this%zeta_mixing_rule
    print *, "exact_crosspot_eff:", this%exact_crosspot_eff
    print *, "hardsphere_EoS:", this%hardsphere_EoS
    print *, "a3_model:", this%a3_model
    print *, "enable_hs:", this%enable_hs
    print *, "enable_A1:", this%enable_A1
    print *, "enable_A3:", this%enable_A3
    print *, "enable_A2:", this%enable_A2
    print *, "enable_chain:", this%enable_chain
    print *, "enable_truncation_correction:", this%enable_truncation_correction
    print *, "enable_hs_extra:", this%enable_hs_extra
    print *, "quantum_correction:", this%quantum_correction
    print *, "quantum_correction_hs:", this%quantum_correction_hs
    print *, "quantum_correction_spec:", this%quantum_correction_spec
    print *, "quantum_correct_A2:", this%quantum_correct_A2
    print *, "Khs_EoS:", this%Khs_EoS
    print *, "enable_shift_correction:", this%enable_shift_correction
    print *, "r_cut:", this%r_cut
    print *, "pure_hs_EoS:", this%pure_hs_EoS

  end subroutine print

end module saftvrmie_options
