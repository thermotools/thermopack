!> The module eosdata contains the definitions of the equation of state, mixing
!> rule and the interaction parameters.

module cubic_eos
  use eosdata
  use thermopack_constants, only: eosid_len, bibref_len, ref_len, uid_len, mix_len
  use thermopack_var, only: base_eos_param, base_eos_dealloc
  use unifac, only: unifacdb
  implicit none

  type :: kijdatadb
    character(len=eosid_len) :: eosid
    character(len=eosid_len) :: mruleid
    character(len=ref_len) :: ref
    character(len=bibref_len) :: bib_ref
    character(len=uid_len) :: uid1
    character(len=uid_len) :: uid2
    real :: kijvalue
  end type kijdatadb

  type :: lijdatadb
    character(len=eosid_len) :: eosid
    character(len=eosid_len) :: mruleid
    character(len=ref_len) :: ref
    character(len=bibref_len) :: bib_ref
    character(len=uid_len) :: uid1
    character(len=uid_len) :: uid2
    real :: lijvalue
  end type lijdatadb

  type :: interGEdatadb
    character(len=eosid_len) :: eosid
    character(len=eosid_len) :: mruleid
    character(len=ref_len) :: ref
    character(len=bibref_len) :: bib_ref
    character(len=uid_len) :: uid1
    character(len=uid_len) :: uid2
    real :: kijvalue
    integer :: correlation
    real :: alphaijvalue(2)
    real :: polyij(3)
    real :: polyji(3)
  end type interGEdatadb

  type :: mixExcessGibbs
    integer :: mGE ! Variant: MHV1 (cbMixHuronVidal),
    !          MHV2 (cbMixHuronVidal2),
                   !          NRTL (cbMixNRTL)
    integer, allocatable, dimension(:,:) :: correlation ! 0: Inactive (Use kij)
    ! 1: default, 2: Maribo-Mogensen (2015) - 10.1002/aic.14829
    real, allocatable, dimension(:,:) :: alpha
    real, allocatable, dimension(:,:) :: aGE, bGE, cGE
    ! tau*T = dg/R = aGE + bGE*T + cGE*T^2
    ! tau is dimensionless
    ! TODO: General NRTL form: tau = a + b/T + c/T^2 + d ln(T) + e T^f
  contains
    procedure, public :: dealloc => excess_gibbs_deallocate
    procedure, public :: excess_gibbs_allocate_and_init
    ! Assignment operator
    procedure  :: assign_excess_gibbs_mix
    generic,   public   :: assignment(=) => assign_excess_gibbs_mix
  end type mixExcessGibbs

  integer, parameter :: nDegreePoly=2
  type :: Fraction
    real, dimension(nDegreePoly+1):: pNum !<Numerator polynom
    real, dimension(nDegreePoly+1):: pDen !<Denominator polynom
  end type Fraction
  !< y = (pNum(1)+pNum(2)*x+...pNum(n-1)*x**n)/(pDen(1)+pDen(2)*x+...pDen(n-1)*x**n)
  !< where n = nDegreePoly.  Generally pDen(1) = 1.0

  type :: mixWongSandler
    real, allocatable, dimension(:,:) :: alphaij
    type(Fraction), allocatable, dimension(:,:) :: f_kij, f_tauij
  contains
    procedure, public :: dealloc => WS_deallocate
    procedure, public :: WS_allocate_and_init
    ! Assignment operator
    procedure  :: assign_WS_mix
    generic,   public   :: assignment(=) => assign_WS_mix
  end type mixWongSandler

  type :: singleData
     !> Cubic EoS quantities for individual components
     real :: omegaA, omegaB, omegaC, zcrit ! Allow for 3-paramter EOS
     real :: a !< Energy constant in front of alpha. Units: Pa*L^2/mol^2.
     real :: b !< Covolume parameter. Units: L/mol.
     real :: c !< Parameter in Patel-Teja EoS. Units: L/mol.

     real :: acf !< Acentric factor to use for the cubic EoS [-]
     real :: tc  !< Critical temperature to use in the cubic EoS [K]
     real :: pc  !< Critical pressure to use in the cubic EoS [Pa]

     ! Alpha correlations for a factor
     integer :: alphaMethod=-1
     character (len=short_label_len) :: alphaCorrName
     real :: alpha, dalphadt, d2alphadt2 !< alpha is dimensionless
     real :: alphaParams(3)=-1e10 ! Fitted coeffs in correlation. Max 3.

     ! Beta correlations for b factor
     integer :: betaMethod=1
     character (len=12) :: betaCorrName = "Classic     "
     real :: beta, dbetadt, d2betadt2 !< beta is dimensionless
     real :: betaParams(3)=-1e10 ! Fitted coeffs in correlation. Max 3.
  end type singleData

  type, extends(base_eos_param) :: cb_eos
    character (len=mix_len) :: mruleid
    character (len=20) :: name
    integer :: mruleidx
    logical  :: cubic_verbose = .false.

    ! Parameters m1 and m2 appearing in the denominator of the attractive term
    real :: m1, m2
    real :: dm1dB, dm1dC, dm2dB, dm2dC
    real :: d2m1dB2, d2m1dC2, d2m2dB2, d2m2dC2,d2m2dBdC,d2m1dBdC
    real :: delta ! m1 and m2 are usually functions of this EoS-dependent constant

    real :: a, b, c !< Dependent of component
    real :: sumN
    real :: sumA !< [Pa*L^2/mol^2]
    real :: sumB !< Molfraction average of single-component b_i [L/mol]
    real :: sumC !< [L/mol]

    ! Pressure differentials
    real :: pn,pa,pb,pc,pt,pv

    ! Reduced residual Helmholtz free energy ff, and its derivatives wrt
    ! explicit variables T,v,n and explicit parameters a,b,c. These are
    ! combined to yield total derivatives wrt T,v,n in the cbhelm module.
    real :: ff,fft,fftt
    real :: ffn,ffnv,ffna,ffnb,ffnc,ffnn,ffnt
    real :: ffa,ffaa,ffab,ffac,ffat
    real :: ffb,ffbb,ffbc,ffbt
    real :: ffc,ffcc,ffct
    real :: ffv,ffvt,ffvv,ffva,ffvb,ffvc

    ! Parameter differentials that will be combined with the ff differentials
    real :: at, att, bt, btt ! derivatives wrt temperature
    real, allocatable  :: ai(:),ait(:), aij(:,:) ! derivatives wrt mole numbers and temperature
    real, allocatable  :: bi(:),bit(:),bitt(:), ci(:), bij(:,:), cij(:,:) ! derivatives wrt mole numbers and temperature

    ! Parameters for numerical solution of cubic EoS
    integer :: extrm(2), nextrm(2), nzfac(2)

    ! Cubic EoS quantities for individual components
    type(singleData), allocatable :: single(:)

    ! Mixing parameters
    real, allocatable, dimension(:,:) :: kij ! interaction parameter for energy
    real, allocatable, dimension(:,:) :: lij ! interaction parameter for size
    real, allocatable, dimension(:,:) :: lowcase_bij
    logical :: simple_covolmixing
    type(mixExcessGibbs) :: mixGE
    type(mixWongSandler) :: mixWS
    type(unifacdb), pointer :: unifdb => NULL()

  contains
    procedure, public :: dealloc => cubic_eos_dealloc
    procedure, public  :: allocate_and_init => allocate_and_init_cubic_eos
    !procedure, public :: de_allocate_cubic_eos
    ! Assignment operator
    procedure, pass(This), public :: assign_eos => assign_cubic_eos
  end type cb_eos

  type, extends(cb_eos) :: lk_eos

  !   private

  ! contains
  end type lk_eos

  type, extends(cb_eos) :: cpa_eos

  !   private

  ! contains
  end type cpa_eos

  integer, parameter :: cbMixClassicGroup = 1 !< Classic kij type mixing
  integer, parameter :: cbMixVdW = 11 !< Classic vdW mixing rule for am and bm - using k_ij == k_ji
  integer, parameter :: cbMixVdWCPA = 12 !< CPA mixing rule (same as cbMixVdW, but kij from another db)
  integer, parameter :: cbMixReid = 13    !< Unsymmertic mixing rule where k_ij can be different than k_ji
  integer, parameter :: cbMixGEGroup = 2
  integer, parameter :: cbMixHuronVidal = 21 !< Huron vidal mixing rule
  integer, parameter :: cbMixHuronVidal2 = 22 !< Huron vidal mixing rule
  integer, parameter :: cbMixNRTL = 23 !< NRTL mixing rule
  integer, parameter :: cbMixUNIFAC = 24 !< UNIFAC mixing rule
  integer, parameter :: cbMixHVCPA = 25 !< Huron Vidal mixing rule (classic, but kij from another db)
  integer, parameter :: cbMixHVCPA2 = 26 !< Huron Vidal mixing rule (classic, but kij from another db)
  integer, parameter :: cbMixWongSandler = 3 !< Wong Sandler mixing rule

  type mix_label_mapping
    integer :: mix_idx_group
    integer :: mix_idx
    character(len=short_label_len) :: short_label
    character(len=label_len) :: label
    character(len=label_len) :: alias
  end type mix_label_mapping

  integer, parameter :: n_mix_rules = 10
  type(mix_label_mapping), dimension(n_mix_rules), parameter :: mix_label_db = (/&
       mix_label_mapping(mix_idx_group=cbMixClassicGroup,&
       mix_idx=cbMixVdW, short_label="VDW", label="Classic",&
       alias = "CLASSIC"), &
       mix_label_mapping(mix_idx_group=cbMixClassicGroup,&
       mix_idx=cbMixVdWCPA, short_label="Classic(CPA)", label="Classic(CPA)",&
       alias = ""), &
       mix_label_mapping(mix_idx_group=cbMixClassicGroup,&
       mix_idx=cbMixReid, short_label="Reid", label="Reid",&
       alias = ""), &
       mix_label_mapping(mix_idx_group=cbMixGEGroup,&
       mix_idx=cbMixHuronVidal, short_label="HV", label="Huron-Vidal",&
       alias = "HV1/HV0"), &
       mix_label_mapping(mix_idx_group=cbMixGEGroup,&
       mix_idx=cbMixHuronVidal2, short_label="HV2", label="Huron-Vidal2",&
       alias = ""), &
       mix_label_mapping(mix_idx_group=cbMixWongSandler,&
       mix_idx=cbMixWongSandler, short_label="WongSandler", label="Wong-Sandler",&
       alias = "WS"), &
       mix_label_mapping(mix_idx_group=cbMixGEGroup,&
       mix_idx=cbMixNRTL, short_label="NRTL", label="NRTL",&
       alias = ""), &
       mix_label_mapping(mix_idx_group=cbMixGEGroup,&
       mix_idx=cbMixUNIFAC, short_label="UNIFAC", label="UNIFAC",&
       alias = "UMR/VTPR"), &
       mix_label_mapping(mix_idx_group=cbMixGEGroup,&
       mix_idx=cbMixHVCPA, short_label="HVCPA", label="HVCPA",&
       alias = ""), &
       mix_label_mapping(mix_idx_group=cbMixGEGroup,&
       mix_idx=cbMixHVCPA2, short_label="HVCPA2", label="HVCPA2",&
       alias = "") &
       /)

  integer, parameter :: nHVCorrs = 4
  integer, parameter, dimension(nHVCorrs) :: HVCorrIndices = (/&
       cbMixHuronVidal,&
       cbMixHuronVidal2,&
       cbMixHVCPA,&
       cbMixHVCPA2/)

  integer, parameter :: nGECorrs = 6
  integer, parameter, dimension(nGECorrs) :: GECorrIndices = (/&
       HVCorrIndices,&
       cbMixNRTL,&
       cbMixGEGroup/)

  ! Alphacorrelations. To add new correlations, add it below and update module cbAlpha
  integer, parameter :: cbAlphaClassicIdx = 1 !< Classic alpha-correlation VdW, RK, SRK and PR
  integer, parameter :: cbAlphaTwuIdx = 2     !< Twu-Coon-Bluck-Cunninghan exponential formulation
  integer, parameter :: cbAlphaMcIdx = 3     !< Mathias-Copeman expression for polar substances
  integer, parameter :: cbAlphaGergIdx = 4     !< Gerg-Water(PR) expression for polar substances Q1,Q2 and Q3
  integer, parameter :: cbAlphaClassicFitIdx = 5 !< Classic alpha-corr where with replaced by a fitted parameter. Used in CPA.
  integer, parameter :: cbAlphaUMRIdx = 6 !< Classic alpha-corr with another function m(acf)

  integer, parameter :: cbAlphaGBIdx = 7 !< Alpha-corr of Graboski and Daubert (10.1021/i260068a009)
  integer, parameter :: cbAlphaRKIdx = 8 !< Alpha-corr of Redlich-Kwong (10.1021/cr60137a013)
  integer, parameter :: cbAlphaSoaveIdx = 9 !< Alpha-corr of Soave ()
  integer, parameter :: cbAlphaPRIdx = 10 !< Alpha-corr of Peng-Robinson
  integer, parameter :: cbAlphaPTIdx = 11 !< Alpha-corr of Patel-Teja
  integer, parameter :: cbAlphaSWIdx = 12 !< Alpha-corr of Schmidt-Wensel
  integer, parameter :: cbAlphaVDWIdx = 13 !< Alpha-corr of van der Waals
  integer, parameter :: cbAlphaPR78Idx = 14 !< Peng-Robinson alpha-corr for w>0.491 (RR-28 GPA)

  type alpha_label_mapping
    integer :: alpha_idx
    integer :: n_param
    character(len=short_label_len) :: short_label
    character(len=label_len) :: description
    integer :: classic_for_eos_idx
  end type alpha_label_mapping

  integer, parameter :: n_alpha_corrs = 14
  type(alpha_label_mapping), dimension(n_alpha_corrs), parameter :: alpha_corr_db = (/&
       alpha_label_mapping(alpha_idx=cbAlphaClassicIdx, n_param=1, short_label="CLASSIC", &
       description="Classic alpha-correlation VdW, SRK, PR, PT and SW", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaTwuIdx, n_param=3, short_label="TWU", &
       description="Twu-Coon-Bluck-Cunninghan exponential formulation", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaMcIdx, n_param=3, short_label="MC", &
       description="Mathias-Copeman expression for polar substances", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaGergIdx, n_param=3, short_label="GERG", &
       description="Gerg-Water(PR) expression for polar substances", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaClassicFitIdx, n_param=1, &
       short_label="CLASSICFIT", &
       description="Classic alpha-corr with fitted parameter. Used in CPA.", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaUMRIdx, n_param=1, short_label="UMR", &
       description="Classic alpha-corr with another function m(acf)", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaGBIdx, n_param=1, short_label="GD", &
       description="Alpha-corr of Graboski and Daubert (10.1021/i260068a009)", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaRKIdx, n_param=1, short_label="RK", &
       description="Alpha-corr of Redlich-Kwong (10.1021/cr60137a013)", &
       classic_for_eos_idx=-1), &
       alpha_label_mapping(alpha_idx=cbAlphaSoaveIdx, n_param=1, short_label="SOAVE", &
       description="Alpha-corr of Soave", &
       classic_for_eos_idx=cbSRK), &
       alpha_label_mapping(alpha_idx=cbAlphaPRIdx, n_param=1, short_label="PR", &
       description="Alpha-corr of Peng-Robinson", &
       classic_for_eos_idx=cbPR), &
       alpha_label_mapping(alpha_idx=cbAlphaPTIdx, n_param=1, short_label="PT", &
       description="Alpha-corr of Patel-Teja", &
       classic_for_eos_idx=cbPT), &
       alpha_label_mapping(alpha_idx=cbAlphaSWIdx, n_param=1, short_label="SW", &
       description="Alpha-corr of cbAlphaSWIdx", &
       classic_for_eos_idx=cbSW), &
       alpha_label_mapping(alpha_idx=cbAlphaVDWIdx, n_param=1, short_label="VDW", &
       description="Alpha-corr of cbAlphaVDWIdx", &
       classic_for_eos_idx=cbVDW), &
       alpha_label_mapping(alpha_idx=cbAlphaPR78Idx, n_param=1, short_label="PR78", &
       description="Alpha-corr of cbAlphaPR78Idx", &
       classic_for_eos_idx=-1) &
       /)

  ! Beta correlations for cubic EoS "b" parameter. To add new correlations, add
  ! it below and update module cbBeta.
  integer, parameter :: nBetaCorrs = 2
  integer, parameter :: cbBetaClassicIdx = 1 !< Classic beta-correlation yielding the classic, temperature-independent covolume
  integer, parameter :: cbBetaQuantumIdx = 2 !< Beta-correlation for quantum fluids He, Ne, H2, D2
  character(len=11), dimension(nBetaCorrs), parameter :: betaCorrNames = (/ &
       "CLASSIC    ", "QUANTUM    " /)
  integer, parameter, dimension(nBetaCorrs) :: betaCorrNumParams = (/0,2/)

contains

  subroutine WS_deallocate(mixWS)
    class(mixWongSandler), intent(inout) :: mixWS
    ! Locals
    integer :: err
    !
    err = 0
    if (allocated(mixWS%f_kij)) deallocate (mixWS%f_kij, stat=err)
    if (err /= 0) call stoperror('WS_deallocate: could not deallocate array: mixWS%f_kij')
    !
    if (allocated(mixWS%f_tauij)) deallocate (mixWS%f_tauij, stat=err)
    if (err /= 0) call stoperror('WS_deallocate: could not deallocate array: mixWS%f_tauij')
    if (allocated(mixWS%alphaij)) deallocate (mixWS%alphaij, stat=err)
    if (err /= 0) call stoperror('WS_deallocate: could not allocate array: mixWS%alphaij')

  end subroutine WS_deallocate

  subroutine WS_allocate_and_init(mixWS,nc)
    use utilities, only: allocate_nc_x_nc
    class(mixWongSandler), intent(inout) :: mixWS
    integer, intent(in) :: nc
    ! Locals
    type(Fraction):: ZeroFraction !<= 0.0/1.0
    integer :: i, j, err
    ZeroFraction%pNum = 0.0
    ZeroFraction%pDen = 0.0
    ZeroFraction%pDen(1) = 1.0
    !
    err = 0
    call mixWS%dealloc()
    allocate (mixWS%f_kij(nc,nc), stat=err)
    if (err /= 0) call stoperror('WS_allocate_and_init: could not allocate array: mixWS%f_kij')
    allocate (mixWS%f_tauij(nc,nc), stat=err)
    if (err /= 0) call stoperror('WS_allocate_and_init: could not allocate array: mixWS%f_tauij')
    call allocate_nc_x_nc(mixWS%Alphaij,nc,"mixWS%Alphaij")
    do i=1,nc
      do j=1,nc
        mixWS%f_kij(i,j) = ZeroFraction
        mixWS%f_Tauij(i,j) = ZeroFraction
        mixWS%Alphaij(i,j) = 0.0
      enddo
    enddo
  end subroutine WS_allocate_and_init

  subroutine assign_WS_mix(mixWS1,mixWS2)
    class(mixWongSandler), intent(inout) :: mixWS1
    class(mixWongSandler), intent(in) :: mixWS2
    if (allocated(mixWS2%f_kij)) then
      call mixWS1%WS_allocate_and_init(size(mixWS2%f_kij,dim=1))
      mixWS1%f_kij = mixWS2%f_kij
      mixWS1%f_Tauij = mixWS2%f_Tauij
      mixWS1%Alphaij = mixWS2%Alphaij
    endif
  end subroutine assign_WS_mix

  subroutine excess_gibbs_deallocate(mixGE)
    class(mixExcessGibbs), intent(inout) :: mixGE
    ! Locals
    integer :: err
    if (allocated (mixGE%alpha)) deallocate (mixGE%alpha, STAT=err)
    if (allocated (mixGE%aGE)) deallocate (mixGE%aGE, STAT=err)
    if (allocated (mixGE%bGE)) deallocate (mixGE%bGE, STAT=err)
    if (allocated (mixGE%cGE)) deallocate (mixGE%cGE, STAT=err)
    if (allocated (mixGE%correlation)) deallocate (mixGE%correlation, STAT=err)
  end subroutine excess_gibbs_deallocate

  subroutine excess_gibbs_allocate_and_init(mixGE,nc)
    class(mixExcessGibbs), intent(inout) :: mixGE
    integer, intent(in) :: nc
    ! Locals
    integer :: i, j
    call mixGE%dealloc()
    allocate (mixGE%alpha(nc,nc))
    allocate (mixGE%aGE(nc,nc))
    allocate (mixGE%bGE(nc,nc))
    allocate (mixGE%cGE(nc,nc))
    allocate (mixGE%correlation(nc,nc))
    do i=1,nc
      do j=1,nc
        mixGE%alpha(i,j) = 0.0
        mixGE%aGE(i,j) = 0.0
        mixGE%bGE(i,j) = 0.0
        mixGE%cGE(i,j) = 0.0
        mixGE%correlation(i,j) = 0
      enddo
   enddo
  end subroutine excess_gibbs_allocate_and_init

  subroutine assign_excess_gibbs_mix(mixGE1,mixGE2)
    class(mixExcessGibbs), intent(inout) :: mixGE1
    class(mixExcessGibbs), intent(in) :: mixGE2
    if (allocated(mixGE2%alpha)) then
      call mixGE1%excess_gibbs_allocate_and_init(size(mixGE2%alpha,dim=1))
      mixGE1%alpha = mixGE2%alpha
      mixGE1%aGE = mixGE2%aGE
      mixGE1%bGE = mixGE2%bGE
      mixGE1%cGE = mixGE2%cGE
      mixGE1%correlation = mixGE2%correlation
    endif
  end subroutine assign_excess_gibbs_mix

  !> Allocate memory for cubic eos
  function cubic_eos_constructor(nc,eos_label) result(cb)
    ! Input:
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label
    ! Created object:
    type(cb_eos) :: cb
    ! Locals

    call cb%allocate_and_init(nc,eos_label)

  end function cubic_eos_constructor

  !> Allocate memory for LK eos
  function lk_eos_constructor(nc,eos_label) result(lk)
    ! Input:
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label
    ! Created object:
    type(lk_eos) :: lk
    !
    call lk%cb_eos%allocate_and_init(nc,eos_label)

  end function lk_eos_constructor


  !> Allocate memory for CPA eos
  function cpa_eos_constructor(nc,eos_label) result(cpa)
    ! Input:
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label
    ! Created object:
    type(cpa_eos) :: cpa
    ! Locals

    call cpa%cb_eos%allocate_and_init(nc,eos_label)

  end function cpa_eos_constructor

  subroutine assign_cubic_eos(this,other)
    use utilities, only: allocate_nc, allocate_nc_x_nc
    class(cb_eos), intent(inout) :: this
    class(*), intent(in) :: other
    ! Locals
    integer :: istat
    select type (other)
    class is (cb_eos)
      call this%assign_base_eos_param(other)
      this%eosid = other%eosid
      this%mruleid = other%mruleid
      this%name = other%name
      this%eosidx = other%eosidx
      this%mruleidx = other%mruleidx
      this%subeosidx = other%subeosidx
      this%cubic_verbose = other%cubic_verbose

      this%m1 = other%m1
      this%m2 = other%m2
      this%dm1dB = other%dm1dB
      this%dm1dC = other%dm1dC
      this%dm2dB = other%dm2dB
      this%dm2dC = other%dm2dC
      this%d2m1dB2 = other%d2m1dB2
      this%d2m1dC2 = other%d2m1dC2
      this%d2m2dB2 = other%d2m2dB2
      this%d2m2dC2 = other%d2m2dC2
      this%d2m2dBdC = other%d2m2dBdC
      this%d2m1dBdC = other%d2m1dBdC
      this%delta = other%delta

      this%a = other%a
      this%b = other%b
      this%c = other%c
      this%sumN = other%sumN
      this%sumA = other%sumA
      this%sumB = other%sumB
      this%sumC = other%sumC

      this%pn = other%pn
      this%pa = other%pa
      this%pb = other%pb
      this%pc = other%pc
      this%pt = other%pt
      this%pv = other%pv

      this%ff = other%ff
      this%fft = other%fft
      this%fftt = other%fftt
      this%ffn = other%ffn
      this%ffnv = other%ffnv
      this%ffna = other%ffna
      this%ffnb = other%ffnb
      this%ffnc = other%ffnc
      this%ffnn = other%ffnn
      this%ffnt = other%ffnt
      this%ffa = other%ffa
      this%ffaa = other%ffaa
      this%ffab = other%ffab
      this%ffac = other%ffac
      this%ffat = other%ffat
      this%ffb = other%ffb
      this%ffbb = other%ffbb
      this%ffbc = other%ffbc
      this%ffbt = other%ffbt
      this%ffc = other%ffc
      this%ffcc = other%ffcc
      this%ffct = other%ffct
      this%ffv = other%ffv
      this%ffvt = other%ffvt
      this%ffvv = other%ffvv
      this%ffva = other%ffva
      this%ffvb = other%ffvb
      this%ffvc = other%ffvc

      this%at = other%at
      this%att = other%att
      this%bt = other%bt
      this%btt = other%btt

      if (allocated(other%aij)) then
        call allocate_nc_x_nc(this%aij,size(other%aij,dim=1),"aij")
        this%aij = other%aij
      endif
      if (allocated(other%bij)) then
        call allocate_nc_x_nc(this%bij,size(other%bij,dim=1),"bij")
        this%bij = other%bij
      endif
      if (allocated(other%cij)) then
        call allocate_nc_x_nc(this%cij,size(other%cij,dim=1),"cij")
        this%cij = other%cij
      endif
      if (allocated(other%ai)) then
        call allocate_nc(this%ai,size(other%ai,dim=1),"ai")
        this%ai = other%ai
      endif
      if (allocated(other%ait)) then
        call allocate_nc(this%ait,size(other%ait,dim=1),"ait")
        this%ait = other%ait
      endif
      if (allocated(other%bi)) then
        call allocate_nc(this%bi,size(other%bi,dim=1),"bi")
        this%bi = other%bi
      endif
      if (allocated(other%bit)) then
        call allocate_nc(this%bit,size(other%bit,dim=1),"bit")
        this%bit = other%bit
      endif
      if (allocated(other%bitt)) then
        call allocate_nc(this%bitt,size(other%bitt,dim=1),"bitt")
        this%bitt = other%bitt
      endif
      if (allocated(other%ci)) then
        call allocate_nc(this%ci,size(other%ci,dim=1),"ci")
        this%ci = other%ci
      endif

      this%extrm = other%extrm
      this%nextrm = other%nextrm
      this%nzfac = other%nzfac

      if (allocated(other%single)) then
        this%single = other%single
      else if (allocated(this%single)) then
        deallocate(this%single, stat=istat)
        if (istat /= 0) call stoperror("cubic_eos: Not able to deallocate single")
      endif

      if (allocated(other%kij)) then
        call allocate_nc_x_nc(this%kij,size(other%kij,dim=1),"kij")
        this%kij = other%kij
      endif
      if (allocated(other%lij)) then
         call allocate_nc_x_nc(this%lij,size(other%lij,dim=1),"lij")
         this%lij = other%lij
      endif
      if (allocated(other%lowcase_bij)) then
        call allocate_nc_x_nc(this%lowcase_bij,size(other%lowcase_bij,dim=1),"lowcase_bij")
        this%lowcase_bij = other%lowcase_bij
      endif
      this%simple_covolmixing = other%simple_covolmixing
      this%mixGE = other%mixGE
      this%mixWS = other%mixWS
      if (associated(other%unifdb)) then
        if (.not. associated(this%unifdb)) then
          allocate(unifacdb :: this%unifdb, stat=istat)
          if (istat /= 0) call stoperror("cubic_eos: Not able to allocate unifacdb")
        endif
        this%unifdb = other%unifdb
      endif
    class default
      print *,"assign_cubic_eos: Should not be here"
    end select

  end subroutine assign_cubic_eos

  subroutine allocate_and_init_cubic_eos(eos,nc,eos_label)
    use utilities, only: allocate_nc, allocate_nc_x_nc
    class(cb_eos), intent(inout) :: eos
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label !< EOS label
    ! Locals
    integer :: err
    err = 0 ! needs initialization due to if clauses below

    call allocate_nc(eos%ai,nc,"eos%ai")
    call allocate_nc(eos%ait,nc,"eos%ait")
    call allocate_nc(eos%bi,nc,"eos%bi")
    call allocate_nc(eos%bit,nc,"eos%bit")
    call allocate_nc(eos%bitt,nc,"eos%bitt")
    call allocate_nc(eos%ci,nc,"eos%ci")

    call allocate_nc_x_nc(eos%aij,nc,"eos%aij")
    call allocate_nc_x_nc(eos%bij,nc,"eos%bij")
    call allocate_nc_x_nc(eos%cij,nc,"eos%cij")

    call allocate_nc_x_nc(eos%kij,nc,"eos%kij")
    call allocate_nc_x_nc(eos%lij,nc,"eos%lij")
    call allocate_nc_x_nc(eos%lowcase_bij,nc,"eos%lowcase_bij")

    if (allocated (eos%single)) deallocate(eos%single, STAT=err)
    if (err /= 0) call stoperror('allocate_cbeos: could not deallocate single')
    allocate (eos%single(nc), STAT=err)
    if (err /= 0) call stoperror('allocate_cbeos: could not allocate single')

    eos%eosid = ""
    eos%mruleid = ""
    eos%name = ""
    eos%eosidx = 0
    eos%mruleidx = 0
    eos%subeosidx = 0
    eos%cubic_verbose = .false.

    eos%m1 = 0
    eos%m2 = 0
    eos%dm1dB = 0
    eos%dm1dC = 0
    eos%dm2dB = 0
    eos%dm2dC = 0
    eos%d2m1dB2 = 0
    eos%d2m1dC2 = 0
    eos%d2m2dB2 = 0
    eos%d2m2dC2 = 0
    eos%d2m2dBdC = 0
    eos%d2m1dBdC = 0
    eos%delta = 0

    eos%a = 0
    eos%b = 0
    eos%c = 0
    eos%sumN = 0
    eos%sumA = 0
    eos%sumB = 0
    eos%sumC = 0

    eos%pn = 0
    eos%pa = 0
    eos%pb = 0
    eos%pc = 0
    eos%pt = 0
    eos%pv = 0

    eos%ff = 0
    eos%fft = 0
    eos%fftt = 0
    eos%ffn = 0
    eos%ffnv = 0
    eos%ffna = 0
    eos%ffnb = 0
    eos%ffnc = 0
    eos%ffnn = 0
    eos%ffnt = 0
    eos%ffa = 0
    eos%ffaa = 0
    eos%ffab = 0
    eos%ffac = 0
    eos%ffat = 0
    eos%ffb = 0
    eos%ffbb = 0
    eos%ffbc = 0
    eos%ffbt = 0
    eos%ffc = 0
    eos%ffcc = 0
    eos%ffct = 0
    eos%ffv = 0
    eos%ffvt = 0
    eos%ffvv = 0
    eos%ffva = 0
    eos%ffvb = 0
    eos%ffvc = 0

    eos%at  = 0
    eos%att = 0
    eos%bt  = 0
    eos%btt = 0

    eos%aij  = 0
    eos%bij  = 0
    eos%cij  = 0
    eos%ai   = 0
    eos%ait  = 0
    eos%bi   = 0
    eos%bit  = 0
    eos%bitt = 0
    eos%ci   = 0

    eos%extrm  = 0
    eos%nextrm = 0
    eos%nzfac  = 0

    eos%kij = 0
    eos%lij = 0
    eos%lowcase_bij = 0
    eos%simple_covolmixing = .true.
  end subroutine allocate_and_init_cubic_eos

  !! \author Morten H
  subroutine cubic_eos_dealloc(eos)
    use utilities, only: deallocate_real, deallocate_real_2
    ! Passed object:
    class(cb_eos), intent(inout) :: eos
    ! Loclas
    integer :: stat
    call base_eos_dealloc(eos)

    call deallocate_real(eos%ai,"eos%ai")
    call deallocate_real(eos%ait,"eos%ait")
    call deallocate_real(eos%bi,"eos%bi")
    call deallocate_real(eos%bit,"eos%bit")
    call deallocate_real(eos%bitt,"eos%bitt")
    call deallocate_real(eos%ci,"eos%ci")

    call deallocate_real_2(eos%aij,"eos%aij")
    call deallocate_real_2(eos%bij,"eos%bij")
    call deallocate_real_2(eos%cij,"eos%cij")

    call deallocate_real_2(eos%kij,"eos%kij")
    call deallocate_real_2(eos%lij,"eos%lij")
    call deallocate_real_2(eos%lowcase_bij,"eos%lowcase_bij")

    stat = 0
    if (allocated(eos%single)) deallocate(eos%single, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating single'

    call eos%mixGE%dealloc()
    call eos%mixWS%dealloc()
    if (associated(eos%unifdb)) then
      call eos%unifdb%dealloc()
      stat = 0
      deallocate(eos%unifdb,STAT=stat)
      if (stat /= 0) write (*,*) 'Error deallocating unifdb'
    endif

  end subroutine cubic_eos_dealloc

  function isHVmixModel(mix_idx) result(isHV)
    integer, intent(in) :: mix_idx
    logical :: isHV
    ! Locals
    integer :: i
    isHV = .false.
    do i=1,nHVCorrs
      if (HVCorrIndices(i) == mix_idx) then
        isHV = .true.
        return
      endif
    enddo
  end function isHVmixModel

  function isGEmixModel(mix_idx) result(isGE)
    integer, intent(in) :: mix_idx
    logical :: isGE
    ! Locals
    integer :: i
    isGE = .false.
    do i=1,nGECorrs
      if (GECorrIndices(i) == mix_idx) then
        isGE = .true.
        return
      endif
    enddo
  end function isGEmixModel

  function get_mix_db_idx(short_label) result(idx)
    character(len=*), intent(in) :: short_label
    integer :: idx
    ! Locals
    integer :: i
    idx = -1
    do i=1,n_mix_rules
      if (str_eq(short_label,mix_label_db(i)%short_label) .or. &
           string_match(short_label,mix_label_db(i)%alias)) then
        idx = i
        return
      endif
    enddo
  end function get_mix_db_idx

  function get_alpha_db_idx(short_label) result(idx)
    character(len=*), intent(in) :: short_label
    integer :: idx
    ! Locals
    integer :: i
    idx = -1
    do i=1,n_alpha_corrs
      if ( str_eq(short_label,alpha_corr_db(i)%short_label)) then
        idx = i
        return
      endif
    enddo
    print *, "Possible alphacorrs:"
    do i=1,n_alpha_corrs
      print *,trim(alpha_corr_db(i)%short_label)
    enddo
    call StopError ('nameToIdx::unknown alpha corr name ')
  end function get_alpha_db_idx

  function get_alpha_db_idx_from_alpha_idx(alpha_idx) result(idx)
    integer, intent(in) :: alpha_idx
    integer :: idx
    ! Locals
    integer :: i
    idx = -1
    do i=1,n_alpha_corrs
      if (alpha_idx == alpha_corr_db(i)%alpha_idx) then
        idx = i
        return
      endif
    enddo
  end function get_alpha_db_idx_from_alpha_idx

  function eos_to_classic_alpha_db_idx(eosidx) result(alpha_idx)
    integer, intent(in) :: eosidx
    integer :: alpha_idx
    ! Locals
    integer :: i
    alpha_idx = -1
    do i=1,n_alpha_corrs
      if (eosidx == alpha_corr_db(i)%classic_for_eos_idx) then
        alpha_idx = i
        exit
      endif
    enddo
  end function eos_to_classic_alpha_db_idx

  function is_classic_alpha(alphaidx) result(is_classic)
    integer, intent(in) :: alphaidx
    logical :: is_classic
    !
    is_classic = (&
         alphaidx == cbAlphaClassicFitIdx .OR. &
         alphaidx == cbAlphaSoaveIdx .OR. &
         alphaidx == cbAlphaPRIdx .OR. &
         alphaidx == cbAlphaPTIdx .OR. &
         alphaidx == cbAlphaSWIdx .OR. &
         alphaidx == cbAlphaVDWIdx)

  end function is_classic_alpha

end module cubic_eos
