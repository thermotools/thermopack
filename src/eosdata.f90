!> The module eosdata contains the definitions of the equation of state, mixing
!> rule and the interaction parameters.

Module eosdata
  use multiparameter_base, only: meos
  use tpmbwr, only: eosmbwr

  implicit none
  save

  integer, parameter :: cbNeq = 3
  integer, parameter :: cbNmix = 3

  integer, parameter :: cbSRK = 1     !< Plain SRK, Soave Redlich Kwong
  integer, parameter :: cbSRKGB = 2   !< SRK/Gabowski
  integer, parameter :: cbPR = 3      !< Peng-Robinson
  integer, parameter :: cbVdW = 4     !< Van der Waals
  integer, parameter :: cbRK = 5      !< Redlich Kwong
  integer, parameter :: cbSW = 6      !< Schmidt-Wensel
  integer, parameter :: cbPT = 7      !< Patel-Teja
  integer, parameter :: eosLK = 8     !< Lee-Kesler
  integer, parameter :: cspSRK = 9    !< Corrensponding State Principle (CSP)
  integer, parameter :: cspSRKGB = 10 !< Corrensponding State Principle (CSP)
  integer, parameter :: cspPR = 11    !< Corrensponding State Principle (CSP)
  integer, parameter :: cpaSRK = 12   !< Cubic Plus Association (CPA)
  integer, parameter :: cpaPR = 13    !< Cubic Plus Association (CPA)
  integer, parameter :: eosPC_SAFT = 14  !< PC-SAFT equation of state
  integer, parameter :: eos_single = 15  !< Single component multiparamater eos

  integer, parameter :: meosMbwr19 = 1511    !< MBWR19 (Bender) multiparameter equation of state
  integer, parameter :: meosMbwr32 = 1512    !< MBWR32 multiparameter equation of state
  integer, parameter :: meosNist = 152       !< Multiparameter EoS on NIST-like form
  integer, parameter :: eosBH_pert = 16      !< Barker-Henderson perturbation theory model
  integer, parameter :: eosSAFT_VR_MIE = 161 !< SAFT-VR-MIE equation of state
  integer, parameter :: eosLJsplined = 162   !< Lennard-Jones splined equation of state
  integer, parameter :: eosPeTS = 17         !< PeTS equation of state for LJTS at 2.5*sigma
  integer, parameter :: meosNist_mix  = 19   !< Multiparameter EoS for fluids with ideal mixture

  integer, parameter :: cbMixClassic = 1 !< Classic WdW mixing rule for am and bm - using k_ij == k_ji
  integer, parameter :: cbMixReid = 2    !< Unsymmertic mixing rule where k_ij can be different than k_ji
  integer, parameter :: cbMixHuronVidal = 3 !< Huron vidal mixing rule
  integer, parameter :: cbMixHuronVidal2 = 4 !< Huron vidal mixing rule

  integer, parameter :: cbMixWongSandler = 7 !< Wong Sandler mixing rule
  integer, parameter :: cbMixClassicCPA = 8 !< CPA mixing rule (same as classic, but kij from another db)
  integer, parameter :: cbMixNRTL = 9 !< NRTL mixing rule
  integer, parameter :: cbMixUNIFAC = 10 !< NRTL mixing rule
  integer, parameter :: cbMixHVCPA = 11 !< Huron Vidal mixing rule (classic, but kij from another db)
  integer, parameter :: cbMixHVCPA2 = 12 !< Huron Vidal mixing rule (classic, but kij from another db)

  ! Alphacorrelations. To add new correlations, add it below and update module cbAlpha
  integer, parameter :: cbAlphaClassicIdx = 1 !< Classic alpha-correlation VdW, RK, SRK and PR
  integer, parameter :: cbAlphaTwuIdx = 2     !< Twu-Coon-Bluck-Cunninghan exponential formulation
  integer, parameter :: cbAlphaMcIdx = 3     !< Mathias-Copeman expression for polar substances Q1,Q2 and Q3 need to fitted to (H2O,MEG,CO2?)
  integer, parameter :: cbAlphaGergIdx = 4     !< Gerg-Water(PR) expression for polar substances Q1,Q2 and Q3
  integer, parameter :: cbAlphaClassicFitIdx = 5 !< Classic alpha-corr where with replaced by a fitted parameter. Used in CPA.
  integer, parameter :: cbAlphaUMRIdx = 6 !< Classic alpha-corr with another function m(acf)

  integer, parameter :: nAlphaCorrs = 4
  character(len=11), dimension(nAlphaCorrs), parameter :: alphaCorrNames = (/ &
       "CLASSIC    ", "TWU        ", "MC         ", "GERG       " /)
  integer, parameter, dimension(nAlphaCorrs) :: alphaCorrNumParams = (/1,3,3,3/)


  integer, parameter :: nDegreePoly=2

  type :: mixExcessGibbs
    sequence
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
  end type mixExcessGibbs

  type :: Fraction
     sequence
     real, dimension(nDegreePoly+1):: pNum !<Numerator polynom
     real, dimension(nDegreePoly+1):: pDen !<Denominator polynom
  end type Fraction
  !< y = (pNum(1)+pNum(2)*x+...pNum(n-1)*x**n)/(pDen(1)+pDen(2)*x+...pDen(n-1)*x**n)
  !< where n = nDegreePoly.  Generally pDen(1) = 1.0

  type :: mixWongSandler
    sequence
    real, allocatable, dimension(:,:) :: alphaij
    type(Fraction), allocatable, dimension(:,:) :: f_kij, f_tauij
  end type mixWongSandler

  ! Allow array of pointers to NIST meos
  type :: nist_meos_ptr
    class(meos), pointer :: meos
  end type nist_meos_ptr

  type :: singleData
     !> Cubic EoS quantities for individual components
     sequence
     real :: omegaA, omegaB, omegaC, zcrit ! Allow for 3-paramter EOS
     real :: a !< Energy constant in front of alpha. Units: Pa*L^2/mol^2.
     real :: b !< Covolume parameter. Units: L/mol.
     real :: c !< Parameter in Patel-Teja EoS. Units: L/mol.

     real :: acf !< Acentric factor to use for the cubic EoS [-]
     real :: tc  !< Critical temperature to use in the cubic EoS [K]
     real :: pc  !< Critical pressure to use in the cubic EoS [Pa]

     ! Alpha correlations for a factor
     integer :: alphaMethod=-1
     character (len=12) :: alphaCorrName
     real :: alpha, dalphadt, d2alphadt2 !< alpha is dimensionless
     real :: alphaParams(3)=-1e10 ! Fitted coeffs in correlation. Max 3.

  end type singleData

  type :: eoscubic
     character (len=8) :: eosid
     character (len=12) :: mruleid
     character (len=20) :: name
     integer :: eosidx, mruleidx, subeosidx
     integer :: volumeShiftId = 0 !< 0: No volume shift, 1:Peneloux shift
     logical :: isElectrolyteEoS = .false. !< Used to enable electrolytes
     logical  :: cubic_verbose = .false.

     ! Parameters used in the "classical" alpha correlation in terms of the
     ! acentric factor
     real :: alfa, beta, gamma

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
     real, allocatable, dimension(:,:) :: kij
     real, allocatable, dimension(:,:) :: lowcase_bij
     logical :: simple_covolmixing
     type (mixExcessGibbs) :: mixGE
     type (mixWongSandler) :: mixWS

     ! Multiparameter equations of state for pure components
     type(eosmbwr), allocatable :: mbwr_meos(:)
     type(nist_meos_ptr), allocatable :: nist(:)

  end type eoscubic

end module eosdata
