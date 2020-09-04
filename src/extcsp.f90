!-----------------------------------------------------------------------------
!> Calulate shape factors for corresponding state method, using either a
!> cubic or a multiparameter EoS for the reference EoS, and a
!> cubic EoS for the shape factor EoS.
!>
!> \todo Need trace-component functionality.
!>
!> This module uses the convention that all quantities are measured in base
!> SI units, with the exception of density [mol/L] and molar volume [L/mol].
!-----------------------------------------------------------------------------

module csp
  use thermopack_constants
  use compdata, only: gendata_pointer
  use cubic_eos, only: cb_eos, cubic_eos_dealloc, assign_cubic_eos, &
       allocate_and_init_cubic_eos
  use tpmbwr, only: eosmbwr, initializembwrmodel
  use multiparameter_c3, only: meos_C3
  use stringmod, only: str_eq
  use multiparameter_base, only: meos
  use thermopack_var, only: base_eos_param, get_active_eos
  implicit none
  private
  save

  real, parameter, dimension(1) :: zRef = 1.0          !< This is the vector of mole numbers if we're using a cubic reference equation
  integer, parameter :: cubic = 1, mbwr = 2, nist = 3

  type shape_diff                                      !< Derivatives. Uses the notation on pages 115-120 in Michelsen & Mollerup.
    ! Shape factors
    real :: H,F
    real :: HT,FT,HTT,FTT
    real, allocatable, dimension(:) :: Hi,HiT,Fi,FiT
    real, allocatable, dimension(:,:) :: Hij,Fij
    real :: v0V,v0VV,t0T,t0TT
    real, allocatable, dimension(:) :: t0Ti,t0i,v0Vi,v0i
    real, allocatable, dimension(:,:) :: v0ij,t0ij
    real :: D, B
    real :: BT,DT,BTT,DTT
    real, allocatable, dimension(:) :: Bi,BiT,Di,DiT
    real, allocatable, dimension(:,:) :: Bij,Dij
    ! Reference EOS
    real :: M,Mt0,Mt0t0,Mt0v0,Mv0,Mv0v0
    ! Combined (derivatives of the reduced residual Helmholtz energy F(T,V,n) of the mixture)
    real :: FF
    real :: FF_T, FF_V, FF_TT, FF_VV, FF_TV
    real, allocatable, dimension(:) :: FF_i, FF_Ti, FF_Vi
    real, allocatable, dimension(:,:) :: FF_ij
    logical :: v0i_set = .false.
  contains
    procedure, public :: shape_diff_alloc
    procedure, public :: shape_diff_dealloc
    !procedure, public :: shape_diff_assign
    !generic, public :: assignment(=) => shape_diff_assign
  end type shape_diff

  type, extends(base_eos_param) :: extcsp_eos
    ! Shape factors
    type(cb_eos) :: shapeEos ! Cubic eos for actual components
    type(cb_eos) :: shapeEosRef ! Cubic eos for reference component
    ! Reference equations
    type(cb_eos) :: cbRefEos ! Cubic eos for reference
    type(eosmbwr), pointer :: mbwrRefEos => NULL()
    class(meos), pointer :: nistRefEos => NULL()
    ! Shape differentials etc.
    type(shape_diff) :: sd
    !
    integer :: refNc = 0                                 !< This is set to 1 in subroutine selectComp.
    type(gendata_pointer), allocatable, dimension(:) :: refComp  !< Will be made to have length refNc=1 in subroutine selectComp.
    integer :: refEosType                                !< Is set to either cubic or mbwr.
    real :: Tc0, Pc0                                     !< Critical constants for reference component.
    real :: m0, bc0, ac0                                 !< Parameters for the cubic shape eos.
  contains
    procedure, public :: dealloc => extcsp_eos_dealloc
    procedure, public :: allocate_and_init => extcsp_eos_allocate_and_init
    procedure, pass(This), public :: assign_eos => assign_extcsp_eos_set
  end type extcsp_eos

  public :: csp_init
  public :: shape_diff
  public :: csp_Zfac, csp_refPressure
  public :: csp_calcFres
  public :: csp_testpressure, csp_mainTestRoutine
  public :: extcsp_eos

contains

  !-----------------------------------------------------------------------------
  !> Init calculation for shape factors, by setting index of reference component
  !>
  !> \author MHA, 2013-11-27
  !> \author Ailo
  !-----------------------------------------------------------------------------
  subroutine csp_init(eos,nce,comps,refcomp_str,shEos,shMixRule,shAlpha,refEos,refAlpha) ! The input consists entirely of strings.
    use cbselect, only: SelectCubicEOS, SelectMixingRules
    use thermopack_constants, only: kRgas
    use tpmbwr, only: initializeMBWRmodel
    use eosdata
    use compdata, only: SelectComp
    use stringmod, only: str_upcase
    !use thermopack_var, only: nce, get_active_thermo_model, thermo_model
    implicit none
    class(extcsp_eos), intent(inout) :: eos
    integer, intent(in) :: nce
    type(gendata_pointer), allocatable, dimension(:), intent(inout) :: comps
    character(len=*), intent(in) :: refcomp_str         !< Reference component
    character(len=*), intent(in) :: shEos,refEos        !< shEos is any two-parameter (a,b) cubic equation of state
    character(len=*), intent(in) :: shMixRule
    character(len=*), intent(in) :: shAlpha
    character(len=*), intent(in), optional :: refAlpha  !< Needed if refEos is a cubic eos. Should not be present if one want to use an mbwr reference eos.
    ! Locals
    integer :: err
    character(len=eosid_len) :: complist_ref(1)
    character(len=len_trim(refEos)) :: refEos_upcase
    !
    refEos_upcase = trim(refEos)
    call str_upcase(refEos_upcase)
    if (refEos_upcase(1:2) .eq. "SR" .or. refEos_upcase(1:2) .eq. "PR") then ! Could in principle be any cubic eos...
      !Cubic reference equation
      if (.not. present(refAlpha)) then
        call stoperror('csp_init: need to input refAlpha')
      end if
    else if (refEos_upcase(1:4) == 'MBWR' .or. refEos_upcase == 'BENDER') then
      !MBWR reference equation
      eos%refEosType = mbwr
      select case(refEos_upcase)
      case ("MBWR19","MBWR20","BENDER")
        refEos_upcase = "MBWR19"
      case("MBWR32")
        refEos_upcase = "MBWR32"
      case default
        print *,'Incorrect EOS, ', trim(refEos), &
             ', defined for reference equation.'
        call stoperror('')
      end select
    else if ( str_eq(refEos,"C3_NIST") .or. str_eq(refEos,"NIST_MEOS") ) then ! Must allow same name as for single component eos
       if (.not. str_eq(refcomp_str,"C3")) then
          call stoperror('Only C3 can be used as reference component for NIST_MEOS')
       endif
       eos%refEosType = nist
       refEos_upcase = "NIST"
    else
      print *, 'Selected reference equation: ',  refEos, ' is invalid'
      print *, 'MBWR32, MBWR19, NIST_MEOS and SRK or PR are valid'
      call stoperror('Unknown reference equation')
    end if
    !
    call eos%allocate_and_init(nce,refEos_upcase//":"//trim(refcomp_str))
    complist_ref = trim(refcomp_str)
    eos%refNc = 1
    call SelectComp(complist_ref,eos%refNc,"DEFAULT",eos%refComp,err)
    !
    call get_eos_index("CSP-"//trim(shEos),eos%eosidx,eos%subeosidx)
    !
    ! Set reference component data
    eos%shapeEosRef%eosid = trim(shEos) ! Has to be set here if getAlphaTWUparams is to find the parameters
    call get_eos_index(shEos,eos%shapeEosRef%eosidx,eos%shapeEosRef%subeosidx)
    call SelectCubicEOS(eos%refNc, eos%refComp, eos%shapeEosRef, trim(shAlpha), "DEFAULT") ! Is only initialized to obtain m0, bc0 and ac0.
    if (.not. (eos%shapeEosRef%subeosidx == cbSRK &
         .or. eos%shapeEosRef%subeosidx == cbPR)) then
      print *,'Incorrect EOS, ', trim(shEos), &
           ', defined for shape factor calculation'
      call stoperror('')
    endif
    !
    eos%shapeEos%eosid = trim(shEos) ! Has to be set here if getAlphaTWUparams is to find the parameters
    eos%shapeEos%eosidx = eos%shapeEosRef%eosidx
    eos%shapeEos%subeosidx = eos%shapeEosRef%subeosidx
    call SelectCubicEOS(nce, comps, eos%shapeEos, &
         trim(shAlpha), "DEFAULT")
    call SelectMixingRules(nce, comps, eos%shapeEos, &
         trim(shMixRule), "DEFAULT")

    if (eos%refEosType == cubic) then
      !Cubic reference equation
      eos%cbrefEos%eosid = trim(refEos)
      call get_eos_index(refEos,eos%cbrefEos%eosidx,eos%cbrefEos%subeosidx)
      call SelectCubicEOS(eos%refNc, eos%refComp, eos%cbRefEos, trim(refAlpha), "DEFAULT")
      call SelectMixingRules(eos%refNc, eos%refComp, eos%cbRefEos, "VDW", "DEFAULT")
      ! Reference component data from database
      eos%Tc0 = eos%refComp(1)%p_comp%Tc
      eos%Pc0 = eos%refComp(1)%p_comp%Pc
    endif

    eos%m0 = eos%shapeEosRef%single(1)%alphaParams(1)
    eos%bc0 = eos%shapeEosRef%single(1)%omegaB*kRgas*eos%Tc0/eos%Pc0 !< constant bi in cubic eos
    eos%ac0 = eos%shapeEosRef%single(1)%omegaA*(kRgas*eos%Tc0)**2/eos%Pc0 !< constant ai in cubic eos
  end subroutine csp_init


  !-----------------------------------------------------------------------------
  !> Calculate shape factors from shape factor eos.
  !>
  !> Differentials are optional, but if present the derivatives of H and F and
  !> derivatives of v0 and t0 (wrt T,V,n_i) are computed to the second order.
  !>
  !> \author MHA, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine shape_factors(eos,H,F,T,n,sdiff)
    use cubic_eos, only: cb_eos
    use thermopack_var, only: nce
    implicit none
    class(extcsp_eos), intent(inout) :: eos !< CSP eos container
    real, intent(out) :: H !< Volume shape factor [mol]
    real, intent(out) :: F !< Temperature shape [mol]
    real, intent(in) :: T !< Temperature [K]
    real, dimension(nce), intent(in) :: n !< Molar values [mol]
    type(shape_diff), optional, intent(inout) :: sdiff

    call cubic_shape_factors(eos,H,F,T,n,sdiff)         ! This may seem weird, but maybe one later wants to add functionality for using non-cubic shape eos's.

  end subroutine shape_factors

  function temperatureShapeFactorF(eos,H,D,T,sumn) result(F)
    use cubic_eos, only: cbAlphaTwuIdx, is_classic_alpha
    use nonlinear_solvers, only: newton_1d
    implicit none
    class(extcsp_eos), intent(inout) :: eos !< CSP eos container
    real, intent(in) :: H    !< the volume shape factor
    real, intent(in) :: D    !< the mixture parameter associated with amix
    real, intent(in) :: T    !< temperature of the mixture
    real, intent(in) :: sumn !< total number of moles
    real :: F                !< the output of the function
    !locals
    real :: t0, t0r
    real :: param(4)
    param = (/H,D,T,sumn/)

    ! are we using the classical alpha corralation?
    if (is_classic_alpha(eos%shapeEosRef%single(1)%alphamethod)) then
      ! compute the temperature shape factor using the classical alpha formulation
      F = ((eos%m0*sqrt(T*sumn/eos%Tc0) + sqrt(D/(H*eos%ac0)))/(1.0 + eos%m0))**2
      !    print *,"F in temperatureShapeFactorF equals ", F
    else if (eos%shapeEosRef%single(1)%alphamethod .eq. cbAlphaTwuIdx) then
      ! if not, estimate t0 using F
      t0 = sumn*T/F
      t0r = t0/eos%tc0

      ! is the (approximate) reference temperature subcritical?
      if (t0r .gt. 1.0) print *, "TWU used for supercritical reference temperature..."
      ! is Twu correlation in database?
      if (eos%shapeEosRef%single(1)%alphaParams(1) .ne. 0.0 .and. &
          eos%shapeEosRef%single(1)%alphaParams(2) .ne. 0.0 .and. &
          eos%shapeEosRef%single(1)%alphaParams(3) .ne. 0.0) then
        F = newton_1d(fun=F_function_TWU,x0=F,param=param)
      endif
    else
      call stoperror('CSP::Wrong alpha formulation')
    end if
  end function temperatureShapeFactorF

  subroutine F_function_TWU(x,param,f,df)
    implicit none
    real, intent(in) :: x                                ! function argument x
    real, dimension(:), intent(in) :: param              ! equals (/H,D,T,sumn/)
    real, intent(out) :: f                               ! function value f(x)
    real, intent(out), optional :: df                    ! derivative value df(x)/dx
    ! local variables
    real :: H, D, T, sumn
    real :: A, B, C
    real :: L, M, N     ! Twu correlation has the form Tr^[N*(M-1)]*exp[L*(1-Tr^(NM))]
    real :: rhs
    real :: expo
    real :: ac0, tc0
    class(base_eos_param), pointer :: eos !< Eos container
    eos => get_active_eos()
    H = param(1)
    D = param(2)
    T = param(3)
    sumn = param(4)

    select type ( p_eos => eos )
    class is (extcsp_eos)
      L = p_eos%shapeEosRef%single(1)%alphaParams(1)
      M = p_eos%shapeEosRef%single(1)%alphaParams(2)
      N = p_eos%shapeEosRef%single(1)%alphaParams(3)
      ac0 = p_eos%ac0
      tc0 = p_eos%tc0
    end select
    C = M*N
    B = -L*(N*T/tc0)**C
    A = 1+N-C

    expo = exp(L + B*x**(-C))
    rhs = (tc0/(sumn*T))**(N*(M-1))*D/(ac0*H)

    ! compute f
    f = (x**A)*expo - rhs

    ! compute df
    if (present(df)) then
      df = expo*(A-B*C/x**C)*(x**(A-1))
    end if
  end subroutine F_function_TWU

  !-----------------------------------------------------------------------------
  !> Calculate shape factors from cubic shape eos.
  !>
  !> Differentials are optional
  !>
  !> \author MHA, 2013-11-28
  !-----------------------------------------------------------------------------
  subroutine cubic_shape_factors(eos,H,F,T,n,sdiff)
    use cubic_eos, only: cb_eos
    use thermopack_var, only: nce
    implicit none
    class(extcsp_eos), intent(inout) :: eos
    real, intent(out) :: H !< Volume shape factor [mol]
    real, intent(out) :: F !< Temperature shape factor [mol]
    real, intent(in) :: T !< Temperature [K]
    real, dimension(nce), intent(in) :: n !< Molar values [mol]
    type(shape_diff), optional, intent(inout) :: sdiff
    ! Locals
    real :: B, D, sumn
    integer :: i,j
    real :: T0,a0,a0T0,a0T0T0,denum

    sumn = sum(n)

    ! Fluid
    call mixture(eos%shapeEos,T,n,D,B,sdiff)
    H = B/eos%bc0                                ! does not depend on alpha formulation
    F = temperatureShapeFactorF(eos,H,D,T,sumn)   ! depends on alpha formulation

    ! H differentials
    if (present(sdiff)) then
      sdiff%H = H
      sdiff%F = F
      sdiff%HT = sdiff%BT/eos%bc0
      sdiff%Hi = sdiff%Bi/eos%bc0
      sdiff%HiT = sdiff%BiT/eos%bc0
      sdiff%Hij = sdiff%Bij/eos%bc0
      sdiff%HTT = sdiff%BTT/eos%bc0

      ! the factors 1000.0 because we measure v0 in units L/mol
      sdiff%v0V = 1.0/H*1000.0
      sdiff%v0VV = 0.0
      sdiff%v0Vi = -sdiff%Hi/H**2*1000.0
      sdiff%v0i = -sdiff%Hi/H
      sdiff%v0i_set = .false. ! Has to be multiplied by v0 to get the correct value. Done in another routine (calcCombinedDiff) when one knows v0.
      do i=1,nce
        do j=1,nce
          sdiff%v0ij(i,j) = (-sdiff%Bij(i,j)/B + 2.0*sdiff%Bi(i)*sdiff%Bi(j)/B**2) ! Has to be multiplied by v0 to get the correct value. Done in another routine when one knows v0.
        enddo
      enddo
      ! F and t0 differentials
      T0 = sumn*T/F
      call a0_diff(eos,T0,a0,a0T0,a0T0T0)
      denum = 1.0 - a0T0*T0/a0
      sdiff%FT = (sdiff%DT/D - a0T0*T0/(a0*T))/denum
      sdiff%Fi = (sdiff%Di/D - sdiff%Bi/B - a0T0*T0/(a0*sumn))/denum
      sdiff%t0T = (1.0/T - sdiff%FT)*T0
      sdiff%FTT = (sdiff%DTT/D - a0T0T0*sdiff%t0T**2/a0)/denum
      sdiff%t0i = (1.0/sumn - sdiff%Fi)*T0
      sdiff%FiT = (sdiff%DiT/D - sdiff%FT*sdiff%Hi/H &
           - sdiff%Hi*a0T0*sdiff%t0T/(H*a0)&
           - a0T0T0*sdiff%t0T*sdiff%t0i/a0&
           - a0T0/(F*a0))/denum
      do i=1,nce
        do j=1,nce
          sdiff%Fij(i,j) = (sdiff%Dij(i,j)/D - sdiff%Fi(i)*sdiff%Hi(j)/H &
               - sdiff%Fi(j)*sdiff%Hi(i)/H - sdiff%Hij(i,j)/H&
               - sdiff%Hi(i)*a0T0*sdiff%t0i(j)/(H*a0)&
               - sdiff%Hi(j)*a0T0*sdiff%t0i(i)/(H*a0)&
               - a0T0T0*sdiff%t0i(i)*sdiff%t0i(j)/a0)/denum
        enddo
      enddo
      sdiff%t0TT = -sdiff%FTT*T0 - 2.0*sdiff%FT*sdiff%t0T
      sdiff%t0Ti = -sdiff%FiT*T0 - sdiff%FT*sdiff%t0i - sdiff%Fi*sdiff%t0T + 1.0/F
      do i=1,nce
        do j=1,nce
          sdiff%t0ij(i,j) = -sdiff%Fij(i,j)*T0 - sdiff%Fi(i)*sdiff%t0i(j) - sdiff%Fi(j)*sdiff%t0i(i)
        enddo
      enddo

      sdiff%FT = sdiff%FT * F
      sdiff%Fi = sdiff%Fi * F
      sdiff%FiT = sdiff%FiT * F
      sdiff%Fij = sdiff%Fij * F
      sdiff%FTT = sdiff%FTT * F
    endif

  end subroutine cubic_shape_factors


  !-----------------------------------------------------------------------------
  !> Calculate mixture rules (D and B)
  !>
  !> \author MHA, 2013-11-28
  !-----------------------------------------------------------------------------
  subroutine mixture(cbeos,T,n,D,B,sdiff)
    !use optimizers, only: optimize, optim_param, setX
    use thermopack_var, only: nce
    use tpcbmix, only: cbCalcMixtureParams
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos !< The cubic equation for the shape factors
    real, intent(out) :: D !< Volume shape factor [mol]
    real, intent(out) :: B !< Temperature shape factor [mol]
    real, intent(in) ::  T !< Temperature [K]
    real, dimension(nce), intent(in) :: n !< Molar values [mol]
    type(shape_diff), optional, intent(inout) :: sdiff
    call cbCalcMixtureParams(nce,cbeos,t,n)
    D = cbeos%suma
    B = cbeos%sumb
    if (present(sdiff)) then
      sdiff%D = cbeos%suma
      sdiff%B = cbeos%sumb
      sdiff%DT = cbeos%at
      sdiff%BT = 0.0 !cbeos%bt
      sdiff%Di = cbeos%ai
      sdiff%Bi = cbeos%bi
      sdiff%DiT = cbeos%ait
      sdiff%BiT = 0.0 !cbeos%bit
      sdiff%Dij = cbeos%aij
      sdiff%Bij = cbeos%bij
      sdiff%DTT = cbeos%att
      sdiff%BTT = 0.0 !cbeos%btt
    end if

  end subroutine mixture

  !-----------------------------------------------------------------------------
  !> Calculate a0 with differentials in the case where a0 takes the SRK-form
  !> a0 = ac0*(1+m0-m0*sqrt(Tr))^2.
  !>
  !> If other alpha-formulations are to implemented in the csp framework,
  !> this function has to be modified.
  !>
  !> \author MH, 2013-11-28
  !-----------------------------------------------------------------------------
  subroutine a0_diff(eos,T0,a0,a0T0,a0T0T0)
    !    use tpcbmix, only: cbCalcAlphaTerm
    implicit none
    class(extcsp_eos), intent(in) :: eos !< CSP eos container
    real, intent(out) :: a0,a0T0,a0T0T0 !< Differentials of a0
    real, intent(in) :: T0 !< Temperature [K]
    ! Locals
    real :: sqrtT0Tc0, var, dvar, d2var
    sqrtT0Tc0 = sqrt(T0/eos%Tc0)
    var = 1.0 + eos%m0 - eos%m0*sqrtT0Tc0
    dvar = - 0.5*eos%m0*sqrtT0Tc0/T0
    d2var = 0.25*eos%m0*sqrtT0Tc0/(T0*T0)
    a0 = eos%ac0*var**2
    a0T0 = 2.0*eos%ac0*var*dvar
    a0T0T0 = 2.0*eos%ac0*(dvar**2 + var*d2var)

    ! ! Somehow, using the method below doesn't pass the supertest anymore... Can't use TWU alpha correlation before this is fixed.
    ! ! Calculates alpha, dalphadt and d2alphadt2, and stores them in the cbeos struct. If one has chosen the TWU correlation, but it doesn't exist in the database for the reference component, the algorithm falls back on the classic SRK alpha formulation.
    ! call cbCalcAlphaTerm(nc=1, comp=refComp(1), cbeos=shapeEos(1), T=T0)
    ! a0 = ac0*shapeEos(1)%alpha(1)
    ! a0T0 = ac0*shapeEos(1)%dalphadT(1)
    ! a0T0T0 = ac0*shapeEos(1)%d2alphadT2(1)
  end subroutine a0_diff

  !-----------------------------------------------------------------------------
  !> Allocate differential struct
  !>
  !> \author MH, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine shape_diff_alloc(sdiff,nce)
    implicit none
    class(shape_diff), intent(inout) :: sdiff
    integer, intent(in) :: nce
    integer :: stat

    call shape_diff_dealloc(sdiff)

    allocate(sdiff%Hi(nce),sdiff%HiT(nce),sdiff%Fi(nce),sdiff%FiT(nce),&
         sdiff%Hij(nce,nce),sdiff%Fij(nce,nce),sdiff%Bi(nce),sdiff%BiT(nce),&
         sdiff%Di(nce),sdiff%DiT(nce),sdiff%Bij(nce,nce),sdiff%Dij(nce,nce),&
         sdiff%t0Ti(nce),sdiff%t0i(nce),sdiff%v0Vi(nce),sdiff%v0i(nce),&
         sdiff%v0ij(nce,nce),sdiff%t0ij(nce,nce),sdiff%FF_i(nce),&
         sdiff%FF_Ti(nce),sdiff%FF_Vi(nce),sdiff%FF_ij(nce,nce),STAT=stat)
    if (stat /= 0) write (*,*) 'Error allocating shape differential struct'

    sdiff%Hi = 0
    sdiff%HiT = 0
    sdiff%Fi = 0
    sdiff%FiT = 0
    sdiff%Hij = 0
    sdiff%Fij = 0
    sdiff%Bi = 0
    sdiff%BiT = 0
    sdiff%Di = 0
    sdiff%DiT = 0
    sdiff%Bij = 0
    sdiff%Dij = 0
    sdiff%t0Ti = 0
    sdiff%t0i = 0
    sdiff%v0Vi = 0
    sdiff%v0i = 0
    sdiff%v0ij = 0
    sdiff%t0ij = 0
    sdiff%FF_i = 0
    sdiff%FF_Ti = 0
    sdiff%FF_Vi = 0
    sdiff%FF_ij = 0
  end subroutine shape_diff_alloc

  !-----------------------------------------------------------------------------
  !> Deallocate differential struct
  !>
  !> \author MH, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine shape_diff_dealloc(sdiff)
    implicit none
    class(shape_diff), intent(inout) :: sdiff
    integer :: stat
    stat = 0
    if (allocated (sdiff%Hi)) deallocate (sdiff%Hi,STAT=stat)
    if (stat /= 0) then
      write (*,*) 'Error deallocating shape differential Hi'
      stat = 0
    endif

    if (allocated (sdiff%HiT)) deallocate (sdiff%HiT,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential HiT'
      stat = 0
    endif

    if (allocated (sdiff%Fi)) deallocate (sdiff%Fi,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Fi'
      stat = 0
    endif

    if (allocated (sdiff%FiT)) deallocate (sdiff%FiT,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential FiT'
      stat = 0
    endif

    if (allocated (sdiff%Hij)) deallocate (sdiff%Hij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Hij'
      stat = 0
    endif

    if (allocated (sdiff%Fij)) deallocate (sdiff%Fij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Fij'
      stat = 0
    endif

    if (allocated (sdiff%t0Ti)) deallocate (sdiff%t0Ti,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential t0Ti'
      stat = 0
    endif

    if (allocated (sdiff%t0i)) deallocate (sdiff%t0i,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential t0i'
      stat = 0
    endif

    if (allocated (sdiff%v0Vi)) deallocate (sdiff%v0Vi,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential v0Vi'
      stat = 0
    endif

    if (allocated (sdiff%v0i)) deallocate (sdiff%v0i,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential v0i'
      stat = 0
    endif

    if (allocated (sdiff%v0ij)) deallocate (sdiff%v0ij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential v0ij'
      stat = 0
    endif

    if (allocated (sdiff%t0ij)) deallocate (sdiff%t0ij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential t0ij'
      stat = 0
    endif

    if (allocated (sdiff%Bi)) deallocate (sdiff%Bi,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Bi'
      stat = 0
    endif

    if (allocated (sdiff%BiT)) deallocate (sdiff%BiT,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential BiT'
      stat = 0
    endif

    if (allocated (sdiff%Bij)) deallocate (sdiff%Bij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Bij'
      stat = 0
    endif

    if (allocated (sdiff%Di)) deallocate (sdiff%Di,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Di'
      stat = 0
    endif

    if (allocated (sdiff%DiT)) deallocate (sdiff%DiT,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential DiT'
      stat = 0
    endif

    if (allocated (sdiff%Dij)) deallocate (sdiff%Dij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential Dij'
      stat = 0
    endif

    if (allocated (sdiff%FF_i)) deallocate (sdiff%FF_i,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential FF_i'
      stat = 0
    endif

    if (allocated (sdiff%FF_Ti)) deallocate (sdiff%FF_Ti,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential F_Ti'
      stat = 0
    endif

    if (allocated (sdiff%FF_ij)) deallocate (sdiff%FF_ij,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential FF_ij'
      stat = 0
    endif

    if (allocated (sdiff%FF_Vi)) deallocate (sdiff%FF_Vi,STAT=stat)
    if (stat /= 0)  then
      write (*,*) 'Error deallocating shape differential FF_Vi'
      stat = 0
    endif

  end subroutine shape_diff_dealloc

  !-----------------------------------------------------------------------------
  !> Calculate Z-factor of the mixture.
  !>
  !> \author MH, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine csp_Zfac(eos,T,P,n,phase,zFac,dZdt,dZdp,dZdz)
    use thermopack_var, only: nce
    use thermopack_constants, only: Rgas, kRgas
    implicit none
    class(extcsp_eos), intent(inout) :: eos !< CSP eos
    real, intent(in) :: P !< Volume shape factor [mol]
    real, intent(in) :: T !< Temperature [K]
    integer, intent(in) :: phase
    real, dimension(nce), intent(in) :: n !< Molar values [mol]
    real, intent(out) :: zFac !<  [-]
    real, intent(out), optional :: dZdt,dZdp,dZdz(nce)
    ! Locals
    real :: dPdT, dPdv, dvdT, dvdn(nce)
    real, dimension(nce) :: dPdn
    real :: P0, T0, sumn, H, F, V, v0
    if (present(dZdt) .or. present(dZdp) .or. present(dZdz)) then
      call shape_factors(eos,H,F,T,n,eos%sd)
    else
      call shape_factors(eos,H,F,T,n)
    endif
    sumn = sum(n)
    P0 = H*P/F
    T0 = T*sumn/F

    zFac = solveRefEqZfac(eos,T0,P0,phase)    ! Z-factor for the reference equation. Also equals the Z-factor for the real mixture.
    V = sumn*zFac*Rgas*T/P                         ! The volume for the reference fluid [m^3].
    if (present(dZdt) .or. present(dZdp) .or. present(dZdz)) then
      v0 = zFac*kRgas*T0/P0
      call calcRefEqDiff(eos,T0,v0,eos%sd)
      call calcCombinedDiff(T0,v0,n,eos%sd)
      dPdV = -Rgas*T*(eos%sd%FF_VV + sumn/V**2)
      if (present(dZdt)) then
        dPdT = P/T - Rgas*T*eos%sd%FF_TV
        dvdT = -dPdT/dPdv
        dZdT = -zFac*(1.0/T - dvdT/V)
      endif
      if (present(dZdp)) then
        dZdP = zFac*(1.0/P + 1.0/(dPdv*V))
      endif
      if (present(dZdz)) then
        dPdn = Rgas*T*(-eos%sd%FF_Vi + 1/V)
        dVdn = -dPdn/dPdv
        dZdz = -zFac*(1.0/sumn - dVdn/V)
      endif
    endif
  end subroutine csp_Zfac

  !-----------------------------------------------------------------------------
  !> Calculate the derivatives of M -- the residual molar Helmholtz
  !> energy of the reference substance -- with respect to reference variables
  !> t0 and v0.
  !>
  !> \author MH, 2013-11-27
  !> \author Ailo A, 2014-12
  !-----------------------------------------------------------------------------
  subroutine calcRefEqDiff(eos,T0,v0,sdiff)
    use tpcubic, only: cbCalcDerivatives_svol
    use thermopack_constants, only: Rgas
    use tpmbwr_additional, only: alphar_derivatives
    use cbHelm
    implicit none
    class(extcsp_eos), intent(inout) :: eos
    real, intent(in) :: T0 !< Reference fluid temperature [K]
    real, intent(in) :: v0 !< Reference fluid pressure [l/mol]
    type(shape_diff), intent(inout) :: sdiff
    !locals
    real :: rho
    real, dimension(0:2) :: alphaDerivatives
    real :: alpha, DalphaDrho, D2alphaDrho2
    real :: DalphaDT, D2alphaDrhoDT
    real :: D2alphaDT2
    real :: alpr,alpr_T,alpr_v,alpr_TT,alpr_Tv,alpr_vv

    if (eos%refEosType .eq. cubic) then
      call cbCalcDerivatives_svol(eos%refNc,eos%cbrefEos,T0,v0)
      sdiff%M = cbF(eos%cbrefEos)*(Rgas*T0)
      sdiff%Mt0 = Rgas*(cbF(eos%cbrefEos) + T0*cbFT(eos%cbrefEos))
      sdiff%Mt0t0 = Rgas*(2*cbFT(eos%cbrefEos) + T0*cbFTT(eos%cbrefEos))
      sdiff%Mt0v0 = Rgas*(cbFV(eos%cbrefEos) + T0*cbFVT(eos%cbrefEos))
      sdiff%Mv0 = Rgas*T0*cbFV(eos%cbrefEos)
      sdiff%Mv0v0 = Rgas*T0*cbFVV(eos%cbrefEos)
    else if (eos%refEosType .eq. mbwr) then ! alpha is the reduced residual molar Helmholtz energy.

      rho = 1.0/v0
      call alphar_derivatives(deriv=alphaDerivatives,t=T0,rho_SI=rho*1e3,&
           nTderivs=0,nRhoDerivs=2,model=eos%mbwrRefEos) ! compute alpha, alpha_rho0, alpha_rho0rho0
      alpha = alphaDerivatives(0)
      DalphaDrho = alphaDerivatives(1)
      D2alphaDrho2 = alphaDerivatives(2)

      sdiff%M = Rgas*T0*alpha
      sdiff%Mv0 = -Rgas*T0*DalphaDrho*rho**2
      sdiff%Mv0v0 = Rgas*T0*(D2alphaDrho2*rho**4 + 2*DalphaDrho*rho**3)
      call alphar_derivatives(deriv=alphaDerivatives,t=T0,rho_SI=rho*1e3,&
           nTderivs=1,nRhoDerivs=1,model=eos%mbwrRefEos) ! compute alpha_T0, alpha_T0rho0
      DalphaDT = alphaDerivatives(0)
      D2alphaDrhoDT = alphaDerivatives(1)

      sdiff%Mt0 = Rgas*(alpha + T0*DalphaDT)
      sdiff%Mt0v0 = -Rgas*(DalphaDrho + T0*D2alphaDrhoDT)*rho**2
      call alphar_derivatives(deriv=alphaDerivatives,t=T0,rho_SI=rho*1e3, &
           nTderivs=2,nRhoDerivs=0,model=eos%mbwrRefEos) ! compute alpha_T0T0
      D2alphaDT2 = alphaDerivatives(0)

      sdiff%Mt0t0 = Rgas*(2*DalphaDT + T0*D2alphaDT2)

      sdiff%M = alpr*(Rgas*T0)
      sdiff%Mt0 = Rgas*(alpr + T0*alpr_T)
      sdiff%Mt0t0 = Rgas*(2*alpr_T + T0*alpr_TT)
      sdiff%Mt0v0 = Rgas*(alpr_v + T0*alpr_Tv)*1e-3
      sdiff%Mv0 = Rgas*T0*alpr_v*1e-3
      sdiff%Mv0v0 = Rgas*T0*alpr_vv*1e-6
    else if (eos%refEosType .eq. nist) then ! this multiparameter EoS operates in SI units
       call eos%nistRefEos%alphaResDerivs_Tv(T0,v0*1e-3,alpr,alpr_T,alpr_v,alpr_TT,alpr_Tv,alpr_vv)
       sdiff%M = alpr*(Rgas*T0)
       sdiff%Mt0 = Rgas*(alpr + T0*alpr_T)
       sdiff%Mt0t0 = Rgas*(2*alpr_T + T0*alpr_TT)
       sdiff%Mt0v0 = Rgas*(alpr_v + T0*alpr_Tv)*1e-3
       sdiff%Mv0 = Rgas*T0*alpr_v*1e-3
       sdiff%Mv0v0 = Rgas*T0*alpr_vv*1e-6
    else
      stop "refEosType wrong"
    end if
  end subroutine calcRefEqDiff

  !-----------------------------------------------------------------------------
  !> Calculate Z-factor of the reference equation.
  !>
  !> \author MH, 2013-11-27
  !-----------------------------------------------------------------------------
  function solveRefEqZfac(eos,T0,P0,phase) result(zFac)
    use tpcubic
    use tpmbwr_additional, only: mbwr_volume
    use thermopack_constants, only: Rgas
    implicit none
    !input
    class(extcsp_eos), intent(inout) :: eos !< CSP eos
    real, intent(in) :: P0 !< Pressure of the reference fluid [Pa]
    real, intent(in) :: T0 !< Temperature of the reference fluid [K]
    integer, intent(in) :: phase
    !output
    real :: zFac !<  [-]
    !locals
    real :: v0 ! specific volume [L/mol]
    real :: rho0 ! specific density (mol/m^3)
    if (eos%refEosType .eq. cubic) then
      call cbCalcZfac(eos%refNc,eos%cbrefEos,T0,P0,zRef,phase,zFac,1)
    else if (eos%refEosType .eq. mbwr) then
      v0 = mbwr_volume(T0,P0,nMoles=1.0,phase=phase,model=eos%mbwrRefEos)
      zFac = P0*v0/(Rgas*T0) ! Need to use kRgas here since V is calculated in litres.
    else if (eos%refEosType .eq. nist) then
      call eos%nistRefEos%densitySolver(T0,P0,phase,rho0)
      zFac = P0/(rho0*Rgas*T0) ! Need to use Rgas here since rho is calculated in mol/m^3..
    else
      call stoperror("refEosType wrong")
    end if
  end function solveRefEqZfac

  !-----------------------------------------------------------------------------
  !> Solve reference equation for p0 for given real temperature and pressure
  !>
  !> \author MH, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine csp_refPressure(eos,T0,v0,n,P0,dp0dv0,dp0dt0)
    use tpcubic
    use thermopack_var, only: nce
    use tpmbwr, only: makeParam, MBWR_pressure
    implicit none
    ! input
    class(extcsp_eos), intent(inout) :: eos !< CSP eos
    real, intent(in) :: T0 !< Reference fluid temperature [K]
    real, intent(in) :: v0 !< Reference fluid molar volume [L/mol]
    real, dimension(nce), intent(in) :: n !< Mole composition of mixture [mol]
    ! ouput
    real, intent(out) :: P0 !< Pressure [Pa]
    real, optional, intent(out) :: dp0dv0, dp0dt0 !< Pressure differentials [Pa*mol/L], [Pa/K]
    ! Locals
    real :: rho0, dp0drho0

    ! Evaluate reference equation
    if (eos%refEosType .eq. cubic) then
      ! Only calculate what is strictly necessary. (Why is n input here?)
      call cbCalcPressure(eos%refNc,eos%cbrefEos,T0,v0,n,P0,dp0dv0,dp0dt0)
    else if (eos%refEosType .eq. mbwr) then
      ! Only calculate what is strictly necessary.
      if (.not. (present(dp0dv0) .or. present(dp0dt0))) then
        call makeParam(parameters=eos%mbwrRefEos%mbwrParameters,&
             t=T0,model=eos%mbwrRefEos,nTderivatives=0)
        call MBWR_pressure(rho=1/v0, param=eos%mbwrRefEos%mbwrParameters, p=P0)
      else
        if (present(dp0dv0)) then
          call makeParam(parameters=eos%mbwrRefEos%mbwrParameters,&
               t=T0,model=eos%mbwrRefEos,nTderivatives=0)
          call MBWR_pressure(rho=1/v0,param=eos%mbwrRefEos%mbwrParameters,&
               p=P0,dpdrho=dp0dv0)
          dp0dv0 = -dp0dv0/v0**2
        end if
        if (present(dp0dt0)) then
          call makeParam(parameters=eos%mbwrRefEos%mbwrParameters,&
               t=T0,model=eos%mbwrRefEos,nTderivatives=1)
          call MBWR_pressure(rho=1/v0, param=eos%mbwrRefEos%mbwrParameters, P=dp0dt0)
        end if
      end if
    else if ( eos%refEosType .eq. nist ) then
      rho0 = 1e3/v0
      call eos%nistRefEos%mp_pressure(rho=rho0,t=T0,p=P0,p_rho=dp0drho0,p_T=dp0dt0)
      if (present(dp0dv0)) then
        dp0dv0 = (-rho0**2)*dp0drho0 !< dp0dv0 where [v0] = m^3/mol
        dp0dv0 = dp0dv0*1e-3 !< dp0dv0 where [v0] = L/mol
      end if
    else
      stop "refEosType wrong"
    end if
  end subroutine csp_refPressure

  !-----------------------------------------------------------------------------
  !> Computes the pressure and its derivatives for the mixture. The routine
  !> starts from scratch, and so it doesn't benefit from e.g. already calculated
  !> shape factors.
  !> \author
  !> Ailo A, 2015-01
  !-----------------------------------------------------------------------------
  subroutine csp_mixtPressure(eos,T,v,n,P,dPdV,dPdT,dpdz)
    use thermopack_var, only: nce
    use thermopack_constants, only: Rgas
    implicit none
    ! in/out
    class(extcsp_eos), intent(inout) :: eos !< CSP eos
    ! input
    real, intent(in) :: T                        !< Mixture temperature [K]
    real, intent(in) :: v                        !< Mixture molar volume [m3/mol]
    real, dimension(nce), intent(in) :: n         !< Mole composition of mixture [mol]
    ! output
    real, intent(out) :: P                       !< Pressure [Pa]
    real, optional, intent(out) :: dPdv, dPdT  !< Pressure differentials [Pa*mol/m^3], [Pa/K], [Pa*mol^2/m^6]
    real, dimension(nce), optional, intent(out) :: dpdz !< Pressure differentials [PaYmol]
    ! Locals
    real :: H, F, P0, T0, v0, sumn
    real :: V_m3
    sumn = sum(n)

    V_m3 = v!*sumn ! volume [m3]
    ! compute shape factors and their differentials
    call shape_factors(eos,H,F,T,n,eos%sd)
    ! compute v0 and T0
    v0 = 1e3*V_m3/H  ! must multiply by 1e3 to get liters/mol
    T0 = sumn*T/F
    ! compute P0
    call csp_refPressure(eos,T0,v0,n,P0)
    ! calculate red. res. Helmholtz energy F and its derivatives
    call calcRefEqDiff(eos,T0,v0,eos%sd)
    call calcCombinedDiff(T0,v0,n,eos%sd)
    ! calculate P and its derivatives
    P = Rgas*T*(-eos%sd%FF_V + sumn/V_m3)
    if (present(dPdT)) dPdT = P/T - Rgas*T*eos%sd%FF_TV
    if (present(dPdv)) dPdV = -Rgas*T*(eos%sd%FF_VV + sumn/V_m3**2) ! Pa*mol/m3

    if (present(dPdz)) then
      dPdz = Rgas*T*(-eos%sd%FF_Vi + 1.0/V_m3)
    endif
  end subroutine csp_mixtPressure

  subroutine checkStateFunctionDerivatives(StateFunction,eos,T,P,n,phase_in)
    use thermopack_var, only: nce
    implicit none
    !input
    interface
      subroutine StateFunction(eos,T,P,n,phase,G,dGdt,dGdp,dGdn)
        use thermopack_var, only: nce
        use thermopack_constants, only: Rgas
        import extcsp_eos
        implicit none
        class(extcsp_eos), intent(inout) :: eos !< CSP eos
        real, intent(in) :: P !< Pressure [Pa]
        real, intent(in) :: T !< Temperature [K]
        integer, intent(in) :: phase !< Phase identifier [-]
        real, dimension(nce), intent(in) :: n !< Composition [mol]
        real, intent(out) :: G !< Gibbs free energy [J/mol]
        real, optional, intent(out) :: dGdt, dGdp
        real, dimension(nce), optional, intent(out) :: dGdn
      end subroutine StateFunction
    end interface
    class(extcsp_eos), intent(inout) :: eos !< CSP eos
    real, intent(in) :: T
    real, intent(in) :: P
    real, dimension(nce), intent(in) :: n
    integer, intent(in) :: phase_in     ! needed in the density solver
    !local variables
    real :: dt, dp, dn
    real :: eps_t, eps_p, releps_t, releps_p
    real :: b, dbdt, dbdp, b_mod
    real, dimension(nce) :: dbdn, eps_n, releps_n, n_temp
    integer :: i ! iterator

    dt = T*1e-6
    dp = P*1e-6
    dn = 1e-6

    releps_t = 0.0
    releps_p = 0.0
    releps_n = 0.0

    call StateFunction(eos,T,P,n,phase_in,b,dbdt,dbdp,dbdn)
    print *, "value of state function:", b

    ! CHECK T-DERIVATIVE
    call StateFunction(eos,T+dt,P,n,phase_in,b_mod)
    print *, "numder t = ", (b_mod-b)/dt
    eps_t = abs((b_mod-b)/dt - dbdt )
    if (dbdt .ne. 0.0) releps_t = eps_t/dbdt
    ! CHECK P-DERIVATIVE
    call StateFunction(eos,T,P+dp,n,phase_in,b_mod)
    print *, "numder p = ", (b_mod-b)/dp
    eps_p = abs((b_mod-b)/dp - dbdp )
    if (dbdp .ne. 0.0) releps_p = eps_p/dbdp
    ! CHECK n-DERIVATIVE
    do i = 1, nce
      n_temp = n
      n_temp(i) = n_temp(i) + dn
      call StateFunction(eos,T,P,n_temp,phase_in,b_mod)
      print *, "numder",i," = ", (b_mod-b)/dn
      eps_n(i) = abs((b_mod-b)/dn - dbdn(i) )
      if (dbdn(i) .ne. 0.0) releps_n = eps_n/dbdn
    end do
    print *, "STATE FUNCTIONS CHECK, eps, releps, dbdx"
    print *, "T",eps_t, releps_t, dbdt
    print *, "P",eps_p, releps_p, dbdp
    do i = 1,nce
      print *, "ni",eps_n(i), releps_n(i), dbdn(i)
    end do
  end subroutine checkStateFunctionDerivatives

  !-----------------------------------------------------------------------------
  !> Combine shape factor and reference equation differentials to give the
  !> derivatives of the residual Helmholtz function A^r(T,V,n) of the mixture.
  !>
  !> \author MH, 2013-12-01
  !-----------------------------------------------------------------------------
  subroutine calcCombinedDiff(T0,v0,n,sdiff)
    use thermopack_constants, only: Rgas
    use thermopack_var, only: nce
    implicit none
    real, intent(in) :: v0 !< Reference fluid specific volume [l/mol]
    real, intent(in) :: T0 !< Reference fluid temperatureature [K]
    real, dimension(nce), intent(in) :: n !< Molar values of mixture [mole]
    type(shape_diff), intent(inout) :: sdiff
    ! Locals
    real :: M, Mt, Mtt, Mtv, Mv, Mvv
    real, dimension(nce) :: Mi, MTi, Mvi
    real, dimension(nce,nce) :: Mij
    integer :: i,j
    real :: n_rt0, n_rt02, one_rt0, one_rt02, twon_RT02, twon_rt03

    if (sdiff%v0i_set .eqv. .false.) then
      sdiff%v0i = sdiff%v0i * v0
      sdiff%v0ij = sdiff%v0ij * v0
      sdiff%v0i_set = .true.
    end if

    M = sdiff%M
    Mt = sdiff%Mt0*sdiff%t0T;                             !sdiff%Mt = Mt
    Mv = sdiff%Mv0*sdiff%v0V;                             !sdiff%Mv = Mv
    Mi = sdiff%Mv0*sdiff%v0i + sdiff%Mt0*sdiff%t0i;       !sdiff%Mi = Mi
    Mtt = sdiff%Mt0t0*sdiff%t0T**2 + sdiff%Mt0*sdiff%t0TT
    Mvv = sdiff%Mv0v0*sdiff%v0V**2 + sdiff%Mv0*sdiff%v0VV
    Mtv = sdiff%Mt0v0*sdiff%t0T*sdiff%v0V
    do i=1,nce
      do j=1,nce
        Mij(i,j) = sdiff%Mt0t0*sdiff%t0i(i)*sdiff%t0i(j) + sdiff%Mt0*sdiff%t0ij(i,j)&
             + sdiff%Mv0v0*sdiff%v0i(i)*sdiff%v0i(j) + sdiff%Mv0*sdiff%v0ij(i,j)&
             + sdiff%Mt0v0*(sdiff%t0i(i)*sdiff%v0i(j) + sdiff%t0i(j)*sdiff%v0i(i))
      enddo
    enddo
    MTi = sdiff%Mt0t0*sdiff%t0T*sdiff%t0i + sdiff%Mt0v0*sdiff%t0T*sdiff%v0i + sdiff%Mt0*sdiff%t0Ti
    Mvi = sdiff%Mv0v0*sdiff%v0V*sdiff%v0i + sdiff%Mt0v0*sdiff%t0i*sdiff%v0V + sdiff%Mv0*sdiff%v0Vi

    ! precalculate some quantities
    one_RT0 = 1/(Rgas*T0)
    one_RT02 = one_RT0/T0
    n_RT0 = sum(n)*one_RT0
    n_RT02 = n_RT0/T0
    twon_RT02 = 2*n_RT02
    twon_RT03 = twon_RT02/T0

    sdiff%FF = n_RT0*M
    sdiff%FF_T = -n_RT02*sdiff%t0T*M + n_RT0*Mt
    sdiff%FF_V = n_RT0*Mv
    sdiff%FF_i = (one_RT0 - n_RT02*sdiff%t0i)*M + n_RT0*Mi ! whole array operations
    sdiff%FF_TT = (twon_RT03*sdiff%t0T**2 - n_RT02*sdiff%t0TT)*M - twon_RT02*sdiff%t0T*Mt + n_RT0*Mtt
    sdiff%FF_VV = n_RT0*Mvv
    do i=1,nce
      do j=1,nce
        sdiff%FF_ij(i,j) = (-one_RT02*(sdiff%t0i(i)+sdiff%t0i(j)) &
             + twon_RT03*sdiff%t0i(i)*sdiff%t0i(j) - n_RT02*sdiff%t0ij(i,j))*M &
             + (one_RT0 - n_RT02*sdiff%t0i(i))*Mi(j) + (one_RT0 - n_RT02*sdiff%t0i(j))*Mi(i) &
             + n_RT0*Mij(i,j)
      enddo
    enddo
    sdiff%FF_TV = -n_RT02*sdiff%t0T*Mv + n_RT0*Mtv
    sdiff%FF_Ti = (-one_RT02*sdiff%t0T + twon_RT03*sdiff%t0T*sdiff%t0i - n_RT02*sdiff%t0Ti)*M &
         + (one_RT0 - n_RT02*sdiff%t0i)*Mt - n_RT02*sdiff%t0T*Mi + n_RT0*MTi
    sdiff%FF_Vi = (one_RT0 - n_RT02*sdiff%t0i)*Mv + n_RT0*Mvi

  end subroutine calcCombinedDiff

  subroutine csp_mainTestRoutine()
    use thermopack_var, only: nce, get_active_eos, base_eos_param
    implicit none
    real :: T, P, v, n(2)
    class(base_eos_param), pointer :: act_eos_ptr
    act_eos_ptr => get_active_eos()
    T = 350.0
    v = 0.85 ! L/mol
    P = 1.0e6
    n(1) = 0.7
    n(2) = 0.9

    ! COMMENT OUT ALL BUT THE INTERESTING TEST
    print *,"*********CALLING TEST_SHAPE_FACTORS*********"
    call test_shape_factors(nce,T,P,n,2)
    print *,"*********CALLING CSP_TESTPRESSURE*********"
    call csp_testPressure(T,v,n)
    print *,"*********CALLING CHECKSTATEFUNCTIONDERIVATIVES*********"
    print *,"********* ZFAC *********"
    select type (p_eos => act_eos_ptr)
    class is(extcsp_eos)
      call checkStateFunctionDerivatives(csp_zFac,p_eos,T,P,n,2)
    end select
    stop "TESTING FINISHED"
  end subroutine csp_mainTestRoutine

  !-----------------------------------------------------------------------------
  !> Compare analytical and numerical derivatives in the sdiff struct.
  !>
  !> \author MH, 2013-12-11
  !> \author Ailo A, 2014-12-04
  !-----------------------------------------------------------------------------
  subroutine test_shape_factors(nce,T,P,n,phase)
    use thermopack_constants, only: Rgas, kRgas
    use tpcubic, only: cbCalcPressure
    use thermopack_var, only: get_active_eos, base_eos_param
    implicit none
    integer, intent(in) :: nce !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, dimension(nce), intent(inout) :: n !< Molar values [mol]
    integer, intent(in) :: phase

    integer :: i, j
    real :: H,H0 !< Volume shape factor [mol]
    real :: F,F0 !< Temperature shape [mol]
    type(shape_diff) :: sdiff, sdiff_org
    real, parameter :: eps = 1.0e-5

    real :: nn(nce)
    real :: Mnn_temp
    real :: V, Vpert
    real :: Tpert
    real :: T0, T01, v0, v01
    real :: P0, P01
    real :: zfac_org, zfac
    real :: temp
    class(base_eos_param), pointer :: act_eos_ptr
    act_eos_ptr => get_active_eos()

    select type (p_eos => act_eos_ptr)
    class is(extcsp_eos)
      ! allocate sdiff structs
      call shape_diff_alloc(sdiff,nce)
      call shape_diff_alloc(sdiff_org,nce)

      ! fill sdiff_org with derivatives of H, F, t0, v0, D, B
      call shape_factors(p_eos,H0,F0,T,n,sdiff_org)

      ! calculate zfac_org and volume
      call csp_Zfac(p_eos,T,P,n,phase,zFac_org)
      V = zFac_org*sum(n)*Rgas*T/P ! [m^3]
      ! calculate T0, v0, P0
      T0 = T*sum(n)/F0         ! [K]
      v0 = V/H0*1000           ! [L/mol]
      P0 = zFac_org*kRgas*T0/v0     ! [Pa]
      print *, "p0 from zfac = ",p0
      call csp_refPressure(p_eos,T0,v0,n,p0)
      print *, "p0 from csp_refPressure = ", p0

      ! fill sdiff_org up with derivatives of M wrt t0 and v0
      call calcRefEqDiff(p_eos,T0,v0,sdiff_org)

      ! calculate the org Helmholtz energy derivatives (and org Mt)
      call calcCombinedDiff(T0,v0,n,sdiff_org)

      ! prepare for volume differentials
      v01 = v0*(1+eps)
      call csp_refPressure(p_eos,T0,v01,n,p01)
      Zfac = p01*v01/(kRgas*T0)
      call shape_factors(p_eos,H,F,T,n,sdiff)
      Vpert = v01*H/1000.0
      call calcRefEqDiff(p_eos,T0,v01,sdiff)
      call calcCombinedDiff(T0,v01,n,sdiff)
      print *,'VOLUME DIFFERENTIALS'
      print *,'v0V   ',(v01-v0)/(Vpert-V),sdiff_org%v0V
      print *,'Mv0:  ',(sdiff%M-sdiff_org%M)/(v0*eps),sdiff_org%Mv0, (sdiff%M-sdiff_org%M)/(v0*eps)/sdiff_org%Mv0
      print *,'Mv0v0:',(sdiff%Mv0-sdiff_org%Mv0)/(v0*eps),sdiff_org%Mv0v0, (sdiff%Mv0-sdiff_org%Mv0)/(v0*eps)/sdiff_org%Mv0v0
      print *,'MT0v0:',(sdiff%Mt0-sdiff_org%Mt0)/(v0*eps),sdiff_org%MT0v0, (sdiff%Mt0-sdiff_org%Mt0)/(v0*eps)/sdiff_org%MT0v0
      !    print *,'*M_v:',(sdiff%M-sdiff_org%M)/(Vpert-V),sdiff_org%Mv
      print *,'*FF_V:',(sdiff%FF-sdiff_org%FF)/(Vpert-V),sdiff_org%FF_V, (sdiff%FF-sdiff_org%FF)/(Vpert-V)/sdiff_org%FF_V
      print *,'*FF_TV:',(sdiff%FF_T-sdiff_org%FF_T)/(Vpert-V),sdiff_org%FF_TV, (sdiff%FF_T-sdiff_org%FF_T)/(Vpert-V)/sdiff_org%FF_TV
      print *,'*FF_VV:',(sdiff%FF_V-sdiff_org%FF_V)/(Vpert-V),sdiff_org%FF_VV, (sdiff%FF_V-sdiff_org%FF_V)/(Vpert-V)/sdiff_org%FF_VV

      ! prepare for temperature differentials
      Tpert = T*(1+eps)                                         ! perturbation of T
      call shape_factors(p_eos,H,F,Tpert,n,sdiff)            ! this also calls the mixture routine, which stores derivatives of D and B
      T01 = Tpert*sum(n)/F                                      ! corresponding perturbation of T0
      call csp_refPressure(p_eos,T01,v0,n,p01)                  ! csp_refPressure assumes the specific volume has units L/mol
      zFac = P01*v0/(kRgas*T01)
      v01 = zFac*kRgas*T01/P01
      call calcRefEqDiff(p_eos,T01,v01,sdiff)           ! fill up sdiff with derivatives of M wrt t0 and v0
      call calcCombinedDiff(T01,v01,n,sdiff)               ! calculate the org Helmholtz energy derivatives (and Mt)

      print *,'TEMPERATURE DIFFERENTIALS'
      print *,'dDdT  ',(sdiff%D-sdiff_org%D)/(T*eps),sdiff%DT, (sdiff%D-sdiff_org%D)/(T*eps)/sdiff%DT
      !print *,'*M_T',(sdiff%M-sdiff_org%M)/(T*eps),sdiff_org%Mt ! for debugging purposes
      print *,'t0T   ',(T01-T0)/(T*eps),sdiff_org%t0T,(T01-T0)/(T*eps)/sdiff_org%t0T
      print *,'FT    ',(F-F0)/(T*eps),sdiff_org%FT,(F-F0)/(T*eps)/sdiff_org%FT
      print *,'FiT:  ',(sdiff%Fi(1)-sdiff_org%Fi(1))/(T*eps),sdiff%FiT(1), (sdiff%Fi(1)-sdiff_org%Fi(1))/(T*eps)/sdiff%FiT(1)
      print *,'FTT:  ',(sdiff%FT-sdiff_org%FT)/(T*eps),sdiff%FTT, (sdiff%FT-sdiff_org%FT)/(T*eps)/sdiff%FTT
      print *,'t0iT: ',(sdiff%t0i(1)-sdiff_org%t0i(1))/(T*eps), sdiff_org%t0Ti(1),&
           ((sdiff%t0i(1)-sdiff_org%t0i(1))/(T*eps))/(sdiff_org%t0Ti(1))
      print *,'*FF_T ',(sdiff%FF-sdiff_org%FF)/(T*eps),sdiff_org%FF_T, (sdiff%FF-sdiff_org%FF)/(T*eps)/sdiff_org%FF_T
      print *,'*FF_TT',(sdiff%FF_T-sdiff_org%FF_T)/(T*eps),sdiff_org%FF_TT, (sdiff%FF_T-sdiff_org%FF_T)/(T*eps)/sdiff_org%FF_TT
      print *,'*FF_VT',(sdiff%FF_V-sdiff_org%FF_V)/(T*eps),sdiff_org%FF_TV, (sdiff%FF_V-sdiff_org%FF_V)/(T*eps)/sdiff_org%FF_TV

      print *,'MT0:  ',(sdiff%M-sdiff_org%M)/(T01-T0),sdiff_org%MT0, (sdiff%M-sdiff_org%M)/(T01-T0)/sdiff_org%MT0
      print *,'MT0T0:',(sdiff%MT0-sdiff_org%MT0)/(T01-T0),sdiff_org%MT0T0, (sdiff%MT0-sdiff_org%MT0)/(T01-T0)/sdiff_org%MT0T0
      print *,'Mv0T0:',(sdiff%Mv0-sdiff_org%Mv0)/(T01-T0),sdiff_org%MT0v0, (sdiff%Mv0-sdiff_org%Mv0)/(T01-T0)/sdiff_org%MT0v0

      print *,'MOLE NUMBER DIFFERENTIALS'
      do i=1,nce
        print *, "COMP NUMBER", i
        ! prepare for composition differentials
        nn = n
        nn(i) = nn(i) + eps

        ! calculations when v0 = v0(nn,V), T0 = T0(nn,T)
        call shape_factors(p_eos,H,F,T,nn,sdiff)
        T01 = T*sum(nn)/F         ! [K]
        v01 = V/H*1000.0         ! [L/mol]
        !call csp_refPressure(T01,v01,nn,p01)

        !zFac = p01*v01/(kRgas*T01)

        call calcRefEqDiff(p_eos,T01,v01,sdiff)

        call calcCombinedDiff(T01,v01,nn,sdiff)
        Mnn_temp = sdiff%M

        !print *,'*Mi:',(sdiff%M-sdiff_org%M)/(eps),sdiff_org%Mi(i)
        temp = (sdiff%FF_T-sdiff_org%FF_T)/(eps)
        print *,'*FF_Ti:',(sdiff%FF_T-sdiff_org%FF_T)/(eps*sdiff_org%FF_Ti(i))
        print *,'*FF_i:',(sdiff%FF-sdiff_org%FF)/(eps*sdiff_org%FF_i(i))

        print *,'*FF_Vi:',(sdiff%FF_V-sdiff_org%FF_V)/(eps*sdiff_org%FF_Vi(i))
        do j=1,nce
          print *,'*FF_ij:',(sdiff%FF_i(j)-sdiff_org%FF_i(j))/(eps*sdiff_org%FF_ij(j,i))
        enddo

        print *,'FiT:',(sdiff%FT-sdiff_org%FT)/(eps*sdiff%FiT(i))
        print *,'Fi:',(F-F0)/(eps*sdiff_org%Fi(i))
        do j=1,nce
          print *,'Fij:',(sdiff%Fi(j)-sdiff_org%Fi(j))/(eps*sdiff_org%Fij(j,i))
        enddo

        print *,'Hi:',(H-H0)/(eps*sdiff_org%Hi(i))
        print *,'Di:',(sdiff%D-sdiff_org%D)/(eps*sdiff_org%Di(i))
        print *,'Bi:',(sdiff%B-sdiff_org%B)/(eps*sdiff_org%Bi(i))

        print *,'t0Ti:',(sdiff%t0T-sdiff_org%t0T)/(eps*sdiff_org%t0Ti(i))
        print *,'t0i:',(T01-T0)/(eps*sdiff_org%t0i(i))

        print *,'v0Vi:',(sdiff%v0V-sdiff_org%v0V)/(eps*sdiff_org%v0Vi(i))
        print *,'v0i:',(v01-v0)/(eps*sdiff_org%v0i(i))
        do j=1,nce
          print *,'v0ij:',(sdiff%v0i(j)-sdiff_org%v0i(j))/(eps*sdiff_org%v0ij(j,i))

          print *,'t0ij:',(sdiff%t0i(j)-sdiff_org%t0i(j))/(eps*sdiff_org%t0ij(j,i))
        enddo
      enddo
    end select
  end subroutine test_shape_factors

  subroutine csp_testPressure(T,v,n)
    use thermopack_var, only: nce, get_active_eos, base_eos_param
    implicit none
    ! input
    real, intent(in) :: T                        !< Mixture temperature [K]
    real, intent(in) :: v                        !< Mixture molar volume [L/mol]
    real, dimension(nce), intent(in) :: n         !< Mole composition of mixture [mol]
    ! locals
    real  :: P                       !< Pressure [Pa]
    real  :: dPdv, dPdT    !< Pressure differentials [Pa*mol/m^3], [Pa/K]
    real  :: sumn
    real :: dv, dt, dpdv_num, dpdt_num, p_pert, eps_v, eps_t
    real :: dPdn(nce), dn, dPdn_num(nce), eps_n(nce), nn(nce)
    integer :: i
    class(base_eos_param), pointer :: act_eos_ptr
    sumn = sum(n)

    dv = 1.0e-6*v
    dt = 1.0e-6*T
    dn = 1.0e-6

    act_eos_ptr => get_active_eos()
    select type (p_eos => act_eos_ptr)
    class is(extcsp_eos)
      print *, "TESTING CSP_REFPRESSURE"
      call csp_refPressure(p_eos,T,v,n,P,dPdV,dPdT)

      call csp_refPressure(p_eos,T,v+dv,n,P_pert)
      dpdv_num = (p_pert-p)/dv
      eps_v = abs(dpdv-dpdv_num)

      call csp_refPressure(p_eos,T+dt,v,n,P_pert)
      dpdt_num = (p_pert-p)/dt
      eps_t = abs(dpdt-dpdt_num)

      print *, "eps,", " releps, ", "value, ", "num_value"
      print *, eps_v, eps_v/abs(dpdv), dpdv, dpdv_num, "(dpdv)"
      print *, eps_t, eps_t/abs(dpdt), dpdt, dpdt_num,  "(dpdt)"

      print *, "TESTING CSP_MIXTPRESSURE"
      call csp_mixtPressure(p_eos,T,v,n,P,dPdV,dPdT,dPdn)
      print *,'p',P
      call csp_mixtPressure(p_eos,T,v+dv,n,P_pert)
      dpdv_num = (p_pert-p)/dv
      eps_v = abs(dpdv-dpdv_num)

      call csp_mixtPressure(p_eos,T+dt,v,n,P_pert)
      dpdt_num = (p_pert-p)/dt
      eps_t = abs(dpdt-dpdt_num)

      do i=1,nce
        nn = n
        nn(i) = nn(i) + dn
        call csp_mixtPressure(p_eos,T,v,nn,P_pert)
        dpdn_num(i) = (p_pert-p)/dn
        eps_n(i) = abs(dpdn(i)-dpdn_num(i))
      enddo

      print *, "eps,", " releps, ", "value, ", "num_value"
      print *, eps_v, eps_v/abs(dpdv), dpdv, dpdv_num, "(dpdv)"
      print *, eps_t, eps_t/abs(dpdt), dpdt, dpdt_num,  "(dpdt)"
      do i=1,nce
        print *, eps_n(i), eps_n(i)/abs(dpdn(i)), dpdn(i), dpdn_num(i),  "(dpdn)", i
      enddo
    end select
  end subroutine csp_testPressure

  !> Calculates the reduced residual Helmholtz energy F,
  !> along with its derivatives.
  subroutine csp_calcFres(nce,eos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn)
    ! Input.
    integer, intent(in) :: nce
    class(extcsp_eos), intent(inout) :: eos
    real, intent(in) :: T,V,n(nce)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nce)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nce),F_VV,F_Vn(nce),F_nn(nce,nce)
    ! Locals
    real :: Hs, Fs, T0, v0, sumn
    sumn = sum(n)

    ! compute shape factors and their differentials
    call shape_factors(eos,Hs,Fs,T,n,eos%sd)
    ! compute v0 and T0
    v0 = 1.0e3*V/Hs  ! must multiply by 1e3 to get liters/mol
    T0 = sumn*T/Fs
    call calcRefEqDiff(eos,T0,v0,eos%sd)
    call calcCombinedDiff(T0,v0,n,eos%sd)

    if (present(F)) F = eos%sd%FF
    if (present(F_T)) F_T = eos%sd%FF_T
    if (present(F_V)) F_V = eos%sd%FF_V
    if (present(F_n)) F_n = eos%sd%FF_i
    if (present(F_TT)) F_TT = eos%sd%FF_TT
    if (present(F_TV)) F_TV = eos%sd%FF_TV
    if (present(F_Tn)) F_Tn = eos%sd%FF_Ti
    if (present(F_VV)) F_VV = eos%sd%FF_VV
    if (present(F_Vn)) F_Vn = eos%sd%FF_Vi
    if (present(F_nn)) F_nn = eos%sd%FF_ij

  end subroutine csp_calcFres

  subroutine assign_extcsp_eos_set(This, other)
    use compdata, only: copy_comp
    class(extcsp_eos), intent(out) :: this
    class(*), intent(in)           :: other
    ! Locals
    integer :: istat
    select type (p_o => other)
    class is (extcsp_eos)
      !call assign_cubic_eos(this, p_o)
      this%shapeEos = p_o%shapeEos
      if (associated(p_o%mbwrRefEos)) then
        if (.not. associated(this%mbwrRefEos)) then
          istat = 0
          allocate(this%mbwrRefEos, stat=istat)
          if (istat /= 0) call stoperror('Error allocating mbwrRefEos')
        endif
        this%mbwrRefEos = p_o%mbwrRefEos
      endif
      if (associated(p_o%nistRefEos)) then
        if (.not. associated(this%nistRefEos)) then
          istat = 0
          allocate(meos_c3 :: this%nistRefEos, stat=istat)
          if (istat /= 0) call stoperror('Error allocating nistRefEos')
        endif
        !this%nistRefEos = p_o%nistRefEos
      endif
      this%sd = p_o%sd
      if (allocated(p_o%refComp)) then
        call copy_comp(this%refComp, p_o%refComp)
      endif
      this%refNc = p_o%refNc
      this%refEosType = p_o%refEosType
      this%Tc0 = p_o%Tc0
      this%Pc0 = p_o%Pc0
      this%m0 = p_o%m0
      this%bc0 = p_o%bc0
      this%ac0 = p_o%ac0
    class default
      print *,"assign_extcsp_eos_set: Should not be here"
    end select
  end subroutine assign_extcsp_eos_set

  subroutine extcsp_eos_allocate_and_init(eos,nc,eos_label)
    ! Passed object:
    class(extcsp_eos), intent(inout) :: eos
    ! Input:
    integer, intent(in) :: nc !< Number of components
    character(len=*), intent(in) :: eos_label !< EOS and component label
    ! Locals
    integer :: istat, ipos
    character(len=len_trim(eos_label)) :: eos_l !< EOS label
    character(len=len_trim(eos_label)) :: comp_l !< Comp label
    call eos%dealloc()

    call eos%shapeEos%allocate_and_init(nc,"SRK")
    call eos%shapeEosRef%allocate_and_init(1,"SRK")
    ipos=index(eos_label,":")
    if (ipos > 0) then
      eos_l = eos_label(1:ipos-1)
      comp_l = eos_label(ipos+1:len_trim(eos_label))
    else
      call stoperror("extcsp_eos_allocate_and_init:"//&
           "Wrong format on input string eos_label")
    endif
    istat = 0
    if (eos%refEosType == mbwr) then
      allocate(eos%mbwrRefEos, stat=istat)
      if (istat /= 0) call stoperror('Error allocating mbwrRefEos')
      if (str_eq(eos_label,'MBWR19')) then
        call initializeMBWRmodel(comp_l, eos%mbwrRefEos, 19)
      else
        call initializeMBWRmodel(comp_l, eos%mbwrRefEos, 32)
      endif
      eos%Tc0 = eos%mbwrRefEos%tc
      eos%Pc0 = eos%mbwrRefEos%pc
    else if (eos%refEosType == nist) then
      if (str_eq(comp_l, "C3")) then
        allocate(meos_c3 :: eos%nistRefEos, stat=istat)
      else
        call stoperror("Only possible to use NIST MEOS with components: C3")
      end if
      if (istat /= 0) call stoperror("Not able to allocate eos%nistRefEos")
      call eos%nistRefEos%init()
      eos%Tc0 = eos%nistRefEos%tc
      eos%Pc0 = eos%nistRefEos%pc
    else if (eos%refEosType == cubic) then
      call eos%cbrefEos%allocate_and_init(nc,eos_label)
      eos%Tc0 = 0 ! Set elswhere
      eos%Pc0 = 0
    else
      call stoperror("Wrong input to extcsp_eos_allocate_and_init")
    endif
    call eos%sd%shape_diff_alloc(nc)

    eos%refNc = 0
    !eos%refComp
    eos%m0 = 0
    eos%bc0 = 0
    eos%ac0 = 0
  end subroutine extcsp_eos_allocate_and_init

  !! \author Morten H
  subroutine extcsp_eos_dealloc(eos)
    use utilities, only: deallocate_real
    use compdata, only: deallocate_comp
    ! Passed object:
    class(extcsp_eos), intent(inout) :: eos
    ! Locals
    integer :: istat
    !call cubic_eos_dealloc(eos)
    call eos%shapeEos%dealloc()
    if (associated(eos%mbwrRefEos)) then
      call eos%mbwrRefEos%dealloc()
      istat = 0
      deallocate(eos%mbwrRefEos, stat=istat)
      if (istat /= 0) print *,"extcsp_eos_dealloc: Not able to deallocate mbwrRefEos"
      eos%mbwrRefEos => NULL()
    endif
    if (associated(eos%nistRefEos)) then
      !call eos%nistRefEos%dealloc()
      istat = 0
      deallocate(eos%nistRefEos, stat=istat)
      if (istat /= 0) print *,"extcsp_eos_dealloc: Not able to deallocate nistRefEos"
      eos%nistRefEos => NULL()
    endif
    call eos%sd%shape_diff_dealloc()
    call deallocate_comp(eos%refComp)
  end subroutine extcsp_eos_dealloc

end module csp
