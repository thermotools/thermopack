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
  use parameters
  use compdata, only: gendata
  use eosdata, only: eoscubic
  use tpmbwr, only: eosmbwr
  use multiparameter_c3, only: meos_C3
  use stringmod, only: str_eq
  implicit none
  private
  save

  integer :: refNc = 0                                 !< This is set to 1 in subroutine selectComp.
  real, parameter, dimension(1) :: zRef = 1.0          !< This is the vector of mole numbers if we're using a cubic reference equation
  type(gendata), allocatable, dimension(:) :: refComp  !< Will be made to have length refNc=1 in subroutine selectComp.
  integer, parameter :: cubic = 1, mbwr = 2, nist = 3
  integer :: refEosType                                !< Is set to either cubic or mbwr.

  real :: Tc0, Pc0                                     !< Critical constants for reference component.
  real :: m0, bc0, ac0                                 !< Parameters for the cubic shape eos.

  !> Shape and rederence Eos data struct
  type(eoscubic) :: shapeEos
  type(eoscubic), allocatable, dimension(:) :: cbrefEos
  !> Multiparameter data struct
  type(eosmbwr), allocatable, dimension(:) :: mbwrRefEos
  class(meos_c3), allocatable, dimension(:) :: nistRefEos !< Should ideally be declared as an array of base class "meos", but pgf90 does not like this.

  type shape_diff                                      !< Derivatives. Uses the notation on pages 115-120 in Michelsen & Mollerup.
    sequence
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
  end type shape_diff

  type(shape_diff), allocatable, dimension(:) :: sd ! Store here for now

  public :: csp_init
  public :: shape_diff
  public :: csp_Zfac, csp_refPressure
  public :: cleanup_csp, csp_calcFres
  public :: mbwrRefEos
  public :: csp_testpressure, csp_mainTestRoutine

contains

  !-----------------------------------------------------------------------------
  !> Init calculation for shape factors, by setting index of reference component
  !>
  !> \author MHA, 2013-11-27
  !> \author Ailo 
  !-----------------------------------------------------------------------------
  subroutine csp_init(refcomp_str,shEos,shMixRule,shAlpha,refEos,refAlpha) ! The input consists entirely of strings.
    use tpselect, only: SelectComp, SelectEOS
    use tpconst, only: kRgas
    use tpmbwr, only: initializeMBWRmodel
    use eosdata
    use tpvar, only: nce
    !$ use omp_lib, only: omp_get_max_threads
    implicit none
    character(len=*), intent(in) :: refcomp_str         !< Reference component
    character(len=*), intent(in) :: shEos,refEos        !< shEos is any two-parameter (a,b) cubic equation of state
    character(len=*), intent(in) :: shMixRule
    character(len=*), intent(in) :: shAlpha
    character(len=*), intent(in), optional :: refAlpha  !< Needed if refEos is a cubic eos. Should not be present if one want to use an mbwr reference eos.
    ! Locals
    integer :: nthreads, i, nMBWR, err
    !
    nthreads = 1
    ! Create one instance of sd, cbrefEos and multiparameter EoS per thread
    !$ nthreads = omp_get_max_threads()

    ! Delete previously allocated memory
    call cleanup_csp()
    allocate(sd(nthreads),STAT=err);
    if (err /= 0) call stoperror('Not able to allocate shape_diff struct for csp model')
    !
    call SelectComp(trim(refcomp_str),refNc,refComp)
    do i=1,nthreads
      call shape_diff_alloc(sd(i),nce)
    enddo
    ! Set reference component data
    shapeEos%eosid = trim(shEos) ! Has to be set here if getAlphaTWUparams is to find the parameters
    call SelectEOS(refNc, refComp, shapeEos, trim(shEos), &
         trim(shMixRule), trim(shAlpha)) ! Is only initialized to obtain m0, bc0 and ac0.

    if (.not. (shapeEos%eosidx == cbSRK &
         .or. shapeEos%eosidx == cbSRKGB &
         .or. shapeEos%eosidx == cbPR)) then
      print *,'Incorrect EOS, ', trim(shEos), &
           ', defined for shape factor calculation'
      call stoperror('')
    endif

    if (refEos(1:2) .eq. "SR" .or. refEos(1:2) .eq. "PR") then ! Could in principle be any cubic eos...
      !Cubic reference equation
      if (present(refAlpha)) then
        refEosType = cubic
        allocate(cbrefEos(nthreads),STAT=err);
        if (err /= 0) call stoperror('Not able to allocate cbrefEos for csp model')
        do i=1,nthreads
          call SelectEOS(refNc, refComp, cbrefEos(i), trim(refEos), &
               trim(shMixRule), trim(refAlpha))
        enddo
      else
        call stoperror('csp_init: need to input refAlpha')
      end if
      ! Reference component data from database
      Tc0 = refComp(1)%Tc
      Pc0 = refComp(1)%Pc

    else if (str_eq(refEos(1:4),'mbwr')) then
      !MBWR reference equation
      refEosType = mbwr
      select case(trim(refEos))
      case ("mbwr19","MBWR19","mbwr20","MBWR20","bender","Bender","BENDER")
        nMBWR = 19
      case("mbwr32","MBWR32")
        nMBWR = 32
      case default
        print *,'Incorrect EOS, ', trim(refEos), &
             ', defined for reference equation.'
        call stoperror('')
      end select

      allocate(mbwrRefEos(nthreads),STAT=err);
      if (err /= 0) call stoperror('Not able to allocate mbwrRefEos for csp model')

      do i=1,nthreads
        call initializeMBWRmodel(trim(refcomp_str),mbwrRefEos(i),nMBWR)
      enddo
      ! Reference component data using critical parameters computed directly from the MBWR equation.
      Tc0 = mbwrRefEos(1)%tc
      Pc0 = mbwrRefEos(1)%pc

    else if ( str_eq(refEos,"C3_NIST") .or. str_eq(refEos,"NIST_MEOS") ) then ! Must allow same name as for single component eos
       if (.not. str_eq(refcomp_str,"C3")) then
          call stoperror('Only C3 can be used as reference component for NIST_MEOS')
       endif
       refEosType = nist

       allocate(nistRefEos(nthreads),STAT=err);
       if (err /= 0) call stoperror('Not able to allocate C3 nistRefEos for csp model')

       do i=1,nthreads
          call nistRefEos(i)%init()
       enddo
       
       Tc0 = nistRefEos(1)%tc
       Pc0 = nistRefEos(1)%pc
    
    else
       print *, 'Selected reference equation: ',  refEos, ' is invalid'
       print *, 'MBWR32, MBWR19, NIST_MEOS and SRK, SRKGB or PR are valid'
       call stoperror('Unknown reference equation')
    end if

    m0 = shapeEos%alfa + shapeEos%beta*refComp(1)%acf&
         - shapeEos%gamma*refComp(1)%acf**2
    bc0 = shapeEos%single(1)%omegaB*kRgas*Tc0/Pc0 !< constant bi in cubic eos
    ac0 = shapeEos%single(1)%omegaA*(kRgas*Tc0)**2/Pc0 !< constant ai in cubic eos


  end subroutine csp_init


  !-----------------------------------------------------------------------------
  !> Calculate shape factors from shape factor eos.
  !>
  !> Differentials are optional, but if present the derivatives of H and F and
  !> derivatives of v0 and t0 (wrt T,V,n_i) are computed to the second order.
  !>
  !> \author MHA, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine shape_factors(cbeos,H,F,T,n,sdiff)
    use eosdata, only: eoscubic
    use tpvar, only: nce
    implicit none
    type(eoscubic), intent(inout) :: cbeos !< Cubic equation for the shape factors
    real, intent(out) :: H !< Volume shape factor [mol]
    real, intent(out) :: F !< Temperature shape [mol]
    real, intent(in) :: T !< Temperature [K]
    real, dimension(nce), intent(in) :: n !< Molar values [mol]
    type(shape_diff), optional, intent(inout) :: sdiff

    call cubic_shape_factors(cbeos,H,F,T,n,sdiff)         ! This may seem weird, but maybe one later wants to add functionality for using non-cubic shape eos's.

  end subroutine shape_factors

  function temperatureShapeFactorF(H,D,T,sumn) result(F)
    use eosdata, only: cbAlphaTwuIdx, cbAlphaClassicIdx
    use nonlinear_solvers, only: newton_1d
    implicit none
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
    if (shapeEos%single(1)%alphamethod .eq. cbAlphaClassicIdx) then
      ! compute the temperature shape factor using the classical alpha formulation
      F = ((m0*sqrt(T*sumn/Tc0) + sqrt(D/(H*ac0)))/(1.0 + m0))**2
      !    print *,"F in temperatureShapeFactorF equals ", F
    else if (shapeEos%single(1)%alphamethod .eq. cbAlphaTwuIdx) then
      ! if not, estimate t0 using F
      t0 = sumn*T/F
      t0r = t0/tc0

      ! is the (approximate) reference temperature subcritical?
      if (t0r .gt. 1.0) print *, "TWU used for supercritical reference temperature..."
      ! is Twu correlation in database?
      if (shapeEos%single(1)%alphaParams(1) .ne. 0.0 .and. &
          shapeEos%single(1)%alphaParams(2) .ne. 0.0 .and. &
          shapeEos%single(1)%alphaParams(3) .ne. 0.0) then
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

    H = param(1)
    D = param(2)
    T = param(3)
    sumn = param(4)

    L = shapeEos%single(1)%alphaParams(1)
    M = shapeEos%single(1)%alphaParams(2)
    N = shapeEos%single(1)%alphaParams(3)

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
  subroutine cubic_shape_factors(cbeos,H,F,T,n,sdiff)
    use eosdata, only: eoscubic
    use tpvar, only: nce
    implicit none
    type(eoscubic), intent(inout) :: cbeos !< Cubic equation for shape factors.
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
    call mixture(cbeos,T,n,D,B,sdiff)
    H = B/bc0                                 ! does not depend on alpha formulation
    F = temperatureShapeFactorF(H,D,T,sumn)   ! depends on alpha formulation

    ! H differentials
    if (present(sdiff)) then
      sdiff%H = H
      sdiff%F = F
      sdiff%HT = sdiff%BT/bc0
      sdiff%Hi = sdiff%Bi/bc0
      sdiff%HiT = sdiff%BiT/bc0
      sdiff%Hij = sdiff%Bij/bc0
      sdiff%HTT = sdiff%BTT/bc0

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
      call a0_diff(T0,a0,a0T0,a0T0T0)
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
    use tpvar, only: comp, nce
    use tpcbmix, only: cbCalcMixtureParams
    use eosdata, only: eoscubic
    implicit none
    type(eoscubic), intent(inout) :: cbeos !< The cubic equation for the shape factors
    real, intent(out) :: D !< Volume shape factor [mol]
    real, intent(out) :: B !< Temperature shape factor [mol]
    real, intent(in) ::  T !< Temperature [K]
    real, dimension(nce), intent(in) :: n !< Molar values [mol]
    type(shape_diff), optional, intent(inout) :: sdiff
    call cbCalcMixtureParams(nce,comp,cbeos,t,n)
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
  subroutine a0_diff(T0,a0,a0T0,a0T0T0)
    !    use tpcbmix, only: cbCalcAlphaTerm
    implicit none
    real, intent(out) :: a0,a0T0,a0T0T0 !< Differentials of a0
    real, intent(in) :: T0 !< Temperature [K]
    ! Locals
    real :: sqrtT0Tc0, var, dvar, d2var
    sqrtT0Tc0 = sqrt(T0/Tc0)
    var = 1.0 + m0 - m0*sqrtT0Tc0
    dvar = - 0.5*m0*sqrtT0Tc0/T0
    d2var = 0.25*m0*sqrtT0Tc0/(T0*T0)
    a0 = ac0*var**2
    a0T0 = 2.0*ac0*var*dvar
    a0T0T0 = 2.0*ac0*(dvar**2 + var*d2var)

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
    type(shape_diff), intent(inout) :: sdiff
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

  end subroutine shape_diff_alloc

  !-----------------------------------------------------------------------------
  !> Deallocate differential struct
  !>
  !> \author MH, 2013-11-27
  !-----------------------------------------------------------------------------
  subroutine shape_diff_dealloc(sdiff)
    implicit none
    type(shape_diff), intent(inout) :: sdiff
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
  subroutine csp_Zfac(cbeos,T,P,n,phase,zFac,dZdt,dZdp,dZdz)
    use tpvar, only: nce
    use tpconst, only: Rgas, kRgas
    use eosdata, only: eoscubic
    !$ use omp_lib, only: omp_get_thread_num
    implicit none
    type(eoscubic), intent(inout) :: cbeos !< Cubic eos for shape factor calculation.
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
    integer :: i_thread
    i_thread = 1
    !$ i_thread = 1 + omp_get_thread_num()
    if (present(dZdt) .or. present(dZdp) .or. present(dZdz)) then
      call shape_factors(cbeos,H,F,T,n,sd(i_thread))
    else
      call shape_factors(cbeos,H,F,T,n)
    endif
    sumn = sum(n)
    P0 = H*P/F
    T0 = T*sumn/F

    zFac = solveRefEqZfac(T0,P0,phase,i_thread)    ! Z-factor for the reference equation. Also equals the Z-factor for the real mixture.
    V = sumn*zFac*Rgas*T/P                         ! The volume for the reference fluid [m^3].
    if (present(dZdt) .or. present(dZdp) .or. present(dZdz)) then
      v0 = zFac*kRgas*T0/P0
      call calcRefEqDiff(T0,v0,sd(i_thread),i_thread)
      call calcCombinedDiff(T0,v0,n,sd(i_thread))
      dPdV = -Rgas*T*(sd(i_thread)%FF_VV + sumn/V**2)
      if (present(dZdt)) then
        dPdT = P/T - Rgas*T*sd(i_thread)%FF_TV
        dvdT = -dPdT/dPdv
        dZdT = -zFac*(1.0/T - dvdT/V)
      endif
      if (present(dZdp)) then
        dZdP = zFac*(1.0/P + 1.0/(dPdv*V))
      endif
      if (present(dZdz)) then
        dPdn = Rgas*T*(-sd(i_thread)%FF_Vi + 1/V)
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
  subroutine calcRefEqDiff(T0,v0,sdiff,i_thread)
    use tpcubic, only: cbCalcDerivatives_svol
    use tpconst, only: Rgas
    use tpmbwr_additional, only: alphar_derivatives
    use cbHelm
    implicit none
    real, intent(in) :: T0 !< Reference fluid temperature [K]
    real, intent(in) :: v0 !< Reference fluid pressure [l/mol]
    type(shape_diff), intent(inout) :: sdiff
    integer, intent(in) :: i_thread
    !locals
    real :: rho
    real, dimension(0:2) :: alphaDerivatives
    real :: alpha, DalphaDrho, D2alphaDrho2
    real :: DalphaDT, D2alphaDrhoDT
    real :: D2alphaDT2
    real :: alpr,alpr_T,alpr_v,alpr_TT,alpr_Tv,alpr_vv

    if (refEosType .eq. cubic) then
      call cbCalcDerivatives_svol(refNc,cbrefEos(i_thread),T0,v0)
      sdiff%M = cbF(cbrefEos(i_thread))*(Rgas*T0)
      sdiff%Mt0 = Rgas*(cbF(cbrefEos(i_thread)) + T0*cbFT(cbrefEos(i_thread)))
      sdiff%Mt0t0 = Rgas*(2*cbFT(cbrefEos(i_thread)) + T0*cbFTT(cbrefEos(i_thread)))
      sdiff%Mt0v0 = Rgas*(cbFV(cbrefEos(i_thread)) + T0*cbFVT(cbrefEos(i_thread)))
      sdiff%Mv0 = Rgas*T0*cbFV(cbrefEos(i_thread))
      sdiff%Mv0v0 = Rgas*T0*cbFVV(cbrefEos(i_thread))
    else if (refEosType .eq. mbwr) then ! alpha is the reduced residual molar Helmholtz energy.

      rho = 1.0/v0
      call alphar_derivatives(deriv=alphaDerivatives,t=T0,rho=rho,&
           nTderivs=0,nRhoDerivs=2,model=mbwrRefEos(i_thread)) ! compute alpha, alpha_rho0, alpha_rho0rho0
      alpha = alphaDerivatives(0)
      DalphaDrho = alphaDerivatives(1)
      D2alphaDrho2 = alphaDerivatives(2)

      sdiff%M = Rgas*T0*alpha
      sdiff%Mv0 = -Rgas*T0*DalphaDrho*rho**2
      sdiff%Mv0v0 = Rgas*T0*(D2alphaDrho2*rho**4 + 2*DalphaDrho*rho**3)
      call alphar_derivatives(deriv=alphaDerivatives,t=T0,rho=rho,nTderivs=1,nRhoDerivs=1,model=mbwrRefEos(i_thread)) ! compute alpha_T0, alpha_T0rho0
      DalphaDT = alphaDerivatives(0)
      D2alphaDrhoDT = alphaDerivatives(1)

      sdiff%Mt0 = Rgas*(alpha + T0*DalphaDT)
      sdiff%Mt0v0 = -Rgas*(DalphaDrho + T0*D2alphaDrhoDT)*rho**2
      call alphar_derivatives(deriv=alphaDerivatives,t=T0,rho=rho,nTderivs=2,nRhoDerivs=0,model=mbwrRefEos(i_thread)) ! compute alpha_T0T0
      D2alphaDT2 = alphaDerivatives(0)

      sdiff%Mt0t0 = Rgas*(2*DalphaDT + T0*D2alphaDT2)
    else if (refEosType .eq. nist) then ! this multiparameter EoS operates in SI units
       call nistRefEos(i_thread)%alphaResDerivs_Tv(T0,v0*1e-3,alpr,alpr_T,alpr_v,alpr_TT,alpr_Tv,alpr_vv)
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
  function solveRefEqZfac(T0,P0,phase,i_thread) result(zFac)
    use tpcubic
    use tpmbwr_additional, only: mbwr_volume
    use tpconst, only: Rgas, kRgas
    implicit none
    !input
    real, intent(in) :: P0 !< Pressure of the reference fluid [Pa]
    real, intent(in) :: T0 !< Temperature of the reference fluid [K]
    integer, intent(in) :: phase
    integer, intent(in) :: i_thread
    !output
    real :: zFac !<  [-]
    !locals
    real :: v0 ! specific volume [L/mol]
    real :: rho0 ! specific density (mol/m^3)
    if (refEosType .eq. cubic) then
      call cbCalcZfac(refNc,refComp,cbrefEos(i_thread),T0,P0,zRef,phase,zFac,1)
    else if (refEosType .eq. mbwr) then
      v0 = mbwr_volume(T0,P0,nMoles=1.0,phase=phase,model=mbwrRefEos(i_thread))
      zFac = P0*v0/(kRgas*T0) ! Need to use kRgas here since V is calculated in litres.
    else if (refEosType .eq. nist) then
      call nistRefEos(i_thread)%densitySolver(T0,P0,phase,rho0)
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
  subroutine csp_refPressure(T0,v0,n,P0,dp0dv0,dp0dt0)
    use tpcubic
    use tpvar, only: nce
    use eosdata, only: eoscubic
    use tpmbwr, only: makeParam, MBWR_pressure
    !$ use omp_lib, only: omp_get_thread_num
    implicit none
    ! input
    real, intent(in) :: T0 !< Reference fluid temperature [K]
    real, intent(in) :: v0 !< Reference fluid molar volume [L/mol]
    real, dimension(nce), intent(in) :: n !< Mole composition of mixture [mol]
    ! ouput
    real, intent(out) :: P0 !< Pressure [Pa]
    real, optional, intent(out) :: dp0dv0, dp0dt0 !< Pressure differentials [Pa*mol/L], [Pa/K]
    ! Locals
    real, allocatable, dimension(:) :: mbwrParameters
    integer :: i_thread
    real :: rho0, dp0drho0
    i_thread = 1
    !$ i_thread = 1 + omp_get_thread_num()

    ! Evaluate reference equation
    if (refEosType .eq. cubic) then
       ! Only calculate what is strictly necessary. (Why is n input here?)
       call cbCalcPressure(refNc,refComp,cbrefEos(i_thread),T0,v0,n,P0,dp0dv0,dp0dt0)
    else if (refEosType .eq. mbwr) then
       allocate(mbwrParameters(1+mbwrRefEos(1)%bplen+mbwrRefEos(1)%belen))
       ! Only calculate what is strictly necessary.
       if (.not. (present(dp0dv0) .or. present(dp0dt0))) then
          call makeParam(parameters=mbwrParameters,t=T0,model=mbwrRefEos(i_thread),nTderivatives=0)
          call MBWR_pressure(rho=1/v0, param=mbwrParameters, p=P0)
       else
          if (present(dp0dv0)) then
             call makeParam(parameters=mbwrParameters,t=T0,model=mbwrRefEos(i_thread),nTderivatives=0)
             call MBWR_pressure(rho=1/v0,param=mbwrParameters,p=P0,dpdrho=dp0dv0)
             dp0dv0 = -dp0dv0/v0**2
          end if
          if (present(dp0dt0)) then
             call makeParam(parameters=mbwrParameters,t=T0,model=mbwrRefEos(i_thread),nTderivatives=1)
             call MBWR_pressure(rho=1/v0, param=mbwrParameters, P=dp0dt0)
          end if
       end if
       deallocate(mbwrParameters)
    else if ( refEosType .eq. nist ) then
       rho0 = 1e3/v0
       call nistRefEos(i_thread)%mp_pressure(rho=rho0,t=T0,p=P0,p_rho=dp0drho0,p_T=dp0dt0)
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
  subroutine csp_mixtPressure(cbeos,T,v,n,P,dPdV,dPdT,dpdz)
    use tpvar, only: nce
    use tpconst, only: Rgas
    !$ use omp_lib, only: omp_get_thread_num
    implicit none
    ! in/out
    type(eoscubic), intent(inout) :: cbeos
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
    integer :: i_thread
    real :: V_m3
    i_thread = 1
    !$ i_thread = 1 + omp_get_thread_num()
    sumn = sum(n)

    V_m3 = v!*sumn ! volume [m3]
    ! compute shape factors and their differentials
    call shape_factors(cbeos,H,F,T,n,sd(i_thread))
    ! compute v0 and T0
    v0 = 1e3*V_m3/H  ! must multiply by 1e3 to get liters/mol
    T0 = sumn*T/F
    ! compute P0
    call csp_refPressure(T0,v0,n,P0)
    ! calculate red. res. Helmholtz energy F and its derivatives
    call calcRefEqDiff(T0,v0,sd(i_thread),i_thread)
    call calcCombinedDiff(T0,v0,n,sd(i_thread))
    ! calculate P and its derivatives
    P = Rgas*T*(-sd(i_thread)%FF_V + sumn/V_m3)
    if (present(dPdT)) dPdT = P/T - Rgas*T*sd(i_thread)%FF_TV
    if (present(dPdv)) dPdV = -Rgas*T*(sd(i_thread)%FF_VV + sumn/V_m3**2) ! Pa*mol/m3

    if (present(dPdz)) then
      dPdz = Rgas*T*(-sd(i_thread)%FF_Vi + 1.0/V_m3)
    endif
  end subroutine csp_mixtPressure

  subroutine checkStateFunctionDerivatives(StateFunction,cbeos,T,P,n,phase_in)
    use tpvar, only: nce
    use eosdata, only: eoscubic
    implicit none
    !input
    interface
      subroutine StateFunction(cbeos,T,P,n,phase,G,dGdt,dGdp,dGdn)
        use tpcubic
        use tpvar, only: nce
        use eosdata, only: eoscubic
        use tpconst, only: Rgas
        implicit none
        type(eoscubic), intent(inout) :: cbeos !< Cubic eos used for shape factor calculations.
        real, intent(in) :: P !< Pressure [Pa]
        real, intent(in) :: T !< Temperature [K]
        integer, intent(in) :: phase !< Phase identifier [-]
        real, dimension(nce), intent(in) :: n !< Composition [mol]
        real, intent(out) :: G !< Gibbs free energy [J/mol]
        real, optional, intent(out) :: dGdt, dGdp
        real, dimension(nce), optional, intent(out) :: dGdn
      end subroutine StateFunction
    end interface
    type(eoscubic), intent(inout) :: cbeos
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

    call StateFunction(cbeos,T,P,n,phase_in,b,dbdt,dbdp,dbdn)
    print *, "value of state function:", b

    ! CHECK T-DERIVATIVE
    call StateFunction(cbeos,T+dt,P,n,phase_in,b_mod)
    print *, "numder t = ", (b_mod-b)/dt
    eps_t = abs((b_mod-b)/dt - dbdt )
    if (dbdt .ne. 0.0) releps_t = eps_t/dbdt
    ! CHECK P-DERIVATIVE
    call StateFunction(cbeos,T,P+dp,n,phase_in,b_mod)
    print *, "numder p = ", (b_mod-b)/dp
    eps_p = abs((b_mod-b)/dp - dbdp )
    if (dbdp .ne. 0.0) releps_p = eps_p/dbdp
    ! CHECK n-DERIVATIVE
    do i = 1, nce
      n_temp = n
      n_temp(i) = n_temp(i) + dn
      call StateFunction(cbeos,T,P,n_temp,phase_in,b_mod)
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
    use tpconst, only: Rgas
    use tpvar, only: nce
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
    use tpvar, only: nce, cbeos
    implicit none
    real :: T, P, v, n(2)
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
    call checkStateFunctionDerivatives(csp_zFac,cbeos(1),T,P,n,2)

    stop "TESTING FINISHED"
  end subroutine csp_mainTestRoutine

  !-----------------------------------------------------------------------------
  !> Compare analytical and numerical derivatives in the sdiff struct.
  !>
  !> \author MH, 2013-12-11
  !> \author Ailo A, 2014-12-04
  !-----------------------------------------------------------------------------
  subroutine test_shape_factors(nce,T,P,n,phase)
    use tpconst, only: Rgas, kRgas
    use tpcubic, only: cbCalcPressure
    use tpvar, only: cbeos             ! cbeos is a global array with the shape eos (one for each potential thread)
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
    integer :: i_thread
    i_thread = 1

    ! allocate sdiff structs
    call shape_diff_alloc(sdiff,nce)
    call shape_diff_alloc(sdiff_org,nce)

    ! fill sdiff_org with derivatives of H, F, t0, v0, D, B
    call shape_factors(cbeos(1),H0,F0,T,n,sdiff_org)

    ! calculate zfac_org and volume
    call csp_Zfac(cbeos(1),T,P,n,phase,zFac_org)
    V = zFac_org*sum(n)*Rgas*T/P ! [m^3]
    ! calculate T0, v0, P0
    T0 = T*sum(n)/F0         ! [K]
    v0 = V/H0*1000           ! [L/mol]
    P0 = zFac_org*kRgas*T0/v0     ! [Pa]
    print *, "p0 from zfac = ",p0
    call csp_refPressure(T0,v0,n,p0)
    print *, "p0 from csp_refPressure = ", p0

    ! fill sdiff_org up with derivatives of M wrt t0 and v0
    call calcRefEqDiff(T0,v0,sdiff_org,1)

    ! calculate the org Helmholtz energy derivatives (and org Mt)
    call calcCombinedDiff(T0,v0,n,sdiff_org)

    ! prepare for volume differentials
    v01 = v0*(1+eps)
    call csp_refPressure(T0,v01,n,p01)
    Zfac = p01*v01/(kRgas*T0)
    call shape_factors(cbeos(1),H,F,T,n,sdiff)
    Vpert = v01*H/1000.0
    call calcRefEqDiff(T0,v01,sdiff,1)
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
    call shape_factors(cbeos(1),H,F,Tpert,n,sdiff)            ! this also calls the mixture routine, which stores derivatives of D and B
    T01 = Tpert*sum(n)/F                                      ! corresponding perturbation of T0
    call csp_refPressure(T01,v0,n,p01)                  ! csp_refPressure assumes the specific volume has units L/mol
    zFac = P01*v0/(kRgas*T01)
    v01 = zFac*kRgas*T01/P01
    call calcRefEqDiff(T01,v01,sdiff,1)           ! fill up sdiff with derivatives of M wrt t0 and v0
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
      call shape_factors(cbeos(1),H,F,T,nn,sdiff)
      T01 = T*sum(nn)/F         ! [K]
      v01 = V/H*1000.0         ! [L/mol]
      !call csp_refPressure(T01,v01,nn,p01)

      !zFac = p01*v01/(kRgas*T01)

      call calcRefEqDiff(T01,v01,sdiff,i_thread)

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
  end subroutine test_shape_factors

  subroutine csp_testPressure(T,v,n)
    use tpvar, only: nce, cbeos
    !$ use omp_lib, only: omp_get_thread_num
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
    integer :: i_thread, i
    i_thread = 1
    !$ i_thread = 1 + omp_get_thread_num()
    sumn = sum(n)

    dv = 1.0e-6*v
    dt = 1.0e-6*T
    dn = 1.0e-6

    print *, "TESTING CSP_REFPRESSURE"
    call csp_refPressure(T,v,n,P,dPdV,dPdT)

    call csp_refPressure(T,v+dv,n,P_pert)
    dpdv_num = (p_pert-p)/dv
    eps_v = abs(dpdv-dpdv_num)

    call csp_refPressure(T+dt,v,n,P_pert)
    dpdt_num = (p_pert-p)/dt
    eps_t = abs(dpdt-dpdt_num)

    print *, "eps,", " releps, ", "value, ", "num_value"
    print *, eps_v, eps_v/abs(dpdv), dpdv, dpdv_num, "(dpdv)"
    print *, eps_t, eps_t/abs(dpdt), dpdt, dpdt_num,  "(dpdt)"

    print *, "TESTING CSP_MIXTPRESSURE"
    call csp_mixtPressure(cbeos(1),T,v,n,P,dPdV,dPdT,dPdn)
    print *,'p',P
    call csp_mixtPressure(cbeos(1),T,v+dv,n,P_pert)
    dpdv_num = (p_pert-p)/dv
    eps_v = abs(dpdv-dpdv_num)

    call csp_mixtPressure(cbeos(1),T+dt,v,n,P_pert)
    dpdt_num = (p_pert-p)/dt
    eps_t = abs(dpdt-dpdt_num)

    do i=1,nce
      nn = n
      nn(i) = nn(i) + dn
      call csp_mixtPressure(cbeos(1),T,v,nn,P_pert)
      dpdn_num(i) = (p_pert-p)/dn
      eps_n(i) = abs(dpdn(i)-dpdn_num(i))
    enddo

    print *, "eps,", " releps, ", "value, ", "num_value"
    print *, eps_v, eps_v/abs(dpdv), dpdv, dpdv_num, "(dpdv)"
    print *, eps_t, eps_t/abs(dpdt), dpdt, dpdt_num,  "(dpdt)"
    do i=1,nce
      print *, eps_n(i), eps_n(i)/abs(dpdn(i)), dpdn(i), dpdn_num(i),  "(dpdn)", i
    enddo
  end subroutine csp_testPressure

  !----------------------------------------------------------------------
  !> Clean up memory used by csp
  !>
  !> \author MH, 2015-01
  !----------------------------------------------------------------------
  subroutine cleanup_csp()
    use tpselect, only: deAllocateEosCubic
    use tpmbwr, only: deallocEosMbwr
    implicit none
    integer :: err, i
    !
    if (allocated(sd)) then
      do i=1,size(sd)
        call shape_diff_dealloc(sd(i))
      enddo
      deallocate(sd,STAT=err);
      if (err /= 0) call stoperror('Not able to deallocate shape_diff struct for csp model')
    endif

    if (allocated(cbrefEos)) then
      do i=1,size(cbrefEos)
        call deAllocateEosCubic(cbrefEos(i))
      enddo
      deallocate(cbrefEos,STAT=err);
      if (err /= 0) call stoperror('Not able to deallocate cbrefEos struct for csp model')
    endif

    if (allocated(mbwrRefEos)) then
      do i=1,size(mbwrRefEos)
        call deallocEosMbwr(mbwrRefEos(i))
      enddo
      deallocate(mbwrRefEos,STAT=err);
      if (err /= 0) call stoperror('Not able to deallocate mbwrRefEos struct for csp model')
    endif

    if (allocated(nistRefEos)) then
       deallocate(nistRefEos,STAT=err)
       if (err /= 0) call stoperror('Not able to deallocate nistRefEos struct for csp model')
    endif

  end subroutine cleanup_csp

  !> Calculates the reduced residual Helmholtz energy F,
  !> along with its derivatives.
  subroutine csp_calcFres(nce,cbeos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use eosdata, only: eoscubic
    !$ use omp_lib, only: omp_get_thread_num
    ! Input.
    integer, intent(in) :: nce
    type (eoscubic), intent(inout) :: cbeos
    real, intent(in) :: T,V,n(nce)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nce)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nce),F_VV,F_Vn(nce),F_nn(nce,nce)
    ! Locals
    real :: Hs, Fs, T0, v0, sumn
    integer :: i_thread
    i_thread = 1
    !$ i_thread = 1 + omp_get_thread_num()
    sumn = sum(n)

    ! compute shape factors and their differentials
    call shape_factors(cbeos,Hs,Fs,T,n,sd(i_thread))
    ! compute v0 and T0
    v0 = 1.0e3*V/Hs  ! must multiply by 1e3 to get liters/mol
    T0 = sumn*T/Fs
    call calcRefEqDiff(T0,v0,sd(i_thread),i_thread)
    call calcCombinedDiff(T0,v0,n,sd(i_thread))

    if (present(F)) F = sd(i_thread)%FF
    if (present(F_T)) F_T = sd(i_thread)%FF_T
    if (present(F_V)) F_V = sd(i_thread)%FF_V
    if (present(F_n)) F_n = sd(i_thread)%FF_i
    if (present(F_TT)) F_TT = sd(i_thread)%FF_TT
    if (present(F_TV)) F_TV = sd(i_thread)%FF_TV
    if (present(F_Tn)) F_Tn = sd(i_thread)%FF_Ti
    if (present(F_VV)) F_VV = sd(i_thread)%FF_VV
    if (present(F_Vn)) F_Vn = sd(i_thread)%FF_Vi
    if (present(F_nn)) F_nn = sd(i_thread)%FF_ij

  end subroutine csp_calcFres

end module csp
