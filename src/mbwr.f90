!> MBWR module
module tpmbwr
  use thermopack_constants, only: Rgas ! [Rgas] has units Pa*m^3/(mol*K) = J/(mol*K)
  use thermopack_constants, only: VAPPH, LIQPH
  implicit none
  save
  integer, parameter :: bplen19 = 6, belen19 = 2      !< the number of coefficients of the rho-powers in the polynomial part and exponential part of MBWR-19
  integer, parameter :: bplen32 = 9, belen32 = 6      !< the number of coefficients of the rho-powers in the polynomial part and exponential part of MBWR-32
  integer, parameter :: Ipol19 = 13, Iexp19 = 6       !< the number of fitted parameters in the polynomial part and exponential part of MBWR-19 (not counting gamma)
  integer, parameter :: Ipol32 = 19, Iexp32 = 13      !< the number of fitted parameters in the polynomial part and exponential part of MBWR-32 (not counting gamma)
  real, parameter    :: PI = 4.e0*ATAN(1.0)

  ! these are used in the initial liquid density guesser
  real :: b_SRK
  real :: m_SRK
  real :: a0_SRK

  ! a debug variable for additional output from the density solver
  logical :: verbose = .false.

  ! A typical polynomial term in the MBWR equation is n*rho^i*T^j
  ! A typical exponential term in the MBWR equations is  n*rho^i*T^j*exp(-gamma*rho^L)
  ! The NIJLarray keeps track of the connection between n,i,j and l.
  type :: NIJLarray
    integer :: len
    real, allocatable, dimension(:) :: N
    integer, allocatable, dimension(:) :: I
    real, allocatable, dimension(:) :: J
    integer, allocatable, dimension(:) :: L
    real :: gamma
  end type NIJLarray

  !> MBWR model type for mbwr19 and mbwr32.
  type :: eosmbwr
    character (LEN=8) :: compId !< Is needed to associate the component to the parameters in MBWRdata
    character (LEN=8) :: eosid !< Not used per now.
    character (LEN=18) :: name !< Not used per now.
    integer :: EqNo, SetNo, LowProp, HighProp
    TYPE(NIJLarray) :: PCoeff_rhoT                !< Pressure explicit terms, in the rho-T form. Doesn't take rho*R*T-term into account.
    TYPE(NIJLarray) :: ZCoeff_redrhoInvredT       !< Compressibility explicit terms. J: exponent for inverse reduced temperature. I: exponent for reduced density. Doesn't take rho*R*T-term into account.
    TYPE(NIJLarray) :: redResHelmCoeff_redrhoT    !< Helmholtz explicit terms, using reduced temperature (J) and reduced density (I). Takes all terms into account.

    ! lengths of things
    integer :: Ipol, Iexp
    integer :: bplen, belen
    integer :: HelmLength     !< Number of terms in the integrated Helmholtz expression.
    integer :: Helm_poly_len  !< Number of polynomial terms in the integrated Helmholtz expression.

    ! some component dependent parameters
    real :: tTriple !< Triple point temperature
    real :: pTriple !< Triple point pressure
    real :: tc    !< Critical temperature
    real :: pc    !< Critical pressure
    real :: rc    !< Critical density (mol/L)
    real :: zc    !< Critical compressibility factor
    real :: acf   !< Acentric factor, used in the SRK initial value method in the density solver.
    real :: gamma !< The parameter gamma in the MBWR equation
  contains
    procedure, public :: dealloc => deallocEosMbwr
  end type eosmbwr

contains

  subroutine allocNIJL(NIJL,len)
    implicit none
    TYPE(NIJLarray), INTENT(INOUT) :: NIJL
    integer, INTENT(IN) :: len
    call deallocNIJL(NIJL)
    ALLOCATE(NIJL%N(len))
    ALLOCATE(NIJL%I(len))
    ALLOCATE(NIJL%J(len))
    ALLOCATE(NIJL%L(len))
  end subroutine allocNIJL

  subroutine deallocNIJL(NIJL)
    implicit none
    TYPE(NIJLarray), INTENT(INOUT) :: NIJL
    if (allocated(NIJL%N)) deallocate(NIJL%N)
    if (allocated(NIJL%I)) deallocate(NIJL%I)
    if (allocated(NIJL%J)) deallocate(NIJL%J)
    if (allocated(NIJL%L)) deallocate(NIJL%L)
  end subroutine deallocNIJL

  subroutine deallocEosMbwr(refEosMbwr)
    implicit none
    class(eosmbwr), INTENT(INOUT) :: refEosMbwr
    call deallocNIJL(refEosMbwr%PCoeff_rhoT)
    call deallocNIJL(refEosMbwr%ZCoeff_redrhoInvredT)
    call deallocNIJL(refEosMbwr%redResHelmCoeff_redrhoT)
  end subroutine deallocEosMbwr

  subroutine NIJLassign(NIJL,idx,nn,ii,jj,ll)
    implicit none
    real, INTENT(IN) :: nn,jj
    integer, INTENT(IN) :: idx, ii, ll
    TYPE(NIJLarray), INTENT(INOUT) :: NIJL
    NIJL%N(idx) = nn
    NIJL%I(idx) = ii
    NIJL%J(idx) = jj
    NIJL%L(idx) = ll
  end subroutine NIJLassign

  subroutine initializeMBWRmodel(compId,model,nineteenOr32)
    character(LEN=*), INTENT(IN) :: compId !< e.g. C3
    TYPE(eosmbwr), INTENT(INOUT) :: model
    integer, intent(in) :: nineteenor32   !< 19 or 32
    CALL readDbParameters(compId,model,nineteenOr32)
    CALL computeZCoeff(model)
    CALL computeHelmCoeff(model) ! Must be computed after Z-coefficients.
  end subroutine initializeMBWRmodel

  ! Read parameters for component compId from the module tpmbwrdata
  subroutine readDbParameters(compId,model,nineteenOr32)
    use tpmbwrdata   ! For the correlated coefficients.
    use compdatadb   ! For component data. Module contained in tpinput.f90.
    use compdata_init   ! For component data. Module contained in tpinput.f90.
    implicit none
    character(LEN=*), INTENT(IN) :: compId
    TYPE(eosmbwr), INTENT(INOUT) :: model
    integer, intent(in) :: nineteenor32
    type(mbwr19Data) :: mbwr19
    type(mbwr32Data) :: mbwr32
    integer :: i, mbwridx, compDbIdx, compDbIdx_mbwrcrit
    real :: tc_mbwr, pc_mbwr, rc_mbwr

    !********************************************
    ! Read in the parameters for the pressure explicit
    ! form of the MBWR equations. Units:
    ! [P] = Pa, [T] = K, [rho] = mol/L.
    !********************************************
    model%compId = trim(compId)
    if (nineteenOr32 .eq. 19) then
      model%name = "Bender"

      call allocNIJL(model%PCoeff_rhoT,19)
      mbwridx = getMBWR19index(compId)
      if (mbwridx .eq. 0) then
        call stoperror("Chosen component not in MBWR19 database.")
      end if
      mbwr19 = mbwr19Array(mbwridx)
      DO i=1,mbwr19%Ndata-1
        model%PCoeff_rhoT%N(i) = mbwr19%coeff(i)
      END DO

      model%gamma = mbwr19%coeff(mbwr19%Ndata)
      model%EqNo = 19
      model%LowProp = mbwr19%LowProp
      model%HighProp = mbwr19%HighProp
      model%bplen = bplen19; model%belen = belen19
      model%Ipol = Ipol19; model%Iexp = Iexp19
      model%Helm_poly_len = 16
      model%HelmLength = 22
      CALL fillIJL19(model%PCoeff_rhoT)

    else if (nineteenOr32 .eq. 32) then
      model%name = "MBWR32"

      call allocNIJL(model%PCoeff_rhoT,32)
      mbwridx = getMBWR32index(compId)
      if (mbwridx .eq. 0) then
        call stoperror("Chosen component not in MBWR32 database.")
      end if
      mbwr32 = mbwr32Array(mbwridx)
      DO i=1,mbwr32%Ndata-1
        model%PCoeff_rhoT%N(i) = mbwr32%coeff(i)
      END DO
      model%gamma = mbwr32%coeff(mbwr32%Ndata)
      model%EqNo = 32
      model%LowProp = mbwr32%LowProp
      model%HighProp = mbwr32%HighProp
      model%bplen = bplen32; model%belen = belen32
      model%Ipol = Ipol32; model%Iexp = Iexp32
      model%Helm_poly_len = 22
      model%HelmLength = 40
      CALL fillIJL32(model%PCoeff_rhoT)
    else
      call stoperror("The only MBWR models available are 19 and 32.")
    end if

    ! store component-dependent critical parameters and accentric factor
    compDbIdx = getCompDBindex(compId)
    model%acf = compdb(compDbIdx)%acf

    ! Get the critical parameters as according to the actual MBWR equation
    if (nineteenOr32 .eq. 19) then
      compDbIdx_mbwrcrit = getMBWR19critPropIndex(compId)
      if (compDbIdx_mbwrcrit .gt. 0) then ! equals 0 if the critical parameters aren't in tpmbwrdata
        tc_mbwr = tc19_computed(compDbIdx_mbwrcrit)
        pc_mbwr = pc19_computed(compDbIdx_mbwrcrit)
        rc_mbwr = rc19_computed(compDbIdx_mbwrcrit)
      else
        ! get estimates for critical parameters from database
        pc_mbwr = compdb(compDbIdx)%pc
        tc_mbwr = compdb(compDbIdx)%tc
        model%zc = compdb(compDbIdx)%zc
        rc_mbwr = pc_mbwr/(Rgas*model%zc*tc_mbwr)
        call MBWR_criticalParameters(tc_mbwr,pc_mbwr,rc_mbwr,model)
      end if
    elseif (nineteenOr32 .eq. 32) then
      compDbIdx_mbwrcrit = getMBWR32critPropIndex(compId)
      if (compDbIdx_mbwrcrit .gt. 0) then ! equals 0 if the critical parameters aren't in tpmbwrdata
        tc_mbwr = tc32_computed(compDbIdx_mbwrcrit)
        pc_mbwr = pc32_computed(compDbIdx_mbwrcrit)
        rc_mbwr = rc32_computed(compDbIdx_mbwrcrit)
      else
        ! get estimates for critical parameters from database
        pc_mbwr = compdb(compDbIdx)%pc
        tc_mbwr = compdb(compDbIdx)%tc
        model%zc = compdb(compDbIdx)%zc
        rc_mbwr = pc_mbwr/(Rgas*model%zc*tc_mbwr)
        call MBWR_criticalParameters(tc_mbwr,pc_mbwr,rc_mbwr,model)
      end if
    end if

    model%pc = pc_mbwr
    model%tc = tc_mbwr
    model%rc = rc_mbwr
    model%zc = pc_mbwr/(rc_mbwr*Rgas*tc_mbwr)


    ! get triple point properties (if they are stored)
    compDbIdx = getTriplePointPropIndex(compId)
    if (compDbIdx > 0) then
      model%tTriple = tTriple_coolprop(compDbIdx)
      model%pTriple = pTriple_coolprop(compDbIdx)
    end if


    ! precalculate some constants in the SRK initial density finder
    b_SRK = 0.08664*Rgas*model%tc/model%pc
    m_SRK = 0.480+1.547*model%acf-0.176*(model%acf)**2
    a0_SRK = 0.42747*(Rgas*model%tc)**2/model%pc
  end subroutine readDbParameters

  ! This function encodes the order of the parameters in the data module tpmbwrdata.
  ! The N coefficient is the coefficient in front of the term.
  ! The I coefficient is the power of the density.
  ! The J coefficient is the power of the temperature.
  ! The L coefficient is the power in the exponential.
  subroutine fillIJL32(NIJL)
    implicit none
    TYPE(NIJLarray), INTENT(INOUT) :: NIJL
    NIJL%I(1)  = 2;   NIJL%J(1) =+1.e+0;   NIJL%L(1) = 0;
    NIJL%I(2)  = 2;   NIJL%J(2) =+0.5e+0;  NIJL%L(2) = 0;
    NIJL%I(3)  = 2;   NIJL%J(3) =+0.e+0;   NIJL%L(3) = 0;
    NIJL%I(4)  = 2;   NIJL%J(4) =-1.e+0;   NIJL%L(4) = 0;
    NIJL%I(5)  = 2;   NIJL%J(5) =-2.e+0;   NIJL%L(5) = 0;
    NIJL%I(6)  = 3;   NIJL%J(6) =+1.e+0;   NIJL%L(6) = 0;
    NIJL%I(7)  = 3;   NIJL%J(7) =-0.e+0;   NIJL%L(7) = 0;
    NIJL%I(8)  = 3;   NIJL%J(8) =-1.e+0;   NIJL%L(8) = 0;
    NIJL%I(9)  = 3;   NIJL%J(9) =-2.e+0;   NIJL%L(9) = 0;
    NIJL%I(10) = 4;  NIJL%J(10) =+1.e+0;   NIJL%L(10) = 0;
    NIJL%I(11) = 4;  NIJL%J(11) =+0.e+0;   NIJL%L(11) = 0;
    NIJL%I(12) = 4;  NIJL%J(12) =-1.e+0;   NIJL%L(12) = 0;
    NIJL%I(13) = 5;  NIJL%J(13) =+0.e+0;   NIJL%L(13) = 0;
    NIJL%I(14) = 6;  NIJL%J(14) =-1.e+0;   NIJL%L(14) = 0;
    NIJL%I(15) = 6;  NIJL%J(15) =-2.e+0;   NIJL%L(15) = 0;
    NIJL%I(16) = 7;  NIJL%J(16) =-1.e+0;   NIJL%L(16) = 0;
    NIJL%I(17) = 8;  NIJL%J(17) =-1.e+0;   NIJL%L(17) = 0;
    NIJL%I(18) = 8;  NIJL%J(18) =-2.e+0;   NIJL%L(18) = 0;
    NIJL%I(19) = 9;  NIJL%J(19) =-2.e+0;   NIJL%L(19) = 0;
    NIJL%I(20) = 3;  NIJL%J(20) =-2.e+0;   NIJL%L(20) = 2;
    NIJL%I(21) = 3;  NIJL%J(21) =-3.e+0;   NIJL%L(21) = 2;
    NIJL%I(22) = 5;  NIJL%J(22) =-2.e+0;   NIJL%L(22) = 2;
    NIJL%I(23) = 5;  NIJL%J(23) =-4.e+0;   NIJL%L(23) = 2;
    NIJL%I(24) = 7;  NIJL%J(24) =-2.e+0;   NIJL%L(24) = 2;
    NIJL%I(25) = 7;  NIJL%J(25) =-3.e+0;   NIJL%L(25) = 2;
    NIJL%I(26) = 9;  NIJL%J(26) =-2.e+0;   NIJL%L(26) = 2;
    NIJL%I(27) = 9;  NIJL%J(27) =-4.e+0;   NIJL%L(27) = 2;
    NIJL%I(28) = 11; NIJL%J(28) =-2.e+0;   NIJL%L(28) = 2;
    NIJL%I(29) = 11; NIJL%J(29) =-3.e+0;   NIJL%L(29) = 2;
    NIJL%I(30) = 13; NIJL%J(30) =-2.e+0;   NIJL%L(30) = 2;
    NIJL%I(31) = 13; NIJL%J(31) =-3.e+0;   NIJL%L(31) = 2;
    NIJL%I(32) = 13; NIJL%J(32) =-4.e+0;   NIJL%L(32) = 2;
  end subroutine fillIJL32

  subroutine fillIJL19(NIJL)
    implicit none
    TYPE(NIJLarray), INTENT(INOUT) :: NIJL
    NIJL%I(1)  = 2;   NIJL%J(1) =+1.e+0;   NIJL%L(1) = 0;
    NIJL%I(2)  = 2;   NIJL%J(2) =+0.e+0;   NIJL%L(2) = 0;
    NIJL%I(3)  = 2;   NIJL%J(3) =-1.e+0;   NIJL%L(3) = 0;
    NIJL%I(4)  = 2;   NIJL%J(4) =-2.e+0;   NIJL%L(4) = 0;
    NIJL%I(5)  = 2;   NIJL%J(5) =-3.e+0;   NIJL%L(5) = 0;
    NIJL%I(6)  = 3;   NIJL%J(6) =+1.e+0;   NIJL%L(6) = 0;
    NIJL%I(7)  = 3;   NIJL%J(7) =+0.e+0;   NIJL%L(7) = 0;
    NIJL%I(8)  = 3;   NIJL%J(8) =-1.e+0;   NIJL%L(8) = 0;
    NIJL%I(9)  = 4;   NIJL%J(9) =+1.e+0;   NIJL%L(9) = 0;
    NIJL%I(10) = 4;  NIJL%J(10) =+0.e+0;   NIJL%L(10) = 0;
    NIJL%I(11) = 5;  NIJL%J(11) =+1.e+0;   NIJL%L(11) = 0;
    NIJL%I(12) = 5;  NIJL%J(12) =+0.e+0;   NIJL%L(12) = 0;
    NIJL%I(13) = 6;  NIJL%J(13) =+0.e+0;   NIJL%L(13) = 0;
    NIJL%I(14) = 3;  NIJL%J(14) =-2.e+0;   NIJL%L(14) = 2;
    NIJL%I(15) = 3;  NIJL%J(15) =-3.e+0;   NIJL%L(15) = 2;
    NIJL%I(16) = 3;  NIJL%J(16) =-4.e+0;   NIJL%L(16) = 2;
    NIJL%I(17) = 5;  NIJL%J(17) =-2.e+0;   NIJL%L(17) = 2;
    NIJL%I(18) = 5;  NIJL%J(18) =-3.e+0;   NIJL%L(18) = 2;
    NIJL%I(19) = 5;  NIJL%J(19) =-4.e+0;   NIJL%L(19) = 2;
  end subroutine fillIJL19

  subroutine computeZCoeff(model)
    implicit none
    TYPE(eosmbwr), INTENT(INOUT) :: model
    integer :: r, k
    real :: rc, tc
    real :: nn, jj
    real :: gamma
    integer :: ii, ll
    integer :: EqNo
    gamma = model%gamma
    rc = model%rc; tc = model%tc
    EqNo = model%EqNo ! 19 or 32
    ! Compute the compressibility explicit coefficients, using reduced density and inverse reduced temperature.
    if (EqNo .eq. 32) then
      CALL allocNIJL(model%ZCoeff_redrhoInvredT,EqNo)
      DO r=1,EqNo
        nn = model%PCoeff_rhoT%N(r)*1.0e3 ! Needed to get the right units
        ii = model%PCoeff_rhoT%I(r)
        jj = -model%PCoeff_rhoT%J(r)  ! This algorithm works with inverse reduced temperatures
        ll = model%PCoeff_rhoT%L(r)
        CALL NIJLassign(                          &
             model%ZCoeff_redrhoInvredT,          &
             r,                                   &
             nn/Rgas,                            &
             ii-1,                                &
             jj+1,                                &
             ll)
      END DO
    else
      CALL allocNIJL(model%ZCoeff_redrhoInvredT,EqNo)
      DO r=1,EqNo
        nn = model%PCoeff_rhoT%N(r)
        ii = model%PCoeff_rhoT%I(r)
        jj = -model%PCoeff_rhoT%J(r)  ! This algorithm works with inverse reduced temperatures
        ll = model%PCoeff_rhoT%L(r)
        CALL NIJLassign(                          &
             model%ZCoeff_redrhoInvredT,          &
             r,                                   &
             nn/Rgas,                            &
             ii-1,                                &
             jj+1,                                &
             ll)
      END DO
      DO k = 2,5
        model%ZCoeff_redrhoInvredT%N(k) = -model%ZCoeff_redrhoInvredT%N(k)  ! Different sign conventions for MBWR-32 and MBWR-19. See Polt.
      END DO
    end if

    model%ZCoeff_redrhoInvredT%gamma = gamma
  end subroutine computeZCoeff

  ! Reference: R. Span, "Multiparameter equations of state", 2003.
  subroutine computeHelmCoeff(model)
    implicit none
    TYPE(eosmbwr), INTENT(INOUT) :: model
    ! local variables
    TYPE(NIJLarray) :: Helm, Z  ! Dummy holders for the Helmholtz coefficients and the compressibility coefficients.
    integer :: k
    integer :: Ipol
    real :: gamma

    IF (model%EqNo .eq. 19) THEN ! MBWR19
      Ipol = Ipol19
      CALL allocNIJL(Z,19)
      CALL allocNIJL(Helm,22)
    ELSE IF (model%EqNo .eq. 32) THEN !MBWR32
      Ipol = Ipol32
      CALL allocNIJL(Z,32)
      CALL allocNIJL(Helm,40)
    END IF

    Z = model%ZCoeff_redrhoInvredT            ! The J-exponents in Z are for inverse reduced temperatures.
    gamma = model%ZCoeff_redrhoInvredT%gamma

    ! Integration of the polynomial part
    DO k=1,Ipol
      Helm%N(k) = Z%N(k)/Z%I(k)
      Helm%I(k) = Z%I(k)
      Helm%J(k) = Z%J(k)
      Helm%L(k) = Z%L(k)
    END DO

    ! Integration of the exponential part
    IF (model%EqNo .eq. 19) THEN !Bender (triple checked)
      Helm%N(14) = Z%N(14)/(2*gamma) + Z%N(17)/(2*gamma**2)
      Helm%I(14) = 0
      Helm%J(14) = 3.e0
      Helm%L(14) = 0

      Helm%N(15) = Z%N(15)/(2*gamma) + Z%N(18)/(2*gamma**2)
      Helm%I(15) = 0
      Helm%J(15) = 4.e0
      Helm%L(15) = 0

      Helm%N(16) = Z%N(16)/(2*gamma) + Z%N(19)/(2*gamma**2)
      Helm%I(16) = 0
      Helm%J(16) = 5.e0
      Helm%L(16) = 0

      Helm%N(17) = -Helm%N(14)
      Helm%I(17) = 0
      Helm%J(17) = 3.e0
      Helm%L(17) = 2

      Helm%N(18) = -Helm%N(15)
      Helm%I(18) = 0
      Helm%J(18) = 4.e0
      Helm%L(18) = 2

      Helm%N(19) = -Helm%N(16)
      Helm%I(19) = 0
      Helm%J(19) = 5.e0
      Helm%L(19) = 2

      Helm%N(20) = -Z%N(17)/(2*gamma)
      Helm%I(20) = 2
      Helm%J(20) = 3.e0
      Helm%L(20) = 2

      Helm%N(21) = -Z%N(18)/(2*gamma)
      Helm%I(21) = 2
      Helm%J(21) = 4.e0
      Helm%L(21) = 2

      Helm%N(22) = -Z%N(19)/(2*gamma)
      Helm%I(22) = 2
      Helm%J(22) = 5.e0
      Helm%L(22) = 2

      CALL allocNIJL(model%redResHelmCoeff_redrhoT,22)

    ELSE IF (model%EqNo .eq. 32) THEN !MBWR32
      Helm%N(20) = Z%N(20)/(2*gamma) + Z%N(22)/(2*gamma**2) + Z%N(24)/gamma**3 &
           + 3*Z%N(26)/gamma**4 + 12*Z%N(28)/gamma**5 + 60*Z%N(30)/gamma**6
      Helm%I(20) = 0
      Helm%J(20) = 3.e0
      Helm%L(20) = 0

      Helm%N(21) = Z%N(21)/(2*gamma) + Z%N(25)/gamma**3 + 12*Z%N(29)/gamma**5 + 60*Z%N(31)/gamma**6
      Helm%I(21) = 0
      Helm%J(21) = 4.e0
      Helm%L(21) = 0

      Helm%N(22) = Z%N(23)/(2*gamma**2) + 3*Z%N(27)/gamma**4 + 60*Z%N(32)/gamma**6
      Helm%I(22) = 0
      Helm%J(22) = 5.e0
      Helm%L(22) = 0

      Helm%N(23) = -Helm%N(20)
      Helm%I(23) = 0
      Helm%J(23) = 3.e0
      Helm%L(23) = 2

      Helm%N(24) = -Helm%N(21)
      Helm%I(24) = 0
      Helm%J(24) = 4.e0
      Helm%L(24) = 2

      Helm%N(25) = -Helm%N(22)
      Helm%I(25) = 0
      Helm%J(25) = 5.e0
      Helm%L(25) = 2

      Helm%N(26) = -gamma*(Helm%N(20) - Z%N(20)/(2*gamma))
      Helm%I(26) = 2
      Helm%J(26) = 3.e0
      Helm%L(26) = 2

      Helm%N(27) = -gamma*(Helm%N(21) - Z%N(21)/(2*gamma))
      Helm%I(27) = 2
      Helm%J(27) = 4.e0
      Helm%L(27) = 2

      Helm%N(28) = -gamma*Helm%N(22)
      Helm%I(28) = 2
      Helm%J(28) = 5.e0
      Helm%L(28) = 2

      Helm%N(29) = -Z%N(24)/(2*gamma) - 3*Z%N(26)/(2*gamma**2) - 6*Z%N(28)/gamma**3 - 30*Z%N(30)/gamma**4
      Helm%I(29) = 4
      Helm%J(29) = 3.e0
      Helm%L(29) = 2

      Helm%N(30) = Helm%N(27)*gamma/2
      Helm%I(30) = 4
      Helm%J(30) = 4.e0
      Helm%L(30) = 2

      Helm%N(31) = -3*Z%N(27)/(2*gamma**2) - 30*Z%N(32)/gamma**4
      Helm%I(31) = 4
      Helm%J(31) = 5.e0
      Helm%L(31) = 2

      Helm%N(32) = -Z%N(26)/(2*gamma) - 2*Z%N(28)/gamma**2 - 10*Z%N(30)/gamma**3
      Helm%I(32) = 6
      Helm%J(32) = 3.e0
      Helm%L(32) = 2

      Helm%N(33) = -2*Z%N(29)/gamma**2 - 10*Z%N(31)/gamma**3
      Helm%I(33) = 6
      Helm%J(33) = 4.e0
      Helm%L(33) = 2

      Helm%N(34) = -Z%N(27)/(2*gamma) - 10*Z%N(32)/gamma**3
      Helm%I(34) = 6
      Helm%J(34) = 5.e0
      Helm%L(34) = 2

      Helm%N(35) = -Z%N(28)/(2*gamma) - 5*Z%N(30)/(2*gamma**2)
      Helm%I(35) = 8
      Helm%J(35) = 3.e0
      Helm%L(35) = 2

      Helm%N(36) = -Z%N(29)/(2*gamma) - 5*Z%N(31)/(2*gamma**2)
      Helm%I(36) = 8
      Helm%J(36) = 4.e0
      Helm%L(36) = 2

      Helm%N(37) = -5*Z%N(32)/(2*gamma**2)
      Helm%I(37) = 8
      Helm%J(37) = 5.e0
      Helm%L(37) = 2

      Helm%N(38) = -Z%N(30)/(2*gamma)
      Helm%I(38) = 10
      Helm%J(38) = 3.e0
      Helm%L(38) = 2

      Helm%N(39) = -Z%N(31)/(2*gamma)
      Helm%I(39) = 10
      Helm%J(39) = 4.e0
      Helm%L(39) = 2

      Helm%N(40) = -Z%N(32)/(2.e0*gamma)
      Helm%I(40) = 10
      Helm%J(40) = 5.e0
      Helm%L(40) = 2

      CALL allocNIJL(model%redResHelmCoeff_redrhoT,40)
    END IF

    Helm%J = -Helm%J ! Convert to a model taking in reduced temperature instead of inverse reduced temperature
    model%redResHelmCoeff_redrhoT = Helm
    model%redResHelmCoeff_redrhoT%gamma = gamma ! Important to have this statement after the preceding one!
  end subroutine computeHelmCoeff


  !  ********************************************************************
  !  *  Temperature dependent constants and their derivatives for the
  !  *  MBWR equations. Units: T [K] P [Pa] rho [mol/m^3]
  !  * ------------------------------------------------------------------
  !  *
  !  * I  nTderivatives:  Flag for calculation
  !  *               1 : Constants
  !  *               2 : First derivative of constants w.r.t. temperature
  !  *               3 : Second derivative of constants w.r.t. temperature
  !  * I  T        : Temperature         ( K )
  !  ********************************************************************
  subroutine MBWR_coef(nTderivatives,T,rhoCoef,model)
    implicit none
    ! input variables
    integer, intent(in) :: nTderivatives
    type(eosmbwr), intent(in) :: model
    real, intent(in) :: T
    ! output variables
    real, dimension(:), intent(out) :: rhoCoef   ! assumes rhoCoef has the right size. Initialized in readDbParameters.
    !locals
    real :: T2, T3, T4, T5
    real, dimension(model%EqNo) :: c
    T2 = T*T
    T3 = T2*T
    T4 = T3*T

    if (model%EqNo .eq. 19) then ! MBWR-19. Has been double-checked.
      c = model%PCoeff_rhoT%N
      if (nTderivatives == 1) then
        rhoCoef(1) =  Rgas*T ! BP(1)
        rhoCoef(2) =  c(1)*T - c(2) - c(3)/T - c(4)/T2 - c(5)/T3  ! This is correct. See e.g. the phd thesis by Axel Polt (1987).
        rhoCoef(3) =  c(6)*T + c(7) + c(8)/T ! BP(3)
        rhoCoef(4) =  c(9)*T + c(10) ! BP(4)
        rhoCoef(5) =  c(11)*T + c(12)  ! BP(5)
        rhoCoef(6) =  c(13)  ! BP(6)
        rhoCoef(7) =  (c(14) + c(15)/T + c(16)/T2)/T2 ! BE(1)
        rhoCoef(8) =  (c(17) + c(18)/T + c(19)/T2)/T2 ! BE(2)
      else if (nTderivatives == 2) then ! First derivative wrt temperature
        rhoCoef(1) =  Rgas !BP(1)
        rhoCoef(2) =  c(1) + c(3)/T2 + 2.0*c(4)/T3 + 3.0*c(5)/T4 !BP(2)
        rhoCoef(3) =  c(6) - c(8)/T2 !BP(3)
        rhoCoef(4) =  c(9) !BP(4)
        rhoCoef(5) =  c(11) !BP(5)
        rhoCoef(6) =  0.0 !BP(6)
        rhoCoef(7) =  (-2.0*c(14) - 3.0*c(15)/T - 4.0*c(16)/T2)/T3 !BE(1)
        rhoCoef(8) =  (-2.0*c(17) - 3.0*c(18)/T - 4.0*c(19)/T2)/T3 !BE(2)
      else !nTderivatives = 3. Second derivative wrt temperature
        T5 = T4*T
        rhoCoef(1) =  0.0 !BP(1)
        rhoCoef(2) =  -2.0*c(3)/T3 - 6.0*c(4)/T4 - 12.0*c(5)/T5 !BP(2)
        rhoCoef(3) =  2.0*c(8)/T3 !BP(3)
        rhoCoef(4) =  0.0 !BP(4)
        rhoCoef(5) =  0.0  !BP(5)
        rhoCoef(6) =  0.0 !BP(6)
        rhoCoef(7) =  (6.0*c(14) + 12.0*c(15)/T + 20.0*c(16)/T2)/T4 !BE(1)
        rhoCoef(8) =  (6.0*c(17) + 12.0*c(18)/T + 20.0*c(19)/T2)/T4 !BE(2)
      end if
      rhoCoef(2:) = rhoCoef(2:)*1.0e+03 ! convert to right units
    else if (model%EqNo .eq. 32) then ! MBWR-32
      c = model%PCoeff_rhoT%N
      if (nTderivatives == 1) then
        rhoCoef(1) = Rgas*T ! BP(1)
        rhoCoef(2) = c(1)*T + c(2)*SQRT(T) + c(3) + c(4)/T + c(5)/T2 ! BP(2)
        rhoCoef(3) = c(6)*T + c(7) + c(8)/T + c(9)/T2 ! BP(3)
        rhoCoef(4) = c(10)*T + c(11) + c(12)/T !    BP(4)
        rhoCoef(5) = c(13) !    BP(5)
        rhoCoef(6) = c(14)/T + c(15)/T2 !    BP(6)
        rhoCoef(7) = c(16)/T !    BP(7)
        rhoCoef(8) = c(17)/T + c(18)/T2 !    BP(8)
        rhoCoef(9) = c(19)/T2 !    BP(9)
        rhoCoef(10) = (c(20) + c(21)/T)/T2 !    BE(1)
        rhoCoef(11) = (c(22) + c(23)/T2)/T2 !    BE(2)
        rhoCoef(12) = (c(24) + c(25)/T)/T2 !    BE(3)
        rhoCoef(13) = (c(26) + c(27)/T2)/T2 !    BE(4)
        rhoCoef(14) = (c(28) + c(29)/T)/T2 !    BE(5)
        rhoCoef(15) = (c(30) + c(31)/T + c(32)/T2)/T2 !    BE(6)
      else if (nTderivatives == 2) then ! First derivative wrt temperature
        rhoCoef(1) =  Rgas ! BP(1)
        rhoCoef(2) =  c(1) + 0.5*c(2)/SQRT(T) - c(4)/T2 - 2.0*c(5)/T3 !    BP(2)
        rhoCoef(3) =  c(6) - c(8)/T2 - 2.0*c(9)/T3 !    BP(3)
        rhoCoef(4) =  c(10) - c(12)/T2 !    BP(4)
        rhoCoef(5) =  0.0 !    BP(5)
        rhoCoef(6) = -c(14)/T2 - 2.0*c(15)/T3 !    BP(6)
        rhoCoef(7) = -c(16)/T2 !    BP(7)
        rhoCoef(8) = -c(17)/T2 - 2.0*c(18)/T3 !    BP(8)
        rhoCoef(9) = -2.0*c(19)/T3 !    BP(9)
        rhoCoef(10)= -(2.0*c(20) + 3.0*c(21)/T)/T3 !    BE(1)
        rhoCoef(11)= -(2.0*c(22) + 4.0*c(23)/T2)/T3 !    BE(2)
        rhoCoef(12)= -(2.0*c(24) + 3.0*c(25)/T)/T3 !    BE(3)
        rhoCoef(13)= -(2.0*c(26) + 4.0*c(27)/T2)/T3 !    BE(4)
        rhoCoef(14)= -(2.0*c(28) + 3.0*c(29)/T)/T3 !    BE(5)
        rhoCoef(15)= -(2.0*c(30) + 3.0*c(31)/T + 4.0*c(32)/T2)/T3 !    BE(6)
      else !nTderivatives == 3. Second derivative wrt temperature
        rhoCoef(1) =  0.0 !BP(1)
        rhoCoef(2) = -0.25*c(2)/T**1.5 + 2.0*c(4)/T3 + 6.0*c(5)/T4 !BP(2)
        rhoCoef(3) =  2.0*c(8)/T3 + 6.0*c(9)/T4 !BP(3)
        rhoCoef(4) =  2.0*c(12)/T3 !BP(4)
        rhoCoef(5) =  0.0 !BP(5)
        rhoCoef(6) =  2.0*c(14)/T3 + 6.0*c(15)/T4 !BP(6)
        rhoCoef(7) =  2.0*c(16)/T3 !BP(7)
        rhoCoef(8) =  2.0*c(17)/T3 + 6.0*c(18)/T4 !BP(8)
        rhoCoef(9) =  6.0*c(19)/T4 !BP(9)
        rhoCoef(10)=  (6.0*c(20) + 1.2e1*c(21)/T)/T4 !BE(1)
        rhoCoef(11)=  (6.0*c(22) + 2.0e1*c(23)/T2)/T4 !BE(2)
        rhoCoef(12)=  (6.0*c(24) + 1.2e1*c(25)/T)/T4 !BE(3)
        rhoCoef(13)=  (6.0*c(26) + 2.0e1*c(27)/T2)/T4 !BE(4)
        rhoCoef(14)=  (6.0*c(28) + 1.2e1*c(29)/T)/T4 !BE(5)
        rhoCoef(15)=  (6.0*c(30) + 1.2e1*c(31)/T +  2.0e1*c(32)/T2)/T4 !    BE(6)
      end if
    else
      call stoperror("error in function MBWR_coef")
    end if

  end subroutine MBWR_coef

  subroutine makeParam(parameters,T,model,nTderivatives) ! optional argument if one wants temperature derivatives?
    implicit none
    !input
    real, intent(in) :: T
    type(eosmbwr), intent(in) :: model
    integer, optional, intent(in) :: nTderivatives
    !output
    !real, dimension(model%param_len), intent(out) :: parameters
    real, dimension(1+model%bplen+model%belen), intent(out) :: parameters
    !local variables
    !real, dimension(model%param_len - 1) :: rhoCoef
    real, dimension(model%bplen+model%belen) :: rhoCoef
    integer :: iter

    if (.not. present(nTderivatives)) then
      call MBWR_coef(1,T,rhoCoef,model)
    else
      call MBWR_coef(nTderivatives+1,T,rhoCoef,model) ! assuming MBWR_coef uses the other convention
    end if

    parameters(1) = model%gamma
    do iter = 2,model%bplen+model%belen+1
      parameters(iter) = rhoCoef(iter-1)
    end do
  end subroutine makeParam


  ! Calculate the pressure and the first and second derivatives of
  ! pressure w.r.t. density from an mbwr equation of state.
  subroutine MBWR_pressure(rho, param, p, dpdrho, d2pdrho2) ! contents of param in order : gamma, rhoCoef
    implicit none
    ! output variables
    real, intent(out) :: p
    real, intent(out), optional :: dpdrho
    real, intent(out), optional :: d2pdrho2
    ! input variables
    real, intent(in) :: rho
    real, dimension(:), intent(in) :: param
    ! local variables
    real, dimension(3) :: expo, poly
    real :: gamma, ex, pled, eled, rhoSquared
    integer :: m,n
    integer :: bplen, belen
    integer :: lenParam

    ! initialize some local variables
    gamma = param(1)
    lenParam = size(param) ! essential that param has exactly the right length
    rhoSquared = rho*rho
    ex = exp(-gamma*rhoSquared)
    poly = 0
    expo = 0
    ! deduce if it is the MBWR-19 or MBWR-32 equation we're dealing with
    if (lenParam .eq. 1+bplen19+belen19) then
      bplen = bplen19
      belen = belen19
    else if (lenParam .eq. 1+bplen32+belen32) then
      bplen = bplen32
      belen = belen32
    end if

    ! computations for the polynomial part (perhaps faster to move if's out of the loop, but probably the compiler catches this)
    do n=1,bplen
      pled = param(n+1)*rho**n ! parameters 2..bplen+1 are bp  !BP(N)*rho**n
      poly(1) = poly(1) + pled
      if ( present(dpdrho) ) poly(2) = poly(2) + pled*n
      if ( present(d2pdrho2) ) poly(3) = poly(3) + pled*(n*(n-1))
    end do

    ! computations for the exponential part
    do m = 1,belen
      eled = param(m+1+bplen)*rho**(2*m + 1) ! parameters bplen+2...bplen+belen+1 are be ! BE(M)*rho**(2*m + 1)
      expo(1) = expo(1) + eled
      if ( present(dpdrho) ) expo(2) = expo(2) + eled * ((2*M+1)/rho - 2.0*gamma*rho)
      if ( present(d2pdrho2) ) expo(3) = expo(3) + &
           eled*(2.0*((M*(2*M+1))/rhoSquared - gamma*((4*M+3) - 2.0*gamma*rhoSquared)))
    end do

    ! store results
    p = poly(1) + ex*expo(1)
    if ( present(dpdrho) ) dpdrho = poly(2)/rho + ex*expo(2)
    if ( present(d2pdrho2) ) d2pdrho2 = poly(3)/rhoSquared + ex*expo(3)
  end subroutine MBWR_pressure

  !> Helper function for the search for density root in newton_density
  logical function densityRootDoesntExist(redT,phase, rho_old, rho_new, prho_old, prho, dpdrho, iter)
    implicit none
    real, intent(in) :: redT
    real, intent(inout) :: rho_old, rho_new, prho_old, prho, dpdrho
    integer, intent(in) :: phase
    real :: drho
    integer, intent(in) :: iter
    densityRootDoesntExist = .false.
    drho = rho_new-rho_old

    ! If drho is too small, the test for (prho-prho_old)/drho below is unstable (besides, small drho usually indicates convergence.)
    ! At supercritical temperatures, the density always exists, irrespective of input phase.
    if (abs(drho/rho_old) < 1e-9 .or. redT > 1.01) then
      densityRootDoesntExist = .false.
      return
    end if

    if (phase .eq. VAPPH) then
      if (dpdrho < 0 .or. prho < 0.999*prho_old .or. (prho-prho_old)/drho < 0.9999*dpdrho) then
        densityRootDoesntExist = .true.
      end if
    elseif (phase .eq. LIQPH) then
      if (dpdrho < 0 .or. prho > 1.001*prho_old .or. (prho-prho_old)/drho < 0.9999*dpdrho) then
        densityRootDoesntExist = .true.
      end if
    end if

    if (verbose) then
      if (phase .eq. LIQPH) then
        print *,"*******"
        print *, "Input phase", phase
        print *, "rho_old, rho",rho_old, rho_new
        print *, "prho_old, prho", prho_old,prho
        print *, "rho_old>rho_new?",rho_old>rho_new
        print *, "prho_old>prho?",prho_old>prho
        print *, "dpdrho>0?", dpdrho>0
        print *, "(prho-prho_old)/drho > 0.9999*dpdrho?", (prho-prho_old)/drho > 0.9999*dpdrho
        print *, "number of iterations: ", iter
        print *,"*******"
      else
        print *,"*******"
        print *, "Input phase", phase
        print *, "rho_old<rho_new?",rho_old<rho_new
        print *, "rho_old, rho",rho_old, rho_new
        print *, "prho_old, prho", prho_old,prho
        print *, "prho_old<prho?",prho_old<prho
        print *, "dpdrho>0?", dpdrho>0
        print *, "(prho-prho_old)/drho > 0.9999*dpdrho?", (prho-prho_old)/drho > 0.9999*dpdrho
        print *, "number of iterations: ", iter
        print *,"*******"
      end if
    end if
  end function densityRootDoesntExist

  !-----------------------------------------------------------------------------
  !> A Newton solver targeted at solving for density from a function P = P(rho).
  !> fun is a function handle computing pressure derivatives, having the form
  !>                fun(x, param, p, dpdrho, d2pdrho2)
  !> where the last three arguments are optional.
  !>
  !> The initial value of rho is the initial guess. It should be an overestimate
  !> for liquids, and an underestimate (e.g. 1e-6) for gas. Equals -1 if no
  !> solution exists for the given phase. A solution always exists if
  !> present(meta_extrem).
  !>
  !> \author Ailo A, November 2014
  !-----------------------------------------------------------------------------
  subroutine newton_density(fun,param,redT,p,rho_inout,phase_in,rho_releps,p_releps,meta_extrem,prho_init,dpdrho_init)
    implicit none
    !INPUT
    interface
      subroutine fun(rho, param, p, dpdrho, d2pdrho2) !< computes pressure and pressure derivatives
        implicit none
        real, intent(out) :: p
        real, intent(out), optional :: dpdrho
        real, intent(out), optional :: d2pdrho2
        real, intent(in) :: rho
        real, dimension(:), intent(in) :: param
      end subroutine fun
    end interface
    real, dimension(:), intent(in) :: param       !< temperature dependent parameters
    real, intent(in) :: redT                      !< reduced temperature
    real, intent(in) :: p                         !< pressure
    integer, intent(in) :: phase_in               !< the phase in which we desire a density.
    real, intent(in) :: rho_releps                !< relative tolerance for density
    real, intent(in) :: p_releps                  !< relative tolerance for pressure
    logical, intent(in), optional :: meta_extrem  !< metastable extremum
    real, intent(in), optional :: prho_init       !< if one happens to know the value of p when fun is evaluated at rho_inout
    real, intent(in), optional :: dpdrho_init     !< if one happens to know the value of dpdrho when fun is evaluated at rho_inout
    !INPUT/OUTPUT
    real, intent(inout) :: rho_inout              !< the computed density; comes in with the initial guess
    !LOCALS
    real :: p_dev                                                         !< (computed pressure at current rho) - (input pressure p)
    integer :: iter, maxiter=15                                           !< iteration counter
    real :: rho_old, prho_old, dpdrho_old, rho_new, prho_new, dpdrho_new  !< computed quantities in the preceding and the current step
    logical :: found_extrem                                               !< if present(meta_extrem), this shows if we found metastable extremum
    ! is prho and dpdrho given at the initial point?
    if (.not. (present(prho_init) .and. present(dpdrho_init))) then
      ! no; calculate them
      call fun(rho_inout, param, prho_new, dpdrho_new)
    else
      ! yes; store them
      prho_new = prho_init
      dpdrho_new = dpdrho_init
    end if

    ! Newton iteration
    rho_new = rho_inout ! The initial density.
    rho_old = 1.0       ! Just to ensure that we enter the Newton iteration.
    p_dev = 5e+10       ! Just to ensure that we enter the Newton iteration.
    iter = 0
    do while (abs(p_dev)/p > p_releps .or. abs(rho_old-rho_new)/rho_old > rho_releps)
      ! have we performed at least one iteration?
      if (iter > 0) then
        ! yes; has the search has wound up in the unstable two-phase part of the eos?
        if (densityRootDoesntExist(redT,phase_in, rho_old, rho_new, prho_old, prho_new, dpdrho_new, iter)) then ! only needs Delta_rho and Delta_prho?
          ! yes; then there doesn't exist a root in the given phase
          rho_new = -1
          exit
        end if
      end if

      ! store old values of rho, prho, dpdrho
      rho_old = rho_new
      prho_old = prho_new
      dpdrho_old = dpdrho_new

      ! perform a newton step to obtain rho_new
      p_dev = prho_new - P
      rho_new = rho_old - p_dev/dpdrho_old

      ! have we performed the max number of iterations, or is rho_new negative?
      iter = iter+1
      if (iter == maxiter .or. rho_new < 0) then
        ! yes; flag this by exiting loop with negative rho_new
        rho_new = -1
        exit
      end if

      ! calculate prho_new and dpdrho_new
      call fun(rho_new,param,prho_new,dpdrho_new)
    end do

    ! did the solver diverge, but we want a solution in this phase anyway?
    if (rho_new < 0 .and. present(meta_extrem)) then
      ! yes; then find the metastable extremum
      call find_extremum(MBWR_pressure,P,phase_in,param,found_extrem,rho_inout) ! here rho_inout is the initial guess
      if (.not. found_extrem) rho_new = -1 ! for some reason, this failed; signal this with a negative density
    end if

    ! assign the computed density
    rho_inout = rho_new
  end subroutine newton_density

  !> Interface to the density solver. Outputs density [mol/m^3].
  function MBWR_density(t,p,phase_in,param,model,phase_found_out,meta_extrem) result(MBWR_dens)
    use numconstants, only: machine_prec
    implicit none
    ! INPUT VARIABLES
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, intent(in) :: phase_in
    type(eosmbwr), intent(in) :: model
    real, dimension(:), intent(in) :: param  ! assumed to already be initialized
    logical, intent(in), optional :: meta_extrem
    ! OUTPUT VARIABLES
    integer, intent(out) :: phase_found_out
    real :: MBWR_dens ! [mol/m^3]
    ! LOCAL VARIABLES
    real, parameter :: rho_releps = 5000*machine_prec, p_releps = 1.0e-6
    real :: initialVaporDensity !< A lower bound for vapor density
    real, parameter :: InitialLiquidDensityScaleFactor = 1.35 ! Scale the SRK guess for liquid density by this factor. A magic number based on experiments.
    real :: rho_init, prho_init, dpdrho_init, d2pdrho2_init
    real :: redT
    redT = t/model%tc
    phase_found_out = phase_in

    ! in the case of extremely low pressures
    if (P < 100.0) then
      initialVaporDensity = 1.0e-12
    else
      initialVaporDensity = 1.0e-6
    endif

    if (phase_found_out .eq. LIQPH) then                                                        ! liquid
      rho_init = LiquidDensitySRKGuess(T,p,model)*InitialLiquidDensityScaleFactor           ! scale the guess, to get it on the right side of the actual liquid density
      call MBWR_pressure(rho_init, param, prho_init,dpdrho_init,d2pdrho2_init)              ! compute zeroth, first and second derivative
      if (.not. (present(meta_extrem)) .and. (d2pdrho2_init < 0 .or. dpdrho_init < 0)) then
        phase_found_out = 2                                                                ! the SRK guess doesn't have the right curvature; change to vapor phase
        if (verbose) print *,"LiquidDensitySRKGuess has wrong curvature", rho_init

      else if (present(meta_extrem) .and. redT < 1.005) then                                ! want the metastable extremum; have to make absolutely sure that the liquid guess is good
        if (prho_init < p .or. dpdrho_init < 0 .or. d2pdrho2_init < 0) then                ! do any of the derivatives have the wrong value?
          rho_init = 40                                                                   ! in that case, discard the SRK guess, and assume 40 moles/L at least gets us near the liquid region
          call MBWR_pressure(rho_init, param, prho_init, dpdrho_init)
          do while (prho_init < p .and. dpdrho_init > 0)                                  ! make sure we are on the right side of the root.
            rho_init = rho_init*1.3
            call MBWR_pressure(rho_init, param, prho_init, dpdrho_init)
          end do
          call MBWR_pressure(rho_init, param, prho_init,dpdrho_init)
          do while (prho_init > 10.0*p .or. dpdrho_init < 0)                              ! make sure we are not too far on the right side.
            rho_init = rho_init*0.7
            call MBWR_pressure(rho_init, param, prho_init, dpdrho_init)
          end do
          MBWR_dens = rho_init
          call newton_density(MBWR_pressure,param,redT,p,MBWR_dens,phase_found_out,rho_releps,p_releps, &
               prho_init=prho_init,dpdrho_init=dpdrho_init)
        end if

      else ! the normal case
        MBWR_dens = rho_init
        call newton_density(MBWR_pressure,param,redT,p,MBWR_dens,phase_found_out,rho_releps,p_releps, &
             prho_init=prho_init,dpdrho_init=dpdrho_init)
        if (verbose) print *,"LiquidDensitySRKGuess", rho_init
      end if
    end if

    if (phase_found_out .eq. VAPPH) then ! vapor
      MBWR_dens = initialVaporDensity
      call newton_density(MBWR_pressure,param,redT,p,MBWR_dens,phase_found_out,rho_releps,p_releps)
    end if

    if (MBWR_dens < 0) then ! didn't find a density root in the given phase_in
      phase_found_out = 3-phase_found_out ! try with the other phase
      if (phase_found_out .eq. LIQPH) then ! liquid
        rho_init = LiquidDensitySRKGuess(T,p,model)*InitialLiquidDensityScaleFactor
      else ! gas
        rho_init = initialVaporDensity
      end if
      MBWR_dens = rho_init
      call newton_density(MBWR_pressure,param,redT,p,MBWR_dens,phase_found_out,rho_releps,p_releps)
    end if

    if (MBWR_dens > 0) return

    ! fallback routine 1. The main routine will always find the vapor root if it exists, so it has to be a liquid root
    phase_found_out = 1
    rho_init = LiquidDensitySRKGuess(T,p+1e5,model)*InitialLiquidDensityScaleFactor*0.8 ! better success with less scaling and a higher pressure?
    MBWR_dens = barenewton(MBWR_pressure,param,P,rho_init,rho_releps,p_releps)
  end function MBWR_density

  ! Initial value finder for the density solver.
  ! This function operates on subcritical temperatures.
  real function LiquidDensitySRKGuess(T,p,model) RESULT (rho)
    real, intent(in) :: p,T
    type(eosmbwr), intent(in) :: model
    real :: a, bigA, bigB, r, q, u, discr, sqrt_discr, theta, phi, y1, y2, y3, Y ! SRK variables
    real :: redt, redp

    ! the ideal gas equation is a good approximation for supercritical temperatures
    redt = T/model%tc
    if (redt > 1.05) then ! at far supercritical temperatures, the ideal gas equation is accurate enough
      rho = P/(Rgas*T)
      return
    end if

    redp = P/model%pc
    if (redt > 0.95 .and. redp > 0.95 .and. redt < 1.01 .and. redp < 1.05) then ! special considerations near the critical point
      rho = model%rc*1.5 ! this is a particularly challenging region, so scale up the critical density. (It is scaled further in MBWR_density)
      return
    end if

    ! For subcritical temperatures, use the SRK equation.
    ! First calculate some characteristic SRK-parameters:
    a = a0_SRK*( 1+m_SRK*(1-sqrt(redt)) )**2
    bigA = a*p/(Rgas*T)**2
    bigB = b_SRK*p/(Rgas*T)

    ! Solve SRK for translated compressibility y = z-1/3
    r = (bigA-bigB-bigB**2)-1.e0/3.e0
    q = -2.e0/27.e0 + 1.e0/3.e0*(bigA-bigB-bigB**2)-bigA*bigB
    discr = (r/3)**3+(q/2)**2 ! The discriminant of the reduced cubic equation
    if (discr .ge. 1e-11 ) then                                                  !positive discriminant. 1 real ROOT
      sqrt_discr = sqrt(discr)
      u = sign(abs(-q/2+sqrt_discr)**(1.e0/3.e0),-q/2+sqrt_discr)
      if (abs(u)>1.0e-16) then
        Y = u-r/(3*u)  !!! Span's formula
      else
        rho = 0
        goto 666
      end if
    elseif (discr .ge. 0) then                                                   !zero discriminant. 3 real ROOTS, AT LEAST TWO OF THEM EQUAL
      sqrt_discr = sqrt(discr)
      u = min(-q/2,sign(abs(-q/2+sqrt_discr)**(1.e0/3.e0),-q/2+sqrt_discr))
      Y = -u
      if (Y < -1e0/3e0) Y = u-r/(3*u)
    else                                                                         !negative discriminant. 3 DISTINCT real ROOTS.
      theta = sqrt(-r**3/27)
      phi = acos(-q/(2*theta))
      y1 = cos(phi/3)
      y2 = cos((phi+2*PI)/3)
      y3 = cos((phi+4*PI)/3)
      Y = 2*(theta)**(1.e0/3.e0)*min(y1,y2,y3)
    end if
    rho = P/(Rgas*T*(Y+1.e0/3.e0))

666 if ( rho .le. 1.e-14 .or. rho .gt. 1e+6) then
      ! Something went wrong. Use ideal gas equation instead.
      ! The best fallback would be to use the cubic eos implemented in ThermoPack;
      ! but per now (Dec. 14) there is no way to have several eos initialized at the same time.
      if (verbose) then
        print *, "Breakdown of IVF"
        print *, "T,P :", T, P
        print *, "rho computed to = ",rho
        print *, "Using ideal gas estimate", P/(Rgas*T),"instead"
      end if
      rho = P/(Rgas*T)
    end if
  end function LiquidDensitySRKGuess


  !> Helper function for find_extremum
  logical function extremaSearchIsDiverging(phase, drho, prho_old, prho, dpdrho, P_in)
    implicit none
    real, intent(in) :: drho, prho_old,prho, dpdrho, P_in
    integer, intent(in) :: phase
    extremaSearchIsDiverging = .false.
    if (phase .eq. VAPPH) then
      if (prho < 0 .or. ((prho-prho_old)/(drho) < dpdrho .and. drho > 0) .or. prho > P_in) then
        extremaSearchIsDiverging = .true.
      end if
    else if (phase .eq. LIQPH) then
      if (prho < 0 .or. ((prho-prho_old)/(drho) > dpdrho .and. drho < 0) .or. prho < P_in) then
        extremaSearchIsDiverging = .true.
      end if
    end if
  end function extremaSearchIsDiverging

  !> Only works as a subroutine for newton_density, as the algorithm assumes that
  !> no root exists in the given phase.
  subroutine find_extremum(fun,P_in,phase,param,found_extrem,rho_extrem)
    use numconstants, only: machine_prec
    implicit none
    ! Input
    interface
      subroutine fun(rho, param, p, dpdrho, d2pdrho2) ! contents of param in order : gamma, rhoCoef
        implicit none
        real, intent(out) :: p
        real, intent(out), optional :: dpdrho
        real, intent(out), optional :: d2pdrho2
        real, intent(in) :: rho
        real, dimension(:), intent(in) :: param
      end subroutine fun
    end interface
    real, intent(in)                  :: P_in         !< Pressure [Pa]
    integer, intent(in)               :: phase        !< Desired phase
    real, dimension(:), intent(in)    :: param        !< The parameters fun depends on
    real, intent(inout)               :: rho_extrem   !< Comes in with the initial value, comes out with metastable density
    ! Output
    logical, intent(out)              :: found_extrem
    ! Locals
    real                              :: drho, prho = 0, dpdrho = 0, d2pdrho2, prho_old, dpdrho_old, s
    integer                           :: n_iter

    integer, parameter                :: max_iter_extr = 100
    real, parameter                   :: rho_extrem_rel_tol = machine_prec*1000.0
    real                              :: gradient_descent_drho, newton_max_drho
    integer, parameter                :: TREND_LIQ = 1, TREND_VAP = 2
    real                              :: rho, rho_init

    rho_init = rho_extrem ! rho_extremum should come in with the initial guess
    select case(phase)
    case(TREND_LIQ)
      gradient_descent_drho = rho_init/10
      newton_max_drho = rho_init/10
      s = 1.0 !Looking for minimum
    case(TREND_VAP)
      gradient_descent_drho = 3.0  ! A magic number...
      newton_max_drho = 3.0        ! A magic number...
      s = -1.0 !Looking for maximum
    end select

    rho = rho_init
    n_iter = 0
    do
      n_iter = n_iter+1

      ! store the old values of pressure and pressure-derivative
      prho_old = prho
      dpdrho_old = dpdrho

      ! compute the new values of pressures derivatives
      call fun(rho, param, prho, dpdrho, d2pdrho2)

      ! A check to see whether the iteration is diverging. Need to be put before calculating the new drho.
      if (n_iter > 1) then
        if (extremaSearchIsDiverging(phase, drho, prho_old, prho, dpdrho,P_in) .eqv. .true.) then
          ! backtrack
          drho = 0.5*drho
          rho = rho - drho
          ! reinstate old values of pressure and pressure derivative
          prho = prho_old
          dpdrho = dpdrho_old
          ! adjust the step limit
          gradient_descent_drho = gradient_descent_drho*0.5
          newton_max_drho = newton_max_drho*0.5
          cycle
        end if
      end if

      if ( ((phase==TREND_VAP) .and. d2pdrho2 < 0.0)  .or.  &  ! vapor
           ((phase==TREND_LIQ) .and. d2pdrho2 > 0.0)        &  ! liquid
           ) then
        ! Newton optimization
        drho = - dpdrho/d2pdrho2
        ! Limit the step
        if (abs(drho) > newton_max_drho) then
          drho = sign(newton_max_drho,drho)
        endif
      else
        ! Gradient descent, constant step.
        drho = -s*sign(1.0e+0,dpdrho)*gradient_descent_drho
      endif
      ! Update rho
      rho = rho + drho
      ! Check convergence of extremum search
      if (abs(drho/rho)<rho_extrem_rel_tol) then
        ! Converged
        found_extrem = .true. ! rho will have the correct value at exit.
        exit
      elseif ((n_iter == max_iter_extr) .or. & ! should never happen
           (phase==TREND_LIQ .and. rho < 0) &
           ) then
        ! Found no extremum
        found_extrem = .false.
        exit
      elseif (n_iter == 1 .and. & ! should never happen
           ((phase==TREND_LIQ .and. drho > 0) .or. (phase==TREND_VAP .and. drho < 0)) ) &
           then
        found_extrem = .false.
        exit
      elseif (phase==TREND_LIQ .and. prho<0 .and. dpdrho>0) then   !
        found_extrem = .true. ! This conditional should never occur, because the algorithm would already have found a true liquid root.
        exit
      endif
    end do
    rho_extrem = rho
  end subroutine find_extremum


  ! Calculates the critical parameters for the component, as according to the MBWR equation.
  ! This rarely coincides exactly with the critical parameters in the ThermoPack database.
  subroutine MBWR_criticalParameters(tc,pc,rc,model)
    implicit none
    !input/output
    real, intent(inout) :: tc, pc, rc
    type(eosmbwr), intent(in) :: model
    !locals
    real :: tc_low, tc_high, pc_low, pc_high, rc_low, rc_high
    real :: T, Tstep, rho, rhoStep
    real :: Tlower, Tupper, rhoLower, rhoUpper
    real :: p, p_old
    real :: paramenters(1 + model%bplen + model%belen)
    logical :: declineDetected, ascentDetected
    Tstep = 0.001
    rhoStep = 0.001

    ! INITIALIZE LOWER AND UPPER BOUNDS FOR RHO AND T
    Tlower = tc-0.7
    Tupper = tc+0.7
    rhoLower = rc-1.5
    rhoUpper = rc+1.5

    ! GET ESTIMATES BY STARTING FROM LOW VALUES OF RHO AND T
    T = Tlower    ! model%tc is the critical temperature in the ThermoPack database
    rho = rhoLower  ! model%pc is the critical pressure in the ThermoPack database
    Tloop: do
      T = T+Tstep
      call makeParam(paramenters,T,model)
      p_old = 0
      declineDetected = .false.
      rho = rhoLower
      rhoLoop: do
        rho = rho+rhoStep
        call MBWR_pressure(rho, paramenters,p)
        if (p<p_old) then
          rc_low = rho ! current lower estimate of critical density
          pc_low = p   ! current lower estimate of critical pressure
          tc_low = t   ! current lower estimate of critical temperature
          declineDetected = .true.
          exit rhoLoop
        else
          p_old = p
        end if
        if (rho > rhoUpper) then
          exit rhoLoop
        end if
      end do rhoLoop
      if (declineDetected .eqv. .false.) then
        exit
      end if
      if (T > Tupper) then
        call stoperror("MBWR critical parameters very different than the ones in database")
      end if
    end do Tloop

    ! GET ESTIMATES BY STARTING FROM HIGH VALUES OF RHO AND T
    T = Tupper
    rho = rhoUpper
    Tloop2: do
      T = T-Tstep
      call makeParam(paramenters,T,model)
      p_old = 1.0e12
      ascentDetected = .false.
      rho = rhoUpper
      rhoLoop2: do
        rho = rho-rhoStep
        call MBWR_pressure(rho, paramenters,p)
        if (p>p_old) then
          rc_high = rho ! current upper estimate of critical density
          pc_high = p   ! current upper estimate of critical pressure
          tc_high = t   ! current upper estimate of critical temperature
          ascentDetected = .true.
          exit Tloop2
        else
          p_old = p
        end if
        if (rho < rhoLower) then
          exit rhoLoop2
        end if
      end do rhoLoop2

      if (T < Tlower) then
        call stoperror("MBWR critical parameters very different than the ones in database")
      end if
   end do Tloop2

    ! Estimate the critical parameters by the average of the lower and upper estimates
    rc = (rc_low+rc_high)/2
    tc = (tc_low+tc_high)/2
    pc = (pc_low+pc_high)/2
  end subroutine MBWR_criticalParameters

  real function barenewton(fun,param,P_in,x,x_releps,f_releps)
    implicit none
    !input
    interface
      SUBROUTINE fun(rho, param, p, dpdrho, d2pdrho2) ! contents of param in order : gamma, rhoCoef
        IMPLICIT NONE
        real, intent(in) :: rho
        real, dimension(:), intent(in) :: param
        real, intent(out) :: p
        real, intent(out), optional :: dpdrho
        real, intent(out), optional :: d2pdrho2
      end SUBROUTINE fun
    end interface
    real, dimension(:), intent(in) :: param
    real, intent(in) :: P_in
    real, intent(in) :: x
    real, intent(in) :: x_releps, f_releps

    !locals
    real :: xold, xnew
    integer :: iter, maxiter=20
    real :: prho, dpdrho, pdev

    pdev = 5e+10
    xnew = x
    iter = 0
    xold = 1.0
    do while (abs(pdev)/P_in > f_releps .or. abs(xold-xnew)/xold > x_releps)
      xold = xnew
      call fun(xold, param, prho, dpdrho)
      pdev = prho-P_in
      xnew = xold - pdev/dpdrho
      iter = iter+1
      !      print *, iter, xnew, prho, dpdrho
      if (dpdrho < 0 .or. iter > maxiter .or. dpdrho < 0 .or. xnew < 0) then
        barenewton = -1
        return
      end if
    end do
    barenewton = xnew
  end function barenewton

END MODULE tpmbwr
