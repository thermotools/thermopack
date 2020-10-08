!> Interface handling both fluid and solid equation of state
!!
!! \author MH, 2016-06.
module vls
  use thermopack_constants
  use thermopack_var, only: nc, nph
  use solideos
  use eos
  !
  implicit none
  save
  !
  !> Thermo state, used for debugging
  type state
    integer :: nd
    real :: t,p
    real, allocatable :: Z(:),BETA(:),XX(:,:)
    integer, allocatable :: phaseVec(:)
  contains
    procedure :: set_state
    procedure :: get_state
    procedure :: get_state_no_z
    procedure :: print_state
  end type state
  !
  private
  public :: vlsThermo, vlsSpecificVolume, vlsEnthalpy, vlsEntropy
  public :: mpSpecificVolume, mpEnthalpy, mpEntropy
  public :: specificEnthalpyVLWS, specificVolumeVLWS, specificEntropyVLWS
  public :: inversePhaseMappingVLWS
  public :: printCurrentPhases
  public :: state
  !
contains

  !----------------------------------------------------------------------
  !> Calculate fugasity coefficient and differentials given composition,
  !> temperature and pressure. Interface for vapour, liquid and solid
  !>
  !> \author MH, 2016-06
  !----------------------------------------------------------------------
  subroutine vlsThermo(t,p,z,phase,lnfug,lnfugt,lnfugp,lnfugx,ophase,metaExtremum)
    use eos, only: thermo
    use solideos, only: solid_thermo
    use thermopack_constants, only: SOLIDPH
    ! Transferred variables
    integer, intent(in)                               :: phase !< Phase identifyer
    integer, optional, intent(out)                    :: ophase !< Phase identifyer for MINGIBBSPH
    real, intent(in)                                  :: t !< K - Temperature
    real, intent(in)                                  :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in)                 :: z !< Compozition
    real, dimension(1:nc), intent(out)                :: lnfug !< Logarithm of fugasity coefficient
    real, optional, dimension(1:nc), intent(out)      :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
    real, optional, dimension(1:nc), intent(out)      :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
    real, optional, dimension(1:nc,1:nc), intent(out) :: lnfugx !< Logarithm of fugasity coefficient differential wrpt. mole numbers
    logical, optional, intent(in)                     :: metaExtremum
    ! Locals
    integer :: i
    integer, dimension(1) :: imax
    real :: lnfugs,lnfugts,lnfugps

    if (phase == SOLIDPH) then
      imax = maxloc(z)
      i = imax(1)
      call solid_thermo(t,p,z,lnfugs,lnfugts,lnfugps)
      lnfug = 0.0
      lnfug(i) = lnfugs
      if (present(lnfugt)) then
        lnfugt = 0.0
        lnfugt(i) = lnfugts
      endif
      if (present(lnfugp)) then
        lnfugp = 0.0
        lnfugp(i) = lnfugps
      endif
      if (present(lnfugx)) then
        lnfugx = 0.0
      endif
      if (present(ophase)) then
        ophase = SOLIDPH
      endif
    else
      call thermo(t,p,z,phase,lnfug,lnfugt,lnfugp,lnfugx,ophase,metaExtremum)
    endif

  end subroutine vlsThermo

  !----------------------------------------------------------------------
  !> Calculate single-phase specific volume given composition, temperature and
  !> pressure for fluid and solid phases
  !>
  !> \author MH, 2016-06
  !----------------------------------------------------------------------
  subroutine vlsSpecificVolume(t,p,z,phase,v,dvdt,dvdp,dvdx)
    use eos, only: specificVolume
    use solideos, only: solid_specificVolume
    use thermopack_constants, only: SOLIDPH
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: v !< m3/mol - Specific volume
    real, optional, intent(out) :: dvdt !< m3/mol/K - Specific volume differential wrpt. temperature
    real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specific volume differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dvdx !< m3/mol - Specific volume differential wrpt. mole numbers
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    if (phase == SOLIDPH) then
      call solid_specificVolume(t,p,z,v,dvdt,dvdp)
      if (present(dvdx)) then
        imax = maxloc(z)
        i = imax(1)
        dvdx = 0.0
        dvdx(i) = v
      endif
    else
      call specificVolume(t,p,z,phase,v,dvdt,dvdp,dvdx)
    endif

  end subroutine vlsSpecificVolume

  !----------------------------------------------------------------------
  !> Calculate single-phase specific enthalpy given composition, temperature and
  !> pressure for fluid and solid phases
  !>
  !> \author MH, 2016-06
  !----------------------------------------------------------------------
  subroutine vlsEnthalpy(t,p,z,phase,h,dhdt,dhdp,dhdx)
    use eos, only: enthalpy
    use solideos, only: solid_enthalpy
    use thermopack_constants, only: SOLIDPH
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: h !< J/mol - Specific enthalpy
    real, optional, intent(out) :: dhdt !< J/mol/K - Specific enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdp !< J/mol/Pa - Specific enthalpy differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dhdx !< J/mol - Specific enthalpy differential wrpt. mole numbers
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    if (phase == SOLIDPH) then
      call solid_enthalpy(t,p,z,h,dhdt,dhdp)
      if (present(dhdx)) then
        imax = maxloc(z)
        i = imax(1)
        dhdx = 0.0
        dhdx(i) = h
      endif
    else
      call enthalpy(t,p,z,phase,h,dhdt,dhdp,dhdx)
    endif

  end subroutine vlsEnthalpy

  !----------------------------------------------------------------------
  !> Calculate single-phase specific entropy given composition, temperature and
  !> pressure for fluid and solid phases
  !>
  !> \author MH, 2016-06
  !----------------------------------------------------------------------
  subroutine vlsEntropy(t,p,z,phase,s,dsdt,dsdp,dsdx)
    use eos, only: entropy
    use solideos, only: solid_entropy
    use thermopack_constants, only: SOLIDPH
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: s !< J/mol/K - Specific entropy
    real, optional, intent(out) :: dsdt !< J/mol/K^2 - Specific entropy differential wrpt. temperature
    real, optional, intent(out) :: dsdp !< J/mol/K/Pa - Specific entropy differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dsdx !< J/mol/K - Specific enthalpy differential wrpt. mole numbers
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    if (phase == SOLIDPH) then
      call solid_entropy(t,p,z,s,dsdt,dsdp)
      if (present(dsdx)) then
        imax = maxloc(z)
        i = imax(1)
        dsdx = 0.0
        dsdx(i) = s
      endif
    else
      call entropy(t,p,z,phase,s,dsdt,dsdp,dsdx)
    endif

  end subroutine vlsEntropy

  !--------------------------------------------------------------------------
  !> Calculate multi-phase entropy given composition, temperature and pressure
  !> Unit: J/mol/K
  !>
  !> \author MH, 2013-02
  !--------------------------------------------------------------------------
  function mpEntropy(nd,t,p,beta,xx,phase) result(s)
    use eos, only: entropy
    use thermopack_constants, only: SOLIDPH
    use solideos, only: solid_entropy
    implicit none
    ! Transferred variables
    integer, intent(in) :: nd !< Numper of phases
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    integer, dimension(nph)    :: phase !< Phase identifyer
    real, intent(in), dimension(nph)    :: beta !< Phase fractions
    real, intent(in), dimension(nph,nc) :: xx !< Composition
    real :: s !< J/mol/K - Specific entropy
    ! Locals
    integer :: i
    real :: hph
    s = 0.0
    do i=1,nd
      if (phase(i) == SOLIDPH) then
        call solid_entropy(t,p,xx(i,:),hph)
      else
        call entropy(t,p,xx(i,:),phase(i),hph)
      endif
      s = s + beta(i)*hph
    enddo

  end function mpEntropy

  !--------------------------------------------------------------------------
  !> Calculate multi-phase enthalpy given composition, temperature and pressure
  !> Unit: J/mol
  !>
  !> \author MH, 2013-02
  !--------------------------------------------------------------------------
  function mpEnthalpy(nd,t,p,beta,xx,phase) result(h)
    use eos, only: enthalpy
    use thermopack_constants, only: SOLIDPH
    use solideos, only: solid_enthalpy
    implicit none
    ! Transferred variables
    integer, intent(in) :: nd !< Numper of phases
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    integer, dimension(nph)    :: phase !< Phase identifyer
    real, intent(in), dimension(nph)    :: beta !< Phase fractions
    real, intent(in), dimension(nph,nc) :: xx !< Composition
    real :: h !< J/mol - Specifc enthalpy
    ! Locals
    integer :: i
    real :: hph
    h = 0.0
    do i=1,nd
      if (phase(i) == SOLIDPH) then
        call solid_enthalpy(t,p,xx(i,:),hph)
      else
        call enthalpy(t,p,xx(i,:),phase(i),hph)
      endif
      h = h + beta(i)*hph
    enddo

  end function mpEnthalpy

  !--------------------------------------------------------------------------
  !> Calculate multi-phase entropy given composition, temperature and pressure
  !> Unit: J/mol/K
  !>
  !> \author MH, 2013-02
  !--------------------------------------------------------------------------
  function mpSpecificVolume(nd,t,p,beta,xx,phase) result(v)
    use eos, only: specificVolume
    use thermopack_constants, only: SOLIDPH
    use solideos, only: solid_specificVolume
    implicit none
    ! Transferred variables
    integer, intent(in) :: nd !< Numper of phases
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    integer, dimension(nph)    :: phase !< Phase identifyer
    real, intent(in), dimension(nph)    :: beta !< Phase fractions
    real, intent(in), dimension(nph,nc) :: xx !< Composition
    real :: v !< m3/mol - Specifc volume
    ! Locals
    integer :: i
    real :: vph
    v = 0.0
    do i=1,nd
      if (phase(i) == SOLIDPH) then
        call solid_specificVolume(t,p,xx(i,:),vph)
      else
        call specificVolume(t,p,xx(i,:),phase(i),vph)
      endif
      v = v + beta(i)*vph
    enddo

  end function mpSpecificVolume

  subroutine inversePhaseMappingVLWS(z,betaGas,Y,gasPresent,&
       betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
       betaSolid,Ws,solidPresent,nd,BETA,XX,phaseVec)
    use numconstants, only: machine_prec
    ! Input:
    real, dimension(nc) :: z
    logical, intent(in) :: gasPresent, liquidPresent
    logical, intent(in) :: waterPresent, solidPresent
    real, intent(in) :: betaGas, betaLiquid, betaWater, betaSolid
    real, intent(in) :: X(nc), Y(nc), W(nc), Ws(nc)
    ! Output:
    integer, intent(out) :: nd !< Number of stabel phases found [-]
    real, dimension(nph), intent(out) :: BETA !< Phase molar fractions [mol/mol]
    real, dimension(nph,nc), intent(out) :: XX !< Phase molar compozition [mol/mol]
    integer, dimension(nph), intent(out) :: phaseVec !< Phase identifier. Not to be trused [-]
    ! Locals:
    integer :: i

    nd = 0
    !ns = 0
    if (gasPresent) then
      ! Add gas phase
      nd = nd + 1
      BETA(nd) = betaGas
      if (BETA(nd) <= 0.0) then
        call stoperror("multiPhaseFlashUV: Gas phase flagged as present, but not actually present")
      endif
      XX(nd,:) = Y
      phaseVec(nd) = VAPPH
    endif
    if (liquidPresent) then
      ! Add liquid phase
      nd = nd + 1
      BETA(nd) = betaLiquid
      if (BETA(nd) <= 0.0) then
        call stoperror("multiPhaseFlashUV: Liquid phase flagged as present, but not actually present")
      endif
      XX(nd,:) = X
      phaseVec(nd) = LIQPH
    endif
    if (waterPresent) then
      ! Add water phase
      nd = nd + 1
      BETA(nd) = betaWater
      if (BETA(nd) <= 0.0) then
        call stoperror("multiPhaseFlashUV: Water phase flagged as present, but not actually present")
      endif
      XX(nd,:) = W
      phaseVec(nd) = LIQPH
    endif
    if (solidPresent) then
      ! Add solid phase
      nd = nd + 1
      !ns = ns + 1
      BETA(nd) = betaSolid
      if (BETA(nd) <= 0.0) then
        call stoperror("multiPhaseFlashUV: Solid phase flagged as present, but not actually present")
      endif
      XX(nd,:) = Ws
      phaseVec(nd) = SOLIDPH
    endif
    ! Test input data for consistency
    if (abs(sum(BETA(1:nd)) - 1.0) > machine_prec*100.0) then
      call printCurrentPhases(nd,0.0,0.0,Z,XX,BETA,phaseVec)
      call stoperror("inversePhaseMappingVLWS: Phase fractions don't sum to one.")
    endif
    do i=1,nc
      if (abs(sum(BETA(1:nd)*XX(1:nd,i)) - Z(i)) > Z(i)*machine_prec*1.0e6) then
        call printCurrentPhases(nd,0.0,0.0,Z,XX,BETA,phaseVec)
        call stoperror("inversePhaseMappingVLWS: Mass balance not met.")
      endif
    enddo
  end subroutine inversePhaseMappingVLWS

  !------------------------------------------------------------------------
  !>  Get the specific enthalpy from VLWS variables.
  !>  Wrapper for mpEnthalpy
  !>
  !>  \author HLS, 2018-08
  !-------------------------------------------------------------------------
  subroutine specificEnthalpyVLWS(nc,nph,T,P,z,betaGas,Y,gasPresent,&
       betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
       betaSolid,Ws,solidPresent,h)
    implicit none
    ! Input:
    integer,                intent(in)    :: nc       !< Number of components
    integer,                intent(in)    :: nph      !< Number of possible phases
    real,                   intent(in)    :: T        !< Temperature [K]
    real,                   intent(in)    :: P        !< Pressure [Pa]
    real, dimension(nc),    intent(in)    :: z        !< Overall molar compozition [-]
    real,                   intent(in)    :: betaGas       !< Gas phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: Y             !< Gas phase molar composition [-]
    logical,                intent(in)    :: gasPresent    !< Is gas phase detected?
    real,                   intent(in)    :: betaLiquid    !< Liquid phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: X             !< Liquid phase molar composition [-]
    logical,                intent(in)    :: liquidPresent !< Is liquid phase detected?
    real,                   intent(in)    :: betaWater     !< Water phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: W             !< Water phase molar composition [-]
    logical,                intent(in)    :: waterPresent  !< Is water phase detected?
    real,                   intent(in)    :: betaSolid     !< Solid phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: Ws            !< Solid phase molar composition [-]
    logical,                intent(in)    :: solidPresent  !< Is solid phase detected?
    ! Output:
    real,                   intent(out)   :: h             !< Enthalpy [J/mol]
    ! Locals:
    real, dimension(nph)                  :: beta     !< Phase molar fractions [mol/mol]
    real, dimension(nph,nc)               :: xx       !< Phase molar composition [mol/mol]
    integer*4, dimension(nph)             :: phaseVec !< Phase identifier [-]
    integer                               :: nd       !< Number of phases [-]


    call inversePhaseMappingVLWS(z,betaGas,Y,gasPresent,&
         betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
         betaSolid,Ws,solidPresent,nd,beta,xx,phasevec)
    h = mpEnthalpy(nd,t,p,beta,xx,phasevec)

  end subroutine specificEnthalpyVLWS

  !------------------------------------------------------------------------
  !>  Get the specific enthalpy from VLWS variables.
  !>  Wrapper for mpSpecificVolume
  !>
  !>  \author HLS, 2018-08
  !-------------------------------------------------------------------------
  subroutine specificVolumeVLWS(nc,nph,T,P,z,betaGas,Y,gasPresent,&
       betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
       betaSolid,Ws,solidPresent,v)
    implicit none
    ! Input:
    integer,                intent(in)    :: nc       !< Number of components
    integer,                intent(in)    :: nph      !< Number of possible phases
    real,                   intent(in)    :: T        !< Temperature [K]
    real,                   intent(in)    :: P        !< Pressure [Pa]
    real, dimension(nc),    intent(in)    :: z        !< Overall molar compozition [-]
    real,                   intent(in)    :: betaGas       !< Gas phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: Y             !< Gas phase molar composition [-]
    logical,                intent(in)    :: gasPresent    !< Is gas phase detected?
    real,                   intent(in)    :: betaLiquid    !< Liquid phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: X             !< Liquid phase molar composition [-]
    logical,                intent(in)    :: liquidPresent !< Is liquid phase detected?
    real,                   intent(in)    :: betaWater     !< Water phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: W             !< Water phase molar composition [-]
    logical,                intent(in)    :: waterPresent  !< Is water phase detected?
    real,                   intent(in)    :: betaSolid     !< Solid phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: Ws            !< Solid phase molar composition [-]
    logical,                intent(in)    :: solidPresent  !< Is solid phase detected?
    ! Output:
    real,                   intent(out)   :: v             !< Specific volume [m^3/mol]
    ! Locals:
    real, dimension(nph)                  :: beta     !< Phase molar fractions [mol/mol]
    real, dimension(nph,nc)               :: xx       !< Phase molar composition [mol/mol]
    integer*4, dimension(nph)             :: phaseVec !< Phase identifier [-]
    integer                               :: nd       !< Number of phases [-]


    call inversePhaseMappingVLWS(z,betaGas,Y,gasPresent,&
         betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
         betaSolid,Ws,solidPresent,nd,beta,xx,phasevec)
    v = mpSpecificVolume(nd,t,p,beta,xx,phasevec)

  end subroutine specificVolumeVLWS

  !------------------------------------------------------------------------
  !>  Get the entropy from VLWS variables.
  !>  Wrapper for mpEntropy
  !>
  !>  \author HLS, 2018-08
  !-------------------------------------------------------------------------
  subroutine specificEntropyVLWS(nc,nph,T,P,z,betaGas,Y,gasPresent,&
       betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
       betaSolid,Ws,solidPresent,s)
    implicit none
    ! Input:
    integer,                intent(in)    :: nc       !< Number of components
    integer,                intent(in)    :: nph      !< Number of possible phases
    real,                   intent(in)    :: T        !< Temperature [K]
    real,                   intent(in)    :: P        !< Pressure [Pa]
    real, dimension(nc),    intent(in)    :: z        !< Overall molar compozition [-]
    real,                   intent(in)    :: betaGas       !< Gas phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: Y             !< Gas phase molar composition [-]
    logical,                intent(in)    :: gasPresent    !< Is gas phase detected?
    real,                   intent(in)    :: betaLiquid    !< Liquid phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: X             !< Liquid phase molar composition [-]
    logical,                intent(in)    :: liquidPresent !< Is liquid phase detected?
    real,                   intent(in)    :: betaWater     !< Water phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: W             !< Water phase molar composition [-]
    logical,                intent(in)    :: waterPresent  !< Is water phase detected?
    real,                   intent(in)    :: betaSolid     !< Solid phase molar fraction [-]
    real, dimension(nc),    intent(in)    :: Ws            !< Solid phase molar composition [-]
    logical,                intent(in)    :: solidPresent  !< Is solid phase detected?
    ! Output:
    real,                   intent(out)   :: s             !< Specific volume [J/(mol K)]
    ! Locals:
    real, dimension(nph)                  :: beta     !< Phase molar fractions [mol/mol]
    real, dimension(nph,nc)               :: xx       !< Phase molar composition [mol/mol]
    integer*4, dimension(nph)             :: phaseVec !< Phase identifier [-]
    integer                               :: nd       !< Number of phases [-]


    call inversePhaseMappingVLWS(z,betaGas,Y,gasPresent,&
         betaLiquid,X,liquidPresent,betaWater,W,waterPresent,&
         betaSolid,Ws,solidPresent,nd,beta,xx,phasevec)
    s = mpEntropy(nd,t,p,beta,xx,phasevec)

  end subroutine specificEntropyVLWS

  !-------------------------------------------------------------------------
  !> Print info about current phases
  !> Used for debugging
  !> \author MH, 2018-04
  !-------------------------------------------------------------------------
  subroutine printCurrentPhases(nd,T,P,Z,XX,BETA,phaseVec)
    implicit none
    real, intent(in) :: T,P
    real, dimension(nc), intent(in) :: Z
    real, dimension(nph,nc), intent(in) :: XX
    real, dimension(nph), intent(in) :: BETA
    integer, dimension(nph), intent(in) :: phaseVec
    integer, intent(in) :: nd !< Number of phases
    ! Locals
    character(len=10) :: phaseName
    integer :: i,j
    real :: lnFug(nc)

    print *,''
    write(*,'(A,es25.16e3)') 'Current temperature: ',T
    write(*,'(A,es25.16e3)') 'Current pressure:',P
    write(*,'(A)') 'Overall composition:'
    do i=1,nc
      write(*,'(es25.16e3)') Z(i)
    enddo
    print *,'Current phases:'
    do i=1,nd
      call phaseIntToName(phaseVec(i),phaseName)
      print *,'Phase: ', trim(phaseName)
      print *,'beta: ',BETA(i)
      print *,'Mole weight: ',moleWeight(XX(i,:))
      print *,'X: ',XX(i,:)
      if (T > 0.0 .and. P > 0.0) then
        call vlsThermo(T,P,XX(i,:),phaseVec(i),lnFug)
        do j=1,nc
          if (XX(i,j) > 0.0) then
            lnFug(j) = lnFug(j) + log(XX(i,j))
          else
            lnFug(j) = 0.0
          endif
        enddo
        print *,'lnFug + ln(X): ',lnFug
      endif
    enddo
    print *,''
  end subroutine printCurrentPhases

  subroutine set_state(st,nd,t,p,Z,BETA,XX,phaseVec)
    class(state), intent(inout) :: st
    integer, intent(in) :: nd !< Number of stabel phases found [-]
    real, dimension(:), intent(in) :: BETA !< Phase molar fractions [mol/mol]
    real, dimension(:,:), intent(in) :: XX !< Phase molar compozition [mol/mol]
    real, dimension(:), intent(in) :: Z !< Overall molar compozition [mol/mol]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Specified pressure [Pa]
    integer, dimension(:), intent(in) :: phaseVec !< Phase identifier. Not to be trused [-]
    ! Locals
    integer :: n, np, istat
    logical :: do_alloc
    n = size(z)
    np = size(BETA)
    do_alloc = .false.
    if (allocated(st%z)) then
      if (size(st%z) /= n) then
        do_alloc = .true.
        deallocate(st%z,st%BETA,st%XX,st%phaseVec,stat=istat)
        if (istat /= 0) call stoperror("Nota able to allocate state variables")
      endif
    endif
    if (do_alloc) then
      allocate(st%z(n),st%BETA(np),st%XX(np,n),st%phaseVec(np),stat=istat)
      if (istat /= 0) call stoperror("Nota able to allocate state variables")
    endif
    st%nd = nd
    st%t = t
    st%p = p
    st%Z = Z
    st%BETA = BETA
    st%XX = XX
    st%phaseVec = phaseVec
  end subroutine set_state

  subroutine get_state(st,nd,t,p,Z,BETA,XX,phaseVec)
    class(state), intent(in) :: st
    integer, intent(out) :: nd !< Number of stabel phases found [-]
    real, dimension(:), intent(out) :: BETA !< Phase molar fractions [mol/mol]
    real, dimension(:,:), intent(out) :: XX !< Phase molar compozition [mol/mol]
    real, dimension(:), intent(out) :: Z !< Overall molar compozition [mol/mol]
    real, intent(out) :: t !< Temperature [K]
    real, intent(out) :: p !< Specified pressure [Pa]
    integer, dimension(:), intent(out) :: phaseVec !< Phase identifier. Not to be trused [-]
    ! Locals
    integer :: n, np
    n = size(z)
    np = size(BETA)
    if (allocated(st%z)) then
      if (size(st%z) /= n) then
        call stoperror("State not initialized")
      endif
    endif
    nd = st%nd
    t = st%t
    p = st%p
    Z = st%Z
    BETA = st%BETA
    XX = st%XX
    phaseVec = st%phaseVec
  end subroutine get_state

  subroutine get_state_no_z(st,nd,t,p,BETA,XX,phaseVec)
    class(state), intent(in) :: st
    integer, intent(out) :: nd !< Number of stabel phases found [-]
    real, dimension(:), intent(out) :: BETA !< Phase molar fractions [mol/mol]
    real, dimension(:,:), intent(out) :: XX !< Phase molar compozition [mol/mol]
    real, intent(out) :: t !< Temperature [K]
    real, intent(out) :: p !< Specified pressure [Pa]
    integer, dimension(:), intent(out) :: phaseVec !< Phase identifier. Not to be trused [-]
    ! Locals
    real, dimension(size(XX,dim=2)) :: Z !< Overall molar compozition [mol/mol]
    call st%get_state(nd,t,p,Z,BETA,XX,phaseVec)
  end subroutine get_state_no_z

  subroutine print_state(st)
    class(state), intent(in) :: st
    call printCurrentPhases(st%nd,st%T,st%P,st%Z,st%XX,st%BETA,st%phaseVec)
  end subroutine print_state

end module vls
