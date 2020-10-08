!-----------------------------------------------------------------------------
!> Calculate jacobian for Michelsen state matrices.
!>
!> \todo Need trace-component functionallity.
!>
!-----------------------------------------------------------------------------
module state_functions
  !
  !
  use thermopack_var, only: nc
  use thermopack_constants
  use eos
  use numconstants, only: almost_zero, machine_prec
  implicit none
  private
  save

  public :: getStateFuncMatrix, dhdt_twoPhase, dvdp_twoPhase, dvdt_twoPhase
  public :: getStateFunc, dhdp_twoPhase, dnvdX, getSVDerivativesTwoPhase
  public :: getUVDerivativesTwoPhase
  public :: getJouleThompsonCoeff, dpdt_twoPhase

contains

  !-----------------------------------------------------------------------------
  !> Calculate jacobian matrix for Michelsen state functions
  !> Implemented for two phase PH, PS, PT and UV flash
  !> Included TV and PV for convenience.
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  subroutine getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,spec,vspec,M,RHS,phase,simpleMatrix)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    character(len=2), intent(in) :: spec !< Character specification
    real, dimension(2), intent(in) :: vspec !< Specification value
    real, dimension(:,:), intent(out) :: M !< Jacobian matrix
    real, dimension(:), intent(out) :: RHS !< Right hand side
    integer, dimension(2), optional, intent(in) :: phase !< Phase integer
    logical, optional, intent(in) :: simpleMatrix !< Deactivate composition diff.
    ! Locals
    real, dimension(nc) :: FUGL,FUGV,FUGLT,FUGVT,FUGLP,FUGVP
    real, dimension(nc,nc) :: FUGLX,FUGVX
    real :: hv,hl,dhvdt,dhldt,cpmix,dhdp,dhvdp,dhldp
    real :: sv,sl,dsldt,dsvdt,smix,hmix,vv,vl,vmix,dvdt,dvdp
    real :: dvldp,dvldt,dvvdp,dvvdt
    real :: denom
    integer :: i, phaseX, phaseY
    logical :: deactivateCompDiff

    phaseX = LIQPH
    phaseY = VAPPH
    if (present(phase)) then
      phaseX = phase(1)
      phaseY = phase(2)
    endif

    if (spec == 'TP' .OR. spec == 'PT') then
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX) ! ln phi vapour
    else if (spec == 'SP' .OR. spec == 'PS' .OR. spec == 'HP' .OR. spec == 'PH') then
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX,lnfugt=FUGLT) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX,lnfugt=FUGVT) ! ln phi vapour
      if (spec == 'SP' .OR. spec == 'PS') then
        call entropy(t,p,X,phaseX,sl,dsdt=dsldt)
        call entropy(t,p,Y,phaseY,sv,dsdt=dsvdt)
        smix = betaV*sv + betaL*sl
        RHS(1+nc) = (vspec(1) - smix)/Rgas
        cpmix = T*(betaV*dsvdt + betaL*dsldt)
      else if (spec == 'HP' .OR. spec == 'PH') then
        call enthalpy(t,p,X,phaseX,hl,dhdt=dhldt)
        call enthalpy(t,p,Y,phaseY,hv,dhdt=dhvdt)
        hmix = betaV*hv + betaL*hl
        RHS(1+nc) = (vspec(1) - hmix)/(Rgas*T)
        cpmix = betaV*dhvdt + betaL*dhldt
      endif
      M(1+nc,1+nc) = -cpmix/Rgas
      M(1:nc,1+nc) = T*(FUGVT - FUGLT)
      M(1+nc,1:nc) = T*(FUGVT - FUGLT)
    else if (spec == 'UV') then
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX,lnfugt=FUGLT,lnfugp=FUGLP) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX,lnfugt=FUGVT,lnfugp=FUGVP) ! ln phi vapour

      ! Temperature terms
      call enthalpy(t,p,X,phaseX,hl,dhdt=dhldt)
      call enthalpy(t,p,Y,phaseY,hv,dhdt=dhvdt)
      hmix = betaV*hv + betaL*hl
      RHS(1+nc) = (vspec(1) + P*vspec(2) - hmix)/(Rgas*T)
      cpmix = betaV*dhvdt + betaL*dhldt
      M(1+nc,1+nc) = -cpmix/Rgas
      M(1:nc,1+nc) = T*(FUGVT - FUGLT)
      M(1+nc,1:nc) = T*(FUGVT - FUGLT)

      ! Pressure terms
      call specificVolume(t,p,X,phaseX,vl,dvdt=dvldt,dvdp=dvldp)
      call specificVolume(t,p,Y,phaseY,vv,dvdt=dvvdt,dvdp=dvvdp)
      vmix = betaV*vv + betaL*vl
      RHS(2+nc) = P*(vmix-vspec(2))/(Rgas*T)
      dVdp = betaV*dvvdp + betaL*dvldp
      dVdt = betaV*dvvdt + betaL*dvldt
      M(2+nc,2+nc) = p*p*dVdp/(Rgas*t)
      M(1+nc,2+nc) = p*dVdt/Rgas
      M(2+nc,1+nc) = p*dVdt/Rgas
      M(1:nc,2+nc) = p*(FUGVP - FUGLP)
      M(2+nc,1:nc) = p*(FUGVP - FUGLP)
    else if (spec == 'TV' .OR. spec == 'VT') then
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX,lnfugp=FUGLP) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX,lnfugp=FUGVP) ! ln phi vapour
      call specificVolume(t,p,X,phaseX,vl,dvdp=dvldp)
      call specificVolume(t,p,Y,phaseY,vv,dvdp=dvvdp)
      dVdp = betaV*dvvdp + betaL*dvldp
      M(1+nc,1+nc) = p*p*dVdp/(Rgas*t)
      M(1:nc,1+nc) = p*(FUGVP - FUGLP)
      M(1+nc,1:nc) = p*(FUGVP - FUGLP)
      vmix = betaV*vv + betaL*vl
      RHS(1+nc) = P*(vmix-vspec(1))/(Rgas*T)
    else if (spec == 'PV' .OR. spec == 'VP') then
      ! NOT SYMETRIC!
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX,lnfugt=FUGLT,lnfugp=FUGLP) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX,lnfugt=FUGVT,lnfugp=FUGVP) ! ln phi vapour
      call specificVolume(t,p,X,phaseX,vl,dvdt=dvldt)
      call specificVolume(t,p,Y,phaseY,vv,dvdt=dvvdt)
      dVdt = betaV*dvvdt + betaL*dvldt
      M(1+nc,1+nc) = p*dVdt/Rgas
      M(1:nc,1+nc) = T*(FUGVT - FUGLT)
      M(1+nc,1:nc) = p*(FUGVP - FUGLP)
      vmix = betaV*vv + betaL*vl
      RHS(1+nc) = P*(vmix-vspec(1))/(Rgas*T)

    else if (spec == 'SV') then
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX,lnfugt=FUGLT,lnfugp=FUGLP) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX,lnfugt=FUGVT,lnfugp=FUGVP) ! ln phi vapour

      ! Entropy terms
      call entropy(t,p,X,phaseX,sl,dsdt=dsldt)
      call entropy(t,p,Y,phaseY,sv,dsdt=dsvdt)
      smix = betaV*sv + betaL*sl
      RHS(1+nc) = (vspec(1) - smix)/Rgas
      cpmix = T*(betaV*dsvdt + betaL*dsldt)
      M(1+nc,1+nc) = -cpmix/Rgas
      M(1:nc,1+nc) = T*(FUGVT - FUGLT)
      M(1+nc,1:nc) = T*(FUGVT - FUGLT)

      ! Pressure terms
      call specificVolume(t,p,X,phaseX,vl,dvdt=dvldt,dvdp=dvldp)
      call specificVolume(t,p,Y,phaseY,vv,dvdt=dvvdt,dvdp=dvvdp)
      vmix = betaV*vv + betaL*vl
      RHS(2+nc) = P*(vmix-vspec(2))/(Rgas*T)
      dVdp = betaV*dvvdp + betaL*dvldp
      dVdt = betaV*dvvdt + betaL*dvldt
      M(2+nc,2+nc) = p*p*dVdp/(Rgas*t)
      M(1+nc,2+nc) = p*dVdt/Rgas
      M(2+nc,1+nc) = p*dVdt/Rgas
      M(1:nc,2+nc) = p*(FUGVP - FUGLP)
      M(2+nc,1:nc) = p*(FUGVP - FUGLP)
    else if (spec == 'TH' .OR. spec == 'HT') then
      call thermo(t,p,X,phaseX,FUGL,lnFUGX=FUGLX,lnfugt=FUGLT,lnfugp=FUGLP) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnFUGX=FUGVX,lnfugt=FUGVT,lnfugp=FUGVP) ! ln phi vapour
      call enthalpy(t,p,X,phaseX,hl,dhdp=dhldp)
      call enthalpy(t,p,Y,phaseY,hv,dhdp=dhvdp)
      hmix = betaV*hv + betaL*hl
      RHS(1+nc) = (vspec(1) - hmix)/(Rgas*T)
      dhdp = betaV*dhvdp + betaL*dhldp
      M(1+nc,1+nc) = -P*dhdp/(Rgas*T)
      M(1:nc,1+nc) = P*(FUGVP - FUGLP)
      M(1+nc,1:nc) = T*(FUGVT - FUGLT)
    endif

    ! If simpleMatrix==.true., set M(1:nc,1:nc) to the identity matrix. A NR
    ! iteration will then not change compositions.
    if (present(simpleMatrix)) then
      deactivateCompDiff = simpleMatrix
    else
      deactivateCompDiff = .false.
    end if
    if (deactivateCompDiff) then
      M(1:nc,1:nc) = 0.0
      do i=1,nc
        M(i,i) = 1.0
        if (Z(i) > zLimit) then
          RHS(i) = log(Y(i)) - log(X(i)) + FUGV(i) - FUGL(i)
        else
          RHS(i) = 0.0
        endif
      end do

      return
    end if

    if (betaV*betaL < almost_zero) then
      M(1:nc,1:nc) = 0.0
    else
      M(1:nc,1:nc) = 1.0/(betaV*betaL)*(-1.0 + betaV*FUGLX + betaL*FUGVX)
    endif

    ! Add to the diagonal
    do i=1,nc
      if (Z(i) > zLimit) then
        RHS(i) = log(Y(i)) - log(X(i)) + FUGV(i) - FUGL(i)
        denom = X(i)*Y(i)*betaV*betaL
        if(denom > almost_zero) M(i,i) = M(i,i) + Z(i)/denom
      else
        RHS(i) = 0.0
        M(:,i) = 0.0
        M(i,:) = 0.0
        M(i,i) = 1.0
      endif
    enddo

  end subroutine getStateFuncMatrix

  !-----------------------------------------------------------------------------
  !> Calculate RHS function values for Michelsen state functions
  !> Values corrensponds to subroutine getStateFuncMatirx
  !>
  !> \author MH, 2014-10-23
  !-----------------------------------------------------------------------------
  subroutine getStateFunc(t,p,Z,betaV,betaL,X,Y,spec,vspec,RHS,phase)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    character(len=2), intent(in) :: spec !< Character specification
    real, dimension(2), intent(in) :: vspec !< Specification value
    real, dimension(:), intent(out) :: RHS !< Right hand side
    integer, dimension(2), optional, intent(in) :: phase !< Phase integer
    ! Locals
    real, dimension(nc) :: FUGL,FUGV
    real :: sv,sl,hv,hl,smix,hmix,vv,vl,vmix
    integer :: i, phaseX, phaseY

    phaseX = LIQPH
    phaseY = VAPPH
    if (present(phase)) then
      phaseX = phase(1)
      phaseY = phase(2)
    endif

    if (spec == 'TP' .OR. spec == 'PT') then
      !
    else if (spec == 'SP' .OR. spec == 'PS') then
      call entropy(t,p,X,phaseX,sl)
      call entropy(t,p,Y,phaseY,sv)
      smix = betaV*sv + betaL*sl
      RHS(1+nc) = (vspec(1) - smix)/Rgas
    else if (spec == 'HP' .OR. spec == 'PH') then
      call enthalpy(t,p,X,phaseX,hl)
      call enthalpy(t,p,Y,phaseY,hv)
      hmix = betaV*hv + betaL*hl
      RHS(1+nc) = (vspec(1) - hmix)/(Rgas*T)
    else if (spec == 'UV') then
      ! Temperature terms
      call enthalpy(t,p,X,phaseX,hl)
      call enthalpy(t,p,Y,phaseY,hv)
      hmix = betaV*hv + betaL*hl
      RHS(1+nc) = (vspec(1) + P*vspec(2) - hmix)/(Rgas*T)

      ! Pressure terms
      call specificVolume(t,p,X,phaseX,vl)
      call specificVolume(t,p,Y,phaseY,vv)
      vmix = betaV*vv + betaL*vl
      RHS(2+nc) = P*(vmix-vspec(2))/(Rgas*T)
    else if (spec == 'TV' .OR. spec == 'VT') then
      call specificVolume(t,p,X,phaseX,vl)
      call specificVolume(t,p,Y,phaseY,vv)
      vmix = betaV*vv + betaL*vl
      RHS(1+nc) = P*(vmix-vspec(1))/(Rgas*T)
    else if (spec == 'PV' .OR. spec == 'VP') then
      ! NOT SYMETRIC!
      call specificVolume(t,p,X,phaseX,vl)
      call specificVolume(t,p,Y,phaseY,vv)
      vmix = betaV*vv + betaL*vl
      RHS(1+nc) = P*(vmix-vspec(1))/(Rgas*T)

    else if (spec == 'SV') then
      ! Entropy terms
      call entropy(t,p,X,phaseX,sl)
      call entropy(t,p,Y,phaseY,sv)
      smix = betaV*sv + betaL*sl
      RHS(1+nc) = (vspec(1) - smix)/Rgas

      ! Pressure terms
      call specificVolume(t,p,X,phaseX,vl)
      call specificVolume(t,p,Y,phaseY,vv)
      vmix = betaV*vv + betaL*vl
      RHS(2+nc) = P*(vmix-vspec(2))/(Rgas*T)

    else if (spec == 'TH' .OR. spec == 'HT') then
      call enthalpy(t,p,X,phaseX,hl)
      call enthalpy(t,p,Y,phaseY,hv)
      hmix = betaV*hv + betaL*hl
      RHS(1+nc) = (vspec(1) - hmix)/(Rgas*T)
    endif

    call thermo(t,p,X,phaseX,FUGL) ! ln phi liquid
    call thermo(t,p,Y,phaseY,FUGV) ! ln phi vapour
    ! Add to the diagonal
    do i=1,nc
      if (Z(i) > zLimit) then
        RHS(i) = log(Y(i)) - log(X(i)) + FUGV(i) - FUGL(i)
      else
        RHS(i) = 0.0
      endif
    enddo

  end subroutine getStateFunc

  !-----------------------------------------------------------------------------
  !> Calculate two-phase heat capacity at constant pressure.
  !>
  !> \author MHA, 2012-03-20
  !-----------------------------------------------------------------------------
  function dhdt_twoPhase(t,p,Z,betaV,betaL,X,Y,ph) result(dhdt)
    use numconstants, only: small
    use eos, only: enthalpy, thermo
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, dimension(2), optional, intent(in) :: ph !< Phase integers
    real :: dhdt
    ! Locals
    real, dimension(nc+1,nc+1) :: M !< Jacobian matrix
    real, dimension(nc+1) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+1) :: ipiv
    real, dimension(3*nc) ::  work
    integer :: ifail, phase
    real, dimension(nc) :: FUG
    real :: h

    vspec = 0.0
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'PH',vspec,M,RHS,ph)
    RHS(nc+1) = -1.0/(Rgas*t)
    RHS(1:nc) = 0.0

    ! lapack routines
    ! symmetric indefinite matrix
    call dsysv('u', nc+1, 1, M, nc+1, ipiv, RHS, nc+1, work, 3*nc, ifail)

    RHS(nc+1) = t * RHS(nc+1) ! Convert from dlnT -> dT
    if (ifail /= 0 .or. RHS(nc+1) <= small .or. RHS(nc+1) /= RHS(nc+1)) then
      ! Invalid heat capacity (proximity of critical point)
      ! Use single phase value
      call thermo(t,p,z,MINGIBBSPH,FUG,ophase=phase)
      if (phase == SINGLEPH) then
        phase = LIQPH
      endif
      call enthalpy(t,p,z,phase,h,dhdt=dhdt)
    else
      dhdt = 1.0 / RHS(nc+1)
    endif
  end function dhdt_twoPhase

  !-----------------------------------------------------------------------------
  !> Calculate two-phase enthalpy change at constant temperature.
  !>
  !> \author MH, 2014
  !-----------------------------------------------------------------------------
  function dhdp_twoPhase(t,p,Z,betaV,betaL,X,Y,ph) result(dhdp)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, dimension(2), optional, intent(in) :: ph !< Phase integers
    real :: dhdp
    ! Locals
    real, dimension(nc+1,nc+1) :: M !< Jacobian matrix
    real, dimension(nc+1) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+1) :: ipiv
    integer :: ifail, phase
    real, dimension(nc) :: FUG
    real :: h
    logical :: lufail

    vspec = 0.0
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'TH',vspec,M,RHS,ph)
    RHS(nc+1) = -1.0/(Rgas*t)
    RHS(1:nc) = 0.0

    ! lapack routines
    ! general lu
    lufail = .false.
    call dgetrf(nc+1,nc+1,M,nc+1,ipiv,ifail)
    if (ifail.ne.0) then
      lufail = .true.
    endif
    ! backsubstitute
    if (.not. lufail) then
      call dgetrs('n',nc+1,1,M,nc+1,ipiv,RHS,nc+1,ifail)
      if (ifail.ne.0) then
        lufail = .true.
      endif
    endif

    if (lufail .or. RHS(nc+1) /= RHS(nc+1)) then
      ! Use sinlgle phase values
      call thermo(t,p,z,MINGIBBSPH,FUG,ophase=phase)
      if (phase == SINGLEPH) then
        phase = LIQPH
      endif
      call enthalpy(t,p,z,phase,h,dhdp=dhdp)
    else
      RHS(nc+1) = P * RHS(nc+1) ! Convert from dlnP -> dP
      dhdp = 1.0 / RHS(nc+1)
    endif

  end function dhdp_twoPhase

  !-----------------------------------------------------------------------------
  !> Calculate two-phase temperature differential of specific volume at
  !! constant pressure.
  !!
  !! \author MH, 2012-07-06
  !-----------------------------------------------------------------------------
  function dvdt_twoPhase(t,p,Z,betaV,betaL,X,Y,ph) result(dvdt)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, dimension(2), optional, intent(in) :: ph !< Phase integers
    real :: dvdt
    ! Locals
    real, dimension(nc+1,nc+1) :: M !< Jacobian matrix
    real, dimension(nc+1) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+1) :: ipiv
    integer :: ifail, phase
    logical :: lufail
    real, dimension(nc) :: FUG
    real :: v

    vspec = 0.0
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'PV',vspec,M,RHS,ph)
    RHS(nc+1) = p/(Rgas*t)
    RHS(1:nc) = 0.0

    ! lapack routines
    ! general lu
    lufail = .false.
    call dgetrf(nc+1,nc+1,M,nc+1,ipiv,ifail)
    if (ifail.ne.0) then
      lufail = .true.
    endif
    ! backsubstitute
    if (.not. lufail) then
      call dgetrs('n',nc+1,1,M,nc+1,ipiv,RHS,nc+1,ifail)
      if (ifail.ne.0) then
        lufail = .true.
      endif
    endif

    if (lufail .or. RHS(nc+1) /= RHS(nc+1) .or. &
         machine_prec > abs(RHS(nc+1))) then
      ! Use sinlgle phase values
      call thermo(t,p,z,MINGIBBSPH,FUG,ophase=phase)
      if (phase == SINGLEPH) then
        phase = LIQPH
      endif
      call specificVolume(t,p,z,phase,v,dvdt=dvdt)
    else
      RHS(nc+1) = t * RHS(nc+1) ! Convert from dlnT -> dT
      dvdt = 1.0 / RHS(nc+1)
    endif
  end function dvdt_twoPhase

  !-----------------------------------------------------------------------------
  !> Calculate two-phase pressure differential of specific volume at
  !! constant temperature.
  !!
  !! \author MH, 2012-07-06
  !-----------------------------------------------------------------------------
  function dvdp_twoPhase(t,p,Z,betaV,betaL,X,Y,ph) result(dvdp)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Vapour phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, dimension(2), optional, intent(in) :: ph !< Phase integers
    real :: dvdp
    ! Locals
    real, dimension(nc+1,nc+1) :: M !< Jacobian matrix
    real, dimension(nc+1) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+1) :: ipiv
    real, dimension(3*nc) ::  work
    integer :: ifail, phase
    real, dimension(nc) :: FUG
    real :: v

    vspec = 0.0
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'TV',vspec,M,RHS,ph)
    RHS(nc+1) = p/(Rgas*t)
    RHS(1:nc) = 0.0

    ! lapack routines
    ! symmetric indefinite matrix
    call dsysv('u', nc+1, 1, M, nc+1, ipiv, RHS, nc+1, work, 3*nc, ifail)
    if (ifail /= 0 .or. RHS(nc+1) /= RHS(nc+1)) then
      ! Use sinlgle phase values
      call thermo(t,p,z,MINGIBBSPH,FUG,ophase=phase)
      if (phase == SINGLEPH) then
        phase = LIQPH
      endif
      call specificVolume(t,p,z,phase,v,dvdp=dvdp)
    else
      RHS(nc+1) = p * RHS(nc+1) ! Convert from dlnP -> dP
      dvdp = 1.0 / RHS(nc+1)
    endif
  end function dvdp_twoPhase

  !-----------------------------------------------------------------------------
  !> Calculate temperature/pressure differential of mole number (and phase fraction)
  !! at constant pressure
  !! \author MH, 2015-01
  !-----------------------------------------------------------------------------
  subroutine dnvdX(t,p,Z,betaV,betaL,X,Y,dbetadT,dbetadP,dndT,dndP)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Vapour phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, optional, intent(out) :: dbetadT,dbetadP,dndT(nc),dndP(nc) !< Mole based
    ! Locals
    real, dimension(nc,nc) :: M !< Jacobian matrix
    real, dimension(nc,2) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc) :: ipiv
    real, dimension(3*nc) ::  work
    integer :: ifail
    real, dimension(nc) :: FUGL,FUGV,FUGLT,FUGVT,FUGLP,FUGVP

    vspec = 0.0
    call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'TP',vspec,M,RHS(:,1))
    call thermo(t,p,X,LIQPH,FUGL,lnfugt=FUGLT,lnfugp=FUGLP)
    call thermo(t,p,Y,VAPPH,FUGV,lnfugt=FUGVT,lnfugp=FUGVP)
    RHS(:,1) = -(FUGVT - FUGLT)
    RHS(:,2) = -(FUGVP - FUGLP)

    ! lapack routines
    ! symmetric indefinite matrix
    call dsysv('u', nc, 2, M, nc, ipiv, RHS, nc, work, 3*nc, ifail)
    if (ifail /= 0 .or. RHS(1,1) /= RHS(1,1)) then
      RHS = 0.0
    endif
    if (present(dbetadT)) then
      dbetadT = sum(RHS(:,1))
    endif
    if (present(dndT)) then
      dndT = RHS(:,1)
    endif
    if (present(dbetadP)) then
      dbetadP = sum(RHS(:,2))
    endif
    if (present(dndP)) then
      dndP = RHS(:,2)
    endif
  end subroutine dnvdX


  !-----------------------------------------------------------------------------
  !> Calculate single- or two-phase derivatives of entropy and/or molar volume.
  !>
  !> \author MAG, 2015-03-03
  !-----------------------------------------------------------------------------
  subroutine getSVDerivativesTwoPhase(t,p,z,betav,betal,x,y,iphase,dsdp_v,dsdt_v,dvdp_s,dvdt_s)
    implicit none
    real, intent(in) :: betav !< Vapour phase molar fraction [-]
    real, intent(in) :: betal !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: x !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, intent(in) :: iphase !< Phase flag
    real, optional, intent(out) :: dsdp_v !< Partial derivative of entropy w.r.t. pressure at constant molar volume
    real, optional, intent(out) :: dsdt_v !< Partial derivative of entropy w.r.t. temperature at constant molar volume
    real, optional, intent(out) :: dvdp_s !< Partial derivative of molar volume w.r.t. pressure at constant entropy
    real, optional, intent(out) :: dvdt_s !< Partial derivative of molar volume w.r.t. temperature at constant entropy
    ! Locals
    real, dimension(nc+2,nc+2) :: a,m !< Jacobian matrix
    real, dimension(nc+2) :: rhs !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+2) :: ipiv
    real :: s,v,dsdp_t,dsdt_p,dvdt_p,dvdp_t
    integer :: ifail,iphase_internal
    logical :: lufail
    !
    iphase_internal = iphase
    !
    if (iphase_internal == TWOPH) then
      ! Two-phase calculation
      !
      vspec = 0.0
      call getStateFuncMatrix(t,p,z,betav,betal,x,y,'SV',vspec,m,rhs)
      !
      if (present(dsdp_v) .or. present(dsdt_v)) then
        rhs(1:nc) = 0.0
        rhs(nc+1) = -1.0/rgas
        rhs(nc+2) = 0.0
        a = m
        ! Solve linear system
        lufail = .false.
        call dgetrf(nc+2,nc+2,a,nc+2,ipiv,ifail)
        if (ifail /= 0) lufail = .true.
        if (.not. lufail) then
          call dgetrs('n',nc+2,1,a,nc+2,ipiv,rhs,nc+2,ifail)
          if (ifail /= 0) lufail = .true.
        endif
        ! Set derivative values
        if (present(dsdt_v)) then
          rhs(nc+1) = t*rhs(nc+1) ! Convert from dlnT -> dT
          dsdt_v = 1.0/rhs(nc+1)
        end if
        if (present(dsdp_v)) then
          rhs(nc+2) = p*rhs(nc+2) ! Convert from dlnP -> dP
          dsdp_v = 1.0/rhs(nc+2)
        end if
      end if
      !
      if (present(dvdp_s) .or. present(dvdt_s)) then
        rhs(1:nc+1) = 0.0
        rhs(nc+2) = p/(rgas*t)
        a = m
        ! Solve linear system
        lufail = .false.
        call dgetrf(nc+2,nc+2,a,nc+2,ipiv,ifail)
        if (ifail /= 0) lufail = .true.
        if (.not. lufail) then
          call dgetrs('n',nc+2,1,a,nc+2,ipiv,rhs,nc+2,ifail)
          if (ifail /= 0) lufail = .true.
        endif
        ! Set derivative values
        if (present(dvdt_s)) then
          rhs(nc+1) = t*rhs(nc+1) ! Convert from dlnT -> dT
          dvdt_s = 1.0/rhs(nc+1)
        end if
        if (present(dvdp_s)) then
          rhs(nc+2) = p*rhs(nc+2) ! Convert from dlnP -> dP
          dvdp_s = 1.0/rhs(nc+2)
        end if
      end if
    else
      ! Single-phase calculation
      !
      if (iphase_internal /= VAPPH) iphase_internal = LIQPH
      call entropy(t,p,z,iphase_internal,s,dsdt=dsdt_p,dsdp=dsdp_t)
      call specificVolume(t,p,z,iphase_internal,v,dvdt=dvdt_p,dvdp=dvdp_t)
      !
      if (present(dsdp_v)) dsdp_v = dsdp_t - dsdt_p*dvdp_t/dvdt_p
      if (present(dsdt_v)) dsdt_v = dsdt_p - dsdp_t*dvdt_p/dvdp_t
      if (present(dvdp_s)) dvdp_s = dvdp_t - dvdt_p*dsdp_t/dsdt_p
      if (present(dvdt_s)) dvdt_s = dvdt_p - dvdp_t*dsdt_p/dsdp_t
    endif
    !
  end subroutine getSVDerivativesTwoPhase

  !-----------------------------------------------------------------------------
  !> Calculate single- or two-phase derivatives of internal energy and/or molar
  !> volume.
  !>
  !> CAUTION: The implementations of dudp_v and dudt_v are covered by the unit
  !>          tests in 3DMF. The implementations of dvdp_u and dvdt_u have not
  !>          been tested yet.
  !>
  !> \author MAG, 2015-11-09
  !-----------------------------------------------------------------------------
  subroutine getUVDerivativesTwoPhase(t,p,z,betav,betal,x,y,iphase, &
       dudp_v,dudt_v,dvdp_u,dvdt_u)
    use eosTV, only: pressure,internal_energy
    implicit none
    real, intent(in) :: betav !< Vapour phase molar fraction [-]
    real, intent(in) :: betal !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: z !< Overall molar composition [-]
    real, dimension(nc), intent(in) :: x !< Liquid molar composition [-]
    real, dimension(nc), intent(in) :: y !< Vapour molar composition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, intent(in) :: iphase !< Phase flag
    real, optional, intent(out) :: dudp_v !< Partial derivative of internal energy w.r.t. pressure at constant molar volume
    real, optional, intent(out) :: dudt_v !< Partial derivative of internal energy w.r.t. temperature at constant molar volume
    real, optional, intent(out) :: dvdp_u !< Partial derivative of molar volume w.r.t. pressure at constant internal energy
    real, optional, intent(out) :: dvdt_u !< Partial derivative of molar volume w.r.t. temperature at constant internal energy
    ! Local variables:
    real, dimension(nc+2,nc+2) :: a,m !< Jacobian matrix
    real, dimension(nc+2) :: rhs !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+2) :: ipiv
    real :: u_internal,v,dvdt_p,dvdp_t,dudt_v_internal
    real :: dudv_t,dudv_p,dudp_v_internal
    real :: dpdv_t,dpdv_u,dpdt_v,p_internal
    integer :: ifail,iphase_internal
    logical :: lufail
    !
    iphase_internal = iphase
    !
    if (present(dvdp_u) .or. present(dvdt_u)) then
      call stoperror("The implementations of the derivatives dvdp_u and dvdt_u have not been tested yet.")
    end if
    !
    if (iphase_internal == TWOPH) then
      !
      ! Two-phase calculation
      ! =====================
      !
      vspec = 0.0
      call getStateFuncMatrix(t,p,z,betav,betal,x,y,'UV',vspec,m,rhs)
      !
      if (present(dudp_v) .or. present(dudt_v)) then
        rhs(1:nc) = 0.0
        rhs(nc+1) = -1.0/(rgas*t)
        rhs(nc+2) = 0.0
        a = m
        ! Solve linear system
        lufail = .false.
        call dgetrf(nc+2,nc+2,a,nc+2,ipiv,ifail)
        if (ifail /= 0) lufail = .true.
        if (.not. lufail) then
          call dgetrs('n',nc+2,1,a,nc+2,ipiv,rhs,nc+2,ifail)
          if (ifail /= 0) lufail = .true.
        endif
        ! Set derivative values
        if (present(dudt_v)) then
          rhs(nc+1) = t*rhs(nc+1) ! Convert from dlnT -> dT
          dudt_v = 1.0/rhs(nc+1)
        end if
        if (present(dudp_v)) then
          rhs(nc+2) = p*rhs(nc+2) ! Convert from dlnP -> dP
          dudp_v = 1.0/rhs(nc+2)
        end if
      end if
      !
      if (present(dvdp_u) .or. present(dvdt_u)) then
        rhs(1:nc+1) = 0.0
        rhs(nc+2) = p/(rgas*t)
        a = m
        ! Solve linear system
        lufail = .false.
        call dgetrf(nc+2,nc+2,a,nc+2,ipiv,ifail)
        if (ifail /= 0) lufail = .true.
        if (.not. lufail) then
          call dgetrs('n',nc+2,1,a,nc+2,ipiv,rhs,nc+2,ifail)
          if (ifail /= 0) lufail = .true.
        endif
        ! Set derivative values
        if (present(dvdt_u)) then
          rhs(nc+1) = t*rhs(nc+1) ! Convert from dlnT -> dT
          dvdt_u = 1.0/rhs(nc+1)
        end if
        if (present(dvdp_u)) then
          rhs(nc+2) = p*rhs(nc+2) ! Convert from dlnP -> dP
          dvdp_u = 1.0/rhs(nc+2)
        end if
      end if
    else
      !
      ! Single-phase calculation
      ! ========================
      !
      if (iphase_internal /= VAPPH) iphase_internal = LIQPH
      call specificVolume(t,p,z,iphase_internal,v,dvdt=dvdt_p,dvdp=dvdp_t)
      call internal_energy(t,v,z,u_internal,dudt=dudt_v_internal,dudv=dudv_t)
      p_internal = pressure(t,v,z,dpdv_t,dpdt_v)
      !
      if (present(dudt_v)) dudt_v = dudt_v_internal
      if (present(dvdt_u)) dvdt_u = -dudt_v_internal/dudv_t
      dudv_p = dudv_t - dudt_v_internal*dpdv_t/dpdt_v
      dpdv_u = dpdv_t - dpdt_v*dudv_t/dudt_v_internal
      dudp_v_internal = -dudv_p/dpdv_u
      if (present(dudp_v)) then
        dudp_v = dudp_v_internal
      end if
      if (present(dvdp_u)) dvdp_u = -dudp_v_internal/dudv_p
    endif
    !
  end subroutine getUVDerivativesTwoPhase

  !--------------------------------------------------------------------------
  !> Calculate two-phase Joule-Thompson coefficient
  !! dT/dP at constant enthalpy
  !! \author MH, 2015-09
  !--------------------------------------------------------------------------
  function getJouleThompsonCoeff(t,p,Z,betaV,betaL,X,Y,phase,ph) result(dTdP)
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Vapour phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, intent(in) :: phase !< Phase identifyer
    integer, dimension(2), optional, intent(in) :: ph !< Phase integers
    real :: dTdP
    ! Locals
    real, dimension(nc+1,nc+1) :: M !< Jacobian matrix
    real, dimension(nc+1) :: RHS !< Right hand side
    real, dimension(2) :: vspec !< Dummy
    integer, dimension(nc+1) :: ipiv
    real, dimension(3*nc) ::  work
    integer :: ifail, lphase, phaseX, phaseY
    real, dimension(nc) :: lnfugl,lnfuglp,lnfugv,lnfugvp
    real :: hl,dhldp,hv,dhvdp,h,dhdt,dhdp

    lphase = phase
    if (phase == TWOPH) then
      vspec = 0.0
      call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'PH',vspec,M,RHS,ph)
      if (present(ph)) then
        phaseX = ph(1)
        phaseY = ph(2)
      else
        phaseX = LIQPH
        phaseY = VAPPH
      endif
      call enthalpy(t,p,X,phaseX,hl,dhdp=dhldp)
      call enthalpy(t,p,Y,phaseY,hv,dhdp=dhvdp)
      RHS(1+nc) = (betaV*dhvdp + betaL*dhldp)/(Rgas*T)
      call thermo(t,p,X,phaseX,lnfugl,lnfugp=lnfuglp)
      call thermo(t,p,Y,phaseY,lnfugv,lnfugp=lnfugvp)
      RHS(1:nc) = -(lnfugvp - lnfuglp)
      ! lapack routines
      ! symmetric indefinite matrix
      call dsysv('u', nc+1, 1, M, nc+1, ipiv, RHS, nc+1, work, 3*nc, ifail)
      if (ifail /= 0 .or. RHS(nc+1) /= RHS(nc+1)) then
        ! Use sinlgle phase values
        call thermo(t,p,z,MINGIBBSPH,lnfugl,ophase=lphase)
      else
        dTdP = T * RHS(nc+1) ! Convert from dlnT -> dT
      endif
    endif
    if (lphase /= TWOPH) then
      if (lphase == SINGLEPH) then
        lphase = LIQPH
      endif
      call enthalpy(t,p,Z,lphase,h,dhdt=dhdt,dhdp=dhdp)
      dTdP = -dhdp/dhdt
    endif
  end function getJouleThompsonCoeff

  !--------------------------------------------------------------------------
  !> Calculate temperaure differential of pressure at
  !! constant volume.
  !!
  !! \author MH, 2015-10
  !--------------------------------------------------------------------------
  function dpdt_twoPhase(t,p,Z,betaV,betaL,X,Y,phase,ph) result(dpdt)
    use eos, only: specificVolume, thermo
    implicit none
    real, intent(in) :: betaV !< Vapour phase molar fraction [-]
    real, intent(in) :: betaL !< Vapour phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, intent(in) :: phase !< Phase identifyer
    integer, dimension(2), optional, intent(in) :: ph !< Phase integers
    real :: dpdt
    ! Locals
    real, dimension(nc+1,nc+1) :: M !< Jacobian matrix
    real, dimension(nc+1) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+1) :: ipiv
    real, dimension(3*nc) ::  work
    integer :: ifail, lphase, phaseX, phaseY
    real, dimension(nc) :: FUGL, FUGV, lnfugvt, lnfuglt
    real :: v, vv, vl, dvvdt, dvldt, dvdt, dvdp

    lphase = phase
    if (phase == TWOPH) then
      vspec = 0.0
      call getStateFuncMatrix(t,p,Z,betaV,betaL,X,Y,'TV',vspec,M,RHS,ph)
      if (present(ph)) then
        phaseX = ph(1)
        phaseY = ph(2)
      else
        phaseX = LIQPH
        phaseY = VAPPH
      endif
      call thermo(t,p,X,phaseX,FUGL,lnfugt=lnfuglt) ! ln phi liquid
      call thermo(t,p,Y,phaseY,FUGV,lnfugt=lnfugvt) ! ln phi vapour
      call specificVolume(t,p,X,phaseX,vl,dvdt=dvldt)
      call specificVolume(t,p,Y,phaseY,vv,dvdt=dvvdt)
      dVdt = betaV*dvvdt + betaL*dvldt
      RHS(nc+1) = dvdt*p/(Rgas*t)
      RHS(1:nc) = (lnfugvt - lnfuglt)
      RHS = - RHS
      ! lapack routines
      ! symmetric indefinite matrix
      call dsysv('u', nc+1, 1, M, nc+1, ipiv, RHS, nc+1, work, 3*nc, ifail)
      if (ifail /= 0 .or. RHS(nc+1) /= RHS(nc+1)) then
        ! Use sinlgle phase values
        call thermo(t,p,z,MINGIBBSPH,FUGL,ophase=lphase)
      else
        dpdt = p * RHS(nc+1) ! Convert from dlnP -> dP
      endif
    endif
    if (lphase /= TWOPH) then
      if (lphase == SINGLEPH) then
        lphase = LIQPH
      endif
      call specificVolume(t,p,z,lphase,v,dvdt=dvdt,dvdp=dvdp)
      dpdt = -dvdt/dvdp
    endif
  end function dpdt_twoPhase

end module state_functions
