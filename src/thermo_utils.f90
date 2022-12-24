!> Module for thermodynamic helper-routines which may be useful for
!> user-applications, but which do not belong in eos.f90
!> \author EA, 2014-05

module thermo_utils
  use thermopack_constants, only: LIQPH, VAPPH, SINGLEPH, FAKEPH, MINGIBBSPH, &
       WATER, TREND
  use thermopack_var, only: nc, thermo_model, get_active_eos, base_eos_param, &
       get_active_thermo_model, get_active_alt_eos, Rgas

  implicit none
  public :: guessPhase, isWaterComponent, waterComponentFraction
  public :: guessPhaseTV, get_n_solids
  public :: isSingleComp, maxComp, isTwoComp
  public :: get_b_linear_mix
  public :: phase_is_fake
  public :: wilsonK, wilsonKdiff, wilsonKi, calcLnPhiOffset

  private

  contains

  !----------------------------------------------------------------------
  !> Estimate the single phase given by T,P,z as either liquid or vapor.
  !>
  !> \author EA, 2014-05. Updated EA, 2015-01
  !> Based on original code by MH, 2012-11-22
  !----------------------------------------------------------------------
  function guessPhase(T,P,z,T_comp,p_comp,vb_ratio)
    use eos, only: thermo, pseudo_safe, zfac, specificVolume
    use thermopack_constants, only: PSEUDO_CRIT_ZFAC,&
                          PSEUDO_CRIT_MOLAR_VOLUME, VOLUME_COVOLUME_RATIO
    implicit none
    ! Input:
    real, intent(in) :: T     !< Temperature (K)
    real, intent(in) :: P     !< Pressure (Pa)
    real, intent(in) :: z(nc) !< Molar composition (mol/mol)
    real, optional, intent(in) :: T_comp !< Override for comparison point (K)
    real, optional, intent(in) :: p_comp !< Override for comparison point (Pa)
    real, optional, intent(in) :: vb_ratio !< Override volume-covolume ratio
    ! Output:
    integer          :: guessPhase !< Best guess for phase (LIQPH or VAPPH)
    ! Internal:
    real             :: Tpc, Ppc, Zpc, vpc, Zf, v, lnfug(nc), vbr, b
    integer          :: ophase
    type(thermo_model), pointer :: act_mod_ptr

    act_mod_ptr => get_active_thermo_model()
    call thermo(T,P,z,MINGIBBSPH,lnfug,ophase=ophase)
    if (ophase == LIQPH .or. ophase == VAPPH) then
      ! If thermo() has a definite answer, just trust that.
      guessPhase = ophase
    else
      ! thermo() did not identify the phase.
      ! Identify phase based on method chosen in liq_vap_discr_method

      if (act_mod_ptr%liq_vap_discr_method /= VOLUME_COVOLUME_RATIO) then
        if (present(T_comp) .and. present(p_comp)) then
          ! Use overridden T,p point
          call zfac(T_comp,p_comp,z,VAPPH,Zpc)
          vpc = Zpc*Rgas*T_comp/p_comp
        else
          ! Get pseudocritical properties
          call pseudo_safe(z,Tpc,Ppc,Zpc,vpc)
        endif
      endif

      ! Select based on chosen method
      select case(act_mod_ptr%liq_vap_discr_method)

        case (PSEUDO_CRIT_ZFAC)
          call zfac(T,P,z,VAPPH,Zf)
          if (Zf > Zpc) then
            guessPhase = VAPPH
          else
            guessPhase = LIQPH
          endif

        case (PSEUDO_CRIT_MOLAR_VOLUME)
          call specificVolume(T,P,z,VAPPH,v)
          if (v > vpc) then
            guessPhase = VAPPH
          else
            guessPhase = LIQPH
          endif

        case (VOLUME_COVOLUME_RATIO)
          call specificVolume(T,P,z,LIQPH,v)
          if (present(vb_ratio)) then
            vbr = vb_ratio
          else
            vbr = 1.75
          endif
          ! Get co-volume
          b = get_b_linear_mix(z)
          if (v/b < vbr) then
            guessPhase = LIQPH
          else
            guessPhase = VAPPH
          endif
        end select
    endif
  end function guessPhase

  !----------------------------------------------------------------------
  !> Estimate the single phase given by T,V,z as either liquid or vapor.
  !>
  !> \author MH, 20018-04-13
  !> Based on guessPhase code
  !----------------------------------------------------------------------
  function guessPhaseTV(T,v,z,T_comp,v_comp,vb_ratio)
    use eos, only: pseudo_safe, zfac
    use eosTV, only: pressure
    use thermopack_constants, only: PSEUDO_CRIT_ZFAC,&
         PSEUDO_CRIT_MOLAR_VOLUME, VOLUME_COVOLUME_RATIO
    implicit none
    ! Input:
    real, intent(in) :: T     !< Temperature (K)
    real, intent(in) :: v     !< Volume (m3/mol)
    real, intent(in) :: z(nc) !< Molar composition (mol/mol)
    real, optional, intent(in) :: T_comp !< Override for comparison point (K)
    real, optional, intent(in) :: v_comp !< Override for comparison point (m3/mol)
    real, optional, intent(in) :: vb_ratio !< Override volume-covolume ratio
    ! Output:
    integer          :: guessPhaseTV !< Best guess for phase (LIQPH or VAPPH)
    ! Internal:
    real             :: Tpc, Ppc, Zpc, vpc, Zf, p, vbr, b
    type(thermo_model), pointer :: act_mod_ptr

    act_mod_ptr => get_active_thermo_model()

    ! Identify phase based on method chosen in liq_vap_discr_method
    if (act_mod_ptr%liq_vap_discr_method /= VOLUME_COVOLUME_RATIO) then
      if (present(T_comp) .and. present(v_comp)) then
        ! Use overridden T,v point
        p = pressure(T_comp,v_comp,z)
        Zpc = v_comp*p/(Rgas*T_comp)
        vpc = v_comp
      else
        ! Get pseudocritical properties
        call pseudo_safe(z,Tpc,Ppc,Zpc,vpc)
      endif
    endif

    ! Select based on chosen method
    select case(act_mod_ptr%liq_vap_discr_method)
    case (PSEUDO_CRIT_ZFAC)
      p = pressure(T,v,z)
      Zf = v*p/(Rgas*T)
      if (Zf > Zpc) then
        guessPhaseTV = VAPPH
      else
        guessPhaseTV = LIQPH
      endif

    case (PSEUDO_CRIT_MOLAR_VOLUME)
      if (v > vpc) then
        guessPhaseTV = VAPPH
      else
        guessPhaseTV = LIQPH
      endif

    case (VOLUME_COVOLUME_RATIO)
      if (present(vb_ratio)) then
        vbr = vb_ratio
      else
        vbr = 1.75
      endif
      ! Get co-volume
      b = get_b_linear_mix(z)

      if (v/b < vbr) then
        guessPhaseTV = LIQPH
      else
        guessPhaseTV = VAPPH
      endif
    end select
  end function guessPhaseTV

  ! Return true if component is water or water soluble
  function isWaterComponent(i) result(isWComp)
    use thermopack_var, only: complist
    implicit none
    integer, intent(in) :: i
    logical :: isWComp
    ! Locals
    isWComp = .false.
    if (trim(complist(i)) == 'H2O') then
      isWComp = .true.
    else if (trim(complist(i)) == 'MEG') then
      isWComp = .true.
    endif
  end function isWaterComponent

  ! Return amount of water components
  function waterComponentFraction(Z) result(wCompFrac)
    implicit none
    real, dimension(nc), intent(in) :: Z
    real :: wCompFrac
    ! Locals
    integer :: i
    wCompFrac = 0.0
    do i=1,nc
      if (isWaterComponent(i)) then
        wCompFrac = wCompFrac + Z(i)
      endif
    enddo
  end function waterComponentFraction

  !-------------------------------------------------------------------------
  !> Set offset to Wilson liquid fugacity
  !>
  !> \author MH, 2016-03
  !-------------------------------------------------------------------------
  subroutine calcLnPhiOffset(pid,lnPhi_offset)
    implicit none
    integer, optional, intent(in) :: pid
    real, dimension(nc), intent(out) :: lnPhi_offset
    ! Locals
    integer :: i

    lnPhi_offset = 0.0
    do i=1,nc
      if (pid == WATER) then
        if (.not. isWaterComponent(i)) then
          lnPhi_offset(i) = 10.0
        endif
      else
        if (isWaterComponent(i)) then
          lnPhi_offset(i) = 10.0
        endif
      endif
    enddo
  end subroutine calcLnPhiOffset

  !----------------------------------------------------------------------
  !> Calculate Wilson K-values
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine wilsonK(t,p,K,dKdt,dKdp,liqType)
    use eos, only: getCriticalParam
    implicit none
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(nc), intent(out) :: K !< K-values
    real, optional, dimension(nc), intent(out) :: dKdt !< 1/K - Differential of K-values wrpt. temperature
    real, optional, dimension(nc), intent(out) :: dKdp !< 1/Pa - Differential of K-values wrpt. pressure
    integer, optional, intent(in) :: liqType !< Type of liquid (WATER, NONWATER)
    ! Locals
    integer :: i
    real :: tci, pci, oi
    real, dimension(nc) :: lnPhi_offset !< -
    if (present(liqType)) then
      call calcLnPhiOffset(liqType, lnPhi_offset)
    else
      lnPhi_offset = 0.0
    endif
    do i=1,nc
      call getCriticalParam(i,tci,pci,oi)
      K(i) = (pci/p)*exp(5.373*(1+oi)*(1-tci/t) + lnPhi_offset(i))
      if (present(dKdp)) then
        dKdp(i) = -K(i)/p
      endif
      if (present(dKdt)) then
        if (K(i) > 0.0) then
          dKdt(i) = 5.373*(1+oi)*tci/t**2*K(i)
        else
          dKdt(i) = 0.0
        endif
      endif
    enddo
  end subroutine wilsonK

  !----------------------------------------------------------------------
  !> Calculate Wilson K-value for component i
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine wilsonKi(i,t,p,lnPhi_offset,K)
    use eos, only: getCriticalParam
    implicit none
    integer, intent(in) :: i !< Component
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, intent(in) :: lnPhi_offset
    real, intent(out) :: K !< K-value
    ! Locals
    real :: tci, pci, oi
    call getCriticalParam(i,tci,pci,oi)
    K = (pci/p)*exp(5.373*(1+oi)*(1-tci/t) + lnPhi_offset)
  end subroutine wilsonKi

  !----------------------------------------------------------------------
  !> Calculate Wilson K-values with pressure and temperature differentials
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine wilsonKdiff(t,p,K,dKdp,dKdt)
    use eos, only: getCriticalParam
    implicit none
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(nc), intent(out) :: K !< K-values
    real, dimension(nc), intent(out) :: dKdt !< 1/K - Differential of K-values wrpt. temperature
    real, dimension(nc), intent(out) :: dKdp !< 1/Pa - Differential of K-values wrpt. pressure
    ! Locals
    integer :: i
    real :: tci, pci, oi
    do i=1,nc
      call getCriticalParam(i,tci,pci,oi)
      K(i) = (pci/p)*exp(5.373*(1+oi)*(1-tci/t))
      dKdp(i) = -K(i)/p
      dKdt(i) = 5.373*(1+oi)*tci/t**2*K(i)
    enddo
  end subroutine wilsonKdiff

  !-------------------------------------------------------------------------
  !> Get number of solids in a mixture
  !>
  !> \author MH, 2018-06
  !-------------------------------------------------------------------------
  function get_n_solids(nd,phaseVec) result(ns)
    use thermopack_constants, only: SOLIDPH
    use thermopack_var, only: nph
    implicit none
    integer :: nd !< Number of solids
    integer, intent(in) :: phaseVec(nph) !< Phases
    integer :: ns
    ! Locals
    integer :: i
    ns = 0
    do i=1,nd
      if (phaseVec(i) == SOLIDPH) then
        ns = ns + 1
      endif
    enddo
  end function get_n_solids

  !> Is the "mixture" single component
  !> \author M. Hammer October 2014
  function isSingleComp(Z) result(isSingle)
    use thermopack_var, only: nc
    implicit none
    real, dimension(nc), intent(in) :: Z
    logical :: isSingle
    ! Locals
    integer, dimension(1) :: imax
    imax = maxloc(Z)
    if (nc == 1 .or. Z(imax(1)) == sum(Z)) then
      isSingle = .true.
    else
      isSingle = .false.
    endif
  end function isSingleComp

  !> Is the "mixture" two component
  !> \author M. Hammer March 2016
  function isTwoComp(Z) result(isTwo)
    use thermopack_var, only: nc
    implicit none
    real, dimension(nc), intent(in) :: Z
    logical :: isTwo
    ! Locals
    integer, dimension(1) :: imax
    real, dimension(nc) :: Zcpy
    isTwo = .false.
    if (nc == 2) then
      isTwo = .true.
    else if (nc > 2) then
      imax = maxloc(Z)
      if (Z(imax(1)) /= sum(Z)) then
        Zcpy = Z
        Zcpy(imax(1)) = 0.0
        imax = maxloc(Zcpy)
        Zcpy(imax(1)) = 0.0
        if (sum(Zcpy) == 0.0) then
          isTwo = .true.
        endif
      endif
    endif
  end function isTwoComp

  !> Return index of largest component fraction
  !> \author M. Hammer October 2014
  function maxComp(Z) result(index)
    use thermopack_var, only: nc
    implicit none
    real, dimension(nc), intent(in) :: Z
    integer :: index
    ! Locals
    integer, dimension(1) :: imax
    imax = maxloc(Z)
    index = imax(1)
  end function maxComp

  !-------------------------------------------------------------------------
  !> Convert overall phase flag to one flag per phase
  !> Optionally set phase and component fractions
  !> \author MH, 2016, 03
  !-------------------------------------------------------------------------
  subroutine phaseToPhaseVec(phase,nd,phaseVec,betaG,betaLS,betaVec,Y,X,XX)
    use thermopack_var, only: nc, nph
    use thermopack_constants, only: TWOPH, VAPPH, LIQPH, VAPSOLPH, SOLIDPH
    implicit none
    integer, intent(in) :: phase !< Phase identifier
    integer, dimension(nph), intent(out) :: phaseVec !< Phase identifiers
    integer, intent(out) :: nd !< Number of phases
    real, optional, intent(in) :: betaG,betaLS !< Phase mole fractions
    real, optional, dimension(nph), intent(out) :: betaVec !< Vector of phase fractions
    real, optional, dimension(nc), intent(in) :: X, Y !< Phase molar fractions
    real, optional, dimension(nph,nc), intent(out) :: XX !< Vector of phase molar fractions
    !
    if (phase == TWOPH) then
      nd = 2
      phaseVec(1) = VAPPH
      phaseVec(2) = LIQPH
    else if (phase == VAPSOLPH) then
      nd = 2
      phaseVec(1) = VAPPH
      phaseVec(2) = SOLIDPH
    else
      nd = 1
      phaseVec(1) = phase
    endif
    if (present(betaVec)) then
      if (nd == 2) then
        if (present(betaG)) then
          if (betaG < 0.0) then
            call stoperror("phaseToPhaseVec: Wrong betaG")
          else
            betaVec(1) = betaG
          endif
        endif
        if (present(betaLS)) then
          betaVec(2) = betaLS
        else
          betaVec(2) = 1.0 - betaG
        endif
      else
        betaVec(1) = 1.0
      endif
    endif
    if (present(X) .and. present(Y) .and. present(XX)) then
      XX(1,:) = Y
      XX(2,:) = X
    endif
  end subroutine phaseToPhaseVec

  !-----------------------------------------------------------------------------
  !> Get linear combination of b_i
  !>
  !> \author MH, 2020-07
  !-----------------------------------------------------------------------------
  function get_b_linear_mix(Z) result(b_mix)
    use cubic_eos, only: cb_eos
    use eosdata, only: eosCPA
    implicit none
    ! Input:
    real, intent(in)                :: Z(nc)          !< Molar compozition [-]
    ! Output:
    real                            :: b_mix          !< m3/mol
    ! Locals
    integer :: i
    type(thermo_model), pointer :: act_mod_ptr
    class(base_eos_param), pointer :: act_eos_ptr

    act_mod_ptr => get_active_thermo_model()
    b_mix = 0
    if (act_mod_ptr%need_alternative_eos .and. act_mod_ptr%eosidx /= eosCPA) then
      act_eos_ptr => get_active_alt_eos()
      select type (p_eos => act_eos_ptr)
      class is (cb_eos)
        do i=1,nc
          b_mix = b_mix + z(i)*p_eos%single(i)%b
        enddo
      class default
        call stoperror("get_b_linear_mix (alt): Should not be here")
      end select
    else
      act_eos_ptr => get_active_eos()
      select type (p_eos => act_eos_ptr)
      class is (cb_eos)
        do i=1,nc
          b_mix = b_mix + z(i)*p_eos%single(i)%b
        enddo
      class default
        call stoperror("get_b_linear_mix: Should not be here")
      end select
    endif
    b_mix = b_mix*1.0e-3 ! L/mol -> m3/mol
  end function get_b_linear_mix

  !-----------------------------------------------------------------------------
  !> Is the solver returning FAKEPH solution
  !>
  !> \author MH, 2020-01
  !-----------------------------------------------------------------------------
  function phase_is_fake(T,P,Z,phase) result(isFake)
    use trend_solver, only: trend_phase_is_fake
    implicit none
    ! Input:
    real, intent(in)                :: Z(nc)          !< Molar compozition [-]
    real, intent(in)                :: T              !< Temperature [K]
    real, intent(in)                :: P              !< Pressure [Pa]
    integer, intent(in)             :: phase          !< Phase
    ! Output:
    logical                         :: isFake         !< Phase is FAKEPH
    ! Locals
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    if (act_mod_ptr%EoSLib == TREND) then
      isFake = trend_phase_is_fake(T,P,Z,phase)
    else
      isFake = .false.
    endif
  end function phase_is_fake

end module thermo_utils
