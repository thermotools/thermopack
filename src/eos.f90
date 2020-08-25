!> Interface to thermodynamic models.
!! Currently ThermoPack and TREND equations of state are supported.
!!
!! \author MH, 2012-01-25
module eos
  use thermopack_constants
  use thermopack_var, only: nc, get_active_eos_container, eos_container, &
       get_active_eos, base_eos_param
  !
  implicit none
  save
  !
  ! Include TREND interface
  include 'trend_interface.f95'
  !
  private
  public :: thermo
  public :: zfac, entropy, enthalpy, specificVolume, pseudo
  public :: pseudo_safe
  public :: twoPhaseEnthalpy, twoPhaseEntropy, twoPhaseSpecificVolume
  public :: twoPhaseInternalEnergy
  public :: idealGibbsSingle, idealEntropySingle, idealEnthalpySingle
  public :: moleWeight, compMoleWeight, getCriticalParam
  public :: residualGibbs
  !
contains

  !----------------------------------------------------------------------
  !> Calculate fugasity coefficient and differentials given composition,
  !> temperature and pressure
  !>
  !> \author MH, 2012-01-27
  !----------------------------------------------------------------------
  subroutine thermo(t,p,z,phase,lnfug,lnfugt,lnfugp,lnfugx,ophase,metaExtremum,v)
    use single_phase, only: TP_CalcFugacity, TP_CalcZfac
    use trend_solver, only: trend_density, useRUBdensitySolver
    use numconstants, only: machine_prec
    implicit none
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
    real, optional, intent(out)                       :: v !< Specific volume [mol/m3]
    ! Locals
    real, dimension(1:nc) :: x, lnfug2, lnfugt2, lnfugp2
    real, dimension(nc*(nc+3)) :: tp_fug, g_tp_fug
    real, dimension(nc,nc) :: lnfugx2
    real :: gLiq, gVap, rho, v2, rhoLiq, rhoVap
    integer :: gflag_opt, ph
    integer :: phase_found_liq, phase_found_vap
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !
    !--------------------------------------------------------------------
    x = z(1:nc)/sum(z(1:nc))

    ph = phase
    if (ph == SINGLEPH) then
      ph = LIQPH
    endif

    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      p_act_eos => get_active_eos()
      gflag_opt = 1
      if (present(metaExtremum)) then
        if (metaExtremum) then
          gflag_opt = 2
        endif
      endif
      if (ph == LIQPH .OR. ph == VAPPH) then
        call TP_CalcFugacity(nc,p_act_eosc%comps,p_act_eos,&
             t,p,x,ph,tp_fug,lnfugt,lnfugp,lnfugx,gflag_opt,v=v)
      else if (ph == MINGIBBSPH) then
        call TP_CalcFugacity(nc,p_act_eosc%comps,p_act_eos,&
             t,p,x,LIQPH,g_tp_fug,lnfugt,lnfugp,lnfugx,gflag_opt,v=v)
        if (present(lnfugt)) then
          lnfugt2 = lnfugt
        endif
        if (present(lnfugp)) then
          lnfugp2 = lnfugp
        endif
        if (present(lnfugx)) then
          lnfugx2 = lnfugx
        endif
        if (present(v)) then
          v2 = v
        endif
        call TP_CalcFugacity(nc,p_act_eosc%comps,p_act_eos,&
             t,p,x,VAPPH,tp_fug,lnfugt,lnfugp,lnfugx,gflag_opt,v=v)

        gVap = sum(x*tp_fug(1:nc))
        gLiq = sum(x*g_tp_fug(1:nc))
        if (present(ophase)) then
          ophase = VAPPH
        endif

        if (abs(gLiq-gVap)/abs(gLiq) < 100.0*machine_prec) then
          if (present(ophase)) then
            ophase = SINGLEPH
          endif
        else if (gLiq < gVap) then
          if (present(ophase)) then
            ophase = LIQPH
          endif
          tp_fug(1:nc) = g_tp_fug(1:nc)
          if (present(lnfugt)) then
            lnfugt = lnfugt2
          endif
          if (present(lnfugp)) then
            lnfugp = lnfugp2
          endif
          if (present(lnfugx)) then
            lnfugx = lnfugx2
          endif
          if (present(v)) then
            v = v2
          endif
        endif
      endif
      lnfug = tp_fug(1:nc)
    case (TREND)
      ! TREND
      if (ph == LIQPH .OR. ph == VAPPH) then
        call trend_density(t,p,x,ph, rho, metaextr=metaExtremum)
        call trend_thermo_dens(t,rho,x,lnfug,lnfugt,lnfugp,lnfugx)
        if (present(v)) then
          v = 1.0/rho
        endif
      else if (ph == MINGIBBSPH) then
        call trend_density(t,p,x,LIQPH, rho, phase_found_liq, metaextr=metaExtremum)
        if (rho<0.0) then
          call stoperror("Error in eos::thermo:" // &
               " trend_density did not find a valid density.")
        end if
        call trend_thermo_dens(t,rho,x,lnfug2,lnfugt,lnfugp,lnfugx)
        if (present(lnfugp)) then
          lnfugp2 = lnfugp
        endif
        if (present(lnfugt)) then
          lnfugt2 = lnfugt
        endif
        if (present(lnfugx)) then
          lnfugx2 = lnfugx
        endif
        rhoLiq = rho
        call trend_density(t,p,x,VAPPH, rho, phase_found_vap, metaextr=metaExtremum)
        rhoVap = rho
        if (rho<0.0) then
          call stoperror("Error in eos::thermo:" // &
               " trend_density did not find a valid density.")
        end if
        call trend_thermo_dens(t,rho,x,lnfug,lnfugt,lnfugp,lnfugx)
        gVap = sum(x*lnfug)
        gLiq = sum(x*lnfug2)

        if (useRUBdensitySolver) then
          ! Have we found one or two roots?
          if ((abs(rhoLiq-rhoVap) < 1.0e5*abs(rhoVap)*machine_prec)) then
            ! Most likely same root
            phase_found_vap = phase_found_liq
          endif
        endif
        ! Finding phase of minimum gibbs
        if (phase_found_vap == phase_found_liq) then
          ! Found only a single density solution.
          if (present(ophase)) ophase = phase_found_liq
        else
          ! Found two different density solutions
          if (gLiq < gVap) then
            ! Liquid had lowest gibbs
            if (present(ophase)) ophase = LIQPH
            lnfug = lnfug2
            if (present(lnfugp)) lnfugp = lnfugp2
            if (present(lnfugt)) lnfugt = lnfugt2
            if (present(lnfugx)) lnfugx = lnfugx2
            if (present(v)) rho = rhoLiq
          else
            ! Vapor had lowest gibbs
            if (present(ophase)) ophase = VAPPH
          endif
        endif
        if (present(v)) then
          v = 1.0/rho
        endif
      endif
    case default
      write(*,*) 'EoSlib error in eos::thermo: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine thermo

  !----------------------------------------------------------------------
  !> Calculate single-phase compressibility factor given composition,
  !> temperature and pressure
  !>
  !> \author MH, 2012-03-20
  !----------------------------------------------------------------------
  subroutine zfac(t,p,x,phase,z,dzdt,dzdp,dzdx)
    use single_phase, only: TP_CalcZfac
    use trend_solver, only: trend_density
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: x !< Compozition
    real, intent(out) :: z !< Compressibillity factor
    real, optional, intent(out) :: dzdt !< 1/K - Compressibillity factor differential wrpt. temperature
    real, optional, intent(out) :: dzdp !< 1/Pa - Compressibillity factor differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dzdx !< Compressibillity factor differential wrpt. mole numbers
    ! Locals
    real, dimension(1:nc) :: xx
    real                  :: rho
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !
    !--------------------------------------------------------------------
    xx = x(1:nc)/sum(x(1:nc))
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      p_act_eosc => get_active_eos_container()
      z = TP_CalcZfac(nc,p_act_eosc%comps,p_act_eos,&
           t,p,xx,phase,1,dzdt,dzdp,dzdx)
    case (TREND)
      ! TREND
      call trend_density(T, p, xx, phase, rho)
      call trend_zfac(T, rho, xx, z, dzdt, dzdp, dzdx)
    case default
      write(*,*) 'EoSlib error in eos::zfac: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine zfac

  !----------------------------------------------------------------------
  !> Calculate single-phase specific volume given composition, temperature and
  !> pressure
  !>
  !> \author MH, 2012-07-06
  !----------------------------------------------------------------------
  subroutine specificVolume(t,p,x,phase,v,dvdt,dvdp,dvdx)
    use single_phase, only: TP_CalcZfac
    use trend_solver, only: trend_density
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: x !< Compozition
    real, intent(out) :: v !< m3/mol - Specific volume
    real, optional, intent(out) :: dvdt !< m3/mol/K - Specific volume differential wrpt. temperature
    real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specific volume differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dvdx !< m3/mol - Specific volume differential wrpt. mole numbers
    ! Locals
    real, dimension(1:nc) :: xx
    real :: rho, z
    integer :: ph
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !
    !--------------------------------------------------------------------
    xx = x(1:nc)/sum(x(1:nc))
    p_act_eosc => get_active_eos_container()
    ph = phase
    if (ph == SINGLEPH) then
      ph = LIQPH
    endif

    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      p_act_eos => get_active_eos()
      z = TP_CalcZfac(nc,p_act_eosc%comps,p_act_eos,&
           t,p,xx,ph,1,dvdt,dvdp,dvdx)
      v = z*Rgas*t/p
      if (present(dvdt)) then
        dvdt = dvdt*Rgas*t/p + v/t
      endif
      if (present(dvdp)) then
        dvdp = dvdp*Rgas*t/p - v/p
      endif
      if (present(dvdx)) then
        dvdx = dvdx*Rgas*t/p + z*Rgas*t/p
      endif
    case (TREND)
      ! trend
      call trend_density(t,p,xx,ph, rho)
      v = 1.0/rho
      call trend_specificVolume(t,xx,v,dvdt,dvdp,dvdx)
    case default
      write(*,*) 'EoSlib error in eos::specificVolume: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine specificVolume

  !----------------------------------------------------------------------
  !> Calculate gas-liquid or single-phase specific volume given composition,
  !> temperature and pressure
  !>
  !> \author MH, 2012-07-30
  !----------------------------------------------------------------------
  function twoPhaseSpecificVolume(t,p,z,x,y,beta,phase,betaL) result(v)
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, intent(in) :: beta !< Gas phase mole fraction
    real, dimension(1:nc), intent(in) :: x !< Liquid compozition
    real, dimension(1:nc), intent(in) :: y !< Gas compozition
    real, dimension(1:nc), intent(in) :: z !< Overall compozition
    real :: v !< m3/mol - Specific mixture volume
    real, optional, intent(in) :: betaL !< Liquid phase mole fraction
    ! Locals
    real :: vg, vl
    integer :: sphase
    !--------------------------------------------------------------------
    if (phase == TWOPH) then
      call specificVolume(t,p,x,LIQPH,vl)
      call specificVolume(t,p,y,VAPPH,vg)
      if (present(betaL)) then
        v = beta*vg+betaL*vl
      else
        v = beta*vg+(1-beta)*vl
      endif
    else
      sphase = phase
      if (phase == SINGLEPH) then ! Only one solution of EoS
        sphase = LIQPH
      endif
      call specificVolume(t,p,z,sphase,v)
    endif
  end function twoPhaseSpecificVolume

  !----------------------------------------------------------------------
  !> Calculate single-phase specific enthalpy given composition, temperature
  !> and pressure
  !>
  !> \author MH, 2012-03-20
  !----------------------------------------------------------------------
  subroutine enthalpy(t,p,x,phase,h,dhdt,dhdp,dhdx,residual)
    use single_phase, only: TP_CalcEnthalpy
    use trend_solver, only: trend_density
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: x !< Compozition
    real, intent(out) :: h !< J/mol - Specific enthalpy
    real, optional, intent(out) :: dhdt !< J/mol/K - Specific enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdp !< J/mol/Pa - Specific enthalpy differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dhdx !< J/mol - Specific enthalpy differential wrpt. mole numbers
    logical, optional, intent(in) :: residual !< Set to true if only residual entropy is required
    ! Locals
    real, dimension(1:nc) :: xx
    real :: rho
    integer :: ph
    logical :: res
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !
    !--------------------------------------------------------------------
    res = .false.
    if (present(residual)) then
      res = residual
    endif
    xx = x(1:nc)/sum(x(1:nc))
    p_act_eosc => get_active_eos_container()
    ph = phase
    if (ph == SINGLEPH) then
      ph = LIQPH
    endif
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      p_act_eos => get_active_eos()
      h = TP_CalcEnthalpy(nc,p_act_eosc%comps,p_act_eos,&
           t,p,xx,ph,res,dhdt,dhdp,dhdx)
    case (TREND)
      ! TREND
      call trend_density(t,p,xx,ph,rho)
      if (res) then
        call trend_residualenthalpy_tp(t,P,rho,xx,h,dhdt,dhdp,dhdx)
      else
        call trend_enthalpy(t,rho,xx,h,dhdt,dhdp,dhdx)
      endif
    case default
      write(*,*) 'EoSlib error in eos::enthalpy: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine enthalpy

  !----------------------------------------------------------------------
  !> Calculate gas-liquid or single-phase specific enthalpy given composition,
  !> temperature and pressure
  !>
  !> \author MH, 2012-03-20
  !----------------------------------------------------------------------
  function twoPhaseEnthalpy(t,p,z,x,y,beta,phase,betaL) result(h)
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, intent(in) :: beta !< Gas phase mole fraction
    real, dimension(1:nc), intent(in) :: x !< Liquid compozition
    real, dimension(1:nc), intent(in) :: y !< Gas compozition
    real, dimension(1:nc), intent(in) :: z !< Overall compozition
    real, optional, intent(in) :: betaL !< Liquid phase mole fraction
    real :: h !< J/mol - Specific mixture enthalpy
    ! Locals
    real :: hg, hl
    !--------------------------------------------------------------------
    if (phase == TWOPH) then
      call enthalpy(t,p,y,VAPPH,hg)
      call enthalpy(t,p,x,LIQPH,hl)
      if (present(betaL)) then
        h = beta*hg+betaL*hl
      else
        h = beta*hg+(1.0-beta)*hl
      endif
    else
      if (phase == VAPPH) then
        call enthalpy(t,p,z,VAPPH,h)
      else
        call enthalpy(t,p,z,LIQPH,h)
      endif
    endif

  end function twoPhaseEnthalpy

  !----------------------------------------------------------------------
  !> Calculate single-phase specific entropy given composition, temperature
  !> and pressure
  !>
  !> \author MH, 2012-03-20
  !----------------------------------------------------------------------
  subroutine entropy(t,p,x,phase,s,dsdt,dsdp,dsdx,residual)
    use single_phase, only: TP_CalcEntropy
    use trend_solver, only: trend_density
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: x !< Compozition
    real, intent(out) :: s !< J/mol/K - Specific entropy
    real, optional, intent(out) :: dsdt !< J/mol/K/K - Specific entropy differential wrpt. temperature
    real, optional, intent(out) :: dsdp !< J/mol/K/Pa - Specific entropy differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dsdx !< J/mol/K - Specific entropy differential wrpt. mole numbers
    logical, optional, intent(in) :: residual !< Set to true if only residual entropy is required
    ! Locals
    real, dimension(1:nc) :: xx
    real :: rho
    integer :: ph
    logical :: res
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !
    !--------------------------------------------------------------------
    res = .false.
    if (present(residual)) then
      res = residual
    endif
    xx = x(1:nc)/sum(x(1:nc))
    p_act_eosc => get_active_eos_container()
    ph = phase
    if (ph == SINGLEPH) then
      ph = LIQPH
    endif

    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      p_act_eos => get_active_eos()
      s = TP_CalcEntropy(nc,p_act_eosc%comps,p_act_eos,&
           t,p,xx,ph,res,dsdt,dsdp,dsdx)
    case (TREND)
      ! TREND
      call trend_density(t,p,xx,ph, rho)
      if (res) then
        call trend_residualentropy_tp(t,P,rho,xx,s,dsdt,dsdp,dsdx)
      else
        call trend_entropy(t,rho,xx,s,dsdt,dsdp,dsdx)
      endif
    case default
      write(*,*) 'EoSlib error in eos::entropy: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine entropy

  !----------------------------------------------------------------------
  !> Calculate gas-liquid or single-phase specific entropy given composition,
  !> temperature and pressure
  !>
  !> \author MH, 2012-03-20
  !----------------------------------------------------------------------
  function twoPhaseEntropy(t,p,z,x,y,beta,phase,betaL) result(s)
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, intent(in) :: beta !< Gas phase mole fraction
    real, dimension(1:nc), intent(in) :: x !< Liquid compozition
    real, dimension(1:nc), intent(in) :: y !< Gas compozition
    real, dimension(1:nc), intent(in) :: z !< Overall compozition
    real, optional, intent(in) :: betaL !< Liquid phase mole fraction
    real :: s !< J/mol/K - Specific mixture entropy
    ! Locals
    real :: sg, sl
    !
    !--------------------------------------------------------------------
    if (phase == TWOPH) then
      call entropy(t,p,y,VAPPH,sg)
      call entropy(t,p,x,LIQPH,sl)
      if (present(betaL)) then
        s = beta*sg+betaL*sl
      else
        s = beta*sg+(1.0-beta)*sl
      endif
    else
      if (phase == VAPPH) then
        call entropy(t,p,z,VAPPH,s)
      else
        call entropy(t,p,z,LIQPH,s)
      endif
    endif
  end function twoPhaseEntropy

  !----------------------------------------------------------------------
  !> Calculate gas-liquid or single-phase internal energy given composition,
  !> temperature, pressure and a phase state given by phase/beta.
  !>
  !> \author EA, 2014-09
  !----------------------------------------------------------------------
  function twoPhaseInternalEnergy(t,p,z,x,y,beta,phase,betaL) result(u)
    use eosTV, only: internal_energy
    implicit none
    ! Input:
    real, intent(in)                  :: t !< K - Temperature
    real, intent(in)                  :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Overall compozition
    real, dimension(1:nc), intent(in) :: x !< Liquid compozition
    real, dimension(1:nc), intent(in) :: y !< Gas compozition
    real, intent(in)                  :: beta !< Gas phase mole fraction
    integer, intent(in)               :: phase !< Phase identifier
    real, optional, intent(in)        :: betaL !< Liquid phase mole fraction
    ! Output:
    real                              :: u !< J/mol - Specific internal energy
    ! Internal:
    real :: vg, ug, vl, ul, v
    integer :: sphase
    !--------------------------------------------------------------------
    if (phase == TWOPH) then
      ! Liquid phase
      call specificVolume(t,p,x,LIQPH,vl)
      call internal_energy(t,vl,x,ul)
      ! Gas phase
      call specificVolume(t,p,y,VAPPH,vg)
      call internal_energy(t,vg,y,ug)
      ! Combined
      if (present(betaL)) then
        u = beta*ug+betaL*ul
      else
        u = beta*ug+(1-beta)*ul
      endif
    else
      sphase = phase
      if (phase == SINGLEPH) then ! Only one solution of EoS
        sphase = LIQPH
      endif
      call specificVolume(t,p,z,sphase,v)
      call internal_energy(t,v,z,u)
    endif
  end function twoPhaseInternalEnergy


  !----------------------------------------------------------------------
  !> Calculate pseudo critical point.
  !>
  !> \author MH, 2012-03-15
  !----------------------------------------------------------------------
  subroutine pseudo(x,tpc,ppc,acfpc,zpc,vpc)
    use single_phase, only: TP_CalcPseudo
    use cubic_eos, only: cb_eos
    implicit none
    ! Transferred variables
    real, dimension(1:nc), intent(in) :: x !< Compozition
    real, intent(out) :: tpc !< K - Pseudo critical temperature
    real, intent(out) :: vpc !< m3/mol - Pseudo critical specific volume
    real, intent(out) :: ppc !< Pa - Pseudo critical pressure
    real, intent(out) :: zpc !< - - Pseudo critical compressibillity
    ! Locals
    real, dimension(1:nc) :: xx
    real :: acfpc
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !--------------------------------------------------------------------
    xx = x(1:nc)/sum(x(1:nc))
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      ! Note! SRK is used for LK
      p_act_eos => get_active_eos()
      select type(p_eos => p_act_eos)
      class is (cb_eos)
        call TP_CalcPseudo(nc,p_act_eosc%comps,p_eos,xx,tpc,ppc,zpc,vpc)
      class default
        call stoperror("pseudo error")
      end select

      vpc = vpc * 1.0e-3 ! m3/kmol -> m3/mol
    case (TREND)
      ! TREND
      write(*,*) 'eos::pseudo: TREND not implemented'
      call stoperror('')
    case default
      write(*,*) 'EoSlib error in eos::pseudo: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine pseudo

  !----------------------------------------------------------------------
  !> Calculate pseudo critical point, or use estimate from
  !> alternative EoS if necessary.
  !>
  !> \author EA, 2015-01
  !----------------------------------------------------------------------
  subroutine pseudo_safe(x,tpc,ppc,zpc,vpc)
    use thermopack_var, only: nc, get_active_alt_eos
    use single_phase, only: TP_CalcPseudo
    use cubic_eos, only: cb_eos
    implicit none
    ! Input:
    real, dimension(1:nc), intent(in) :: x !< Compozition
    ! Output:
    real, intent(out) :: tpc !< K - Pseudo critical temperature
    real, intent(out) :: vpc !< m3/mol - Pseudo critical specific volume
    real, intent(out) :: ppc !< Pa - Pseudo critical pressure
    real, intent(out) :: zpc !< - - Pseudo critical compressibillity
    ! Locals
    real              :: acfpc
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos

    p_act_eosc => get_active_eos_container()

    if (p_act_eosc%need_alternative_eos) then
      p_act_eos => get_active_alt_eos()
      select type(p_eos => p_act_eos)
      class is (cb_eos)
        call TP_CalcPseudo(nc,p_act_eosc%comps,p_eos,x,tpc,ppc,zpc,vpc)
      class default
        call stoperror("pseudo_safe error")
      end select
      vpc = vpc * 1.0e-3 ! m3/kmol -> m3/mol
    else
      call pseudo(x,tpc,ppc,acfpc,zpc,vpc)
    endif

  end subroutine pseudo_safe

  !----------------------------------------------------------------------
  !> Get critical state (and more) of pure fluid
  !!
  !! \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine getCriticalParam(i,tci,pci,oi,vci,tnbi)
    use thermopack_constants, only: Rgas
    use eosdata
    implicit none
    integer, intent(in) :: i !< Component index
    real, intent(out) :: tci !< K - Critical temperature
    real, intent(out) :: pci !< Pa - Critical pressure
    real, intent(out) :: oi  !< Acentric factor
    real, intent(out), optional :: vci  !< m3/mol - Critical volume
    real, intent(out), optional :: tnbi !< Normal boiling point  (gs)
    ! Locals
    type(eos_container), pointer :: p_act_eosc
    p_act_eosc => get_active_eos_container()
    if ( present(vci) .and. p_act_eosc%EosLib /= THERMOPACK .and. p_act_eosc%EosLib /= TREND) then
       print *, "Current EoSlib: ", p_act_eosc%EosLib
       call stoperror("Can only get vc if ThermoPack or TREND library is active.")
    end if
    ! Locals
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      tci = p_act_eosc%comps(i)%p_comp%tc
      pci = p_act_eosc%comps(i)%p_comp%pc
      oi = p_act_eosc%comps(i)%p_comp%acf
      if (present(tnbi)) then
        tnbi = p_act_eosc%comps(i)%p_comp%tb
      endif
      if (present(vci)) then
        vci = (p_act_eosc%comps(i)%p_comp%zc)*Rgas*tci/pci
      end if
    case (TREND)
      ! TREND
      call trend_getcrit(i,tci,pci,oi,vci,tnbi)
    case default
      write(*,*) 'eos::getCriticalParam: EoSlib error: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select

  end subroutine getCriticalParam

  !----------------------------------------------------------------------
  !> Calculate single component ideal Gibbs energy.
  !> Unit: J/mol
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine idealGibbsSingle(t,p,j,g,dgdt,dgdp)
    use ideal, only: Hideal_apparent, TP_Sideal_apparent
    implicit none
    ! Transferred variables
    real, intent(in) :: t                   !< K - Temperature
    real, intent(in) :: p                   !< Pa - Pressure
    integer, intent(in) :: j                !< Component index
    real, intent(out) :: g                  !< J/mol - Ideal Gibbs energy
    real, optional, intent(out) :: dgdt     !< J/mol/K - Temperature differential of ideal Gibbs energy
    real, optional, intent(out) :: dgdp     !< J/mol/Pa - Pressure differential of ideal Gibbs energy
    ! Locals
    real :: s, h, z(nc)
    type(eos_container), pointer :: p_act_eosc
    !--------------------------------------------------------------------
    !
    s = 0.0
    h = 0.0
    z = 0.0
    z(j) = 1.0
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      h = Hideal_apparent(p_act_eosc%comps,j,T)
      call TP_Sideal_apparent(p_act_eosc%comps, j, T, P, s)
    case (TREND)
      ! TREND
      h = trend_ideal_enthalpy(T,j)
      s = trend_ideal_entropy(T,P,j)
    case default
      write(*,*) 'EosLib error in eos::idealGibbsSingle: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
    g = h - T*s
    if (present(dgdt)) then
      dgdt = -s
    end if
    if (present(dgdp)) then
      dgdp=T*Rgas/P
    end if
  end subroutine idealGibbsSingle

  !----------------------------------------------------------------------
  !> Calculate single component ideal entropy.
  !> Unit: J/mol/K
  !>
  !> \author MH, 2014-01
  !----------------------------------------------------------------------
  subroutine idealEntropySingle(t,p,j,s,dsdt,dsdp)
    use ideal, only: TP_Sideal_apparent
    implicit none
    ! Transferred variables
    real, intent(in) :: t                   !< K - Temperature
    real, intent(in) :: p                   !< Pa - Pressure
    integer, intent(in) :: j                !< Component index
    real, intent(out) :: s                  !< J/mol/K - Ideal entropy
    real, optional, intent(out) :: dsdt     !< J/mol/K^2 - Temperature differential of ideal entropy
    real, optional, intent(out) :: dsdp     !< J/mol/Pa - Pressure differential of ideal entopy
    ! Locals
    real :: z(nc)
    type(eos_container), pointer :: p_act_eosc
    !--------------------------------------------------------------------
    !
    z = 0.0
    z(j) = 1.0
    s = 0.0
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      call TP_Sideal_apparent(p_act_eosc%comps, j, T, P, s, dsdt)
    case (TREND)
      ! TREND
      s = trend_ideal_entropy(T,P,j)
      if (present(dsdt)) then
        dsdt = trend_ideal_Cp(T,j) / T ! J/mol/K^2
      end if
    case default
      write(*,*) 'EoSlib error in eos::idealEntropySingle: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
    if (present(dsdp)) then
      dsdp=-Rgas/P
    end if
  end subroutine idealEntropySingle

  !----------------------------------------------------------------------
  !> Calculate single component ideal enthalpy.
  !> Unit: J/mol
  !>
  !> \author MH, 2014-01
  !----------------------------------------------------------------------
  subroutine idealEnthalpySingle(t,p,j,h,dhdt,dhdp)
    use ideal, only: Hideal_apparent, Cpideal_apparent
    implicit none
    ! Transferred variables
    real, intent(in) :: t                   !< K - Temperature
    real, intent(in) :: p                   !< Pa - Pressure
    integer, intent(in) :: j                !< Component index
    real, intent(out) :: h                  !< J/mol - Ideal enthalpy
    real, optional, intent(out) :: dhdt     !< J/mol/K - Temperature differential of ideal enthalpy
    real, optional, intent(out) :: dhdp     !< J/mol/Pa - Pressure differential of ideal enthalpy
    ! Locals
    type(eos_container), pointer :: p_act_eosc
    !--------------------------------------------------------------------
    !
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      h = Hideal_apparent(p_act_eosc%comps,j,T)
    case (TREND)
      ! TREND
      h = trend_ideal_enthalpy(T,j)
      if (present(dhdt)) then
        dhdt = trend_ideal_Cp(T,j) ! J/mol/K^2
      end if
    case default
      write(*,*) 'EoSlib error in eos::idealEnthalpySingle: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
    if (present(dhdp)) then
      dhdp=0.0
    end if
  end subroutine idealEnthalpySingle

  !----------------------------------------------------------------------
  !> Calculate residual Gibbs energy.
  !> Unit: J/mol
  !>
  !> \author MH, 2013-10-17
  !----------------------------------------------------------------------
  subroutine residualGibbs(t,p,z,phase,gr,dgrdt,dgrdp,dgrdn,metaExtremum)
    use single_phase, only: TP_CalcGibbs
    use trend_solver, only: trend_density
    implicit none
    ! Transferred variables
    real, intent(in) :: t                    !< K - Temperature
    real, intent(in) :: p                    !< Pa - Pressure
    real, dimension(nc), intent(in) :: z     !< Component fractions
    integer, intent(in) :: phase             !< Phase identifyer
    real, intent(out) :: gr                  !< J/mol - Residual Gibbs energy
    real, optional, intent(out) :: dgrdt     !< J/mol/K - Temperature differential of ideal Gibbs energy
    real, optional, intent(out) :: dgrdp     !< J/mol/Pa - Pressure differential of ideal Gibbs energy
    real, dimension(nc), optional, intent(out) :: dgrdn     !< J/mol^2 - Mole number differential of ideal Gibbs energy
    logical, optional, intent(in) :: metaExtremum !< Calculate phase properties at metastable extremum
    ! Locals
    integer :: gflag
    real, dimension(nc) :: xx
    real                :: rho
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos
    !--------------------------------------------------------------------
    !
    xx = z(1:nc)/sum(z(1:nc))
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      p_act_eos => get_active_eos()
      gflag = 1
      if (present(metaExtremum)) then
        if (metaExtremum) then
          gflag = 2
        endif
      endif
      call TP_CalcGibbs(nc,p_act_eosc%comps,p_act_eos,&
           T,P,xx,phase,.true.,gr,dgrdt,dgrdp,dgrdn,gflag)
    case (TREND)
      ! TREND
      if (present(metaExtremum)) then
        write(*,*) 'eos::residualGibbs: metaExtremum not implemented for TREND.'
        call stoperror('')
      endif
      call trend_density(T, p, xx, phase, rho)
      call trend_residualgibbs_tp(T, P, rho, xx, gr, dgrdt, dgrdp)
    case default
      write(*,*) 'EoSlib error in eos::residualGibbs: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end subroutine residualGibbs

  !----------------------------------------------------------------------
  !> Get mole weight.
  !> Unit: g/mol
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  function moleWeight(z) result(mw)
    use single_phase, only: TP_CalcMw
    implicit none
    ! Transferred variables
    real, dimension(1:nc), intent(in) :: z !< Composition
    real :: mw !< g/mol - Mole weight
    ! Locals
    type(eos_container), pointer :: p_act_eosc
    ! External
    !--------------------------------------------------------------------
    !
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      mw = TP_CalcMw(nc,p_act_eosc%comps,z)
    case (TREND)
      mw = trend_moleWeight(z)
    case default
      write(*,*) 'EoSlib error in eos::moleWeight: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end function moleWeight

  !----------------------------------------------------------------------
  !> Get component mole weight.
  !> Unit: g/mol
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  function compMoleWeight(j) result(mw)
    implicit none
    ! Transferred variables
    integer, intent(in) :: j !< Component index
    real :: mw !< g/mol - Mole weight
    ! Locals
    type(eos_container), pointer :: p_act_eosc
    !--------------------------------------------------------------------
    !
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      mw = p_act_eosc%comps(j)%p_comp%mw
    case (TREND)
      mw = trend_compMoleWeight(j)
    case default
      write(*,*) 'EoSlib error in eos::compMoleWeight: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end function compMoleWeight

  !----------------------------------------------------------------------
  !> Get estimation of pure component saturation pressure
  !> Unit: Pa
  !>
  !> \author MH, 2013-10-10
  !----------------------------------------------------------------------
  function PsatEstPure(j,T) result(psat)
    use ideal, only: EstPsat
    implicit none
    ! Transferred variables
    integer, intent(in) :: j !< Component index
    real, intent(in) :: T !< K - Temperature
    real :: psat !< Pa - Estimated saturation pressure
    type(eos_container), pointer :: p_act_eosc
    !--------------------------------------------------------------------
    !
    psat = 0.0
    p_act_eosc => get_active_eos_container()
    select case (p_act_eosc%EosLib)
    case (THERMOPACK)
      ! Thermopack
      psat = EstPsat(p_act_eosc%comps,j,T)
    case (TREND)
      write(*,*) 'eos::PsatEstPure: TREND not implemented'
    case default
      write(*,*) 'EoSlib error in eos::PsatEstPure: No such EoS libray:',p_act_eosc%EosLib
      call stoperror('')
    end select
  end function PsatEstPure

  !----------------------------------------------------------------------
  !> Returns true if a fallback-EoS is sometimes needed due to incomplete
  !> features, slow speed, or other reasons.
  !>
  !> Will return false for the full-featured cubic EoS in EoSlib THERMOPACK
  !>
  !> \author EA, 2014-08
  !----------------------------------------------------------------------
  ! function need_alternative_eos() result(needalt)
  !   use thermopack_constants, only: THERMOPACK
  !   use eosdata
  !   !$ use omp_lib, only: omp_get_thread_num
  !   implicit none
  !   ! Output:
  !   logical         :: needalt
  !   ! Internal
  !   integer         ::  i_cbeos

  !   needalt = .false.
  !   if (p_act_eos%EosLib == THERMOPACK) then
  !     i_cbeos = 1
  !     !$ i_cbeos = 1 + omp_get_thread_num()
  !     if (cbeos(i_cbeos)%eosidx == eosLK .OR. &
  !          cbeos(i_cbeos)%eosidx == cpaSRK .OR. &
  !          cbeos(i_cbeos)%eosidx == cpaPR .OR. &
  !          cbeos(i_cbeos)%eosidx == cspSRK .OR. &
  !          cbeos(i_cbeos)%eosidx == cspSRKGB .OR. &
  !          cbeos(i_cbeos)%eosidx == cspPR .OR. &
  !          cbeos(i_cbeos)%eosidx == eosPC_SAFT .OR. &
  !          cbeos(i_cbeos)%eosidx == eosPeTS .OR. &
  !          cbeos(i_cbeos)%eosidx == eosBH_pert .OR. &
  !          cbeos(i_cbeos)%eosidx == meosNist_mix .OR. &
  !          cbeos(i_cbeos)%eosidx == eos_single) then
  !       ! Lee Kesler is not cubic, and lacks some features.
  !       needalt = .true.
  !     endif
  !   else
  !     ! Not Thermopack
  !     needalt = .true.
  !   endif
  ! end function need_alternative_eos

end module eos
