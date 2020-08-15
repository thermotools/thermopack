!> Initialize thermodynamic models
!!
!! \author MH, 2014-02
module eoslibinit
  !
  use thermopack_var, only: nce, get_active_eos_container, eos_container, &
       get_active_alt_eos, base_eos_param, add_eos, &
       active_eos_container_is_associated, numAssocSites
  use eos_containers, only: allocate_eos
  implicit none
  save
  !
  logical :: silent_init = .false. ! Disable output for unit testing
  !
  ! Include TREND interface
  include 'trend_interface.f95'
  !
  private
  public :: init_thermo
  public :: init_cubic, init_cpa, init_saftvrmie, init_pcsaft
  public :: silent_init
  !public :: cleanup_eos
  public :: redefine_critical_parameters
  !
contains

  ! EOSLIBINIT.F90
  ! init_cubic(compstr, eos, mixrule, alpha)
  ! init_cpa(comp_string)
  ! init_quantum_cubic(comp_string)
  ! init_corresponding_states(comp_string, ref_comp, ref_eos, shape_eos)
  ! init_multiparameter(comp_string, eos)
  ! init_pcsaft(comp_string)
  ! init_saftvrmie(comp_string)
  ! init_quantum_saftvrmie(comp_string)
  ! init_leekesler(comp_string)
  ! init_solid
  ! init_legacy_interface -> init_thermo
  !
  ! TUNING.F90
  ! set_cubic_kij()
  ! ...


  !----------------------------------------------------------------------
  !> Set method to discriminate between liquid and vapor in case of an
  !! undefined single phase
  !!
  !! \author MH, 2020-06
  !----------------------------------------------------------------------
  subroutine set_liq_vap_discr_method(liq_vap_discr_method)
    ! Method information
    integer, intent(in) :: liq_vap_discr_method !< Method to discriminate between liquid and vapor in case of an undefined single phase.
    type(eos_container), pointer :: p_act_eos
    p_act_eos => get_active_eos_container()

    ! Method for discriminating between liquid/vapor when poorly defined
    p_act_eos%liq_vap_discr_method = liq_vap_discr_method
  end subroutine set_liq_vap_discr_method

  !> Initialize volume translation
  !>
  !> \author MH, 2020
  !----------------------------------------------------------------------
  subroutine init_volume_translation(volume_trans_model, param_ref)
    use volume_shift, only: InitVolumeShift
    character(len=*), intent(in) :: volume_trans_model   !< String model for volume translation
    character(len=*), intent(in) :: param_ref !< String defining parameter set
    !
    type(eos_container), pointer :: p_act_eosc
    integer :: i
    p_act_eosc => get_active_eos_container()

    p_act_eosc%eos(1)%p_eos%volumeShiftId = &
         InitVolumeShift(nce, p_act_eosc%comps, &
         volume_trans_model, p_act_eosc%eos(1)%p_eos%eosid)

    ! Distribute volumeShiftId
    do i=2,size(p_act_eosc%eos)
      p_act_eosc%eos(i)%p_eos%volumeShiftId = &
           p_act_eosc%eos(1)%p_eos%volumeShiftId
    enddo
  end subroutine init_volume_translation

  !----------------------------------------------------------------------
  !> Initialize thermo library
  !>
  !> \author MH, 2014-02
  !----------------------------------------------------------------------
  subroutine init_thermo(eos,mixing,alpha,comp_string,nphases,&
       liq_vap_discr_method_in,csp_eos,csp_ref_comp,kij_ref,alpha_ref,&
       saft_ref,b_exponent,TrendEosForCp,cptype,silent)
    use thermopack_constants, only: clen, TREND, THERMOPACK
    use cbselect,   only: SelectCubicEOS
    use compdata,   only: SelectComp, initCompList
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use stringmod,  only: uppercase, str_eq
    use eosdata,    only: cpaSRK, cpaPR, eosPC_SAFT, eosPeTS, eosBH_pert
    !$ use omp_lib, only: omp_get_max_threads
    ! Method information
    character(len=*), intent(in) :: eos    !< String defining equation of state
    character(len=*), intent(in) :: mixing !< String defining mixing rules
    character(len=*), intent(in) :: alpha  !< String defining alpha correlation
    character(len=*), intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
    integer, intent(in) :: nphases !< Number of phases
    integer, optional, intent(in) :: liq_vap_discr_method_in !< Method to discriminate between liquid and vapor in case of an undefined single phase. Will be set to none if absent.
    character(len=*), optional, intent(in) :: csp_eos !< Corrensponding state equation
    character(len=*), optional, intent(in) :: csp_ref_comp !< CSP component
    character(len=*), optional, intent(in) :: kij_ref, alpha_ref, saft_ref !< Data set numbers
    real, optional, intent(in) :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    character(len=*), optional, intent(in) :: TrendEosForCp !< Option to init trend for ideal gas properties.
    integer, optional, intent(in) :: cptype !< Type numbers for Cp
    logical, optional, intent(in) :: silent !< Option to disable init messages.
    ! Locals
    integer :: ncomp !< Number of components
    character(len=clen) :: message
    integer             :: i, ierr, index, ncbeos
    type(eos_container), pointer :: p_act_eosc
    if (present(silent)) then
      silent_init = silent
    endif

    if (.not. active_eos_container_is_associated()) then
      ! No eos_container have been allocated
      index = add_eos()
    endif
    p_act_eosc => get_active_eos_container()

    ! Component list.
    call initCompList(trim(uppercase(comp_string)),ncomp,p_act_eosc%complist)
    call allocate_eos(ncomp, eos)

    ! Method for discriminating between liquid/vapor when poorly defined
    if (present(liq_vap_discr_method_in)) then
      p_act_eosc%liq_vap_discr_method = liq_vap_discr_method_in
    end if

    ! Number of phases
    p_act_eosc%nph = nphases

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = p_act_eosc%nph
    complist => p_act_eosc%complist
    apparent => NULL()
    numAssocSites = 0

    ! Initialize components
    call SelectComp(complist,nce,"DEFAULT",p_act_eosc%comps,ierr)

    ! Set cptype
    if (present(cptype)) then
      do i=1,nc
        p_act_eosc%comps(i)%p_comp%id_cp%cptype = cptype
      enddo
    endif

    if (str_eq(eos, "EOSCG") .or. &
         str_eq(eos, "EOS-CG") .or. &
         str_eq(eos, "GERG2008") .or. &
         str_eq(eos, "GERG-2008") .or. &
         str_eq(eos, "EOSCG-GERG")) then
      p_act_eosc%eosLib = TREND
    else
      p_act_eosc%eosLib = THERMOPACK
    endif

    ! Initialize the selected EoS-library
    select case (p_act_eosc%eosLib)
    case (THERMOPACK)
      ! Initialize Thermopack
      call init_thermopack(trim(uppercase(eos)),trim(uppercase(mixing)), &
           trim(uppercase(alpha)), &
           nphases,csp_eos,csp_ref_comp, & ! csp_refcomp is case sensitive in compDB
           kij_ref,Alpha_ref,saft_ref,&
           b_exponent)
      if (present(TrendEosForCp)) then
        ! Initialize Trend for ideal properties
        call init_trend(trim(uppercase(TrendEosForCp)),ncomp,nphases,.false.)
      endif
    case (TREND)
      ! Initialize Trend
      call init_trend(trim(uppercase(eos)),ncomp,nphases,.true.)
    case default
      write(message,*) 'Wrong EoS library'
      call stoperror(trim(message))
    end select

    call init_fallback_and_redefine_criticals(silent_init)
  end subroutine init_thermo

  !----------------------------------------------------------------------------
  !> Initialize cubic fallback eos
  !----------------------------------------------------------------------------
  subroutine init_fallback_and_redefine_criticals(silent)
    use thermopack_constants, only: TREND, THERMOPACK
    use thermopack_var, only: nce
    use eosdata, only: isSAFTEOS
    use cbselect, only: selectCubicEOS, SelectMixingRules
    use cubic_eos, only: cb_eos
    !$ use omp_lib, only: omp_get_max_threads
    logical, intent(in) :: silent !< Option to disable init messages.
    ! Locals
    integer             :: err, ncbeos, i
    logical             :: isSAFTmodel
    real                :: Tci, Pci, oi

    type(eos_container), pointer :: p_act_eosc
    p_act_eosc => get_active_eos_container()

    if (.not. p_act_eosc%need_alternative_eos) return

    if (p_act_eosc%EoSLib == TREND) then
      ! Use TREND parameters to get better critical point in alternative model
      do i=1,nce
        call trend_getcrit(i,Tci,Pci,oi)
        p_act_eosc%comps(i)%p_comp%tc = Tci
        p_act_eosc%comps(i)%p_comp%pc = Pci
        p_act_eosc%comps(i)%p_comp%acf = oi
      enddo
    endif

    select type(p_eos => p_act_eosc%cubic_eos_alternative(1)%p_eos)
    type is (cb_eos)
      call SelectCubicEOS(nce, p_act_eosc%comps, p_eos, &
           "CLASSIC", "DEFAULT")

      call SelectMixingRules(nce, p_act_eosc%comps, p_eos, &
         "VDW", "DEFAULT")
    class default
      call stoperror("init_cubic: Should be cubic EOS")
    end select

    ! Distribute parameters from redefined eos
    do i=2,size(p_act_eosc%cubic_eos_alternative)
      p_act_eosc%cubic_eos_alternative(i)%p_eos = &
           p_act_eosc%cubic_eos_alternative(1)%p_eos
    enddo

    if (p_act_eosc%EoSLib == THERMOPACK .and. &
         isSAFTEOS(p_act_eosc%eos(1)%p_eos%eosidx)) then
      call redefine_critical_parameters(silent)
    endif
  end subroutine init_fallback_and_redefine_criticals

  !----------------------------------------------------------------------------
  !> Initialize cubic EoS. Use: call init_cubic('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cubic(comps,eos,mixing,alpha,param_ref)
    use compdata,   only: SelectComp, initCompList
    use thermopack_var, only: nc, nce, ncsym, complist, nph, apparent
    use thermopack_constants, only: THERMOPACK
    use stringmod,  only: uppercase
    use eos_containers, only: allocate_eos
    use cbselect, only: selectCubicEOS, SelectMixingRules
    use cubic_eos, only: cb_eos
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: eos     !< Equation of state
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    character(len=*), optional, intent(in) :: param_ref  !< Parameter reference
    ! Locals
    integer                          :: ncomp, i, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    character(len=100)               :: mixing_loc, alpha_loc, param_ref_loc
    type(eos_container), pointer     :: p_act_eosc

    if (.not. active_eos_container_is_associated()) then
      ! No eos_container have been allocated
      index = add_eos()
    endif
    p_act_eosc => get_active_eos_container()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,p_act_eosc%complist)
    !
    call allocate_eos(ncomp, eos)

    ! Number of phases
    p_act_eosc%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = p_act_eosc%nph
    complist => p_act_eosc%complist
    apparent => NULL()

    ! Set eos library identifyer
    p_act_eosc%eosLib = THERMOPACK

    ! Initialize components module
    call SelectComp(complist,nce,"DEFAULT",p_act_eosc%comps,ierr)

    ! Initialize Thermopack
    alpha_loc = "Classic"
    mixing_loc = "Classic"
    param_ref_loc = "Default"
    if (present(mixing)) mixing_loc = uppercase(mixing)
    if (present(alpha)) alpha_loc = uppercase(alpha)
    if (present(param_ref)) param_ref_loc = uppercase(param_ref)

    select type(p_eos => p_act_eosc%eos(1)%p_eos)
    type is (cb_eos)
      call SelectCubicEOS(nc, p_act_eosc%comps, &
           p_eos, alpha_loc, param_ref_loc)

      call SelectMixingRules(nc, p_act_eosc%comps, &
           p_eos, mixing_loc, param_ref_loc)
    class default
      call stoperror("init_cubic: Should be cubic EOS")
    end select
    ! Distribute parameters from redefined eos
    do i=2,size(p_act_eosc%eos)
      p_act_eosc%eos(i)%p_eos = p_act_eosc%eos(1)%p_eos
    enddo

  end subroutine init_cubic

  ! !----------------------------------------------------------------------------
  ! !> Initialize extended corresponding state EoS. Use: call init_extcsp
  ! !----------------------------------------------------------------------------
  ! subroutine init_extcsp(comps,eos,sh_eos,sh_mixing,sh_alpha,ref_eos,ref_comp)
  !   use parameters, only: initCompList
  !   use tpselect,   only: SelectComp
  !   use thermopack_var,      only: comp, nce, ncsym
  !   use stringmod,  only: uppercase
  !   !$ use omp_lib, only: omp_get_max_threads
  !   character(len=*), intent(in) :: comps !< Components. Comma or white-space
  !   !separated
  !   character(len=*), intent(in) :: eos     !< Equation of state
  !   character(len=*), optional, intent(in) :: mixing !< Mixing rule
  !   character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
  !   ! Locals
  !   integer                          :: ncomp
  !   character(len=len_trim(compstr)) :: compstr_upper
  !   character(len=100)               :: mixing_loc, alpha_loc

  !   ! Set component list
  !   compstr_upper=trim(uppercase(compstr))
  !   call initCompList(compstr_upper,ncomp)
  !   ncsym = ncomp

  !   ! Initialize components module
  !   call SelectComp(trim(compstr_upper),nce,comp)

  !   ! Initialize Thermopack
  !   alpha_loc = "Classic"
  !   mixing_loc = "Classic"
  !   if (present(mixing)) mixing_loc = uppercase(mixing)
  !   if (present(alpha)) alpha_loc = uppercase(alpha)
  !   call init_thermopack(trim(uppercase(eos)),trim(uppercase(mixing_loc)), &
  !        trim(uppercase(alpha_loc)), nphase=2, kij_setno=1,alpha_setno=1)

  ! end subroutine init_extcsp



  !----------------------------------------------------------------------
  !> Set critical parameters to represent actual model
  !>
  !> \author MH, 2019-03
  !----------------------------------------------------------------------
  subroutine redefine_critical_parameters(silent_init)
    use tpcbmix,      only: cbSingleCalcABC
    use thermopack_var, only: nce
    use eosTV,        only: pressure
    use critical,     only: calcCriticalTV
    use saturation,   only: acentricFactorEos
    use thermopack_constants,      only: tpTmin, Rgas
    use cbAlpha,      only: getAcentricAlphaParam
    use cubic_eos,    only: cb_eos
    logical, intent(in) :: silent_init
    ! Locals
    integer :: i, j, ierr
    real :: Tmin
    real :: Vc
    real :: Tc  !< Specified critical temperature [K]
    real :: Pc  !< Specified critical pressure [Pa]
    real :: Acf !< Specified acentric factor [-]
    real :: Z(nce)
    type(eos_container), pointer :: p_act_eosc
    class(base_eos_param), pointer :: p_act_eos

    p_act_eosc => get_active_eos_container()
    p_act_eos => get_active_alt_eos()
    Tmin = tpTmin
    tpTmin = 2.0
    do i=1,nce
      Z = 0
      Z(i) = 1
      Tc = -1.0
      Vc = -1.0
      call calcCriticalTV(Tc,Vc,Z,ierr)
      if (ierr /= 0 .and. .not. silent_init) then
        print *, 'Not able to redefine critical properties for component: ', &
             trim(p_act_eosc%comps(i)%p_comp%ident)
      else
        Pc = pressure(Tc,Vc,Z)
        select type (p_eos => p_act_eos)
        class is (cb_eos)
          p_eos%single(i)%Tc = Tc
          p_eos%single(i)%Pc = Pc
          call cbSingleCalcABC(nce,p_eos,i)
          p_act_eosc%comps(i)%p_comp%tc = Tc
          p_act_eosc%comps(i)%p_comp%pc = Pc
          p_act_eosc%comps(i)%p_comp%zc = Pc*Vc/(Tc*Rgas)
          Acf = acentricFactorEos(i,ierr)
          p_act_eosc%comps(i)%p_comp%acf = Acf
          p_eos%single(i)%Acf = Acf
          call getAcentricAlphaParam(p_eos%single(i)%alphaMethod, Acf, &
               p_eos%single(i)%alphaParams)

          ! Copy to others
          do j=2,size(p_act_eosc%cubic_eos_alternative)
            select type (p_eos_j => p_act_eosc%cubic_eos_alternative(j)%p_eos)
            class is (cb_eos)
              p_eos_j%single(i)%Tc = p_eos%single(i)%Tc
              p_eos_j%single(i)%Pc = p_eos%single(i)%Pc
              p_eos_j%single(i)%Acf = p_eos%single(i)%Acf
              p_eos_j%single(i)%alphaParams = p_eos%single(i)%alphaParams
              call cbSingleCalcABC(nce, p_eos_j, i)
            class default
              print *,"Fallback eos should be cubic"
            end select
          enddo
        class default
          print *,"Fallback eos should be cubic"
        end select
      endif
    enddo
    tpTmin = Tmin
  end subroutine redefine_critical_parameters

  !> Initialize ThermoPack
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine init_thermopack(eos,mixing,alpha,nphase,&
       csp_eos,csp_ref_comp,kij_ref,alpha_ref,saft_ref,&
       b_exponent)
    use eosdata, only: isSAFTEOS
    use stringmod, only: str_eq
    use thermopack_constants, only: THERMOPACK, ref_len
    use thermopack_var, only: nce, nc, nph, complist
    use volume_shift, only: initVolumeShift, NOSHIFT
    use csp, only: csp_init
    use cubic_eos, only: cb_eos
    use cbselect, only: SelectCubicEOS, SelectMixingRules
    use saft_interface, only: saft_type_eos_init
    use cpa_parameters, only: mixHasSelfAssociatingComp
    use assocschemeutils, only: no_assoc
    use saft_association, only: numAssocSites
    use stringmod, only: uppercase
    use eos_containers, only: allocate_eos
    !$ use omp_lib
    ! Method information
    character(len=*), intent(in) :: eos    !< String defining equation of state
    character(len=*), intent(in) :: mixing !< String defining mixing rules
    character(len=*), intent(in) :: alpha  !< String defining alpha correlation
    integer, intent(in) :: nphase !< Number of phases
    character(len=*), optional, intent(in) :: csp_eos !< Corrensponding state equation
    character(len=*), optional, intent(in) :: csp_ref_comp !< CSP component
    character(len=*), optional, intent(in) :: kij_ref, alpha_ref, saft_ref !< Data set number
    real, optional, intent(in) :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    ! String containing components on TPLib format
    character(len=100) :: mixRule,csp_refEos,csp_refcomp_str
    character(len=len(eos)) :: eosLocal  !< Local copy of string defining equation of state
    integer :: ncbeos, err, i, volumeShiftId, index
    character(len=ref_len) :: kij_ref_local, alpha_ref_local, saft_ref_local
    type(eos_container), pointer :: p_act_eosc
    !
    p_act_eosc => get_active_eos_container()

    if (present(kij_ref)) then
      kij_ref_local = trim(kij_ref)
    else
      kij_ref_local = "DEFAULT"
    endif
    if (present(alpha_ref)) then
      alpha_ref_local = trim(alpha_ref)
    else
      alpha_ref_local = "DEFAULT"
    endif
    if (present(saft_ref)) then
      saft_ref_local = trim(saft_ref)
    else
      saft_ref_local = "DEFAULT"
    endif

    ! if (.not. associated(p_act_eosc)) then
    !   ! No eos_container have been allocated
    !   index = add_eos()
    !   p_act_eosc => get_active_eos_container()
    ! endif
    ! ! Component list.
    ! call allocate_eos(nce, eos)

    ! Set parameters
    !p_act_eosc%EoSLib = THERMOPACK
    !p_act_eosc%nph = nphase
    mixRule = trim(mixing)
    volumeShiftId = NOSHIFT
    eosLocal = eos
    numAssocSites = 0
    if (len(eos) >= 3) then
      if (eos(1:3) == 'CPA') then
        if ( .not. mixHasSelfAssociatingComp(nc,trim(eosLocal),&
             complist,saft_ref)) then
          print *,'No self associating components. Initializing ',&
               eos(5:len(eos)),' instead of ',trim(eos)
          eosLocal = eos(5:len(eos))
          do i=1,nce
            p_act_eosc%comps(i)%p_comp%assoc_scheme = no_assoc
          enddo
        endif
      endif
    endif

    if (trim(uppercase(eos)) == 'LK') then
      mixRule = 'VDW'
    else if (trim(uppercase(eos)) == 'SRK-PENELOUX') then
      eosLocal = 'SRK'
      volumeShiftId = InitVolumeShift(nc,p_act_eosc%comps,'Peneloux','SRK')
    else if (trim(uppercase(eos)) == 'PR-PENELOUX') then
      eosLocal = 'PR'
      volumeShiftId = InitVolumeShift(nc,p_act_eosc%comps,'Peneloux','PR')
    else if (len(eos) >= 3) then
      if (uppercase(eos(1:3)) == 'CSP') then
        eosLocal = eos(5:len(trim(eos)))
        if (present(csp_eos)) then
          csp_refEos = uppercase(csp_eos)
        else
          csp_refEos = "NIST_MEOS"
          if (.not. silent_init) &
               print *,'init_thermopack: CSP model defaulted to MBWR32'
        endif
        if (present(csp_ref_comp)) then
          csp_refcomp_str = csp_ref_comp ! This is case sensitive
        else
          csp_refcomp_str = "C3"
          if (.not. silent_init) &
               print *,'init_thermopack: CSP reference component defaulted to C3'
        endif
        call csp_init(refcomp_str=trim(csp_refcomp_str),shEos=trim(eosLocal),&
             shMixRule=trim(mixRule),shAlpha=trim(alpha),&
             refEos=trim(csp_refEos),refAlpha=trim(alpha))
        eosLocal = uppercase(eos)
      end if
    end if

    p_act_eosc%eos(1)%p_eos%volumeShiftId = volumeShiftId
    p_act_eosc%eos(1)%p_eos%isElectrolyteEoS = .false.
    select type(p_eos => p_act_eosc%eos(1)%p_eos)
    type is (cb_eos)
      call SelectCubicEOS(nce,p_act_eosc%comps,p_eos,trim(alpha),&
           alpha_ref_local)
      call SelectMixingRules(nce,p_act_eosc%comps,p_eos,mixRule,&
           kij_ref_local,b_exponent)
    end select

    ! SAFT initialization must be done after cbeos initialization.
    if (isSAFTEOS(p_act_eosc%eos(1)%p_eos%eosidx)) then
      call saft_type_eos_init(nce,p_act_eosc%comps,&
           p_act_eosc%eos(1)%p_eos,saft_ref_local,silent_init)
    end if
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      p_act_eosc%eos(i) = p_act_eosc%eos(1)
    enddo
  end subroutine init_thermopack

  !----------------------------------------------------------------------
  !> Initialize trend
  !>
  !> \author MH, 2013-04-10, EA 2014-02
  !----------------------------------------------------------------------
  subroutine init_trend(eos,ncomp,nphase,doFallbackInit)
    !use thermopack_constants, only: Rgas,TREND,verbose
    !use thermopack_var, only: complist,nph
    !use stringmod, only: chartoascii
    !use compname_translation, only: translate_compname
#ifdef __INTEL_COMPILER
    use ifport
#endif
    ! Input:
    character(len=*), intent(in)    :: eos            !< String defining equation of state
    integer, intent(in)             :: ncomp          !< Number of components
    integer, intent(in)             :: nphase         !< Number of phases
    logical, intent(in)             :: doFallbackInit !< Init thermopack as fallback
    !Internal:
    integer                         :: i
    integer                         :: mix
    character (12)                  :: comps(ncomp)
    character(len=255)              :: path,trendroot
    integer                         :: npath,int_path(255),int_comps(12*ncomp),ncomps

    ! if (doFallbackInit) then
    !   ! Also initialize Thermopack cubic EoS, to be used for initial estimates.
    !   if (verbose) then
    !     write(*,*) "Initializing EoS-lib Thermopack for initial estimates. (SRK, Classic)"
    !   endif
    !   call init_thermopack("SRK","Classic","Classic", nphase)
    ! endif
    ! ! Setting EoSLib flag
    ! if (verbose) then
    !   write(*,*) "Initializing EoS-lib TREND"
    ! endif
    ! EoSLib = TREND
    ! call set_constants() ! Depend on EoSLib
    ! ! Setting global nph number
    ! nph = nphase
    ! do i=1,ncomp
    !   call translate_compname(trim(complist(i)), TREND, comps(i))
    !   char_comps((i-1)*12+1:i*12) = comps(i)
    ! enddo
    ! ! Setting mixing rules
    ! if (verbose) then
    !   write(*,*) "OBS: Mixing rule and alpha-correlation input ignored."
    ! endif
    ! mix = 1 ! Lorentz-Berthelot or modified Helmholtz mixing rules
    ! call getenv("TRENDROOT",trendroot)
    ! if (trim(trendroot) == "") then
    !    trendroot = "./trend/"
    ! endif
    ! ! Setting path based on input EoS-string:
    ! ! They are all "Multi-parameter explicit Helmholtz" EoS, but with different parameters.
    ! select case(trim(eos))
    ! case ("EOSCG")
    !   ! The EoS for CCS mixtures, parameters fitted by Johannes Gernert.
    !   ! Will fail when selecting components not covered in the original fitting.
    !    path = trim(trendroot)//"EOS_CG/"
    ! case ("GERG2008")
    !   ! The GERG-2008 EoS for natural gas mixtures.
    !   path = trim(trendroot)//"GERG-2008/"
    ! case ("EOSCG-GERG")
    !   ! Primarily EOSCG, but now falling back to GERG2008 for unsupported components.
    !   path = trim(trendroot)//"/"
    ! case default
    !   call stoperror("No such EoS in TREND (EOSCG,GERG2008,EOSCG-GERG): "//trim(eos))
    ! end select
    ! if (verbose) then
    !   write(*,*) "Will look for dirs FLUIDS and BINARY_MIX_FILES in: "//trim(path)
    ! endif
    ! npath = len(trim(path))
    ! call chartoascii(int_path,path,npath)
    ! ncomps = 12*ncomp
    ! call chartoascii(int_comps,char_comps,ncomps)
    ! call trend_init_no_char(ncomp,int_path,npath,int_comps,ncomps,mix,Rgas)

  end subroutine init_trend

  !----------------------------------------------------------------------
  !> Clean up memory used by eos
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  ! subroutine cleanup_eos()
  !   use thermopack_var, only: complist
  !   use multiparameter_base, only: cleanup_meos
  !   integer :: stat, i
  !   !
  !   stat = 0
  !   if (allocated(complist)) deallocate(complist,STAT=stat)
  !   if (stat /= 0) write(*,*) 'Error deallocating complist!'
  !   if (allocated(comp)) deallocate(comp,STAT=stat)
  !   if (stat /= 0) write(*,*) 'Error deallocating comp!'
  !   if (allocated(cbeos)) then
  !     do i=1,size(cbeos)
  !       call deAllocateEosCubic(cbeos(i))
  !     enddo
  !     deallocate(cbeos,STAT=stat);
  !     if (stat /= 0) call stoperror('Not able to deallocate cbeos')
  !   endif
  !   if (allocated(cbeos_alternative)) then
  !     do i=1,size(cbeos_alternative)
  !       call deAllocateEosCubic(cbeos_alternative(i))
  !     enddo
  !     deallocate(cbeos_alternative,STAT=stat);
  !     if (stat /= 0) call stoperror('Not able to deallocate cbeos')
  !   endif

  !   call cleanup_meos() ! Clean up multiparameter EoS
  ! end subroutine cleanup_eos

  !----------------------------------------------------------------------------
  !> Initialize SAFT-VR-MIE EoS. Use: call init_saftvrmie('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_saftvrmie(comps,param_reference)
    use compdata,   only: SelectComp, initCompList
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    type(eos_container), pointer     :: p_act_eosc

    if (.not. active_eos_container_is_associated()) then
      ! No eos_container have been allocated
      index = add_eos()
    endif
    p_act_eosc => get_active_eos_container()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,p_act_eosc%complist)
    !
    call allocate_eos(ncomp, "SAFT-VR-MIE")

    ! Number of phases
    p_act_eosc%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = p_act_eosc%nph
    complist => p_act_eosc%complist
    apparent => NULL()

    ! Set eos library identifyer
    p_act_eosc%eosLib = THERMOPACK

    ! Initialize components module
    call SelectComp(complist,nce,"DEFAULT",p_act_eosc%comps,ierr)

    ! Initialize Thermopack
    call init_thermopack("SAFT-VR-MIE", "Classic", "Classic", nphase=3, &
         saft_ref=param_reference)

    ! Initialize fallback eos
    call init_fallback_and_redefine_criticals(silent=.true.)
  end subroutine init_saftvrmie

  !----------------------------------------------------------------------------
  !> Initialize PC-SAFT EoS. Use: call init_pcsaft('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_pcsaft(comps,param_reference)
    use compdata,   only: SelectComp, initCompList
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    type(eos_container), pointer     :: p_act_eosc

    if (.not. active_eos_container_is_associated()) then
      ! No eos_container have been allocated
      index = add_eos()
    endif
    p_act_eosc => get_active_eos_container()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,p_act_eosc%complist)
    !
    call allocate_eos(ncomp, "PC-SAFT")

    ! Number of phases
    p_act_eosc%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = p_act_eosc%nph
    complist => p_act_eosc%complist
    apparent => NULL()

    ! Set eos library identifyer
    p_act_eosc%eosLib = THERMOPACK

    ! Initialize components module
    call SelectComp(complist,nce,"DEFAULT",p_act_eosc%comps,ierr)

    ! Initialize Thermopack
    call init_thermopack("PC-SAFT", "Classic", "Classic", nphase=3,&
         saft_ref=param_reference)

    ! Initialize fallback eos
    call init_fallback_and_redefine_criticals(silent=.true.)

  end subroutine init_pcsaft

  !----------------------------------------------------------------------------
  !> Initialize CPA EoS. Use: call init_cpa('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cpa(comps,eos,mixing,alpha,param_reference)
    use compdata,   only: SelectComp, initCompList
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), optional, intent(in) :: eos    !< Equation of state
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    character(len=*), optional, intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    character(len=100)               :: mixing_loc, alpha_loc, eos_loc, param_ref_loc
    type(eos_container), pointer     :: p_act_eosc

    ! Initialize Thermopack
    eos_loc = "SRK"
    if (present(eos)) eos_loc = uppercase(eos)
    !
    if (.not. active_eos_container_is_associated()) then
      ! No eos_container have been allocated
      index = add_eos()
    endif
    p_act_eosc => get_active_eos_container()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,p_act_eosc%complist)
    !
    call allocate_eos(ncomp, "CPA-"//trim(eos_loc))

    ! Number of phases
    p_act_eosc%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = p_act_eosc%nph
    complist => p_act_eosc%complist
    apparent => NULL()

    ! Set eos library identifyer
    p_act_eosc%eosLib = THERMOPACK

    ! Initialize components module
    call SelectComp(complist,nce,"DEFAULT",p_act_eosc%comps,ierr)

    param_ref_loc = "Default"
    alpha_loc = "Classic"
    mixing_loc = "Classic"
    if (present(param_reference)) param_ref_loc = uppercase(param_reference)
    if (present(mixing)) mixing_loc = uppercase(mixing)
    if (present(alpha)) alpha_loc = uppercase(alpha)

    call init_thermopack("CPA-"//trim(eos_loc),trim(uppercase(mixing_loc)), &
         trim(uppercase(alpha_loc)), nphase=3, saft_ref=param_ref_loc)

    ! Initialize fallback eos
    call init_fallback_and_redefine_criticals(silent=.true.)
  end subroutine init_cpa

end module eoslibinit
