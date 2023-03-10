!> Initialize thermodynamic models
!!
!! \author MH, 2014-02
module eoslibinit
  !
  use thermopack_var, only: nce, get_active_eos, thermo_model, &
       get_active_thermo_model, get_active_alt_eos, base_eos_param, add_eos, &
       active_thermo_model_is_associated, numAssocSites
  use eos_container, only: allocate_eos
  use stringmod,  only: uppercase, str_eq, string_match, string_match_val
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
  public :: init_cubic, init_cpa, init_saftvrmie, init_pcsaft, init_tcPR, init_quantum_cubic
  public :: init_extcsp, init_lee_kesler, init_quantum_saftvrmie
  public :: init_multiparameter, init_pets, init_ljs, init_lj
  public :: silent_init
  public :: redefine_critical_parameters
  public :: init_volume_translation
  !
contains

  !----------------------------------------------------------------------
  !> Set method to discriminate between liquid and vapor in case of an
  !! undefined single phase
  !!
  !! \author MH, 2020-06
  !----------------------------------------------------------------------
  subroutine set_liq_vap_discr_method(liq_vap_discr_method)
    ! Method information
    integer, intent(in) :: liq_vap_discr_method !< Method to discriminate between liquid and vapor in case of an undefined single phase.
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    ! Method for discriminating between liquid/vapor when poorly defined
    act_mod_ptr%liq_vap_discr_method = liq_vap_discr_method
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
    type(thermo_model), pointer :: act_mod_ptr
    integer :: i
    act_mod_ptr => get_active_thermo_model()

    act_mod_ptr%eos(1)%p_eos%volumeShiftId = &
         InitVolumeShift(nce, act_mod_ptr%comps, &
         volume_trans_model, act_mod_ptr%eos(1)%p_eos%eosid)

    ! Distribute volumeShiftId
    do i=2,size(act_mod_ptr%eos)
      act_mod_ptr%eos(i)%p_eos%volumeShiftId = &
           act_mod_ptr%eos(1)%p_eos%volumeShiftId
    enddo
  end subroutine init_volume_translation

  !----------------------------------------------------------------------
  !> Initialize thermodynamics library (legacy interface)
  !>
  !> \author MH, 2014-02
  !----------------------------------------------------------------------
  subroutine init_thermo(eos,mixing,alpha,comp_string,nphases,&
       liq_vap_discr_method_in,csp_eos,csp_ref_comp,kij_ref,alpha_ref,&
       saft_ref,b_exponent,TrendEosForCp,cptype,silent)
    use thermopack_constants, only: clen, TREND, THERMOPACK
    use cbselect,   only: SelectCubicEOS
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
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
    character(len=*), optional, intent(in) :: kij_ref, alpha_ref, saft_ref !< Data set identifiers
    real, optional, intent(in) :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    character(len=*), optional, intent(in) :: TrendEosForCp !< Option to init trend for ideal gas properties.
    integer, optional, intent(in) :: cptype !< Type numbers for Cp
    logical, optional, intent(in) :: silent !< Option to disable init messages.
    ! Locals
    integer :: ncomp !< Number of components
    character(len=clen) :: message
    integer             :: i, ierr, index
    type(thermo_model), pointer :: act_mod_ptr
    if (present(silent)) then
      silent_init = silent
    endif

    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()

    ! Component list.
    call initCompList(trim(uppercase(comp_string)),ncomp,act_mod_ptr%complist)
    allocate(complist, source=act_mod_ptr%complist)


    ! Equation of state
    call allocate_eos(ncomp, eos)

    ! Method for discriminating between liquid/vapor when poorly defined
    if (present(liq_vap_discr_method_in)) then
      act_mod_ptr%liq_vap_discr_method = liq_vap_discr_method_in
    end if

    ! Number of phases
    act_mod_ptr%nph = nphases

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    complist => act_mod_ptr%complist
    apparent => NULL()
    numAssocSites = 0

    ! Initialize components
    call SelectComp(complist,nce,"DEFAULT",act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Set cptype
    if (present(cptype)) then
      do i=1,nc
        act_mod_ptr%comps(i)%p_comp%id_cp%cptype = cptype
      enddo
    endif

    if (str_eq(eos, "EOSCG") .or. &
         str_eq(eos, "EOS-CG") .or. &
         str_eq(eos, "GERG2008") .or. &
         str_eq(eos, "GERG-2008") .or. &
         str_eq(eos, "EOSCG-GERG")) then
      act_mod_ptr%eosLib = TREND
    else
      act_mod_ptr%eosLib = THERMOPACK
    endif

    ! Initialize the selected EoS-library
    select case (act_mod_ptr%eosLib)
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
    use eos_parameters, only: single_eos
    !$ use omp_lib, only: omp_get_max_threads
    logical, intent(in) :: silent !< Option to disable init messages.
    ! Locals
    integer             :: i
    real                :: Tci, Pci, oi, rhocrit
    logical             :: redefine_critical
    type(thermo_model), pointer :: act_mod_ptr
    !
    act_mod_ptr => get_active_thermo_model()

    if (.not. act_mod_ptr%need_alternative_eos) return

    redefine_critical = isSAFTEOS(act_mod_ptr%eos(1)%p_eos%eosidx)

    if (act_mod_ptr%EoSLib == TREND) then
      ! Use TREND parameters to get better critical point in alternative model
      do i=1,nce
        call trend_getcrit(i,Tci,Pci,oi)
        act_mod_ptr%comps(i)%p_comp%tc = Tci
        act_mod_ptr%comps(i)%p_comp%pc = Pci
        act_mod_ptr%comps(i)%p_comp%acf = oi
      enddo
    endif

    select type(p_eos => act_mod_ptr%eos(1)%p_eos)
    type is (single_eos)
      if (allocated(p_eos%mbwr_meos)) then
        redefine_critical = .true.
      else if (allocated(p_eos%nist)) then
        do i=1,nce
          call p_eos%nist(i)%meos%getCritPoint(Tci,Pci,rhocrit)
          act_mod_ptr%comps(i)%p_comp%tc = Tci
          act_mod_ptr%comps(i)%p_comp%pc = Pci
          act_mod_ptr%comps(i)%p_comp%zc = Pci/(rhocrit*Tci*p_eos%nist(i)%meos%Rgas_meos)
          act_mod_ptr%comps(i)%p_comp%acf = p_eos%nist(i)%meos%acf
        enddo
      endif
    end select

    select type(p_eos => act_mod_ptr%cubic_eos_alternative(1)%p_eos)
    type is (cb_eos)
      call SelectCubicEOS(nce, act_mod_ptr%comps, p_eos, &
           "CLASSIC", "DEFAULT")

      call SelectMixingRules(nce, act_mod_ptr%comps, p_eos, &
         "VDW", "DEFAULT")
    class default
      call stoperror("init_cubic: Should be cubic EOS")
    end select

    ! Distribute parameters from redefined eos
    do i=2,size(act_mod_ptr%cubic_eos_alternative)
      act_mod_ptr%cubic_eos_alternative(i)%p_eos = &
           act_mod_ptr%cubic_eos_alternative(1)%p_eos
    enddo

    if (act_mod_ptr%EoSLib == THERMOPACK .and. redefine_critical) then
      call redefine_critical_parameters(silent)
    endif
  end subroutine init_fallback_and_redefine_criticals

  !----------------------------------------------------------------------------
  !> Initialize cubic EoS. Use: call init_cubic('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cubic(comps,eos,mixing,alpha,parameter_reference,vol_shift)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var, only: nc, nce, ncsym, complist, nph, apparent
    use thermopack_constants, only: THERMOPACK
    use eos_container, only: allocate_eos
    use cbselect, only: selectCubicEOS, SelectMixingRules
    use cubic_eos, only: cb_eos
    use volume_shift, only: InitVolumeShift
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: eos     !< Equation of state
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    character(len=*), optional, intent(in) :: parameter_reference  !< Parameter reference
    logical, optional, intent(in) :: vol_shift  !< Volume shift
    ! Locals
    integer                          :: ncomp, i, index, ierr, volumeShiftId
    character(len=len_trim(comps))   :: comps_upper
    character(len=100)               :: mixing_loc, alpha_loc, paramref_loc, beta_loc
    logical                          :: volshift_loc
    type(thermo_model), pointer      :: act_mod_ptr
    logical                          :: found_tcPR, found_QuantumCubic
    integer                          :: matchval_tcPR, matchval_QuantumCubic
    ! Get a pointer to the active thermodynamics model
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model has been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    call allocate_eos(ncomp, eos)

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = act_mod_ptr%nph
    act_mod_ptr%nc = ncomp
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifier
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variables inputs that are optional
    mixing_loc = "vdW"
    alpha_loc = "Classic"
    paramref_loc = "DEFAULT"
    volshift_loc = .False.
    beta_loc = "Classic"
    if (present(mixing)) then
       mixing_loc = uppercase(mixing)
       if (str_eq(mixing_loc, "Classic")) mixing_loc = "VDW"
    end if
    if (present(alpha)) alpha_loc = uppercase(alpha)
    if (present(parameter_reference)) paramref_loc = parameter_reference
    if (present(vol_shift)) volshift_loc = vol_shift

    ! Special handling of translated-consistent Peng--Robinson EoS by le Guennec
    ! et al. (10.1016/j.fluid.2016.09.003)
    call string_match_val("tcPR", paramref_loc, found_tcPR, matchval_tcPR)
    if (found_tcPR) then
       alpha_loc = "TWU"
       volshift_loc = .True.
    end if

    ! Special handling of Quantum Cubic Peng-Robinson equation of state by Aasen
    ! et al. (10.1016/j.fluid.2020.112790)
    call string_match_val("QuantumCubic", paramref_loc, found_QuantumCubic, matchval_QuantumCubic)
    if (found_QuantumCubic) then
       alpha_loc = "TWU"
       volshift_loc = .True.
       beta_loc = "Quantum"
    end if

    ! Initialize components module
    call SelectComp(complist,nce,paramref_loc,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Initialize volume shift
    if (volshift_loc) then
       volumeShiftId = InitVolumeShift(nc,act_mod_ptr%comps,'Peneloux',eos, param_ref=paramref_loc)
       act_mod_ptr%eos(1)%p_eos%volumeShiftId = volumeShiftId
    end if

    ! Initialize Thermopack
    select type(p_eos => act_mod_ptr%eos(1)%p_eos)
    type is (cb_eos)
      call SelectCubicEOS(nc, act_mod_ptr%comps, &
           p_eos, alpha_loc, paramref_loc, betastr=beta_loc)

      call SelectMixingRules(nc, act_mod_ptr%comps, &
           p_eos, mixing_loc, paramref_loc)
    class default
      call stoperror("init_cubic: Should be cubic EOS")
   end select

    ! Distribute parameters from redefined eos
    do i=2,size(act_mod_ptr%eos)
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

  end subroutine init_cubic

  !----------------------------------------------------------------------------
  !> Initialize translated and consistent cubic EoS by le Guennec et al.
  ! (10.1016/j.fluid.2016.09.003)
  !----------------------------------------------------------------------------
  subroutine init_tcPR(comps, mixing, parameter_ref)
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in), optional :: mixing !< Mixing rule
    character(len=*), intent(in), optional :: parameter_ref !< Parameter set reference
    !
    character(len=200) :: parameter_reference
    parameter_reference = "tcPR"
    if (present(parameter_ref)) then
      parameter_reference = trim(parameter_ref) // "/" // trim(parameter_reference)
    endif
    call init_cubic(eos="PR", comps=comps, mixing=mixing, &
         parameter_reference=parameter_reference)
  end subroutine init_tcPR

  !----------------------------------------------------------------------------
  !> Initialize Quantum Cubic Peng-Robinson equation of state by Aasen et al.
  !> (10.1016/j.fluid.2020.112790)
  !----------------------------------------------------------------------------
  subroutine init_quantum_cubic(comps, mixing)
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in), optional :: mixing !< Mixing rule
    call init_cubic(eos="PR", comps=comps, mixing=mixing, parameter_reference="QuantumCubic/tcPR")
  end subroutine init_quantum_cubic

  !----------------------------------------------------------------------------
  !> Initialize extended corresponding state EoS. Use: call init_extcsp
  !----------------------------------------------------------------------------
  subroutine init_extcsp(comps,sh_eos,sh_mixing,sh_alpha,&
       ref_eos,ref_comp,ref_alpha,&
       parameter_ref)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var, only: nc, nce, ncsym, complist, nph, apparent
    use thermopack_constants, only: THERMOPACK, ref_len
    use stringmod,  only: uppercase
    use eos_container, only: allocate_eos
    use extcsp, only: extcsp_eos, csp_init
    use volume_shift, only: NOSHIFT
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), intent(in) :: comps !< Components. Comma or white-space
    !separated
    character(len=*), intent(in) :: sh_eos      !< Shape factor equation of state
    character(len=*), intent(in) :: sh_alpha    !< Shape factor alpha
    character(len=*), intent(in) :: sh_mixing   !< Shape factor mixing rules
    character(len=*), intent(in) :: ref_eos     !< Reference equation of state
    character(len=*), intent(in) :: ref_comp    !< Reference component
    character(len=*), intent(in), optional :: ref_alpha  !< Needed if refEos is a cubic eos. Should not be present if one want to use an mbwr reference eos.
    character(len=*), intent(in), optional :: parameter_ref !< Parameter set reference

    ! Locals
    integer                          :: ncomp, i, index, ierr, ncbeos
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    class(base_eos_param), pointer   :: act_eos_ptr
    character(len=ref_len)           :: param_ref

    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    call allocate_eos(ncomp, "CSP-"//trim(sh_eos))

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = act_mod_ptr%nph
    act_mod_ptr%nc = ncomp
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_ref)) then
      param_ref = parameter_ref
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    act_eos_ptr => get_active_eos()
    act_eos_ptr%volumeShiftId = NOSHIFT
    act_eos_ptr%isElectrolyteEoS = .false.
    select type(p_eos => act_eos_ptr)
    type is (extcsp_eos)
      call csp_init(p_eos,nce,act_mod_ptr%comps,&
           refcomp_str=trim(ref_comp),&
           shEos=trim(sh_eos),&
           shMixRule=trim(sh_mixing),shAlpha=trim(sh_alpha),&
           refEos=trim(ref_eos),refAlpha=ref_alpha,&
           parameter_ref=parameter_ref)
   end select

   ncbeos = 1
   !$ ncbeos = omp_get_max_threads()
   do i=2,ncbeos
     act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
   enddo

   ! Set globals
   call update_global_variables_form_active_thermo_model()

   ! Initialize fallback eos
   call init_fallback_and_redefine_criticals(silent=.true.)

  end subroutine init_extcsp

  !----------------------------------------------------------------------
  !> Set critical parameters to represent actual model
  !>
  !> \author MH, 2019-03
  !----------------------------------------------------------------------
  subroutine redefine_critical_parameters(silent_init, Tc_in, vc_in)
    use cbmix,      only: cbSingleCalcABC
    use thermopack_var, only: nce
    use eosTV,        only: pressure
    use critical,     only: calcCriticalTV
    use saturation,   only: acentricFactorEos
    use thermopack_constants,      only: tpTmin, Rgas
    use cbAlpha,      only: getAcentricAlphaParam
    use cubic_eos,    only: cb_eos
    use saft_interface, only: estimate_critical_parameters
    use eosdata, only: eosCPA, isSAFTEOS
    logical, intent(in) :: silent_init
    real, optional, intent(in) :: Tc_in(nce), vc_in(nce)
    ! Locals
    integer :: i, j, ierr
    real :: Tmin
    real :: Vc
    real :: Tc  !< Specified critical temperature [K]
    real :: Pc  !< Specified critical pressure [Pa]
    real :: Acf !< Specified acentric factor [-]
    real :: Z(nce)
    logical :: isSAFT
    type(thermo_model), pointer :: act_mod_ptr
    class(base_eos_param), pointer :: act_eos_ptr, act_alt_eos_ptr

    act_mod_ptr => get_active_thermo_model()
    act_alt_eos_ptr => get_active_alt_eos()
    act_eos_ptr => get_active_eos()
    isSAFT = isSAFTEOS(act_eos_ptr%eosidx) .and. act_mod_ptr%eosidx /= eosCPA
    Tmin = tpTmin
    tpTmin = 2.0
    do i=1,nce
      Z = 0
      Z(i) = 1
      Tc = -1.0
      Vc = -1.0
      if (present(Tc_in)) Tc = Tc_in(i)
      if (present(Vc_in)) Vc = Vc_in(i)
      call calcCriticalTV(Tc,Vc,Z,ierr,p=Pc)
      if (ierr /= 0 .and. isSAFT) then
        ! Maybe sigma and eps are redefined and the initial guess is bad?
        call estimate_critical_parameters(i, Tc, vc)
        call calcCriticalTV(Tc,Vc,Z,ierr,p=Pc)
      endif
      if (ierr /= 0 .and. .not. silent_init) then
        print *, 'Not able to redefine critical properties for component: ', &
             trim(act_mod_ptr%comps(i)%p_comp%ident)
      else
        select type (p_eos => act_alt_eos_ptr)
        class is (cb_eos)
          p_eos%single(i)%Tc = Tc
          p_eos%single(i)%Pc = Pc
          call cbSingleCalcABC(nce,p_eos,i)
          act_mod_ptr%comps(i)%p_comp%tc = Tc
          act_mod_ptr%comps(i)%p_comp%pc = Pc
          act_mod_ptr%comps(i)%p_comp%zc = Pc*Vc/(Tc*Rgas)
          Acf = acentricFactorEos(i,ierr)
          act_mod_ptr%comps(i)%p_comp%acf = Acf
          p_eos%single(i)%Acf = Acf
          call getAcentricAlphaParam(p_eos%single(i)%alphaMethod, Acf, &
               p_eos%single(i)%alphaParams)

          ! Copy to others
          do j=2,size(act_mod_ptr%cubic_eos_alternative)
            select type (p_eos_j => act_mod_ptr%cubic_eos_alternative(j)%p_eos)
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
    use thermopack_var, only: nce, nc, complist
    use volume_shift, only: initVolumeShift, NOSHIFT
    use extcsp, only: csp_init, extcsp_eos
    use cubic_eos, only: cb_eos
    use cbselect, only: SelectCubicEOS, SelectMixingRules
    use saft_interface, only: saft_type_eos_init
    use cpa_parameters, only: mixHasSelfAssociatingComp
    use assocschemeutils, only: no_assoc
    use saft_association, only: numAssocSites
    use stringmod, only: uppercase
    use eos_container, only: allocate_eos
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
    integer :: ncbeos, i, volumeShiftId
    character(len=ref_len) :: kij_ref_local, alpha_ref_local, saft_ref_local
    type(thermo_model), pointer :: act_mod_ptr
    class(base_eos_param), pointer :: act_eos_ptr
    !
    act_mod_ptr => get_active_thermo_model()
    act_eos_ptr => get_active_eos()

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

    ! Set parameters
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
            act_mod_ptr%comps(i)%p_comp%assoc_scheme = no_assoc
          enddo
        endif
      endif
    endif

    if (trim(uppercase(eos)) == 'LK') then
      mixRule = 'VDW'
    else if (trim(uppercase(eos)) == 'SRK-PENELOUX') then
      eosLocal = 'SRK'
      volumeShiftId = InitVolumeShift(nc,act_mod_ptr%comps,'Peneloux','SRK')
    else if (trim(uppercase(eos)) == 'PR-PENELOUX') then
      eosLocal = 'PR'
      volumeShiftId = InitVolumeShift(nc,act_mod_ptr%comps,'Peneloux','PR')
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
      end if
    end if

    act_eos_ptr%volumeShiftId = volumeShiftId
    act_eos_ptr%isElectrolyteEoS = .false.
    select type(p_eos => act_eos_ptr)
    class is (cb_eos)
      call SelectCubicEOS(nce,act_mod_ptr%comps,p_eos,trim(alpha),&
           alpha_ref_local)
      call SelectMixingRules(nce,act_mod_ptr%comps,p_eos,mixRule,&
           kij_ref_local,b_exponent)
    type is (extcsp_eos)
      call csp_init(p_eos,nce,act_mod_ptr%comps,refcomp_str=trim(csp_refcomp_str),&
           shEos=trim(eosLocal),&
           shMixRule=trim(mixRule),shAlpha=trim(alpha),&
           refEos=trim(csp_refEos),refAlpha=trim(alpha))
   end select

    ! SAFT initialization must be done after cbeos initialization.
    if (isSAFTEOS(act_eos_ptr%eosidx)) then
       call saft_type_eos_init(nce,act_mod_ptr%comps,&
            act_eos_ptr,saft_ref_local,silent_init)
    end if
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
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
    ! integer                         :: i
    ! integer                         :: mix
    ! character (12)                  :: comps(ncomp)
    ! character(len=255)              :: path,trendroot
    ! integer                         :: npath,int_path(255),int_comps(12*ncomp),ncomps

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

  !----------------------------------------------------------------------------
  !> Initialize SAFT-VR-MIE EoS. Use: call init_saftvrmie('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_saftvrmie(comps,parameter_reference)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nce, complist
    use thermopack_constants, only: THERMOPACK, ref_len
    use stringmod,  only: uppercase
    use volume_shift, only: NOSHIFT
    use saft_interface, only: saft_type_eos_init
    !$ use omp_lib
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, index, ierr, i, ncbeos
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    class(base_eos_param), pointer   :: act_eos_ptr
    character(len=ref_len)           :: param_ref

    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)

    call allocate_eos(ncomp, "SAFT-VR-MIE")
    act_mod_ptr%need_alternative_eos = .true.

    ! Number of phases
    act_mod_ptr%nph = 3
    ! Assign active mode variables
    act_mod_ptr%nc = ncomp
    nce = ncomp
    complist => act_mod_ptr%complist
    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Initialize Thermopack
    act_eos_ptr => act_mod_ptr%eos(1)%p_eos
    act_eos_ptr%volumeShiftId = NOSHIFT
    act_eos_ptr%isElectrolyteEoS = .false.

    ! SAFT initialization must be done after cbeos initialization.
    call saft_type_eos_init(nce,act_mod_ptr%comps,&
         act_eos_ptr,param_ref,silent_init=.true.)
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ! Initialize fallback eos
    call init_fallback_and_redefine_criticals(silent=.true.)

  end subroutine init_saftvrmie

  !----------------------------------------------------------------------------
  !> Initialize SAFT-VR-MIE with quantum corrections EoS.
  !! Use: call init_quantum_saftvrmie('He,Ne',feynman_hibbs_order=1)
  !----------------------------------------------------------------------------
  subroutine init_quantum_saftvrmie(comps,feynman_hibbs_order,parameter_reference)
    use saftvrmie_options, only: NON_ADD_HS_REF, saftvrmieaij_model_options
    use thermopack_constants, only: clen
    use saftvrmie_containers, only: svrm_opt
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    integer, optional, intent(in) :: feynman_hibbs_order
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    character(len=clen) :: param_ref !< Data set reference
    integer :: FH, FH_model
    if (present(feynman_hibbs_order)) then
      FH = max(min(feynman_hibbs_order, 2),0)
    else
      FH = 1 ! Default to FH1
    endif
    FH_model = FH + 1
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif
    if (str_eq(param_ref, "DEFAULT")) then
      ! Default parameter sets
      select case(FH)
      case(0) ! FH0
        param_ref = "AASEN2019-FH0"
      case(1) ! FH1
        param_ref = "AASEN2019-FH1"
      case(2) ! FH2
        param_ref = "AASEN2019-FH2"
      end select
    endif
    call init_saftvrmie(comps,param_ref)
    call svrm_opt%saftvrmieaij_model_options(FH_model, NON_ADD_HS_REF)
  end subroutine init_quantum_saftvrmie

  !----------------------------------------------------------------------------
  !> Initialize PC-SAFT EoS. Use: call init_pcsaft('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_pcsaft(comps,parameter_reference,simplified,polar)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK, ref_len
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    logical, optional, intent(in) :: simplified !< Use simplified PC-SAFT (Von Solms et al. 2003: 10.1021/ie020753p)
    logical, optional, intent(in) :: polar !< Use PCP-SAFT:
    ! Gross 2005: 10.1002/aic.10502
    ! Gross and Vrabec 2006: 10.1002/aic.10683
    ! Vrabec and Gross 2008: 10.1021/jp072619u
    ! Locals
    integer                          :: ncomp, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    character(len=ref_len)           :: param_ref
    character(len=10)                :: label
    logical                          :: polar_local

    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    label = "PC-SAFT"
    polar_local = .false.
    if (present(polar)) then
      polar_local = polar
      if (polar) label = "PCP-SAFT"
    endif
    if (present(simplified)) then
      if (simplified) label = "s"//trim(label)
    endif

    call allocate_eos(ncomp, trim(label))

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = act_mod_ptr%nph
    act_mod_ptr%nc = ncomp
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif
    if (polar_local .and. str_eq(param_ref,"DEFAULT")) then
      param_ref = "Gross2005/Gross2006" ! Make sure PCP entries are used
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Initialize Thermopack
    call init_thermopack("PC-SAFT", "Classic", "Classic", nphase=3,&
         saft_ref=param_ref)

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ! Initialize fallback eos
    call init_fallback_and_redefine_criticals(silent=.true.)

  end subroutine init_pcsaft

  !----------------------------------------------------------------------------
  !> Initialize CPA EoS. Use: call init_cpa('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cpa(comps,eos,mixing,alpha,parameter_reference)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), optional, intent(in) :: eos    !< Equation of state
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    character(len=100)               :: mixing_loc, alpha_loc, eos_loc, param_ref
    type(thermo_model), pointer     :: act_mod_ptr

    ! Initialize Thermopack
    eos_loc = "SRK"
    if (present(eos)) eos_loc = uppercase(eos)
    !
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    call allocate_eos(ncomp, "CPA-"//trim(eos_loc))

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    alpha_loc = "Classic"
    mixing_loc = "Classic"
    if (present(mixing)) mixing_loc = uppercase(mixing)
    if (present(alpha)) alpha_loc = uppercase(alpha)

    call init_thermopack("CPA-"//trim(eos_loc),trim(uppercase(mixing_loc)), &
         trim(uppercase(alpha_loc)), nphase=3, saft_ref=param_ref)

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ! Initialize fallback eos
    act_mod_ptr%need_alternative_eos = .true.
    call init_fallback_and_redefine_criticals(silent=.true.)
  end subroutine init_cpa

  !----------------------------------------------------------------------------
  !> Initialize Lee-Kesler EoS.
  !----------------------------------------------------------------------------
  subroutine init_lee_kesler(comps,parameter_reference)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK, ref_len
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, index, ierr
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    character(len=ref_len)           :: param_ref

    ! Initialize Lee-Kesler eos
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    call allocate_eos(ncomp, "LK")

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    call init_thermopack("LK","CLASSIC", &
         "CLASSIC", nphase=3, kij_ref=param_ref)
    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ! Initialize fallback eos
    act_mod_ptr%need_alternative_eos = .true.
    call init_fallback_and_redefine_criticals(silent=.true.)
  end subroutine init_lee_kesler

  !----------------------------------------------------------------------------
  !> Initialize multiparamaters eos
  !----------------------------------------------------------------------------
  subroutine init_multiparameter(comps, eos)
    use compdata,   only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK
    use stringmod,  only: uppercase
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: eos !< Equation of state
    ! Locals
    integer                          :: ncomp, index, ierr, ncbeos, i
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr

    ! Initialize MEOS
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    complist => act_mod_ptr%complist
    call allocate_eos(ncomp, eos)

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Initialize components module
    call SelectComp(complist,nce,"DEFAULT",act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

    ! Initialize fallback eos
    act_mod_ptr%need_alternative_eos = .true.
    call init_fallback_and_redefine_criticals(silent=.true.)

  end subroutine init_multiparameter

  !----------------------------------------------------------------------------
  !> Initialize Pets EoS.
  !----------------------------------------------------------------------------
  subroutine init_pets(parameter_reference)
    use compdata, only: SelectComp, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK, ref_len
    use stringmod,  only: uppercase
    use volume_shift, only: NOSHIFT
    use saft_interface, only: saft_type_eos_init
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, ncbeos, i, ierr, index
    character(len=3)                 :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    class(base_eos_param), pointer   :: act_eos_ptr
    character(len=ref_len)           :: param_ref

    ! Initialize Pets eos
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper="AR"
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    call allocate_eos(ncomp, "PETS")

    ! Number of phases
    act_mod_ptr%nph = 2

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Initialize Thermopack
    act_eos_ptr => act_mod_ptr%eos(1)%p_eos
    act_eos_ptr%volumeShiftId = NOSHIFT
    act_eos_ptr%isElectrolyteEoS = .false.

    call saft_type_eos_init(nce,act_mod_ptr%comps,&
         act_eos_ptr,param_ref,silent_init=.true.)

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

    ! Initialize fallback eos
    act_mod_ptr%need_alternative_eos = .true.
    call init_fallback_and_redefine_criticals(silent=.true.)
  end subroutine init_pets

  !----------------------------------------------------------------------------
  !> Initialize Lennard-Jones splined equation of state using perturbation theory
  !----------------------------------------------------------------------------
  subroutine init_ljs(model,parameter_reference)
    character(len=*), optional, intent(in) :: model !< Model selection: "UV" (Default), "BH", "WCA"
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    !
    call init_lj_ljs("LJS",model,parameter_reference)
  end subroutine init_ljs

  !----------------------------------------------------------------------------
  !> Initialize Lennard-Jones equation of state using perturbation theory
  !----------------------------------------------------------------------------
  subroutine init_lj(model,parameter_reference)
    character(len=*), optional, intent(in) :: model !< Model selection: "UV" (Default), "UF"
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    !
    call init_lj_ljs("LJ",model,parameter_reference)
  end subroutine init_lj

  !----------------------------------------------------------------------------
  !> Initialize Lennard-Jones (spline) equation of state using perturbation theory
  !----------------------------------------------------------------------------
  subroutine init_lj_ljs(potential,model,parameter_reference)
    use compdata, only: SelectComp, initCompList
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: THERMOPACK, ref_len
    use stringmod,  only: uppercase
    use volume_shift, only: NOSHIFT
    use saft_interface, only: saft_type_eos_init
    use ideal, only: set_reference_energies
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), intent(in) :: potential !< Potential selection: "LJ", "LJS"
    character(len=*), optional, intent(in) :: model !< Model selection: "UV" (Default), "UF", "BH", "WCA"
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    integer                          :: ncomp, ncbeos, i, ierr, index, len_model
    character(len=3)                 :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    class(base_eos_param), pointer   :: act_eos_ptr
    character(len=ref_len)           :: param_ref
    character(len=7)                 :: model_local

    ! Initialize Pets eos
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model have been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()
    ! Set component list
    comps_upper="AR"
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    !
    if (present(model)) then
      len_model = min(3,len_trim(model))
      model_local = trim(potential)//"-"//uppercase(model(1:len_model))
    else
      model_local = trim(potential)//"-UV"
    endif
    call allocate_eos(ncomp, model_local)

    ! Number of phases
    act_mod_ptr%nph = 2

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    complist => act_mod_ptr%complist
    apparent => NULL()

    ! Set eos library identifyer
    act_mod_ptr%eosLib = THERMOPACK

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call SelectComp(complist,nce,param_ref,act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Initialize Thermopack
    act_eos_ptr => act_mod_ptr%eos(1)%p_eos
    act_eos_ptr%volumeShiftId = NOSHIFT
    act_eos_ptr%isElectrolyteEoS = .false.

    call saft_type_eos_init(nce,act_mod_ptr%comps,&
         act_eos_ptr,param_ref,silent_init=.true.)

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

    ! Initialize fallback eos
    act_mod_ptr%need_alternative_eos = .true.
    call init_fallback_and_redefine_criticals(silent=.true.)
  end subroutine init_lj_ljs

end module eoslibinit
