!> Initialize thermodynamic models
!!
!! \author MH, 2014-02
module eoslibinit
  !
  use thermopack_var, only: nce, get_active_eos, thermo_model, &
    get_active_thermo_model, get_active_alt_eos, base_eos_param, add_eos, &
    active_thermo_model_is_associated, numAssocSites, Rgas, tpTmin, &
    kRgas
  use eos_container, only: allocate_eos
  use stringmod,  only: uppercase, str_eq, string_match
  implicit none
  save
  !
  logical :: silent_init = .false. ! Disable output for unit testing
  !
  private
  public :: init_thermo
  public :: init_cubic, init_cpa, init_saftvrmie, init_pcsaft, init_tcPR, init_quantum_cubic
  public :: init_cubic_pseudo
  public :: init_extcsp, init_lee_kesler, init_quantum_saftvrmie
  public :: init_multiparameter, init_pets, init_ljs, init_lj
  public :: silent_init
  public :: redefine_critical_parameters
  public :: init_volume_translation
  public :: init_ideal_eos
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
       saft_ref,b_exponent,cptype,silent)
    use thermopack_constants, only: clen
    use cbselect,   only: SelectCubicEOS
    use compdata,   only: init_component_data_from_db, initCompList
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
    integer, optional, intent(in) :: cptype !< Type numbers for Cp
    logical, optional, intent(in) :: silent !< Option to disable init messages.
    ! Locals
    integer :: ncomp !< Number of components
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Initialize components
    call init_component_data_from_db(complist,nce,"DEFAULT",act_mod_ptr%comps,ierr)
    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Set cptype
    if (present(cptype)) then
      do i=1,nc
        act_mod_ptr%comps(i)%p_comp%id_cp%cptype = cptype
      enddo
    endif

    ! Initialize Thermopack
    call init_thermopack(trim(uppercase(eos)),trim(uppercase(mixing)), &
         trim(uppercase(alpha)), &
         nphases,csp_eos,csp_ref_comp, & ! csp_refcomp is case sensitive in compDB
         kij_ref,Alpha_ref,saft_ref,&
         b_exponent)

    call init_fallback_and_redefine_criticals(silent_init)
  end subroutine init_thermo

  !----------------------------------------------------------------------------
  !> Initialize cubic fallback eos
  !----------------------------------------------------------------------------
  subroutine init_fallback_and_redefine_criticals(silent, enable_volume_shift)
    use thermopack_var, only: nce
    use eosdata, only: isSAFTEOS
    use cbselect, only: selectCubicEOS, SelectMixingRules
    use cubic_eos, only: cb_eos
    use eos_parameters, only: single_eos
    use volume_shift, only: InitVolumeShift
    !$ use omp_lib, only: omp_get_max_threads
    logical, intent(in) :: silent !< Option to disable init messages.
    logical, optional, intent(in) :: enable_volume_shift !< Initialize volume shift
    ! Locals
    integer             :: i
    real                :: Tci, Pci, rhocrit
    logical             :: redefine_critical
    type(thermo_model), pointer :: act_mod_ptr
    !
    act_mod_ptr => get_active_thermo_model()

    if (.not. act_mod_ptr%need_alternative_eos) return

    redefine_critical = isSAFTEOS(act_mod_ptr%eos(1)%p_eos%eosidx)

    select type(p_eos => act_mod_ptr%eos(1)%p_eos)
    class is (single_eos)
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

    if (present(enable_volume_shift)) then
      if (enable_volume_shift) then
        act_mod_ptr%cubic_eos_alternative(1)%p_eos%volumeShiftId = &
             InitVolumeShift(nce, act_mod_ptr%comps, &
             'PENELOUX', act_mod_ptr%cubic_eos_alternative(1)%p_eos%eosid)
      endif
    endif
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

    if (redefine_critical) then
      call redefine_critical_parameters(silent)
    endif
  end subroutine init_fallback_and_redefine_criticals

  !----------------------------------------------------------------------------
  !> Initialize cubic EoS. Use: call init_cubic('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cubic(comps,eos,mixing,alpha,parameter_reference,vol_shift)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var, only: nc, nce, ncsym, complist, nph, apparent
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

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
    if (string_match("tcPR", paramref_loc)) then
      alpha_loc = "TWU"
      volshift_loc = .True.
    end if

    ! Initialize components module
    call init_component_data_from_db(complist,nce,paramref_loc,act_mod_ptr%comps,ierr)

    ! Special handling of Quantum Cubic Peng-Robinson equation of state by Aasen
    ! et al. (10.1016/j.fluid.2020.112790)
    if (string_match("QuantumCubic", paramref_loc)) then
      alpha_loc = "TWU"
      volshift_loc = .True.
      beta_loc = "Quantum"
      do i=1,nc
        ! Set critical parameters according to Aasen et al. (10.1016/j.fluid.2020.112790)
        if (str_eq(complist(i),"HE")) then
          act_mod_ptr%comps(i)%p_comp%tc = 5.1953
          act_mod_ptr%comps(i)%p_comp%pc = 2.276e5
        else if (str_eq(complist(i),"H2")) then
          act_mod_ptr%comps(i)%p_comp%tc = 33.19
          act_mod_ptr%comps(i)%p_comp%pc = 12.964e5
        else if (str_eq(complist(i),"Ne")) then
          act_mod_ptr%comps(i)%p_comp%tc = 44.492
          act_mod_ptr%comps(i)%p_comp%pc = 26.79e5
        else if (str_eq(complist(i),"D2")) then
          act_mod_ptr%comps(i)%p_comp%tc = 38.34
          act_mod_ptr%comps(i)%p_comp%pc = 16.796e5
        end if
      enddo
    endif

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
  !> Initialize pseudo components of a cubic EoS. Use:
  !> call init_cubic("CO2,PSEUDO,PSEUDO")
  !> call init_cubic_pseudo(names=(/"", "C20", "C25"/), Tclist=(\0,300,400\), &
  !>                               Pclist=(\0,100e5,200e5\), acflist=(\0,0.3,0.5\))
  !----------------------------------------------------------------------------
  subroutine init_cubic_pseudo(comps, Tclist, Pclist, acflist, Mwlist, mixing, alpha)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var, only: nc
    use cbselect, only: selectCubicEOS, SelectMixingRules
    use cubic_eos, only: cb_eos
    use volume_shift, only: InitVolumeShift
    character(len=*), intent(in) :: comps          !< Components. Comma or white-space separated
    real, intent(in)             :: Tclist(nc)     !< List of critical temperatures (K)
    real, intent(in)             :: Pclist(nc)     !< List of critical pressures (Pa)
    real, intent(in)             :: acflist(nc)    !< List of acentric factors (-)
    real, intent(in), optional   :: Mwlist(nc)     !< List of molar masses (kg/mol)
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    ! Locals
    integer                          :: ncomp, i, index
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    logical                          :: is_pseudo_comp(nc)
    character(len=100)               :: mixing_loc, alpha_loc, paramref_loc
    logical                          :: volshift_loc

    ! Get a pointer to the active thermodynamics model
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model has been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()

    ! Special handling of pseudo components
    is_pseudo_comp = .false.

    ! Iterate through all components. If pseudo, update with corresponding value.
    do i=1,nc
      is_pseudo_comp(i) = str_eq("PSEUDO", act_mod_ptr%complist(i))
      if (is_pseudo_comp(i)) then
        act_mod_ptr%comps(i)%p_comp%Tc = Tclist(i)
        act_mod_ptr%comps(i)%p_comp%Pc = Pclist(i)
        act_mod_ptr%comps(i)%p_comp%acf = acflist(i)
        if (present(Mwlist)) act_mod_ptr%comps(i)%p_comp%Mw = Mwlist(i)
      end if
    enddo

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)

    mixing_loc = "vdW"
    alpha_loc = "Classic"
    paramref_loc = "DEFAULT"
    volshift_loc = .False.
    if (present(alpha)) alpha_loc = uppercase(alpha)
    if (present(mixing)) then
      mixing_loc = uppercase(mixing)
      if (str_eq(mixing_loc, "Classic")) mixing_loc = "VDW"
    end if

    ! Initialize Thermopack
    select type(p_eos => act_mod_ptr%eos(1)%p_eos)
    type is (cb_eos)
      call SelectCubicEOS(nc, act_mod_ptr%comps, &
        p_eos, alpha_loc, paramref_loc)

      call SelectMixingRules(nc, act_mod_ptr%comps, &
        p_eos, mixing_loc, paramref_loc)
    class default
      call stoperror("init_cubic: Should be cubic EOS")
    end select

    ! Distribute parameters from redefined eos
    do i=2,size(act_mod_ptr%eos)
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

  end subroutine init_cubic_pseudo

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
    parameter_reference = "tcPR/DEFAULT"
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
    call init_cubic(eos="PR", comps=comps, mixing=mixing, parameter_reference="QuantumCubic/tcPR/DEFAULT")
  end subroutine init_quantum_cubic

  !----------------------------------------------------------------------------
  !> Initialize extended corresponding state EoS. Use: call init_extcsp
  !----------------------------------------------------------------------------
  subroutine init_extcsp(comps,sh_eos,sh_mixing,sh_alpha,&
    ref_eos,ref_comp,ref_alpha,&
    parameter_ref)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var, only: nc, nce, ncsym, complist, nph, apparent
    use thermopack_constants, only: ref_len
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Set local variable for parameter reference
    if (present(parameter_ref)) then
      param_ref = parameter_ref
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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
        call calcCriticalTV(Tc,Vc,Z,ierr,v_min=vc*0.25,p=Pc)
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
    use thermopack_constants, only: ref_len
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

  !----------------------------------------------------------------------------
  !> Initialize SAFT-VR-MIE EoS. Use: call init_saftvrmie('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_saftvrmie(comps,parameter_reference)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nce, complist
    use thermopack_constants, only: ref_len
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

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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
  subroutine init_quantum_saftvrmie(comps,feynman_hibbs_order,additive_hs_ref,parameter_reference)
    use saftvrmie_options, only: NON_ADD_HS_REF, ADDITIVE_EXACT_HS_REF, saftvrmieaij_model_options
    use thermopack_constants, only: clen
    use saftvrmie_containers, only: svrm_opt
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    integer, optional, intent(in) :: feynman_hibbs_order
    logical, optional, intent(in) :: additive_hs_ref
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    ! Locals
    character(len=clen) :: param_ref !< Data set reference
    integer :: FH, FH_model, hs_ref
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
    hs_ref = NON_ADD_HS_REF
    if (present(additive_hs_ref)) then
      if (additive_hs_ref) hs_ref = ADDITIVE_EXACT_HS_REF
    endif
    call svrm_opt%saftvrmieaij_model_options(FH_model, hs_ref)
  end subroutine init_quantum_saftvrmie

  !----------------------------------------------------------------------------
  !> Initialize PC-SAFT EoS. Use: call init_pcsaft('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_pcsaft(comps,parameter_reference,simplified,polar)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: ref_len
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

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
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: ref_len
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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
  subroutine init_multiparameter(comps, meos, ref_state)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use stringmod,  only: uppercase
    use eos_parameters, only: single_eos, get_single_eos_pointer
    use multiparameter_base, only: REF_NO_SOLVE, REF_EVALUATE_ID, &
      REF_SOLVE_FOR_T, REF_SOLVE_FOR_P
    use saturation, only: safe_bubT, safe_bubP
    use eostv, only: entropy_tv, enthalpy_tv, Fideal
    use eos, only: specificvolume
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: meos !< Equation of state
    character(len=*), intent(in) :: ref_state !< Reference state ("DEFAULT", "IIR", "NBP", "ASHRAE", "IDGAS", "TRIPLE_POINT")
    ! Locals
    integer                          :: ncomp, index, ierr, ncbeos, i
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    character(len=len_trim(meos)+4)  :: eos_local !< Equation of state
    real :: T_ref, P_ref, v_ref, s_ref, h_ref, tmin, FI, FI_T
    real, allocatable :: x_ref(:), y_ref(:)
    integer :: phase_ref, solve_ref
    type(single_eos), pointer :: p_single_eos

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
    eos_local = trim(meos)
    complist => act_mod_ptr%complist
    call allocate_eos(ncomp, eos_local)

    ! Number of phases
    act_mod_ptr%nph = 3

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    act_mod_ptr%nc = ncomp
    nph = act_mod_ptr%nph
    apparent => NULL()
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Initialize components module
    call init_component_data_from_db(complist,nce,"DEFAULT",act_mod_ptr%comps,ierr)

    ! Set globals
    call update_global_variables_form_active_thermo_model()

    ! Initialize fallback eos
    act_mod_ptr%need_alternative_eos = .true.
    call init_fallback_and_redefine_criticals(silent=.true., enable_volume_shift=.true.)

    ! Calculate reference states
    if (str_eq(meos, "MEOS") .and. .not. str_eq(ref_state, "DEFAULT")) then
      ! Set reference entalpies and entropies
      ! call set_reference_energies(act_mod_ptr%comps)
      allocate(x_ref(nce),y_ref(nce))
      p_single_eos => get_single_eos_pointer(act_mod_ptr%eos(1)%p_eos)
      if (allocated(p_single_eos%nist)) then
        tmin = tptmin
        tptmin = 2.0
        do i=1,nce
          x_ref = 0
          x_ref(i) = 1
          call p_single_eos%nist(i)%meos%get_ref_state_spec(ref_state,T_ref,P_ref,phase_ref,solve_ref)
          if (solve_ref == REF_SOLVE_FOR_P) then
            P_ref = safe_bubP(T_ref,x_ref,y_ref,ierr)
          else if (solve_ref == REF_SOLVE_FOR_T) then
            T_ref = safe_bubT(P_ref,x_ref,y_ref,ierr)
          endif
          if (solve_ref == REF_EVALUATE_ID) then
            v_ref = T_ref*p_single_eos%nist(i)%meos%Rgas_meos/P_ref
            call Fideal(T_ref,v_ref,x_ref,F=FI,F_T=FI_T)
            h_ref = (1.d0 - T_ref*FI_T ) * p_single_eos%nist(i)%meos%Rgas_meos * T_ref
            s_ref = -(T_ref*FI_T + FI) * p_single_eos%nist(i)%meos%Rgas_meos
            call p_single_eos%nist(i)%meos%set_ref_state(T_ref,P_ref,v_ref,h_ref,s_ref)
          else if (solve_ref /= REF_NO_SOLVE) then
            call specificvolume(T_ref,P_ref,x_ref,phase_ref,v_ref)
            call enthalpy_tv(T_ref,v_ref,x_ref,h_ref)
            call entropy_tv(T_ref,v_ref,x_ref,s_ref)
            call p_single_eos%nist(i)%meos%set_ref_state(T_ref,P_ref,v_ref,h_ref,s_ref)
          endif
        enddo
        deallocate(x_ref,y_ref)
        tptmin = tmin
      endif
    endif
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=2,ncbeos
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo
  end subroutine init_multiparameter

  !----------------------------------------------------------------------------
  !> Initialize Pets EoS.
  !----------------------------------------------------------------------------
  subroutine init_pets(parameter_reference)
    use compdata, only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: ref_len
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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
    use compdata, only: init_component_data_from_db, initCompList
    use thermopack_var,  only: nc, nce, ncsym, complist, apparent, nph
    use thermopack_constants, only: ref_len
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
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)
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

  !----------------------------------------------------------------------------
  !> Initialize ideal EoS. Use: call init_ideal('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_ideal_eos(comps, ierr, parameter_reference)
    use compdata,   only: init_component_data_from_db, initCompList
    use ideal, only: set_reference_energies
    use thermopack_var, only: nc, nce, ncsym, complist, nph, apparent
    use thermopack_constants, only: ref_len
    use eos_container, only: allocate_eos
    character(len=*), intent(in) :: comps !< Component names. Comma separated
    character(len=*), optional, intent(in) :: parameter_reference !< Data set reference
    integer, intent(out)         :: ierr
    ! Locals
    integer                          :: ncomp, i, index
    character(len=len_trim(comps))   :: comps_upper
    type(thermo_model), pointer      :: act_mod_ptr
    character(len=ref_len)           :: param_ref
    ! Get a pointer to the active thermodynamics model
    if (.not. active_thermo_model_is_associated()) then
      ! No thermo_model has been allocated
      index = add_eos()
    endif
    act_mod_ptr => get_active_thermo_model()

    ! Set local variable for parameter reference
    if (present(parameter_reference)) then
      param_ref = parameter_reference
    else
      param_ref = "DEFAULT"
    endif

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp,act_mod_ptr%complist)
    call allocate_eos(ncomp, "IDEAL")

    ! Number of phases
    act_mod_ptr%nph = 1

    ! Assign active mode variables
    ncsym = ncomp
    nce = ncomp
    nc = ncomp
    nph = act_mod_ptr%nph
    act_mod_ptr%nc = ncomp
    complist => act_mod_ptr%complist
    apparent => NULL()
    Rgas = act_mod_ptr%Rgas
    kRgas = act_mod_ptr%kRgas

    ! Initialize components module
    call init_component_data_from_db(complist,nce,param_ref,act_mod_ptr%comps,ierr)

    ! Set reference entalpies and entropies
    call set_reference_energies(act_mod_ptr%comps)

    ! Distribute parameters from redefined eos
    do i=2,size(act_mod_ptr%eos)
      act_mod_ptr%eos(i)%p_eos = act_mod_ptr%eos(1)%p_eos
    enddo

  end subroutine init_ideal_eos

end module eoslibinit
