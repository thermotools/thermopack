!> Initialize thermodynamic models
!!
!! \author MH, 2014-02
module eoslibinit
  !
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
  public :: cleanup_eos, silent_init
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
  !> Initialize thermo library
  !>
  !> \author MH, 2014-02
  !----------------------------------------------------------------------
  subroutine init_thermo(eosLibrary,eos,mixing,alpha,ncomp,comp_string,nphases,&
       liq_vap_discr_method_in,csp_eos,csp_ref_comp,kij_setno,alpha_setno,&
       saft_setno,b_exponent,TrendEosForCp,cptype,silent)
    use parameters, only: clen, liq_vap_discr_method, initCompList_legacy, nc, TREND, &
         eosLib, model, THERMOPACK
    use tpselect,   only: SelectComp, SelectEOS
    use tpvar,      only: comp, cbeos, cbeos_alternative, nce, ncsym
    use stringmod,  only: uppercase
    use eosdata,    only: cpaSRK, cpaPR, eosPC_SAFT, eosPeTS, eosBH_pert
    !$ use omp_lib, only: omp_get_max_threads
    ! Method information
    character(len=*), intent(in) :: eosLibrary !< String defining eos library (TREND, ThermoPack)
    character(len=*), intent(in) :: eos    !< String defining equation of state
    character(len=*), intent(in) :: mixing !< String defining mixing rules
    character(len=*), intent(in) :: alpha  !< String defining alpha correlation
    integer, intent(in) :: ncomp !< Number of components
    character(len=*), intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
    integer, intent(in) :: nphases !< Number of phases
    integer, optional, intent(in) :: liq_vap_discr_method_in !< Method to discriminate between liquid and vapor in case of an undefined single phase. Will be set to none if absent.
    character(len=*), optional, intent(in) :: csp_eos !< Corrensponding state equation
    character(len=*), optional, intent(in) :: csp_ref_comp !< CSP component
    integer, optional, intent(in) :: kij_setno, alpha_setno, saft_setno(ncomp) !< Data set numbers
    real, optional, intent(in) :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    character(len=*), optional, intent(in) :: TrendEosForCp !< Option to init trend for ideal gas properties.
    integer, optional, intent(in) :: cptype(ncomp) !< Type numbers for Cp
    logical, optional, intent(in) :: silent !< Option to disable init messages.
    ! Locals
    character(len=clen) :: message
    integer             :: i, err, ncbeos
    character(len=len_trim(comp_string)) :: comp_string_upper
    character(len=len_trim(eosLibrary)) :: eosLibrary_cpy
    logical             :: isSAFTmodel
    real :: Tci, Pci, oi
    if (present(silent)) then
      silent_init = silent
    endif

    ! Store eos model information
    model = trim(eos)

    ! Method for discriminating between liquid/vapor when poorly defined
    if (present(liq_vap_discr_method_in)) then
      liq_vap_discr_method = liq_vap_discr_method_in
    end if

    ! Component list is always set, no matter the EoSlib.
    comp_string_upper=trim(uppercase(comp_string))
    call initCompList_legacy(ncomp,comp_string_upper)
    ncsym = ncomp
    ! Initialize components
    call SelectComp(trim(comp_string_upper),nce,comp)
    ! Set cptype
    if (present(cptype)) then
      do i=1,nc
        if (cptype(i) /= 0) then
          comp(i)%cptype = cptype(i)
        endif
      enddo
    endif

    eosLibrary_cpy = trim(uppercase(eosLibrary))
    if (nce /= nc .and. eosLibrary_cpy /= "THERMOPACK") then
      write(message,*) 'EoS library: ', trim(eosLibrary), '. Does not yet support electrolytes!'
      call stoperror(trim(message))
    endif

    ! Initialize the selected EoS-library
    select case (eosLibrary_cpy)
    case ("THERMOPACK")
      ! Initialize Thermopack
      call init_thermopack(trim(uppercase(eos)),trim(uppercase(mixing)), &
           trim(uppercase(alpha)), &
           nphases,csp_eos,csp_ref_comp, & ! csp_refcomp is case sensitive in compDB
           kij_setno,alpha_setno,saft_setno,&
           b_exponent)
      if (present(TrendEosForCp)) then
        ! Initialize Trend for ideal properties
        call init_trend(trim(uppercase(TrendEosForCp)),ncomp,nphases,.false.)
      endif
    case ("TREND")
      ! Initialize Trend
      call init_trend(trim(uppercase(eos)),ncomp,nphases,.true.)
    case default
      write(message,*) 'EoS library: ', trim(uppercase(eosLibrary)), '. Is not yet implemented.'
      call stoperror(trim(message))
    end select

    ! Initialize fallback-EoS (SRK from EoSlib Thermopack):
    ! Alternate SRK EoS for generating initial values, etc.
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    if (allocated(cbeos_alternative)) then
      deallocate(cbeos_alternative,STAT=err);
      if (err /= 0) call stoperror('Not able to deallocate cbeos_alternative')
    endif
    allocate(cbeos_alternative(ncbeos),STAT=err);
    if (err /= 0) call stoperror('Not able to allocate cbeos_alternative')
    ! Fill values of cbeos_alt according to standard SRK.
    ! If possible modify to match critical point and saturation line at T=0.7Tc
    if (EoSLib == TREND) then
      ! Use TREND parameters to get better critical point in alternative model
      do i=1,nce
        call trend_getcrit(i,Tci,Pci,oi)
        comp(i)%tc = Tci
        comp(i)%pc = Pci
        comp(i)%acf = oi
      enddo
    endif
    do i=1,ncbeos
      call SelectEOS(nce,comp,cbeos_alternative(i),"SRK","CLASSIC","CLASSIC")
    enddo

    if (EoSLib == THERMOPACK) then
       isSAFTmodel = (cbeos(1)%eosidx == cpaSRK .OR. &
            cbeos(1)%eosidx == cpaPR .OR. &
            cbeos(1)%eosidx == eosPC_SAFT .OR. &
            cbeos(1)%eosidx == eosPeTS .OR. &
            cbeos(1)%eosidx == eosBH_pert)
      if (isSAFTmodel .and. EoSLib == THERMOPACK) then
        call redefine_critical_parameters(silent_init)
      endif
    endif
  end subroutine init_thermo


  !----------------------------------------------------------------------------
  !> Initialize cubic EoS. Use: call init_cubic('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cubic(comps,eos,mixing,alpha,param_reference)
    use parameters, only: initCompList
    use tpselect,   only: SelectComp
    use tpvar,      only: comp, nce, ncsym
    use stringmod,  only: uppercase
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: eos     !< Equation of state
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    character(len=*), optional, intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp
    character(len=len_trim(comps)) :: comps_upper
    character(len=100)               :: mixing_loc, alpha_loc

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp)
    ncsym = ncomp

    ! Initialize components module
    call SelectComp(trim(comps_upper),nce,comp)

    ! Initialize Thermopack
    alpha_loc = "Classic"
    mixing_loc = "Classic"
    if (present(mixing)) mixing_loc = uppercase(mixing)
    if (present(alpha)) alpha_loc = uppercase(alpha)
    call init_thermopack(trim(uppercase(eos)),trim(uppercase(mixing_loc)), &
         trim(uppercase(alpha_loc)), nphase=2, kij_setno=1,alpha_setno=1)

  end subroutine init_cubic

  ! !----------------------------------------------------------------------------
  ! !> Initialize extended corresponding state EoS. Use: call init_extcsp
  ! !----------------------------------------------------------------------------
  ! subroutine init_extcsp(comps,eos,sh_eos,sh_mixing,sh_alpha,ref_eos,ref_comp)
  !   use parameters, only: initCompList
  !   use tpselect,   only: SelectComp
  !   use tpvar,      only: comp, nce, ncsym
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
    use tpvar,        only: comp, cbeos_alternative, nce
    use eosTV,        only: pressure
    use critical,     only: calcCriticalTV
    use saturation,   only: acentricFactorEos
    use tpconst,      only: tpTmin, Rgas
    use cbAlpha,      only: getAlphaClassicParam
    logical, intent(in) :: silent_init
    ! Locals
    integer :: i, j, ierr
    real :: Tmin
    real :: Vc
    real :: Tc  !< Specified critical temperature [K]
    real :: Pc  !< Specified critical pressure [Pa]
    real :: Acf !< Specified acentric factor [-]
    real :: Z(nce)
    Tmin = tpTmin
    tpTmin = 2.0
    do i=1,nce
      Z = 0
      Z(i) = 1
      Tc = -1.0
      Vc = -1.0
      call calcCriticalTV(Tc,Vc,Z,ierr)
      if (ierr /= 0 .and. .not. silent_init) then
        print *, 'Not able to redefine critical properties for component: ', trim(comp(i)%ident)
      else
        Pc = pressure(Tc,Vc,Z)
        cbeos_alternative(1)%single(i)%Tc = Tc
        cbeos_alternative(1)%single(i)%Pc = Pc
        call cbSingleCalcABC(nce,cbeos_alternative(1),i)
        comp(i)%tc = Tc
        comp(i)%pc = Pc
        comp(i)%zc = Pc*Vc/(Tc*Rgas)
        Acf = acentricFactorEos(i,ierr)
        comp(i)%acf = Acf
        cbeos_alternative(1)%single(i)%Acf = Acf
        call getAlphaClassicParam(i, cbeos_alternative(1), &
             cbeos_alternative(1)%single(i)%alphaParams)
        ! Copy to others
        do j=2,size(cbeos_alternative)
          cbeos_alternative(j)%single(i)%Tc = cbeos_alternative(1)%single(i)%Tc
          cbeos_alternative(j)%single(i)%Pc = cbeos_alternative(1)%single(i)%Pc
          cbeos_alternative(j)%single(i)%Acf = cbeos_alternative(1)%single(i)%Acf
          cbeos_alternative(j)%single(i)%alphaParams = &
               cbeos_alternative(1)%single(i)%alphaParams
          call cbSingleCalcABC(nce, cbeos_alternative(j), i)
        enddo
      endif
    enddo
    tpTmin = Tmin
  end subroutine redefine_critical_parameters

  !> Initialize ThermoPack
  !>
  !> \author MH, 2013-03-06
  !----------------------------------------------------------------------
  subroutine init_thermopack(eos,mixing,alpha,nphase,&
       csp_eos,csp_ref_comp,kij_setno,alpha_setno,saft_setno,&
       b_exponent)
    use stringmod, only: str_eq
    use parameters, only: EoSLib, nph, THERMOPACK, &
         complist, nc
    use tpselect, only: SelectEOS, deAllocateEosCubic
    use tpvar, only: nce, cbeos, comp
    use tpconst, only: set_constants
    use volume_shift, only: initVolumeShift, NOSHIFT
    use csp, only: csp_init
    use saft_interface, only: saft_type_eos_init
    use cpa_parameters, only: mixHasSelfAssociatingComp
    use assocschemeutils, only: no_assoc
    use saft_association, only: numAssocSites
    use stringmod, only: uppercase
    !$ use omp_lib
    ! Method information
    character(len=*), intent(in) :: eos    !< String defining equation of state
    character(len=*), intent(in) :: mixing !< String defining mixing rules
    character(len=*), intent(in) :: alpha  !< String defining alpha correlation
    integer, intent(in) :: nphase !< Number of phases
    character(len=*), optional, intent(in) :: csp_eos !< Corrensponding state equation
    character(len=*), optional, intent(in) :: csp_ref_comp !< CSP component
    integer, optional, intent(in) :: kij_setno, alpha_setno, saft_setno(nc) !< Data set number
    real, optional, intent(in) :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)
    ! String containing components on TPLib format
    character(len=100) :: mixRule,csp_refEos,csp_refcomp_str
    character(len=len(eos)) :: eosLocal  !< Local copy of string defining equation of state
    integer :: ncbeos, err, i, volumeShiftId
    !
    ncbeos = 1
    ! Create one instance of cbeos per thread
    !$ ncbeos = omp_get_max_threads()
    if (allocated(cbeos)) then
      do i=1,size(cbeos)
        call deAllocateEosCubic(cbeos(i))
      enddo
      deallocate(cbeos,STAT=err);
      if (err /= 0) call stoperror('Not able to deallocate cbeos')
    endif
    allocate(cbeos(ncbeos),STAT=err);
    if (err /= 0) call stoperror('Not able to allocate cbeos')

    ! Set parameters
    EoSLib = THERMOPACK
    nph = nphase
    call set_constants() ! Depends on EoSLib
    mixRule = trim(mixing)
    if (trim(uppercase(mixRule)) == 'VDW') then
      mixRule = 'Classic'
    endif
    volumeShiftId = NOSHIFT
    eosLocal = eos
    numAssocSites = 0
    if (len(eos) >= 3) then
      if (eos(1:3) == 'CPA') then
        if ( .not. mixHasSelfAssociatingComp(nc,trim(eosLocal),&
             complist,saft_setno)) then
          print *,'No self associating components. Initializing ',&
               eos(5:len(eos)),' instead of ',trim(eos)
          eosLocal = eos(5:len(eos))
          do i=1,nce
            comp(i)%assoc_scheme = no_assoc
          enddo
        endif
      endif
    endif

    if (trim(uppercase(eos)) == 'LK') then
      mixRule = 'Classic'
    else if (trim(uppercase(eos)) == 'SRK-PENELOUX') then
      eosLocal = 'SRK'
      volumeShiftId = InitVolumeShift(nc,comp,'Peneloux','SRK')
    else if (trim(uppercase(eos)) == 'SRKGB-PENELOUX') then
      eosLocal = 'SRKGB'
      volumeShiftId = InitVolumeShift(nc,comp,'Peneloux','SRK')
    else if (trim(uppercase(eos)) == 'PR-PENELOUX') then
      eosLocal = 'PR'
      volumeShiftId = InitVolumeShift(nc,comp,'Peneloux','PR')
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

    do i=1,ncbeos
      call SelectEOS(nce,comp,cbeos(i),trim(eosLocal),trim(mixRule),&
           trim(alpha),kij_setno,alpha_setno,b_exponent)
      cbeos(i)%volumeShiftId = volumeShiftId
      cbeos(i)%isElectrolyteEoS = .false.
    enddo

    ! SAFT initialization must be done after cbeos initialization.
    if (len(eos) >= 3) then
       if ( str_eq(eosLocal,'PC-SAFT') .or. &
            str_eq(eosLocal,'PETS') .or. &
            eosLocal(1:3) == 'CPA' .or. &
            str_eq(eosLocal,'SAFT-VR-MIE') .or. &
            str_eq(eosLocal,'LJS')) then
        do i=1,ncbeos
          call saft_type_eos_init(nce,comp,cbeos(i),saft_setno,silent_init)
        end do
      end if
    end if
  end subroutine init_thermopack

  !----------------------------------------------------------------------
  !> Initialize trend
  !>
  !> \author MH, 2013-04-10, EA 2014-02
  !----------------------------------------------------------------------
  subroutine init_trend(eos,ncomp,nphase,doFallbackInit)
    use parameters, only: complist,TREND,nph,EoSLib,verbose
    use tpconst, only: set_constants, Rgas
    use stringmod, only: chartoascii
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
    character (12*ncomp)            :: char_comps
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
  subroutine cleanup_eos()
    use parameters, only: complist
    use tpselect, only: deAllocateEosCubic
    use tpvar, only: comp, cbeos, cbeos_alternative
    use multiparameter_base, only: cleanup_meos
    integer :: stat, i
    !
    stat = 0
    if (allocated(complist)) deallocate(complist,STAT=stat)
    if (stat /= 0) write(*,*) 'Error deallocating complist!'
    if (allocated(comp)) deallocate(comp,STAT=stat)
    if (stat /= 0) write(*,*) 'Error deallocating comp!'
    if (allocated(cbeos)) then
      do i=1,size(cbeos)
        call deAllocateEosCubic(cbeos(i))
      enddo
      deallocate(cbeos,STAT=stat);
      if (stat /= 0) call stoperror('Not able to deallocate cbeos')
    endif
    if (allocated(cbeos_alternative)) then
      do i=1,size(cbeos_alternative)
        call deAllocateEosCubic(cbeos_alternative(i))
      enddo
      deallocate(cbeos_alternative,STAT=stat);
      if (stat /= 0) call stoperror('Not able to deallocate cbeos')
    endif

    call cleanup_meos() ! Clean up multiparameter EoS
  end subroutine cleanup_eos

  !----------------------------------------------------------------------------
  !> Initialize SAFT-VR-MIE EoS. Use: call init_saftvrmie('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_saftvrmie(comps,param_reference)
    use parameters, only: initCompList
    use tpselect,   only: SelectComp
    use tpvar,      only: comp, nce, ncsym
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp
    character(len=len_trim(comps))   :: comps_upper

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp)
    ncsym = ncomp

    ! Initialize components module
    call SelectComp(trim(comps_upper),nce,comp)

    ! Initialize Thermopack
    call init_thermopack("SAFT-VR-MIE", "Classic", "Classic", nphase=3)

  end subroutine init_saftvrmie

  !----------------------------------------------------------------------------
  !> Initialize PC-SAFT EoS. Use: call init_pcsaft('CO2,N2')
  !----------------------------------------------------------------------------
  subroutine init_pcsaft(comps,param_reference)
    use parameters, only: initCompList
    use tpselect,   only: SelectComp
    use tpvar,      only: comp, nce, ncsym
    use stringmod,  only: uppercase
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp
    character(len=len_trim(comps))   :: comps_upper

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp)
    ncsym = ncomp

    ! Initialize components module
    call SelectComp(trim(comps_upper),nce,comp)

    ! Initialize Thermopack
    call init_thermopack("PC-SAFT", "Classic", "Classic", nphase=3)

  end subroutine init_pcsaft

  !----------------------------------------------------------------------------
  !> Initialize CPA EoS. Use: call init_cpa('CO2,N2','PR', alpha='TWU')
  !----------------------------------------------------------------------------
  subroutine init_cpa(comps,eos,mixing,alpha,param_reference)
    use parameters, only: initCompList
    use tpselect,   only: SelectComp
    use tpvar,      only: comp, nce, ncsym
    use stringmod,  only: uppercase
    !$ use omp_lib, only: omp_get_max_threads
    character(len=*), intent(in) :: comps !< Components. Comma or white-space separated
    character(len=*), optional, intent(in) :: eos    !< Equation of state
    character(len=*), optional, intent(in) :: mixing !< Mixing rule
    character(len=*), optional, intent(in) :: alpha  !< Alpha correlation
    character(len=*), optional, intent(in) :: param_reference !< Data set reference
    ! Locals
    integer                          :: ncomp
    character(len=len_trim(comps))   :: comps_upper
    character(len=100)               :: mixing_loc, alpha_loc, eos_loc

    ! Set component list
    comps_upper=trim(uppercase(comps))
    call initCompList(comps_upper,ncomp)
    ncsym = ncomp

    ! Initialize components module
    call SelectComp(trim(comps_upper),nce,comp)

    ! Initialize Thermopack
    eos_loc = "SRK"
    alpha_loc = "Classic"
    mixing_loc = "Classic"
    if (present(mixing)) mixing_loc = uppercase(mixing)
    if (present(alpha)) alpha_loc = uppercase(alpha)
    if (present(eos)) eos_loc = uppercase(eos)

    call init_thermopack("CPA-"//trim(eos_loc),trim(uppercase(mixing_loc)), &
         trim(uppercase(alpha_loc)), nphase=3, kij_setno=1,alpha_setno=1)

  end subroutine init_cpa

end module eoslibinit
