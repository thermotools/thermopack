module thermopack_constants
  ! Constants.
  implicit none
  save
  public

  real, parameter :: kB_const=1.380649e-23 !< J/K (Boltzmanns const)
  real, parameter :: h_const=6.626069311e-34  !< Js (Plancks constant)
  real, parameter :: N_Avogadro = 6.02214076e23
  real, parameter :: Rgas_default = N_Avogadro*kB_const
  !> Apply special treatement to phases with low consentrations of electrolytes
  real, parameter :: elEps = 1.0e-25
  !> Trace components, and zero components
  real, parameter :: traceEps = 1.0e-20
  !< Minimum mol number for apparent composition
  real, parameter :: min_mol_num = 1.0e-150
  !> String length
  integer, parameter :: clen=2048
  !> Control output of debug information
  logical :: verbose = .false.
  !> Phase identifiers
  integer, parameter :: TWOPH=0,LIQPH=1,VAPPH=2,MINGIBBSPH=3,&
       SINGLEPH=4,SOLIDPH=5,FAKEPH=6,VAPSOLPH=7
  !> Liquid phase type identifiers
  integer, parameter :: NONWATER=-1, WATER=-2
  !> Method to discriminate between liquid and vapor in the event of an
  !> undefined single phase
  integer, parameter :: PSEUDO_CRIT_ZFAC=1, PSEUDO_CRIT_MOLAR_VOLUME=2, &
       VOLUME_COVOLUME_RATIO=3
  !> Ignore components where z <= zLimit
  real, parameter :: zLimit = 0.0
  !> Continue on error?
  logical :: continueOnError = .false.
  !> Test type
  integer, parameter :: GRID = 1, ENVELOPE_PL = 2, &
       BINARY_PL = 3, BINARY_VLLE_PL = 4, META_LIMIT_PL = 5, SOLIDENVELOPE_PL = 6
  !> Property type
  integer, parameter :: PROP_OVERALL=0, PROP_RESIDUAL=1, PROP_IDEAL=2
  !> String lengths
  integer, parameter :: uid_len = 20
  integer, parameter :: ref_len = 40
  integer, parameter :: comp_name_len = 40
  integer, parameter :: formula_len = 20
  integer, parameter :: bibref_len = 100
  integer, parameter :: structure_len = 40
  integer, parameter :: eosid_len = 20
  integer, parameter :: mix_len = 20
  integer, parameter :: eos_name_len = 30
  integer, parameter :: short_label_len = 20
  integer, parameter :: label_len = 100
  integer, parameter :: element_len = 2

contains

  !----------------------------------------------------------------------
  subroutine phaseIntToName(phase,phaseName)
    integer, intent(in) :: phase
    character(len=*), intent(out) :: phaseName
    !
    if (len(phaseName) >= 10) then
      select case(phase)
      case (TWOPH)
        phaseName = 'TWOPH'
      case (LIQPH)
        phaseName = 'LIQPH'
      case (VAPPH)
        phaseName = 'VAPPH'
      case (MINGIBBSPH)
        phaseName = 'MINGIBBSPH'
      case (SINGLEPH)
        phaseName = 'SINGLEPH'
      case (SOLIDPH)
        phaseName = 'SOLIDPH'
      case (FAKEPH)
        phaseName = 'FAKEPH'
      end select
    endif
  end subroutine phaseIntToName

  !----------------------------------------------------------------------
  subroutine get_true(true_value)
    logical, intent(out) :: true_value
    true_value = .true.
  end subroutine get_true

end module thermopack_constants
