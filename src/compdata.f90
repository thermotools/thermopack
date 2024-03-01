!> The module compdata stores pure component data.
!! After initialisation of a mixture, data from the database "compdatadb.f90"
!! are selected and copied into the working array "comp" of active components.
!!
module compdata
  use thermopack_constants, only: uid_len, ref_len, bibref_len, eosid_len, eos_name_len, clen, &
       comp_name_len, formula_len, structure_len
  implicit none
  save
  public

  ! Correlations for ideal-gas heat-capacity calculation
  integer, parameter :: &
       CP_POLY3_CAL=1, &
       CP_API44_MASS=2, &
       CP_HYPOTETIC_MASS=3, &
       CP_POLY3_SI=4, &
       CP_ICI_MASS=5, &
       CP_CHEN_BENDER_MASS=6, &
       CP_DIPPR_KMOL=7, &
       CP_POLY4_SI=8, &
       CP_MOGENSEN_SI=9, &
       CP_H2_KMOL=10, &
       CP_TREND_SI=11, &
       CP_SHOMATE_SI=12, &
       CP_EINSTEIN_SI=13

  ! Maximum number of cp parameters
  integer, parameter :: n_max_cp=21

  !> Ideal heat capacity at constant pressure
  type :: cpdata
    character (len=uid_len) :: cid !< The component ID
    character (len=ref_len) :: ref !< Data group reference
    character (len=bibref_len) :: bib_ref !> Bibliograpich reference
    integer :: cptype                     !> Correlation type (see above)
    real, dimension(n_max_cp) :: cp       !> Cp correlation parameters
    real :: tcpmin                        !> Correlation lower temperature limit [K]
    real :: tcpmax                        !> Correlation upper temperature limit [K]
  !contains
  !  procedure :: assign_cpdata
  !  generic, public :: assignment(=) => assign_cpdata
  end type cpdata

  !> Alpha correlation for cubic EoS
  type :: alphadatadb
    character (len=uid_len) :: cid !< The component ID
    character (len=ref_len) :: ref !< Data group reference
    character (len=eosid_len) :: eosid !< EOS identifyer
    real :: coeff(3)
  end type alphadatadb

  integer, parameter :: VS_CONSTANT = 1, VS_LINEAR = 2, VS_QUADRATIC = 3, VS_QUINTIC = 6
  !> Volume shift parameters
  type :: cidatadb
    character (len=uid_len) :: cid !< The component ID
    character (len=ref_len) :: ref !< Data group reference
    character (len=eosid_len) :: eosid !< EOS identifyer
    character (len=bibref_len) :: bib_ref !< Bibliograpich reference
    integer :: c_type = VS_CONSTANT !< VS_CONSTANT, VS_LINEAR, VS_QUADRATIC
    real :: ciA = 0 !< Volume shift (m3/mol)
    real :: ciB = 0 !< Volume shift (m3/mol/K)
    real :: ciC = 0 !< Volume shift (m3/mol/K/K)
    real :: ciDD = 0 !< Volume shift (m3/mol/K/K/K)
    real :: ciE = 0 !< Volume shift (m3/mol/K/K/K/K)
    real :: ciF = 0 !< Volume shift (m3/mol/K/K/K/K/K)
  contains
    procedure, public :: get_vol_trs_c => cidatadb_get_vol_trs_c
    procedure, public :: set_zero_vol_trs => cidatadb_set_zero_vol_trs
  end type cidatadb

  !> Pure component parameters.
  !> This data structure stores pure component parameters for the CPA-SRK and
  !> CPA-PR equations of state.
  !------------------------------------------------------------------------------------------------
  type :: CPAdata
    character(len=eosid_len) :: eosid
    character (len=uid_len) :: compName
    ! SRK fitted parameters.
    real :: a0 !< [Pa*L^2/mol^2]. The usual a0 parameter.
    real :: b  !< [L/mol]. The usual b parameter.
    real :: alphaParams(3) !< Up to three parameters for use in alpha corr.
    integer :: alphacorridx !< Either cpaClassicIdx, cpaTwuIdx, cpaMcIdx.
    ! Association parameters.
    real :: eps  !< [J/mol]. Caveat: people sometimes tabulate epsilon/R.
    real :: beta !< [-]
    ! Association scheme.
    integer :: assoc_scheme
    ! Fitting method used to obtain the parameters.
    character(len=ref_len) :: ref
    character(len=bibref_len) :: bib_reference
  end type CPAdata

  type :: gendatadb
    character (len=uid_len) :: ident !< The component ID
    character (len=formula_len) :: formula !< Chemical formula
    character (len=comp_name_len) :: name !< The component name
    character (len=structure_len) :: structure !< Molecular structure
    real :: mw !< Mole weight[g/mol]
    real :: tc !< Critical temperature [K]
    real :: pc !< Critical pressure [Pa]
    real :: zc !< Critical compressibility [-]
    real :: acf !< Acentric factor [-]
    real :: tb !< Normal boiling point [K]
    real :: ttr !< Triple point temperature [K]
    real :: ptr !< Triple point temperature [K]
    real :: sref !< Reference entropy [J/mol/K]
    character (len=uid_len) :: sref_state !< Entropy reference state pressure
    real :: href !< Reference enthalpy [J/mol]
    real :: gref !< Reference Gibbs free energy (1bar reference) [J/mol]
    integer :: psatcode !< Vapour pressure correlation 1: Antoine 2: Wilson (Michelsen) 3: Starling
    real, dimension(3) :: ant !< Vapour pressure correlation parameters
    real :: tantmin !< Vapour pressure correlation lower temperature limit [K]
    real :: tantmax !< Vapour pressure correlation upper temperature limit [K]
    real :: zra !< Rackett compressibility factor
    real :: mu_dipole !< Electric dipole moment (D)
    real :: q_quadrupole !< Electric quadrupole moment (ÅD)
  contains
    ! Assignment operator
    procedure, pass(This), public :: assign_comp => assign_gendatadb
    generic, public :: assignment(=) => assign_comp
  end type gendatadb

  type, extends(gendatadb) :: gendata
    type(cpdata) :: id_cp          !< Ideal gas Cp correlation
    type(cidatadb) :: cid          !< Volume shift parameters
    integer :: assoc_scheme        !< Association scheme for use in the SAFT model. The various schemes are defined in saft_parameters_db.f90.
    real :: sref_int = 0 !< Entropy integration constant [J/mol/K]
    real :: href_int = 0 !< Enthalpy integration constants [J/mol]
  contains
    procedure, public :: init_from_name => gendata_init_from_name
    ! Assignment operator
    procedure, pass(This), public :: assign_comp => assign_gendata
  end type gendata

  interface
    module subroutine gendata_init_from_name(c, cname, ref, ierr)
      class(gendata), intent(inout) :: c
      character(len=*), intent(in) :: cname
      character(len=*), intent(in) :: ref
      integer, intent(out) :: ierr
    end subroutine
  end interface

  interface
    module function comp_index_active(compName) result(index)
      character(len=*), intent(in) :: compName
      integer :: index
    end function comp_index_active
  end interface

  interface
    module subroutine comp_name_active(index, shortname, comp_name)
      integer, intent(in) :: index
      logical, intent(in) :: shortname
      character(len=*), intent(out) :: comp_name
    end subroutine comp_name_active
  end interface

  interface
    module subroutine comp_structure(cname, struct)
      character(len=*), intent(in) :: cname
      character(len=*), intent(out) :: struct
    end subroutine comp_structure
  end interface

  interface
    module subroutine set_ideal_cp_correlation(index, correlation, parameters)
      integer, intent(in) :: index
      integer, intent(in) :: correlation
      real, intent(in) :: parameters(n_max_cp)
    end subroutine set_ideal_cp_correlation
  end interface

  interface
    module subroutine get_ideal_cp_correlation(index, correlation, parameters)
      integer, intent(in) :: index
      integer, intent(out) :: correlation
      real, intent(out) :: parameters(n_max_cp)
    end subroutine get_ideal_cp_correlation
  end interface

  type gendata_pointer
    class(gendata), pointer :: p_comp => NULL()
  end type gendata_pointer

  public :: gendatadb, gendata, cpdata, alphadatadb, cidatadb, n_max_cp
  public :: getComp, compIndex, copy_comp, comp_index_active, comp_name_active
  public :: parseCompVector, initCompList, deallocate_comp
  public :: get_ideal_cp_correlation, set_ideal_cp_correlation
  public :: comp_structure

contains

  !---------------------------------------------------------------------- >
  !> Is this component named cname?
  !!
  function isComponent(cid, cname) result(isComp)
    use stringmod, only: str_eq
    character(len=*), intent(in) :: cid
    character(len=*), intent(in) :: cname
    logical :: isComp

    isComp = str_eq(cname,cid)

  end function isComponent

  !---------------------------------------------------------------------- >
  !> Is referance tag in referance list?
  !!
  function isRef(ref, ref_list) result(isR)
    character(len=*), intent(in) :: ref
    character(len=*), intent(in) :: ref_list
    logical :: isR

    isR = (index(ref, ref_list) > 0)

  end function isRef

  !---------------------------------------------------------------------- >
  !> Is it match in eosid?
  !!
  function isEOS(eosid, eos) result(isE)
    use stringmod, only: str_eq
    character(len=*), intent(in) :: eosid
    character(len=*), intent(in) :: eos
    logical :: isE

    ise = str_eq(eosid, eos)

  end function isEOS

  !---------------------------------------------------------------------- >
  !> Assignment operator for gendatadb
  !!
  subroutine assign_gendatadb(this,cmp)
    class(gendatadb), intent(inout) :: this
    class(*), intent(in) :: cmp

    select type (pc => cmp)
    class is (gendatadb)
      this%ident = pc%ident
      this%formula = pc%formula
      this%name = pc%name

      this%mw = pc%mw
      this%tc = pc%tc
      this%pc = pc%pc
      this%zc = pc%zc
      this%acf = pc%acf
      this%tb = pc%tb

      this%psatcode = pc%psatcode
      this%ant = pc%ant
      this%tantmin = pc%tantmin
      this%tantmax = pc%tantmax

      this%zra = pc%zra
      this%mu_dipole = pc%mu_dipole
      this%q_quadrupole = pc%q_quadrupole

      this%ttr = pc%ttr
      this%ptr = pc%ptr
      this%href = pc%href
      this%sref = pc%sref
      this%sref_state = pc%sref_state
      this%structure = pc%structure
    end select
  end subroutine assign_gendatadb

  !---------------------------------------------------------------------- >
  !> Assignment operator for gendata
  !!
  subroutine assign_gendata(this,cmp)
    class(gendata), intent(inout) :: this
    class(*), intent(in) :: cmp

    select type (pc => cmp)
    class is (gendata)
      call assign_gendatadb(this, pc)
      this%cid = pc%cid
      this%id_cp = pc%id_cp
      this%assoc_scheme = pc%assoc_scheme
      this%href_int = 0
      this%sref_int = 0

    class is (gendatadb)
      call assign_gendatadb(this, pc)

    end select
  end subroutine assign_gendata

  function compIndex(complist, compName) result(index)
    character (len=*), intent(in) :: complist(:)
    character(len=*), intent(in) :: compName
    integer :: index
    ! Locals
    integer :: i
    index = -1
    do i=1,size(complist)
      if (trim(complist(i)) == trim(compName)) then
        index = i
        return
      endif
    enddo
    !call stoperror('Component '//trim(compName)//'not found in list of components')
  end function compIndex

  subroutine initCompList(componentString, ncomp, complist)
    use stringmod, only: clen, count_substr, contains_space, &
         space_delimited_to_comma_delimited
    use thermopack_constants, only: verbose
    character(len=*), intent(in) :: componentString
    integer, intent(out) :: ncomp
    character (len=*), allocatable, dimension(:), intent(inout) :: complist !> List of component names
    !
    integer :: ipos, err, i
    character(len=clen) :: comp_string

    comp_string = trim(componentString)

    ! Ensure the components are comma-delimited.
    if (contains_space(trim(comp_string))) then
      comp_string = space_delimited_to_comma_delimited(trim(comp_string))
    end if

    ncomp = count_substr(str=comp_string, substr=',') + 1

    if (allocated(complist)) then
      deallocate(complist,STAT=err)
      if (err /= 0) Call StopError('Could not deallocate component list!')
    endif
    allocate(complist(ncomp),STAT=err)
    if (err /= 0) Call StopError('Could not allocate component list!')

    do i=1,ncomp
      ipos = getComp(trim(comp_string))
      complist(i)=comp_string(1:ipos)
      comp_string = comp_string(ipos+2:clen)
    enddo

    if (verbose) then
      print *,'Component vector:'
      do i=1,ncomp
        print *,trim(complist(i))
      enddo
    endif

  end subroutine initCompList

  !----------------------------------------------------------------------
  function parseCompVector(compvector) result(nc)
    character(len=*), intent(in) :: compvector
    integer :: nc
    ! Locals
    character(len=1), parameter :: delim = ','
    character(len=1), parameter :: space = ' '
    integer :: strLen, i, lastI
    strLen = len(compvector)
    nc = 0
    lastI = 0
    do i=2,strLen
      if (compvector(i:i) == delim .or. compvector(i:i)== space) then
        if (i > lastI) then
          nc = nc + 1
        endif
        lastI = i
      endif
    enddo
    if (strLen > lastI) then
      nc = nc + 1
    endif
  end function parseCompVector

  !----------------------------------------------------------------------
  function getComp(compvector) result(ipos)
    character(len=*), intent(in) :: compvector
    integer :: ipos
    ! Locals
    character(len=1), parameter :: delim = ','
    character(len=1), parameter :: space = ' '
    integer :: strLen, i
    strLen = len(compvector)
    ipos = strLen
    do i=2,strLen
      if (compvector(i:i) == delim .or. compvector(i:i) == space) then
        ipos = i - 1
        return
      endif
    enddo
  end function getComp

  !> Initialize component data from compdatadb
  subroutine init_component_data_from_db(complist,nc,ref,comp,ierr)
    character(len=*), intent(in) :: complist(:) !< List of component names
    integer, intent(in) :: nc !< Number of components
    character(len=*), intent(in) :: ref !< Reference for ideal cp correlation
    type(gendata_pointer), allocatable, dimension(:), intent(inout) :: comp !< Pointer to structure for holding data
    integer, intent(out) :: ierr ! <Error flag (0 means success)
    ! Loclas
    integer :: i, stat
    call deallocate_comp(comp)
    allocate (comp(nc),STAT=stat)
    if (stat /= 0) write (*,*) 'Error allocating comp'
    do i=1,nc
      allocate (comp(i)%p_comp,STAT=stat)
      if (stat /= 0) write (*,*) 'Error allocating p_comp'
      call comp(i)%p_comp%init_from_name(complist(i),ref,ierr)
    enddo
  end subroutine init_component_data_from_db

  subroutine deallocate_comp(comp)
    type(gendata_pointer), allocatable, dimension(:), intent(inout) :: comp
    ! Loclas
    integer :: stat, i
    stat = 0
    if (allocated(comp)) then
      do i=1,size(comp)
        if (associated(comp(i)%p_comp)) deallocate (comp(i)%p_comp,STAT=stat)
        if (stat /= 0) write (*,*) 'Error deallocating p_comp'
      enddo
      deallocate (comp,STAT=stat)
      if (stat /= 0) write (*,*) 'Error deallocating comp'
    endif
  end subroutine deallocate_comp

  subroutine copy_comp(comp_cpy, comp)
    type(gendata_pointer), allocatable, dimension(:), intent(inout) :: comp_cpy
    type(gendata_pointer), allocatable, dimension(:), intent(in) :: comp
    ! Loclas
    integer :: stat, i
    if (allocated(comp)) then
      stat = 0
      if (allocated(comp_cpy)) then
        call deallocate_comp(comp_cpy)
        allocate(comp_cpy(size(comp)), stat=stat)
      else
        allocate(comp_cpy(size(comp)), stat=stat)
      endif
      if (stat /= 0) write (*,*) 'Error allocating comp'
      do i=1,size(comp)
        if (associated(comp(i)%p_comp)) then
          if (.not. associated(comp_cpy(i)%p_comp)) then
            allocate (comp_cpy(i)%p_comp,STAT=stat)
            if (stat /= 0) write (*,*) 'Error allocating p_comp'
          endif
          comp_cpy(i)%p_comp = comp(i)%p_comp
        endif
      enddo
    else
      call deallocate_comp(comp_cpy)
    endif
  end subroutine copy_comp

  subroutine cidatadb_get_vol_trs_c(cid, T, ci, cit, citt, ci_temp_dep)
    class(cidatadb), intent(in) :: cid
    real, intent(in) :: T !< Temperature (K)
    real, intent(out) :: ci !< Volume translation (m3/mol)
    real, intent(out) :: cit !< Volume translation differential (m3/mol/K)
    real, intent(out) :: citt !< Volume translation second differential (m3/mol/K2)
    logical, intent(out) :: ci_temp_dep !< Volume translation is temp. dependent
    ! Loclas
    select case(cid%c_type)
    case(VS_CONSTANT)
      ci = cid%ciA
      cit = 0
      citt = 0
      ci_temp_dep = .false.
    case(VS_LINEAR)
      ci = cid%ciA + cid%ciB*T
      cit = cid%ciB
      citt = 0
      ci_temp_dep = .true.
    case(VS_QUADRATIC)
      ci = cid%ciA + cid%ciB*T + cid%ciC*T**2
      cit = cid%ciB + 2*cid%ciC*T
      citt = 2*cid%ciC
      ci_temp_dep = .true.
    case(VS_QUINTIC)
      ci = cid%ciA + cid%ciB*T + cid%ciC*T**2 + cid%ciDD*T**3 + cid%ciE*T**4 + cid%ciF*T**5
      cit = cid%ciB + 2*cid%ciC*T + 3*cid%ciDD*T**2 + 4*cid%ciE*T**3 + 5*cid%ciF*T**4
      citt = 2*cid%ciC + 6*cid%ciDD*T + 12*cid%ciE*T**2 + 20*cid%ciF*T**3
      ci_temp_dep = .true.
    case default
      ci = 0
      cit = 0
      citt = 0
      ci_temp_dep = .false.
    end select
  end subroutine cidatadb_get_vol_trs_c

  subroutine cidatadb_set_zero_vol_trs(cid)
    implicit none
    class(cidatadb), intent(inout) :: cid
    cid%ciA = 0
    cid%ciB = 0
    cid%ciC = 0
    cid%ciDD = 0
    cid%ciE = 0
    cid%ciF = 0
    cid%c_type = VS_CONSTANT
  end subroutine cidatadb_set_zero_vol_trs

end module compdata
