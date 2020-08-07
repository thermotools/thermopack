!> The module compdata stores pure component data.
!! After initialisation of a mixture, data from the database "compdatadb.f90"
!! are selected and copied into the working array "comp" of active components.
!!
!! Available CP-ideal correlations can vary, depending on the fluid.
!!
!! The ones that are in use are:
!!\verbatim
!! CPTYPE   - METHOD FOR IDEAL-GAS HEAT-CAPACITY CALCULATION                *
!!             - 1 : SHERWOOD, REID & PRAUSNITZ, THIRD EDITION              *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3                    (cal/gmol K) *
!!             - 2 : API-PROJECT 44                                         *
!!             - 3 : HYPOTETIC COMPONENTS                                   *
!!             - 4 : SHERWOOD, REID & PRAUSNITZ, FOURTH EDITION             *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3                    (J/mol K)    *
!!             - 5 : ICI (KRISTER STR\M)                                    *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3 + CP(5)/T**2           (kJ/kgK) *
!!             - 6 : CHEN, BENDER (PETTER NEKSÅ)                            *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3+ CP(5)*T**4        (kJ/kg K)    *
!!             - 7 : AIChE, Daubert and Danner, DIPPR-databasen             *
!!                   CP(ideal) = A + B[(C/T)/sinh(C/T)]**2                  *
!!                               + D[(E/T)/cosh(E/T)]**2      (J/(kmol K))  *
!!             - 8 : POLING, PRAUSNITZ & O'CONNEL, FIFTH EDITION            *
!!                   CP(ideal)/R = CP(1) + CP(2)*T + CP(3)*T**2 +           *
!!                               CP(4)*T**3 + CP(5)*T**4       (-)          *
!!             - 9 : Linear function and fraction (J/mol/K)                 *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)/(T + CP(4))        *
!!                                                                          *
!!             -10 : Leachman (NIST) and Valenta expression H2              *
!!                                                                          *
!!             -11 : Use TREND model                                        *
!! \endverbatim
!!
module compdata
  use thermopack_constants, only: uid_len, ref_len, bibref_len, eosid_len, eos_name_len, clen
  implicit none
  save
  public

  ! type, abstract :: basecomp
  !   character (len=uid_len) :: cid !< The component ID
  ! contains
  !   procedure, public :: isComponent
  ! end type basecomp

  ! type, extends(basecomp), abstract :: basecompref
  !   character (len=ref_len) :: ref !< Data group reference
  ! contains
  !   procedure, public :: isRef
  ! end type basecompref

  ! type, extends(basecompref), abstract :: basecompref_eos
  !   character (len=eosid_len) :: eosid !< EOS identifyer
  ! contains
  !   procedure, public :: isEOS
  ! end type basecompref_eos

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
       CP_TREND_SI=11

  type :: cpdata
    character (len=uid_len) :: cid !< The component ID
    character (len=ref_len) :: ref !< Data group reference
    character (len=bibref_len) :: bib_ref !> Bibliograpich reference
    integer :: cptype                     !> Correlation type (see above)
    real, dimension(10) :: cp             !> Cp correlation parameters
    real :: tcpmin                        !> Correlation lower temperature limit [K]
    real :: tcpmax                        !> Correlation upper temperature limit [K]
  !contains
  !  procedure :: assign_cpdata
  !  generic, public :: assignment(=) => assign_cpdata
  end type cpdata

  type :: alphadatadb
    character (len=uid_len) :: cid !< The component ID
    character (len=ref_len) :: ref !< Data group reference
    character (len=eosid_len) :: eosid !< EOS identifyer
    real :: coeff(3)
  end type alphadatadb

  type :: cidatadb
    character (len=uid_len) :: cid !< The component ID
    character (len=ref_len) :: ref !< Data group reference
    character (len=eosid_len) :: eosid !< EOS identifyer
    !character (len=bibref_len) :: bib_ref !< Bibliograpich reference
    real :: ci                            !< Volume shift (m3/mol)
  end type cidatadb

  type :: gendatadb
    character (len=uid_len) :: ident !< The component ID
    character (len=12) :: formula !< Chemical formula
    character (len=40) :: name !< The component name
    real :: mw !< Mole weight[g/mol]
    real :: tc !< Critical temperature [K]
    real :: pc !< Critical pressure [Pa]
    real :: zc !< Critical compressibility [-]
    real :: acf !< Acentric factor [-]
    real :: tb !< Normal boiling point [K]
    real :: ttr !< Triple point temperature [K]
    real :: ptr !< Triple point temperature [K]
    real :: sref !< Reference entropy [J/mol/K]
    real :: href !< Reference enthalpy [J/mol]
    real :: DfH !< Enthalpy of formation [J/mol]
    real :: DfG !< Gibbs energy of formation [J/mol]
    integer :: psatcode !< Vapour pressure correlation 1: Antoine 2: Wilson (Michelsen) 3: Starling
    real, dimension(3) :: ant !< Vapour pressure correlation parameters
    real :: tantmin !< Vapour pressure correlation lower temperature limit [K]
    real :: tantmax !< Vapour pressure correlation upper temperature limit [K]
    real :: zra !< Rackett compressibility factor
  !contains
  !  procedure :: assign_gendatadb
  !  generic, public :: assignment(=) => assign_gendatadb
  end type gendatadb

  type, extends(gendatadb) :: gendata
    type(cpdata) :: id_cp          !< Ideal gas Cp correlation
    real  :: ci                    !< Volume shift parameter [m3/mol]
    integer :: assoc_scheme        !< Association scheme for use in the SAFT model. The various schemes are defined in saft_parameters_db.f90.
  contains
    procedure, public :: init_from_name => gendata_init_from_name
    procedure :: assign_to_gendatadb
    !procedure :: assign_gendata
    !generic, public :: assignment(=) => assign_gendata
  end type gendata

  interface
    module subroutine gendata_init_from_name(c, cname, ref, ierr)
      class(gendata), intent(inout) :: c
      character(len=*), intent(in) :: cname
      character(len=*), intent(in) :: ref
      integer, intent(out) :: ierr
    end subroutine
  end interface

  type gendata_pointer
    class(gendata), pointer :: p_comp
  end type gendata_pointer

  public :: gendatadb, gendata, cpdata, alphadatadb, cidatadb
  public :: getComp, compIndex
  public :: parseCompVector, initCompList

contains

  !---------------------------------------------------------------------- >
  !> Is this component named cname?
  !!
  function isComponent(cid, cname) result(isComp)
    use stringmod, only: str_eq
    implicit none
    character(len=*), intent(in) :: cid
    character(len=*), intent(in) :: cname
    logical :: isComp

    isComp = str_eq(cname,cid)

  end function isComponent

  !---------------------------------------------------------------------- >
  !> Is referance tag in referance list?
  !!
  function isRef(ref, ref_list) result(isR)
    implicit none
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
    implicit none
    character(len=*), intent(in) :: eosid
    character(len=*), intent(in) :: eos
    logical :: isE

    ise = str_eq(eosid, eos)

  end function isEOS

  !---------------------------------------------------------------------- >
  !> Assignment operator for gendatadb
  !!
  ! subroutine assign_gendatadb(c1,c2)
  !   implicit none
  !   class(gendatadb), intent(inout) :: c1
  !   class(gendatadb), intent(in) :: c2

  !   c1%cid = c2%cid
  !   c1%formula = c2%formula
  !   c1%name = c2%name

  !   c1%mw = c2%mw
  !   c1%tc = c2%tc
  !   c1%pc = c2%pc
  !   c1%zc = c2%zc
  !   c1%acf = c2%acf
  !   c1%tb = c2%tb

  !   c1%psatcode = c2%psatcode
  !   c1%ant = c2%ant
  !   c1%tantmin = c2%tantmin
  !   c1%tantmax = c2%tantmax

  !   c1%zra = c2%zra

  !   c1%ttr = c2%ttr
  !   c1%ptr = c2%ptr
  !   c1%href = c2%href
  !   c1%sref = c2%sref
  !   c1%DfH = c2%DfH
  !   c1%DfG = c2%DfG

  ! end subroutine assign_gendatadb

  !---------------------------------------------------------------------- >
  !> Assignment operator for gendata
  !!
  subroutine assign_to_gendatadb(c1,c2)
    implicit none
    class(gendata), intent(inout) :: c1
    class(gendatadb), intent(in) :: c2

    c1%ident = c2%ident
    c1%formula = c2%formula
    c1%name = c2%name

    c1%mw = c2%mw
    c1%tc = c2%tc
    c1%pc = c2%pc
    c1%zc = c2%zc
    c1%acf = c2%acf
    c1%tb = c2%tb

    c1%psatcode = c2%psatcode
    c1%ant = c2%ant
    c1%tantmin = c2%tantmin
    c1%tantmax = c2%tantmax

    c1%zra = c2%zra

    c1%ttr = c2%ttr
    c1%ptr = c2%ptr
    c1%href = c2%href
    c1%sref = c2%sref
    c1%DfH = c2%DfH
    c1%DfG = c2%DfG

  end subroutine assign_to_gendatadb

  !   !---------------------------------------------------------------------- >
  ! !> Assignment operator for gendata
  ! !!
  ! subroutine assign_gendata(c1,c2)
  !   implicit none
  !   class(gendata), intent(inout) :: c1
  !   class(gendata), intent(in) :: c2

  !   call assign_gendatadb(c1, c2)
  !   c1%ci = c2%ci
  !   c1%id_cp = c2%id_cp
  !   c1%assoc_scheme = c2%assoc_scheme
  ! end subroutine assign_gendata

  ! !---------------------------------------------------------------------- >
  ! !> Assignment operator cpdata
  ! !!
  ! subroutine assign_cpdata(cp1,cp2)
  !   implicit none
  !   class(cpdata), intent(inout) :: cp1
  !   class(cpdata), intent(in) :: cp2
  !   cp1%cid = cp2%cid
  !   cp1%ref = cp2%ref
  !   cp1%bib_ref = cp2%bib_ref
  !   cp1%cptype = cp2%cptype
  !   cp1%cp = cp2%cp
  !   cp1%tcpmin = cp2%tcpmin
  !   cp1%tcpmax = cp2%tcpmax
  ! end subroutine assign_cpdata

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
    integer :: ipos, err, i, j
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
    ! Check for duplicates
    do i=1,ncomp
      do j=1,ncomp
        if (trim(complist(i)) == trim(complist(j)) .and. j /= i) then
          Call StopError('Duplicate in component list. Check input!')
        endif
      enddo
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

  !> Select all components in the mixture
  !! The parameters are:
  !!
  !!  \param comp_string The compontn string sperated either by "," or by " ".
  !!
  !! Example tpSelectComp ('C1 CO2 N2') or tpSelectComp ('C1,CO2,N2')
  !!
  !! \author Geir S
  subroutine SelectComp(complist,nc,ref,comp,ierr)
    implicit none
    character(len=*), intent(in) :: complist(:)
    integer, intent(in) :: nc
    character(len=*), intent(in) :: ref
    type(gendata_pointer), allocatable, dimension(:), intent(inout) :: comp
    integer, intent(out) :: ierr
    ! Loclas
    integer :: i, stat
    stat = 0
    if (allocated(comp)) then
      do i=1,size(comp)
        if (associated(comp(i)%p_comp)) deallocate (comp(i)%p_comp,STAT=stat)
        if (stat /= 0) write (*,*) 'Error deallocating p_comp'
      enddo
      deallocate (comp,STAT=stat)
      if (stat /= 0) write (*,*) 'Error deallocating comp'
    endif
    allocate (comp(nc),STAT=stat)
    if (stat /= 0) write (*,*) 'Error allocating comp'
    do i=1,nc
      allocate (comp(i)%p_comp,STAT=stat)
      if (stat /= 0) write (*,*) 'Error allocating p_comp'
      call comp(i)%p_comp%init_from_name(complist(i),ref,ierr)
    enddo
  end subroutine SelectComp

  !> Free the memory allocated after initializing components, compositions and
  !! equation of state
  !!
  !! \todo Need to check if this should be called prior to initializing.  From
  !! for instance excel and other non-sequantial 'look-up' type interfaces where
  !! the library need to initialized for each call.
  !!
  !! \author Geir S

  ! subroutine DeSelectComp(comp,cbeos)
  !   use thermopack_var, only: complist
  !   implicit none
  !   type (gendata), allocatable, dimension(:), intent(inout) :: comp
  !   type (eoscubic), intent(inout) :: cbeos
  !   integer :: stat = 0

  !   if (allocated (complist)) deallocate (complist,STAT=stat)
  !   if (stat /= 0) write (*,*) 'Error deallocating complist'

  !   if (allocated (comp)) deallocate (comp,STAT=stat)
  !   if (stat /= 0) write (*,*) 'Error deallocating comp'

  !   call deAllocateEosCubic(cbeos)

  ! end subroutine DeSelectComp


end module compdata
