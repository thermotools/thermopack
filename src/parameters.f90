!> This module contains global parameters
!!
!! \author MH, 2012-01-25
module parameters
  ! Parameters
  implicit none
  save

  !> String length
  integer, parameter :: clen=2048
  !> Control output of debug information
  logical :: verbose = .false.
  !> Phase identifiers
  integer, parameter :: TWOPH=0,LIQPH=1,VAPPH=2,MINGIBBSPH=3,&
       SINGLEPH=4,SOLIDPH=5,FAKEPH=6,VAPSOLPH=7
  !> Liquid phase type identifiers
  integer, parameter :: NONWATER=-1, WATER=-2
  !> Number of phases:
  integer :: nph=3
  !> Number of apparent components:
  integer :: nc=0
  !> Library used to solve EoS
  integer, parameter :: THERMOPACK=1, TREND=2
  integer :: EoSlib=0
  !> Description of model
  character(len=clen) :: model
  !> Method to discriminate between liquid and vapor in the event of an
  !> undefined single phase
  integer, parameter :: PSEUDO_CRIT_ZFAC=1, PSEUDO_CRIT_MOLAR_VOLUME=2, &
       VOLUME_COVOLUME_RATIO=3
  integer :: liq_vap_discr_method=PSEUDO_CRIT_MOLAR_VOLUME
  ! TPlib refererence state
  integer :: tplib_ref_state = 1
  !> Ignore components where z <= zLimit
  real, parameter :: zLimit = 0.0
  !> List of component names
  character (len=clen), allocatable, dimension(:) :: complist
  !> Continue on error?
  logical :: continueOnError = .false.
  !> Grid type option
  integer, parameter :: TPGRID = 1, SPGRID = 2, HPGRID = 3, UVGRID = 4, SVGRID = 5
  !> Test type
  integer, parameter :: GRID = 1, ENVELOPE_PL = 2, &
       BINARY_PL = 3, BINARY_VLLE_PL = 4, META_LIMIT_PL = 5, SOLIDENVELOPE_PL = 6

  public :: parseCompVector, compIndex, initCompList
  public :: continueOnError, phaseIntToName
  public :: isWaterComponent, calcLnPhiOffset, waterComponentFraction

contains

  function compIndex(compName) result(index)
    implicit none
    character(len=*), intent(in) :: compName
    integer :: index
    ! Locals
    integer :: i
    index = -1
    do i=1,nc
      if (trim(complist(i)) == trim(compName)) then
        index = i
        return
      endif
    enddo
    !call stoperror('Component '//trim(compName)//'not found in list of components')
  end function compIndex

  subroutine initCompList(ncomp,componentString)
    implicit none
    integer, intent(in) :: ncomp
    character(len=*), intent(in) :: componentString
    integer :: ipos, err, i, j
    character(len=clen) :: comp_string
    nc = ncomp
    comp_string = trim(componentString)

    ! Ensure the components are comma-delimited.
    if (contains_space(trim(comp_string))) then
      comp_string = space_delimited_to_comma_delimited(trim(comp_string))
    end if

    if (allocated(complist)) then
      deallocate(complist,STAT=err)
      if (err /= 0) Call StopError('Could not deallocate component list!')
    endif
    allocate(complist(nc),STAT=err)
    if (err /= 0) Call StopError('Could not allocate component list!')

    do i=1,nc
      ipos = getComp(trim(comp_string))
      complist(i)=comp_string(1:ipos)
      comp_string = comp_string(ipos+2:clen)
    enddo
    ! Check for duplicates
    do i=1,nc
      do j=1,nc
        if (trim(complist(i)) == trim(complist(j)) .and. j /= i) then
          Call StopError('Duplicate in component list. Check input!')
        endif
      enddo
    enddo

    if (verbose) then
      print *,'Component vector:'
      do i=1,nc
        print *,trim(complist(i))
      enddo
    endif
  end subroutine initCompList
  !----------------------------------------------------------------------
  function parseCompVector(compvector) result(nc)
    implicit none
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
  logical function contains_space(in_string)
    implicit none
    character(len=*), intent(in) :: in_string !< No leading or trailing spaces.
    ! Locals
    integer :: i

    contains_space = .false.
    do i = 1,len_trim(in_string)
      if (in_string(i:i) == ' ') then
        contains_space = .true.
        exit
      end if
    end do

  end function contains_space

  !----------------------------------------------------------------------
  function space_delimited_to_comma_delimited(in_string) result(out_string)
    implicit none
    character(len=*), intent(in) :: in_string !< Space-separated component string
    character(len=clen) :: out_string !< Comma-separated
    ! Locals
    integer :: i

    out_string = ''
    i = 1
    do while (i < len_trim(in_string)) ! (length of in_string after removing trailing space)
      if (in_string(i:i) == ' ') then
        out_string(i:i) = ','
        do while (in_string(i:i) == ' ' .and. i < len(in_string))
          i = i+1
        end do
      else
        out_string(i:i) = in_string(i:i)
        i = i+1
      end if
    end do

    out_string = trim(out_string) ! Remove trailing space.
  end function space_delimited_to_comma_delimited

  !----------------------------------------------------------------------
  function getComp(compvector) result(ipos)
    implicit none
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
  !----------------------------------------------------------------------
  subroutine phaseIntToName(phase,phaseName)
    implicit none
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

  ! Return true if component is water or water soluble
  function isWaterComponent(i) result(isWComp)
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

end module parameters
