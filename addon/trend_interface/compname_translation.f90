!< Module for translating Thermopack component names to possibly
!< different corresponding names in other EoSlibs.
!
!< \author EA, 2014-05

module compname_translation

  integer, parameter          :: complen = 64
  integer, parameter          :: nametablesize = 30
  
  type :: name_db_type
     sequence
     character (len=complen) :: univname   !< Universal ThermoPack name
     ! EoSlibs to translate to. OBS: The order matches the initialization below.
     character (len=complen) :: name_TREND !< TREND name
  end type name_db_type
  
  
  type(name_db_type), dimension(nametablesize), parameter :: name_db = &
    !               Univ.       TREND 
    (/ name_db_type("NH3",      "AMMONIA"),  &
       name_db_type("AR",       "ARGON"),  &
       name_db_type("BENZENE",  "BENZENE"), &
       name_db_type("NC4",      "BUTANE"), &
       name_db_type("CO",       "CO"), &
       name_db_type("CO2",      "CO2"), &
       name_db_type("NC10",     "DECANE"), &
       name_db_type("C2",       "ETHANE"), &
       name_db_type("C3",       "PROPANE"),  &
       name_db_type("H2S",      "H2S"), &
       name_db_type("HE",       "HELIUM"), &
       name_db_type("NC7",      "HEPTANE"), &
       name_db_type("NC6",      "HEXANE"), &
       name_db_type("H2",       "HYDROGEN"), &
       name_db_type("IC5",      "IPENTANE"), &
       name_db_type("IC4",      "ISOBUTAN"), &
       name_db_type("C1",       "METHANE"), &
       name_db_type("N2O",      "N2O"), &
       name_db_type("NE",       "NEON"), &
       name_db_type("N2",       "NITROGEN"), &
       name_db_type("NC10 ",    "DECANE"), &
       name_db_type("NC9 ",     "NONANE"), &
       name_db_type("NC8",      "OCTANE"), &
       name_db_type("O2",       "OXYGEN"), &
       name_db_type("NC5",      "PENTANE"), &
       name_db_type("NC4",      "PROPANE"), &
       name_db_type("SO2",      "SO2"), &
       name_db_type("H2O",      "WATER"), &
       name_db_type("PRLN",     "PROPYLEN"), &
       name_db_type("XE",       "XENON")  /)
  
    public :: translate_compname
    private

  contains
     
  !----------------------------------------------------------------------
  !> Translate component name from Thermopack naming scheme to 
  !> another EoSlib's possibly different naming scheme.
  !>
  !> \author EA, 2014-05
  !----------------------------------------------------------------------
  subroutine translate_compname(tpcompname, eoslib, newcompname)
    use parameters, only: verbose, TREND
    implicit none
    ! Input:
    character(len=*), intent(in)  :: tpcompname  !< Thermopack comp. name
    integer,          intent(in)  :: eoslib      !< EoSlib to translate to
    ! Output:
    character(len=*), intent(out) :: newcompname  !< Translated comp. name
    ! Internal:
    character(len=complen)        :: nametable(nametablesize)
    integer                       :: i
    
    ! Find correct nametable, or return the same name if none exists.
    select case(eoslib)
      case (TREND)
        nametable(:) = name_db(:)%name_TREND
      case default
        write(*,*) "No component name translation performed for this EoSlib"
        newcompname = trim(tpcompname)
        return
    end select
    ! Translate based on nametable set above.
    do i=1,nametablesize
      if (trim(tpcompname) == trim(name_db(i)%univname)) then
        ! Found table entry for component
        newcompname = trim(nametable(i))
        if (verbose) write(*,*) trim(tpcompname), "-->", trim(newcompname)
        return
      end if
    end do
    ! If no entry was found, assume that no translation is needed.
    if (verbose) write(*,*) "No translation for: ",trim(tpcompname)
    newcompname = trim(tpcompname)
    return
  end subroutine translate_compname
  
   

end module compname_translation


