module compdata_init
  use compdata
  use compdatadb, only: maxncdb, compdb, maxcpdb, cpdb
  implicit none
  save

  public :: getCompDBindex, getCpDBindex

contains

  !---------------------------------------------------------------------- >
  !> The function retun the index of the data record (in the module compdatadb)
  !! for the compoentent that is found
  !!
  !! \param compid Character string for a single component ID.
  !! \retval idx The array index of compdb.
  !!
  function getCompDBindex(compid) result(idx)
    use stringmod, only: str_eq
    implicit none
    character(len=*), intent(in) :: compid
    integer :: idx
    ! Locals
    logical :: found
    idx = 1
    found = .false.
    do while (idx <= maxncdb .and. .not. found)
      if (isComponent(compid,compdb(idx)%ident)) then
        found = .true.
        exit
      else
        idx = idx + 1
      endif
    enddo
    if (.not. found) then
      idx = 0
    endif
  end function getCompDBindex

  !---------------------------------------------------------------------- >
  !> The function retun the index of the data record (in the module compdatadb)
  !! for the cpdata that is found
  !!
  !! \param compid Character string for a single component ID.
  !! \param ref Reference tag
  !! \retval idx The array index of cpdb.
  !!
  function getCpDBindex(compid, ref) result(idx)
    use thermopack_constants, only: verbose
    use compdata
    implicit none
    character(len=*), intent(in) :: compid
    character(len=*), intent(in) :: ref
    integer :: idx
    ! Locals
    logical :: found
    integer :: idx_default
    idx_default = 0
    idx = 1
    found = .false.
    do while (idx <= maxcpdb .and. .not. found)
      if (isComponent(cpdb(idx)%cid,compid)) then
        if (isRef(cpdb(idx)%ref,ref)) then
          found = .true.
          exit
        else if (isRef(cpdb(idx)%ref,"Default")) then
          idx_default = idx
        endif
      endif
      idx = idx + 1
    enddo
    if (.not. found) then
      if (idx_default > 0) then
        idx = idx_default
        if (verbose) print *,trim(compid)//&
             "Did not find Cp correlation with reference "//&
             trim(ref)//&
             "Using default Cp correlation."
      else
        idx = 0
      endif
    endif
  end function getCpDBindex

  !---------------------------------------------------------------------- >
  ! Function to change the CP-method,
  !! \param comp_sting List of components
  !!
  !! \param Cptype Index for Cptype for the different components
  !!
  !! \retval component The component data - including the default choices where
  !! parameters for several methods are stored in the database.
  !!
  !!\verbatim
  !! Cptype:     - METHOD FOR IDEAL-GAS HEAT-CAPACITY CALCULATIONS            *
  !!
  !!             - 1 : SHERWOOD, REID & PRAUSNITZ, THIRD EDITION              *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3                    (cal/gmol K) *
  !!             - 2 : API-PROJECT 44                                         *
  !!             - 3 : HYPOTETIC COMPONENTS                                   *
  !!             - 4 : SHERWOOD, REID & PRAUSNITZ, FOURTH EDITION             *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3                    (J/mol K)    *
  !!             - 5 : ICI (KRISTER STR\M)
  !!                   Shomate eq. Data from NIST for CO2 (Geir S, Sep 2016)  *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3 + CP(5)/T**2           (kJ/kgK) *
  !!             - 6 : CHEN, BENDER (PETTER NEKSÅ)                            *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3+ CP(5)*T**4        (kJ/kg K)    *
  !!             - 7 : AIChE, Daubert and Danner, DIPPR-databasen             *
  !!                   CP(ideal) = A + B[(C/T)/sinh(C/T)]**2                  *
  !!                               + D[(E/T)/cosh(E/T)]**2      (J/(kmol K))  *
  !! \endverbatim
  !!
  subroutine set_cp(comps,comp_string,Cp_ref,ierr)
    use compdatadb
    use thermopack_constants, only: clen
    use thermopack_var, only: complist
    implicit none
    character(len=*), intent(in) :: comp_string
    character(len=*), intent(in) :: Cp_ref
    type(gendata_pointer), intent(inout) :: comps(:)
    integer, intent(out) :: ierr
    !
    integer :: index, icp

    ierr = 0
    icp = getCpDBindex(comp_string, Cp_ref)
    if (icp <= 0) then
      ierr = 1
      print *,"Not able to locate Cp data of "//trim(comp_string)//&
           " for reference "//trim(Cp_ref)
    endif

    index = compIndex(complist, comp_string)
    if (index <= 0) then
      ierr = 1
      print *,"Componet " // trim(comp_string) // ", not initialized"
    endif

    comps(index)%p_comp%id_cp = cpdb(icp)

  end subroutine set_cp

end module compdata_init

submodule (compdata) comp_init
  use compdatadb, only: compdb, cpdb, maxncdb
  use compdata_init, only: getCompDBindex, getCpDBindex
  implicit none

 contains

   !---------------------------------------------------------------------- >
   !> Init gendata from database
   !!
   module subroutine gendata_init_from_name(c, cname, ref, ierr)
     implicit none
     class(gendata), intent(inout) :: c
     character(len=*), intent(in) :: cname
     character(len=*), intent(in) :: ref
     integer, intent(out) :: ierr
     ! Locals
     integer :: cidx, cpidx
     ierr = 0
     cidx = getCompDBindex(cname)
     if (cidx < 1 .or. cidx > maxncdb) then
       call stoperror("Component "//trim(cname)//" not found in database")
     endif
     c = compdb(cidx)
     ! Set Cp
     cpidx = getCpDBindex(cname, ref)
     c%id_cp = cpdb(cpidx)
     ! Init parameters set elsewhere
     call c%cid%set_zero_vol_trs()
     c%assoc_scheme = 0
   end subroutine gendata_init_from_name

   module function comp_index_active(compName) result(index)
    use thermopack_var, only: complist
    implicit none
    character(len=*), intent(in) :: compName
    integer :: index
    !
    index = compIndex(complist, compName)
  end function comp_index_active

  module subroutine comp_name_active(index, comp_name)
    use thermopack_var, only: get_active_comps
    implicit none
    integer, intent(in) :: index
    character(len=*), intent(out) :: comp_name
    !
    type(gendata_pointer), pointer :: p_comps(:)
    p_comps => get_active_comps()
    comp_name = trim(p_comps(index)%p_comp%name)
  end subroutine

end submodule comp_init
