!> Functionality for working with association schemes. The work mostly consists
!> of keeping track of indices.
module AssocSchemeUtils
  use thermopack_constants, only: verbose
  use association_var, only: association
  implicit none
  save

  ! An association scheme is nothing but a number of positive sites and a number
  ! of negative sites. Implementing sites as such pairs would be cleaner and
  ! more extensible.

  !> Association schemes.                   ! NONZERO INTERACTIONS FOR SCHEMES:
  integer, parameter :: assoc_scheme_1  = 1 !< AA
  integer, parameter :: assoc_scheme_2A = 2 !< AA AB BB
  integer, parameter :: assoc_scheme_2B = 3 !< AB
  integer, parameter :: assoc_scheme_2C = 4 !< AA AB, where A is a bipolar site that associates with all other sites
  integer, parameter :: assoc_scheme_3A = 5 !< AA AB AC BB BC CC
  integer, parameter :: assoc_scheme_3B = 6 !< AC BC
  integer, parameter :: assoc_scheme_4A = 7 !< AA AB AC AD BB BC BD CC CD DD
  integer, parameter :: assoc_scheme_4B = 8 !< AD BD CD
  integer, parameter :: assoc_scheme_4C = 9 !< AC AD BC BD
  integer, parameter :: assoc_scheme_1ea = 10 !< One site with positive polarity
  integer, parameter :: no_assoc = -1       !< No self-association.

  !> Combining rules
  integer, parameter :: ariComb = 1 !< Arithmetic mean combining rule.
  integer, parameter :: geoComb = 0 !< Geometric mean combining rule.

  integer, parameter :: noSitesFlag = -1

contains

  subroutine assocIndices_bookkeeping (assoc, nc, saft_model, assocSchemes_db)
    use eosdata, only: eosPC_SAFT, eosSAFT_VR_MIE, eosPeTS, eosLJS_BH, &
         get_eos_short_label_from_subidx, eosLJS_WCA, eosLJS_UV, eosLJ_UF
    type(association), intent(inout) :: assoc
    integer, intent(in) :: nc
    integer, intent(in) :: saft_model
    integer, intent(in) :: assocSchemes_db(nc)
    ! Locals
    integer :: ic
    integer :: assocCompIdcs(nc)

    if (allocated(assoc%comp_vs_sites)) deallocate(assoc%comp_vs_sites)
    allocate(assoc%comp_vs_sites(nc,2))

    ! Iterate through the components, count the number of self-associating ones,
    ! as well as the total number of association sites.
    assoc%numAssocComps = 0
    assoc%numAssocSites = 0
    do ic=1,nc
       ! Count numAssocComps.
       if (assocSchemes_db(ic) .ne. no_assoc) then
          assoc%numAssocComps = assoc%numAssocComps + 1
          assocCompIdcs(assoc%numAssocComps) = ic
       end if
       ! Count association sites, and construct a mapping from component
       ! indices to site numbers.
       select case (assocSchemes_db(ic))
       case (assoc_scheme_1)
          assoc%numAssocSites = assoc%numAssocSites + 1
          assoc%comp_vs_sites(ic,1) = assoc%numAssocSites
          assoc%comp_vs_sites(ic,2) = assoc%numAssocSites
       case (assoc_scheme_2A,assoc_scheme_2B,assoc_scheme_2C)
          assoc%numAssocSites = assoc%numAssocSites + 2
          assoc%comp_vs_sites(ic,1) = assoc%numAssocSites - 1
          assoc%comp_vs_sites(ic,2) = assoc%numAssocSites
       case (assoc_scheme_3A,assoc_scheme_3B)
          assoc%numAssocSites = assoc%numAssocSites + 3
          assoc%comp_vs_sites(ic,1) = assoc%numAssocSites - 2
          assoc%comp_vs_sites(ic,2) = assoc%numAssocSites
       case (assoc_scheme_4A,assoc_scheme_4B,assoc_scheme_4C)
          assoc%numAssocSites = assoc%numAssocSites + 4
          assoc%comp_vs_sites(ic,1) = assoc%numAssocSites - 3
          assoc%comp_vs_sites(ic,2) = assoc%numAssocSites
       case (no_assoc)
          assoc%comp_vs_sites(ic,:) = noSitesFlag
       end select
    end do

    if (allocated(assoc%compidcs)) deallocate(assoc%compidcs)
    allocate(assoc%compidcs(assoc%numAssocComps))

    assoc%compidcs(:) = assocCompIdcs(1:assoc%numAssocComps)

    if (assoc%numAssocSites .eq. 0) then
       ! No associating components: exit routine.
      if ( saft_model == eosPC_SAFT .or. &
           saft_model == eosSAFT_VR_MIE .or. &
           saft_model == eosPeTS .or. &
           saft_model == eosLJS_BH .or. &
           saft_model == eosLJS_WCA .or. &
           saft_model == eosLJS_UV .or. &
           saft_model == eosLJ_UF) then
        if (verbose) print *, "Using " // get_eos_short_label_from_subidx(saft_model) &
             // " with no associating components."
        return
      else
        call stoperror("At least one CPA-component must self-associate.")
      end if
    end if

  end subroutine assocIndices_bookkeeping

  !*************************** INDEX FUNCTIONS ***************************!
  !> Gives the component number ic to which association site number k belongs.
  INTEGER FUNCTION site_to_compidx(assoc,k)
    type(association), intent(in) :: assoc
    integer, intent(in)  :: k !< the association site number
    integer :: ic

    ic = 1
    do while (.true.)
       if (assoc%comp_vs_sites(ic,2) >= k .and. k >= assoc%comp_vs_sites(ic,1)) then
          site_to_compidx = ic
          return
       end if
       ic = ic+1
    end do
  end FUNCTION site_to_compidx

  !> Gives the first and last association sites, k_first and k_last, for comp i.
  subroutine compidx_to_sites(assoc,ic,k_first,k_last)
    type(association), intent(in) :: assoc
    integer, intent(in)  :: ic  !< the original component index
    integer, intent(out) :: k_first !< the first association site number for comp i
    integer, intent(out) :: k_last  !< the last association site number for comp i

    k_first = assoc%comp_vs_sites(ic,1)
    k_last = assoc%comp_vs_sites(ic,2)
  end subroutine compidx_to_sites

  !> Implements the combining rules for eps and beta seen in CPA models.
  function applyCombiningRule (ruleIdx,val1,val2)
    real :: applyCombiningRule
    real, intent(in) :: val1, val2
    integer, intent(in) :: ruleIdx

    if ( ruleIdx == ariComb ) then
       applyCombiningRule = (val1+val2)/2
    else if (ruleIdx == geoComb) then
       applyCombiningRule = sqrt(val1*val2)
    else
       print *, "Comb rule inputted:", ruleIdx
       call stoperror("No such combining rule.")
    end if
  end function applyCombiningRule

  !> Returns true if there is nonzero interaction between site1 and site2,
  !> according to association scheme assoc_scheme.
  function site_interaction_internal (site1,site2,assoc_scheme)
    ! Input.
    integer, intent(in) :: site1, site2 !< Interaction sites
    integer, intent(in) :: assoc_scheme !< Association scheme
    ! Output.
    logical :: site_interaction_internal

    ! ! Sanity check on input (development phase)
    ! call check_site_and_scheme(site1,assoc_scheme)
    ! call check_site_and_scheme(site2,assoc_scheme)

    ! Assign truth value according to association scheme. Reference: Kontogeorgis&Folas: "Thermodynamic Models for Industrial Applications".
    site_interaction_internal = .false.
    select case (assoc_scheme)
    case (assoc_scheme_1)
       site_interaction_internal = .true.
    case (assoc_scheme_2A)
       site_interaction_internal = .true.
    case (assoc_scheme_2B)
       if (site1 .ne. site2) site_interaction_internal = .true.
    case (assoc_scheme_2C)
       if (site1==1 .or. site2==1) site_interaction_internal = .true.
    case (assoc_scheme_3A)
       site_interaction_internal = .true.
    case (assoc_scheme_3B)
       if (site1 .eq. 3 .neqv. site2 .eq. 3) site_interaction_internal = .true.
    case (assoc_scheme_4A)
       site_interaction_internal = .true.
    case (assoc_scheme_4B)
       if (site1 .eq. 4 .neqv. site2 .eq. 4) site_interaction_internal = .true.
    case (assoc_scheme_4C)
       if ( (site1 .le. 2 .and. (site2 .ge. 3)) .or. (site2 .le. 2 .and. (site1 .ge. 3)) ) site_interaction_internal = .true.
    end select
  end function site_interaction_internal

  !> Given two associating components with different association schemes, one sometimes wants to model only the realistic
  !> site--site combinations on the different molecules, e.g. only the positively charged sites on ethanol should interact
  !> only with the negatively charged sites on H2O, and vice versa for negatively charged sites on ethanol and positively
  !> charged sites on H2O. This is the function which specifies this. It returns true if there is nonzero interaction
  !> between site1 on a molecule using scheme I and site2 on a molecule with scheme II.
  function cross_site_interaction (site1,site2,assoc_scheme_I,assoc_scheme_II)
    ! Input.
    integer, intent(in) :: site1, site2                    !< Interaction sites.
    integer, intent(in) :: assoc_scheme_I, assoc_scheme_II !< Association schemes.
    ! Output.
    logical :: cross_site_interaction
    ! Internal.
    integer :: pol1,pol2

    ! ! Sanity check on input. (For testing.)
    ! call check_site_and_scheme(site1,assoc_scheme_I)
    ! call check_site_and_scheme(site2,assoc_scheme_II)

    cross_site_interaction = .false.
    if (assoc_scheme_I == no_assoc .or. assoc_scheme_II == no_assoc) then
       cross_site_interaction = .false.
       return
    end if

    pol1 = polarity(site1,assoc_scheme_I)
    pol2 = polarity(site2,assoc_scheme_II)
    if (pol1*pol2 > 0) then
       cross_site_interaction = .false.
    else
       cross_site_interaction = .true.
    end if
  end function cross_site_interaction

  !> Retrieve polarity of site given association scheme.
  !> If an association scheme X is not coded in this function, the site will be given polarity 0, which the
  !> code interprets as having cross-interaction with all sites on any different component.
  function polarity (site,scheme)
    integer, intent(in) :: site,scheme
    integer :: polarity
    polarity = 0
    select case (scheme)
    case (assoc_scheme_1)
       polarity = -1
    case (assoc_scheme_2B)
       if (site .eq. 1) polarity = -1
       if (site .eq. 2) polarity = 1
    case (assoc_scheme_2C)
       if (site .eq. 1) polarity = 0 ! bipolar site
       if (site .eq. 2) polarity = -1
    case (assoc_scheme_3B)
       if (site .le. 2) polarity = 1
       if (site .eq. 3) polarity = -1
    case (assoc_scheme_4C)
       if (site .le. 2) polarity = -1
       if (site .ge. 3) polarity = 1
    case (assoc_scheme_1ea)
       polarity = 1
    end select
  end function polarity

  !> Sanity check for the input (site,scheme).
  subroutine check_site_and_scheme(site,scheme)
    integer, intent(in) :: site, scheme
    if (site < 1) call stoperror("Specify a site using a positive integer.")
    select case (scheme)
    case (assoc_scheme_1)
       if (site > 1) call stoperror("Index exceeds number of sites in scheme!")
    case (assoc_scheme_2A, assoc_scheme_2B, assoc_scheme_2C)
       if (site > 2) call stoperror("Index exceeds number of sites in scheme!")
    case (assoc_scheme_3A,assoc_scheme_3B)
       if (site > 3) call stoperror("Index exceeds number of sites in scheme!")
    case (assoc_scheme_4A,assoc_scheme_4B,assoc_scheme_4C)
       if (site > 4) call stoperror("Index exceeds number of sites in scheme!")
    case (no_assoc)
       call stoperror("Component has not been assigned an association scheme.")
    end select

  end subroutine check_site_and_scheme

  function get_assoc_string(assoc) result(as)
    integer, intent(in) :: assoc
    character(len=10) :: as
    select case(assoc)
    case(assoc_scheme_1)
      as = "1"
    case(assoc_scheme_2A)
      as = "2A"
    case(assoc_scheme_2B)
      as = "2B"
    case(assoc_scheme_2C)
      as = "2C"
    case(assoc_scheme_3A)
      as = "3A"
    case(assoc_scheme_3B)
      as = "3B"
    case(assoc_scheme_4A)
      as = "4A"
    case(assoc_scheme_4B)
      as = "4B"
    case(assoc_scheme_4C)
      as = "4C"
    case(assoc_scheme_1ea)
      as = "1ea"
    case(no_assoc)
      as = "None"
    case default
      print *,"Wrong assoc model"
      as = ""
    end select
  end function get_assoc_string

end module AssocSchemeUtils
