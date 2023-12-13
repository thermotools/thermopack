!> Module for CPA parameters.
MODULE CPA_parameters
  use thermopack_constants, only: eosid_len
  use cubic_eos, only: cbMixHVCPA, cbMixHVCPA2, mixExcessGibbs
  use compdatadb, only: CPAarray, nCPAmodels
  use mixdatadb, only: CPAmaxkij, CPAkijdb, interGEdb, maxinterGEij
  use AssocSchemeUtils
  use eosdata, only: get_eos_short_label_from_subidx
  use stringmod, only: str_eq
  implicit none
  save

contains

  !> Get the index in the CPAarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getCPAdataIdx(eos,compName,param_ref) result(idx)
    use thermopack_constants, only: verbose
    use stringmod, only: string_match
    character(len=*), intent(in) :: eos
    character(len=*), intent(in) :: compName
    character(len=*), intent(in) :: param_ref
    integer :: idx, idx_default
    logical :: found

    found = .false.
    idx_default = -1
    idx = 1
    do while (idx <= nCPAmodels)
       if ((str_eq(eos,CPAarray(idx)%eosid)) .and. &
            trim(compName)==trim(CPAarray(idx)%compName)) then
         if (string_match(CPAarray(idx)%ref, param_ref)) then
           found = .true.
           exit
         elseif (string_match(CPAarray(idx)%ref, "DEFAULT")) then
           idx_default = idx
         endif
      endif
      idx = idx + 1
    enddo

    if (.not. found) then
      if (verbose) then
        print *, "No CPA parameters for compName, ref ", compName, trim(param_ref)
      endif
      if (idx_default > 0) then
        idx = idx_default
        if (verbose) then
          print *, "Using default parameter set instead"
        endif
      else
        idx = 0
      endif
    end if
  end function getCPAdataIdx

  !> Get information on if it is safe to initialize CPA
  !! ie. are there any self-associating components in the mix
  function mixHasSelfAssociatingComp(nc,eos,complist,ref) result(isAssoc)
    use stringmod, only: str_eq
    integer, intent(in) :: nc
    character(len=*), intent(in) :: ref
    character(len=*), intent(in) :: eos
    character(len=*), dimension(nc), intent(in) :: compList
    logical :: isAssoc
    ! Locals
    integer :: i, idx
    !
    isAssoc = .false.

    if (.not. (str_eq(eos,'CPA-SRK') .or. str_eq(eos,'CPA-PR'))) then
      return
    endif

    do i=1,nc
      idx = getCPAdataIdx(eos,trim(compList(i)),ref)
      if (idx > 0) then
        if ( CPAarray(idx)%assoc_scheme /= no_assoc .AND. &
             CPAarray(idx)%assoc_scheme /= assoc_scheme_1ea) then
          isAssoc = .true.
          exit
        endif
      endif
    enddo
  end function mixHasSelfAssociatingComp

  subroutine getCpaPureParams_allcomps(nc,comp,eosidx,ref,&
       found,a0,b,alphaParams,eps,beta,alphaCorrIdx,scheme)
    use compdata, only: gendata_pointer
    ! Input
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: nc, eosidx
    character(len=*) :: ref
    ! Output
    logical, intent(out) :: found(nc)
    integer, intent(out) :: alphaCorrIdx(nc), scheme(nc)
    real, intent(out) :: a0(nc), b(nc), alphaParams(3,nc), eps(nc), beta(nc)
    ! Locals
    integer :: ic
    character(len=eosid_len) :: eos

    eos = get_eos_short_label_from_subidx(eosidx)
    do ic=1,nc
       call getCpaPureParams_singleComp(comp(ic)%p_comp%ident,eos,ref,&
            found(ic),a0(ic),b(ic),alphaParams(:,ic),eps(ic),beta(ic),alphaCorrIdx(ic),scheme(ic))
    end do
  end subroutine getCpaPureParams_allcomps


  subroutine getCpaPureParams_singleComp(compName,eos,ref,&
       found,a0,b,alphaParams,eps,beta,alphaCorrIdx,scheme)
    ! Input
    character(len=*), intent(in) :: compName, ref, eos
    ! Output
    logical, intent(out) :: found
    integer, intent(out) :: alphaCorrIdx, scheme
    real, intent(out) :: a0, b, alphaParams(3), eps, beta
    ! Locals
    integer :: idx

    idx = getCPAdataIdx(eos,compName,ref)
    if ( idx == 0 ) then
       found = .false.
       return
    end if

    found = .true.
    a0 = CPAarray(idx)%a0
    b = CPAarray(idx)%b
    alphaParams = CPAarray(idx)%alphaParams
    eps = CPAarray(idx)%eps
    beta = CPAarray(idx)%beta
    alphaCorrIdx = CPAarray(idx)%alphaCorrIdx
    scheme = CPAarray(idx)%assoc_scheme
  end subroutine getCpaPureParams_singleComp


  subroutine getCpaKijAndCombRules_allComps(nc,comp,eosidx,&
       aEpsBeta_kij,epsbeta_combrules)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    ! Output
    real, intent(out) :: aEpsBeta_kij(3,nc,nc) ! kijs for the a, eps and beta comb rules
    integer, intent(out) :: epsbeta_combrules(2,nc,nc)
    ! Locals
    integer :: ic,jc
    character(len=10) :: ref
    logical :: found_

    aEpsBeta_kij = -1000
    epsbeta_combrules = -1000
    ref = "DEFAULT"
    do ic=1,nc
       do jc=1,nc
          if (ic == jc) cycle
          aEpsBeta_kij(1,ic,jc) = getCPAkij_a(eosidx,comp(ic)%p_comp%ident,&
               comp(jc)%p_comp%ident,found_)
          call getCPAkij_epsbeta(eosidx,comp(ic)%p_comp%ident,comp(jc)%p_comp%ident,ref,&
               found_,epsbeta_combrules(1:2,ic,jc),aEpsBeta_kij(2:3,ic,jc))
       end do
    end do
  end subroutine getCpaKijAndCombRules_allComps


  !> Retrieve association binary interaction parameter for components uid1 and
  !> uid2. Found is true if and only if the parameters is in the database. As
  !> of now this function sets interaction parameters to 0.0 if epsBetaCombRules
  !> is not exactly what is inputted.
  subroutine getCPAkij_epsbeta (eosidx,uid1,uid2,param_ref,found,epsBetaCombRules,kijepsbeta)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    logical, intent(out) :: found
    integer, intent(out) :: epsBetaCombRules(2)
    real, intent(out) :: kijepsbeta(2)
    character(len=*), intent(in) :: param_ref
    !
    integer :: idx, idx_default
    logical :: correct_eos, correct_comps, correct_ref, default_ref
    character(len=eosid_len) :: eos

    eos = get_eos_short_label_from_subidx(eosidx)
    kijepsbeta = 0.0
    idx = 1
    idx_default = -1
    found = .false.
    do while (idx <= CPAmaxkij .and. (.not. found))
       !print *, "idx,uid1,uid2,setno_loc,CPAkijdb(idx)%eosidx",idx,uid1,uid2,setno_loc,CPAkijdb(idx)%eosidx
       correct_eos = str_eq(eos,CPAkijdb(idx)%eosid)
       correct_comps = (str_eq(uid1,CPAkijdb(idx)%uid1) .and. str_eq(uid2,CPAkijdb(idx)%uid2)) &
            .or. ( str_eq(uid1,CPAkijdb(idx)%uid2) .and. str_eq(uid2,CPAkijdb(idx)%uid1))
       correct_ref = string_match(param_ref,CPAkijdb(idx)%ref)
       default_ref = string_match("DEFAULT",CPAkijdb(idx)%ref)

       if ( correct_eos .and. correct_comps) then
         if (correct_ref) then
           kijepsbeta(1) = CPAkijdb(idx)%kij_eps
           kijepsbeta(2) = CPAkijdb(idx)%kij_beta
           epsBetaCombRules = (/CPAkijdb(idx)%eps_comb_rule, CPAkijdb(idx)%beta_comb_rule/)
           return
         else if (default_ref) then
           idx_default = idx
         endif
       else
         idx = idx + 1
       endif
     enddo

     ! Default values.
     if (.not. found) then
       if (idx_default > 0) then
         idx = idx_default
         kijepsbeta(1) = CPAkijdb(idx)%kij_eps
         kijepsbeta(2) = CPAkijdb(idx)%kij_beta
         epsBetaCombRules = (/CPAkijdb(idx)%eps_comb_rule, CPAkijdb(idx)%beta_comb_rule/)
       else
         kijepsbeta = 0.0
         if (str_eq(eos,"CPA-SRK")) then
           epsBetaCombRules = (/aricomb,geocomb/)
         else if (str_eq(eos,"CPA-PR")) then
           epsBetaCombRules = (/geocomb,geocomb/)
         end if
       endif
     end if

  end subroutine getCPAkij_epsbeta

  !> Retrieve cubic binary interaction parameter for components uid1 and uid2,
  !> with set number setno. found is true if and only if the parameter is in
  !> the database.
  function getCpaKij_a(eosidx,uid1,uid2,found) result(kij_a)
    use stringmod, only: str_eq
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    logical, intent(out) :: found
    !integer, intent(in), optional :: setno     ! Functionality for using other set numbers, not implemented.
    real :: kij_a
    integer :: idx
    character(len=eosid_len) :: eos

    eos = get_eos_short_label_from_subidx(eosidx)
    idx = 1
    found = .false.
    do while (idx <= CPAmaxkij .and. (.not. found))
       if ( str_eq(eos,CPAkijdb(idx)%eosid) .and. &
            (str_eq(uid1,CPAkijdb(idx)%uid1) &
            .and. str_eq(uid2,CPAkijdb(idx)%uid2)) &
            .or. ( str_eq(uid1,CPAkijdb(idx)%uid2) & ! Symmetrical mixing rule.
            .and. str_eq(uid2,CPAkijdb(idx)%uid1))) then
          found = .true.
          kij_a = CPAkijdb(idx)%kij_a
       else
          idx = idx + 1
       endif
     enddo

     ! Default kij.
     if (.not. found) then
       kij_a = 0.0
     end if

  end function getCpaKij_a

  subroutine getCpaGEij(mGE, eosid, ref, uid1, uid2, &
       indxi, indxj, found)
    use stringmod, only: str_eq, string_match
    implicit none
    type (mixExcessGibbs), intent(inout) ::  mGE
    character(len=*), intent(in) :: eosid, uid1, uid2, ref
    integer, intent(in) :: indxi, indxj
    logical, intent(out) :: found
    ! Locals
    integer :: idx, i, j, idx_default
    logical :: isUidMatch, isHVCPA

    idx_default = -1
    idx = 1
    found = .false.

    mGE%correlation(indxi, indxj) = 0
    mGE%correlation(indxj, indxi) = 0

    do idx=1,maxinterGEij
      isUidMatch = (str_eq(uid1,interGEdb(idx)%uid1) .AND. &
           str_eq(uid2,interGEdb(idx)%uid2)) .OR. &
           (str_eq(uid2,interGEdb(idx)%uid1) .AND. &
           str_eq(uid1,interGEdb(idx)%uid2))
      isHVCPA = ((str_eq ('HV2',interGEdb(idx)%mruleid) &
           .and. (mGE%mGE == cbMixHVCPA2)) .OR. &
           (str_eq ('HV1',interGEdb(idx)%mruleid) &
           .and. mGE%mGE == cbMixHVCPA))

      if ( isUidMatch .AND. &
           str_eq (eosid,interGEdb(idx)%eosid) .AND.&
           isHVCPA) then
        if (string_match(ref,interGEdb(idx)%ref)) then
          found = .true.
          exit ! Exit do loop
        else if (string_match("DEFAULT",interGEdb(idx)%ref)) then
          idx_default = idx
        endif
      endif
    enddo

    if (.not. found .AND. idx_default > 0) then
      idx = idx_default
      found = .true.
    endif

    if (found) then
      if ( str_eq(uid1,interGEdb(idx)%uid1) .AND. &
           str_eq(uid2,interGEdb(idx)%uid2)) then
        i = indxi
        j = indxj
      else
        ! Swap i and j
        i = indxj
        j = indxi
      endif
      mGE%alpha(i,j) = interGEdb(idx)%alphaijvalue(1)
      mGE%alpha(j,i) = interGEdb(idx)%alphaijvalue(2)
      mGE%correlation(i,j) = interGEdb(idx)%correlation
      mGE%correlation(j,i) = interGEdb(idx)%correlation

      mGE%aGE(i,j) = interGEdb(idx)%polyij(1)
      mGE%aGE(j,i) = interGEdb(idx)%polyji(1)
      mGE%bGE(i,j) = interGEdb(idx)%polyij(2)
      mGE%bGE(j,i) = interGEdb(idx)%polyji(2)
      if (mGE%mGE == cbMixHVCPA2) then
        mGE%cGE(i,j) = interGEdb(idx)%polyij(3)
        mGE%cGE(j,i) = interGEdb(idx)%polyji(3)
      else
        if (interGEdb(idx)%correlation == 2) then
          call stoperror('The Maribo-Mogensen correlation'//&
               ' for component interaction require HV2')
        endif
      endif
    endif
  end subroutine getCpaGEij

end module CPA_parameters
