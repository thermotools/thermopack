!> Module for CPA parameters.
MODULE CPA_parameters
  use thermopack_constants, only: eosid_len
  use cubic_eos, only: cbMixHVCPA, cbMixHVCPA2, mixExcessGibbs
  use compdatadb, only: CPAarray, nCPAmodels
  use mixdatadb, only: CPAmaxkij, CPAkijdb, interGEdb, maxinterGEij
  use AssocSchemeUtils
  use eosdata, only: get_eos_short_label_from_subidx, get_eos_index
  use stringmod, only: str_eq
  implicit none
  save

contains

  subroutine get_pure_cpa_db_entry(idx, eos_subidx, comp_name, ref)
    use thermopack_constants, only: ref_len, uid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=uid_len), intent(out) :: comp_name !< Component name
    character(len=ref_len), intent(out) :: ref !< Reference string
    ! Locals
    integer :: eosidx
    call get_eos_index(CPAarray(idx)%eosid, eosidx, eos_subidx)
    comp_name = CPAarray(idx)%compName
    ref = CPAarray(idx)%ref
  end subroutine get_pure_cpa_db_entry

  subroutine get_binary_cpa_db_entry(idx, mrule, eos_subidx, uid1, uid2, ref, kijvalue)
    use thermopack_constants, only: ref_len, uid_len, eosid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=eosid_len), intent(out) :: mrule !< Mixing rule
    character(len=uid_len), intent(out) :: uid1, uid2 !< Component names
    character(len=ref_len), intent(out) :: ref !< Reference string
    real, intent(out) :: kijvalue !< Interaction parameter
    ! Locals
    integer :: eosidx
    uid1 = CPAkijdb(idx)%uid1
    uid2 = CPAkijdb(idx)%uid2
    call get_eos_index(CPAkijdb(idx)%eosid, eosidx, eos_subidx)
    ref = CPAkijdb(idx)%ref
    mrule = "" ! Dummy
    kijvalue = 0 ! Dummy
  end subroutine get_binary_cpa_db_entry

  subroutine get_binary_cpa_ge_db_entry(idx, mrule, eos_subidx, uid1, uid2, ref, kijvalue)
    use thermopack_constants, only: ref_len, uid_len, eosid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=eosid_len), intent(out) :: mrule !< Mixing rule
    character(len=uid_len), intent(out) :: uid1, uid2 !< Component names
    character(len=ref_len), intent(out) :: ref !< Reference string
    real, intent(out) :: kijvalue !< Interaction parameter
    ! Locals
    integer :: eosidx
    uid1 = interGEdb(idx)%uid1
    uid2 = interGEdb(idx)%uid2
    call get_eos_index(interGEdb(idx)%eosid, eosidx, eos_subidx)
    ref = interGEdb(idx)%ref
    mrule = interGEdb(idx)%mruleid
    kijvalue = 0 ! Dummy
  end subroutine get_binary_cpa_ge_db_entry

  !> Get the index in the CPAarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getCPAdataIdx(eos,comp_name,param_ref) result(idx)
    use eosdata, only: get_eos_index
    use thermopack_constants, only: verbose
    use parameters, only: get_pure_data_db_idx
    character(len=*), intent(in) :: eos
    character(len=*), intent(in) :: comp_name
    character(len=*), intent(in) :: param_ref
    ! Locals
    integer :: idx, idx_default, eos_subidx, eosidx
    call  get_eos_index(eos, eosidx, eos_subidx)
    call get_pure_data_db_idx(get_pure_cpa_db_entry,nCPAmodels,"CPA",&
         eos_subidx,comp_name,param_ref,.false.,idx,idx_default)
    if (idx < 0) then
      if (verbose) then
        print *, "No CPA parameters for compName, ref ", comp_name, trim(param_ref)
      endif
      if (idx_default > 0) then
        idx = idx_default
        if (verbose) then
          print *, "Using default parameter set instead"
        endif
      else
        idx = 0
      endif
    endif
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
       found,a0,b,alphaParams,eps,beta,alphaCorrIdx,scheme,simplified_rdf)
    use compdata, only: gendata_pointer
    ! Input
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: nc, eosidx
    character(len=*) :: ref
    ! Output
    logical, intent(out) :: found(nc)
    integer, intent(out) :: alphaCorrIdx(nc), scheme(nc)
    real, intent(out) :: a0(nc), b(nc), alphaParams(3,nc), eps(nc), beta(nc)
    logical, intent(out) :: simplified_rdf
    ! Locals
    integer :: ic
    character(len=eosid_len) :: eos
    logical :: simplified_rdf_array(nc)

    eos = get_eos_short_label_from_subidx(eosidx)
    do ic=1,nc
      call getCpaPureParams_singleComp(comp(ic)%p_comp%ident,eos,ref,&
           found(ic),a0(ic),b(ic),alphaParams(:,ic),eps(ic),beta(ic),&
           alphaCorrIdx(ic),scheme(ic),simplified_rdf_array(ic))
    end do
    simplified_rdf = all(simplified_rdf_array)
    if ( .not. simplified_rdf .and. .not. all(.not. simplified_rdf_array)) &
         print *,"Detected mix of parameters for simplified RDF and non-simplified RDF"
  end subroutine getCpaPureParams_allcomps

  subroutine getCpaPureParams_singleComp(compName,eos,ref,&
       found,a0,b,alphaParams,eps,beta,alphaCorrIdx,scheme,simplified_rdf)
    ! Input
    character(len=*), intent(in) :: compName, ref, eos
    ! Output
    logical, intent(out) :: found
    integer, intent(out) :: alphaCorrIdx, scheme
    real, intent(out) :: a0, b, alphaParams(3), eps, beta
    logical, intent(out) :: simplified_rdf
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
    simplified_rdf = CPAarray(idx)%simplified_rdf
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
    use parameters, only: get_binary_db_idx
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    logical, intent(out) :: found
    integer, intent(out) :: epsBetaCombRules(2)
    real, intent(out) :: kijepsbeta(2)
    character(len=*), intent(in) :: param_ref
    ! Locals
    integer :: idx, idx_default
    character(len=eosid_len) :: eos
    call get_binary_db_idx(get_binary_cpa_db_entry,CPAmaxkij,&
       "",eosidx,uid1,uid2,param_ref,idx,idx_default)
    if (idx > 0) then
      kijepsbeta(1) = CPAkijdb(idx)%kij_eps
      kijepsbeta(2) = CPAkijdb(idx)%kij_beta
      epsBetaCombRules = (/CPAkijdb(idx)%eps_comb_rule, CPAkijdb(idx)%beta_comb_rule/)
    else if (idx_default > 0) then
      kijepsbeta(1) = CPAkijdb(idx_default)%kij_eps
      kijepsbeta(2) = CPAkijdb(idx_default)%kij_beta
      epsBetaCombRules = (/CPAkijdb(idx_default)%eps_comb_rule, CPAkijdb(idx_default)%beta_comb_rule/)
    else
      kijepsbeta = 0.0
      eos = get_eos_short_label_from_subidx(eosidx)
      if (str_eq(eos,"CPA-SRK")) then
        epsBetaCombRules = (/aricomb,geocomb/)
      else if (str_eq(eos,"CPA-PR")) then
        epsBetaCombRules = (/geocomb,geocomb/)
      end if
    endif
  end subroutine getCPAkij_epsbeta

  !> Retrieve cubic binary interaction parameter for components uid1 and uid2,
  !> with set number setno. found is true if and only if the parameter is in
  !> the database.
  function getCpaKij_a(eosidx,uid1,uid2,found) result(kij_a)
    use stringmod, only: str_eq
    use thermopack_constants, only: ref_len
    use parameters, only: get_binary_db_idx
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    logical, intent(out) :: found
    !integer, intent(in), optional :: setno     ! Functionality for using other set numbers, not implemented.
    real :: kij_a
    ! Locals
    integer :: idx, idx_default
    character(len=ref_len) :: param_ref
    param_ref = ""
    call get_binary_db_idx(get_binary_cpa_db_entry,CPAmaxkij,&
         "",eosidx,uid1,uid2,param_ref,idx,idx_default)
    found = (idx > 0) .or. (idx_default > 0)
    if (idx > 0) then
      kij_a = CPAkijdb(idx)%kij_a
    else if (idx_default > 0) then
      kij_a = CPAkijdb(idx_default)%kij_a
    else
      kij_a = 0
    endif
  end function getCpaKij_a

  subroutine getCpaGEij(mGE, eosid, ref, uid1, uid2, &
       indxi, indxj, found)
    use stringmod, only: str_eq
    use eosdata, only: get_eos_index
    use parameters, only: get_binary_db_idx
    implicit none
    type (mixExcessGibbs), intent(inout) ::  mGE
    character(len=*), intent(in) :: eosid, uid1, uid2, ref
    integer, intent(in) :: indxi, indxj
    logical, intent(out) :: found
    ! Locals
    integer :: i, j
    integer :: idx, idx_default, eosidx, eos_subidx
    character(len=eosid_len) :: mrule !< Mixing rule

    mGE%correlation(indxi, indxj) = 0
    mGE%correlation(indxj, indxi) = 0

    if (mGE%mGE == cbMixHVCPA2) then
      mrule = "HV2"
    else if (mGE%mGE == cbMixHVCPA) then
      mrule = "HV1"
    else
      call stoperror("Wrong GE mixing rule for CPA")
    endif
    call get_eos_index(eosid, eosidx, eos_subidx)
    call get_binary_db_idx(get_binary_cpa_ge_db_entry,maxinterGEij,&
         mrule,eos_subidx,uid1,uid2,ref,idx,idx_default)
    found = (idx > 0)
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
