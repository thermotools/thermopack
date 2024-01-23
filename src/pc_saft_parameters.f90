!> Module for PC-SAFT pure-component parameters and binary interaction
!> parameters. Also contains parameters for the PeTS equation of state.

!> NB: If you want to add new parameters here, beware that different authors are
!> inconsistent wrt. whether beta should be multiplied with pi/6 or not. For
!> this reason you should validate your results before using these parameters.
!> If you get strange results, try multiplying beta with pi/6=0.5236.
module pc_saft_parameters
  use thermopack_constants, only: Rgas => Rgas_default, verbose, eosid_len
  use pc_saft_datadb, only: nPCmodels, PCarray, PCmaxkij, PCkijdb
  use eosdata, only: eosPC_SAFT, eosSPC_SAFT, eosOPC_SAFT, &
       eosSPCP_SAFT, eosPCP_SAFT
  implicit none
  save

contains

  !> Checks that we use the correct gas constant when initializing the
  !> pc_saft_data parameters.
  logical function Rgas_is_correct()
    use thermopack_constants, only: Rgas_default
    Rgas_is_correct = (Rgas == Rgas_default)
  end function Rgas_is_correct

  subroutine get_pure_pc_saft_db_entry(idx, eos_subidx, comp_name, ref)
    use thermopack_constants, only: ref_len, uid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=uid_len), intent(out) :: comp_name !< Component name
    character(len=ref_len), intent(out) :: ref !< Reference string
    eos_subidx = PCarray(idx)%eosidx
    comp_name = PCarray(idx)%compName
    ref = PCarray(idx)%ref
  end subroutine get_pure_pc_saft_db_entry

  subroutine get_binary_pc_saft_db_entry(idx, mrule, eos_subidx, uid1, uid2, ref, kijvalue)
    use thermopack_constants, only: ref_len, uid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=eosid_len), intent(out) :: mrule !< Mixing rule
    character(len=uid_len), intent(out) :: uid1, uid2 !< Component names
    character(len=ref_len), intent(out) :: ref !< Reference string
    real, intent(out) :: kijvalue !< Interaction parameter
    uid1 = PCkijdb(idx)%uid1
    uid2 = PCkijdb(idx)%uid2
    eos_subidx = PCkijdb(idx)%eosidx
    ref = PCkijdb(idx)%ref
    mrule = "" ! Dummy
    kijvalue = PCkijdb(idx)%kijvalue
  end subroutine get_binary_pc_saft_db_entry

  !> Get the index in the PCarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getPCdataIdx(eosidx,comp_name,param_ref) result(idx)
    use parameters, only: get_pure_data_db_idx
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: comp_name, param_ref
    integer :: idx, idx_default

    if (.not. Rgas_is_correct()) call stoperror("Rgas_default must be default Rgas parameter.")

    call get_pure_data_db_idx(get_pure_pc_saft_db_entry,nPCmodels,"PC-SAFT",&
         eosidx,comp_name,param_ref,.true.,idx,idx_default)
  end function getPCdataIdx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database PCkijdb, it returns 0.0.
  function getPCkij(eos_subidx,uid1,uid2,param_ref) result(kijvalue)
    use eosdata, only: eosPC_SAFT, eosSPC_SAFT, eosOPC_SAFT
    use parameters, only: get_binary_interaction_parameter
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: uid1, uid2, param_ref
    real :: kijvalue
    ! Locals
    integer :: idx, idx_default, eos_subidx_local
    logical :: found
    real :: default_value
    default_value = 0
    if (eos_subidx == eosOPC_SAFT .or. eos_subidx == eosPCP_SAFT) then
      eos_subidx_local = eosPC_SAFT
    else
      eos_subidx_local = eos_subidx
    endif
    kijvalue = get_binary_interaction_parameter(get_binary_pc_saft_db_entry,PCmaxkij,"PC-SAFT",&
         "",eos_subidx_local,uid1,uid2,param_ref,default_value,idx,idx_default)
    found = (idx > 0) .or. (idx_default > 0)

    ! Use PC-SAFT kij with sPC-SAFT
    if ((eos_subidx == eosSPC_SAFT .or. eos_subidx == eosSPCP_SAFT) &
         .and. .not. found) then
      eos_subidx_local = eosPC_SAFT
      kijvalue = get_binary_interaction_parameter(get_binary_pc_saft_db_entry,PCmaxkij,"PC-SAFT",&
           "",eos_subidx_local,uid1,uid2,param_ref,default_value,idx,idx_default)
      found = (idx > 0) .or. (idx_default > 0)
      if (found) print *,"Using PC-SAFT kij with simplified EOS: "//trim(uid1)//"-"//trim(uid2)
    endif
  end function getPCkij

  subroutine getPcSaftKij_allComps(nc,comp,eos_subidx,kij,param_ref)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: param_ref
    ! Output
    real, intent(out) :: kij(nc,nc)
    ! Locals
    integer :: ic,jc

    kij = 0.0
    do ic=1,nc
      do jc=ic+1,nc
        kij(ic,jc) = getPCkij(eos_subidx,comp(ic)%p_comp%ident,comp(jc)%p_comp%ident,param_ref)
        kij(jc,ic) = kij(ic,jc)
      end do
    end do

  end subroutine getPcSaftKij_allComps

  subroutine getPcSaftCombRules_allComps(nc,comp,eos_subidx,&
    epsbeta_combrules, param_ref)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: param_ref
    ! Output
    integer, intent(out) :: epsbeta_combrules(2,nc,nc)
    ! Locals
    integer :: ic,jc
    logical :: found_

    epsbeta_combrules = -1000
    do ic=1,nc
        do jc=1,nc
          if (ic == jc) cycle
          call getPcSaftCombRules(eos_subidx,comp(ic)%p_comp%ident,comp(jc)%p_comp%ident,param_ref,&
                found_,epsbeta_combrules(1:2,ic,jc))
        end do
    end do
  end subroutine getPcSaftCombRules_allComps

  !> Retrieve association combining rules for components uid1 and
  !> uid2. Found is true if and only if the parameters are in the database.
  subroutine getPcSaftCombRules (eos_subidx,uid1,uid2,param_ref,found,epsBetaCombRules)
    use AssocSchemeUtils, only: defaultComb
    use parameters, only: get_binary_db_idx
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: uid1, uid2
    logical, intent(out) :: found
    integer, intent(out) :: epsBetaCombRules(2)
    character(len=*), intent(in) :: param_ref
    !
    integer :: idx, idx_default

    call get_binary_db_idx(get_binary_pc_saft_db_entry,PCmaxkij,&
         "",eos_subidx,uid1,uid2,param_ref,idx,idx_default)
    if (idx > 0) then
      epsBetaCombRules = (/PCkijdb(idx)%eps_comb_rule, PCkijdb(idx)%beta_comb_rule/)
    else if (idx_default > 0) then
      epsBetaCombRules = (/PCkijdb(idx_default)%eps_comb_rule, PCkijdb(idx_default)%beta_comb_rule/)
    else
      epsBetaCombRules = (/defaultComb,defaultComb/)
    endif
  end subroutine getPcSaftCombRules

  subroutine getPcSaftPureParams_allComps(nc,comp,eos_subidx,param_ref,&
       found,m,sigma,eps_depth_divk,eps,beta,scheme,mu,Q)
    use compdata, only: gendata_pointer
    use numconstants, only: small
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: param_ref
    ! Output
    logical, intent(out) :: found(nc)
    real, intent(out) :: m(nc), sigma(nc), eps_depth_divk(nc), eps(nc), beta(nc)
    integer, intent(out) :: scheme(nc)
    real, intent(out) :: mu(nc), Q(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
      call getPcSaftpureParams_singleComp(comp(ic)%p_comp%ident,eos_subidx,param_ref,&
           found(ic),m(ic),sigma(ic),eps_depth_divk(ic),eps(ic),beta(ic),scheme(ic),&
           mu(ic),Q(ic))
      ! Check if electrical moments where overrided?
      if (abs(mu(ic)) < small) mu(ic) = comp(ic)%p_comp%mu_dipole
      if (abs(Q(ic)) < small) Q(ic) = comp(ic)%p_comp%q_quadrupole
    end do
  end subroutine getPcSaftPureParams_allComps

  subroutine getPcSaftpureParams_singleComp(compName,eos_subidx,param_ref,&
       found,m,sigma,eps_depth_divk,eps,beta,scheme,mu,Q)
    ! Input
    character(len=*), intent(in) :: compName, param_ref
    integer, intent(in) :: eos_subidx
    ! Output
    logical, intent(out) :: found
    real, intent(out) :: m, sigma, eps_depth_divk, eps, beta
    integer, intent(out) :: scheme
    real, intent(out) :: mu, Q
    ! Locals
    integer :: idx

    idx = getPCdataIdx(eos_subidx,compName,param_ref)
    if ( idx == 0 ) then
      found = .false.
      return
    end if

    found = .true.
    m = PCarray(idx)%m
    scheme = PCarray(idx)%assoc_scheme
    sigma = PCarray(idx)%sigma
    eps_depth_divk = PCarray(idx)%eps_depth_divk
    eps = PCarray(idx)%eps
    beta = PCarray(idx)%beta
    scheme = PCarray(idx)%assoc_scheme
    mu = PCarray(idx)%mu
    Q = PCarray(idx)%Q
  end subroutine getPcSaftpureParams_singleComp

end module pc_saft_parameters
