!> Module for PC-SAFT pure-component parameters and binary interaction
!> parameters. Also contains parameters for the PeTS equation of state.

!> NB: If you want to add new parameters here, beware that different authors are
!> inconsistent wrt. whether beta should be multiplied with pi/6 or not. For
!> this reason you should validate your results before using these parameters.
!> If you get strange results, try multiplying beta with pi/6=0.5236.
module pc_saft_parameters
  use thermopack_constants, only: Rgas => Rgas_default, verbose
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

  !> Get the index in the PCarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getPCdataIdx(eosidx,compName,param_ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, param_ref
    integer :: idx, idx_default
    logical :: found

    if (.not. Rgas_is_correct()) call stoperror("Rgas_default must be default Rgas parameter.")

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nPCmodels)
      if ((eosidx==PCarray(idx)%eosidx) .and. &
           str_eq(compName, PCarray(idx)%compName)) then
        if (string_match(PCarray(idx)%ref, param_ref)) then
          found = .true.
          exit
        elseif (string_match(PCarray(idx)%ref, "DEFAULT")) then
          idx_default = idx
        endif
      endif
      idx = idx + 1
    enddo

    if (.not. found) then
      if (verbose) then
        print *, "No PC-SAFT parameters for compName, ref ", compName, trim(param_ref)
      endif
      if (idx_default > 0) then
        idx = idx_default
        print *, "Using default parameter set for "//trim(compName)
      else
        print *, "ERROR FOR COMPONENT ", compname
        call stoperror("The PC-SAFT parameters don't exist.")
      endif
    end if
  end function getPCdataIdx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database PCkijdb, it returns 0.0.
  function getPCkij(eosidx,uid1,uid2,param_ref) result(kijvalue)
    use stringmod, only: str_eq, string_match
    use eosdata, only: eosPC_SAFT, eosSPC_SAFT, eosOPC_SAFT
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2, param_ref
    real :: kijvalue
    ! Locals
    integer :: idx, idx_default, eosidx_local
    logical :: match_11_22, match_12_21, found

    if (eosidx == eosOPC_SAFT .or. eosidx == eosPCP_SAFT) then
      eosidx_local = eosPC_SAFT
    else
      eosidx_local = eosidx
    endif
    call getkij()

    ! Use PC-SAFT kij with sPC-SAFT
    if ((eosidx == eosSPC_SAFT .or. eosidx == eosSPCP_SAFT) &
         .and. .not. found) then
      eosidx_local = eosPC_SAFT
      call getkij()
      if (found) print *,"Using PC-SAFT kij with simplified EOS: "//trim(uid1)//"-"//trim(uid2)
    endif

  contains
    subroutine getkij()
      kijvalue = 0.0 ! default value if the binary is not in PCkijdb.
      idx = 1
      idx_default = -1
      found = .false.
      do idx = 1,PCmaxkij
        match_11_22 = str_eq(uid1,PCkijdb(idx)%uid1) .and. str_eq(uid2,PCkijdb(idx)%uid2)
        match_12_21 = str_eq(uid1,PCkijdb(idx)%uid2) .and. str_eq(uid2,PCkijdb(idx)%uid1)

        if (eosidx_local == PCkijdb(idx)%eosidx .and. (match_11_22 .or. match_12_21)) then
          if (string_match(param_ref,PCkijdb(idx)%ref)) then
            kijvalue = PCkijdb(idx)%kijvalue
            found = .true.
            exit
          elseif (string_match("DEFAULT", PCkijdb(idx)%ref)) then
            idx_default = idx
          endif
        endif
      enddo
      if (.not. found .and. idx_default > 0) then
        found = .true.
        kijvalue = PCkijdb(idx_default)%kijvalue
      endif
    end subroutine getkij
  end function getPCkij

  subroutine getPcSaftKij_allComps(nc,comp,eosidx,kij)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    ! Output
    real, intent(out) :: kij(nc,nc)
    ! Locals
    integer :: ic,jc

    kij = 0.0
    do ic=1,nc
      do jc=ic+1,nc
        kij(ic,jc) = getPCkij(eosidx,comp(ic)%p_comp%ident,comp(jc)%p_comp%ident,"DEFAULT")
        kij(jc,ic) = kij(ic,jc)
      end do
    end do

  end subroutine getPcSaftKij_allComps

  subroutine getPcSaftPureParams_allComps(nc,comp,eosidx,param_ref,&
       found,m,sigma,eps_depth_divk,eps,beta,scheme,mu,Q)
    use compdata, only: gendata_pointer
    use numconstants, only: small
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: param_ref
    ! Output
    logical, intent(out) :: found(nc)
    real, intent(out) :: m(nc), sigma(nc), eps_depth_divk(nc), eps(nc), beta(nc)
    integer, intent(out) :: scheme(nc)
    real, intent(out) :: mu(nc), Q(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
      call getPcSaftpureParams_singleComp(comp(ic)%p_comp%ident,eosidx,param_ref,&
           found(ic),m(ic),sigma(ic),eps_depth_divk(ic),eps(ic),beta(ic),scheme(ic),&
           mu(ic),Q(ic))
      ! Check if electrical moments where overrided?
      if (abs(mu(ic)) < small) mu(ic) = comp(ic)%p_comp%mu_dipole
      if (abs(Q(ic)) < small) Q(ic) = comp(ic)%p_comp%q_quadrupole
    end do
  end subroutine getPcSaftPureParams_allComps

  subroutine getPcSaftpureParams_singleComp(compName,eosidx,param_ref,&
       found,m,sigma,eps_depth_divk,eps,beta,scheme,mu,Q)
    ! Input
    character(len=*), intent(in) :: compName, param_ref
    integer, intent(in) :: eosidx
    ! Output
    logical, intent(out) :: found
    real, intent(out) :: m, sigma, eps_depth_divk, eps, beta
    integer, intent(out) :: scheme
    real, intent(out) :: mu, Q
    ! Locals
    integer :: idx

    idx = getPCdataIdx(eosidx,compName,param_ref)
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
