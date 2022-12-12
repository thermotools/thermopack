!> Selection of components, eos etc
!
!
module cbselect
  implicit none
  save

  public :: SelectCubicEOS
  public :: tpSelectInteractionParameters!, deAllocateEosCubic
  !public :: redefine_fallback_TcPcAcf

contains

  !----------------------------------------------------------------------
  !> From mixing rule string get mixing rule index
  !! \param eosidx Index of EOS
  !! \param mrulestr The mixing rule as a character string e.g 'Classic'
  !! \retval mruleidx Mixing rule index
  !! The character strings are case-insensitive
  !!
  !! \author Geir S
  !! \author Morten Hammer
  subroutine get_mixing_rule_index(eosidx, mrulestr, mruleidx)
    use eosdata
    use cubic_eos, only: mix_label_db, get_mix_db_idx, cbMixHuronVidal, &
         cbMixVdW, cbMixHVCPA, cbMixHVCPA2, cbMixHuronVidal2, &
         cbMixWongSandler, cbMixWSCPA, cbMixVdWCPA
    use stringmod, only: str_eq
    implicit none
    integer, intent(in) :: eosidx
    character (len=*), intent (in) :: mrulestr
    integer, intent(out) :: mruleidx
    ! Locals
    integer :: idx_db


    idx_db = get_mix_db_idx(mrulestr)
    if (idx_db < 0) then
      call stoperror('unknown mixing rule')
    endif

    mruleidx = mix_label_db(idx_db)%mix_idx

    ! Use the CPA interaction parameters if they exist, if not use the standard
    ! database. The details are in tpSelectInteractionParameters.
    if (eosidx == eosCPA) then
      if (mruleidx == cbMixVdW) then
        mruleidx = cbMixVdWCPA
      elseif (mruleidx ==  cbMixHuronVidal) then
        mruleidx = cbMixHVCPA
      elseif (mruleidx ==  cbMixHuronVidal2) then
        mruleidx = cbMixHVCPA2
     elseif (mruleidx == cbMixWongSandler) then
        mruleidx = cbMixWSCPA
      else
        call stoperror("Selected mixing rule not implemented for cubic part of the CPA model.")
      end if
    end if

  end subroutine get_mixing_rule_index

 !----------------------------------------------------------------------
  !> Selection of equation of state and the mixing rule
  !! Data from the eos-database is copied to the global variable cbeos
  !! \param eosstr The equation of state as a character string e.g 'SRK' og 'PR'
  !! \param mrulestr The mixing rule as a character string e.g 'Classic'
  !!
  !! The character strings are case-insensitive
  !!
  !! \author Geir S
  !! \author Morten Hammer
  subroutine SelectCubicEOS(nc, comp, cbeos, alphastr, alpha_reference, betastr)
    use eosdata
    use cubic_eos
    use stringmod, only: str_eq
    use compdata, only: gendata_pointer
    !use cbmix, only: cbCalcParameters
    !use unifac, only: init_unifac, unifdb
    use cbAlpha, only: tpInitAlphaCorr
    use cbBeta, only: tpInitBetaCorr
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(:)
    class(cb_eos), intent(inout) :: cbeos
    character (len=*), intent (in) :: alphastr
    character (len=*), intent(in) :: alpha_reference
    character (len=*), intent (in), optional :: betastr
    ! Locals
    integer :: err

    err = 0
    cbeos%cubic_verbose = .false.

    ! Calculates the constant parameters in the cubic EoS:

    ! Initialize critical temperature, critical pressure, and acentric factor of
    ! the cubic EoS
    call initCubicTcPcAcf(nc, comp, cbeos)

    ! Calculate bi etc.
    call cbCalcParameters(nc, cbeos)

    ! Set alpha correlation
    call tpInitAlphaCorr(nc, comp, cbeos, alphastr, alpha_reference)

    ! Set beta correlation (CLASSIC means the regular cubic b parameter)
    if (present(betastr)) then
       call tpInitBetaCorr(nc, comp, cbeos, betastr)
    else
       call tpInitBetaCorr(nc, comp, cbeos, "CLASSIC")
    end if

  end subroutine SelectCubicEOS

  !> Calculates constants for the various cubic EOS
  !!
  !! \author Geir Skaugen
  !! \date 2012-06-12
  !! \author Morten Hammer
  !!
  subroutine cbCalcParameters (nc, cbeos)
    use eosdata
    use cubic_eos, only: cb_eos
    use cbmix, only: cbCalcM, cbCalcOmegaZc
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    !
    select case (cbeos%subeosidx)
    case (cbSRK, cspSRK, cpaSRK, eosLK) ! Use SRK to generate initial values for LK
      cbeos%delta =  0.0D+00

    case (cbPR,cspPR,cpaPR)
      cbeos%delta =  1.0D+00

    case default
      cbeos%delta =  -1.0D20

    end select

    if (cbeos%subeosidx /= cbSW .and. cbeos%subeosidx /= cbPT) then
       call cbCalcM(cbeos)

       ! Initialze mixing rule for the C-parameter
       cbeos%ci = 0.0
       cbeos%sumc = 0.0
       cbeos%cij = 0.0
    endif

    call cbCalcOmegaZc(nc,cbeos) !< Uses the calculated m1 and m2 for two-param eos

  end subroutine cbCalcParameters


  !----------------------------------------------------------------------
  !> Selection of equation of state and the mixing rule
  !! Data from the eos-database is copied to the global variable cbeos
  !! \param eosstr The equation of state as a character string e.g 'SRK' og 'PR'
  !! \param mrulestr The mixing rule as a character string e.g 'Classic'
  !!
  !! The character strings are case-insensitive
  !!
  !! \author Geir S
  !! \author Morten Hammer
  subroutine SelectMixingRules(nc, comp, cbeos, mrulestr, param_reference, b_exponent)
    use cubic_eos, only: cb_eos, cbMixUNIFAC, cbMixWongSandler, cbMixWSCPA, cbMixHVWS,cbMixNRTL, isHVmixModel
    use stringmod, only: str_eq
    use compdata, only: gendata_pointer
    use cbmix, only: cbCalcLowcasebij
    use unifac, only: init_unifac, unifdb
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(nc), intent(in) :: comp
    type(cb_eos) :: cbeos
    character (len=*), intent (in) :: mrulestr
    character (len=*), intent (in) :: param_reference
    real, optional, intent(in) :: b_exponent
    ! Locals
    integer :: mruleidx
    real :: b_exp

    call get_mixing_rule_index(cbeos%eosidx, mrulestr, mruleidx)
    cbeos%mruleidx = mruleidx
    cbeos%mruleid = trim(mrulestr)

    if (isHVmixModel(cbeos%mruleidx) .or. cbeos%mruleidx == cbMixNRTL) then
       call cbeos%mixGE%excess_gibbs_allocate_and_init(nc)
    end if

    if (cbeos%mruleidx == cbMixWongSandler .or. cbeos%mruleidx == cbMixWSCPA .or. cbeos%mruleidx == cbMixHVWS) then
       !print *, cbeos%mruleidx, isHVmixModel(cbeos%mruleidx)
       call cbeos%mixWS%WS_allocate_and_init(nc)
    endif

    if (cbeos%mruleidx == cbMixUNIFAC) then
      call init_unifac(unifdb, mrulestr)
    else
      ! Select default set no 1 interaction paramteres
      call tpSelectInteractionParameters(nc, comp, cbeos, param_reference)
    endif

    if (present(b_exponent)) then
      b_exp = b_exponent
    else
      b_exp = 1
    endif
    ! Select default set no 1 interaction paramteres
    call cbCalcLowcasebij(nc,cbeos,b_exp)

  end subroutine SelectMixingRules

  !> Initialize Tc, Pc and acentric factor to use in the cubic EoS
  !>
  !> \author Ailo Aasen
  subroutine initCubicTcPcAcf(nc, comp, cbeos, TcSpec, PcSpec, AcfSpec)
    use cubic_eos, only: cb_eos
    use compdata, only: gendata_pointer
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(nc), intent(in) :: comp
    class(cb_eos) :: cbeos
    real, optional, intent(in) :: TcSpec(nc) !< Specified critical temperature [K]
    real, optional, intent(in) :: PcSpec(nc) !< Specified critical pressure [Pa]
    real, optional, intent(in) :: AcfSpec(nc) !< Specified acentric factor [-]
    ! Locals
    integer :: i

    ! Select Tcrit, Pcrit and acentric factors to use in the cubic EoS
    do i=1,nc
      if (present(TcSpec)) then
        cbeos%single(i)%Tc = TcSpec(i)
      else
        cbeos%single(i)%Tc = comp(i)%p_comp%tc
      end if

      if (present(PcSpec)) then
        cbeos%single(i)%Pc = PcSpec(i)
      else
        cbeos%single(i)%Pc = comp(i)%p_comp%pc
      end if

      if (present(AcfSpec)) then
        cbeos%single(i)%Acf = AcfSpec(i)
      else
        cbeos%single(i)%Acf = comp(i)%p_comp%acf
      end if
    end do
  end subroutine initCubicTcPcAcf

  !> Redefine the critical temperature, critical pressure, and acentric factor
  !> of the cubic EoS.
  !
  !> \author Morten Hammer
  subroutine redefine_TcPcAcf_comp_cubic(j,TcSpec, PcSpec, AcfSpec, ierr)
    use thermopack_var, only: nce, get_active_thermo_model, thermo_model
    !use cbmix, only: cbCalcParameters
    use eosdata, only: eosCubic
    use cubic_eos, only: cb_eos
    !$ use omp_lib, only: omp_get_max_threads
    integer, intent(in) :: j !< Component index
    integer, intent(out) :: ierr !< Component index
    real, intent(in) :: TcSpec !< Specified critical temperature [K]
    real, intent(in) :: PcSpec !< Specified critical pressure [Pa]
    real, intent(in) :: AcfSpec !< Specified acentric factor [-]
    ! Locals
    integer :: i, ncbeos
    type(thermo_model), pointer :: act_mod_ptr
    !
    act_mod_ptr => get_active_thermo_model()
    if ( act_mod_ptr%eosidx /= eosCubic ) then
      print *,"Not able to redefine component. Returning."
      ierr = 1
      return
    else
      ierr = 0
    endif

    act_mod_ptr%comps(j)%p_comp%tc = TcSpec
    act_mod_ptr%comps(j)%p_comp%pc = PcSpec
    act_mod_ptr%comps(j)%p_comp%Acf = AcfSpec
    !
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=1,ncbeos
      select type ( p_eos => act_mod_ptr%eos(i)%p_eos )
      class is ( cb_eos )
        ierr = 0
        p_eos%single(j)%Tc = TcSpec
        p_eos%single(j)%Tc = TcSpec
        p_eos%single(j)%Pc = PcSpec
        p_eos%single(j)%Acf = AcfSpec
        call cbCalcParameters(nce, p_eos)
      class default
        print *,"Not able to redefine component. Not cubic."
        ierr = 1
        exit
      end select
    enddo
  end subroutine redefine_TcPcAcf_comp_cubic

  !> Redefine the critical temperature, critical pressure, and acentric factor
  !> of the cubic fallback EoS. Can be used to enforce the fallback EoS to have
  !> the same Tc, Pc and Acf as the main EoS.
  !
  !> \author Ailo Aasen
  subroutine redefine_fallback_TcPcAcf(TcSpec, PcSpec, AcfSpec)
    use thermopack_var, only: nce, get_active_thermo_model, thermo_model
    use eosdata, only: eosCubic
    use cubic_eos, only: cb_eos
    !$ use omp_lib, only: omp_get_max_threads
    real, intent(in), optional :: TcSpec(nce) !< Specified critical temperature [K]
    real, intent(in), optional :: PcSpec(nce) !< Specified critical pressure [Pa]
    real, intent(in), optional :: AcfSpec(nce) !< Specified acentric factor [-]
    ! Locals
    integer :: i, ncbeos
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=1,ncbeos
      select type ( p_eos => act_mod_ptr%cubic_eos_alternative(i)%p_eos )
      class is ( cb_eos )
        call initCubicTcPcAcf(nce, act_mod_ptr%comps, p_eos, TcSpec, PcSpec, AcfSpec)
        call cbCalcParameters(nce, p_eos)
      class default
        call stoperror("Fallback not cubic?")
      end select
    enddo

  end subroutine redefine_fallback_TcPcAcf

  !----------------------------------------------------------------------
  !> Selection of binary interacion parameters
  !! This routine is called with a default set from tpSelectEOS.  If more than
  !! one set of interaction parameters are available this set can be retrieved.
  !! All interaction parameters are stored in the variable kijdb in the kijdatadb
  !! structure.  The selected binary interaction paramters are stored in a
  !! 2-dimensional array in the global cbeos type.
  !!
  !! \param setno Set no
  !!
  !! \author Geir S
  !!
  subroutine tpSelectInteractionParameters (nc,comp,cbeos,param_reference)
    use eosdata
    use cubic_eos, only: cb_eos, cbMixReid, cbMixVdW, cbMixHuronVidal, &
         cbmixHuronVidal2, cbmixNRTL, cbMixVdWCPA,cbMixHVCPA,cbMixHVCPA2
    use compdata, only: gendata_pointer
    use stringmod, only: str_eq
    use CPA_parameters, only: getCPAkij_a, getCpaGEij
    implicit none
    integer, intent(in) :: nc
    character (len=*), intent (in) :: param_reference
    type(cb_eos), intent(inout) :: cbeos
    type(gendata_pointer), dimension(nc), intent(in) :: comp
    ! Locals
    integer :: i,j !< Counters
    logical :: found, found_ge

    if (allocated (cbeos%kij) .eqv. .false. ) call StopError('Equation of state are not selected')

    do i=1,nc-1
      do j=i+1,nc

        mixingrule: select case (cbeos%mruleidx)
        case (cbMixReid) !< Asymetric
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,cbeos%mruleid,&
               param_reference,comp(i)%p_comp%ident,comp(j)%p_comp%ident)
          cbeos%kij(j,i) = getkij(cbeos,cbeos%eosid,cbeos%mruleid,&
               param_reference,comp(j)%p_comp%ident,comp(i)%p_comp%ident)
        case (cbMixVdW)
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,cbeos%mruleid,&
               param_reference,comp(i)%p_comp%ident,comp(j)%p_comp%ident)
          cbeos%kij(j,i) = cbeos%kij(i,j)
          cbeos%lij(i,j) = getlij(cbeos,cbeos%eosid,cbeos%mruleid,&
               param_reference,comp(i)%p_comp%ident,comp(j)%p_comp%ident)
          cbeos%lij(j,i) = cbeos%lij(i,j)
          if (cbeos%lij(i,j)/=0) cbeos%simple_covolmixing = .false.
        case (cbMixHuronVidal, cbmixHuronVidal2, cbmixNRTL)
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,"Classic",&
               param_reference,comp(i)%p_comp%ident,comp(j)%p_comp%ident) !< First get the kij for vdW
          cbeos%kij(j,i) = cbeos%kij(i,j)
          call getInterDataGEij(cbeos%mixGE,cbeos%eosid,param_reference,&
               comp(i)%p_comp%ident,comp(j)%p_comp%ident,i,j,found_ge) !< get both  the ij- and ji-pair
        case (cbMixVdWCPA,cbMixHVCPA,cbMixHVCPA2)
          cbeos%kij(i,j) = getCPAkij_a(cbeos%subeosidx,comp(i)%p_comp%ident,&
               comp(j)%p_comp%ident,found=found)
          if ((.not. found) .and. (cbeos%subeosidx .eq. cpaSRK)) &
               cbeos%kij(i,j) = getkij(cbeos,eosid="srk",mruleid="classic",&
               ref=param_reference,uid1=comp(i)%p_comp%ident,uid2=comp(j)%p_comp%ident)
          if ((.not. found) .and. (cbeos%subeosidx .eq. cpaPR)) &
               cbeos%kij(i,j) = getkij(cbeos,eosid="pr",mruleid="classic",&
               ref=param_reference,uid1=comp(i)%p_comp%ident,uid2=comp(j)%p_comp%ident)
          cbeos%kij(j,i) = cbeos%kij(i,j)
          if (cbeos%mruleidx == cbMixHVCPA .OR. cbeos%mruleidx == cbMixHVCPA2) then
            call getCpaGEij(cbeos%mixGE,cbeos%eosid,param_reference,&
                 comp(i)%p_comp%ident,comp(j)%p_comp%ident,i,j,found) !< get both  the ij- and ji-pair
          endif
        end select mixingrule
      enddo
    enddo
    ! Set no interaction with itselves
    if (cbeos%eosidx == eosLK) then
      do i=1,nc
        cbeos%kij(i,i) = 1.0
      enddo
    endif
  end subroutine tpSelectInteractionParameters

  !---------------------------------------------------------------------- >
  !> The function retun the index of the working data record (in the module compdata)
  !! for the compoentent that is found
  !!
  !! \param compid Character string for a single component ID.
  !! \retval idx The array index of compdb.
  !!
  function getCompindex(nc,comp,compid) result(idx)
    use compdata, only: gendata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(nc), intent(in) :: comp
    character(len=*), intent(in) :: compid
    integer :: idx
    logical :: found
    idx = 1
    found = .false.
    do while (idx <= nc .and. .not. found)
       if (trim(compid) /= comp(idx)%ident) then
          idx = idx + 1
       else
          found = .true.
       endif
     enddo
     if (.not. found) then
       idx = -1
     endif
    return
  end function getCompindex

  !----------------------------------------------------------------------
  !< Retrive the interaction paramtere for the pair uid1 and uid2
  !! \param eosid String variable for eos
  !! \param mruleid String varable for mixing rule
  !! \param setno Integer value for set number
  !! \param uid1 Component no 1 of binary pair
  !! \param uid2 Component no 2 of binary pair
  !! \return kijvalue The value for the interaction paramter -
  !!                  stored as kij(i,j) in the calling module
  !!
  !! For the classic (vdW) mixing rules, kij is equal to kji and both
  !! combination of input data must be checked.
  !!
  !! For asymetic mixing rules - as proposed by Reid, kij /= kji and only the
  !! exact order of components are valid.
  !!
  !! \author Geir S
  !!

  function getkij (cbeos, eosid, mruleid, ref, uid1, uid2) result(kijvalue)
    use mixdatadb
    use eosdata
    use stringmod, only: str_eq, string_match_val
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    character(len=*), intent(in) :: eosid, mruleid, uid1, uid2, ref
    real :: kijvalue
    ! Locals
    integer :: i, idx_lowest, match_val
    logical :: found, candidate_found, ref_match
    character(len=max(len(eosid),2)) :: eosid_local

    found = .false.
    eosid_local = eosid
    if (cbeos%eosidx == eosLK) then
      eosid_local = 'LK'
      kijvalue = 1.0 !< Default value - no interaction
    else
      kijvalue = 0.0 !< Default value - no interaction
    endif

    found = .false.
    kijvalue = 0
    idx_lowest = 100000
    do i=1,maxkij
      candidate_found = str_eq (eosid_local,kijdb(i)%eosid) .and. str_eq(mruleid,kijdb(i)%mruleid) &
           .and. ((str_eq(uid1,kijdb(i)%uid1) .and. str_eq(uid2,kijdb(i)%uid2)) &
           .or.  ( str_eq(uid1,kijdb(i)%uid2) .and. str_eq(uid2,kijdb(i)%uid1)))
      if (candidate_found) then
        if (.not. found) then ! we at least found one match
          kijvalue = kijdb(i)%kijvalue
        end if
        found = .true.

        call string_match_val(ref,kijdb(i)%ref,ref_match,match_val)
        if (ref_match .and. match_val<idx_lowest) then ! the match takes precedence
          idx_lowest = match_val
          kijvalue = kijdb(i)%kijvalue
        end if
      end if
    enddo
  end function getkij

  function getlij(cbeos, eosid, mruleid, ref, uid1, uid2) result(lijvalue)
    use mixdatadb, only: lijdb, maxlij
    use stringmod, only: str_eq, string_match_val, str_upcase
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    character(len=*), intent(in) :: eosid
    character(len=*), intent(in) :: uid1, uid2
    character(len=*), intent(in) :: mruleid
    character(len=*), intent(in) :: ref
    real :: lijvalue
    ! Locals
    logical :: ref_match
    integer :: match_val, i, idx_lowest
    logical :: candidate_found
    character(len=len_trim(ref)+8) :: ref_local
    !
    ! Use default value if possible:
    ref_local = trim(ref)
    call str_upcase(ref_local)
    if (index(ref_local,"DEFAULT") == 0) then
      ref_local = trim(ref_local)//"/DEFAULT"
    endif
    lijvalue = 0
    idx_lowest = 100000
    do i=1,maxlij
      candidate_found = str_eq(eosid,lijdb(i)%eosid) &
            .and. str_eq(mruleid,lijdb(i)%mruleid) &
            .and. ((str_eq(uid1,lijdb(i)%uid1) .and. str_eq(uid2,lijdb(i)%uid2)) &
            .or.  ( str_eq(uid1,lijdb(i)%uid2) .and. str_eq(uid2,lijdb(i)%uid1)))
      if (candidate_found) then
        call string_match_val(ref_local,lijdb(i)%ref,ref_match,match_val)
        if (ref_match .and. match_val<idx_lowest) then ! the match takes precedence
          idx_lowest = match_val
          lijvalue = lijdb(i)%lijvalue
        end if
      end if
    enddo
  end function getlij

  subroutine getInterDataGEij(mGE, eosid, ref, uid1, uid2, &
       indxi, indxj, found)
    use mixdatadb
    use eosdata
    use cubic_eos, only: mixExcessGibbs, cbMixHuronVidal, cbMixHuronVidal2,&
         cbMixNRTL
    use stringmod, only: str_eq
    implicit none
    type(mixExcessGibbs), intent(inout) ::  mGE
    character(len=*), intent(in) :: eosid, uid1, uid2, ref
    integer, intent(in) :: indxi, indxj
    logical, intent(out) :: found
    ! Locals
    logical :: isHV, isNRTL, isUidMatch
    integer :: idx, idx_default, i, j

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
      isHV = ((str_eq ('HV2',interGEdb(idx)%mruleid) &
           .and. (mGE%mGE == cbMixHuronVidal2)) .OR. &
           (str_eq ('HV1',interGEdb(idx)%mruleid) &
           .and. mGE%mGE == cbMixHuronVidal))
      isNRTL = (str_eq ('NRTL',interGEdb(idx)%mruleid) &
           .and. mGE%mGE == cbMixNRTL)

      if ( isUidMatch .AND. &
           str_eq (eosid,interGEdb(idx)%eosid) .AND.&
           isHV .OR. isNRTL) then
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
      if (mGE%mGE == cbMixHuronVidal2) then
        mGE%cGE(i,j) = interGEdb(idx)%polyij(3)
        mGE%cGE(j,i) = interGEdb(idx)%polyji(3)
      else
        if (interGEdb(idx)%correlation == 2) then
          call stoperror('The Maribo-Mogensen correlation'//&
               ' for component interaction require HV2')
        endif
      endif
    endif

  end subroutine getinterdataGEij


end module cbselect
