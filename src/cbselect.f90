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
  !> Map EOS string to integer index
  !! \param eosstr The equation of state as a character string e.g 'SRK' og 'PR'
  !! The character strings are case-insensitive
  !! \retval eosidx,subeosidx Indices defining selected EOS
  !! \author Geir Skaugen
  !! \author Morten Hammer
  ! subroutine get_eos_index(eosstr,eosidx,subeosidx)
  !   use eosdata
  !   use stringmod, only: str_eq
  !   implicit none
  !   character (len=*), intent (in) :: eosstr
  !   integer, intent(out) :: eosidx
  !   integer, intent(out) :: subeosidx
  !   ! Locals
  !   integer :: idx_db
  !   idx_db = get_eos_db_idx(eosstr)
  !   if (idx_db < 0) then
  !     call stoperror('unknown eos')
  !   endif
  !   subeosidx = eos_label_db(i)%

  ! end subroutine get_eos_index

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
         cbMixVdW, cbMixHVCPA, cbMixHVCPA2, cbMixHuronVidal2, cbMixVdWCPA
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

    ! Use the CPA interaction parameters if they exist, if not use the standard database. The details are in tpSelectInteractionParameters.
    if (eosidx == eosCPA) then
      if (mruleidx == cbMixVdW) then
        mruleidx = cbMixVdWCPA
      elseif (mruleidx ==  cbMixHuronVidal) then
        mruleidx = cbMixHVCPA
        !cbeos%mixGE%mGE = cbMixHVCPA
      elseif (mruleidx ==  cbMixHuronVidal2) then
        mruleidx = cbMixHVCPA2
        !cbeos%mixGE%mGE = cbMixHVCPA2
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
  subroutine SelectCubicEOS(nc, comp, cbeos, alphastr, alpha_reference)
    use eosdata
    use cubic_eos
    use stringmod, only: str_eq
    use compdata, only: gendata_pointer
    !use tpcbmix, only: cbCalcParameters
    !use unifac, only: init_unifac, unifdb
    use cbAlpha, only: tpInitAlphaCorr
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(:)
    class(cb_eos), intent(inout) :: cbeos
    character (len=*), intent (in) :: alphastr
    character (len=*), intent(in) :: alpha_reference
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
    use tpcbmix, only: cbCalcM, cbCalcOmegaZc
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
    use cubic_eos, only: cb_eos, cbMixUNIFAC
    use stringmod, only: str_eq
    use compdata, only: gendata_pointer
    use tpcbmix, only: cbCalcLowcasebij
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
   !  cbeos%cubic_verbose = .false.
   ! elseif (str_eq(eosstr,'MBWR19')) then
   !   if (nc /= 1) call stoperror("MBWR equation only for single component.")
   !   cbeos%eosidx = eos_single
   !   cbeos%subeosidx = meosMbwr19
   !   if (allocated(cbeos%mbwr_meos)) deallocate(cbeos%mbwr_meos)
   !   allocate(cbeos%mbwr_meos(1))
   !   call initializeMBWRmodel(comp(1)%ident, cbeos%mbwr_meos(1), 19)
   !   return
   ! elseif (str_eq(eosstr,'MBWR32')) then
   !   if (nc /= 1) call stoperror("MBWR equation only for single component.")
   !   if (allocated(cbeos%mbwr_meos)) deallocate(cbeos%mbwr_meos)
   !   allocate(cbeos%mbwr_meos(1))
   !   cbeos%eosidx = eos_single
   !   cbeos%subeosidx = meosMbwr32
   !   call initializeMBWRmodel(comp(1)%ident, cbeos%mbwr_meos(1), 32)
   !   return
   ! elseif (str_eq(eosstr,'NIST_MEOS')) then
   !   if (nc /= 1) call stoperror("NIST_MEOS only implemented for pure components.")
   !   cbeos%eosidx = eos_single
   !   if (allocated(cbeos%nist)) then
   !     do i=1,nc
   !       deallocate(cbeos%nist(i)%meos, stat=err)
   !       if (err /= 0) call stoperror("Not able to deallocate cbeos%nist(i)")
   !     enddo
   !     deallocate(cbeos%nist, stat=err)
   !     if (err /= 0) call stoperror("Not able to deallocate cbeos%nist")
   !   endif
   !   allocate(cbeos%nist(nc), stat=err)
   !   cbeos%subeosidx = meosNist
   !   if (str_eq(comp(1)%ident, "C3")) then
   !     allocate(meos_c3 :: cbeos%nist(1)%meos, stat=err)
   !   elseif (str_eq(comp(1)%ident,"N-H2")) then
   !     allocate(meos_normal_h2 :: cbeos%nist(1)%meos, stat=err)
   !   elseif (str_eq(comp(1)%ident,"O-H2")) then
   !     allocate(meos_ortho_h2 :: cbeos%nist(1)%meos, stat=err)
   !   elseif (str_eq(comp(1)%ident,"P-H2")) then
   !     allocate(meos_para_h2 :: cbeos%nist(1)%meos, stat=err)
   !   elseif (str_eq(comp(1)%ident,"R134A")) then
   !     allocate(meos_r134a :: cbeos%nist(1)%meos, stat=err)
   !   else
   !     call stoperror("Only possible to use NIST MEOS with components: C3 or N/O/P-H2, or R134A")
   !   end if
   !   if (err /= 0) call stoperror("Not able to allocate cbeos%nist(1)%meos")
   !   call cbeos%nist(1)%meos%init()
   !   !Rgas_meos = cbeos%nist(1)%Rgas_fit ! use fitting value of Rgas
   !   return
   ! elseif (str_eq(eosstr,'NIST_MEOS_MIX')) then
   !   cbeos%eosidx = meosNist_mix
   !   if (allocated(cbeos%nist)) then
   !     do i=1,nc
   !       deallocate(cbeos%nist(i)%meos, stat=err)
   !       if (err /= 0) call stoperror("Not able to deallocate cbeos%nist(i), mix")
   !     enddo
   !     deallocate(cbeos%nist, stat=err)
   !     if (err /= 0) call stoperror("Not able to deallocate cbeos%nist(i), mix")
   !   endif
   !   allocate(cbeos%nist(nc), stat=err)
   !   !< Loop through components and search components with multiparameter EoS
   !   ! available
   !   do iz=1,nc
   !     if (str_eq(comp(1)%ident, "C3")) then
   !       allocate(meos_c3 :: cbeos%nist(iz)%meos, stat=err)
   !     elseif (str_eq(comp(iz)%ident,"O-H2")) then
   !       allocate(meos_ortho_h2 :: cbeos%nist(iz)%meos, stat=err)
   !     elseif (str_eq(comp(iz)%ident,"P-H2")) then
   !       allocate(meos_para_h2 :: cbeos%nist(iz)%meos, stat=err)
   !     else
   !       call stoperror("Only possible to use NIST MIX MEOS with these components for now: O/P-H2 or C3")
   !     end if
   !     call cbeos%nist(iz)%meos%init()
   !   enddo
   !   if (err /= 0) call stoperror("Not able to allocate cbeos%nist_Mix(iz)%meos")
   !    !Rgas_meos = cbeos%nist(1)%Rgas_fit ! use fitting value of Rgas
   !   return
   ! else
   !   Call StopError ('Unknown EOS')
   ! endif

    call get_mixing_rule_index(cbeos%eosidx, mrulestr, mruleidx)
    cbeos%mruleidx = mruleidx
    cbeos%mruleid = trim(mrulestr)
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
    use thermopack_var, only: nce, get_active_eos_container, eos_container
    !use tpcbmix, only: cbCalcParameters
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
    type(eos_container), pointer :: p_act_eosc
    !
    p_act_eosc => get_active_eos_container()
    if ( p_act_eosc%eosidx /= eosCubic ) then
      print *,"Not able to redefine component. Returning."
      ierr = 1
      return
    else
      ierr = 0
    endif

    p_act_eosc%comps(j)%p_comp%tc = TcSpec
    p_act_eosc%comps(j)%p_comp%pc = PcSpec
    p_act_eosc%comps(j)%p_comp%Acf = AcfSpec
    !
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=1,ncbeos
      select type ( p_eos => p_act_eosc%eos(i)%p_eos )
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

      ! ierr = redefine_TcPcAcf_comp_cubic_inner(p_active_eos_c%eos(i)%p_eos, j, &
      !      TcSpec, PcSpec, AcfSpec)
      ! if (ierr /= 0) then
      !   print *,"Not able to redefine component. Not cubic."
      !   ierr = 1
      !   exit
      ! endif
    enddo
  ! contains
  !   function redefine_TcPcAcf_comp_cubic_inner(p_eos, j, TcSpec, PcSpec, AcfSpec) result(ierr)
  !     use thermopack_var, only: nce, base_eos_param
  !     use cubic_eos, only: cb_eos
  !     class(base_eos_param), intent(inout) :: p_eos
  !     integer, intent(in) :: j !< Component index
  !     real, intent(in) :: TcSpec !< Specified critical temperature [K]
  !     real, intent(in) :: PcSpec !< Specified critical pressure [Pa]
  !     real, intent(in) :: AcfSpec !< Specified acentric factor [-]
  !     integer :: ierr
  !     select type ( p_eos )
  !     class is ( cb_eos )
  !       ierr = 0
  !       p_eos%single(j)%Tc = TcSpec
  !       p_eos%single(j)%Pc = PcSpec
  !       p_eos%single(j)%Acf = AcfSpec
  !       call cbCalcParameters(nce, p_eos)
  !     class default
  !       ierr = 1
  !     end select
  !   end function redefine_TcPcAcf_comp_cubic_inner
  end subroutine redefine_TcPcAcf_comp_cubic

  !> Redefine the critical temperature, critical pressure, and acentric factor
  !> of the cubic fallback EoS. Can be used to enforce the fallback EoS to have
  !> the same Tc, Pc and Acf as the main EoS.
  !
  !> \author Ailo Aasen
  subroutine redefine_fallback_TcPcAcf(TcSpec, PcSpec, AcfSpec)
    use thermopack_var, only: nce, get_active_eos_container, eos_container
    use eosdata, only: eosCubic
    use cubic_eos, only: cb_eos
    !$ use omp_lib, only: omp_get_max_threads
    real, intent(in), optional :: TcSpec(nce) !< Specified critical temperature [K]
    real, intent(in), optional :: PcSpec(nce) !< Specified critical pressure [Pa]
    real, intent(in), optional :: AcfSpec(nce) !< Specified acentric factor [-]
    ! Locals
    integer :: i, ncbeos
    type(eos_container), pointer :: p_act_eosc
    p_act_eosc => get_active_eos_container()
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=1,ncbeos
      select type ( p_eos => p_act_eosc%cubic_eos_alternative(i)%p_eos )
      class is ( cb_eos )
        call initCubicTcPcAcf(nce, p_act_eosc%comps, p_eos, TcSpec, PcSpec, AcfSpec)
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
        case (cbMixHuronVidal, cbmixHuronVidal2, cbmixNRTL)
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,"Classic",&
               param_reference,comp(i)%p_comp%ident,comp(j)%p_comp%ident) !< First get the kij for vdW
          cbeos%kij(j,i) = cbeos%kij(i,j)
          call getInterDataGEij(cbeos%mixGE,cbeos%eosid,param_reference,&
               comp(i)%p_comp%ident,comp(j)%p_comp%ident,i,j,found_ge) !< get both  the ij- and ji-pair
          !if (found_int == 0) print *, "WARNING: HV parameters for binary ",comp(i)%p_comp%ident,comp(j)%p_comp%ident," not in database." ! This generates too much output. Should be uncommented when debugging.
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

  !---------------------------------------------------------------------- >
  !< Copy the parameters from the database in tpinput.f90 go the current cubic equation of state
  !! \param cbdb The database element
  !! \param cb The current cubic eos -- treated as a local variable here .
  !!
  !!
  !!
  !! \author Geir S

!  subroutine copyEOSdataDB (cbdb,cb)
!    use eosdatadb
!    use eosdata
!
!    implicit none
!
!    type (eoscubicdb), intent (in) :: cbdb
!    type (eoscubic), intent (inout) :: cb
!
!    cb%eosid = cbdb%eosid
!    cb%name = cbdb%name
!
!    cb%delta = cbdb%delta
!    cb%alfa = cbdb%alfa
!    cb%beta = cbdb%beta
!    cb%gamma = cbdb%gamma
!
!  end subroutine copyEOSdataDB

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
    use stringmod, only: str_eq, string_match
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    character(len=*), intent(in) :: eosid, mruleid, uid1, uid2, ref
    real :: kijvalue
    ! Locals
    integer :: idx, idx_default
    logical :: found
    character(len=max(len(eosid),2)) :: eosid_local

    idx_default = -1
    idx = 1
    found = .false.
    eosid_local = eosid
    if (cbeos%eosidx == eosLK) then
      eosid_local = 'LK'
      kijvalue = 1.0 !< Default value - no interaction
    else
      kijvalue = 0.0 !< Default value - no interaction
    endif

    do while (idx <= maxkij .and. .not. found)
       if ( str_eq (eosid_local,kijdb(idx)%eosid) &
            .and. str_eq(mruleid,kijdb(idx)%mruleid) &
            .and. ((str_eq(uid1,kijdb(idx)%uid1) &
            .and. str_eq(uid2,kijdb(idx)%uid2)) &
            .or. ( str_eq(uid1,kijdb(idx)%uid2) &
            .and. str_eq(uid2,kijdb(idx)%uid1)))) then
         if (string_match(ref,kijdb(idx)%ref)) then
           found = .true.
           kijvalue = kijdb(idx)%kijvalue
           exit
         elseif (str_eq(kijdb(idx)%ref, "DEFAULT")) then
           idx_default = idx
         endif
       endif
       idx = idx + 1
    enddo
    if (.not. found .and. idx_default > 0) then
      kijvalue = kijdb(idx_default)%kijvalue
    endif
  end function getkij

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
