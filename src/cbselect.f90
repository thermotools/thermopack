!> Selection of components, eos etc
!
!

module tpselect
  use compdatadb, only: getCompDbindex
  implicit none
  save

  public :: SelectComp, SelectEOS
  public :: tpSelectInteractionParameters, TP_CPmethod, deAllocateEosCubic
  public :: redefine_fallback_TcPcAcf

contains

  !> Select all components in the mixture
  !! The parameters are:
  !!
  !!  \param comp_string The compontn string sperated either by "," or by " ".
  !!
  !! Example tpSelectComp ('C1 CO2 N2') or tpSelectComp ('C1,CO2,N2')
  !!
  !! \author Geir S
  subroutine SelectComp(comp_string,nc,comp)
    use compdata
    use compdatadb
    use parameters, only: clen, getComp, parseCompVector
    implicit none
    character(len=*), intent(in) :: comp_string
    integer, intent(out) :: nc
    type(gendata), allocatable, dimension(:), intent(out) :: comp

    integer :: i, cdbindex = 0, err=0,ipos=0
    character (len=clen) :: compstr, icompstr
    compstr = comp_string
    nc = parseCompVector(trim(compstr))
    if (nc <= 0) then
      write(*,*) 'Invalid no. of components:',nc
      call StopError('')
    end if

    if (allocated (comp)) deallocate (comp, STAT=err)
    allocate(comp(nc),STAT=err)
    if (err /= 0) Call StopError('Could not allocate comp array!')

    do i=1,nc
      ipos = getComp(trim(compstr))
      icompstr=compstr(1:ipos)
      cdbindex = getCompDBIndex(icompstr)
      if (cdbindex == 0 .or.  cdbindex > maxncdb) then
        write (*,*) 'Look for component: ', trim(icompstr)
        Call StopError('Could not find component in DB')
      endif
      comp(i) = copyFromDB(cdbindex)
      comp(i)%idx = cdbindex
      ! next
      compstr = compstr(ipos+2:clen)
    enddo

  end subroutine SelectComp

  !> Free the memory allocated after initializing components, compositions and
  !! equation of state
  !!
  !! \todo Need to check if this should be called prior to initializing.  From
  !! for instance excel and other non-sequantial 'look-up' type interfaces where
  !! the library need to initialized for each call.
  !!
  !! \author Geir S

  subroutine DeSelectComp(comp,cbeos)
    use compdata, only: gendata
    use eosdata, only: eoscubic
    use parameters, only: complist
    implicit none
    type (gendata), allocatable, dimension(:), intent(inout) :: comp
    type (eoscubic), intent(inout) :: cbeos
    integer :: stat = 0

    if (allocated (complist)) deallocate (complist,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating complist'

    if (allocated (comp)) deallocate (comp,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating comp'

    call deAllocateEosCubic(cbeos)

  end subroutine DeSelectComp

  !> Free the memory allocated in SelectEOS
  !!
  !! \author Morten H
  subroutine deAllocateEosCubic(cbeos)
    use eosdata, only: eoscubic
    implicit none
    type (eoscubic), intent(inout) :: cbeos
    integer :: stat, i
    stat = 0

    if (allocated (cbeos%kij)) deallocate (cbeos%kij,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating kij'

    if (allocated (cbeos%ai)) deallocate (cbeos%ai, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating ai'

    if (allocated (cbeos%ait)) deallocate (cbeos%ait, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating ait'

    if (allocated (cbeos%bit)) deallocate (cbeos%bit, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating bit'

    if (allocated (cbeos%aij)) deallocate (cbeos%aij, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating aij'

    if (allocated (cbeos%bi)) deallocate (cbeos%bi, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating bi'

    if (allocated (cbeos%bij)) deallocate (cbeos%bij, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating bij'

    if (allocated (cbeos%lowcase_bij)) deallocate (cbeos%lowcase_bij, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating lowcase_bij'

    if (allocated (cbeos%ci)) deallocate (cbeos%ci, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating ci'

    if (allocated (cbeos%cij)) deallocate (cbeos%cij, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating cij'

    if (allocated (cbeos%single)) deallocate (cbeos%single, STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating single'

    if (allocated (cbeos%mixGE%alpha)) deallocate (cbeos%mixGE%alpha,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating mixGE%alpha'

    if (allocated (cbeos%mixGE%aGE)) deallocate (cbeos%mixGE%aGE,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating mixGE%aGE'

    if (allocated (cbeos%mixGE%bGE)) deallocate (cbeos%mixGE%bGE,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating mixGE%bGE'

    if (allocated (cbeos%mixGE%cGE)) deallocate (cbeos%mixGE%cGE,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating mixGE%cGE'

    if (allocated (cbeos%mixGE%correlation)) deallocate (cbeos%mixGE%correlation,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating mixGE%correlation'

    if (allocated (cbeos%mbwr_meos)) deallocate (cbeos%mbwr_meos,STAT=stat)
    if (stat /= 0) write (*,*) 'Error deallocating mbwr_meos'

    if (allocated (cbeos%nist)) then
      do i=1,size(cbeos%nist)
        deallocate (cbeos%nist(i)%meos,STAT=stat)
        if (stat /= 0) write (*,*) 'Error deallocating nist(i)%meos'
      enddo
      deallocate (cbeos%nist,STAT=stat)
      if (stat /= 0) write (*,*) 'Error deallocating nist'
    endif

  end subroutine deAllocateEosCubic

  !----------------------------------------------------------------------
  !> Selection of equation of state and the mixing rule
  !! Data from the eos-database is copied to the global variable cbeos
  !! \param eosstr The equation of state as a character string e.g 'SRK' og 'PR'
  !! \param mrulestr The mixing rule as a character string e.g 'Classic'
  !!
  !! The character strings are case-insensitive
  !!
  !! \author Geir S
  subroutine SelectEOS(nc, comp, cbeos, eosstr, mrulestr, alphastr, kij_setno, alpha_setno, b_exponent)
    use eosdata
    use eosdatadb
    use stringmod, only: str_eq
    use compdata, only: gendata
    use tpcbmix, only: cbCalcParameters
    use unifac, only: init_unifac, unifdb
    use cbAlpha, only: tpInitAlphaCorr
    use tpmbwr, only: initializeMBWRmodel
    use multiparameter_base, only: meos
    use multiparameter_c3, only: meos_c3
    use multiparameter_ortho_h2, only: meos_ortho_h2
    use multiparameter_para_h2, only: meos_para_h2
    use multiparameter_normal_h2, only: meos_normal_h2
    use multiparameter_r134a, only: meos_r134a
    implicit none
    integer, intent(in) :: nc
    type (gendata), dimension(nc), intent(in) :: comp
    type (eoscubic) :: cbeos
    character (len=*), intent (in) :: eosstr
    character (len=*), intent (in) :: mrulestr
    character (len=*), optional, intent (in) :: alphastr
    integer, optional, intent(in) :: kij_setno
    integer, optional, intent(in) :: alpha_setno
    real, optional, intent(in) :: b_exponent
    ! Locals
    integer :: err
    integer :: setno !< Possibility of more than one set of kij
    integer :: i,j,iz
    type(Fraction):: ZeroFraction !<= 0.0/1.0

    ZeroFraction%pNum = 0.0
    ZeroFraction%pDen = 0.0
    ZeroFraction%pDen(1) = 1.0

    err = 0
    if (present(kij_setno)) then
      setno = kij_setno
    else
      setno = 1
    endif
    cbeos%eosid = eosstr  !Moved before setting Alhpa Correlation. Alpha correlation depends on it

    cbeos%cubic_verbose = .false.
    cbeos%subeosidx = 0
    if (str_eq(eosstr,'SRK')) then
      cbeos%eosidx = cbSRK
    elseif (str_eq(eosstr, 'SRKGB')) then
      cbeos%eosidx = cbSRKGB
    elseif (str_eq(eosstr,'PR')) then
      cbeos%eosidx = cbPR
    elseif (str_eq(eosstr,'VdW')) then
      cbeos%eosidx = cbVdW
    elseif (str_eq(eosstr,'RK')) then
      cbeos%eosidx = cbRK
    elseif (str_eq(eosstr,'SW')) then
      cbeos%eosidx = cbSW
    elseif (str_eq(eosstr,'PT')) then
      cbeos%eosidx = cbPT
    elseif (str_eq(eosstr,'CSP-SRK')) then
      cbeos%eosidx = cspSRK
    elseif (str_eq(eosstr,'CSP-SRKGB')) then
      cbeos%eosidx = cspSRKGB
    elseif (str_eq(eosstr,'CSP-PR')) then
      cbeos%eosidx = cspPR
    elseif (str_eq(eosstr,'LK')) then
      cbeos%eosidx = eosLK
    elseif (str_eq(eosstr,'CPA-SRK')) then
      cbeos%eosidx = cpaSRK
    elseif (str_eq(eosstr,'CPA-PR')) then
      cbeos%eosidx = cpaPR
   elseif (str_eq(eosstr,'PC-SAFT')) then
      cbeos%eosidx = eosPC_SAFT
   elseif (str_eq(eosstr,'PETS')) then
      cbeos%eosidx = eosPeTS
   elseif (str_eq(eosstr,'SAFT-VR-MIE')) then
     cbeos%eosidx = eosBH_pert
     cbeos%subeosidx = eosSAFT_VR_MIE
   elseif (str_eq(eosstr,'LJS')) then
     cbeos%eosidx = eosBH_pert
     cbeos%subeosidx = eosLJsplined
   elseif (str_eq(eosstr,'MBWR19')) then
     if (nc /= 1) call stoperror("MBWR equation only for single component.")
     cbeos%eosidx = eos_single
     cbeos%subeosidx = meosMbwr19
     if (allocated(cbeos%mbwr_meos)) deallocate(cbeos%mbwr_meos)
     allocate(cbeos%mbwr_meos(1))
     call initializeMBWRmodel(comp(1)%ident, cbeos%mbwr_meos(1), 19)
     return
   elseif (str_eq(eosstr,'MBWR32')) then
     if (nc /= 1) call stoperror("MBWR equation only for single component.")
     if (allocated(cbeos%mbwr_meos)) deallocate(cbeos%mbwr_meos)
     allocate(cbeos%mbwr_meos(1))
     cbeos%eosidx = eos_single
     cbeos%subeosidx = meosMbwr32
     call initializeMBWRmodel(comp(1)%ident, cbeos%mbwr_meos(1), 32)
     return
   elseif (str_eq(eosstr,'NIST_MEOS')) then
     if (nc /= 1) call stoperror("NIST_MEOS only implemented for pure components.")
     cbeos%eosidx = eos_single
     if (allocated(cbeos%nist)) then
       do i=1,nc
         deallocate(cbeos%nist(i)%meos, stat=err)
         if (err /= 0) call stoperror("Not able to deallocate cbeos%nist(i)")
       enddo
       deallocate(cbeos%nist, stat=err)
       if (err /= 0) call stoperror("Not able to deallocate cbeos%nist")
     endif
     allocate(cbeos%nist(nc), stat=err)
     cbeos%subeosidx = meosNist
     if (str_eq(comp(1)%ident, "C3")) then
       allocate(meos_c3 :: cbeos%nist(1)%meos, stat=err)
     elseif (str_eq(comp(1)%ident,"N-H2")) then
       allocate(meos_normal_h2 :: cbeos%nist(1)%meos, stat=err)
     elseif (str_eq(comp(1)%ident,"O-H2")) then
       allocate(meos_ortho_h2 :: cbeos%nist(1)%meos, stat=err)
     elseif (str_eq(comp(1)%ident,"P-H2")) then
       allocate(meos_para_h2 :: cbeos%nist(1)%meos, stat=err)
     elseif (str_eq(comp(1)%ident,"R134A")) then
       allocate(meos_r134a :: cbeos%nist(1)%meos, stat=err)
     else
       call stoperror("Only possible to use NIST MEOS with components: C3 or N/O/P-H2, or R134A")
     end if
     if (err /= 0) call stoperror("Not able to allocate cbeos%nist(1)%meos")
     call cbeos%nist(1)%meos%init()
     !Rgas_meos = cbeos%nist(1)%Rgas_fit ! use fitting value of Rgas
     return
   elseif (str_eq(eosstr,'NIST_MEOS_MIX')) then
     cbeos%eosidx = meosNist_mix
     if (allocated(cbeos%nist)) then
       do i=1,nc
         deallocate(cbeos%nist(i)%meos, stat=err)
         if (err /= 0) call stoperror("Not able to deallocate cbeos%nist(i), mix")
       enddo
       deallocate(cbeos%nist, stat=err)
       if (err /= 0) call stoperror("Not able to deallocate cbeos%nist(i), mix")
     endif
     allocate(cbeos%nist(nc), stat=err)
     !< Loop through components and search components with multiparameter EoS
     ! available
     do iz=1,nc
       if (str_eq(comp(1)%ident, "C3")) then
         allocate(meos_c3 :: cbeos%nist(iz)%meos, stat=err)
       elseif (str_eq(comp(iz)%ident,"O-H2")) then
         allocate(meos_ortho_h2 :: cbeos%nist(iz)%meos, stat=err)
       elseif (str_eq(comp(iz)%ident,"P-H2")) then
         allocate(meos_para_h2 :: cbeos%nist(iz)%meos, stat=err)
       else
         call stoperror("Only possible to use NIST MIX MEOS with these components for now: O/P-H2 or C3")
       end if
       call cbeos%nist(iz)%meos%init()
     enddo
     if (err /= 0) call stoperror("Not able to allocate cbeos%nist_Mix(iz)%meos")
      !Rgas_meos = cbeos%nist(1)%Rgas_fit ! use fitting value of Rgas
     return
   else
     Call StopError ('Unknown EOS')
   endif

    if (str_eq(mrulestr,'CLASSIC')) then
      cbeos%mruleid = trim(mrulestr)
      cbeos%mruleidx = cbMixClassic
    elseif (str_eq(mrulestr,'REID')) then
      cbeos%mruleid = trim(mrulestr)
      cbeos%mruleidx = cbMixReid
    elseif (str_eq(mrulestr,'HV') .or. str_eq(mrulestr,'HV0') .or. str_eq(mrulestr,'HV1')) then
      cbeos%mruleid = trim(mrulestr)
      cbeos%mruleidx = cbMixHuronVidal
      cbeos%mixGE%mGE = cbMixHuronVidal
    elseif (str_eq(mrulestr,'HV2')) then
       cbeos%mruleid = trim(mrulestr)
       cbeos%mruleidx = cbMixHuronVidal2
       cbeos%mixGE%mGE = cbMixHuronVidal2
    elseif (str_eq(mrulestr, 'WONGSANDLER') .or. str_eq(mrulestr,"WS")) then
       cbeos%mruleid = trim(mrulestr)
       cbeos%mruleidx = cbMixWongSandler
    elseif (str_eq(mrulestr, 'NRTL')) then
       cbeos%mruleid = trim(mrulestr)
       cbeos%mruleidx = cbMixNRTL
       cbeos%mixGE%mGE = cbMixNRTL
    elseif (str_eq(mrulestr, 'UNIFAC') .or. str_eq(mrulestr, 'UMR') .or. str_eq(mrulestr, 'VTPR')) then
       cbeos%mruleid = trim(mrulestr)
       cbeos%mruleidx = cbMixUNIFAC
    else
      write (*,*) 'Mixing rule: ',trim(mrulestr)
      Call StopError ('Unknown mixing rule')
    endif

    ! Use the CPA interaction parameters if they exist, if not use the standard database. The details are in tpSelectInteractionParameters.
    if (len(eosstr) .ge. 3) then
       if (str_eq(eosstr(1:3),'CPA')) then
          if (str_eq(mrulestr,'CLASSIC')) then
             cbeos%mruleid = "CLASSIC(CPA)"
             cbeos%mruleidx = cbMixClassicCPA
           elseif (str_eq(mrulestr,'HV') .or. &
                str_eq(mrulestr,'HV0') .or. &
                str_eq(mrulestr,'HV1')) then
            cbeos%mruleid = trim(mrulestr)
            cbeos%mruleidx = cbMixHVCPA
            cbeos%mixGE%mGE = cbMixHVCPA
          elseif (str_eq(mrulestr,'HV2')) then
            cbeos%mruleid = trim(mrulestr)
            cbeos%mruleidx = cbMixHVCPA2
            cbeos%mixGE%mGE = cbMixHVCPA2
          else
            call stoperror("Selected mixing rule not implemented for cubic part of the CPA model.")
          end if
       end if
    end if

    ! Use only one active set - at the time deallocate before reallocating
    if (allocated (cbeos%kij)) deallocate (cbeos%kij, STAT=err)
    allocate (cbeos%kij(nc,nc), STAT=err)

    ! For Huron vidal
    if (cbeos%mruleidx == cbMixHuronVidal .or. &
        cbeos%mruleidx == cbMixHuronVidal2 .or. &
        cbeos%mruleidx == cbMixNRTL .or. &
        cbeos%mruleidx == cbMixHVCPA .or. &
        cbeos%mruleidx == cbMixHVCPA2) then
       if (allocated (cbeos%mixGE%alpha)) deallocate (cbeos%mixGE%alpha, STAT=err)
       if (allocated (cbeos%mixGE%aGE)) deallocate (cbeos%mixGE%aGE, STAT=err)
       if (allocated (cbeos%mixGE%bGE)) deallocate (cbeos%mixGE%bGE, STAT=err)
       if (allocated (cbeos%mixGE%cGE)) deallocate (cbeos%mixGE%cGE, STAT=err)
       if (allocated (cbeos%mixGE%correlation)) deallocate (cbeos%mixGE%correlation, STAT=err)
       allocate (cbeos%mixGE%alpha(nc,nc))
       allocate (cbeos%mixGE%aGE(nc,nc))
       allocate (cbeos%mixGE%bGE(nc,nc))
       allocate (cbeos%mixGE%cGE(nc,nc))
       allocate (cbeos%mixGE%correlation(nc,nc))
       do i=1,nc
          do j=1,nc
             cbeos%mixGE%alpha(i,j) = 0.0
             cbeos%mixGE%aGE(i,j) = 0.0
             cbeos%mixGE%bGE(i,j) = 0.0
             cbeos%mixGE%cGE(i,j) = 0.0
             cbeos%mixGE%correlation(i,j) = 0
          enddo
       enddo
    endif

    if (cbeos%mruleidx == cbMixWongSandler ) then
       if ( allocated (cbeos%mixWS%f_kij) ) deallocate (cbeos%mixWS%f_kij, STAT = err)
       if ( allocated (cbeos%mixWS%f_tauij) ) deallocate (cbeos%mixWS%f_Tauij, STAT = err)
       if ( allocated (cbeos%mixWS%Alphaij) ) deallocate (cbeos%mixWS%Alphaij, STAT = err)
       allocate (cbeos%mixWS%f_kij(nc,nc))
       allocate (cbeos%mixWS%f_Tauij(nc,nc))
       allocate (cbeos%mixWS%Alphaij(nc,nc))
       do i=1,nc
          do j=1,nc
             cbeos%mixWS%f_kij(i,j) = ZeroFraction
             cbeos%mixWS%f_Tauij(i,j) = ZeroFraction
             cbeos%mixWS%Alphaij(i,j) = 0.0
          enddo
       enddo
    endif

    ! For mixing rules in general
    if (allocated (cbeos%ai)) deallocate (cbeos%ai, STAT=err)
    allocate (cbeos%ai(nc), STAT=err)

    if (allocated (cbeos%ait)) deallocate (cbeos%ait, STAT=err)
    allocate (cbeos%ait(nc), STAT=err)

    if (allocated (cbeos%bit)) deallocate (cbeos%bit, STAT=err)
    allocate (cbeos%bit(nc), STAT=err)

    if (allocated (cbeos%aij)) deallocate (cbeos%aij, STAT=err)
    allocate (cbeos%aij(nc,nc), STAT=err)

    if (allocated (cbeos%bij)) deallocate (cbeos%bij, STAT=err)
    allocate (cbeos%bij(nc,nc), STAT=err)

    if (allocated (cbeos%lowcase_bij)) deallocate (cbeos%lowcase_bij, STAT=err)
    allocate (cbeos%lowcase_bij(nc,nc), STAT=err)

    if (allocated (cbeos%cij)) deallocate (cbeos%cij, STAT=err)
    allocate (cbeos%cij(nc,nc), STAT=err)

    if (allocated (cbeos%bi)) deallocate (cbeos%bi, STAT=err)
    allocate (cbeos%bi(nc), STAT=err)

    if (allocated (cbeos%ci)) deallocate (cbeos%ci, STAT=err)
    allocate (cbeos%ci(nc), STAT=err)

    if (allocated (cbeos%single)) deallocate (cbeos%single, STAT=err)
    allocate (cbeos%single(nc), STAT=err)

    if (err /= 0) Call StopError('SelectEOS::Could not allocate array')

    ! The remaining code calculates the constant parameters in the cubic EoS

    ! Initialize critical temperature, critical pressure, and acentric factor of
    ! the cubic EoS
    call initCubicTcPcAcf(nc, comp, cbeos)

    ! Set up parameters for EOS
    if (.not. (cbeos%eosidx == eosPC_SAFT .or. cbeos%eosidx == eosBH_pert)) then
      call cbCalcParameters(nc, comp, cbeos, b_exponent)
    endif

    ! Set alpha correlation
    if (present(alphastr)) then
       call tpInitAlphaCorr(nc, comp, cbeos, alphastr, alpha_setno)
    else
       call tpInitAlphaCorr(nc, comp, cbeos, "CLASSIC", alpha_setno)
    endif

    if (cbeos%mruleidx == cbMixUNIFAC) then
      call init_unifac(unifdb,mrulestr)
    else
      ! Select default set no 1 interaction paramteres
      call tpSelectInteractionParameters(nc, comp, cbeos, setno)
    endif

  end subroutine SelectEOS

  !> Initialize Tc, Pc and acentric factor to use in the cubic EoS
  !>
  !> \author Ailo Aasen
  subroutine initCubicTcPcAcf(nc, comp, cbeos, TcSpec, PcSpec, AcfSpec)
    use eosdata, only: eoscubic
    use compdata, only: gendata
    integer, intent(in) :: nc
    type (gendata), dimension(nc), intent(in) :: comp
    type (eoscubic) :: cbeos
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
        cbeos%single(i)%Tc = comp(i)%tc
      end if

      if (present(PcSpec)) then
        cbeos%single(i)%Pc = PcSpec(i)
      else
        cbeos%single(i)%Pc = comp(i)%pc
      end if

      if (present(AcfSpec)) then
        cbeos%single(i)%Acf = AcfSpec(i)
      else
        cbeos%single(i)%Acf = comp(i)%acf
      end if
    end do
  end subroutine initCubicTcPcAcf

  !> Redefine the critical temperature, critical pressure, and acentric factor
  !> of the cubic EoS.
  !
  !> \author Morten Hammer
  subroutine redefine_TcPcAcf_comp_cubic(j,TcSpec, PcSpec, AcfSpec, ierr)
    use tpvar, only: nce, comp, cbeos
    use tpcbmix, only: cbCalcParameters
    use eosdata, only: cbSRK, cbSRKGB, cbPR, cbVdW, cbRK
    !$ use omp_lib, only: omp_get_max_threads
    integer, intent(in) :: j !< Component index
    integer, intent(out) :: ierr !< Component index
    real, intent(in) :: TcSpec !< Specified critical temperature [K]
    real, intent(in) :: PcSpec !< Specified critical pressure [Pa]
    real, intent(in) :: AcfSpec !< Specified acentric factor [-]
    ! Locals
    integer :: i, ncbeos
    !
    if ( cbeos(1)%eosidx /= cbSRK .and. &
         cbeos(1)%eosidx /= cbSRKGB .and. &
         cbeos(1)%eosidx /= cbPR .and. &
         cbeos(1)%eosidx /= cbVdW .and. &
         cbeos(1)%eosidx /= cbRK) then
      print *,"Not able to redefine component. Returning."
      ierr = 1
    else
      ierr = 0
    endif
    comp(j)%tc = TcSpec
    comp(j)%pc = PcSpec
    comp(j)%Acf = AcfSpec
    !
    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=1,ncbeos
      cbeos(i)%single(j)%Tc = TcSpec
      cbeos(i)%single(j)%Pc = PcSpec
      cbeos(i)%single(j)%Acf = AcfSpec
      call cbCalcParameters(nce, comp, cbeos(i))
    enddo

  end subroutine redefine_TcPcAcf_comp_cubic

  !> Redefine the critical temperature, critical pressure, and acentric factor
  !> of the cubic fallback EoS. Can be used to enforce the fallback EoS to have
  !> the same Tc, Pc and Acf as the main EoS.
  !
  !> \author Ailo Aasen
  subroutine redefine_fallback_TcPcAcf(TcSpec, PcSpec, AcfSpec)
    use tpvar, only: nce, comp, cbeos_alternative
    use tpcbmix, only: cbCalcParameters
    use eosdata, only: eoscubic
    !$ use omp_lib, only: omp_get_max_threads
    real, intent(in), optional :: TcSpec(nce) !< Specified critical temperature [K]
    real, intent(in), optional :: PcSpec(nce) !< Specified critical pressure [Pa]
    real, intent(in), optional :: AcfSpec(nce) !< Specified acentric factor [-]
    ! Locals
    integer :: i, ncbeos

    ncbeos = 1
    !$ ncbeos = omp_get_max_threads()
    do i=1,ncbeos
      call initCubicTcPcAcf(nce, comp, cbeos_alternative(i), TcSpec, PcSpec, AcfSpec)
      call cbCalcParameters(nce, comp, cbeos_alternative(i))
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
  subroutine tpSelectInteractionParameters (nc,comp,cbeos,setno)
    use eosdata
    use compdata, only: gendata
    use stringmod, only: str_eq
    use CPA_parameters, only: getCPAkij_a, getCpaGEij
    implicit none
    integer, intent(in) :: setno, nc
    type (eoscubic), intent(inout) :: cbeos
    type (gendata), dimension(nc), intent(in) :: comp
    ! Locals
    integer :: i,j !< Counters
    logical :: found
    integer :: found_int

    if (allocated (cbeos%kij) .eqv. .false. ) call StopError('Equation of state are not selected')

    do i=1,nc-1
      do j=i+1,nc

        mixingrule: select case (cbeos%mruleidx)
        case (cbMixReid) !< Asymetric
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,cbeos%mruleid,setno,comp(i)%ident,comp(j)%ident)
          cbeos%kij(j,i) = getkij(cbeos,cbeos%eosid,cbeos%mruleid,setno,comp(j)%ident,comp(i)%ident)
        case (cbMixClassic)
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,cbeos%mruleid,setno,comp(i)%ident,comp(j)%ident)
          cbeos%kij(j,i) = cbeos%kij(i,j)
        case (cbMixHuronVidal, cbmixHuronVidal2, cbmixNRTL)
          cbeos%kij(i,j) = getkij(cbeos,cbeos%eosid,"Classic",setno,comp(i)%ident,comp(j)%ident) !< First get the kij for vdW
          cbeos%kij(j,i) = cbeos%kij(i,j)
          call getInterDataGEij(cbeos%mixGE,cbeos%eosid,setno,&
               comp(i)%ident,comp(j)%ident,i,j,found_int) !< get both  the ij- and ji-pair
          !if (found_int == 0) print *, "WARNING: HV parameters for binary ",comp(i)%ident,comp(j)%ident," not in database." ! This generates too much output. Should be uncommented when debugging.
        case (cbMixClassicCPA,cbMixHVCPA,cbMixHVCPA2)
          cbeos%kij(i,j) = getCPAkij_a(cbeos%eosidx,comp(i)%ident,comp(j)%ident,found=found)
          if ((.not. found) .and. (cbeos%eosidx .eq. cpaSRK)) &
               cbeos%kij(i,j) = getkij(cbeos,eosid="srk",mruleid="classic",&
               setno=setno,uid1=comp(i)%ident,uid2=comp(j)%ident)
          if ((.not. found) .and. (cbeos%eosidx .eq. cpaPR)) &
               cbeos%kij(i,j) = getkij(cbeos,eosid="pr",mruleid="classic",&
               setno=setno,uid1=comp(i)%ident,uid2=comp(j)%ident)
          cbeos%kij(j,i) = cbeos%kij(i,j)
          if (cbeos%mruleidx == cbMixHVCPA .OR. cbeos%mruleidx == cbMixHVCPA2) then
            call getCpaGEij(cbeos%mixGE,cbeos%eosid,setno,&
                 comp(i)%ident,comp(j)%ident,i,j,found) !< get both  the ij- and ji-pair
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

  function getkij (cbeos, eosid, mruleid, setno, uid1, uid2) result(kijvalue)
    use eosdatadb
    use eosdata
    use stringmod, only: str_eq
    implicit none
    type(eoscubic), intent(inout) :: cbeos
    character(len=*), intent(in) :: eosid, mruleid, uid1, uid2
    integer, intent(in) :: setno
    real :: kijvalue
    ! Locals
    integer :: idx, found
    character(len=max(len(eosid),2)) :: eosid_local

    idx = 1
    found = 0
    eosid_local = eosid
    if (cbeos%eosidx == eosLK) then
      eosid_local = 'LK'
      kijvalue = 1.0 !< Default value - no interaction
    else
      kijvalue = 0.0 !< Default value - no interaction
    endif

    do while (idx <= maxkij .and. found == 0)
       if ( str_eq (eosid_local,kijdb(idx)%eosid) &
            .and. str_eq(mruleid,kijdb(idx)%mruleid) &
            .and. (setno == kijdb(idx)%setno &
            .or.  setno == 0)) then
          if ( (str_eq(uid1,kijdb(idx)%uid1) &
               .and. str_eq(uid2,kijdb(idx)%uid2)) & !> then !& !> k_ij
               .or. ( str_eq(uid1,kijdb(idx)%uid2) & !> if classic, symmetrical mixing rule..
               .and. str_eq(uid2,kijdb(idx)%uid1))) then !> k_ji
             found = 1
             kijvalue = kijdb(idx)%kijvalue
          else
             idx = idx + 1
          endif
       else
          idx = idx + 1
       endif
    enddo
    return
  end function getkij

  subroutine getInterDataGEij(mGE, eosid, setno, uid1, uid2, &
       indxi, indxj,found)
    use eosdatadb
    use eosdata
    use stringmod, only: str_eq
    implicit none
    type (mixExcessGibbs), intent(inout) ::  mGE
    character(len=*), intent(in) :: eosid, uid1, uid2
    integer, intent(in) :: setno, indxi, indxj
    integer, optional, intent(out) :: found
    integer :: idx

    idx = 1
    found = 0

    mGE%correlation(indxi, indxj) = 0
    mGE%correlation(indxj, indxi) = 0

    do while (idx <= maxinterGEij .and. found == 0)
      if ( str_eq (eosid,interGEdb(idx)%eosid) &
           .and. (setno == interGEdb(idx)%setno .OR. setno == 0) &
           .and. ( (str_eq ('huronvidal',interGEdb(idx)%mruleid) &
           .and. (mGE%mGE == cbMixHuronVidal &
           .or. mGE%mGE == cbMixHuronVidal2)) &
           .or. (str_eq ('NRTL',interGEdb(idx)%mruleid) &
           .and. mGE%mGE == cbMixNRTL) )) then

        if ( (str_eq(uid1,interGEdb(idx)%uid1) &
             .and. str_eq(uid2,interGEdb(idx)%uid2))) then
          found = 1
          mGE%alpha(indxi,indxj) = interGEdb(idx)%alphaijvalue(1)
          mGE%alpha(indxj,indxi) = interGEdb(idx)%alphaijvalue(2)
          mGE%correlation(indxi,indxj) = interGEdb(idx)%correlation
          mGE%correlation(indxj,indxi) = interGEdb(idx)%correlation

          if (mGE%mGE == cbMixHuronVidal .OR. mGE%mGE == cbMixNRTL) then
            if (interGEdb(idx)%correlation == 2) then
              call stoperror('The Maribo-Mogensen correlation for component interaction require HV2')
            endif
            mGE%aGE(indxi,indxj) = interGEdb(idx)%polyij1(1)
            mGE%aGE(indxj,indxi) = interGEdb(idx)%polyji1(1)
            mGE%bGE(indxi,indxj) = interGEdb(idx)%polyij1(2)
            mGE%bGE(indxj,indxi) = interGEdb(idx)%polyji1(2)
          else
            mGE%aGE(indxi,indxj) = interGEdb(idx)%polyij2(1)
            mGE%aGE(indxj,indxi) = interGEdb(idx)%polyji2(1)
            mGE%bGE(indxi,indxj) = interGEdb(idx)%polyij2(2)
            mGE%bGE(indxj,indxi) = interGEdb(idx)%polyji2(2)
            mGE%cGE(indxi,indxj) = interGEdb(idx)%polyij2(3)
            mGE%cGE(indxj,indxi) = interGEdb(idx)%polyji2(3)
          endif
        elseif ( (str_eq(uid1,interGEdb(idx)%uid2) &
             .and. str_eq(uid2,interGEdb(idx)%uid1)) &
             .and. ( (str_eq ('huronvidal',interGEdb(idx)%mruleid) &
             .and. (mGE%mGE == cbMixHuronVidal .or. mGE%mGE == cbMixHuronVidal2)) &
             .or. (str_eq ('NRTL',interGEdb(idx)%mruleid) &
             .and. mGE%mGE == cbMixNRTL) )) then

          found = 1
          ! switch index i and j
          mGE%alpha(indxj,indxi) = interGEdb(idx)%alphaijvalue(1)
          mGE%alpha(indxi,indxj) = interGEdb(idx)%alphaijvalue(2)
          mGE%correlation(indxi,indxj) = interGEdb(idx)%correlation
          mGE%correlation(indxj,indxi) = interGEdb(idx)%correlation

          if (mGE%mGE == cbMixHuronVidal .OR. mGE%mGE == cbMixNRTL) then
            mGE%aGE(indxi,indxj) = interGEdb(idx)%polyji1(1)
            mGE%aGE(indxj,indxi) = interGEdb(idx)%polyij1(1)
            mGE%bGE(indxi,indxj) = interGEdb(idx)%polyji1(2)
            mGE%bGE(indxj,indxi) = interGEdb(idx)%polyij1(2)
            if (interGEdb(idx)%correlation == 2) then
              call stoperror('The Maribo-Mogensen correlation for component interaction require HV2')
            endif
          else
            mGE%aGE(indxi,indxj) = interGEdb(idx)%polyji2(1)
            mGE%aGE(indxj,indxi) = interGEdb(idx)%polyij2(1)
            mGE%bGE(indxi,indxj) = interGEdb(idx)%polyji2(2)
            mGE%bGE(indxj,indxi) = interGEdb(idx)%polyij2(2)
            mGE%cGE(indxi,indxj) = interGEdb(idx)%polyji2(3)
            mGE%cGE(indxj,indxi) = interGEdb(idx)%polyij2(3)
          endif
        else
          idx = idx + 1 ! Components not found
        endif
      else
        idx = idx + 1 ! EOS or SET not found
      endif
    enddo

  end subroutine getinterdataGEij

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
  !! \author Oivind W
  !!
  subroutine TP_CPmethod(nc,comp,comp_string,Cptype)
    use compdata
    use compdatadb
    use parameters, only: clen, parseCompVector
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(nc), intent(inout) :: comp
    character(len=*), intent(in) :: comp_string
    integer, intent(in) :: Cptype
    integer ::cdbindex = 0, i, index, icp, cindex
    integer :: ncomp = 0!< local
    character(len=256) :: sErr
    character (len=clen) :: compstr

    compstr = comp_string
    ncomp = parseCompVector(trim(compstr))

    if (ncomp <= 0) then
      write(sErr,*) 'No components in tpSelectCpMethod:',ncomp
      call StopError(trim(sErr))
    elseif (ncomp > 1) then
      write(sErr,*) 'Only one comp can be changed at a time:',ncomp
      call StopError(trim(sErr))
    end if

    cdbindex = getCompDBIndex(comp_string) ! Index in full database
    cindex = getCompIndex(nc,comp,comp_string)     ! Index in working database
    if (cindex < 1 .or. cindex > nc) then
      write(sErr,*) 'TP_CPmethod: Component ',trim(comp_string), &
           ' not in active component set.'
      call StopError(trim(sErr))
    endif

    index=0
    do i=1,compdb(cdbindex)%ncptype
      if (compdb(cdbindex)%cptype(i)==Cptype) then
        index=i
      end if
    end do

    if(index>0) then
      comp(cindex)%cptype = compdb(cdbindex)%cptype(index)
      do icp = 1,10
        comp(cindex)%cp(icp) = compdb(cdbindex)%cp(index,icp)
      enddo
      comp(cindex)%tcpmin = compdb(cdbindex)%tcpmin(index)
      comp(cindex)%tcpmax = compdb(cdbindex)%tcpmax(index)
    else
      write(sErr,*) 'This CP-polynom has not yet been implemented: ',Cptype
      call StopError(trim(sErr))
    end if


  end subroutine TP_CPmethod

end module tpselect
