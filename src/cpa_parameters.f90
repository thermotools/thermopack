!> Module for CPA parameters.
MODULE CPA_parameters
  use eosdata, only: cpaSRK, cpaPR, cbAlphaClassicIdx, cbAlphaMCIdx, cbAlphaTwuIdx, &
       cbMixHVCPA, cbMixHVCPA2, mixExcessGibbs
  use eosdatadb, only: interGEdatadb
  use AssocSchemeUtils
  implicit none
  save

  !> PURE COMPONENT PARAMETERS.
  !> This data structure stores pure component parameters for the CPA-SRK and
  !> CPA-PR equations of state.
  !------------------------------------------------------------------------------------------------
  type :: CPAdata
     sequence
     integer :: eosidx
     character (len=10) :: compName
     ! SRK fitted parameters.
     real :: a0 !< [Pa*L^2/mol^2]. The usual a0 parameter.
     real :: b  !< [L/mol]. The usual b parameter.
     real :: alphaParams(3) !< Up to three parameters for use in alpha corr.
     integer :: alphacorridx !< Either cpaClassicIdx, cpaTwuIdx, cpaMcIdx.
     ! Association parameters.
     real :: eps  !< [J/mol]. Caveat: people sometimes tabulate epsilon/R.
     real :: beta !< [-]
     ! Association scheme.
     integer :: assoc_scheme
     ! Fitting method used to obtain the parameters.
     integer :: setno
  end type CPAdata

  ! SOURCES:
  ! [0] Regressed in-house.
  ! [1] Kontogeorgis et al., "Solvation Phenomena in Association Theories with Applications to Oil & Gas and Chemical Industries", Oil and Gas Science and Technology (2008).
  ! [2] Kontogeorgis & Folas, "Thermodynamic Models for Industrial Applications", Wiley 2010 (appendix A).
  ! [3] Queimada 2005, (10.1016/j.fluid.2004.08.011)
  !
  ! Note that setno=1 gives the default parameters.
  ! CAVEAT: Careful about units when adding new parameters. See CPAdata struct type.
  type(CPAdata), parameter :: CPAcx1 = CPAdata(cpaSRK,"MEOH" , &
       4.0531E+5, 0.030978,(/ 0.43102,0.0,0.0/),cbAlphaClassicIdx, 24591.0, 16.1E-3,  assoc_scheme_2B,1)  ! [1] methanol
  type(CPAdata), parameter :: CPAcx2 = CPAdata(cpaSRK,"C3"  ,  &
       9.11875E+5, 0.057834 , (/ 0.6307,0.0,0.0/),cbAlphaClassicIdx , 0.0   ,  0.0 ,     no_assoc,1)      ! [2] propane
  type(CPAdata), parameter :: CPAcx3 = CPAdata(cpaSRK,"NC4" , &
       13.14274E+5, 0.072081, (/ 0.70771,0.0,0.0/),cbAlphaClassicIdx, 0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-butane
  type(CPAdata), parameter :: CPAcx4 = CPAdata(cpaSRK,"NC5" , &
       18.198E+5, 0.091008, (/   0.79858,0.0,0.0/),cbAlphaClassicIdx, 0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-pentane
  type(CPAdata), parameter :: CPAcx5 = CPAdata(cpaSRK,"NC6" , &
       23.681E+5, 0.10789, (/   0.8313,0.0,0.0/),cbAlphaClassicIdx,  0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-hexane
  type(CPAdata), parameter :: CPAcx6 = CPAdata(cpaSRK,"NC7" , &
       29.178E+5, 0.12535, (/   0.9137,0.0,0.0/),cbAlphaClassicIdx,  0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-heptane
  type(CPAdata), parameter :: CPAcx7 = CPAdata(cpaSRK,"NC8" , &
       34.875E+5, 0.14244, (/   0.99415,0.0,0.0/),cbAlphaClassicIdx, 0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-octane
  type(CPAdata), parameter :: CPAcx8 = CPAdata(cpaSRK,"NC9" , &
       41.25061E+5, 0.16035, (/ 1.04628,0.0,0.0/),cbAlphaClassicIdx, 0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-nonane
  type(CPAdata), parameter :: CPAcx9 =CPAdata(cpaSRK,"NC10",&
       47.389E+5, 0.17865, (/   1.13243,0.0,0.0/),cbAlphaClassicIdx, 0.0   ,  0.0 ,     no_assoc,1)      ! [2] n-decane

  ! Queimada 2005, (10.1016/j.fluid.2004.08.011)
  type(CPAdata), parameter :: CPAcx10 =CPAdata(cpaSRK,"ETOH",&
       8.6716E+5, 0.049110, (/ 0.73690,0.0,0.0/),cbAlphaClassicIdx, 21.532E3,8E-3,assoc_scheme_2B,2)
  ! Oliveira 2008, doi:10.1016/j.fluid.2008.02.02
  type(CPAdata), parameter :: CPAcx11 =CPAdata(cpaSRK,"ETOH",&
       0.68415E+6, 4.7508E-2, (/0.93923 ,0.0,0.0/),cbAlphaClassicIdx, 21336 ,1.9212E-2, assoc_scheme_2B,1)
  ! Oliveira 2008, doi:10.1016/j.fluid.2008.02.02
  type(CPAdata), parameter :: CPAcx12 =CPAdata(cpaSRK,"PROP1OL",&
       1.1424E+6, 6.3788E-2, (/0.90134 ,0.0,0.0/),cbAlphaClassicIdx,21913,7.736E-3,assoc_scheme_2B,1)
  ! Oliveira 2008, doi:10.1016/j.fluid.2008.02.02
  type(CPAdata), parameter :: CPAcx13 =CPAdata(cpaSRK,"BUT1OL",&
       1.8019E+6, 8.1309E-2, (/0.98766,0.0,0.0/),cbAlphaClassicIdx,20069,3.6694E-3,assoc_scheme_2B,1)
  ! Oliveira 2008, doi:10.1016/j.fluid.2008.02.02
  type(CPAdata), parameter :: CPAcx14 =CPAdata(cpaSRK,"PENT1OL",&
       2.3552E+6, 9.7179E-2, (/1.0690,0.0,0.0/),cbAlphaClassicIdx,18666,2.6724E-3,assoc_scheme_2B,1)
  ! Oliveira 2008, doi:10.1016/j.fluid.2008.02.02
  type(CPAdata), parameter :: CPAcx15 =CPAdata(cpaSRK,"HEX1OL",&
       2.8386E+6, 11.313E-2, (/0.96959,0.0,0.0/),cbAlphaClassicIdx,22759,1.6727E-3,assoc_scheme_2B,1)

  type(CPAdata), parameter :: CPAcx16 =CPAdata(cpaSRK,"CO2",& ! [2]
       3.5079E5, 2.72E-02, (/0.7602,0.0,0.0/),cbAlphaClassicIdx, 0.0, 0.0, no_assoc, 1)
  type(CPAdata), parameter :: CPAcx17 =CPAdata(cpaSRK,"CO2",& ! [0] For modeling solvation; one electron acceptor and no electron donors.
       3.5079E5, 2.72E-02, (/0.7602,0.0,0.0/),cbAlphaClassicIdx,&
       0.0, 0.05, assoc_scheme_1ea, 2)
  type(CPAdata), parameter :: CPAcx18 =CPAdata(cpaSRK,"CO2",& ! [0]
       2.99377508e+05, 2.68729432e-02, (/4.62138711e-01,0.0,0.0/),&
       cbAlphaClassicIdx, 1.28604049e+04, 9.08545617e-03, assoc_scheme_1,3)

  type(CPAdata), parameter :: CPAcx19 = CPAdata(cpaSRK,"H2O", & ! [3] sCPA
       1.2277E+5, 1.4515e-02, (/0.67359,0.0,0.0/),&
       cbAlphaClassicIdx, 166.55e2, 69.2E-03, assoc_scheme_4C,1)
  type(CPAdata), parameter :: CPAcx20 = CPAdata(cpaSRK,"H2O", & ! [0]
       4.67542e+05, 1.57983e-02, (/7.76671e-01,0.,0./),cbAlphaClassicIdx,&
       4.44953e+03, 6.21918e-03,  assoc_scheme_2B,2)

  type(CPAdata), parameter :: CPAcx21 =CPAdata(cpaSRK,"NH3",&
       3.73160e+05, 2.07666e-02, (/ 7.17324e-1,0.0,0.0/),cbAlphaClassicIdx, 7.60835e+03,7.93725e-04,assoc_scheme_2B,1) ! [0] ammonia (AAD(P,liqv)=(0.759,0.912))

  integer, parameter :: nCPAmodels = 21
  type(CPAdata), dimension(nCPAmodels), parameter :: CPAarray = (/&
       CPAcx1, &
       CPAcx2, &
       CPAcx3, &
       CPAcx4, &
       CPAcx5, &
       CPAcx6, &
       CPAcx7, &
       CPAcx8, &
       CPAcx9, &
       CPAcx10, &
       CPAcx11, &
       CPAcx12, &
       CPAcx13, &
       CPAcx14, &
       CPAcx15, &
       CPAcx16, &
       CPAcx17, &
       CPAcx18, &
       CPAcx19, &
       CPAcx20, &
       CPAcx21/)

  !> TEMPERATURE-INDEPENDENT INTERACTION PARAMETERS for
  !> * the a parameter in the vdW mixing rules,
  !> * the eps parameter (can be modeled by arithmetic or geometric mean)
  !> * the beta parameter (can be modeled by arithmetic or geometric mean)
  !> Geometric mean is numbered 0, arithmetic mean is numbered 1.
  !> (It also depends on the scheme used for the two components, but we assume that
  !> each component only has one scheme stored in the database.)
  ! ------------------------------------------------------------------------------------------------
  type :: CPAkijdata
     sequence
     integer:: eosidx ! EoS identifier index
     character (len=8) :: uid1, uid2 ! Component names
     integer :: setno ! If several sets of kij in DB.
     real :: kij_a ! Binary interaction parameter for cubic a parameter

     ! Gives the combination models for eps and beta (in that order). 0 is
     ! geometric mean, 1 is arithmetic mean. For example, epsBetaCombRules=(/1,0/)
     ! mean that eps_ij=0.5*(eps_i+eps_j), beta_ij=sqrt(beta_i*beta_j).
     integer :: epsBetaCombRules(2)

     real :: kij_eps  ! kij for epsilon, e.g. eps_ij = (eps_i+eps_j)*(1-kij_eps)/2
     real :: kij_beta ! kij for beta, e.g. beta_ij = sqrt(beta_i*beta_j)*(1-kij_beta)
  end type CPAkijdata

  integer, parameter :: CPAmaxkij = 12
  type(CPAkijdata), dimension(CPAmaxkij), parameter :: CPAkijdb = (/ &
       CPAkijdata(cpaSRK,"C3","H2O",1,0.1135,(/ariComb,geoComb/),0.0,0.0), &     ! propane -- water
       CPAkijdata(cpaSRK,"C4","H2O",1,0.0875,(/ariComb,geoComb/),0.0,0.0), &     ! butane -- water
       CPAkijdata(cpaSRK,"C5","H2O",1,0.0615,(/ariComb,geoComb/),0.0,0.0), &     ! n-pentane -- water
       CPAkijdata(cpaSRK,"C6","H2O",1,0.0355,(/ariComb,geoComb/),0.0,0.0), &     ! n-hexane -- water
       CPAkijdata(cpaSRK,"C7","H2O",1,0.0095,(/ariComb,geoComb/),0.0,0.0), &     ! n-heptane -- water
       CPAkijdata(cpaSRK,"C8","H2O",1,-0.0165,(/ariComb,geoComb/),0.0,0.0), &    ! n-octane -- water
       CPAkijdata(cpaSRK,"C10","H2O",1,-0.0685,(/ariComb,geoComb/),0.0,0.0), &   ! n-decane -- water
       CPAkijdata(cpaSRK,"C3","MEOH",1,0.059,(/ariComb,geoComb/),0.0,0.0), &     ! methanol -- propane
       CPAkijdata(cpaSRK,"MEOH","H2O",1,-0.09,(/ariComb,geoComb/),0.0,0.0), &    ! methanol -- water
       CPAkijdata(cpaSRK,"ETOH","H2O",1,-0.11,(/ariComb,geoComb/),0.0,0.0), &     ! ethanol -- water
       CPAkijdata(cpaSRK,"CO2","H2O",1,0.04626056,(/ariComb,geoComb/),0.06022255,0), &      ! carbon dioxide -- water (regressed for temperatures from 278 K to 318 K, from 4 bar to 80 bar.
       CPAkijdata(cpaPR,"CO2","H2O",1,0.03,(/geoComb,geoComb/),0,0) & ! CPA-PR modeling uses geometric mean for both parameters. See e.g. (Tabasinejad et al., Ind. Eng. Chem. Res. 2012).
       /)


  integer, parameter :: maxinterGEij =  9
  type (interGEdatadb), dimension (maxinterGEij), parameter :: interGEdb = (/ &
       interGEdatadb("srk","huronvidal", 1,"H2O","NA+",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/-223.15,1573.0,340.0/),(/-223.15,1573.0,340.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"H2O","CL-",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/-223.15,1573.0,340.0/),(/-223.15,1573.0,340.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"MEOH","NA+",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/322.4,0.0,1.0/),(/322.4,0.0,1.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"MEOH","CL-",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/322.4,0.0,1.0/),(/322.4,0.0,1.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"NA+","CL-",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/0.0,0.0,1.0/),(/0.0,0.0,1.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"CO2","Na+",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/724.8,0.0,1.0/),(/724.8,0.0,1.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"CO2","CL-",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/724.8,0.0,1.0/),(/724.8,0.0,1.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"C1","Na+",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/1128.0,0.0,1.0/),(/1128.0,0.0,1.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       interGEdatadb("srk","huronvidal", 1,"C1","CL-",&
       0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       (/1128.0,0.0,1.0/),(/1128.0,0.0,1.0/)) & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       /)
       !   interGEdatadb("srk","huronvidal", 1,"H2O","NA+",&
       ! 0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       ! (/-33.66,564.8,340.0/),(/-33.66,564.8,340.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829
       ! interGEdatadb("srk","huronvidal", 1,"H2O","CL-",&
       ! 0.0,2,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
       ! (/-329.7,1008.2,340.0/),(/-329.7,1008.2,340.0/)), & ! Maribo-Mogensen (2015) - 10.1002/aic.14829

contains

  !> Get the index in the CPAarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getCPAdataIdx(eosidx,compName,setno) result(idx)
    use parameters, only: verbose
    integer, intent(in) :: eosidx,setno
    character(len=*), intent(in) :: compName
    integer :: idx
    logical :: found

    found = .false.
    idx = 1
    do while (idx <= nCPAmodels .and. .not. found)
       if ((eosidx==CPAarray(idx)%eosidx) .and. &
            trim(compName)==trim(CPAarray(idx)%compName) .and. &
            setno==CPAarray(idx)%setno) then
          found = .true.
       else
          idx = idx + 1
       endif
    enddo

    if (.not. found) then
      idx = 0
      if (verbose) then
        print *, "No CPA parameters for compName, setno ", compName, setno
      endif
    end if
  end function getCPAdataIdx

  !> Get information on if it is safe to initialize CPA
  !! ie. are there any self-associating components in the mix
  function mixHasSelfAssociatingComp(nc,eos,complist,setno) result(isAssoc)
    use stringmod, only: str_eq
    integer, intent(in) :: nc
    integer, dimension(nc), optional, intent(in) :: setno
    character(len=*), intent(in) :: eos
    character(len=*), dimension(nc), intent(in) :: compList
    logical :: isAssoc
    ! Locals
    integer :: i, eosidx, idx, setno_l(nc)
    !
    isAssoc = .false.

    if (str_eq(eos,'CPA-SRK')) then
      eosidx = cpaSRK
    elseif (str_eq(eos,'CPA-PR')) then
      eosidx = cpaPR
    else
      return
    endif
    ! Assign set number
    setno_l = 1
    if (present(setno)) then
      setno_l = setno
    endif

    do i=1,nc
      idx = getCPAdataIdx(eosidx,trim(compList(i)),setno_l(i))
      if (idx > 0) then
        if ( CPAarray(idx)%assoc_scheme /= no_assoc .AND. &
             CPAarray(idx)%assoc_scheme /= assoc_scheme_1ea) then
          isAssoc = .true.
          exit
        endif
      endif
    enddo
  end function mixHasSelfAssociatingComp

  subroutine getCpaPureParams_allcomps(nc,comp,eosidx,setno,&
       found,a0,b,alphaParams,eps,beta,alphaCorrIdx,scheme)
    use compdata, only: gendata
    ! Input
    type(gendata), intent(in) :: comp(nc)
    integer, intent(in) :: nc, eosidx, setno(nc)
    ! Output
    logical, intent(out) :: found(nc)
    integer, intent(out) :: alphaCorrIdx(nc), scheme(nc)
    real, intent(out) :: a0(nc), b(nc), alphaParams(3,nc), eps(nc), beta(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
       call getCpaPureParams_singleComp(comp(ic)%ident,eosidx,setno(ic),&
            found(ic),a0(ic),b(ic),alphaParams(:,ic),eps(ic),beta(ic),alphaCorrIdx(ic),scheme(ic))
    end do
  end subroutine getCpaPureParams_allcomps


  subroutine getCpaPureParams_singleComp(compName,eosidx,setno,&
       found,a0,b,alphaParams,eps,beta,alphaCorrIdx,scheme)
    ! Input
    character(len=*), intent(in) :: compName
    integer, intent(in) :: eosidx, setno
    ! Output
    logical, intent(out) :: found
    integer, intent(out) :: alphaCorrIdx, scheme
    real, intent(out) :: a0, b, alphaParams(3), eps, beta
    ! Locals
    integer :: idx

    idx = getCPAdataIdx(eosidx,compName,setno)
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
    use compdata, only: gendata
    ! Input
    integer, intent(in) :: nc
    type(gendata), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    ! Output
    real, intent(out) :: aEpsBeta_kij(3,nc,nc) ! kijs for the a, eps and beta comb rules
    integer, intent(out) :: epsbeta_combrules(2,nc,nc)
    ! Locals
    integer :: ic,jc,setno
    logical :: found_

    aEpsBeta_kij = -1000
    epsbeta_combrules = -1000
    setno = 1
    do ic=1,nc
       do jc=1,nc
          if (ic == jc) cycle
          aEpsBeta_kij(1,ic,jc) = getCPAkij_a(eosidx,comp(ic)%ident,comp(jc)%ident,found_)
          call getCPAkij_epsbeta(eosidx,comp(ic)%ident,comp(jc)%ident,setno,&
               found_,epsbeta_combrules(1:2,ic,jc),aEpsBeta_kij(2:3,ic,jc))
       end do
    end do
  end subroutine getCpaKijAndCombRules_allComps


  !> Retrieve association binary interaction parameter for components uid1 and
  !> uid2. Found is true if and only if the parameters is in the database. As
  !> of now this function sets interaction parameters to 0.0 if epsBetaCombRules
  !> is not exactly what is inputted.
  subroutine getCPAkij_epsbeta (eosidx,uid1,uid2,setno,found,epsBetaCombRules,kijepsbeta)
    use stringmod, only: str_eq
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    logical, intent(out) :: found
    integer, intent(out) :: epsBetaCombRules(2)
    real, intent(out) :: kijepsbeta(2)
    integer, intent(in), optional :: setno
    integer :: idx, setno_loc
    logical :: correct_eos, correct_comps, correct_setno

    setno_loc = 1
    if (present(setno)) setno_loc = setno

    kijepsbeta = 0.0
    idx = 1
    found = .false.
    do while (idx <= CPAmaxkij .and. (.not. found))
       !print *, "idx,uid1,uid2,setno_loc,CPAkijdb(idx)%eosidx",idx,uid1,uid2,setno_loc,CPAkijdb(idx)%eosidx
       correct_eos = (eosidx == CPAkijdb(idx)%eosidx)
       correct_comps = (str_eq(uid1,CPAkijdb(idx)%uid1) .and. str_eq(uid2,CPAkijdb(idx)%uid2)) &
            .or. ( str_eq(uid1,CPAkijdb(idx)%uid2) .and. str_eq(uid2,CPAkijdb(idx)%uid1))
       correct_setno = (CPAkijdb(idx)%setno == setno_loc)

       if ( correct_eos .and. correct_comps .and. correct_setno) then
          kijepsbeta(1) = CPAkijdb(idx)%kij_eps
          kijepsbeta(2) = CPAkijdb(idx)%kij_beta
          epsBetaCombRules = CPAkijdb(idx)%epsBetaCombRules
          return
       else
          idx = idx + 1
       endif
     enddo

     ! Default values.
     if (.not. found) then
       kijepsbeta = 0.0
       if (eosidx == cpaSRK) then
         epsBetaCombRules = (/aricomb,geocomb/)
       else if (eosidx == cpaPR) then
         epsBetaCombRules = (/geocomb,geocomb/)
       end if
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

    idx = 1
    found = .false.
    do while (idx <= CPAmaxkij .and. (.not. found))
       if ( eosidx == CPAkijdb(idx)%eosidx .and. &
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

  subroutine getCpaGEij(mGE, eosid, setno, uid1, uid2, &
       indxi, indxj, found)
    use stringmod, only: str_eq
    implicit none
    type (mixExcessGibbs), intent(inout) ::  mGE
    character(len=*), intent(in) :: eosid, uid1, uid2
    integer, intent(in) :: setno, indxi, indxj
    logical, intent(out) :: found
    ! Locals
    integer :: idx, i, j
    logical :: isUidMatch

    idx = 1
    found = .false.

    mGE%correlation(indxi, indxj) = 0
    mGE%correlation(indxj, indxi) = 0

    do idx=1,maxinterGEij
      isUidMatch = (str_eq(uid1,interGEdb(idx)%uid1) .AND. &
           str_eq(uid2,interGEdb(idx)%uid2)) .OR. &
           (str_eq(uid2,interGEdb(idx)%uid1) .AND. &
           str_eq(uid1,interGEdb(idx)%uid2))

      if ( isUidMatch .AND. str_eq (eosid,interGEdb(idx)%eosid) &
           .and. (setno == interGEdb(idx)%setno .OR. setno == 0) &
           .and. ( (str_eq ('huronvidal',interGEdb(idx)%mruleid) &
           .and. (mGE%mGE == cbMixHVCPA &
           .or. mGE%mGE == cbMixHVCPA2)))) then

        found = .true.
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

        if (mGE%mGE == cbMixHVCPA) then
          if (interGEdb(idx)%correlation == 2) then
            call stoperror('The Maribo-Mogensen correlation'//&
                 ' for component interaction require HV2')
          endif
          mGE%aGE(i,j) = interGEdb(idx)%polyij1(1)
          mGE%aGE(j,i) = interGEdb(idx)%polyji1(1)
          mGE%bGE(i,j) = interGEdb(idx)%polyij1(2)
          mGE%bGE(j,i) = interGEdb(idx)%polyji1(2)
        else
          mGE%aGE(i,j) = interGEdb(idx)%polyij2(1)
          mGE%aGE(j,i) = interGEdb(idx)%polyji2(1)
          mGE%bGE(i,j) = interGEdb(idx)%polyij2(2)
          mGE%bGE(j,i) = interGEdb(idx)%polyji2(2)
          mGE%cGE(i,j) = interGEdb(idx)%polyij2(3)
          mGE%cGE(j,i) = interGEdb(idx)%polyji2(3)
        endif
        exit ! Exit do loop
      endif
    enddo

  end subroutine getCpaGEij

end module CPA_parameters
