!> Interface to solid equations of state.
!! Currently only a Gibbs based equations of state for CO2 are supported.
!!
!! \author MH, 2013-03-05.
module solideos
  use thermopack_constants, only: Rgas, LIQPH, verbose, VAPPH, SINGLEPH
  use thermopack_var, only: nc, nph, thermo_model, &
       get_active_thermo_model, complist
  use co2_gibbs
  use h2o_gibbs
  !
  implicit none
  save
  !
  integer :: CO2GIBBSMODEL
  integer :: H2OGIBBSMODEL
  integer, allocatable, dimension(:) :: solidComp
  integer :: nSolid = 0
  !
  private
  public :: solid_thermo, solid_specificVolume, solid_enthalpy, solid_entropy
  public :: solid_init, nSolid, solidComp, solidForming, solidFraction
  public :: initIce, initDryIce, CO2GIBBSMODEL, H2OGIBBSMODEL
  public :: isFormingSolid
  !
contains

  !--------------------------------------------------------------------------
  !> Initialize solid model for given component
  !>
  !> \author MH, 2013-03-05
  !--------------------------------------------------------------------------
  subroutine solid_init(comp)
    use compdata, only: compIndex
    implicit none
    ! Transferred variables
    character(len=*), intent(in) :: comp !< Component name string
    !
    select case(trim(comp))
    case('CO2')
      CO2GIBBSMODEL = compIndex(complist, trim(comp))
      call addSolid(CO2GIBBSMODEL)
      call initDryIce()
    case('H2O')
      H2OGIBBSMODEL = compIndex(complist, trim(comp))
      call addSolid(H2OGIBBSMODEL)
      call initIce()
    case default
      call stoperror('solideos::solid_thermo' &
                     // ' - No solid model initialized for ' // trim(comp))
    endselect

  end subroutine solid_init

  !--------------------------------------------------------------------------
  !> Clear initialized solids
  !>
  !> \author MH, 2018-08
  !--------------------------------------------------------------------------
  subroutine clear_solids()
    implicit none
    !
    integer :: ierr
    if (allocated(solidComp)) then
      deallocate(solidComp,STAT=ierr)
      if (ierr /= 0) call stoperror('Could not deallocate ' &
           // 'solidComp in solideos::clear_solids')
    endif

    nSolid = 0

  end subroutine clear_solids

  !--------------------------------------------------------------------------
  !> Increase number of solids and add component index to solidComp vector
  !>
  !> \author MH, 2013-03-07
  !--------------------------------------------------------------------------
  subroutine addSolid(iComp)
    implicit none
    ! Transferred variables
    integer, intent(in)  :: iComp !< Component index
    ! Locals
    integer :: ierr
    integer, dimension(nSolid+1) :: solidCompLocal

    if (allocated(solidComp)) then

      ! Only initialize new components
      if (any(solidComp(:) == iComp)) return

      if (nSolid > 0) then
        solidCompLocal(1:nSolid) = solidComp
        deallocate(solidComp,STAT=ierr)
        if (ierr /= 0) call stoperror('Could not deallocate ' &
                    // 'solidComp in solideos::addSolid')
        allocate(solidComp(nSolid+1),STAT=ierr)
        if (ierr /= 0) call stoperror('Could not allocate ' &
                    // 'solidComp in solideos::addSolid')
        solidComp(1:nSolid) = solidCompLocal(1:nSolid)
      endif

    else

      allocate(solidComp(nSolid+1),STAT=ierr)
      if (ierr /= 0) call stoperror('Could not allocate ' &
                  // 'solidComp in solideos::addSolid')

    endif

    nSolid = nSolid + 1
    solidComp(nSolid) = iComp
  end subroutine addSolid

  !--------------------------------------------------------------------------
  !> Calculate solid fugasities etc. given composition, temperature and pressure
  !>
  !> \author MH, 2013-03-05
  !--------------------------------------------------------------------------
  subroutine solid_thermo(t,p,z,lnfug,lnfugt,lnfugp)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: lnfug !< Logarithm of solid fugasity coefficient
    real, optional, intent(out) :: lnfugt !< 1/K - Logarithm of solid fugasity coefficient differential wrpt. temperature
    real, optional, intent(out) :: lnfugp !< 1/Pa - Logarithm of solid fugasity coefficient differential wrpt. pressure
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    imax = maxloc(z)
    i = imax(1)

    call solidGibbs_fug(T,P,i,lnfug,lnfugt,lnfugp)

  end subroutine solid_thermo

  !--------------------------------------------------------------------------
  !> Calculate solid enthalpy given composition, temperature and pressure
  !> Unit: J/mol
  !>
  !> \author MH, 2013-03-06
  !--------------------------------------------------------------------------
  subroutine solid_enthalpy(t,p,z,h,dhdt,dhdp)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: h !< J/mol - Specifc enthalpy
    real, optional, intent(out) :: dhdt !< J/mol/K - Specifc enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdp !< J/mol/Pa - Specifc enthalpy differential wrpt. pressure
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    imax = maxloc(z)
    i = imax(1)

    if (i == CO2GIBBSMODEL) then
      h = sco2_enthalpy(T,P)
      if (present(dhdt)) then
        dhdt = sco2_heat_capacity(T,P)
      endif
      if (present(dhdp)) then
        dhdp = sco2_dgdp(T,P) -T*sco2_d2gdTdP(T,P)
      endif
    else if (i == H2OGIBBSMODEL) then
      h = sh2o_enthalpy(T,P)
      if (present(dhdt)) then
        dhdt = sh2o_heat_capacity(T,P)
      endif
      if (present(dhdp)) then
        dhdp = sh2o_dgdp(T,P) -T*sh2o_d2gdTdP(T,P)
      endif
    else
      call stoperror('solideos::solid_enthalpy - No solid model initialized for '//trim(complist(i)))
    endif

  end subroutine solid_enthalpy

  !-----------------------------------------------------------------------------
  !> Calculate solid entropy given composition, temperature and pressure
  !> Unit: J/mol/K
  !>
  !> \author MH, 2013-03-06
  !-----------------------------------------------------------------------------
  subroutine solid_entropy(t,p,z,s,dsdt,dsdp)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: s !< J/mol/K - Specifc entropy
    real, optional, intent(out) :: dsdt !< J/mol/K2 - Specifc entropy differential wrpt. temperature
    real, optional, intent(out) :: dsdp !< J/mol/K/Pa - Specifc entropy differential wrpt. pressure
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    imax = maxloc(z)
    i = imax(1)

    if (i == CO2GIBBSMODEL) then
      s = sco2_entropy(T,P)
      if (present(dsdt)) then
        dsdt = -sco2_d2gdT2(T,P)
      endif
      if (present(dsdp)) then
        dsdp = -sco2_d2gdTdP(T,P)
      endif
    else if (i == H2OGIBBSMODEL) then
      s = sh2o_entropy(T,P)
      if (present(dsdt)) then
        dsdt = -sh2o_d2gdT2(T,P)
      endif
      if (present(dsdp)) then
        dsdp = -sh2o_d2gdTdP(T,P)
      endif
    else
      call stoperror('solideos::solid_entropy - No solid model initialized for '//trim(complist(i)))
    endif

  end subroutine solid_entropy

  !-----------------------------------------------------------------------------
  !> Calculate solid specific volume given composition, temperature and pressure
  !> Unit: m3/mol
  !>
  !> \author MH, 2013-03-06
  !-----------------------------------------------------------------------------
  subroutine solid_specificVolume(t,p,z,v,dvdt,dvdp)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: v !< m3/mol - Specifc volume
    real, optional, intent(out) :: dvdt !< m3/mol/K - Specifc volume differential wrpt. temperature
    real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specifc volume differential wrpt. pressure
    ! Locals
    integer :: i
    integer, dimension(1) :: imax

    imax = maxloc(z)
    i = imax(1)

    if (i == CO2GIBBSMODEL) then
      v = sco2_dgdp(T,P)
      if (present(dvdt)) then
        dvdt = sco2_d2gdTdP(T,P)
      endif
      if (present(dvdp)) then
        dvdp = sco2_d2gdP2(T,P)
      endif
    else if (i == H2OGIBBSMODEL) then
      v = sh2o_dgdp(T,P)
      if (present(dvdt)) then
        dvdt = sh2o_d2gdTdP(T,P)
      endif
      if (present(dvdp)) then
        dvdp = sh2o_d2gdP2(T,P)
      endif
    else
      call stoperror('solideos::solid_specificVolume - No solid model initialized for '//trim(complist(i)))
    endif

  end subroutine solid_specificVolume

  !-----------------------------------------------------------------------------
  !> Calculate phase fraction of solid given current fugacities.
  !> If result is positive, the solid will be a stable phase.
  !>
  !> \author MH, 2013-03-07
  !-----------------------------------------------------------------------------
  function solidForming(T,P,j,Z,nd,BETA,IFUGAC,lnfugs) result(solidBeta)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    integer, intent(in) :: j !< Solid component index
    integer, intent(in) :: nd !< Number of phases
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, dimension(nd), intent(in) :: BETA !< Phase fractions
    real, dimension(nd,nc), intent(in) :: IFUGAC !< Inverse fugasity coefficient of exsisting phases
    real, intent(out) :: lnfugs !< - Logarithm of solid fugasity coefficient
    real :: solidBeta !< Solid phase fraction
    ! Locals
    integer :: i
    real :: fugs
    real, dimension(nc) :: Zsolid
    !
    Zsolid = 0.0
    Zsolid(j) = 1.0
    call solid_thermo(t,p,Zsolid,lnfugs)
    fugs = exp(lnfugs)
    solidBeta = z(j)
    do i=1,nd
      solidBeta = solidBeta - BETA(i)*fugs*IFUGAC(i,j)
    enddo
  end function solidForming

  !-------------------------------------------------------------------------
  !> Return logical flag that is true when a solid phase is stable
  !>
  !> \author MH, 2016, 03
  !-------------------------------------------------------------------------
  function isFormingSolid(nd,t,p,Z,beta,XX,phaseVec,betaSol) result(isForming)
    use eos, only: thermo
    implicit none
    real, dimension(nph), intent(inout) :: beta !< Phase molar fractions [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nph,nc), intent(inout) :: XX !< Phase molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    integer, dimension(nph), intent(in) :: phaseVec !< Phase identifiers
    integer, intent(in) :: nd !< Number of phases
    real, optional, intent(out) :: betaSol
    logical :: isForming
    ! Locals
    integer :: i, phase, is
    real :: lnfug(nc), fugs, IFUGAC(nd,nc), betaSol_l
    if (nd == 1) then
      XX(1,:) = Z
    endif
    do i=1,nd
      phase = phaseVec(i)
      if (phase == SINGLEPH) then
        phase = LIQPH
      endif
      call thermo(t,p,XX(i,:),phase,lnfug)
      IFUGAC(i,:) = 1.0/exp(lnfug)
    enddo
    is = solidComp(1)
    betaSol_l = solidForming(t,p,is,Z,nd,beta,IFUGAC,fugs)
    if (present(betaSol)) then
      betaSol = betaSol_l
    endif
    isForming = (betaSol_l > 0.0)
  end function isFormingSolid

  !-----------------------------------------------------------------------------
  !> Calculate phase fraction of solid given current fugacities.
  !>
  !> \author MH, 2013-08-21
  !-----------------------------------------------------------------------------
  function solidFraction(j,Z,nd,BETA,IFUGAC,fugs) result(solidBeta)
    implicit none
    integer, intent(in) :: j !< Solid component index
    integer, intent(in) :: nd !< Number of phases
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, dimension(nd), intent(in) :: BETA !< Phase fractions
    real, dimension(nd,nc), intent(in) :: IFUGAC !< Inverse fugasity coefficient of exsisting phases
    real, intent(in) :: fugs !< - Solid fugasity coefficient
    real :: solidBeta !< Solid phase fraction
    ! Locals
    integer :: i
    !
    solidBeta = z(j)
    do i=1,nd
      solidBeta = solidBeta - BETA(i)*fugs*IFUGAC(i,j)
    enddo
  end function solidFraction

  !--------------------------------------------------------------------------
  !> Calculate triple point from good initial guess
  !>
  !> \author MH, 2016-03
  !--------------------------------------------------------------------------
  subroutine calc_single_comp_triple_point(j,ttr,ptr)
    use nonlinear_solvers, only: nonlinear_solver,limit_dx,premReturn,setXv, &
         nonlinear_solve
    use thermopack_constants, only: tpPmax, tpPmin, tpTmin, tpTmax
    implicit none
    integer, intent(in) :: j !< Solid component index
    real, intent(inout) :: ttr !< Triple point temperature (K)
    real, intent(inout) :: ptr !< Triple point pressure (Pa)
    ! Locals
    real :: param(1), X(2), Xmin(2), Xmax(2)
    type(nonlinear_solver) :: solver
    param(1) = real(j)
    X(1) = log(ttr)
    X(2) = log(ptr)
    Xmin(1) = log(tpTmin)
    Xmax(1) = log(tpTmax)
    Xmin(2) = log(tpPmin)
    Xmax(2) = log(tpPmax)
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-12
    solver%limit_x_values = .true.
    solver%max_it = 200
    solver%ls_max_it = 3
    call nonlinear_solve(solver,sat_fun_newton_triple,sat_diff_newton_triple,&
         sat_diff_newton_triple,limit_dx,premReturn,setXv,X,Xmin,Xmax,param)
    if (solver%exitflag == 0) then
      ttr = exp(X(1))
      ptr = exp(X(2))
    endif
  end subroutine calc_single_comp_triple_point

  !--------------------------------------------------------------------------
  !> Triple point offset function
  !>
  !> \author MH, 2016-03
  !--------------------------------------------------------------------------
  subroutine sat_fun_newton_triple(Fun,Xvar,param)
    use eos, only: thermo
    implicit none
    real, dimension(2), intent(out) :: Fun !< Function values
    real, dimension(2), intent(in) :: Xvar !< Variable vector
    real, dimension(1) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, lnfugV, lnfugL
    integer :: s
    real :: p, t, lnfugs
    t = exp(Xvar(1))
    p = exp(Xvar(2))
    s = int(param(1))
    Z = 0.0
    Z(s) = 1.0
    call thermo(t,p,Z,VAPPH,lnfugV)
    call thermo(t,p,Z,LIQPH,lnfugL)
    call solid_thermo(t,p,Z,lnfugs)
    Fun(1) = lnfugV(s) - lnfugs
    Fun(2) = lnfugL(s) - lnfugs
  end subroutine sat_fun_newton_triple

  !--------------------------------------------------------------------------
  !> Triple point offset differentials
  !>
  !> \author MH, 2016-03
  !--------------------------------------------------------------------------
  subroutine sat_diff_newton_triple(Jac,Xvar,param)
    use eos, only: thermo
    implicit none
    real, dimension(2), intent(in) :: Xvar !< Variable vector
    real, dimension(2,2), intent(out) :: Jac !< Function differentials
    real, dimension(1) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: Z, lnfugV, lnfugL, lnfugtV, lnfugtL
    real, dimension(nc) :: lnfugpV, lnfugpL
    integer :: s
    real :: p, t, lnfugs,lnfugst,lnfugsp
    t = exp(Xvar(1))
    p = exp(Xvar(2))
    s = int(param(1))
    Z = 0.0
    Z(s) = 1.0

    call thermo(t,p,Z,VAPPH,lnfugV,lnfugt=lnfugtV,lnfugp=lnfugpV)
    call thermo(t,p,Z,LIQPH,lnfugL,lnfugt=lnfugtL,lnfugp=lnfugpL)
    call solid_thermo(t,p,Z,lnfugs,lnfugst,lnfugsp)

    Jac(1,1) = t*(lnfugtV(s) - lnfugst)
    Jac(1,2) = p*(lnfugpV(s) - lnfugsp)

    Jac(2,1) = t*(lnfugtL(s) - lnfugst)
    Jac(2,2) = p*(lnfugpL(s) - lnfugsp)

  end subroutine sat_diff_newton_triple

  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------
  ! Interface functions used to calculate solid fugasity
  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------

  !--------------------------------------------------------------------------
  !> Calculate fugacity coefficient and differentials for solids.
  !> Unit: -
  !>
  !> \author MH, 2013-03-01
  !--------------------------------------------------------------------------
  subroutine solidGibbs_fug(T,P,iSolid,lnfug,dlnfugdt,dlnfugdp)
    use eos, only: idealGibbsSingle
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    integer, intent(in) :: iSolid !< Solid component integer
    real, intent(out) :: lnfug !< - Fugasity coefficient
    real, optional, intent(out) :: dlnfugdt !< 1/K - Fugasity coefficient differential wrpt. temperature
    real, optional, intent(out) :: dlnfugdp !< 1/Pa - Fugasity coefficient differential wrpt. pressure
    ! Locals
    real :: gId,dgIddt,dgIddp
    real :: g,dgdt,dgdp
    logical :: limitedFugacity
    !
    ! Find reference Gibbs energy for fluid EoS
    call idealGibbsSingle(T,P,iSolid,gId,dgIddt,dgIddp)
    !
    if (iSolid == CO2GIBBSMODEL) then
      g = sco2_gibbs(T,P) ! Actual Gibbs energy
      if (present(dlnfugdt)) then
        dgdt = sco2_dgdt(T,P)
      endif
      if (present(dlnfugdp)) then
        dgdp = sco2_dgdp(T,P)
      endif
    else if (iSolid == H2OGIBBSMODEL) then
      g = sh2o_gibbs(T,P) ! Actual Gibbs energy
    if (present(dlnfugdt)) then
        dgdt = sh2o_dgdt(T,P)
      endif
      if (present(dlnfugdp)) then
        dgdp = sh2o_dgdp(T,P)
      endif
      else
      call stoperror('solideos::solidGibbs_fug - No solid model initialized for '//trim(complist(iSolid)))
    endif
    lnfug = (g-gId)/(Rgas*T)
    limitedFugacity = .false.
    if (lnfug < -500.0) then
      lnfug = -500.0
      limitedFugacity = .true.
    else if (lnfug > 500.0) then
      lnfug = 500.0
      limitedFugacity = .true.
    endif
    if (present(dlnfugdt)) then
      if (limitedFugacity) then
        dlnfugdt = 0.0
      else
        dlnfugdt = (dgdt-dgIddt)/(Rgas*T) - lnfug/T
      endif
    endif
    if (present(dlnfugdp)) then
      if (limitedFugacity) then
        dlnfugdp = 0.0
      else
        dlnfugdp = (dgdp-dgIddp)/(Rgas*T)
      endif
    endif
  end subroutine solidGibbs_fug

  !--------------------------------------------------------------------------
  !> Init water ice model
  !! Option to use Ice model without checking for ice in multiphase flash
  !! \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine initIce()
    use eos, only: enthalpy, entropy
    use compdata, only: compIndex
    implicit none
    ! Locals
    integer :: iH2O
    real :: x(nc), sl, hl, gl
    iH2O = compIndex(complist, trim("H2O"))
    H2OGIBBSMODEL = iH2O
    x = 0.0
    x(iH2O) = 1.0
    call entropy(T0_H2O,P0_H2O,x,LIQPH,sl)
    call enthalpy(T0_H2O,P0_H2O,x,LIQPH,hl)
    gl = hl - T0_H2O*sl
    call sho2_init(sl, gl)
  end subroutine initIce

  !--------------------------------------------------------------------------
  !> Init dry ice model
  !! Option to use dry ice model without checking for dry ice
  !! in multiphase flash
  !! \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine initDryIce()
    use saturation, only: bubP
    use eos, only: enthalpy, entropy
    use compdata, only: compIndex
    implicit none
    ! Locals
    integer :: iCO2
    real :: sl_tr, gl_tr, T_tr, P_tr, hl_tr
    real, dimension(nc) :: x,y
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()
    T_tr = 216.592
    P_tr = 5.1795e5
    iCO2 = compIndex(complist, "CO2")
    CO2GIBBSMODEL = iCO2
    x = 0.0
    x(iCO2) = 1.0
    y = x
    ! Make pure component sublimation line meet saturation line in triple
    ! point. That is; triple point pressure modified.
    P_tr = bubP(T_tr,P_tr,x,y)
    call entropy(T_tr,P_tr,x,LIQPH,sl_tr)
    call enthalpy(T_tr,P_tr,x,LIQPH,hl_tr)
    gl_tr = hl_tr - T_tr*sl_tr
    call sco2init(sl_tr, gl_tr, T_tr, P_tr)
    ! Locate exact triple point and store result
    call calc_single_comp_triple_point(CO2GIBBSMODEL, T_tr, P_tr)
    act_mod_ptr%comps(CO2GIBBSMODEL)%p_comp%ttr = T_tr
    act_mod_ptr%comps(CO2GIBBSMODEL)%p_comp%ptr = P_tr
    if (verbose) then
      print *,'Triple point of CO2 after combining models'
      print *,'Temperature (K): ',t_tr
      print *,'Pressure (bar):  ',p_tr*1.0e-5
    endif
  end subroutine initDryIce


end module solideos
