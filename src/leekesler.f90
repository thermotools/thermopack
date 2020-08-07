!----------------------------------------------------------------------
!> This module solves the Lee-Kesler EoS. Contains all partial derivative functions needed to do consistency check.
!>
!> \author JA, June - July 2013
!----------------------------------------------------------------------

! To be able to compile doxygen documentation correctly, the following LaTeX the
! amsmath package must be included in the configuration file.
! I belive the package can be added by:
!
! EXTRA_PACKAGES = amsmath
!


module LeeKesler
  use thermopack_constants, only: Rgas
  implicit none

  real, parameter :: eta = 0.25
  real, parameter :: wRef = 0.3978
  integer, parameter :: rootSelection = 3 !< 1: No phase root analysis
                                          !< 2: Return minima if no root exsist
                                          !< 3: If the Simple/Reference phase don't
                                          !<    have a root, but have a minima
                                          !<    switch to other phase
  real, parameter :: vrMinimum = 0.05
  real, parameter :: vrMaximum = 1E10

  !----------------------------------------------------------------------
  !> The constants b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, beta and gamma
  !> have two distinct values each. Whether the first or second stored value
  !> for the constants are used, is determined by whether the calculations
  !> are done for a simple fluid, or a reference fluid.
  !> 1=simple fluid, 2=reference fluid
  !----------------------------------------------------------------------

  real, parameter, dimension(2) :: b1 = (/ 0.1181193, 0.2026579 /), b2 = (/ 0.265728, 0.331511 /), &
  b3 = (/ 0.154790, 0.027655 /), b4 = (/ 0.030323, 0.203488 /)
  real, parameter, dimension(2) :: c1 = (/ 0.0236744, 0.0313385 /), c2 = (/ 0.0186984, 0.0503618 /), &
  c3 = (/ 0.0, 0.016901 /), c4 = (/ 0.042724,  0.041577/)
  real, parameter, dimension(2) :: d1 = (/ 0.0000155488, 0.000048736 /), d2 = (/ 0.0000623689,  0.00000740336/)
  real, parameter, dimension(2) :: beta = (/ 0.65392, 1.226 /), gamma = (/ 0.060167, 0.03754 /)

  public :: mainLeeKesler, testDiffLeeKesler
  public :: lkCalcFug, lkCalcZfac, lkCalcEntropy, lkCalcEnthalpy
  public :: lkCalcGdep

  contains

  !----------------------------------------------------------------------
  !> Main subroutine of the Lee Kesler eos. Takes temperature, pressure,
  !> composition and phase as input, and returns comrpessibility, entropy departure,
  !> enthalpy departure and fugacity coefficients.
  !> Calls upon routine thermProps to calculate these values for simple fluid (0)
  !> and reference fluid (1) seperatly, with reduced values for temperature and
  !> pressure calculated by the mixing rules, and combines the results according to
  !> the Lee Kesler relation:
  !>
  !> \f[
  !>   M = M^{(0)} + \frac{\omega_M}{\omega^{(r)}}(M^{(r)} - M^{(0)})
  !> \f]
  !>
  !> where M is an arbitrary thermodynamic property.
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  subroutine mainLeeKesler(nc,comp,cbeos,T,P,nMoles,phase,z,Sdep,Hdep,lnphi)!,z0,z1,Sdep0,Sdep1,Hdep0,Hdep1)
    use eosdata
    use cubic_eos, only: cb_eos
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in)    :: T, P, nMoles(nc)
    integer, intent(in) :: phase
    real, intent(out)   :: z, Sdep, Hdep, lnphi(nc)

    real :: Pr, Tr
    real :: zSimp, SSimp, HSimp, lnphiSimp(nc)
    real :: zRef, SRef, HRef, lnphiRef(nc)
    real :: TcM, vcM, PcM, zcM, wM
    real :: moles
    real :: vrSimp, vrRef
    integer :: usedPhase
    logical :: corPhaseSimp, corPhaseRef
    !Functions called: mixRules, thermProps, vrNewtRaps

    moles = sum(nMoles)
    usedPhase = phase      !decleared as new variable, to avoid changing input phase, which may result in error in later runs
    call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles)

    Tr = T/TcM
    Pr = P/PcM
    !write (*,*) 'Tr = ', Tr
    !write (*,*) 'Pr = ', Pr

    ! Tr =   0.99736043376134298
    ! Pr =   0.98089802643643742
    ! call fixedTrPlot(0.05,1.0,Tr,'lk_tr_0.99.dat')
    ! print *,'Done plotting Pr=Pr(Tr,vr)'
    ! call exit(1)

    ! Tr = 1.0
    ! call fixedTrPlot(0.05,1.0,Tr,'lk_tr_1.0.dat')
    ! Tr = 0.99
    ! call fixedTrPlot(0.05,1.0,Tr,'lk_tr_0.99.dat')
    ! print *,'Done plotting Pr=Pr(Tr,vr)'
    ! call exit(1)
    !call calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)
    !return
    call vrNewtRaps(Tr,Pr,usedPhase,1,vrSimp,corPhaseSimp)

    !----------------------------------------------------------------------
    ! Due to the risk of simple and reference fluid being in different phases for
    ! for the given temperature, pressure and composition, the volume is calculated
    ! with a phase check, to ensures that both fluids are in the same, physically
    ! valid, phase.
    !----------------------------------------------------------------------

    call vrNewtRaps(Tr,Pr,usedPhase,1,vrSimp,corPhaseSimp)
    call vrNewtRaps(Tr,Pr,usedPhase,2,vrRef,corPhaseRef)
    if ((.not. corPhaseSimp) .or. (.not. corPhaseRef)) then
      usedPhase = abs(usedPhase-3)       !phase changed for both simple and reference fluid if one or both are in invalid phase
      print *, 'Invalid phase, phase set to:'
      if (usedPhase == 1) print *, 'Liquid'
      if (usedPhase == 2) print *, 'Gas'
      call vrNewtRaps(Tr,Pr,usedPhase,1,vrSimp,corPhaseSimp)
      call vrNewtRaps(Tr,Pr,usedPhase,2,vrRef,corPhaseRef)
      if ((.not. corPhaseSimp) .or. (.not. corPhaseRef)) then
        print *, 'Crudely approximated value for volume used in further calculations.'
      end if
    end if


    call thermProps(nc,comp,cbeos,Tr,Pr,vrSimp,nMoles,TcM,vcM,PcM,zcM,wM,moles,zSimp,SSimp,HSimp,lnphiSimp,1)  !> Call routine with parameter 2, calculates for simple fluid.
    call thermProps(nc,comp,cbeos,Tr,Pr,vrRef,nMoles,TcM,vcM,PcM,zcM,wM,moles,zRef,SRef,HRef,lnphiRef,2)      !> Call routine with parameter 1, calculates for reference fluid.

    z = zSimp + (wM/wRef)*(zRef - zSimp)
    Sdep = 1.0/Rgas * (SSimp + (wM/wRef)*(SRef - SSimp))
    Hdep = (1.0/(Rgas*TcM))*(HSimp + (wM/wRef)*(HRef - HSimp))
    lnphi = lnphiSimp + (wM/wRef)*(lnphiRef - lnphiSimp)

    write (*,12) 'fugCoef =', exp(lnphi)
    12 format(A10, 2f13.5)
    print *, '---------------------------'

  end subroutine mainLeeKesler

  !----------------------------------------------------------------------
  !> This function calculates the Fugacity coefficient and the derivatives.
  !!
  !! \author Morten H.
  subroutine lkCalcFug(nc,comp,cbeos,T,p,z,phase,lnfug,dlnfdt,dlnfdp,dlnfdz,v)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< The overall mole fraction [-]
    integer, intent(in) :: phase !< Phase, 1=liquid, 2=vapour
    real, dimension(nc), intent(out) :: lnfug !< The fugacity coefficients [-]
    real, optional, dimension(nc), intent(out) :: dlnfdt !< Temperature derivative [1/K] (dln(f)/dT)
    real, optional, dimension(nc), intent(out) :: dlnfdp !< Pressure derivative [1/Pa]   (dln(f)/dP)
    real, optional, dimension(nc,nc), intent(out) :: dlnfdz !< Composition derivative [1/kmole] (dln(f)/dNi)
    real, optional, intent(out) :: v !< Specific volume [mol/m3]
    ! Locals
    real, dimension(nc) :: lnphiSimp, lnphiRef, dlnphiSimp, dlnphiRef, dwMdNi, dwMdNidNj
    real :: moles,TcM,vcM,PcM,zcM,wM,Pr,Tr
    real :: Bsimp,Csimp,Dsimp,Esimp,B,C,D,E,lnPhiMref,lnPhiMsimp
    real :: dlnPhiMrefdt,dlnPhiMrefdp,dlnPhiMsimpdt,dlnPhiMsimpdp
    real :: vrRef, vrSimp, Zfac_ref, lgZ_ref, Zfac_simp, lgZ_simp, Zfac
    integer :: i,j
    moles = sum(z)
    call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,z,moles)
    Tr = T/TcM
    Pr = P/PcM
    call calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)

    ! Find solution for reference fluid
    call TrCoeff(Tr,B,C,D,E,2)
    lnPhiMref = lnPhiM(T,P,Tr,Pr,vrRef,moles,B,C,D,E,2,dlnPhiMrefdt,&
         dlnPhiMrefdp)
    Zfac_ref = Pr*vrRef/Tr
    lgZ_ref = log(Zfac_ref)
    do i = 1,nc
      lnphiRef(i) = FDiffNi(nc,comp,cbeos,Tr,vrRef,TcM,vcM,PcM,zcM,wM,z,&
           moles,i,B,C,D,E,2) - lgZ_ref
    end do

    ! Find solution for simple fluid
    call TrCoeff(Tr,Bsimp,Csimp,Dsimp,Esimp,1)
    lnPhiMsimp = lnPhiM(T,P,Tr,Pr,vrSimp,moles,Bsimp,Csimp,Dsimp,Esimp,1,&
         dlnPhiMsimpdt,dlnPhiMsimpdp)
    Zfac_simp = Pr*vrSimp/Tr
    lgZ_simp = log(Zfac_simp)
    do i = 1,nc
      lnphiSimp(i) = FDiffNi(nc,comp,cbeos,Tr,vrSimp,TcM,vcM,PcM,zcM,wM,z,&
           moles,i,Bsimp,Csimp,Dsimp,Esimp,1) - lgZ_simp
    end do

    do i = 1,nc
      dwMdNi(i) = wMDiffNi(nc,comp,moles,wM,i)
    enddo
    ! Set fugacity coefficients
    lnfug = lnphiSimp + (wM/wRef)*(lnphiRef - lnphiSimp) &
         + (dwMdNi/wRef)*(lnPhiMref - lnPhiMsimp)

    if (present(v)) then
      Zfac = Zfac_simp + (wM/wRef)*(Zfac_ref - Zfac_simp)
      v = Zfac*Rgas*T/P
    endif

    ! Temperature differentials
    if (present(dlnfdt)) then
      do i = 1,nc
        dlnphiSimp(i) = lnphiDiffT(nc,comp,cbeos,T,P,moles,Tr,vrSimp,&
             TcM,vcM,PcM,zcM,wM,Bsimp,Csimp,Dsimp,Esimp,z,i,1)
        dlnphiRef(i) = lnphiDiffT(nc,comp,cbeos,T,P,moles,Tr,vrRef,&
             TcM,vcM,PcM,zcM,wM,B,C,D,E,z,i,2)
      end do
      dlnfdt = dlnphiSimp + (wM/wRef)*(dlnphiRef - dlnphiSimp) &
           + (dwMdNi/wRef)*(dlnPhiMrefdt - dlnPhiMsimpdt)
    endif

    ! Pressure differentials
    if (present(dlnfdp)) then
      do i = 1,nc
        dlnphiSimp(i) = lnphiDiffP(nc,comp,cbeos,T,P,moles,Tr,vrSimp,&
             TcM,vcM,PcM,zcM,wM,Bsimp,Csimp,Dsimp,Esimp,z,i,1)
        dlnphiRef(i) = lnphiDiffP(nc,comp,cbeos,T,P,moles,Tr,vrRef,&
             TcM,vcM,PcM,zcM,wM,B,C,D,E,z,i,2)
      end do
      dlnfdp = dlnphiSimp + (wM/wRef)*(dlnphiRef - dlnphiSimp) &
           + (dwMdNi/wRef)*(dlnPhiMrefdp - dlnPhiMsimpdp)
    endif

    ! Compositional differentials
    if (present(dlnfdz)) then
      do i = 1,nc
        do j = 1,nc
          dlnphiSimp(j) = lnphiDiffNj(nc,comp,cbeos,moles,Tr,vrSimp,&
               TcM,vcM,PcM,zcM,wM,Bsimp,Csimp,Dsimp,Esimp,z,i,j,1)
          dlnphiRef(j) = lnphiDiffNj(nc,comp,cbeos,moles,Tr,vrRef,&
               TcM,vcM,PcM,zcM,wM,B,C,D,E,z,i,j,2)
          dwMdNidNj(j) = wMDiff2NiNj(nc,comp,moles,wM,i,j)
        end do
        dlnfdz(i,:) = dlnphiSimp + (wM/wRef)*(dlnphiRef - dlnphiSimp) + &
             (dwMdNi/wRef)*(lnphiRef(i) - lnphiSimp(i)) + &
             (dwMdNi(i)/wRef)*(lnphiRef - lnphiSimp) + &
             (dwMdNidNj/wRef)*(lnPhiMref - lnPhiMsimp)
      enddo
    endif

  end subroutine lkCalcFug

  !----------------------------------------------------------------------
  !> This function calculates reduced deparure Gibbs energy, or
  !! the mixture fugacity coefficient. That is;
  !! equation 21 in chapter 2 of Michelsen book:
  !!
  !! \f[
  !!   \frac{G(T,P,\textbf{n})}{R T} =  F(T,V,\textbf{n}) + n\left(\frac{PV}{R T} - 1 - \ln(z)\right)
  !! \f]
  !!
  !! \author Morten H.
  function lnPhiM(T,P,Tr,Pr,vr,moles,B,C,D,E,simpOrRef,dlnphidt,dlnphidp) result(lnphi)
    implicit none
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(in) :: Tr !< Reduced temperature [-]
    real, intent(in) :: Pr !< Reduced pressure [-]
    real, intent(in) :: vr !< Reduced volume [-]
    real, intent(in) :: moles !< Total number of moles [mol]
    real, intent(in) :: B,C,D,E
    real :: lnphi !< The mixture fugacity coefficients [-]
    integer, intent(in) :: simpOrRef !< Simple (1) or reference (2) fluid
    real, optional, intent(out) :: dlnphidt,dlnphidp
    ! Locals
    real :: F,zFac,H_RT
    zFac = vr*Pr/Tr
    F = FSolver(moles,vr,B,C,D,E,simpOrRef)
    lnphi = moles*(zFac - 1.0 - log(zFac)) + F
    if (present(dlnphidt)) then
      H_RT = -Tr*FDiffTr(moles,Tr,vr,simpOrRef) + moles*(zFac-1)
      dlnphidt = -H_RT/T
    endif
    if (present(dlnphidp)) then
      dlnphidp = moles*(zFac-1.0)/P
    endif
  end function lnPhiM

  !----------------------------------------------------------------------
  !> This function calculates reduced deparure Gibbs energy.
  !!
  !! \author Morten H.
  subroutine lkCalcGdep(nc,comp,cbeos,T,P,nMoles,phase,g,dgdt,dgdp)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, dimension(nc), intent(in) :: nMoles !< Number of moles per component [mol]
    integer, intent(in) :: phase !< Phase integer
    real,intent(out) :: g !< The departure gibbs energy [J/mol]
    real, optional, intent(out) :: dgdt,dgdp
    ! Locals
    real :: B,C,D,E,Tr,Pr,vrSimp,vrRef
    real :: moles,TcM,vcM,PcM,zcM,wM
    real :: lnPhiMsimp,lnPhiMref
    real :: dlnPhiMrefdt,dlnPhiMrefdp,dlnPhiMsimpdt,dlnPhiMsimpdp

    moles = sum(nMoles)
    call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles)
    Tr = T/TcM
    Pr = P/PcM
    call calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)

    ! Find reference fluid gibbs energy
    call TrCoeff(Tr,B,C,D,E,2)
    lnPhiMref = lnPhiM(T,P,Tr,Pr,vrRef,moles,B,C,D,E,2,dgdt,dgdp)
    if (present(dgdt)) then
      dlnPhiMrefdt = dgdt
    endif
    if (present(dgdp)) then
      dlnPhiMrefdp = dgdp
    endif

    ! Find simple fluid gibbs energy
    call TrCoeff(Tr,B,C,D,E,1)
    lnPhiMsimp = lnPhiM(T,P,Tr,Pr,vrSimp,moles,B,C,D,E,1,dgdt,dgdp)
    if (present(dgdt)) then
      dlnPhiMsimpdt = dgdt
    endif
    if (present(dgdp)) then
      dlnPhiMsimpdp = dgdp
    endif

    ! Gibbs free energy
    g = Rgas*T*(lnphiMsimp + (wM/wRef)*(lnPhiMref - lnPhiMsimp))
    if (present(dgdt)) then
      dgdt = Rgas*T*(dlnphiMsimpdt + (wM/wRef)*(dlnPhiMrefdt - dlnPhiMsimpdt)) + g/T
    endif
    if (present(dgdp)) then
      dgdp = Rgas*T*(dlnphiMsimpdp + (wM/wRef)*(dlnPhiMrefdp - dlnPhiMsimpdp))
    endif
  end subroutine lkCalcGdep

  !---------------------------------------------------------------------- >
  !> This function calculates the compressibillity factor and the derivatives.
  !!
  !! \author Morten H.
  subroutine lkCalcZfac(nc,comp,cbeos,T,p,z,phase,Zfac,dZdt,dZdp,dZdz)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< The overall mole fraction [-]
    integer, intent(in) :: phase !< Phase, 1=liquid, 2=vapour
    real, intent(out) :: Zfac !< The compressibilltiy factor [-]
    real, optional, intent(out) :: dZdt !< Temperature derivative [1/K]
    real, optional, intent(out) :: dZdp !< Pressure derivative [1/Pa]
    real, optional, dimension(nc), intent(out) :: dZdz !< Composition derivative [-]
    ! Locals
    real :: zSimp, zRef, dzSimp, dzRef
    real, dimension(nc) :: dZSimpdz, dZRefdz, dwMdNi
    real :: moles,TcM,vcM,PcM,zcM,wM
    real :: Bsimp,Csimp,Dsimp,Esimp,B,C,D,E
    real :: vrRef, vrSimp, Pr, Tr
    integer :: i
    moles = sum(z)
    call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,z,moles)
    Tr = T/TcM
    Pr = P/PcM
    call calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)

    ! Find solution for reference fluid
    call TrCoeff(Tr,B,C,D,E,2)
    zRef = Pr*vrRef/Tr

    ! Find solution for simple fluid
    call TrCoeff(Tr,Bsimp,Csimp,Dsimp,Esimp,1)
    zSimp = Pr*vrSimp/Tr

    ! Set compressibillity factor
    Zfac = zSimp + (wM/wRef)*(zRef - zSimp)

    ! Temperature differential
    if (present(dZdt)) then
      dzSimp = zDiffT(zSimp,T,P,moles,Tr,vrSimp,TcM,PcM,Bsimp,Csimp,Dsimp,Esimp,1)
      dzRef = zDiffT(zRef,T,P,moles,Tr,vrRef,TcM,PcM,B,C,D,E,2)
      dZdt = dzSimp + (wM/wRef)*(dzRef - dzSimp)
    endif
    ! Pressure differential
    if (present(dZdp)) then
      dzSimp = zDiffP(zSimp,P,moles,Tr,vrSimp,TcM,PcM,Bsimp,Csimp,Dsimp,Esimp,1)
      dzRef = zDiffP(zRef,P,moles,Tr,vrRef,TcM,PcM,B,C,D,E,2)
      dZdp = dzSimp + (wM/wRef)*(dzRef - dzSimp)
    endif
    ! Compozition differentials
    if (present(dZdz)) then
      do i = 1,nc
        dwMdNi(i) = wMDiffNi(nc,comp,moles,wM,i)
        dZSimpdz(i) = zDiffNi(nc,comp,cbeos,zSimp,moles,Tr,vrSimp,TcM,vcM,PcM,zcM,wM,&
             Bsimp,Csimp,Dsimp,Esimp,z,i,1)
        dZRefdz(i) = zDiffNi(nc,comp,cbeos,zRef,moles,Tr,vrRef,TcM,vcM,PcM,zcM,wM,&
             B,C,D,E,z,i,2)
      enddo
      dZdz = dZSimpdz + (wM/wRef)*(dZRefdz - dZSimpdz) + (dwMdNi/wRef)*(zRef - zSimp)
    endif
  end subroutine lkCalcZfac

  !---------------------------------------------------------------------- >
  !> This function calculates the residual entropy and the derivatives.
  !!
  !! \author Morten H.
  subroutine lkCalcEntropy(nc,comp,cbeos,T,p,Z,phase,entropy,dsdt,dsdp,dsdz)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< The overall mole fraction [-]
    integer, intent(in) :: phase !< Phase, 1=liquid, 2=vapour
    real, intent(out) :: entropy !< The residual entropy [J/mol/K]
    real, optional, intent(out) :: dsdt !< Temperature derivative [J/mol/K^2]
    real, optional, intent(out) :: dsdp !< Pressure derivative [J/mol/K/Pa]
    real, optional, dimension(nc), intent(out) :: dsdz !< Composition derivative [J/mol/K]
    ! Locals
    real :: sSimp, sRef, dsSimp, dsRef
    real, dimension(nc) :: dsSimpdz, dsRefdz, dwMdNi
    real :: moles,TcM,vcM,PcM,zcM,wM
    real :: Bsimp,Csimp,Dsimp,Esimp,B,C,D,E
    real :: vrRef, vrSimp, Pr, Tr, FRef, FSimp, S_VDep_Ref, S_VDep_Simp
    real :: zRef, lgZ_Ref, zSimp, lgZ_Simp
    integer :: i
    moles = sum(z)
    call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,z,moles)
    Tr = T/TcM
    Pr = P/PcM
    call calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)

    ! Find solution for reference fluid
    call TrCoeff(Tr,B,C,D,E,2)
    FRef = FSolver(moles,vrRef,B,C,D,E,2)
    S_VDep_Ref = -Rgas*(FRef + Tr*FDiffTr(moles,Tr,vrRef,2))
    zRef = Pr*vrRef/Tr
    lgZ_Ref = log(zRef)
    sRef = S_VDep_Ref + moles*Rgas*lgZ_Ref

    ! Find solution for simple fluid
    call TrCoeff(Tr,Bsimp,Csimp,Dsimp,Esimp,1)
    FSimp = FSolver(moles,vrSimp,Bsimp,Csimp,Dsimp,Esimp,1)
    S_VDep_Simp = -Rgas*(FSimp + Tr*FDiffTr(moles,Tr,vrSimp,1))
    zSimp = Pr*vrSimp/Tr
    lgZ_Simp = log(zSimp)
    sSimp = S_VDep_Simp + moles*Rgas*lgZ_Simp

    ! Set entropy
    entropy = sSimp + (wM/wRef)*(sRef - sSimp)

    ! Temperature differential
    if (present(dsdt)) then
      dsSimp = SDiffT(T,P,moles,Tr,vrSimp,TcM,PcM,Bsimp,Csimp,Dsimp,Esimp,1)
      dsRef = SDiffT(T,P,moles,Tr,vrRef,TcM,PcM,B,C,D,E,2)
      dsdt = dsSimp + (wM/wRef)*(dsRef - dsSimp)
    endif
    ! Pressure differential
    if (present(dsdp)) then
      dsSimp = SDiffP(T,P,moles,Tr,vrSimp,TcM,PcM,Bsimp,Csimp,Dsimp,Esimp,1)
      dsRef = SDiffP(T,P,moles,Tr,vrRef,TcM,PcM,B,C,D,E,2)
      dsdp = dsSimp + (wM/wRef)*(dsRef - dsSimp)
    endif
    ! Compozition differentials
    if (present(dsdz)) then
      do i=1,nc
        dwMdNi(i) = wMDiffNi(nc,comp,moles,wM,i)
        dsSimpdz(i) = SDiffNi(nc,comp,cbeos,T,P,moles,Tr,vrSimp,TcM,vcM,PcM,zcM,wM,&
             Bsimp,Csimp,Dsimp,Esimp,z,i,zSimp,1)
        dsRefdz(i) = SDiffNi(nc,comp,cbeos,T,P,moles,Tr,vrRef,TcM,vcM,PcM,zcM,wM,B,C,D,E,z,i,zRef,2)
      enddo
        dsdz = dsSimpdz + (wM/wRef)*(dsRefdz - dsSimpdz) + (dwMdNi/wRef)*(sRef - sSimp)
    endif
  end subroutine lkCalcEntropy

  !---------------------------------------------------------------------- >
  !> This function calculates the residual enthalpy and the derivatives.
  !!
  !! \author Morten H.
  subroutine lkCalcEnthalpy(nc,comp,cbeos,T,p,Z,phase,enthalpy,dhdt,dhdp,dhdz)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< The overall mole fraction [-]
    integer, intent(in) :: phase !< Phase, 1=liquid, 2=vapour
    real, intent(out) :: enthalpy !< The residual enthalpy [J/mol]
    real, optional, intent(out) :: dhdt !< Temperature derivative [J/mol/K]
    real, optional, intent(out) :: dhdp !< Pressure derivative [J/mol/Pa]
    real, optional, dimension(nc), intent(out) :: dhdz !< Composition derivative [J/mol]
    ! Locals
    real :: hSimp, hRef, dhSimp, dhRef
    real, dimension(nc) :: dhSimpdz, dhRefdz, dwMdNi
    real :: moles,TcM,vcM,PcM,zcM,wM
    real :: Bsimp,Csimp,Dsimp,Esimp,B,C,D,E
    real :: vrRef, vrSimp, Pr, Tr
    integer :: i
    moles = sum(z)
    call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,z,moles)
    Tr = T/TcM
    Pr = P/PcM
    call calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)

    ! Find solution for reference fluid
    call TrCoeff(Tr,B,C,D,E,2)
    hRef = Rgas*T*(-Tr*FDiffTr(moles,Tr,vrRef,2) + moles*(vrRef*Pr/Tr-1))

    ! Find solution for simple fluid
    call TrCoeff(Tr,Bsimp,Csimp,Dsimp,Esimp,1)
    hSimp = Rgas*T*(-Tr*FDiffTr(moles,Tr,vrSimp,1) + moles*(vrSimp*Pr/Tr-1))

    ! Set enthalpy
    enthalpy = hSimp + (wM/wRef)*(hRef - hSimp)

    ! Temperature differential
    if (present(dhdt)) then
      dhSimp = HDiffT(T,P,moles,Tr,vrSimp,TcM,PcM,Bsimp,Csimp,Dsimp,Esimp,1)
      dhRef = HDiffT(T,P,moles,Tr,vrRef,TcM,PcM,B,C,D,E,2)
      dhdt = dhSimp + (wM/wRef)*(dhRef - dhSimp)
    endif
    ! Pressure differential
    if (present(dhdp)) then
      dhSimp = HDiffP(T,P,moles,Tr,vrSimp,TcM,PcM,Bsimp,Csimp,Dsimp,Esimp,1)
      dhRef = HDiffP(T,P,moles,Tr,vrRef,TcM,PcM,B,C,D,E,2)
      dhdp = dhSimp + (wM/wRef)*(dhRef - dhSimp)
    endif
    ! Compozition differentials
    if (present(dhdz)) then
      do i=1,nc
        dwMdNi(i) = wMDiffNi(nc,comp,moles,wM,i)
        dhSimpdz(i) = HDiffNi(nc,comp,cbeos,T,P,moles,Tr,vrSimp,TcM,vcM,PcM,zcM,wM,Bsimp,Csimp,Dsimp,Esimp,z,i,1)
        dhRefdz(i) = HDiffNi(nc,comp,cbeos,T,P,moles,Tr,vrRef,TcM,vcM,PcM,zcM,wM,B,C,D,E,z,i,2)
      enddo
        dhdz = dhSimpdz + (wM/wRef)*(dhRefdz - dhSimpdz) + (dwMdNi/wRef)*(hRef - hSimp)
    endif
  end subroutine lkCalcEnthalpy

  !----------------------------------------------------------------------
  !> Subroutine to calculate the thermodynamic properties of either simple
  !> fluid or reference fluid. Solutions are compined in main routine.
  !>
  !> Thermodynamic properties calculated by:
  !>
  !> \f{align*}{
  !>   & z = \frac{P_r v_r}{T_r} \\
  !>   & S^R(T,V,\textbf{n}) = -R \left[F + T \left(\frac{\partial F}{\partial T}\right)_{V,\textbf{n}} \right] \\
  !>   & H^R(T,P,\textbf{n}) = RT \left(\frac{\partial F}{\partial T}\right)_{V,\textbf{n}} + PV - nRT  \\
  !>   & \ln \phi_i (T,P,\textbf{n}) = \left(\frac{\partial F}{\partial n_i}\right)_{T,V} - \ln z  \\
  !> \f}
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  subroutine thermProps(nc,comp,cbeos,Tr,Pr,vr,nMoles,TcM,vcM,PcM,zcM,wM,moles,z,S,H,lnphi,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in)    :: Tr, Pr, vr, nMoles(nc), TcM, vcM, PcM, zcM, wM, moles
    integer, intent(in) :: simpOrRef
    real, intent(out)   :: z, S, H, lnphi(nc)

    real        :: F, S_VDep, lgZ
    real        :: B, C, D, E
    integer     :: i

    call TrCoeff(Tr,B,C,D,E,simpOrRef)
!    call zPRTshape(T,Tr,B,C,D,E,simpOrRef)
!    call fvShape(Tr*TcM,Pr*PcM,Tr,Pr,B,C,D,E,simpOrRef)

    F = FSolver(moles,vr,B,C,D,E,simpOrRef)
    S_VDep = -Rgas*(F + Tr*FDiffTr(moles,Tr,vr,simpOrRef))
    z = Pr*vr/Tr
    lgZ = log(z)

    S = S_VDep + moles*Rgas*lgZ
    H = Rgas*Tr*TcM*F + Tr*TcM*S_VDep + Pr*(moles*Rgas*TcM*vr) - moles*Rgas*Tr*TcM

    do i = 1,nc
      lnphi(i) = FDiffNi(nc,comp,cbeos,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,B,C,D,E,simpOrRef) - lgZ
    end do

  end subroutine thermProps

  !----------------------------------------------------------------------
  !> Calculate the critical mixing temperature, volume, pressure, compressibility
  !> and acentricity factor, for given composition.
  !> Critical temperature and critical volume is used in the calculations, these
  !> values are known.
  !>
  !> Mixing rules:
  !> \f{align*}{
  !>   & T_{cM} = \frac{1}{v_{cM}^\eta n^2} \sum_j \sum_k n_j n_k \cdot v_{c,jk}^\eta \cdot T_{c,jk} \\
  !>   & v_{cM} = \frac{1}{n^2} \sum_j \sum_k n_j n_k \cdot v_{c,jk} \\
  !>   & \omega_M = \frac{1}{n} \sum_j n_j \omega_j  \\
  !>   & z_{cM} = (0.2905 - 0.085 \omega_M) \\
  !>   & P_{cM} = R\frac{z_{cM} T_{cM}}{v_{cM}} \\
  !> \f}
  !>
  !> \f{align*}{
  !>   & T_{c,jk} = (T_{c,j} \cdot T_{c,k})^{1/2} \cdot \kappa_{jk} \\
  !>   & v_{c,jk} = \frac{1}{8}(v_{c,j}^{1/3} + v_{c,k}^{1/3})^3 \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------
  subroutine mixRules(nc,comp,cbeos,TcM, vcM , PcM, zcM, wM, nMoles, moles)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: nMoles(nc), moles
    real, intent(out) :: vcM, TcM, wM, zcM, PcM

    integer :: j,k
    real :: vcjk, Tcjk, vcMinv, vcj, vck, tcj, tck, molesInv

    vcM = 0.0
    TcM = 0.0
    wM = 0.0
    molesInv = 1/moles
    do j = 1,nc
      do k = 1,nc
        tcj = comp(j)%p_comp%tc
        tck = comp(k)%p_comp%tc
        vcj = Rgas*comp(j)%p_comp%zc * tcj/comp(j)%p_comp%pc
        vck = Rgas*comp(k)%p_comp%zc * tck/comp(k)%p_comp%pc
        vcjk = 0.125*(vcj**(1.0/3.0) + vck**(1.0/3.0))**3
        Tcjk = cbeos%kij(j,k)*((tcj*tck)**0.5)
        vcM = vcM + nMoles(j)*nMoles(k)*vcjk
        TcM = TcM + nMoles(j)*nMoles(k)*Tcjk*(vcjk**eta)
      end do
      wM = wM + nMoles(j)*comp(j)%p_comp%acf
    end do

    vcM = vcM*molesInv*molesInv
    vcMinv = 1/vcM

    TcM = TcM*molesInv*molesInv*(vcMinv**eta)
    wM = wM*molesInv
    zcM = 0.2905 - 0.085*wM
    PcM = Rgas*zcM*TcM*vcMinv

  end subroutine mixRules

  !----------------------------------------------------------------------
  !> Calculate the reduced temperature dependent coefficients B,C,D and E,
  !> in the expression for Helmholts reduced recidual function F, for given
  !> reduced temperature.
  !>
  !> \f{align*}{
  !>   & B(T_r) = b_1 - \frac{b_2}{T_r} - \frac{b_3}{T_r^2} - \frac{b_4}{T_r^3} \\
  !>   & C(T_r) = c_1 - \frac{c_2}{T_r} + \frac{c_3}{T_r^3} \\
  !>   & D(T_r) = d_1 + \frac{d_2}{T_r}  \\
  !>   & E(T_r) = \frac{c_4}{T_r^3}  \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------
  subroutine TrCoeff(Tr,B,C,D,E,simpOrRef)

    real, intent(in) :: Tr
    integer, intent(in) :: simpOrRef
    real, intent(out) :: B,C,D,E
    real :: TrInv, TrInv2,TrInv3
    integer :: sr

    sr = simpOrRef
    TrInv = 1/Tr
    TrInv2 = TrInv*TrInv
    TrInv3 = TrInv2*TrInv

    B = b1(sr) - b2(sr)*TrInv - b3(sr)*TrInv2 - b4(sr)*TrInv3
    C = c1(sr) - c2(sr)*TrInv + c3(sr)*TrInv3
    D = d1(sr) + d2(sr)*TrInv
    E = c4(sr)*TrInv3

  end subroutine TrCoeff

  !----------------------------------------------------------------------
  !> Calculate the first order derivatives of the reduced temperature dependent
  !> coefficients B,C,D and E, for given reduced temperature.
  !>
  !> \f{align*}{
  !>   & B_{T_r} = \frac{\partial B}{\partial T_r} = \frac{b_2}{T_r^2} + \frac{2b_3}{T_r^3} + \frac{3b_4}{T_r^4} \\
  !>   & C_{T_r} = \frac{\partial C}{\partial T_r} = \frac{c_2}{T_r^2} - \frac{3c_3}{T_r^4} \\
  !>   & D_{T_r} = \frac{\partial D}{\partial T_r} = -\frac{d_2}{T_r^2}  \\
  !>   & E_{T_r} = \frac{\partial E}{\partial T_r} = -\frac{3c_4}{T_r^4}  \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  subroutine TrCoeffDiff1(Tr,B_Tr,C_Tr,D_Tr,E_Tr,simpOrRef)

    real, intent(in) :: Tr
    integer, intent(in) :: simpOrRef
    real, intent(out) :: B_Tr,C_Tr,D_Tr,E_Tr
    real :: TrInv, TrInv2,TrInv3, TrInv4
    integer :: sr

    sr = simpOrRef
    TrInv = 1/Tr
    TrInv2 = TrInv*TrInv
    TrInv3 = TrInv2*TrInv
    TrInv4 = TrInv2*TrInv2

    B_Tr = b2(sr)*TrInv2 + 2*b3(sr)*TrInv3 + 3*b4(sr)*TrInv4
    C_Tr = c2(sr)*TrInv2 - 3*c3(sr)*TrInv4
    D_Tr = -d2(sr)*TrInv2
    E_Tr = -3*c4(sr)*TrInv4

  end subroutine TrCoeffDiff1


  !----------------------------------------------------------------------
  !> Calculate the second order derivatives of the reduced temperature dependent
  !> coefficients B,C,D and E, for given reduced temperature.
  !>
  !> \f{align*}{
  !>   & B_{TT_r} = \frac{\partial^2 B}{\partial^2 T_r} = -\frac{2b_2}{T_r^3} - \frac{6b_3}{T_r^4} -\frac{12b_4}{T_r^5} \\
  !>   & C_{TT_r} = \frac{\partial^2 C}{\partial^2 T_r} = - \frac{2c_2}{T_r^3} + \frac{12c_3}{T_r^5} \\
  !>   & D_{TT_r} = \frac{\partial^2 D}{\partial^2 T_r} = \frac{2d_2}{T_r^3}  \\
  !>   & E_{TT_r} = \frac{\partial^2 E}{\partial^2 T_r} = \frac{12c_4}{T_r^5}  \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  subroutine TrCoeffDiff2(Tr,B_TrTr,C_TrTr,D_TrTr,E_TrTr,simpOrRef)

    real, intent(in) :: Tr
    integer, intent(in) :: simpOrRef
    real, intent(out) :: B_TrTr,C_TrTr,D_TrTr,E_TrTr
    real :: TrInv, TrInv2,TrInv3, TrInv4, TrInv5
    integer :: sr

    sr = simpOrRef
    TrInv = 1/Tr
    TrInv2 = TrInv*TrInv
    TrInv3 = TrInv2*TrInv
    TrInv4 = TrInv2*TrInv2
    TrInv5 = TrInv4*TrInv

    B_TrTr = -2*b2(sr)*TrInv3 - 6*b3(sr)*TrInv4 - 12*b4(sr)*TrInv5
    C_TrTr = -2*c2(sr)*TrInv3 + 12*c3(sr)*TrInv5
    D_TrTr = 2*d2(sr)*TrInv3
    E_TrTr = 12*c4(sr)*TrInv5

  end subroutine TrCoeffDiff2

  !----------------------------------------------------------------------
  !> Calculate the reduced volume, vr, by use of the Newton-Rapson numerical
  !> method to solve Lee Keslers equation of state, given reduced temperature,
  !> reduced pressure and composition.The half step method is used to secure that
  !> vr is inside its limit.
  !> Calls upon functions fv and fvDiff, acting as the function f and the derivative
  !> of f in the numerical method.
  !>
  !> \f[
  !>   x_{n+1} = x_{n} - \frac{f(x_{n})}{f'(x_{n})}
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  subroutine vrNewtRaps(Tr,Pr,usedPhase,simpOrRef,vr,correctPhase)

    real, intent(in)::Tr, Pr
    integer, intent(in) :: usedPhase, simpOrRef
    real, intent(out) :: vr
    logical, intent(out) :: correctPhase

    real :: vrTemp, f, fDiff, vrMin, vrMax, B, C, D, E
    real, parameter :: tolerance = 1E-12
    integer, parameter :: maxIterations = 1000
    integer :: i

    !Functions called by this function: fv, fvDiff, vrInitial

    correctPhase = .false.
    call TrCoeff(Tr,B,C,D,E,simpOrRef)
    call vrInitial(Pr,Tr,usedPhase,simpOrRef,vr,vrMin,vrMax)

    f = fv(Pr,Tr,vr,B,C,D,E,simpOrRef)

    ! open(file='v.dat',unit=12)
    ! write(12,*) '#F(z)'
    ! write(12,*) '#v (-)',char(9),'F (-)'
    ! do i=1,10000
    !   !z = zmin + (zmax - zmin)*real(i-1)/real(10000-1)
    !   vr = vrmin + (vrmin*1000.0 - vrmin)*real(i-1)/real(10000-1)
    !   f = fv(Pr,Tr,vr,B,C,D,E,simpOrRef)
    !   write(12,*) vr, f
    !   print *,f
    ! enddo
    ! close(12)
    ! call exit(1)

    do i = 1,maxIterations
      fDiff = fvDiff(Pr,Tr,vr,B,C,D,E,simpOrRef)
      vrTemp = vr - f/fDiff

      if(abs(vrTemp-vr) < tolerance) then
        correctPhase = .true.
        exit
      end if

      if (vrTemp > vrMax) then
        vr = (vr+vrMax)*0.5
      else if (vrTemp < vrMin) then
        vr = (vr+vrMin)*0.5
      else
        vr = vrTemp
      end if


      f = fv(Pr,Tr,vr,B,C,D,E,simpOrRef)
      if(f < 0) then
        vrMin = vr
      else
        vrMax = vr
      end if

    end do

    if (correctPhase .eqv. .false.) then
        print *, 'Error. No solution found within desired tolerance for phase/fluid:'
        if (usedPhase == 1 .and. simpOrRef == 1) print *, 'liquid/simple'
        if (usedPhase == 1 .and. simpOrRef == 2) print *, 'liquid/reference'
        if (usedPhase == 2 .and. simpOrRef == 1) print *, 'gas/simple'
        if (usedPhase == 2 .and. simpOrRef == 2) print *, 'gas/reference'
        vr = vrMin
    end if

  end subroutine vrNewtRaps

  !----------------------------------------------------------------------
  !> Calculate the function f to be used in the numerical method to find reduced
  !> volume, given reduced pressure, reduced temperature, coefficients B - E
  !> and an increasingly more correct reduced volume, for each call.
  !>
  !> \f[
  !>   f = v_r - \frac{T_r}{P_r} \left[1 + \frac{B}{v_r} + \frac{C}{v_r^2} + \frac{D}{v_r^5} + \frac{E}{v_r^2} \left( \beta + \frac{\gamma}{v_r^2} \right) \exp{\left(-\frac{\gamma}{v_r^2}\right)} \right] = 0
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function fv(Pr,Tr,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: Pr, Tr, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: fv, vrInv, vrInv2, vrInv5
    real :: be, ga

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)

    vrInv = 1/vr
    vrInv2 = vrInv*vrInv
    vrInv5 = vrInv2*vrInv2*vrInv
    fv = vr - (Tr/Pr)*(1 + B*vrInv + C*vrInv2 + D*vrInv5 + E*vrInv2*(be + ga*vrInv2)*exp(-ga*vrInv2))

  end function fv

  !----------------------------------------------------------------------
  !> Calculate the derivative of function f to be used in the numerical method to find
  !> reduced specific volume.
  !>
  !> \f[
  !>   \left(\frac{\partial f}{\partial v_r}\right)_{T_r,P_r, \textbf{n}} = 1 + \frac{Pr}{Tr} \left[ \frac{B}{v_r} +
  !>  \frac{2C}{v_r^3} + \frac{5D}{v_r^6} - \frac{E}{v_r^3}  \left(-2 \beta + \frac{\gamma (\beta - 4)}{v_r^2} +
  !>   \frac{2 \gamma^2}{v_r^4} \right) \exp \left(- \frac{\gamma}{v_r^2} \right) \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function fvDiff(Pr,Tr,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: Pr, Tr, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: fvDiff, vrInv, vrInv2, vrInv3, vrInv4, vrInv5, vrInv6
    real :: be, ga

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)

    vrInv = 1/vr
    vrInv2 = vrInv*vrInv
    vrInv3 = vrInv * vrInv2
    vrInv4 = vrInv2*vrInv2
    vrInv5 = vrInv2*vrInv3
    vrInv6 = vrInv3*vrInv3

    fvDiff = 1 + (Pr/Tr)*(B*vrInv2+2*C*vrInv3+5*D*vrInv6-E*exp(-ga*vrInv2)*vrInv3*(-2*be+ ga*vrInv2*(be-4) + 2*ga*ga*vrInv4))

  end function fvDiff

  !----------------------------------------------------------------------
  !> Calculate the reduced recidual Helmholtz function F, given number of moles,
  !> reduced volume and coefficients B - E. This is the main function to be used
  !> to find the desired thermodynamic properties.
  !>
  !> \f[
  !>   F(T,V,\textbf{n}) = n\left[\frac{B}{v_r} + \frac{C}{2v_r^2} + \frac{D}{5v_r^5} + \frac{E}{2 \gamma} (\beta + 1) -
  !>   E \exp \left(-\frac{\gamma}{v_r^2}\right) \left(\frac{1}{2\gamma} (\beta +1) + \frac{1}{2v_r^2}\right) \right]
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function FSolver(moles,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: FSolver, vrInv, vrInv2, vrInv5
    real :: be, ga, gaInv

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)
    gaInv = 1/ga

    vrInv = 1/vr
    vrInv2 = vrInv*vrInv
    vrInv5 = vrInv*vrInv2*vrInv2

    FSolver = moles*(B*vrInv+0.5*C*vrInv2+0.2*D*vrInv5+0.5*E*gaInv*(be+1)-E*exp(-ga*vrInv2)*(0.5*gaInv*(be+1)+0.5*vrInv2))

  end function FSolver


  !----------------------------------------------------------------------
  !>In the following, functions that give the partial derivatives of F with respect
  !>to reduced temperature, reduced specific volume and composition are given
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the first order derivative of Helmholts reduced recidual function, with respect to
  !> reduced temperature, for given moles, reduced temperature and reduced specific volume.
  !> Volume and compotsition is fixed.
  !>
  !> \f{align*}{
  !>   \left( \frac{\partial F}{\partial T_r} \right)_{v_r, n}
  !>  & = n \bigg[ \frac{B_{T_r}}{v_r} + \frac{C_{T_r}}{2v_r^2} + \frac{D_{T_r}}{5v_r^5} + \frac{E_{T_r}}{2\gamma} (\beta + 1) \\
  !>   & \qquad \qquad- E_{T_r}\exp \left(-\frac{\gamma}{v_r^2} \right) \left(\frac{1}{2\gamma} (\beta + 1) + \frac{1}{2v_r^2} \right)  \bigg] \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiffTr(moles,Tr,vr,simpOrRef)

    real, intent(in) :: moles, Tr, vr
    integer, intent(in) :: simpOrRef
    real :: B_Tr, C_Tr, D_Tr, E_Tr
    real :: FDiffTr
    !Functions called: Fsolver

    call TrCoeffDiff1(Tr,B_Tr,C_Tr,D_Tr,E_Tr,simpOrRef)
    FDiffTr = FSolver(moles,vr,B_Tr,C_Tr,D_Tr,E_Tr,simpOrRef)

  end function FDiffTr

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of Helmholts reduced recidual function, with respect to
  !> reduced temperature, for given moles, reduced temperature and reduced specific volume.
  !> Volume and compotsition is fixed.
  !>
  !> \f{align*}{
  !>   \left( \frac{\partial^2 F}{\partial T_r^2} \right)_{v_r, n}
  !>   & = n \bigg[ \frac{B_{TT_r}}{v_r} + \frac{C_{TT_r}}{2v_r^2} + \frac{D_{TT_r}}{5v_r^5} + \frac{E_{TT_r}}{2\gamma} (\beta + 1) \\
  !>   & \qquad \qquad- E_{TT_r}\exp \left(-\frac{\gamma}{v_r^2} \right) \left(\frac{1}{2\gamma} (\beta + 1) + \frac{1}{2v_r^2} \right)  \bigg] \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiff2Tr(moles,Tr,vr,simpOrRef)

    real, intent(in) :: moles, Tr, vr
    integer, intent(in) :: simpOrRef
    real :: B_TrTr, C_TrTr, D_TrTr, E_TrTr
    real :: FDiff2Tr
    !Functinos called: FSolver

    call TrCoeffDiff2(Tr, B_TrTr, C_TrTr, D_TrTr, E_TrTr,simpOrRef)
    FDiff2Tr = FSolver(moles,vr,B_TrTr,C_TrTr,D_TrTr,E_TrTr,simpOrRef)

  end function FDiff2Tr

  !----------------------------------------------------------------------
  !> Calculate the first order derivative of Helmholts reduced recidual function, with respect to
  !> reduced volume, for given moles, reduced temperature and reduced specific volume.
  !> Temperature and compotsition is fixed.
  !>
  !> \f[
  !>   \left( \frac{\partial F}{\partial v_r} \right)_{T_r, n} = n \left[ -\frac{B}{v_r^2} - \frac{C}{v_r^3} - \frac{D}{v_r^6} -
  !>   \frac{E}{v_r^3} \left( \beta + \frac{\gamma}{v_r^2} \right)\exp \left(-\frac{\gamma}{v_r^2} \right) \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiffVr(moles,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: vrInv, vrInv2, vrInv3, vrInv6
    real :: FDiffVr
    real :: be, ga

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)

    vrInv = 1/vr
    vrInv2 = vrInv*vrInv
    vrInv3 = vrInv * vrInv2
    vrInv6 = vrInv3*vrInv3

    FDiffVr = - moles*(B*vrInv2 + C*vrInv3 + D*vrInv6 + E*vrInv3*(be + ga*vrInv2)*exp(-ga*vrInv2))

  end function FDiffVr

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of Helmholts reduced recidual function, with respect to
  !> reduced volume, for given moles, reduced temperature and reduced specific volume.
  !> Temperature and compotsition is fixed.
  !>
  !> \f[
  !>   \left(\frac{\partial^2 F}{\partial v_r^2}\right)_{T_r, n} = n \left[\frac{2B}{v_r^3} + \frac{3C}{v_r^4} + \frac{6D}{v_r^7} +
  !>   \frac{E}{v_r^4} \left(3\beta + \frac{\gamma(5 - 2\beta)}{v_r^2} -\frac{2\gamma^2}{v_r^4}\right) \exp \left(-\frac{\gamma}{v_r^2} \right)\right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiff2Vr(moles,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: vrInv, vrInv2, vrInv3, vrInv4, vrInv5, vrInv6, vrInv7
    real :: FDiff2Vr
    real :: be, ga

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)

    vrInv = 1/vr
    vrInv2 = vrInv*vrInv
    vrInv3 = vrInv * vrInv2
    vrInv4 = vrInv2*vrInv2
    vrInv5 = vrInv2*vrInv3
    vrInv6 = vrInv3*vrInv3
    vrInv7 = vrInv3*vrInv4

    FDiff2Vr = moles*(2*B*vrInv3 + 3*C*vrInv4 +6*D*vrInv7 + E*vrInv4*exp(-ga*vrInv2)*(3*be + ga*(5-2*be)*vrInv2 -2*ga*ga*vrInv4))

  end function FDiff2Vr

  !----------------------------------------------------------------------
  !> Calculate the third order derivative of Helmholtz reduced recidual function, with respect to
  !! reduced volume, for given moles, reduced temperature and reduced specific volume.
  !! Temperature and compotsition is fixed.
  !!
  !! \f{align*}{
  !!   \left(\frac{\partial^3 F}{\partial v_r^3}\right)_{T_r, n} & = n \bigg[-\frac{6B}{v_r^4} - \frac{12C}{v_r^5} - \frac{42D}{v_r^8} \\
  !!  & \quad - \frac{E}{v_r^5} \left(12\beta + \frac{(30 - 18\beta)\gamma}{v_r^2} + \frac{\left(4\beta-26\right)\gamma^2}{v_r^4} + \frac{4\gamma^3}{v_r^6} \right) \exp \left(-\frac{\gamma}{v_r^2} \right)\bigg] \\
  !! \f}
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  function FDiff3Vr(moles,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: vrInv, vrInv2, vrInv3, vrInv4, vrInv5, vrInv6, vrInv8
    real :: FDiff3Vr
    real :: be, ga, ga2, ga3

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)
    ga2 = ga*ga
    ga3 = ga*ga2
    vrInv = 1/vr
    vrInv2 = vrInv*vrInv
    vrInv3 = vrInv*vrInv2
    vrInv4 = vrInv2*vrInv2
    vrInv5 = vrInv2*vrInv3
    vrInv6 = vrInv3*vrInv3
    vrInv8 = vrInv4*vrInv4

    FDiff3Vr = - moles*(6*B*vrInv4 + 12*C*vrInv5 +42*D*vrInv8 + E*vrInv5*exp(-ga*vrInv2)*&
         (12*be + ga*(30-18*be)*vrInv2 + ga2*(4*be-26)*vrInv4 + 4*ga3*vrInv6))

  end function FDiff3Vr

  !----------------------------------------------------------------------
  !> Calculate the first order derivative of Helmholts reduced recidual function, with respect to
  !> composition, n(i), for given copmosition, reduced temperature and reduced specific volume.
  !> Temperature and volume is fixed.
  !>
  !> \f[
  !>   F_i = \left(\frac{\partial F}{\partial n_i}\right)_{T,V} = F_N + F_X X_i + F_Y Y_i
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiffNi(nc,comp,cbeos,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,B,C,D,E,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: Tr, vr, TcM, vcM, PcM, zcM, wM, nMoles(nc), moles, B, C, D, E
    integer, intent(in) :: i, simpOrRef
    real :: FDiffNi
    !Functions called: FDiffTr, FDiffVr, FDiffN, TrDiffNi, vrDiffNi

    FDiffNi = FDiffN(moles,vr,B,C,D,E,simpOrRef) &
         + FDiffTr(moles,Tr,vr,simpOrRef) * &
            TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles, moles, i) &
         + FDiffVr(moles,vr,B,C,D,E,simpOrRef) * &
           vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)

  end function FDiffNi

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of Helmholts reduced recidual function, with respect to
  !> composition.
  !> Temperature and volume is fixed.
  !>
  !> \f{align*}{
  !>   F_{ij} = \left(\frac{\partial^2 F}{\partial n_i \partial n_j}\right)_{T,V} & = F_{NN} + F_{NX}X_{j} + F_{NY} Y_{j} \\
  !>   & \quad + \left( F_{N X} + F_{XY} Y_{j} + F_{XX} X_{j} \right) X_{i} + F_{X} X_{ij} \\
  !>   & \quad + \left( F_{N Y} + F_{YY} Y_{j} + F_{XY} X_{j} \right) Y_{i} + F_{Y}Y_{ij}  \\
  !> \f}
  !>
  !> With:
  !> X = T_r
  !> Y = v_r
  !> N = n
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiff2NiNj(nc,comp,cbeos,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j,B,C,D,E,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: Tr, vr, TcM, vcM, PcM, zcM, wM, nMoles(nc), moles, B, C, D, E
    integer, intent(in) :: i,j,simpOrRef
    real :: molesInv, F_NN, F_Tr, F_TrTr, F_TrN, Tr_j, Tr_i, Tr_ij, F_Vr, F_VrVr, F_VrN, vr_j, vr_i, vr_ij, F_TrVr
    real :: FDiff2NiNj
    !Functions called: FDiffTr, FDiff2Tr, TrDiffNi, TrDiff2NiNj, FDiffVr, FDiff2Vr, vrDiffNi, vrDiff2NiNj,
    !                  FDiff2TrVr, FDiff2TrN, FDiff2VrN

    molesInv = 1.0/moles

    F_NN = 0

    F_Tr = FDiffTr(moles,Tr,vr,simpOrRef)
    F_TrTr = FDiff2Tr(moles,Tr,vr,simpOrRef)
    F_TrN = FDiff2TrN(moles,Tr,vr,simpOrRef)
    Tr_j = TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,j)
    Tr_i = TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i)
    Tr_ij = TrDiff2NiNj(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i,j)

    F_Vr = FDiffVr(moles,vr,B,C,D,E,simpOrRef)
    F_VrVr = FDiff2Vr(moles,vr,B,C,D,E,simpOrRef)
    F_VrN = FDiff2VrN(moles,vr,B,C,D,E,simpOrRef)
    vr_j = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,j)
    vr_i = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    vr_ij = vrDiff2NiNj(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j)

    F_TrVr = FDiff2TrVr(moles,Tr,vr,simpOrRef)

    FDiff2NiNj = F_NN + F_TrN*Tr_j + F_VrN*vr_j &
         + (F_TrN + F_TrTr*Tr_j + F_TrVr*vr_j)*Tr_i &
         + F_Tr*Tr_ij + (F_VrN + F_TrVr*Tr_j + F_VrVr*vr_j)*vr_i + F_Vr*vr_ij

  end function FDiff2NiNj


  !---------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Help functions to calculate the first and second order derivatives of
  !> F with respect to composition.
  !>
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the derivative of Helmholts reduced recidual function, with respect to
  !> total number of moles, for given moles, reduced temperature and reduced specific volume.
  !> Reduced temperature and reduced volume are fixed in this derivative.
  !>
  !> \f[
  !>   F_N = \left(\frac{\partial F}{\partial n}\right)_{T_r, v_r} = \frac{F}{n}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function FDiffN(moles,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: FDiffN, molesInv
    !Functions called: FSolver, FDiffVr

    molesInv = 1.0/moles

    FDiffN = molesInv*FSolver(moles,vr,B,C,D,E,simpOrRef)

  end function FDiffN

  !----------------------------------------------------------------------
  !> Calculate the cross derivative of F, with respect to reduced volume and
  !> total number of moles, for fixed reduced temperature.
  !>
  !> \f[
  !>   F_{N Y} =  \left(\frac{\partial^2 F}{\partial n \partial v_r}\right)_{T_r} = \left(\frac{\partial}{\partial n}
  !>    \left(\frac{\partial F}{\partial v_r}\right)_{T_r,n} \right)_{T_r, v_r} = \frac{1}{n} \left(\frac{\partial F}{\partial v_r}\right)_{T_r,n}
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function FDiff2VrN(moles,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: FDiff2VrN
    !Functions called: FDiffVr, FDiff2Vr

    FDiff2VrN = (1.0/moles)*FDiffVr(moles,vr,B,C,D,E,simpOrRef)

  end function FDiff2VrN

  !----------------------------------------------------------------------
  !> Calculate the cross derivative of F, with respect to reduced temperature and
  !> total number of moles, for fixed reduced volume.
  !>
  !> \f[
  !>   F_{n X} = \left(\frac{\partial^2 F}{\partial n \partial T_r}\right)_{v_r} = \frac{1}{n} \left(\frac{\partial F}{\partial T_r}\right)_{v_r,n}
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function FDiff2TrN(moles,Tr,vr,simpOrRef)
    real, intent(in) :: moles, Tr, vr
    integer, intent(in) :: simpOrRef
    real :: FDiff2TrN
    !Functions called: FDiffTr

    FDiff2TrN = (1.0/moles)*FDiffTr(moles,Tr,vr,simpOrRef)

  end function FDiff2TrN

  !----------------------------------------------------------------------
  !> Calculate the first order derivative of reduced temperature, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   X_i = \left(\frac{\partial T_r}{\partial n_i} \right)_{T,V} = -\frac{T_r}{T_{cM}} \left(\frac{\partial T_{cM}}{\partial n_i} \right)_{T,V}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: Tr, TcM, vcM, nMoles(nc), moles
    integer, intent(in) :: i
    real :: TrDiffNi
    !Functions called: TcMDiffNi

    TrDiffNi = -(Tr/TcM) * TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)

  end function TrDiffNi

  !----------------------------------------------------------------------
  !> Calculate the first order derivative of reduced volume, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   Y_i = \left(\frac{\partial v_r}{\partial n_i} \right)_{T,V} = \frac{v_r}{P_{cM}} \left(\frac{\partial P_{cM}}{\partial n_i} \right)_{T,V} - \frac{v_r}{T_{cM}} \left( \frac{\partial T_{cM}}{\partial n_i} \right)_{T,V} -\frac{v_r}{n}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------
  function vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: vr, TcM, vcM, PcM, zcM, wM, nMoles(nc), moles
    integer, intent(in) :: i
    real :: vrDiffNi
    !Functions called: PcMDiffNi, TcMDiffNi

    vrDiffNi = (vr/PcM) * PcMDiffNi(nc, comp, cbeos, TcM, vcM, PcM, zcM, wM, nMoles, moles, i) &
              - (vr/TcM) * TcMDiffNi(nc, comp, cbeos, TcM, vcM, nMoles, moles, i) - (vr/moles)

  end function vrDiffNi


  !----------------------------------------------------------------------
  !> Calculate the second order derivative of reduced temperature, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the second order derivative of F, with respect to composition.
  !>
  !> \f[
  !>    X_{ij} = \left(\frac{\partial^2 T_r}{\partial n_i \partial n_j}\right)_{T,V} = \frac{2T_r}{T_{cM}^2}
  !>  \left(\frac{\partial T_{cM}}{\partial n_i}\right) \left(\frac{\partial T_{cM}}{\partial n_j}\right) - \frac{T_r}{T_{cM}}
  !>  \left(\frac{\partial^2 T_cM}{\partial n_i \partial n_j}\right)
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function TrDiff2NiNj(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i,j)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: Tr, TcM, vcM, nMoles(nc), moles
    integer, intent(in) :: i,j
    real :: TcMInv
    real :: TrDiff2NiNj
    !Functions called: TcMDiffNi, TcMDiff2NiNj

    TcMInv = 1.0/TcM

    TrDiff2NiNj = 2*Tr*TcMInv*TcMInv*TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)*&
         TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,j) &
         - Tr*TcMInv*TcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,nMoles,moles,i,j)

  end function TrDiff2NiNj

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of reduced volume, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the second order derivative of F, with respect to composition.
  !>
  !> \f{align*}{
  !>   & Y_{ij} = \left(\frac{\partial^2 v_r}{\partial n_i \partial n_j}\right)_{T,V} = \frac{1}{v_r} \left(\frac{\partial v_r}{\partial n_i}\right)_{T,V}
  !> \left(\frac{\partial v_r}{\partial n_j}\right)_{T,V} - \frac{v_r}{P_{cM}^2} \left(\frac{\partial P_{cM}}{\partial n_i}\right)
  !> \left(\frac{\partial P_{cM}}{\partial n_j}\right) \\
  !>   & \qquad  \qquad  \qquad \qquad + \frac{v_r}{P_{cM}} \left(\frac{\partial^2 P_cM}{\partial n_i \partial n_j}\right) +
  !>   \frac{v_r}{T_{cM}^2} \left(\frac{\partial T_{cM}}{\partial n_i}\right) \left(\frac{\partial T_{cM}}{\partial n_j}\right) \\
  !>   & \qquad  \qquad  \qquad \qquad - \frac{v_r}{T_{cM}} \left(\frac{\partial^2 T_cM}{\partial n_i \partial n_j}\right) + \frac{v_r}{n^2}  \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------
  function vrDiff2NiNj(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: vr, TcM, vcM, PcM, zcM, wM, nMoles(nc), moles
    integer, intent(in) :: i,j
    real :: vr_i, vr_j, PcM_i, PcM_j, PcM_ij, TcM_i, TcM_j, TcM_ij, vrInv, PcMInv, TcMInv, molesInv
    real :: vrDiff2NiNj
    !Functions called: vrDiffNi, PcMDiffNi, PcMDiff2NiNj, TcMDiffNi, TcMDiff2NiNj

    vrInv = 1.0/vr
    PcMInv = 1.0/PcM
    TcMInv = 1.0/TcM
    molesInv = 1.0/moles

    vr_i = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    vr_j = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,j)
    PcM_i = PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    PcM_j = PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,j)
    PcM_ij = PcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j)
    TcM_i = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)
    TcM_j = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,j)
    TcM_ij = TcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,nMoles,moles,i,j)

    vrDiff2NiNj = vrInv*vr_i*vr_j - vr*PcMInv*PcMInv*PcM_i*PcM_j + vr*PcMInv*PcM_ij &
                  + vr*TcMInv*TcMInv*TcM_i*TcM_j - vr*TcMInv*TcM_ij + vr*molesInv*molesInv

  end function vrDiff2NiNj


  !----------------------------------------------------------------------
  !> Calculate the first order derivative of critical mixing temperature, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !> \left(\frac{\partial T_{cM}}{\partial n_i} \right)_{T,V} =-\frac{2}{n}T_{cM} -\frac{\eta T_{cM}}{v_{cM}} \left(\frac{\partial v_{cM}}{\partial n_i}\right)_{T,V}+ \frac{2}{v_{cM}^{\eta} n^2} \sum_l n_l v_{c,il}^\eta T_{c,il}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------
  function TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: TcM, vcM, nMoles(nc), moles
    integer, intent(in) :: i
    real :: vcMInv, molesInv, t1, t2, t3, vcil, Tcil, tci, tcl, vci, vcl
    real :: TcMDiffNi
    integer :: l
    !Functions called: vcMDiffNi

    molesInv = 1.0/moles
    vcMInv = 1.0/vcM
    t2 = 0.0

    t1 = -eta*TcM*vcMInv*vcMDiffNi(nc,comp,nMoles,vcM,moles,i)

    tci = comp(i)%p_comp%tc
    vci = Rgas*comp(i)%p_comp%zc * tci/comp(i)%p_comp%pc
    do l = 1,nc
      tcl = comp(l)%p_comp%tc
      vcl = Rgas*comp(l)%p_comp%zc * tcl/comp(l)%p_comp%pc
      vcil = 0.125*(vci**(1.0/3.0) + vcl**(1.0/3.0))**3
      Tcil = (cbeos%kij(i,l))*(tci*tcl)**0.5
      t2 = t2 + nMoles(l)*(vcil**eta)*Tcil
    end do

    t2 = 2*t2*(vcMinv**eta)*molesInv*molesInv
    t3 = -2*TcM*molesInv
    TcMDiffNi = t1 + t2 + t3

  end function TcMDiffNi

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of critical mixing temperature, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the second order derivative of F, with respect to composition.
  !>
  !> \f{align*}{
  !>   \left(\frac{\partial^2 T_cM}{\partial n_i \partial n_j}\right) =
  !>   & \frac{2}{n^2} T_{cM} - \frac{2}{n} \left(\frac{\partial T_{cM}}{\partial n_j}\right) - \frac{\eta}{v_{cM}}
  !>  \left(\frac{\partial v_{cM}}{\partial n_{i}}\right) \left(\frac{\partial T_{cM}}{\partial n_j}\right) \\
  !>   & \qquad + \frac{\eta T_{cM}}{v_{cM}^2} \left(\frac{\partial v_{cM}}{\partial n_{i}}\right) \left(\frac{\partial v_{cM}}{\partial n_{j}}\right) - \frac{\eta T_{cM}}{v_{cM}} \left(\frac{\partial^2 v_cM}{\partial n_i \partial n_j}\right) \\
  !>   & \qquad - \frac{4}{n^3 v_{cM}^\eta} \sum_l n_l v_{c,il}^\eta T_{c,il} - \frac{2 \eta}{v_{cM}^{\eta + 1} n^2}
  !> \left(\frac{\partial v_{cM}}{\partial n_{j}}\right) \sum_l n_l v_{c,il}^\eta T_{c,il}  \\
  !>   & \qquad + \frac{2}{v_{cM}^\eta n^2} v_{c,ij}^\eta T_{c,ij}  \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function TcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,nMoles,moles,i,j)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: TcM, vcM, nMoles(nc), moles
    integer, intent(in) :: i,j
    real :: vcMInv, vcMInv2, molesInv, molesInv2, molesInv3, TcM_j, vcM_i, vcM_j, vcM_ij, compSum
    real :: tci, vci, tcl, vcl, tcj, vcj, vcil, Tcil, vcij, Tcij
    real :: TcMDiff2NiNj
    integer :: l
    !Functions called: TcMDiffNi, vcMDiffNi, vcMDiff2NiNj

    vcMInv = 1.0/vcM
    vcMInv2 = vcMInv * vcMInv
    molesInv = 1.0/moles
    molesInv2 = molesInv*molesInv
    molesInv3 = molesInv*molesInv2

    TcM_j = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,j)
    vcM_i = vcMDiffNi(nc,comp,nMoles,vcM,moles,i)
    vcM_j = vcMDiffNi(nc,comp,nMoles,vcM,moles,j)
    vcM_ij = vcMDiff2NiNj(nc,comp,nMoles,moles,vcM,i,j)

    compSum = 0.0

    tci = comp(i)%p_comp%tc
    vci = Rgas*comp(i)%p_comp%zc * tci/comp(i)%p_comp%pc
    do l = 1,nc
      tcl = comp(l)%p_comp%tc
      vcl = Rgas*comp(l)%p_comp%zc * tcl/comp(l)%p_comp%pc
      vcil = 0.125*(vci**(1.0/3.0) + vcl**(1.0/3.0))**3
      Tcil = (cbeos%kij(i,l))*(tci*tcl)**0.5
      compSum = compSum + nMoles(l)*(vcil**eta)*Tcil
    end do

    tcj = comp(j)%p_comp%tc
    vcj = Rgas*comp(j)%p_comp%zc * tcj/comp(j)%p_comp%pc
    Tcij = (cbeos%kij(i,j))*(tci*tcj)**0.5
    vcij = 0.125*(vci**(1.0/3.0) + vcj**(1.0/3.0))**3

    TcMDiff2NiNj = 2*molesInv2*TcM - 2*molesInv*TcM_j - eta*vcMInv*vcM_i*TcM_j + eta*TcM*vcMInv2*vcM_i*vcM_j &
    - eta*TcM*vcMInv*vcM_ij - (4*molesInv3*(vcMInv**eta) + 2*eta*molesInv2*vcM_j*(vcMInv**(eta+1)))*compSum &
    + 2*(vcMInv**eta)*molesInv2*(vcij**eta)*Tcij

  end function TcMDiff2NiNj

  !----------------------------------------------------------------------
  !> Calculate the first order derivative of critical mixing volume, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   \left(\frac{\partial v_{cM}}{\partial n_i} \right)_{T,V} = -\frac{2}{n}v_{cM} + \frac{2}{n^2} \sum_l n_l v_{c,il}
  !> \f]
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function vcMDiffNi(nc,comp,nMoles,vcM,moles,i)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    real, intent(in) :: nMoles(nc), vcM, moles
    integer, intent(in) :: i
    integer :: l
    real :: vcMDiffNi, vci, vcl, vcil, molesInv, v1

    molesInv = 1.0/moles
    v1 = 0.0

    vci = Rgas*comp(i)%p_comp%zc*comp(i)%p_comp%tc/comp(i)%p_comp%pc
    do l=1,nc
      vcl = Rgas*comp(l)%p_comp%zc*comp(l)%p_comp%tc/comp(l)%p_comp%pc
      vcil = 0.125*(vci**(1.0/3.0) + vcl**(1.0/3.0))**3
      v1 = v1 + nMoles(l)*vcil
    end do
    vcMDiffNi = 2*molesInv*(molesInv*v1 - vcM)
  end function vcMDiffNi

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of critical mixing volume, with respect to composition,
  !> for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f{align*}{
  !>   \left(\frac{\partial^2 v_cM}{\partial n_i \partial n_j}\right)
  !>   & = \frac{2}{n^2} v_{cM} - \frac{2}{n} \left(\frac{\partial v_{cM}}{\partial n_j}\right) - \frac{4}{n^3}\sum_l n_l v_{c,il} + \frac{2}{n^2} v_{c,ij} \\
  !>   & = \frac{2}{n^2} (v_{c,ij} - v_{cM}) - \frac{2}{n}\left[\left(\frac{\partial v_{cM}}{\partial n_i}\right) + \left(\frac{\partial v_{cM}}{\partial n_j}\right)\right] \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function vcMDiff2NiNj(nc,comp,nMoles,moles,vcM,i,j)
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    real, intent(in) :: nMoles(nc), vcM,moles
    integer, intent(in) :: i,j
    real :: vci, vcj, vcij, vcM_i, vcM_j, molesInv
    real :: vcMDiff2NiNj
    !Functions called: vcMDiffNi

    molesInv = 1.0/moles

    vci = Rgas*comp(i)%p_comp%zc*comp(i)%p_comp%tc/comp(i)%p_comp%pc
    vcj = Rgas*comp(j)%p_comp%zc *comp(j)%p_comp%tc/comp(j)%p_comp%pc
    vcij = (0.125*(vci**(1.0/3.0) + vcj**(1.0/3.0))**3)
    vcM_i = vcMDiffNi(nc,comp,nMoles,vcM,moles,i)
    vcM_j = vcMDiffNi(nc,comp,nMoles,vcM,moles,j)

    vcMDiff2NiNj = 2*molesInv*molesInv*(vcij - vcM) - 2*molesInv*(vcM_i + vcM_j)

  end function vcMDiff2NiNj


  !----------------------------------------------------------------------
  !> Calculate the first order derivative of the pseudo critical mixing pressure,
  !> with respect to composition, for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   \left(\frac{\partial P_{cM}}{\partial n_i}\right)_{T,V} = P_{cM} \left(\frac{1}{z_{cM}} \left(\frac{\partial z_{cM}}{\partial n_i}\right)_{T,V} + \frac{1}{T_{cM}} \left(\frac{\partial T_{cM}}{\partial n_i}\right)_{T,V} - \frac{1}{v_{cM}}
  !> \left(\frac{\partial v_{cM}}{\partial n_i}\right)_{T,V} \right)
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: TcM, vcM, PcM, zcM, wM, nMoles(nc), moles
    integer, intent(in) :: i
    real :: PcMDiffNi
    !Functions called: zcMDiffNi, TcMDiffNi, vcMDiffNi

    PcMDiffNi = (1.0/zcM)*zcMDiffNi(nc,comp,moles,wM,i) + &
         (1.0/TcM)*TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i) - &
         (1.0/vcM)*vcMDiffNi(nc,comp,nMoles,vcM,moles,i)
    PcMDiffNi = PcMDiffNi*PcM

  end function PcMDiffNi


  !----------------------------------------------------------------------
  !> Calculate the first order derivative of the pseudo critical mixing pressure,
  !> with respect to composition, for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f{align*}{
  !>   \left(\frac{\partial^2 P_cM}{\partial n_i \partial n_j}\right)
  !>   & =  \frac{1}{P_{cM}} \left(\frac{\partial P_{cM}}{\partial n_i}\right) \left(\frac{\partial P_{cM}}{\partial n_j}\right) +
  !> P_{cM} \bigg[ -\frac{1}{z_{cM}^2} \left(\frac{\partial z_{cM}}{\partial n_i}\right) \left(\frac{\partial z_{cM}}{\partial n_j}\right) \\
  !>   & \qquad + \frac{1}{z_{cM}} \left(\frac{\partial^2 z_cM}{\partial n_i \partial n_j}\right) - \frac{1}{T_{cM}^2} \left(\frac{\partial T_{cM}}{\partial n_i}\right) \left(\frac{\partial T_{cM}}{\partial n_j}\right) +
  !> \frac{1}{T_{cM}} \left(\frac{\partial^2 T_cM}{\partial n_i \partial n_j}\right) \\
  !>   & \qquad + \frac{1}{v_{cM}^2} \left(\frac{\partial v_{cM}}{\partial n_i}\right) \left(\frac{\partial v_{cM}}{\partial n_j}\right) -
  !> \frac{1}{v_{cM}} \left(\frac{\partial^2 v_cM}{\partial n_i \partial n_j}\right) \bigg] \\
  !> \f}
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function PcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: TcM, vcM, PcM, zcM, wM, nMoles(nc), moles
    integer, intent(in) :: i, j
    real :: vcMInv, TcMInv, PcMInv, zcMInv, PcM_i, PcM_j, zcM_i, zcM_j, TcM_i, TcM_j, vcM_i, vcM_j, zcM_ij, TcM_ij, vcM_ij
    real :: PcMDiff2NiNj
    !Functions called: PcMDiffNi, zcMDiffNi, TcMDiffNi, vcMDiffNi, zcMDiff2NiNj, TcMDiff2NiNj, vcMDiff2NiNj

    vcMInv = 1.0/vcM
    TcMInv = 1.0/TcM
    PcMInv = 1.0/PcM
    zcMInv = 1.0/zcM

    PcM_i = PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    PcM_j = PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,j)
    zcM_i = zcMDiffNi(nc,comp,moles,wM,i)
    zcM_j = zcMDiffNi(nc,comp,moles,wM,j)
    TcM_i = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)
    TcM_j = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,j)
    vcM_i = vcMDiffNi(nc,comp,nMoles,vcM,moles,i)
    vcM_j = vcMDiffNi(nc,comp,nMoles,vcM,moles,j)
    zcM_ij = zcMDiff2NiNj(nc,comp,moles,wM,i,j)
    TcM_ij = TcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,nMoles,moles,i,j)
    vcM_ij = vcMDiff2NiNj(nc,comp,nMoles,moles,vcM,i,j)

    !PcMDiff2NiNj = PcMInv*PcM_i*PcM_j + &
    !     PcM*(-zcMInv*zcMInv*zcM_i*zcM_j + zcMInv*zcM_ij - &
    !     TcMInv*TcMInv*TcM_i*TcM_j &
    !     + TcMInv*TcM_ij + vcMInv*vcM_i*vcM_j - vcMInv*vcM_ij)

    PcMDiff2NiNj = vcMInv*( Rgas*zcM_ij*TcM &
                          + Rgas*zcM_i*TcM_j &
                          + Rgas*zcM_j*TcM_i &
                          + Rgas*zcM*TcM_ij &
                          - vcM_ij*PcM &
                          - vcM_i*PcM_j &
                          - vcM_j*PcM_i )

  end function PcMDiff2NiNj


  !----------------------------------------------------------------------
  !> Calculate the first order derivative of the pseudo critical mixing compressibility,
  !> with respect to composition, for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   \left(\frac{\partial z_{cM}}{\partial n_i}\right)_{T,V} = -0.085 \left(\frac{\partial \omega_{M}}{\partial n_i}\right)_{T,V}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function zcMDiffNi(nc,comp,moles,wM,i)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    real, intent(in) :: moles, wM
    integer, intent(in) :: i
    real :: zcMDiffNi
    !Functions called: wMDiffNi

    zcMDiffNi = -0.085*wMDiffNi(nc,comp,moles,wM,i)

  end function zcMDiffNi

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of the pseudo critical mixing compressibility,
  !> with respect to composition, for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   \left(\frac{\partial^2 z_cM}{\partial n_i \partial n_j}\right) = -0.085 \left(\frac{\partial^2 \omega_M}{\partial n_i \partial n_j}\right)
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function zcMDiff2NiNj(nc,comp,moles,wM,i,j)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    real, intent(in) :: moles, wM
    integer, intent(in) :: i, j
    real :: zcMDiff2NiNj
    !Functins called: wMDiff2NiNj


    zcMDiff2NiNj = -0.085*wMDiff2NiNj(nc,comp,moles,wM,i,j)

  end function zcMDiff2NiNj


  !----------------------------------------------------------------------
  !> Calculate the first order derivative of the pseudo critical acentricity factor,
  !> with respect to composition, for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   \left(\frac{\partial \omega_{M}}{\partial n_i} \right)_{T,V} = \frac{1}{n} (\omega_i - \omega_M)
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function wMDiffNi(nc,comp,moles,wM,i)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    real, intent(in) :: moles, wM
    integer, intent(in) :: i
    real :: wMDiffNi, molesInv

    molesInv = 1.0/moles

    wMDiffNi = molesInv*(comp(i)%p_comp%acf - wM)

  end function wMDiffNi

  !----------------------------------------------------------------------
  !> Calculate the second order derivative of the pseudo critical acentricity factor,
  !> with respect to composition, for fixed temperature and volume.
  !> Help function to be used in finding the derivative of F, with respect to composition.
  !>
  !> \f[
  !>   \left(\frac{\partial^2 \omega_M}{\partial n_i \partial n_j}\right) = -\frac{1}{n} \left[\left(\frac{\partial \omega_{M}}{\partial n_i}\right) + \left(\frac{\partial w_{M}}{\partial n_j}\right) \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function wMDiff2NiNj(nc,comp,moles,wM,i,j)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    real, intent(in) :: moles, wM
    integer, intent(in) :: i, j
    real :: wMDiff2NiNj, molesInv
    !Functins called: wMDiffNi
    molesInv = 1.0/moles

    wMDiff2NiNj = -molesInv*(wMDiffNi(nc,comp,moles,wM,i) + wMDiffNi(nc,comp,moles,wM,j))

  end function wMDiff2NiNj

  !---------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> The derivatives of the compressibility, with respect
  !> to temperature, pressure and composition follow.
  !>
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the derivative of the compressibility, with respect to
  !> temperature, for fixed pressure and composition.
  !>
  !> \f[
  !>   \left( \frac{\partial z}{\partial T} \right)_{P, \textbf{n}} = -z\left[\frac{1}{T} - \frac{\bar{V}_T}{V}\right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function zDiffT(z,T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: z, T, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: zDiffT, VInv
    !Functions called: VDiffT

    VInv = PcM/(moles*Rgas*TcM*vr)
    zDiffT = -z*(1.0/T - VInv* VDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef))

  end function zDiffT

  !----------------------------------------------------------------------
  !> Calculate the derivative of the compressibility, with respect to
  !> pressure, for fixed temperature and composition.
  !>
  !> \f[
  !>   \left( \frac{\partial z}{\partial P} \right)_{T, \textbf{n}} = z \left[ \frac{1}{P} + \frac{1}{V \left(\frac{\partial P}{\partial V}\right)_{T,\textbf{n}}} \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function zDiffP(z,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: z, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: zDiffP, VInv
    !Functions called: PDiffV

    VInv = PcM/(moles*Rgas*TcM*vr)
    zDiffP = z*(1.0/P + VInv/PDiffV(moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef))

  end function zDiffP

  !----------------------------------------------------------------------
  !> Calculate the derivative of the compressibility, with respect to
  !> composition, for fixed temperature and pressure.
  !>
  !> \f[
  !>   \left( \frac{\partial z}{\partial n_i} \right)_{T,P} = - z \left[ \frac{1}{n} - \frac{\bar{V}_i}{V}  \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function zDiffNi(nc,comp,cbeos,z,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: z, moles, Tr, vr, TcM, vcM, PcM, zcM, wM, B, C, D, E, nMoles(nc)
    integer, intent(in) :: i, simpOrRef
    real :: zDiffNi, VInv
    !Functions called: VDiffNi

    VInv = PcM/(moles*Rgas*TcM*vr)
    zDiffNi = z*(VInv *VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)- 1.0/moles)

  end function zDiffNi


  !---------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> The derivatives of the entropy, with respect
  !> to temperature, pressure and composition follow.
  !>
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the derivative of entropy, with respect to
  !> temperature, for fixed pressure and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial S^R(T,P,\textbf{n})}{\partial T}\right)_{P,\textbf{n}} = \bar{V}_T \left(\frac{\partial P}{\partial T}\right)_{V,\textbf{n}}
  !> - R \left[2 \left(\frac{\partial F}{\partial T}\right)_{V,\textbf{n}} + T \left(\frac{\partial^2 F}{\partial T^2}\right)_{V,\textbf{n}} + \frac{n}{T} \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function SDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: T, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: TcMInv
    real :: SDiffT
    !Functions called: VDiffT, PDiffT, FDiffTr, FDiff2Tr
    TcMInv = 1.0/TcM

    SDiffT = VDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)*PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef) &
    - Rgas*(2*TcMInv*FDiffTr(moles,Tr,vr,simpOrRef)+TcMInv*Tr*FDiff2Tr(moles,Tr,vr,simpOrRef) + moles/T)

  end function SDiffT

  !----------------------------------------------------------------------
  !> Calculate the derivative of entropy, with respect to
  !> pressure, for fixed temperature and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial S^R(T,P,\textbf{n})}{\partial P}\right)_{T,\textbf{n}} = \frac{nR}{P} - \bar{V}_T
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function SDiffP(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: T, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: SDiffP
    !Functions called: VDiffT

    SDiffP = moles*Rgas/P - VDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

  end function SDiffP


  !----------------------------------------------------------------------
  !> Calculate the derivative of entropy, with respect to
  !> pressure, for fixed temperature and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial S^R(T,P,\textbf{n})}{\partial n_i}\right)_{T,P} = \bar{V}_i \left(\frac{\partial P}{\partial T}\right)_{V,\textbf{n}} -
  !>   R\left[ \left(\frac{\partial F}{\partial n_i}\right)_{T,V} + T \left(\frac{\partial^2 F}{\partial T \partial n_i}\right)_V + 1 - \ln z \right]
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function SDiffNi(nc,comp,cbeos,T,P,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,z,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T, P, moles, Tr, vr, TcM, vcM, PcM, zcM, wM, B, C, D, E, nMoles(nc),z
    integer, intent(in) :: i, simpOrRef
    real :: SDiffNi
    !Functions called: VDiffNi, PDiffT, FDiffNi, FDiff2TNi

    SDiffNi = VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)*&
         PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef) - &
         Rgas*(FDiffNi(nc,comp,cbeos,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,B,C,D,E,simpOrRef) &
         + T*FDiff2TNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,i,simpOrRef) + 1) + &
         Rgas*log(z)

  end function SDiffNi


  !---------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> The derivatives of the enthalpy, with respect
  !> to temperature, pressure and composition follow.
  !>
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the derivative of enthalpy, with respect to
  !> temperature, for fixed pressure and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial H^R}{\partial T}\right)_{P, \textbf{n}} = \bar{V}_T T \left(\frac{\partial P}{\partial T}\right)_{V,\textbf{n}} -
  !>  RT \left[ 2\left(\frac{\partial F}{\partial T}\right)_{V,\textbf{n}} + T \left(\frac{\partial^2 F}{\partial T^2}\right)_{V,\textbf{n}} + \frac{n}{T} \right]
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function HDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: T, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: HDiffT
    !Functions called: VDiffT, PDiffT, FDiffTr, FDiff2Tr

    HDiffT = VDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)*T*PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef) &
    - Rgas*Tr*(2*FDiffTr(moles,Tr,vr,simpOrRef) + Tr*FDiff2Tr(moles,Tr,vr,simpOrRef) + moles/Tr)

  end function HDiffT

  !----------------------------------------------------------------------
  !> Calculate the derivative of enthalpy, with respect to
  !> pressure, for fixed temperature and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial H^R}{\partial P}\right)_{T, \textbf{n}} = V - T \bar{V}_T
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function HDiffP(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: T, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: HDiffP, V
    !Functions called: VDiffT

    V = moles*Rgas*TcM*vr/PcM
    HDiffP = V - T*VDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

  end function HDiffP

  !----------------------------------------------------------------------
  !> Calculate the derivative of enthalpy, with respect to
  !> composition, for fixed temperature and pressure.
  !>
  !> \f[
  !>   \left(\frac{\partial H^R}{\partial n_i}\right)_{T,P}  = \bar{V}_i T \left(\frac{\partial P}{\partial T}\right)_{V,\textbf{n}} -RT^2 \left(\frac{\partial^2 F}{\partial T \partial n_i}\right)_V - RT
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function HDiffNi(nc,comp,cbeos,T,P,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T, P, moles, Tr, vr, TcM, vcM, PcM, zcM, wM, B, C, D, E, nMoles(nc)
    integer, intent(in) :: i, simpOrRef
    real :: HDiffNi
    !Functions called: VDiffNi, PDiffT, FDiff2TNi

    HDiffNi = VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)*T*&
         PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef) &
         - Rgas*T*T*FDiff2TNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,i,simpOrRef) - Rgas*T

  end function HDiffNi

  !---------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> The derivatives of the logarithm of the fugacity coefficient, with respect
  !> to temperature, pressure and composition follow.
  !>
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the derivative of the logarithmic fugacity coefficients, with respect to
  !> temperature, for fixed pressure and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial \ln \phi_i}{\partial T}\right)_{P, \textbf{n}} = \left(\frac{\partial^2 F}{\partial T \partial n_i}\right)_V + \frac{1}{T} - \frac{\bar{V}_i}{RT}  \left(\frac{\partial P}{\partial T}\right)_{V,\textbf{n}}
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function lnphiDiffT(nc,comp,cbeos,T,P,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T, P, moles, Tr, vr, TcM, vcM, PcM, zcM, wM, B, C, D, E, nMoles(nc)
    integer, intent (in) :: i, simpOrRef
    real :: lnphiDiffT, TInv
    !Functions called: FDiff2TNi, VDiffNi, PDiffT

    TInv = 1.0/T
    lnphiDiffT = FDiff2TNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,i,simpOrRef) + TInv &
                - (TInv/Rgas)*VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef) &
                *PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef)

  end function lnphiDiffT


  !----------------------------------------------------------------------
  !> Calculate the derivative of the logarithmic fugacity coefficients, with respect to
  !> pressure, for fixed temperature and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial \ln \phi_i}{\partial P}\right)_{T, \textbf{n}} = \frac{\bar{V}_i}{RT} - \frac{1}{P}
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function lnphiDiffP(nc,comp,cbeos,T,P,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: T, P, moles, Tr, vr, TcM, vcM, PcM, zcM, wM, B, C, D, E, nMoles(nc)
    integer, intent(in) :: i, simpOrRef
    real :: lnphiDiffP
    !Functions called: VDiffNi

    lnphiDiffP = (1.0/(Rgas*T))*VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef) - (1.0/P)

  end function lnphiDiffP


  !----------------------------------------------------------------------
  !> Calculate the derivative of the logarithmic fugacity coefficients, with respect to
  !> composition, for fixed temperature and pressure.
  !>
  !> \f[
  !>   \left(\frac{\partial \ln \phi_i}{\partial n_i}\right)_{T, P}  = \left(\frac{\partial^2 F}{\partial n_j \partial n_i}\right)_{T,P} + \frac{1}{n} + \frac{\left(\frac{\partial P}{\partial V}\right)_{T,\textbf{n}}}{RT} \bar{V}_j \bar{V}_i
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function lnphiDiffNj(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,&
       B,C,D,E,nMoles,i,j,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: moles, Tr, vr, TcM, vcM, PcM, zcM, wM, &
         B, C, D, E, nMoles(nc)
    integer, intent(in) :: i, j, simpOrRef
    real :: lnphiDiffNj
    !Functions called: FDiff2NiNj, PDiffV, VDiffNi

    lnphiDiffNj = FDiff2NiNj(nc,comp,cbeos,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j,B,C,D,E,simpOrRef) + &
    (1.0/(Rgas*TcM*Tr)) * PDiffV(moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef) * &
    VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef) * &
    VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,j,simpOrRef) + 1/moles

  end function lnphiDiffNj



  !---------------------------------------------------------------------------------------------------------
  !----------------------------------------------------------------------
  !> Help functions to calculate the derivatives of the entropy, enthalpy and logarithm of the fugacity
  !> coeficients.
  !>
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !> Calculate the derivative of Helmholts reduced recidual function, with respect to
  !> composition and temperature.
  !> Volume is fixed.
  !>
  !> \f{align*}{
  !>   F_{iT} = \left(\frac{\partial^2 F}{\partial T \partial n_i}\right)_{V}
  !>   & =  F_{NT} + F_{NX}X_{T} + F_{NY}Y_{T} \\
  !>   & \quad + (F_{X T} + F_{XX} X_{T} + F_{XY} Y_{T}) X_{i} + F_{X}X_{iT} \\
  !>   & \quad + (F_{Y T} + F_{YX} X_{T} + F_{YY} Y_{T}) Y_{i} + F_{Y}Y_{iT}  \\
  !> \f}
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function FDiff2TNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: moles, Tr, vr, TcM, vcM, PcM, zcM, wM, nMoles(nc)
    integer, intent(in) :: i, simpOrRef
    real :: FDiff2TNi
    real :: F_TrN, Tr_T, F_TrVr, vr_i, F_TrTr, Tr_i, F_Tr, Tr_TNi
    !Functions called: FDiff2TrN, FDiff2TrVr, FDiff2Tr, FDiffTr, TrDiffNi, vrDiffNi

    F_TrN = FDiff2TrN(moles,Tr,vr,simpOrRef)
    F_TrVr = FDiff2TrVr(moles,Tr,vr,simpOrRef)
    F_TrTr = FDiff2Tr(moles,Tr,vr,simpOrRef)
    F_Tr = FDiffTr(moles,Tr,vr,simpOrRef)
    Tr_T = 1.0/TcM
    Tr_i = TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i)
    Tr_TNi = (Tr_T/Tr)*Tr_i
    vr_i = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)

    FDiff2TNi = Tr_T*(F_TrN + F_TrVr*vr_i + F_TrTr*Tr_i) + F_Tr*Tr_TNi

  end function FDiff2TNi
  !----------------------------------------------------------------------
  !> Calculate the derivative of Helmholts reduced recidual function, with respect to
  !> reduced temperature and reduced specific volume.
  !> Composition is fixed.
  !>
  !> \f[
  !>   F_{X Y} = \left(\frac{\partial^2 F}{\partial T_r \partial v_r}\right)_{n} = - n \left[ \frac{B_{T_r}}{v_r^2} + \frac{C_{T_r}}{v_r^3} +
  !> \frac{D_{T_r}}{v_r^6} + \frac{E_{T_r}}{v_r^3} \left( \beta + \frac{\gamma}{v_r^2} \right)\exp \left(-\frac{\gamma}{v_r^2} \right) \right]
  !> \f]
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function FDiff2TrVr(moles,Tr,vr,simpOrRef)

    real, intent(in) :: Tr, vr, moles
    integer, intent(in) :: simpOrRef
    real :: B_Tr, C_Tr, D_Tr, E_Tr, vrInv, vrInv2, vrInv3, vrInv6
    real :: FDiff2TrVr
    real :: be, ga

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)
    call TrCoeffDiff1(Tr,B_Tr,C_Tr,D_Tr,E_Tr,simpOrRef)

    vrInv = 1.0/vr
    vrInv2 = vrInv*vrInv
    vrInv3 = vrInv*vrInv2
    vrInv6 = vrInv3*vrInv3

    FDiff2TrVr = -moles*(B_Tr*vrInv2 + C_Tr*vrInv3 + D_Tr*vrInv6 + E_Tr*vrInv3*(be + ga*vrInv2)*exp(-ga*vrInv2))

  end function FDiff2TrVr

  !----------------------------------------------------------------------
  !> Calculate the derivative of Helmholts reduced recidual function, with respect to
  !> composition and volume.
  !> Temperature is fixed.
  !>
  !> \f{align*}{
  !>   F_{iV} = \left(\frac{\partial^2 F}{\partial V \partial n_i}\right)_{T}
  !>   =  F_{NV} + F_{NX}X_{V} + F_{NY}Y_{V} \\
  !>   & \quad + (F_{X V} + F_{XX} X_{V} + F_{XY} Y_{V}) X_{i} + F_{X}X_{iV} \\
  !>   & \quad + (F_{Y V} + F_{YX} X_{V} + F_{YY} Y_{V}) Y_{i} + F_{Y}Y_{iV}  \\
  !> \f}
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function FDiff2VNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,B,C,D,E,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: moles, Tr, vr, TcM, vcM, PcM, zcM, wM, nMoles(nc), B, C, D, E
    integer, intent(in) :: i, simpOrRef
    real :: FDiff2VNi
    real :: F_VrN, vr_V, F_VrVr, vr_i, F_TrVr, Tr_i, vr_VNi, F_Vr
    !Functions called: FDiff2VrN, FDiff2Vr, FDiffVr, FDiff2TrVr, vrDiffNi, TrDiffNi
    F_VrN = FDiff2VrN(moles,vr,B,C,D,E,simpOrRef)
    F_VrVr = FDiff2Vr(moles,vr,B,C,D,E,simpOrRef)
    F_Vr = FDiffVr(moles,vr,B,C,D,E,simpOrRef)
    F_TrVr = FDiff2TrVr(moles,Tr,vr,simpOrRef)
    vr_V = PcM/(moles*Rgas*TcM)
    vr_i = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    vr_VNi = (vr_V/vr)*vr_i
    Tr_i = TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i)

    FDiff2VNi = vr_V*(F_VrN + F_VrVr*vr_i + F_TrVr*Tr_i) + F_Vr*vr_VNi

  end function FDiff2VNi


  !----------------------------------------------------------------------
  !> Calculate the derivative of the volume, with respect to composition,
  !> For fixed temperature and pressure.
  !> Help function that simplifies notation.
  !>
  !> \f[
  !>   \bar{V}_i \equiv \left(\frac{\partial V}{\partial n_i}\right)_{T,P} =  - \frac{\left(\frac{\partial P}{\partial n_i}\right)_{T,V}}{\left(\frac{\partial P}{\partial V}\right)_{T,\textbf{n}}}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function VDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: moles, Tr, vr, TcM, vcM, PcM, zcM, wM, B, C, D, E, nMoles(nc)
    integer, intent(in) :: i, simpOrRef
    real :: VDiffNi
    !Functions called: PDiffNi, PDiffV

    VDiffNi = -PDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef) &
    /PDiffV(moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

  end function VDiffNi


  !----------------------------------------------------------------------
  !> Calculate the derivative of the volume, with respect to temperature,
  !> For fixed pressire and composition.
  !> Help function that simplifies notation.
  !>
  !> \f[
  !>   \bar{V}_T \equiv \left(\frac{\partial V}{\partial T}\right)_{P,\textbf{n}} =  - \frac{\left(\frac{\partial P}{\partial T}\right)_{V,\textbf{n}}}{\left(\frac{\partial P}{\partial V}\right)_{T,\textbf{n}}}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function VDiffT(T,P,moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: T, P, moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: VDiffT
    !Functions called: PDiffT, PDiffV

    VDiffT = -PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef)/PDiffV(moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

  end function VDiffT

  !---------------------------------------------------------------------------------------------------------
  !> Pressure P = P(T,V,n)
  !> Derivatives of P with respect to temperature, volume and composition
  !> are used when finding several of the derivatives of the thermodynamic
  !> properties of interest. The functions for these derivatives follow.
  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  !> Calculate the derivative of the pressure, with respect to temperature,
  !> for fixed volume and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial P}{\partial T}\right)_{V, \textbf{n}} = \frac{P}{T} - RT \left(\frac{\partial^2 F}{\partial T \partial V}\right)_{n_i} \\
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function PDiffT(T,P,moles,Tr,vr,TcM,PcM,simpOrRef)

    real, intent(in) :: T, P, moles, Tr, vr, PcM, TcM
    integer, intent(in) :: simpOrRef
    real :: PDiffT, FDiff2TV
    !Functions called: FDiff2TrVr

    FDiff2TV = PcM/(moles*Rgas*TcM*TcM) * FDiff2TrVr(moles,Tr,vr,simpOrRef)
    PDiffT = P/T - Rgas*T*FDiff2TV

  end function PDiffT

  !----------------------------------------------------------------------
  !> Calculate the derivative of the pressure, with respect to volume,
  !> for fixed temperature and composition.
  !>
  !> \f[
  !>   \left(\frac{\partial P}{\partial V}\right)_{T, \textbf{n}} = -RT \left(\frac{\partial^2 F}{\partial V^2}\right)_{T, \textbf{n}} - \frac{nRT}{V^2}
  !> \f]
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function PDiffV(moles,Tr,vr,TcM,PcM,B,C,D,E,simpOrRef)

    real, intent(in) :: moles, Tr, vr, TcM, PcM, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: vfac, V
    real :: PDiffV
    !Functions called: FDiff2Vr

    vfac = PcM/(moles*Rgas*TcM)
    V = vr/vfac

    PDiffV = -Rgas*TcM*Tr*vfac*vfac*FDiff2Vr(moles,vr,B,C,D,E,simpOrRef) - moles*Rgas*TcM*Tr/(V*V)

  end function PDiffV

  !----------------------------------------------------------------------
  !> Calculate the derivative of the pressure, with respect to composition,
  !> for fixed temperature and volume.
  !>
  !> \f[
  !>   \left(\frac{\partial P}{\partial n_i}\right)_{T,V} = -RT \left(\frac{\partial^2 F}{\partial V \partial n_i}\right)_T + \frac{RT}{V}
  !> \f]
  !>
  !> \author JA, 2013-06
  !----------------------------------------------------------------------

  function PDiffNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,B,C,D,E,nMoles,i,simpOrRef)
     use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: moles, Tr, vr, TcM, vcM, PcM, zcM,wM, B, C, D, E, nMoles(nc)
    integer, intent(in) :: i, simpOrRef
    real :: PDiffNi
    !Functions called: FDiff2VNi

    PDiffNi = -Rgas*TcM*Tr*FDiff2VNi(nc,comp,cbeos,moles,Tr,vr,TcM,vcM,PcM,zcM,wM,nMoles,B,C,D,E,i,simpOrRef) &
    + PcM*Tr/(moles*vr)

  end function PDiffNi

  !---------------------------------------------------------------------------------------------------------
  !> The following subroutine is a routine that helps solving the non-linear
  !> equation of state (i.e. finding the reduced volume) by calculationg an
  !> appropriate initial value to be used in the numerical method. A bound for
  !> the allowed value of the reduced specific volume is also found.
  !>
  !> \author Ander, 2007, adapted by JA, 2013-07
  !----------------------------------------------------------------------

  subroutine vrInitial(Pr,Tr,usedPhase,simpOrRef,vrInit,vrMin,vrMax)
    implicit none
    !     Upodated Apr 2007 to work up to critical point
    !     See worksheet: C:\TwoPhase\COTT\TPLIB\AdaptingVMinMax.xls
    !
    !     Calcualtes initial valuwe for vr
    !     Assume it is at evaporating pressure and uses adapted equations
    !     for Volume and Z
    !     Find also minimum and maaximum values
    !     For liquid Pr(Tr,vr) = Z*Tr/vr  have local minimum for vr = vrMax
    !     For gas Pr(Tr,vr) have local maximum for vr = vrMin
    !     The Curves for vr and Z is adapted in the area 0.3<Tr<1
    !     The curves for vrMax and vrMin is adapted in area 0.4 < Tr < 1

    real, intent(in)    :: Pr, Tr
    integer, intent(in) :: usedPhase, simpOrRef
    real, intent(out)   :: vrInit, vrMin, vrMax
    real :: TrNew, Z, vr,  x

    TrNew = Max(Tr, 0.4)      !Adapted curves down to Tr = 0.4

    if (TrNew < 1 ) then
      x = 1.0 - TrNew
      if (usedPhase==1) then
        vrMin = 0.05
        if (simpOrRef == 2) then
          vr = (0.0507597 + 0.0203290*TrNew - 0.0684645*TrNew**2)      &
                       /(1.0 - 0.9898632*TrNew)
          vrMax =  0.260004781 - 0.243586952*x**0.299442804 +     &
                       x*(0.153866542 + x*(-0.24318588 + x*0.18830192))
        else
          vr = (0.0589257 + 0.0293946*TrNew - 0.0852730*TrNew**2)      &
                        /(1 - 0.9895063*TrNew)
          vrMax = 0.29357705 - 0.52850447 * x**0.410968967        &
                        + 0.77547108 * x**0.756622288 - 0.462790587*x &
                        + 0.001906586 *x*x
        endif
        if (vr >= vrMax-0.001) then
          vr = vrMax- 0.001
        endif
      else                   ! if Gas
        vrMax = 1E10
        if (simpOrRef == 2) then
          Z =  (0.9503235  - 0.5999993*TrNew  - 0.3009715*TrNew**2)  &
                       /(1.0 - 0.8485730*TrNew)
          vrMin = 0.260004781 + 1.135875238 *x**0.536171251   &
                       + 165.7526983 * x**8.546977142       &
                       + x*x*(5.710834142+1.369944361*x+15.33422908*x*x)
        else
          Z = (1.0352651  - 0.9144101*TrNew  - 0.0706954 *TrNew**2)  &
                        /(1 - 0.8544288 *TrNew)
          vrMin = 0.29357705 + 0.93528691 *x**0.54719415   &
                        + 41.00382077 *x**7.496078769      &
                        + x*x*(2.479091038 + 3.90973725 *x)
        endif               !Assume Z changes less than vr:
        vr = TrNew * Z / Pr
        if (vr < vrMin + 0.01 )then
          vr = vrMin + 0.01
        endif
      endif
    else                      ! if TrNew >= 1
      vrMin = 0.05
      vrMax = 1E10
      if (usedPhase==1) then
        if (simpOrRef == 2) then
          vr = 0.2588817
        else
          vr = 0.2903920
        endif
      else
        if (simpOrRef == 2) then
          Z = 0.3259176
        else
          Z = 0.3445706
        endif
        vr = TrNew * Z / Pr
      endif
    endif
    vrInit =  vr

  end subroutine vrInitial

  !---------------------------------------------------------------------------------------------------------
  !> Optional subroutines and functions.
  !> These arenot called upon automatically, hence the thermProps
  !> routine should be modified to call upon them, if this output is desired.
  !---------------------------------------------------------------------------------------------------------
  !----------------------------------------------------------------------
  !> The two following subroutines are help routines to generate neat output
  !> of large amounts of data. Output is used to get an intuitive understanding
  !> of the shape of the compressibility z and the function fv (of which the
  !> roots are found by the Newton-Rapson method), respectfully.
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  subroutine zPRTshape(T,Tr,B,C,D,E,simpOrRef)

    real, intent(in) :: T,Tr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: z, Pr, vr, ga, be
    integer :: i
    integer :: output = 20
    character(len=2) :: foo
    character(len=4) :: bar ='.txt'
    character(len = 9) :: fileout
    integer :: Tint

    Tint = int(T)
    ga = gamma(simpOrRef)
    be = beta(simpOrRef)

    if (simpOrRef == 2) then
      foo = 'rT'
    else
      foo = 'sT'
    end if

    write (fileout, 20) foo,Tint,bar
    20 format(A2,I3,A4)

    open(unit=output,file=fileout,action='write')
    write (output,*) '##           vr               Pr      '

    do i = 0,100000
      vr = 0.015 + i*1E-4
      z = (1 + B/vr + C/(vr*vr) + D/(vr**5) + E/(vr*vr)*(be + ga/(vr*vr))*exp(-ga/(vr*vr)))
      Pr = z*Tr/vr
      write (output, 1) vr,pr
      1  format(2f25.16)
    end do

  end subroutine zPRTshape

  !----------------------------------------------------------------------

  subroutine fvShape(T,P,Tr,Pr,B,C,D,E,simpOrRef)

    real, intent(in):: T, P, Tr, Pr, B, C, D, E
    integer, intent(in):: simpOrRef
    real :: f, vr
    integer :: i, Tint, Pint
    integer :: output = 20
    character(len=5) :: foo
    character(len=4) :: bar ='.txt'
    character(len=20) :: fileout

    Tint = int(T)
    if (simpOrRef == 2) then
      foo = 'FV2rT'
    else
      foo = 'FV2sT'
    end if
    Pint = int(P)
    write (fileout, 20) foo,Tint,'P',Pint,bar
    20 format(A5,I3,A1,I7,A4)

    open(unit=output,file=fileout,action='write')

    write (output,*) '## Tr = ',Tr, 'Pr =', Pr
    write (output,*) '##           vr               fv      '

    do i = 0,200000
      vr = 0.010 + i*5*1E-4
      f =fv(Pr,Tr,vr,B,C,D,E,simpOrRef)
      write (output, 1) vr,f
      1  format(2f30.16)
    end do

  end subroutine fvShape


  !----------------------------------------------------------------------
  !> Calculate the reduced pressure, Pr, by directly solving the
  !> Lee Keslers equation of state, given reduced temperature,
  !> reduced volume and composition.
  !>
  !> \author JA, 2013-07
  !----------------------------------------------------------------------

  function PrSolver(Tr,vr,B,C,D,E,simpOrRef)

    real, intent(in) :: Tr, vr, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: vrInv, vrInv2, vrInv5
    real :: PrSolver
    real :: be, ga

    be = beta(simpOrRef)
    ga = gamma(simpOrRef)

    vrInv = 1.0/vr
    vrInv2 = vrInv*vrInv
    vrInv5 = vrInv2*vrInv2*vrInv

    PrSolver = Tr*vrInv*(1 + B*vrInv + C*vrInv2 + D*vrInv5 + E*vrInv2*(be + ga*vrInv2)*exp(-ga*vrInv2))

  end function PrSolver

  !----------------------------------------------------------------------
  !> Calculate the reduced volume, vr, by use of the Newton-Rapson numerical
  !! method to solve Lee Keslers equation of state, given reduced temperature,
  !! reduced pressure and composition.The half step method is used to secure that
  !! vr is inside its limit.
  !! Calls upon functions fz, fzDiff and fzDiff2 acting as the function f, derivative
  !! of f and second derivative in the numerical method.
  !!
  !! \f[
  !!   x_{n+1} = x_{n} - \frac{f(x_{n})}{f'(x_{n})}\left(1+\frac{f(x_{n})f''(x_{n})}{2 f'(x_{n})^2}\right)
  !! \f]
  !!
  !! A Taylor expansion of f is used, and the root giving the smallest change in x is used.
  !! To simplify the step, the following series approximation is used,
  !!
  !! \f[
  !!   1 - \sqrt{1 - \alpha} = \frac{\alpha}{2} + \frac{\alpha^2}{8} + O(\alpha^3).
  !! \f]
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  subroutine zNewtRaps(Tr,Pr,phase,simpOrRef,vr,zInit,zMax,zMin)
    use numconstants, only: machine_prec
    use thermopack_constants
    implicit none
    real, intent(in)::Tr, Pr
    integer, intent(in) :: phase, simpOrRef
    real, intent(out) :: vr
    real, intent(in) :: zInit
    real, intent(inout) :: zMax,zMin
    ! Locals
    real :: zTemp, f, fDiff, B, C, D, E, fDiff2, fold, zold
    real :: z, dz, delta
    real, parameter :: tolerance = machine_prec*200.0
    integer, parameter :: maxIterations = 200
    integer :: i

    call TrCoeff(Tr,B,C,D,E,simpOrRef)
    !call vrInitial(Pr,Tr,usedPhase,simpOrRef,vr,vrMin,vrMax)



    z = zInit
    f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)

    ! Debug
    ! open(file='z.dat',unit=12)
    ! write(12,*) '#F(z)'
    ! write(12,*) '#z (-)',char(9),'F (-)'
    ! do i=1,10000
    !   !z = zmin + (zmax - zmin)*real(i-1)/real(10000-1)
    !   z = zmin + (zmin*1000.0 - zmin)*real(i-1)/real(10000-1)
    !   f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
    !   write(12,*) z, f
    !   print *,f
    ! enddo
    ! close(12)
    ! call exit(1)

    do i = 1,maxIterations
      !fDiff = fzDiff(Pr,Tr,z,B,C,D,E,simpOrRef)
      call fzWithDiff(Pr,Tr,z,B,C,D,E,simpOrRef,f,fDiff,fDiff2)
      if (fDiff < 0.0) then ! There is no root
        if (phase == 1) then
          ! Restart, and find gas phase solution
          z = zmax
        else
          ! Restart, and find liquid phase solution
          z = zmin
        endif
        !f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
        !fDiff = fzDiff(Pr,Tr,z,B,C,D,E,simpOrRef)
        call fzWithDiff(Pr,Tr,z,B,C,D,E,simpOrRef,f,fDiff,fDiff2)
      endif
      !fDiff2 = fzDiff2(Pr,Tr,z,B,C,D,E,simpOrRef)
      ! Take the smalest change in dz
      delta = 0.5*f*fDiff2/fDiff**2
      dz = -(f/fDiff)*(1.0+delta)
      zTemp = z + dz

      ! Debug
      !print *,(zTemp-z)/z,(abs(zTemp-z)/z)/tolerance
      if(abs(zTemp-z)/z < tolerance) then
        z = zTemp
        exit
      end if

      zold = z
      if (zTemp > zMax) then
        if (z < zMax) then
          z = (z+zMax)*0.5
        else
          z = (z+zMin)*0.5
        endif
      else if (zTemp < zMin) then
        if (z > zMin) then
          z = (z+zMin)*0.5
        else
          z = (z+zMax)*0.5
        endif
      else
        z = zTemp
      end if

      fold = f
      f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
      if (fold*f < 0.0 .and. abs(f) >= 0.9*abs(fold)) then
        z = 0.5*(zold+z)
        f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
      endif
      if(f < 0) then
        zMin = z
      else
        zMax = z
      end if

    end do

    if (i >= maxIterations .and. .not. continueOnError) then
      print *,'Error:'
      print *,'Tr = ', Tr
      print *,'Pr = ', Pr
      print *,'Phase = ', phase
      print *,'Simp/Ref = ', simpOrRef
      call stoperror('Lee-Kesler solver did not converge...')
    endif

    vr = z*Tr/Pr

  end subroutine zNewtRaps

  !----------------------------------------------------------------------
  !> Find phase minima/maxima of the Lee Keslers equation of state. If
  !! f(z) = 0 have a solution an interval for the solution is returned.
  !! If the search don't cross f(z) = 0, the minima/maxima is returned.
  !! Calls upon functions fz, fzDiff and fzDiff2 acting as the function f, derivative
  !! of f and second derivative in the numerical method.
  !!
  !! \f[
  !!   z_{n+1} = z_{n} - \frac{f'(z_{n})}{f''(x_{n})}
  !! \f]
  !!
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  subroutine zInitial(Tr,Pr,phase,simpOrRef,z,zmin,zmax,solved,hasPhase)
    use numconstants, only: machine_prec
    use thermopack_constants
    implicit none
    real, intent(in)::Tr, Pr
    integer, intent(in) :: phase, simpOrRef
    real, intent(out) :: z,zmin,zmax
    logical, intent(out) :: solved, hasPhase
    ! Locals
    real :: zTemp, f, fDiff, B, C, D, E, fDiff2, fold, zold
    real :: dz, delta, zdmax, zdmin, fDiffold
    real, parameter :: tolerance = machine_prec*200.0
    integer, parameter :: maxIterations = 200
    integer, parameter :: maxLinesr = 2
    integer, parameter :: safetySteps = 2
    integer :: i, j

    solved = .false.
    hasPhase = .false.
    call TrCoeff(Tr,B,C,D,E,simpOrRef)
    !call vrInitial(Pr,Tr,usedPhase,simpOrRef,vr,vrMin,vrMax)

    zmin = Pr*vrMinimum/Tr
    zmax = Pr*vrMaximum/Tr
    zdmin = zmin
    zdmax = zmax
    if (phase == 1) then ! Liquid
      z = zmin ! Start from liquid side
    else ! Gas
      z = 1.0
    endif
    if (rootSelection == 1) then
      ! No phase analysis
      return
    endif

    f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
    if (f < 0.0 .and. phase == 2) then
      ! Increase z to give positive f
      z = z - 2.0*f
      f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
    endif
    zold = z*2.0
    fold = f
    fDiff = 1.0e100

    ! Debug
!     open(file='z.dat',unit=12)
!     write(12,*) '#F(z)'
!     write(12,*) '#z (-)',char(9),'F (-)',char(9),'dFdz (-)'
!     do i=1,10000
!       !z = zmin + (zmax - zmin)*real(i-1)/real(10000-1)
!       z = zmin + (zmin*1000.0 - zmin)*real(i-1)/real(10000-1)
!       f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
!       fDiff = fzDiff(Pr,Tr,z,B,C,D,E,simpOrRef)
!       write(12,*) z, f, fDiff
!       print *, z, f, fDiff
!     enddo
!     close(12)
!     call exit(1)

    do i = 1,maxIterations
      fDiffold = fDiff
      call fzWithDiff(Pr,Tr,z,B,C,D,E,simpOrRef,f,fDiff,fDiff2)
      if (phase == 2) then
        if (abs(fDiff) > abs(fDiffOld)) then
          ! Have overshooten the solution. Need to backtrack
          z = 0.25*z+0.75*zold
          call fzWithDiff(Pr,Tr,z,B,C,D,E,simpOrRef,f,fDiff,fDiff2)
        endif
      endif
      if (fDiff >= 0.0) then
        if (f == 0.0) then
          zMin = z
          zMax = z
        else if(f < 0) then
          zMin = z
        else
          zMax = z
        end if
      else
        if (phase == 1) then
          zMax = z
        else
          zMin = z
        endif
      endif

      if (f*fold <= 0.0) then
        if (phase == 2) then ! Minima
          z = zMax
        else
          z = zMin
        endif
        hasPhase = .true.
        return
      else
        fold = f
      endif

      if (phase == 1) then
        if (fDiff < 0.0) then ! Passed minima
          zdmax = z
        else
          zdmin = z
        endif
      else
        if (fDiff < 0.0) then ! Passed minima
          zdmin = z
        else
          zdmax = z
        endif
      endif
      if (phase == 2 .and. i <= safetySteps) then
        delta = 0.5*f*fDiff2/fDiff**2
        dz = -(f/fDiff)*(1.0+delta)
      else
        dz = max(-fDiff/fDiff2, -z*0.33)
      endif
      zTemp = z + dz

      ! Debug
      !print *,(zTemp-z)/z,(abs(zTemp-z)/z)/tolerance
      if(abs(zTemp-z)/z < tolerance .or. abs(fDiff) < tolerance) then
        z = zTemp
        if (f*fold > 0.0) then
          solved = .true.
        endif
        exit
      end if

      zold = z
      if (zTemp > zdMax) then
        if (z < zdMax) then
          z = (z+zdMax)*0.5
        else
          z = (z+zdMin)*0.5
        endif
      else if (zTemp < zdMin) then
        if (z > zdMin) then
          z = (z+zdMin)*0.5
        else
          z = (z+zdMax)*0.5
        endif
      else
        z = zTemp
      end if
      dz = z - zold

      fold = f
      f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
      if (rootSelection == 2) then
        if (phase == 2 .and. i > safetySteps) then ! Minima
          do j=1,maxLinesr
            if (f > fold) then
              z = zold + dz*0.5**j
              f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
            else
              exit
            endif
          enddo
        else if (phase == 1) then ! Maxima
          do j=1,maxLinesr
            if (f < fold) then
              z = zold + dz*0.5**j
              f = fz(Pr,Tr,z,B,C,D,E,simpOrRef)
            else
              exit
            endif
          enddo
        endif
      endif
    end do

    if (i >= maxIterations .and. .not. continueOnError) then
      print *,'Error:'
      print *,'Tr = ', Tr
      print *,'Pr = ', Pr
      print *,'Phase = ', phase
      print *,'Simp/Ref = ', simpOrRef
      call stoperror('Lee-Kesler minima solver did not converge...')
    endif

  end subroutine zInitial


  !----------------------------------------------------------------------
  !> Calculate the function f to be used in the numerical method to find
  !! compressibility, given reduced pressure, reduced temperature, coefficients B - E
  !! and an increasingly more correct compressibility, for each call.
  !!
  !! \f[
  !!   f = \frac{v_r P_r}{T_r} - \left[1 - \frac{v_r}{n} \left( \frac{\partial F}{\partial v_r} \right)_{T_r, n} \right] = 0
  !! \f]
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  function fz(Pr,Tr,z,B,C,D,E,simpOrRef)

    real, intent(in) :: Pr, Tr, z, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: fz, vr

    vr = z*Tr/Pr
    fz = z - (1 - vr*FDiffVr(1.0,vr,B,C,D,E,simpOrRef))

  end function fz

  !----------------------------------------------------------------------
  !> Calculate the derivative of function f to be used in the numerical method to find
  !! compressibillity.
  !!
  !! \f[
  !!   \left(\frac{\partial f}{\partial z}\right)_{T_r,P_r, \textbf{n}} = 1 + \frac{Tr}{Pr n} \left[
  !!   \left( \frac{\partial F}{\partial v_r} \right)_{T_r, n} + v_r \left( \frac{\partial^2 F}{\partial v_r^2} \right)_{T_r, n} \right]
  !! \f]
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  function fzDiff(Pr,Tr,z,B,C,D,E,simpOrRef)

    real, intent(in) :: Pr, Tr, z, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: fzDiff
    real :: vr

    vr = z*Tr/Pr
    fzDiff = 1 + (Tr/Pr)*(FDiffVr(1.0,vr,B,C,D,E,simpOrRef) + vr*FDiff2Vr(1.0,vr,B,C,D,E,simpOrRef))

  end function fzDiff

  !----------------------------------------------------------------------
  !> Calculate the second derivative of function f to be used in the numerical method to find
  !! compressibility.
  !!
  !! \f[
  !!   \left(\frac{\partial^2 f}{\partial z^2}\right)_{T_r,P_r, \textbf{n}} = \frac{Tr^2}{Pr^2 n} \left[
  !!   2 \left( \frac{\partial^2 F}{\partial v_r^2} \right)_{T_r, n} + v_r \left( \frac{\partial^3 F}{\partial v_r^3} \right)_{T_r, n} \right]
  !! \f]
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  function fzDiff2(Pr,Tr,z,B,C,D,E,simpOrRef)

    real, intent(in) :: Pr, Tr, z, B, C, D, E
    integer, intent(in) :: simpOrRef
    real :: fzDiff2
    real :: vr

    vr = z*Tr/Pr
    fzDiff2 = (Tr/Pr)**2*(2.0*FDiff2Vr(1.0,vr,B,C,D,E,simpOrRef) + vr*FDiff3Vr(1.0,vr,B,C,D,E,simpOrRef))

  end function fzDiff2

  !----------------------------------------------------------------------
  !> Calculate the function f to be used in the numerical method to find
  !> compressibillity. Differentials is also calculated, see fz, fzDiff
  !> and fzDiff2 for details
  !>
  !> \author MH, 2013-09
  !----------------------------------------------------------------------
  subroutine fzWithDiff(Pr,Tr,z,B,C,D,E,simpOrRef,fz,fzd,fzd2)

    real, intent(in) :: Pr, Tr, z, B, C, D, E
    integer, intent(in) :: simpOrRef
    real, intent(out) :: fz,fzd,fzd2
    ! Locals
    real :: vr, FdVr, Fd2Vr, Fd3Vr

    vr = z*Tr/Pr
    FdVr = FDiffVr(1.0,vr,B,C,D,E,simpOrRef)
    Fd2Vr = FDiff2Vr(1.0,vr,B,C,D,E,simpOrRef)
    Fd3Vr = FDiff3Vr(1.0,vr,B,C,D,E,simpOrRef)

    fz = z - (1 - vr*FdVr)
    fzd = 1 + (Tr/Pr)*(FdVr + vr*Fd2Vr)
    fzd2 = (Tr/Pr)**2*(2.0*Fd2Vr + vr*Fd3Vr)

  end subroutine fzWithDiff

  !----------------------------------------------------------------------
  !> Test differentials of the Lee Kesler eos. Takes temperature, pressure,
  !> composition and phase as input, and test compressibility, entropy departure,
  !> enthalpy departure and fugacity coefficients differentials.
  !>
  !> \author Morten H, 2013-09
  !----------------------------------------------------------------------
  subroutine testDiffLeeKesler(nc,comp,cbeos,Tin,Pin,Z,phase)
    use eosdata
    use compdata, only: gendata_pointer
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc) :: comp
    class(cb_eos), intent(in) :: cbeos
    real, intent(in)                :: Tin, Pin
    real, dimension(nc), intent(in) :: Z
    integer, intent(in) :: phase
    ! Locals
    real, dimension(nc) :: nMoles,lnphi,lnphi2,dlnfdt,dlnfdp
    real, dimension(nc,nc) :: dlnfdz,dlnfdz_num
    real :: zfac,zfact,zfacp,zfacn1,t,p
    real :: dZdt,dZdp,entropy,dsdt,dsdp,enthalpy,dhdt,dhdp,dgdt,dgdp,g
    real :: st,sp,sn1,ht,hp,hn1,gt,gp,gn1
    real, dimension(nc) :: dZdz,dsdz,dhdz,dZdz_num,dsdz_num,dhdz_num,gn_num
    integer :: i

    ! integer :: j
    ! real :: hn2,zfacn2,sn2,v
    ! real :: F_0, F_Tr_0, F_TrTr_0, F_TrN_0, Tr_j_0, Tr_i_0, Tr_ij_0
    ! real :: F_Vr_0, F_VrVr_0, F_VrN_0, vr_j_0, vr_i_0, vr_ij_0, F_TrVr_0
    ! real :: vr, moles, molesInv, B,C,D,E
    ! real :: TcM,vcM,PcM,zcM,wM,Tr
    ! integer :: simpOrRef
    ! real :: F_1, F_Tr_1, F_Vr_1, F_N_0, F_N_1, Tr_1, Tr_i_1
    ! real :: Tr_ij_0_vec(nc,nc), vr_ij_0_vec(nc,nc)
    ! real :: Tr_i_0_vec(nc), vr_i_0_vec(nc)
    ! real :: Tr_i_1_vec(nc), vr_i_1_vec(nc)
    ! real :: Tr_ij_0_vecN(nc,nc), vr_ij_0_vecN(nc,nc)
    ! real :: Tr_1_vec(nc), vr_1_vec(nc)
    ! real :: TcM0, vcM0, PcM0, zcM0, TcM_i_1, bigV, vr_1, vr_i_1, PcM_i_1, v0, dTr
    ! real :: TcM_i_0(nc), TcM_ij_0(nc,nc), PcM_i_0(nc), PcM_ij_0(nc,nc)

    nMoles = Z
    p = Pin
    t = Tin
    ! v0 = 0.10990341828649056
    ! v = v0
    ! vr = v
    ! simpOrRef = 1
    ! moles = sum(Z)

    ! call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,z,moles)
    ! TcM0 = TcM
    ! vcM0 = vcM
    ! PcM0 = PcM
    ! zcM0 = zcM
    ! Tr = T/TcM
    ! bigV = moles*vr*Rgas*TcM/PcM
    ! ! Find solution for reference fluid
    ! call TrCoeff(Tr,B,C,D,E,simpOrRef)
    ! ! F_NN = 0
    ! F_0 = FSolver(moles,vr,B,C,D,E,simpOrRef)
    ! F_N_0 = F_0/moles
    ! F_Tr_0 = FDiffTr(moles,Tr,vr,simpOrRef)
    ! F_TrTr_0 = FDiff2Tr(moles,Tr,vr,simpOrRef)
    ! F_TrN_0 = FDiff2TrN(moles,Tr,vr,simpOrRef)
    ! F_Vr_0 = FDiffVr(moles,vr,B,C,D,E,simpOrRef)
    ! F_VrVr_0 = FDiff2Vr(moles,vr,B,C,D,E,simpOrRef)
    ! F_VrN_0 = FDiff2VrN(moles,vr,B,C,D,E,simpOrRef)
    ! F_TrVr_0 = FDiff2TrVr(moles,Tr,vr,simpOrRef)
    ! do i = 1,nc
    !   Tr_i_0_vec(i) = TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i)
    !   vr_i_0_vec(i) = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    !   TcM_i_0(i) = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)
    !   PcM_i_0(i) = PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    !   do j = 1,nc
    !     Tr_ij_0_vec(i,j) = TrDiff2NiNj(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i,j)
    !     vr_ij_0_vec(i,j) = vrDiff2NiNj(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,&
    !          nMoles,moles,i,j)
    !     TcM_ij_0(i,j) = TcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,nMoles,moles,i,j)
    !     PcM_ij_0(i,j) = PcMDiff2NiNj(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i,j)
    !   enddo
    ! enddo

    ! ! Perturbate in Tr
    ! dTr = Tr*1.0e-5
    ! Tr = Tr + dTr
    ! call TrCoeff(Tr,B,C,D,E,simpOrRef)
    ! ! F_NN = 0
    ! F_1 = FSolver(moles,vr,B,C,D,E,simpOrRef)
    ! F_N_1 = F_1/moles
    ! F_Tr_1 = FDiffTr(moles,Tr,vr,simpOrRef)
    ! F_Vr_1 = FDiffVr(moles,vr,B,C,D,E,simpOrRef)
    ! print *,'F_Tr',(F_1-F_0)/(Tr*1.0e-5),F_Tr_0,&
    !      abs(((F_1-F_0)/(dTr)-F_Tr_0)/F_Tr_0)
    ! print *,'F_TrTr',(F_Tr_1-F_Tr_0)/(Tr*1.0e-5),F_TrTr_0,&
    !      abs(((F_Tr_1-F_Tr_0)/(dTr)-F_TrTr_0)/F_TrTr_0)
    ! print *,'F_TrVr',(F_Vr_1-F_Vr_0)/(dTr),F_TrVr_0,&
    !      abs(((F_Vr_1-F_Vr_0)/(dTr)-F_TrVr_0)/F_TrVr_0)
    ! print *,'F_TrN',(F_N_1-F_N_0)/(dTr),F_TrN_0,&
    !      abs(((F_N_1-F_N_0)/(dTr)-F_TrN_0)/F_TrN_0)
    ! Tr = T/TcM
    ! call TrCoeff(Tr,B,C,D,E,simpOrRef)

    ! ! Perturbate in vr
    ! vr = v0 + v0*1.0e-5
    ! ! F_NN = 0
    ! F_1 = FSolver(moles,vr,B,C,D,E,simpOrRef)
    ! F_N_1 = F_1/moles
    ! F_Tr_1 = FDiffTr(moles,Tr,vr,simpOrRef)
    ! F_Vr_1 = FDiffVr(moles,vr,B,C,D,E,simpOrRef)
    ! print *,'F_vr',(F_1-F_0)/(vr*1.0e-5),F_vr_0,&
    !      abs(((F_1-F_0)/(v0*1.0e-5)-F_vr_0)/F_vr_0)
    ! print *,'F_vrvr',(F_vr_1-F_vr_0)/(v0*1.0e-5),F_vrvr_0,&
    !      abs(((F_vr_1-F_vr_0)/(v0*1.0e-5)-F_vrvr_0)/F_vrvr_0)
    ! print *,'F_Trvr',(F_Tr_1-F_Tr_0)/(vr*1.0e-5),F_TrVr_0,&
    !      abs(((F_Tr_1-F_Tr_0)/(v0*1.0e-5)-F_TrVr_0)/F_TrVr_0)
    ! print *,'F_vrN',(F_N_1-F_N_0)/(v0*1.0e-5),F_VrN_0,&
    !      abs(((F_N_1-F_N_0)/(V0*1.0e-5)-F_VrN_0)/F_VrN_0)
    ! vr = v

    ! ! Perturbate in mole numbers
    ! do j = 1,nc
    !   nMoles = Z
    !   nMoles(j) = nMoles(j) + nMoles(j)*1.0e-5
    !   moles = sum(nMoles)
    !   call mixRules(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles)
    !   Tr_1 = T/TcM
    !   vr_1 = bigV*PcM/(moles*Rgas*TcM)
    !   !print *,'Tr_i',j,(Tr_1-Tr)/(z(j)*1.0e-5),Tr_i_0_vec(j),&
    !   !     abs(((Tr_1-Tr)/(z(j)*1.0e-5)-Tr_i_0_vec(j))/Tr_i_0_vec(j))
    !   !print *,'vr_i',j,(vr_1-vr)/(z(j)*1.0e-5),vr_i_0_vec(j),&
    !   !     abs(((vr_1-vr)/(z(j)*1.0e-5)-vr_i_0_vec(j))/vr_i_0_vec(j))

    !   !print *,'PcM_i',j,(PcM-PcM0)/(z(j)*1.0e-5),PcM_i_0(j),&
    !   !     abs(((PcM-PcM0)/(z(j)*1.0e-5)-PcM_i_0(j))/PcM_i_0(j))

    !   !print *,'TcM_i',j,(TcM-TcM0)/(z(j)*1.0e-5),TcM_i_0(j),&
    !   !     abs(((TcM-TcM0)/(z(j)*1.0e-5)-TcM_i_0(j))/TcM_i_0(j))
    !   !vr_1_vec(j) =
    !   !Tr_i_1_vec(i) = TrDiffNi(nc,comp,cbeos,Tr,TcM,vcM,nMoles,moles,i)
    !   !vr_i_1_vec(i) = vrDiffNi(nc,comp,cbeos,vr,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    !   do i = 1,nc
    !     Tr_i_1 = TrDiffNi(nc,comp,cbeos,Tr_1,TcM,vcM,nMoles,moles,i)
    !     TcM_i_1 = TcMDiffNi(nc,comp,cbeos,TcM,vcM,nMoles,moles,i)
    !     vr_i_1 = vrDiffNi(nc,comp,cbeos,vr_1,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    !     PcM_i_1 = PcMDiffNi(nc,comp,cbeos,TcM,vcM,PcM,zcM,wM,nMoles,moles,i)
    !     !print *,'Tr_ij',i,j,(Tr_i_1-Tr_i_0_vec(i))/(z(j)*1.0e-5),&
    !     !     Tr_ij_0_vec(i,j),&
    !     !     abs(((Tr_i_1-Tr_i_0_vec(i))/(z(j)*1.0e-5)-Tr_ij_0_vec(i,j))/Tr_ij_0_vec(i,j))
    !     !print *,'vr_ij',i,j,(vr_i_1-vr_i_0_vec(i))/(z(j)*1.0e-5),&
    !     !     vr_ij_0_vec(i,j),&
    !     !     abs(((vr_i_1-vr_i_0_vec(i))/(z(j)*1.0e-5)-vr_ij_0_vec(i,j))/vr_ij_0_vec(i,j))
    !     !print *,'TcM_ij',i,j,(TcM_i_1-TcM_i_0(i))/(z(j)*1.0e-5),&
    !     !     TcM_ij_0(i,j),&
    !     !     abs(((TcM_i_1-TcM_i_0(i))/(z(j)*1.0e-5)-TcM_ij_0(i,j))/TcM_ij_0(i,j))
    !     !print *,'PcM_ij',i,j,(PcM_i_1-PcM_i_0(i))/(z(j)*1.0e-5),&
    !     !     PcM_ij_0(i,j),&
    !     !     abs(((PcM_i_1-PcM_i_0(i))/(z(j)*1.0e-5)-PcM_ij_0(i,j))/PcM_ij_0(i,j))
    !   enddo
    ! enddo
    ! nMoles = Z

    call lkCalcFug(nc,comp,cbeos,T,p,nMoles,phase,lnphi,dlnfdt,dlnfdp,dlnfdz)
    call lkCalcZfac(nc,comp,cbeos,T,p,nMoles,phase,Zfac,dZdt,dZdp,dZdz)
    call lkCalcEntropy(nc,comp,cbeos,T,p,nMoles,phase,entropy,dsdt,dsdp,dsdz)
    call lkCalcEnthalpy(nc,comp,cbeos,T,p,nMoles,phase,enthalpy,dhdt,dhdp,dhdz)
    call lkCalcGdep(nc,comp,cbeos,T,P,nMoles,phase,g,dgdt,dgdp)

    print *
    print *,'Fugacity'
    print *,'Temperature'
    t = Tin + Tin*1.0e-5
    call lkCalcFug(nc,comp,cbeos,T,p,nMoles,phase,lnphi2)
    call lkCalcZfac(nc,comp,cbeos,T,p,nMoles,phase,Zfact)
    call lkCalcEntropy(nc,comp,cbeos,T,p,nMoles,phase,st)
    call lkCalcEnthalpy(nc,comp,cbeos,T,p,nMoles,phase,ht)
    call lkCalcGdep(nc,comp,cbeos,T,P,nMoles,phase,gt)
    !print *,(lnphi2-lnphi)/(t*1.0e-5)
    !print *,dlnfdt
    print *,abs(((lnphi2-lnphi)/(Tin*1.0e-5)-dlnfdt)/dlnfdt)

    print *,'Pressure'
    t = Tin
    p = Pin + Pin*1.0e-5
    call lkCalcFug(nc,comp,cbeos,T,p,nMoles,phase,lnphi2)
    call lkCalcZfac(nc,comp,cbeos,T,p,nMoles,phase,Zfacp)
    call lkCalcEntropy(nc,comp,cbeos,T,p,nMoles,phase,sp)
    call lkCalcEnthalpy(nc,comp,cbeos,T,p,nMoles,phase,hp)
    call lkCalcGdep(nc,comp,cbeos,T,P,nMoles,phase,gp)
    !print *,(lnphi2-lnphi)/(p*1.0e-5)
    !print *,dlnfdp
    print *,abs(((lnphi2-lnphi)/(Pin*1.0e-5)-dlnfdp)/dlnfdp)

    print *,'Mole numbers'
    p = Pin
    do i=1,nc
      nMoles = Z
      nMoles(i) = nMoles(i) + nMoles(i)*1.0e-5
      call lkCalcFug(nc,comp,cbeos,T,p,nMoles,phase,lnphi2)
      call lkCalcZfac(nc,comp,cbeos,T,p,nMoles,phase,Zfacn1)
      call lkCalcEntropy(nc,comp,cbeos,T,p,nMoles,phase,sn1)
      call lkCalcEnthalpy(nc,comp,cbeos,T,p,nMoles,phase,hn1)
      call lkCalcGdep(nc,comp,cbeos,T,P,nMoles,phase,gn1)
      dlnfdz_num(:,i) = (lnphi2-lnphi)/(Z(i)*1.0e-5)
      dZdz_num(i) = (Zfacn1-Zfac)/(Z(i)*1.0e-5)
      dsdz_num(i) = (sn1-entropy)/(Z(i)*1.0e-5)
      dhdz_num(i) = (hn1-enthalpy)/(Z(i)*1.0e-5)
      gn_num(i) = (gn1-g)/(Z(i)*1.0e-5)
      print *,'dlnphi_num',i,dlnfdz_num(:,i)
      print *,'dlnphi',i,dlnfdz(:,i)
      print *,'dlnphi rel',i,':',abs((dlnfdz_num(:,i)-dlnfdz(:,i))/dlnfdz(:,i))
    enddo
    nMoles = Z

    print *
    print *,'ZFac'
    print *,'Temperature'
    !print *,(zfact-zfac)/(Tin*1.0e-5)
    !print *,dZdt
    print *,abs(((zfact-zfac)/(Tin*1.0e-5)-dZdt)/dZdt)
    print *,'Pressure'
    !print *,(zfacp-zfac)/(Pin*1.0e-5)
    !print *,dZdp
    print *,abs(((zfacp-zfac)/(Pin*1.0e-5)-dZdp)/dZdp)
    print *,'Mole Numbers'
    !print *,dZdz_num
    !print *,dZdz
    print *,abs((dZdz_num-dZdz)/dZdz)

    print *
    print *,'Entropy'
    print *,'Temperature'
    !print *,(st-entropy)/(Tin*1.0e-5)
    !print *,dsdt
    print *,abs(((st-entropy)/(Tin*1.0e-5)-dsdt)/dsdt)
    print *,'Pressure'
    !print *,(sp-entropy)/(Pin*1.0e-5)
    !print *,dsdp
    print *,abs(((sp-entropy)/(Pin*1.0e-5)-dsdp)/dsdp)
    print *,'Mole Numbers'
    !print *,dsdz_num
    !print *,dsdz
    print *,abs((dsdz_num-dsdz)/dsdz)

    print *
    print *,'Enthalpy'
    print *,'Temperature'
    !print *,(ht-enthalpy)/(Tin*1.0e-5)
    !print *,dhdt
    print *,abs(((ht-enthalpy)/(Tin*1.0e-5)-dhdt)/dhdt)
    print *,'Pressure'
    !print *,(hp-enthalpy)/(Pin*1.0e-5)
    !print *,dhdp
    print *,abs(((hp-enthalpy)/(Pin*1.0e-5)-dhdp)/dhdp)
    print *,'Mole Numbers'
    !print *,dhdz_num
    !print *,dhdz
    print *,abs((dhdz_num-dhdz)/dhdz)

    print *
    print *,'Gibbs free energy'
    print *,'Temperature'
    !print *,(gt-g)/(Tin*1.0e-5)
    !print *,dgdt
    print *,abs(((gt-g)/(Tin*1.0e-5)-dgdt)/dgdt)
    print *,'Pressure'
    !print *,(gp-g)/(Pin*1.0e-5)
    !print *,dgdp
    print *,abs(((gp-g)/(Pin*1.0e-5)-dgdp)/dgdp)
    print *,'g-(h-t*s)'
    !print *,g-(enthalpy-T*entropy)
    print *,abs((g-(enthalpy-T*entropy))/g)
    print *,'sum(n*mu)*R*T'
    !print *,sum(nMoles*lnphi)*Rgas*T,g
    print *,abs((sum(nMoles*lnphi)*Rgas*T-g)/g)
    print *,'Mole Numbers'
    !print *,gn_num
    !print *,lnphi*Rgas*T
    print *,abs((gn_num-lnphi*Rgas*T)/(lnphi*Rgas*T))

  end subroutine testDiffLeeKesler


  !----------------------------------------------------------------------
  !> Calculate the reduced volume, vr, by use of the Newton-Rapson numerical
  !! method to solve Lee Keslers equation of state, given reduced temperature,
  !! reduced pressure and composition.The half step method is used to secure that
  !! vr is inside its limit.
  !! Calls upon functions fz, fzDiff and fzDiff2 acting as the function f, derivative
  !! of f and second derivative in the numerical method.
  !!
  !! \f[
  !!   x_{n+1} = x_{n} - \frac{f(x_{n})}{f'(x_{n})}\left(1+\frac{f(x_{n})f''(x_{n})}{f'(x_{n})^2}\right)
  !! \f]
  !!
  !! A Taylor expansion of f is used, and the root giving the smallest change in x is used.
  !! To simplify the step, the following series approximation is used,
  !!
  !! \f[
  !!   1 - \sqrt{1 - \alpha} = \frac{\alpha}{2} + \frac{\alpha^2}{8} + O(\alpha^3).
  !! \f]
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  subroutine calcReducedVolume(Tr,Pr,phase,vrSimp,vrRef)
    implicit none
    real, intent(in)::Tr, Pr
    integer, intent(in) :: phase
    real, intent(out) :: vrSimp, vrRef
    ! Locals
    real :: zSimp,zMinSimp,zMaxSimp,zRef,zMinRef,zMaxRef
    real :: z,zMax,zMin
    logical :: solvedSimp,solvedRef,hasPhaseSimp,hasPhaseRef
    integer :: altPhase, phaseSimp, phaseRef

    ! Simple fluid
    call zInitial(Tr,Pr,phase,1,zSimp,zMinSimp,zMaxSimp,solvedSimp,hasPhaseSimp)
    ! Reference fluid
    call zInitial(Tr,Pr,phase,2,zRef,zMinRef,zMaxRef,solvedRef,hasPhaseRef)

    select case (rootSelection)
    case (1)
      call zNewtRaps(Tr,Pr,phase,1,vrSimp,zSimp,zMaxSimp,zMinSimp)
      call zNewtRaps(Tr,Pr,phase,2,vrRef,zRef,zMaxRef,zMinRef)
    case (2)
      if (solvedSimp) then
        vrSimp = zSimp*Tr/Pr
      else
        call zNewtRaps(Tr,Pr,phase,1,vrSimp,zSimp,zMaxSimp,zMinSimp)
      endif
      if (solvedRef) then
        vrRef = zRef*Tr/Pr
      else
        call zNewtRaps(Tr,Pr,phase,2,vrRef,zRef,zMaxRef,zMinRef)
      endif
    case (3)
      phaseSimp = phase
      phaseRef = phase
      if (phase == 1) then
          altPhase = 2
        else
          altPhase = 1
        endif
      if (hasPhaseSimp .neqv. hasPhaseRef) then
        if (.not. hasPhaseSimp) then
          phaseSimp = altPhase
          call setMaxMinZ(Tr,Pr,phaseSimp,zSimp,zMaxSimp,zMinSimp)
          z = zRef
          zMax = zMaxRef
          zMin = zMinRef
          call zInitial(Tr,Pr,altphase,2,zRef,zMinRef,zMaxRef,solvedRef,hasPhaseRef)
          if (hasPhaseRef) then
            phaseRef = altPhase
          else
            ! No common phase - take the available root from each equation
            ! Restore info for reference phase
            zRef = z
            zMaxRef = zMax
            zMinRef = zMin
            ! print *,'Error:'
            ! print *,'Tr = ', Tr
            ! print *,'Pr = ', Pr
            ! print *,'Phase = ', phase
            ! call StopError('Lee-Kesler: No common phase')
          endif
        else
          phaseRef = altPhase
          call setMaxMinZ(Tr,Pr,phaseRef,zRef,zMaxRef,zMinRef)
          z = zSimp
          zMax = zMaxSimp
          zMin = zMinSimp
          call zInitial(Tr,Pr,altphase,1,zSimp,zMinSimp,zMaxSimp,solvedSimp,hasPhaseSimp)
          if (hasPhaseSimp) then
            phaseSimp = altPhase
          else
            ! No common phase - take the available root from each equation
            ! Restore info for simple phase
            zSimp = z
            zMaxSimp = zMax
            zMinSimp = zMin
            ! print *,'Error:'
            ! print *,'Tr = ', Tr
            ! print *,'Pr = ', Pr
            ! print *,'Phase = ', phase
            ! call StopError('Lee-Kesler: No common phase')
          endif
        endif
      else if (.not. hasPhaseSimp .and. .not. hasPhaseRef) then
        phaseSimp = altPhase
        call setMaxMinZ(Tr,Pr,phaseSimp,zSimp,zMaxSimp,zMinSimp)
        phaseRef = altPhase
        zMinRef = zMinSimp
        zMaxRef = zMaxSimp
        zRef = zSimp
      endif
      call zNewtRaps(Tr,Pr,phaseSimp,1,vrSimp,zSimp,zMaxSimp,zMinSimp)
      call zNewtRaps(Tr,Pr,phaseRef,2,vrRef,zRef,zMaxRef,zMinRef)
    end select

  end subroutine calcReducedVolume

  !----------------------------------------------------------------------
  !> Set limits for compressibillity factor, and initial value based on
  !! phase flag
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------
  subroutine setMaxMinZ(Tr,Pr,phase,z,zMax,zMin)
    implicit none
    real, intent(in)::Tr, Pr
    integer, intent(in) :: phase
    real, intent(out) :: z,zMax,zMin
    ! Locals
    zMin = Pr*vrMinimum/Tr
    zMax = Pr*vrMaximum/Tr
    if (phase == 1) then ! Liquid
      z = zMin ! Start from liquid side
    else ! Gas
      z = zMax
    endif
  end subroutine setMaxMinZ


  !----------------------------------------------------------------------
  !> Dump data to file for plotting
  !>
  !> \author MH, 2013-09
  !----------------------------------------------------------------------
  subroutine fixedTrPlot(vrLow,vrHigh,Tr,outFile)
    implicit none
    real, intent(in) :: Tr, vrLow, vrHigh
    character(len=*), optional, intent(in) :: outFile
    ! Locals
    integer, parameter :: n = 1000
    real :: vr, PrSimp, PrRef, zSimp, zRef
    integer :: i

    ! Debug
    if (present(outFile)) then
      open(file=trim(outfile),unit=12)
    else
      open(file='lk.dat',unit=12)
    endif
    write(12,*) '#LK; Tr = ',Tr
    write(12,*) '#vr (-)',char(9),'PrSimp (-)',char(9),'zSimp (-)',&
         char(9),'PrRef (-)',char(9),'zRef (-)'
    do i=1,n
      vr = vrLow + (vrHigh - vrLow)*real(i-1)/real(n-1)
      PrSimp = Pred(Tr,vr,1)
      zSimp = vr*PrSimp/Tr
      PrRef = Pred(Tr,vr,2)
      zRef = vr*PrRef/Tr
      write(12,*) vr,PrSimp,zSimp,PrRef,zRef
    enddo
    close(12)
  end subroutine fixedTrPlot

  !----------------------------------------------------------------------
  !> Calculate the reduced pressure given reduzed temperature and
  !! reduced volume:
  !!
  !! \f[
  !!   Pr = \frac{T_r}{v_r}\left[1 - \frac{v_r}{n} \left( \frac{\partial F}{\partial v_r} \right)_{T_r, n} \right]
  !! \f]
  !!
  !! \author MH, 2013-09
  !----------------------------------------------------------------------

  function Pred(Tr,vr,simpOrRef)

    real, intent(in) :: Tr, vr
    integer, intent(in) :: simpOrRef
    real :: Pred
    ! Locals
    real :: B, C, D, E

    call TrCoeff(Tr,B,C,D,E,simpOrRef)
    Pred = (Tr/vr)*(1 - vr*FDiffVr(1.0,vr,B,C,D,E,simpOrRef))

  end function Pred

end module LeeKesler
