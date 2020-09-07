MODULE mbwr_additional
  use thermopack_constants, ONLY: Rgas ! [Rgas] = Pa*m^3/(mol*K)
CONTAINS

  SUBROUTINE alphar_deltaCoef(opt,Tr,deltaCoef,model)
    USE mbwr, ONLY: eosmbwr, nijlarray, allocNIJL
    implicit none
    !input variables
    integer, intent(in) :: opt
    real, intent(in) :: Tr
    type(eosmbwr), intent(in) :: model
    !output variables
    real, dimension(:), intent(out) :: deltaCoef
    !local variables
    type(NIJLarray) :: c
    integer :: k, upper

    upper = model%HelmLength !22 or 40
    call allocNIJL(c,upper)
    c = model%redResHelmCoeff_redrhoT
    if (opt .eq. 1) then ! zeroth derivative wrt Tr
       do k=1,upper
          deltaCoef(k) = c%N(k)*Tr**(c%J(k))
       end do
    elseif (opt .eq. 2) then ! first derivative wrt Tr
       do k=1,upper
          deltaCoef(k) = c%J(k)*c%N(k)*Tr**(c%J(k)-1)
       end do
    else !opt = 3. Second derivative wrt Tr
       do k=1,upper
          deltaCoef(k) = c%J(k)*(c%J(k)-1)*c%N(k)*Tr**(c%J(k)-2)
       end do
    end if

  END SUBROUTINE alphar_deltaCoef


  SUBROUTINE alphar_deltaDerivatives(deriv,nRhoDerivs,t,tDependentCoef,model)
    USE mbwr, ONLY: eosmbwr, nijlarray, allocNIJL
    implicit none
    !input variables
    REAL, INTENT(IN) :: t
    real, dimension(:), intent(in) :: tDependentCoef
    type(eosmbwr), intent(in) :: model
    integer, intent(in) :: nRhoDerivs
    !output variables
    real, dimension(3), intent(out) :: deriv ! the zeroth, first and second derivative of alphar wrt t
    !local variables
    integer :: k ! iteration variable
    real, dimension(3) :: poly, expo
    real :: gamma, pled, eled
    type(NIJLarray) :: c
    real :: t_1, t_2, exponential, twoGamma, twoGammaT

    ! rename variables for readability
    call allocNIJL(c,model%HelmLength)
    c = model%redResHelmCoeff_redrhoT
    gamma = c%gamma ! crucial to use this gamma, and not model%gamma.

    ! precalculate quantities for speed
    t_1 = 1/t
    t_2 = t_1*t_1
    twoGamma = 2*gamma
    twoGammaT = twoGamma*t
    exponential = exp(-gamma*t*t)

    poly = 0
    do k = 1,model%Helm_poly_len
       pled = tDependentCoef(k)*t**(c%I(k))
       poly(1) = poly(1) + pled
       if (nRhoDerivs .ge. 2) poly(2) = poly(2) + c%I(k)*pled
       if (nRhoDerivs .ge. 3) poly(3) = poly(3) + c%I(k)*(c%I(k)-1)*pled
    end do

    expo = 0
    do k = model%Helm_poly_len+1,model%HelmLength
       eled = tDependentCoef(k)*t**(c%I(k))
       expo(1) = expo(1) + eled
       if (nRhoDerivs .ge. 2) expo(2) = expo(2) + eled*(c%I(k)*t_1 - twoGammaT)
       if (nRhoDerivs .ge. 3) expo(3) = expo(3) &
            + eled*(c%I(k)*(c%I(k)-1)*t_2 + twoGamma*(-1-2*c%I(k)+twoGammaT*t))
    end do

    deriv(1) = poly(1) + expo(1)*exponential
    if (nRhoDerivs .ge. 2) deriv(2) = poly(2)*t_1 + expo(2)*exponential
    if (nRhoDerivs .ge. 3) deriv(3) = poly(3)*t_2 + expo(3)*exponential
  END SUBROUTINE alphar_deltaDerivatives

  !> Compute derivatives of residual molar Helmholtz function alpha_r(T,rho) [A^res(T,V,n)/nRT].
  ! OUTPUT
  !  deriv: a real array of dimension 3, indexed by 0,1,2,
  !  containing the computed derivatives
  ! INPUT
  !  t: temperature (K)
  !  rho: density (mol/m^3)
  !  nTderivs: number of t-derivatives (0,1 or 2)
  !  nRhoderivs: number of rho-derivatives (0,1 or 2)
  !  model: the mbwreos instance
  ! USAGE
  !  Inputting nTderivs=m and nRhoDerivs=n computes
  !  deriv = [d_t^m alpha,...,d_t^m d_rho^n alpha],
  !  while deriv(n+1:2) is not initialized.
  subroutine alphar_derivatives(deriv,t,rho_SI,nTderivs,nRhoDerivs,model)
    USE mbwr, ONLY: eosmbwr, nijlarray, allocNIJL
    implicit none
    !output variables
    real, dimension(0:2), intent(out) :: deriv
    !input variables
    real,intent(in) :: t, rho_SI
    integer, intent(in) :: nTderivs, nRhoDerivs
    type(eosmbwr), intent(in) :: model
    !local variables
    integer :: k ! iteration variable
    real, dimension(0:2) :: poly, expo
    type(NIJLarray) :: c
    real :: gamma
    real :: pled, eled
    real :: rho_1, rho_2, exponential, twoGamma, twoGammaRho
    real :: rho

    rho = rho_SI*1e-3 !< MBWR backend is based on mol/L units

    if ((nTderivs .lt. 0 .or. nTderivs .gt. 2) .or. (nRhoDerivs .lt. 0 .or. nRhoDerivs .gt. 2)) then
      call stoperror("error in number of derivatives")
    end if
    poly = 0
    expo = 0

    ! rename variables for readability
    c = model%redResHelmCoeff_redrhoT
    gamma = c%gamma

    ! precalculate quantities for speed
    rho_1 = 1/rho
    rho_2 = rho_1*rho_1
    exponential = exp(-gamma*rho**2)
    twoGamma = 2*gamma
    twoGammaRho = twoGamma*rho

    if (nTderivs .eq. 0) then
       do k = 1,model%Helm_poly_len
          pled = c%N(k)*T**(c%J(k))*rho**(c%I(k))
          poly(0) = poly(0) + pled
          if (nRhoDerivs .ge. 1) poly(1) = poly(1) + c%I(k)*pled
          if (nRhoDerivs .ge. 2) poly(2) = poly(2) + c%I(k)*(c%I(k)-1)*pled
       end do
       do k = model%Helm_poly_len+1,model%HelmLength
          eled = c%N(k)*T**(c%J(k))*rho**(c%I(k))
          expo(0) = expo(0) + eled
          if (nRhoDerivs .ge. 1) expo(1) = expo(1) + eled*(c%I(k)*rho_1 - twoGammaRho)
          if (nRhoDerivs .ge. 2) expo(2) = expo(2) &
               + eled*(c%I(k)*(c%I(k)-1)*rho_2 + twoGamma*(-1-2*c%I(k)+twoGammaRho*rho))
       end do

    elseif (nTderivs .eq. 1) then
       do k = 1,model%Helm_poly_len
          pled = c%N(k)*c%J(k)*T**(c%J(k)-1)*rho**(c%I(k))
          poly(0) = poly(0) + pled
          if (nRhoDerivs .ge. 1) poly(1) = poly(1) + c%I(k)*pled
          if (nRhoDerivs .ge. 2) poly(2) = poly(2) + c%I(k)*(c%I(k)-1)*pled
       end do
       do k = model%Helm_poly_len+1,model%HelmLength
          eled = c%N(k)*c%J(k)*T**(c%J(k)-1)*rho**(c%I(k))
          expo(0) = expo(0) + eled
          if (nRhoDerivs .ge. 1) expo(1) = expo(1) + eled*(c%I(k)*rho_1 - twoGammaRho)
          if (nRhoDerivs .ge. 2) expo(2) = expo(2) &
               + eled*(c%I(k)*(c%I(k)-1)*rho_2 + twoGamma*(-1-2*c%I(k)+twoGammaRho*rho))
       end do

    else
       do k = 1,model%Helm_poly_len
          pled = c%N(k)*c%J(k)*(c%J(k)-1)*T**(c%J(k)-2)*rho**(c%I(k))
          poly(0) = poly(0) + pled
          if (nRhoDerivs .ge. 1) poly(1) = poly(1) + c%I(k)*pled
          if (nRhoDerivs .ge. 2) poly(2) = poly(2) + c%I(k)*(c%I(k)-1)*pled
       end do
       do k = model%Helm_poly_len+1,model%HelmLength
          eled = c%N(k)*c%J(k)*(c%J(k)-1)*T**(c%J(k)-2)*rho**(c%I(k))
          expo(0) = expo(0) + eled
          if (nRhoDerivs .ge. 1) expo(1) = expo(1) + eled*(c%I(k)*rho_1 - twoGammaRho)
          if (nRhoDerivs .ge. 2) expo(2) = expo(2) &
               + eled*(c%I(k)*(c%I(k)-1)*rho_2 + twoGamma*(-1-2*c%I(k)+twoGammaRho*rho))
       end do
    end if

    deriv(0) = poly(0) + expo(0)*exponential
    if (nRhoDerivs .ge. 1) deriv(1) = (poly(1)*rho_1 + expo(1)*exponential)*1e-3
    if (nRhoDerivs .ge. 2) deriv(2) = (poly(2)*rho_2 + expo(2)*exponential)*1e-6
  end subroutine alphar_derivatives

  !> Outputs volume in m^3/mol
  real function mbwr_volume(T,P,nMoles,phase,model)
    use mbwr, only: eosmbwr, makeParam, MBWR_density
    implicit none
    !input variables
    real, intent(in) :: T !< temperature [K]
    real, intent(in) :: P !< pressure [Pa]
    real, intent(in) :: nMoles !< moles
    integer, intent(in) :: phase
    type(eosmbwr), intent(in) :: model
    !locals
    real :: rho

    real :: param(1+model%bplen+model%belen)
    integer :: phase_found_out
    call makeParam(parameters=param,T=T,model=model,nTderivatives=0)
    rho = MBWR_density(t=T,p=P,phase_in=phase,param=param,model=model,phase_found_out=phase_found_out)

    rho = rho*1e3 ! mol/L -> mol/m^3
    mbwr_volume = nMoles/rho
  end function mbwr_volume


  !> Solves for the density having lowest (residual) Gibbs energy.
  real function MBWR_stableDensity(T,P,model)
    use mbwr, only: eosmbwr, makeParam, MBWR_density
    implicit none
    !input
    real, intent(in) :: T, P
    type(eosmbwr),intent(in) :: model
    !locals
    real :: rho_vap, rho_liq
    real :: Gres_vap, Gres_liq
    integer :: phase_found_out
    real :: param(1+model%bplen+model%belen)
    call makeParam(parameters=param,T=T,model=model,nTderivatives=0)

    rho_vap = MBWR_density(t=T,p=P,phase_in=2,param=param,model=model,phase_found_out=phase_found_out)
    if (phase_found_out == 1) then !only one density solution
       MBWR_stableDensity = rho_vap
       return
    end if
    rho_liq = MBWR_density(t=T,p=P,phase_in=1,param=param,model=model,phase_found_out=phase_found_out)

    call MBWR_Gres(model=model, T=T, P=P, V=1/rho_vap, nMoles=1.0, Gres=Gres_vap)
    call MBWR_Gres(model=model, T=T, P=P, V=1/rho_liq, nMoles=1.0, Gres=Gres_liq)
    if (Gres_vap < Gres_liq) then
       MBWR_stableDensity = rho_vap
    else
       MBWR_stableDensity = rho_liq
    end if
  end function MBWR_stableDensity

  subroutine MBWR_Gres(model, T, P, V, nMoles, Gres)!, dgresdt, dgresdp, dgresdn)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T ! [K]
    real, intent(in) :: P ! [Pa]
    real, intent(in) :: V ! [L]
    real, intent(in) :: nMoles ! [mol]
    type(eosmbwr), intent(in) :: model
    !output
    real, intent(out) :: Gres ! [kJ/mol]
    !local variables
    real :: F_
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    real :: Tr, delta
    Tr = T
    delta = nMoles/(V)
    call alphar_derivatives(res,tr,delta,0,0,model)
    alphaDerivatives(0,0) = res(0)
    F_ = F(T,V,nMoles,alphaDerivatives)
    Gres = Rgas*T*F_ + P*V - nMoles*Rgas*T*(1+log(P*V/(nMoles*Rgas*T)))
  end subroutine MBWR_Gres
  !***************************************************************

  !> Residual entropy [kJ/mol].
  subroutine MBWR_Sres(model, T, P, V, nMoles, Sres, DSresDt, DSresDp, DSresDn)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !output
    real, intent(out) :: Sres
    real, optional, intent(out) :: DSresDT, DSresDP, DSresDn(1)
    !local variables
    real ::F_, DFDT_, DFDn_, D2FDVDn_, D2FDV2_, D2FDTDV_, D2FDTDn_, D2FDT2_, DPDT_, DPDV_, DPDn_, DVDT_, DVDn_
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    real :: Tr, delta
    real :: z
    Tr = T
    delta = nMoles/(V)
    call alphar_derivatives(res,tr,delta,0,2,model)
    alphaDerivatives(0,0:2) = res
    call alphar_derivatives(res,tr,delta,1,1,model)
    alphaDerivatives(1,0:1) = res(0:1)

    call alphar_derivatives(res,tr,delta,2,0,model)
    alphaDerivatives(2,0) = res(0)

    F_ = F(T,V,nMoles,alphaDerivatives)
    DFDT_ = DFDT(T,V,nMoles,alphaDerivatives)
    DFDn_ = DFDn(T,V,nMoles,alphaDerivatives)
    D2FDVDn_ = D2FDVDn(T,V,nMoles,alphaDerivatives)
    D2FDV2_ = D2FDV2(T,V,nMoles,alphaDerivatives)
    D2FDTDV_ = D2FDTDV(T,V,nMoles,alphaDerivatives)
    D2FDTDn_ = D2FDTDn(T,V,nMoles,alphaDerivatives)
    D2FDT2_ = D2FDT2(T,V,nMoles,alphaDerivatives)

    ! Necessary P-derivatives
    DPDT_ = P/T-Rgas*T*D2FDTDV_
    DPDV_ = -Rgas*(T*D2FDV2_ - nMoles*T/V**2)
    DPDn_ = -Rgas*(T*D2FDVDn_ + T/V)

    ! Necessary V-derivatives
    DVDT_ = -DPDT_/DPDV_
    DVDn_ = -DPDn_/DPDV_

    z = P*V/(nMoles*Rgas*T)
    ! Sres and its derivatives
    Sres = Rgas*(- F_ - T*DFDT_ + nMoles*log(z))
    if (present(dsresdt)) DSresDT = DVDT_*DPDT_ - Rgas*(2*DFDT_ + T*D2FDT2_ + nMoles/T)
    if (present(dsresdp)) DSresDP = nMoles*Rgas/P - DVDT_
    if (present(dsresdn)) DSresDn = DVDn_*DPDT_ - Rgas*(DFDn_ + T*D2FDTDn_ + 1 - log(z))
  end subroutine MBWR_Sres
  !***************************************************************

  !> Residual reduced Helmholtz energy.
  subroutine MBWR_Fres(model, T, V, nMoles, Fr, F_T, F_v, F_TT, F_Tv, F_vv, &
       F_n, F_Tn, F_vn, F_nn)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !output
    real, optional, intent(out) :: Fr, F_T, F_v, F_TT, F_Tv, F_vv, &
       F_n, F_Tn, F_vn, F_nn
    !local variables
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    real :: Tr, delta
    Tr = T
    delta = nMoles/(V)

    if (present(F_TT) .OR. &
         present(F_vv) .OR. &
         present(F_nn) .OR. &
         present(F_Tv) .OR. &
         present(F_Tn) .OR. &
         present(F_vn)) then
      ! Second order differentials
      call alphar_derivatives(res,tr,delta,0,2,model)
      alphaDerivatives(0,0:2) = res
      call alphar_derivatives(res,tr,delta,1,2,model)
      alphaDerivatives(1,0:2) = res
      call alphar_derivatives(res,tr,delta,2,2,model)
      alphaDerivatives(2,0:2) = res
    else if (present(F_T) .OR. present(F_v) .OR. present(F_n)) then
      ! First order differentials
      call alphar_derivatives(res,tr,delta,0,1,model)
      alphaDerivatives(0,0:1) = res(0:1)
      call alphar_derivatives(res,tr,delta,1,1,model)
      alphaDerivatives(1,0:1) = res(0:1)
    else ! No differentials
      call alphar_derivatives(res,tr,delta,0,0,model)
      alphaDerivatives(0,0) = res(0)
    endif

    if (present(Fr)) then
      Fr = F(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_T)) then
      F_T = DFDT(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_v)) then
      F_v = DFDV(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_n)) then
      F_n = DFDn(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_vn)) then
      F_vn = D2FDVDn(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_vv)) then
      F_vv = D2FDV2(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_Tv)) then
      F_Tv = D2FDTDV(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_Tn)) then
      F_Tn = D2FDTDn(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_TT)) then
      F_TT = D2FDT2(T,V,nMoles,alphaDerivatives)
    endif
    if (present(F_nn)) then
      F_nn = D2FDn2(T,V,nMoles,alphaDerivatives)
    endif
  end subroutine MBWR_Fres
  !***************************************************************

  !********************* Derivatives of pressure *********************
  subroutine MBWR_press(model, T, v, nMoles, p, dpdv, dpdt)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !output
    real, optional, intent(out) :: p, dpdv, dpdt
    ! Locals
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    integer :: v_diff
    real :: F_v, F_vv, F_Tv, delta, Tr
    if (present(dpdv)) then
      v_diff = 2
    else
      v_diff = 1
    endif
    delta = nMoles/(V)
    Tr = T
    call alphar_derivatives(res,tr,delta,0,v_diff,model)
    alphaDerivatives(0,0:v_diff) = res(0:v_diff)
    F_v = DFDV(T,V,nMoles,alphaDerivatives)

    if (present(p)) then
      p = Rgas*T*(nMoles/V-F_v)
    endif
    if (present(dpdv)) then
      F_vv = D2FDV2(T,V,nMoles,alphaDerivatives)
      dpdv = -Rgas*T*(nMoles/V**2+F_vv)
    endif
    if (present(dpdt)) then
      call alphar_derivatives(res,tr,delta,1,1,model)
      alphaDerivatives(1,0:1) = res(0:1)
      F_Tv = D2FDTDV(T,V,nMoles,alphaDerivatives)
      dpdt = Rgas*(nMoles/V-F_v) - Rgas*T*F_Tv
    endif
  end subroutine MBWR_press
  !***************************************************************

  !********************* Derivatives of Hres *********************
  subroutine MBWR_Hres(model, T, P, V, nMoles, Hres, DHresDT, DHresDP, DHresDn)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !output
    real, intent(out) :: Hres
    real, optional, intent(out) :: DHresDT, DHresDP, DHresDn(1)
    !local variables
    real :: DFDT_, D2FDVDn_, D2FDV2_, D2FDTDV_, D2FDT2_, D2FDTDn_, DPDV_, DPDT_, DPDn_, DVDT_, DVDn_
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    real :: Tr, delta

    Tr = T
    delta = nMoles/(V)
    ! Necessary alpha-derivatives
    call alphar_derivatives(res,tr,delta,0,2,model)
    alphaDerivatives(0,0:2) = res

    call alphar_derivatives(res,tr,delta,1,1,model)
    alphaDerivatives(1,0:1) = res(0:1)

    call alphar_derivatives(res,tr,delta,2,0,model)
    alphaDerivatives(2,0) = res(0)

    ! Necessary F-derivatives
    DFDT_ = DFDT(T,V,nMoles,alphaDerivatives)
    D2FDVDn_ = D2FDVDn(T,V,nMoles,alphaDerivatives)
    D2FDV2_ = D2FDV2(T,V,nMoles,alphaDerivatives)
    D2FDTDV_ = D2FDTDV(T,V,nMoles,alphaDerivatives)
    D2FDTDn_ = D2FDTDn(T,V,nMoles,alphaDerivatives)
    D2FDT2_ = D2FDT2(T,V,nMoles,alphaDerivatives)

    ! Necessary P-derivatives
    DPDT_ = P/T-Rgas*T*D2FDTDV_
    DPDV_ = -Rgas*T*(D2FDV2_ + nMoles/V**2)
    DPDn_ = Rgas*T*(-D2FDVDn_ + 1/V)

    ! Necessary V-derivatives
    DVDT_ = -DPDT_/DPDV_
    DVDn_ = -DPDn_/DPDV_
    ! Hres and its derivatives
    Hres = -Rgas*T*T*DFDT_+P*V-nMoles*Rgas*T
    if (present(dhresdt)) DHresDT = T*(DVDT_*DPDT_ - Rgas*(2*DFDT_ + T*D2FDT2_ + nMoles/T))
    if (present(dhresdp)) DHresDP = V-T*DVDT_
    if (present(dhresdn)) DHresDn = T*(DVDn_*DPDT_ - Rgas*(D2FDTDn_*T + 1))
  end subroutine MBWR_Hres
  !***************************************************************


  !********************* Derivatives of z *********************
  subroutine MBWR_zfac(model, T, P, V, nMoles, z, DzDt, DzDp, DzDn)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !output
    real, intent(out) :: z
    real, intent(out), optional :: DzdT, DzDP, DzDn(1)
    !local variables
    real :: D2FDV2_, D2FDTDV_, D2FDVDn_, DPDV_, DPDT_, DPDn_, DVDT_, DVDn_
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    real :: delta, Tr

    ! Necessary alpha-derivatives
    Tr = T
    delta = nMoles/(V)
    call alphar_derivatives(res,tr,delta,0,2,model)
    alphaDerivatives(0,0:2) = res
    call alphar_derivatives(res,tr,delta,1,1,model)
    alphaDerivatives(1,0:2) = res

    D2FDV2_ = D2FDV2(T,V,nMoles,alphaDerivatives)
    D2FDTDV_ = D2FDTDV(T,V,nMoles,alphaDerivatives)
    D2FDVDn_ = D2FDVDn(T,V,nMoles,alphaDerivatives)
    DPDV_ = -Rgas*T*D2FDV2_ - nMoles*Rgas*T/V**2
    DPDT_ = P/T-Rgas*T*D2FDTDV_
    DPDn_ = -Rgas*T*D2FDVDn_ + Rgas*T/V

    DVDT_ = -DPDT_/DPDV_
    DVDn_ = -DPDn_/DPDV_

    ! z and its derivatives
    z = P*V/(nMoles*Rgas*T)
    if (present(dzdt)) DzDT = -z*(1/T-DVDT_/V)
    if (present(dzdp)) DzDP = z*(1/P+1/(V*DPDV_))
    if (present(dzdn)) DzDn = -z*(1/nMoles - DVDn_/V)
  end subroutine MBWR_zfac
  !***************************************************************

  !********************* Derivatives of lnphi ********************
  subroutine MBWR_lnphi(model, T, P, V, nMoles, lnphi, DlnphiDT, DlnphiDP, DlnphiDn)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !output
    real, intent(out) :: lnphi(1)
    real, intent(out), optional :: DlnphiDT(1), DlnphiDP(1), DlnphiDn(1,1)
    !local variables
    real :: D2FDV2_, D2FDn2_, D2FDTDV_, D2FDTDn_, D2FDVDn_, DPDV_, DPDn_, DPDT_, DVDn_
    real, dimension(0:2) :: res
    real, dimension(0:2,0:2) :: alphaDerivatives
    real :: Tr, delta

    ! Necessary alpha-derivatives
    Tr = T
    delta = nMoles/(V)
    call alphar_derivatives(res,tr,delta,0,2,model)
    alphaDerivatives(0,0:2) = res
    call alphar_derivatives(res,tr,delta,1,1,model)
    alphaDerivatives(1,0:2) = res

    ! necessary F-derivatives
    D2FDV2_ = D2FDV2(T,V,nMoles,alphaDerivatives)
    D2FDn2_ = D2FDn2(T,V,nMoles,alphaDerivatives)
    D2FDVDn_ = D2FDVDn(T,V,nMoles,alphaDerivatives)
    D2FDTDV_ = D2FDTDV(T,V,nMoles,alphaDerivatives)
    D2FDTDn_ = D2FDTDn(T,V,nMoles,alphaDerivatives)

    ! necessary P-derivatives
    DPDV_ = -Rgas*T*D2FDV2_ - nMoles*Rgas*T/V**2
    DPDn_ = -Rgas*T*D2FDVDn_ + Rgas*T/V
    DPDT_ = P/T-Rgas*T*D2FDTDV_
    ! necessary V-derivatives
    DVDn_ = -DPDn_/DPDV_
    ! lnphi and its derivatives
    lnphi = alphaDerivatives(0,0) + delta*alphaDerivatives(0,1) - log(P*V/(nMoles*Rgas*T))
    if (present(dlnphidt)) DlnphiDT = D2FDTDn_ + (1 - DVDn_*DPDT_/Rgas)/T
    if (present(dlnphidp)) DlnphiDP = DVDn_/(Rgas*T)-1/P
    if (present(dlnphidn)) DlnphiDn = D2FDn2_ + 1/nMoles + DPDV_*(DVDn_**2)/(Rgas*T)
  end subroutine MBWR_lnphi
  !***************************************************************

  subroutine checkStateFunctionDerivatives(StateFunction,T,P,V,nMoles,model,phase)
    use mbwr, only : eosmbwr, MBWR_coef, MBWR_density, makeParam
    implicit none
    !input
    interface
       subroutine StateFunction(model, T, P, V, nMoles, lnphi, DlnphiDT, DlnphiDP, DlnphiDn)
         use mbwr, only: eosmbwr
         implicit none
         !input
         real, intent(in) :: T
         real, intent(in) :: P
         real, intent(in) :: V
         real, intent(in) :: nMoles
         type(eosmbwr), intent(in) :: model
         real, intent(out) :: lnphi(1)
         real, intent(out), optional ::DlnphiDT(1), DlnphiDP(1), DlnphiDn(1,1)
       end subroutine StateFunction
    end interface
    !    external :: StateFunction
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    integer, intent(inout) :: phase     ! needed in the density solver
    !local variables
    real :: dt, dp, dn
    real :: eps_t, eps_p, eps_n, releps_t, releps_p, releps_n
    real :: b(1), dbdt(1), dbdp(1), dbdn(1,1), b_mod(1), dbdt_mod(1), dbdp_mod(1), dbdn_mod(1,1)
    real :: rho_new, Vnew
    integer :: phase_found_out ! dummy variable
    real, dimension(1+model%bplen+model%belen) :: param

    dt = 1e-5
    dp = 1e-1
    dn = 1e-5

    releps_t = 0.0
    releps_p = 0.0
    releps_n = 0.0

    call StateFunction(model,T,P,V,nMoles,b,dbdt,dbdp,dbdn)
    ! check T-derivative
    call makeParam(param,T+dt,model)
    rho_new = MBWR_density(T+dt,P,phase,param,model,phase_found_out)
    Vnew = nMoles/rho_new
    call StateFunction(model,T+dt,P,Vnew,nMoles,b_mod,dbdt_mod,dbdp_mod,dbdn_mod)
    eps_t = abs((b_mod(1)-b(1))/dt - dbdt(1) )
    if (dbdt(1) .ne. 0.0) releps_t = eps_t/dbdt(1)

    ! check P-derivative
    call makeParam(param,T,model)
    rho_new = MBWR_density(T,P+dp,phase,param,model,phase_found_out)
    Vnew = nMoles/rho_new
    call StateFunction(model,T,P+dp,Vnew,nMoles,b_mod,dbdt_mod,dbdp_mod,dbdn_mod)
    eps_p = abs((b_mod(1)-b(1))/dp - dbdp(1) )
    if (dbdp(1) .ne. 0.0) releps_p = eps_p/dbdp(1)

    ! check n-derivative
    Vnew = (nMoles+dn)/nMoles*V
    call StateFunction(model,T,P,Vnew,nMoles+dn,b_mod,dbdt_mod,dbdp_mod,dbdn_mod)
    eps_n = abs((b_mod(1)-b(1))/dn - dbdn(1,1) )
    if (dbdn(1,1) .ne. 0.0) releps_n = eps_n/dbdn(1,1)

   print *, "STATE FUNCTIONS CHECK"
   print *, "eps, releps, dbd(t,p,n)"
   print *, eps_t, releps_t, dbdt
   print *, eps_p, releps_p, dbdp
   print *, eps_n, releps_n, dbdn
  end subroutine checkStateFunctionDerivatives


  subroutine CheckModelConsistency(T,P,V,nMoles,model)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    !local variables
    real :: Tr, delta
    real, dimension(0:2) :: res
    real :: eps1, eps2, eps3, eps4, eps5, eps6, eps7, eps8, eps9
    real :: z, DzDT, DzDP, DzDn(1), lnphi(1), DlnphiDT(1), DlnphiDP(1), DlnphiDn(1,1)
    real :: Hres, DHresDT, DHresDP, DHresDn(1), Sres, DSresDT, DSresDP, DSresDn(1)
    real :: Gres
    real :: DFDT_, DFDV_, DFDn_, D2FDT2_, D2FDV2_, D2FDn2_, D2FDTDV_, D2FDTDn_, D2FDVDn_
    real :: DPDT_, DPDV_, DPDn_, DVDT_, DVDn_
    real, dimension(0:2,0:2) :: alphaDerivatives

    !initializations
    Tr = T
    delta = nMoles/(V)
    call alphar_derivatives(res,tr,delta,0,2,model)
    alphaDerivatives(0,0:2) = res
    call alphar_derivatives(res,tr,delta,1,2,model)
    alphaDerivatives(1,0:2) = res
    call alphar_derivatives(res,tr,delta,2,2,model)
    alphaDerivatives(2,0:2) = res

    ! necessary F-derivatives
    DFDT_ = DFDT(T,V,nMoles,alphaDerivatives)
    DFDV_ = DFDV(T,V,nMoles,alphaDerivatives)
    DFDn_ = DFDn(T,V,nMoles,alphaDerivatives)
    D2FDVDn_ = D2FDVDn(T,V,nMoles,alphaDerivatives)
    D2FDV2_ = D2FDV2(T,V,nMoles,alphaDerivatives)
    D2FDTDV_ = D2FDTDV(T,V,nMoles,alphaDerivatives)
    D2FDn2_ = D2FDn2(T,V,nMoles,alphaDerivatives)
    D2FDTDn_ = D2FDTDn(T,V,nMoles,alphaDerivatives)
    D2FDT2_ = D2FDT2(T,V,nMoles,alphaDerivatives)
    ! necessary P-derivatives
    DPDT_ = P/T-Rgas*T*D2FDTDV_
    DPDV_ = -Rgas*T*D2FDV2_ - nMoles*Rgas*T/V**2
    DPDn_ = -Rgas*T*D2FDVDn_ + Rgas*T/V
    ! necessary V-derivatives
    DVDn_ = -DPDn_/DPDV_
    DVDT_ = -DPDT_/DPDV_

    call MBWR_lnphi(model, T, P, V, nMoles, lnphi, DlnphiDT, DlnphiDP, DlnphiDn)
    call MBWR_Hres(model,T, P, V, nMoles, Hres, DHresDT, DHresDP, DHresDn)
    call MBWR_Sres(model,T, P, V, nMoles, Sres, DSresDT, DSresDP, DSresDn)
    call MBWR_zfac(model,T, P, V, nMoles, z, DzDT, DzDP, DzDn)
    call MBWR_Gres(model,T, P, V, nMoles, Gres)

    ! Testing the relation between P and alpha
    eps1 = abs(P + Rgas*T*DFDV_ - nMoles*Rgas*T/V)

    ! Testing derivatives of F
    eps2 = abs(nMoles*alphaDerivatives(0,0) - V*DFDV_ - nMoles*DFDn_)
    eps3 = abs(V*D2FDVDn_ + nMoles*D2FDn2_)
    eps4 = abs(V*D2FDV2_ + nMoles*D2FDVDn_)

    ! Testing derivatives of the fugacity coefficients
    eps5 = abs(DlnphiDn(1,1))
    eps6 = abs(DlnphiDP(1) - (z-1)/P)
    eps7 = abs(nMoles*DlnphiDT(1) + Hres/(Rgas*T**2))

    eps8 = abs(Sres-(Hres-Gres)/T)
    eps9 = abs(DSresDn(1)-(DHresDn(1)-Rgas*T*lnphi(1))/T)

   print *, "MODEL CONSISTENCY (these should be zero):"
   print *, eps1
   print *, eps2
   print *, eps3
   print *, eps4
   print *, eps5
   print *, eps6
   print *, eps7
   print *, eps8
   print *, eps9
  end subroutine CheckModelConsistency

  subroutine checkFderivatives(T,V,nMoles,model)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    type(eosmbwr), intent(in) :: model
    ! local variables
    real :: tr, delta
    real :: dt, dtr, dv, ddelta_dv, dn, ddelta_dn
    real, dimension(0:2,0:2) :: alphaDerivatives, alphaDerivatives_dtr, alphaDerivatives_ddelta_dv, alphaDerivatives_ddelta_dn
    real, dimension(0:2) :: res
    real :: eps_dfdt, eps_dfdv, eps_dfdn
    real :: eps_d2fdt2, eps_d2fdv2, eps_d2fdn2
    real :: eps_d2fdtdv, eps_d2fdtdn, eps_d2fdvdn

    ! check the alphar_derivatives subroutine
    real :: alpha, DalphaDdelta, D2alphaDdelta2, DalphaDtr, D2alphaDtrDdelta, D2alphaDtr2
    real, dimension(3) :: pres
    real, dimension(model%HelmLength) :: deltaCoef
    Tr = T
    delta = nMoles/(V)
    call alphar_deltaCoef(1,Tr,deltaCoef,model)
    call alphar_deltaDerivatives(pres,1,delta,deltaCoef,model)
    alpha = pres(1)

    call alphar_deltaCoef(1,Tr,deltaCoef,model) ! opt=1 (zeroth derivative wrt tr)
    call alphar_deltaDerivatives(pres,3,delta,deltaCoef,model) ! opt=3 (zeroth, first and second derivative wrt delta)
    DalphaDdelta = pres(2)
    D2alphaDdelta2 = pres(3)
    call alphar_deltaCoef(2,Tr,deltaCoef,model) ! opt=2 (first derivative wrt tr)
    call alphar_deltaDerivatives(pres,2,delta,deltaCoef,model) ! opt=3 (zeroth, first and second
    DalphaDtr = pres(1)
    D2alphaDtrDdelta = pres(2)
    call alphar_deltaCoef(3,Tr,deltaCoef,model)
    call alphar_deltaDerivatives(pres,1,delta,deltaCoef,model)
    D2alphaDtr2 = pres(1)

    ! initialize step-lengths in T, V and nMoles, and the corresponding step lengts in Tr and delta
    dt = 1e-6
    dv = 1e-6
    dn = 1e-6
    dtr=dt
    ddelta_dv = -nMoles*dv/(V*(V+dv))
    ddelta_dn = dn/(V)

    ! initialize alpha-derivatives
    call alphar_derivatives(res,tr,delta,0,2,model)
    alphaDerivatives(0,0:2) = res
    call alphar_derivatives(res,tr,delta,1,2,model)
    alphaDerivatives(1,0:2) = res
    call alphar_derivatives(res,tr,delta,2,2,model)
    alphaDerivatives(2,0:2) = res

    call alphar_derivatives(res,tr+dtr,delta,0,2,model)
    alphaDerivatives_dtr(0,0:2) = res
    call alphar_derivatives(res,tr+dtr,delta,1,2,model)
    alphaDerivatives_dtr(1,0:2) = res
    call alphar_derivatives(res,tr+dtr,delta,2,2,model)
    alphaDerivatives_dtr(2,0:2) = res

    call alphar_derivatives(res,tr,delta+ddelta_dv,0,2,model)
    alphaDerivatives_ddelta_dv(0,0:2) = res
    call alphar_derivatives(res,tr,delta+ddelta_dv,1,2,model)
    alphaDerivatives_ddelta_dv(1,0:2) = res
    call alphar_derivatives(res,tr,delta+ddelta_dv,2,2,model)
    alphaDerivatives_ddelta_dv(2,0:2) = res

    call alphar_derivatives(res,tr,delta+ddelta_dn,0,2,model)
    alphaDerivatives_ddelta_dn(0,0:2) = res
    call alphar_derivatives(res,tr,delta+ddelta_dn,1,2,model)
    alphaDerivatives_ddelta_dn(1,0:2) = res
    call alphar_derivatives(res,tr,delta+ddelta_dn,2,2,model)
    alphaDerivatives_ddelta_dn(2,0:2) = res

    eps_dfdt = abs( &
         (F(T+dt,V,nMoles,alphaDerivatives_dtr)-F(T,V,nMoles,alphaDerivatives))/dt &
         -DFDT(T,V,nMoles,alphaDerivatives) &
         )/abs(DFDT(T,V,nMoles,alphaDerivatives))

    eps_dfdv = abs( &
         (F(T,V+dv,nMoles,alphaDerivatives_ddelta_dv)-F(T,V,nMoles,alphaDerivatives))/dv &
         -DFDV(T,V,nMoles,alphaDerivatives) &
         )/abs(DFDV(T,V,nMoles,alphaDerivatives))

    eps_dfdn = abs( &
         (F(T,V,nMoles+dn,alphaDerivatives_ddelta_dn)-F(T,V,nMoles,alphaDerivatives))/dn &
         -DFDN(T,V,nMoles,alphaDerivatives) &
         )/abs(DFDN(T,V,nMoles,alphaDerivatives))

    eps_d2fdt2 = abs( &
         (DFDT(T+dt,V,nMoles,alphaDerivatives_dtr)-DFDT(T,V,nMoles,alphaDerivatives))/dt &
         -D2FDT2(T,V,nMoles,alphaDerivatives) &
         )/abs(D2FDT2(T,V,nMoles,alphaDerivatives))

    eps_d2fdv2 = abs( &
         (DFDV(T,V+dv,nMoles,alphaDerivatives_ddelta_dv)-DFDV(T,V,nMoles,alphaDerivatives))/dv &
         -D2FDV2(T,V,nMoles,alphaDerivatives) &
         )/abs(D2FDV2(T,V,nMoles,alphaDerivatives))

    eps_d2fdn2 = abs( &
         (DFDn(T,V,nMoles+dn,alphaDerivatives_ddelta_dn)-DFDn(T,V,nMoles,alphaDerivatives))/dn &
         -D2FDn2(T,V,nMoles,alphaDerivatives) &
         )/abs(D2FDn2(T,V,nMoles,alphaDerivatives))

    eps_d2fdtdv = abs( &
         (DFDT(T,V+dv,nMoles,alphaDerivatives_ddelta_dv)-DFDT(T,V,nMoles,alphaDerivatives))/dv &
         -D2FDTDV(T,V,nMoles,alphaDerivatives) &
         )/abs(D2FDTDV(T,V,nMoles,alphaDerivatives))

    eps_d2fdtdn = abs( &
         (DFDT(T,V,nMoles+dn,alphaDerivatives_ddelta_dn)-DFDT(T,V,nMoles,alphaDerivatives))/dn &
         -D2FDTDn(T,V,nMoles,alphaDerivatives) &
         )/abs(D2FDTDn(T,V,nMoles,alphaDerivatives))

    eps_d2fdvdn = abs( &
         (DFDV(T,V,nMoles+dn,alphaDerivatives_ddelta_dn)-DFDV(T,V,nMoles,alphaDerivatives))/dn &
         -D2FDVDn(T,V,nMoles,alphaDerivatives) &
         )/abs(D2FDVDn(T,V,nMoles,alphaDerivatives))

    print *, "eps_dfdt=",eps_dfdt, "dfdt", dfdt(T,V,nMoles,alphaDerivatives)
    print *, "eps_dfdv=",eps_dfdv, "dfdv", dfdv(T,V,nMoles,alphaDerivatives)
    print *, "eps_dfdn=",eps_dfdn, "dfdn", dfdn(T,V,nMoles,alphaDerivatives)
    print *, "eps_d2fdt2=",eps_d2fdt2, "d2fdt2", d2fdt2(T,V,nMoles,alphaDerivatives)
    print *, "eps_d2fdv2=",eps_d2fdv2, "d2fdv2", d2fdv2(T,V,nMoles,alphaDerivatives)
    print *, "eps_d2fdn2=",eps_d2fdn2, "d2fdn2", d2fdn2(T,V,nMoles,alphaDerivatives)
    print *, "eps_d2fdtdv=",eps_d2fdtdv, "d2fdtdv", d2fdtdv(T,V,nMoles,alphaDerivatives)
    print *, "eps_d2fdtdn=",eps_d2fdtdn, "d2fdtdn", d2fdtdn(T,V,nMoles,alphaDerivatives)
    print *, "eps_d2fdvdn=",eps_d2fdvdn, "d2fdvdn", d2fdvdn(T,V,nMoles,alphaDerivatives)

  end subroutine checkFderivatives


  real function F(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: alpha
    alpha = alphaDerivatives(0,0)
    F = nMoles*alpha
  end function F

  real function DFDT(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: DalphaDtr
    DalphaDtr = alphaDerivatives(1,0)
    DFDT = (nMoles)*DalphaDtr
  end function DFDT

  real function D2FDT2(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: D2alphaDtr2
    D2alphaDtr2 = alphaDerivatives(2,0)
    D2FDT2 = (nMoles)*D2alphaDtr2
  end function D2FDT2

  real function DFDV(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: DalphaDdelta
    DalphaDdelta = alphaDerivatives(0,1)
    DFDV = -DalphaDdelta*(nMoles**2)/(V*V)
  end function DFDV

  real function D2FDV2(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: DalphaDdelta, D2alphaDdelta2
    DalphaDdelta = alphaDerivatives(0,1)
    D2alphaDdelta2 = alphaDerivatives(0,2)
    D2FDV2 = nMoles**2/(V*V*V)*(2*DalphaDdelta + nMoles/(V)*D2alphaDdelta2)
  end function D2FDV2

  real function DFDn(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: alpha, DalphaDdelta
    alpha = alphaDerivatives(0,0)
    DalphaDdelta = alphaDerivatives(0,1)
    DFDn = alpha + nMoles/(V)*DalphaDdelta
  end function DFDn

  real function D2FDn2(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: D2alphaDdelta2, DalphaDdelta
    D2alphaDdelta2 = alphaDerivatives(0,2)
    DalphaDdelta = alphaDerivatives(0,1)
    D2FDn2 = 2/(V)*DalphaDdelta + nMoles/((V**2))*D2alphaDdelta2
  end function D2FDn2

  real function D2FDTDV(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: D2alphaDtrDdelta
    D2alphaDtrDdelta = alphaDerivatives(1,1)
    D2FDTDV = -nMoles**2/(V*V)*D2alphaDtrDdelta
  end function D2FDTDV

  real function D2FDTDn(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: DalphaDtr, D2alphaDtrDdelta
    DalphaDtr = alphaDerivatives(1,0)
    D2alphaDtrDdelta = alphaDerivatives(1,1)
    D2FDTDn = DalphaDtr + nMoles/(V)*D2alphaDtrDdelta
  end function D2FDTDn

  real function D2FDVDn(T,V,nMoles,alphaDerivatives)
    use mbwr, only: eosmbwr
    implicit none
    !input
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: nMoles
    real, dimension(0:2,0:2), intent(in) :: alphaDerivatives
    !local variables
    real :: DalphaDdelta, D2alphaDdelta2
    DalphaDdelta = alphaDerivatives(0,1)
    D2alphaDdelta2 = alphaDerivatives(0,2)
    D2FDVDn = -(nMoles/(V*V))*(2*DalphaDdelta + nMoles/(V)*D2alphaDdelta2)
  end function D2FDVDn

end module mbwr_additional
