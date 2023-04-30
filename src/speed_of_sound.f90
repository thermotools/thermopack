!-----------------------------------------------------------------------------
!> Calculate speed of sound for full (P,T, chemical potential) equilibrium
!>
!>
!>
!-----------------------------------------------------------------------------
module speed_of_sound
  !
  !
  use thermopack_constants, only: VAPPH,LIQPH,SOLIDPH, &
       TREND
  use thermopack_var, only: nc, nph, get_active_thermo_model, thermo_model, &
       Rgas
  implicit none
  private
  save

  public :: sound_velocity_2ph
  public :: twoPhaseSpeedOfSound, singlePhaseSpeedOfSound, solidSpeedOfSound
  public :: speed_of_sound_TV

  include "trend_interface.f95"

contains

  !-----------------------------------------------------------------------------
  !> Calculate speed-of-sound for single-phase flow.
  !>
  !> \author KYL, 2013-10
  !-----------------------------------------------------------------------------
  function singlePhaseSpeedOfSound(t,p,Z,phase) result(sos)
    use eos, only: specificVolume, entropy, moleWeight
    use trend_solver, only: trend_density
    implicit none
    real,                  intent(in) :: t     !< Temperature [K]
    real,                  intent(in) :: p     !< Pressure [Pa]
    real, dimension(nc),   intent(in) :: Z     !< Overall molar compozition [-]
    integer,               intent(in) :: phase !< Phase spec
    real :: sos
    real :: v,dvdt,dvdp
    real :: s,dsdt,dsdp,dtdp
    real :: rho_trend
    type(thermo_model), pointer :: act_mod_ptr

    act_mod_ptr => get_active_thermo_model()
    select case(act_mod_ptr%eoslib)
      case(TREND)
        call trend_density(T,p,z,phase,rho_trend)
        sos = trend_speedofsound(T,rho_trend,z)
      case default
        ! Find s, dsdt_p, dsdp_t, and then dtdp_s
        !
        call entropy(t,p,Z,phase,s,dsdt,dsdp)
        dtdp = -dsdp/dsdt

        !
        ! Find v, dvdt_p, dvdp_t, and then dvdp_s
        !
        call specificVolume(t,p,Z,phase,v,dvdt,dvdp)
        dvdp = dvdp + dvdt*dtdp

        !
        ! Finally find speed of sound
        !
        if (dvdp < 0.0) then
          sos = sqrt(-1000*v*v/(moleWeight(Z)*dvdp))
        else
           sos = 0.0
           ! Tesing process optimisation - close to critical
!           point/saturation line .... supressing warning and continue
!
!           write (*,*) "speed_of_sound:singlePhaseSpeedOfSound, negative drho/dp-term, set to zero"

!           Will probaly crash sooner of later if this close the the
!           two-phase region...

!           call stoperror('speed_of_sound:singlePhaseSpeedOfSound')
        endif
      end select

  end function singlePhaseSpeedOfSound

  !-----------------------------------------------------------------------------
  !> Calculate speed-of-sound for liqid-liquid or gas-liquid mixtures.
  !>
  !> \author MH, 2013-09
  !-----------------------------------------------------------------------------
  function twoPhaseSpeedOfSound(nph,t,p,Z,beta,X,phase) result(sos)
    use state_functions, only: getStateFuncMatrix
    use eos, only: moleWeight
    implicit none
    integer,               intent(in) :: nph   !< Maximum number of phases
    real,                  intent(in) :: t     !< Temperature [K]
    real,                  intent(in) :: p     !< Pressure [Pa]
    real, dimension(nc),   intent(in) :: Z     !< Overall molar compozition [-]
    real, dimension(nph),  intent(in) :: beta  !< Phase molar fraction [-]
    real, dimension(nph,nc), intent(in) :: X     !< Phase molar compozition [-]
    integer, dimension(nph), intent(in) :: phase !< Phase spec
    real :: sos
    ! Locals
    real, dimension(nc+2,nc+2) :: M !< Jacobian matrix
    real, dimension(nc+2) :: RHS !< Right hand side
    real, dimension(2) :: vspec
    integer, dimension(nc+2) :: ipiv
    real, dimension(3*nc) ::  work
    integer :: ifail
    real :: v, dpdv, mw

    vspec = 0.0
    call getStateFuncMatrix(t,p,Z,beta(2),beta(1),X(1,:),X(2,:), &
                            'SV',vspec,M,RHS,phase)
    v = Rgas*t*RHS(nc+2)/p
    RHS(nc+2) = p/(Rgas*t)
    RHS(1:nc+1) = 0.0

    ! lapack routines
    ! symmetric indefinite matrix
    call dsysv('u', nc+2, 1, M, nc+2, ipiv, RHS, nc+2, work, 3*nc, ifail)
    if (ifail /= 0 .or. RHS(nc+2) /= RHS(nc+2)) then
      dpdv = 1.0 ! Force single phase calculation
    else
      mw = moleWeight(Z)
      dpdv = p*RHS(nc+2) ! Convert from d lnP to d P
    endif

    if (dpdv < 0.0) then
      sos = sqrt(-1000*dpdv*v*v/mw)
    else
      ! Use single phase liquid speed of sound
      sos = singlePhaseSpeedOfSound(t,p,Z,LIQPH)
    endif

  end function twoPhaseSpeedOfSound

  !-----------------------------------------------------------------------------
  !> Calculate speed-of-sound for liqid-solid or gas-solid mixtures.
  !>
  !> Equation set considered:
  !> \f{align*}{
  !>   & f(1) = \ln\left(x^F(js)\varphi^F(js)\right) - \ln\left(\varphi^S\right) \\
  !>   & f(2) = \frac{S_{\text{spec}}-S_{\text{mix}}}{R} \\
  !>   & f(3) = \frac{P\left(v_{\text{mix}}-v_{\text{spec}}\right)}{R T} \\
  !> \f}
  !> Variable vector:
  !> \f[
  !>   X^\intercal =  \left[n^F(js),\ln T, \ln P\right]
  !> \f]
  !>
  !> \author MH, 2013-09
  !-----------------------------------------------------------------------------
  function solidSpeedOfSound(t,p,Z,betaF,Xf,Xs,phase) result(sos)
    use eos, only: moleWeight, thermo, entropy, specificVolume
    use solideos, only: solid_thermo, solid_entropy, &
         solid_specificVolume
    implicit none
    real, intent(in) :: betaF !< Fluid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(in) :: Xf !< Fluid molar compozition [-]
    real, dimension(nc), intent(in) :: Xs !< Solid molar compozition [-]
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    integer, intent(in) :: phase !< Fluid phase integer
    real :: sos
    ! Locals
    real, dimension(3,3) :: M !< Jacobian matrix
    real, dimension(3) :: RHS !< Right hand side
    integer, dimension(3) :: ipiv
    real, dimension(9) :: work
    integer :: ifail, js
    integer, dimension(1) :: imax
    real :: mw,ss,dssdt,s,dsdt,lnphis,lnphist,lnphisp
    real :: v,dvdt,dvdp,vs,dvsdt,dvsdp,dpdv
    real, dimension(nc) :: lnphi,lnphiT,lnphiP
    real, dimension(nc,nc) :: lnphiX
    !
    imax = maxloc(Xs)
    js = imax(1)

    ! Solid properties
    call solid_thermo(t,p,Xs,lnphis,lnphist,lnphisp)
    call solid_entropy(t,p,Xs,ss,dssdt)
    call solid_specificVolume(t,p,Xs,vs,dvsdt,dvsdp)

    ! Fluid properties
    call thermo(t,p,Xf,phase,lnphi,lnFUGX=lnphiX,lnfugt=lnphiT,lnfugp=lnphiP) ! ln phi
    call entropy(t,p,Xf,phase,s,dsdt=dsdt)
    call specificVolume(t,p,Xf,phase,v,dvdt=dvdt,dvdp=dvdp)

    ! Variable Xf(js), ln T, ln P
    M(1,1) = (1.0/(XF(js)*betaF) - 1.0/betaF + lnphiX(js,js)/betaF)
    M(1,2) = T*(lnphiT(js) - lnphisT)
    M(1,3) = P*(lnphiP(js) - lnphisP)
    M(2,1) = M(1,2)
    M(3,1) = M(1,3)

    dsdt = T*(betaF*dsdt + (1.0-betaF)*dssdt)
    M(2,2) = -dsdt/Rgas

    v = betaF*v + (1.0-betaF)*vs
    dVdp = betaF*dvdp + (1.0-betaF)*dvsdp
    dVdt = betaF*dvsdt + (1.0-betaF)*dvsdt
    M(3,3) = p*p*dVdp/(Rgas*t)
    M(2,3) = p*dVdt/Rgas
    M(3,2) = M(2,3)

    ! Right hand side
    RHS(3) = p/(Rgas*t)
    RHS(1:2) = 0.0

    ! lapack routines
    ! symmetric indefinite matrix
    call dsysv('u', 3, 1, M, 3, ipiv, RHS, 3, work, 9, ifail)
    if (ifail .ne. 0) then
      call stoperror('Matrix error in speed_of_sound::solidSpeedOfSound')
    endif

    mw = moleWeight(Z)
    dpdv = p*RHS(3) ! Convert from d lnP to d P
    sos = sqrt(-1000*dpdv*v*v/mw)

  end function solidSpeedOfSound

  !-----------------------------------------------------------------------------
  !> Calculate speed of sound for single phase or gas-liquid. Alternative
  !! interface to sound_velocity.
  !!
  !! \author MH, 2014-05
  !-----------------------------------------------------------------------------
  function sound_velocity_2ph(t,p,X,Y,Z,betaV,betaL,phase,ph) result(sos)
    use thermopack_constants, only: TWOPH,VAPPH,LIQPH,SINGLEPH
    implicit none
    real,                   intent(in) :: t     !< Temperature [K]
    real,                   intent(in) :: p     !< Pressure [Pa]
    real, dimension(nc),    intent(in) :: Z     !< Overall molar compozition [-]
    real,                   intent(in) :: betaV !< Vapor molar fraction [-]
    real,                   intent(in) :: betaL !< Liquid molar fraction [-]
    real, dimension(nc),    intent(in) :: X     !< Liquid molar compozition [-]
    real, dimension(nc),    intent(in) :: Y     !< Vapor molar compozition [-]
    integer,                intent(in) :: phase !< Phase spec
    integer, dimension(2), optional, intent(in) :: ph !< Override phase integers
    real :: sos !< Speed of sound [m/s]
    ! Locals
    real, dimension(2) :: betaVec
    real, dimension(2,nc) :: XVec
    integer, dimension(2) :: phaseVec

    if (phase == TWOPH) then
      ! Gas-Liquid speed of sound
      betaVec(1) = betaV
      XVec(1,:) = Y
      betaVec(2) = betaL
      XVec(2,:) = X
      if (present(ph)) then
        if (ph(1) == SINGLEPH) then
          phaseVec(1) = LIQPH
        else
          phaseVec(1) = ph(1)
        endif
        if (ph(2) == SINGLEPH) then
          phaseVec(2) = LIQPH
        else
          phaseVec(2) = ph(2)
        endif
      else
        phaseVec(1) = VAPPH
        phaseVec(2) = LIQPH
      endif
      sos = twoPhaseSpeedOfSound(2,t,p,Z,betaVec,XVec,phaseVec)
    else
      if (phase == SINGLEPH) then
        sos = singlePhaseSpeedOfSound(t,p,Z,LIQPH)
      else
        sos = singlePhaseSpeedOfSound(t,p,Z,phase)
      endif
    endif
  end function sound_velocity_2ph

  !> Calculate speed of sound for single phase given temperature,
  !! volume and mol numbers
  !!
  !! \author MH, 2019-06
  !-----------------------------------------------------------------------------
  function speed_of_sound_TV(t,v,n) result(sos)
    use eosTV, only: pressure, entropy_tv
    use eos, only: moleWeight
    implicit none
    real,                   intent(in) :: t     !< Temperature [K]
    real,                   intent(in) :: v     !< Pressure [m3]
    real, dimension(nc),    intent(in) :: n     !< Mol numbers [mol]
    real :: sos !< Speed of sound [m/s]
    ! Locals
    real :: p, dpdv, dpdt, s, dsdt, dsdv, dtdv, dpdrho, rho
    p = pressure(t,v,n,dpdv,dpdt)
    call entropy_tv(t,v,n,s,dsdt,dsdv)
    dtdv = -dsdv/dsdt
    dpdv = dpdv + dpdt*dtdv
    if (dpdv < 0.0) then
      rho = sum(n)/v
      dpdrho = -1000*v*dpdv/rho/moleWeight(n/sum(n))
      sos = sqrt(dpdrho)
    else
      sos = 0.0
    endif
  end function speed_of_sound_TV

end module speed_of_sound
