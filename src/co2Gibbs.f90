!> Module calculating thermodynamic potentials and differentials
!! for solid CO2.
!!
!! Ref:
!! Equation of State for Solid Carbon Dioxide Based on the Gibbs Free Energy
!! Andreas Jäger and Roland Span
!! Journal of Chemical & Engineering Data (J. Chem. Eng. Data)
!! Pages: 590-597
!! January 2012
!! Number 57
!!
!!
module co2Gibbs
  !use co2_properties_sw, only: mwco2
  implicit none
  private
  save
  !
  real, parameter :: R = 8.314472 !< J/mol/K
  real, parameter :: Tref = 150.0 !< K
  real, parameter :: Pref = 101325 !< Pa
  real, parameter :: Vref = 2.7186286e-5 !< m3/mol
  real, parameter :: dh_melt_tr = 8875.0 !< Triple point melting enthalpy [J/mol]
  real, parameter :: mwco2 = 44.01 !< ! Mole weight [g/mol]

  !real, parameter :: g0  = -2.6385478 ! Parameter given by Jager
  !real, parameter :: g1  = 4.5088732
  real            :: g0 = -28.605849252957245 !-22.168494972829066
  real            :: g1 = 4.5088732 + 14.500287452234922
  real, parameter :: g2  = -2.0109135
  real, parameter :: g3  = -2.7976237
  real, parameter :: g4  = 2.6427834e-1
  real, parameter :: g5  = 3.8259935
  real, parameter :: g6  = 3.1711996e-1
  real, parameter :: g7  = 2.2087195e-3
  real, parameter :: g8  = -1.1289668
  real, parameter :: g9  = 9.2923982e-3
  real, parameter :: g10 = 3.3914617e3

  real, parameter :: n = 7.0
  real, parameter :: nn = (n-1)/n

  real, parameter :: g0a = 3.9993365e-2
  real, parameter :: g1a = 2.3945101e-3
  real, parameter :: g2a = 3.2839467e-1
  real, parameter :: g3a = 5.7918471e-2
  real, parameter :: g4a = g1a
  real, parameter :: g5a = -2.6531689e-3
  real, parameter :: g6a = 1.6419734e-1
  real, parameter :: g7a = 1.7594802e-1
  real, parameter :: g8a = 2.6531689e-3
  real, parameter :: g0k = 2.2690751e-1
  real, parameter :: g1k = -7.5019750e-2
  real, parameter :: g2k = 2.6442913e-1

  public :: sco2init
  public :: sco2_gibbs, sco2_dgdp, sco2_d2gdp2, sco2_dgdT
  public :: sco2_d2gdT2, sco2_d2gdtdp

  public :: sco2_specvol, sco2_heat_capacity, sco2_speed_of_sound
  public :: sco2_entropy, sco2_enthalpy, sco2_energy, sco2_helmholtz

contains

  !-----------------------------------------------------------------------------
  !> Initialize module for solid co2 Gibbs energy to same reference levels as
  !> another EoS.
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  subroutine sco2init(sl_tr, g_tr, T_tr, P_tr)
    implicit none
    real, intent(in) :: sl_tr !< J/mol/K - Liquid entropy at triple point
    real, intent(in) :: g_tr  !< J/mol - Gibbs energy at triple point
    real, intent(in) :: T_tr  !< K - Triple point temperature
    real, intent(in) :: P_tr  !< Pa - Triple point pressure
    ! Locals
    real :: ss, ss_current, gs_current

    ! Set g1:
    ss = sl_tr - dh_melt_tr/T_tr
    ss_current = sco2_entropy(T_tr,P_tr)
    g1 = g1 - (ss - ss_current)/R

    ! Set g0:
    gs_current = sco2_gibbs(T_tr,P_tr)
    g0 = g0 + (g_tr-gs_current)/(R*Tref)
  end subroutine sco2init

  !-----------------------------------------------------------------------------
  !> Calculate gibbs energy for dry-ice.
  !> Unit: J/mol
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_gibbs(T,P) result(g)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: g
    real :: v, pi, K, f, dv, dpi
    v = T/Tref
    dv = v - 1
    pi = P/Pref
    dpi = pi - 1
    K = Kfun(v)
    f = Falpha(v)
    g = g0 + g1 * dv +  g2 * dv**2 &
         + g3 * (log((v**2+g4**2)/(1+g4**2)) - &
         (2*v/g4)*(atan(v/g4) - atan(1/g4))) &
         + g5 * (log((v**2+g6**2)/(1+g6**2)) - &
         (2*v/g6)*(atan(v/g6) - atan(1/g6))) &
         + g7*dpi*(exp(f) + K*g8) + &
         g9*K*((pi + g10)**nn - (1+g10)**nn)
    g = g * R * TRef
  end function sco2_gibbs

  !-----------------------------------------------------------------------------
  !> Calculate specific volume of dry-ice.
  !> Gibbs differential wrpt. pressure at constant temperature.
  !> Unit: m3/mol
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_dgdp(T,P) result(dg)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: dg
    ! Locals
    real :: v, pi, K, f
    v = T/Tref
    pi = P/Pref
    K = Kfun(v)
    f = Falpha(v)
    dg = (R*Tref/Pref)*(g7*(exp(f) + K*g8) + g9*K*nn*(pi+g10)**(-1.0/n))
  end function sco2_dgdp

  !-----------------------------------------------------------------------------
  !> Calculate specific volume differential wrpt. pressure of dry-ice.
  !> Temperature is held constant
  !> Unit: m3/mol/Pa
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_d2gdp2(T,P) result(dg)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: dg
    ! Locals
    real :: v, pi, K
    v = T/Tref
    pi = P/Pref
    K = Kfun(v)
    dg = -R*Tref/Pref**2*g9*K*nn/n*(pi+g10)**(-(n+1)/n)
  end function sco2_d2gdp2

  !-----------------------------------------------------------------------------
  !> Calculate Gibbs differential wrpt. temperature.
  !> Pressure is held constant
  !> Unit: J/mol/K
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_dgdT(T,P) result(dg)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: dg
    ! Locals
    real :: v, pi, ef , dv, dpi, dfdv, dKdv
    v = T/Tref
    dv = v - 1
    pi = P/Pref
    dpi = pi - 1
    ef = exp(Falpha(v))
    dfdv = dFalphadv(v)
    dKdv = dKdvfun(v)
    dg = R*(g1 + g2*2*dv - 2*g3/g4*(atan(v/g4) - atan(1/g4)) &
         - 2*g5/g6*(atan(v/g6) - atan(1/g6)) + g7*dpi*(ef*dfdv + g8*dKdv) &
         + g9*dKdv*((pi + g10)**nn - (1 + g10)**nn))
  end function sco2_dgdT

  !-----------------------------------------------------------------------------
  !> Calculate Gibbs second differential wrpt. temperature.
  !> Pressure is held constant
  !> Unit: J/mol/K2
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_d2gdT2(T,P) result(dg)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: dg
    ! Locals
    real :: v, pi, ef, dpi, dfdv, d2fdv2, d2Kdv2
    v = T/Tref
    pi = P/Pref
    dpi = pi - 1
    ef = exp(Falpha(v))
    dfdv = dFalphadv(v)
    d2fdv2 = d2Falphadv2(v)
    d2Kdv2 = d2Kdv2fun(v)
    dg = R/Tref*(2*g2 - 2*g3/(v**2+g4**2) - 2*g5/(v**2+g6**2) &
         + g7*dpi*(ef*d2fdv2 + ef*dfdv**2 + g8*d2Kdv2) &
         + g9*d2Kdv2*((pi + g10)**nn - (1 + g10)**nn))
  end function sco2_d2gdT2

  !-----------------------------------------------------------------------------
  !> Calculate Gibbs second differential wrpt. temperature and pressure.
  !> Unit: J/mol/K/Pa
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_d2gdTdP(T,P) result(dg)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: dg
    ! Locals
    real :: v, pi, ef, dfdv, dKdv
    v = T/Tref
    pi = P/Pref
    ef = exp(Falpha(v))
    dfdv = dFalphadv(v)
    dKdv = dKdvfun(v)
    dg = R/Pref*(g7*(ef*dfdv + g8*dKdv) &
         + g9*dKdv*nn*(pi + g10)**(-1/n))
  end function sco2_d2gdTdP

  function Falpha(v) result(f)
    implicit none
    real, intent(in) :: v ! Reduced temperature
    real :: f
    f = g0a*(v**2-1)+g1a*log((v**2 - g2a*v + g3a)/(1 - g2a + g3a)) &
         + g4a*log((v**2 + g2a*v + g3a)/(1 + g2a + g3a)) &
         + g5a*(atan((v - g6a)/g7a)-atan((1 - g6a)/g7a)) &
         + g8a*(atan((v + g6a)/g7a)-atan((1 + g6a)/g7a))
  end function Falpha

  function dFalphadv(v) result(df)
    implicit none
    real, intent(in) :: v ! Reduced temperature
    real :: df
    df = 2*g0a*v + g1a*(2*v-g2a)/(v**2-g2a*v+g3a) &
         + g4a*(2*v+g2a)/(v**2+g2a*v+g3a) &
         + (g5a/g7a)/(1 + ((v-g6a)/g7a)**2) &
         + (g8a/g7a)/(1 + ((v+g6a)/g7a)**2)
  end function dFalphadv

  function d2Falphadv2(v) result(df)
    implicit none
    real, intent(in) :: v ! Reduced temperature
    real :: df
    df = 2*g0a + g1a*(2*(v**2-g2a*v+g3a)-(2*v-g2a)**2)/(v**2-g2a*v+g3a)**2 &
         + g4a*(2*(v**2+g2a*v+g3a)-(2*v+g2a)**2)/(v**2+g2a*v+g3a)**2 &
         - (g5a/g7a**2)/(1 + ((v-g6a)/g7a)**2)**2*2*(v-g6a)/g7a &
         - (g8a/g7a**2)/(1 + ((v+g6a)/g7a)**2)**2*2*(v+g6a)/g7a
  end function d2Falphadv2

  function Kfun(v) result(K)
    implicit none
    real, intent(in) :: v ! Reduced volume
    real :: K
    K = g0k*v**2 + g1k*v + g2k
  end function Kfun

  function dKdvfun(v) result(dK)
    implicit none
    real, intent(in) :: v ! Reduced volume
    real :: dK
    dK = 2*g0k*v + g1k
  end function dKdvfun

  function d2Kdv2fun(v) result(dK)
    implicit none
    real, intent(in) :: v ! Reduced volume
    real :: dK
    dK = 2*g0k
  end function d2Kdv2fun

  !-----------------------------------------------------------------------------
  !> Calculate specific volume of dry-ice.
  !> Gibbs differential wrpt. pressure at constant temperature.
  !> Unit: m3/mol
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_specvol(T,P) result(vol)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: vol
    vol = sco2_dgdP(T,P)
  end function sco2_specvol

  !-----------------------------------------------------------------------------
  !> Calculate specific entropy of dry-ice.
  !> Unit: J/mol/K
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_entropy(T,P) result(s)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: s
    s = -sco2_dgdT(T,P)
  end function sco2_entropy

  !-----------------------------------------------------------------------------
  !> Calculate specific enthalpy of dry-ice.
  !> Unit: J/mol
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_enthalpy(T,P) result(h)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: h
    h = sco2_gibbs(T,P) + T*sco2_entropy(T,P)
  end function sco2_enthalpy

  !-----------------------------------------------------------------------------
  !> Calculate specific internal energy of dry-ice.
  !> Unit: J/mol
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_energy(T,P) result(e)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: e
    e = sco2_enthalpy(T,P)-P*sco2_specvol(T,P)
  end function sco2_energy

  !-----------------------------------------------------------------------------
  !> Calculate specific Helmholtz energy of dry-ice.
  !> Unit: J/mol
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_helmholtz(T,P) result(a)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: a
    a = sco2_gibbs(T,P)-P*sco2_specvol(T,P)
  end function sco2_helmholtz

  !-----------------------------------------------------------------------------
  !> Calculate specific heat-capasity of dry-ice at constant pressure.
  !> Unit: J/mol/K
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_heat_capacity(T,P) result(cp)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: cp
    cp = -T*sco2_d2gdt2(T,P)
  end function sco2_heat_capacity

  !-----------------------------------------------------------------------------
  !> Calculate speed of sound.
  !> Unit: m/s
  !>
  !> \author MH, 2013-03-01
  !-----------------------------------------------------------------------------
  function sco2_speed_of_sound(T,P) result(c)
    implicit none
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: P !< Pa - Pressure
    real :: c
    ! Locals
    real :: dsdt, dsdp, dvdp, dvdt, dtdp
    real :: denum, v
    v = sco2_specvol(T,P)
    dsdt = -sco2_d2gdT2(T,P)
    dsdp = -sco2_d2gdTdp(T,P)
    dtdp = - dsdp/dsdt
    dvdp = sco2_d2gdP2(T,P)
    dvdt = sco2_d2gdTdP(T,P)
    denum = dvdp + dvdt*dtdp
    c = v/sqrt(-denum/mwco2/1.0e3)
  end function sco2_speed_of_sound

end module co2Gibbs
