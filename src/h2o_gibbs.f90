!> Module calculating thermodynamic potentials and differentials
!! for solid H2O.
!!
!! Ref:
!! A New Equation of State for H$_2$O Ice Ih
!! Rainer Feistel and Wolfgang Wagner
!! Journal of Physical and Chemical Reference Data (J. Phys. Chem. Ref. Data)
!! Pages: 1021-1047
!! January 2006
!! Volume(Number): 35(2)
!! doi: http://dx.doi.org/10.1063/1.2183324
!!
module h2o_gibbs
  implicit none
  private
  save
  !
  real, parameter :: Ttriple = 273.160  !< K
  real, parameter :: Ptriple = 611.657  !< Pa
  real, parameter :: P0_H2O = 101325.0 !< Pa
  real, parameter :: T0_H2O = 273.152519  !< K
  real, parameter :: mw_H2O = 18.01528e-3 !< Molar weight in kg/mol
  !
  real, parameter :: g00  = -632020.233449497     !< J/kg
  real, parameter :: g01  = 0.655022213658955     !< J/kg
  real, parameter :: g02  = -1.893699293261e-8    !< J/kg
  real, parameter :: g03  = 3.39746123271053e-15  !< J/kg
  real, parameter :: g04  = -5.56464869058991e-22 !< J/kg
  real, dimension(5) :: g0_vec  = (/ g00, g01, g02, g03, g04 /)
  real :: s0  = -3327.33756492168 !189.13 !< J/kg/K ()


  !> J/kg/K
  complex*16, parameter :: t1  = (3.68017112855051e-02, 5.10878114959572e-02)
  !> J/kg/K
  complex*16, parameter :: t2  = (0.337315741065416, 0.335449415919309)
  !> J/kg/K
  complex*16, dimension(2), parameter :: t_vec  = (/ t1, t2 /)
  !> J/kg/K
  complex*16, parameter :: r1  = (44.7050716285388, 65.6876847463481)
  !> J/kg/K
  complex*16, parameter :: r20 = (-72.597457432922, -78.100842711287)
  !> J/kg/K
  complex*16, parameter :: r21 = (-5.57107698030123e-05, 4.64578634580806e-05)
  !> J/kg/K
  complex*16, parameter :: r22 = (2.34801409215913e-11, -2.85651142904972e-11)
  !> J/kg/K
  complex*16, dimension(3), parameter :: r2_vec  = (/ r20, r21, r22 /)

  !Melting enthalpy at (T0_H2O, P0_H2O)
  !Giauque and Stout 1936: 333.4220 kJ/kg and 333.4920 kJ/kg
  !Osborne 1939: 333.54(20) kJ/kg
  !Haida et al. 1974: 333.41 kJ/kg
  !Model: 333.426516 kJ/kg
  real, parameter :: dSmelt = mw_H2O*333.426516*1.0e3/T0_H2O !< J/mol/K - (Sliq - Ssolid)

  public :: sh2o_gibbs, sh2o_dgdp, sh2o_d2gdp2, sh2o_dgdT
  public :: sh2o_d2gdT2, sh2o_d2gdtdp
  public :: dSmelt, T0_H2O, P0_H2O, sho2_init

  public :: sh2o_specvol, sh2o_heat_capacity
  public :: sh2o_entropy, sh2o_enthalpy, sh2o_energy, sh2o_helmholtz

contains

  !> Set reference potentials (H2O)
  !! Input: Liquid entropy and Gibbs energy evaluated at (T0_H2O,P0_H2O)
  subroutine sho2_init(sl,gl)
    implicit none
    real, intent(in) :: sl !< Entropy - J/mol/K
    real, intent(in) :: gl !< Gibbs energy - J/mol
    ! Locals
    real :: ss_current, gs_current, ss

    ! Set entropy reference
    ss_current = sh2o_entropy(T0_H2O,P0_H2O)
    ss = sl - dSmelt
    s0 = s0 + (ss - ss_current)/mw_H2O

    ! Set Gibbs energy reference
    gs_current = sh2o_gibbs(T0_H2O,P0_H2O)
    g0_vec(1) = g0_vec(1) + (gl-gs_current)/mw_H2O

  end subroutine sho2_init

  !> Gibbs free energy for ice (H2O) (J/mol)
  function sh2o_gibbs(T,P) result(g)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: g !< J/mol
    real :: g0
    complex*16, dimension(2) :: r
    complex*16 :: c
    integer :: k
    real :: tau
    g0 = sh2o_g0(P)
    r(1) = r1
    r(2) = sh2o_r2(P)
    tau = T/Ttriple
    c = 0.0
    do k=1,2
       c = c + r(k)*( &
       (t_vec(k) - tau)*log(t_vec(k) - tau) &
       + (t_vec(k) + tau)*log(t_vec(k) + tau) &
       - 2.0*t_vec(k)*log(t_vec(k)) - tau**2/t_vec(k) &
       )
    enddo
    g = g0 - s0*T + Ttriple*REAL(c)
    g = g * mw_H2O ! per kg -> per mol
  end function sh2o_gibbs

  !> Gibbs free energy for ice (H2O) -
  !! Differential wrpt. temperature at constant pressure (J/mol/K)
  function sh2o_dgdT(T,P) result(dgdT)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: dgdT !< J/mol/K
    complex*16, dimension(2) :: r
    complex*16 :: c
    real :: tau
    integer :: k
    r(1) = r1
    r(2) = sh2o_r2(P)
    tau = T/Ttriple
    c = 0.0
    do k=1,2
       c = c + r(k)*( &
       -log(t_vec(k) - tau) &
       + log(t_vec(k) + tau) &
       - 2.0*tau/t_vec(k) &
       )
    enddo
    dgdT = - s0 + REAL(c)
    dgdT = dgdT * mw_H2O ! per kg -> per mol
  end function sh2o_dgdT

  !> Gibbs free energy for ice (H2O) -
  !! Second differential wrpt. temperature at constant pressure (J/mol/K)
  function sh2o_d2gdT2(T,P) result(d2gdT2)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: d2gdT2 !< J/mol/K2
    complex*16, dimension(2) :: r
    complex*16 :: c
    integer :: k
    real :: tau
    r(1) = r1
    r(2) = sh2o_r2(P)
    tau = T/Ttriple
    c = 0.0
    do k=1,2
       c = c + r(k)*( &
       1.0/(t_vec(k) - tau) &
       + 1.0/(t_vec(k) + tau) &
       - 2.0/t_vec(k) &
       )
    enddo
    d2gdT2 = REAL(c)/Ttriple
    d2gdT2 = d2gdT2 * mw_H2O ! per kg -> per mol
  end function sh2o_d2gdT2

  !> Gibbs free energy for ice (H2O) -
  !! Second differential wrpt. temperature and pressure (m3/mol/K)
  function sh2o_d2gdTdP(T,P) result(d2gdTdP)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: d2gdTdP !< m3/mol/K
    complex*16 :: r2p
    complex*16 :: c
    real :: tau
    r2p = sh2o_r2p(P)
    tau = T/Ttriple
    c = r2p*( &
         -log(t_vec(2) - tau) &
         + log(t_vec(2) + tau) &
         - 2.0*tau/t_vec(2) &
         )
    d2gdTdP = REAL(c)
    d2gdTdP = d2gdTdP * mw_H2O ! per kg -> per mol
  end function sh2o_d2gdTdP

  !> Gibbs free energy for ice (H2O) -
  !! Differential wrpt. pressure at constant temperature (m3/mol)
  function sh2o_dgdP(T,P) result(dgdP)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: dgdP !< m3/mol
    real :: dg0dP
    complex*16 :: r2p
    complex*16 :: c
    real :: tau
    dg0dP = sh2o_dg0dP(P)
    r2p = sh2o_r2p(P)
    tau = T/Ttriple
    c = r2p*( &
         (t_vec(2) - tau)*log(t_vec(2) - tau) &
         + (t_vec(2) + tau)*log(t_vec(2) + tau) &
         - 2.0*t_vec(2)*log(t_vec(2)) - tau**2/t_vec(2) &
         )
    dgdP = dg0dP + Ttriple*REAL(c)
    dgdP = dgdP * mw_H2O ! per kg -> per mol
  end function sh2o_dgdP

  !> Gibbs free energy for ice (H2O) -
  !! Second differential wrpt. pressure at constant temperature (m3/mol/Pa)
  function sh2o_d2gdP2(T,P) result(d2gdP2)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: d2gdP2 !< m3/mol/Pa
    real :: d2g0dP2
    complex*16 :: r2pp
    complex*16 :: c
    real :: tau
    d2g0dP2 = sh2o_d2g0dP2(P)
    r2pp = 2.0*r22/Ptriple**2
    tau = T/Ttriple
    c = r2pp*( &
         (t_vec(2) - tau)*log(t_vec(2) - tau) &
         + (t_vec(2) + tau)*log(t_vec(2) + tau) &
         - 2.0*t_vec(2)*log(t_vec(2)) - tau**2/t_vec(2) &
         )
    d2gdP2 = d2g0dP2 + Ttriple*REAL(c)
    d2gdP2 = d2gdP2 * mw_H2O ! per kg -> per mol
  end function sh2o_d2gdP2

  !> Utility function for Gibbs free energy of ice (H2O) (J/kg)
  function sh2o_g0(P) result(g0)
    implicit none
    real, intent(in) :: P !< Pa
    real :: g0 !< J/kg
    real :: pi, pi0, dpi
    integer :: k
    pi0 = P0_H2O/Ptriple
    pi = P/Ptriple
    dpi = pi - pi0
    g0 = 0.0
    do k=1,5
       g0 = g0 + g0_vec(k)*dpi**(k-1)
    enddo
  end function sh2o_g0

  !> Utility function for Gibbs free energy of ice (H2O) -
  !! Differential wrpt. pressure (m3/kg)
  function sh2o_dg0dP(P) result(dg0dP)
    implicit none
    real, intent(in) :: P !< Pa
    real :: dg0dP !< m3/kg
    real :: pi, pi0, dpi
    integer :: k
    pi0 = P0_H2O/Ptriple
    pi = P/Ptriple
    dpi = pi - pi0
    dg0dP = 0.0
    do k=1,4
       dg0dP = dg0dP + g0_vec(k+1)*k*dpi**(k-1)/Ptriple
    enddo
  end function sh2o_dg0dP

  !> Utility function for Gibbs free energy of ice (H2O) -
  !! Second differential wrpt. pressure (m3/kg/Pa)
  function sh2o_d2g0dP2(P) result(d2g0dP2)
    implicit none
    real, intent(in) :: P !< Pa
    real :: d2g0dP2 !< m3/kg/Pa
    real :: pi, pi0, dpi
    integer :: k
    pi0 = P0_H2O/Ptriple
    pi = P/Ptriple
    dpi = pi - pi0
    d2g0dP2 = 0.0
    do k=2,4
       d2g0dP2 = d2g0dP2 + g0_vec(k+1)*k*(k-1)*dpi**(k-2)/Ptriple**2
    enddo
  end function sh2o_d2g0dP2

  !> Utility function (J/kg/K)
  function sh2o_r2(P) result(r2)
    implicit none
    real, intent(in) :: P !< Pa
    complex*16 :: r2 !< J/kg/K
    real :: pi, pi0, dpi
    integer :: k
    pi0 = P0_H2O/Ptriple
    pi = P/Ptriple
    dpi = pi - pi0
    r2 = cmplx(0.0,00)
    do k=1,3
       r2 = r2 + r2_vec(k)*dpi**(k-1)
    enddo
  end function sh2o_r2

  !> Utility function
  !! Differential wrpt. pressure (m3/kg/K)
  function sh2o_r2p(P) result(r2p)
    implicit none
    real, intent(in) :: P !< Pa
    complex*16 :: r2p !< m3/kg/K
    real :: pi, pi0, dpi
    integer :: k
    pi0 = P0_H2O/Ptriple
    pi = P/Ptriple
    dpi = pi - pi0
    r2p = cmplx(0.0,00)
    do k=1,2
       r2p = r2p + r2_vec(k+1)*k*dpi**(k-1)/Ptriple
    enddo
  end function sh2o_r2p

  !> Specific volume for ice (H2O) (m3/mol)
  function sh2o_specvol(T,P) result(vol)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: vol !< m3/mol
    vol = sh2o_dgdP(T,P)
  end function sh2o_specvol

  !> Ice (H2O) entropy (J/mol/K)
  function sh2o_entropy(T,P) result(s)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: s !< J/mol/K
    s = -sh2o_dgdT(T,P)
  end function sh2o_entropy

  !> Ice (H2O) enthalpy (J/mol)
  function sh2o_enthalpy(T,P) result(h)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: h ! J/mol
    h = sh2o_gibbs(T,P) + T*sh2o_entropy(T,P)
  end function sh2o_enthalpy

  !> Ice (H2O) energy (J/mol)
  function sh2o_energy(T,P) result(e)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: e !< J/mol
    e = sh2o_enthalpy(T,P)-P*sh2o_specvol(T,P)
  end function sh2o_energy

  !> Ice (H2O) helmholtz energy (J/mol)
  function sh2o_helmholtz(T,P) result(a)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: a !< J/mol
    a = sh2o_gibbs(T,P)-P*sh2o_specvol(T,P)
  end function sh2o_helmholtz

  !> Ice (H2O) heat capacity at constant pressure (J/mol/K)
  function sh2o_heat_capacity(T,P) result(cp)
    implicit none
    real, intent(in) :: T !< K
    real, intent(in) :: P !< Pa
    real :: cp ! J/mol/K
    cp = -T*sh2o_d2gdt2(T,P)
    cp = cp * mw_H2O ! per kg -> per mol
  end function sh2o_heat_capacity

  ! function sh2o_sound_velocity(T,P) result(c)
  !   implicit none
  !   real, intent(in) :: T !< K
  !   real, intent(in) :: P !< Pa
  !   real :: c
  !   ! Locals
  !   real :: dsdt, dsdp, dvdp, dtdp
  !   dsdt = -sh2o_d2gdT2(T,P)
  !   dsdp = -sh2o_d2gdTdp(T,P)
  !   dvdp = sh2o_d2gdP2(T,P)
  !   dtdp = sh2o_d2gdTdP(T,P)
  !   c = 0.0 ! MH TODO - implement
  ! end function sh2o_sound_velocity

end module h2o_gibbs
