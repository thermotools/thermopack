module hardsphere_bmcsl
  use hyperdual_mod
  use numconstants, only: PI
  implicit none

  public :: calc_ares_hardsphere_bmcsl

contains

  subroutine calc_ares_hardsphere_bmcsl(nc, rhovec, diameters, a)
    !> Boublik-Mansoori-Carnahan-Starling-Leland equation of state for
    !> reduced, residual helmholtz energy of additive hard-sphere mixtures
    integer, intent(in) :: nc                     !< number of components (-)
    type(hyperdual), intent(in)  :: rhovec(nc)    !< number densities (1/m^3)
    type(hyperdual), intent(in)  :: diameters(nc) !< hard-sphere diameters (m)
    type(hyperdual), intent(out) :: a             !< reduced residual Helmholtz energy beta*Ares/N (-)
    ! Locals
    type(hyperdual) :: pref, term, en_m_zeta3, ln_term
    type(hyperdual) :: zeta0, zeta1, zeta2, zeta3
    integer :: i

    ! Extracting variables from vector to enhance readability
    zeta0 = 0.0
    zeta1 = 0.0
    zeta2 = 0.0
    zeta3 = 0.0
    do i=1,nc
       term = (PI/6)*rhovec(i)
       zeta0 = zeta0 + term
       zeta1 = zeta1 + term*diameters(i)
       zeta2 = zeta2 + term*diameters(i)**2
       zeta3 = zeta3 + term*diameters(i)**3
    end do

    ! Precalculate
    pref = 1.0/zeta0
    en_m_zeta3 = 1.0 - zeta3
    ln_term = log(abs(en_m_zeta3))

    ! The reduced, residual Helmholtz energy
    a = pref * ((3.0*zeta1*zeta2)/en_m_zeta3 + &
         (zeta2**3)/(zeta3*(en_m_zeta3**2)) + &
         (((zeta2**3)/(zeta3**2)) - zeta0) * ln_term)
  end subroutine calc_ares_hardsphere_bmcsl

end module hardsphere_bmcsl
