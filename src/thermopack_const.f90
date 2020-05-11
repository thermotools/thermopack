module tpconst
  ! Constants.
  implicit none
  save
  real, parameter :: Tmax=2000.0 !< K
  real, parameter :: Tmin=50.0 !< K
  real, parameter :: kB_const=1.380649e-23 !< J/K (Boltzmanns const)
  real, parameter :: h_const=6.626069311e-34  !< Js (Plancks constant)
  real, parameter :: N_Avogadro = 6.02214076e23
  real, parameter :: Rgas_default = N_Avogadro*kB_const
  real :: Rgas=Rgas_default !< J/mol/K
  real :: kRgas=1000.0*Rgas_default !< J/kmol/K
  real :: tpTmax=999.0 !< K
  real :: tpTmin=80.0 !< K
  real :: tpPmax=1.0e8 !< Pa
  real :: tpPmin=1.0e1 !< Pa
  !> Apply special treatement to phases with low consentrations of electrolytes
  real, parameter :: elEps = 1.0e-25
  !> Trace components, and zero components
  real, parameter :: traceEps = 1.0e-20
  public

contains

  subroutine set_constants(RgasIn)
    use parameters, only: EosLib, TREND
    implicit none
    real, optional, intent(in) :: RgasIn
    if (.not. present(RgasIn)) then
      Rgas = Rgas_default
    else
      Rgas = RgasIn
    endif
    kRgas = 1000.0*Rgas
    if (EosLib == TREND) then
      tpTmin = 140.0
    else
      tpTmin = 80.0
    endif
  end subroutine set_constants

  !----------------------------------------------------------------------
  subroutine get_eoslib_templimits(ieoslib, Tmin, Tmax)
    !> Get EoSlib-specific max/min supported temperature
    !>
    !> \author EA, 2014-05
    implicit none
    ! Input:
    integer,  intent(in)  :: ieoslib !< EoSlib to get temp.-limits for
    ! Output:
    real,     intent(out) :: Tmin !< Minimum supported temperature (K)
    real,     intent(out) :: Tmax !< Maximum supported temperature (K)

    Tmin = tpTmin
    Tmax = tpTmax
  end subroutine get_eoslib_templimits

  subroutine get_eoslib_presslimits(ieoslib, Pmin, Pmax)
    !> Get EoSlib-specific max/min supported pressure
    implicit none
    ! Input:
    integer,  intent(in)  :: ieoslib !< EoSlib to get temp.-limits for
    ! Output:
    real,     intent(out) :: Pmin !< Minimum supported temperature (Pa)
    real,     intent(out) :: Pmax !< Maximum supported temperature (Pa)

    Pmin = tpPmin
    Pmax = tpPmax
  end subroutine get_eoslib_presslimits

  function getRgas(z) result(R)
    use parameters, only: EosLib, TREND, nc
    implicit none
    ! Include TREND interface
    include 'trend_interface.f95'
    real, intent(in) :: z(nc) !< Mole fractions
    real :: R
    !
    if (EosLib == TREND) then
      R = trend_Rmix(z)
    else
      R = Rgas
    endif
  end function getRgas

  function getkRgas(z) result(kR)
    use parameters, only: nc
    implicit none
    real, intent(in) :: z(nc) !< Mole fractions
    real :: kR
    !
    kR = 1000.0*getRgas(z)
  end function getkRgas

end module tpconst
