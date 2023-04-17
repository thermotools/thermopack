program run_thermopack
  ! Program to call the thermopack code
  ! Morten Hammer, 2020-05.
  !----------------------------------------------------------------------
  use eoslibinit
  use solid_saturation
  use thermopack_constants
  use critical, only: calcCriticalTV
  use eosTV, only: pressure
  implicit none
  integer, parameter :: nmax = 25
  real :: Pm(nmax), Tm(nmax), Ps(nmax), Ts(nmax)
  integer :: ierr

  call init_tcpr("CO2")
  tptmin = 40
  
  ! S3
  !call sublimation_pressure_correlation(T_min=194.6857,icomp=1,scale_to_eos=.false.,nmax=nmax,T_sub=Ts,p_sub=ps,ierr=ierr)
  ! S2
  !call sublimation_pressure_correlation(T_min=62.0,icomp=1,scale_to_eos=.true.,nmax=nmax,T_sub=Ts,p_sub=ps,ierr=ierr)
  !print *,ts
  !print *,ps
  call melting_pressure_correlation(T_max=400.0,icomp=1,scale_to_eos=.true.,nmax=nmax,t_melt=Tm,p_melt=pm,ierr=ierr)
  print *,tm
  print *,pm*1e-6

end program run_thermopack
