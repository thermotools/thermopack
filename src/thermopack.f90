program run_thermopack
  ! Program to call the thermopack code
  ! Morten Hammer, 2020-05.
  !----------------------------------------------------------------------
  use eoslibinit, only: init_thermo
  use thermopack_constants
  use critical, only: calcCriticalTV
  use eosTV, only: pressure
  implicit none
  real, dimension(2) :: z
  real :: pc, Tc, vc
  integer :: ierr

  !call init_thermo('PC-SAFT', 'VDW', 'CLASSIC', "CO2,H2O", 2)
  !call init_thermo('PR', 'VDW', 'CLASSIC', "CO2,H2O", 2)
  call init_thermo('CPA-SRK', 'VDW', 'CLASSIC', "H2O,ETOH", 2)
  !call init_thermo('PR', 'HV1', 'CLASSIC', "CO2,H2O", 2)
  Tc = 290.0
  vc = -1!1.0/(50.5e3)
  Z = [0.99,0.01]
  !print *, z
  call calcCriticalTV(Tc, vc, z, ierr)
  pc = pressure(Tc, vc, z)
  print *,"Tc, vc, pc",Tc, vc, pc
end program run_thermopack
