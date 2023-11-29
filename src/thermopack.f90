program run_thermopack
  ! Program to call the thermopack code
  ! Morten Hammer, 2020-05.
  !----------------------------------------------------------------------
  use eoslibinit, only: init_thermo, init_saftvrmie
  use thermopack_constants
  use critical, only: calcCriticalTV
  use eosTV, only: pressure
  use stringmod
  implicit none
  real, dimension(2) :: z
  real :: pc, Tc, vc
  integer :: ierr
  logical :: m

  m = exact_substring_match("TCRK/OLIVEIRA2008/DUMMY","DEFAULT/OLIVEIRA2008")
  print *,m
  m = exact_substring_match("TCRK/OLIVEIRA200/DEFAULT","DEFAULT/OLIVEIRA2008")
  print *,m
  m = exact_substring_match("TCRK/OLIVEIRA2008/TANG_GROSS2010","TANG_GROSS2010")
  print *,m
  m = exact_substring_match("tcRK/Oliveira200/Default","Default/Oliveira2008")
  print *,m
  stop
  !call init_thermo('PC-SAFT', 'VDW', 'CLASSIC', "CO2,H2O", 2)
  !call init_thermo('PR', 'VDW', 'CLASSIC', "CO2,H2O", 2)
  !call init_thermo('CPA-SRK', 'VDW', 'CLASSIC', "H2O,ETOH", 2)
  !call init_thermo('PR', 'HV1', 'CLASSIC', "CO2,H2O", 2)
  call init_saftvrmie("CO2,NE","DEFAULT")
  Tc = -1
  vc = -1
  Z = [0.99,0.01]
  call calcCriticalTV(Tc, vc, z, ierr)
  pc = pressure(Tc, vc, z)
  print *,"Tc, vc, pc",Tc, vc, pc
end program run_thermopack
