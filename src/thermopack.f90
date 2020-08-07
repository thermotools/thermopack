program run_thermopack
  ! Program to call the thermopack code
  ! Morten Hammer, 2020-05.
  !----------------------------------------------------------------------
  use eoslibinit, only: init_thermo
  use thermopack_constants
  use critical, only: calcCriticalTV
  use eosTV, only: pressure
  implicit none
  real, dimension(5) :: z
  real :: pc, Tc, vc
  integer :: ierr

  call init_thermo('PR', 'VDW', 'CLASSIC', "CO2,N2,H2S,C2,C1", 2)
  Tc = -1
  vc = -1
  Z = [0.94980650312939319, 4.9991460192386570E-003, &
       2.0000656425762893E-004, 4.9997302858171425E-003, &
       3.9994614001293179E-002]
  call calcCriticalTV(Tc, vc, z, ierr)
  pc = pressure(Tc, vc, z)
  print *,"Tc, vc, pc",Tc, vc, pc
end program run_thermopack
