program run_thermopack
  ! Dummy program to call the thermopack code
  ! Morten Hammer, 2020-06.
  !----------------------------------------------------------------------
  use eoslibinit, only: init_cubic
  use eos, only: enthalpy, specificvolume
  use parameters, only: LIQPH
  implicit none
  !----------------------------------------------------------------------
  integer, parameter :: ncomp = 2
  real :: t, p, z(ncomp), v, h

  call init_cubic("CO2,C1","PR","Classic","Classic")
  z = (/0.9, 0.1/)
  p=1.0e6
  t=270.0
  call specificvolume(T,P,z,LIQPH,v)
  print *,"Specific volume (m3/mol) ",v
  call enthalpy(T,P,z,LIQPH,h_eos)
  print *,"Specific enthalpy (J/mol) ",h

end program run_thermopack
