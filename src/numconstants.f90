module numconstants
  ! Numerical and mathematical constants.
  implicit none
  save
  public
  real, parameter :: almost_zero = tiny(1.0)
  real, parameter :: machine_prec = epsilon(1.0)
  real, parameter :: large = 1.0e10
  real, parameter :: small = 1.0e-12
  real, parameter :: rtol = machine_prec*1.0e-10
  real, parameter :: pi = 4.0*atan(1.0)
  real, parameter :: expMax = log(huge(1.0))
  real, parameter :: expMin = log(tiny(1.0))

end module numconstants
