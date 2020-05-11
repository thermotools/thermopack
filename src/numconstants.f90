module numconstants
  ! Constants.
  implicit none
  save
  real, parameter :: almost_zero=tiny(1.0)
  real, parameter :: machine_prec=epsilon(1.0)
  real, parameter :: large=1.0e10
  real, parameter :: small=1.0e-12
  real, parameter :: rtol=machine_prec*1.0e-10
  real :: pi = 0.0
  real :: expMax, expMin
  !
  public
contains
  subroutine set_numconstants
    pi=4.0*atan(1.0)
    expMax = log(huge(1.0))
    expMin = log(tiny(1.0))
  end subroutine set_numconstants
end module numconstants
