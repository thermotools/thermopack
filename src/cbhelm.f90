!-------------------------------------------------------------------------------
!> Get reduced Helmholtz function (F = A^r/RT) and differentials from eoscubic
!> type. The cubiceos instance stores F and its derivatives as a function of
!> certain explicit quantities, which includes the variable T, v and n, but also
!> the cubic EoS parameters a and b that depend on (T,V,n). To get the total
!> derivatives of F wrt (T,V,n), these have to be combined in the appropriate
!> way, which is the purpose of this module.
!-------------------------------------------------------------------------------
module cbHelm
  !
  !
  use cubic_eos, only: cb_eos
  implicit none
  private
  save

  public :: cbF, cbFv, cbFvv, cbFt, cbFtt, cbFvt, cbFvvv
  public :: cbFi, cbFij, cbFiv, cbFit
  public :: cbPress, cbPv, cbPvv, cbPrst, cbPi
contains

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy for cubic equation of state
  !>
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  function cbF(cubiceos) result(F)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: F
    ! Locals
    F = cubiceos%ff
  end function cbF

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! differential wrpt. specific volume
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  function cbFv(cubiceos) result(Fv)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: Fv
    ! Locals
    Fv = cubiceos%ffv
  end function cbFv

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! second differential wrpt. specific volume
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  function cbFvv(cubiceos) result(Fvv)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: Fvv
    ! Locals
    Fvv = cubiceos%ffvv
  end function cbFvv

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! third differential wrpt. specific volume
  !! for cubic equation of state
  !!
  !> \author MH, 2015-10
  !-----------------------------------------------------------------------------
  function cbFvvv(cubiceos,T,V) result(Fvvv)
    use thermopack_var, only: kRgas
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real, intent(in) :: T,V
    real :: Fvvv
    ! Locals
    real :: pvv, pidvv

    pvv = cbPvv(cubiceos,T,V)
    pidvv = 2.0*cubiceos%sumn*kRgas*T/V**3
    Fvvv = -(pvv-pidvv)/(kRgas*T*cubiceos%sumn)
  end function cbFvvv

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! differential wrpt. temperature
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  function cbFt(cubiceos) result(Ft)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: Ft
    ! Locals
    Ft = cubiceos%fft + cubiceos%ffa*cubiceos%at  &
       + cubiceos%ffb*cubiceos%bt
  end function cbFt

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! second differential wrpt. specific volume
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  function cbFtt(cubiceos) result(Ftt)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: Ftt
    ! Locals:
    Ftt = cubiceos%fftt + 2.0*cubiceos%ffat*cubiceos%at  &
         + cubiceos%ffa * cubiceos%att                   &
         + 2.0 * cubiceos%ffbt*cubiceos%bt &
         + cubiceos%ffb * cubiceos%btt &
         + cubiceos%ffaa * cubiceos%at**2 &
        + 2.0 * cubiceos%ffab * cubiceos%at * cubiceos%bt &
        +  cubiceos%ffbb * cubiceos%bt**2
  end function cbFtt

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! differential wrpt. specific volume
  !! and temperature
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  function cbFvt(cubiceos) result(Fvt)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: Fvt
    ! Locals
    Fvt = cubiceos%ffvt + cubiceos%ffva*cubiceos%at + cubiceos%ffvb * cubiceos%bt
  end function cbFvt

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! differentials wrpt. mole numbers
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  subroutine cbFi(nc,cubiceos,Fi)
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(in) :: cubiceos
    real, dimension(nc), intent(out) :: Fi
    ! Locals
    Fi = cubiceos%ffn + cubiceos%ffa*cubiceos%ai &
         + cubiceos%ffb*cubiceos%bi &
         + cubiceos%ffc*cubiceos%ci
  end subroutine cbFi

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! second differentials wrpt. mole numbers
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  subroutine cbFij(nc,cubiceos,Fij)
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(in) :: cubiceos
    real, dimension(nc,nc), intent(out) :: Fij
    ! Locals
    integer :: i,j
    do i=1,nc
      do j=1,nc
        Fij(i,j) = cubiceos%ffnb*(cubiceos%bi(i) + cubiceos%bi(j)) &
                 + (cubiceos%ffab*cubiceos%ai(j) &
                 +  cubiceos%ffbb*cubiceos%bi(j) &
                 +  cubiceos%ffbc*cubiceos%ci(j)) * cubiceos%bi(i) &
                 + cubiceos%ffb*cubiceos%bij(i,j) &
                 + (cubiceos%ffaa*cubiceos%ai(j) &
                 +  cubiceos%ffab*cubiceos%bi(j) &
                 +  cubiceos%ffac*cubiceos%ci(j)) * cubiceos%ai(i) &
                 +  cubiceos%ffa*cubiceos%aij(i,j) &
                 + (cubiceos%ffac*cubiceos%ai(j) &
                 +  cubiceos%ffbc*cubiceos%bi(j) &
                 +  cubiceos%ffcc*cubiceos%ci(j)) * cubiceos%ci(i) &
                 + cubiceos%ffc*cubiceos%cij(i,j)
      enddo
    enddo
  end subroutine cbFij

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! second differentials wrpt. mole numbers
  !! and temperature
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  subroutine cbFiT(nc,cubiceos,FiT)
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(in) :: cubiceos
    real, dimension(nc), intent(out) :: FiT
    ! Locals
    FiT = (cubiceos%ffat + cubiceos%ffaa*cubiceos%at)*cubiceos%ai &
         + (cubiceos%ffbt+cubiceos%ffab*cubiceos%at)*cubiceos%bi &
         + (cubiceos%ffct+cubiceos%ffac*cubiceos%at)*cubiceos%ci  &
         + cubiceos%ffa*cubiceos%ait
    FiT = FiT + cubiceos%ffab * cubiceos%bt * cubiceos%ai &
         + cubiceos%ffbb*cubiceos%bt*cubiceos%bi   &
         + cubiceos%ffbc*cubiceos%bt*cubiceos%ci   &
         + cubiceos%ffb*cubiceos%bit
    Fit = Fit + cubiceos%ffnt &
        + cubiceos%ffna * cubiceos%at &
        + cubiceos%ffnb * cubiceos%bt
  end subroutine cbFiT

  !-----------------------------------------------------------------------------
  !> Get reduced residual Helmholtz energy
  !! second differentials wrpt. mole numbers
  !! and volume
  !! for cubic equation of state
  !!
  !> \author MH, 2013-11-30
  !-----------------------------------------------------------------------------
  subroutine cbFiv(nc,cubiceos,Fiv)
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(in) :: cubiceos
    real, dimension(nc), intent(out) :: Fiv
    ! Locals
    Fiv = cubiceos%ffnv&
        + cubiceos%ffva*cubiceos%ai &
        + cubiceos%ffvb*cubiceos%bi &
        + cubiceos%ffvc*cubiceos%ci
  end subroutine cbFiv

  !-----------------------------------------------------------------------------
  !> Pressure from cubic equation of state
  !!
  !> \author MH, 2014-04
  !-----------------------------------------------------------------------------
  function cbPress(cubiceos,T,V) result(p)
    use thermopack_var, only: kRgas
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real, intent(in) :: T,V
    real :: p
    ! Locals
    p = cubiceos%sumn*kRgas*T/(V-cubiceos%sumb) &
         - cubiceos%suma/&
         ((V - cubiceos%m1*cubiceos%sumb)*(V - cubiceos%m2*cubiceos%sumb))
  end function cbPress

  !-----------------------------------------------------------------------------
  !> Pressure differential wrpt. volume from cubic equation of state
  !!
  !> \author MH, 2015-02
  !-----------------------------------------------------------------------------
  function cbPv(cubiceos) result(pv)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: pv
    ! Locals
    pv = cubiceos%pv
  end function cbPv

  !-----------------------------------------------------------------------------
  !> Second pressure differential wrpt. volume from cubic equation of state
  !!
  !> \author MH, 2015-02
  !-----------------------------------------------------------------------------
  function cbPvv(cubiceos,T,V) result(pvv)
    use thermopack_var, only: kRgas
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real, intent(in) :: T,V
    real :: pvv
    ! Locals
    real :: n0, n1, n2, den, lnn

    n0 = v - cubiceos%sumb
    n1 = v - cubiceos%m1 * cubiceos%sumb
    n2 = v - cubiceos%m2 * cubiceos%sumb
    den = n1*n2
    lnn = n2/n1

    pvv = cubiceos%sumn*2.0*kRgas*T/n0**3 - &
         2.0*cubiceos%suma/den**2*(1.0 + lnn + 1.0/lnn)
  end function cbPvv

  !-----------------------------------------------------------------------------
  !> Pressure differential wrpt. temperature from cubic equation of state
  !!
  !> \author MH, 2015-03
  !-----------------------------------------------------------------------------
  function cbPrst(cubiceos) result(pt)
    implicit none
    class(cb_eos), intent(in) :: cubiceos
    real :: pt
    ! Locals
    pt = cubiceos%pt+cubiceos%pa*cubiceos%at + &
         cubiceos%pb*cubiceos%bt
  end function cbPrst

  !-----------------------------------------------------------------------------
  !> Get pressure differential wrpt. mole number i
  !! for cubic equation of state
  !!
  !> \author MH, 2015-03
  !-----------------------------------------------------------------------------
  subroutine cbPi(nc,cubiceos,Pi)
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(in) :: cubiceos
    real, dimension(nc), intent(out) :: Pi
    ! Locals
    Pi=cubiceos%pn+cubiceos%pa*cubiceos%ai &
         + cubiceos%pb * cubiceos%bi &
         + cubiceos%pc * cubiceos%ci
  end subroutine cbPi


end module cbHelm
