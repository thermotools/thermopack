!-----------------------------------------------------------------------------
!> Wong-Sandler equation of state
!! Cubic EOS with temperature dependent b-parameter
!!
!! See: Wong, D. S. H. and Sandler, S. I. (1992).
!! "A theoretically correct mixing rule for cubic equations of state".
!! AIChE Journal 38: 671-680. doi: 10.1002/aic.690380505.
!!
!! Equation reference in this module is to appendix equations A1 etc. of the
!! Wong-Sandler paper.
!!
!! See also separate memo regarding the model and differentials,
!! located in the doc folder.
!!
!! \author AA, 2015-02, MH, 2015-03
!!
!-----------------------------------------------------------------------------
module wong_sandler
  use cbalpha, only: cbCalcAlphaTerm
  implicit none
  private
  save
  !
  integer, parameter :: nArrTProp = 4
  integer, parameter :: modTPolynom=1, modTFraction=2,  EqAlpha=3
  ! Model type, at moment either polynom or fraction
  ! Exeption is Alpha that can be 1/(2 + g12*g21)
  integer, parameter :: max_ws_datab = 2

  type :: GeneralTProp
    ! General expression as function of temperature
    integer :: nMod
    integer :: nDegreeOver, nDegreeUnder
    real :: aCoeff(nArrTProp)
  end type GeneralTProp

  type :: inter_ws_datadb
    character (len=8) :: eosid
    character (len=8) :: uid1, uid2
    type(GeneralTProp):: kijvalue,  Alpha, Tau12, Tau21
  end type inter_ws_datadb

  type(GeneralTProp), parameter :: EmptyTProp = &
       GeneralTProp(1,1,0, (/1.0, 0.0, 0.0, 0.0/))

  type(inter_ws_datadb), parameter :: TEstDB = &
       inter_ws_datadb("PR","CO2","AR", EmptyTProp, EmptyTProp, &
       EmptyTProp, EmptyTProp)

  type(inter_ws_datadb), dimension(max_ws_datab), parameter :: data_ws_datadb = (/ &
       inter_ws_datadb("PR","CO2","AR", EmptyTProp, EmptyTProp, &
       EmptyTProp, EmptyTProp),                            &
       inter_ws_datadb("PR","CO2","N2", EmptyTProp, EmptyTProp, &
       EmptyTProp, EmptyTProp) /)

  public :: fidel_alpha
  public :: WongSandlerMix

contains
  !>  Does calcualtions realted to the Wang-Sandler mixing model
  !-----------------------------------------------------------
  real function fidel_alpha(tau12, tau21)
    !  Calcualtes alhpa after alpha = 1/(2+g12*g21)
    ! g12 = exp(-alpha*tau12), g21 = exp(-alpha*tau21)
    ! Uses usually 6 iterations
    real, intent(in):: tau12, tau21
    real :: taus, aa, ee, ff ,dfda, da
    integer :: i
    taus = tau12+tau21
    aa = 1.0 / 2.5
    do i=1,10
      ee = exp(-taus*aa)
      ff = 2 + ee - 1/aa
      dfda = -taus*ee + 1 / aa**2
      da = -ff/dfda
      !       print *, i, aa, ff, dfda
      aa = aa - ff/dfda
      if ( abs(da) < 1E-12) then
        exit
      endif
    enddo
    fidel_alpha = aa
  end function fidel_alpha

  !-------------------------------------------
  subroutine WongSandlerMix(cbeos, T, zcomp)
    use thermopack_var, only : nc
    use cubic_eos, only : cb_eos, isHVmixModel
    use thermopack_constants, only: kRgas
    use excess_gibbs, only: gEinf, getInfinitLimitC, GetFraction
    use hyperdual_mod
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T, zcomp(nc)
    ! Local variables:
    integer ::i,j, k
    real :: r1, n
    real ::  Q, Qt, Qtt, C ,D, Dt, Dtt, rrijt, rrijtt
    real ::  QQ, QQt, QQtt, QQi(nc), QQij(nc,nc), QQit(nc)
    real, dimension(nc) :: aa, aat, aatt, bb, rr, rrt, rrtt
    real, dimension(nc) :: Qi, Di, Qit, Dit, dAExInfdNi, d2AexInfdNidT
    real, dimension(nc,nc) :: kWS, kWSt, kWStt, Qij, Dij
    real, dimension(nc,nc) :: rrij, d2AExInfdNidNj
    real :: AExInf, dAExInfdT, d2AExinfdT2
    type(hyperdual) :: Qhd, Thd, nhd(nc), nhdp(nc), Qhdp, Thdp

    ! Variable
    !  Used(f)      df/dt,      df/dni   Variable
    !  zSum         0.0           1.0       sum(zComp) is genrally not 1
    !  aa(i)        aat(i)        0.0       a_i=0.46*(RT)²/Pc* Alpha
    !  bb(i)        0.0           0.0       b_i
    !  rr(i)        rrt(i)        0.0       b_i - a_i/(R*T)
    !  kWS(i,j)     kWSt(i,j)     0.0       kij: Binary parameter
    !  rrij(i,j)    rrijt(i,j)    0.0       =0.5*(rr(i)+rr(j))*(1-kWs(i,j))
    !  AExInf       dAExInfdT     dAExInfdNi: AE = AE/(RT)
    !  Q            Qt            Qi(i)     : Q for calcualtion of a and b
    !  D            Dt            Di(i)     : D for calcualtion of a and b
    !  cbeos%a      cbeos%at      cbeos%ai(i): Variable cbeos
    !  cbeos%b      cbeos%bt      cbeos%bi(i): Variable cbeos

    n = 0.0
    do i=1,nc
      r1 = cbeos%single(i)%a !< constant ai in cubic eos
      aa(i) = cbeos%single(i)%alpha * r1
      aat(i) = cbeos%single(i)%dalphadt * r1
      aatt(i) = cbeos%single(i)%d2alphadt2 * r1
      bb(i) = cbeos%single(i)%b
      rr(i) = bb(i) - aa(i)/(kRGas*T)
      rrt(i) = -aat(i)/(kRGas*T) + aa(i)/(kRGas*T*T)
      rrtt(i) = -aatt(i)/(kRGas*T) + 2.0*aat(i)/(kRGas*T**2) &
           - 2.0*aa(i)/(kRGas*T**3)
      n = n + zComp(i)
    enddo

    ! Read kij data
    do i=1,nc
      do j=1,nc
        call GetFraction(cbeos%mixWS%f_kij(i,j), T, kWS(i,j), kWSt(i,j), kWStt(i,j))
!        kWS(i,j)  = cbeos%mixWS%kij(i,j)
!        kWSt(i,j) = 0.0 !At moment constant, later get temperature depend
!        kWStt(i,j) = 0.0 !At moment constant, later get temperature depend
      enddo
    enddo

    ! Calculate AEinf -
    call gEinf(cbeos,t,zcomp,AExInf,dAExInfdT,d2AExinfdT2,&
         dAExInfdNi,d2AexInfdNidT,d2AExInfdNidNj)

    !------Q-----Eq(A8)-------------------
    Q = 0.0
    Qt = 0.0
    Qtt = 0.0
    Qit = 0.0
    Qij = 0.0
    QQ = 0.0
    QQt = 0.0
    QQtt = 0.0
    QQit = 0.0
    QQij = 0.0

    Thd = 0.0
    nhd = 0.0
    if (isHVmixModel(cbeos%mruleidx)) then ! use HV formulation of NRTL
       ! Q, QT, QTT, Qn, QTn, Qnn
       Thd%f0 = T
       nhd(:)%f0 = zcomp

       ! T, n, and Tn derivatives
       Thd%f1 = 1
       do i=1,nc
          nhd(i)%f2 = 1
          call calc_Q_HVNRTL(cbeos, Thd, nhd, kWS, Qhd)
          QT = Qhd%f1
          Qi(i) = Qhd%f2
          QiT(i) = Qhd%f12
          nhd(i)%f2 = 0
       end do
       Thd%f1 = 0

       ! TT derivative
       Thd%f1 = 1
       Thd%f2 = 1
       call calc_Q_HVNRTL(cbeos, Thd, nhd, kWS, Qhd)
       Qtt = Qhd%f12
       Thd%f1 = 0
       Thd%f2 = 0

       Qij = 0.0
       ! nn derivative
       do i=1,nc
          nhd(i)%f1 = 1
          do j=i,nc
             nhd(j)%f2 = 1
             call calc_Q_HVNRTL(cbeos, Thd, nhd, kWS, Qhd)
             Qij(i,j) = Qhd%f12
             Qij(j,i) = Qij(i,j)
             nhd(j)%f2 = 0
          end do
          nhd(i)%f1 = 0
       end do
       Q = Qhd%f0

       ! Original calculation
       do i=1,nc
          do j=1,nc
             rrij(i,j)=0.5*(rr(i)+rr(j))*(1-kWS(i,j))
             rrijt=0.5*(rrt(i)+rrt(j))*(1-kWS(i,j)) - 0.5*(rr(i)+rr(j))*kWSt(i,j)
             rrijtt=0.5*(rrtt(i)+rrtt(j))*(1-kWS(i,j)) &
                  - (rrt(i)+rrt(j))*kWSt(i,j) &
                  - 0.5*(rr(i)+rr(j))*kWStt(i,j)
             QQ = QQ + zcomp(i)*zcomp(j)*rrij(i,j) !Eq( A8)
             QQt = QQt + zcomp(i)*zcomp(j)*rrijt
             QQtt = QQtt + zcomp(i)*zcomp(j)*rrijtt
             QQiT(i) = QQiT(i) + 2.0*rrijt*zcomp(j)
          enddo
       enddo
       do k = 1,nc
          do i=1,nc
             QQi(k) = QQi(k) + zcomp(i)*(rrij(i,k)+rrij(k,i))
             QQij(i,k) = rrij(i,k)+rrij(k,i)
          enddo
       enddo

       ! print *, "Q"
       ! print *, Q
       ! print *, QQ

       ! print *, "Qt"
       ! print *, Qt
       ! print *, QQt

       ! print *, "Qtt"
       ! print *, Qtt
       ! print *, QQtt

       ! print *, "Qi"
       ! print *, Qi
       ! print *, QQi

       ! print *, "Qit"
       ! print *, Qit
       ! print *, QQit

       ! print *, "Qij"
       ! print *, Qij
       ! print *, QQij
       ! stop "END OF COMPARISON"
    else
       do i=1,nc
          do j=1,nc
             rrij(i,j)=0.5*(rr(i)+rr(j))*(1-kWS(i,j))
             rrijt=0.5*(rrt(i)+rrt(j))*(1-kWS(i,j)) - 0.5*(rr(i)+rr(j))*kWSt(i,j)
             rrijtt=0.5*(rrtt(i)+rrtt(j))*(1-kWS(i,j)) &
                  - (rrt(i)+rrt(j))*kWSt(i,j) &
                  - 0.5*(rr(i)+rr(j))*kWStt(i,j)
             Q = Q + zcomp(i)*zcomp(j)*rrij(i,j) !Eq( A8)
             Qt = Qt + zcomp(i)*zcomp(j)*rrijt
             Qtt = Qtt + zcomp(i)*zcomp(j)*rrijtt
             QiT(i) = QiT(i) + 2.0*rrijt*zcomp(j)
          enddo
       enddo
       do k = 1,nc
          do i=1,nc
             Qi(k) = Qi(k) + zcomp(i)*(rrij(i,k)+rrij(k,i))
             Qij(i,k) = rrij(i,k)+rrij(k,i)
          enddo
       enddo
    end if

    ! print *, "Test first T-derivatives""
    ! Thdp = 0.0
    ! Thdp%f0 = Thd%f0*(1+1e-5)
    ! call cbCalcAlphaTerm(nc, cbeos, Thdp%f0)
    ! call calc_Q_HVNRTL(cbeos, Thdp, nhd, kWs, Qhdp)
    ! print *, "T"
    ! print *, ((Qhdp%f0-Qhd%f0)/(Thdp%f0-Thd%f0))
    ! print *, Qt
    ! call cbCalcAlphaTerm(nc, cbeos, Thd%f0)
    ! Thdp = Thd

    ! print *, "Test Tn-derivatives"
    ! Thdp = 0.0
    ! Thdp%f0 = Thd%f0
    ! Thdp%f1 = 1.0
    ! nhdp%f0 = nhd%f0
    ! do i=1,nc
    !    nhdp(i)%f0 = nhd(i)%f0*(1+1e-5)
    !    call calc_Q_HVNRTL(cbeos, Thdp, nhdp, kWs, Qhdp)
    !    print *, i
    !    print *, ((Qhdp%f1-Qt)/(nhdp(i)%f0-nhd(i)%f0))
    !    print *, Qit(i)
    !    nhdp(i)%f0 = nhd(i)%f0
    ! end do

    ! print *, "Test first n-derivatives""
    ! nhdp%f0 = nhd%f0
    ! do i=1,nc
    !    nhdp(i)%f0 = nhd(i)%f0*(1+1e-5)
    !    call calc_Q_HVNRTL(cbeos, Thd, nhdp, kWs, Qhdp)
    !    print *, i
    !    print *, ((Qhdp%f0-Qhd%f0)/(nhdp(i)%f0-nhd(i)%f0))
    !    print *, Qi(i)
    !    nhdp(i)%f0 = nhd(i)%f0
    ! end do

    ! print *, "Test mixed n-derivatives"
    ! nhdp%f0 = nhd%f0
    ! do i=1,nc
    !    do j=1,nc
    !       nhdp(i)%f0 = nhd(i)%f0*(1+1e-5)
    !       nhdp(j)%f1 = 1
    !       call calc_Q_HVNRTL(cbeos, Thd, nhdp, kWs, Qhdp)
    !       print *, i, j
    !       print *, ((Qhdp%f1-Qi(j))/(nhdp(i)%f0-nhd(i)%f0))
    !       print *, Qij(i, j)
    !       nhdp(j)%f1 = 0.0
    !    end do
    !    nhdp(i)%f0 = nhd(i)%f0
    ! end do

    !-----D--- Eq(A9)--------------
    C = getInfinitLimitC(cbeos) ! Note! C defined positive -> Sign change
    D = 0.0
    Dt = 0.0
    Dtt = 0.0
    do i=1,nc
      D = D + zcomp(i)*aa(i)/bb(i)
      Dt = Dt + zcomp(i) * aat(i)/bb(i)
      Dtt = Dtt + zcomp(i) * aatt(i)/bb(i)
      Di(i) = (aa(i)/bb(i)-dAExInfdNi(i)/C)/(kRGas*T)
      DiT(i) = (aat(i)/bb(i)-d2AexInfdNidT(i)/C-Di(i)*kRGas)/(kRGas*T)
      do j=1,nc
        Dij(i,j) = -d2AExInfdNidNj(i,j)/(C*kRGas*T)
      enddo
    enddo
    D = (D-AExInf/C)/(kRGas*T)
    Dt = (Dt-dAExInfdT/C-D*kRGas)/(kRGas*T)
    Dtt = (Dtt-d2AExinfdT2/C-2.0*Dt*kRGas)/(kRGas*T)

    !-------cbeos: b------------
    cbeos%b = Q / (n - D)  !Eq (A6)
    cbeos%bt = (Qt+Dt*cbeos%b)/(n-D)
    cbeos%btt = (Qtt+2.0*Dt*cbeos%bt+Dtt*cbeos%b)/(n-D)
    do i=1,nc
      cbeos%bi(i) = (Qi(i) + cbeos%b*(Di(i) - 1.0))/(n - D)
      cbeos%bit(i) = (Qit(i) + cbeos%bi(i)*Dt + &
           cbeos%bt*(Di(i) - 1.0) + cbeos%b*Dit(i))/(n - D)
    enddo

    do i=1,nc
      do j=1,nc
        cbeos%bij(i,j) = (Qij(i,j) + cbeos%bi(i)*(Di(j) - 1.0) + &
             cbeos%bi(j)*(Di(i) - 1.0) + cbeos%b*Dij(i,j))/(n - D)
      enddo
    enddo

    cbeos%sumb = cbeos%b
    !-------cbeos: a------------
    cbeos%a = Q * D * kRGas * T / (n - D)  !Eq (A7)
    cbeos%at = kRGas*(cbeos%bt*D*T + cbeos%b*Dt*T + cbeos%b*D)
    cbeos%att = kRGas*(cbeos%btt*D*T + 2.0*cbeos%bt*Dt*T &
         + 2.0*cbeos%bt*D + 2.0*cbeos%b*Dt + cbeos%b*Dtt*T)
    do k=1,nc
      cbeos%ai(k) = kRGas*T*(cbeos%bi(k)*D + cbeos%b*Di(k))
      cbeos%ait(k) = kRGas*(cbeos%bit(k)*D*T + cbeos%bi(k)*Dt*T &
           + cbeos%bi(k)*D + cbeos%bt*Di(k)*T + cbeos%b*Dit(k)*T &
           + cbeos%b*Di(k))
    enddo

    do i=1,nc
      do j=1,nc
        cbeos%aij(i,j) = kRGas*T*(cbeos%bij(i,j)*D + cbeos%bi(i)*Di(j)&
             + cbeos%bi(j)*Di(i) + cbeos%b*Dij(i,j))
      enddo
    enddo

    cbeos%suma = cbeos%a
  end subroutine WongSandlerMix


  subroutine calc_Q_HVNRTL(cbeos, Thd, nhd, kij_a, Qhd)
    use thermopack_var, only : nc
    use cubic_eos, only : cb_eos
    use thermopack_constants, only: kRgas
    use hyperdual_mod
    class(cb_eos), intent(inout) :: cbeos
    type(hyperdual), intent(in) :: Thd, nhd(nc)
    real, intent(in) :: kij_a(nc,nc)
    type(hyperdual), intent(out) :: Qhd
    ! Local variables:
    integer :: i, j, k
    real :: r1, a, a_T, a_TT, bij
    type(hyperdual) :: aahd(nc), virbin(nc,nc), aij

    aahd = 0.0
    do i=1,nc
       r1 = cbeos%single(i)%a !< constant ai in cubic eos
       a = cbeos%single(i)%alpha*r1
       a_T = cbeos%single(i)%dalphadt*r1
       a_TT = cbeos%single(i)%d2alphadt2*r1
       aahd(i)%f0 = a
       aahd(i)%f1 = Thd%f1 * a_T
       aahd(i)%f2 = Thd%f2 * a_T
       aahd(i)%f12 = (Thd%f1*Thd%f2*a_TT + Thd%f12*a_T)
    enddo

    ! Assemble hyperdual Qhd
    Qhd = 0.0
    do i=1,nc
       do j=1,nc
          aij = sqrt(aahd(i)*aahd(j)) * (1 - kij_a(i,j))
          bij = 0.5*(cbeos%single(i)%b+cbeos%single(j)%b)*(1-cbeos%lij(i,j))
          virbin(i,j) = bij - aij/(kRGas*Thd)
          ! The following yields the original original WS formulation: virbin(i,j) = 0.5*(bij/0.5 - (aahd(i)+aahd(j))/(kRGas*Thd)) * (1-kij_a(i,j))
          Qhd = Qhd + nhd(i)*nhd(j)*virbin(i,j)
       end do
    end do

  end subroutine calc_Q_HVNRTL


end module wong_sandler
