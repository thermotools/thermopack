!> Excess Gibbs Energy Models
!!
!! Currently support Huron-Vidal and NRTL
!!
!! Based on Geir Skaugens implementation of Huron-Vidal, and
!! Anders Austegards implementation of Wong-Sandler
!!
!! \author Morten Hammer
!! \date 2015-03
module excess_gibbs
  implicit none
  save

  public :: gEinf, getInfinitLimitC
  public :: ExcessGibbsMix, GetPoly, GetFraction

contains

  !< Get residual Helmholtz energy parameter C
  !! at the infinite pressure limit.
  function getInfinitLimitC(cbeos) result(C)
    use cubic_eos, only: cb_eos
    use eosdata
    implicit none
    class(cb_eos), intent(in) :: cbeos
    real :: C
    ! Locals
    real, parameter :: LN2 = log(2.0)
    real, parameter :: C_PR = -log(sqrt(2.0)-1.0)/sqrt(2.0)
    select case (cbeos%subeosidx)
    case (cbVdW)
      C = 1.0
    case (cbSRK,cspSRK,cpaSRK)
      C = LN2
    case (cbPR,cspPR,cpaPR)
      C = C_PR
    case (cbSW,cbPT)
      C = -log((1.0-cbeos%m1)/(1.0-cbeos%m2))/(cbeos%m1-cbeos%m2)
    case default
      call stoperror('excess_gibbs::getInfinitLimitC: Wrong EOS!')
    end select
  end function getInfinitLimitC

  !< Get residual Helmholtz energy parameter C
  !! at the zero pressure limit.
  function getZeroLimitC(cbeos) result(C)
    use cubic_eos, only: cb_eos
    use eosdata
    implicit none
    class(cb_eos), intent(in) :: cbeos
    real :: C
    ! Locals
    select case (cbeos%subeosidx)
    case (cbVdW)
      C = 0.55
    case (cbSRK,cspSRK)
      C = 0.593
    case (cbPR,cspPR)
      C = 0.53087
    case default
      call stoperror('excess_gibbs::getZeroLimitC: Wrong EOS!')
    end select
  end function getZeroLimitC

  !< Set up tau, C and alpha paramater for different models.
  subroutine getGeParam(cbeos,t,tau,dtaudT,d2taudT2,Cij,alpha)
    use thermopack_var, only: nce
    use eosdata
    use cubic_eos, only: cb_eos, isHVmixModel, cbMixNRTL, cbMixWongSandler, cbMixWSCPA
    implicit none
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: t !> Temperature
    real, intent(out) :: tau(nce,nce),dtaudT(nce,nce),d2taudT2(nce,nce)
    real, intent(out) :: alpha(nce,nce),Cij(nce,nce)
    ! Locals

    if ( isHVmixModel(cbeos%mruleidx) .OR. &
         cbeos%mruleidx == cbMixNRTL) then
      call getHvNRTLParam(cbeos,t,tau,dtaudT,d2taudT2,Cij,alpha)
   else if (cbeos%mruleidx==cbMixWongSandler .or. cbeos%mruleidx==cbMixWSCPA) then
      call getWSParam(cbeos,t,tau,dtaudT,d2taudT2,Cij,alpha)
    else
      call stoperror('excess_gibbs::getGeParam: Wrong mixing rule!')
    endif

  end subroutine getGeParam

  !< Set up tau, C and alpha paramater for Huron-Vidal or NRTL.
  subroutine getHvNRTLParam(cbeos,t,tau,dtaudT,d2taudT2,Cij,alpha)
    use thermopack_var, only: nce
    use eosdata
    use cubic_eos, only: cb_eos, cbMixHuronVidal, cbMixNRTL
    use thermopack_constants, only: kRgas
    implicit none
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: t !> Temperature
    real, intent(out) :: tau(nce,nce),dtaudT(nce,nce),d2taudT2(nce,nce)
    real, intent(out) :: alpha(nce,nce),Cij(nce,nce)
    ! Locals
    real :: t1,t2,t3,t4
    real :: dt4dT, d2t4dT2
    real :: gii,gjj, dgiidT, d2giidT2
    real :: bi,bj,LAMBDA_GEINF
    integer :: i, j

    LAMBDA_GEINF = getInfinitLimitC(cbeos)

    ! Calculating the temperature dependent tauij and Cij
    do i=1,nce
      bi = cbeos%single(i)%b
      do j = 1,nce
        bj = cbeos%single(j)%b
        if (i .eq. j) then
          tau(j,i) = 0.0D0
          dtaudT(j,i) = 0.0D0
          d2taudT2(j,i) = 0.0D0
          alpha(j,i) = 0.0
          Cij(j,i) = 1.0
        else
          alpha(j,i) = cbeos%mixGE%alpha(j,i)
          if (cbeos%mixGE%correlation(j,i) > 0) then
            if (cbeos%mixGE%mGE .eq. cbMixHuronVidal .or. &
                cbeos%mixGE%mGE .eq. cbMixNRTL) then
              ! Beware!!
              ! The coefficients from literature for dgji correlates directly to tau(j,i)
              ! NOT dgji as tau(j,i) = dgji/R
              ! linear
              tau(j,i) = (cbeos%mixGE%aGE(j,i)/T + cbeos%mixGE%bGE(j,i))
              dtaudT(j,i) = -cbeos%mixGE%aGE(j,i)/(T*T)
              d2taudT2(j,i) = 2.0*cbeos%mixGE%aGE(j,i)/(T*T*T)
            else
              if (cbeos%mixGE%correlation(j,i) == 1) then
                ! 2'nd order
                tau(j,i) = (cbeos%mixGE%aGE(j,i)/T + cbeos%mixGE%bGE(j,i)+ cbeos%mixGE%cGE(j,i)*T)
                dtaudT(j,i) = (-cbeos%mixGE%aGE(j,i)/(T*T) + cbeos%mixGE%cGE(j,i))
                d2taudT2(j,i) = 2.0*cbeos%mixGE%aGE(j,i)/(T*T*T)
              else ! cbeos%mixGE%correlation(j,i) = 2
                ! Maribo-Mogensen
                tau(j,i) = (cbeos%mixGE%aGE(j,i) + cbeos%mixGE%bGE(j,i)*&
                     ((1.0-T/cbeos%mixGE%cGE(j,i))**2 -&
                     (1.0-298.15/cbeos%mixGE%cGE(j,i))**2))/T
                dtaudT(j,i) = -tau(j,i)/T - 2.0*cbeos%mixGE%bGE(j,i)*&
                     (1.0-T/cbeos%mixGE%cGE(j,i))/(cbeos%mixGE%cGE(j,i)*T)
                d2taudT2(j,i) = -2.0* (dtaudT(j,i) -&
                     cbeos%mixGE%bGE(j,i)/cbeos%mixGE%cGE(j,i)**2)/T
              endif
            endif
            Cij(j,i) = exp(-alpha(j,i) * tau(j,i))
          else ! revert to classic vdW mixing rule
            t1 = -2.0*sqrt(bi*bj)/(bi+bj)
            gii = -cbeos%single(i)%alpha*cbeos%single(i)%a*LAMBDA_GEINF/bi ! can calculate these outside of j-loop ...
            gjj = -cbeos%single(j)%alpha*cbeos%single(j)%a*LAMBDA_GEINF/bj

            t2 = cbeos%single(i)%a*LAMBDA_GEINF/bi
            t3 = t2*cbeos%single(j)%a*LAMBDA_GEINF/bj
            t4 = sqrt(cbeos%single(i)%alpha*cbeos%single(j)%alpha*t3)

            tau(j,i) = (t1 * t4*(1-cbeos%kij(j,i)) - gii) /(kRgas*T)

            dt4dT = t3*(cbeos%single(i)%alpha*cbeos%single(j)%dalphadT &
                 +cbeos%single(j)%alpha*cbeos%single(i)%dalphadT)/(2*t4)
            d2t4dT2 = t3*(cbeos%single(i)%alpha*cbeos%single(j)%d2alphadT2 &
                 +2*cbeos%single(i)%dalphadT*cbeos%single(j)%dalphadT+ &
                 cbeos%single(j)%alpha*cbeos%single(i)%d2alphadT2)/(2*t4) &
                 - dt4dT*dt4dT/t4
            dgiidT = -t2*cbeos%single(i)%dalphadT
            d2giidT2 = -t2*cbeos%single(i)%d2alphadT2

            dtaudT(j,i) = ((1-cbeos%kij(j,i))*t1*dt4dT-dgiidT)/(kRgas*T) &
                 - ((1-cbeos%kij(j,i))*t1*t4-gii)/(kRgas*T*T)

            d2taudT2(j,i) = ((1-cbeos%kij(j,i))*t1*d2t4dT2 - d2giidT2)/(kRgas*T) &
                 - 2*((1-cbeos%kij(j,i))*t1*dt4dT-dgiidT)/(kRgas*T*T) &
                 + 2*((1-cbeos%kij(j,i))*t1*t4-gii)/(kRgas*T*T*T)

            if (cbeos%mixGE%mGE .eq. cbMixNRTL) then
              Cij(j,i) = cbeos%single(j)%b
            else
              Cij(j,i) = 1.0 !exp(-alpha(j,i) * tau(j,i))
            endif
          endif
        endif ! if i <> j

        if (cbeos%cubic_verbose) then
          write(*,*) 'Cji: i: ',i,' j: ',j,' alphaHV: ', alpha(j,i)

          write (*,*) 'j: ',j,' i: ',i
          write(*,*) 'tau: ',tau(j,i)
          write(*,*) 'dtaudt: ',dtaudt(j,i)
          write(*,*) 'd2taudt2: ',d2taudt2(j,i)
        endif
      enddo ! j
    enddo !i
  end subroutine getHvNRTLParam

  !----------------------------------------
  subroutine GetPoly(p, n, x, y, yd ,ydd)
    integer, intent(in):: n !>Deggree, uses p(1)..p(n+1)
    real, intent(in):: p(n+1),x
    real, intent(out):: y,yd,ydd
    integer i
    real s, sd, sdd, xx

    if ( x==0) then
      y = p(1)
      if (n>0) yd = p(2)
      if (n>1) ydd = 0.5*p(3)
      return
    endif
    s=0; sd=0; sdd=0
    do i=0, n
      xx = x**i
      s = s + p(i+1) * xx
      sd = sd + i*p(i+1)*xx/x
      sdd = sdd + i*(i-1)*p(i+1)*xx/(x*x)
    enddo
    y=s
    yd = sd
    ydd =  sdd
  end subroutine GetPoly

  !-----------------------------------------
  subroutine GetFraction(Frac, x, y, yd, ydd)
    use cubic_eos, only: Fraction, nDegreePoly
    type(Fraction), intent(in):: Frac
    real, intent(in) :: x
    real, intent(out):: y, yd, ydd  !<y and its derived
    real :: ynum, ynumd, ynumdd, yden, ydend, ydendd

    call GetPoly(Frac%pNum, nDegreePoly, x, ynum, ynumd, ynumdd)
    call GetPoly(Frac%pDen, nDegreePoly, x, yden, ydend, ydendd)

    if (yden == 0) then
      y=0; yd=0; ydd=0
      print *,'GetFraction: yden = 0, return szero'
      return
    endif

    y = ynum/yden
    yd = (ynumd*yden - ynum*ydend) / yden**2
    ydd = (ynumdd*yden**2 - ynum*ydendd*yden - 2*ynumd*yden*ydend + 2*ynum*ydend**2)/yden**3

  end subroutine GetFraction

  !-------------------------------------------------------------------------
  !< Set up tau, C and alpha paramater for Wong-Sandler.
  subroutine getWSParam(cbeos,t,tau,dtaudT,d2taudT2,Cij,alpha)
    use thermopack_var, only: nce
    use eosdata
    use cubic_eos, only: cb_eos
    !use thermopack_constants, only: kRgas
    implicit none
    class(cb_eos), intent(in) :: cbeos
    real, intent(in) :: t !> Temperature
    real, intent(out) :: tau(nce,nce),dtaudT(nce,nce),d2taudT2(nce,nce)
    real, intent(out) :: alpha(nce,nce),Cij(nce,nce)
    ! Locals
    !real :: LAMBDA_GEINF
    real :: bi,bj
    integer :: i,j

    !LAMBDA_GEINF = getInfinitLimitC(cbeos)

    ! Calculating the temperature dependent tauij and Cij
    do i=1,nce
      bi = cbeos%single(i)%b
      do j = 1,nce
        bj = cbeos%single(j)%b
        if (i .eq. j) then
          tau(i,j) = 0.0
          dtaudT(i,j) = 0.0
          d2taudT2(i,j) = 0.0
          alpha(i,j) = 0.0
        else
          if (abs(cbeos%mixWS%alphaij(i,j)) > 1.0E-20) then
            call GetFraction(cbeos%mixWS%f_tauij(i,j), T, tau(i,j), dtaudT(i,j), d2taudT2(i,j))
!           tau(i,j) = cbeos%mixWS%tauij(i,j)
            alpha(i,j) = cbeos%mixWS%alphaij(i,j)
!           dtaudT(i,j) = 0.0
!           d2taudT2(i,j) = 0.0
          else ! revert to classic vdW mixing rule
            !call stoperror('Wong-Sandler must have interaction parameters for all binaries!')
            ! TODO: Verify and implement relations of Twu.
            ! For now set zero interaction
            tau(i,j) = 0.0
            alpha(i,j) = 1.0
            dtaudT(i,j) = 0.0
            d2taudT2(i,j) = 0.0
          endif
        endif ! if i <> j
        Cij(i,j) = exp(-alpha(i,j) * tau(i,j))

        if (cbeos%cubic_verbose) then
          write (*,*) 'j: ',j,' i: ',i
          write(*,*) 'Cji:', Cij(i,j)
          write(*,*) 'alphaWS: ', alpha(j,i)
          write(*,*) 'tau: ',tau(j,i)
          write(*,*) 'dtaudt: ',dtaudt(j,i)
          write(*,*) 'd2taudt2: ',d2taudt2(j,i)
        endif
      enddo ! j
    enddo !i
  end subroutine getWSParam

  !< Excess Gibbs energy
  subroutine gExcess(cbeos,t,n,gExInf,dgExInfdT,d2gExinfdT2,&
       dGExInfdNi,d2GexInfdNidT,d2GExInfdNidNj)
    use eosdata
    use cubic_eos, only: cb_eos, cbMixUNIFAC
    use thermopack_var, only: nce
    use unifac, only: Ge_UNIFAC_GH_SG, unifdb
    !use thermopack_constants, only: kRgas
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t, n(nce)
    real, intent(out) :: gExInf, dgExInfdT, d2gExinfdT2
    real, intent(out) :: dGExInfdNi(nce), d2GexInfdNidT(nce), d2GExInfdNidNj(nce,nce)
    !
    !real :: eps
    !real :: t2, n2(nce)
    !real :: gExInf_2, dgExInfdT_2, d2gExinfdT2_2
    !real  :: dGExInfdNi_2(nce), d2GexInfdNidT_2(nce), d2GExInfdNidNj_2(nce,nce)

    if (cbeos%mruleidx == cbMixUNIFAC) then
      call Ge_UNIFAC_GH_SG(n,T,unifdb,gExInf,dgExInfdT,d2gExinfdT2,&
           dGExInfdNi,d2GexInfdNidT,d2GExInfdNidNj)

      ! print *,'n',n
     !  print *,'T',T
     !  print *,'Ge',gExInf
     !  print *,'Ln of activity coeff',dGExInfdNi/(T*kRgas)
     !  print *,'Activity coeff',exp(dGExInfdNi/(T*kRgas))

     !  eps = 1.0e-7
     !  n2 = n
     !  n2(1) = n2(1) + eps
     !  call Ge_UNIFAC_GH_SG(n2,T,unifdb,gExInf_2,dgExInfdT_2,d2gExinfdT2_2,&
     !       dGExInfdNi_2,d2GexInfdNidT_2,d2GExInfdNidNj_2)

     !  print *,'dGExInfdNi(1): ',(gExInf_2-gExInf)/eps,dGExInfdNi(1)
     !  print *,'gamma(1): ',(gExInf_2-gExInf)/(eps*T*kRgas),dGExInfdNi(1)/(T*kRgas)
     !  print *,'d2GExInfdNidNj(:,1): ',(dGExInfdNi_2-dGExInfdNi)/eps,d2GExInfdNidNj(:,1)

     ! ! stop
     !  n2 = n
     !  n2(2) = n2(2) + eps
     !  call Ge_UNIFAC_GH_SG(n2,T,unifdb,gExInf_2,dgExInfdT_2,d2gExinfdT2_2,&
     !       dGExInfdNi_2,d2GexInfdNidT_2,d2GExInfdNidNj_2)

     !  print *,'dGExInfdNi(2): ',(gExInf_2-gExInf)/eps,dGExInfdNi(2)
     !  print *,'d2GExInfdNidNj(:,1): ',(dGExInfdNi_2-dGExInfdNi)/eps,d2GExInfdNidNj(:,2)

     !  n2 = n
     !  T2 = T + eps
     !  call Ge_UNIFAC_GH_SG(n2,T2,unifdb,gExInf_2,dgExInfdT_2,d2gExinfdT2_2,&
     !       dGExInfdNi_2,d2GexInfdNidT_2,d2GExInfdNidNj_2)
     !  print *,'dgExInfdT: ',(gExInf_2-gExInf)/eps,dgExInfdT
     !  print *,'d2gExInfdT2: ',(dgExInfdT_2-dgExInfdT)/eps,d2gExInfdT2
     !  print *,'d2GexInfdNidT: ',(dGExInfdNi_2-dGExInfdNi)/eps,d2GexInfdNidT
     !  stop
    else
      call gEinf(cbeos,t,n,gExInf,dgExInfdT,d2gExinfdT2,&
           dGExInfdNi,d2GexInfdNidT,d2GExInfdNidNj)
    endif
  end subroutine gExcess

  !< NRTL type excess Gibbs energy at infinite pressure
  subroutine gEinf(cbeos,t,zcomp,gExInf,dgExInfdT,d2gExinfdT2,&
       dGExInfdNi,d2GexInfdNidT,d2GExInfdNidNj)
    use eosdata
    use cubic_eos, only: cb_eos, isHVmixModel
    use thermopack_constants, only: kRgas
    use thermopack_var, only: nce
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t, zcomp(nce)
    real, intent(out) :: gExInf, dgExInfdT, d2gExinfdT2
    real, intent(out) :: dGExInfdNi(nce), d2GexInfdNidT(nce), d2GExInfdNidNj(nce,nce)
    ! Locals
    real :: tau(nce,nce),dtaudT(nce,nce), d2taudT2(nce,nce),Cij(nce,nce),alpha(nce,nce)
    real :: gamma1(nce),gamma2(nce),gamma3(nce), gamma4(nce),gamma5(nce),&
         gamma6(nce)
    real :: fracin1(nce)
    real :: sum2i, sum3i, sum4i, sum5i, sum6i,sum7i,sum8i
    real :: sum1ji, sum2ji, sum3ji, sum4ji, sum5ji, sum6ji, sum7ji
    real :: sumi, sumj, sumij
    real :: bi,bj,b_array(nce),aa(nce),daadT(nce),d2aadT2(nce)
    integer :: ii, i, j

    ! Start by calculating the temperature dependent tauij and Cij
    call getGeParam(cbeos,t,tau,dtaudT,d2taudT2,Cij,alpha)

    ! Are we in HV or NRTL mode
    if (isHVmixModel(cbeos%mruleidx)) then
      do i=1,nce
        b_array(i) = cbeos%single(i)%b
      enddo
    else !NRTL mode: cbMixNRTL and cbMixWongSandler
      b_array = 1.0
    endif

    ! Map alpha*a with differentials
    do i=1,nce
      aa(i) = cbeos%single(i)%alpha*cbeos%single(i)%a
      daadT(i) = cbeos%single(i)%dalphadT*cbeos%single(i)%a
      d2aadT2(i) = cbeos%single(i)%d2alphadT2*cbeos%single(i)%a
    enddo

    ! The a, b and c-coefficients are constants stored in the block-data structure
    ! originating from an tpmixdata.inp - file

    do i=1,nce
      ! Inner sums very often used later
      ! Stored in array for the component
      gamma1(i) = 0.0D0
      gamma2(i) = 0.0D0
      gamma3(i) = 0.0D0
      gamma4(i) = 0.0D0
      gamma5(i) = 0.0D0
      gamma6(i) = 0.0D0

      fracin1(i) = 0.0D0

      bi = b_array(i)
      do j = 1,nce
        bj = b_array(j)

        ! Inner sums often used ...
        ! Stored with the index of component "i" in the interaction energies Cji(j,i) and tau(j,i)

        gamma1(i) = gamma1(i) + tau(j,i) *  bj * zcomp(j) * Cij(j,i)
        gamma2(i) = gamma2(i) +             bj * zcomp(j) * Cij(j,i)
        gamma3(i) = gamma3(i) + dtaudT(j,i)*bj * zcomp(j) * Cij(j,i) * alpha(j,i)
        gamma4(i) = gamma4(i) + dtaudT(j,i)*bj * zcomp(j) * Cij(j,i) * (1-tau(j,i)*alpha(j,i))
        gamma5(i) = gamma5(i) +             bj * zcomp(j) * Cij(j,i) * alpha(j,i) &
             *(d2taudT2(j,i) - alpha(j,i)*dtaudT(j,i)*dtaudT(j,i))
        gamma6(i) = gamma6(i) +             bj * zcomp(j) * Cij(j,i) &
             * (d2taudT2(j,i) *(1 - alpha(j,i)*tau(j,i))  &
             - (dtaudT(j,i)*dtaudT(j,i) * alpha(j,i)*(2.0 - tau(j,i)*alpha(j,i))))
        if (cbeos%cubic_verbose) then
          write(*,*) 'tau_ji   (',j,',',i,') = ',tau(j,i)
          write(*,*) 'dtau_ji  (',j,',',i,') = ',dtaudt(j,i)
          write(*,*) 'd2tau_ji (',j,',',i,') = ',d2taudt2(j,i)
          write(*,*) 'cij_ji   (',j,',',i,') = ',cij(j,i)
          write(*,*) 'ghv_ji   (',j,',',i,') = ',tau(j,i)*(kRgas*T)
        endif
      enddo
      ! Some often used relations
      fracin1(i) = gamma1(i)*gamma3(i)/(gamma2(i)*gamma2(i))

      if (cbeos%cubic_verbose) then
        write (*,*) 'gamma1(',i,'): ',gamma1(i)
        write (*,*) 'gamma2(',i,'): ',gamma2(i)
        write (*,*) 'gamma3(',i,'): ',gamma3(i)
        write (*,*) 'gamma4(',i,'): ',gamma4(i)
        write (*,*) 'gamma5(',i,'): ',gamma5(i)
        write (*,*) 'gamma6(',i,'): ',gamma6(i)

        write (*,*) 'fracin1(',i,'): ',fracin1(i)
      endif
    enddo

    ! Start developing mixing rule
    sum2i = 0.0D0
    sum3i = 0.0D0
    sum4i = 0.0D0
    sum5i = 0.0D0
    sum6i = 0.0D0
    sum7i = 0.0D0
    sum8i = 0.0D0

    do i=1,nce
      ! for GE_inf ..
      sum2i = sum2i + zcomp(i)* gamma1(i)/gamma2(i)
      ! for dGE/dT
      sum3i = sum3i + zcomp(i)* fracin1(i)
      sum4i = sum4i + zcomp(i)* (gamma4(i)/ gamma2(i))
      ! for d2GE/dT2
      sum5i = sum5i + zcomp(i)* (fracin1(i) + gamma4(i)/gamma2(i))
      sum6i = sum6i + zcomp(i)* (2* fracin1(i) * gamma3(i)/gamma2(i))
      sum7i = sum7i + zcomp(i) * (2*gamma4(i)*gamma3(i) + gamma1(i)*gamma5(i))/(gamma2(i)*gamma2(i))
      sum8i = sum8i + zcomp(i)* gamma6(i)/gamma2(i)
    enddo !i
    !
    ! GexInf and all the derivatives could be programmed as a separate subroutine.
    ! Thus: Differenent models than the NRTL model can be applied later..
    !

    gExInf      = kRgas* T * sum2i                         ! == GEinf
    dgExInfdT   = kRgas*(sum2i + T*(sum3i + sum4i))        ! dGEinf/dT
    d2gExInfdT2 = kRgas*(2*sum5i+T*(sum6i +sum7i + sum8i)) ! d2GEinf/dT2

    if (cbeos%cubic_verbose) then
      write(*,*) 'sum2i = ',sum2i
      write(*,*) 'sum3i = ',sum3i
      write(*,*) 'sum4i = ',sum4i

      !       write(*,*) 'sum5i = ',sum5i
      !       write(*,*) 'sum6i = ',sum6i
      !       write(*,*) 'sum7i = ',sum7i
      !       write(*,*) 'sum8i = ',sum8i
      write(*,*) 'gexInf = ',gExInf
      write(*,*) 'dgexInfdT = ',dgExInfdT
      write(*,*) 'd2gexInfdT2 = ',d2gExInfdT2
    endif

    ! Needed for composition derivatives - new outer loop ... for each component i
    do i=1,nce
      sum1ji = 0.0D0
      sum2ji = 0.0D0
      sum3ji = 0.0D0
      sum4ji = 0.0D0
      sum5ji = 0.0D0
      sum6ji = 0.0D0
      sum7ji = 0.0D0

      bi=b_array(i)
      do j=1,nce
        ! for dGE/dNi
        sum1ji = sum1ji + zcomp(j)*bi*Cij(i,j)*tau(i,j)/gamma2(j)
        sum2ji = sum2ji + zcomp(j)*bi*Cij(i,j)*gamma1(j)/(gamma2(j)*gamma2(j))
        ! for d2GE/dNidT
        sum3ji = sum3ji + zcomp(j)*bi*Cij(i,j)*dtaudT(i,j)*(1.0-alpha(i,j)*tau(i,j))/gamma2(j)
        sum4ji = sum4ji + zcomp(j)*bi*Cij(i,j)*tau(i,j)*gamma3(j)/(gamma2(j)*gamma2(j))
        sum5ji = sum5ji + zcomp(j)*bi*Cij(i,j)*dTaudT(i,j)*alpha(i,j)*gamma1(j)/(gamma2(j)*gamma2(j))
        sum6ji = sum6ji + zcomp(j)*bi*Cij(i,j)*gamma4(j)/(gamma2(j)*gamma2(j))
        sum7ji = sum7ji + zcomp(j)*bi*Cij(i,j)*fracin1(j)/gamma2(j)
      enddo !j

      dGexInfdNi(i) = kRgas*T* (gamma1(i)/gamma2(i) + sum1ji - sum2ji)
      d2GexInfdNidT(i) = (1/T)* dGexInfdNi(i) &
           + kRgas*T*(fracin1(i) + gamma4(i)/gamma2(i) + sum3ji + sum4ji + sum5ji - sum6ji - 2*sum7ji)

      if (cbeos%cubic_verbose) then
        !          write (*,*) 'sum0ji(',i,'): ',gamma1(i)/gamma2(i)
        !          write (*,*) 'sum1ji(',i,'): ',sum1ji
        !          write (*,*) 'sum2ji(',i,'): ',-sum2ji
        write (*,*) 'dGexInfdNi(',i,'): ',dGexInfdNi(i)

        !          write (*,*) 'sum3ji(',i,'): ', sum3ji
        !          write (*,*) 'sum4ji(',i,'): ', sum4ji
        !          write (*,*) 'sum5ji(',i,'): ', sum5ji
        !          write (*,*) 'sum6ji(',i,'): ', sum6ji
        !          write (*,*) 'sum7ji(',i,'): ', sum7ji
        write (*,*) 'd2GexInfdNidT(',i,'): ',d2GexInfdNidT(i)
      endif

    enddo ! i

    ! Second derivatives
    ! Store some inner sums
    do i=1,nce
      bi = b_array(i)
      do j=1,nce
        !             if (j.ne.i) then ! ! j == i is OK Cii = 1 tau(ii) = 0 gamma(ii) has a value
        sumi = 0.0D0
        sumj = 0.0D0
        sumij = 0.0D0
        bj = b_array(j)
        do ii=1,nce
          sumi = sumi + zcomp(ii)*(bj*Cij(j,ii))*tau(i,ii)*bi*Cij(i,ii)/(gamma2(ii)*gamma2(ii))
          sumj = sumj + zcomp(ii)*(bi*Cij(i,ii))*tau(j,ii)*bj*Cij(j,ii)/(gamma2(ii)*gamma2(ii))
          sumij= sumij+ zcomp(ii)*bi*bj*Cij(i,ii)*Cij(j,ii)*gamma1(ii)/(gamma2(ii)*gamma2(ii)*gamma2(ii))
        enddo ! ii

        d2GexInfdNidNj(i,j) = kRgas*T*(tau(j,i)*bj*cij(j,i)/gamma2(i) &
             - bj*Cij(j,i)*gamma1(i)/(gamma2(i)*gamma2(i)) &
             + tau(i,j)*bi*Cij(i,j)/gamma2(j) &
             - sumi &
             - bi*Cij(i,j)*gamma1(j)/(gamma2(j)*gamma2(j)) &
             - sumj &
             + 2*sumij)

        if (cbeos%cubic_verbose) then
          !             write(*,*) 'sumi  ',sumi
          !             write(*,*) 'sumj  ',sumj
          !             write(*,*) 'sumij ',sumij
          write(*,*) 'd2GexInfdNidNj(',i,',',j,'): ',d2GexInfdNidNj(i,j)
        endif
        !             endif
      enddo ! j
    enddo !i

  end subroutine gEinf

  !< Excess gibbs energy
  subroutine ExcessGibbsMix(cbeos,t,n)
    use cubic_eos, only: cb_eos, cbMixUNIFAC
    use thermopack_var, only: nce
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t, n(nce)
    ! Locals
    real :: gEx, dgExdT, d2gExdT2
    real :: dGExdNi(nce), d2GexdNidT(nce), d2GExdNidNj(nce,nce)
    real :: LAMBDA_GE ! Which Lambda to use in the GE-model, dependent of the Cubic EOS
    real :: aa(nce),daadT(nce),d2aadT2(nce)
    real :: b_ii(nce), A, B, Bn(nce), An(nce)!, Bnn(nce,nce)
    integer :: i,j
    real :: W, WT, WTT, Wn(nce), WnT(nce), Wnn(nce,nce)

    call gExcess(cbeos,t,n,gEx,dgExdT,d2gExdT2,&
         dGExdNi,d2GexdNidT,d2GExdNidNj)

    if (cbeos%mruleidx == cbMixUNIFAC) then
      LAMBDA_GE = getZeroLimitC(cbeos)
    else
      LAMBDA_GE = getInfinitLimitC(cbeos)
    endif

    ! Excess gibbs energy at zero pressure - b contribution
    ! Not yet included
    W = 0.0
    WT = 0.0
    WTT = 0.0
    Wn = 0.0
    WnT = 0.0
    Wnn = 0.0

    ! Map alpha*a with differentials
    do i=1,nce
      aa(i) = cbeos%single(i)%alpha*cbeos%single(i)%a
      daadT(i) = cbeos%single(i)%dalphadT*cbeos%single(i)%a
      d2aadT2(i) = cbeos%single(i)%d2alphadT2*cbeos%single(i)%a
      b_ii(i) = cbeos%single(i)%b
    enddo
    ! Copy for readabillity
    B = cbeos%sumb
    Bn = cbeos%bi
    !Bnn = cbeos%bij

    ! Start developing mixing rule
    cbeos%suma = B * (sum(n*aa/b_ii) - (W + gEx)/LAMBDA_GE)
    cbeos%at   = B * (sum(n*daadT/b_ii) - (WT + dgExdT)/LAMBDA_GE)
    cbeos%att  = B * (sum(n*d2aadT2/b_ii) - (WTT + d2gExdT2)/LAMBDA_GE)
    A = cbeos%suma

    cbeos%ai = Bn*A/B + B * (aa/b_ii - (Wn + dGExdNi)/LAMBDA_GE)
    An = cbeos%ai
    cbeos%ait = (Bn/B)*cbeos%at + B * (daadT/b_ii - (WnT + d2GExdNidT)/LAMBDA_GE)

    ! Ovarall second derivatives
    do i=1,nce
      do j=i,nce
        cbeos%aij(i,j) = (cbeos%bij(i,j)*A + Bn(j)*An(i) + Bn(i)*An(j))/B &
             - 2.0*Bn(i)*Bn(j)*A/B**2&
             - B*(Wnn(i,j) + d2GexdNidNj(i,j))/LAMBDA_GE
        if (i /= j) then
          cbeos%aij(j,i) = cbeos%aij(i,j)
        endif
      enddo
    enddo
  end subroutine ExcessGibbsMix

end module excess_gibbs
