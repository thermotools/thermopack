!> Test model consistency
subroutine consistency(t,p,n,phase,gi,phiP,phiT,gd,phi,phisym,phinumP,phinumT,phinumX)
  use eos, only: thermo, residualGibbs, enthalpy, zfac
  use thermopack_constants, only: Rgas
  use thermopack_var, only: nc
  implicit none
  real, intent(in) :: t,p
  integer, intent(in) :: phase
  real, dimension(nc), intent(in) :: n
  real, intent(out) :: gi,phiP,phiT
  real, dimension(nc), intent(out) :: gd,phi,phinumP,phinumT
  real, dimension(nc*nc), intent(out) :: phinumX
  real, dimension((nc*nc-nc)/2), intent(out) :: phisym
  ! Locals
  real, parameter :: dn = 1.0e-5
  real, parameter :: dp = 1.0e-5
  real, parameter :: dt = 1.0e-5
  real, dimension(nc) :: nl,lnfug,lnfugt,lnfugp,lnfugnum
  real, dimension(nc) :: lnfugnumPlus,lnfugnumMinus
  real, dimension(nc,nc) :: lnfugx
  real :: Gr, Hr, Z, GrPlus, GrMinus, pNum, tNum
  integer :: i, j, index
  logical, parameter :: printRes = .false.

 call thermo(t,p,n,phase,lnfug,lnfugt,lnfugp,lnfugx)
  call zfac(t,p,n,phase,Z)
  call enthalpy(t,p,n,phase,Hr,residual=.true.)
  call residualGibbs(t,p,n,phase,Gr)

  ! Gibbs identity
  gi = sum(n*lnfug) - sum(n)*Gr/(Rgas*T)
  if (printRes) then
     print *
     print *,'Gibbs identity:'
     print *,gi
  endif
  ! Pressure identity
  phiP = sum(n*lnfugp) - (Z-1)*sum(n)/P
  if (printRes) then
     print *
     print *,'Pressure identity:'
     print *,phiP
  endif

  ! Temperature identity
  phiT = sum(n*lnfugT) + sum(n)*Hr/(Rgas*T**2)
  if (printRes) then
     print *
     print *,'Temperature identity:'
     print *,phiT
  endif

  ! Gibbs-Duheme
  do i=1,nc
     gd(i) = sum(n*lnfugx(:,i))
  enddo
  if (printRes) then
     print *
     print *,'Gibbs-Duheme:'
     print *,gd
  endif

  ! Numerical test of fugacity coeff using central difference
  do i=1,nc
     nl = n
     nl(i) = n(i) + n(i)*dn
     call thermo(t,p,nl,phase,lnfugnumPlus)
     GrPlus = sum(nl*lnfugnumPlus)
     !lnfugnumPlus = sum(nl)*lnfugnumPlus
     nl(i) = n(i) - n(i)*dn
     call thermo(t,p,nl,phase,lnfugnumMinus)
     GrMinus = sum(nl*lnfugnumMinus)
     !lnfugnumMinus = sum(nl)*lnfugnumMinus
     lnfugnum(i) = (GrPlus-GrMinus)/(2*n(i)*dn)
     phinumX((i-1)*nc+1:nc*i) = sum(n)*(lnfugnumPlus-lnfugnumMinus)/(2*n(i)*dn)-&
          lnfugx(i,:)
     if (printRes) then
        print *
        print *,'Numerical differentials for fugacities, ',i,':'
        print *,phinumX((i-1)*nc+1:nc*i)
     endif
  enddo
  phi = lnfugnum - lnfug
  if (printRes) then
     print *
     print *,'Numerical fugacities:'
     print *,phi
  endif

  ! Test symmetry of compositional differential
  index = 0
  do i=1,nc
     do j=i+1,nc
        index = index + 1
        phisym(index) = lnfugx(i,j) - lnfugx(j,i)
     enddo
  enddo
  if (printRes) then
     print *
     print *,'Symmetry test:'
     print *,phisym
  endif

  ! Numerical test of fugacity coeff pressure diff. using central difference
  pNum = p + p*dp
  call thermo(t,pNum,n,phase,lnfugnumPlus)
  pNum = p - p*dp
  call thermo(t,pNum,n,phase,lnfugnumMinus)
  lnfugnum = (lnfugnumPlus-lnfugnumMinus)/(2*p*dp)
  phinumP = (lnfugP - lnfugnum)*P ! Scale with P
  if (printRes) then
     print *
     print *,'Pressure differentials:'
     print *,phinumP
  endif

  ! Numerical test of fugacity coeff temperature diff. using central difference
  tNum = t + t*dt
  call thermo(tNum,p,n,phase,lnfugnumPlus)
  tNum = t - t*dt
  call thermo(tNum,p,n,phase,lnfugnumMinus)
  lnfugnum = (lnfugnumPlus-lnfugnumMinus)/(2*t*dt)
  phinumT = (lnfugT - lnfugnum)*T ! Scale with T
  if (printRes) then
     print *
     print *,'Temperature differentials:'
     print *,phinumT
  endif
end subroutine consistency

!< Test differentials
subroutine testCubicModel(t,p,n,phase)
  use eos, only: zfac
  use thermopack_var, only: nc, get_active_eos, base_eos_param
  use thermopack_constants, only: kRgas
  use cubic_eos
  use cubic
  use cbmix
  use cbhelm
  use eosdata, only: cbSW, cbPT
  implicit none

  real, intent(in) :: t,p
  integer, intent(in) :: phase
  real, dimension(nc), intent(in) :: n
  ! Locals
  real, parameter :: dn = 1.0e-5
  real, parameter :: dv = 1.0e-5
  real, parameter :: dt = 1.0e-5
  real, parameter :: dc = 1.0e-5
  real, parameter :: db = 1.0e-5
  real, parameter :: da = 1.0e-5
  integer :: i, j

  real :: nn(nc),f0,f1,ft0,ftt0,ft1,tpert,np(nc),v1,FiT0(nc),FiV0(nc), Fij0(nc,nc)
  real :: a0,at0,v0,fv0,fvv0,fv1,fvt0,Fi0(nc),Fi1(nc),att0
  real :: p0,p1,pc0,pb0,pt0,pv0,b0,bt0,btt0,c0, pa0, Z
  real :: ff0, ffac0, ffa0, ffn0, ffv0, ffb0, ffc0, fft0, fftt0, ffaa0
  real :: ffab0,  ffat0, ffbb0, ffbc0, ffbt0, ffcc0, ffct0, ffnv0
  real :: ffnb0, ffna0, ffnc0, ffnn0, ffnt0, ffvt0, ffvv0,ffva0, ffvb0, ffvc0
  real :: ci0(nc), cij0(nc,nc), bi0(nc), bij0(nc,nc), ai0(nc), aij0(nc,nc)
  real :: m10, m20, dm1dc0, dm2dc0, d2m1dc20, d2m2dc20, dm1db0, dm2db0, d2m1db20
  real :: d2m2db20, d2m1dbdc0, d2m2dbdc0
  class(base_eos_param), pointer :: act_eos_ptr
  print *,'nc=', nc

  act_eos_ptr => get_active_eos()

  select type(p_eos => act_eos_ptr)
    class is(cb_eos)
      nn = n/sum(n)
      call zfac(t,p,nn,phase,Z)
      call cbCalcMixtureParams (nc,p_eos,T,nn)
      call cbCalcDerivatives(nc,p_eos,T,p,Z)
      v0 = z*T*kRgas/p
      f0 = cbF(p_eos)
      ft0 = cbFt(p_eos)
      ftt0 = cbFtt(p_eos)
      fv0 = cbFv(p_eos)
      fvv0 = cbFvv(p_eos)
      fvt0 = cbFvt(p_eos)
      call cbFi(nc,p_eos,Fi0)
      a0 = p_eos%suma
      ai0 = p_eos%ai
      c0 = p_eos%sumc
      b0 = p_eos%sumb
      bi0 = p_eos%bi
      at0 = p_eos%at
      bt0 = p_eos%bt
      att0 = p_eos%att
      btt0 = p_eos%btt
      call cbFiT(nc,p_eos,FiT0)
      call cbFiV(nc,p_eos,FiV0)
      call cbFij(nc,p_eos,Fij0)
      p0 = cbPress(p_eos,T,v0)
      pc0 = p_eos%pc
      pb0 = p_eos%pb
      pt0 = p_eos%pt
      pv0 = p_eos%pv
      pa0 = p_eos%pa
      !
      ff0 = p_eos%ff
      ffac0 = p_eos%ffac
      ffa0 = p_eos%ffa
      ffn0 = p_eos%ffn
      ffv0 = p_eos%ffv
      ffb0 = p_eos%ffb
      ffc0 = p_eos%ffc
      fft0 = p_eos%fft
      fftt0 = p_eos%fftt
      ffaa0 = p_eos%ffaa
      ffab0 = p_eos%ffab
      ffat0 = p_eos%ffat
      ffbb0 = p_eos%ffbb
      ffbc0 = p_eos%ffbc
      ffbt0 = p_eos%ffbt
      ffcc0 = p_eos%ffcc
      ffct0 = p_eos%ffct
      ffnv0 = p_eos%ffnv
      ffnb0 = p_eos%ffnb
      ffna0 = p_eos%ffna
      ffnc0 = p_eos%ffnc
      ffnn0 = p_eos%ffnn
      ffnt0 = p_eos%ffnt
      ffvt0 = p_eos%ffvt
      ffvv0 = p_eos%ffvv
      ffva0 = p_eos%ffva
      ffvb0 = p_eos%ffvb
      ffvc0 = p_eos%ffvc

      print *
      print *,'Temperature diferentials'
      print *
      Tpert = T+T*dT
      p1 = cbPress(p_eos,Tpert,v0)
      call cbCalcMixtureParams (nc,p_eos,Tpert,nn)
      call cbCalcDerivatives_svol(nc,p_eos,Tpert,v0)
      f1 = cbF(p_eos)
      fv1 = cbFv(p_eos)
      ft1 = cbFt(p_eos)
      print *,'At', (p_eos%suma-a0)/(T*dT),at0
      print *,'Att', (p_eos%at-at0)/(T*dT),att0
      do i=1,nc
        print *,'Ait',i, (p_eos%ai(i)-ai0(i))/(T*dT), p_eos%ait(i)
      enddo
      print *,'Bt', (p_eos%sumb-b0)/(T*dT), bt0
      print *,'Btt', (p_eos%bt-bt0)/(T*dT), btt0
      do i=1,nc
        print *, 'Bit',i, (p_eos%bi(i)-bi0(i))/(T*dT), p_eos%bit(i)
      enddo
      print *,'Ft', (f1-f0)/(T*dT),ft0
      print *,'Ftt', (ft1-ft0)/(T*dT),ftt0
      print *,'Fvt', (fv1-fv0)/(T*dT),fvt0
      print *,'pt',(p1-p0)/(T*dT), pt0
      call cbFi(nc,p_eos,Fi1)
      do i=1,nc
        print *, 'Fit',(Fi1(i)-Fi0(i))/(T*dT),FiT0(i)
      enddo

      print *
      print *,'Volume diferentials'
      print *
      v1 = v0 + v0*dv
      call cbCalcMixtureParams (nc,p_eos,T,nn)
      call cbCalcDerivatives_svol(nc,p_eos,T,v1)
      f1 = cbF(p_eos)
      fv1 = cbFv(p_eos)
      ft1 = cbFt(p_eos)
      print *, 'Fv', (f1-f0)/(v0*dv),fv0
      print *, 'Fvv', (fv1-fv0)/(v0*dv),fvv0
      print *, 'Fvt', (ft1-ft0)/(v0*dv),fvt0
      p1 = cbPress(p_eos,T,v1)
      print *,'pv',(p1-p0)/(v0*dv), pv0
      call cbFi(nc,p_eos,Fi1)
      do i=1,nc
        print *, 'Fiv',(Fi1(i)-Fi0(i))/(v0*dv),FiV0(i)
      enddo
      !
      print *
      print *,'Mole number differentials'
      print *
      do i=1,nc
        np = nn
        np(i) = nn(i) + nn(i)*dn
        call cbCalcMixtureParams (nc,p_eos,T,np)
        call cbCalcDerivatives_svol(nc,p_eos,T,v0)
        f1 = cbF(p_eos)
        ft1 = cbFt(p_eos)
        fv1 = cbFv(p_eos)
        print *, 'Fi',(f1-f0)/(nn(i)*1.0e-5),Fi0(i)
        print *, 'Fit',(ft1-ft0)/(nn(i)*1.0e-5),FiT0(i)
        print *, 'Fiv',(fv1-fv0)/(nn(i)*1.0e-5),Fiv0(i)
      enddo
      print *
      do j=1,nc
        np = nn
        np(j) = nn(j) + nn(j)*dn
        call cbCalcMixtureParams(nc,p_eos,T,np)
        call cbCalcDerivatives_svol(nc,p_eos,T,v0)
        call cbFi(nc,p_eos,Fi1)
        do i=1,nc
          print *, 'Fij',(fi1(i)-fi0(i))/(nn(j)*dn),Fij0(i,j)
        enddo
      enddo

      if (p_eos%eosidx == cbSW .or. p_eos%eosidx == cbPT) then
        print *
        print *,'Perturbate in c'
        print *
        call cbCalcMixtureParams(nc,p_eos,T,nn)
        p_eos%sumc = c0 + c0*dc
        p_eos%c = p_eos%sumc
        call cbCalcM(p_eos,p_eos%c,b0)
        call cbCalcDerivatives_svol(nc,p_eos,T,v0)
        print *, 'ffc',(p_eos%ff-ff0)/(c0*dc),ffc0
        print *, 'ffcc',(p_eos%ffc-ffc0)/(c0*dc),ffcc0
        print *, 'ffbc',(p_eos%ffb-ffb0)/(c0*dc),ffbc0
        print *, 'ffvc',(p_eos%ffv-ffv0)/(c0*dc),ffvc0
        print *, 'ffac',(p_eos%ffa-ffa0)/(c0*dc),ffac0
        p1 = cbPress(p_eos,T,v0)
        print *,'pc',(p1-p0)/(c0*dc), pc0
      end if

      print *
      print *,'Perturbate in b'
      print *
      call cbCalcMixtureParams(nc,p_eos,T,nn)
      !Must call this to get correct B winth Wong Sandler not set in cbCalcBmix.
      p_eos%sumb = b0 + b0*db
      p_eos%b = p_eos%sumb
      call cbCalcM(p_eos,c0,p_eos%b)
      call cbCalcDerivatives_svol(nc,p_eos,T,v0)
      print *, 'ffb',(p_eos%ff-ff0)/(b0*db),ffb0
      print *, 'ffab',(p_eos%ffa-ffa0)/(b0*db),ffab0
      print *, 'ffbb',(p_eos%ffb-ffb0)/(b0*db),ffbb0
      print *, 'ffvb',(p_eos%ffv-ffv0)/(b0*db),ffvb0
      p1 = cbPress(p_eos,T,v0)
      print *,'pb',(p1-p0)/(b0*db), pb0

      print *
      print *,'Perturbate in a'
      print *
      call cbCalcMixtureParams(nc,p_eos,T,nn)
      p_eos%suma = a0 + a0*da
      p_eos%a = p_eos%suma
      call cbCalcDerivatives_svol(nc,p_eos,T,v0)
      print *, 'ffa',(p_eos%ff-ff0)/(a0*da),ffa0
      print *, 'ffaa',(p_eos%ffa-ffa0)/(a0*da),ffaa0
      print *, 'ffab',(p_eos%ffb -ffb0)/(a0*da),ffab0
      print *, 'ffva',(p_eos%ffv-ffv0)/(a0*da),ffva0
      p1 = cbPress(p_eos,T,v0)
      print *,'pa',(p1-p0)/(a0*da), pa0

      print *
      print *,'Perturbate in t'
      print *
      call cbCalcMixtureParams(nc,p_eos,T,nn)
      call cbCalcDerivatives_svol(nc,p_eos,Tpert,v0)
      print *, 'fft',(p_eos%ff-ff0)/(T*dT),fft0
      print *, 'ffat',(p_eos%ffa-ffa0)/(T*dT),ffat0
      print *, 'ffbt',(p_eos%ffb -ffb0)/(T*dT),ffbt0


      if (p_eos%eosidx == cbSW .or. p_eos%eosidx == cbPT) then
        print *
        print *,'Testing C-mix'
        print *
        call cbCalcCmix(nc,p_eos,n)
        c0 = p_eos%c
        ci0 = p_eos%ci
        cij0 = p_eos%cij
        do j=1,nc
          nn = n
          nn(j) = nn(j) + nn(j)*dn
          call cbCalcCmix(nc,p_eos,nn)
          print *,'ci',j,(p_eos%c-c0)/(nn(j)*dn),ci0(j)
          do i=1,nc
            print *,'cij',j,i,(p_eos%ci(i)-ci0(i))/(n(j)*dn),cij0(i,j)
          enddo
        enddo
      endif

      print *
      print *,'Testing B-mix'
      print *
      call cbCalcMixtureParams (nc,p_eos,T,n)
      call cbCalcDerivatives_svol(nc,p_eos,T,v0)
      b0 = p_eos%b
      bi0 = p_eos%bi
      bij0 = p_eos%bij
      do j=1,nc
        nn = n
        nn(j) = nn(j) + nn(j)*dn
        call cbCalcMixtureParams (nc,p_eos,T,nn)
        call cbCalcDerivatives_svol(nc,p_eos,T,v0)
        print *,'bi',j,(p_eos%b-b0)/(n(j)*dn),bi0(j)
        do i=1,nc
          print *,'bij',j,i,(p_eos%bi(i)-bi0(i))/(n(j)*dn),bij0(i,j)
        enddo
      enddo

      print *
      print *,'Testing A-mix'
      print *
      call cbCalcMixtureParams (nc,p_eos,T,n)
      call cbCalcDerivatives_svol(nc,p_eos,T,v0)
      a0 = p_eos%a
      ai0 = p_eos%ai
      aij0 = p_eos%aij
      do j=1,nc
        nn = n
        nn(j) = nn(j) + n(j)*dn
        call cbCalcMixtureParams (nc,p_eos,T,nn)
        call cbCalcDerivatives_svol(nc,p_eos,T,v0)
        print *,'ai',j,(p_eos%a-a0)/(n(j)*dn),ai0(j)
        do i=1,nc
          print *,'aij',j,i,(p_eos%ai(i)-ai0(i))/(n(j)*dn),aij0(i,j)
        enddo
      enddo

      if (p_eos%eosidx == cbSW .or. p_eos%eosidx == cbPT) then
        print *
        print *,'Testing m1 and m2'
        print *
        call cbCalcM(p_eos, c0, b0)
        m10 = p_eos%m1
        m20 = p_eos%m2
        dm1dc0 = p_eos%dm1dc
        dm2dc0 = p_eos%dm2dc
        d2m1dc20 = p_eos%d2m1dc2
        d2m2dc20 = p_eos%d2m2dc2
        dm1db0 = p_eos%dm1db
        dm2db0 = p_eos%dm2db
        d2m1db20 = p_eos%d2m1db2
        d2m2db20 = p_eos%d2m2db2
        d2m1dbdc0 = p_eos%d2m1dbdc
        d2m2dbdc0 = p_eos%d2m2dbdc
        call cbCalcM(p_eos, c0 + c0*dc, b0)
        print *,'dm1dc',(p_eos%m1-m10)/(c0*dc),dm1dc0
        print *,'dm2dc',(p_eos%m2-m20)/(c0*dc),dm2dc0
        print *,'d2m1dc2',(p_eos%dm1dc-dm1dc0)/(c0*dc),d2m1dc20
        print *,'d2m2dc2',(p_eos%dm2dc-dm2dc0)/(c0*dc),d2m2dc20
        print *,'d2m1dbdc',(p_eos%dm1db-dm1db0)/(c0*dc),d2m1dbdc0
        print *,'d2m2dbdc',(p_eos%dm2db-dm2db0)/(c0*dc),d2m2dbdc0

        call cbCalcM(p_eos, c0, b0 + b0*db)
        print *,'dm1db',(p_eos%m1-m10)/(b0*db),dm1db0
        print *,'dm2db',(p_eos%m2-m20)/(b0*db),dm2db0
        print *,'d2m1db2',(p_eos%dm1db-dm1db0)/(b0*db),d2m1db20
        print *,'d2m2db2',(p_eos%dm2db-dm2db0)/(b0*db),d2m2db20
        print *,'d2m1dcdb',(p_eos%dm1dc-dm1dc0)/(b0*db),d2m1dbdc0
        print *,'d2m2dcdb',(p_eos%dm2dc-dm2dc0)/(b0*db),d2m2dbdc0
      endif
    class default
      print *,"Model not cubic..."
    end select
end subroutine testCubicModel
