!> Test model consistency
subroutine consistency(t,p,n,phase,gi,phiP,phiT,gd,phi,phisym,phinumP,phinumT,phinumX)
  use eos, only: thermo, residualGibbs, enthalpy, zfac
  use parameters, only: nc
  use tpconst, only: getRgas
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
  real :: Gr, Hr, Z, GrPlus, GrMinus, pNum, tNum, Rgas
  integer :: i, j, index
  logical, parameter :: printRes = .false.

  call thermo(t,p,n,phase,lnfug,lnfugt,lnfugp,lnfugx)
  call zfac(t,p,n,phase,Z)
  call enthalpy(t,p,n,phase,Hr,residual=.true.)
  call residualGibbs(t,p,n,phase,Gr)

  ! Gibbs identity
  Rgas = getRgas(n/sum(n))
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
  use parameters, only: nc
  use tpconst, only: kRgas
  use tpcubic
  use tpcbmix
  use tpvar, only: comp,cbeos
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

  print *,'nc=', nc


  nn = n/sum(n)
  call zfac(t,p,nn,phase,Z)
  call cbCalcMixtureParams (nc,comp,cbeos(1),T,nn)
  call cbCalcDerivatives(nc,cbeos(1),T,p,Z)
  v0 = z*T*kRgas/p
  f0 = cbF(cbeos(1))
  ft0 = cbFt(cbeos(1))
  ftt0 = cbFtt(cbeos(1))
  fv0 = cbFv(cbeos(1))
  fvv0 = cbFvv(cbeos(1))
  fvt0 = cbFvt(cbeos(1))
  call cbFi(nc,cbeos(1),Fi0)
  a0 = cbeos(1)%suma
  ai0 = cbeos(1)%ai
  c0 = cbeos(1)%sumc
  b0 = cbeos(1)%sumb
  bi0 = cbeos(1)%bi
  at0 = cbeos(1)%at
  bt0 = cbeos(1)%bt
  att0 = cbeos(1)%att
  btt0 = cbeos(1)%btt
  call cbFiT(nc,cbeos(1),FiT0)
  call cbFiV(nc,cbeos(1),FiV0)
  call cbFij(nc,cbeos(1),Fij0)
  p0 = cbPress(cbeos(1),T,v0)
  pc0 = cbeos(1)%pc
  pb0 = cbeos(1)%pb
  pt0 = cbeos(1)%pt
  pv0 = cbeos(1)%pv
  pa0 = cbeos(1)%pa
  !
  ff0 = cbeos(1)%ff
  ffac0 = cbeos(1)%ffac
  ffa0 = cbeos(1)%ffa
  ffn0 = cbeos(1)%ffn
  ffv0 = cbeos(1)%ffv
  ffb0 = cbeos(1)%ffb
  ffc0 = cbeos(1)%ffc
  fft0 = cbeos(1)%fft
  fftt0 = cbeos(1)%fftt
  ffaa0 = cbeos(1)%ffaa
  ffab0 = cbeos(1)%ffab
  ffat0 = cbeos(1)%ffat
  ffbb0 = cbeos(1)%ffbb
  ffbc0 = cbeos(1)%ffbc
  ffbt0 = cbeos(1)%ffbt
  ffcc0 = cbeos(1)%ffcc
  ffct0 = cbeos(1)%ffct
  ffnv0 = cbeos(1)%ffnv
  ffnb0 = cbeos(1)%ffnb
  ffna0 = cbeos(1)%ffna
  ffnc0 = cbeos(1)%ffnc
  ffnn0 = cbeos(1)%ffnn
  ffnt0 = cbeos(1)%ffnt
  ffvt0 = cbeos(1)%ffvt
  ffvv0 = cbeos(1)%ffvv
  ffva0 = cbeos(1)%ffva
  ffvb0 = cbeos(1)%ffvb
  ffvc0 = cbeos(1)%ffvc

  print *
  print *,'Temperature diferentials'
  print *
  Tpert = T+T*dT
  p1 = cbPress(cbeos(1),Tpert,v0)
  call cbCalcMixtureParams (nc,comp,cbeos(1),Tpert,nn)
  call cbCalcDerivatives_svol(nc,cbeos(1),Tpert,v0)
  f1 = cbF(cbeos(1))
  fv1 = cbFv(cbeos(1))
  ft1 = cbFt(cbeos(1))
  print *,'At', (cbeos(1)%suma-a0)/(T*dT),at0
  print *,'Att', (cbeos(1)%at-at0)/(T*dT),att0
  do i=1,nc
     print *,'Ait',i, (cbeos(1)%ai(i)-ai0(i))/(T*dT), cbeos(1)%ait(i)
  enddo
  print *,'Bt', (cbeos(1)%sumb-b0)/(T*dT), bt0
  print *,'Btt', (cbeos(1)%bt-bt0)/(T*dT), btt0
  do i=1,nc
     print *, 'Bit',i, (cbeos(1)%bi(i)-bi0(i))/(T*dT), cbeos(1)%bit(i)
  enddo
  print *,'Ft', (f1-f0)/(T*dT),ft0
  print *,'Ftt', (ft1-ft0)/(T*dT),ftt0
  print *,'Fvt', (fv1-fv0)/(T*dT),fvt0
  print *,'pt',(p1-p0)/(T*dT), pt0
  call cbFi(nc,cbeos(1),Fi1)
  do i=1,nc
     print *, 'Fit',(Fi1(i)-Fi0(i))/(T*dT),FiT0(i)
  enddo

  print *
  print *,'Volume diferentials'
  print *
  v1 = v0 + v0*dv
  call cbCalcMixtureParams (nc,comp,cbeos(1),T,nn)
  call cbCalcDerivatives_svol(nc,cbeos(1),T,v1)
  f1 = cbF(cbeos(1))
  fv1 = cbFv(cbeos(1))
  ft1 = cbFt(cbeos(1))
  print *, 'Fv', (f1-f0)/(v0*dv),fv0
  print *, 'Fvv', (fv1-fv0)/(v0*dv),fvv0
  print *, 'Fvt', (ft1-ft0)/(v0*dv),fvt0
  p1 = cbPress(cbeos(1),T,v1)
  print *,'pv',(p1-p0)/(v0*dv), pv0
  call cbFi(nc,cbeos(1),Fi1)
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
     call cbCalcMixtureParams (nc,comp,cbeos(1),T,np)
     call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
     f1 = cbF(cbeos(1))
     ft1 = cbFt(cbeos(1))
     fv1 = cbFv(cbeos(1))
     print *, 'Fi',(f1-f0)/(nn(i)*1.0e-5),Fi0(i)
     print *, 'Fit',(ft1-ft0)/(nn(i)*1.0e-5),FiT0(i)
     print *, 'Fiv',(fv1-fv0)/(nn(i)*1.0e-5),Fiv0(i)
  enddo
  print *
  do j=1,nc
     np = nn
     np(j) = nn(j) + nn(j)*dn
     call cbCalcMixtureParams(nc,comp,cbeos(1),T,np)
     call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
     call cbFi(nc,cbeos(1),Fi1)
     do i=1,nc
        print *, 'Fij',(fi1(i)-fi0(i))/(nn(j)*dn),Fij0(i,j)
     enddo
  enddo

  if (cbeos(1)%eosidx == cbSW .or. cbeos(1)%eosidx == cbPT) then
     print *
     print *,'Perturbate in c'
     print *
     call cbCalcMixtureParams(nc,comp,cbeos(1),T,nn)
     cbeos(1)%sumc = c0 + c0*dc
     cbeos(1)%c = cbeos(1)%sumc
     call cbCalcM(cbeos(1),cbeos(1)%c,b0)
     call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
     print *, 'ffc',(cbeos(1)%ff-ff0)/(c0*dc),ffc0
     print *, 'ffcc',(cbeos(1)%ffc-ffc0)/(c0*dc),ffcc0
     print *, 'ffbc',(cbeos(1)%ffb-ffb0)/(c0*dc),ffbc0
     print *, 'ffvc',(cbeos(1)%ffv-ffv0)/(c0*dc),ffvc0
     print *, 'ffac',(cbeos(1)%ffa-ffa0)/(c0*dc),ffac0
     p1 = cbPress(cbeos(1),T,v0)
     print *,'pc',(p1-p0)/(c0*dc), pc0
  end if

  print *
  print *,'Perturbate in b'
  print *
  call cbCalcMixtureParams(nc,comp,cbeos(1),T,nn)
  !Must call this to get correct B winth Wong Sandler not set in cbCalcBmix.
  cbeos(1)%sumb = b0 + b0*db
  cbeos(1)%b = cbeos(1)%sumb
  call cbCalcM(cbeos(1),c0,cbeos(1)%b)
  call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
  print *, 'ffb',(cbeos(1)%ff-ff0)/(b0*db),ffb0
  print *, 'ffab',(cbeos(1)%ffa-ffa0)/(b0*db),ffab0
  print *, 'ffbb',(cbeos(1)%ffb-ffb0)/(b0*db),ffbb0
  print *, 'ffvb',(cbeos(1)%ffv-ffv0)/(b0*db),ffvb0
  p1 = cbPress(cbeos(1),T,v0)
  print *,'pb',(p1-p0)/(b0*db), pb0

  print *
  print *,'Perturbate in a'
  print *
  call cbCalcMixtureParams(nc,comp,cbeos(1),T,nn)
  cbeos(1)%suma = a0 + a0*da
  cbeos(1)%a = cbeos(1)%suma
  call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
  print *, 'ffa',(cbeos(1)%ff-ff0)/(a0*da),ffa0
  print *, 'ffaa',(cbeos(1)%ffa-ffa0)/(a0*da),ffaa0
  print *, 'ffab',(cbeos(1)%ffb -ffb0)/(a0*da),ffab0
  print *, 'ffva',(cbeos(1)%ffv-ffv0)/(a0*da),ffva0
  p1 = cbPress(cbeos(1),T,v0)
  print *,'pa',(p1-p0)/(a0*da), pa0

  print *
  print *,'Perturbate in t'
  print *
  call cbCalcMixtureParams(nc,comp,cbeos(1),T,nn)
  call cbCalcDerivatives_svol(nc,cbeos(1),Tpert,v0)
  print *, 'fft',(cbeos(1)%ff-ff0)/(T*dT),fft0
  print *, 'ffat',(cbeos(1)%ffa-ffa0)/(T*dT),ffat0
  print *, 'ffbt',(cbeos(1)%ffb -ffb0)/(T*dT),ffbt0


  if (cbeos(1)%eosidx == cbSW .or. cbeos(1)%eosidx == cbPT) then
     print *
     print *,'Testing C-mix'
     print *
     call cbCalcCmix(nc,comp,cbeos(1),n)
     c0 = cbeos(1)%c
     ci0 = cbeos(1)%ci
     cij0 = cbeos(1)%cij
     do j=1,nc
        nn = n
        nn(j) = nn(j) + nn(j)*dn
        call cbCalcCmix(nc,comp,cbeos(1),nn)
        print *,'ci',j,(cbeos(1)%c-c0)/(nn(j)*dn),ci0(j)
        do i=1,nc
           print *,'cij',j,i,(cbeos(1)%ci(i)-ci0(i))/(n(j)*dn),cij0(i,j)
        enddo
     enddo
  endif

  print *
  print *,'Testing B-mix'
  print *
  call cbCalcMixtureParams (nc,comp,cbeos(1),T,n)
  call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
  b0 = cbeos(1)%b
  bi0 = cbeos(1)%bi
  bij0 = cbeos(1)%bij
  do j=1,nc
     nn = n
     nn(j) = nn(j) + nn(j)*dn
     call cbCalcMixtureParams (nc,comp,cbeos(1),T,nn)
     call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
     print *,'bi',j,(cbeos(1)%b-b0)/(n(j)*dn),bi0(j)
     do i=1,nc
        print *,'bij',j,i,(cbeos(1)%bi(i)-bi0(i))/(n(j)*dn),bij0(i,j)
     enddo
  enddo

  print *
  print *,'Testing A-mix'
  print *
  call cbCalcMixtureParams (nc,comp,cbeos(1),T,n)
  call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
  a0 = cbeos(1)%a
  ai0 = cbeos(1)%ai
  aij0 = cbeos(1)%aij
  do j=1,nc
     nn = n
     nn(j) = nn(j) + n(j)*dn
     call cbCalcMixtureParams (nc,comp,cbeos(1),T,nn)
     call cbCalcDerivatives_svol(nc,cbeos(1),T,v0)
     print *,'ai',j,(cbeos(1)%a-a0)/(n(j)*dn),ai0(j)
     do i=1,nc
        print *,'aij',j,i,(cbeos(1)%ai(i)-ai0(i))/(n(j)*dn),aij0(i,j)
     enddo
  enddo

  if (cbeos(1)%eosidx == cbSW .or. cbeos(1)%eosidx == cbPT) then
     print *
     print *,'Testing m1 and m2'
     print *
     call cbCalcM(cbeos(1), c0, b0)
     m10 = cbeos(1)%m1
     m20 = cbeos(1)%m2
     dm1dc0 = cbeos(1)%dm1dc
     dm2dc0 = cbeos(1)%dm2dc
     d2m1dc20 = cbeos(1)%d2m1dc2
     d2m2dc20 = cbeos(1)%d2m2dc2
     dm1db0 = cbeos(1)%dm1db
     dm2db0 = cbeos(1)%dm2db
     d2m1db20 = cbeos(1)%d2m1db2
     d2m2db20 = cbeos(1)%d2m2db2
     d2m1dbdc0 = cbeos(1)%d2m1dbdc
     d2m2dbdc0 = cbeos(1)%d2m2dbdc
     call cbCalcM(cbeos(1), c0 + c0*dc, b0)
     print *,'dm1dc',(cbeos(1)%m1-m10)/(c0*dc),dm1dc0
     print *,'dm2dc',(cbeos(1)%m2-m20)/(c0*dc),dm2dc0
     print *,'d2m1dc2',(cbeos(1)%dm1dc-dm1dc0)/(c0*dc),d2m1dc20
     print *,'d2m2dc2',(cbeos(1)%dm2dc-dm2dc0)/(c0*dc),d2m2dc20
     print *,'d2m1dbdc',(cbeos(1)%dm1db-dm1db0)/(c0*dc),d2m1dbdc0
     print *,'d2m2dbdc',(cbeos(1)%dm2db-dm2db0)/(c0*dc),d2m2dbdc0

     call cbCalcM(cbeos(1), c0, b0 + b0*db)
     print *,'dm1db',(cbeos(1)%m1-m10)/(b0*db),dm1db0
     print *,'dm2db',(cbeos(1)%m2-m20)/(b0*db),dm2db0
     print *,'d2m1db2',(cbeos(1)%dm1db-dm1db0)/(b0*db),d2m1db20
     print *,'d2m2db2',(cbeos(1)%dm2db-dm2db0)/(b0*db),d2m2db20
     print *,'d2m1dcdb',(cbeos(1)%dm1dc-dm1dc0)/(b0*db),d2m1dbdc0
     print *,'d2m2dcdb',(cbeos(1)%dm2dc-dm2dc0)/(b0*db),d2m2dbdc0
  endif

end subroutine testCubicModel
