!-------------------------------------------------------------------------
!> Calculate critical point of a mixture.
!! Good initial values assumed.
!! Based on: M.L. Michelsen,
!! Calculation of critical points and phase boundaries in the critical region.
!! Fluid phase equilibria, 16, 1984, pp. 57-76.
!!
!-------------------------------------------------------------------------
module critical
  use thermopack_constants, only: verbose, VAPPH, LIQPH, Rgas
  use thermopack_var, only: nc, thermo_model, get_active_thermo_model
  use eos, only : thermo, pseudo_safe
  use eosTV, only : thermo_tv, pressure
  implicit none
  private
  save
  real, parameter :: eps = 1.0e-5

  public :: calcCritical, calcStabMinEig
  public :: calcCriticalTV
  public :: calcCriticalZ, critZsensitivity
  public :: calcCriticalEndPoint
  public :: calcStabMinEigTV
  public :: stabFun, stabFunTV, stabJacTV, calcBmatrixTV
contains

  !-------------------------------------------------------------------------
  !> Calculate critical point for mixture given good inital guess.
  !>
  !> \todo Add v=sqrt(z) to paramater vector
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine calcCritical(t,p,Z,phase,ierr,tol)
    use thermopack_constants, only: tpPmin, tpPmax, get_templimits
    use nonlinear_solvers
    implicit none
    real, dimension(nc), intent(in) :: Z !< Trial composition (Overall compozition)
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: p !< Pressure [Pa]
    integer, intent(in) :: phase !< Phase identifer
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: tol !< Toleranse
    ! Locals
    real :: t0, p0
    real, dimension(nc+1) :: param
    real, dimension(2) :: X, xmax, xmin
    !real :: Fun(2), dF(2,2), Fun2(2), numJac(2,2)
    type(nonlinear_solver) :: solver

    ierr = 0
    p0 = p
    t0 = t
    param(1:nc) = z
    param(nc+1) = real(phase)

    X(1) = t
    X(2) = p*1.0e-5

    ! call critFun(Fun,X,param)
    ! print *,'Fun',Fun
    ! call critJac(dF,X,param)
    ! X(1) = t + 1.0e-4
    ! call critFun(Fun2,X,param)
    ! print *,'Fun2 t',Fun2
    ! numJac(:,1) = (Fun2-Fun)/1.0e-4
    ! X(1) = t
    ! X(2) = X(2) + 1.0e-4
    ! call critFun(Fun2,X,param)
    ! print *,'Fun2 p',Fun2
    ! numJac(:,2) = (Fun2-Fun)/1.0e-4
    ! print *,'num jac 1',numJac(1,:)
    ! print *,dF(1,:)
    ! print *,'num jac 2',numJac(2,:)
    ! print *,dF(2,:)
    ! call exit(1)

    solver%abs_tol = 1.0e-8
    if (present(tol)) then
      solver%abs_tol = tol
    endif
    solver%rel_tol = 1.0e-20
    call get_templimits(xmin(1), xmax(1))
    xmin(2) = tpPmin*1.0e-5
    xmax(2) = tpPmax*1.0e-5
    call nonlinear_solve(solver,critFun,critJac,critJac,limit_dx,&
         premterm_at_dx_zero, setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    ! Solution
    t = X(1)
    p = X(2)*1.0e5

    if (ierr /= 0) then
      if (verbose) then
        if (solver%error_on_exit < 1.0e-5) then
          print *,'Critical point solver: Error at solution: ', &
               solver%error_on_exit
        else
          print *,'Not able to locate critical point'
          print *,'Initial pressure (Pa): ', p0
          print *,'Initial temperature (K): ', t0
          t = 0.0
          p = 0.0
        endif
      endif
    endif
  end subroutine calcCritical

  !-------------------------------------------------------------------------
  !> Function value for calculation of critical point for mixtures.
  !>
  !>
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine critFun(Fun,X,param)
    implicit none
    real, dimension(2), intent(out) :: Fun !< Function value
    real, dimension(2), intent(in) :: X !< Variables
    real, dimension(nc+1), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: p !< Pressure [Pa]
    integer :: phase !< Phase identifer

    real, dimension(nc) :: v,dd,lnfugz,lnfugTz,lnfugPz
    real, dimension(nc) :: u,y,y1
    real, dimension(nc,nc) :: Bmat
    real, dimension(nc) :: lnfugTs,lnfugPs,g,lnfugTs1,lnfugPs1,g1
    real :: b,c
    real :: s, dfds, f, f1, dfds1
    real :: lambdaMin
    z = param(1:nc)
    phase = int(param(nc+1))
    t = X(1)
    p = X(2)*1.0e5
    v = sqrt(z)
    call calcBmatrix(t,p,z,v,phase,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugPz)
    b = 0.5*lambdaMin
    s = 1.0e-4
    dd = log(z) + lnfugz
    call calcFofs(t,p,z,v,u,dd,f,dfds,s,phase,lnfugTs,lnfugPs,y,g)
    s = -1.0e-4
    call calcFofs(t,p,z,v,u,dd,f1,dfds1,s,phase,lnfugTs1,lnfugPs1,y1,g1)
    c = (dfds+dfds1)/(6.0*s**2)
    Fun(1) = b
    Fun(2) = c
    !print *,b,c
  end subroutine critFun

  !-------------------------------------------------------------------------
  !> Differentials of b and c.
  !>
  !>
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine critJac(dF,X,param)
    implicit none
    real, dimension(2,2), intent(out) :: dF !< Function differential
    real, dimension(2), intent(in) :: X !< Variables
    real, dimension(nc+1), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: p !< Pressure [Pa]
    integer :: phase !< Phase identifer
    real, dimension(nc) :: v,dd,lnfugz,lnfugTz,lnfugPz,wp,wt,Bpu,Btu
    real, dimension(nc) :: u,up,ut,y,y1
    real, dimension(nc,nc) :: Bmat
    real, dimension(nc) :: lnfugTs,lnfugPs,g
    real, dimension(nc) :: lnfugTs1,lnfugPs1,g1
    real :: cp,ct,bp,bt
    real :: s, dfds, f, f1, dfds1, dfdp1, dfdp, dfdt1, dfdt
    real :: lambdaMin, lambdaP, lambdaT
    !
    !real :: fun0(2), fun1(2), X1(2)
    z = param(1:nc)
    phase = int(param(nc+1))
    ! call critFun(fun0,X,param)
    ! X1 = X
    ! X1(1) = X1(1) + 1.0e-4
    ! call critFun(fun1,X1,param)
    ! dF(:,1) = (fun1-fun0)/1.0e-4
    ! X1 = X
    ! X1(2) = X1(2) + 1.0e-4
    ! call critFun(fun1,X1,param)
    ! dF(:,2) = (fun1-fun0)/1.0e-4
    ! return
    t = X(1)
    p = X(2)*1.0e5

    v = sqrt(z)
    call calcBmatrix(t,p,z,v,phase,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugPz)
    dd = log(z) + lnfugz
    s = -1.0e-4
    call calcFofs(t,p,z,v,u,dd,f1,dfds1,s,phase,lnfugTs1,lnfugPs1,y1,g1)
    s = 1.0e-4
    call calcFofs(t,p,z,v,u,dd,f,dfds,s,phase,lnfugTs,lnfugPs,y,g)
    Bpu = 0.5*v*(lnfugPs-lnfugPs1)/s
    lambdaP = sum(u*Bpu)
    Btu = 0.5*v*(lnfugTs-lnfugTs1)/s
    lambdaT = sum(u*Btu)
    wp = lambdaP*u-Bpu
    wt = lambdaT*u-Btu
    call calcUdiff(Bmat,lambdaMin,u,wp,wt,up,ut)
    bp = 0.5*lambdaP
    bt = 0.5*lambdaT

    dfdp = sum(y*(lnfugPs-lnfugPz)) + sum(g*s*v*up)
    dfdT = sum(y*(lnfugTs-lnfugTz)) + sum(g*s*v*ut)
    dfdp1 = sum(y1*(lnfugPs1-lnfugPz)) + sum(g1*s*v*up)
    dfdT1 = sum(y1*(lnfugTs1-lnfugTz)) + sum(g1*s*v*ut)
    cp = 0.5*(dfdp - dfdp1)/s**3
    ct = 0.5*(dfdt - dfdt1)/s**3
    ! Use bar as unit for pressure
    bp = bp * 1.0e5
    cp = cp * 1.0e5
    df(1,1) = bt
    df(1,2) = bp
    df(2,1) = ct
    df(2,2) = cp
  end subroutine critJac

  !-------------------------------------------------------------------------
  !> Calculate stability matrix (B) and minimum eigenvalue with eigenvector
  !>
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine calcBmatrix(t,p,z,v,phase,B,u,lambdaMin,lnfugz,lnfugTz,lnfugPz)
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall composition
    real, dimension(nc), intent(in) :: v !<
    real, intent(in) :: T !< Temperature
    real, intent(in) :: P !< Pressure
    integer, intent(in) :: phase !< Phase identifer
    real, dimension(nc,nc), intent(out) :: B !< Stability matrix
    real, intent(out) :: lambdaMin !< Eigenvalue
    real, dimension(nc), intent(out) :: u !< Eigenvector
    real, dimension(nc), intent(out) :: lnfugz !<
    real, dimension(nc), intent(out) :: lnfugTz, lnfugPz !<
    ! Locals
    integer :: i, j
    ! Lapack
    integer :: INFO, LWORK
    real, dimension(nc,nc) :: A
    real, dimension(nc) :: W
    real, dimension(3*nc) :: WORK
    call thermo(t,p,z,phase,lnfugz,lnfugTz,lnfugPz,B)

    do i=1,nc
      do j=1,nc
        B(i,j) = B(i,j)*v(i)*v(j)
      enddo
      B(i,i) = B(i,i) + 1.0
    enddo

    LWORK = 3*nc
    A = B
    call DSYEV( 'V', 'U', nc, A, nc, W, WORK, LWORK, INFO )
    if (INFO /= 0) then
      call stoperror('Error in critical::calcBmatrix')
    endif
    lambdaMin = W(1)
    u = A(1:nc,1)
  end subroutine calcBmatrix

  !-------------------------------------------------------------------------
  !> Calculate stability function and differential as function of s
  !>
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine calcFofs(t,p,z,v,u,d,f,dfds,s,phase,lnfugTs,lnfugPs,y,g)
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall composition
    real, dimension(nc), intent(in) :: v !<
    real, dimension(nc), intent(in) :: d !<
    real, intent(in) :: T !< Temperature
    real, intent(in) :: P !< Pressure
    real, intent(in) :: s !< Distance parameter
    real, dimension(nc), intent(in) :: u !< Eigenvector
    integer, intent(in) :: phase !< Phase identifer
    real, intent(out) :: f !<
    real, intent(out) :: dfds !<
    real, dimension(nc), intent(out) :: lnfugTs,lnfugPs !<
    real, dimension(nc), intent(out) :: g,y
    ! Locals
    real, dimension(nc) :: lnfugy
    y = s*u*v + z
    call thermo(t,p,y,phase,lnfugy,lnfugTs,lnfugPs)
    g = log(y)+lnfugy-d
    f = 1.0 + sum(y*(g-1.0))
    dfds = sum(v*u*g)
  end subroutine calcFofs

  !-------------------------------------------------------------------------
  !> Calculate differentials of eigenvecotr (u) wrpt. pressure and temperature
  !>
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine calcUdiff(B,lambdaMin,u,vp,vt,up,ut)
    implicit none
    real, dimension(nc,nc), intent(inout) :: B !<
    real, intent(in) :: lambdaMin !<
    real, dimension(nc), intent(in) :: u !<
    real, dimension(nc), intent(in) :: vp !<
    real, dimension(nc), intent(in) :: vt !<
    real, dimension(nc), intent(out) :: up !<
    real, dimension(nc), intent(out) :: ut !<
    ! Locals
    integer :: i, k
    ! Lapack
    integer, dimension(nc) :: iPiv
    integer :: info

    do i=1,nc
      B(i,i) = B(i,i) - lambdaMin
    enddo

    ! Factor A = P*L*U
    ! P - permutation
    ! L - Lower triangular (unit diagonal, not stored in B)
    ! U - Upper triangular
    call DGETRF( nc, nc, B, nc, iPiv, info)
    up = vp
    ut = vt
    ! Solve A * X = B.
    ! Apply row interchanges to the right hand sides.
    CALL DLASWP( 1, up, nc, 1, nc, IPIV, 1 )
    CALL DLASWP( 1, ut, nc, 1, nc, IPIV, 1 )
    ! Solve L*X = u, overwriting u with X.
    CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', nc, 1,&
         1.0, B, nc, up, nc )
    CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', nc, 1,&
         1.0, B, nc, ut, nc )
    ! Back substitute, setting up(nc) = 0.0
    up(nc) = 0.0
    ut(nc) = 0.0
    do k = nc-1,1,-1
      if (up(k) /= 0.0) then
        up(k) = up(k)/B(k,k)
        do i = 1,k - 1
          up(i) = up(i) - up(k)*B(i,k)
        enddo
      endif
      if (ut(k) /= 0.0) then
        ut(k) = ut(k)/B(k,k)
        do i = 1,k - 1
          ut(i) = ut(i) - ut(k)*B(i,k)
        enddo
      endif
    enddo
    !
    up = up - sum(u*up)*u
    ut = ut - sum(u*ut)*u
  end subroutine calcUdiff

  !-------------------------------------------------------------------------
  !> Calculate minimum eigenvalue for stability matrix
  !>
  !> \author MH, 2013-10-10
  !-------------------------------------------------------------------------
  function calcStabMinEig(t,p,z,phase) result(lambdaMin)
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall composition
    real, intent(in) :: T !< Temperature
    real, intent(in) :: P !< Pressure
    integer, intent(in) :: phase !< Phase identifer
    real :: lambdaMin !< Eigenvalue
    ! Locals
    integer :: i, j
    real, dimension(nc) :: v, lnfug
    real, dimension(nc,nc) :: B !< Stability matrix
    ! Lapack
    integer :: INFO, LWORK
    real, dimension(nc) :: W
    real, dimension(3*nc) :: WORK
    call thermo(t,p,z,phase,lnfug,lnfugx=B)
    v = sqrt(z)
    do i=1,nc
      do j=1,nc
        B(i,j) = B(i,j)*v(i)*v(j)
      enddo
      B(i,i) = B(i,i) + 1.0
    enddo

    LWORK = 3*nc
    call DSYEV( 'N', 'U', nc, B, nc, W, WORK, LWORK, INFO )
    if (INFO /= 0) then
      call stoperror('Error in critical::calcMinEig')
    endif
    lambdaMin = W(1)
  end function calcStabMinEig

  !--------------------------------------------------------------------------
  !> Calculate minimum eigenvalue for stability matrix
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  function calcStabMinEigTV(t,v,z) result(lambdaMin)
    use eosTV, only: thermo_tv
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall composition
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: v !< Volume (m3/mol)
    real :: lambdaMin !< Eigenvalue
    ! Locals
    integer :: i, j
    real, dimension(nc) :: zs, lnfug
    real, dimension(nc,nc) :: B !< Stability matrix
    ! Lapack
    integer :: INFO, LWORK
    real, dimension(nc) :: W
    real, dimension(3*nc) :: WORK
    call thermo_tv(t,v,z,lnfug,lnphin=B)
    zs = sqrt(z)
    do i=1,nc
      do j=1,nc
        B(i,j) = B(i,j)*zs(i)*zs(j)
      enddo
    enddo
    do j=1,nc
      if (zs(j) == 0) then
        B(j,j) = 1
      endif
    enddo
    LWORK = 3*nc
    call DSYEV( 'N', 'U', nc, B, nc, W, WORK, LWORK, INFO )
    if (INFO /= 0) then
      call stoperror('Error in critical::calcMinEigTV')
    endif
    lambdaMin = W(1)
  end function calcStabMinEigTV

  !--------------------------------------------------------------------------
  !> Calculate stability matrix (B) and minimum eigenvalue with eigenvector
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine calcBmatrixTV(t,v,z,zs,B,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    use eosTV, only: thermo_tv
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall composition
    real, dimension(nc), intent(in) :: zs !< z
    real, intent(in) :: T !< Temperature
    real, intent(in) :: v !< Specific volume (m3/mol)
    real, dimension(nc,nc), intent(out) :: B !< Stability matrix
    real, intent(out) :: lambdaMin !< Eigenvalue
    real, dimension(nc), intent(out) :: u !< Eigenvector
    real, dimension(nc), intent(out) :: lnfugz !<
    real, dimension(nc), intent(out) :: lnfugTz, lnfugVz !<
    ! Locals
    integer :: i, j
    ! Lapack
    integer :: INFO, LWORK
    real, dimension(nc,nc) :: A
    real, dimension(nc) :: L
    real, dimension(3*nc) :: WORK
    call thermo_tv(t,v,z,lnfugz,lnfugTz,lnfugVz,B)

    do i=1,nc
      do j=1,nc
        B(i,j) = B(i,j)*zs(i)*zs(j)
      enddo
    enddo
    do j=1,nc
      if (zs(j) == 0) then
        B(j,j) = 1.0e8
      endif
    enddo
    LWORK = 3*nc
    A = B
    call DSYEV( 'V', 'U', nc, A, nc, L, WORK, LWORK, INFO )
    if (INFO /= 0) then
      call stoperror('Error in critical::calcBmatrixTV')
    endif
    lambdaMin = L(1)
    u = A(1:nc,1)
  end subroutine calcBmatrixTV

  !--------------------------------------------------------------------------
  !> Function value for calculating stability limit given v
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine stabFunTV(Fun,X,param)
    implicit none
    real, dimension(1), intent(out) :: Fun !< Function value
    real, dimension(1), intent(in) :: X !< Variables
    real, dimension(nc+1), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Specific volume [m3/mol]
    real, dimension(nc) :: zs,lnfugz,lnfugTz,lnfugVz
    real, dimension(nc) :: u
    real, dimension(nc,nc) :: Bmat
    real :: lambdaMin
    z = param(1:nc)
    v = param(nc+1)
    t = X(1)
    zs = sqrt(z)
    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    Fun = 0.5*lambdaMin
  end subroutine stabFunTV

  !--------------------------------------------------------------------------
  !> Differentials of b wrpt. T
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine stabJacTV(dF,X,param)
    implicit none
    real, dimension(1,1), intent(out) :: dF !< Function differential
    real, dimension(1), intent(in) :: X !< Variables
    real, dimension(nc+1), intent(in) :: param !< Parameters
    ! Locals
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Specific volume [m3/mol]
    real, dimension(nc) :: zs,lnfugz,lnfugTz,lnfugVz
    real, dimension(nc) :: u
    real, dimension(nc,nc) :: Bmat
    real :: bt, dT, lambdaT
    real :: lambdaMin,lambdaMin1
    z = param(1:nc)
    v = param(nc+1)
    t = X(1)
    zs = sqrt(z)
    dT = T*eps
    ! Central difference
    T = X(1) - dT
    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    T = X(1) + dT
    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin1,lnfugz,lnfugTz,lnfugVz)
    lambdaT = (lambdaMin1-lambdaMin)/(2.0*dT)
    bt = 0.5*lambdaT
    df(1,1) = bt
  end subroutine stabJacTV

  !--------------------------------------------------------------------------
  !> Function value for calculating stability limit given pressure
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine stabFun(Fun,X,param)
    implicit none
    real, dimension(1), intent(out) :: Fun !< Function value
    real, dimension(1), intent(in) :: X !< Variables
    real, dimension(nc+2), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: P !< Pressure [Pa]
    real, dimension(nc) :: zs,lnfugz,lnfugTz,lnfugPz
    real, dimension(nc) :: u
    real, dimension(nc,nc) :: Bmat
    real :: lambdaMin
    integer :: phase
    z = param(1:nc)
    p = param(nc+1)
    phase = int(param(nc+2))
    t = X(1)
    zs = sqrt(z)
    call calcBmatrix(t,p,z,zs,phase,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugPz)
    Fun = 0.5*lambdaMin
  end subroutine stabFun

  !--------------------------------------------------------------------------
  !> Differentials of b wrpt. T
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine stabJac(dF,X,param)
    implicit none
    real, dimension(1,1), intent(out) :: dF !< Function differential
    real, dimension(1), intent(in) :: X !< Variables
    real, dimension(nc+2), intent(in) :: param !< Parameters
    ! Locals
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: p !< Pressure [Pa]
    real, dimension(nc) :: zs,lnfugz,lnfugTz,lnfugPz
    real, dimension(nc) :: u
    real, dimension(nc,nc) :: Bmat
    real :: bt, dT, lambdaT
    real :: lambdaMin,lambdaMin1
    integer :: phase
    z = param(1:nc)
    P = param(nc+1)
    phase = int(param(nc+2))
    t = X(1)
    zs = sqrt(z)
    dT = T*eps
    ! Central difference
    T = X(1) - dT
    call calcBmatrix(t,p,z,zs,phase,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugPz)
    T = X(1) + dT
    call calcBmatrix(t,p,z,zs,phase,Bmat,u,lambdaMin1,lnfugz,lnfugTz,lnfugPz)
    lambdaT = (lambdaMin1-lambdaMin)/(2.0*dT)
    bt = 0.5*lambdaT
    df(1,1) = bt
  end subroutine stabJac

  !-------------------------------------------------------------------------
  !> Calculate critical point in variables T and V
  !! Method of Michelsen 1984 is implemented using
  !! temperature (T) and volume (V) as variables
  !! If the initial temperature T < 0.0, the pseudo-critical
  !! temperature is used as initial value.
  !! If the initial volume V < 0.0, the V = 4.0*b is
  !! used as initial value.
  !!
  !! \author MH, 2016-01
  !-------------------------------------------------------------------------
  subroutine calcCriticalTV(t,v,Z,ierr,tol,p)
    use thermopack_constants, only: get_templimits
    use nonlinear_solvers
    use eosdata, only: eosCPA
    use numconstants, only: Small
    use thermo_utils, only: isSingleComp, get_b_linear_mix
    use eosTV, only: pressure
    implicit none
    real, dimension(nc), intent(in) :: Z !< Trial composition (Overall compozition)
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: v !< Pressure [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: tol !< Toleranse
    real, optional, intent(out) :: p !< Pressure (Pa)
    ! Locals
    real :: t0, v0
    real, dimension(2*nc) :: param
    real, dimension(2) :: X, xmax, xmin
    !real :: Fun(2), dF(2,2), Fun2(2), numJac(2,2)
    type(nonlinear_solver) :: solver
    real :: tpc,ppc,zpc,vpc,b
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    ierr = 0
    v0 = v
    t0 = t
    param(1:nc) = z
    param(nc+1:2*nc) = sqrt(z)

    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    ! Calculate co-volume
    b = get_b_linear_mix(z)
    if (v <= 0.0) then
      v = 4.0 * b ! m3/mol
    endif
    if (t <= 0.0) then
      ! Use pseudocritical temperature
      call pseudo_safe(z,tpc,ppc,zpc,vpc)
      t = tpc
    endif
    X(1) = t
    X(2) = v

    !...................................
    ! call critFunTV(Fun,X,param)
    ! !print *,'Fun',Fun
    ! call critJacTV(dF,X,param)
    ! X(1) = t + t*1.0e-4
    ! print *,"dt",t*1.0e-4
    ! call critFunTV(Fun2,X,param)
    ! !print *,'Fun2 t',Fun2
    ! numJac(:,1) = (Fun2-Fun)/(t*1.0e-4)
    ! X(1) = t
    ! X(2) = X(2) + X(2)*1.0e-4
    ! print *,"dv",X(2)*1.0e-4
    ! call critFunTV(Fun2,X,param)
    ! !print *,'Fun2 v',Fun2
    ! numJac(:,2) = (Fun2-Fun)/(X(2)*1.0e-4)
    ! print *,'num jac 1',numJac(:,1)
    ! print *,'analyt   ',dF(:,1)
    ! print *,'num jac 2',numJac(:,2)
    ! print *,'analyt   ',dF(:,2)
    ! call exit(1)

    solver%abs_tol = 1.0e-7
    if (present(tol)) then
      solver%abs_tol = tol
    endif
    solver%rel_tol = 1.0e-20
    solver%max_it = 200
    call get_templimits(xmin(1), xmax(1))
    if (needalt .and. .not. isCPA) then
      xmin(2) = 1.0e-8
    else
      xmin(2) = b + Small ! m3/mol
    endif
    xmax(2) = 100.0
    solver%ls_max_it = 3
    call nonlinear_solve(solver,critFunTV,critJacTV,critJacTV,limit_dx,&
         premterm_at_dx_zero, setXv,x,xmin,xmax,param)
    ierr = solver%exitflag

    ! Solution
    t = X(1)
    v = X(2)

    !if (isSingleComp(Z) .and. ierr == 2. .and. solver%error_on_exit < solver%abs_tol) then
    !  ierr = 0
    !else
    if (ierr /= 0) then
      if (verbose) then
        if (solver%error_on_exit < 1.0e-5) then
          print *,'Critical point solver: Error at solution: ', &
               solver%error_on_exit
        else
          print *,'Not able to locate critical point'
          print *,'Initial volume (m3/mol): ', v0
          print *,'Initial temperature (K): ', t0
          v = 0.0
          t = 0.0
        endif
      endif
    else
      if (present(p)) then
        p = pressure(t,v,z)
      endif
    endif
  end subroutine calcCriticalTV

  !-------------------------------------------------------------------------
  !> Function value for calculation of critical point for mixtures.
  !>
  !>
  !> \author MH, 2016-01
  !-------------------------------------------------------------------------
  subroutine critFunTV(Fun,X,param)
    implicit none
    real, dimension(2), intent(out) :: Fun !< Function value
    real, dimension(2), intent(in) :: X !< Variables
    real, dimension(2*nc), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Specific volume [m3/mol]

    real, dimension(nc) :: w,lnfugz,lnfugTz,lnfugVz
    real, dimension(nc) :: u,y,y1
    real, dimension(nc,nc) :: Bmat
    real, dimension(nc) :: lnfugTs,lnfugVs,g,lnfugTs1,lnfugVs1,g1
    real :: Py, Pvy, Pty, Py1, Pvy1, Pty1, Pz, Pvz, Ptz
    real :: b,c
    real :: s, dfds, f, f1, dfds1
    real :: lambdaMin
    real, parameter :: ss = 1.0e-4
    z = param(1:nc)
    w = param(nc+1:2*nc)
    t = X(1)
    v = X(2)
    call calcBmatrixTV(t,v,z,w,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    b = 0.5*lambdaMin
    Pz = pressure(t,v,z,Pvz,PTz)
    s = ss
    call calcFofsTV(t,v,z,w,u,lnfugz,Pz,f,dfds,s,lnfugTs,lnfugVs,&
         y,g,Py,Pvy,PTy)
    s = -ss
    call calcFofsTV(t,v,z,w,u,lnfugz,Pz,f1,dfds1,s,lnfugTs1,lnfugVs1,&
         y1,g1,Py1,Pvy1,PTy1)
    c = (dfds+dfds1)/(6.0*s**2)
    Fun(1) = b
    Fun(2) = c
    !print *,'t,v,b,c',t,v,b,c
  end subroutine critFunTV

  !-------------------------------------------------------------------------
  !> Differentials of b and c.
  !>
  !>
  !> \author MH, 2016-01
  !-------------------------------------------------------------------------
  subroutine critJacTV(dF,X,param)
    implicit none
    real, dimension(2,2), intent(out) :: dF !< Function differential
    real, dimension(2), intent(in) :: X !< Variables
    real, dimension(2*nc), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Pressure [m3/mol]
    real, dimension(nc) :: zs,lnfugz,lnfugTz,lnfugVz,wv,wt,Bvu,Btu
    real, dimension(nc) :: u,uv,ut,y,y1
    real, dimension(nc,nc) :: Bmat
    real, dimension(nc) :: lnfugTs,lnfugVs,g
    real, dimension(nc) :: lnfugTs1,lnfugVs1,g1
    real :: cv,ct,bv,bt
    real :: Py,Pvy,PTy,Py1,Pvy1,PTy1,Pz,Pvz,Ptz
    real :: s, dfds, f, f1, dfds1, dfdv1, dfdv, dfdt1, dfdt
    real :: lambdaMin, lambdaV, lambdaT
    real, parameter :: ss = 1.0e-4
    !
    z = param(1:nc)
    zs = param(nc+1:2*nc)
    t = X(1)
    v = X(2)

    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    Pz = pressure(t,v,z,Pvz,PTz)

    s = -ss
    call calcFofsTV(t,v,z,zs,u,lnfugz,Pz,f1,dfds1,s,lnfugTs1,lnfugVs1,&
         y1,g1,Py1,Pvy1,PTy1)
    s = ss
    call calcFofsTV(t,v,z,zs,u,lnfugz,Pz,f,dfds,s,lnfugTs,lnfugVs,y,g,Py,&
         Pvy,PTy)

    Bvu = 0.5*zs*(lnfugVs-lnfugVs1)/s
    lambdaV = sum(u*Bvu)
    Btu = 0.5*zs*(lnfugTs-lnfugTs1)/s
    lambdaT = sum(u*Btu)
    wv = lambdaV*u-Bvu
    wt = lambdaT*u-Btu
    call calcUdiff(Bmat,lambdaMin,u,wv,wt,uv,ut)
    bv = 0.5*lambdaV
    bt = 0.5*lambdaT

    dfdv = sum(y*(lnfugVs-lnfugVz)) + sum(g*s*zs*uv) &
         - (Pvy-Pvz)*v/(Rgas*T) - (Py-Pz)/(Rgas*T)
    dfdT = sum(y*(lnfugTs-lnfugTz))  + sum(g*s*zs*ut) &
         - (PTy-PTz)*v/(Rgas*T) + (Py-Pz)*v/(Rgas*T**2)
    dfdv1 = sum(y1*(lnfugVs1-lnfugVz)) - sum(g1*s*zs*uv) &
         - (Pvy1-Pvz)*v/(Rgas*T) - (Py1-Pz)/(Rgas*T)
    dfdT1 = sum(y1*(lnfugTs1-lnfugTz)) - sum(g1*s*zs*ut) &
         - (PTy1-PTz)*v/(Rgas*T) + (Py1-Pz)*v/(Rgas*T**2)
    cv = 0.5*(dfdv - dfdv1)/s**3
    ct = 0.5*(dfdt - dfdt1)/s**3

    df(1,1) = bt
    df(1,2) = bv
    df(2,1) = ct
    df(2,2) = cv

    !print *,"tc,vc",t,v
    !print *,"bt,bv,ct,cv",bt,bv,ct,cv
  end subroutine critJacTV

  !-------------------------------------------------------------------------
  !> Calculate stability function and differential as function of s
  !>
  !> \author MH, 2016-01
  !-------------------------------------------------------------------------
  subroutine calcFofsTV(t,v,z,zs,u,lnfugz,Pz,f,dfds,s,lnfugTs,lnfugVs,&
       y,g,Py,Pvy,PTy)
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall composition
    real, dimension(nc), intent(in) :: zs !<
    real, intent(in) :: T !< Temperature
    real, intent(in) :: v !< Volume
    real, intent(in) :: s !< Distance parameter
    real, dimension(nc), intent(in) :: u !< Eigenvector
    real, dimension(nc), intent(in) :: lnfugz !< Log fugacity at z
    real, intent(in) :: Pz !< Pressure
    real, intent(out) :: f !<
    real, intent(out) :: dfds !<
    real, intent(out) :: Py, Pvy, PTy
    real, dimension(nc), intent(out) :: lnfugTs,lnfugVs !<
    real, dimension(nc), intent(out) :: g,y
    ! Locals
    real, dimension(nc) :: lnfugy
    y = s*u*zs + z
    call thermo_tv(t,v,y,lnfugy,lnfugTs,lnfugVs)
    Py = pressure(t,v,y,Pvy,PTy)
    g = lnfugy-lnfugz
    f = sum(y*g) - (Py-Pz)*v/(T*Rgas)
    dfds = sum(zs*u*g)
  end subroutine calcFofsTV

  !-------------------------------------------------------------------------
  !> Calculate critical point specified z (s=1), T (s=2), V (s=3) or P (s=4)
  !! Good initial values are assumed
  !!
  !! \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine calcCriticalZ(t,v,P,Z,s,ierr,tol,free_comp,iter)
    use thermopack_constants, only: get_templimits
    use nonlinear_solvers
    use eosdata, only: eosCPA
    use numconstants, only: Small
    use utilities, only: isXwithinBounds
    use thermo_utils, only: get_b_linear_mix
    implicit none
    real, dimension(nc), intent(inout) :: Z !< Trial composition (Overall compozition)
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: v !< Volume [m3/mol]
    real, intent(inout) :: P !< Pressure [Pa]
    integer, intent(in) :: s !< Specification
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: tol !< Toleranse
    integer, optional, intent(in) :: free_comp !< Component variable
    integer, optional, intent(out) :: iter !< Number of iterations
    ! Locals
    real :: t0, v0, z0(nc), b
    real, dimension(nc+3) :: param
    real, dimension(4) :: X, xmax, xmin
    !real :: Fun(4), dF(4,4), Fun2(4), numJac(4,4), X1(4)
    type(nonlinear_solver) :: solver
    integer :: ic
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    if (nc == 1 .or. nc > 2) then
      call stoperror("calcCriticalZ: Only two components can be active.")
    endif

    ierr = 0
    v0 = v
    t0 = t
    z0 = z
    param(1:nc) = z
    param(nc+1) = s
    if (present(free_comp)) then
      ic = free_comp
    else
      ic = 1
    endif
    param(nc+3) = ic ! Free component
    X(1) = Z(ic)
    X(2) = log(t)
    X(3) = log(v)
    X(4) = log(P)

    param(nc+2) = X(s)

    !...................................
    ! print *,"Testing differentials"
    ! call critFunZ(Fun,X,param)
    ! !print *,'Fun',Fun
    ! call critJacZ(dF,X,param)
    ! X1 = X
    ! X1(1) = X1(1) + X1(1)*1.0e-4
    ! call critFunZ(Fun2,X1,param)
    ! !print *,'Fun2 z',Fun2
    ! numJac(:,1) = (Fun2-Fun)/(X1(1)*1.0e-4)
    ! X1 = X
    ! X1(2) = X1(2) + X1(2)*1.0e-4
    ! call critFunZ(Fun2,X1,param)
    ! !print *,'Fun2 lnT',Fun2
    ! numJac(:,2) = (Fun2-Fun)/(X1(2)*1.0e-4)
    ! X1 = X
    ! X1(3) = X1(3) + X1(3)*1.0e-4
    ! call critFunZ(Fun2,X1,param)
    ! !print *,'Fun2 lnV',Fun2
    ! numJac(:,3) = (Fun2-Fun)/(X1(3)*1.0e-4)
    ! X1 = X
    ! X1(4) = X1(4) + X1(4)*1.0e-4
    ! call critFunZ(Fun2,X1,param)
    ! !print *,'Fun2 lnP',Fun2
    ! numJac(:,4) = (Fun2-Fun)/(X1(4)*1.0e-4)
    ! print *,'jac'
    ! print *,numJac(:,1)
    ! print *,dF(:,1)
    ! print *,numJac(:,2)
    ! print *,dF(:,2)
    ! print *,numJac(:,3)
    ! print *,dF(:,3)
    ! print *,numJac(:,4)
    ! print *,dF(:,4)
    ! call exit(1)

    solver%abs_tol = 1.0e-6
    if (present(tol)) then
      solver%abs_tol = tol
    endif
    solver%rel_tol = 1.0e-20
    solver%max_it = 200
    call get_templimits(xmin(2), xmax(2))
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      xmin(3) = 1.0e-8
    else
      ! Calculate co-volume
      b = get_b_linear_mix(z)
      xmin(3) = b + Small ! m3/mol
    endif
    xmax(3) = 100.0
    ! Convert to logartimic values
    xmin(2:3) = log(xmin(2:3))
    xmax(2:3) = log(xmax(2:3))
    ! Composition
    xmax(1) = 1.0
    xmin(1) = 0.0
    ! Pressure
    xmax(4) = 1e50
    xmin(4) = -1e50
    call isXwithinBounds(4,X,Xmin,Xmax,"x,ln(T),ln(v),ln(P)",&
         "calcCriticalZ: Initial values not within bounds!!")
    call nonlinear_solve(solver,critFunZ,critJacZ,critJacZ,limit_dx,&
         premterm_at_dx_zero, setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    if (present(iter)) then
      iter = solver%iter
    endif

    ! Solution
    z(ic) = 0.0
    z = z/sum(z)
    z = z*(1.0-X(1))
    z(ic) = X(1)
    t = exp(X(2))
    v = exp(X(3))
    if (s /= 4) then
      P = pressure(t,v,z)
    endif
    if (ierr /= 0) then
      if (verbose) then
        print *,'Not able to locate critical point'
        print *,'Initial volume (m3/mol): ', v0
        print *,'Initial temperature (K): ', t0
        print *,'Initial composition (-): ', z0
        print *,'Specification : ', s
        if (s == 4) then
          print *,'Pressure given (Pa): ', P
        endif
      endif
    endif
  end subroutine calcCriticalZ

  !-------------------------------------------------------------------------
  !> Function value for calculation of critical point for mixtures.
  !>
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine critFunZ(Fun,X,param)
    use eosTV, only: pressure
    implicit none
    real, dimension(4), intent(out) :: Fun !< Function value
    real, dimension(4), intent(in) :: X !< Variables
    real, dimension(nc+3), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Specific volume [m3/mol]
    real :: P
    integer :: ic, is
    real, dimension(nc) :: w,lnfugz,lnfugTz,lnfugVz
    real, dimension(nc) :: u,y,y1
    real, dimension(nc,nc) :: Bmat
    real, dimension(nc) :: lnfugTs,lnfugVs,g,lnfugTs1,lnfugVs1,g1
    real :: Py, Pvy, Pty, Py1, Pvy1, Pty1, Pz, Pvz, Ptz
    real :: b,c
    real :: s, dfds, f, f1, dfds1
    real :: lambdaMin

    z = param(1:nc)
    ic = nint(param(nc+3))
    z(ic) = 0.0
    if (sum(z) > 0) then
      z = z/sum(z)
    endif
    z = z*(1.0-X(1))
    z(ic) = X(1)
    w = sqrt(z)

    t = exp(X(2))
    v = exp(X(3))
    call calcBmatrixTV(t,v,z,w,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    b = 0.5*lambdaMin
    Pz = pressure(t,v,z,Pvz,PTz)
    s = 1.0e-4
    call calcFofsTV(t,v,z,w,u,lnfugz,Pz,f,dfds,s,lnfugTs,lnfugVs,&
         y,g,Py,Pvy,PTy)
    s = -1.0e-4
    call calcFofsTV(t,v,z,w,u,lnfugz,Pz,f1,dfds1,s,lnfugTs1,lnfugVs1,&
         y1,g1,Py1,Pvy1,PTy1)
    c = (dfds+dfds1)/(6.0*s**2)
    is = nint(param(nc+1))
    Fun(1) = X(is) - param(nc+2)
    Fun(2) = b
    Fun(3) = c
    if (is == 4) then
      P = pressure(t,v,z)
      Fun(4) = (exp(X(4)) - P)/exp(param(nc+2))
    else
      ! Dummy equation
      Fun(4) = 0.0
    endif
    !print *,t,v,b,c
  end subroutine critFunZ

  !-------------------------------------------------------------------------
  !> Differentials of critical equation system.
  !>
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine critJacZ(dF,X,param)
    implicit none
    real, dimension(4,4), intent(out) :: dF !< Function differential
    real, dimension(4), intent(in) :: X !< Variables
    real, dimension(nc+3), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Pressure [m3/mol]
    real, dimension(nc) :: zs
    integer :: ic, is
    real :: p,dpdt,dpdv,dpdn(nc),dz,vmult(nc),P_spec
    ! TV intrface
    real, dimension(2,2) :: dF_TV !< Function differential
    real, dimension(2) :: X_TV !< Variables
    real, dimension(2*nc) :: param_TV !< Parameters
    ! Perturbation
    real, dimension(4) :: Fun1, Fun2, X1, X2, dFdz !<
    !
    z = param(1:nc)
    ic = nint(param(nc+3))
    z(ic) = 0.0
    if (sum(z) > 0) then
      z = z/sum(z)
    endif
    z = z*(1.0-X(1))
    z(ic) = X(1)
    zs = sqrt(z)
    t = exp(X(2))
    v = exp(X(3))

    dF = 0.0
    ! Get sub matrix
    param_TV(1:nc) = z
    param_TV(nc+1:2*nc) = zs
    X_TV(1) = t
    X_TV(2) = v
    call critJacTV(dF_TV,X_TV,param_TV)
    ! Convert from dt -> dlnt
    dF_TV(:,1) = T*dF_TV(:,1)
    ! Convert from dv -> dlnv
    dF_TV(:,2) = v*dF_TV(:,2)
    ! Assign to overall Jacobian
    dF(2:3,2:3) = dF_TV
    !
    ! Compozition differentials
    ! Two-sided numerical differential
    dz = 1.0e-4
    X1 = X
    if (X(1) > dz) then
      X1(1) = X1(1) - dz
    endif
    X2 = X
    if (x(1) < 1.0 - dz) then
      X2(1) = X2(1) + dz
    endif
    dz = X2(1) - X1(1)
    call critFunZ(Fun1,X1,param)
    call critFunZ(Fun2,X2,param)
    dFdz = (Fun2-Fun1)/dz
    dF(2:3,1) = dFdz(2:3)
    !
    ! Specification equaition
    is = nint(param(nc+1))
    dF(1,is) = 1.0
    !
    ! Pressure function
    if (is == 4) then
      P_spec = exp(param(nc+2))
      dF(4,4) = exp(X(4))/P_spec
      P = pressure(t,v,z,dpdv,dpdt,dpdn=dpdn)
      vmult = -1
      vmult(ic) = 1
      dF(4,1) = -sum(dpdn/P_spec*vmult) !dFdz(4)
      dF(4,2) = -T*dpdt/P_spec
      dF(4,3) = -v*dpdv/P_spec
    else
      dF(4,4) = 1.0
    endif

  end subroutine critJacZ

  !-------------------------------------------------------------------------
  !> Sensitivities of critical equation system
  !>
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine critZsensitivity(Z,ic,X,dXdS,s,ierr)
    implicit none
    real, dimension(4), intent(out) :: dXds !< System sensitivities
    real, dimension(4), intent(in) :: X !< Variables
    real, dimension(nc), intent(in) :: Z !< Composition
    integer, intent(in) :: s !< Specification
    integer, intent(in) :: ic !< Component specification
    integer, intent(out) :: ierr !< Error flag
    ! Locals
    real, dimension(nc+3) :: param
    real, dimension(4,4) :: Jac
    integer, dimension(4) :: INDX
    integer :: info
    !
    param(1:nc) = z
    param(nc+1) = s
    param(nc+2) = X(s)
    param(nc+3) = ic

    call critJacZ(Jac,X,param)
    dXdS = 0.0
    dXdS(1) = 1.0

    ! Solve equation system
    call DGESV( 4, 1, Jac, 4, INDX, dXdS, 4, info )
    if (info /= 0) then
      ierr = 2
    endif
  end subroutine critZsensitivity

  !-------------------------------------------------------------------------
  !> Calculate critical point in equilibrium with incipient phase
  !! Good initial values are assumed
  !!
  !! \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine calcCriticalEndPoint(t,vc,Zc,y,vy,ierr,tol,free_comp)
    use thermopack_constants, only: get_templimits
    use nonlinear_solvers
    use eosdata, only: eosCPA
    use numconstants, only: Small
    use utilities, only: isXwithinBounds
    use thermo_utils, only: get_b_linear_mix
    implicit none
    real, dimension(nc), intent(inout) :: Zc !< Trial composition (Overall compozition)
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: vc !< Volume [m3/mol]
    real, intent(inout) :: vy !< Volume [m3/mol]
    real, dimension(nc), intent(inout) :: y  !< Incipient phase
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: tol !< Toleranse
    integer, optional, intent(in) :: free_comp !< Component variable
    ! Locals
    real :: t0, v0, z0(nc), bc, y0(nc), vy0, Pc, by
    real, dimension(2) :: param
    real, dimension(6) :: X, xmax, xmin
    !real :: Fun(6), dF(6,6), Fun2(6), X1(6)
    type(nonlinear_solver) :: solver
    integer :: ic
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    if (.not. nc == 2) then
      call stoperror("calcCriticalEndPoint: Only two components can be active.")
    endif

    ierr = 0
    v0 = vc
    t0 = t
    z0 = zc
    vy0 = vy
    y0 = y
    if (present(free_comp)) then
      ic = free_comp
    else
      if (zc(1) > zc(2)) then
        ic = 2
      else
        ic = 1
      endif
    endif
    param(1) = ic ! Free component
    X(1) = Zc(ic)
    X(2) = log(t)
    X(3) = log(vc)
    if (minval(y) < 1.0e-35) then
      ierr = 1
      return
    endif
    X(4:5) = log(y)
    X(6) = log(vy)

    Pc = pressure(t,vc,zc)
    param(2) = max(Pc,1.0e5)

    !...................................
    ! print *,"Testing differentials"
    ! call critFunEnd(Fun,X,param)
    ! call critJacEnd(dF,X,param)
    ! do i=1,6
    !   print *,i,X(i)
    !   X1 = X
    !   dX = sign(max(abs(X1(i)*1.0e-4),1.0e-4),X(i))
    !   X1(i) = X1(i) + dX
    !   call critFunEnd(Fun2,X1,param)
    !   print *,(Fun2-Fun)/dX
    !   print *,dF(:,i)
    !   print *
    ! enddo
    ! call exit(1)

    solver%abs_tol = 1.0e-6
    if (present(tol)) then
      solver%abs_tol = tol
    endif
    solver%rel_tol = 1.0e-20
    solver%max_it = 200
    call get_templimits(xmin(2), xmax(2))
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)
    if (needalt .and. .not. isCPA) then
      xmin(3) = 1.0e-8
      xmin(6) = log(xmin(3))
    else
      ! Calculate co-volume
      bc = get_b_linear_mix(zc)
      by = get_b_linear_mix(y)
      xmin(3) = bc + Small ! m3/mol
      xmin(6) = log(by + Small) ! m3/mol
    endif
    xmax(3) = 100.0
    ! Convert to logartimic values
    xmin(2:3) = log(xmin(2:3))
    xmax(2:3) = log(xmax(2:3))
    ! Composition
    xmax(1) = 1.0
    xmin(1) = 1.0e-35
    xmax(4:5) = 5.0
    xmin(4:5) = log(1.0e-35)
    ! Incipient volume
    xmax(6) = xmax(3)
    call isXwithinBounds(6,X,Xmin,Xmax,"x,ln(T),ln(vx),ln(y(1:2)),ln(vy)",&
         "calcCriticalEndPoint: Initial values not within bounds!!",ierr)
    if (ierr /= 0) then
      return
    endif
    call nonlinear_solve(solver,critFunEnd,critJacEnd,critJacEnd,limit_dx,&
         premterm_at_dx_zero, setXv,x,xmin,xmax,param)
    ierr = solver%exitflag

    ! Solution
    if (ic == 1) then
      zc(1) = X(1)
      zc(2) = 1.0-X(1)
    else
      zc(2) = X(1)
      zc(1) = 1.0-X(1)
    endif
    t = exp(X(2))
    vc = exp(X(3))
    y = exp(X(4:5))
    vy = exp(X(6))

    if (ierr /= 0) then
      if (verbose) then
        print *,'Not able to locate critical end point'
        print *,'Initial volume (m3/mol): ', v0
        print *,'Initial temperature (K): ', t0
        print *,'Initial composition (-): ', z0
        print *,'Initial incipient volume (m3/mol): ', vy0
        print *,'Initial incipient composition (-): ', y0
      endif
    endif
  end subroutine calcCriticalEndPoint

  !-------------------------------------------------------------------------
  !> Function value for calculation of critical point for mixtures.
  !>
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine critFunEnd(Fun,X,param)
    use eosTV, only: pressure, thermo_tv
    implicit none
    real, dimension(6), intent(out) :: Fun !< Function value
    real, dimension(6), intent(in) :: X !< Variables
    real, dimension(2), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Zc, Zy !< Overall compozition
    real :: t !< Temperature [K]
    real :: vc, vy !< Specific volume [m3/mol]
    real :: Pzy
    integer :: ic
    real, dimension(nc) :: lnfy
    real, dimension(nc) :: w,lnfugz,lnfugTz,lnfugVz
    real, dimension(nc) :: u,y,y1
    real, dimension(nc,nc) :: Bmat
    real, dimension(nc) :: lnfugTs,lnfugVs,g,lnfugTs1,lnfugVs1,g1
    real :: Py, Pvy, Pty, Py1, Pvy1, Pty1, Pz, Pvz, Ptz
    real :: b,c
    real :: s, dfds, f, f1, dfds1
    real :: lambdaMin

    ic = nint(param(1))
    if (ic == 1) then
      zc(1) = X(1)
      zc(2) = 1.0-X(1)
    else
      zc(2) = X(1)
      zc(1) = 1.0-X(1)
    endif
    t = exp(X(2))
    vc = exp(X(3))
    zy = exp(X(4:5))
    vy = exp(X(6))
    !
    w = sqrt(zc)

    call calcBmatrixTV(t,vc,zc,w,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    b = 0.5*lambdaMin
    Pz = pressure(t,vc,zc,Pvz,PTz)
    s = 1.0e-4
    call calcFofsTV(t,vc,zc,w,u,lnfugz,Pz,f,dfds,s,lnfugTs,lnfugVs,&
         y,g,Py,Pvy,PTy)
    s = -1.0e-4
    call calcFofsTV(t,vc,zc,w,u,lnfugz,Pz,f1,dfds1,s,lnfugTs1,lnfugVs1,&
         y1,g1,Py1,Pvy1,PTy1)
    c = (dfds+dfds1)/(6.0*s**2)
    Fun(1) = b
    Fun(2) = c
    Pzy = pressure(t,vy,zy)
    Fun(3) = (Pz-Pzy)/param(2)
    call thermo_tv(t,vy,zy,lnfy)
    Fun(4:5) = lnfugz - lnfy
    Fun(6) = sum(zy) - 1
    !print *,t,v,b,c
  end subroutine critFunEnd

  !-------------------------------------------------------------------------
  !> Differentials of critical equation system.
  !>
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine critJacEnd(dF,X,param)
    use eosTV, only: pressure, thermo_tv
    implicit none
    real, dimension(6,6), intent(out) :: dF !< Function differential
    real, dimension(6), intent(in) :: X !< Variables
    real, dimension(2), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Zc, Zy !< Compozition
    real :: t !< Temperature [K]
    real :: vc !< Pressure [m3/mol]
    real :: vy !< Pressure [m3/mol]
    real, dimension(nc) :: zs
    integer :: ic
    real :: p,dpcdt,dpcdv,dpcdn(nc),dz,vmult(nc)
    real :: dpydt,dpydv,dpydn(nc)
    real, dimension(nc) :: lnfy, lnfc, dlnfydt, dlnfcdt, dlnfydv, dlnfcdv
    real, dimension(nc,nc) :: dlnfydn, dlnfcdn

    ! TV intrface
    real, dimension(2,2) :: dF_TV !< Function differential
    real, dimension(2) :: X_TV !< Variables
    real, dimension(2*nc) :: param_TV !< Parameters
    ! Perturbation
    real, dimension(6) :: Fun1, Fun2, X1, X2, dFdz !<
    !
    ic = nint(param(1))
    if (ic == 1) then
      zc(1) = X(1)
      zc(2) = 1.0-X(1)
    else
      zc(2) = X(1)
      zc(1) = 1.0-X(1)
    endif
    t = exp(X(2))
    vc = exp(X(3))
    zy = exp(X(4:5))
    vy = exp(X(6))
    !
    zs = sqrt(zc)

    dF = 0.0
    ! Get sub matrix
    param_TV(1:nc) = zc
    param_TV(nc+1:2*nc) = zs
    X_TV(1) = t
    X_TV(2) = vc
    call critJacTV(dF_TV,X_TV,param_TV)
    ! Convert from dt -> dlnt
    dF_TV(:,1) = T*dF_TV(:,1)
    ! Convert from dv -> dlnv
    dF_TV(:,2) = vc*dF_TV(:,2)
    ! Assign to overall Jacobian
    dF(1:2,2:3) = dF_TV
    !
    ! Compozition differentials
    ! Two-sided numerical differential
    dz = 1.0e-4
    X1 = X
    if (X(1) > dz) then
      X1(1) = X1(1) - dz
    endif
    X2 = X
    if (x(1) < 1.0 - dz) then
      X2(1) = X2(1) + dz
    endif
    dz = X2(1) - X1(1)
    call critFunEnd(Fun1,X1,param)
    call critFunEnd(Fun2,X2,param)
    dFdz = (Fun2-Fun1)/dz
    dF(1:2,1) = dFdz(1:2)
    !
    ! Pressure function
    vmult = -1
    vmult(ic) = 1
    P = pressure(t,vy,zy,dpydv,dpydt,dpdn=dpydn)
    P = pressure(t,vc,zc,dpcdv,dpcdt,dpdn=dpcdn)
    dF(3,1) = sum(dpcdn*vmult)/param(2)
    dF(3,2) = T*(dpcdt-dpydt)/param(2)
    dF(3,3) = vc*dpcdv/param(2)
    dF(3,4:5) = -zy*dpydn/param(2)
    dF(3,6) = -vy*dpydv/param(2)

    ! Fugacities
    call thermo_tv(t,vy,zy,lnfy,dlnfydt,dlnfydv,dlnfydn)
    call thermo_tv(t,vc,zc,lnfc,dlnfcdt,dlnfcdv,dlnfcdn)
    dF(4,1) = sum(dlnfcdn(1,:)*vmult)
    dF(5,1) = sum(dlnfcdn(2,:)*vmult)
    dF(4:5,2) = T*(dlnfcdt-dlnfydt)
    dF(4:5,3) = vc*dlnfcdv
    dF(4,4:5) = -zy*dlnfydn(1,:)
    dF(5,4:5) = -zy*dlnfydn(2,:)
    dF(4:5,6) = -vy*dlnfydv

    ! Mol fractions
    dF(6,4:5) = zy
  end subroutine critJacEnd

end module critical

  !-------------------------------------------------------------------
  !> Test extraploation on critical line
  !>
  !> \author MH, 2019-05
  !-------------------------------------------------------------------
  subroutine test_critZsensitivity()
    use critical, only: calcCriticalZ, critZsensitivity, &
         calcCriticalTV, calcCriticalEndPoint
    use eosTV, only: pressure
    use thermopack_var, only: base_eos_param, get_active_eos
    use cubic_eos, only: cb_eos
    !use eoslibinit, only: init_thermo
    implicit none
    ! Locals
    real, parameter :: tol = 1.0e-8
    integer :: s, ierr
    real :: ds, X(4), X0(4), dXds(4)
    real :: Zc(2), Pc, Tc, vc
    class(base_eos_param), pointer :: act_eos_ptr
    act_eos_ptr => get_active_eos()

    print *,"Testing critical sensitivity calculation"
    !call init_thermo("Thermopack","SRK","Classic","Classic",2,"CO2,NC14",2)
    select type(p_eos => act_eos_ptr)
      class is (cb_eos)
        p_eos%kij(1,2) = 0.09
        p_eos%kij(2,1) = 0.09
      class default
        print *,"Intended for SRK"
        return
      end select

    Tc=355.88390802375659
    vc=9.5639809221576924E-005
    Zc=(/0.93657108336136841,6.3428916638631594E-002/)
    call calcCriticalTV(Tc,vc,Zc,ierr,tol)
    if (ierr /= 0) stop
    Pc = pressure(Tc,vc,Zc)
    call setX(Tc,vc,Zc,Pc,X0)
    X = X0

    ! Extrapolate state
    s = 3
    call critZsensitivity(Zc,1,X,dXdS,s,ierr)
    ds = 1.0e-5
    X(s) = X(s) + ds
    call getPropFromX(X,Tc,vc,Zc,Pc)
    call calcCriticalZ(Tc,vc,Pc,Zc,s,ierr,tol)
    call setX(Tc,vc,Zc,Pc,X)

    ! Differentials
    print *,"Analyt: ",dXds(1:3)
    print *,"Num:    ",(X(1:3)-X0(1:3))/ds

  contains
    subroutine setX(Tc,vc,Zc,Pc,X)
      real, intent(in) :: Tc,vc,Zc(2),Pc
      real, intent(out) :: X(4)
      X(1) = Zc(1)
      X(2) = log(Tc)
      X(3) = log(vc)
      X(4) = log(Pc)
    end subroutine setX
    subroutine getPropFromX(X,Tc,vc,Zc,Pc)
      real, intent(out) :: Tc,vc,Zc(2),Pc
      real, intent(in) :: X(4)
      Zc(1) = X(1)
      Zc(2) = max(0.0, 1.0 - Zc(1))
      Tc = exp(X(2))
      vc = exp(X(3))
      Pc = exp(X(4))
    end subroutine getPropFromX
  end subroutine test_critZsensitivity
