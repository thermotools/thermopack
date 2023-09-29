!-------------------------------------------------------------------------
!> Methods for mapping spinodal
!!
!-------------------------------------------------------------------------
module spinodal
  use thermopack_constants, only: verbose, VAPPH, LIQPH, SINGLEPH
  use numconstants, only: machine_prec
  use thermopack_var, only: nc, thermo_model, get_active_thermo_model, Rgas, &
       tpTmin, tpTmax
  use eos, only : thermo, entropy, specificVolume
  use eosTV, only : thermo_tv, pressure
  use critical, only: stabFun, stabFunTV, stabJacTV, calcBmatrixTV
  use nonlinear_solvers
  implicit none
  private
  save

  real, parameter :: eps = 1.0e-5
  integer, parameter :: nMax = 1000, nMaxSingle = 100
  real, parameter :: dpdvScaling = 1.0e-4

  integer, parameter, public :: spin_locate_from_entropy = 1
  integer, parameter, public :: spin_locate_from_volume = 2
  integer, parameter, public :: spin_locate_from_enthalpy = 3
  integer, parameter, public :: spin_locate_from_temperature = 4
  integer, parameter, public :: spin_locate_from_pressure = 5
  integer, parameter, public :: spin_locate_from_energy = 6

  abstract interface
    function prop_fun_type(Xs,param) result(fun)
      implicit none
      real, intent(in) :: Xs
      real, intent(inout) :: param(7)
      real :: fun
    end function prop_fun_type
  end interface

  public :: initial_stab_limit_point
  public :: map_stability_limit
  public :: nMax
  public :: rhomax_PR, rho_of_meta_extremum
  public :: locate_spinodal_prop_pure_fluid
  public :: map_meta_isentrope, tv_meta_ps
  public :: locate_spinodal_prop_min_max_pure_fluid

contains

  !--------------------------------------------------------------------------
  !> Given change in volume, extrapolate temperature
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine extrapolateStablimit(v,deltav,Z,T,deltaT)
    implicit none
    real, intent(in) :: v !< Function differential
    real, intent(in) :: deltav !< Variables
    real, dimension(nc), intent(in) :: Z !< Composition
    real, intent(in) :: T !<
    real, intent(out) :: deltaT !<
    ! Locals
    real, dimension(nc) :: zs,lnfugz,lnfugTz,lnfugVz
    real, dimension(nc) :: u
    real, dimension(nc,nc) :: Bmat
    real :: dT, dv, v1, T1, dlambdadT, dlambdadv
    real :: lambdaMin,lambdaMin1
    zs = sqrt(z)
    dT = T*eps
    ! Central difference
    T1 = T - dT
    call calcBmatrixTV(T1,v,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    T1 = T + dT
    call calcBmatrixTV(T1,v,z,zs,Bmat,u,lambdaMin1,lnfugz,lnfugTz,lnfugVz)
    dlambdadT = (lambdaMin1-lambdaMin)/(2.0*dT)
    dv = eps*v
    v1 = v - dv
    call calcBmatrixTV(T,v1,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    v1 = v + dv
    call calcBmatrixTV(T,v1,z,zs,Bmat,u,lambdaMin1,lnfugz,lnfugTz,lnfugVz)
    dlambdadv = (lambdaMin1-lambdaMin)/(2.0*dv)

    deltaT = -dlambdadv*deltav/dlambdadT
  end subroutine extrapolateStablimit

  !--------------------------------------------------------------------------
  !> Given volume and initial temperature find stability limit
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine solveStabLimitTV(T,v,z,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    ! Local
    real :: Tmin, param(nc+1)
    type(nonlinear_solver) :: solver
    real, dimension(1) :: x,xmin,xmax
    solver%abs_tol = 1.0e-8
    param(1:nc) = z
    param(nc+1) = v
    Tmin = min(20.0, tpTmin)
    x = min(max(T,Tmin),tpTmax)
    xmin = Tmin
    xmax = tpTmax
    call nonlinear_solve(solver,stabfunTV,stabjacTV,stabjacTV,&
         limit_dx,premReturn,setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    T = x(1)
  end subroutine solveStabLimitTV

  !--------------------------------------------------------------------------
  !> Given temperature and initial volume find stability limit
  !>
  !> \author MH, 2023-04
  !--------------------------------------------------------------------------
  subroutine solveStabLimitT(T,v,z,ierr)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: t !< Temperature [K]
    real, intent(inout) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    ! Local
    real :: param(nc+1)
    type(nonlinear_solver) :: solver
    real, dimension(1) :: x,xmin,xmax
    solver%abs_tol = 1.0e-8
    param(1:nc) = z
    param(nc+1) = T
    x = v
    xmin = v*0.25
    xmax = v*4.0
    call nonlinear_solve(solver,stabfunT,stabjacT,stabjacT,&
         limit_dx,premReturn,setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    v = x(1)
  end subroutine solveStabLimitT

  !--------------------------------------------------------------------------
  !> Function value for calculating stability limit given temperature
  !>
  !> \author MH, 2023-04
  !--------------------------------------------------------------------------
  subroutine stabFunT(Fun,X,param)
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
    t = param(nc+1)
    v = X(1)
    zs = sqrt(z)
    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    Fun = 0.5*lambdaMin
  end subroutine stabFunT

  !--------------------------------------------------------------------------
  !> Differentials of minimum eigenvalue (b) wrpt. v
  !>
  !> \author MH, 2023-04
  !--------------------------------------------------------------------------
  subroutine stabJacT(dF,X,param)
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
    real :: bv, dv, lambdav
    real :: lambdaMin,lambdaMin1
    z = param(1:nc)
    T = param(nc+1)
    v = X(1)
    zs = sqrt(z)
    dv = v*eps
    ! Central difference
    v = X(1) - dv
    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin,lnfugz,lnfugTz,lnfugVz)
    v = X(1) + dv
    call calcBmatrixTV(t,v,z,zs,Bmat,u,lambdaMin1,lnfugz,lnfugTz,lnfugVz)
    lambdav = (lambdaMin1-lambdaMin)/(2.0*dv)
    bv = 0.5*lambdav
    df(1,1) = bv
  end subroutine stabJacT

  !--------------------------------------------------------------------------
  !> Given pressure find initial point (T,v) for mapping stability line
  !>
  !> \author MH, 2016-02
  !--------------------------------------------------------------------------
  subroutine initial_stab_limit_point(P,z,v,T,phase,ierr,Tmin)
    use puresaturation, only: puresat
    use eos, only: specificVolume
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: P !< Pressure [Pa]
    integer, intent(in) :: phase !< Where to look for initial point
    real, intent(out) :: t !< Temperature [K]
    real, intent(out) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: Tmin
    ! Local
    real :: Pcpy, TsatPure, v0
    if (isSingleComp(z)) then
      Pcpy = P
      call PureSat(TsatPure,Pcpy,Z,.true.)
      ! Do nested loop: P(T(v),v) = P
      T = TsatPure
      call nestedLoopVTSingle(P,z,T,v,phase,ierr)
      if (present(Tmin) .and. ierr == 0) then
        v0 = v
        call singleCompLiqStabilityLimitTemp(T,v0,Tmin,v,ierr)
        if (ierr == 0) then
          T = Tmin
        else
          v = v0
        endif
      endif
    else
      call initialStablimitPointMC(P,z,v,T,phase,ierr,Tmin)
    endif
  end subroutine initial_stab_limit_point

  !--------------------------------------------------------------------------
  !> Given pressure find initial point (T,v) for mapping stability line
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine initialStablimitPointMC(P,z,v,T,phase,ierr,Tmin)
    use puresaturation, only: puresat
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: P !< Pressure [Pa]
    integer, intent(in) :: phase !< Where to look for initial point
    real, intent(out) :: t !< Temperature [K]
    real, intent(out) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: Tmin
    ! Local
    real :: Pcpy, TsatPure
    !
    Pcpy = P
    call PureSat(TsatPure,Pcpy,Z,.true.)
    if (phase == VAPPH) then
      T = TsatPure - 0.1
    else
      T = TsatPure + 0.1
    endif
    call nestedLoopVT(P,z,T,v,phase,ierr,Tmin)
  end subroutine initialStablimitPointMC

  !--------------------------------------------------------------------------
  !> Solve for single point (pressure) on stability limit
  !>
  !> \author MH, 2016-02
  !--------------------------------------------------------------------------
  subroutine nestedLoopVT(p0,z,T,v,phase,ierr,Tmin)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver, &
         NS_PEGASUS
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall compozition
    real, intent(in) :: p0 !< Pressure [Pa]
    real, intent(inout) :: T !< Temperature
    real, intent(inout) :: v !< Specific volume [m3/mol]
    integer, intent(in) :: phase !< Phase flag
    integer, intent(out) :: ierr !< Error message
    real, optional, intent(in) :: Tmin
    !
    ! Local
    type(nonlinear_solver) :: solver
    real :: param(nc+4) !< Paramaters to be passed to stabFunT
    real :: deltav, deltaT, sgn, pn, p, dlnv, vn, Tn, vm, dT
    integer :: i
    integer, parameter :: nIterMax = 1000
    ierr = 0
    ! Start by findign dpdv = 0
    call initialStablimitPointSingleComp(p0,T,z,v,phase,ierr)
    if (ierr /= 0) then
      ierr = 20
      return
    endif
    ! Refine temperature to find stability limit
    call solveStabLimitTV(T,v,Z,ierr)
    if (ierr /= 0) then
      ierr = 21
      return
    endif

    if (present(Tmin)) then
      do i=1,nIterMax
        dT = sign(5.0, Tmin-T)
        if ((T-Tmin)*(T+dT-Tmin) > 0.0) dT = Tmin - T
        deltav=1.0
        call extrapolateStablimit(v,deltav,Z,T,deltaT)
        Tn = T
        vn = v
        v = vn + dT/deltaT
        T = Tn + dT
        call solveStabLimitTV(T,v,Z,ierr)
        if (ierr /= 0) then
          ! Try reducing dlnv
          dT = max(0.5*dT,0.25)
          v = vn + dT/deltaT
          T = Tn + dT
          call solveStabLimitTV(T,v,Z,ierr)
          if (ierr /= 0) then
            ierr = 21
            return
          endif
        endif
        if (abs(Tn-Tmin) <= 1.0e-12*Tmin) exit
      end do
      if (abs(Tn-Tmin) > 1.0e-12*Tmin) then
        ierr = 23
        return
      endif
    else
      dlnv = 5.0e-2
      pn = pressure(T,v,z)
      p = pn
      if (p < p0) then
        sgn = 1.0 ! Increase pressure
      else
        sgn = -1.0 ! Decrease pressure
      endif

      i = 1
      do while ((pn-p0)*(p-p0) > 0.0)
        deltav = exp(log(v) + sgn*dlnv) - v
        call extrapolateStablimit(v,deltav,Z,T,deltaT)
        Tn = T
        vn = v
        v = vn + deltav
        T = Tn + deltaT
        call solveStabLimitTV(T,v,Z,ierr)
        if (ierr /= 0) then
          ! Try reducing dlnv
          dlnv = max(0.5*dlnv,1.0e-4)
          deltav = exp(log(vn) + sgn*dlnv) - vn
          T = Tn
          v = vn
          call extrapolateStablimit(v,deltav,Z,T,deltaT)
          v = vn + deltav
          T = Tn + deltaT
          call solveStabLimitTV(T,v,Z,ierr)
          if (ierr /= 0) then
            ierr = 21
            return
          endif
        endif
        pn = p
        p = pressure(t,v,Z)
        if (i > nIterMax) then ! Infinite loop protection
          ierr = 23
          return
        endif
      end do
      param(1:nc) = z
      param(nc+1) = Tn
      param(nc+2) = vn
      param(nc+3) = deltaT/deltav ! Extrapolation
      param(nc+4) = p0
      solver%abs_tol = 1.0e-7
      solver%isolver = NS_PEGASUS
      vm = v
      call bracketing_solver(vn,vm,stabfunV,v,solver,param)
      ierr = solver%exitflag
      if (ierr /= 0) then
        ierr = 22
        return
      endif
      ! Calculate temperature:
      T = Tn + param(nc+3)*(v-vn)
      call solveStabLimitTV(T,v,Z,ierr)
      if (ierr /= 0) then
        ierr = 21
        return
      endif
    endif
  end subroutine nestedLoopVT

  !--------------------------------------------------------------------------
  !> Solve for single point (pressure) on stability limit
  !> \todo Merge with MC version
  !> \author MH, 2016-02
  !--------------------------------------------------------------------------
  subroutine nestedLoopVTsingle(p0,z,T,v,phase,ierr)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver, &
         NS_PEGASUS
    implicit none
    real, dimension(nc), intent(in) :: z !< Overall compozition
    real, intent(in) :: p0 !< Pressure [Pa]
    real, intent(inout) :: T !< Temperature
    real, intent(inout) :: v !< Specific volume [m3/mol]
    integer, intent(in) :: phase !< phase flag
    integer, intent(out) :: ierr !< Error message
    !
    ! Local
    type(nonlinear_solver) :: solver
    real :: param(nc+4) !< Paramaters to be passed to stabFunT
    real :: dT, dvdT, sgn, pn, p, vn, Tn, Tm
    integer :: i, j
    integer, parameter :: nIterMax = 1000
    ierr = 0
    ! Start by findign dpdv = 0
    call initialStablimitPointSingleComp(p0,T,z,v,phase,ierr)
    if (ierr /= 0) then
      ierr = 20
      return
    endif
    pn = pressure(T,v,z)
    p = pn
    if (p < p0) then
      sgn = 1.0 ! Increase pressure
    else
      sgn = -1.0 ! Decrease pressure
    endif

    i = 1
    do while ((pn-p0)*(p-p0) > 0.0)
      dvdT = dvdT_meta_line(t,v,z)
      Tn = T
      vn = v
      dT = 1.0 ! Reset dT
      v = vn + dvdT*dT*sgn
      T = Tn + dT*sgn
      call StablimitPointSingleComp(T,z,v,ierr)
      j = 1
      do while (ierr /= 0 .OR. j > 10)
        ! Try reducing dT
        dT = 0.5*dT
        v = vn + dvdT*dT*sgn
        T = Tn + dT*sgn
        call StablimitPointSingleComp(T,z,v,ierr)
      enddo
      if (ierr /= 0) then
        ierr = 21
        return
      endif
      pn = p
      p = pressure(t,v,Z)
      if (i > nIterMax) then ! Infinite loop protection
        ierr = 23
        return
      endif
    end do
    param(1:nc) = z
    param(nc+1) = Tn
    param(nc+2) = vn
    param(nc+3) = dvdT ! Extrapolation
    param(nc+4) = p0
    solver%abs_tol = 1.0e-7
    solver%isolver = NS_PEGASUS
    Tm = T
    call bracketing_solver(Tn,Tm,stabfunVsingle,T,solver,param)
    ierr = solver%exitflag
    if (ierr /= 0) then
      ierr = 22
      return
    endif
    ! Calculate volume:
    v = vn + dvdT*(T-Tn)
    call StablimitPointSingleComp(T,z,v,ierr)
    if (ierr /= 0) then
      ierr = 21
      return
    endif
  end subroutine nestedLoopVTsingle

  !--------------------------------------------------------------------------
  !> Pressure error function in variables v, T(v)
  !> \author MH, 2016-01
  !--------------------------------------------------------------------------
  function stabfunV(v,param) result(f)
    use eosTV, only: pressure
    implicit none
    real, intent(in) :: v !< Specific volume [m3/mol]
    real, dimension(nc+4), intent(in) :: param !< Parameter vector
    real :: f
    ! Local
    real, dimension(nc) :: Z !< Overall compozition
    real :: dTdv, dv, dT, p0, T0, v0, T
    integer :: ierr
    z = param(1:nc)
    T0 = param(nc+1)
    v0 = param(nc+2)
    dTdv = param(nc+3)
    p0 = param(nc+4)

    dv = v - v0
    dT = dTdv*dv
    T = T0 + dT
    call solveStabLimitTV(T,v,Z,ierr)
    if (ierr /= 0) then
      f = 1.0
      return
    endif

    f = (pressure(T,v,z) - p0)*1.0e-6
  end function stabfunV

  !--------------------------------------------------------------------------
  !> Pressure error function in variables v, T(v). Pure fluid case.
  !> \author MH, 2016-01
  !--------------------------------------------------------------------------
  function stabfunVsingle(T,param) result(f)
    use eosTV, only: pressure
    implicit none
    real, intent(in) :: T !< Temperature [K]
    real, dimension(nc+4), intent(in) :: param !< Parameter vector
    real :: f
    ! Local
    real, dimension(nc) :: Z !< Overall compozition
    real :: dvdT, dT, p0, T0, v0, v
    integer :: ierr
    z = param(1:nc)
    T0 = param(nc+1)
    v0 = param(nc+2)
    dvdT = param(nc+3)
    p0 = param(nc+4)

    dT = T - T0
    v = v0 + dvdT*dT
    call StablimitPointSingleComp(T,z,v,ierr)
    if (ierr /= 0) then
      f = 1.0
      return
    endif

    f = (pressure(T,v,z) - p0)*1.0e-6
  end function stabfunVsingle

  !--------------------------------------------------------------------------
  !> Dump minimum eigenvalue vs. temperature to file
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine initialSPdebug(P,z,Tmin,Tmax,phase,filename,param)
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: P !< Pressure [Pa]
    integer, intent(in) :: phase !< Where to look for initial point
    real, intent(in) :: Tmin,Tmax !< Temperature limits [K]
    real, intent(in) :: param(nc+2) !< Paramaters to be passed to stabFun
    character(len=*), intent(in) :: filename
    ! Local
    integer, parameter :: nPoints = 1000, ifile = 12
    integer :: i
    real :: F(1),X(1)
    open(file=trim(filename),unit=ifile)
    write(ifile,*) '#Meta stability initial point. P (bar) = ', P*1e-5
    write(ifile,*) '#T (K)',char(9),'lambda'
    do i=1,nPoints
      X = Tmin + (Tmax - Tmin)*real(i-1)/real(nPoints-1)
      call stabFun(F,X,param)
      write(ifile,'(2es19.10e3)') X(1),F(1)
    enddo
    close(ifile)
    stop
  end subroutine initialSPdebug

  !--------------------------------------------------------------------------
  !> Map limit of stable phases
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine map_stability_limit(P0,z,Tmin,Tl,Pl,vl,nl,ierr,dlnv_override,Tliq_start)
    use eos, only: specificVolume
    use eosTV, only: pressure
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    use thermo_utils, only: isSingleComp
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: P0 !< Pressure first point on line [Pa]
    real, intent(in) :: Tmin !< Stop mapping if T < Tmin [K]
    real, intent(out) :: Tl(nMax) !< Line temperature [K]
    real, intent(out) :: vl(nMax) !< Line specific volume [m3/mol]
    real, intent(out) :: Pl(nMax) !< Line pressure [Pa]
    integer, intent(out) :: ierr !< Error flag
    integer, intent(out) :: nl !< Actual number of points on curve
    real, optional, intent(in) :: dlnv_override !< Volume step override [m3/mol]
    real, optional, intent(in) :: Tliq_start
    ! Local
    real :: T, P, v
    real :: deltav, deltaT, dlnv, param(nc+4)
    integer :: i
    real :: TaLiq(nMaxSingle), TaGas(nMaxSingle) !< Temperature [K]
    real :: VaLiq(nMaxSingle), VaGas(nMaxSingle) !< Specific volume [m3/mol]
    real :: PaLiq(nMaxSingle), PaGas(nMaxSingle) !< Pressure [Pa]
    type(nonlinear_solver) :: solver

    if (isSingleComp(Z)) then
      call singleCompStabilityLimit(P0,z,TaLiq,VaLiq,PaLiq,&
           TaGas,VaGas,PaGas,ierr,Tliq_start,Tvap_start=Tmin)
      ierr = 0
      nl = 2*nMaxSingle
      Tl(1:nMaxSingle) = TaLiq
      vl(1:nMaxSingle) = VaLiq
      Pl(1:nMaxSingle) = PaLiq
      Tl(nMaxSingle+1:nl) = TaGas(nMaxSingle:1:-1)
      vl(nMaxSingle+1:nl) = VaGas(nMaxSingle:1:-1)
      Pl(nMaxSingle+1:nl) = PaGas(nMaxSingle:1:-1)

      ! open(file='single.dat',unit=12)
      ! write(12,*) '#Meta stability limit'
      ! write(12,*) '#Tl (K)',char(9),'Pl (bar)',char(9),'vl (m3/mol)',&
      !      char(9),'Tg (K)',char(9),'Pg (bar)',char(9),'vg (m3/mol)'
      ! do i=1,nMaxSingle
      !   write(12,'(6es19.10e3)') TaLiq(i),PaLiq(i)*1e-5,VaLiq(i),&
      !        TaGas(i),PaGas(i)*1.0e-5,VaGas(i)
      ! enddo
      ! close(12)
      ! stop
    else
      call initialStablimitPointMC(P0,z,v,T,LIQPH,ierr,Tliq_start)
      if (ierr /= 0) then
        return
      endif
      if (verbose) then
        print *,T,P0
      endif
      nl = 1
      vl(nl) = v
      Tl(nl) = T
      Pl(nl) = P0
      if (present(dlnv_override)) then
        dlnv = dlnv_override
      else
        dlnv = 2.0e-2
      endif
      do i=2,nMax
        deltav = exp(log(v) + dlnv) - v
        call extrapolateStablimit(v,deltav,Z,T,deltaT)
        v = v + deltav
        t = t + deltaT
        call solveStabLimitTV(T,v,Z,ierr)
        if (ierr /= 0) then
          return
        endif
        p = pressure(t,v,Z)
        nl = nl + 1
        vl(nl) = v
        Tl(nl) = T
        Pl(nl) = p
        if (verbose) then
          print *,T,P,v,ierr
        endif
        if (T < Tmin .AND. deltaT < 0.0) then
          ! Solve for Tmin
          T = Tmin
          v = v + (T-Tl(nl-1))*(vl(nl)-vl(nl-1))/(Tl(nl)-Tl(nl-1))
          call solveStabLimitT(T,v,z,ierr)
          vl(nl) = v
          Tl(nl) = T
          Pl(nl) = p
          exit
        else if (P < P0 .AND. Pl(nl) - Pl(nl-1) < 0.0) then
          ! Solve for P0:
          param(1:nc) = z
          param(nc+1) = Tl(nl-1)
          param(nc+2) = vl(nl-1)
          param(nc+3) = (Tl(nl)-Tl(nl-1))/(vl(nl)-vl(nl-1)) ! Interpolation
          param(nc+4) = p0
          solver%abs_tol = 1.0e-7
          solver%isolver = NS_PEGASUS
          call bracketing_solver(vl(nl-1),vl(nl),stabfunV,v,solver,param)
          ierr = solver%exitflag
          if (ierr /= 0) then
            ierr = 22
            return
          endif
          ! Calculate temperature:
          T = Tl(nl-1) + param(nc+3)*(v-vl(nl-1))
          call solveStabLimitTV(T,v,Z,ierr)
          if (ierr /= 0) then
            ierr = 21
            return
          endif
          p = pressure(t,v,Z)
          vl(nl) = v
          Tl(nl) = T
          Pl(nl) = p
          exit
        endif
      enddo
    endif
  end subroutine map_stability_limit

  !--------------------------------------------------------------------------
  !> Map limit of meta stable phases
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine singleCompStabilityLimit(P0,z,TaLiq,VaLiq,PaLiq,&
       TaGas,VaGas,PaGas,ierr,Tliq_start, Tvap_start)
    use eos, only: specificVolume, getCriticalParam
    use eosTV, only: pressure
    use eosdata, only: eosPC_SAFT, cpaSRK, cpaPR, eosLK
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: P0 !< Pressure [Pa]
    real, optional, intent(in) :: Tliq_start, Tvap_start
    real, intent(out) :: TaLiq(nMaxSingle), TaGas(nMaxSingle) !< Temperature [K]
    real, intent(out) :: VaLiq(nMaxSingle), VaGas(nMaxSingle) !< Specific volume [m3/mol]
    real, intent(out) :: PaLiq(nMaxSingle), PaGas(nMaxSingle) !< Pressure [Pa]
    integer, intent(out) :: ierr
    ! Local
    real :: dT, dvdT
    real :: tc, pc, omega
    integer :: i

    ! ! Critical point
    ! if (cbeos(1)%eosidx == eosPC_SAFT .OR. &
    !      cbeos(1)%eosidx == cpaSRK .OR. &
    !      cbeos(1)%eosidx == cpaPR .OR. &
    !      cbeos(1)%eosidx == eosLK) then
    !   call stoperror('singleCompStabilityLimit: Need critical point solver.')
    ! endif
    call getCriticalParam(1,tc,pc,omega)

    ! Liquid line
    call initial_stab_limit_point(P0,z,VaLiq(1),TaLiq(1),LIQPH,ierr,Tmin=Tliq_start)
    if (ierr /= 0) then
      call stoperror('singleCompStabilityLimit: Not able to find initial point on liquid meta-stability line.')
    endif
    PaLiq(1) = pressure(TaLiq(1),VaLiq(1),z)
    dT = (tc-TaLiq(1))/(nMaxSingle-1)
    do i=2,nMaxSingle-1
      dvdT = dvdT_meta_line(TaLiq(i-1),VaLiq(i-1),z)
      TaLiq(i) = TaLiq(i-1) + dT
      VaLiq(i) = VaLiq(i-1) + dvdT*dT
      call stablimitPointSingleComp(TaLiq(i),z,VaLiq(i),ierr)
      if (ierr /= 0) then
        call stoperror('singleCompStabilityLimit: Not able to solve for point on liquid meta-stability line.')
      endif
      PaLiq(i) = pressure(TaLiq(i),VaLiq(i),z)
      !print *,PaLiq(i),TaLiq(i)
    enddo
    TaLiq(nMaxSingle) = tc
    PaLiq(nMaxSingle) = pc
    call specificVolume(tc,pc,z,LIQPH,VaLiq(nMaxSingle))

    ! Gas line
    call initial_stab_limit_point(P0,z,VaGas(1),TaGas(1),VAPPH,ierr,Tmin=Tvap_start)
    if (ierr /= 0) then
      call stoperror('singleCompStabilityLimit: Not able to find initial point on gas meta-stability line.')
    endif

    dT = (tc-TaGas(1))/(nMaxSingle-1)
    PaGas(1) = pressure(TaGas(1),VaGas(1),z)
    do i=2,nMaxSingle-1
      dvdT = dvdT_meta_line(TaGas(i-1),VaGas(i-1),z)
      TaGas(i) = TaGas(i-1) + dT
      VaGas(i) = VaGas(i-1) + dvdT*dT
      call stablimitPointSingleComp(TaGas(i),z,VaGas(i),ierr)
      if (ierr /= 0) then
        call stoperror('singleCompStabilityLimit: Not able to solve for point on gas meta-stability line.')
      endif
      PaGas(i) = pressure(TaGas(i),VaGas(i),z)
      !print *,PaGas(i),TaGas(i)
    enddo
    TaGas(nMaxSingle) = tc
    PaGas(nMaxSingle) = pc
    call specificVolume(tc,pc,z,VAPPH,VaGas(nMaxSingle))

  end subroutine singleCompStabilityLimit

  !--------------------------------------------------------------------------
  !> Locate temperature on spinodal
  !>
  !> \author MH, 2022-11
  !--------------------------------------------------------------------------
  subroutine singleCompLiqStabilityLimitTemp(T0,v0,T,v,ierr)
    use eos, only: specificVolume, getCriticalParam
    use eosTV, only: pressure
    implicit none
    real, intent(in) :: T0 !< Initial temperature [K]
    real, intent(in) :: v0 !< Specific volume [m3/mol]
    real, intent(in) :: T !< Temperature [K]
    real, intent(out) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr
    ! Local
    real, dimension(nc) :: Z !< Overall compozition
    real :: Ti, dT, dvdT
    integer :: i
    Z = (/1.0/)
    dT = sign(5.0, T-T0)
    Ti = T0
    v = v0
    do i=1,100
      dvdT = dvdT_meta_line(Ti,v,z)
      if ((Ti - T)*(Ti + dT - T) < 0.0) dT = T - Ti
      Ti = Ti + dT
      v = v + dvdT*dT
      call stablimitPointSingleComp(Ti,z,v,ierr)
      if (ierr /= 0) then
        return
      endif
      if (abs(Ti-T) < T*1.0e-12) return
    enddo

  end subroutine singleCompLiqStabilityLimitTemp

  !--------------------------------------------------------------------------
  !> Given pressure find initial point (T,v) for mapping stability line
  !> of single component
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine initialStablimitPointSingleComp(P,T,z,v,phase,ierr)
    use eos, only: specificVolume
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: P !< Pressure [Pa]
    integer, intent(in) :: phase !< Where to look for initial point
    real, intent(in) :: t !< Temperature [K]
    real, intent(out) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    ! Local
    real :: rho_init_in, rho

    call specificVolume(T,P,z,phase,v)
    rho_init_in = 1.0/v
    rho = rho_of_meta_extremum(T,z,phase,rho_init_in)
    v = 1.0/rho
    ierr = 0

  end subroutine initialStablimitPointSingleComp

  !--------------------------------------------------------------------------
  !> Given temperature find volume on stability line
  !> of pure fluid
  !> \author MH, 2016-01
  !--------------------------------------------------------------------------
  subroutine stablimitPointSingleComp(T,z,v,ierr)
    use eos, only: specificVolume
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    use eosTV, only: pressure
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: t !< Temperature [K]
    real, intent(inout) :: v !< Specific volume [m3/mol]
    integer, intent(out) :: ierr !< Error flag
    ! Local
    real :: param(nc+1)
    type(nonlinear_solver) :: solver
    real, dimension(1) :: x,xmin,xmax

    solver%abs_tol = 1.0e-8
    param(1:nc) = z
    param(nc+1) = T
    x(1) = v
    xmin(1) = v/10.0
    xmax(1) = v*10.0
    call nonlinear_solve(solver,stabFunSingleV,stabJacSingleV,&
         stabJacSingleV,limit_dx,premReturn,setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    v = x(1)
  end subroutine stablimitPointSingleComp

  !--------------------------------------------------------------------------
  !> Get dvdT at the meta-stability limit
  !> \author MH, 2016-01
  !--------------------------------------------------------------------------
  function dvdT_meta_line(t,v,z) result(dvdT)
    use eosTV, only: pressure
    implicit none
    real, dimension(nc), intent(in) :: Z !< Overall compozition
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: v !< Specific volume [m3/mol]
    real :: dvdT
    ! Local
    real, parameter :: eps = 1.0e-5
    real :: dT, dv, dpdv, dpdv1, d2pdvdt, d2pdv2, p
    dT = T*eps
    p = pressure(t + dT,v,z,dpdv1)
    p = pressure(t - dT,v,z,dpdv)
    d2pdvdt = 0.5*(dpdv1 - dpdv)/dT
    dv = v*eps
    p = pressure(t,v + dv,z,dpdv1)
    p = pressure(t,v - dv,z,dpdv)
    d2pdv2 = 0.5*(dpdv1 - dpdv)/dv
    dvdT = -d2pdvdt/d2pdv2
  end function dvdT_meta_line

  !--------------------------------------------------------------------------
  !> Function value for calculating stability limit for
  !! pure fluid given temperature
  !!
  !! \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine stabFunSingleV(Fun,X,param)
    use eosTV, only: pressure
    implicit none
    real, dimension(1), intent(out) :: Fun !< Function value
    real, dimension(1), intent(in) :: X !< Variables
    real, dimension(nc+1), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Specific volume [m3/mol]
    real :: p, dpdv
    z = param(1:nc)
    T = param(nc+1)
    v = X(1)
    p = pressure(t,v,Z,dpdv=dpdv)
    Fun(1) = dpdvScaling*dpdv/1.0e6
  end subroutine stabFunSingleV

  !--------------------------------------------------------------------------
  !> Differentials of dpdv for pure fluids
  !>
  !> \author MH, 2015-11
  !--------------------------------------------------------------------------
  subroutine stabJacSingleV(dF,X,param)
    use eosTV, only: pressure
    implicit none
    real, dimension(1,1), intent(out) :: dF !< Function differential
    real, dimension(1), intent(in) :: X !< Variables
    real, dimension(nc+1), intent(in) :: param !< Parameters
    ! Locals
    ! Locals
    real, dimension(nc) :: Z !< Overall compozition
    real :: t !< Temperature [K]
    real :: v !< Specific volume [m3/mol]
    real :: p, dpdv, dpdv1, d2pdv2, dv
    z = param(1:nc)
    T = param(nc+1)
    v = X(1)
    dv = v*eps
    ! Central difference
    v = X(1) - dv
    p = pressure(t,v,Z,dpdv=dpdv)
    v = X(1) + dv
    p = pressure(t,v,Z,dpdv=dpdv1)
    d2pdv2 = (dpdv1-dpdv)/(2.0*dv)

    dF(1,1) = dpdvScaling*d2pdv2/1.0e6
  end subroutine stabJacSingleV

  !-------------------------------------------------------------------------
  !> Calculate maximum density according to the Peng-Robinson EoS. Equals the
  !> inverse covolume. PR is preferred over SRK since it gives higher max.
  !>
  !> There is no guarantee that this is the maximum for an EoS other than PR.
  !>
  !> \author Ailo 2016-01
  !-------------------------------------------------------------------------
  function rhomax_PR(x)
    use thermopack_var, only: nce, Rgas
    use eos, only: getCriticalParam
    ! Input:
    real :: x(nce)      !< Composition (needn't be normalized)
    ! Output:
    real :: rhomax_PR  !< Maximum density [mol/m^3]
    ! Local:
    integer :: i
    real :: t_crit, p_crit, b_pure(nce), omega

    do i=1,nce
      call getCriticalParam(i,t_crit,p_crit,omega) ! t_crit [K], p_crit [Pa]
      b_pure(i) = 0.077796*Rgas*t_crit/p_crit ! PR pure covolume [m^3/mol]
    enddo

    rhomax_PR = sum(x)/sum(b_pure*x) ! Inverse mixture covolume.
  end function rhomax_PR


  !-------------------------------------------------------------------------
  !> Computes the density at the first local pressure extremum for a general
  !> EoS. If no such extremum exists (i.e. a monotone rho-P curve), return a
  !> negative density.
  !>
  !> \todo: May need more sophisticated method of choosing initial liquid rho.
  !> \todo: May need more robust handling of overshoots. Now we just "hope".
  !> \todo: May need to check that we have converged to the *correct* extremum.
  !>
  !> \author Ailo 2016-01
  !-------------------------------------------------------------------------
  function rho_of_meta_extremum (T,x,phase,rho_init_in)
    use cubic_eos, only: cb_eos
    use thermopack_var, only: nce
    use thermopack_constants, only: LIQPH, VAPPH, verbose
    use numconstants, only: machine_prec
    ! Input:
    real, intent(in) :: T                          !< Temperature [K]
    real, intent(in) :: x(nce)                      !< Composition
    integer, intent(in) :: phase                   !< Phase flag; VAPPH or LIQPH
    real, intent(in), optional :: rho_init_in      !< Override initial rho if desired
    ! Output:
    real :: rho_of_meta_extremum !< [mol/m^3]
    ! Locals:
    real               :: rho_min, rho_max, rho_init, rho, rho_old, drho, p_rho, p_rhorho, &
         max_drho, s
    real, parameter    :: rel_eps=1e-8, rho_extrem_rel_tol=machine_prec*1e3, &
         dpdrho_extrem_abs_tol=machine_prec*1e8
    integer            :: n_iter
    integer, parameter :: max_iter_extr = 50

    ! Perform various initializations before the Newton iteration.

    n_iter = 0
    rho_min = 1e-6
    rho_max = rhomax_PR(x)*0.99
    max_drho = (rho_max-rho_min)/10.0 ! Upper limit on density steps

    select case(phase)
    case(LIQPH)
      rho_init = rho_max
      s = 1.0 ! Looking for minimum
    case(VAPPH)
      rho_init = rho_min
      s = -1.0 ! Looking for maximum
    case default
      call stoperror("rho_of_meta_extremum::phase is neither liquid nor vapor.")
    end select

    ! Override initial rho if desired.
    if ( present(rho_init_in) ) then
      rho_init = rho_init_in
    end if

    rho = rho_init
    p_rho = dpdrho(rho, x, T)
    p_rhorho = d2pdrho2(rho, x, T)

    ! Make an effort to ensure that we're starting from a good place.
    ! (Note to self: If p_rho < 0, then either we are too high or too low;
    ! therefore not allowed to go from neg. to pos. to neg. p_rho. Implement
    ! this later. The below test is at least good enough for vapor roots.)
    do while (p_rho < 0.0 .or. p_rhorho*s < 0.0)
      if ( verbose ) then
        print *, "Initial rho, p_rho, p_rhorho", rho, p_rho,p_rhorho
        print *,"rho_of_meta_extremum::trying to adjust initial rho"
        print *
      end if
      rho = rho*(1-s*0.1) ! increase or decrease by 10 percent
      p_rho = dpdrho(rho, x, T)
      p_rhorho = d2pdrho2(rho, x, T)
    end do

    ! Solve dpdrho=0 using Newton's method with numerical gradient.
    do
      if (.true.) then
        ! Newton step
        drho = -p_rho/p_rhorho
        ! Limit the step
        if ( verbose ) then
          print *, n_iter, rho, drho, p_rho, p_rhorho
        end if
        if (abs(drho) > max_drho) then
          drho = sign(max_drho,drho)
        endif
      else
        ! I have to consider the case where we overshoot. Perhaps bisect a
        ! couple of times and cycle the loop? But for now, keep it simple:
        rho_of_meta_extremum = -1.0
        return
      endif

      ! Updates.
      rho_old = rho
      rho = rho + drho
      p_rho = dpdrho(rho, x, T)
      p_rhorho = d2pdrho2(rho, x, T)

      ! Check convergence of extremum search
      n_iter = n_iter+1
      if (abs(drho/rho)<rho_extrem_rel_tol .and.  &
           abs(p_rho)<dpdrho_extrem_abs_tol) then ! .and. p_rhorho*s > 0.0) then
        ! Converged
        rho_of_meta_extremum = rho
        if ( verbose ) then
          print *, "Found meta_extremum"
          print *, "drho, p_rho,p_rhorho", drho, p_rho, p_rhorho
          print *
        end if
        exit
      elseif ((n_iter == max_iter_extr) .or.         &
           (phase==LIQPH .and. (rho < rho_min .or. p_rhorho < 0.0)) .or.  &
           (phase==VAPPH .and. (rho > rho_max .or. p_rhorho > 0.0))       &
           ) then
        ! Found no extremum
        rho_of_meta_extremum = -1.0
        exit
      endif
    end do

  end function rho_of_meta_extremum

  function dpdrho(rho, x, t)
    use thermopack_var, only: nce
    use eosTV, only: pressure
    real, intent(in) :: rho
    real, intent(in) :: x(nce)                      !< Composition
    real, intent(in) :: T                          !< Temperature [K]
    real :: dpdrho
    ! Locals
    real :: p
    p = pressure(t,1.0/rho,x,dpdv=dpdrho)
    dpdrho = -dpdrho/rho**2
  end function dpdrho

  function d2pdrho2(rho, x, T)
    use thermopack_var, only: nce
    real, intent(in) :: rho
    real, intent(in) :: x(nce)                      !< Composition
    real, intent(in) :: T                          !< Temperature [K]
    real :: d2pdrho2
    real :: p_rho_1, p_rho_2
    real, parameter    :: rel_eps=1e-8
    p_rho_1 = dpdrho(rho*(1-rel_eps), x, T)
    p_rho_2 = dpdrho(rho*(1+rel_eps), x, T)

    d2pdrho2 = (p_rho_2 - p_rho_1)/(2*rho*rel_eps)
  end function d2pdrho2

  function genericPropertyTV(t,v,n,propflag) result(prop)
    use eosTV, only: pressure, internal_energy_tv, enthalpy_tv, entropy_tv
    implicit none
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< mol - Mol numbers
    integer, intent(in) :: propflag !< Flag determining what property to return
    real :: prop !< Property

    prop = 0
    if (propflag == spin_locate_from_entropy) then
      call entropy_tv(t,v,n,prop)
    else if (propflag == spin_locate_from_volume) then
      prop = v
    else if (propflag == spin_locate_from_enthalpy) then
      call enthalpy_tv(t,v,n,prop)
    else if (propflag == spin_locate_from_temperature) then
      prop = T
    else if (propflag == spin_locate_from_pressure) then
      prop = pressure(t,v,n)
    else if (propflag == spin_locate_from_energy) then
      call internal_energy_tv(t,v,n,prop)
    end if

  end function genericPropertyTV

  !-----------------------------------------------------------------------------
  !> Bracket solver for finding the exact point on saturation line
  !>
  !> What property is is determined by propflag.
  !>
  !> \author MH 2020-01
  !-----------------------------------------------------------------------------
  subroutine bracketSolveForPropertySingleSpinodal(ic,propflag,propspec,&
       property_function,T1,v1,Ts,vs,ierr)
    implicit none
    integer, intent(in) :: ic
    real, intent(in) :: T1,v1
    real, intent(inout) :: Ts,vs
    integer, intent(in) :: propflag
    real, intent(in) :: propspec
    procedure(prop_fun_type) :: property_function
    integer, intent(out) :: ierr
    ! Locals
    real, dimension(7) :: param
    real :: z(nc)
    type(nonlinear_solver) :: solver
    real :: Xs,Xsmax,Xsmin,dvdt1
    !
    if (verbose) then
      print *,'In bracketSolveForPropertySingleSpinodal....'
    endif
    z = 0
    z(ic) = 1
    dvdT1 = dvdT_meta_line(T1,V1,z)
    param(1) = propspec
    param(2) = real(ic)
    param(3) = dvdT1
    param(4) = T1
    param(5) = v1
    param(6) = real(propflag)
    param(7) = 0
    solver%abs_tol = 1e-8
    solver%isolver = NS_PEGASUS
    Xsmax = max(vs,v1)
    Xsmin = min(vs,v1)
    Xs = v1
    call bracketing_solver(Xsmin,Xsmax,&
         property_Function,&
         Xs,solver,param)
    ierr = solver%exitflag
    vs = Xs
    Ts = param(7)
  end subroutine bracketSolveForPropertySingleSpinodal

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on saturation line having property value
  !> propspec.
  !>
  !> \author MH 2020-01
  !-----------------------------------------------------------------------------
  function propertyFunctionWrapperSingleSpinodal(Xs,param) result(fun)
    implicit none
    real, intent(in) :: Xs
    real, dimension(7), intent(inout) :: param
    real :: fun
    ! Locals:
    real, dimension(nc) :: n
    real :: t, prop, propspec, v, T0, v0, dvdT
    integer :: propflag, ic, ierr

    ! Unpack the param vector
    propspec = param(1)
    ic = nint(param(2))
    dvdT = param(3)
    T0 = param(4)
    v0 = param(5)
    propflag = nint(param(6))

    ! Composition
    n = 0
    n(ic) = 1

    ! Extrapolate for better initial values
    v = Xs
    T = T0 + (v-v0)/dvdT
    call StablimitPointSingleComp(T,n,v,ierr)
    ! Update t-slot in param vector with new t value
    param(7) = t

    ! Calculate the property value at the new spinodial point
    prop = genericPropertyTV(t,v,n,propflag)

    ! Compute new value of objective function
    fun = (prop - propspec)/max(abs(propspec), 1.0)
  end function propertyFunctionWrapperSingleSpinodal

  function genericPropertyTV_pressure_gradient(t,v,n,propflag) result(prop)
    use eosTV, only: pressure, internal_energy_tv, enthalpy_tv, entropy_tv
    use eos, only: getCriticalParam
    implicit none
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< mol - Mol numbers
    integer, intent(in) :: propflag !< Flag determining what property to return
    real :: prop !< Property
    ! Locals
    real :: p,dpdv,dpdt,x,dxdt,dxdv
    real :: dvdt_spin !< Differential along spinodal
    integer :: ic
    real :: t_crit,p_crit,omega

    if (propflag /= spin_locate_from_pressure) then
      dvdt_spin = dvdT_meta_line(t,v,n)
      p = pressure(t,v,n,dpdv,dpdt)
      dpdt = dpdt + dpdv*dvdt_spin
      ic = maxloc(n, dim=1)
      call getCriticalParam(ic,t_crit,p_crit,omega)
    endif
    prop = 0
    if (propflag == spin_locate_from_entropy) then
      call entropy_tv(t,v,n,x,dxdt,dxdv)
      dxdt = dxdt + dxdv*dvdt_spin
      prop = dxdt/dpdt
      prop = prop*1e4
    else if (propflag == spin_locate_from_volume) then
      prop = dvdt_spin/dpdt
    else if (propflag == spin_locate_from_enthalpy) then
      call enthalpy_tv(t,v,n,x,dxdt,dxdv)
      dxdt = dxdt + dxdv*dvdt_spin
      prop = dxdt/dpdt
      prop = prop*1e5/t_crit
    else if (propflag == spin_locate_from_temperature) then
      prop = 1.0/dpdt
    else if (propflag == spin_locate_from_pressure) then
      prop = 1
    else if (propflag == spin_locate_from_energy) then
      call internal_energy_tv(t,v,n,x,dxdt,dxdv)
      dxdt = dxdt + dxdv*dvdt_spin
      prop = dxdt/dpdt
      prop = prop*1e5/t_crit
   end if
  end function genericPropertyTV_pressure_gradient

  !-----------------------------------------------------------------------------
  !> Function used to solve for point on spinodal curve having zero property differential
  !> with respect to pressure.
  !>
  !> \author MH 2020-02
  !-----------------------------------------------------------------------------
  function propertyFunctionWrapperSingleSpinodalPressureGradient(Xs,param) result(fun)
    implicit none
    real, intent(in) :: Xs
    real, dimension(7), intent(inout) :: param
    real :: fun
    ! Locals:
    real, dimension(nc) :: n
    real :: t, propspec, v, T0, v0, dvdT
    integer :: propflag, ic, ierr

    ! Unpack the param vector
    propspec = param(1)
    ic = nint(param(2))
    dvdT = param(3)
    T0 = param(4)
    v0 = param(5)
    propflag = nint(param(6))

    ! Composition
    n = 0
    n(ic) = 1

    ! Extrapolate for better initial values
    v = Xs
    T = T0 + (v-v0)/dvdT
    call StablimitPointSingleComp(T,n,v,ierr)
    ! Update t-slot in param vector with new t value
    param(7) = t

    ! Calculate the property value differential to pressure
    fun = genericPropertyTV_pressure_gradient(t,v,n,propflag)
  end function propertyFunctionWrapperSingleSpinodalPressureGradient

  !-----------------------------------------------------------------------------
  !> Locate property - spinodal curve intersect
  !>
  !> \author MH, 2020-01
  !-----------------------------------------------------------------------------
  subroutine locate_spinodal_prop_pure_fluid(propflag,z,propspec,&
       phase,P0,Ts,vs,Ps,found_crossing,ierr,Tliq_start)
    use thermopack_constants, only: TWOPH
    implicit none
    integer, intent(in) :: propflag
    real, intent(in) :: z(nc)
    real, intent(in) :: P0
    real, intent(in) :: propspec
    integer, intent(in) :: phase
    real, intent(out) :: Ts,Ps,vs
    logical, intent(out) :: found_crossing
    integer, intent(out) :: ierr
    real, optional, intent(in) :: Tliq_start
    ! Locals
    real :: TaLiq(nMaxSingle), TaGas(nMaxSingle) !< Temperature [K]
    real :: VaLiq(nMaxSingle), VaGas(nMaxSingle) !< Specific volume [m3/mol]
    real :: PaLiq(nMaxSingle), PaGas(nMaxSingle) !< Pressure [Pa]
    real :: prop_a(nMaxSingle)
    real :: prop_1, prop_n, prop_i, Tx, vx
    integer :: ic, i, jm
    logical :: isMonotonous
    ic = maxloc(z, dim=1)

    ! Calculate entire spinodals
    ! Some room for speed-up........
    call singleCompStabilityLimit(P0,z,TaLiq,VaLiq,PaLiq,&
         TaGas,VaGas,PaGas,ierr,Tliq_start=Tliq_start)
    ierr = 0
    found_crossing = .false.
    Ts = 0
    if (phase == LIQPH .or. phase == TWOPH) then
      isMonotonous = .true.
      jm = 0
      do i=1,nMaxSingle
        prop_a(i) = genericPropertyTV(TaLiq(i),VaLiq(i),z,propflag)
        if (i > 2) then
          if ((prop_a(i)-prop_a(i-1))*(prop_a(i-1)-prop_a(i-2)) < 0) then
            isMonotonous = .false.
            jm = i-1
            ! Intentionally continue to nMaxSingle
          endif
        endif
      enddo
      if (isMonotonous) then
        prop_1 = prop_a(1)
        prop_n = prop_a(nMaxSingle)
        jm = 2
      else
        ierr = 0
        Tx = TaLiq(min(jm+1,nMaxSingle))
        vx = VaLiq(min(jm+1,nMaxSingle))
        call bracketSolveForPropertySingleSpinodal(ic,propflag,propspec, &
             propertyFunctionWrapperSingleSpinodalPressureGradient, &
             TaLiq(jm-1),VaLiq(jm-1),Tx,vx,ierr)
        prop_a(jm) = genericPropertyTV(Tx,Vx,z,propflag)
        prop_1 = prop_a(jm)
        prop_n = prop_a(nMaxSingle)
        jm = jm + 1
      endif
      if ((prop_1-propspec)*(prop_n-propspec) <= 0) then
        do i=jm,nMaxSingle
          prop_i = prop_a(i)
          if ((prop_1-propspec)*(prop_i-propspec) <= 0) then
            ierr = 0
            Ts = TaLiq(i)
            vs = VaLiq(i)
            call bracketSolveForPropertySingleSpinodal(ic,propflag,propspec, &
                 propertyFunctionWrapperSingleSpinodal, &
                 TaLiq(i-1),VaLiq(i-1),Ts,vs,ierr)
            found_crossing = .true.
            exit
          endif
          prop_1 = prop_i
        enddo
      endif
    endif
    if (phase == VAPPH .or. phase == TWOPH) then
      isMonotonous = .true.
      jm = 0
      do i=1,nMaxSingle
        prop_a(i) = genericPropertyTV(TaGas(i),VaGas(i),z,propflag)
        if (i > 2) then
          if ((prop_a(i)-prop_a(i-1))*(prop_a(i-1)-prop_a(i-2)) < 0) then
            isMonotonous = .false.
            jm = i-1
            ! Intentionally continue to nMaxSingle
          endif
        endif
      enddo
      if (isMonotonous) then
        prop_1 = prop_a(1)
        prop_n = prop_a(nMaxSingle)
        jm = 2
      else
        ierr = 0
        Tx = TaGas(min(jm+1,nMaxSingle))
        vx = VaGas(min(jm+1,nMaxSingle))
        call bracketSolveForPropertySingleSpinodal(ic,propflag,propspec, &
             propertyFunctionWrapperSingleSpinodalPressureGradient, &
             TaGas(jm-1),VaGas(jm-1),Tx,vx,ierr)
        prop_a(jm) = genericPropertyTV(Tx,Vx,z,propflag)
        prop_1 = prop_a(jm)
        prop_n = prop_a(nMaxSingle)
        jm = jm + 1
      endif
      if ((prop_1-propspec)*(prop_n-propspec) <= 0) then
        do i=jm,nMaxSingle
          prop_i = prop_a(i)
          if ((prop_1-propspec)*(prop_i-propspec) <= 0) then
            ierr = 0
            Ts = TaGas(i)
            vs = VaGas(i)
            call bracketSolveForPropertySingleSpinodal(ic,propflag,propspec, &
                 propertyFunctionWrapperSingleSpinodal, &
                 TaGas(i-1),VaGas(i-1),Ts,vs,ierr)
            found_crossing = .true.
            exit
          endif
          prop_1 = prop_i
        enddo
      endif
    endif
    if (found_crossing) then
      Ps = pressure(Ts,vs,z)
    else
      Ps = 0
    endif
  end subroutine locate_spinodal_prop_pure_fluid

  function genericPropertyTV_pressure_gradient_mc(t,v,n,propflag) result(prop)
    use eosTV, only: pressure, internal_energy_tv, enthalpy_tv, entropy_tv
    use eos, only: getCriticalParam
    use critical, only: calcCriticalTV
    implicit none
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< mol - Mol numbers
    integer, intent(in) :: propflag !< Flag determining what property to return
    real :: prop !< Property
    ! Locals
    real :: p,dpdv,dpdt,x,dxdt,dxdv
    real :: dvdt_spin !< Differential along spinodal
    integer :: ierr
    real :: t_crit,p_crit,v_crit

    if (propflag /= spin_locate_from_pressure) then
      dvdt_spin = dvdT_meta_line(t,v,n)
      p = pressure(t,v,n,dpdv,dpdt)
      dpdt = dpdt + dpdv*dvdt_spin
      if (propflag /= spin_locate_from_entropy) then
         ! Need an initial guess.. probably there are better guesses than this..
         t_crit = 300.
         v_crit = v ! need better guess!
         call calcCriticalTV(t_crit,v_crit,n,ierr, p=p_crit)
         if (ierr /= 0) then
            print*, "tc, vc, pc", t_crit, v_crit, p_crit
            call stoperror("spinodal::genericPropertyTV_pressure_gradient_mc:"&
                 "could not solve for critical point parameters.")
         end if
      end if
    endif
    prop = 0
    if (propflag == spin_locate_from_entropy) then
      call entropy_tv(t,v,n,x,dxdt,dxdv)
      dxdt = dxdt + dxdv*dvdt_spin
      prop = dxdt/dpdt
      prop = prop*1e4
    else if (propflag == spin_locate_from_volume) then
      prop = dvdt_spin/dpdt
    else if (propflag == spin_locate_from_enthalpy) then
      call enthalpy_tv(t,v,n,x,dxdt,dxdv)
      dxdt = dxdt + dxdv*dvdt_spin
      prop = dxdt/dpdt
      prop = prop*1e5/t_crit
    else if (propflag == spin_locate_from_temperature) then
      prop = 1.0/dpdt
    else if (propflag == spin_locate_from_pressure) then
      prop = 1
    else if (propflag == spin_locate_from_energy) then
      call internal_energy_tv(t,v,n,x,dxdt,dxdv)
      dxdt = dxdt + dxdv*dvdt_spin
      prop = dxdt/dpdt
      prop = prop*1e5/t_crit
   end if
 end function genericPropertyTV_pressure_gradient_mc

   !-----------------------------------------------------------------------------
  !> Locate extrema on spinodal curve
  !>
  !> \author MH, 2021-02
  !-----------------------------------------------------------------------------
  subroutine locate_spinodal_prop_min_max_pure_fluid(propflag,z,&
       P0,Ts_min,vs_min,Ps_min,Ts_max,vs_max,Ps_max,ierr)
    implicit none
    integer, intent(in) :: propflag
    real, intent(in) :: z(nc)
    real, intent(in) :: P0
    real, intent(out) :: Ts_min,vs_min,Ps_min,Ts_max,vs_max,Ps_max
    integer, intent(out) :: ierr
    ! Locals
    real :: TaLiq(nMaxSingle), TaGas(nMaxSingle) !< Temperature [K]
    real :: VaLiq(nMaxSingle), VaGas(nMaxSingle) !< Specific volume [m3/mol]
    real :: PaLiq(nMaxSingle), PaGas(nMaxSingle) !< Pressure [Pa]
    real :: prop_a(nMaxSingle)
    real :: Tx, vx
    integer :: ic, i, jm
    logical :: isMonotonous
    real :: extrema_liq(3)
    ic = maxloc(z, dim=1)

    ! Calculate entire spinodals
    ! Some room for speed-up........
    call singleCompStabilityLimit(P0,z,TaLiq,VaLiq,PaLiq,&
         TaGas,VaGas,PaGas,ierr)

    ierr = 0
    isMonotonous = .true.
    jm = 1
    do i=1,nMaxSingle
      prop_a(i) = genericPropertyTV(TaLiq(i),VaLiq(i),z,propflag)
      if (i > 2) then
        if ((prop_a(i)-prop_a(i-1))*(prop_a(i-1)-prop_a(i-2)) < 0) then
          isMonotonous = .false.
          jm = i-1
          ! Intentionally continue to nMaxSingle
        endif
      endif
    enddo
    if (.not. isMonotonous) then
      ierr = 0
      Tx = TaLiq(min(jm+1,nMaxSingle))
      vx = VaLiq(min(jm+1,nMaxSingle))
      call bracketSolveForPropertySingleSpinodal(ic,propflag,0.0, &
           propertyFunctionWrapperSingleSpinodalPressureGradient, &
           TaLiq(jm-1),VaLiq(jm-1),Tx,vx,ierr)
      if (ierr /= 0) return
      prop_a(jm) = genericPropertyTV(Tx,Vx,z,propflag)
      TaLiq(jm) = Tx
      VaLiq(jm) = vx
    endif
    extrema_liq = (/TaLiq(jm), VaLiq(jm), prop_a(jm)/)

    isMonotonous = .true.
    jm = 1
    do i=1,nMaxSingle
      prop_a(i) = genericPropertyTV(TaGas(i),VaGas(i),z,propflag)
      if (i > 2) then
        if ((prop_a(i)-prop_a(i-1))*(prop_a(i-1)-prop_a(i-2)) < 0) then
          isMonotonous = .false.
          jm = i-1
          ! Intentionally continue to nMaxSingle
        endif
      endif
    enddo
    if (.not. isMonotonous) then
      ierr = 0
      Tx = TaGas(min(jm+1,nMaxSingle))
      vx = VaGas(min(jm+1,nMaxSingle))
      call bracketSolveForPropertySingleSpinodal(ic,propflag,0.0, &
           propertyFunctionWrapperSingleSpinodalPressureGradient, &
           TaGas(jm-1),VaGas(jm-1),Tx,vx,ierr)
      if (ierr /= 0) return
      prop_a(jm) = genericPropertyTV(Tx,Vx,z,propflag)
      TaGas(jm) = Tx
      VaGas(jm) = vx
    endif

    if (prop_a(jm) > extrema_liq(3)) then
      Ts_min = extrema_liq(1)
      vs_min = extrema_liq(2)
      Ts_max = TaGas(jm)
      vs_max = VaGas(jm)
    else
      Ts_max = extrema_liq(1)
      vs_max = extrema_liq(2)
      Ts_min = TaGas(jm)
      vs_min = VaGas(jm)
    endif
    Ps_min = pressure(Ts_max,vs_min,z)
    Ps_max = pressure(Ts_max,vs_max,z)
  end subroutine locate_spinodal_prop_min_max_pure_fluid

  !-----------------------------------------------------------------------------
  !> Solve for stable or meta-stable state given entropy and pressure
  !>
  !> \author MH, 2021-02
  !-----------------------------------------------------------------------------
  subroutine tv_meta_ps(p,s,n,T,v,ierr)
    implicit none
    real, intent(in) :: n(nc)
    real, intent(in) :: P
    real, intent(in) :: s
    real, intent(inout) :: T,v
    integer, intent(out) :: ierr
    ! Locals
    real, dimension(nc+2) :: param !< Parameter vector
    type(nonlinear_solver) :: solver
    real, dimension(2) :: x,xmin,xmax
    param(1:nc) = n
    param(nc+1) = s
    param(nc+2) = p
    !
    x(1) = log(T)
    x(2) = log(v)

    solver%abs_tol = 1.0e-10
    ! MH: Need to improve limits for robustness.......
    xmin = (/log(T-100.0), log(v/10.0)/)
    xmax = (/log(T+100.0), log(v*10.0)/)
    call nonlinear_solve(solver,ps_fun_meta,ps_jac_meta,ps_jac_meta,&
         limit_dx,premReturn,setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    T = exp(x(1))
    v = exp(x(2))
  end subroutine tv_meta_ps

  !--------------------------------------------------------------------------
  !> Pressure and entropy error function in variables v and T
  !> \author MH, 2021-02
  !--------------------------------------------------------------------------
  subroutine ps_fun_meta(f,x,param)
    use eosTV, only: pressure, entropy_tv
    implicit none
    real, dimension(2), intent(in) :: x !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    real, dimension(2), intent(out) :: f !< Function value
    ! Locals
    real, dimension(nc) :: n !< Overall compozition
    real :: ss, ps, T, v, s, p
    n = param(1:nc)
    ss = param(nc+1)
    ps = param(nc+2)
    !
    T = exp(x(1))
    v = exp(x(2))
    !
    p = pressure(T,v,n)
    call entropy_tv(T,v,n,s)
    !
    f(1) = (p - ps)/max(abs(ps),1.0e5)
    f(2) = (s - ss)/max(abs(ss),1.0)
  end subroutine ps_fun_meta

  !--------------------------------------------------------------------------
  !> Pressure and entropy error jacobian in variables v and T
  !> \author MH, 2021-02
  !--------------------------------------------------------------------------
  subroutine ps_jac_meta(df,x,param)
    use eosTV, only: pressure, entropy_tv
    implicit none
    real, dimension(2), intent(in) :: x !< Variable vector
    real, dimension(nc+2), intent(in) :: param !< Parameter vector
    real, dimension(2,2), intent(out) :: df !< Jacobian
    ! Locals
    real, dimension(nc) :: n !< Overall compozition
    real :: ss, ps, T, v, s, p, dpdv, dpdT, dsdv, dsdT
    n = param(1:nc)
    ss = param(nc+1)
    ps = param(nc+2)
    !
    T = exp(x(1))
    v = exp(x(2))
    !
    p = pressure(T,v,n,dpdv=dpdv,dpdT=dpdT)
    call entropy_tv(T,v,n,s,dsdT=dsdT,dsdv=dsdv)
    !
    df(1,1) = T*dpdT/max(abs(ps),1.0e5)
    df(2,1) = T*dsdT/max(abs(ss),1.0)
    df(1,2) = v*dpdv/max(abs(ps),1.0e5)
    df(2,2) = v*dsdv/max(abs(ss),1.0)
  end subroutine ps_jac_meta

  !--------------------------------------------------------------------------
  !> Pressure and entropy error jacobian in variables v and T
  !> \author MH, 2021-02
  !--------------------------------------------------------------------------
  subroutine meta_isentrope_extrapolate_pressure(n,T_old,v_old,p_old,dp,T,v,p)
    implicit none
    real, dimension(nc), intent(in) :: n !< Overall compozition
    real, intent(in) :: T_old,v_old,p_old,dp !<
    real, intent(out) :: T,v,p !<
    ! Locals
    real, dimension(nc+2) :: param !< Parameter vector
    real, dimension(2,2) :: df !< Jacobian
    real, dimension(2) :: x, rhs
    integer, dimension(2) :: ipiv
    integer :: ifail
    logical :: lufail
    param(1:nc) = n
    param(nc+1) = 1.0
    param(nc+2) = p_old
    !
    x(1) = log(T_old)
    x(2) = log(v_old)
    call ps_jac_meta(df,x,param)
    rhs = 0
    rhs(1) = 1.0/max(abs(p_old),1.0e5)
    ! general lu
    lufail = .false.
    call dgetrf(2,2,df,2,ipiv,ifail)
    if (ifail.ne.0) then
      lufail = .true.
    endif
    ! backsubstitute
    if (.not. lufail) then
      call dgetrs('n',2,1,df,2,ipiv,rhs,2,ifail)
      if (ifail.ne.0) then
        lufail = .true.
      endif
    endif

    if (lufail .or. rhs(1) /= rhs(1)) then
      rhs = 0
    endif
    x = x + rhs*dp
    T = exp(x(1))
    v = exp(x(2))
    p = p_old + dp
  end subroutine meta_isentrope_extrapolate_pressure

  !-----------------------------------------------------------------------------
  !> Map single phase isentrope in meta-stable region
  !>
  !> \author MH, 2021-02
  !-----------------------------------------------------------------------------
  subroutine map_meta_isentrope(p0,s,n,pmin,nmax,T,v,p,ierr)
    use ps_solver, only: twoPhasePSflash
    use thermopack_constants, only: TWOPH
    implicit none
    real, intent(in) :: n(nc)
    real, intent(in) :: p0, pmin
    real, intent(in) :: s
    integer, intent(in) :: nmax
    real, intent(out) :: T(nmax), v(nmax), p(nmax)
    integer, intent(out) :: ierr
    ! Locals
    integer :: i, phase
    real :: Ts, Ps, vs, dp
    real :: beta,betaL,X(nc),Y(nc)
    logical :: found_crossing

    call locate_spinodal_prop_pure_fluid(spin_locate_from_entropy,&
         n,s,TWOPH,pmin,Ts,vs,Ps,found_crossing,ierr)
    if (found_crossing) then
      p(nmax) = Ps
      T(nmax) = Ts
      v(nmax) = vs
    else
      Ps = pmin
    endif
    call twoPhasePSflash(t(1),p0,n,beta,betaL,X,Y,s,phase)
    if (phase == TWOPH) then
      if (betaL > beta) then
        phase = LIQPH
      else
        phase = VAPPH
      endif
    endif
    if (phase == SINGLEPH) phase = LIQPH
    call specificvolume(t(1),p0,n,phase,v(1))
    dp = (Ps - p0)/(nmax - 1)
    p(1) = p0
    do i=2,nmax
      if (i==nmax .and. Ps /= pmin) exit
      call meta_isentrope_extrapolate_pressure(n,T(i-1),v(i-1),p(i-1),dp,T(i),v(i),p(i))
      call tv_meta_ps(p(i),s,n,T(i),v(i),ierr)
    enddo
  end subroutine map_meta_isentrope

  subroutine solve_for_spinodal_entropy(z,s0,T_old,v_old,p_old,T,v,p,ierr)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver, &
         NS_PEGASUS
    real, intent(in) :: z(nc) !<
    real, intent(in) :: s0 !<
    real, intent(in) :: T_old,v_old,p_old !<
    real, intent(inout) :: T, v, p !<
    integer, intent(out) :: ierr
    ! Locals
    real :: p_min, p1, param(nc+4), dp
    type(nonlinear_solver) :: solver
    param(1:nc) = z
    param(nc+1) = T_old
    param(nc+2) = v_old
    param(nc+3) = p_old
    param(nc+4) = s0
    solver%abs_tol = 1.0e-7
    solver%isolver = NS_PEGASUS
    p_min = p
    call bracketing_solver(p_min,p_old,spinodal_entropy,p,solver,param)
    ierr = solver%exitflag
    if (ierr /= 0) then
      ierr = 23
      return
    else
      dp = p - p_old
      call meta_isentrope_extrapolate_pressure(z,T_old,v_old,p_old,dp,T,v,p1)
      call tv_meta_ps(p,s0,z,T,v,ierr)
    endif
  end subroutine solve_for_spinodal_entropy

  !--------------------------------------------------------------------------
  !> Critical isentrope spinodal crossing
  !> \author Morten Hammer, 2021-08
  !--------------------------------------------------------------------------
  function spinodal_entropy(p,param) result(f)
    use critical, only: calcStabMinEigTV
    implicit none
    real, intent(in) :: p !< Pressure [Pa]
    real, dimension(nc+4), intent(in) :: param !< Parameter vector
    real :: f
    ! Local
    real, dimension(nc) :: Z !< Overall compozition
    real :: s0, v0, T0, P0, dp, T, v, p1
    integer :: ierr
    z = param(1:nc)
    T0 = param(nc+1)
    v0 = param(nc+2)
    p0 = param(nc+3)
    s0 = param(nc+4)
    dp = p - p0
    call meta_isentrope_extrapolate_pressure(z,T0,v0,p0,dp,T,v,p1)
    call tv_meta_ps(p,s0,z,T,v,ierr)
    f = calcStabMinEigTV(t,v,z)
  end function spinodal_entropy

end module spinodal
