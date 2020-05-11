integer function TestPrecision()
! AA: 2013-04-17:  Check double Presision
! To secure that the library is compiled with the same length of double presision
  double precision:: x
  double precision :: Eps
  integer :: ilog

  x = 1.0
  eps = epsilon(x)
  ilog = - int(log(eps) / log(2.0D0))
  TestPrecision = ilog
end function TestPrecision

subroutine trend_init_no_char(nc,int_path,npath,int_comps,ncomps,mix)
  ! MH, 2014-02
  !----------------------------------------------------------------------
  implicit none
  ! Input:
  integer, intent(in) :: nc
  integer, intent(in) :: mix
  integer, intent(in) :: npath
  integer, intent(in) :: ncomps
  integer, intent(in) :: int_path(npath)
  integer, intent(in) :: int_comps(ncomps)
  ! Internal
end subroutine trend_init_no_char

subroutine trend_init(nc,path,comps,mix)
  ! MH, 2013-04-04, EA, 2014-02
  !----------------------------------------------------------------------
  implicit none
  ! Input:
  integer, intent(in)               :: nc
  character (12), intent(in)        :: comps(nc)
  character(len=255), intent(inout) :: path
  integer, intent(in)               :: mix
end subroutine trend_init



  !----------------------------------------------------------------------
  !> Calculate fugasity coefficient and differentials given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_thermo(t,p,z,phase,lnfug,lnfugt,lnfugp,nlnfugn)
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(:), intent(in) :: z !< Compozition
    real, dimension(:), intent(out) :: lnfug !< Logarithm of fugasity coefficient
    real, optional, dimension(:), intent(out) :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
    real, optional, dimension(:), intent(out) :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
    real, optional, dimension(:,:), intent(out) :: nlnfugn !< Logarithm of fugasity coefficient differential wrpt. mole numbers, times n.
    ! Locals
    lnfug = 0.0
    lnfugt = 0.0
    lnfugp = 0.0
    nlnfugn = 0.0
  end subroutine trend_thermo

  !----------------------------------------------------------------------
  !> Calculate single phase specific enthalpy given composition, temperature and density
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_enthalpy(t,rho,x,h,dhdt,dhdp,dhdx)
    implicit none
    ! Transferred variables
    real, intent(in) :: rho !< mol/m3 - Density
    real, intent(in) :: t !< K - Temperature
    real, dimension(:), intent(in) :: x !< Compozition
    real, intent(out) :: h !< J/mol - Specific enthalpy
    real, optional, intent(out) :: dhdt !< J/mol/K - Specific enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdp !< J/mol/K/Pa - Specific enthalpy differential wrpt. pressure
    real, optional, dimension(:), intent(out) :: dhdx !< J/mol - Specific enthalpy differential wrpt. mole numbers
    h = 0.0
    dhdt = 0.0
    dhdp = 0.0
    dhdx = 0.0
  end subroutine trend_enthalpy

  !----------------------------------------------------------------------
  !> Calculate fugasity coefficient and differentials given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_thermo_dens(t,rho,z,lnfug,lnfugt,lnfugp,nlnfugn)
    implicit none
    ! Transferred variables
    real, intent(in) :: rho !< mol/m3 - Density
    real, intent(in) :: t !< K - Temperature
    real, dimension(:), intent(in) :: z !< Compozition
    real, dimension(:), intent(out) :: lnfug !< Logarithm of fugasity coefficient
    real, optional, dimension(:), intent(out) :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
    real, optional, dimension(:), intent(out) :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
    real, optional, dimension(:,:), intent(out) :: nlnfugn !< Logarithm of fugasity coefficient differential wrpt. mole numbers, times n.
    lnfug = 0.0
    lnfugt = 0.0
    lnfugp = 0.0
    nlnfugn = 0.0
  end subroutine trend_thermo_dens

  !----------------------------------------------------------------------
  !> Calculate single phase specific entropy given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_entropy(t,rho,x,s,dsdt,dsdp,dsdx)
    implicit none
    ! Transferred variables
    real, intent(in) :: rho !< mol/m3 - Density
    real, intent(in) :: t !< K - Temperature
    real, dimension(:), intent(in) :: x !< Compozition
    real, intent(out) :: s !< J/mol/K - Specific enthalpy
    real, optional, intent(out) :: dsdt !< J/mol/K2 - Specific enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dsdp !< J/mol/K2/Pa - Specific enthalpy differential wrpt. pressure
    real, optional, dimension(:), intent(out) :: dsdx !< J/mol 2/K - Specific enthalpy differential wrpt. mole numbers
    s = 0.0
    dsdt = 0.0
    dsdp = 0.0
    dsdx = 0.0
  end subroutine trend_entropy

  !----------------------------------------------------------------------
  !> Calculate pressure given composition, temperature and density.
  !>
  !> \author Ms, 2013-04-08
  !----------------------------------------------------------------------
  function trend_pressure(x,t,v,dpdv,dpdt) result(p)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3/mol - Specific volume
    real, dimension(:), intent(in) :: x !< Compozition
    real, optional, intent(out) :: dpdt !< Pa/K - Pressure differential wrpt. temperature
    real, optional, intent(out) :: dpdv !< Pa mol/m3 - Pressure differential wrpt. specific volume
    real :: p !< Pa - Pressure
    ! Locals
    p = 0.0
  end function trend_pressure

  !----------------------------------------------------------------------
  !> Calculate pressure differential vrpt. density given composition, temperature and density.
  !>
  !> \author MH, 2013-04-18
  !----------------------------------------------------------------------
  function trend_dpdrho(x,t,rho) result(dpdrho)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: rho !< mol/m3 - Density
    real, dimension(:), intent(in) :: x !< Composition
    real :: dpdrho
    ! Locals
    !--------------------------------------------------------------------
    dpdrho = 0.0
  end function trend_dpdrho

  !----------------------------------------------------------------------
  !> Calculate the second derivative of pressure vrpt. density given composition, temperature and density.
  !>
  !> \author EA, 2013-08-08
  !----------------------------------------------------------------------
  function trend_d2Pdrho2(x,T,rho) result(d2Pdrho2)
    implicit none
    ! Transferred variables
    real, intent(in)                :: T !< K - Temperature
    real, intent(in)                :: rho !< mol/m3 - Density
    real, dimension(:), intent(in)  :: x !< Composition
    real :: d2Pdrho2
    ! 
    ! Locals
    !--------------------------------------------------------------------
    d2Pdrho2 = 0.0
  end function trend_d2Pdrho2


  !----------------------------------------------------------------------
  !> Calculate SRK density given composition, temperature and pressure.
  !> To be used as initial guess
  !>
  !> \author MH, 2013-04-11
  !----------------------------------------------------------------------
  function trend_psrk(t,p,x,phase) result(rho)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(:), intent(in) :: x !< Compozition
    integer, intent(in) :: phase !< Phase indicator
    real :: rho !< mol/m3 - Pressure
    ! Locals

    rho = 0.0
  end function trend_psrk

  !-----------------------------------------------------------------------------
  !> Calculate the maximum molar density according to the SRK EoS and the
  !> classic mixing rule
  !>
  !> \author MAG, 2013-08-19
  !-----------------------------------------------------------------------------
  subroutine trend_rhomax_srk(x,rhomax)
    implicit none
    !---------------------------------------------------------------------------
    real, dimension(:), intent(in)  :: x      !< Composition
    real,                   intent(out) :: rhomax !< Maximum molar density
    !---------------------------------------------------------------------------
    ! Locals
    rhomax=0.0
    !
  end subroutine trend_rhomax_srk

  !----------------------------------------------------------------------
  !> Calculate single phase specific volume given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_specificVolume(t,x,v,dvdt,dvdp,dvdx)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, dimension(:), intent(in) :: x !< Compozition
    real, intent(in) :: v !< mol/m3 - Specific volume
    real, optional, intent(out) :: dvdt !< m3/mol/K - Specific volume differential wrpt. temperature
    real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specific volume differential wrpt. pressure
    real, optional, dimension(:), intent(out) :: dvdx !< m3/mol - Specific volume differential wrpt. mole numbers
    ! Locals
    !
    !--------------------------------------------------------------------
  end subroutine trend_specificVolume

  !----------------------------------------------------------------------
  !> Get component mole weight.
  !> Unit: g/mol
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  function trend_compMoleWeight(j) result(mw)
    implicit none
    ! Transferred variables
    integer, intent(in) :: j !< Component index
    real :: mw !< g/mol - Mole weight
    !--------------------------------------------------------------------
    !
    mw = 0.0
  end function trend_compMoleWeight

  !----------------------------------------------------------------------
  !> Get mole weight.
  !> Unit: g/mol
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  function trend_moleWeight(z) result(mw)
    implicit none
    ! Transferred variables
    real, dimension(:), intent(in) :: z !< Composition
    real :: mw !< g/mol - Mole weight
    !--------------------------------------------------------------------
    !
    mw = 0.0
  end function trend_moleWeight

  !----------------------------------------------------------------------
  !> Get critical parameters
  !>
  !> \author MH, 2013-04-10
  !----------------------------------------------------------------------
  subroutine trend_getcrit(i,tci,pci,oi,vci,tnbi)
    implicit none
    ! Transferred variables
    integer, intent(in) :: i !< Component index
    real, intent(out) :: tci !< K - Critical temperature
    real, intent(out) :: pci !< Pa - Critical pressure
    real, intent(out) :: oi  !< Asentric factor
    real, optional, intent(out) :: vci !< Critical specific volume (m3/mol)
    real, optional, intent(out) :: tnbi !< Normal boiling point
    !--------------------------------------------------------------------
    !
    tci = 0.0
    pci = 0.0
    oi  = 0.0
    if (present(vci)) then
       vci = 0.0
    endif
    if (present(tnbi)) then
       tnbi = 0.0
    endif
  end subroutine trend_getcrit

  !----------------------------------------------------------------------
  !> Get the Z-factor (compressibility factor),
  !> given temperature, density and composition.
  !>
  !> \author EA, 2014-04
  !----------------------------------------------------------------------
  subroutine trend_zfac(T, rho, x, zfac, dzdt, dzdp, dzdx)
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(:) !< Composition (mol/mol)
    ! Output:
    real,   intent(out)         :: zfac !< Compressibility factor (-)
    real, optional, intent(out) :: dzdt !< zfac diff. wrt. temperature
    real, optional, intent(out) :: dzdp !< zfac diff. wrt. pressure
    real, optional, intent(out) :: dzdx(:) !< zfac diff. wrt. composition
    ! TREND functions:
    zfac = 0.0
    if (present(dzdt)) then
      dzdt = 0.0
    endif
    if (present(dzdp)) then
      dzdp = 0.0
    endif
    if (present(dzdx)) then
      dzdx = 0.0
    endif
  end subroutine trend_zfac

  !----------------------------------------------------------------------
  !> Get the residual Gibbs energy,
  !> given temperature, density and composition.
  !>
  !> \author EA, 2014-04
  !----------------------------------------------------------------------
  subroutine trend_residualgibbs_tp(T, p, rho, x, gr, dgrdt, dgrdp)
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: p        !< Pressure (Pa)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(:)  !< Composition (mol/mol)
    ! Output:
    real,   intent(out)         :: gr     !< Residual Gibbs Energy (J/mol)
    real, optional, intent(out) :: dgrdt  !< Res. Gibbs diff. wrt. temperature
    real, optional, intent(out) :: dgrdp  !< Res. Gibbs diff. wrt. pressure
    gr = 0.0
    if (present(dgrdt)) then
      dgrdt = 0.0
    endif
    if (present(dgrdp)) then
      dgrdp = 0.0
    endif
  end subroutine trend_residualgibbs_tp

  !----------------------------------------------------------------------
  !> Get the residual enthalpy,
  !> given temperature, density and composition.
  !>
  !> \author EA, 2014-04
  !----------------------------------------------------------------------
  subroutine trend_residualenthalpy_tp(T, p, rho, x, hr, dhrdt, dhrdp, dhrdx)
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: p        !< Pressure (Pa)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(:)  !< Composition (mol/mol)
    ! Output:
    real,   intent(out)         :: hr     !< Residual enthalpy (J/mol)
    real, optional, intent(out) :: dhrdt  !< Res. enth. diff. wrt. temperature
    real, optional, intent(out) :: dhrdp  !< Res. enth. diff. wrt. pressure
    real, optional, intent(out) :: dhrdx(:)!< Res. enth. diff. wrt. comp
    hr = 0.0
    if (present(dhrdt)) then
      dhrdt = 0.0
    endif
    if (present(dhrdp)) then
      dhrdp = 0.0
    endif
    if (present(dhrdx)) then
      dhrdx = 0.0
    endif

  end subroutine trend_residualenthalpy_tp

  !----------------------------------------------------------------------
  !> Calculate single phase residual specific entropy given composition,
  ! temperature and pressure
  !>
  !> \author EA, 2014-05
  !----------------------------------------------------------------------
  subroutine trend_residualentropy_tp(T,p,rho,x,sr,dsrdt,dsrdp,dsrdx)
    implicit none
    ! Input:
    real, intent(in)                  :: T    !< Temperature (K)
    real, intent(in)                  :: p    !< Pressure (Pa)
    real, intent(in)                  :: rho  !< Density (mol/m3)
    real, dimension(:), intent(in) :: x    !< Composition (mol/mol)
    ! Output:
    real, intent(out)                 :: sr   !< Specific residual entropy
                                              ! (J/(mol*K)
    real, optional, intent(out)       :: dsrdt!< Specific residual entropy diff.
                                              ! wrpt. temperature (J/(mol*K^2)
    real, optional, intent(out)       :: dsrdp!< Specific residual entropy diff.
                                              ! wrpt. pressure (J/(mol*K*Pa)
    real, optional, intent(out)       :: dsrdx(:) !< Specific residual
                                              !entropy diff. wrpt. mole numbers
                                              ! (J/(mol^2*K)
    sr = 0.0
    if (present(dsrdt)) then
      dsrdt = 0.0
    endif
    if (present(dsrdp)) then
      dsrdp = 0.0
    endif
    if (present(dsrdx)) then
      dsrdx = 0.0
    endif

  end subroutine trend_residualentropy_tp


  !----------------------------------------------------------------------
  !> Get the speed of sound of a single phase of composition z
  !> at the given temperature and density.
  !>
  !> \author EA, 2014-05
  !----------------------------------------------------------------------
  function trend_speedofsound(T,rho,x) result(c)
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(:)  !< Composition (mol/mol)
    ! Output:
    real                        :: c        !< Speed of sound (m/s)

    c = 0.0

  end function trend_speedofsound

  !----------------------------------------------------------------------
  !> Get the component ideal gas heat capacity
  !! at the given temperature and density.
  !!
  !! \author MH, 2014-09
  !----------------------------------------------------------------------
  function trend_ideal_Cp(T,j) result(Cp_id)
    implicit none
    ! Input:
    real,    intent(in)         :: T        !< Temperature (K)
    integer, intent(in)         :: j        !< Component index
    ! Output:
    real                        :: Cp_id    !< Ideal heat capacity (J/mol/K)
    ! Locals
    Cp_id = 0.0
  end function trend_ideal_Cp

  !----------------------------------------------------------------------
  !> Get the component ideal gas enthalpy
  !! at the given temperature and density.
  !!
  !! \author MH, 2014-09
  !----------------------------------------------------------------------
  function trend_ideal_enthalpy(T,j) result(hid)
    implicit none
    ! Input:
    real,    intent(in)         :: T        !< Temperature (K)
    integer, intent(in)         :: j        !< Component index
    ! Output:
    real                        :: hid      !< Ideal enthalpy (J/mol)
    ! Locals
    hid = 0.0
  end function trend_ideal_enthalpy

  !----------------------------------------------------------------------
  !> Get the component ideal gas entropy
  !! at the given temperature and density.
  !!
  !! \author MH, 2014-09
  !----------------------------------------------------------------------
  function trend_ideal_entropy(T,P,j) result(sid)
    implicit none
    ! Input:
    real,    intent(in)         :: T        !< Temperature (K)
    real,    intent(in)         :: P        !< Density (Pa)
    integer, intent(in)         :: j        !< Component index
    ! Output:
    real                        :: sid      !< Ideal entropy (J/mol/K)
    ! Locals
    sid = 0.0
  end function trend_ideal_entropy

  !------------------------------------------------------------------
  !>Get current binary parameters used in trend
  !! \author AA: 2014-09
  ! See document ?? for descripotion
  !Return ParREdFunc = (/Beta_T, Gamma_T, Beta_V, Gamma_V, F_ij/)
  !and    ParDefFun(i) = (/n, t, d, l, 0, 0, 0, 0/) for i=1..ndf(1)
  !       PArDefFun(i) = (/n, t, d, l, Eta, Epsilon, Beta, Gamma/) for i=ndf(1)+1,...,ndf(2)
  !--------------------------------------------------
  subroutine trend_Get_binary_parameters(i1,i2, ParRedFun, ndf, ParDepFun)
    implicit none
    integer, parameter:: nDim = 25  !Dimensions of array in module_mixture_parameters
    integer(kind=4), intent(in) :: i1,i2
    real, intent(out):: ParRedFun(5)
    integer(kind=4), intent(out) :: ndf(2)
    real, intent(out)::ParDepFun(nDim,8)
    ParRedFun = 0.0
    ParDepFun = 0.0
    ndf = 0
  end subroutine trend_Get_binary_parameters

  !------------------------------------------------------------------
  !>Set current binary parameters used in trend
  !! \author AA: 2014-09
  ! See document ?? for descripotion
  !Set    (/Beta_T, Gamma_T, Beta_V, Gamma_V, F_ij/) = PArRedFunc
  !and    (/n, t, d, l, 0, 0, 0, 0/)=ParDepFun(i) for i=1..ndf(1)
  !       (/n, t, d, l, Eta, Epsilon, Beta, Gamma/)=ParDepFun(i) for i=ndf(1)+1,...,ndf(1)+ndf(2)
  !       but only if ndf(1) or ndf(2) > 0
  ! MArk that
  !--------------------------------------------------
  subroutine trend_Set_binary_parameters(i1i,i2i, ParRedFun, ndf, ParDepFun)
    ! Ir read this parameters by file in read_mix_file
    implicit none
    integer, parameter:: nDim = 25  !Dimensions of array in module_mixture_parameters
    integer, intent(in) :: i1i,i2i
    real, intent(in):: ParRedFun(5)
    integer, intent(in) :: ndf(2)
    real, intent(in)::ParDepFun(nDim,8)

  end subroutine trend_Set_binary_parameters

  !------------------------------------------------------------------
  !>Get reduced density and temperature
  !! \author AA: 2014-09
  !--------------------------------------------------
  subroutine trend_GetReducedRhoT(T, x, RhoRed, TRed)
    implicit none
    real, intent(in) :: T
    real, intent(in) :: x(30)
    real, intent(out):: RhoRed
    real, intent(out)::TRed

    RhoRed = 0.0
    TRed = 0.0
  end subroutine trend_GetREducedRhoT
!-------------------------------------------------------------------------
!Get Evaporating pressure for single phase
 function trend_GetPVapPure(T, icmp) result(P)
   implicit none
   real, intent(in) :: T
   integer, intent(in):: icmp
   real :: P
   P = 0.0
 end function trend_GetPVapPure

  !----------------------------------------------------------------------
  !> Calculate Helmholtz free energy given composition,
  !! temperature and specific volume.
  !>
  !> \author MH, 2015-02
  !----------------------------------------------------------------------
  function trend_free_energy(x,t,v,dydv,dydt,d2ydt2,d2ydv2,d2ydvdt) result(y)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3/mol - Specific volume
    real, dimension(:), intent(in) :: x !< Compozition
    real, optional, intent(out) :: dydt !< J/(mol K) - Helmholtz differential wrpt. temperature
    real, optional, intent(out) :: dydv !< J/(m3) - Helmholtz differential wrpt. specific volume
    real, optional, intent(out) :: d2ydt2 !< J/(mol K2) - Helmholtz differential wrpt. temperature
    real, optional, intent(out) :: d2ydv2 !< J/m6 - Helmholtz second differential wrpt. specific volume
    real, optional, intent(out) :: d2ydvdt !< J/(m3 K) - Helmholtz second differential wrpt. specific volume and temperature
    real :: y !< J/mol - Helmholtz free energy
    ! Locals

    y = 0.0
    if (present(dydv)) then
      dydv = 0.0
    endif
    if (present(d2ydv2)) then
      d2ydv2 = 0.0
    endif
    if (present(dydt)) then
      dydt = 0.0
    endif
    if (present(d2ydt2)) then
      d2ydt2 = 0.0
    endif
    if (present(d2ydvdt)) then
      d2ydvdt = 0.0
    endif

  end function trend_free_energy

  !----------------------------------------------------------------------
  !> Calculate internal energy given composition,
  !! temperature and specific volume.
  !>
  !> \author MH, 2015-02
  !----------------------------------------------------------------------
  function trend_internal_energy(x,t,v,dudv,dudt) result(u)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3/mol - Specific volume
    real, dimension(:), intent(in) :: x !< Compozition
    real, optional, intent(out) :: dudt !< J/(mol K) - Pressure differential wrpt. temperature
    real, optional, intent(out) :: dudv !< J/m3 - Pressure differential wrpt. specific volume
    real :: u !< J/mol - Internal energy
    ! Locals
    u = 0.0

    if (present(dudv)) then
      dudv = 0.0
    endif
    if (present(dudt)) then
      dudt = 0.0
    endif

  end function trend_internal_energy

  !> Calculate fugacity and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine trend_thermoTV(T,V,n,lnfug,lnfugT,lnfugV,lnfugn)
    implicit none
    real, intent(in) :: T,V,n(:)
    ! Output.
    real, optional, intent(out) :: lnfug(:)
    real, optional, intent(out) :: lnfugT(:),lnfugV(:),lnfugn(:,:)
    ! Locals
    if (present(lnfug)) then
      lnfug = 0.0
    endif
    if (present(lnfugT)) then
      lnfugT = 0.0
    endif
    if (present(lnfugV)) then
      lnfugV = 0.0
    endif
    if (present(lnfugn)) then
    lnfugn = 0.0
    endif
  end subroutine trend_thermoTV

  !> Calculate resudial reduced Helmholtz and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  !! \param F... - Resudial reduced Helmholtz differentiuals
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine trend_CalcFres(T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn,Rmix)
    implicit none
    real, intent(in) :: T,V,n(:)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(:),F_TT,F_TV,F_VV,Rmix
    real, optional, intent(out) :: F_Tn(:),F_Vn(:),F_nn(:,:)
    ! Locals

    if (present(F)) F = 0.0
    if (present(F_T)) F_T = 0.0
    if (present(F_TT)) F_TT = 0.0
    if (present(F_V)) F_V = 0.0
    if (present(F_TV)) F_TV = 0.0
    if (present(F_VV)) F_VV = 0.0
    if (present(F_n) .OR. present(F_Tn) .OR.&
        present(F_nn) .OR. present(F_Vn)) then
      F_n = 0.0
      F_Tn = 0.0
      F_Vn = 0.0
      F_nn = 0.0
    endif
    if (present(Rmix)) then
      Rmix = 0.0
    endif
  end subroutine trend_CalcFres

  !> Calculate reduced ideal Helmholtz and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  !! \param F... - Ideal reduced Helmholtz differentiuals
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine trend_CalcFid(T,V,n,F,F_T,F_V,F_TT,F_TV,F_VV,Rmix)
    implicit none
    real, intent(in) :: T,V,n(:)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_TT,F_TV,F_VV,Rmix
    ! Locals

    if (present(F)) F = 0.0
    if (present(F_T)) F_T = 0.0
    if (present(F_TT)) F_TT = 0.0
    if (present(F_V)) F_V = 0.0
    if (present(F_TV)) F_TV = 0.0
    if (present(F_VV)) F_VV = 0.0

    if (present(Rmix)) then
      Rmix = 0.0
    endif
  end subroutine trend_CalcFid

  !> Calculate density from pressure and temperature
  !!
  !! \param T - Temperature [K]
  !! \param P - Specific volume [Pa]
  !! \param n - Mole numbers [mol]
  !! \param phase id
  !!
  !! \author Morten Hammer
  !! \date 2015-10
  function trend_specvol(T,P,n,phase) result(v)
    implicit none
    real, intent(in) :: T,P,n(:)
    integer, intent(in) :: phase
    ! Output.
    real :: v
    ! Locals
    v = 0.0
  end function trend_specvol

  !> Calculate density from pressure and temperature
  !!
  !! \param T - Temperature [K]
  !! \param P - Specific volume [Pa]
  !! \param V - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  !! \param phase id
  !!
  !! \author Morten Hammer
  !! \date 2015-10
  function trend_voltest(T,P,V,n,phase) result(correctPhase)
    implicit none
    real, intent(in) :: T,P,V,n(:)
    integer, intent(in) :: phase
    ! Output.
    integer :: correctPhase
    ! Locals
    !
    correctPhase = 0
  end function trend_voltest

  !----------------------------------------------------------------------
  !> Calculate mixture gas constant
  !>
  !> \author MH, 2015-11
  !----------------------------------------------------------------------
  function trend_rmix(x) result(Rmix)
    implicit none
    ! Input:
    real, dimension(:), intent(in) :: x    !< Composition (mol/mol)
    ! Output:
    real :: Rmix
    Rmix = 0.0
  end function trend_rmix

  !----------------------------------------------------------------------
  !> Calculate saturation properties from auxillary equations
  !>
  !> \author MH, 2016-02
  !----------------------------------------------------------------------
  subroutine trend_aux_sat_prop(T,i,P,vv,vl)
    implicit none
    ! Input:
    real, intent(in) :: T     !< Temperature (K)
    integer, intent(in) :: i  !< Component index
    ! Output:
    real, intent(out) :: P    !< Pressure (Pa)
    real, intent(out) :: vv   !< Vapour volume (m3/mol)
    real, intent(out) :: vl   !< Liquid volume (m3/mol)
    ! Locals
    !--------------------------------------------------------------------
    P = 0.0
    vl = 0.0
    vv = 0.0
  end subroutine trend_aux_sat_prop

  subroutine trend_ideal(T,i,Cp,h,s)
    implicit none
    real, intent(in) :: T     !< Temperature (K)
    integer, intent(in) :: i  !< Component index
    real, optional, intent(out) :: Cp  !< Heat capcity at constant prerssure (J/mol/K)
    real, optional, intent(out) :: h   !< Enthalpy (J/mol)
    real, optional, intent(out) :: s   !< Entropy (J/mol/K)
  end subroutine trend_ideal
