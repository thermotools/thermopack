  interface
    subroutine trend_init_no_char(nc,int_path,npath,int_comps,ncomps,mix,Rgas)
      implicit none
      integer, intent(in) :: nc
      integer, intent(in) :: mix
      integer, intent(in) :: npath
      integer, intent(in) :: ncomps
      integer, intent(in) :: int_path(npath)
      integer, intent(in) :: int_comps(ncomps)
      real,    intent(in) :: Rgas
    end subroutine trend_init_no_char
  end interface

  interface
    subroutine trend_init(nc,path,comps,mix,Rgas)
      implicit none
      integer, intent(in) :: nc
      character (12), dimension (nc), intent(in) :: comps
      character(len=255), intent(inout) :: path
      integer, intent(in) :: mix
      real,    intent(in) :: Rgas
    end subroutine trend_init
  end interface

  interface
    subroutine envelope(moles)
      implicit none
      real, dimension (:), intent(in) :: moles
    end subroutine envelope
  end interface

  interface
    subroutine trend_thermo(t,p,z,phase,lnfug,lnfugt,lnfugp,lnfugn)
      implicit none
      ! Transferred variables
      integer, intent(in) :: phase !< Phase identifyer
      real, intent(in) :: t !< K - Temperature
      real, intent(in) :: p !< Pa - Pressure
      real, dimension(:), intent(in) :: z !< Compozition
      real, dimension(:), intent(out) :: lnfug !< Logarithm of fugasity coefficient
      real, optional, dimension(:), intent(out) :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
      real, optional, dimension(:), intent(out) :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
      real, optional, dimension(:,:), intent(out) :: lnfugn !< Logarithm of fugasity coefficient differential wrpt. mole numbers.
    end subroutine trend_thermo
  end interface

  interface
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
    end subroutine trend_enthalpy
  end interface

  interface
    subroutine trend_thermo_dens(t,rho,z,lnfug,lnfugt,lnfugp,lnfugn)
      implicit none
      ! Transferred variables
      real, intent(in) :: rho !< mol/m3 - Density
      real, intent(in) :: t !< K - Temperature
      real, dimension(:), intent(in) :: z !< Compozition
      real, dimension(:), intent(out) :: lnfug !< Logarithm of fugasity coefficient
      real, optional, dimension(:), intent(out) :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
      real, optional, dimension(:), intent(out) :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
      real, optional, dimension(:,:), intent(out) :: lnfugn !< Logarithm of fugasity coefficient differential wrpt. mole numbers
    end subroutine trend_thermo_dens
  end interface

  interface
    subroutine trend_entropy(t,rho,x,s,dsdt,dsdp,dsdx)
      implicit none
      ! Transferred variables
      real, intent(in) :: rho !< mol/m3 - Density
      real, intent(in) :: t !< K - Temperature
      real, dimension(:), intent(in) :: x !< Compozition
      real, intent(out) :: s !< J/mol/K - Specific entropy
      real, optional, intent(out) :: dsdt !< J/mol/K2 - Specific entropy differential wrpt. temperature
      real, optional, intent(out) :: dsdp !< J/mol/K2/Pa - Specific entropy differential wrpt. pressure
      real, optional, dimension(:), intent(out) :: dsdx !< J/mol 2/K - Specific entropy differential wrpt. mole numbers
    end subroutine trend_entropy
  end interface

  interface
    function trend_pressure(n,t,v,dpdv,dpdt,dpdn) result(p)
      implicit none
      ! Transferred variables
      real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Specific volume
      real, dimension(:), intent(in) :: n !< Mol numbers
      real, optional, intent(out) :: dpdt !< Pa/K - Pressure differential wrpt. temperature
      real, optional, intent(out) :: dpdv !< Pa/m3 - Pressure differential wrpt. volume
      real, optional, intent(out) :: dpdn(:) !< Pa/mol - Pressure differential wrpt. mol
      real :: p !< Pa - Pressure
    end function trend_pressure
  end interface

  interface
    function trend_dpdrho(x,t,rho) result(dpdrho)
      implicit none
      ! Transferred variables
      real, intent(in) :: t !< K - Temperature
      real, intent(in) :: rho !< mol/m3 - Density
      real, dimension(:), intent(in) :: x !< Compozition
      real :: dpdrho
    end function trend_dpdrho
  end interface

  interface
    function trend_d2Pdrho2(x,T,rho) result(d2Pdrho2)
      implicit none
      ! Transferred variables
      real, intent(in)                :: T !< K - Temperature
      real, intent(in)                :: rho !< mol/m3 - Density
      real, dimension(:), intent(in)  :: x !< Composition
      real :: d2Pdrho2
    end function trend_d2Pdrho2
  end interface

  interface
    function trend_psrk(t,p,x,phase) result(rho)
      implicit none
      ! Transferred variables
      real, intent(in) :: t !< K - Temperature
      real, intent(in) :: p !< Pa - Pressure
      real, dimension(:), intent(in) :: x !< Compozition
      integer, intent(in) :: phase !< Phase indicator
      real :: rho !< mol/m3 - Pressure
    end function trend_psrk
  end interface

  interface
    subroutine trend_rhomax_srk(x,rhomax)
      implicit none
      real, dimension(:), intent(in)  :: x      !< Composition
      real,               intent(out) :: rhomax !< Maximum molar density
    end subroutine trend_rhomax_srk
  end interface

  interface
    subroutine trend_specificVolume(t,x,v,dvdt,dvdp,dvdx)
      implicit none
      ! Transferred variables
      real, intent(in) :: t !< K - Temperature
      real, dimension(:), intent(in) :: x !< Compozition
      real, intent(in) :: v !< m3/mol - Specific volume
      real, optional, intent(out) :: dvdt !< m3/mol/K - Specific volume differential wrpt. temperature
      real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specific volume differential wrpt. pressure
      real, optional, dimension(:), intent(out) :: dvdx !< m3/mol - Specific volume differential wrpt. mole numbers
    end subroutine trend_specificVolume
  end interface

  interface
    function trend_compMoleWeight(j) result(mw)
      implicit none
      ! Transferred variables
      integer, intent(in) :: j !< Component index
      real :: mw !< g/mol - Mole weight
    end function trend_compMoleWeight
  end interface

  interface
    function trend_moleWeight(z) result(mw)
      implicit none
      ! Transferred variables
      real, dimension(:), intent(in) :: z !< Composition
      real :: mw !< g/mol - Mole weight
    end function trend_moleWeight
  end interface

  interface
    subroutine trend_getcrit(i,tci,pci,oi, vci, tnbi)
      implicit none
      ! Transferred variables
      integer, intent(in) :: i !< Component index
      real, intent(out) :: tci !< K - Critical temperature
      real, intent(out) :: pci !< Pa - Critical pressure
      real, intent(out) :: oi  !< Asentric factor
      real, optional, intent(out) :: vci !< Critical specific volume (m3/mol)
      real, optional, intent(out) :: tnbi !< Normal boiling point temperature
    end subroutine trend_getcrit
  end interface

  interface
    subroutine trend_zfac(T, rho, x, zfac, dzdt, dzdp, dzdx)
      implicit none
      ! Input:
      real,   intent(in)          :: T        !< Temperature (K)
      real,   intent(in)          :: rho      !< Density (mol/m^3)
      real,   intent(in)          :: x(:)     !< Composition (mol/mol)
      ! Output:
      real,   intent(out)         :: zfac     !< Compressibility factor (-)
      real, optional, intent(out) :: dzdt     !< zfac diff. wrt. temperature
      real, optional, intent(out) :: dzdp     !< zfac diff. wrt. pressure
      real, optional, intent(out) :: dzdx(:)  !< zfac diff. wrt. composition
    end subroutine trend_zfac
  end interface

  interface
    subroutine trend_residualgibbs_tp(T, p, rho, x, gr, dgrdt, dgrdp)
      implicit none
      ! Input:
      real,   intent(in)          :: T        !< Temperature (K)
      real,   intent(in)          :: p        !< Pressure (Pa)
      real,   intent(in)          :: rho      !< Density (mol/m^3)
      real,   intent(in)          :: x(:)     !< Composition (mol/mol)
      ! Output:
      real,   intent(out)         :: gr     !< Residual Gibbs Energy (J/mol)
      real, optional, intent(out) :: dgrdt  !< Res. Gibbs diff. wrt. temperature
      real, optional, intent(out) :: dgrdp  !< Res. Gibbs diff. wrt. pressure
    end subroutine trend_residualgibbs_tp
  end interface

  interface
    subroutine trend_residualenthalpy_tp(T, p, rho, x, hr, dhrdt, dhrdp, dhrdx)
      implicit none
      ! Input:
      real,   intent(in)          :: T        !< Temperature (K)
      real,   intent(in)          :: p        !< Pressure (Pa)
      real,   intent(in)          :: rho      !< Density (mol/m^3)
      real,   intent(in)          :: x(:)     !< Composition (mol/mol)
      ! Output:
      real,   intent(out)         :: hr     !< Residual enthalpy (J/mol)
      real, optional, intent(out) :: dhrdt  !< Res. enth. diff. wrt. temperature
      real, optional, intent(out) :: dhrdp  !< Res. enth. diff. wrt. pressure
      real, optional, intent(out) :: dhrdx(:)!< Res. enth. diff. wrt. comp
    end subroutine trend_residualenthalpy_tp
  end interface

  interface
    subroutine trend_residualentropy_tp(T,p,rho,x,sr,dsrdt,dsrdp,dsrdx)
      implicit none
      ! Input:
      real, intent(in)                  :: T    !< Temperature (K)
      real, intent(in)                  :: p    !< Pressure (Pa)
      real, intent(in)                  :: rho  !< Density (mol/m3)
      real, dimension(:), intent(in)    :: x    !< Composition (mol/mol)
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
    end subroutine trend_residualentropy_tp
  end interface

  interface
    function trend_speedofsound(T,rho,x) result(c)
      implicit none
      ! Input:
      real,   intent(in)          :: T        !< Temperature (K)
      real,   intent(in)          :: rho      !< Density (mol/m^3)
      real,   intent(in)          :: x(:)  !< Composition (mol/mol)
      ! Output:
      real                        :: c        !< Speed of sound (m/s)
    end function trend_speedofsound
  end interface

  interface
    function trend_ideal_Cp(T,j) result(Cp_id)
      implicit none
      ! Input:
      real,    intent(in)         :: T        !< Temperature (K)
      integer, intent(in)         :: j        !< Component index
      ! Output:
      real                        :: Cp_id    !< Ideal heat capacity (J/mol/K)
    end function trend_ideal_Cp
  end interface

  interface
    function trend_ideal_enthalpy(T,j) result(hid)
      implicit none
      ! Input:
      real,    intent(in)         :: T        !< Temperature (K)
      integer, intent(in)         :: j        !< Component index
      ! Output:
      real                        :: hid      !< Ideal enthalpy (J/mol)
    end function trend_ideal_enthalpy
  end interface

  interface
    function trend_ideal_entropy(T,P,j) result(sid)
      implicit none
      ! Input:
      real,    intent(in)         :: T        !< Temperature (K)
      real,    intent(in)         :: P        !< Density (Pa)
      integer, intent(in)         :: j        !< Component index
      ! Output:
      real                        :: sid      !< Ideal entropy (J/mol/K)
    end function trend_ideal_entropy
  end interface


  interface
    subroutine trend_Get_binary_parameters(i1,i2, ParRedFun, ndf, ParDepFun)
      integer, parameter:: nDim = 25  !Dimensions of array in module_mixture_parameters
      integer, intent(in) :: i1,i2
      real, intent(out):: ParRedFun(5)
      integer, intent(out) :: ndf(2)
      real, intent(out)::ParDepFun(nDim,8)
    end subroutine trend_Get_binary_parameters
  end interface

  interface
    subroutine trend_Set_binary_parameters(i1,i2, ParRedFun, ndf, ParDepFun)
      integer, parameter:: nDim = 25  !Dimensions of array in module_mixture_parameters
      integer, intent(in) :: i1,i2
      real, intent(in):: ParRedFun(5)
      integer, intent(in) :: ndf(2)
      real, intent(in)::ParDepFun(nDim,8)
    end subroutine trend_SEt_Binary_parameters
  end interface

  interface
    subroutine trend_GetReducedRhoT(T, x, RhoRed, TRed)
      implicit none
      real, intent(in) :: T
      real, intent(in) :: x(:)
      real, intent(out):: RhoRed
      real, intent(out)::TRed
    end subroutine trend_GetReducedRhoT
  end interface

  interface
    function trend_GetPVapPure(T, icmp) result(P)
      implicit none
      real, intent(in) :: T
      integer, intent(in):: icmp
      real :: P
    end function trend_GetPVapPure
  end interface

  interface
    function trend_free_energy(x,t,v,dydv,dydt,d2ydt2,d2ydv2,d2ydvdt) result(y)
      implicit none
      ! Transferred variables
      real, intent(in) :: t !< K - Temperature
      real, intent(in) :: v !< m3/mol - Specific volume
      real, dimension(:), intent(in) :: x !< Compozition
      real, optional, intent(out) :: dydt !< J/(mol K) - Helmholtz free energy differential wrpt. temperature
      real, optional, intent(out) :: dydv !< J/m3 - Helmholtz free energy differential wrpt. specific volume
      real, optional, intent(out) :: d2ydt2 !< J/(mol K2) - Helmholtz differential wrpt. temperature
      real, optional, intent(out) :: d2ydv2 !< J/m6 - Helmholtz second differential wrpt. specific volume
      real, optional, intent(out) :: d2ydvdt !< J/(m3 K) - Helmholtz second differential wrpt. specific volume and temperature
      real :: y !< J/mol - Helmholtz free energy
    end function trend_free_energy
  end interface

  interface
    function trend_internal_energy(x,t,v,dudv,dudt) result(u)
      implicit none
      ! Transferred variables
      real, intent(in) :: t !< K - Temperature
      real, intent(in) :: v !< m3/mol - Specific volume
      real, dimension(:), intent(in) :: x !< Compozition
      real, optional, intent(out) :: dudt !< J/(mol K) - Internal energy differential wrpt. temperature
      real, optional, intent(out) :: dudv !< J/m3 - Internal energy differential wrpt. specific volume
      real :: u !< J/mol - Internal energy
    end function trend_internal_energy
  end interface

  interface
    subroutine trend_thermoTV(T,V,n,lnfug,lnfugT,lnfugV,lnfugn)
      real, intent(in) :: T,V,n(:)
      real, optional, intent(out) :: lnfug(:),lnfugT(:)
      real, optional, intent(out) :: lnfugV(:),lnfugn(:,:)
    end subroutine trend_thermoTV
  end interface

  interface
    subroutine trend_CalcFres(T,V,n,F,F_T,F_V,F_n,F_TT,&
         F_TV,F_VV,F_Tn,F_Vn,F_nn)
      implicit none
      real, intent(in) :: T,V,n(:)
      real, optional, intent(out) :: F,F_T,F_V,F_n(:),F_TT,F_TV,F_VV
      real, optional, intent(out) :: F_Tn(:),F_Vn(:),F_nn(:,:)
    end subroutine trend_CalcFres
  end interface

  interface
    subroutine trend_CalcFid(T,V,n,F,F_T,F_V,F_TT,F_TV,F_VV,F_n,F_Tn,F_Vn,F_nn)
      real, intent(in) :: T,V,n(:)
      real, optional, intent(out) :: F,F_T,F_V,F_TT,F_TV,F_VV
      real, optional, intent(out) :: F_n(:),F_Tn(:),F_Vn(:),F_nn(:,:)
    end subroutine trend_CalcFid
  end interface

  interface
    function trend_rmix(x) result(Rmix)
      real, dimension(:), intent(in) :: x
      real :: Rmix
    end function trend_rmix
  end interface

  interface
    subroutine trend_aux_sat_prop(T,i,P,vv,vl)
      implicit none
      real, intent(in) :: T     !< Temperature (K)
      integer, intent(in) :: i  !< Component index
      real, intent(out) :: P    !< Pressure (Pa)
      real, intent(out) :: vv   !< Vapour volume (m3/mol)
      real, intent(out) :: vl   !< Liquid volume (m3/mol)
    end subroutine trend_aux_sat_prop
  end interface

  interface
    subroutine trend_ideal(T,i,Cp,h,s)
      implicit none
      real, intent(in) :: T     !< Temperature (K)
      integer, intent(in) :: i  !< Component index
      real, optional, intent(out) :: Cp  !< Heat capacity at constant pressure (J/mol/K)
      real, optional, intent(out) :: h   !< Enthalpy (J/mol)
      real, optional, intent(out) :: s   !< Entropy (J/mol/K)
    end subroutine trend_ideal
  end interface
