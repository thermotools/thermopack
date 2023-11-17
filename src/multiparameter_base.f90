!> Definitions of adimensional variables:
!> delta = rho/rho_c
!> tau = T_c/T
!> alpha0 = A^{ideal}/(nRT)
!> alphaRes = A^{res}/(nRT)
!> alpha = A/(nRT)
module multiparameter_base
  use numconstants, only: machine_prec
  use thermopack_constants, only: LIQPH, VAPPH
  use thermopack_constants, only: Rgas_default
  implicit none
  save
  public

  ! Relative accuracy in density solver.
  real, parameter :: releps_p = machine_prec*1e8
  real, parameter :: releps_rho = machine_prec*1e6

  ! Relative accuracy in density solver.
  integer, parameter :: REF_NO_SOLVE=1, REF_EVALUATE_ID=2, &
       REF_SOLVE_FOR_T=3, REF_SOLVE_FOR_P=4

  !> Base class for multiparameter equations of state
  type, abstract :: meos
    !> Parameters in SI units. These are set in the deferred init routine.
    character(LEN=20), public :: compName
    real, public :: tc, pc, rc, acf
    real, public :: t_triple, p_triple, rhoLiq_triple, rhoVap_triple
    real, public :: molarMass !< (kg/mol)
    real, public :: maxT, maxP !< (K), (Pa)
    real :: tr, rhor !> Reducing temperature and density


    ! When a MEoS is used with Thermopack, Rgas_meos must have the same value as
    ! in Thermopack. When a MEoS is used on its own, one should set
    ! Rgas_meos=meos%Rgas_fit. This is because different multiparameter EoS use
    ! slightly different values of the gas constant.
    real, public :: Rgas_meos = Rgas_default
    real, public :: Rgas_fit ! Rgas used when the equation was fitted (J/(mol*K))
  contains

    ! Public methods
    procedure, public :: mp_pressure
    procedure, public :: alpha_to_F_conversion
    procedure, public :: calc_F
    procedure, public :: calc_Fid
    procedure, public :: calc_zfac
    procedure, public :: calc_lnphi
    procedure, public :: calc_entropy
    procedure, public :: calc_enthalpy
    procedure, public :: calc_resgibbs
    procedure, public :: densitySolver ! The child class should override this if necessary
    procedure, public :: alphaDerivs_Tv ! d^{i+j}alpha/(d_v)^i(d_T)^j
    procedure, public :: alphaResDerivs_Tv ! d^{i+j}alphaRes/(d_v)^i(d_T)^j
    procedure, public :: alphaIdDerivs_Tv ! d^{i+j}alphaRes/(d_v)^i(d_T)^j
    procedure, public :: getCritPoint
    procedure(init_intf), public, deferred :: init !< Initiate compName, critical point and triple point

    procedure, public :: cv ! Isochoric heat capacity [J/(mol*K)]
    procedure, public :: cp ! Isobaric heat capacity[J/(mol*K)]
    procedure, public :: speed_of_sound !< [m/s]

    ! Reference state
    procedure, public :: get_ref_state_spec => get_ref_state_spec_default !< Get specification for the current reference state
    procedure, public :: set_ref_state => set_ref_state_default !< Set reference state

    ! Private methods
    procedure(satDeltaEstimate_intf), public, deferred :: satDeltaEstimate !< An estimate delta_sat(tau_sat) for use in density solver.
    procedure(alpha0Derivs_intf), public, deferred :: alpha0Derivs_taudelta  !< [d^{j}alpha0/(d_tau)^j]*tau^j
    procedure(alphaResDerivs_intf), public, deferred :: alphaResDerivs_taudelta  !< [d^{i+j}alphaRes/(d_delta)^i(d_tau)^j]*delta^i*tau^j
    ! Hyperdual number interfaces
    procedure(alpha0_hd_intf), public, deferred :: alpha0_hd_taudelta  !< Calculate alpha0 using hyperdual numbers
    procedure(alphaRes_hd_intf), public, deferred :: alphaRes_hd_taudelta  !< Calculate alphaRes using hyperdual numbers

    procedure, public :: assign_meos_base
    ! Assignment operator
    procedure(assign_meos_intf), deferred, pass(This), public :: assign_meos
    generic, public :: assignment(=) => assign_meos

  end type meos

  abstract interface
    subroutine assign_meos_intf(This, other)
      import meos
      ! Passed object:
      class(meos), intent(inout) :: This
      class(*), intent(in) :: other
    end subroutine assign_meos_intf
  end interface

  abstract interface
    subroutine init_intf (this, use_Rgas_fit)
      import meos
      class(meos) :: this
      logical, optional, intent(in) :: use_Rgas_fit
    end subroutine init_intf
  end interface

  abstract interface
    function satDeltaEstimate_intf(this,tau,phase) result(deltaSat)
      import meos
      class(meos) :: this
      real, intent(in) :: tau !< Reduced temperature (-)
      integer, intent(in) :: phase !< Phase flag
      real :: deltaSat !< Reduced density (-)
    end function satDeltaEstimate_intf
  end interface

  abstract interface
    subroutine alphaResDerivs_intf(this, delta, tau, alpr)
      import meos
      class(meos) :: this
      real, intent(in) :: delta !< Reduced density (-)
      real, intent(in) :: tau !< Reduced temperature (-)
      real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = [(d_delta)^i(d_tau)^j alphaRes]*delta^i*tau^j
    end subroutine alphaResDerivs_intf
  end interface

  abstract interface
    subroutine alpha0Derivs_intf(this,delta, tau, alp0)
      import meos
      class(meos) :: this
      real, intent(in) :: delta !< Reduced density (-)
      real, intent(in) :: tau !< Reduced temperature (-)
      real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    end subroutine alpha0Derivs_intf
  end interface

  abstract interface
    function alphaRes_hd_intf(this, delta, tau) result(alpr)
      use hyperdual_mod, only: hyperdual
      import meos
      class(meos) :: this
      type(hyperdual), intent(in) :: delta !< Reduced density (-)
      type(hyperdual), intent(in) :: tau !< Reduced temperature (-)
      type(hyperdual) :: alpr !< Residual reduced Helmholtz energy
    end function alphaRes_hd_intf
  end interface

  abstract interface
    function alpha0_hd_intf(this,delta, tau) result(alp0)
      use hyperdual_mod, only: hyperdual
      import meos
      class(meos) :: this
      type(hyperdual), intent(in) :: delta !< Reduced density (-)
      type(hyperdual), intent(in) :: tau !< Reduced temperature (-)
      type(hyperdual) :: alp0 !< Ideal reduced Helmholtz energy
    end function alpha0_hd_intf
  end interface

  ! Allow array of pointers to NIST meos
  type :: nist_meos_ptr
    class(meos), pointer :: meos
  end type nist_meos_ptr

contains

  !> Isochoric heat capacity
  function cv (this,T,v)
    class(meos) :: this !< Calling class
    real, intent(in) :: T,v !< Temperature (K) and molar volume (m^3/mol)
    real :: cv !< [J/(mol*K)]
    ! Internals
    real :: delta, tau
    real :: alpr(0:2,0:2), alp0(0:2,0:2)

    delta = 1/(v*this%rc)
    tau = this%tc/T
    ! Calculate [d^{i+j}alphaRes/(d_delta)^i(d_tau)^j]*delta^i*tau^j
    call this%alphaResDerivs_taudelta(delta, tau, alpr)
    call this%alpha0Derivs_taudelta(delta, tau, alp0)

    cv = -this%Rgas_meos * (alp0(0,2) + alpr(0,2))

  end function cv


  !> Isobaric heat capacity
  function cp (this,T,v)
    class(meos) :: this !< Calling class
    real, intent(in) :: T,v !< Temperature (K) and molar volume (m^3/mol)
    real :: cp !< [J/(mol*K)]
    ! Internals
    real :: delta, tau
    real :: alpr(0:2,0:2), alp0(0:2,0:2)

    delta = 1/(v*this%rc)
    tau = this%tc/T

    ! Calculate [d^{i+j}alphaRes/(d_delta)^i(d_tau)^j]*delta^i*tau^j
    call this%alphaResDerivs_taudelta(delta, tau, alpr)
    call this%alpha0Derivs_taudelta(delta, tau, alp0)

    cp = this%cv(T,v) + this%Rgas_meos* ( 1+alpr(1,0)-alpr(1,1) )**2/( 1+2*alpr(1,0)+alpr(2,0) )

  end function cp

  !> Speed of sound
  function speed_of_sound (this,T,v)
    class(meos) :: this !< Calling class
    real, intent(in) :: T,v !< Temperature (K) and molar volume (m^3/mol)
    real :: speed_of_sound !< [m/s]
    ! Internals
    real :: delta, tau
    real :: alpr(0:2,0:2), alp0(0:2,0:2)
    real :: adim_temp

    delta = 1/(v*this%rc)
    tau = this%tc/T

    ! Calculate [d^{i+j}alphaRes/(d_delta)^i(d_tau)^j]*delta^i*tau^j
    call this%alphaResDerivs_taudelta(delta, tau, alpr)
    call this%alpha0Derivs_taudelta(delta, tau, alp0)

    adim_temp = 1 + 2*alpr(1,0) + alpr(2,0) - ( 1 + alpr(1,0) - alpr(1,1) )**2/( alp0(0,2) + alpr(0,2))

    speed_of_sound = sqrt(adim_temp*this%Rgas_meos*T/this%molarMass)

  end function speed_of_sound


  subroutine getCritPoint (this, tcrit, pcrit, rhocrit)
    class(meos) :: this
    real, intent(out) :: tcrit, pcrit, rhocrit

    tcrit = this%tc
    pcrit = this%pc
    rhocrit = this%rc
  end subroutine getCritPoint


  subroutine calc_zfac(this, t, p, n, phase, Z, Z_t, Z_p, Z_n)
    class(meos) :: this
    ! input
    real, intent(in) :: t
    real, intent(in) :: p
    real, intent(in) :: n(1)
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: Z
    real, optional, intent(out) :: Z_t
    real, optional, intent(out) :: Z_p
    real, optional, intent(out) :: Z_n(1)
    ! local
    integer :: phase_found
    real :: rho, v
    real :: alpr, alpr_T, alpr_v, alpr_TT, alpr_Tv, alpr_vv
    real :: dPdV, dPdT
    real :: dVdT

    ! Necessary alpha-derivatives
    call this%densitySolver(t, p, phase, rho, phase_found)
    v = 1/rho
    Z = p/(rho*this%Rgas_meos*T)

    if (present(Z_t) .or. present(Z_p)) then
      call this%alphaResDerivs_Tv(t, v, alpr,alpr_T,alpr_v,alpr_TT,alpr_Tv,alpr_vv)

      dPdV = -this%Rgas_meos*T*(alpr_VV + 1/V**2)
    end if

    if (present(Z_t)) then
      dPdT = P/T - this%Rgas_meos*T*alpr_TV
      dVdT = -dPdT/dPdV
      Z_t = -Z*(1.0/T - dVdT/V)
    end if

    if (present(Z_p)) then
      Z_p = Z*(1.0/P + 1.0/(dPdV*V))
    end if

    if (present(Z_n)) then
      Z_n = 0.0
    end if

  end subroutine calc_zfac


  subroutine calc_lnphi(this, t, p, n, phase, lnphi, lnphi_t, lnphi_p, lnphi_n)
    class(meos) :: this
    ! input
    real, intent(in) :: t
    real, intent(in) :: p
    real, intent(in) :: n(1)
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: lnphi(1)
    real, optional, intent(out) :: lnphi_t(1)
    real, optional, intent(out) :: lnphi_p(1)
    real, optional, intent(out) :: lnphi_n(1,1)
    ! local
    integer :: phase_found
    real :: rho, v, zfac
    real :: dPdV, dPdT, dPdn, dVdn
    real :: F ! A/(RT), so that [F]=mol
    real :: F_T, F_V, F_n(1)
    real :: F_TT, F_TV, F_Tn(1), F_VV, F_Vn(1), F_nn(1,1)
    real :: sumn

    sumn=n(1)
    ! Necessary alpha-derivatives
    call this%densitySolver(t, p, phase, rho, phase_found)
    V = sumn/rho
    zfac = p/(rho*this%Rgas_meos*T)

    call this%calc_F(T, V, n, F, F_T, F_V, F_n, F_TT, F_TV, F_tn, F_VV, F_Vn, F_nn)

    lnphi = F_n - log(zfac)
    dPdV = -this%Rgas_meos*T*(F_VV + sumn/V**2)
    dPdn = this%Rgas_meos*T*(-F_Vn(1) + 1/V)
    dVdn = -dPdn/dPdV

    if (present(lnphi_t)) then
      dPdT = P/T-this%Rgas_meos*T*F_TV
      lnphi_t = F_Tn + (1 - dVdn*dPdT/this%Rgas_meos)/T
    endif

    if (present(lnphi_p)) then
      lnphi_p = dVdn/(this%Rgas_meos*T)-1/P
    endif

    if (present(lnphi_n)) then
      lnphi_n(1,1) = 0.0
    endif

  end subroutine calc_lnphi

  ! Calculate residual or total entropy.
  subroutine calc_entropy(this, t, p, n, phase, S, S_t, S_p, S_n, residual)
    class(meos) :: this
    ! input
    real, intent(in) :: t
    real, intent(in) :: p
    real, intent(in) :: n(1)
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: S ! J/(mol*K)
    real, optional, intent(out) :: S_t
    real, optional, intent(out) :: S_p
    real, optional, intent(out) :: S_n(1)
    logical, optional, intent(in) :: residual
    ! local
    integer :: phase_found
    real :: rho, v, zfac, sumn
    real :: alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv
    real :: dPdV, dPdT, dVdT
    logical :: res_loc

    sumn = n(1)
    res_loc = .false.
    if (present(residual)) res_loc = residual

    call this%densitySolver(t, p, phase, rho, phase_found)
    V = sumn/rho
    zfac = p/(rho*this%Rgas_meos*T)
    call this%alphaDerivs_Tv(t, 1.0/rho, alp, alp_T,alp_v,alp_TT,alp_Tv,alp_vv,residual=residual)
    dPdV = -this%Rgas_meos*T*(alp_vv)/sumn
    if (res_loc) dPdV = dPdV -this%Rgas_meos*T*sumn/V**2
    dPdT = P/T-This%Rgas_Meos*T*alp_TV
    dVdT = -dPdT/dPdV

    S = -sumn*this%Rgas_meos*(alp + t*alp_t)
    if (res_loc) S = S + sumn*this%Rgas_meos*log(zfac)

    if (present(S_t)) then
      S_t = dVdt*dPdt - sumn*This%Rgas_Meos*(2*alp_T + T*alp_TT)
      if (res_loc) S_t = S_t - sumn*this%Rgas_meos/T
    endif

    if (present(S_p)) then
      S_p = -dVdT
      if (res_loc) S_p = S_p + sumn*This%Rgas_Meos/P
    endif

    if (present(S_n)) then
      S_n(1) = S/sumn
    endif

  end subroutine calc_entropy

  ! Calculate residual or total enthalpy.
  subroutine calc_enthalpy(this, t, p, n, phase, h, h_t, h_p, h_n, residual)
    class(meos) :: this
    ! input
    real, intent(in) :: t
    real, intent(in) :: p
    real, intent(in) :: n(1)
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: h ! J/mol
    real, optional, intent(out) :: h_t
    real, optional, intent(out) :: h_p
    real, optional, intent(out) :: h_n(1)
    logical, optional, intent(in) :: residual
    ! local
    integer :: phase_found
    real :: rho, v, sumn
    real :: alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv
    real :: dPdV, dPdT, dVdT
    logical :: res_loc
    sumn = n(1)

    res_loc = .false.
    if (present(residual)) res_loc = residual

    call this%densitySolver(t, p, phase, rho, phase_found)

    V = sumn/rho

    call this%alphaDerivs_Tv(t, v/sumn, alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv,residual=residual)
    dPdV = -this%Rgas_meos*T*(alp_vv)/sumn
    if (res_loc) dPdV = dPdV -this%Rgas_meos*T*sumn/V**2
    dPdT = P/T-This%Rgas_Meos*T*alp_TV
    dVdT = -dPdT/dPdV

    H = -sumn*This%Rgas_Meos*T*T*alp_T + P*V
    if (res_loc) h = h-sumn*this%Rgas_meos*T

    if (present(h_t)) then
      h_t = T*(dVdT*dPdT - sumn*This%Rgas_Meos*(2*alp_T + T*alp_TT + 1/T))
      if (.not. res_loc) h_t = h_t + sumn*this%Rgas_meos
    endif

    if (present(h_p)) then
      h_p = V-T*dVdt
    endif

    if (present(h_n)) then
      h_n(1) = h/sumn
    endif

  end subroutine calc_enthalpy

  subroutine alpha_to_F_conversion(this, T, V, n, alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv, &
       F, F_T, F_V, F_n, F_TT, F_TV, F_tn, F_VV, F_Vn, F_nn)
    ! input
    class(meos), intent(in) :: this
    real, intent(in) :: t ! [K]
    real, intent(in) :: V ! [m^3]
    real, intent(in) :: n(1) ! [mol]
    real, intent(in) :: alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv
    ! output
    real, optional, intent(out) :: F ! A/(RT), so that [F]=mol
    real, optional, intent(out) :: F_T, F_V, F_n(1)
    real, optional, intent(out) :: F_TT, F_TV, F_Tn(1), F_VV, F_Vn(1), F_nn(1,1)

    ! F(T,V,n) = n*alp(T,V/n)
    if (present(F)) F = alp*n(1)
    if (present(F_T)) F_T = alp_T*n(1)
    if (present(F_V)) F_V = alp_v
    if (present(F_n)) F_n = alp - alp_v*V/n(1)
    if (present(F_TT)) F_TT = alp_TT*n(1)
    if (present(F_TV)) F_TV = alp_Tv
    if (present(F_Tn)) F_Tn = alp_T - alp_Tv*V/n(1)
    if (present(F_VV)) F_VV = alp_vv/n(1)
    if (present(F_Vn)) F_Vn = -alp_vv*V/n(1)**2
    if (present(F_nn)) F_nn = alp_vv*V**2/n(1)**3

  end subroutine alpha_to_F_conversion

  subroutine calc_F(this, T, V, n, F, F_T, F_V, F_n, F_TT, F_TV, F_tn, F_VV, F_Vn, F_nn)
    class(meos) :: this
    ! input
    real, intent(in) :: t ! [K]
    real, intent(in) :: V ! [m^3]
    real, intent(in) :: n(1) ! [mol]
    ! output
    real, optional, intent(out) :: F ! A/(RT), so that [F]=mol
    real, optional, intent(out) :: F_T, F_V, F_n(1)
    real, optional, intent(out) :: F_TT, F_TV, F_Tn(1), F_VV, F_Vn(1), F_nn(1,1)
    ! local
    real :: alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv

    call this%alphaDerivs_Tv(t, V/n(1), alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv,residual=.TRUE.)

    call this%alpha_to_F_conversion(T, V, n, alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv, &
         F, F_T, F_V, F_n, F_TT, F_TV, F_tn, F_VV, F_Vn, F_nn)

  end subroutine calc_F

  !> Calculate reduced ideal Helmholtz energy and differentials
  subroutine calc_Fid(this, T, V, n, F, F_T, F_V, F_n, F_TT, F_TV, F_Tn, F_VV, F_Vn, F_nn)
    class(meos) :: this
    ! input
    real, intent(in) :: t ! [K]
    real, intent(in) :: V ! [m^3]
    real, intent(in) :: n(1) ! [mol]
    ! output
    real, optional, intent(out) :: F ! A/(RT), so that [F]=mol
    real, optional, intent(out) :: F_T, F_V, F_n(1)
    real, optional, intent(out) :: F_TT, F_TV, F_Tn(1), F_VV, F_Vn(1), F_nn(1,1)
    ! local
    real :: alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv

    call this%alphaIdDerivs_Tv(t, V/n(1), alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv)

    call this%alpha_to_F_conversion(T, V, n, alp, alp_T, alp_v, alp_TT, alp_Tv, alp_vv, &
         F, F_T, F_V, F_n, F_TT, F_TV, F_tn, F_VV, F_Vn, F_nn)

  end subroutine calc_Fid

  ! Calculate residual Gibbs energy.
  subroutine calc_resgibbs(this, t, p, n, phase, g, g_t, g_p, g_n)
    class(meos) :: this
    ! input
    real, intent(in) :: t
    real, intent(in) :: p
    real, intent(in) :: n(1)
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: g ! J/mol
    real, optional, intent(out) :: g_t, g_p, g_n(1)
    ! local
    integer :: phase_found
    real :: rho, v, zfac
    real :: dPdV, dPdT, dVdT
    real :: F ! A/(RT), so that [F]=mol
    real :: F_T, F_V, F_n(1)
    real :: F_TT, F_TV, F_Tn(1), F_VV, F_Vn(1), F_nn(1,1)
    real :: sumn

    sumn=n(1)
    ! Necessary alpha-derivatives
    call this%densitySolver(t, p, phase, rho, phase_found)
    V = sumn/rho
    zfac = p/(rho*this%Rgas_meos*T)

    call this%calc_F(T, V, n, F, F_T, F_V, F_n, F_TT, F_TV, F_tn, F_VV, F_Vn, F_nn)

    G = this%Rgas_meos*T*F + P*V - sumn*this%Rgas_meos*T*(1+log(zFac))

    if (present(G_P)) then
      G_p = V*(1-1/zFac)
    endif

    if (present(G_T)) then
      dPdV = -this%Rgas_meos*T*(F_VV + sumn/V**2)
      dPdT = P/T-this%Rgas_meos*T*F_TV
      dVdT = -dPdT/dPdV
      G_T =  this%Rgas_meos*(F + T*F_T - sumn*log(zfac)) &
           + (P-P/zfac+this%Rgas_meos*T*F_V)*dVdT
    end if

    if (present(G_n)) then
      G_n(1) = G/sumn
    endif

  end subroutine calc_resgibbs


  ! Derivatives of alpha=alpha0+alphaRes wrt. T and v.
  subroutine alphaResDerivs_Tv (this,T,v,alpr,alpr_T,alpr_v,alpr_TT,alpr_Tv,alpr_vv, &
       alpr_n, alpr_Tn, alpr_vn, alpr_nn)
    class(meos) :: this !< Calling class
    real, intent(in) :: T,v !< Temperature (K) and molar volume (m^3/mol)
    real, optional, intent(out) :: alpr !< A/(nRT)
    real, intent(out), optional :: alpr_T, alpr_v, alpr_TT, alpr_Tv, alpr_vv
    real, intent(out), optional :: alpr_n, alpr_vn, alpr_Tn, alpr_nn
    ! Internals
    real :: delta, tau
    real :: alpr_deltaTau(0:2,0:2)

    delta = 1/(v*this%rc)
    tau = this%tc/T

    ! Calculate [d^{i+j}alphaRes/(d_delta)^i(d_tau)^j]*delta^i*tau^j
    call this%alphaResDerivs_taudelta(delta, tau, alpr_deltaTau)

    ! Compute alpr and its derivatives
    if ( present(alpr)) alpr = alpr_deltaTau(0,0)
    if ( present(alpr_T) ) alpr_T = -alpr_deltaTau(0,1)/T
    if ( present(alpr_v) ) alpr_v = -alpr_deltaTau(1,0)/v
    if ( present(alpr_Tv) ) alpr_Tv = alpr_deltaTau(1,1)/(T*v)
    if ( present(alpr_TT) ) alpr_TT = (alpr_deltaTau(0,2) + 2*alpr_deltaTau(0,1))/T**2
    if ( present(alpr_vv) ) alpr_vv = (alpr_deltaTau(2,0) + 2*alpr_deltaTau(1,0))/v**2

    ! Differentiating d(alp*n)/dn, assuming sum(n) = 1
    if ( present(alpr_n)) alpr_n = alpr_deltaTau(0,0) + alpr_deltaTau(1,0)
    if ( present(alpr_Tn) ) alpr_Tn = -(alpr_deltaTau(0,1) + alpr_deltaTau(1,1))/T
    if ( present(alpr_vn) ) alpr_vn = -(2.0*alpr_deltaTau(1,0) + alpr_deltaTau(2,0))/v
    if ( present(alpr_nn) ) alpr_nn = 2.0*alpr_deltaTau(1,0) + alpr_deltaTau(2,0)

  end subroutine alphaResDerivs_Tv


  ! Derivatives of alpha=alpha0+alphaRes wrt. T and v.
  subroutine alphaDerivs_Tv (this,T,v,alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv,residual)
    class(meos) :: this !< Calling class
    real, intent(in) :: T,v !< Temperature (K) and molar volume (m^3/mol)
    real, optional, intent(out) :: alp !< A/(nRT)
    real, intent(out), optional :: alp_T, alp_v, alp_TT, alp_Tv, alp_vv
    logical, optional, intent(in) :: residual
    ! Internals
    real :: delta, tau
    real :: alp0_deltaTau(0:2,0:2)
    real :: alpr_deltaTau(0:2,0:2)
    real :: alp_deltaTau(0:2,0:2)

    delta = 1/(v*this%rc)
    tau = this%tc/T

    if (present(residual)) then
      if (residual) then
        alp0_deltaTau = 0.0
      else
        call this%alpha0Derivs_taudelta(delta, tau, alp0_deltaTau)
      end if
    else
      call this%alpha0Derivs_taudelta(delta, tau, alp0_deltaTau)
    end if
    call this%alphaResDerivs_taudelta(delta, tau, alpr_deltaTau)
    alp_deltaTau = alpr_deltaTau + alp0_deltaTau

    ! Compute alpha and its derivatives. Independent of the scale of T and v.
    if ( present(alp)) alp = alp_deltaTau(0,0)
    if ( present(alp_T) ) alp_T = -alp_deltaTau(0,1)/T
    if ( present(alp_v) ) alp_v = -alp_deltaTau(1,0)/v
    if ( present(alp_Tv) ) alp_Tv = alp_deltaTau(1,1)/(T*v)
    if ( present(alp_TT) ) alp_TT = (alp_deltaTau(0,2) + 2*alp_deltaTau(0,1))/T**2
    if ( present(alp_vv) ) alp_vv = (alp_deltaTau(2,0) + 2*alp_deltaTau(1,0))/v**2
  end subroutine alphaDerivs_Tv

  ! Derivatives of alpha=alpha0 wrt. T and v.
  subroutine alphaIdDerivs_Tv(this,T,v,alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv, &
       alp_n, alp_Tn, alp_vn, alp_nn)
    class(meos) :: this !< Calling class
    real, intent(in) :: T,v !< Temperature (K) and molar volume (m^3/mol)
    real, optional, intent(out) :: alp !< A/(nRT)
    real, intent(out), optional :: alp_T, alp_v, alp_TT, alp_Tv, alp_vv
    real, intent(out), optional :: alp_n, alp_Tn, alp_vn, alp_nn
    ! Internals
    real :: delta, tau
    real :: alp_deltaTau(0:2,0:2)

    delta = 1/(v*this%rc)
    tau = this%tc/T

    call this%alpha0Derivs_taudelta(delta, tau, alp_deltaTau)

    ! Compute alpha and its derivatives. Independent of the scale of T and v.
    if ( present(alp)) alp = alp_deltaTau(0,0)
    if ( present(alp_T) ) alp_T = -alp_deltaTau(0,1)/T
    if ( present(alp_v) ) alp_v = -alp_deltaTau(1,0)/v
    if ( present(alp_Tv) ) alp_Tv = alp_deltaTau(1,1)/(T*v)
    if ( present(alp_TT) ) alp_TT = (alp_deltaTau(0,2) + 2*alp_deltaTau(0,1))/T**2
    if ( present(alp_vv) ) alp_vv = (alp_deltaTau(2,0) + 2*alp_deltaTau(1,0))/v**2

    ! Differentiating d(alp*n)/dn, assuming sum(n) = 1
    if ( present(alp_n)) alp_n = alp_deltaTau(0,0) + alp_deltaTau(1,0)
    if ( present(alp_Tn) ) alp_Tn = -(alp_deltaTau(0,1) + alp_deltaTau(1,1))/T
    if ( present(alp_vn) ) alp_vn = -(2.0*alp_deltaTau(1,0) + alp_deltaTau(2,0))/v
    if ( present(alp_nn) ) alp_nn = 2.0*alp_deltaTau(1,0) + alp_deltaTau(2,0)

  end subroutine alphaIdDerivs_Tv

  subroutine densitySolver(this, T_spec, p_spec, phase_spec, rho, phase_found, ierr)
    use thermopack_constants
    class(meos) :: this !< The calling class.
    real, intent(in) :: T_spec, p_spec !< Temperature (K) and pressure (Pa)
    integer, intent(in) :: phase_spec !< Phase flag.
    real, intent(out) :: rho !< Density (mol/m^3)
    integer, optional, intent(out) :: phase_found
    integer, optional, intent(out) :: ierr
    ! Internals
    integer :: iter, maxiter=200
    real :: rhoOld, pOld, dpdrhoOld
    real :: p, dpdrho
    real :: pMin, dpdrhoMin
    integer :: curvatureSign
    logical :: converged
    integer :: currentPhase, nPhaseSwitches

    pMin = 0 ! Minimum allowable pressure during iteration.
    dpdrhoMin = 0 ! Minimum allowable pressure derivative during iteration.
    currentPhase = phase_spec ! May change during iteration
    nPhaseSwitches = 0 ! No phase switches so far
    iter = 0
    call initializeSearch() ! Set initial rho, p, dpdrho
    if (present(ierr)) ierr = 0

    ! Newton iteration loop
    do while (.true.)
      rhoOld = rho
      pOld = p
      dpdrhoOld = dpdrho

      rho = rho - (p-p_spec)/dpdrho
      if (rho<0) then
        call switchPhase()
      else
        call this%mp_pressure(rho, T_spec, p, dpdrho)
        if ( p<pMin .or. dpdrho < dpdrhoMin .or. &
             curvatureSign*(rho-rhoOld)*(dpdrho-dpdrhoOld) < -2e-10*abs(rho*dpdrho) ) then
          call switchPhase()
          continue
        end if
      end if
      iter = iter+1
      converged = (abs(p_spec-pOld)<(releps_p*pOld+1e-6) .and. abs(rho-rhoOld)<releps_rho*rhoOld)
      if ( converged ) then
        exit
      else if ( iter == maxiter ) then
        if (.not. continueOnError) then
          if (present(ierr)) then
            ierr = 1
          else
            print *, "iter ", iter
            print *, "T_spec, P_spec, phase_spec", T_spec, P_spec, phase_spec
            print *, "rho, rhoOld ", rho, rhoOld
            print *, "p, pOld ", p, pOld
            print *, "dpdrho, dpdrhoOld ", dpdrho, dpdrhoOld
            print *, "currentPhase", currentPhase
            print *, "curvature", (rho-rhoOld)*(dpdrho-dpdrhoOld)
            call stoperror("multiparameter_eos::densitySolver: iter == max_iter.")
          end if
        end if
      end if
    end do

    if ( present(phase_found) ) then
      phase_found = currentPhase
    end if

  contains

    !> Call this routine if stability fails. It switches the phases, and
    !> initializes a new search for a root in the other phase; iter is NOT reset
    !> to zero.
    subroutine switchPhase()
      if ( currentPhase == VAPPH ) then
        currentPhase = LIQPH
      else
        currentPhase = VAPPH
      end if
      nPhaseSwitches = nPhaseSwitches + 1
      call initializeSearch()
    end subroutine switchPhase

    !> This routine computes initial rho and dpdrho, as well as setting parameters
    !> for the stability test (pMin, dpdrhoMin, curvatureSign).
    subroutine initializeSearch ()
      real :: tau
      tau = this%tc/T_spec
      converged = .false.

      if ( t_spec > this%tc ) then ! Supercritical fluid. Start at ideal gas density.
        curvatureSign = 0
        rho = p_spec/(T_spec*this%Rgas_meos)
        call this%mp_pressure(rho, T_spec, p, p_rho=dpdrho)

      else if( currentPhase == VAPPH) then ! Subcritical vapor. Start at ideal gas density.
        curvatureSign = -1
        rho = p_spec/(T_spec*this%Rgas_meos)
        call this%mp_pressure(rho, T_spec, p, p_rho=dpdrho)
      else ! Subcritical liquid. Start at saturated liquid density, with five percent margin.
        curvatureSign = 1
        rho = (this%rc)*this%satDeltaEstimate(tau=tau, phase=currentPhase) * 1.01 ! changed to 1% Geir S (liquid molar density is in the range 12-15000)
        ! the p-rho-curve is quite vertical so 5% off the estimated liquid
        ! root seem a bit excessive
        call this%mp_pressure(rho, T_spec, p, p_rho=dpdrho)

        do while (p<0 .or. dpdrho<0) ! Should only kick in at extremely low temperatures.
          rho = 2*rho
          curvatureSign = 0
          call this%mp_pressure(rho, T_spec, p, p_rho=dpdrho)
        end do
      end if

      ! Have we switched phases two times? Then we're back at the original phase.
      ! This should only happen at extremely low temperatures where the eos is
      ! unphysical. In this case, turn off all robustness measures, and just
      ! try to find a root.
      if ( nPhaseSwitches == 2 ) then
        if ( t_spec > this%t_triple ) then
          if (.not. continueOnError) then
            print *, "T_spec, T_triple", t_spec, this%t_triple
            call stoperror("multiparameter_eos::densitySolver failed at T < T_triple")
          end if
        end if

        curvatureSign = 0
        pMin = -1e100
        dpdrhoMin = -1e100
      end if

    end subroutine initializeSearch

  end subroutine densitySolver

  !> Pressure and (optionally) its derivatives.
  subroutine mp_pressure (this, rho, T, p, p_rho, p_T)
    class(meos) :: this
    real, intent(in) :: rho, T
    real, intent(out) :: p
    real, intent(out), optional :: p_rho, p_T

    ! Internals
    real :: delta, tau
    real :: alpr(0:2,0:2)

    delta = rho/this%rc
    tau = this%tc/T

    call this%alphaResDerivs_taudelta(delta,tau,alpr)
    p = rho*this%Rgas_meos*T*(1+alpr(1,0))

    if ( present(p_rho) ) then
      p_rho = this%Rgas_meos*T*(1 + 2*alpr(1,0) + alpr(2,0))
    end if

    if ( present(p_T) ) then
      p_T = this%Rgas_meos*rho*(1 + alpr(1,0) - alpr(1,1))
    end if
  end subroutine mp_pressure

  subroutine assign_meos_base(this,other)
    class(meos), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos)
      this%compName = other%compName
      this%tc = other%tc
      this%pc = other%pc
      this%rc = other%rc
      this%t_triple = other%t_triple
      this%p_triple = other%p_triple
      this%rhoLiq_triple = other%rhoLiq_triple
      this%rhoVap_triple = other%rhoVap_triple
      this%molarMass = other%molarMass
      this%maxT = other%maxT
      this%maxP = other%maxP
      this%Rgas_meos = other%Rgas_meos
      this%Rgas_fit = other%Rgas_fit
    end select
  end subroutine assign_meos_base

  subroutine get_ref_state_spec_default(this, ref_state, T, P, phase, solve)
    class(meos) :: this
    character(len=*), intent(in) :: ref_state
    real, intent(out) :: T, P
    integer, intent(out) :: phase
    integer, intent(out) :: solve
    !
    T = 0
    P = 0
    phase = 1
    solve = REF_NO_SOLVE
  end subroutine get_ref_state_spec_default

  subroutine set_ref_state_default(this, T, P, v, h, s)
    class(meos) :: this
    real, intent(in) :: T, P, v, h, s
    ! Already ddefined properly.....
  end subroutine set_ref_state_default

end module multiparameter_base
