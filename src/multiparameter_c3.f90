module multiparameter_C3
  !> Propane multiparameter fundamental equation for Helmholtz
  !> energy. See doi:10.1021/je900217v.
  use multiparameter_base, only: meos
  implicit none
  save
  public :: meos_c3
  private

  ! Parameters for the ideal gas part alpha^0
  real, parameter, dimension(1:2) :: a = (/ -4.970583,  4.29352 /)
  real, parameter, dimension(3:6) :: b = (/ 1.062478, 3.344237, 5.363757, 11.762957 /)
  real, parameter, dimension(3:6) :: v = (/ 3.043, 5.874, 9.337, 7.922 /)

!  real, parameter, dimension(3:6) :: bsq = b*b

  ! upPol is the upper index for polynomial terms, upExp the same for
  ! single-expontential terms, upExpExp for double-exponential terms.
  integer, parameter :: upPol = 5
  integer, parameter :: upExp = 11
  integer, parameter :: upExpExp = 18

  real, parameter, dimension(1:upPol) :: N_pol = (/0.042910051, 1.7313671, -2.4516524,&
       0.34157466, -0.46047898 /)
  real, parameter, dimension(upPol+1:upExp) :: N_exp =(/ -0.66847295, 0.20889705, &
       0.19421381, -0.22917851, -0.60405866, 0.066680654 /)
  real, parameter, dimension(upExp+1:upExpExp) :: N_expexp = (/ 0.017534618, 0.33874242, &
       0.22228777, -0.23219062, -0.092206940, -0.47575718, -0.017486824 /)

  real, parameter, dimension(1:upPol) :: t_pol = (/1.00, 0.33, 0.80, 0.43, 0.90 /)
  real, parameter, dimension(upPol+1:upExp) :: t_exp = (/2.46, 2.09, 0.88, 1.09, 3.25, 4.62 /)
  real, parameter, dimension(upExp+1:upExpExp) :: t_expexp = (/0.76, 2.50, 2.75, 3.05, 2.55, 8.40, 6.75 /)

  integer, parameter, dimension(1:upPol) :: d_pol = (/ 4, 1, 1, 2, 2/)
  integer, parameter, dimension(upPol+1:upExp) :: d_exp = (/ 1, 3, 6, 6, 2, 3 /)
  integer, parameter, dimension(upExp+1:upExpExp) :: d_expexp = (/ 1, 1, 1, 2, 2, 4, 1 /)

  integer, parameter, dimension(upPol+1:upExp) :: l_exp = (/ 1, 1, 1, 1, 2, 2 /)
  real, parameter, dimension(upExp+1:upExpExp) :: eta_expexp = (/ 0.963, 1.977, 1.917, 2.307, 2.546, 3.28, 14.6 /)
  real, parameter, dimension(upExp+1:upExpExp) :: beta_expexp = (/ 2.33, 3.47, 3.15, 3.19, 0.92, 18.8, 547.8 /)
  real, parameter, dimension(upExp+1:upExpExp) :: gam_expexp = (/ 0.684, 0.829, 1.419, 0.817, 1.500, 1.426, 1.093 /)
  real, parameter, dimension(upExp+1:upExpExp) :: eps_expexp = (/ 1.283, 0.6936, 0.788, 0.473, 0.8577, 0.271, 0.948 /)

  ! Parameters for the ancillary equations to compute saturation densities
  real, parameter, dimension(4) :: N_liqsat = (/ 1.82205, 0.65802, 0.21109, 0.083973 /)
  real, parameter, dimension(4) :: expo_liqsat = (/ 0.345, 0.74, 2.6, 7.2 /)
  real, parameter, dimension(6) :: N_vapsat = (/ -2.4887, -5.1069, -12.174, -30.495, -52.192, -134.89 /)
  real, parameter, dimension(6) :: expo_vapsat = (/ 0.3785, 1.07, 2.7, 5.5, 10.0, 20.0/)

  !> C3 multiparameter equations of state (Lemmon, McLinden and Wagner 2009).
  type, extends(meos) :: meos_c3
     private
     ! Cache the expensive temperature-dependent quantities.
     real :: tau_cache
     real :: deltaSatLiq_cache
     real :: deltaSatVap_cache
     real :: prefactors_pol_cache(1:upPol)
     real :: prefactors_exp_cache(upPol+1:upExp)
     real :: prefactors_expexp_cache(upExp+1:upExpExp)

   contains

     procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_C3
     procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_C3
     procedure, public :: satDeltaEstimate => satDeltaEstimate_C3
     procedure, public :: init => init_C3

     procedure, private :: alphaResPrefactors => alphaResPrefactors_C3

  end type meos_c3
  
contains

  subroutine init_C3 (this)
    class(meos_c3) :: this

    this%tau_cache = 0.0

    this%compName = "C3"
    this%tc = 369.89  !< (K)
    this%rc = 5000    !< (mol/m^3)
    this%pc = 4251200 !< (Pa)

    this%t_triple = 85.525  !< (K)
    this%p_triple = 0.00017 !< (Pa)
    this%rhoLiq_triple = 16625  !< (mol/m^3)
    this%rhoVap_triple = 2.4e-7  !< (mol/m^3)

    this%molarMass = 44.09562e-3  !< (kg/mol)
    this%Rgas_fit = 8.314472 !< (J/(mol*K))

    this%maxT = 650.0 ! (T)
    this%maxP = 1000e6 ! (Pa)

  end subroutine init_C3

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  subroutine alpha0Derivs_C3(this, delta, tau, alp0)
    class(meos_c3) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    ! Internals
    real, dimension(3:6) :: exps !, exps2
    ! Precalculate exponentials
    exps = exp(b*tau)
!    exps2 = exps/(exps-1)
!    exps2 = exps2*exps2

    alp0 = 0.0
    alp0(0,0) = log(delta) + 3*log(tau) + a(1) + a(2)*tau + dot_product(v, log(1-1/exps))
    alp0(1,0) = 1.0
    alp0(2,0) = -1.0
    alp0(0,1) = 3 + a(2)*tau + tau*dot_product(v*b, 1/(exps-1))
    alp0(0,2) = -3 - tau*tau*dot_product(v*b**2, exps/(exps-1)**2)
!    alp0(0,2) = -3 - tau*tau*dot_product(v*bsq, exps2)

  end subroutine alpha0Derivs_C3

  ! Supplies all prefactors that do not depend on delta. Prefactors are cached.
  subroutine alphaResPrefactors_C3 (this, tau, prefactors_pol, prefactors_exp, prefactors_expexp)
    class(meos_c3) :: this
    real, intent(in) :: tau
    real, intent(out) :: prefactors_pol(1:upPol)
    real, intent(out) :: prefactors_exp(upPol+1:upExp)
    real, intent(out) :: prefactors_expexp(upExp+1:upExpExp)
    ! Internal

    if ( tau /= this%tau_cache ) then
      this%tau_cache = tau
      this%prefactors_pol_cache = N_pol * tau**t_pol
      this%prefactors_exp_cache = N_exp * tau**t_exp
      this%prefactors_expexp_cache = N_expexp * tau**t_expexp
    end if

    prefactors_pol = this%prefactors_pol_cache
    prefactors_exp = this%prefactors_exp_cache
    prefactors_expexp = this%prefactors_expexp_cache

  end subroutine alphaResPrefactors_C3


  subroutine alphaResDerivs_C3 (this, delta, tau, alpr)
    class(meos_c3) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    ! Internal
    real :: deltaL(upPol+1:upExp)
    real :: prefactors_pol(1:upPol)
    real :: prefactors_exp(upPol+1:upExp)
    real :: prefactors_expexp(upExp+1:upExpExp)
    real :: polTerms(1:upPol), expTerms(upPol+1:upExp), expExpTerms(upExp+1:upExpExp)
    call this%alphaResPrefactors(tau, prefactors_pol, prefactors_exp, prefactors_expexp)

    ! Precalculate polynomial terms
    polTerms(1:upPol) = prefactors_pol*delta**d_pol

    ! Precalculate single-exponential terms
    deltaL(upPol+1:upExp) = delta**l_exp
    expTerms(upPol+1:upExp) = prefactors_exp * delta**d_exp * exp(-deltaL)

    ! Precalculate double-exponential terms
    expExpTerms(upExp+1:upExpExp) = prefactors_expexp * delta**d_expexp &
         * exp(-eta_expexp*(delta-eps_expexp)**2 - beta_expexp*(tau-gam_expexp)**2)

    ! alpha
    alpr(0,0) = sum(polTerms) + sum(expTerms) + sum(expExpTerms)

    ! delta*alpha_delta
    alpr(1,0) = dot_product(polTerms, d_pol) + dot_product(expTerms, d_exp - l_exp*deltaL) &
         + dot_product(expExpTerms, d_expexp - 2*eta_expexp*delta*(delta-eps_expexp))

    ! delta**2 * alpha_deltadelta
    alpr(2,0) = dot_product(polTerms, d_pol*(d_pol-1)) + &
         dot_product(expTerms, (d_exp - l_exp*deltaL)*(d_exp-1-l_exp*deltaL) - l_exp*l_exp*deltaL ) + &
         dot_product(expExpTerms, (d_expexp - 2*eta_expexp*delta*(delta-eps_expexp))**2 - d_expexp - 2*eta_expexp*delta**2 )

    ! tau*alpha_tau
    alpr(0,1) = dot_product(polTerms, t_pol) + dot_product(expTerms, t_exp) &
         + dot_product(expExpTerms, t_expexp - 2*beta_expexp*tau*(tau-gam_expexp))

    ! tau**2 * alpha_tautau
    alpr(0,2) = dot_product(polTerms, t_pol*(t_pol-1)) + dot_product(expTerms, t_exp*(t_exp-1)) &
         + dot_product(expExpTerms, (t_expexp - 2*beta_expexp*tau*(tau-gam_expexp))**2 - t_expexp - 2*beta_expexp*tau**2 )

    ! delta*tau*alpha_deltatau
    alpr(1,1) = dot_product(polTerms, d_pol*t_pol) + dot_product(expTerms, (d_exp - l_exp*deltaL)*t_exp ) &
         + dot_product(expExpTerms, &
         (d_expexp - 2*eta_expexp*delta*(delta-eps_expexp))*(t_expexp - 2*beta_expexp*tau*(tau-gam_expexp)))

  end subroutine alphaResDerivs_C3

  function satDeltaEstimate_C3 (this,tau,phase) result(deltaSat)
    use parameters, only: LIQPH, VAPPH
    class(meos_c3) :: this
    real, intent(in) :: tau
    integer, intent(in) :: phase
    real :: deltaSat
    ! Internals
    real :: theta

    theta = 1-1/tau
    if ( phase == LIQPH ) then
      deltaSat = 1 + dot_product(N_liqsat,theta**expo_liqsat)
    else if ( phase == VAPPH ) then
      deltaSat = exp(dot_product(N_vapsat,theta**expo_vapsat))
    else
      call stoperror("satDeltaEstimate_C3: only LIQPH and VAPPH allowed!")
    end if

  end function satDeltaEstimate_C3

end module multiparameter_C3
