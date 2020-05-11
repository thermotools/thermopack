module multiparameter_ortho_h2
  !> Orthohydrogen multiparameter fundamental equation for Helmholtz energy. See
  !> doi:10.1063/1.3160306.
  use multiparameter_base, only: meos
  implicit none
  save
  public :: meos_ortho_h2
  private

  ! Parameters for the ideal gas part alpha^0
  real, parameter, dimension(1:2) :: a = (/  -1.4675442336,  1.8845068862 /)
  real, parameter, dimension(3:6) :: b = (/ -25.7676098736,&
       -43.4677904877,&
       -66.0445514750,&
       -209.7531607465/)

  real, parameter, dimension(3:6) :: v = (/ 2.54151,&
       -2.3661,&
       1.00365,&
       1.22447/)

  ! upPol is the upper index for polynomial terms, upExp the same for
  ! single-expontential terms, upExpExp for double-exponential terms.
  integer, parameter :: upPol = 7
  integer, parameter :: upExp = 9
  integer, parameter :: upExpExp = 14

  real, parameter, dimension(1:upPol) :: N_pol = (/ -6.83148,&
       0.01,&
       2.11505,&
       4.38353,&
       0.211292,&
       -1.00939,&
       0.142086 /)

  real, parameter, dimension(upPol+1:upExp) :: N_exp =(/ -0.87696,  0.804927 /)

  real, parameter, dimension(upExp+1:upExpExp) :: N_expexp = (/ -0.710775,&
       0.0639688,&
       0.0710858,&
       -0.087654,&
       0.647088 /)

  real, parameter, dimension(1:upPol) :: t_pol = (/ 0.7333,&
       1.0,&
       1.1372,&
       0.5136,&
       0.5638,&
       1.6248,&
       1.829&
       /)

  real, parameter, dimension(upPol+1:upExp) :: t_exp = (/  2.404,2.105  /)

  real, parameter, dimension(upExp+1:upExpExp) :: t_expexp = (/  4.1,&
       7.658,&
       1.259,&
       7.589,&
       3.946&
       /)

  integer, parameter, dimension(1:upPol) :: d_pol = (/ 1, 4, 1, 1, 2, 2, 3 /)
  integer, parameter, dimension(upPol+1:upExp) :: d_exp = (/1,3/)
  integer, parameter, dimension(upExp+1:upExpExp) :: d_expexp = (/ 2, 1, 3, 1, 1 /)

  integer, parameter, dimension(upPol+1:upExp) :: l_exp = (/1,1/)

  real, parameter, dimension(upExp+1:upExpExp) :: eta_expexp = (/&
       1.169,&
       0.894,&
       0.04,&
       2.072,&
       1.306/)

  real, parameter, dimension(upExp+1:upExpExp) :: beta_expexp = (/&
       0.4555,&
       0.4046,&
       0.0869,&
       0.4415,&
       0.5743  /)

  real, parameter, dimension(upExp+1:upExpExp) :: gam_expexp = (/&
       1.5444,&
       0.6627,&
       0.763,&
       0.6587,&
       1.4327 /)

  real, parameter, dimension(upExp+1:upExpExp) :: eps_expexp = (/&
       0.6366,&
       0.3876,&
       0.9437,&
       0.3976,&
       0.9626 /)

  type, extends(meos) :: meos_ortho_h2
  private
  ! Cache the expensive temperature-dependent quantities.
  real :: tau_cache
  real :: deltaSatLiq_cache
  real :: deltaSatVap_cache
  real :: prefactors_pol_cache(1:upPol)
  real :: prefactors_exp_cache(upPol+1:upExp)
  real :: prefactors_expexp_cache(upExp+1:upExpExp)

contains

  procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_ORTHO_H2
  procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_ORTHO_H2
  procedure, public :: satDeltaEstimate => satDeltaEstimate_ORTHO_H2
  procedure, public :: init => init_ORTHO_H2

  procedure, private :: alphaResPrefactors => alphaResPrefactors_ORTHO_H2

end type meos_ortho_h2

contains

subroutine init_ortho_h2 (this)
 class(meos_ortho_h2) :: this

 this%compName = "ortho_h2"
 this%tau_cache = 0.0

 this%molarMass = 2.01594e-3 !< (kg/mol) (Not stated in paper. From NIST)
 this%Rgas_fit = 8.314472 !< (J/(mol*K))

 this%maxT = 1000.0 ! (T)
 this%maxP = 2000e6 ! (Pa)

 this%tc = 33.22     !< (K)
 this%pc = 1.31065e6 !< (Pa)
 this%rc = 15.445e3  !< (mol/m^3)

 this%t_triple = 14.008      !< (K)
 this%p_triple = 7461.0      !< (Pa)
 this%rhoLiq_triple = 38.2e3 !< (mol/m^3)
 this%rhoVap_triple = 0.12985/this%molarMass !< (mol/m^3)

end subroutine init_ortho_h2

! The functional form of the ideal gas function varies among multiparameter EoS,
! which explains why this routine may seem a bit hard-coded.
subroutine alpha0Derivs_ortho_h2(this, delta, tau, alp0)
 class(meos_ortho_h2) :: this
 real, intent(in) :: delta, tau
 real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
 ! Internals
 real, dimension(3:6) :: exps

 exps = exp(b*tau)

 alp0 = 0.0
 alp0(0,0) = log(delta) + 1.5*log(tau) + a(1) + a(2)*tau + dot_product(v, log(1-exps))
 alp0(1,0) = 1.0
 alp0(2,0) = -1.0
 alp0(0,1) = 1.5 + a(2)*tau + tau*dot_product(v*b, exps/(exps-1))
 alp0(0,2) = -1.5 - tau*tau*dot_product(v*b**2, exps/(exps-1)**2)

end subroutine alpha0Derivs_ortho_h2

! Supplies all prefactors that do not depend on delta. Prefactors are cached.
subroutine alphaResPrefactors_ORTHO_H2 (this, tau, prefactors_pol, prefactors_exp, prefactors_expexp)
 class(meos_ortho_h2) :: this
 real, intent(in) :: tau
 real, intent(out) :: prefactors_pol(1:upPol)
 real, intent(out) :: prefactors_exp(upPol+1:upExp)
 real, intent(out) :: prefactors_expexp(upExp+1:upExpExp)

 if ( tau /= this%tau_cache ) then
   this%tau_cache = tau
   this%prefactors_pol_cache = N_pol * tau**t_pol
   this%prefactors_exp_cache = N_exp * tau**t_exp
   this%prefactors_expexp_cache = N_expexp * tau**t_expexp
 end if

 prefactors_pol = this%prefactors_pol_cache
 prefactors_exp = this%prefactors_exp_cache
 prefactors_expexp = this%prefactors_expexp_cache

end subroutine alphaResPrefactors_ORTHO_H2


subroutine alphaResDerivs_ORTHO_H2 (this, delta, tau, alpr)
 class(meos_ortho_h2) :: this
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

end subroutine alphaResDerivs_ORTHO_H2

function satDeltaEstimate_ortho_h2 (this,tau,phase) result(deltaSat)
 use parameters, only: LIQPH, VAPPH
 class(meos_ortho_h2) :: this
 real, intent(in) :: tau
 integer, intent(in) :: phase
 real :: deltaSat

 if ( phase == LIQPH .or. phase == 4) then
! if ( phase == LIQPH) then
   deltaSat = this%rhoLiq_triple/this%rc
 else if ( phase == VAPPH) then
   deltaSat = this%rhoVap_triple/this%rc
 else
   call stoperror("satDeltaEstimate_ortho_h2: only LIQPH and VAPPH allowed!")
 end if

end function satDeltaEstimate_ORTHO_H2

end module multiparameter_ORTHO_H2
