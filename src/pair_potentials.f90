module pair_potentials
  use hyperdual_mod
  use numconstants, only: PI
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
  use eosdata, only: eosMie_UV_WCA, eosMie_UV_BH

  !> Base class for pair potential
  type, abstract :: pair_potential
     type(hyperdual) :: sigma       !< length scale [m]
     type(hyperdual) :: epsdivk     !< energy scale divided by kB [K]
   contains
     procedure(calc_pot_intf), public, deferred  :: calc
     procedure(calc_potderivs_intf), public, deferred  :: calc_r_derivs
     procedure(alpha_x_intf), public, deferred   :: alpha_x
     procedure, public :: calc_bh_diameter
     procedure, public :: B2 => calc_B2_by_quadrature
     procedure(display_intf), public, deferred :: display
  end type pair_potential

  abstract interface
     function calc_pot_intf (this, r) result(value)
       use hyperdual_mod, only: hyperdual
       import pair_potential
       class(pair_potential), intent(in) :: this
       type(hyperdual), intent(in) :: r !< Separation (m)
       type(hyperdual) :: value         !< Potential value (J)
     end function calc_pot_intf
  end interface

  abstract interface
     subroutine calc_potderivs_intf (this, r, pot, pot_r, pot_rr, pot_rrr)
       use hyperdual_mod, only: hyperdual
       import pair_potential
       class(pair_potential), intent(in) :: this
       type(hyperdual), intent(in) :: r                  !< Separation (m)
       type(hyperdual), intent(out) :: pot               !< Potential value (K)
       type(hyperdual), intent(out), optional :: pot_r   !< 1st derivative of potential wrt separation (K/m)
       type(hyperdual), intent(out), optional :: pot_rr  !< 2nd derivative of potential wrt separation (K/m^2)
       type(hyperdual), intent(out), optional :: pot_rrr !< 3rd derivative of potential wrt separation (K/m^3)
     end subroutine calc_potderivs_intf
  end interface

  abstract interface
     type(hyperdual) function alpha_x_intf(this, x, adim) result(alpha_x)
       !> alpha_x = -\int_x^\infty pot(x)*x^2 dx (everything adimensional)
       use hyperdual_mod, only: hyperdual
       import pair_potential
       class(pair_potential), intent(in) :: this !< potential
       type(hyperdual), intent(in) :: x          !< lower integration limit (-)
       logical, intent(in), optional :: adim     !< adimensional value for alpha, or in ((J/K)*m^3)?
     end function alpha_x_intf
  end interface

  abstract interface
     subroutine display_intf (this)
       use hyperdual_mod, only: hyperdual
       import pair_potential
       class(pair_potential), intent(in) :: this
     end subroutine display_intf
  end interface

  type, extends(pair_potential) :: mie_potential_hd
     type(hyperdual) :: lamr        !< repulsive exponent [-]
     type(hyperdual) :: lama        !< attractive exponent [-]
     type(hyperdual) :: Cmie        !< Mie potential prefactor [-]
     type(hyperdual) :: rmin        !< location of minimum [m]
     type(hyperdual) :: rmin_adim   !< rmin/sigma [-]
     type(hyperdual) :: alpha       !< adimensional van der waals energy [-]
   contains
     procedure, public :: init => mie_potential_hd_init
     procedure, public :: calc => mie_potential_hd_calc
     procedure, public :: calc_r_derivs => mie_potential_hd_calc_r_derivs
     procedure, public :: alpha_x => mie_potential_hd_calc_alpha_x
     procedure, public :: display => mie_potential_hd_display
  end type mie_potential_hd

  type, extends(pair_potential) :: sutherlandsum
     !> u(r) = sum_i C(i)*eps*(sigma/r)**lam(i)
     integer         :: nt                  !< Number of Sutherland terms [-]
     real, allocatable :: Cconst(:)         !< Coefficients of terms [-]
     type(hyperdual), allocatable :: C(:)   !< Coefficients of terms [-]
     type(hyperdual), allocatable :: lam(:) !< Exponents [-]
     type(hyperdual) :: rmin        !< location of minimum [m]
     type(hyperdual) :: rmin_adim   !< rmin/sigma [-]
     integer, allocatable :: bexp(:)!< beta exponents
     logical :: beta_dependence     !< whether bexp!=0
   contains
     procedure, public :: init => sutherlandsum_init
     procedure, public :: calc => sutherlandsum_calc
     procedure, public :: calc_r_derivs => sutherlandsum_calc_r_derivs
     procedure, public :: alpha_x => sutherlandsum_calc_alpha_x
     procedure, public :: display => sutherlandsum_display
     procedure, public :: dealloc => sutherlandsum_dealloc
     procedure, public :: update_beta => sutherlandsum_update_beta
  end type sutherlandsum


  !> PURE COMPONENT PARAMETERS.
  ! ---------------------------------------------------------------------------
  type :: mie_data
     integer :: eosidx
     character(len=uid_len) :: compName
     ! Pure component parameters.
     real :: lamr           !< [-] Repulsive exponent
     real :: sigma          !< [m] Temperature-independent segment diameter
     real :: eps_divk !< [K] Well depth divided by k_B
     ! Parameter set
     character(len=ref_len) :: ref
  end type mie_data

  integer, parameter :: nMie = 7
  type(mie_data), dimension(nMie), parameter :: MieArray = (/ &
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="AR", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
                                !
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="AR", &
       lamr=12.26, &
       sigma=3.41E-10, &
       eps_divk=118.7, &
       ref="SVRMIE"), &
                                !
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="C1", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
                                !
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="NE", &
       lamr=24, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
                                !
       mie_data(eosidx = eosMie_UV_BH, &
       compName="AR", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
                                !
       mie_data(eosidx = eosMie_UV_BH, &
       compName="AR", &
       lamr=12.26, &
       sigma=3.41E-10, &
       eps_divk=118.7, &
       ref="SVRMIE"), &
                                !
       mie_data(eosidx = eosMie_UV_BH, &
       compName="C1", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT")/)

contains

  function approx_lamr_from_alpha(alpha) result(lamr)
    !> Find the repulsive exponent lamr such that Mie(lamr,6) has the
    !> specified alpha. Maximum deviation for alpha \in [0.520, 1.125]
    !> is 0.006. This corresponds to lamr \in [9, 36].
    type(hyperdual), intent(in) :: alpha !< vdw energy (-)
    type(hyperdual)             :: lamr  !< repulsive exponent (-)
    ! Locals
    real, parameter :: A = 18.864
    real, parameter :: B = 4.541
    real, parameter :: C = 0.382

    lamr = 2*A/(-B + sqrt(B**2-4*A*(C-alpha)))
  end function approx_lamr_from_alpha


  !> Get the index in the MieArray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getMiedataIdx(eosidx,compName,ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, ref
    integer :: idx, idx_default
    logical :: found

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nMie)
       if ((eosidx == Miearray(idx)%eosidx) .and. &
            str_eq(compName, Miearray(idx)%compName)) then
          if (string_match(ref,Miearray(idx)%ref)) then
             found = .true.
             exit
          else if (string_match("DEFAULT",Miearray(idx)%ref)) then
             idx_default = idx
          endif
       endif
       idx = idx + 1
    enddo

    if (.not. found .and. idx_default > 0) then
       idx = idx_default
       found = .true.
    endif
    if (.not. found) then
       print *, "ERROR FOR COMPONENT ", compname
       call stoperror("The Mie parameters don't exist.")
    end if
  end function getMiedataIdx


  subroutine mie_potential_hd_init(this, lama, lamr, sigma, epsdivk)
    class(mie_potential_hd), intent(inout) :: this
    type(hyperdual), intent(in) :: lama, lamr, sigma, epsdivk

    this%lama = lama
    this%lamr = lamr
    this%sigma = sigma
    this%epsdivk = epsdivk

    this%CMie = lamr/(lamr-lama) * (lamr/lama)**(lama/(lamr-lama))
    this%rmin_adim = (lamr/lama)**(1.0/(lamr-lama))
    this%rmin = sigma*this%rmin_adim
    this%alpha = this%CMie*(1.0/(lama - 3.0) - 1.0/(lamr - 3.0))
  end subroutine mie_potential_hd_init

  function mie_potential_hd_calc(this, r) result(pot)
    class(mie_potential_hd), intent(in) :: this
    type(hyperdual), intent(in) :: r !< Separation (m)
    type(hyperdual) :: pot           !< Potential value (J)

    pot = this%Cmie * this%epsdivk * ( (this%sigma/r)**this%lamr - (this%sigma/r)**this%lama )
  end function mie_potential_hd_calc

  subroutine mie_potential_hd_calc_r_derivs(this, r, pot, pot_r, pot_rr, pot_rrr)
    class(mie_potential_hd), intent(in) :: this
    type(hyperdual), intent(in) :: r                  !< Separation (m)
    type(hyperdual), intent(out) :: pot               !< Potential value (K)
    type(hyperdual), intent(out), optional :: pot_r   !< 1st derivative of potential wrt separation (K/m)
    type(hyperdual), intent(out), optional :: pot_rr  !< 2nd derivative of potential wrt separation (K/m^2)
    type(hyperdual), intent(out), optional :: pot_rrr !< 3rd derivative of potential wrt separation (K//m^3)
    ! Locals
    type(hyperdual) :: prefac, ur, urr, rep_term, att_term
    pot = 0.0
    ur = 0.0
    urr = 0.0

    rep_term = this%Cmie * this%epsdivk*(this%sigma/r)**this%lamr
    att_term =-this%Cmie * this%epsdivk*(this%sigma/r)**this%lama
    pot = rep_term + att_term

    if (present(pot_r)) then
       pot_r = -(rep_term * this%lamr + att_term * this%lama)/r
    end if
    if (present(pot_rr)) then
       pot_rr = (rep_term * this%lamr*(this%lamr+1.0) + att_term* this%lama*(this%lama+1.0))/r**2
    end if
    if (present(pot_rrr)) then
       pot_rrr = -(rep_term * this%lamr*(this%lamr+1.0)*(this%lamr+2.0) &
            + att_term* this%lama*(this%lama+1.0)*(this%lama+2.0)) / r**3
    end if

  end subroutine mie_potential_hd_calc_r_derivs


  type(hyperdual) function mie_potential_hd_calc_alpha_x(this, x, adim) result(alpha_x)
    !> alpha_x = -\int_x^\infty pot(x)*x^2 dx (everything adimensional)
    class(mie_potential_hd), intent(in) :: this !< This potential
    type(hyperdual), intent(in) :: x           !< lower integration limit (-)
    logical, intent(in), optional :: adim      !< adimensional value for alpha, or in (K*m^3)?
    ! Locals
    type(hyperdual) :: n, nu
    n = this%lama
    nu = this%lamr
    alpha_x = this%cmie * (x**(3.0-n)/(n-3.0) - x**(3.0-nu)/(nu-3.0))
    if (present(adim)) then
       if (.not. adim) then
          alpha_x = alpha_x * this%epsdivk * this%sigma**3
       end if
    end if
  end function mie_potential_hd_calc_alpha_x

  subroutine mie_potential_hd_display(this)
    !> Print potential parameters
    class(mie_potential_hd), intent(in) :: this !< This potential
    ! Locals
    WRITE ( unit=* , fmt='(a)') '-------------------------------------------------------------------------------------'
    WRITE(unit=*,fmt='(a)')  'Mie potential'
    WRITE(unit=*,fmt='(5a17)')  'C', 'lambda_r', 'lambda_a', 'epsdivk', 'sigma'
    WRITE(unit=*,fmt='(4f17.8,es17.8)') this%Cmie%f0, this%lamr%f0, this%lama%f0, this%epsdivk%f0, this%sigma%f0
    WRITE ( unit=* , fmt='(a)') '-------------------------------------------------------------------------------------'

  end subroutine mie_potential_hd_display

  subroutine sutherlandsum_init(this, nt, C, lam, sigma, epsdivk, beta_expo)
    class(sutherlandsum), intent(inout) :: this
    integer, intent(in)         :: nt          !< Number of terms (-)
    type(hyperdual), intent(in) :: C(nt)       !< Coefficients (-)
    type(hyperdual), intent(in) :: lam(nt)     !< Exponents (-)
    type(hyperdual), intent(in) :: sigma       !< Diameter (m)
    type(hyperdual), intent(in) :: epsdivk     !< Energy (K)
    integer, intent(in), optional :: beta_expo(nt)  !< Energy (K)
    ! Locals
    integer :: stat

    this%nt = nt
    this%sigma = sigma
    this%epsdivk = epsdivk

    call sutherlandsum_dealloc(this)

    allocate(this%C(nt))
    this%C = C%f0

    allocate(this%lam(nt))
    this%lam = lam

    allocate(this%bexp(nt))
    if (present(beta_expo)) then
       this%beta_dependence = .true.
       allocate(this%bexp(nt))
       allocate(this%Cconst(nt))
       this%bexp = beta_expo
       this%Cconst = C%f0
    else
       this%beta_dependence = .false.
       this%bexp = 0
    end if

  end subroutine sutherlandsum_init

  subroutine sutherlandsum_dealloc(this)
    class(sutherlandsum), intent(inout) :: this
    ! Locals
    integer :: stat

    if (allocated(this%C)) then
       deallocate(this%C,stat=stat)
       if (stat /= 0) call stoperror("Unable to deallocate Sutherland coefficients")
    end if

    if (allocated(this%Cconst)) then
       deallocate(this%Cconst,stat=stat)
       if (stat /= 0) call stoperror("Unable to deallocate Sutherland coefficients Cconst")
    end if

    if (allocated(this%lam)) then
       deallocate(this%lam,stat=stat)
       if (stat /= 0) call stoperror("Unable to deallocate Sutherland exponents")
    end if

    if (allocated(this%bexp)) then
       deallocate(this%bexp,stat=stat)
       if (stat /= 0) call stoperror("Unable to deallocate beta exponents")
    end if

  end subroutine sutherlandsum_dealloc

  function sutherlandsum_calc(this, r) result(pot)
    class(sutherlandsum), intent(in) :: this
    type(hyperdual), intent(in) :: r !< Separation (m)
    type(hyperdual) :: pot           !< Potential value (J)
    ! Locals
    integer :: i

    pot = 0.0
    do i=1,this%nt
       pot = pot + this%C(i) * this%epsdivk*(this%sigma/r)**this%lam(i)
    end do
  end function sutherlandsum_calc


  subroutine sutherlandsum_calc_r_derivs(this, r, pot, pot_r, pot_rr, pot_rrr)
    class(sutherlandsum), intent(in) :: this
    type(hyperdual), intent(in) :: r                  !< Separation (m)
    type(hyperdual), intent(out) :: pot               !< Potential value (K)
    type(hyperdual), intent(out), optional :: pot_r   !< 1st derivative of potential wrt separation (K/m)
    type(hyperdual), intent(out), optional :: pot_rr  !< 2nd derivative of potential wrt separation (K/m^2)
    type(hyperdual), intent(out), optional :: pot_rrr !< 3rd derivative of potential wrt separation (K/m^3)
    ! Locals
    integer :: i
    type(hyperdual) :: ur, urr, urrr, term
    pot = 0.0
    ur = 0.0
    urr = 0.0
    urrr = 0.0
    do i=1,this%nt
       term = this%C(i) * this%epsdivk*(this%sigma/r)**this%lam(i)
       pot = pot + term

       term = -term*this%lam(i)
       ur  = ur  + term
       term = -term*(this%lam(i)+1.0)
       urr = urr + term
       term = -term*(this%lam(i)+2.0)
       urrr = urrr + term
    end do
    if (present(pot_r)) then
       pot_r = ur/r
    end if
    if (present(pot_rr)) then
       pot_rr = urr/r**2
    end if
    if (present(pot_rrr)) then
       pot_rrr = urrr/r**3
    end if
  end subroutine sutherlandsum_calc_r_derivs

  subroutine sutherlandsum_update_beta(this, beta)
    !> Update Sutherland coefficients with coefficients possibly depending on temperature
    class(sutherlandsum), intent(inout) :: this !< This potential
    type(hyperdual), intent(in)         :: beta !< 1/T (1/K)
    ! Locals
    integer :: i

    if (.not. this%beta_dependence) return

    do i=1,this%nt
       this%C(i) = this%Cconst(i) * beta**(this%bexp(i))
    end do
  end subroutine sutherlandsum_update_beta

  type(hyperdual) function sutherlandsum_calc_alpha_x(this, x, adim) result(alpha_x)
    !> alpha_x = -\int_x^\infty pot(x)*x^2 dx (everything adimensional)
    class(sutherlandsum), intent(in) :: this !< This potential
    type(hyperdual), intent(in) :: x         !< lower integration limit (-)
    logical, intent(in), optional :: adim    !< adimensional value for alpha, or in (K*m^3)?
    ! Locals
    integer :: k
    type(hyperdual) :: n

    alpha_x = 0.0
    do k=1,this%nt
       n = this%lam(k)
       alpha_x = alpha_x - this%C(k) * (x**(3.0-n)/(n-3.0))
    end do
    if (present(adim)) then
       if (.not. adim) then
          alpha_x = alpha_x * this%epsdivk * this%sigma**3
       end if
    end if
  end function sutherlandsum_calc_alpha_x


  subroutine sutherlandsum_display(this)
    !> Print potential parameters
    class(sutherlandsum), intent(in) :: this !< This potential
    ! Locals
    integer :: k
    WRITE ( unit=* , fmt='(a)') '-------------------------------------------------------------------------------------'
    WRITE(unit=*,fmt='(a)')  'SutherlandSum potential'
    WRITE(unit=*,fmt='(a4, 4a18)')  'term', 'C', 'lambda', 'epsdivk', 'sigma'
    do k=1,this%nt
       WRITE(unit=*,fmt='(i4, 3f18.8,es18.8)') k, this%C(k)%f0, this%lam(k)%f0, this%epsdivk%f0, this%sigma%f0
    end do
    WRITE ( unit=* , fmt='(a)') '-------------------------------------------------------------------------------------'

  end subroutine sutherlandsum_display


  type(hyperdual) function calc_B2_by_quadrature(pot, beta) result(B2)
    !> Second virial coefficient (m^3)
    use quadratures
    class(pair_potential), intent(in)   :: pot !< Pair potential
    type(hyperdual), intent(in)           :: beta  !< 1/kT (J)
    ! Locals
    type(hyperdual) :: r, potrmin
    integer :: B2_quadrature = GAUSS_KRONROD_31 ! Modify to improve accuracy
    integer :: i, n_quad
    real :: x_vec(max_n_quadrature), w_vec(max_n_quadrature)
    real :: rmin, rmax ! lower and upper integration limits
    real :: rmid, xscale

    ! Potential minimum
    call calc_rmin_and_epseff(pot, rmin=potrmin)

    ! Get quadrature points
    call get_quadrature_positions(B2_quadrature,x_vec,n_quad)
    call get_quadrature_weights(B2_quadrature,w_vec,n_quad)

    ! Calculate lower and upper integration limits
    rmin = 0.4*pot%sigma%f0
    rmax = potrmin%f0
    rmid   = (rmax + rmin)/2
    xscale = (rmax - rmin)/2

    ! Analytic integration between 0 and rmin, where boltzmann factor
    ! is approximately zero
    B2 = (2*PI/3.0)*rmin**3

    ! Numerical integration between rmin and rmax
    do i=1,n_quad
       r = rmid + xscale*x_vec(i)
       B2 = B2 + xscale*B2_integrand(r, beta, pot)*w_vec(i)
    end do

    ! Numerical integration between rmin and rmax
    rmin = rmax
    rmax = 5*potrmin%f0
    rmid   = (rmax + rmin)/2
    xscale = (rmax - rmin)/2
    do i=1,n_quad
       r = rmid + xscale*x_vec(i)
       B2 = B2 + xscale*B2_integrand(r, beta, pot)*w_vec(i)
    end do

    ! Add analytic correction for the region rmax...infty, where
    ! exp(-beta*u(r))-1 \approx -beta u(r)
    B2 = B2 + (-2*PI) * beta*pot%alpha_x(rmax/pot%sigma)*pot%epsdivk*pot%sigma**3

  contains

    function B2_integrand(r, beta, pot) result(val)
      type(hyperdual), intent(in)         :: r    !< Separation (m)
      type(hyperdual), intent(in)         :: beta !< 1/kT (J)
      class(pair_potential), intent(in)   :: pot  !< Pair potential
      type(hyperdual)                     :: val  !< Integrand (m^2)
      val = -2.0*PI*(exp(-beta*pot%calc(r)) - 1.0) * r*r
    end function B2_integrand

  end function calc_B2_by_quadrature


  subroutine calc_bh_diameter(pot,beta,dhs)
    !> Barker-Henderson diameter
    use quadratures
    class(pair_potential), intent(in) :: pot  !< pair potential
    type(hyperdual), intent(in)         :: beta !< 1/T (1/K)
    type(hyperdual), intent(out)        :: dhs  !< BH diameter (m)
    ! Locals
    type(hyperdual) :: r, sigmaeff
    integer :: hs_diam_quadrature = GAUSS_KRONROD_31 ! Modify to improve accuracy
    integer :: i, n_quad
    real :: x_vec(max_n_quadrature), w_vec(max_n_quadrature)
    real :: rmin, rmax ! lower and upper integration limits
    real :: rmid, xscale

    ! Get quadrature points
    call get_quadrature_positions(hs_diam_quadrature,x_vec,n_quad)
    call get_quadrature_weights(hs_diam_quadrature,w_vec,n_quad)

    ! Calculate effective sigma
    sigmaeff = calc_sigmaeff(pot)

    ! Calculate lower and upper integration limits
    rmin = 0.4*sigmaeff%f0
    rmax = sigmaeff%f0
    rmid   = (rmax + rmin)/2.0
    xscale = (rmax - rmin)/2.0

    ! Get quadrature points
    call get_quadrature_positions(hs_diam_quadrature,x_vec,n_quad)
    call get_quadrature_weights(hs_diam_quadrature,w_vec,n_quad)

    ! Analytic integration between 0 and rmin, where boltzmann factor
    ! is approximately zero
    dhs = rmin

    ! Numerical integration between rmin and rmax
    r = 0.0
    do i=1,n_quad
       r = rmid + xscale*x_vec(i)
       dhs = dhs + xscale*(1.0 - exp(-beta*pot%calc(r)))*w_vec(i)
    end do

  end subroutine calc_bh_diameter


  function calc_sigmaeff(pot) result(sigmaeff)
    !> Calculate effective sigma, defined as the position where the
    !> interaction potential equals zero.
    use nonlinear_solvers, only: nonlinear_solver, newton_secondorder_singlevar
    class(pair_potential), intent(in) :: pot      !< Pair potential
    type(hyperdual)                   :: sigmaeff !< Effective diameter (m)
    ! Locals
    type(nonlinear_solver) :: solver
    type(hyperdual) :: pot0, dpot0, d2pot0
    real :: sigma_scaled, xinit, xmin, xmax, param(1)

    ! Set the limits and the initial condition
    param(1) = pot%sigma%f0
    xinit = 0.99
    xmin = 0.001
    xmax = 10
    sigma_scaled = 1.0
    solver%rel_tol = 1e-12
    solver%abs_tol = 1e-12

    ! Call solver
    call newton_secondorder_singlevar(resid_with_derivs,xinit,xmin,xmax,solver,sigma_scaled,param)
    if (solver%exitflag /= 0) then
       call stoperror("Not able to solve for effective sigma")
    endif

    ! Set real component of sigmaeff
    sigmaeff = 0.0
    sigmaeff%f0 = sigma_scaled*pot%sigma%f0

    ! Calculate the remaining hyperdual components, obtained by
    ! expanding u(r=sigma,beta) in sigma for "fixed" beta
    call pot%calc_r_derivs(sigmaeff, pot0, dpot0, d2pot0)
    sigmaeff%f1 = -pot0%f1/dpot0%f0
    sigmaeff%f2 = -pot0%f2/dpot0%f0
    sigmaeff%f12 = -(pot0%f12 + (dpot0%f1*sigmaeff%f2+dpot0%f2*sigmaeff%f1) + d2pot0%f0*sigmaeff%f1*sigmaeff%f2)/dpot0%f0

  contains

    subroutine resid_with_derivs(rstar,f,param,dfdr,d2fdr2)
      use numconstants, only: machine_prec
      real, intent(in) :: rstar
      real, intent(in) :: param(1)
      real, intent(out) ::f,dfdr,d2fdr2
      ! Locals
      type(hyperdual) :: sigma, f_hd, dfdr_hd, d2fdr2_hd

      sigma = param(1)
      call pot%calc_r_derivs(rstar*sigma, f_hd, dfdr_hd, d2fdr2_hd)
      f = f_hd%f0
      dfdr = dfdr_hd%f0*sigma%f0
      d2fdr2 = d2fdr2_hd%f0*sigma%f0**2
    end subroutine resid_with_derivs

  end function calc_sigmaeff


  subroutine calc_rmin_and_epseff(pot, rmin, epseff)
    !> Calculate rmin and epseff. Here rmin is defined as the position
    !> where the interaction potential is at a minimum, and epseff is
    !> the absolute value of the minimum.
    use nonlinear_solvers, only: nonlinear_solver, newton_secondorder_singlevar
    class(pair_potential), intent(in) :: pot    !< Pair potential
    type(hyperdual), intent(out), optional :: rmin   !< Position of minimum (m)
    type(hyperdual), intent(out), optional :: epseff !< Effective well-depth (K)
    ! Locals
    type(nonlinear_solver) :: solver
    type(hyperdual) :: pot0, dpot0, d2pot0, d3pot0, rminloc
    real :: sigma_scaled, xinit, xmin, xmax, param(1)

    ! Set the limits and the initial condition
    param(1) = pot%sigma%f0
    xinit = 1.1
    xmin = 0.01
    xmax = 10
    sigma_scaled = 1.0
    solver%rel_tol = 1e-12
    solver%abs_tol = 1e-12

    ! Call solver
    call newton_secondorder_singlevar(resid_with_derivs,xinit,xmin,xmax,solver,sigma_scaled,param)
    if (solver%exitflag /= 0) then
       call stoperror("Not able to solve for effective eps")
    endif

    ! Set real component rmin
    rminloc = 0.0
    rminloc%f0 = sigma_scaled*pot%sigma%f0

    ! Calculate the remaining hyperdual components of rmin, obtained by
    ! expanding u(r=rmin,beta) in rmin for "fixed" beta
    call pot%calc_r_derivs(rminloc, pot0, dpot0, d2pot0)
    rminloc%f1 = -dpot0%f1/d2pot0%f0
    rminloc%f2 = -dpot0%f2/d2pot0%f0
    rminloc%f12 = -(dpot0%f12 + (d2pot0%f1*rmin%f2+d2pot0%f2*rmin%f1) + d3pot0%f0*rmin%f1*rmin%f2)/d2pot0%f0

    ! Calculate effective epsilon
    if (present(rmin)) rmin = rminloc
    if (present(epseff)) epseff = abs(pot%calc(rminloc))

  contains

    subroutine fun(x, param, f, df)
      implicit none
      real, intent(in)  :: x                     !< function argument x
      real, dimension(:), intent(in) :: param    !< additional parameters
      real, intent(out) :: f                     !< function value f(x)
      real, intent(out), optional :: df          !< derivative value df(x)/dx
    end subroutine fun

    subroutine resid_with_derivs(rstar,f,param,dfdr,d2fdr2)
      use numconstants, only: machine_prec
      real, intent(in) :: rstar
      real, intent(in) :: param(1)
      real, intent(out) ::f,dfdr,d2fdr2
      ! Locals
      type(hyperdual) :: sigma, f_hd, dfdr_hd, d2fdr2_hd, d3fdr3_hd

      sigma = param(1)
      call pot%calc_r_derivs(rstar*sigma, f_hd, dfdr_hd, d2fdr2_hd, d3fdr3_hd)
      f = dfdr_hd%f0*sigma%f0
      dfdr = d2fdr2_hd%f0*sigma%f0**2
      d2fdr2 = d3fdr3_hd%f0*sigma%f0**3
    end subroutine resid_with_derivs

  end subroutine calc_rmin_and_epseff


end module pair_potentials
