module pair_potentials
  use hyperdual_mod
  use numconstants, only: PI
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
  use eosdata, only: eosMie_UV_WCA, eosMie_UV_BH

  !> Base class for pair potential
  type, abstract :: pair_potential
   contains
     procedure(calc_pot_intf), public, deferred  :: value
     procedure(calc_B2_intf), public, deferred   :: B2
     procedure(alpha_x_intf), public, deferred   :: alpha_x
  end type pair_potential

  abstract interface
     type(hyperdual) function calc_B2_intf(pot, beta) result(B2)
       use hyperdual_mod
       use quadratures
       import pair_potential
       class(pair_potential), intent(in) :: pot !< Pair potential
       type(hyperdual), intent(in)       :: beta  !< 1/kT (J)
       !type(hyperdual), intent(out)      :: B2     !< Second virial coefficient (m^3)
     end function calc_B2_intf
  end interface

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
     type(hyperdual) function alpha_x_intf(this, x, adim) result(alpha_x)
       !> alpha_x = -\int_x^\infty pot(x)*x^2 dx (everything adimensional)
       use hyperdual_mod, only: hyperdual
       import pair_potential
       class(pair_potential), intent(in) :: this !< potential
       type(hyperdual), intent(in) :: x          !< lower integration limit (-)
       logical, intent(in), optional :: adim     !< adimensional value for alpha, or in ((J/K)*m^3)?
     end function alpha_x_intf
  end interface

  type, extends(pair_potential) :: mie_potential_hd
     type(hyperdual) :: lamr        !< repulsive exponent [-]
     type(hyperdual) :: lama        !< attractive exponent [-]
     type(hyperdual) :: sigma       !< diameter [m]
     type(hyperdual) :: epsdivk     !< energy divided by kB [K]
     type(hyperdual) :: Cmie        !< Mie potential prefactor [-]
     type(hyperdual) :: rmin        !< location of minimum [m]
     type(hyperdual) :: rmin_adim   !< rmin/sigma [-]
     type(hyperdual) :: alpha       !< adimensional van der waals energy [-]
   contains
     procedure, public :: init => mie_potential_hd_init
     procedure, public :: value => mie_potential_hd_calc
     procedure, public :: B2 => calc_B2_by_quadrature
     procedure, public :: alpha_x => mie_potential_hd_calc_alpha_x
  end type mie_potential_hd

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

  integer, parameter :: nMie = 5
  type(mie_data), dimension(nMie), parameter :: MieArray = (/ &
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="AR", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
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
       compName="C1", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT")/)

contains

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

  type(hyperdual) function mie_potential_hd_calc_alpha_x(this, x, adim) result(alpha_x)
    !> alpha_x = -\int_x^\infty pot(x)*x^2 dx (everything adimensional)
    class(mie_potential_hd), intent(in) :: this !< This potential
    type(hyperdual), intent(in) :: x           !< lower integration limit (-)
    logical, intent(in), optional :: adim                !< adimensional value for alpha, or in ((J/K)*m^3)?
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

 function B2_integrand(r, beta, pot) result(val)
    type(hyperdual), intent(in)         :: r    !< Separation (m)
    type(hyperdual), intent(in)         :: beta !< 1/kT (J)
    class(mie_potential_hd), intent(in) :: pot  !< Pair potential
    type(hyperdual)                     :: val  !< Integrand (m^2)
    val = -2.0*PI*(exp(-beta*pot%value(r)) - 1.0) * r*r
  end function B2_integrand

  type(hyperdual) function calc_B2_by_quadrature(pot, beta) result(B2)
    !> Second virial coefficient (m^3)
    use quadratures
    class(mie_potential_hd), intent(in) :: pot !< Pair potential
    type(hyperdual), intent(in)       :: beta  !< 1/kT (J)
    ! Locals
    type(hyperdual) :: r
    integer :: B2_quadrature = GAUSS_KRONROD_31 ! Modify to improve accuracy
    integer :: i, n_quad
    real :: x_vec(max_n_quadrature), w_vec(max_n_quadrature)
    real :: rmin, rmax ! lower and upper integration limits
    real :: rmid, xscale

    ! Get quadrature points
    call get_quadrature_positions(B2_quadrature,x_vec,n_quad)
    call get_quadrature_weights(B2_quadrature,w_vec,n_quad)

    ! Calculate lower and upper integration limits
    rmin = 0.3*pot%sigma%f0
    rmax = pot%rmin%f0
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
    rmin = pot%rmin%f0
    rmax = 5*pot%rmin%f0
    rmid   = (rmax + rmin)/2
    xscale = (rmax - rmin)/2
    do i=1,n_quad
       r = rmid + xscale*x_vec(i)
       B2 = B2 + xscale*B2_integrand(r, beta, pot)*w_vec(i)
    end do

    ! Add analytic correction for the region rmax...infty, where
    ! exp(-beta*u(r))-1 \approx -beta u(r)
    B2 = B2 + (-2*PI) * beta*pot%alpha_x(rmax/pot%sigma)*pot%epsdivk*pot%sigma**3
  end function calc_B2_by_quadrature

end module pair_potentials
