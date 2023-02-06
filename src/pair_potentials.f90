module pair_potentials
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
  use eosdata, only: eosMie_UV_WCA, eosMie_UV_BH

  type :: mie_potential
     real :: lamr        !< repulsive exponent [-]
     real :: lama        !< attractive exponent [-]
     real :: sigma       !< diameter [m]
     real :: epsdivk     !< energy divided by kB [K]
     real :: Cmie        !< Mie potential prefactor [-]
     real :: rmin        !< location of minimum [m]
     real :: rmin_adim   !< rmin/sigma [-]
     real :: alpha       !< adimensional van der waals energy [-]
   contains
     procedure, public :: init => mie_potential_init
  end type mie_potential

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

  integer, parameter :: nMie = 2
  type(mie_data), dimension(nMie), parameter :: MieArray = (/ &
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="AR", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="AR", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT")/)

contains

  subroutine mie_potential_init(this, lama, lamr, sigma, epsdivk)
    class(mie_potential), intent(inout) :: this
    real, intent(in) :: lama, lamr, sigma, epsdivk

    this%lama = lama
    this%lamr = lamr
    this%sigma = sigma
    this%epsdivk = epsdivk

    this%CMie = abs(lamr/(lamr-lama) * (lamr/lama)**(lama/(lamr-lama)))
    this%rmin = sigma*(lamr/lama)**(1/(lamr-lama))
    this%rmin_adim = this%rmin/sigma
    this%alpha = this%CMie*(1.0/(lama - 3.0) - 1.0/(lamr - 3.0))
  end subroutine mie_potential_init


end module pair_potentials
