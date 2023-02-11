module pair_potentials
  use hyperdual_mod
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


  type :: mie_potential_hd
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

  integer, parameter :: nMie = 2
  type(mie_data), dimension(nMie), parameter :: MieArray = (/ &
       mie_data(eosidx = eosMie_UV_WCA, &
       compName="AR", &
       lamr=12, &
       sigma=3.42E-10, &
       eps_divk=124.0, &
       ref="DEFAULT"), &
       mie_data(eosidx = eosMie_UV_BH, &
       compName="AR", &
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

  subroutine mie_potential_init(this, lama, lamr, sigma, epsdivk)
    class(mie_potential), intent(inout) :: this
    real, intent(in) :: lama, lamr, sigma, epsdivk

    this%lama = lama
    this%lamr = lamr
    this%sigma = sigma
    this%epsdivk = epsdivk

    this%CMie = lamr/(lamr-lama) * (lamr/lama)**(lama/(lamr-lama))
    this%rmin = sigma*(lamr/lama)**(1/(lamr-lama))
    this%rmin_adim = this%rmin/sigma
    this%alpha = this%CMie*(1.0/(lama - 3.0) - 1.0/(lamr - 3.0))
  end subroutine mie_potential_init

  subroutine mie_potential_hd_init(this, lama, lamr, sigma, epsdivk)
    class(mie_potential_hd), intent(inout) :: this
    type(hyperdual), intent(in) :: lama, lamr, sigma, epsdivk

    this%lama = lama
    this%lamr = lamr
    this%sigma = sigma
    this%epsdivk = epsdivk

    this%CMie = lamr/(lamr-lama) * (lamr/lama)**(lama/(lamr-lama))
    this%rmin = sigma*(lamr/lama)**(1.0/(lamr-lama))
    this%rmin_adim = this%rmin/sigma
    this%alpha = this%CMie*(1.0/(lama - 3.0) - 1.0/(lamr - 3.0))
  end subroutine mie_potential_hd_init

end module pair_potentials
