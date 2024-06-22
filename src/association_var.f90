!> This module defines a type for association variables in the ThermoPack library.
module association_var
  implicit none
  save
  !
  public

  ! Choice of combining rule for cross-association Delta
  integer, parameter :: STANDARD=1
  integer, parameter :: ELLIOT=2

  !> Current state for eos evaluation
  type association_state
    logical :: fmt_mode = .false.
    real :: T
    real :: V
    real, allocatable :: n(:)
    real, allocatable :: n_fmt(:,:)
  contains
    procedure :: init => init_assoc_state
    procedure :: init_fmt => init_assoc_state_fmt
    procedure :: dealloc => dealloc_assoc_state
  end type association_state

  type association
    integer :: saft_model   !> Active SAFT model. Set to the correct EoS index stored in module eosdata.
    integer :: numAssocSites !< Total number of associating sites.
    integer :: numAssocComps !< Number of associating components.
    integer, allocatable, dimension(:) :: compIdcs !< Component indices of associating components
    !> comp_vs_sites is an (nc)x2-matrix. Row i holds information on component number i.
    !> Column 1 and column 2 both equal the integer noSitesFlag if the component does not self-associate.
    !> If the component does self-associate, the rows hold the first and last association site number.
    integer, allocatable, dimension(:,:) :: comp_vs_sites
    !> Model parameters that control the association strength.
    real, allocatable, dimension(:,:) :: beta_kl  !< Effective association volume between site Ai and Bj (called \beta^{A_i B_j} in CPA).
    real, allocatable, dimension(:,:) :: eps_kl   !< Association energy.
    type(association_state) :: state
    integer :: delta_combrule = STANDARD
  contains
    procedure, public :: dealloc
    procedure, public :: print
!    procedure, public :: allocate_and_init => association_allocate_and_init
  end type association


  public :: association

contains

  subroutine dealloc(assoc)
    use utilities, only: deallocate_real_2
    class(association), intent(inout) :: assoc
    ! Locals
    integer :: ierr
    ierr = 0
    if (allocated(assoc%compidcs)) deallocate(assoc%compidcs,stat=ierr)
    if (allocated(assoc%comp_vs_sites)) deallocate(assoc%comp_vs_sites,stat=ierr)
    if (ierr /= 0) print *,'association: Not able to allocate comp_vs_sites memory'
    call deallocate_real_2(assoc%beta_kl,"assoc%beta_kl")
    call deallocate_real_2(assoc%eps_kl,"assoc%eps_kl")
    call assoc%state%dealloc()
  end subroutine dealloc

  subroutine init_assoc_state(assoc_p, nc, T, V, n)
    class(association_state), intent(inout) :: assoc_p
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc)
    call assoc_p%dealloc()
    assoc_p%fmt_mode = .false.
    assoc_p%T = T
    assoc_p%V = V
    assoc_p%n = n
  end subroutine init_assoc_state

  subroutine init_assoc_state_fmt(assoc_p, nc, T, n_fmt, m)
    use thermopack_constants, only: N_AVOGADRO
    class(association_state), intent(inout) :: assoc_p
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: n_fmt(nc, 0:5) !< Mol based FMT weighted densities
    real, intent(in) :: m(nc) !< Segment numbers
    integer, intent(in) :: nc
    ! Locals
    real :: xi(nc)
    call assoc_p%dealloc()
    assoc_p%fmt_mode = .true.
    !assoc_p%fmt_mode = .false.
    assoc_p%T = T
    assoc_p%n_fmt = n_fmt*N_AVOGADRO ! Convert from mole based to molecular based
    xi = 1.0 - n_fmt(:,5)**2/n_fmt(:,2)**2
    assoc_p%n = xi*n_fmt(:,0)/m ! Use mole based numbers
    assoc_p%V = 1.0/sum(assoc_p%n)
    assoc_p%n = assoc_p%n*assoc_p%V
  end subroutine init_assoc_state_fmt

  subroutine dealloc_assoc_state(assoc_p)
    class(association_state), intent(inout) :: assoc_p
    if (allocated(assoc_p%n)) deallocate(assoc_p%n)
    if (allocated(assoc_p%n_fmt)) deallocate(assoc_p%n_fmt)
  end subroutine dealloc_assoc_state

  !subroutine print_assoc_state(assoc_p)
  !  class(association_state), intent(in) :: assoc_p
  !  assoc_p%fmt_mode = .false.
  !  assoc_p%T = T
  !  assoc_p%V = V
  !  assoc_p%n = n
  !end subroutine init_assoc_state

  subroutine print(assoc)
    class(association), intent(in) :: assoc
    !
    print *,"saft_model:",assoc%saft_model
    print *,"numAssocSites:",assoc%numAssocSites
    print *,"numAssocComps:",assoc%numAssocComps
    print *,"compIdcs:",assoc%compIdcs
    print *,"comp_vs_sites:",assoc%comp_vs_sites
    print *,"beta_kl:",assoc%beta_kl
    print *,"eps_kl:",assoc%eps_kl
  end subroutine print

end module association_var
