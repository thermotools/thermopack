!> This module defines a type for association variables in the ThermoPack library.
module association_var
  implicit none
  save
  !
  public

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

    !> Cached states
  contains
    procedure, public :: dealloc
!    procedure, public :: allocate_and_init => association_allocate_and_init
  end type association


  public :: association

contains

  subroutine dealloc(assoc)
    use utilities, only: deallocate_real_2
    ! Input:
    ! Created object:
    class(association), intent(inout) :: assoc
    ! Locals
    integer :: ierr
    ierr = 0
    if (allocated(assoc%compidcs)) deallocate(assoc%compidcs,stat=ierr)
    if (allocated(assoc%comp_vs_sites)) deallocate(assoc%comp_vs_sites,stat=ierr)
    if (ierr /= 0) print *,'association: Not able to allocate comp_vs_sites memory'
    call deallocate_real_2(assoc%beta_kl,"assoc%beta_kl")
    call deallocate_real_2(assoc%eps_kl,"assoc%eps_kl")
  end subroutine dealloc

end module association_var
