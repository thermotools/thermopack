!> This module defines a type for association variables in the ThermoPack library.
module association_var
  implicit none
  save
  !
  public

  type association
    integer :: saft_model   !> Active SAFT model. Set to the correct EoS index stored in module eosdata.
    integer :: numAssocSites !< Total number of associating sites.
    !> comp_vs_sites is an (nc)x2-matrix. Row i holds information on component number i.
    !> Column 1 and column 2 both equal the integer noSitesFlag if the component does not self-associate.
    !> If the component does self-associate, the rows hold the first and last association site number.
    integer, allocatable, dimension(:,:) :: comp_vs_sites
    !> Model parameters that control the association strength.
    real, allocatable, dimension(:,:) :: beta_kl  !< Effective association volume between site Ai and Bj (called \beta^{A_i B_j} in CPA).
    real, allocatable, dimension(:,:) :: eps_kl   !< Association energy.

    !> Cached states
    real :: T_cache = 0.0
    real, allocatable, dimension(:,:) :: boltzmann_fac_cache !< Cached Delta_kl matrix
!  contains
!    procedure, public :: allocate_and_init => association_allocate_and_init
  end type association


  public :: association

contains

  ! subroutine association_allocate_and_init(numAssocSites)
  !   integer, intent(in) :: numAssocSites
  !     allocate(beta_kl(numAssocSites,numAssocSites))
  !   allocate(eps_kl(numAssocSites,numAssocSites))
  !   allocate(boltzmann_fac_cache(numAssocSites,numAssocSites))
  !   T_cache = -1.0 to ensure a new init recalculates any cached variables

end module association_var
