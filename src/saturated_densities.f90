module saturated_densities
  use stringmod, only: str_eq
  use thermopack_constants, only: VAPPH
  implicit none
  save
  public

  !> Class calculating saturated densities
  type :: sat_densities
    private
    character(len=40), public :: compName
    integer :: correlation_id = 0    ! Identifyer for correlation
    real, public :: t_reducing, rho_reducing
    integer :: n_param = 0    ! Number of parameters
    real :: N_sat(6)
    real :: expo_sat(6)
  contains

    ! Public methods
    procedure, public :: init => init_sat_dens
    procedure, public :: density
    !procedure(density_intf), public, deferred :: reduced_density !< An estimate of the reduced density at the saturated state.

  end type sat_densities

  !  abstract interface
  !    function density_intf(this, T) result(d)
  !      import saturated_densities
  !      class(saturated_densities), intent(in) :: this
  !      real, intent(in) :: T ! Temperature
  !      real :: d ! Density
  !    end function density_intf
  ! end interface

contains

  function density(this, T) result(d)
    implicit none
    class(sat_densities), intent(in) :: this
    real, intent(in) :: T   ! Temperature
    real :: d               ! Density
    ! Locals
    real :: sum, theta
    integer :: j
    d = 0
    theta = dabs(1.d0-T/this%t_reducing)
    if (mod(this%correlation_id, 2) .eq. 0) theta = theta**(1.d0/3.d0)
    if (this%n_param == 0) then
      print *,"Parameters for saturated density missing"
      return
      d = 0
    endif
    sum = 0
    do j=1, this%n_param
      sum = sum + this%N_sat(j)*theta**this%expo_sat(j)
    end do
    if (this%correlation_id .eq. 1 .or. this%correlation_id .eq. 2) sum = 1 + sum
    if (this%correlation_id .eq. 3 .or. this%correlation_id .eq. 4) sum = exp(sum)
    if (this%correlation_id .eq. 5 .or. this%correlation_id .eq. 6) sum = exp(this%t_reducing/T*sum)

    d = this%rho_reducing*sum
  end function density

  function constructor_sat_dens(comp_name, phase) result(satdens)
    use satdensdatadb, only: maxsatdens, satdensdb
    character(len=*), intent(in) :: comp_name
    integer, intent(in) :: phase
    type(sat_densities) :: satdens
    !
    call satdens%init(comp_name, phase)
  end function constructor_sat_dens

  subroutine init_sat_dens(this, comp_name, phase)
    use satdensdatadb, only: maxsatdens, satdensdb
    class(sat_densities), intent(inout) :: this
    character(len=*), intent(in) :: comp_name
    integer, intent(in) :: phase
    ! Locals
    integer :: i_comp, i
    character(len=1) :: ph
    if (phase == VAPPH) then
      ph = "G"
    else
      ph = "L"
    endif
    i_comp = -1
    do i=1,maxsatdens
      if (str_eq(comp_name, satdensdb(i)%ident) .and. satdensdb(i)%phase == ph) then
        i_comp = i
        exit
      endif
    enddo
    if (i_comp > 0) then
      this%compName = satdensdb(i_comp)%name
      this%correlation_id = satdensdb(i_comp)%correlation_id
      this%t_reducing = satdensdb(i_comp)%tr
      this%rho_reducing = satdensdb(i_comp)%rhor*1.0e3 !  -> mol/l -> mol/m3
      this%n_param = satdensdb(i_comp)%n_param
      this%n_sat = satdensdb(i_comp)%n
      this%expo_sat = satdensdb(i_comp)%t
    end if
  end subroutine init_sat_dens

end module saturated_densities
