!> This is global variables for ThermoPack library. The variables are controlled form
!! eos_container.f90
module thermopack_var
  use thermopack_constants, only: eosid_len, label_len, &
       PSEUDO_CRIT_MOLAR_VOLUME
  use apparent_compostion, only: apparent_container
  use compdata, only: gendata_pointer
  use utilities, only: get_thread_index
  use association_var, only: association
  implicit none
  save
  !
  public

  !> Number of phases:
  integer :: nph = 0
  !> Number of apparent components:
  integer :: nc = 0
  !< Number of real components, to support apparent composition mode. Always have: nce >= nc
  integer :: nce = 0
  !< Symmetrical upper left part of v_stoich
  integer :: ncsym = 0
  !< Total number of associating sites.
  integer :: numAssocSites = 0

  !> List of component names
  character (len=eosid_len), pointer :: complist(:)
  ! Apparent composition
  type(apparent_container), pointer :: apparent

  type, abstract :: base_eos_param
    ! Base class for holding eos-data
    character (len=eosid_len) :: eosid !< Eos identefyer
    !character (len=eos_name_len) :: label !< Name of EOS
    integer :: eosidx !< Eos group index
    integer :: subeosidx !< Eos sub-index
    integer :: volumeShiftId = 0 !< 0: No volume shift, 1:Peneloux shift
    logical :: isElectrolyteEoS = .false. !< Used to enable electrolytes
    !
    type(association), pointer :: assoc => NULL()

  contains
    procedure(allocate_and_init_intf), deferred, public :: allocate_and_init
    procedure, public :: dealloc => eos_dealloc
    ! Assignment operator
    procedure(assign_intf), deferred, pass(This), public :: assign_eos
    generic, public :: assignment(=) => assign_eos
  end type base_eos_param

  abstract interface
    subroutine allocate_and_init_intf(eos,nc,eos_label)
      import base_eos_param
      ! Passed object:
      class(base_eos_param), intent(inout) :: eos
      ! Input:
      integer, intent(in) :: nc !< Number of components
      character(len=*), intent(in) :: eos_label !< EOS label
    end subroutine allocate_and_init_intf
  end interface

  abstract interface
    subroutine assign_intf(This, other)
      import base_eos_param
      ! Passed object:
      class(base_eos_param), intent(out) :: This
      class(*), intent(in) :: other
    end subroutine assign_intf
  end interface

  type :: eos_param_pointer
    class(base_eos_param), pointer :: p_eos
  end type eos_param_pointer

  type eos_container
    integer :: eosc_idx !< Container index
    ! From parameters
    integer :: nph=3
    integer :: nc=0
    integer :: EoSlib=0
    integer :: eosidx=0
    character(len=label_len) :: model
    integer :: liq_vap_discr_method=PSEUDO_CRIT_MOLAR_VOLUME

    ! Apparent composition
    type(apparent_container), pointer :: apparent

    ! Component data
    type(gendata_pointer), allocatable :: comps(:)
    character(len=eosid_len), allocatable :: complist(:)

    ! Need to be list for OMP support
    type(eos_param_pointer), allocatable, dimension(:) :: eos

    ! Alternative EOS used to generate initial values
    logical :: need_alternative_eos
    type(eos_param_pointer), allocatable, dimension(:) :: cubic_eos_alternative
  contains
    procedure, public :: dealloc => eos_container_dealloc
    procedure, public :: is_model_container
    !procedure :: assign_eos_container
    !generic, public :: assignment(=) => assign_eos_container
  end type eos_container

  type :: eos_container_pointer
    type(eos_container), pointer :: p_eosc => NULL()
  end type eos_container_pointer

  ! Index used for naming
  integer :: eos_idx = 0
  ! Active model
  type(eos_container), pointer :: p_active_eos_c => NULL()
  ! ! Multiple model support
  type(eos_container_pointer), allocatable, dimension(:) :: eos_conts

  public :: get_active_eos, get_active_eos_container, &
       get_active_alt_eos, active_eos_container_is_associated
  public :: apparent_to_real_mole_numbers, real_to_apparent_diff, &
       real_to_apparent_differentials, TP_lnfug_apparent
  public :: update_global_variables_form_active_eos_container

contains

  function get_active_eos_container() result(p_eos)
    type(eos_container), pointer :: p_eos
    if (.not. associated(p_active_eos_c)) call stoperror("get_active_eos_container: No active eos found")
    p_eos => p_active_eos_c
  end function get_active_eos_container

  function active_eos_container_is_associated() result(is_assoc)
    logical :: is_assoc
    is_assoc = associated(p_active_eos_c)
  end function active_eos_container_is_associated

  function get_active_eos() result(p_eos)
    class(base_eos_param), pointer :: p_eos
    ! Locals
    type(eos_container), pointer :: p_eos_cont
    integer :: i_eos
    p_eos_cont => get_active_eos_container()

    if (.not. allocated(p_eos_cont%eos)) call stoperror("get_active_eos: eos array not allocted found")
    i_eos = get_thread_index()
    if (.not. associated(p_eos_cont%eos(i_eos)%p_eos)) call stoperror("get_active_eos: eos not acociated")
    p_eos => p_eos_cont%eos(i_eos)%p_eos
  end function get_active_eos

  function get_active_alt_eos() result(p_eos)
    class(base_eos_param), pointer :: p_eos
    ! Locals
    type(eos_container), pointer :: p_eos_cont
    integer :: i_eos
    p_eos_cont => get_active_eos_container()

    if (.not. allocated(p_eos_cont%eos)) call stoperror("get_active_alt_eos: eos array not allocted found")
    i_eos = get_thread_index()
    if (.not. associated(p_eos_cont%cubic_eos_alternative(i_eos)%p_eos)) call stoperror("get_active_alt_eos: eos not acociated")
    p_eos => p_eos_cont%cubic_eos_alternative(i_eos)%p_eos
  end function get_active_alt_eos

  function get_active_comps() result(p_comps)
    type(gendata_pointer), pointer :: p_comps(:)
    ! Locals
    type(eos_container), pointer :: p_eos_cont
    p_eos_cont => get_active_eos_container()
    p_comps => p_eos_cont%comps
  end function get_active_comps

  function is_model_container(eosc, index) result(isC)
    class(eos_container), intent(in) :: eosc
    integer, intent(in) :: index
    logical :: isC
    isC = (eosc%eosc_idx == index)
  end function is_model_container

  subroutine activate_model(index)
    integer, intent(in) :: index
    ! Locals
    integer :: i
    character(len=4) :: index_str
    if (.not. allocated(eos_conts)) call stoperror("No eos exists....")
    do i=1,size(eos_conts)
      if (eos_conts(i)%p_eosc%is_model_container(index)) then
        p_active_eos_c => eos_conts(i)%p_eosc
        call update_global_variables_form_active_eos_container()
        return
      endif
    enddo
    write(index_str,"(I4)") index
    call stoperror("No eos matches label "//adjustl(trim(index_str)))
  end subroutine activate_model

  subroutine update_global_variables_form_active_eos_container()
    nc = p_active_eos_c%nc
    nph = p_active_eos_c%nph
    complist => p_active_eos_c%complist
    apparent => p_active_eos_c%apparent
    if (associated(apparent)) then
      nce = apparent%nce
      ncsym = apparent%ncsym
    else
      nce = nc
      ncsym = nc
    endif
    if (associated(p_active_eos_c%eos(1)%p_eos%assoc)) then
      numAssocSites = p_active_eos_c%eos(1)%p_eos%assoc%numAssocSites
    else
      numAssocSites = 0
    endif
  end subroutine update_global_variables_form_active_eos_container

  function add_eos() result(index)
    !type(eos_container), pointer, intent(in) :: eosc
    integer :: index
    ! Locals
    integer :: i, istat, n
    type(eos_container_pointer), allocatable, dimension(:) :: eos_copy
    type(eos_container), pointer :: eosc
    allocate(eosc, stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate new eos")
    n = 1
    if (allocated(eos_conts)) n = n + size(eos_conts)
    allocate(eos_copy(n), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate eos_copy")
    do i=1,n-1
      eos_copy(i)%p_eosc => eos_conts(i)%p_eosc
    enddo
    if (allocated(eos_conts)) deallocate(eos_conts, stat=istat)
    if (istat /= 0) call stoperror("Not able to deallocate eos_conts")
    allocate(eos_conts(n), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate eos_conts")
    do i=1,n-1
      eos_conts(i)%p_eosc => eos_copy(i)%p_eosc
    enddo
    eos_conts(n)%p_eosc => eosc
    deallocate(eos_copy, stat=istat)
    if (istat /= 0) call stoperror("Not able to deallocate eos_copy")
    p_active_eos_c => eosc
    eos_idx = eos_idx + 1
    index = eos_idx
    p_active_eos_c%eosc_idx = index
  end function add_eos

  subroutine delete_eos(index)
    integer, intent(in) :: index
    ! Locals
    integer :: i, istat, n, nr
    type(eos_container_pointer), allocatable, dimension(:) :: eos_copy
    if (.not. allocated(eos_conts)) call stoperror("Not able to delete model. No models exists....")
    n = size(eos_conts)
    allocate(eos_copy(n-1), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate eos_copy")
    nr = 0
    do i=1,n
      if (eos_conts(i)%p_eosc%is_model_container(index)) then
        if (p_active_eos_c%is_model_container(index)) then
          p_active_eos_c => NULL()
        endif
        call eos_conts(i)%p_eosc%dealloc()
        deallocate(eos_conts(i)%p_eosc, stat=istat)
        if (istat /= 0) call stoperror("Not able to deallocate eos(i)%p_eosc")
      else
        nr = nr + 1
        eos_copy(nr)%p_eosc => eos_conts(i)%p_eosc
      endif
    enddo
    deallocate(eos_conts, stat=istat)
    if (istat /= 0) call stoperror("Not able to deallocate eos_conts")
    if (nr > 0) then
      allocate(eos_conts(n-1), stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate eos_conts")
      do i=1,n-1
        eos_conts(i)%p_eosc => eos_copy(i)%p_eosc
      enddo
      if (.not. associated(p_active_eos_c)) then
        p_active_eos_c => eos_conts(1)%p_eosc
      endif
    endif
  end subroutine delete_eos

  subroutine eos_dealloc(eos)
    ! Passed object:
    class(base_eos_param), intent(inout) :: eos
    ! Input:
  end subroutine eos_dealloc

  subroutine eos_container_dealloc(eosc)
    ! Passed object:
    class(eos_container), intent(inout) :: eosc
    ! Locals
    integer :: i, istat
    do i=1,size(eosc%eos)
      if (associated(eosc%eos(i)%p_eos)) then
        call eosc%eos(i)%p_eos%dealloc()
        deallocate(eosc%eos(i)%p_eos, stat=istat)
        if (istat /= 0) print *,"Error deallocating eos"
        eosc%eos(i)%p_eos => NULL()
      endif
      if (associated(eosc%cubic_eos_alternative(i)%p_eos)) then
        call eosc%cubic_eos_alternative(i)%p_eos%dealloc()
        deallocate(eosc%cubic_eos_alternative(i)%p_eos, stat=istat)
        if (istat /= 0) print *,"Error deallocating cubic_eos_alternative"
        eosc%cubic_eos_alternative(i)%p_eos => NULL()
      endif
    enddo

    if (allocated(eosc%comps)) then
      deallocate(eosc%comps, stat=istat)
      if (istat /= 0) print *,"Error deallocating comps"
    endif
    if (allocated(eosc%complist)) then
      deallocate(eosc%complist, stat=istat)
      if (istat /= 0) print *,"Error deallocating complist"
    endif
    if (associated(eosc%apparent)) then
      call eosc%apparent%dealloc()
      deallocate(eosc%apparent, stat=istat)
      if (istat /= 0) print *,"Error deallocating apparent class"
      eosc%apparent => NULL()
    endif

  end subroutine eos_container_dealloc


  subroutine apparent_to_real_mole_numbers(n,ne)
    real, intent(in) :: n(nc)
    real, intent(out) :: ne(nce)
    if (associated(apparent)) then
      call apparent%apparent_to_real_mole_numbers(n,ne)
    else
      ne = n
    endif
  end subroutine apparent_to_real_mole_numbers

  subroutine real_to_apparent_diff(Fe_n,F_n)
    real, intent(in) :: Fe_n(nce)
    real, intent(out) :: F_n(nc)
    if (associated(apparent)) then
      call apparent%real_to_apparent_diff(Fe_n,F_n)
    else
      F_n = Fe_n
    endif
  end subroutine real_to_apparent_diff

  subroutine real_to_apparent_differentials(Fe_n,Fe_Tn,Fe_Vn,Fe_nn,&
       F_n,F_Tn,F_Vn,F_nn)
    real, intent(in) :: Fe_n(nce),Fe_Tn(nce),Fe_Vn(nce),Fe_nn(nce,nce)
    real, optional, intent(out) :: F_n(nc),F_Tn(nc),F_Vn(nc),F_nn(nc,nc)
    if (associated(apparent)) then
      call apparent%real_to_apparent_differentials(Fe_n,Fe_Tn,Fe_Vn,Fe_nn,&
           F_n,F_Tn,F_Vn,F_nn)
    else
      if (present(F_n)) F_n = Fe_n
      if (present(F_Tn)) F_Tn = Fe_Tn
      if (present(F_Vn)) F_Vn = Fe_Vn
      if (present(F_nn)) F_nn = Fe_nn
    endif
  end subroutine real_to_apparent_differentials

  subroutine TP_lnfug_apparent(nc,ne,n,P,lnfug_real,lnfug,dlnfugdt_real,&
       dlnfugdp_real,dlnfugdn_real,dlnfugdT,dlnfugdP,dlnfugdn)
    ! Input.
    integer, intent(in) :: nc
    real, intent(in) :: n(nc)                           !< Apparent mole numbers [mols]
    real, intent(in) :: P                               !< Pressure [Pa]
    real, intent(in) :: ne(nce)                         !< Real mole numbers [mols]
    real, intent(in) :: lnfug_real(nce)                 !< Log of real fugacities
    real, optional, intent(in) :: dlnfugdt_real(nce), dlnfugdp_real(nce)
    real, optional, intent(in) :: dlnfugdn_real(nce,nce)
    ! Output
    real, intent(out) :: lnfug(nc)                      !< Log of apparent fugacity
    real, optional, intent(out) :: dlnfugdt(nc), dlnfugdp(nc), dlnfugdn(nc,nc)

    if (associated(apparent)) then
      call apparent%TP_lnfug_apparent(nc,ne,n,P,lnfug_real,lnfug,dlnfugdt_real,&
           dlnfugdp_real,dlnfugdn_real,dlnfugdT,dlnfugdP,dlnfugdn)
    else
      lnfug = lnfug_real
      if (present(dlnfugdt)) dlnfugdt = dlnfugdt_real
      if (present(dlnfugdp)) dlnfugdp = dlnfugdp_real
      if (present(dlnfugdn)) dlnfugdn = dlnfugdn_real
    endif
  end subroutine TP_lnfug_apparent
end module thermopack_var
