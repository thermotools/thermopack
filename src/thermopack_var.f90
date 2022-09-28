!> Global variables for ThermoPack. They are initialized in the thermo_model
!> module.
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
  type(apparent_container), pointer :: apparent => NULL()

  type, abstract :: base_eos_param
    ! Base class for holding parameters unique to an EoS.
    character (len=eosid_len) :: eosid !< Eos identifier
    !character (len=eos_name_len) :: label !< Name of EOS
    integer :: eosidx !< Eos group index
    integer :: subeosidx !< Eos sub-index
    integer :: volumeShiftId = 0 !< 0: No volume shift, 1:Peneloux shift
    logical :: isElectrolyteEoS = .false. !< Used to enable electrolytes
    !
    type(association), pointer :: assoc => NULL()

  contains
    procedure(allocate_and_init_intf), deferred, public :: allocate_and_init
    procedure, public :: dealloc => base_eos_dealloc
    ! Assignment operator
    procedure(assign_intf), deferred, pass(This), public :: assign_eos
    generic, public :: assignment(=) => assign_eos

    procedure, public :: assign_base_eos_param
  end type base_eos_param


  type :: eos_param_pointer
     !> A trivial type that only contains a pointer to base_eos_param. This type
     !> is needed because gfortran does not allow arrays of pointer to
     !> base_eos_param, whereas arrays of the eos_param_pointer type is allowed.
     class(base_eos_param), pointer :: p_eos
  end type eos_param_pointer


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
      class(base_eos_param), intent(inout) :: This
      class(*), intent(in) :: other
    end subroutine assign_intf
  end interface

  type thermo_model
     !> A complete ThermoPack model. Holds information about the EoS, the
     !> mixture, and the computational solver options
    integer :: model_idx !< Model is active if this equals activated_model_idx
    ! From parameters
    integer :: nph=3
    integer :: nc=0
    integer :: EoSlib=0
    integer :: eosidx=0
    character(len=label_len) :: label
    integer :: liq_vap_discr_method=PSEUDO_CRIT_MOLAR_VOLUME

    ! Apparent composition
    type(apparent_container), pointer :: apparent => NULL()

    ! Component data
    type(gendata_pointer), allocatable :: comps(:)
    character(len=eosid_len), allocatable :: complist(:)

    ! Need to be list for OMP support
    type(eos_param_pointer), allocatable, dimension(:) :: eos

    ! Alternative EOS used to generate initial values
    logical :: need_alternative_eos
    type(eos_param_pointer), allocatable, dimension(:) :: cubic_eos_alternative
  contains
    procedure, public :: dealloc => thermo_model_dealloc
    procedure, public :: is_model_container
    !procedure :: assign_thermo_model
    !generic, public :: assignment(=) => assign_thermo_model
  end type thermo_model


  type :: thermo_model_pointer
     !> A trivial type that only contains a pointer to thermo_model. This type
     !> is needed because gfortran does not allow arrays of pointer to
     !> base_eos_param, whereas arrays of the thermo_model_pointer type is
     !> allowed.
    type(thermo_model), pointer :: p_model => NULL()
  end type thermo_model_pointer


  ! Index that indicates the active model
  integer, private :: thermo_model_idx_counter = 0
  ! Pointer to active model
  type(thermo_model), pointer :: p_active_model => NULL()
  ! Multiple model support
  type(thermo_model_pointer), allocatable, dimension(:) :: thermo_models

  public :: get_active_eos, get_active_thermo_model, &
       get_active_alt_eos, active_thermo_model_is_associated
  public :: apparent_to_real_mole_numbers, real_to_apparent_diff, &
       real_to_apparent_differentials, TP_lnfug_apparent
  public :: base_eos_dealloc, delete_all_eos
  public :: add_eos, delete_eos, activate_model, get_eos_identification

contains

  function get_active_thermo_model() result(p_eos)
    type(thermo_model), pointer :: p_eos
    if (.not. associated(p_active_model)) call stoperror("get_active_thermo_model: No active eos found")
    p_eos => p_active_model
  end function get_active_thermo_model

  function active_thermo_model_is_associated() result(is_assoc)
    logical :: is_assoc
    is_assoc = associated(p_active_model)
  end function active_thermo_model_is_associated

  function get_active_eos() result(p_eos)
    class(base_eos_param), pointer :: p_eos
    ! Locals
    type(thermo_model), pointer :: p_eos_cont
    integer :: i_eos
    p_eos_cont => get_active_thermo_model()

    if (.not. allocated(p_eos_cont%eos)) call stoperror("get_active_eos: eos array not allocted found")
    i_eos = get_thread_index()
    if (.not. associated(p_eos_cont%eos(i_eos)%p_eos)) call stoperror("get_active_eos: eos not acociated")
    p_eos => p_eos_cont%eos(i_eos)%p_eos
  end function get_active_eos

  function get_active_alt_eos() result(p_eos)
    class(base_eos_param), pointer :: p_eos
    ! Locals
    type(thermo_model), pointer :: p_eos_cont
    integer :: i_eos
    p_eos_cont => get_active_thermo_model()

    if (.not. allocated(p_eos_cont%eos)) call stoperror("get_active_alt_eos: eos array not allocted found")
    i_eos = get_thread_index()
    if (.not. associated(p_eos_cont%cubic_eos_alternative(i_eos)%p_eos)) call stoperror("get_active_alt_eos: eos not acociated")
    p_eos => p_eos_cont%cubic_eos_alternative(i_eos)%p_eos
  end function get_active_alt_eos

  function get_active_comps() result(p_comps)
    type(gendata_pointer), pointer :: p_comps(:)
    ! Locals
    type(thermo_model), pointer :: p_eos_cont
    p_eos_cont => get_active_thermo_model()
    p_comps => p_eos_cont%comps
  end function get_active_comps

  function is_model_container(model, index) result(isC)
    class(thermo_model), intent(in) :: model
    integer, intent(in) :: index
    logical :: isC
    isC = (model%model_idx == index)
  end function is_model_container

  subroutine activate_model(index)
    integer, intent(in) :: index
    ! Locals
    integer :: i
    character(len=4) :: index_str
    if (.not. allocated(thermo_models)) call stoperror("No eos exists....")
    do i=1,size(thermo_models)
      if (thermo_models(i)%p_model%is_model_container(index)) then
        p_active_model => thermo_models(i)%p_model
        call update_global_variables_form_active_thermo_model()
        !call print_globals()
        return
      endif
    enddo
    write(index_str,"(I4)") index
    call stoperror("No eos matches label "//adjustl(trim(index_str)))
  end subroutine activate_model

  function add_eos() result(index)
    integer :: index
    ! Locals
    integer :: i, istat, n
    type(thermo_model_pointer), allocatable, dimension(:) :: eos_copy
    type(thermo_model), pointer :: model
    allocate(model, stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate new eos")
    n = 1
    if (allocated(thermo_models)) n = n + size(thermo_models)
    if (n > 1) then
      allocate(eos_copy(n), stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate eos_copy")
      do i=1,n-1
        eos_copy(i)%p_model => thermo_models(i)%p_model
      enddo
    endif
    if (allocated(thermo_models)) deallocate(thermo_models, stat=istat)
    if (istat /= 0) call stoperror("Not able to deallocate thermo_models")
    allocate(thermo_models(n), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate thermo_models")
    if (n > 1) then
      do i=1,n-1
        thermo_models(i)%p_model => eos_copy(i)%p_model
      enddo
      deallocate(eos_copy, stat=istat)
      if (istat /= 0) call stoperror("Not able to deallocate eos_copy")
    endif
    thermo_models(n)%p_model => model
    p_active_model => model
    thermo_model_idx_counter = thermo_model_idx_counter + 1
    index = thermo_model_idx_counter
    p_active_model%model_idx = index
  end function add_eos

  subroutine delete_eos(index)
    integer, intent(in) :: index
    ! Locals
    integer :: i, istat, n, nr
    type(thermo_model_pointer), allocatable, dimension(:) :: eos_copy
    if (.not. allocated(thermo_models)) call stoperror("Not able to delete model. No models exists....")
    n = size(thermo_models)
    allocate(eos_copy(n-1), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate eos_copy")
    nr = 0
    do i=1,n
      if (thermo_models(i)%p_model%is_model_container(index)) then
        if (p_active_model%is_model_container(index)) then
          p_active_model => NULL()
        endif
        call thermo_models(i)%p_model%dealloc()
        deallocate(thermo_models(i)%p_model, stat=istat)
        if (istat /= 0) call stoperror("Not able to deallocate eos(i)%p_model")
      else
        nr = nr + 1
        eos_copy(nr)%p_model => thermo_models(i)%p_model
      endif
    enddo
    deallocate(thermo_models, stat=istat)
    if (istat /= 0) call stoperror("Not able to deallocate thermo_models")
    if (nr > 0) then
      allocate(thermo_models(n-1), stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate thermo_models")
      do i=1,n-1
        thermo_models(i)%p_model => eos_copy(i)%p_model
      enddo
      if (.not. associated(p_active_model)) then
        p_active_model => thermo_models(1)%p_model
      endif
    endif
  end subroutine delete_eos

  subroutine base_eos_dealloc(eos)
    ! Passed object:
    class(base_eos_param), intent(inout) :: eos
    ! Locals
    integer :: istat
    if (associated(eos%assoc)) then
      deallocate(eos%assoc, stat=istat)
      if (istat /= 0) print *,"Error deallocating eos%assoc"
      eos%assoc => NULL()
    endif
  end subroutine base_eos_dealloc

  subroutine assign_base_eos_param(this, other)
    ! Passed object:
    class(base_eos_param), intent(inout) :: this
    class(base_eos_param), intent(in) :: other
    ! Locals
    integer :: istat
    this%eosid = other%eosid
    this%eosidx = other%eosidx
    this%subeosidx = other%subeosidx
    this%volumeShiftId = other%volumeShiftId
    this%isElectrolyteEoS = other%isElectrolyteEoS
    !
    if (associated(other%assoc)) then
      if (.not. associated(this%assoc)) then
        allocate(this%assoc, stat=istat)
        if (istat /= 0) print *,"Error allocating assoc"
      endif
      this%assoc = other%assoc
    endif

  end subroutine assign_base_eos_param

  subroutine thermo_model_dealloc(model)
    ! Passed object:
    class(thermo_model), intent(inout) :: model
    ! Locals
    integer :: i, istat
    do i=1,size(model%eos)
       if (allocated(model%eos)) then
          if (associated(model%eos(i)%p_eos)) then
             call model%eos(i)%p_eos%dealloc()
             deallocate(model%eos(i)%p_eos, stat=istat)
             if (istat /= 0) print *,"Error deallocating eos"
             model%eos(i)%p_eos => NULL()
          end if
      endif
      if (allocated(model%cubic_eos_alternative)) then
        if (associated(model%cubic_eos_alternative(i)%p_eos)) then
          call model%cubic_eos_alternative(i)%p_eos%dealloc()
          deallocate(model%cubic_eos_alternative(i)%p_eos, stat=istat)
          if (istat /= 0) print *,"Error deallocating cubic_eos_alternative"
          model%cubic_eos_alternative(i)%p_eos => NULL()
        endif
      endif
    enddo

    if (allocated(model%comps)) then
      do i=1,size(model%comps)
        if (associated(model%comps(i)%p_comp)) then
          deallocate(model%comps(i)%p_comp, stat=istat)
          if (istat /= 0) print *,"Error deallocating comps%p_comp"
        endif
      enddo
      deallocate(model%comps, stat=istat)
      if (istat /= 0) print *,"Error deallocating comps"
    endif
    if (allocated(model%complist)) then
      deallocate(model%complist, stat=istat)
      if (istat /= 0) print *,"Error deallocating complist"
    endif
    if (associated(model%apparent)) then
      call model%apparent%dealloc()
      deallocate(model%apparent, stat=istat)
      if (istat /= 0) print *,"Error deallocating apparent class"
      model%apparent => NULL()
    endif

  end subroutine thermo_model_dealloc

  subroutine delete_all_eos()
    ! Locals
    integer :: i, istat
    if (allocated(thermo_models)) then
      do i=1,size(thermo_models)
        call thermo_models(i)%p_model%dealloc()
        deallocate(thermo_models(i)%p_model, stat=istat)
        if (istat /= 0) call stoperror("Not able to deallocate eos(i)%p_model")
      enddo
      deallocate(thermo_models, stat=istat)
      if (istat /= 0) call stoperror("Not able to deallocate thermo_models")
      p_active_model => NULL()
      complist => NULL()
      apparent => NULL()
      nph = 0
      nc = 0
      nce = 0
      ncsym = 0
      numAssocSites = 0
    endif
  end subroutine delete_all_eos

  subroutine get_eos_identification(eosid)
    character(len=*), intent(out) :: eosid
    ! Locals
    character(len=4) :: eosid_len_str
    if (len(eosid) < eosid_len) then
      write(eosid_len_str,"(I4)") eosid_len
      call stoperror("get_model_identification: len(eosid) should be at least "//&
           adjustl(trim(eosid_len_str)))
    endif
    eosid = "NONE"
    if (associated(p_active_model)) then
      if (allocated(p_active_model%eos)) then
        eosid = trim(p_active_model%eos(1)%p_eos%eosid)
      endif
    endif
  end subroutine get_eos_identification

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
