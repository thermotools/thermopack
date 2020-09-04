!> The module eosdata contains the definitions of the equation of state, mixing
!> rule and the interaction parameters.

module thermo_models
  use compdata
  use eos_parameters
  use cubic_eos
  use thermopack_var
  use saftvrmie_containers, only: saftvrmie_eos
  use pc_saft_nonassoc, only: PCSAFT_eos
  use csp, only: extcsp_eos
  ! type thermo_model
  !   integer :: eosc_idx !< Container index
  !   ! From parameters
  !   integer, target :: nph=3
  !   integer, target :: nc=0
  !   integer :: EoSlib=0
  !   character(len=label_len) :: model
  !   integer :: liq_vap_discr_method=PSEUDO_CRIT_MOLAR_VOLUME

  !   ! Apparent composition
  !   type(apparent_container), target :: apparent

  !   ! Component data
  !   type(gendata_pointer), allocatable :: comps(:)
  !   character(len=eosid_len), allocatable :: complist(:)

  !   ! Need to be list for OMP support
  !   type(eos_data_pointer), allocatable, dimension(:) :: eos

  !   ! Alternative EOS used to generate initial values
  !   logical :: need_alternative_eos
  !   type(eos_data_pointer), allocatable, dimension(:) :: cubic_eos_alternative
  ! contains
  !   procedure, public :: is_model_container
  !   !procedure :: assign_thermo_model
  !   !generic, public :: assignment(=) => assign_thermo_model
  ! end type thermo_model

  ! type :: thermo_model_pointer
  !   type(thermo_model), pointer :: eosc => NULL()
  ! end type thermo_model_pointer

  ! ! Index used for naming
  ! integer :: eos_idx = 0
  ! ! Active model
  ! type(thermo_model), pointer :: p_active_eos = NULL()
  ! ! Multiple model support
  ! type(eos_data_pointer), allocatable, dimension(:) :: eos

contains

  ! function is_model_container(eosc, index) result(isC)
  !   class(thermo_model), intent(in) :: eosc
  !   integer, intent(in) :: index
  !   logical :: isC
  !   isC = (eosc(i)%eosc_idx == index)
  ! end function is_model_container

  ! subroutine activate_model(index)
  !   use thermopack_var, only: nc, nph, complist, apparent, nce, ncsym
  !   integer, intent(in) :: index
  !   ! Locals
  !   integer :: i
  !   if (.not. allocated(eos)) call stoperror("No eos exists....")
  !   do i=1,size(eos)
  !     if (eos(i)%is_model_container(index)) then
  !       p_active_eos => eos(i)
  !       nc => p_active_eos%nc
  !       nph => p_active_eos%nph
  !       complist => p_active_eos%complist
  !       apparent => p_active_eos%apparent
  !       nce => apparent%nce
  !       ncsym => apparent%ncsym
  !       return
  !     endif
  !   enddo
  !   call stoperror("No eos matches label "//trim(model_label))
  ! end subroutine activate_model

  ! function add_eos() result(index)
  !   !type(thermo_model), pointer, intent(in) :: eosc
  !   integer :: index
  !   ! Locals
  !   integer :: i, istat, n
  !   type(eos_data_pointer), allocatable, dimension(:) :: eos_copy
  !   type(thermo_model), allocatable :: eosc
  !   allocate(eosc, stat=istat)
  !   if (istat /= 0) call stoperror("Not able to allocate new eos")
  !   n = 1
  !   if (allocated(eos)) n = n + size(eos)
  !   allocate(eos_copy(n), stat=istat)
  !   if (istat /= 0) call stoperror("Not able to allocate eos_copy")
  !   do i=1,n-1
  !     eos_copy(i) => eos(i)
  !   enddo
  !   if (allocated(eos, stat=istat)) deallocate(eos, stat=istat)
  !   if (istat /= 0) call stoperror("Not able to deallocate eos")
  !   allocate(eos(n), stat=istat)
  !   if (istat /= 0) call stoperror("Not able to allocate eos")
  !   do i=1,n-1
  !     eos(i) => eos_copy(i)
  !   enddo
  !   eos(n) => eosc
  !   deallocate(eos_copy, stat=istat)
  !   if (istat /= 0) call stoperror("Not able to deallocate eos_copy")
  !   p_active_eos => eosc
  !   eos_idx = eos_idx + 1
  !   index = eos_idx
  !   p_active_eos%eosc_idx = index
  ! end function add_eos

  ! subroutine delete_eos(index)
  !   integer, intent(in) :: index
  !   ! Locals
  !   integer :: i, istat, n, nr
  !   type(eos_data_pointer), allocatable, dimension(:) :: eos_copy
  !   if (.not. allocated(eos)) call stoperror("Not able to delete model. No models exists....")
  !   n = size(eos)
  !   allocate(eos_copy(n-1), stat=istat)
  !   if (istat /= 0) call stoperror("Not able to allocate eos_copy")
  !   nr = 0
  !   do i=1,n
  !     if (eos(i)%is_model_container(index)) then
  !       if (p_active_eos%is_model_container(index)) then
  !         p_active_eos => NULL()
  !       endif
  !       deallocate(eos(i), stat=istat)
  !       if (istat /= 0) call stoperror("Not able to deallocate eos(i)")
  !     else
  !       nr = nr + 1
  !       eos_copy(nr) => eos(i)
  !     endif
  !   enddo
  !   deallocate(eos, stat=istat)
  !   if (istat /= 0) call stoperror("Not able to deallocate eos")
  !   if (nr > 0) then
  !     allocate(eos(n-1), stat=istat)
  !     if (istat /= 0) call stoperror("Not able to allocate eos")
  !     do i=1,n-1
  !       eos(i) => eos_copy(i)
  !     enddo
  !     if (.not. associated(p_active_eos)) then
  !       p_active_eos => eos(1)
  !     endif
  !   endif
  ! end subroutine add_eos

  ! subroutine assign_thermo_model(c1, c2)
  !   class(thermo_model), intent(inout) :: eos_c1
  !   class(thermo_model), intent(in) :: eos_c2
  !   ! Locals
  !   eos_c1%nph = eos_c2%nph
  !   eos_c1%nc = eos_c2%nc
  !   eos_c1%EoSlib = eos_c2%EoSlib
  !   eos_c1%model = eos_c2%model

  !   eos_c1%eosidx = eos_c2%eosidx
  !   eos_c1%subeosidx = eos_c2%subeosidx
  !   eos_c1%volumeShiftId = eos_c2%volumeShiftId

  !   eos_c1%need_alternative_eos = eos_c2%need_alternative_eos

  !   ! From tpvar
  !   eos_c1%nce = eos_c2%nce
  !   eos_c1%apparent = eos_c2%apparent

  !   eos_c1%comps = eos_c2%comps

  !   if (allocated(eos_c2%model_eos)) then
  !     if (allocated(eos_c1%model_eos)) then
  !       do i=1,size(eos_c1%model_eos)
  !         if (associated(eos_c1%model_eos(i)%eos)) then
  !           deallocate(eos_c1%model_eos(i)%eos, stat=istat)
  !           if (istat /= 0) call stoperror("Not able to deallocate eos_c1%model_eos(i)%eos")
  !         endif
  !       enddo
  !       allocate(eos_c1%model_eos(size(eos_c2%model_eos)), stat=istat)
  !       if (istat /= 0) call stoperror("Not able to allocate eos_c1%model_eos")
  !       do i=1,size(eos_c1%model_eos)
  !         allocate(eos_c1%model_eos(i)%eos, stat=istat)
  !         if (istat /= 0) call stoperror("Not able to deallocate eos_c1%model_eos(i)%eos")
  !         eos_c1%model_eos(i)%eos = eos_c2%model_eos(i)%eos
  !       enddo
  !     endif
  !   endif

  !   if (allocated(eos_c2%cubic_eos_alternative)) then
  !     if (allocated(eos_c1%cubic_eos_alternative)) then
  !       do i=1,size(eos_c1%cubic_eos_alternative)
  !         if (associated(eos_c1%cubic_eos_alternative(i)%eos)) then
  !           deallocate(eos_c1%cubic_eos_alternative(i)%eos, stat=istat)
  !           if (istat /= 0) call stoperror("Not able to deallocate eos_c1%cubic_eos_alternative(i)%eos")
  !         endif
  !       enddo
  !       allocate(eos_c1%cubic_eos_alternative(size(eos_c2%cubic_eos_alternative)), stat=istat)
  !       if (istat /= 0) call stoperror("Not able to allocate eos_c1%cubic_eos_alternative")
  !       do i=1,size(eos_c1%cubic_eos_alternative)
  !         allocate(eos_c1%cubic_eos_alternative(i)%eos, stat=istat)
  !         if (istat /= 0) call stoperror("Not able to deallocate eos_c1%cubic_eos_alternative(i)%eos")
  !         eos_c1%cubic_eos_alternative(i)%eos = eos_c2%cubic_eos_alternative(i)%eos
  !       enddo
  !     endif
  !   endif

  ! end subroutine assign_thermo_model

  ! ! Functions for allocate statement:
  ! function cpaeos_constructor(....) result(cpa_eos)
  !   ! Input:
  !   ! Created object:
  !   type(cpaeos) :: cpa_eos
  !   ! Locals
  ! end function cpaeos_constructor

  !----------------------------------------------------------------------
  !> Selection of equation of state and allocation of container classes
  !! \param eosstr The equation of state as a character string e.g 'SRK' og 'PR'
  !!
  !! The character strings are case-insensitive
  !!
  !! \author Morten Hammer
  subroutine allocate_eos(nc, eosstr)
    use eosdata
    !$ use omp_lib, only: omp_get_max_threads
    implicit none
    integer, intent (in) :: nc
    character (len=*), intent (in) :: eosstr
    ! Locals
    integer :: idx_db, eos_index, eos_subindex, i, istat, neos
    idx_db = get_eos_db_idx(eosstr)
    if (idx_db < 0) then
      call stoperror('unknown eos')
    endif
    eos_index = eos_label_db(idx_db)%eos_idx
    eos_subindex = eos_label_db(idx_db)%eos_subidx
    if (allocated(p_active_eos_c%eos)) then
      do i=1,size(p_active_eos_c%eos)
        if (associated(p_active_eos_c%eos(i)%p_eos)) then
          call p_active_eos_c%eos(i)%p_eos%dealloc()
          deallocate(p_active_eos_c%eos(i)%p_eos, stat=istat)
          if (istat /= 0) call stoperror("Not able to deallocate p_active_eos_c%eos(i)%p_eos")
        endif
      enddo
      deallocate(p_active_eos_c%eos, stat=istat)
      if (istat /= 0) call stoperror("Not able to deallocate p_active_eos_c%eos")
    endif
    if (allocated(p_active_eos_c%cubic_eos_alternative)) then
      do i=1,size(p_active_eos_c%cubic_eos_alternative)
        if (associated(p_active_eos_c%cubic_eos_alternative(i)%p_eos)) then
          call p_active_eos_c%cubic_eos_alternative(i)%p_eos%dealloc()
          deallocate(p_active_eos_c%cubic_eos_alternative(i)%p_eos, stat=istat)
          if (istat /= 0) call stoperror("Not able to deallocate p_active_eos_c&cubic_eos_alternative(i)%p_eos")
        endif
      enddo
      deallocate(p_active_eos_c%cubic_eos_alternative, stat=istat)
      if (istat /= 0) call stoperror("Not able to deallocate p_active_eos_c%cubic_eos_alternative")
    endif

    ! Set container data
    p_active_eos_c%need_alternative_eos = eos_label_db(idx_db)%need_alternative_eos
    p_active_eos_c%model = eos_label_db(idx_db)%label
    p_active_eos_c%eosidx = eos_index

    ! Number of threads
    neos = 1
    !$ neos = omp_get_max_threads()
    allocate(p_active_eos_c%eos(neos), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate p_active_eos_c%eos")
    if (eos_label_db(idx_db)%need_alternative_eos) then
      allocate(p_active_eos_c%cubic_eos_alternative(neos), stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate p_active_eos_c%cubic_eos_alternative")
    endif
    do i=1,neos
      p_active_eos_c%eos(i)%p_eos => allocate_p_eos(nc, eos_index, eos_subindex, eosstr)
      !call p_active_eos_c%eos(i)%p_eos%allocate_and_init(nc,eosstr)
      if (istat /= 0) call stoperror("Not able to allocate p_active_eos_c%eos(i)")
      p_active_eos_c%eos(i)%p_eos%eosid = eosstr
      p_active_eos_c%eos(i)%p_eos%eosidx = eos_index
      p_active_eos_c%eos(i)%p_eos%subeosidx = eos_subindex
      if (eos_label_db(idx_db)%need_alternative_eos) then
        allocate(p_active_eos_c%cubic_eos_alternative(i)%p_eos, &
             source=cubic_eos_constructor(nc, "SRK"), stat=istat)
        if (istat /= 0) call stoperror("Not able to allocate p_active_eos_c%cubic_eos_alternative")
        p_active_eos_c%cubic_eos_alternative(i)%p_eos%eosid = "SRK"
        p_active_eos_c%cubic_eos_alternative(i)%p_eos%eosidx = eosCubic
        p_active_eos_c%cubic_eos_alternative(i)%p_eos%subeosidx = cbSRK
        !p_active_eos_c%cubic_eos_alternative(i)%p_eos%allocate_and_init(nc,"SRK")
      endif
   enddo

  end subroutine allocate_eos

  function allocate_p_eos(nc, eos_index, eos_subindex, eosstr) result(p_eos)
    use pets, only: pets_eos_constructor
    integer, intent (in) :: nc
    integer, intent (in) :: eos_index, eos_subindex
    character (len=*), intent (in) :: eosstr
    class(base_eos_param), pointer :: p_eos
    ! Locals
    integer :: istat
    select case(eos_index)
    case(eosCubic)
      allocate(p_eos, &
           source=cubic_eos_constructor(nc, eosstr), stat=istat)
    case(eosLK)
      allocate(p_eos, &
           source=cubic_eos_constructor(nc, eosstr), stat=istat)
    case(eosCSP)
      allocate(extcsp_eos :: p_eos, stat=istat)
    case(eosCPA)
       allocate(p_eos, source=cpa_eos_constructor(nc, eosstr), stat=istat)
    case(eosPC_SAFT)
      allocate(PCSAFT_eos :: p_eos, stat=istat)
      call p_eos%allocate_and_init(nc,eosstr)
    case(eos_single)
      allocate(p_eos, &
           source=single_eos_constructor(nc, eosstr), stat=istat)
    case(eosBH_pert)
      select case(eos_subindex)
      case(eosSAFT_VR_MIE)
        allocate(saftvrmie_eos :: p_eos, stat=istat)
      case default
        istat = 1
      end select
    case(eosPeTS)
      allocate(p_eos, &
           source=pets_eos_constructor(nc, eosstr), stat=istat)
    case(meosNist_mix)
      allocate(p_eos, &
           source=meos_mix_constructor(nc, eosstr), stat=istat)
    case default
      istat = 1
    end select
    if (istat /= 0 .or. .not. associated(p_eos)) then
      call stoperror("Not able to allocate p_eos")
    endif
  end function allocate_p_eos

  subroutine assign_thermo_model(eos_c1, eos_c2)
    class(thermo_model), intent(inout) :: eos_c1
    class(thermo_model), intent(in) :: eos_c2
    ! Locals
    eos_c1%nph = eos_c2%nph
    eos_c1%nc = eos_c2%nc
    eos_c1%EoSlib = eos_c2%EoSlib
    eos_c1%model = eos_c2%model

    eos_c1%eosidx = eos_c2%eosidx
    !eos_c1%subeosidx = eos_c2%subeosidx
    !eos_c1%volumeShiftId = eos_c2%volumeShiftId

    eos_c1%need_alternative_eos = eos_c2%need_alternative_eos

    ! From tpvar
    !eos_c1%nce = eos_c2%nce
    eos_c1%apparent = eos_c2%apparent

    eos_c1%comps = eos_c2%comps

    if (allocated(eos_c2%eos)) then
      if (allocated(eos_c1%eos)) then
        do i=1,size(eos_c1%eos)
          if (associated(eos_c1%eos(i)%p_eos)) then
            deallocate(eos_c1%eos(i)%p_eos, stat=istat)
            if (istat /= 0) call stoperror("Not able to deallocate eos_c1%model_eos(i)%eos")
          endif
        enddo
        allocate(eos_c1%eos(size(eos_c2%eos)), stat=istat)
        if (istat /= 0) call stoperror("Not able to allocate eos_c1%model_eos")
        do i=1,size(eos_c1%eos)
          eos_c1%eos(i)%p_eos => allocate_p_eos(nc, eos_c2%eos(1)%p_eos%eosidx, &
               eos_c2%eos(1)%p_eos%subeosidx, eos_c2%eos(1)%p_eos%eosid)
          if (istat /= 0) call stoperror("Not able to allocate eos_c1%model_eos(i)%eos")
          eos_c1%eos(i)%p_eos = eos_c2%eos(i)%p_eos
        enddo
      endif
    endif

    if (allocated(eos_c2%cubic_eos_alternative)) then
      if (allocated(eos_c1%cubic_eos_alternative)) then
        do i=1,size(eos_c1%cubic_eos_alternative)
          if (associated(eos_c1%cubic_eos_alternative(i)%p_eos)) then
            deallocate(eos_c1%cubic_eos_alternative(i)%p_eos, stat=istat)
            if (istat /= 0) call stoperror("Not able to deallocate eos_c1%cubic_eos_alternative(i)%eos")
          endif
        enddo
        allocate(eos_c1%cubic_eos_alternative(size(eos_c2%cubic_eos_alternative)), stat=istat)
        if (istat /= 0) call stoperror("Not able to allocate eos_c1%cubic_eos_alternative")
        do i=1,size(eos_c1%cubic_eos_alternative)
          eos_c1%cubic_eos_alternative(i)%p_eos => allocate_p_eos(nc, &
               eos_c2%cubic_eos_alternative(1)%p_eos%eosidx, &
               eos_c2%cubic_eos_alternative(1)%p_eos%subeosidx, &
               eos_c2%cubic_eos_alternative(1)%p_eos%eosid)
          eos_c1%cubic_eos_alternative(i)%p_eos = eos_c2%cubic_eos_alternative(i)%p_eos
        enddo
      endif
    endif

  end subroutine assign_thermo_model

end module thermo_models

subroutine update_global_variables_form_active_thermo_model()
  use thermopack_var, only: nc, nph, complist, apparent, nce, &
       ncsym, numAssocSites, get_active_thermo_model, &
       thermo_model
  use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_param
  type(thermo_model), pointer :: act_mod_ptr
  act_mod_ptr => get_active_thermo_model()
  nc = act_mod_ptr%nc
  nph = act_mod_ptr%nph
  complist => act_mod_ptr%complist
  apparent => act_mod_ptr%apparent
  if (associated(apparent)) then
    nce = apparent%nce
    ncsym = apparent%ncsym
  else
    nce = nc
    ncsym = nc
  endif
  if (associated(act_mod_ptr%eos(1)%p_eos%assoc)) then
    numAssocSites = act_mod_ptr%eos(1)%p_eos%assoc%numAssocSites
  else
    numAssocSites = 0
  endif
  select type (p_eos => act_mod_ptr%eos(1)%p_eos)
  class is (saftvrmie_eos)
    saftvrmie_param => p_eos%saftvrmie_param
  class default
    saftvrmie_param => NULL()
  end select
end subroutine update_global_variables_form_active_thermo_model
