!> The module eosdata contains the definitions of the equation of state, mixing
!> rule and the interaction parameters.

module eos_container
  use compdata
  use eos_parameters
  use cubic_eos
  use thermopack_var
  use saftvrmie_containers, only: saftvrmie_eos
  use lj_splined, only: ljs_bh_eos, ljs_wca_eos, ljx_ux_eos_constructor
  use pc_saft_nonassoc, only: PCSAFT_eos, sPCSAFT_eos
  use extcsp, only: extcsp_eos
  use ideal, only: ideal_eos, ideal_eos_constructor

contains


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
    if (allocated(p_active_model%eos)) then
      do i=1,size(p_active_model%eos)
        if (associated(p_active_model%eos(i)%p_eos)) then
          call p_active_model%eos(i)%p_eos%dealloc()
          deallocate(p_active_model%eos(i)%p_eos, stat=istat)
          if (istat /= 0) call stoperror("Not able to deallocate p_active_model%eos(i)%p_eos")
        endif
      enddo
      deallocate(p_active_model%eos, stat=istat)
      if (istat /= 0) call stoperror("Not able to deallocate p_active_model%eos")
    endif
    if (allocated(p_active_model%cubic_eos_alternative)) then
      do i=1,size(p_active_model%cubic_eos_alternative)
        if (associated(p_active_model%cubic_eos_alternative(i)%p_eos)) then
          call p_active_model%cubic_eos_alternative(i)%p_eos%dealloc()
          deallocate(p_active_model%cubic_eos_alternative(i)%p_eos, stat=istat)
          if (istat /= 0) call stoperror("Not able to deallocate p_active_model&cubic_eos_alternative(i)%p_eos")
        endif
      enddo
      deallocate(p_active_model%cubic_eos_alternative, stat=istat)
      if (istat /= 0) call stoperror("Not able to deallocate p_active_model%cubic_eos_alternative")
    endif
    ! Set container data
    p_active_model%need_alternative_eos = eos_label_db(idx_db)%need_alternative_eos
    p_active_model%label = eos_label_db(idx_db)%label
    p_active_model%eosidx = eos_index

    ! Number of threads
    neos = 1
    !$ neos = omp_get_max_threads()
    allocate(p_active_model%eos(neos), stat=istat)
    if (istat /= 0) call stoperror("Not able to allocate p_active_model%eos")
    if (eos_label_db(idx_db)%need_alternative_eos) then
      allocate(p_active_model%cubic_eos_alternative(neos), stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate p_active_model%cubic_eos_alternative")
    endif
    do i=1,neos
      p_active_model%eos(i)%p_eos => allocate_p_eos(nc, eos_index, eos_subindex, eosstr)
      !call p_active_model%eos(i)%p_eos%allocate_and_init(nc,eosstr)
      if (istat /= 0) call stoperror("Not able to allocate p_active_model%eos(i)")
      p_active_model%eos(i)%p_eos%eosid = eosstr
      p_active_model%eos(i)%p_eos%eosidx = eos_index
      p_active_model%eos(i)%p_eos%subeosidx = eos_subindex
      if (eos_label_db(idx_db)%need_alternative_eos) then
        allocate(p_active_model%cubic_eos_alternative(i)%p_eos, &
             source=cubic_eos_constructor(nc, "SRK"), stat=istat)
        if (istat /= 0) call stoperror("Not able to allocate p_active_model%cubic_eos_alternative")
        p_active_model%cubic_eos_alternative(i)%p_eos%eosid = "SRK"
        p_active_model%cubic_eos_alternative(i)%p_eos%eosidx = eosCubic
        p_active_model%cubic_eos_alternative(i)%p_eos%subeosidx = cbSRK
        !p_active_model%cubic_eos_alternative(i)%p_eos%allocate_and_init(nc,"SRK")
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
           source=lk_eos_constructor(nc, eosstr), stat=istat)
    case(eosCSP)
      allocate(extcsp_eos :: p_eos, stat=istat)
    case(eosCPA)
      allocate(p_eos, source=cpa_eos_constructor(nc, eosstr), stat=istat)
    case(eosPC_SAFT)
      select case(eos_subindex)
      case(eosOPC_SAFT, eosPCP_SAFT)
        allocate(PCSAFT_eos :: p_eos, stat=istat)
      case(eosSPC_SAFT,eosSPCP_SAFT)
        allocate(sPCSAFT_eos :: p_eos, stat=istat)
      case default
        istat = 1
      end select
      if (istat == 0) &
           call p_eos%allocate_and_init(nc,eosstr)
    case(eos_single)
      allocate(p_eos, &
           source=single_eos_constructor(nc, eosstr), stat=istat)
    case(eosPT)
      select case(eos_subindex)
      case(eosSAFT_VR_MIE)
        allocate(saftvrmie_eos :: p_eos, stat=istat)
      case(eosLJS_BH)
        allocate(ljs_bh_eos :: p_eos, stat=istat)
      case(eosLJS_WCA)
        allocate(ljs_wca_eos :: p_eos, stat=istat)
      case(eosLJS_UV, eosLJS_UF, eosLJ_UF)
        allocate(p_eos, source=ljx_ux_eos_constructor(eosstr), stat=istat)
      case default
        istat = 1
      end select
    case(eosPeTS)
      allocate(p_eos, &
           source=pets_eos_constructor(nc, eosstr), stat=istat)
    case(meosNist_mix)
      allocate(p_eos, &
           source=meos_idealmix_constructor(nc, eosstr), stat=istat)
    case(eos_ideal)
      allocate(p_eos, &
           source=ideal_eos_constructor(nc, eosstr), stat=istat)
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
    eos_c1%label = eos_c2%label

    eos_c1%Rgas = eos_c2%Rgas
    eos_c1%kRgas = eos_c2%kRgas
    eos_c1%tpPmin = eos_c2%tpPmin
    eos_c1%tpPmax = eos_c2%tpPmax
    eos_c1%tpTmin = eos_c2%tpTmin
    eos_c1%tpTmax = eos_c2%tpTmax

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

end module eos_container

subroutine update_global_variables_form_active_thermo_model()
  use thermopack_var, only: nc, nph, complist, apparent, nce, &
       ncsym, numAssocSites, get_active_thermo_model, &
       thermo_model, Rgas, kRgas, tpTmax, tpTmin, tpPmax, tpPmin, &
       robustness_level
  use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_param, svrm_opt
  use lj_splined, only: ljs_wca_eos
  implicit none
  type(thermo_model), pointer :: act_mod_ptr
  act_mod_ptr => get_active_thermo_model()
  nc = act_mod_ptr%nc
  nph = act_mod_ptr%nph
  complist => act_mod_ptr%complist
  apparent => act_mod_ptr%apparent
  Rgas = act_mod_ptr%Rgas
  kRgas = act_mod_ptr%kRgas
  tpTmax = act_mod_ptr%tpTmax
  tpTmin = act_mod_ptr%tpTmin
  tpPmax = act_mod_ptr%tpPmax
  tpPmin = act_mod_ptr%tpPmin
  robustness_level = act_mod_ptr%robustness_level
  if (associated(apparent)) then
    nce = apparent%nce
    ncsym = apparent%ncsym
  else
    nce = nc
    ncsym = nc
  endif
  numAssocSites = 0
  saftvrmie_param => NULL()
  svrm_opt => NULL()
  if (allocated(act_mod_ptr%eos)) then
    if (associated(act_mod_ptr%eos(1)%p_eos%assoc)) then
      numAssocSites = act_mod_ptr%eos(1)%p_eos%assoc%numAssocSites
    endif
    select type (p_eos => act_mod_ptr%eos(1)%p_eos)
    class is (saftvrmie_eos)
      saftvrmie_param => p_eos%saftvrmie_param
      svrm_opt => p_eos%svrm_opt
    class is (ljs_wca_eos)
      svrm_opt => p_eos%svrm_opt
    end select
  endif
end subroutine update_global_variables_form_active_thermo_model

subroutine print_globals()
  use thermopack_var, only: nc, nph, complist, apparent, nce, &
       ncsym, numAssocSites, Rgas, kRgas, tpTmax, tpTmin, tpPmax, tpPmin
  implicit none
  integer :: i
  print *,"nph",nph
  print *,"nc",nc
  print *,"nce",nce
  print *,"ncsym",ncsym
  print *,"numAssocSites",numAssocSites
  print *,"complist:"
  do i=1,nc
    print *," ",trim(complist(i))
  enddo
  print *,"associated(apparent)",associated(apparent)
  print *,"Rgas", Rgas
  print *,"kRgas", kRgas
  print *,"tpTmax", tpTmax
  print *,"tpTmin", tpTmin
  print *,"tpPmax", tpPmax
  print *,"tpPmin", tpPmin
end subroutine print_globals
