!> Module with template functions for locating paramameters in a database
module parameters
  use thermopack_constants, only: eosid_len, ref_len, uid_len, verbose
  implicit none
  save

  abstract interface
    subroutine get_pure_data_db_entry(idx, eos_subidx, comp_name, ref)
      use thermopack_constants, only: ref_len, uid_len
      integer, intent(in) :: idx !< Database index
      integer, intent(out) :: eos_subidx !< Index of EOS
      character(len=uid_len), intent(out) :: comp_name !< Component name
      character(len=ref_len), intent(out) :: ref !< Reference string
    end subroutine get_pure_data_db_entry
  end interface

  abstract interface
    subroutine get_binary_data_db_entry(idx, mrule, eos_subidx, uid1, uid2, ref, kijvalue)
      use thermopack_constants, only: ref_len, uid_len, eosid_len
      integer, intent(in) :: idx !< Database index
      integer, intent(out) :: eos_subidx !< Index of EOS
      character(len=eosid_len), intent(out) :: mrule !< Mixing rule
      character(len=uid_len), intent(out) :: uid1, uid2 !< Component names
      character(len=ref_len), intent(out) :: ref !< Reference string
      real, intent(out) :: kijvalue !< Interaction parameter
    end subroutine get_binary_data_db_entry
  end interface

contains

  !> Get the index in the database array of the specified component
  subroutine get_pure_data_db_idx(get_db_entry,n_db_entries,db_name,&
       eos_subidx,comp_name,param_ref,print_error_and_stop,idx,idx_default)
    use stringmod, only: str_eq, string_match, exact_substring_match
    procedure(get_pure_data_db_entry) :: get_db_entry !< Database wrapper
    integer, intent(in) :: n_db_entries !< Number of entries in database
    character(len=*), intent(in) :: db_name !< Name of database
    integer, intent(in) :: eos_subidx !< Index of EOS
    character(len=*), intent(in) :: comp_name !< Component name
    character(len=*), intent(in) :: param_ref !< Reference string
    logical, intent(in) :: print_error_and_stop !< If no data is found print error message and stop
    integer, intent(out) :: idx !< Database index. Negative if not found.
    integer, intent(out) :: idx_default !< Database index of default data. Negative if not found.
    ! Locals:
    integer :: idx_iter, substring_index, substring_index_old
    integer :: eos_subidx_db !< Index of EOS
    character(len=uid_len) :: comp_name_db !< Component name
    character(len=ref_len) :: ref_db !< Reference string
    idx = -1
    idx_default = -1
    substring_index_old = 1000
    do idx_iter=1,n_db_entries
      call get_db_entry(idx_iter, eos_subidx_db, comp_name_db, ref_db)
      if (eos_subidx == eos_subidx_db .and. &
           str_eq(comp_name, comp_name_db)) then
        if (exact_substring_match(param_ref, ref_db, substring_index)) then
          if (substring_index < substring_index_old) then
            idx = idx_iter
            substring_index_old = substring_index
          endif
        elseif (string_match("DEFAULT", ref_db)) then
          idx_default = idx_iter
        endif
      endif
    enddo

    if (print_error_and_stop) then
      if (idx < 0) then
        if (verbose) then
          print *, "No "//trim(db_name)//" parameters for comp_name, ref ", comp_name, trim(param_ref)
        endif
        if (idx_default > 0) then
          idx = idx_default
          if (.not. string_match("DEFAULT", param_ref)) &
               print *, "Using default parameter set for "//trim(comp_name)
        else
          print *, "ERROR FOR COMPONENT ", comp_name
          call stoperror("The "//trim(db_name)//" parameters don't exist.")
        endif
      endif
    endif
  end subroutine get_pure_data_db_idx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database, the default value is returned.
  function get_binary_interaction_parameter(get_db_entry,n_db_entries,db_name,&
       mrule,eos_subidx,uid1,uid2,param_ref,default_value,idx,idx_default) result(kijvalue)
    use stringmod, only: string_match
    procedure(get_binary_data_db_entry) :: get_db_entry !< Database wrapper
    integer, intent(in) :: n_db_entries !< Number of entries in database
    character(len=*), intent(in) :: db_name !< Name of database
    integer, intent(in) :: eos_subidx !< Index of EOS
    character(len=*), intent(in) :: uid1, uid2 !< Component uids
    character(len=*), intent(in) :: param_ref !< Reference string
    character(len=*), intent(in) :: mrule !< Mixing rule
    real, intent(in) :: default_value
    integer, intent(out) :: idx !< Database index. Negative if not found.
    integer, intent(out) :: idx_default !< Database index of default data. Negative if not found.
    real :: kijvalue
    ! Locals
    integer :: eos_subidx_db !< Index of EOS
    character(len=uid_len) :: uid1_db, uid2_db !< Component name
    character(len=ref_len) :: ref_db !< Reference string
    character(len=eosid_len) :: mrule_db !< Mixing rule

    call get_binary_db_idx(get_db_entry,n_db_entries,&
         mrule,eos_subidx,uid1,uid2,param_ref,idx,idx_default)
    if (idx > 0) then
      call get_db_entry(idx, mrule_db, eos_subidx_db, uid1_db, uid2_db, ref_db, kijvalue)
    else if (idx_default > 0) then
      call get_db_entry(idx_default, mrule_db, eos_subidx_db, uid1_db, uid2_db, ref_db, kijvalue)
      if (.not. string_match("DEFAULT", param_ref)) &
           print *, "Using default database ("//trim(db_name)//") kij entry for the binary: ", trim(uid1), ",", trim(uid2)
    else
      kijvalue = default_value
      if (verbose) print *, "Using default kij for the binary: ", trim(uid1), ",", trim(uid2)
    endif
  end function get_binary_interaction_parameter

  !> Retrieve database entry containing binary interaction parameter for components uid1 and uid2.
  subroutine get_binary_db_idx(get_db_entry,n_db_entries,&
       mrule,eos_subidx,uid1,uid2,param_ref,idx,idx_default)
    use stringmod, only: str_eq, string_match, exact_substring_match
    use thermopack_constants, only: uid_len,ref_len, eosid_len
    procedure(get_binary_data_db_entry) :: get_db_entry !< Database wrapper
    integer, intent(in) :: n_db_entries !< Number of entries in database
    integer, intent(in) :: eos_subidx !< Index of EOS
    character(len=*), intent(in) :: mrule !< Mixing rule
    character(len=*), intent(in) :: uid1, uid2 !< Component uids
    character(len=*), intent(in) :: param_ref !< Reference string
    integer, intent(out) :: idx !< Database index. Negative if not found.
    integer, intent(out) :: idx_default !< Database index of default data. Negative if not found.
    ! Locals:
    integer :: idx_iter, substring_index, substring_index_old
    integer :: eos_subidx_db !< Index of EOS
    character(len=uid_len) :: uid1_db, uid2_db !< Component name
    character(len=ref_len) :: ref_db !< Reference string
    character(len=eosid_len) :: mrule_db !< Mixing rule
    logical :: uid_match, mrule_match
    real :: kij_value_db
    !
    idx = -1
    idx_default = -1
    mrule_match = .true.
    substring_index_old = 1000
    do idx_iter = 1,n_db_entries
      call get_db_entry(idx_iter, mrule_db, eos_subidx_db, uid1_db, uid2_db, ref_db, kij_value_db)
      uid_match = ((str_eq(uid1,uid1_db) .and. str_eq(uid2,uid2_db)) .or. &
           (str_eq(uid1,uid2_db) .and. str_eq(uid2,uid1_db)))
      if (len_trim(mrule) > 0) mrule_match = str_eq(mrule, mrule_db)
      if (eos_subidx == eos_subidx_db .and. uid_match .and. mrule_match) then
        if (exact_substring_match(param_ref, ref_db, substring_index)) then
          if (substring_index < substring_index_old) then
            idx = idx_iter
            substring_index_old = substring_index
          endif
        elseif (string_match("DEFAULT", ref_db)) then
          idx_default = idx_iter
        endif
      endif
    enddo
  end subroutine get_binary_db_idx

end module parameters
