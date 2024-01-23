!---------------------------------------------------------------------
! Module containing the parameters for SAFT-VRQ Mie
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------
module saftvrmie_parameters
  use saftvrmie_datadb
  use thermopack_constants, only: ref_len
  implicit none
  save

contains

  subroutine get_pure_saftvrmie_db_entry(idx, eos_subidx, comp_name, ref)
    use thermopack_constants, only: ref_len, uid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=uid_len), intent(out) :: comp_name !< Component name
    character(len=ref_len), intent(out) :: ref !< Reference string
    eos_subidx = Miearray(idx)%eosidx
    comp_name = Miearray(idx)%compName
    ref = Miearray(idx)%ref
  end subroutine get_pure_saftvrmie_db_entry

  subroutine get_binary_saftvrmie_kij_db_entry(idx, mrule, eos_subidx, uid1, uid2, ref, kijvalue)
    use thermopack_constants, only: ref_len, uid_len, eosid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=eosid_len), intent(out) :: mrule !< Mixing rule
    character(len=uid_len), intent(out) :: uid1, uid2 !< Component names
    character(len=ref_len), intent(out) :: ref !< Reference string
    real, intent(out) :: kijvalue !< Interaction parameter
    uid1 = Miekijdb(idx)%uid1
    uid2 = Miekijdb(idx)%uid2
    eos_subidx = Miekijdb(idx)%eosidx
    ref = Miekijdb(idx)%ref
    mrule = "" ! Dummy
    kijvalue = Miekijdb(idx)%kijvalue
  end subroutine get_binary_saftvrmie_kij_db_entry

  subroutine get_binary_saftvrmie_lij_db_entry(idx, mrule, eos_subidx, uid1, uid2, ref, kijvalue)
    use thermopack_constants, only: ref_len, uid_len, eosid_len
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=eosid_len), intent(out) :: mrule !< Mixing rule
    character(len=uid_len), intent(out) :: uid1, uid2 !< Component names
    character(len=ref_len), intent(out) :: ref !< Reference string
    real, intent(out) :: kijvalue !< Interaction parameter
    uid1 = Mielijdb(idx)%uid1
    uid2 = Mielijdb(idx)%uid2
    eos_subidx = Mielijdb(idx)%eosidx
    ref = Mielijdb(idx)%ref
    mrule = "" ! Dummy
    kijvalue = Mielijdb(idx)%kijvalue
  end subroutine get_binary_saftvrmie_lij_db_entry

  !> Get the index in the PCarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getMiedataIdx(eos_subidx,comp_name,ref) result(idx)
    use parameters, only: get_pure_data_db_idx
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: comp_name, ref
    integer :: idx
    ! Locals
    integer :: idx_default
    call get_pure_data_db_idx(get_pure_saftvrmie_db_entry,nMiemodels,"SAFT-VR-MIE",&
         eos_subidx,comp_name,ref,.true.,idx,idx_default)
  end function getMiedataIdx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database, it returns 0.0.
  function getMie_kij(eos_subidx,uid1,uid2,ref) result(kijvalue)
    use parameters, only: get_binary_interaction_parameter
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: uid1, uid2
    character(len=*), intent(in), optional :: ref
    real :: kijvalue
    ! Locals
    integer :: idx, idx_default
    character(len=ref_len) :: ref_local
    if (present(ref)) then
      ref_local = ref
    else
      ref_local = "DEFAULT"
    endif
    kijvalue = get_binary_interaction_parameter(get_binary_saftvrmie_kij_db_entry,&
         Miemaxkij,"SAFT-VR Mie",&
         "",eos_subidx,uid1,uid2,ref_local,0.0,idx,idx_default)
  end function getMie_kij

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no lij is stored in the database, it returns 0.0.
  function getMie_lij(eos_subidx,uid1,uid2,ref) result(kijvalue)
    use parameters, only: get_binary_interaction_parameter
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: uid1, uid2
    character(len=*), intent(in), optional :: ref
    real :: kijvalue
    ! Locals
    integer :: idx, idx_default
    character(len=ref_len) :: ref_local
    if (present(ref)) then
      ref_local = ref
    else
      ref_local = "DEFAULT"
    endif
    kijvalue = get_binary_interaction_parameter(get_binary_saftvrmie_lij_db_entry,&
         Miemaxlij,"SAFT-VR Mie",&
         "",eos_subidx,uid1,uid2,ref_local,0.0,idx,idx_default)
  end function getMie_lij

  subroutine getMie_k_or_l_ij_allComps(nc,comp,eos_subidx,get_kij,ref,kij)
    use thermopack_var, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eos_subidx
    logical, intent(in) :: get_kij
    character(len=*), intent(in), optional :: ref
    ! Output
    real, intent(out) :: kij(nc,nc)
    ! Locals
    integer :: ic,jc

    kij = 0.0
    do ic=1,nc
       do jc=ic+1,nc
         if (get_kij) then
           kij(ic,jc) = getMie_kij(eos_subidx,comp(ic)%p_comp%ident,&
                comp(jc)%p_comp%ident,ref=ref)
         else
           kij(ic,jc) = getMie_lij(eos_subidx,comp(ic)%p_comp%ident,&
                comp(jc)%p_comp%ident,ref=ref)
         endif
         kij(jc,ic) = kij(ic,jc)
       end do
    end do

  end subroutine getMie_k_or_l_ij_allComps

  !> Map SAFT-VR Mie paramaters to active component
  subroutine getSaftVrMiePureFluidParams(compName,eos_subidx,ref,saftvrmie_comp,fh_order,found)
    ! Input
    character(len=*), intent(in) :: compName, ref
    integer, intent(in) :: eos_subidx
    ! Output
    type(saftvrmie_data), intent(out) :: saftvrmie_comp
    integer, intent(out) :: fh_order
    logical, optional, intent(out) :: found
    ! Locals
    integer :: idx
    character(len=100) :: message

    idx = getMiedataIdx(eos_subidx,compName,ref)
    if ( idx <= 0 ) then
       if (present(found)) then
          found = .false.
       else
          write(message,*) "saftvrmie_interface::getSaftVrMiePureFluidParams"//&
               "No SAFT-VR Mie paramaters for "//trim(compName)
          call stoperror(trim(message))
       endif
       return
    else
       if (present(found)) then
          found = .true.
       endif
    end if

    saftvrmie_comp%compname = Miearray(idx)%compname
    saftvrmie_comp%eosidx = Miearray(idx)%eosidx
    saftvrmie_comp%m = Miearray(idx)%m
    saftvrmie_comp%sigma = Miearray(idx)%sigma
    saftvrmie_comp%eps_depth_divk = Miearray(idx)%eps_depth_divk
    saftvrmie_comp%lambda_r = Miearray(idx)%lambda_r
    saftvrmie_comp%lambda_a = Miearray(idx)%lambda_a
    saftvrmie_comp%eps = Miearray(idx)%eps
    saftvrmie_comp%beta = Miearray(idx)%beta
    saftvrmie_comp%assoc_scheme = Miearray(idx)%assoc_scheme
    saftvrmie_comp%mass = Miearray(idx)%mass
    saftvrmie_comp%eps_depth_divk = Miearray(idx)%eps_depth_divk
    fh_order = Miearray(idx)%fh_order
    saftvrmie_comp%fh_order = Miearray(idx)%fh_order
    saftvrmie_comp%bib_ref = Miearray(idx)%bib_ref
    saftvrmie_comp%ref = Miearray(idx)%ref
  end subroutine getSaftVrMiePureFluidParams

  !> Map SAFT-VR Mie paramaters to active components
  subroutine getSaftVrMieParams(nc,comp,eos_subidx,ref,saftvrmie_comp,fh_orders,found)
    use thermopack_var, only: gendata_pointer
    ! Input
    type(gendata_pointer), intent(inout)  :: comp(nc)    !< Component vector.
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    integer, intent(in) :: eos_subidx,nc
    ! Output
    type(saftvrmie_data), intent(out) :: saftvrmie_comp(nc)
    integer, intent(out) :: fh_orders(nc)
    logical, optional, intent(out) :: found
    ! Locals
    integer :: i

    do i=1,nc
       call getSaftVrMiePureFluidParams(trim(comp(i)%p_comp%ident),eos_subidx,ref,&
            saftvrmie_comp(i),fh_orders(i), found)
       if (present(found)) then
          if (.not. found) then
             return
          endif
       endif
    enddo
  end subroutine getSaftVrMieParams


  subroutine getSaftVrMieAssocParams_allComps(nc,comp,eos_subidx,ref,found,&
       eps,beta,scheme)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eos_subidx
    character(len=*), intent(in) :: ref
    ! Output
    logical, intent(out) :: found(nc)
    real, intent(out) :: eps(nc), beta(nc)
    integer, intent(out) :: scheme(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
       call getSaftVrMieAssocParams_singleComp(comp(ic)%p_comp%ident,eos_subidx,&
            ref,found(ic), eps(ic),beta(ic),scheme(ic))
    end do

  end subroutine getSaftVrMieAssocParams_allComps

  subroutine getSaftVrMieAssocParams_singleComp(compName,eos_subidx,ref,found,&
       eps,beta,scheme)
    use thermopack_var, only: Rgas
    ! Input
    character(len=*), intent(in) :: compName, ref
    integer, intent(in) :: eos_subidx
    ! Output
    logical, intent(out) :: found
    real, intent(out) :: eps, beta
    integer, intent(out) :: scheme
    ! Locals
    integer :: idx

    idx = getMiedataIdx(eos_subidx,compName,ref)
    if ( idx == 0 ) then
       found = .false.
       return
    end if

    found = .true.
    eps = Miearray(idx)%eps*Rgas
    beta = Miearray(idx)%beta
    scheme = Miearray(idx)%assoc_scheme
  end subroutine getSaftVrMieAssocParams_singleComp

end module saftvrmie_parameters
