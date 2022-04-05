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

  !> Get the index in the PCarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getMiedataIdx(eosidx,compName,ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, ref
    integer :: idx, idx_default
    logical :: found

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nMiemodels)
      if ((eosidx==Miearray(idx)%eosidx) .and. &
           str_eq(compName, Miearray(idx)%compName)) then
        if (string_match(ref,MieArray(idx)%ref)) then
          found = .true.
          exit
        else if (string_match("DEFAULT",MieArray(idx)%ref)) then
          idx_default = idx
        endif
      endif
      idx = idx + 1
    enddo

    if (.not. found .and. idx_default > 0) then
      idx = idx_default
      found = .true.
    endif
    if (.not. found) then
       print *, "ERROR FOR COMPONENT ", compname
       call stoperror("The SAFT-VR-MIE parameters don't exist.")
    end if

  end function getMiedataIdx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database, it returns 0.0.
  function getMie_l_or_k_ij (eosidx,uid1,uid2,nMiemaxij,Mieijdb,ref) result(kijvalue)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    integer, intent(in) :: nMiemaxij
    type(Miekijdata), intent(in) :: Mieijdb(:)
    character(len=*), intent(in), optional :: ref
    real :: kijvalue
    ! Locals
    integer :: idx
    logical :: match_11_22, match_12_21
    character(len=ref_len) :: ref_local

    if (present(ref)) then
      ref_local = ref
    else
      ref_local = "DEFAULT"
    endif

    kijvalue = 0.0 ! default value if the binary is not in Miekijdb.
    idx = 1
    do idx = 1,nMiemaxij
       match_11_22 = str_eq(uid1,Mieijdb(idx)%uid1) .and. str_eq(uid2,Mieijdb(idx)%uid2)
       match_12_21 = str_eq(uid1,Mieijdb(idx)%uid2) .and. str_eq(uid2,Mieijdb(idx)%uid1)

       if ( eosidx==Mieijdb(idx)%eosidx .and. (match_11_22 .or. match_12_21) &
            .and. string_match(ref,Mieijdb(idx)%ref)) then
          kijvalue = Mieijdb(idx)%kijvalue
          exit ! Exit means "break" in Fortran.
       endif
    end do
  end function getMie_l_or_k_ij

  subroutine getMie_k_or_l_ij_allComps(nc,comp,eosidx,get_kij,ref,kij)
    use thermopack_var, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
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
           kij(ic,jc) = getMie_l_or_k_ij(eosidx,comp(ic)%p_comp%ident,&
                comp(jc)%p_comp%ident,Miemaxkij,Miekijdb,ref=ref)
         else
           kij(ic,jc) = getMie_l_or_k_ij(eosidx,comp(ic)%p_comp%ident,&
                comp(jc)%p_comp%ident,Miemaxlij,Mielijdb,ref=ref)
         endif
         kij(jc,ic) = kij(ic,jc)
       end do
    end do

  end subroutine getMie_k_or_l_ij_allComps

  !> Map SAFT-VR Mie paramaters to active component
  subroutine getSaftVrMiePureFluidParams(compName,eosidx,ref,saftvrmie_comp,fh_order,found)
    ! Input
    character(len=*), intent(in) :: compName, ref
    integer, intent(in) :: eosidx
    ! Output
    type(saftvrmie_data), intent(out) :: saftvrmie_comp
    integer, intent(out) :: fh_order
    logical, optional, intent(out) :: found
    ! Locals
    integer :: idx
    character(len=100) :: message

    idx = getMiedataIdx(eosidx,compName,ref)
    if ( idx == 0 ) then
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
  subroutine getSaftVrMieParams(nc,comp,eosidx,ref,saftvrmie_comp,fh_orders,found)
    use thermopack_var, only: gendata_pointer
    ! Input
    type(gendata_pointer), intent(inout)  :: comp(nc)    !< Component vector.
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    integer, intent(in) :: eosidx,nc
    ! Output
    type(saftvrmie_data), intent(out) :: saftvrmie_comp(nc)
    integer, intent(out) :: fh_orders(nc)
    logical, optional, intent(out) :: found
    ! Locals
    integer :: i

    do i=1,nc
       call getSaftVrMiePureFluidParams(trim(comp(i)%p_comp%ident),eosidx,ref,&
            saftvrmie_comp(i),fh_orders(i), found)
       if (present(found)) then
          if (.not. found) then
             return
          endif
       endif
    enddo
  end subroutine getSaftVrMieParams


  subroutine getSaftVrMieAssocParams_allComps(nc,comp,eosidx,ref,found,&
       eps,beta,scheme)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: ref
    ! Output
    logical, intent(out) :: found(nc)
    real, intent(out) :: eps(nc), beta(nc)
    integer, intent(out) :: scheme(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
       call getSaftVrMieAssocParams_singleComp(comp(ic)%p_comp%ident,eosidx,&
            ref,found(ic), eps(ic),beta(ic),scheme(ic))
    end do

  end subroutine getSaftVrMieAssocParams_allComps

  subroutine getSaftVrMieAssocParams_singleComp(compName,eosidx,ref,found,&
       eps,beta,scheme)
    ! Input
    character(len=*), intent(in) :: compName, ref
    integer, intent(in) :: eosidx
    ! Output
    logical, intent(out) :: found
    real, intent(out) :: eps, beta
    integer, intent(out) :: scheme
    ! Locals
    integer :: idx

    idx = getMiedataIdx(eosidx,compName,ref)
    if ( idx == 0 ) then
       found = .false.
       return
    end if

    found = .true.
    eps = Miearray(idx)%eps
    beta = Miearray(idx)%beta
    scheme = Miearray(idx)%assoc_scheme
  end subroutine getSaftVrMieAssocParams_singleComp

end module saftvrmie_parameters
