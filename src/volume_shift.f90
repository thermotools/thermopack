!> Calculate potenital corrections due to volume correction
!! For documentation see memo: peneloux.pdf
!! \author MH, June 2014

module volume_shift
  use compdata, only: gendata_pointer
  use stringmod, only: str_eq, uppercase
  use thermopack_var, only: Rgas
  implicit none
  private
  save
  !> Volume shift identifyers
  integer, parameter :: NOSHIFT=0, PENELOUX=1

  public :: initVolumeShift, &
       eosVolumeFromShiftedVolume, &
       volumeShiftZfac, &
       NOSHIFT, PENELOUX, &
       redefine_volume_shift, &
       vshift_F_terms, &
       vshift_F_differential_dependencies

contains

  !----------------------------------------------------------------------
  !> Initialize volume shift parameters
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  function initVolumeShift(nc,comp,shiftId,eos, param_ref) result (volumeShiftId)
    use thermopack_constants, only: verbose
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(inout) :: comp !< Component data
    character(len=*), intent(in) :: shiftId !< String volume shif identifyer
    character(len=*), intent(in) :: eos !< Eos string
    character(len=*), optional, intent(in) :: param_ref !< Parameter set
    integer :: volumeShiftId
    ! Locals
    integer :: i
    logical :: found

    found = .false.
    if (str_eq(shiftId, 'PENELOUX')) then
      do i=1,nc
        found = get_database_ci(comp(i)%p_comp%ident,uppercase(eos),&
             comp(i)%p_comp%cid, ref=param_ref)
        if (.not. found) then
          if (verbose) &
               print *, "Using Rackett-factor correlation for Peneloux volume shift"

          if (comp(i)%p_comp%zra > 0.0) then
            if (trim(eos) == 'SRK') then
              comp(i)%p_comp%cid%ciA = 0.40768*(Rgas*comp(i)%p_comp%tc/&
                   comp(i)%p_comp%pc)*(0.29441 - comp(i)%p_comp%zra)
            else if (trim(eos) == 'PR') then
              comp(i)%p_comp%cid%ciA = 0.50033*(Rgas*comp(i)%p_comp%tc/&
                   comp(i)%p_comp%pc)*(0.25969 - comp(i)%p_comp%zra)
            else
              call stoperror('Wrong EOS for Peneloux volume shift')
            endif
          else
            print *, "NB: no volume shift parameter available for ", &
                 comp(i)%p_comp%ident
            comp(i)%p_comp%cid%ciA = 0.0
          end if
        endif
      enddo
      volumeShiftId = PENELOUX
    else
      volumeShiftId = NOSHIFT
    endif
  end function initVolumeShift

  !----------------------------------------------------------------------
  !> Get the volume to feed to the EoS, given the actual (shifted) volume
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  function eosVolumeFromShiftedVolume(nc,comp,volumeShiftId,t,v,z) result (v_eos)
    ! Transferred variables
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc), intent(in) :: comp
    integer, intent(in) :: volumeShiftId !< Volume shift identifier
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3/mol - Actual, shifted volume
    real, dimension(1:nc), intent(in) :: z !< Composition
    real :: v_eos !< m3/mol - EoS volume
    ! Locals
    real :: c, ci, cit, citt
    logical :: ci_temp_dep
    integer :: i
    !
    v_eos = v
    if (volumeShiftId == PENELOUX) then
      c = 0
      do i=1,nc
        call comp(i)%p_comp%cid%get_vol_trs_c(T, ci, cit, citt, ci_temp_dep)
        c = c + z(i)*ci
      enddo
      v_eos = v + c
    endif
  end function eosVolumeFromShiftedVolume

  !----------------------------------------------------------------------
  !> Calculate volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftZfac(nc,comp,volumeShiftId,t,p,z,phase,Zfac,dZdt,dZdp,dZdz)
    ! Transferred variables
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc), intent(in) :: comp
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: Zfac !< - Compressibillity factor
    real, optional, intent(out) :: dzdt !< 1/K - Compressibillity factor differential wrpt. temperature
    real, optional, intent(out) :: dzdp !< 1/Pa - Compressibillity factor differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dzdz !< 1/mol - Compressibillity factor differential wrpt. mol numbers
    ! Locals
    real :: c, n, ca(nc), cat(nc), citt, ct
    integer :: i
    logical :: shift_t_terms, ci_temp_dep
    !
    if (volumeShiftId == PENELOUX) then
      shift_t_terms = .false.
      do i=1,nc
        call comp(i)%p_comp%cid%get_vol_trs_c(T, ca(i), cat(i), citt, ci_temp_dep)
        if (ci_temp_dep) shift_t_terms = .true.
      enddo
      c = sum(z*ca)
      ct = sum(z*cat)
      n = sum(z)
      Zfac = Zfac - c*P/(n*Rgas*T) ! Volume corrections
      if (present(dzdt)) then
        dzdt = dzdt + c*P/(n*Rgas*T**2)
        if (shift_t_terms) dzdt = dzdt - ct*P/(n*Rgas*T)
      endif
      if (present(dzdp)) then
        dzdp = dzdp - c/(n*Rgas*T)
      endif
      if (present(dzdz)) then
        do i=1,nc
          dzdz(i) = dzdz(i) - (ca(i) - c/n)*P/(n*Rgas*T)
        enddo
      endif
    endif
  end subroutine volumeShiftZfac

  !----------------------------------------------------------------------
  !> Redefine volume shift for component j
  !>
  !> \author MH, May 2019
  !----------------------------------------------------------------------
  subroutine redefine_volume_shift(nc,j,comp,vLcurrent,vLexp)
    integer, intent(in) :: nc !< number of components
    integer, intent(in) :: j !< component
    type (gendata_pointer), dimension(nc), intent(inout) :: comp !< Component data
    real, intent(in) :: vLcurrent !< specific volume with current ci [m3/mol]
    real, intent(in) :: vLexp !< experimental volume [m3/mol]
    ! Locals
    real :: v0
    !
    ! Assume temperature independent volume shift
    v0 = vLcurrent + comp(j)%p_comp%cid%ciA  ! Volume without correction (m3/mol)
    comp(j)%p_comp%cid%ciA = v0 - vLexp  ! Updated volume-shift (m3/mol)
  end subroutine redefine_volume_shift

  !----------------------------------------------------------------------
  !> Wrapper function for locating database entries
  !>
  !> \author Morten Hammer, Nov. 2023
  !----------------------------------------------------------------------
  subroutine get_ci_db_entry(idx, eos_subidx, comp_name, ref)
    use thermopack_constants, only: ref_len, uid_len
    use eosdata, only: get_eos_index
    use compdatadb, only: cidb, maxcidb
    integer, intent(in) :: idx !< Database index
    integer, intent(out) :: eos_subidx !< Index of EOS
    character(len=uid_len), intent(out) :: comp_name !< Component name
    character(len=ref_len), intent(out) :: ref !< Reference string
    ! Locals
    integer :: eosidx
    call get_eos_index(cidb(idx)%eosid,eosidx,eos_subidx)
    comp_name = cidb(idx)%cid
    ref = cidb(idx)%ref
  end subroutine get_ci_db_entry

  !----------------------------------------------------------------------
  !> Look for volume-shift parameters in data-base
  !>
  !> \author Ailo, Sep 2020
  !----------------------------------------------------------------------
  function get_database_ci(cid,eos,cidc,ref) result (found_ci)
    use compdata, only: cidatadb
    use compdatadb, only: cidb, maxcidb
    use thermopack_constants, only: ref_len
    use eosdata, only: get_eos_index
    use parameters, only: get_pure_data_db_idx
    character(len=*), intent(in) :: eos !< Eos string
    character(len=*), intent(in) :: cid !< Component
    type(cidatadb), intent(inout) :: cidc !< Volume shift type
    character(len=*), optional, intent(in) :: ref !< Reference string
    logical :: found_ci
    ! Locals
    integer :: idx, idx_default
    integer :: eos_subidx, eosidx
    character(len=ref_len) :: ref_local
    call cidc%set_zero_vol_trs()
    if (present(ref)) then ! check if there is a match with the ref
      ref_local = ref
    else
      ref_local = "DEFAULT"
    endif
    call get_eos_index(eos, eosidx, eos_subidx)
    call get_pure_data_db_idx(get_ci_db_entry,maxcidb,"Volume translation (ci)",&
         eos_subidx,cid,ref_local,.false.,idx,idx_default)
    if (idx > 0) then
      found_ci = .true.
      cidc = cidb(idx)
    else if (idx_default > 0) then
      found_ci = .true.
      cidc = cidb(idx_default)
    else
      found_ci = .false.
    endif
  end function get_database_ci

  !----------------------------------------------------------------------
  !> Volume shift of residual, reduced Helmholtz energy, F
  !>
  !> Temperature-dependent volume shift not implemented, but easy to do
  !>
  !> \author Ailo, May 2020
  !----------------------------------------------------------------------
  subroutine vshift_F_terms(nc,comp,volumeShiftId,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn,F_VVV)
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc), intent(in) :: comp
    integer, intent(in) :: volumeShiftId !< Volume shift identifier
    real, intent(in) :: T !< K - Temperature
    real, intent(in) :: V !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mole numbers
    real, optional, intent(inout) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(inout) :: F_TT,F_TV,F_Tn(nc),F_VV,F_VVV,F_Vn(nc),F_nn(nc,nc)
    ! Locals
    integer :: i, j
    real :: C, ci, cj, sumn, Veos, ca(nc), cat(nc), catt(nc), CT, CTT, cit
    logical :: shift_t_terms, ci_temp_dep
    if (volumeShiftId == NOSHIFT) return

    shift_t_terms = .false.
    sumn = sum(n)
    do i=1,nc
      call comp(i)%p_comp%cid%get_vol_trs_c(T, ca(i), cat(i), catt(i), ci_temp_dep)
      if (ci_temp_dep) shift_t_terms = .true.
    enddo
    C = sum(n*ca)
    CT = sum(n*cat)
    CTT = sum(n*catt)
    Veos = V + C

    if (present(F_nn)) then
      do i=1,nc
        ci = ca(i)
        do j=i,nc
          cj = ca(j)
          F_nn(i,j) = F_nn(i,j) + F_Vn(i)*cj + F_Vn(j)*ci + F_VV*ci*cj - (ci+cj)/Veos + sumn*ci*cj/Veos**2
          F_nn(j,i) = F_nn(i,j)
        end do
      end do
    end if

    do i=1,nc
      ci = ca(i)
      if (present(F_Tn)) F_Tn(i) = F_Tn(i) + F_TV*ci
      if (shift_t_terms) then
        cit = cat(i)
        if (present(F_Tn)) F_Tn(i) = F_Tn(i) - (CT + sumn*ciT)/Veos &
             + sumn*CT*ci/Veos**2 + F_VV*ci*CT + F_V*ciT + F_Vn(i)*CT
      endif
      if (present(F_Vn)) F_Vn(i) = F_Vn(i) + F_VV*ci + 1/V-1/Veos + sumn*ci/Veos**2
      if (present(F_n)) F_n(i) = F_n(i) + F_V*ci + log(V/Veos) - sumn*ci/Veos
    end do

    if (shift_t_terms) then
      if (present(F_TT)) F_TT = F_TT - sumn*CTT/Veos + sumn*CT**2/Veos**2 &
           + 2*F_TV*CT + F_VV*CT**2 + F_V*CTT
      if (present(F_TV)) F_TV = F_TV + sumn*CT/Veos**2 + F_VV*CT
      if (present(F_T)) F_T = F_T - sumn*CT/Veos + F_V*CT
    endif

    if (present(F_VVV)) F_VVV = F_VVV + sumn*(2/V**3 - 2/Veos**3)
    if (present(F_VV)) F_VV = F_VV + sumn*(-1/V**2 + 1/Veos**2)
    if (present(F_V)) F_V = F_V + sumn*(1/V-1/Veos)
    if (present(F)) F = F + sumn*log(V/Veos)

  end subroutine vshift_F_terms

  !----------------------------------------------------------------------
  !> Which additional differentials are needed for volume shift correction=
  !>
  !>
  !> \author Morten Hammer, April 2023
  !----------------------------------------------------------------------
  subroutine vshift_F_differential_dependencies(nc,volumeShiftId,&
       include_F_V,include_F_TV,include_F_VV,include_F_Vn,&
       F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    integer, intent(in) :: nc
    integer, intent(in) :: volumeShiftId !< Volume shift identifier
    real, optional, intent(inout) :: F_T,F_V,F_n(nc)
    real, optional, intent(inout) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    logical, intent(out) :: include_F_V,include_F_TV,include_F_VV,include_F_Vn
    include_F_V = .false.
    include_F_TV = .false.
    include_F_VV = .false.
    include_F_Vn = .false.
    if (volumeShiftId == NOSHIFT) return

    if (.not. present(F_V)) include_F_V = present(F_T) .or. &
         present(F_TT) .or. &
         present(F_Tn) .or. &
         present(F_n)
    if (.not. present(F_VV)) include_F_VV = present(F_TT) .or. &
         present(F_nn) .or. &
         present(F_Tn) .or. &
         present(F_Vn)  .or. &
         present(F_TV)
    if (.not. present(F_TV)) include_F_TV = present(F_TT) .or. &
         present(F_Tn)
    if (.not. present(F_Vn)) include_F_Vn = present(F_Tn) .or. &
         present(F_nn)
  end subroutine vshift_F_differential_dependencies

end module volume_shift
