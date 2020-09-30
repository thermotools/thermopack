!> Calculate potenital corrections due to volume correction
!! For documentation see memo: peneloux.pdf
!! \author MH, June 2014

module volume_shift
  use compdata, only: gendata_pointer
  use stringmod, only: str_eq, uppercase, string_match, string_match_val
  use thermopack_constants, only: Rgas
  implicit none
  save
  !> Volume shift identifyers
  integer, parameter :: NOSHIFT=0, PENELOUX=1

  public :: initVolumeShift, &
            volumeShiftVolume, &
            volumeShiftZfac, &
            NOSHIFT, PENELOUX, &
            redefine_volume_shift, &
            vshift_F_terms

contains

  !----------------------------------------------------------------------
  !> Initialize volume shift parameters
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  function initVolumeShift(nc,comp,shiftId,eos, param_ref) result (volumeShiftId)
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
          found = get_database_ci(comp(i)%p_comp%ident,uppercase(eos),comp(i)%p_comp%ci, ref=param_ref)
          if (.not. found) then
             print *, "Using Rackett-factor correlation for Peneloux volume shift"

             if (comp(i)%p_comp%zra > 0.0) then
                if (trim(eos) == 'SRK') then
                   comp(i)%p_comp%ci = 0.40768*(Rgas*comp(i)%p_comp%tc/comp(i)%p_comp%pc)*&
                        (0.29441 - comp(i)%p_comp%zra)
                else if (trim(eos) == 'PR') then
                   comp(i)%p_comp%ci = 0.50033*(Rgas*comp(i)%p_comp%tc/comp(i)%p_comp%pc)*&
                        (0.25969 - comp(i)%p_comp%zra)
                else
                   call stoperror('Wrong EOS for Peneloux volume shift')
                endif
             else
                print *, "NB: no volume shift parameter available for ", comp(i)%p_comp%ident
                comp(i)%p_comp%ci = 0.0
             end if
          endif
       enddo
       volumeShiftId = PENELOUX
    else
       volumeShiftId = NOSHIFT
    endif
  end function initVolumeShift

  !----------------------------------------------------------------------
  !> Calculate volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftVolume(nc,comp,volumeShiftId,t,z,v,dvdt,dvdp,dvdz,dvdn_TV)
    ! Transferred variables
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc), intent(in) :: comp
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    real, intent(in) :: t !< K - Temperature
    real, dimension(1:nc), intent(in) :: z !< Compozition
    real, intent(out) :: v !< m3/mol - Specific volume
    real, optional, intent(out) :: dvdt !< m3/mol/K - Specific volume differential wrpt. temperature
    real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specific volume differential wrpt. pressure
    real, optional, dimension(1:nc), intent(out) :: dvdz !< m3/mol - Specific volume differential wrpt. mol numbers
    real, optional, dimension(1:nc), intent(out) :: dvdn_TV !< m3/mol - Mol number differentials at constant T and unshifted volume
    ! Locals
    real :: c
    integer :: i
    !
    if (volumeShiftId == PENELOUX) then
      c = 0.0
      do i=1,nc
        c = c + z(i)*comp(i)%p_comp%ci
        if (present(dvdz)) then
          dvdz = dvdz - comp(i)%p_comp%ci
        endif
        if (present(dvdn_TV)) then
          dvdn_TV(i) = - comp(i)%p_comp%ci
        endif
      enddo
      v = v - c ! Volume corrections
    endif
  end subroutine volumeShiftVolume

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
    real :: c
    integer :: i
    !
    v_eos = v
    if (volumeShiftId == PENELOUX) then
       c = 0.0
       do i=1,nc
          c = c + z(i)*comp(i)%p_comp%ci
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
    real :: c,n
    integer :: i
    !
    if (volumeShiftId == PENELOUX) then
      c = 0.0
      n = 0.0
      do i=1,nc
        c = c + z(i)*comp(i)%p_comp%ci
        n = n + z(i)
      enddo
      Zfac = Zfac - c*P/(n*Rgas*T) ! Volume corrections
      if (present(dzdt)) then
        dzdt = dzdt + c*P/(n*Rgas*T**2)
      endif
      if (present(dzdp)) then
        dzdp = dzdp - c/(n*Rgas*T)
      endif
      if (present(dzdz)) then
        do i=1,nc
          dzdz(i) = dzdz(i) - (comp(i)%p_comp%ci - c/n)*P/(n*Rgas*T)
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
    real, intent(in) :: vLcurrent !< specific volume with current ci [m3/kmol]
    real, intent(in) :: vLexp !< experimental volume [m3/kmol]
    ! Locals
    real :: v0
    !
    v0 = vLcurrent + comp(j)%p_comp%ci  ! Volume without correction (m3/kmol)
    comp(j)%p_comp%ci = v0 - vLexp  ! Updated volume-shift (m3/kmol)
  end subroutine redefine_volume_shift

  !----------------------------------------------------------------------
  !> Look for volume-shift parameters in data-base
  !>
  !> \author Ailo, Sep 2020
  !----------------------------------------------------------------------
  function get_database_ci(cid,eos,ci,ref) result (found_ci)
    use compdatadb, only: cidb, maxcidb
    character(len=*), intent(in) :: eos !< Eos string
    character(len=*), intent(in) :: cid !< Component
    real, intent(out) :: ci !< Volume shift
    character(len=*), optional, intent(in) :: ref !< Reference string
    logical :: found_ci, match
    integer :: idx, idx_lowest, match_val
    ! Locals
    integer :: i
    found_ci = .false.
    ci = 0
    idx_lowest = 100000
    do i=1,maxcidb
       if (str_eq(cidb(i)%eosid, eos) .and. str_eq(cidb(i)%cid, cid)) then
          if (.not. found_ci) then ! we at least found one match
             ci = cidb(i)%ci
          end if
          found_ci = .true.

          if (present(ref)) then ! check if there is a match with the ref
             call string_match_val(ref,cidb(i)%ref,match,match_val)
             if (match .and. match_val<idx_lowest) then ! the match takes precedence
                idx_lowest = match_val
                ci = cidb(i)%ci
             end if
          end if

       endif
    enddo
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
    real :: C, ci, cj, sumn, Veos

    if (volumeShiftId == NOSHIFT) return

    sumn = sum(n)
    C = 0.0
    do i=1,nc
       C = C + n(i)*comp(i)%p_comp%ci
    end do
    Veos = V + C

    if (present(F_nn)) then
       do i=1,nc
          ci = comp(i)%p_comp%ci
          do j=i,nc
             cj = comp(j)%p_comp%ci
             F_nn(i,j) = F_nn(i,j) + F_Vn(i)*cj + F_Vn(j)*ci + F_VV*ci*cj - (ci+cj)/Veos + sumn*ci*cj/Veos**2
             F_nn(j,i) = F_nn(i,j)
          end do
       end do
    end if

    do i=1,nc
       ci = comp(i)%p_comp%ci
       if (present(F_Tn)) F_Tn(i) = F_Tn(i) + F_TV*ci
       if (present(F_Vn)) F_Vn(i) = F_Vn(i) + F_VV*ci + 1/V-1/Veos + sumn*ci/Veos**2
       if (present(F_n)) F_n(i) = F_n(i) + F_V*ci + log(V/Veos) - sumn*ci/Veos
    end do

    if (present(F_VVV)) F_VVV = F_VVV + sumn*(2/V**3 - 2/Veos**3)
    if (present(F_VV)) F_VV = F_VV + sumn*(-1/V**2 + 1/Veos**2)
    if (present(F_V)) F_V = F_V + sumn*(1/V-1/Veos)
    if (present(F)) F = F + sumn*log(V/Veos)

  end subroutine vshift_F_terms

end module volume_shift
