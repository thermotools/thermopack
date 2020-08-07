!> Calculate potenital corrections due to volume correction
!! For documentation see memo: peneloux.pdf
!! \author MH, June 2014

module volume_shift
  implicit none
  save
  !> Volume shift identifyers
  integer, parameter :: NOSHIFT=0, PENELOUX=1

  public :: initVolumeShift, &
            volumeShiftGibbs, &
            volumeShiftEnthalpy, &
            volumeShiftEntropy, &
            volumeShiftFugacity, &
            volumeShiftVolume, &
            volumeShiftInternalEnergy, &
            volumeShiftZfac, &
            NOSHIFT, PENELOUX, &
            redefine_volume_shift

contains

  !----------------------------------------------------------------------
  !> Initialize volume shift parameters
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  function initVolumeShift(nc,comp,shiftId,eos) result (volumeShiftId)
    use stringmod, only: str_eq
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    implicit none
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(inout) :: comp !< Component data
    character(len=*), intent(in) :: shiftId !< String volume shif identifyer
    character(len=*), intent(in) :: eos !< Eos string
    integer :: volumeShiftId
    ! Locals
    integer :: i

    if (str_eq(shiftId, 'PENELOUX')) then
      do i=1,nc
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
          !comp(i)%zra = 0.29056 - 0.08775*comp(i)%acf
          comp(i)%p_comp%ci = 0.0
        endif
      enddo
      volumeShiftId = PENELOUX
    else
      volumeShiftId = NOSHIFT
    endif
  end function initVolumeShift

  !----------------------------------------------------------------------
  !> Enthalpy change due to volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftEnthalpy(nc,comp,volumeShiftId,T,P,Z,phase,h,dhdt,dhdp,dhdz)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(in) :: comp !< Component data
    real, dimension(nc), intent(in) :: Z !< The mol fraction [-]
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(out) :: h !< Enthalpy [J/mol]
    real, optional, intent(out) :: dhdt !< Temperature derivative [J/(mol K)]
    real, optional, intent(out) :: dhdp !< Pressure derivative [J/(mol Pa)]
    real, optional, dimension(nc), intent(out) :: dhdz !< Mol number differential [J/mol^2]
    integer, intent(in) :: phase !< Phase identifyer
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    ! Locals
    real :: c
    integer :: i
    !
    if (volumeShiftId == PENELOUX) then
      c = 0.0
      do i=1,nc
        c = c + Z(i)*comp(i)%p_comp%ci
      enddo
      h = h - P*c
      if (present(dhdp)) then
        dhdp = dhdp - c
      endif
      if (present(dhdz)) then
        do i=1,nc
          dhdz(i) = dhdz(i) - P*comp(i)%p_comp%ci
        enddo
      endif
    endif

  end subroutine volumeShiftEnthalpy

  !----------------------------------------------------------------------
  !> Entropy change due to volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftEntropy(nc,comp,volumeShiftId,T,P,Z,phase,s,dsdt,dsdp,dsdz)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(in) :: comp !< Component data
    real, dimension(nc), intent(in) :: Z !< The mol fraction [-]
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(out) :: s !< Entropy [J/(mol K)]
    real, optional, intent(out) :: dsdt !< Temperature derivative [J/(mol K^2)]
    real, optional, intent(out) :: dsdp !< Pressure derivative [J/(mol Pa K)]
    real, optional, dimension(nc), intent(out) :: dsdz !< Mol number differential [J/(K mol^2)]
    integer, intent(in) :: phase !< Phase identifyer
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    ! Locals

  end subroutine volumeShiftEntropy

  !----------------------------------------------------------------------
  !> Internal energy change due to volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftInternalEnergy(nc,comp,volumeShiftId,T,P,Z,phase,u,dudt,dudp,dudz)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(in) :: comp !< Component data
    real, dimension(nc), intent(in) :: Z !< The mol fraction [-]
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(out) :: u !< Internal energy [J/mol]
    real, optional, intent(out) :: dudt !< Temperature derivative [J/(mol K)]
    real, optional, intent(out) :: dudp !< Pressure derivative [J/(mol Pa)]
    real, optional, dimension(nc), intent(out) :: dudz !< Mol number differential [J/mol^2]
    integer, intent(in) :: phase !< Phase identifyer
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    ! Locals

  end subroutine volumeShiftInternalEnergy

  !----------------------------------------------------------------------
  !> Gibbs free energy change due to volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftGibbs(nc,comp,volumeShiftId,T,P,Z,phase,g,dgdt,dgdp)
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(in) :: comp !< Component data
    real, dimension(nc), intent(in) :: Z !< The mol fraction [-]
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(out) :: g !< Gibbs free energy [J/mol]
    real, optional, intent(out) :: dgdt !< Temperature derivative [J/(mol K)]
    real, optional, intent(out) :: dgdp !< Pressure derivative [J/(mol Pa)]
    integer, intent(in) :: phase !< Phase identifyer
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    ! Locals
    integer :: i
    real :: nxc

    if (volumeShiftId == PENELOUX) then
      nxc = 0.0
      do i=1,nc
        nxc = nxc +  Z(i)*comp(i)%p_comp%ci
      enddo
      g = g - P*nxc
      if (present(dgdp)) then
        dgdp = dgdp - nxc
      endif
    endif
  end subroutine volumeShiftGibbs

  !----------------------------------------------------------------------
  !> Calculate fugacity corrections from volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftFugacity(nc,comp,volumeShiftId,T,P,Z,phase,lnfug,dlnfdt,dlnfdp,dlnfdz)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    implicit none
    integer, intent(in) :: nc !< Number of components
    type (gendata_pointer), dimension(nc), intent(in) :: comp !< Component data
    real, dimension(nc), intent(in) :: Z !< The mol fraction [-]
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: P !< Pressure [Pa]
    integer, intent(in) :: phase !< Phase identifyer
    integer, intent(in) :: volumeShiftId !< Volume shift identifyer
    real, dimension(nc), intent(out) :: lnfug !< Fugacity logarithm [-]
    real, optional, dimension(nc), intent(out) :: dlnfdt !< Temperature differential [1/K]
    real, optional, dimension(nc), intent(out) :: dlnfdp !< Pressure differential [1/Pa]
    real, optional, dimension(nc,nc), intent(out) :: dlnfdz !< Mol number differential [1/mol]

    ! Locals
    integer :: j
    if (volumeShiftId == PENELOUX) then
      do j=1,nc
        lnfug(j) = lnfug(j) - comp(j)%p_comp%ci*P/(Rgas*T)
      enddo
      if (present(dlnfdp)) then
        do j=1,nc
          dlnfdp(j) = dlnfdp(j) - comp(j)%p_comp%ci/(Rgas*T)
        enddo
      endif
      if (present(dlnfdt)) then
        do j=1,nc
          dlnfdt(j) = dlnfdt(j) + P*comp(j)%p_comp%ci/(Rgas*T**2)
        enddo
      endif
    endif

    end subroutine volumeShiftFugacity

  !----------------------------------------------------------------------
  !> Calculate volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftVolume(nc,comp,volumeShiftId,t,z,v,dvdt,dvdp,dvdz,dvdn_TV)
    use compdata, only: gendata_pointer
    implicit none
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
  !> Calculate volume shift
  !>
  !> \author MH, June 2014
  !----------------------------------------------------------------------
  subroutine volumeShiftZfac(nc,comp,volumeShiftId,t,p,z,phase,Zfac,dZdt,dZdp,dZdz)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    implicit none
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
    use compdata, only: gendata_pointer
    implicit none
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

end module volume_shift
