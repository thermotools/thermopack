!> The module eosdata contains the definitions of the equation of state, mixing
!> rule and the interaction parameters.

Module eosdata
  use stringmod, only: string_match, str_eq
  use thermopack_constants, only: short_label_len, label_len
  implicit none
  save

  integer, parameter :: eosCubic = 1   !< Cubic model
  integer, parameter :: cbSRK = 11     !< Plain SRK, Soave Redlich Kwong
  integer, parameter :: cbPR = 12      !< Peng-Robinson
  integer, parameter :: cbVdW = 13     !< Van der Waals
  integer, parameter :: cbSW = 14      !< Schmidt-Wensel
  integer, parameter :: cbPT = 15      !< Patel-Teja
  integer, parameter :: eosLK = 2      !< Lee-Kesler
  integer, parameter :: eosCSP = 3     !< Corrensponding State Principle (CSP)
  integer, parameter :: cspSRK = 31    !< CSP using SRK for scaling
  integer, parameter :: cspPR = 32     !< CSP using PR for scaling
  integer, parameter :: eosCPA = 4     !< Cubic Plus Association (CPA)
  integer, parameter :: cpaSRK = 41    !< SRK Plus Association
  integer, parameter :: cpaPR = 42     !< PR Plus Association
  integer, parameter :: eosPC_SAFT = 5      !< PC-SAFT equation of state
  integer, parameter :: eos_single = 6      !< Single component multiparamater eos
  integer, parameter :: meosMbwr19 = 611    !< MBWR19 (Bender) multiparameter equation of state
  integer, parameter :: meosMbwr32 = 612    !< MBWR32 multiparameter equation of state
  integer, parameter :: meosNist = 62       !< Multiparameter EoS on NIST-like form
  integer, parameter :: eosPT = 7           !< Perturbation theory model
  integer, parameter :: eosSAFT_VR_MIE = 71 !< SAFT-VR-MIE equation of state
  integer, parameter :: eosLJS_BH = 72      !< Lennard-Jones splined equation of state using Barker-Henderson perturbation theory
  integer, parameter :: eosPeTS = 8         !< PeTS equation of state for LJTS at 2.5*sigma
  integer, parameter :: meosNist_mix  = 9   !< Multiparameter EoS for fluids with ideal mixture

  type eos_label_mapping
    integer :: eos_idx
    integer :: eos_subidx
    character(len=short_label_len) :: short_label
    character(len=label_len) :: label
    logical :: need_alternative_eos
  end type eos_label_mapping

  integer, parameter :: max_n_eos = 18
  type(eos_label_mapping), dimension(max_n_eos), parameter :: eos_label_db = (/&
       eos_label_mapping(&
       eos_idx = eosCubic, &
       eos_subidx = cbSRK, &
       short_label = "SRK", &
       label = "Soave Redlich Kwong", &
       need_alternative_eos = .false. &
       ), &
       !
       eos_label_mapping(&
       eos_idx = eosCubic, &
       eos_subidx = cbPR, &
       short_label = "PR", &
       label = "Peng-Robinson", &
       need_alternative_eos = .false. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCubic, &
       eos_subidx = cbVdW, &
       short_label = "VDW", &
       label = "van der Waals", &
       need_alternative_eos = .false. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCubic, &
       eos_subidx = cbSW, &
       short_label = "SW", &
       label = "Schmidt-Wensel", &
       need_alternative_eos = .false. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCubic, &
       eos_subidx = cbPT, &
       short_label = "PT", &
       label = "Patel-Teja", &
       need_alternative_eos = .false. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosLK, &
       eos_subidx = eosLK, &
       short_label = "LK", &
       label = "Lee-Kesler", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCSP, &
       eos_subidx = cspSRK, &
       short_label = "CSP-SRK", &
       label = "Corrensponding state priciple with SRK scaling", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCSP, &
       eos_subidx = cspPR, &
       short_label = "CSP-PR", &
       label = "Corrensponding state priciple with PR scaling", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCPA, &
       eos_subidx = cpaSRK, &
       short_label = "CPA-SRK", &
       label = "Cubic Pluss Association Soave Redlich Kwong", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosCPA, &
       eos_subidx = cpaPR, &
       short_label = "CPA-PR", &
       label = "Cubic Pluss Association Peng Robinson", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosPC_SAFT, &
       eos_subidx = eosPC_SAFT, &
       short_label = "PC-SAFT", &
       label = "Perturbed Chain SAFT", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eos_single, &
       eos_subidx = meosMbwr19, &
       short_label = "MBWR19", &
       label = "Modified Benedict-Webb-Rubin. Bender 1970.", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eos_single, &
       eos_subidx = meosMbwr32, &
       short_label = "MBWR32", &
       label = "Modified Benedict-Webb-Rubin. Younglove and Ely 1987", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eos_single, &
       eos_subidx = meosNist, &
       short_label = "NIST_MEOS", &
       label = "Multiparameter EoS on NIST-like form", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosPT, &
       eos_subidx = eosSAFT_VR_MIE, &
       short_label = "SAFT-VR-MIE", &
       label = "SAFT for variable range Mie potentials", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosPT, &
       eos_subidx = eosLJS_BH, &
       short_label = "LJS-BH", &
       label = "LJs equation of state using BH perturbation theory", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = eosPeTS, &
       eos_subidx = eosPeTS, &
       short_label = "PETS", &
       label = "PeTS equation of state for LJTS at 2.5*sigma", &
       need_alternative_eos = .true. &
       ),&
       !
       eos_label_mapping(&
       eos_idx = meosNist_mix, &
       eos_subidx = meosNist_mix, &
       short_label = "NIST_MEOS_MIX", &
       label = "Ideal mixture of NIST multiparameter EOS", &
       need_alternative_eos = .true. &
       ) &
       /)

  integer, parameter :: nSRK = 3
  integer, parameter, dimension(nSRK) :: SRKindices = (/&
       cbSRK,&
       cspSRK,&
       cpaSRK/)

  integer, parameter :: nPR = 3
  integer, parameter, dimension(nPR) :: PRindices = (/&
       cbPR,&
       cspPR,&
       cpaPR/)

  integer, parameter :: nSAFT = 4
  integer, parameter, dimension(nSAFT) :: SAFTindices = (/&
       eosCPA, &
       eosPC_SAFT,&
       eosPT,&
       eosPeTS/)

contains

  function isSRKEOS(subeos_idx) result(isSRK)
    integer, intent(in) :: subeos_idx
    logical :: isSRK
    ! Locals
    integer :: i
    isSRK = .false.
    do i=1,nSRK
      if (SRKIndices(i) == subeos_idx) then
        isSRK = .true.
        return
      endif
    enddo
  end function isSRKEOS

  function isPREOS(subeos_idx) result(isPR)
    integer, intent(in) :: subeos_idx
    logical :: isPR
    ! Locals
    integer :: i
    isPR = .false.
    do i=1,nPR
      if (PRIndices(i) == subeos_idx) then
        isPR = .true.
        return
      endif
    enddo
  end function isPREOS

  function isSAFTEOS(eos_idx) result(isSAFT)
    integer, intent(in) :: eos_idx
    logical :: isSAFT
    ! Locals
    integer :: i
    isSAFT = .false.
    do i=1,nSAFT
      if (SAFTindices(i) == eos_idx) then
        isSAFT = .true.
        return
      endif
    enddo
  end function isSAFTEOS

  function get_eos_db_idx(short_label) result(idx)
    character(len=*), intent(in) :: short_label
    integer :: idx
    ! Locals
    integer :: i
    idx = -1
    do i=1,max_n_eos
      if (str_eq(short_label,eos_label_db(i)%short_label)) then
        idx = i
        return
      endif
    enddo
  end function get_eos_db_idx

  subroutine get_eos_index(short_label, eos_index, eos_subindex)
    character(len=*), intent(in) :: short_label
    integer, intent(out) :: eos_index, eos_subindex
    ! Locals
    integer :: idx_db
    idx_db = get_eos_db_idx(short_label)
    if (idx_db < 0) then
      call stoperror('unknown eos')
    endif
    eos_index = eos_label_db(idx_db)%eos_idx
    eos_subindex = eos_label_db(idx_db)%eos_subidx
  end subroutine get_eos_index

  function get_eos_short_label_from_subidx(subidx) result(short_label)
    character(len=short_label_len) :: short_label
    integer :: subidx
    ! Locals
    integer :: i, idx
    idx = -1
    do i=1,max_n_eos
      if (subidx == eos_label_db(i)%eos_subidx) then
        idx = i
        exit
      endif
    enddo
    if (idx > 0) then
      short_label = eos_label_db(idx)%short_label
    else
      short_label = ""
    endif
  end function get_eos_short_label_from_subidx

end module eosdata
