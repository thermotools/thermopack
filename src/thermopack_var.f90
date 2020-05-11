!> This is global variables for ThermoPack library when the component vector, the EOS/mixing rule
!! including interaction parameters and ideal gas CP-correlation
!! \author GS, 2012-04-13.
module tpvar
  use compdata, only: gendata
  use eosdata, only: eoscubic
  implicit none
  save
  !
  integer :: nce=0 !< Number of real components, to support apparent composition mode. Always have: nce >= nc
  real, parameter :: min_mol_num = 1.0e-150

  !< Stoichiometric matrix. Need to have dimension (nc,nce),
  !! and map relation between apparent and real components
  !! Currently used only for electrolytes. See electrolytes.f90
  real, allocatable, dimension(:,:)           :: v_stoich
  real, allocatable, dimension(:)             :: v_sum
  integer :: ncsym=0 !< Symmetrical upper left part of v_stoich

  type (gendata), allocatable, dimension(:)   :: comp !< Active component array
  type (eoscubic), allocatable, dimension(:)  :: cbeos !> Global variable for current selected cubic EOS
  type (eoscubic), allocatable, dimension(:)  :: cbeos_alternative !> Global variable for alternate cubic EOS

  public :: TP_lnfug_apparent, apparent_to_real_mole_numbers
  public :: real_to_apparent_differentials, real_to_apparent_diff
  public :: getModFugacity
  public :: get_i_cbeos

contains

  !----------------------------------------------------------------------
  !> Map apparent mole numbers to real mole numbers used by model
  subroutine apparent_to_real_mole_numbers(n,ne,nc)
    integer, intent(in) :: nc
    real, intent(in) :: n(nc)
    real, intent(out) :: ne(nce)
    ! Locals
    integer :: i, j
    if (nc == nce) then
      ne = n
    else
      ne(1:ncsym) = n(1:ncsym)
      ne(ncsym+1:nce) = 0.0
      do i=ncsym+1,nce
        do j=ncsym+1,nc
          ne(i) = ne(i) + v_stoich(j,i)*n(j)
        enddo
      enddo
    endif
  end subroutine apparent_to_real_mole_numbers

  !----------------------------------------------------------------------
  !> Map from real mole number differentials to apparent mole number
  !! differentials
  subroutine real_to_apparent_differentials(nc,Fe_n,Fe_Tn,Fe_Vn,Fe_nn,&
       F_n,F_Tn,F_Vn,F_nn)
    integer, intent(in) :: nc
    real, intent(in) :: Fe_n(nce),Fe_Tn(nce),Fe_Vn(nce),Fe_nn(nce,nce)
    real, optional, intent(out) :: F_n(nc),F_Tn(nc),F_Vn(nc),F_nn(nc,nc)
    ! Locals
    integer :: i, j, k, l
    if (present(F_n)) then
      call real_to_apparent_diff(Fe_n,F_n,nc)
    endif
    if (present(F_Tn)) then
      call real_to_apparent_diff(Fe_Tn,F_Tn,nc)
    endif
    if (present(F_Vn)) then
      call real_to_apparent_diff(Fe_Vn,F_Vn,nc)
    endif
    if (present(F_nn)) then
      if (nc == nce) then
        F_nn = Fe_nn
      else
        F_nn = 0.0
        F_nn(1:ncsym,1:ncsym) = Fe_nn(1:ncsym,1:ncsym)
        ! Solvent-salt differentials
        do k=1,ncsym ! Solvent
          do i=ncsym+1,nc ! Salt
            do j=ncsym+1,nce ! Ion
              F_nn(k,i) = F_nn(k,i) + Fe_nn(k,j)*v_stoich(i,j)
              F_nn(i,k) = F_nn(i,k) + Fe_nn(j,k)*v_stoich(i,j)
            enddo
          enddo
        enddo
        ! Salt-salt differentials
        do k=ncsym+1,nc ! Salt1
          do i=ncsym+1,nc ! Salt2
            do j=ncsym+1,nce ! Ions of salt 1
              do l=ncsym+1,nce ! Ions of salt 2
                F_nn(k,i) = F_nn(k,i) + Fe_nn(l,j)*v_stoich(k,j)*v_stoich(i,l)
              enddo
            enddo
          enddo
        enddo
      endif
    endif
  end subroutine real_to_apparent_differentials

  !----------------------------------------------------------------------
  !> Map from real mole number differential to apparent mole number
  !! differentials
  subroutine real_to_apparent_diff(Fe_n,F_n,nc)
    integer, intent(in) :: nc
    real, intent(in) :: Fe_n(nce)
    real, intent(out) :: F_n(nc)
    ! Locals
    integer :: i, j
    if (nc == nce) then
      F_n = Fe_n
    else
      F_n(1:ncsym) = Fe_n(1:ncsym)
      F_n(ncsym+1:nc) = 0.0
      do i=ncsym+1,nc ! Salt
        do j=ncsym+1,nce ! Ion
          F_n(i) = F_n(i) + Fe_n(j)*v_stoich(i,j)
        enddo
      enddo
    endif
  end subroutine real_to_apparent_diff

  !-----------------------------------------------------------------------------
  !> Convert logarithmic fugacity coefficient from real to apparent composition
  !!
  !! \author Morten Hammer, 2017-03
  !-----------------------------------------------------------------------------
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
    ! Locals.
    real :: sumn, sumne  !< Total mole number in mixture [mol]
    real :: dik
    real :: xe(nce), x(nc), logP
    integer :: i,j,k,l
    if (nc == nce) then
      ! Apparent is real composition
      lnfug = lnfug_real
      if (present(dlnfugdt)) then
        dlnfugdt = dlnfugdt_real
      endif
      if (present(dlnfugdp)) then
        dlnfugdp = dlnfugdp_real
      endif
      if (present(dlnfugdn)) then
        dlnfugdn = dlnfugdn_real
      endif
    else
      sumn = sum(n)
      x = n/sumn
      sumne = sum(ne)
      xe = ne/sumne
      logP = log(P)

      lnfug(1:ncsym) = lnfug_real(1:ncsym)  + log(sumn/sumne)
      do i=ncsym+1,nc
        if (x(i) > 0.0) then
          lnfug(i) = 0.0
          do j=ncsym+1,nce
            lnfug(i) = lnfug(i) + v_stoich(i,j)*(log(xe(j)) + lnfug_real(j))
          enddo
          lnfug(i) = lnfug(i) - log(x(i))
        else
          lnfug(i) = 0.0
        endif
      enddo

      if (present(dlnfugdt)) then
        call real_to_apparent_diff(dlnfugdt_real,dlnfugdt,nc)
      endif
      if (present(dlnfugdp)) then
        call real_to_apparent_diff(dlnfugdp_real,dlnfugdp,nc)
      endif
      if (present(dlnfugdn)) then
        dlnfugdn = 0.0
        ! Upper left matrix (ncsym)x(ncsym)
        do i=1,ncsym
          do j=1,ncsym
            dlnfugdn(i,j) = dlnfugdn_real(i,j) - 1.0/sumne + 1.0/sumn
          end do
        end do
        ! Lower right matrix (nc-ncsym)x(nc-ncsym)
        do k=ncsym+1,nc ! App k
          do i=ncsym+1,nc ! App i
            do j=ncsym+1,nce ! Real j
              do l=ncsym+1,nce ! Real l
                dlnfugdn(k,i) = dlnfugdn(k,i) + dlnfugdn_real(l,j)*v_stoich(k,j)*v_stoich(i,l)
              enddo
            enddo
            if (i == k) then
              if (n(k) > 0.0) then
                dik = 1.0/max(n(k),min_mol_num)
              else
                dik = 0.0
              endif
            else
              dik = 0.0
            endif
            dlnfugdn(k,i) = dlnfugdn(k,i) + 1.0/sumn - dik - v_sum(k)*v_sum(i)/sumne
            do j=ncsym+1,nce ! Real j
              if (ne(j) > 0.0) then
                dlnfugdn(k,i) = dlnfugdn(k,i) + v_stoich(k,j)*v_stoich(i,j)/max(ne(j),min_mol_num)
              endif
            enddo
          enddo
        enddo

        ! Lower and left and upper right (ncsym)x(nc-ncsym) and (nc-ncsym)x(ncsym)
        do k=1,ncsym ! App k
          do i=ncsym+1,nc ! App i
            do j=ncsym+1,nce ! Real j
              dlnfugdn(k,i) = dlnfugdn(k,i) + dlnfugdn_real(k,j)*v_stoich(i,j)
              dlnfugdn(i,k) = dlnfugdn(i,k) + dlnfugdn_real(j,k)*v_stoich(i,j)
            enddo
            dlnfugdn(k,i) = dlnfugdn(k,i) + 1.0/sumn - v_sum(i)/sumne
            dlnfugdn(i,k) = dlnfugdn(i,k) + 1.0/sumn - v_sum(i)/sumne
          enddo
        enddo
      endif
    endif
  end subroutine TP_lnfug_apparent

  !---------------------------------------------------------------------------
  !> Back calculate logaritm of fugacity coefficient, by removing
  !! dependency of log(x) and log(xe)
  !!
  !! \author MH, 2017-05
  !---------------------------------------------------------------------------
  subroutine getModFugacity(t,p,X,lnfug,sumne)
    use parameters, only: nc
    implicit none
    real, intent(in) :: t !< Temperature (K)
    real, intent(in) :: p !< Pressure (Pa)
    real, dimension(nc), intent(in) :: X !< Phase compozition
    real, intent(inout) :: lnfug(nc)
    real, intent(out) :: sumne
    ! Locals
    integer :: i, k
    real :: Xe(nce)
    call apparent_to_real_mole_numbers(X,Xe,nc)
    sumne = sum(Xe)
    Xe = Xe/sumne
    do i=ncsym+1,nc
      lnfug(i) = lnfug(i) + log(X(i))
      do k=ncsym+1,nce
        if (v_stoich(i,k) > 0) then
          lnfug(i) = lnfug(i) - v_stoich(i,k)*log(xe(k))
          lnfug(i) = lnfug(i) + v_stoich(i,k)*log(v_stoich(i,k))
        endif
      enddo
    enddo
  end subroutine getModFugacity

  function get_i_cbeos() result(i_cbeos)
    !$ use omp_lib, only: omp_get_thread_num
    integer :: i_cbeos
    i_cbeos = 1
    !$ i_cbeos = 1 + omp_get_thread_num()
  end function get_i_cbeos

end module tpvar
