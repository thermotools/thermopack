!> Code for handling apparent composition approach
!! See XMHX
!! \author Morten Hammer, 2020
module apparent_compostion
  use thermopack_constants, only: min_mol_num
  implicit none
  save
  !
  type :: apparent_container
    real, allocatable, dimension(:,:)           :: v_stoich
    real, allocatable, dimension(:)             :: v_sum
    integer :: nc=0
    integer :: nce=0
    integer :: ncsym=0 !< Symmetrical upper left part of v_stoich
  contains
    procedure, public       :: apparent_to_real_mole_numbers
    procedure, public       :: real_to_apparent_differentials
    procedure, public       :: real_to_apparent_diff
    procedure, public       :: TP_lnfug_apparent
    procedure, public       :: getModFugacity
    procedure, public       :: update_v_sum
    procedure, public       :: set_v_stoich_ij
    procedure, public       :: dealloc
  end type apparent_container

  public :: apparent_container
  public :: apparent_constructor

contains

  function apparent_constructor(nc,nce) result(apparent)
    ! Input:
    ! Created object:
    type(apparent_container) :: apparent
    integer, intent(in) :: nce, nc
    ! Locals
    integer :: ierr
    ! Set up overall matrix for stoichiometrics matrix
    allocate(apparent%v_stoich(nc,nce),apparent%v_sum(nc),stat=ierr)
    if (ierr /= 0) then
      call stoperror('Not able to allocate v_stoich memory')
    endif
    apparent%v_stoich = 0
    apparent%v_sum = 0
    apparent%ncsym = nc
    apparent%nc = nc
    apparent%nce = nce
  end function apparent_constructor

  subroutine dealloc(apparent)
    ! Input:
    ! Created object:
    class(apparent_container), intent(inout) :: apparent
    ! Locals
    integer :: ierr
    ierr = 0
    if (allocated(apparent%v_stoich)) deallocate(apparent%v_stoich,stat=ierr)
    if (ierr /= 0) print *,'apparent: Not able to allocate v_stoich memory'
    ierr = 0
    if (allocated(apparent%v_sum)) deallocate(apparent%v_sum,stat=ierr)
    if (ierr /= 0) print *,'apparent: Not able to allocate v_sum memory'
  end subroutine dealloc

  subroutine set_v_stoich_ij(apparent,i,j,v)
    ! Passes object:
    class(apparent_container), intent(inout) :: apparent
    ! Input
    integer, intent(in) :: i, j
    real, intent(in) :: v
    apparent%v_stoich(i,j) = v
  end subroutine set_v_stoich_ij

  subroutine update_v_sum(apparent)
    ! Passes object:
    class(apparent_container), intent(inout) :: apparent
    ! Locals
    integer :: i
    do i=1,apparent%nc
      apparent%v_sum(i) = sum(apparent%v_stoich(i,:))
    enddo
  end subroutine update_v_sum

  !----------------------------------------------------------------------
  !> Map apparent mole numbers to real mole numbers used by model
  subroutine apparent_to_real_mole_numbers(apparent,n,ne)
    ! Passes object:
    class(apparent_container), intent(in) :: apparent
    real, intent(in) :: n(apparent%nc)
    real, intent(out) :: ne(apparent%nce)
    ! Locals
    integer :: i, j
    if (apparent%nc == apparent%nce) then
      ne = n
    else
      ne(1:apparent%ncsym) = n(1:apparent%ncsym)
      ne(apparent%ncsym+1:apparent%nce) = 0.0
      do i=apparent%ncsym+1,apparent%nce
        do j=apparent%ncsym+1,apparent%nc
          ne(i) = ne(i) + apparent%v_stoich(j,i)*n(j)
        enddo
      enddo
    endif
  end subroutine apparent_to_real_mole_numbers

  !----------------------------------------------------------------------
  !> Map from real mole number differentials to apparent mole number
  !! differentials
  subroutine real_to_apparent_differentials(apparent,Fe_n,Fe_Tn,Fe_Vn,Fe_nn,&
       F_n,F_Tn,F_Vn,F_nn)
    ! Passes object:
    class(apparent_container), intent(in) :: apparent
    real, intent(in) :: Fe_n(apparent%nce),Fe_Tn(apparent%nce),Fe_Vn(apparent%nce),Fe_nn(apparent%nce,apparent%nce)
    real, optional, intent(out) :: F_n(apparent%nc),F_Tn(apparent%nc),&
         F_Vn(apparent%nc),F_nn(apparent%nc,apparent%nc)
    ! Locals
    integer :: i, j, k, l
    if (present(F_n)) then
      call apparent%real_to_apparent_diff(Fe_n,F_n)
    endif
    if (present(F_Tn)) then
      call apparent%real_to_apparent_diff(Fe_Tn,F_Tn)
    endif
    if (present(F_Vn)) then
      call apparent%real_to_apparent_diff(Fe_Vn,F_Vn)
    endif
    if (present(F_nn)) then
      if (apparent%nc == apparent%nce) then
        F_nn = Fe_nn
      else
        F_nn = 0.0
        F_nn(1:apparent%ncsym,1:apparent%ncsym) = Fe_nn(1:apparent%ncsym,1:apparent%ncsym)
        ! Solvent-salt differentials
        do k=1,apparent%ncsym ! Solvent
          do i=apparent%ncsym+1,apparent%nc ! Salt
            do j=apparent%ncsym+1,apparent%nce ! Ion
              F_nn(k,i) = F_nn(k,i) + Fe_nn(k,j)*apparent%v_stoich(i,j)
              F_nn(i,k) = F_nn(i,k) + Fe_nn(j,k)*apparent%v_stoich(i,j)
            enddo
          enddo
        enddo
        ! Salt-salt differentials
        do k=apparent%ncsym+1,apparent%nc ! Salt1
          do i=apparent%ncsym+1,apparent%nc ! Salt2
            do j=apparent%ncsym+1,apparent%nce ! Ions of salt 1
              do l=apparent%ncsym+1,apparent%nce ! Ions of salt 2
                F_nn(k,i) = F_nn(k,i) + Fe_nn(l,j)*apparent%v_stoich(k,j)*apparent%v_stoich(i,l)
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
  subroutine real_to_apparent_diff(apparent,Fe_n,F_n)
    ! Passes object:
    class(apparent_container), intent(in) :: apparent
    real, intent(in) :: Fe_n(apparent%nce)
    real, intent(out) :: F_n(apparent%nc)
    ! Locals
    integer :: i, j
    if (apparent%nc == apparent%nce) then
      F_n = Fe_n
    else
      F_n(1:apparent%ncsym) = Fe_n(1:apparent%ncsym)
      F_n(apparent%ncsym+1:apparent%nc) = 0.0
      do i=apparent%ncsym+1,apparent%nc ! Salt
        do j=apparent%ncsym+1,apparent%nce ! Ion
          F_n(i) = F_n(i) + Fe_n(j)*apparent%v_stoich(i,j)
        enddo
      enddo
    endif
  end subroutine real_to_apparent_diff

  !-----------------------------------------------------------------------------
  !> Convert logarithmic fugacity coefficient from real to apparent composition
  !!
  !! \author Morten Hammer, 2017-03
  !-----------------------------------------------------------------------------
  subroutine TP_lnfug_apparent(apparent,nc,ne,n,P,lnfug_real,lnfug,dlnfugdt_real,&
       dlnfugdp_real,dlnfugdn_real,dlnfugdT,dlnfugdP,dlnfugdn)
    ! Passes object:
    class(apparent_container), intent(in) :: apparent
    ! Input.
    integer, intent(in) :: nc
    real, intent(in) :: n(nc)                           !< Apparent mole numbers [mols]
    real, intent(in) :: P                               !< Pressure [Pa]
    real, intent(in) :: ne(apparent%nce)                         !< Real mole numbers [mols]
    real, intent(in) :: lnfug_real(apparent%nce)                 !< Log of real fugacities
    real, optional, intent(in) :: dlnfugdt_real(apparent%nce), dlnfugdp_real(apparent%nce)
    real, optional, intent(in) :: dlnfugdn_real(apparent%nce,apparent%nce)
    ! Output
    real, intent(out) :: lnfug(nc)                      !< Log of apparent fugacity
    real, optional, intent(out) :: dlnfugdt(nc), dlnfugdp(nc), dlnfugdn(nc,nc)
    ! Locals.
    real :: sumn, sumne  !< Total mole number in mixture [mol]
    real :: dik
    real :: xe(apparent%nce), x(nc), logP
    integer :: i,j,k,l
    if (nc == apparent%nce) then
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

      lnfug(1:apparent%ncsym) = lnfug_real(1:apparent%ncsym)  + log(sumn/sumne)
      do i=apparent%ncsym+1,nc
        if (x(i) > 0.0) then
          lnfug(i) = 0.0
          do j=apparent%ncsym+1,apparent%nce
            lnfug(i) = lnfug(i) + apparent%v_stoich(i,j)*(log(xe(j)) + lnfug_real(j))
          enddo
          lnfug(i) = lnfug(i) - log(x(i))
        else
          lnfug(i) = 0.0
        endif
      enddo

      if (present(dlnfugdt)) then
        call apparent%real_to_apparent_diff(dlnfugdt_real,dlnfugdt)
      endif
      if (present(dlnfugdp)) then
        call apparent%real_to_apparent_diff(dlnfugdp_real,dlnfugdp)
      endif
      if (present(dlnfugdn)) then
        dlnfugdn = 0.0
        ! Upper left matrix (apparent%ncsym)x(apparent%ncsym)
        do i=1,apparent%ncsym
          do j=1,apparent%ncsym
            dlnfugdn(i,j) = dlnfugdn_real(i,j) - 1.0/sumne + 1.0/sumn
          end do
        end do
        ! Lower right matrix (nc-apparent%ncsym)x(nc-apparent%ncsym)
        do k=apparent%ncsym+1,nc ! App k
          do i=apparent%ncsym+1,nc ! App i
            do j=apparent%ncsym+1,apparent%nce ! Real j
              do l=apparent%ncsym+1,apparent%nce ! Real l
                dlnfugdn(k,i) = dlnfugdn(k,i) + dlnfugdn_real(l,j)*apparent%v_stoich(k,j)*apparent%v_stoich(i,l)
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
            dlnfugdn(k,i) = dlnfugdn(k,i) + 1.0/sumn - dik - apparent%v_sum(k)*apparent%v_sum(i)/sumne
            do j=apparent%ncsym+1,apparent%nce ! Real j
              if (ne(j) > 0.0) then
                dlnfugdn(k,i) = dlnfugdn(k,i) + apparent%v_stoich(k,j)*apparent%v_stoich(i,j)/max(ne(j),min_mol_num)
              endif
            enddo
          enddo
        enddo

        ! Lower and left and upper right (apparent%ncsym)x(nc-apparent%ncsym) and (nc-apparent%ncsym)x(apparent%ncsym)
        do k=1,apparent%ncsym ! App k
          do i=apparent%ncsym+1,nc ! App i
            do j=apparent%ncsym+1,apparent%nce ! Real j
              dlnfugdn(k,i) = dlnfugdn(k,i) + dlnfugdn_real(k,j)*apparent%v_stoich(i,j)
              dlnfugdn(i,k) = dlnfugdn(i,k) + dlnfugdn_real(j,k)*apparent%v_stoich(i,j)
            enddo
            dlnfugdn(k,i) = dlnfugdn(k,i) + 1.0/sumn - apparent%v_sum(i)/sumne
            dlnfugdn(i,k) = dlnfugdn(i,k) + 1.0/sumn - apparent%v_sum(i)/sumne
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
  subroutine getModFugacity(apparent,t,p,X,lnfug,sumne)
    implicit none
    ! Passes object:
    class(apparent_container), intent(in) :: apparent
    real, intent(in) :: t !< Temperature (K)
    real, intent(in) :: p !< Pressure (Pa)
    real, dimension(apparent%nc), intent(in) :: X !< Phase compozition
    real, intent(inout) :: lnfug(apparent%nc)
    real, intent(out) :: sumne
    ! Locals
    integer :: i, k
    real :: Xe(apparent%nce)
    call apparent%apparent_to_real_mole_numbers(X,Xe)
    sumne = sum(Xe)
    Xe = Xe/sumne
    do i=apparent%ncsym+1,apparent%nc
      lnfug(i) = lnfug(i) + log(X(i))
      do k=apparent%ncsym+1,apparent%nce
        if (apparent%v_stoich(i,k) > 0) then
          lnfug(i) = lnfug(i) - apparent%v_stoich(i,k)*log(xe(k))
          lnfug(i) = lnfug(i) + apparent%v_stoich(i,k)*log(apparent%v_stoich(i,k))
        endif
      enddo
    enddo
  end subroutine getModFugacity

end module apparent_compostion
