module unifac
  use unifacdata
  implicit none
  private
  save

  integer :: ng !< Number of active sub-groups in unifac

  !> Structure for active unifac parameters
  type :: unifacdb
    logical :: FloryHuggins
    logical :: StavermanGuggenheim
    integer, allocatable, dimension(:) :: mainGroupMapping
    integer, allocatable, dimension(:,:) :: vik !< (nc,ng) [-] Number of groups in one component
    real, allocatable, dimension(:,:) :: ajk !< (ng,ng) [K] Group interaction energy
    real, allocatable, dimension(:,:) :: bjk !< (ng,ng) [-] Group interaction energy
    real, allocatable, dimension(:,:) :: cjk !< (ng,ng) [1/K] Group interaction energy
    real, allocatable, dimension(:) :: Qk !< (ng) [] Molecyle group surface area
    real, allocatable, dimension(:) :: qi !< [-] (nc) Combinatorial term param
    real, allocatable, dimension(:) :: ri !< [-] (nc) Combinatorial term param
  contains
    procedure :: dealloc => unifacdb_dealloc
    procedure :: assign_unifacdb
    generic, public :: assignment(=) => assign_unifacdb
  end type unifacdb

  type(unifacdb) :: unifdb !< Active unifac parameters

  public :: init_unifac, cleanup_unifac
  public :: GeFloryHuggins, GeStavermanGuggenheim
  public :: GeUNIFAC, Ge_UNIFAC_GH_SG
  public :: unifacdb, unifdb
  public :: setUNIFACgroupInteraction, getUNIFACgroupInteraction

contains

  !> Initiate unifac model
  !!
  subroutine init_unifac(UFdb,mrulestr)
    use thermopack_var, only: complist, nc
    use stringmod, only: str_eq
    !
    type (unifacdb), intent(out) :: UFdb
    character(len=*), intent(in) :: mrulestr
    ! Locals
    integer, dimension(nSubGroups) :: activeGroups
    type (unifacComp), dimension(nc) :: activeUnifacComp
    real, dimension(nSubGroups) :: Rk
    integer :: i,j,k,err,idx

    if ( str_eq(mrulestr,"UMR") ) then
!       print *,"USING UMR MIXING RULE"
       UFdb%FloryHuggins = .false.
       UFdb%StavermanGuggenheim = .true.
    else if ( str_eq(mrulestr,"VTPR") ) then
!       print *,"USING VTPR MIXING RULE"
       UFdb%FloryHuggins = .false.
       UFdb%StavermanGuggenheim = .false.
    else if ( str_eq(mrulestr,"UNIFAC") ) then
       UFdb%FloryHuggins = .true.
       UFdb%StavermanGuggenheim = .true.
    else
       call stoperror("Invalid unifac mixing rule.")
    end if

    ! Find componets in database
    do i=1,nc
      activeUnifacComp(i) = unifacCompdb(getUNIFACcompdbidx(complist(i)))
    enddo
    activeGroups = 0
    do i=1,nc
      do j=1,nSubGroups
        activeGroups(j) = activeGroups(j) + activeUnifacComp(i)%v(j)
      enddo
    enddo
    ng = 0
    do j=1,nSubGroups
      if (activeGroups(j) > 0) then
        ng = ng + 1
        activeGroups(ng) = j ! Mapping for active sub-groups
      endif
    enddo

    ! Deallocat previously allocated memory
    call cleanup_unifac(UFdb)
    ! Allocate new memory
    allocate(UFdb%vik(nc,ng),UFdb%ajk(ng,ng),&
         UFdb%bjk(ng,ng),UFdb%cjk(ng,ng),&
         UFdb%Qk(ng),UFdb%qi(nc),&
         UFdb%ri(nc),UFdb%mainGroupMapping(ng),STAT=err)
    if (err /= 0) Call StopError('Could not allocate unifac memory!')

    ! Set up Qk, and main group mapping
    do j=1,ng
      UFdb%Qk(j) = unifacprmdb(activeGroups(j))%Qk
      Rk(j) = unifacprmdb(activeGroups(j))%Rk
      UFdb%mainGroupMapping(j) = unifacprmdb(activeGroups(j))%mainGrp
    enddo
    ! Set up vik, qi and ri
    do i=1,nc
      do j=1,ng
        UFdb%vik(i,j) = activeUnifacComp(i)%v(activeGroups(j))
      enddo
      UFdb%qi(i) = sum(UFdb%vik(i,:)*UFdb%Qk)
      UFdb%ri(i) = sum(UFdb%vik(i,:)*Rk(1:ng))
    enddo

    ! Set up ajk, bjk and cjk
    do j=1,ng
      do k=1,ng
        idx = getUNIFACujkdbidx(UFdb%mainGroupMapping(j),UFdb%mainGroupMapping(k))
        if (idx > 0) then
          UFdb%ajk(j,k) = unifacUijdb(idx)%aij
          UFdb%bjk(j,k) = unifacUijdb(idx)%bij
          UFdb%cjk(j,k) = unifacUijdb(idx)%cij
        else
          print *,'UNIFAC interaction energies not found for main groups: ',&
               UFdb%mainGroupMapping(j),UFdb%mainGroupMapping(k)
          UFdb%ajk(j,k) = 0.0
          UFdb%bjk(j,k) = 0.0
          UFdb%cjk(j,k) = 0.0
        endif
      enddo
    enddo

  end subroutine init_unifac

  !> Set interaction parameters for tunig
  subroutine setUNIFACgroupInteraction(UFdb,i,j,aij,bij,cij)
    !
    type (unifacdb), intent(out) :: UFdb
    integer, intent(in) :: i,j ! Main group indices
    real, intent(in) :: aij,bij,cij
    !
    integer :: ii, jj
    ! Set aij, bij and cij
    do ii=1,ng
      do jj=1,ng
        if (UFdb%mainGroupMapping(ii) == i .and. UFdb%mainGroupMapping(jj) == j) then
          UFdb%ajk(ii,jj) = aij
          UFdb%bjk(ii,jj) = bij
          UFdb%cjk(ii,jj) = cij
        endif
      enddo
    enddo
  end subroutine setUNIFACgroupInteraction

  !> Set interaction parameters for tunig
  subroutine getUNIFACgroupInteraction(UFdb,i,j,aij,bij,cij)
    !
    type (unifacdb), intent(in) :: UFdb
    integer, intent(in) :: i,j ! Main group indices
    real, intent(out) :: aij,bij,cij
    !
    integer :: ii, jj
    ! Set aij, bij and cij
    do ii=1,ng
      do jj=1,ng
        if (UFdb%mainGroupMapping(ii) == i .and. UFdb%mainGroupMapping(jj) == j) then
          aij = UFdb%ajk(ii,jj)
          bij = UFdb%bjk(ii,jj)
          cij = UFdb%cjk(ii,jj)
          return
        endif
      enddo
    enddo
  end subroutine getUNIFACgroupInteraction

  !> Initiate unifac model
  !!
  subroutine cleanup_unifac(UFdb)
    !
    type (unifacdb), intent(inout) :: UFdb
    integer :: err
    ! De-allocate memory
    err = 0
    if (allocated (UFdb%vik)) deallocate (UFdb%vik, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%vik!')
    if (allocated (UFdb%ajk)) deallocate (UFdb%ajk, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%ajk!')
    if (allocated (UFdb%bjk)) deallocate (UFdb%bjk, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%bjk!')
    if (allocated (UFdb%cjk)) deallocate (UFdb%cjk, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%cjk!')
    if (allocated (UFdb%Qk)) deallocate (UFdb%Qk, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%Qk!')
    if (allocated (UFdb%qi)) deallocate (UFdb%qi, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%qi!')
    if (allocated (UFdb%ri)) deallocate (UFdb%ri, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%ri!')
    if (allocated (UFdb%mainGroupMapping)) deallocate (UFdb%mainGroupMapping, STAT=err)
    if (err /= 0) Call StopError('Could not deallocate UFdb%mainGroupMapping!')
  end subroutine cleanup_unifac

  !> Get index of entry containg component information in the unifac db
  !! \author MH, 2015
  function getUNIFACcompdbidx(compid) result(idx)
    integer :: idx
    character(len=*), intent(in) :: compid
    logical :: found
    idx = 1
    found = .false.
    do while (idx <= nUnifacComp .and. .not. found)
      if (trim(compid) /= trim(unifacCompdb(idx)%uid)) then
        idx = idx + 1
      else
        found = .true.
      endif
    enddo
    if (.not. found) then
      idx = 0
      call stoperror('UNIFAC component not found: '//trim(compid))
    endif
  end function getUNIFACcompdbidx

  !> Get index of entry containg energies of mixing in the unifac db
  !! \author MH, 2015
  function getUNIFACujkdbidx(id1,id2) result(idx)
    integer, intent(in) :: id1,id2
    integer :: idx
    !
    logical :: found
    idx = 1
    found = .false.
    do while (idx <= nUnifacUij .and. .not. found)
      if (id1 == unifacUijdb(idx)%mgi .and. id2 == unifacUijdb(idx)%mgj) then
        found = .true.
      else
        idx = idx + 1
      endif
    enddo
    if (.not. found) then
      idx = 0
    endif
  end function getUNIFACujkdbidx

  !> Calculate Flory Huggins combinatorial term
  !! \author MH, 2015
  subroutine GeFloryHuggins(n,UFdb,Ge,dGedn,d2Gedn2)
    use thermopack_var, only: nc
    !
    real, dimension(nc), intent(in) :: n
    type (unifacdb), intent(in) :: UFdb
    real, intent(out) :: Ge
    real, optional, dimension(nc), intent(out) :: dGedn
    real, optional, dimension(nc,nc), intent(out) :: d2Gedn2
    ! Locals
    integer :: i,j
    real, dimension(nc) :: lnr
    real :: sumn, sumnr, sumlnr, lnsumnr, lnsumn
    !
    sumn = sum(n)
    sumnr = sum(n*UFdb%ri)
    lnr = log(UFdb%ri)
    sumlnr = sum(n*lnr)
    lnsumnr = log(sumnr)
    lnsumn = log(sumn)
    Ge = sumlnr - sumn*lnsumnr + sumn*lnsumn

    if (present(dGedn)) then
      do i=1,nc
        dGedn(i) = lnr(i) - lnsumnr + lnsumn + 1.0 - sumn*UFdb%ri(i)/sumnr
      enddo
    endif
    if (present(d2Gedn2)) then
      do i=1,nc
        do j=i,nc
          d2Gedn2(i,j) = - (UFdb%ri(i) + UFdb%ri(j))/sumnr + 1.0/sumn &
               + sumn*UFdb%ri(i)*UFdb%ri(j)/sumnr**2
          d2Gedn2(j,i) = d2Gedn2(i,j)
        enddo
      enddo
    endif

  end subroutine GeFloryHuggins

  !> Calculate Staverman-Guggenheim combinatorial term
  !! \author MH, 2015
  subroutine GeStavermanGuggenheim(n,UFdb,Ge,dGedn,d2Gedn2)
    use thermopack_var, only: nc
    !
    real, dimension(nc), intent(in) :: n
    type (unifacdb), intent(in) :: UFdb
    real, intent(out) :: Ge
    real, optional, dimension(nc), intent(out) :: dGedn
    real, optional, dimension(nc,nc), intent(out) :: d2Gedn2
    ! Locals
    integer :: i,j
    real, dimension(nc) :: lnqor !, phi_i, theta_i
    real :: sumn, sumnr, lnsumnr
    real :: sumnq, lnsumnq
    real, parameter :: zdiv2 = 5.0
    !
    sumn = sum(n)
    sumnr = sum(n*UFdb%ri)
    sumnq = sum(n*UFdb%qi)
    lnqor = log(UFdb%qi/UFdb%ri)
    lnsumnr = log(sumnr)
    lnsumnq = log(sumnq)
    Ge = zdiv2*(sum(n*UFdb%qi*lnqor) - sumnq*lnsumnq + sumnq*lnsumnr)
    !Ge = zdiv2*(sum(n*UFdb%qi*log(theta_i/phi_i)))
    if (present(dGedn)) then
      do i=1,nc
        dGedn(i) = zdiv2*UFdb%qi(i)*(lnqor(i) - lnsumnq + lnsumnr - 1.0 &
             + UFdb%ri(i)*sumnq/(UFdb%qi(i)*sumnr))
        !theta_i = n*UFdb%qi/sumnq
        !phi_i = n*UFdb%ri/sumnr
        !dGedn(i) = zdiv2*UFdb%qi(i)*(log(theta_i(i)/phi_i(i))-1+phi_i(i)/theta_i(i))
      enddo
    endif
    if (present(d2Gedn2)) then
      do i=1,nc
        do j=i,nc
          d2Gedn2(i,j) = zdiv2*(-UFdb%qi(i)*UFdb%qi(j)/sumnq &
               + (UFdb%qi(i)*UFdb%ri(j) + UFdb%qi(j)*UFdb%ri(i))/sumnr &
               - UFdb%ri(i)*UFdb%ri(j)*sumnq/sumnr**2)
          d2Gedn2(j,i) = d2Gedn2(i,j)
        enddo
      enddo
    endif

  end subroutine GeStavermanGuggenheim

  !> Calculate UNIFAC Ge (Ae)
  !! \author MH, 2015
  subroutine GeUNIFAC(n,T,UFdb,Ge,dGedT,d2GedT2,dGedn,d2GedndT,d2Gedn2)
    use thermopack_var, only: nc, kRgas
    !
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T
    type (unifacdb), intent(in) :: UFdb
    real, intent(out) :: Ge
    real, optional, intent(out) :: dGedT,d2GedT2
    real, optional, dimension(nc), intent(out) :: dGedn,d2GedndT
    real, optional, dimension(nc,nc), intent(out) :: d2Gedn2
    ! Locals
    integer :: i,j, k
    real, dimension(ng) :: theta_j, lambda_k, sum_nlvlj, sum_thetaj_Ejk
    real, dimension(ng,ng) :: Ejk, dEjkdT, d2EjkdT2
    real, dimension(nc,ng) :: theta_ij, lambda_ik, dlambda_k_dn, sum_vijQjEjk
    real, dimension(nc,ng) :: d2lambda_k_dndT
    real, dimension(nc,nc,ng) :: d2lambda_k_dn2
    real, dimension(nc) :: sum_vikQk, sum_vQLambda, sum_vijQjdLdTdiff
    real, dimension(nc,nc) :: sum_Q_v_dlambda_k_dn
    real, dimension(ng) :: sum_nivijQjEjk, dlambda_k_dT, d2lambda_k_dT2
    real, dimension(ng) :: sum_nivijQjdEjkdT
    real, dimension(nc,ng) :: sum_vijQjdEjkdT, dlambda_ik_dT, d2lambda_ik_dT2
    real, dimension(nc,ng) :: sum_thetaij_Ejk, sum_vijQjd2EjkdT2

    real :: sum_nivikQk, temp
    real, dimension(nc) :: tempsum

    ! Calculate mixture energies
    do j=1,ng
      do k=1,ng
        Ejk(j,k) = exp(-(UFdb%ajk(j,k)/T + UFdb%bjk(j,k) + UFdb%cjk(j,k)*T))
      enddo
    enddo
    ! Do we need temperature differentials?
    if (present(dGedT) .or. present(d2GedT2) .or. present(d2GedndT)) then
      do j=1,ng
        do k=1,ng
          dEjkdT(j,k) = Ejk(j,k)*(UFdb%ajk(j,k)/T**2 - UFdb%cjk(j,k))
          if (present(d2GedT2)) then
            d2EjkdT2(j,k) = dEjkdT(j,k)*(UFdb%ajk(j,k)/T**2 - UFdb%cjk(j,k)) &
                 - Ejk(j,k)*2.0*UFdb%ajk(j,k)/T**3
          endif
        enddo
      enddo
    endif
    ! Calculate some useful sums
    do j=1,nc
      sum_vikQk(j) = sum(UFdb%vik(j,:)*UFdb%Qk(:))
    enddo
    sum_nivikQk = sum(n*sum_vikQk)
    ! Calculate theta_ij
    do i=1,nc
      do j=1,ng
        theta_ij(i,j) = UFdb%vik(i,j)*UFdb%Qk(j)/sum_vikQk(i)
      enddo
    enddo

    ! Calculate theta_j
    do j=1,ng
      sum_nlvlj(j) = sum(n*UFdb%vik(:,j))
      theta_j(j) = sum_nlvlj(j)*UFdb%Qk(j)/sum_nivikQk
    enddo
    ! Lambda_k
    do k=1,ng
      sum_thetaj_Ejk(k) = sum(theta_j*Ejk(:,k))
    enddo
    lambda_k = log(sum_thetaj_Ejk)
    ! Lambda_ik
    do i=1,nc
      do k=1,ng
        sum_thetaij_Ejk(i,k) = sum(theta_ij(i,:)*Ejk(:,k))
      enddo
    enddo
    lambda_ik = log(sum_thetaij_Ejk)

    ! Calculate Ge
    do i=1,nc
      sum_vQLambda(i) = sum(UFdb%vik(i,:)*UFdb%Qk*(lambda_k - lambda_ik(i,:)))
    enddo
    Ge = - sum(n*sum_vQLambda)

    ! Calculate differential of lambda_k wrpt. ni
    if (present(dGedn) .or. present(d2GedndT) .or. present(d2Gedn2)) then
      do i=1,nc
        do k=1,ng
          sum_vijQjEjk(i,k) = sum(UFdb%vik(i,:)*UFdb%Qk*Ejk(:,k))
        enddo
      enddo
      do k=1,ng
        sum_nivijQjEjk(k) = sum(n*sum_vijQjEjk(:,k))
      enddo
      do i=1,nc
        dlambda_k_dn(i,:) = sum_vijQjEjk(i,:)/sum_nivijQjEjk - sum_vikQk(i)/sum_nivikQk
      enddo
    endif

    ! dGedn
    if (present(dGedn)) then
      do i=1,nc
        tempsum(i) = sum(sum_nlvlj*UFdb%Qk*dlambda_k_dn(i,:))
      enddo
      dGedn = -sum_vQLambda - tempsum
    endif

    ! d2Gedn2
    if (present(d2Gedn2)) then
      do i=1,nc
        do j=1,nc
          sum_Q_v_dlambda_k_dn(i,j) = sum(UFdb%Qk*UFdb%vik(j,:)*dlambda_k_dn(i,:))
          d2lambda_k_dn2(i,j,:) = - sum_vijQjEjk(i,:)*sum_vijQjEjk(j,:)/sum_nivijQjEjk**2 + sum_vikQk(i)*sum_vikQk(j)/sum_nivikQk**2
        enddo
      enddo
      do i=1,nc
        do j=1,nc
          temp = sum(sum_nlvlj*d2lambda_k_dn2(i,j,:)*UFdb%Qk)
          d2Gedn2(i,j) = -(sum_Q_v_dlambda_k_dn(i,j) + sum_Q_v_dlambda_k_dn(j,i)) - temp
        enddo
      enddo
    endif

    if (present(d2GedndT) .or. present(d2GedT2) .or. present(dGedT)) then
      do i=1,nc
        do k=1,ng
          sum_vijQjdEjkdT(i,k) = sum(UFdb%vik(i,:)*UFdb%Qk*dEjkdT(:,k))
          sum_vijQjd2EjkdT2(i,k) = sum(UFdb%vik(i,:)*UFdb%Qk*d2EjkdT2(:,k))
        enddo
      enddo

      ! Does this work?
      dlambda_ik_dT = sum_vijQjdEjkdT/sum_vijQjEjk
      d2lambda_ik_dT2 = sum_vijQjd2EjkdT2/sum_vijQjEjk - dlambda_ik_dT*dlambda_ik_dT

      do k=1,ng
        sum_nivijQjdEjkdT(k) = sum(n*sum_vijQjdEjkdT(:,k))
        dlambda_k_dT(k) = sum_nivijQjdEjkdT(k)/sum_nivijQjEjk(k)
        d2lambda_k_dT2(k) = sum(n*sum_vijQjd2EjkdT2(:,k))/sum_nivijQjEjk(k) - dlambda_k_dT(k)**2
      enddo

      do i=1,nc
        sum_vijQjdLdTdiff(i) = sum(UFdb%vik(i,:)*UFdb%Qk*(dlambda_k_dT - dlambda_ik_dT(i,:)))
      enddo
    endif

    ! Calculate differential of Ge/RT wrpt. ni and T
    if (present(d2GedndT)) then
      do i=1,nc
        d2lambda_k_dndT(i,:) = sum_vijQjdEjkdT(i,:)/sum_nivijQjEjk - sum_vijQjEjk(i,:)*sum_nivijQjdEjkdT/sum_nivijQjEjk**2
      enddo
      do i=1,nc
        tempsum(i) = sum(sum_nlvlj*UFdb%Qk*d2lambda_k_dndT(i,:))
      enddo
      d2GedndT = -sum_vijQjdLdTdiff - tempsum
    endif

    ! Calculate differential of Ge/RT wrpt. T
    if (present(dGedT)) then
      dGedT = -sum(n*sum_vijQjdLdTdiff)
    endif

    ! Calculate second differential of Ge/RT wrpt. T
    if (present(d2GedT2)) then
      d2GedT2 = 0.0
      do i=1,nc
        d2GedT2 = d2GedT2 - n(i)*sum(UFdb%vik(i,:)*UFdb%Qk*(d2lambda_k_dT2 - d2lambda_ik_dT2(i,:)))
      enddo
    endif

    ! Final scaling of results
    if (present(d2GedT2)) then
      d2GedT2 = kRgas*(2.0*dGedT + T*d2GedT2)
    endif
    if (present(dGedT)) then
      dGedT = kRgas*(Ge + dGedT*T)
    endif
    if (present(d2GedndT)) then
      d2GedndT = kRgas*(dGedn + d2GedndT*T)
    endif
    Ge = Ge*kRgas*T
    if (present(dGedn)) then
      dGedn = dGedn*kRgas*T
    endif
    if (present(d2Gedn2)) then
      d2Gedn2 = d2Gedn2*kRgas*T
    endif

  end subroutine GeUNIFAC

  !> Calculate excess Gibbs combined from UNIFAC, Staverman-Guggenheim and
  !! Flory-Huggins
  !! \author MH, 2015
  subroutine Ge_UNIFAC_GH_SG(n,T,UFdb,Ge,dGedT,d2GedT2,dGedn,d2GedndT,d2Gedn2)
    use thermopack_var, only: nc, kRgas
    !
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T
    type (unifacdb), intent(in) :: UFdb
    real, intent(out) :: Ge ! [kJ]
    real, intent(out) :: dGedT,d2GedT2
    real, dimension(nc), intent(out) :: dGedn,d2GedndT
    real, dimension(nc,nc), intent(out) :: d2Gedn2
    ! Locals
    real :: Ge_FH, Ge_SG
    real, dimension(nc) :: dGedn_FH, dGedn_SG
    real, dimension(nc,nc) :: d2Gedn2_FH, d2Gedn2_SG

    call GeUNIFAC(n,T,UFdb,Ge,dGedT,d2GedT2,dGedn,d2GedndT,d2Gedn2)
    ! Ge = 0.0
    ! dGedT = 0.0
    ! d2GedT2 = 0.0
    ! dGedn = 0.0
    ! d2GedndT = 0.0
    ! d2Gedn2 = 0.0
    if (UFdb%FloryHuggins) then
      !print *,'FloryHuggins'
      call GeFloryHuggins(n,UFdb,Ge_FH,dGedn_FH,d2Gedn2_FH)
      Ge = Ge + Ge_FH*kRgas*T
      dGedn = dGedn + dGedn_FH*kRgas*T
      d2Gedn2 = d2Gedn2 + d2Gedn2_FH*kRgas*T
      dGedT = dGedT + Ge_FH*kRgas
      d2GedndT = d2GedndT + dGedn_FH*kRgas
    endif
    if (UFdb%StavermanGuggenheim) then
      !print *,'StavermanGuggenheim'
      call GeStavermanGuggenheim(n,UFdb,Ge_SG,dGedn_SG,d2Gedn2_SG)
      Ge = Ge + Ge_SG*kRgas*T
      dGedn = dGedn + dGedn_SG*kRgas*T
      d2Gedn2 = d2Gedn2 + d2Gedn2_SG*kRgas*T
      dGedT = dGedT + Ge_SG*kRgas
      d2GedndT = d2GedndT + dGedn_SG*kRgas
    endif

  end subroutine Ge_UNIFAC_GH_SG

  subroutine assign_unifacdb(u1,u2)
    class(unifacdb), intent(inout) :: u1
    class(unifacdb), intent(in) :: u2
    ! Locals
    integer :: ierr
    u1%FloryHuggins = u2%FloryHuggins
    u1%StavermanGuggenheim = u2%StavermanGuggenheim
    if (allocated(u2%mainGroupMapping)) then
      if (allocated(u1%mainGroupMapping)) then
        deallocate(u1%mainGroupMapping, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%mainGroupMapping")
      endif
      allocate(u1%mainGroupMapping(size(u2%mainGroupMapping,dim=1)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%mainGroupMapping")
      u1%mainGroupMapping = u2%mainGroupMapping
    endif
    if (allocated(u2%vik)) then
      if (allocated(u1%vik)) then
        deallocate(u1%vik, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%vik")
      endif
      allocate(u1%vik(size(u2%vik,dim=1),size(u2%vik,dim=2)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%vik")
      u1%vik = u2%vik
    endif
    if (allocated(u2%ajk)) then
      if (allocated(u1%ajk)) then
        deallocate(u1%ajk, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%ajk")
      endif
      allocate(u1%ajk(size(u2%ajk,dim=1),size(u2%ajk,dim=2)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%ajk")
      u1%ajk = u2%ajk
    endif
    if (allocated(u2%bjk)) then
      if (allocated(u1%bjk)) then
        deallocate(u1%bjk, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%bjk")
      endif
      allocate(u1%bjk(size(u2%bjk,dim=1),size(u2%bjk,dim=2)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%bjk")
      u1%bjk = u2%bjk
    endif
    if (allocated(u2%cjk)) then
      if (allocated(u1%cjk)) then
        deallocate(u1%cjk, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%cjk")
      endif
      allocate(u1%cjk(size(u2%cjk,dim=1),size(u2%cjk,dim=2)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%cjk")
      u1%cjk = u2%cjk
    endif
    if (allocated(u2%Qk)) then
      if (allocated(u1%Qk)) then
        deallocate(u1%Qk, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%Qk")
      endif
      allocate(u1%Qk(size(u2%Qk,dim=1)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%Qk")
      u1%Qk = u2%Qk
    endif
    if (allocated(u2%qi)) then
      if (allocated(u1%qi)) then
        deallocate(u1%qi, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%qi")
      endif
      allocate(u1%qi(size(u2%qi,dim=1)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%qi")
      u1%qi = u2%qi
    endif
    if (allocated(u2%ri)) then
      if (allocated(u1%ri)) then
        deallocate(u1%ri, stat=ierr)
        if (ierr /= 0) call stoperror("Not able to deallocate u1%ri")
      endif
      allocate(u1%ri(size(u2%ri,dim=1)), stat=ierr)
      if (ierr /= 0) call stoperror("Not able to allocate u1%ri")
      u1%ri = u2%ri
    endif
  end subroutine assign_unifacdb

  subroutine unifacdb_dealloc(u)
    use utilities, only: deallocate_real, deallocate_real_2
    class(unifacdb), intent(inout) :: u
    ! Locals
    integer :: ierr
    ierr = 0
    if (allocated(u%mainGroupMapping)) deallocate(u%mainGroupMapping, stat=ierr)
    if (ierr /= 0) call stoperror("unifacdb_dealloc: Not able to deallocate u%mainGroupMapping")
    if (allocated(u%vik)) deallocate(u%vik, stat=ierr)
    if (ierr /= 0) call stoperror("unifacdb_dealloc: Not able to deallocate u%vik")

    call deallocate_real_2(u%ajk,"u%ajk")
    call deallocate_real_2(u%bjk,"u%bjk")
    call deallocate_real_2(u%cjk,"u%cjk")
    call deallocate_real(u%Qk,"u%Qk")
    call deallocate_real(u%qi,"u%qi")
    call deallocate_real(u%ri,"u%ri")

  end subroutine unifacdb_dealloc

end module unifac
