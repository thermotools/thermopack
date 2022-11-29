module fundamental_measure_theory
  use hyperdual_mod
  use numconstants, only: pi, small
  implicit none

  integer, parameter :: FMT_ROSENFELD=1, FMT_WB=2, FMT_WBII=3

  public :: fmt_energy_density
  ! Testing only:
  public :: test_fmt

contains

  !> Calculate reduced energy density from FMT
  !! \author Morten Hammer, 2022-11
  subroutine fmt_energy_density(fmt_model,n_grid,nv,nw,phi,phi_n,phi_nn)
    use hyperdual_mod
    implicit none
    !procedure(hyperdual_fmt) :: fun
    integer, intent(in) :: n_grid,nv,fmt_model
    real, intent(in) :: nw(n_grid,nv)
    real, optional, intent(out) :: phi(n_grid),phi_n(n_grid,nv),phi_nn(n_grid,nv,nv)
    ! Locals
    type(hyperdual) :: nw_hd(nv), phi_hd
    integer :: i, j, k

    ! Loop grid
    do k=1,n_grid
      nw_hd(:)%f0 = nw(k,:)
      if (present(phi_nn)) then
        do i=1,nv
          nw_hd(i)%f1 = 1
          do j=i,nv
            nw_hd(j)%f2 = 1
            phi_hd = fmt(fmt_model,nv,nw_hd)
            phi_nn(k,i,j) = phi_hd%f12
            phi_nn(k,j,i) = phi_nn(k,i,j)
            nw_hd(j)%f2 = 0
          enddo
          nw_hd(i)%f1 = 0
          if (present(phi_n)) then
            phi_n(k,i) = phi_hd%f1
          endif
        enddo
        if (present(phi)) phi(k) = phi_hd%f0
      else if (present(phi_n)) then
        do i=1,nv
          nw_hd(i)%f1 = 1
          phi_hd = fmt(fmt_model,nv,nw_hd)
          nw_hd(i)%f1 = 0
          phi_n(k,i) = phi_hd%f1
        enddo
        if (present(phi)) phi(k) = phi_hd%f0
      else if (present(phi)) then
        phi_hd = fmt(fmt_model,nv,nw_hd)
        phi(k) = phi_hd%f0
      endif
    enddo

  end subroutine fmt_energy_density

  function fmt(fmt_model,nv,nw) result(phi)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: fmt_model
    integer, intent(in) :: nv
    type(hyperdual), intent(in) :: nw(nv)
    type(hyperdual) :: phi
    select case(fmt_model)
    case(FMT_ROSENFELD)
      phi = rosenfeld_fmt(nv,nw)
    case(FMT_WB)
      phi = wb_fmt(nv,nw)
    case(FMT_WBII)
      phi = wbII_fmt(nv,nw)
    case default
      call stoperror("Wrong FMT model")
    end select
  end function fmt

  !> Rosenfeld FMT functional
  !! Rosenfeld, Yaakov
  !! Free-energy model for the inhomogeneous hard-sphere fluid mixture andl
  !! density-functional theory of freezing.
  !! Phys. Rev. Lett. 1989, 63(9):980-983
  !! doi:10.1103/PhysRevLett.63.980
  function rosenfeld_fmt(nv,nw) result(phi)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nv
    type(hyperdual), intent(in) :: nw(0:nv-1)
    type(hyperdual) :: phi
    ! Locals
    type(hyperdual) :: n3neg
    n3neg = max(1.0 - nw(3), small)
    phi = -nw(0) * log(n3neg) + (nw(1) * nw(2) - nw(4) * nw(5)) / n3neg + &
         (nw(2)**3 - 3*nw(2)*nw(5)**2) / (24 * pi * n3neg ** 2)
  end function rosenfeld_fmt

  !> Whitebear FMT functional
  !! R. Roth, R. Evans, A. Lang and G. Kahl
  !! Fundamental measure theory for hard-sphere mixtures revisited: the White Bear version
  !! Journal of Physics: Condensed Matter
  !! 2002, 14(46):12063-12078
  !! doi: 10.1088/0953-8984/14/46/313
  !!
  !! In the bulk phase the functional reduces to the Boublik and
  !! Mansoori, Carnahan, Starling, and Leland (BMCSL) EOS.
  !! T. Boublik, doi: 10/bjgkjgb
  !! G. A. Mansoori, N. F. Carnahan, K. E. Starling, T. Leland, doi: 10/dkfhh7
  function wb_fmt(nv,nw) result(phi)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nv
    type(hyperdual), intent(in) :: nw(0:nv-1)
    type(hyperdual) :: phi
    ! Locals
    type(hyperdual) :: n3neg, logn3neg
    n3neg = max(1.0 - nw(3), small)
    logn3neg = log(n3neg)
    phi = -nw(0) * logn3neg + (nw(1) * nw(2) - nw(4) * nw(5)) / n3neg + &
         (nw(2) ** 3 - 3.0*nw(2)*nw(5)**2) * (nw(3) + n3neg**2 * logn3neg) / &
         (36 * pi * nw(3)**2*n3neg**2)
  end function wb_fmt

  !> Whitebear Mark II FMT functional
  !! Hendrik Hansen-Goos and Roland Roth
  !! Density functional theory for hard-sphere mixtures:
  !! the White Bear version mark II.
  !! Journal of Physics: Condensed Matter
  !! 2006, 18(37): 8413-8425
  !! doi: 10.1088/0953-8984/18/37/002
  function wbII_fmt(nv,nw) result(phi)
    use hyperdual_mod
    implicit none
    integer, intent(in) :: nv
    type(hyperdual), intent(in) :: nw(0:nv-1)
    type(hyperdual) :: phi
    ! Locals
    type(hyperdual) :: n3neg, logn3neg, n32, phi2_div3, phi3_div3
    n3neg = max(1.0 - nw(3), small)
    logn3neg = log(n3neg)
    n32 = nw(3)**2
    phi2_div3 = (1.0D0 / 3.0D0) * (2.0D0 - nw(3) + 2*n3neg * logn3neg / nw(3))
    phi3_div3 = (1.0D0 / 3.0D0) * (2.0D0 * nw(3) - 3 * n32 + 2 * nw(3) * n32 + 2*n3neg**2 * logn3neg) / n32
    phi = -nw(0) * logn3neg + (nw(1) * nw(2) - nw(4) * nw(5)) * &
         (1.0D0 + phi2_div3) / n3neg + (nw(2)**3 - 3*nw(2) * nw(5)**2) * &
         (1.0D0 - phi3_div3) / (24*pi*n3neg**2)
  end function wbII_fmt

  subroutine test_fmt(fmt_model)
    !
    integer, intent(in) :: fmt_model
    ! Locals
    integer, parameter :: n_grid=1, nv=6
    real :: nw(n_grid, nv), nw0(n_grid, nv)
    real :: phi(n_grid), phi_pm(n_grid), phi_pp(n_grid)
    real :: phi_n(n_grid,nv), phi_n_pm(n_grid,nv), phi_n_pp(n_grid,nv)
    real :: phi_nn(n_grid,nv,nv)
    real :: eps = 1.0e-5, dn
    real, parameter :: phi_ref(3) = (/0.02120926003404677, 0.021037042277793582, 0.021048705586168167/)
    integer :: i
    nw0(1,:) = (/ 0.013023390121386327, 0.0222485871456107, 0.4776290003040184, &
         0.2797390690655379, 0.0035959306386384605, 0.07719684602239196 /) ! Dummy weighted densities
    nw = nw0
    call fmt_energy_density(fmt_model,n_grid,nv,nw,phi,phi_n,phi_nn)
    print *,"phi",phi,phi_ref(fmt_model)
    do i=1,nv
      nw = nw0
      dn = eps*nw(1,i)
      nw(1,i) = nw(1,i) - dn
      call fmt_energy_density(fmt_model,n_grid,nv,nw,phi_pm,phi_n_pm)
      nw(1,i) = nw0(1,i) + dn
      call fmt_energy_density(fmt_model,n_grid,nv,nw,phi_pp,phi_n_pp)
      print *,"phi_n",phi_n(:,i), (phi_pp - phi_pm)/(2*dn)
      print *,"phi_nn",phi_nn(:,i,:)
      print *,"phi_nn",(phi_n_pp(:,:) - phi_n_pm(:,:))/(2*dn)
    enddo
  end subroutine test_fmt

end module fundamental_measure_theory
