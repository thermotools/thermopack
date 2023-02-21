module multipol_var
  use thermopack_constants, only: kB_const
  use numconstants, only: small
  implicit none
  private

  type multipol_param
    !integer, len :: nce ! Not working with gfortran 11.2.0
    real, allocatable :: mu_star_2(:)  !< Reduced dipol moment squared [-]
    real, allocatable :: Q_star_2(:)   !< Reduced quadrupol moment squared [-]
    integer, allocatable :: l_mu(:)   !< Number of dipol moments per molecule
    integer, allocatable :: l_Q(:)   !< Number of quadrupol moments per molecule
    real, allocatable :: m(:)
    real, allocatable :: eps_divk_ij(:,:), sigma_ij(:,:)
    real, allocatable :: sigma_ij_3(:,:), sigma_ij_5(:,:)
    !
    real, allocatable :: a_ij_QQ(:,:,:), b_ij_QQ(:,:,:), c_ijk_QQ(:,:,:,:)
    real, allocatable :: a_ij_DD(:,:,:), b_ij_DD(:,:,:), c_ijk_DD(:,:,:,:)
    real, allocatable :: a_ij_DQ(:,:,:), b_ij_DQ(:,:,:), c_ijk_DQ(:,:,:,:)
    ! Helper parameters
    integer, allocatable :: mu_indices(:) !<
    integer, allocatable :: Q_indices(:)  !<
    integer :: num_mu  !< Number of diplo moments different from zero
    integer :: num_Q   !< Number of diplo moments different from zero
    ! Model control
    logical :: enable_QQ = .true.
    logical :: enable_DD = .true.
    logical :: enable_DQ = .true.
  contains
    procedure, public :: init_multipol_param
  end type multipol_param

  ! Dipole parameters
  real, parameter :: AD(0:2,0:4) = reshape((/ 0.30435038064, 0.95346405973, -1.16100802773, &
    -0.13585877707, -1.83963831920, 4.52586067320, &
    1.44933285154, 2.01311801180, 0.97512223853, &
    0.35569769252, -7.37249576667, -12.2810377713, &
    -2.06533084541, 8.23741345333, 5.93975747420 /), (/3,5/))

  real, parameter :: BD(0:2,0:4) = reshape((/ 0.21879385627, -0.58731641193, 3.48695755800, &
    -1.18964307357, 1.24891317047, -14.9159739347, &
    1.16268885692, -0.50852797392, 15.3720218600, &
    0.0, 0.0, 0.0, &
    0.0, 0.0, 0.0 /), (/3,5/))

  real, parameter :: CD(0:2,0:3) = reshape((/ -0.06467735252, -0.95208758351, -0.62609792333, &
    0.19758818347, 2.99242575222, 1.29246858189, &
    -0.80875619458, -2.38026356489, 1.65427830900, &
    0.69028490492, -0.27012609786, -3.43967436378 /), (/3,4/))

  ! Quadrupole parameters
  real, parameter :: AQ(0:2,0:4) = reshape((/ 1.237830788, 1.285410878, 1.794295401, &
    2.435503144, -11.46561451, 0.769510293, &
    1.633090469, 22.08689285, 7.264792255, &
    -1.611815241, 7.46913832, 94.48669892, &
    6.977118504, -17.19777208, -77.1484579 /), (/3,5/))

  real, parameter :: BQ(0:2,0:4) = reshape((/ 0.454271755, -0.813734006, 6.868267516, &
    -4.501626435, 10.06402986, -5.173223765, &
    3.585886783, -10.87663092, -17.2402066, &
    0.0, 0.0, 0.0, &
    0.0, 0.0, 0.0 /), (/3,5/))

  real, parameter :: CQ(0:2,0:3) =   reshape((/ -0.500043713, 2.000209381, 3.135827145, &
    6.531869153, -6.78386584, 7.247588801, &
    -16.01477983, 20.38324603, 3.075947834, &
    14.42597018, -10.89598394, 0.0 /), (/3,4/))

  ! Dipole-Quadrupole parameters
  real, parameter :: ADQ(0:2,0:3) = reshape((/ 0.697094963, -0.673459279, 0.670340770, &
    -0.633554144, -1.425899106, -4.338471826, &
    2.945509028, 4.19441392, 7.234168360, &
    -1.467027314, 1.0266216, 0.0 /), (/3,4/))

  real, parameter :: BDQ(0:2,0:3) = reshape((/ -0.484038322, 0.67651011, -1.167560146, &
    1.970405465, -3.013867512, 2.13488432, &
    -2.118572671, 0.46742656, 0.0, &
    0.0, 0.0, 0.0 /), (/3,4/))

  real, parameter :: CDQ(0:1,0:2) = reshape((/ 0.795009692, -2.099579397, &
    3.386863396, -5.941376392, &
    0.475106328, -0.178820384 /), (/2,3/))

  public :: multipol_param
  public :: multipol_param_constructor

contains

  subroutine init_multipol_param(self,nce,mu,Q,m,sigma_ij,eps_divk_ij)
    class(multipol_param), intent(inout) :: self
    integer, intent(in) :: nce
    real, intent(in) :: mu(nce)  !< [D]
    real, intent(in) :: Q(nce)   !< [ÅD]
    real, intent(in) :: m(nce)  !<
    real, intent(in) :: sigma_ij(nce,nce)   !<
    real, intent(in) :: eps_divk_ij(nce,nce)  !<
    ! Locals.
    integer :: i, j, k, n
    real :: mij, mijk
    self%m = m
    self%sigma_ij = sigma_ij
    self%sigma_ij_3 = self%sigma_ij**3
    self%sigma_ij_5 = self%sigma_ij**5
    self%eps_divk_ij = eps_divk_ij
    ! Setting number of moments to one
    self%l_Q = 1
    self%l_mu = 1

    !(1 statC)^2 = 1 cm^3 g /s^2 * 1e-6 (kg/cm)^3 1.0e-3 kg/g
    !D^2 = 10e−40 m^2
    !J * m^3 = kg m^5 /s2
    do i=1,nce
      self%mu_star_2(i) = 1e-49 * mu(i)**2 / (m(i)*kB_const*eps_divk_ij(i,i)*self%sigma_ij_3(i,i))
      self%Q_star_2(i) = 1e-69 * Q(i)**2 / (m(i)*kB_const*eps_divk_ij(i,i)*self%sigma_ij_5(i,i))
    enddo
    !
    self%mu_indices = 0
    self%Q_indices = 0
    self%num_mu = 0
    self%num_Q = 0
    do i=1,nce
      if (abs(mu(i)) > small) then
        self%num_mu = self%num_mu + 1
        self%mu_indices(self%num_mu) = i
      endif
      if (abs(Q(i)) > small) then
        self%num_Q = self%num_Q + 1
        self%Q_indices(self%num_Q) = i
      endif
    end do

    do i=1,nce
      do j=1,nce
        mij = sqrt(m(i)*m(j))
        do n=0,4
          self%a_ij_QQ(n,i,j) = AQ(0,n) + AQ(1,n)*(mij-1)/mij + AQ(2,n)*(mij-1)/mij*(mij-2)/mij
          self%b_ij_QQ(n,i,j) = (BQ(0,n) + BQ(1,n)*(mij-1)/mij + BQ(2,n)*(mij-1)/mij*(mij-2)/mij)&
               *self%eps_divk_ij(i,j)
          if (n < 4) then
            self%a_ij_DQ(n,i,j) = ADQ(0,n) + ADQ(1,n)*(mij-1)/mij + ADQ(2,n)*(mij-1)/mij*(mij-2)/mij
            self%b_ij_DQ(n,i,j) = (BDQ(0,n) + BDQ(1,n)*(mij-1)/mij + BDQ(2,n)*(mij-1)/mij*(mij-2)/mij)&
                 *self%eps_divk_ij(i,j)
          endif
          mij = min(mij,2.0)
          self%a_ij_DD(n,i,j) = AD(0,n) + AD(1,n)*(mij-1)/mij + AD(2,n)*(mij-1)/mij*(mij-2)/mij
          self%b_ij_DD(n,i,j) = (BD(0,n) + BD(1,n)*(mij-1)/mij + BD(2,n)*(mij-1)/mij*(mij-2)/mij)&
               *self%eps_divk_ij(i,j)
        enddo
        do k=1,nce
          mijk = (m(i)*m(j)*m(k))**(1.0/3.0)
          do n=0,3
            self%c_ijk_QQ(n,i,j,k) = CQ(0,n) + CQ(1,n)*(mijk-1)/mijk + CQ(2,n)*(mijk-1)/mijk*(mijk-2)/mijk
            if (n < 3) then
              self%c_ijk_DQ(n,i,j,k) = CDQ(0,n) + CDQ(1,n)*(mijk-1)/mijk
            endif
            mijk = min(mijk,2.0)
            self%c_ijk_DD(n,i,j,k) = CD(0,n) + CD(1,n)*(mijk-1)/mijk + CD(2,n)*(mijk-1)/mijk*(mijk-2)/mijk
          enddo
        enddo
      enddo
    enddo

  end subroutine init_multipol_param

  !> Allocate memory for multipol_param
  function multipol_param_constructor(nce) result(mpol_param)
    ! Input:
    integer, intent(in) :: nce
    ! Created object:
    type(multipol_param) :: mpol_param
    ! Locals
    allocate(mpol_param%mu_star_2(nce),&
         mpol_param%Q_star_2(nce),&
         mpol_param%m(nce),&
         mpol_param%l_mu(nce),&
         mpol_param%l_Q(nce),&
         mpol_param%eps_divk_ij(nce,nce),&
         mpol_param%sigma_ij(nce,nce),&
         mpol_param%sigma_ij_3(nce,nce),&
         mpol_param%sigma_ij_5(nce,nce),&
         mpol_param%a_ij_QQ(0:4,nce,nce),&
         mpol_param%b_ij_QQ(0:4,nce,nce),&
         mpol_param%c_ijk_QQ(0:3,nce,nce,nce),&
         mpol_param%a_ij_DD(0:4,nce,nce),&
         mpol_param%b_ij_DD(0:4,nce,nce),&
         mpol_param%c_ijk_DD(0:3,nce,nce,nce),&
         mpol_param%a_ij_DQ(0:3,nce,nce),&
         mpol_param%b_ij_DQ(0:3,nce,nce),&
         mpol_param%c_ijk_DQ(0:2,nce,nce,nce),&
         mpol_param%mu_indices(nce),&
         mpol_param%Q_indices(nce))
  end function multipol_param_constructor

end module multipol_var
