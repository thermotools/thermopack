!---------------------------------------------------------------------
! Module implementing uv-theory equation of state for Mie fluids.
!
! REFERENCES
!
! [1] van Westen and Gross 2021, Accurate thermodynamics of simple
! fluids and chain fluids based on first-order perturbation theory and
! second virial coefficients: uv-theory. J Chem Phys 155
! (10.1063/5.0073572)
! ---------------------------------------------------------------------
module uv_theory
  use hyperdual_mod
  use numconstants, only: PI
  use pair_potentials
  use thermopack_constants, only: kB_const,N_AVOGADRO, ref_len, uid_len
  use eosdata, only: eosMie_UV_WCA, eosMie_UV_BH
  use thermopack_var, only: base_eos_param, get_active_eos, base_eos_dealloc
  use hardsphere_bmcsl
  implicit none
  save

  ! Coefficient vectors for u fraction correlation (Table I in [1])
  real, parameter :: C_PHI_WCA_LJ(2) = (/ 1.5661, 2.5422 /)
  real, parameter :: C_PHI_WCA_MIE(3) = (/ 1.4419, 1.1169, 16.8810 /)
  real, parameter :: C_PHI_BH_LJ(4) = (/ 0.74158, 0.14102, 2.3966, 4.6984 /)
  real, parameter :: C1_PHI_BH_MIE(4) = (/ 0.72188, -0.0059822, 2.2919, 5.1647 /)
  real, parameter :: C2_PHI_BH_MIE(4) = (/ 0.0, 2.4676, 14.9735, 2.4017 /)
  real, parameter :: A_PHI_BH_MIE = 1.2187
  real, parameter :: B_PHI_BH_MIE = 4.2773

  ! Coefficient vectors for hard sphere diameter d (Table S23 in [1])
  real, parameter :: C_DHS_WCA_MIE(9) = (/ &
       1.92840364363978E+00, 4.43165896265079E-01, 5.20120816141761E-01 , &
       1.82526759234412E-01, 1.10319989659929E-02, -7.97813995328348E-05 , &
       1.29885156087242E-02, 6.41039871789327E-03, 1.85866741090323E-05 /)
  real, parameter :: C_DHS_BH_MIE(9) = (/ &
       1.09360455168912E-02, -2.00897880971934E-01, -1.27074910870683E-02 ,&
       1.40422470174053E-02, 7.35946850956932E-02, 1.28463973950737E-02 ,&
       3.71527116894441E-03, 5.05384813757953E-03, 4.91003312452622E-02 /)

  ! Coefficient vectors for effective packing fraction \eta_A (Table S2 in [1])
  real, parameter :: C_ETA_A_WCA_MIE(4,4) = RESHAPE( (/ &
       -0.888512176, 0.265207151, -0.851803291, -1.380304110, &
       -0.395548410, -0.626398537, -1.484059291, -3.041216688, &
       -2.905719617, -1.778798984, -1.556827067, -4.308085347, &
       0.429154871, 20.765871545, 9.341250676, -33.787719418 /), &
       shape=(/4,4/), order=(/2,1/))
  real, parameter :: C_ETA_A_BH_MIE(3,4) = RESHAPE( (/ &
       -1.217417282, 6.754987582, -0.5919326153, -28.99719604, &
       1.579548775, -26.93879416, 0.3998915410, 106.9446266, &
       -1.993990512, 44.11863355, -40.10916106, -29.61308489 /), &
       shape=(/3,4/), order=(/2,1/))

  ! Coefficient matrices for effective packing fraction \eta_B (Eqs S35, S36 in [1])
  real, parameter :: C_ETA_B_WCA_MIE(3,2) = RESHAPE( (/ &
       -0.883143456, -0.618156214,&
       -0.589914255, -3.015264636,&
       -2.152046477, 4.7038689542/), shape=(/3,2/), order=(/2,1/))
  real, parameter :: C_ETA_B_BH_MIE(3,2) = RESHAPE( (/ &
       -0.960919783, -0.921097447, &
       -0.547468020, -3.508014069, &
       -2.253750186, 3.581161364/), shape=(/3,2/), order=(/2,1/))

  ! Coefficient matrix for the dimensionless correlation integral I^u_BH (Table S3 in [1])
  real, parameter :: C_IU_BH_MIE(3,4) = RESHAPE( (/ &
       0.0, 0.0, 0.0, 0.0, &
       0.1689669965, -0.9915458191, 0.7431421806, -4.323495934, &
       -0.5326281629, 2.660390140, -1.950702799, -0.000137219512 &
       /), shape=(/3,4/), order=(/2,1/))

  ! Coefficient matrix for the dimensionless correlation integral I^u_WCA (Table S4 in [1])
  real, parameter :: C_IU_WCA_MIE(6,6) = RESHAPE( (/ &
       -0.2622378162, 0.6585817423, 5.5318022309, 0.6902354794, -3.6825190645, -1.7263213318, &
       -0.1899241690, -0.5555205158, 9.1361398949, 0.7966155658, -6.1413017045 , 4.9553415149, &
       0.1169786415, -0.2216804790, -2.0470861617, -0.3742261343, 0.9568416381 , 10.1401796764, &
       0.5852642702, 2.0795520346, 19.0711829725, -2.3403594600, 2.5833371420, 432.3858674425, &
       -0.6084232211, -7.2376034572, 19.0412933614, 3.2388986513, 75.4442555789, -588.3837110653, &
       0.0512327656, 6.6667943569, 47.1109947616, -0.5011125797, -34.8918383146, 189.5498636006 &
       /), shape=(/6,6/), order=(/2,1/))


  ! Coefficients for calculation of DeltaB2 (Table S5 in [1])
  real, parameter :: C_DELTAB2_WCA(6) = (/ 1.45805207053190E-03, 3.57786067657446E-02, &
       1.25869266841313E-04, 1.79889086453277E-03, 0.0, 0.0 /)
  real, parameter :: A_DELTAB2_WCA = 1.05968091375869
  real, parameter :: B_DELTAB2_WCA = 3.41106168592999


  type, extends(base_eos_param) :: uv_theory_param
     ! Mie potential parameters
     type(mie_potential_hd), allocatable :: mie(:,:)
     real, allocatable :: lambda_r(:,:)        !< Repulsive exponent [-]
     real, allocatable :: lambda_a(:,:)        !< Attractive exponent [-]
     real, allocatable :: sigma(:,:)           !< [m]
     real, allocatable :: eps_divk(:,:)        !< [K]
     real, allocatable :: Cmie(:,:)            !< Mie potential prefactor [-]
     real, allocatable :: rmin(:,:)            !< Location of minimum [m]
   contains
     procedure, public :: dealloc => uv_dealloc
     procedure, public :: allocate_and_init => uv_allocate_and_init
     procedure, public :: calc_onefluid_params
     ! Assignment operator
     procedure, pass(This), public :: assign_eos => assign_uv_eos
  end type uv_theory_param

  public :: uv_theory_param
  public :: calc_fres_uv
  
contains


  subroutine init_uv_theory(nc,comp,uv,ref)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas, kRgas, N_Avogadro, kB_const
    integer, intent(in) :: nc          !< Number of components.
    type(gendata_pointer), intent(inout) :: comp(nc)    !< Component vector.
    class(uv_theory_param), intent(inout) :: uv
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    ! Locals
    real :: lamr, lama, sigma, eps_divk
    integer :: idx

    ! Deallocate old memory and init new memory
    call uv%allocate_and_init(nc, eos_label="UV-MIE-WCA")

    idx = getUVdataIdx(uv%subeosidx,trim(comp(1)%p_comp%ident),ref)
    sigma = MieArray(idx)%sigma
    eps_divk = MieArray(idx)%eps_divk
    lamr = MieArray(idx)%lamr
    lama = 6.0

    ! Set component data
    uv%lambda_a = 6.0
    uv%lambda_r = lamr
    uv%eps_divk = eps_divk
    uv%sigma = sigma
    uv%CMie = abs(lamr/(lamr-lama) * (lamr/lama)**(lama/(lamr-lama)))
    uv%rmin = sigma*(lamr/6)**(1/(lamr-6))

    ! Set consistent Rgas
    Rgas = N_Avogadro*kB_const
    kRgas = Rgas*1.0e3

    ! Set ideal gas Cp
    comp(1)%p_comp%id_cp%cptype = 8
    comp(1)%p_comp%id_cp%cp(:) = 0.0
    comp(1)%p_comp%id_cp%cp(1) = 2.5
  end subroutine init_uv_theory


  subroutine calc_fres_uv(eos,nc,T,V,n, f,f_T,f_V,f_n,&
       f_TT,f_VV,f_TV,f_Tn,f_Vn,f_nn)
    use hyperdual_utility, only: hyperdual_fres_wrapper
    !> The reduced helmholtz energy from uv-theory
    class(uv_theory_param), intent(inout) :: eos
    integer, intent(in)                :: nc
    real, intent(in)  :: T             !< temperature (K)
    real, intent(in)  :: V             !< volume (m^3)
    real, intent(in)  :: n(nc)         !< mole numbers (mol)
    real, intent(out) :: f             !< Fres=Ares/RT (mol)
    real, optional, intent(inout) :: f_T,f_V,f_TT,f_VV,f_TV
    real, optional, intent(inout) :: f_n(nc),f_Tn(nc),f_Vn(nc),f_nn(nc,nc)
    ! Locals
    
    call hyperdual_fres_wrapper(fun,eos,nc,T,V,n,f,f_T,f_V,f_n,&
       f_TT,f_VV,f_TV,f_Tn,f_Vn,f_nn)
    
  contains
    function fun(eos,nc,T,V,n) result(f)
      !class(uv_theory_param), intent(inout) :: eos
      class(base_eos_param), intent(inout) :: eos
      integer, intent(in)                :: nc
      type(hyperdual), intent(in)  :: T             !< temperature (K)
      type(hyperdual), intent(in)  :: V             !< volume (m^3)
      type(hyperdual), intent(in)  :: n(nc)         !< mole numbers (mol)
      type(hyperdual) :: f             !< Fres=Ares/RT (mol)
      ! Locals
      type(hyperdual) :: rho, z(nc), a
      z = n/sum(n)
      rho = sum(n)/V
      select type ( p_eos => eos )
      type is ( uv_theory_param )
         call calc_a_uv(p_eos, nc, T, rho=rho, z=z, a=f)
      end select
      f = a*sum(n)
    end function fun
  end subroutine calc_fres_uv
  
  

  subroutine calc_a_uv(eos, nc, T, rho, z, a)
    !> The reduced helmholtz energy from uv-theory
    integer, intent(in)                :: nc
    class(uv_theory_param), intent(inout) :: eos
    type(hyperdual), intent(in)  :: T             !< temperature (K)
    type(hyperdual), intent(in)  :: rho           !< density (mol/m^3)
    type(hyperdual), intent(in)  :: z(nc)         !< mole fractions (-)
    type(hyperdual), intent(out) :: a             !< helmholtz energy a=A/NRT (-)
    ! Locals
    type(hyperdual) :: rho_r, epsdivk_x, T_x, denom
    type(hyperdual) :: a0, a_hs, delta_a0, Delta_a1u
    type(hyperdual) :: phi, Delta_B2, Delta_B2u
    type(hyperdual) :: dhs, diameters(nc)
    type(hyperdual) :: lama, lamr
    integer :: i,j

    lamr = eos%mie(1,1)%lamr
    lama = eos%mie(1,1)%lama
    do i = 1,nc
       if (eos%mie(i,i)%lamr /= lamr .or. eos%mie(i,i)%lama /= lama) then
          stop "Invalid Mie exponents in uv-theory"
       end if
    end do

    ! Calculate one-fluid parameters
    rho_r = 0.0
    epsdivk_x = 0.0
    denom = 0.0
    do i=1,nc
       rho_r = rho_r + rho*z(i)*eos%mie(i,i)%sigma**3
       do j=1,nc
          epsdivk_x = epsdivk_x + z(i)*z(j)*eos%mie(i,j)%sigma**3*eos%mie(i,j)%epsdivk
          denom = denom + z(i)*z(j)*eos%mie(i,j)%sigma**3
       end do
    end do
    epsdivk_x = epsdivk_x/denom
    T_x = T/epsdivk_x

    ! Calculate hard sphere part of reference system (Boublik-Mansoori
    ! for additive hard spheres)
    call dhs_WCA_Mie(T,eos%mie(1,1), dhs)
    do i=1,nc
       diameters(i) = dhs*eos%mie(i,i)%sigma/eos%mie(1,1)%sigma
    end do
    call calc_a_hardsphere_bmcsl(nc, rho, z, diameters, a_hs)

    ! Calculate perturbation part of reference system
    call Delta_a0_WCA_Mie(eos, nc, T, rho, z, delta_a0)

    ! Calculate contribution from reference system
    a0 = a_hs + delta_a0

    ! Calculate Delta a1u and Delta B2u
    call delta_a1u_b2u_WCA_Mie(eos, nc, T, rho, z, delta_a1u, delta_b2u)

    ! Calculate Delta B2
    call DeltaB2_WCA_Mie(eos,nc,T,z, Delta_B2)

    ! Calculate u fraction phi
    call phi_WCA_Mie(rho_r, lamr, phi)

    ! Calculate total helmholtz energy
    a = a0 + phi*Delta_a1u + (1.0-phi)*(Delta_B2 - Delta_B2u)*rho

  end subroutine calc_a_uv


  subroutine assign_uv_eos(this, other)
    class(uv_theory_param), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (uv_theory_param)
       call this%assign_base_eos_param(other)
       this%mie = other%mie
       ! this%lambda_r = other%lambda_r
       ! this%lambda_a = other%lambda_a
       ! this%sigma = other%sigma
       ! this%eps_divk = other%eps_divk
       ! this%Cmie = other%Cmie
       ! this%rmin = other%rmin
    class default
    end select
  end subroutine assign_uv_eos

  !> Get the index in the MieArray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getUVdataIdx(eosidx,compName,ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, ref
    integer :: idx, idx_default
    logical :: found

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nMie)
       if ((eosidx == Miearray(idx)%eosidx) .and. &
            str_eq(compName, Miearray(idx)%compName)) then
          if (string_match(ref,Miearray(idx)%ref)) then
             found = .true.
             exit
          else if (string_match("DEFAULT",Miearray(idx)%ref)) then
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
       call stoperror("The UV parameters don't exist.")
    end if

  end function getUVdataIdx


  subroutine calc_onefluid_params(this,nc,z, epsdivk, sigma3_single, sigma3_double)
    class(uv_theory_param), intent(in) :: this
    integer, intent(in) :: nc                           !< Number of components.
    type(hyperdual), intent(in) :: z(nc)                !< Mole fractions
    type(hyperdual), intent(out) :: sigma3_single   !< Cubed one-fluid diameter
    type(hyperdual), intent(out) :: sigma3_double   !< Cubed one-fluid diameter
    type(hyperdual), intent(out) :: epsdivk         !< One-fluid energy scale
    ! Locals
    integer :: i, j

    epsdivk = 0.0
    sigma3_single = 0.0
    sigma3_double = 0.0
    do i=1,nc
       sigma3_single = sigma3_single + z(i) * this%mie(i,i)%sigma**3
       do j=1,nc
          sigma3_double = sigma3_double + z(i)*z(j) * this%mie(i,j)%sigma**3
          epsdivk = epsdivk + z(i)*z(j) * this%mie(i,j)%sigma**3 * this%mie(i,j)%epsdivk
       end do
    end do
    epsdivk = epsdivk / sigma3_double
  end subroutine calc_onefluid_params


  subroutine uv_dealloc(eos)
    class(uv_theory_param), intent(inout) :: eos
    ! Locals
    integer :: stat
    call base_eos_dealloc(eos)

    deallocate(eos%mie,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")

    deallocate(eos%lambda_r,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")

    deallocate(eos%lambda_a,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")

    deallocate(eos%sigma,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")

    deallocate(eos%eps_divk,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")

    deallocate(eos%CMie,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")

    deallocate(eos%rmin,stat=stat)
    if (stat /= 0) call stoperror("saftvrmie_dealloc: Not able to deallocate")
  end subroutine uv_dealloc

  subroutine uv_allocate_and_init(eos,nc,eos_label)
    class(uv_theory_param), intent(inout) :: eos
    integer, intent(in) :: nc
    character(len=*), intent(in) :: eos_label !< EOS label
    ! Locals

    call eos%dealloc()
    allocate(eos%mie(nc,nc))
    allocate(eos%lambda_r(nc,nc))
    allocate(eos%lambda_a(nc,nc))
    allocate(eos%sigma(nc,nc))
    allocate(eos%eps_divk(nc,nc))
    allocate(eos%Cmie(nc,nc))
    allocate(eos%rmin(nc,nc))
  end subroutine uv_allocate_and_init


  subroutine delta_a1u_b2u_WCA_Mie(eos, nc, T, rho, z, delta_a1u, delta_b2u)
    !> Pertubation contribution to a1u going into the u-term in uv-theory (Eq S42 in [1])
    type(uv_theory_param), intent(in) :: eos ! uv-theory eos
    integer, intent(in) :: nc ! number of components
    type(hyperdual), intent(in) :: T      ! reduced temperature (-)
    type(hyperdual), intent(in) :: rho    ! reduced density (-)
    type(hyperdual), intent(in) :: z(nc)    ! mole fractions
    type(hyperdual), intent(out) :: delta_a1u
    type(hyperdual), intent(out) :: delta_b2u
    ! Locals
    type(hyperdual) :: beta, lamr, lama
    type(hyperdual) :: epsdivk, sigma3_single, sigma3_double, Iu, Iu_zerodensity, rho_r, T_r
    type(mie_potential_hd) :: mie_x

    ! Calc effective one-fluid parameters
    !this,nc,z, epsdivk, sigma3_single, sigma3_double
    call eos%calc_onefluid_params(nc,z, epsdivk=epsdivk, &
         sigma3_single=sigma3_single, sigma3_double=sigma3_double)
    lamr = eos%mie(1,1)%lamr
    lama = eos%mie(1,1)%lama
    call mie_x%init(lama=lama, lamr=lamr, sigma=(sigma3_single)**(1.0/3.0), epsdivk=epsdivk)
    rho_r = rho*sigma3_single
    T_r = T/epsdivk

    ! Calc correlation integral of effective pure fluid
    call Iu_WCA_Mie(T_r, rho_r, mie_x, Iu_zerodensity, Iu)

    ! Calculate Delta_a1u (Eq S42 in [1])
    beta = 1.0/(kB_const*T)
    Delta_a1u = 2*PI*rho*beta*(epsdivk*sigma3_double)*Iu
    
    Delta_b2u = 2*PI*beta*(epsdivk*sigma3_double)*Iu_zerodensity

  end subroutine delta_a1u_b2u_WCA_Mie


  subroutine Iu_WCA_Mie(T_r, rho_r, mie, Iu_zerodensity, Iu)
    !> Correlation integral of pure fluids going into the u-term in uv-theory (Eq S54 in [1])
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(in) :: T_r      ! reduced temperature (-)
    type(hyperdual), intent(in) :: rho_r    ! reduced density (-)
    type(hyperdual), intent(out) :: Iu_zerodensity, Iu ! correlation integral and its zero density limit
    ! Locals
    type(hyperdual) :: n, nu, sigma, eps_divk                   ! Mie attractive exponent
    type(hyperdual) :: rm, rs, alpha_rm
    type(hyperdual) :: C(6), pade_num, pade, qhs, dhs, tau, tauvec(6)
    integer :: i, j

    ! Extract Mie parameters for component ic
    n = mie%lama
    nu = mie%lamr
    sigma = mie%sigma
    eps_divk = mie%epsdivk
    rm = mie%rmin
    rs = rm

    ! Calculate zero-density limit of Iu
    call qhs_WCA_Mie(T_r=T_r, mie=mie, qhs=qhs)
    alpha_rm = alpha_x(rm, mie)
    Iu_zerodensity = (qhs**3-rm**3)/3.0 - alpha_rm

    ! Calculate length scale tau (Eq S33)
    call dhs_WCA_Mie(T_r=T_r,mie=mie, dhs=dhs)
    tau = rs - dhs

    ! Calculate remaining Padé approximation for the density-dependent term
    tauvec(1) = 1.0
    tauvec(2) = 1.0/nu
    tauvec(3) = 1.0/nu**2
    tauvec(4:6) = tauvec(1:3)*tau
    C = 0.0
    do i=1,6
       do j=1,6
          C(i) = C(i) + C_IU_WCA_MIE(i,j)*tauvec(j)
       end do
    end do
    pade_num = mie%cmie*(C(1)*rho_r + C(2)*rho_r**2 + C(3)*rho_r**3)
    pade = pade_num/(1.0 + C(4)*rho_r + C(5)*rho_r**2 + C(6)*rho_r**3)

    Iu = Iu_zerodensity + pade

  end subroutine Iu_WCA_Mie


  subroutine DeltaB2_WCA_Mie(eos,nc,T,z, DeltaB2)
    !> Perturbation contribution to the v-term in uv-theory (Eq S57 in [1])
    integer, intent(in) :: nc !< Number of components
    class(uv_theory_param), intent(in) :: eos
    type(hyperdual), intent(in) :: T         ! reduced temperature
    type(hyperdual), intent(in) :: z(nc)
    type(hyperdual), intent(out) :: DeltaB2  ! Perturbation contribution to B2
    ! Locals
    type(hyperdual) :: T_r, DeltaB2ij
    integer :: i, j

    DeltaB2 = 0.0
    do i=1,nc
       do j=1,nc
          T_r = T/eos%mie(i,j)%epsdivk
          call DeltaB2_pure_WCA_Mie(T_r=T_r, mie=eos%mie(i,j), DeltaB2=DeltaB2ij)
          DeltaB2 = DeltaB2 + z(i)*z(j)*DeltaB2ij
       end do
    end do
  end subroutine DeltaB2_WCA_Mie


  ! TODO: double-check whether parameters are reduced used correctly
  subroutine DeltaB2_pure_WCA_Mie(T_r, mie, DeltaB2)
    !> Perturbation contribution to the v-term in uv-theory (Eq S57 in [1])
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(in) :: T_r         ! reduced temperature
    type(hyperdual), intent(out) :: DeltaB2  ! Perturbation contribution to B2
    ! Locals
    type(hyperdual) :: C0, C1, C2, C3
    real :: a, b
    type(hyperdual) :: rm, rc, rs, alpha_rs, alpha_rc, beta, beta_eff, qhs
    type(hyperdual) :: sigma, eps_divk, n,nu

    ! Extract Mie parameters
    n = mie%lama
    nu = mie%lamr
    sigma = mie%sigma
    eps_divk = mie%epsdivk
    rm = mie%rmin
    rs = rm

    ! Precalculate quantities
    call qhs_WCA_Mie(T_r, mie, qhs)
    qhs = qhs/mie%sigma
    alpha_rs = alpha_x(rm, mie)
    alpha_rc = alpha_x(rc, mie)
    beta = 1.0/T_r

    ! Calculate beta_eff (Eq S58 in [1])
    C0 = 1.0 - 3*(alpha_rs-alpha_rc)/(rc**3-rs**3)
    C1 = C_DELTAB2_WCA(1) + C_DELTAB2_WCA(2)/nu
    C2 = C_DELTAB2_WCA(3) + C_DELTAB2_WCA(4)/nu
    C3 = C_DELTAB2_WCA(5) + C_DELTAB2_WCA(6)/nu
    a = A_DELTAB2_WCA
    b = B_DELTAB2_WCA
    beta_eff = beta*(1.0 - C0/(1.0 + C1*beta**a + C2*beta**b + 0.0))

    DeltaB2 = -2*PI*sigma**3*( (rm**3-qhs**3)/3.0*(exp(beta)-1.0) + &
         (rc**3-rm**3)/3.0*(exp(beta_eff)-1.0) + beta*alpha_rc)

  end subroutine DeltaB2_Pure_WCA_Mie


  type(hyperdual) function alpha_x(x, mie)
    type(hyperdual), intent(in) :: x
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    ! Locals
    type(hyperdual) :: n, nu
    n = mie%lama
    nu = mie%lamr
    alpha_x = mie%cmie * (x**(3.0-n)/(n-3.0) - x**(3.0-nu)/(nu-3.0))
  end function alpha_x

  subroutine calc_eta_dhs(nc, eos, T, rho, z, eta)
    integer, intent(in) :: nc !< Number of components
    class(uv_theory_param), intent(in) :: eos
    type(hyperdual), intent(in) :: T, rho, z(nc)
    type(hyperdual), intent(out) :: eta
    ! Locals
    type(hyperdual) :: dhs
    integer :: i
    eta = 0.0
    do i=1,nc
       call dhs_WCA_Mie(T/eos%mie(i,i)%epsdivk, eos%mie(i,i), dhs)
       eta = eta + pi/6*rho*z(i)*dhs**3
    end do
  end subroutine calc_eta_dhs

  subroutine Delta_a0_WCA_Mie(eos, nc, T, rho, x, delta_a0)
    !> Perturbation contribution delta_a0 to the reference Helmholtz
    !> energy a0, defined by a_0 = ahs_d + delta_a0. See Eq S40 in [1].
    integer, intent(in) :: nc !< Number of components
    class(uv_theory_param), intent(in) :: eos
    type(hyperdual), intent(in) :: T
    type(hyperdual), intent(in) :: rho
    type(hyperdual), intent(in) :: x(nc)
    type(hyperdual), intent(out) :: delta_a0
    ! Locals
    type(hyperdual) :: IA, IB, I0f, eta, etaA, etaB, qhs_r, dhs_r, Tij_r, tau, rs_r
    integer :: i, j

    call calc_eta_dhs(nc, eos, T, rho, x, eta)

    delta_a0 = 0.0
    do i=1,nc
       do j=1,nc
          Tij_r = T/eos%mie(i,j)%epsdivk
          rs_r = eos%mie(i,j)%rmin_adim

          call dhs_WCA_Mie(Tij_r, eos%mie(i,j), dhs_r)
          dhs_r = dhs_r/eos%mie(i,j)%sigma
          call qhs_WCA_Mie(Tij_r, eos%mie(i,j), qhs_r)
          qhs_r = qhs_r/eos%mie(i,j)%sigma

          tau = eos%mie(i,j)%rmin_adim - dhs_r ! adimensional length scale
          call etaA_WCA_Mie(eta, tau, eos%mie(i,j), etaA)
          call etaB_WCA_Mie(eta, tau, etaB)

          IA = (1.0-etaA/2.0)/(1.0-etaA)**3 * (rs_r**3-qhs_r**3)/3.0
          IB = (1.0-etaB/2.0)/(1.0-etaB)**3 * (rs_r**3-dhs_r**3)/3.0
          I0f = IA-IB
          delta_a0 = delta_a0 + x(i)*x(j)*eos%sigma(i,j)**3 * I0f
       end do
    end do
    delta_a0 = -2*PI*rho*delta_a0
  end subroutine Delta_A0_WCA_Mie


  subroutine etaA_WCA_Mie(eta, tau, mie, etaA)
    !> Intermediate quantity to calculate the soft-repulsive
    !> perturbation term \Delta a0. The effective packing fraction
    !> used to calculate I_A
    !real, intent(in) :: rs_r
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(in) :: tau   ! adimensional length scale
    type(hyperdual), intent(in) :: eta   ! packing fraction
    type(hyperdual), intent(out) :: etaA ! effective packing fraction A
    ! Locals
    type(hyperdual) :: C(4), nu
    nu = mie%lamr
    C =     (C_ETA_A_WCA_MIE(:,1)+C_ETA_A_WCA_MIE(:,2)/nu)*tau
    C = C + (C_ETA_A_WCA_MIE(:,3)+C_ETA_A_WCA_MIE(:,4)/nu)*tau**2

    etaA = eta + C(1)*eta + C(2)*eta**2 + C(3)*eta**3 + C(4)*eta**4
  end subroutine etaA_WCA_Mie

  subroutine etaB_WCA_Mie(eta, tau, etaB)
    !> Intermediate quantity to calculate the soft-repulsive
    !> perturbation term \Delta a0. The effective packing fraction
    !> used to calculate I_A
    !real, intent(in) :: rs_r
    type(hyperdual), intent(in) :: tau   ! adimensional length scale
    type(hyperdual), intent(in) :: eta   ! packing fraction
    type(hyperdual), intent(out) :: etaB ! effective packing fraction B
    ! Locals
    type(hyperdual) :: C(3)
    integer :: i

    ! Eq S36 in [1]
    do i=1,3
       C(i) = C_ETA_B_WCA_MIE(i,1)*tau + C_ETA_B_WCA_MIE(i,2)*tau**2
    end do

    ! \TODO: double-check
    etaB = eta + C(1)*eta + C(2)*eta**2 + C(3)*eta**3
  end subroutine etaB_WCA_Mie

  subroutine dhs_WCA_Mie(T_r,mie,dhs)
    !> The effective hard sphere diameter used for WCA
    type(hyperdual), intent(in) :: T_r     ! reduced temperature (-)
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(out) :: dhs    ! hard-sphere diameter (m)
    ! Locals
    type(hyperdual) :: c, nu

    ! Hard-sphere diameter (Eq S19)
    nu = mie%lamr
    c = (nu/6.0)**(-nu/(12.0-2*nu)) - 1.0
    dhs = mie%rmin*(1.0/(1.0+c*T_r**0.5))**(2.0/nu)
  end subroutine dhs_WCA_Mie

  subroutine qhs_WCA_Mie(T_r,mie,qhs)
    !> The effective hard sphere diameter used for the WCA perturbation contribution to DeltaB2
    type(hyperdual), intent(in) :: T_r   ! reduced temperature (-)
    type(mie_potential_hd), intent(in) :: mie ! Mie potential
    type(hyperdual), intent(out) :: qhs  ! hard-sphere diameter (m)
    ! Locals
    type(hyperdual) :: n, nu, nu7, C0, C1, C2, C3

    ! Assemble C coefficients (Eqs S24 and S26)
    n = mie%lama
    nu = mie%lamr
    nu7 = nu-7.0
    C0 = sqrt(2*PI*nu/n)
    C1 = C_DHS_WCA_MIE(1) + C_DHS_WCA_MIE(2)*nu7
    C2 = C_DHS_WCA_MIE(3) + C_DHS_WCA_MIE(4)*nu7 + C_DHS_WCA_MIE(5)*nu7**2 + C_DHS_WCA_MIE(6)*nu7**3
    C3 =                    C_DHS_WCA_MIE(7)*nu7 + C_DHS_WCA_MIE(8)*nu7**2 + C_DHS_WCA_MIE(9)*nu7**3

    ! Hard-sphere diameter (S28)
    qhs = mie%rmin*(1.0 + C0*T_r**0.5 + C1*T_r + C2*T_r**1.5 + C3*T_r*T_r)**(-1.0/(2*nu))
  end subroutine qhs_WCA_Mie


  subroutine phi_WCA_Mie(rho_r,lamr, phi)
    !> Eq 13 in [1]    
    type(hyperdual), intent(in) :: rho_r   ! reduced density (-)
    type(hyperdual), intent(in) :: lamr    ! repulsive exponent (-)
    type(hyperdual), intent(out) :: phi    ! u fraction (-)
    ! Locals
    type(hyperdual) :: x, e2x, tanhx

    ! Calculate tanh factor
    x = (C_PHI_WCA_MIE(1) + (C_PHI_WCA_MIE(2) + C_PHI_WCA_MIE(3)/lamr)*rho_r)*rho_r
    e2x = exp(2*x)
    tanhx = (e2x-1.0)/(e2x+1.0)
    phi = tanhx
  end subroutine phi_WCA_Mie

end module uv_theory



subroutine test_Fres_derivatives()
  use uv_theory
  use thermopack_constants, only: N_AVOGADRO
  implicit none
  ! Locals
  integer :: nc=1
  real :: n(1),n0(1),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(1) :: F_n,F_Tn,F_Vn
  real, dimension(1,1) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(1) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(1,1) :: Fp_nn
  real :: Fm,Fm_T,Fm_V,Fm_TT,Fm_VV,Fm_TV
  real, dimension(1) :: Fm_n,Fm_Tn,Fm_Vn
  real, dimension(1,1) :: Fm_nn
  class(uv_theory_param), pointer :: eos
  real, parameter :: sigma_param = 2.801e-10, eps_divk_param = 33.921
  allocate(uv_theory_param :: eos)
  call eos%allocate_and_init(nc,"uv-theory")
  eps = 1.0e-5
  ! eos%enable_A1 = .true.
  ! eos%enable_A2 = .true.
  ! eos%enable_A3 = .true.
  ! eos%enable_A4 = .true.
  ! eos%enable_hs = .true.
  ! eos%enable_cavity = .true.

  T = eps_divk_param
  n = 1.2
  n0 = n
  V = n(1)*N_AVOGADRO*sigma_param**3/0.5
  V = 1.0
  print *,V

  ! select type ( p_eos => eos )
  ! type is ( uv_theory_param )
     call calc_fres_uv(eos,1,T,V,n,F,F_T,F_V,F_n,F_TT,&
          F_VV,F_TV,F_Tn,F_Vn,F_nn)
     call calc_fres_uv(eos,1,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
          Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
     call calc_fres_uv(eos,1,T,V-V*eps,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
          Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  ! end select
  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - Fm)/(2*V*eps)
  print *,F_VV,(Fp_V - Fm_V)/(2*V*eps)
  print *,F_TV,(Fp_T - Fm_T)/(2*V*eps)
  print *,F_Vn,(Fp_n - Fm_n)/(2*V*eps)

  print *,"n1"
  n(1) = n0(1) + eps
  call calc_fres_uv(eos,1,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  n(1) = n0(1) - eps
  call calc_fres_uv(eos,1,T,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  print *,F_n(1),(Fp - Fm)/(2*eps)
  print *,F_Tn(1),(Fp_T - Fm_T)/(2*eps)
  print *,F_Vn(1),(Fp_V - Fm_V)/(2*eps)
  print *,F_nn(1,:),(Fp_n - Fm_n)/(2*eps)

  print *,"T"
  n = n0
  call calc_fres_uv(eos,1,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calc_fres_uv(eos,1,T-T*eps,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *,F_T,(Fp - Fm)/(2*T*eps)
  print *,F_TT,(Fp_T - Fm_T)/(2*T*eps)
  print *,F_TV,(Fp_V - Fm_V)/(2*T*eps)
  print *,F_Tn,(Fp_n - Fm_n)/(2*T*eps)
  stop
end subroutine test_Fres_derivatives
