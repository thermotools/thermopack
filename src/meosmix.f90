module meosmix
  !> Mixture of multiparamater EoSs
  !!
  !! \author Morten Hammer, 2023
  use gergmix, only: meos_gergmix
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH, FAKEPH, Rgas_default
  use thermopack_var, only: Rgas, nce, complist, base_eos_param, &
       get_active_alt_eos
  use meosmixdb, only: max_meos_mix_reducing, meos_mix_reducingdb, &
       max_meos_mix_data, meos_mix_datadb
  use iso_fortran_env, only: dp => REAL64
  implicit none
  save
  private

  !> Multiparameter equations of state.
  type, extends(meos_gergmix) :: meos_mix

  contains
    procedure, public :: rgas_mix
    procedure, public :: calc_del_alpha_r => calc_del_alpha_r_meos_mix
    procedure, private :: density_extrema
    procedure, public :: fake_density => fake_density_meosmix

  end type meos_mix

  public :: meos_mix, constructor_meos

contains

  !> Constructor for mixture of multiparameter equations of state.
  function constructor_meos(nc) result(mmix)
    use stringmod, only: str_eq
    use thermopack_var, only: get_active_thermo_model, thermo_model
    integer, intent(in) :: nc
    type(meos_mix) :: mmix
    ! Locals
    type(thermo_model), pointer :: p_thermo
    integer :: i, j, k
    real :: rhoc_i, rhoc_j, Tc_i, Tc_j

    call mmix%allocate_and_init(nc, "MEOS")
    call mmix%allocate_param(nc)

    mmix%mix_data_index = -1
    do i=1,nc
      rhoc_i = mmix%nist(i)%meos%rc
      Tc_i = mmix%nist(i)%meos%tc
      do j=i,nc
        rhoc_j = mmix%nist(j)%meos%rc
        Tc_j = mmix%nist(j)%meos%tc
        mmix%inv_rho_pow(i,j) = (1.0_dp/rhoc_i**(1.0_dp/3.0_dp) + 1.0_dp/rhoc_j**(1.0_dp/3.0_dp))**3
        mmix%tc_prod_sqrt(i,j) = (Tc_i*Tc_j)**(0.5_dp)
        !
        if (j > i) then
          do k=1,max_meos_mix_reducing
            if (str_eq(meos_mix_reducingdb(k)%ident1, complist(i)) .and. &
                 str_eq(meos_mix_reducingdb(k)%ident2, complist(j))) then
              mmix%beta_T(i,j) = meos_mix_reducingdb(k)%beta_T
              mmix%beta_v(i,j) = meos_mix_reducingdb(k)%beta_v
              mmix%gamma_T(i,j) = meos_mix_reducingdb(k)%gamma_T
              mmix%gamma_v(i,j) = meos_mix_reducingdb(k)%gamma_v
              exit
            else if (str_eq(meos_mix_reducingdb(k)%ident1, complist(j)) .and. &
                 str_eq(meos_mix_reducingdb(k)%ident2, complist(i))) then
              mmix%beta_T(i,j) = 1.0/meos_mix_reducingdb(k)%beta_T
              mmix%beta_v(i,j) = 1.0/meos_mix_reducingdb(k)%beta_v
              mmix%gamma_T(i,j) = meos_mix_reducingdb(k)%gamma_T
              mmix%gamma_v(i,j) = meos_mix_reducingdb(k)%gamma_v
              exit
            endif
          enddo
          do k=1,max_meos_mix_data
            if ((str_eq(meos_mix_datadb(k)%ident1, complist(i)) .and. &
                 str_eq(meos_mix_datadb(k)%ident2, complist(j))) .or. &
                 (str_eq(meos_mix_datadb(k)%ident1, complist(j)) .and. &
                 str_eq(meos_mix_datadb(k)%ident2, complist(i)))) then
              mmix%mix_data_index(i,j) = k
              exit
            endif
          enddo
        endif
      enddo
    enddo

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    if (nc == 1) then
      p_thermo%Rgas = mmix%nist(1)%meos%Rgas_meos
    else
      ! Instead of mixing Rgas, use default Rgas
      p_thermo%Rgas = Rgas_default
    endif
    p_thermo%kRgas = 1000.0*p_thermo%Rgas !< J/kmol/K
  end function constructor_meos

  ! Component fraction averaged Rgas
  function rgas_mix(this, x) result(rmix)
    use hyperdual_mod
    class(meos_mix) :: this
    type(hyperdual), intent(in) :: x(nce)
    type(hyperdual) :: rmix
    ! Internals
    integer :: i
    rmix = 0.0_dp
    do i=1,nce
      rmix = rmix + x(i)*this%nist(i)%meos%Rgas_meos
    enddo
  end function rgas_mix

  ! Deparure function for mixing
  function calc_del_alpha_r_meos_mix(this, x, tau, delta) result(del_alpha_r)
    use hyperdual_mod
    class(meos_mix) :: this
    type(hyperdual), intent(in) :: x(nce), tau, delta
    type(hyperdual) :: del_alpha_r
    ! Internals
    integer :: i, j, k, idb, n_poly, n_exp, n_gauss
    type(hyperdual) :: del_alpha_r_ij
    del_alpha_r = 0.0_dp

    do i=1,nce-1
      do j=i+1,nce
        del_alpha_r_ij = 0.0_dp
        if (this%mix_data_index(i,j) > 0) then
          idb = this%mix_data_index(i,j)
          n_gauss = meos_mix_datadb(idb)%num_mix
          n_exp = n_gauss - meos_mix_datadb(idb)%num_gauss
          n_poly = n_exp - meos_mix_datadb(idb)%num_exp
          do k=1,n_poly
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)
          enddo
          do k=n_poly+1,n_exp
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)* &
                 exp(-delta**meos_mix_datadb(idb)%l_mix(k))
          enddo
          do k=n_exp+1,n_gauss
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)* &
                 exp(-meos_mix_datadb(idb)%eta_mix(k)*(delta-meos_mix_datadb(idb)%epsilon_mix(k))**2 &
                 - meos_mix_datadb(idb)%beta_mix(k)*(delta-meos_mix_datadb(idb)%gamma_mix(k)))
          enddo
          del_alpha_r = del_alpha_r + x(i)*x(j)*meos_mix_datadb(idb)%Fij*del_alpha_r_ij
        endif
      enddo
    enddo
  end function calc_del_alpha_r_meos_mix

  !> Find density extrema
  function density_extrema(this,T,z,phase) result(rho_extr)
    use numconstants, only: machine_prec
    use cubic_eos, only: get_b_linear_mix
    implicit none
    ! Input:
    class(meos_mix), intent(inout)  :: this
    real, intent(in)                :: z(nce)        !< Overall molar compozition [-]
    real, intent(in)                :: T            !< Temperature [K]
    integer, intent(in)             :: phase        !< Desired phase
    ! Output:
    real                            :: rho_extr !< Density of P_EoS extremum (Negative if not found) [mol/m3]
    ! Locals
    real                            :: drho,dpdrho,d2pdrho2,rhomin,&
                                       rhomax,s,p
    integer                         :: n_iter
    integer, parameter              :: max_iter_extr = 200
    real, parameter                 :: rho_extrem_rel_tol = machine_prec*1000.0
    real                            :: gradient_descent_drho,&
                                       newton_max_drho

    rhomin = 1.0e-10
    rhomax = 1.0/get_b_linear_mix(z)
    gradient_descent_drho = (rhomax-rhomin)/10.0
    newton_max_drho = (rhomax-rhomin)/10.0

    select case(phase)
      case(LIQPH)
        rho_extr = rhomax
        s = 1.0 !Looking for minimum
      case(VAPPH)
        rho_extr = rhomin
        s = -1.0 !Looking for maximum
    end select

    n_iter = 0
    do  ! Find first extremum
      n_iter = n_iter+1
      call this%pressure(rho_extr, z, T, p, dpdrho, d2pdrho2)
      if ( (phase==VAPPH .and. d2pdrho2 < 0.0)  .or.  &
           (phase==LIQPH .and. d2pdrho2 > 0.0)        &
         ) then
        ! Newton optimization
        drho = - dpdrho/d2pdrho2
        ! Limit the step
        if (abs(drho) > newton_max_drho) then
          drho = sign(newton_max_drho,drho)
        endif
      else
        ! Gradient descent, constant step.
        drho = -s*sign(1.0,dpdrho)*gradient_descent_drho
      endif

      ! Update rho
      rho_extr = rho_extr + drho

      ! Check convergence of extremum search
      if (abs(drho/rho_extr)<rho_extrem_rel_tol) then
        ! Converged
        return
      elseif ((n_iter == max_iter_extr) .or.       &
           (phase==LIQPH .and. rho_extr < rhomin) .or.  &
           (phase==VAPPH .and. rho_extr > rhomax)       &
           ) then
        ! Found no extremum
        rho_extr = -1.0
        return
      endif
    end do

  end function density_extrema

  !> Density solver. Specified T,P and composition,
  subroutine fake_density_meosmix(this, x, T_spec, p_spec, rho, ierr, phase_found)
    use utilities, only: fallback_density
    class(meos_mix) :: this !< The calling class.
    real, intent(in) :: T_spec, p_spec, x(nce) !< Temperature (K) and pressure (Pa)
    real, intent(out) :: rho !< Density (mol/m^3)
    !real, intent(out) :: rho_extr_liq, rho_extr_vap !< Density extrema (mol/m^3)
    integer, intent(out) :: ierr
    integer, optional, intent(out) :: phase_found
    ! Internals
    real :: rho_extr_liq, rho_extr_vap,P_extr_liq,d2P_drho2_extr_liq,P_extr_vap
    ierr = 0
    ! No stable root. Determine fake root.
    rho_extr_liq = this%density_extrema(T_spec,x,phase=LIQPH)
    rho_extr_vap = this%density_extrema(T_spec,x,phase=VAPPH)
    if (rho_extr_liq < 0.0 .or. rho_extr_vap < 0.0) then
      ierr = 3
    else
      call this%pressure(rho_extr_liq, x, T_spec, P_extr_liq, p_rhorho=d2P_drho2_extr_liq)
      call this%pressure(rho_extr_vap, x, T_spec, P_extr_vap)
      ! Test pressures
      if (P_extr_liq > p_spec .and. P_extr_vap < p_spec) then
        ! Calculate fallback density
        rho = fallback_density(p_spec,rho_extr_liq,rho_extr_vap,P_extr_liq,d2P_drho2_extr_liq)
        if (present(phase_found)) phase_found = FAKEPH
      else
        if (P_extr_liq < p_spec) then
          ierr = 4 ! A liqiud solution exist
        else if (P_extr_vap > p_spec) then
          ierr = -4 ! A vapour solution exist
        else
          ierr = 5 ! Both phases should exist
        endif
      endif
    endif
  end subroutine fake_density_meosmix

end module meosmix
