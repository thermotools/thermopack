module meosmix
  !> Mixture of multiparamater EoSs
  !!
  !! \author Morten Hammer, 2023
  use gergmix, only: meos_gergmix
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH, FAKEPH, Rgas_default
  use thermopack_var, only: Rgas, nce, complist, base_eos_param, &
       get_active_alt_eos, get_active_eos
  use meosmixdb, only: max_meos_mix_reducing, meos_mix_reducingdb, &
       max_meos_mix_data, meos_mix_datadb
  use iso_fortran_env, only: dp => REAL64
  use numconstants, only: machine_prec
  implicit none
  save
  private

  !> Multiparameter equations of state.
  type, extends(meos_gergmix) :: meos_mix

  contains
    procedure, public  :: rgas_mix
    procedure, public  :: calc_del_alpha_r => calc_del_alpha_r_meos_mix
    procedure, private :: pressure_extrema
    procedure, public  :: fake_density => fake_density_meosmix
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
    real :: rhor_i, rhor_j, Tr_i, Tr_j

    call mmix%allocate_and_init(nc, "MEOS")
    call mmix%allocate_param(nc)

    mmix%mix_data_index = -1
    do i=1,nc
      rhor_i = mmix%nist(i)%meos%rhor
      Tr_i = mmix%nist(i)%meos%tr
      do j=i,nc
        rhor_j = mmix%nist(j)%meos%rhor
        Tr_j = mmix%nist(j)%meos%tr
        mmix%inv_rho_pow(i,j) = (1.0_dp/rhor_i**(1.0_dp/3.0_dp) + 1.0_dp/rhor_j**(1.0_dp/3.0_dp))**3
        mmix%tc_prod_sqrt(i,j) = (Tr_i*Tr_j)**(0.5_dp)
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
    integer :: i, j, k, idb, n_poly, n_exp, n_gauss, sgn
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
            if (meos_mix_datadb(idb)%eta_mix(k) < 0.0) then
              sgn = -1
            else
              sgn = 1
            endif
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)* &
                 exp(-sgn*meos_mix_datadb(idb)%eta_mix(k)*(delta-sgn*meos_mix_datadb(idb)%epsilon_mix(k))**2 &
                 - meos_mix_datadb(idb)%beta_mix(k)*(tau-meos_mix_datadb(idb)%gamma_mix(k))**2)
          enddo
          del_alpha_r = del_alpha_r + x(i)*x(j)*meos_mix_datadb(idb)%Fij*del_alpha_r_ij
        endif
      enddo
    enddo
  end function calc_del_alpha_r_meos_mix

  !> Find pressure extrema of p(rho)
  function pressure_extrema(this,T,z,phase) result(rho_extr)
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

  end function pressure_extrema

  !> Calculate extremas in pressure and determine if a fake root is needed
  subroutine fake_density_meosmix(this, x, T_spec, p_spec, phase_spec, rho, ierr, phase_found)
    use utilities, only: fallback_density
    class(meos_mix) :: this !< The calling class.
    real, intent(in) :: T_spec, p_spec, x(nce) !< Temperature (K) and pressure (Pa)
    integer, intent(in) :: phase_spec !< Phase flag
   real, intent(out) :: rho !< Density (mol/m^3)
    integer, intent(out) :: ierr
    integer, optional, intent(out) :: phase_found !< Phase flag of detected phase
    ! Internals
    real :: rho_extr_liq, rho_extr_vap,P_extr_liq,d2P_drho2_extr_liq,P_extr_vap
    real :: rho_bracket, rho_extr
    integer :: ophase
    ierr = 0
    ! No stable root. Determine fake root.
    rho_extr_liq = this%pressure_extrema(T_spec,x,phase=LIQPH)
    rho_extr_vap = this%pressure_extrema(T_spec,x,phase=VAPPH)
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
        if ((P_extr_liq < p_spec) .and. (P_extr_vap > p_spec)) then
          ! Both phases should exist
          ophase = phase_spec
          if (ophase == VAPPH) then
            rho_extr = rho_extr_vap
          else
            rho_extr = rho_extr_liq
          endif
        else if (P_extr_liq < p_spec) then
          ! A liqiud solution exist
          ophase = LIQPH
          rho_extr = rho_extr_liq
        else if (P_extr_vap > p_spec) then
          ophase = VAPPH
          rho_extr = rho_extr_vap
        endif
        call find_bracketed_density_root(T_spec,P_spec,x,ophase,rho_extr,rho_bracket,ierr)
        if (present(phase_found)) phase_found = ophase
        if (ierr == 0) rho = rho_bracket
      endif
    endif
  end subroutine fake_density_meosmix

  !> Find a bracketed root of P(rho)-P.
  !> \author Morten Hammer, 2023-04
  subroutine find_bracketed_density_root(T,P,z,phase,rho_extr,rho,ierr)
    use nonlinear_solvers, only: nonlinear_solver,bracketing_solver,&
                                 NS_RIDDERS
    implicit none
    ! Input:
    real,intent(in)         :: T,P,z(nce),rho_extr
    integer, intent(in)     :: phase
    ! Output:
    real,intent(out)        :: rho
    integer, intent(out)    :: ierr
    ! Internal:
    integer, parameter      :: max_iter = 1000
    real, parameter         :: rho_rel_tol = machine_prec*100.0
    type(nonlinear_solver)  :: solver
    real, dimension(nce+2)  :: param
    real                    :: rho_min, rho_max, fun_ext, drho
    integer                 :: i

    ! Close to extremum use bracketing solver
    param(1) = p
    param(2) = T
    param(3:nce+2) = z(1:nce)

    solver%rel_tol = rho_rel_tol
    solver%max_it = max_iter
    solver%isolver = NS_RIDDERS

    ! Locate density range
    fun_ext = pressure_fun(rho_extr,param)
    if (phase == VAPPH) then
      drho = -0.1*rho_extr
      rho_max = rho_extr
    else
      drho = 0.1*rho_extr
      rho_min = rho_extr
    endif
    rho = rho_extr
    do i=1,5
      rho = rho + drho
      if (pressure_fun(rho,param)*fun_ext < 0) exit
    enddo
    if (phase == VAPPH) then
      rho_min = rho
    else
      rho_max = rho
    endif

    call bracketing_solver(rho_min,rho_max,pressure_fun,rho,solver,param)
    ierr = solver%exitflag

  end subroutine find_bracketed_density_root

  !> Calculate error in pressure
  !> \author Morten Hammer, 2023-04
  function pressure_fun(var,param) result(f)
    implicit none
    real, intent(in)                   :: var !< Density [mol/m3]
    real, dimension(nce+2), intent(in) :: param !< Parameter vector
    real                               :: f !< Function value [Pa]
    ! Locals
    real                               :: p, t, p_spec, rho
    real, dimension(nce)               :: Z
    class(base_eos_param), pointer     :: base_eos
    rho = var
    p_spec = param(1)
    t = param(2)
    Z(1:nce) = param(3:nce+2)
    base_eos => get_active_eos()
    select type ( eos => base_eos )
    class is(meos_gergmix)
      call eos%pressure(rho, z, T, p)
    class default
      call stoperror("meosmix pressure_fun")
    end select
    f = (p_spec - p)*1.0e-6
  end function pressure_fun

end module meosmix
