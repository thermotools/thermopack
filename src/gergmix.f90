module gergmix
  !> The GERG-2008 Wide-Range Equation of State for Natural Gases and Other Mixtures:
  !! An Expansion of GERG-2004
  !! O. Kunz and W. Wagner
  !! Journal of Chemical & Engineering Data 2012 57 (11), 3032-3091
  !! DOI: 10.1021/je300655b
  use eos_parameters, only: single_eos
  use gerg, only: meos_gerg
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH
  use thermopack_var, only: Rgas, nce, complist, base_eos_param, &
       get_active_alt_eos
  use saturated_densities, only: sat_densities
  use gergmixdb, only: max_gerg_mix_reducing, gerg_mix_reducingdb, &
       max_gerg_mix_data, gerg_mix_datadb
  implicit none
  save
  private

  !> GERG-2008 multiparameter equations of state.
  type, extends(single_eos) :: meos_gergmix

    real, allocatable, dimension(:, :) :: inv_rho_pow
    real, allocatable, dimension(:, :) :: tc_prod_sqrt
    real, allocatable, dimension(:, :) :: beta_T
    real, allocatable, dimension(:, :) :: beta_v
    real, allocatable, dimension(:, :) :: gamma_T
    real, allocatable, dimension(:, :) :: gamma_v
    integer, allocatable, dimension(:, :) :: mix_data_index

  contains

    procedure, private :: allocate_param
    procedure, public :: alpha0_hd
    procedure, public :: alphaRes_hd
    procedure, public :: Zfac => Zfac_gergmix

    ! Assignment operator
    procedure, pass(This), public :: assign_meos => assign_meos_gergmix

    ! Privates
    procedure, private ::  calc_delta
    procedure, private ::  calc_tau
    procedure, private ::  calc_del_alpha_r
    procedure, private ::  pressure
    procedure, private ::  densitySolver

  end type meos_gergmix

  public :: meos_gergmix, constructor_gergmix
  public :: hd_fres_GERGMIX, hd_fid_GERGMIX

contains

  function constructor_GERGMIX(nc) result(gerg_mix)
    use stringmod, only: str_eq
    use iso_fortran_env, only: dp => REAL64
    use thermopack_var, only: get_active_thermo_model, thermo_model
    !character(len=*), intent(in) :: comp_name
    integer, intent(in) :: nc
    type(meos_gergmix) :: gerg_mix
    ! Locals
    type(thermo_model), pointer :: p_thermo
    integer :: i, j, k
    real :: rhoc_i, rhoc_j, Tc_i, Tc_j

    call gerg_mix%allocate_and_init(nc, "GERG2008")
    call gerg_mix%allocate_param(nc)

    gerg_mix%mix_data_index = -1
    do i=1,nc
      rhoc_i = gerg_mix%nist(i)%meos%rc
      Tc_i = gerg_mix%nist(i)%meos%tc
      do j=1,nc
        rhoc_j = gerg_mix%nist(j)%meos%rc
        Tc_j = gerg_mix%nist(j)%meos%tc
        gerg_mix%inv_rho_pow(i,j) = (1/rhoc_i**(1.0_dp/3.0_dp) + 1/rhoc_j**(1.0_dp/3.0_dp))**3
        gerg_mix%tc_prod_sqrt(i,i) = (Tc_i*Tc_j)**(0.5_dp)
        !
        do k=1,max_gerg_mix_reducing
          if ((str_eq(gerg_mix_reducingdb(k)%ident1, complist(i)) .and. &
               str_eq(gerg_mix_reducingdb(k)%ident2, complist(j))) .or. &
               (str_eq(gerg_mix_reducingdb(k)%ident1, complist(j)) .and. &
               str_eq(gerg_mix_reducingdb(k)%ident2, complist(i)))) then
            gerg_mix%beta_T(i,j) = gerg_mix_reducingdb(k)%beta_T
            gerg_mix%beta_v(i,j) = gerg_mix_reducingdb(k)%beta_v
            gerg_mix%gamma_T(i,j) = gerg_mix_reducingdb(k)%gamma_T
            gerg_mix%gamma_v(i,j) = gerg_mix_reducingdb(k)%gamma_v
            exit
          endif
        enddo
        do k=1,max_gerg_mix_data
          if ((str_eq(gerg_mix_datadb(k)%ident1, complist(i)) .and. &
               str_eq(gerg_mix_datadb(k)%ident2, complist(j))) .or. &
               (str_eq(gerg_mix_datadb(k)%ident1, complist(j)) .and. &
               str_eq(gerg_mix_datadb(k)%ident2, complist(i)))) then
            gerg_mix%mix_data_index(i,j) = k
            exit
          endif
        enddo
      enddo
    enddo

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = 8.314472_dp
    p_thermo%kRgas = 1000.0*p_thermo%Rgas !< J/kmol/K
  end function constructor_GERGMIX

  subroutine allocate_param(this, nc)
    class(meos_gergmix), intent(inout) :: this
    integer, intent(in) :: nc
    !
    if (allocated(this%inv_rho_pow)) deallocate(this%inv_rho_pow)
    if (allocated(this%tc_prod_sqrt)) deallocate(this%tc_prod_sqrt)
    if (allocated(this%beta_T)) deallocate(this%beta_T)
    if (allocated(this%beta_v)) deallocate(this%beta_v)
    if (allocated(this%gamma_T)) deallocate(this%gamma_T)
    if (allocated(this%gamma_v)) deallocate(this%gamma_v)
    if (allocated(this%mix_data_index)) deallocate(this%mix_data_index)
    !
    allocate(this%inv_rho_pow(nc,nc), this%tc_prod_sqrt(nc,nc), &
         this%beta_T(nc,nc), this%beta_v(nc,nc), &
         this%gamma_T(nc,nc), this%gamma_v(nc,nc), &
         this%mix_data_index(nc,nc))
  end subroutine allocate_param

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  function alpha0_hd(this, x, delta, tau) result(alp0)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: delta, tau, x(nce)
    type(hyperdual) :: alp0 !< alp0
    ! Internals
    integer :: i
    alp0 = 0.0_dp
    do i=1,nce
      alp0 = alp0 + x(i)*(this%nist(i)%meos%alphaRes_hd_taudelta(delta,tau) + log(x(i)))
    enddo
  end function alpha0_hd

  function alphaRes_hd(this, x, delta, tau) result(alpr)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: delta, tau, x(nce)
    type(hyperdual) :: alpr !< alpr
    ! Internal
    integer :: i

    alpr = 0.0_dp
    do i=1,nce
      alpr = alpr + x(i)*this%nist(i)%meos%alphaRes_hd_taudelta(delta,tau)
    enddo
    alpr = alpr + this%calc_del_alpha_r(x, tau, delta)
  end function alphaRes_hd

  subroutine pressure(this, rho, x, T_spec, p, p_rho)
    use hyperdual_mod
    class(meos_gergmix) :: this
    real, intent(in) :: rho, T_spec, x(nce)
    real, intent(out) :: p
    real, optional, intent(out) :: p_rho
    ! Internal
    type(hyperdual) :: T, v, n(nce), f_res
    n = x
    T = T_spec
    v%f0 = 1.0/rho
    v%f1 = 1.0_dp
    v%f2 = 1.0_dp
    f_res = hd_fres_GERGMIX(this,nce,T,V,n)
    p = -Rgas*T_spec*(f_res%f1 + 1.0_dp)
    if (present(p_rho)) p_rho = Rgas*T_spec*f_res%f12*rho**2
  end subroutine pressure

  subroutine densitySolver(this, x, T_spec, p_spec, phase_spec, rho, phase_found)
    use thermopack_constants
    use numconstants, only: machine_prec
    use cubic, only: cbCalcZfac
    use cubic_eos, only: cb_eos
    class(meos_gergmix) :: this !< The calling class.
    real, intent(in) :: T_spec, p_spec, x(nce) !< Temperature (K) and pressure (Pa)
    integer, intent(in) :: phase_spec !< Phase flag.
    real, intent(out) :: rho !< Density (mol/m^3)
    integer, optional, intent(out) :: phase_found
    ! Internals
    integer :: iter, maxiter=200
    real :: rhoOld, pOld, dpdrhoOld
    real :: p, dpdrho
    real :: pMin, dpdrhoMin
    integer :: curvatureSign
    logical :: converged
    integer :: currentPhase, nPhaseSwitches
    ! Relative accuracy in density solver.
    real, parameter :: releps_p = machine_prec*1e8
    real, parameter :: releps_rho = machine_prec*1e6
    class(base_eos_param), pointer :: p_alt_eos

    pMin = 0 ! Minimum allowable pressure during iteration.
    dpdrhoMin = 0 ! Minimum allowable pressure derivative during iteration.
    currentPhase = phase_spec ! May change during iteration
    nPhaseSwitches = 0 ! No phase switches so far
    iter = 0
    call initializeSearch() ! Set initial rho, p, dpdrho

    ! Newton iteration loop
    do while (.true.)
      rhoOld = rho
      pOld = p
      dpdrhoOld = dpdrho

      rho = rho - (p-p_spec)/dpdrho
      if (rho<0) then
        call switchPhase()
      else
        call this%pressure(rho, x, T_spec, p, dpdrho)
        if ( p<pMin .or. dpdrho < dpdrhoMin .or. &
             curvatureSign*(rho-rhoOld)*(dpdrho-dpdrhoOld) < -2e-10*abs(rho*dpdrho) ) then
          call switchPhase()
          continue
        end if
      end if
      iter = iter+1
      converged = (abs(p_spec-pOld)<(releps_p*pOld+1e-6) .and. abs(rho-rhoOld)<releps_rho*rhoOld)
      if ( converged ) then
        exit
      else if ( iter == maxiter ) then
        if (.not. continueOnError) then
          print *, "iter ", iter
          print *, "T_spec, P_spec, phase_spec", T_spec, P_spec, phase_spec
          print *, "rho, rhoOld ", rho, rhoOld
          print *, "p, pOld ", p, pOld
          print *, "dpdrho, dpdrhoOld ", dpdrho, dpdrhoOld
          print *, "currentPhase", currentPhase
          print *, "curvature", (rho-rhoOld)*(dpdrho-dpdrhoOld)
          call stoperror("multiparameter_eos::densitySolver: iter == max_iter.")
        end if
      end if
    end do

    if ( present(phase_found) ) then
      phase_found = currentPhase
    end if

  contains

    !> Call this routine if stability fails. It switches the phases, and
    !> initializes a new search for a root in the other phase; iter is NOT reset
    !> to zero.
    subroutine switchPhase()
      if ( currentPhase == VAPPH ) then
        currentPhase = LIQPH
      else
        currentPhase = VAPPH
      end if
      nPhaseSwitches = nPhaseSwitches + 1
      call initializeSearch()
    end subroutine switchPhase

    !> This routine computes initial rho and dpdrho, as well as setting parameters
    !> for the stability test (pMin, dpdrhoMin, curvatureSign).
    subroutine initializeSearch ()
      real :: z
      converged = .false.

      if( currentPhase == VAPPH) then
        curvatureSign = -1
        rho = p_spec/(T_spec*Rgas)
        call this%pressure(rho, x, T_spec, p, p_rho=dpdrho)
      else
        curvatureSign = 1
        p_alt_eos => get_active_alt_eos()
        select type ( p_eos => p_alt_eos )
        type is ( cb_eos ) ! cubic equations of state
          call cbCalcZfac(nce,p_eos,T_spec,p_spec,x,currentPhase,Z,gflag_opt=1)
          rho = p_spec/(z*Rgas*t_spec)
        class default ! Saft eos
          call stoperror("Error in initializeSearch")
        end select
        call this%pressure(rho, x, T_spec, p, p_rho=dpdrho)
        do while (p<0 .or. dpdrho<0) ! Should only kick in at extremely low temperatures.
          rho = 2*rho
          curvatureSign = 0
          call this%pressure(rho, x, T_spec, p, p_rho=dpdrho)
        end do
      end if

      ! Have we switched phases two times? Then we're back at the original phase.
      ! This should only happen at extremely low temperatures where the eos is
      ! unphysical. In this case, turn off all robustness measures, and just
      ! try to find a root.
      if ( nPhaseSwitches == 2 ) then
        curvatureSign = 0
        pMin = -1e100
        dpdrhoMin = -1e100
      end if

    end subroutine initializeSearch

  end subroutine densitySolver

  subroutine assign_meos_gergmix(this,other)
    class(meos_gergmix), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_gergmix)
      call this%single_eos%assign_eos(other)

      !
    class default
      call stoperror("assign_meos_gergmix: Should not be here....")
    end select
  end subroutine assign_meos_gergmix

  function calc_delta(this, x, rho) result(delta)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: x(nce), rho
    type(hyperdual) :: delta
    ! Internals
    integer :: i, j
    delta = 0.0_dp
    do i=1,nce
      do j=1,nce
        delta = delta + x(i)*x(j)*this%beta_v(i,j)*this%gamma_v(i,j)*&
             ((x(i)+x(j))/(this%beta_v(i,j)**2*x(i)+x(j)))*this%inv_rho_pow(i,j)/8.0_dp
      enddo
    enddo
    delta = delta*rho
  end function calc_delta

  function calc_tau(this, x, T) result(tau)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: x(nce), T
    type(hyperdual) :: tau
    ! Internals
    integer :: i, j
    tau = 0.0_dp
    do i=1,nce
      do j=1,nce
        tau = tau + x(i)*x(j)*this%beta_T(i,j)*this%gamma_T(i,j)*&
             ((x(i)+x(j))/(this%beta_T(i,j)**2*x(i)+x(j)))*this%tc_prod_sqrt(i,j)
      enddo
    enddo
    tau = tau/T
  end function calc_tau

  function calc_del_alpha_r(this, x, tau, delta) result(del_alpha_r)
    use hyperdual_mod
    class(meos_gergmix) :: this
    type(hyperdual), intent(in) :: x(nce), tau, delta
    type(hyperdual) :: del_alpha_r
    ! Internals
    integer :: i, j, k, idb
    type(hyperdual) :: del_alpha_r_ij
    del_alpha_r = 0.0_dp

    do i=1,nce
      do j=1,nce
        del_alpha_r_ij = 0.0_dp
        if (this%mix_data_index(i,j) > 0) then
          idb = this%mix_data_index(i,j)
          do k=1,gerg_mix_datadb(idb)%num_mix - gerg_mix_datadb(idb)%num_exp
            del_alpha_r_ij = del_alpha_r_ij + gerg_mix_datadb(idb)%n_mix(k)*delta**gerg_mix_datadb(idb)%d_mix(k)*&
                 tau**gerg_mix_datadb(idb)%t_mix(k)
          enddo
          do k=1+gerg_mix_datadb(idb)%num_exp,gerg_mix_datadb(idb)%num_mix
            del_alpha_r_ij = del_alpha_r_ij + gerg_mix_datadb(idb)%n_mix(k)*delta**gerg_mix_datadb(idb)%d_mix(k)*&
                 tau**gerg_mix_datadb(idb)%t_mix(k)* &
                 exp(-gerg_mix_datadb(idb)%eta_mix(k)*(delta-gerg_mix_datadb(idb)%epsilon_mix(k))**2 &
                 - gerg_mix_datadb(idb)%beta_mix(k)*(delta-gerg_mix_datadb(idb)%gamma_mix(k)))
          enddo
          del_alpha_r = del_alpha_r + x(i)*x(j)*gerg_mix_datadb(idb)%Fij*del_alpha_r_ij
        endif
      enddo
    enddo
  end function calc_del_alpha_r

  subroutine Zfac_gergmix(eos,T,P,Z,phase,Zfac,dZdt,dZdp,dZdz)
    implicit none
    class(meos_gergmix), intent(inout) :: eos
    real, dimension(nce), intent(in) :: Z
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    real, intent(out) :: Zfac
    real, optional, intent(out) :: dZdt, dZdp
    real, optional, dimension(nce), intent(out) :: dZdz
    ! Locals
    real :: rho
    integer :: phase_found
    !
    call eos%densitySolver(z, T, P, phase, rho, phase_found)
    zfac = P/(sum(z)*Rgas*T*rho)
  end subroutine Zfac_Gergmix

  function hd_fid_GERGMIX(p_eos,nc,T,V,n) result(f)
    use hyperdual_mod
    use thermopack_var, only: base_eos_param
    implicit none
    class(base_eos_param), intent(inout) :: p_eos
    integer, intent(in) :: nc
    type(hyperdual), intent(in) :: T, V, n(nc)
    type(hyperdual) :: f
    ! Locals
    type(hyperdual) :: tau, delta, x(nc)
    select type ( eos => p_eos )
    class is(meos_gergmix)
      x = n/sum(n)
      tau = eos%calc_tau(x, T)
      delta = eos%calc_delta(x, 1.0_dp/V)
      f = sum(n)*eos%alpha0_hd(x, delta, tau)
    class default
      call stoperror("Error in hd_fid_GERGMIX")
    end select
  end function hd_fid_GERGMIX

  function hd_fres_GERGMIX(p_eos,nc,T,V,n) result(f)
    use hyperdual_mod
    use thermopack_var, only: base_eos_param
    implicit none
    class(base_eos_param), intent(inout) :: p_eos
    integer, intent(in) :: nc
    type(hyperdual), intent(in) :: T, V, n(nc)
    type(hyperdual) :: f
    ! Locals
    type(hyperdual) :: tau, delta, x(nc)
    select type ( eos => p_eos )
    class is(meos_gergmix)
      x = n/sum(n)
      tau = eos%calc_tau(x, T)
      delta = eos%calc_delta(x, 1.0_dp/V)
      f = sum(n)*eos%alphaRes_hd(x, delta, tau)
    class default
      call stoperror("Error in hd_fres_GERGMIX")
    end select

  end function hd_fres_GERGMIX

end module gergmix
