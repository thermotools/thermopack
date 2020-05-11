!-----------------------------------------------------------------------------
!> Solve for trend density given pressure and temperature
!>
!>
!>
!-----------------------------------------------------------------------------
module trend_solver
  !
  !
  use parameters, only: nc,verbose,LIQPH,VAPPH,SINGLEPH,FAKEPH,MINGIBBSPH
  use numconstants, only: machine_prec
  implicit none
  integer, parameter :: TREND_LIQ=1,TREND_VAP=2
  ! Should be true for water-containing systems; the subroutine trend_density
  ! below is poor for these systems.
  logical            :: useRUBdensitySolver = .false.
  private

  ! TREND interface
  include 'trend_interface.f95'

  public :: trend_density
  public :: useRUBdensitySolver
  public :: trend_phase_is_fake

contains

  !-----------------------------------------------------------------------------
  !> Solve for density given pressure, temperature and composition.
  !> Inteface RUB solver and local solver
  !>
  !> \author MH, 2015-10
  !-----------------------------------------------------------------------------
  subroutine trend_density(T,P,Z,phase_in,rho,phase_found_out,metaextr)
    use eosTV, only: pressure
    implicit none
    ! Input:
    real, intent(in)                :: Z(nc)          !< Overall molar compozition [-]
    real, intent(in)                :: T              !< Temperature [K]
    real, intent(in)                :: P              !< Pressure [Pa]
    integer, intent(in)             :: phase_in       !< Desired phase
    logical, optional, intent(in)   :: metaextr       !< Use extremum if no root
    ! Output:
    real, intent(out)               :: rho            !< Density [mol/m3]
    integer, optional, intent(out)  :: phase_found_out!< Phase actually found
    ! Internal:
    real :: v
    real :: rholiq, rhovap, lnfugliq(nc), lnfugvap(nc)
    real, external :: trend_specvol ! RUB solver
    logical :: solveForMetaextr
    integer :: phase

    if (present(metaextr)) then
      solveForMetaextr = metaextr
    else
      solveForMetaextr = .false.
    endif

    phase = phase_in
    if (phase == SINGLEPH) then
      phase = LIQPH
    endif

    ! Check phase flag
    if (phase_in /= LIQPH .and. phase_in /= VAPPH .and. phase_in /= MINGIBBSPH .and. phase_in /= SINGLEPH) then
      write(*,*) "trend_density must be called with LIQPH, VAPPH or MINGIBBSPH."
      write(*,*) "phase=",phase_in
      stop
    end if

    if (useRUBdensitySolver .and. .not. solveForMetaextr) then
      v = trend_specvol(T,P,Z,phase)
      if (v <= 0.0) then
        call trend_density_local(T,P,Z,phase,rho,phase_found_out)
      else
        rho = 1.0/v
        if (abs(pressure(t,v,z) - p) > 1.0) then
          if (present(phase_found_out)) then
            phase_found_out = FAKEPH
          endif
          return
        else
          call trend_density_refine(T,P,z,rho)
          if (present(phase_found_out)) then
            phase_found_out = phase
          endif
        endif
      endif
    else if (phase_in == MINGIBBSPH) then
      if (nc /= 1) call stoperror("trend_density::MINGIBBSPH flag only implemented for nc=1")
      call trend_density_local(T,P,Z,TREND_LIQ,rho,phase_found_out, metaextr=.true.)
      rholiq = rho
      call trend_density_local(T,P,Z,TREND_VAP,rho,phase_found_out, metaextr=.true.)
      rhovap = rho
      if (rholiq /= rhovap) then
        call trend_thermo_dens(T,rholiq,Z,lnfugliq)
        call trend_thermo_dens(T,rhovap,Z,lnfugvap)
        if (lnfugliq(1) < lnfugvap(1)) then
          rho = rholiq
          if (present(phase_found_out)) phase_found_out = LIQPH
        else
          rho = rhovap
          if (present(phase_found_out)) phase_found_out = VAPPH
        end if
      end if
    else
      call trend_density_local(T,P,Z,phase,rho,phase_found_out, metaextr)
    endif
  end subroutine trend_density

  !-----------------------------------------------------------------------------
  !> Solve for density given pressure, temperature and composition.
  !> Will preferably return the solution of the kind given by "phase"
  !> (vapor or liquid). If such a solution cannot be found, the solution
  !> of the opposite kind will be returned.
  !> If no valid solution exists at all, a fallback liquid density is returned.
  !>
  !> \author EA, 2013-08, MAG, 2013-08
  !-----------------------------------------------------------------------------
  subroutine trend_density_local(T,P,Z,phase_in,rho,phase_found_out,metaextr)
    implicit none
    ! Input:
    real, intent(in)                :: Z(nc)          !< Overall molar compozition [-]
    real, intent(in)                :: T              !< Temperature [K]
    real, intent(in)                :: P              !< Pressure [Pa]
    integer, intent(in)             :: phase_in       !< Desired phase
    logical, optional, intent(in)   :: metaextr       !< Use extremum if no root
    ! Output:
    real, intent(out)               :: rho            !< Density [mol/m3]
    integer, optional, intent(out)  :: phase_found_out!< Phase actually found
    ! Internal:
    integer                         :: phase_alt
    real                            :: rho_prim
    real                            :: rho_alt
    real                            :: rho_extr_prim
    real                            :: rho_extr_alt
    real                            :: rho_extr_liq
    real                            :: rho_div
    integer                         :: phase_found_prim
    integer                         :: phase_found_alt
    integer                         :: phase

    ! Check phase flag
    if (phase_in /= LIQPH .and. phase_in /= VAPPH) then
      write(*,*) "trend_density must be called with LIQPH or VAPPH."
      stop
    end if

    ! Convert from Thermopack phase flag to TREND phase flag
    phase = phaseflag_thermopack_to_trend(phase_in)

    ! Attempt primary desired solution:
    call trend_density_robust(T,P,Z,phase,rho_prim,phase_found_prim,rho_extr_prim)
    ! Found it?
    if (rho_prim >= 0.0) then
      rho = rho_prim
      if (present(phase_found_out)) phase_found_out = phaseflag_trend_to_thermopack(phase_found_prim)
      return
    end if

    ! Did not find solution of desired phase kind.
    ! Use extremum, or search for other kind of phase.
    if (present(metaextr)) then
      if (metaextr) then
        ! Use extremum as a "solution"
        rho = rho_extr_prim
        if (present(phase_found_out)) phase_found_out = phase_in
        return
      endif
    endif

    ! Search for the alternative phase.
    select case(phase)
      case(TREND_LIQ)
        phase_alt = TREND_VAP
      case(TREND_VAP)
        phase_alt = TREND_LIQ
    end select

    ! Attempt alternative solution:
    call trend_density_robust(T,P,Z,phase_alt, rho_alt,phase_found_alt,rho_extr_alt)
    ! Found it?
    if (rho_alt >= 0.0) then
      rho = rho_alt
      if (present(phase_found_out)) phase_found_out = phaseflag_trend_to_thermopack(phase_found_alt)
      return
    end if

    ! No density prediction. Use fallback.
    if (rho_extr_prim>0.0 .and. rho_extr_alt>0.0) then
      select case(phase)
        case (TREND_LIQ)
          rho_extr_liq = rho_extr_prim
          rho_div = 0.75*rho_extr_prim + 0.25*rho_extr_alt
        case (TREND_VAP)
          rho_extr_liq = rho_extr_alt
          rho_div = 0.75*rho_extr_alt + 0.25*rho_extr_prim
      end select
      ! Always choose liquid fallback.
      rho = trend_fallback_density(T,P,z,TREND_LIQ,rho_extr_liq,rho_div)
      if (present(phase_found_out)) phase_found_out = FAKEPH
    else
      write(*,*) "    PRIM     ALT"
      write(*,*) "Phase:    ",phase_found_prim, phase_found_alt
      write(*,*) "rho:      ",rho_prim, rho_alt
      write(*,*) "rho_extr: ",rho_extr_prim, rho_extr_alt
      write(*,*) "ERROR: Did not find two extrema. Cannot construct rho_div."
      stop
    endif

  end subroutine trend_density_local

  function phaseflag_trend_to_thermopack(trend_phase) result(tp_phase)
    implicit none
    ! Input:
    integer,    intent(in)        :: trend_phase
    ! Output:
    integer                       :: tp_phase

    select case(trend_phase)
      case (TREND_VAP)
        tp_phase = VAPPH
      case (TREND_LIQ)
        tp_phase = LIQPH
      case (SINGLEPH)
        tp_phase = SINGLEPH
      case default
        write(*,*) "ERROR: Unknown TREND phase flag"
        stop
    end select

  end function phaseflag_trend_to_thermopack

  function phaseflag_thermopack_to_trend(tp_phase) result(trend_phase)
    implicit none
    ! Input:
    integer,    intent(in)        :: tp_phase
    ! Output:
    integer                       :: trend_phase

    select case(tp_phase)
      case (VAPPH)
        trend_phase = TREND_VAP
      case (LIQPH)
        trend_phase = TREND_LIQ
      case (SINGLEPH)
        trend_phase = SINGLEPH
      case default
        write(*,*) "ERROR: Unknown Thermopack phase flag"
        stop
    end select

  end function phaseflag_thermopack_to_trend

  subroutine trend_density_robust(T,P,z,phase, rho,phase_found,rho_extr_out)
    implicit none
    ! Input:
    real, intent(in)                :: z(nc)        !< Overall molar compozition [-]
    real, intent(in)                :: T            !< Temperature [K]
    real, intent(in)                :: P            !< Pressure [Pa]
    integer, intent(in)             :: phase        !< Desired phase
    ! Output:
    real, intent(out)               :: rho          !< Density [mol/m3]
    integer, intent(out)            :: phase_found  !< Phase actually found
    real, optional, intent(out)     :: rho_extr_out !< Density of P_EoS extremum [mol/m3]
    ! Locals
    real                            :: drho,dpdrho,d2pdrho2,rho_extr,rhomin,&
                                       rhomax, rho_init,P_init,P_extr,s,P_min,&
                                       P_max
    integer                         :: n_iter
    logical                         :: found_extremum
    integer, parameter              :: max_iter_extr = 1000,&
                                       max_iter_root = 1000
    real, parameter                 :: rho_extrem_rel_tol = machine_prec*1000.0,&
                                       rho_root_rel_tol = machine_prec*100.0
    real                            :: gradient_descent_drho,&
                                       newton_max_drho

    rhomin = 1.0e-10
    ! rhomax = 50000.0
    call trend_rhomax_srk(Z,rhomax)
    gradient_descent_drho = (rhomax-rhomin)/10.0
    newton_max_drho = (rhomax-rhomin)/10.0

    select case(phase)
      case(TREND_LIQ)
        rho_init = rhomax
        s = 1.0 !Looking for minimum
      case(TREND_VAP)
        rho_init = rhomin
        s = -1.0 !Looking for maximum
    end select

    rho = rho_init
    P_init = trend_pressure(Z,T,1.0/rho_init)
    n_iter = 0

    do  ! Find first extremum
      n_iter = n_iter+1
      dpdrho = trend_dpdrho(z,t,rho)
      d2pdrho2 = trend_d2pdrho2(z,t,rho)
      if ( ((phase==TREND_VAP) .and. d2pdrho2 < 0.0)  .or.  &
           ((phase==TREND_LIQ) .and. d2pdrho2 > 0.0)        &
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
      rho = rho + drho

      ! Check convergence of extremum search
      if (abs(drho/rho)<rho_extrem_rel_tol) then
        ! Converged
        found_extremum = .true.
        exit
      elseif ((n_iter == max_iter_extr) .or.         &
              (phase==TREND_LIQ .and. rho < rhomin) .or.  &
              (phase==TREND_VAP .and. rho > rhomax)       &
              ) then
        ! Found no extremum
        found_extremum = .false.
        exit
      endif
    end do

    if (found_extremum) then
      ! An extremum was found
      rho_extr = rho
      if (present(rho_extr_out)) rho_extr_out = rho_extr
      P_extr = trend_pressure(Z,T,1.0/rho_extr)
      ! Is there a root between starting point and the first extremum?
      if ((P_init-P)*(P_extr-P) < 0.0) then
        ! Yes. Find the root.
        select case(phase)
          case(TREND_LIQ)
            rhomin = rho_extr
          case(TREND_VAP)
            rhomax = rho_extr
        end select
        call find_bracketed_root(T,P,Z,phase,rhomin,rhomax,rho_root_rel_tol,max_iter_root, rho)
        phase_found = phase
      else
        ! No. Return -1.0
        rho = -1.0
        phase_found = -1
      endif
    else
      ! An extremum was not found.
      ! P(rho) should be monotonically increasing.
      ! Search entire domain for (presumably) a single root.
      ! rhomax may be way too large, giving negative P_max, etc:
      P_min = trend_pressure(Z,T,1.0/rhomin)
      P_max = trend_pressure(Z,T,1.0/rhomax)
      do while (P_max < P_min)
        rhomax = 0.9*rhomax
        P_max = trend_pressure(Z,T,1.0/rhomax)
      end do
      ! Search for the root
      call find_bracketed_root(T,P,Z,TREND_VAP,rhomin,rhomax,rho_root_rel_tol,max_iter_root, rho)
      phase_found = SINGLEPH
      if (present(rho_extr_out)) rho_extr_out = -1.0
    endif
    return

  end subroutine trend_density_robust

  function trend_fallback_density(T,P,z,phase,rho_extr,rho_div) result(rho)
    use numconstants, only: pi
    implicit none
    ! Input:
    real, intent(in)                :: Z(nc)      !< Overall molar compozition [-]
    real, intent(in)                :: T          !< Temperature [K]
    real, intent(in)                :: P          !< Pressure [Pa]
    integer, intent(in)             :: phase      !< Desired phase
    real, intent(in)                :: rho_extr   !< Density of P_EoS extremum [mol/m3]
    real, intent(in)                :: rho_div    !< Density of wanted divergence point [mol/m3]
    ! Output:
    real                            :: rho        !< Density (fallback) [mol/m3]
    ! Internal:
    real                      :: A,B,C,D
    real                      :: s
    real, parameter           :: alpha = -10.0

    real                      :: d2P_drho2_extr
    real                      :: P_extr
    real                      :: rho_curv

    ! Set phase sign parameter
    select case(phase)
      case(TREND_LIQ)
        s = 1.0
      case(TREND_VAP)
        s = -1.0
    end select

    ! Get some needed quantities
    d2P_drho2_extr = trend_d2Pdrho2(z,T,rho_extr)
    rho_curv = 0.5*(rho_extr+rho_div)
    P_extr = trend_pressure(z,T,1.0/rho_extr)

    ! Calculate coefficients
    B = 0.5*s*pi/(rho_extr-rho_div)
    C = B*rho_extr + 0.5*s*pi
    A = ((alpha*d2P_drho2_extr*sin(B*rho_curv-C)) / &
       (B**2*(1.0/(tan(B*rho_curv-C))**2 + 1.0/(sin(B*rho_curv-C))**2) ))
    D = P_extr + s*A

    ! Calculate fallback density
    rho = (-asin(min(1.0,max(-1.0,A/(P-D)))) -s*pi + C )/B

  end function trend_fallback_density

  !-----------------------------------------------------------------------------
  !> Find a bracketed root of P(rho)-P.
  !>
  !> \author EA, 2013-08-12
  !-----------------------------------------------------------------------------
  subroutine find_bracketed_root(T,P,z,phase,rhomin,rhomax,rho_root_rel_tol,maxit, rhoroot)
    use nonlinear_solvers, only: nonlinear_solver,bracketing_solver,&
                                 NS_RIDDERS
    implicit none
    ! Input:
    real,intent(in)         :: T,P,z(nc),rhomin,rhomax,rho_root_rel_tol
    integer, intent(in)     :: phase,maxit
    ! Output:
    real,intent(out)        :: rhoroot
    ! Internal:
    type(nonlinear_solver)  :: solver
    real, dimension(nc+4)   :: param
    real                    :: rho_srk

    real                    :: rho_init
    real                    :: of
    real                    :: drho
    integer                 :: i

    ! If rho_SRK is within the bracket, use this as a starting point
    rho_SRK = trend_psrk(T,P,z,phase)
    if (rho_srk > rhomin .and. rho_srk < rhomax) then
      rho_init = rho_srk
    else
      rho_init = 0.5*(rhomin+rhomax)
    endif

    ! Simple Newton solver
    rhoroot = rho_init
    do i=1,maxit
      of = trend_pressure(z,T,1.0/rhoroot) - P
      drho = -(of)/trend_dpdrho(z,T,rhoroot)
      rhoroot = rhoroot+drho
      if (abs(drho/rhoroot) < rho_root_rel_tol) then
        ! Converged
        return
      endif
      if (rhoroot > rhomax) then
        ! Fell above the bracket.
        rhoroot = 0.5*(rhoroot-drho+rhomax)
      elseif (rhoroot < rhomin) then
        ! Fell below the bracket.
        rhoroot = 0.5*(rhoroot-drho+rhomin)
      endif
    end do

    ! Newton did not converge. Fall back to bracket solver.
    param(1) = p
    param(2) = T
    param(3:nc+2) = z(1:nc)

    solver%rel_tol = rho_root_rel_tol
    solver%max_it = maxit
    solver%isolver = NS_RIDDERS

    call bracketing_solver(rhomin,rhomax,fun,rhoroot,solver,param)

    select case(solver%exitflag)
      case(0)
        return
      case(1)
        call stoperror("Ridders' method did not converge in the maximum number of iterations.")
      case(4)
        write(*,*) "T,P = ",T,P
        write(*,*) "z =",z
        write(*,*) "phase = ",phase
        write(*,*) "x1,x2 = ",rhomin, rhomax
        write(*,*) "f(x1),f(x2) = ",fun(rhomin,param),fun(rhomax,param)
        call stoperror("Ridders' method did not receive a valid bracket.")
      case default
        write(*,*) "Exit-flag: ",solver%exitflag
        call stoperror("Ridders' method: Unknown error.")
    end select

  end subroutine find_bracketed_root

  !-----------------------------------------------------------------------------
  !> Calculate error in pressure
  !>
  !> \author MHA, 2013-04-11
  !-----------------------------------------------------------------------------
  function fun(var,param) result(f)
    implicit none
    real, intent(in)                  :: var !< Density [mol/m3]
    real, dimension(nc+4), intent(in) :: param !< Parameter vector
    real                              :: f !< Function value [Pa]
    ! Locals
    real                              :: p_trend, t, p, v, rho
    real, dimension(nc)               :: Z

    rho = var
    p = param(1)
    t = param(2)
    Z(1:nc) = param(3:nc+2)
    v = 1.0/rho
    p_trend = trend_pressure(z,t,v)
    f = (p - p_trend)*1.0e-6
  end function fun

  !-----------------------------------------------------------------------------
  !> Calculate differential for pressure function
  !> differentials.
  !>
  !> \author MH, 2013-04-11
  !-----------------------------------------------------------------------------
  subroutine jac(J,var,param)
    implicit none
    real, dimension(1), intent(in) :: var !< Density [mol/m3]
    real, dimension(nc+4), intent(inout) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: J !< Jacobean matrix of objective function
    ! Locals
    real:: p_trend, t, p, v, dpdv, dpdrho, rho, dpdt
    real, dimension(nc) :: Z

    rho = var(1)
    p = param(1)
    t = param(2)
    Z(1:nc) = param(3:nc+2)
    v = 1.0/rho
    p_trend = trend_pressure(z,t,v,dpdv,dpdt)
    dpdrho = - dpdv/rho**2
    param(nc+4) = dpdrho
    J = -dpdrho*1.0e-6
  end subroutine jac

  !-----------------------------------------------------------------------------
  !> Premature termination when solution is outside valid temperature region.
  !>
  !> \author MH, 2013-04-11
  !-----------------------------------------------------------------------------
  function prematureReturn(var,dvar,param,n,np) result(doReturn)
    implicit none
    integer, intent(in)               :: n !< Dimension of X
    integer, intent(in)               :: np !< Dimension of param
    real, dimension(n), intent(in)    :: var !< Density [m3/mol]
    real, dimension(np), intent(inout):: param !< Parameter vector
    real, dimension(n), intent(in)    :: dvar !< Differential of objective function
    logical                           :: doReturn !< Terminate minimization?
    ! Locals

    doReturn = .false.
    if (param(nc+4) < 0.0) then
      doReturn = .true.
    else if (abs(dvar(1)/var(1)) < 2.0*machine_prec) then
      if (param(nc+3) < 2.0*machine_prec) then
        doReturn = .true.
      else
        param(nc+3) = abs(dvar(1)/var(1))
      endif
    endif
  end function prematureReturn

  subroutine trend_density_refine(T,P,z,rho)
    implicit none
    ! Input:
    real, intent(in)                :: z(nc)        !< Overall molar compozition [-]
    real, intent(in)                :: T            !< Temperature [K]
    real, intent(in)                :: P            !< Pressure [Pa]
    ! Output:
    real, intent(inout)             :: rho          !< Density [mol/m3]

    ! Locals
    real                            :: of,drho,rhomin,rhomax,rho_init
    integer                         :: i
    integer, parameter              :: max_iter = 5
    real, parameter                 :: rho_root_rel_tol = machine_prec*10.0

    rhomin = rho*0.98
    rhomax = rho*1.02
    rho_init = rho

    ! Simple Newton solver
    do i=1,max_iter
      of = trend_pressure(z,T,1.0/rho) - P
      drho = -(of)/trend_dpdrho(z,T,rho)
      rho = rho+drho
      if (rho > rhomax) then
        ! Fell above the bracket.
        rho = rho_init
        return
      elseif (rho < rhomin) then
        ! Fell below the bracket.
        rho = rho_init
        return
      endif
      if (abs(drho/rho) < rho_root_rel_tol) then
        ! Converged
        return
      endif
    end do


  end subroutine trend_density_refine

  !-----------------------------------------------------------------------------
  !> Is the solver returning FAKEPH solution
  !>
  !> \author MH, 2020-01
  !-----------------------------------------------------------------------------
  function trend_phase_is_fake(T,P,Z,phase) result(isFake)
    implicit none
    ! Input:
    real, intent(in)                :: Z(nc)          !< Molar compozition [-]
    real, intent(in)                :: T              !< Temperature [K]
    real, intent(in)                :: P              !< Pressure [Pa]
    integer, intent(in)             :: phase          !< Phase
    ! Output:
    logical                         :: isFake         !< Phase is FAKEPH
    ! Internal:
    real :: rho
    integer :: phase_found_out

    call trend_density(T,P,Z,phase,rho,phase_found_out)
    isFake = (phase_found_out == FAKEPH)
  end function trend_phase_is_fake

end module trend_solver
