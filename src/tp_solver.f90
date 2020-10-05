!-------------------------------------------------------------------------
!> Solve TP flash problem. Look for single phase or a mixture of LV.
!>
!> \todo Need trace-component functionallity.
!> \todo Add DEM of order 2 for accelration of multi-phase Rachford-Rice
!>
!-------------------------------------------------------------------------
module tp_solver
  !
  !
  use numconstants, only: machine_prec, small
  use thermopack_constants
  use thermopack_var, only: nc
  use eos
  use thermo_utils, only: wilsonK, phase_Is_fake
  use stability, only : stabcalc, stabilityLimit
  use utilities, only: safe_exp
  implicit none
  private
  save

  !> Maximum number of iterations used in nonlinear solvers
  integer, parameter :: max_iter = 1500
  !> Accept two-phase solution even though its Gibbs energy is slightly larger than that of the single-phase feed
  real :: g_tolerance = machine_prec * 10.0

  public :: twoPhaseTPflash, differentials, rr_solve, objective
  public :: g_tolerance
  public :: rr_successive_substitution_iteration

contains

  !-------------------------------------------------------------------------
  !> Given pressure and temperature calculate phase distribution.
  !> Start by doing successive substitutions with acceleration
  !> with the Dominant Eigenvalue Method every 5th iteration.
  !> If no solution is found in 10 iterations, a modified Newton
  !> is used to converge the two phase flash.
  !>
  !> In order to find the phase distribution with the lowest gibbs
  !> energy, the minimum single gibbs energy is comapred with the
  !> mixture gibbs energy.
  !>
  !> \author MHA, 2012-01-30, EA, 2013-07, MAG, 2013-09
  !-----------------------------------------------------------------------------
  subroutine twoPhaseTPflash(t,p,Z,beta,betaL,phase,X,Y)
    implicit none
    real,                intent(out)   :: beta  !< Vapour phase molar fraction [-]
    real, dimension(nc), intent(in)    :: Z     !< Overall molar compozition [-]
    real, dimension(nc), intent(out)   :: X     !< Liquid molar compozition [-]
    real, dimension(nc), intent(out)   :: Y     !< Vapour molar compozition [-]
    real,                intent(in)    :: t     !< Temperature [K]
    real,                intent(in)    :: p     !< Pressure [Pa]
    integer,             intent(out)   :: phase !< Phase identefier
    real,                intent(out)   :: betaL !< Liquid phase molar fraction [-]
    ! Locals
    real, dimension(nc) :: K,FUGL,FUGV,FUGL_old,FUGV_old,DKm1,DKm2,DK,K_old,K_in
    real, dimension(nc) :: FUGZ
    integer :: iter,minGphase
    logical :: sloppy
    real :: g_feed, g_simp, g_simp_old, g_solution
    logical :: do_DEM, gas_stab_negative, liq_stab_negative, do_DEM_now, converged
    logical :: stab_analysis_done, rr_has_solution, do_stability_check
    logical :: feed_is_stable, found_smaller_g, valid_beta
    integer, parameter :: dem_acc_freq = 5
    real, dimension(nc) :: stabK, Wg, Wl

    beta = 0.5 ! Make sure beta is initialized
    if (verbose) then
      write(*,*) ""
      write(*,*) "---Entering twoPhaseTPflash---"
      write(*,*) "T=",T,"  P=",P
      write(*,*) "Z=",Z
      write(*,*) "------------------------------"
    endif

    ! Set stability flags
    gas_stab_negative  = .false.
    liq_stab_negative  = .false.
    stab_analysis_done = .false.

    ! Calculate starting values
    call wilsonK(t,p,K)
    if (verbose) write(*,*) "Wilson K = ",K

    ! Get minimum gibbs solution for single phase. Store phase flag in minGphase.
    call calc_singlephase(T,P,Z, FUGZ,g_feed,minGphase)
    if (minGphase == FAKEPH) then
      if (verbose) write(*,*) "Feed is artificial phase! Setting g_feed to huge."
      g_feed = huge(1.0)
      stab_analysis_done = .true. ! Two-phase only possible outcome
    endif

    ! Successive substitution init and settings
    FUGL = 1.0e-6
    FUGV = 1.0e-6
    if (minGphase == FAKEPH) then
      g_simp = g_feed
    else
      g_simp = 10.0*g_feed
    endif
    do_DEM = .true.
    sloppy = .false.

    ! Successive substitution
    do iter=1,max_iter
      if (verbose) write(*,*) "Iteration: ",iter
      g_simp_old = g_simp
      do_DEM_now = (do_DEM .and. mod(iter,dem_acc_freq) == 0)
      ! Perform dominant eigenvalue method every dem_acc_freq iterations.
      if (do_DEM_now) then
        K_old = K
        K = k_dem(DK,DKm1,DKm2,FUGL,FUGV,K)
      endif
      ! Perform Rachford-Rice successive substitution iteration
      FUGL_old = FUGL
      FUGV_old = FUGV

      K_in = K
      call rr_successive_substitution_iteration(T,P,Z,K_in,sloppy, &
           X,Y,beta,K,FUGL,FUGV,g_simp,converged,rr_has_solution,betaL)
      if (converged) then
        ! Success: Successive substitution of RR-eq. converged.
        phase = TWOPH
        return
      endif
      ! Revert if dominant eigenvalue method did not help.
      if (do_DEM_now) then
        if (g_simp > g_simp_old) then ! Discard extrapolation of ln K
          if (verbose) write(*,*) "Dom.eig.method did not help. (g_simp > g_simp_old)"
          if (verbose) write(*,*) "Reverting: K=K_old, recalculating beta,X,Y,FUGL,FUGV,g_simp."
          K_in = K_old
          call rr_successive_substitution_iteration(T,P,Z,K_in,sloppy, &
                X,Y,beta,K,FUGL,FUGV,g_simp,converged,rr_has_solution,betaL)
        endif
      endif

      ! Updating variables for dominant eigenvalue method.
      if (do_DEM .and. rr_has_solution) then
        DKm2 = DKm1
        DKm1 = DK
        DK = FUGL - FUGV - FUGL_old + FUGV_old
      endif

      ! Do stability check if the total Gibbs energy did not decrease, or if beta is outside [0,1].
      found_smaller_g = g_simp < g_feed
      if (beta /= beta) then ! beta = NaN ?
        valid_beta = .false.
      else
        valid_beta = (beta > 0.0 .and. beta < 1.0)
      endif
      do_stability_check = ((((.not. found_smaller_g) .OR. (.not. valid_beta)) .and. iter == dem_acc_freq)&
                           .or. (.not. rr_has_solution))
      if (do_stability_check) then
        if (.not. stab_analysis_done) then ! Have we restarted already?
          if (verbose) then
            write(*,*) "Performing stability analysis"
            write(*,*) "beta,g_simp,g_feed,rr_has_solution: ",beta,g_simp,g_feed,rr_has_solution
          end if
          call stability_analysis(T,P,Z,FUGZ, K,FUGV,FUGL,feed_is_stable,stab_analysis_done,Wg,Wl)
          if (feed_is_stable) then
            ! Success: Single phase
            call set_single_phase(minGphase,Z, beta,X,Y,phase,betaL)
            return
          else
            stabK = K
          endif
        else
          ! Have already tried stability analysis. Try newton method directly?
          call mod_newton_search(T,P,Z,beta, X,Y,phase,g_solution,betaL,stabK,Wg,Wl)
          if (g_solution - g_feed > g_tolerance) then
            if (continueOnError) then
              ! phase = -1 : If this is to be included all thermopack code must handle "phase = -1"
              return
            else
              write(*,*) 'tp_solver::twoPhaseTPflash: (mod_newton_search) g_solution > g_feed!'
              write(*,*) 'g_solution,g_feed,g_tolerance = ',g_solution, g_feed, g_tolerance
              write(*,*) 'Temperature = ',T
              write(*,*) 'Pressure = ',P
              write(*,*) 'Composition = ',Z
              call stoperror('')
            endif
          else
            ! Success: Modified Newton search converged to a two-phase solution of smaller Gibbs.
            return
          endif
          print *,'tp_solver::twoPhaseTPflash: Will not perform stability analysis a second time.'
          print *,'tp_solver::twoPhaseTPflash: Not able to solve for beta given the initial K values'
          print *,'T,P = ',T,P
          print *,'K = ',K
          call stoperror('')
        endif
      endif
      ! If no solution is found in 2*dem_acc_freq iterations, a modified Newton is used to converge the two phase flash.
      if (iter == 2*dem_acc_freq) then
        if (verbose) write(*,*) "iter == 2*dem_acc_freq: Modified Newton"
        do_stability_check = ( (.not. found_smaller_g) .OR. (.not. valid_beta) )
        if (do_stability_check .and. (.not. stab_analysis_done)) then
          call stability_analysis(T,P,Z,FUGZ, K,FUGV,FUGL,feed_is_stable,stab_analysis_done,Wg,Wl)
          if (feed_is_stable) then
            ! Success: Single phase
            call set_single_phase(minGphase,Z, beta,X,Y,phase,betaL)
            return
          else
            stabK = K
          endif
        endif
        ! Do modified newton search
        call mod_newton_search(T,P,Z,beta, X,Y,phase,g_solution,betaL,stabK,Wg,Wl)
        if (g_solution - g_feed > g_tolerance) then
          if (continueOnError) then
            ! phase = -1 : If this is to be included all thermopack code must handle "phase = -1"
            return
          else
            write(*,*) 'tp_solver::twoPhaseTPflash: (mod_newton_search) g_solution > g_feed!'
            write(*,*) 'g_solution,g_feed,g_tolerance = ',g_solution, g_feed, g_tolerance
            write(*,*) 'Temperature = ',T
            write(*,*) 'Pressure = ',P
            write(*,*) 'Composition = ',Z
            call stoperror('')
          endif
        else
          ! Success: Modified Newton search converged to a two-phase solution of smaller Gibbs.
          return
        endif
      endif
    enddo

    ! EA: Will we ever get here? The modified Newton search either ends in error or return.
    print *,'tp_solver::twoPhaseTPflash: No convergence in maximum number of iterations.'
    call exit(1)

  end subroutine twoPhaseTPflash

  !-----------------------------------------------------------------------------
  !> Calculate the most fugacity and minimum Gibbs energy of the feed as a
  !> single phase.
  !-------------------------------------------------------------------------
  subroutine calc_singlephase(T,P,Z,FUGZ,g_feed,phaseflag)
    implicit none
    ! Input:
    real,    intent(in)   :: T,P,Z(nc)
    ! Output:
    real,    intent(out)  :: FUGZ(nc),g_feed
    integer, intent(out)  :: phaseflag
    ! Internal:
    integer               :: i
    !---------------------------------------------------------------------------
    call thermo(T,P,Z,MINGIBBSPH,FUGZ,ophase=phaseflag)
    g_feed = 0.0
    do i=1,nc
      if (Z(i) > zLimit) then
        g_feed = g_feed + Z(i)*(log(Z(i))+FUGZ(i))
      endif
    enddo
    !
  end subroutine calc_singlephase

  !-----------------------------------------------------------------------------
  !> Advance the equilibrium factors K using the dominant eigenvalue method.
  !>
  !> Note: There are several ways to approximate the dominant eigenvalue
  !>       lambda. For one eigenvalue, we opt for that described in Michelsen
  !>       and Mollerup (2007). For two eigenvalues, we follow Crowe and Nisho
  !>       (1975).
  !-------------------------------------------------------------------------
  function k_dem(DK,DKm1,DKm2,FUGL,FUGV,K)
    implicit none
    ! Input:
    real, intent(in)    :: DK(nc),DKm1(nc),DKm2(nc),FUGL(nc),FUGV(nc),K(nc)
    ! Output:
    real                :: k_dem(nc)
    ! Internal:
    real                              :: lambda,denominator
    real,    dimension(0:2,1:2)       :: b
    real,    dimension(1:2)           :: mu
    integer, parameter                :: neigenvalues=2
    !---------------------------------------------------------------------------
    select case (neigenvalues)
    case (1)
      if (verbose) write(*,*) "Dominant eigenvalue method."
      ! lambda = sum(DK**2)/sum(DK*DKm1) ! An alternative.
      lambda = sum(DK*DKm1)/sum(DKm1*DKm1)
      if (lambda < 1.0) then
        ! Extrapolate ln K
        if (verbose) write(*,*) "lambda = ", lambda
        if (verbose) write(*,*) "lambda < 1 => Extrapolating ln(K)"
        ! k_dem = safe_exp(FUGL - FUGV + DK*lambda/(1.0-lambda))
        k_dem = safe_exp(FUGL - FUGV + DK/(1.0-lambda))
        if (verbose) write(*,*) "New K = ",k_dem
      else
        k_dem = K
      endif
      !
    case (2)
      b(0,1) = inner_product(dk,dkm1)
      b(0,2) = inner_product(dk,dkm2)
      b(1,1) = inner_product(dkm1,dkm1)
      b(1,2) = inner_product(dkm1,dkm2)
      b(2,1) = b(1,2)
      b(2,2) = inner_product(dkm2,dkm2)
      !
      denominator =   b(1,1)*b(2,2) - b(2,1)*b(1,2)
      if (abs(denominator) > 1e-20) then
        mu(1)       = ( b(0,2)*b(2,1) - b(0,1)*b(2,2) )/denominator
        mu(2)       = ( b(0,1)*b(1,2) - b(0,2)*b(1,1) )/denominator
        k_dem = safe_exp( &
             fugl - fugv + ( dk - dkm1*mu(2) )/( 1.0 - mu(1) - mu(2) ) )
      else
        k_dem = k
      end if
    end select
    !
  contains
    !
    pure function inner_product(x,y)
      real, dimension(nc), intent(in) :: x,y
      real                            :: inner_product
      inner_product = sum(x*y)
    end function inner_product
    !
  end function k_dem

  !-----------------------------------------------------------------------------
  !> Do one iteration of successive substitution. First, solve the Rachford-
  !> Rice equation for the given Z and K. Then, calculate the logarithm of the
  !> fugacity coefficients and update K.
  !-----------------------------------------------------------------------------
  subroutine rr_successive_substitution_iteration(T,P,Z,K_in,sloppy,X,Y,&
       beta,K_out,FUGL,FUGV,g_simp,converged,rr_has_solution,betaL,phaseY)
    implicit none
    ! Input:
    real,    intent(in)           :: T,P,Z(nc),K_in(nc)
    logical, intent(in)           :: sloppy
    integer, optional, intent(in) :: phaseY
    ! Output:
    real,    intent(out)          :: X(nc),Y(nc),beta,K_out(nc),FUGL(nc),FUGV(nc),g_simp
    logical, intent(out)          :: converged,rr_has_solution
    real,    intent(out)          :: betaL
    ! Internal:
    integer                       :: i, phase
    real                          :: error
    real, parameter               :: rel_tol=1.0e6*machine_prec
    !---------------------------------------------------------------------------
    converged = .false.
    !
    ! Solve the Rachford-Rice equation (find beta,X,Y) given Z and K, using Newton's method.
    if (verbose) write(*,*) "Attempting to solve Rachford-Rice equation."
    rr_has_solution = rr_solve(nc,Z,K_in,beta,X,Y,sloppy,betaL)
    !
    if (rr_has_solution) then
      if (verbose) write(*,*) "Found solution to Rachford-Rice equation."
      if (verbose) write(*,*) "beta = ",beta
      if (verbose) write(*,*) "X = ",X,", sum(X) = ",sum(X)
      if (verbose) write(*,*) "Y = ",Y,", sum(Y) = ",sum(Y)
      !
      ! Get ln(fugacity coeff) in the phases from the Rachford-Rice solution.
      if (verbose) write(*,*) "Found solution to Rachford-Rice equation."
      if (verbose) write(*,*) "Finding FUGL..., X = ",X
      call thermo(t,p,X,LIQPH,FUGL) ! ln phi liquid
      if (verbose) write(*,*) "Done. FUGL = ",FUGL
      if (verbose) write(*,*) "Finding FUGV..., Y = ",Y
      if (present(phaseY)) then
        phase = phaseY
      else
        phase = VAPPH
      endif
      call thermo(t,p,Y,phase,FUGV) ! ln phi vapour
      if (verbose) write(*,*) "Done. FUGV = ",FUGV
      !
      ! Calculate the Gibbs energy
      g_simp = 0.0
      do i=1,nc
        if (Z(i) > zLimit) then
          ! Since x*ln(x) = 0 as x -> 0:
          if (Y(i) /= 0.0) g_simp = g_simp + (beta)*(Y(i)*(log(Y(i))+FUGV(i)))
          if (X(i) /= 0.0) g_simp = g_simp + (1.0-beta)*(X(i)*(log(X(i))+FUGL(i)))
        endif
      enddo
      !
      ! Update K values based on the fugacities from the Rachford-Rice solution.
      K_out = exp(FUGL - FUGV)
      !
      ! Compute iteration error, i.e. magnitude of change since last iteration.
      error = maxval(abs((K_out-K_in))/K_in)
      !
      if (error < rel_tol) then
        ! If the fugacities have converged
        ! Update solution
        rr_has_solution = rr_solve(nc,Z,K_out,beta,X,Y,sloppy,betaL)
        if (beta >= 0.0 .and. beta <= 1.0) then
          ! If solution is two phase, return.
          if (verbose) then
            write (*,*) "tp_solver::twoPhaseTPflash: Successive substitution converged!"
          endif
          converged = .true.
        else
          ! If not, continue to stability check.
          rr_has_solution = .false.
        endif
      else
        if (verbose) write(*,*) "tp_solver::twoPhaseTPflash: Successive substitution not yet converged. Error = ",error
      endif
    else
      if (verbose) write(*,*) "Did not find solution to Rachford-Rice equation."
    endif
    !
  end subroutine rr_successive_substitution_iteration

  !-----------------------------------------------------------------------------
  !> Do tangent-plane stability analysis.
  !-------------------------------------------------------------------------
  subroutine stability_analysis(T,P,Z,FUGZ, K,FUGV,FUGL,feed_is_stable,&
       stab_analysis_done,Wg,Wl)
    implicit none
    ! Input:
    real,    intent(in)         :: T,P,Z(nc),FUGZ(nc)
    ! Output:
    real,    intent(out)        :: K(nc),FUGV(nc),FUGL(nc)
    logical, intent(out)        :: feed_is_stable
    logical, intent(inout)      :: stab_analysis_done
    real, optional, intent(out) :: Wg(nc),Wl(nc)
    ! Internal:
    real                    :: tpdl,tpdg,FUGLs(nc),FUGVs(nc)
    logical                 :: liq_stab_negative,gas_stab_negative
    !---------------------------------------------------------------------------
    ! Tangent plane analysis with "liquid-like" initial W. (Output: FUGLs,tpdl)
    if (verbose) write(*,*) "Stability analysis (liquid-like initial W)"
    tpdl = stabcalc(t,p,Z,LIQPH,FUGZ,FUGLs,Wl)
    liq_stab_negative = (tpdl < stabilityLimit .and. &
         .not. phase_Is_fake(T,P,Wl,LIQPH))
    ! Tangent plane analysis with "vapor-like" initial W. (Output: FUGVs,tpdg)
    if (verbose) write(*,*) "Stability analysis (vapor-like initial W)"
    tpdg = stabcalc(t,p,Z,VAPPH,FUGZ,FUGVs,Wg)
    gas_stab_negative = (tpdg < stabilityLimit .and. &
         .not. phase_Is_fake(T,P,Wg,VAPPH))
    !
    ! Do we need to restart?
    if (liq_stab_negative .or. gas_stab_negative) then
      feed_is_stable = .false.
      if (liq_stab_negative .and. gas_stab_negative) then
        if (verbose) write(*,*) "Feed is unstable with respect to both liquid-like and vapor-like phase"
        if (verbose) write(*,*) "FUGLs =",FUGLs
        if (verbose) write(*,*) "FUGVs =",FUGVs
        K = safe_exp(FUGLs-FUGVs)
        FUGV = FUGVs
        FUGL = FUGLs
      else if (liq_stab_negative) then
        if (verbose) write(*,*) "Feed is unstable with respect to liquid-like phase"
        if (verbose) write(*,*) "FUGLs =",FUGLs
        K = safe_exp(FUGLs-FUGZ)
        FUGV = 1.0e-6
        FUGL = 1.0e-6
      else if (gas_stab_negative) then
        if (verbose) write(*,*) "Feed is unstable with respect to vapor-like phase"
        if (verbose) write(*,*) "FUGVs =",FUGVs
        K = safe_exp(FUGZ-FUGVs)
        FUGV = 1.0e-6
        FUGL = 1.0e-6
      endif
      if (verbose) write(*,*) "New K =",K
    else
      if (verbose) write(*,*) "Feed is stable as single phase"
      feed_is_stable = .true.
    endif
    stab_analysis_done = .true.
    !
  end subroutine stability_analysis

  !-----------------------------------------------------------------------------
  !> Set beta, X and Y according to the specified phase flag.
  !-------------------------------------------------------------------------
  subroutine set_single_phase(phaseflag,Z,beta,X,Y,phase,betaL)
    implicit none
    ! Input
    integer, intent(in)  :: phaseflag
    real,    intent(in)  :: Z(nc)
    ! Output:
    real,    intent(out) :: beta,X(nc),Y(nc),betaL
    integer, intent(out) :: phase
    !---------------------------------------------------------------------------
    if (phaseflag == LIQPH) then
      beta = 0.0
      betaL = 1.0
      Y = 0.0
      X = Z
    else if (phaseflag == VAPPH) then
      beta = 1.0
      betaL = 0.0
      Y = Z
      X = 0.0
    else if (phaseflag == SINGLEPH) then
      beta = -1.0
      betaL = -1.0
      X = Z
      Y = 0.0
    else
      call stoperror("set_single_phase: Unknown phase flag.")
    endif
    phase = phaseflag
    !
  end subroutine set_single_phase

  !-------------------------------------------------------------------------
  !> Map out the simplified Gibbs energy in k space.
  !-------------------------------------------------------------------------
  subroutine map_g_simp(t,p,z,fugl_old,fugv_old,k_dem,k_old)
    implicit none
    ! Arguments:
    real, intent(in) :: t,p
    real, dimension(nc), intent(in) :: z,fugv_old,fugl_old,k_dem,k_old
    ! Local variables
    integer :: nks,ik,jk
    real :: g_simp
    real :: beta, betaL
    real, dimension(nc) :: deltak
    real, dimension(nc) :: x_dummy,y_dummy
    real, dimension(nc) :: fugv_dummy,fugl_dummy
    real, dimension(nc) :: k_in,k_out,k_max,k_min,dk
    logical :: rr_has_solution_dummy,converged_dummy
    !---------------------------------------------------------------------------
    nks=200
    !
    open(unit=81,file="g_simp.dat",status="replace",action="write")
    !
    deltak=k_dem-k_old
    k_min=(K_in+K_old)/2-0.75*deltak-0.1
    k_max=(K_in+K_old)/2+0.75*deltak
    dk=(k_max-k_min)/(nks-1)
    !
    k_in=k_min
    do ik=1,nks
      k_in(2)=k_min(2)
      do jk=1,nks
        call rr_successive_substitution_iteration(t,p,z,k_in,.false., x_dummy,y_dummy,&
             beta,k_out,fugl_dummy,fugv_dummy,g_simp,converged_dummy,&
             rr_has_solution_dummy,betaL)
        write (81,*) k_in(1),k_in(2),g_simp,beta
        k_in(2)=k_in(2)+dk(2)
      end do
      k_in(1)=k_in(1)+dk(1)
    end do
    !
    close(81)
    !
  end subroutine map_g_simp

  !-------------------------------------------------------------------------
  !> Attempt to solve two-phase TP flash by the Modified Newton Method.
  !!
  !! \author MHA, 2012-01-30, EA, 2013-07-26, MAG, 2013-09-12
  !-------------------------------------------------------------------------
  subroutine mod_newton_search(T,P,Z,beta,X,Y,phase,g_solution,betaL,stabK,Wg,Wl)
    use optimizers, only: optimize, optim_param
    implicit none
    ! Input:
    real, intent(in)        :: T,P,Z(nc),stabK(nc),Wg(nc),Wl(nc)
    ! Output:
    real, intent(out)       :: Y(nc),X(nc),g_solution,betaL
    integer, intent(out)    :: phase
    ! In/Out:
    real, intent(inout)     :: beta
    ! Internal:
    real                    :: V(nc),L(nc)
    type(optim_param)       :: optim
    real, dimension(2*nc+2) :: param
    logical                 :: rr_has_solution
    real, parameter         :: rel_tol_mod_newton = machine_prec * 300.0
    integer, parameter      :: max_iter_mod_newton = 1000
    integer                 :: i
    !---------------------------------------------------------------------------
    if (beta>=0.0 .and. beta<=1.0) then
      ! beta is acceptable. Continue with recieved values.
      V = beta*Y
      L = Z - V
    else
      ! beta is unacceptable. Calculate new starting values.
      rr_has_solution = rr_solve(nc,Z,stabK,beta,X,Y,.false.,betaL)
      if (rr_has_solution .and. beta>=0.0 .and. beta<=1.0) then
        V=beta*Y
        L=Z-V
      else if (beta <= 0.0) then
        beta = 0.001
        L=min((1.0-beta)*Wl,(1.0-1.0e-6)*Z)
        V=Z-L
      else
        beta = 0.999
        V=min(beta*Wg,(1.0-1.0e-6)*Z)
        L=Z-V
      endif
      betaL = sum(L)
      X = L/betaL
      Y = V/beta
    end if
    do i=1,nc
      if (V(i) >= L(i)) then
        L(i) = betaL*X(i)
        V(i) = Z(i) - L(i)
      else
        V(i) = beta*Y(i)
        L(i) = Z(i) - V(i)
      end if
    enddo
    !
    param(1) = t
    param(2) = p
    param(3:nc+2) = Z
    param(nc+3:2*nc+2) = L
    optim%max_line_search_iter = 2
    optim%rel_tol = rel_tol_mod_newton
    optim%max_iter = max_iter_mod_newton
    !
    call optimize(optim,objective,differentials,V,param,&
         limitDV,prematureReturn,get_problem_size,setV)
    !
    beta = sum(V)
    Y = V/beta
    L = param(nc+3:2*nc+2)
    betaL = sum(L)
    X = L/betaL
    phase = TWOPH
    g_solution = optim%of
    !
    if (optim%exitflag > 0 .and. .not. continueOnError) then
      print *,'(T,P): ',T, P
      print *,'Z:',Z
      print *, 'Exitflag: ',optim%exitflag
      print *,'beta: ', beta
      print *,'betaL: ', betaL
      print *,'X: ', X
      print *,'Y: ', Y
      print *,'g two phase: ', g_solution
      call stoperror('tp_solver::twoPhaseTPflash: The nonlinear solver did not converge')
    endif
    !
  end subroutine mod_newton_search


  !-------------------------------------------------------------------------
  !> Given K-values for all components solve for vapour fraction.
  !! If a solution exsist the function will return true, otherwise false.
  !!
  !! The Rachford-Rice equation to be solved:
  !!  \f$ g(\beta)  = \sum
  !!      \frac{z_i (K_i - 1)}{1-\beta+\beta K_i} = 0 \f$.
  !!
  !! Where the sum is over all components.
  !!
  !! To avoid problems for \f$ \beta \approx 1.0\f$, the problem is instead
  !! solved for \f$ 1.0 - \beta\f$.
  !!
  !! g is a monotonous function in \f$ \beta\f$, and is solved using a Newton
  !! method combined with bracketing.
  !!
  !! \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function rr_solve(nc_rr,Z,K,beta,X,Y,sloppy,betaL) result(solution)
    implicit none
    integer, intent(in) :: nc_rr !< Number of components
    real, intent(out) :: beta !< Vapour phase molar fraction [-]
    real, dimension(nc_rr), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc_rr), intent(in) :: K !< Molar based K-values [-]
    real, dimension(nc_rr), intent(out) :: X !< Liquid molar compozition [-]
    real, dimension(nc_rr), intent(out) :: Y !< Vapour molar compozition [-]
    logical, intent(in) :: sloppy !< Only do single iteration
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    logical :: solution !< True is a solution exsist
    ! Locals
    !real, parameter :: tol = machine_prec * 1.0e5, dtol=machine_prec * 1.0e5
    !real, parameter :: K_lim = machine_prec * 100.0
    real, parameter :: tol = machine_prec * 1.0e2, dtol=machine_prec * 1.0e2
    real, parameter :: K_lim = machine_prec * 10.0
    real :: g, g0, g1, beta_max, beta_min, dgdb
    real :: dbeta, dbak, dbakm1, beta_old
    real :: K_max, K_min
    real, dimension(nc_rr) :: L,V, Div
    integer :: iter,i
    logical :: solve_for_beta_vap, converged

    if (verbose) print *,'tp_solver:rr_solver: K=', K
    if (verbose) print *,'tp_solver:rr_solver: Z=', Z

    solution = .true.
    if (sloppy) then
      ! Do single Newton step
      g = sum(Z*(K-1)/(1-beta+beta*K))
      dgdb = -sum(Z*((K-1)/(1-beta+beta*K))**2)
      dbeta = - g/dgdb
      beta = beta + dbeta
      L = (1-beta)*Z/(1-beta+beta*K)
      V = beta*Z*K/(1-beta+beta*K)
      beta = sum(V)
      X = L/(1-beta)
      Y = V/beta
      return
    endif

    K_max = maxval(K)
    K_min = minval(K)
    if (K_min < 1.0 .and. K_max > 1.0) then
      if (abs(K_min-1) < K_lim .and. abs(K_max-1) < K_lim) then
        ! Avoid divide by zero
        solution = .false.
        if (verbose) then
          print *,'tp_solver:rr_solver: All k values are 1'
        endif
        return
      endif
      beta_max = 1.0 / (1.0 - K_min)
      beta_min = -1.0 / (K_max - 1.0)
    else
      beta_max = 1.0
      beta_min = 0.0
      g0 = sum(Z*(K-1)/(1-beta_min+beta_min*K))
      g1 = sum(Z*(K-1)/(1-beta_max+beta_max*K))
      if (.not. (g0 > 0.0 .and. g1 < 0.0)) then
        ! No solution exsist
        solution = .false.
        if (verbose) then
          print *,'tp_solver:rr_solver: [beta_min,beta_max] = ',beta_min,beta_max
          print *,'tp_solver:rr_solver: [beta_min,beta_max] does not bracket a zero in g.'
          print *,'tp_solver:rr_solver: No solution to rr problem'
        endif
        return
      endif
    endif
!    if (verbose) then
!      print *,'tp_solver::rr_solver: minimum beta: ', beta_min
!      print *,'tp_solver::rr_solver: maximum beta: ', beta_max
!    endif

    ! Solution exists
    ! Try to limit beta_max and beta_min
    do i=1,nc_rr
      if (K(i) > 1.0) then
        beta_min = max(beta_min, (K(i)*Z(i)-1.0)/(K(i)-1.0))
      else if (K(i) < 1.0) then
        beta_max = min(beta_max, (1.0-Z(i))/(1.0-K(i)))
      endif
    end do

    ! Select form to solve for
    solve_for_beta_vap = .true.
    if (beta_max > 0.5) then
      g = sum(Z*(K-1)/(0.5+0.5*K))
      if (g < 0.0) then
        beta_max = 0.5
      else
        ! Solve for liquid phase fraction
        solve_for_beta_vap = .false.
        beta_min = 1 - beta_max
        beta_max = 0.5
      endif
    endif

    ! Set initial guess
    beta = 0.5*(beta_max + beta_min)
    dbak = 0.0

    if (verbose) then
      print *,'tp_solver::rr_solver: minimum beta: ', beta_min
      print *,'tp_solver::rr_solver: maximum beta: ', beta_max
      print *,'tp_solver::rr_solver: initial beta: ', beta
      print *,'tp_solver::rr_solver: solve_for_beta_vap: ',solve_for_beta_vap
    endif


    if (verbose) print *,'tp_solver:rr_solver: Solving to find beta,X,Y (given K)'
    converged = .false.
    do iter=1,max_iter
      if (verbose) print *,'tp_solver:rr_solver: Newton iteration: ',iter
      if (solve_for_beta_vap) then
        Div = (1-beta+beta*K)
        if ( any(Div==0)) then
          solution = .false.
          return
        endif
        g = sum(Z*(K-1)/Div)
        if (g > 0.0) then
          beta_min = beta
        else if (g < 0.0) then
          beta_max = beta
        endif
        ! Newton search direction
        dgdb = -sum(Z*((K-1)/(1-beta+beta*K))**2)
      else
        Div = beta+(1-beta)*K
        if ( any(Div == 0 )) then
          solution = .false.
          return
        endif
        g = sum(Z*(K-1)/Div)
        if (g < 0.0) then
          beta_min = beta
        else if (g > 0.0) then
          beta_max = beta
        endif
        ! Newton search direction
        dgdb = sum(Z*((K-1)/(beta+(1-beta)*K))**2)
      endif
      dbeta = - g/dgdb
      beta_old = beta
      beta = beta + dbeta
      dbakm1 = dbak
      if (beta < beta_min .or. beta > beta_max) then
        beta = 0.5*(beta_min+beta_max)
      endif
      dbak = beta - beta_old
      if (verbose) then
        if (iter > 1) then
          print *,'beta = ',beta,",  g(beta) = ",g,",  dbeta = ",dbak
          print *,'tp_solver::rr_solver: delta beta k divided by delta beta k-1 squared: ',dbak/max(dbakm1**2,1.0e-30)
        endif
      endif
      if (abs(g) < tol .and. abs(dbak) < dtol) then
        if (verbose) write(*,*) "tp_solver::rr_solver: Detected convergence."
        converged = .true.
        exit ! Terminate do-loop
      endif
    enddo
    if (.not. converged) then
      solution = .false.
      if (solve_for_beta_vap) then
        betaL = 1.0 - beta
      else
        betaL = beta
        beta = 1.0 - beta
      endif
      return
    endif
    if (verbose) then
      print *,'tp_solver::rr_solver: Number of iterations: ', iter
    endif
    if (solve_for_beta_vap) then
      X = Z/(1-beta+beta*K)
      Y = K*Z/(1-beta+beta*K)
      betaL = 1.0 - beta
    else
      X = Z/(beta+(1-beta)*K)
      Y = K*Z/(beta+(1-beta)*K)
      betaL = beta
      beta = 1.0 - beta
    endif
    if (verbose) then
      write(*,*) "tp_solver::rr_solver: "
      write(*,*) "Given K = ",K
      write(*,*) "the beta_vap which minimizes g is: ",beta
    end if

  end function rr_solve

  !-------------------------------------------------------------------------
  !> Calculate Gibbs energy for a liquid-vapour mixture.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function objective(V,param) result(of)
    implicit none
    real, dimension(nc), intent(in) :: V !< Vapour mole numbers [mole]
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    real :: of !< Objective function value
    ! Locals
    integer :: i
    real, dimension(nc) :: Z
    real:: beta, t, p, betaL
    real, dimension(nc) :: FUGL,FUGV,L,X,Y

    t = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    L = param(nc+3:2*nc+2)
    beta = sum(V)
    Y = V/beta
    betaL = sum(L)
    X = L/betaL
    call thermo(t,p,X,LIQPH,FUGL)
    call thermo(t,p,Y,VAPPH,FUGV)
    of = 0.0
    do i=1,nc
      if (Z(i) > zLimit) then
        of = of + V(i)*(log(Y(i))+FUGV(i)) + L(i)*(log(X(i))+FUGL(i))
      end if
    end do
  end function objective

  !-------------------------------------------------------------------------
  !> Calculate Gibbs energy for a liquid-vapour mixture. And its
  !> differentials.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  subroutine differentials(V,param,of,dOFdV,H)
    implicit none
    real, dimension(nc), intent(in) :: V !< Vapour mole numbers [mole]
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    real, dimension(nc,nc), intent(out) :: H !< Hessian matrix of objective function
    real, dimension(nc), intent(out) :: dOFdV !< Differential of objective function with respect to variables (V)
    real, intent(out) :: of !< Objective function value
    ! Locals
    real:: beta, t, p, betaL
    real, dimension(nc) :: FUGL,FUGV,L,X,Y,Z
    real, dimension(nc,nc) :: FUGVX,FUGLX
    integer :: i

    t = param(1)
    p = param(2)
    Z(1:nc) = param(3:nc+2)
    L = param(nc+3:2*nc+2)

    beta = sum(V)
    Y = V/beta
    betaL = sum(L)
    X = L/betaL

    call thermo(t,p,X,LIQPH,FUGL,lnFUGX=FUGLX) ! ln phi liquid
    call thermo(t,p,Y,VAPPH,FUGV,lnFUGX=FUGVX) ! ln phi vapour
    of = 0.0
    dOFdV = 0.0
    H = 1.0/(beta*betaL)*(-1.0 + beta*FUGLX + betaL*FUGVX)
    do i=1,nc
      if (Z(i) > zLimit) then
        of = of + V(i)*(log(Y(i))+FUGV(i)) + L(i)*(log(X(i))+FUGL(i))
        dOFdV(i) = log(Y(i)) - log(X(i)) + FUGV(i) - FUGL(i)
      else
        H(:,i) = 0.0
        H(i,:) = 0.0
      end if
    end do
    ! Add to the diagonal
    do i=1,nc
      if (Z(i) > zLimit) then
        H(i,i) = H(i,i) + Z(i)/(X(i)*Y(i)*beta*betaL)
      else
        H(i,i) = 1.0
      endif
    enddo
  end subroutine differentials

  !-------------------------------------------------------------------------
  !> Limit change in mole numbers, to maintain positive mole numbers.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  subroutine limitDV(V,param,dV)
    implicit none
    real, dimension(nc), intent(in) :: V !< Vapour mole numbers [mole]
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    real, dimension(nc), intent(inout) :: DV !< Calculated change in mole numbers [mole]
    ! Locals
    real, dimension(nc) :: Z, L
    real:: scale
    integer :: i

    Z(1:nc) = param(3:nc+2)
    L = param(nc+3:2*nc+2)
    scale = 1.0
    do i=1,nc
      if (V(i) < L(i)) then
        if (V(i) + DV(i) < 0.0) then
          scale = min(scale, -V(i)/DV(i))
        else if (Z(i) - V(i) - DV(i) < 0.0) then
          scale = min(scale, (Z(i)-V(i))/DV(i))
        endif
      else
        if (L(i) - DV(i) < 0.0) then
          scale = min(scale, L(i)/DV(i))
        else if (Z(i) - L(i) + DV(i) < 0.0) then
          scale = min(scale, (L(i)-Z(i))/DV(i))
        endif
      endif
    enddo
    if (scale < 1.0) then
      scale = scale * (1.0 - small)
      DV = DV*scale
    endif

  end subroutine limitDV

  !-------------------------------------------------------------------------
  !> No premature return for Gibbs minimization.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function prematureReturn(V,param,of,dofdV) result(doReturn)
    implicit none
    real, dimension(nc), intent(in)     :: V !< Vapour mole numbers [mole]
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    real, intent(in)                    :: of !< Objective function value
    real, dimension(nc), intent(in)     :: dofdV !< Differential of objective function
    logical                             :: doReturn !< Terminate minimization?
    !
    doReturn = .false.
  end function prematureReturn

  !-----------------------------------------------------------------------------
  !> Support for variable size in problem
  !>
  !> \author MHA, 2012-03-29
  !-------------------------------------------------------------------------
  function get_problem_size(param) result(nvar)
    implicit none
    real, dimension(2*nc+2), intent(in) :: param !< Parameter vector
    integer :: nvar

    nvar = nc

  end function get_problem_size

  !-------------------------------------------------------------------------
  !> Set mole numbers. Try to avoid truncation error.
  !>
  !> \author MH, 2013-02-27
  !-------------------------------------------------------------------------
  subroutine setV(n,nparam,V,dV,param,alpha)
    implicit none
    integer, intent(in) :: n, nparam !< Problem dimension
    real, dimension(n), intent(inout) :: V !< Vapour mole numbers [mole]
    real, dimension(n), intent(inout) :: dV !< Change in vapour mole numbers [mole]
    real, dimension(nparam), intent(inout) :: param !< Parameter vector
    real, intent(in) :: alpha !< dV scaling
    ! Locals
    real, dimension(n) :: L,Z
    integer :: i

    Z = param(3:n+2)
    L = param(n+3:2*n+2)

    do i=1,n
      if (V(i) >= L(i)) then
        L(i) = L(i) - alpha*dV(i)
        V(i) = Z(i) - L(i)
      else
        V(i) = V(i) + alpha*dV(i)
        L(i) = Z(i) - V(i)
      end if
    enddo

    param(n+3:2*n+2) = L

  end subroutine setV

end module tp_solver
