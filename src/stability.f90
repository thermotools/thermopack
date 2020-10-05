!-------------------------------------------------------------------------
!> Minimize reduced tangent plane distance
!>
!> \todo Add termination when approaching trivial solution.
!>
!-------------------------------------------------------------------------
module stability
  use thermopack_constants
  use thermopack_var, only: nc, nph, complist
  use numconstants, only: machine_prec, small
  use utilities
  use eos, only : thermo
  use thermo_utils, only: wilsonK
  implicit none
  private
  save

  !> Tolerance for the tangent plane minimization
  real :: rel_tol = machine_prec * 1.0e5
  !> Values > negative_lim is treated as zero or positive
  real, parameter :: stabilityLimit = -machine_prec * 1000.0
  !> Limit when we are sufficiently below zero to terminate
  real, parameter :: premature_terminate = -0.001
  !> Terminate if fraction of salts passes
  real, parameter :: elFractionLimit = 0.5

  public :: stabcalc, stabcalcW, stabilityLimit
  public :: set_stability_tolerance, get_stability_tolerance
  public :: checkVLEstability, tpd_fun

contains

  !-----------------------------------------------------------------------------
  !> Check if a feed is stable against vapor-liquid split.
  !>
  !> \author Ailo, 2016-12-21
  !-----------------------------------------------------------------------------
  subroutine checkVLEstability(t,p,Z,isStable,Wsol,new_phase)
    implicit none
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, dimension(nc), intent(in) :: Z !< Trial composition (Overall compozition)
    logical, intent(out) :: isStable !< Is the solution trivial? (That is; W == Z)
    real, dimension(nc), intent(out) :: Wsol !< Solution.
    integer, intent(out) :: new_phase
    ! Locals
    real :: tpd, LNPHIZ(nc), LNPHIV(nc), LNPHIL(nc), LNPHIO(nc)
    logical :: vaporIsMostStable

    call thermo(t,p,Z,VAPPH,LNPHIV)
    call thermo(t,p,Z,LIQPH,LNPHIL)

    vaporIsMostStable = dot_product(z,LNPHIV) < dot_product(z,LNPHIL)
    if (vaporIsMostStable) then
      LNPHIZ = LNPHIV
      new_phase = LIQPH
    else
      LNPHIZ = LNPHIL
      new_phase = VAPPH
    end if

    tpd = stabcalc(t,p,Z,new_phase,LNPHIZ,LNPHIO,Wsol)
    isStable = tpd > stabilityLimit
    if (.not. isStable) return

    ! Have to check the possibility of the other phase then..
    if (vaporIsMostStable) then
      new_phase = VAPPH
    else
      new_phase = LIQPH
    end if

    tpd = stabcalc(t,p,Z,new_phase,LNPHIZ,LNPHIO,Wsol)
    isStable = tpd > stabilityLimit

  end subroutine checkVLEstability

  !-----------------------------------------------------------------------------
  !> Calculate minimum reduced tangent plane distance.
  !>
  !> Set starting compozition based on phase flag.
  !>
  !> Note that the FUGZ must contain the single phase fugasities
  !> with the lowest gibbs energy when calling this function.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function stabcalc(t,p,Z,phase,FUGZ,FUG,Wsol,preTermLim) result(tpd)
    implicit none
    integer, intent(in) :: phase !< Phase flag for compozition initiation (the
                                 !phase to search for). Initiate a vapour
                                 !compzition or a liquid composition.
    real, dimension(nc), intent(in) :: Z !< Trial composition (Overall compozition)
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, dimension(nc), intent(in) :: FUGZ !< Sigle phase fagasities for Z. Minimum gibbs solution.
    real, dimension(nc), intent(out) :: FUG !< The fugasities at the solution, W.
    real, optional, dimension(nc), intent(out) :: Wsol !< Solution.
    real, optional, intent(in) :: preTermLim !< Control when solver terminates prematurely
    real :: tpd !< Tangent plane distance
    ! Locals
    integer, parameter :: nd = 1, j = 1
    real, dimension(nc) :: W, K
    real, dimension(nd,nc) :: XX
    integer :: ph, i

    ph = phase

    ! Calculate starting values
    if (phase < 0) then ! WATER or NONWATER
      ph = LIQPH
      call wilsonK(t,p,K,liqType=phase)
    else
      call wilsonK(t,p,K)
    endif

    do i=1,nc
      if (complist(i) == "HE") then
        ! Make Helium more volatile than predicted by Wilson-K values
        K(i) = (226970.0/p)*exp(5.373*(1-5.19/t))
      endif
    enddo
    if (phase == VAPPH) then
      W = K * Z
    else
      W = Z / K
    endif

    XX(1,:) = Z
    tpd = stabcalcW(nd,j,t,p,XX,W,ph,FUGZ,FUG,preTermLim)
    if (present(Wsol)) then
      Wsol = W
    endif
  end function stabcalc

  !-------------------------------------------------------------------------
  !> Calculate minimum reduced tangent plane distance.
  !!
  !!  \f$ tpdm(W) = 1 + \sum W_i (\ln W_i + \ln \varphi - d_i - 1) \f$.
  !!
  !!  Where the sum is over all components.
  !!
  !! Start by doing 3 iterations with successive substitution,
  !! if not converged, switch to a modefied newton minimizer.
  !!
  !! Note that the FUGZ must contain the single phase fugasities
  !! with the lowest gibbs energy when calling this function.
  !!
  !! Initial compozition guess for W must also be supplied.
  !!
  !! \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function stabcalcW(nd,k,t,p,XX,W,phase,FUGZ,FUGW,preTermLim) result(tpd)
    use optimizers, only: optimize, optim_param, setX
    use thermopack_var, only: ncsym
    implicit none
    integer, intent(in) :: nd !< Dimension of compozition matrix
    integer, intent(in) :: k !< Index of Trial phase
    integer, intent(in) :: phase !< Phase flag. Determine what root we are calculating in the eos.
    real, dimension(nd,nc), intent(in) :: XX !< All phases in equilibrium
    real, dimension(nc), intent(inout) :: W !< Inital guess
    real, dimension(nc), intent(in) :: FUGZ !< Fugacity of trial phase
    real, intent(in) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, dimension(nc), intent(out) :: FUGW !< The fugasities at the solution, W. 
    real, optional, intent(in) :: preTermLim !< Control when solver terminates prematurely
    real :: tpd
    ! Locals
    real, dimension(nc) :: D,FUGW_old, Z, Alpha
    integer :: iter,i
    real :: error, wz_error
    type(optim_param) :: optim
    real, dimension(3*nc+5) :: param
    real, dimension(nc) :: dtpddWi
    real :: r, dtpdds, r_old, prematureTermLim, elFraction
    logical :: r_terminate

    if (present(preTermLim)) then
      prematureTermLim = preTermLim
    else
      prematureTermLim = premature_terminate
    endif
    elFraction = 0.0

    if (verbose) then
      write(*,*) "Entering stabcalcW. Phase flag = ", phase
      write(*,*) "Initial W =",W
    endif
    Z = XX(k,:)
    D = 0.0
    do i=1,nc
      if (Z(i) > 0.0) then
        D(i) =log(Z(i))+FUGZ(i)
      endif
    enddo

    FUGW = 0.0
    error = 1.0
    r = 1.0
    r_terminate = .false.
    if (verbose) write(*,*) "Attempting 3 iterations of successive substitution."
    do iter=1,3
      if (verbose) then
        write(*,*) "Iteration: ",iter
        write(*,*) "W =",W
      end if
      FUGW_old = FUGW
      call thermo(t,p,W,phase,FUGW)
      if (verbose) write(*,*) "FUGW =",FUGW
      tpd = tpd_fun(W,FUGW,D)
      if (tpd < prematureTermLim) then
        if (verbose) write(*,*) "Premature termination. Found negative tpd."
        exit ! Have found composition with lower gibbs energy
      endif
      ! Calculate differential dtpddWi
      do i=1,nc
        if (W(i) > 0.0) then
          dtpddWi(i) = log(W(i)) + FUGW(i) - D(i)
        else
          dtpddWi(i) = 0.0
        endif
      enddo
      dtpdds = sum((W-Z)*dtpddWi)
      r_old = r
      r = 2.0*tpd/max(dtpdds,machine_prec*machine_prec)
      ! if (i > 1 .and. r > r_old .and. r > 0.8) then
      !   r_terminate = .true.
      !   exit
      ! endif

      error = 0.0
      do i=1,nc
        if (abs(FUGW_old(i)) < 100*rel_tol) then
          error = max(error,abs((FUGW(i)-FUGW_old(i))))
          if (verbose) write(*,*) "Error(abs) =",error
        else
          error = max(error,abs((FUGW(i)-FUGW_old(i))/FUGW_old(i)))
          if (verbose) write(*,*) "Error(rel) =",error
        endif
      enddo
      if (error < rel_tol) then
        if (verbose) write(*,*) "Successive substitution converged"
        exit
      endif
      ! Update W
      do i=1,nc
        if (Z(i) > 0.0) then
          W(i) = exp(D(i)-FUGW(i))
        endif
      enddo
      if (ncsym < nc) then
        elFraction = sum(w(ncsym+1:nc))/sum(w)
        if (elFraction > elFractionLimit) then
          tpd = 1.0
          error = 0.0
          exit
        endif
      endif
    enddo
    if (verbose) write(*,*) "Successive substitution done"
    if (error > rel_tol .and. tpd >= prematureTermLim .and. .not. r_terminate) then
      if (verbose) then 
        write(*,*) "Successive substitution failed to converge or find negative tpd."
        write(*,*) "Attempting Newton search."
      endif
      ! Need second order search
      Alpha = 2.0 * sqrt(W) ! Transform from W to alpha
      param(1) = t
      param(2) = p
      param(3) = REAL(phase)
      param(4:nc+3) = D
      param(2*nc+4:3*nc+3) = Z
      param(3*nc+4) = r
      param(3*nc+5) = prematureTermLim
      optim%rel_tol = rel_tol
      !optim%gradient_termination = .true.
      call optimize(optim,objective,differentials,Alpha,param,&
           limit_dAlpha,prematureStabilityReturn,get_problem_size,setX)
      FUGW = param(nc+4:2*nc+3)
      tpd = optim%of
      W = Alpha*Alpha / 4.0
    endif

    call normalize(W)
    wz_error = 1.0
    do i=1,nd
      Z = XX(i,:)
      call normalize(Z)
      wz_error = min(wz_error, maxval(abs(Z-W)))
    enddo
    if (verbose) then
      write(*,*) "Exiting stabcalcW. W =",W
      write(*,*) "FUGW =",FUGW
    endif

  end function stabcalcW

  !-------------------------------------------------------------------------
  !> Calculate reduced tangent plane distance.
  !!
  !!  \f$ tpdm(W) = 1 + \sum W_i (\ln W_i + \ln \varphi - d_i - 1) \f$.
  !!
  !!  Where the sum is over all components.
  !!
  !! \author MH, 2014-10-27
  !-------------------------------------------------------------------------
  function tpd_fun(W,FUGW,D) result(tpd)
    implicit none
    real, dimension(nc), intent(in) :: W !< Composition vector
    real, dimension(nc), intent(in) :: D !<
    real, dimension(nc), intent(in) :: FUGW !< The fugasities at the solution, W.
    real :: tpd
    ! Locals
    integer :: i

    tpd = 1.0
    do i=1,nc
      if (W(i) > 0.0) then
        tpd = tpd + W(i)*(log(W(i))+FUGW(i)-D(i)-1.0)
      endif
    enddo
  end function tpd_fun

  !-------------------------------------------------------------------------
  !> Calculate minimum reduced tangent plane distance.
  !!
  !!  \f$ tpdm(W) = 1 + \sum W_i (\ln W_i + \ln \varphi - d_i - 1) \f$.
  !!
  !!  Where the sum is over all components.
  !!
  !! \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function objective(Alpha, param) result(tpd)
    implicit none
    real, dimension(3*nc+5), intent(inout) :: param !< Parameter vector
    real, dimension(nc), intent(in) :: Alpha !< Transformation for mole numbers [mole^(1/2)]
    real :: tpd !< Tangent plane distance
    ! Locals
    real, dimension(nc) :: W, D, FUG, Z
    integer :: phase, i
    real :: t,p
    t = param(1)
    p = param(2)
    phase = INT(param(3))
    D = param(4:nc+3)
    Z=param(2*nc+4:3*nc+3)

    W = Alpha*Alpha / 4.0
    call thermo(t,p,W,phase,FUG)
    !tpd = 1.0 + sum(W*(log(W)+FUG-D-1.0))
    tpd = 1.0
    do i=1,nc
      if (W(i) > zLimit) then
        tpd = tpd + W(i)*(log(W(i))+FUG(i)-D(i)-1.0)
      endif
    enddo
    param(nc+4:2*nc+3) = FUG
  end function objective

  !-------------------------------------------------------------------------
  !> Calculate minimum reduced tangent plane distance, and its differentials.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  subroutine differentials(Alpha,param,tpd,G,H)
    implicit none
    real, dimension(3*nc+5), intent(inout) :: param !< Parameter vector
    real, dimension(nc), intent(in) :: Alpha !< Transformation for mole numbers [mole^(1/2)]
    real, dimension(nc,nc), intent(out) :: H !< Tangent plane distance hessian
    real, dimension(nc), intent(out) :: G !< Tangent plane distance differential
    real, intent(out) :: tpd !< Tangent plane distance
    ! Locals
    real, dimension(nc,nc) :: FUGX
    real, dimension(nc) :: W, D, FUG, Z
    integer :: i,j, phase
    real :: t,p

    t = param(1)
    p = param(2)
    phase = INT(param(3))
    D = param(4:nc+3)
    Z=param(2*nc+4:3*nc+3)

    W = Alpha*Alpha/4.0 ! Transform from alpha to W
    call thermo(t,p,W,phase,FUG,lnfugx=FUGX)
    param(nc+4:2*nc+3) = FUG
    tpd = 1.0
    do i=1,nc
      if (W(i) > 0.0) then
        tpd = tpd + W(i)*(log(W(i))+FUG(i)-D(i)-1.0)
        G(i) = sqrt(W(i))*(log(W(i)) + FUG(i) - D(i))
      else
        G(i) = 0.0
      endif
    enddo
    !tpd = 1.0 + sum(W*(log(W)+FUG-D-1.0))
    !G = sqrt(W)*(log(W) + FUG - D)
    H = FUGX/sum(W)
    do i=1,nc
      if (Z(i) < zLimit) then
        H(i,:) = 0.0
        H(:,i) = 0.0
      endif
      do j=1,nc
        H(i,j) = H(i,j)*sqrt(W(i)*W(j))
      enddo
      H(i,i) = H(i,i) + 1.0
    enddo

  end subroutine differentials

  !-------------------------------------------------------------------------
  !> Limit change in mole numbers, to maintain positive mole numbers.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  subroutine limit_dAlpha(Alpha,param,dAlpha)
    implicit none
    real, dimension(3*nc+5), intent(in) :: param !< Parameter vector
    real, dimension(nc), intent(in) :: Alpha !< Transformation for mole numbers [mole^(1/2)]
    real, dimension(nc), intent(inout) :: dAlpha !< Calculated change in transformed mole numbers [mole^(1/2)]
    ! Locals
    real:: s
    integer :: i

    s = 1.0
    do i=1,nc
      if (Alpha(i) + dAlpha(i) < 0.0) then
        s = min(s, -Alpha(i)/dAlpha(i)*(1.0 - small))
      else if (Alpha(i) + dAlpha(i) == 0.0) then
        s = min(s, 1.0 - small)
      endif
    enddo
    if (s < 1.0) then
      dAlpha = dAlpha*s
    endif
  end subroutine limit_dAlpha

  !-------------------------------------------------------------------------
  !> Terminate optimization prematurely when the tangent plane
  !> is below the gibbs function.
  !>
  !> \author MHA, 2012-01-30
  !-------------------------------------------------------------------------
  function prematureStabilityReturn(Alpha,param,tpd,dofdAlpha) result(doReturn)
    use eos, only: thermo
    implicit none
    real, dimension(nc), intent(in)        :: Alpha !< Transformation for mole numbers [mole^(1/2)]
    real, dimension(3*nc+5), intent(inout) :: param !< Parameter vector
    real, intent(in)                       :: tpd !< Tangent plane distance
    real, dimension(nc), intent(in)        :: dofdAlpha !< Differential of objective function
    logical                                :: doReturn !< Terminate minimization?
    !
    ! Locals
    real :: r, r_old, dtpdds, prematureTermLim
    real, dimension(nc) :: W, D, FUGW, dtpddWi, Z
    integer :: i
    doReturn = .false.
    prematureTermLim = param(3*nc+5)
    if (tpd < prematureTermLim) then
      doReturn = .true.
    else
      W = Alpha*Alpha / 4.0
      D = param(4:nc+3)
      r_old = param(3*nc+4)
      Z = param(2*nc+4:3*nc+3)
      FUGW = param(nc+4:2*nc+3)
      ! Calculate differential dtpddWi
      do i=1,nc
        if (W(i) > 0.0) then
          dtpddWi(i) = log(W(i)) + FUGW(i) - D(i)
        else
          dtpddWi(i) = 0.0
        endif
      enddo
      dtpdds = sum((W-Z)*dtpddWi)
      r = 2.0*tpd/max(dtpdds,machine_prec*machine_prec)
      param(3*nc+4) = r
      ! if (r > r_old .and. r > 0.8) then
      !   doReturn = .true.
      ! endif
    endif
  end function prematureStabilityReturn

  !-------------------------------------------------------------------------
  !> Support for variable size in problem 
  !> 
  !> \author MHA, 2012-03-29
  !-------------------------------------------------------------------------
  function get_problem_size(param) result(nvar)
    implicit none
    real, dimension(3*nc+5), intent(in) :: param !< Parameter vector
    integer :: nvar

    nvar = nc

  end function get_problem_size

  !-------------------------------------------------------------------------
  !> Set tolerance for stability solver
  !>
  !> \author MH, 2014-11
  !-------------------------------------------------------------------------
  subroutine set_stability_tolerance(relativeTolerance)
    implicit none
    real, intent(in) :: relativeTolerance !< Relative tolerance for staility solver

    rel_tol = relativeTolerance

  end subroutine set_stability_tolerance

  !-------------------------------------------------------------------------
  !> Get tolerance for stability solver
  !>
  !> \author MH, 2014-11
  !------------------------------------------------------------------------ -
  function get_stability_tolerance() result(relativeTolerance)
    implicit none
    real :: relativeTolerance !< Relative tolerance for staility solver

    relativeTolerance = rel_tol

  end function get_stability_tolerance

end module stability
