!> This module handles all data and routines related to association.
module saft_association
  use assocschemeutils, only: noSitesFlag, site_to_compidx, compidx_to_sites
  use thermopack_constants, only: Rgas => Rgas_default
  use thermopack_var, only: base_eos_param, get_active_eos, numAssocSites
  use association_var, only: association
  implicit none
  save

  ! Choice of combining rule for cross-association Delta
  integer, parameter :: STANDARD=1
  integer, parameter :: ELLIOT=2
  integer :: DELTA_COMBRULE = STANDARD
contains

  !> Calculate Boltzmann factor for association energy, with caching
  subroutine calc_boltzmann_fac(assoc, T, boltzmann_fac)
    type(association), intent(inout) :: assoc
    real, intent(in) :: T
    real, intent(out) :: boltzmann_fac(numAssocSites, numAssocSites)
    integer :: k, l
    do k=1, numAssocSites
      do l=k, numAssocSites
        if (assoc%eps_kl(k,l)>0) then
          boltzmann_fac(k,l) = exp(assoc%eps_kl(k,l)/(Rgas*T))
        else
          boltzmann_fac(k,l) = 1
        end if
        boltzmann_fac(l,k) = boltzmann_fac(k,l)
      end do
    end do
  end subroutine calc_boltzmann_fac

  !> Assemble Delta^{kl} matrix, and derivatives if wanted. Can be optimized
  !> e.g. by not calculating the exponential in every loop iteration
  subroutine Delta_kl(eos,nc,T,V,n,Delta,Delta_T,Delta_V,Delta_n,&
       Delta_TT,Delta_TV,Delta_Tn,Delta_VV,Delta_Vn,Delta_nn)
    use saft_globals, only: assoc_covol_binary, eosSAFT_VR_MIE
    use saft_rdf
    ! Input.
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: n(nc)
    ! Output.
    real, dimension(numAssocSites,numAssocSites), intent(out) :: Delta
    real, intent(out), optional :: Delta_T(numAssocSites,numAssocSites)
    real, intent(out), optional :: Delta_V(numAssocSites,numAssocSites)
    real, intent(out), optional :: Delta_n(numAssocSites,numAssocSites,nc)
    real, intent(out), optional :: Delta_TT(numAssocSites,numAssocSites)
    real, intent(out), optional :: Delta_TV(numAssocSites,numAssocSites)
    real, intent(out), optional :: Delta_VV(numAssocSites,numAssocSites)
    real, intent(out), optional :: Delta_Tn(numAssocSites,numAssocSites,nc)
    real, intent(out), optional :: Delta_Vn(numAssocSites,numAssocSites,nc)
    real, intent(out), optional :: Delta_nn(numAssocSites,numAssocSites,nc,nc)
    ! Locals.
    real :: g, g_V, g_VV
    real :: g_T, g_TT, g_TV, g_Tn(nc)
    real :: g_n(nc), g_Vn(nc), g_nn(nc,nc)
    real :: h, h_T, h_TT
    integer :: ic,jc
    integer :: k,l
    integer :: ii, jj, k1, k2, l1, l2
    real :: d1, d2
    real :: covol, expo
    logical :: fir_der_present, sec_der_present
    real :: boltzmann_fac(numAssocSites, numAssocSites)
    integer :: difflevel
    type(association), pointer :: assoc
    assoc => eos%assoc

    fir_der_present = present(Delta_T) .or. present(Delta_V) .or. present(Delta_n)
    sec_der_present = present(Delta_TT) .or. present(Delta_TV) .or. &
         present(Delta_Tn) .or. present(Delta_VV) .or. &
         present(Delta_Vn) .or. present(Delta_nn)

    Delta = 0.0
    if (present(Delta_T)) Delta_T = 0.0
    if (present(Delta_V)) Delta_V = 0.0
    if (present(Delta_n)) Delta_n = 0.0
    if (present(Delta_TT)) Delta_TT = 0.0
    if (present(Delta_TV)) Delta_TV = 0.0
    if (present(Delta_VV)) Delta_VV = 0.0
    if (present(Delta_Tn)) Delta_Tn = 0.0
    if (present(Delta_Vn)) Delta_Vn = 0.0
    if (present(Delta_nn)) Delta_nn = 0.0

    if (sec_der_present) then
       call master_saft_rdf(eos,nc,T,V,n,1,1,g,g_T,g_V,g_n,&
            g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
       difflevel = 2
    else if (fir_der_present) then
       call master_saft_rdf(eos,nc,T,V,n,1,1,g,g_T,g_V,g_n)
       difflevel = 1
    else
       call master_saft_rdf(eos,nc,T,V,n,1,1,g)
       difflevel = 0
    end if

    ! Assemble Delta matrix.
    call calc_boltzmann_fac(assoc, T, boltzmann_fac)

    if (assoc%saft_model/=eosSAFT_VR_MIE) then
       call master_saft_rdf(eos,nc,T,V,n,1,1,g,g_T,g_V,g_n,g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
    end if

    do k = 1,numAssocSites
       do l = k,numAssocSites

          ic = site_to_compidx(assoc,k)
          jc = site_to_compidx(assoc,l)
          if (DELTA_COMBRULE==ELLIOT .and. jc/=ic) cycle
          if (assoc%saft_model==eosSAFT_VR_MIE) then
             call master_saft_rdf(eos,nc,T,V,n,ic,jc,g,g_T,g_V,g_n,g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
          end if

          covol = assoc_covol_binary(ic,jc)
          expo = boltzmann_fac(k,l)

          h = assoc%beta_kl(k,l)*covol*(expo-1)
          h_T = -assoc%eps_kl(k,l)*expo*covol*assoc%beta_kl(k,l)/(Rgas*T*T)

          Delta(k,l) = g*h
          Delta(l,k) = Delta(k,l)

          if (present(Delta_T)) then
             Delta_T(k,l) = g_T*h + g*h_T
             Delta_T(l,k) = Delta_T(k,l)
          end if

          if (present(Delta_V)) then
             Delta_V(k,l) = g_V*h
             Delta_V(l,k) = Delta_V(k,l)
          end if

          if (present(Delta_n)) then
             Delta_n(k,l,:) = g_n*h
             Delta_n(l,k,:) = Delta_n(k,l,:)
          end if

          if (present(Delta_TT)) then
             h_TT = (2+assoc%eps_kl(k,l)/(Rgas*T))*&
                  assoc%eps_kl(k,l)/(Rgas*T**3)*expo*assoc%beta_kl(k,l)*covol
             Delta_TT(k,l) = g_TT*h + 2*g_T*h_T + g*h_TT
             Delta_TT(l,k) = Delta_TT(k,l)
          end if

          if (present(Delta_TV)) then
             Delta_TV(k,l) = g_TV*h + g_V*h_T
             Delta_TV(l,k) = Delta_TV(k,l)
          end if

          if (present(Delta_Tn)) then
             Delta_Tn(k,l,:) = g_Tn*h + g_n*h_T
             Delta_Tn(l,k,:) = Delta_Tn(k,l,:)
          end if

          if (present(Delta_VV)) then
             Delta_VV(k,l) = g_VV*h
             Delta_VV(l,k) = Delta_VV(k,l)
          end if

          if (present(Delta_Vn)) then
             Delta_Vn(k,l,:) = g_Vn*h
             Delta_Vn(l,k,:) = Delta_Vn(k,l,:)
          end if

          if (present(Delta_nn)) then
             Delta_nn(k,l,:,:) = g_nn*h
             Delta_nn(l,k,:,:) = Delta_nn(k,l,:,:)
          end if
       end do
    end do

    if (DELTA_COMBRULE==ELLIOT) then
       do ic=1,nc
          do jc=ic+1,nc
             call compidx_to_sites(assoc,ic,k1,k2)
             call compidx_to_sites(assoc,jc,l1,l2)
             if (k1<0 .or. l1<0) cycle
             if (assoc%beta_kl(k1,k2)==0.0 .or. assoc%beta_kl(l1,l2)==0.0) then
                stop "Delta_kl::Need more general implementation of Elliot combining rule"
             end if
             do k=k1, k2
                do l=l1, l2
                   if (.not. assoc%beta_kl(k,l)>0) cycle ! Do sites have the same polarity?
                   Delta(k,l) = sqrt(Delta(k1,k2)*Delta(l1,l2))
                   Delta(l,k) = Delta(k,l)

                   if (present(Delta_T)) then
                      Delta_T(k,l) = (Delta_T(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_T(l1,l2))/(2*Delta(k,l))
                      Delta_T(l,k) = Delta_T(k,l)
                   end if

                   if (present(Delta_V)) then
                      Delta_V(k,l) = (Delta_V(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_V(l1,l2))/(2*Delta(k,l))
                      Delta_V(l,k) = Delta_V(k,l)
                   end if

                   if (present(Delta_n)) then
                      Delta_n(k,l,:) = (Delta_n(k1,k2,:)*Delta(l1,l2) + Delta(k1,k2)*Delta_n(l1,l2,:))/(2*Delta(k,l))
                      Delta_n(l,k,:) = Delta_n(k,l,:)
                   end if

                   if (present(Delta_TT)) then
                      d1 = (Delta_TT(k1,k2)*Delta(l1,l2) + 2*Delta_T(k1,k2)*Delta_T(l1,l2) + &
                           Delta(k1,k2)*Delta_TT(l1,l2))/(2*Delta(k,l))
                      d2 = -(Delta_T(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_T(l1,l2))**2/(4*Delta(k,l)**3)
                      Delta_TT(k,l) = d1+d2
                      Delta_TT(l,k) = Delta_TT(k,l)
                   end if

                   if (present(Delta_TV)) then
                      d1 = (Delta_TV(k1,k2)*Delta(l1,l2) + Delta_T(k1,k2)*Delta_V(l1,l2) + Delta_V(k1,k2)*Delta_T(l1,l2) + &
                           Delta(k1,k2)*Delta_TV(l1,l2))/(2*Delta(k,l))
                      d2 = -(Delta_T(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_T(l1,l2))* &
                           (Delta_V(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_V(l1,l2))/(4*Delta(k,l)**3)
                      Delta_TV(k,l) = d1+d2
                      Delta_TV(l,k) = Delta_TV(k,l)
                   end if

                   if (present(Delta_Tn)) then
                      do ii=1,nc
                         d1 = (Delta_Tn(k1,k2,ii)*Delta(l1,l2) + &
                              Delta_T(k1,k2)*Delta_n(l1,l2,ii) + Delta_n(k1,k2,ii)*Delta_T(l1,l2) + &
                              Delta(k1,k2)*Delta_Tn(l1,l2,ii))/(2*Delta(k,l))
                         d2 = -(Delta_T(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_T(l1,l2))* &
                              (Delta_n(k1,k2,ii)*Delta(l1,l2) + Delta(k1,k2)*Delta_n(l1,l2,ii))/(4*Delta(k,l)**3)
                         Delta_Tn(k,l,ii) = d1+d2
                      end do
                      Delta_Tn(l,k,:) = Delta_Tn(k,l,:)
                   end if

                   if (present(Delta_VV)) then
                      d1 = (Delta_VV(k1,k2)*Delta(l1,l2) + 2*Delta_V(k1,k2)*Delta_V(l1,l2) + &
                           Delta(k1,k2)*Delta_VV(l1,l2))/(2*Delta(k,l))
                      d2 = -(Delta_V(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_V(l1,l2))**2/(4*Delta(k,l)**3)
                      Delta_VV(k,l) = d1+d2
                      Delta_VV(l,k) = Delta_VV(k,l)
                   end if

                   if (present(Delta_Vn)) then
                      do ii=1,nc
                         d1 = (Delta_Vn(k1,k2,ii)*Delta(l1,l2) + &
                              Delta_V(k1,k2)*Delta_n(l1,l2,ii) + Delta_n(k1,k2,ii)*Delta_V(l1,l2) + &
                              Delta(k1,k2)*Delta_Vn(l1,l2,ii))/(2*Delta(k,l))
                         d2 = -(Delta_V(k1,k2)*Delta(l1,l2) + Delta(k1,k2)*Delta_V(l1,l2))* &
                              (Delta_n(k1,k2,ii)*Delta(l1,l2) + Delta(k1,k2)*Delta_n(l1,l2,ii))/(4*Delta(k,l)**3)
                         Delta_Vn(k,l,ii) = d1+d2
                      end do
                      Delta_Vn(l,k,:) = Delta_Vn(k,l,:)
                   end if

                   if (present(Delta_nn)) then
                      do ii=1,nc
                         do jj=1,nc
                            d1 = (Delta_nn(k1,k2,ii,jj)*Delta(l1,l2) + &
                                 Delta_n(k1,k2,ii)*Delta_n(l1,l2,jj) + Delta_n(k1,k2,jj)*Delta_n(l1,l2,ii) + &
                                 Delta(k1,k2)*Delta_nn(l1,l2,ii,jj))/(2*Delta(k,l))
                            d2 = -(Delta_n(k1,k2,ii)*Delta(l1,l2) + Delta(k1,k2)*Delta_n(l1,l2,ii))*&
                                 (Delta_n(k1,k2,jj)*Delta(l1,l2) + Delta(k1,k2)*Delta_n(l1,l2,jj))/(4*Delta(k,l)**3)
                            Delta_nn(k,l,ii,jj) = d1+d2
                         end do
                      end do
                      Delta_nn(l,k,:,:) = Delta_nn(k,l,:,:)
                   end if

                end do
             end do
          end do
       end do
    end if
  end subroutine Delta_kl

  ! !> Calculate temperature-dependent association volume from the range of
  ! !> association rc. This is one approach to handling association for the
  ! !> SAFT-VR Mie EoS, and is described in Lafitte et al. (2013).
  ! subroutine assoc_vol_from_rc (rc, d)
  !   real, intent(in) :: rc !< Range of association
  !   real, intent(in) :: d !< BH diameter
  !   real, intent(out) :: kappa !< association volume (also known as beta)
  !   real :: rd
  !   rd = 0.4*sigma

  !   if (numAssocSites == 0) call stoperror("No associating components.")

  !   kappa = (4*PI*d**2)/(72*rc**2*sigma**3)
  !   kappa = kappa*(log((rc+2*rd)/d))*(6*rc**3+18*rc**2*rd-24*rd**3)

  ! end subroutine assoc_vol_from_rc


  !> Assemble the m vector from Michelsen paper, holding the number of moles of
  !> each association site.
  subroutine assemble_m_mich_k (assoc,nc,n,m_mich_k)
    integer, intent(in) :: nc
    real, intent(in) :: n(nc)                     !< Component mole numbers.
    real, intent(out) :: m_mich_k(numAssocSites) !< Michelsen m vector.
    integer :: ic
    type(association), intent(in) :: assoc
    if (numAssocSites == 0) call stoperror("No associating components.")

    do ic=1,nc
       if ( assoc%comp_vs_sites(ic,1) /= noSitesFlag ) then
          m_mich_k(assoc%comp_vs_sites(ic,1):assoc%comp_vs_sites(ic,2)) = n(ic)
       end if
    end do

  end subroutine assemble_m_mich_k

  !> Computes the K matrix from Michelsen paper. Needed when solving for X.
  subroutine K_mich (eos,nc,T,V,n,K_mich_kl,m_opt,Delta_opt)
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: n(nc) !< Component mole numbers.
    real, intent(out) :: K_mich_kl(numAssocSites,numAssocSites)
    real, intent(in), optional :: m_opt(numAssocSites)                !< Optional input m_mich_k, in case it has already been computed.
    real, intent(in), optional :: Delta_opt(numAssocSites,numAssocSites) !< Optional input Delta_kl, in case it has already been computed.
    ! Locals.
    real, dimension(numAssocSites) :: m_mich_k
    real, dimension(numAssocSites,numAssocSites) :: Delta
    integer :: k,l

    if (present(m_opt)) then
      m_mich_k = m_opt
    else
      call assemble_m_mich_k (eos%assoc,nc,n,m_mich_k)
    end if

    if (present(Delta_opt)) then
      Delta = Delta_opt
    else
      call Delta_kl(eos,nc,T,V,n,Delta)
    end if

    do k=1,numAssocSites
      do l=k,numAssocSites
        K_mich_kl(k,l) = m_mich_k(k)*m_mich_k(l)*Delta(k,l)/V
        K_mich_kl(l,k) = K_mich_kl(k,l)
      end do
    end do

  end subroutine K_mich

  !> Compute the value of X_k consistent with (T,V,n) stored in param.
  subroutine solve_for_X_k(eos,nc,param,X_k,maxit,tol)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, premReturn, setXv
    use numconstants, only: machine_prec
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, dimension(2+nc), intent(inout) :: param
    real, dimension(numAssocSites), intent(inout) :: X_k
    integer, intent(in), optional :: maxit !< Maximum number of iterations.
    real, intent(in), optional :: tol      !< Tolerance.
    ! Locals.
    real :: xmin(numAssocSites), xmax(numAssocSites)
    type(nonlinear_solver) :: solver
    integer :: k
    real :: m_mich_k(numAssocSites)

    type(association), pointer :: assoc
    assoc => eos%assoc

    ! Special considerations in the case that one of the mole numbers are
    ! zero. In that case, the jacobian in the Newton solver can't be inverted,
    ! and we use successive substitution instead.
    if (minval(param(3:)) < 1e-20) then
      call assemble_m_mich_k (eos%assoc,nc,n=param(3:),m_mich_k=m_mich_k)
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          ! Set the nonphysical X_k-components to 1.0.
          X_k(k) = 1.0
        end if
      end do
    end if

    ! Explicit solution for the case of a single associating component
    ! if (assoc%numAssocSites==1) then
    !    cidx = assoc%compIdcs(1)

    ! end if

    if (present(maxit)) then
      solver%max_it = maxit
    else
      solver%max_it = 15
    end if

    solver%limit_x_values = .true.  ! don't take too large steps
    solver%verbose = .false.        ! for debugging
    solver%symmetric_jac = .true.   ! since it is really a hessian matrix

    if (present(tol)) then
      solver%rel_tol = tol               ! set the solver tolerance for the...
      solver%abs_tol = tol               ! ...inner loop fairly loose
    else
      solver%rel_tol = 1e8*machine_prec  ! default relative tolerance
      solver%abs_tol = 1e8*machine_prec  ! default absolute tolerance
    end if

    ! Solve for non-bonded site fractions X_k.
    xmin = 0.0
    xmax = 1.0

    call nonlinear_solve(solver=solver,fun=fun,jac=jac,hess=hess,limit=limit,&
         premterm=premReturn,setXvar=setXv,x=X_k,xmin=xmin,xmax=xmax,param=param)

    if (solver%exitflag /= 0) then
       X_k = 0.2
      call succ_subs (eos,nc,T=param(1),V=param(2),n=param(3:),X=X_k,n_iter=300)
    end if

  end subroutine solve_for_X_k

  !> Assembles the param struct, which ultimately is needed due to the input
  !> structure of nonlinear_solve.
  function assemble_param(T,V,n,nc)
    ! Input.
    integer, intent(in) :: nc
    real, intent(in)    :: T,V,n(nc)
    ! Output.
    real :: assemble_param(2+nc)
    ! Assembly.
    assemble_param(1) = T
    assemble_param(2) = V
    assemble_param(3:(nc+2)) = n
  end function assemble_param

  subroutine succ_subs (eos,nc,T,V,n,X,n_iter)
    use numconstants, only: machine_prec
    class(base_eos_param), intent(in) :: eos
    integer, intent(in)  :: nc
    real, intent(in)     :: T,V
    real, intent(in)     :: n(nc)
    real, intent(inout)  :: X(numAssocSites)
    integer, intent(in)  :: n_iter
    ! Locals.
    integer :: i
    real :: Xold(numAssocSites)

    do i=1,n_iter
       Xold = X
       call fun_succ_subst (eos,T,V,n,X)
       if ( maxval(abs(X-Xold)) < 1e6*machine_prec ) then
          return
       end if
    end do
  end subroutine succ_subs


  !> Computes the derivatives of X. Assumes that X is known.
  subroutine X_derivatives_knowing_X (eos,nc,T,V,n,X,X_T,X_V,X_n,&
       X_TT,X_TV,X_VV,X_Tn,X_Vn,X_nn)
    ! Input.
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, intent(in)            :: T, V, n(nc), X(numAssocSites)
    ! Output.
    real, intent(out), optional :: X_T(numAssocSites), X_V(numAssocSites), X_n(numAssocSites,nc)
    real, intent(out), optional :: X_TT(numAssocSites), X_TV(numAssocSites)
    real, intent(out), optional :: X_VV(numAssocSites),X_Tn(numAssocSites,nc)
    real, intent(out), optional :: X_Vn(numAssocSites,nc),X_nn(numAssocSites,nc,nc)
    ! Locals.
    real   :: m_mich_k(numAssocSites)
    real   :: Q_XX(numAssocSites,numAssocSites), Q_XX_temp(numAssocSites,numAssocSites)
    real   :: Q_XV(numAssocSites), Q_XT(numAssocSites)
    real   :: Q_Xn(numAssocSites,nc)
    real   :: ipiv(numAssocSites)
    real   :: work(3*(numAssocSites))
    integer :: ifail
    integer :: k,i,j
    logical :: first_order_diff, second_order_diff
    real :: Q_XXX(numAssocSites),Q_XXT(numAssocSites,numAssocSites)
    real :: Q_XXn(numAssocSites,numAssocSites,nc)
    real :: Q_XXV(numAssocSites,numAssocSites)
    real :: Q_XTT(numAssocSites),Q_XVV(numAssocSites)
    real :: Q_Xnn(numAssocSites,nc,nc)
    real :: Q_XTn(numAssocSites,nc),Q_XVn(numAssocSites,nc)
    real :: Q_XTV(numAssocSites)
    real :: X_nn_temp(numAssocSites,nc*nc)
    first_order_diff = present(X_T) .or. present(X_V) .or. present(X_n)
    second_order_diff = present(X_TT) .or. present(X_VV) .or. &
         present(X_TV) .or. present(X_Tn) .or. present(X_Vn) .or. &
         present(X_nn)
    ! Sanity check on input.
    if (.not. (first_order_diff .OR. second_order_diff) ) then
      call stoperror("Why call this routine if you don't want any of X_T, X_V, X_n, X_TT, X_TV, X_VV, X_Tn, X_Vn, X_nn?")
    end if

    ! Assemble the m vector.
    call assemble_m_mich_k (eos%assoc,nc,n,m_mich_k)

    ! Assemble necessary Q-derivatives.
    if (second_order_diff) then
      call Q_derivatives_knowing_X(eos,nc,T=T,V=V,n=n,X_k=X,&
           Q_XT=Q_XT,Q_XV=Q_XV,Q_Xn=Q_Xn,Q_XX=Q_XX,&
           Q_XXX=Q_XXX,Q_XXT=Q_XXT,Q_XXV=Q_XXV,Q_XXn=Q_XXn,&
           Q_XTT=Q_XTT,Q_XVV=Q_XVV,Q_XTV=Q_XTV,Q_XTn=Q_XTn,&
           Q_XVn=Q_XVn,Q_Xnn=Q_Xnn,&
           X_calculated=.true.)
    else ! first_order_diff
      call Q_derivatives_knowing_X(eos,nc,T=T,V=V,n=n,X_k=X,&
           Q_XT=Q_XT,Q_XV=Q_XV,Q_Xn=Q_Xn,Q_XX=Q_XX,X_calculated=.true.)
    endif
    if (present(X_T)) then
      ! Solve for -X_T using dsysv.
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; Q_XT(k)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,1,Q_XX_temp,numAssocSites,ipiv,Q_XT,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_T.")
      end if

      X_T = -Q_XT
    end if
    if (present(X_V)) then
      ! Solve for -X_V using dsysv.
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; Q_XV(k)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,1,Q_XX_temp,numAssocSites,ipiv,Q_XV,numAssocSites,work,3*(numAssocSites),ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_V.")
      end if

      X_V = -Q_XV
    end if

    if (present(X_n)) then
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; Q_Xn(k,:)=0.0
        end if
      end do
      ! Solve for -X_n using dsysv.
      call dsysv('u',numAssocSites,nc,Q_XX_temp,numAssocSites,ipiv,&
           Q_Xn,numAssocSites,work,3*(numAssocSites),ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_n.")
      end if

      X_n = -Q_Xn
    end if

    ! Second order differentials
    if (present(X_TT)) then
      if (.not. present(X_T)) then
        call stoperror("saft module: X_T required when computing X_TT.")
      endif
      ! Solve for X_TT using dsysv.
      X_TT = -(Q_XTT+2.0*matmul(Q_XXT,X_T)+X_T*Q_XXX*X_T)
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; X_TT(k)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,1,Q_XX_temp,numAssocSites,ipiv,&
           X_TT,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_TT.")
      end if
    end if
    if (present(X_VV)) then
      if (.not. present(X_T)) then
        call stoperror("saft module: X_V required when computing X_VV.")
      endif
      ! Solve for X_VV using dsysv.
      X_VV = -(Q_XVV+2.0*matmul(Q_XXV,X_V)+X_V*Q_XXX*X_V)
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; X_VV(k)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,1,Q_XX_temp,numAssocSites,ipiv,&
           X_VV,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_VV.")
      end if
    end if
    if (present(X_TV)) then
      if (.not. (present(X_T) .and. present(X_V))) then
        call stoperror("saft module: X_T and X_V required when computing X_TV.")
      endif
      ! Solve for X_TV using dsysv.
      X_TV = -(Q_XTV+matmul(Q_XXV,X_T)+matmul(Q_XXT,X_V)+X_V*Q_XXX*X_T)
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; X_TV(k)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,1,Q_XX_temp,numAssocSites,ipiv,&
           X_TV,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_TV.")
      end if
    end if
    if (present(X_Tn)) then
      if (.not. (present(X_T) .and. present(X_n))) then
        call stoperror("saft module: X_T and X_n required when computing X_Tn.")
      endif
      ! Solve for X_Tn using dsysv.
      do i=1,nc
        X_Tn(:,i) = -(Q_XTn(:,i)+matmul(Q_XXn(:,:,i),X_T)+&
             matmul(Q_XXT,X_n(:,i))+X_n(:,i)*Q_XXX*X_T)
      enddo
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; X_Tn(k,:)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,nc,Q_XX_temp,numAssocSites,ipiv,&
           X_Tn,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_Tn.")
      end if
    end if
    if (present(X_Vn)) then
      if (.not. (present(X_V) .and. present(X_n))) then
        call stoperror("saft module: X_V and X_n required when computing X_Vn.")
      endif
      ! Solve for X_Vn using dsysv.
      do i=1,nc
        X_Vn(:,i) = -(Q_XVn(:,i)+matmul(Q_XXn(:,:,i),X_V)+&
             matmul(Q_XXV,X_n(:,i))+X_n(:,i)*Q_XXX*X_V)
      enddo
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; X_Vn(k,:)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,nc,Q_XX_temp,numAssocSites,ipiv,&
           X_Vn,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_Vn.")
      end if
    end if
    if (present(X_nn)) then
      if (.not. present(X_n)) then
        call stoperror("saft module: X_n required when computing X_nn.")
      endif
      ! Solve for X_nn using dsysv.
      do i=1,nc
        do j=1,nc
          X_nn_temp(:,nc*(i-1)+j) = -(Q_Xnn(:,i,j)+&
               matmul(Q_XXn(:,:,j),X_n(:,i))+&
               matmul(Q_XXn(:,:,i),X_n(:,j))+X_n(:,j)*Q_XXX*X_n(:,i))
        enddo
      enddo
      Q_XX_temp = Q_XX
      do k=1,numAssocSites
        if (m_mich_k(k) .eq. 0.0) then
          Q_XX_temp(k,:)=0.0; Q_XX_temp(k,k)=1.0; X_nn_temp(k,:)=0.0
        end if
      end do
      call dsysv('u',numAssocSites,nc*nc,Q_XX_temp,numAssocSites,ipiv,&
           X_nn_temp,numAssocSites,work,3*numAssocSites,ifail)
      if (ifail .ne. 0) then
        call stoperror("saft module: dsysv failed when computing X_nn.")
      end if
      ! Copy solution to X_nn
      do i=1,nc
        do j=1,nc
          X_nn(:,i,j) = X_nn_temp(:,nc*(i-1)+j)
        enddo
      enddo
    end if
  end subroutine X_derivatives_knowing_X


  !> This back-end routine computes the necessary Q-derivatives. Note that X_k
  !> is an independent variable of this routine, but if one feeds it an X_k
  !> calculated from (T,V,n), one should set X_calculated = .true. This routine
  !> is valid for general SAFT equations (both CPA and PC-SAFT).
  subroutine Q_derivatives_knowing_X(eos,nc,T,V,n,X_k,Q,Q_T,Q_V,Q_n,Q_X,&
       Q_XT,Q_XV,Q_Xn,Q_XX,Q_TT,Q_TV,Q_Tn,Q_VV,Q_Vn,Q_nn,&
       Q_XXX,Q_XXT,Q_XXV,Q_XXn,Q_XTT,Q_XVV,Q_XTV,Q_XTn,Q_XVn,Q_Xnn,X_calculated)
    ! Input.
    class(base_eos_param), intent(in) :: eos
    integer, intent(in)         :: nc
    real, intent(in)            :: T,V,n(nc)
    real, intent(in)            :: X_k(numAssocSites)
    ! Output.
    real, intent(out), optional :: Q
    real, intent(out), optional :: Q_T, Q_V, Q_n(nc), Q_X(numAssocSites)
    real, intent(out), optional :: Q_XT(numAssocSites),Q_XV(numAssocSites),Q_Xn(numAssocSites,nc)
    real, intent(out), optional :: Q_TT,Q_TV,Q_Tn(nc),Q_VV,Q_Vn(nc),Q_nn(nc,nc)
    real, intent(out), optional :: Q_XX(numAssocSites,numAssocSites)
    real, intent(out), optional :: Q_XXX(numAssocSites),Q_XXT(numAssocSites,numAssocSites)
    real, intent(out), optional :: Q_XXn(numAssocSites,numAssocSites,nc)
    real, intent(out), optional :: Q_XXV(numAssocSites,numAssocSites)
    real, intent(out), optional :: Q_XTT(numAssocSites),Q_XVV(numAssocSites)
    real, intent(out), optional :: Q_Xnn(numAssocSites,nc,nc)
    real, intent(out), optional :: Q_XTn(numAssocSites,nc),Q_XVn(numAssocSites,nc)
    real, intent(out), optional :: Q_XTV(numAssocSites)
    logical, intent(in), optional :: X_calculated !< Is X_k = X_k(T,V,n) calculated?
    ! Local variables.
    real :: Delta(numAssocSites,numAssocSites)
    real :: Delta_T(numAssocSites,numAssocSites)
    real :: Delta_V(numAssocSites,numAssocSites)
    real :: Delta_n(numAssocSites,numAssocSites,nc)
    real :: Delta_TT(numAssocSites,numAssocSites)
    real :: Delta_TV(numAssocSites,numAssocSites)
    real :: Delta_Tn(numAssocSites,numAssocSites,nc)
    real :: Delta_VV(numAssocSites,numAssocSites)
    real :: Delta_Vn(numAssocSites,numAssocSites,nc)
    real :: Delta_nn(numAssocSites,numAssocSites,nc,nc)
    real :: m_mich_k(numAssocSites)
    real :: Kmich(numAssocSites,numAssocSites)
    real :: quantity(numAssocSites,numAssocSites), logX(numAssocSites)
    integer :: i,j,k,l,m,k1,k2,l1,l2
    integer :: n_k,C_nu,B_j,A_k
    logical :: loc_flag, present_fir_der, present_sec_der, K_mich_calc

    loc_flag = .false.
    if (present(X_calculated)) then
      if (X_calculated) loc_flag = .true.
    end if

    call assemble_m_mich_k (eos%assoc,nc,n,m_mich_k)

    K_mich_calc = present(Q_XX) .or. present(Q_XXT) .or. &
         present(Q_XXV) .or. present(Q_XXn) .or. present(Q_XTT) .or. &
         present(Q_XVV) .or. present(Q_XTV) .or. present(Q_XTn) .or. &
         present(Q_XVn) .or. present(Q_Xnn)
    ! Which derivatives are present?
    present_fir_der = present(Q_T) .or. present(Q_V) .or. &
         present(Q_n) .or. present(Q_XT) .or. present(Q_XV) .or. &
         present(Q_Xn) .or. present(Q_XXT) .or. present(Q_XXn) .or. &
         present(Q_XXV)
    present_sec_der = present(Q_TT) .or. present(Q_TV) .or. &
         present(Q_Tn) .or. present(Q_VV) .or. present(Q_Vn) .or. &
         present(Q_nn) .or. K_mich_calc

    ! Assemble necessary Delta derivatives.
    if (present_sec_der) then
      call Delta_kl(eos,nc,T,V,n,Delta,Delta_T,Delta_V,Delta_n,&
           Delta_TT,Delta_TV,Delta_Tn,Delta_VV,Delta_Vn,Delta_nn)
    else if ( present_fir_der ) then
      call Delta_kl(eos,nc,T,V,n,Delta,Delta_T,Delta_V,Delta_n)
    else
      call Delta_kl(eos,nc,T,V,n,Delta)
    end if

    ! Calculate the K_mich
    if (K_mich_calc) then
      call K_mich(eos,nc,T,V,n,Kmich,m_opt=m_mich_k,Delta_opt=Delta)
    endif

    ! Precalculate a quantity common to many of the expressions.
    do k=1,numAssocSites
      do l=k,numAssocSites
        quantity(k,l) = 0.5*m_mich_k(k)*m_mich_k(l)*X_k(k)*X_k(l)/V
        quantity(l,k) = quantity(k,l)
      end do
    end do

    ! Precalculate logarithms.
    if (present(Q) .or. present(Q_n)) logX = log(X_k)


    ! Start assembling Q and its required derivatives.

    if (present(Q)) then
      ! Assemble Q.
      Q = 0.0
      if (loc_flag) then
        do k=1,numAssocSites
          Q = Q + m_mich_k(k)*(logX(k)+0.5*(-X_k(k)+1))
        end do
      else
        do k=1,numAssocSites
          Q = Q + m_mich_k(k)*(logX(k)-X_k(k)+1)
          do l=1,numAssocSites
            Q = Q - quantity(k,l)*Delta(k,l)
          end do
        end do
      end if
    end if

    if (present(Q_T)) then
      ! Assemble Q_T.
      Q_T = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_T = Q_T - quantity(k,l)*Delta_T(k,l)
        end do
      end do
    end if

    if (present(Q_V)) then
      ! Assemble Q_V.
      Q_V = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_V = Q_V + quantity(k,l)*(Delta(k,l)/V - Delta_V(k,l))
        end do
      end do
    end if

    if (present(Q_n)) then
      ! Assemble Q_n.
      Q_n = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_n = Q_n - quantity(k,l)*Delta_n(k,l,:)
        end do
      end do
      if (loc_flag) then
        do i=1,nc
          if (eos%assoc%comp_vs_sites(i,1) .ne. noSitesFlag) then
            do k=eos%assoc%comp_vs_sites(i,1),eos%assoc%comp_vs_sites(i,2)
              Q_n(i) = Q_n(i) + logX(k)
            end do
          end if
        end do
      else
        do i=1,nc
          if (eos%assoc%comp_vs_sites(i,1) .ne. noSitesFlag) then
            do k=eos%assoc%comp_vs_sites(i,1),eos%assoc%comp_vs_sites(i,2)
              Q_n(i) = Q_n(i) + logX(k)-X_k(k)+1
              do l=1,numAssocSites
                Q_n(i) = Q_n(i) - m_mich_k(l)*X_k(k)*X_k(l)*Delta(k,l)/V
              end do
            end do
          end if
        end do
      end if
    end if

    if (present(Q_X)) then
      ! Assemble Q_X.
      Q_X = 0.0
      if (.not. loc_flag) then
        do k=1,numAssocSites
          Q_X(k) = m_mich_k(k)*(1/X_k(k) - 1)
          do l=1,numAssocSites
            Q_X(k) = Q_X(k) - m_mich_k(k)*m_mich_k(l)*X_k(l)*Delta(k,l)/V
          end do
        end do
      end if
    end if

    if (present(Q_XX)) then
      ! Assemble Q_XX.                                            Initialize to the ..
      Q_XX = -Kmich                                              ! .. -K_mich_kl matrix.
      do k=1,numAssocSites
        Q_XX(k,k) = Q_XX(k,k) - m_mich_k(k)/(X_k(k)**2) ! Diagonal contributions.
      end do
    end if

    if (present(Q_XT)) then
      ! Assemble Q_XT.
      Q_XT = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_XT(k) = Q_XT(k) - m_mich_k(k)*m_mich_k(l)*X_k(l)*Delta_T(k,l)/V
        end do
      end do
    end if

    if (present(Q_XV)) then
      ! Assemble Q_XV.
      Q_XV = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_XV(k) = Q_XV(k) + m_mich_k(k)*m_mich_k(l)*X_k(l)*(Delta(k,l)/(V*V) - Delta_V(k,l)/V)
        end do
      end do
    end if

    if (present(Q_Xn)) then
      ! Assemble Q_Xn.
      Q_Xn = 0.0
      do n_k=1,nc
        call compidx_to_sites(eos%assoc,n_k,k1,k2)
        do C_nu=1,numAssocSites

          if ((.not. loc_flag) .and. (k1 .le. C_nu) .and. (C_nu .le. k2)) then
            Q_Xn(C_nu,n_k) = Q_Xn(C_nu,n_k) + 1/X_k(C_nu) - 1
            do B_J=1,numAssocSites
              Q_Xn(C_nu,n_k) = Q_Xn(C_nu,n_k) - m_mich_k(B_j)*X_k(B_j)*Delta(C_nu,B_j)/V
            end do
          end if

          if (k1 .ne. noSitesFlag) then
            do A_k = k1,k2
              Q_Xn(C_nu,n_k) = Q_Xn(C_nu,n_k) - m_mich_k(C_nu)*X_k(A_k)*Delta(A_k,C_nu)/V
            end do
          end if

          do B_j=1,numAssocSites
            Q_Xn(C_nu,n_k) = Q_Xn(C_nu,n_k) - m_mich_k(C_nu)*m_mich_k(B_j)*X_k(B_j)*Delta_n(C_nu,B_j,n_k)/V
          end do

        end do
      end do
    end if

    if (present(Q_TT)) then
      ! Assemble Q_TT.
      Q_TT = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_TT = Q_TT - quantity(k,l)*Delta_TT(k,l)
        end do
      end do
    end if

    if (present(Q_TV)) then
      ! Assemble Q_TV.
      Q_TV = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_TV = Q_TV + quantity(k,l)*(Delta_T(k,l)/V-Delta_TV(k,l))
        end do
      end do
    end if

    if (present(Q_Tn)) then
      ! Assemble Q_Tn.
      Q_Tn = 0.0
      do i=1,nc
        call compidx_to_sites(eos%assoc,i,k1,k2)
        do k=1,numAssocSites
          if ((k1 .le. k) .and. (k .le. k2)) then
            do l=1,numAssocSites
              Q_Tn(i) = Q_Tn(i) - m_mich_k(l)*X_k(k)*X_k(l)*Delta_T(k,l)/V
            end do
          end if
          do l=1,numAssocSites
            Q_Tn(i) = Q_Tn(i) - quantity(k,l)*Delta_Tn(k,l,i)
          end do
        end do
      end do
    end if

    if (present(Q_VV)) then
      ! Assemble Q_VV.
      Q_VV = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_VV = Q_VV + quantity(k,l)*(2*(-Delta(k,l)/V + Delta_V(k,l))/V - Delta_VV(k,l))
        end do
      end do
    end if

    if (present(Q_Vn)) then
      ! Assemble Q_Vn.
      Q_Vn = 0.0
      do i=1,nc
        call compidx_to_sites(eos%assoc,i,k1,k2)
        do k=1,numAssocSites
          if ((k1 .le. k) .and. (k .le. k2)) then
            do l=1,numAssocSites
              Q_Vn(i) = Q_Vn(i) + m_mich_k(l)*X_k(k)*X_k(l)*(Delta(k,l)/V - Delta_V(k,l))/V
            end do
          end if
          do l=1,numAssocSites
            Q_Vn(i) = Q_Vn(i) + quantity(k,l)*(Delta_n(k,l,i)/V - Delta_Vn(k,l,i))
          end do
        end do
      end do
    end if


    if (present(Q_nn)) then
      ! Assemble Q_nn.
      Q_nn = 0.0

      do i=1,nc
        call compidx_to_sites(eos%assoc,i,k1,k2)
        do j=1,nc
          call compidx_to_sites(eos%assoc,j,l1,l2)
          do k=1,numAssocSites

            do l=1,numAssocSites
              if ((k1 .le. k) .and. (k .le. k2) .and. (l1 .le. l) .and. (l .le. l2)) then
                Q_nn(i,j) = Q_nn(i,j) - X_k(k)*X_k(l)*Delta(k,l)/V
              end if
            end do

            if ((k1 .le. k) .and. (k .le. k2)) then
              do l=1,numAssocSites
                Q_nn(i,j) = Q_nn(i,j) - m_mich_k(l)*X_k(k)*X_k(l)*Delta_n(k,l,j)/V
              end do
            end if

            if ((l1 .le. k) .and. (k .le. l2)) then
              do m=1,numAssocSites
                Q_nn(i,j) = Q_nn(i,j) - m_mich_k(m)*X_k(k)*X_k(m)*Delta_n(k,m,i)/V
              end do
            end if

            do l=1,numAssocSites
              Q_nn(i,j) = Q_nn(i,j) - quantity(k,l)*Delta_nn(k,l,i,j)
            end do

          end do
        end do
      end do

    end if

    if (present(Q_XXX)) then
      ! Assemble Q_XXX.
      do k=1,numAssocSites
        Q_XXX(k) = 2.0*m_mich_k(k)/(X_k(k)**3) ! Only diagonal different from zero.
      end do
    end if
    if (present(Q_XXT)) then
      ! Assemble Q_XXT.
      Q_XXT = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          if (Delta(k,l) /= 0.0) then
            Q_XXT(k,l) = -Kmich(k,l)*Delta_T(k,l)/Delta(k,l)
          endif
        end do
      end do
    end if
    if (present(Q_XXV)) then
      ! Assemble Q_XXV.
      Q_XXV = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          if (Delta(k,l) /= 0.0) then
            Q_XXV(k,l) = Kmich(k,l)*(1.0/V - Delta_V(k,l)/Delta(k,l))
          endif
        end do
      end do
    end if
    if (present(Q_XXn)) then
      ! Assemble Q_XXn.
      Q_XXn = 0.0
      do k=1,nc
        call compidx_to_sites(eos%assoc,k,k1,k2)
        do i=1,numAssocSites
          do j=1,numAssocSites
            Q_XXn(i,j,k) = Q_XXn(i,j,k) - m_mich_k(i)*m_mich_k(j)*Delta_n(i,j,k)/V
            if (i >= k1 .and. i <= k2) then
              Q_XXn(i,j,k) = Q_XXn(i,j,k) - m_mich_k(j)*Delta(i,j)/V
            endif
            if (j >= k1 .and. j <= k2) then
              Q_XXn(i,j,k) = Q_XXn(i,j,k) - m_mich_k(i)*Delta(i,j)/V
            endif
          end do
          ! Add diagonal contribution
          if (i >= k1 .and. i <= k2) then
            Q_XXn(i,i,k) = Q_XXn(i,i,k) - 1.0/X_k(i)**2
          endif
        end do
      end do
    end if

    if (present(Q_XTT)) then
      ! Assemble Q_XTT.
      Q_XTT = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_XTT(k) = Q_XTT(k) - m_mich_k(k)*m_mich_k(l)*X_k(l)*Delta_TT(k,l)
        enddo
      enddo
      Q_XTT = Q_XTT/V
    endif
    if (present(Q_XTV)) then
      ! Assemble Q_XTV.
      Q_XTV = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_XTV(k) = Q_XTV(k) + m_mich_k(k)*m_mich_k(l)*X_k(l)*&
               (Delta_T(k,l)/V**2 - Delta_TV(k,l)/V)
        enddo
      enddo
    endif
    if (present(Q_XVV)) then
      ! Assemble Q_XVV.
      Q_XVV = 0.0
      do k=1,numAssocSites
        do l=1,numAssocSites
          Q_XVV(k) = Q_XVV(k) - m_mich_k(k)*m_mich_k(l)*X_k(l)*&
               (2.0*Delta(k,l)/V**2 - 2.0*Delta_V(k,l)/V + Delta_VV(k,l))/V
        enddo
      enddo
    endif
    if (present(Q_XTn)) then
      ! Assemble Q_XTn.
      Q_XTn = 0.0
      do k=1,nc
        call compidx_to_sites(eos%assoc,k,k1,k2)
        do i=1,numAssocSites
          do j=1,numAssocSites
            Q_XTn(i,k) = Q_XTn(i,k) - m_mich_k(i)*m_mich_k(j)*X_k(j)*&
                 Delta_Tn(i,j,k)
            if (i >= k1 .and. i <= k2) then
              Q_XTn(i,k) = Q_XTn(i,k) - m_mich_k(j)*X_k(j)*Delta_T(i,j)
            endif
            if (j >= k1 .and. j <= k2) then
              Q_XTn(i,k) = Q_XTn(i,k) - m_mich_k(i)*X_k(j)*Delta_T(i,j)
            endif
          enddo
        enddo
      enddo
      Q_XTn = Q_XTn/V
    endif
    if (present(Q_XVn)) then
      ! Assemble Q_XVn.
      Q_XVn = 0.0
      do k=1,nc
        call compidx_to_sites(eos%assoc,k,k1,k2)
        do i=1,numAssocSites
          do j=1,numAssocSites
            Q_XVn(i,k) = Q_XVn(i,k) + m_mich_k(i)*m_mich_k(j)*X_k(j)*&
                 (Delta_n(i,j,k)/V - Delta_Vn(i,j,k) )
            if (i >= k1 .and. i <= k2) then
              Q_XVn(i,k) = Q_XVn(i,k) + m_mich_k(j)*X_k(j)*&
                   (Delta(i,j)/V - Delta_V(i,j))
            endif
            if (j >= k1 .and. j <= k2) then
              Q_XVn(i,k) = Q_XVn(i,k) + m_mich_k(i)*X_k(j)*&
                   (Delta(i,j)/V - Delta_V(i,j))
            endif
          enddo
        enddo
      enddo
      Q_XVn = Q_XVn/V
    endif
    if (present(Q_Xnn)) then
      ! Assemble Q_Xnn.
      Q_Xnn = 0.0
      do k=1,nc
        call compidx_to_sites(eos%assoc,k,k1,k2)
        do l=1,nc
          call compidx_to_sites(eos%assoc,l,l1,l2)
          do i=1,numAssocSites
            do j=1,numAssocSites
              Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - m_mich_k(i)*m_mich_k(j)*X_k(j)*&
                   Delta_nn(i,j,k,l)
              if (i >= k1 .and. i <= k2) then
                Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - m_mich_k(j)*X_k(j)*&
                     Delta_n(i,j,l)
              endif
              if (j >= k1 .and. j <= k2) then
                Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - m_mich_k(i)*X_k(j)*&
                     Delta_n(i,j,l)
              endif
              if (i >= l1 .and. i <= l2) then
                Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - m_mich_k(j)*X_k(j)*&
                     Delta_n(i,j,k)
              endif
              if (j >= l1 .and. j <= l2) then
                Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - m_mich_k(i)*X_k(j)*&
                     Delta_n(i,j,k)
              endif
              if (i >= k1 .and. i <= k2 .and. j >= l1 .and. j <= l2) then
                Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - X_k(j)*Delta(i,j)
              endif
              if (i >= l1 .and. i <= l2 .and. j >= k1 .and. j <= k2) then
                Q_Xnn(i,k,l) = Q_Xnn(i,k,l) - X_k(j)*Delta(i,j)
              endif
            enddo
          enddo
        enddo
      enddo
      Q_Xnn = Q_Xnn/V
    endif

  end subroutine Q_derivatives_knowing_X

  !******** DERIVATIVES OF THE ASSOCIATION CONTRIBUTION TO THE REDUCED, RESIDUAL HELMHOLTZ ENERGY ********

  !> Calculates the association contribution to the reduced, residual Helmholtz
  !> energy, along with its derivatives.
  subroutine calcFder_assoc(eos,nc,X_k,T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    ! Input.
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, intent(in) :: X_k(numAssocSites)
    real, intent(in) :: T
    real, intent(in) :: V
    real, intent(in) :: n(nc)
    ! Output.
    real, optional, intent(out) :: F
    real, optional, intent(out) :: F_T, F_V, F_n(nc)
    real, optional, intent(out) :: F_TT, F_TV, F_VV, F_Tn(nc), F_Vn(nc), F_nn(nc,nc)
    ! Locals.
    real, dimension(numAssocSites) :: Q_XT, Q_XV, X_T, X_V
    real, dimension(numAssocSites,nc) :: Q_Xn, X_n
    logical :: sec_der_present

    ! Compute contributions from direct differentiation of Q.
    ! This syntax only computes the variables that are present.
    call Q_derivatives_knowing_X(eos,nc,T,V,n,X_k,Q=F,Q_T=F_T,Q_V=F_V,Q_n=F_n,&
         Q_TT=F_TT,Q_TV=F_TV,Q_Tn=F_Tn,Q_VV=F_VV,Q_Vn=F_Vn,Q_nn=F_nn,X_calculated=.true.)

    sec_der_present = present(F_TT) .or. present(F_TV) .or. present(F_Tn) .or. &
         present(F_VV) .or. present(F_Vn) .or. present(F_nn)

    ! Additional contributions to second derivatives.
    if (sec_der_present) then
      call X_derivatives_knowing_X (eos,nc=nc,T=T,V=V,n=n,X=X_k,X_T=X_T,X_V=X_V,X_n=X_n)
      call Q_derivatives_knowing_X(eos,nc,T=T,V=V,n=n,X_k=X_k,Q_XT=Q_XT,Q_XV=Q_XV,Q_Xn=Q_Xn,X_calculated=.true.)

      if (present(F_TT)) F_TT = F_TT + dot_product(Q_XT,X_T)
      if (present(F_TV)) F_TV = F_TV + dot_product(Q_XV,X_T)
      if (present(F_Tn)) F_Tn = F_Tn + matmul(X_T,Q_Xn)

      if (present(F_VV)) F_VV = F_VV + dot_product(Q_XV,X_V)
      if (present(F_Vn)) F_Vn = F_Vn + matmul(X_V,Q_Xn)

      if (present(F_nn)) then
        F_nn = F_nn + matmul(transpose(Q_Xn),X_n)
      end if
    end if

  end subroutine calcFder_assoc

  !> Gives the association contribution to pressure.
  subroutine assoc_pressure(eos,nc,T,V,n,X_k,P,dPdV,dPdT,dPdn)
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, intent(in)  :: T
    real, intent(in)  :: V
    real, intent(in)  :: n(nc)
    real, intent(in)  :: X_k(numAssocSites)
    real, intent(out) :: P
    real, intent(out), optional :: dPdV, dPdT, dPdn(nc)
    ! Locals.
    real :: F_V

    call calcFder_assoc(eos,nc=nc,X_k=X_k,T=T,V=V,n=n,F_V=F_V,&
         F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
    P = -Rgas*T*F_V
    if (present(dPdV)) dPdV = -Rgas*T*dPdV
    if (present(dPdT)) dPdT = -Rgas*F_V - Rgas*T*dPdT
    if (present(dPdn)) dPdn = -Rgas*T*dPdn
  end subroutine assoc_pressure

  !******************** ROUTINES NEEDED IN NONLINEAR SOLVE *******************!

  !> The X-gradient of Q.
  subroutine fun(resid0,X,param)
    use thermopack_var, only: nce
    ! Output.
    real, dimension(numAssocSites), intent(out) :: resid0   !< the X-gradient of Q.
    ! Input.
    real, dimension(numAssocSites), intent(in) :: X         !< the X-vector
    real, dimension(2+nce), intent(in) :: param
    ! Locals.
    real :: T, V, n(nce)
    integer :: k
    real :: m_mich_k(numAssocSites)
    class(base_eos_param), pointer :: eos
    T = param(1)
    V = param(2)
    n = param(3:(nce+2))
    eos => get_active_eos()

    call Q_derivatives_knowing_X(eos,nce,T=T,V=V,n=n,X_k=X,Q_X=resid0)

    call assemble_m_mich_k (eos%assoc,nce,n,m_mich_k)
    do k=1,numAssocSites
      if (m_mich_k(k) .eq. 0.0) resid0(k) = 0.0 ! Previous bug: resid0 = 0.0
    end do

  end subroutine fun

  !> The X-Hessian of Q, modified according to Michelsen.
  subroutine jac(J,X,param)
    use thermopack_var, only: nce
    ! Output.
    real, dimension(numAssocSites,numAssocSites), intent(out) :: J   !< the modified X-Hessian of Q
    ! Input.
    real, dimension(numAssocSites), intent(in) :: X                   !< the X-vector
    real, dimension(2+nce), intent(in) :: param
    ! Locals.
    integer :: k
    real :: T, V, n(nce)
    real, dimension(numAssocSites,numAssocSites) :: K_mich_kl
    real, dimension(numAssocSites) :: m_mich_k
    class(base_eos_param), pointer :: eos
    T = param(1)
    V = param(2)
    n = param(3:(nce+2))
    eos => get_active_eos()

    ! Assemble m_mich and K_mich.
    call assemble_m_mich_k (eos%assoc,nce,n,m_mich_k)
    call K_mich (eos,nce,T,V,n,K_mich_kl,m_opt=m_mich_k)

    ! Assemble the modified Hessian matrix hatH, here called J.
    J = -K_mich_kl
    do k=1,numAssocSites
      if (m_mich_k(k) .eq. 0.0) then
        J(k,k) = 1.0 ! Other elements in kth row and kth column are 0.0.
      else
        J(k,k) = J(k,k) - (m_mich_k(k) + dot_product(K_mich_kl(k,:),X))/X(k)
      end if
    end do
  end subroutine jac

  !> Just a dummy function needed in nonlinear_solve.
  subroutine hess(Jinv,x,param)
    use thermopack_var, only: nce
    real, dimension(numAssocSites,numAssocSites) :: Jinv
    real, dimension(numAssocSites) :: x
    real, dimension(2+nce) :: param
    return
  end subroutine hess

  !> Procedure for limiting the step (deny more than 80% reduction of any X_{A_i}-variable)
  subroutine limit(n,x,xmin,xmax,dx,np,lim_param)
    integer, intent(in) :: n
    real, dimension(n),     intent(in) :: x,xmin,xmax
    real, dimension(n),     intent(inout) :: dx
    integer, intent(in) :: np
    real, dimension(np),     intent(inout) :: lim_param
    real :: scaling
    integer :: i

    scaling = 1.0
    do i=1,n
      if (x(i)+dx(i) < 0.2*x(i)) then ! deny more than 80% reduction in any X-component
        scaling = min(scaling,-0.8*x(i)/dx(i))
      endif
      if (x(i)+dx(i) > xmax(i) .AND. abs(dx(i)) > 1.0e-9) then ! we're inputting xmax = 1.0 to nonlinear_solve()
        scaling = min(scaling,(xmax(i)-x(i))/dx(i))
      endif
    enddo

    if (scaling < 1.0) then
      dx = dx * scaling
    endif

    return
  end subroutine limit

  !> Successive substitution method.
  subroutine fun_succ_subst (eos,T,V,n,X)
    use thermopack_var, only: nce
    class(base_eos_param), intent(in) :: eos
    real, intent(inout)  :: X(numAssocSites)
    ! Locals.
    integer :: k,l
    real :: T, V, n(nce)
    real, dimension(numAssocSites,numAssocSites) :: K_mich_kl
    real, dimension(numAssocSites) :: m_mich_k
    real :: temp
    real :: omega
    omega = 0.2 ! damping factor

    ! assemble m_mich and K_mich
    call assemble_m_mich_k (eos%assoc,nce,n,m_mich_k)
    call K_mich (eos,nce,T,V,n,K_mich_kl,m_opt=m_mich_k)
    do k=1,numAssocSites
      if (m_mich_k(k) .eq. 0.0) then
        cycle
      end if
      temp = 0
      do l=1,numAssocSites
        temp = temp + X(l)*K_mich_kl(l,k)
      end do
      X(k) = (1-omega)*m_mich_k(k)/(m_mich_k(k) + temp) + omega*X(k)
    end do

  end subroutine fun_succ_subst

end module saft_association
