module sutherland_a2tilde
    !> The SAFT-VR Mie implementation of the first-order perturbation
    !> term a1 for the Sutherland potential, defined by u(r) =
    !> -eps*(sigma/r)**lambda. A hard-sphere reference system is used,
    !> which enters as the input x0=sigma/dhs
  
    !> Equation numbers refer to
    !> the SAFT-VR Mie paper by Lafitte et al. (2013).
    use hyperdual_mod
    implicit none
  
    public :: calc_a2tilde_sutherland
  
  contains
  
    !> Calculate first order perturbation term a1tilde=a1/eta
    subroutine calc_a2tilde_sutherland(x0,eta,eta_av,lambda,eps,alpha,a2,K_hs,chi)!,a1_zerodensity)
      ! Input
      type(hyperdual), intent(in) :: x0     !< Reduced center-center hard sphere distance
      type(hyperdual), intent(in) :: eta    !< Packing fraction (-)
      type(hyperdual), intent(in) :: lambda !< Sutherland exponent (-)
      type(hyperdual), intent(in) :: eps    !< Sutherland energy (K)
      

      type(hyperdual), intent(in) :: eta_av
      type(hyperdual), intent(in) :: alpha
      ! Output
      type(hyperdual), intent(out) :: a2    !< a1/eta (-)
      type(hyperdual) :: K_hs
      type(hyperdual) :: chi
      !type(hyperdual), intent(out), optional :: a1_zerodensity
      ! Locals
      type(hyperdual) :: B!, B_zerodensity
      type(hyperdual) :: as!, as_zerodensity
      type(hyperdual) :: x0_lam
   
      !type(hyperdual) :: denum
  
      x0_lam = x0**lambda

      call calc_isoCompress(eta,K_hs)
      call calc_chi(eta_av,alpha,chi)

      call calcA1tilde_hardcore_sutherland(eta,lambda,eps,as)
      call calcBtilde(x0,eta,lambda,eps,B)
      a2 = x0_lam*(as+B)
   end subroutine calc_a2tilde_sutherland


    !> Calculate first order perturbation term a1tilde=a1/eta
    subroutine calc_isoCompress(eta, K_hs)
      !input
      type(hyperdual), intent(in) :: eta
      !output
      type(hyperdual), intent(out) :: K_hs
      !local
      type(hyperdual) :: denum
      denum = 1.0+4.0*eta+4.0*eta**2-4.0*eta**3+eta**4
      K_hs = (1.0-eta)**4/denum
    end subroutine calc_isoCompress

    subroutine calc_chi(eta_av,alpha,chi)
      !input
      type(hyperdual), intent(in) :: eta_av
      type(hyperdual), intent(in) :: alpha
      !output
      type(hyperdual), intent(out) :: chi
      ! Locals
      real, parameter, dimension(0:6,1:6) :: phi = &
         reshape((/7.5365557, -37.60463, 71.745953, -46.83552, -2.467982, -0.50272, 8.0956883, &
         -359.44, 1825.6, -3168.0, 1884.2, -0.82376, -3.1935, 3.709, &
         1550.9, -5070.1, 6534.6, -3288.7, -2.7171, 2.0883, 0.0, &
         -1.19932, 9.063632, -17.9482, 11.34027, 20.52142, -56.6377, 40.53683, &
         -1911.28, 21390.175, -51320.7, 37064.54, 1103.742, -3264.61, 2556.181, &
         9236.9, -129430.0, 357230.0, -315530.0, 1390.2, -4518.2, 4241.6/), (/7,6/))
      type(hyperdual) :: alpha_n(0:3)
      type(hyperdual) :: f_i_num, f_i_denum
      type(hyperdual) :: f(6)
      integer :: i
      alpha_n(0) = 1.0
      do i=1,3
         alpha_n(i) = alpha*alpha_n(i-1)
      enddo
      do i=1,6
         f_i_num = sum(phi(0:3,i)*alpha_n(0:3))
         f_i_denum = 1.0 + sum(phi(4:6,i)*alpha_n(1:3))
         f(i) = f_i_num/f_i_denum
      enddo
      chi = f(1)*eta_av+f(2)*eta_av**5+f(3)*eta_av**8
    end subroutine calc_chi


    
    !> Correlation integral from d to sigma, Eq33
    subroutine calcBtilde(x0,eta,lambda,eps,B,B_zerodensity)
      ! Input
      type(hyperdual), intent(in) :: x0     !< Reduced center-center hard sphere distance
      type(hyperdual), intent(in) :: eta    !< Packing fraction (-)
      type(hyperdual), intent(in) :: lambda !< Sutherland exponent (-)
      type(hyperdual), intent(in) :: eps    !< Sutherland energy (K)
      ! Output
      type(hyperdual), intent(out) :: B
      type(hyperdual), intent(out), optional :: B_zerodensity
      ! Locals
      type(hyperdual) :: J,I,keta(2)
      type(hyperdual) :: denum3
  
      ! Calculate I_lambda (Eq28) and J_lambda (Eq29)
      I = - (x0**(3.0 - lambda) - 1.0)/(lambda - 3.0)
      J = - ((lambda - 3.0)*x0**(4.0 - lambda) - (lambda - 4.0)*x0**(3.0 - lambda) - 1.0)/&
           ((lambda - 3.0)*(lambda - 4.0))
  
      denum3 = (1.0-eta)**3
      keta(1) = (2.0-eta)/denum3
      keta(2) = -9.0*eta*(1.0+eta)/denum3
      B = 6.0*eps*(keta(1)*I + keta(2)*J)
      if (present(B_zerodensity)) then
         B_zerodensity = 12*eps*I
      end if
    end subroutine calcBtilde
  
    !> Calculate utility function for the Helmholtz free energy of
    !> hard-core Sutherland particle
    subroutine calcEffEta(eta,lambda,ef)
      ! Input
      type(hyperdual), intent(in) :: eta    !< Packing fraction (-)
      type(hyperdual), intent(in) :: lambda !< Sutherland exponent (-)
      ! Output
      type(hyperdual), intent(out) :: ef
      ! Locals
      real, parameter :: lam_coeff(4,4) = reshape((/ 0.81096, 1.7888, -37.578, &
           92.284, 1.0205, -19.341, 151.26, -463.50, -1.9057, 22.845, -228.14, 973.92, &
           1.0885, -6.1962, 106.98, -677.64 /), (/4,4/))
      type(hyperdual) :: c(4), inv_lam(4)
      integer :: i
      inv_lam(1) = 1.0
      do i=2,4
         inv_lam(i) = inv_lam(i-1)/lambda
      enddo
      do i=1,4
         c(i) = sum(lam_coeff(:,i)*inv_lam)
      enddo
      ef = eta*(c(1) + eta*(c(2) + eta*(c(3) + eta*c(4))))
    end subroutine calcEffEta
  
    !> Correlation integral from d to infty, i.e. a1 for a hard-core
    !> Sutherland particle (Eq39)
    subroutine calcA1tilde_hardcore_sutherland(eta,lambda,eps,a1s,a1s_zerodensity)
      ! Input
      type(hyperdual), intent(in) :: eta    !< Packing fraction (-)
      type(hyperdual), intent(in) :: lambda !< Sutherland exponent (-)
      type(hyperdual), intent(in) :: eps    !< Sutherland energy (K)
      ! Output
      type(hyperdual), intent(out) :: a1s
      type(hyperdual), intent(out), optional :: a1s_zerodensity
      ! Locals
      type(hyperdual) :: ef
      call calcEffEta(eta,lambda,ef)
      a1s = -12*eps/(lambda-3.0) * (1.0 - 0.5*ef)/(1.0 - ef)**3
      if (present(a1s_zerodensity)) then
         a1s_zerodensity = -12*eps/(lambda-3.0)
      end if
    end subroutine calcA1tilde_hardcore_sutherland
  
  end module sutherland_a2tilde