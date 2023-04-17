module sutherland_a3tilde
    !> The SAFT-VR Mie implementation of the first-order perturbation
    !> term a1 for the Sutherland potential, defined by u(r) =
    !> -eps*(sigma/r)**lambda. A hard-sphere reference system is used,
    !> which enters as the input x0=sigma/dhs
  
    !> Equation numbers refer to
    !> the SAFT-VR Mie paper by Lafitte et al. (2013).
    use hyperdual_mod
    implicit none
  
    public :: calc_a3tilde_sutherland
  
  contains
  
    !> Calculate first order perturbation term a1tilde=a1/eta
    subroutine calc_a3tilde_sutherland(eta_av,eps,alpha,a3)!,a1_zerodensity)
      ! Input
      type(hyperdual), intent(in) :: eps    !< Sutherland energy (K)
      type(hyperdual), intent(in) :: eta_av
      type(hyperdual), intent(in) :: alpha
      ! Output
      type(hyperdual), intent(out) :: a3    !< a1/eta (-)
      !type(hyperdual), intent(out), optional :: a1_zerodensity
      ! Locals
      type(hyperdual) :: x0_lam
      type(hyperdual) :: K_hs
      type(hyperdual) :: alpha_n(0:3)
      type(hyperdual) :: f_i_num, f_i_denum
      type(hyperdual) :: f(6)
      !output
   
      ! Locals
      real, parameter, dimension(0:6,1:6) :: phi = &
         reshape((/7.5365557, -37.60463, 71.745953, -46.83552, -2.467982, -0.50272, 8.0956883, &
         -359.44, 1825.6, -3168.0, 1884.2, -0.82376, -3.1935, 3.709, &
         1550.9, -5070.1, 6534.6, -3288.7, -2.7171, 2.0883, 0.0, &
         -1.19932, 9.063632, -17.9482, 11.34027, 20.52142, -56.6377, 40.53683, &
         -1911.28, 21390.175, -51320.7, 37064.54, 1103.742, -3264.61, 2556.181, &
         9236.9, -129430.0, 357230.0, -315530.0, 1390.2, -4518.2, 4241.6/), (/7,6/))
      
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
      !Eq(A25)
      a3 = -eps**3 *f(4)*eta_av*EXP(f(5)*eta_av+f(6)*eta_av**2)
   end subroutine calc_a3tilde_sutherland


    


   
  
  end module sutherland_a3tilde