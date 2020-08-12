!---------------------------------------------------------------------
! Module and subroutines for testing SAFT-VRQ Mie
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------

module saftvrmie_testing
  use saftvrmie_containers
  use thermopack_constants, only: kB_const,N_AVOGADRO,h_const
  use thermopack_var, only: nc, get_active_eos_container, eos_container
  use numconstants, only: pi
  use saftvrmie_hardsphere, only: calc_hardsphere_rdf_and_U
  use saftvrmie_dispersion
  use saftvrmie_options
  implicit none
  !private
  save

  public :: test_a1_integration, test_a2_integration, test_alpha_a3_integration
  public :: test_by_integration

contains

  subroutine test_by_integration
    use thermopack_constants
    use saftvrmie_hardsphere
    use saftvrmie_containers, only: init_saftvrmie_containers
    use saftvrmie_interface, only:  preCalcSAFTVRMie
    implicit none
    integer :: mixing
    real :: T, V, n(nc), Int_B, Int_B1, Int_B2
    type(eos_container), pointer :: p_act_eosc
    type(saftvrmie_var_container), pointer :: saftvrmie_var
    type(saftvrmie_eos), pointer :: eos
    p_act_eosc => get_active_eos_container()

    mixing=1
    T=4.0
    V=1
    n(1)=2

    ! No quantum corrections
    quantum_correction=0
    quantum_correction_hs=0
    eos => get_saftvrmie_eos_pointer(p_act_eosc%eos(1)%p_eos)
    saftvrmie_var => eos%saftvrmie_var
    call init_saftvrmie_containers(nc,p_act_eosc%comps,eos,&
         "DEFAULT",mixing)
    call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var)
    call test_a1_integration(nc,T,V,n,saftvrmie_var)
    call test_a2_integration(nc,T,V,n,saftvrmie_var)
    call test_alpha_a3_integration(nc,T,saftvrmie_var)
    call calc_virial_B_by_integration(nc,T,saftvrmie_var,Int_B)

    ! First order quantum corrections
    quantum_correction=1
    quantum_correction_hs=1
    call init_saftvrmie_containers(nc,p_act_eosc%comps,eos,"DEFAULT",mixing)
    call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var)
    call test_a1_integration(nc,T,V,n,saftvrmie_var)
    call test_a2_integration(nc,T,V,n,saftvrmie_var)
    call test_alpha_a3_integration(nc,T,saftvrmie_var)
    call calc_virial_B_by_integration(nc,T,saftvrmie_var,Int_B1)

    ! Second order quantum corrections
    quantum_correction=2
    quantum_correction_hs=2
    call init_saftvrmie_containers(nc,p_act_eosc%comps,eos,"DEFAULT",mixing)
    call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var)
    call test_a1_integration(nc,T,V,n,saftvrmie_var)
    call test_a2_integration(nc,T,V,n,saftvrmie_var)
    call test_alpha_a3_integration(nc,T,saftvrmie_var)
    call calc_virial_B_by_integration(nc,T,saftvrmie_var,Int_B2)
    stop

  end subroutine test_by_integration

  subroutine calc_virial_B_by_integration(nc,T,s_vc,Int_B,prt)
    !--------------------------------------------------------------------
    !  2018-04, Morten Hammer
    !
    !  This subroutine tests virial coefficient
    !---------------------------------------------------------------------
    use eosTV, only: Fres
    integer, intent(in) :: nc   ! Number of components
    real, intent(in) :: T       ! Temperature [K]
    type(saftvrmie_var_container), intent(inout) :: s_vc
    real, intent(out) :: Int_B       ! Integrated virial coefficient
    logical, optional, intent(in) :: prt ! Print results

    ! Other variables used
    real :: prefactor, a, b, V, n(nc), F_V
    real :: BvirialEOS
    integer :: Nr, spec
    logical :: prt_l

    ! Check for more than one component
    if (nc>1) then
       call stoperror("This test is only made for one component")
    end if

    prt_l = .true.
    if (present(prt)) then
       prt_l = prt
    endif
    ! Compute B from EOS
    V = 1.0e8 ! Might need tuning...
    n = 1.0
    call Fres(T,V,n,F_V=F_V)
    F_V = F_V*1.0e3 ! 1/m3
    V = V*1.0e-3 ! m3
    BvirialEOS = -V**2*F_V

    ! The prefactor of alpha
    prefactor=2.0*pi*N_Avogadro

    ! Set dummy variables
    V=0.0
    n = 0.0

    ! The integration boundaries
    a=0.0
    b=100.0*saftvrmie_param%comp(1)%sigma

    Nr=100000
    spec = quantum_correction
    call trapz_integration(nc,T,V,n,testing_virial_B_term, a, b, Nr, Int_B, spec)

    Int_B=Int_B*prefactor
    if (prt_l) then
       ! Print stuff
       print *, " ---- Testing of second virial coeff  ------- "
       print *, " -------------------------------------------- "
       print *, " The quantum correction            : ", quantum_correction
       print *, " Integrated B                      : ", Int_B
       print *, " EOS B                             : ", BvirialEOS
       print *, " -------------------------------------------- "
       print *, ""
    endif
  end subroutine calc_virial_B_by_integration

  subroutine test_alpha_a3_integration(nc,T,s_vc)
    !--------------------------------------------------------------------
    !  2018-03-15, Oivind Wilhelmsen
    !
    !  This subroutine tests a2
    !---------------------------------------------------------------------
    integer, intent(in) :: nc   ! Number of components
    real, intent(in) :: T       ! Temperature [K]
    type(saftvrmie_var_container), intent(inout) :: s_vc

    ! Other variables used
    real :: prefactor, a, b, V, n(nc), alph(nc)
    real :: alpha
    integer :: Nr
    real :: Int_alpha, Int_alpha_true, a_1B, Int_alpha_trueMie
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()

    ! Check for more than one component
    if (nc>1) then
       call stoperror("This test is only made for one component")
    end if

    Nr=10000

    ! Compute alpha
    if (quantum_correction_hs == 0) then
       alpha = saftvrmie_param%alpha_ij(1,1)
    else
       call calcAlpha(nc,s_vc%sigma_eff,s_vc%eps_divk_eff,T,svrm_var,alph)
       alpha = alph(1)
    endif

    ! The prefactor of alpha
    prefactor=-1.0/(s_vc%eps_divk_eff%d(1,1)*s_vc%sigma_eff%d(1,1)**3)

    ! Set variables giving g=1
    V=1.0
    n = 0.0

    ! The integration boundaries
    a=saftvrmie_param%comp(1)%sigma
    b=16.0*a

    a_1B=s_vc%sigma_eff%d(1,1)

    call trapz_integration(nc,T,V,n,testing_a1_term, a, b, Nr, Int_alpha)
    call trapz_integration(nc,T,V,n,testing_a1_term, a_1B, b, Nr, Int_alpha_true)
    call trapz_integration(nc,T,V,n,a1_term_integral_argument,&
         a_1B, b, Nr, Int_alpha_trueMie,0)

    ! Print stuff
    print *, " --------- Testing of alpha -------------- "
    print *, " -- s=sigma, s_eff=sigma_eff, Int=Integral -- "
    print *, " -------------------------------------------- "
    print *, " The quantum correction            : ", quantum_correction
    print *, " True alpha (from s_eff to inf)    : ", Int_alpha_true*prefactor
    print *, " Our value of alpha                : ", alpha
    print *, " -------------------- "
    print *, " Part alpha (Int. s to inf)        : ", Int_alpha*prefactor
    print *, " Part alpha (Mie, Int s_eff to inf): ", Int_alpha_trueMie*prefactor
    print *, " Part alpha (Mie) / Full alpha     : ", Int_alpha_trueMie/Int_alpha_true
    print *, " -------------------------------------------- "
    print *, ""

  end subroutine test_alpha_a3_integration

  subroutine test_a2_integration(nc,T,V,n,s_vc)
    !--------------------------------------------------------------------
    !  2018-03-15, Oivind Wilhelmsen
    !
    !  This subroutine tests a2
    !---------------------------------------------------------------------
    integer, intent(in) :: nc   ! Number of components
    real, intent(in) :: T       ! Temperature [K]
    real, intent(in) :: V       ! Volume [m^3]
    real, intent(in) :: n(nc)   ! Number of moles of each component
    type(saftvrmie_var_container), intent(inout) :: s_vc

    ! Other variables used
    real :: a2
    real :: c2, prefactor, a, b
    integer :: Nr
    real :: Int_a2, Int_a2_true, a_1B, Int_a2_trueMie, Int_a2_q1
    real :: Int_a2_Mie, Int_a2_Dpart, Int_a2_FH1, Int_a2_FH2

    ! Check for more than one component
    if (nc>1) then
       call stoperror("This test is only made for one component")
    end if

    Nr=10000

    ! Compute A2
    call calcA2(nc,T,V,n,s_vc,a2)

    ! Obtain c2, which is (1+xi) in the expression for a2:
    call calcA2CorrectionVn(nc,V,n,1,1,s_vc%alpha,s_vc%zeta_bar,c2)

    ! The prefactor of a1
    prefactor=-1.0*pi*s_vc%Khs%zx*c2*&
         N_AVOGADRO*n(1)*saftvrmie_param%ms(1)/V


    ! The integration boundaries
    a=saftvrmie_param%comp(1)%sigma
    b=16.0*a

    a_1B=s_vc%sigma_eff%d(1,1)

    call trapz_integration(nc,T,V,n,a2_FH_integrand,&
         a, b, Nr, Int_a2_Mie,0)
    call trapz_integration(nc,T,V,n,a2_FH_integrand,&
         a, b, Nr, Int_a2_FH1,1)
    call trapz_integration(nc,T,V,n,a2_FH_integrand,&
         a, b, Nr, Int_a2_FH2,2)

    call trapz_integration(nc,T,V,n,testing_a2_term, a, b, Nr, Int_a2)
    call trapz_integration(nc,T,V,n,testing_a2_term, a_1B, b, Nr, Int_a2_true)
    call trapz_integration(nc,T,V,n,a2_term_integral_argument,&
         a_1B, b, Nr, Int_a2_trueMie,0)
    call trapz_integration(nc,T,V,n,a2_term_integral_argument,&
         a, b, Nr, Int_a2_q1,1)
    call trapz_integration(nc,T,V,n,a2_term_integral_argument,&
         a, b, Nr, Int_a2_Mie,0)
    call trapz_integration(nc,T,V,n,a2_term_integral_argument,&
         a, b, Nr, Int_a2_Dpart,3)



    ! Print stuff
    print *, " --------- Testing of a2 terms -------------- "
    print *, " -- s=sigma, s_eff=sigma_eff, Int=Integral -- "
    print *, " -------------------------------------------- "
    print *, " The quantum correction         : ", quantum_correction
    print *, " True a2 (from s_eff to inf)    : ", Int_a2_true*prefactor
    print *, " Our value of a2                : ", a2
    print *, " -------------------- "
    print *, " Part a2 (Mie^2 from s to inf)  : ", (Int_a2_Mie)*prefactor!§+2.0*Int_a2_Dpart+Int_a2_q1)*prefactor
    print *, " Part a2 (Dpart from s to inf)  : ", (2*Int_a2_Dpart)*prefactor!§+2.0*Int_a2_Dpart+Int_a2_q1)*prefactor
    print *, " Part a2 (D2part from s to inf) : ", (Int_a2_q1)*prefactor!§+2.0*Int_a2_Dpart+Int_a2_q1)*prefactor
    print *, " Part a2 (FH1 from s to inf)    : ", (Int_a2_FH1)*prefactor!§+2.0*Int_a2_Dpart+Int_a2_q1)*prefactor
    print *, " Part a2 (FH2 from s to inf)    : ", (Int_a2_FH2)*prefactor!§+2.0*Int_a2_Dpart+Int_a2_q1)*prefactor

    print *, " Part a2 (Int. s to inf)        : ", Int_a2*prefactor
    print *, " Part a2 (Mie, Int s_eff to inf): ", Int_a2_trueMie*prefactor
    print *, " Part a3 (Mie) / Full a2        : ", Int_a2_trueMie/Int_a2_true
    print *, " -------------------------------------------- "
    print *, ""

  end subroutine test_a2_integration

  subroutine test_a1_integration(nc,T,V,n,saftvrmie_vc)
    !--------------------------------------------------------------------
    !  2018-03-13, Oivind Wilhelmsen
    !
    !  This subroutine tests a1
    !---------------------------------------------------------------------
    integer, intent(in) :: nc   ! Number of components
    real, intent(in) :: T       ! Temperature [K]
    real, intent(in) :: V       ! Volume [m^3]
    real, intent(in) :: n(nc)   ! Number of moles of each component
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc

    ! Internal variables
    !real :: U_divk  ! intermolecular potential
    !real :: r, r_p
    !real :: U_divk_p
    ! Output
    real :: a1
    real :: a1_test
    integer :: Nr     ! Number of steps in integration
    real :: Int
    real :: a, b      ! Integral boundaries
    real :: prefactor
    real :: I1A, I1B, I1C, I1D, a1A, b1A, a1B, b1B, a1C, b1C, a1D, b1D
    real :: Int_1A, Int_1B, Int_1C, Int_1D, Int_1D_Mie, Int_1D_Q1
    real :: a1_Mie, a1_Q1, Int_morten, Minimum

    if (nc>1) then
       call stoperror("This test is only made for one component")
    end if

    ! The integral and the specs.
    Nr=10000

    ! Integration boundaries
    a=saftvrmie_vc%sigma_eff%d(1,1)
    b=16.0*a

    a1A=saftvrmie_vc%dhs%d(1,1)
    b1A=b

    a1B=a1A
    b1B=a

    a1C=saftvrmie_param%comp(1)%sigma
    b1C=a

    a1D=a1C
    b1D=b

    call  calcA1(nc,T,V,n,saftvrmie_vc,a1)
    call  calcA1ij(nc,T,V,n,1,1,saftvrmie_vc,a1_Mie)

    if (quantum_correction==0) then
       a1_Q1=0.0
    else
       call calcA1ijQuantumCorrection(nc,T,V,n,1,1,saftvrmie_vc,a1_Q1)
    end if

    ! Multiply by the appropriate prefactors
    prefactor=2.0*pi*N_AVOGADRO*n(1)*saftvrmie_param%ms(1)/(V)
    call trapz_integration(nc,T,V,n,testing_a1_term, a, b, Nr, Int)
    call trapz_integration(nc,T,V,n,testing_a1_term, a1A, b1A, Nr, Int_1A)
    call trapz_integration(nc,T,V,n,testing_a1_term, a1B, b1B, Nr, Int_1B)
    call trapz_integration(nc,T,V,n,testing_a1_term, a1C, b1C, Nr, Int_1C)
    call trapz_integration(nc,T,V,n,testing_a1_term, a1D, b1D, Nr, Int_1D)
    call trapz_integration(nc,T,V,n,a1_term_integral_argument,&
         a1D, b1D, Nr, Int_1D_Mie,0)
    call trapz_integration(nc,T,V,n,a1_term_integral_argument,&
         a1D, b1D, Nr, Int_1D_Q1,1)
    call trapz_integration(nc,T,V,n,a1_term_integral_argument,&
         a1C, b1C, Nr, Int_Morten,0)
    call find_approximate_epsilon_eff(nc,T,V,n,testing_U,a,b,Nr,Minimum)

    a1_test=Int*prefactor
    I1A=Int_1A*prefactor
    I1B=-1.0*Int_1B*prefactor
    I1C=-1.0*Int_1C*prefactor
    I1D=Int_1D*prefactor

    print *, " Epsilon_eff/Epsilon: ", &
         Minimum/saftvrmie_param%comp(1)%eps_depth_divk
    !print *, " x0 ", saftvrmie_vc%sigma_eff%d(1,1)/saftvrmie_var%dhs%d(1,1)

    print *, " --------- Testing of a1 terms -------------- "
    print *, " -- s=sigma, s_eff=sigma_eff, Int=Integral -- "
    print *, " -------------------------------------------- "
    print *, " The quantum correction         : ", quantum_correction
    print *, " True a1 (from s_eff to inf)    : ", a1_test
    print *, " Our value of a1                : ", a1
    print *, " -------------------- "
    print *, " Part a1 (Int. s to inf)        : ", I1D
    print *, " -------------------------------------------- "
    print *, ""

    ! print *, "---------- Testing of A1 terms ------------ "
    !print *, " The quantum correction: ", quantum_correction
    !  print *, " The a1                  : ", a1
    ! print *, " Test of a1             : ", a1_test
    ! print *, " Test of wrong a1        : ", I1D
    ! print *, " The wrong a1 Mie        : ", a1_Mie
    ! print *, " Test of the wrong a1 Mie: ", Int_1D_Mie*prefactor
    ! print *, " The wrong a1 Q1         : ", a1_Q1
    ! print *, " Test of the wrong a1 Q1 : ", (Int_1D_Q1+Int_1D_Q2)*prefactor
    ! print *, " a1+I_1C (numerical) : ", I1C+a1
    !print *, " I_1B (from d to sigma): ", I1B
    ! print *, " I_1C (from sigma to sigma_eff): ", I1C
    !  print *, " Mortens parameter:  ", Int_Morten*prefactor
    !  print *, " a1 Morten:          ", a1_Mie-Int_Morten*prefactor
    !  print *, " -------------------------------------------"

  end subroutine test_a1_integration

  subroutine trapz_integration(nc,T,V,n,func,a,b,Nr,Int,spec)
    !--------------------------------------------------------------------
    !  2018-03-14, Oivind Wilhelmsen
    !
    !  This function obtains the numerical integral of func
    !  from a to b with N number of steps.
    !---------------------------------------------------------------------
    integer, intent(in) :: nc             ! Number of components
    real, intent(in) :: T,V,n(nc)         ! State variables
    external :: func                      ! Subroutine to integrate, f(r)
    real, intent(in) :: a,b               ! Integration boundaries
    integer, intent(in) :: Nr             ! Number of steps
    real, intent(out) :: Int              ! Integral
    integer, intent(in), optional :: spec ! 0= Mie-Term, 1=Q1, 2=Q2, 3:Q1Mie
    real :: dr                            ! Change in r
    real :: r                             ! The intergation variable
    real :: f_val                         ! Function value
    integer :: i

    ! Initialize the routine
    Int=0.0
    dr=(b-a)/Nr
    r=a

    ! We now perform a trapezoidal integration
    do i= 1, Nr

       if (present(spec)) then
          ! Evaluate the subroutine
          call  func(nc,T,V,n,r,spec,f_val)
       else
          call  func(nc,T,V,n,r,f_val)
       end if

       if (i==1) then
          Int=Int+f_val
       elseif(i==Nr) then
          Int=Int+f_val
       else
          Int=Int+2.0*f_val
       end if

       ! Update r
       r=r+dr
    end do
    ! Multiply by the prefactors
    Int=((b-a)/(2.0*Nr))*Int

  end subroutine trapz_integration

  subroutine find_approximate_epsilon_eff(nc,T,V,n,func,a,b,Nr,Minimum)
    !--------------------------------------------------------------------
    !  2018-03-14, Oivind Wilhelmsen
    !
    !  This function obtains the numerical integral of func
    !  from a to b with N number of steps.
    !---------------------------------------------------------------------
    integer, intent(in) :: nc             ! Number of components
    real, intent(in) :: T,V,n(nc)         ! State variables
    external :: func                      ! Subroutine to integrate, f(r)
    real, intent(in) :: a,b               ! Integration boundaries
    integer, intent(in) :: Nr             ! Number of steps
    real, intent(out) :: Minimum          ! Integral
    real :: dr                            ! Change in r
    real :: r                             ! The intergation variable
    real :: f_val                         ! Function value
    integer :: i

    ! Initialize the routine
    Minimum=0.0
    dr=(b-a)/Nr
    r=a

    ! We now perform a trapezoidal integration
    do i= 1, Nr

       call  func(nc,T,V,n,r,f_val)

       if (f_val<Minimum) then
          Minimum=f_val
       end if

       ! Update r
       r=r+dr
    end do
  end subroutine find_approximate_epsilon_eff

  subroutine testing_U(nc,T,V,n,r,out_var)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    real, intent(out) :: out_var
    real :: g, U_divk
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()

    call calc_hardsphere_rdf_and_U(nc,T,n,V,r,svrm_var,g,U_divk)
    out_var=U_divk
  end subroutine testing_U

  subroutine testing_a1_term(nc,T,V,n,r,out_var)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    real, intent(out) :: out_var
    real :: g, U_divk
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()

    call calc_hardsphere_rdf_and_U(nc,T,n,V,r,svrm_var,g,U_divk)
    out_var=g*U_divk*r**2
  end subroutine testing_a1_term
  subroutine testing_a2_term(nc,T,V,n,r,out_var)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    real, intent(out) :: out_var
    real :: g, U_divk
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()

    call calc_hardsphere_rdf_and_U(nc,T,n,V,r,svrm_var,g,U_divk)
    out_var=g*(U_divk**2)*r**2
  end subroutine testing_a2_term

  subroutine a1_term_integral_argument(nc,T,V,n,r,spec,out_var)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    integer, intent(in) :: spec    ! 0= Mie-Term, 1=Q1, 2=Q2
    real, intent(out) :: out_var
    real :: g, U_divk

    call get_rdf(nc,T,V,n,r,g)
    call get_u_part(T,r,spec,U_divk)

    out_var=g*U_divk*r**2

  end subroutine a1_term_integral_argument

  subroutine a2_term_integral_argument(nc,T,V,n,r,spec,out_var)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    integer, intent(in) :: spec    ! 0= Mie-Term, 1=Q1, 2=Q2
    real, intent(out) :: out_var
    real :: g, U_divk, U_divk_1, U_divk_2

    call get_rdf(nc,T,V,n,r,g)

    if (spec<3) then
       call get_u_part(T,r,spec,U_divk)
       out_var=g*(U_divk**2)*r**2
    elseif (spec==3) then
       call get_u_part(T,r,0,U_divk_1)
       call get_u_part(T,r,1,U_divk_2)
       out_var=g*(U_divk_1*U_divk_2)*r**2
    end if

  end subroutine a2_term_integral_argument

  subroutine a2_FH_integrand(nc,T,V,n,r,spec,out_var)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    integer, intent(in) :: spec ! 0=MiePotential, 1=FH1Potential, 2=FH2Potential
    real, intent(out) :: out_var
    real :: g, U_divk

    call get_rdf(nc,T,V,n,r,g)
    call get_FH_potential(T,r,spec,U_divk)
    out_var=g*(U_divk**2)*r**2

  end subroutine a2_FH_integrand

  subroutine get_rdf(nc,T,V,n,r,g)
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    real, intent(out) :: g
    real :: eta

    eta= pi*N_AVOGADRO*n(1)*(r**3)*saftvrmie_param%ms(1)/(6.0*V)
    g=(1.0-0.5*eta)/((1.0-eta)**3)

  end subroutine get_rdf

  subroutine testing_virial_B_term(nc,T,V,n,r,spec,out_var)
    use numconstants, only: expMax
    ! Variables
    integer, intent(in) :: nc
    real, intent(in) :: T, V, n(nc), r
    integer, intent(in) :: spec    ! 0= Mie-Term, 1=Q1, 2=Q2
    real, intent(out) :: out_var
    real :: U_divk, arg

    if (r > 1.0e-300) then
       call get_FH_potential(T,r,spec,U_divk)
    else
       U_divk = expMax
    endif
    arg = 1.0 - exp(-U_divk/T)
    out_var=arg*r**2
  end subroutine testing_virial_B_term

  subroutine get_u_part(T,r,spec,U_divk)
    ! Variables
    real, intent(in) :: T, r
    integer, intent(in) :: spec
    real, intent(out) :: U_divk
    real :: n, m, r_2, D, D2, r_4
    real :: en_r, sigma_r, r_n, r_m
    real :: Q1_n, Q1_m, Q2_n, Q2_m
    real :: product_q1, product_q2

    Q1_m=saftvrmie_param%Quantum_const_1a_ij(1,1)
    Q1_n=saftvrmie_param%Quantum_const_1r_ij(1,1)
    Q2_m=saftvrmie_param%Quantum_const_2a_ij(1,1)
    Q2_n=saftvrmie_param%Quantum_const_2r_ij(1,1)

    ! The repulsive and attracive exponents
    n=saftvrmie_param%comp(1)%lambda_r
    m=saftvrmie_param%comp(1)%lambda_a

    ! The inverse radii
    en_r=1/r
    sigma_r=saftvrmie_param%comp(1)%sigma*en_r

    ! Powers of the inverse radii and derivs
    r_n=(sigma_r)**n
    r_m=(sigma_r)**m

    if (spec==0) then
       U_divk=r_n-r_m
    elseif (spec==1) then

       r_2=en_r**2

       D=(h_const**2)/(48.0*(pi**2)*saftvrmie_param%comp(1)%mass*kB_const*T)

       ! The modified interaction potential and derivatives
       product_q1=r_2*(Q1_n*r_n-Q1_m*r_m)
       U_divk=D*product_q1

    elseif(spec==2) then

       r_4=en_r**4
       D2=((h_const**2)&
            /(48.0*(pi**2)*saftvrmie_param%comp(1)%mass*kB_const*T))**2

       ! The modified interaction potential and its T-derivatives
       product_q2=r_4*(Q2_n*r_n-Q2_m*r_m)

       ! The interaction potential
       U_divk=D2*product_q2
    else
       U_divk=0
    end if

    ! Multiply everything by the appropriate prefactor:
    U_divk=U_divk*mie_c_factor(saftvrmie_param%comp(1)%lambda_r,&
         saftvrmie_param%comp(1)%lambda_a)*&
         saftvrmie_param%comp(1)%eps_depth_divk
  end subroutine get_u_part

  subroutine get_FH_potential(T,r,spec,U_divk)
    ! Variables
    real, intent(in) :: T, r
    integer, intent(in) :: spec
    real, intent(out) :: U_divk
    real :: n, m, r_2, D, D2, r_4
    real :: en_r, sigma_r, r_n, r_m
    real :: Q1_n, Q1_m, Q2_n, Q2_m
    real :: product_q1, product_q2

    Q1_m=saftvrmie_param%Quantum_const_1a_ij(1,1)
    Q1_n=saftvrmie_param%Quantum_const_1r_ij(1,1)
    Q2_m=saftvrmie_param%Quantum_const_2a_ij(1,1)
    Q2_n=saftvrmie_param%Quantum_const_2r_ij(1,1)

    ! The repulsive and attracive exponents
    n=saftvrmie_param%comp(1)%lambda_r
    m=saftvrmie_param%comp(1)%lambda_a

    ! The inverse radii
    en_r=1/r
    sigma_r=saftvrmie_param%comp(1)%sigma*en_r

    ! Powers of the inverse radii and derivs
    r_n=(sigma_r)**n
    r_m=(sigma_r)**m
    U_divk=r_n-r_m

    if (spec>=1) then
       r_2=en_r**2
       D=(h_const**2)/(48.0*(pi**2)*saftvrmie_param%comp(1)%mass*kB_const*T)
       product_q1=r_2*(Q1_n*r_n-Q1_m*r_m)
       U_divk = U_divk + D*product_q1
    end if

    if(spec>=2) then
       r_4=en_r**4
       D2=((h_const**2)&
            /(48.0*(pi**2)*saftvrmie_param%comp(1)%mass*kB_const*T))**2
       product_q2=r_4*(Q2_n*r_n-Q2_m*r_m)
       U_divk = U_divk + D2*product_q2
    end if

    ! Multiply everything by the appropriate prefactor:
    U_divk=U_divk*mie_c_factor(saftvrmie_param%comp(1)%lambda_r,&
         saftvrmie_param%comp(1)%lambda_a)*&
         saftvrmie_param%comp(1)%eps_depth_divk
  end subroutine get_FH_potential

  subroutine map_second_virial_coeff()
    use saftvrmie_containers, only: init_saftvrmie_containers, get_saftvrmie_eos_pointer
    use saftvrmie_interface, only:  preCalcSAFTVRMie
    implicit none
    integer :: mixing, nIter, i
    real :: T, V, n(nc), Int_B, Int_B1, Int_B2, Tmin, Tmax
    type(eos_container), pointer :: p_act_eosc
    type(saftvrmie_var_container), pointer :: saftvrmie_var
    type(saftvrmie_eos), pointer :: eos
    p_act_eosc => get_active_eos_container()

    mixing=1
    T=4.0
    V=1.0
    n(1)=2

    Tmin = 3.7
    Tmax = 26.0
    open(unit=20,file="virial_B_int.dat")
    nIter = 100
    eos => get_saftvrmie_eos_pointer(p_act_eosc%eos(1)%p_eos)
    saftvrmie_var => eos%saftvrmie_var
    call init_saftvrmie_containers(nc,p_act_eosc%comps,eos,&
         "DEFAULT",mixing)
    do i=1,nIter
       T = Tmin + (Tmax-Tmin)*real(i-1)/(nIter-1)
       quantum_correction=0
       quantum_correction_hs=0
       call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var)
       call calc_virial_B_by_integration(nc,T,saftvrmie_var,Int_B,.false.)
       quantum_correction=1
       quantum_correction_hs=1
       call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var)
       call calc_virial_B_by_integration(nc,T,saftvrmie_var,Int_B1,.false.)
       quantum_correction=2
       quantum_correction_hs=2
       call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var)
       call calc_virial_B_by_integration(nc,T,saftvrmie_var,Int_B2,.false.)
       write(20,*) T, Int_B, Int_B1, Int_B2
    enddo
    close(20)
    stop

  end subroutine map_second_virial_coeff

end module saftvrmie_testing
