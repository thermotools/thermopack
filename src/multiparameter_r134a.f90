module multiparameter_r134a
  !> Tiller-Roth, R., and H. D. Baehr.
  !> "An International Standard Formulation for the Thermodynamic Properties of
  !> 1, 1, 1, 2-Tetrafluoroethane (HCF-134a) from 170 K to 455 K and Pressure up to 70 MPa."
  !> Journal Physics Chemistry Reference Data 23, no. 5 (1994): 657-729.
  use multiparameter_base, only: meos
  implicit none
  save
  public :: meos_r134a
  private

  integer, parameter :: resTerms = 21 ! Number of terms in residual Helm. energy
  integer, parameter :: a0Terms = 5 ! Number of terms in ideal Helm. energy

  ! Parameters for ideal gas part of the Helmholtz energy
  real, parameter, dimension(1:a0Terms) :: a0i =(/-1.019535,&
       9.047135, -1.629789, -9.723916, -3.927170/)

  ! Parameters for residual part of the Helmholtz energy
  real, parameter, dimension(1:resTerms) :: ai = (/0.5586817000e-01, 0.4982230000e+00,&
       0.2458698000e-01, 0.8570145000e-03, 0.4788584000e-03, -0.1800808000e+01, 0.2671641000e+00,&
       -0.4781652000e-01, 0.1423987000e-01, 0.3324062000e+00, -0.7485907000e-02, 0.1017263000e-03,&
       -0.5184567000e+00, -0.8692288000e-01, 0.2057144000e+00, -0.5000457000e-02, 0.4603262000e-03,&
       -0.3497836000e-02, 0.6995038000e-02, -0.1452184000e-01, -0.1285458000e-03/)

  real, parameter, dimension(1:resTerms) :: ti = (/-0.50, 0.00, 0.00,&
       0.00, 1.50, 1.50, 2.00, 2.00, 1.00, 3.00, 5.00, 1.00, 5.00, 5.00, 6.0,&
       10.0, 10.0, 10.0, 18.0, 22.0, 50./)

  integer, parameter, dimension(1:resTerms) :: di = (/2, 1, 3,&
       6, 6, 1, 1, 2, 5, 2, 2, 4, 1, 4,&
       1, 2, 4, 1, 5, 3, 10/)

  integer, parameter, dimension(1:resTerms) :: li = (/0, 0, 0, 0, 0, 0,&
       0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4/)


  ! Parameters for the ancillary equations to compute saturation densities
  real, parameter, dimension(4) :: N_liqsat = (/ 518.20, 884.13, 485.84, 193.29/)
  real, parameter, dimension(4) :: expo_liqsat = (/ 0.0, 0.33333, 0.66667, 3.33333/)
  real, parameter, dimension(5) :: N_vapsat = (/ -2.837294, -7.875988, 4.478586, -14.140125, -52.361297/)
  real, parameter, dimension(5) :: expo_vapsat = (/ 0.33333, 0.66667, 0.5, 2.5, 5.5/)

  !> R134A multiparameter equation of state (R. Tillner-Roth and H. Dieter Baehr).
  type, extends(meos) :: meos_r134a
     private

   contains

     procedure, public :: alpha0Derivs_taudelta => alpha0Derivs_r134a
     procedure, public :: alphaResDerivs_taudelta => alphaResDerivs_r134a
     procedure, public :: satDeltaEstimate => satDeltaEstimate_r134a
     procedure, public :: init => init_r134a

     ! Assignment operator
     procedure, pass(This), public :: assign_meos => assign_meos_r134a

  end type meos_r134a

contains

  subroutine init_r134a (this, use_Rgas_fit)
    use thermopack_var, only: get_active_thermo_model, thermo_model
    class(meos_r134a) :: this
    logical, optional, intent(in) :: use_Rgas_fit
    ! Locals
    type(thermo_model), pointer :: p_thermo

    this%compName = "R134A"
    this%tc = 374.21  !< (K)
    this%rc = 4978.8301!5.017053e3    !< (mol/m^3)
    this%pc = 4059.28e3 !< (Pa)
    this%acf = 0.32684

    this%t_triple = 169.85  !< (K)
    this%p_triple = 389.6 !< (Pa)
    this%rhoLiq_triple = 15.5942e3  !< (mol/m^3)

    this%molarMass = 102.032e-3  !< (kg/mol)
    this%Rgas_fit = 8.314471 !< (J/(mol*K))

    this%maxT = 455.0 ! (T)
    this%maxP = 70.0e6 ! (Pa)

    if (present(use_Rgas_fit)) then
      if (use_Rgas_fit) then
        this%Rgas_meos = this%Rgas_fit
      end if
    end if

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    p_thermo%Rgas = this%Rgas_meos
    p_thermo%kRgas = 1000.0*this%Rgas_meos !< J/kmol/K
  end subroutine init_r134a

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  subroutine alpha0Derivs_r134a(this, delta, tau, alp0)
    class(meos_r134a) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alp0(0:2,0:2) !< alp0(i,j) = [(d_delta)^i(d_tau)^j alpha0]*delta^i*tau^j
    ! Internals
    ! Precalculate exponentials

    alp0 = 0.0
    alp0(0,0) = a0i(1) + a0i(2)*tau+a0i(3)*log(tau)+log(delta)&
         +a0i(4)*tau**(-0.5) + a0i(5)*tau**(-0.75)
    alp0(1,0) = 1.0
    alp0(2,0) = -1.0
    alp0(0,1) = a0i(2)*tau + a0i(3) -0.5*a0i(4)*tau**(-0.5) - 0.75*a0i(5)*tau**(-0.75)
    alp0(0,2) = -a0i(3) + 0.75*a0i(4)*tau**(-0.5) + 1.3125*a0i(5)*tau**(-0.75)

  end subroutine alpha0Derivs_r134a


  subroutine alphaResDerivs_r134a (this, delta, tau, alpr)
    class(meos_r134a) :: this
    real, intent(in) :: delta, tau
    real, intent(out) :: alpr(0:2,0:2) !< alpr(i,j) = (d_delta)^i(d_tau)^j alphaRes
    ! Internal
    real :: prefactors(1:resTerms)
    real :: prefactors_delta(1:resTerms)
    real :: prefactors_deltadelta(1:resTerms)
    real :: prefactors_tau(1:resTerms)
    real :: prefactors_tautau(1:resTerms)
    real :: prefactors_deltatau(1:resTerms)

    ! Precalculate terms
    prefactors(1:resTerms) = ai(:)*tau**ti(:)*delta**di(:)
    prefactors(9:resTerms) = prefactors(9:resTerms)*exp(-delta**li(9:resTerms))
    prefactors_delta = (di(:) - li(:)*delta**li(:)) ! Includes *delta
    prefactors_deltadelta = prefactors_delta(:)**2 +(-di(:)-li(:)*(li(:)-1.0)*delta**li(:)) !Includes *delta**2
    prefactors_tau = ti(:) !Includes *tau
    prefactors_tautau = prefactors_tau(:)**2-ti(:) !Includes *tau**2
    prefactors_deltatau = prefactors_delta(:)*prefactors_tau(:) !Includes *delta*tau



    ! alpha
    alpr(0,0) = sum(prefactors)

    ! delta*alpha_delta
    alpr(1,0) = dot_product(prefactors, prefactors_delta)

    ! delta**2 * alpha_deltadelta
    alpr(2,0) = dot_product(prefactors, prefactors_deltadelta)

    ! tau*alpha_tau
    alpr(0,1) = dot_product(prefactors, prefactors_tau)

    ! tau**2 * alpha_tautau
    alpr(0,2) = dot_product(prefactors, prefactors_tautau)

    ! delta*tau*alpha_deltatau
    alpr(1,1) = dot_product(prefactors, prefactors_deltatau)

  end subroutine alphaResDerivs_r134a

  function satDeltaEstimate_r134a (this,tau,phase) result(deltaSat)
    use thermopack_constants, only: LIQPH, VAPPH
    class(meos_r134a) :: this
    real, intent(in) :: tau
    integer, intent(in) :: phase
    real :: deltaSat
    ! Internals
    real :: theta

    theta = 1-1/tau
    if ( phase == LIQPH ) then
       deltaSat = dot_product(N_liqsat,theta**expo_liqsat)/(this%molarMass*this%rc)
    else if ( phase == VAPPH ) then
       deltaSat = exp(dot_product(N_vapsat,theta**expo_vapsat))*516.86/(this%molarMass*this%rc)
    else
       call stoperror("satDeltaEstimate_r134a: only LIQPH and VAPPH allowed!")
    end if

  end function satDeltaEstimate_r134a

  subroutine assign_meos_r134a(this,other)
    class(meos_r134a), intent(inout) :: this
    class(*), intent(in) :: other
    !
    select type (other)
    class is (meos_r134a)
      call this%assign_meos_base(other)
    class default
      call stoperror("assign_meos_r134a: Should not be here....")
    end select
  end subroutine assign_meos_r134a

end module multiparameter_r134a
