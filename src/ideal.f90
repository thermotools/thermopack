module ideal
  !> Ideal gas properties.
  !! Available CP-ideal correlations can vary, depending on the fluid.
  !!
  !! The ones that are in use are:
  !!\verbatim
  !! CPTYPE   - METHOD FOR IDEAL-GAS HEAT-CAPACITY CALCULATION                *
  !!             - 1 : SHERWOOD, REID & PRAUSNITZ, THIRD EDITION              *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3                    (cal/gmol K) *
  !!             - 2 : API-PROJECT 44                                         *
  !!             - 3 : HYPOTETIC COMPONENTS                                   *
  !!             - 4 : SHERWOOD, REID & PRAUSNITZ, FOURTH EDITION             *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3                    (J/mol K)    *
  !!             - 5 : ICI (KRISTER STR\M)                                    *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3 + CP(5)/T**2           (kJ/kgK) *
  !!             - 6 : CHEN, BENDER (PETTER NEKSÅ)                            *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
  !!                               CP(4)*T**3+ CP(5)*T**4        (kJ/kg K)    *
  !!             - 7 : AIChE, Daubert and Danner, DIPPR-databasen             *
  !!                   CP(ideal) = A + B[(C/T)/sinh(C/T)]**2                  *
  !!                               + D[(E/T)/cosh(E/T)]**2      (J/(kmol K))  *
  !!             - 8 : POLING, PRAUSNITZ & O'CONNEL, FIFTH EDITION            *
  !!                   CP(ideal)/R = CP(1) + CP(2)*T + CP(3)*T**2 +           *
  !!                               CP(4)*T**3 + CP(5)*T**4       (-)          *
  !!             - 9 : Linear function and fraction (J/mol/K)                 *
  !!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)/(T + CP(4))        *
  !!                                                                          *
  !!             -10 : Leachman (NIST) and Valenta expression H2              *
  !!                                                                          *
  !!             -11 : Shomate Equation (Note that: Ts=T/1000)                *
  !!                   CP(ideal) = CP(1) + CP(2)*Ts + CP(3)*Ts**2 +           *
  !!                               CP(4)*Ts**3 + CP(5)/Ts**2         (J/molK) *
  !! \endverbatim
  use thermopack_var, only: Rgas
  implicit none
  save

  !> Log cut-off value
  real, parameter :: logCutOff = 1.0e-100

  public:: EstPsat, TP_Kideal, CPideal, Hideal, Sideal_T,&
       CPideal_mix, Hideal_mix, TP_Sideal_mix, TV_Yideal_mix, &
       Sideal_Vn, TV_Sideal_mix, Fideal_mix_SI
  public :: Hideal_apparent, TP_Sideal_apparent, Cpideal_apparent
  public :: idealEntropy_ne
  public :: set_entropy_reference_value, get_entropy_reference_value
  public :: set_enthalpy_reference_value, get_enthalpy_reference_value
  public :: set_reference_energies

contains
  !---------------------------------------------------------------------- >
  !> The function returns the vapour pressure for a pure component
  !! for the compoentent that is found
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval Psat The vapour pressure [Pa]
  !!
  !! \author Oivind W

  function EstPsat(comp, i, T) result (Psat)
    use thermopack_constants
    use compdata
    use idealh2, only: lnpvapred_H2

    implicit none
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: Psat, Term

    method_psat: select case (comp(i)%p_comp%psatcode)
    case (1) ! Antoine vapour-pressure calculation
      write(*,*) ' Tmax:  ', comp(i)%p_comp%tantmax, '  Tmin ', comp(i)%p_comp%tantmin

      if(T>comp(i)%p_comp%tantmin .AND. T<comp(i)%p_comp%tantmax) then
        Psat=exp(comp(i)%p_comp%ant(1)-comp(i)%p_comp%ant(2)/(T+comp(i)%p_comp%ant(3)))
        Psat=Psat*133.3224 ! Input in K, output in Pa. Ant-coeffcients give psat in mmHg
        ! psat = [mmHG] * [rhoHg * gGrav / 1000.0]
      else
        Psat=comp(i)%p_comp%pc*exp(5.42*(1.0-comp(i)%p_comp%tc/T))
        if(Psat<1.0E-6) then
          Psat=1.0E-6
        endif
      endif
    case (2)  ! Michelsen approach
      Psat=comp(i)%p_comp%pc*exp(5.42*(1.0-comp(i)%p_comp%tc/T))
      if(Psat<1.0D-6) then
        Psat=1.0D-6
      endif
    case (3)   ! Starling approach
      if( T .LE. comp(i)%p_comp%tc ) then
        Psat=(4.92*comp(i)%p_comp%acf+5.81)*LOG(T/comp(i)%p_comp%tc) - &
             0.0838*(4.92*comp(i)%p_comp%acf+2.06)*(36.0/(T/comp(i)%p_comp%tc)-&
             35.0-(T/comp(i)%p_comp%tc)**6+42.0*LOG(T/comp(i)%p_comp%Tc))
      else
        Term=-8.68*((T/comp(i)%p_comp%tc)-1.8+6.2*comp(i)%p_comp%zc)**2
        if (Term .GT. -30.0) then
          Term = 10.0**Term
        else
          Term = 0.0
        end if

        Psat= -(16.26-73.85*comp(i)%p_comp%zc+90.0*comp(i)%p_comp%zc**2)*&
             (1.0-(T/comp(i)%p_comp%tc))/(T/comp(i)%p_comp%tc) - Term
      end if

      Psat = comp(i)%p_comp%pc*EXP(Psat)

    case (10) ! Leachman ancillary equation for pvap for P-H2, N-H2 and O-H2
      Psat = lnpvapred_H2(comp(i)%p_comp%ident, T/comp(i)%p_comp%tc) ! Returns ln(psat/pcrit)
      psat = exp(psat)*comp(i)%p_comp%pc

    end select method_psat
  end function EstPsat

  !---------------------------------------------------------------------- >
  !> The subroutine returns the ideal K-values for vapour-liquid
  !! equilibria
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \retval K The K-value
  !!
  !! \author Oivind W

  subroutine TP_Kideal (nc, comp, T, P, K)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T, P
    real, dimension(nc), intent(out) :: K
    integer :: i

    do i=1,nc
      K(i)=EstPsat(comp,i,T)/P
    end do

  end subroutine TP_Kideal

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas heat capacity for
  !! a apparent component, i.
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval Cp The ideal gas heat capacity [J/mol K]
  !!
  !! \author M. Hammer
  function CPideal_apparent(comp, i, T) result (Cp_id)
    use compdata, only: gendata_pointer
    use thermopack_var, only: nce, ncsym, apparent
    implicit none
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: Cp_id
    ! Locals
    integer :: j
    if (i <= ncsym) then
      Cp_id = CPideal(comp(i)%p_comp, i, T)
    else
      Cp_id = 0.0
      do j=ncsym+1,nce
        if (apparent%v_stoich(i,j) > 0.0) then
          Cp_id = Cp_id + CPideal(comp(j)%p_comp, j, T)*apparent%v_stoich(i,j)
        endif
      enddo
    endif
  end function CPideal_apparent

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas heat capacity for a component, i.
  !> See tpgendat.f90 for details of the correlations.
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval Cp The ideal gas heat capacity [J/mol K]
  function CPideal(comp, i, T) result (Cp_id)
    use compdata, only: gendata, CP_POLY3_CAL, &
         CP_API44_MASS, CP_HYPOTETIC_MASS, CP_POLY3_SI, &
         CP_ICI_MASS, CP_CHEN_BENDER_MASS, CP_DIPPR_KMOL, &
         CP_POLY4_SI, CP_MOGENSEN_SI, CP_H2_KMOL, &
         CP_SHOMATE_SI
    use thermopack_constants, only: verbose
    use idealh2, only: cpideal_h2
    implicit none
    type(gendata), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: Cp_id
    !
    real :: TminCp, TmaxCp, Ts

    method_Cp: select case (comp%id_cp%cptype)
    case (CP_POLY3_CAL) ! Third degree poynomial

      Cp_id=comp%id_cp%cp(1)+comp%id_cp%cp(2)*T+comp%id_cp%cp(3)*T**2+comp%id_cp%cp(4)*T**3
      Cp_id=Cp_id*4.1868

    case (CP_API44_MASS) ! API-project 44

      Cp_id = comp%id_cp%cp(2) + T*(2.0*comp%id_cp%cp(3)+ T*(3.0*comp%id_cp%cp(4)+ &
           T*(4.0*comp%id_cp%cp(5)+ T*5.0*comp%id_cp%cp(6))))
      Cp_id = Cp_id*comp%mw

      TminCp = comp%id_cp%tcpmin + 273.15
      TmaxCp = comp%id_cp%tcpmax + 273.15

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperaure range for component ', trim(comp%ident)
        endif
      end if

    case (CP_HYPOTETIC_MASS) ! Hypotetic components

      Cp_id=comp%id_cp%cp(1)+comp%id_cp%cp(2)*T*1.8+comp%id_cp%cp(3)*(T*1.8)**2
      Cp_id=Cp_id*4.1868*comp%mw

    case (CP_POLY3_SI) ! Third degree polynomial (different units)

      Cp_id=comp%id_cp%cp(1)+comp%id_cp%cp(2)*T+comp%id_cp%cp(3)*T**2+comp%id_cp%cp(4)*T**3
      Cp_id=Cp_id

    case (CP_ICI_MASS) ! Third degree polynomial + 1/T**2 term

      Cp_id=comp%id_cp%cp(1)+comp%id_cp%cp(2)*T+comp%id_cp%cp(3)*T**2+comp%id_cp%cp(4)*T**3+ &
           comp%id_cp%cp(5)/(T**2)
      Cp_id=Cp_id*comp%mw

    case (CP_CHEN_BENDER_MASS) ! Fourth degree polynomial

      Cp_id=comp%id_cp%cp(1) + comp%id_cp%cp(2)*T + comp%id_cp%cp(3)*T**2 + comp%id_cp%cp(4)*T**3 + &
           comp%id_cp%cp(5)*T**4
      Cp_id=Cp_id*comp%mw

    case (CP_DIPPR_KMOL) ! DIPPR-database

      Cp_id = comp%id_cp%cp(1)+ &
           comp%id_cp%cp(2)*(( comp%id_cp%cp(3)/T)/sinh( comp%id_cp%cp(3)/T))**2 +&
           comp%id_cp%cp(4)*(( comp%id_cp%cp(5)/T)/cosh( comp%id_cp%cp(5)/T))**2
      Cp_id = 1.0e-3*Cp_id ! J/kmol/K -> J/mol/K
      TminCp = comp%id_cp%tcpmin
      TmaxCp = comp%id_cp%tcpmax

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp%ident)
        endif
      end if

    case (CP_POLY4_SI) ! Fourth degree polynomial (different units).
      Cp_id=comp%id_cp%cp(1) + comp%id_cp%cp(2)*T + comp%id_cp%cp(3)*T**2 + comp%id_cp%cp(4)*T**3 + &
           comp%id_cp%cp(5)*T**4
      Cp_id=Cp_id*rgas

    case (CP_MOGENSEN_SI) ! Linear function and fraction (J/mol/K).
      Cp_id=comp%id_cp%cp(1) + comp%id_cp%cp(2)*T + comp%id_cp%cp(3)/(T + comp%id_cp%cp(4))

    case (CP_H2_KMOL) ! Leachman (NIST) and Valenta expression for N-H2 , O-H2, P-H2 and E-H2 (Valenta)
      Cp_id = CPideal_H2(comp%ident, T)

    case (CP_SHOMATE_SI) ! Third degree polynomial + 1/T**2 term, Ts=T/1000
      Ts = T*1.0e-3
      Cp_id=comp%id_cp%cp(1)+comp%id_cp%cp(2)*Ts+comp%id_cp%cp(3)*Ts**2+comp%id_cp%cp(4)*Ts**3+ &
           comp%id_cp%cp(5)/(Ts**2)

    end select method_Cp
  end function CPideal

  !---------------------------------------------------------------------- >
  !> The function returns ideal enthalpy for
  !! a apparent component, i.
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval h The ideal enthalpy [J/mol]
  !!
  !! \author M. Hammer
  function Hideal_apparent(comp, i, T) result (H_id)
    use compdata
    use thermopack_var, only: nce, ncsym, apparent
    implicit none
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: H_id
    ! Locals
    integer :: j
    if (i <= ncsym) then
      H_id = Hideal(comp(i)%p_comp, i, T)
    else
      H_id = 0.0
      do j=ncsym+1,nce
        if (apparent%v_stoich(i,j) > 0.0) then
          H_id = H_id + Hideal(comp(j)%p_comp, j, T)*apparent%v_stoich(i,j)
        endif
      enddo
    endif
  end function Hideal_apparent

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas enthalpy for component i
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval H_id The ideal gas enthalpy [J/mol]
  !!
  !! \author Oivind W
  function Hideal(comp, i, T) result (H_id)
    use compdata, only: gendata, CP_POLY3_CAL, &
         CP_API44_MASS, CP_HYPOTETIC_MASS, CP_POLY3_SI, &
         CP_ICI_MASS, CP_CHEN_BENDER_MASS, CP_DIPPR_KMOL, &
         CP_POLY4_SI, CP_MOGENSEN_SI, CP_H2_KMOL, &
         CP_SHOMATE_SI
    use thermopack_constants, only: verbose
    use idealh2, only: hideal_h2
    implicit none
    type(gendata), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: H_id, TminCp, TmaxCp, Ts

    method_H: select case (comp%id_cp%cptype)
    case (CP_POLY3_CAL) ! Third degree Cp-poynomial

      H_id=comp%id_cp%cp(1)*T+0.5*comp%id_cp%cp(2)*T**2+comp%id_cp%cp(3)*T**3/3.0+&
           0.25*comp%id_cp%cp(4)*T**4
      H_id=H_id*4.1868+comp%href

    case (CP_API44_MASS) ! API-project 44

      H_id = comp%id_cp%cp(1)+T*(comp%id_cp%cp(2)+T*(comp%id_cp%cp(3)+&
           T*(comp%id_cp%cp(4)+T*(comp%id_cp%cp(5)+T*comp%id_cp%cp(6)))))
      H_id = H_id*comp%mw+comp%href

      TminCp = comp%id_cp%tcpmin + 273.15
      TmaxCp = comp%id_cp%tcpmax + 273.15

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperaure range for component ', trim(comp%ident)
        endif
      end if

    case (CP_HYPOTETIC_MASS) ! Hypotetic components

      H_id=comp%id_cp%cp(1)*T*1.8+comp%id_cp%cp(2)*(T*1.8)**2/2.0+&
           comp%id_cp%cp(3)*(T*1.8)**3/3.0
      H_id=H_id*(4.1868/1.8)*comp%mw+comp%href

    case (CP_POLY3_SI) ! Third degree Cp-polynomial (different units)

      H_id=comp%id_cp%cp(1)*T+comp%id_cp%cp(2)*T**2/2.0+&
           comp%id_cp%cp(3)*T**3/3.0+comp%id_cp%cp(4)*T**4/4.0
      H_id=H_id+comp%href

    case (CP_ICI_MASS) ! Third degree polynomial + 1/T**2 term

      H_id=comp%id_cp%cp(1)*T+comp%id_cp%cp(2)*T**2.0/2.0 + &
           comp%id_cp%cp(3)*T**3/3.0+comp%id_cp%cp(4)*T**4/4.0 - &
           comp%id_cp%cp(5)/T
      H_id=H_id*comp%mw+comp%href

    case (CP_CHEN_BENDER_MASS) ! Fourth degree Cp-polynomial

      H_id=comp%id_cp%cp(1)*T+comp%id_cp%cp(2)*T**2.0/2.0 + &
           comp%id_cp%cp(3)*T**3/3.0+comp%id_cp%cp(4)*T**4/4.0 + &
           comp%id_cp%cp(5)*T**5/5.0
      H_id=H_id*comp%mw+comp%href

    case (CP_DIPPR_KMOL) ! DIPPR-database

      H_id = comp%id_cp%cp(1)*T+&
           (cosh(comp%id_cp%cp(3)/T)*comp%id_cp%cp(3)*comp%id_cp%cp(2))/(sinh(comp%id_cp%cp(3)/T))-&
           (sinh(comp%id_cp%cp(5)/T)*comp%id_cp%cp(5)*comp%id_cp%cp(4))/(cosh(comp%id_cp%cp(5)/T))
      H_id=1.0e-3*H_id+comp%href


      TminCp = comp%id_cp%tcpmin
      TmaxCp = comp%id_cp%tcpmax

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp%ident)
        endif
      end if

    case (CP_POLY4_SI) ! Fourth degree Cp-polynomial (different units)

      H_id=comp%id_cp%cp(1)*T+comp%id_cp%cp(2)*T**2.0/2.0 + &
           comp%id_cp%cp(3)*T**3/3.0+comp%id_cp%cp(4)*T**4/4.0 + &
           comp%id_cp%cp(5)*T**5/5.0
      H_id=H_id*rgas+comp%href

    case (CP_MOGENSEN_SI) ! Linear function and fraction (J/mol).
      H_id=comp%id_cp%cp(1)*T+comp%id_cp%cp(2)*T**2.0/2.0 + &
           comp%id_cp%cp(3)*log(T+comp%id_cp%cp(4))
      H_id=H_id + comp%href

    case (CP_H2_KMOL) ! Leachman (NIST) and Valenta expression for N-H2 , O-H2, P-H2 and E-H2 (Valenta)
      H_id = Hideal_H2 (comp%ident, T) + comp%href

    case (CP_SHOMATE_SI) ! Third degree polynomial + 1/T**2 term, Ts=T/1000
      Ts = T*1.0e-3
      H_id=comp%id_cp%cp(1)*Ts+comp%id_cp%cp(2)*Ts**2.0/2.0 + &
           comp%id_cp%cp(3)*Ts**3/3.0+comp%id_cp%cp(4)*Ts**4/4.0 - &
           comp%id_cp%cp(5)/Ts
      H_id=H_id*1.0e3 + comp%href
    end select method_H

  end function Hideal

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas entropy for component i
  !! NOT including P or V contribution
  !!
  !! \param i The number of the component
  !! \param T Temperature [K]
  !! \retval S_id Ideal gas entropy [J/mol K]
  !!
  !! \author Oivind W

  function Sideal_T(comp, i, T) result (S_id)
    use compdata, only: gendata, CP_POLY3_CAL, &
         CP_API44_MASS, CP_HYPOTETIC_MASS, CP_POLY3_SI, &
         CP_ICI_MASS, CP_CHEN_BENDER_MASS, CP_DIPPR_KMOL, &
         CP_POLY4_SI, CP_MOGENSEN_SI, CP_H2_KMOL, &
         CP_SHOMATE_SI
    use thermopack_constants, only: verbose
    use idealh2, only: sideal_h2
    implicit none
    type(gendata), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    !
    real :: S_id, TminCp, TmaxCp, Ts

    method_S: select case (comp%id_cp%cptype)
    case (CP_POLY3_CAL) ! Third degree Cp-poynomial
      S_id = comp%id_cp%cp(1)*log(T)+comp%id_cp%cp(2)*T + &
           comp%id_cp%cp(3)*T**2.0/2.0+comp%id_cp%cp(4)*T**3.0/3.0
      S_id = S_id*4.1868
      S_id = S_id + comp%sref
    case (CP_API44_MASS) ! API-project 44
      S_id = comp%id_cp%cp(2)*log(T)+2.0*comp%id_cp%cp(3)*T+&
           3.0/2.0*comp%id_cp%cp(4)*T**2+4.0/3.0*comp%id_cp%cp(5)*T**3+&
           5.0/4.0*comp%id_cp%cp(6)*T**4
      S_id = S_id*comp%mw
      S_id = S_id + comp%sref
      TminCp = comp%id_cp%tcpmin + 273.15
      TmaxCp = comp%id_cp%tcpmax + 273.15
      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp%ident)
        endif
      end if
    case (CP_HYPOTETIC_MASS) ! Hypotetic components
      S_id = comp%id_cp%cp(1)*log(T*1.8)+comp%id_cp%cp(2)*T*1.8 +&
           comp%id_cp%cp(3)*(T*1.8)**2/2.0
      S_id = S_id*(4.1868)*comp%mw
      S_id = S_id + comp%sref
    case (CP_POLY3_SI) ! Third degree Cp-polynomial (different units)
      S_id = comp%id_cp%cp(1)*log(T)+comp%id_cp%cp(2)*T+&
           comp%id_cp%cp(3)*T**2.0/2.0+comp%id_cp%cp(4)*T**3/3.0
      S_id = S_id
      S_id = S_id + comp%sref
    case (CP_ICI_MASS) ! Third degree polynomial + 1/T**2 term
      S_id = comp%id_cp%cp(1)*log(T)+comp%id_cp%cp(2)*T +&
           comp%id_cp%cp(3)*T**2/2.0+comp%id_cp%cp(4)*T**3/3.0-&
           comp%id_cp%cp(5)/(2.0*T**2)
      S_id = S_id*comp%mw
      S_id = S_id + comp%sref
    case (CP_CHEN_BENDER_MASS) ! Fourth degree Cp-polynomial
      S_id = comp%id_cp%cp(1)*log(T)+comp%id_cp%cp(2)*T+&
           comp%id_cp%cp(3)*T**2/2.0+comp%id_cp%cp(4)*T**3/3.0+&
           comp%id_cp%cp(5)*T**4/4.0
      S_id = S_id*comp%mw
      S_id = S_id + comp%sref

    case (CP_DIPPR_KMOL) ! DIPPR-database
      S_id = comp%id_cp%cp(1)*log(T)+2*comp%id_cp%cp(3)/T*comp%id_cp%cp(2)+&
           2*comp%id_cp%cp(3)/T*comp%id_cp%cp(2)/(exp(comp%id_cp%cp(3)/T)**2-1)-&
           comp%id_cp%cp(2)*log(exp(comp%id_cp%cp(3)/T)**2-1)-&
           2*comp%id_cp%cp(5)/T*comp%id_cp%cp(4)+2*comp%id_cp%cp(5)/T*comp%id_cp%cp(4)/&
           (exp(comp%id_cp%cp(5)/T)**2+1)+comp%id_cp%cp(4)*log(exp(comp%id_cp%cp(5)/T)**2+1)
      S_id = 1.0e-3*S_id + comp%sref

      TminCp = comp%id_cp%tcpmin
      TmaxCp = comp%id_cp%tcpmax

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp%ident)
        endif
      end if
    case (CP_POLY4_SI) ! Fourth degree Cp-polynomial (different units)
      S_id = comp%id_cp%cp(1)*log(T)+comp%id_cp%cp(2)*T+&
           comp%id_cp%cp(3)*T**2/2.0+comp%id_cp%cp(4)*T**3/3.0+&
           comp%id_cp%cp(5)*T**4/4.0
      S_id = S_id*rgas
      S_id = S_id + comp%sref

    case (CP_MOGENSEN_SI) ! Linear function and fraction (J/mol/K).
      S_id = comp%id_cp%cp(1)*log(T)+comp%id_cp%cp(2)*T
      if (comp%id_cp%cp(4) == 0.0) then
        S_id = S_id-comp%id_cp%cp(3)/T
      else
        S_id = S_id+(comp%id_cp%cp(3)/comp%id_cp%cp(4))*(log(T)-log(T+comp%id_cp%cp(4)))
      endif
      S_id = S_id + comp%sref

    case (CP_H2_KMOL) !   ! Leachman (NIST) and Valenti expression for N-H2 , O-H2, P-H2 and E-H2 (Valenta)
      S_id = sideal_H2 (comp%ident, T) + comp%sref  ! + rgas * log(rho*T/ rho0*T0)

    case (CP_SHOMATE_SI) ! Third degree polynomial + 1/T**2 term, Ts=T/1000
      Ts = T*1.0e-3
      S_id = comp%id_cp%cp(1)*log(Ts)+comp%id_cp%cp(2)*Ts +&
           comp%id_cp%cp(3)*Ts**2/2.0+comp%id_cp%cp(4)*Ts**3/3.0-&
           comp%id_cp%cp(5)/(2.0*Ts**2)
      S_id=S_id + comp%sref

    end select method_S
  end function Sideal_T

  !---------------------------------------------------------------------- >
  !> The function returns pressure and composition contribution
  !! to ideal gas entropy
  !!
  !! \param n Mole numbers
  !! \param P The pressure [Pa]
  !! \retval s The ideal gas entropy [J/mol K]
  !! \param dsdt Temperature derivative [J/mol K^2]
  !! \param dsdp Spec. volume derivative [J/m3 Pa]
  !! \param dsdn Mole number differential [J/mol^2 K]
  !!
  !! \author MH, 10-2015

  subroutine Sideal_Pn(nc, n, P, s, dsdP, dsdn)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: P
    real, intent(out) :: s
    real, optional, intent(out) :: dsdP
    real, optional, dimension(nc), intent(out) :: dsdn
    ! Locals
    real :: sumn, logZi, z(nc), sn, logp
    integer :: i

    logp = log(p)
    sumn = sum(n)
    z = n/sumn
    sn = 0.0
    do i=1,nc
      if(abs(z(i)) .LT. logCutOff) then
        logZi = log(logCutOff)
      else
        logZi = log(z(i))
      end if
      sn = sn - rgas*n(i)*logZi
      if(present(dsdn)) then  ! composition derivative
        dsdn(i)=-rgas*(logZi + logp)
      end if
    end do
    s = -sumn*rgas*logp + sn

    if (present(dsdP)) then
      dsdP = -sumn*rgas/P
    endif

  end subroutine Sideal_Pn

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas entropy volume and
  !! compozition contribution
  !!
  !! \param n Mole numbers
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/mol]
  !! \retval s Ideal gas entropy [J/mol K]
  !! \param dsdt Temperature derivative [J/mol K^2]
  !! \param dsdv Spec. volume derivative [J/m^3 K]
  !! \param dsdn Mole number differential [J/mol^2 K]
  !! \param d2sdn2 Mole number differential [J/mol^3 K]
  !! \param d2sdv2 Volume differential [J/mol/K/m^6]
  !!
  !! \author GL, 23-01-2015
  !!         MH, 10-2015

  subroutine Sideal_Vn(nc, n, T, V, s, dsdT, dsdV, dsdn, d2sdndT, d2sdndV, &
       d2sdn2, d2sdV2, d2sdT2)
    implicit none
    integer, intent(in) :: nc
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, V
    real, intent(out) :: s
    real, optional, intent(out) :: dsdT, dsdV, d2sdndT, d2sdndV, d2sdV2, d2sdT2
    real, optional, dimension(nc), intent(out) :: dsdn, d2sdn2
    !
    real :: sumn, sn, logni, sv
    integer :: i

    sumn = sum(n)

    sv = log(V/(rgas*T))
    sn = 0.0
    do i=1,nc
      if(abs(n(i)) .LT. logCutOff) then
        logni = log(logCutOff)
      else
        logni = log(n(i))
      end if
      sn = sn - n(i)*logni
      if(present(dsdn)) then  ! composition derivative
        dsdn(i) = rgas*(sv - 1.0 - logni)
      end if
      if(present(d2sdn2)) then  ! second composition derivative
        if (n(i) > 0.0) then
          d2sdn2(i) = - rgas/n(i)
        else
          d2sdn2(i) = 0.0
        endif
      end if
    end do
    ! Add volume and composition contribution
    s = rgas*(sumn*sv + sn)
    if(present(dsdt)) then
      dsdt = -sumn*rgas/T
    end if
    if(present(d2sdt2)) then
      d2sdt2 = sumn*rgas/T**2
    end if

    if(present(dsdv)) then
      dsdv = sumn*rgas/V
    end if

    if(present(d2sdv2)) then
      d2sdv2 = -sumn*rgas/V**2
    end if

    if(present(d2sdndv)) then
      d2sdndv = rgas/V
    end if

    if(present(d2sdndT)) then
      d2sdndT = -rgas/T
    end if
  end subroutine Sideal_Vn

  !---------------------------------------------------------------------- >
  !> The function returns heat capacity for a mixture of ideal gases
  !!
  !! \param T The temperature [K]
  !! \param Z The overal molar composition [-]
  !! \retval Cp The ideal gas heat capacity [J/mol K]
  !!
  !! \author Oivind W

  function CPideal_mix(nc, comp, T, Z) result (Cpid_mix)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    real, dimension(nc) :: Z
    real :: Cpid_mix
    integer :: i

    Cpid_mix=0.0

    do i=1,nc
      Cpid_mix=Cpid_mix+Z(i)*CPideal(comp(i)%p_comp,i,T)
    end do
  end function CPideal_mix

  !---------------------------------------------------------------------- >
  !> The subroutine returns the enthalpy of ideal gases
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z Composition [-]
  !! \param dhdt_ideal_mix - temperature derivative [J/mol K]
  !! \param dhdp_ideal_mix - pressure derivative [J/mol Pa]
  !! \param dhdz_ideal_mix - composition derivatives [J/mol]
  !! \param H_ideal_mix - enthalpy of ideal gases [J/mol]
  !!
  !! \author Oivind W
  subroutine Hideal_mix(nc, comp, T, Z, H_ideal_mix,dhdt_ideal_mix,&
       dhdp_ideal_mix,dhdz_ideal_mix)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    real, dimension(nc), intent(in) :: Z
    real, intent(out) :: H_ideal_mix
    real, optional, intent(out) :: dhdt_ideal_mix, dhdp_ideal_mix
    real, optional, dimension(nc), intent(out) :: dhdz_ideal_mix
    ! Locals
    integer :: i
    real :: h

    H_ideal_mix=0.0

    do i=1,nc
      h=Hideal(comp(i)%p_comp,i,T)
      H_ideal_mix=H_ideal_mix+h*Z(i)
      if(present(dhdz_ideal_mix)) then  ! composition derivative
        dhdz_ideal_mix(i)=h
      end if
    end do

    if(present(dhdt_ideal_mix)) then
      dhdt_ideal_mix=Cpideal_mix(nc,comp,T,Z)
    end if

    if(present(dhdp_ideal_mix)) then
      dhdp_ideal_mix=0.0
    end if

  end subroutine Hideal_mix

  !---------------------------------------------------------------------- >
  !> The subroutine returns the entropy of ideal gases
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z Composition [-]
  !! \param dsdt_ideal_mix - temperature derivative [J/mol K^2]
  !! \param dsdp_ideal_mix - pressure derivative [J/mol Pa K]
  !! \param dsdz_ideal_mix - composition derivatives [J/mol K]
  !! \param S_ideal_mix - entropy of ideal gases [J/mol/K]
  !!
  !! \author Oivind W

  subroutine TP_Sideal_mix(nc,comp,T, P, Z, S_ideal_mix, &
       dsdt_ideal_mix, dsdp_ideal_mix, dsdz_ideal_mix)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T, P
    real, dimension(nc), intent(in) :: Z
    real, intent(out) :: S_ideal_mix
    real, optional, intent(out) :: dsdt_ideal_mix, dsdp_ideal_mix
    real, optional, dimension(nc), intent(out) :: dsdz_ideal_mix
    ! Locals
    real :: s
    integer :: i

    call Sideal_Pn(nc, z, P, S_ideal_mix, dsdp_ideal_mix, dsdz_ideal_mix)

    do i=1,nc
      s = Sideal_T(comp(i)%p_comp, i, T)
      S_ideal_mix = S_ideal_mix + Z(i)*s
      if(present(dsdz_ideal_mix)) then  ! composition derivative
        dsdz_ideal_mix(i) = dsdz_ideal_mix(i) + s
      end if
    end do

    if(present(dsdt_ideal_mix)) then
      dsdt_ideal_mix = Cpideal_mix(nc,comp,T,Z)/T
    end if

  end subroutine TP_Sideal_mix

  !> The subroutine returns the entropy of ideal gases
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param i Component index [-]
  !! \param dsdt_ideal_mix - temperature derivative [J/mol K^2]
  !! \param dsdp_ideal_mix - pressure derivative [J/mol Pa K]
  !! \param dsdz_ideal_mix - composition derivatives [J/mol K]
  !! \param S_ideal_mix - entropy of ideal gases [J/mol/K]
  !!
  !! \author M. Hammer
  subroutine TP_Sideal_apparent(comp, i, T, P, S_ideal_mix, &
       dsdt_ideal_mix, dsdp_ideal_mix)
    use compdata
    use thermopack_var, only: nce, ncsym, apparent
    implicit none
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    real, intent(in) :: P
    integer, intent(in) :: i
    real, intent(out) :: S_ideal_mix
    real, optional, intent(out) :: dsdt_ideal_mix, dsdp_ideal_mix
    ! Locals
    integer :: j
    real :: z(nce)
    if (i <= ncsym) then
      z = 0.0
      z(i) = 1.0
      call TP_Sideal_mix(nce, comp, T, P, Z, S_ideal_mix, &
           dsdt_ideal_mix, dsdp_ideal_mix)
    else
      ! Calculate apparent component ideal entropy
      S_ideal_mix = 0.0
      if (present(dsdt_ideal_mix)) then
        dsdt_ideal_mix = 0.0
      endif
      do j=ncsym+1,nce
        if (apparent%v_stoich(i,j) > 0.0) then
          S_ideal_mix = S_ideal_mix + apparent%v_stoich(i,j)*Sideal_T(comp(j)%p_comp, j, T)
          if (present(dsdt_ideal_mix)) then
            dsdt_ideal_mix = dsdt_ideal_mix + apparent%v_stoich(i,j)*Cpideal(comp(j)%p_comp,j,T)/T
          endif
        endif
      enddo
      S_ideal_mix = S_ideal_mix - apparent%v_sum(i)*rgas*log(p)
      if (present(dsdp_ideal_mix)) then
        dsdp_ideal_mix = - apparent%v_sum(i)*rgas/P
      endif
    endif
  end subroutine TP_Sideal_apparent

  !---------------------------------------------------------------------- >
  !> The subroutine returns the entropy of ideal gases
  !! with respect to T, v
  !!
  !! \param T Temperature [K]
  !! \param V Specific volume [m3/mol]
  !! \param Z Composition [-]
  !! \param dsdt_ideal_mix Temperature derivative [J/mol K^2]
  !! \param dsdv_ideal_mix Spec. volume derivative [J/m^3 K]
  !! \param dsdz_ideal_mix Composition derivatives [J/mol K]
  !! \param S_ideal_mix Entropy of ideal gases [J/mol K]
  !!
  !! \author GL, 2015-01-23

  subroutine TV_Sideal_mix(nc,comp,T, V, Z, S_ideal_mix, &
       dsdt_ideal_mix, dsdv_ideal_mix, dsdz_ideal_mix)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T, V
    real, dimension(nc), intent(in) :: Z
    real, intent(out) :: S_ideal_mix
    real, optional, intent(out) :: dsdt_ideal_mix, dsdv_ideal_mix
    real, optional, dimension(nc), intent(out) :: dsdz_ideal_mix
    ! Locals
    real :: s
    integer :: i

    call Sideal_Vn(nc, Z, T, V, S_ideal_mix, dsdT=dsdt_ideal_mix, &
         dsdV=dsdv_ideal_mix, dsdn=dsdz_ideal_mix)
    do i = 1,nc
      s = Sideal_T(comp(i)%p_comp, i, T)
      S_ideal_mix = S_ideal_mix + Z(i)*s
      if(present(dsdz_ideal_mix)) then  ! composition derivative
        dsdz_ideal_mix(i) = dsdz_ideal_mix(i) + s
      end if
    end do

    if(present(dsdt_ideal_mix)) then
      dsdt_ideal_mix = dsdt_ideal_mix + Cpideal_mix(nc,comp,T,Z)/T
    end if

  end subroutine TV_Sideal_mix


  !---------------------------------------------------------------------- >
  !> The subroutine returns the Helmholtz free energy of ideal gases
  !! and optionally derivatives
  !!
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/mol]
  !! \param Z Composition [-]
  !! \param dYdt_ideal_mix Temperature derivative [J/mol K]
  !! \param dYdv_ideal_mix Specific volume derivative [J/m3]
  !! \param dYdz_ideal_mix Composition derivatives [J/mol]
  !! \param Y_ideal_mix Free energy of ideal gases [J/mol]
  !!
  !! \author GL, 22-01-2015
  subroutine TV_Yideal_mix(nc, comp, T, V, Z, Y_ideal_mix, &
       dYdt_ideal_mix, dYdv_ideal_mix,dYdz_ideal_mix)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    real, intent(in) :: T, V
    real, dimension(nc), intent(in) :: Z
    real, intent(out) :: Y_ideal_mix
    real, optional, intent(out) :: dYdt_ideal_mix, dYdv_ideal_mix
    real, optional, dimension(nc), intent(out) :: dYdz_ideal_mix
    ! Locals
    real :: H_ideal_mix, S_ideal_mix
    real :: dhdt_ideal_mix, sumz
    real :: dsdt_ideal_mix, dsdv_ideal_mix
    real, dimension(nc) :: dhdz_ideal_mix, dsdz_ideal_mix

    Y_ideal_mix=0.0

    call Hideal_mix(nc, comp, T, Z, H_ideal_mix, &
         dhdt_ideal_mix=dhdt_ideal_mix, dhdz_ideal_mix=dhdz_ideal_mix)
    call TV_Sideal_mix(nc, comp, T, V, Z, S_ideal_mix, &
         dsdt_ideal_mix=dsdt_ideal_mix, dsdv_ideal_mix=dsdv_ideal_mix, &
         dsdz_ideal_mix=dsdz_ideal_mix)

    sumz = sum(z)
    Y_ideal_mix = H_ideal_mix - T*S_ideal_mix - sumz*T*rgas

    if(present(dYdt_ideal_mix)) then
      dYdt_ideal_mix = dhdt_ideal_mix - S_ideal_mix &
           - sumz*rgas - T*dsdt_ideal_mix
    end if

    if(present(dYdv_ideal_mix)) then
      dYdv_ideal_mix = -T*dsdv_ideal_mix
    end if

    if (present(dYdz_ideal_mix)) then
      dYdz_ideal_mix = dhdz_ideal_mix - T*dsdz_ideal_mix - T*rgas
    end if

  end subroutine TV_Yideal_mix

  !---------------------------------------------------------------------- >
  !> The subroutine returns the reduced Helmholtz free energy of ideal gases
  !! and optionally derivatives
  !!
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/mol]
  !! \param n Composition [-]
  !! \param Fid_T Temperature derivative [1/K]
  !! \param Fid_v Specific volume derivative [1/m3]
  !! \param Fid_n Composition derivatives [1/mol]
  !! \param Fid Free energy of ideal gases []
  !!
  !! \author MH, 05-2017
  subroutine Fideal_mix_SI(nc, comp, T, V_SI, n, Fid, Fid_T, Fid_v, Fid_n, &
       Fid_TT, Fid_vv, Fid_nn, Fid_Tv, Fid_vn, Fid_Tn)
    use thermopack_constants
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(nc), intent(in) :: comp
    real, intent(in) :: T, V_SI
    real, dimension(nc), intent(in) :: n
    real, optional, intent(out) :: Fid
    real, optional, intent(out) :: Fid_T, Fid_v, Fid_TT, Fid_vv, Fid_Tv
    real, optional, dimension(nc), intent(out) :: Fid_n, Fid_Tn, Fid_vn
    real, optional, dimension(nc,nc), intent(out) :: Fid_nn
    ! Locals
    integer :: i
    real :: h, s_i, s, V
    real :: h_T, sumn
    real :: s_T, s_v, s_vn, s_Tn, s_vv, s_TT
    real, dimension(nc) :: h_n, s_n, s_nn

    V = V_SI
    call Hideal_mix(nc, comp, T, n, h, &
         dhdt_ideal_mix=h_T, dhdz_ideal_mix=h_n)
    call Sideal_Vn(nc, n, T, V, s, dsdT=s_T, &
         dsdV=s_v, dsdn=s_n, d2sdndT=s_Tn, &
         d2sdndV=s_vn, d2sdn2=s_nn, d2sdV2=s_vv, &
         d2sdT2=s_TT)

    do i = 1,nc
      s_i = Sideal_T(comp(i)%p_comp, i, T)
      s = s + n(i)*s_i
      s_n(i) = s_n(i) + s_i
    end do

    if(present(Fid)) then
      sumn = sum(n)
      Fid = h/(T*rgas) - s/rgas - sumn
    endif
    if(present(Fid_T)) then
      Fid_T = - h/(T**2*rgas) - s_T/rgas
    end if
    if(present(Fid_TT)) then
      Fid_TT = (2.0*h/T - h_T)/(T**2*rgas) - s_TT/rgas
    end if
    if(present(Fid_v)) then
      Fid_v = -s_v/rgas
      Fid_v = Fid_v
    end if
    if(present(Fid_vv)) then
      Fid_vv = -s_vv/rgas
      Fid_vv = Fid_vv
    end if
    if(present(Fid_Tv)) then
      Fid_Tv = 0.0
    end if
    if (present(Fid_n)) then
      Fid_n = h_n/(T*rgas) - s_n/rgas - 1.0
    end if
    if (present(Fid_nn)) then
      Fid_nn = 0.0
      do i = 1, nc
        Fid_nn(i, i) = -s_nn(i)/rgas
      end do
    end if
    if (present(Fid_Tn)) then
      Fid_Tn = - h_n/(T**2*rgas) - s_Tn/rgas
    end if
    if (present(Fid_vn)) then
      Fid_vn = -s_vn/rgas
      Fid_vn = Fid_vn
    end if

  end subroutine Fideal_mix_SI

  !----------------------------------------------------------------------
  !> Calculate ideal entropy.
  !> Unit: J/mol/K
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine idealEntropy_ne(t,p,n,s,dsdt,dsdp,dsdn)
    use thermopack_var, only: get_active_thermo_model, thermo_model, nce
    implicit none
    ! Transferred variables
    real, intent(in) :: t                    !< K - Temperature
    real, intent(in) :: p                    !< Pa - Pressure
    real, intent(in) :: n(nce)               !< Component index
    real, intent(out) :: s                   !< J/mol/K - Ideal entropy
    real, optional, intent(out) :: dsdt      !< J/mol/K^2 - Temperature differential of ideal entropy
    real, optional, intent(out) :: dsdp      !< J/mol/Pa - Pressure differential of ideal entopy
    real, optional, intent(out) :: dsdn(nce) !< J/mol^2/K - Mol number differentials of ideal entopy
    ! Locals
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    !
    act_mod_ptr => get_active_thermo_model()
    call TP_Sideal_mix(nce,act_mod_ptr%comps,T, P, n, S_ideal_mix=s, &
         dsdt_ideal_mix=dsdt, dsdp_ideal_mix=dsdp, dsdz_ideal_mix=dsdn)
  end subroutine idealEntropy_ne

  !----------------------------------------------------------------------
  !> Set ideal entropy reference value
  !> Unit: J/mol/K
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine set_entropy_reference_value(i,s0)
    use thermopack_var, only: get_active_thermo_model, thermo_model
    implicit none
    ! Transferred variables
    integer, intent(in) :: i             !< Component index
    real, intent(in) :: s0               !< J/mol/K - Ideal entropy
    ! Locals
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    !
    act_mod_ptr => get_active_thermo_model()
    act_mod_ptr%comps(i)%p_comp%sref = s0
  end subroutine set_entropy_reference_value

  !----------------------------------------------------------------------
  !> Get ideal entropy reference value
  !> Unit: J/mol/K
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine get_entropy_reference_value(i,s0)
    use thermopack_var, only: get_active_thermo_model, thermo_model
    implicit none
    ! Transferred variables
    integer, intent(in) :: i             !< Component index
    real, intent(out) :: s0              !< J/mol/K - Ideal entropy
    ! Locals
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    !
    act_mod_ptr => get_active_thermo_model()
    s0 = act_mod_ptr%comps(i)%p_comp%sref
  end subroutine get_entropy_reference_value

  !----------------------------------------------------------------------
  !> Set ideal enthalpy reference value
  !> Unit: J/mol
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine set_enthalpy_reference_value(i,h0)
    use thermopack_var, only: get_active_thermo_model, thermo_model
    implicit none
    ! Transferred variables
    integer, intent(in) :: i             !< Component index
    real, intent(in) :: h0               !< J/mol - Ideal enthalpy
    ! Locals
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    !
    act_mod_ptr => get_active_thermo_model()
    act_mod_ptr%comps(i)%p_comp%href = h0
  end subroutine set_enthalpy_reference_value

  !----------------------------------------------------------------------
  !> Set ideal enthalpy reference value
  !> Unit: J/mol
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine get_enthalpy_reference_value(i,h0)
    use thermopack_var, only: get_active_thermo_model, thermo_model
    implicit none
    ! Transferred variables
    integer, intent(in) :: i             !< Component index
    real, intent(out) :: h0              !< J/mol - Ideal enthalpy
    ! Locals
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    !
    act_mod_ptr => get_active_thermo_model()
    h0 = act_mod_ptr%comps(i)%p_comp%href
  end subroutine get_enthalpy_reference_value

  !---------------------------------------------------------------------- >
  !>  Set the ideal gas reference entropy and enthalpy
  !!
  !! \param comps Component array
  !!
  !! \author Morten Hammer
  subroutine set_reference_energies(comps)
    use compdata, only: gendata_pointer
    implicit none
    type(gendata_pointer), intent(inout) :: comps(:)
    !
    real :: T0
    integer :: i
    real :: s_id, h_id
    T0 = 298.15
    do i=1,size(comps)
      ! Test if parameters are given
      if (comps(i)%p_comp%sref /= 0 .or. comps(i)%p_comp%href /= 0) then
        s_id = Sideal_T(comps(i)%p_comp, i, T0) - comps(i)%p_comp%sref - Rgas*log(1e5)
        comps(i)%p_comp%sref = comps(i)%p_comp%sref - s_id
        h_id = Hideal(comps(i)%p_comp, i, T0) - comps(i)%p_comp%href
        comps(i)%p_comp%href = comps(i)%p_comp%href - h_id
      endif
    enddo
  end subroutine set_reference_energies

end module ideal
