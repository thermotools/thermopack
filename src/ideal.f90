module ideal

  !---------------------------------------------------------------------
  ! Module which calculates the ideal gas properties
  ! implmented in thermopack.
  ! Programmed by OW
  ! © SINTEF Energy Research, 2012--2012. All rights reserved.
  !---------------------------------------------------------------------

  implicit none
  save
  ! Include TREND interface
  include 'trend_interface.f95'

  !> Add a constant to ideal entropy, to get values comparable with TP-lib
  logical, parameter :: correct_for_tplib_bug = .true.

  !> Log cut-off value
  real, parameter :: logCutOff = 1.0e-100

  public:: EstPsat, TP_Kideal, CPideal, Hideal, Sideal_T,&
       CPideal_mix, Hideal_mix, TP_Sideal_mix, TV_Yideal_mix, &
       Sideal_Vn, TV_Sideal_mix, Fideal_mix_SI
  public :: Hideal_apparent, TP_Sideal_apparent, Cpideal_apparent

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
    use tpconst
    use compdata
    use tpidealh2, only: lnpvapred_H2

    implicit none
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: Psat, Term

    method_psat: select case (comp(i)%psatcode)
    case (1) ! Antoine vapour-pressure calculation
      write(*,*) ' Tmax:  ', comp(i)%tantmax, '  Tmin ', comp(i)%tantmin

      if(T>comp(i)%tantmin .AND. T<comp(i)%tantmax) then
        Psat=exp(comp(i)%ant(1)-comp(i)%ant(2)/(T+comp(i)%ant(3)))
        Psat=Psat*133.3224 ! Input in K, output in Pa. Ant-coeffcients give psat in mmHg
        ! psat = [mmHG] * [rhoHg * gGrav / 1000.0]
      else
        Psat=comp(i)%pc*exp(5.42*(1.0-comp(i)%tc/T))
        if(Psat<1.0E-6) then
          Psat=1.0E-6
        endif
      endif
    case (2)  ! Michelsen approach
      Psat=comp(i)%pc*exp(5.42*(1.0-comp(i)%tc/T))
      if(Psat<1.0D-6) then
        Psat=1.0D-6
      endif
    case (3)   ! Starling approach
      if( T .LE. comp(i)%tc ) then
        Psat=(4.92*comp(i)%acf+5.81)*LOG(T/comp(i)%tc) - &
             0.0838*(4.92*comp(i)%acf+2.06)*(36.0/(T/comp(i)%tc)-&
             35.0-(T/comp(i)%tc)**6+42.0*LOG(T/comp(i)%Tc))
      else
        Term=-8.68*((T/comp(i)%tc)-1.8+6.2*comp(i)%zc)**2
        if (Term .GT. -30.0) then
          Term = 10.0**Term
        else
          Term = 0.0
        end if

        Psat= -(16.26-73.85*comp(i)%zc+90.0*comp(i)%zc**2)*&
             (1.0-(T/comp(i)%tc))/(T/comp(i)%tc) - Term
      end if

      Psat = comp(i)%pc*EXP(Psat)

    case (10) ! Leachman ancillary equation for pvap for P-H2, N-H2 and O-H2
      Psat = lnpvapred_H2(comp(i)%ident, T/comp(i)%tc) ! Returns ln(psat/pcrit)
      psat = exp(psat)*comp(i)%pc

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
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(:), intent(in) :: comp
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
  !! \retval Cp The ideal gas heat capacity [J/kmol K]
  !!
  !! \author M. Hammer
  function CPideal_apparent(comp, i, T) result (Cp_id)
    use compdata
    use tpvar, only: nce, ncsym, v_stoich
    implicit none
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: Cp_id
    ! Locals
    integer :: j
    if (i <= ncsym) then
      Cp_id = CPideal(comp, i, T)
    else
      Cp_id = 0.0
      do j=ncsym+1,nce
        if (v_stoich(i,j) > 0.0) then
          Cp_id = Cp_id + CPideal(comp, j, T)*v_stoich(i,j)
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
  !! \retval Cp The ideal gas heat capacity [J/kmol K]
  function CPideal(comp, i, T) result (Cp_id)
    use tpconst
    use compdata
    use parameters, only: verbose
    use tpidealh2, only: cpideal_h2
    implicit none
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: Cp_id, TminCp, TmaxCp

    method_Cp: select case (comp(i)%cptype)
    case (1) ! Third degree poynomial

      Cp_id=comp(i)%cp(1)+comp(i)%cp(2)*T+comp(i)%cp(3)*T**2+comp(i)%cp(4)*T**3
      Cp_id=Cp_id*4.1868*1000.0

    case (2) ! API-project 44

      Cp_id = comp(i)%cp(2) + T*(2.0*comp(i)%cp(3)+ T*(3.0*comp(i)%cp(4)+ &
           T*(4.0*comp(i)%cp(5)+ T*5.0*comp(i)%cp(6))))
      Cp_id = Cp_id*1000.0*comp(i)%mw

      TminCp = comp(i)%tcpmin + 273.15
      TmaxCp = comp(i)%tcpmax + 273.15

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperaure range for component ', trim(comp(i)%ident)
        endif
      end if

    case (3) ! Hypotetic components

      Cp_id=comp(i)%cp(1)+comp(i)%cp(2)*T*1.8+comp(i)%cp(3)*(T*1.8)**2
      Cp_id=Cp_id*4.1868E03*comp(i)%mw

    case (4) ! Third degree polynomial (different units)

      Cp_id=comp(i)%cp(1)+comp(i)%cp(2)*T+comp(i)%cp(3)*T**2+comp(i)%cp(4)*T**3
      Cp_id=Cp_id*1000.0

    case (5) ! Third degree polynomial + 1/T**2 term

      Cp_id=comp(i)%cp(1)+comp(i)%cp(2)*T+comp(i)%cp(3)*T**2+comp(i)%cp(4)*T**3+ &
           comp(i)%cp(5)/(T**2)
      Cp_id=Cp_id*1000.0*comp(i)%mw

    case (6) ! Fourth degree polynomial

      Cp_id=comp(i)%cp(1) + comp(i)%cp(2)*T + comp(i)%cp(3)*T**2 + comp(i)%cp(4)*T**3 + &
           comp(i)%cp(5)*T**4
      Cp_id=Cp_id*1000.0*comp(i)%mw

    case (7) ! DIPPR-database

      Cp_id = comp(i)%cp(1)+ &
           comp(i)%cp(2)*(( comp(i)%cp(3)/T)/sinh( comp(i)%cp(3)/T))**2 +&
           comp(i)%cp(4)*(( comp(i)%cp(5)/T)/cosh( comp(i)%cp(5)/T))**2

      TminCp = comp(i)%tcpmin
      TmaxCp = comp(i)%tcpmax

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp(i)%ident)
        endif
      end if

    case (8) ! Fourth degree polynomial (different units).
      Cp_id=comp(i)%cp(1) + comp(i)%cp(2)*T + comp(i)%cp(3)*T**2 + comp(i)%cp(4)*T**3 + &
           comp(i)%cp(5)*T**4
      Cp_id=Cp_id*kRgas

    case (9) ! Linear function and fraction (J/mol/K).
      Cp_id=comp(i)%cp(1) + comp(i)%cp(2)*T + comp(i)%cp(3)/(T + comp(i)%cp(4))
      Cp_id=Cp_id*1.0e3 ! J/mol/K -> J/kmol/k

    case (10) ! Leachman (NIST) and Valenta expression for N-H2 , O-H2, P-H2 and E-H2 (Valenta)
      Cp_id = CPideal_H2 (comp(i)%ident, T)

    case (CP_TREND_SI) ! Use EOSCG-GERG ideal Cp
      call trend_ideal(T,i,Cp=Cp_id)
      Cp_id = 1.0e3*Cp_id ! J/mol/K -> J/kmol/K

    end select method_Cp
  end function CPideal

  !---------------------------------------------------------------------- >
  !> The function returns ideal enthalpy for
  !! a apparent component, i.
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval h The ideal enthalpy [J/kmol]
  !!
  !! \author M. Hammer
  function Hideal_apparent(comp, i, T) result (H_id)
    use compdata
    use tpvar, only: nce, ncsym, v_stoich
    implicit none
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: H_id
    ! Locals
    integer :: j
    if (i <= ncsym) then
      H_id = Hideal(comp, i, T)
    else
      H_id = 0.0
      do j=ncsym+1,nce
        if (v_stoich(i,j) > 0.0) then
          H_id = H_id + Hideal(comp, j, T)*v_stoich(i,j)
        endif
      enddo
    endif
  end function Hideal_apparent

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas enthalpy for component i
  !!
  !! \param I The number of the component
  !! \param T The temperature [K]
  !! \retval H_id The ideal gas enthalpy [J/kmol]
  !!
  !! \author Oivind W
  function Hideal(comp, i, T) result (H_id)
    use tpconst
    use compdata
    use parameters, only: verbose
    use tpidealh2, only: hideal_h2
    implicit none
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    real :: H_id, TminCp, TmaxCp

    method_H: select case (comp(i)%cptype)
    case (1) ! Third degree Cp-poynomial

      H_id=comp(i)%cp(1)*T+0.5*comp(i)%cp(2)*T**2+comp(i)%cp(3)*T**3/3.0+&
           0.25*comp(i)%cp(4)*T**4
      H_id=H_id*4.1868*1000.0+comp(i)%href

    case (2) ! API-project 44

      H_id = comp(i)%cp(1)+T*(comp(i)%cp(2)+T*(comp(i)%cp(3)+&
           T*(comp(i)%cp(4)+T*(comp(i)%cp(5)+T*comp(i)%cp(6)))))
      H_id = H_id*1000.0*comp(i)%mw+comp(i)%href

      TminCp = comp(i)%tcpmin + 273.15
      TmaxCp = comp(i)%tcpmax + 273.15

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperaure range for component ', trim(comp(i)%ident)
        endif
      end if

    case (3) ! Hypotetic components

      H_id=comp(i)%cp(1)*T*1.8+comp(i)%cp(2)*(T*1.8)**2/2.0+&
           comp(i)%cp(3)*(T*1.8)**3/3.0
      H_id=H_id*(4.1868E03/1.8)*comp(i)%mw+comp(i)%href

    case (4) ! Third degree Cp-polynomial (different units)

      H_id=comp(i)%cp(1)*T+comp(i)%cp(2)*T**2/2.0+&
           comp(i)%cp(3)*T**3/3.0+comp(i)%cp(4)*T**4/4.0
      H_id=H_id*1000.0+comp(i)%href

    case (5) ! Third degree polynomial + 1/T**2 term

      H_id=comp(i)%cp(1)*T+comp(i)%cp(2)*T**2.0/2.0 + &
           comp(i)%cp(3)*T**3/3.0+comp(i)%cp(4)*T**4/4.0 - &
           comp(i)%cp(5)/T
      H_id=H_id*1000.0*comp(i)%mw+comp(i)%href

    case (6) ! Fourth degree Cp-polynomial

      H_id=comp(i)%cp(1)*T+comp(i)%cp(2)*T**2.0/2.0 + &
           comp(i)%cp(3)*T**3/3.0+comp(i)%cp(4)*T**4/4.0 + &
           comp(i)%cp(5)*T**5/5.0
      H_id=H_id*1000.0*comp(i)%mw+comp(i)%href

    case (7) ! DIPPR-database

      H_id = comp(i)%cp(1)*T+&
           (cosh(comp(i)%cp(3)/T)*comp(i)%cp(3)*comp(i)%cp(2))/(sinh(comp(i)%cp(3)/T))-&
           (sinh(comp(i)%cp(5)/T)*comp(i)%cp(5)*comp(i)%cp(4))/(cosh(comp(i)%cp(5)/T))
      H_id=H_id+comp(i)%href


      TminCp = comp(i)%tcpmin
      TmaxCp = comp(i)%tcpmax

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp(i)%ident)
        endif
      end if

    case (8) ! Fourth degree Cp-polynomial (different units)

      H_id=comp(i)%cp(1)*T+comp(i)%cp(2)*T**2.0/2.0 + &
           comp(i)%cp(3)*T**3/3.0+comp(i)%cp(4)*T**4/4.0 + &
           comp(i)%cp(5)*T**5/5.0
      H_id=H_id*kRgas+comp(i)%href

    case (9) ! Linear function and fraction (J/mol).
      H_id=comp(i)%cp(1)*T+comp(i)%cp(2)*T**2.0/2.0 + &
           comp(i)%cp(3)*log(T+comp(i)%cp(4))
      H_id=H_id*1.0e3+comp(i)%href

    case (10) ! Leachman (NIST) and Valenta expression for N-H2 , O-H2, P-H2 and E-H2 (Valenta)
      H_id = Hideal_H2 (comp(i)%ident, T) + comp(i)%href

    case (CP_TREND_SI) ! Use EOSCG-GERG ideal Cp
      call trend_ideal(T,i,h=H_id)
      H_id = 1.0e3*H_id ! J/mol -> J/kmol

    end select method_H
  end function Hideal

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas entropy for component i
  !! NOT including P or V contribution
  !!
  !! \param i The number of the component
  !! \param T Temperature [K]
  !! \retval S_id Ideal gas entropy [J/kmol K]
  !!
  !! \author Oivind W

  function Sideal_T(comp, i, T) result (S_id)
    use tpconst
    use compdata
    use parameters, only: verbose
    use tpidealh2, only: sideal_h2
    implicit none
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    integer, intent(in) :: i
    !
    real :: S_id, TminCp, TmaxCp

    method_S: select case (comp(i)%cptype)
    case (1) ! Third degree Cp-poynomial
      S_id = comp(i)%cp(1)*log(T)+comp(i)%cp(2)*T + &
           comp(i)%cp(3)*T**2.0/2.0+comp(i)%cp(4)*T**3.0/3.0
      S_id = S_id*4.1868*1000.0
      S_id = S_id + comp(i)%sref
    case (2) ! API-project 44
      S_id = comp(i)%cp(2)*log(T)+2.0*comp(i)%cp(3)*T+&
           3.0/2.0*comp(i)%cp(4)*T**2+4.0/3.0*comp(i)%cp(5)*T**3+&
           5.0/4.0*comp(i)%cp(6)*T**4
      S_id = S_id*1000.0*comp(i)%mw
      S_id = S_id + comp(i)%sref
      if (correct_for_tplib_bug) then
        S_id = S_id + kRgas*log(1000.0)
      endif
      TminCp = comp(i)%tcpmin + 273.15
      TmaxCp = comp(i)%tcpmax + 273.15
      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp(i)%ident)
        endif
      end if
    case (3) ! Hypotetic components
      S_id = comp(i)%cp(1)*log(T*1.8)+comp(i)%cp(2)*T*1.8 +&
           comp(i)%cp(3)*(T*1.8)**2/2.0
      S_id = S_id*(4.1868E03)*comp(i)%mw
      S_id = S_id + comp(i)%sref
    case (4) ! Third degree Cp-polynomial (different units)
      S_id = comp(i)%cp(1)*log(T)+comp(i)%cp(2)*T+&
           comp(i)%cp(3)*T**2.0/2.0+comp(i)%cp(4)*T**3/3.0
      S_id = S_id*1000.0
      S_id = S_id + comp(i)%sref
    case (5) ! Third degree polynomial + 1/T**2 term
      S_id = comp(i)%cp(1)*log(T)+comp(i)%cp(2)*T +&
           comp(i)%cp(3)*T**2/2.0+comp(i)%cp(4)*T**3/3.0-&
           comp(i)%cp(5)/(2.0*T**2)
      S_id = S_id*1000.0*comp(i)%mw
      S_id = S_id + comp(i)%sref
    case (6) ! Fourth degree Cp-polynomial
      S_id = comp(i)%cp(1)*log(T)+comp(i)%cp(2)*T+&
           comp(i)%cp(3)*T**2/2.0+comp(i)%cp(4)*T**3/3.0+&
           comp(i)%cp(5)*T**4/4.0
      S_id = S_id*1000.0*comp(i)%mw
      S_id = S_id + comp(i)%sref
    case (7) ! DIPPR-database
      S_id = comp(i)%cp(1)*log(T)+2*comp(i)%cp(3)/T*comp(i)%cp(2)+&
           2*comp(i)%cp(3)/T*comp(i)%cp(2)/(exp(comp(i)%cp(3)/T)**2-1)-&
           comp(i)%cp(2)*log(exp(comp(i)%cp(3)/T)**2-1)-&
           2*comp(i)%cp(5)/T*comp(i)%cp(4)+2*comp(i)%cp(5)/T*comp(i)%cp(4)/&
           (exp(comp(i)%cp(5)/T)**2+1)+comp(i)%cp(4)*log(exp(comp(i)%cp(5)/T)**2+1)
      S_id = S_id + comp(i)%sref

      TminCp = comp(i)%tcpmin
      TmaxCp = comp(i)%tcpmax

      if ( T .LT. TminCp .OR. T .GT. TmaxCp ) then
        if (verbose) then
          write(*,*) 'Ideal gas Cp-polynomial out of temperature range for component ', trim(comp(i)%ident)
        endif
      end if
    case (8) ! Fourth degree Cp-polynomial (different units)
      S_id = comp(i)%cp(1)*log(T)+comp(i)%cp(2)*T+&
           comp(i)%cp(3)*T**2/2.0+comp(i)%cp(4)*T**3/3.0+&
           comp(i)%cp(5)*T**4/4.0
      S_id = S_id*kRgas
      S_id = S_id + comp(i)%sref

    case (9) ! Linear function and fraction (J/mol/K).
      S_id = comp(i)%cp(1)*log(T)+comp(i)%cp(2)*T
      if (comp(i)%cp(4) == 0.0) then
        S_id = S_id-comp(i)%cp(3)/T
      else
        S_id = S_id+(comp(i)%cp(3)/comp(i)%cp(4))*(log(T)-log(T+comp(i)%cp(4)))
      endif
      S_id = S_id*1.0e3 ! J/mol/K -> J/kmol/K
      S_id = S_id + comp(i)%sref

    case (10) !   ! Leachman (NIST) and Valenti expression for N-H2 , O-H2, P-H2 and E-H2 (Valenta)
      S_id = sideal_H2 (comp(i)%ident, T) + comp(i)%sref  ! + kRgas * log(rho*T/ rho0*T0)

    case (CP_TREND_SI) ! Use EOSCG-GERG ideal Cp
      call trend_ideal(T,i,s=S_id)
      S_id = 1.0e3*S_id ! J/mol/K -> J/kmol/K

    end select method_S
  end function Sideal_T

  !---------------------------------------------------------------------- >
  !> The function returns pressure and composition contribution
  !! to ideal gas entropy
  !!
  !! \param n Mole numbers
  !! \param P The pressure [Pa]
  !! \retval s The ideal gas entropy [J/kmol K]
  !! \param dsdt Temperature derivative [J/kmol K^2]
  !! \param dsdp Spec. volume derivative [J/m3 Pa]
  !! \param dsdn Mole number differential [J/kmol^2 K]
  !!
  !! \author MH, 10-2015

  subroutine Sideal_Pn(nc, n, P, s, dsdP, dsdn)
    use tpconst
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
      sn = sn - kRgas*n(i)*logZi
      if(present(dsdn)) then  ! composition derivative
        dsdn(i)=-kRgas*(logZi + logp)
      end if
    end do
    s = -sumn*kRgas*logp + sn

    if (present(dsdP)) then
      dsdP = -sumn*kRgas/P
    endif

  end subroutine Sideal_Pn

  !---------------------------------------------------------------------- >
  !> The function returns ideal gas entropy volume and
  !! compozition contribution
  !!
  !! \param n Mole numbers
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/kmol]
  !! \retval s Ideal gas entropy [J/kmol K]
  !! \param dsdt Temperature derivative [J/kmol K^2]
  !! \param dsdv Spec. volume derivative [J/m3K]
  !! \param dsdn Mole number differential [J/kmol^2 K]
  !! \param d2sdn2 Mole number differential [J/kmol^3 K]
  !! \param d2sdv2 Volume differential [J/kmol/K/m6]
  !!
  !! \author GL, 23-01-2015
  !!         MH, 10-2015

  subroutine Sideal_Vn(nc, n, T, V, s, dsdT, dsdV, dsdn, d2sdndT, d2sdndV, &
       d2sdn2, d2sdV2, d2sdT2)
    use tpconst, only: kRgas
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

    sv = log(V/(kRgas*T))
    sn = 0.0
    do i=1,nc
      if(abs(n(i)) .LT. logCutOff) then
        logni = log(logCutOff)
      else
        logni = log(n(i))
      end if
      sn = sn - n(i)*logni
      if(present(dsdn)) then  ! composition derivative
        dsdn(i) = kRgas*(sv - 1.0 - logni)
      end if
      if(present(d2sdn2)) then  ! second composition derivative
        if (n(i) > 0.0) then
          d2sdn2(i) = - kRgas/n(i)
        else
          d2sdn2(i) = 0.0
        endif
      end if
    end do
    ! Add volume and composition contribution
    s = kRgas*(sumn*sv + sn)
    if(present(dsdt)) then
      dsdt = -sumn*kRgas/T
    end if
    if(present(d2sdt2)) then
      d2sdt2 = sumn*kRgas/T**2
    end if

    if(present(dsdv)) then
      dsdv = sumn*kRgas/V
    end if

    if(present(d2sdv2)) then
      d2sdv2 = -sumn*kRgas/V**2
    end if

    if(present(d2sdndv)) then
      d2sdndv = kRgas/V
    end if

    if(present(d2sdndT)) then
      d2sdndT = -kRgas/T
    end if
  end subroutine Sideal_Vn

  !---------------------------------------------------------------------- >
  !> The function returns heat capacity for a mixture of ideal gases
  !!
  !! \param T The temperature [K]
  !! \param Z The overal molar composition [-]
  !! \retval Cp The ideal gas heat capacity [J/kmol K]
  !!
  !! \author Oivind W

  function CPideal_mix(nc, comp, T, Z) result (Cpid_mix)
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(:), intent(in) :: comp
    real, intent(in) :: T
    real, dimension(nc) :: Z
    real :: Cpid_mix
    integer :: i

    Cpid_mix=0.0

    do i=1,nc
      Cpid_mix=Cpid_mix+Z(i)*CPideal(comp,i,T)
    end do
  end function CPideal_mix

  !---------------------------------------------------------------------- >
  !> The subroutine returns the enthalpy of ideal gases
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z Composition [-]
  !! \param dhdt_ideal_mix - temperature derivative [J/kmole K]
  !! \param dhdp_ideal_mix - pressure derivative [J/kmole Pa]
  !! \param dhdz_ideal_mix - composition derivatives [J/kmole]
  !! \param H_ideal_mix - enthalpy of ideal gases [J/kmole]
  !!
  !! \author Oivind W
  subroutine Hideal_mix(nc, comp, T, Z, H_ideal_mix,dhdt_ideal_mix,&
       dhdp_ideal_mix,dhdz_ideal_mix)
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(:), intent(in) :: comp
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
      h=Hideal(comp,i,T)
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
  !! \param dsdt_ideal_mix - temperature derivative [J/kmole K^2]
  !! \param dsdp_ideal_mix - pressure derivative [J/kmole Pa K]
  !! \param dsdz_ideal_mix - composition derivatives [J/kmole K]
  !! \param S_ideal_mix - entropy of ideal gases [J/kmole/K]
  !!
  !! \author Oivind W

  subroutine TP_Sideal_mix(nc,comp,T, P, Z, S_ideal_mix, &
       dsdt_ideal_mix, dsdp_ideal_mix, dsdz_ideal_mix)
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(:), intent(in) :: comp
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
      s = Sideal_T(comp, i, T)
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
  !! \param dsdt_ideal_mix - temperature derivative [J/kmole K^2]
  !! \param dsdp_ideal_mix - pressure derivative [J/kmole Pa K]
  !! \param dsdz_ideal_mix - composition derivatives [J/kmole K]
  !! \param S_ideal_mix - entropy of ideal gases [J/kmole/K]
  !!
  !! \author M. Hammer
  subroutine TP_Sideal_apparent(comp, i, T, P, S_ideal_mix, &
       dsdt_ideal_mix, dsdp_ideal_mix)
    use compdata
    use tpvar, only: nce, ncsym, v_stoich, v_sum
    use tpconst, only: kRgas
    implicit none
    type(gendata), dimension(:), intent(in) :: comp
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
        if (v_stoich(i,j) > 0.0) then
          S_ideal_mix = S_ideal_mix + v_stoich(i,j)*Sideal_T(comp, j, T)
          if (present(dsdt_ideal_mix)) then
            dsdt_ideal_mix = dsdt_ideal_mix + v_stoich(i,j)*Cpideal(comp,j,T)/T
          endif
        endif
      enddo
      S_ideal_mix = S_ideal_mix - v_sum(i)*kRgas*log(p)
      if (present(dsdp_ideal_mix)) then
        dsdp_ideal_mix = - v_sum(i)*kRgas/P
      endif
    endif
  end subroutine TP_Sideal_apparent

  !---------------------------------------------------------------------- >
  !> The subroutine returns the entropy of ideal gases
  !! with respect to T, v
  !!
  !! \param T Temperature [K]
  !! \param V Specific volume [m3/kmol]
  !! \param Z Composition [-]
  !! \param dsdt_ideal_mix Temperature derivative [J/kmol K^2]
  !! \param dsdv_ideal_mix Spec. volume derivative [J/m3K]
  !! \param dsdz_ideal_mix Composition derivatives [J/kmol K]
  !! \param S_ideal_mix Entropy of ideal gases [J/kmolK]
  !!
  !! \author GL, 2015-01-23

  subroutine TV_Sideal_mix(nc,comp,T, V, Z, S_ideal_mix, &
       dsdt_ideal_mix, dsdv_ideal_mix, dsdz_ideal_mix)
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(:), intent(in) :: comp
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
      s = Sideal_T(comp, i, T)
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
  !! \param v Specific volume [m3/kmol]
  !! \param Z Compsition [-]
  !! \param dYdt_ideal_mix Temperature derivative [J/kmolK]
  !! \param dYdv_ideal_mix Specific volume derivative [J/m3]
  !! \param dYdz_ideal_mix Composition derivatives [J/kmol]
  !! \param Y_ideal_mix Free energy of ideal gases [J/kmol]
  !!
  !! \author GL, 22-01-2015

  subroutine TV_Yideal_mix(nc, comp, T, V, Z, Y_ideal_mix, &
       dYdt_ideal_mix, dYdv_ideal_mix,dYdz_ideal_mix)
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(:), intent(in) :: comp
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
    Y_ideal_mix = H_ideal_mix - T*S_ideal_mix - sumz*T*kRgas

    if(present(dYdt_ideal_mix)) then
      dYdt_ideal_mix = dhdt_ideal_mix - S_ideal_mix &
           - sumz*kRgas - T*dsdt_ideal_mix
    end if

    if(present(dYdv_ideal_mix)) then
      dYdv_ideal_mix = -T*dsdv_ideal_mix
    end if

    if (present(dYdz_ideal_mix)) then
      dYdz_ideal_mix = dhdz_ideal_mix - T*dsdz_ideal_mix - T*kRgas
    end if

  end subroutine TV_Yideal_mix

  !---------------------------------------------------------------------- >
  !> The subroutine returns the reduced Helmholtz free energy of ideal gases
  !! and optionally derivatives
  !!
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/mol]
  !! \param n Compsition [-]
  !! \param Fid_T Temperature derivative [1/K]
  !! \param Fid_v Specific volume derivative [1/m3]
  !! \param Fid_n Composition derivatives [1/mol]
  !! \param Fid Free energy of ideal gases []
  !!
  !! \author MH, 05-2017
  subroutine Fideal_mix_SI(nc, comp, T, V_SI, n, Fid, Fid_T, Fid_v, Fid_n, &
       Fid_TT, Fid_vv, Fid_nn, Fid_Tv, Fid_vn, Fid_Tn)
    use tpconst
    use compdata
    implicit none
    integer, intent(in) :: nc
    type(gendata), dimension(nc), intent(in) :: comp
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

    V = V_SI*1.0e3 ! m3/mol -> m3/kmol
    call Hideal_mix(nc, comp, T, n, h, &
         dhdt_ideal_mix=h_T, dhdz_ideal_mix=h_n)
    call Sideal_Vn(nc, n, T, V, s, dsdT=s_T, &
         dsdV=s_v, dsdn=s_n, d2sdndT=s_Tn, &
         d2sdndV=s_vn, d2sdn2=s_nn, d2sdV2=s_vv, &
         d2sdT2=s_TT)

    do i = 1,nc
      s_i = Sideal_T(comp, i, T)
      s = s + n(i)*s_i
      s_n(i) = s_n(i) + s_i
    end do

    if(present(Fid)) then
      sumn = sum(n)
      Fid = h/(T*kRgas) - s/kRgas - sumn
    endif
    if(present(Fid_T)) then
      Fid_T = - h/(T**2*kRgas) - s_T/kRgas
    end if
    if(present(Fid_TT)) then
      Fid_TT = (2.0*h/T - h_T)/(T**2*kRgas) - s_TT/kRgas
    end if
    if(present(Fid_v)) then
      Fid_v = -s_v/kRgas
      Fid_v = Fid_v*1.0e3
    end if
    if(present(Fid_vv)) then
      Fid_vv = -s_vv/kRgas
      Fid_vv = Fid_vv*1.0e6
    end if
    if(present(Fid_Tv)) then
      Fid_Tv = 0.0
    end if
    if (present(Fid_n)) then
      Fid_n = h_n/(T*kRgas) - s_n/kRgas - 1.0
    end if
    if (present(Fid_nn)) then
      Fid_nn = 0.0
      do i = 1, nc
        Fid_nn(i, i) = -s_nn(i)/kRgas
      end do
    end if
    if (present(Fid_Tn)) then
      Fid_Tn = - h_n/(T**2*kRgas) - s_Tn/kRgas
    end if
    if (present(Fid_vn)) then
      Fid_vn = -s_vn/kRgas
      Fid_vn = Fid_vn*1.0e3
    end if

  end subroutine Fideal_mix_SI

end module ideal
