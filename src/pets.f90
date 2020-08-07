!------------------------------------------------------------------------------
! The PeTS equation of state for the LJ fluid truncated and shifted at 2.5
! sigma. Reference: Heier et al. 2018 (10.1080/00268976.2018.1447153)
! Implemented by A. Aasen in May 2019.
! ------------------------------------------------------------------------------
module pets
  use thermopack_var, only: nce
  use thermopack_constants, only: N_AVOGADRO
  use numconstants, only: PI
  implicit none
  save

  real :: SIGMA_PETS    !< [m]
  real :: EPSDIVK_PETS  !< [K]

  real, parameter, dimension(0:6) :: a_vec = (/0.690603404, 1.189317012, &
       1.265604153, -24.34554201, 93.67300357, -157.8773415, 96.93736697 /)

  real, parameter, dimension(0:6) :: b_vec = (/0.664852128, 2.10733079, &
       -9.597951213, -17.37871193, 30.17506222, 209.3942909, -353.2743581/)

contains

  !> Gives the contribution to the reduced, residual Helmholtz function F [mol]
  !> coming from PETS' hard-sphere and dispersion contributions. All
  !> variables are in base SI units. F is defined by
  !> F(T,V,n) = sumn*alpha_PC(rho,T,n) = sumn*alpha_PC(sumn/V,T,n)
  subroutine F_PETS_TVn(T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
    real, intent(in) :: T,V,n(nce) ! temp. [K], vol. [m^3], mole numbers [mol]
    ! Output.
    real, intent(out), optional :: F !< [mol]
    real, intent(out), optional :: F_T,F_V,F_n(nce)
    real, intent(out), optional :: F_TT,F_TV,F_Tn(nce),F_VV,F_Vn(nce),F_nn(nce,nce)
    ! Locals.
    real :: rho
    real :: alp
    real :: alp_rho, alp_T, alp_n(nce)
    real :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real :: alp_Tn(nce), alp_nn(nce,nce)
    real :: sumn
    logical :: fir_der_present, sec_der_present

    sumn = sum(n)
    rho = sumn/V

    fir_der_present = present(F_T) .or. present(F_V) .or. present(F_n)
    sec_der_present = present(F_TT) .or. present(F_TV) .or. present(F_Tn) .or. &
         present(F_VV) .or. present(F_Vn) .or. present(F_nn)

    if (sec_der_present) then
       call alpha_PETS(rho,T,n,alp=alp,alp_rho=alp_rho,alp_T=alp_T,alp_n=alp_n,&
            alp_rhorho=alp_rhorho,alp_rhoT=alp_rhoT,alp_rhon=alp_rhon,&
            alp_TT=alp_TT,alp_Tn=alp_Tn,alp_nn=alp_nn)
    else if (fir_der_present) then
       call alpha_PETS(rho,T,n,alp=alp,alp_rho=alp_rho,alp_T=alp_T,alp_n=alp_n)
    else
       call alpha_PETS(rho,T,n,alp=alp)
    end if

    if (present(F)) F = sumn*alp
    if (present(F_T)) F_T = sumn*alp_T
    if (present(F_V)) F_V = -(sumn/V)**2*alp_rho
    if (present(F_n)) F_n = alp + sumn*alp_rho/V + sumn*alp_n
    if (present(F_TT)) F_TT = sumn*alp_TT
    if (present(F_TV)) F_TV = -(sumn/V)**2*alp_rhoT
    if (present(F_Tn)) F_Tn = alp_T + sumn*alp_rhoT/V + sumn*alp_Tn
    if (present(F_VV)) F_VV = 2*sumn**2/V**3*alp_rho + sumn**3/V**4*alp_rhorho
    if (present(F_Vn)) F_Vn = -2*sumn*alp_rho/V**2-sumn**2*alp_rhorho/V**3-sumn**2*alp_rhon/V**2
    if (present(F_nn)) then
       F_nn(1,1) = sumn*alp_nn(1,1) + sumn*alp_rhorho/V**2 + 2*alp_rho/V
    end if

  end subroutine F_PETS_TVn

  !> alpha_PETS = alp^{hard_sphere} + alpha^{dispersion}
  subroutine alpha_PETS(rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    real, intent(in) :: rho, T, n(nce)  !< [mol/m^3], [K], [mol]
    real, intent(out), optional :: alp !< [-]
    real, intent(out), optional :: alp_rho, alp_T, alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)
    ! Locals.
    real :: alp_hs, alp_hs_rho, alp_hs_T, alp_hs_n(nce)
    real :: alp_hs_rhorho, alp_hs_rhoT, alp_hs_rhon(nce)
    real :: alp_hs_TT, alp_hs_Tn(nce), alp_hs_nn(nce,nce)

    real :: alp_d, alp_d_rho, alp_d_T, alp_d_n(nce)
    real :: alp_d_rhorho, alp_d_rhoT, alp_d_rhon(nce)
    real :: alp_d_TT, alp_d_Tn(nce), alp_d_nn(nce,nce)

    logical :: fir_der_present, sec_der_present

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call alpha_pets_hs(rho,T,n,alp_hs,alp_hs_rho,alp_hs_T,alp_hs_n,&
            alp_hs_rhorho,alp_hs_rhoT,alp_hs_rhon,&
            alp_hs_TT,alp_hs_Tn,alp_hs_nn)
       call alpha_disp(rho,T,n,alp_d,alp_d_rho,alp_d_T,alp_d_n,&
            alp_d_rhorho,alp_d_rhoT,alp_d_rhon,&
            alp_d_TT,alp_d_Tn,alp_d_nn)
    else if (fir_der_present) then
       call alpha_pets_hs(rho,T,n,alp_hs,alp_hs_rho,alp_hs_T,alp_hs_n)
       call alpha_disp(rho,T,n,alp_d,alp_d_rho,alp_d_T,alp_d_n)
    else
       call alpha_pets_hs(rho,T,n,alp_hs)
       call alpha_disp(rho,T,n,alp_d)
    end if

    if (present(alp)) then
       alp = alp_hs + alp_d
    end if

    if (present(alp_rho)) then
       alp_rho = alp_hs_rho + alp_d_rho
    end if

    if (present(alp_T)) then
       alp_T = alp_hs_T + alp_d_T
    end if

    if (present(alp_n)) then
       alp_n = 0.0
    end if

    if (present(alp_rhorho)) then
       alp_rhorho = alp_hs_rhorho + alp_d_rhorho
    end if

    if (present(alp_rhoT)) then
       alp_rhoT = alp_hs_rhoT + alp_d_rhoT
    end if

    if (present(alp_rhon)) then
       alp_rhon = 0.0
    end if

    if (present(alp_TT)) then
       alp_TT = alp_hs_TT + alp_d_TT
    end if

    if (present(alp_Tn)) then
       alp_Tn = 0.0
    end if

    if (present(alp_nn)) then
       alp_nn = 0.0
    end if

  end subroutine alpha_PETS


  !> The reduced, molar Helmholtz energy contribution from dispersion.
  subroutine alpha_disp(rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    real, intent(in) :: rho, T, n(nce)  !< [mol/m^3], [K], [mol]

    real, intent(out), optional :: alp ! [-]
    real, intent(out), optional :: alp_rho,alp_T,alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)

    real :: I1, I1_rho, I1_T, I1_n(nce)
    real :: I1_rhorho, I1_rhoT, I1_rhon(nce),I1_TT,I1_Tn(nce),I1_nn(nce,nce)
    real :: I2, I2_rho, I2_T, I2_n(nce)
    real :: I2_rhorho, I2_rhoT, I2_rhon(nce),I2_TT,I2_Tn(nce),I2_nn(nce,nce)
    real :: C1,C1_rho,C1_T,C1_n(nce)
    real :: C1_rhorho,C1_rhoT,C1_rhon(nce)
    real :: C1_TT,C1_Tn(nce),C1_nn(nce,nce)
    real :: m2e1s3, m2e2s3, m2e1s3_T, m2e2s3_T, m2e1s3_TT, m2e2s3_TT
    logical :: fir_der_present, sec_der_present

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call I_1(rho,T,n,I1,I1_rho,I1_T,I1_n,I1_rhorho,I1_rhoT,I1_rhon,I1_TT,I1_Tn,I1_nn)
       call I_2(rho,T,n,I2,I2_rho,I2_T,I2_n,I2_rhorho,I2_rhoT,I2_rhon,I2_TT,I2_Tn,I2_nn)
       call C_1(rho,T,n,C1,C1_rho,C1_T,C1_n,&
            C1_rhorho,C1_rhoT,C1_rhon,C1_TT,C1_Tn,C1_nn)
    else if (fir_der_present) then
       call I_1(rho,T,n,I1,I1_rho,I1_T,I1_n)
       call I_2(rho,T,n,I2,I2_rho,I2_T,I2_n)
       call C_1(rho,T,n,C1,C1_rho,C1_T,C1_n)
    else
       call I_1(rho,T,n,I1)
       call I_2(rho,T,n,I2)
       call C_1(rho,T,n,C1)
    end if

    m2e1s3 = epsdivk_pets/T * sigma_pets**3
    m2e1s3_T = -m2e1s3/T
    m2e1s3_TT = -2*m2e1s3_T/T

    m2e2s3 = m2e1s3*epsdivk_pets/T
    m2e2s3_T = -2*m2e2s3/T
    m2e2s3_TT = -3*m2e2s3_T/T

    if (present(alp)) then
       alp = -PI*rho*(2*I1*m2e1s3 + C1*I2*m2e2s3)
       alp = alp*N_AVOGADRO
    end if

    if (present(alp_rho)) then
       alp_rho = -2*PI*(I1+rho*I1_rho)*m2e1s3
       alp_rho = alp_rho - PI*(C1*I2+rho*C1_rho*I2+rho*C1*I2_rho)*m2e2s3
       alp_rho = alp_rho*N_AVOGADRO
    end if

    if (present(alp_T)) then
       alp_T = -2*PI*rho*(I1_T*m2e1s3 + I1*m2e1s3_T)
       alp_T = alp_T- PI*rho*(C1_T*I2*m2e2s3 + C1*I2_T*m2e2s3 + C1*I2*m2e2s3_T)
       alp_T = alp_T*N_AVOGADRO
    end if

    if (present(alp_n)) then
       alp_n = 0.0
    end if

    if (present(alp_rhorho)) then
       alp_rhorho = -2*PI*(2*I1_rho+rho*I1_rhorho)*m2e1s3
       alp_rhorho = alp_rhorho - PI*(2*C1_rho*I2 + 2*C1*I2_rho + 2*rho*C1_rho*I2_rho &
            + rho*C1_rhorho*I2 +rho*C1*I2_rhorho)*m2e2s3
       alp_rhorho = alp_rhorho*N_AVOGADRO
    end if

    if (present(alp_rhoT)) then
       alp_rhoT = -2*PI*(I1_T+rho*I1_rhoT)*m2e1s3 -2*PI*(I1+rho*I1_rho)*m2e1s3_T
       alp_rhoT = alp_rhoT + 2*PI*(C1*I2 + rho*C1_rho*I2 + rho*C1*I2_rho)*m2e2s3/T - &
            PI*(C1_T*I2 + C1*I2_T + rho*C1_rhoT*I2 + &
            rho*C1_rho*I2_T + rho*C1_T*I2_rho + rho*C1*I2_rhoT)*m2e2s3
       alp_rhoT = alp_rhoT*N_AVOGADRO
    end if

    if (present(alp_rhon)) then
       alp_rhon = 0.0
    end if

    if (present(alp_TT)) then
       alp_TT = -2*PI*rho*(I1_TT*m2e1s3 + 2*I1_T*m2e1s3_T + I1*m2e1s3_TT)
       alp_TT = alp_TT - PI*rho*(&
            C1_TT*I2*m2e2s3 + C1*I2_TT*m2e2s3 + C1*I2*m2e2s3_TT &
            + 2*(C1_T*I2_T*m2e2s3 + C1_T*I2*m2e2s3_T + C1*I2_T*m2e2s3_T))
       alp_TT = alp_TT*N_AVOGADRO
    end if

    if (present(alp_Tn)) then
       alp_Tn = 0.0
    end if

    if (present(alp_nn)) then
       alp_nn(1,1) = 0.0
    end if

  end subroutine alpha_disp

  ! Reduced molar Helmholtz free energy contribution from a hard-sphere fluid.
  subroutine alpha_pets_hs(rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: alp          !< [-]
    real, intent(out), optional :: alp_rho,alp_T,alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)
    ! Locals.
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: alp_eta,alp_etaeta
    logical :: fir_der_present, sec_der_present

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call eta_pets(rho,T,n,e,e_rho,e_T,e_n,e_rhorho,e_rhoT,e_rhon,e_TT,e_Tn,e_nn)
    else if (fir_der_present) then
       call eta_pets(rho,T,n,e,e_rho,e_T,e_n)
    else
       call eta_pets(rho,T,n,e)
    end if

    alp = (4*e-3*e**2)/(1-e)**2

    alp_eta = 2*(e-2)/(e-1)**3
    if (present(alp_rho)) alp_rho = alp_eta*e_rho
    if (present(alp_T)) alp_T = alp_eta*e_T
    if (present(alp_n)) alp_n = alp_eta*e_n

    alp_etaeta = 2*(5-2*e)/(e-1)**4
    if (present(alp_rhorho)) alp_rhorho = alp_eta*e_rhorho + alp_etaeta*e_rho**2
    if (present(alp_rhoT)) alp_rhoT = alp_eta*e_rhoT + alp_etaeta*e_rho*e_T
    if (present(alp_rhon)) alp_rhon = 0.0
    if (present(alp_TT)) alp_TT = alp_eta*e_TT + alp_etaeta*e_T**2
    if (present(alp_Tn)) alp_Tn = 0.0
    if (present(alp_nn)) alp_nn(1,1) = 0.0
  end subroutine alpha_pets_hs



  ! A power series approximation of a perturbation theory integral.
  subroutine I_1(rho,T,n,i1,i1_rho,i1_t,i1_n,&
       i1_rhorho,i1_rhoT,i1_rhon,i1_TT,i1_Tn,i1_nn)
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: i1           !< [-]
    real, intent(out), optional :: i1_rho,i1_T,i1_n(nce)
    real, intent(out), optional :: i1_rhorho,i1_rhoT,i1_rhon(nce)
    real, intent(out), optional :: i1_TT,i1_Tn(nce),i1_nn(nce,nce)
    ! Locals.
    integer :: i
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: i1_eta, i1_etaeta

    call eta_pets(rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n,&
         e_rhorho=e_rhorho,e_rhoT=e_rhoT,e_rhon=e_rhon,e_TT=e_TT,e_Tn=e_Tn,e_nn=e_nn)

    i1 = a_vec(0)
    do i=1,6
       i1 = i1 + a_vec(i)*e**i
    end do

    i1_eta = 0.0
    i1_etaeta = 0.0
    do i=1,6
       i1_eta = i1_eta+a_vec(i)*i*e**(i-1)
       i1_etaeta = i1_etaeta + i*(i-1)*a_vec(i)*e**(i-2)
    end do

    if (present(i1_rho)) then
       i1_rho = i1_eta*e_rho
    end if

    if (present(i1_T)) then
       i1_T = i1_eta*e_T
    end if

    if (present(i1_n)) then
       i1_n = 0.0
    end if

    if (present(i1_rhorho)) then
       i1_rhorho = i1_etaeta*e_rho**2 + i1_eta*e_rhorho
    end if

    if (present(i1_rhoT)) then
       i1_rhoT = i1_etaeta*e_rho*e_T + i1_eta*e_rhoT
    end if

    if (present(i1_rhon)) then
       i1_rhon = 0.0
    end if

    if (present(i1_TT)) then
       i1_TT = i1_etaeta*e_T**2 + i1_eta*e_TT
    end if

    if (present(i1_Tn)) then
       i1_Tn = 0.0
    end if

    if (present(i1_nn)) then
       i1_nn(1,1) = 0.0
    end if

  end subroutine I_1

  !> A power series approximation of a perturbation theory integral.
  subroutine I_2(rho,T,n,i2,i2_rho,i2_t,i2_n,&
       i2_rhorho,i2_rhoT,i2_rhon,i2_TT,i2_Tn,i2_nn)
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: i2           !< [-]
    real, intent(out), optional :: i2_rho,i2_T,i2_n(nce)
    real, intent(out), optional :: i2_rhorho,i2_rhoT,i2_rhon(nce)
    real, intent(out), optional :: i2_TT,i2_Tn(nce),i2_nn(nce,nce)
    ! Locals.
    integer :: i
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: i2_eta, i2_etaeta

    call eta_pets(rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n,&
         e_rhorho=e_rhorho,e_rhoT=e_rhoT,e_rhon=e_rhon,e_TT=e_TT,e_Tn=e_Tn,e_nn=e_nn)

    i2 = b_vec(0)
    do i=1,6
       i2 = i2 + b_vec(i)*e**i
    end do

    i2_eta = 0.0
    i2_etaeta = 0.0
    do i=1,6
       i2_eta = i2_eta+b_vec(i)*i*e**(i-1)
       i2_etaeta = i2_etaeta + i*(i-1)*b_vec(i)*e**(i-2)
    end do

    if (present(i2_rho)) then
       i2_rho = i2_eta*e_rho
    end if

    if (present(i2_T)) then
       i2_T = i2_eta*e_T
    end if

    if (present(i2_n)) then
       i2_n = 0.0
    end if

    if (present(i2_rhorho)) then
       i2_rhorho = i2_etaeta*e_rho**2 + i2_eta*e_rhorho
    end if

    if (present(i2_rhoT)) then
       i2_rhoT = i2_etaeta*e_rho*e_T + i2_eta*e_rhoT
    end if

    if (present(i2_rhon)) then
       i2_rhon = 0.0
    end if

    if (present(i2_TT)) then
       i2_TT = i2_etaeta*e_T**2 + i2_eta*e_TT
    end if

    if (present(i2_Tn)) then
       i2_Tn = 0.0
    end if

    if (present(i2_nn)) then
       i2_nn(1,1) = 0.0
    end if

  end subroutine I_2


  ! The compressibility term, defined as
  ! (1 + Z^{hs} + rho*dZ^{hs}/drho)^{-1}
  subroutine C_1(rho,T,n,c1,c1_rho,c1_t,c1_n,&
       c1_rhorho,c1_rhoT,c1_rhon,c1_TT,c1_Tn,c1_nn)
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: c1           !< [-]
    real, intent(out), optional :: c1_rho,c1_t,c1_n(nce)
    real, intent(out), optional :: c1_rhorho,c1_rhot,c1_rhon(nce)
    real, intent(out), optional :: c1_tt,c1_tn(nce),c1_nn(nce,nce)
    ! Locals.
    real :: e, e2, e3, e4
    real :: e_rho, e_T, e_n(nce)
    real :: e_rhorho, e_rhoT, e_rhon(nce), e_TT, e_Tn(nce), e_nn(nce,nce)
    real :: c1_eta, c1_etaeta
    real :: O1
    real :: O1_eta
    real :: O1_etaeta
    logical :: fir_der_present, sec_der_present

    fir_der_present = present(c1_rho) .or. present(c1_T) .or. present(c1_n)
    sec_der_present = present(c1_rhorho) .or. present(c1_rhoT) .or. &
         present(c1_rhon) .or. present(c1_TT) .or. &
         present(c1_Tn) .or. present(c1_nn)

    if (sec_der_present) then
       call eta_pets(rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n,&
            e_rhorho=e_rhorho,e_rhoT=e_rhoT,e_rhon=e_rhon,e_TT=e_TT,e_Tn=e_Tn,e_nn=e_nn)
    else if ( fir_der_present ) then
       call eta_pets(rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n)
    else
       call eta_pets(rho,T,n,e=e)
    end if

    e2 = e*e
    e3 = e2*e
    e4 = e2*e2
    O1 = (8*e-2*e2)/(1-e)**4

    c1 = 1.0/(1 + O1)

    if (fir_der_present .or. sec_der_present) then
       O1_eta = (-4*e2+20*e+8)/(1-e)**5
       c1_eta = -(O1_eta )*c1**2
       if (sec_der_present) then
          O1_etaeta = ((-8*e+20)*(1-e) + (-4*e2+20*e+8)*5)/(1-e)**6
          c1_etaeta = O1_etaeta
          c1_etaeta = 2*c1_eta**2/c1 - c1_etaeta*c1**2
       end if
    end if

    if (present(c1_rho)) c1_rho = c1_eta*e_rho

    if (present(c1_T)) c1_T = c1_eta*e_T

    if (present(c1_n)) then
       c1_n = c1_eta*e_n
    end if

    if (present(c1_rhorho)) then
       c1_rhorho = c1_etaeta*e_rho**2 + c1_eta*e_rhorho
    end if

    if (present(c1_rhoT)) then
       c1_rhoT = c1_etaeta*e_rho*e_T + c1_eta*e_rhoT
    end if

    if (present(c1_TT)) then
       c1_TT = c1_etaeta*e_T**2 + c1_eta*e_TT
    end if

    if (present(c1_rhon)) then
       c1_rhon = c1_etaeta*e_rho*e_n + c1_eta*e_rhon
    end if

    if (present(c1_Tn)) then
       c1_Tn = c1_etaeta*e_T*e_n + c1_eta*e_Tn
    end if

    if (present(c1_nn)) then
       c1_nn(1,1) = c1_etaeta*e_n(1)*e_n(1) + c1_eta*e_nn(1,1)
    end if

  end subroutine C_1

  ! The packing fraction eta and its derivatives.
  subroutine eta_pets(rho,T,n,e,e_rho,e_T,e_n,e_rhorho,e_rhoT,e_rhon,e_TT,e_Tn,e_nn)
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: e            !< [-]
    real, intent(out), optional :: e_rho, e_T, e_n(nce)
    real, intent(out), optional :: e_rhorho, e_rhoT, e_rhon(nce)
    real, intent(out), optional :: e_TT, e_Tn(nce), e_nn(nce,nce)
    ! Locals.
    real :: d(nce),d_T(nce),d_TT(nce)
    real :: e_div_rho

    call calc_d_pets(T,d=d,d_T=d_T,d_TT=d_TT)

    e_div_rho = (PI/6)*d(1)**3*N_AVOGADRO
    e = e_div_rho*rho

    if (present(e_rho)) then
       e_rho = e_div_rho
    end if

    if (present(e_T)) then
       e_T = e*3*d_T(1)/d(1)
    end if

    if (present(e_n)) then
       e_n(1) = 0
    end if

    if (present(e_rhorho)) then
       e_rhorho = 0.0
    end if

    if (present(e_rhoT)) then
       e_rhoT = e*3*d_T(1)/(rho*d(1))
    end if

    if (present(e_rhon)) then
       e_rhon(1) = 0.0
    end if

    if (present(e_TT)) then
       e_TT = e/d(1)**2 * (6*d_T(1)**2 + 3*d(1)*d_TT(1))
    end if

    if (present(e_Tn)) then
       e_Tn(1) = 0.0
    end if

    if (present(e_nn)) then
       e_nn(1,1) = 0.0
    end if
  end subroutine eta_pets


  ! The Barker--Henderson diameter and its derivatives.
  subroutine calc_d_pets(T,d,d_T,d_TT)
    real, intent(in) :: T            !< [mol/m^3], [K], [mol]
    real, intent(out) :: d(nce)       !< [m]
    real, intent(out), optional :: d_T(nce)
    real, intent(out), optional :: d_TT(nce)
    ! Locals.
    real :: expo
    real :: Tinv, Tinv2, Tinv3
    real, parameter :: c1 = 0.127112544
    real, parameter :: c2 = 3.052785558
    real :: c2scaled
    c2scaled = c2*epsdivk_pets
    Tinv = 1/T
    expo = -c1*exp(-c2scaled*Tinv)
    d(1) = sigma_pets*(1+expo)

    if (present(d_T)) then
       Tinv2 = Tinv*Tinv
       d_T(1) = sigma_pets*(expo*c2scaled*Tinv2)
    end if

    if (present(d_TT)) then
       Tinv3 = Tinv2*Tinv
       d_TT(1) = sigma_pets*expo*((c2scaled*Tinv2)**2-c2scaled*2*Tinv3)
    end if

  end subroutine calc_d_pets

end module pets
