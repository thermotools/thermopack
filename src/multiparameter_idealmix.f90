!-----------------------------------------------------------------------------
!> This module mixes multiparameter EoS through an
!> ideal mixing rule. It relies heavily on the multiparameter EoS
!> framwork and the methodology implemented by Ailo Aasen.
!> 2018-10-01 Oivind Wilhelmsen
!-----------------------------------------------------------------------------

module multiparameter_idealmix
  use thermopack_constants
  use eos_parameters, only: meos_mix

  public :: calc_multiparameter_idealmix_zfac
  public :: calc_multiparameter_idealmix_entropy
  public :: calc_multiparameter_idealmix_enthalpy
  public :: calc_multiparameter_idealmix_Gres
  public :: calc_multiparameter_idealmix_fugacity

contains
  !-----------------------------------------------------------------------------
  !> Initialize framework for computing thermodynamic properties with an ideal
  !> mixture of multiparameter EoS. NB: Only expected to give reasonable results
  !> in the single-phase regions, two-phase regions should be avoided due to
  !> possible multiple maxwell loops ++.
  !>
  !> \AUTHOR OW, date:2018-10-01
  !-----------------------------------------------------------------------------

  !---------------------------------------------------------------------- >
  !> This function calculates the Compressibility factor with derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param Zfac The Z-factor [-]
  !! \param dZdt Temperature derivative [1/K]
  !! \param dZdp Pressure derivative [1/Pa]
  !! \param dZdz Composition derivative [-]
  !!
  !! Author: OW, date: 2018-10-02
  !> --------------------------------------------------------------------------
  subroutine calc_multiparameter_idealmix_zfac(nc, meos, T, P, Z, phase, &
       Zfac, dZdt, dZdp, dZdz)

    integer, intent(in) :: nc
    class(meos_mix), intent(in) :: meos

    ! input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in), dimension(nc) :: Z
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: Zfac
    real, optional, intent(out) :: dZdt
    real, optional, intent(out) :: dZdp
    real, optional, intent(out) :: dZdz(nc)

    ! local
    integer :: iz
    real :: Zfac_comp, dZdt_comp, dZdp_comp, dZdz_comp(1)

    !> Set to zero
    Zfac=0.0

    !> Check if the optionals are present
    if (present(dZdt)) then
      dZdt=0.0
      dZdt_comp=0.0
    end if
    if (present(dZdp)) then
      dZdp=0.0
      dZdp_comp=0.0
    end if
    if (present(dZdz)) then
      dZdz=0.0
      dZdz_comp=0.0
    end if

    !< Loop through and compute quantities
    do iz=1,nc
       call meos%nist(iz)%meos%calc_zfac(T,P,(/1.0/),phase, Zfac_comp, &
           dZdt_comp, dZdp_comp, dZdz_comp)

      Zfac=Zfac+Zfac_comp*Z(iz)

      if (present(dZdt)) then
        dZdt=dZdt+dZdt_comp*Z(iz)
      end if

      if (present(dZdp)) then
        dZdp=dZdp+dZdp_comp*Z(iz)
      end if

      if (present(dZdz)) then
        dZdz(iz)=Zfac_comp
      end if
    enddo
  end subroutine calc_multiparameter_idealmix_zfac

  !> --------------------------------------------------------------------------
  !> This function calculates the residual Enthalpy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param enthalpy h [J/mol]
  !! \param dhdt Temperature derivative [J/mole K]
  !! \param dhdp Pressure derivative [J/mole Pa]
  !! \param dhdz Composition derivative [J/mole^2]
  !!
  !! Author: OW, date: 2018-10-02
  !> --------------------------------------------------------------------------
  subroutine calc_multiparameter_idealmix_enthalpy(nc, meos, T, P, Z, phase, &
       h, dhdt, dhdp, dhdz)

    integer, intent(in) :: nc
    class(meos_mix), intent(in) :: meos

    ! input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in), dimension(nc) :: Z
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: h
    real, optional, intent(out) :: dhdt
    real, optional, intent(out) :: dhdp
    real, optional, intent(out) :: dhdz(nc)

    ! local
    integer :: iz
    real :: h_comp, dhdt_comp, dhdp_comp, dhdz_comp(1)
    logical :: residual

    !> Set to zero
    h=0.0

    !> We extract only the residual values (Excluding ideal gas terms)
    residual=.true.

    !> Check if the optionals are present
    if (present(dhdt)) then
      dhdt=0.0
      dhdt_comp=0.0
    end if
    if (present(dhdp)) then
      dhdp=0.0
      dhdt_comp=0.0
    end if
    if (present(dhdz)) then
      dhdz=0.0
      dhdz_comp=0.0
    end if

    !< Loop through and compute quantities
    do iz=1,nc
       call meos%nist(iz)%meos%calc_enthalpy(T, P, (/1.0/),phase, h_comp, &
           dhdt_comp, dhdp_comp, dhdz_comp, residual=residual)

      h=h+h_comp*Z(iz)

      if (present(dhdt)) then
        dhdt=dhdt+dhdt_comp*Z(iz)
      end if

      if (present(dhdp)) then
        dhdp=dhdp+dhdp_comp*Z(iz)
      end if

      if (present(dhdz)) then
        dhdz(iz)=h_comp  ! NB: H_MIX=sum_i^N N_iH_i
      end if
    enddo
  end subroutine calc_multiparameter_idealmix_enthalpy

  !---------------------------------------------------------------------- >
  !> This function calculates the residual Entropy and the derivatives
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param dsdt Temperature derivative [J/kmoleK]
  !! \param dsdp Pressure derivative [J/kmolePa]
  !! \param dsdz Composition derivative [J/kmole^2]
  !! \param numder Analytical derivatives if true (default false)
  !!
  !! \retval entropy The entropy [J/kmole K]
  !!
  !! Author: OW, date: 2018-10-09
  !> --------------------------------------------------------------------------
    subroutine calc_multiparameter_idealmix_entropy(nc, meos, T, P, Z, phase, &
       s, dsdt, dsdp, dsdz)

    integer, intent(in) :: nc
    class(meos_mix), intent(in) :: meos

    ! input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in), dimension(nc) :: Z
    integer, intent(in) :: phase
    ! output
    real, intent(out) :: s
    real, optional, intent(out) :: dsdt
    real, optional, intent(out) :: dsdp
    real, optional, intent(out) :: dsdz(nc)

    ! local
    integer :: iz
    real :: s_comp, dsdt_comp, dsdp_comp, dsdz_comp(1)
    logical :: residual

    !> Set to zero
    s=0.0

    !> We extract only the residual values (Excluding ideal gas terms)
    residual=.true.

    !> Check if the optionals are present
    if (present(dsdt)) then
      dsdt=0.0
      dsdt_comp=0.0
    end if
    if (present(dsdp)) then
      dsdp=0.0
      dsdt_comp=0.0
    end if
    if (present(dsdz)) then
      dsdz=0.0
      dsdz_comp=0.0
    end if

    !< Loop through and compute quantities
    do iz=1,nc
       call meos%nist(iz)%meos%calc_entropy(T, P, (/1.0/),phase, s_comp, &
           dsdt_comp, dsdp_comp, dsdz_comp, residual=residual)

      s=s+s_comp*Z(iz)

      if (present(dsdt)) then
        dsdt=dsdt+dsdt_comp*Z(iz)
      end if

      if (present(dsdp)) then
        dsdp=dsdp+dsdp_comp*Z(iz)
      end if

      if (present(dsdz)) then
        dsdz(iz)=s_comp !> ! NB: S_mix=sum_i^N N_iS_i
      end if
    enddo
  end subroutine calc_multiparameter_idealmix_entropy

  !---------------------------------------------------------------------- >
  !> This function calculates the Fugacity coefficient and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param fug, Fugacity coefficient []
  !! \param dlnfdt Temperature derivative [1/K] (dlog(f)/dT)
  !! \param dlnfdp Pressure derivative [1/Pa]   (dlog(f)/dP)
  !! \param dlnfdz Composition derivative [1/kmole] (dlog(f)/dNi)
  !!
  !! Author: OW, date: 2018-10-09
  !> --------------------------------------------------------------------------
  subroutine calc_multiparameter_idealmix_fugacity(nc, meos, T, p, Z, phase, &
       fug,dlnfdt,dlnfdp,dlnfdz)

    integer, intent(in) :: nc
    class(meos_mix), intent(in) :: meos

    ! input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in), dimension(nc) :: Z
    integer, intent(in) :: phase

    ! output
    real, dimension(nc), intent(out) :: fug
    real, optional, intent(out) :: dlnfdt(nc), dlnfdp(nc), dlnfdz(nc,nc)

    ! Intermediate variables
    integer :: iz
    real :: lnphi(1)
    real :: lnphi_t(1)
    real :: lnphi_p(1)
    real :: lnphi_n(1,1)

    !> Check if the optionals are present, if they are, call
    !> subroutines with optionals
    if (present(dlnfdt)) then
      lnphi_t(1)=0.0
    end if
    if (present(dlnfdp)) then
      lnphi_p(1)=0.0
    end if
    if (present(dlnfdz)) then
      lnphi_n(1,1)=0.0
      dlnfdz=0.0      ! Set all instances to zero
    end if

    do iz=1,nc
       call meos%nist(iz)%meos%calc_lnphi(T, P, (/1.0/),phase, &
           lnphi, lnphi_t, lnphi_p, lnphi_n)

      fug(iz)=exp(lnphi(1))

      ! Check if optionals are present
      if (present(dlnfdt)) then
        dlnfdt(iz)=lnphi_t(1)
      end if
      if (present(dlnfdp)) then
        dlnfdp(iz)=lnphi_p(1)
      end if
      if (present(dlnfdz)) then
        dlnfdz(iz,iz)=lnphi_n(1,1)
      end if
    end do
  end subroutine calc_multiparameter_idealmix_fugacity

  !---------------------------------------------------------------------- >
  !> This subroutine calculates the residual gibbs energy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gr Residual gibbs energy [J/kmol]
  !! \param dgrdt Temperature derivative [J/kmol/K]
  !! \param dgrdp Pressure derivative [J/kmol/Pa]
  !!
  !! Author: OW, date: 2018-10-09
  !> --------------------------------------------------------------------------
  subroutine calc_multiparameter_idealmix_Gres(nc, meos, T, P, Z, phase, &
       gr, dgrdt, dgrdp)

    integer, intent(in) :: nc
    class(meos_mix), intent(in) :: meos

    ! input
    real, intent(in) :: T
    real, intent(in) :: P
    real, intent(in), dimension(nc) :: Z
    integer, intent(in) :: phase

    ! output
    real, intent(out) :: gr
    real, optional, intent(out) :: dgrdt, dgrdp

    ! local
    integer :: iz
    real :: gr_comp, dgrdt_comp, dgrdp_comp

    ! Initialize the residual at zero
    gr=0.0

    !> Check if the optionals are present
    if (present(dgrdt)) then
      dgrdt=0.0
      dgrdt_comp=0.0
    end if
    if (present(dgrdp)) then
      dgrdp=0.0
      dgrdt_comp=0.0
    end if

    !< Loop through and compute quantities
    do iz=1,nc
      call meos%nist(iz)%meos%calc_resgibbs(T, P, (/1.0/), phase, gr_comp, &
           dgrdt_comp, dgrdp_comp)

      gr=gr+gr_comp*Z(iz)

      if (present(dgrdt)) then
        dgrdt=dgrdt+dgrdt_comp*Z(iz)
      end if
      if (present(dgrdp)) then
        dgrdp=gr
      end if
    enddo
  end subroutine calc_multiparameter_idealmix_Gres
end module multiparameter_idealmix
