module single_component
  !> Interface to single-component equations of state
  use eos_parameters, only: single_eos
  implicit none
  save

  public :: Zfac_single, enthalpy_single, entropy_single
  public :: lnfugCoeff_single, Gres_single, pressure_single
  public :: Fid_single, Fres_single

contains

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
  !!
  !! \author MH
  subroutine Zfac_single(nc,seos,T,P,Z,phase,Zfac,dZdt,dZdp,dZdz)
    use eosdata
    use mbwr_additional, only: mbwr_zfac, mbwr_volume
    implicit none
    integer, intent(in) :: nc
    class(single_eos), intent(inout) :: seos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    real, intent(out) :: Zfac
    real, optional, intent(out) :: dZdt, dZdp
    real, optional, dimension(nc), intent(out) :: dZdz
    ! Locals
    real :: v
    !
    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      v = mbwr_volume (t,p,Z(1),phase,seos%mbwr_meos(1))
      call MBWR_zfac(seos%mbwr_meos(1), t, p, v, Z(1), zfac, dzdt,dzdp,dzdz)
    case (meosNist)
      call seos%nist(1)%meos%calc_zfac(t,p,Z,phase, zfac, dzdt, dzdp, dzdz)
    end select Choice_EoS

  end subroutine Zfac_single

  !---------------------------------------------------------------------- >
  !> This function calculates the Enthalpy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param residual Return only the residual part if .true.
  !! \param enthalpy Enthalpy [J/mol]
  !! \param dhdt Temperature derivative [J/moleK]
  !! \param dhdp Pressure derivative [J/molePa]
  !! \param dhdz Composition derivative [J/mole^2]
  !!
  !! \author MH
  subroutine enthalpy_single(nc,comp,seos,T,P,Z,phase,residual,&
       enthalpy,dhdt,dhdp,dhdz)
    use eosdata
    use compdata, only: gendata_pointer
    use mbwr_additional, only: mbwr_hres, mbwr_volume
    use ideal, only: Hideal_mix
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(nc) :: comp
    class(single_eos), intent(inout) :: seos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    real, intent(out) :: enthalpy
    real, optional, intent(out) :: dhdt, dhdp
    real, optional, dimension(nc), intent(out) :: dhdz
    integer, intent(in) :: phase
    logical, intent(in) :: residual
    ! Locals
    real :: h_id, dhdt_id, dhdp_id
    real, dimension(nc) :: dhdz_id
    real :: v
    !
    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      if (.not. residual) then
        call Hideal_mix(nc, comp, T, Z, h_id, dhdt, dhdp, dhdz)
        if(present(dhdt)) dhdt_id = dhdt
        if(present(dhdp)) dhdp_id = dhdp
        if(present(dhdz)) dhdz_id = dhdz
      endif
      v = mbwr_volume (t,p,Z(1),phase,seos%mbwr_meos(1))
      call MBWR_Hres(seos%mbwr_meos(1), t, p, v, Z(1), enthalpy, dhdt,dhdp,dhdz)
      if (.not. residual) then
        enthalpy = enthalpy + h_id
        if(present(dhdt)) dhdt = dhdt + dhdt_id
        if(present(dhdp)) dhdp_id = dhdp + dhdp_id
        if(present(dhdz)) dhdz_id = dhdz + dhdz_id
      endif
     case (meosNist)
      call seos%nist(1)%meos%calc_enthalpy(t, p, Z, phase, enthalpy, &
           dhdt, dhdp, dhdz, residual=residual)
    end select Choice_EoS
  end subroutine enthalpy_single

  !---------------------------------------------------------------------- >
  !> This function calculates the Entropy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param residual Return only the residual part if .true.
  !! \param entropy The entropy [J/mol K]
  !! \param dsdt Temperature derivative [J/mol K]
  !! \param dsdp Pressure derivative [J/mol Pa]
  !! \param dsdz Composition derivative [J/mol^2]
  !!
  !! \author MH
  subroutine entropy_single(nc,comp,seos,T,P,Z,phase,residual,&
       entropy,dsdt,dsdp,dsdz)
    use eosdata
    use compdata, only: gendata_pointer
    use mbwr_additional, only: mbwr_sres, mbwr_volume
    use ideal, only: TP_Sideal_mix
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(nc) :: comp
    class(single_eos), intent(inout) :: seos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    logical, intent(in) :: residual
    real, intent(out) :: entropy
    real, optional, intent(out) :: dsdt, dsdp
    real, optional, dimension(nc), intent(out) :: dsdz
    integer, intent(in) :: phase
    ! Locals
    real :: s_id, dsdt_id, dsdp_id
    real, dimension(nc) :: dsdz_id
    real :: v
    !
    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      if (.not. residual) then
        call TP_Sideal_mix(nc, comp, T, P, Z, S_id, dsdt, dsdp, dsdz)
        if(present(dsdt)) dsdt_id = dsdt
        if(present(dsdp)) dsdp_id = dsdp
        if(present(dsdz)) dsdz_id = dsdz
      endif
      v = mbwr_volume (t,p,Z(1),phase,seos%mbwr_meos(1))
      call MBWR_Sres(seos%mbwr_meos(1), t, p, v, Z(1), entropy, dsdt, dsdp, dsdz)
      if (.not. residual) then
        entropy = entropy + s_id
        if(present(dsdt)) dsdt = dsdt + dsdt_id
        if(present(dsdp)) dsdp_id = dsdp + dsdp_id
        if(present(dsdz)) dsdz_id = dsdz + dsdz_id
      endif
    case (meosNist)
      call seos%nist(1)%meos%calc_entropy(t, p, Z, phase, entropy, &
           dsdt, dsdp, dsdz, residual=residual)
    end select Choice_EoS

  end subroutine entropy_single

  !---------------------------------------------------------------------- >
  !> This function calculates the Fugacity coefficient and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param dlnfdt Temperature derivative [1/K] (dlog(f)/dT)
  !! \param dlnfdp Pressure derivative [1/Pa]   (dlog(f)/dP)
  !! \param dlnfdz Composition derivative [1/mol] (dlog(f)/dNi)
  !! \param numder Analytical derivatives if true (default false)
  !! \param lnfug The fugacity coefficients [-]
  !!
  !! \author MH
  subroutine lnfugCoeff_single(nc,seos,T,P,Z,phase,lnfug,dlnfdt,dlnfdp,dlnfdz)
    use eosdata
    use mbwr_additional, only: mbwr_lnphi, mbwr_volume
    implicit none
    integer, intent(in) :: nc
    class(single_eos), intent(inout) :: seos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    real, dimension(nc), intent(out) :: lnfug
    real, optional, intent(out) :: dlnfdt(nc), dlnfdp(nc), dlnfdz(nc,nc)
    ! Locals
    real :: v

    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      v = mbwr_volume (t,p,Z(1),phase,seos%mbwr_meos(1))
      call MBWR_lnphi(seos%mbwr_meos(1), t, p, v, Z(1), lnfug, &
           dlnfdt, dlnfdp, dlnfdz)
    case (meosNist)
      call seos%nist(1)%meos%calc_lnphi(t, p, Z, phase, lnfug, &
           dlnfdt, dlnfdp, dlnfdz)
    end select Choice_EoS

  end subroutine lnfugCoeff_single

  !---------------------------------------------------------------------- >
  !> This subroutine calculates the residual gibbs energy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gr Residual gibbs energy [J/mol]
  !! \param dgrdt Temperature derivative [J/mol/K]
  !! \param dgrdp Pressure derivative [J/mol/Pa]
  !!
  !! \author Morten H.
  subroutine Gres_single(nc,seos,T,P,Z,phase,gr,dgrdt,dgrdp)
    use eosdata
    use mbwr_additional, only: MBWR_volume, MBWR_Gres
    implicit none
    integer, intent(in) :: nc
    class(single_eos), intent(inout) :: seos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    real, intent(out) :: gr
    real, optional, intent(out) :: dgrdt, dgrdp
    ! Locals
    real :: v

    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      v = mbwr_volume (t,p,Z(1),phase,seos%mbwr_meos(1))
      call MBWR_Gres(seos%mbwr_meos(1), t, p, v, Z(1), gr)
      if (present(dgrdt)) call stoperror("dgrdt not implemented in MBWR_Gres")
      if (present(dgrdp)) call stoperror("dgrdp not implemented in MBWR_Gres")
    case (meosNist)
      call seos%nist(1)%meos%calc_resgibbs(t, p, Z, phase, gr, dgrdt, dgrdp)
    end select Choice_EoS

  end subroutine Gres_Single

  !> Calculate pressure given composition, temperature and density
  !!
  !! \param T - Temprature [K]
  !! \param v - Specific volume [m3/mol]
  !!
  subroutine pressure_single(nc,seos,T,v,n,p,dpdv,dpdt,dpdz)
    use eosdata
    use mbwr_additional, only: MBWR_press
    implicit none
    integer, intent(in) :: nc
    class(single_eos), intent(inout) :: seos
    real, intent(in) :: t, v, n(nc)
    real, intent(inout) :: p
    real, optional, intent(inout) :: dpdv, dpdt
    real, dimension(nc), optional, intent(out) :: dpdz
    ! Locals
    real :: rho
    !---------------------------------------------------------------------
    rho = 1.0/v
    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      call MBWR_press(seos%mbwr_meos(1), T, v, sum(n), p, dpdv, dpdt)
    case (meosNist)
      call seos%nist(1)%meos%mp_pressure(rho=rho,t=T,p=p,p_rho=dpdv,p_T=dpdt)
      if (present(dpdv)) then
        dpdv = -dpdv/v**2
      end if
    end select Choice_EoS
    if (present(dpdz)) then
      call stoperror("single_component::pressure_single: dpdz not yet implemented")
    endif
  end subroutine pressure_single

  !> Calculate resudial reduced Helmholtz and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  subroutine Fres_single(nc,seos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use eosdata
    use mbwr_additional, only: MBWR_Fres
    implicit none
    integer, intent(in) :: nc
    real, intent(in) :: T,V,n(nc)
    ! Output.
    class(single_eos), intent(inout) :: seos
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    ! Locals
    real :: v_l
    real, pointer :: F_n_p, F_nn_p, F_Tn_p, F_Vn_p
    real, target :: F_n_l, F_nn_l, F_Tn_l, F_Vn_l
    !---------------------------------------------------------------------
    if (present(F_n)) then
      F_n_p => F_n_l
    else
      F_n_p => NULL()
    endif
    if (present(F_nn)) then
      F_nn_p => F_nn_l
    else
      F_nn_p => NULL()
    endif
    if (present(F_Tn)) then
      F_Tn_p => F_Tn_l
    else
      F_Tn_p => NULL()
    endif
    if (present(F_Vn)) then
      F_Vn_p => F_Vn_l
    else
      F_Vn_p => NULL()
    endif
    v_l = V/n(1)

    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      call MBWR_Fres(seos%mbwr_meos(1), T, V, n(1), F, &
           F_T, F_v, F_TT, F_Tv, F_vv, F_n_p, F_Tn_p, F_vn_p, F_nn_p)
      if (present(F_Vn)) F_Vn = F_Vn_l
      if (present(F_n)) F_n = F_n_l
      if (present(F_nn)) F_nn = F_nn_l
      if (present(F_Tn)) F_Tn = F_Tn_l
    case (meosNist)
      call seos%nist(1)%meos%alphaResDerivs_Tv(T, v_l, F, &
           F_T, F_v, F_TT, F_Tv, F_vv, F_n_p, F_Tn_p, F_vn_p, F_nn_p)
      ! F = n*alpha(tau,delta)
      if (present(F)) F = F*n(1)
      if (present(F_T)) F_T = F_T*n(1)
      if (present(F_TT)) F_TT = F_TT*n(1)
      if (present(F_VV)) F_VV = F_VV/n(1)
      if (present(F_n)) then
        F_n = F_n_l
      endif
      if (present(F_nn)) then
        F_nn = F_nn_l/n(1)
      endif
      if (present(F_Tn)) then
        F_Tn = F_Tn_l
      endif
      if (present(F_Vn)) then
        F_Vn = F_Vn_l/n(1)
      endif
    end select Choice_EoS

  end subroutine Fres_single

  !> Calculate reduced ideal Helmholtz energy and differentials
  !!
  !! \param T - Temprature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  subroutine Fid_single(nc,comp,seos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use eosdata
    use compdata, only: gendata_pointer
    use ideal, only: Fideal_mix_SI
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(nc) :: comp
    real, intent(in) :: T,V,n(nc)
    ! Output.
    class(single_eos), intent(inout) :: seos
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    ! Locals
    real :: v_l
    real, pointer :: F_n_p, F_nn_p, F_Tn_p, F_Vn_p
    real, target :: F_n_l, F_nn_l, F_Tn_l, F_Vn_l
    !---------------------------------------------------------------------
    if ( seos%subeosidx == meosNist) then
      if (present(F_n)) then
        F_n_p => F_n_l
      else
        F_n_p => NULL()
      endif
      if (present(F_nn)) then
        F_nn_p => F_nn_l
      else
        F_nn_p => NULL()
      endif
      if (present(F_Tn)) then
        F_Tn_p => F_Tn_l
      else
        F_Tn_p => NULL()
      endif
      if (present(F_Vn)) then
        F_Vn_p => F_Vn_l
      else
        F_Vn_p => NULL()
      endif
    endif
    v_l = V/sum(n)

    !-------- Specific for each equation of state ------------------------
    Choice_EoS: select case (seos%subeosidx) ! choose the Equation of State
    case (meosMbwr19, meosMbwr32)
      call Fideal_mix_SI(nc, comp, T, V, n, Fid=F, Fid_T=F_T, Fid_v=F_V, &
           Fid_n=F_n, Fid_TT=F_TT, Fid_vv=F_vv, Fid_nn=F_nn, Fid_Tv=F_TV,&
           Fid_vn=F_Vn, Fid_Tn=F_Tn)
    case (meosNist)
      call seos%nist(1)%meos%alphaIdDerivs_Tv(T,v_l,alp=F,alp_T=F_T,&
           alp_v=F_V,alp_TT=F_TT,alp_Tv=F_TV,alp_vv=F_VV, &
           alp_n=F_n_p, alp_Tn=F_Tn_p, alp_vn=F_vn_p, alp_nn=F_nn_p)
      ! F = n*alpha(tau,delta)
      if (present(F)) F = F*n(1)
      if (present(F_T)) F_T = F_T*n(1)
      if (present(F_TT)) F_TT = F_TT*n(1)
      if (present(F_VV)) F_VV = F_VV/n(1)
      if (present(F_n)) then
        F_n = F_n_l
      endif
      if (present(F_nn)) then
        F_nn = F_nn_l/n(1)
      endif
      if (present(F_Tn)) then
        F_Tn = F_Tn_l
      endif
      if (present(F_Vn)) then
        F_Vn = F_Vn_l/n(1)
      endif
    end select Choice_EoS

  end subroutine Fid_single

end module single_component
