module single_phase
  implicit none
  save

  ! Assign Values to numerical derivation routine
  real, parameter :: delta_T = 1.0D-6
  real, parameter :: delta_P = 1.0D-2
  real, parameter :: delta_Ni = 1.0D-6

  public :: TP_CalcMw, TP_CalcZfac, TP_CalcEnthalpy, TP_CalcEntropy
  public :: TP_CalcFugacity, TV_CalcInnerEnergy, TP_CalcGibbs, TV_CalcPressure
  public :: TP_CalcPseudo, TV_CalcFreeEnergy, TV_CalcFres, TV_CalcFid
  public :: TV_CalcFugacity, TV_CalcEntropy
contains
  !---------------------------------------------------------------------- >
  !> The function returns the moleweight of a mixture
  !!
  !! \param n The mole numbers [-]
  !! \retval Mw The moleweight [kg/kmole]
  !!
  !! \author Oivind W
  function TP_CalcMw(nc,comp,n) result (Mw)
    use compdata, only: gendata_pointer
    use thermopack_var, only: nce, apparent_to_real_mole_numbers
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in), dimension(:) :: comp
    real, dimension(nc), intent(in) :: n !< The mole numbers [mole]
    ! Locals
    real :: Mw, nMoles, ne(nce)
    integer :: i

    call apparent_to_real_mole_numbers(n,ne)
    nMoles = sum(ne)
    Mw = 0.0
    do i=1,nce
      Mw=Mw+ne(i)*comp(i)%p_comp%mw
    end do
    if (nMoles > 0.0) then
      Mw = Mw/nMoles
    endif

  end function TP_CalcMw

  !---------------------------------------------------------------------- >
  !> This function calculates the Compressibility factor with derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gflag_opt
  !!       1: Normal
  !!       2: if the metastable maxima or minima of the z-factor are to be returned
  !!       3: If possibilities of having three roots, return the one having minimum
  !!        Gibbs free energy is to be returned - calls cbGres
  !! \param dZdt Temperature derivative [1/K]
  !! \param dZdp Pressure derivative [1/Pa]
  !! \param dZdz Composition derivative [-]
  !!
  !! \retval Zfac The Z-factor [-]
  !!
  !! \author Oivind W
  function TP_CalcZfac(nc,comp,cbeos,T,P,Z,phase,gflag_opt,dZdt,dZdp,dZdz) result (Zfac)
    use compdata, only: gendata_pointer
    use thermopack_var, only: base_eos_param
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    integer, optional, intent(in) :: gflag_opt
    real, optional, intent(out) :: dZdt, dZdp
    real, optional, dimension(nc), intent(out) :: dZdz
    real :: Zfac

    Zfac = fork_Zfac_calculation(nc,comp,cbeos,T,P,Z/sum(Z),phase,gflag_opt,&
         dZdt,dZdp,dZdz)

  end function TP_CalcZfac

  !---------------------------------------------------------------------- >
  !> This function calculates the Enthalpy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gflag_opt
  !!       1: Normal
  !!       2: if the metastable maxima or minima of the z-factor are to be returned
  !!       3: If possibilities of having three roots, return the one having minimum
  !!        Gibbs free energy is to be returned - calls cbGres
  !! \param residual Return only the residual part if .true.
  !! \param dhdt Temperature derivative [J/molK]
  !! \param dhdp Pressure derivative [J/molPa]
  !! \param dhdz Composition derivative [J/mol^2]
  !!
  !! \retval enthalpy The enthalpy with derivatives [-]
  !!
  !! \author Oivind W
  function TP_CalcEnthalpy(nc,comp,cbeos,T,P,n,phase,residual,&
       dhdt,dhdp,dhdz,gflag_opt) result (enthalpy)
    use cubic
    use ideal
    use stringmod, only: str_eq
    use LeeKesler, only: lkCalcEnthalpy
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: volumeShiftEnthalpy, NOSHIFT
    use thermopack_var, only: nce, apparent_to_real_mole_numbers
    use multiparameter_idealmix, only: calc_multiparameter_idealmix_enthalpy
    use single_component, only: enthalpy_single
    use thermopack_var, only: base_eos_param
    use eos_parameters, only: meos_mix, single_eos
    use cubic_eos, only: lk_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, P
    real, optional, intent(out) :: dhdt, dhdp
    real, optional, dimension(nc), intent(out) :: dhdz
    integer, intent(in) :: phase
    logical, intent(in) :: residual
    integer, optional, intent(in) :: gflag_opt
    real :: enthalpy
    ! Locals
    real :: h_ideal_mix, dhdt_ideal_mix, dhdp_ideal_mix
    real, dimension(nce) :: dhdz_ideal_mix, ne
    !
    call apparent_to_real_mole_numbers(n,ne)
    if (nc /= nce .and. present(dhdz)) then
      call stoperror("TP_CalcEnthalpy: dhdn not yet implemented for apparent mode")
    endif

    ! Ideal gas enthalpy
    if (residual .or. eos_single == cbeos%eosidx) then
      H_ideal_mix = 0.0
      dhdt_ideal_mix=0.0
      dhdp_ideal_mix=0.0
      dhdz_ideal_mix=0.0
    else
       call Hideal_mix(nce, comp, T, ne, h_ideal_mix, dhdt_ideal_mix, dhdp_ideal_mix, dhdz_ideal_mix)
    endif

    select type ( p_eos => cbeos )
    type is ( lk_eos ) ! Lee-Kesler equations of state
      call lkCalcEnthalpy(nce,comp,p_eos,T,p,ne,phase,enthalpy,dhdt,dhdp,dhdz)
    type is ( single_eos )
      call enthalpy_single(nce,comp,p_eos,T,P,ne,phase,residual,&
           enthalpy,dhdt,dhdp,dhdz)
      return ! Already includes ideal contibution
    type is ( meos_mix )
      call calc_multiparameter_idealmix_enthalpy(nc, p_eos, T, p, ne, phase, &
           enthalpy, dhdt, dhdp, dhdz)
    class default
       call TP_ResidEnthalpy(nce,comp,cbeos,phase,T,P,ne,enthalpy,&
            dhdt,dhdp,dhdz,gflag_opt=gflag_opt)
     end select

    if (cbeos%volumeShiftId /= NOSHIFT) then
      call volumeShiftEnthalpy(nc,comp,cbeos%volumeShiftId,T,P,n,phase,&
           enthalpy,dhdt,dhdp,dhdz)
    endif

    ! Add ideal gas contributions
    if (.not. residual) then
      enthalpy=enthalpy+H_ideal_mix
      if(present(dhdt)) then ! Temperature derivative
        dhdt=dhdt+dhdt_ideal_mix
      end if
      if(present(dhdp)) then ! Pressure derivative
        dhdp=dhdp+dhdp_ideal_mix
      end if
      if(present(dhdz)) then ! Composition derivative,
        dhdz=dhdz+dhdz_ideal_mix
      end if
    endif
  end function tp_CalcEnthalpy

  !---------------------------------------------------------------------- >
  !> This function calculates the Entropy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gflag_opt
  !!       1: Normal
  !!       2: if the metastable maxima or minima of the z-factor are to be returned
  !!       3: If possibilities of having three roots, return the one having minimum
  !!        Gibbs free energy is to be returned - calls cbGres
  !! \param residual Return only the residual part if .true.
  !! \param dsdt Temperature derivative [J/molK]
  !! \param dsdp Pressure derivative [J/molPa]
  !! \param dsdz Composition derivative [J/mol^2]
  !!
  !! \retval entropy The entropy [J/mol K]
  !!
  !! \author Oivind W
  function TP_CalcEntropy(nc,comp,cbeos,T,P,n,phase,residual,&
       dsdt,dsdp,dsdz,gflag_opt) result (entropy)
    use cubic
    use ideal
    use stringmod, only: str_eq
    use LeeKesler, only: lkCalcEntropy
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: volumeShiftEntropy, NOSHIFT
    use thermopack_var, only: nce, apparent_to_real_mole_numbers
    use multiparameter_idealmix, only: calc_multiparameter_idealmix_entropy
    use single_component, only: entropy_single
    use thermopack_var, only: base_eos_param
    use eos_parameters, only: meos_mix, single_eos
    use cubic_eos, only: lk_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, P
    logical, intent(in) :: residual
    real, optional, intent(out) :: dsdt, dsdp
    real, optional, dimension(nc), intent(out) :: dsdz
    integer, intent(in) :: phase
    integer, optional, intent(in) :: gflag_opt
    real :: entropy
    ! Locals
    real :: S_ideal_mix, dsdt_ideal_mix, dsdp_ideal_mix
    real, dimension(nce) :: dsdz_ideal_mix, ne

    call apparent_to_real_mole_numbers(n,ne)
    if (nc /= nce .and. present(dsdz)) then
      call stoperror("TP_CalcEntropy: dsdn not yet implemented for apparent mode")
    endif

    ! Ideal gas entropy
    if (residual .or. eos_single == cbeos%eosidx) then
      S_ideal_mix = 0.0
      dsdt_ideal_mix=0.0
      dsdp_ideal_mix=0.0
      dsdz_ideal_mix=0.0
    else
       call TP_Sideal_mix(nce, comp, T, P, ne, S_ideal_mix, dsdt_ideal_mix, dsdp_ideal_mix, dsdz_ideal_mix)
    endif

    select type ( p_eos => cbeos )
    type is ( lk_eos ) ! Lee-Kesler equations of state
      call lkCalcEntropy(nce,comp,p_eos,T,p,ne,phase,entropy,dsdt,dsdp,dsdz)
    type is ( single_eos )
      call entropy_single(nce,comp,p_eos,T,P,ne,phase,residual,&
           entropy,dsdt,dsdp,dsdz)
    type is ( meos_mix )
      call calc_multiparameter_idealmix_entropy(nc, p_eos, T, p, ne, phase, &
           entropy, dsdt, dsdp, dsdz)
    class default
      call TP_ResidEntropy(nce,comp,cbeos,phase,T,P,ne,entropy,dSdt,dSdp,dSdz,gflag_opt)
    end select
    if (cbeos%volumeShiftId /= NOSHIFT) then
      call volumeShiftEntropy(nce,comp,cbeos%volumeShiftId,T,P,ne,phase,&
           entropy,dsdt,dsdp,dsdz)
    endif

    if (.not. residual) then
      ! Add ideal gas contributions
      entropy=entropy+S_ideal_mix
      if(present(dsdt)) then ! Temperature derivative
        dsdt=dsdt+dsdt_ideal_mix
      end if
      if(present(dsdp)) then ! Pressure derivative
        dsdp=dsdp+dsdp_ideal_mix
      end if
      if(present(dsdz)) then ! Composition derivative,
        dsdz=dsdz+dsdz_ideal_mix
      end if
    endif

  end function TP_CalcEntropy

  !---------------------------------------------------------------------- >
  !> This function calculates the internal energy and its derivatives.
  !!
  !! \param T The temperature [K]
  !! \param v The specific volume [m3/mol]
  !! \param Z The mole fraction [-]
  !! \param dudt Temperature derivative [J/molK]
  !! \param dudv Pressure derivative [J/m3]
  !!
  !! \retval energy The internal energy with derivatives [-]
  !!
  !! \author Morten Hammer
  function TV_CalcInnerEnergy(nc,comp,cbeos,T,v,n,dudt,dudv,dudn,&
       recalculate) result (u)
    use thermopack_constants, only: Rgas
    use ideal
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: NOSHIFT
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff, base_eos_param
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, v
    real, optional, intent(out) :: dudt, dudv, dudn(nc)
    logical, optional, intent(in) :: recalculate
    real :: u
    !
    real :: F_T, u_id, F_Tn(nc)
    real, pointer :: F_TT_p, F_TV_p!, F_Tn_p(:)
    real, target :: F_TT_l, F_TV_l, F_Tn_l(nce)
    real, dimension(nce) :: ne
    call apparent_to_real_mole_numbers(n,ne)

    if (cbeos%volumeShiftId /= NOSHIFT) then
      call stoperror("TV_CalcInnerEnergy volume shift not supported for this function")
    endif

    if (present(dudt)) then
      F_TT_p => F_TT_l
    else
      F_TT_p => NULL()
    endif
    if (present(dudv)) then
      F_TV_p => F_TV_l
    else
      F_TV_p => NULL()
    endif
    !if (present(dudn)) then
    !  F_Tn_p => F_Tn_l
    !else
    !  F_Tn_p => NULL()
    !endif

    call TV_CalcFres(nce,comp,cbeos,T,V,ne,F_T=F_T,F_TT=F_TT_p,&
         F_TV=F_TV_p,F_Tn=F_Tn_l,recalculate=recalculate)

    u = (-Rgas)*T**2*F_T
    if (present(dudv)) dudv = (-Rgas)*T**2*F_TV_l
    if (present(dudt)) dudt = (-Rgas)*T*(T*F_TT_l+2.0*F_T)
    if (present(dudn)) then
      call real_to_apparent_diff(F_Tn_l,F_Tn)
      dudn = (-Rgas)*T**2*F_Tn
    endif
    ! Add ideal gas contributions
    call TV_CalcFid(nce,comp,cbeos,T,V,ne,F_T=F_T,F_TT=F_TT_p,&
      F_TV=F_TV_p,F_Tn=F_Tn_l)
    u_id = (-Rgas)*T**2*F_T
    u = u + u_id
    if (present(dudv)) dudv = dudv + (-Rgas)*T**2*F_TV_l
    if (present(dudt)) dudt = dudt + (-Rgas)*T*(T*F_TT_l+2.0*F_T)
    if (present(dudn)) then
      call real_to_apparent_diff(F_Tn_l,F_Tn)
      dudn = dudn + (-Rgas)*T**2*F_Tn
    endif
  end function TV_CalcInnerEnergy

  !---------------------------------------------------------------------- >
  !> This function calculates the Helmholtz free energy
  !! and, optionally, derivatives
  !! with respect to T, v
  !!
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/mol]
  !! \param Z Mole fraction [-]
  !! \param dYdt Temperature derivative [J/molK]
  !! \param dYdv Pressure derivative [J/m3]
  !!
  !! \retval Y Free energy with derivatives [-]
  !!
  !! \author GL, 22-01-2015
  function TV_CalcFreeEnergy(nc,comp,cbeos,T,v,n,&
       dYdt,dYdv,dYdn,recalculate) result (Y)
    use cubic
    use ideal
    use stringmod, only: str_eq
    use eosdata
    use extcsp, only: csp_calcFres
    use saft_interface, only: calcSaftFder_res
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    use volume_shift, only: NOSHIFT
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff, base_eos_param
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, v
    real, optional, intent(out) :: dYdt, dYdv, dYdn(nc)
    logical, optional, intent(in) :: recalculate
    real :: Y
    !
    real :: F, F_n(nc)
    real, pointer :: F_T_p, F_V_p, F_n_p(:)
    real, target :: F_T_l, F_V_l, F_n_l(nce)
    real, dimension(nce) :: ne
    call apparent_to_real_mole_numbers(n,ne)

    if (cbeos%volumeShiftId /= NOSHIFT) then
      call stoperror("TV_CalcFreeEnergy volume shift not supported for this function")
    endif

    if (present(dYdT)) then
      F_T_p => F_T_l
    else
      F_T_p => NULL()
    endif
    if (present(dYdV)) then
      F_V_p => F_V_l
    else
      F_V_p => NULL()
    endif
    if (present(dYdn)) then
      F_n_p => F_n_l
    else
      F_n_p => NULL()
    endif

    ! Residual reduced Helmholtz
    call TV_CalcFres(nce,comp,cbeos,T,V,ne,F=F,F_T=F_T_p,F_V=F_V_p,F_n=F_n_p,&
         recalculate=recalculate)
    Y = Rgas*T*F
    if (present(dYdt)) dYdt = Rgas*(F + T*F_T_l)
    if (present(dYdv)) dYdv = Rgas*T*F_V_l
    if (present(dYdn)) then
      call real_to_apparent_diff(F_n_l,F_n)
      dYdn = Rgas*T*F_n
    endif
    ! Ideal reduced Helmholtz
    call TV_CalcFid(nce,comp,cbeos,T,V,ne,F=F,F_T=F_T_p,F_V=F_V_p,F_n=F_n_p)
    Y = Y + Rgas*T*F
    if (present(dYdt)) dYdt = dYdt + Rgas*(F + T*F_T_l)
    if (present(dYdv)) dYdv = dYdv + Rgas*T*F_V_l
    if (present(dYdn)) then
      call real_to_apparent_diff(F_n_l,F_n)
      dYdn = dYdn + Rgas*T*F_n
    endif

  end function TV_CalcFreeEnergy

  !---------------------------------------------------------------------- >
  !> This function calculates the Fugacity coefficient and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gflag_opt
  !!       1: Normal
  !!       2: if the metastable maxima or minima of the z-factor are to be returned
  !!       3: If possibilities of having three roots, return the one having minimum
  !!        Gibbs free energy is to be returned - calls cbGres
  !! \param dlnfdt Temperature derivative [1/K] (dlog(f)/dT)
  !! \param dlnfdp Pressure derivative [1/Pa]   (dlog(f)/dP)
  !! \param dlnfdz Composition derivative [1/mol] (dlog(f)/dNi)
  !! \param lnfug Logarithm of the fugacity coefficients [-]
  !!
  !! \author Oivind W
  subroutine TP_CalcFugacity(nc,comp,cbeos,T,P,Z,phase,lnfug,&
       dlnfugdt,dlnfugdp,dlnfugdn,gflag_opt,v)
    use cubic
    use LeeKesler, only: lkCalcFug
    use eosdata
    use compdata, only: gendata_pointer
    use multiparameter_idealmix, only: calc_multiparameter_idealmix_fugacity
    use thermopack_var, only: base_eos_param
    use eos_parameters, only: meos_mix
    use cubic_eos, only: lk_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: Z
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    integer, optional, intent(in) :: gflag_opt
    real, dimension(nc), intent(out) :: lnfug
    real, optional, dimension(nc), intent(out) :: dlnfugdt, dlnfugdp
    real, optional, dimension(nc,nc), intent(out) :: dlnfugdn
    real, optional, intent(out) :: v !< Specific volume [mol/m3]

    select type ( p_eos => cbeos )
    type is ( lk_eos ) ! Lee-Kesler equations of state
      call lkCalcFug(nc,comp,p_eos,T,p,z,phase,lnfug,dlnfugdt,dlnfugdp,dlnfugdn,v)
    type is ( meos_mix )
      call calc_multiparameter_idealmix_fugacity(nc, p_eos, T, p, Z, phase, &
           lnfug,dlnfugdT,dlnfugdP,dlnfugdn)
    class default
      call TP_lnfug(nc,comp,cbeos,phase,T,P,z,lnfug,&
             dlnfugdT,dlnfugdP,dlnfugdn,gflag_opt,v_out=v)
    end select
  end subroutine TP_CalcFugacity

  !---------------------------------------------------------------------- >
  !> This subroutine calculates the residual gibbs energy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The overall mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gflag_opt
  !!       1: Normal
  !!       2: if the metastable maxima or minima of the z-factor are to be returned
  !!       3: If possibilities of having three roots, return the one having minimum
  !!        Gibbs free energy is to be returned - calls cbGres
  !! \param g Gibbs energy [J/mol]
  !! \param dgdt Temperature derivative [J/mol/K]
  !! \param dgdp Pressure derivative [J/mol/Pa]
  !!
  !! \author Morten H.
  subroutine TP_CalcGibbs(nc,comp,cbeos,T,P,n,phase,residual,g,&
       dgdt,dgdp,dgdn,gflag_opt,recalculate)
    use cubic
    use ideal
    use LeeKesler, only: lkCalcGdep
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: volumeShiftGibbs, NOSHIFT
    use thermopack_constants, only: Rgas
    use single_component, only: Gres_single
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff, base_eos_param
    use multiparameter_idealmix, only: calc_multiparameter_idealmix_Gres
    use eos_parameters, only: meos_mix
    use cubic_eos, only: lk_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    logical, intent(in) :: residual
    integer, optional, intent(in) :: gflag_opt
    logical, optional, intent(in) :: recalculate
    real, intent(out) :: g
    real, optional, intent(out) :: dgdt, dgdp, dgdn(nc)
    ! Locals
    real, pointer :: F_T_p, F_n_p(:)
    real, target :: F_T_l, F_n_l(nce)
    real :: Zfac, v, sumne, vid
    real :: F, logZfac, F_n(nc)
    integer :: VSHIFTID, i
    real, dimension(nce) :: ne
    call apparent_to_real_mole_numbers(n,ne)

    if (present(dgdT)) then
      F_T_p => F_T_l
    else
      F_T_p => NULL()
    endif
    if (present(dgdn)) then
      F_n_p => F_n_l
    else
      F_n_p => NULL()
    endif

    select type ( p_eos => cbeos )
    type is ( lk_eos ) ! Lee-Kesler equations of state
      call lkCalcGdep(nc,comp,p_eos,T,P,n,phase,g,dgdt,dgdp)
      if (present(dGdn)) then
        call stoperror("TP_CalcGibbs: Mole number differentials not available")
      endif
    type is ( meos_mix )
      call calc_multiparameter_idealmix_Gres(nc, p_eos, T, p, n, phase, &
           g,dgdt,dgdp)
      if (present(dGdn)) then
        call stoperror("TP_CalcGibbs: Mole number differentials not available")
      endif
    class default
      sumne = sum(ne)
      VSHIFTID = cbeos%volumeShiftId
      cbeos%volumeShiftId = NOSHIFT ! Disable volume shift
      zFac = TP_CalcZfac(nce,comp,cbeos,T,P,ne/sumne,phase,gflag_opt)
      v = zFac*sumne*Rgas*T/P

      call TV_CalcFres(nce,comp,cbeos,T,v,ne,F=F,F_T=F_T_p,&
        F_n=F_n_p,recalculate=recalculate)

      logZfac = log(Zfac)
      g = Rgas*T*F + P*V - sumne*Rgas*T*(1.0+logZfac)

      if (present(dGdP)) then
        dGdp = V*(1.0-1.0/zFac)
      endif

      if (present(dGdT)) then
        ! Use that dGdT = - S
        dGdT =  Rgas*(F + T*F_T_l - sumne*logZfac)
      end if

      if (present(dGdn)) then
        do i = 1,nce
          F_n_l(i) = Rgas*T*(F_n_l(i) - logZfac)
        end do
        call real_to_apparent_diff(F_n_l,dGdn)
      endif

      cbeos%volumeShiftId = VSHIFTID ! Reset volume shift flag
      if (cbeos%volumeShiftId /= NOSHIFT) then
        call volumeShiftGibbs(nce,comp,cbeos%volumeShiftId,T,P,ne,phase,g,dgdt,dgdp)
      endif
    end select

    ! Add ideal contribution
    if (.not. residual) then
      ! Evaluate using ideal volume at P
      vid = sumne*Rgas*T/P
      call TV_CalcFid(nce,comp,cbeos,T,vid,ne,F=F,F_T=F_T_p,&
           F_n=F_n_p)
      g = g + Rgas*T*(F + sumne)
      if(present(dgdt)) then ! Temperature derivative
        dgdt = dgdt + Rgas*(F + T*F_T_l)
      end if
      if(present(dgdp)) then ! Pressure derivative
        dgdp = dgdp + sumne*Rgas*T/P
      end if
      if(present(dgdn)) then ! Composition derivative,
        call real_to_apparent_diff(F_n_l,F_n)
        dgdn = dgdn + Rgas*T*F_n
      end if
    endif

  end subroutine TP_CalcGibbs

  !> Calculate pressure given composition, temperature and density
  !!
  !! \param T - Temprature [K]
  !! \param v - Specific volume [m3/mol]
  !!
  subroutine TV_CalcPressure(nc,comp,cbeos,T,v,n,p,&
       dpdv,dpdt,d2pdv2,dpdn,recalculate)
    use cubic
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: volumeShiftVolume, NOSHIFT
    use thermopack_constants, only: Rgas
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff, base_eos_param
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, intent(in) :: t, v, n(nc)
    real, intent(inout) :: p
    real, optional, intent(inout) :: dpdv, dpdt, d2pdv2
    real, dimension(nc), optional, intent(out) :: dpdn
    logical, optional, intent(in) :: recalculate
    ! Locals
    real :: vshift
    real :: sumne
    real :: F_V
    real, dimension(nce) :: ne
    real, pointer :: F_Vne_p(:)
    real, target :: F_Vne(nce)
    if (present(dpdn)) then
      F_Vne_p => F_Vne
    else
      F_Vne_p => NULL()
    endif
    call apparent_to_real_mole_numbers(n,ne)
    sumne = sum(ne)
    vshift = 0.0
    if (cbeos%volumeShiftId /= NOSHIFT) then
      call volumeShiftVolume(nce,comp,cbeos%volumeShiftId,t,ne,vshift)
    endif
    vshift = v - vshift

    call TV_CalcFres(nce,comp,cbeos,T,vshift,ne,F_V=F_V,F_TV=dpdt,&
         F_VV=dpdv,F_Vn=F_Vne_p,F_VVV=d2pdv2,recalculate=recalculate)

    P = -Rgas*T*F_V + sumne*Rgas*T/V
    if (present(dPdV)) dPdV = -Rgas*T*dPdV - sumne*Rgas*T/V**2
    if (present(dPdT)) dPdT = -Rgas*T*dPdT + P/T
    if (present(dPdn)) then
      F_Vne = -Rgas*T*F_Vne + Rgas*T/V
      call real_to_apparent_diff(F_Vne,dPdn)
    endif
    if (present(d2PdV2)) d2PdV2 = -Rgas*T*d2PdV2 + 2.0*sumne*Rgas*T/V**3
  end subroutine TV_CalcPressure

  !> Calculate pseudo critical properties.
  subroutine TP_CalcPseudo(nc,comp,cbeos,n,tpc,ppc,zpc,vpc)
    use cubic, only: cbCalcPseudo
    use cubic_eos, only: cb_eos
    use compdata, only: gendata_pointer
    use thermopack_var, only: nce, apparent_to_real_mole_numbers
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: n(nc)
    real, intent(out) :: tpc,ppc,zpc,vpc
    ! Locals
    real :: ne(nce)
    call apparent_to_real_mole_numbers(n,ne)
    call cbCalcPseudo(nce,cbeos,ne,tpc,ppc,zpc,vpc)
  end subroutine TP_CalcPseudo

  !> Calculate fugacit given composition, temperature and density
  !!
  !! \param T - Temprature [K]
  !! \param v - Specific volume [m3/mol]
  !!
  subroutine TV_CalcFugacity(nc,comp,cbeos,T,v,n,lnphi,&
       lnphiT,lnphiV,lnphin,recalculate)
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: volumeShiftVolume, NOSHIFT
    use ideal, only: Sideal_Vn
    use thermopack_constants, only: Rgas
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_differentials, base_eos_param
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, intent(in) :: t, v, n(nc)
    real, dimension(nc), intent(out) :: lnphi
    real, optional, intent(out) :: lnphiT(nc) , lnphiV(nc), lnphin(nc,nc)
    logical, optional, intent(in) :: recalculate
    ! Locals
    real :: Sid, Fid_n(nce), Fid_Vn, Fid_Tn, Fid_nn(nce), F_n(nce)
    real, target :: F_Tn(nce),F_Vn(nce),F_nn(nce,nce)
    real, pointer :: F_Tn_p(:), F_Vn_p(:), F_nn_p(:,:)
    real :: vshift
    integer :: i
    real :: ne(nce), sumne
    !
    call apparent_to_real_mole_numbers(n,ne)
    sumne = sum(ne)
    if (present(lnphiT)) then
      F_Tn_p => F_Tn
    else
      F_Tn_p => NULL()
    endif
    if (present(lnphiV)) then
      F_Vn_p => F_Vn
    else
      F_Vn_p => NULL()
    endif
    if (present(lnphin)) then
      F_nn_p => F_nn
    else
      F_nn_p => NULL()
    endif
    if (cbeos%volumeShiftId /= NOSHIFT) then
      call stoperror("TV_CalcFugacity volume shift not supported for this function")
    endif
    call TV_CalcFres(nce,comp,cbeos,T,v,ne,F_n=F_n,F_Tn=F_Tn_p,&
         F_Vn=F_Vn_p,F_nn=F_nn_p,recalculate=recalculate)
    ! Ideal contribution
    vshift = v
    call Sideal_Vn(nce, ne, T, vshift, Sid, dsdn=Fid_n, d2sdndT=Fid_Tn, &
         d2sdndV=Fid_Vn, d2sdn2=Fid_nn)
    Fid_n = -Fid_n/Rgas
    Fid_Tn = -Fid_Tn/Rgas
    Fid_Vn = -Fid_Vn/Rgas
    Fid_nn = -Fid_nn/Rgas
    Fid_Vn = Fid_Vn

    F_n = F_n + Fid_n
    if (present(lnphiT)) then
      F_Tn = F_Tn + Fid_Tn
    endif
    if (present(lnphiV)) then
      F_Vn = F_Vn + Fid_Vn
    endif
    if (present(lnphin)) then
      do i=1,nce
        F_nn(i,i) = F_nn(i,i) + Fid_nn(i)
      enddo
    endif
    call real_to_apparent_differentials(Fe_n=F_n,Fe_Tn=F_Tn,&
         Fe_Vn=F_Vn,Fe_nn=F_nn,&
         F_n=lnphi,F_Tn=lnphiT,F_Vn=lnphiV,F_nn=lnphin)
  end subroutine TV_CalcFugacity

  !> Calculate residual reduced Helmholtz and differentials
  !!
  !! \param T - Temprature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  subroutine TV_CalcFres(nc,comp,cbeos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn,F_VVV,recalculate)
    use cubic, only: calcCbFder_res_SI
    use eosdata
    use extcsp, only: csp_calcFres, extcsp_eos
    use saft_interface, only: calcSaftFder_res
    use compdata, only: gendata_pointer
    use volume_shift, only: NOSHIFT
    use single_component, only: Fres_single
    use thermopack_var, only: base_eos_param
    !use pets, only: PETS_eos
    use eos_parameters, only: single_eos, meos_mix
    use cubic_eos, only: cb_eos, cpa_eos, lk_eos
    use saftvrmie_containers, only: saftvrmie_eos
    use pc_saft_nonassoc, only: PCSAFT_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(nc) :: comp
    real, intent(in) :: T,V,n(nc)
    ! Output.
    class(base_eos_param), intent(inout) :: cbeos
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc), F_VVV
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    logical, optional, intent(in) :: recalculate
    ! Locals
    real :: eF,eF_T,eF_V,eF_n(nc), eF_VVV
    real :: eF_TT,eF_TV,eF_Tn(nc),eF_VV,eF_Vn(nc),eF_nn(nc,nc)
    logical :: isCubic, do_all_derivs
    real :: v_eos
    isCubic = .false.

    if (cbeos%volumeShiftId == NOSHIFT) then
       v_eos = v
    else
       call stoperror("TV_CalcFres volume shift not supported for this function")
    end if

    isCubic = .false.

    do_all_derivs = present(F_TT) .or. present(F_TV) .or. present(F_Tn) .or. &
         present(F_VV) .or. present(F_Vn) .or. present(F_nn) .or. present(F_VVV)

    if (do_all_derivs) then
       call get_eos_F(v_eos,eF,eF_T,eF_V,eF_n,eF_TT,eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn,eF_VVV)
    else
       call get_eos_F(v_eos,eF,eF_T,eF_V,eF_n)
    end if

    ! ! Correct the F from the individual models according to the volume shift
    ! if (cbeos%volumeShiftId /= NOSHIFT) then
    !    call vshift_F_terms(nc,comp,cbeos%volumeShiftId,T,V,n,eF,eF_T,eF_V,eF_n,eF_TT,&
    !         eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn,eF_VVV)
    ! end if

    if (present(F)) F = eF
    if (present(F_T)) F_T = eF_T
    if (present(F_V)) F_V = eF_V
    if (present(F_n)) F_n = eF_n
    if (present(F_TT)) F_TT = eF_TT
    if (present(F_TV)) F_TV = eF_TV
    if (present(F_Tn)) F_Tn = eF_Tn
    if (present(F_Vn)) F_Vn = eF_Vn
    if (present(F_VV)) F_VV = eF_VV
    if (present(F_VVV)) F_VVV = eF_VVV
    if (present(F_nn)) F_nn = eF_nn

  contains
    subroutine get_eos_F(veos,eF,eF_T,eF_V,eF_n,eF_TT,eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn,eF_VVV)
      real, intent(in) :: veos
      real, optional, intent(out) :: eF,eF_T,eF_V,eF_n(nc), eF_VVV
      real, optional, intent(out) :: eF_TT,eF_TV,eF_Tn(nc),eF_VV,eF_Vn(nc),eF_nn(nc,nc)
      if (cbeos%isElectrolyteEoS) then
         call stoperror("TV_CalcFres::electrolyteeos not yet implemented")
      else
         select type ( p_eos => cbeos )
         type is ( cb_eos ) ! cubic equations of state
            call calcCbFder_res_SI(nc,p_eos,T,v,n,eF,eF_T,eF_V,eF_n,eF_TT,&
                 eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn,eF_VVV,recalculate)
            isCubic = .true.
         type is ( lk_eos ) ! Lee-Kesler equations of state
            call stoperror('Lee-Kesler model does not support TV_CalcFres')
         type is ( single_eos )
            call Fres_single(nc,p_eos,T,v,n,eF,eF_T,eF_V,eF_n,eF_TT,&
                 eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn)
         type is ( meos_mix )
            call stoperror('Not possible to call Fres as a T-V function for meosNist_mix')
         type is ( extcsp_eos ) ! Corresponding State Principle
            call csp_calcFres(nc,p_eos,T,v,n,eF,eF_T,eF_V,eF_n,eF_TT,&
                 eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn)
         class default ! Saft eos
            call calcSaftFder_res(nc,cbeos,T,v,n,eF,eF_T,eF_V,eF_n,eF_TT,&
                 eF_TV,eF_VV,eF_Tn,eF_Vn,eF_nn)
         end select
      end if
      if (present(F_VVV) .and. .not. isCubic) then
         call stoperror('F_VVV only supported for pure cubical models')
      endif
    end subroutine get_eos_F

  end subroutine TV_CalcFres


  !-----------------------------------------------------------------------------
  !> Calculate the logarithmic fugacity and its derivatives.
  !!
  !! \author Ailo A, 2015-04
  !! \author Morten Hammer, 2017-02
  !-----------------------------------------------------------------------------
  subroutine TP_lnfug(nc,comp,cbeos,phase,T,P,n,lnfug,dlnfugdT,dlnfugdP,dlnfugdn,&
       gflag_opt,v_out)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    use volume_shift, only: volumeShiftFugacity, NOSHIFT, volumeShiftVolume
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         TP_lnfug_apparent, base_eos_param
    ! Input.
    integer, intent(in) :: nc
    type(gendata_pointer), dimension(:), intent(in) :: comp
    class (base_eos_param), intent(inout) :: cbeos
    integer, intent(in) :: phase
    real, intent(in) :: T                               !< Temperature [K]
    real, intent(in) :: P                               !< Pressure [Pa]
    real, intent(in) :: n(nc)                           !< Mole numbers [mols]
    integer, optional, intent(in) :: gflag_opt
    real, optional, intent(out) :: v_out !< Specific volume [mol/m3]
    ! Output
    real, intent(out) :: lnfug(nc)
    real, optional, intent(out) :: dlnfugdt(nc), dlnfugdp(nc), dlnfugdn(nc,nc)
    ! Locals.
    real :: V     !< Volume [m^3].
    real :: sumne  !< Total mole number in mixture [mol]
    real :: zFac
    real :: F_n(nce),F_Tn(nce),F_TV,F_VV,F_Vn(nce),F_nn(nce,nce)
    real :: dPdV, dPdT, dPdn(nce)
    real :: dVdn(nce)
    real :: ne(nce), lnfug_real(nce), ze(nce)
    real :: dlnfugdt_real(nce), dlnfugdp_real(nce), dlnfugdn_real(nce,nce)
    integer :: i,j,VSHIFTID

    call apparent_to_real_mole_numbers(n,ne)
    sumne = sum(ne)

    VSHIFTID = cbeos%volumeShiftId
    cbeos%volumeShiftId = NOSHIFT ! Disable volume shift
    ze = ne/sumne
    zFac = TP_CalcZfac(nce,comp,cbeos,T,P,ze,phase,gflag_opt)
    V = zFac*sumne*Rgas*T/P
    if (present(v_out)) then
      v_out = v
      call volumeShiftVolume(nce,comp,VSHIFTID,T,ze,v_out)
    endif

    if (present(dlnfugdt) .or. present(dlnfugdp) .or. present(dlnfugdn)) then
      call TV_CalcFres(nc=nce,comp=comp,cbeos=cbeos,T=T,V=V,n=ne,F_n=F_n,&
           F_VV=F_VV,F_Vn=F_Vn,F_TV=F_TV,F_Tn=F_Tn,F_nn=F_nn)
      dPdV = -Rgas*T*(F_VV + sumne/V**2)
      dPdn = Rgas*T*(-F_Vn + 1/V)
      dVdn = -dPdn/dPdV
    else
      call TV_CalcFres(nc=nce,comp=comp,cbeos=cbeos,T=T,V=V,n=ne,F_n=F_n)
    end if

    lnfug_real = F_n - log(zFac)

    if (present(dlnfugdt)) then
      dPdT = P/T-Rgas*T*F_TV
      dlnfugdt_real = F_Tn + (1 - dVdn*dPdT/Rgas)/T
    endif

    if (present(dlnfugdp)) then
      dlnfugdp_real = dVdn/(Rgas*T)-1/P
    endif

    if (present(dlnfugdn)) then
      do i=1,nce
        do j=1,nce
          dlnfugdn_real(i,j) = F_nn(i,j) + 1/sumne - dVdn(j)*dPdn(i)/(Rgas*T)
        end do
      end do
    endif

    call TP_lnfug_apparent(nc,ne,n,P,lnfug_real,lnfug,dlnfugdt_real,&
       dlnfugdp_real,dlnfugdn_real,dlnfugdT,dlnfugdP,dlnfugdn)

    cbeos%volumeShiftId = VSHIFTID ! Reset volume shift flag
    if (cbeos%volumeShiftId /= NOSHIFT) then
      call volumeShiftFugacity(nc,comp,cbeos%volumeShiftId,T,P,n,phase,&
           lnfug,dlnfugdt,dlnfugdp,dlnfugdn)
    endif

  end subroutine TP_lnfug

  !-----------------------------------------------------------------------------
  !> Calculate residual entropy given pressure, temperature and composition
  !!
  !! \author Ailo A, 2015-04
  !! \author Morten Hammer, 2017-02
  !-----------------------------------------------------------------------------
  subroutine TP_ResidEntropy(nc,comp,cbeos,phase,T,P,n,S,dSdt,dSdp,dSdn,gflag_opt)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    use volume_shift, only: NOSHIFT
    use thermopack_var, only: base_eos_param
    integer, intent(in) :: nc !< Number of components in mixture.
    type(gendata_pointer), intent(in) :: comp(nc) !< Component vector.
    class(base_eos_param), intent(inout) :: cbeos !< Cubic eos for
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(in) :: T !< Temperature [K]
    integer, intent(in) :: phase !< Phase identifier [-]
    real, dimension(nc), intent(in) :: n !< Composition [mol]
    real, intent(out) :: S !< Entropy [J/mol/K]
    real, optional, intent(out) :: dSdt,dSdp
    real, dimension(nc), optional, intent(out) :: dSdn
    integer, optional, intent(in) :: gflag_opt
    ! Locals
    real :: V
    real :: dPdV, dPdT, dVdT, dPdn(nc), dVdn(nc)
    real :: sumn
    real :: zFac
    real :: F,F_T,F_TT,F_n(nc),F_Tn(nc),F_TV,F_VV,F_Vn(nc)
    integer :: VSHIFTID
    sumn = sum(n)

    VSHIFTID = cbeos%volumeShiftId
    cbeos%volumeShiftId = NOSHIFT ! Disable volume shift
    zFac = TP_CalcZfac(nc,comp,cbeos,T,P,n/sumn,phase,gflag_opt)
    V = zFac*sumn*Rgas*T/P

    if (present(dSdt) .or. present(dSdp) .or. present(dSdn)) then
      call TV_CalcFres(nc=nc,comp=comp,cbeos=cbeos,T=T,V=V,n=n,F=F,F_n=F_n,F_T=F_T, &
           F_VV=F_VV,F_TV=F_TV,F_TT=F_TT,F_Vn=F_Vn,F_Tn=F_Tn)
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
      dPdT = P/T-Rgas*T*F_TV
      dVdT = -dPdT/dPdV
    else
      call TV_CalcFres(nc=nc,comp=comp,cbeos=cbeos,T=T,V=V,n=n,F=F,F_T=F_T)
    end if

    S = Rgas*(-F - T*F_T + sumn*log(zFac))

    if (present(dSdt)) then
      dSdt = dVdt*dPdt - Rgas*(2*F_T + T*F_TT + sumn/T)
    endif

    if (present(dSdp) ) then
      dSdp = sumn*Rgas/P - dVdT
    end if

    if (present(dSdn)) then
      dPdn = Rgas*T*(-F_Vn + 1/V)
      dVdn = -dPdn/dPdV
      dSdn = dVdn*dPdt - Rgas*(F_n + T*F_Tn + 1 - log(zFac))
    endif

    cbeos%volumeShiftId = VSHIFTID ! Reset volume shift flag
  end subroutine TP_ResidEntropy

  !-----------------------------------------------------------------------------
  !> Calculate residual enthalpy given pressure, temperature and composition.
  !!
  !! \author Ailo A, 2015-04
  !! \author Morten Hammer, 2017-02
  !-----------------------------------------------------------------------------
  subroutine TP_ResidEnthalpy(nc,comp,cbeos,phase,T,P,n,H,dHdT,dHdP,dHdn,gflag_opt)
    use compdata, only: gendata_pointer
    use thermopack_constants, only: Rgas
    use volume_shift, only: NOSHIFT
    use thermopack_var, only: base_eos_param
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    class(base_eos_param), intent(inout) :: cbeos !< Cubic eos
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(in) :: T !< Temperature [K]
    integer, intent(in) :: phase !< Phase identifier [-]
    real, dimension(nc), intent(in) :: n !< Composition [mol]
    real, intent(out) :: H !< Enthalpy [J/mol/K]
    real, optional, intent(out) :: dHdt, dHdp
    real, optional, intent(out) :: dHdn(nc)
    integer, optional, intent(in) :: gflag_opt
    ! Locals
    real :: V, Zfac
    real :: dPdV, dPdT, dVdT
    real, dimension(nc) ::  dPdn, dVdn
    real :: F_T,F_VV,F_TV,F_TT,F_Vn(nc),F_Tn(nc)
    real :: sumn
    integer :: VSHIFTID
    sumn = sum(n)

    VSHIFTID = cbeos%volumeShiftId
    cbeos%volumeShiftId = NOSHIFT ! Disable volume shift
    zFac = TP_CalcZfac(nc,comp,cbeos,T,P,n/sumn,phase,gflag_opt)
    V = zFac*sumn*Rgas*T/P

    if (present(dHdt) .or. present(dHdp) .or. present(dHdn)) then
      call TV_CalcFres(nc=nc,comp=comp,cbeos=cbeos,T=T,V=V,n=n,F_T=F_T,&
           F_VV=F_VV,F_TV=F_TV,F_TT=F_TT,F_Vn=F_Vn,F_Tn=F_Tn)
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
      dPdT = P/T-Rgas*T*F_TV
      dVdT = -dPdT/dPdV
    else
      call TV_CalcFres(nc=nc,comp=comp,cbeos=cbeos,T=T,V=V,n=n,F_T=F_T)
    end if

    H = -Rgas*T*T*F_T + P*V - sumn*Rgas*T

    if (present(dHdt)) then
      dHdt = T*(dVdT*dPdT - Rgas*(2*F_T + T*F_TT + sumn/T))
    endif

    if (present(dHdp)) then
      dHdp = V-T*dVdt
    endif

    if (present(dHdn)) then
      dPdn = Rgas*T*(-F_Vn + 1/V)
      dVdn = -dPdn/dPdV
      dHdn = T*(dVdn*dPdt - Rgas*(F_Tn*T + 1))
    endif

    cbeos%volumeShiftId = VSHIFTID ! Reset volume shift flag
  end subroutine TP_ResidEnthalpy

  !---------------------------------------------------------------------- >
  !> This function calculates the Compressibility factor with derivatives.
  !!
  !! \param T The temperature [K]
  !! \param P The pressure [Pa]
  !! \param Z The mole fraction [-]
  !! \param phase The phase, 1=liquid, 2=vapour
  !! \param gflag_opt
  !!       1: Normal
  !!       2: if the metastable maxima or minima of the z-factor are to be returned
  !!       3: If possibilities of having three roots, return the one having minimum
  !!        Gibbs free energy is to be returned - calls cbGres
  !! \param dZdt Temperature derivative [1/K]
  !! \param dZdp Pressure derivative [1/Pa]
  !! \param dZdz Composition derivative [-]
  !!
  !! \retval Zfac The Z-factor [-]
  !!
  !! \author Oivind W
  !! \author Morten Hammer
  function fork_Zfac_calculation(nc,comp,cbeos,T,P,n,phase,gflag_opt,&
       dZdt,dZdp,dZdz) result (Zfac)
    use cubic, only: cbCalcZfac
    use LeeKesler, only: lkCalcZfac
    use extcsp, only: csp_zfac, extcsp_eos
    use saft_interface, only: saft_zfac
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: volumeShiftZfac, NOSHIFT
    use single_component, only: Zfac_single
    use multiparameter_idealmix, only: calc_multiparameter_idealmix_zfac
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, base_eos_param
    use eos_parameters, only: single_eos, meos_mix
    !use pets, only: PETS_eos
    use cubic_eos, only: cb_eos, cpa_eos, lk_eos
    use saftvrmie_containers, only: saftvrmie_eos
    use pc_saft_nonassoc, only: PCSAFT_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, P
    integer, intent(in) :: phase
    integer, optional, intent(in) :: gflag_opt
    real, optional, intent(out) :: dZdt, dZdp
    real, optional, dimension(nc), intent(out) :: dZdz
    real :: Zfac
    ! Locals
    integer :: gflag_opt_local
    logical :: is_apparent_mode
    real :: ne(nce), factor
    !
    ! If values are not present, then assign deafult values
    gflag_opt_local = 1
    if (present(gflag_opt)) then
      gflag_opt_local = gflag_opt
    end if

    call apparent_to_real_mole_numbers(n,ne)
    is_apparent_mode = (nc /= nce)
    if (is_apparent_mode) then
      factor = sum(ne)/sum(n)
      if (present(dZdz)) then
        call stoperror("fork_Zfac_calculation: dZdn not yet implemented for apparent mode")
      endif
    endif

    select type ( p_eos => cbeos )
    type is ( cb_eos ) ! cubic equations of state
      call cbCalcZfac(nce,p_eos,T,p,ne,phase,Zfac,gflag_opt_local,dZdt,dZdp,dZdz)
    type is ( single_eos )
      call Zfac_single(nc,p_eos,T,p,ne,phase,Zfac,dZdt,dZdp,dZdz)
    type is ( extcsp_eos ) ! Corresponding State Principle
      call csp_zfac(p_eos,T,P,ne,phase,zfac,dZdt,dZdp,dZdz)
    type is ( lk_eos ) ! Lee-Kesler eos
      call lkCalcZfac(nce,comp,p_eos,T,p,ne,phase,Zfac,dZdt,dZdp,dZdz)
    type is ( meos_mix )
      call calc_multiparameter_idealmix_zfac(nc, p_eos, T, p, ne, phase, &
           Zfac, dZdt, dZdp, dZdz)
    class default ! Saft eos
      call saft_zfac(nce,cbeos,phase,T,P,ne,Z=zfac,dZdT=dZdt,dZdP=dZdp,dZdn=dZdz)
    end select

    if (is_apparent_mode) then
      ! Convert from real to apparant
      Zfac = Zfac*factor
      if (present(dZdT)) then
        dZdT = dZdT*factor
      endif
      if (present(dZdP)) then
        dZdP = dZdP*factor
      endif
    endif

    ! Apply volume shift
    if (cbeos%volumeShiftId /= NOSHIFT) then
      call volumeShiftZfac(nc,comp,cbeos%volumeShiftId,T,P,n,phase,Zfac,dZdt,dZdp,dZdz)
    endif

  end function fork_Zfac_Calculation

  !---------------------------------------------------------------------- >
  !> This function calculates the Entropy and the derivatives.
  !!
  !! \param T The temperature [K]
  !! \param V The pressure [m3/mol]
  !! \param Z The overall mole numbers [-]
  !! \param residual Return only the residual part if .true.
  !! \param dsdt Temperature derivative [J/mol/K]
  !! \param dsdv Volume derivative [J/mol/m3]
  !! \param dsdz Composition derivative [J/mol^2]
  !!
  !! \retval entropy The entropy [J/mol K]
  !!
  !! \author M. Hammer
  function TV_CalcEntropy(nc,comp,cbeos,T,V,n,residual,&
       dsdt,dsdv,dsdn) result (s)
    use cubic
    use ideal
    use stringmod, only: str_eq
    use LeeKesler, only: lkCalcEntropy
    use eosdata
    use compdata, only: gendata_pointer
    use volume_shift, only: NOSHIFT
    use thermopack_constants, only: Rgas
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff, base_eos_param
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(:) :: comp
    class(base_eos_param), intent(inout) :: cbeos
    real, dimension(nc), intent(in) :: n
    real, intent(in) :: T, V
    logical, intent(in) :: residual
    real, optional, intent(out) :: dsdt, dsdv
    real, optional, dimension(nc), intent(out) :: dsdn
    real :: s
    ! Locals
    real :: S_ideal_mix, dsdt_ideal_mix, dsdv_ideal_mix
    real :: F, F_n(nc), F_T, F_TV, F_TT, F_Tn(nc), F_V
    real :: vshift
    real, dimension(nc) :: dsdn_ideal_mix
    real, dimension(nce) :: ne, dsdne, F_ne(nce), F_Tne(nce)

    call apparent_to_real_mole_numbers(n,ne)

    if (cbeos%volumeShiftId /= NOSHIFT) then
      call stoperror("TV_CalcEntropy volume shift not supported for this function")
    endif

    ! Ideal gas entropy
    if (residual) then
      S_ideal_mix = 0.0
      dsdt_ideal_mix=0.0
      dsdv_ideal_mix=0.0
      dsdn_ideal_mix=0.0
    else
      vshift = v
      call TV_Sideal_mix(nce, comp, T, vshift, ne, s_ideal_mix, &
           dsdt_ideal_mix=dsdt_ideal_mix, dsdv_ideal_mix=dsdv_ideal_mix, &
           dsdz_ideal_mix=dsdne)
      if(present(dsdn)) then ! Composition derivative,
        call real_to_apparent_diff(dsdne,dsdn)
        dsdn_ideal_mix=dsdn
      end if
    endif

    if (present(dSdt) .or. present(dSdv) .or. present(dSdn)) then
      call TV_CalcFres(nc=nce,comp=comp,cbeos=cbeos,T=T,V=V,n=ne,F=F,F_n=F_ne,&
           F_T=F_T,F_V=F_V,F_TV=F_TV,F_TT=F_TT,F_Tn=F_Tne)
    else
      call TV_CalcFres(nc=nce,comp=comp,cbeos=cbeos,T=T,V=V,n=ne,F=F,F_T=F_T)
    end if
    s = -Rgas*(F + T*F_T)

    ! Add ideal gas contributions
    s = s + s_ideal_mix
    if(present(dsdt)) then ! Temperature derivative
      dsdt = -Rgas*(2.0*F_T + T*F_TT)
      dsdt = dsdt + dsdt_ideal_mix
    end if
    if(present(dsdv)) then ! Volume derivative
      dsdv = -Rgas*(F_V + T*F_TV)
      dsdv = dsdv + dsdv_ideal_mix
    end if
    if(present(dsdn)) then ! Composition derivative,
      call real_to_apparent_diff(F_ne,F_n)
      call real_to_apparent_diff(F_Tne,F_Tn)
      dsdn = -Rgas*(F_n + T*F_Tn)
      dsdn = dsdn + dsdn_ideal_mix
    end if
  end function TV_CalcEntropy

  !> Calculate resudial reduced Helmholtz and differentials
  !!
  !! \param T - Temprature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  subroutine TV_CalcFid(nc,comp,cbeos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use eosdata
    use compdata, only: gendata_pointer
    use single_component, only: Fid_single
    use ideal, only: Fideal_mix_SI
    use thermopack_var, only: base_eos_param
    use eos_parameters, only: single_eos
    implicit none
    integer, intent(in) :: nc
    type (gendata_pointer), intent(in), dimension(nc) :: comp
    real, intent(in) :: T,V,n(nc)
    ! Output.
    class(base_eos_param), intent(inout) :: cbeos
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    !
    !---------------------------------------------------------------------
    !-------- Specific for each equation of state ------------------------
    select type ( p_seos => cbeos )
    type is ( single_eos )
      call Fid_single(nc,comp,p_seos,T,v,n,F=F,F_T=F_T,F_V=F_V,F_n=F_n,&
           F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tn,F_Vn=F_Vn,F_nn=F_nn)
    class default
      call Fideal_mix_SI(nc, comp, T, v, n, Fid=F, Fid_T=F_T, Fid_v=F_V, &
           Fid_n=F_n, Fid_TT=F_TT, Fid_vv=F_vv, Fid_nn=F_nn, Fid_Tv=F_TV,&
           Fid_vn=F_Vn,Fid_Tn=F_Tn)
    end select
  end subroutine TV_CalcFid

end module single_phase
