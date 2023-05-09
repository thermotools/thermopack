!> Interface to thermodynamic models.
!! Using Helmholtz formulation in temperature and volume
!!
!! \author MH, 2015-02
module eosTV
  use thermopack_var, only: nc, nce, get_active_eos, base_eos_param, &
       thermo_model, get_active_thermo_model, apparent_to_real_mole_numbers, &
       real_to_apparent_diff, Rgas
  use thermopack_constants
  !
  implicit none
  save
  !
  private
  public :: pressure, internal_energy_tv, free_energy_tv, entropy_tv
  public :: thermo_tv, Fres, Fideal, chemical_potential_tv
  public :: virial_coefficients, secondvirialcoeffmatrix
  public :: binaryThirdVirialCoeffMatrix
  public :: enthalpy_tv, Fres_ne, Fideal_ne
  !
  public :: enthalpy_tvp, entropy_tvp, thermo_tvp
contains

  !----------------------------------------------------------------------
  !> Calculate pressure given composition, temperature and density.
  !>
  !> \author MH, 2012-03-14
  !----------------------------------------------------------------------
  function pressure(t,v,n,dpdv,dpdt,d2pdv2,dpdn,contribution) result(p)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, optional, intent(out) :: dpdt !< Pa/K - Pressure differential wrpt. temperature
    real, optional, intent(out) :: dpdv !< Pa/m3 - Pressure differential wrpt. specific volume
    real, optional, intent(out) :: d2pdv2 !< Pa/m6 - Second pressure differential wrpt. specific volume
    real, optional, dimension(nc), intent(out) :: dpdn !< Pa/mol - Second pressure differential wrpt. specific mole numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    real :: p !< Pa - Pressure
    ! Locals
    real :: sumne
    real :: F_V
    logical :: res, ideal
    real, dimension(nce) :: ne
    real, pointer :: F_Vne_p(:)
    real, target :: F_Vne(nce)
    !--------------------------------------------------------------------
    if (present(dpdn)) then
      F_Vne_p => F_Vne
    else
      F_Vne_p => NULL()
    endif
    call apparent_to_real_mole_numbers(n,ne)
    sumne = sum(ne)

    res = .true.
    ideal = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    if (res) then
      call Fres_ne(T,V,ne,F_V=F_V,F_TV=dpdt,F_VV=dpdv,F_Vn=F_Vne_p,F_VVV=d2pdv2)
      P = -Rgas*T*F_V
      if (present(dPdV)) dPdV = -Rgas*T*dPdV
      if (present(dPdT)) dPdT = -Rgas*T*dPdT
      if (present(dPdn)) F_Vne = -Rgas*T*F_Vne
      if (present(d2PdV2)) d2PdV2 = -Rgas*T*d2PdV2
    else
      P = 0
      if (present(dPdV)) dPdV = 0
      if (present(dPdT)) dPdT = 0
      if (present(dPdn)) F_Vne = 0
      if (present(d2PdV2)) d2PdV2 = 0
    endif
    if (ideal) then
      P = P + sumne*Rgas*T/V
      if (present(dPdV)) dPdV = dPdV - sumne*Rgas*T/V**2
      if (present(dPdT)) dPdT = dPdT + P/T
      if (present(dPdn)) F_Vne = F_Vne + Rgas*T/V
      if (present(d2PdV2)) d2PdV2 = d2PdV2 + 2.0*sumne*Rgas*T/V**3
    endif

    if (present(dPdn)) then
      call real_to_apparent_diff(F_Vne,dPdn)
    endif
  end function pressure

  !----------------------------------------------------------------------
  !> Calculate internal energy given composition, temperature and density.
  !>
  !> \author MH, 2012-03-14
  !----------------------------------------------------------------------
  subroutine internal_energy_tv(t,v,n,u,dudt,dudv,dudn,contribution)
    use eosdata
    use thermopack_constants, only: PROP_RESIDUAL, PROP_IDEAL
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Specific volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: u !< J - Specific internal energy
    real, optional, intent(out) :: dudt !< J/K - Energy differential wrpt. temperature
    real, optional, intent(out) :: dudv !< J/m3 - Energy differential wrpt. volume
    real, optional, intent(out) :: dudn(nc) !< J/mol - Energy differential wrpt. mol numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    !--------------------------------------------------------------------
    real :: F_T, u_id, F_Tn(nc)
    real, pointer :: F_TT_p, F_TV_p, F_Tn_p(:)
    real, target :: F_TT_l, F_TV_l, F_Tn_l(nce)
    real, dimension(nce) :: ne
    logical :: res, ideal
    call apparent_to_real_mole_numbers(n,ne)

    res = .true.
    ideal = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    if (present(dudt)) then
      dudt = 0
      F_TT_p => F_TT_l
    else
      F_TT_p => NULL()
    endif
    if (present(dudv)) then
      dudv = 0
      F_TV_p => F_TV_l
    else
      F_TV_p => NULL()
    endif
    if (present(dudn)) then
      dudn = 0
      F_Tn_p => F_Tn_l
    else
      F_Tn_p => NULL()
    endif
    u = 0

    if (res) then
      call Fres_ne(T,V,ne,F_T=F_T,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_Tn=F_Tn_p)
      u = (-Rgas)*T**2*F_T
      if (present(dudv)) dudv = (-Rgas)*T**2*F_TV_l
      if (present(dudt)) dudt = (-Rgas)*T*(T*F_TT_l+2.0*F_T)
      if (present(dudn)) then
        call real_to_apparent_diff(F_Tn_l,F_Tn)
        dudn = (-Rgas)*T**2*F_Tn
      endif
    endif
    if (ideal) then
      ! Add ideal gas contributions
      call Fideal_ne(T,V,ne,F_T=F_T,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_Tn=F_Tn_p)
      u_id = (-Rgas)*T**2*F_T
      u = u + u_id
      if (present(dudv)) dudv = dudv + (-Rgas)*T**2*F_TV_l
      if (present(dudt)) dudt = dudt + (-Rgas)*T*(T*F_TT_l+2.0*F_T)
      if (present(dudn)) then
        call real_to_apparent_diff(F_Tn_l,F_Tn)
        dudn = dudn + (-Rgas)*T**2*F_Tn
      endif
    endif
  end subroutine internal_energy_tv

  !----------------------------------------------------------------------
  !> Calculate Helmholtz free energy given composition, temperature and density.
  !>
  !> \author GL, 2015-01-23
  !----------------------------------------------------------------------
  subroutine free_energy_tv(t,v,n,y,dydt,dydv,dydn,contribution)
    use thermopack_constants, only: PROP_RESIDUAL, PROP_IDEAL
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: y !< J - Free energy
    real, optional, intent(out) :: dydt !< J/K - Differential wrt. temperature
    real, optional, intent(out) :: dydv !< J/m3 - Differential wrt. specific volume
    real, optional, intent(out) :: dydn(nc) !< J/mol - Helmholtz differential wrpt. mol numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    logical :: res, ideal
    real :: F, F_n(nc)
    real, pointer :: F_T_p, F_V_p, F_n_p(:)
    real, target :: F_T_l, F_V_l, F_n_l(nce)
    real, dimension(nce) :: ne
    !--------------------------------------------------------------------
    call apparent_to_real_mole_numbers(n,ne)

    res = .true.
    ideal = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    if (present(dYdT)) then
      dYdT = 0
      F_T_p => F_T_l
    else
      F_T_p => NULL()
    endif
    if (present(dYdV)) then
      dYdV = 0
      F_V_p => F_V_l
    else
      F_V_p => NULL()
    endif
    if (present(dYdn)) then
      dYdn = 0
      F_n_p => F_n_l
    else
      F_n_p => NULL()
    endif
    y = 0

    if (res) then
      call Fres_ne(T,V,ne,F=F,F_T=F_T_p,F_V=F_V_p,F_n=F_n_p)
      Y = Rgas*T*F
      if (present(dYdt)) dYdt = Rgas*(F + T*F_T_l)
      if (present(dYdv)) dYdv = Rgas*T*F_V_l
      if (present(dYdn)) then
        call real_to_apparent_diff(F_n_l,F_n)
        dYdn = Rgas*T*F_n
      endif
    endif
    if (ideal) then
      ! Add ideal gas contributions
      call Fideal_ne(T,V,ne,F=F,F_T=F_T_p,F_V=F_V_p,F_n=F_n_p)
      Y = Y + Rgas*T*F
      if (present(dYdt)) dYdt = dYdt + Rgas*(F + T*F_T_l)
      if (present(dYdv)) dYdv = dYdv + Rgas*T*F_V_l
      if (present(dYdn)) then
        call real_to_apparent_diff(F_n_l,F_n)
        dYdn = dYdn + Rgas*T*F_n
      endif
    endif
  end subroutine free_energy_tv

  !----------------------------------------------------------------------
  !> Calculate entropy given composition, temperature and density.
  !>
  !> \author MH, 2015-02
  !----------------------------------------------------------------------
  subroutine entropy_tv(t,v,n,s,dsdt,dsdv,dsdn,contribution)
    use thermopack_constants, only: PROP_RESIDUAL, PROP_IDEAL
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: s !< J/K - Entropy
    real, optional, intent(out) :: dsdt !< J/K2 - Entropy differential wrpt. temperature
    real, optional, intent(out) :: dsdv !< J/K/m3 - Entropy differential wrpt. specific volume
    real, optional, intent(out) :: dsdn(nc) !< J/K/mol - Entropy differential wrpt. mol numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    real :: s_id, dsdt_id, dsdv_id, F, F_T, dsdne_id(nce), dsdne(nce)
    real :: ne(nce)
    logical :: res, ideal
    real, pointer :: F_TT_p, F_V_p, F_TV_p
    real, target :: F_TT, F_V, F_TV
    real, pointer :: F_Tne_p(:), F_ne_p(:)
    real, target :: F_Tne(nce), F_ne(nce)
    !--------------------------------------------------------------------
    call apparent_to_real_mole_numbers(n,ne)
    res = .true.
    ideal = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    if (present(dsdT)) then
      dsdT = 0
      F_TT_p => F_TT
    else
      F_TT_p => NULL()
    endif
    if (present(dsdV)) then
      dsdV = 0
      F_V_p => F_V
      F_TV_p => F_TV
    else
      F_V_p => NULL()
      F_TV_p => NULL()
    endif
    if (present(dsdn)) then
      dsdne = 0
      F_ne_p => F_ne
      F_Tne_p => F_Tne
    else
      F_ne_p => NULL()
      F_Tne_p => NULL()
    endif
    s = 0

    if (res) then
      ! Residual contribution
      call Fres_ne(T,v,ne,F=F,F_T=F_T,F_V=F_V_p,F_n=F_ne_p,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_Tn=F_Tne_p)
      s = - Rgas*(F + T*F_T)
      if (present(dsdt)) then
        dsdt = - Rgas*(2*F_T + T*F_TT)
      endif
      if (present(dsdv)) then
        dsdv = - Rgas*(F_V + T*F_TV)
      endif
      if(present(dsdn)) then
        dsdne = -Rgas*(F_ne + T*F_Tne)
      end if
    endif

    if (ideal) then
      call Fideal_ne(T,v,ne,F,F_T,F_V=F_V_p,F_n=F_ne_p,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_Tn=F_Tne_p)
      s_id = - Rgas*(F + T*F_T)
      s = s + s_id
      if (present(dsdt)) then
        dsdt_id = - Rgas*(2*F_T + T*F_TT)
        dsdt = dsdt + dsdt_id
      endif
      if (present(dsdv)) then
        dsdv_id = - Rgas*(F_V + T*F_TV)
        dsdv = dsdv + dsdv_id
      endif
      if(present(dsdn)) then
        dsdne_id = -Rgas*(F_ne + T*F_Tne)
        dsdne = dsdne + dsdne_id
      end if
    endif
    if (present(dSdn)) then
      call real_to_apparent_diff(dSdne,dSdn)
    endif
  end subroutine entropy_tv

  !----------------------------------------------------------------------
  !> Calculate enthalpy given composition, temperature and density.
  !>
  !> \author MH, 2019-06
  !----------------------------------------------------------------------
  subroutine enthalpy_tv(t,v,n,h,dhdt,dhdv,dhdn,contribution)
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: h !< J - Enthalpy
    real, optional, intent(out) :: dhdt !< J/K - Enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdv !< J/m3 - Enthalpy differential wrpt. volume
    real, optional, intent(out) :: dhdn(nc) !< J/m3 - Enthalpy differential wrpt. mol numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    real :: h_id, dhdt_id, dhdv_id, F, F_T, F_V, h_T, h_id_T, dhdne_id(nc), dhdne(nce)
    real :: ne(nce)
    logical :: res, ideal
    real, pointer :: F_TT_p, F_TV_p, F_VV_p
    real, target :: F_TT, F_TV, F_VV
    real, pointer :: F_Tne_p(:), F_Vne_p(:)
    real, target :: F_Tne(nce), F_Vne(nce)
    !--------------------------------------------------------------------
    call apparent_to_real_mole_numbers(n,ne)
    res = .true.
    ideal = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    if (present(dhdT)) then
      dhdT = 0
      F_TT_p => F_TT
    else
      F_TT_p => NULL()
    endif
    if (present(dhdV)) then
      dhdV = 0
      F_VV_p => F_VV
      F_TV_p => F_TV
    else
      F_VV_p => NULL()
      F_TV_p => NULL()
    endif
    if (present(dhdn)) then
      dhdne = 0
      F_Vne_p => F_Vne
      F_Tne_p => F_Tne
    else
      F_Vne_p => NULL()
      F_Tne_p => NULL()
    endif
    h = 0

    if (res) then
      ! Residual contribution
      call Fres_ne(T,v,ne,F=F,F_T=F_T,F_V=F_V,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_VV=F_VV_p,F_Tn=F_Tne_p,F_Vn=F_Vne_p)
      h_T = -Rgas*(T*F_T + v*F_V)
      h = h_T*T
      if (present(dhdt)) then
        dhdt = h_T - Rgas*T*(F_T + T*F_TT + v*F_TV)
      endif
      if (present(dhdv)) then
        dhdv = - Rgas*T*(T*F_TV + F_V + v*F_VV)
      endif
      if (present(dhdn)) then
        dhdne = - Rgas*T*(T*F_Tne + v*F_Vne)
      endif
    endif
    if (ideal) then
      call Fideal_ne(T,v,ne,F,F_T,F_V=F_V,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_VV=F_VV_p,F_Tn=F_Tne_p,F_Vn=F_Vne_p)
      h_id_T = -Rgas*(T*F_T + v*F_V)
      h_id = h_id_T*T
      h = h + h_id
      if (present(dhdt)) then
        dhdt_id = h_id_T - Rgas*T*(F_T + T*F_TT + v*F_TV)
        dhdt = dhdt + dhdt_id
      endif
      if (present(dhdv)) then
        dhdv_id = - Rgas*T*(T*F_TV + F_V + v*F_VV)
        dhdv = dhdv + dhdv_id
      endif
      if (present(dhdn)) then
        dhdne_id = - Rgas*T*(T*F_Tne + v*F_Vne)
        dhdne = dhdne + dhdne_id
      endif
    endif
    if (present(dhdn)) then
      call real_to_apparent_diff(dhdne,dhdn)
    endif
  end subroutine enthalpy_tv

  !----------------------------------------------------------------------
  !> Calculate fugacity and differentials given composition,
  !> temperature and specific volume
  !>
  !> \author MH, 2015-10
  !----------------------------------------------------------------------
  subroutine thermo_tv(t,v,n,lnphi,lnphit,lnphiv,lnphin)
    use single_phase, only: TV_CalcFugacity
    !$ use omp_lib
    implicit none
    ! Transferred variables
    real, intent(in)                                  :: t !< K - Temperature
    real, intent(in)                                  :: v !< m3 - Volume
    real, dimension(1:nc), intent(in)                 :: n !< Mole numbers [mol]
    real, dimension(1:nc), intent(out)                :: lnphi !< Logarithm of fugasity
    real, optional, dimension(1:nc), intent(out)      :: lnphit !< 1/K - Logarithm of fugasity differential wrpt. temperature
    real, optional, dimension(1:nc), intent(out)      :: lnphiv !< mol/m3 - Logarithm of fugasity differential wrpt. volume
    real, optional, dimension(1:nc,1:nc), intent(out) :: lnphin !< Logarithm of fugasity differential wrpt. mole numbers
    ! Locals
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
      call TV_CalcFugacity(nc,act_mod_ptr%comps,act_eos_ptr,T,v,n,lnphi,&
           lnphiT,lnphiV,lnphin)
    case default
      write(*,*) 'EoSlib error in eosTV::thermo: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine thermo_tv

  !----------------------------------------------------------------------
  !> Calculate residual reduced Helmholtz energy
  !>
  !> \author MH, 2012-01-27
  !----------------------------------------------------------------------
  subroutine Fres(T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn,F_VVV,recalculate)
    use single_phase, only: TV_CalcFres
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, real_to_apparent_differentials
    implicit none
    ! Transferred variables
    real, intent(in)                                  :: t !< K - Temperature
    real, intent(in)                                  :: v !< m3 - Volume
    real, dimension(1:nc), intent(in)                 :: n !< mol numbers
    real, optional, intent(out)                       :: F,F_T,F_V,F_TT,F_TV,F_VV,F_VVV !<
    real, optional, dimension(1:nc), intent(out)      :: F_n,F_Tn,F_Vn !<
    real, optional, dimension(1:nc,1:nc), intent(out) :: F_nn !<
    logical, optional, intent(in)                     :: recalculate
    ! Locals
    real, dimension(nce) :: ne
    real, target, dimension(nce) :: F_ne, F_Tne, F_Vne
    real, target, dimension(nce,nce) :: F_nene
    real, pointer :: F_ne_p(:), F_Tne_p(:), F_Vne_p(:)
    real, pointer :: F_nene_p(:,:)
    !
    !--------------------------------------------------------------------
    call apparent_to_real_mole_numbers(n,ne)
    if (present(F_n)) then
      F_ne_p => F_ne
    else
      F_ne_p => NULL()
    endif
    if (present(F_Vn)) then
      F_Vne_p => F_Vne
    else
      F_Vne_p => NULL()
    endif
    if (present(F_Tn)) then
      F_Tne_p => F_Tne
    else
      F_Tne_p => NULL()
    endif
    if (present(F_nn)) then
      F_nene_p => F_nene
    else
      F_nene_p => NULL()
    endif
    call Fres_ne(T,V,ne,F,F_T=F_T,F_V=F_V,F_n=F_ne_p,F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,&
         F_Tn=F_Tne_p,F_Vn=F_Vne_p,F_nn=F_nene_p,F_VVV=F_VVV,recalculate=recalculate)
    call real_to_apparent_differentials(F_ne,F_Tne,F_Vne,F_nene,&
           F_n,F_Tn,F_Vn,F_nn)
  end subroutine Fres

  !----------------------------------------------------------------------
  !> Calculate residual reduced Helmholtz energy for real (not apparent) mol numbers
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine Fres_ne(T,V,ne,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn,F_VVV,recalculate)
    use single_phase, only: TV_CalcFres
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, real_to_apparent_differentials
    implicit none
    ! Transferred variables
    real, intent(in)                                    :: t !< K - Temperature
    real, intent(in)                                    :: v !< m3 - Volume
    real, dimension(1:nce), intent(in)                  :: ne !< mol numbers
    real, optional, intent(out)                         :: F,F_T,F_V,F_TT,F_TV,F_VV,F_VVV !<
    real, optional, dimension(1:nce), intent(out)       :: F_n,F_Tn,F_Vn !<
    real, optional, dimension(1:nce,1:nce), intent(out) :: F_nn !<
    logical, optional, intent(in)                       :: recalculate
    ! Locals
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !
    !--------------------------------------------------------------------
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
      call TV_CalcFres(nce,act_mod_ptr%comps,act_eos_ptr,&
           T,V,ne,F=F,F_T=F_T,F_V=F_V,F_n=F_n,&
           F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tn,F_Vn=F_Vn,F_nn=F_nn,&
           F_VVV=F_VVV,recalculate=recalculate)
    case default
      write(*,*) 'EoSlib error in eosTV::Fres: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine Fres_ne

  !----------------------------------------------------------------------
  !> Calculate ideal reduced Helmholtz energy
  !>
  !> \author MH, 2012-01-27
  !----------------------------------------------------------------------
  subroutine Fideal(T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use single_phase, only: TV_CalcFid
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, real_to_apparent_differentials
    implicit none
    ! Transferred variables
    real, intent(in)                                  :: t !< K - Temperature
    real, intent(in)                                  :: v !< m3/mol - Volume
    real, dimension(1:nc), intent(in)                 :: n !< Compozition
    real, optional, intent(out)                       :: F,F_T,F_V,F_TT,F_TV,F_VV !<
    real, optional, dimension(1:nc), intent(out)      :: F_n,F_Tn,F_Vn !<
    real, optional, dimension(1:nc,1:nc), intent(out) :: F_nn !<
    ! Locals
    real, dimension(nce) :: ne
    real, target, dimension(nce) :: F_ne, F_Tne, F_Vne
    real, target, dimension(nce,nce) :: F_nene
    real, pointer :: F_ne_p(:), F_Tne_p(:), F_Vne_p(:)
    real, pointer :: F_nene_p(:,:)
    !
    !--------------------------------------------------------------------
    call apparent_to_real_mole_numbers(n,ne)
    if (present(F_n)) then
      F_ne_p => F_ne
    else
      F_ne_p => NULL()
    endif
    if (present(F_Vn)) then
      F_Vne_p => F_Vne
    else
      F_Vne_p => NULL()
    endif
    if (present(F_Tn)) then
      F_Tne_p => F_Tne
    else
      F_Tne_p => NULL()
    endif
    if (present(F_nn)) then
      F_nene_p => F_nene
    else
      F_nene_p => NULL()
    endif
    call Fideal_ne(T,V,ne,F=F,F_T=F_T,F_V=F_V,F_n=F_ne,&
         F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tne_p,F_Vn=F_Vne_p,F_nn=F_nene_p)
    call real_to_apparent_differentials(F_ne,F_Tne,F_Vne,F_nene,&
         F_n,F_Tn,F_Vn,F_nn)
  end subroutine Fideal

  !----------------------------------------------------------------------
  !> Calculate ideal reduced Helmholtz energy for real (not apparent) components
  !>
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine Fideal_ne(T,V,ne,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use single_phase, only: TV_CalcFid
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, real_to_apparent_differentials
    implicit none
    ! Transferred variables
    real, intent(in)                                    :: t !< K - Temperature
    real, intent(in)                                    :: v !< m3/mol - Volume
    real, dimension(1:nce), intent(in)                  :: ne !< Compozition
    real, optional, intent(out)                         :: F,F_T,F_V,F_TT,F_TV,F_VV !<
    real, optional, dimension(1:nce), intent(out)       :: F_n,F_Tn,F_Vn !<
    real, optional, dimension(1:nce,1:nce), intent(out) :: F_nn !<
    ! Locals
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !
    !--------------------------------------------------------------------
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
      call TV_CalcFid(nce,act_mod_ptr%comps,act_eos_ptr,T,V,ne,F=F,F_T=F_T,F_V=F_V,F_n=F_n,&
           F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tn,F_Vn=F_Vn,F_nn=F_nn)
    case default
      write(*,*) 'EoSlib error in eosTV::Fideal: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine Fideal_ne

  !----------------------------------------------------------------------
  !> Calculate (composition-dependent) virial coefficients B and C,
  !> defined as P/RT = rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.
  !----------------------------------------------------------------------
  subroutine virial_coefficients(T,n,B,C)
    use single_phase, only: TV_CalcFres
    implicit none
    ! Transferred variables
    real, intent(in)                      :: t !< Temperature [K]
    real, dimension(1:nc), intent(in)     :: n !< Composition [-]
    real, intent(out)                     :: B !< Second virial coefficients [m3/mol]
    real, intent(out)                     :: C !< Third virial coefficient [m6/mol2]
    ! Locals
    real :: V, F_V, F_VV, z(nc)
    !
    !-------------------------------------------------------------------
    V = 1.0e7 ! might need tuning
    z = n/sum(n)
    call Fres(T,V,Z,F_V=F_V,F_VV=F_VV)
    B = -V**2*F_V
    C = V**4*(F_VV + 2.0*F_V/V)
  end subroutine virial_coefficients

  !----------------------------------------------------------------------
  !> Calculate composition-independent virial coefficients B,
  !! defined as P = RT*rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.
  !! Including cross coefficients.
  !----------------------------------------------------------------------
  subroutine secondVirialCoeffMatrix(T,Bmat)
    implicit none
    ! Transferred variables
    real, intent(in)                      :: t !< Temperature [K]
    real, intent(out)                     :: Bmat(nc,nc) !< Second virial coefficients [m3/mol]
    ! Locals
    real :: Bii, Bij, C, z(nc), x
    integer :: i, j
    !
    !-------------------------------------------------------------------

    ! Pure virial coefficients
    do i=1,nc
      z = 0
      z(i) = 1.0
      call virial_coefficients(T,z,Bii,C)
      Bmat(i,i) = Bii
    end do

    ! Cross virial coefficients
    x = 0.5 ! The answer should be independent of the value of x
    do i=1,nc
      do j=i+1,nc
        z = 0
        z(i) = x
        z(j) = 1-x
        call virial_coefficients(T,z,Bij,C)
        Bmat(i,j) = (Bij - Bmat(i,i)*x**2 - Bmat(j,j)*(1-x)**2)/(2*x*(1-x))
        Bmat(j,i) = Bmat(i,j)
      end do
    end do
  end subroutine secondVirialCoeffMatrix

  !----------------------------------------------------------------------
  !> Calculate composition-independent virial coefficients C,
  !! defined as P = RT*rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.
  !! Including cross coefficients
  !! Currently the code only support binary mixtures
  !----------------------------------------------------------------------
  subroutine binaryThirdVirialCoeffMatrix(T,Cmat)
    implicit none
    ! Transferred variables
    real, intent(in)                      :: t !< Temperature [K]
    real, intent(out)                     :: Cmat(nc,nc) !< Third virial coefficients [m6/mol2]
    ! Locals
    real :: Ciii, Cij1, Cij2, B, z(nc), x
    real :: A(2,2), D(2)
    integer :: i, j
    integer :: ifail, ipiv(2)
    logical :: lufail
    !
    !-------------------------------------------------------------------
    if (nc /= 2) then
      call stoperror("binaryThirdVirialCoeffMatrix only intended binary systems")
    endif
    ! Pure virial coefficients
    do i=1,nc
      z = 0
      z(i) = 1.0
      call virial_coefficients(T,z,B,Ciii)
      Cmat(i,i) = Ciii
    end do

    ! Cross virial coefficients for binaries
    x = 0.75
    do i=1,nc
      do j=i+1,nc
        z = 0
        z(i) = x
        z(j) = 1-x
        call virial_coefficients(T,z,B,Cij1)
        A(1,1) = 3.0*z(i)**2*z(j)
        A(1,2) = 3.0*z(j)**2*z(i)
        D(1) = -Cij1 + Cmat(i,i)*z(i)**3 + Cmat(j,j)*z(j)**3
        z(j) = x
        z(i) = 1-x
        call virial_coefficients(T,z,B,Cij2)
        A(2,1) = 3.0*z(i)**2*z(j)
        A(2,2) = 3.0*(1-x)*z(j)**2*z(i)
        D(2) = -Cij2 + Cmat(i,i)*z(i)**3 + Cmat(j,j)*z(j)**3

        ! Solve linear system
        lufail = .false.
        call dgetrf(2,2,A,2,ipiv,ifail)
        if (ifail /= 0) lufail = .true.
        if (.not. lufail) then
          call dgetrs('n',2,1,A,2,ipiv,D,2,ifail)
          if (ifail /= 0) lufail = .true.
        endif
        Cmat(i,j) = D(1)
        Cmat(j,i) = D(2)
      end do
    end do

  end subroutine binaryThirdVirialCoeffMatrix

  !> Calculate chemical potential and derivatives
  !>
  !> \author MAG, 2018-10-31
  !----------------------------------------------------------------------
  subroutine chemical_potential_tv(t, v, n, mu, dmudt, dmudv, dmudn, contribution)
    use thermopack_var, only: real_to_apparent_differentials
    implicit none
    real,                             intent(in)  :: t !< K - Temperature
    real,                             intent(in)  :: v !< m3 - Molar volume
    real, dimension(nc),              intent(in)  :: n !< mol - Mol numbers
    real, dimension(nc),              intent(out) :: mu !< J/mol
    real, dimension(nc),    optional, intent(out) :: dmudv !< J/m^3
    real, dimension(nc),    optional, intent(out) :: dmudt !< J/mol K
    real, dimension(nc,nc), optional, intent(out) :: dmudn !< J/mol^2
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    logical :: res, ideal
    real :: Fr_ne(nce), Fi_ne(nce), mue(nce), dmudt_e(nce), dmudv_e(nce)
    real :: ne(nce), dmudne_e(nce,nce)
    real, pointer :: Fr_Tne_p(:), Fr_vne_p(:), Fr_nene_p(:,:)
    real, target :: Fr_Tne(nce), Fr_vne(nce), Fr_nene(nc,nce)
    real, pointer :: Fi_Tne_p(:), Fi_vne_p(:), Fi_nene_p(:,:)
    real, target :: Fi_Tne(nce), Fi_vne(nce), Fi_nene(nce,nce)
    !
    call apparent_to_real_mole_numbers(n,ne)
    res = .true.
    ideal = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    if (present(dmudt)) then
      dmudt_e = 0
      Fr_Tne_p => Fr_Tne
      Fi_Tne_p => Fi_Tne
    else
      Fr_Tne_p => NULL()
      Fi_Tne_p => NULL()
    endif
    if (present(dmudV)) then
      dmudV_e = 0
      Fr_Vne_p => Fr_Vne
      Fi_Vne_p => Fi_Vne
    else
      Fr_Vne_p => NULL()
      Fi_Vne_p => NULL()
    endif
    if (present(dmudn)) then
      dmudne_e = 0
      Fr_nene_p => Fr_nene
      Fi_nene_p => Fi_nene
    else
      Fr_nene_p => NULL()
      Fi_nene_p => NULL()
    endif
    mue = 0

    if (res) then
      call Fres_ne(t, v, ne, f_n=Fr_ne, F_Tn=Fr_Tne_p, F_Vn=Fr_Vne_p, F_nn=Fr_nene_p)
      mue = rgas*t*Fr_ne
      !
      if (present(dmudt)) then
        dmudt_e = rgas*(Fr_ne + t*Fr_Tne)
      end if
      if (present(dmudv)) then
        dmudv_e = rgas*t*Fr_Vne
      end if
      if (present(dmudn)) then
        dmudne_e = rgas*t*Fr_nene
      end if
    endif
    if (ideal) then
      call Fideal_ne(t, v, ne, f_n=Fi_ne, F_Tn=Fi_Tne_p, F_Vn=Fi_Vne_p, F_nn=Fi_nene_p)
      mue = mue + rgas*t*Fi_ne
      !
      if (present(dmudt)) then
        dmudt_e = dmudt_e + rgas*(Fi_ne + t*Fi_Tne)
      end if
      if (present(dmudv)) then
        dmudv_e = dmudv_e + rgas*t*Fi_Vne
      end if
      if (present(dmudn)) then
        dmudne_e = dmudne_e + rgas*t*Fi_nene
      end if
    endif
    !
    call real_to_apparent_differentials(mue,dmudt_e,dmudv_e,dmudne_e,&
         mu,dmudt,dmudv,dmudn)
  end subroutine chemical_potential_tv

  !-----------------------------------------------------------------------------
  !> Calculate the logarithmic fugacity coefficient and its differentials.
  !! Evaluate using (T,v,n) but output differentials for  (T, P, n)
  !!
  !! \author Morten Hammer, 2022-02
  !-----------------------------------------------------------------------------
  subroutine thermo_tvp(T,v,n,lnfug,dlnfugdT,dlnfugdP,dlnfugdn)
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         TP_lnfug_apparent, base_eos_param
    ! Input.
    real, intent(in) :: T                               !< Temperature [K]
    real, intent(in) :: v                               !< Volume [m3]
    real, intent(in) :: n(nc)                           !< Mole numbers [mols]
    ! Output
    real, intent(out) :: lnfug(nc)
    real, optional, intent(out) :: dlnfugdt(nc), dlnfugdp(nc), dlnfugdn(nc,nc)
    ! Locals.
    real :: P     !< Pressure [Pa].
    real :: sumne  !< Total mole number in mixture [mol]
    real :: zFac
    real :: dPdV, dPdT, dPdn(nce)
    real :: dVdn(nce), F_n(nce), F_V
    real :: ne(nce), lnfug_real(nce)
    real :: dlnfugdt_real(nce), dlnfugdp_real(nce), dlnfugdn_real(nce,nce)
    integer :: i,j
    logical :: differentials
    real, target :: F_Tn(nce),F_TV,F_VV,F_Vn(nce),F_nn(nce,nce)
    real, pointer :: F_Tn_p(:),F_TV_p,F_VV_p,F_Vn_p(:),F_nn_p(:,:)

    call apparent_to_real_mole_numbers(n,ne)
    sumne = sum(ne)

    F_TV_p => NULL()
    F_VV_p => NULL()
    F_nn_p => NULL()
    F_Tn_p => NULL()
    F_Vn_p => NULL()
    differentials = .false.
    if (present(dlnfugdt) .or. present(dlnfugdp) .or. present(dlnfugdn)) then
      differentials = .true.
      F_Vn_p => F_Vn
      F_VV_p => F_VV
      if (present(dlnfugdt)) then
        F_Tn_p => F_Tn
        F_TV_p => F_TV
      endif
      if (present(dlnfugdn)) then
        F_nn_p => F_nn
      endif
    endif

    call Fres_ne(T=T,V=V,ne=ne,F_n=F_n,F_V=F_V,&
         F_VV=F_VV,F_Vn=F_Vn,F_TV=F_TV,F_Tn=F_Tn,F_nn=F_nn)
    P = -Rgas*T*F_V + sumne*Rgas*T/V
    zFac = V*P/(sumne*Rgas*T)
    if (differentials) then
      dPdV = -Rgas*T*(F_VV + sumne/V**2)
      dPdn = Rgas*T*(-F_Vn + 1/V)
      dVdn = -dPdn/dPdV
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

  end subroutine thermo_tvp

  !----------------------------------------------------------------------
  !> Calculate enthalpy given composition, temperature and density.
  !> Differentials at constant pressure
  !> \author MH, 2019-06
  !----------------------------------------------------------------------
  subroutine enthalpy_tvp(t,v,n,h,dhdt,dhdp,dhdn,contribution)
    use thermopack_constants, only: PROP_RESIDUAL, PROP_IDEAL
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: h !< J - Enthalpy
    real, optional, intent(out) :: dhdt !< J/K - Enthalpy differential wrpt. temperature  (const pressure)
    real, optional, intent(out) :: dhdp !< J/Pa - Enthalpy differential wrpt. pressure
    real, optional, intent(out) :: dhdn(nc) !< J/m3 - Enthalpy differential wrpt. mol numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    real :: F, F_T, F_V, h_T
    real :: sumne, ne(nce), P, dPdV, dPdT, dVdT, dPdn(nce), dVdn(nce), dhdne(nce)
    logical :: res, ideal_gas, differentials
    real, pointer :: F_TT_p, F_TV_p, F_VV_p
    real, target :: F_TT, F_TV, F_VV
    real, pointer :: F_Tn_p(:), F_Vn_p(:)
    real, target :: F_Tn(nce), F_Vn(nce)
    !--------------------------------------------------------------------
    res = .true.
    ideal_gas = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal_gas = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    F_TV_p => NULL()
    F_VV_p => NULL()
    F_TT_p => NULL()
    F_Tn_p => NULL()
    F_Vn_p => NULL()
    differentials = .false.
    h = 0
    if (present(dhdT) .or. present(dhdp) .or. present(dhdn)) then
      differentials = .true.
      F_TV_p => F_TV
      F_VV_p => F_VV
      if (present(dhdT)) then
        F_TT_p => F_TT
        dHdt = 0
      endif
      if (present(dhdP)) then
        dHdp = 0
      endif
      if (present(dhdn)) then
        F_Tn_p => F_Tn
        F_Vn_p => F_Vn
        dHdne = 0
      endif
    endif

    ! Get real mol numbers
    call apparent_to_real_mole_numbers(n,ne)
    sumne = sum(ne)

    if (res) then
      ! Residual contribution
      call Fres_ne(T,v,ne,F=F,F_T=F_T,F_V=F_V,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_VV=F_VV_p,F_Tn=F_Tn_p,F_Vn=F_Vn_p)
      h_T = -Rgas*(T*F_T + v*F_V)
      h = h_T*T
      if (differentials) then
        P = -Rgas*T*F_V + sumne*Rgas*T/V
        dPdV = -Rgas*T*(F_VV + sumne/V**2)
        dPdT = P/T-Rgas*T*F_TV
        dVdT = -dPdT/dPdV
        if (present(dHdt)) then
          dHdt = T*(dVdT*dPdT - Rgas*(2*F_T + T*F_TT + sumne/T))
        endif
        if (present(dHdp)) then
          dHdp = V-T*dVdt
        endif
        if (present(dHdn)) then
          dPdn = Rgas*T*(-F_Vn + 1/V)
          dVdn = -dPdn/dPdV
          dHdne = T*(dVdn*dPdt - Rgas*(F_Tn*T + 1))
        endif
      endif
    endif

    if (ideal_gas) then
      ! Ideal contribution
      call Fideal_ne(T,V,ne,F=F,F_T=F_T,F_TT=F_TT_p,F_Tn=F_Tn_p)
      h = h + Rgas*T*(-T*F_T + sumne)
      if (present(dHdt)) then
        dHdt = dHdt - Rgas*T*(2*F_T + T*F_TT) + Rgas*sumne
      endif
      if (present(dHdn)) then
        dHdne = dhdne - Rgas*T**2*F_Tn + Rgas*T
      endif
    endif

    if (present(dHdn)) then
      call real_to_apparent_diff(dhdne,dhdn)
    endif
  end subroutine enthalpy_tvp

  !----------------------------------------------------------------------
  !> Calculate entropy given composition, temperature and density.
  !> Differentials at constant pressure
  !> \author MH, 2022-02
  !----------------------------------------------------------------------
  subroutine entropy_tvp(t,v,n,s,dsdt,dsdp,dsdn,contribution)
    use thermopack_constants, only: PROP_RESIDUAL, PROP_IDEAL
    use thermopack_var, only: nce, apparent_to_real_mole_numbers, &
         real_to_apparent_diff
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: s !< J/K - Entropy
    real, optional, intent(out) :: dsdt !< J/K2 - Entropy differential wrpt. temperature (const pressure)
    real, optional, intent(out) :: dsdp !< J/K/Pa - Entropy differential wrpt. specific pressure
    real, optional, intent(out) :: dsdn(nc) !< J/K/mol - Entropy differential wrpt. mol numbers
    integer, optional, intent(in) :: contribution !< Contribution from ideal (PROP_IDEAL), residual (PROP_RESIDUAL) or both (PROP_OVERALL)
    ! Locals
    real :: F, F_T, F_V, dsdne(nce), ne(nce)
    real :: sumne, P, dPdV, dPdT, dVdT, zFac, dPdn(nce), dVdn(nce)
    logical :: res, ideal_gas, differentials
    real, pointer :: F_TT_p, F_TV_p, F_VV_p
    real, target :: F_TT, F_TV, F_VV
    real, pointer :: F_Tn_p(:), F_n_p(:), F_Vn_p(:)
    real, target :: F_Tn(nce), F_n(nce), F_Vn(nce)
    !--------------------------------------------------------------------
    res = .true.
    ideal_gas = .true.
    if (present(contribution)) then
      if (contribution == PROP_RESIDUAL) then
        ideal_gas = .false.
      else if (contribution == PROP_IDEAL) then
        res = .false.
      endif
    endif

    F_TV_p => NULL()
    F_VV_p => NULL()
    F_TT_p => NULL()
    F_n_p => NULL()
    F_Tn_p => NULL()
    F_Vn_p => NULL()
    differentials = .false.
    s = 0
    if (present(dsdT) .or. present(dsdp) .or. present(dsdn)) then
      differentials = .true.
      F_TV_p => F_TV
      F_VV_p => F_VV
      if (present(dsdT)) then
        F_TT_p => F_TT
        dsdT = 0
      endif
      if (present(dsdP)) then
        dsdP = 0
      endif
      if (present(dsdn)) then
        F_n_p => F_n
        F_Tn_p => F_Tn
        F_Vn_p => F_Vn
        dsdne = 0
      endif
    endif

    call apparent_to_real_mole_numbers(n,ne)

    if (res) then
      ! Residual contribution
      call Fres_ne(T,v,ne,F=F,F_T=F_T,F_V=F_V,F_n=F_n_p,F_VV=F_VV_p,&
           F_TT=F_TT_p,F_TV=F_TV_p,F_Tn=F_Tn_p,F_Vn=F_Vn_p)

      sumne = sum(ne)
      P = -Rgas*T*F_V + sumne*Rgas*T/V
      zFac = V*P/(sumne*Rgas*T)
      s = Rgas*(-F - T*F_T  + sumne*log(zFac))
      if (differentials) then
        dPdV = -Rgas*T*(F_VV + sumne/V**2)
        dPdT = P/T-Rgas*T*F_TV
        dVdT = -dPdT/dPdV

        if (present(dSdt)) then
          dSdt = dVdt*dPdt - Rgas*(2*F_T + T*F_TT + sumne/T)
        endif

        if (present(dSdp) ) then
          dSdp = sumne*Rgas/P - dVdT
        end if

        if (present(dSdn)) then
          dPdn = Rgas*T*(-F_Vn + 1/V)
          dVdn = -dPdn/dPdV
          dSdne = dVdn*dPdt - Rgas*(F_n + T*F_Tn + 1 - log(zFac))
        endif
      endif
    endif

    if (ideal_gas) then
      if (.not. res) then
        call Fres_ne(T,V,ne,F_V=F_V)
        P = -Rgas*T*F_V + sumne*Rgas*T/V
        zFac = V*P/(sumne*Rgas*T)
      endif
      ! Ideal contribution
      call Fideal_ne(T,V,ne,F=F,F_T=F_T,F_n=F_n_p,F_TT=F_TT_p,F_Tn=F_Tn_p)

      s = s - Rgas*(F + T*F_T + sumne*log(zFac))
      if (present(dSdt)) then
        dsdt = dsdt - Rgas*(2*F_T + T*F_TT) + Rgas*sumne/T
      endif
      if (present(dSdp)) then
        dsdp = dsdp - Rgas*sumne/P
      endif
      if (present(dsdn)) then
        dsdne = dsdne - Rgas*(F_n + T*F_Tn - 1 + log(zFac))
      endif
    endif

    if (present(dSdn)) then
      call real_to_apparent_diff(dSdne,dSdn)
    endif
  end subroutine entropy_tvp

end module eosTV
