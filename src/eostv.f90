!> Interface to thermodynamic models.
!! Using Helmholtz formulation in temperature and volume
!!
!! \author MH, 2015-02
module eosTV
  use thermopack_var, only: nc, nce, get_active_eos, base_eos_param, &
       thermo_model, get_active_thermo_model
  use thermopack_constants
  !
  implicit none
  save
  !
  ! Include TREND interface
  include 'trend_interface.f95'
  !
  private
  public :: pressure, internal_energy, free_energy, entropyTV
  public :: thermoTV, Fres, Fideal, chemical_potential
  public :: virial_coefficients, secondvirialcoeffmatrix
  public :: binaryThirdVirialCoeffMatrix
  public :: enthalpyTV
  !
contains

  !----------------------------------------------------------------------
  !> Calculate pressure given composition, temperature and density.
  !>
  !> \author MH, 2012-03-14
  !----------------------------------------------------------------------
  function pressure(t,v,n,dpdv,dpdt,d2pdv2,dpdn,recalculate) result(p)
    use single_phase, only: TV_CalcPressure
    use cbHelm, only: cbPvv, cbPi
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, optional, intent(out) :: dpdt !< Pa/K - Pressure differential wrpt. temperature
    real, optional, intent(out) :: dpdv !< Pa/m3 - Pressure differential wrpt. specific volume
    real, optional, intent(out) :: d2pdv2 !< Pa/m6 - Second pressure differential wrpt. specific volume
    real, optional, dimension(nc), intent(out) :: dpdn !< Pa/mol - Second pressure differential wrpt. specific mole numbers
    logical, optional, intent(in) :: recalculate !< flag for Thermopack: if true, recalculate cbeos-structure
    real :: p !< Pa - Pressure
    ! Locals
    real :: d2Pdrho2, rho, dPdrho
    !integer :: VSHIFTID
    logical :: recalculate_loc
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    end if
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
      call TV_CalcPressure(nc,act_mod_ptr%comps,act_eos_ptr,T,v,n,p,&
           dpdv, dpdt, d2pdv2, dpdn, recalculate=recalculate_loc)
    case (TREND)
      ! TREND
      p = trend_pressure(n,t,v,dpdv,dpdt,dpdn)
      if (present(d2pdv2)) then
        rho = 1.0/v
        d2Pdrho2 = trend_d2Pdrho2(n,T,rho)
        if (present(dpdv)) then
          dPdrho = -dPdv*v**2
        else
          dPdrho = trend_dpdrho(n,T,rho)
        endif
        d2pdv2 = rho**4*d2Pdrho2 + 2.0*rho**3*dPdrho
      endif
    case default
      write(*,*) 'EoSlib error in eos::pressure: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end function pressure

  !----------------------------------------------------------------------
  !> Calculate internal energy given composition, temperature and density.
  !>
  !> \author MH, 2012-03-14
  !----------------------------------------------------------------------
  subroutine internal_energy(t,v,n,u,dudt,dudv,recalculate)
    use single_phase, only: TV_CalcInnerEnergy
    use eosdata
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Specific volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: u !< J - Specific internal energy
    real, optional, intent(out) :: dudt !< J/K - Energy differential wrpt. temperature
    real, optional, intent(out) :: dudv !< J/m3 - Energy differential wrpt. specific volume
    logical, optional, intent(in) :: recalculate !< Recalculate cbeos-structure
    ! Locals
    logical :: recalculate_loc
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    end if

    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
      u = TV_CalcInnerEnergy(nc,act_mod_ptr%comps,act_eos_ptr,T,v,n,dudt,dudv,&
           recalculate=recalculate_loc)
    case (TREND)
      ! TREND
      u = trend_internal_energy(n,t,v,dudv,dudt)
    case default
      write(*,*) 'EoSlib error in eos::internal_energy: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine internal_energy

  !----------------------------------------------------------------------
  !> Calculate Helmholtz free energy given composition, temperature and density.
  !>
  !> \author GL, 2015-01-23
  !----------------------------------------------------------------------
  subroutine free_energy(t,v,n,y,dydt,dydv,d2ydt2,d2ydv2,d2ydvdt,recalculate)
    use single_phase, only: TV_CalcFreeEnergy
    !$ use omp_lib
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: y !< J - Free energy
    real, optional, intent(out) :: dydt !< J/K - Differential wrt. temperature
    real, optional, intent(out) :: dydv !< J/m3 - Differential wrt. specific volume
    real, optional, intent(out) :: d2ydt2 !< J/K2 - Helmholtz differential wrpt. temperature
    real, optional, intent(out) :: d2ydv2 !< J/m6 - Helmholtz second differential wrpt. specific volume
    real, optional, intent(out) :: d2ydvdt !< J/(m3 K) - Helmholtz second differential wrpt. specific volume and temperature
    logical, optional, intent(in) :: recalculate !< Recalculate cbeos-structure
    ! Locals
    logical :: recalculate_loc
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !--------------------------------------------------------------------
    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    end if
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
      y = TV_CalcFreeEnergy(nc,act_mod_ptr%comps,act_eos_ptr,T,v,n,&
           dydt,dydv,recalculate=recalculate_loc)
      if (present(d2ydt2) .or. present(d2ydv2) .or. present(d2ydvdt)) then
        write(*,*) 'eos::free_energy: Differentials not implemented'
        call stoperror('')
      endif
    case (TREND)
      ! TREND
      y = trend_free_energy(n,t,v,dydv,dydt,d2ydt2,d2ydv2,d2ydvdt)
    case default
      write(*,*) 'EoSlib error in eos::internal_energy: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine free_energy

  !----------------------------------------------------------------------
  !> Calculate entropy given composition, temperature and density.
  !>
  !> \author MH, 2015-02
  !----------------------------------------------------------------------
  subroutine entropyTV(t,v,n,s,dsdt,dsdv,dsdn,residual)
    use thermopack_constants, only: Rgas
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: s !< J/K - Entropy
    real, optional, intent(out) :: dsdt !< J/K2 - Entropy differential wrpt. temperature
    real, optional, intent(out) :: dsdv !< J/K/m3 - Entropy differential wrpt. specific volume
    real, optional, intent(out) :: dsdn(nc) !< J/K/mol - Entropy differential wrpt. mol numbers
    logical, optional, intent(in) :: residual
    ! Locals
    real :: s_id, dsdt_id, dsdv_id, F, F_T, dsdn_id(nc)
    logical :: res
    real, pointer :: F_TT_p, F_V_p, F_TV_p
    real, target :: F_TT, F_V, F_TV
    real, pointer :: F_Tn_p(:), F_n_p(:)
    real, target :: F_Tn(nc), F_n(nc)

    !--------------------------------------------------------------------
    if (present(residual)) then
      res = residual
    else
      res = .false.
    endif

    if (present(dsdT)) then
      F_TT_p => F_TT
    else
      F_TT_p => NULL()
    endif
    if (present(dsdV)) then
      F_V_p => F_V
      F_TV_p => F_TV
    else
      F_V_p => NULL()
      F_TV_p => NULL()
    endif
    if (present(dsdn)) then
      F_n_p => F_n
      F_Tn_p => F_Tn
    else
      F_n_p => NULL()
      F_Tn_p => NULL()
    endif

    ! Residual contribution
    call Fres(T,v,n,F=F,F_T=F_T,F_V=F_V_p,F_n=F_n_p,F_TT=F_TT_p,F_TV=F_TV_p,F_Tn=F_Tn_p)
    s = - Rgas*(F + T*F_T)
    if (present(dsdt)) then
      dsdt = - Rgas*(2*F_T + T*F_TT)
    endif
    if (present(dsdv)) then
      dsdv = - Rgas*(F_V + T*F_TV)
    endif
    if(present(dsdn)) then
      dsdn = -Rgas*(F_n + T*F_Tn)
    end if
    if (.not. res) then
      call Fideal(T,v,n,F,F_T,F_V=F_V_p,F_n=F_n_p,F_TT=F_TT_p,F_TV=F_TV_p,F_Tn=F_Tn_p)
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
        dsdn_id = -Rgas*(F_n + T*F_Tn)
        dsdn = dsdn + dsdn_id
      end if
    endif
  end subroutine entropyTV

  !----------------------------------------------------------------------
  !> Calculate enthalpy given composition, temperature and density.
  !>
  !> \author MH, 2019-06
  !----------------------------------------------------------------------
  subroutine enthalpyTV(t,v,n,h,dhdt,dhdv,dhdn,residual)
    use thermopack_constants, only: Rgas
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: h !< J - Enthalpy
    real, optional, intent(out) :: dhdt !< J/K - Enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdv !< J/m3 - Enthalpy differential wrpt. volume
    real, optional, intent(out) :: dhdn(nc) !< J/m3 - Enthalpy differential wrpt. mol numbers
    logical, optional, intent(in) :: residual
    ! Locals
    real :: h_id, dhdt_id, dhdv_id, F, F_T, F_V, h_T, h_id_T, dhdn_id(nc)
    logical :: res
    real, pointer :: F_TT_p, F_TV_p, F_VV_p
    real, target :: F_TT, F_TV, F_VV
    real, pointer :: F_Tn_p(:), F_Vn_p(:)
    real, target :: F_Tn(nc), F_Vn(nc)
    !--------------------------------------------------------------------
    if (present(residual)) then
      res = residual
    else
      res = .false.
    endif

    if (present(dhdT)) then
      F_TT_p => F_TT
    else
      F_TT_p => NULL()
    endif
    if (present(dhdV)) then
      F_VV_p => F_VV
      F_TV_p => F_TV
    else
      F_VV_p => NULL()
      F_TV_p => NULL()
    endif
    if (present(dhdn)) then
      F_Vn_p => F_Vn
      F_Tn_p => F_Tn
    else
      F_Vn_p => NULL()
      F_Tn_p => NULL()
    endif

    ! Residual contribution
    call Fres(T,v,n,F=F,F_T=F_T,F_V=F_V,F_TT=F_TT_p,&
         F_TV=F_TV_p,F_VV=F_VV_p,F_Tn=F_Tn_p,F_Vn=F_Vn_p)
    h_T = -Rgas*(T*F_T + v*F_V)
    h = h_T*T
    if (present(dhdt)) then
      dhdt = h_T - Rgas*T*(F_T + T*F_TT + v*F_TV)
    endif
    if (present(dhdv)) then
      dhdv = - Rgas*T*(T*F_TV + F_V + v*F_VV)
    endif
    if (present(dhdn)) then
      dhdn = - Rgas*T*(T*F_Tn + v*F_Vn)
    endif
    if (.not. res) then
      call Fideal(T,v,n,F,F_T,F_V=F_V,F_TT=F_TT_p,&
           F_TV=F_TV_p,F_VV=F_VV_p,F_Tn=F_Tn_p,F_Vn=F_Vn_p)
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
        dhdn_id = - Rgas*T*(T*F_Tn + v*F_Vn)
        dhdn = dhdn + dhdn_id
      endif
    endif
  end subroutine enthalpyTV

  !----------------------------------------------------------------------
  !> Calculate fugacity and differentials given composition,
  !> temperature and specific volume
  !>
  !> \author MH, 2015-10
  !----------------------------------------------------------------------
  subroutine thermoTV(t,v,n,lnphi,lnphit,lnphiv,lnphin)
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
    case (TREND)
      ! TREND
      call trend_thermoTV(T,v,n,lnphi,lnphiT,lnphiV,lnphin)
    case default
      write(*,*) 'EoSlib error in eosTV::thermo: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine thermoTV

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
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !
    !--------------------------------------------------------------------
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
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
      call TV_CalcFres(nce,act_mod_ptr%comps,act_eos_ptr,&
           T,V,ne,F=F,F_T=F_T,F_V=F_V,F_n=F_ne_p,&
           F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tne_p,F_Vn=F_Vne_p,F_nn=F_nene_p,&
           F_VVV=F_VVV,recalculate=recalculate)
      call real_to_apparent_differentials(F_ne,F_Tne,F_Vne,F_nene,&
           F_n,F_Tn,F_Vn,F_nn)
    case (TREND)
      ! TREND
      call trend_calcFres(T,v,n,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    case default
      write(*,*) 'EoSlib error in eosTV::Fres: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine Fres

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
    class(base_eos_param), pointer :: act_eos_ptr
    type(thermo_model), pointer :: act_mod_ptr
    !
    !--------------------------------------------------------------------
    act_mod_ptr => get_active_thermo_model()
    select case (act_mod_ptr%EoSlib)
    case (THERMOPACK)
      ! Thermopack
      act_eos_ptr => get_active_eos()
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
      call TV_CalcFid(nce,act_mod_ptr%comps,act_eos_ptr,T,V,ne,F=F,F_T=F_T,F_V=F_V,F_n=F_ne,&
           F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tne_p,F_Vn=F_Vne_p,F_nn=F_nene_p)
      call real_to_apparent_differentials(F_ne,F_Tne,F_Vne,F_nene,&
           F_n,F_Tn,F_Vn,F_nn)
    case (TREND)
      ! TREND
      call trend_CalcFid(T,V,n,F,F_T,F_V,F_TT,F_TV,F_VV,F_n,F_Tn,F_Vn,F_nn)
    case default
      write(*,*) 'EoSlib error in eosTV::Fideal: No such EoS libray:',act_mod_ptr%EoSlib
      call stoperror('')
    end select
  end subroutine Fideal

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
  subroutine chemical_potential(t, v, n, mu, dmudv, dmudt, dmudn)
    use thermopack_constants, only: rgas
    implicit none
    real,                             intent(in)  :: t !< K - Temperature
    real,                             intent(in)  :: v !< m3 - Molar volume
    real, dimension(nc),              intent(in)  :: n !< mol - Mol numbers
    real, dimension(nc),              intent(out) :: mu !< J/mol
    real, dimension(nc),    optional, intent(out) :: dmudv !< J/m^3
    real, dimension(nc),    optional, intent(out) :: dmudt !< J/mol K
    real, dimension(nc,nc), optional, intent(out) :: dmudn !< J/mol^2
    ! Locals
    real :: Fr_n(nc), Fi_n(nc)
    real, pointer :: Fr_Tn_p(:), Fr_vn_p(:), Fr_nn_p(:,:)
    real, target :: Fr_Tn(nc), Fr_vn(nc), Fr_nn(nc,nc)
    real, pointer :: Fi_Tn_p(:), Fi_vn_p(:), Fi_nn_p(:,:)
    real, target :: Fi_Tn(nc), Fi_vn(nc), Fi_nn(nc,nc)
    !
    if (present(dmudt)) then
      Fr_Tn_p => Fr_Tn
      Fi_Tn_p => Fi_Tn
    else
      Fr_Tn_p => NULL()
      Fi_Tn_p => NULL()
    endif
    if (present(dmudV)) then
      Fr_Vn_p => Fr_Vn
      Fi_Vn_p => Fi_Vn
    else
      Fr_Vn_p => NULL()
      Fi_Vn_p => NULL()
    endif
    if (present(dmudn)) then
      Fr_nn_p => Fr_nn
      Fi_nn_p => Fi_nn
    else
      Fr_nn_p => NULL()
      Fi_nn_p => NULL()
    endif

    call Fres(t, v, n, f_n=Fr_n, F_Tn=Fr_Tn_p, F_Vn=Fr_Vn_p, F_nn=Fr_nn_p)
    call Fideal(t, v, n, f_n=Fi_n, F_Tn=Fi_Tn_p, F_Vn=Fi_Vn_p, F_nn=Fi_nn_p)
    mu = rgas*t*(Fr_n + Fi_n)
    !
    if (present(dmudt)) then
      dmudt = rgas*(Fr_n + Fi_n + t*(Fr_Tn + Fi_Tn))
    end if
    if (present(dmudv)) then
      dmudv = rgas*t*(Fr_Vn + Fi_Vn)
    end if
    if (present(dmudn)) then
      dmudn = rgas*t*(Fr_nn +  Fi_nn)
    end if
    !
  end subroutine chemical_potential

end module eosTV
