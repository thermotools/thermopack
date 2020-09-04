!---------------------------------------------------------------------
! Interface module for SAFT-VRQ Mie.
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------

module saftvrmie_interface
  use saftvrmie_containers, only: saftvrmie_zeta, saftvrmie_dhs, &
       saftvrmie_aij, saftvrmie_param, init_saftvrmie_containers, &
       saftvrmie_param_container, saftvrmie_var_container, calc_DFeynHibbsij, &
       saftvrmie_eos
  use thermopack_constants, only: kB_const
  use saftvrmie_options
  implicit none
  private
  save

  public :: init_saftvrmie, calcFresSAFTVRMie, preCalcSAFTVRMie
  public :: calc_saftvrmie_zeta
  public :: deBoerParameter
  public :: calc_saftvrmie_term, calc_alpha_saftvrmie

contains

  !> Initialize the SAFT-VR Mie model
  !! See Lafitte 2013 (doi:10.1063/1.4819786)
  !! for model description
  !!
  !! \author Morten Hammer, February 2018
  subroutine init_saftvrmie(nc,comp,eos,ref,mixing)
    use thermopack_var, only: gendata_pointer, base_eos_param
    use thermopack_constants, only: Rgas, kRgas, N_Avogadro, kB_const
    integer, intent(in)           :: nc          !< Number of components.
    type(gendata_pointer), intent(inout)  :: comp(nc)    !< Component vector.
    class(base_eos_param), intent(inout) :: eos       !< Underlying cubic equation of state.
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    integer, intent(in), optional :: mixing      !< Binary combination rule id
    ! Locals
    !cbeos%name = "SAFT-VR-MIE"
    select type(p_eos => eos)
    type is (saftvrmie_eos)
      call init_saftvrmie_containers(nc,comp,p_eos,ref,mixing)
    end select
    ! Association is added in the saft_interface module

    ! Set consistent Rgas
    Rgas = N_Avogadro*kB_const
    kRgas=1000.0*Rgas !< J/kmol/K
  end subroutine init_saftvrmie

  !> Calculate hypotetical pure fluid packing fraction
  !!
  !! \author Morten Hammer, March 2018
  function calc_saftvrmie_zeta(eos,nc,T,V,n) result(zeta)
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
         calc_binary_effective_sigma
    use saftvrmie_dispersion, only: calcZetaX
    ! Input
    class(saftvrmie_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real :: zeta
    ! Calculate Feynman--Hibbs D parameter
    call calc_DFeynHibbsij(nc,T,eos%saftvrmie_param%DFeynHibbsParam_ij, &
         eos%saftvrmie_var%DFeynHibbsij, EOS%saftvrmie_var%D2FeynHibbsij)
    ! Calculate effective sigma
    call calc_binary_effective_sigma(nc,T,eos%saftvrmie_var,&
         eos%saftvrmie_var%sigma_eff%d,&
         eos%saftvrmie_var%sigma_eff%d_T,eos%saftvrmie_var%sigma_eff%d_TT)
    ! Calculate hard-sphere diameter
    call calc_hardsphere_diameter(nc,T,eos%saftvrmie_var,eos%saftvrmie_var%sigma_eff%d,&
         eos%saftvrmie_var%sigma_eff%d_T,eos%saftvrmie_var%sigma_eff%d_TT,&
         eos%saftvrmie_var%dhs%d,&
         eos%saftvrmie_var%dhs%d_T,eos%saftvrmie_var%dhs%d_TT)

    ! Calculate hypotetical pure fluid packing fraction
    call calcZetaX(nc,T,V,n,0,eos%saftvrmie_var%dhs,eos%saftvrmie_var%zeta)
    zeta = eos%saftvrmie_var%zeta%zx
  end function calc_saftvrmie_zeta

  !> Calculate common variable properties for SAFT-VR Mie
  !!
  !! \author Morten Hammer, February 2018
  subroutine preCalcSAFTVRMie(nc,T,V,n,difflevel,saftvrmie_vc)
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
         calc_binary_effective_sigma, calc_binary_effective_eps_divk, &
         calc_d_pure
    use saftvrmie_dispersion, only: calcZetaX, &
         calcKhsTVn, calcAlpha
    use saftvrmie_options, only: check_model_consitency
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: difflevel !< Level of differentials
    ! Output
    type(saftvrmie_var_container), intent(inout) :: saftvrmie_vc
    !
    ! Check that the spceified model is consistent
    call check_model_consitency()
    !
    ! NB: the order of the function calls below is important
    !
    ! Calculate Feynman--Hibbs D parameter
    call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
         saftvrmie_vc%DFeynHibbsij, saftvrmie_vc%D2FeynHibbsij)
    ! Calculate effective sigma
    call calc_binary_effective_sigma(nc,T,saftvrmie_vc,saftvrmie_vc%sigma_eff%d,&
         saftvrmie_vc%sigma_eff%d_T,saftvrmie_vc%sigma_eff%d_TT)
    ! Calculate effective epsilon divided by k
    call calc_binary_effective_eps_divk(nc,T,saftvrmie_vc,saftvrmie_vc%eps_divk_eff%d,&
         saftvrmie_vc%eps_divk_eff%d_T,saftvrmie_vc%eps_divk_eff%d_TT)
    ! Calculate hard-sphere diameter
    call calc_hardsphere_diameter(nc,T,saftvrmie_vc,saftvrmie_vc%sigma_eff%d,&
         saftvrmie_vc%sigma_eff%d_T,saftvrmie_vc%sigma_eff%d_TT,saftvrmie_vc%dhs%d,&
         saftvrmie_vc%dhs%d_T,saftvrmie_vc%dhs%d_TT)
    ! Calculate dimensionless van der Waals energy
    call calcAlpha(nc,saftvrmie_vc%sigma_eff,saftvrmie_vc%eps_divk_eff,&
         T,saftvrmie_vc,saftvrmie_vc%alpha%d,saftvrmie_vc%alpha%d_T,saftvrmie_vc%alpha%d_TT)

    ! Calculate hypotetical pure fluid packing fraction
    call calcZetaX(nc,T,V,n,difflevel,saftvrmie_vc%dhs,saftvrmie_vc%zeta)
    ! Calculate isothermal hard sphere compressibillity factor
    call calcKhsTVn(nc,T,V,n,difflevel,saftvrmie_vc)
    !> Calculate packing fraction
    call calcZetaX(nc,T,V,n,difflevel,saftvrmie_vc%sigma_eff,saftvrmie_vc%zeta_bar, zetaxbar=.true.)

    if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
       ! Calculate diameter for pure-fluid hard-sphere reference
       call calc_d_pure(nc,n,V,difflevel,saftvrmie_vc%dhs,saftvrmie_vc%d_pure)
    endif

  end subroutine preCalcSAFTVRMie

  !> Calculate residual reduced Helmholts free energy
  !!
  !! \author Morten Hammer, February 2018
  subroutine calcFresSAFTVRMie(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_hardsphere, only: calc_hardsphere_helmholtzenergy, calc_hardsphere_extra_helmholtzenergy
    use saftvrmie_dispersion, only: calcA1, calcA2, calcA3, calc_delta_Ac
    use saftvrmie_chain, only: calcAchain
    ! Input
    class(saftvrmie_eos), intent(inout) :: eos
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    ! Locals
    real :: beta(3),ns
    real :: Fhs,Fhs_T,Fhs_V,Fhs_TT,Fhs_VV,Fhs_TV
    real, dimension(nc) :: Fhs_n,Fhs_Tn,Fhs_Vn
    real, dimension(nc,nc) :: Fhs_nn
    real :: Fhse,Fhse_T,Fhse_V,Fhse_TT,Fhse_VV,Fhse_TV
    real, dimension(nc) :: Fhse_n,Fhse_Tn,Fhse_Vn
    real, dimension(nc,nc) :: Fhse_nn
    real :: F1,F1_T,F1_V,F1_TT,F1_VV,F1_TV
    real, dimension(nc) :: F1_n,F1_Tn,F1_Vn
    real, dimension(nc,nc) :: F1_nn
    real :: F2,F2_T,F2_V,F2_TT,F2_VV,F2_TV
    real, dimension(nc) :: F2_n,F2_Tn,F2_Vn
    real, dimension(nc,nc) :: F2_nn
    real :: F3,F3_T,F3_V,F3_TT,F3_VV,F3_TV
    real, dimension(nc) :: F3_n,F3_Tn,F3_Vn
    real, dimension(nc,nc) :: F3_nn
    real :: Fc,Fc_T,Fc_V,Fc_TT,Fc_VV,Fc_TV
    real, dimension(nc) :: Fc_n,Fc_Tn,Fc_Vn
    real, dimension(nc,nc) :: Fc_nn
    real :: Ftc,Ftc_T,Ftc_V,Ftc_TT,Ftc_VV,Ftc_TV
    real, dimension(nc) :: Ftc_n,Ftc_Tn,Ftc_Vn
    real, dimension(nc,nc) :: Ftc_nn
    real :: am,am_T,am_V
    real, dimension(nc) :: am_n
    integer :: l
    logical, parameter :: returnF = .true.

    if (present(F_TT) .or. present(F_VV) .or. present(F_TV) .or. &
         present(F_Tn) .or. present(F_Vn) .or. present(F_nn)) then
       ! Precalculate common variables
       call preCalcSAFTVRMie(nc,T,V,n,2,eos%saftvrmie_var)
       ! Calculate hard-sphere term
       if (enable_hs) then
          call calc_hardsphere_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,&
               Fhs,a_T=Fhs_T,a_V=Fhs_V,a_n=Fhs_n,&
               a_TT=Fhs_TT,a_VV=Fhs_VV,a_TV=Fhs_TV,a_Tn=Fhs_Tn,a_Vn=Fhs_Vn,a_nn=Fhs_nn)
       endif
       ! Calculate extraterm to hard-sphere reference for non-additive mixtures
       if (enable_hs_extra) then
          call calc_hardsphere_extra_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,&
               Fhse,a_T=Fhse_T,a_V=Fhse_V,a_n=Fhse_n,&
               a_TT=Fhse_TT,a_VV=Fhse_VV,a_TV=Fhse_TV,a_Tn=Fhse_Tn,a_Vn=Fhse_Vn,a_nn=Fhse_nn)
       endif
       ! Calculate first order monomer term
       if (enable_A1) then
          call calcA1(nc,T,V,n,eos%saftvrmie_var,&
               F1,a1_T=F1_T,a1_V=F1_V,a1_n=F1_n,a1_TT=F1_TT,a1_VV=F1_VV,&
               a1_TV=F1_TV,a1_Tn=F1_Tn,a1_Vn=F1_Vn,a1_nn=F1_nn)
       endif
       ! Calculate second order monomer term
       if (enable_A2) then
          call calcA2(nc,T,V,n,eos%saftvrmie_var,&
               F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n,a2_TT=F2_TT,&
               a2_VV=F2_VV,a2_TV=F2_TV,a2_Tn=F2_Tn,a2_Vn=F2_Vn,a2_nn=F2_nn)
       end if
       ! Calculate third order monomer term
       if (enable_A3) then
          call calcA3(nc,T,V,n,eos%saftvrmie_var,&
               F3,a3_T=F3_T,a3_V=F3_V,a3_n=F3_n,a3_TT=F3_TT,&
               a3_VV=F3_VV,a3_TV=F3_TV,a3_Tn=F3_Tn,a3_Vn=F3_Vn,a3_nn=F3_nn)
       endif
       ! Calculate chain term
       if (enable_chain) then
          call calcAchain(nc,T,V,n,eos%saftvrmie_var,&
               Fc,ach_T=Fc_T,ach_V=Fc_V,ach_n=Fc_n,ach_TT=Fc_TT,&
               ach_VV=Fc_VV,ach_TV=Fc_TV,ach_Tn=Fc_Tn,ach_Vn=Fc_Vn,ach_nn=Fc_nn,&
               returnF=returnF)
       endif
    else if (present(F_T) .or. present(F_V) .or. present(F_n)) then
       ! Precalculate common variables
       call preCalcSAFTVRMie(nc,T,V,n,1,eos%saftvrmie_var)
       ! Calculate hard-sphere term
       if (enable_hs) then
          call calc_hardsphere_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,&
               Fhs,a_T=Fhs_T,a_V=Fhs_V,a_n=Fhs_n)
       endif
       ! Calculate extraterm to hard-sphere reference for non-additive mixtures
       if (enable_hs_extra) then
          call calc_hardsphere_extra_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,&
               Fhse,a_T=Fhse_T,a_V=Fhse_V,a_n=Fhse_n)
       endif
       ! Calculate first order monomer term
       if (enable_A1) then
          call calcA1(nc,T,V,n,eos%saftvrmie_var,F1,a1_T=F1_T,a1_V=F1_V,a1_n=F1_n)
       endif
       ! Calculate second order monomer term
       if (enable_A2) then
          call calcA2(nc,T,V,n,eos%saftvrmie_var,F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n)
       end if
       ! Calculate third order monomer term
       if (enable_A3) then
          call calcA3(nc,T,V,n,eos%saftvrmie_var,&
               F3,a3_T=F3_T,a3_V=F3_V,a3_n=F3_n)
       endif
       ! Calculate chain term
       if (enable_chain) then
          call calcAchain(nc,T,V,n,eos%saftvrmie_var,Fc,ach_T=Fc_T,ach_V=Fc_V,ach_n=Fc_n,&
               returnF=returnF)
       endif
    else
       ! Precalculate common variables
       call preCalcSAFTVRMie(nc,T,V,n,0,eos%saftvrmie_var)
       ! Calculate hard-sphere term
       if (enable_hs) then
          call calc_hardsphere_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,Fhs)
       endif
       ! Calculate extraterm to hard-sphere reference for non-additive mixtures
       if (enable_hs_extra) then
          call calc_hardsphere_extra_helmholtzenergy(nc,T,V,n,eos%saftvrmie_var,&
               Fhse)
       endif
       ! Calculate first order monomer term
       if (enable_A1) then
          call calcA1(nc,T,V,n,eos%saftvrmie_var,F1)
       endif
       if (enable_A2) then
          ! Calculate second order monomer term
          call calcA2(nc,T,V,n,eos%saftvrmie_var,F2)
       end if
       ! Calculate third order monomer term
       if (enable_A3) then
          call calcA3(nc,T,V,n,eos%saftvrmie_var,F3)
       endif
       ! Calculate chain term
       if (enable_chain) then
          call calcAchain(nc,T,V,n,eos%saftvrmie_var,Fc,returnF=returnF)
       endif
    endif

    if (.not. enable_hs) then
       Fhs = 0.0
       Fhs_T = 0.0
       Fhs_V = 0.0
       Fhs_TT = 0.0
       Fhs_VV = 0.0
       Fhs_TV = 0.0
       Fhs_n = 0.0
       Fhs_Tn = 0.0
       Fhs_Vn = 0.0
       Fhs_nn = 0.0
    endif

    if (.not. enable_hs_extra) then
       Fhse = 0.0
       Fhse_T = 0.0
       Fhse_V = 0.0
       Fhse_TT = 0.0
       Fhse_VV = 0.0
       Fhse_TV = 0.0
       Fhse_n = 0.0
       Fhse_Tn = 0.0
       Fhse_Vn = 0.0
       Fhse_nn = 0.0
    endif

    if (.not. enable_A1) then
       F1 = 0.0
       F1_T = 0.0
       F1_V = 0.0
       F1_TT = 0.0
       F1_VV = 0.0
       F1_TV = 0.0
       F1_n = 0.0
       F1_Tn = 0.0
       F1_Vn = 0.0
       F1_nn = 0.0
    endif

    if (.not. enable_A2) then
       F2 = 0.0
       F2_T = 0.0
       F2_V = 0.0
       F2_TT = 0.0
       F2_VV = 0.0
       F2_TV = 0.0
       F2_n = 0.0
       F2_Tn = 0.0
       F2_Vn = 0.0
       F2_nn = 0.0
    endif

    if (.not. enable_A3) then
       F3 = 0.0
       F3_T = 0.0
       F3_V = 0.0
       F3_TT = 0.0
       F3_VV = 0.0
       F3_TV = 0.0
       F3_n = 0.0
       F3_Tn = 0.0
       F3_Vn = 0.0
       F3_nn = 0.0
    endif

    if (.not. enable_chain) then
       Fc = 0.0
       Fc_T = 0.0
       Fc_V = 0.0
       Fc_TT = 0.0
       Fc_VV = 0.0
       Fc_TV = 0.0
       Fc_n = 0.0
       Fc_Tn = 0.0
       Fc_Vn = 0.0
       Fc_nn = 0.0
    endif

    if (.not. enable_truncation_correction) then
       Ftc = 0.0
       Ftc_T = 0.0
       Ftc_V = 0.0
       Ftc_TT = 0.0
       Ftc_VV = 0.0
       Ftc_TV = 0.0
       Ftc_n = 0.0
       Ftc_Tn = 0.0
       Ftc_Vn = 0.0
       Ftc_nn = 0.0
    else
       call calc_delta_Ac(nc,T,V,n,r_cut,eos%saftvrmie_var,&
            Ftc,a_T=Ftc_T,a_V=Ftc_V,a_n=Ftc_n,a_TT=Ftc_TT,&
            a_VV=Ftc_VV,a_TV=Ftc_TV,a_Tn=Ftc_Tn,a_Vn=Ftc_Vn,&
            a_nn=Ftc_nn)
    endif

    beta(1) = 1.0/T
    beta(2) = beta(1)*beta(1)
    beta(3) = beta(1)*beta(2)
    ns = sum(n*saftvrmie_param%ms)
    am = Fhs + beta(1)*F1 + beta(2)*F2 + beta(3)*F3
    F = ns*am + Fhse + Fc - Ftc
    if (present(F_T) .or. present(F_Tn)) then
       am_T = Fhs_T + beta(1)*F1_T + beta(2)*F2_T + beta(3)*F3_T &
            -(beta(1)*F1 + 2.0*beta(2)*F2 + 3.0*beta(3)*F3)/T
    endif
    if (present(F_T)) then
       F_T = ns*am_T + Fhse_T + Fc_T - Ftc_T
    endif
    if (present(F_V) .or. present(F_Vn)) then
       am_V = Fhs_V + beta(1)*F1_V + beta(2)*F2_V + beta(3)*F3_V
    endif
    if (present(F_V)) then
       F_V = ns*am_V + Fhse_V + Fc_V - Ftc_V
    endif
    if (present(F_TT)) then
       F_TT = ns*(Fhs_TT + beta(1)*F1_TT + beta(2)*F2_TT + beta(3)*F3_TT) + Fc_TT - Ftc_TT &
            +ns*(2.0*beta(1)*F1 + 6.0*beta(2)*F2 + 12.0*beta(3)*F3)/T**2 &
            -2.0*ns*(beta(1)*F1_T + 2.0*beta(2)*F2_T + 3.0*beta(3)*F3_T)/T&
            +Fhse_TT
    endif
    if (present(F_VV)) then
       F_VV = ns*(Fhs_VV + beta(1)*F1_VV + beta(2)*F2_VV + beta(3)*F3_VV) + Fhse_VV + Fc_VV - Ftc_VV
    endif
    if (present(F_TV)) then
       F_TV = ns*(Fhs_TV + beta(1)*F1_TV + beta(2)*F2_TV + beta(3)*F3_TV)  + Fhse_TV + Fc_TV - Ftc_TV &
            -ns*(beta(1)*F1_V + 2.0*beta(2)*F2_V + 3.0*beta(3)*F3_V)/T
    endif
    if (present(F_Tn)) then
       F_Tn = ns*(Fhs_Tn + beta(1)*F1_Tn + beta(2)*F2_Tn + beta(3)*F3_Tn) + Fc_Tn - Ftc_Tn &
            -ns*(beta(1)*F1_n + 2.0*beta(1)**2*F2_n + 3.0*beta(3)*F3_n)/T &
            + saftvrmie_param%ms*am_T+ Fhse_Tn
    endif
    if (present(F_Vn)) then
       F_Vn = ns*(Fhs_Vn + beta(1)*F1_Vn + beta(2)*F2_Vn + beta(3)*F3_Vn) + Fhse_Vn + Fc_Vn - Ftc_Vn &
            + saftvrmie_param%ms*am_V
    endif
    if (present(F_n) .or. present(F_nn)) then
       am_n = Fhs_n + beta(1)*F1_n + beta(2)*F2_n + beta(3)*F3_n
    endif
    if (present(F_n)) then
       F_n = ns*am_n + Fc_n - Ftc_n + saftvrmie_param%ms*am + Fhse_n
    endif
    if (present(F_nn)) then
       F_nn = ns*(Fhs_nn + beta(1)*F1_nn + beta(1)**2*F2_nn + beta(1)**3*F3_nn) + Fc_nn - Ftc_nn
       do l=1,nc
          F_nn(:,l) = F_nn(:,l) + saftvrmie_param%ms*am_n(l) + saftvrmie_param%ms(l)*am_n
       enddo
       ! Add extra term
       F_nn=F_nn+Fhse_nn
    endif

  end subroutine calcFresSAFTVRMie

  !> Return de Boer parameter for component i
  !!
  !! \author Morten Hammer, March 2018
  function deBoerParameter(i) result(LAMBDA)
    use saftvrmie_containers, only: saftvrmie_param
    use thermopack_constants, only: h_const, kB_const
    ! Input
    integer, intent(in) :: i !< Component number
    real :: LAMBDA !< de Boer parameter
    !
    LAMBDA = h_const/(saftvrmie_param%comp(i)%sigma*&
         sqrt(kB_const*saftvrmie_param%comp(i)%mass*saftvrmie_param%comp(i)%eps_depth_divk))
  end function deBoerParameter

  !> Calculate model term:
  !! 0 - hard sphere
  !! 1 - a1
  !! 2 - a2
  !! 3 - a3
  !! \author Morten Hammer, March 2018
  function calc_saftvrmie_term(nc,T,V,n,term) result(a)
    use saftvrmie_hardsphere, only: calc_hardsphere_helmholtzenergy
    use saftvrmie_dispersion, only: calcA1, calcA2, calcA3
    use saftvrmie_containers, only: get_saftvrmie_var, &
         saftvrmie_var_container
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    integer, intent(in) :: term !< 0-3
    ! Output
    real :: a
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()

    ! Precalculate common variables
    call preCalcSAFTVRMie(nc,T,V,n,0,svrm_var)
    select case(term)
    case(0)
       ! Calculate hard-sphere term
       call calc_hardsphere_helmholtzenergy(nc,T,V,n,svrm_var,a)
    case(1)
       ! Calculate first order monomer term
       call calcA1(nc,T,V,n,svrm_var,a)
    case(2)
       ! Calculate second order monomer term
       call calcA2(nc,T,V,n,svrm_var,a)
    case(3)
       ! Calculate third order monomer term
       call calcA3(nc,T,V,n,svrm_var,a)
    case default
       a = 0.0
    end select
  end function calc_saftvrmie_term

  !> Calculate van der Waals alpha parameter for Mie-type fluid
  !! Function intended for plotting in python
  !! \author Morten Hammer, Mai 2018
  function calc_alpha_saftvrmie(T) result(alpha)
    use thermopack_var, only: nc
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
         calc_binary_effective_sigma, calc_binary_effective_eps_divk
    use saftvrmie_dispersion, only: calcAlpha
    use saftvrmie_containers, only: saftvrmie_param, get_saftvrmie_var, &
         saftvrmie_var_container
    ! Input
    real, intent(in) :: T !< Temperature [K]
    ! Output
    real :: alpha(nc,nc)
    ! Locals
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()
    if (quantum_correction_hs > 0) then
       ! NB: the order of the function calls below is important
       !
       ! Calculate Feynman--Hibbs D parameter
       call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
            svrm_var%DFeynHibbsij, svrm_var%D2FeynHibbsij)
       ! Calculate effective sigma
       call calc_binary_effective_sigma(nc,T,svrm_var,svrm_var%sigma_eff%d,&
            svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
       ! Calculate effective epsilon divided by k
       call calc_binary_effective_eps_divk(nc,T,svrm_var,svrm_var%eps_divk_eff%d,&
            svrm_var%eps_divk_eff%d_T,svrm_var%eps_divk_eff%d_TT)
       ! Calculate hard-sphere diameter
       call calc_hardsphere_diameter(nc,T,svrm_var,svrm_var%sigma_eff%d,&
            svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT,svrm_var%dhs%d,&
            svrm_var%dhs%d_T,svrm_var%dhs%d_TT)
       ! Calculate dimensionless van der Waals energy
       call calcAlpha(nc,svrm_var%sigma_eff,svrm_var%eps_divk_eff,&
            T,svrm_var,alpha,svrm_var%alpha%d_T,svrm_var%alpha%d_TT)
    else
       alpha = saftvrmie_param%alpha_ij
    endif
  end function calc_alpha_saftvrmie

end module saftvrmie_interface

subroutine testing()
  use thermopack_constants
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  use saftvrmie_containers
  use saftvrmie_hardsphere
  use saftvrmie_dispersion
  use saftvrmie_interface
  implicit none
  real :: n(nc),n0(nc)
  real :: ef,ef_e,ef_ee,ef_eee
  real :: ef2,ef2_e,ef2_ee,ef2_eee
  real :: a1s,a1s_e,a1s_ee,a1s_eee
  real :: a1s2,a1s2_e,a1s2_ee,a1s2_eee
  real :: x0,I,I_x,I_xx,I2,I2_x,I2_xx
  real :: J,J_x,J_xx,J2,J2_x,J2_xx
  real :: B,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx
  real :: B2,B2_e,B2_x,B2_ee,B2_xx,B2_ex,B2_eee,B2_eex,B2_exx
  real :: eta,lambda,eps,epsilon,T,C,lambda_r,lambda_a
  real :: a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx
  real :: a12,a12_e,a12_x,a12_ee,a12_xx,a12_ex,a12_eee,a12_eex,a12_exx
  real :: d,d_T,d_TT,V
  real :: K, K_e, K_ee, K_eee
  real :: K2, K2_e, K2_ee, K2_eee
  real :: chi, chi_z, chi_zz, chi_zzz, f_alpha(6)
  real :: chi2, chi2_z, chi2_zz, chi2_zzz
  real :: alpha, chi_a, chi_aa, chi_az, chi_aaz, chi_azz
  real :: chi2_a, chi2_aa, chi2_az, chi2_aaz, chi2_azz
  real :: x0_T,x0_TT
  real :: x02,x02_T,x02_TT
  type(saftvrmie_zeta) :: zeta2
  type(saftvrmie_dhs) :: dhs
  integer :: ii,jj
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()
  lambda = 12.0
  eta = 10.0
  call calcEffEta(eta,lambda,ef,ef_e,ef_ee,ef_eee)
  eps = 1.0e-7
  call calcEffEta(eta+eps,lambda,ef2,ef2_e,ef2_ee,ef2_eee)
  print *,"Testing eta eff"
  print *,ef
  print *,ef_e,(ef2-ef)/eps
  print *,ef_ee,(ef2_e-ef_e)/eps
  print *,ef_eee,(ef2_ee-ef_ee)/eps
  epsilon = 111.0
  call calcA1Sutherland(eta,lambda,epsilon,a1s,a1s_e,a1s_ee,a1s_eee)
  call calcA1Sutherland(eta+eps,lambda,epsilon,a1s2,a1s2_e,a1s2_ee,a1s2_eee)
  print *,"Testing A1 Sutherland"
  print *,a1s
  print *,a1s_e,(a1s2-a1s)/eps
  print *,a1s_ee,(a1s2_e-a1s_e)/eps
  print *,a1s_eee,(a1s2_ee-a1s_ee)/eps

  x0 = 1.05
  call calcILambda(x0,lambda,I,I_x,I_xx)
  call calcILambda(x0+eps,lambda,I2,I2_x,I2_xx)
  print *,"Testing I lambda"
  print *,I
  print *,I_x,(I2-I)/eps
  print *,I_xx,(I2_x-I_x)/eps
  call calcJLambda(x0,lambda,J,J_x,J_xx)
  call calcJLambda(x0+eps,lambda,J2,J2_x,J2_xx)
  print *,"Testing J lambda"
  print *,J
  print *,J_x,(J2-J)/eps
  print *,J_xx,(J2_x-J_x)/eps
  call calcBtilde(x0,eta,lambda,epsilon,B,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx,fac_in=100.)
  call calcBtilde(x0+eps,eta,lambda,epsilon,B2,B2_e,B2_x,B2_ee,B2_xx,B2_ex,B2_eee,B2_eex,B2_exx,fac_in=100.)
  print *,"Testing B tilde"
  print *, "x"
  print *,B
  print *,B_x,(B2-B)/eps
  print *,B_xx,(B2_x-B_x)/eps
  print *,B_ex,(B2_e-B_e)/eps
  print *,B_exx,(B2_ex-B_ex)/eps
  print *,B_eex,(B2_ee-B_ee)/eps
  call calcBtilde(x0,eta+eps,lambda,epsilon,B2,B2_e,B2_x,B2_ee,B2_xx,B2_ex,B2_eee,B2_eex,B2_exx,fac_in=100.)
  print *, "e"
  print *,B_e,(B2-B)/eps
  print *,B_ee,(B2_e-B_e)/eps
  print *,B_eee,(B2_ee-B_ee)/eps
  print *,B_ex,(B2_x-B_x)/eps
  print *,B_exx,(B2_xx-B_xx)/eps
  print *,B_eex,(B2_ex-B_ex)/eps

  print *,"Testing A1"
  T = 5.0
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  ! Calculate hard-sphere diameter
  call calc_hardsphere_diameter(nc,T,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT,svrm_var%dhs%d,&
       svrm_var%dhs%d_T,svrm_var%dhs%d_TT)
  ii = 1
  jj = 1
  d = svrm_var%dhs%d(ii,jj)
  d_T = svrm_var%dhs%d_T(ii,jj)
  d_TT = svrm_var%dhs%d_TT(ii,jj)
  x0 = saftvrmie_param%sigma_ij(ii,jj)/d
  !eta = svrm_var%zeta%zx
  lambda_a = saftvrmie_param%lambda_a_ij(ii,jj)
  lambda_r = saftvrmie_param%lambda_r_ij(ii,jj)
  epsilon = saftvrmie_param%eps_divk_ij(ii,jj)
  C = saftvrmie_param%Cij(ii,jj)

  call calcA1Tilde(x0,eta,lambda_a,lambda_r,eps,C,&
       a1,a1_e,a1_x,a1_ee,a1_xx,a1_ex,a1_eee,a1_eex,a1_exx)
  call calcA1Tilde(x0+eps,eta,lambda_a,lambda_r,eps,C,&
       a12,a12_e,a12_x,a12_ee,a12_xx,a12_ex,a12_eee,a12_eex,a12_exx)
  print *,"Testing A1 tilde"
  print *,a1
  print *,a1_x,(a12-a1)/eps
  print *,a1_xx,(a12_x-a1_x)/eps
  print *,a1_ex,(a12_e-a1_e)/eps
  print *,a1_exx,(a12_ex-a1_ex)/eps
  print *,a1_eex,(a12_ee-a1_ee)/eps
  call calcA1Tilde(x0,eta+eps,lambda_a,lambda_r,eps,C,&
       a12,a12_e,a12_x,a12_ee,a12_xx,a12_ex,a12_eee,a12_eex,a12_exx)
  print *,a1_e,(a12-a1)/eps
  print *,a1_ee,(a12_e-a1_e)/eps
  print *,a1_eee,(a12_ee-a1_ee)/eps
  print *,a1_ex,(a12_x-a1_x)/eps
  print *,a1_exx,(a12_xx-a1_xx)/eps
  print *,a1_eex,(a12_ex-a1_ex)/eps

  call calcKhs(eta, K, K_e, K_ee, K_eee)
  call calcKhs(eta+eps, K2, K2_e, K2_ee, K2_eee)
  print *,"Testing Khs"
  print *,K
  print *,K_e,(K2-K)/eps
  print *,K_ee,(K2_e-K_e)/eps
  print *,K_eee,(K2_ee-K_ee)/eps
  f_alpha = saftvrmie_param%f_alpha_ij(:,ii,jj)
  alpha = saftvrmie_param%alpha_ij(ii,jj)
  eps = 1.0e-5
  call calcCorrectionFactorA2(eta, alpha, f_alpha, chi, chi_z, chi_zz, chi_zzz, &
       chi_a, chi_aa, chi_az, chi_aaz, chi_azz)
  call calcCorrectionFactorA2(eta+eps, alpha, f_alpha, chi2, chi2_z, chi2_zz, &
       chi2_zzz, chi2_a, chi2_aa, chi2_az, chi2_aaz, chi2_azz)
  print *,"Testing chi"
  print *,chi
  print *,chi_z,(chi2-chi)/eps
  print *,chi_zz,(chi2_z-chi_z)/eps
  print *,chi_zzz,(chi2_zz-chi_zz)/eps
  print *,chi_az,(chi2_a-chi_a)/eps
  print *,chi_azz,(chi2_az-chi_az)/eps
  print *,chi_aaz,(chi2_aa-chi_aa)/eps
  call calcCorrectionFactorA2(eta, alpha+eps, f_alpha, chi2, chi2_z, chi2_zz, &
       chi2_zzz, chi2_a, chi2_aa, chi2_az, chi2_aaz, chi2_azz)
  print *,chi_a,(chi2-chi)/eps
  print *,chi_aa,(chi2_a-chi_a)/eps
  print *,chi_az,(chi2_z-chi_z)/eps
  print *,chi_azz,(chi2_zz-chi_zz)/eps
  print *,chi_aaz,(chi2_az-chi_az)/eps
  !stop
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  call allocate_saftvrmie_zeta(nc,zeta2)
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,svrm_var%zeta)
  call calcZetaX(nc,T,V+V*eps,n,3,svrm_var%dhs,zeta2)
  print *,"Testing hypotetical pure fluid packing fraction"
  print *,"Volume"
  print *,svrm_var%zeta%zx
  print *,svrm_var%zeta%zx_V,(zeta2%zx - svrm_var%zeta%zx)/(V*eps)
  print *,svrm_var%zeta%zx_VV,(zeta2%zx_V - svrm_var%zeta%zx_V)/(V*eps)
  print *,svrm_var%zeta%zx_TV,(zeta2%zx_T - svrm_var%zeta%zx_T)/(V*eps)
  print *,svrm_var%zeta%zx_Vn,(zeta2%zx_n - svrm_var%zeta%zx_n)/(V*eps)
  print *,svrm_var%zeta%zx_VVV,(zeta2%zx_VV - svrm_var%zeta%zx_VV)/(V*eps)
  print *,svrm_var%zeta%zx_VVT,(zeta2%zx_TV - svrm_var%zeta%zx_TV)/(V*eps)
  print *,svrm_var%zeta%zx_VTT,(zeta2%zx_TT - svrm_var%zeta%zx_TT)/(V*eps)
  print *,svrm_var%zeta%zx_VTn,(zeta2%zx_Tn - svrm_var%zeta%zx_Tn)/(V*eps)
  print *,svrm_var%zeta%zx_VVn,(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/(V*eps)
  print *,svrm_var%zeta%zx_Vnn(1,:),(zeta2%zx_nn(1,:) - svrm_var%zeta%zx_nn(1,:))/(V*eps)
  print *,svrm_var%zeta%zx_Vnn(2,:),(zeta2%zx_nn(2,:) - svrm_var%zeta%zx_nn(2,:))/(V*eps)

  n(1) = n(1) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,zeta2)
  print *,"n(1)"
  print *,svrm_var%zeta%zx_n(1),(zeta2%zx - svrm_var%zeta%zx)/eps
  print *,svrm_var%zeta%zx_Tn(1),(zeta2%zx_T - svrm_var%zeta%zx_T)/eps
  print *,svrm_var%zeta%zx_Vn(1),(zeta2%zx_V - svrm_var%zeta%zx_V)/eps
  print *,svrm_var%zeta%zx_nn(1,:),(zeta2%zx_n - svrm_var%zeta%zx_n)/eps
  print *,svrm_var%zeta%zx_VVn(1),(zeta2%zx_VV - svrm_var%zeta%zx_VV)/eps
  print *,svrm_var%zeta%zx_Vnn(1,:),(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/eps
  print *,svrm_var%zeta%zx_VTn(1),(zeta2%zx_TV - svrm_var%zeta%zx_TV)/eps

  n = n0
  n(2) = n(2) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,zeta2)
  print *,"n(2)"
  print *,svrm_var%zeta%zx_n(2),(zeta2%zx - svrm_var%zeta%zx)/eps
  print *,svrm_var%zeta%zx_Tn(2),(zeta2%zx_T - svrm_var%zeta%zx_T)/eps
  print *,svrm_var%zeta%zx_Vn(2),(zeta2%zx_V - svrm_var%zeta%zx_V)/eps
  print *,svrm_var%zeta%zx_nn(2,:),(zeta2%zx_n - svrm_var%zeta%zx_n)/eps
  print *,svrm_var%zeta%zx_VVn(2),(zeta2%zx_VV - svrm_var%zeta%zx_VV)/eps
  print *,svrm_var%zeta%zx_Vnn(2,:),(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/eps
  print *,svrm_var%zeta%zx_VTn(2),(zeta2%zx_TV - svrm_var%zeta%zx_TV)/eps

  n = n0
  call allocate_saftvrmie_dhs(nc,dhs)
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T+eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  ! Calculate hard-sphere diameter
  call calc_hardsphere_diameter(nc,T+eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT,svrm_var%dhs%d,&
       svrm_var%dhs%d_T,svrm_var%dhs%d_TT)
  call calcZetaX(nc,T+eps,V,n,3,svrm_var%dhs,zeta2)
  print *,"Temperature"
  print *,svrm_var%zeta%zx_T,(zeta2%zx - svrm_var%zeta%zx)/eps
  print *,svrm_var%zeta%zx_TT,(zeta2%zx_T - svrm_var%zeta%zx_T)/eps
  print *,svrm_var%zeta%zx_TV,(zeta2%zx_V - svrm_var%zeta%zx_V)/eps
  print *,svrm_var%zeta%zx_Tn,(zeta2%zx_n - svrm_var%zeta%zx_n)/eps
  print *,svrm_var%zeta%zx_VVT,(zeta2%zx_VV - svrm_var%zeta%zx_VV)/eps
  print *,svrm_var%zeta%zx_VTT,(zeta2%zx_TV - svrm_var%zeta%zx_TV)/eps
  print *,svrm_var%zeta%zx_VTn,(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/eps

  !> Calculate packing fraction
  call cleanup_saftvrmie_zeta(zeta2)
  call allocate_saftvrmie_zeta(nc,zeta2)
  call calc_binary_effective_sigma(nc,T,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  call calcZetaX(nc,T,V,n,3,svrm_var%sigma_eff,svrm_var%zeta_bar)
  call calcZetaX(nc,T,V+V*eps,n,3,svrm_var%sigma_eff,zeta2)
  print *,"Testing packing fraction"
  print *,"Volume"
  print *,svrm_var%zeta_bar%zx
  print *,svrm_var%zeta_bar%zx_V,(zeta2%zx - svrm_var%zeta_bar%zx)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VV,(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/(V*eps)
  print *,svrm_var%zeta_bar%zx_TV,(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/(V*eps)
  print *,svrm_var%zeta_bar%zx_Vn,(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VVV,(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VVT,(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VTT,(zeta2%zx_TT - svrm_var%zeta_bar%zx_TT)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VTn,(zeta2%zx_Tn - svrm_var%zeta_bar%zx_Tn)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VVn,(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/(V*eps)
  print *,svrm_var%zeta_bar%zx_Vnn(1,:),(zeta2%zx_nn(1,:) - svrm_var%zeta_bar%zx_nn(1,:))/(V*eps)
  print *,svrm_var%zeta_bar%zx_Vnn(2,:),(zeta2%zx_nn(2,:) - svrm_var%zeta_bar%zx_nn(2,:))/(V*eps)

  n(1) = n(1) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%sigma_eff,zeta2)
  print *,"n(1)"
  print *,svrm_var%zeta_bar%zx_n(1),(zeta2%zx - svrm_var%zeta_bar%zx)/eps
  print *,svrm_var%zeta_bar%zx_Tn(1),(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/eps
  print *,svrm_var%zeta_bar%zx_Vn(1),(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/eps
  print *,svrm_var%zeta_bar%zx_nn(1,:),(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/eps
  print *,svrm_var%zeta_bar%zx_VVn(1),(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/eps
  print *,svrm_var%zeta_bar%zx_Vnn(1,:),(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/eps
  print *,svrm_var%zeta_bar%zx_VTn(1),(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/eps
  !stop
  n = n0
  n(2) = n(2) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%sigma_eff,zeta2)
  print *,"n(2)"
  print *,svrm_var%zeta_bar%zx_n(2),(zeta2%zx - svrm_var%zeta_bar%zx)/eps
  print *,svrm_var%zeta_bar%zx_Tn(2),(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/eps
  print *,svrm_var%zeta_bar%zx_Vn(2),(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/eps
  print *,svrm_var%zeta_bar%zx_nn(2,:),(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/eps
  print *,svrm_var%zeta_bar%zx_VVn(2),(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/eps
  print *,svrm_var%zeta_bar%zx_Vnn(2,:),(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/eps
  print *,svrm_var%zeta_bar%zx_VTn(2),(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/eps
  !stop
  n = n0

  call calc_binary_effective_sigma(nc,T+eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  call calcZetaX(nc,T+eps,V,n,3,svrm_var%sigma_eff,zeta2)
  print *,"Temperature"
  print *,svrm_var%zeta_bar%zx_T,(zeta2%zx - svrm_var%zeta_bar%zx)/eps
  print *,svrm_var%zeta_bar%zx_TT,(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/eps
  print *,svrm_var%zeta_bar%zx_TV,(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/eps
  print *,svrm_var%zeta_bar%zx_Tn,(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/eps
  print *,svrm_var%zeta_bar%zx_VVT,(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/eps
  print *,svrm_var%zeta_bar%zx_VTT,(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/eps
  print *,svrm_var%zeta_bar%zx_VTn,(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/eps

  ! Test x0 differentials
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcXDifferentials(svrm_var%sigma_eff%d(ii,jj),&
       svrm_var%sigma_eff%d_T(ii,jj),&
       svrm_var%sigma_eff%d_TT(ii,jj),&
       svrm_var%dhs%d(ii,jj),svrm_var%dhs%d_T(ii,jj),&
       svrm_var%dhs%d_TT(ii,jj),x0,x0_T,x0_TT)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcXDifferentials(svrm_var%sigma_eff%d(ii,jj),&
       svrm_var%sigma_eff%d_T(ii,jj),&
       svrm_var%sigma_eff%d_TT(ii,jj),svrm_var%dhs%d(ii,jj),&
       svrm_var%dhs%d_T(ii,jj),&
       svrm_var%dhs%d_TT(ii,jj),x02,x02_T,x02_TT)
  print *,"x0 differentials"
  print *,x0
  print *,x0_T,(x02-x0)/(T*eps)
  print *,x0_TT,(x02_T-x0_T)/(T*eps)
  stop
end subroutine testing

subroutine test_Khs_TVn()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_hardsphere
  use saftvrmie_dispersion
  use saftvrmie_interface
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  type(saftvrmie_zeta) :: khs,zeta2
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 273.15
  eps = 1.0e-8
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  call allocate_saftvrmie_zeta(nc,zeta2)
  call allocate_saftvrmie_zeta(nc,khs)
  call calcZetaX(nc,T,V+V*eps,n,3,svrm_var%dhs,zeta2)
  call calcKhsTVn(nc,T,V+V*eps,n,3,svrm_var)
  print *,"Testing Khs"
  print *,"Volume"
  print *,svrm_var%Khs%zx
  print *,svrm_var%Khs%zx_V,(khs%zx - svrm_var%Khs%zx)/(V*eps)
  print *,svrm_var%Khs%zx_VV,(khs%zx_V - svrm_var%Khs%zx_V)/(V*eps)
  print *,svrm_var%Khs%zx_TV,(khs%zx_T - svrm_var%Khs%zx_T)/(V*eps)
  print *,svrm_var%Khs%zx_Vn,(khs%zx_n - svrm_var%Khs%zx_n)/(V*eps)
  print *,svrm_var%Khs%zx_VVV,(khs%zx_VV - svrm_var%Khs%zx_VV)/(V*eps)
  print *,svrm_var%Khs%zx_VVT,(khs%zx_TV - svrm_var%Khs%zx_TV)/(V*eps)
  print *,svrm_var%Khs%zx_VTT,(khs%zx_TT - svrm_var%Khs%zx_TT)/(V*eps)
  print *,svrm_var%Khs%zx_VTn,(khs%zx_Tn - svrm_var%Khs%zx_Tn)/(V*eps)
  print *,svrm_var%Khs%zx_VVn,(khs%zx_Vn - svrm_var%Khs%zx_Vn)/(V*eps)
  print *,svrm_var%Khs%zx_Vnn(1,:),(khs%zx_nn(1,:) - svrm_var%Khs%zx_nn(1,:))/(V*eps)
  print *,svrm_var%Khs%zx_Vnn(2,:),(khs%zx_nn(2,:) - svrm_var%Khs%zx_nn(2,:))/(V*eps)

  n(1) = n(1) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,zeta2)
  call calcKhsTVn(nc,T,V,n,3,svrm_var)
  print *,"n1"
  print *,svrm_var%Khs%zx_n(1),(khs%zx - svrm_var%Khs%zx)/eps
  print *,svrm_var%Khs%zx_Tn(1),(khs%zx_T - svrm_var%Khs%zx_T)/eps
  print *,svrm_var%Khs%zx_Vn(1),(khs%zx_V - svrm_var%Khs%zx_V)/eps
  print *,svrm_var%Khs%zx_nn(1,:),(khs%zx_n - svrm_var%Khs%zx_n)/eps
  print *,svrm_var%Khs%zx_VVn(1),(khs%zx_VV - svrm_var%Khs%zx_VV)/eps
  print *,svrm_var%Khs%zx_Vnn(1,:),(khs%zx_Vn - svrm_var%Khs%zx_Vn)/eps
  print *,svrm_var%Khs%zx_VTn(1),(khs%zx_TV - svrm_var%Khs%zx_TV)/eps

  n = n0
  n(2) = n(2) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,zeta2)
  call calcKhsTVn(nc,T,V,n,3,svrm_var)
  print *,"n2"
  print *,svrm_var%Khs%zx_n(2),(khs%zx - svrm_var%Khs%zx)/eps
  print *,svrm_var%Khs%zx_Tn(2),(khs%zx_T - svrm_var%Khs%zx_T)/eps
  print *,svrm_var%Khs%zx_Vn(2),(khs%zx_V - svrm_var%Khs%zx_V)/eps
  print *,svrm_var%Khs%zx_nn(2,:),(khs%zx_n - svrm_var%Khs%zx_n)/eps
  print *,svrm_var%Khs%zx_VVn(2),(khs%zx_VV - svrm_var%Khs%zx_VV)/eps
  print *,svrm_var%Khs%zx_Vnn(2,:),(khs%zx_Vn - svrm_var%Khs%zx_Vn)/eps
  print *,svrm_var%Khs%zx_VTn(2),(khs%zx_TV - svrm_var%Khs%zx_TV)/eps

  n = n0
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T+T*eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  ! Calculate hard-sphere diameter
  call calc_hardsphere_diameter(nc,T+T*eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT,svrm_var%dhs%d,&
       svrm_var%dhs%d_T,svrm_var%dhs%d_TT)
  call calcZetaX(nc,T+T*eps,V,n,3,svrm_var%dhs,zeta2)
  call calcKhsTVn(nc,T+T*eps,V,n,3,svrm_var)
  print *,"Temperature"
  print *,svrm_var%Khs%zx_T,(khs%zx - svrm_var%Khs%zx)/(T*eps)
  print *,svrm_var%Khs%zx_TT,(khs%zx_T - svrm_var%Khs%zx_T)/(T*eps)
  print *,svrm_var%Khs%zx_TV,(khs%zx_V - svrm_var%Khs%zx_V)/(T*eps)
  print *,svrm_var%Khs%zx_Tn,(khs%zx_n - svrm_var%Khs%zx_n)/(T*eps)
  print *,svrm_var%Khs%zx_VVT,(khs%zx_VV - svrm_var%Khs%zx_VV)/(T*eps)
  print *,svrm_var%Khs%zx_VTT,(khs%zx_TV - svrm_var%Khs%zx_TV)/(T*eps)
  print *,svrm_var%Khs%zx_VTn,(khs%zx_Vn - svrm_var%Khs%zx_Vn)/(T*eps)
  stop
end subroutine test_Khs_TVn

subroutine test_A1ij()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_hardsphere
  use saftvrmie_dispersion
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  type(saftvrmie_var_container) :: s_vc
  integer :: i,j
  real :: a1,a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
  real, dimension(nc) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
  real, dimension(nc,nc) :: a1_nn,a1_Vnn
  real :: a1p,a1p_T,a1p_V,a1p_TT,a1p_VV,a1p_TV,a1p_VVV,a1p_VVT,a1p_VTT
  real, dimension(nc) :: a1p_n,a1p_Tn,a1p_Vn,a1p_VVn,a1p_VTn
  real, dimension(nc,nc) :: a1p_nn,a1p_Vnn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  i = 1
  j = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  !call calcA1TildeTVn(nc,T,V,n,i,j,svrm_var%dhs,svrm_var%zeta,&
  !     a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
  !     a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
  call calcA1ij(nc,T,V,n,i,j,svrm_var,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)

  ! a1=1.0
  ! a1_T=0.0
  ! a1_V=0.0
  ! a1_n=0.0
  ! a1_TT=0.0
  ! a1_VV=0.0
  ! a1_TV=0.0
  ! a1_Tn=0.0
  ! a1_Vn=0.0
  ! a1_nn=0.0
  ! a1_VVV=0.0
  ! a1_VVT=0.0
  ! a1_VTT=0.0
  ! a1_VVn=0.0
  ! a1_Vnn=0.0
  ! a1_VTn=0.0
  ! call eta_a_product(nc,T,V,n,svrm_var%dhs%d(i,j),svrm_var%dhs%d_T(i,j),&
  !      svrm_var%dhs%d_TT(i,j),&
  !      a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
  !      a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
  call allocate_saftvrmie_var_container(nc,s_vc)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,s_vc)
  !call calcA1TildeTVn(nc,T,V,n,i,j,svrm_var%dhs,zeta,&
  !     a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !     a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  call calcA1ij(nc,T,V+V*eps,n,i,j,s_vc,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  ! a1p=1.0
  ! a1p_T=0.0
  ! a1p_V=0.0
  ! a1p_n=0.0
  ! a1p_TT=0.0
  ! a1p_VV=0.0
  ! a1p_TV=0.0
  ! a1p_Tn=0.0
  ! a1p_Vn=0.0
  ! a1p_nn=0.0
  ! a1p_VVV=0.0
  ! a1p_VVT=0.0
  ! a1p_VTT=0.0
  ! a1p_VVn=0.0
  ! a1p_Vnn=0.0
  ! a1p_VTn=0.0
  ! call eta_a_product(nc,T,V+V*eps,n,svrm_var%dhs%d(i,j),svrm_var%dhs%d_T(i,j),&
  !      svrm_var%dhs%d_TT(i,j),&
  !      a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !      a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  !call allocate_saftvrmie_zeta(nc,khs)
  !call calcKhsTVn(nc,T,V+V*eps,n,3,zeta,Khs)

  print *,"Testing A1 tilde"
  print *,"V"
  print *,a1
  print *,a1_V,(a1p - a1)/(V*eps)
  print *,a1_VV,(a1p_V - a1_V)/(V*eps)
  print *,a1_TV,(a1p_T - a1_T)/(V*eps)
  print *,a1_Vn,(a1p_n - a1_n)/(V*eps)
  print *,a1_VVV,(a1p_VV - a1_VV)/(V*eps)
  print *,a1_VVT,(a1p_TV - a1_TV)/(V*eps)
  print *,a1_VTT,(a1p_TT - a1_TT)/(V*eps)
  print *,a1_VTn,(a1p_Tn - a1_Tn)/(V*eps)
  print *,a1_VVn,(a1p_Vn - a1_Vn)/(V*eps)
  print *,a1_Vnn(1,:),(a1p_nn(1,:) - a1_nn(1,:))/(V*eps)
  print *,a1_Vnn(2,:),(a1p_nn(2,:) - a1_nn(2,:))/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,s_vc)
  !call calcA1TildeTVn(nc,T,V,n,i,j,svrm_var%dhs,zeta,&
  !     a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !     a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  call calcA1ij(nc,T,V,n,i,j,s_vc,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  ! a1p=1.0
  ! a1p_T=0.0
  ! a1p_V=0.0
  ! a1p_n=0.0
  ! a1p_TT=0.0
  ! a1p_VV=0.0
  ! a1p_TV=0.0
  ! a1p_Tn=0.0
  ! a1p_Vn=0.0
  ! a1p_nn=0.0
  ! a1p_VVV=0.0
  ! a1p_VVT=0.0
  ! a1p_VTT=0.0
  ! a1p_VVn=0.0
  ! a1p_Vnn=0.0
  ! a1p_VTn=0.0
  ! call eta_a_product(nc,T,V,n,svrm_var%dhs%d(i,j),svrm_var%dhs%d_T(i,j),&
  !      svrm_var%dhs%d_TT(i,j),&
  !      a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !      a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  print *,a1_n(1),(a1p - a1)/eps
  print *,a1_Tn(1),(a1p_T - a1_T)/eps
  print *,a1_Vn(1),(a1p_V - a1_V)/eps
  print *,a1_nn(1,:),(a1p_n - a1_n)/eps
  print *,a1_VVn(1),(a1p_VV - a1_VV)/eps
  print *,a1_Vnn(1,:),(a1p_Vn - a1_Vn)/eps
  print *,a1_VTn(1),(a1p_TV - a1_TV)/eps
  !stop
  print *,"n2"
  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,s_vc)
  !call calcA1TildeTVn(nc,T,V,n,i,j,svrm_var%dhs,zeta,&
  !     a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !     a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  call calcA1ij(nc,T,V,n,i,j,s_vc,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  ! a1p=1.0
  ! a1p_T=0.0
  ! a1p_V=0.0
  ! a1p_n=0.0
  ! a1p_TT=0.0
  ! a1p_VV=0.0
  ! a1p_TV=0.0
  ! a1p_Tn=0.0
  ! a1p_Vn=0.0
  ! a1p_nn=0.0
  ! a1p_VVV=0.0
  ! a1p_VVT=0.0
  ! a1p_VTT=0.0
  ! a1p_VVn=0.0
  ! a1p_Vnn=0.0
  ! a1p_VTn=0.0
  ! call eta_a_product(nc,T,V,n,svrm_var%dhs%d(i,j),svrm_var%dhs%d_T(i,j),&
  !      svrm_var%dhs%d_TT(i,j),&
  !      a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !      a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  print *,a1_n(2),(a1p - a1)/eps
  print *,a1_Tn(2),(a1p_T - a1_T)/eps
  print *,a1_Vn(2),(a1p_V - a1_V)/eps
  print *,a1_nn(2,:),(a1p_n - a1_n)/eps
  print *,a1_VVn(2),(a1p_VV - a1_VV)/eps
  print *,a1_Vnn(2,:),(a1p_Vn - a1_Vn)/eps
  print *,a1_VTn(2),(a1p_TV - a1_TV)/eps

  print *,"T"
  n = n0
  ! Calculate effective sigma
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,s_vc)
  !call calcA1TildeTVn(nc,T,V,n,i,j,svrm_var%dhs,zeta,&
  !     a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !     a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  call calcA1ij(nc,T+T*eps,V,n,i,j,s_vc,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  ! a1p=1.0
  ! a1p_T=0.0
  ! a1p_V=0.0
  ! a1p_n=0.0
  ! a1p_TT=0.0
  ! a1p_VV=0.0
  ! a1p_TV=0.0
  ! a1p_Tn=0.0
  ! a1p_Vn=0.0
  ! a1p_nn=0.0
  ! a1p_VVV=0.0
  ! a1p_VVT=0.0
  ! a1p_VTT=0.0
  ! a1p_VVn=0.0
  ! a1p_Vnn=0.0
  ! a1p_VTn=0.0
  ! call eta_a_product(nc,T+eps,V,n,svrm_var%dhs%d(i,j),svrm_var%dhs%d_T(i,j),&
  !      svrm_var%dhs%d_TT(i,j),&
  !      a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
  !      a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  print *,a1_T,(a1p - a1)/(T*eps)
  print *,a1_TT,(a1p_T - a1_T)/(T*eps)
  print *,a1_TV,(a1p_V - a1_V)/(T*eps)
  print *,a1_Tn,(a1p_n - a1_n)/(T*eps)
  print *,a1_VVT,(a1p_VV - a1_VV)/(T*eps)
  print *,a1_VTT,(a1p_TV - a1_TV)/(T*eps)
  print *,a1_VTn,(a1p_Vn - a1_Vn)/(T*eps)

  stop
end subroutine test_A1ij

subroutine test_A1()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  integer :: i,j
  real :: a1,a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
  real, dimension(nc) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
  real, dimension(nc,nc) :: a1_nn,a1_Vnn
  real :: a1p,a1p_T,a1p_V,a1p_TT,a1p_VV,a1p_TV,a1p_VVV,a1p_VVT,a1p_VTT
  real, dimension(nc) :: a1p_n,a1p_Tn,a1p_Vn,a1p_VVn,a1p_VTn
  real, dimension(nc,nc) :: a1p_nn,a1p_Vnn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  i = 1
  j = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA1(nc,T,V+V*eps,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)

  print *,"Testing A1"
  print *,"V"
  print *,a1
  print *,a1_V,(a1p - a1)/(V*eps)
  print *,a1_VV,(a1p_V - a1_V)/(V*eps)
  print *,a1_TV,(a1p_T - a1_T)/(V*eps)
  print *,a1_Vn,(a1p_n - a1_n)/(V*eps)
  print *,a1_VVV,(a1p_VV - a1_VV)/(V*eps)
  print *,a1_VVT,(a1p_TV - a1_TV)/(V*eps)
  print *,a1_VTT,(a1p_TT - a1_TT)/(V*eps)
  print *,a1_VTn,(a1p_Tn - a1_Tn)/(V*eps)
  print *,a1_VVn,(a1p_Vn - a1_Vn)/(V*eps)
  print *,a1_Vnn(1,:),(a1p_nn(1,:) - a1_nn(1,:))/(V*eps)
  print *,a1_Vnn(2,:),(a1p_nn(2,:) - a1_nn(2,:))/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  print *,a1_n(1),(a1p - a1)/eps
  print *,a1_Tn(1),(a1p_T - a1_T)/eps
  print *,a1_Vn(1),(a1p_V - a1_V)/eps
  print *,a1_nn(1,:),(a1p_n - a1_n)/eps
  print *,a1_VVn(1),(a1p_VV - a1_VV)/eps
  print *,a1_Vnn(1,:),(a1p_Vn - a1_Vn)/eps
  print *,a1_VTn(1),(a1p_TV - a1_TV)/eps
  !stop
  print *,"n2"
  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  print *,a1_n(2),(a1p - a1)/eps
  print *,a1_Tn(2),(a1p_T - a1_T)/eps
  print *,a1_Vn(2),(a1p_V - a1_V)/eps
  print *,a1_nn(2,:),(a1p_n - a1_n)/eps
  print *,a1_VVn(2),(a1p_VV - a1_VV)/eps
  print *,a1_Vnn(2,:),(a1p_Vn - a1_Vn)/eps
  print *,a1_VTn(2),(a1p_TV - a1_TV)/eps

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA1(nc,T+T*eps,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn,&
       a1p_VVV,a1p_VVT,a1p_VTT,a1p_VVn,a1p_Vnn,a1p_VTn)
  print *,a1_T,(a1p - a1)/(T*eps)
  print *,a1_TT,(a1p_T - a1_T)/(T*eps)
  print *,a1_TV,(a1p_V - a1_V)/(T*eps)
  print *,a1_Tn,(a1p_n - a1_n)/(T*eps)
  print *,a1_VVT,(a1p_VV - a1_VV)/(T*eps)
  print *,a1_VTT,(a1p_TV - a1_TV)/(T*eps)
  print *,a1_VTn,(a1p_Vn - a1_Vn)/(T*eps)

  stop

end subroutine test_A1

subroutine test_A2_tilde()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc)
  real :: x0
  real :: eta,eps,epsilon,T,C,lambda_r,lambda_a
  real :: a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx
  real :: a22,a22_e,a22_x,a22_ee,a22_xx,a22_ex,a22_eee,a22_eex,a22_exx
  real :: d,d_T,d_TT,V
  integer :: i,j
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()

  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 273.15
  eps = 1.0e-8
  i = 1
  j = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)


  eps = 1.0e-8

  print *,"Testing A22"
  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  x0 = saftvrmie_param%sigma_ij(i,j)/d
  eta = svrm_var%zeta%zx
  lambda_a = saftvrmie_param%lambda_a_ij(i,j)
  lambda_r = saftvrmie_param%lambda_r_ij(i,j)
  epsilon = saftvrmie_param%eps_divk_ij(i,j)
  C = saftvrmie_param%Cij(i,j)
  call calcA22Tilde(x0,eta,lambda_a,lambda_r,epsilon,&
       a2,a2_e,a2_x,a2_ee,a2_xx,a2_ex,a2_eee,a2_eex,a2_exx)
  call calcA22Tilde(x0+eps,eta,lambda_a,lambda_r,epsilon,&
       a22,a22_e,a22_x,a22_ee,a22_xx,a22_ex,a22_eee,a22_eex,a22_exx)

  print *,"Testing A2 tilde"
  print *,a2
  print *,a2_x,(a22-a2)/eps
  print *,a2_xx,(a22_x-a2_x)/eps
  print *,a2_ex,(a22_e-a2_e)/eps
  print *,a2_exx,(a22_ex-a2_ex)/eps
  print *,a2_eex,(a22_ee-a2_ee)/eps
  call calcA22Tilde(x0,eta+eps,lambda_a,lambda_r,epsilon,&
       a22,a22_e,a22_x,a22_ee,a22_xx,a22_ex,a22_eee,a22_eex,a22_exx)
  print *,a2_e,(a22-a2)/eps
  print *,a2_ee,(a22_e-a2_e)/eps
  print *,a2_eee,(a22_ee-a2_ee)/eps
  print *,a2_ex,(a22_x-a2_x)/eps
  print *,a2_exx,(a22_xx-a2_xx)/eps
  print *,a2_eex,(a22_ex-a2_ex)/eps
  stop
end subroutine test_A2_tilde

subroutine test_A2()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  integer :: i,j
  real :: a1,a1_T,a1_V,a1_TT,a1_VV,a1_TV
  real, dimension(nc) :: a1_n,a1_Tn,a1_Vn
  real, dimension(nc,nc) :: a1_nn
  real :: a1p,a1p_T,a1p_V,a1p_TT,a1p_VV,a1p_TV
  real, dimension(nc) :: a1p_n,a1p_Tn,a1p_Vn
  real, dimension(nc,nc) :: a1p_nn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()

  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  i = 1
  j = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA2(nc,T,V,n,svrm_var,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA2(nc,T,V+V*eps,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)

  print *,"Testing A2"
  print *,"V"
  print *,a1
  print *,a1_V,(a1p - a1)/(V*eps)
  print *,a1_VV,(a1p_V - a1_V)/(V*eps)
  print *,a1_TV,(a1p_T - a1_T)/(V*eps)
  print *,a1_Vn,(a1p_n - a1_n)/(V*eps)
  ! print *,a1_VVV,(a1p_VV - a1_VV)/(V*eps)
  ! print *,a1_VVT,(a1p_TV - a1_TV)/(V*eps)
  ! print *,a1_VTT,(a1p_TT - a1_TT)/(V*eps)
  ! print *,a1_VTn,(a1p_Tn - a1_Tn)/(V*eps)
  ! print *,a1_VVn,(a1p_Vn - a1_Vn)/(V*eps)
  ! print *,a1_Vnn(1,:),(a1p_nn(1,:) - a1_nn(1,:))/(V*eps)
  ! print *,a1_Vnn(2,:),(a1p_nn(2,:) - a1_nn(2,:))/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA2(nc,T,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)
  print *,a1_n(1),(a1p - a1)/eps
  print *,a1_Tn(1),(a1p_T - a1_T)/eps
  print *,a1_Vn(1),(a1p_V - a1_V)/eps
  print *,a1_nn(1,:),(a1p_n - a1_n)/eps
  ! print *,a1_VVn(1),(a1p_VV - a1_VV)/eps
  ! print *,a1_Vnn(1,:),(a1p_Vn - a1_Vn)/eps
  ! print *,a1_VTn(1),(a1p_TV - a1_TV)/eps
  !stop
  print *,"n2"
  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA2(nc,T,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)
  print *,a1_n(2),(a1p - a1)/eps
  print *,a1_Tn(2),(a1p_T - a1_T)/eps
  print *,a1_Vn(2),(a1p_V - a1_V)/eps
  print *,a1_nn(2,:),(a1p_n - a1_n)/eps
  ! print *,a1_VVn(2),(a1p_VV - a1_VV)/eps
  ! print *,a1_Vnn(2,:),(a1p_Vn - a1_Vn)/eps
  ! print *,a1_VTn(2),(a1p_TV - a1_TV)/eps

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA2(nc,T+T*eps,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)
  print *,a1_T,(a1p - a1)/(T*eps)
  print *,a1_TT,(a1p_T - a1_T)/(T*eps)
  print *,a1_TV,(a1p_V - a1_V)/(T*eps)
  print *,a1_Tn,(a1p_n - a1_n)/(T*eps)
  ! print *,a1_VVT,(a1p_VV - a1_VV)/(T*eps)
  ! print *,a1_VTT,(a1p_TV - a1_TV)/(T*eps)
  ! print *,a1_VTn,(a1p_Vn - a1_Vn)/(T*eps)

  stop

end subroutine test_A2

subroutine test_A3()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  integer :: i,j
  real :: a1,a1_T,a1_V,a1_TT,a1_VV,a1_TV
  real, dimension(nc) :: a1_n,a1_Tn,a1_Vn
  real, dimension(nc,nc) :: a1_nn
  real :: a1p,a1p_T,a1p_V,a1p_TT,a1p_VV,a1p_TV
  real, dimension(nc) :: a1p_n,a1p_Tn,a1p_Vn
  real, dimension(nc,nc) :: a1p_nn
  real :: z,a3,a3_z,a3_zz,a32,a32_z,a32_zz
  real :: a3_a,a3_aa,a3_az,a32_a,a32_aa,a32_az
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()
  if (nc == 2) then
     n = (/0.2,1.2/)
  else
     n = 1.2
  endif
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  i = 1
  j = 1
  quantum_correction = 2
  quantum_correction_hs = 2

  a3_model = A3_SIJ_PREFAC
  !a3_model = A3_LAFITTE
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  z = svrm_var%zeta_bar%zx
  call calcA3zeta(saftvrmie_param%eps_divk_ij(i,j), z, svrm_var%alpha%d(i,j),&
       saftvrmie_param%f_alpha_ij(:,i,j), a3, a3_z, a3_zz, a3_a, a3_aa, a3_az)
  call calcA3zeta(saftvrmie_param%eps_divk_ij(i,j), z+eps, svrm_var%alpha%d(i,j),&
       saftvrmie_param%f_alpha_ij(:,i,j), a32, a32_z, a32_zz, a32_a, a32_aa, a32_az)
  print *,"Testing a3 differentials in zeta"
  print *,a3
  print *,a3_z,(a32 - a3)/(eps)
  print *,a3_zz,(a32_z - a3_z)/(eps)
  print *,a3_az,(a32_a - a3_a)/(eps)
  call calcA3zeta(saftvrmie_param%eps_divk_ij(i,j), z, svrm_var%alpha%d(i,j) + eps,&
       saftvrmie_param%f_alpha_ij(:,i,j), a32, a32_z, a32_zz, a32_a, a32_aa, a32_az)
  print *,"Testing a3 differentials in alpha"
  print *,a3_a,(a32 - a3)/(eps)
  print *,a3_aa,(a32_a - a3_a)/(eps)
  print *,a3_az,(a32_z - a3_z)/(eps)

  call calcA3(nc,T,V,n,svrm_var,&
       a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA3(nc,T,V+V*eps,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)

  print *,"Testing A3"
  print *,"V"
  print *,a1
  print *,a1_V,(a1p - a1)/(V*eps)
  print *,a1_VV,(a1p_V - a1_V)/(V*eps)
  print *,a1_TV,(a1p_T - a1_T)/(V*eps)
  print *,a1_Vn,(a1p_n - a1_n)/(V*eps)
  ! print *,a1_VVV,(a1p_VV - a1_VV)/(V*eps)
  ! print *,a1_VVT,(a1p_TV - a1_TV)/(V*eps)
  ! print *,a1_VTT,(a1p_TT - a1_TT)/(V*eps)
  ! print *,a1_VTn,(a1p_Tn - a1_Tn)/(V*eps)
  ! print *,a1_VVn,(a1p_Vn - a1_Vn)/(V*eps)
  ! print *,a1_Vnn(1,:),(a1p_nn(1,:) - a1_nn(1,:))/(V*eps)
  ! print *,a1_Vnn(2,:),(a1p_nn(2,:) - a1_nn(2,:))/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA3(nc,T,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)
  print *,a1_n(1),(a1p - a1)/eps
  print *,a1_Tn(1),(a1p_T - a1_T)/eps
  print *,a1_Vn(1),(a1p_V - a1_V)/eps
  print *,a1_nn(1,:),(a1p_n - a1_n)/eps
  ! print *,a1_VVn(1),(a1p_VV - a1_VV)/eps
  ! print *,a1_Vnn(1,:),(a1p_Vn - a1_Vn)/eps
  ! print *,a1_VTn(1),(a1p_TV - a1_TV)/eps
  !stop
  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcA3(nc,T,V,n,svrm_var,&
          a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)
     print *,a1_n(2),(a1p - a1)/eps
     print *,a1_Tn(2),(a1p_T - a1_T)/eps
     print *,a1_Vn(2),(a1p_V - a1_V)/eps
     print *,a1_nn(2,:),(a1p_n - a1_n)/eps
     ! print *,a1_VVn(2),(a1p_VV - a1_VV)/eps
     ! print *,a1_Vnn(2,:),(a1p_Vn - a1_Vn)/eps
     ! print *,a1_VTn(2),(a1p_TV - a1_TV)/eps
  endif
  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA3(nc,T+T*eps,V,n,svrm_var,&
       a1p,a1p_T,a1p_V,a1p_n,a1p_TT,a1p_VV,a1p_TV,a1p_Tn,a1p_Vn,a1p_nn)
  print *,a1_T,(a1p - a1)/(T*eps)
  print *,a1_TT,(a1p_T - a1_T)/(T*eps)
  print *,a1_TV,(a1p_V - a1_V)/(T*eps)
  print *,a1_Tn,(a1p_n - a1_n)/(T*eps)
  ! print *,a1_VVT,(a1p_VV - a1_VV)/(T*eps)
  ! print *,a1_VTT,(a1p_TV - a1_TV)/(T*eps)
  ! print *,a1_VTn,(a1p_Vn - a1_Vn)/(T*eps)

  stop
end subroutine test_A3

subroutine test_g0(Ti,Vi,ni,doInit,qc)
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_chain
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  integer, optional, intent(in) :: qc
  ! Locals
  real :: n(nc),n0(nc),T,V,eps
  integer :: i
  logical :: doInit_L
  real :: x0,eta,g,g_e,g_x,g_ee,g_xx,g_ex,g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex
  real, dimension(nc) :: g0,g0_T,g0_V,g0_TT,g0_VV,g0_TV
  real, dimension(nc,nc) :: g0_n,g0_Tn,g0_Vn
  real, dimension(nc,nc,nc) :: g0_nn
  real, dimension(nc) :: g02,g02_T,g02_V,g02_TT,g02_VV,g02_TV
  real, dimension(nc,nc) :: g02_n,g02_Tn,g02_Vn
  real, dimension(nc,nc,nc) :: g02_nn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()

  if (present(qc)) then
     quantum_correction=qc
  else
     quantum_correction=1
  endif
  if (present(doInit)) then
     doInit_L = doInit
  else
     doInit_L = .true.
  endif
  if (doInit_L) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     if (nc == 2) then
        n = (/0.2,1.2/)
     else
        n = 0.9
     endif
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif
  eps = 1.0e-8
  i = 1
  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  eta = svrm_var%zeta%zx
  x0 = saftvrmie_param%sigma_ij(i,i)/svrm_var%dhs%d(i,i)
  call rdf_at_contact_zeroth_order(eta,x0,g,g_e,g_x,g_ee,g_xx,g_ex)
  call rdf_at_contact_zeroth_order(eta+eps,x0,g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
  print *,g
  print *,g_e,(g2 - g)/(eps)
  print *,g_ee,(g2_e - g_e)/(eps)
  print *,g_ex,(g2_x - g_x)/(eps)
  call rdf_at_contact_zeroth_order(eta,x0+eps,g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
  print *,g_x,(g2 - g)/(eps)
  print *,g_xx,(g2_x - g_x)/(eps)
  print *,g_ex,(g2_e - g_e)/(eps)

  !stop
  call rdf_at_contact_zeroth_order_TVn(nc,T,V,n,svrm_var,&
       g0,g0_T,g0_V,g0_n,g0_TT,g0_VV,g0_TV,g0_Tn,g0_Vn,g0_nn)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call rdf_at_contact_zeroth_order_TVn(nc,T,V+V*eps,n,svrm_var,&
       g02,g02_T,g02_V,g02_n,g02_TT,g02_VV,g02_TV,g02_Tn,g02_Vn,g02_nn)
  print *,"Testing g0"
  print *,"V"
  print *,g0(i)
  print *,g0_V(i),(g02(i) - g0(i))/(V*eps)
  print *,g0_VV(i),(g02_V(i) - g0_V(i))/(V*eps)
  print *,g0_TV(i),(g02_T(i) - g0_T(i))/(V*eps)
  print *,g0_Vn(:,i),(g02_n(:,i) - g0_n(:,i))/(V*eps)

  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call rdf_at_contact_zeroth_order_TVn(nc,T,V,n,svrm_var,&
       g02,g02_T,g02_V,g02_n,g02_TT,g02_VV,g02_TV,g02_Tn,g02_Vn,g02_nn)
  print *,g0_n(1,i),(g02(i) - g0(i))/eps
  print *,g0_Tn(1,i),(g02_T(i) - g0_T(i))/eps
  print *,g0_Vn(1,i),(g02_V(i) - g0_V(i))/eps
  print *,g0_nn(1,:,i),(g02_n(:,i) - g0_n(:,i))/eps

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call rdf_at_contact_zeroth_order_TVn(nc,T+T*eps,V,n,svrm_var,&
       g02,g02_T,g02_V,g02_n,g02_TT,g02_VV,g02_TV,g02_Tn,g02_Vn,g02_nn)
  print *,g0_T(i),(g02(i) - g0(i))/(T*eps)
  print *,g0_TT(i),(g02_T(i) - g0_T(i))/(T*eps)
  print *,g0_TV(i),(g02_V(i) - g0_V(i))/(T*eps)
  print *,g0_Tn(:,i),(g02_n(:,i) - g0_n(:,i))/(T*eps)

  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call rdf_at_contact_zeroth_order_TVn(nc,T,V,n,svrm_var,&
          g02,g02_T,g02_V,g02_n,g02_TT,g02_VV,g02_TV,g02_Tn,g02_Vn,g02_nn)
     print *,g0_n(2,i),(g02(i) - g0(i))/eps
     print *,g0_Tn(2,i),(g02_T(i) - g0_T(i))/eps
     print *,g0_Vn(2,i),(g02_V(i) - g0_V(i))/eps
     print *,g0_nn(2,:,i),(g02_n(:,i) - g0_n(:,i))/eps
  endif
  stop
end subroutine test_g0

subroutine test_g1(Ti,Vi,ni,doInit,qc)
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_chain
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  integer, optional, intent(in) :: qc
  ! Locals
  logical :: doInit_L
  real :: n(nc),n0(nc),T,V,eps
  integer :: i
  real :: x0,eta,g,g_e,g_x,g_ee,g_xx,g_ex,g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex
  real, dimension(nc) :: g1,g1_T,g1_V,g1_TT,g1_VV,g1_TV
  real, dimension(nc,nc) :: g1_n,g1_Tn,g1_Vn
  real, dimension(nc,nc,nc) :: g1_nn
  real, dimension(nc) :: g12,g12_T,g12_V,g12_TT,g12_VV,g12_TV
  real, dimension(nc,nc) :: g12_n,g12_Tn,g12_Vn
  real, dimension(nc,nc,nc) :: g12_nn
  real ::  a1_VVV,a1_VVT,a1_VTT
  real, dimension(nc) :: a1_VVn,a1_VTn
  real, dimension(nc,nc) :: a1_Vnn
  type(saftvrmie_zeta) :: pf, pf2
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()

  if (present(qc)) then
     quantum_correction=qc
  else
     quantum_correction=1
  endif
  if (present(doInit)) then
    doInit_L = doInit
  else
    doInit_L = .true.
  endif
  if (doInit_L) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     if (nc == 2) then
        n = (/0.2,1.2/)
     else
        n = 0.9
     endif
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif
  eps = 1.0e-5
  i = 1
  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  eta = svrm_var%zeta%zx
  x0 = saftvrmie_param%sigma_ij(i,i)/svrm_var%dhs%d(i,i)
  call calcG12_ex(x0,eta,saftvrmie_param%lambda_a_ij(i,i),&
       saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
       saftvrmie_param%Cij(i,i),g,g_e,g_x,g_ee,g_xx,g_ex)
  call calcG12_ex(x0,eta+eps,saftvrmie_param%lambda_a_ij(i,i),&
       saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
       saftvrmie_param%Cij(i,i),g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
  print *,g
  print *,g_e,(g2 - g)/(eps)
  print *,g_ee,(g2_e - g_e)/(eps)
  print *,g_ex,(g2_x - g_x)/(eps)
  call calcG12_ex(x0+eps,eta,saftvrmie_param%lambda_a_ij(i,i),&
       saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
       saftvrmie_param%Cij(i,i),g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
  print *,g_x,(g2 - g)/(eps)
  print *,g_xx,(g2_x - g_x)/(eps)
  print *,g_ex,(g2_e - g_e)/(eps)

  if (hardsphere_EoS == HS_EOS_PURE_DIJ) then
     call allocate_saftvrmie_zeta(nc,pf)
     call allocate_saftvrmie_zeta(nc,pf2)
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcGX1_prefac_pure_ref(nc,T,V,n,svrm_var%d_pure,pf)
     call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
     call calcGX1_prefac_pure_ref(nc,T,V+V*eps,n,svrm_var%d_pure,pf2)
     print *,"Testing prefactor"
     print *,"V"
     print *,pf%zx
     print *,pf%zx_V,(pf2%zx - pf%zx)/(V*eps)
     print *,pf%zx_VV,(pf2%zx_V - pf%zx_V)/(V*eps)
     print *,pf%zx_TV,(pf2%zx_T - pf%zx_T)/(V*eps)
     print *,pf%zx_Vn,(pf2%zx_n - pf%zx_n)/(V*eps)

     call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
     call calcGX1_prefac_pure_ref(nc,T+T*eps,V,n,svrm_var%d_pure,pf2)
     print *,"T"
     print *,pf%zx_T,(pf2%zx - pf%zx)/(T*eps)
     print *,pf%zx_TT,(pf2%zx_T - pf%zx_T)/(T*eps)
     print *,pf%zx_TV,(pf2%zx_V - pf%zx_V)/(T*eps)
     print *,pf%zx_Tn,(pf2%zx_n - pf%zx_n)/(T*eps)

     n(1) = n(1) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcGX1_prefac_pure_ref(nc,T,V,n,svrm_var%d_pure,pf2)
     print *,"n1"
     print *,pf%zx_n(1),(pf2%zx - pf%zx)/(eps)
     print *,pf%zx_Tn(1),(pf2%zx_T - pf%zx_T)/(eps)
     print *,pf%zx_Vn(1),(pf2%zx_V - pf%zx_V)/(eps)
     print *,pf%zx_nn(:,1),(pf2%zx_n - pf%zx_n)/(eps)

     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcGX1_prefac_pure_ref(nc,T,V,n,svrm_var%d_pure,pf2)
     print *,"n1"
     print *,pf%zx_n(2),(pf2%zx - pf%zx)/(eps)
     print *,pf%zx_Tn(2),(pf2%zx_T - pf%zx_T)/(eps)
     print *,pf%zx_Vn(2),(pf2%zx_V - pf%zx_V)/(eps)
     print *,pf%zx_nn(:,2),(pf2%zx_n - pf%zx_n)/(eps)

     call cleanup_saftvrmie_zeta(pf)
     call cleanup_saftvrmie_zeta(pf2)
  endif
  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g1(i),a1_T=g1_T(i),a1_V=g1_V(i),a1_n=g1_n(:,i),a1_TT=g1_TT(i),a1_VV=g1_VV(i),&
       a1_TV=g1_TV(i),a1_Tn=g1_Tn(:,i),a1_Vn=g1_Vn(:,i),a1_nn=g1_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcG11_if(nc,T,V,n,i,svrm_var,&
       g1(i),g1_T(i),g1_V(i),g1_n(:,i),g1_TT(i),g1_VV(i),g1_TV(i),&
       g1_Tn(:,i),g1_Vn(:,i),g1_nn(:,:,i))

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA1(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcG11_if(nc,T,V+V*eps,n,i,svrm_var,&
       g12(i),g12_T(i),g12_V(i),g12_n(:,i),g12_TT(i),g12_VV(i),g12_TV(i),&
       g12_Tn(:,i),g12_Vn(:,i),g12_nn(:,:,i))

  print *,"Testing g11"
  print *,"V"
  print *,g1(i)
  print *,g1_V(i),(g12(i) - g1(i))/(V*eps)
  print *,g1_VV(i),(g12_V(i) - g1_V(i))/(V*eps)
  print *,g1_TV(i),(g12_T(i) - g1_T(i))/(V*eps)
  print *,g1_Vn(:,i),(g12_n(:,i) - g1_n(:,i))/(V*eps)

  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcG11_if(nc,T,V,n,i,svrm_var,&
       g12(i),g12_T(i),g12_V(i),g12_n(:,i),g12_TT(i),g12_VV(i),g12_TV(i),&
       g12_Tn(:,i),g12_Vn(:,i),g12_nn(:,:,i))
  print *,g1_n(1,i),(g12(i) - g1(i))/eps
  print *,g1_Tn(1,i),(g12_T(i) - g1_T(i))/eps
  print *,g1_Vn(1,i),(g12_V(i) - g1_V(i))/eps
  print *,g1_nn(1,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  !stop
  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcA1(nc,T,V,n,svrm_var,&
          g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
          a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
          a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
     call calcG11_if(nc,T,V,n,i,svrm_var,&
          g12(i),g12_T(i),g12_V(i),g12_n(:,i),g12_TT(i),g12_VV(i),g12_TV(i),&
          g12_Tn(:,i),g12_Vn(:,i),g12_nn(:,:,i))
     print *,g1_n(2,i),(g12(i) - g1(i))/eps
     print *,g1_Tn(2,i),(g12_T(i) - g1_T(i))/eps
     print *,g1_Vn(2,i),(g12_V(i) - g1_V(i))/eps
     print *,g1_nn(2,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  endif

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA1(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcG11_if(nc,T+T*eps,V,n,i,svrm_var,&
       g12(i),g12_T(i),g12_V(i),g12_n(:,i),g12_TT(i),g12_VV(i),g12_TV(i),&
       g12_Tn(:,i),g12_Vn(:,i),g12_nn(:,:,i))
  print *,g1_T(i),(g12(i) - g1(i))/(T*eps)
  print *,g1_TT(i),(g12_T(i) - g1_T(i))/(T*eps)
  print *,g1_TV(i),(g12_V(i) - g1_V(i))/(T*eps)
  print *,g1_Tn(:,i),(g12_n(:,i) - g1_n(:,i))/(T*eps)

  !********************************************************************************
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call rdf_at_contact_first_order_TVn(nc,T,V,n,svrm_var,&
       g1,g1_T,g1_V,g1_n,g1_TT,g1_VV,g1_TV,&
       g1_Tn,g1_Vn,g1_nn)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA1(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call rdf_at_contact_first_order_TVn(nc,T,V+V*eps,n,svrm_var,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)

  print *,"Testing g1"
  print *,"V"
  print *,g1(i)
  print *,g1_V(i),(g12(i) - g1(i))/(V*eps)
  print *,g1_VV(i),(g12_V(i) - g1_V(i))/(V*eps)
  print *,g1_TV(i),(g12_T(i) - g1_T(i))/(V*eps)
  print *,g1_Vn(:,i),(g12_n(:,i) - g1_n(:,i))/(V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call rdf_at_contact_first_order_TVn(nc,T,V,n,svrm_var,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)
  print *,g1_n(1,i),(g12(i) - g1(i))/eps
  print *,g1_Tn(1,i),(g12_T(i) - g1_T(i))/eps
  print *,g1_Vn(1,i),(g12_V(i) - g1_V(i))/eps
  print *,g1_nn(1,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  !stop
  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcA1(nc,T,V,n,svrm_var,&
          g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
          a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
          a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
     call rdf_at_contact_first_order_TVn(nc,T,V,n,svrm_var,&
          g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
          g12_Tn,g12_Vn,g12_nn)
     print *,g1_n(2,i),(g12(i) - g1(i))/eps
     print *,g1_Tn(2,i),(g12_T(i) - g1_T(i))/eps
     print *,g1_Vn(2,i),(g12_V(i) - g1_V(i))/eps
     print *,g1_nn(2,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  endif

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA1(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call rdf_at_contact_first_order_TVn(nc,T+T*eps,V,n,svrm_var,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)

  print *,g1_T(i),(g12(i) - g1(i))/(T*eps)
  print *,g1_TT(i),(g12_T(i) - g1_T(i))/(T*eps)
  print *,g1_TV(i),(g12_V(i) - g1_V(i))/(T*eps)
  print *,g1_Tn(:,i),(g12_n(:,i) - g1_n(:,i))/(T*eps)

  print *,"At end"
  stop
end subroutine test_g1

subroutine test_g2(Ti,Vi,ni,doInit,qc)
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_chain
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  integer, optional, intent(in) :: qc
  ! Locals
  logical :: doInit_L, no_correction
  real :: n(nc),n0(nc),T,V,eps
  integer :: i
  real :: x0,eta,g,g_e,g_x,g_ee,g_xx,g_ex,g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex
  real, dimension(nc) :: g1,g1_T,g1_V,g1_TT,g1_VV,g1_TV
  real, dimension(nc,nc) :: g1_n,g1_Tn,g1_Vn
  real, dimension(nc,nc,nc) :: g1_nn
  real, dimension(nc) :: g12,g12_T,g12_V,g12_TT,g12_VV,g12_TV
  real, dimension(nc,nc) :: g12_n,g12_Tn,g12_Vn
  real, dimension(nc,nc,nc) :: g12_nn
  real, dimension(nc) :: g2_VVn,g2_Vn,g2p_Vn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  no_correction = .true.
  if (present(qc)) then
     quantum_correction=qc
  else
     quantum_correction=1
  endif
  if (present(doInit)) then
     doInit_L = doInit
  else
     doInit_L = .true.
  endif
  if (doInit_L) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     if (nc == 2) then
        n = (/0.2,1.2/)
     else
        n = 0.9
     endif
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif
  eps = 1.0e-5
  i = 1
  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  eta = svrm_var%zeta%zx
  x0 = saftvrmie_param%sigma_ij(i,i)/svrm_var%dhs%d(i,i)
  call calcG22MCA(x0,eta,saftvrmie_param%lambda_a_ij(i,i),&
       saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
       saftvrmie_param%Cij(i,i),g,g_e,g_x,g_ee,g_xx,g_ex)
  call calcG22MCA(x0,eta+eps,saftvrmie_param%lambda_a_ij(i,i),&
       saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
       saftvrmie_param%Cij(i,i),g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
  print *,"Testing g2(e,x)"
  print *,g
  print *,"e"
  print *,g_e,(g2 - g)/(eps)
  print *,g_ee,(g2_e - g_e)/(eps)
  print *,g_ex,(g2_x - g_x)/(eps)
  call calcG22MCA(x0+eps,eta,saftvrmie_param%lambda_a_ij(i,i),&
       saftvrmie_param%lambda_r_ij(i,i),saftvrmie_param%eps_divk_ij(i,i),&
       saftvrmie_param%Cij(i,i),g2,g2_e,g2_x,g2_ee,g2_xx,g2_ex)
  print *,"x"
  print *,g_x,(g2 - g)/(eps)
  print *,g_xx,(g2_x - g_x)/(eps)
  print *,g_ex,(g2_e - g_e)/(eps)

  ! stop
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact_second_order_TVn(nc,T,V,n,svrm_var,no_correction,&
       g1,g1_T,g1_V,g1_n,g1_TT,g1_VV,g1_TV,&
       g1_Tn,g1_Vn,g1_nn)

  g2_Vn = svrm_var%a2chij%am_Vn(:,i,i)
  g2_VVn = svrm_var%a2chij%am_VVn(:,i,i)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA2(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact_second_order_TVn(nc,T,V+V*eps,n,svrm_var,no_correction,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)
  g2p_Vn = svrm_var%a2chij%am_Vn(:,i,i)
  print *,"Testing g2"
  print *,"V"
  print *,g1(i)
  print *,g1_V(i),(g12(i) - g1(i))/(V*eps)
  print *,g1_VV(i),(g12_V(i) - g1_V(i))/(V*eps)
  print *,g1_TV(i),(g12_T(i) - g1_T(i))/(V*eps)
  print *,g1_Vn(:,i),(g12_n(:,i) - g1_n(:,i))/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact_second_order_TVn(nc,T,V,n,svrm_var,no_correction,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)
  print *,g1_n(1,i),(g12(i) - g1(i))/eps
  print *,g1_Tn(1,i),(g12_T(i) - g1_T(i))/eps
  print *,g1_Vn(1,i),(g12_V(i) - g1_V(i))/eps
  print *,g1_nn(1,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  !stop
  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcA2(nc,T,V,n,svrm_var,&
          g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
          a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
     call rdf_at_contact_second_order_TVn(nc,T,V,n,svrm_var,no_correction,&
          g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
          g12_Tn,g12_Vn,g12_nn)
     print *,g1_n(2,i),(g12(i) - g1(i))/eps
     print *,g1_Tn(2,i),(g12_T(i) - g1_T(i))/eps
     print *,g1_Vn(2,i),(g12_V(i) - g1_V(i))/eps
     print *,g1_nn(2,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  endif

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA2(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact_second_order_TVn(nc,T+T*eps,V,n,svrm_var,no_correction,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)

  print *,g1_T(i),(g12(i) - g1(i))/(T*eps)
  print *,g1_TT(i),(g12_T(i) - g1_T(i))/(T*eps)
  print *,g1_TV(i),(g12_V(i) - g1_V(i))/(T*eps)
  print *,g1_Tn(:,i),(g12_n(:,i) - g1_n(:,i))/(T*eps)

  stop
end subroutine test_g2

subroutine test_rdf_at_contact(Ti,Vi,ni,doInit,qc)
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_dispersion
  use saftvrmie_interface
  use saftvrmie_chain
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  integer, optional, intent(in) :: qc
  ! Locals
  logical :: doInit_L
  real :: n(nc),n0(nc),T,V,eps
  integer :: i
  real, dimension(nc) :: g1,g1_T,g1_V,g1_TT,g1_VV,g1_TV
  real, dimension(nc,nc) :: g1_n,g1_Tn,g1_Vn
  real, dimension(nc,nc,nc) :: g1_nn
  real, dimension(nc) :: g12,g12_T,g12_V,g12_TT,g12_VV,g12_TV
  real, dimension(nc,nc) :: g12_n,g12_Tn,g12_Vn
  real, dimension(nc,nc,nc) :: g12_nn
  real ::  a1_VVV,a1_VVT,a1_VTT
  real, dimension(nc) :: a1_VVn,a1_VTn
  real, dimension(nc,nc) :: a1_Vnn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  if (present(qc)) then
     quantum_correction=qc
  else
     quantum_correction=1
  endif
  if (present(doInit)) then
     doInit_L = doInit
  else
     doInit_L = .true.
  endif
  if (doInit_L) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     if (nc == 2) then
        n = (/0.2,1.2/)
     else
        n = 0.9
     endif
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif
  eps = 1.0e-4
  i = 1
  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact(nc,T,V,n,svrm_var,&
       g1,g1_T,g1_V,g1_n,g1_TT,g1_VV,g1_TV,&
       g1_Tn,g1_Vn,g1_nn)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA1(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact(nc,T,V+V*eps,n,svrm_var,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)
  print *,"Testing g1"
  print *,"V"
  print *,g1(i)
  print *,g1_V(i),(g12(i) - g1(i))/(V*eps)
  print *,g1_VV(i),(g12_V(i) - g1_V(i))/(V*eps)
  print *,g1_TV(i),(g12_T(i) - g1_T(i))/(V*eps)
  print *,g1_Vn(:,i),(g12_n(:,i) - g1_n(:,i))/(V*eps)

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA1(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact(nc,T+T*eps,V,n,svrm_var,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)

  print *,g1_T(i),(g12(i) - g1(i))/(T*eps)
  print *,g1_TT(i),(g12_T(i) - g1_T(i))/(T*eps)
  print *,g1_TV(i),(g12_V(i) - g1_V(i))/(T*eps)
  print *,g1_Tn(:,i),(g12_n(:,i) - g1_n(:,i))/(T*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call rdf_at_contact(nc,T,V,n,svrm_var,&
       g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
       g12_Tn,g12_Vn,g12_nn)
  print *,g1_n(1,i),(g12(i) - g1(i))/eps
  print *,g1_Tn(1,i),(g12_T(i) - g1_T(i))/eps
  print *,g1_Vn(1,i),(g12_V(i) - g1_V(i))/eps
  print *,g1_nn(1,:,i),(g12_n(:,i) - g1_n(:,i))/eps

  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
     call calcA1(nc,T,V,n,svrm_var,&
          g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
          a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
          a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
     call calcA2(nc,T,V,n,svrm_var,&
          g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
          a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
     call rdf_at_contact(nc,T,V,n,svrm_var,&
          g12,g12_T,g12_V,g12_n,g12_TT,g12_VV,g12_TV,&
          g12_Tn,g12_Vn,g12_nn)
     print *,g1_n(2,i),(g12(i) - g1(i))/eps
     print *,g1_Tn(2,i),(g12_T(i) - g1_T(i))/eps
     print *,g1_Vn(2,i),(g12_V(i) - g1_V(i))/eps
     print *,g1_nn(2,:,i),(g12_n(:,i) - g1_n(:,i))/eps
  endif

  stop
end subroutine test_rdf_at_contact

subroutine test_a_chain
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_chain
  use saftvrmie_hardsphere
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  integer :: i
  real :: ach,ach_T,ach_V,ach_TT,ach_VV,ach_TV
  real, dimension(nc) :: ach_n,ach_Tn,ach_Vn
  real, dimension(nc,nc) :: ach_nn
  real :: ach2,ach2_T,ach2_V,ach2_TT,ach2_VV,ach2_TV
  real, dimension(nc) :: ach2_n,ach2_Tn,ach2_Vn
  real, dimension(nc,nc) :: ach2_nn
  real ::  a1_VVV,a1_VVT,a1_VTT
  real, dimension(nc) :: a1_VVn,a1_VTn
  real, dimension(nc,nc) :: a1_Vnn
  real, dimension(nc) :: g12,g12_T,g12_V,g12_TT,g12_VV,g12_TV
  real, dimension(nc,nc) :: g12_n,g12_Tn,g12_Vn
  real, dimension(nc,nc,nc) :: g12_nn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()

  quantum_correction=1
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  i = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V,n,svrm_var,ach,ach_T,ach_V,ach_n,ach_TT,&
       ach_VV,ach_TV,ach_Tn,ach_Vn,ach_nn)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA1(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V+V*eps,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn)

  print *,"Testing a chain"
  print *,"V"
  print *,ach
  print *,ach_V,(ach2 - ach)/(V*eps)
  print *,ach_VV,(ach2_V - ach_V)/(V*eps)
  print *,ach_TV,(ach2_T - ach_T)/(V*eps)
  print *,ach_Vn,(ach2_n - ach_n)/(V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn)
  print *,ach_n(1),(ach2 - ach)/eps
  print *,ach_Tn(1),(ach2_T - ach_T)/eps
  print *,ach_Vn(1),(ach2_V - ach_V)/eps
  print *,ach_nn(1,:),(ach2_n - ach_n)/eps
  !stop
  print *,"n2"
  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn)
  print *,ach_n(2),(ach2 - ach)/eps
  print *,ach_Tn(2),(ach2_T - ach_T)/eps
  print *,ach_Vn(2),(ach2_V - ach_V)/eps
  print *,ach_nn(2,:),(ach2_n - ach_n)/eps

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA1(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T+T*eps,V,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn)

  print *,ach_T,(ach2 - ach)/(T*eps)
  print *,ach_TT,(ach2_T - ach_T)/(T*eps)
  print *,ach_TV,(ach2_V - ach_V)/(T*eps)
  print *,ach_Tn,(ach2_n - ach_n)/(T*eps)

  stop
end subroutine test_a_chain

subroutine test_a_chain_pure(Ti,Vi,ni,doInit,qc)
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_chain
  use saftvrmie_hardsphere
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  integer, optional, intent(in) :: qc
  ! Locals
  real :: n(nc),n0(nc),T,V,eps
  integer :: i
  logical :: doInit_L, return_F
  real :: ach,ach_T,ach_V,ach_TT,ach_VV,ach_TV
  real, dimension(nc) :: ach_n,ach_Tn,ach_Vn
  real, dimension(nc,nc) :: ach_nn
  real :: ach2,ach2_T,ach2_V,ach2_TT,ach2_VV,ach2_TV
  real, dimension(nc) :: ach2_n,ach2_Tn,ach2_Vn
  real, dimension(nc,nc) :: ach2_nn
  real ::  a1_VVV,a1_VVT,a1_VTT
  real, dimension(nc) :: a1_VVn,a1_VTn
  real, dimension(nc,nc) :: a1_Vnn
  real, dimension(nc) :: g12,g12_T,g12_V,g12_TT,g12_VV,g12_TV
  real, dimension(nc,nc) :: g12_n,g12_Tn,g12_Vn
  real, dimension(nc,nc,nc) :: g12_nn
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  return_F = .true.
  if (present(qc)) then
     quantum_correction=qc
  else
     quantum_correction=1
  endif
  if (present(doInit)) then
     doInit_L = doInit
  else
     doInit_L = .true.
  endif
  if (doInit_L) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     n = (/0.9/)
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif
  eps = 1.0e-8
  i = 1
  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V,n,svrm_var,ach,ach_T,ach_V,ach_n,ach_TT,&
       ach_VV,ach_TV,ach_Tn,ach_Vn,ach_nn,returnF=return_F)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calcA1(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V+V*eps,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V+V*eps,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn,returnF=return_F)

  print *,"Testing a chain"
  print *,"V"
  print *,ach
  print *,ach_V,(ach2 - ach)/(V*eps)
  print *,ach_VV,(ach2_V - ach_V)/(V*eps)
  print *,ach_TV,(ach2_T - ach_T)/(V*eps)
  print *,ach_Vn,(ach2_n - ach_n)/(V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T,V,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn,returnF=return_F)
  print *,ach_n(1),(ach2 - ach)/eps
  print *,ach_Tn(1),(ach2_T - ach_T)/eps
  print *,ach_Vn(1),(ach2_V - ach_V)/eps
  print *,ach_nn(1,:),(ach2_n - ach_n)/eps
  !stop
  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calcA1(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a1_T=g12_T(i),a1_V=g12_V(i),a1_n=g12_n(:,i),a1_TT=g12_TT(i),a1_VV=g12_VV(i),&
       a1_TV=g12_TV(i),a1_Tn=g12_Tn(:,i),a1_Vn=g12_Vn(:,i),a1_nn=g12_nn(:,:,i),&
       a1_VVV=a1_VVV,a1_VVT=a1_VVT,a1_VTT=a1_VTT,a1_VVn=a1_VVn,a1_Vnn=a1_Vnn,a1_VTn=a1_VTn)
  call calcA2(nc,T+T*eps,V,n,svrm_var,&
       g12(i),a2_T=g12_T(i),a2_V=g12_V(i),a2_n=g12_n(:,i),a2_TT=g12_TT(i),a2_VV=g12_VV(i),&
       a2_TV=g12_TV(i),a2_Tn=g12_Tn(:,i),a2_Vn=g12_Vn(:,i),a2_nn=g12_nn(:,:,i))
  call calcAchain(nc,T+T*eps,V,n,svrm_var,ach2,ach2_T,ach2_V,ach2_n,ach2_TT,&
       ach2_VV,ach2_TV,ach2_Tn,ach2_Vn,ach2_nn,returnF=return_F)

  print *,ach_T,(ach2 - ach)/(T*eps)
  print *,ach_TT,(ach2_T - ach_T)/(T*eps)
  print *,ach_TV,(ach2_V - ach_V)/(T*eps)
  print *,ach_Tn,(ach2_n - ach_n)/(T*eps)

  stop
end subroutine test_a_chain_pure

subroutine test_fres(Ti,Vi,ni,doInit)
  use thermopack_constants

  use saftvrmie_containers
  use saftvrmie_hardsphere
  use saftvrmie_interface
  use saftvrmie_options
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  ! Locals
  real :: n(nc),n0(nc),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(nc) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(nc,nc) :: Fp_nn
  logical :: call_init
  type(thermo_model), pointer :: act_mod_ptr
  class(saftvrmie_eos), pointer :: eos
  act_mod_ptr => get_active_thermo_model()
  eos => get_saftvrmie_eos_pointer(act_mod_ptr%eos(1)%p_eos)
  !enable_chain = .false.
  quantum_correction=0
  if (present(doInit)) then
     call_init = doInit
  else
     call_init = .true.
  endif
  if (call_init) then
    call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     if (nc == 2) then
        n = (/0.2,1.2/)
     else
        n = 0.9
     endif
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif

  eps = 1.0e-8

  call calcFresSAFTVRMie(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcFresSAFTVRMie(eos,nc,T,V+V*eps,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - F)/(V*eps)
  print *,F_VV,(Fp_V - F_V)/(V*eps)
  print *,F_TV,(Fp_T - F_T)/(V*eps)
  print *,F_Vn,(Fp_n - F_n)/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call calcFresSAFTVRMie(eos,nc,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  print *,F_n(1),(Fp - F)/eps
  print *,F_Tn(1),(Fp_T - F_T)/eps
  print *,F_Vn(1),(Fp_V - F_V)/eps
  print *,F_nn(1,:),(Fp_n - F_n)/eps
  !stop
  if (nc > 1) then
     print *,"n2"
     n = n0
     n(2) = n(2) + eps
     call calcFresSAFTVRMie(eos,nc,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
          Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
     print *,F_n(2),(Fp - F)/eps
     print *,F_Tn(2),(Fp_T - F_T)/eps
     print *,F_Vn(2),(Fp_V - F_V)/eps
     print *,F_nn(2,:),(Fp_n - F_n)/eps
  endif

  print *,"T"
  n = n0
  call calcFresSAFTVRMie(eos,nc,T+T*eps,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,F_T,(Fp - F)/(T*eps)
  print *,F_TT,(Fp_T - F_T)/(T*eps)
  print *,F_TV,(Fp_V - F_V)/(T*eps)
  print *,F_Tn,(Fp_n - F_n)/(T*eps)

  stop
end subroutine test_fres

subroutine test_a1_quantum_corrections()
  use thermopack_constants

  use saftvrmie_containers
  use saftvrmie_dispersion
  use saftvrmie_hardsphere
  use saftvrmie_interface
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real :: n(nc),n0(nc),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(nc) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(nc,nc) :: Fp_nn
  real :: Fm,Fm_T,Fm_V,Fm_TT,Fm_VV,Fm_TV
  real, dimension(nc) :: Fm_n,Fm_Tn,Fm_Vn
  real, dimension(nc,nc) :: Fm_nn

  real :: x0,eta,lambda_a,lambda_r,epsEnergy,C, Q1_r,Q1_a, Q2_r,Q2_a
  real :: a1Q,a1Q_e,a1Q_x,a1Q_ee,a1Q_xx,a1Q_ex, a1Q_eee,a1Q_eex,a1Q_exx
  real :: a1Qp,a1Qp_e,a1Qp_x,a1Qp_ee,a1Qp_xx,a1Qp_ex, a1Qp_eee,a1Qp_eex,a1Qp_exx
  real :: a1Qm,a1Qm_e,a1Qm_x,a1Qm_ee,a1Qm_xx,a1Qm_ex, a1Qm_eee,a1Qm_eex,a1Qm_exx
  real :: a1qq,a1qq_e,a1qq_x,a1qq_ee,a1qq_xx,a1qq_ex, a1qq_eee,a1qq_eex,a1qq_exx
  real :: a1qqp,a1qqp_e,a1qqp_x,a1qqp_ee,a1qqp_xx,a1qqp_ex, a1qqp_eee,a1qqp_eex,a1qqp_exx
  real :: a1qqm,a1qqm_e,a1qqm_x,a1qqm_ee,a1qqm_xx,a1qqm_ex, a1qqm_eee,a1qqm_eex,a1qqm_exx
  type(thermo_model), pointer :: act_mod_ptr
  class(saftvrmie_eos), pointer :: eos
  act_mod_ptr => get_active_thermo_model()
  eos => get_saftvrmie_eos_pointer(act_mod_ptr%eos(1)%p_eos)

  quantum_correction = 2
  quantum_correction_hs = 0
  quantum_correction_spec = 0
  call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  n = (/0.4,0.8/) ! He, Ne mixture
  n0 = n
  V = 4.0e-4
  T = 10.15
  eps = 1.0e-5

  x0 = 1.2015127412131341
  eta=7.4122074689595349E-002
  lambda_a=6.0000000000000000
  lambda_r=14.840000000000000
  epsEnergy=4.4400000000000004
  C=3.1039560381846205

  Q1_r = 205.38559999999998
  Q1_a = 30.000000000000000
  Q2_r = 8580.0000000000000
  Q2_a = 840.00000000000000

  print *, "TEST A1QTILDE"
  ! call calcFresSAFTVRMie(eos,nc,T,V,n0,F,F_T,F_V,F_n,F_TT,&
  !      F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcA1QTilde(x0,eta,lambda_a,lambda_r,epsEnergy,C,&
       Q1_r,Q1_a,a1Q,a1Q_e,a1Q_x,a1Q_ee,a1Q_xx,a1Q_ex,&
       a1Q_eee,a1Q_eex,a1Q_exx)
  call calcA1QTilde(x0*(1+eps),eta,lambda_a,lambda_r,epsEnergy,C,&
       Q1_r,Q1_a,a1Qp,a1Qp_e,a1Qp_x,a1Qp_ee,a1Qp_xx,a1Qp_ex,&
       a1Qp_eee,a1Qp_eex,a1Qp_exx)
  call calcA1QTilde(x0*(1-eps),eta,lambda_a,lambda_r,epsEnergy,C,&
       Q1_r,Q1_a,a1Qm,a1Qm_e,a1Qm_x,a1Qm_ee,a1Qm_xx,a1Qm_ex,&
       a1Qm_eee,a1Qm_eex,a1Qm_exx)
  print *, "x0"
  print *,a1Q_x, (a1qp-a1qm)/(2*x0*eps)
  print *,a1Q_ex,(a1qp_e - a1qm_e)/(2*x0*eps)
  print *,a1Q_xx,(a1qp_x - a1qm_x)/(2*x0*eps)
  print *,a1Q_eex,(a1qp_ee - a1qm_ee)/(2*x0*eps)
  print *,a1Q_exx,(a1qp_ex - a1qm_ex)/(2*x0*eps)

  call calcA1QTilde(x0,eta*(1+eps),lambda_a,lambda_r,epsEnergy,C,&
       Q1_r,Q1_a,a1Qp,a1Qp_e,a1Qp_x,a1Qp_ee,a1Qp_xx,a1Qp_ex,&
       a1Qp_eee,a1Qp_eex,a1Qp_exx)
  call calcA1QTilde(x0,eta*(1-eps),lambda_a,lambda_r,epsEnergy,C,&
       Q1_r,Q1_a,a1Qm,a1Qm_e,a1Qm_x,a1Qm_ee,a1Qm_xx,a1Qm_ex,&
       a1Qm_eee,a1Qm_eex,a1Qm_exx)
  print *, "eta"
  print *,a1Q_e, (a1qp-a1qm)/(2*eta*eps)
  print *,a1Q_ex,(a1qp_x - a1qm_x)/(2*eta*eps)
  print *,a1Q_ee,(a1qp_e - a1qm_e)/(2*eta*eps)
  print *,a1Q_eex,(a1qp_ex - a1qm_ex)/(2*eta*eps)
  print *,a1Q_exx,(a1qp_xx - a1qm_xx)/(2*eta*eps)
  print *,a1Q_eee,(a1qp_ee - a1qm_ee)/(2*eta*eps)

  print *, "TEST A1QQTILDE"
  call calcA1QQTilde(x0,eta,lambda_a,lambda_r,epsEnergy,C,&
       Q2_r,Q2_a,a1qq,a1qq_e,a1qq_x,a1qq_ee,a1qq_xx,a1qq_ex,&
       a1qq_eee,a1qq_eex,a1qq_exx)
  call calcA1QQTilde(x0*(1+eps),eta,lambda_a,lambda_r,epsEnergy,C,&
       Q2_r,Q2_a,a1qqp,a1qqp_e,a1qqp_x,a1qqp_ee,a1qqp_xx,a1qqp_ex,&
       a1qqp_eee,a1qqp_eex,a1qqp_exx)
  call calcA1QQTilde(x0*(1-eps),eta,lambda_a,lambda_r,epsEnergy,C,&
       Q2_r,Q2_a,a1qqm,a1qqm_e,a1qqm_x,a1qqm_ee,a1qqm_xx,a1qqm_ex,&
       a1qqm_eee,a1qqm_eex,a1qqm_exx)

  print *, "x0"
  print *,a1qq_x, (a1qqp-a1qqm)/(2*x0*eps)
  print *,a1qq_ex,(a1qqp_e - a1qqm_e)/(2*x0*eps)
  print *,a1qq_xx,(a1qqp_x - a1qqm_x)/(2*x0*eps)
  print *,a1qq_eex,(a1qqp_ee - a1qqm_ee)/(2*x0*eps)
  print *,a1qq_exx,(a1qqp_ex - a1qqm_ex)/(2*x0*eps)

  call calcA1QQTilde(x0,eta*(1+eps),lambda_a,lambda_r,epsEnergy,C,&
       Q2_r,Q2_a,a1qqp,a1qqp_e,a1qqp_x,a1qqp_ee,a1qqp_xx,a1qqp_ex,&
       a1qqp_eee,a1qqp_eex,a1qqp_exx)
  call calcA1QQTilde(x0,eta*(1-eps),lambda_a,lambda_r,epsEnergy,C,&
       Q2_r,Q2_a,a1qqm,a1qqm_e,a1qqm_x,a1qqm_ee,a1qqm_xx,a1qqm_ex,&
       a1qqm_eee,a1qqm_eex,a1qqm_exx)
  print *, "eta"
  print *,a1qq_e, (a1qqp-a1qqm)/(2*eta*eps)
  print *,a1qq_ex,(a1qqp_x - a1qqm_x)/(2*eta*eps)
  print *,a1qq_ee,(a1qqp_e - a1qqm_e)/(2*eta*eps)
  print *,a1qq_eex,(a1qqp_ex - a1qqm_ex)/(2*eta*eps)
  print *,a1qq_exx,(a1qqp_xx - a1qqm_xx)/(2*eta*eps)
  print *,a1qq_eee,(a1qqp_ee - a1qqm_ee)/(2*eta*eps)

  ! TEST Fres
  call calcFresSAFTVRMie(eos,nc,T,V,n0,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calcFresSAFTVRMie(eos,nc,T,V+V*eps,n0,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calcFresSAFTVRMie(eos,nc,T,V-V*eps,n0,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *, "TEST FRES"
  print *,"V"
  print *,F
  print *,F_V,(Fp-Fm)/(2*V*eps)
  print *,F_VV,(Fp_V - Fm_V)/(2*V*eps)
  print *,F_TV,(Fp_T - Fm_T)/(2*V*eps)
  print *,F_Vn,(Fp_n - Fm_n)/(2*V*eps)
  !stop
  print *,"n1"
  n(1) = n0(1) + eps
  call calcFresSAFTVRMie(eos,nc,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  n(1) = n0(1) - eps
  call calcFresSAFTVRMie(eos,nc,T,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  print *,F_n(1),(Fp - Fm)/(2*eps)
  print *,F_Tn(1),(Fp_T - Fm_T)/eps/2
  print *,F_Vn(1),(Fp_V - Fm_V)/eps/2
  print *,F_nn(1,:),(Fp_n - Fm_n)/eps/2
  !stop
  print *,"n2"
  n = n0
  n(2) = n0(2) + eps
  call calcFresSAFTVRMie(eos,nc,T,V,n,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  n(2) = n0(2) - eps
  call calcFresSAFTVRMie(eos,nc,T,V,n,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)
  print *,F_n(2),(Fp - Fm)/eps/2
  print *,F_Tn(2),(Fp_T - Fm_T)/eps/2
  print *,F_Vn(2),(Fp_V - Fm_V)/eps/2
  print *,F_nn(2,:),(Fp_n - Fm_n)/eps/2

  print *,"T"
  n = n0
  call calcFresSAFTVRMie(eos,nc,T+T*eps,V,n0,Fp,Fp_T,Fp_V,Fp_n,Fp_TT,&
       Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  call calcFresSAFTVRMie(eos,nc,T-T*eps,V,n0,Fm,Fm_T,Fm_V,Fm_n,Fm_TT,&
       Fm_VV,Fm_TV,Fm_Tn,Fm_Vn,Fm_nn)

  print *,F_T,(Fp - Fm)/(2*T*eps)
  print *,F_TT,(Fp_T - Fm_T)/(2*T*eps)
  print *,F_TV,(Fp_V - Fm_V)/(2*T*eps)
  print *,F_Tn,(Fp_n - Fm_n)/(2*T*eps)

  stop
end subroutine test_a1_quantum_corrections

subroutine test_IC
  use thermopack_constants
  use thermopack_var
  use saftvrmie_containers
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_chain
  implicit none
  real :: n(nc),n0(nc),T,V,eps,lambda_a,lambda_r,epsilon,C
  integer :: i, j
  real :: x0,x0_T,x0_TT,eta,d,d_T,d_TT,s,s_T,s_TT,y,y_T,y_TT
  real :: a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT
  real :: a1Cc2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT
  real :: a1C_T,a1C_V,a1C_TT,a1C_VV,a1C_TV,a1C_VVV,a1C_VVT,a1C_VTT
  real, dimension(nc) :: a1C_n,a1C_Tn,a1C_Vn,a1C_VVn,a1C_VTn
  real, dimension(nc,nc) :: a1C_nn,a1C_Vnn
  real :: a1C2,a1C2_T,a1C2_V,a1C2_TT,a1C2_VV,a1C2_TV,a1C2_VVV,a1C2_VVT,a1C2_VTT
  real, dimension(nc) :: a1C2_n,a1C2_Tn,a1C2_Vn,a1C2_VVn,a1C2_VTn
  real, dimension(nc,nc) :: a1C2_nn,a1C2_Vnn
  real :: Q1_r, Q1_a
  type(saftvrmie_var_container), pointer :: svrm_var
  Q1_r = 100000
  Q1_a = 1
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  i = 1
  j = 1
  quantum_correction_hs = 2
  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)

  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
  s = saftvrmie_param%sigma_ij(i,j)
  call calcXDifferentials(s,0.0,0.0,d,d_T,d_TT,x0,x0_T,x0_TT)
  eta = svrm_var%zeta%zx
  lambda_a = saftvrmie_param%lambda_a_ij(i,j)
  lambda_r = saftvrmie_param%lambda_r_ij(i,j)
  epsilon = saftvrmie_param%eps_divk_ij(i,j)
  C = saftvrmie_param%Cij(i,j)
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,epsilon,C,&
       a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
       fac_r_in=Q1_r,fac_a_in=Q1_a)
  call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,svrm_var%zeta,&
       a1Cc,a1Cc_e,a1Cc_T,a1Cc_ee,a1Cc_TT,a1Cc_eT,a1Cc_eee,a1Cc_eeT,a1Cc_eTT,&
       a1C_T,a1C_V,a1C_n,a1C_TT,a1C_VV,a1C_TV,a1C_Tn,a1C_Vn,a1C_nn,&
       a1C_VVV,a1C_VVT,a1C_VTT,a1C_VVn,a1C_Vnn,a1C_VTn,difflevel=3)
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta+eps,lambda_a,lambda_r,epsilon,C,&
       a1Cc2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,&
       fac_r_in=Q1_r,fac_a_in=Q1_a)
  print *,"Testing integration form sigma to sigma_eff"
  print *,"eta"
  print *,a1Cc
  print *,a1Cc_e,(a1Cc2-a1Cc)/eps
  print *,a1Cc_ee,(a1Cc2_e-a1Cc_e)/eps
  print *,a1Cc_eT,(a1Cc2_T-a1Cc_T)/eps
  print *,a1Cc_eeT,(a1Cc2_eT-a1Cc_eT)/eps
  print *,a1Cc_eee,(a1Cc2_ee-a1Cc_ee)/eps

  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
  s = saftvrmie_param%sigma_ij(i,j)
  call calcXDifferentials(s,0.0,0.0,d,d_T,d_TT,x0,x0_T,x0_TT)
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,epsilon,C,&
       a1Cc2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,&
       fac_r_in=Q1_r,fac_a_in=Q1_a)
  print *,"T"
  print *,a1Cc_T,(a1Cc2-a1Cc)/(T*eps)
  print *,a1Cc_TT,(a1Cc2_T-a1Cc_T)/(T*eps)
  print *,a1Cc_eT,(a1Cc2_e-a1Cc_e)/(T*eps)
  print *,a1Cc_eTT,(a1Cc2_eT-a1Cc_eT)/(T*eps)
  print *,a1Cc_eeT,(a1Cc2_ee-a1Cc_ee)/(T*eps)
  stop

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
  s = saftvrmie_param%sigma_ij(i,j)
  call calcXDifferentials(s,0.0,0.0,d,d_T,d_TT,x0,x0_T,x0_TT)
  eta = svrm_var%zeta%zx
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,epsilon,C,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,fac_r_in=Q1_r,fac_a_in=Q1_a)
  call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,svrm_var%zeta,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,&
       a1C2_T,a1C2_V,a1C2_n,a1C2_TT,a1C2_VV,a1C2_TV,a1C2_Tn,a1C2_Vn,a1C2_nn,&
       a1C2_VVV,a1C2_VVT,a1C2_VTT,a1C2_VVn,a1C2_Vnn,a1C2_VTn,difflevel=3)

  print *,"V"
  print *,a1Cc
  print *,a1C_V,(a1C2 - a1Cc)/(V*eps)
  print *,a1C_VV,(a1C2_V - a1C_V)/(V*eps)
  print *,a1C_TV,(a1C2_T - a1C_T)/(V*eps)
  print *,a1C_Vn,(a1C2_n - a1C_n)/(V*eps)
  print *,a1C_VVV,(a1C2_VV - a1C_VV)/(V*eps)
  print *,a1C_VVT,(a1C2_TV - a1C_TV)/(V*eps)
  print *,a1C_VTT,(a1C2_TT - a1C_TT)/(V*eps)
  print *,a1C_VTn,(a1C2_Tn - a1C_Tn)/(V*eps)
  print *,a1C_VVn,(a1C2_Vn - a1C_Vn)/(V*eps)
  print *,a1C_Vnn(1,:),(a1C2_nn(1,:) - a1C_nn(1,:))/(V*eps)
  print *,a1C_Vnn(2,:),(a1C2_nn(2,:) - a1C_nn(2,:))/(V*eps)

  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
  s = saftvrmie_param%sigma_ij(i,j)
  call calcXDifferentials(s,0.0,0.0,d,d_T,d_TT,x0,x0_T,x0_TT)
  eta = svrm_var%zeta%zx
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,epsilon,C,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,fac_r_in=Q1_r,fac_a_in=Q1_a)
  call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,svrm_var%zeta,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,&
       a1C2_T,a1C2_V,a1C2_n,a1C2_TT,a1C2_VV,a1C2_TV,a1C2_Tn,a1C2_Vn,a1C2_nn,&
       a1C2_VVV,a1C2_VVT,a1C2_VTT,a1C2_VVn,a1C2_Vnn,a1C2_VTn,difflevel=3)

  print *,"T"
  print *,a1C_T,(a1C2 - a1Cc)/(T*eps)
  print *,a1C_TT,(a1C2_T - a1C_T)/(T*eps)
  print *,a1C_TV,(a1C2_V - a1C_V)/(T*eps)
  print *,a1C_Tn,(a1C2_n - a1C_n)/(T*eps)
  print *,a1C_VTT,(a1C2_TV - a1C_TV)/(T*eps)
  print *,a1C_VVT,(a1C2_VV - a1C_VV)/(T*eps)
  print *,a1C_VTn,(a1C2_Vn - a1C_Vn)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
  s = saftvrmie_param%sigma_ij(i,j)
  call calcXDifferentials(s,0.0,0.0,d,d_T,d_TT,x0,x0_T,x0_TT)
  eta = svrm_var%zeta%zx
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,epsilon,C,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,fac_r_in=Q1_r,fac_a_in=Q1_a)
  call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,svrm_var%zeta,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,&
       a1C2_T,a1C2_V,a1C2_n,a1C2_TT,a1C2_VV,a1C2_TV,a1C2_Tn,a1C2_Vn,a1C2_nn,&
       a1C2_VVV,a1C2_VVT,a1C2_VTT,a1C2_VVn,a1C2_Vnn,a1C2_VTn,difflevel=3)

  print *,"n(1)"
  print *,a1C_n(1),(a1C2 - a1Cc)/(eps)
  print *,a1C_Tn(1),(a1C2_T - a1C_T)/(eps)
  print *,a1C_Vn(1),(a1C2_V - a1C_V)/(eps)
  print *,a1C_VVn(1),(a1C2_VV - a1C_VV)/(eps)
  print *,a1C_VTn(1),(a1C2_TV - a1C_TV)/(eps)
  print *,a1C_nn(1,:),(a1C2_n - a1C_n)/(eps)
  print *,a1C_Vnn(1,:),(a1C2_Vn - a1C_Vn)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  d = svrm_var%dhs%d(i,j)
  d_T = svrm_var%dhs%d_T(i,j)
  d_TT = svrm_var%dhs%d_TT(i,j)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentials(s,s_T,s_TT,d,d_T,d_TT,y,y_T,y_TT)
  s = saftvrmie_param%sigma_ij(i,j)
  call calcXDifferentials(s,0.0,0.0,d,d_T,d_TT,x0,x0_T,x0_TT)
  eta = svrm_var%zeta%zx
  call calcIcTilde(x0,x0_T,x0_TT,y,y_T,y_TT,eta,lambda_a,lambda_r,epsilon,C,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,fac_r_in=Q1_r,fac_a_in=Q1_a)
  call convert_zeta_x_to_TVn(nc,0.0,1.0,0.0,svrm_var%zeta,&
       a1C2,a1Cc2_e,a1Cc2_T,a1Cc2_ee,a1Cc2_TT,a1Cc2_eT,a1Cc2_eee,a1Cc2_eeT,a1Cc2_eTT,&
       a1C2_T,a1C2_V,a1C2_n,a1C2_TT,a1C2_VV,a1C2_TV,a1C2_Tn,a1C2_Vn,a1C2_nn,&
       a1C2_VVV,a1C2_VVT,a1C2_VTT,a1C2_VVn,a1C2_Vnn,a1C2_VTn,difflevel=3)

  print *,"n(2)"
  print *,a1C_n(2),(a1C2 - a1Cc)/(eps)
  print *,a1C_Tn(2),(a1C2_T - a1C_T)/(eps)
  print *,a1C_Vn(2),(a1C2_V - a1C_V)/(eps)
  print *,a1C_VVn(2),(a1C2_VV - a1C_VV)/(eps)
  print *,a1C_VTn(2),(a1C2_TV - a1C_TV)/(eps)
  print *,a1C_nn(2,:),(a1C2_n - a1C_n)/(eps)
  print *,a1C_Vnn(2,:),(a1C2_Vn - a1C_Vn)/(eps)

  stop
end subroutine test_IC

subroutine test_hard_sphere_diameter
  use thermopack_constants
  use saftvrmie_containers, only: get_saftvrmie_var, saftvrmie_var_container
  use saftvrmie_interface, only: preCalcSAFTVRMie
  use saftvrmie_options
  use thermopack_var
  implicit none
  real :: n(nc),T,V,eps
  integer :: i, j
  real, dimension(nc,nc) :: d,d_T,d_TT
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()
  n = (/0.2,1.2/)
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-8
  quantum_correction_hs = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  d = svrm_var%dhs%d
  d_T = svrm_var%dhs%d_T
  d_TT = svrm_var%dhs%d_TT
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  print *,"T"
  do i=1,nc
     do j=1,nc
        print *,"i,j,d: ",i,j,d(i,j)
        print *,d_T(i,j),(svrm_var%dhs%d(i,j)-d(i,j))/(T*eps)
        print *,d_TT(i,j),(svrm_var%dhs%d_T(i,j)-d_T(i,j))/(T*eps)
     enddo
  enddo
  stop
end subroutine test_hard_sphere_diameter

subroutine test_hard_sphere_Santos
  use thermopack_constants
  use saftvrmie_containers, only: get_saftvrmie_var, saftvrmie_var_container, &
       allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta, saftvrmie_zeta
  use saftvrmie_interface, only: preCalcSAFTVRMie
  use saftvrmie_hardsphere, only: calc_Santos_eta, calc_ahs_div_nRT_Santos_part, &
       calc_ahs_div_nRT_CSK, calc_ahs_div_nRT_CS, calc_F11_Santos, calc_pure_ahs_div_nRT, &
       calc_hardsphere_virial_B2, calc_hardsphere_virial_B3, calc_hardsphere_virial_Bijk, &
       calc_Santos_F12_or_F22, calc_hardsphere_helmholtzenergy_santos
  use saftvrmie_options
  use thermopack_var
  implicit none
  real :: n(nc),T,V,eps,n0(nc),eta
  real :: ahs,ahs_e,ahs_ee,ahs2,ahs2_e,ahs2_ee
  integer :: i, j, k, difflevel
  type(saftvrmie_zeta) :: eta_hs_l
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  real :: F2,F2_T,F2_V
  real, dimension(nc) :: F2_n
  real, dimension(nc,nc,nc) :: B_ijk, B_ijk_T, B_ijk_TT
  real, dimension(nc,nc,nc) :: B2_ijk, B2_ijk_T, B2_ijk_TT
  real :: B2s,B2s_T,B2s_TT,B2s_Tn(nc),B2s_n(nc),B2s_nn(nc,nc)
  real :: B2s2,B2s2_T,B2s2_TT,B2s2_Tn(nc),B2s2_n(nc),B2s2_nn(nc,nc)
  real :: B3s,B3s_T,B3s_TT,B3s_Tn(nc),B3s_n(nc),B3s_nn(nc,nc)
  real :: B3s2,B3s2_T,B3s2_TT,B3s2_Tn(nc),B3s2_n(nc),B3s2_nn(nc,nc)
  real :: aP1, aP2
  real, target :: aL1_T,aL1_V,aL1_n(nc)
  real, target :: aL1_VV,aL1_TV,aL1_Vn(nc)
  real, target :: aL1_TT,aL1_Tn(nc),aL1_nn(nc,nc)
  real, pointer :: aP1_T,aP1_V,aP1_n(:)
  real, pointer :: aP1_VV,aP1_TV,aP1_Vn(:)
  real, pointer :: aP1_TT,aP1_Tn(:),aP1_nn(:,:)
  real, target :: aL2_T,aL2_V,aL2_n(nc)
  real, target :: aL2_VV,aL2_TV,aL2_Vn(nc)
  real, target :: aL2_TT,aL2_Tn(nc),aL2_nn(nc,nc)
  real, pointer :: aP2_T,aP2_V,aP2_n(:)
  real, pointer :: aP2_VV,aP2_TV,aP2_Vn(:)
  real, pointer :: aP2_TT,aP2_Tn(:),aP2_nn(:,:)
  real :: aP1d, aP2d
  real, target :: aL1d_T,aL1d_V,aL1d_n(nc)
  real, target :: aL1d_VV,aL1d_TV,aL1d_Vn(nc)
  real, target :: aL1d_TT,aL1d_Tn(nc),aL1d_nn(nc,nc)
  real, pointer :: aP1d_T,aP1d_V,aP1d_n(:)
  real, pointer :: aP1d_VV,aP1d_TV,aP1d_Vn(:)
  real, pointer :: aP1d_TT,aP1d_Tn(:),aP1d_nn(:,:)
  real, target :: aL2d_T,aL2d_V,aL2d_n(nc)
  real, target :: aL2d_VV,aL2d_TV,aL2d_Vn(nc)
  real, target :: aL2d_TT,aL2d_Tn(nc),aL2d_nn(nc,nc)
  real, pointer :: aP2d_T,aP2d_V,aP2d_n(:)
  real, pointer :: aP2d_VV,aP2d_TV,aP2d_Vn(:)
  real, pointer :: aP2d_TT,aP2d_Tn(:),aP2d_nn(:,:)
  real :: a
  real :: a_T,a_V,a_n(nc)
  real :: a_VV,a_TV,a_Vn(nc)
  real :: a_TT,a_Tn(nc),a_nn(nc,nc)
  real :: a2
  real :: a2_T,a2_V,a2_n(nc)
  real :: a2_VV,a2_TV,a2_Vn(nc)
  real :: a2_TT,a2_Tn(nc),a2_nn(nc,nc)
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()

  call allocate_saftvrmie_zeta(nc,eta_hs_l)
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-6
  difflevel = 2
  quantum_correction_hs = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)


  eta = svrm_var%eta_hs%zx
  call calc_ahs_div_nRT_Santos_part(eta,ahs,ahs_e,ahs_ee)
  call calc_ahs_div_nRT_Santos_part(eta+eps*eta,ahs2,ahs2_e,ahs2_ee)
  print *,"ahs_div_nRT"
  print *,ahs
  print *,ahs_e,(ahs2-ahs)/(eps*eta)
  print *,ahs_ee,(ahs2_e-ahs_e)/(eps*eta)

  call calc_ahs_div_nRT_CSK(eta,ahs,ahs_e,ahs_ee)
  call calc_ahs_div_nRT_CSK(eta+eps*eta,ahs2,ahs2_e,ahs2_ee)
  print *,"ahs_div_nRT_CSK"
  print *,ahs
  print *,ahs_e,(ahs2-ahs)/(eps*eta)
  print *,ahs_ee,(ahs2_e-ahs_e)/(eps*eta)

  call calc_ahs_div_nRT_CS(eta,ahs,ahs_e,ahs_ee)
  call calc_ahs_div_nRT_CS(eta+eps*eta,ahs2,ahs2_e,ahs2_ee)
  print *,"ahs_div_nRT_CS"
  print *,ahs
  print *,ahs_e,(ahs2-ahs)/(eps*eta)
  print *,ahs_ee,(ahs2_e-ahs_e)/(eps*eta)

  call calc_Santos_eta(nc,n,V+V*eps,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"Testing hypotetical pure fluid packing fraction"
  print *,"Volume"
  print *,svrm_var%eta_hs%zx
  print *,svrm_var%eta_hs%zx_V,(eta_hs_l%zx - svrm_var%eta_hs%zx)/(V*eps)
  print *,svrm_var%eta_hs%zx_VV,(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/(V*eps)
  print *,svrm_var%eta_hs%zx_TV,(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/(V*eps)
  print *,svrm_var%eta_hs%zx_Vn,(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/(V*eps)

  n(1) = n(1) + eps
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"n(1)"
  print *,svrm_var%eta_hs%zx_n(1),(eta_hs_l%zx - svrm_var%eta_hs%zx)/eps
  print *,svrm_var%eta_hs%zx_Tn(1),(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/eps
  print *,svrm_var%eta_hs%zx_Vn(1),(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/eps
  print *,svrm_var%eta_hs%zx_nn(1,:),(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/eps

  n = n0
  n(2) = n(2) + eps
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"n(2)"
  print *,svrm_var%eta_hs%zx_n(2),(eta_hs_l%zx - svrm_var%eta_hs%zx)/eps
  print *,svrm_var%eta_hs%zx_Tn(2),(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/eps
  print *,svrm_var%eta_hs%zx_Vn(2),(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/eps
  print *,svrm_var%eta_hs%zx_nn(2,:),(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/eps

  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"Temperature"
  print *,svrm_var%eta_hs%zx_T,(eta_hs_l%zx - svrm_var%eta_hs%zx)/(T*eps)
  print *,svrm_var%eta_hs%zx_TT,(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/(T*eps)
  print *,svrm_var%eta_hs%zx_TV,(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/(T*eps)
  print *,svrm_var%eta_hs%zx_Tn,(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/(T*eps)

  call cleanup_saftvrmie_zeta(eta_hs_l)


  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_F11_Santos(nc,svrm_var%eta_hs,F,F_T,F_V,F_n,&
       F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V+V*eps,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_F11_Santos(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"Testing F11 Santos"
  print *,"Volume"
  print *,F
  print *,F_V,(F2 - F)/(V*eps)
  print *,F_VV,(F2_V - F_V)/(V*eps)
  print *,F_TV,(F2_T - F_T)/(V*eps)
  print *,F_Vn,(F2_n - F_n)/(V*eps)

  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_F11_Santos(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"Temperature"
  print *,F_T,(F2 - F)/(T*eps)
  print *,F_TT,(F2_T - F_T)/(T*eps)
  print *,F_TV,(F2_V - F_V)/(T*eps)
  print *,F_Tn,(F2_n - F_n)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_F11_Santos(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"n(1)"
  print *,F_n(1),(F2 - F)/(eps)
  print *,F_Vn(1),(F2_V - F_V)/(eps)
  print *,F_Tn(1),(F2_T - F_T)/(eps)
  print *,F_nn(1,:),(F2_n - F_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_F11_Santos(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"n(2)"
  print *,F_n(2),(F2 - F)/(eps)
  print *,F_Vn(2),(F2_V - F_V)/(eps)
  print *,F_Tn(2),(F2_T - F_T)/(eps)
  print *,F_nn(2,:),(F2_n - F_n)/(eps)

  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_pure_ahs_div_nRT(nc,svrm_var%eta_hs,F,F_T,F_V,F_n,&
       F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V+V*eps,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_pure_ahs_div_nRT(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"Testing pure ahs Santos"
  print *,"Volume"
  print *,F
  print *,F_V,(F2 - F)/(V*eps)
  print *,F_VV,(F2_V - F_V)/(V*eps)
  print *,F_TV,(F2_T - F_T)/(V*eps)
  print *,F_Vn,(F2_n - F_n)/(V*eps)

  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_pure_ahs_div_nRT(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"Temperature"
  print *,F_T,(F2 - F)/(T*eps)
  print *,F_TT,(F2_T - F_T)/(T*eps)
  print *,F_TV,(F2_V - F_V)/(T*eps)
  print *,F_Tn,(F2_n - F_n)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_pure_ahs_div_nRT(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"n(1)"
  print *,F_n(1),(F2 - F)/(eps)
  print *,F_Vn(1),(F2_V - F_V)/(eps)
  print *,F_Tn(1),(F2_T - F_T)/(eps)
  print *,F_nn(1,:),(F2_n - F_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_Santos_eta(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)
  call calc_pure_ahs_div_nRT(nc,svrm_var%eta_hs,F2,F2_T,F2_V,F2_n)
  print *,"n(2)"
  print *,F_n(2),(F2 - F)/(eps)
  print *,F_Vn(2),(F2_V - F_V)/(eps)
  print *,F_Tn(2),(F2_T - F_T)/(eps)
  print *,F_nn(2,:),(F2_n - F_n)/(eps)

  print *,"Testing Santos virial B_ijk term"
  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_Bijk(nc,svrm_var%dhs,B_ijk,B_ijk_T,B_ijk_TT)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_virial_Bijk(nc,svrm_var%dhs,B2_ijk,B2_ijk_T, B2_ijk_TT)
  print *,"T"
  do i=1,nc
     do j=1,nc
        do k=1,nc
           print *,"i,j,k,B_ijk: ",i,j,k,B_ijk(i,j,k)
           print *,"B_T, B_T num:",B_ijk_T(i,j,k),(B2_ijk(i,j,k) - B_ijk(i,j,k))/(T*eps)
           print *,"B_TT, B_TT num:",B_ijk_TT(i,j,k),(B2_ijk_T(i,j,k) - B_ijk_T(i,j,k))/(T*eps)
        enddo
     enddo
  enddo

  print *,"Testing Santos virial B2"
  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s2,B2s2_T,B2s2_TT,B2s2_Tn,B2s2_n,B2s2_nn)
  print *,"Temperature"
  print *,B2s
  print *,B2s_T,(B2s2 - B2s)/(T*eps)
  print *,B2s_TT,(B2s2_T - B2s_T)/(T*eps)
  print *,B2s_Tn,(B2s2_n - B2s_n)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s2,B2s2_T,B2s2_TT,B2s2_Tn,B2s2_n,B2s2_nn)
  print *,"n(1)"
  print *,B2s_n(1),(B2s2 - B2s)/(eps)
  print *,B2s_Tn(1),(B2s2_T - B2s_T)/(eps)
  print *,B2s_nn(1,:),(B2s2_n - B2s_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s2,B2s2_T,B2s2_TT,B2s2_Tn,B2s2_n,B2s2_nn)
  print *,"n(2)"
  print *,B2s_n(2),(B2s2 - B2s)/(eps)
  print *,B2s_Tn(2),(B2s2_T - B2s_T)/(eps)
  print *,B2s_nn(2,:),(B2s2_n - B2s_n)/(eps)

  print *,"Testing Santos virial B3"
  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s2,B3s2_T,B3s2_TT,B3s2_Tn,B3s2_n,B3s2_nn)
  print *,"Temperature"
  print *,B3s
  print *,B3s_T,(B3s2 - B3s)/(T*eps)
  print *,B3s_TT,(B3s2_T - B3s_T)/(T*eps)
  print *,B3s_Tn,(B3s2_n - B3s_n)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s2,B3s2_T,B3s2_TT,B3s2_Tn,B3s2_n,B3s2_nn)
  print *,"n(1)"
  print *,B3s_n(1),(B3s2 - B3s)/(eps)
  print *,B3s_Tn(1),(B3s2_T - B3s_T)/(eps)
  print *,B3s_nn(1,:),(B3s2_n - B3s_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s2,B3s2_T,B3s2_TT,B3s2_Tn,B3s2_n,B3s2_nn)
  print *,"n(2)"
  print *,B3s_n(2),(B3s2 - B3s)/(eps)
  print *,B3s_Tn(2),(B3s2_T - B3s_T)/(eps)
  print *,B3s_nn(2,:),(B3s2_n - B3s_n)/(eps)


  difflevel = 2
  aP1_T => aL1_T
  aP1_V => aL1_V
  aP1_n => aL1_n
  aP1_VV => aL1_VV
  aP1_TV => aL1_TV
  aP1_Vn => aL1_Vn
  aP1_TT => aL1_TT
  aP1_Tn => aL1_Tn
  aP1_nn => aL1_nn
  aP2_T => aL2_T
  aP2_V => aL2_V
  aP2_n => aL2_n
  aP2_VV => aL2_VV
  aP2_TV => aL2_TV
  aP2_Vn => aL2_Vn
  aP2_TT => aL2_TT
  aP2_Tn => aL2_Tn
  aP2_nn => aL2_nn
  aP1d_T => aL1d_T
  aP1d_V => aL1d_V
  aP1d_n => aL1d_n
  aP1d_VV => aL1d_VV
  aP1d_TV => aL1d_TV
  aP1d_Vn => aL1d_Vn
  aP1d_TT => aL1d_TT
  aP1d_Tn => aL1d_Tn
  aP1d_nn => aL1d_nn
  aP2d_T => aL2d_T
  aP2d_V => aL2d_V
  aP2d_n => aL2d_n
  aP2d_VV => aL2d_VV
  aP2d_TV => aL2d_TV
  aP2d_Vn => aL2d_Vn
  aP2d_TT => aL2d_TT
  aP2d_Tn => aL2d_Tn
  aP2d_nn => aL2d_nn

  print *,"Testing Santos F12"
  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.true.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP1,F12_T=aP1_T,F12_V=aP1_V,F12_n=aP1_n,&
       F12_TT=aP1_TT,F12_TV=aP1_TV,F12_Tn=aP1_Tn,F12_VV=aP1_VV,&
       F12_Vn=aP1_Vn,F12_nn=aP1_nn)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.true.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP1d,F12_T=aP1d_T,F12_V=aP1d_V,F12_n=aP1d_n,&
       F12_TT=aP1d_TT,F12_TV=aP1d_TV,F12_Tn=aP1d_Tn,F12_VV=aP1d_VV,&
       F12_Vn=aP1d_Vn,F12_nn=aP1d_nn)
  print *,"Temperature"
  print *,aP1
  print *,aP1_T,(aP1d - aP1)/(T*eps)
  print *,aP1_TT,(aP1d_T - aP1_T)/(T*eps)
  print *,aP1_Tn,(aP1d_n - aP1_n)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.true.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP1d,F12_T=aP1d_T,F12_V=aP1d_V,F12_n=aP1d_n,&
       F12_TT=aP1d_TT,F12_TV=aP1d_TV,F12_Tn=aP1d_Tn,F12_VV=aP1d_VV,&
       F12_Vn=aP1d_Vn,F12_nn=aP1d_nn)
  print *,"n(1)"
  print *,aP1_n(1),(aP1d - aP1)/(eps)
  print *,aP1_Tn(1),(aP1d_T - aP1_T)/(eps)
  print *,aP1_nn(1,:),(aP1d_n - aP1_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.true.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP1d,F12_T=aP1d_T,F12_V=aP1d_V,F12_n=aP1d_n,&
       F12_TT=aP1d_TT,F12_TV=aP1d_TV,F12_Tn=aP1d_Tn,F12_VV=aP1d_VV,&
       F12_Vn=aP1d_Vn,F12_nn=aP1d_nn)
  print *,"n(2)"
  print *,aP1_n(2),(aP1d - aP1)/(eps)
  print *,aP1_Tn(2),(aP1d_T - aP1_T)/(eps)
  print *,aP1_nn(2,:),(aP1d_n - aP1_n)/(eps)

  print *,"Testing Santos F22"
  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.false.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP2,F12_T=aP2_T,F12_V=aP2_V,F12_n=aP2_n,&
       F12_TT=aP2_TT,F12_TV=aP2_TV,F12_Tn=aP2_Tn,F12_VV=aP2_VV,&
       F12_Vn=aP2_Vn,F12_nn=aP2_nn)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.false.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP2d,F12_T=aP2d_T,F12_V=aP2d_V,F12_n=aP2d_n,&
       F12_TT=aP2d_TT,F12_TV=aP2d_TV,F12_Tn=aP2d_Tn,F12_VV=aP2d_VV,&
       F12_Vn=aP2d_Vn,F12_nn=aP2d_nn)
  print *,"Temperature"
  print *,aP2
  print *,aP2_T,(aP2d - aP2)/(T*eps)
  print *,aP2_TT,(aP2d_T - aP2_T)/(T*eps)
  print *,aP2_Tn,(aP2d_n - aP2_n)/(T*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.false.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP2d,F12_T=aP2d_T,F12_V=aP2d_V,F12_n=aP2d_n,&
       F12_TT=aP2d_TT,F12_TV=aP2d_TV,F12_Tn=aP2d_Tn,F12_VV=aP2d_VV,&
       F12_Vn=aP2d_Vn,F12_nn=aP2d_nn)
  print *,"n(1)"
  print *,aP2_n(1),(aP2d - aP2)/(eps)
  print *,aP2_Tn(1),(aP2d_T - aP2_T)/(eps)
  print *,aP2_nn(1,:),(aP2d_n - aP2_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_virial_B2(nc,n,svrm_var%dhs,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn)
  call calc_hardsphere_virial_B3(nc,n,svrm_var%dhs,B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn)
  call calc_Santos_F12_or_F22(nc,n,svrm_var%dhs,.false.,B2s,B2s_T,B2s_TT,B2s_Tn,B2s_n,B2s_nn,&
       B3s,B3s_T,B3s_TT,B3s_Tn,B3s_n,B3s_nn,&
       aP2d,F12_T=aP2d_T,F12_V=aP2d_V,F12_n=aP2d_n,&
       F12_TT=aP2d_TT,F12_TV=aP2d_TV,F12_Tn=aP2d_Tn,F12_VV=aP2d_VV,&
       F12_Vn=aP2d_Vn,F12_nn=aP2d_nn)
  print *,"n(2)"
  print *,aP2_n(2),(aP2d - aP2)/(eps)
  print *,aP2_Tn(2),(aP2d_T - aP2_T)/(eps)
  print *,aP2_nn(2,:),(aP2d_n - aP2_n)/(eps)

  print *,"Testing entire Santos EoS"

  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_santos(nc,T,V,n,svrm_var,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_santos(nc,T+T*eps,V,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"Temperature"
  print *,a
  print *,a_T,(a2 - a)/(T*eps)
  print *,a_TV,(a2_V - a_V)/(T*eps)
  print *,a_TT,(a2_T - a_T)/(T*eps)
  print *,a_Tn,(a2_n - a_n)/(T*eps)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_santos(nc,T,V+V*eps,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"Volume"
  print *,a_V,(a2 - a)/(V*eps)
  print *,a_TV,(a2_T - a_T)/(V*eps)
  print *,a_VV,(a2_V - a_V)/(V*eps)
  print *,a_Vn,(a2_n - a_n)/(V*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_santos(nc,T,V,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"n(1)"
  print *,a_n(1),(a2 - a)/(eps)
  print *,a_Tn(1),(a2_T - a_T)/(eps)
  print *,a_Vn(1),(a2_V - a_V)/(eps)
  print *,a_nn(1,:),(a2_n - a_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_santos(nc,T,V,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"n(2)"
  print *,a_n(2),(a2 - a)/(eps)
  print *,a_Tn(2),(a2_T - a_T)/(eps)
  print *,a_Vn(2),(a2_V - a_V)/(eps)
  print *,a_nn(2,:),(a2_n - a_n)/(eps)

  stop
end subroutine test_hard_sphere_Santos

subroutine test_hardsphere_pure()
  use thermopack_constants
  use saftvrmie_containers, only: get_saftvrmie_var, saftvrmie_param, &
       allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta, saftvrmie_zeta, &
       saftvrmie_var_container
  use saftvrmie_interface, only: preCalcSAFTVRMie
  use saftvrmie_hardsphere, only: calc_eta_dij, calc_hardsphere_helmholtzenergy_pure, &
       calc_d_pure
  use saftvrmie_options
  use saftvrmie_dispersion, only: calcXDifferentialsPureHSRef
  use thermopack_var
  implicit none
  real :: n(nc),T,V,eps,n0(nc)
  integer :: difflevel, i, j
  type(saftvrmie_zeta) :: eta_hs_l, d_pure, x0, x0r
  real :: a
  real :: a_T,a_V,a_n(nc)
  real :: a_VV,a_TV,a_Vn(nc)
  real :: a_TT,a_Tn(nc),a_nn(nc,nc)
  real :: a2
  real :: a2_T,a2_V,a2_n(nc)
  real :: a2_VV,a2_TV,a2_Vn(nc)
  real :: a2_TT,a2_Tn(nc),a2_nn(nc,nc)
  real :: s, s_T, s_TT
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()

  call allocate_saftvrmie_zeta(nc,eta_hs_l)
  call allocate_saftvrmie_zeta(nc,d_pure)
  call allocate_saftvrmie_zeta(nc,x0)
  call allocate_saftvrmie_zeta(nc,x0r)
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-6
  difflevel = 2
  quantum_correction_hs = 2
  !hardsphere_EoS = HS_EOS_PURE_DIJ
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_eta_dij(nc,n,V,difflevel,svrm_var%dhs,svrm_var%eta_hs)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_eta_dij(nc,n,V+V*eps,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"Testing hypotetical pure fluid packing fraction"
  print *,"Volume"
  print *,svrm_var%eta_hs%zx
  print *,svrm_var%eta_hs%zx_V,(eta_hs_l%zx - svrm_var%eta_hs%zx)/(V*eps)
  print *,svrm_var%eta_hs%zx_VV,(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/(V*eps)
  print *,svrm_var%eta_hs%zx_TV,(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/(V*eps)
  print *,svrm_var%eta_hs%zx_Vn,(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/(V*eps)
  print *,svrm_var%eta_hs%zx_VVV,(eta_hs_l%zx_VV - svrm_var%eta_hs%zx_VV)/(V*eps)
  print *,svrm_var%eta_hs%zx_VVT,(eta_hs_l%zx_TV - svrm_var%eta_hs%zx_TV)/(V*eps)
  print *,svrm_var%eta_hs%zx_VTT,(eta_hs_l%zx_TT - svrm_var%eta_hs%zx_TT)/(V*eps)
  print *,svrm_var%eta_hs%zx_VTn,(eta_hs_l%zx_Tn - svrm_var%eta_hs%zx_Tn)/(V*eps)
  print *,svrm_var%eta_hs%zx_VVn,(eta_hs_l%zx_Vn - svrm_var%eta_hs%zx_Vn)/(V*eps)
  print *,svrm_var%eta_hs%zx_Vnn(1,:),(eta_hs_l%zx_nn(1,:) - svrm_var%eta_hs%zx_nn(1,:))/(V*eps)
  print *,svrm_var%eta_hs%zx_Vnn(2,:),(eta_hs_l%zx_nn(2,:) - svrm_var%eta_hs%zx_nn(2,:))/(V*eps)

  n(1) = n(1) + eps
  call calc_eta_dij(nc,n,V,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"n(1)"
  print *,svrm_var%eta_hs%zx_n(1),(eta_hs_l%zx - svrm_var%eta_hs%zx)/eps
  print *,svrm_var%eta_hs%zx_Tn(1),(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/eps
  print *,svrm_var%eta_hs%zx_Vn(1),(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/eps
  print *,svrm_var%eta_hs%zx_nn(1,:),(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/eps
  print *,svrm_var%eta_hs%zx_VTn(1),(eta_hs_l%zx_TV - svrm_var%eta_hs%zx_TV)/eps
  print *,svrm_var%eta_hs%zx_VVn(1),(eta_hs_l%zx_VV - svrm_var%eta_hs%zx_VV)/eps
  print *,svrm_var%eta_hs%zx_Vnn(1,:),(eta_hs_l%zx_Vn - svrm_var%eta_hs%zx_Vn)/eps

  n = n0
  n(2) = n(2) + eps
  call calc_eta_dij(nc,n,V,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"n(2)"
  print *,svrm_var%eta_hs%zx_n(2),(eta_hs_l%zx - svrm_var%eta_hs%zx)/eps
  print *,svrm_var%eta_hs%zx_Tn(2),(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/eps
  print *,svrm_var%eta_hs%zx_Vn(2),(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/eps
  print *,svrm_var%eta_hs%zx_nn(2,:),(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/eps
  print *,svrm_var%eta_hs%zx_VTn(2),(eta_hs_l%zx_TV - svrm_var%eta_hs%zx_TV)/eps
  print *,svrm_var%eta_hs%zx_VVn(2),(eta_hs_l%zx_VV - svrm_var%eta_hs%zx_VV)/eps
  print *,svrm_var%eta_hs%zx_Vnn(2,:),(eta_hs_l%zx_Vn - svrm_var%eta_hs%zx_Vn)/eps

  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_eta_dij(nc,n,V,difflevel,svrm_var%dhs,eta_hs_l)
  print *,"Temperature"
  print *,svrm_var%eta_hs%zx_T,(eta_hs_l%zx - svrm_var%eta_hs%zx)/(T*eps)
  print *,svrm_var%eta_hs%zx_TT,(eta_hs_l%zx_T - svrm_var%eta_hs%zx_T)/(T*eps)
  print *,svrm_var%eta_hs%zx_TV,(eta_hs_l%zx_V - svrm_var%eta_hs%zx_V)/(T*eps)
  print *,svrm_var%eta_hs%zx_Tn,(eta_hs_l%zx_n - svrm_var%eta_hs%zx_n)/(T*eps)
  print *,svrm_var%eta_hs%zx_VVT,(eta_hs_l%zx_VV - svrm_var%eta_hs%zx_VV)/(T*eps)
  print *,svrm_var%eta_hs%zx_VTT,(eta_hs_l%zx_TV - svrm_var%eta_hs%zx_TV)/(T*eps)
  print *,svrm_var%eta_hs%zx_VTn,(eta_hs_l%zx_Vn - svrm_var%eta_hs%zx_Vn)/(T*eps)

  print *,"Testing entire pure EoS"

  n = n0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_pure(nc,T,V,n,svrm_var,a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_pure(nc,T+T*eps,V,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"Temperature"
  print *,a
  print *,a_T,(a2 - a)/(T*eps)
  print *,a_TV,(a2_V - a_V)/(T*eps)
  print *,a_TT,(a2_T - a_T)/(T*eps)
  print *,a_Tn,(a2_n - a_n)/(T*eps)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_pure(nc,T,V+V*eps,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"Volume"
  print *,a_V,(a2 - a)/(V*eps)
  print *,a_TV,(a2_T - a_T)/(V*eps)
  print *,a_VV,(a2_V - a_V)/(V*eps)
  print *,a_Vn,(a2_n - a_n)/(V*eps)

  n(1) = n(1) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_pure(nc,T,V,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"n(1)"
  print *,a_n(1),(a2 - a)/(eps)
  print *,a_Tn(1),(a2_T - a_T)/(eps)
  print *,a_Vn(1),(a2_V - a_V)/(eps)
  print *,a_nn(1,:),(a2_n - a_n)/(eps)

  n = n0
  n(2) = n(2) + eps
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_hardsphere_helmholtzenergy_pure(nc,T,V,n,svrm_var,a2,a2_T,a2_V,a2_n,&
       a2_TT,a2_TV,a2_Tn,a2_VV,a2_Vn,a2_nn)
  print *,"n(2)"
  print *,a_n(2),(a2 - a)/(eps)
  print *,a_Tn(2),(a2_T - a_T)/(eps)
  print *,a_Vn(2),(a2_V - a_V)/(eps)
  print *,a_nn(2,:),(a2_n - a_n)/(eps)


  n = n0
  print *
  print *,"Testing d_pure"
  print *
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,svrm_var%d_pure)

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_d_pure(nc,n,V+V*eps,2,svrm_var%dhs,d_pure)
  print *,"Volume"
  print *,svrm_var%d_pure%zx
  print *,svrm_var%d_pure%zx_V,(d_pure%zx - svrm_var%d_pure%zx)/(V*eps)
  print *,svrm_var%d_pure%zx_VV,(d_pure%zx_V - svrm_var%d_pure%zx_V)/(V*eps)
  print *,svrm_var%d_pure%zx_TV,(d_pure%zx_T - svrm_var%d_pure%zx_T)/(V*eps)
  print *,svrm_var%d_pure%zx_Vn,(d_pure%zx_n - svrm_var%d_pure%zx_n)/(V*eps)

  n(1) = n(1) + eps
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  print *,"n(1)"
  print *,svrm_var%d_pure%zx_n(1),(d_pure%zx - svrm_var%d_pure%zx)/eps
  print *,svrm_var%d_pure%zx_Tn(1),(d_pure%zx_T - svrm_var%d_pure%zx_T)/eps
  print *,svrm_var%d_pure%zx_Vn(1),(d_pure%zx_V - svrm_var%d_pure%zx_V)/eps
  print *,svrm_var%d_pure%zx_nn(1,:),(d_pure%zx_n - svrm_var%d_pure%zx_n)/eps

  n = n0
  n(2) = n(2) + eps
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  print *,"n(2)"
  print *,svrm_var%d_pure%zx_n(2),(d_pure%zx - svrm_var%d_pure%zx)/eps
  print *,svrm_var%d_pure%zx_Tn(2),(d_pure%zx_T - svrm_var%d_pure%zx_T)/eps
  print *,svrm_var%d_pure%zx_Vn(2),(d_pure%zx_V - svrm_var%d_pure%zx_V)/eps
  print *,svrm_var%d_pure%zx_nn(2,:),(d_pure%zx_n - svrm_var%d_pure%zx_n)/eps

  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  print *,"Temperature"
  print *,svrm_var%d_pure%zx_T,(d_pure%zx - svrm_var%d_pure%zx)/(T*eps)
  print *,svrm_var%d_pure%zx_TT,(d_pure%zx_T - svrm_var%d_pure%zx_T)/(T*eps)
  print *,svrm_var%d_pure%zx_TV,(d_pure%zx_V - svrm_var%d_pure%zx_V)/(T*eps)
  print *,svrm_var%d_pure%zx_Tn,(d_pure%zx_n - svrm_var%d_pure%zx_n)/(T*eps)

  print *
  print *,"Testing x0"
  print *

  s = saftvrmie_param%sigma_ij(1,1)
  s_T = 0.0
  s_TT = 0.0
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,svrm_var%d_pure,x0r)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_d_pure(nc,n,V+V*eps,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)

  print *,"Volume"
  print *,x0r%zx
  print *,x0r%zx_V,(x0%zx - x0r%zx)/(V*eps)
  print *,x0r%zx_VV,(x0%zx_V - x0r%zx_V)/(V*eps)
  print *,x0r%zx_TV,(x0%zx_T - x0r%zx_T)/(V*eps)
  print *,x0r%zx_Vn,(x0%zx_n - x0r%zx_n)/(V*eps)

  n(1) = n(1) + eps
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)
  print *,"n(1)"
  print *,x0r%zx_n(1),(x0%zx - x0r%zx)/eps
  print *,x0r%zx_Tn(1),(x0%zx_T - x0r%zx_T)/eps
  print *,x0r%zx_Vn(1),(x0%zx_V - x0r%zx_V)/eps
  print *,x0r%zx_nn(1,:),(x0%zx_n - x0r%zx_n)/eps

  n = n0
  n(2) = n(2) + eps
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)
  print *,"n(2)"
  print *,x0r%zx_n(2),(x0%zx - x0r%zx)/eps
  print *,x0r%zx_Tn(2),(x0%zx_T - x0r%zx_T)/eps
  print *,x0r%zx_Vn(2),(x0%zx_V - x0r%zx_V)/eps
  print *,x0r%zx_nn(2,:),(x0%zx_n - x0r%zx_n)/eps

  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)
  print *,"Temperature"
  print *,x0r%zx_T,(x0%zx - x0r%zx)/(T*eps)
  print *,x0r%zx_TT,(x0%zx_T - x0r%zx_T)/(T*eps)
  print *,x0r%zx_TV,(x0%zx_V - x0r%zx_V)/(T*eps)
  print *,x0r%zx_Tn,(x0%zx_n - x0r%zx_n)/(T*eps)

  print *
  print *,"Testing x1"
  print *

  i = 1
  j = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,svrm_var%d_pure,x0r)
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,svrm_var)
  call calc_d_pure(nc,n,V+V*eps,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)

  print *,"Volume"
  print *,x0r%zx
  print *,x0r%zx_V,(x0%zx - x0r%zx)/(V*eps)
  print *,x0r%zx_VV,(x0%zx_V - x0r%zx_V)/(V*eps)
  print *,x0r%zx_TV,(x0%zx_T - x0r%zx_T)/(V*eps)
  print *,x0r%zx_Vn,(x0%zx_n - x0r%zx_n)/(V*eps)

  n(1) = n(1) + eps
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)
  print *,"n(1)"
  print *,x0r%zx_n(1),(x0%zx - x0r%zx)/eps
  print *,x0r%zx_Tn(1),(x0%zx_T - x0r%zx_T)/eps
  print *,x0r%zx_Vn(1),(x0%zx_V - x0r%zx_V)/eps
  print *,x0r%zx_nn(1,:),(x0%zx_n - x0r%zx_n)/eps

  n = n0
  n(2) = n(2) + eps
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)
  print *,"n(2)"
  print *,x0r%zx_n(2),(x0%zx - x0r%zx)/eps
  print *,x0r%zx_Tn(2),(x0%zx_T - x0r%zx_T)/eps
  print *,x0r%zx_Vn(2),(x0%zx_V - x0r%zx_V)/eps
  print *,x0r%zx_nn(2,:),(x0%zx_n - x0r%zx_n)/eps

  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_d_pure(nc,n,V,2,svrm_var%dhs,d_pure)
  s = svrm_var%sigma_eff%d(i,j)
  s_T = svrm_var%sigma_eff%d_T(i,j)
  s_TT = svrm_var%sigma_eff%d_TT(i,j)
  call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,d_pure,x0)
  print *,"Temperature"
  print *,x0r%zx_T,(x0%zx - x0r%zx)/(T*eps)
  print *,x0r%zx_TT,(x0%zx_T - x0r%zx_T)/(T*eps)
  print *,x0r%zx_TV,(x0%zx_V - x0r%zx_V)/(T*eps)
  print *,x0r%zx_Tn,(x0%zx_n - x0r%zx_n)/(T*eps)

  call cleanup_saftvrmie_zeta(eta_hs_l)
  call cleanup_saftvrmie_zeta(d_pure)
  call cleanup_saftvrmie_zeta(x0)
  call cleanup_saftvrmie_zeta(x0r)

end subroutine test_hardsphere_pure

subroutine test_zeta_zeta()
  use thermopack_constants
  use saftvrmie_options
  use thermopack_var
  implicit none
  real :: n(nc),T,V,eps,n0(nc)
  real :: a, a_T,a_V,a_n(nc)
  real :: a_VV,a_TV,a_Vn(nc)
  real :: a_TT,a_Tn(nc),a_nn(nc,nc)
  real :: a_VVV,a_VVT,a_VTT,a_VVn(nc),a_Vnn(nc,nc),a_VTn(nc)
  real :: a2, a2_T,a2_V,a2_n(nc)
  real :: a2_VV,a2_TV,a2_Vn(nc)
  real :: a2_TT,a2_Tn(nc),a2_nn(nc,nc)
  real :: a2_VVV,a2_VVT,a2_VTT,a2_VVn(nc),a2_Vnn(nc,nc),a2_VTn(nc)
  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-6
  quantum_correction_hs = 2
  hardsphere_EoS = HS_EOS_PURE_DIJ

  call calc_a(nc,T,V,n,a,a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)

  print *,"Testing zeta zeta"
  print *,"V"
  call calc_a(nc,T,V+V*eps,n,a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
  print *,a
  print *,a_V,(a2 - a)/(V*eps)
  print *,a_VV,(a2_V - a_V)/(V*eps)
  print *,a_TV,(a2_T - a_T)/(V*eps)
  print *,a_Vn,(a2_n - a_n)/(V*eps)
  print *,a_VVV,(a2_VV - a_VV)/(V*eps)
  print *,a_VVT,(a2_TV - a_TV)/(V*eps)
  print *,a_VTT,(a2_TT - a_TT)/(V*eps)
  print *,a_VTn,(a2_Tn - a_Tn)/(V*eps)
  print *,a_VVn,(a2_Vn - a_Vn)/(V*eps)
  print *,a_Vnn(1,:),(a2_nn(1,:) - a_nn(1,:))/(V*eps)
  print *,a_Vnn(2,:),(a2_nn(2,:) - a_nn(2,:))/(V*eps)

  print *,"n1"
  n(1) = n(1) + eps
  call calc_a(nc,T,V,n,a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
  print *,a_n(1),(a2 - a)/eps
  print *,a_Tn(1),(a2_T - a_T)/eps
  print *,a_Vn(1),(a2_V - a_V)/eps
  print *,a_nn(1,:),(a2_n - a_n)/eps
  print *,a_VVn(1),(a2_VV - a_VV)/eps
  print *,a_Vnn(1,:),(a2_Vn - a_Vn)/eps
  print *,a_VTn(1),(a2_TV - a_TV)/eps

  print *,"n2"
  n = n0
  n(2) = n(2) + eps
  call calc_a(nc,T,V,n,a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)
  print *,a_n(2),(a2 - a)/eps
  print *,a_Tn(2),(a2_T - a_T)/eps
  print *,a_Vn(2),(a2_V - a_V)/eps
  print *,a_nn(2,:),(a2_n - a_n)/eps
  print *,a_VVn(2),(a2_VV - a_VV)/eps
  print *,a_Vnn(2,:),(a2_Vn - a_Vn)/eps
  print *,a_VTn(2),(a2_TV - a_TV)/eps

  print *,"T"
  n = n0
  call calc_a(nc,T+T*eps,V,n,a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

  print *,a_T,(a2 - a)/(T*eps)
  print *,a_TT,(a2_T - a_T)/(T*eps)
  print *,a_TV,(a2_V - a_V)/(T*eps)
  print *,a_Tn,(a2_n - a_n)/(T*eps)
  print *,a_VVT,(a2_VV - a_VV)/(T*eps)
  print *,a_VTT,(a2_TV - a_TV)/(T*eps)
  print *,a_VTn,(a2_Vn - a_Vn)/(T*eps)

contains
  subroutine calc_a(nc,T,V,n,a1,a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
       a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn)
    use saftvrmie_dispersion, only: calcXDifferentialsPureHSRef, calcBtilde
    use saftvrmie_containers, only: allocate_saftvrmie_zeta, cleanup_saftvrmie_zeta, &
         saftvrmie_zeta, get_saftvrmie_var, saftvrmie_param, saftvrmie_var_container
    use saftvrmie_interface, only: preCalcSAFTVRMie
    use saftvrmie_utils, only:  convert_zeta_zeta_to_TVn
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T, V, n(nc)
    ! Output
    real, intent(out) ::  a1,a1_T,a1_V,a1_TT,a1_VV,a1_TV,a1_VVV,a1_VVT,a1_VTT
    real, dimension(nc), intent(out) :: a1_n,a1_Tn,a1_Vn,a1_VVn,a1_VTn
    real, dimension(nc,nc), intent(out) :: a1_nn,a1_Vnn
    ! Locals
    type(saftvrmie_zeta) :: y
    real :: y0, e0, lambda, C, eps
    real :: Bea,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,Bea_eee,Bea_eey,Bea_eyy
    integer :: i,j
    real :: s, s_T, s_TT
    type(saftvrmie_var_container), pointer :: svrm_var
    svrm_var => get_saftvrmie_var()
    call allocate_saftvrmie_zeta(nc,y)
    i = 1
    j = 2
    lambda = saftvrmie_param%lambda_a_ij(i,j)
    !lambda_r = saftvrmie_param%lambda_r_ij(i,j)
    eps = saftvrmie_param%eps_divk_ij(i,j)
    C = saftvrmie_param%Cij(i,j)
    call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
    s = svrm_var%sigma_eff%d(i,j)
    s_T = svrm_var%sigma_eff%d_T(i,j)
    s_TT = svrm_var%sigma_eff%d_TT(i,j)
    call calcXDifferentialsPureHSRef(nc,s,s_T,s_TT,svrm_var%d_pure,y)
    y0 = y%zx
    e0 = svrm_var%zeta%zx
    call calcBtilde(y0,e0,lambda,eps,Bea,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,&
         Bea_eee,Bea_eey,Bea_eyy,fac_in=C)
    a1 = Bea
    call convert_zeta_zeta_to_TVn(nc,y,svrm_var%zeta,&
         a1,Bea_e,Bea_y,Bea_ee,Bea_yy,Bea_ey,Bea_eee,0.0,Bea_eey,Bea_eyy,&
         a1_T,a1_V,a1_n,a1_TT,a1_VV,a1_TV,a1_Tn,a1_Vn,a1_nn,&
         a1_VVV,a1_VVT,a1_VTT,a1_VVn,a1_Vnn,a1_VTn,difflevel=3)
    call cleanup_saftvrmie_zeta(y)
  end subroutine calc_a
end subroutine test_zeta_zeta

subroutine test_pure_fluid()
  use thermopack_constants
  use saftvrmie_options
  use saftvrmie_interface, only: preCalcSAFTVRMie
  use saftvrmie_dispersion, only: calcA1, get_saftvrmie_var, saftvrmie_var_container
  use saftvrmie_containers, only: get_saftvrmie_var, saftvrmie_var_container
  use thermopack_var
  implicit none
  real :: n(nc),T,V,eps,n0(nc)
  real :: a, a_T,a_V,a_n(nc)
  real :: a_VV,a_TV,a_Vn(nc)
  real :: a_TT,a_Tn(nc),a_nn(nc,nc)
  real :: a_VVV,a_VVT,a_VTT,a_VVn(nc),a_Vnn(nc,nc),a_VTn(nc)
  real :: a2, a2_T,a2_V,a2_n(nc)
  real :: a2_VV,a2_TV,a2_Vn(nc)
  real :: a2_TT,a2_Tn(nc),a2_nn(nc,nc)
  real :: a2_VVV,a2_VVT,a2_VTT,a2_VVn(nc),a2_Vnn(nc,nc),a2_VTn(nc)
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()
  n = (/1.2/)
  n0 = n
  V = 1.0e-4
  T = 5.0
  eps = 1.0e-6
  quantum_correction_hs = 2
  quantum_correction = 2

  hardsphere_EoS = HS_EOS_ORIGINAL
  zeta_mixing_rule = ZETA_LEONARD
  exact_binary_dhs = .true.
  call preCalcSAFTVRMie(nc,T,V,n,3,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       a,a_T,a_V,a_n,a_TT,a_VV,a_TV,a_Tn,a_Vn,a_nn,&
       a_VVV,a_VVT,a_VTT,a_VVn,a_Vnn,a_VTn)

  hardsphere_EoS = HS_EOS_PURE_DIJ
  call preCalcSAFTVRMie(nc,T,V,n,3,svrm_var)
  call calcA1(nc,T,V,n,svrm_var,&
       a2,a2_T,a2_V,a2_n,a2_TT,a2_VV,a2_TV,a2_Tn,a2_Vn,a2_nn,&
       a2_VVV,a2_VVT,a2_VTT,a2_VVn,a2_Vnn,a2_VTn)

  print *,"Testing a1 pure fluid"
  print *,a, a2
  print *,a_V,a2_V
  print *,a_VV,a2_VV
  print *,a_TV,a2_TV
  print *,a_Vn,a2_Vn
  print *,a_VVV,a2_VVV
  print *,a_VVT,a2_VVT
  print *,a_VTT,a2_VTT
  print *,a_VTn,a2_VTn
  print *,a_VVn,a2_VVn
  print *,a_Vnn,a2_Vnn
  print *,a_T,a2_T
  print *,a_TT,a2_TT
  print *,a_Tn,a2_Tn
  print *,a_n,a2_n
  print *,a_nn,a2_nn

end subroutine test_pure_fluid

subroutine test_zeta()
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_dispersion
  use saftvrmie_hardsphere
  use saftvrmie_interface
  use thermopack_var
  implicit none
  real :: n(nc),n0(nc),eps,T,V
  type(saftvrmie_zeta) :: zeta2
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()

  n = (/0.2,1.2/)
  n0 = n
  V = 1.0e-4
  eps=1.0e-5
  T = 20.0
  quantum_correction = 1
  quantum_correction_hs = 1
  call allocate_saftvrmie_zeta(nc,zeta2)

  call preCalcSAFTVRMie(nc,T,V,n,3,svrm_var)
  call calcZetaX(nc,T,V+V*eps,n,3,svrm_var%dhs,zeta2)
  print *,"Testing hypotetical pure fluid packing fraction (zeta)"
  print *,"Volume"
  print *,svrm_var%zeta%zx
  print *,svrm_var%zeta%zx_V,(zeta2%zx - svrm_var%zeta%zx)/(V*eps)
  print *,svrm_var%zeta%zx_VV,(zeta2%zx_V - svrm_var%zeta%zx_V)/(V*eps)
  print *,svrm_var%zeta%zx_TV,(zeta2%zx_T - svrm_var%zeta%zx_T)/(V*eps)
  print *,svrm_var%zeta%zx_Vn,(zeta2%zx_n - svrm_var%zeta%zx_n)/(V*eps)
  print *,svrm_var%zeta%zx_VVV,(zeta2%zx_VV - svrm_var%zeta%zx_VV)/(V*eps)
  print *,svrm_var%zeta%zx_VVT,(zeta2%zx_TV - svrm_var%zeta%zx_TV)/(V*eps)
  print *,svrm_var%zeta%zx_VTT,(zeta2%zx_TT - svrm_var%zeta%zx_TT)/(V*eps)
  print *,svrm_var%zeta%zx_VTn,(zeta2%zx_Tn - svrm_var%zeta%zx_Tn)/(V*eps)
  print *,svrm_var%zeta%zx_VVn,(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/(V*eps)
  print *,svrm_var%zeta%zx_Vnn(1,:),(zeta2%zx_nn(1,:) - svrm_var%zeta%zx_nn(1,:))/(V*eps)
  print *,svrm_var%zeta%zx_Vnn(2,:),(zeta2%zx_nn(2,:) - svrm_var%zeta%zx_nn(2,:))/(V*eps)

  n(1) = n(1) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,zeta2)
  print *,"n(1)"
  print *,svrm_var%zeta%zx_n(1),(zeta2%zx - svrm_var%zeta%zx)/eps
  print *,svrm_var%zeta%zx_Tn(1),(zeta2%zx_T - svrm_var%zeta%zx_T)/eps
  print *,svrm_var%zeta%zx_Vn(1),(zeta2%zx_V - svrm_var%zeta%zx_V)/eps
  print *,svrm_var%zeta%zx_nn(1,:),(zeta2%zx_n - svrm_var%zeta%zx_n)/eps
  print *,svrm_var%zeta%zx_VVn(1),(zeta2%zx_VV - svrm_var%zeta%zx_VV)/eps
  print *,svrm_var%zeta%zx_Vnn(1,:),(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/eps
  print *,svrm_var%zeta%zx_VTn(1),(zeta2%zx_TV - svrm_var%zeta%zx_TV)/eps

  n = n0
  n(2) = n(2) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%dhs,zeta2)
  print *,"n(2)"
  print *,svrm_var%zeta%zx_n(2),(zeta2%zx - svrm_var%zeta%zx)/eps
  print *,svrm_var%zeta%zx_Tn(2),(zeta2%zx_T - svrm_var%zeta%zx_T)/eps
  print *,svrm_var%zeta%zx_Vn(2),(zeta2%zx_V - svrm_var%zeta%zx_V)/eps
  print *,svrm_var%zeta%zx_nn(2,:),(zeta2%zx_n - svrm_var%zeta%zx_n)/eps
  print *,svrm_var%zeta%zx_VVn(2),(zeta2%zx_VV - svrm_var%zeta%zx_VV)/eps
  print *,svrm_var%zeta%zx_Vnn(2,:),(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/eps
  print *,svrm_var%zeta%zx_VTn(2),(zeta2%zx_TV - svrm_var%zeta%zx_TV)/eps

  n = n0
  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T+eps, saftvrmie_param%DFeynHibbsParam_ij, &
       svrm_var%DFeynHibbsij, svrm_var%D2FeynHibbsij)
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T+eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  ! Calculate hard-sphere diameter
  call calc_hardsphere_diameter(nc,T+eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT,svrm_var%dhs%d,&
       svrm_var%dhs%d_T,svrm_var%dhs%d_TT)
  call calcZetaX(nc,T+eps,V,n,3,svrm_var%dhs,zeta2)
  print *,"Temperature"
  print *,svrm_var%zeta%zx_T,(zeta2%zx - svrm_var%zeta%zx)/eps
  print *,svrm_var%zeta%zx_TT,(zeta2%zx_T - svrm_var%zeta%zx_T)/eps
  print *,svrm_var%zeta%zx_TV,(zeta2%zx_V - svrm_var%zeta%zx_V)/eps
  print *,svrm_var%zeta%zx_Tn,(zeta2%zx_n - svrm_var%zeta%zx_n)/eps
  print *,svrm_var%zeta%zx_VVT,(zeta2%zx_VV - svrm_var%zeta%zx_VV)/eps
  print *,svrm_var%zeta%zx_VTT,(zeta2%zx_TV - svrm_var%zeta%zx_TV)/eps
  print *,svrm_var%zeta%zx_VTn,(zeta2%zx_Vn - svrm_var%zeta%zx_Vn)/eps

  !> Calculate packing fraction
  call cleanup_saftvrmie_zeta(zeta2)
  call allocate_saftvrmie_zeta(nc,zeta2)
  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       svrm_var%DFeynHibbsij, svrm_var%D2FeynHibbsij)
  call calc_binary_effective_sigma(nc,T,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  call calcZetaX(nc,T,V,n,3,svrm_var%sigma_eff,svrm_var%zeta_bar)
  call calcZetaX(nc,T,V+V*eps,n,3,svrm_var%sigma_eff,zeta2)
  print *,"Testing packing fraction (zeta_bar)"
  print *,"Volume"
  print *,svrm_var%zeta_bar%zx
  print *,svrm_var%zeta_bar%zx_V,(zeta2%zx - svrm_var%zeta_bar%zx)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VV,(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/(V*eps)
  print *,svrm_var%zeta_bar%zx_TV,(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/(V*eps)
  print *,svrm_var%zeta_bar%zx_Vn,(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VVV,(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VVT,(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VTT,(zeta2%zx_TT - svrm_var%zeta_bar%zx_TT)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VTn,(zeta2%zx_Tn - svrm_var%zeta_bar%zx_Tn)/(V*eps)
  print *,svrm_var%zeta_bar%zx_VVn,(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/(V*eps)
  print *,svrm_var%zeta_bar%zx_Vnn(1,:),(zeta2%zx_nn(1,:) - svrm_var%zeta_bar%zx_nn(1,:))/(V*eps)
  print *,svrm_var%zeta_bar%zx_Vnn(2,:),(zeta2%zx_nn(2,:) - svrm_var%zeta_bar%zx_nn(2,:))/(V*eps)

  n(1) = n(1) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%sigma_eff,zeta2)
  print *,"n(1)"
  print *,svrm_var%zeta_bar%zx_n(1),(zeta2%zx - svrm_var%zeta_bar%zx)/eps
  print *,svrm_var%zeta_bar%zx_Tn(1),(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/eps
  print *,svrm_var%zeta_bar%zx_Vn(1),(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/eps
  print *,svrm_var%zeta_bar%zx_nn(1,:),(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/eps
  print *,svrm_var%zeta_bar%zx_VVn(1),(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/eps
  print *,svrm_var%zeta_bar%zx_Vnn(1,:),(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/eps
  print *,svrm_var%zeta_bar%zx_VTn(1),(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/eps
  !stop
  n = n0
  n(2) = n(2) + eps
  call calcZetaX(nc,T,V,n,3,svrm_var%sigma_eff,zeta2)
  print *,"n(2)"
  print *,svrm_var%zeta_bar%zx_n(2),(zeta2%zx - svrm_var%zeta_bar%zx)/eps
  print *,svrm_var%zeta_bar%zx_Tn(2),(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/eps
  print *,svrm_var%zeta_bar%zx_Vn(2),(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/eps
  print *,svrm_var%zeta_bar%zx_nn(2,:),(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/eps
  print *,svrm_var%zeta_bar%zx_VVn(2),(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/eps
  print *,svrm_var%zeta_bar%zx_Vnn(2,:),(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/eps
  print *,svrm_var%zeta_bar%zx_VTn(2),(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/eps
  !stop
  n = n0

  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T+eps, saftvrmie_param%DFeynHibbsParam_ij, &
       svrm_var%DFeynHibbsij, svrm_var%D2FeynHibbsij)
  call calc_binary_effective_sigma(nc,T+eps,svrm_var,svrm_var%sigma_eff%d,&
       svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
  call calcZetaX(nc,T+eps,V,n,3,svrm_var%sigma_eff,zeta2)
  print *,"Temperature"
  print *,svrm_var%zeta_bar%zx_T,(zeta2%zx - svrm_var%zeta_bar%zx)/eps
  print *,svrm_var%zeta_bar%zx_TT,(zeta2%zx_T - svrm_var%zeta_bar%zx_T)/eps
  print *,svrm_var%zeta_bar%zx_TV,(zeta2%zx_V - svrm_var%zeta_bar%zx_V)/eps
  print *,svrm_var%zeta_bar%zx_Tn,(zeta2%zx_n - svrm_var%zeta_bar%zx_n)/eps
  print *,svrm_var%zeta_bar%zx_VVT,(zeta2%zx_VV - svrm_var%zeta_bar%zx_VV)/eps
  print *,svrm_var%zeta_bar%zx_VTT,(zeta2%zx_TV - svrm_var%zeta_bar%zx_TV)/eps
  print *,svrm_var%zeta_bar%zx_VTn,(zeta2%zx_Vn - svrm_var%zeta_bar%zx_Vn)/eps
  stop
end subroutine test_zeta

subroutine test_delta_Ac(Ti,Vi,ni,doInit)
  use thermopack_constants
  use saftvrmie_containers
  use saftvrmie_dispersion
  use saftvrmie_interface
  use saftvrmie_options
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  ! Locals
  real :: n(nc),n0(nc),T,V,eps
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  real :: Fp,Fp_T,Fp_V,Fp_TT,Fp_VV,Fp_TV
  real, dimension(nc) :: Fp_n,Fp_Tn,Fp_Vn
  real, dimension(nc,nc) :: Fp_nn
  logical :: call_init
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()

  if (present(doInit)) then
     call_init = doInit
  else
     call_init = .true.
  endif
  if (call_init) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     n = (/1.2/)
  endif
  n0 = n
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif

  eps = 1.0e-8

  r_cut = 2.0
  quantum_correction=2
  quantum_correction_hs=0
  enable_truncation_correction = .true.
  enable_shift_correction = .true.

  svrm_var => get_saftvrmie_var()
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calc_delta_Ac(nc,T,V,n,r_cut,svrm_var,&
       F,F_T,F_V,F_n,F_TT,F_VV,F_TV,F_Tn,F_Vn,F_nn)
  call calc_delta_Ac(nc,T,V+V*eps,n,r_cut,svrm_var,&
       Fp,Fp_T,Fp_V,Fp_n,Fp_TT,Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,"Testing the residual reduced Helmholtz energy"
  print *,"V"
  print *,F
  print *,F_V,(Fp - F)/(V*eps)
  print *,F_VV,(Fp_V - F_V)/(V*eps)
  print *,F_TV,(Fp_T - F_T)/(V*eps)
  print *,F_Vn,(Fp_n - F_n)/(V*eps)
  !stop
  print *,"n1"
  n(1) = n(1) + eps
  call calc_delta_Ac(nc,T,V,n,r_cut,svrm_var,&
       Fp,Fp_T,Fp_V,Fp_n,Fp_TT,Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)
  print *,F_n(1),(Fp - F)/eps
  print *,F_Tn(1),(Fp_T - F_T)/eps
  print *,F_Vn(1),(Fp_V - F_V)/eps
  print *,F_nn(1,:),(Fp_n - F_n)/eps

  print *,"T"
  n = n0
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,svrm_var)
  call calc_delta_Ac(nc,T+T*eps,V,n,r_cut,svrm_var,&
       Fp,Fp_T,Fp_V,Fp_n,Fp_TT,Fp_VV,Fp_TV,Fp_Tn,Fp_Vn,Fp_nn)

  print *,F_T,(Fp - F)/(T*eps)
  print *,F_TT,(Fp_T - F_T)/(T*eps)
  print *,F_TV,(Fp_V - F_V)/(T*eps)
  print *,F_Tn,(Fp_n - F_n)/(T*eps)

  stop
end subroutine test_delta_Ac

subroutine print_saftvrmie_model(Ti,Vi,ni,doInit)
  use thermopack_constants
  use thermopack_var, only: nc, get_active_thermo_model, thermo_model
  use saftvrmie_containers
  use saftvrmie_hardsphere
  use saftvrmie_interface
  use saftvrmie_options
  !use thermopack_constants, only: Rgas
  implicit none
  real, optional, intent(in) :: Ti,Vi,ni(nc)
  logical, optional, intent(in) :: doInit
  ! Locals
  real :: n(nc),T,V,F,F_n(nc)
  logical :: call_init
  type(thermo_model), pointer :: act_mod_ptr
  class(saftvrmie_eos), pointer :: eos
  act_mod_ptr => get_active_thermo_model()
  eos => get_saftvrmie_eos_pointer(act_mod_ptr%eos(1)%p_eos)
  quantum_correction=0
  if (present(doInit)) then
     call_init = doInit
  else
     call_init = .true.
  endif
  if (call_init) then
      call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  endif
  if (present(ni)) then
     n = ni
  else
     n = (/0.2,1.2/)
  endif
  if (present(Vi)) then
     V = Vi
  else
     V = 1.0e-4
  endif
  if (present(Ti)) then
     T = Ti
  else
     T = 5.0
  endif
  !p = pressure(T,v,n)

  print *,"Printing SAFT-VR Mie sub-models at:"
  print *,"T  ",T
  print *,"v  ",v
  print *,"n  ",n
  !print *,"p  ", p
  !print *,"z  ", p*v/sum(n)/Rgas/T
  print *
  enable_hs = .true.
  enable_A1 = .true.
  enable_A3 = .true.
  enable_A2 = .true.
  enable_chain = .true.
  enable_truncation_correction = .false.
  enable_hs_extra = .false.

  call calcFresSAFTVRMie(eos,nc,T,V,n,F=F,F_n=F_n)
  !print *,"F,-p*v/Rgas/T+sum(F_n*n) + sum(n)",F,-p*v/Rgas/T+sum(F_n*n) + sum(n)
  !print *
  print *,"Entire model:"
  call print_fres(T,V,n)

  enable_hs = .true.
  enable_A1 = .false.
  enable_A3 = .false.
  enable_A2 = .false.
  enable_chain = .false.
  enable_truncation_correction = .false.
  enable_hs_extra = .false.

  print *,"HS term:"
  call print_fres(T,V,n)

  enable_hs = .false.
  enable_A1 = .true.
  enable_A3 = .false.
  enable_A2 = .false.
  enable_chain = .false.
  enable_truncation_correction = .false.
  enable_hs_extra = .false.

  print *,"A1 term:"
  call print_fres(T,V,n)

  enable_hs = .false.
  enable_A1 = .false.
  enable_A3 = .false.
  enable_A2 = .true.
  enable_chain = .false.
  enable_truncation_correction = .false.
  enable_hs_extra = .false.

  print *,"A2 term:"
  call print_fres(T,V,n)
  call print_a2chi(T,V,n)

  enable_hs = .false.
  enable_A1 = .false.
  enable_A3 = .true.
  enable_A2 = .false.
  enable_chain = .false.
  enable_truncation_correction = .false.
  enable_hs_extra = .false.

  print *,"A3 term:"
  call print_fres(T,V,n)

  enable_hs = .false.
  enable_A1 = .false.
  enable_A3 = .false.
  enable_A2 = .false.
  enable_chain = .true.
  enable_truncation_correction = .false.
  enable_hs_extra = .false.

  print *,"Chain term:"
  call print_fres(T,V,n)

  stop
end subroutine print_saftvrmie_model

subroutine print_fres(T,V,n)
  use thermopack_var, only: nc, thermo_model, get_active_thermo_model
  use saftvrmie_interface
  use saftvrmie_containers
  implicit none
  real, intent(in) :: T,V,n(nc)
  ! Locals
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  integer :: i
  type(thermo_model), pointer :: act_mod_ptr
  class(saftvrmie_eos), pointer :: eos
  act_mod_ptr => get_active_thermo_model()
  eos => get_saftvrmie_eos_pointer(act_mod_ptr%eos(1)%p_eos)
  call calcFresSAFTVRMie(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)

  print *,"Residual reduced Helmholtz energy"
  print *,"F  ",F
  print *,"F_V ",F_V
  print *,"F_VV",F_VV
  print *,"F_T ",F_T
  print *,"F_TT",F_TT
  print *,"F_TV",F_TV
  print *,"F_Vn",F_Vn
  print *,"F_Tn",F_Tn
  print *,"F_n ",F_n
  do i=1,nc
     print *,"F_nn",F_nn(i,:)
  enddo
  print *
end subroutine print_fres

subroutine print_a2chi(T,V,n)
  use thermopack_var, only: nc, thermo_model, get_active_thermo_model
  use saftvrmie_interface
  use saftvrmie_containers
  implicit none
  real, intent(in) :: T,V,n(nc)
  ! Locals
  real :: F,F_T,F_V,F_TT,F_VV,F_TV
  real, dimension(nc) :: F_n,F_Tn,F_Vn
  real, dimension(nc,nc) :: F_nn
  integer :: i, j
  type(thermo_model), pointer :: act_mod_ptr
  class(saftvrmie_eos), pointer :: eos
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  eos => get_saftvrmie_eos_pointer(act_mod_ptr%eos(1)%p_eos)
  svrm_var => get_saftvrmie_var()
  call calcFresSAFTVRMie(eos,nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)

  do j=1,2
     print *,"a2/(1-chi)"
     print *,"a2 ",svrm_var%a2chij%am(j,j)
     print *,"a2_T ",svrm_var%a2chij%am_T(j,j)
     print *,"a2_V ",svrm_var%a2chij%am_V(j,j)
     print *,"a2_TT ",svrm_var%a2chij%am_TT(j,j)
     print *,"a2_VV ",svrm_var%a2chij%am_VV(j,j)
     print *,"a2_TV ",svrm_var%a2chij%am_TV(j,j)
     print *,"a2_VVV ",svrm_var%a2chij%am_VVV(j,j)
     print *,"a2_VVT ",svrm_var%a2chij%am_VVT(j,j)
     print *,"a2_VTT ",svrm_var%a2chij%am_VTT(j,j)
     print *,"a2_n ",svrm_var%a2chij%am_n(:,j,j)
     print *,"a2_Tn ",svrm_var%a2chij%am_Tn(:,j,j)
     print *,"a2_Vn ",svrm_var%a2chij%am_Vn(:,j,j)
     print *,"a2_VVn ",svrm_var%a2chij%am_VVn(:,j,j)
     print *,"a2_VTn ",svrm_var%a2chij%am_VTn(:,j,j)
     do i=1,nc
        print *,"a2_nn",svrm_var%a2chij%am_nn(i,:,j,j)
     enddo
     do i=1,nc
        print *,"a2_Vnn",svrm_var%a2chij%am_Vnn(i,:,j,j)
     enddo
     print *
  enddo
end subroutine print_a2chi

subroutine test_a2chi(T,V,n)
  use thermopack_var, only: nc
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_containers
  implicit none
  real, intent(in) :: T,V,n(nc)
  ! Locals
  real :: n1(nc)
  real :: F2,F2_T,F2_V,F2_TT,F2_VV,F2_TV
  real, dimension(nc) :: F2_n,F2_Tn,F2_Vn
  real, dimension(nc,nc) :: F2_nn
  real :: eps
  type(saftvrmie_var_container) :: s_vc
  integer :: j
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()

  j = 1
  call allocate_saftvrmie_var_container(nc,s_vc)
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcA2(nc,T,V,n,svrm_var,&
       F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n,a2_TT=F2_TT,&
       a2_VV=F2_VV,a2_TV=F2_TV,a2_Tn=F2_Tn,a2_Vn=F2_Vn,a2_nn=F2_nn)
  print *,"Testing a2/(1-chi)"
  print *,"a2 ",svrm_var%a2chij%am(j,j)

  eps = 1.0e-7
  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,s_vc)
  call calcA2(nc,T,V+V*eps,n,s_vc,&
       F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n,a2_TT=F2_TT,&
       a2_VV=F2_VV,a2_TV=F2_TV,a2_Tn=F2_Tn,a2_Vn=F2_Vn,a2_nn=F2_nn)
  print *,"V"
  print *,"a2_V ",svrm_var%a2chij%am_V(j,j),(s_vc%a2chij%am(j,j)-svrm_var%a2chij%am(j,j))/(eps*V)
  print *,"a2_VV ",svrm_var%a2chij%am_VV(j,j),(s_vc%a2chij%am_V(j,j)-svrm_var%a2chij%am_V(j,j))/(eps*V)
  print *,"a2_VVV ",svrm_var%a2chij%am_VVV(j,j),(s_vc%a2chij%am_VV(j,j)-svrm_var%a2chij%am_VV(j,j))/(eps*V)
  print *,"a2_TV ",svrm_var%a2chij%am_TV(j,j),(s_vc%a2chij%am_T(j,j)-svrm_var%a2chij%am_T(j,j))/(eps*V)
  print *,"a2_VVT ",svrm_var%a2chij%am_VVT(j,j),(s_vc%a2chij%am_TV(j,j)-svrm_var%a2chij%am_TV(j,j))/(eps*V)
  print *,"a2_VTT ",svrm_var%a2chij%am_VTT(j,j),(s_vc%a2chij%am_TT(j,j)-svrm_var%a2chij%am_TT(j,j))/(eps*V)
  print *,"a2_Vn ",svrm_var%a2chij%am_Vn(:,j,j),(s_vc%a2chij%am_n(:,j,j)-svrm_var%a2chij%am_n(:,j,j))/(eps*V)
  print *,"a2_VVn ",svrm_var%a2chij%am_VVn(:,j,j),(s_vc%a2chij%am_Vn(:,j,j)-svrm_var%a2chij%am_Vn(:,j,j))/(eps*V)
  print *,"a2_VTn ",svrm_var%a2chij%am_VTn(:,j,j),(s_vc%a2chij%am_Tn(:,j,j)-svrm_var%a2chij%am_Tn(:,j,j))/(eps*V)

  print *,"T"
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,s_vc)
  call calcA2(nc,T+T*eps,V,n,s_vc,&
       F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n,a2_TT=F2_TT,&
       a2_VV=F2_VV,a2_TV=F2_TV,a2_Tn=F2_Tn,a2_Vn=F2_Vn,a2_nn=F2_nn)
  print *,"a2_T ",svrm_var%a2chij%am_T(j,j),(s_vc%a2chij%am(j,j)-svrm_var%a2chij%am(j,j))/(eps*T)
  print *,"a2_TT ",svrm_var%a2chij%am_VV(j,j),(s_vc%a2chij%am_T(j,j)-svrm_var%a2chij%am_T(j,j))/(eps*T)
  print *,"a2_TV ",svrm_var%a2chij%am_TV(j,j),(s_vc%a2chij%am_V(j,j)-svrm_var%a2chij%am_V(j,j))/(eps*T)
  print *,"a2_VVT ",svrm_var%a2chij%am_VVT(j,j),(s_vc%a2chij%am_VV(j,j)-svrm_var%a2chij%am_VV(j,j))/(eps*T)
  print *,"a2_VTT ",svrm_var%a2chij%am_VTT(j,j),(s_vc%a2chij%am_TV(j,j)-svrm_var%a2chij%am_TV(j,j))/(eps*T)
  print *,"a2_Tn ",svrm_var%a2chij%am_Tn(:,j,j),(s_vc%a2chij%am_n(:,j,j)-svrm_var%a2chij%am_n(:,j,j))/(eps*T)
  print *,"a2_VTn ",svrm_var%a2chij%am_VTn(:,j,j),(s_vc%a2chij%am_Vn(:,j,j)-svrm_var%a2chij%am_Vn(:,j,j))/(eps*T)

  print *,"n"
  n1 = n
  n1(j) = n1(j) + n1(j)*eps
  call preCalcSAFTVRMie(nc,T,V,n1,2,s_vc)
  call calcA2(nc,T,V,n1,s_vc,&
       F2,a2_T=F2_T,a2_V=F2_V,a2_n=F2_n,a2_TT=F2_TT,&
       a2_VV=F2_VV,a2_TV=F2_TV,a2_Tn=F2_Tn,a2_Vn=F2_Vn,a2_nn=F2_nn)
  print *,"a2_n ",svrm_var%a2chij%am_n(:,j,j),(s_vc%a2chij%am(j,j)-svrm_var%a2chij%am(j,j))/(eps*n(j))
  print *,"a2_nn ",svrm_var%a2chij%am_nn(:,j,j,j),(s_vc%a2chij%am_n(:,j,j)-svrm_var%a2chij%am_n(:,j,j))/(eps*n(j))
  print *,"a2_Tn ",svrm_var%a2chij%am_Tn(:,j,j),(s_vc%a2chij%am_T(j,j)-svrm_var%a2chij%am_T(j,j))/(eps*n(j))
  print *,"a2_Vn ",svrm_var%a2chij%am_Vn(:,j,j),(s_vc%a2chij%am_V(j,j)-svrm_var%a2chij%am_V(j,j))/(eps*n(j))
  print *,"a2_VVn",svrm_var%a2chij%am_VVn(:,j,j),(s_vc%a2chij%am_VV(j,j)-svrm_var%a2chij%am_VV(j,j))/(eps*n(j))
  print *,"a2_Vnn ",svrm_var%a2chij%am_Vnn(:,j,j,j),&
       (s_vc%a2chij%am_Vn(:,j,j)-svrm_var%a2chij%am_Vn(:,j,j))/(eps*n(j))
  print *,"a2_VTn ",svrm_var%a2chij%am_VTn(:,j,j),(s_vc%a2chij%am_TV(j,j)-svrm_var%a2chij%am_TV(j,j))/(eps*n(j))

end subroutine test_a2chi

subroutine test_mixKhs(T,V,n)
  use thermopack_var, only: nc
  use saftvrmie_interface
  use saftvrmie_dispersion
  use saftvrmie_containers
  implicit none
  real, intent(in) :: T,V,n(nc)
  ! Locals
  real :: n1(nc)
  real :: eps, e,K,L
  real :: B,Be,Bee,Bk,Bkk,Bkl,Bke,Bl,Bll,Ble
  real :: B2,B2e,B2ee,B2k,B2kk,B2kl,B2ke,B2l,B2ll,B2le
  type(saftvrmie_var_container) :: s_vc, s_vc_pure
  integer :: j, i
  type(saftvrmie_zeta) :: M(3), zL, zK, M2(3), zL2, zK2
  type(saftvrmie_var_container), pointer :: svrm_var
  svrm_var => get_saftvrmie_var()
  eps = 1.0e-5
  call allocate_saftvrmie_var_container(nc,s_vc)
  call allocate_saftvrmie_zeta(nc,zL)
  call allocate_saftvrmie_zeta(nc,zK)
  call allocate_saftvrmie_zeta(nc,zL2)
  call allocate_saftvrmie_zeta(nc,zK2)
  do j=1,3
     call allocate_saftvrmie_zeta(nc,M(j))
     call allocate_saftvrmie_zeta(nc,M2(j))
  enddo
  i = 1
  j = 2
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call mbar(nc,n,svrm_var%dhs,M)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,s_vc)
  call mbar(nc,n,s_vc%dhs,M2)
  print *,"Mt",M(i)%zx_T,(M2(i)%zx-M(i)%zx)/(T*eps)
  print *,"Mtt",M(i)%zx_TT,(M2(i)%zx_T-M(i)%zx_T)/(T*eps)
  print *,"Mtn",M(i)%zx_Tn,(M2(i)%zx_n-M(i)%zx_n)/(T*eps)
  n1 = n
  n1(j) = n1(j) + n1(j)*eps
  call preCalcSAFTVRMie(nc,T,V,n1,2,s_vc)
  call mbar(nc,n1,s_vc%dhs,M2)
  print *,"Mn",M(i)%zx_n(j),(M2(i)%zx-M(i)%zx)/(n(j)*eps)
  print *,"Mnn",M(i)%zx_nn,(M2(i)%zx_n-M(i)%zx_n)/(n(j)*eps)
  print *,"Mtn",M(i)%zx_Tn(j),(M2(i)%zx_T-M(i)%zx_T)/(n(j)*eps)

  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call KandL(nc,n,svrm_var%dhs,zK,zL)
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,s_vc)
  call KandL(nc,n,s_vc%dhs,zK2,zL2)
  print *,"Kt",zK%zx_T,(zK2%zx-zK%zx)/(T*eps)
  print *,"Ktt",zK%zx_TT,(zK2%zx_T-zK%zx_T)/(T*eps)
  print *,"Ktv",zK%zx_TV,(zK2%zx_V-zK%zx_V)/(T*eps)
  print *,"Ktn",zK%zx_Tn,(zK2%zx_n-zK%zx_n)/(T*eps)

  print *,"Lt",zL%zx_T,(zL2%zx-zL%zx)/(T*eps)
  print *,"Ltt",zL%zx_TT,(zL2%zx_T-zL%zx_T)/(T*eps)
  print *,"Ltv",zL%zx_TV,(zL2%zx_V-zL%zx_V)/(T*eps)
  print *,"Ltn",zL%zx_Tn,(zL2%zx_n-zL%zx_n)/(T*eps)
  !
  n1 = n
  n1(j) = n1(j) + n1(j)*eps
  call preCalcSAFTVRMie(nc,T,V,n1,2,s_vc)
  call KandL(nc,n1,s_vc%dhs,zK2,zL2)
  print *,"Kn",zK%zx_n(j),(zK2%zx-zK%zx)/(n1(j)*eps)
  print *,"Knn",zK%zx_nn(:,j),(zK2%zx_n-zK%zx_n)/(n1(j)*eps)
  print *,"Ktn",zK%zx_Tn(j),(zK2%zx_T-zK%zx_T)/(n1(j)*eps)
  print *,"Kvn",zK%zx_Vn(j),(zK2%zx_V-zK%zx_V)/(n1(j)*eps)
  print *,"Ln",zL%zx_n(j),(zL2%zx-zL%zx)/(n1(j)*eps)
  print *,"Lnn",zL%zx_nn(:,j),(zL2%zx_n-zL%zx_n)/(n1(j)*eps)
  print *,"Ltn",zL%zx_Tn(j),(zL2%zx_T-zL%zx_T)/(n1(j)*eps)
  print *,"Lvn",zL%zx_Vn(j),(zL2%zx_v-zL%zx_v)/(n1(j)*eps)
  ! stop


  eps = 1.0e-6
  e=0.79335635806161309
  K=0.83333350732520994
  L=0.77160515307641664
  call betabar(e,K,L,B,Be,Bee,Bk,Bkk,Bkl,Bke,Bl,Bll,Ble)
  call betabar(e+e*eps,K,L,B2,B2e,B2ee,B2k,B2kk,B2kl,B2ke,B2l,B2ll,B2le)
  print *,"Be",Be,(B2-B)/(e*eps)
  print *,"Bee",Bee,(B2e-Be)/(e*eps)
  print *,"BKe",Bke,(B2k-Bk)/(e*eps)
  print *,"BLe",Ble,(B2l-Bl)/(e*eps)
  call betabar(e,K+K*eps,L,B2,B2e,B2ee,B2k,B2kk,B2kl,B2ke,B2l,B2ll,B2le)
  print *,"Bk",Bk,(B2-B)/(k*eps)
  print *,"Bkk",Bkk,(B2k-Bk)/(k*eps)
  print *,"BKe",Bke,(B2e-Be)/(k*eps)
  print *,"BKL",Bkl,(B2l-Bl)/(k*eps)
  call betabar(e,K,L+L*eps,B2,B2e,B2ee,B2k,B2kk,B2kl,B2ke,B2l,B2ll,B2le)
  print *,"Bl",Bl,(B2-B)/(l*eps)
  print *,"Bll",Bll,(B2l-Bl)/(l*eps)
  print *,"Ble",Ble,(B2e-Be)/(l*eps)
  print *,"BKL",Bkl,(B2k-Bk)/(l*eps)
  ! stop
  j = 1
  call preCalcSAFTVRMie(nc,T,V,n,2,svrm_var)
  call calcMixKhsTVn(nc,T,V,n,2,svrm_var)
  print *,"Khs num",svrm_var%Khs%zx
  call calcMixKhsTVn(nc,T,V,n,2,svrm_var)
  print *,"Testing test_mixKhs"
  print *,"Khs ",svrm_var%Khs%zx
  call allocate_saftvrmie_var_container(nc,s_vc_pure)
  call preCalcSAFTVRMie(nc,T,V,n,2,s_vc_pure)
  call calcKhsTVnCS(nc,T,V,n,2,s_vc_pure)
  print *,"Khs CS",s_vc_pure%Khs%zx

  call preCalcSAFTVRMie(nc,T,V+V*eps,n,2,s_vc)
  call calcMixKhsTVn(nc,T,V+V*eps,n,2,s_vc)
  print *,"V"
  print *,"Khs_V ",svrm_var%Khs%zx_V,(s_vc%Khs%zx-svrm_var%Khs%zx)/(eps*V),s_vc_pure%Khs%zx_V
  print *,"Khs_VV ",svrm_var%Khs%zx_VV,(s_vc%Khs%zx_V-svrm_var%Khs%zx_V)/(eps*V),s_vc_pure%Khs%zx_VV
  !print *,"Khs_VVV ",svrm_var%Khs%zx_VVV,(s_vc%Khs%zx_VV-svrm_var%Khs%zx_VV)/(eps*V)
  print *,"Khs_TV ",svrm_var%Khs%zx_TV,(s_vc%Khs%zx_T-svrm_var%Khs%zx_T)/(eps*V),s_vc_pure%Khs%zx_TV
  !print *,"Khs_VVT ",svrm_var%Khs%zx_VVT(j,j),(s_vc%Khs%zx_TV(j,j)-svrm_var%Khs%zx_TV(j,j))/(eps*V)
  !print *,"Khs_VTT ",svrm_var%Khs%zx_VTT(j,j),(s_vc%Khs%zx_TT(j,j)-svrm_var%Khs%zx_TT(j,j))/(eps*V)
  print *,"Khs_Vn ",svrm_var%Khs%zx_Vn(j),(s_vc%Khs%zx_n(j)-svrm_var%Khs%zx_n(j))/(eps*V),s_vc_pure%Khs%zx_Vn(j)
  !print *,"Khs_VVn ",svrm_var%Khs%zx_VVn(:,j,j),(s_vc%Khs%zx_Vn(:,j,j)-svrm_var%Khs%zx_Vn(:,j,j))/(eps*V)
  !print *,"Khs_VTn ",svrm_var%Khs%zx_VTn(:,j,j),(s_vc%Khs%zx_Tn(:,j,j)-svrm_var%Khs%zx_Tn(:,j,j))/(eps*V)

  print *,"T"
  call preCalcSAFTVRMie(nc,T+T*eps,V,n,2,s_vc)
  call calcMixKhsTVn(nc,T+T*eps,V,n,2,s_vc)
  print *,"Khs_T ",svrm_var%Khs%zx_T,(s_vc%Khs%zx-svrm_var%Khs%zx)/(eps*T),s_vc_pure%Khs%zx_T
  print *,"Khs_TT ",svrm_var%Khs%zx_TT,(s_vc%Khs%zx_T-svrm_var%Khs%zx_T)/(eps*T),s_vc_pure%Khs%zx_TT
  print *,"Khs_TV ",svrm_var%Khs%zx_TV,(s_vc%Khs%zx_V-svrm_var%Khs%zx_V)/(eps*T),s_vc_pure%Khs%zx_TV
  !print *,"Khs_VVT ",svrm_var%Khs%zx_VVT(j,j),(s_vc%Khs%zx_VV(j,j)-svrm_var%Khs%zx_VV(j,j))/(eps*T)
  !print *,"Khs_VTT ",svrm_var%Khs%zx_VTT(j,j),(s_vc%Khs%zx_TV(j,j)-svrm_var%Khs%zx_TV(j,j))/(eps*T)
  print *,"Khs_Tn ",svrm_var%Khs%zx_Tn(j),(s_vc%Khs%zx_n(j)-svrm_var%Khs%zx_n(j))/(eps*T),s_vc_pure%Khs%zx_Tn(j)
  !print *,"Khs_VTn ",svrm_var%Khs%zx_VTn(:,j,j),(s_vc%Khs%zx_Vn(:,j,j)-svrm_var%Khs%zx_Vn(:,j,j))/(eps*T)

  print *,"n1"
  n1 = n
  n1(j) = n1(j) + n1(j)*eps
  call preCalcSAFTVRMie(nc,T,V,n1,2,s_vc)
  call calcMixKhsTVn(nc,T,V,n1,2,s_vc)
  print *,"Khs_n ",svrm_var%Khs%zx_n(j),(s_vc%Khs%zx-svrm_var%Khs%zx)/(eps*n(j)),s_vc_pure%Khs%zx_n(j)
  print *,"Khs_nn ",svrm_var%Khs%zx_nn(:,j),(s_vc%Khs%zx_n-svrm_var%Khs%zx_n)/(eps*n(j)),s_vc_pure%Khs%zx_nn(:,j)
  print *,"Khs_Tn ",svrm_var%Khs%zx_Tn(j),(s_vc%Khs%zx_T-svrm_var%Khs%zx_T)/(eps*n(j)),s_vc_pure%Khs%zx_Tn(j)
  print *,"Khs_Vn ",svrm_var%Khs%zx_Vn(j),(s_vc%Khs%zx_V-svrm_var%Khs%zx_V)/(eps*n(j)),s_vc_pure%Khs%zx_Vn(j)
  !print *,"Khs_VVn",svrm_var%Khs%zx_VVn(:,j,j),(s_vc%Khs%zx_VV(j,j)-svrm_var%Khs%zx_VV(j,j))/(eps*n(j))
  !print *,"Khs_Vnn ",svrm_var%Khs%zx_Vnn(:,j,j,j),&
  !     (s_vc%Khs%zx_Vn(:,j,j)-svrm_var%Khs%zx_Vn(:,j,j))/(eps*n(j))
  !print *,"Khs_VTn ",svrm_var%Khs%zx_VTn(:,j,j),(s_vc%Khs%zx_TV(j,j)-svrm_var%Khs%zx_TV(j,j))/(eps*n(j))

  if (nc > 1) then
     print *,"n2"
     n1 = n
     j = 2
     n1(j) = n1(j) + n1(j)*eps
     call preCalcSAFTVRMie(nc,T,V,n1,2,s_vc)
     call calcMixKhsTVn(nc,T,V,n1,2,s_vc)
     print *,"Khs_n ",svrm_var%Khs%zx_n(j),(s_vc%Khs%zx-svrm_var%Khs%zx)/(eps*n(j)),s_vc_pure%Khs%zx_n(j)
     print *,"Khs_nn ",svrm_var%Khs%zx_nn(:,j),(s_vc%Khs%zx_n-svrm_var%Khs%zx_n)/(eps*n(j)),s_vc_pure%Khs%zx_nn(:,j)
     print *,"Khs_Tn ",svrm_var%Khs%zx_Tn(j),(s_vc%Khs%zx_T-svrm_var%Khs%zx_T)/(eps*n(j)),s_vc_pure%Khs%zx_Tn(j)
     print *,"Khs_Vn ",svrm_var%Khs%zx_Vn(j),(s_vc%Khs%zx_V-svrm_var%Khs%zx_V)/(eps*n(j)),s_vc_pure%Khs%zx_Vn(j)
     !print *,"Khs_VVn",svrm_var%Khs%zx_VVn(:,j,j),(s_vc%Khs%zx_VV(j,j)-svrm_var%Khs%zx_VV(j,j))/(eps*n(j))
     !print *,"Khs_Vnn ",svrm_var%Khs%zx_Vnn(:,j,j,j),&
     !     (s_vc%Khs%zx_Vn(:,j,j)-svrm_var%Khs%zx_Vn(:,j,j))/(eps*n(j))
     !print *,"Khs_VTn ",svrm_var%Khs%zx_VTn(:,j,j),(s_vc%Khs%zx_TV(j,j)-svrm_var%Khs%zx_TV(j,j))/(eps*n(j))
  endif
end subroutine test_mixKhs
