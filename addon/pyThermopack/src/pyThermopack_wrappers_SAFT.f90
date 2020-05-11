subroutine initsaft(ncomp,eos,mixing,alpha,comp_string,nphases,saft_setno)
  use eoslibinit, only: init_thermo
  use parameters, only: liq_vap_discr_method

  implicit none
  integer, intent(in)           :: ncomp        !< Number of components
  character(len=*), intent(in)  :: eos          !< String defining equation of state
  character(len=*), intent(in)  :: mixing       !< String defining mixing rules
  character(len=*), intent(in)  :: alpha        !< String defining alpha correlation
  character(len=*), intent(in)  :: comp_string  !< String defining componets. Comma separated.
  integer, intent(in)           :: nphases      !< Number of phases
  real, intent(in)  :: saft_setno(ncomp)        !< Parameter set to use for SAFT EoS.

  integer :: i, int_array(ncomp)
  do i=1,ncomp
     int_array(i) = int(saft_setno(i))
  end do

  call init_thermo("Thermopack",eos,mixing,alpha,ncomp,comp_string,nphases, &
       saft_setno=int_array)

end subroutine initsaft

subroutine set_cpa_formulation(simplified)
  use saft_interface, only: setCPAformulation
  implicit none
  ! Input:
  logical, intent(in) :: simplified

  call setCPAformulation(simplified)
end subroutine set_cpa_formulation


subroutine set_kij_cpa(i,j,kij)
  ! Set the kij CPA parameter.
  !
  ! Note that this sets k(j,i)=k(i,j), so it enforces
  ! symmetry in the kij matrix.
  !
  use saft_interface, only: cpa_set_kij
  implicit none
  ! Input:
  integer,    intent(in)    :: i
  integer,    intent(in)    :: j
  real,       intent(in)    :: kij(2)
  if (i<1 .or. j<1) call stoperror("set_kij_cpa::i,j must be 1 or higher")
  call cpa_set_kij(i,j,kij)
end subroutine set_kij_cpa

subroutine set_kij_pc_saft(i,j,kij)
  ! Set the kij PC-SAFT parameter.
  !
  ! Note that this sets k(j,i)=k(i,j), so it enforces
  ! symmetry in the kij matrix.
  !
  use saft_interface, only: pc_saft_set_kij
  implicit none
  ! Input:
  integer,    intent(in)    :: i
  integer,    intent(in)    :: j
  real,       intent(in)    :: kij

  if (i<1 .or. j<1) call stoperror("set_kij_pc_saft::i,j must be 1 or higher")
  call pc_saft_set_kij(i,j,kij)

end subroutine set_kij_pc_saft


subroutine set_kij_pc_saft_asym(i,j,kij)
  ! Set the kij PC-SAFT parameter, not assuming kij=kji.
  !
  !
  !
  use saft_interface, only: pc_saft_set_kij_asym
  implicit none
  ! Input:
  integer,    intent(in)    :: i
  integer,    intent(in)    :: j
  real,       intent(in)    :: kij

  call pc_saft_set_kij_asym(i,j,kij)

end subroutine set_kij_pc_saft_asym

subroutine get_kij_pc_saft(i,j,kij)
  ! Get the kij PC-SAFT parameter.
  !
  use saft_interface, only: pc_saft_get_kij
  implicit none
  ! Input:
  integer,    intent(in)    :: i
  integer,    intent(in)    :: j
  ! Output
  real,       intent(out)    :: kij

  call pc_saft_get_kij(i,j,kij)

end subroutine get_kij_pc_saft

subroutine get_pure_deBoer_saftvrmie(ic,Lambda)
  use tpvar, only: cbeos
  use parameters, only: nc
  use saftvrmie_containers, only: get_saftvrmie_pure_fluid_deBoer
  use eosdata, only: eosSAFT_VR_MIE, cpaSRK, cpaPR, eosPC_SAFT
  implicit none
  ! Input:
  integer,             intent(in)  :: ic !< Component number
  ! Output:
  real, intent(out) :: Lambda  !< Parameter array
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
    call stoperror("get_pure_deBoer_saftvrmie::only for saftvrmie")
  end if
  call get_saftvrmie_pure_fluid_deBoer(ic, Lambda)
end subroutine get_pure_deBoer_saftvrmie

subroutine set_pure_deBoer_saftvrmie(ic,Lambda)
  use tpvar, only: cbeos
  use parameters, only: nc
  use saftvrmie_containers, only: set_saftvrmie_pure_fluid_deBoer
  use eosdata, only: eosSAFT_VR_MIE, cpaSRK, cpaPR, eosPC_SAFT
  implicit none
  ! Input:
  integer,             intent(in)  :: ic !< Component number
  ! Output:
  real, intent(in) :: Lambda  !< Parameter array
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
    call stoperror("set_pure_deBoer_saftvrmie::only for saftvrmie")
  end if
  if (ic<1) call stoperror("set_pure_deBoer_saftvrmie::ic must be 1 or higher")

  call set_saftvrmie_pure_fluid_deBoer(ic, Lambda)
end subroutine set_pure_deBoer_saftvrmie

subroutine get_kij_saftvrmie(ic,jc,kij)
  use tpvar, only: cbeos
  use eosdata, only: eosSAFT_VR_MIE
  use saftvrmie_containers, only: get_saftvrmie_eps_kij
  implicit none
  ! Input:
  integer,             intent(in)  :: ic,jc !< Component numbers
  ! Output:
  real, intent(out) :: kij  !< Parameter array
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
    call stoperror("get_kij_saftvrmie::only for saftvrmie")
  end if
  call get_saftvrmie_eps_kij(ic,jc,kij)
end subroutine get_kij_saftvrmie

subroutine set_kij_saftvrmie(ic,jc,kij)
  use tpvar, only: cbeos
  use eosdata, only: eosSAFT_VR_MIE
  use saftvrmie_containers, only: set_saftvrmie_eps_kij
  implicit none
  ! Input:
  integer,             intent(in)  :: ic,jc !< Component numbers
  ! Output:
  real, intent(in) :: kij  !< Interaction parameter
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
    call stoperror("set_kij_saftvrmie::only for saftvrmie")
  end if
  call set_saftvrmie_eps_kij(ic,jc,kij)
end subroutine set_kij_saftvrmie

subroutine get_gammaij_saftvrmie(ic,jc,gammaij)
  use tpvar, only: cbeos
  use eosdata, only: eosSAFT_VR_MIE
  use saftvrmie_containers, only: get_saftvrmie_lr_gammaij
  implicit none
  ! Input:
  integer,             intent(in)  :: ic,jc !< Component numbers
  ! Output:
  real, intent(out) :: gammaij  !< Parameter array
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
    call stoperror("get_gammaij_saftvrmie::only for saftvrmie")
  end if
  call get_saftvrmie_lr_gammaij(ic,jc,gammaij)
end subroutine get_gammaij_saftvrmie

subroutine set_gammaij_saftvrmie(ic,jc,gammaij)
  use tpvar, only: cbeos
  use eosdata, only: eosSAFT_VR_MIE
  use saftvrmie_containers, only: set_saftvrmie_lr_gammaij
  implicit none
  ! Input:
  integer,             intent(in)  :: ic,jc !< Component numbers
  ! Output:
  real, intent(in) :: gammaij  !< Interaction parameter
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
    call stoperror("set_gammaij_saftvrmie::only for saftvrmie")
  end if
  call set_saftvrmie_lr_gammaij(ic,jc,gammaij)
end subroutine set_gammaij_saftvrmie


subroutine set_lij_saftvrmie(ic,jc,lij)
  use tpvar, only: cbeos
  use eosdata, only: eosSAFT_VR_MIE
  use saftvrmie_containers, only: set_saftvrmie_sigma_lij
  implicit none
  ! Input:
  integer,             intent(in)  :: ic,jc !< Component numbers
  ! Output:
  real, intent(in) :: lij  !< Interaction parameter
  if (cbeos(1)%subeosidx /= eosSAFT_VR_MIE) then
     call stoperror("set_lij_saftvrmie::only for saftvrmie")
  end if
  call set_saftvrmie_sigma_lij(ic,jc,lij)
end subroutine set_lij_saftvrmie


subroutine get_pure_params_saft(ic,params)
  ! Get pure-component parameters for an EoS initialized with one component.
  ! Beware of ordering of the parameters, as well as their units!
  use tpvar, only: cbeos
  use parameters, only: nc
  use saft_interface, only: cpa_get_pure_params, pc_saft_get_pure_params, pets_get_pure_params
  use saftvrmie_containers, only: get_saftvrmie_pure_fluid_param
  use eosdata, only: eosBH_pert, cpaSRK, cpaPR, eosPC_SAFT, eosPeTS
  implicit none
  ! Input:
  integer,             intent(in)  :: ic !< Component number
  integer,             parameter   :: num_params = 5 !< Number of parameters
  ! Output:
  real,                intent(out) :: params(num_params)  !< Parameter array

  if (ic<1) call stoperror("get_pure_params_saft::ic must be 1 or higher")

  if (cbeos(1)%eosidx == cpaSRK .or. cbeos(1)%eosidx == cpaPR) then
    call cpa_get_pure_params(ic,params)
  else if (cbeos(1)%eosidx == eosPC_SAFT) then
    call pc_saft_get_pure_params(ic,params)
 else if (cbeos(1)%eosidx == eosPeTS) then
    call pets_get_pure_params(ic,params)
  else if (cbeos(1)%eosidx == eosBH_pert) then
    call get_saftvrmie_pure_fluid_param(ic,params(1),params(2),params(3),params(4),params(5))
  else
    call stoperror("get_pure_params_saft::not implemented for this eos")
  end if
end subroutine get_pure_params_saft

subroutine set_pure_params_saft(ic,num_params,params)
  ! Set pure-component parameters for an EoS initialized with one component.
  ! Beware of ordering of the parameters, as well as their units!
  use saft_interface, only: cpa_set_pure_params, pc_saft_set_pure_params, pets_set_pure_params
  use saftvrmie_containers, only: set_saftvrmie_pure_fluid_param
  use eosdata, only: eosBH_pert, cpaSRK, cpaPR, eosPC_SAFT, eosPeTS
  use tpvar, only: cbeos
  use parameters, only: nc
  implicit none
  ! Input:
  integer,             intent(in) :: ic !< Component number
  integer,             intent(in) :: num_params !< Number of parameters
  real,                intent(in) :: params(num_params)  !< Parameter array

  if (ic<1) call stoperror("set_pure_params_saft::ic must be 1 or higher")

  if (cbeos(1)%eosidx == cpaSRK .or. cbeos(1)%eosidx == cpaPR) then
     if ( num_params /= 5 ) call stoperror("set_pure_params::num_params should be 5 for CPA")
     call cpa_set_pure_params(ic,params)
  else if (cbeos(1)%eosidx == eosPC_SAFT) then
    if ( num_params /= 5 ) call stoperror("set_pure_params::num_params should be 5 for PC-SAFT")
     call pc_saft_set_pure_params(ic,params)
  else if (cbeos(1)%eosidx == eosPeTS) then
     call pets_set_pure_params(ic,params)
  else if (cbeos(1)%eosidx == eosBH_pert) then
     if ( num_params /= 5 ) call stoperror("set_pure_params::num_params should be 5 for SAFT-VR Mie")
     call set_saftvrmie_pure_fluid_param(ic,params(1),params(2),params(3),params(4),params(5))
  else
     call stoperror("set_pure_params::not implemented for this eos")
  end if
end subroutine set_pure_params_saft

subroutine set_pure_params_qsaft(ic, params, fh_order)
  ! Set relevant pure-component parameters for QSAFT EoS
  ! Beware of ordering of the parameters, as well as their units!
  use saftvrmie_containers, only: set_saftvrmie_pure_fluid_param
  use saftvrmie_options, only: quantum_correct_A2, &
       quantum_correction, quantum_correction_hs
  implicit none
  ! Input:
  integer,             intent(in) :: ic         !< Component number
  real,                intent(in) :: params(5)  !< Parameter array
  integer,             intent(in) :: fh_order   !< Order of FH correction
  quantum_correct_A2 = (fh_order>=1)
  quantum_correction = fh_order
  quantum_correction_hs = fh_order
  call set_saftvrmie_pure_fluid_param(ic,params(1),params(2),params(3),params(4),params(5))
end subroutine set_pure_params_qsaft

! Specify model options for CPA
subroutine cpa_model_control(simplified, elliot_rule)
  use saft_association, only: DELTA_COMBRULE, ELLIOT, STANDARD
  use saft_rdf, only: useSimplifiedCPA
  implicit none
  logical, intent(in) :: simplified, elliot_rule
  DELTA_COMBRULE = STANDARD
  if (elliot_rule) DELTA_COMBRULE = ELLIOT
  useSimplifiedCPA = simplified
  print *, "Simplified CPA ", simplified
  print *, "Elliot combrule", elliot_rule
end subroutine cpa_model_control

! Activte/decativate parts of the quantum corrected SAFT-VR Mie model
subroutine saftvrmie_model_control(en_A3,q_corr_A2,q_corr,q_corr_hs,en_A2)
  use saftvrmie_options, only: enable_A3, enable_A2, quantum_correct_A2, &
       quantum_correction, quantum_correction_hs
  implicit none
  ! Input
  integer, intent(in) :: q_corr,q_corr_hs
  logical, intent(in) :: en_A3,q_corr_A2,en_A2
  !
  enable_A3 = en_A3
  enable_A2 = en_A2
  quantum_correct_A2 = q_corr_A2
  quantum_correction = q_corr
  quantum_correction_hs = q_corr_hs
end subroutine saftvrmie_model_control

! Activate/deactivate the a terms
subroutine saftvrmie_toggle_a123(en_a1, en_a2, en_a3)
  use saftvrmie_options, only: enable_a1, enable_a2, enable_a3
  implicit none
  ! Input
  logical, intent(in) :: en_a1, en_a2, en_a3
  !
  enable_a1 = en_a1
  enable_a2 = en_a2
  enable_a3 = en_a3
end subroutine saftvrmie_toggle_a123


! Determine how binary interactions should be computed
subroutine saftvrmie_model_control_interaction(exact_cross_eff, exact_dhs)
  use saftvrmie_options, only: exact_binary_dhs, exact_crosspot_eff
  implicit none
  ! Input
  logical, intent(in) :: exact_cross_eff, exact_dhs
  !
  exact_crosspot_eff = exact_cross_eff
  exact_binary_dhs = exact_dhs
end subroutine saftvrmie_model_control_interaction

! Define how binary interaction energy should be computed
subroutine saftvrmie_set_eps_rule(use_eps_rule_Lafitte)
  use saftvrmie_options, only: use_epsrule_Lafitte
  implicit none
  ! Input
  logical, intent(in) :: use_eps_rule_Lafitte
  !
  use_epsrule_Lafitte = use_eps_rule_Lafitte
end subroutine saftvrmie_set_eps_rule

subroutine saftvrmie_set_a3_model(use_lafitte)
  use saftvrmie_options, only: a3_model, A3_LAFITTE, A3_SIJ_PREFAC
  implicit none
  ! Input
  logical, intent(in) :: use_lafitte
  !
  if (use_lafitte) then
    a3_model = A3_LAFITTE
  else
    a3_model = A3_SIJ_PREFAC
  end if
end subroutine saftvrmie_set_a3_model

subroutine saftvrmie_set_oivind_model()
  !> Choose whether to use Lafitte's hard sphere term, or the one by Santos et
  !> al. for non-additive hard spheres.
  use saftvrmie_options, only: enable_hs_extra, exact_binary_dhs, &
       hardsphere_eos, HS_EOS_ORIGINAL
  implicit none
  enable_hs_extra = .true.
  exact_binary_dhs=.false.
  hardsphere_eos = HS_EOS_ORIGINAL
end subroutine saftvrmie_set_oivind_model

subroutine saftvrmie_set_lafitte_model()
  !> Choose whether to use Lafitte's hard sphere term, or the one by Santos et
  !> al. for non-additive hard spheres.
  use saftvrmie_options, only: enable_hs_extra, exact_binary_dhs, &
       hardsphere_eos, HS_EOS_ORIGINAL
  implicit none
  enable_hs_extra = .false.
  exact_binary_dhs=.false.
  hardsphere_eos = HS_EOS_ORIGINAL
end subroutine saftvrmie_set_lafitte_model


subroutine saftvrmie_set_hs_model(use_CS_pure, mix_hs_model)
  !> Choose whether to use Lafitte's hard sphere term, or the one by Santos et
  !> al. for non-additive hard spheres.
  use saftvrmie_options, only: hardsphere_eos, pure_hs_EoS, HS_EOS_ORIGINAL, &
       HS_EOS_SANTOS, HS_EOS_PURE_DIJ, PURE_HS_CS, PURE_HS_CSK
  implicit none
  ! Input
  logical, intent(in) :: use_CS_pure
  integer, intent(in) :: mix_hs_model
  !
  if (use_CS_pure) then
    pure_hs_EoS = PURE_HS_CS
  else
     pure_hs_EoS = PURE_HS_CSK
  end if
  hardsphere_eos = mix_hs_model
  if (.not. (mix_hs_model == HS_EOS_ORIGINAL .or. &
       mix_hs_model == HS_EOS_SANTOS .or. &
       mix_hs_model == HS_EOS_PURE_DIJ)) then
    print *,"Wrong hard-sphere model specified"
  endif
end subroutine saftvrmie_set_hs_model

! Activte/decativate a1 from model
subroutine saftvrmie_a1_control(en_A1)
  use saftvrmie_options, only: enable_A1
  implicit none
  ! Input
  logical, intent(in) :: en_A1
  !
  enable_A1 = en_A1
end subroutine saftvrmie_a1_control

! Activte/decativate hs from model
subroutine saftvrmie_hs_control(en_HS)
  use saftvrmie_options, only: enable_hs
  implicit none
  ! Input
  logical, intent(in) :: en_HS
  !
  enable_hs = en_HS
end subroutine saftvrmie_hs_control

! Get SAFT-VR Mie model term (a0,a1,a2 or a3)
subroutine calc_saftvrmie_model_term(nc,T,V,n,term,a)
  use saftvrmie_interface, only: calc_saftvrmie_term
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: T !< Temperature [K]
  real, intent(in) :: V !< Volume [m3]
  real, intent(in) :: n(nc) !< Mol numbers [mol]
  integer, intent(in) :: term !< 0-3
  ! Output
  real, intent(out) :: a
  !
  a = calc_saftvrmie_term(nc,T,V,n,term)
end subroutine calc_saftvrmie_model_term

! Get de Boer parameter used in SAFT-VR Mie EOS
subroutine get_saftvrmie_de_boer_parameter(i, LAMBDA)
  use saftvrmie_interface, only: deBoerParameter
  implicit none
  ! Input
  integer, intent(in) :: i !< Component
  ! Output
  real, intent(out) :: LAMBDA
  !
  if (i<1) call stoperror("get_saftvrmie_de_boer_parameter::must have i>=1")
  LAMBDA = deBoerParameter(i)
end subroutine get_saftvrmie_de_boer_parameter

! Get alpha used in in SAFT-VR Mie EOS
subroutine get_saftvrmie_van_der_waals_alpha(nc, T, alpha)
  use saftvrmie_interface, only: calc_alpha_saftvrmie
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: T !< Temperature
  ! Output
  real, intent(out) :: alpha(nc,nc)
  !

  alpha = calc_alpha_saftvrmie(T)
end subroutine get_saftvrmie_van_der_waals_alpha

! Calculate hypotetical pure fluid packing fraction used in the SAFT-VR Mie EOS
subroutine get_saftvrmie_zeta(nc,T,V,n,zeta)
  use saftvrmie_interface, only: calc_saftvrmie_zeta
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: T !< Temperature [K]
  real, intent(in) :: V !< Volume [m3/mol]
  real, intent(in) :: n(nc) !< Component mol masses [mol]
  ! Output
  real, intent(out) :: zeta
  !
  zeta = calc_saftvrmie_zeta(nc,T,V,n)
end subroutine get_saftvrmie_zeta

! Calculate effective packing fraction
subroutine get_saftvrmie_eta_eff(lambda,zeta,eta_eff)
  use saftvrmie_dispersion, only: calcEffEta
  implicit none
  ! Input
  real, intent(in) :: zeta !< Hypotetical pure fluid packing fraction
  real, intent(in) :: lambda !< Potential exponent
  ! Output
  real, intent(out) :: eta_eff
  !
  ! Locals
  real :: ef_e,ef_ee,ef_eee
  call calcEffEta(zeta,lambda,eta_eff,ef_e,ef_ee,ef_eee)
end subroutine get_saftvrmie_eta_eff

! Calculate pair-correlation function at contact
subroutine get_saftvrmie_g_d(zeta,gd)
  implicit none
  ! Input
  real, intent(in) :: zeta !< Hypotetical pure fluid packing fraction
  ! Output
  real, intent(out) :: gd !< Pair-correlation function at contact
  !
  real :: denum
  denum = (1.0 - zeta)**3
  gd = (1.0 - 0.5*zeta)/denum
end subroutine get_saftvrmie_g_d

! Calculate Helmholtz free energy of hard-core Sutherland particle
subroutine calc_a1_sutherland_div_eta(lambda,zeta,eps,a1s)
  use saftvrmie_dispersion, only: calcA1Sutherland
  implicit none
  ! Input
  real, intent(in) :: zeta !< Pure fluid packing fraction
  real, intent(in) :: lambda !< Sutherland potential
  real, intent(in) :: eps !< Well depth divided by Blotzmann constant
  ! Output
  real, intent(out) :: a1s !< Helmholtz free energy of hard-core Sutherland particle
  !
  ! Locals
  real :: a1s_e,a1s_ee,a1s_eee
  call calcA1Sutherland(zeta,lambda,eps,a1s,a1s_e,a1s_ee,a1s_eee)
end subroutine calc_a1_sutherland_div_eta

! Calculate integral from d to sigma
subroutine calc_b1_sutherland_div_eta(dhs,sigma,lambda,zeta,eps,b1s)
  use saftvrmie_dispersion, only: calcBtilde
  implicit none
  ! Input
  real, intent(in) :: dhs !< Hard-sphere diameter
  real, intent(in) :: sigma !< Effective sigma
  real, intent(in) :: zeta !< Pure fluid packing fraction
  real, intent(in) :: lambda !< Sutherland potential
  real, intent(in) :: eps !< Well depth divided by Blotzmann constant
  ! Output
  real, intent(out) :: b1s !< Integral form d to sigma eff
  !
  ! Loclas
  real :: x
  real :: B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx
  x = sigma/dhs
  call calcBtilde(x,zeta,lambda,eps,b1s,B_e,B_x,B_ee,B_xx,B_ex,B_eee,B_eex,B_exx)
end subroutine calc_b1_sutherland_div_eta

! Get C mie
subroutine get_c_mie(i,c)
  use saftvrmie_containers, only: saftvrmie_param
  implicit none
  ! Input
  integer, intent(in) :: i !< Component index
  ! Output
  real, intent(out) :: C !<
  !
  C=saftvrmie_param%Cij(i,i)
end subroutine get_c_mie


! Calculate effective eps_divk
subroutine get_saftvrmie_eps_eff(T,i,eps_eff)
  use saftvrmie_hardsphere, only: calc_binary_effective_eps_divk
  use saftvrmie_containers, only: calc_DFeynHibbsij
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var
  use parameters, only: nc
  implicit none
  ! Input
  real, intent(in) :: T !< Temperature [K]
  integer, intent(in) :: i !< Component index
  ! Output
  real, intent(out) :: eps_eff !< Effective epsilon divided by kB
  if (i<1) call stoperror("get_saftvrmie_sigma_eff::must have i>=1")
  !
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       saftvrmie_var(1)%DFeynHibbsij, saftvrmie_var(1)%D2FeynHibbsij)
  call calc_binary_effective_eps_divk(nc,T, saftvrmie_var(1), saftvrmie_var(1)%eps_divk_eff%d,&
       saftvrmie_var(1)%eps_divk_eff%d_T,saftvrmie_var(1)%eps_divk_eff%d_TT)
  eps_eff = saftvrmie_var(1)%eps_divk_eff%d(i,i)
end subroutine get_saftvrmie_eps_eff

! Get classical binary parameters for SAFT-VR-MIE (that do not depend on T)
subroutine get_saftvrmie_binary_params(nc,m,sigma,eps,lambda_a,lambda_r)
  use saftvrmie_containers, only: saftvrmie_param
  implicit none
  ! Input
  integer, intent(in) :: nc
  ! Output
  real, intent(out) :: m(nc) !< segment number [-]
  real, intent(out) :: sigma(nc,nc) !< diameter [m]
  real, intent(out) :: eps(nc,nc) !< dispersive energy over kB [K]
  real, intent(out) :: lambda_a(nc,nc) !< attractive exponent [-]
  real, intent(out) :: lambda_r(nc,nc) !< repulsive exponent [-]
  !
  m = saftvrmie_param%ms
  sigma = saftvrmie_param%sigma_ij
  eps = saftvrmie_param%eps_divk_ij
  lambda_a = saftvrmie_param%lambda_a_ij
  lambda_r = saftvrmie_param%lambda_r_ij
end subroutine get_saftvrmie_binary_params

! Calculate effective eps_divk
subroutine get_saftvrmie_eps_eff_binary(nc,T,eps_eff)
  use saftvrmie_hardsphere, only: calc_binary_effective_eps_divk
  use saftvrmie_containers, only: calc_DFeynHibbsij
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: T !< Temperature [K]
  ! Output
  real, intent(out) :: eps_eff(nc,nc) !< Effective epsilon divided by kB
  !
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       saftvrmie_var(1)%DFeynHibbsij, saftvrmie_var(1)%D2FeynHibbsij)
  call calc_binary_effective_eps_divk(nc,T,saftvrmie_var(1),saftvrmie_var(1)%eps_divk_eff%d,&
       saftvrmie_var(1)%eps_divk_eff%d_T,saftvrmie_var(1)%eps_divk_eff%d_TT)
  eps_eff = saftvrmie_var(1)%eps_divk_eff%d
end subroutine get_saftvrmie_eps_eff_binary

! Calculate effective sigma
subroutine get_saftvrmie_sigma_eff_binary(nc,T,sigma_eff)
  use saftvrmie_hardsphere, only: calc_binary_effective_sigma
  use saftvrmie_containers, only: calc_DFeynHibbsij
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: T !< Temperature [K]
  ! Output
  real, intent(out) :: sigma_eff(nc,nc) !< Effective sigma
  !
  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       saftvrmie_var(1)%DFeynHibbsij, saftvrmie_var(1)%D2FeynHibbsij)
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T,saftvrmie_var(1),saftvrmie_var(1)%sigma_eff%d,&
       saftvrmie_var(1)%sigma_eff%d_T,saftvrmie_var(1)%sigma_eff%d_TT)
  sigma_eff = saftvrmie_var(1)%sigma_eff%d
end subroutine get_saftvrmie_sigma_eff_binary

! Calculate effective sigma
subroutine get_saftvrmie_sigma_eff(T,sigma_eff,i)
  use saftvrmie_hardsphere, only: calc_binary_effective_sigma
  use saftvrmie_containers, only: calc_DFeynHibbsij
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var
  use parameters, only: nc
  implicit none
  ! Input
  real, intent(in) :: T !< Temperature [K]
  integer, intent(in) :: i !< Component index
  ! Output
  real, intent(out) :: sigma_eff !< Effective sigma
  if (i<1) call stoperror("get_saftvrmie_sigma_eff::must have i>=1")
  !
  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       saftvrmie_var(1)%DFeynHibbsij, saftvrmie_var(1)%D2FeynHibbsij)
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T,saftvrmie_var(1),saftvrmie_var(1)%sigma_eff%d,&
       saftvrmie_var(1)%sigma_eff%d_T,saftvrmie_var(1)%sigma_eff%d_TT)
  sigma_eff = saftvrmie_var(1)%sigma_eff%d(i,i)
end subroutine get_saftvrmie_sigma_eff

! Calculate the Barker--Henderson diameter dij
subroutine get_saftvrmie_bh_diameter(nc,T,dhs)
  use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
       calc_binary_effective_sigma
  use saftvrmie_containers, only: calc_DFeynHibbsij
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var
  implicit none
  ! Input
  integer, intent(in) :: nc !< number of components
  real, intent(in) :: T !< Temperature [K]
  ! Output
  real, intent(out) :: dhs(nc,nc) !< Barker--Henderson hard-sphere diameter
  !
  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       saftvrmie_var(1)%DFeynHibbsij, saftvrmie_var(1)%D2FeynHibbsij)
  ! Calculate effective sigma
  call calc_binary_effective_sigma(nc,T,saftvrmie_var(1),saftvrmie_var(1)%sigma_eff%d,&
       saftvrmie_var(1)%sigma_eff%d_T,saftvrmie_var(1)%sigma_eff%d_TT)
  ! Calculate hard-sphere diameter
  call calc_hardsphere_diameter(nc,T,saftvrmie_var(1),saftvrmie_var(1)%sigma_eff%d,&
       saftvrmie_var(1)%sigma_eff%d_T,saftvrmie_var(1)%sigma_eff%d_TT,saftvrmie_var(1)%dhs%d,&
       saftvrmie_var(1)%dhs%d_T,saftvrmie_var(1)%dhs%d_TT)
  dhs = saftvrmie_var(1)%dhs%d
end subroutine get_saftvrmie_bh_diameter

! g Mie
! Intended to reproduce figure 4 of the Lafitte 2013 paper
subroutine calc_g_mie(nc,T,V,n,order,gMie)
  use saftvrmie_containers, only: saftvrmie_var
  use saftvrmie_dispersion, only: calcA1, calcA2
  use saftvrmie_interface, only: preCalcSAFTVRMie
  use saftvrmie_chain, only: rdf_at_contact
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: T !< Temperature [K]
  real, intent(in) :: n(nc) !< Mol numbers [mol]
  real, intent(in) :: V !< Volume [m3]
  integer, intent(in) :: order
  ! Output
  real, intent(out) :: gMie(nc) !< Hard-sphere diameter
  ! Locals
  real :: F1, F2
  !
  ! Precalculate common variables
  call preCalcSAFTVRMie(nc,T,V,n,0,saftvrmie_var(1))
  ! Calculate first order monomer term - store results to
  ! saftvrmie_var(1) needed for rdf_at_contact
  call calcA1(nc,T,V,n,saftvrmie_var(1),F1)
  ! Calculate second order monomer term - store results to
  ! saftvrmie_var(1) needed for rdf_at_contact
  call calcA2(nc,T,V,n,saftvrmie_var(1),F2)
  ! Calculate gMie
  call rdf_at_contact(nc,T,V,n,saftvrmie_var(1),gMie,order=order)

end subroutine calc_g_mie

! g Mie
! Intended to reproduce figure 4 of the Lafitte 2013 paper
subroutine calc_a123(nc,T,V,n,a1,a2,a3)
  use saftvrmie_interface, only: preCalcSAFTVRMie
  use saftvrmie_containers, only: saftvrmie_var
  use saftvrmie_dispersion, only: calcA1, calcA2, calcA3
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: T !< Temperature [K]
  real, intent(in) :: n(nc) !< Mol numbers [mol]
  real, intent(in) :: V !< Volume [m3]
  ! Output
  real, intent(out) :: a1, a2, a3
  !
  call preCalcSAFTVRMie(nc,T,V,n,3,saftvrmie_var(1))
  call calcA1(nc,T,V,n,saftvrmie_var(1),a1)
  call calcA2(nc,T,V,n,saftvrmie_var(1),a2)
  call calcA3(nc,T,V,n,saftvrmie_var(1),a3)
end subroutine calc_a123

! Set cut off radius and enable truncation correction
subroutine set_r_cut(r_c)
  use saftvrmie_options, only: src => set_r_cut
  implicit none
  ! Input
  real, intent(in) :: r_c !< Cut of radius. Multiplum of sigma. [-]
  call src(r_c)
end subroutine set_r_cut

! Enable/disable truncation and shift correction
subroutine truncation_correction_model_control(truncation,shift)
  use saftvrmie_options, only: tcmc => truncation_correction_model_control
  implicit none
  logical, intent(in) :: truncation,shift
  ! Input
  call tcmc(truncation,shift)
end subroutine truncation_correction_model_control

! Calculate NAHS compressibillity
subroutine calc_binary_compressibillity_nahs(nc,x,eta,d1,d2,delta,Z)
  use saftvrmie_hardsphere, only: calc_binary_Z_Santos
  use saftvrmie_containers, only: saftvrmie_var
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: x(nc) !< Mol fractions [-]
  real, intent(in) :: eta !< Packing fraction [-]
  real, intent(in) :: d1 !< Diameter of sphere 1 [m]
  real, intent(in) :: d2 !< Diameter of sphere 2 [m]
  real, intent(in) :: delta !< d12=0.5*(d1+d2)*(1+delta) [-]
  ! Output
  real, intent(out) :: Z
  Z = calc_binary_Z_Santos(nc,x,eta,d1,d2,delta,saftvrmie_var(1))
end subroutine calc_binary_compressibillity_nahs

subroutine calc_hs_helmholtzenergy(nc,T,V,n,a,a_T,a_V,a_n,&
     a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
  use saftvrmie_hardsphere, only: calc_hardsphere_helmholtzenergy
  use saftvrmie_containers, only: saftvrmie_var
  use saftvrmie_interface, only: preCalcSAFTVRMie
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: n(nc) !< Mol numbers [-]
  real, intent(in) :: T !< Temperature [K]
  real, intent(in) :: V !< Volume [m^3]
  ! Output
  real, intent(out) :: a,a_T,a_V,a_n(nc),a_TT,a_TV,a_Tn(nc)
  real, intent(out) :: a_VV,a_Vn(nc),a_nn(nc,nc)

  ! Precalculate common variables
  call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var(1))
  call calc_hardsphere_helmholtzenergy(nc,T,V,n,saftvrmie_var(1),a,a_T,a_V,a_n,&
       a_TT,a_TV,a_Tn,a_VV,a_Vn,a_nn)
end subroutine calc_hs_helmholtzenergy

subroutine calc_Fderivs(nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
     F_VV,F_TV,F_Tn,F_Vn,F_nn)
  use saftvrmie_containers, only: saftvrmie_var
  use saftvrmie_interface, only: preCalcSAFTVRMie, calcFresSAFTVRMie
  implicit none
  ! Input
  integer, intent(in) :: nc
  real, intent(in) :: n(nc) !< Mol numbers [-]
  real, intent(in) :: T !< Temperature [K]
  real, intent(in) :: V !< Volume [m^3]
  ! Output
  real, intent(out) :: F,F_T,F_V,F_n(nc),F_TT,F_TV,F_Tn(nc)
  real, intent(out) :: F_VV,F_Vn(nc),F_nn(nc,nc)

  ! Precalculate common variables
  call preCalcSAFTVRMie(nc,T,V,n,2,saftvrmie_var(1))
  call calcFresSAFTVRMie(nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
end subroutine calc_Fderivs


subroutine saftvrmie_set_packing_mixture_rule(rule)
  use saftvrmie_options, only: zeta_mixing_rule, ZETA_LAFITTE, ZETA_LINEAR, ZETA_LEONARD
  use stringmod, only: str_eq
  implicit none
  ! Input
  character(len=*), intent(in) :: rule
  !
  if (str_eq(rule,"lafitte")) then
     zeta_mixing_rule = ZETA_LAFITTE
  else if (str_eq(rule,"linear")) then
     zeta_mixing_rule = ZETA_LINEAR
  else if (str_eq(rule,"leonard")) then
     zeta_mixing_rule = ZETA_LEONARD
  end if
end subroutine saftvrmie_set_packing_mixture_rule

subroutine saftvrmie_set_model_options(model,hs_reference)
  use saftvrmie_options, only: LAFITTE, QSAFT_FH1, QSAFT_FH2, &
       LAFITTE_HS_REF, SINGLE_COMP_HS_REF, ADDITIVE_HS_REF, &
       NON_ADD_HS_REF, saftvrmieaij_model_options
  use stringmod, only: str_eq
  implicit none
  ! Input
  character(len=*), intent(in) :: model
  character(len=*), intent(in) :: hs_reference
  ! Locals
  integer :: mod, hs_ref
  if (str_eq(model,"LAFITTE")) then
    mod = LAFITTE
  else if (str_eq(model,"QSAFT_FH1")) then
    mod = QSAFT_FH1
  else if (str_eq(model,"QSAFT_FH2")) then
    mod = QSAFT_FH2
  else
    call stoperror("pyThermopack: Wrong SAFT-VR Mie model")
  end if
  if (str_eq(hs_reference,"LAFITTE_HS_REF")) then
    hs_ref = LAFITTE_HS_REF
  else if (str_eq(hs_reference,"SINGLE_COMP_HS_REF")) then
    hs_ref = SINGLE_COMP_HS_REF
  else if (str_eq(hs_reference,"ADDITIVE_HS_REF")) then
    hs_ref = ADDITIVE_HS_REF
  else if (str_eq(hs_reference,"NON_ADD_HS_REF")) then
    hs_ref = NON_ADD_HS_REF
  else
    call stoperror("pyThermopack: Wrong SAFT-VR Mie HS model")
  end if
  call saftvrmieaij_model_options(mod,hs_ref)

end subroutine saftvrmie_set_model_options

! Calculate quantum corrected potential
subroutine saftvrmie_q_potential_div_kb(j,k,T,r,U_divk)
  use saftvrmie_hardsphere, only: calc_mie_potential_quantumcorrected
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var, calc_DFeynHibbsij
  use parameters, only: nc
  ! Input
  integer, intent(in) :: j,k ! Binary interaction potential
  real, intent(in) :: T ! Temperature
  real, intent(in) :: r ! Radius (m)
  ! Output
  real, intent(out) :: U_divk
  ! Calculate Feynman--Hibbs D parameter
  call calc_DFeynHibbsij(nc, T, saftvrmie_param%DFeynHibbsParam_ij, &
       saftvrmie_var(1)%DFeynHibbsij, saftvrmie_var(1)%D2FeynHibbsij)
  ! Obtain the interaction potential
  call calc_mie_potential_quantumcorrected(j,k,saftvrmie_var(1),&
       saftvrmie_param%sigma_ij(j,k),saftvrmie_param%eps_divk_ij(j,k),&
       saftvrmie_param%lambda_a_ij(j,k),saftvrmie_param%lambda_r_ij(j,k),&
       saftvrmie_param%Cij(j,k),&
       saftvrmie_param%Quantum_const_1a_ij(j,k),&
       saftvrmie_param%Quantum_const_1r_ij(j,k),&
       saftvrmie_param%Quantum_const_2a_ij(j,k),&
       saftvrmie_param%Quantum_const_2r_ij(j,k),&
       r,U_divk)
end subroutine saftvrmie_q_potential_div_kb

! Get correlated f value for alpha
subroutine get_f_of_alpha(alpha,f)
  use saftvrmie_dispersion, only: calcFunAlpha
  implicit none
  ! Input
  real, intent(in)  :: alpha
  ! Output
  real, intent(out) :: f(6)
  call calcFunAlpha(alpha,f)
end subroutine get_f_of_alpha
