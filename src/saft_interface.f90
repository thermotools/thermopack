!------------------------------------------------------------------------------
! MODULES FOR SAFT MODELS:     SOURCE FILES:
! saft_globals                 (saft_globals.f90)
! saft_interface               (saft_interface.f90)
! saft_association             (saft_association.f90)
! saft_rdf                     (saft_rdf.f90)
! pc_saft_nonassoc             (pc_saft_nonassoc.f90)
! pc_saft_parameters           (pc_saft_parameters.f90)
! cpa_parameters               (cpa_parameters.f90)
! AssocSchemeUtils             (assocschemeutils.f90)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!> The interface module for SAFT equations of state. Contains all routines a
!> user may wish to call. Also responsible for combining the association and
!> non-association contributions.
!>
!> Available SAFT equations: CPA-SRK, CPA-PR, PC-SAFT and SAFT-VR Mie.
!>
!> Caveat for future programmers modifying this module:
!> It operates only with SI units. Most notably, the routines use V [m^3]
!> and n [mole numbers] instead of v [L/mol] and z [normalized mole numbers].
!> However, the a and b parameters in the CPA database use non-SI units that
!> comply with the units in eoscubic type.
!------------------------------------------------------------------------------
module saft_interface
  use thermopack_constants, only: verbose
  use compdata, only: gendata
  use eosdata, only: cpaSRK, cpaPR, eosPC_SAFT, eosOPC_SAFT, &
       eosSPC_SAFT, eosPeTS, eosSAFT_VR_MIE, &
       eosLJS_BH, eosLJS_WCA, eosLJS_UF, eosLJS_UV, eosLJ_UF, &
       eosPCP_SAFT, eosSPCP_SAFT
  use thermopack_var, only: nce, get_active_thermo_model, thermo_model, &
       get_active_eos, base_eos_param, numassocsites, Rgas => Rgas_default
  use association_var, only: association
  implicit none
  save
  public

contains

  !> Called from routine init_thermopack in eoslibinit.f90.
  subroutine saft_type_eos_init(nc,comp,eos,param_ref,silent_init)
    use assocschemeutils, only: no_assoc,assocIndices_bookkeeping
    use cubic_eos, only: cb_eos
    use compdata, only: gendata_pointer
    use CPA_parameters, only: getCpaPureParams_allcomps, getCpaKijAndCombRules_allComps
    use PC_SAFT_parameters, only: getPcSaftPureParams_allComps, getPcSaftKij_allComps, getPcSaftCombRules_allComps
    use pc_saft_nonassoc, only: PCSAFT_eos, sPCSAFT_eos
    use saftvrmie_interface, only: init_saftvrmie
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_bh_eos, init_ljs_bh, ljs_wca_eos, init_ljs_wca
    use saftvrmie_parameters, only: getSaftVrMieAssocParams_allComps
    use pets, only: PETS_eos, getPetsPureParams
    use multipol_var, only: multipol_param, multipol_param_constructor
    integer, intent(in)           :: nc          !< Number of components.
    type(gendata_pointer), intent(inout)  :: comp(nc)    !< Component vector.
    class(base_eos_param), intent(inout) :: eos       !< Underlying cubic equation of state.
    character(len=*), intent(in) :: param_ref   !< Parameter sets to use for components
    logical, intent(in), optional :: silent_init !< Print no varnings during init
    ! Index variables.
    integer :: ic, l
    ! The following will be fetched from database.
    real :: a0_db(nc), b_db(nc), alphaParams_db(3,nc)
    real :: m_db(nc), sigma_db(nc), eps_depth_divk_db(nc)
    real :: eps_db(nc), beta_db(nc)
    real :: kij_PCSAFT(nc,nc)
    real :: kij_aEpsBeta_CPA(3,nc,nc)
    integer :: epsbeta_combrules_CPA(2,nc,nc) ! arithm. or geom. CPA comb rules
    integer :: epsbeta_combrules_PCSAFT(2,nc,nc) ! arithm. or geom. PC-SAFT comb rules
    logical :: compInDB(nc) ! is the component in the db?
    integer :: alpharCorrIdx_db(nc), assocSchemes_db(nc)
    real :: mu_db_PCSAFT(nc),Q_db_PCSAFT(nc)
    logical :: silent
    type(association), pointer :: assoc => NULL()

    if (present(silent_init)) then
      silent = silent_init
    else
      silent = .false.
    endif

    allocate(assoc)

    ! Set the model index.
    assoc%saft_model = eos%subeosidx

    ! Fetch parameters from the database.
    if (assoc%saft_model == eosSAFT_VR_MIE) then
      call getSaftVrMieAssocParams_allComps(nc,comp,eos%subeosidx,param_ref,&
           compinDB,eps_db,beta_db,assocSchemes_db)
    else if (eos%eosidx == eosPC_SAFT) then
      call getPcSaftPureParams_allComps(nc,comp,eos%eosidx,param_ref,compInDB,&
           m_db,sigma_db,eps_depth_divk_db,eps_db,beta_db,assocSchemes_db,&
           mu_db_PCSAFT,Q_db_PCSAFT)
    else if (assoc%saft_model == eosPeTS) then
      call getPetsPureParams(nc,comp,assoc%saft_model,param_ref,compInDB,&
           m_db,sigma_db,eps_depth_divk_db,eps_db,beta_db,assocSchemes_db)
    else
      assocSchemes_db = no_assoc
      call getCpaPureParams_allcomps(nc,comp,assoc%saft_model,param_ref,compInDB,&
           a0_db,b_db,alphaParams_db,eps_db,beta_db,alpharCorrIdx_db,&
           assocSchemes_db)
    end if

    ! Set association scheme components. Must be done before setting cubic
    ! params, because we use standard parameters if not self-associating.
    do ic=1,nc
      comp(ic)%p_comp%assoc_scheme = assocSchemes_db(ic)
    end do

    if (.not. silent) then
      ! Can only initiate to something already in database (for now).
      do ic=1,nc
        if ( .not. compInDB(ic) ) then
          ! Allow ions:
          l = len_trim(comp(ic)%p_comp%ident)
          if (comp(ic)%p_comp%ident(l:l) /= "+" .and. comp(ic)%p_comp%ident(l:l) /= "-") then
            print *, "Component not in database: ", comp(ic)%p_comp%ident
          end if
        end if
      end do
    end if
    ! Fetch interaction params from the database (defaults to 0 if not present).
    if (assoc%saft_model == eosSAFT_VR_MIE .or. assoc%saft_model == eosPeTS) then
      !....
    else if (eos%eosidx == eosPC_SAFT) then
       call getPcSaftKij_allComps(nc,comp,assoc%saft_model,kij_PCSAFT,param_ref)
       call getPcSaftCombRules_allComps(nc,comp,eos%eosidx,&
            epsbeta_combrules_PCSAFT,param_ref)
    else
       call getCpaKijAndCombRules_allComps(nc,comp,assoc%saft_model,kij_aEpsBeta_CPA,&
            epsbeta_combrules_CPA)
    end if

    ! Set nonassoc parameters
    select type ( p_eos => eos )
    class is ( cb_eos )
      ! Set new b, a0, c1 in the cbeos-struct for self-associating components.
      call cpa_set_cubic_params(nc,comp,p_eos,a0_db,b_db,&
           alphaParams_db,alpharCorrIdx_db,kij_aEpsBeta_CPA(1,:,:))
    class is(sPCSAFT_eos)
      call pcsaft_set_nonassoc_params(p_eos,nc,m_db,sigma_db,&
           eps_depth_divk_db,kij_PCSAFT)
      ! PCP-SAFT?
      if (eos%subeosidx == eosPCP_SAFT .OR. eos%subeosidx == eosSPCP_SAFT) then
        if (associated(p_eos%mpol_param)) deallocate(p_eos%mpol_param)
        allocate(p_eos%mpol_param, source=multipol_param_constructor(nce))
        call p_eos%mpol_param%init_multipol_param(nce,mu_db_PCSAFT,Q_db_PCSAFT,&
             p_eos%m,p_eos%sigma,p_eos%eps_depth_divk)
      endif
    class is ( saftvrmie_eos )
      call init_saftvrmie(nc,comp,p_eos,param_ref)
    class is ( ljs_bh_eos )
      call init_ljs_bh(nc,comp,p_eos,param_ref)
    class is ( ljs_wca_eos )
      call init_ljs_wca(nc,comp,p_eos,param_ref)
    class is ( PETS_eos )
      call pets_set_params(p_eos,sigma_db,eps_depth_divk_db)
    class default
      call stoperror("calcSaftFder_res_nonassoc: Wrong eos...")
    end select

    ! Init calls above will clean eos class memory before init
    ! Therefore the assoc pointer is assigned after init
    eos%assoc => assoc
    ! Fill module variables in the module assocschemeutils, keeping track of
    ! associating components and association sites.
    call assocIndices_bookkeeping(eos%assoc,nc,eos%assoc%saft_model,assocSchemes_db)
    numAssocSites = eos%assoc%numAssocSites

    ! Set the association parameters.
    if (allocated(eos%assoc%beta_kl)) deallocate(eos%assoc%beta_kl)
    allocate(eos%assoc%beta_kl(numAssocSites,numAssocSites))
    if (allocated(eos%assoc%eps_kl)) deallocate(eos%assoc%eps_kl)
    allocate(eos%assoc%eps_kl(numAssocSites,numAssocSites))

    if (eos%assoc%saft_model == eosSAFT_VR_MIE) then
       call saft_setAssocParams(eos%assoc,nc,eos%assoc%saft_model,assocSchemes_db,eps_db,beta_db,sigma_db)
    else if (eos%eosidx == eosPC_SAFT) then 
       call saft_setAssocParams(eos%assoc,nc,eos%assoc%saft_model,assocSchemes_db,&
            eps_db,beta_db,sigma_db, epsbeta_combrules_PCSAFT)
    else
       call cpa_setAssocParams(eos%assoc,nc,assocSchemes_db,eps_db,beta_db,&
            epsbeta_combrules_CPA,kij_aEpsBeta_CPA(2:3,:,:))
    end if

  end subroutine saft_type_eos_init

  !> Sets the fitted parameters in the cubic eos.
  subroutine cpa_set_cubic_params(nc,comp,cbeos,a0_in,b_in,alphaParams_in,alphaCorrIdx_in,kij_in)
    use cbAlpha, only: setSingleAlphaCorr
    use CPA_parameters, only: no_assoc
    use cubic_eos, only: cb_eos
    use compdata, only: gendata_pointer
    integer, intent(in) :: nc                   !< Number of components.
    type(gendata_pointer), intent(inout) :: comp(nc)    !< Component vector.
    class(cb_eos), intent(inout) :: cbeos      !< The underlying cubic equation of state.
    integer, intent(in) :: alphaCorrIdx_in(nc)
    real, intent(in) :: a0_in(nc),b_in(nc),alphaParams_in(3,nc),kij_in(nc,nc)
    integer :: ic,jc

    do ic = 1,nc

       ! If component not self-associating: use standard cubic parameters.
       if (comp(ic)%p_comp%assoc_scheme==no_assoc) cycle

       ! Set cubic a and b parameters. Database parameters have same units as
       ! those in cbeos%single.
       cbeos%single(ic)%b = b_in(ic)
       cbeos%single(ic)%a = a0_in(ic)

       ! Set alpha correlation. Only classic_fit correlation tested.
       call setSingleAlphaCorr(i=ic, cbeos=cbeos, &
            alphaIdx=alphaCorrIdx_in(ic), &
            alphaParams=alphaParams_in(1:3,ic))

       do jc = (ic+1),nc
         cbeos%kij(ic,jc) = kij_in(ic,jc)
         cbeos%kij(jc,ic) = kij_in(ic,jc)
       end do
    end do

  end subroutine cpa_set_cubic_params


  !> Sets the fitted parameters in the non-association part of PC-SAFT.
  subroutine pcsaft_set_nonassoc_params(eos,nc,m_in,sigma_in,eps_depth_divk_in,kij_in, allocate)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    class(sPCSAFT_eos), intent(inout) :: eos
    integer, intent(in) :: nc                   !< Number of components.
    real, intent(in) :: m_in(nc),sigma_in(nc),eps_depth_divk_in(nc),kij_in(nc,nc)
    logical, intent(in), optional :: allocate
    integer ::  ic, jc

    ! Allocate by default
    if (present(allocate)) then
      if (allocate) call eos%allocate_and_init(nc,"PC-SAFT")
    else
    call eos%allocate_and_init(nc,"PC-SAFT")
    end if

    do ic = 1,nc
      ! Set pure-component nonassoc parameters, and scheme.
      eos%m(ic) = m_in(ic)
      eos%sigma(ic,ic) = sigma_in(ic)
      eos%eps_depth_divk(ic,ic) = eps_depth_divk_in(ic)
      ! Compute the cross-component parameters.
      do jc = (ic+1),nc
        eos%sigma(ic,jc) = (sigma_in(ic)+sigma_in(jc))/2.0
        eos%sigma(jc,ic) =  eos%sigma(ic,jc)

        ! eps_depth_divk is the only pc-saft parameter with an interaction parameter kij.
        eos%eps_depth_divk(ic,jc) = &
             sqrt(eps_depth_divk_in(ic)*eps_depth_divk_in(jc))*&
             (1-kij_in(ic,jc))
        eos%eps_depth_divk(jc,ic) = eos%eps_depth_divk(ic,jc)
      end do
    end do
  end subroutine pcsaft_set_nonassoc_params

  !> Set the molecular parameters in the PeTS equation of state
  subroutine pets_set_params(eos,sigma_in,eps_depth_divk_in)
    use pets, only: PETS_eos
    class (PETS_eos), intent(inout) :: eos
    real, intent(in) :: sigma_in(1),eps_depth_divk_in(1)
    eos%sigma_pets = sigma_in(1)
    eos%epsdivk_pets = eps_depth_divk_in(1)
  end subroutine pets_set_params

  !> Set association parameters for PC-SAFT and SAFT-VR Mie
  subroutine saft_setAssocParams(assoc,nc,saft_model,assoc_scheme,epsVal,betaVal,sigmaVal,epsBetaCombRulesPCSAFT)
    use saft_association, only: numAssocSites, compidx_to_sites, site_to_compidx
    use AssocSchemeUtils, only: applyCombiningRule, site_interaction_internal, cross_site_interaction, defaultComb
    use eosdata, only: get_eos_idx_from_subidx
    ! Input.
    type(association), intent(inout) :: assoc
    integer, intent(in) :: nc
    integer, intent(in) :: saft_model
    integer, intent(in) :: assoc_scheme(nc)
    real, intent(in) :: epsVal(nc), betaVal(nc), sigmaVal(nc)
    integer, intent(in), optional :: epsBetaCombRulesPCSAFT(2,nc,nc)
    ! Locals.
    integer :: k,l,ic,jc,k_first,k_last,l_first,l_last, eos_idx

    eos_idx = get_eos_idx_from_subidx(saft_model)

    ! All elements not explicitly set should be zero.
    assoc%eps_kl  = 0.0
    assoc%beta_kl = 0.0

    do k=1,numAssocSites
      do l=1,numAssocSites

        ! Get component indices.
        ic = site_to_compidx(assoc,k)
        jc = site_to_compidx(assoc,l)

        if (ic == jc) then
          ! Association between like molecules.
          call compidx_to_sites(assoc,ic,k_first,k_last)
          if ( site_interaction_internal(k-k_first+1,l-k_first+1,assoc_scheme(ic)) ) then
            assoc%eps_kl(k,l) = epsVal(ic)
            assoc%beta_kl(k,l) = betaVal(ic)
          end if
        else
          ! Cross-association between unlike molecules. (Would also work for
          ! like molecules.)
          call compidx_to_sites(assoc,ic,k_first,k_last)
          call compidx_to_sites(assoc,jc,l_first,l_last)
          if (cross_site_interaction (site1=k-k_first+1,site2=l-l_first+1,&
            assoc_scheme_I=assoc_scheme(ic), assoc_scheme_II=assoc_scheme(jc)) ) then          
            if (saft_model == eosSAFT_VR_MIE) then
              assoc%eps_kl(k,l) = sqrt(epsVal(ic)*epsVal(jc))
              assoc%beta_kl(k,l) = ((betaVal(ic)**(1.0/3.0) + betaVal(jc)**(1.0/3.0))/2)**3
            else
              assoc%eps_kl(k,l) = (epsVal(ic)+epsVal(jc))/2
              assoc%beta_kl(k,l) = sqrt(betaVal(ic)*betaVal(jc))*( 2*sqrt(sigmaVal(ic)*sigmaVal(jc))&
                /(sigmaVal(ic)+sigmaVal(jc)) )**3
              if (eos_idx == eosPC_SAFT .and. present(epsBetaCombRulesPCSAFT)) then
                if (epsBetaCombRulesPCSAFT(1,ic,jc) /= defaultComb) then
                  assoc%eps_kl(k,l) = applyCombiningRule(epsBetaCombRulesPCSAFT(1,ic,jc), &
                    epsVal(ic), epsVal(jc))
                endif
                if (epsBetaCombRulesPCSAFT(2,ic,jc) /= defaultComb) then
                  assoc%beta_kl(k,l) = applyCombiningRule(epsBetaCombRulesPCSAFT(2,ic,jc), &
                    betaVal(ic), betaVal(jc))
                endif
              endif
            endif
          endif
        endif
      end do
    end do
  end subroutine saft_setAssocParams

  subroutine cpa_setAssocParams(assoc,nc,assoc_scheme,epsVal,betaVal,epsBetaCombRules,epsBetaKij)
    use saft_association, only: numAssocSites, compidx_to_sites, site_to_compidx
    use AssocSchemeUtils, only: applyCombiningRule, site_interaction_internal, cross_site_interaction
    ! Input.
    type(association), intent(inout) :: assoc
    integer, intent(in) :: nc
    integer, intent(in) :: assoc_scheme(nc)
    real, intent(in) :: epsVal(nc), betaVal(nc)
    integer, intent(in) :: epsBetaCombRules(2,nc,nc)
    real, intent(in), optional :: epsBetaKij(2,nc,nc)
    ! Locals.
    integer :: k,l,ic,jc,k_first,k_last,l_first,l_last
    real :: kij_eps,kij_beta

    ! All elements not explicitly set should be zero.
    assoc%eps_kl  = 0.0
    assoc%beta_kl = 0.0

    do k=1,numAssocSites
       do l=1,numAssocSites

          ! Get component indices.
          ic = site_to_compidx(assoc, k)
          jc = site_to_compidx(assoc, l)

          if (ic == jc) then
             ! Association between like molecules.
             call compidx_to_sites(assoc, ic,k_first,k_last)
             if ( site_interaction_internal(k-k_first+1,l-k_first+1,assoc_scheme(ic)) ) then
                assoc%eps_kl(k,l) = epsVal(ic)
                assoc%beta_kl(k,l) = betaVal(ic)
             end if
          else
             ! Cross-association between unlike molecules. (Would also work for
             ! like molecules.)
             call compidx_to_sites(assoc,ic,k_first,k_last)
             call compidx_to_sites(assoc,jc,l_first,l_last)
             if (cross_site_interaction (site1=k-k_first+1,site2=l-l_first+1,&
                  assoc_scheme_I=assoc_scheme(ic), assoc_scheme_II=assoc_scheme(jc)) ) then

                kij_eps = epsBetaKij(1,ic,jc)
                kij_beta = epsBetaKij(2,ic,jc)
                assoc%eps_kl(k,l) = applyCombiningRule (epsBetaCombRules(1,ic,jc), &
                     epsVal(ic), epsVal(jc)) * (1-kij_eps)
                assoc%beta_kl(k,l) = applyCombiningRule(epsBetaCombRules(2,ic,jc), &
                     betaVal(ic), betaVal(jc)) * (1-kij_beta)
             end if
          end if

       end do
    end do
  end subroutine cpa_setAssocParams

  !> Calculates the reduced residual Helmholtz energy F (both the association
  !> contribution and the underlying equation (e.g. SRK)), together with its
  !> derivatives.
  subroutine calcSaftFder_res(nc,eos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn,Xk)
    use cubic_eos, only: cb_eos
    use numconstants, only: machine_prec ! Equals 2^{-52} ~ 2.22*e-16 for double precision reals.
    use saft_association, only: numAssocSites, solve_for_X_k, &
         calcFder_assoc
    use saft_association, only: numAssocSites, solve_for_X_k, calcFder_assoc
    use eos_parameters, only: base_eos_param
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in) :: T,V,n(nc)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    real, optional, intent(out) :: Xk(numAssocSites)
    ! Locals.
    real :: F_nonassoc,F_T_nonassoc,F_V_nonassoc,F_n_nonassoc(nc)
    real :: F_TT_nonassoc,F_TV_nonassoc,F_Tn_nonassoc(nc),F_VV_nonassoc
    real :: F_Vn_nonassoc(nc),F_nn_nonassoc(nc,nc)
    real :: X_k(numAssocSites)

    ! Calculate the non-association contribution.
    call calcSaftFder_res_nonassoc(nc,eos,T,V,n,F,F_T,&
         F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)

    if (numAssocSites > 0) then
      if (sum(n(eos%assoc%compIdcs)) > 1e-20) then
        ! Store the non-association contribution
        if (present(F)) F_nonassoc = F
        if (present(F_T)) F_T_nonassoc = F_T
        if (present(F_V)) F_V_nonassoc = F_V
        if (present(F_n)) F_n_nonassoc = F_n
        if (present(F_TT)) F_TT_nonassoc = F_TT
        if (present(F_TV)) F_TV_nonassoc = F_TV
        if (present(F_Tn)) F_Tn_nonassoc = F_Tn
        if (present(F_VV)) F_VV_nonassoc = F_VV
        if (present(F_Vn)) F_Vn_nonassoc = F_Vn
        if (present(F_nn)) F_nn_nonassoc = F_nn

        ! Calculate the association contribution.
        call eos%assoc%state%init(nc,T,V,n)
        X_k = 0.2 ! Initial guess.
        call solve_for_X_k(eos,nc,X_k,tol=10**5*machine_prec)
        if (present(Xk)) then
          Xk = X_k ! Return X_k
        endif
        call calcFder_assoc(eos,nc=nc,X_k=X_k,F=F,F_T=F_T,F_V=F_V,F_n=F_n,&
             F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tn,F_Vn=F_Vn,F_nn=F_nn)

        ! Add the non-association and association contribution.
        if (present(F)) F = F + F_nonassoc
        if (present(F_T)) F_T = F_T + F_T_nonassoc
        if (present(F_V)) F_V = F_V + F_V_nonassoc
        if (present(F_n)) F_n = F_n + F_n_nonassoc
        if (present(F_TT)) F_TT = F_TT + F_TT_nonassoc
        if (present(F_TV)) F_TV = F_TV + F_TV_nonassoc
        if (present(F_Tn)) then
          F_Tn = F_Tn + F_Tn_nonassoc
        end if

        if (present(F_VV)) F_VV = F_VV + F_VV_nonassoc
        if (present(F_Vn)) F_Vn = F_Vn + F_Vn_nonassoc
        if (present(F_nn)) F_nn = F_nn + F_nn_nonassoc
      endif
    endif

  end subroutine calcSaftFder_res

  !> Calculates the reduced residual Helmholtz energy F
  !> for the non-associationg part, together with its
  !> derivatives.
  subroutine calcSaftFder_res_nonassoc(nc,eos,T,V,n,F,F_T,&
       F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use cubic, only: calcCbFder_res_SI
    use cubic_eos, only: cb_eos
    use pc_saft_nonassoc, only: F_sPC_SAFT_TVn, F_PC_SAFT_TVn, PCSAFT_eos, sPCSAFT_eos
    use pets, only: F_PeTS_TVn, PETS_eos
    use saftvrmie_interface, only: calcFresSAFTVRMie
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_bh_eos, calcFresLJs_bh, &
         ljs_wca_eos, calcFres_WCA
    use multipol, only: add_hyperdual_fres_multipol
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in) :: T,V,n(nc)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    ! Locals
    real :: Fl
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is ( cb_eos )
      call calcCbFder_res_SI(nc,p_eos,T,V,n,F,F_T,F_V,F_n,&
           F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
      !call calcFder_nonassoc_cpa(nc,comp,cbeos,T,V,n,F,F_T,F_V,F_n,&
      !      F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    class is ( PCSAFT_eos )
      call F_PC_SAFT_TVn(p_eos,T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
    class is ( sPCSAFT_eos )
      call F_sPC_SAFT_TVn(p_eos,T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
    class is (ljs_bh_eos)
      call calcFresLJs_bh(p_eos,nc,T,V,n,Fl,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      if (present(F)) then
        F = Fl
      endif
    class is (ljs_wca_eos)
      call calcFres_WCA(p_eos,nc,T,V,n,Fl,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      if (present(F)) then
        F = Fl
      endif
    class is (saftvrmie_eos)
      call calcFresSAFTVRMie(p_eos,nc,T,V,n,Fl,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
      if (present(F)) then
        F = Fl
      endif
    class is ( PETS_eos )
      call F_PeTS_TVn(p_eos,T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
    class default
      call stoperror("calcSaftFder_res_nonassoc: Wrong eos...")
    end select

    ! Include polar contributions
    if (associated(eos%mpol_param)) then
      call add_hyperdual_fres_multipol(eos,nc,T,V,n,f=F,f_T=F_T,&
           f_V=F_V,f_n=F_n,f_TT=F_TT,f_VV=F_VV,f_TV=F_TV,f_Tn=F_Tn,&
           f_Vn=F_Vn,f_nn=F_nn)
    endif
  end subroutine calcSaftFder_res_nonassoc

  !> Front-end procedure giving the combined pressure of the cubic contribution
  !> and the association contribution. Only works for mixtures with association.
  subroutine saft_total_pressure_assoc_mix(nc,eos,T,V,n,P,&
       dPdV,dPdT,dPdn)
    use thermopack_var, only: base_eos_param
    use saft_association, only: numAssocSites, solve_for_X_k
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in)  :: T                  !< Temperature [K]
    real, intent(in)  :: V                  !< Volume [m^3]
    real, intent(in)  :: n(nc)              !< Mole numbers [moles]
    real, intent(out) :: P                  !< Pressure [Pa]
    real, intent(out), optional :: dPdV, dPdT, dPdn(nc)
    ! Locals.
    real :: X_k(numAssocSites)

    if (numAssocSites == 0) call stoperror("For associating mixtures only.")
    call eos%assoc%state%init(nc,T,V,n)
    X_k = 0.2 ! Initial guess.
    call solve_for_X_k(eos,nc,X_k)
    call saft_total_pressure_knowing_X_k(nc,eos,X_k,P,&
         dPdV=dPdV,dPdT=dPdT,dPdn=dPdn)

  end subroutine saft_total_pressure_assoc_mix

  !> Front-end procedure giving the combined pressure of the cubic contribution
  !> and the association contribution.
  subroutine saft_total_pressure(nc,cbeos,T,V,n,P,dPdV,dPdT,dPdn)
    use saft_association, only: numAssocSites
    use thermopack_var, only: base_eos_param
    use compdata, only: gendata
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: cbeos
    real, intent(in)  :: T                  !< Temperature [K]
    real, intent(in)  :: V                  !< Volume [m^3]
    real, intent(in)  :: n(nc)              !< Mole numbers [moles]
    real, intent(out) :: P                  !< Pressure [Pa]
    real, intent(out), optional :: dPdV, dPdT
    real, intent(out), optional :: dpdn(nc)

    if (numAssocSites > 0) then
      call saft_total_pressure_assoc_mix(nc,cbeos,T,V,n,P,&
           dPdV,dPdT,dPdn)
    else
      call nonassoc_pressure(nc,cbeos,T,V,n,P,dPdV,dPdT,dPdn)
    end if

  end subroutine saft_total_pressure


  !> Calculates the reduced dispersion contribution to the
  !! Helmholtz energy, together with its derivatives.
  !!
  subroutine calc_saft_dispersion(T,V,n,a,a_T,&
       a_V,a_n,a_TT,a_TV,a_VV,a_Tn,a_Vn,a_nn)
    use pc_saft_nonassoc, only: alpha_disp_PC_TVn, sPCSAFT_eos
    use saftvrmie_interface, only: calc_saftvrmie_dispersion
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_wca_eos,ljs_bh_eos,calc_ljs_dispersion
    use pets, only: PETS_eos
    ! Input.
    real, intent(in) :: T,V,n(nce)
    ! Output.
    real, optional, intent(out) :: a,a_T,a_V,a_n(nce)
    real, optional, intent(out) :: a_TT,a_TV,a_Tn(nce),a_VV,a_Vn(nce),a_nn(nce,nce)
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
      call alpha_disp_PC_TVn(p_eos,T,V,n,a,alp_V=a_V,alp_T=a_T,alp_n=a_n, &
           alp_VV=a_VV,alp_TV=a_TV,alp_Vn=a_Vn,alp_TT=a_TT,alp_Tn=a_Tn,alp_nn=a_nn)
    class is ( ljs_wca_eos )
      call calc_ljs_dispersion(eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_v,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class is ( ljs_bh_eos )
      call calc_ljs_dispersion(eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_v,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class is ( saftvrmie_eos )
      call calc_saftvrmie_dispersion(p_eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_V,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class is ( PETS_eos )
      call p_eos%alpha_disp_TVn(V,T,n,alp=a,alp_V=a_V,alp_T=a_T,alp_n=a_n, &
           alp_VV=a_VV,alp_VT=a_TV,alp_Vn=a_Vn,alp_TT=a_TT,alp_Tn=a_Tn,alp_nn=a_nn)
    class default
      call stoperror("calc_saft_dispersion: Wrong eos...")
    end select
  end subroutine calc_saft_dispersion

  !> Calculates the reduced molar hard-sphere contribution to the
  !> Helmholtz energy, together with its derivatives.
  !>
  subroutine calc_saft_hard_sphere(T,V,n,a,a_T,&
       a_V,a_n,a_TT,a_TV,a_VV,a_Tn,a_Vn,a_nn)
    use pc_saft_nonassoc, only: alpha_hs_sPC_TVn, sPCSAFT_eos, PCSAFT_eos, alpha_hs_PC_TVn
    use saftvrmie_interface, only: calc_saftvrmie_hard_sphere
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_wca_eos,ljs_bh_eos,calc_ljs_hard_sphere
    use pets, only: PETS_eos
    ! Input.
    real, intent(in) :: T,V,n(nce)
    ! Output.
    real, optional, intent(out) :: a,a_T,a_V,a_n(nce)
    real, optional, intent(out) :: a_TT,a_TV,a_Tn(nce),a_VV,a_Vn(nce),a_nn(nce,nce)
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is ( PCSAFT_eos )
      call alpha_hs_PC_TVn(p_eos,T,V,n,a,alp_V=a_V,alp_T=a_T,alp_n=a_n, &
           alp_VV=a_VV,alp_TV=a_TV,alp_Vn=a_Vn,alp_TT=a_TT,alp_Tn=a_Tn,alp_nn=a_nn)
    class is ( sPCSAFT_eos )
      call alpha_hs_sPC_TVn(p_eos,T,V,n,a,alp_V=a_V,alp_T=a_T,alp_n=a_n, &
           alp_VV=a_VV,alp_TV=a_TV,alp_Vn=a_Vn,alp_TT=a_TT,alp_Tn=a_Tn,alp_nn=a_nn)
    class is ( ljs_wca_eos )
      call calc_ljs_hard_sphere(eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_v,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class is ( ljs_bh_eos )
      call calc_ljs_hard_sphere(eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_v,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class is ( saftvrmie_eos )
      call calc_saftvrmie_hard_sphere(p_eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_V,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class is ( PETS_eos )
      call p_eos%alpha_hs_TVn(V,T,n,alp=a,alp_V=a_V,alp_T=a_T,alp_n=a_n, &
           alp_VV=a_VV,alp_VT=a_TV,alp_Vn=a_Vn,alp_TT=a_TT,alp_Tn=a_Tn,alp_nn=a_nn)
    class default
      call stoperror("calc_saft_hard_sphere: Wrong eos...")
    end select
  end subroutine calc_saft_hard_sphere

  !> Calculates the reduced molar soft repulsion contribution to the
  !> Helmholtz energy, together with its derivatives.
  !>
  subroutine calc_soft_repulsion(T,V,n,a,a_T,&
       a_V,a_n,a_TT,a_TV,a_VV,a_Tn,a_Vn,a_nn)
    use lj_splined, only: ljs_wca_eos, calc_wca_soft_repulsion
    ! Input.
    real, intent(in) :: T,V,n(nce)
    ! Output.
    real, optional, intent(out) :: a,a_T,a_V,a_n(nce)
    real, optional, intent(out) :: a_TT,a_TV,a_Tn(nce),a_VV,a_Vn(nce),a_nn(nce,nce)
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is ( ljs_wca_eos )
      call calc_wca_soft_repulsion(p_eos,nce,T,V,n,F=a,F_T=a_T,F_V=a_v,F_n=a_n,F_TT=a_TT,&
           F_VV=a_VV,F_TV=a_TV,F_Tn=a_Tn,F_Vn=a_Vn,F_nn=a_nn)
    class default
      call stoperror("calc_soft_repulsion: Wrong eos...")
    end select
  end subroutine calc_soft_repulsion

  !> Calculates Hard-sphere diameter
  subroutine calc_hard_sphere_diameter(T,d,d_T)
    use pc_saft_nonassoc, only: calc_d, sPCSAFT_eos
    use saftvrmie_interface, only: update_saftvrmie_hs_diameter
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_wca_eos
    use hardsphere_wca, only: calc_dhs_WCA
    use pets, only: PETS_eos
    ! Input.
    real, intent(in) :: T!,V,n(nce)
    ! Output.
    real, intent(out) :: d(nce) !(m)
    real, intent(out) :: d_T(nce) !(m/K)
    ! Locals
    class(base_eos_param), pointer :: eos
    integer :: i
    eos => get_active_eos()
    ! Calculate the hard-sphere diameter
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
      call calc_d(p_eos,T,d,d_T)
    class is (saftvrmie_eos)
      call update_saftvrmie_hs_diameter(p_eos,nce,T)
      do i=1,nce
        d(i) = p_eos%saftvrmie_var%dhs%d(i,i)
        d_T(i) = p_eos%saftvrmie_var%dhs%d_T(i,i)
      enddo
    class is (ljs_wca_eos)
      call calc_dhs_WCA(1,p_eos%sigma,p_eos%eps_divk,T,p_eos%dhs)
      do i=1,nce
        d(i) = p_eos%dhs%d(i,i)
        d_T(i) = p_eos%dhs%d_T(i,i)
      enddo
    class is (PETS_eos)
      call p_eos%calc_d_pets(T,d=d,d_T=d_T)
    class default
      call stoperror("calc_hard_sphere_diameter: Wrong eos...")
    end select

  end subroutine calc_hard_sphere_diameter

  !> Calculates non-additive Hard-sphere diameter
  subroutine calc_hard_sphere_diameter_ij(i,j,T,d,d_T)
    use saftvrmie_interface, only: update_saftvrmie_hs_diameter
    use saftvrmie_containers, only: saftvrmie_eos
    ! Input.
    integer, intent(in) :: i, j
    real, intent(in) :: T
    ! Output.
    real, intent(out) :: d !(m)
    real, intent(out) :: d_T !(m/K)
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    ! Calculate the non-additive hard-sphere diameter
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      call update_saftvrmie_hs_diameter(p_eos,nce,T)
      d = p_eos%saftvrmie_var%dhs%d(i,j)
      d_T = p_eos%saftvrmie_var%dhs%d_T(i,j)
    class default
      call stoperror("calc_hard_sphere_diameter_ij: Wrong eos...")
    end select
  end subroutine calc_hard_sphere_diameter_ij

  !> Enable/disable truncation corrections
  subroutine truncation_corrections(enable_truncation_correction, &
       enable_shift_correction, reduced_radius_cut)
    use saftvrmie_containers, only: saftvrmie_eos
    ! Input.
    logical, intent(in) :: enable_truncation_correction
    logical, intent(in) :: enable_shift_correction
    real, intent(in) :: reduced_radius_cut
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    ! Calculate .
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      call p_eos%svrm_opt%set_r_cut(reduced_radius_cut)
      call p_eos%svrm_opt%truncation_correction_model_control(enable_truncation_correction, &
           enable_shift_correction)
    class default
      call stoperror("truncation_corrections: Wrong eos...")
    end select

  end subroutine truncation_corrections

  !> Return de Broglie wavelength for component i
  !!
  !! \author Morten Hammer, March 2022
  subroutine de_Broglie_wavelength(i, T, lambda)
    use numconstants, only: pi
    use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_param
    use thermopack_constants, only: h_const, kB_const, N_Avogadro
    use thermopack_var, only: thermo_model
    ! Input
    integer, intent(in) :: i !< Component number
    real, intent(in) :: T !< Temperature
    real, intent(out) :: lambda !< de Broglie wavelength
    !
    ! Locals
    type(thermo_model), pointer :: p_thermo
    class(base_eos_param), pointer :: eos
    real :: mass
    p_thermo => get_active_thermo_model()
    eos => get_active_eos()
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      mass = saftvrmie_param%comp(i)%mass
      if (mass < 0.0) then
        mass = 1.0e-3*p_thermo%comps(i)%p_comp%mw/N_Avogadro
      endif
    class default
      mass = 1.0e-3*p_thermo%comps(i)%p_comp%mw/N_Avogadro
    end select
    lambda = h_const/sqrt(2*pi*mass*kB_const*T)

  end subroutine de_Broglie_wavelength

  !> Return de Boer parameter for component i
  !!
  !! \author Morten Hammer, July 2022
  subroutine de_boer_parameter(i, lambda)
    use saftvrmie_containers, only: saftvrmie_eos, get_saftvrmie_pure_fluid_deBoer
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use thermopack_constants, only: h_const, kB_const, N_Avogadro
    use thermopack_var, only: thermo_model
    ! Input
    integer, intent(in) :: i !< Component number
    real, intent(out) :: lambda !< de Boer
    !
    ! Locals
    type(thermo_model), pointer :: p_thermo
    class(base_eos_param), pointer :: eos
    real :: mass, sigma, eps_div_kb
    p_thermo => get_active_thermo_model()
    eos => get_active_eos()
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      call get_saftvrmie_pure_fluid_deBoer(i, lambda)
    class is (sPCSAFT_eos)
      sigma = p_eos%sigma(i,i)
      eps_div_kb = p_eos%eps_depth_divk(i,i)
      mass = 1.0e-3*p_thermo%comps(i)%p_comp%mw/N_Avogadro
      lambda = h_const/(sigma*sqrt(mass*eps_div_kb*kB_const))
    class default
      print *,"Need to implement de Boer function for specified model"
      stop
    end select
  end subroutine de_boer_parameter

  !> Return de Boer parameter for component i
  !!
  !! \author Morten Hammer, July 2022
  subroutine adjust_mass_to_specified_de_boer_parameter(i, lambda)
    use saftvrmie_containers, only: saftvrmie_eos, set_saftvrmie_pure_fluid_deBoer
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use thermopack_constants, only: h_const, kB_const, N_Avogadro
    use thermopack_var, only: thermo_model
    ! Input
    integer, intent(in) :: i !< Component number
    real, intent(in) :: lambda !< de Boer
    !
    ! Locals
    type(thermo_model), pointer :: p_thermo
    class(base_eos_param), pointer :: eos
    real :: mass, sigma, eps_div_kb
    p_thermo => get_active_thermo_model()
    eos => get_active_eos()
    ! Calculate the non-association contribution.
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      call set_saftvrmie_pure_fluid_deBoer(i, lambda)
    class is (sPCSAFT_eos)
      sigma = p_eos%sigma(i,i)
      eps_div_kb = p_eos%eps_depth_divk(i,i)
      mass = (h_const/(lambda*sigma))**2/(eps_div_kb*kB_const)
      p_thermo%comps(i)%p_comp%mw = mass*1.0e3*N_Avogadro
    class default
      print *,"Need to implement mass adjustment function for specified model"
      stop
    end select
  end subroutine adjust_mass_to_specified_de_boer_parameter

  !> Return interaction potential between component i and j
  !!
  !! \author Morten Hammer, July 2022
  subroutine potential(i, j, n, r, T, pot)
    use saftvrmie_containers, only: saftvrmie_eos, calc_DFeynHibbsij, &
         saftvrmie_param
    use saftvrmie_hardsphere, only: mie_potential_quantumcorrected_wrapper
    use thermopack_var, only: base_eos_param, thermo_model, nce
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use pets, only: PETS_eos
    use lj_splined, only: ljs_bh_eos, ljs_wca_eos, ljs_potential_reduced
    ! Input
    integer, intent(in) :: i, j !< Component number
    real, intent(in) :: T !< Temperature
    integer, intent(in) :: n !< Array size
    real, intent(in) :: r(n) !< Intermolecular separation (m)
    real, intent(out) :: pot(n) !< Potential divided by Boltzmann constant
    !
    ! Locals
    type(thermo_model), pointer :: p_thermo
    class(base_eos_param), pointer :: eos
    integer :: ir, i_cut
    real, parameter :: max_pot_val = 500.0
    real :: eps_divk, sigma, r_div_sigma(n), r_cut(1), u_cut(1)
    p_thermo => get_active_thermo_model()
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is (ljs_bh_eos)
      sigma = p_eos%saftvrmie_param%sigma_ij(1,1)
      r_div_sigma = r/sigma
      call ljs_potential_reduced(n, r_div_sigma, pot)
      eps_divk = p_eos%saftvrmie_param%eps_divk_ij(1,1)
      pot = pot*eps_divk
    class is (ljs_wca_eos)
      sigma = p_eos%sigma
      r_div_sigma = r/sigma
      call ljs_potential_reduced(n, r_div_sigma, pot)
      eps_divk = p_eos%eps_divk
      pot = pot*eps_divk
    class is (saftvrmie_eos)
      ! Update Feynman--Hibbs D parameter
      if (p_eos%svrm_opt%quantum_correction_hs > 0) &
           call calc_DFeynHibbsij(nce,T,p_eos%saftvrmie_param%DFeynHibbsParam_ij, &
           p_eos%saftvrmie_var%DFeynHibbsij, p_eos%saftvrmie_var%D2FeynHibbsij)
      pot = mie_potential_quantumcorrected_wrapper(i,j, p_eos%saftvrmie_var, n, r)
      eps_divk = saftvrmie_param%eps_divk_ij(i,j)
      if (p_eos%svrm_opt%enable_truncation_correction) then
        ! Correct potential for truncation correction
        r_cut = p_eos%svrm_opt%r_cut*p_eos%saftvrmie_param%sigma_ij(i,j)
        u_cut = mie_potential_quantumcorrected_wrapper(i,j, p_eos%saftvrmie_var, 1, r_cut)
        i_cut = n
        do ir=1,n
          if (r(ir) > r_cut(1)) then
            i_cut = ir
            exit
          endif
        enddo
        if (i_cut < n) pot(i_cut:n) = 0
        if (p_eos%svrm_opt%enable_shift_correction) then
          if (i_cut > 1) pot(1:i_cut-1) = pot(1:i_cut-1) - u_cut(1)
        endif
      endif
    class is (sPCSAFT_eos)
      pot = 4.0 * p_eos%eps_depth_divk(i,j) &
           * ((p_eos%sigma(i,j) / r)**12 - (p_eos%sigma(i,j) / r)**6)
      eps_divk = p_eos%eps_depth_divk(i,j)
    class is (PETS_eos)
      call p_eos%calc_potential_pets(n,r,pot)
      eps_divk = p_eos%epsdivk_pets
    class default
      print *,"Need to implement potential function for specified model"
      stop
      pot = 0
      eps_divk = 1
    end select

    do ir=1,n
      if (pot(ir) /= pot(ir)) then
        ! Avoid NaN in output
        pot(ir) = max_pot_val*eps_divk
      else if (pot(ir) > max_pot_val*eps_divk) then
        ! Cap potential at max_pot_val
        pot(ir) = max_pot_val*eps_divk
      endif
    enddo

  end subroutine potential

  !> Calculate dimensionless van der Waals energy for interaction potentials
  !!
  !! \author Morten Hammer, June 2023
  subroutine alpha(T, a_ij)
    use saftvrmie_containers, only: saftvrmie_eos
    use saftvrmie_interface, only: calc_alpha_saftvrmie
    use saftvrmie_dispersion, only: calc_alpha_ts
    use thermopack_var, only: base_eos_param, nce
    !use pc_saft_nonassoc, only: sPCSAFT_eos
    use pets, only: PETS_eos
    use lj_splined, only: ljs_bh_eos, ljs_wca_eos, alpha_ljs
    ! Input
    real, intent(in) :: T !< Temperature
    real, intent(out) :: a_ij(nce,nce) !< Dimensionless van der Waals energy
    !
    ! Locals
    integer :: i, j
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is (ljs_bh_eos)
      a_ij = alpha_ljs
    class is (ljs_wca_eos)
      a_ij = alpha_ljs
    class is (saftvrmie_eos)
      if (p_eos%svrm_opt%enable_truncation_correction) then
        ! Correct alpha for truncation and shifted (?) potential
        do i=1,nce
          do j=i,nce
            call calc_alpha_ts(i, j, p_eos%saftvrmie_var, a_ij(i,j))
            if (i /= j) a_ij(j,i) = a_ij(i,j)
          enddo
        enddo
      else
        a_ij = calc_alpha_saftvrmie(T)
      endif
    class is (PETS_eos)
      a_ij = 0.73463521003968
    class default
      print *,"Need to implement potential function for specified model"
      stop
      a_ij = 0
    end select

  end subroutine alpha

  !> Get well depth divided by kB for interaction i and j
  !!
  !! \author Morten Hammer, June 2023
  subroutine epsilon_ij(i, j, eps_div_kb_ij)
    use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_param
    use thermopack_var, only: base_eos_param
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use pets, only: PETS_eos
    use lj_splined, only: ljs_bh_eos, ljs_wca_eos
    ! Input
    integer, intent(in) :: i, j !< Component number
    real, intent(out) :: eps_div_kb_ij !< Well depth divided by Boltzmann constant
    !
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is (ljs_bh_eos)
      eps_div_kb_ij = p_eos%saftvrmie_param%eps_divk_ij(1,1)
    class is (ljs_wca_eos)
      eps_div_kb_ij = p_eos%eps_divk
    class is (saftvrmie_eos)
      eps_div_kb_ij = saftvrmie_param%eps_divk_ij(i,j)
    class is (sPCSAFT_eos)
      eps_div_kb_ij = p_eos%eps_depth_divk(i,j)
    class is (PETS_eos)
      eps_div_kb_ij = p_eos%epsdivk_pets
    class default
      print *,"Need to implement epsilon_ij function for specified model"
      stop
      eps_div_kb_ij = 1
    end select

  end subroutine epsilon_ij

  !> Get effective well depth divided by kB for interaction i and j
  !!
  !! \author Morten Hammer, June 2023
  subroutine epsilon_eff_ij(i, j, T, eps_div_kb_ij)
    use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_param, &
         saftvrmie_var_container, svrm_opt, get_saftvrmie_var, &
         calc_DFeynHibbsij
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
         calc_binary_effective_sigma, calc_binary_effective_eps_divk
    use thermopack_var, only: base_eos_param, nce
    ! Input
    integer, intent(in) :: i, j !< Component number
    real, intent(in) :: T !< Temperature (K)
    real, intent(out) :: eps_div_kb_ij !< Effective well depth divided by Boltzmann constant
    !
    ! Locals
    type(saftvrmie_var_container), pointer :: svrm_var
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      if (svrm_opt%quantum_correction_hs > 0) then
        svrm_var => get_saftvrmie_var()
        ! Calculate Feynman--Hibbs D parameter
        call calc_DFeynHibbsij(nce, T, saftvrmie_param%DFeynHibbsParam_ij, &
             svrm_var%DFeynHibbsij, svrm_var%D2FeynHibbsij)
        ! Calculate effective sigma
        call calc_binary_effective_sigma(nce,T,svrm_var,svrm_var%sigma_eff%d,&
             svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
        ! Calculate effective epsilon divided by k
        call calc_binary_effective_eps_divk(nce,T,svrm_var,svrm_var%eps_divk_eff%d,&
             svrm_var%eps_divk_eff%d_T,svrm_var%eps_divk_eff%d_TT)
        eps_div_kb_ij = svrm_var%eps_divk_eff%d(i,j)
      else
        eps_div_kb_ij = saftvrmie_param%eps_divk_ij(i,j)
      endif
    class default
      call epsilon_ij(i, j, eps_div_kb_ij)
    end select
  end subroutine epsilon_eff_ij

  !> Size parameter for interaction i and j
  !!
  !! \author Morten Hammer, June 2023
  subroutine sigma_ij(i, j, s_ij)
    use saftvrmie_containers, only: saftvrmie_eos
    use thermopack_var, only: base_eos_param
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use pets, only: PETS_eos
    use lj_splined, only: ljs_bh_eos, ljs_wca_eos
    ! Input
    integer, intent(in) :: i, j !< Component number
    real, intent(out) :: s_ij !< Size paramater (m)
    !
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is (ljs_bh_eos)
      s_ij = p_eos%saftvrmie_param%sigma_ij(1,1)
    class is (ljs_wca_eos)
      s_ij = p_eos%sigma
    class is (saftvrmie_eos)
      s_ij = p_eos%saftvrmie_param%sigma_ij(i,j)
    class is (sPCSAFT_eos)
      s_ij = p_eos%sigma(i,j)
    class is (PETS_eos)
      s_ij = p_eos%sigma_pets
    class default
      print *,"Need to implement sigma_ij function for specified model"
      stop
      s_ij = 0
    end select
  end subroutine sigma_ij

  !> Effective size parameter for interaction i and j
  !!
  !! \author Morten Hammer, June 2023
  subroutine sigma_eff_ij(i, j, T, s_ij)
    use saftvrmie_containers, only: saftvrmie_eos, saftvrmie_param, &
         saftvrmie_var_container, svrm_opt, get_saftvrmie_var, &
         calc_DFeynHibbsij
    use saftvrmie_hardsphere, only: calc_hardsphere_diameter, &
         calc_binary_effective_sigma
    use thermopack_var, only: base_eos_param, nce
    ! Input
    integer, intent(in) :: i, j !< Component number
    real, intent(in) :: T !< Temperature (K)
    real, intent(out) :: s_ij !< Effective size paramater (m)
    !
    ! Locals
    type(saftvrmie_var_container), pointer :: svrm_var
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is (saftvrmie_eos)
      if (svrm_opt%quantum_correction_hs > 0) then
        svrm_var => get_saftvrmie_var()
        ! Calculate Feynman--Hibbs D parameter
        call calc_DFeynHibbsij(nce, T, saftvrmie_param%DFeynHibbsParam_ij, &
             svrm_var%DFeynHibbsij, svrm_var%D2FeynHibbsij)
        ! Calculate effective sigma
        call calc_binary_effective_sigma(nce,T,svrm_var,svrm_var%sigma_eff%d,&
             svrm_var%sigma_eff%d_T,svrm_var%sigma_eff%d_TT)
        s_ij = svrm_var%sigma_eff%d(i,j)
      else
        s_ij = p_eos%saftvrmie_param%sigma_ij(i,j)
      endif
    class default
      call sigma_ij(i, j, s_ij)
    end select
  end subroutine sigma_eff_ij

  !> Test if model setup is comaptible with the Fundamental
  !! Measure Theory (FMT)
  !!
  !! \author Morten Hammer, October 2022
  subroutine test_fmt_compatibility(is_fmt_consistent, na_enabled)
    use saftvrmie_containers, only: saftvrmie_eos
    use pc_saft_nonassoc, only: sPCSAFT_eos, PCSAFT_eos
    use pets, only: PETS_eos
    use lj_splined, only: ljs_wca_eos, ljs_bh_eos
    use thermopack_var, only: base_eos_param, nce
    logical, intent(out) :: is_fmt_consistent, na_enabled
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    na_enabled = .false.
    select type ( p_eos => eos )
    class is (ljs_bh_eos)
      is_fmt_consistent = .true.
    class is (saftvrmie_eos)
      if (nce == 1) then
        is_fmt_consistent = .true.
      else
        call p_eos%svrm_opt%test_fmt_compatibility(is_fmt_consistent, na_enabled)
      endif
    class is (PCSAFT_eos)
      is_fmt_consistent = .true.
    class is (sPCSAFT_eos)
      if (nce == 1) then
        is_fmt_consistent = .true.
      else
        is_fmt_consistent = .false.
      endif
    class is (PETS_eos)
      is_fmt_consistent = .true.
    class is (ljs_wca_eos)
      is_fmt_consistent = .true.
    class default
      is_fmt_consistent = .false.
    end select
  end subroutine test_fmt_compatibility

  !****************** ROUTINES NEEDED IN TPSINGLE **************************!

  !> Calculate the compressibility and its derivatives.
  subroutine saft_zfac(nc,eos,phase,T,P,n,Z,dZdT,dZdP,dZdn)
    use eos_parameters, only: base_eos_param
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    integer, intent(in) :: phase
    real, intent(in) :: T                               !< Temperature [K]
    real, intent(in) :: P                               !< Pressure [Pa]
    real, intent(in) :: n(nc)                           !< Mole numbers [moles]
    ! Output.
    real, intent(out) :: Z
    real, optional, intent(out) :: dZdT
    real, optional, intent(out) :: dZdP
    real, optional, intent(out) :: dZdn(nc)
    ! Locals.
    real :: V     !< Volume [m^3].
    real :: sumn  !< Total mole number in mixture.
    real :: F_VV, F_TV, F_Vn(nc)
    real :: dPdV, dPdT, dPdn(nc)
    real :: dVdT, dVdn(nc)

    sumn = sum(n)

    call saft_master_volume_solver (nc,eos,T,P,n,phase,V)
    Z = P*V/(sumn*Rgas*T)

    if (present(dZdT) .or. present(dZdP) .or. present(dZdn)) then
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F_VV=F_VV,F_TV=F_TV,F_Vn=F_Vn)
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
    end if

    if (present(dZdT)) then
      dPdT = P/T - Rgas*T*F_TV
      dVdT = -dPdT/dPdV
      dZdT = -Z*(1.0/T - dVdT/V)
    end if

    if (present(dZdP)) then
      dZdP = Z*(1.0/P + 1.0/(dPdV*V))
    end if

    if (present(dZdn)) then
      dPdn = Rgas*T*(-F_Vn + 1/V)
      dVdn = -dPdn/dPdV
      dZdn = -Z*(1.0/sumn - dVdn/V)
    end if

  end subroutine saft_zfac

  !> Calculate the logarithmic fugacity and its derivatives.
  subroutine saft_lnphi(nc,eos,phase,T,P,n,lnphi,dlnphidT,dlnphidP,dlnphidn)
    use eos_parameters, only: base_eos_param
    ! Input.
    integer, intent(in) :: nc
    class (base_eos_param), intent(inout) :: eos
    integer, intent(in) :: phase
    real, intent(in) :: T                               !< Temperature [K]
    real, intent(in) :: P                               !< Pressure [Pa]
    real, intent(in) :: n(nc)                           !< Mole numbers [moles]
    ! Output
    real, intent(out) :: lnphi(nc)
    real, optional, intent(out) :: dlnphidt(nc), dlnphidp(nc), dlnphidn(nc,nc)
    ! Locals.
    real :: V     !< Volume [m^3].
    real :: sumn  !< Total mole number in mixture [mole]
    real :: zFac
    real :: F_n(nc),F_Tn(nc),F_TV,F_VV,F_Vn(nc),F_nn(nc,nc)
    real :: dPdV, dPdT, dPdn(nc)
    real :: dVdn(nc)
    integer :: i,j
    sumn = sum(n)

    call saft_master_volume_solver(nc,eos,T,P,n,phase,V)

    zFac = P*V/(sumn*Rgas*T)

    if (present(dlnphidt) .or. present(dlnphidp) .or. present(dlnphidn)) then
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F_n=F_n,F_VV=F_VV,F_Vn=F_Vn,F_TV=F_TV,F_Tn=F_Tn,F_nn=F_nn)
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
      dPdn = Rgas*T*(-F_Vn + 1/V)
      dVdn = -dPdn/dPdV
    else
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F_n=F_n)
    end if

    lnphi = F_n - log(zFac)

    if (present(dlnphidt)) then
      dPdT = P/T-Rgas*T*F_TV
      dlnphidt = F_Tn + (1 - dVdn*dPdT/Rgas)/T
    endif

    if (present(dlnphidp)) then
      dlnphidp = dVdn/(Rgas*T)-1/P
    endif

    if (present(dlnphidn)) then
      do i=1,nc
        do j=1,nc
          dlnphidn(i,j) = F_nn(i,j) + 1/sumn - dVdn(j)*dPdn(i)/(Rgas*T)
        end do
      end do
    endif

  end subroutine saft_lnphi


  ! !-----------------------------------------------------------------------------
  ! !> Calculate residual entropy given pressure, temperature and composition
  ! !>
  ! !> \author Ailo A, 2015-04
  ! !-----------------------------------------------------------------------------
  subroutine saft_ResidEntropy(nc,eos,phase,T,P,n,S,dSdt,dSdp,dSdn)
    use compdata, only: gendata
    use thermopack_var, only: base_eos_param
    integer, intent(in) :: nc !< Number of components in mixture.
    class(base_eos_param), intent(inout) :: eos !< Cubic eos for
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(in) :: T !< Temperature [K]
    integer, intent(in) :: phase !< Phase identifier [-]
    real, dimension(nc), intent(in) :: n !< Composition [mol]
    real, intent(out) :: S !< Entropy [J/mol/K]
    real, optional, intent(out) :: dSdt,dSdp
    real, dimension(nc), optional, intent(out) :: dSdn
    ! Locals
    real :: V
    real :: dPdV, dPdT, dVdT, dPdn(nc), dVdn(nc)
    real :: sumn
    real :: zFac
    real :: F,F_T,F_TT,F_n(nc),F_Tn(nc),F_TV,F_VV,F_Vn(nc)
    sumn = sum(n)

    call saft_master_volume_solver(nc,eos,T,P,n,phase,V)
    zFac = P*V/(sumn*Rgas*T)

    if (present(dSdt) .or. present(dSdp) .or. present(dSdn)) then
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F=F,F_n=F_n,F_T=F_T, &
           F_VV=F_VV,F_TV=F_TV,F_TT=F_TT,F_Vn=F_Vn,F_Tn=F_Tn)
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
      dPdT = P/T-Rgas*T*F_TV
      dVdT = -dPdT/dPdV
    else
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F=F,F_T=F_T)
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

  end subroutine saft_ResidEntropy

  ! !-----------------------------------------------------------------------------
  ! !> Calculate residual enthalpy given pressure, temperature and composition.
  ! !>
  ! !> \author Ailo A, 2015-04
  ! !-----------------------------------------------------------------------------
  subroutine saft_ResidEnthalpy(nc,eos,phase,T,P,n,H,dHdT,dHdP,dHdn)
    use compdata, only: gendata
    use thermopack_var, only: base_eos_param
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos !< Cubic eos
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(in) :: T !< Temperature [K]
    integer, intent(in) :: phase !< Phase identifier [-]
    real, dimension(nc), intent(in) :: n !< Composition [mol]
    real, intent(out) :: H !< Enthalpy [J/mol/K]
    real, optional, intent(out) :: dHdt, dHdp
    real, optional, intent(out) :: dHdn(nc)
    ! Locals
    real :: V
    real :: dPdV, dPdT, dVdT
    real, dimension(nc) ::  dPdn, dVdn
    real :: F_T,F_VV,F_TV,F_TT,F_Vn(nc),F_Tn(nc)
    real :: sumn
    sumn = sum(n)

    call saft_master_volume_solver(nc,eos,T,P,n,phase,V)

    if (present(dHdt) .or. present(dHdp) .or. present(dHdn)) then
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F_T=F_T,F_VV=F_VV,F_TV=F_TV,F_TT=F_TT,F_Vn=F_Vn,F_Tn=F_Tn)
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
      dPdT = P/T-Rgas*T*F_TV
      dVdT = -dPdT/dPdV
    else
      call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F_T=F_T)
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
  end subroutine saft_ResidEnthalpy

  ! !-----------------------------------------------------------------------------
  ! !> Calculate residual Gibbs energy given pressure, temperature and composition
  ! !>
  ! !> \author Ailo, 2015-04
  ! !-----------------------------------------------------------------------------
  subroutine saft_ResidGibbs(nc,eos,phase,T,P,n,G,dGdT,dGdP,dGdn)
    use compdata, only: gendata
    use thermopack_var, only: base_eos_param
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos !< Cubic eos.
    real, intent(in) :: P !< Pressure [Pa]
    real, intent(in) :: T !< Temperature [K]
    integer, intent(in) :: phase !< Phase identifier [-]
    real, dimension(nc), intent(in) :: n !< Composition [mol]
    real, intent(out) :: G !< Gibbs free energy [J/mol]
    real, optional, intent(out) :: dGdT, dGdP
    real, dimension(nc), optional, intent(out) :: dGdn
    ! Locals
    real :: zFac,V
    real :: dPdV, dPdT, dVdT
    real :: F,F_VV,F_TV,F_T,F_V,F_n(nc)
    integer :: i
    real :: sumn
    sumn = sum(n)

    call saft_master_volume_solver(nc,eos,T,P,n,phase,V)
    zFac = P*V/(sumn*Rgas*T)

    call calcSaftFder_res(nc=nc,eos=eos,T=T,V=V,n=n,F=F,F_VV=F_VV,F_TV=F_TV,F_T=F_T,F_V=F_V,F_n=F_n)
    G = Rgas*T*F + P*V - sumn*Rgas*T*(1+log(zFac))

    if (present(dGdP)) then
      dGdp = V*(1-1/zFac)
    endif

    if (present(dGdT)) then
      dPdV = -Rgas*T*(F_VV + sumn/V**2)
      dPdT = P/T-Rgas*T*F_TV
      dVdT = -dPdT/dPdV
      dGdT =  Rgas*(F + T*F_T - sumn*log(zfac)) &
           + (P-P/zfac+Rgas*T*F_V)*dVdT
    end if

    if (present(dGdn)) then
      do i = 1,nc
        dGdn(i) = Rgas*T*(F_n(i) - log(zFac))
      end do
    endif

  end subroutine saft_ResidGibbs

  subroutine saft_master_volume_solver(nc,cbeos,T,P_spec,n,phase,V)
    use saft_association, only: numAssocSites
    use eos_parameters, only: base_eos_param
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: cbeos
    real, intent(in) :: T
    real, intent(in) :: P_spec
    real, intent(in) :: n(nc)
    integer, intent(in) :: phase
    ! Output.
    real, intent(out) :: V

    if (numAssocSites > 0) then
      call saft_volume_solver(nc,cbeos,T,P_spec,n,phase,V)
    else
      call pc_saft_nonassoc_volume_solver(nc,cbeos,T,P_spec,n,phase,V)
    end if

  end subroutine saft_master_volume_solver


  !> Routine for when we want to use PC-SAFT on a non-association mixture.
  subroutine pc_saft_nonassoc_volume_solver(nc,cbeos,T,P_spec,n,phase,V)
    use saft_association, only: numAssocSites
    use thermopack_var, only: base_eos_param
    use thermopack_constants, only: VAPPH, verbose
    use numconstants, only: machine_prec ! Equals 2^{-52} ~ 2.22*e-16 for double precision reals.
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: cbeos
    real, intent(in) :: T ! [K]
    real, intent(in) :: P_spec ! [Pa]
    real, intent(in) :: n(nc) ! [mol]
    integer, intent(in) :: phase
    ! Output.
    real, intent(out) :: V ! [m^3/mol]
    ! Locals.
    real :: Vold
    real :: zeta                            !< Reduced density
    real :: F, dFdzeta                      !< The objective function (not Helmholtz energy)
    real :: zetaMin, zetaMax
    real :: P, P_V
    integer :: iter, maxiter
    real :: conv_num
    logical :: V_has_converged, P_has_converged

    if (numAssocSites > 0) then
      call stoperror("Don't call pc_saft_nonassoc_volume_solver on an associating mixture.")
    end if

    ! Compute "conversion numerator".
    conv_num = conversion_numerator(cbeos,nc,T,n)

    ! Initialize the reduced density zeta.
    if (phase .eq. VAPPH) then
      zeta = 1e-10
    else
      zeta = 0.5
    end if

    ! Set the initial volume.
    V = conv_num/zeta

    ! Set zeta limits.
    zetaMin = 1e-10
    zetaMax = 0.75 ! Higher values entails unphysically close packing of segments (Gross&Sadowski 2001).

    ! Compute the pressure P.
    call nonassoc_pressure(nc,cbeos,T,V,n,P=P,dPdV=P_V)

    ! Initialize iteration variables and objective function.
    maxiter = 40
    iter = 0
    F = (1-zeta)*(P-P_spec)

    ! Volume iteration loop. The starting condition is:
    ! we have a value of zeta, and the corresponding V and F.
    do
      ! Perform a Newton iteration to get a new zeta.
      dFdzeta = P_spec - P - ((1-zeta)/zeta)*P_V*V
      zeta = zeta - F/dFdzeta

      ! Did we overshoot?
      if (zeta > zetaMax .or. zeta < zetaMin) then
        ! Yes. Do a bisection step.
        zeta = (zetaMax + zetaMin)/2
      end if

      ! Update V correspondingly. Also store the old value,
      ! since |V-Vold| is used in the convergence criterion.
      Vold = V
      V = conv_num/zeta

      ! Compute P and P_V at the new V, as well as the value of F.
      call nonassoc_pressure(nc,cbeos,T,V,n,P=P,dPdV=P_V)
      F = (1-zeta)*(P-P_spec)

      ! Convergence?
      V_has_converged = (abs(V-Vold) < 1e5*machine_prec*Vold)
      P_has_converged = (abs(P-P_spec) < 1e8*machine_prec*P_spec)
      if (V_has_converged .and. P_has_converged) exit

      ! Not converged yet. Have we reached the maximum number of iterations?
      iter = iter + 1
      if (V_has_converged .or. iter > maxiter) then
        if (verbose) then
          print *, "The SAFT non-association volume solver didn't fully converge. Sorry."
          print *, "T", T
          write(*,'(A,3ES15.5)') "P, P_spec, abs(P-P_spec)/P", P, P_spec, abs(P-P_spec)/P
          write(*,'(A,3ES15.5)') "Vold, V, abs(V-Vold)/Vold", Vold, V, abs(V-Vold)/Vold
          print *, "P_V<0?", P_V<0
          print *, " "
        end if
        exit
      end if

      ! Not converged and below maximum number of iterations.
      ! Use the sign of F to bound the value of zeta in the next iteration.
      if (F>0) then
        zetaMax = zeta
      elseif (F<0) then
        zetaMin = zeta
      end if
    end do

    ! Get V from zeta, and then exit.
    V = conv_num/zeta
  end subroutine pc_saft_nonassoc_volume_solver

  !> Calculate conversion numerator for pressure solver
  function conversion_numerator(eos,nc,T,n) result(conv_num)
    use pc_saft_nonassoc, only: calc_d, sPCSAFT_eos
    use pets, only: eta_pets, PETS_eos
    use thermopack_constants, only: N_AVOGADRO
    use numconstants, only: PI
    use saftvrmie_interface, only: calc_saftvrmie_zeta
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_bh_eos, calc_ljs_bh_zeta, &
         ljs_wca_eos, calc_ljx_wca_zeta
    ! Input.
    class(base_eos_param), intent(inout) :: eos
    integer, intent(in) :: nc
    real, intent(in) :: T
    real, intent(in) :: n(nc)
    ! Output.
    real :: conv_num
    ! Locals.
    real :: prod_sum, diam(nc)
    integer :: i
    select type(p_eos => eos)
    class is(sPCSAFT_eos)
      call calc_d(p_eos,T=T,d=diam)
      prod_sum = 0.0
      do i=1,nc
        prod_sum = prod_sum + n(i)*p_eos%m(i)*diam(i)**3
      end do
      conv_num = N_AVOGADRO*(PI/6)*prod_sum/sum(n)
    class is(ljs_bh_eos)
      conv_num = calc_ljs_bh_zeta(p_eos,nc,T,1.0,n)
    class is(ljs_wca_eos)
      conv_num = calc_ljx_wca_zeta(p_eos,nc,T,1.0,n)
    class is(saftvrmie_eos)
      conv_num = calc_saftvrmie_zeta(p_eos,nc,T,1.0,n)
    class is ( PETS_eos )
      call eta_pets(p_eos, rho=1.0,T=T,n=n, e=conv_num)
    class default
      call stoperror("No such SAFT model")
    end select
  end function conversion_numerator

  !> Volume solver for associating mixtures. Modeled after the paper: Michelsen
  !> (2006) "Robust and Efficient Solution Procedures for Association Models."
  subroutine saft_volume_solver (nc,eos,T,P_spec,n,phase,V)
    use thermopack_var, only: base_eos_param
    use thermopack_constants, only: VAPPH, verbose
    use numconstants, only: machine_prec ! Equals 2^{-52} ~ 2.22*e-16 for double precision reals.
    use saft_association, only: numAssocSites, solve_for_X_k
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in) :: T ! [K]
    real, intent(in) :: P_spec ! [Pa]
    real, intent(in) :: n(nc) ! [mol]
    integer, intent(in) :: phase
    ! Output.
    real, intent(out) :: V ! [m^3/mol]
    ! Locals.
    real, dimension(numAssocSites) :: X_k
    real, dimension(numAssocSites) :: X_V
    real :: Vold
    real :: sumn                                       !< The total number of moles in the mixture
    real :: zeta                                       !< Reduced density
    real :: F, dFdzeta                                 !< The objective function (not Helmholtz energy)
    real :: zetaMin, zetaMax
    real :: P, P_V
    integer :: iter, maxiter
    real :: b_mix, conv_num
    logical :: V_has_converged, P_has_converged
    sumn = sum(n)

    ! Compute conversion numerator and initialize the reduced density zeta.
    if (eos%assoc%saft_model == eosOPC_SAFT .or. &
         eos%assoc%saft_model == eosSPC_SAFT .or. &
         eos%assoc%saft_model == eosPCP_SAFT .or. &
         eos%assoc%saft_model == eosSPCP_SAFT .or. &
         eos%assoc%saft_model == eosSAFT_VR_MIE .or. &
         eos%assoc%saft_model == eosLJS_BH .or. &
         eos%assoc%saft_model == eosLJS_WCA .or. &
         eos%assoc%saft_model == eosLJS_UF .or. &
         eos%assoc%saft_model == eosLJS_UV .or. &
         eos%assoc%saft_model == eosLJ_UF) then
      conv_num = conversion_numerator(eos,nc,T,n)
      if (phase .eq. VAPPH) then
        zeta = 1e-10
      else
        zeta = 0.5
      end if

      zetaMin = 1e-10
      zetaMax = 0.75
    else
      b_mix = get_bmix(nc, eos, n)
      conv_num = b_mix*sumn

      if (phase .eq. VAPPH) then
        zeta = b_mix/(b_mix+Rgas*T/P_spec)
      else
        zeta = 0.99
      end if

      zetaMin = 0.0
      zetaMax = 1.0
    end if

    ! Set the param vector.
    V = conv_num/(zeta)
    call eos%assoc%state%init(nc,T,V,n)

    ! Initialize all X_k components to 0.2, and then converge X_k exactly.
    X_k = 0.2
    call solve_for_X_k(eos,nc,X_k,maxit=10,tol=machine_prec*1e9)

    ! Having obtained the correct value of X_k, compute the corresponding
    ! pressure P.
    call saft_total_pressure_knowing_X_k(nc,eos,X_k,P)

    ! Initialize iteration variables and objective function.
    V_has_converged = .false.
    maxiter = 80
    iter = 0
    F = (1-zeta)*(P-P_spec)

    ! Volume iteration loop. The starting condition is: we have a value of V and
    ! zeta, and the corresponding param, X_k and F.
    do
      ! Compute X_V and P_V at V.
      call compute_dxdv_and_dpdv(nc,eos,X_k,X_V,P_V)

      ! Perform a Newton iteration to get a new zeta.
      dFdzeta = P_spec - P - ((1-zeta)/zeta)*P_V*V
      zeta = zeta - F/dFdzeta

      ! Did we overshoot?
      if (zeta > zetaMax .or. zeta < zetaMin) then
        ! Yes. Do a bisection step.
        zeta = (zetaMax + zetaMin)/2
      end if

      ! Update V correspondingly. We also store the old value, since |V-Vold| is
      ! used as a convergence criterion.
      Vold = V
      V = conv_num/(zeta)

      ! Update param. Solve for X_k given the current volume.
      eos%assoc%state%V = V
      X_k = X_k + X_V*(V-Vold)
      call solve_for_X_k(eos,nc,X_k,maxit=10,tol=machine_prec*1e8)

      ! Compute the pressure P at V, and the value of F.
      call saft_total_pressure_knowing_X_k(nc,eos,X_k,P)
      F = (1-zeta)*(P-P_spec)

      ! Convergence?
      V_has_converged = (abs(V-Vold) < 1e5*machine_prec*Vold)
      P_has_converged = (abs(P-P_spec) < 1e8*machine_prec*P_spec)
      if (V_has_converged .and. P_has_converged) exit

      ! Not converged. Have we reached the maximum number of iterations?
      iter = iter + 1
      if (V_has_converged .or. iter > maxiter) then
         if (verbose) then
            print *, "The SAFT volume solver didn't fully converge. Sorry."
            print *, "T", T
            write(*,'(A,3ES15.5)') "P,P_spec, abs(P-P_spec)/P", P, P_spec, abs(P-P_spec)/P
            write(*,'(A,3ES15.5)') "Vold, V, abs(V-Vold)/Vold", Vold, V, abs(V-Vold)/Vold
            print *, "P_V<0?", P_V<0
            print *, " "
         endif
         exit
      end if

      ! Not converged, and below maximum number of iterations. Use the sign of F
      ! to bound the value of zeta in the next iteration.
      if (F>0) then
        zetaMax = zeta
      elseif (F<0) then
        zetaMin = zeta
      end if
    end do

    ! Get V from zeta, and then exit.
    V = conv_num/(zeta)
  contains
    function get_bmix(nc, cbeos, n) result(bm)
      use cubic_eos, only: cb_eos
      integer, intent(in) :: nc
      real, intent(in) :: n(nc) ! [mol]
      class(base_eos_param), intent(inout) :: cbeos
      real :: bm
      ! Locals
      select type ( p_eos => cbeos )
      class is ( cb_eos )
        bm = dot_product(p_eos%single%b/1000,n)/sum(n)
      class default
        bm = 0
      end select
    end function get_bmix
  end subroutine saft_volume_solver


  !> A back-end procedure giving the combined pressure of the cubic
  !> contribution and the association contribution.
  subroutine saft_total_pressure_knowing_X_k(nc,eos,X_k,P,&
       dPdV,dPdT,dPdn)
    use thermopack_var, only: base_eos_param
    use saft_association, only: numAssocSites, numAssocSites, assoc_pressure
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in)  :: X_k(numAssocSites)
    real, intent(out) :: P !< Pressure [Pa]
    real, intent(out), optional :: dPdV, dPdT, dPdn(nc)
    ! Locals.
    real :: P_nonassoc, P_assoc
    real :: temp_v, temp_t, temp_n(nc)
    real  :: T !< Temperature [K]
    real  :: V !< Volume [m^3]
    real  :: n(nc)
    T = eos%assoc%state%T
    V = eos%assoc%state%V
    n = eos%assoc%state%n
    call assoc_pressure(eos,nc,X_k,P_assoc,dPdV=dPdV,dPdT=dPdT,dPdn=dPdn)
    if (present(dPdV)) temp_v = dPdV
    if (present(dPdT)) temp_t = dPdT
    if (present(dPdn)) temp_n = dPdn
    call nonassoc_pressure(nc,eos,T,V,n,P_nonassoc,&
         dPdV=dPdV,dPdT=dPdT,dPdn=dPdn)

    P = P_nonassoc + P_assoc
    if (present(dPdV)) dPdV = dPdV + temp_v
    if (present(dPdT)) dPdT = dPdT + temp_t
    if (present(dPdn)) dPdn = dPdn + temp_n

  end subroutine saft_total_pressure_knowing_X_k


  !> The pressure contribution not coming from association.
  subroutine nonassoc_pressure(nc,eos,T,V,n,P,dPdV,dPdT,dPdn)
    use cubic, only: cbCalcPressure
    use cubic_eos, only: cb_eos
    use eos_parameters, only: base_eos_param
    use pc_saft_nonassoc, only: F_sPC_SAFT_TVn, F_PC_SAFT_TVn, PCSAFT_eos, sPCSAFT_eos
    use pets, only: F_PETS_TVn, PETS_eos
    use saftvrmie_containers, only: saftvrmie_eos
    use saftvrmie_interface, only: calcFresSAFTVRMie
    use lj_splined, only: ljs_bh_eos, calcFresLJs_bh, &
         ljs_wca_eos, calcFres_WCA
    use multipol, only: add_hyperdual_fres_multipol
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, intent(in) :: T, V
    real, intent(in), dimension(nc) :: n
    real, intent(out) :: P
    ! Locals.
    real, optional, intent(out) :: dPdV, dPdT, dPdn(nc)
    real :: sumn
    real :: F_V, F
    logical :: correct_fres
    sumn = sum(n)
    correct_fres = .true.

    select type ( p_eos => eos )
    class is ( PCSAFT_eos )
      call F_PC_SAFT_TVn(p_eos, T=T,V=V,n=n,F_V=F_V,F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
    class is ( sPCSAFT_eos )
      call F_sPC_SAFT_TVn(p_eos, T=T,V=V,n=n,F_V=F_V,F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
    class is ( saftvrmie_eos )
      call calcFresSAFTVRMie(p_eos, nc,T,V,n,F,F_V=F_V,F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
    class is ( ljs_bh_eos )
      call calcFresLJs_bh(p_eos,nc,T,V,n,F,F_V=F_V,F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
    class is(ljs_wca_eos)
      call calcFres_WCA(p_eos,nc,T,V,n,F,F_V=F_V,F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
    class is ( PETS_eos )
      call F_PETS_TVn(p_eos, T=T,V=V,n=n,F_V=F_V,F_VV=dPdV,F_TV=dPdT,F_Vn=dPdn)
   class is ( cb_eos )
      ! This routine takes in the volume in L/mole.
      call cbCalcPressure(nc,p_eos,T,1000*V/sumn,n/sumn,P,&
           dPdv,dPdT,dpdz=dPdn)
      correct_fres = .false.
      ! Convert to volume derivative from (specific volume)-derivative.
      if (present(dPdV)) dPdV = 1000*dPdv/sumn
    class default
      call stoperror("nonassoc_pressure: Not able to call cbCalcPressure. Not cubic eos.")
    end select

    ! Include polar contributions
    if (associated(eos%mpol_param)) then
      call add_hyperdual_fres_multipol(eos,nc,T,V,n,&
           f_V=F_V,f_VV=dPdV,f_TV=dPdT,f_Vn=dPdn)
    endif

    if (correct_fres) then
      P = -Rgas*T*F_V + sumn*Rgas*T/V
      if (present(dPdV)) dPdV = -Rgas*T*dPdV - sumn*Rgas*T/V**2
      if (present(dPdT)) dPdT = -Rgas*T*dPdT + P/T
      if (present(dPdn)) dPdn = -Rgas*T*dPdn + Rgas*T/V
    endif
  end subroutine nonassoc_pressure


  !> Calculates the contibution to the reduced residual Helmholtz energy F
  !> coming from the non-association part, along with its derivatives.
  subroutine calcFder_nonassoc_cpa(nc,cbeos,T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use cubic, only: cbCalcDerivatives_svol
    use cbmix, only: cbCalcMixtureParams
    use cubic_eos, only: cb_eos
    !use eos_parameters, only: base_eos_param
    use cbhelm
    ! Input.
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T,V,n(nc)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    ! Locals.
    real :: sumn
    sumn = sum(n)

    ! Calculate contributions from the non-association part.
    call cbCalcMixtureParams(nc,cbeos,T,n/sumn)
    call cbCalcDerivatives_svol(nc,cbeos,T,1000*V/sumn)
    if (present(F)) F = sumn*cbF(cbeos)
    if (present(F_T)) F_T = sumn*cbFt(cbeos)
    if (present(F_V)) F_V = 1000*cbFv(cbeos)
    if (present(F_n)) call cbFi(nc,cbeos,F_n)
    if (present(F_TT)) F_TT = cbFtt(cbeos)*sumn
    if (present(F_TV)) F_TV = 1000*cbFvt(cbeos)
    if (present(F_Tn)) call cbFiT(nc,cbeos,F_Tn)
    if (present(F_VV)) F_VV = 1000*1000*cbFvv(cbeos)/sumn
    if (present(F_Vn)) then
      call cbFiv(nc,cbeos,F_Vn)
      F_Vn = 1000*F_Vn/sumn
    end if
    if (present(F_nn)) then
      call cbFij(nc,cbeos,F_nn)
      F_nn = F_nn/sumn
    end if
  end subroutine calcFder_nonassoc_cpa


  !> Special routine for computing the derivatives needed in the Newton
  !> iteration of volume_solver.
  subroutine compute_dXdV_and_dPdV(nc,eos,X_k,X_V,P_V)
    use saft_association, only: numAssocSites,X_derivatives_knowing_X, Q_derivatives_knowing_X
    use cubic_eos, only: cb_eos
    use thermopack_var, only: base_eos_param
    ! Input.
    integer, intent(in) :: nc
    class(base_eos_param), intent(inout) :: eos
    real, dimension(numAssocSites), intent(in) :: X_k
    !real, dimension(nc+2), intent(in) :: param
    ! Output.
    real, dimension(numAssocSites), intent(out) :: X_V
    real, intent(out) :: P_V
    ! Locals.
    real :: T,V,n(nc)
    real :: Q_VV, Q_XV(numAssocSites)
    real :: P

    T = eos%assoc%state%T
    V = eos%assoc%state%V
    n = eos%assoc%state%n

    ! Calculate X_V.
    call X_derivatives_knowing_X (eos,nc=nc,X=X_k,X_V=X_V)

    ! Efficient calculation of P_V.
    call nonassoc_pressure(nc,eos,T,V,n,P,dPdV=P_V)
    call Q_derivatives_knowing_X(eos,nc,X_k,Q_VV=Q_VV,Q_XV=Q_XV,X_calculated=.true.)
    P_V = P_V - Rgas*T*(Q_VV + dot_product(Q_XV,X_V))
  end subroutine compute_dXdV_and_dPdV


  !> Routine useful when fitting binary interaction parameters.
  subroutine pc_saft_get_kij(i,j,kij)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    integer, intent(in) :: i,j !< Component indices.
    real, intent(out) :: kij    !< Binary interaction parameter.
    class(base_eos_param), pointer :: eos

    if (i == j) then
      kij = 0.0
      return
    end if

    eos => get_active_eos()
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
      kij = 1 - p_eos%eps_depth_divk(i,j)/sqrt(p_eos%eps_depth_divk(i,i)*&
           p_eos%eps_depth_divk(j,j))
    class default
      call stoperror("pc_saft_get_kij: Wrong type.")
    end select

  end subroutine pc_saft_get_kij


  !> Routine useful when fitting binary interaction parameters.
  subroutine pc_saft_set_kij(i,j,kij)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    integer, intent(in) :: i,j !< Component indices.
    real, intent(in) :: kij    !< Binary interaction parameter.
    class(base_eos_param), pointer :: eos

    if (i == j) then
      call stoperror("Trying to set interaction parameter between a component and itself!")
    end if

    eos => get_active_eos()
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
      ! eps_depth_div is the only pc-saft parameter with an interaction parameter kij.
      p_eos%eps_depth_divk(i,j) = sqrt(p_eos%eps_depth_divk(i,i)*p_eos%eps_depth_divk(j,j))*(1-kij)
      p_eos%eps_depth_divk(j,i) = p_eos%eps_depth_divk(i,j)
    class default
      call stoperror("pc_saft_set_kij: Wrong type.")
    end select

  end subroutine pc_saft_set_kij

  !> Routine useful when fitting binary interaction parameters. For the cases when kij/=kji.
  subroutine pc_saft_set_kij_asym(i,j,kij)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    integer, intent(in) :: i,j !< Component indices.
    real, intent(in) :: kij    !< Binary interaction parameter.
    class(base_eos_param), pointer :: eos

    if (i == j) then
      call stoperror("Trying to set interaction parameter between a component and itself!")
    end if

    eos => get_active_eos()
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
      ! eps_depth_div is the only pc-saft parameter with an interaction parameter kij.
      p_eos%eps_depth_divk(i,j) = sqrt(p_eos%eps_depth_divk(i,i)*p_eos%eps_depth_divk(j,j))*(1-kij)
    class default
      call stoperror("pc_saft_set_kij_asym: Wrong type.")
    end select

  end subroutine pc_saft_set_kij_asym


  !> Routine useful when fitting binary interaction parameters.
  subroutine cpa_get_kij(i,j,aEps_kij_out)
    use saft_association, only: compidx_to_sites
    use AssocSchemeUtils
    use cpa_parameters, only: getCpaKij_epsBeta
    use thermopack_var, only: get_active_thermo_model, thermo_model
    use cubic_eos, only: cb_eos
    integer, intent(in) :: i,j !< Component indices.
    real, intent(out) :: aEps_kij_out(2) !< Binary interaction parameters.
    ! Locals
    real :: eps_i, eps_j, beta_i, beta_j
    integer :: k,l,k_first,k_last,l_first,l_last
    real :: denominator, dummy(2)
    integer :: scheme_i, scheme_j
    integer :: epsBetaCombRules(2)
    logical :: found
    type(thermo_model), pointer :: act_mod_ptr
    type(association), pointer :: p_assoc
    act_mod_ptr => get_active_thermo_model()
    p_assoc => act_mod_ptr%eos(1)%p_eos%assoc

    aEps_kij_out = -1e10 ! Make sure things crash if this function fails.

    if (i == j) then
       aEps_kij_out = 0.0
       return
    end if

    ! Call this routine to get the combining rules for eps and beta.
    call getCPAkij_epsbeta (p_assoc%saft_model,act_mod_ptr%comps(i)%p_comp%ident,&
         act_mod_ptr%comps(j)%p_comp%ident,param_ref="DEFAULT",&
         found=found,epsBetaCombRules=epsBetaCombRules, kijepsbeta=dummy)

    ! Get cubic interaction parameter.
    select type ( p_eos => act_mod_ptr%eos(1)%p_eos )
    class is ( cb_eos )
      aEps_kij_out(1) = p_eos%kij(i,j)
    class default
      aEps_kij_out(1) = 0
      print *,"Not able to calculate assoc_covol. Not cubic."
      stop
    end select

    scheme_i = act_mod_ptr%comps(i)%p_comp%assoc_scheme
    scheme_j = act_mod_ptr%comps(j)%p_comp%assoc_scheme
    if ((scheme_i /= no_assoc) .and. (scheme_j /= no_assoc)) then
      ! Get the values of eps and beta for components i and j.
      call getActiveAssocParams(i, eps_i, beta_i)
      call getActiveAssocParams(j, eps_j, beta_j)

      call compidx_to_sites(p_assoc,i,k_first,k_last)
      call compidx_to_sites(p_assoc,j,l_first,l_last)
      do k=k_first,k_last
        do l=l_first,l_last
          if (cross_site_interaction (site1=k-k_first+1,site2=l-l_first+1,&
               assoc_scheme_I=scheme_i, assoc_scheme_II=scheme_j) ) then

            denominator = applyCombiningRule(epsBetaCombRules(1), eps_i, eps_j)
            aEps_kij_out(2) = 1 - p_assoc%eps_kl(k,l)/denominator
            return
          end if
        end do
      end do
    else
      aEps_kij_out(2) = 0
    endif

  end subroutine cpa_get_kij


  !> Routine useful when fitting binary interaction parameters.
  subroutine cpa_set_kij(i,j,kij_a,kij_eps)
    use saft_association, only: compidx_to_sites
    use AssocSchemeUtils
    use cpa_parameters, only: getCpaKijAndCombRules_allComps
    use cubic_eos, only: cb_eos
    integer, intent(in) :: i,j !< Component indices.
    real, intent(in) :: kij_a !< Cubic interaction parameter
    real, intent(in) :: kij_eps !< Association interaction parameter 
    ! Locals
    real :: eps_i, eps_j, beta_i, beta_j
    integer :: k,l,k_first,k_last,l_first,l_last
    real :: dummy(3,nce,nce)
    integer :: scheme_i, scheme_j, epsbeta_combrules(2,nce,nce)
    type(thermo_model), pointer :: act_mod_ptr
    type(association), pointer :: p_assoc
    act_mod_ptr => get_active_thermo_model()
    p_assoc => act_mod_ptr%eos(1)%p_eos%assoc

    if (i == j) then
       call stoperror("Trying to set interaction parameter between a component and itself!")
    end if

    ! Call this routine to get the combining rules for eps and beta.
    call getCpaKijAndCombRules_allComps(nce,act_mod_ptr%comps,p_assoc%saft_model,dummy,&
         epsbeta_combrules)

    ! Set cubic interaction parameter.
    select type ( p_eos => act_mod_ptr%eos(1)%p_eos )
    class is ( cb_eos )
      p_eos%kij(i,j) = kij_a
    class default
      call stoperror("Not able to set cubic interaction parameter. Eos not cubic.")
    end select

    scheme_i = act_mod_ptr%comps(i)%p_comp%assoc_scheme
    scheme_j = act_mod_ptr%comps(j)%p_comp%assoc_scheme
    if ((scheme_i /= no_assoc) .and. (scheme_j /= no_assoc)) then
      ! Get the values of eps and beta for components i and j.
      call getActiveAssocParams(i, eps_i, beta_i)
      call getActiveAssocParams(j, eps_j, beta_j)

      call compidx_to_sites(p_assoc,i,k_first,k_last)
      call compidx_to_sites(p_assoc,j,l_first,l_last)
      do k=k_first,k_last
        do l=l_first,l_last
          if (cross_site_interaction (site1=k-k_first+1,site2=l-l_first+1,&
               assoc_scheme_I=scheme_i, assoc_scheme_II=scheme_j) ) then
             p_assoc%eps_kl(k,l) = applyCombiningRule(epsbeta_combrules(1,i,j), &
                  eps_i, eps_j) * (1-kij_eps)
          end if
        end do
      end do
    endif
  end subroutine cpa_set_kij

  !> Input a0, b in their conventional (non-SI) units,
  !> beta and eps in SI units, c1 dimensionless.
  subroutine cpa_set_pure_params(ic,params)
    use cubic_eos, only: cb_eos
    integer, intent(in) :: ic
    real, intent(in) :: params(5) !< a0, b, beta, eps, c1
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    select type ( p_eos => act_mod_ptr%eos(1)%p_eos )
    class is ( cb_eos )
      p_eos%single(ic)%a = params(1) !< Attraction constant a0. [a0] = Pa*L^2/mol^2.
      p_eos%single(ic)%b = params(2) !< Covolume b. [b] = L/mol.
      call setActiveAssocParams(ic, eps=params(3), beta=params(4))
      p_eos%single(ic)%alphaParams(1) = params(5)
    class default
      call stoperror("cpa_set_pure_params: Not able to set pure cpa parameters. Eos not cubic.")
    end select
  end subroutine cpa_set_pure_params


  subroutine cpa_get_pure_params(ic,params)
    use cubic_eos, only: cb_eos
    integer, intent(in) :: ic
    real, intent(out) :: params(5) !< a0, b, beta, eps, c1
    type(thermo_model), pointer :: act_mod_ptr
    act_mod_ptr => get_active_thermo_model()

    select type ( p_eos => act_mod_ptr%eos(1)%p_eos )
    class is ( cb_eos )
      params(1) = p_eos%single(ic)%a !< Attraction constant a0. [a0] = Pa*L^2/mol^2.
      params(2) = p_eos%single(ic)%b !< Covolume b. [b] = L/mol.
      call getActiveAssocParams(ic, eps=params(3), beta=params(4))
      params(5) = p_eos%single(ic)%alphaParams(1)
    class default
      call stoperror("cpa_get_pure_params: Not able to set pure cpa parameters. Eos not cubic.")
    end select

  end subroutine cpa_get_pure_params

  subroutine pc_saft_set_pure_params(ic,params)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use thermopack_var, only: nce
    use saft_association, only: numAssocSites
    integer, intent(in) :: ic
    real, intent(in) :: params(5) ! m, sigma/m, eps_depth_divk/K, eps/(J/mol), beta
    real :: m(nce),sigma(nce),eps_depth_divk(nce), kij(nce,nce)
    integer :: j
    class(base_eos_param), pointer :: eos

    eos => get_active_eos()
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
      do j=1,nce
        m(j) = p_eos%m(j)
        sigma(j) = p_eos%sigma(j,j)
        eps_depth_divk(j) = p_eos%eps_depth_divk(j,j)
      end do
      m(ic) = params(1) !< Chain length (-)
      sigma(ic) = params(2) !< Monomer diameter (m)
      eps_depth_divk(ic) = params(3) !< Dispersion energy scale (K)

      ! This routine must be called to ensure binary parameters such as
      ! sigma(i,j) are also updated correctly
      kij = 0.0
      call pcsaft_set_nonassoc_params(p_eos,nce,m,sigma,eps_depth_divk,kij_in=kij, allocate=.false.)
    class default
      call stoperror("pc_saft_set_pure_params: Wrong type.")
    end select

    if (associated(eos%assoc)) then
      if (eos%assoc%numAssocSites > 0) &
           call setActiveAssocParams(ic, eps=params(4), beta=params(5))
    endif
  end subroutine pc_saft_set_pure_params

  subroutine pc_saft_get_pure_params(ic,params)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    integer, intent(in) :: ic
    real, intent(out) :: params(5) ! m, sigma/m, eps_depth_divk/K, eps/(J/mol), beta
    class(base_eos_param), pointer :: eos

    eos => get_active_eos()
    select type ( p_eos => eos )
    class is ( sPCSAFT_eos )
        params(1) = p_eos%m(ic) !< Attraction constant a0. [a0] = Pa*L^2/mol^2.
        params(2) = p_eos%sigma(ic,ic) !< Covolume b. [b] = L/mol.
        params(3) = p_eos%eps_depth_divk(ic,ic) !< Constant used instead of m(omega) in classic alpha formulation. [c] = -.
    class default
      call stoperror("pc_saft_get_pure_params: Wrong type.")
    end select

    call getActiveAssocParams(ic, eps=params(4), beta=params(5))

  end subroutine pc_saft_get_pure_params

  subroutine pets_set_pure_params(ic,params)
    use pets, only: PETS_eos
    integer, intent(in) :: ic
    real, intent(in) :: params(2) ! sigma/m, epsdivk/K
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()

    select type( p_eos => eos )
    class is ( PETS_eos )
      p_eos%sigma_pets = params(1)
      p_eos%epsdivk_pets = params(2)
    end select
  end subroutine pets_set_pure_params

  subroutine pets_get_pure_params(ic,params)
    use pets, only: PETS_eos
    integer, intent(in) :: ic
    real, intent(out) :: params(2) ! sigma/m, epsdivk/K
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()

    select type( p_eos => eos )
    class is ( PETS_eos )
      params(1) = p_eos%sigma_pets
      params(2) = p_eos%epsdivk_pets
    end select
  end subroutine pets_get_pure_params

  ! Returns eps=-1.0, beta=-1.0 if component ic is not self-associating.
  subroutine getActiveAssocParams(ic, eps, beta)
    use saft_association, only: compidx_to_sites, noSitesFlag
    integer, intent(in) :: ic
    real, intent(out) :: eps, beta
    ! Locals
    integer :: k, l, firstSiteIdx, lastSiteIdx
    type(thermo_model), pointer :: act_mod_ptr
    type(association), pointer :: assoc

    act_mod_ptr => get_active_thermo_model()
    assoc => act_mod_ptr%eos(1)%p_eos%assoc

    call compidx_to_sites(assoc, ic, firstSiteIdx, lastSiteIdx)

    if ( firstSiteIdx == noSitesFlag  ) then
       eps = -1.0
       beta = -1.0
       return
    end if
      
    do k=firstSiteIdx, lastSiteIdx
       do l=firstSiteIdx, lastSiteIdx
          if (abs(assoc%eps_kl(k,l)) > 0.0) then
             eps = assoc%eps_kl(k,l)
             beta = assoc%beta_kl(k,l)
             return
          end if
       end do
    end do

  end subroutine getActiveAssocParams

  subroutine setActiveAssocParams(ic, eps, beta)
    use saft_association, only: compidx_to_sites, noSitesFlag
    integer, intent(in) :: ic
    real, intent(in) :: eps, beta
    ! Locals
    integer :: k, l, firstSiteIdx, lastSiteIdx
    type(thermo_model), pointer :: act_mod_ptr
    type(association), pointer :: assoc
    
    act_mod_ptr => get_active_thermo_model()
    assoc => act_mod_ptr%eos(1)%p_eos%assoc

    call compidx_to_sites(assoc,ic,firstSiteIdx, lastSiteIdx)
    if ( firstSiteIdx == noSitesFlag  .and. (eps>0.0 .or. beta>0.0) ) then
       call stoperror("Trying to set association parameters for non-associating component.")
    end if
    do k=firstSiteIdx, lastSiteIdx
       do l=firstSiteIdx, lastSiteIdx
          if (abs(assoc%eps_kl(k,l)) > 0.0) then
             assoc%eps_kl(k,l) = eps
             assoc%beta_kl(k,l) = beta
             !print *, assoc%eps_kl(k,l)
             !print *, assoc%beta_kl(k,l)
          end if
       end do
    end do

  end subroutine setActiveAssocParams

  ! subroutine setActiveAssocParams(ic, eps, beta)
  !   use saft_association, only: compidx_to_sites, noSitesFlag
  !   integer, intent(in) :: ic
  !   real, intent(in) :: eps, beta
  !   ! Locals
  !   integer :: k, l, firstSiteIdx, lastSiteIdx
  !   type(thermo_model), pointer :: act_mod_ptr
  !   type(association), pointer :: assoc

  !   act_mod_ptr => get_active_thermo_model()
  !   assoc => act_mod_ptr%eos(1)%p_eos%assoc

  !   call compidx_to_sites(assoc,ic,firstSiteIdx, lastSiteIdx)
  !   if ( firstSiteIdx == noSitesFlag  .and. (eps>0.0 .or. beta>0.0) ) then
  !      call stoperror("Trying to set association parameters for non-associating component.")
  !   end if

  !   do k=firstSiteIdx, lastSiteIdx
  !      do l=firstSiteIdx, lastSiteIdx
  !         if (abs(assoc%eps_kl(k,l)) > 1e-100 .and. abs(assoc%beta_kl(k,l)) > 1e-100) then
  !            assoc%eps_kl(k,l) = eps
  !            assoc%beta_kl(k,l) = beta
  !         end if
  !         if (k<l) then
  !           print *, "eps_assoc, beta_assoc", k, l, assoc%eps_kl(k,l), assoc%beta_kl(k,l)
  !         end if
  !      end do
  !   end do

  ! end subroutine setActiveAssocParams


  subroutine print_cpa_report()
    use cpa_parameters, only: getCPAkij_epsbeta
    use saft_globals
    use eosdata, only: get_eos_short_label_from_subidx
    use thermopack_var, only: nce
    use AssocSchemeUtils, only: get_assoc_string

    integer :: i
    real :: pure_params(5), cpa_aEps_kij(2)
    type(thermo_model), pointer :: act_mod_ptr
    type(association), pointer :: assoc
    act_mod_ptr => get_active_thermo_model()
    assoc => act_mod_ptr%eos(1)%p_eos%assoc

    write(*, '(A)') repeat("=", 40)
    print *, "CPA Parameters"
    write(*, '(A)') repeat("=", 40)
    print *, "Model: ", get_eos_short_label_from_subidx(assoc%saft_model)
    print *, " "
    do i=1,nce
      call cpa_get_pure_params(ic=i,params=pure_params)
      print *,"Component:", act_mod_ptr%comps(i)%p_comp%ident
      print *,"Scheme:", get_assoc_string(act_mod_ptr%comps(i)%p_comp%assoc_scheme)
      print *,"Parameters:"
      write(*, '(A,F10.1)') "  a0 (Pa*L^2/mol^2) =", pure_params(1)
      write(*, '(A,E10.3)') "  b (L/mol)         =", pure_params(2)
      write(*, '(A,F10.3)') "  eps (J/mol)       =", pure_params(3)
      write(*, '(A,E10.3)') "  beta (-)          =", pure_params(4)
      write(*, '(A,E10.3)') "  c1 (-)            =", pure_params(5)
      print *," "
    enddo

    call cpa_get_kij(1,2,cpa_aEps_kij)
    print *, "Binary parameters:"
    write(*, '(A,E10.3)') "  kij_a = ", cpa_aEps_kij(1)
    write(*, '(A,E10.3)') "  kij_eps = ", cpa_aEps_kij(2)
    write(*, '(A)') repeat("=", 40)

  end subroutine print_cpa_report

  subroutine printBinaryMixtureReportSaft()
    use cpa_parameters, only: getCPAkij_epsbeta
    use saft_globals
    use thermopack_constants, only: verbose
    use thermopack_var, only: nce
    integer :: rules(2)
    real :: params1(5), params2(5)
    real :: pcSaft_kij, cpa_aEps_kij(2), cpa_kijepsbeta_db(2)
    logical :: found
    type(thermo_model), pointer :: act_mod_ptr
    type(association), pointer :: assoc
    act_mod_ptr => get_active_thermo_model()
    assoc => act_mod_ptr%eos(1)%p_eos%assoc

    if ( assoc%saft_model == eosOPC_SAFT .OR. &
         assoc%saft_model == eosSPC_SAFT .OR. &
         assoc%saft_model == eosPCP_SAFT .OR. &
         assoc%saft_model == eosSPCP_SAFT) then
       print *, "Model: PC-SAFT"
       call pc_saft_get_pure_params(ic=1,params=params1)
       if (nce==2) call pc_saft_get_pure_params(ic=2,params=params2)
    else
       call cpa_get_pure_params(ic=1,params=params1)
       if (nce==2) call cpa_get_pure_params(ic=2,params=params2)

       if (assoc%saft_model == cpaSRK) then
          if (verbose) print *, "Model: CPA-SRK"
       elseif (assoc%saft_model == cpaPR) then
          if (verbose) print *, "Model: CPA-PR"
       end if
    end if

    if ( nce == 1 ) then
          print *,"Component:", act_mod_ptr%comps(1)%p_comp%ident
          print *,"Association scheme:",act_mod_ptr%comps(1)%p_comp%assoc_scheme
        write(*,'(A, 5ES11.3)') "Component 1 pure params:", params1
    else if ( nce == 2 ) then
         print *,"Component 1, scheme:", act_mod_ptr%comps(1)%p_comp%ident, &
              act_mod_ptr%comps(1)%p_comp%assoc_scheme
         print *,"Component 2, scheme:", act_mod_ptr%comps(2)%p_comp%ident, &
              act_mod_ptr%comps(2)%p_comp%assoc_scheme
          write(*,'(A, 5ES11.3)') "Component 1 pure params:", params1
          write(*,'(A, 5ES11.3)') "Component 2 pure params:", params2
       if ( assoc%saft_model == eosOPC_SAFT .OR. &
            assoc%saft_model == eosSPC_SAFT .OR. &
            assoc%saft_model == eosPCP_SAFT .OR. &
            assoc%saft_model == eosSPCP_SAFT) then
          call pc_saft_get_kij(1,2,pcSaft_kij)
          write(*,'(A, ES11.3)') "kij", pcSaft_kij
       else
          call cpa_get_kij(1,2,cpa_aEps_kij)
          write(*,'(A, 2ES11.3)') "kij a_cubic, eps_assoc: ", cpa_aEps_kij
       end if

       if ( assoc%saft_model /= eosOPC_SAFT .AND. &
            assoc%saft_model /= eosSPC_SAFT .AND. &
            assoc%saft_model /= eosPCP_SAFT .AND. &
            assoc%saft_model /= eosSPCP_SAFT) then
         call getCPAkij_epsbeta (eosidx=assoc%saft_model,&
              uid1=act_mod_ptr%comps(1)%p_comp%ident,&
              uid2=act_mod_ptr%comps(2)%p_comp%ident,&
              param_ref="DEFAULT",found=found,epsBetaCombRules=rules,&
              kijepsbeta=cpa_kijepsbeta_db)
          print *, "Eps/beta combining rules:", rules
       end if

    end if

  end subroutine printBinaryMixtureReportSaft

  !> Lets the user choose whether to use the simplified or the original
  !> formulation of CPA.
  subroutine setCPAformulation(simplified)
    use saft_rdf, only: useSimplifiedCPA
    logical, intent(in) :: simplified
    useSimplifiedCPA = simplified
  end subroutine setCPAformulation

  !> Estimate critical parameters based on reduced values
  subroutine estimate_critical_parameters(i, Tc, vc)
    use pc_saft_nonassoc, only: PCSAFT_eos
    use saftvrmie_containers, only: saftvrmie_eos
    use lj_splined, only: ljs_bh_eos, ljs_wca_eos
    use pets, only: PETS_eos
    use thermopack_constants, only: N_Avogadro
    integer, intent(in) :: i
    real, intent(out) :: Tc, vc
    ! Locals
    real :: rhos, Ts
    real :: sigma, eps, alpha
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()

    select type ( p_eos => eos )
    class is ( PCSAFT_eos )
      ! Treat as LJ fluid
      rhos = 0.312
      Ts = 1.312
      !Ps = 0.119
      sigma = p_eos%sigma(i,i)
      eps = p_eos%eps_depth_divk(i,i)
    class is ( saftvrmie_eos )
      alpha = p_eos%saftvrmie_param%alpha_ij(i,i)
      sigma = p_eos%saftvrmie_param%sigma_ij(i,i)
      eps = p_eos%saftvrmie_param%eps_divk_ij(i,i)
      Ts = 1.173*alpha + 0.254 ! Ramrattan 2015 correlation (10/ggxvnw)
      rhos = min(max(-0.0813*alpha + 0.3884,0.3), 0.4)
      !Ps = -1.0
    class is ( ljs_bh_eos )
      rhos = 0.333
      Ts = 0.885
      !Ps = 0.075
      sigma = p_eos%saftvrmie_param%sigma_ij(i,i)
      eps = p_eos%saftvrmie_param%eps_divk_ij(i,i)
    class is( ljs_wca_eos )
      rhos = 0.333
      Ts = 0.885
      !Ps = 0.075
      sigma = p_eos%sigma
      eps = p_eos%eps_divk
    class is ( PETS_eos )
      Ts = 1.086
      rhos = 0.319
      !Ps = -1.0
      sigma = p_eos%sigma_pets
      eps = p_eos%epsdivk_pets
    class default
      call stoperror("estimate_critical_parameters: Wrong eos.")
    end select

    Tc = Ts*eps
    vc = (sigma**3*N_Avogadro)/rhos
    !Pc =
  end subroutine estimate_critical_parameters

  !> Calculates the reduced association Helmholtz energy density
  !! together with its derivatives.
  !! FMT interface
  subroutine calc_assoc_phi(n_fmt,T,F,F_T,F_n,F_TT,F_Tn,F_nn)
    use hyperdual_mod
    use thermopack_var, only: nce, base_eos_param, get_active_eos
    use numconstants, only: machine_prec ! Equals 2^{-52} ~ 2.22*e-16 for double precision reals.
    use saft_association, only: numAssocSites, solve_for_X_k, &
         calcFder_assoc
    use saft_association, only: numAssocSites, solve_for_X_k, calcFder_assoc, Q_fmt_hd
    use eos_parameters, only: base_eos_param
    use pc_saft_nonassoc, only: PCSAFT_eos
    ! Input.
    real, intent(in) :: n_fmt(nce,0:5)
    real, intent(in) :: T
    ! Output.
    real, optional, intent(out) :: F,F_T,F_n(nce,0:5)
    real, optional, intent(out) :: F_TT,F_Tn(nce,0:5),F_nn(nce,nce,0:5,0:5)
    ! Locals.
    class(base_eos_param), pointer :: eos
    real :: ms(nce)
    real, dimension(numAssocSites) :: X_k
    type(hyperdual) :: T_hd,n_fmt_hd(nce,0:5), Q
    logical :: F_T_calculated, F_n_calculated, F_calculated
    integer :: i, j, k, l
    F_calculated = .false.
    F_T_calculated = .false.
    F_n_calculated = .false.
    eos => get_active_eos()

    ! Initialize
    if (present(F)) F = 0
    if (present(F_T)) F_T = 0
    if (present(F_n)) F_n = 0
    if (present(F_TT)) F_TT = 0
    if (present(F_Tn)) F_Tn = 0
    if (present(F_nn)) F_nn = 0

    if (numAssocSites > 0) then
      if (sum(n_fmt(eos%assoc%compIdcs,0)) > 1e-20) then
        ! Set segment numbers
        select type ( p_eos => eos )
        class is(PCSAFT_eos)
          ms = p_eos%m
        class default
          call stoperror("calc_assoc_phi: Wrong eos...")
        end select

        ! Calculate the association contribution.
        call eos%assoc%state%init_fmt(nce, T, n_fmt, ms)
        X_k = 0.2 ! Initial guess.
        call solve_for_X_k(eos,nce,X_k,tol=10**5*machine_prec)
        T_hd = T
        n_fmt_hd = n_fmt

        if (present(F_TT)) then
          T_hd%f1 = 1.0_dp
          T_hd%f2 = 1.0_dp
          Q = Q_fmt_hd(eos,nce,T_hd,n_fmt_hd,X_k,n=2)
          F_TT = Q%f12
          if (present(F)) then
            F = Q%f0
            F_calculated = .true.
          endif
          if (present(F_T)) then
            F_T = Q%f1
            F_T_calculated = .true.
          endif
          T_hd%f1 = 0.0_dp
          T_hd%f2 = 0.0_dp
        endif

        if (present(F_Tn)) then
          T_hd%f1 = 1.0_dp
          do i=1,nce
            do j=0,5
              n_fmt_hd(i,j)%f2 = 1.0_dp
              Q = Q_fmt_hd(eos,nce,T_hd,n_fmt_hd,X_k,n=2)
              F_Tn(i,j) = Q%f12
              n_fmt_hd(i,j)%f2 = 0.0_dp
            enddo
          enddo
          T_hd%f1 = 0.0_dp
          if (present(F_T)) then
            F_T = Q%f1
            F_T_calculated = .true.
          endif
          if (present(F)) then
            F = Q%f0
            F_calculated = .true.
          endif
        endif

        if (present(F_nn)) then
          ! Loop upper triangle
          do i=1,nce
            do j=0,5
              n_fmt_hd(i,j)%f1 = 1.0_dp
              do k=i,nce
                do l=j,5
                  n_fmt_hd(k,l)%f2 = 1.0_dp
                  Q = Q_fmt_hd(eos,nce,T_hd,n_fmt_hd,X_k,n=2)
                  F_nn(i,k,j,l) = Q%f12
                  F_nn(k,i,l,j) = F_nn(i,k,j,l)
                  n_fmt_hd(k,l)%f2 = 0.0_dp
                enddo
              enddo
              n_fmt_hd(i,j)%f1 = 0.0_dp
              if (present(F_n)) then
                F_n(i,j) = Q%f1
                F_n_calculated = .true.
              endif
            enddo
          enddo
          if (present(F)) then
            F = Q%f0
            F_calculated = .true.
          endif
        endif

        if (present(F_T) .and. .not. F_T_calculated) then
          T_hd%f1 = 1.0_dp
          Q = Q_fmt_hd(eos,nce,T_hd,n_fmt_hd,X_k,n=1)
          if (present(F_T)) then
            F_T = Q%f1
            F_T_calculated = .true.
          endif
          T_hd%f1 = 0.0_dp
          if (present(F)) then
            F = Q%f0
            F_calculated = .true.
          endif
        endif

        if (present(F_n) .and. .not. F_n_calculated) then
          do i=1,nce
            do j=0,5
              n_fmt_hd(i,j)%f1 = 1.0_dp
              Q = Q_fmt_hd(eos,nce,T_hd,n_fmt_hd,X_k,n=1)
              F_n(i,j) = Q%f1
              n_fmt_hd(i,j)%f1 = 0.0_dp
            enddo
          enddo
          if (present(F)) then
            F = Q%f0
            F_calculated = .true.
          endif
        endif

        if (present(F) .and. .not. F_calculated) then
          Q = Q_fmt_hd(eos,nce,T_hd,n_fmt_hd,X_k,n=0)
          F = Q%f0
        endif
      endif
    endif
  end subroutine calc_assoc_phi

  !> Test calc_assoc_phi
  subroutine test_calc_assoc_phi()
    use iso_fortran_env, only: dp => REAL64
    use thermopack_var, only: nce
    use pc_saft_nonassoc, only: PCSAFT_eos, calc_dhs
    use saft_association, only: solve_for_X_k, calcFder_assoc
    use numconstants, only: machine_prec
    ! Locals
    real :: n_fmt(nce,0:5), n(1), n_fmt_0(nce,0:5)
    real :: T, V
    real :: F, eps, dn
    real :: F_T, F_V, F_n(nce,0:5)
    real :: F_TT, F_TV, F_VV, F_Tn(nce,0:5), F_Vn(nce,0:5), F_nn(nce,nce,0:5,0:5)
    real :: F1,F1_T,F1_V,F1_n(nce,0:5)
    real :: F2,F2_T,F2_V,F2_n(nce,0:5)
    real :: Xk(numAssocSites)
    integer :: i, j
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    T = 280.0
    V = 1.0e-4
    n = 1
    print *,"Evaluating Fres:"
    call calcSaftFder_res(nce,eos,T,V,n,F,Xk=Xk)
    call eos%assoc%state%init(nce,T,V,n)
    Xk = 0.2 ! Initial guess.
    call solve_for_X_k(eos,nce,Xk,tol=10**5*machine_prec)
    call calcFder_assoc(eos,nc=nce,X_k=Xk,F=F,F_T=F_T,F_V=F_V,F_n=F_n,&
         F_TT=F_TT,F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tn,F_Vn=F_Vn,F_nn=F_nn)
    eps = 1.0e-5*T
    call eos%assoc%state%init(nce,T-eps,V,n)
    Xk = 0.2 ! Initial guess.
    call solve_for_X_k(eos,nce,Xk,tol=10**5*machine_prec)
    call calcFder_assoc(eos,nc=nce,X_k=Xk,F=F1,F_T=F1_T,F_V=F1_V,F_n=F1_n)
    call eos%assoc%state%init(nce,T+eps,V,n)
    Xk = 0.2 ! Initial guess.
    call solve_for_X_k(eos,nce,Xk,tol=10**5*machine_prec)
    call calcFder_assoc(eos,nc=nce,X_k=Xk,F=F2,F_T=F2_T,F_V=F2_V,F_n=F2_n)
    print *,"F",F*V, F1*V, F2*V
    print *,"F_T",F_T, (F2-F1)/(2*eps)
    print *,"F_TT",F_TT, (F2_T-F1_T)/(2*eps)
    !
    ! Note that n_fmt = n_fmt(T,V,n)!
    call set_fmt_densities(T, V, n, n_fmt)
    n_fmt(:,5) = n_fmt(:,2)*1e-3
    !call calc_assoc_phi(n_fmt,T,F,F_T=F_T,F_n=F_n,F_TT=F_TT,F_Tn=F_Tn,F_nn=F_nn)
    call calc_assoc_phi(n_fmt,T,F,F_T=F_T,F_n=F_n,F_TT=F_TT,F_Tn=F_Tn,F_nn=F_nn)
    !
    ! Temperature differentials
    !call set_fmt_densities(T-eps, V, n, n_fmt)
    call calc_assoc_phi(n_fmt,T-eps,F1,F_n=F1_n)
    !call set_fmt_densities(T+eps, V, n, n_fmt)
    call calc_assoc_phi(n_fmt,T+eps,F2,F_n=F2_n)
    print *,"F",F*V, F1*V, F2*V
    print *,"F_T",F_T*V, (F2-F1)/(2*eps)*V
    print *,"F_TT",F_TT*V, (F2+F1-2*F)/(eps)**2*V
    print *,"F_Tn",F_Tn*V
    print *,"F_Tn",(F2_n-F1_n)/(2*eps)*V
    !
    ! n - differentials
    n_fmt_0 = n_fmt
    do i=1,nce
      do j=0,5
        dn = n_fmt_0(i,j)*1.0e-5
        if (dn > 1.0e-10) then
          n_fmt(i,j) = n_fmt_0(i,j) - dn
          !call set_fmt_densities(T, V, n, n_fmt)
          call calc_assoc_phi(n_fmt,T,F1,F_T=F1_T,F_n=F1_n)
          n_fmt(i,j) = n_fmt_0(i,j) + dn
          !call set_fmt_densities(T, V, n, n_fmt)
          call calc_assoc_phi(n_fmt,T,F2,F_T=F2_T,F_n=F2_n)
          print *,"w",j
          print *,"F_n",F_n(i,j)*V, (F2-F1)/(2*dn)*V
          print *,"F_nn",F_nn(i,:,j,:)*V
          print *,"F_nn",(F2_n-F1_n)/(2*dn)*V
          n_fmt = n_fmt_0
        endif
      enddo
    enddo

  end subroutine test_calc_assoc_phi

  subroutine set_fmt_densities(T, V, n, n_fmt)
    use thermopack_var, only: nce
    use pc_saft_nonassoc, only: PCSAFT_eos, calc_dhs
    use thermopack_constants, only: N_AVOGADRO
    use numconstants, only: pi
    real, intent(out) :: n_fmt(nce,0:5)
    real, intent(in) :: T, V, n(nce)
    ! Locls
    real :: ms(nce)
    real :: R(nce)
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    select type ( p_eos => eos )
    class is(PCSAFT_eos)
      ms = p_eos%m
      call calc_dhs(p_eos, T)
      R = 0.5*p_eos%dhs%d
    class default
      call stoperror("set_fmt_densities: Wrong eos...")
    end select
    !
    n_fmt = 0
    n_fmt(:,0) = ms(:)/V
    n_fmt(:,2) = 4*pi*R(:)**2*n_fmt(:,0)*N_AVOGADRO
    n_fmt(:,3) = 4*pi*R(:)**3*n_fmt(:,0)*N_AVOGADRO/3
  end subroutine set_fmt_densities

end module saft_interface
