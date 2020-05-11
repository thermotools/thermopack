subroutine init(eosLib,eos,mixing,alpha,ncomp,comp_string,nphases,&
     kij_setno,alpha_setno)
  ! Initialize, with given EoS-lib (Thermopack, TREND, ...), EoS,
  ! components, etc...
  use eoslibinit, only: init_thermo

  implicit none
  character(len=*), intent(in)  :: eosLib       !< String defining eos library
  character(len=*), intent(in)  :: eos          !< String defining equation of state
  character(len=*), intent(in)  :: mixing       !< String defining mixing rules
  character(len=*), intent(in)  :: alpha        !< String defining alpha correlation
  integer, intent(in)           :: ncomp        !< Number of components
  character(len=*), intent(in)  :: comp_string  !< String defining componets. Comma separated.
  integer, intent(in)           :: nphases      !< Number of phases
  integer, intent(in) :: kij_setno    !< Data set number for kij (default: 1)
  integer, intent(in) :: alpha_setno  !< Data set number for alpha (default: 1)

  call init_thermo(eosLib,eos,mixing,alpha,ncomp,comp_string,nphases,&
       kij_setno=kij_setno,alpha_setno=alpha_setno)

end subroutine init

subroutine init_solid(scomp)
! Initialize solid components, at moment only CO2
!Exampel of call
!    tp.init("Thermopack","SRK","Classic","Classic", 2, "CO2 N2", 3, 1, 1)
!     #   Mark that nph = 3: Gas, Liquid, Solid
!    tp.init_solid("CO2")  #Must be called if ne model is choosen
!    (nd, beta, iph, x)=tp.tpflash_multiphase(3, 216.22, 10E5, [0.9,0.1])
  use solideos, only: solid_init
  implicit none
  character(len=*), intent(in)::scomp  !<String with solid component

  call solid_init(scomp)

end subroutine init_solid

subroutine initcsp(eosLib,eos,mixing,alpha,ncomp,comp_string,nphases, &
    csp_eos,csp_ref)
  ! Initialize, with given EoS-lib - including the CSP method with reference equation and fluid
  ! components, etc...
  use eoslibinit, only: init_thermo
  use parameters, only: liq_vap_discr_method

  implicit none
  character(len=*), intent(in)  :: eosLib       !< String defining eos library
  character(len=*), intent(in)  :: eos          !< String defining equation of state
  character(len=*), intent(in)  :: mixing       !< String defining mixing rules
  character(len=*), intent(in)  :: alpha        !< String defining alpha correlation
  integer, intent(in)           :: ncomp        !< Number of components
  character(len=*), intent(in)  :: comp_string  !< String defining componets. Comma separated.
  integer, intent(in)           :: nphases      !< Number of phases
  character(len=*), intent(in)  :: csp_eos, csp_ref !< Corresponding state eos and ref fluid

!  write (*,*)  "Calling init_thermo with: "
!  write (*,*)  "eoslib:     ", eosLib
!  write (*,*)  "eos:        ", eos
!  write (*,*)  "eos_mixing: ", mixing
!  write (*,*)  "alpha:      ", alpha
!  write (*,*)  "ncomp:      ", ncomp
!  write (*,*)  "comp_string ", comp_string
!  write (*,*)  "nphase:     ", nphases
!  write (*,*)  "liqvapdisc  ", liq_vap_discr_method
!  write (*,*)  "csp_eos     ", csp_eos
!  write (*,*)  "csp_ref     ", csp_ref

  call init_thermo(eosLib,eos,mixing,alpha,ncomp,comp_string,nphases, &
       csp_eos=csp_eos,csp_ref_comp=csp_ref)

end subroutine initcsp

subroutine meos_alphaderivs(meos_name,T,v,alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv)
  ! Derivatives of pure-component multiparameter equation of state.
  use stringmod, only: str_eq
  use multiparameter_C3, only: meos_C3
  use multiparameter_ortho_h2, only: meos_ortho_h2
  use multiparameter_para_h2, only: meos_para_h2
  implicit none
  character(len=*), intent(in)  :: meos_name    !< String defining multiparametereos
  real, intent(in) :: T ! Temperature [K]
  real, intent(in) :: v ! Molar volume [m3/mol]
  real, optional, intent(out) :: alp !< A/(nRT)
  real, intent(out):: alp_T, alp_v, alp_TT, alp_Tv, alp_vv

  ! Internals
  type(meos_c3) :: m_C3
  type(meos_ortho_h2) :: m_orthoH2
  type(meos_para_h2) :: m_paraH2

  if ( str_eq(meos_name,"C3_NIST") ) then
    call m_C3%init()
    call m_C3%alphaDerivs_Tv(T,v,alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv)
  else if ( str_eq(meos_name,"ORTHO_H2_NIST") ) then
    call m_orthoH2%init()
    call m_orthoH2%alphaDerivs_Tv(T,v,alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv)
  else if ( str_eq(meos_name,"PARA_H2_NIST") ) then
    call m_paraH2%init()
    call m_paraH2%alphaDerivs_Tv(T,v,alp,alp_T,alp_v,alp_TT,alp_Tv,alp_vv)
  else
     call stoperror("Cannot call this multiparameter eos.")
  end if

end subroutine meos_alphaderivs

subroutine set_cpmethod(icomp, imethod)
  ! Change the Cp-method (ideal-gas heat capacity) for a component
  ! from the default.
  use parameters, only: nc, complist
  use tpvar, only: comp
  use tpselect, only: TP_CpMethod
  ! Input:
  integer,  intent(in)      :: icomp    !< Index of component to modify
  integer,  intent(in)      :: imethod  !< Index of cp-method (see TP_CPmethod)

  if (EoSlib == THERMOPACK) then
    if (icomp > nc) then
      write(*,*) "set_cpmethod: Cannot set. icomp > nc"
    endif
    write(*,*) "Changing the Cp-method of component: "//trim(complist(icomp))
    call TP_CpMethod(nc,comp,trim(complist(icomp)),imethod)
  else
    write(*,*) "set_cpmethod: Only for EoSlib Thermopack!"
  endif

end subroutine set_cpmethod

subroutine set_cp_polynom(icomp, cp, cptype)
  ! Change the ideal Cp (ideal-gas heat capacity) for a component
  ! from the default.
  use parameters, only: nc, EoSlib, THERMOPACK
  use tpvar, only: comp
  use tpselect, only: TP_CpMethod
  ! Input:
  integer,  intent(in)      :: icomp   !< Index of component to modify
  real,     intent(in)      :: cp(10)  !< Ideal cp paramaters (see TP_CPmethod)
  integer,  intent(in)      :: cptype  !< Expression type
  if (EoSlib == THERMOPACK) then
    if (icomp > nc) then
      write(*,*) "set_cp_polynom: Cannot set. icomp > nc"
    endif
    ! Set ideal gas Cp
    comp(icomp)%cptype = cptype
    comp(icomp)%cp(:) = cp
  else
    write(*,*) "set_cp_polynom: Only for EoSlib Thermopack!"
  endif
end subroutine set_cp_polynom

subroutine get_phase_flags(iTWOPH,iLIQPH,iVAPPH,iMINGIBBSPH,iSINGLEPH,iSOLIDPH,iFAKEPH)
  ! Get the internal integer flags for various phase states and
  ! specifications.
  use parameters, only: TWOPH,LIQPH,VAPPH,MINGIBBSPH,SINGLEPH,SOLIDPH,FAKEPH
  implicit none
  ! Output:
  integer, intent(out) :: iTWOPH,iLIQPH,iVAPPH,iMINGIBBSPH,iSINGLEPH,iSOLIDPH,iFAKEPH
  iTWOPH      = TWOPH
  iLIQPH      = LIQPH
  iVAPPH      = VAPPH
  iMINGIBBSPH = MINGIBBSPH
  iSINGLEPH   = SINGLEPH
  iSOLIDPH    = SOLIDPH
  iFAKEPH     = FAKEPH

end subroutine get_phase_flags

subroutine get_gas_constant(r)
  ! Get the gas constant used by thermopack
  use tpconst, only: rgas
  implicit none
  real, intent(out) :: r ! Gas constant (J/mol K)
  r = rgas
end subroutine get_gas_constant

subroutine TPflash_twophase(nc,T,P,z,beta_init,beta,betaL,phase,X,Y)
  ! Get the thermodynamic state of a system of total composition z at
  ! conditions T,P.
  !
  ! This routine is only for single- and two-phase (liquid-vapor)
  ! states, and should not be used if more than two phases may be expected.
  !
  use tp_solver, only: twophaseTPflash
  implicit none
  ! Input:
  integer,             intent(in)     :: nc       !< Number of components
  real,                intent(in)     :: T        !< Temperature [K]
  real,                intent(in)     :: P        !< Pressure [Pa]
  real, dimension(nc), intent(in)     :: z        !< Overall molar compozition [-]
  real,                intent(in)     :: beta_init!< Vapour phase molar fraction (guess) [-]
  ! Output:
  real,                intent(out)    :: beta     !< Vapour phase molar fraction (solution) [-]
  real,                intent(out)    :: betaL    !< Liquid phase molar fraction [-]
  integer,             intent(out)    :: phase    !< Phase identefier
  real, dimension(nc), intent(out)    :: X        !< Liquid molar compozition [-]
  real, dimension(nc), intent(out)    :: Y        !< Vapour molar compozition [-]

  beta = beta_init
  call twophaseTPflash(T,P,z,beta,betaL,phase,X,Y)

end subroutine TPflash_twophase

subroutine PSflash_twophase(nc,P,s,Z,T_init,beta_init,T,beta,betaL,phase,X,Y)
  ! Get the thermodynamic state of a system of total composition z at
  ! conditions s,P.
  !
  use ps_solver, only:  twoPhasePSflash
  implicit none
  ! Input:
  integer,             intent(in)     :: nc       !< Number of components
  real,                intent(in)     :: P        !< Pressure [Pa]
  real,                intent(in)     :: s        !< Entropy [J/K mol]
  real, dimension(nc), intent(in)     :: Z        !< Overall molar compozition [-]
  real,                intent(in)     :: T_init   !< Temperature (guess) [K]
  real,                intent(in)     :: beta_init!< Vapour phase molar fraction (guess) [-]
  ! Output:
  real,                intent(out)    :: T        !< Temperature [K]
  real,                intent(out)    :: beta     !< Vapour phase molar fraction (solution) [-]
  real,                intent(out)    :: betaL    !< Liquid phase molar fraction [-]
  integer,             intent(out)    :: phase    !< Phase identefier
  real, dimension(nc), intent(out)    :: X        !< Liquid molar compozition [-]
  real, dimension(nc), intent(out)    :: Y        !< Vapour molar compozition [-]

  T = T_init
  beta = beta_init
  call twoPhasePSflash(T,P,Z,beta,betaL,X,Y,s,phase)

end subroutine PSflash_twophase

subroutine uvflash_twophase(nc,u,v,z,T_init,P_init,beta_init,X_init,Y_init,T,P,beta,betaL,phase,X,Y)
  ! Get the thermodynamic state of a system of total composition z at
  ! conditions u,v.
  !
  ! This routine is only for single- and two-phase (liquid-vapor)
  ! states, and should not be used if more than two phases may be expected.
  !
  use uv_solver, only: twoPhaseUVflash
  use parameters, only: TWOPH,LIQPH,VAPPH
  implicit none
  ! Input:
  integer,             intent(in)     :: nc       !< Number of components
  real,                intent(in)     :: u        !< Specific internal energy [J/mol]
  real,                intent(in)     :: v        !< Specific volume [m3/mol]
  real, dimension(nc), intent(in)     :: z        !< Overall molar compozition [-]
  real,                intent(in)     :: T_init   !< Temperature (guess) [K]
  real,                intent(in)     :: p_init   !< Pressure (guess) [Pa]
  real,                intent(in)     :: beta_init!< Vapour phase molar fraction (guess) [-]
  real, dimension(nc), intent(in)     :: X_init   !< Liquid molar compozition (guess) [-]
  real, dimension(nc), intent(in)     :: Y_init   !< Vapour molar compozition (guess) [-]
  ! Output:
  real,                intent(out)    :: T        !< Temperature [K]
  real,                intent(out)    :: P        !< Pressure [Pa]
  real,                intent(out)    :: beta     !< Vapour phase molar fraction (solution) [-]
  real,                intent(out)    :: betaL    !< Liquid phase molar fraction [-]
  integer,             intent(out)    :: phase    !< Phase identefier
  real, dimension(nc), intent(out)    :: X        !< Liquid molar compozition [-]
  real, dimension(nc), intent(out)    :: Y        !< Vapour molar compozition [-]

  T = T_init
  p = p_init
  beta = beta_init
  X = X_init
  Y = Y_init
  if (beta_init == 0.0) then
     phase = LIQPH
  elseif (beta_init == 1.0) then
     phase = VAPPH
  else
     phase = TWOPH
  endif

  call twoPhaseUVflash(T,p,z,beta,betaL,X,Y,u,v,phase)

end subroutine uvflash_twophase


subroutine sat_points_grid(nc,n_grid,Z,T0,P0,x0,y0,propflag,prop_grid,&
     T_grid,P_grid,phase_grid,wi_grid,n_grid_found)
  ! Get the saturation points of a mixture of composition Z having the
  ! property values specified in prop_grid.
  !
  ! The routine starts from the dew curve at pressure p0 and traverses
  ! the envelope trying to bracket the next grid point in prop_grid.
  !
  use saturation_point_locators, only: sat_points_based_on_prop
  implicit none
  integer, intent(in) :: nc             ! number of components
  integer, intent(in) :: n_grid         ! number of points in grid
  real, intent(in) :: Z(nc)                ! total composition
  real, intent(in) :: t0, p0               ! init. point has pressue p0
  real, intent(in) :: x0(nc),y0(nc)        ! init. liq./vap. compo. guess
  real, intent(in) :: prop_grid(n_grid)   ! descending grid [J/(mol*K)]
  integer, intent(in) :: propflag       ! the property that is specified (s:1, v:2)
  real, intent(out) :: T_grid(n_grid)        ! t at the grid points
  real, intent(out) :: P_grid(n_grid)        ! p at the grid points
  integer*4, intent(out) :: phase_grid(n_grid)    ! incumbent phase at grid points
  real, intent(out) :: wi_grid(nc,n_grid)    !< Incipient phase at boundary
  integer, intent(out) :: n_grid_found       !< Number of grid points found  i! Local
  real :: prop_grid_cpy(n_grid)   ! descending grid [J/(mol*K)]
  prop_grid_cpy = prop_grid
  call sat_points_based_on_prop(Z,T0,P0,x0,y0,n_grid,propflag,&
       prop_grid_cpy,T_grid,P_grid,phase_grid,wi_grid,&
       n_grid_found,phase_in=2)

end subroutine sat_points_grid



subroutine PHflash_twophase(nc,P,h,z,T_init, beta_init, T,beta,betaL,phase,X,Y)
  ! Get the thermodynamic state of a system of total composition z at
  ! conditions p,h.
  !
  ! This routine is only for single- and two-phase (liquid-vapor)
  ! states, and should not be used if more than two phases may be expected.
  !
  use ph_solver, only: twophasePHflash
  implicit none
  ! Input:
  integer,             intent(in)     :: nc       !< Number of components
  real,                intent(in)     :: P        !< Pressure [Pa]
  real,                intent(in)     :: h        !< Specific enthalpy [J/mol]
  real, dimension(nc), intent(in)     :: z        !< Overall molar compozition [-]
  real,                intent(in)     :: T_init   !< Temperature (guess) [K]
  real,                intent(in)     :: beta_init!< Vapour phase molar fraction (guess) [-]
  ! Output:
  real,                intent(out)    :: T        !< Temperature [K]
  real,                intent(out)    :: beta     !< Vapour phase molar fraction (solution) [-]
  real,                intent(out)    :: betaL    !< Liquid phase molar fraction [-]
  integer,             intent(out)    :: phase    !< Phase identefier
  real, dimension(nc), intent(out)    :: X        !< Liquid molar compozition [-]
  real, dimension(nc), intent(out)    :: Y        !< Vapour molar compozition [-]

  beta = beta_init
  T = T_init
  call twoPhasePHflash(T,P,z,beta,betaL,X,Y,h,phase)

end subroutine PHflash_twophase


subroutine svflash_twophase(nc,s,v,z,T_init,P_init,beta_init,T,p,beta,betaL,&
     phase,X,Y)
  ! Get the thermodynamic state of a system of total composition z at
  ! conditions s,v
  !
  ! This routine is only for single- and two-phase (liquid-vapor)
  ! states, and should not be used if more than two phases may be expected.
  !
  use sv_solver, only: twophaseSVflash
  use parameters, only: TWOPH
  implicit none
  ! Input:
  integer,             intent(in)     :: nc       !< Number of components
  real,                intent(in)     :: s        !< Entropy [J/(mol*K)]
  real,                intent(in)     :: v        !< Volume [m3/mol]
  real, dimension(nc), intent(in)     :: z        !< Overall molar compozition [-]
  real,                intent(in)     :: T_init   !< Guess temperature [K]
  real,                intent(in)     :: p_init   !< Guess pressure [Pa]
  real,                intent(in)     :: beta_init!< Guess vapor frac. [mol/mol]
  ! Output:
  real,                intent(out)    :: T        !< Temperature
  real,                intent(out)    :: p        !< Pressure
  real,                intent(out)    :: beta     !< Vapor fraction [mol/mol]
  real,                intent(out)    :: betaL    !< Liquid fraction [mol/mol]
  integer,             intent(out)    :: phase    !< Phase identifier
  real, dimension(nc), intent(out)    :: X        !< Liquid molar compozition [-]
  real, dimension(nc), intent(out)    :: Y        !< Vapour molar compozition [-]

  T = T_init
  p = p_init
  beta = beta_init
  phase = TWOPH
  call twoPhaseSVflash(T,p,Z,beta,betaL,X,Y,s,v,phase)

end subroutine svflash_twophase

subroutine boiling_prop(ic, TBoil, VLiqBoil)
  !Get parameter Tb from component and calualtes boiling
  !
  use parameters, only :nc
  use tpconst, only :RGas
  use tpvar, only :comp
  use eos, only: zfac
  implicit none
  integer,  intent(in)::ic     !< Component number
  real, intent(out)::TBoil     !< Boiling temperature
  real, intent(out)::VLiqBoil  !< Boiling liquid volume m^3/mol
  real Pb, z(nc), z_fac

  TBoil = comp(ic)%tb
  Pb = 1.013E5 !1 Athmosphere
  z=0
  z(ic)=1.0
  call zfac(TBoil, Pb, z, 1, z_fac) !Liquid phase
  VLiqBoil = z_fac * RGas * TBoil / Pb
end subroutine boiling_prop

subroutine specific_volume_no_derivs(nc,T,P,x,phase,v)
  ! Get the specific volume (volume per mole) of a phase of composition
  ! x at conditions T,P. Doesn't compute any derivatives.
  !
  use eos, only: specificVolume
  ! Input:
  implicit none
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition [-]
  integer,  intent(in)      :: phase  !< Desired phase kind (liq or vap)
  ! Output:
  real,     intent(out)     :: v        !< Specific volume [m3/mol]

  call specificVolume(T,P,x,phase,v)
end subroutine specific_volume_no_derivs

subroutine specific_volume(nc,T,P,x,phase,v,dvdt,dvdp,dvdn, bNotCalc_dvdn)
  ! Get the specific volume (volume per mole) of a phase of composition
  ! x at conditions T,P. Will also give its derivatives with respect to
  ! T,P and any mole number.
  !
  use eos, only: specificVolume
  ! Input:
  implicit none
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition [-]
  integer,  intent(in)      :: phase  !< Desired phase kind (liq or vap)
  integer,  optional, intent(in):: bNotCalc_dvdn !<1 if dvdn should NOT be calculated
  !<Variables in python get value 0 if not present.
  ! Output:
  real,     intent(out)     :: v        !< Specific volume [m3/mol]
  real,     intent(out)     :: dvdt     !< Specific volume differential wrpt. temperature [m3/(mol*K)]
  real,     intent(out)     :: dvdp     !< Specific volume differential wrpt. pressure [m3/(mol*Pa)]
  real,     intent(out)     :: dvdn(nc) !< Specific volume differential wrpt. mole numbers [m3/mol]

  !  if (.not. present(bCalc_dvdn)) then
  !Does not work with variables in python present(vCalcdvdn) = True if present or not

  if (bNotCalc_dvdn==1) then
     call specificVolume(T,P,x,phase,v,dvdt,dvdp)
     dvdn = 0.0
  else
     call specificVolume(T,P,x,phase,v,dvdt,dvdp, dvdn)
  endif

end subroutine specific_volume

subroutine specific_volume_twophase(nc,T,P,z,x,y,beta,phase,v)
  ! Get the specific volume of a (potentially) two-phase
  ! mixture of total composition z, given the result of a
  ! TP-flash: x,y,beta,phase
  !
  ! If "phase" indicates single-phase: x,y and beta is ignored.
  !
  use eos, only: twoPhaseSpecificVolume
  implicit none
  ! Input:
  integer, intent(in)  :: nc    !< Number of components
  integer, intent(in)  :: phase !< Phase identifier
  real,    intent(in)  :: t     !< Temperature [K]
  real,    intent(in)  :: p     !< Pressure [Pa]
  real,    intent(in)  :: beta  !< Vapor phase mole fraction
  real,    intent(in)  :: x(nc) !< Liquid composition [mol/mol]
  real,    intent(in)  :: y(nc) !< Vapor composition [mol/mol]
  real,    intent(in)  :: z(nc) !< Overall composition [mol/mol]
  ! Output:
  real,    intent(out) :: v     !< Specific mixture volume [m^3/mol]

  v=twoPhaseSpecificVolume(t,p,z,x,y,beta,phase)

end subroutine specific_volume_twophase

subroutine specific_internal_energy(nc,T,P,x,phase,u)
  ! Get the specific internal energy (joule per mole) of a phase of composition
  ! x at conditions T,P.
  use eos, only: specificVolume
  use eosTV, only: internal_energy
  ! Input:
  implicit none
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition [mol/mol]
  integer,  intent(in)      :: phase  !< Desired phase kind (liq or vap)
  ! Output:
  real,     intent(out)     :: u      !< Specific internal energy [J/mol]
  ! Internal:
  real                      :: v

  call specificVolume(T,P,x,phase,v)
  call internal_energy(T,v,x,u)

end subroutine specific_internal_energy

subroutine specific_internal_energy_twophase(nc,T,P,z,x,y,beta,phase,u)
  ! Get the specific internal energy of a (potentially) two-phase
  ! mixture of total composition z, given the result of a
  ! TP-flash: x,y,beta,phase
  !
  ! If "phase" indicates single-phase: x,y and beta is ignored.
  !
  use eos, only: twoPhaseInternalEnergy
  implicit none
  ! Input:
  integer, intent(in)  :: nc    !< Number of components
  real,    intent(in)  :: t     !< Temperature [K]
  real,    intent(in)  :: p     !< Pressure [Pa]
  real,    intent(in)  :: z(nc) !< Overall composition [mol/mol]
  real,    intent(in)  :: x(nc) !< Liquid composition [mol/mol]
  real,    intent(in)  :: y(nc) !< Vapor composition [mol/mol]
  real,    intent(in)  :: beta  !< Vapor phase mole fraction
  integer, intent(in)  :: phase !< Phase identifier
  ! Output:
  real,    intent(out) :: u     !< Specific internal energy [J/mol]

  u = twoPhaseInternalEnergy(t,p,z,x,y,beta,phase)

end subroutine specific_internal_energy_twophase

subroutine specific_enthalpy(nc,T,P,x,phase, h,dhdt,dhdp,dhdn,bNotCalc_dhdn)
  ! Get the specific enthalpy of a phase of composition
  ! x at conditions T,P. Will also give its derivatives with respect to
  ! T,P and any mole number.
  !
  use eos, only: enthalpy
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition [-]
  integer,  intent(in)      :: phase  !< Desired phase kind (liq or vap)
  integer,  optional, intent(in):: bNotCalc_dhdn !<1 if dhdn should NOT be calculated
  ! Output:
  real,     intent(out)     :: h        !< Specific enthalpy [J/mol]
  real,     intent(out)     :: dhdt     !< Specific enthalpy differential wrpt. temperature [J/(mol*K)]
  real,     intent(out)     :: dhdp     !< Specific enthalpy differential wrpt. pressure [J/(mol*Pa)]
  real,     intent(out)     :: dhdn(nc) !< Specific enthalpy differential wrpt. mole numbers [J/mol]

  !  if (.not. present(bNotCalc_dhdn)) then
  !Does not work with variables in python present(bNotCalc_dhdn) = True if present or not
  ! Default value of bNotCalc_dhdn is 0
  ! Use call specific_enthalpy(....,bnotcalc_dhdn=1)
  if (bNotCalc_dhdn == 1) then
    call enthalpy(t,p,x,phase,h,dhdt,dhdp)
    dhdn = 0
  else
  call enthalpy(t,p,x,phase,h,dhdt,dhdp,dhdn)
  endif
end subroutine specific_enthalpy

subroutine specific_enthalpy_twophase(nc,T,P,z,x,y,beta,phase,hmix)
  ! Get the total specific enthalpy of a (potentially) two-phase
  ! mixture of total composition z, given the result of a
  ! TP-flash: x,y,beta,phase
  !
  ! If "phase" indicates single-phase: x,y and beta is ignored.
  !
  use eos, only:  twoPhaseEnthalpy
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: z(nc)  !< Molar composition (total)  [-]
  real,     intent(in)      :: x(nc)  !< Molar composition (liquid) [-]
  real,     intent(in)      :: y(nc)  !< Molar composition (vapor)  [-]
  real,     intent(in)      :: beta   !< Vapor fraction  [-]
  integer,  intent(in)      :: phase  !< Phase indicator
  ! Output:
  real,     intent(out)     :: hmix   !< Specific enthalpy [J/mol]

  hmix = twoPhaseEnthalpy(t,p,z,x,y,beta,phase)
end subroutine specific_enthalpy_twophase

subroutine specific_entropy(nc,T,P,x,phase,s)
  ! Get specific entropy of a phase with composition x at
  ! specified T and P.
  !
  use eos, only: entropy
  implicit none
  !
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition [-]
  integer,  intent(in)      :: phase  !< Desired phase kind (liq or vap)
  ! Output:
  real,     intent(out)     :: s        !< Specific entropy [J/(K*mol)]

  call entropy(t,p,x,phase,s)

end subroutine specific_entropy

subroutine specific_entropy_deriv(nc,T,P,x,phase, s,dsdt,dsdp,dsdn)
  ! Get the specific entropy of a phase of composition
  ! x at conditions T,P. Will also give its derivatives with respect to
  ! T,P and any mole number.
  !
  use eos, only: entropy
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition [-]
  integer,  intent(in)      :: phase  !< Desired phase kind (liq or vap)
  ! Output:
  real,     intent(out)     :: s        !< Specific entropy [J/mol K]
  real,     intent(out)     :: dsdt     !< Specific entropy differential wrpt. temperature [J/(mol*K^2)]
  real,     intent(out)     :: dsdp     !< Specific entropy differential wrpt. pressure [J/(mol*Pa K)]
  real,     intent(out)     :: dsdn(nc) !< Specific entropy differential wrpt. mole numbers [J/mol K]

  call entropy(t,p,x,phase,s,dsdt,dsdp,dsdn)

end subroutine specific_entropy_deriv

subroutine specific_entropy_twophase(nc,T,P,z,x,y,beta,phase,smix)
  ! Get the total specific entropy of a (potentially) two-phase
  ! mixture of total composition z, given the result of a
  ! TP-flash: x,y,beta,phase
  !
  ! If "phase" indicates single-phase: x,y and beta is ignored.
  !
  use eos, only: twoPhaseEntropy
  implicit none
  !
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: z(nc)  !< Molar composition (total)  [-]
  real,     intent(in)      :: x(nc)  !< Molar composition (liquid) [-]
  real,     intent(in)      :: y(nc)  !< Molar composition (vapor)  [-]
  real,     intent(in)      :: beta   !< Vapor fraction  [-]
  integer,  intent(in)      :: phase  !< Phase indicator
  ! Output:
  real,     intent(out)     :: smix   !< Specific entropy [J/(K*mol)]

  smix = twoPhaseEntropy(T,P,z,x,y,beta,phase)

end subroutine specific_entropy_twophase

subroutine bubble_t_full(nc, p, Z, T, Y)
  ! Calculate bubble point and vapor composition at pressure p
  use saturation, only: safe_bubT
  implicit none
  ! Input
  integer, intent(in) :: nc                 !< Number of components
  real, dimension(nc), intent(in) :: Z      !< Total composition
  real, intent(inout) :: p                  !< Pressure [Pa]
  ! Output
  real, dimension(nc), intent(out) :: Y     !< Incipient vapor phase comp
  real, intent(out) :: T                    !< Temperature [K]

  Y = Z
  T = safe_bubT(p, Z, Y)
end subroutine bubble_t_full

subroutine bubble_p_full(nc,t,z,bubp, y)
  ! Get the bubble-point pressure of mixture z
  ! at a given temperature T.
  !
  use saturation, only: safe_bubP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: t      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: bubp   !< Bubble pressure [Pa]
  real,     intent(out)   :: y(nc)  !< Incipient vapor composition [-]
  bubp = safe_bubP(t,z,y)
end subroutine bubble_p_full


subroutine dew_p_full(nc,t,z,dewp,x)
  ! Get the dew-point pressure of mixture z
  ! at a given temperature T.
  !
  use saturation, only: safe_dewP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: t      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: dewp   !< Dew pressure [Pa]
  real,     intent(out)   :: x(nc)  !< Incipient liquid composition [-]
  ! Internal:
  dewp = safe_dewP(t,x,z)
end subroutine dew_p_full

subroutine bubble_p(nc,T,z,bubP)
  ! Get the bubble-point pressure of mixture z
  ! at a given temperature T.
  !
  use saturation, only: safe_bubP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: T      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: bubP   !< Bubble pressure [Pa]
  ! Internal:
  real                    :: Y(nc)
  bubP = safe_bubP(T,Z,Y)
end subroutine bubble_p

subroutine bubble_p_error_handling(nc,T,z,bubP,ierr)
  ! Get the bubble-point pressure of mixture z at a given temperature T. Also
  ! output error flag, for flexibility of error handling.
  !
  use saturation, only: safe_bubP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: T      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: bubP   !< Bubble pressure [Pa]
  integer,  intent(out)   :: ierr
  ! Internal:
  real                    :: Y(nc)
  bubP = safe_bubP(T,Z,Y,ierr)
end subroutine bubble_p_error_handling

subroutine dew_p_error_handling(nc,T,z,dewP,ierr)
  ! Get the dew-point pressure of mixture z at a given temperature T. Also
  ! output error flag, for flexibility of error handling.
  !
  use saturation, only: safe_dewP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: T      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: dewP   !< Dew pressure [Pa]
  integer,  intent(out)   :: ierr
  ! Internal:
  real                    :: X(nc)
  dewP = safe_dewP(T,X,Z,ierr)
end subroutine dew_p_error_handling
  
subroutine bubble_p_unsafe(nc,T,z,P0,pbub,ierr)
  ! Get the bubble-point pressure of mixture z at a given temperature T and
  ! initial estimate P0. Also output error flag, for flexibility of error
  ! handling.
  !
  use saturation, only: bubP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: T      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  real,     intent(in)    :: P0     !< Molar composition
  ! Output:
  real,     intent(out)   :: Pbub   !< Bubble pressure [Pa]
  integer,  intent(out)   :: ierr
  ! Internal:
  real                    :: tloc,ploc,Y(nc)
  tloc = T
  ploc = P0
  !print *, " "
  !print *, "tin,pin",tloc,ploc
  Pbub = bubP(tloc,ploc,Z,Y,ierr)
  !print *, "tloc,ploc, ierr",tloc,ploc, ierr
  Pbub = ploc
end subroutine bubble_p_unsafe

subroutine bubble_T(nc,p,z,bubT)
  ! Get the bubble-point temperature of mixture z
  ! at a given pressure p.
  !
  use saturation, only: safe_bubT
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: p      !< Pressure [Pa]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: bubT   !< Bubble temperature [K]
  ! Internal:
  real                    :: Y(nc)
  bubT = safe_bubT(p,Z,Y)
end subroutine bubble_T

subroutine dew_p(nc,T,z,dewP)
  ! Get the dew-point pressure of mixture z
  ! at a given temperature T.
  !
  use saturation, only: safe_dewP
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: T      !< Temperature [K]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: dewP   !< Dew pressure [Pa]
  ! Internal:
  real                    :: X(nc)
  dewP = safe_dewP(T,X,Z)
end subroutine dew_p

subroutine dew_T(nc,p,z,dewT)
  ! Get the dew-point temperature of mixture z
  ! at a given pressure p.
  !
  use saturation, only: safe_dewT
  implicit none
  ! Input:
  integer,  intent(in)    :: nc     !< Number of components
  real,     intent(in)    :: p      !< Pressure [Pa]
  real,     intent(in)    :: z(nc)  !< Molar composition
  ! Output:
  real,     intent(out)   :: dewT   !< Dew temperature [K]
  ! Internal:
  real                    :: X(nc)
  dewT = safe_dewT(p,X,Z)
end subroutine dew_T

subroutine sat_p_pure(T,nc,icomp,satp)
  ! Get the saturation pressure of pure component icomp
  ! at temperature T.
  !
  use puresaturation, only: puresat
  implicit none
  ! Input
  real,     intent(in)    :: T      !< Temperature [K]
  integer,  intent(in)    :: nc     !< Number of components
  integer,  intent(in)    :: icomp  !< Index of desired component (1,2,...)
  ! Output:
  real,     intent(out)   :: satp   !< Saturation pressure [Pa]
  ! Internal:
  real                    :: z_pure(nc)
  integer                 :: ierr
  real                    :: T_inout

  z_pure = 0.0
  z_pure(icomp) = 1.0
  T_inout = T
  call puresat(T_inout, satp, z_pure, .false., .false., ierr)
  if (ierr /= 0) then
    call stoperror("puresat() failed to find saturation pressure.")
  endif

end subroutine sat_p_pure

subroutine sat_T_pure(p,nc,icomp,satT)
  ! Get the saturation temperature of pure component icomp
  ! at pressure p.
  !
  use puresaturation, only: puresat
  implicit none
  ! Input
  real,     intent(in)    :: p      !< Pressure [Pa]
  integer,  intent(in)    :: nc     !< Number of components
  integer,  intent(in)    :: icomp  !< Index of desired component (1,2,...)
  ! Output:
  real,     intent(out)   :: satT   !< Saturation temp. [K]
  ! Internal:
  real                    :: z_pure(nc)
  integer                 :: ierr
  real                    :: p_inout

  z_pure = 0.0
  z_pure(icomp) = 1.0
  p_inout = p
  call puresat(satT, p_inout, z_pure, .true., .false., ierr)
  if (ierr /= 0) then
    call stoperror("puresat() failed to find saturation temperature.")
  endif
end subroutine sat_T_pure

subroutine fugasity(nc, T, P, z, iphase, fug, fugt, fugp, fugx, blnfug)
  ! Calculates Fugacity unit Pa.
  !  (beta, betal, phase, x, y)=tp.tpflash_twophase(T, P, [0.4, 0.6], 0.0);
  !  f1=tp.specific_fugasity(T,P,x, 1)
  !  f2=tp.specific_fugasity(T,P,y, 2)
  !  Gives: exp(f1)*x - exp(f2)*y = [0,0]

  use eos , only: thermo
  integer, intent(in) :: nc
  real,    intent(in) :: T
  real,    intent(in) :: P
  real,    intent(in) :: z(nc)
  integer, intent(in) :: iphase
  integer, intent(in) :: blnfug !<Return ln(fug/(z*P))
  real,    intent(out) :: fug(nc)
  real,    intent(out) :: fugt(nc)
  real,    intent(out) :: fugp(nc)
  real,    intent(out) :: fugx(nc,nc)

  real :: lnfug(nc), lnfugt(nc), lnfugp(nc), lnfugx(nc,nc), zz

  call thermo(T, P, z, iphase, lnfug, lnfugt, lnfugp, lnfugx)

  if (blnfug == 1) then
     fug = lnfug
     fugt = lnfugt
     fugp = lnfugp
     fugx = lnfugx
  else
     fug = exp(lnfug)*z*P
     fugt = lnfugt * fug
     fugp = lnfugp * fug + fug/p
     do i=1,nc
        do j=1,nc
           fugx(i,j)=lnfugx(i,j)*fug(i)
        enddo
        fugx(i,i) = fugx(i,i) + exp(lnfug(i))*P
     enddo
  endif
end subroutine fugasity

subroutine envelope_twophase(nc,z,T_init,p_init, rspec, beta_in, pmax,&
     T_out,p_out,K_out,beta_out,nvals)
  ! Get the two-phase envelope of a mixture of composition z, starting from the
  ! bubble-line at T_init (at appriximately p_init_guess).
  ! The output is arrays T_out and p_out, which are filled if nvals values.
  !
  use saturation_curve, only: envelopePlot
  implicit none
  ! Output length:
  integer, parameter        :: nmax = 1000
  ! Input:
  integer,    intent(in)    :: nc
  real,       intent(in)    :: z(nc)
  real,       intent(in)    :: T_init
  real,       intent(in)    :: p_init
  real,       intent(in)    :: rspec  !1: Spesify P, 2: Specify T (Def 2)
  real,       intent(in)    :: beta_in   !Default(0.0)
  real,       intent(in)    :: pmax      !Default 150
  ! Output:
  real,       intent(out)   :: T_out(nmax)
  real,       intent(out)   :: p_out(nmax)
  real,       intent(out)   :: K_out(nmax,nc)
  real,       intent(out)   :: beta_out(nmax)
  integer,    intent(out)   :: nvals
  ! Internal:
  integer        :: spec! Specify T at initial point

  spec =   floor(rspec + 0.1)

  call envelopePlot(z,T_init,p_init,spec,beta_in,pmax,nmax,T_out,p_out,K_out,beta_out,nvals)

end subroutine envelope_twophase

subroutine envelope_twophase_full(nc,z,t_init,p_init, rspec_pt_12, beta_in, pmax,&
     nmax, T_out,p_out,K_out,beta_out,criconden,crit,ds_override,nvals)
  ! Get the two-phase envelope of a mixture of composition z, starting from the
  ! bubble-line at T_init (at appriximately p_init_guess).
  ! The output is arrays T_out and p_out, which are filled if nvals values.
  !
  use saturation_curve, only: envelopePlot
  implicit none
  ! Input:
  integer,    intent(in)    :: nc
  real,       intent(in)    :: z(nc)
  real,       intent(in)    :: T_init
  real,       intent(in)    :: p_init
  real,       intent(in)    :: rspec_pt_12  !1: Specify P, 2: Specify T
  real,       intent(in)    :: beta_in   !Default 0.0
  real,       intent(in)    :: pmax      !Default 150
  integer, intent(in)        :: nmax ! Output length
  real, intent(in)  :: dS_override ! Override step length
  ! Output:
  real,       intent(out)   :: T_out(nmax)
  real,       intent(out)   :: p_out(nmax)
  real,       intent(out)   :: K_out(nmax,nc)
  real,       intent(out)   :: beta_out(nmax)
  real, intent(out) :: criconden(4)! tcb,pcb,tct,pct
  real, intent(out) :: crit(2)     ! tc, pc

  integer,    intent(out)   :: nvals
  ! Internal:
  integer        :: spec! Specify T at initial point

  spec =   floor(rspec_pt_12 + 0.1)

  call envelopePlot(z,T_init,p_init,spec,beta_in,pmax,&
       nmax,T_out,p_out,K_out,beta_out,nvals, &
       criconden=criconden, crit=crit, ds_override=ds_override)

end subroutine envelope_twophase_full

subroutine envelope_twophase_fitting(nc,z,t_init,p_init, rspec_pt_12, beta_in, pmax,&
     tmin_in, ds_override, nmax, T_out,p_out,K_out,beta_out,nvals)
  ! Get the two-phase envelope of a mixture of composition z, starting from the
  ! bubble-line at T_init (at appriximately p_init_guess).
  ! The output is arrays T_out and p_out, which are filled if nvals values.
  !
  use saturation_curve, only: envelopePlot
  implicit none
  ! Input:
  integer,    intent(in)    :: nc
  real,       intent(in)    :: z(nc)
  real,       intent(in)    :: T_init
  real,       intent(in)    :: p_init
  real,       intent(in)    :: rspec_pt_12  !1: Specify P, 2: Specify T
  real,       intent(in)    :: beta_in   !Default 0.0 (vapor)
  real,       intent(in)    :: pmax      !Default 150 bar
  real,       intent(in)    :: Tmin_in      !Default 80 K
  integer, intent(in)       :: nmax ! Output length
  real, intent(in)          :: dS_override ! Override step length
  ! Output:
  real,       intent(out)   :: T_out(nmax)
  real,       intent(out)   :: p_out(nmax)
  real,       intent(out)   :: K_out(nmax,nc)
  real,       intent(out)   :: beta_out(nmax)

  integer,    intent(out)   :: nvals
  ! Internal:
  integer        :: spec! Specify T at initial point

  spec = floor(rspec_pt_12 + 0.1)

  call envelopePlot(z,T_init,p_init,spec,beta_in,pmax,&
       nmax,T_out,p_out,K_out,beta_out,nvals, &
       ds_override=ds_override,Tme=Tmin_in)

end subroutine envelope_twophase_fitting

subroutine stability_limit_line_tpv_single_neg_press(p0,Tmin,T,p,v)
  ! Get the stability limit line in T,p-space, from a starting
  ! pressure until the critical pressure.
  !
  use critical, only: singleCompLiqStabilityLimitNegPress
  use parameters, only: LIQPH, VAPPH
  implicit none
  ! Input:
  real,   intent(in)    :: p0       ! Pressure to start line from (Pa)
  real,   intent(in)    :: Tmin     ! Minimum temperature (K)
  ! Output:
  real,   intent(out)   :: T(100)  ! Temperatures along line (K)
  real,   intent(out)   :: p(100)  ! Pressures along line (Pa)
  real,   intent(out)   :: v(100)  ! Specific volume (m3/mol)
  !integer,intent(out)   :: npts     ! Points along line
  !integer,intent(out)   :: ierr     ! Error-flag
  ! Internal:
  !
  call singleCompLiqStabilityLimitNegPress(P0,Tmin,T,v,p)

end subroutine stability_limit_line_tpv_single_neg_press

subroutine stability_limit_line_Tpv(nc,z,p0,Tmin,T,p,v,npts,ierr)
  ! Get the stability limit line in T,p-space, from a starting
  ! pressure until the critical pressure.
  !
  use critical, only: mapMetaStabilityLimit, nMax
  use parameters, only: LIQPH, VAPPH
  implicit none
  ! Input:
  integer,intent(in)    :: nc       ! Number of components
  real,   intent(in)    :: z(nc)    ! Molar composition (-)
  real,   intent(in)    :: p0       ! Pressure to start line from (Pa)
  real,   intent(in)    :: Tmin     ! Minimum temperature (K)
  ! Output:
  real,   intent(out)   :: T(1000)  ! Temperatures along line (K)
  real,   intent(out)   :: p(1000)  ! Pressures along line (Pa)
  real,   intent(out)   :: v(1000)  ! Specific volume (m3/mol)
  integer,intent(out)   :: npts     ! Points along line
  integer,intent(out)   :: ierr     ! Error-flag
  ! Internal:

  call mapMetaStabilityLimit(p0,z,Tmin,T,p,v,npts,ierr)

end subroutine stability_limit_line_Tpv

subroutine stability_limit_liquid_p(nc,z,p,T,v,ierr)
  ! Given the pressure and composition find stability limit
  !
  use critical, only: initialStablimitPoint
  use parameters, only: LIQPH
  implicit none
  ! Input:
  integer,intent(in)    :: nc       ! Number of components
  real,   intent(in)    :: z(nc)    ! Molar composition (-)
  real,   intent(in)    :: p        ! Pressure (Pa)
  ! Output:
  real,   intent(out)   :: T        ! Temperature stab. limit (K)
  real,   intent(out)   :: v        ! Molar volume (m^3/mol)
  integer,intent(out)   :: ierr     ! Error-flag

  call initialStablimitPoint(p,z,v,T,LIQPH,ierr)

end subroutine stability_limit_liquid_p

subroutine stability_limit_vapor_p(nc,z,p,T,v,ierr)
  ! Given the pressure and composition find stability limit
  !
  use critical, only: initialStablimitPoint
  use parameters, only: VAPPH
  implicit none
  ! Input:
  integer,intent(in)    :: nc       ! Number of components
  real,   intent(in)    :: z(nc)    ! Molar composition (-)
  real,   intent(in)    :: p        ! Pressure (Pa)
  ! Output:
  real,   intent(out)   :: T        ! Temperature stab. limit (K)
  real,   intent(out)   :: v        ! Molar volume (m^3/mol)
  integer,intent(out)   :: ierr     ! Error-flag

  call initialStablimitPoint(p,z,v,T,VAPPH,ierr)

end subroutine stability_limit_vapor_p

subroutine critical(nc, z, T_guess, p_guess, T_crit, p_crit, ierr)
  use parameters, only: VAPPH
  use eos, only: getCriticalParam
  use critical, only: calcCritical
  ! Calculate critical point (T_crit, p_crit)
  !
  ! Input:
  integer,intent(in)    :: nc       ! Number of components
  real,   intent(in)    :: z(nc)    ! Molar composition (-)
  real,   intent(in)    :: T_guess  ! Temperature guess (K)
  real,   intent(in)    :: p_guess  ! Pressure guess (Pa)
  ! Output:
  real,   intent(out)   :: T_crit   ! Critical temperature (K)
  real,   intent(out)   :: p_crit   ! Critical pressure (K)
  integer,intent(out)   :: ierr     ! If zero, success.
  ! Internal:
  real                  :: acentricfac

  ! one-component: Just get the critical point
  if (nc==1) then
     call getCriticalParam(1, T_crit, p_crit, acentricfac)
     ierr = 0
  else
     T_crit = T_guess
     p_crit = p_guess
     call calcCritical(T_crit, p_crit, z, VAPPH, ierr)
  endif
end subroutine critical


subroutine print_cubic_params(nc,T,P,z)
  ! Print the contents of tpvar%cbeos(1), i.e. the
  ! properties of the current cubic EoS, at the given
  ! temperature, pressure and composition.
  !
  ! A few of the outputs require z-factor. Here the single-phase
  ! (vapor-preferred) solution is used.
  !
  use parameters, only: EoSlib, THERMOPACK, VAPPH
  use tpvar, only: cbeos
  use eos, only: zfac
  use tpcubic, only: cbDumpEosData
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: z(nc)  !< Molar composition
  ! Internal:
  real                      :: z_fac

  if (EoSlib == THERMOPACK) then
     call zfac(T,P,z,VAPPH,z_fac)
     call cbDumpEosData(nc,cbeos(1),T,P,z_fac)
  else
     write(*,*) "print_cubic_params: Only for EoSlib Thermopack!"
  endif

end subroutine print_cubic_params


subroutine set_kijset (iset)
  use parameters, only: nc
  use tpvar, only: comp, cbeos
  use tpselect, only: tpSelectInteractionParameters

  ! Select active set of binary interaction parameters
  call tpSelectInteractionParameters(nc,comp,cbeos(1),iset)
end subroutine set_kijset


subroutine set_kij(i,j,kij)
  ! Set the kij cubic EoS parameter.
  !
  ! Note that this does NOT set k(j,i)=k(i,j), i.e. it supports
  ! asymmetry in the kij matrix.
  !
  use tpvar, only: cbeos
  use parameters, only: EoSlib, THERMOPACK
  implicit none
  ! Input:
  integer,    intent(in)    :: i
  integer,    intent(in)    :: j
  real,       intent(in)    :: kij

  if (EoSlib == THERMOPACK) then
     cbeos(1)%kij(i,j) = kij
  else
     write(*,*) "print_cubic_params: Only for EoSlib Thermopack!"
  endif
end subroutine set_kij

subroutine set_cubic_m1m2(m1, m2)
  !> Set the cubic m1 and m2 parameters (in the denominator)
  use parameters, only: EoSlib, THERMOPACK
  use tpcbmix, only: setCubicm1m2
  implicit none
  ! Input:
  real,       intent(in)    :: m1, m2 
  if (EoSlib == THERMOPACK) then
    call setCubicm1m2(m1, m2) ! see tuning.f90
  else
     write(*,*) "set_cubic_m1m2: Only for EoSlib Thermopack!"
  endif
end subroutine set_cubic_m1m2

subroutine get_cubic_m1m2(m1, m2)
  !> Set the cubic m1 and m2 parameters (in the denominator)
  use parameters, only: EoSlib, THERMOPACK
  use tpcbmix, only: getCubicm1m2
  implicit none
  ! Input:
  real,       intent(out)    :: m1, m2 
  if (EoSlib == THERMOPACK) then
    call getCubicm1m2(m1, m2) ! see tuning.f90
  else
     write(*,*) "get_cubic_m1m2: Only for EoSlib Thermopack!"
  endif
end subroutine get_cubic_m1m2

!--------------------------------------------------------------
subroutine set_excess_gibbs_inter_ij(i,j, aGE, bGE, cGE, alpha)
  ! Set the parameters for the Huron Vidal mixing rules
  ! Must be called with i,j and j,i
  ! SET tau = aGE/T + bGE + cGE*T for MHV2 and alpha=0
  !    tau = aGE/T + bGE for MHV1  and alpha=0
  ! if alpha >0: uses default mixign rule with alpha
  use tpvar, only :cbeos
  use parameters, only: EosLib, THERMOPACK
  use eosdata, only: cbMixHuronVidal, cbMixHuronVidal2, cbMixNRTL
  implicit none
  ! Input
  integer, intent(in) :: i
  integer, intent(in) :: j
  real,    intent(in) :: aGE
  real,    intent(in) :: bGE
  real,    intent(in) :: cGE
  real,    intent(in) :: alpha
  if ( cbeos(1)%mruleidx  /= cbMixHuronVidal .and. &
       cbeos(1)%mruleidx /= cbMixHuronVidal2 .and. &
       cbeos(1)%mruleidx /= cbMixNRTL) then
     write(*,*) 'set_excess_gibbs_inter_ij: Must use mixing rule HuronVidal or NRTL'
  elseif (EosLib /= THERMOPACK ) then
     write(*,*) 'set_excess_gibbs_inter_ij: Only for EOSlib ThermoPack'
  else
     cbeos(1)%mixGE%aGE(i,j) = aGE
     cbeos(1)%mixGE%bGE(i,j) = bGE
     cbeos(1)%mixGE%cGE(i,j) = cGE
     cbeos(1)%mixGE%alpha(i,j) = alpha
  endif
  !See tuning.f90 for setting and gettign binary parameters
  !See tbcmix.f90: For use of HV parameters
end subroutine set_excess_gibbs_inter_ij

!-------------------------------------------------------------
subroutine get_kij(i,j,kij)
  ! Get the kij cubic EoS parameter.
  !
  use tpvar, only: cbeos
  use parameters, only: EoSlib, THERMOPACK
  implicit none
  ! Input:e
  integer,    intent(in)    :: i
  integer,    intent(in)    :: j
  ! Output
  real,       intent(out)    :: kij

  if (EoSlib == THERMOPACK) then
     kij = cbeos(1)%kij(i,j)
  else
     write(*,*) "print_cubic_params: Only for EoSlib Thermopack!"
  endif
end subroutine get_kij

!--------------------------------------------------------------
subroutine get_excess_gibbs_inter_ij(i,j, aGE, bGE, cGE, alpha)
  ! Get the parameters for the Huron Vidal mixing rules
  ! Must be called with both i,j and j,i to get all parameters.
  use tpvar, only :cbeos
  use parameters, only: EosLib, THERMOPACK
  use eosdata, only: cbMixHuronVidal, cbMixHuronVidal2, cbMixNRTL
  implicit none
  ! Input
  integer, intent(in) :: i
  integer, intent(in) :: j
  ! Output
  real,    intent(out) :: aGE
  real,    intent(out) :: bGE
  real,    intent(out) :: cGE
  real,    intent(out) :: alpha
  if ( cbeos(1)%mruleidx  /= cbMixHuronVidal .and. &
       cbeos(1)%mruleidx /= cbMixHuronVidal2 .and. &
       cbeos(1)%mruleidx /= cbMixNRTL) then
     write(*,*) 'get_excess_gibbs_inter_ij: Must use mixing rule HuronVidal or NRTL'
  elseif (EosLib /= THERMOPACK ) then
     write(*,*) 'get_excess_gibbs_inter_ij: Only for EOSlib ThermoPack'
  else
     aGE = cbeos(1)%mixGE%aGE(i,j)
     bGE = cbeos(1)%mixGE%bGE(i,j)
     cGE = cbeos(1)%mixGE%cGE(i,j)
     alpha = cbeos(1)%mixGE%alpha(i,j)
  endif
  !See routine tuning.f90
end subroutine get_excess_gibbs_inter_ij

!----------------------------------------------------------------------
subroutine get_molar_weights(nc,mws)
  ! Return the component molar weights
  use eos, only: compMoleWeight
  implicit none
  ! Input:
  integer,             intent(in)  :: nc  !< Number of components
  ! Output:
  real, dimension(nc), intent(out) :: mws !< Molar weights (kg/mol)
  ! Internal:
  integer                          :: ic
  !
  do ic=1,nc
     mws(ic) = compMoleWeight(ic)/1000.0
  end do
  !
end subroutine get_molar_weights

!----------------------------------------------------------------------
subroutine single_phase_speed_of_sound(nc,T,p,Z,phase, sos)
  use speed_of_sound, only: singlePhaseSpeedOfSound
  implicit none
  ! Input:
  integer,               intent(in) :: nc    !< Number of components
  real,                  intent(in) :: T     !< Temperature [K]
  real,                  intent(in) :: p     !< Pressure [Pa]
  real, dimension(nc),   intent(in) :: Z     !< Overall molar compozition [-]
  integer,               intent(in) :: phase !< Phase spec
  real,                  intent(out) :: sos  !< Speed of sound
  sos = singlePhaseSpeedOfSound(T,p,Z,phase)
end subroutine single_phase_speed_of_sound

subroutine speed_of_sound_twophase(nc,T,p,z,x,y,beta,phase,sos)
  use speed_of_sound, only: sound_velocity_2ph
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: z(nc)  !< Molar composition (total)  [-]
  real,     intent(in)      :: x(nc)  !< Molar composition (liquid) [-]
  real,     intent(in)      :: y(nc)  !< Molar composition (vapor)  [-]
  real,     intent(in)      :: beta   !< Vapor fraction  [-]
  integer,  intent(in)      :: phase  !< Phase indicator
  ! Output:
  real,     intent(out)     :: sos    !< Speed of sound
  ! Internal:
  real                      :: betal

  betal = 1.0 - beta
  sos = sound_velocity_2ph(T,p,x,y,z,beta,betal,phase)
end subroutine speed_of_sound_twophase


subroutine z_factor(nc,T,P,x,phase,z_fac)
  ! Get the Z-factor (compressibility factor) of a phase of
  ! composition x at state T and P.
  !
  ! A preferred phase (liquid or varor) must be specified, in case
  ! more than one solution exists for T,P,x.
  !
  use eos, only: zfac
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition
  integer,  intent(in)      :: phase  !< Preferred phase
  ! Output:
  real,     intent(out)     :: z_fac  !< Compressibility

  call zfac(T,P,x,phase,z_fac)

end subroutine z_factor


subroutine guess_phase(nc,T,P,x,phase)
  ! Estimate the single phase given by T,P,z as either liquid or vapor.
  !
  use thermo_utils, only: guessPhase
  implicit none
  ! Input:
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: P      !< Pressure [Pa]
  real,     intent(in)      :: x(nc)  !< Molar composition
  ! Output:
  integer,  intent(out)     :: phase  !< Best guess for phase (LIQPH or VAPPH)

  phase = guessPhase(T,P,x)

end subroutine guess_phase


subroutine pseudo_critical(nc,x,tpc,ppc,zpc,vpc)
  ! Get pseudo critical point.
  !
  use eos, only: pseudo
  implicit none
  ! Input:
  integer,             intent(in)  :: nc  !< Number of components
  real, dimension(nc), intent(in)  :: x   !< Composition [mol/mol]
  ! Output:
  real,                intent(out) :: tpc !< Pseudo critical temperature [K]
  real,                intent(out) :: vpc !< Pseudo critical specific volume [m3/mol]
  real,                intent(out) :: ppc !< Pseudo critical pressure [Pa]
  real,                intent(out) :: zpc !< Pseudo critical compressibillity factor [-]
  ! Internal:
  real :: acfpc

  call pseudo(x,tpc,ppc,acfpc,zpc,vpc)

end subroutine pseudo_critical

subroutine envelope_isentrope_cross_single(nc,p0,s_spec,z,p_low,has_crossing,&
     p_crossing, T_crossing,&
     phase_crossing, ierr)
  ! Find if, and if so where, the isentrope from a point p0,s_spec
  ! meets the saturation line before reaching pressure p_low.
  !
  ! This is for single-component calculation only.
  ! The maximum value in composition-vector z defines which component
  ! is used.
  use saturation_curve, only: envelopeIsentropeCross_single
  implicit none
  ! Input:
  integer,      intent(in)    :: nc       ! Number of components
  real,         intent(in)    :: p0       ! Initial pressure (Pa)
  real,         intent(in)    :: s_spec   ! Isentrope entropy (J/(mol*K))
  real,         intent(in)    :: z(nc)    ! Molar composition (-)
  real,         intent(in)    :: p_low

  ! Output:
  logical,      intent(out)   :: has_crossing !> If isentrope meets sat. line
  real,         intent(out)   :: p_crossing !> Pres. when meeting sat.line (Pa)
  real,         intent(out)   :: T_crossing !> Temp. when meeting sat.line (Pa)
  integer,      intent(out)   :: phase_crossing !> Phase on incoming side
  integer,      intent(out)   :: ierr         !> Error if not zero.

  call envelopeIsentropeCross_single(p0,s_spec,z,p_low,has_crossing,&
       p_crossing,T_crossing,phase_crossing,ierr)


end subroutine envelope_isentrope_cross_single

!------------------Wong Sandler-------------------

subroutine alpha_wong_sandler(tau, alpha)
  use wong_sandler, only : fidel_alpha
  real, intent(in) :: tau
  real, intent(out) :: alpha
  real tau2

  tau2=0.0
  alpha = fidel_alpha(tau, tau2)
end subroutine alpha_wong_sandler

subroutine set_par_Wong_Sandler(i,j, k, Tau, Alpha)
  use tpvar, only: cbeos
  use eosdata, only: fraction
  implicit none

  !Set parameters for Wong_Sandler
  integer, intent(in) :: i   !First parameter
  integer, intent(in) :: j   !Second parameter
  real, intent(in) :: k !Binary parameter Peng Robinson
  real, intent(in) :: Tau !Tau, PS normally Tau(1,2) <> Tau(2,1)
  real, intent(in) ::  Alpha  !Alpha
  type(fraction):: Frac

  Frac%pNum = 0.0
  Frac%pDen = 0.0
  Frac%pDen(1) = 1.0

  Frac%pNum(1) = k
  cbeos(1)%mixWS%f_kij(i,j) = Frac
  Frac%pNum(1) = Tau
  cbeos(1)%mixWS%f_tauij(i,j) = Frac
  cbeos(1)%mixWS%alphaij(i,j) = Alpha
end subroutine set_par_Wong_Sandler

subroutine get_par_Wong_Sandler(i,j,k, Tau, Alpha)
  use tpvar, only: cbeos
  use eosdata, only: fraction
  implicit none

  !Get parameters for Wong_Sandler
  !AT moment only for constant
  integer, intent(in) :: i
  integer, intent(in) :: j
  real, intent(out) :: k
  real, intent(out) :: Tau
  real, intent(out) :: Alpha
  type(fraction):: Frac

  Frac%pNum = 0.0
  Frac%pDen = 0.0
  Frac%pDen(1) = 1.0

  Frac = cbeos(1)%mixWS%f_kij(i,j)
  k = Frac%pNum(1)
  Frac = cbeos(1)%mixWS%f_tauij(i,j)
  Tau = Frac%pNum(1)
  Alpha = cbeos(1)%mixWS%Alphaij(i,j)
end subroutine get_par_Wong_Sandler


subroutine test(nc, T, P, z, iphase)
  integer,      intent(in)    :: nc           ! Number of components

  real, intent(in):: T
  real, intent(in):: P
  real, intent(in):: z(nc)
  integer, intent(in):: iphase
  call testcubicmodel(T, P, z, iphase)
end subroutine test

subroutine test2(x1, pNum, pDen)
  use eosdata, only: fraction
  use excess_gibbs, only: GetFraction
  implicit none

  real, intent(in) :: x1, pNum(3), pDen(3)

  type(fraction):: Frac
  real x2,y1,yd1,ydd1,y2,yd2,ydd2,ym, ydm, ydn, yddn, yddm, dx

  Frac%pNum = 0.0
  Frac%pDen = 0.0
  Frac%pNum = pNum
  Frac%pDen = pDen

  dx = 1E-5
  call GetFraction(Frac, x1, y1, yd1, ydd1)
  x2 = x1+dx
  call GetFraction(Frac, x2, y2, yd2, ydd2)

  ym = 0.5*(y1+y2)
  ydm = 0.5*(yd1+yd2)
  ydn= (y2-y1)/dx
  yddn = (yd2 - yd1)/dx
  yddm = 0.5*(ydd1+ydd2)
  print *,'yd(an,num):',ydm, ydn
  print *,'ydd(an,num):',yddm, yddn

end subroutine test2

!--------------------------------------------------------------------------
subroutine VLLEBinaryPxy(T,Pmax,dzmax,dlnsMax,Pmin,xLLE,wLLE,pLLE,nLLE,&
     xL1VE,yL1VE,pL1VE,nL1VE,xL2VE,yL2VE,pL2VE,nL2VE)
  !Get phenvelope by using  binary XY
  use binaryPlot, only : VLLEBinaryXY, TSPEC
  implicit none
  integer, parameter :: maxpoints = 10000 !< Redefinition of maxpoints
  ! Input
  real,   intent(in) :: T       !< Temperature (K)
  real,   intent(in) :: Pmax    !< Max Pressure (Pa)
  real,   intent(in) :: dzMax   !< Max difference between x1 and y1 between two points
  real,   intent(in) :: dlnsMax !< Max step size
  real,   intent(in) :: Pmin    !< Min Pressure (Pa)

  !Output
  real, dimension(maxpoints), intent(out) :: xLLE,wLLE,pLLE
  real, dimension(maxpoints), intent(out) :: xL1VE,yL1VE,pL1VE
  real, dimension(maxpoints), intent(out) :: xL2VE,yL2VE,pL2VE
  integer, intent(out) :: nLLE, nL1VE, nL2VE

  ! Locals
  real :: Tmin, P, Tl
  character(len=*), parameter :: filename = 'binary.dat'
  real :: res(9,maxpoints)
  integer :: i, nRes(3)

  Tmin = 100.0
  P = 1.0
  Tl = T
  call VLLEBinaryXY(T=Tl,P=P,ispec=TSPEC,Tmin=Tmin,Pmax=Pmax,dzmax=dzmax,&
       filename=filename,dlns_max=dlnsMax,res=res,nRes=nRes,Pmin=Pmin)

  nLLE = nRes(1)
  nL1VE = nRes(2)
  nL2VE = nRes(3)

  do i=1,nLLE
     xLLE(i) = res(1,i)
     wLLE(i) = res(2,i)
     pLLE(i) = res(3,i)
  enddo
  do i=1,nL1VE
     xL1VE(i) = res(4,i)
     yL1VE(i) = res(5,i)
     pL1VE(i) = res(6,i)
  enddo
  do i=1,nL2VE
     xL2VE(i) = res(7,i)
     yL2VE(i) = res(8,i)
     pL2VE(i) = res(9,i)
  enddo
end subroutine VLLEBinaryPxy

subroutine two_phase_property(nc,T,P,z,x,y,nph,ph,betaV,betaL,id,prop)
  ! Calculate:
  ! Joule-Thompson coefficient (JTC)
  ! Heat capacity (CP,DHDT)
  ! Compressibillity (DVDP)
  ! Speed of sound (SOS)
  use parameters, only: TWOPH, LIQPH, SINGLEPH
  use stringmod, only: str_upcase
  use speed_of_sound, only: sound_velocity_2ph
  use state_functions, only: getJouleThompsonCoeff, dhdt_twoPhase, &
       dvdp_twoPhase, dpdt_twoPhase
  use eos, only: enthalpy, specificVolume
  ! Input:
  implicit none
  integer,   intent(in)      :: nc     !< Number of components
  real,      intent(in)      :: T      !< Temperature [K]
  real,      intent(in)      :: P      !< Pressure [Pa]
  real,      intent(in)      :: z(nc)  !< Molar composition [-]
  real,      intent(in)      :: x(nc)  !< Molar composition [-]
  real,      intent(in)      :: y(nc)  !< Molar composition [-]
  integer,   intent(in)      :: nph    !< Number of phases present in mixt.
  integer*4, intent(in)      :: ph(2)  !< Phase kind (liq or vap)
  real,      intent(in)      :: betaV  !< Molar phase fraction [-]
  real,      intent(in)      :: betaL  !< Molar phase fraction [-]
  character(len=*), intent(in) :: id  !< What property do we want
  ! Output:
  real,     intent(out)     :: prop   !< Thermodynamical property
  ! Locals
  character(len=len_trim(id)) :: id_upper
  real :: h, v
  integer :: lph(2), phase
  lph = ph
  if (lph(1) == SINGLEPH) then
     lph(1) = LIQPH
  endif
  if (lph(2) == SINGLEPH) then
     lph(2) = LIQPH
  endif
  if (nph == 2) then
     phase = TWOPH
  else
     phase = lph(1)
  endif
  !
  id_upper = trim(id)
  call str_upcase(id_upper)
  if (id_upper == 'DHDT' .OR. id_upper == 'CP') then
     ! Heat capacity. Enthalpy differential vrpt. temperature
     ! at constant pressure [J/mol/K]
     if (phase == TWOPH) then
        prop = dhdt_twoPhase(t,p,Z,betaV,betaL,X,Y,lph)
     else
        call enthalpy(t,p,z,phase,h,dhdt=prop)
     endif
  else if (id_upper == 'DVDP') then
     ! Compressibillity at constant temperature [m3/mol/Pa]
     if (phase == TWOPH) then
        prop = dvdp_twoPhase(t,p,Z,betaV,betaL,X,Y,lph)
     else
        call specificVolume(t,p,z,phase,v,dvdp=prop)
     endif
  else if (id_upper == 'SOS') then
     ! Speed of sound [m/s]
     prop = sound_velocity_2ph(t,p,X,Y,Z,betaV,betaL,phase,lph)
  else if (id_upper == 'JTC') then
     !Joule-Thompson coeff (dTdP constant h) [K/Pa]
     prop = getJouleThompsonCoeff(t,p,Z,betaV,betaL,X,Y,phase,lph)
  else if (id_upper == 'DPDT') then
     ! Thermal pressure change (dPdT constant v) [Pa/K]
     prop = dpdt_twoPhase(t,p,Z,betaV,betaL,X,Y,phase,lph)
  endif

end subroutine Two_phase_property

subroutine ideal_enthalpy(nc,T,h,dhdt)
  ! Get ideal enthalpy and heat capcity
  use eos, only: idealEnthalpySingle
  ! Input:
  implicit none
  integer,  intent(in)      :: nc       !< Number of components
  real,     intent(in)      :: T        !< Temperature [K]
  ! Output:
  real,     intent(out)     :: h(nc)    !< Ideal enthalpy
  real,     intent(out)     :: dhdt(nc) !< Ideal heat capacity
  ! Locals
  real :: p
  integer :: i
  do i=1,nc
    call idealEnthalpySingle(t,p,i,h(i),dhdt(i))
  enddo
end subroutine Ideal_enthalpy

subroutine set_alpha_corr(i, numparam, corrname, c)
  use cbAlpha, only: setSingleAlphaCorr
  use tpvar, only: cbeos
  integer, intent(in)           :: i
  character(len=*), intent(in)  :: corrname
  integer, intent(in)           :: numparam
  real, intent(in)              :: c(numparam)

  call setSingleAlphaCorr(i=i, cbeos=cbeos(1), &
       corrName=corrname, alphaParams=c)
end subroutine set_alpha_corr

subroutine get_alpha_corr(i, numparam, corrName, C)
  use stringmod, only: str_eq
  use tpvar, only: cbeos, comp
  use cbAlpha, only: getSingleAlphaCorr
  integer, intent(in)           :: i
  character(len=*), intent(in)  :: corrName
  integer, intent(in)           :: numparam
  real, intent(out)             :: C(numparam)
  real :: acfi

  call getSingleAlphaCorr(i=i, cbeos=cbeos(1), &
       corrName=corrName, numparam=numparam, alphaParams=C)


  ! if (str_eq(corrName,"Classic")) then
  !   acfi = comp(i)%acf
  !   C(1) = cbeos(1)%alfa + cbeos(1)%beta*acfi - cbeos(1)%gamma*acfi**2
  ! end if

end subroutine get_alpha_corr

!> Calculate cubic alpha function and temperature differentials
subroutine calc_cubic_alpha(ic,nc,T,a,dadt,d2adt2)
  use tpvar, only: cbeos, comp
  use cbAlpha, only: cbCalcAlphaTerm
  integer, intent(in)           :: ic, nc
  real, intent(out)             :: a,dadt,d2adt2
  !
  call cbCalcAlphaTerm(nc,comp,cbeos(1),T)
  a = cbeos(1)%single(ic)%alpha
  dadt = cbeos(1)%single(ic)%dalphadt
  d2adt2 = cbeos(1)%single(ic)%d2alphadt2
end subroutine calc_cubic_alpha

subroutine get_cubic_a_b(ic,a,b)
  use tpvar, only: cbeos
  integer, intent(in) :: ic
  real, intent(out) :: a,b

  a = cbeos(1)%single(ic)%a ! Pa*L^2/mol^2.
  b = cbeos(1)%single(ic)%b ! L/mol

  !print *, cbeos(1)%alfa + cbeos(1)%beta*acfi - cbeos(1)%gamma*acfi**2

end subroutine get_cubic_a_b

!> Beware when using for mixtures; it doesn't update mixture parameters
subroutine set_cubic_a_b(ic,a,b)
  use tpvar, only: cbeos
  integer, intent(in) :: ic
  real, intent(in) :: a,b

  cbeos(1)%single(ic)%a = a ! Pa*L^2/mol^2.
  cbeos(1)%single(ic)%b = b ! L/mol

end subroutine set_cubic_a_b

subroutine set_cubic_peneloux_param(ic,param)
  use tpvar, only: comp
  !use cbAlpha, only: setSingleAlphaCorr
  integer, intent(in) :: ic
  real, intent(in) :: param
  comp(ic)%ci = param
end subroutine set_cubic_peneloux_param

subroutine get_cubic_peneloux_param(ic,param)
  use tpvar, only: comp
  !use cbAlpha, only: setSingleAlphaCorr
  integer, intent(in) :: ic
  real, intent(out) :: param
  param = comp(ic)%ci
end subroutine get_cubic_peneloux_param

subroutine rho_meta_extremum (nc,T,x,phase,rho)
  use critical, only: rho_of_meta_extremum
  ! Input:
  integer, intent(in) :: nc                      !< Number of components
  real, intent(in) :: T                          !< [K]
  real, intent(in) :: x(nc)                      !< Composition [-]
  integer, intent(in) :: phase                   !< Phase flag. VAPPH or LIQPH
  ! Output:
  real, intent(out) :: rho                       !< [mol/m^3]

  rho = rho_of_meta_extremum (T=T,x=x,phase=phase)
end subroutine rho_meta_extremum

  !> Calculate the acentric factor from the EoS.
subroutine eosAcentricFac(i, acf, ierr)
  use saturation, only: acentricFactorEos
  ! Input
  integer, intent(in) :: i !< Component index
  real, intent(out) :: acf !< Acentric factor [-]
  integer, intent(out) :: ierr !< Error flag; 0 iff success
  acf = acentricFactorEos(i, ierr)
end subroutine eosAcentricFac

subroutine redefine_fallback_TcritPcritAcf(nc,tc,pc,acf)
  use tpselect, only: redefine_fallback_TcPcAcf
  ! Input
  integer, intent(in) :: nc !< number of components
  real, intent(in) :: tc(nc) !< specified critical temperature [K]
  real, intent(in) :: pc(nc) !< specified critical pressure [Pa]
  real, intent(in) :: acf(nc) !< specified acentric factor [-]

  call redefine_fallback_TcPcAcf(tc, pc, acf)
end subroutine redefine_fallback_TcritPcritAcf

subroutine redefine_fallback_TcritPcrit(nc,tc,pc)
  use tpselect, only: redefine_fallback_TcPcAcf
  ! Input
  integer, intent(in) :: nc !< number of components
  real, intent(in) :: tc(nc) !< specified critical temperature [K]
  real, intent(in) :: pc(nc) !< specified critical pressure [Pa]

  call redefine_fallback_TcPcAcf(tc, pc)
end subroutine redefine_fallback_TcritPcrit

subroutine redefine_comp_crit_state(nc,tc,pc,vc)
  use tpconst, only: Rgas
  use tpvar, only: comp
  ! Input
  integer, intent(in) :: nc !< number of components
  real, intent(in) :: tc(nc) !< specified critical temperature [K]
  real, intent(in) :: pc(nc) !< specified critical pressure [Pa]
  real, intent(in) :: vc(nc) !< specified critical volume [m3/mol]
  ! Locals
  integer :: i
  do i=1,nc
    comp(i)%Tc = tc(i)
    comp(i)%Pc = pc(i)
    comp(i)%Zc = vc(i)*pc(i)/(tc(i)*Rgas)
  enddo
end subroutine redefine_comp_crit_state

subroutine redefine_comp_cubic(j, Tc, Pc, Acf, vL,ierr)
  use tpselect, only: redefine_TcPcAcf_comp_cubic
  use volume_shift, only: redefine_volume_shift
  use tpvar, only: comp
  use parameters, only: nc, LIQPH
  use eos, only: specificvolume
  use saturation, only: safe_bubP
  ! Input
  integer, intent(in) :: j !< component
  integer, intent(out) :: ierr !< error flag
  real, intent(in) :: tc !< specified critical temperature [K]
  real, intent(in) :: pc !< specified critical pressure [Pa]
  real, intent(in) :: acf !< specified acentric factor [-]
  real, intent(in) :: vL !< saturated liquid volume at Tr=0.7 (ignored if vL <= 0) [m3/mol]
  ! Locals
  real :: T, X(nc), Y(nc), P, vLs
  call redefine_TcPcAcf_comp_cubic(j,Tc, Pc, Acf, ierr)
  if (vL > 0 .and. ierr == 0) then
    T = 0.7*Tc
    X = 0.0
    X(j) = 1.0
    P = safe_bubP(T,X,Y,ierr)
    call specificvolume(T,P,X,LIQPH,vLs)
    call redefine_volume_shift(nc,j,comp,vLs*1e3,vL*1e3)
  endif
end subroutine redefine_comp_cubic

subroutine set_volume_shift(j, vLcurrent, vLexp)
  use volume_shift, only: redefine_volume_shift
  use tpvar, only: comp
  ! Input
  integer, intent(in) :: j !< component
  real, intent(in) :: vLcurrent !< specific volume with current ci [m3/mol]
  real, intent(in) :: vLexp !< experimental volume [m3/mol]
  !
  call redefine_volume_shift(nc,j,comp,vLcurrent*1e3,vLexp*1e3)
end subroutine set_volume_shift

subroutine crit_tpv_thermopack_database(i, Tc, Pc, vc)
  ! Retrieve critical temperature, pressure and volume for a
  ! component.
  !
  use parameters, only: nc
  use eos, only: getCriticalParam
  ! Input:
  integer, intent(in)  :: i        ! Component number
  ! Output:
  real,   intent(out)  :: Tc   ! Crit. temperatures (K)
  real,   intent(out)  :: Pc   ! Crit. pressures (Pa)
  real,   intent(out)  :: vc   ! Crit. volume (m3/mol)
  ! Internal:
  real                 :: acfi
  if (i<0 .or. i>nc) then
    call stoperror("crit_tpv_thermopack_database::i out of bounds")
  end if
  call getCriticalParam(i=i, tci=Tc, pci=Pc, oi=acfi, vci=vc)
end subroutine crit_tpv_thermopack_database

subroutine get_rgas(Rgas_out)
  ! Get system Rgas
  !
  use tpconst, only: Rgas
  ! Output:
  real,   intent(out)  :: Rgas_out ! Gas constant used in thermopack (J/K/mol)
  !
  Rgas_out = Rgas
end subroutine get_rgas

subroutine parameter_report()
  ! Prints all details of the active model.
  !
  use eosdata
  use csp, only: mbwrRefEos
  use tpvar, only: cbeos, nce, comp
  use saft_interface, only: printBinaryMixtureReportSaft
  use saftvrmie_options, only: enable_hs_extra, quantum_correction, &
       quantum_correction_hs, quantum_correction_spec, quantum_correct_A2, &
       use_epsrule_lafitte,  exact_binary_dhs, exact_crosspot_eff, a3_model, &
       hardsphere_eos
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var, get_saftvrmie_pure_fluid_deBoer
  use pc_saft_nonassoc, only: m, sigma, eps_depth_divk
  use assocschemeutils, only: numAssocSites
  use saft_association, only: eps_kl, beta_kl
  real :: acfi, deboer
  integer :: i
  print *, ' '
  print *, '****************************** MODEL REPORT ***********************************'

  print *, "eos:   ", cbeos(1)%name
  print *, "mrule: ", cbeos(1)%mruleid
  do i=1,nce
     write(*,'(A5,I2,A1,A12)') "comp", i, ":", comp(i)%ident
  end do
  if (cbeos(1)%eosidx==eosBH_pert) then
     call get_saftvrmie_pure_fluid_deBoer(1, deboer)
     print *, "m          ", saftvrmie_param%ms
     print *, "lambda_r   ", saftvrmie_param%lambda_r_ij
     print *, "lambda_a   ", saftvrmie_param%lambda_a_ij
     print *, "epsilon    ", saftvrmie_param%eps_divk_ij
     print *, "epsiloneff ", saftvrmie_var(1)%eps_divk_eff%d
     print *, "sigma      ", saftvrmie_param%sigma_ij
     print *, "sigmaeff   ", saftvrmie_var(1)%sigma_eff%d
     print *, "d_bh       ", saftvrmie_var(1)%dhs%d
     print *, "mass       ", saftvrmie_param%comp(:)%mass
     print *, "quantum_correction     ", quantum_correction
     print *, "quantum_correction_hs  ", quantum_correction_hs
     print *, "quantum_correction_spec", quantum_correction_spec
     print *, "use_epsrule_Lafitte    ", use_epsrule_Lafitte
     print *, "exact_binary_dhs       ", exact_binary_dhs
     print *, "exact_crosspot_eff     ", exact_crosspot_eff
     print *, "enable_hs_extra        ", enable_hs_extra
     print *, "a3_model               ", a3_model
     print *, "hardsphere_eos         ", hardsphere_eos
     print *, "de Boer parameter      ", deboer
     print *, "alpha_eff   ", saftvrmie_var(1)%alpha%d
     print *, "alpha_param ", saftvrmie_param%alpha_ij
     print *, "kij         ", saftvrmie_param%kij
     print *, "lij         ", saftvrmie_param%lij

  else
    if (nce==1) then
      select case (cbeos(1)%eosidx)
      case (cpaSRK, cpaPR, cbPR, cbSRK)
        write(*,'(A,2ES12.4)'), "a(Pa*L^2/mol^2), b(L/mol) : ", cbeos(1)%single(1)%a, cbeos(1)%single(1)%b
        write(*,'(2A,3ES12.4)'), "alpha: ", cbeos(1)%single(1)%alphaCorrName, cbeos(1)%single(1)%alphaParams
        acfi = cbeos(1)%single(1)%acf
        print *, "effective c1 value", cbeos(1)%alfa + cbeos(1)%beta*acfi - cbeos(1)%gamma*acfi**2
     case (eosPC_SAFT)
        !print *, "saft_setno     ", saft_setno
        print *, "m              ", m
        print *, "sigma          ", sigma
        print *, "eps_deptk_divk ", eps_depth_divk
        print *, "numAssocSites  ", numAssocSites
        if (numAssocSites>0) then
           print *, "betal_kl       ", beta_kl
           print *, "eps_kl         ", eps_kl
        end if
       end select
    else if (nce==2) then
      select case (cbeos(1)%eosidx)
      case (cpaSRK, cpaPR, eosPC_SAFT)
        call printBinaryMixtureReportSaft()

      case (cbSRK, cbSRKGB, cbPR,  cspSRK, cspSRKGB, cspPR)
        write(*,'(2A,3ES12.4)'), "alpha 1: ", cbeos(1)%single(1)%alphaCorrName, cbeos(1)%single(1)%alphaParams
        write(*,'(2A,3ES12.4)'), "alpha 2: ", cbeos(1)%single(2)%alphaCorrName, cbeos(1)%single(2)%alphaParams

        select case (cbeos(1)%mruleidx)
        case (cbMixClassic)
          write(*,'(A,ES12.4)'), "kij: ", cbeos(1)%kij(1,2)
        case (cbMixHuronVidal, cbMixHuronVidal2)
          write(*,'(A, 2ES12.4)'), "alpha12, alpha21: ", cbeos(1)%MixGE%alpha(1,2), cbeos(1)%MixGE%alpha(2,1)
          write(*,'(A, 3ES12.4)'), "aGE12, bGE12, cGE12: ", &
               cbeos(1)%MixGE%aGE(1,2), cbeos(1)%MixGE%bGE(1,2), cbeos(1)%MixGE%cGE(1,2)
          write(*,'(A, 3ES12.4)'), "aGE21, bGE21, cGE21: ", &
               cbeos(1)%MixGE%aGE(2,1), cbeos(1)%MixGE%bGE(2,1), cbeos(1)%MixGE%cGE(2,1)
        case (cbMixWongSandler)
          write(*,'(A,4ES12.4)'), "alpha, k, tau12, tau21: ", cbeos(1)%MixWS%alphaij(1,2), &
               cbeos(1)%MixWS%f_kij(1,2)%pNum(1), cbeos(1)%MixWS%f_tauij(1,2)%pNum(1), cbeos(1)%MixWS%f_tauij(2,1)%pNum(1)
        end select

        if (cbeos(1)%eosidx == cspSRK .or. cbeos(1)%eosidx == cspSRKGB .or. cbeos(1)%eosidx == cspPR) then
          print *, "Reference equation: ", mbwrRefEos(1)%name
          print *, "Reference component: ", mbwrRefEos(1)%compId
        end if

      case default
        print *, "No report implemented for the eos: ", cbeos(1)%name
      end select
    end if
  end if

  print *, '*******************************************************************************'
  print *, ' '

end subroutine parameter_report


subroutine set_ph_tolerance(tol)
  ! Set tolerance of PH-flash
  use ph_solver, only: setPHtolerance
  implicit none
  ! Input:
  real, intent(in) :: tol ! Tolerance (-)
  !
  call setPHtolerance(tol)
end subroutine set_ph_tolerance

subroutine component_index(name,index)
  ! Return compositon index give component name
  use parameters, only: compIndex
  use stringmod, only: str_upcase
  implicit none
  character(len=*), intent(in) :: name  !< Component name
  ! Output:
  integer*4, intent(out) :: index ! Component index
  ! Locals
  character(len=len(name)) :: name_up
  name_up = name
  call str_upcase(name_up)
  index = compIndex(name_up) - 1
end subroutine component_index

subroutine set_g_tolerance(tol)
  ! Set tolerance of PH-flash
  use tp_solver, only: g_tolerance
  implicit none
  ! Input:
  real, intent(in) :: tol ! Tolerance (-)
  !
  g_tolerance = tol
end subroutine set_g_tolerance

! Calculate virial coeffcients
subroutine virial_coeffcients(nc,T,n,B,C)
  use eosTV, only : virial_coefficients
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: n(nc) !< Phase mole numbers
  real, intent(in) :: T !< Temperature  [K]
  ! Output
  real, intent(out) :: B !< Second virial coeffcient [m3/mol]
  real, intent(out) :: C !< Third virial coeffcient [m6/mol2]
  call virial_coefficients(t,n,B,C)
end subroutine virial_coeffcients

! Calculate virial coeffcients
subroutine second_virial_matrix(nc,T,bmat)
  use eosTV, only: secondVirialCoeffMatrix
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: T !< Temperature  [K]
  ! Output
  real, intent(out) :: bmat(nc,nc) !< Second virial coeffcient matrix [m3/mol]
  call secondVirialCoeffMatrix(t,bmat)
end subroutine second_virial_matrix

! Calculate virial coeffcients
subroutine binary_third_virial_matrix(nc,T,cmat)
  use eosTV, only: binaryThirdVirialCoeffMatrix
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: T !< Temperature  [K]
  ! Output
  real, intent(out) :: cmat(nc,nc) !< Third virial coeffcient matrix [m6/mol2]
  ! Locals
  !real :: C_tmp
  call binaryThirdVirialCoeffMatrix(T,cmat)
  ! Do we need to remap memory to c-style?....
end subroutine binary_third_virial_matrix

! Set minimum temperature for numerical methods in thermopack
subroutine set_tmin(Tmin)
  use tpconst, only : tpTmin
  implicit none
  ! Input
  real, intent(in) :: Tmin !< Temperature  [K]
  !
  tpTmin = Tmin
end subroutine set_tmin

! Set minimum temperature for numerical methods in thermopack
subroutine set_pmin(Pmin)
  use tpconst, only : tpPmin
  implicit none
  ! Input
  real, intent(in) :: Pmin !< Pressure [Pa]
  !
  tpPmin = Pmin
end subroutine set_pmin

subroutine pure_fluid_saturation_line(p_init,maxDeltaP,T_out,p_out,nvals)
  ! Get saturation line starting at p_init.
  ! The output is arrays T_out and p_out, which are filled with nvals values.
  !
  use saturation_curve, only: singleCompSaturation
  use parameters, only: nc
  implicit none
  ! Input:
  integer, parameter :: nmax = 1000
  real,       intent(in)    :: p_init
  real,       intent(in)    :: maxDeltaP
  ! Output:
  real,       intent(out)   :: T_out(nmax)
  real,       intent(out)   :: p_out(nmax)
  integer,    intent(out)   :: nvals
  ! Internal:
  real    :: z(nc)
  real    :: T_init, p_in

  if (.not.(nc==1)) call stoperror("pure_fluid_saturation_line applicable to pure fluids")

  z = 1.0
  T_init = 0.0
  p_in = p_init
  call singleCompSaturation(Z,T_init,p_in,1,T_out,p_out,nmax,nvals,maxDeltaP)

end subroutine pure_fluid_saturation_line

subroutine pure_fluid_saturation_line_tspec(t_init,maxDeltaP,T_out,p_out,nvals)
  ! Get saturation line starting at t_init.
  ! The output is arrays T_out and p_out, which are filled with nvals values.
  !
  use saturation_curve, only: singleCompSaturation
  use parameters, only: nc
  implicit none
  ! Input:
  integer, parameter :: nmax = 3500
  real,       intent(in)    :: t_init
  real,       intent(in)    :: maxDeltaP
  ! Output:
  real,       intent(out)   :: T_out(nmax)
  real,       intent(out)   :: p_out(nmax)
  integer,    intent(out)   :: nvals
  ! Internal:
  real    :: z(nc)
  real    :: p_in, t_in

  if (.not.(nc==1)) call stoperror("pure_fluid_saturation_line applicable to pure fluids")

  z = 1.0
  t_in = t_init
  p_in = 0.0
  call singleCompSaturation(z,t_in,p_in,2,T_out,p_out,nmax,nvals,maxDeltaP)

end subroutine pure_fluid_saturation_line_tspec

subroutine activate_rub_density_solver(activate)
  ! Set falg to cativate/deactivate RUB density solver
  !
  use trend_solver, only: useRUBdensitySolver
  implicit none
  ! Input:
  logical, intent(in) :: activate
  !
  useRUBdensitySolver = activate
end subroutine activate_rub_density_solver

subroutine pure_sat_line(nc,z,p_start,T_out,p_out)
  !> Calculate saturation line as if the
  !! mixture was a single component (i.e. both phases are assumed to have
  !! composition Z).
  ! The output is arrays T_out and p_out, which are filled with nvals values.
  !
  use puresaturation, only: PureSatLine
  implicit none
  ! Output length:
  integer, parameter        :: nmax = 100
  ! Input:
  integer,    intent(in)    :: nc
  real,       intent(in)    :: z(nc)
  real,       intent(in)    :: p_start
  ! Output:
  real,       intent(out)   :: T_out(nmax)
  real,       intent(out)   :: p_out(nmax)

  call PureSatLine(P_start,Z,T_out,P_out,nmax)

end subroutine pure_sat_line

subroutine rr_successive_substitution_iteration_two_phase(nc,Z,T,P,K_in,phaseY,beta,betaL,X,Y,K,converged,rr_has_solution)
  !> Do one iteration of successive substitution. First, solve the Rachford-
  !! Rice equation for the given Z and K. Then, update K.
  use tp_solver, only: rr_successive_substitution_iteration
  implicit none
  ! Input:
  integer,    intent(in)    :: nc
  real,       intent(in)    :: z(nc)
  real,       intent(in)    :: p
  real,       intent(in)    :: T
  real,       intent(in)    :: K_in(nc)
  integer,    intent(in)    :: phaseY
  ! Output:
  real,       intent(out)   :: beta
  real,       intent(out)   :: betaL
  real,       intent(out)   :: K(nc)
  real,       intent(out)   :: X(nc)
  real,       intent(out)   :: Y(nc)
  logical,    intent(out)   :: converged
  logical,    intent(out)   :: rr_has_solution
  ! Locals:
  real :: g_simp, FUGL(nc), FUGV(nc)

  call rr_successive_substitution_iteration(T,P,Z,K_in,.false., &
       X,Y,beta,K,FUGL,FUGV,g_simp,converged,rr_has_solution,betaL,phaseY=phaseY)

end subroutine rr_successive_substitution_iteration_two_phase

subroutine thermo_no_derivs(nc, T, P, z, iphase, lnfug)
  ! Calculates logarithm of fugacity coefficient.
  use eos , only: thermo
  ! Input
  integer, intent(in) :: nc
  real,    intent(in) :: T
  real,    intent(in) :: P
  real,    intent(in) :: z(nc)
  integer, intent(in) :: iphase
  ! Output
  real,    intent(out) :: lnfug(nc)

  call thermo(T, P, z, iphase, lnfug)

end subroutine thermo_no_derivs
