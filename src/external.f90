! File with all routines intended to be called from external applications
! linking to .a library file.
!
! Prefix all routines with "thermopack_".
!
! No routines should be contained in modules, to avoid having to distribute
! .mod files.


!-----------------------------------------------------------------!
! C utility functions
!-----------------------------------------------------------------!
module C_interface_module
  use, intrinsic :: ISO_C_BINDING
  public
  !> Interface to std C library function strlen
  interface
    pure function C_strlen(s) result(result) bind(C,name="strlen")
      import C_size_t, C_ptr
      integer(C_size_t) :: result
      type(C_ptr), intent(in) :: s  !character(len=*), intent(in)
    end function C_strlen
  end interface

contains
  function C_associated_pure(ptr) result(associated)
    implicit none
    type(C_ptr), target, intent(in) :: ptr
    logical :: associated
    ! Locals
    integer(C_intptr_t) :: iptr
    iptr = transfer(c_loc(ptr),iptr)
    associated = (iptr /= 0)
  end function C_associated_pure

  function C_strlen_safe(s) result(length)
    implicit none
    type(C_ptr), intent(in) :: s
    integer(C_size_t) :: length
    if (.not. C_associated_pure(s)) then
      length = 0
    else
      length = C_strlen(s)
    end if
  end function C_strlen_safe

  subroutine C_char_to_character_string(C_string, F_string)
    implicit none
    type(C_ptr), target, intent(in) :: C_string
    character(len=*) :: F_string
    ! Locals
    character, pointer :: F_string_a(:) !< Fortran representation of character arrays
    integer(C_size_t) :: c_len, i
    c_len = C_strlen_safe(C_string)
    F_string = ""
    if (c_len > 0) then
      if (c_len > len(F_string)) then
        call stoperror("C_char_to_character_string: Not able to copy string")
      endif
      call c_f_pointer(c_loc(C_string), F_string_a, [c_len])
      do i=1,c_len
        F_string(i:i) = F_string_a(i)
      enddo
    endif
  end subroutine C_char_to_character_string

end module C_interface_module

!> Interface to init accepting C types as arguments
!! \author MH, 2017-06
!! Optional argument should be supplied as NULL pointers
!-----------------------------------------------------------------!
subroutine thermopack_init_c(eos,mixing,alpha,comp_string,&
     nphases,liq_vap_discr_method_in,csp_eos,csp_ref_comp,kij_ref,alpha_ref,&
     saft_ref,b_exponent) BIND(C)
!-----------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eoslibinit, only: init_thermo
  use C_interface_module, only: C_strlen_safe, C_char_to_character_string
  use thermopack_constants, only: clen
  implicit none
  ! Required input
  type(C_ptr), target, intent(in) :: eos    !< String defining equation of state
  type(C_ptr), target, intent(in) :: mixing !< String defining mixing rules
  type(C_ptr), target, intent(in) :: alpha  !< String defining alpha correlation
  type(C_ptr), target, intent(in) :: comp_string    !< String defining components. Comma or white-space separated.
  integer(C_int), intent(in) :: nphases !< Number of phases

  ! Optional input (must send zero pointers if not available)
  integer(C_int), intent(in) :: liq_vap_discr_method_in !< Method to discriminate between liquid and vapor in case of an undefined single phase. Will be set to none if absent.
  type(C_ptr), target, intent(in) :: csp_eos !< Corrensponding state equation
  type(C_ptr), target, intent(in) :: csp_ref_comp !< CSP component
  type(C_ptr), target, intent(in) :: kij_ref, alpha_ref !< Data set numbers
  type(C_ptr), target, intent(in) :: saft_ref !< Data set numbers
  real(C_double), intent(in) :: b_exponent !< Inverse exponent (1/s) in mixing of covolume (s>1.0)

  ! Fortran representation of character strings
  character(len=clen) :: eos_f
  character(len=clen) :: mixing_f
  character(len=clen) :: alpha_f
  character(len=clen) :: comp_string_f
  character(len=:), pointer :: csp_eos_pf
  character(len=:), pointer :: csp_ref_comp_pf
  character(len=clen), target :: csp_eos_f
  character(len=clen), target :: csp_ref_comp_f
  character(len=clen), target :: kij_ref_f
  character(len=clen), target :: alpha_ref_f
  character(len=clen), target :: saft_ref_f
  character(len=clen), pointer :: kij_ref_pf
  character(len=clen), pointer :: alpha_ref_pf
  character(len=clen), pointer :: saft_ref_pf

  ! Length variable
  integer(C_size_t) :: c_len

  call C_char_to_character_string(eos, eos_f)
  call C_char_to_character_string(mixing, mixing_f)
  call C_char_to_character_string(alpha, alpha_f)
  call C_char_to_character_string(comp_string, comp_string_f)

  c_len = C_strlen_safe(csp_eos)
  if (c_len == 0) then
    csp_eos_pf => NULL()
  else
    call C_char_to_character_string(csp_eos, csp_eos_f)
    csp_eos_pf => csp_eos_f
  endif
  c_len = C_strlen_safe(csp_ref_comp)
  if (c_len == 0) then
    csp_ref_comp_pf => NULL()
  else
    call C_char_to_character_string(csp_ref_comp, csp_ref_comp_f)
    csp_ref_comp_pf => csp_ref_comp_f
  endif
  !
  c_len = C_strlen_safe(kij_ref)
  if (c_len == 0) then
    kij_ref_pf => NULL()
  else
    call C_char_to_character_string(kij_ref, kij_ref_f)
    kij_ref_pf => kij_ref_f
  endif
  !
  c_len = C_strlen_safe(alpha_ref)
  if (c_len == 0) then
    alpha_ref_pf => NULL()
  else
    call C_char_to_character_string(alpha_ref, alpha_ref_f)
    alpha_ref_pf => alpha_ref_f
  endif
  !
  c_len = C_strlen_safe(saft_ref)
  if (c_len == 0) then
    saft_ref_pf => NULL()
  else
    call C_char_to_character_string(saft_ref, saft_ref_f)
    saft_ref_pf => saft_ref_f
  endif

  call init_thermo(eos_f,mixing_f,alpha_f,comp_string_f,nphases,&
       liq_vap_discr_method_in,csp_eos_pf,csp_ref_comp_pf,kij_ref_pf,alpha_ref_pf,&
       saft_ref_pf,b_exponent)

end subroutine thermopack_init_c


!-----------------------------------------------------------------!
subroutine thermopack_TPflash_c(T,P,x_overall,beta,phase,comp_liquid,comp_vapor) BIND(C)
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use tp_solver, only: twoPhaseTPflash
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), intent(in)          :: T,P,x_overall(nc)
  ! Output:
  integer(C_int), intent(out)      :: phase
  real(C_double), intent(out)         :: beta
  real(C_double), intent(out)         :: comp_liquid(nc),comp_vapor(nc)
  ! Internal:
  real :: betaL
  ! Phase flags:
  ! TWOPH=0,LIQPH=1,VAPPH=2,MINGIBBSPH=3,SINGLEPH=4,LLV=5,LL=6


  !write(*,*) "--------------------------"
  !write(*,*) "Running thermopack TPflash"
  call twoPhaseTPflash(T,P,x_overall,beta,betaL,phase,comp_liquid,comp_vapor)
  !write(*,*) "-Done---------------------"

  !write(*,*) "beta = ",beta
  !write(*,*) "phase = ",phase
  !write(*,*) "comp_liquid = ",comp_liquid
  !write(*,*) "comp_vapor = ",comp_vapor

end subroutine thermopack_TPflash_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_pressure_c(t,rho,z,p) BIND(C)
!-----------------------------------------------------------------!
  !> External interface for the pressure.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-08-13
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use thermopack_var, only: nc
  use eosTV,      only: pressure
  implicit none
  ! Input:
  real(C_double),                intent(in)  :: t   !< K      - Temperature
  real(C_double),                intent(in)  :: rho !< mol/m3 - Molar density
  real(C_double), dimension(nc), intent(in)  :: z   !<          Composition
  ! Output:
  real(C_double),                intent(out) :: p   !< Pa     - Pressure
  !---------------------------------------------------------------!
  ! Thermopack
  p=pressure(t,1.0/rho,z)
end subroutine thermopack_pressure_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_UVflash_c(t,p,z,beta,x,y,uspec,vspec,iphase,ierr) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the UV-flash routine.
  !>
  !> Note: This routine only supports two-phase flash at the
  !>       moment. We are also restricted to the nested loop
  !>       algorithm.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-09-05
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use uv_solver,  only: twoPhaseUVflash,twoPhaseUVflashNested
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), dimension(nc), intent(in)    :: z      !< Composition
  real(C_double),                intent(in)    :: uspec  !< J/mol  - Specified internal energy
  real(C_double),                intent(in)    :: vspec  !< m3/mol - Specified molar volume
  ! Output:
  integer(C_int),             intent(inout) :: iphase !< Phase flag
  real(C_double),                intent(inout) :: t      !< Temperature
  real(C_double),                intent(inout) :: p      !< Pressure
  real(C_double),                intent(inout) :: beta   !< Vapour mole fraction
  real(C_double), dimension(nc), intent(inout) :: x      !< Liquid composition
  real(C_double), dimension(nc), intent(inout) :: y      !< Vapour composition
  integer(C_int), intent(out) :: ierr
  !---------------------------------------------------------------!
  real :: betaL
  logical :: isConverged
  ! call twoPhaseUVflash(t,p,z,beta,x,y,uspec,vspec,iphase)
  call twoPhaseUVflashNested(t,p,z,beta,betaL,x,y,uspec,vspec,iphase,isConverged)
  if (isConverged) then
    ierr = 0
  else
    ierr = 1
  endif
  !
end subroutine thermopack_UVflash_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_HPflash_c(t,p,Z,beta,betaL,X,Y,hspec,phase,ierr) BIND(C)
  !----------------------------------------------------------------!
  !> External interface for the HP-flash routine.
  !>
  !>
  !> --------------------------------------------------------------!
  !> \author HHU, 2014-06-19
  !----------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use ph_solver,  only: twoPhasePHflash
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), intent(inout) :: beta !< Vapour phase molar fraction [-]
  real(C_double), intent(out) :: betaL !< Liquid phase molar fraction [-]
  real(C_double), dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
  real(C_double), dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
  real(C_double), dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
  real(C_double), intent(inout) :: t !< Temperature [K]
  real(C_double), intent(in) :: p !< Pressure [Pa]
  real(C_double), intent(in) :: hspec !< Specified enthalpy [J/mol]
  integer(C_int), intent(inout) :: phase !< Phase identifier
  integer(C_int), intent(out) :: ierr
  !---------------------------------------------------------------!
  call twoPhasePHflash(t,p,Z,beta,betaL,X,Y,hspec,phase,ierr)

end subroutine thermopack_HPflash_c

 !-----------------------------------------------------------------!
subroutine thermopack_SPflash_c(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr) BIND(C)
 !-----------------------------------------------------------------!
  !> External interface for the SP-flash routine.
  !>
  !>
  !----------------------------------------------------------------!
  !> \author HHU, 2014-06-19
  !----------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use ps_solver,  only: twoPhasePSflash
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), intent(inout) :: beta !< Vapour phase molar fraction [-]
  real(C_double), intent(out) :: betaL !< Liquid phase molar fraction [-]
  real(C_double), dimension(nc), intent(in) :: Z !<Overall molar composition [-]
  real(C_double), dimension(nc), intent(inout) :: X !<Liquid molar composition [-]
  real(C_double), dimension(nc), intent(inout) :: Y !<Vapour molar composition [-]
  real(C_double), intent(inout) :: t !< Temperature [K]
  real(C_double), intent(in) :: p !< Pressure [Pa]
  real(C_double), intent(in) :: sspec !<Specified entropy [J/mol*K]
  integer(C_int), intent(inout) :: phase !< Phase identifier
  integer(C_int), intent(out) :: ierr
  !----------------------------------------------------------------!
  call twoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase,ierr)

end subroutine thermopack_SPflash_c
 !-----------------------------------------------------------------!

!----------------------------------------------------------------!
subroutine thermopack_bubT_c(P,X,Y,Tbub,ierr) BIND(C)
  !--------------------------------------------------------------!
  !> External interface for the bubble point temperature routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !--------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use saturation
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), dimension(nc), intent(in) :: X
  real(C_double), dimension(nc), intent(out) :: Y
  real(C_double), intent(in) :: P
  real(C_double), intent(out):: Tbub
  integer(C_int), intent(out) :: ierr
  !--------------------------------------------------------------!
  Tbub = safe_bubT(P,X,Y,ierr)
  !--------------------------------------------------------------!
end subroutine thermopack_bubT_c
!----------------------------------------------------------------!


!----------------------------------------------------------------!
subroutine thermopack_bubP_c(T,X,Y,Pbub,ierr) BIND(C)
  !--------------------------------------------------------------!
  !> External interface for the bubble point pressure routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !--------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use saturation
  use thermopack_var, only: nc
  implicit none
  ! Input:
   real(C_double), dimension(nc), intent(in) :: X
   real(C_double), dimension(nc), intent(out) :: Y
   real(C_double), intent(in) :: T
   real(C_double), intent(out) :: Pbub
   integer(C_int), intent(out) :: ierr
   !--------------------------------------------------------------!
   Pbub = safe_bubP(T,X,Y,ierr)
   !--------------------------------------------------------------!
 end subroutine thermopack_bubP_c
 !----------------------------------------------------------------!


 !----------------------------------------------------------------!
subroutine thermopack_dewT_c(P,X,Y,Tdew,ierr) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the dew point temperature routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use saturation
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), dimension(nc), intent(out) :: X
  real(C_double), dimension(nc), intent(in) :: Y
  real(C_double), intent(in) :: P
  real(C_double), intent(out) :: Tdew !< [K]
  integer(C_int), intent(out) :: ierr

  !---------------------------------------------------------------!
  Tdew = safe_dewT(P,X,Y,ierr)
  !---------------------------------------------------------------!
end subroutine thermopack_dewT_c
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_dewP_c(T,X,Y,Pdew,ierr) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the dew point pressure routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use saturation
  use thermopack_var, only:nc
  implicit none
  ! Input:
  real(C_double), dimension(nc), intent(out) :: X
  real(C_double), dimension(nc), intent(in) :: Y
  real(C_double), intent(in) :: T
  real(C_double), intent(out) :: Pdew !< Pa
  integer(C_int), intent(out) :: ierr

  !---------------------------------------------------------------!
  Pdew = safe_dewP(T,X,Y,ierr)
  !---------------------------------------------------------------!
end subroutine thermopack_dewP_c
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_zfac_c(t,p,x,phase,z) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the zfac routine
  !> in eos.f90
  !> Note: Optional arguments not included here
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  integer(C_int), intent(in) :: phase !< Phase identifyer
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), dimension(1:nc), intent(in) :: x !< Compozition
  real(C_double), intent(out) :: z !< Compressibillity factor
  !---------------------------------------------------------------!
  call zfac(t,p,x,phase,z)
  !---------------------------------------------------------------!
end subroutine thermopack_zfac_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_entropy_c(t,p,x,phase,s) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the entropy routine
  !> in eos.f90
  !> Notes: Optional arguments not included here.
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  integer(C_int), intent(in) :: phase !< Phase identifyer
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), dimension(1:nc), intent(in) :: x !< Compozition
  real(C_double), intent(out) :: s !< J/mol/K - Molar entropy
  !---------------------------------------------------------------!
  if(phase == 0) then
    write(*,*) "Called entropy with twophase"
  end if

  call entropy(t,p,x,phase,s)
  !---------------------------------------------------------------!
end subroutine thermopack_entropy_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_wilsonK_c(t,p,k) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the wilson K  routine
  !> in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use thermo_utils, only: wilsonK
  use thermopack_var, only: nc
  implicit none
  !Input:
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), dimension(nc), intent(out) :: k !< K-values
  !---------------------------------------------------------------!
  call wilsonK(t,p,k)
  !---------------------------------------------------------------!
end subroutine thermopack_wilsonK_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_getCriticalParam_c(i,tci,pci,oi) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the getCriticalParam
  !> routine in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eos
  implicit none
  !Input:
  integer(C_int), intent(in) :: i !< Component index
  real(C_double), intent(out) :: tci !< K - Critical temperature
  real(C_double), intent(out) :: pci !< Pa - Critical pressure
  real(C_double), intent(out) :: oi  !< Acentric factor
  !---------------------------------------------------------------!
  call getCriticalParam(i,tci,pci,oi)
  !---------------------------------------------------------------!
end subroutine thermopack_getCriticalParam_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_compMoleWeight_c(j,mw) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the compMoleWeight
  !> routine in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eos
  implicit none
  !input:
  integer(C_int), intent(in) :: j !< Component index
  real(C_double), intent(out) :: mw !< kg/mol - Mole weight
  !---------------------------------------------------------------!
  mw = 1e-3 * compMoleWeight(j)
  !---------------------------------------------------------------!
end subroutine thermopack_compMoleWeight_c
!-----------------------------------------------------------------!

subroutine get_phase_flags_c(iTWOPH,iLIQPH,iVAPPH,iMINGIBBSPH,&
     iSINGLEPH,iSOLIDPH,iFAKEPH) BIND(C)
  ! Get the internal integer flags for various phase states and
  ! specifications.
  use, intrinsic :: ISO_C_BINDING
  use thermopack_constants, only: TWOPH,LIQPH,VAPPH,MINGIBBSPH,SINGLEPH,SOLIDPH,FAKEPH
  implicit none
  ! Output:
  integer(C_int), intent(out) :: iTWOPH,iLIQPH,iVAPPH,iMINGIBBSPH,&
       iSINGLEPH,iSOLIDPH,iFAKEPH
  iTWOPH      = TWOPH
  iLIQPH      = LIQPH
  iVAPPH      = VAPPH
  iMINGIBBSPH = MINGIBBSPH
  iSINGLEPH   = SINGLEPH
  iSOLIDPH    = SOLIDPH
  iFAKEPH     = FAKEPH

end subroutine get_phase_flags_c

!-----------------------------------------------------------------!
subroutine thermopack_entropy_tv_c(t,v,n,s) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the entropy routine
  !> in eosTV.f90
  !> Notes: Optional arguments not included here.
  !> \author MH, 2019-04-15
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eosTV, only: entropy_tv
  use thermopack_var, only: nc
  implicit none
  !Input:
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: v !< m3 - Volume
  real(C_double), dimension(1:nc), intent(in) :: n !< Mol numbers
  real(C_double), intent(out) :: s !< J/K - Entropy
  !---------------------------------------------------------------!
  call entropy_tv(t,v,n,s)
  !---------------------------------------------------------------!
end subroutine thermopack_entropy_tv_c

!> Interface to enthalpy accepting C types as arguments
!! \author MH, 2019-04
!! Optional argument should be supplied as NULL pointers
!! External interface for the enhalpy routine
!! in eos.f90
!-----------------------------------------------------------------!
subroutine thermopack_enthalpy_c(t,p,x,phase,h,dhdt,dhdp,dhdx) BIND(C)
  use, intrinsic :: ISO_C_BINDING
  use eos, only: enthalpy
  use thermopack_var, only: nc
  implicit none
  ! Required input
  integer(C_int), intent(in) :: phase !< Phase identifyer
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), intent(in) :: x(nc) !< Compozition

  ! Output
  real(C_double), intent(out) :: h !< J/mol - Molar enthalpy
  real(C_double), intent(out) :: dhdt !< J/mol/K - Heat capacity
  real(C_double), intent(out) :: dhdp !< J/mol/Pa - Enthalpy pressure differential
  real(C_double), intent(out) :: dhdx(nc) !< J/mol/mol - Enthalpy mol number differentials

  call enthalpy(t,p,x,phase,h,dhdt,dhdp,dhdx)

end subroutine thermopack_enthalpy_c

subroutine thermopack_puresat_t_c(P,Z,T,ierr) BIND(C)
  use, intrinsic :: ISO_C_BINDING
  use puresaturation, only: puresat
  use thermopack_var, only: nc
  implicit none
  integer(C_int), intent(out) :: ierr !< Error flag
  real(C_double), intent(out) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), intent(in) :: z(nc) !< Compozition
  ! Locals
  real :: Pcopy
  Pcopy = P
  call PureSat(T,Pcopy,Z,.true.,ierr)
end subroutine thermopack_puresat_t_c

subroutine thermopack_guess_phase_c(T,P,Z,phase) BIND(C)
  use, intrinsic :: ISO_C_BINDING
  use thermo_utils, only: guessPhase
  use thermopack_var, only: nc
  implicit none
  integer(C_int), intent(out) :: phase !< phase flag
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), intent(in) :: z(nc) !< Compozition
  !
  phase = guessPhase(T,P,z)
  end subroutine thermopack_guess_phase_c

!-----------------------------------------------------------------!
subroutine thermopack_thermo_c(T,P,x,phase,lnfug,lnfugt,lnfugp,lnfugx,ophase) BIND(C)
!-----------------------------------------------------------------!
!
! phaseflag: LIQPH, VAPPH or MINGIBBSPH
!
  use, intrinsic :: ISO_C_BINDING
  use eos, only: thermo
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real(C_double), intent(in)          :: T,P,x(nc)
  integer(C_int), intent(in)          :: phase
  ! Output:
  real(C_double), dimension(1:nc), intent(out)     :: lnfug
  ! Optionals
  real(C_double), dimension(1:nc), intent(out)      :: lnfugt
  real(C_double), dimension(1:nc), intent(out)      :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
  real(C_double), dimension(1:nc,1:nc), intent(out) :: lnfugx !< Logarithm of fugasity coefficient differential wrpt. mole numbers
  integer(C_int), intent(out)                       :: ophase !< Phase identifyer for MINGIBBSPH
  ! Locals
  call thermo(T,P,x,phase,lnfug,lnfugt,lnfugp,lnfugx,ophase)

  end subroutine thermopack_thermo_c

!-----------------------------------------------------------------!
subroutine thermopack_moleweight_c(z, mw) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the moleWeight
  !> routine in eos.f90
  !> Notes:
  !> \author MH, 2019-04
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eos, only: moleWeight
  use thermopack_var, only: nc
  implicit none
  !Input:
  real(C_double), dimension(1:nc), intent(in) :: z !< Composition
  real(C_double), intent(out) :: mw !< g/mol - moleweight
  !---------------------------------------------------------------!
  mw = moleWeight(z)
  !---------------------------------------------------------------!
  end subroutine thermopack_moleWeight_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_specific_volume_c(t,p,x,iphase,v) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the specific volume.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-09-05
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eos,        only: specificVolume
  use thermopack_var, only: nc
  implicit none
  ! Input:
  integer(C_int),                intent(in)  :: iphase !<          Phase flag
  real(C_double),                intent(in)  :: t      !< K      - Temperature [K]
  real(C_double),                intent(in)  :: p      !< Pa     - Pressure [Pa]
  real(C_double), dimension(nc), intent(in)  :: x      !<          Composition
  ! Output
  real(C_double),                intent(out) :: v      !< m3/mol - Molar mixture enthalpy
  !---------------------------------------------------------------!
  call specificVolume(t,p,x,iphase,v)
  !
  end subroutine thermopack_specific_volume_c
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_wilsonKi_c(i,t,p,lnPhi_offset,k) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the wilson K  routine
  !> in eos.f90
  !> Notes:
  !> \author MH, 2019-04
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use thermo_utils, only: wilsonKi
  implicit none
  !Input:
  integer(C_int), intent(in) :: i !< Component index
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: p !< Pa - Pressure
  real(C_double), intent(in) :: lnPhi_offset
  real(C_double), intent(out) :: K !< K-values
  !---------------------------------------------------------------!
  call wilsonKi(i,t,p,lnPhi_offset,K)
  !---------------------------------------------------------------!
  end subroutine thermopack_wilsonKi_c
!-----------------------------------------------------------------!

subroutine thermopack_twophase_dhdt_c(t,p,Z,X,Y,betaV,betaL,dhdt) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for calculating dhdt for two phases, using the
  !> dhdt_twoPhase routine in state_func.f90. dhdt.
  !> Notes:
  !> \author HHU, 2014-07-08
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use state_functions, only: dhdt_twoPhase
  use thermopack_var, only:nc
  implicit none
  real(C_double), intent(in) :: betaV !< Vapour phase molar fraction [-]
  real(C_double), intent(in) :: betaL !< Liquid phase molar fraction [-]
  real(C_double), dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
  real(C_double), dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
  real(C_double), dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
  real(C_double), intent(in) :: t !< Temperature [K]
  real(C_double), intent(in) :: p !< Pressure [Pa]
  real(C_double), intent(out) :: dhdt !< [J/K*mol]

  dhdt = dhdt_twoPhase(t,p,Z,betaV,betaL,X,Y)

!-----------------------------------------------------------------!
  end subroutine thermopack_twophase_dhdt_c
