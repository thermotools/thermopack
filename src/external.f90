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


!  !
!  !-----------------------------------------------------------------!
!  use stringmod, only:parse
!  implicit none
!  character(len=*),intent(in) :: components_in
!  integer, intent(in) :: n_comp
!  character(len=*), intent(in) :: eoslib, eos, mixrule, alpha
!  integer,intent(out) :: ierr
!  integer, intent(in) :: liq_vap_discr_flag
!  !INTERNAL:
!  integer, parameter :: max_strlen = 8
!  character (len=max_strlen) :: components_out(n_comp)
!  integer :: icomp
!  character(len=1) :: delim
!  character(len=len(components_in)) :: components_str
!-----------------------------------------------------------------!
subroutine thermopack_init_int_flags(components_in, n_comp, eos_lib_flag,&
     eos_flag, mixrule_flag, alpha_flag, ierr, liq_vap_discr_flag)
  !-----------------------------------------------------------------!
  ! External interface to thermopack_init using flags instead of strings.
  ! Should make communication between C and f90 more smooth.
  ! \author: HHU 2014-07-01, Ailo 2016-09-13
  !
  !-----------------------------------------------------------------!
  use stringmod, only:parse
  implicit none
  character(len=*),intent(in) :: components_in
  integer, intent(in) :: n_comp
  integer, intent(in) :: eos_lib_flag, eos_flag, mixrule_flag, alpha_flag
  integer,intent(out) :: ierr
  integer, intent(in) :: liq_vap_discr_flag
  !INTERNAL:
  character (len=15) :: eos_lib_str
  character (len=15) :: eos_str,mixrule_str,alpha_str
  character (len=8) :: components_out(n_comp)
  integer :: icomp
  character(len=1) :: delim
  character(len=len(components_in)) :: components_str
  !-----------------------------------------------------------------!
  components_str=trim(components_in)
  delim = ' '
  call parse(components_str,delim,components_out,n_comp)

  do icomp=1,n_comp
    components_out(icomp) = trim(components_out(icomp))
  end do

  select case(eos_lib_flag)
  case(1)
    eos_lib_str = 'Thermopack'
  case(2)
    eos_lib_str = 'TREND'
  case default
    call StopError('Error: eos_lib_flag not valid.')
  end select
  !-----------------------------------------------------------------!
  select case(eos_flag)
  case(1)
    eos_str = 'SRK' !
  case default
    call StopError('Error: eos_flag not valid.')
  end select
  !-----------------------------------------------------------------!
  select case(mixrule_flag)
  case(1)
    mixrule_str = 'CLASSIC' !length=11
  case default
    call StopError('Error: mixrule_flag not valid.')
  end select
  !-----------------------------------------------------------------!
  select case(alpha_flag)
  case(1)
    alpha_str = 'CLASSIC' !length=11
  case default
    call StopError('Error:alpha_flag not valid.')
  end select

  !-----------------------------------------------------------------!
  call thermopack_init_liq_vap_discr(components_out,n_comp,8,eos_lib_str,&
       eos_str,mixrule_str,alpha_str,ierr,liq_vap_discr_flag)

end subroutine thermopack_init_int_flags
!-------------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_init_liq_vap_discr(components,n_comp,strlen,&
     eos_lib_str,eos_str,mixrule_str,alpha_str,ierr,liq_vap_discr_method)
!-----------------------------------------------------------------!
!
! Modelled after init.f90
!
  use eoslibinit, only: init_thermo
  use thermopack_constants, only: clen
  !use error, only: stoperror
  implicit none
  ! Input:
  integer, intent(in)               :: n_comp,strlen
  character(len=*), intent(in)      :: eos_lib_str
  character(len=strlen), intent(in) :: components(n_comp)
  character(len=*), intent(in)      :: eos_str,mixrule_str,alpha_str
  integer, intent(in)               :: liq_vap_discr_method
  ! Output:
  integer, intent(out)              :: ierr
  ! Internal:
  integer                           :: i,nphases
  character(len=clen)               :: component_str

  ! write(*,*) "--------------------------"
  ! write(*,*) "---WELCOME TO THERMOPACK--"
  ! write(*,*) "--------------------------"


  ierr = 0

  component_str = trim(components(1))
  do i=2,n_comp
    component_str = trim(component_str)//(','//trim(components(i)))
  end do

  nphases = n_comp + 1

  call init_thermo(eos_str,mixrule_str,alpha_str,&
       component_str,nphases,liq_vap_discr_method)

  ! write(*,*) "--------------------------"
  ! write(*,*) "--------------------------"

end subroutine thermopack_init_liq_vap_discr

!-----------------------------------------------------------------!
subroutine thermopack_cleanup()
!-----------------------------------------------------------------!
  !use eoslibinit, only: cleanup_eos
  implicit none
  !call cleanup_eos()

end subroutine thermopack_cleanup
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_TPflash(T,P,x_overall,beta,phase,comp_liquid,comp_vapor)
  !---------------------------------------------------------------!

  use tp_solver, only: twoPhaseTPflash
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, intent(in)          :: T,P,x_overall(nc)
  ! Output:
  integer, intent(out)      :: phase
  real, intent(out)         :: beta
  real, intent(out)         :: comp_liquid(nc),comp_vapor(nc)
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

end subroutine thermopack_TPflash
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_fugacity(T,P,x_phase,phaseflag,fug)
!-----------------------------------------------------------------!
!
! phaseflag: LIQPH, VAPPH or SINGLEPH
!
  use eos, only: thermo
  use thermopack_constants, only: LIQPH,VAPPH,SINGLEPH,MINGIBBSPH
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, intent(in)          :: T,P,x_phase(nc)
  integer                   :: phaseflag
  ! Output:
  real, intent(out)         :: fug(nc)
  ! Internal:
  integer                   :: i,phase

  select case (phaseflag)
    case (LIQPH,VAPPH)
      phase = phaseflag
    case (SINGLEPH)
      phase = MINGIBBSPH
    case default
      write(*,*) "Invalid phase flag in thermopack_fugacity"
      stop
  end select

  !write(*,*) "--------------------------"
  !write(*,*) "Running thermopack thermo"
  call thermo(T,P,x_phase,phase,fug) ! Returns ln(fug_coeff)
  do i=1,nc
    fug(i) = P*x_phase(i)*exp(fug(i))
  end do
  !write(*,*) "-Done---------------------"

end subroutine thermopack_fugacity
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
function thermopack_bubble_pressure(T,P,x,y) result(pBub)
!-----------------------------------------------------------------!
!
  use saturation, only: bubP
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, intent(inout)                :: T,P
  real, dimension(nc), intent(in)    :: x
  ! Output:
  real, dimension(nc), intent(inout) :: y
  real :: pBub
  !
  pBub = bubP(T,P,x,y)

end function thermopack_bubble_pressure
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_phase_envelope(T,P,beta,cspec,z,Pmax,Ta,Pa,Ki,betai,nmax,n)
!-----------------------------------------------------------------!
!
  use saturation_curve, only: envelopePlot
  use thermopack_var, only: nc
  use stringmod, only: str_eq
  implicit none
  ! Input:
  real, intent(in)             :: T,P
  real, intent(in)                :: beta, Pmax
  real, dimension(nc), intent(in) :: z
  character(len=1), intent(in) :: cspec
  integer, intent(in) :: nmax
  ! Output:
  real, dimension(nmax), intent(out) :: Ta,Pa
  real, dimension(nmax,nc), intent(out) :: Ki
  real, dimension(nmax), intent(out) :: betai
  integer, intent(out) :: n
  ! Locals:
  integer :: spec

  if ( str_eq(cspec, 'P') ) then
    spec = 1
  else if ( str_eq(cspec,'T') ) then
    spec = 2
  else
    call StopError("thermopack_phase_envelope: specification should be 'T' or 'P'!")
  endif
  call envelopePlot(Z,t,p,spec,beta,Pmax,nmax,Ta,Pa,Ki,betai,n)

end subroutine thermopack_phase_envelope
!-----------------------------------------------------------------!
!-----------------------------------------------------------------!
subroutine thermopack_density(t,p,z,iphase,rho)
!-----------------------------------------------------------------!
  !> External interface for the root finder of the pressure-density
  !> curve.
  !>
  !> Note: For TREND, a negative rho is returned if no root is
  !>       found.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-08-12
  !---------------------------------------------------------------!
  use eos, only: specificVolume
  use thermopack_var,   only: nc
  implicit none
  ! Arguments:
  real,                intent(in)  :: t      !< K      - Temperature
  real,                intent(in)  :: p      !< Pa     - Pressure
  real, dimension(nc), intent(in)  :: z      !<          Composition
  integer,             intent(in)  :: iphase !<          Phase identifier
  real,                intent(out) :: rho    !< mol/m3 - Molar density
  ! Internal
  real :: v
  !---------------------------------------------------------------!
  call specificVolume(t,p,x=z,phase=iphase,v=v)
  rho = 1.0/v

end subroutine thermopack_density
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_pressure(t,rho,z,p)
!-----------------------------------------------------------------!
  !> External interface for the pressure.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-08-13
  !---------------------------------------------------------------!
  use thermopack_var, only: nc
  use eosTV,      only: pressure
  implicit none
  include 'trend_interface.f95'
  ! Input:
  real,                intent(in)  :: t   !< K      - Temperature
  real,                intent(in)  :: rho !< mol/m3 - Molar density
  real, dimension(nc), intent(in)  :: z   !<          Composition
  ! Output:
  real,                intent(out) :: p   !< Pa     - Pressure
  !---------------------------------------------------------------!
  ! Thermopack
  p=pressure(t,1.0/rho,z)
end subroutine thermopack_pressure
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_specific_volume(t,p,x,iphase,v)
  !---------------------------------------------------------------!
  !> External interface for the specific volume.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-09-05
  !---------------------------------------------------------------!
  use eos,        only: specificVolume
  use thermopack_var, only: nc
  implicit none
  ! Input:
  integer,             intent(in)  :: iphase !<          Phase flag
  real,                intent(in)  :: t      !< K      - Temperature [K]
  real,                intent(in)  :: p      !< Pa     - Pressure [Pa]
  real, dimension(nc), intent(in)  :: x      !<          Composition
  ! Output
  real,                intent(out) :: v      !< m3/mol - Molar mixture enthalpy
  !---------------------------------------------------------------!
  call specificVolume(t,p,x,iphase,v)
  !
end subroutine thermopack_specific_volume
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_internal_energy(t,v,x,u)
  !---------------------------------------------------------------!
  !> External interface for the internal energy.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-09-05
  !---------------------------------------------------------------!
  use eosTV,      only: internal_energy
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real,                intent(in)  :: t    !< K -       Temperature
  real,                intent(in)  :: v    !< m3/mol  - Molar volume
  real, dimension(nc), intent(in)  :: x    !<           Composition
  ! Output:
  real,                intent(out) :: u    !< J/mol   - Molar internal energy
  ! Locals:
  real                             :: dudv
  real                             :: dudt
  !---------------------------------------------------------------!
  call internal_energy(t,v,x,u,dudv,dudt)
  !
end subroutine thermopack_internal_energy
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_UVflash(t,p,z,beta,x,y,uspec,vspec,iphase)
  !---------------------------------------------------------------!
  !> External interface for the UV-flash routine.
  !>
  !> Note: This routine only supports two-phase flash at the
  !>       moment. We are also restricted to the nested loop
  !>       algorithm.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-09-05
  !---------------------------------------------------------------!
  use uv_solver,  only: twoPhaseUVflash,twoPhaseUVflashNested
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, dimension(nc), intent(in)    :: z      !< Composition
  real,                intent(in)    :: uspec  !< J/mol  - Specified internal energy
  real,                intent(in)    :: vspec  !< m3/mol - Specified molar volume
  ! Output:
  integer,             intent(inout) :: iphase !< Phase flag
  real,                intent(inout) :: t      !< Temperature
  real,                intent(inout) :: p      !< Pressure
  real,                intent(inout) :: beta   !< Vapour mole fraction
  real, dimension(nc), intent(inout) :: x      !< Liquid composition
  real, dimension(nc), intent(inout) :: y      !< Vapour composition
  !---------------------------------------------------------------!
  real :: betaL
  ! call twoPhaseUVflash(t,p,z,beta,x,y,uspec,vspec,iphase)
  call twoPhaseUVflashNested(t,p,z,beta,betaL,x,y,uspec,vspec,iphase)
  !
end subroutine thermopack_UVflash
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_objective(t,p,z,v,iobjective,of)
  !---------------------------------------------------------------!
  !> External interface for the various objective functions
  !>
  !> Note: This routine only supports two-phase flash at the
  !>       moment.
  !---------------------------------------------------------------!
  !> \author MAG, 2013-09-11
  !---------------------------------------------------------------!
  use tp_solver,  only: tp_objective => objective
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real,                   intent(in)  :: t
  real,                   intent(in)  :: p
  real,    dimension(nc), intent(in)  :: z
  real,    dimension(nc), intent(in)  :: v
  integer,                intent(in)  :: iobjective
  ! Output:
  real,                   intent(out) :: of !< Value of objective function
  ! Locals:
  real, dimension(nc)                 :: l
  real, dimension(2*nc+2)             :: params
  !---------------------------------------------------------------!
  select case(iobjective)
  case(1)
    ! TP-flash objective function
    l=z-v
    params(1)=t
    params(2)=p
    params(3:nc+2)=z
    params(nc+3:2*nc+2)=l
    of=tp_objective(v,params)
  case default
    write (*,*) "Error in external.f90:thermopack_objective: No such iobjective!"
    stop
  end select
  !
end subroutine thermopack_objective
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_HPflash(t,p,Z,beta,betaL,X,Y,hspec,phase)
 !----------------------------------------------------------------!
  !> External interface for the HP-flash routine.
  !>
  !>
  !> --------------------------------------------------------------!
  !> \author HHU, 2014-06-19
  !----------------------------------------------------------------!
  use ph_solver,  only: twoPhasePHflash
  use thermopack_var, only: nc
  implicit none
  ! Input:
    real, intent(inout) :: beta !< Vapour phase molar fraction [-]
    real, intent(out) :: betaL !< Liquid phase molar fraction [-]
    real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
    real, dimension(nc), intent(inout) :: X !< Liquid molar compozition [-]
    real, dimension(nc), intent(inout) :: Y !< Vapour molar compozition [-]
    real, intent(inout) :: t !< Temperature [K]
    real, intent(in) :: p !< Pressure [Pa]
    real, intent(in) :: hspec !< Specified enthalpy [J/mol]
    integer, intent(inout) :: phase !< Phase identifier
   !---------------------------------------------------------------!
  call twoPhasePHflash(t,p,Z,beta,betaL,X,Y,hspec,phase)

!  if(phase == 4) then
!    if(beta > 1-1.0E-3) then
 !     phase = 2
  !  else
   !   phase = 1
    !end if
  !end if
  !
 end subroutine thermopack_HPflash
 !-----------------------------------------------------------------!


 !-----------------------------------------------------------------!
subroutine thermopack_SPflash(t,p,Z,beta,betaL,X,Y,sspec,phase)
 !-----------------------------------------------------------------!
  !> External interface for the SP-flash routine.
  !>
  !>
  !----------------------------------------------------------------!
  !> \author HHU, 2014-06-19
  !----------------------------------------------------------------!
  use ps_solver,  only: twoPhasePSflash
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, intent(inout) :: beta !< Vapour phase molar fraction [-]
  real, intent(out) :: betaL !< Liquid phase molar fraction [-]
  real, dimension(nc), intent(in) :: Z !<Overall molar composition [-]
  real, dimension(nc), intent(inout) :: X !<Liquid molar composition [-]
  real, dimension(nc), intent(inout) :: Y !<Vapour molar composition [-]
  real, intent(inout) :: t !< Temperature [K]
  real, intent(in) :: p !< Pressure [Pa]
  real, intent(in) :: sspec !<Specified entropy [J/mol*K]
  integer, intent(inout) :: phase !< Phase identifier
  !----------------------------------------------------------------!
  call twoPhasePSflash(t,p,Z,beta,betaL,X,Y,sspec,phase)

 ! if(phase == 4) then
  !  if(beta > 1-1.0E-3) then
   !   phase = 2
!    else
 !     phase = 1
  !  end if
 ! end if
  !
end subroutine thermopack_SPflash
 !-----------------------------------------------------------------!

!----------------------------------------------------------------!
subroutine thermopack_bubT(P,X,Y,Tbub,ierr)
  !--------------------------------------------------------------!
  !> External interface for the bubble point temperature routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !--------------------------------------------------------------!
  use saturation
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, dimension(nc), intent(in) :: X
  real, dimension(nc), intent(out) :: Y
  real, intent(in) :: P
  real, intent(out):: Tbub
  integer, intent(out) :: ierr
  !--------------------------------------------------------------!
  Tbub = safe_bubT(P,X,Y,ierr)
  !--------------------------------------------------------------!
end subroutine thermopack_bubT
!----------------------------------------------------------------!


!----------------------------------------------------------------!
subroutine thermopack_bubP(T,X,Y,Pbub,ierr)
  !--------------------------------------------------------------!
  !> External interface for the bubble point pressure routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !--------------------------------------------------------------!
  use saturation
  use thermopack_var, only: nc
  implicit none
  ! Input:
   real, dimension(nc), intent(in) :: X
   real, dimension(nc), intent(out) :: Y
   real, intent(in) :: T
   real, intent(out) :: Pbub
   integer, intent(out) :: ierr
   !--------------------------------------------------------------!
   Pbub = safe_bubP(T,X,Y,ierr)
   !--------------------------------------------------------------!
 end subroutine thermopack_bubP
 !----------------------------------------------------------------!


 !----------------------------------------------------------------!
subroutine thermopack_dewT(P,X,Y,Tdew,ierr)
  !---------------------------------------------------------------!
  !> External interface for the dew point temperature routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !---------------------------------------------------------------!
  use saturation
  use thermopack_var, only: nc
  implicit none
  ! Input:
  real, dimension(nc), intent(out) :: X
  real, dimension(nc), intent(in) :: Y
  real, intent(in) :: P
  real, intent(out) :: Tdew !< [K]
  integer, intent(out) :: ierr

  !---------------------------------------------------------------!
  Tdew = safe_dewT(P,X,Y,ierr)
  !---------------------------------------------------------------!
end subroutine thermopack_dewT
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_dewP(T,X,Y,Pdew,ierr)
  !---------------------------------------------------------------!
  !> External interface for the dew point pressure routine
  !> in saturation.f90
  !>
  !> \author HHU, 2014-06-20
  !---------------------------------------------------------------!
  use saturation
  use thermopack_var, only:nc
  implicit none
  ! Input:
  real, dimension(nc), intent(out) :: X
  real, dimension(nc), intent(in) :: Y
  real, intent(in) :: T
  real, intent(out) :: Pdew !< Pa
  integer, intent(out) :: ierr

  !---------------------------------------------------------------!
  Pdew = safe_dewP(T,X,Y,ierr)
  !---------------------------------------------------------------!
end subroutine thermopack_dewP
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_zfac(t,p,x,phase,z)
  !---------------------------------------------------------------!
  !> External interface for the zfac routine
  !> in eos.f90
  !> Note: Optional arguments not included here
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  integer, intent(in) :: phase !< Phase identifyer
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, dimension(1:nc), intent(in) :: x !< Compozition
  real, intent(out) :: z !< Compressibillity factor
  !---------------------------------------------------------------!
  call zfac(t,p,x,phase,z)
  !---------------------------------------------------------------!
end subroutine thermopack_zfac
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_twophase_specific_volume(t,p,z,x,y,beta,phase,v)
  !---------------------------------------------------------------!
  !> External interface for the two phase molar volume routine
  !> in eos.f90
  !>
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  integer, intent(in) :: phase !< Phase identifyer
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, intent(in) :: beta !< Gas phase mole fraction
  real, dimension(1:nc), intent(in) :: x !< Liquid compozition
  real, dimension(1:nc), intent(in) :: y !< Gas compozition
  real, dimension(1:nc), intent(in) :: z !< Overall compozition
  real, intent(out) :: v !< m3/mol - Molar mixture volume
  !---------------------------------------------------------------!
  v = twoPhaseSpecificVolume(t,p,z,x,y,beta,phase)
  !---------------------------------------------------------------!
end subroutine thermopack_twophase_specific_volume
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_enthalpy(t,p,x,phase,h,spec,dhdx,specflag,residual)
  !---------------------------------------------------------------!
  !> External interface for the enhalpy routine
  !> in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  integer, intent(in) :: phase !< Phase identifyer
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, dimension(1:nc), intent(in) :: x !< Compozition
  real, intent(out) :: h !< J/mol - Molar enthalpy
  !Optional:
  real, intent(inout) :: spec !< Extra outputs. Either dhdt,dhdp,residual or none
  real, dimension(nc), intent(inout) :: dhdx !<For dhdx
  integer, intent(in) :: specflag !< Case 0:none; 1:dhdt; 2:dhdp; 3:dhdx;
  logical, intent(in) :: residual !< If only residual enthalpy is needed. Standard: .false.

  !----------------------------------------------------------------!
  if(phase == 0) then
    write(*,*) "Called enthalpy with twophase"
  end if

  select case(specflag)
  case(0)
    call enthalpy(t,p,x,phase,h)
  case(1)
    call enthalpy(t=t,p=p,x=x,phase=phase,h=h,dhdt=spec)
    !if(spec.GE.0) then
     ! if(spec==spec) then
      !  write (*,*) "OK dhdt with phase=",phase
    !  end if
    !end if

    if(spec<0) then
      write (*,*) "t =", t, ", p =", p
      write (*,*) "phase =", phase, ", h =", h
      write (*,*) "dhdt=", spec
    end if
    if(spec/=spec) then
      write (*,*) "t = ", t, ", p = ", p
      write (*,*) "phase = ", phase, ", h = ", h
      write (*,*) "dhdt= ", spec
    end if
  case(2)
    call enthalpy(t=t,p=p,x=x,phase=phase,h=h,dhdp=spec)
  case(3)
    call enthalpy(t=t,p=p,x=x,phase=phase,h=h,dhdx=dhdx)
  case(4)
    call enthalpy(t=t,p=p,x=x,phase=phase,h=h,residual=residual)
  case default
    call stoperror("Error. Specflag invalid value")
  end select

  !----------------------------------------------------------------!
end subroutine thermopack_enthalpy
!------------------------------------------------------------------!


!------------------------------------------------------------------!
subroutine thermopack_twophase_enthalpy(t,p,z,x,y,beta,phase,h)
  !---------------------------------------------------------------!
  !> External interface for the two phase enhalpy routine
  !> in eos.f90
  !>
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !input:
  integer, intent(in) :: phase !< Phase identifyer
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, intent(in) :: beta !< Gas phase mole fraction
  real, dimension(1:nc), intent(in) :: x !< Liquid compozition
  real, dimension(1:nc), intent(in) :: y !< Gas compozition
  real, dimension(1:nc), intent(in) :: z !< Overall compozition
  real, intent(out) :: h !< J/mol - Molar mixture enthalpy
  !---------------------------------------------------------------!
  h = twoPhaseEnthalpy(t,p,z,x,y,beta,phase)
  !---------------------------------------------------------------!
end subroutine thermopack_twophase_enthalpy
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_entropy(t,p,x,phase,s)
  !---------------------------------------------------------------!
  !> External interface for the entropy routine
  !> in eos.f90
  !> Notes: Optional arguments not included here.
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  integer, intent(in) :: phase !< Phase identifyer
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, dimension(1:nc), intent(in) :: x !< Compozition
  real, intent(out) :: s !< J/mol/K - Molar entropy
  !---------------------------------------------------------------!
  if(phase == 0) then
    write(*,*) "Called entropy with twophase"
  end if

  call entropy(t,p,x,phase,s)
  !---------------------------------------------------------------!
end subroutine thermopack_entropy
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_twophase_entropy(t,p,z,x,y,beta,phase,s)
  !---------------------------------------------------------------!
  !> External interface for the two phase  entropy routine
  !> in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only:nc
  implicit none
  !Input:
  integer, intent(in) :: phase !< Phase identifyer
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, intent(in) :: beta !< Gas phase mole fraction
  real, dimension(1:nc), intent(in) :: x !< Liquid compozition
  real, dimension(1:nc), intent(in) :: y !< Gas compozition
  real, dimension(1:nc), intent(in) :: z !< Overall compozition
  real, intent(out) :: s !< J/mol/K - Molar mixture entropy
  !---------------------------------------------------------------!
  s = twoPhaseEntropy(t,p,z,x,y,beta,phase)
  !---------------------------------------------------------------!
end subroutine thermopack_twophase_entropy
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_pseudo(x,tpc,ppc,zpc,vpc)
  !---------------------------------------------------------------!
  !> External interface for the pseudo critical point routine
  !> in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  use thermo_utils, only: issingleComp
  implicit none
  !Input:
  real, dimension(1:nc), intent(in) :: x !< Compozition
  real, intent(out) :: tpc !< K - Pseudo critical temperature
  real, intent(out) :: vpc !< m3/mol - Pseudo critical molar volume
  real, intent(out) :: ppc !< Pa - Pseudo critical pressure
  real, intent(out) :: zpc !< - - Pseudo critical compressibillity
  !---------------------------------------------------------------!

  call pseudo_safe(x,tpc,ppc,zpc,vpc)
  !---------------------------------------------------------------!
end subroutine thermopack_pseudo
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_wilsonK(t,p,k)
  !---------------------------------------------------------------!
  !> External interface for the wilson K  routine
  !> in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use thermo_utils, only: wilsonK
  use thermopack_var, only: nc
  implicit none
  !Input:
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, dimension(nc), intent(out) :: k !< K-values
  !---------------------------------------------------------------!
  call wilsonK(t,p,k)
  !---------------------------------------------------------------!
end subroutine thermopack_wilsonK
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_wilsonKdiff(t,p,K,dKdp,dKdt)
  !---------------------------------------------------------------!
  !> External interface for the wilson K w/ temperature and pressure
  !> differentials routine in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use thermo_utils, only: wilsonK
  use thermopack_var, only: nc
  implicit none
  !Input:
  real, intent(in) :: t !< K - Temperature
  real, intent(in) :: p !< Pa - Pressure
  real, dimension(nc), intent(out) :: K !< K-values
  real, dimension(nc), intent(out) :: dKdt !< 1/K - Differential of K-values wrpt. temperature
  real, dimension(nc), intent(out) :: dKdp !< 1/Pa - Differential of K-values wrpt. pressure
  !---------------------------------------------------------------!
  call wilsonK(t,p,K,dKdp,dKdt)
  !---------------------------------------------------------------!
end subroutine thermopack_wilsonKdiff
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_getCriticalParam(i,tci,pci,oi)
  !---------------------------------------------------------------!
  !> External interface for the getCriticalParam
  !> routine in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  implicit none
  !Input:
  integer, intent(in) :: i !< Component index
  real, intent(out) :: tci !< K - Critical temperature
  real, intent(out) :: pci !< Pa - Critical pressure
  real, intent(out) :: oi  !< Acentric factor
  !---------------------------------------------------------------!
  call getCriticalParam(i,tci,pci,oi)
  !---------------------------------------------------------------!
end subroutine thermopack_getCriticalParam
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_single_idealGibbs(t,p,j,g)
  !---------------------------------------------------------------!
  !> External interface for the idealGibbsSingle
  !> routine in eos.f90
  !> Notes: Optional parameters not included here
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  implicit none
  !Input:
  real, intent(in) :: t                   !< K - Temperature
  real, intent(in) :: p                   !< Pa - Pressure
  integer, intent(in) :: j                !< Component index
  real, intent(out) :: g                  !< J/mol - Ideal Gibbs energy
  !---------------------------------------------------------------!
  call idealGibbsSingle(t,p,j,g)
  !---------------------------------------------------------------!
end subroutine thermopack_single_idealGibbs
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_single_idealEntropy(t,p,j,s)
  !---------------------------------------------------------------!
  !> External interface for the idealEntropySingle
  !> routine in eos.f90
  !> Notes: Optional parameters not included here
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  implicit none
  !Input:
  real, intent(in) :: t                   !< K - Temperature
  real, intent(in) :: p                   !< Pa - Pressure
  integer, intent(in) :: j                !< Component index
  real, intent(out) :: s                  !< J/mol/K - Ideal entropy
  !---------------------------------------------------------------!
  call idealEntropySingle(t,p,j,s)
  !---------------------------------------------------------------!
end subroutine thermopack_single_idealEntropy
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_single_idealEnthalpy(t,p,j,h)
  !---------------------------------------------------------------!
  !> External interface for the idealEnthalpySingle
  !> routine in eos.f90
  !> Notes: Optional parameters not included here
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  implicit none
  !input:
  real, intent(in) :: t                   !< K - Temperature
  real, intent(in) :: p                   !< Pa - Pressure
  integer, intent(in) :: j                !< Component index
  real, intent(out) :: h                  !< J/mol - Ideal enthalpy
  !---------------------------------------------------------------!
  call idealEnthalpySingle(t,p,j,h)
  !---------------------------------------------------------------!
end subroutine thermopack_single_idealEnthalpy
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_residual_Gibbs(t,p,z,phase,gr)
  !---------------------------------------------------------------!
  !> External interface for the residualGibbs
  !> routine in eos.f90
  !> Notes: Optional parameters not included here
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !input:
  real, intent(in) :: t                    !< K - Temperature
  real, intent(in) :: p                    !< Pa - Pressure
  real, dimension(nc), intent(in) :: z     !< Component fractions
  integer, intent(in) :: phase             !< Phase identifyer
  real, intent(out) :: gr                  !< J/mol - Residual Gibbs energy
  !---------------------------------------------------------------!
  call residualGibbs(t,p,z,phase,gr)
  !---------------------------------------------------------------!
end subroutine thermopack_residual_Gibbs
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_moleweight(z,mw)
  !---------------------------------------------------------------!
  !> External interface for the moleWeight
  !> routine in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  real, dimension(1:nc), intent(in) :: z !< Composition
  real, intent(out) :: mw !< g/mol - moleweight
  !---------------------------------------------------------------!
  mw =moleWeight(z)
  !---------------------------------------------------------------!
end subroutine thermopack_moleWeight
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_compMoleWeight(j,mw)
  !---------------------------------------------------------------!
  !> External interface for the compMoleWeight
  !> routine in eos.f90
  !> Notes:
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use eos
  implicit none
  !input:
  integer, intent(in) :: j !< Component index
  real, intent(out) :: mw !< kg/mol - Mole weight
  !---------------------------------------------------------------!
  mw = 1e-3 * compMoleWeight(j)
  !---------------------------------------------------------------!
end subroutine thermopack_compMoleWeight
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_twophase_sound_velocity(t,p,X,Y,Z,betaV,betaL,phase,sos)
  !---------------------------------------------------------------!
  !> External interface for the sound_velocity
  !> routine in sound_velocity_2ph.f90
  !> Notes: If there is only a single phase present, the single phase
  !> routine is called through this routine.
  !> \author HHU, 2014-06-23
  !---------------------------------------------------------------!
  use speed_of_sound
  use thermopack_var, only: nc
  implicit none
  !Input:
  real,                   intent(in) :: t     !< Temperature [K]
  real,                   intent(in) :: p     !< Pressure [Pa]
  real, dimension(nc),    intent(in) :: Z     !< Overall molar compozition [-]
  real,                   intent(in) :: betaV !< Vapor molar fraction [-]
  real,                   intent(in) :: betaL !< Liquid molar fraction [-]
  real, dimension(nc),    intent(in) :: X     !< Liquid molar compozition [-]
  real, dimension(nc),    intent(in) :: Y     !< Vapor molar compozition [-]
  integer,                intent(in) :: phase !< Phase spec
  real,                  intent(out) :: sos   !< Speed of sound [m/s]
  !---------------------------------------------------------------!
  sos = sound_velocity_2ph(t,p,X,Y,Z,betaV,betaL,phase)
  !---------------------------------------------------------------!
end subroutine thermopack_twophase_sound_velocity
!-----------------------------------------------------------------!


!-----------------------------------------------------------------!
subroutine thermopack_cp (t,p,x,phase,cp)
  !---------------------------------------------------------------!
  !> External interface for calculating cp, using the enthalpy routine
  !> in eos.f90. Cp = dhdt.
  !> Notes:
  !> \author HHU, 2014-06-26
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  real, intent(in) :: t !< Temperature [K]
  real, intent(in) :: p !< Pressure[Pa]
  real, dimension(nc), intent(in) :: x !< Composition
  integer, intent(in) :: phase !< Phase identifier [1=liq, 2=vap, 0=multi]
  real, intent(out) :: cp !< Molar heat capacity (J/(mol*K))
  real :: h

  call enthalpy(t,p,x,phase,h,dhdt=cp)

end subroutine thermopack_cp
!-----------------------------------------------------------------!

!-----------------------------------------------------------------!
subroutine thermopack_cv (t,p,x,phase,cv)
  !---------------------------------------------------------------!
  !> External interface for calculating cp, using the enthalpy routine
  !> in eos.f90. Cp = dhdt.
  !> Notes:
  !> \author HHU, 2014-06-26
  !---------------------------------------------------------------!
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  real, intent(in) :: t !< Temperature [K]
  real, intent(in) :: p !< Pressure[Pa]
  real, dimension(nc), intent(in) :: x !< Composition
  integer, intent(in) :: phase !< Phase identifier [1=liq, 2=vap, 0=multi]
  real, intent(out) :: cv !< Molar heat capacity (J/(mol*K))
  real :: h,v,cp,dvdt,dvdp

  call specificVolume(t,p,x,phase,v,dvdt=dvdt,dvdp=dvdp)
  call enthalpy(t,p,x,phase,h,dhdt=cp)
  cv = cp + t*dvdt*dvdt/dvdp
end subroutine thermopack_cv
!-----------------------------------------------------------------!

subroutine themopack_exp_adiabatic (t,p,x,phase, beta_s)
  use eos
  use thermopack_var, only: nc
  implicit none
  !Input:
  real, intent(in) :: t !< Temperature [K]
  real, intent(in) :: p !< Pressure[Pa]
  real, dimension(nc), intent(in) :: x !< Composition
  integer, intent(in) :: phase !< Phase identifier [1=liq, 2=vap, 0=multi]
  real, intent(out) :: beta_s !< Adiabatic expansion coefficient
  real :: h,v,cp,dvdt,dvdp,cv,beta_t

  call specificVolume(t,p,x,phase,v,dvdt=dvdt,dvdp=dvdp)
  call enthalpy(t,p,x,phase,h,dhdt=cp)

  beta_t = -1.0/V*dvdp
  cv = cp + t*dvdt*dvdt/dvdp
  beta_s = cv/cp * beta_t
end subroutine themopack_exp_adiabatic

subroutine thermopack_cp_ideal (i,t,cp_id)
  !---------------------------------------------------------------!
  !> Get ideal cp for component i.
  !> \author Ailo
  !---------------------------------------------------------------!
  use thermopack_var, only: nce, eos_container, get_active_eos_container
  use ideal, only: cpideal
  implicit none
  !Input:
  integer, intent(in) :: i !< Component number
  real, intent(in) :: t !< Temperature [K]
  real, intent(out) :: cp_id !< Ideal spec. heat capacity (J/mol*K)
  type(eos_container), pointer :: p_act_eosc

  p_act_eosc => get_active_eos_container()

  if (i<1 .or. i>nce) then
    print *, "index in thermopack_cp_ideal = ", i
    call stoperror("index out of range")
  end if

  cp_id = cpideal(p_act_eosc%comps(i)%p_comp,i,t)

end subroutine thermopack_cp_ideal


subroutine thermopack_twophase_dhdt(t,p,Z,X,Y,betaV,betaL,dhdt)
  !---------------------------------------------------------------!
  !> External interface for calculating dhdt for two phases, using the
  !> dhdt_twoPhase routine in state_func.f90. dhdt.
  !> Notes:
  !> \author HHU, 2014-07-08
  !---------------------------------------------------------------!
  use state_functions
  use thermopack_var, only:nc
  implicit none
  real, intent(in) :: betaV !< Vapour phase molar fraction [-]
  real, intent(in) :: betaL !< Liquid phase molar fraction [-]
  real, dimension(nc), intent(in) :: Z !< Overall molar compozition [-]
  real, dimension(nc), intent(in) :: X !< Liquid molar compozition [-]
  real, dimension(nc), intent(in) :: Y !< Vapour molar compozition [-]
  real, intent(in) :: t !< Temperature [K]
  real, intent(in) :: p !< Pressure [Pa]
  real, intent(out) :: dhdt !< [J/K*mol]

  dhdt = dhdt_twoPhase(t,p,Z,betaV,betaL,X,Y)

!-----------------------------------------------------------------!
end subroutine thermopack_twophase_dhdt

subroutine get_phase_flags(iTWOPH,iLIQPH,iVAPPH,iMINGIBBSPH,iSINGLEPH,iSOLIDPH,iFAKEPH)
  ! Get the internal integer flags for various phase states and
  ! specifications.
  use thermopack_constants, only: TWOPH,LIQPH,VAPPH,MINGIBBSPH,SINGLEPH,SOLIDPH,FAKEPH
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

!-----------------------------------------------------------------!
subroutine thermopack_entropy_tv_c(t,v,n,s) BIND(C)
  !---------------------------------------------------------------!
  !> External interface for the entropy routine
  !> in eosTV.f90
  !> Notes: Optional arguments not included here.
  !> \author MH, 2019-04-15
  !---------------------------------------------------------------!
  use, intrinsic :: ISO_C_BINDING
  use eosTV, only: entropyTV
  use thermopack_var, only: nc
  implicit none
  !Input:
  real(C_double), intent(in) :: t !< K - Temperature
  real(C_double), intent(in) :: v !< m3 - Volume
  real(C_double), dimension(1:nc), intent(in) :: n !< Mol numbers
  real(C_double), intent(out) :: s !< J/K - Entropy
  !---------------------------------------------------------------!
  call entropyTV(t,v,n,s)
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
subroutine thermopack_moleweight_c(z,mw) BIND(C)
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
