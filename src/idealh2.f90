module idealh2
  implicit none
  save

  ! Author Geir S (2017-03-24)
  !
  ! Containes the Leachman / (NIST) parameters for cpideal for para-, ortho- and
  ! normalhydrogen and the Valenta parameters for equilibrium hydrogen.
  !
  ! Used when setting the cp-method to 10 in compdatadb
  ! This is done for: N-H2, O-H2, P-H2 and E-H2
  !
  ! This routines will then be automatically used with cubic equations of state.
  ! The full Helmholtz-free energy equation (NIST_MEOS family) uses that ideal
  ! helmholz free energy paramters that are stored locally in the apppropriate
  ! modules
  !
  ! The expression used is this:
  !   Cp,0/R = 2.5 + sum_k=1,N(k) [u(k) * (v(k)/T)^2 ! * exp(v(k)/T) / (exp(v(k)/T)-1)^2]
  !
  ! Units: T[K] and Cp,0 in J/mol K
  !
  ! The parameters can only be used for T > 30 K, at lower temperature the
  ! two-last terms causes (exp(v(k)/T) to overflow. The resulting term
  ! approaches zero
  !
  ! Data from Table 3 in Leachman et.al
  ! "Fundamental Equations of State for Parahydrogen, Normal Hydrogen, and Orthohydrogen" doi:10.1063/1.3160306

  ! For equilibrium Hydrogen: Table 2 in
  ! "The influence of the thermodynamic model of equilibriumhydrogen on the
  ! simulation of its liquefaction" Gianluca Valenti*, Ennio Macchi, Samuele
  ! Brioschi

  ! Array order P-H2, N-H2, O-H2, E-H2
  integer, parameter, dimension (4) :: Nk = (/7, 5, 4, 7/)

  real, parameter, dimension(4,7) :: uk = reshape( (/ &
       4.30256, 13.0289, -47.7365, 50.0013, -18.6261, 0.993973, 0.53678, & ! P-H2
       1.616,   -0.4117, -0.792,     0.758,   1.217,  0.0,      0.0, & ! N-H2
       2.54151, -2.3661, 1.00365,  1.22447,   0.0,    0.0,      0.0, & ! O-H2
       29.5750,-83.1270, 54.4050,  1.84050, -1.80600,2.69390,-2.09290 /), & ! E-H2
       (/4,7/),order=(/2,1/))

  real, parameter, dimension(4,7) :: vk = reshape( (/ &
       499.0, 826.5,  970.8,  1166.2, 1341.4, 5395.,   10185., &  ! P-H2
       531.0,   751.,   1989.,  2484.,   6859.,      0.,        0., & ! N-H2
       856.,  1444.,   2194.,  6968.,   0.,         0.,        0., & ! O-H2
       201.9, 263.5, 294.7, 1046.3, 1241.5, 5394.8, 10185. /), & !E -H2
       (/4,7/),order=(/2,1/))

  real, parameter ::  rgas = 8.3144772 ! As used by NIST in the fitting

  ! Table 8 for vapour pressure
  real, parameter,dimension(4,4) :: Nvap = reshape( (/ &
       -4.87767, 1.03359, 0.82668, -0.129412, & ! P-H2
       -4.89789, 0.988558,0.349689,0.499356, & ! N-H2
       -4.88684, 1.05310, 0.856947,-0.185355, & ! O-H2
       -4.87767, 1.03359, 0.82668, -0.129412 /), & ! E-H2 = P-H2
       (/4,4/),order=(/2,1/))

   real, parameter,dimension(4,4) :: kvap = reshape( (/ &
       1.0, 1.5, 2.65, 7.7, & ! P-H2
       1.0, 1.5, 2.0, 2.85, & ! N-H2
       1.0, 1.6, 2.7, 6.2, & ! O-H2
       1.0, 1.5, 2.65, 7.7 /), & ! E-H2 = P-H2
       (/4,4/),order=(/2,1/))

   ! Adjust the reference enthalpy so that Delta H becomes right
   ! J/kmol (Ortho hydrogen has been set as reference, h0_ref=0
   ! and adjust s0_ref such as min G corresponds to the reaction
   ! equilibrium: OW, 08.10.2018
   real, parameter, dimension (4) :: h0_ref = (/-1413.6, -354.4, 0.0, -1413.6/)
                                              !  P-H2     N-H2   O-H2  E-H2
   real, parameter, dimension (4) :: s0_ref = (/ 0.0, 13.7009, 18.2678, 0.0/)
                                              !  P-H2 N-H2  O-H2  E-H2

  public :: CPIdeal_H2, HIdeal_H2, SIdeal_H2 !, seth2refstate
  public :: lnpvapred_H2

  interface h2func
     module procedure geth2index
  end interface h2func

! ... make this public ...
!  interface h2subs
!     module procedure seth2refstate
!  end interface h2subs

contains

  integer function geth2index (ident) result (idx)
    use stringmod, only: str_eq
    implicit none

    character (len=*), intent(in) :: ident

    if (str_eq(ident,'P-H2')) then
       idx = 1
    elseif (str_eq(ident,'N-H2')) then
       idx = 2
    elseif (str_eq(ident,'O-H2')) then
       idx = 3
    elseif (str_eq(ident,'E-H2')) then
       idx = 4
    else
       write (*,*) 'Fluid: "',ident,'"'
       call stoperror ('Ideal H2: Fluid not recognized as Hydrogen')
    endif
  end function geth2index

!  !
!  !
!  ! The reference state for H2 is to assign h0=0, s0=0 at saturated liquid at
!  ! the normal boiling point of 1.01325E5 Pa
!  !
!  subroutine seth2refstate (t0, p0, rho0, h0, s0)
!    use eos, only: specificVolume, getcriticalparam
!
!    implicit none
!    real, intent(inout) :: t0,p0,rho0,h0,s0
!
!    real, dimension(1) :: z0
!    real :: tc,pc,acf,vc,tnb
!
!    real :: v0
!    integer :: phase
!
!    z0(1) = 1.0
!
!    h0 = 0.0
!    s0 = 0.0
!    p0 = 1.01325E5
!
!    call getCriticalParam(1,tc,pc,acf,vc,tnb)
!    t0 = tnb
!
!    ! .. or for consistancy ??
!    !   solveForT = .true.
!    !   call puresat(t0,p0,z0,solveForT);
!    phase = 1.0
!    call specificVolume(t0,p0,z0,phase,v0)
!    rho0 = 1.0/v0
!  end subroutine seth2refstate


  function CPIdeal_H2 (ident, T) result (Cp_id)
    implicit none

    character (len=*), intent(in) :: ident
    real, intent(in) :: T ! Temperature [K]

    real :: vr, expvr,cp0
    real :: CP_Id ! J / kmol K
    integer :: idx,k, kstp

    !integer :: geth2index

    idx = geth2index(ident)

    cp0 = 2.5

    ! Below 30 K the exp(v(k)/T) becomes too large - the resulting term approaches zero, so for the lowest temperaures
    ! these are omitted
    !
    if (T > 30) then
       kstp = Nk(idx)
    else
       kstp = Nk(idx) - 2;
    end if

    do k=1,kstp
       vr = vk(idx,k)/T
       expvr = exp(vr)
       cp0 = cp0 + uk(idx,k) * vr*vr * expvr /((expvr-1)*(expvr-1))
    enddo
    CP_id = 1000 * cp0 * rgas ! J/mol K -> J/kmol K

  end function CPIdeal_H2


  function HIdeal_H2 (ident, T) result (H_id)
    implicit none

    character (len=*), intent(in) :: ident
    real, intent(in) :: T ! Temperature [K]

    real :: vr,expvr,h0
    real :: H_Id ! J /kmol
    integer :: idx,k, kstp

!    integer :: geth2index
    idx = geth2index(ident)

    if (T > 30) then
       kstp = Nk(idx)
    else
       kstp = Nk(idx) - 2;
    end if

    h0 = 2.5*T
    do k=1,kstp
       vr = vk(idx,k)/T
       expvr = exp(vr)
       h0 =  h0 + uk(idx,k) * vk(idx,k)/(expvr-1)
    enddo
    H_id = 1000.0 * h0_ref(idx)+1000.0 * h0 * rgas !  J/mol -> J/kmol
  end function HIdeal_H2

  function SIdeal_H2 (ident, T) result (s_id)
    implicit none

    character (len=*), intent(in) :: ident
    real, intent(in) :: T ! Temperature [K]

    real :: vr,expvr,s0
    real :: S_Id ! J/ kmol K
    integer :: idx,k, kstp

!    integer :: geth2index
    idx = geth2index(ident)

    if (T > 30) then
       kstp = Nk(idx)
    else
       kstp = Nk(idx) - 2;
    end if

    s0 = 2.5*log(T)
    do k=1,kstp
       vr = vk(idx,k)/T
       expvr = exp(vr)
       s0 = s0 + uk(idx,k) * (vr * expvr / (expvr-1)  -  log(expvr-1)) ! Without the reference volume term ...
    enddo
    s_id = 1000.0 * s0_ref(idx)+1000.0 * rgas * s0 ! log(v*T/(v0*T0))! J/mol K -> J/kmol K
    ! .. in calling procedure: need to add the term: rgas*ln( (t*rho) / t0*rho0))
  end function SIdeal_H2


  ! The ancillary equation for satturated vapour pressure from Leachman
  ! (eq. 33, with coefficients from Table 8)

  function lnpvapred_H2 (ident, Tr) result (lnpvapr)
    implicit none

    character (len=*), intent(in) :: ident
    real, intent(in) :: Tr ! Reduced temperature T/Tc

    real :: teta
    integer :: idx,k
    real :: lnpvapr

    idx = geth2index(ident)
    teta = 1.0 - Tr

    lnpvapr = 0.0
    do k=1,4
       lnpvapr = lnpvapr + Nvap(idx,k)*teta**kvap(idx,k)
    end do
    lnpvapr = Tr * lnpvapr ! ln(psat/pcrit)
  end function lnpvapred_H2

end module idealh2

