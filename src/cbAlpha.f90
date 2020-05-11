!> Routines for setting and computing (T-dependent) alpha correlations in cubic
!> EoS.
!!
module cbAlpha
  implicit none
  private
  save
  public :: cbCalcAlphaTerm    !< Computing alpha at given temperature.
  public :: tpInitAlphaCorr    !< Set alpha correlations for all components.
  public :: setSingleAlphaCorr !< Set alpha correlation for a component.
  public :: getSingleAlphaCorr !< Set alpha correlation for a component.
  public :: getAlphaClassicParam !< Get classical alpha correlation paramaters for component.
  public :: nameToIdx
  logical, parameter :: issueWarnings = .true. !< Warnings to user via stdout stream.
contains

  function nameToIdx(corrName)
    use eosdata, only: alphaCorrNames, nAlphaCorrs
    use stringmod, only: str_eq
    character(len=*), intent(in) :: corrName
    integer :: nameToIdx
    integer :: i

    do i=1,nAlphaCorrs
       if ( str_eq(corrName,alphaCorrNames(i)) ) then
          nameToIdx = i
          return
       end if
    end do

    print *, "Possible alphacorrs:", alphaCorrNames
    call StopError ('nameToIdx::unknown alpha corr name ')
  end function nameToIdx

  !> Set alpha correlation and parameters for a component. If one wants to use
  !> value of alphaParams from the database, one has to retrieve them first
  !> using the appropriate get function, e.g. getAlphaMcParamsFromDb.
  !>
  !> \author Ailo 19.01.16.
  subroutine setSingleAlphaCorr(i, cbeos, corrName, alphaParams)
    use eosdata, only: eoscubic, alphaCorrNumParams
    use stringmod, only: str_eq
    integer, intent(in)            :: i
    type (eoscubic), intent(inout) :: cbeos
    character (len=*), intent (in) :: corrName
    real, intent(in)               :: alphaParams(:)
    ! Locals:
    integer :: alphaIdx, nParams

    ! Sanity check.
    if ( .not. allocated(cbeos%single) ) then
       call stoperror("cbeos%single not allocated.")
    end if

    ! Get alpha corr idx
    alphaIdx = nameToIdx(corrName)
    ! Set idx and name.
    cbeos%single(i)%alphaMethod = alphaIdx
    cbeos%single(i)%alphaCorrName = corrName
    ! Get number of alpha params for the correlation
    nParams = alphaCorrNumParams(alphaIdx)
    ! Set alpha params
    if ( nParams > 0 ) then
       cbeos%single(i)%alphaParams(1:nParams) = alphaParams(1:nParams)
    end if

  end subroutine setSingleAlphaCorr

  !> Get active alpha correlation parameters.
  !>
  !> \author Ailo 19.01.16.
  subroutine getSingleAlphaCorr(i, cbeos, corrName, numparam, alphaParams)
    use eosdata, only: eoscubic, alphaCorrNumParams
    use compdata, only: gendata
    use stringmod, only: str_eq
    integer, intent(in)            :: i
    type (eoscubic), intent(inout) :: cbeos
    character (len=*), intent (in) :: corrName
    integer, intent(in)            :: numparam
    real, intent(out)              :: alphaParams(numparam)
    ! Locals:
    integer :: alphaIdx

    ! Get idx
    alphaIdx = nameToIdx(corrName)

    ! Sanity checks.
    if ( .not. allocated(cbeos%single) ) then
       call stoperror("cbeos%single not allocated.")
    else if ( alphaCorrNumParams(alphaIdx) /= numparam ) then
       call stoperror("numparam for alpha corr is wrong")
    end if

    alphaParams = cbeos%single(i)%alphaParams(1:numparam)

  end subroutine getSingleAlphaCorr

  !> Initializes alpha correlations (the same correlation for all components).
  !
  !> This routine is invoked after eoslibinit as one of the subsequent
  !> initialization calls. To change correlation, one has to call
  !> setSingleAlphaCorr.
  !
  !>\author Ailo Aasen
  subroutine tpInitAlphaCorr(nc, comp, cbeos, alphastr, setno)
    use eosdata
    use compdata, only: gendata
    use stringmod, only: str_eq
    implicit none
    integer, intent(in) :: nc
    type(gendata), intent(in) :: comp(nc)
    type (eoscubic), intent(inout) :: cbeos
    character (len=*), intent (in) :: alphastr
    integer, optional, intent(in) :: setno
    !
    integer :: i, ierror, setno_loc
    real :: params(3)
    character (len=25) :: corrName
    setno_loc = 0 ! 0 means "take first parameter set found"
    if (present(setno)) setno_loc = setno
    do i = 1,nc
      params = 0.0
      ierror = 0 ! Becomes nonzero if parameters not in database.
      corrName = trim(alphastr)

      ! If 'MC' or 'TWU', try to find parameters in the database.
      if (str_eq(alphastr, 'TWU')) then
        call getAlphaTwuParamsFromDb(i, comp, cbeos, twuParams=params, &
             setno=setno_loc, ierror=ierror)
      elseif (str_eq(alphastr, 'MC')) then
        call getAlphaMcParamsFromDb(i, comp, cbeos, mcParams=params,&
             setno=setno_loc, ierror = ierror)
      elseif (str_eq(alphastr, 'ALPHA_UMR')) then
        if (str_eq(comp(i)%ident,"H2O")) then
          corrName = 'MC'
          call getAlphaMcParamsFromDb(i, comp, cbeos, mcParams=params,&
               setno=setno_loc, ierror = ierror)
        else
          corrName = 'CLASSIC'
          call getAlphaUMRParam(i, comp, params)
        endif
      elseif (str_eq(alphastr, 'CLASSIC')) then
        call getAlphaClassicParam(i, cbeos, params)
      endif

      if ( ierror /= 0 ) then
        corrName = "CLASSIC"
        ! Error when retrieving from database for 'MC' or 'TWU'. Default to classic alpha corr.
        if (issueWarnings) print *, "Using classic alpha corr for ", comp(i)%ident
        call getAlphaClassicParam(i, cbeos, params)
      end if
      call setSingleAlphaCorr(i=i, cbeos=cbeos, &
           corrName=corrName, alphaParams=params)
    end do

  end subroutine tpInitAlphaCorr

  !> Calculates the alpha term for all components.
  subroutine cbCalcAlphaTerm (nc,comp,cbeos,T)
    use eosdata
    use compdata, only: gendata
    implicit none
    integer, intent(in) :: nc
    type (gendata), intent(in), dimension(nc) :: comp
    type(eoscubic), intent(inout) :: cbeos
    real, intent(in) :: T
    ! Local variables
    real :: tci,acfi,tr
    integer :: i, low, high
    low = 1
    high = nc
    do i=low,high
      tci = cbeos%single(i)%tc !< Critical temperature component, i
      if (tci <= 0.0) then
        ! Ion or salt...
        cbeos%single(i)%alpha = 0.0
        cbeos%single(i)%dalphadT = 0.0
        cbeos%single(i)%d2alphadT2 = 0.0
        cycle
      endif
      acfi = cbeos%single(i)%acf !< Acentric factor of component i

      tr = T/tci !< Reduced temperature component, i

      select case (cbeos%eosidx)
      case (cbVdW)
         cbeos%single(i)%alpha = 1.0
         cbeos%single(i)%dalphadt = 0.0D0
         cbeos%single(i)%d2alphadt2 = 0.0D0
      case (cbRK)
         cbeos%single(i)%alpha = 1.0/sqrt(tr)
         cbeos%single(i)%dalphadt = -0.5/(T*sqrt(tr))
         cbeos%single(i)%d2alphadt2 = 0.75/(T*T*sqrt(tr))
      case (cbSRK,cbSRKGB,cbPR,cbPT,eosLK,cspSRK,cspSRKGB,cspPR,cpaSRK,cpaPR)
         if (cbeos%cubic_verbose) then
            write (*,*) 'alphamethod for comp 1: ', cbeos%single(1)%alphamethod
         endif
         ! TWU
         if (cbeos%single(i)%alphamethod .eq. cbAlphaTwuIdx) then
            if (cbeos%single(i)%alphaParams(1) .ne. 0.0 &
                 .and. cbeos%single(i)%alphaParams(2) .ne. 0.0 &
                 .and. cbeos%single(i)%alphaParams(3) .ne. 0.0) then ! twu-coon-bluck- is safe
               call calcAlpha_twu (cbeos, i, T,tci)
            else
               call calcAlpha_classic(cbeos, i,T, tci, cbeos%single(i)%alphaParams(1))
            endif
            ! Mathias-Copeman
         else if (cbeos%single(i)%alphamethod .eq. cbAlphaMCIdx) then
            if (cbeos%single(i)%alphaParams(1) .ne. 0.0 &
                 .and. cbeos%single(i)%alphaParams(2) .ne. 0.0 &
                 .and. cbeos%single(i)%alphaParams(3) .ne. 0.0) then
               call calcAlpha_MC (cbeos, i,T, Tci)
            else
               call calcAlpha_classic(cbeos, i,T, tci, cbeos%single(i)%alphaParams(1))
            endif
         else ! Standard alpha-corr
            call calcAlpha_classic(cbeos, i,T, tci, cbeos%single(i)%alphaParams(1))
         endif
         ! Schmidt and Wensel variant
      case (cbSW)
         call calcAlpha_SW(cbeos, i,T, tci, acfi)
      end select
   end do
 end subroutine cbCalcAlphaTerm

 subroutine getAlphaUMRParam(ic, comp, params)
   use compdata, only: gendata
   implicit none
   type(gendata), intent(in) :: comp(:)
   integer, intent (in) :: ic
   real, intent (out) :: params(3)
   ! Locals
   real :: acfi

   acfi = comp(ic)%acf ! ... cbeos%single(ic)%acf
   params = 0.0
   params(1) = 0.384401 + 1.52276*acfi - 0.213808*acfi**2 + 0.034616*acfi**3 - 0.001976*acfi**4

 end subroutine getAlphaUMRParam

 subroutine getAlphaClassicParam(ic, cbeos, params)
   use eosdata, only: eoscubic
   implicit none
   type(eoscubic), intent(inout) :: cbeos
   integer, intent (in) :: ic
   real, intent(out) :: params(3)
   ! Locals
   real :: acfi

   acfi = cbeos%single(ic)%acf
   params = 0.0
   params(1) = cbeos%alfa + cbeos%beta*acfi - cbeos%gamma*acfi**2 !< m(omega) in srk,pr and k0 in sw

 end subroutine getAlphaClassicParam

  !> In this routine, a fitted parameter c_1 is used instead of the usual
  !> polynomial expression of the acentric factor, alfa + beta*acf - gamma*acf**2.
  !> It is also possible to use other alpha-formulations for CPA, by
  !> specifying it in the usual way in eoslibinit.
  subroutine calcAlpha_classic (cbeos,ic,T,tci,c1_value_i)
    use eosdata, only: eoscubic
    implicit none
    type(eoscubic), intent(inout) :: cbeos
    integer, intent (in) :: ic
    real, intent (in) :: T, tci, c1_value_i

    real :: tr, sqrt_Ttci, s, xx, yy

    tr = T/tci !< Reduced temperature of component i
    s = c1_value_i

    xx = s**2
    yy = -2.0*s*(s+1.0)

    sqrt_Ttci = sqrt(T*tci)
    cbeos%single(ic)%alpha = 1.0E0 + xx*(tr-1.0E0) + yy*(sqrt(tr)-1.0E0)
    cbeos%single(ic)%dalphadT = xx/tci + 0.5*yy/sqrt_Ttci
    cbeos%single(ic)%d2alphadT2 = -yy/(4*T*sqrt(T*tci))
    ! MH: Should alpha really go negative?
    !if (cbeos%single(ic)%alpha < 1.0e-13) then
    !  cbeos%single(ic)%alpha = 0.0
    !  cbeos%single(ic)%dalphadT = 0.0
    !  cbeos%single(ic)%d2alphadT2 = 0.0
    !endif
  end subroutine calcAlpha_classic

  subroutine calcAlpha_twu (cbeos,ic, T,tci)
    use eosdata, only: eoscubic
    implicit none
    type(eoscubic), intent(inout) :: cbeos
    integer, intent (in) :: ic
    real, intent (in) :: T, tci

    real :: tr
    real :: a1,a2,a3, b1,b2,c1,c2,c3 ! Parameters for Twu-Coon-Bluck alpha(T) formulation

    tr = t/tci !< Reduced temperature component, i

    c1=cbeos%single(ic)%alphaParams(1)
    c2=cbeos%single(ic)%alphaParams(2)
    c3=cbeos%single(ic)%alphaParams(3)
    b1 = c3*(c2-1.0)
    b2 = c3*c2
    a1 = tr**b1
    a2 = tr**b2
    a3 = exp(c1*(1.0-a2))

    ! Since component index is input, the cbeos%alpha(ic) could be set directly here
    ! instead of transfering it back
    cbeos%single(ic)%alpha      =  a1*a3
    cbeos%single(ic)%dalphadt   = -a1*a3*(-b1 + c1*b2*a2) / T
    cbeos%single(ic)%d2alphadt2= a1*a3*(b1*b1 - b1-2*b1*c1*a2*b2 &
         - c1*a2*b2*b2 + c1*a2*b2+c1*c1*a2*a2*b2*b2)/(T*T)
  end subroutine calcAlpha_twu

  subroutine calcAlpha_MC (cbeos, ic, T, Tci)
    use eosdata, only: eoscubic
    implicit none
    type(eoscubic), intent(inout) :: cbeos
    integer, intent (in) :: ic ! The component index
    real, intent (in) :: T, tci

    real :: tr
    real :: yf, yfT, yfTT
    real :: a,b,c
    real xx, xxt, xxtt
    integer :: nPot  !Potense:
    nPot = 2
    ! Before it was 1: Now it is increased to 2 in C. Coquette

    tr =T/tci
    a = cbeos%single(ic)%alphaParams(1)
    if ( tr < 1.0 ) then
       b = cbeos%single(ic)%alphaParams(2)
       c = cbeos%single(ic)%alphaParams(3)
    else
       b = 0.0
       c = 0.0
    endif

    yf = 1.0 - sqrt(tr)
    yfT = - 0.5 / (sqrt(tr)*Tci)  !dyf/dT
    yfTT = 0.25/((Tr**1.5)*Tci*Tci) !d²yf/dT²

    ! Alhpa = xx**nPot
    xx = 1 + a * yf + b * yf**2 + c * yf**3
    xxt=     a * yfT + 2*b*yfT*yf + 3*c*yfT*yf**2
    xxtt =   a * yfTT + 2*b*(yfTT*yf+yfT*yfT) + 3*c*(yfTT * yf**2 + 2*yfT**2*yf)

    cbeos%single(ic)%alpha = xx**nPot
    cbeos%single(ic)%dalphadt = nPot*xxt*xx**(nPot-1)
    cbeos%single(ic)%d2alphadt2 = nPot*xxtt*xx**(nPot-1) + nPot*(nPot-1)*xxt**2*xx**(nPot-2)
  end subroutine calcAlpha_MC

  !< Special calculation of alpha-term for the Shmidt and Wensel eos
  subroutine calcAlpha_SW (cbeos, ic, T, Tci, acfi)
    use eosdata, only: eoscubic
    implicit none
    type(eoscubic), intent(inout) :: cbeos
    integer, intent (in) :: ic
    real, intent (in) :: T, tci,acfi
    !
    real :: tr, sqrt_tr, alpha, dtrdt
    real :: kappa0, kappa, dkappadTr, d2kappadTr2
    real :: argum, sqrt_alpha, dalphadtr, d2alphadtr2

    tr = t/tci
    sqrt_tr = sqrt(tr)
    dtrdt = 1.0/tci
    kappa0 = cbeos%alfa + cbeos%beta*acfi - cbeos%gamma*acfi**2 !< fw in srk,pr and k0 in sw

    if (tr > 1.0) then
      argum = 5.0-3*kappa0-1.0
      kappa = kappa0 + argum*argum / 70.0
      dkappadTr = 0.0
      d2kappadTr2 = 0.0
    else
      argum = 5.0*Tr-3*kappa0-1.0
      kappa = kappa0 + argum*argum / 70.0
      dkappadTr = 2 * argum / 70.0 * 5.0
      d2kappadTr2 = 5.0 / 7.0
    endif

    sqrt_alpha = 1.0 + kappa*(1.0-sqrt_tr)
    alpha = sqrt_alpha**2
    dalphadtr = 2.0*sqrt_alpha*(dkappadTr*(1.0-sqrt_tr)-0.5*kappa/sqrt_tr)
    d2alphadtr2 = dalphadtr**2/(2.0*alpha) + 2.0*sqrt_alpha*( &
         + d2kappadTr2*(1.0-sqrt_tr) + 0.25*kappa/(tr*sqrt_tr) &
         - dkappadTr/sqrt_tr)

    cbeos%single(ic)%alpha = alpha
    cbeos%single(ic)%dalphadt = dalphadtr*dtrdt
    cbeos%single(ic)%d2alphadt2 = d2alphadtr2*dtrdt**2
  end subroutine calcAlpha_SW

  !> Retrieve TWU parameters from database.
  !> \author GS, Ailo
  subroutine getAlphaTwuParamsFromDb(i, comp, cbeos, twuParams, setno, ierror)
    use eosdata, only: eoscubic
    use eosdatadb, only: alphaTWUdb, maxalphaTWUparam
    use compdata, only: gendata
    use stringmod, only: str_eq
    ! Input:
    integer, intent(in) :: i
    type(gendata), intent(in) :: comp(:)
    type(eoscubic), intent(in) :: cbeos
    integer, optional, intent(in) :: setno !< Not yet used
    ! Output:
    real, intent(out) :: twuParams(3)
    integer, optional, intent(out) :: ierror
    ! Locals
    logical :: found
    integer :: idx, setno_loc

    setno_loc = 0
    if (present(setno)) setno_loc = setno

    idx = 1
    found = .false.
    do while ( idx <= maxalphaTWUparam .and. (.not. found) )
       if  (str_eq(comp(i)%ident,alphaTWUdb(idx)%uid) .and.  &
            str_eq(cbeos%eosid,alphaTWUdb(idx)%eosid) .and. &
            (alphaTWUdb(idx)%setno == setno_loc .or. setno_loc == 0) )  then
          found = .true.
          twuParams(1:3) = alphaTWUdb(idx)%coeff(1:3)
       endif
       idx = idx+1
    end do

    ierror = 0
    if (.not. found) then
       ierror = 1
    end if

  end subroutine getAlphaTwuParamsFromDb


  !> Retrieve MC alpha parameters from database.
  !> \author GS, Ailo
  subroutine getAlphaMcParamsFromDb(i, comp, cbeos, mcParams, setno, ierror)
    use eosdata, only: eoscubic
    use eosdatadb, only: alphaMCdb, maxalphaMCparam
    use compdata, only: gendata
    use stringmod, only: str_eq
    ! Input:
    integer, intent(in) :: i
    type(gendata), intent(in) :: comp(:)
    type(eoscubic), intent(in) :: cbeos
    integer, optional, intent(in) :: setno ! 0 or not present: use first set found
    ! Output:
    real, intent(out) :: mcParams(3)
    integer, optional, intent(out) :: ierror
    ! Locals:
    logical :: found
    integer :: idx, setno_loc

    setno_loc = 0
    if (present(setno)) setno_loc = setno

    idx = 1
    found = .false.
    do while ( idx <= maxalphaMCparam .and. (.not. found) )
       if (str_eq(comp(i)%ident,alphaMCdb(idx)%uid) .and.  &
            str_eq(cbeos%eosid,alphaMCdb(idx)%eosid) .and. &
            (alphaMCdb(idx)%setno == setno_loc .or. setno_loc == 0) ) then
          found = .true.
          mcParams(1:3) = alphaMCdb(idx)%coeff(1:3)
       endif
       idx = idx+1
    end do

    ierror = 0
    if (.not. found) then
       ierror = 1
    end if

  end subroutine getAlphaMcParamsFromDb

end module cbAlpha
