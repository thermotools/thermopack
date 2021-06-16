!> Routines for setting and computing (T-dependent) alpha correlations in cubic
!> EoS.
!!
module cbAlpha
  use stringmod, only: str_eq, string_match_val
  implicit none
  private
  save
  public :: cbCalcAlphaTerm    !< Computing alpha at given temperature.
  public :: tpInitAlphaCorr    !< Set alpha correlations for all components.
  public :: setSingleAlphaCorr !< Set alpha correlation for a component.
  public :: getSingleAlphaCorr !< Set alpha correlation for a component.
  public :: getAcentricAlphaParam !< Get classical alpha correlation paramaters for component.
  logical, parameter :: issueWarnings = .true. !< Warnings to user via stdout stream.
contains

  !> Set alpha correlation and parameters for a component. If one wants to use
  !> value of alphaParams from the database, one has to retrieve them first
  !> using the appropriate get function, e.g. getAlphaMcParamsFromDb.
  !>
  !> \author Ailo 19.01.16.
  subroutine setSingleAlphaCorr(i, cbeos, alphaIdx, alphaParams)
    use cubic_eos, only: get_alpha_db_idx_from_alpha_idx, &
         alpha_corr_db, cb_eos
    integer, intent(in)            :: i
    class(cb_eos), intent(inout) :: cbeos
    integer, intent (in) :: alphaIdx
    real, intent(in)               :: alphaParams(:)
    ! Locals:
    integer :: idx_db, nParams

    ! Sanity check.
    if ( .not. allocated(cbeos%single) ) then
       call stoperror("cbeos%single not allocated.")
    end if

    idx_db = get_alpha_db_idx_from_alpha_idx(alphaIdx)
    ! Set idx and name.
    cbeos%single(i)%alphaMethod = alphaIdx
    cbeos%single(i)%alphaCorrName = alpha_corr_db(idx_db)%short_label
    ! Get number of alpha params for the correlation
    nParams = alpha_corr_db(idx_db)%n_param
    ! Set alpha params
    if ( nParams > 0 ) then
       cbeos%single(i)%alphaParams(1:nParams) = alphaParams(1:nParams)
    end if

  end subroutine setSingleAlphaCorr

  !> Get active alpha correlation parameters.
  !>
  !> \author Ailo 19.01.16.
  subroutine getSingleAlphaCorr(i, cbeos, corrName, numparam, alphaParams)
    use cubic_eos, only: get_alpha_db_idx, alpha_corr_db, cb_eos
    integer, intent(in)            :: i
    class(cb_eos), intent(inout) :: cbeos
    character (len=*), intent (in) :: corrName
    integer, intent(in)            :: numparam
    real, intent(out)              :: alphaParams(numparam)
    ! Locals:
    integer :: alpha_idx_db

    ! Get idx
    alpha_idx_db = get_alpha_db_idx(corrName)

    ! Sanity checks.
    if ( .not. allocated(cbeos%single) ) then
       call stoperror("cbeos%single not allocated.")
    else if ( alpha_corr_db(alpha_idx_db)%n_param /= numparam ) then
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
  subroutine tpInitAlphaCorr(nc, comp, cbeos, alphastr, alpha_reference)
    use cubic_eos, only: get_alpha_db_idx, alpha_corr_db, cb_eos, &
         eos_to_classic_alpha_db_idx, cbAlphaTwuIdx, cbAlphaMcIdx, &
         cbAlphaUMRIdx, cbAlphaGergIdx, cbAlphaClassicIdx, cbAlphaPR78Idx
    use compdata, only: gendata_pointer
    implicit none
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    class(cb_eos), intent(inout) :: cbeos
    character(len=*), intent (in) :: alphastr
    character(len=*), optional, intent(in) :: alpha_reference
    !
    integer :: i, ierror, idx_db, istat, corr_idx, idx_db_classic
    real :: params(3)
    character(len=:), allocatable :: alpha_ref
    logical :: inDB
    if (present(alpha_reference)) then
      allocate(character(len=len_trim(alpha_reference)) :: alpha_ref, stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate alpha_ref")
      alpha_ref = alpha_reference
    else
      allocate(character(len=8) :: alpha_ref, stat=istat)
      if (istat /= 0) call stoperror("Not able to allocate alpha_ref")
      alpha_ref = "DEFAULT"
    endif
    ! Get database entry
    idx_db = get_alpha_db_idx(alphastr)
    if (idx_db < 0) then
      call stoperror('unknown alpha')
    endif

    ! Map classic id to actual alpha
    idx_db_classic = eos_to_classic_alpha_db_idx(cbeos%subeosidx)
    if (idx_db_classic < 0) then
      ! Use Soave as default
      idx_db_classic = get_alpha_db_idx("SOAVE")
    endif
    !
    do i = 1,nc
      params = 0.0
      ierror = 0 ! Becomes nonzero if parameters not in database.
      corr_idx = alpha_corr_db(idx_db)%alpha_idx
      select case(corr_idx)
      case(cbAlphaTwuIdx)
        ! If 'MC' or 'TWU', try to find parameters in the database.
         inDB = getAlphaTwuParamsFromDb(comp(i)%p_comp%ident,cbeos%eosid, twuParams=params, &
              ref=alpha_reference)
         if (.not. inDB) ierror = 1
        if (params(1) == 0.0 .and. params(2) == 0.0 .and. params(3) == 0.0) then
          ierror = 1
        endif
      case(cbAlphaMcIdx)
         inDB = getAlphaTwuParamsFromDb(comp(i)%p_comp%ident,cbeos%eosid, twuParams=params, &
              ref=alpha_reference)
         if (.not. inDB) ierror = 1
        if (params(1) == 0.0 .and. params(2) == 0.0 .and. params(3) == 0.0) then
          ierror = 1
        endif
      case(cbAlphaUMRIdx)
        if (str_eq(comp(i)%p_comp%ident,"H2O")) then
          corr_idx = cbAlphaMcIdx
          inDB = getAlphaMCParamsFromDb(comp(i)%p_comp%ident,cbeos%eosid, mcParams=params, &
               ref=alpha_reference)
          if (.not. inDB) ierror = 1
          if (params(1) == 0.0 .and. params(2) == 0.0 .and. params(3) == 0.0) then
            ierror = 1
          endif
        else
          call getAcentricAlphaParam(cbAlphaUMRIdx, cbeos%single(i)%acf, params)
        endif
      case(cbAlphaGergIdx)
        ierror = 1
      case(cbAlphaClassicIdx)
        corr_idx = alpha_corr_db(idx_db_classic)%alpha_idx
        call getAcentricAlphaParam(corr_idx, cbeos%single(i)%acf, params)
      case(cbAlphaPR78Idx)
        call getAcentricAlphaParam(corr_idx, cbeos%single(i)%acf, params)
      end select

      if ( ierror /= 0 ) then
        corr_idx = alpha_corr_db(idx_db_classic)%alpha_idx
        ! Error when retrieving from database for 'MC' or 'TWU'. Default to classic alpha corr.
        if (issueWarnings) print *, "Using classic alpha corr for ", comp(i)%p_comp%ident
        call getAcentricAlphaParam(corr_idx, cbeos%single(i)%acf, params)
      end if
      call setSingleAlphaCorr(i=i, cbeos=cbeos, &
           alphaIdx=corr_idx, alphaParams=params)
    end do

    deallocate(alpha_ref, stat=istat)
    if (istat /= 0) call stoperror("Not able to deallocate alpha_ref")
  end subroutine tpInitAlphaCorr

  !> Calculates the alpha term for all components.
  subroutine cbCalcAlphaTerm (nc,cbeos,T)
    use cubic_eos, only: cb_eos, cbAlphaVDWIdx, cbAlphaClassicIdx, cbAlphaClassicFitIdx, &
         cbAlphaGBIdx, cbAlphaRKIdx, cbAlphaSoaveIdx, cbAlphaPRIdx, &
         cbAlphaPTIdx, cbAlphaVDWIdx, cbAlphaRKIdx, cbAlphaTwuIdx, &
         cbAlphaMCIdx, cbAlphaSWIdx, cbAlphaGergIdx, cbAlphaRKIdx, &
         cbAlphaPR78Idx
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
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
      if (cbeos%cubic_verbose) then
        write (*,*) 'alphamethod for comp 1: ', cbeos%single(1)%alphamethod
      endif
      select case (cbeos%single(i)%alphaMethod)
      case (cbAlphaVDWIdx)
        cbeos%single(i)%alpha = 1.0
        cbeos%single(i)%dalphadt = 0.0D0
        cbeos%single(i)%d2alphadt2 = 0.0D0
      case (cbAlphaRKIdx)
        cbeos%single(i)%alpha = 1.0/sqrt(tr)
        cbeos%single(i)%dalphadt = -0.5/(T*sqrt(tr))
        cbeos%single(i)%d2alphadt2 = 0.75/(T*T*sqrt(tr))
      case (cbAlphaTwuIdx)
        call calcAlpha_twu(cbeos, i, T,tci)
      case (cbAlphaMCIdx)
        call calcAlpha_MC(cbeos, i, T, Tci)
      case (cbAlphaClassicIdx, &
           cbAlphaClassicFitIdx, &
           cbAlphaGBIdx, &
           cbAlphaSoaveIdx, &
           cbAlphaPRIdx, &
           cbAlphaPTIdx, &
           cbAlphaPR78Idx)
        call calcAlpha_classic(cbeos, i,T, tci, cbeos%single(i)%alphaParams(1))
      case (cbAlphaSWIdx)
        call calcAlpha_SW(cbeos, i,T, tci, acfi)
      case (cbAlphaGergIdx)
        call stoperror("Don't know what to do for GERG alpha method")
     case default
        call stoperror("Don't know what to do for alpha method "//&
             trim(cbeos%single(i)%alphaCorrName))
      end select
    end do
  end subroutine cbCalcAlphaTerm

  subroutine getAcentricAlphaParam(alpha_idx, acfi, params)
    implicit none
    integer, intent(in) :: alpha_idx
    real, intent (in) :: acfi
    real, intent(out) :: params(3)
    ! Locals
    real :: a, b, c, d, e
    call get_alpha_param_acentric(alpha_idx, acfi, a, b, c, d, e)
    params = 0.0
    params(1) = a + b*acfi + c*acfi**2 + d*acfi**3 + e*acfi**4

  end subroutine getAcentricAlphaParam

  !> In this routine, a fitted parameter c_1 is used instead of the usual
  !> polynomial expression of the acentric factor, alfa + beta*acf - gamma*acf**2.
  !> It is also possible to use other alpha-formulations for CPA, by
  !> specifying it in the usual way in eoslibinit.
  subroutine calcAlpha_classic (cbeos,ic,T,tci,c1_value_i)
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
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
  end subroutine calcAlpha_classic

  subroutine calcAlpha_twu (cbeos,ic, T,tci)
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
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
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
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
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    integer, intent (in) :: ic
    real, intent (in) :: T, tci,acfi
    !
    real :: tr, sqrt_tr, alpha, dtrdt
    real :: kappa0, kappa, dkappadTr, d2kappadTr2
    real :: argum, sqrt_alpha, dalphadtr, d2alphadtr2

    tr = t/tci
    sqrt_tr = sqrt(tr)
    dtrdt = 1.0/tci
    kappa0 = cbeos%single(ic)%alphaParams(1) !< k0 in sw

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
  function getAlphaTwuParamsFromDb(cid,eos,TWUparams,ref) result(found_TWU)
    use compdatadb, only: alphaTWUdb, maxTWUdb
    character(len=*), intent(in) :: eos !< Eos string
    character(len=*), intent(in) :: cid !< Component
    real, intent(out) :: TWUparams(3)
    character(len=*), optional, intent(in) :: ref !< Reference string
    logical :: found_TWU, match
    integer :: idx_lowest, match_val
    ! Locals
    integer :: i
    found_TWU = .false.
    TWUparams = 0.0
    idx_lowest = 1000000
    do i=1,maxTWUdb
       if (str_eq(alphaTWUdb(i)%eosid, eos) .and. str_eq(alphaTWUdb(i)%cid, cid)) then
          if (.not. found_TWU) then ! we at least found one match
             TWUparams = alphaTWUdb(i)%coeff
          end if
          found_TWU = .true.

          call string_match_val(ref,alphaTWUdb(i)%ref,match,match_val)
          if (present(ref)) then ! check if there is a match with the ref
             call string_match_val(ref,alphaTWUdb(i)%ref,match,match_val)
             if (match .and. match_val<idx_lowest) then ! the match takes precedence
                idx_lowest = match_val
                TWUparams = alphaTWUdb(i)%coeff
             end if
          end if
       endif
    enddo
  end function getAlphaTwuParamsFromDb

  function getAlphaMCParamsFromDb(cid,eos,MCparams,ref) result(found_MC)
    use compdatadb, only: alphaMCdb, maxMCdb
    character(len=*), intent(in) :: eos !< Eos string
    character(len=*), intent(in) :: cid !< Component
    real, intent(out) :: MCparams(3)
    character(len=*), optional, intent(in) :: ref !< Reference string
    logical :: found_MC, match
    integer :: idx_lowest, match_val
    ! Locals
    integer :: i
    found_MC = .false.
    MCparams = 0.0
    idx_lowest = 1000000
    do i=1,maxMCdb
       if (str_eq(alphaMCdb(i)%eosid, eos) .and. str_eq(alphaMCdb(i)%cid, cid)) then
          if (.not. found_MC) then ! we at least found one match
             MCparams = alphaMCdb(i)%coeff
          end if
          found_MC = .true.

          call string_match_val(ref,alphaMCdb(i)%ref,match,match_val)
          if (present(ref)) then ! check if there is a match with the ref
             call string_match_val(ref,alphaMCdb(i)%ref,match,match_val)
             if (match .and. match_val<idx_lowest) then ! the match takes precedence
                idx_lowest = match_val
                MCparams = alphaMCdb(i)%coeff
             end if
          end if
       endif
    enddo
  end function getAlphaMCParamsFromDb



  !> S = a + b * w + c * w**2 + d * w**3 + e * w**4
  !! alpha = (1 + S(1-Tr**(1/2)))**2
  !! Acentric correlation used in cubic alpha
  !! \author Geir Skaugen
  !! \author Morten Hammer
  !!
  subroutine get_alpha_param_acentric(alpha_idx, acfi, a, b, c, d, e)
    use cubic_eos, only: cb_eos, cbAlphaVDWIdx, cbAlphaUMRIdx, &
         cbAlphaGBIdx, cbAlphaRKIdx, cbAlphaSoaveIdx, cbAlphaPRIdx, &
         cbAlphaPTIdx, cbAlphaVDWIdx, cbAlphaRKIdx, cbAlphaSWIdx, &
         cbAlphaPR78Idx
    implicit none
    integer, intent(in) :: alpha_idx
    real, intent(in) :: acfi
    real, intent(out) :: a, b , c, d, e
    !

    d = 0
    e = 0

    select case (alpha_idx)

    case (cbAlphaSoaveIdx)
      a = 0.48D+00
      b = 1.574D+00
      c = -0.176D+00

    case (cbAlphaPRIdx)
      a = 0.37464D+00
      b = 1.54226D+00
      c = -0.26992D+00

    case (cbAlphaGBIdx)
      a = 0.48508D+00
      b = 1.555171D+00
      c = -0.15613D+00

    case (cbAlphaVDWIdx)
      a = 1
      b = 0
      c = 0

    case (cbAlphaRKIdx)
      a = 1
      b = 0
      c = 0

    case (cbAlphaSWIdx) ! Schmidt-Wensel: eq. 13 in paper
      a = 0.465D+00
      b = 1.347D+00
      c = -0.528D+00

    case (cbAlphaPTIdx) ! Patel-Teja: Individual "S" called F by Patel-Teja, classic alpha-term.
      a = 0.452413D+00
      b = 1.30982D+00 !  Typo, from wrong eq in PT paper -0.076799
      c = -0.295937D+00

    case (cbAlphaUMRIdx)
      a = 0.384401D+00
      b = 1.52276D+00
      c = - 0.213808D+00
      d = 0.034616D+00
      e = - 0.001976D+00

    case (cbAlphaPR78Idx)
      if (acfi <= 0.491) then
        a = 0.37464D+00
        b = 1.54226D+00
        c = -0.26992D+00
      else
        a = 0.379642D+00
        b = 1.48503D+00
        c = -0.164423D+00
        d = 0.016666D+00
      endif

    case default
      call stoperror("Wrong alpha correlation ")
    end select

  end subroutine get_alpha_param_acentric

end module cbAlpha
