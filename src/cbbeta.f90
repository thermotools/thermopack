!-----------------------------------------------------------------------------
!> Functionality for temperature-dependent covolume factor beta in cubic EoS,
!> defined by b(T) = b(Tc)*beta(T)
!>
!>\author Ailo, Jan 2020
!-----------------------------------------------------------------------------
module cbBeta
  use cubic_eos, only: cb_eos, nBetaCorrs, betaCorrNames, betaCorrNumParams,&
       cbBetaClassicIdx, cbBetaQuantumIdx
  use eosdata
  use compdata, only: gendata_pointer
  use stringmod, only: str_eq
  implicit none
  private
  save

  public :: cbCalcBetaTerm    !< Computing beta at given temperature.
  public :: tpInitBetaCorr    !< Set beta correlations for all components.
  public :: setSingleBetaCorr !< Set beta correlation for a component.
  public :: getSingleBetaCorr !< Set beta correlation for a component.
  logical, parameter :: issueWarnings = .true. !< Warnings to user via stdout stream.

contains

  !> Initializes beta correlations (the same correlation for all components).
  !
  !> This routine is invoked after eoslibinit as one of the subsequent
  !> initialization calls. To change correlation, one has to call
  !> setSingleBetaCorr.
  subroutine tpInitBetaCorr(nc, comp, cbeos, betastr, setno)
    ! Arguments
    integer, intent(in) :: nc
    type (gendata_pointer), dimension(nc), intent(in) :: comp
    class(cb_eos), intent(inout) :: cbeos
    character (len=*), intent (in) :: betastr
    integer, optional, intent(in) :: setno
    ! Local variables
    integer :: i, ierror, setno_loc
    real :: params(2)
    character (len=25) :: corrName
    setno_loc = 0 ! 0 means "take first parameter set found"
    if (present(setno)) setno_loc = setno

    do i = 1,nc
       params = 0.0
       ierror = 0 ! Becomes nonzero if parameters not in database.
       corrName = trim(betastr)

       if (str_eq(betastr, "CLASSIC")) then
          params = 0.0
       else if (str_eq(betastr, 'QUANTUM')) then
          if (str_eq(comp(i)%p_comp%ident,"HE")) then
             params = (/1.4912, 3.2634/)
          else if (str_eq(comp(i)%p_comp%ident,"NE")) then
             params = (/0.4673, 2.4634/)
          else if (str_eq(comp(i)%p_comp%ident,"H2")) then
             params = (/3.0696, 12.682/)
          else if (str_eq(comp(i)%p_comp%ident,"D2")) then
             params = (/1.6501, 7.309/)
          else
             params = (/0.0, 0.0/) ! Equivalent to beta(T)=1.0
          end if
       else
          stop "Chosen correlation for cubic covolume beta factor is not implemented"
       end if
       call setSingleBetaCorr(i=i, cbeos=cbeos, corrName=corrName, betaParams=params)
    end do
  end subroutine tpInitBetaCorr


  function nameToIdx(corrName)
    character(len=*), intent(in) :: corrName
    integer :: nameToIdx
    integer :: i

    do i=1,nBetaCorrs
       if ( str_eq(corrName,betaCorrNames(i)) ) then
          nameToIdx = i
          return
       end if
    end do

    print *, "Possible betacorrs:", betaCorrNames
    call StopError ('nameToIdx::unknown beta corr name ')
  end function nameToIdx

  !> Set beta correlation and parameters for a component
  subroutine setSingleBetaCorr(i, cbeos, corrName, betaParams)
    integer, intent(in)            :: i
    class(cb_eos), intent(inout) :: cbeos
    character (len=*), intent (in) :: corrName
    real, intent(in)               :: betaParams(:)
    ! Locals:
    integer :: betaIdx, nParams

    ! Sanity check.
    if ( .not. allocated(cbeos%single) ) then
       call stoperror("cbeos%single not allocated.")
    end if

    ! Get beta corr idx
    betaIdx = nameToIdx(corrName)
    ! Set idx and name
    cbeos%single(i)%betaMethod = betaIdx
    cbeos%single(i)%betaCorrName = corrName
    ! Get number of beta params for the correlation
    nParams = betaCorrNumParams(betaIdx)
    ! Set beta params
    if ( nParams > 0 ) then
       cbeos%single(i)%betaParams(1:nParams) = betaParams(1:nParams)
    end if

  end subroutine setSingleBetaCorr

  !> Get active beta correlation parameters
  subroutine getSingleBetaCorr(i, cbeos, corrName, numparam, betaParams)
    integer, intent(in)            :: i
    class(cb_eos), intent(inout) :: cbeos
    character (len=*), intent (in) :: corrName
    integer, intent(in)            :: numparam
    real, intent(out)              :: betaParams(numparam)
    ! Locals:
    integer :: betaIdx

    ! Get idx
    betaIdx = nameToIdx(corrName)

    ! Sanity checks.
    if ( .not. allocated(cbeos%single) ) then
       call stoperror("cbeos%single not allocated.")
    else if ( betaCorrNumParams(betaIdx) /= numparam ) then
       call stoperror("numparam for beta corr is wrong")
    end if

    betaParams = cbeos%single(i)%betaParams(1:numparam)

  end subroutine getSingleBetaCorr


  !> Calculates the beta term for all components.
  subroutine cbCalcBetaTerm (nc,cbeos,T)
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T
    ! Local variables
    real :: tci
    integer :: ic

    do ic=1,nc
       tci = cbeos%single(ic)%tc !< Critical temperature component i
       if (cbeos%single(ic)%betamethod .eq. cbBetaClassicIdx) then
          call calcBeta_classic (cbeos,ic)
       else if (cbeos%single(ic)%betamethod .eq. cbBetaQuantumIdx) then
          call calcBeta_quantum (cbeos,ic,T, tci)
       else
          call stoperror("cbbeta::beta correlation undefined")
       end if
    end do

  end subroutine cbCalcBetaTerm


 !> The classic, constant beta formulation
 subroutine calcBeta_classic (cbeos,ic)
   class(cb_eos), intent(inout) :: cbeos
   integer, intent (in) :: ic
   cbeos%single(ic)%beta = 1.0
   cbeos%single(ic)%dbetadT = 0.0
   cbeos%single(ic)%d2betadT2 = 0.0
 end subroutine calcBeta_classic

  !> The beta formulation for quantum fluids
 subroutine calcBeta_quantum (cbeos,ic,T,Tc)
   class(cb_eos), intent(inout) :: cbeos
   integer, intent(in) :: ic
   real, intent(in) :: T
   real, intent(in) :: Tc
   ! Locals
   real :: xi, Tadj
   real :: Delta, Delta_T, Delta_TT
   real :: div, div2, div3, div4
   real :: div_c, Delta_c, prefac

   xi = cbeos%single(ic)%betaParams(1)
   Tadj = cbeos%single(ic)%betaParams(2)

   div = 1/(T+Tadj)
   div2 = div*div
   div3 = div*div2
   div4 = div*div3

   Delta = xi*div
   Delta_T = -xi*div2
   Delta_TT = 2*xi*div3

   div_c = 1/(Tc+Tadj)
   Delta_c = xi*div_c
   prefac = 1/(1+Delta_c)**3

   cbeos%single(ic)%beta = prefac*(1+Delta)**3
   cbeos%single(ic)%dbetadT = prefac*(3*(1+Delta)**2 * Delta_T)
   cbeos%single(ic)%d2betadT2 = prefac*(6*(1+Delta)*Delta_T**2 + 3*(1+Delta)**2 * Delta_TT)
 end subroutine calcBeta_quantum


end module cbBeta
