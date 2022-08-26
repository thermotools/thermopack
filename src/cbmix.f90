module cbmix
  implicit none
  save

  public :: cbCalcMixtureParams
  public :: cbCalcM, setCubicm1m2, getCubicm1m2
  public :: calcBmixCubic

contains

  !---------------------------------------------------------------------------------------
  !> Calculation of parameter m1 and m2 for the selected EOS
  ! \param acf -  Ascentric factor (used for Schmidth/Wenzel
  !
  !
  ! Not really necessay to use input parameters, but done to emphasize that
  ! for some EOS's the m's are dependent of b and/or c
  !
  subroutine cbCalcM (cbeos, cpar, bpar)
    use eosdata
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, optional, intent (in)  :: cpar,bpar
    real ::  sqterm,sw,acf,b,c
    real ::  sqterm2, t1,t2,t3,t4

    real :: m1,m2,bdiff,cdiff,bplus,cplus,bminus,cminus
    real :: sqterm2_bplus, sqterm2_cplus, sqterm2_bminus, sqterm2_cminus
    real :: sqterm_bminus, sqterm_cminus, sqterm_bplus, sqterm_cplus
    real :: dm1db_num, dm1dc_num, dm2db_num,dm2dc_num
    real :: m1_bplus, m1_bminus, m1_cplus, m1_cminus
    real :: m2_bplus, m2_bminus, m2_cplus, m2_cminus

    cbeos%dm1dB = 0.0D0
    cbeos%dm1dC = 0.0D0
    cbeos%dm2dB = 0.0D0
    cbeos%dm2dC = 0.0D0

    cbeos%d2m1dB2 = 0.0D0
    cbeos%d2m2dB2 = 0.0D0

    cbeos%d2m1dC2 = 0.0D0
    cbeos%d2m2dC2 = 0.0D0

    cbeos%d2m1dBdC = 0.0D0
    cbeos%d2m2dBdC = 0.0D0

    select case (cbeos%subeosidx)
    case (cbVdW)
      cbeos%m1 = 0.0D0
      cbeos%m2 = 0.0D0
    case (cbSRK, cbPR, cspSRK, cpaSRK, cspPR, cpaPR, eosLK)
      cbeos%m1 = (-(cbeos%delta + 1)+ sqrt(cbeos%delta**2 + 6.0*cbeos%delta + 1.0))/2.0
      cbeos%m2 = (-(cbeos%delta + 1)- sqrt(cbeos%delta**2 + 6.0*cbeos%delta + 1.0))/2.0
    case (cbSW) !< For this EOS, the m1 and m2 are re-calculated after the mixing paramteres are calculated
      acf = cpar
      sw = 1.0+18.0*acf+9.0*acf*acf
      sqterm = sqrt(sw)
      cbeos%m1 = (-1.0 - 3.0*acf + sqterm)/2.0
      cbeos%m2 = (-1.0 - 3.0*acf - sqterm)/2.0

      cbeos%dm1dc = -1.5 + 4.5*(1.0+acf)/sqterm
      cbeos%dm2dc = -1.5 - 4.5*(1.0+acf)/sqterm

      cbeos%d2m1dc2 = 4.5*(sw-9.0*(1.0+acf)**2)/(sw*sqterm)
      cbeos%d2m2dc2 = -cbeos%d2m1dc2

    case (cbPT) !< The Patel-Teja 3 parameter cubic EOS
      ! Need to calculated after mixing of b and c
      c = cpar
      b = bpar
      sqterm2 = b*b+6*b*c+c*c
      sqterm = sqrt(sqterm2)

      cbeos%m1 = (-b-c + sqterm)/(2*b)
      cbeos%m2 = (-b-c - sqterm)/(2*b)

      cbeos%dm1dB = -(c*(3*b-sqterm+c))/(2*sqterm*b*b)
      cbeos%dm2dB =  (c*(3*b+sqterm+c))/(2*sqterm*b*b)

      cbeos%dm1dC =  (3*b-sqterm+c)/(2*sqterm*b)
      cbeos%dm2dC = -(3*b+sqterm+c)/(2*sqterm*b)

      t2 = b*b + 5*b*c + 3*c*c
      t3 = b*b*b*sqterm*sqterm2
      t4 = c*c*c
      cbeos%d2m1dB2 =   c*(3*b*t2 - sqterm*sqterm2 + t4) /t3
      cbeos%d2m2dB2 = -(c*(3*b*t2 + sqterm*sqterm2 + t4))/t3

      t1 = 3*b+c
      cbeos%d2m1dC2 = -(t1*t1 - sqterm2)/(2*sqterm2*sqterm*b)
      cbeos%d2m2dC2 = -cbeos%d2m1dC2

      cbeos%d2m1dBdC = -((t1-sqterm)* (b*(b+3*c)-c*sqterm))/(2*sqterm*sqterm2*b*b)
      cbeos%d2m2dBdC =  ((t1+sqterm)* (b*(b+3*c)+c*sqterm))/(2*sqterm*sqterm2*b*b)

      ! Test numerical:
      if (cbeos%cubic_verbose) then
        m1 = cbeos%m1
        m2 = cbeos%m2

        bdiff = 1.0E-5
        cdiff = 1.0E-5

        bplus = b+bdiff
        cplus = c+cdiff

        bminus = b-bdiff
        cminus = c-cdiff

        sqterm2_bplus = bplus*bplus+6*bplus*c+c*c
        sqterm2_cplus = b*b+6*b*cplus+cplus*cplus

        sqterm_bplus = sqrt(sqterm2_bplus)
        sqterm_cplus = sqrt(sqterm2_cplus)

        sqterm2_bminus = bminus*bminus+6*bminus*c+c*c
        sqterm2_cminus = b*b+6*b*cminus+cminus*cminus

        sqterm_bminus = sqrt(sqterm2_bminus)
        sqterm_cminus = sqrt(sqterm2_cminus)

        m1_bplus = (-bplus-c + sqterm_bplus)/(2*bplus)
        m1_bminus = (-bminus-c + sqterm_bminus)/(2*bminus)

        m1_cplus = (-b-cplus + sqterm_cplus)/(2*b)
        m1_cminus = (-b-cminus + sqterm_cminus)/(2*b)

        m2_bplus = (-bplus-c - sqterm_bplus)/(2*bplus)
        m2_bminus = (-bminus-c - sqterm_bminus)/(2*bminus)

        m2_cplus = (-b-cplus - sqterm_cplus)/(2*b)
        m2_cminus = (-b-cminus - sqterm_cminus)/(2*b)

        dm1db_num  = (m1_bplus - m1_bminus)/(2*bdiff)
        dm1dc_num  = (m1_cplus - m1_cminus)/(2*cdiff)

        dm2db_num  = (m2_bplus - m2_bminus)/(2*bdiff)
        dm2dc_num  = (m2_cplus - m2_cminus)/(2*cdiff)

        write (*,*) 'PT derivtest, analytical, numerical'
        write (*,*) 'PT derivtest, dm1/db: ', cbeos%dm1db, dm1db_num, cbeos%dm1db - dm1db_num
        write (*,*) 'PT derivtest, dm1/dc: ', cbeos%dm1dc, dm1dc_num, cbeos%dm1dc- dm1dc_num
        write (*,*) 'PT derivtest, dm2/db: ', cbeos%dm2db, dm2db_num, cbeos%dm2db- dm2db_num
        write (*,*) 'PT derivtest, dm2/dc: ', cbeos%dm2dc, dm2dc_num, cbeos%dm2dc- dm2dc_num
      endif
    end select
  end subroutine cbCalcM

  !-----------------------------------------------------------------------------------------

  !> Calculation of critical compressibility and the Ohmega-term in the cubic EOS.
  !! The parameters m1 and m2 should already be calculated
  !!
  !! The terms OhmegaA, OhmegaB and Zcrit are composition dependendent and are
  !! stored as an array of length nc in the cbeos type
  !!
  !! For the VdW, RK, SRK, SKRGB, PR and later EOS that does not have
  !! composition dependent m1 and m2, these terms have the same value for all
  !! elementes in the array.
  !!
  !! The OhmegaA, OhmegaB and Zcrit are solved using a second order
  !! Newton-Raphson method and converges normally after 3-4 iterations.
  !!

  subroutine cbCalcOmegaZc(nc,cbeos)
    use eosdata
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, parameter  :: eps = 1.0E-10
    integer, parameter  :: maxiter = 10
    real :: n,n1,n2,zcomb,dzcomb,f,fd,fdd,argum
    integer :: iter
    integer :: ic,i
    real :: bc,dbc,kc
    real :: acfi
    if (cbeos%subeosidx == cbSW) then !> For the Schmidt and Wenzel EOS
       do ic = 1,nc
          acfi = cbeos%single(ic)%acf
          bc = 0.25989 - 0.0217*acfi + 0.00375*acfi*acfi !< initial value critical compressibility
          dbc = 1.0E10
          iter = 1
          do while (iter < maxiter .and. abs (dbc) > eps)
             f = (6.0*acfi+1)*bc*bc*bc + 3*bc*bc + 3*bc -1.0
             fd = 3.0*(6.0*acfi+1)*bc*bc+ 6.0*bc + 3.0
             fdd = 6.0*(6.0*acfi+1)*bc + 6.0
             argum = 1.0 - 2.0*f*fdd/fd**2
             if (argum.gt.0.0) then
                argum = sqrt(argum)
             else
                argum = 1.0
             end if
             dbc = (-2.0)*f/(fd*(1.0+argum))
             bc= bc + dbc
             iter = iter + 1
          enddo

          if (iter == maxiter) call StopError ("Third order method failed to calculate zcrit")
          if (cbeos%cubic_verbose) then
             write (*,*) 'Calculation of ohmega/zcrit converged after ',iter-1,' iterations for SW EOS'
          endif
          kc = 1.0/(3.0*(1 + bc*acfi))
          cbeos%single(ic)%omegaB = bc * kc

          argum = 1.0 - kc*(1.0-bc)
          cbeos%single(ic)%omegaA = argum**3
          cbeos%single(ic)%zcrit = kc
          cbeos%single(ic)%omegaC = 0.0D0
       enddo
    else if (cbeos%subeosidx == cbPT) then
       ! General term for zcrit
       do ic = 1,nc
          acfi = cbeos%single(ic)%acf
          ! From Patel-Teja general correlation (eq 21)
          cbeos%single(ic)%zcrit = 0.329032-0.076799*acfi + 0.0211947*acfi*acfi
          cbeos%single(ic)%omegaC = 1.0-3.0*cbeos%single(ic)%zcrit
          dbc = 1.0E10
          iter = 1
          bc = cbeos%single(ic)%zcrit
          do while (iter < maxiter .and. abs (dbc) > eps)
             f = bc*bc*bc+(2-3*cbeos%single(ic)%zcrit)*bc*bc &
                  + 3*cbeos%single(ic)%zcrit*cbeos%single(ic)%zcrit*bc &
                  - cbeos%single(ic)%zcrit**3
             fd = 3*bc*bc + 2*(2-3*cbeos%single(ic)%zcrit)*bc &
                  + 3*cbeos%single(ic)%zcrit*cbeos%single(ic)%zcrit
             fdd = 6*bc + 2*(2-3*cbeos%single(ic)%zcrit)

             argum = 1.0 - 2.0*f*fdd/fd**2
             if (argum.gt.0.0) then
                argum = sqrt(argum)
             else
                argum = 1.0
             end if
             dbc = (-2.0)*f/(fd*(1.0+argum))
             bc= bc + dbc
             iter = iter + 1
          enddo

          cbeos%single(ic)%omegaB = bc
          cbeos%single(ic)%omegaC = 1.0-3.0*cbeos%single(ic)%zcrit

          cbeos%single(ic)%omegaA = 3*cbeos%single(ic)%zcrit*cbeos%single(ic)%zcrit &
               + 3*(1-2*cbeos%single(ic)%zcrit)*cbeos%single(ic)%omegaB &
               + cbeos%single(ic)%omegaB*cbeos%single(ic)%omegaB &
               + cbeos%single(ic)%omegaC
       enddo
    else ! other 2 param eos's
       !! Solve Zc/OhmegaB with a 3rd order method for two-param eques
       zcomb = 5.0
       dzcomb = 1.0E10
       iter = 1
       do while (iter < maxiter .and. abs(dzcomb) > eps)
          n  = zcomb - 1.0
          n1 = zcomb - cbeos%m1
          n2 = zcomb - cbeos%m2
          f = n*n1**2+n*n1*n2+n*n2**2-n1**2*n2-n1*n2**2
          fd = 3.0*(n*n1+n*n2-n1*n2)
          fdd = 6.0*n
          argum = 1.0-2.0*f*fdd/fd**2
          if (argum.gt.0.0) then
             argum = sqrt(argum)
          else
             argum = 1.0
          end if
          dzcomb = (-2.0)*f/(fd*(1.0+argum))
          zcomb = zcomb+dzcomb
          iter = iter + 1
       enddo

       if (iter == maxiter) call StopError ("Third order method failed to calculate zcrit")
       if (cbeos%cubic_verbose) then
          write (*,*) 'Calculation of ohmega/zcrit converged after ',iter-1,' iterations for CB  EOS'
       endif
       argum = n1*n2/(n*n1+n*n2)
       do ic=1,nc
          cbeos%single(ic)%omegaB = (1.0-argum)/(zcomb-1.0)
          cbeos%single(ic)%zcrit = cbeos%single(ic)%omegaB*zcomb
          cbeos%single(ic)%omegaA = cbeos%single(ic)%omegaB*(1.0/n-cbeos%single(ic)%omegaB)*n1*n2
          cbeos%single(ic)%omegaC = 0.0D0
       enddo
    endif

    do i=1,nc
      call cbSingleCalcABC(nc,cbeos,i)
    enddo

  end subroutine cbCalcOmegaZc

  subroutine cbSingleCalcABC(nc,cbeos,i)
    use cubic_eos, only: cb_eos
    use thermopack_constants, only: kRgas
    implicit none
    integer, intent(in) :: nc, i
    class(cb_eos), intent(inout) :: cbeos
    ! Locals
    real :: tci,pci

    tci = cbeos%single(i)%tc !< Critical temperature component, i
    pci = cbeos%single(i)%pc !< Critical pressure component, i
    cbeos%single(i)%a = cbeos%single(i)%omegaA*(kRgas*tci)**2/pci
    cbeos%single(i)%b = cbeos%single(i)%omegaB*kRgas*tci/pci
    cbeos%single(i)%c = cbeos%single(i)%omegaC*kRgas*tci/pci

  end subroutine cbSingleCalcABC

  !----------------------------------------------------------------------
  !> Calculate bij to be used in the mixing rules of the covolume parameter
  !!
  !! \param s Exponent
  !!
  !! \author Morten Hammer
  !! \todo Add lij mixing parameter
  subroutine cbCalcLowcasebij(nc,cbeos,s)
    use cubic_eos, only: cb_eos
    use numconstants, only: small
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: s
    ! Locals
    integer :: i,j !< Counters
    real :: inv_s

    if (abs(s-1.0) >= small) then
      cbeos%simple_covolmixing = .false.

      if (allocated (cbeos%lowcase_bij) .eqv. .false. ) call StopError('CalcLowcasebij: Equation of state are not selected')

      do i=1,nc
        cbeos%lowcase_bij(i,i) = cbeos%single(i)%b
      enddo
      inv_s = 1.0/s
      do i=1,nc
        do j=i,nc
          cbeos%lowcase_bij(i,j) = (0.5*(cbeos%single(i)%b**inv_s + cbeos%single(j)%b**inv_s))**s
          cbeos%lowcase_bij(j,i) = cbeos%lowcase_bij(i,j) ! *(1.0-lij(i,j)) ! Todo
        enddo
      enddo
    endif
  end subroutine cbCalcLowcasebij

  !< Calculate the mixing parameters according to the selected mixing rule.
  !! \param t - Temperature [K]
  !! \param p - Pressure [Pa]
  !! \param zComp - mole numbers [mole]
  !!
  !! For the Schmidt and Wenzel EOS, the constants m1 and m2 in the
  !! EOS-formulations needs to be updated based on the calculated b_i's for
  !! the components in the mixture and the mixture ascentric factor. This
  !! call to calcM after the temperature-dependent alpha-term has been calculated.
  !!
  !! First part could be written as a separate subroutine
  !! calculating the derivatives of the alpha-function
  !!

  !< The m_srk and m_pr by Michelsen
  ! For Patel-Teja the "s" is a fitted paramtere called "F" and are available
  ! In their article if component specific "s" are to be used

  !! xx = s**2 ! This is the original SOAVE(1972) formulation. Correct in form, also supercritical
  !! yy = -2.0*s*(s+1.0)


  subroutine cbCalcBmix (nc, cbeos, T, zcomp)
    !> Calculate covolume parameter, which can depend on both composition and
    !> temperature.
    !> \author Ailo, Jan 2020
    use cubic_eos, only: cb_eos
    use cbBeta, only: cbCalcBetaTerm
    integer, intent (in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T
    real, intent (in) :: zcomp(nc)
    ! Locals
    integer :: i, j
    real :: bii, biiT, biiTT
    real :: bjj, bjjT, bjjTT
    real :: bij, bijT, bijTT, lijfac, biTT(nc)
    real :: sumn

    call cbCalcBetaTerm(nc, cbeos, T)
    cbeos%b = 0.0
    cbeos%bT = 0.0
    cbeos%bTT = 0.0
    if (cbeos%simple_covolmixing) then
       do i=1,nc
          cbeos%bi(i) = cbeos%single(i)%b * cbeos%single(i)%beta
          cbeos%biT(i) = cbeos%single(i)%b * cbeos%single(i)%dbetadT
          cbeos%bij(i,1:nc) = 0.0 ! d^2b/(dni*dnj) = 0

          cbeos%b = cbeos%b + zcomp(i)*cbeos%bi(i)
          cbeos%bT = cbeos%bT + zcomp(i)*cbeos%biT(i)
          cbeos%bTT = cbeos%bTT + zcomp(i)*cbeos%single(i)%b*cbeos%single(i)%d2betadT2
       end do
       cbeos%sumb = cbeos%b
    else
       ! Michelsen approach for general mixing of covolume
       sumn = sum(zcomp)
       do i=1,nc
          cbeos%bi(i) = 0.0
          cbeos%biT(i) = 0.0
          biTT(i) = 0.0
          bii = cbeos%single(i)%b * cbeos%single(i)%beta
          biiT = cbeos%single(i)%b * cbeos%single(i)%dbetadT
          biiTT = cbeos%single(i)%b * cbeos%single(i)%d2betadT2
          do j=1,nc
             bjj = cbeos%single(j)%b * cbeos%single(j)%beta
             bjjT = cbeos%single(j)%b * cbeos%single(j)%dbetadT
             bjjTT = cbeos%single(j)%b * cbeos%single(j)%d2betadT2

             lijfac = 1-cbeos%lij(i,j)
             bij = lijfac*(bii+bjj)/2.0
             bijT = lijfac*(biiT+bjjT)/2.0
             bijTT = lijfac*(biiTT+bjjTT)/2.0
             cbeos%bi(i)  = cbeos%bi(i)  + zcomp(j)*bij
             cbeos%biT(i) = cbeos%biT(i) + zcomp(j)*bijT
             biTT(i) = biTT(i) + zcomp(j)*bijTT
          enddo
          cbeos%b  = cbeos%b  + zcomp(i)*cbeos%bi(i)/sumn
          cbeos%bT = cbeos%bT + zcomp(i)*cbeos%biT(i)/sumn
          cbeos%bTT = cbeos%bTT + zcomp(i)*biTT(i)/sumn
       enddo

       cbeos%bi  = (2.0*cbeos%bi  - cbeos%b)/sumn
       cbeos%biT = (2.0*cbeos%biT - cbeos%bT)/sumn

       do i=1,nc
          bii = cbeos%single(i)%b * cbeos%single(i)%beta
          do j=1,nc
             bjj = cbeos%single(j)%b * cbeos%single(j)%beta
             lijfac = 1-cbeos%lij(i,j)
             bij = lijfac*(bii+bjj)/2.0
             cbeos%bij(i,j) = (2*bij - cbeos%bi(i) - cbeos%bi(j))/sumn
          enddo
       enddo
       cbeos%sumb = cbeos%b
    end if

  end subroutine cbCalcBmix

  ! As cbCalcBmix, but not filling cbeos type
  function calcBmixCubic(nc, cbeos, z) result(Bmix)
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent (in) :: nc
    class(cb_eos), intent(in) :: cbeos
    real, intent (in) :: z(nc)
    real :: Bmix
    ! Locals
    integer :: i,j
    real :: sumn, bi

    sumn = sum(z)
    if (cbeos%simple_covolmixing) then
      Bmix = 0
      do i=1,nc
        Bmix = Bmix + z(i)*cbeos%single(i)%b
      enddo
      Bmix = Bmix/sumn
    else
      ! Michelsen approach for general mixing of covolume
      Bmix = 0.0
      do i=1,nc
        bi = 0.0
        do j=1,nc
          bi = bi + z(j)*cbeos%lowcase_bij(i,j)
        enddo
        Bmix = Bmix + z(i)*bi
      enddo
      Bmix = Bmix/sumn**2
    endif
  end function calcBmixCubic

  subroutine cbCalcCmix (nc, cbeos, zcomp)
    use eosdata, only: cbSW
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent (in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: zcomp(nc)
    ! Locals
    integer :: low,high,i,j
    real :: acfi
    real :: sum1, sum2, bi07, bj07

    sum1 = 0.0
    sum2 = 0.0
    cbeos%sumc = 0.0
    low = 1
    high = nc
    do i=low,high
       acfi = cbeos%single(i)%acf !< Acentric factor component, i

       if (cbeos%subeosidx == cbSW) then !< Need intermediate sums
          bi07 =  cbeos%bi(i)**0.7 !< Assume the cbCalcBmix has been called
          sum1 = sum1 + zcomp(i)*acfi*bi07
          sum2 = sum2 + zcomp(i)*bi07
       else
          cbeos%ci(i) = cbeos%single(i)%c
          cbeos%sumc = cbeos%sumc + zcomp(i)*cbeos%ci(i)
          cbeos%cij(i,1:nc) = 0.0D00
       endif
    end do

    if (cbeos%subeosidx == cbSW) then
       cbeos%sumc = sum1 / sum2
       do i=1,nc
          bi07 = cbeos%bi(i)**0.7
          cbeos%ci(i) = (bi07/sum2) * (cbeos%single(i)%acf - (sum1/sum2))
          do j=1,nc
             bj07 = cbeos%bi(j)**0.7
             cbeos%cij(i,j) = -(bi07*bj07/(sum2*sum2)) &
                  * (cbeos%single(i)%acf + cbeos%single(j)%acf - 2*sum1/sum2)
          end do
       end do
    endif
    cbeos%c = cbeos%sumc
  end subroutine cbCalcCmix

  !-------------------------------------------
  subroutine cbCalcAmix(nc, cbeos, t, zcomp)
    use cubic_eos, only: cb_eos, cbMixVdW, cbMixVdWCPA, &
         cbMixHuronVidal, cbMixHuronVidal2, cbMixNRTL, cbMixUNIFAC, &
         cbMixHVCPA, cbMixHVCPA2, cbMixWongSandler, cbMixWSCPA, cbMixHvWS, cbMixReid
    use wong_sandler, only : WongSandlerMix
    use excess_gibbs, only : ExcessGibbsMix
    use cbAlpha, only: cbCalcAlphaTerm
    implicit none
    integer, intent (in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t, zcomp(nc)

    ! Common part for all mixing rules - actually the temperature dependent
    ! alpha(T) term in the EOS. ap = alpha(T)*ai

    ! Assign  alpha-terms including temperature derivatives
    call cbCalcAlphaTerm(nc, cbeos, T)

    select case (cbeos%mruleidx)

    case (cbMixVdW, cbMixVdWCPA)
      call vanderWaalsMix(nc,cbeos,t,zcomp)

    case (cbMixHuronVidal, cbMixHuronVidal2, cbMixNRTL, cbMixUNIFAC, &
         cbMixHVCPA, cbMixHVCPA2)
      call ExcessGibbsMix(cbeos,T,zcomp)

   case (cbMixWongSandler, cbMixWSCPA, cbMixHVWS)
      call WongSandlerMix(cbeos, T, zcomp)

    case (cbMixReid) !< Un-symmetric - need to be developed
      call stoperror ("Reid mixing rule not yet implemented")
    case default
      call stoperror ("Unknown mixing rules")
    end select

    cbeos%a = cbeos%suma
  end subroutine cbCalcAmix

  subroutine vanderWaalsMix(nc,cbeos,t,zcomp)
    use stringmod, only: str_eq
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t, zcomp(nc)
    !
    ! Local variables
    real :: sum1,sum2,sum3
    integer :: i, j,  low, high
    real :: pij,dpijdt, d2pijdt2
    real :: afac(nc)
    real :: dafacdt(nc),d2afacdt2(nc)

    low = 1
    high = nc

    !> Ideal contributions
    sum1 = 0.0D0
    sum2 = 0.0D0
    sum3 = 0.0D0
    do i=low,high
      afac(i) = sqrt(cbeos%single(i)%a*cbeos%single(i)%alpha)
      sum1 = sum1 + afac(i)*zcomp(i)
      if (cbeos%cubic_verbose) then
        write(*,*) 'afac(,',i,') = ',afac(i)
        write(*,*) 'sum1 = ',sum1
      endif
    end do
    cbeos%suma = sum1*sum1

    do i=low,high
      if (afac(i) > 0.0) then
        dafacdt(i) = (0.5*cbeos%single(i)%a/afac(i)) * cbeos%single(i)%dalphadt
        d2afacdt2(i) = -0.25*cbeos%single(i)%a/afac(i) &
            * ((cbeos%single(i)%dalphadt*cbeos%single(i)%dalphadt) &
            - (2.0*cbeos%single(i)%alpha*cbeos%single(i)%d2alphadt2))/cbeos%single(i)%alpha
       sum2 = sum2 + dafacdt(i)*zcomp(i)
       sum3 = sum3 + d2afacdt2(i)*zcomp(i)
       cbeos%ait(i) = 2.0*dafacdt(i)*sum1
       cbeos%ai(i) = 2.0*afac(i)*sum1
     else
       dafacdt(i) = 0.0
       d2afacdt2(i) = 0.0
       cbeos%ait(i) = 0.0
       cbeos%ai(i) = 0.0
     endif
    end do

    do i=low,high
       cbeos%ait(i) = cbeos%ait(i) + 2.0*sum2*afac(i)
    end do

    do i=low,high
       do j=low,high
          cbeos%aij(i,j) = 2.0*afac(i)*afac(j)
       end do
    end do

    cbeos%at = 2.0*sum1*sum2
    cbeos%att = 2.0*(sum2*sum2+sum1*sum3)

    !> Nonideal contributions (the kij terms)
    do i=low,high
      do j=i+1,high
        pij = afac(i)*afac(j)*cbeos%kij(i,j)
        dpijdt = cbeos%kij(i,j)*(afac(j)*dafacdt(i) + &
             afac(i)*dafacdt(j))
        d2pijdt2 = cbeos%kij(i,j)*(afac(j)*d2afacdt2(i)+ &
             2.0*dafacdt(i)*dafacdt(j)+ &
             afac(i)*d2afacdt2(j))

        cbeos%suma   = cbeos%suma - 2.0*zcomp(i)*zcomp(j)*pij
        cbeos%ai(i)  = cbeos%ai(i) - 2.0*pij*zcomp(j)
        cbeos%ai(j)  = cbeos%ai(j) - 2.0*pij*zcomp(i)
        cbeos%ait(i) = cbeos%ait(i) - 2.0*dpijdt*zcomp(j)
        cbeos%ait(j) = cbeos%ait(j) - 2.0*dpijdt*zcomp(i)
        cbeos%at     = cbeos%at - 2.0*zcomp(i)*zcomp(j)*dpijdt
        cbeos%aij(i,j) = cbeos%aij(i,j) - 2.0*pij
        cbeos%aij(j,i) = cbeos%aij(j,i) - 2.0*pij
        cbeos%att   = cbeos%att - 2.0*zcomp(i)*zcomp(j)*d2pijdt2

       enddo
    enddo
  end subroutine vanderWaalsMix

  subroutine cbCalcMixtureParams(nc,cbeos,t,zcomp)
    !> Update the parameters of the cubic EoS, such as a and b, given the state (t, zcomp).
    use eosdata, only: cbSW, cbPT
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc                         !< number of components
    class(cb_eos), intent(inout) :: cbeos            !< cubic eos instance
    real, intent(in) :: t                             !< temperature (K)
    real, intent(in) :: zcomp(nc)                     !< mole numbers (mole)

    cbeos%sumn = sum(zcomp)
    cbeos%suma = 0.0D+00
    cbeos%sumb = 0.0D+00
    cbeos%sumc = 0.0D+00
    cbeos%At   = 0.0D+00
    cbeos%Att  = 0.0D+00
    cbeos%Bt   = 0.0D+00
    cbeos%Bit  = 0.0D+00
    cbeos%Btt   = 0.0D+00
    cbeos%Bi = 0.0D+00

    ! Mixing rule for the B-parameter
    call cbCalcBmix(nc,cbeos,t,zcomp)

    if (cbeos%subeosidx == cbSW .or. cbeos%subeosidx == cbPT) then
       ! Mixing rule for the C-parameter - need B-paramteter
       call cbCalcCmix(nc, cbeos, zcomp)
       call cbCalcM(cbeos, cbeos%sumc, cbeos%sumb)
    endif

    ! Mixing rule for the A-parameter - need B-paramters
    call cbCalcAmix(nc,cbeos,t,zcomp)
  end subroutine cbCalcMixtureParams

  !> For tuning of the parameters m1 and m2 in the cubic eos
  subroutine setCubicm1m2(cbeos, m1, m2)
    use thermopack_var, only: nce
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos            !< cubic eos instance
    real, intent(in) :: m1, m2
    !
    cbeos%m1 = m1
    cbeos%m2 = m2
    call cbCalcOmegaZc(nce,cbeos) !Uses the m1 and m2 for two-param eos
    cbeos%delta = -1.0D20
  end subroutine setCubicm1m2

  !> For retrieval of the parameters m1 and m2 in the cubic eos
  subroutine getCubicm1m2(cbeos, m1,m2)
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos            !< cubic eos instance
    real, intent(out) :: m1, m2
    !
    m1 = cbeos%m1
    m2 = cbeos%m2
  end subroutine getCubicm1m2

end module cbmix
