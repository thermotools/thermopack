!> This module contains methods for various cubic equation of states The cubic
!! eos's are formulated using the m1 and the m2.
!! Suppoerted cubic EOS's
!!   SRK - Soave-Redlich-Kwong
!!   PR    - Peng Robinson
!!   VdW   - Van Der Waals
!!   RK    - Redlich-Kwong
!!   SW    - Schmidt and Wenzel
!!   PT    - Patel-Teja

module tpcubic
  use thermopack_constants, only: LIQPH, VAPPH, SINGLEPH
  implicit none
  save

  type :: cbBig ! Cubic solver parameters
    sequence
    real :: A,B,C
  end type cbBig

  public ::  &
       cbCalcZfac, cbCalcPressure, cbCalcFug, cbCalcEntropy,&
       cbGres,cbCalcEnthalpy, cbCalcPseudo, cbCalcFreeEnergy
  public :: cbCalcDerivatives_svol, cbCalcFres
  public :: cbDumpEosData
contains

  subroutine cbCalcDerivatives(nc,cbeos,T,p,Zfac)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T,p,Zfac
    real :: v
    ! General part all cubic EOS
    v = Zfac * kRgas*T/p
    call cbCalcDerivatives_svol(nc,cbeos,T,v)
  end subroutine cbCalcDerivatives

  ! Calculate derivatives given temperature [K] and specific volume [L/mol].
  subroutine cbCalcDerivatives_svol(nc,cbeos,T,v)
    use thermopack_constants, only: kRgas
    use eosdata
    use cubic_eos, only: cb_eos
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T, v
    ! Locals
    real :: n0,n1,n2,den,lnn, m, sumn
    real :: D,DD,B,C,R,mBR

    real :: f, fV, fB, fC, fVV, fVB, fVC, fBB,fBC,fCC
    real :: g, gV, gB, gVV, gVB, gBB
    real :: h, hV, hB, hC, hVV, hVB, hVC, hBB, hBC, hCC

    real :: m1B, m2B, mB,m1BB,m2BB,m1BC,m2BC
    real :: m1C, m2C, mC,m1CC,m2CC
    real :: mBB, mBC, mCC
    real :: n1B, n2B, n1V, n2V
    real :: n1C, n2C

    real :: n1BB,n2BB,n1BC,n2BC,n1CC,n2CC

    ! General part all cubic EOS
    sumn = cbeos%sumn
    n0 = v - cbeos%sumb
    n1 = v - cbeos%m1 * cbeos%sumb
    n2 = v - cbeos%m2 * cbeos%sumb
    den = n1*n2
    lnn = n2/n1

    if (abs(n0) <= 1D-20)  then
       print *, "In cbCalcDerivatives_svol: v, b = ", v, cbeos%sumb
       Call StopError &
            ("The (V-m1*B) or (V-m2*B) term has become negative for selected CubicEOS")
    else
       lnn = log(lnn)
       cbeos%pn = kRgas*T/n0
       cbeos%pa = (-1.0)/den
       cbeos%pb = kRgas*T/n0**2 &
            -cbeos%suma*((cbeos%m1 + cbeos%dm1dB*cbeos%sumb)*n2 &
            +(cbeos%m2+cbeos%dm2dB*cbeos%sumb)*n1)/den**2

       cbeos%pt = cbeos%sumn*kRgas/n0  !dp/dt=cbeos%pt+cbeos%pa*cbeos%at + cbeos%pb*cbeos%bt
       cbeos%pv = cbeos%sumn*(-kRgas)*T/n0**2+cbeos%suma*(n1+n2)/den**2  !dp/dv =cbeos%pv
       cbeos%pn = (kRgas*T)/n0 !dP/dn_i = cbeos%pn + cbeos%pa * cbeos%ai(i)
       !    +cbeos%pb * cbeos%bi(i) + cbeos%pc * cbeos%ci(i)

       cbeos%ffn = v/n0
       if (abs(cbeos%ffn) <= 1.0D-20) then
          print *, "In cbCalcDerivatives_svol: inputted T [K], v [L/mol] = ", T, v
          call StopError("Expression v/n <= 0 in selected cubic EOS")
       else

          select case (cbeos%subeosidx)
          case (cbVDW) !< Where m1 = m2 = 0.0
             DD = cbeos%suma/(krGas*T)
             cbeos%ffn = log(cbeos%ffn)

             cbeos%ffnv = -cbeos%sumb/(v*n0)
             cbeos%ffnb = 1.0/n0

             cbeos%ff = sumn*cbeos%ffn-DD/n1

             cbeos%ffa = -1.0/(kRgas*T*n1)
             cbeos%ffb = sumn/n0 - DD * cbeos%m1/(n1*n1)

             cbeos%fft = DD/(T*n1)
             cbeos%fftt = -2.0*DD/(n1*n1)

             cbeos%ffab = -1.0*cbeos%m1/(kRgas*T*n1*n1)
             cbeos%ffat = 1.0/(kRgas*T*T*n1)
             cbeos%ffbb = sumn/n0**2 - 2*DD*cbeos%m1/(n1*n1)
             cbeos%ffbt = DD * cbeos%m1 /(T*n1*n1)


             cbeos%ffv = -sumn*cbeos%sumb/(v*n0) + DD/(n1*n1)
             cbeos%ffvt = -DD/(n1*n1*T)

             cbeos%ffva = 1.0/(n1*n1*kRgas*T)
             cbeos%ffvv = -sumn/(v*v) + 1.0/(n0*n0) - 2.0*DD/(n1*n1*n1)
             cbeos%ffvb = -sumn/(n0*n0) + DD*cbeos%m1/(n1*n1*n1)

             !          else !< Rest of the cubic EOS family
          case (cbSRK,cbPR,cbSW,cbPT,eosLK,cspSRK,cspPR,cpaSRK,cpaPR)  ! 2 and 3 paramter eos
                                                         ! LK added when SRK is used to
                                                         ! generate initial values ....

!>
!! For three param like Patel-Teja, m1 = m1(sumb,sumc) nad m2 = m2(sumb,sumc)
!! For Schmidt and Wenzel, m1 = m1(sumc) and m2 = m2(sumc) - or m2(acfmix)
!!
!! Define reduced Helmholz energy: as F = -g - (amix(T)/T) * f
!!
!!    n = v - cbeos%sumb
!!    n1 = v - cbeos%m1 * cbeos%sumb
!!    n2 = v - cbeos%m2 * cbeos%sumb
!!    den = n1*n2
!!    lnn = log(n2/n1) or log n2 - log n1
!!    h = lnn
!!

!>
!!------------------------------
!! Michelsen | Thermopack
!!  p88-91   |
!!-----------------------------
!!     D     | cbeos%suma,
!!     Di    | cbeos%a(i)
!!     Dj    | cbeos%a(j)
!!     Dt    | cbeos%at
!!     Dtt   | cbeos%att
!!     Dij   | cbeos%aij(i,j)
!!     Dit   | cbeos%ait(i)
!!     B     | cbeos%sumb,
!!     Bi    | cbeos%b(i),
!!     Bij   | cbeos%bij = 0
!!-----------------------------

             m = cbeos%m1 - cbeos%m2

             D = cbeos%suma
             B = cbeos%sumb
             C = cbeos%sumc

             R = kRgas

             mBR = m*B*R

             h = lnn
             f = h/mBR

             g = log(n0) - log(v)

! Derivatives of helper functions

             if (cbeos%subeosidx == cbPT .or. cbeos%subeosidx == cbSW) then
! 1.st derivatives
                m1B = cbeos%dm1dB
                m2B = cbeos%dm2dB

                m1C = cbeos%dm1dC
                m2C = cbeos%dm2dC

                mB = m1B - m2B
                mC = m1C - m2C

                n1B = -m1B*B-cbeos%m1
                n2B = -m2B*B-cbeos%m2

                n1C = -m1C*B
                n2C = -m2C*B

! 2nd derivatives
                m1BB = cbeos%d2m1dB2
                m2BB = cbeos%d2m2dB2

                m1BC = cbeos%d2m1dBdC
                m2BC = cbeos%d2m2dBdC

                m1CC = cbeos%d2m1dC2
                m2CC = cbeos%d2m2dC2

                mBB = m1BB - m2BB
                mBC = m1BC - m2BC
                mCC = m1CC - m2CC

                n1BB = -m1BB * B - 2.0*m1B
                n2BB = -m2BB * B - 2.0*m2B
                n1BC = -m1BC*B - m1C
                n2BC = -m2BC*B - m2C
                n1CC = -m1CC*B
                n2CC = -m2CC*B
             else
                m1B = 0.0D0
                m2B = 0.0D0
                m1C = 0.0D0
                m2C = 0.0D0
                mB = 0.0D00
                mC = 0.0D00
                m1BB = 0.0D0
                m2BB = 0.0D0
                m1C = 0.0D0
                m2C = 0.0D0
                m1CC = 0.0D0
                m2CC = 0.0D0
                mBB = 0.0D00
                mBC = 0.0D00
                mCC = 0.0D00
                n1B = -cbeos%m1
                n2B = -cbeos%m2
                n1C = 0.0D00
                n2C = 0.0D00
                n1BB = 0.0D00
                n2BB = 0.0D00
                n1BC = 0.0D00
                n2BC = 0.0D00
                n1CC = 0.0D00
                n2CC = 0.0D00
             endif

! General for all cubic eos
             n1V = 1.0
             n2V = 1.0

             hV = 1.0/n2 - 1.0/n1
             hB = n2B/n2 - n1B/n1
             hC = n2C/n2 - n1C/n1

             hVV = (-1.0/(n2*n2)) + (1.0/(n1*n1))
             hVB = n1B/(n1*n1) - n2B/(n2*n2)
             hVC = n1C/(n1*n1) - n2C/(n2*n2)

             hBB = n2BB/n2 - n1BB/n1 &
                  - (n2B/n2)*(n2B/n2) &
                  + (n1B/n1)*(n1B/n1)

             hBC = n2BC/n2 - n1BC/n1 &
                  + (n1B*n1C)/(n1*n1) &
                  - (n2B*n2C)/(n2*n2)

             hCC =  n2CC/n2 - n1CC/n1 &
                  - (n2C/n2)*(n2C/n2) &
                  + (n1C/n1)*(n1C/n1)


             ! 1'st
             gV = 1.0/n0 - 1.0/V
             gB = -1.0/n0

             fV = hV/mBR
             fB = (1.0/mBR) * (hB - h*mB/m - h/B)

! Or alternative short form
!             fB = hB/mBR - f/B - f*mB/m
! From michelsen - 2 param with constant m1 and m2
!             fB = -(f+V*fV)/B

             fc = hC/mBR - f*mC/m

             ! 2'nd
             gVV = -1.0/(n0*n0)+1.0/(V*V)
             gVB = 1.0/(n0*n0)
             gBB = -gVB

             fVV = hVV/mBR

! From, mich - 2 param const m1 and m2
             fVB = -(2.0*fV+V*fVV)/B

! General alternative formulation
             fVB = (1.0/mBR) * (hVB - hV*mB/m - hV/B)

             fVC = (1.0/mBR) * (hVC - hV*mC/m)

             fBB = -fB*(1.0/B + mB/m) + (1.0/mBR)*(hBB - hB*(mB/m+1.0/B) &
                  + h*(1.0/B**2 + mB**2/m**2 - mBB/m))

             fBC = -fB*mC/m + (1.0/mBR) * (hBC - (hC*mB + h*mBC)/m &
                  + h*mB*mC/(m*m) - hC/B)

             fCC = (1.0/mBR) * (hCC - (2*hC*mC + h*mCC)/m &
                  + 2*h*mC*mC/(m*m))

             ! The reduced Helmholz function
             cbeos%ff = -g*sumn - (D/T)*f

             ! Now for the Helmholz-derivatives

             ! 1'st deriv
             cbeos%ffn = -g
             cbeos%ffv = -gV*sumn - (D/T)*fV
             cbeos%ffa = -f/T
             cbeos%ffb = -gB*sumn -(D/T)*fB
             cbeos%ffc = -(D/T) * fC
             cbeos%fft = D*f/(T*T)

             ! 2'nd deriv
             cbeos%fftt = -2.0*cbeos%fft/T

             cbeos%ffaa = 0.0D00
             cbeos%ffab = -fB/T
             cbeos%ffac = -fC/T
             cbeos%ffat = f/(T*T)

             cbeos%ffbb = -gBB*sumn - (D/T)*fBB
             cbeos%ffbc = -(D/T)*fBC
             cbeos%ffbt = D*fB/(T*T)

             cbeos%ffcc = -(D/T) * fCC
             cbeos%ffct = (D/(T*T))*fC

             cbeos%ffnv = -gV
             cbeos%ffnb = -gB
             cbeos%ffna = 0.0D0
             cbeos%ffnc = 0.0
             cbeos%ffnn = 0.0D0
             cbeos%ffnt = 0.0D0

             cbeos%ffvt =  D*fV/(T*T)
             cbeos%ffvv = -gVV*sumn - (D/T)* fVV
             cbeos%ffva = -fV/T
             cbeos%ffvb = -gVB*sumn - (D/T)* fVB
             cbeos%ffvc = -(D/T) * fVC

             ! Pressure derivative dp/dC

             cbeos%pc = -R * T * cbeos%ffvc

          end select ! Choice of eos
       endif ! ffn < 0
    endif ! n < 0
  end subroutine cbCalcDerivatives_svol


  !> Explicit calculation of pressure including the volume and temperature derivative
  ! using the current cubic equation of state.
  !
  ! \param T - Temperature [K]
  ! \param v - Specific volume [m3/kmol] (you read correctly, it is not SI)
  !
  !
  subroutine cbCalcPressure (nc,cbeos,T,v,z,p,dpdv,dpdt,d2pdv2,dpdz,recalculate)
    use cubic_eos, only: cb_eos
    use tpcbmix, only: cbCalcMixtureParams
    use cbHelm, only: cbPress, cbPrst, cbPv, cbPvv, cbPi
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t, v
    real, intent(in), dimension(nc) :: z
    real, intent(out) :: p
    real, optional, intent(out) :: dpdv, dpdt, d2pdv2
    real, dimension(nc), optional, intent(out) :: dpdz
    logical, optional, intent(in) :: recalculate
    ! Locals
    logical :: recalculate_loc
    !
    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    end if

    if (recalculate_loc) then
      call cbCalcMixtureParams(nc,cbeos,t,z)
      call cbCalcDerivatives_svol(nc, cbeos, T, v)
    end if

    p = cbPress(cbeos,T,V)
    if (present(dpdv)) then
      dpdv = cbPv(cbeos)
    endif
    if (present(dpdt)) then
      dpdt = cbPrsT(cbeos)
    endif
    if (present(d2pdv2)) then
      d2pdv2 = cbPvv(cbeos,T,v)
    endif
    if (present(dpdz)) then
      call cbPi(nc,cbeos,dpdz)
    endif
  end subroutine cbCalcPressure

  !-----------------------------------------------------------------------------
  !> Find a root z0 of the cubic polynomial
  !! \f[
  !!   f(z) = z^3 + p2 z^2 + p1 z + p0,
  !! \f]
  !!
  !! to solve for for the compressibility factor.
  !!
  !! \author MAG, 2013-09
  !!
  !! \param p2     - Polynomial coefficient
  !! \param p1     - Polynomial coefficient
  !! \param p0     - Polynomial coefficient
  !!
  !! \return z     - Root found, initial guess as input
  !! \return lconverged - True if solver converged
  !-----------------------------------------------------------------------------
  subroutine cb_solve_cubic_zfac(p2,p1,p0,z,lconverged)
    use numconstants, only: machine_prec
    implicit none
    ! Arguments:
    real,    intent(in)    :: p2,p1,p0
    real,    intent(inout) :: z
    logical, intent(out)   :: lconverged
    ! Locals:
    real               :: zn,dzn,fn,dfn,ddfn,discr,fnl
    integer            :: iiter,irelax
    integer, parameter :: maxiters=50,maxrelaxes=3
    real,    parameter :: dztol=100.0*machine_prec
    !---------------------------------------------------------------------------
    zn = z
    lconverged=.false.
    !
    do iiter = 1,maxiters
      ! Evaluate f and its derivatives.
      fn    = f(zn)
      dfn   = df(zn)
      ddfn  = ddf(zn)
      ! Find the discriminant of the quadratic equation for dz.
      discr = 1.0 - 2.0*fn*ddfn/dfn**2
      if (discr >= 0.0) then
        ! Find dz according to a third-order Newton method.
        dzn = -(dfn/ddfn)*(1.0-sqrt(discr))
      else
        ! The quadratic equation for dz has complex solutions.
        ! Revert to the regular, second-order Newton's method.
        dzn = -fn/dfn
      end if
      zn = zn + dzn
      !
      ! Check for convergence.
      if ( abs(dzn/zn) < dztol ) then
        lconverged = .true.
        exit
      end if
      !
      ! Relax. Shorten the step if it did not decrease f.
      dzn = -dzn
      do irelax = 1,maxrelaxes
        fnl = f(zn)
        if (abs(fnl) >= abs(fn)) then
        !  if (fnl*fn < 0.0) then
            ! Secant line search
        !    if (dzn > 0.0) then
        !      dzn = max(0.05*dzn,-fn*dzn/(fnl-fn))
        !    else
        !      dzn = min(0.05*dzn,-fn*dzn/(fnl-fn))
        !    endif
        !  else
            dzn = 0.5*dzn
        !  endif
          zn  = zn + dzn
        else
          exit
        end if
      end do
      !
    end do
    !
    z = zn
    !
  contains
    !
    pure function f(z)
      real             :: f
      real, intent(in) :: z
      f = p0 + z*( p1 + z*( p2 + z ))
    end function f
    !
    pure function df(z)
      real             :: df
      real, intent(in) :: z
      df = p1 + z*( 2.0*p2 + 3.0*z)
    end function df
    !
    pure function ddf(z)
      real             :: ddf
      real, intent(in) :: z
      ddf = 6.0*z + 2.0*p2
    end function ddf
    !
  end subroutine cb_solve_cubic_zfac

  !-----------------------------------------------------------------------------
  !> Having found the compressibility factor z = z0, that satisfies f(z0) = 0
  !! for the cubic polinomial
  !!
  !! \f[
  !!   f(z) = z^3 + p2 z^2 + p1 z + p0,
  !! \f]
  !!
  !! we can find the others analytically by solving the quadratic
  !! equation
  !!
  !! \f{align*}{
  !!   g(z) &= z^2 + (p2 + z0) z + (p2 z0 + z0^2 +p1 ) \\
  !!   & = z^2 +    q1     z +        q0\\
  !!   & = 0.\\
  !! \f}
  !!
  !! \author MAG, 2013-09
  !!
  !! \param p2     - Polynomial coefficient
  !! \param p1     - Polynomial coefficient
  !! \param z0     - Root of polynomial
  !!
  !! \return zl    - Smallest real root found
  !! \return zg    - Largest real root found
  !! \return iflag - Return code
  !!   ifalg = 1 : Converged to a single, real root
  !!   iflag = 2 : Converged to three, real though maybe degenerate, roots
  !-----------------------------------------------------------------------------
  subroutine cb_cubic_second_zfac(p2,p1,z0,zl,zg,iflag)
    implicit none
    ! Arguments:
    real,    intent(in)    :: p2,p1,z0
    real,    intent(out)   :: zl,zg
    integer, intent(out)   :: iflag
    ! Locals:
    real               :: q0,q1
    real               :: z1,z2,discr
    !---------------------------------------------------------------------------
    !
    ! Solve quadratic equation g(z) = 0 to find the other roots.
    q0    = z0**2 + p2*z0 + p1
    q1    = p2 + z0
    discr = q1**2 - 4*q0
    if (discr < 0.0) then
      ! The two other roots of f are a complex conjugate pair,
      ! so f has only one real root.
      iflag = 1
    else
      ! The two other roots of f are also real, so f has
      ! three real, though maybe degenerate, roots.
      iflag = 2
      discr = sqrt(discr)
      z1 = 0.5*( -q1 + discr )
      ! Refine by a single Newton step.
      z1 = z1-g(z1)/dg(z1)
      z2 = 0.5*( -q1 - discr )
      ! Refine by a single Newton step.
      z2 = z2-g(z2)/dg(z2)
    end if
    !
    select case (iflag)
    case (1)
      ! Found one real root.
      zl = z0
      zg = z0
    case (2)
      ! Found three real, though maybe degenerate, roots.
      zl = min(z0,z1,z2)
      zg = max(z0,z1,z2)
    end select
    !
  contains
    !
    pure function g(z)
      real             :: g
      real, intent(in) :: z
      g = q0 + z*( q1 + z )
    end function g
    !
    pure function dg(z)
      real             :: dg
      real, intent(in) :: z
      dg = q1 + 2.0*z
    end function dg
    !
  end subroutine cb_cubic_second_zfac

  !>     This subroutine solves a cubic polynomial
  !!
  !!     f(z) = z**3 + pp*z**2 + qq*z + rr = 0
  !!
  !!     with a third order method
  !!
  !!     Relaxation is necessarry in the critical region because of
  !!     both the first and second derivative approach zero
  !!
  !!     Original TPlib routine converted to use new data structure
  !!
  !!     \param pp - 1'st polynmominal coefficient
  !!     \param qq - 2'nd polynominal coefficient
  !!     \param rr - 3'rd polynominal coefficient
  !!
  !!     \param  z - Initial and return value for the compressibility factor
  !!
  !!     \return ifail - Return code
  !!       ifail = 0 : Converged, the gradient toward the solution is positive
  !!       ifail = 1 : Not converged after maximum number of iterations
  !!       ifail = 2 : Converged, the gradient toward the solution is negative
  !!       ifail = 3 : Not converged and the maximum number of relaxtions has been reach wi
  !!
  subroutine cbSolveCubicZfac(pp,qq,rr,z,ifail)
    implicit none
    ! Arguments:
    real,    intent (in)    :: pp,qq,rr
    real,    intent (inout) :: z
    integer, intent (out)   :: ifail
    ! Local constants:
    integer, parameter :: maxiter=50
    integer, parameter :: maxrelax=15
    real,    parameter :: eps=10.0*epsilon(0.0) ! 1.0E-18
    ! Local variables
    real :: fz,dfz,ddfz,dz,argum,fznew,znew
    integer ::iter,nrelax
    !
    fz=rr+z*(qq+z*(pp+z))
    iter=0
    ifail=1
    dz=1.0e10*eps
    !
    !do while (iter < maxiter .and. abs(dz) >= eps)
    do while (iter < maxiter .and. abs(dz/z) >= eps) ! Here we check for convergence in dz, but dz may be modified by the relaxing procedure!
      dfz=qq+z*(2.0*pp+3.0*z)
      ddfz=2.0*pp+6.0*z
      argum=1.0-2.0*fz*ddfz/dfz**2
      if (argum<0.0) argum=1.0
      argum=sqrt(argum)
      dz=-(fz/dfz)*2.0/(1.0+argum)
      znew=z+dz
      !if (abs(dz) < eps) then ! finish
      if (abs(dz/z) < eps) then ! finish
        if (dfz < 0.0) then
          ifail = 2
        else
          ifail = 0
        end if
        z = znew
      else ! abs(dz/z) > eps
        fznew = rr+znew*(qq+znew*(pp+znew))
        nrelax = 0
        do while (abs(fznew) >= abs(fz) .and. nrelax < maxrelax)
          nrelax = nrelax + 1
          if (nrelax.ge.maxrelax) then
            ifail = 3
          endif
          dz = dz*0.5
          znew = z + dz
          fznew = rr+znew*(qq+znew*(pp+znew))
        end do
        z = znew
        fz = fznew
        iter = iter + 1
      end if
    end do
    return
  end subroutine cbSolveCubicZfac


  subroutine cbSolveCubicZfacMinimumGibb (cbeos, T, p, pp, qq, rr, big, zfac, ifail, phase)
    use cubic_eos, only: cb_eos
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in) :: T, p
    real, intent (in) :: pp, qq, rr
    type(cbBig), intent(in) :: big
    real, intent (inout) :: zfac
    integer, intent (out) :: ifail, phase

    ! Local variables
    real :: zl, zg, gl, gg
    logical :: lconverged

    call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
    call cb_cubic_second_zfac(pp,qq,zfac,zl,zg,ifail)
    if (zl < big%B) then
      gl = 1.0E20
    else
      call cbGres(cbeos,T,p,zl,gl)
    end if
    if(zg < big%B) then
      gg = 1.0E20
    else
      call cbGres(cbeos,T,p,zg,gg)
    end if
    if (gl < gg) then ! Select the root with lowest Gibbs Free Energy
      zfac = zl
      phase = LIQPH
    else
      zfac = zg
      phase = VAPPH
    end if
    if (ifail == 1) then
      phase = SINGLEPH
    endif

  end subroutine cbSolveCubicZfacMinimumGibb


  !< Routine for calculation of the compressibility factor for a cubic equation of states
  !! This routine is based on the original TPlib routine CB_ZFAC
  !!
  !!  \param T - Temperature [K]
  !!  \param p - Pressure [Pa]
  !!  \param z - Composition vector (1..nc) [mol/mol]
  !!  \param iphase - Phase for which root to return.
  !!            1: Smallest possible (Liquid)
  !!            2: Largest possible (Vapour)
  !!  \param gflag - The TPlib "Gunder" flag
  !!            1: Normal
  !!            2: if the metastable maxima or minima of the z-factor are to be returned
  !!            3: If possibilities of having three roots, return the one having minimum
  !!               Gibbs free energy is to be returned - calls cbGres
  !! \param dZdt - Compressibility factor differential wrpt. temperature
  !! \param dZdp - Compressibility factor differential wrpt. pressure
  !! \param dZdz - Compressibility factor differential wrpt. mole numbers
  !! \param zfac - The compressibility factor
  !!
  !! We are here solving the cubic equation:
  !! z^3 + p*z^2 + q*z + r = 0
  !! Differentiating we get the quadratic equation:
  !! 3z^2 + 2p*z + q = 0
  !! With solution:
  !! z = ((-2p) +- sqrt(argum))/6
  !! argum = (2p)^2-12q
  !! if argum <= 0; The cubic equation will only have one root.
  !!
  subroutine cbCalcZfac(nc,cbeos,T,p,z,iphase,zfac,gflag_opt,dZdt,dZdp,dZdz,minGphase)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use tpcbmix, only: cbCalcMixtureParams
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T,P,z(:)
    real, intent (out) :: zfac
    integer, intent(in) :: iphase, gflag_opt
    real, intent(out),optional :: dZdt,dZdp
    real, dimension(nc), intent(out),optional :: dZdz
    integer, optional, intent(out) :: minGphase
    ! Local variables
    integer :: iphase_loc
    real :: pp,qq,rr,z1,z2,f1,f2,f3,argum
    type(cbBig) :: big
    integer :: IFAIL1
    integer :: gunder
    real :: zFacGas
    !logical :: lconverged

    ! Calculate first the mixing rules
    call cbCalcMixtureParams (nc,cbeos,T,Z)

    ! Common for all cubic eos ?
    Big%A = cbeos%suma*p/(T*kRgas*t*kRgas)
    Big%B = cbeos%sumb*p/(T*kRgas)
    Big%C = cbeos%sumc*p/(T*kRgas)
    !
    zFacGas = max(2.0,Big%B*1.001)
    !
    !     Initiate ifail
    gunder = gflag_opt
    iphase_loc = iphase
    if (.not. (iphase == VAPPH .or. iphase == LIQPH) ) then
      if (iphase == SINGLEPH) then
        iphase_loc = VAPPH ! shouldn't matter if it's set to VAPPH or LIQPH
      else
        print *, "phase", iphase
        call stoperror('cbCalcZfac: You must give phase , 1 or 2 when' &
             //' calculating the compressibillity factor')
      end if
    endif

    cbeos%extrm(iphase_loc) = 0
    !
    !     Calculate the coefficients for third order equation based on m1 and m2
    pp = big%B*(-cbeos%m1 - cbeos%m2 - 1) - 1.0
    qq = big%B *((cbeos%m1+cbeos%m2)*(big%B+1) &
         + big%B * cbeos%m1 * cbeos%m2) + big%A
    rr = -(big%B**3 + big%B**2)*(cbeos%m1*cbeos%m2) - big%A*big%B
    argum = (2.0*pp)**2-12.0*qq
    f3 = rr+cbeos%single(1)%zcrit*(qq+cbeos%single(1)%zcrit*(pp+cbeos%single(1)%zcrit)) ! Beware for 3 param eq.

    if (cbeos%cubic_verbose) then
      write (*,*) 'zcrit ',cbeos%single%zcrit
      write (*,*) 'm1 ',cbeos%m1
      write (*,*) 'm2 ',cbeos%m2
      write (*,*) 'bigA ',big%A
      write (*,*) 'bigB ',big%B
      write (*,*) 'bigC ',big%C
      write (*,*) 'pp ',pp
      write (*,*) 'qq ',qq
      write (*,*) 'rr ',rr
      write (*,*) 'argum ',argum
      write (*,*) 'f3 ',f3
    endif

    if (argum <= 0.0) then !  Only one root.
      cbeos%nextrm(iphase_loc) = 0
      cbeos%nzfac(iphase_loc) = 1
      if (f3 > 0.0) then
        zfac = big%B ! Initial value from liquid side
      else
        zfac = zFacGas ! Initial value from ideal gas
      end if
      !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
      call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
    else ! Two or three roots
      cbeos%nextrm(iphase_loc) = 2
      z1 = ((-2.0)*pp-sqrt(argum))/6.0
      z2 = ((-2.0)*pp+sqrt(argum))/6.0
      f1 = rr+z1*(qq+z1*(pp+z1))
      f2 = rr+z2*(qq+z2*(pp+z2))
      if (f1*f2 > 0.0) then ! Only one root possible ?
        cbeos%nzfac(iphase_loc) = 1
        if (f3.gt.0.0d0) then
          if (iphase_loc.eq.1) then
            zfac = big%B
            !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
            call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
          else
            if (gunder == 2) then
              zfac = z2
              zfac = max(zfac,big%B*1.001) ! Ensure v > b
              cbeos%extrm(iphase_loc) = 1
            else
              zfac = big%B
              !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
              call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
            end if
          end if
        else ! f3 <= 0
          if (iphase_loc == 1) then
            if (gunder == 2) then
              zfac = Z1
              zfac = max(zfac,big%B*1.001) ! Ensure v > b
              cbeos%extrm(iphase_loc) = 1
            else
              zfac = zFacGas
              !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
              call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
            end if
          else
            zfac = zFacGas
            !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
            call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
          end if
        end if
      else ! f1*f2 <= 0 Three roots possible
        cbeos%nzfac(iphase_loc) = 3
        if (gunder == 3) then
          call cbSolveCubicZfacMinimumGibb (cbeos,T,p,pp,qq,rr,big,zfac,ifail1,minGphase)
        else ! Gunderflag 1 or 2
          if (iphase_loc == 1) then
            zfac = big%B
            !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
            call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
            if(zfac < big%B) then ! Re-initialize from vapour side ....
              zfac = zFacGas
              !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
              call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
            end if
          else
            zfac = zFacGas
            !call cb_solve_cubic_zfac(pp,qq,rr,zfac,lconverged)
            call cbSolveCubicZfac(pp,qq,rr,zfac,ifail1)
          end if
        end if
      end if
    end if

    call cbCalcZfacDiff(nc,cbeos,T,p,zfac,dZdt,dZdp,dZdz)

  end subroutine cbCalczfac

  !< Routine for calculation of the compressibility factor for a cubic equation of states
  !! This routine is based on the original TPlib routine CB_ZFAC
  !!
  !!  \param T - Temperature [K]
  !!  \param p - Pressure [Pa]
  !!  \param z - Composition vector (1..nc) [mol/mol]
  !!  \param iphase - Phase for which root to return.
  !!            1: Smallest possible (Liquid)
  !!            2: Largest possible (Vapour)
  !!  \param gflag - The TPlib "Gunder" flag
  !!            1: Normal
  !!            2: if the metastable maxima or minima of the z-factor are to be returned
  !!            3: If possibilities of having three roots, return the one having minimum
  !!               Gibbs free energy is to be returned - calls cbGres
  !! \param dZdt - Compressibility factor differential wrpt. temperature
  !! \param dZdp - Compressibility factor differential wrpt. pressure
  !! \param dZdz - Compressibility factor differential wrpt. mole numbers
  !! \param zfac - The compressibility factor
  !!
  !!
  !!
  subroutine cbCalcZfacDiff(nc,cbeos,T,p,zfac,dZdt,dZdp,dZdz)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbPi,cbPrst
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T,P
    real, intent (in) :: zfac
    real, intent(out),optional :: dZdt,dZdp
    real, dimension(nc), intent(out),optional :: dZdz
    ! Locals
    real ::  dpdt,dvdt,dpdni(nc)

    ! Why is it not possible to disable this call?
    !if (present(dZdt) .or. present(dZdp) .or. present(dZdz)) then
      call cbCalcDerivatives(nc,cbeos,T,p,zfac)
    !endif

    if (present(dzdt)) then
      ! Start development of Z-derivatives here
      dpdt = cbPrst(cbeos)
      dvdt = -dpdt/cbeos%pv
      dzdt = p*dvdt/(kRgas*T) - zfac/T
    endif
    if (present(dzdp)) then
      dzdp = p/(cbeos%pv*kRgas*T) + zfac/p
    endif

    if (present(dZdz)) then
      call cbPi(nc,cbeos,dpdni)
      !dvdni = -dpdni/cbeos%pv
      dZdz =  -p/(kRgas*T)*dpdni/cbeos%pv - zfac
    endif
  end subroutine cbCalcZfacDiff


  !> The function to find the residual Gibbs free energy for the cubic equation of state
  !!
  !! This function is called if the "gflag" is set to 3 in the cbCalcZfac routine.
  !!
  !! \param T - Temperature [K]
  !! \param p - Pressure [Pa]
  !! \param zfac - Compressibility factor
  !! \param gr - The residual Gibbs free energy
  !! \param dgrdt - The residual Gibbs free energy temperature differential
  !! \param dgrdp - The residual Gibbs free energy pressure differential
  !!
  subroutine cbGres(cbeos, T, p, zfac, gr, dgrdt, dgrdp)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbFt
    implicit none
    class(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: t,p,zfac
    real, intent(out) :: gr
    real, optional, intent(out) :: dgrdt, dgrdp

    !     local declarations
    real :: v,n,n1,n2,lnn,m,ares
    real :: ff, ffn, dfdt

    v = zfac*kRgas*T/P
    n = v-cbeos%sumb
    n1 = v-cbeos%m1*cbeos%sumb
    n2 = v-cbeos%m2*cbeos%sumb
    lnn = n2/n1

    if (lnn < 0.0) then
      call StopError ("Parameter zfac or v are out of range")
      gr = 0.0
      return
    end if

    lnn = log(lnn)

    ffn = v/n
    if (ffn <= 0) then
      call StopError("Parameter ffn has become negative in cbGres")
      gr = 0.0
      return
    endif
    ffn = log(ffn)

    if (cbeos%m1 == cbeos%m2) then
      ff = ffn - cbeos%suma /(kRgas*t*n1)
    else
      m = cbeos%m1-cbeos%m2
      ff = ffn - cbeos%suma*lnn/(m*cbeos%sumb*kRgas*t)
    endif

    if (zfac > 0.0) then
      ares = kRgas*t*(ff - log(zfac))
    else
      ares = kRgas*t*(ff - log(-zfac))
    end if

    gr =  ares + kRgas*t*(zfac-1.0)

    if (present(dgrdt)) then
      dfdt = cbFt( cbeos )
      dgrdt = kRgas*(T*dfdt+ff-log(abs(zfac)))
    endif

    if (present(dgrdp)) then
      dgrdp = (zfac-1.0)*kRgas*T/P
    endif
  end subroutine cbGres

  !> The subroutine finds the fugacity coefficients and derivatives
  !!
  !! \param T - Temperature [K]
  !! \param P - Pressure [Pa]
  !! \param Zfac - Compressibility factor
  !! \param dlnfdt - Fugacity coefficients differential wrpt. temperature
  !! \param dlnfdp - Fugacity coefficients differential wrpt. pressure
  !! \param dlnfdz - Fugacity coefficients differential wrpt. mole numbers
  !! \param res_fug - Fugacity coefficients
  !!
  !! \author Oivind Wilhelmsen
  !! \date 2012-06-13

  subroutine cbCalcFug(nc,cbeos,T,p,Zfac,res_fug,dlnfdt,dlnfdp,dlnfdz)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbFt, cbFit, cbPrst, cbPi, cbFij, cbFi
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T,P,Zfac   ! Only compressibility without derivatives
    real, dimension(nc), intent (out) :: res_fug
    real, dimension(nc), optional, intent (out) :: dlnfdt, dlnfdp
    real, dimension(nc,nc), optional, intent (out) :: dlnfdz

    ! Local variables
    real :: dfdni(nc), Term, dpdni(nc), d2fdnidt(nc), d2fdnidnj(nc,nc), dpdt
    integer :: i,j

    call cbFi(nc,cbeos,dfdni)
    do i=1,nc
      Term = dfdni(i)-log(Zfac)

      if(Term .GT. 500.0) then
        Term = 500.0
      endif

      res_fug(i)=exp(Term)  ! The fugacity coefficients
    end do

    if(present(dlnfdt)) then  ! Temperature derivatives
      dpdt = cbPrst(cbeos)
      call cbFit(nc, cbeos, d2fdnidt)
      call cbPi(nc, cbeos, dpdni)
      dlnfdt=d2fdnidt+dpdni*dpdt/(kRgas*T*cbeos%pv)+1.0/T
    end if

    if(present(dlnfdp)) then ! Pressure derivative
      call cbPi(nc, cbeos, dpdni)
      dlnfdp=-dpdni/(kRgas*T*cbeos%pv)-1.0/P
    end if

    if(present(dlnfdz)) then ! Composition derivative
      call cbPi(nc, cbeos, dpdni)
      call cbFij(nc,cbeos,d2fdnidnj)
      do i=1,nc
        do j=1,nc
          dlnfdz(j,i)= d2fdnidnj(j,i)+dpdni(i)*dpdni(j)/(kRgas*T*cbeos%pv)+1.0
        end do
      end do
    end if

  end subroutine cbCalcFug

  !> The subroutine finds the entropy residual of cubic Equations of State
  !! optionally the derivatives
  !!
  !! \param T - Temperature [K]
  !! \param P - Pressure [Pa]
  !! \param Z - Composition [-]
  !! \param phase - Phase (1=liquid, 2=vapour)
  !! \param gflag_opt The TPlib "Gunder" flag
  !!            1: Normal
  !!            2: if the metastable maxima or minima of the z-factor are to be returned
  !!            3: If possibilities of having three roots, return the one having minimum
  !!               Gibbs free energy is to be returned - calls cbGres
  !! \param res_entropy - Residual entropy
  !! \param dsdt - Residual entropy differential wrpt. temperature
  !! \param dsdp - Residual entropy differential wrpt. pressure
  !! \param dsdz - Residual entropy differential wrpt. mole numbers
  !!
  !! \author Oivind Wilhelmsen
  !! \date 2012-06-13

  subroutine cbCalcEntropy(nc,cbeos,T,p,Z,phase,res_entropy,gflag_opt,dsdt,dsdp,dsdz)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbFtt, cbFit, cbPrst, cbFt, cbPi, cbFi
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T,P
    real, dimension(nc), intent (in)  :: Z
    real, intent (out) :: res_entropy
    integer, intent(in) :: gflag_opt
    integer, intent(in) :: phase
    real, intent(out),optional :: dsdt,dsdp
    real, dimension(nc), intent(out),optional ::dsdz
    ! Local variables
    real :: dfdt, dpdt, d2fdt2, CVres, dpdni(nc), d2fdnidt(nc), dfdni(nc), Term ! Nc+3
    integer :: i
    real :: Zfac

    call cbCalcZfac(nc,cbeos,T,P,Z,phase,Zfac,gflag_opt)
    dpdt=cbPrst(cbeos)
    dfdt=cbFt(cbeos)
    res_entropy=(-kRgas)*(T*dfdt+cbeos%ff-log(abs(Zfac)))

    if(present(dsdt)) then  ! Temperature derivative
      d2fdt2 = cbFtt(cbeos)
      CVres=(-kRgas)*T*(T*d2fdt2+2.0*dfdt)
      dsdt=CVres/T-dpdt**2/cbeos%pv-kRgas/T
    end if

    if(present(dsdp)) then    !Pressure derivative
      dsdp=dpdt/cbeos%pv+kRgas/P
    end if

    if(present(dsdz)) then ! Composition derivative
      call cbFiT(nc, cbeos, d2fdnidt)
      call cbPi(nc, cbeos, dpdni)
      call cbFi(nc, cbeos, dfdni)
      do i=1,nc
        dsdz(i)=d2fdnidt(i)+dpdni(i)*dpdt/(kRgas*T*cbeos%pv)+1.0/T
        dsdz(i)=-dsdz(i)*kRgas*T**2  ! dHresdT

        term = dfdni(i)-log(abs(Zfac)) ! log(fug_i)
        dsdz(i)=dsdz(i)/T-kRgas*term ! dHresdT/T-Rlog(fug_i)
      end do
    end if

  end subroutine cbCalcEntropy

  !> The subroutine finds the enthalpy residual of cubic Equations of State
  !! optionally the derivatives
  !!
  !! \param T - Temperature [K]
  !! \param P - Pressure [Pa]
  !! \param Z - Composition [-]
  !! \param phase - Phase (1=liquid, 2=vapour)
  !! \param gflag_opt The TPlib "Gunder" flag
  !!            1: Normal
  !!            2: if the metastable maxima or minima of the z-factor are to be returned
  !!            3: If possibilities of having three roots, return the one having minimum
  !!               Gibbs free energy is to be returned - calls cbGres
  !! \param res_enthalpy - Residual enthalpy
  !! \param dhdt - Residual enthalpy differential wrpt. temperature
  !! \param dhdp - Residual enthalpy differential wrpt. pressure
  !! \param dhdz - Residual enthalpy differential wrpt. mole numbers
  !!
  !! \author Oivind Wilhelmsen
  !! \date 2012-06-14

  subroutine cbCalcEnthalpy(nc,cbeos,T,p,Z,phase,res_enthalpy,gflag_opt,dhdt,dhdp,dhdz)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbFt, cbFtt, cbFit, cbPrst, cbPi
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T,P
    real, dimension(nc), intent (in)  :: Z
    integer, intent(in) :: gflag_opt
    real, intent (out) :: res_enthalpy
    integer, intent(in) :: phase
    real, intent(out), optional :: dhdt, dhdp
    real, dimension(nc), intent(out), optional ::dhdz

    ! Local variables
    real :: Ures, Ares, Sres, Cvres, dfdt, d2fdt2, dpdt, dpdni(nc), &
         d2fdnidt(nc)
    real :: Zfac

    call cbCalcZfac(nc,cbeos,T,P,Z,phase,Zfac,gflag_opt)

    dfdt = cbFt(cbeos)
    Ares = kRgas*T*(cbeos%ff-log(abs(Zfac)))
    Sres = (-kRgas)*(T*dfdt+cbeos%ff-log(abs(Zfac)))
    Ures = (-kRgas)*T**2*(dfdt)
    res_enthalpy = Ures + kRgas*T*(Zfac-1.0)
    if (cbeos%cubic_verbose) then
      write (*,*) 'cbCalcEnthalpy'
      write (*,*) 'dfdt: ',dfdt
      write (*,*) 'Ares: ',Ares
      write (*,*) 'Sres: ',Sres
      write (*,*) 'Ures: ',Ures
      write (*,*) 'Zfac: ',Zfac
    endif
    if (present(dhdt)) then ! Temperature derivative
      dpdt = cbPrst(cbeos)
      d2fdt2 = cbFtt( cbeos)
      CVres = (-kRgas)*T*(T*d2fdt2+2.0*dfdt)
      dhdt = CVres-T*dpdt**2/cbeos%pv-kRgas
    end if

    if (present(dhdp)) then ! Pressure derivative
      dpdt = cbPrst(cbeos)
      dhdp = Zfac*kRgas*T/P+T*dpdt/cbeos%pv
    end if

    if (present(dhdz)) then ! Composition derivative
      dpdt=cbPrst(cbeos)
      call cbFit(nc, cbeos, d2fdnidt)
      call cbPi(nc, cbeos, dpdni)
      dhdz=d2fdnidt+dpdni*dpdt/(kRgas*T*cbeos%pv)+1.0/T
      dhdz=(-dhdz)*kRgas*T**2
    end if

  end subroutine cbCalcEnthalpy

  !----------------------------------------------------------------------
  !> The subroutine finds the innerenergy residual of cubic Equations of State
  !! optionally the derivatives
  !!
  !! \param T The temperature [K]
  !! \param v The specific volume [m3/kmole]
  !! \param Z The mole fraction [-]
  !! \param dudt Temperature derivative [J/kmoleK]
  !! \param dudv Pressure derivative [J/m3]
  !!       0: No derivatives
  !!       1: Analytical derivatives
  !!
  !! \retval energy The internal energy with derivatives [-]
  !!
  !! \author Morten Hammer
  subroutine cbCalcInnerEnergy(nc,cbeos,T,v,Z,u,dudt,dudv,recalculate)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use tpcbmix, only: cbCalcMixtureParams
    use cbHelm, only: cbFtt, cbFvt, cbFt
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T,v,Z(1:nc)
    real, intent (out) :: u
    real, optional, intent(out) :: dudt, dudv
    logical, optional, intent(in) :: recalculate

    ! Local variables
    real :: Cvres, dfdt, d2fdt2, d2fdtdv
    logical :: recalculate_loc
    !

    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    end if

    if (recalculate_loc) then
      call cbCalcMixtureParams(nc,cbeos,T,Z)
      call cbCalcDerivatives_svol(nc,cbeos,T,v)
    end if

    dfdt = cbFt(cbeos)
    u = (-kRgas)*T**2*(dfdt)

    if (present(dudt)) then
      d2fdt2 = cbFtt(cbeos)
      CVres = (-kRgas)*T*(T*d2fdt2+2.0*dfdt)
      dudt = CVres
    endif

    if (present(dudv)) then
      d2fdtdv = cbFvt(cbeos)
      dudv = (-kRgas)*T**2*(d2fdtdv)
    endif

  end subroutine cbCalcInnerEnergy

  !> The subroutine calculates the pseudocritical temperature, pressure, acentric factor
  !! compressibility and volume as function of composition.
  !! The pseudo-critical point is the state where with constant Temperature:
  !! (dP/dV)=(d2P/dV^2)=0
  !!
  !! \param Z - Composition [-]
  !! \return Tpc - Pseudo critical temperature [K]
  !! \return Ppc - Pseudo critical pressure [Pa]
  !! \return Zpc - Pseudo critical compressibility [-]
  !! \return Vpc - Pseudo critical volume [m^3/kmole]
  !!
  !! \author Oivind Wilhelmsen
  !! \date 2012-06-14
  subroutine cbCalcPseudo(nc,cbeos,Z,Tpc,Ppc,Zpc,Vpc)
    use thermopack_constants, only: kRgas
    use eosdata
    use cubic_eos, only: cb_eos
    use tpcbmix, only: cbCalcMixtureParams
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: Z(1:nc)
    real, intent (out) :: Tpc, Ppc, Zpc, Vpc

    ! Local variables
    integer :: i, iter
    real :: ft, dft, ddft, Argum, dTpc, omegaA_mix, OmegaB_mix, zcrit_mix
    real :: zcomb, dzcomb, n, n1, n2, f, fd, fdd, argum1
    real, parameter :: Eps=1.0D-10
    integer, parameter :: Maxiter=20

    ! Initial values

    Tpc=0.0
    Ppc=0.0
    iter=0
    zcomb = 5.0
    dzcomb = 1.0E10

    do i=1,nc
      Tpc=Tpc+Z(i)*cbeos%single(i)%tc
      Ppc=Ppc+Z(i)*cbeos%single(i)%pc
    end do

    ! make sure m1 and m2 are calculated for pseudeo critical t and p
    !call cbCalcParameters (nc, comp, cbeos, Tpc, Z)

    ! First, find the Omega_B and Omega_B of the "mixture" which is such that
    ! (dP/dV)=(d2P/dV2)=0 in a critical point. This is not necessary for three-
    ! parameter cubic EoS because m1 and m2 do not depend on composition

    if (cbeos%subeosidx == cbSW) then !> For the Schmidt and Wenzel EOS
      do while (iter < maxiter .and. abs(dzcomb) > eps)
        n  = zcomb - 1.0
        n1 = zcomb - cbeos%m1
        n2 = zcomb - cbeos%m2
        f = n*n1**2+n*n1*n2+n*n2**2-n1**2*n2-n1*n2**2
        fd = 3.0*(n*n1+n*n2-n1*n2)
        fdd = 6.0*n
        argum1 = 1.0-2.0*f*fdd/fd**2
        if (argum1.gt.0.0) then
          argum1 = sqrt(argum1) ! changed sqrt(argum) to sqrt(argum1) - GS 2012-12-22
        else
          argum1 = 1.0
        end if

        dzcomb = (-2.0)*f/(fd*(1.0+argum1))
        zcomb = zcomb+dzcomb
        iter = iter + 1
      enddo

      ! if (iter == maxiter) call StopError ("Third order method failed to calculate zcrit")
      !  write (*,*) 'Calculation of ohmega/zcrit converged after ',iter-1,' iterations for CB  EOS'
      ! endif

      argum1 = n1*n2/(n*n1+n*n2)
      omegaB_mix = (1.0-argum1)/(zcomb-1.0)
      zcrit_mix = omegaB_mix*zcomb
      omegaA_mix = omegaB_mix*(1.0/n-omegaB_mix)*n1*n2
    elseif (cbeos%subeosidx == cbPT) then
       ! for now
      omegaA_mix=cbeos%single(1)%omegaA
      omegaB_mix=cbeos%single(1)%omegaB
      zcrit_mix=cbeos%single(1)%zcrit
    else
      omegaA_mix=cbeos%single(1)%omegaA
      omegaB_mix=cbeos%single(1)%omegaB
      zcrit_mix=cbeos%single(1)%zcrit
    end if

    ! reinitialize
    iter=0
    dTpc=20.0

    !  Start the third order Newton-Raphsons method to find the correspoding pseudo
    ! critical point

    do while(iter<Maxiter .AND. abs(dTpc)>Eps)
      iter=iter+1
      call cbCalcMixtureParams(nc,cbeos,Tpc,Z)
      ft=cbeos%suma-omegaA_mix/omegaB_mix*kRgas*Tpc*cbeos%sumb
      dft=cbeos%at-omegaA_mix/omegaB_mix*kRgas* ( cbeos%sumb + Tpc * cbeos%bt)
      ddft=cbeos%att
      Argum=(1.0D-2)*ft*ddft/dft**2
      if(Argum < 0.0) Argum=1.0
      Argum=sqrt(Argum)
      dTpc=(-ft)/dft*2.0/(1.0+Argum)
      Tpc=Tpc+dTpc
    end do

    if(iter.GE.Maxiter) then
      call StopError ("Pseudo-critical pressure exceeded Maxiter")
    end if

    Zpc=zcrit_mix
    Vpc=Zpc*cbeos%sumb/omegaB_mix

    Ppc=kRgas*Tpc/(Vpc-cbeos%sumb)-cbeos%suma/((Vpc-cbeos%m1*cbeos%sumb)*&
         (Vpc-cbeos%m2*cbeos%sumb))

  end subroutine cbCalcPseudo


!! ":"-delimited text-output with all data from the current EOS
!!
  subroutine cbDumpEosData(nc,cbeos,T, P, zfac)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbFi, cbFit
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos

    real, intent(in) :: T,P, zfac
    integer :: i,j
    real :: dpdt,dfdni(nc),dpdni,dpdnj,d2fdnidt(nc),d2fdnidnj

    write (*,*) 'Calculated parameters for selected EOS: ',cbeos%eosid
    write (*,*) 'Mixing rule:                            ',cbeos%mruleid

    write (*,*) 'm1  :',cbeos%m1
    write (*,*) 'm2  :',cbeos%m2
    do i=1,nc
       write (*,*) 'alphacorr(',i,') :',cbeos%single(i)%alphaCorrName
       write (*,*) 'omegaA(',i,')    :',cbeos%single(i)%omegaA
       write (*,*) 'omegaB(',i,')    :',cbeos%single(i)%omegaB
       write (*,*) 'omegaC(',i,')    :',cbeos%single(i)%omegaC
       write (*,*) 'zcrit(',i,')     :',cbeos%single(i)%zcrit
    enddo

    write (*,*) 'sumA :',cbeos%suma
    write (*,*) 'sumB :',cbeos%sumb
    write (*,*) 'sumC :',cbeos%sumc

    write (*,*) 'At :',cbeos%At
    write (*,*) 'Att :',cbeos%Att

    write (*,*) 'Bt :',cbeos%Bt
    write (*,*) 'Btt :',cbeos%Btt


! Desireable to keep the values in a single column.
! Use this data to import into for instance EXCEL to directly compare values
! old - new
!
    do i=1,nc
       write (*,*) 'ai(',i,') :',cbeos%ai(i)
       write (*,*) 'bi(',i,') :',cbeos%bi(i)
       write(*,*) 'ait(',i,') :',cbeos%ait(i)
       write (*,*) 'bit(',i,') :',cbeos%bit(i)

       do j=1,nc
          write(*,*) 'aij(',i,',',j,') :',cbeos%aij(i,j)
          write(*,*) 'bij(',i,',',j,') :',cbeos%bij(i,j)
! Could also include mixing rule interactionparameters - but these are really constants
! but could be useful to look at when doing regression ...
!
!          write(*,*) 'bij(',i,',',j,') :',cbeos%kij(i,j)
       end do
    end do

    write (*,*) 'pn :',cbeos%pn
    write (*,*) 'pa :',cbeos%pa
    write (*,*) 'pb :',cbeos%pb
    write (*,*) 'pc :',cbeos%pc
    write (*,*) 'pt :',cbeos%pt
    write (*,*) 'pv :',cbeos%pv
    write (*,*) 'ff :',cbeos%ff
    write (*,*) 'ffn :',cbeos%ffn
    write (*,*) 'ffa :',cbeos%ffa
    write (*,*) 'ffb :',cbeos%ffb
    write (*,*) 'ffc :',cbeos%ffc
    write (*,*) 'fft :',cbeos%fft
    write (*,*) 'ffv :',cbeos%ffv
    write (*,*) 'ffnv :',cbeos%ffnv
    write (*,*) 'ffna :',cbeos%ffna
    write (*,*) 'ffnb :',cbeos%ffnb
    write (*,*) 'ffnc :',cbeos%ffnc
    write (*,*) 'ffaa :',cbeos%ffaa
    write (*,*) 'ffab :',cbeos%ffab
    write (*,*) 'ffac :',cbeos%ffac
    write (*,*) 'ffat :',cbeos%ffat
    write (*,*) 'ffbb :',cbeos%ffbb
    write (*,*) 'ffbc :',cbeos%ffbc
    write (*,*) 'ffbt :',cbeos%ffbt
    write (*,*) 'ffcc :',cbeos%ffcc
    write (*,*) 'ffct :',cbeos%ffct
    write (*,*) 'ffnt :',cbeos%ffnt
    write (*,*) 'fftt :',cbeos%fftt
    write (*,*) 'ffvt :',cbeos%ffvt
    write (*,*) 'ffva :',cbeos%ffva
    write (*,*) 'ffvb :',cbeos%ffvb
    write (*,*) 'ffvc :',cbeos%ffvc
    write (*,*) 'ffvv :',cbeos%ffvv

    write (*,*) 'dm1dB  :',cbeos%dm1dB
    write (*,*) 'dm1dC  :',cbeos%dm1dC
    write (*,*) 'dm2dB  :',cbeos%dm2dB
    write (*,*) 'dm2dC  :',cbeos%dm2dC

    write (*,*) 'd2m1dB2  :',cbeos%d2m1dB2
    write (*,*) 'd2m1dC2  :',cbeos%d2m1dC2
    write (*,*) 'd2m2dB2  :',cbeos%d2m2dB2
    write (*,*) 'd2m2dC2  :',cbeos%d2m2dC2

    ! Other differentials needed in departure functions and fugacities
    call cbFi(nc, cbeos, dfdni)
    do i=1,nc
       write (*,*) 'dfdni(',i,') :',dfdni(i)
       write (*,*) 'ln(fug) :', dfdni(i) - log(zfac)
    enddo

    dpdt=cbeos%pt+cbeos%pa*cbeos%at
    write (*,*) 'dpdt :',dpdt
    call cbFit(nc, cbeos, d2fdnidt)
    do i=1,nc
       dpdni=cbeos%pn+cbeos%pa*cbeos%ai(i)+cbeos%pb*cbeos%bi(i)+cbeos%pc*cbeos%ci(i)

!       d2fdnidt =  &
!            +  cbeos%ffat * cbeos%ai(i) + cbeos%ffa*cbeos%ait(i) &
!            + (cbeos%ffbt + cbeos%ffab*cbeos%at)* cbeos%bi(i) &
!            + (cbeos%ffct + cbeos%ffac*cbeos%at)* cbeos%ci(i)

       write (*,*) 'dpdni(',i,') :',dpdni
       write (*,*) 'd2fdnidt(',i,') :',d2fdnidt(i)
       write (*,*) 'ln(dfug/dt) :', d2fdnidt(i)+dpdni*dpdt/(kRgas*T*cbeos%pv)+1.0/T
    end do

    do i=1,nc
       dpdni=cbeos%pn+cbeos%pa*cbeos%ai(i) &
            + cbeos%pb*cbeos%bi(i) &
            + cbeos%pc*cbeos%ci(i)

       write (*,*) 'dpdni(',i,') :',dpdni
       write (*,*) 'ln(dfug/dp) :', -dpdni/(kRgas*T*cbeos%pv)-1.0/P

    end do

    do i=1,nc
       dpdni=cbeos%pn+cbeos%pa*cbeos%ai(i) &
            +cbeos%pb*cbeos%bi(i) &
            +cbeos%pc*cbeos%ci(i)
       write (*,*) 'dpdni(',i,') :',dpdni
       do j=1,nc
          dpdnj=cbeos%pn+cbeos%pa*cbeos%ai(j) &
               + cbeos%pb*cbeos%bi(j) &
               +cbeos%pc*cbeos%ci(j)
          write (*,*) 'dpdnj(',j,') :',dpdnj
          d2fdnidnj = cbeos%ffnb*cbeos%bi(j) &
               + (cbeos%ffab*cbeos%bi(j) + cbeos%ffac*cbeos%ci(j)) * cbeos%ai(i) + cbeos%ffa*cbeos%aij(i,j) &
               + cbeos%ffb*cbeos%bij(i,j) & !Term used in Wong Sandler
               + (cbeos%ffnb + cbeos%ffab*cbeos%ai(j) + cbeos%ffbb*cbeos%bi(j) + cbeos%ffbc*cbeos%ci(j)) * cbeos%bi(i)  &
               + ( cbeos%ffac*cbeos%ai(j) + cbeos%ffbc*cbeos%bi(j) + cbeos%ffcc*cbeos%ci(j)) * cbeos%ci(i) +&
               cbeos%ffc*cbeos%cij(i,j)
          write (*,*) 'd2fdnidnj(',j,',',i,') :',d2fdnidnj
          write (*,*) 'dlnfdz(',j,',',i,') :', d2fdnidnj+dpdni*dpdnj/(kRgas*T*cbeos%pv)+1.0
       end do
    end do


! New - add also interaction parameters
! Put this at the end for now, (GEIR)

    write(*,*) "kij (i:row, j:col):"
    do i=1,nc
       do j=1,nc
          write(*,"(ES20.10)",advance="no") cbeos%kij(i,j)
       end do
       write(*,*)
    end do
    write(*,*)


  end subroutine cbDumpEosData

  !----------------------------------------------------------------------
  !> Subroutine finds Helmholtz free energy residual of cubic EoS
  !! optionally the derivatives
  !! with respect to T, v
  !!
  !! \param T Temperature [K]
  !! \param v Specific volume [m3/kmol]
  !! \param Z Mole fraction [-]
  !! \param dYdt Temperature derivative [J/kmolK]
  !! \param dYdv Pressure derivative [J/m3]
  !!       0: No derivatives
  !!       1: Analytical derivatives
  !!
  !! \retval Y Free energy with derivatives [-]
  !!
  !! \author GL, 2015-01-22
  subroutine cbCalcFreeEnergy(nc,cbeos,T,v,Z,Y,dYdt,dYdv,recalculate)
    use thermopack_constants, only: kRgas
    use cubic_eos, only: cb_eos
    use tpcbmix, only: cbCalcMixtureParams
    use cbHelm, only: cbFt
    implicit none
    integer, intent(in) :: nc
    class(cb_eos), intent(inout) :: cbeos
    real, intent (in)  :: T, v, Z(1:nc)
    real, intent (out) :: Y
    real, optional, intent(out) :: dYdt, dYdv
    logical, optional, intent(in) :: recalculate
    ! Local variables
    real :: f, dfdt, dfdv
    logical :: recalculate_loc

    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    end if

    if (recalculate_loc) then
      call cbCalcMixtureParams(nc, cbeos, T, Z)
      call cbCalcDerivatives_svol(nc, cbeos, T, v)
    end if

    f = cbeos%ff
    dfdt = cbFt(cbeos)
    dfdv = cbeos%ffv

    Y = kRgas*T*f

    if (present(dYdt)) then
      dYdt = kRgas*(f + T*dfdt)
    endif

    if (present(dYdv)) then
      dYdv = kRgas*T*dfdv
    endif

  end subroutine cbCalcFreeEnergy

  !> Calculate resudial reduced Helmholtz and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3/kmol]
  !! \param n - Mole numbers [mol]
  !! \param F... - Resudial reduced Helmholtz differentiuals
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine cbCalcFres(nc,cbeos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn,recalculate)
    use cubic_eos, only: cb_eos
    use cbHelm, only: cbF, cbFv, cbFvv, cbFt, cbFtt, cbFvt, &
         cbFi, cbFij, cbFiv, cbFit
    use tpcbmix, only: cbCalcMixtureParams
    implicit none
    integer, intent(in) :: nc
    real, intent(in) :: T,V,n(nc)
    ! Output.
    class(cb_eos), intent(inout) :: cbeos
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc)
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    logical, optional, intent(in) :: recalculate
    ! Locals
    logical :: recalculate_loc
    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    endif
    if (recalculate_loc) then
      call cbCalcMixtureParams(nc,cbeos,t,n)
      call cbCalcDerivatives_svol(nc, cbeos, T, v)
    endif

    if (present(F)) F = cbF(cbeos)
    if (present(F_T)) F_T = cbFt(cbeos)
    if (present(F_V)) F_V = cbFv(cbeos)
    if (present(F_n)) call cbFi(nc,cbeos,F_n)
    if (present(F_TT)) F_TT = cbFtt(cbeos)
    if (present(F_TV)) F_TV = cbFvt(cbeos)
    if (present(F_Tn)) call cbFit(nc,cbeos,F_Tn)
    if (present(F_VV)) F_VV = cbFvv(cbeos)
    if (present(F_Vn)) call cbFiv(nc,cbeos,F_Vn)
    if (present(F_nn)) call cbFij(nc,cbeos,F_nn)
  end subroutine cbCalcFres

    !> Calculates the contibution to the reduced residual Helmholtz energy F
  !> coming from the cubic eos, along with its derivatives.
  !> Note!!! Input and output in SI units
  subroutine calcCbFder_res_SI(nc,cbeos,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn,F_VVV,recalculate)
    use cbhelm, only: cbF, cbFv, cbFvv, cbFt, cbFtt, cbFvt, cbFvvv, &
         cbFi, cbFij, cbFiv, cbFit
    use tpcbmix, only: cbCalcMixtureParams
    use cubic_eos, only: cb_eos
    ! Input
    integer, intent(in) :: nc
    type(cb_eos), intent(inout) :: cbeos
    real, intent(in) :: T,V,n(nc)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(nc),F_VVV
    real, optional, intent(out) :: F_TT,F_TV,F_Tn(nc),F_VV,F_Vn(nc),F_nn(nc,nc)
    logical, optional, intent(in) :: recalculate
    ! Locals
    logical :: recalculate_loc
    real :: sumn
    sumn = sum(n)

    if (present(recalculate)) then
      recalculate_loc = recalculate
    else
      recalculate_loc = .true.
    endif
    if (recalculate_loc) then
      ! Calculate contributions from the non-association part.
      call cbCalcMixtureParams(nc,cbeos,T,n/sumn)
      call cbCalcDerivatives_svol(nc,cbeos,T,1000*V/sumn)
    endif

    if (present(F)) F = sumn*cbF(cbeos)
    if (present(F_T)) F_T = sumn*cbFt(cbeos)
    if (present(F_V)) F_V = 1000*cbFv(cbeos)
    if (present(F_n)) call cbFi(nc,cbeos,F_n)
    if (present(F_TT)) F_TT = cbFtt(cbeos)*sumn
    if (present(F_TV)) F_TV = 1000*cbFvt(cbeos)
    if (present(F_Tn)) call cbFiT(nc,cbeos,F_Tn)
    if (present(F_VV)) F_VV = 1000*1000*cbFvv(cbeos)/sumn
    if (present(F_VVV)) F_VVV = 1.0e9*cbFvvv(cbeos,T,1000*V/sumn)/sumn**2
    if (present(F_Vn)) then
      call cbFiv(nc,cbeos,F_Vn)
      F_Vn = 1000*F_Vn/sumn
    end if
    if (present(F_nn)) then
      call cbFij(nc,cbeos,F_nn)
      F_nn = F_nn/sumn
    end if
  end subroutine calcCbFder_res_SI

end module tpcubic
