!-----------------------------------------------------------------------------
!> Module for mapping iso-lines
!>
!>
!-----------------------------------------------------------------------------
module isolines
  implicit none
  private
  save

  public :: isotherm, isobar, isentrope, isenthalp

contains

  !-----------------------------------------------------------------------------
  !> Map isotherm
  !>
  !> \author Morten Hammer, 2020-06
  !-----------------------------------------------------------------------------
  subroutine isotherm(T,Pmin,Pmax,z,n,pa,va,sa,ha)
    use parameters, only: nc, TWOPH, SINGLEPH, VAPPH, LIQPH
    use eosTV, only: entropyTV, enthalpyTV
    use eos, only: specificvolume, getCriticalParam
    use thermo_utils, only: isSingleComp
    use saturation, only: safe_bubP
    use tp_solver, only: twoPhaseTPflash
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: Pmin, Pmax !< Pressure limits (Pa)
    real, intent(in) :: z(nc) !< Composition (-)
    integer, intent(in) :: n !< Number of points
    real, intent(out) :: pa(n) !< Pressure (Pa)
    real, intent(out) :: va(n) !< Spcecific volume (m3/mol)
    real, intent(out) :: sa(n) !< Specific entropy (J/mol/K)
    real, intent(out) :: ha(n) !< Specifc enthalpy (J/mol)
    ! Locals
    integer :: i, i_pure, phase(n), n_vap
    real :: Pbub, Y(nc), X(nc), beta, betaL
    real :: vg, vl, sg, sl, hg, hl
    real :: tci,pci,oi,vci

    if (isSingleComp(z)) then
      i_pure = maxloc(z,dim=1)
      call getCriticalParam(i_pure,tci,pci,oi,vci)
      if (T >= tci) then
        phase = VAPPH
        call linspace(Pmin,Pmax,n,pa)
      else
        Pbub = safe_bubP(T,Z,Y)
        if (Pbub <= Pmin) then
          phase = LIQPH
          call linspace(Pmin,Pmax,n,pa)
        else if (Pbub >= Pmax) then
          phase = VAPPH
          call linspace(Pmin,Pmax,n,pa)
        else
          n_vap = ceiling(n*(Pbub - Pmin)/(Pmax-Pmin))
          if (n_vap > nint(n*0.5)) n_vap = n_vap - 1
          phase(1:n_vap) = VAPPH
          call linspace(Pmin,Pbub,n_vap,pa(1:n_vap))
          phase(n_vap+1:n) = LIQPH
          call linspace(Pmin,Pbub,n-n_vap,pa(n_vap+1:n))
        endif
        do i=1,n
          call specificvolume(T,pa(i),z,phase(i),va(i))
          call enthalpyTV(T,va(i),z,ha(i))
          call entropyTV(T,va(i),z,sa(i))
        enddo
      endif
    else
      call linspace(Pmin,Pmax,n,pa)
      do i=1,n
        call twoPhaseTPflash(T,pa(i),Z,beta,betaL,phase(i),X,Y)
        if (phase(i) == SINGLEPH) phase(i) = LIQPH
        if (phase(i) == TWOPH) then
          call specificvolume(T,pa(i),y,VAPPH,vg)
          call enthalpyTV(T,vg,y,hg)
          call entropyTV(T,vg,y,sg)
          call specificvolume(T,pa(i),x,LIQPH,vl)
          call enthalpyTV(T,vl,x,hl)
          call entropyTV(T,vl,x,sl)
          va(i) = beta*vg + betaL*vl
          ha(i) = beta*hg + betaL*hl
          sa(i) = beta*sg + betaL*sl
        else
          call specificvolume(T,pa(i),z,phase(i),va(i))
          call enthalpyTV(T,va(i),z,ha(i))
          call entropyTV(T,va(i),z,sa(i))
        endif
      enddo
    endif
  end subroutine isotherm

  !-----------------------------------------------------------------------------
  !> Map isobar
  !>
  !> \author Morten Hammer, 2020-06
  !-----------------------------------------------------------------------------
  subroutine isobar(P,Tmin,Tmax,z,n,ta,va,sa,ha)
    use parameters, only: nc, TWOPH, SINGLEPH, VAPPH, LIQPH
    use eosTV, only: entropyTV, enthalpyTV
    use eos, only: specificvolume, getCriticalParam
    use thermo_utils, only: isSingleComp
    use saturation, only: safe_bubT
    use tp_solver, only: twoPhaseTPflash
    real, intent(in) :: P !< Pressure (Pa)
    real, intent(in) :: Tmin, Tmax !< Temperature limits (K)
    real, intent(in) :: z(nc) !< Composition (-)
    integer, intent(in) :: n !< Number of points
    real, intent(out) :: ta(n) !< Temperature (K)
    real, intent(out) :: va(n) !< Spcecific volume (m3/mol)
    real, intent(out) :: sa(n) !< Specific entropy (J/mol/K)
    real, intent(out) :: ha(n) !< Specifc enthalpy (J/mol)
    ! Locals
    integer :: i, i_pure, phase(n), n_liq
    real :: Tbub, Y(nc), X(nc), beta, betaL
    real :: vg, vl, sg, sl, hg, hl
    real :: tci,pci,oi,vci

    if (isSingleComp(z)) then
      i_pure = maxloc(z,dim=1)
      call getCriticalParam(i_pure,tci,pci,oi,vci)
      if (P >= pci) then
        phase = LIQPH
        call linspace(Tmin,Tmax,n,ta)
      else
        Tbub = safe_bubT(P,Z,Y)
        if (Tbub <= Tmin) then
          phase = VAPPH
          call linspace(Tmin,Tmax,n,ta)
        else if (Tbub >= Tmax) then
          phase = LIQPH
          call linspace(Tmin,Tmax,n,ta)
        else
          n_liq = ceiling(n*(Tbub - Tmin)/(Tmax-Tmin))
          if (n_liq > nint(n*0.5)) n_liq = n_liq - 1
          phase(1:n_liq) = LIQPH
          call linspace(Tmin,Tbub,n_liq,ta(1:n_liq))
          phase(n_liq+1:n) = VAPPH
          call linspace(Tmin,Tbub,n-n_liq,ta(n_liq+1:n))
        endif
        do i=1,n
          call specificvolume(ta(i),P,z,phase(i),va(i))
          call enthalpyTV(ta(i),va(i),z,ha(i))
          call entropyTV(ta(i),va(i),z,sa(i))
        enddo
      endif
    else
      call linspace(Tmin,Tmax,n,ta)
      do i=1,n
        call twoPhaseTPflash(ta(i),P,Z,beta,betaL,phase(i),X,Y)
        if (phase(i) == SINGLEPH) phase(i) = LIQPH
        if (phase(i) == TWOPH) then
          call specificvolume(ta(i),P,y,VAPPH,vg)
          call enthalpyTV(ta(i),vg,y,hg)
          call entropyTV(ta(i),vg,y,sg)
          call specificvolume(ta(i),P,x,LIQPH,vl)
          call enthalpyTV(ta(i),vl,x,hl)
          call entropyTV(ta(i),vl,x,sl)
          va(i) = beta*vg + betaL*vl
          ha(i) = beta*hg + betaL*hl
          sa(i) = beta*sg + betaL*sl
        else
          call specificvolume(ta(i),P,z,phase(i),va(i))
          call enthalpyTV(ta(i),va(i),z,ha(i))
          call entropyTV(ta(i),va(i),z,sa(i))
        endif
      enddo
    endif
  end subroutine isobar

  !-----------------------------------------------------------------------------
  !> Map isoenthalp
  !>
  !> \author Morten Hammer, 2020-06
  !-----------------------------------------------------------------------------
  subroutine isenthalp(h,Pmin,Pmax,Tmin,Tmax,z,n,pa,va,sa,ta,na)
    use tpconst, only: tpTmin, tpTmax
    use parameters, only: nc, TWOPH, SINGLEPH, VAPPH, LIQPH
    use eosTV, only: entropyTV
    use eos, only: specificvolume
    use ph_solver, only: twoPhasePHflash
    real, intent(in) :: h !< Specific enthalpy (J/mol)
    real, intent(in) :: Pmin, Pmax !< Pressure limits (Pa)
    real, intent(in) :: Tmin, Tmax !< Temperature limits (K)
    real, intent(in) :: z(nc) !< Composition (-)
    integer, intent(in) :: n !< Number of points
    real, intent(out) :: pa(n) !< Temperature (Pa)
    real, intent(out) :: va(n) !< Spcecific volume (m3/mol)
    real, intent(out) :: sa(n) !< Specific entropy (J/mol/K)
    real, intent(out) :: ta(n) !< Temperature (K)
    integer, intent(out) :: na !< Number of points
    ! Locals
    integer :: i, phase, ierr(n)
    real :: Y(nc), X(nc), beta, betaL
    real :: vg, vl, sg, sl
    real :: tmax_old, tmin_old
    tmax_old = tpTmax
    tmin_old = tpTmin
    tpTmax = tmax
    tpTmin = tmin

    call linspace(Pmin,Pmax,n,pa)
    na = n
    ta(1) = 0.5*(tmax + tmin)
    do i=1,n
      if (i > 1) ta(i) = ta(i-1)
      call twoPhasePHflash(ta(i),pa(i),Z,beta,betaL,X,Y,h,phase,ierr(i))
      if (ierr(i) == 0 .or. ierr(i) == -1) then
        ierr(i) = 0
        if (phase == SINGLEPH) phase = LIQPH
        if (phase == TWOPH) then
          call specificvolume(ta(i),pa(i),y,VAPPH,vg)
          call entropyTV(ta(i),vg,y,sg)
          call specificvolume(ta(i),pa(i),x,LIQPH,vl)
          call entropyTV(ta(i),vl,x,sl)
          va(i) = beta*vg + betaL*vl
          sa(i) = beta*sg + betaL*sl
        else
          call specificvolume(ta(i),pa(i),z,phase,va(i))
          call entropyTV(ta(i),va(i),z,sa(i))
        endif
      else
        ierr(i) = 1
      endif
    enddo
    if (sum(ierr) > 0) then
      na = 0
      do i=1,n
        if (ierr(i) == 0) then
          na = na + 1
          ta(na) = ta(i)
          pa(na) = pa(i)
          va(na) = va(i)
          sa(na) = sa(i)
        endif
      enddo
    endif
    tpTmax = tmax_old
    tpTmin = tmin_old

  end subroutine isenthalp

  !-----------------------------------------------------------------------------
  !> Map isoentrope
  !>
  !> \author Morten Hammer, 2020-06
  !-----------------------------------------------------------------------------
  subroutine isentrope(s,Pmin,Pmax,Tmin,Tmax,z,n,pa,va,ha,ta,na)
    use tpconst, only: tpTmin, tpTmax
    use parameters, only: nc, TWOPH, SINGLEPH, VAPPH, LIQPH
    use eosTV, only: enthalpyTV
    use eos, only: specificvolume
    use ps_solver, only: twoPhasePSflash
    real, intent(in) :: s !< Specific entropy (J/mol/K)
    real, intent(in) :: Pmin, Pmax !< Pressure limits (Pa)
    real, intent(in) :: Tmin, Tmax !< Temperature limits (K)
    real, intent(in) :: z(nc) !< Composition (-)
    integer, intent(in) :: n !< Number of points
    real, intent(out) :: pa(n) !< Pressure (Pa)
    real, intent(out) :: va(n) !< Spcecific volume (m3/mol)
    real, intent(out) :: ha(n) !< Specific enthalpy (J/mol)
    real, intent(out) :: ta(n) !< Temperature (K)
    integer, intent(out) :: na !< Number of points
    ! Locals
    integer :: i, phase, ierr(n)
    real :: Y(nc), X(nc), beta, betaL
    real :: vg, vl, hg, hl
    real :: tmax_old, tmin_old
    tmax_old = tpTmax
    tmin_old = tpTmin
    tpTmax = tmax
    tpTmin = tmin

    call linspace(Pmin,Pmax,n,pa)
    na = n
    ta(1) = 0.5*(tmax + tmin)
    do i=1,n
      if (i > 1) ta(i) = ta(i-1)
      call twoPhasePSflash(ta(i),pa(i),Z,beta,betaL,X,Y,s,phase,ierr(i))
      if (ierr(i) == 0 .or. ierr(i) == -1) then
        ierr(i) = 0
        if (phase == SINGLEPH) phase = LIQPH
        if (phase == TWOPH) then
          call specificvolume(ta(i),pa(i),y,VAPPH,vg)
          call enthalpyTV(ta(i),vg,y,hg)
          call specificvolume(ta(i),pa(i),x,LIQPH,vl)
          call enthalpyTV(ta(i),vl,x,hl)
          va(i) = beta*vg + betaL*vl
          ha(i) = beta*hg + betaL*hl
        else
          call specificvolume(ta(i),pa(i),z,phase,va(i))
          call enthalpyTV(ta(i),va(i),z,ha(i))
        endif
      else
        ierr(i) = 1
      endif
    enddo
    if (sum(ierr) > 0) then
      na = 0
      do i=1,n
        if (ierr(i) == 0) then
          na = na + 1
          ta(na) = ta(i)
          pa(na) = pa(i)
          va(na) = va(i)
          ha(na) = ha(i)
        endif
      enddo
    endif
    tpTmax = tmax_old
    tpTmin = tmin_old

  end subroutine isentrope

  subroutine linspace(xmin,xmax,n,x)
    real, intent(in) :: xmin, xmax !< Limits
    integer, intent(in) :: n !< Number of points
    real, intent(out) :: x(n) !< Equidistant points
    ! Locals
    integer :: i
    real :: dx
    dx = (xmax-xmin)/(n-1)
    do i=1,n
      x(i) = xmin + dx*(i-1)
    enddo
  end subroutine linspace

end module isolines
