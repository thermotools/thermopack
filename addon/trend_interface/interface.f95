#ifdef PGF90COMP
#define cdim ncomp
#else
#define cdim :
#endif

module intface
  implicit none
  public
  logical, parameter :: useConstantRmix = .true.
end module intface

integer function TestPrecision()
! AA: 2013-04-17:  Check double Presision
! To secure that the library is compiled with the same length of double presision
  double precision:: x
  double precision :: Eps
  integer :: ilog

  x = 1.0
  eps = epsilon(x)
  ilog = - int(log(eps) / log(2.0D0))
  TestPrecision = ilog
end function TestPrecision

subroutine trend_init_no_char(nc,int_path,npath,int_comps,ncomps,mix,Rgas)
  ! MH, 2014-02
  !----------------------------------------------------------------------
  implicit none
  ! Input:
  integer, intent(in) :: nc
  integer, intent(in) :: mix
  integer, intent(in) :: npath
  integer, intent(in) :: ncomps
  integer, intent(in) :: int_path(npath)
  integer, intent(in) :: int_comps(ncomps)
  real,    intent(in) :: Rgas
  ! Internal
  character (12)       :: comps(nc)
  character(len=npath) :: path
  character(len=ncomps) :: char_comps
  character(len=255) :: char_path
  integer :: i
  call asciitochar(int_path,path,npath)
  if (npath > 255) then
    print *,"trend_init_no_char: path string is too long"
    call exit(1)
  endif
  char_path = trim(path)
  call asciitochar(int_comps,char_comps,ncomps)
  if (nc*12 == ncomps) then
    do i=1,nc
      comps(i) = char_comps((i-1)*12+1:i*12)
    enddo
  else
    print *,'Error in input to trend_init_no_char: ncomps not equal to 12*nc'
    call exit(1)
  endif

  call trend_init(nc,char_path,comps,mix,Rgas)
  contains
    !> Convert an character array to an ASCII integer array.
    !> Intended for communication between C/CPP code and Fortran.
    !> \author M. Hammer January 2012
    subroutine asciitochar(inttxt,chartxt,n)
      implicit none
      integer, intent(in) :: n !< dimension of integer and character array
      integer, dimension(n), intent(in) :: inttxt !< integer array
      character(len=n), intent(out) :: chartxt !< character array
      ! locals
      integer :: i

      do i=1,n
        chartxt(i:i) = char(inttxt(i))
      enddo
    end subroutine asciitochar
end subroutine trend_init_no_char

subroutine trend_init(nc,path,comps,mix,Rgas)
  ! MH, 2013-04-04, EA, 2014-02
  !----------------------------------------------------------------------
  use module_fluid_parameters, only: Eq_type, Mix_type, components, &
       ncomp, molfractions, factor, factorpress, factortrans, &
       factorrbwr, factor_VS5eta, factor_VS5slj
  use trend_constants, only: mnc
  use module_fluid_parameters, only: Rgas_External
  implicit none
  ! Input:
  integer, intent(in)               :: nc
  character (12), intent(in)        :: comps(nc)
  character(len=255), intent(inout) :: path
  integer, intent(in)               :: mix
  real,    intent(in)               :: Rgas
  ! Internal
  integer                           :: i
  integer                           :: errorflag

  Rgas_External = Rgas
  errorflag = 0
  ! Resetting module variables
  call initialize()

  ! Set factor paramaters
  if (trim(comps(1)) == 'LJF') then
    Factor = 1.D0
    factorpress=1.D6
    factortrans=1.D0
    factorrbwr=1.D0
    factor_VS5eta=1.D0
    factor_VS5slj=1.D0
  else
    factor = 1.d3
    factorpress=1.D0
    factortrans=1.D6
    factorrbwr=1.D2
    factor_VS5eta=1.D5
    factor_VS5slj=1.D1
  endif
  ! Setting global TREND flags for EoS and Mixing rules.
  Eq_type = 1
  Mix_type = mix

  ! set given fluids
  ncomp = nc
  components = ''
  components(1:nc) = comps
  molfractions = 0.0
  molfractions(1:nc) = 1.0/real(nc)
  !Reading parameter files
  call read_from_fluidfiles(path, errorflag)

  if (errorflag /= 0) then
    write(*,*) "************************"
    write(*,*) "trend_init: errorflag = ",errorflag
    select case(errorflag)
      case (-7878)
        write(*,*) "Failed to find a .FLD or .PPF file."
      case (-7879)
        write(*,*) "Failed to open a .FLD file."
      case (-7881)
        write(*,*) "Failed to open a .MIX file."
      case (-9953)
        write(*,*) "number of eqtypes does not match given number of fluids."
      case default
        write(*,*) "Unknown error flag. Check the TREND manual."
    end select
    write(*,*) "Check that:"
    write(*,*) "  -The 'trend' directory is available at the point of execution."
    write(*,*) "  -Current path: ",trim(path)
    write(*,*) "  -All components are valid component names in the chosen EoS."
    write(*,*) "    (Check file names in "//trim(path)//")"
    write(*,*) "    Looked for components: "
    do i=1,nc
      write(*,*) "      ",trim(comps(i))
    end do
    call exit(1)
  endif
  call reduced_parameters_calc(300.D0) !Dummy temperature 300 K for the SRK

  call ref_state(errorflag)   !reference point needed

end subroutine trend_init

  !----------------------------------------------------------------------
  !> Calculate fugasity coefficient and differentials given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_thermo(t,p,z,phase,lnfug,lnfugt,lnfugp,nlnfugn)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    integer, intent(in) :: phase !< Phase identifyer
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(cdim), intent(in) :: z !< Compozition
    real, dimension(cdim), intent(out) :: lnfug !< Logarithm of fugasity coefficient
    real, optional, dimension(cdim), intent(out) :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
    real, optional, dimension(cdim), intent(out) :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
    real, optional, dimension(cdim,cdim), intent(out) :: nlnfugn !< Logarithm of fugasity coefficient differential wrpt. mole numbers, times n.
    ! Locals
    real :: p_MPa, rho, Rscale, Rmix
    integer :: i, j
    real, dimension(mnc) :: x_trend
    real, dimension(mnc):: dlnfiP, dlnfiT
    real, dimension(mnc, mnc):: ndlnphidnj_arg
    real :: rhomax, rhomin, rhomax_allowed, rhomin_allowed, rho_est
    real, external :: rhomix_iter, PSRK
    !
    !--------------------------------------------------------------------
    x_trend = 0.0
    x_trend(1:ncomp) = z(1:ncomp)/sum(z(1:ncomp))

    molfractions = x_trend
    call reduced_parameters_calc(T)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rmix/Rgas_External
    else
      Rscale = 1.0
    endif

    p_MPa = Rscale*p*1.0e-6
    ! calculate the phase density
    rho_est = PSRK(T, p_MPa, phase, 0)
    rhomax = 1.10D0*rho_est
    rhomin = 0.90D0*rho_est
    rhomax_allowed = 1.5D0*rho_est
    rhomin_allowed = 0.7D0*rho_est
    ! call the iteration routine for rhomix(T,P)
    rho = rhomix_iter(T, p_MPa, rhomin, rhomax, rhomin_allowed, rhomax_allowed,0)
    if (rho <= 0.0) then
      print *,'Error solving P=P(T,rho) for TREND'
      call exit(1)
    endif
    ! get the fugacities
    !call lnf_mix(T, rho, p_MPa, lnfi)

    !Errorhandling: Calculation of the fugacities failed
    !if (lnfi(1) == 0.D0) then
    !  print *,'Error calculating fugacities in TREND'
    !  print *, "T,P,rho:", T,P,rho
    !  print *, "z:", z
    !  call exit(1)
    !end if

    ! OBS: lnf_mix returned ln(f[MPa]), NOT phi (fug.coeffs).
    !lnfug = lnfi(1:ncomp) - log(x_trend(1:ncomp)*p_MPa)

    call fugco_calc_mix(T, rho, lnfug)
    lnfug = log(lnfug)

    if (present(lnfugt)) then
      call dlnphii_dT(T, rho, dlnfiT)
      lnfugt = dlnfiT(1:ncomp)
    endif
    if (present(lnfugp)) then
      call dlnphii_dP(T, rho, dlnfiP)
      lnfugp = dlnfiP(1:ncomp)*Rscale
    endif
    if (present(nlnfugn)) then
      call ndlnphii_dnj(T, rho, ndlnphidnj_arg)
      do i=1,ncomp
        do j=1,ncomp
          nlnfugn(i,j) = ndlnphidnj_arg(i,j)
        enddo
      enddo
    endif

  end subroutine trend_thermo
  
  !----------------------------------------------------------------------
  !> Calculate single phase specific enthalpy given composition, temperature and density
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_enthalpy(t,rho,x,h,dhdt,dhdp,dhdx)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: rho !< mol/m3 - Density
    real, intent(in) :: t !< K - Temperature
    real, dimension(cdim), intent(in) :: x !< Compozition
    real, intent(out) :: h !< J/mol - Specific enthalpy
    real, optional, intent(out) :: dhdt !< J/mol/K - Specific enthalpy differential wrpt. temperature
    real, optional, intent(out) :: dhdp !< J/mol/K/Pa - Specific enthalpy differential wrpt. pressure
    real, optional, dimension(cdim), intent(out) :: dhdx !< J/mol - Specific enthalpy differential wrpt. mole numbers
    ! Locals
    real, dimension(mnc) :: x_trend
    real, external :: h_calc, dh_dT_px, dh_dP_Tx
    real :: Rscale,Rmix
    !
    !--------------------------------------------------------------------
    x_trend = 0.0
    x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))

    molfractions = x_trend
    call reduced_parameters_calc(T)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
    else
      Rscale = 1.0
    endif

    h = Rscale*h_calc(T, rho, 0)
    if (present(dhdt)) then
      dhdT = Rscale*dh_dT_px(T, rho)
    endif
    if (present(dhdp)) then
      dhdP = dh_dP_Tx(T, rho)
    endif
    if (present(dhdx)) then
      write(*,*) "TREND:trend_enthalpy: dhdx not yet implemented!"
      call exit(1)
    endif

  end subroutine trend_enthalpy

  !----------------------------------------------------------------------
  !> Calculate fugasity coefficient and differentials given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_thermo_dens(t,rho,z,lnfug,lnfugt,lnfugp,nlnfugn)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: rho !< mol/m3 - Density
    real, intent(in) :: t !< K - Temperature
    real, dimension(cdim), intent(in) :: z !< Compozition
    real, dimension(cdim), intent(out) :: lnfug !< Logarithm of fugasity coefficient
    real, optional, dimension(cdim), intent(out) :: lnfugt !< 1/K - Logarithm of fugasity coefficient differential wrpt. temperature
    real, optional, dimension(cdim), intent(out) :: lnfugp !< 1/Pa - Logarithm of fugasity coefficient differential wrpt. pressure
    real, optional, dimension(cdim,cdim), intent(out) :: nlnfugn !< Logarithm of fugasity coefficient differential wrpt. mole numbers, times n.
    ! Locals
    integer :: i, j
    real, dimension(mnc) :: x_trend
    real, dimension(mnc):: dlnfiP, dlnfiT
    real, dimension(mnc, mnc):: ndlnphidnj_arg
    real :: Rmix, Rscale
    !
    !--------------------------------------------------------------------

    x_trend = 0.0
    x_trend(1:ncomp) = z(1:ncomp)/sum(z(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)

    call fugco_calc_mix(T, rho, lnfug)
    lnfug = log(lnfug)

    if (present(lnfugt)) then
      call dlnphii_dT(T, rho, dlnfiT)
      lnfugt = dlnfiT(1:ncomp)
    endif
    if (present(lnfugp)) then
      if (useConstantRmix) then
        call R_mix_calc(Rmix)
        Rscale = Rmix/Rgas_External
      else
        Rscale = 1.0
      endif
      call dlnphii_dP(T, rho, dlnfiP)
      lnfugp = dlnfiP(1:ncomp)*Rscale
    endif
    if (present(nlnfugn)) then
      call ndlnphii_dnj(T, rho, ndlnphidnj_arg)
      do i=1,ncomp
        do j=1,ncomp
          nlnfugn(i,j) = ndlnphidnj_arg(i,j)
        enddo
      enddo
    endif

  end subroutine trend_thermo_dens

  !----------------------------------------------------------------------
  !> Calculate single phase specific entropy given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_entropy(t,rho,n,s,dsdt,dsdp,dsdn)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: rho !< mol/m3 - Density
    real, intent(in) :: t !< K - Temperature
    real, dimension(cdim), intent(in) :: n !< Mol numbers
    real, intent(out) :: s !< J/mol/K - Specific entropy
    real, optional, intent(out) :: dsdt !< J/mol/K2 - Specific entropy differential wrpt. temperature
    real, optional, intent(out) :: dsdp !< J/mol/K2/Pa - Specific entropy differential wrpt. pressure
    real, optional, dimension(cdim), intent(out) :: dsdn !< J/mol 2/K - Specific entropy differential wrpt. mole numbers
    ! Locals
    real :: Rscale, Rmix, sumn, P, V
    real :: dPdn(mnc),dVdn(mnc),dPdV,dPdT,dVdT,dSdV
    real :: F,F_T,F_V,F_TT,F_TV,F_VV
    real, dimension(mnc) :: F_n,F_Tn,F_Vn
    real :: Fid,Fid_T,Fid_V,Fid_TT,Fid_TV,Fid_VV
    real, dimension(mnc) :: Fid_n,Fid_Tn,Fid_Vn
    real, external :: s_calc
    interface
      subroutine trend_CalcFres(T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_TV,F_VV,F_Tn,F_Vn,F_nn)
        use module_fluid_parameters, only: ncomp
        implicit none
        real, intent(in) :: T,V,n(cdim)
        real, optional, intent(out) :: F,F_T,F_V,F_n(cdim),F_TT,F_TV,F_VV
        real, optional, intent(out) :: F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
      end subroutine trend_CalcFres
    end interface
    interface
      subroutine trend_CalcFid(T,V,n,F,F_T,F_V,F_TT,F_TV,F_VV,F_n,F_Tn,F_Vn,F_nn)
        use module_fluid_parameters, only: ncomp
        real, intent(in) :: T,V,n(cdim)
        real, optional, intent(out) :: F,F_T,F_V,F_TT,F_TV,F_VV
        real, optional, intent(out) :: F_n(cdim),F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
      end subroutine trend_CalcFid
    end interface
    !
    !--------------------------------------------------------------------
    sumn = sum(n)
    V = sumn/rho
    !
    if (useConstantRmix) then
      Rmix = Rgas_External
    else
      call R_mix_calc(Rmix) ! calculates the "mixture molar gas constant"
    endif

    if (present(dSdt) .or. present(dSdp) .or. present(dSdn)) then
      call trend_CalcFres(T,V,n,F=F,F_T=F_T,F_V=F_V,F_n=F_n,F_TT=F_TT,&
           F_TV=F_TV,F_VV=F_VV,F_Tn=F_Tn,F_Vn=F_Vn)
      call trend_CalcFid(T,V,n,F=Fid,F_T=Fid_T,F_V=Fid_V,F_TT=Fid_TT,&
           F_TV=Fid_TV,F_VV=Fid_VV,F_n=Fid_n,F_Tn=Fid_Tn,F_Vn=Fid_Vn)
      p = -Rmix*T*(F_V - sumn/V)
      dPdV = -Rmix*T*(F_VV + sumn/V**2)
      dPdT = P/T-Rmix*T*F_TV
      dVdT = -dPdT/dPdV
      dSdV = - Rmix*(F_V + T*F_TV + Fid_V + T*Fid_TV)
    else
      call trend_CalcFres(T,V,n,F=F,F_T=F_T,F_V=F_V)
      call trend_CalcFid(T,V,n,F=Fid,F_T=Fid_T)
      p = -Rmix*T*(F_V - sumn/V)
    end if

    S = - Rmix*(F + T*F_T + Fid + T*Fid_T)
    if (present(dSdt)) then
      dSdt = dSdV*dVdT - Rmix*(2*F_T + T*F_TT + 2*Fid_T + T*Fid_TT)
    endif
    if (present(dSdp)) then
      dSdp = dSdV/dPdV
    end if
    if (present(dSdn)) then
      dPdn(1:ncomp) = Rmix*T*(-F_Vn(1:ncomp) + 1/V)
      dVdn(1:ncomp) = -dPdn(1:ncomp)/dPdV
      dSdn(1:ncomp) = dVdn(1:ncomp)*dPdt - Rmix*(F_n(1:ncomp) + T*F_Tn(1:ncomp) &
           + Fid_n(1:ncomp) + T*Fid_Tn(1:ncomp))
    endif
  end subroutine trend_entropy

  !----------------------------------------------------------------------
  !> Calculate pressure given composition, temperature and density.
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  function trend_pressure(n,t,v,dpdv,dpdt,dpdn) result(p)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(cdim), intent(in) :: n !< Mol numbers
    real, optional, intent(out) :: dpdt !< Pa/K - Pressure differential wrpt. temperature
    real, optional, intent(out) :: dpdv !< Pa/m3 - Pressure differential wrpt. volume
    real, optional, intent(out) :: dpdn(cdim) !< Pa/mol - Pressure differential wrpt. mol numbers
    real :: p !< Pa - Pressure
    ! Locals
    real :: Rscale, Rmix, sumn, F_V, F_TV, F_VV, F_Vn(mnc)
    real, dimension(mnc) :: x_trend
    real, external :: p_calc
    interface
      subroutine trend_CalcFres(T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_TV,F_VV,F_Tn,F_Vn,F_nn)
        use module_fluid_parameters, only: ncomp
        implicit none
        real, intent(in) :: T,V,n(cdim)
        real, optional, intent(out) :: F,F_T,F_V,F_n(cdim),F_TT,F_TV,F_VV
        real, optional, intent(out) :: F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
      end subroutine trend_CalcFres
    end interface
    interface
      subroutine trend_CalcFid(T,V,n,F,F_T,F_V,F_TT,F_TV,F_VV,F_n,F_Tn,F_Vn,F_nn)
        use module_fluid_parameters, only: ncomp
        real, intent(in) :: T,V,n(cdim)
        real, optional, intent(out) :: F,F_T,F_V,F_TT,F_TV,F_VV
        real, optional, intent(out) :: F_n(cdim),F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
      end subroutine trend_CalcFid
    end interface
    !--------------------------------------------------------------------
    sumn = sum(n(1:ncomp))
    x_trend = 0.0
    x_trend(1:ncomp) = n(1:ncomp)/sumn
    molfractions = x_trend
    call reduced_parameters_calc(T)
    call R_mix_calc(Rmix)
    if (useConstantRmix) then
      Rscale = Rgas_External/Rmix
      Rmix = Rgas_External
    else
      Rscale = 1.0
    endif
    call trend_CalcFres(T,V,n,F_V=F_V,F_TV=dpdt,F_VV=dpdv,F_Vn=dpdn)
    p = F_V
    if (present(dpdt)) then
      F_TV = dpdt
    endif
    if (present(dpdv)) then
      F_VV = dpdv
    endif
    if (present(dpdn)) then
      F_Vn(1:ncomp) = dpdn
    endif
    call trend_CalcFid(T,V,n,F_V=F_V,F_TV=dpdt,F_VV=dpdv,F_Vn=dpdn)
    F_V = F_V + p
    p = - Rmix*T*F_V
    if (present(dpdt)) then
      F_TV = F_TV + dpdt
      dpdt = -Rmix*(F_V + T*F_TV)
    endif
    if (present(dpdv)) then
      F_VV = F_VV + dpdv
      dpdv = - Rmix*T*F_VV
    endif
    if (present(dpdn)) then
      F_Vn(1:ncomp) = F_Vn(1:ncomp) + dpdn
      dpdn = - Rmix*T*F_Vn(1:ncomp)
    endif
  end function trend_pressure

  !----------------------------------------------------------------------
  !> Calculate pressure differential vrpt. density given composition, temperature and density.
  !>
  !> \author MH, 2013-04-18
  !----------------------------------------------------------------------
  function trend_dpdrho(x,t,rho) result(dpdrho)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: rho !< mol/m3 - Density
    real, dimension(cdim), intent(in) :: x !< Composition
    real :: dpdrho
    ! Locals
    real :: Rscale, Rmix
    !--------------------------------------------------------------------
    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    call reduced_parameters_calc(T)
    call dP_drho (T, rho, dPdrho)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
      dPdrho = dPdrho*Rscale
    endif
  end function trend_dpdrho

  !----------------------------------------------------------------------
  !> Calculate the second derivative of pressure vrpt. density given composition, temperature and density.
  !>
  !> \author EA, 2013-08-08
  !----------------------------------------------------------------------
  function trend_d2Pdrho2(x,T,rho) result(d2Pdrho2)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in)                :: T !< K - Temperature
    real, intent(in)                :: rho !< mol/m3 - Density
    real, dimension(cdim), intent(in)  :: x !< Composition
    real :: d2Pdrho2
    ! Routines:
    real, external                  :: d2P_drho2
    ! Locals
    real :: Rscale, Rmix
    !--------------------------------------------------------------------
    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    call reduced_parameters_calc(T)
    d2Pdrho2 = d2P_drho2(T, rho)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
      d2Pdrho2 = d2Pdrho2*Rscale
    endif
  end function trend_d2Pdrho2

  !----------------------------------------------------------------------
  !> Calculate SRK density given composition, temperature and pressure.
  !> To be used as initial guess
  !>
  !> \author MH, 2013-04-11
  !----------------------------------------------------------------------
  function trend_psrk(t,p,x,phase) result(rho)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: p !< Pa - Pressure
    real, dimension(cdim), intent(in) :: x !< Compozition
    integer, intent(in) :: phase !< Phase indicator
    real :: rho !< mol/m3 - Pressure
    ! Locals
    real :: p_MPa, Rscale, Rmix
    real, dimension(mnc) :: x_trend
    real, external :: psrk
    !--------------------------------------------------------------------
    p_MPa = p*1.0e-6

    x_trend = 0.0
    x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))

    molfractions = x_trend
    call reduced_parameters_calc(T)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
      p_MPa = p_MPa*Rscale
    endif

    rho = PSRK(T, p_MPa, phase, 0)
  end function trend_psrk

  !-----------------------------------------------------------------------------
  !> Calculate the maximum molar density according to the SRK EoS and the
  !> classic mixing rule
  !>
  !> \author MAG, 2013-08-19
  !-----------------------------------------------------------------------------
  subroutine trend_rhomax_srk(x,rhomax)
    use module_fluid_parameters, only: tc,pc,req,ncomp
    implicit none
    !---------------------------------------------------------------------------
    real, dimension(cdim), intent(in)  :: x      !< Composition
    real,                   intent(out) :: rhomax !< Maximum molar density
    !---------------------------------------------------------------------------
    ! Locals
    integer         :: ic
    real            :: b
    real, parameter :: c1=0.08664e-6
    !---------------------------------------------------------------------------
    b=0.0
    do ic=1,ncomp
      b=b+x(ic)*c1*req(ic)*tc(ic)/pc(ic)
    end do
    rhomax=1.0/b
    !
  end subroutine trend_rhomax_srk

  !----------------------------------------------------------------------
  !> Calculate single phase specific volume given composition, temperature and pressure
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  subroutine trend_specificVolume(t,x,v,dvdt,dvdp,dvdx)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, dimension(cdim), intent(in) :: x !< Compozition
    real, intent(in) :: v !< mol/m3 - Specific volume
    real, optional, intent(out) :: dvdt !< m3/mol/K - Specific volume differential wrpt. temperature
    real, optional, intent(out) :: dvdp !< m3/mol/Pa - Specific volume differential wrpt. pressure
    real, optional, dimension(cdim), intent(out) :: dvdx !< m3/mol - Specific volume differential wrpt. mole numbers
    ! Locals
    real, dimension(mnc) :: x_trend, dPdni
    real :: rho, dPdrho, dPdT, drhodT, dPdV, Rscale, Rmix
    !
    !--------------------------------------------------------------------
    x_trend = 0.0
    x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
    endif

    rho = 1.0/v
    if (present(dvdp)) then
      call dP_drho(T, rho, dPdrho)
      dPdrho = Rscale*dPdrho
      dvdP = -1.0/( dPdrho*rho**2)
    endif
    if (present(dvdT)) then
      call dP_drho(T, rho, dPdrho)
      call dP_dT(T, rho, dPdT)
      drhodT = -dPdT/dPdrho
      dvdT = -drhodT/rho**2
    endif
    if (present(dvdx)) then
      ! get the derivative n*d(p)/d(ni) at const. T, V
      call ndP_dni_TV(T, rho, dPdni)
      ! get the derivative n*d(p)/d(V) at const. T, x
      call ndP_dV(T, rho, dPdV)
      dvdx = -dpdni(1:ncomp)/dPdV
    endif
  end subroutine trend_specificVolume

  !----------------------------------------------------------------------
  !> Get component mole weight.
  !> Unit: g/mol
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  function trend_compMoleWeight(j) result(mw)
    use module_fluid_parameters, only: wm
    implicit none
    ! Transferred variables
    integer, intent(in) :: j !< Component index
    real :: mw !< g/mol - Mole weight
    !--------------------------------------------------------------------
    !
    mw = wm(j)*1.0e3
  end function trend_compMoleWeight

  !----------------------------------------------------------------------
  !> Get mole weight.
  !> Unit: g/mol
  !>
  !> \author MH, 2013-04-08
  !----------------------------------------------------------------------
  function trend_moleWeight(z) result(mw)
    use module_fluid_parameters, only: molfractions, ncomp
    implicit none
    ! Transferred variables
    real, dimension(cdim), intent(in) :: z !< Composition
    real :: mw !< g/mol - Mole weight
    !--------------------------------------------------------------------
    !
    molfractions = 0.0
    molfractions(1:ncomp) = z
    call wm_mix_calc(mw)
    mw = mw * 1.0e3
  end function trend_moleWeight

  !----------------------------------------------------------------------
  !> Get critical parameters
  !>
  !> \author MH, 2013-04-10
  !----------------------------------------------------------------------
  subroutine trend_getcrit(i,tci,pci,oi,vci,tnbi)
    use module_fluid_parameters, only: accen, tc, pc, rhoc, tnbp
    implicit none
    ! Transferred variables
    integer, intent(in) :: i !< Component index
    real, intent(out) :: tci !< K - Critical temperature
    real, intent(out) :: pci !< Pa - Critical pressure
    real, intent(out) :: oi  !< Asentric factor
    real, optional, intent(out) :: vci !< Critical specific volume (m3/mol)
    real, optional, intent(out) :: tnbi !< Normal boiling point
    !--------------------------------------------------------------------
    !
    tci = tc(i)
    pci = pc(i)*1.0e6 ! MPa -> Pa
    oi  = accen(i)
    if (present(vci)) then
       vci = 1.0/rhoc(i) ! mol/m3
    endif
    if (present(tnbi)) then
       tnbi = tnbp(i)
    endif
  end subroutine trend_getcrit

  !----------------------------------------------------------------------
  !> Get the Z-factor (compressibility factor),
  !> given temperature, density and composition.
  !>
  !> \author EA, 2014-04
  !----------------------------------------------------------------------
  subroutine trend_zfac(T, rho, x, zfac, dzdt, dzdp, dzdx)
    use module_fluid_parameters, only: molfractions, ncomp
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(cdim) !< Composition (mol/mol)
    ! Output:
    real,   intent(out)         :: zfac !< Compressibility factor (-)
    real, optional, intent(out) :: dzdt !< zfac diff. wrt. temperature
    real, optional, intent(out) :: dzdp !< zfac diff. wrt. pressure
    real, optional, intent(out) :: dzdx(cdim) !< zfac diff. wrt. composition
    ! TREND functions:
    real, external              :: z_calc
    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    call reduced_parameters_calc(T)

    zfac = z_calc(T, rho, 0)
    if (present(dzdt)) then
      write(*,*) "TREND:trend_zfac: dzdt not yet implemented!"
      call exit(1)
    endif
    if (present(dzdp)) then
      write(*,*) "TREND:trend_zfac: dzdp not yet implemented!"
      call exit(1)
    endif
    if (present(dzdx)) then
      write(*,*) "TREND:trend_zfac: dzdx not yet implemented!"
      call exit(1)
    endif
  end subroutine trend_zfac

  !----------------------------------------------------------------------
  !> Get the residual Gibbs energy,
  !> given temperature, density and composition.
  !>
  !> \author EA, 2014-04
  !----------------------------------------------------------------------
  subroutine trend_residualgibbs_tp(T, p, rho, x, gr, dgrdt, dgrdp)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: p        !< Pressure (Pa)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(cdim)  !< Composition (mol/mol)
    ! Output:
    real,   intent(out)         :: gr     !< Residual Gibbs Energy (J/mol)
    real, optional, intent(out) :: dgrdt  !< Res. Gibbs diff. wrt. temperature
    real, optional, intent(out) :: dgrdp  !< Res. Gibbs diff. wrt. pressure
    ! TREND functions:
    real, external              :: gr_calc
    ! Internals
    real                        :: Rmix, z, Rscale
    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)
    call reduced_parameters_calc(T)
    ! Get residual gibbs in TV variables
    gr = gr_calc(T, rho, 0)
    ! Convert to TP evaluation
    call R_mix_calc(Rmix)                   ! calculates the "mixture molar gas constant"
    if (useConstantRmix) then
      Rscale = Rgas_External/Rmix
      Rmix = Rgas_External
    else
      Rscale = 1.0
    endif
    z = P/(Rmix*T*rho)
    gr = Rscale*gr - Rmix*T*log(z)
    if (present(dgrdt)) then
      write(*,*) "TREND:trend_residualgibbs: dgrdt not yet implemented!"
      call exit(1)
    endif
    if (present(dgrdp)) then
      write(*,*) "TREND:trend_residualgibbs: dgrdp not yet implemented!"
      call exit(1)
    endif
  end subroutine trend_residualgibbs_tp

  !----------------------------------------------------------------------
  !> Get the residual enthalpy,
  !> given temperature, density and composition.
  !>
  !> \author EA, 2014-04
  !----------------------------------------------------------------------
  subroutine trend_residualenthalpy_tp(T, p, rho, x, hr, dhrdt, dhrdp, dhrdx)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: p        !< Pressure (Pa)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(cdim)  !< Composition (mol/mol)
    ! Output:
    real,   intent(out)         :: hr     !< Residual enthalpy (J/mol)
    real, optional, intent(out) :: dhrdt  !< Res. enth. diff. wrt. temperature
    real, optional, intent(out) :: dhrdp  !< Res. enth. diff. wrt. pressure
    real, optional, intent(out) :: dhrdx(cdim)!< Res. enth. diff. wrt. comp
    ! TREND functions:
    real, external            :: hr_calc
    real :: Rmix, Rscale
    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)
    call reduced_parameters_calc(T)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
    else
      Rscale = 1.0
    endif
    hr = Rscale*hr_calc(T, rho, 0)
    if (present(dhrdt)) then
      write(*,*) "TREND:trend_residualenthapy: dhrdt not yet implemented!"
      call exit(1)
    endif
    if (present(dhrdp)) then
      write(*,*) "TREND:trend_residualenthalpy: dhrdp not yet implemented!"
      call exit(1)
    endif
    if (present(dhrdx)) then
      write(*,*) "TREND:trend_residualenthalpy: dhrdx not yet implemented!"
      call exit(1)
    endif

  end subroutine trend_residualenthalpy_tp

  !----------------------------------------------------------------------
  !> Calculate single phase residual specific entropy given composition,
  ! temperature and pressure
  !>
  !> \author EA, 2014-05
  !----------------------------------------------------------------------
  subroutine trend_residualentropy_tp(T,p,rho,x,sr,dsrdt,dsrdp,dsrdx)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real, intent(in)                  :: T    !< Temperature (K)
    real, intent(in)                  :: p    !< Pressure (Pa)
    real, intent(in)                  :: rho  !< Density (mol/m3)
    real, dimension(cdim), intent(in) :: x    !< Composition (mol/mol)
    ! Output:
    real, intent(out)                 :: sr   !< Specific residual entropy
                                              ! (J/(mol*K)
    real, optional, intent(out)       :: dsrdt!< Specific residual entropy diff.
                                              ! wrpt. temperature (J/(mol*K^2)
    real, optional, intent(out)       :: dsrdp!< Specific residual entropy diff.
                                              ! wrpt. pressure (J/(mol*K*Pa)
    real, optional, intent(out)       :: dsrdx(cdim) !< Specific residual
                                              !entropy diff. wrpt. mole numbers
                                              ! (J/(mol^2*K)
    ! Internal:
    real                              :: x_trend(mnc), Rmix, z, Rscale
    ! TREND functions:
    real, external                    :: sr_calc
    !
    !--------------------------------------------------------------------
    ! Set parameters
    x_trend = 0.0
    x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)
    ! Get residual entropy in TV
    sr = sr_calc(T, rho, 0)
    ! Convert to TP evaluation
    call R_mix_calc(Rmix)                   ! calculates the "mixture molar gas constant"
    if (useConstantRmix) then
      Rscale = Rgas_External/Rmix
      Rmix = Rgas_External
    else
      Rscale = 1.0
    endif
    z = P/(Rmix*T*rho)
    sr = Rscale*sr + Rmix*log(z)
    ! Get differentials
    if (present(dsrdt)) then
      write(*,*) "TREND:trend_residualentropy: dsrdt not yet implemented!"
      call exit(1)
    endif
    if (present(dsrdp)) then
      write(*,*) "TREND:trend_residualentropy: dsrdp not yet implemented!"
      call exit(1)
    endif
    if (present(dsrdx)) then
      write(*,*) "TREND:trend_residualentropy: dsrdx not yet implemented!"
      call exit(1)
    endif

  end subroutine trend_residualentropy_tp


  !----------------------------------------------------------------------
  !> Get the speed of sound of a single phase of composition z
  !> at the given temperature and density.
  !>
  !> \author EA, 2014-05
  !----------------------------------------------------------------------
  function trend_speedofsound(T,rho,x) result(c)
    use module_fluid_parameters, only: molfractions, ncomp
    implicit none
    ! Input:
    real,   intent(in)          :: T        !< Temperature (K)
    real,   intent(in)          :: rho      !< Density (mol/m^3)
    real,   intent(in)          :: x(cdim)  !< Composition (mol/mol)
    ! Output:
    real                        :: c        !< Speed of sound (m/s)
    ! TREND functions:
    real, external              :: ws_calc

    ! Set global state of TREND
    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)
    call reduced_parameters_calc(T)
    ! Get speed of sound
    c = ws_calc(T,rho,0)

  end function trend_speedofsound

  !----------------------------------------------------------------------
  !> Get the component ideal gas heat capacity
  !! at the given temperature and density.
  !!
  !! \author MH, 2014-09
  !----------------------------------------------------------------------
  function trend_ideal_Cp(T,j) result(Cp_id)
    use module_fluid_parameters, only: Req, Rgas_External
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real,    intent(in)         :: T        !< Temperature (K)
    integer, intent(in)         :: j        !< Component index
    ! Output:
    real                        :: Cp_id    !< Ideal heat capacity (J/mol/K)
    ! Locals
    real :: rho !< Density (mol/m3)
    real :: TT_AITT, R
    real :: FIDeriv(nderivs)
    integer :: getderivs(nderivs)

    getderivs = 0
    getderivs(6) = 1
    if (useConstantRmix) then
      R = Rgas_External
    else
      R = Req(j)
    endif
    rho = 100.0 ! Sould not be used in calculation of TT_AITT
    ! Get ideal gas properties
    call fniderivs(t, rho, getderivs, FIDeriv, j)
    TT_AITT = FIDeriv(6)
    Cp_id = (1-TT_AITT)*R
  end function trend_ideal_Cp

  !----------------------------------------------------------------------
  !> Get the component ideal gas enthalpy
  !! at the given temperature and density.
  !!
  !! \author MH, 2014-09
  !----------------------------------------------------------------------
  function trend_ideal_enthalpy(T,j) result(hid)
    use module_fluid_parameters, only: Req, Rgas_External
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real,    intent(in)         :: T        !< Temperature (K)
    integer, intent(in)         :: j        !< Component index
    ! Output:
    real                        :: hid      !< Ideal enthalpy (J/mol)
    ! Locals
    real :: rho !< Density (mol/m3)
    real :: T_AIT, R
    real :: FIDeriv(nderivs)
    integer :: getderivs(nderivs)

    getderivs = 0
    getderivs(5) = 1
    if (useConstantRmix) then
      R = Rgas_External
    else
      R = Req(j)
    endif
    rho = 100.0 ! Should not be used to calculate ideal enthalpy
    ! Get ideal gas properties
    call fniderivs(t, rho, getderivs, FIDeriv, j)
    T_AIT = FIDeriv(5)
    hid = (1.0 + T_AIT) * R * T
  end function trend_ideal_enthalpy

  !----------------------------------------------------------------------
  !> Get the component ideal gas entropy
  !! at the given temperature and density.
  !!
  !! \author MH, 2014-09
  !----------------------------------------------------------------------
  function trend_ideal_entropy(T,P,j) result(sid)
    use module_fluid_parameters, only: Req, Rgas_External
    use module_general_eos_parameters, only: RHORED
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real,    intent(in)         :: T        !< Temperature (K)
    real,    intent(in)         :: P        !< Density (Pa)
    integer, intent(in)         :: j        !< Component index
    ! Output:
    real                        :: sid      !< Ideal entropy (J/mol/K)
    ! Locals
    real :: rho !< Density (mol/m3)
    real :: AI, T_AIT, R
    real :: FIDeriv(nderivs)
    integer :: getderivs(nderivs)

    getderivs = 0
    getderivs(1) = 1
    getderivs(5) = 1
    ! Set density
    if (useConstantRmix) then
      R = Rgas_External
    else
      R = Req(j)
    endif
    rho = RHORED(j) ! Remove contribution from density
    ! Get ideal gas properties
    call fniderivs(t, rho, getderivs, FIDeriv, j)
    AI = FIDeriv(1)
    T_AIT = FIDeriv(5)
    sid = (T_AIT - AI - log(P) + log(rho*R*T)) * R
  end function trend_ideal_entropy

  !------------------------------------------------------------------
  !>Get current binary parameters used in trend
  !! \author AA: 2014-09
  ! See document ?? for descripotion
  !Return ParREdFunc = (/Beta_T, Gamma_T, Beta_V, Gamma_V, F_ij/)
  !and    ParDefFun(i) = (/n, t, d, l, 0, 0, 0, 0/) for i=1..ndf(1)
  !       PArDefFun(i) = (/n, t, d, l, Eta, Epsilon, Beta, Gamma/) for i=ndf(1)+1,...,ndf(2)
  !--------------------------------------------------
  subroutine trend_Get_binary_parameters(i1,i2, ParRedFun, ndf, ParDepFun)
    use module_mixture_parameters
    implicit none
    integer, parameter:: nDim = 25  !Dimensions of array in module_mixture_parameters
    integer(kind=4), intent(in) :: i1,i2
    real, intent(out):: ParRedFun(5)
    integer(kind=4), intent(out) :: ndf(2)
    real, intent(out)::ParDepFun(nDim,8)
    integer i

    ParRedFun = 0.0
    ParDepFun = 0.0

    if (dfpol(i1,i2) + dfexp(i1,i2) > nDim) then
      print *,'Internal error Get_binary_parameters'
      return
    endif

    ParRedFun(1) = rfbetat(i1,i2)
    ParRedFun(2) = rfgammat(i1,i2)
    ParRedFun(3) = rfbetarho(i1,i2)
    ParRedFun(4) = rfgammarho(i1,i2)
    ParRedFun(5) = Fij(i1,i2)

    ndf(1) = dfpol(i1,i2)
    ndf(2) = dfexp(i1,i2)

    do i=1, ndf(1)+ndf(2)
      ParDepFun(i,1) = dfn(i1,i2,i)
      ParDepFun(i,2) = dft(i1,i2,i)
      ParDepFun(i,3) = dfd(i1,i2,i)
      ParDepFun(i,4) = dfl(i1,i2,i)
    enddo
    do i = 1, ndf(2)
      ParDepFun(i+ndf(1),5) = dfeta(i1,i2,i)
      ParDepFun(i+ndf(1),6) = dfeps(i1,i2,i)
      ParDepFun(i+ndf(1),7) = dfbeta(i1,i2,i)
      ParDepFun(i+ndf(1),8) = dfgamma(i1,i2,i)
    enddo
  end subroutine trend_Get_binary_parameters

  !------------------------------------------------------------------
  !>Set current binary parameters used in trend
  !! \author AA: 2014-09
  ! See document ?? for descripotion
  !Set    (/Beta_T, Gamma_T, Beta_V, Gamma_V, F_ij/) = PArRedFunc
  !and    (/n, t, d, l, 0, 0, 0, 0/)=ParDepFun(i) for i=1..ndf(1)
  !       (/n, t, d, l, Eta, Epsilon, Beta, Gamma/)=ParDepFun(i) for i=ndf(1)+1,...,ndf(1)+ndf(2)
  !       but only if ndf(1) or ndf(2) > 0
  ! MArk that
  !--------------------------------------------------
  subroutine trend_Set_binary_parameters(i1i,i2i, ParRedFun, ndf, ParDepFun)
    use module_mixture_parameters
    ! Ir read this parameters by file in read_mix_file
    implicit none
    integer, parameter:: nDim = 25  !Dimensions of array in module_mixture_parameters
    integer, intent(in) :: i1i,i2i
    real, intent(in):: ParRedFun(5)
    integer, intent(in) :: ndf(2)
    real, intent(in)::ParDepFun(nDim,8)
    integer :: i1,i2, k, ipot, i

    if(ndf(1)+ndf(2) > nDim ) then
      print *,'MAx value for ndf(1)+ndf(2) = 25, value is ',ndf(1)+ndf(2)
      return
    endif


    do k=1,2
      if (k==1) then
        i1 = i1i; i2=i2i; ipot = 1
      else
        i1 = i2i; i2=i1i; ipot = -1
      endif
      rfbetat(i1,i2)=  ParRedFun(1)**ipot
      rfgammat(i1,i2)=  ParRedFun(2)
      rfbetarho(i1,i2)=  ParRedFun(3)**ipot
      rfgammarho(i1,i2)=  ParRedFun(4)
      Fij(i1,i2)=  ParRedFun(5)

      if ( ndf(1)>0 .or. ndf(2)>0) then
        dfpol(i1,i2) = ndf(1)
        dfexp(i1,i2) = ndf(2)
        do i=1,nDim
          dfn(i1,i2,i)=0.0
          dft(i1,i2,i)=0.0
          dfd(i1,i2,i)=0.0
          dfl(i1,i2,i)=0.0
          dfeta(i1,i2,i)=0.0
          dfeps(i1,i2,i)=0.0
          dfbeta(i1,i2,i)=0.0
          dfgamma(i1,i2,i)=0.0
        enddo
        do i=1,ndf(1)+ndf(2)
          dfn(i1,i2,i) = ParDepFun(i,1)
          dft(i1,i2,i) =ParDepFun(i,2)
          dfd(i1,i2,i) =ParDepFun(i,3)
          dfl(i1,i2,i) =ParDepFun(i,4)
        enddo
        do i = 1, ndf(2)
          dfeta(i1,i2,i) =ParDepFun(i+ndf(1),5)
          dfeps(i1,i2,i) =ParDepFun(i+ndf(1),6)
          dfbeta(i1,i2,i) =ParDepFun(i+ndf(1),7)
          dfgamma(i1,i2,i) =ParDepFun(i+ndf(1),8)
        enddo
      endif
    enddo
  end subroutine trend_Set_binary_parameters

  !------------------------------------------------------------------
  !>Get reduced density and temperature
  !! \author AA: 2014-09
  !--------------------------------------------------
  subroutine trend_GetReducedRhoT(T, x, RhoRed, TRed)
    use module_general_eos_parameters, only: rhoredmix, tredmix
    use module_fluid_parameters, only: molfractions, ncomp
    implicit none
    real, intent(in) :: T
    real, intent(in) :: x(30)
    real, intent(out):: RhoRed
    real, intent(out)::TRed

    molfractions = 0.0
    molfractions(1:ncomp) = x(1:ncomp)
    call reduced_parameters_calc(T)
    RhoRed = rhoredmix
    TRed = tredmix
  end subroutine trend_GetREducedRhoT

  !-------------------------------------------------------------------------
  !Get Evaporating pressure for single phase
  function trend_GetPVapPure(T, icmp) result(P)
    implicit none
    real, intent(in) :: T
    integer, intent(in):: icmp
    real :: P
    ! Locals
    real :: rhoVap, rhoLiq, PP, TT
    integer :: iter, errval
    rhoVap = 0.0
    rhoLiq = 0.0
    PP = 0.0
    TT = T
    call Flash_Pure_PhaseBoundary(PP, TT, rhoVap, rholiq, 1, errval, iter, icmp)
    P = PP*1E6
  end function trend_GetPVapPure

  !----------------------------------------------------------------------
  !> Calculate Helmholtz free energy given composition,
  !! temperature and specific volume.
  !>
  !> \author MH, 2015-02
  !----------------------------------------------------------------------
  function trend_free_energy(x,t,v,dydv,dydt,d2ydt2,d2ydv2,d2ydvdt) result(y)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3/mol - Specific volume
    real, dimension(cdim), intent(in) :: x !< Compozition
    real, optional, intent(out) :: dydt !< J/(mol K) - Helmholtz differential wrpt. temperature
    real, optional, intent(out) :: dydv !< J/(m3) - Helmholtz differential wrpt. specific volume
    real, optional, intent(out) :: d2ydt2 !< J/(mol K2) - Helmholtz differential wrpt. temperature
    real, optional, intent(out) :: d2ydv2 !< J/m6 - Helmholtz second differential wrpt. specific volume
    real, optional, intent(out) :: d2ydvdt !< J/(m3 K) - Helmholtz second differential wrpt. specific volume and temperature
    !real, dimension(cdim), optional, intent(out) :: dydn !< J/(mol2 m3) - Helmholtz differential wrpt. mole numbers
    !real, dimension(cdim), optional, intent(out) :: d2ydndt !< J/(mol2 K) - Helmholtz second differential wrpt. mole numbers and temperature
    !real, dimension(cdim), optional, intent(out) :: d2ydndv !< J/(mol2 m3) - Helmholtz second differential wrpt. specific volume and mole numbers
    !real, dimension(cdim,cdim), optional, intent(out) :: d2ydn2 !< J/mol3 - Helmholtz second differential wrpt. to mole numbers

    real :: y !< J/mol - Helmholtz free energy
    ! Locals
    real :: D
    real, dimension(mnc) :: x_trend
    integer, dimension(nderivs) :: GETDERR   !Vector tells the subroutine which derivative is neccessary to calculate the property (0 or 1)
    integer, dimension(nderivs) :: GETDERI   !Vector tells the subroutine which derivative is neccessary to calculate the property (0 or 1)
    real, dimension(nderivs) :: FNRDER       !Vector gives the values of the calculated derivatives
    real, dimension(nderivs) :: FNIDER       !Vector gives the values of the calculated derivatives
    real :: Rmix
    real :: AI,D_AID,DD_AIDD,TD_AIDT,T_AIT,TT_AITT
    real :: AR,D_ARD,DD_ARDD,T_ART,TT_ARTT,TD_ARDT
    real :: F,F_V,F_VV,F_T,F_TT,F_TV
    real :: FI,FI_V,FI_VV,FI_T,FI_TT,FI_TV

    !--------------------------------------------------------------------
    x_trend = 0.0
    x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)
    D = 1.0/v

    GETDERR = 0
    GETDERI = 0
    GETDERR(1) = 1
    GETDERI(1) = 1
    if (present(dydv) .or. present(d2ydv2)  .or. present(d2ydvdt)) then
      GETDERR(2) = 1
      GETDERI(2) = 1
    endif
    if (present(dydt) .or. present(d2ydt2)) then
      GETDERR(4) = 1
      GETDERI(5) = 1
    endif
    if (present(d2ydt2)) then
      GETDERR(5) = 1
      GETDERI(6) = 1
    endif
    if (present(d2ydv2)) then
      GETDERR(3) = 1
      GETDERI(3) = 1
    endif
    if (present(d2ydvdt)) then
      GETDERR(6) = 1
      GETDERI(4) = 1
    endif
    if (useConstantRmix) then
      Rmix = Rgas_External
    else
      call R_mix_calc(Rmix)                   ! calculates the "mixture molar gas constant"
    endif
    CALL MIXDERIVSFNR(T,D,GETDERR,FNRDER)   !subroutine calculates derivatives of residual part
    CALL MIXDERIVSFNI(T,D,GETDERI,FNIDER)   !subroutine calculates derivatives of ideal part

    ! Ideal contribution
    AI = FNIDER(1)
    D_AID = FNIDER(2)
    DD_AIDD = FNIDER(3)
    TD_AIDT = FNIDER(4)
    T_AIT = FNIDER(5)
    TT_AITT = FNIDER(6)

    FI = AI
    FI_T = -T_AIT/T
    FI_V = -D*D_AID

    ! Residual contribution
    AR = FNRDER(1)
    D_ARD = FNRDER(2)
    DD_ARDD = FNRDER(3)
    T_ART = FNRDER(4)
    TT_ARTT = FNRDER(5)
    TD_ARDT = FNRDER(6)

    F = AR
    F_T = -T_ART/T
    F_V = -D_ARD*D

    ! Overall reduced Helmholtz
    F = F + FI
    F_T = F_T + FI_T
    F_V = F_V + FI_V

    y = F*Rmix*T
    if (present(dydv)) then
      dydv = F_V*Rmix*T
    endif
    if (present(d2ydv2)) then
      FI_VV = D**2*(2.0*D_AID + DD_AIDD)
      F_VV = D**2*(2.0*D_ARD + DD_ARDD)
      F_VV = F_VV + FI_VV
      d2ydv2 = F_VV*Rmix*T
    endif
    if (present(dydt)) then
      dydt = (F + F_T*T)*Rmix
    endif
    if (present(d2ydt2)) then
      FI_TT = (TT_AITT + 2.0*T_AIT)/(T*T)
      F_TT = (TT_ARTT + 2.0*T_ART)/(T*T)
      F_TT = F_TT + FI_TT
      d2ydt2 = (2.0*F_T + F_TT*T)*Rmix
    endif
    if (present(d2ydvdt)) then
      FI_TV = D*TD_AIDT/T
      F_TV = D*TD_ARDT/T
      F_TV = F_TV + FI_TV
      d2ydvdt = (F_V + F_TV*T)*Rmix
    endif

  end function trend_free_energy

  !----------------------------------------------------------------------
  !> Calculate internal energy given composition,
  !! temperature and specific volume.
  !>
  !> \author MH, 2015-02
  !----------------------------------------------------------------------
  function trend_internal_energy(x,t,v,dudv,dudt) result(u)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    ! Transferred variables
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3/mol - Specific volume
    real, dimension(cdim), intent(in) :: x !< Compozition
    real, optional, intent(out) :: dudt !< J/(mol K) - Pressure differential wrpt. temperature
    real, optional, intent(out) :: dudv !< J/m3 - Pressure differential wrpt. specific volume
    real :: u !< J/mol - Internal energy
    ! Locals
    real :: D
    real, dimension(mnc) :: x_trend
    integer, dimension(nderivs) :: GETDERR   !Vector tells the subroutine which derivative is neccessary to calculate the property (0 or 1)
    integer, dimension(nderivs) :: GETDERI   !Vector tells the subroutine which derivative is neccessary to calculate the property (0 or 1)
    real, dimension(nderivs) :: FNRDER                !Vector gives the values of the calculated derivatives
    real, dimension(nderivs) :: FNIDER                !Vector gives the values of the calculated derivatives
    real :: Rmix,D_ARD,DD_ARDT,P,TT_ARTT,TT_AITT,T_ART,T_AIT,dPdT

    !--------------------------------------------------------------------
    x_trend = 0.0
    x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)
    D = 1.0/v

    GETDERR = 0
    GETDERI = 0
    GETDERR(4) = 1
    GETDERI(5) = 1
    if (present(dudv)) then
      GETDERR(2) = 1
      GETDERR(6) = 1
    endif
    if (present(dudt)) then
      GETDERR(5) = 1
      GETDERI(6) = 1
    endif

    !call wm_mix_calc(wmmix)                 !subroutine calculates molmass of fluidmixture
    if (useConstantRmix) then
      Rmix = Rgas_External
    else
      call R_mix_calc(Rmix)                   ! calculates the "mixture molar gas constant"
    endif
    CALL MIXDERIVSFNR(T,D,GETDERR,FNRDER)   !subroutine calculates derivatives of residual part
    CALL MIXDERIVSFNI(T,D,GETDERI,FNIDER)   !subroutine calculates derivatives of ideal part

    T_ART = FNRDER(4)
    T_AIT = FNIDER(5)

    u = (T_AIT + T_ART) * Rmix * T

    if (present(dudv)) then
      D_ARD = FNRDER(2)
      p = (1.D0 + D_ARD) * D * Rmix * T
      DD_ARDT = FNRDER(6)
      dPdT = D * Rmix * (1.d0 +  D_ARD - DD_ARDT)
      dudv = T*dPdT - P
    endif
    if (present(dudt)) then
      TT_AITT = FNIDER(6)
      TT_ARTT = FNRDER(5)
      dudt = - (TT_AITT + TT_ARTT) * Rmix
    endif

  end function trend_internal_energy

  !> Calculate fugacity and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3]
  !! \param n - Mole numbers [mol]
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine trend_thermoTV(T,V,n,lnfug,lnfugT,lnfugV,lnfugn)
    use module_fluid_parameters, only: molfractions, ncomp, REQ
    use trend_constants, only: mnc
    use module_nderivs, only: nderivs
    implicit none
    real, intent(in) :: T,V,n(cdim)
    ! Output.
    real, optional, intent(out) :: lnfug(cdim)
    real, optional, intent(out) :: lnfugT(cdim),lnfugV(cdim),lnfugn(cdim,cdim)
    ! Locals
    real :: D, logni
    real, dimension(mnc) :: x_trend
    integer :: i
    real :: sumn, inv_sumn
    real :: dfiddn(mnc),d2fiddn2(mnc),d2fiddndt(mnc),d2fiddndv(mnc)
    real, parameter :: logCutOff = 1.0e-50
    interface
      subroutine trend_CalcFres(T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_TV,F_VV,F_Tn,F_Vn,F_nn)
        use module_fluid_parameters, only: ncomp
        implicit none
        real, intent(in) :: T,V,n(cdim)
        real, optional, intent(out) :: F,F_T,F_V,F_n(cdim),F_TT,F_TV,F_VV
        real, optional, intent(out) :: F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
      end subroutine trend_CalcFres
    end interface

    x_trend = 0.0
    sumn = sum(n(1:ncomp))
    inv_sumn = 1.0/sumn
    x_trend(1:ncomp) = n(1:ncomp)*inv_sumn
    molfractions = x_trend
    call reduced_parameters_calc(T)
    D = sumn/v

    do i=1,ncomp
      if (present(lnfug)) then
        if(abs(n(i)) .LT. logCutOff) then
          logni = log(logCutOff)
        else
          logni = log(n(i))
        end if
        dfiddn(i) = logni - log(V/(REQ(i)*T))
      endif
      if(present(lnfugn)) then  ! second composition derivative
        if (n(i) > 0.0) then
          d2fiddn2(i) = 1.0/n(i)
        else
          d2fiddn2(i) = 0.0
        endif
      end if
    end do
    if(present(lnfugT)) then
      d2fiddndt = 1.0/T
    end if
    if(present(lnfugV)) then
      d2fiddndv = -1.0/v
    end if

    call trend_CalcFres(T,V,n,F_n=lnfug,F_Tn=lnfugT,F_Vn=lnfugV,F_nn=lnfugn)

    if (present(lnfug)) then
      do i=1,ncomp
        lnfug(i) = lnfug(i) + dfiddn(i) !min(max(chempot_res(i),-25.0),700.0)
      enddo
    endif
    if (present(lnfugT)) then
      lnfugT = lnfugT + d2fiddndt(1:ncomp)
    endif
    if (present(lnfugV)) then
      lnfugV = lnfugV + d2fiddndv(1:ncomp)
    endif
    if (present(lnfugn)) then
      do i=1,ncomp
        lnfugn(i,i) = lnfugn(i,i) + d2fiddn2(i)
      enddo
    endif
  end subroutine trend_thermoTV

  !> Calculate resudial reduced Helmholtz and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3]
  !! \param n - Mole numbers [mol]
  !! \param F... - Resudial reduced Helmholtz differentiuals
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine trend_CalcFres(T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_TV,F_VV,F_Tn,F_Vn,F_nn)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External, REQ
    use trend_constants, only: mnc
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    real, intent(in) :: T,V,n(cdim)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_n(cdim),F_TT,F_TV,F_VV
    real, optional, intent(out) :: F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
    ! Locals
    real :: D
    real, dimension(mnc) :: x_trend, chempot_res, d2nadndT, ndnardnidV
    real, dimension(mnc,mnc) :: DERIVSANINJ
    integer, dimension(nderivs) :: GETDERR !Vector tells the subroutine which derivative is neccessary to calculate the property (0 or 1)
    real, dimension(nderivs) :: FNRDER                !Vector gives the values of the calculated derivatives
    real :: AR,D_ARD,TD_ARDT,TT_ARTT,T_ART,DD_ARDD,Rmix,sumn,inv_sumn,inv_Rext
    real :: Rscale, Req_Scale(mnc), AR_T, AR_V, Fl_n(mnc)
    integer :: i,j

    !
    x_trend = 0.0
    sumn = sum(n(1:ncomp))
    inv_sumn = 1.0/sumn
    x_trend(1:ncomp) = n(1:ncomp)/sumn
    molfractions = x_trend
    call reduced_parameters_calc(T)
    D = sumn/v

    if (useConstantRmix) then
      Rscale = 1.0
      Req_Scale(1:ncomp) = 1.0
    else
      call R_mix_calc(Rmix) ! calculates the "mixture molar gas constant"
      inv_Rext = 1.0/Rgas_External
      Rscale = Rmix*inv_Rext
      Req_Scale(1:ncomp) = REQ(1:ncomp)*inv_Rext
    endif

    GETDERR(1:6) = 1
    GETDERR(7:nderivs) = 0
    CALL MIXDERIVSFNR(T,D,GETDERR,FNRDER) !subroutine calculates derivatives of residual part

    AR = FNRDER(1)
    T_ART = FNRDER(4)
    D_ARD = FNRDER(2)
    TD_ARDT = FNRDER(6)
    TT_ARTT = FNRDER(5)
    DD_ARDD = FNRDER(3)

    AR_T = (-T_ART/T)
    AR_V = -D_ARD*D
    if (present(F)) F = AR*Rscale*sumn
    if (present(F_T)) F_T = AR_T*Rscale*sumn
    if (present(F_TT)) F_TT = sumn*Rscale*(TT_ARTT + 2.0*T_ART)/(T*T)
    if (present(F_V)) F_V = AR_V*Rscale
    if (present(F_TV)) F_TV = Rscale*D*TD_ARDT/T
    if (present(F_VV)) F_VV = Rscale*D**2*(2.0*D_ARD + DD_ARDD)/sumn

    if (present(F_n) .or. present(F_nn)) then
      call dna_dni(T, D, chempot_res, 2)
      Fl_n(1:ncomp) = (Req_Scale(1:ncomp) - Rscale)*AR + Rscale*chempot_res(1:ncomp)
    endif
    if (present(F_n)) then
      F_n(1:ncomp) = Fl_n(1:ncomp)
    endif
    if (present(F_Tn)) then
      ! get the derivative d^2(n*ar)/d(ni)d(T) at const. V, n
      call d2na_dnidT_V(T, D, d2nadndT)
      F_Tn(1:ncomp) = (Req_Scale(1:ncomp) - Rscale)*AR_T + Rscale*d2nadndT(1:ncomp)
    endif
    if (present(F_Vn)) then
      call nd2na_dnidV_T(T, D, ndnardnidV)
      F_Vn(1:ncomp) = ((Req_Scale(1:ncomp) - Rscale)*AR_V + &
           Rscale*ndnardnidV(1:ncomp))*inv_sumn
    endif
    if (present(F_nn)) then
      Fl_n(1:ncomp) = Fl_n(1:ncomp)*inv_sumn
      chempot_res(1:ncomp) = chempot_res(1:ncomp)*inv_sumn
      ! call the derivative n*d(d(n*ar)/d(ni))dnj
      call nddna_dnidnj(T, D, DERIVSANINJ)
      do i=1,ncomp
        do j=1,ncomp
          F_nn(i,j) = Rscale*(DERIVSANINJ(i,j)*inv_sumn) + &
               Req_Scale(i)*chempot_res(j) + Req_Scale(j)*chempot_res(i) - &
               Fl_n(i) - Fl_n(j)
        enddo
      enddo
    endif
  end subroutine trend_CalcFres

  !> Calculate reduced ideal Helmholtz and differentials
  !!
  !! \param T - Temperature [K]
  !! \param v - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  !! \param F... - Ideal reduced Helmholtz differentiuals
  !!
  !! \author Morten Hammer
  !! \date 2015-09
  subroutine trend_CalcFid(T,V,n,F,F_T,F_V,F_TT,F_TV,F_VV,F_n,F_Tn,F_Vn,F_nn)
    use module_fluid_parameters, only: molfractions, ncomp, REQ, Rgas_External
    use trend_constants, only: mnc
    use module_nderivs, only: nderivs
    use intface, only: useConstantRmix
    implicit none
    real, intent(in) :: T,V,n(cdim)
    ! Output.
    real, optional, intent(out) :: F,F_T,F_V,F_TT,F_TV,F_VV
    real, optional, intent(out) :: F_n(cdim),F_Tn(cdim),F_Vn(cdim),F_nn(cdim,cdim)
    ! Locals
    real :: D
    real, dimension(mnc) :: x_trend
    integer, dimension(nderivs) :: GETDERI !Vector tells the subroutine which derivative is neccessary to calculate the property (0 or 1)
    real, dimension(nderivs) :: FNIDER ! Vector gives the values of the calculated derivatives
    real :: AI,D_AID,TD_AIDT,TT_AITT,T_AIT,DD_AIDD,sumn,inv_sumn,logni,Rmix
    integer :: i
    real, parameter :: logCutOff = 1.0e-50
    real :: Rscale, Req_Scale(mnc), inv_Rext
    !
    sumn = sum(n(1:ncomp))
    inv_sumn = 1.0/sumn
    x_trend = 0.0
    x_trend(1:ncomp) = n(1:ncomp)*inv_sumn
    molfractions = x_trend
    call reduced_parameters_calc(T)
    D = sumn/v

    if (useConstantRmix) then
      Rscale = 1.0
      Req_Scale(1:ncomp) = 1.0
    else
      call R_mix_calc(Rmix) ! calculates the "mixture molar gas constant"
      inv_Rext = 1.0/Rgas_External
      Rscale = Rmix*inv_Rext
      Req_Scale(1:ncomp) = REQ(1:ncomp)*inv_Rext
    endif

    GETDERI(1:6) = 1
    GETDERI(7:nderivs) = 0
    CALL MIXDERIVSFNI(T,D,GETDERI,FNIDER)   !subroutine calculates derivatives of ideal part

    AI = FNIDER(1)
    D_AID = FNIDER(2)
    DD_AIDD = FNIDER(3)
    TD_AIDT = FNIDER(4)
    T_AIT = FNIDER(5)
    TT_AITT = FNIDER(6)

    if (present(F)) F = AI*sumn*Rscale
    if (present(F_T)) F_T = Rscale*sumn*(-T_AIT/T)
    if (present(F_TT)) F_TT = Rscale*sumn*(TT_AITT + 2.0*T_AIT)/(T*T)
    if (present(F_V)) F_V = -D*D_AID*Rscale
    if (present(F_TV)) F_TV = Rscale*D*TD_AIDT/T
    if (present(F_VV)) F_VV = Rscale*D**2*(2.0*D_AID + DD_AIDD)*inv_sumn

    if ( present(F_n) .OR. present(F_Tn) .OR.&
         present(F_nn) .OR. present(F_Vn)) then

      GETDERI(1:2) = 1
      GETDERI(3:nderivs) = 0
      if (present(F_Tn)) then
        GETDERI(5) = 1
      endif
      D = 1.0/v
      if (present(F_nn)) then
        F_nn = 0.0
      endif
      do i = 1, ncomp
        !Get F of the pure component
        Call FNIDERIVS(T,D,GETDERI,FNIDER,i)
        if (present(F_n)) then
          if(abs(n(i)) .LT. logCutOff) then
            logni = log(logCutOff)
          else
            logni = log(n(i))
          end if
          F_n(i) = 1.0 + FNIDER(1) + logni
        endif
        if (present(F_nn)) then
          if (n(i) > 0.0) then
            F_nn(i,i) = 1.0/n(i)
          endif
        endif
        if (present(F_Tn)) then
          F_Tn(i) = -FNIDER(5)/T
        endif
        if (present(F_Vn)) then
          F_Vn(i) = -D*FNIDER(2)
        endif
      end do
    endif

  end subroutine trend_CalcFid

  !> Calculate density from pressure and temperature
  !!
  !! \param T - Temperature [K]
  !! \param P - Specific volume [Pa]
  !! \param n - Mole numbers [mol]
  !! \param phase id
  !!
  !! \author Morten Hammer
  !! \date 2015-10
  function trend_specvol(T,P,n,phase) result(v)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    real, intent(in) :: T,P,n(ncomp)
    integer, intent(in) :: phase
    ! Output.
    real :: v
    ! Locals
    real :: rho, P_MPa, Rscale, Rmix
    real, dimension(mnc) :: x_trend
    real, external :: rhomix_calc, p_calc
    !
    x_trend = 0.0
    x_trend(1:ncomp) = n(1:ncomp)/sum(n(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)
    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rmix/Rgas_External
    else
      Rscale = 1.0
    endif

    P_MPa = Rscale*P*1.0e-6
    rho = rhomix_calc(T, P_MPa, 0.0, phase, 0)
    if (rho > 0.0) then
      v = 1.0/rho
    else
      v = 0.0
    endif
  end function trend_specvol

  !> Calculate density from pressure and temperature
  !!
  !! \param T - Temperature [K]
  !! \param P - Specific volume [Pa]
  !! \param V - Specific volume [m3/mol]
  !! \param n - Mole numbers [mol]
  !! \param phase id
  !!
  !! \author Morten Hammer
  !! \date 2015-10
  function trend_voltest(T,P,V,n,phase) result(correctPhase)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    real, intent(in) :: T,P,V,n(cdim)
    integer, intent(in) :: phase
    ! Output.
    integer :: correctPhase
    ! Locals
    real :: rho, P_MPa, Rscale, Rmix
    integer :: IFound
    real, dimension(mnc) :: x_trend

    !
    x_trend = 0.0
    x_trend(1:ncomp) = n(1:ncomp)/sum(n(1:ncomp))
    molfractions = x_trend
    call reduced_parameters_calc(T)

    if (useConstantRmix) then
      call R_mix_calc(Rmix)
      Rscale = Rgas_External/Rmix
    else
      Rscale = 1.0
    endif

    P_MPa = Rscale*P*1.0e-6
    rho = 1.0/v
    call rho_test(T, P_MPa, rho, phase, IFound, ncomp)
    correctPhase = IFound
  end function trend_voltest

  !----------------------------------------------------------------------
  !> Calculate mixture gas constant
  !>
  !> \author MH, 2015-11
  !----------------------------------------------------------------------
  function trend_rmix(x) result(Rmix)
    use module_fluid_parameters, only: molfractions, ncomp, Rgas_External
    use trend_constants, only: mnc
    use intface, only: useConstantRmix
    implicit none
    ! Input:
    real, dimension(cdim), intent(in) :: x    !< Composition (mol/mol)
    ! Output:
    real :: Rmix
    ! Locals:
    real, dimension(mnc) :: x_trend
    !
    !--------------------------------------------------------------------
    if (useConstantRmix) then
      Rmix = Rgas_External
    else
      ! Set parameters
      x_trend = 0.0
      x_trend(1:ncomp) = x(1:ncomp)/sum(x(1:ncomp))
      molfractions = x_trend
      call R_mix_calc(Rmix)                   ! calculates the "mixture molar gas constant"
    endif
  end function trend_rmix

  !----------------------------------------------------------------------
  !> Calculate saturation properties from auxillary equations
  !>
  !> \author MH, 2016-02
  !----------------------------------------------------------------------
  subroutine trend_aux_sat_prop(T,i,P,vv,vl)
    implicit none
    ! Input:
    real, intent(in) :: T     !< Temperature (K)
    integer, intent(in) :: i  !< Component index
    ! Output:
    real, intent(out) :: P    !< Pressure (Pa)
    real, intent(out) :: vv   !< Vapour volume (m3/mol)
    real, intent(out) :: vl   !< Liquid volume (m3/mol)
    ! Locals
    real, external :: vp_eq, dl_eq, dv_eq
    !--------------------------------------------------------------------
    P = vp_eq(T, i)*1.0e6 ! MPa -> Pa
    vl = 1.0/dl_eq(T, i)
    vv = 1.0/dv_eq(T, i)
  end subroutine trend_aux_sat_prop

  !> Calculate reduced ideal properties
  !! Only temperature dependent part
  !!
  !! \param[in] T - Temperature [K]
  !! \param[in] i - Component number
  !! \param[out] Cp - Ideal Cp [J/mol/K]
  !! \param[out] h - Ideal enthalpy [J/mol]
  !! \param[out] s - Ideal entropy [J/mol/K]
  !!
  !! \author Morten Hammer
  !! \date 2019-11
  subroutine trend_Ideal(T,i,Cp,h,s)
    use module_fluid_parameters, only: REQ
    use module_nderivs, only: nderivs
    use module_general_eos_parameters, only: RHORED
    implicit none
    real, intent(in) :: T
    integer, intent(in) :: i
    ! Output.
    real, optional, intent(out) :: Cp,h,s
    ! Locals
    real :: rho !< Density (mol/m3)
    real :: AI,T_AIT,TT_AITT, R
    real :: FIDeriv(nderivs)
    integer :: getderivs(nderivs)

    getderivs = 0
    if (present(s)) then
      getderivs(1) = 1
    endif
    if (present(h) .or. present(s)) then
      getderivs(5) = 1
    endif
    if (present(Cp)) then
      getderivs(6) = 1
    endif
    R = Req(i)
    rho = RHORED(i) ! Remove contribution from density
    ! Get ideal gas properties
    call fniderivs(t, rho, getderivs, FIDeriv, i)

    if (present(s)) then
      AI = FIDeriv(1)
      T_AIT = FIDeriv(5)
      ! Sid = -A_T = -(RTF)_T = -R(TF_T+F)
      ! S0 = Sid + Rln(rhoRT)
      s = (T_AIT - AI + log(rho*R*T)) * R
    endif

    if (present(Cp)) then
      TT_AITT = FIDeriv(6)
      ! F_T = (-T_AIT/T)
      ! F_TT = (TT_AITT + 2.0*T_AIT)/(T*T)
      ! Cv = T*s_T = -TT_AITT*R
      ! Cp = Cv + R
      Cp = (1-TT_AITT)*R
    endif

    if (present(h)) then
      ! H = U + RT = A + TS + RT = RT*(1 + F - (F + T*F_T))
      ! H = RT*(1 - T*F_T)
      ! H0 = H
      T_AIT = FIDeriv(5)
      h = (1 + T_AIT)*R*T
    endif

  end subroutine trend_Ideal

