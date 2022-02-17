module binaryPlot
  !
  ! Module for binary Txy and Pxy plots.
  !
  ! MHA, 2012-01-27.
  !
  !
  use thermopack_constants, only: verbose, LIQPH, VAPPH
  use thermopack_var, only: nc, nph, get_active_thermo_model, thermo_model
  implicit none
  save
  !
  !
  private

  type :: criticalLine
    integer :: nPoints
    real, allocatable :: Tc(:), vc(:), Pc(:), Zc(:,:)
  contains
    procedure, public :: get_size => criticalLine_get_size
    procedure, public :: get_n_points => criticalLine_get_npoints
    procedure, public :: clear => criticalLine_clear
    procedure, public :: allocate => criticalLine_allocate
    procedure, public :: push_back => criticalLine_push_back
    procedure :: criticalLine_assign
    generic,   public   :: assignment(=) => criticalLine_assign
  end type criticalLine

  type :: VLLE_Line
    integer :: nPoints
    real, allocatable :: T(:), P(:)
    real, allocatable :: X(:,:), W(:,:), Y(:,:)
    real, allocatable :: vx(:), vw(:), vy(:)
  contains
    procedure, public :: get_size => VLLE_Line_get_size
    procedure, public :: get_n_points => VLLE_Line_get_npoints
    procedure, public :: clear => VLLE_Line_clear
    procedure, public :: allocate => VLLE_Line_allocate
    procedure, public :: push_back => VLLE_Line_push_back
    procedure, public :: print_index => VLLE_Line_print_index
    procedure, public :: swap_with_last => VLLE_Line_swap_with_last
    procedure :: VLLE_Line_assign
    generic,   public   :: assignment(=) => VLLE_Line_assign
  end type VLLE_Line

  type :: azeotropicLine
    integer :: nPoints
    real, allocatable :: T(:), vg(:), vl(:), P(:), Z(:,:)
  contains
    procedure, public :: get_size => azeotropicLine_get_size
    procedure, public :: get_n_points => azeotropicLine_get_npoints
    procedure, public :: clear => azeotropicLine_clear
    procedure, public :: allocate => azeotropicLine_allocate
    procedure, public :: push_back => azeotropicLine_push_back
    procedure :: azeotropicLine_assign
    generic,   public   :: assignment(=) => azeotropicLine_assign
  end type azeotropicLine

  integer, parameter :: TSPEC = 1,PSPEC = 2
  integer, parameter :: neq = 7
  integer, parameter :: maxPoints = 10000
  real :: binaryAbsTol = 1.0e-12
  integer, parameter :: binaryMaxIt = 40
  integer, parameter :: BP_TERM_NONE = 0, BP_TERM_PMAX = 1, BP_TERM_PMIN = 2, &
       BP_TERM_TMIN = 3, BP_TERM_CRIT = 4, BP_TERM_SINGLE = 5, &
       BP_TERM_ERR = 6, BP_TERM_NPOINT = 7, BP_TERM_TPD = 8
  integer, parameter :: BCL_PMAX = 1, BCL_PURE_C2 = 2, BCL_PURE_C1 = 3
  integer , parameter :: char_term_len = 15
  public :: binaryXY, fillX, binaryXYfun
  public :: map_binary_envelope, binaryXYarray
  public :: binaryPxy, binaryTxy, maxPoints
  public :: VLLEBinaryXY, TSPEC, PSPEC, binaryAbsTol
  public :: global_binary_plot, BP_TERM_ERR
  public :: LLVE_TV_sensitivity, LLVEpointTV, binary_is_stable
  public :: getPropFromX_LLVE, setX_LLVE, get_BP_TERM
  !
contains

  !----------------------------------------------------------------------
  subroutine get_BP_TERM(iTerm, charTerm)
    integer, intent(in) :: iTerm
    character(len=*), intent(out) :: charTerm
    if (len(charTerm) >= char_term_len) then
      charTerm = print_BP_TERM(iTerm)
    else
      charTerm = ""
      print *,"get_BP_TERM: character variable too short"
    endif
  end subroutine get_BP_TERM

  !----------------------------------------------------------------------
  function print_BP_TERM(iTerm) result(charTerm)
    integer, intent(in) :: iTerm
    character(len=char_term_len) :: charTerm
    select case(iTerm)
    case(BP_TERM_NONE)
      charTerm = "BP_TERM_NONE"
    case(BP_TERM_PMAX)
      charTerm = "BP_TERM_PMAX"
    case(BP_TERM_PMIN)
      charTerm = "BP_TERM_PMIN"
    case(BP_TERM_TMIN)
      charTerm = "BP_TERM_TMIN"
    case(BP_TERM_CRIT)
      charTerm = "BP_TERM_CRIT"
    case(BP_TERM_SINGLE)
      charTerm = "BP_TERM_SINGLE"
    case(BP_TERM_ERR)
      charTerm = "BP_TERM_ERR"
    case(BP_TERM_NPOINT)
      charTerm = "BP_TERM_NPOINT"
    case(BP_TERM_TPD)
      charTerm = "BP_TERM_TPD"
    case default
      call stoperror("print_BP_TERM: Wrong iTerm")
    end select
  end function print_BP_TERM

  !----------------------------------------------------------------------
  subroutine binaryXY(T,P,x,y,ispec,Tmin,Pmax,dzmax,filename,&
       dlns_max_override,phase,ispecStep0,dlns0,Pmin,calcvolumes)
    ! Dump x/y,P/T to file (filename)
    ! Routine assumes minimum T or minimum P of the envelope is given
    ! Steps upward in T or P
    ! ispec = 1:T is specified, 2:P is specified
    use thermopack_constants, only: LIQPH, VAPPH
    use saturation, only: bubT,bubP
    use eos, only: specificvolume
    implicit none
    integer, intent(in) :: ispec
    real, intent(in) :: T,P,Tmin,Pmax,dzmax
    real, dimension(nc), intent(in) :: x,y
    character(len=*), intent(in) :: filename
    integer, dimension(2), optional, intent(in) :: phase
    integer, optional, intent(in) :: ispecStep0
    real, optional :: dlns_max_override,dlns0,Pmin
    logical, optional, intent(in) :: calcvolumes

    ! Internal variables
    real, dimension(maxPoints) :: xres, yres, tpres
    real, target, dimension(maxPoints) :: vgres, vlres
    real, pointer :: vgres_p(:), vlres_p(:)
    integer :: npoints, ierr, i
    real :: Ti, Pi, xi(nc), yi(nc)

    call binaryXYarray(T,P,x,y,ispec,Tmin,Pmax,dzmax,xres,yres,&
         tpres,npoints,ierr,dlns_max_override,phase,ispecStep0,&
         dlns0,Pmin)

    vgres_p => NULL()
    vlres_p => NULL()
    if (present(calcvolumes)) then
      if (calcvolumes) then
        Ti = T
        Pi = P
        do i=1,npoints
          xi(1) = xres(i)
          xi(2) = min(max(1-xi(1),0.0),1.0)
          yi(1) = yres(i)
          yi(2) = min(max(1-yi(1),0.0),1.0)
          if (ispec == TSPEC) then
            Pi = tpres(i)
          else
            Ti = tpres(i)
          endif
          call specificvolume(Ti,Pi,xi,LIQPH,vlres(i))
          call specificvolume(Ti,Pi,yi,VAPPH,vgres(i))
        enddo
        vgres_p => vgres
        vlres_p => vlres
      endif
    endif
    call dumpBinaryDataToFile(filename,ispec,T,P,xres,yres,tpres,npoints,vgres_p,vlres_p)

  end subroutine binaryXY

  !----------------------------------------------------------------------
  subroutine dumpBinaryDataToFile(filename,ispec,T,P,xres,yres,tpres,nres,vgres,vlres)
    use utilities, only: newunit
    integer, intent(in) :: ispec
    character(len=*), intent(in) :: filename
    real, intent(in) :: T,P
    real, dimension(:), intent(in) :: xres, yres, tpres
    real, dimension(:), optional, intent(in) :: vgres, vlres
    integer, intent(in) :: nres
    !
    integer :: i, ifile
    logical :: save_volumes
    character(len=100) :: header
    save_volumes = (present(vgres) .and. present(vlres))

    ! Dump data to file
    if (nres > 0) then
      ifile = newunit()
      open(unit=ifile,file=trim(filename))
      if (ispec == TSPEC) then
        write(ifile,*) '#Pxy plot: Temperature (K):',T
        header = '#x (-)'//char(9)//'y (-)'//char(9)//'P (Pa)'
      else
        write(ifile,*) '#Txy plot: Pressure (bar):',P*1.0e-5
        header = '#x (-)'//char(9)//'y (-)'//char(9)//'T (K)'
      endif
      if (save_volumes) then
        header = trim(header)//char(9)//'vl (m3/mol)'//char(9)//'vg (m3/mol)'
      endif
      write(ifile,*) trim(header)
      do i=1,nres
        if (save_volumes) then
          write(ifile,'(5es19.10e3)') xres(i), yres(i), tpres(i), vlres(i), vgres(i)
        else
          write(ifile,'(3es19.10e3)') xres(i), yres(i), tpres(i)
        endif
      enddo
      close(ifile)
    endif
  end subroutine dumpBinaryDataToFile

  !----------------------------------------------------------------------
  subroutine mergeBinaryDataToFile(filename,ispec,T,P,xLLE,wLLE,tpLLE,nLLE,&
       xL1VE,yL1VE,tpL1VE,nL1VE,xL2VE,yL2VE,tpL2VE,nL2VE,&
       x_tpl,y_tpl,w_tpl)
    use utilities, only: newunit
    use thermopack_constants, only: clen
    integer, intent(in) :: ispec
    character(len=*), intent(in) :: filename
    real, intent(in) :: T,P
    real, dimension(:), intent(in) :: xLLE,wLLE, tpLLE
    integer, intent(in) :: nLLE,nL1VE,nL2VE
    real, dimension(:), intent(in) :: xL1VE,yL1VE,tpL1VE
    real, dimension(:), intent(in) :: xL2VE,yL2VE,tpL2VE
    real, intent(in) :: x_tpl, y_tpl, w_tpl
    !
    integer :: i, n, ifile
    character(len=clen) :: binaryline, mergedlines
    character(len=*), parameter :: sep = '  '

    n = max(nLLE,nL1VE,nL2VE)
    ! Dump data to file
    if (n > 0) then
      ifile = newunit()
      open(unit=ifile,file=trim(filename))
      if (ispec == TSPEC) then
        write(ifile,'(A,es19.10e3)') '#Pxy plot: Temperature (K):',T
        write(ifile,'(A,4es19.10e3)')'#Triple line: ',x_tpl,w_tpl,y_tpl,P
        write(ifile,'(A)') '#xLL (-)'//sep//'wLL (-)'//sep//'pLL (Pa)'//&
             sep//'xL1V (-)'//sep//'yL1V (-)'//sep//'pL1V (Pa)'//&
             sep//'xL2V (-)'//sep//'yL2V (-)'//sep//'pL2V (Pa)'
      else
        write(ifile,'(A,es19.10e3)') '#Txy plot: Pressure (bar):',P*1.0e-5
        write(ifile,'(A,4es19.10e3)')'#Triple line: ',x_tpl,w_tpl,y_tpl,T
        write(ifile,'(A)') '#xLL (-)'//sep//'wLL (-)'//sep//'TLL (K)'//&
             sep//'xL1V (-)'//sep//'yL1V (-)'//sep//'TL1V (K)'//&
             sep//'xL2V (-)'//sep//'yL2V (-)'//sep//'TL2V (K)'
      endif
      do i=1,n
        mergedlines = ''
        if (i > nLLE) then
          binaryline = '.' // sep // '.' // sep // '.'
        else
          write(binaryline,'(3es19.10e3)') xLLE(i), wLLE(i), tpLLE(i)
        endif
        mergedlines = trim(binaryline)
        if (i > nL1VE) then
          binaryline = '.' // sep // '.' // sep //'.'
        else
          write(binaryline,'(3es19.10e3)') xL1VE(i), yL1VE(i), tpL1VE(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(binaryline)
        if (i > nL2VE) then
          binaryline = '.' // sep // '.' // sep // '.'
        else
          write(binaryline,'(3es19.10e3)') xL2VE(i), yL2VE(i), tpL2VE(i)
        endif
        mergedlines = trim(mergedlines) // sep // trim(binaryline)
        write(ifile,'(A)') trim(mergedlines)
      enddo
      close(ifile)
    endif
  end subroutine mergeBinaryDataToFile

  !----------------------------------------------------------------------
  subroutine binaryXYarray(T,P,x0,y0,ispec,Tmin,Pmax,dzmax,xres,yres,tpres,&
       npoints,ierr,dlns_max_override,phase,ispecStep0,dlns0,Pmin,sign0,&
       iterm,paranoid)
    ! Dump x/y,P/T to file (filename)
    ! Routine assumes minimum T or minimum P of the envelope is given
    ! Steps upward in T or P
    ! ispec = 1:T is specified, 2:P is specified
    use thermopack_constants, only: LIQPH, VAPPH
    use saturation, only: bubT,bubP
    use critical, only: calcStabMinEig, calcCriticalZ
    use eos, only: specificvolume
    use thermopack_constants, only: tpTmin
    implicit none
    integer, intent(in) :: ispec
    real, intent(in) :: T,P,Tmin,Pmax,dzmax
    real, dimension(nc) :: x0,y0
    real, dimension(:), intent(out) :: xres, yres, tpres
    integer, intent(out) :: ierr,npoints
    integer, dimension(2), optional, intent(in) :: phase
    integer, optional, intent(in) :: ispecStep0
    real, optional :: dlns_max_override,dlns0,Pmin,sign0
    integer, optional, target :: iterm
    logical, optional :: paranoid

    ! Internal variables
    integer :: maxPoints, ispecStep, i, j, nStep
    real, dimension(neq) :: XX,dXds,XXold,dXdsOld !,F,B
    !real, dimension(neq,neq) :: JAC
    real :: dlns, PP, TT, dlns_max, dz, vc
    real, parameter :: x_tol = 1.0e-6
    integer, dimension(2) :: ph
    logical :: zeroCompCross, solveForLimit, solveForCrit
    integer, parameter :: nTries = 6
    real, dimension(nc) :: x,y
    integer, pointer :: p_iterm
    integer, target :: l_iterm
    logical :: l_paranoid, isStable
    real :: Tmin_sys

    ! Sould every point be checked for stability
    if (present(paranoid)) then
      l_paranoid = paranoid
    else
      l_paranoid = .false.
    endif

    if (ispec == TSPEC) then
      Tmin_sys = tpTmin
      tpTmin = 0.9*T
    endif

    maxPoints = min(size(xres),size(yres),size(tpres))

    if (present(phase)) then
      ph = phase
    else
      ph(1) = LIQPH
      ph(2) = VAPPH
    endif

    if (present(iterm)) then
      p_iterm => iterm
    else
      p_iterm => l_iterm
    endif
    p_iterm = BP_TERM_NONE

    ! Written for nc == 2
    if (.not. nc == 2) then
      call stoperror('binaryXY written for 2 components')
    endif

    PP = P
    TT = T
    x = x0
    y = y0
    call fillX(XX,TT,PP,x,y,ispec,ph)
    ispecStep = 7
    if (present(ispecStep0)) then
      ispecStep = ispecStep0
    endif
    ! Store starting point
    call set_bin_res(x,y,TT,PP,ispec,xres,yres,tpres,maxPoints,1,npoints,&
         ph,isStable,l_paranoid)

    ! Start by increasing P/T
    dlns = 0.005
    dlns_max = 0.01
    if (present(dlns_max_override)) then
      dlns_max = dlns_max_override
    endif
    dlns = min(dlns_max*0.25, 0.005)
    if (present(dlns0)) then
      dlns = dlns0
    endif
    if (present(sign0)) then
      dlns = dlns*sign0
    else
      dlns = dlns*initialStepSign(TT,PP,XX,ispec,ispecStep,ph)
    endif
    solveForLimit = .false.
    solveForCrit = .false.
    dXds=0.0
    do i=2,maxPoints
      XXold = XX
      dXdsOld = dXds
      zeroCompCross = .false.
      do j=1,nTries
        ! Make sure step is limited, and that dlns is increased when possible
        XX=XXold
        nStep = binaryStep(XX,TT,PP,x,y,ispec,ispecStep,dlns,&
             dzmax,Pmax,Tmin,ph,dXds,ierr,Pmin)
        dz = max(abs(XX(3)-XXold(3)),abs(XX(5)-XXold(5))) + 1E-6
        if ( dz > dzmax .or. (ierr /= 0)) then
          if (ierr == -2) then ! Single component?
            zeroCompCross = .true.
            exit ! Solve for single component
          else if (ierr == -1) then ! At Pmax/Pmin or Tmin
            solveForLimit = .true.
            exit ! Solve for Pmax/Tmin
          else !if (ierr /= 0) then
            dlns = dlns*0.5
            if (abs(dlns) < 5.0e-5) then
              !call testJacobian(XX,TT,PP,ispec,ispecStep,ph)
              ! dlns is becoming to small. Terminating.
              if ( min(minval(x),minval(y)) < 1.0e-5 .and. &
                   minval(XX(3:6)) < min(minval(x),minval(y))) then
                zeroCompCross = .true.
              endif
              exit
            endif
          endif
        else
          ! Increase dlns
          dlns = dlns * min(1.4, max(1.05,0.6*dzmax/dz))
          dlns = sign(min(dlns_max,dlns),dlns)
          exit
        endif
      enddo
      if (i > 2 .and. (XX(1)*XXold(1) <= 0.0)) then
        ! Composition crossing - solve for critical point?
        if (dXdsOld(3)*dxdsOld(5) < 0.0 .or. &
             (XX(3)-XXold(3))*(XX(5)-XXold(5)) < 0.0) then
          solveForCrit = .true.
        else if (calcStabMinEig(tt,pp,y,VAPPH) < 1.0e-4) then
          solveForCrit = .true.
        endif
      endif
      if (zeroCompCross) then
        call singleComponentSolution(XX,TT,PP,x,y,dXds,ispec,ierr)
        if (ierr /= 0) then
          print *,"binaryXYarray: Not able to terminate binary envelope."//&
               "Not able to solve for single component solution."
          p_iterm = BP_TERM_ERR
          return
        endif
        call set_bin_res(x,y,TT,PP,ispec,xres,yres,tpres,&
             maxPoints,i,npoints,ph,isStable,.false.)
        if (verbose) then
          print *,'binaryXY terminated with zero compozition'
          print *,'p,x1: ',pp,x(1)
        endif
        p_iterm=BP_TERM_SINGLE
        exit ! Exit do loop
      else if (solveForLimit) then
        if (ispec == TSPEC) then
          if (present(Pmin)) then
            if (abs(Pmin-exp(XXold(neq))) < abs(Pmax-exp(XXold(neq)))) then
              dlns = log(Pmin) - XXold(neq)
              p_iterm=BP_TERM_PMIN
            else
              dlns = log(Pmax) - XXold(neq)
              p_iterm=BP_TERM_PMAX
            endif
          else
            dlns = log(Pmax) - XXold(neq)
            p_iterm=BP_TERM_PMAX
          endif
          dXdS = dXdS/dXdS(neq)
        else
          dlns = log(Tmin) - XXold(neq-1)
          dXdS = dXdS/dXdS(neq-1)
          p_iterm=BP_TERM_TMIN
        endif
        XX = XXold + dXdS*dlns
        dlns = 0.0
        nStep = binaryStep(XX,TT,PP,x,y,ispec,ispecStep,dlns,&
             dzmax,Pmax*1.05,Tmin*0.95,ph,dXds,ierr)
        call set_bin_res(x,y,TT,PP,ispec,xres,yres,tpres,&
             maxPoints,i,npoints,ph,isStable,l_paranoid)
        if (.not. isStable) then
          ! Located additional phase exiting
          p_iterm = BP_TERM_TPD
          if (verbose) then
            print *,'binaryXY terminated due to phase instabillity'
          endif
        else
          if (verbose) then
            if (ispec == TSPEC) then
              print *,'binaryXY terminated with P == Pmax'
            else
              print *,'binaryXY terminated with T == Tmin'
            endif
          endif
        endif
        exit ! Exit do loop
      else if (ierr /= 0) then
        print *,'Not able to solve for binary XY point'
        print *,'Point number on envelope',i
        print *,'Number of iterations ',nStep-1, ' of maximum ', binaryMaxIt
        print *,'Phase 1:',ph(1)
        print *,'Phase 2:',ph(2)
        print *,'Current values:'
        print *,'Pressure (Pa):',PP
        print *,'Temperature (K):',TT
        print *,'Phase 1 composition:',x
        print *,'Phase 2 composition:',y
        print *,'Did not converge. Terminating in the following divergent state:'
        if (ispec == TSPEC) then ! T is specified
          print *,'Pressure (Pa):',tpres(i)
          print *,'Temperature (K):',TT
        else ! P is specified
          print *,'Pressure (Pa):',PP
          print *,'Temperature (K):',tpres(i)
        endif
        print *,'Phase 1 composition:',xres(npoints),1.0-xres(npoints)
        print *,'Phase 2 composition:',yres(npoints),1.0-yres(npoints)
        p_iterm=BP_TERM_ERR
        return
      else if (j>nTries .and. x(1) > 1E-12 .and. &
               x(1) < 1.0-1E-12 .and. verbose) then
        print *,'Warning: binaryXY2: Not all criterion met for step'
        print *,'at P = ',PP, 'x(1)=',x(1),'y(1)=',y(1)
      endif

      if (ispec == TSPEC .and. P > Pmax) then
        if (verbose) then
          print *,'binaryXY terminated with P > Pmax'
        endif
        p_iterm = BP_TERM_PMAX
        exit ! Exit do loop
      endif
      if (ispec == PSPEC .and. T < Tmin) then
        if (verbose) then
          print *,'binaryXY terminated with T < Tmin'
        endif
        p_iterm = BP_TERM_TMIN
        exit ! Exit do loop
      endif

      ! Critical point?
      if (solveForCrit .or. &
           (abs(x(1)-y(1)) < x_tol .and. dXds(3)*dxds(5) < 0.0)) then
        call specificvolume(TT,PP,x,LIQPH,vc)
        call calcCriticalZ(TT,vc,PP,x,s=2,ierr=ierr,tol=1.0e-7,free_comp=1)
        call set_bin_res(x,x,TT,PP,ispec,xres,yres,tpres,&
               maxPoints,i,npoints,ph,isStable,.false.)
        if (verbose) then
          print *,'binaryXY terminated at critical point'
          print *,'p,x1: ',pp,x(1)
        endif
        p_iterm = BP_TERM_CRIT
        exit ! Exit do loop
      endif

      call set_bin_res(x,y,TT,PP,ispec,xres,yres,tpres,maxPoints,i,npoints,&
           ph,isStable,l_paranoid)
      if (.not. isStable) then
        ! Located additional phase exiting
        p_iterm = BP_TERM_TPD
        if (verbose) then
          print *,'binaryXY terminated due to phase instabillity'
        endif
        exit
      endif
      if (verbose) then
        print *,PP,TT,x(1),y(1)
      endif
    enddo

    if (i>=maxPoints) then
      print *,'binaryXY terminated after maximum number of data point'
      p_iterm = BP_TERM_NPOINT
    endif

    if (ispec == TSPEC) then
      tpTmin = Tmin_sys
    endif
  end subroutine binaryXYarray

  !----------------------------------------------------------------------
  subroutine set_bin_res(x,y,T,P,ispec,xres,yres,tpres,nres,ires,npoints,&
       phase,isStable,paranoid)
    implicit none
    real, dimension(2), intent(in) :: x,y
    real, intent(in) :: T, P
    integer, intent(in) :: ispec, nres, ires
    integer, intent(out) :: npoints
    real, dimension(nres), intent(inout) :: xres,yres,tpres
    integer, intent(in) :: phase(2)
    logical, intent(in) :: paranoid
    logical, intent(out) :: isStable
    ! Locals
    real :: v1(2), v2(2)
    logical :: testStabillity

    xres(ires) = x(1)
    yres(ires) = y(1)
    if (ispec == TSPEC) then ! T is specified
      tpres(ires) = P
    else ! P is specified
      tpres(ires) = T
    endif
    npoints = ires

    testStabillity = .false.
    if (y(1)*y(2) > 0.0) then
      if (paranoid) then
        testStabillity = .true.
      else if (npoints > 3) then
        ! Try to detect swallow tail behaviour
        v2(1) = yres(ires) - yres(ires-1)
        v2(2) = log(tpres(ires)) - log(tpres(ires-1))
        v1(1) = yres(ires-1) - yres(ires-2)
        v1(2) = log(tpres(ires-1)) - log(tpres(ires-2))
        if (dot_product(v1,v2) < 0.0) then
          testStabillity = .true.
        endif
      endif
    endif

    if (testStabillity) then
      isStable = binaryIsStable(x,y,T,P,phase)
    else
      isStable = .true.
    endif
    !print *,tpres(ires),x(1),y(1)
  end subroutine set_bin_res

  !----------------------------------------------------------------------
  function binaryIsStable(x,y,T,P,phase,xx_tpl) result(isStable)
    use stability, only: stabcalcW
    use eos, only: thermo
    implicit none
    real, dimension(2), intent(in) :: x,y
    real, intent(in) :: T, P
    integer, intent(in) :: phase(2)
    ! Output
    real, optional, intent(out) :: xx_tpl(3,nc)
    logical :: isStable
    ! Locals
    real, dimension(nc) :: w
    real :: xx(nc), yy(nc)
    real :: lnFUGx(nc), lnFUGw(nc), tpd, Xvec(2,nc)
    real, parameter :: stabLimit = 1.0e-8
    logical :: isStable1, isStable2
    ! Set reference phase
    call thermo(t,p,x,LIQPH,lnFUGx)
    Xvec(1,:) = y
    Xvec(2,:) = x
    if (phase(1) == LIQPH .and. phase(2) == LIQPH) then
      w = y
      ! Look for vapour phase
      yy = (/1.0,0.0/)
      tpd = stabcalcW(2,2,t,p,Xvec,yy,VAPPH,lnFUGx,lnFUGw)
      isStable = .not. (tpd < stabLimit)
      if (isStable) then
        yy = (/0.0,1.0/)
        tpd = stabcalcW(2,2,t,p,Xvec,yy,VAPPH,lnFUGx,lnFUGw)
        isStable = .not. (tpd < stabLimit)
      endif
    else
      yy = y
      ! Look for liquid phase
      w = (/1.0,0.0/)
      tpd = stabcalcW(2,2,t,p,Xvec,W,LIQPH,lnFUGx,lnFUGw)
      isStable1 = .not. (tpd < stabLimit)
      xx = w
      if (isStable1 .or. present(xx_tpl)) then
        w = (/0.0,1.0/)
        tpd = stabcalcW(2,2,t,p,Xvec,W,LIQPH,lnFUGx,lnFUGw)
        isStable2 = .not. (tpd < stabLimit)
      else
        isStable2 = .true.
      endif
      isStable = (isStable1 .and. isStable2)
    endif

    if (.not. isStable .and. present(xx_tpl)) then
      xx_tpl(1,:) = xx
      xx_tpl(2,:) = w
      xx_tpl(3,:) = yy
    endif

  end function binaryIsStable

  !----------------------------------------------------------------------
  subroutine singleComponentSolution(XX,T,P,x,y,dXds,ispec,ierr)
    use saturation, only: bubT, safe_bubT, bubP, safe_bubP
    use numconstants, only: small
    implicit none
    real, dimension(2), intent(inout) :: x,y
    real, intent(inout) :: T, P
    integer, intent(in) :: ispec
    real, dimension(neq), intent(in) :: dXds
    real, dimension(neq), intent(inout) :: XX
    integer, intent(out) :: ierr
    ! Locals
    real :: dx,dxdP,dxdT
    if (x(1) <= 0.5) then
      !ln K1(T,P,[0,1]) = 0
      x(1) = 0.0
      x(2) = 1.0
      y(1) = 0.0
      y(2) = 1.0
      dx = -XX(3)
    else
      !ln K2(T,P,[1,0]) = 0
      x(1) = 1.0
      x(2) = 0.0
      y(1) = 1.0
      y(2) = 0.0
      dx = 1.0 - XX(3)
    endif
    if (ispec == TSPEC) then ! T is specified
      dxdP = dXds(3)/dXds(7)/P
      P = exp(XX(7))
      if (abs(dxdP) > small) then
        P = P + dx/dxdP
      endif
      P = bubP(T,P,x,y,ierr)
      if (ierr /= 0) then
        P = safe_bubP(T,x,y,ierr)
      endif
    else ! P is specified
      dxdT = dXds(3)/dXds(6)/T
      T = exp(XX(6))
      if (abs(dxdT) > small) then
        T = T + dx/dxdT
      endif
      T = bubT(T,P,x,y,ierr)
      if (ierr /= 0) then
        T = safe_bubT(P,x,y,ierr)
      endif
    endif
  end subroutine singleComponentSolution

  !----------------------------------------------------------------------
  subroutine binaryXYfun(XX,T,P,x,y,ispec,ispecStep,ln_spec,&
       FUN,JAC,phase)
    use eos, only: thermo
    use thermopack_constants, only: LIQPH, VAPPH
    implicit none
    integer, intent(in) :: ispec,ispecStep
    real, intent(inout) :: T,P
    real, intent(in) :: ln_spec
    real, dimension(nc), intent(out) :: x,y
    real, dimension(neq), intent(in) :: XX
    real, dimension(neq), intent(out) :: FUN
    real, dimension(neq,neq), intent(out) :: JAC
    integer, dimension(2), optional, intent(in) :: phase
    ! ispec = 1:T, 2:P
    ! ispecStep = 1: ln K1, 2: ln K2, 7: ln T or P
    ! XX vector 1: ln K1, 2: ln K2, 3: x1, 4: x2, 5: y1, 6: y2, 7: ln T or ln P
    ! Locals
    real, dimension(nc) :: K,Fv,Fl,FvT,FlT,FvP,FlP
    real, dimension(nc,nc) :: FvY,FlX
    real :: xd1, yd1

    ! Calculate variables for XX
    if (ispec == TSPEC) then ! T is specified
      P = exp(XX(7))
    else ! P is specified
      T = exp(XX(7))
    endif
    K(1:2) = exp(XX(1:2))
    x(1:2) = XX(3:4)
    y(1:2) = XX(5:6)

    ! Update thermodynamics
    xd1=x(1);yd1=y(1);
    if ( any(XX(3:6) < 0) ) then
      print *,'binaryXYfun: x or y < 0'
      print *,'XX',XX(3:6)
      FUN = 0; JAC = 0;x=0;y=0;
      return
    endif
    if (present(phase)) then
      call thermo(t,p,x,phase(1),Fl,FlT,FlP,FlX)
      call thermo(t,p,y,phase(2),Fv,FvT,FvP,FvY)
    else
      call thermo(t,p,x,LIQPH,Fl,FlT,FlP,FlX)
      call thermo(t,p,y,VAPPH,Fv,FvT,FvP,FvY)
    endif

    ! Evaluate function
    FUN(1:2) = -fl(1:2) + fv(1:2) + XX(1:2)
    FUN(3:4) = y - x * K
    FUN(5) = sum(x) - 1.0
    FUN(6) = sum(y) - 1.0
    FUN(7) = XX(ispecStep) - ln_spec

    ! Calculate Jacobian
    JAC = 0.0
    JAC(1,1) = 1.0
    JAC(2,2) = 1.0
    JAC(1:2,3:4) = -FlX
    JAC(1:2,5:6) = FvY
    if (ispec == TSPEC) then ! T is specified
      JAC(1:2,7) = (FvP - FlP)*P
    else ! P is specified
      JAC(1:2,7) = (FvT - FlT)*T
    endif

    JAC(3,1) = -K(1)*x(1)
    JAC(4,2) = -K(2)*x(2)
    JAC(3,3) = -K(1)
    JAC(4,4) = -K(2)
    JAC(3,5) = 1.0
    JAC(4,6) = 1.0

    JAC(5,3) = 1.0
    JAC(5,4) = 1.0

    JAC(6,5) = 1.0
    JAC(6,6) = 1.0

    JAC(7,ispecStep) = 1.0

  end subroutine binaryXYfun

  !----------------------------------------------------------------------
  subroutine fillX(XX,T,P,x,y,ispec,phase)
    ! Set up variable vector XX for the given specification
    use eos, only: thermo
    use thermopack_constants, only: LIQPH, VAPPH
    implicit none
    integer, intent(in) :: ispec
    real, intent(inout) :: T,P
    real, dimension(nc), intent(in) :: x,y
    real, dimension(neq), intent(out) :: XX
    integer, dimension(2), optional, intent(in) :: phase
    ! ispec = 1:T, 2:P
    ! XX vector 1: ln K1, 2: ln K2, 3: x1, 4: x2, 5: y1, 6: y2, 7: ln T or ln P
    ! Locals
    real, dimension(nc) :: Fv,Fl

    ! Update thermodynamics
    if (present(phase)) then
      call thermo(t,p,x,phase(1),Fl)
      call thermo(t,p,y,phase(2),Fv)
    else
      call thermo(t,p,x,LIQPH,Fl)
      call thermo(t,p,y,VAPPH,Fv)
    endif

    ! Calculate XX variables
    if (ispec == TSPEC) then ! T is specified
      XX(7) = log(P)
    else ! P is specified
      XX(7) = log(T)
    endif
    XX(1:2) = Fl - Fv
    XX(3) = x(1)
    XX(4) = x(2)
    XX(5) = y(1)
    XX(6) = y(2)

  end subroutine fillX

  !----------------------------------------------------------------------
  subroutine readX(XX,T,P,x,y,ispec)
    ! Set up T,P,x,y from variable vector XX
    implicit none
    integer, intent(in) :: ispec
    real, intent(inout) :: T,P
    real, dimension(nc), intent(out) :: x,y
    real, dimension(neq), intent(in) :: XX
    ! ispec = 1:T, 2:P
    ! XX vector 1: ln K1, 2: ln K2, 3: x1, 4: x2, 5: y1, 6: y2, 7: ln T or ln P

    if (ispec == TSPEC) then ! T is specified
      P = exp(XX(7))
    else ! P is specified
      T = exp(XX(7))
    endif
    x(1) = XX(3)
    x(2) = XX(4)
    y(1) = XX(5)
    y(2) = XX(6)
  end subroutine readX

  !-------------------------------------------------------------------
  !> Calculate binary three phase envelope
  !!
  !! \author MH, 2015-05
  !-------------------------------------------------------------------
  subroutine VLLEBinaryXY(T,P,ispec,Tmin,Pmax,dzmax,filename,dlns_max,&
       res,nRes,writeSingleFile,Pmin)
    use thermopack_constants, only: tpTmin, LIQPH, clen, VAPPH
    use thermopack_var, only: nc
    implicit none
    integer, intent(in) :: ispec
    real, intent(inout) :: T,P
    real, intent(in) :: Tmin,Pmax,dzmax
    character(len=*), intent(in) :: filename
    real, optional, intent(in) :: dlns_max
    real, optional, intent(out) :: res(9,maxpoints)
    integer, optional, intent(out) :: nRes(3)
    logical, optional, intent(in) :: writeSingleFile
    real, optional, intent(in) :: Pmin
    ! Internal variables
    logical :: hasThreePhaseLine, hasLLE
    logical :: isSuperCritical, isStable, isTrivial, isSolved
    real, dimension(nc) :: x_tpl,y_tpl,w_tpl
    integer :: phase(2), ispecStep
    real :: dlns
    real, dimension(maxpoints) :: xLLE, wLLE, tpLLE
    real, dimension(maxpoints) :: xL1VE, yL1VE, tpL1VE
    real, dimension(maxpoints) :: xL2VE, yL2VE, tpL2VE
    integer :: nLLE, nL1VE, nL2VE
    integer :: ierr, i, iterm
    real, parameter :: Pmin_default = 1.0e5
    real :: Pmin_actual, xx_tpl(3,2)
    real :: Tmin_sys
    ! Written for nc == 2
    if (.not. nc == 2) then
      call stoperror('threePhaseBinaryXY written for 2 components')
    endif

    if (present(Pmin)) then
      Pmin_actual = Pmin
    else
      Pmin_actual = Pmin_default
    endif

    if (ispec == TSPEC) then
      Tmin_sys = tpTmin
      tpTmin = 0.9*T
    endif
    nLLE = 0
    nL1VE = 0
    nL2VE = 0

    call threePhaseLine(T,P,x_tpl,y_tpl,w_tpl,hasThreePhaseLine,&
         hasLLE,ispec,Tmin,Pmax,Pmin_actual)

    if (hasThreePhaseLine) then
      call mapVLLEformTripleLine(ispec,T,P,&
           x_tpl,w_tpl,y_tpl,&
           xLLE,wLLE,tpLLE,nLLE,&
           xL1VE,yL1VE,tpL1VE,nL1VE,&
           xL2VE,yL2VE,tpL2VE,nL2VE,&
           Tmin,Pmax,dzmax,Pmin_actual,dlns_max)

    else if (hasLLE) then ! Only LLE exsist. Exitied at Pmin.
      ! MH: It is possible that LLE area terminates at critical point.
      ! This is currently not handled properly.....
      !
      ! Start stepping pressure or temperature
      ispecStep = 7
      phase = LIQPH
      dlns = 1.0e-3 ! Initial step
      call binaryXYarray(T,P,x_tpl,w_tpl,ispec,Tmin,Pmax,dzmax,&
           xLLE,wLLE,tpLLE,nLLE,ierr,dlns_max,phase,ispecStep,&
           dlns,Pmin_actual)
    else
      ! Start stepping pressure or temperature
      ispecStep = 7
      ! Map LV-binary plot
      call initial_point(T,P,x_tpl,y_tpl,ispec,isSuperCritical)
      if (P < Pmin_actual) then
        Pmin_actual = P
        if (present(Pmin)) then
          print *,"Reducing minimum pressure to include initial point"
        endif
      endif
      if (isSuperCritical) then
        iterm = BP_TERM_NONE
        print *,'map_binary_envelope: Both components supercritical and T is specified'
      else
        call binaryXYarray(T,P,x_tpl,y_tpl,ispec,Tmin,Pmax,dzmax,&
             xL1VE,yL1VE,tpL1VE,nL1VE,ierr,dlns_max,Pmin=Pmin_actual,&
             iterm=iterm)
      endif

      if (iterm == BP_TERM_TPD) then
        ! Detetcted additional phase
        ! Start search for LLVE-line
        phase = (/LIQPH,VAPPH/)
        x_tpl = (/xL1VE(nL1VE),1.0-xL1VE(nL1VE)/)
        y_tpl = (/yL1VE(nL1VE),1.0-yL1VE(nL1VE)/)
        if (ispec == TSPEC) then
          P=tpL1VE(nL1VE)
        else
          T=tpL1VE(nL1VE)
        endif
        isStable = binaryIsStable(x_tpl,y_tpl,T,P,phase,xx_tpl)
        x_tpl = xx_tpl(1,:)
        w_tpl = xx_tpl(2,:)
        y_tpl = xx_tpl(3,:)
        call LLVEline(T,P,x_tpl,w_tpl,y_tpl,ispec,ierr)
        if (ierr /= 0) then
          print *,"LLVE detected, failed to solve for LLVE line"
          print *,"ierr",ierr
          print *,"y",y_tpl
          print *,"x",x_tpl
          print *,"w",w_tpl
          print *,"p",P
          call stoperror('Not able to solve for LLVE composition')
        endif
        hasThreePhaseLine = .true.
        call mapVLLEformTripleLine(ispec,T,P,&
             x_tpl,w_tpl,y_tpl,&
             xLLE,wLLE,tpLLE,nLLE,&
             xL1VE,yL1VE,tpL1VE,nL1VE,&
             xL2VE,yL2VE,tpL2VE,nL2VE,&
             Tmin,Pmax,dzmax,Pmin_actual,dlns_max)
      else if (iterm /= BP_TERM_PMAX) then
        ! Test Pmax for LLE.
        if (ispec == TSPEC) then
          P = Pmax
          call initialComposition(T,P,x_tpl,w_tpl,isTrivial,isSolved,.true.)
          if (isSolved .and. .not. isTrivial) then
            ! Look for separated LLE region
            phase = VAPPH ! Set phases to vapor, as one of the phases
                          ! can drop into the vapor stable region
            dlns = 1.0e-3 ! Initial step
            call binaryXYarray(T,P,x_tpl,w_tpl,ispec,Tmin,Pmax,dzmax,&
                 xLLE,wLLE,tpLLE,nLLE,ierr,dlns_max,phase,ispecStep,&
                 dlns,Pmin_actual,-1.0)
          endif
        endif
      endif
    endif

    ! Dump to file
    if (present(res) .and. present(nRes)) then
      nRes(1) = nLLE
      do i=1,nLLE
        res(1,i) = xLLE(i)
        res(2,i) = wLLE(i)
        res(3,i) = tpLLE(i)
      enddo
      nRes(2) = nL1VE
      do i=1,nL1VE
        res(4,i) = xL1VE(i)
        res(5,i) = yL1VE(i)
        res(6,i) = tpL1VE(i)
      enddo
      nRes(3) = nL2VE
      do i=1,nL2VE
        res(7,i) = xL2VE(i)
        res(8,i) = yL2VE(i)
        res(9,i) = tpL2VE(i)
      enddo
    else
      call writeVLLEBinaryXY(ispec,T,P,filename,&
           x_tpl,w_tpl,y_tpl,hasThreePhaseLine,&
           xLLE,wLLE,tpLLE,nLLE,&
           xL1VE,yL1VE,tpL1VE,nL1VE,&
           xL2VE,yL2VE,tpL2VE,nL2VE,&
           writeSingleFile)
    endif

    ! Reset Tmin
    if (ispec == TSPEC) then
      tpTmin = Tmin_sys
    endif


  end subroutine VLLEBinaryXY

  subroutine mapVLLEformTripleLine(ispec,T,P,&
       x_tpl,w_tpl,y_tpl,&
       xLLE,wLLE,tpLLE,nLLE,&
       xL1VE,yL1VE,tpL1VE,nL1VE,&
       xL2VE,yL2VE,tpL2VE,nL2VE,&
       Tmin,Pmax,dzmax,Pmin,dlns_max)
    integer, intent(in) :: ispec
    real, intent(in) :: T,P
    real, intent(in), dimension(nc) :: x_tpl,y_tpl,w_tpl
    real, intent(out), dimension(maxpoints) :: xLLE, wLLE, tpLLE
    real, intent(out), dimension(maxpoints) :: xL1VE, yL1VE, tpL1VE
    real, intent(out), dimension(maxpoints) :: xL2VE, yL2VE, tpL2VE
    integer, intent(out) :: nLLE, nL1VE, nL2VE
    real, intent(in) :: Tmin,Pmax,dzmax,Pmin
    real, optional, intent(in) :: dlns_max
    ! Locals
    real :: dlns
    integer :: phase(2), ispecStep, ierr
    nLLE = 0
    nL1VE = 0
    nL2VE = 0

    ! Start stepping pressure or temperature
    ispecStep = 7

    ! L1L2E
    dlns = 1.0e-3 ! Initial step
    phase = LIQPH
    call binaryXYarray(T,P,x_tpl,w_tpl,ispec,Tmin,Pmax,dzmax,&
         xLLE,wLLE,tpLLE,nLLE,ierr,dlns_max,phase,ispecStep,&
         dlns,Pmin)

    ! L1VE
    phase(2) = VAPPH
    dlns = initialStep(T,P,x_tpl,y_tpl,w_tpl,ispec,ispecStep,phase)
    call binaryXYarray(T,P,x_tpl,y_tpl,ispec,Tmin,Pmax,dzmax,&
         xL1VE,yL1VE,tpL1VE,nL1VE,ierr,dlns_max,phase,ispecStep,&
         dlns,Pmin)

    ! L2VE
    dlns = initialStep(T,P,w_tpl,y_tpl,x_tpl,ispec,ispecStep,phase)
    call binaryXYarray(T,P,w_tpl,y_tpl,ispec,Tmin,Pmax,dzmax,&
         xL2VE,yL2VE,tpL2VE,nL2VE,ierr,dlns_max,phase,ispecStep,&
         dlns,Pmin)

  end subroutine mapVLLEformTripleLine

  subroutine writeVLLEBinaryXY(ispec,T,P,filename,&
       x_tpl,w_tpl,y_tpl,hasThreePhaseLine,&
       xLLE,wLLE,tpLLE,nLLE,&
       xL1VE,yL1VE,tpL1VE,nL1VE,&
       xL2VE,yL2VE,tpL2VE,nL2VE,&
       writeSingleFile)
    use thermopack_constants, only: clen
    use utilities, only: newunit
    integer, intent(in) :: ispec
    real, intent(in) :: T,P
    character(len=*), intent(in) :: filename
    logical, intent(in) :: hasThreePhaseLine
    logical, optional, intent(in) :: writeSingleFile
    real, intent(in), dimension(nc) :: x_tpl,y_tpl,w_tpl
    real, intent(in), dimension(maxpoints) :: xLLE, wLLE, tpLLE
    real, intent(in), dimension(maxpoints) :: xL1VE, yL1VE, tpL1VE
    real, intent(in), dimension(maxpoints) :: xL2VE, yL2VE, tpL2VE
    integer, intent(in) :: nLLE, nL1VE, nL2VE
    ! Internal variables
    logical :: wSF
    character(len=clen) :: basefilename, extfilename, fnameLLVline
    character(len=clen) :: fnameLLE, fnameL1V, fnameL2V
    integer :: ifile

    if (present(writeSingleFile)) then
      wSF = writeSingleFile
    else
      wSF = .false.
    endif

    if (wSF) then
      call mergeBinaryDataToFile(filename,ispec,T,P,xLLE,wLLE,tpLLE,nLLE,&
           xL1VE,yL1VE,tpL1VE,nL1VE,xL2VE,yL2VE,tpL2VE,nL2VE,&
           x_tpl(1),y_tpl(1),w_tpl(1))
    else
      ! File names:
      call getBaseFileName(filename,basefilename,extfilename)
      fnameLLVline = trim(basefilename) // '_LLVE' // trim(extfilename)
      fnameLLE = trim(basefilename) // '_LLE' // trim(extfilename)
      fnameL1V = trim(basefilename) // '_L1VE' // trim(extfilename)
      fnameL2V = trim(basefilename) // '_L2VE' // trim(extfilename)

      call dumpBinaryDataToFile(fnameLLE,ispec,T,P,xLLE,wLLE,tpLLE,nLLE)
      call dumpBinaryDataToFile(fnameL1V,ispec,T,P,xL1VE,yL1VE,tpL1VE,nL1VE)
      call dumpBinaryDataToFile(fnameL2V,ispec,T,P,xL2VE,yL2VE,tpL2VE,nL2VE)

      ! Write LLV line to file
      if (hasThreePhaseLine) then
        ifile = newunit()
        open(unit=ifile,file=trim(fnameLLVline))
        if (ispec == TSPEC) then
          write(ifile,*) '#Pxy plot: Temperature (K):',T
          write(ifile,*) '#x/w/y (-)'//char(9)//'P (Pa)'
          write(ifile,'(2es19.10e3)') min(x_tpl(1),w_tpl(1)),P
          write(ifile,'(2es19.10e3)') max(x_tpl(1),w_tpl(1)),P
          write(ifile,'(2es19.10e3)') y_tpl(1),P
        else
          write(ifile,*) '#Txy plot: Pressure (bar):',P*1.0e-5
          write(ifile,*) '#x/w/y (-)'//char(9)//'T (K)'
          write(ifile,'(2es19.10e3)') min(x_tpl(1),w_tpl(1)),T
          write(ifile,'(2es19.10e3)') max(x_tpl(1),w_tpl(1)),T
          write(ifile,'(2es19.10e3)') y_tpl(1),T
        endif
        close(ifile)
      endif
    endif

  contains
    subroutine getBaseFileName(filename,basefilename,extfilename)
      character(len=*), intent(in) :: filename
      character(len=*), intent(out) :: basefilename,extfilename
      ! Locals
      integer :: lfn
      basefilename = trim(filename)
      extfilename = '.dat'
      lfn = len(trim(filename))
      if (lfn > 4) then
        if (filename(lfn-3:lfn) == '.dat') then
          basefilename = filename(1:lfn-4)
          extfilename = '.dat'
        endif
      endif
    end subroutine getBaseFileName
  end subroutine writeVLLEBinaryXY

  !-------------------------------------------------------------------
  !> Find step direction for phase line
  !!
  !! \author MH, 2015-09
  !-------------------------------------------------------------------
  function initialStep(T0,P0,x0,y0,w0,ispec,ispecStep,phase) result(dlns)
    use thermopack_constants, only: LIQPH
    use thermopack_var, only: nc
    use stability, only: stabcalcW, stabilityLimit
    use eos, only: thermo
    implicit none
    integer, intent(in) :: ispec, ispecStep
    integer, dimension(2), intent(in) :: phase
    real, dimension(nc), intent(in) :: x0,y0,w0
    real, intent(in) :: T0,P0
    real :: dlns
    ! Internal variables
    real, dimension(nc) :: x,y,w
    real :: T,P,XX(neq),dXds(neq),Xvec(2,nc),FUGZ(nc),FUGW(nc)
    real :: dzmax,Pmax,Tmin,tpd
    integer :: iter,ierr,iss
    logical :: stab_negative
    T = T0
    P = P0
    x = x0
    y = y0
    w = w0
    dzmax = 1.0
    Pmax = P*2.0
    Tmin = T*0.5

    if (ispec == TSPEC) then
      ! Try increasing pressure
      dlns = 0.01
      call fillX(XX,T,P,x,y,ispec,phase)
      ! Extrapolate
      iss = ispecStep
      iter = binaryStep(XX,T,P,x,y,ispec,iss,dlns,&
             dzmax,Pmax,Tmin,phase,dXds,ierr)
      if (ierr == 0) then
        ! Test stabillity of w
        Xvec(1,:) = x
        Xvec(2,:) = y
        call thermo(t,p,x,LIQPH,FUGZ)
        tpd = stabcalcW(2,1,t,p,Xvec,W,LIQPH,FUGZ,FUGW)
        stab_negative = (tpd < stabilityLimit)
        if (stab_negative) then
          dlns = -0.001
        else
          dlns = 0.001
        endif
      else
        dlns = -0.001
      endif
    else
      dlns = 0.0
      call stoperror('PSPEC not yet supported')
    endif
  end function initialStep

  !-------------------------------------------------------------------
  !> Calculate three phase line
  !!
  !! \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine threePhaseLine(T,P,x,y,w,hasThreePhaseLine,hasLLE,&
       ispec,Tmin,Pmax,Pmin)
    ! Search for LLV line in binary plot
    ! ispec = 1:T is specified, 2:P is specified
    use thermopack_constants, only: LIQPH
    use thermopack_var, only: nc
    implicit none
    integer, intent(in) :: ispec
    real, intent(inout) :: T,P
    real, intent(in) :: Tmin,Pmax,Pmin
    real, dimension(nc), intent(out) :: x,y,w
    logical, intent(out) :: hasThreePhaseLine, hasLLE

    ! Internal variables
    real, parameter :: tpdlimit = -1.0e-8
    integer, parameter :: imax = 1000
    integer :: i, ierr, phase(2), ispecStep, iter
    real :: dlns, tpd, dzmax
    logical :: isTrivial, isSolved
    real, dimension(neq) :: dXds, XX

    ! Written for nc == 2
    if (.not. nc == 2) then
      call stoperror('threePhaseLine written for 2 components')
    endif

    hasThreePhaseLine = .false.
    hasLLE = .false.
    isTrivial = .false.
    phase = LIQPH
    ispecStep = 7
    dzmax = 0.003
    ! Calculate initial pressure or temperature
    call initialLLtp(T,P,ispec,ierr)
    if (  ierr == 0 .AND. &
         ((ispec == TSPEC .and. P < Pmax) .OR. &
         (ispec == PSPEC .and. T  > Tmin))) then
      call initialComposition(T,P,x,w,isTrivial,isSolved,.true.)
      hasLLE = (isSolved .and. .not. isTrivial)
    endif

    if (hasLLE) then
      ! Find vapor phase and adjust T/P
      if (ispec == TSPEC) then
        dlns = -0.01
        ispecStep = neq
      else
        dlns = 0.01
        ispecStep = neq-1
      endif
      call findVaporPhase(T,P,x,y)
      tpd = vaporTpd(T,P,x,y)
      call fillX(XX,T,P,x,w,ispec,phase)
      do i=1,imax
        if (tpd < tpdlimit) then
          exit
        endif
        ! Exit at Pmin
        if (abs(P - Pmin) < 1.0) then
          exit
        endif
        ! Limit to Pmin
        if (log(P) + dlns < log(Pmin)) then
          dlns = log(Pmin) - log(P)
        endif
        ! Solve LLE at new T/P
        iter = binaryStep(XX,T,P,x,w,ispec,ispecStep,dlns,&
             dzmax,Pmax,Tmin,phase,dXds,ierr)
        if (ierr == 0) then
          call findVaporPhase(T,P,x,y)
          tpd = vaporTpd(T,P,x,y)
        else
          ! No solution?
          print *,'threePhaseLine: LLE binaryStep failed'
          exit
        endif
      enddo
      if (tpd < tpdlimit) then
        call LLVEline(T,P,x,w,y,ispec,ierr)
        if (ierr == 0) then
          hasThreePhaseLine = .true.
        else
          call stoperror('Not able to solve for LLVE composition')
        endif
      endif
    endif

  end subroutine threePhaseLine

  !-------------------------------------------------------------------
  !> Find initial LL temperature/pressure
  !!
  !! \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine initialLLtp(T,P,ispec,ierr)
    use thermopack_var, only: nc
    use eos, only: thermo, getCriticalParam
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver,&
                                 NS_PEGASUS
    use saturation, only: safe_bubP
    use thermopack_constants, only: get_templimits
    use puresaturation, only: PureSat
    implicit none
    real, intent(inout) :: T,P
    integer, intent(in) :: ispec
    integer, intent(out) :: ierr
    ! Locals
    real, dimension(nc) :: x, w, y, tci, pci, oi
    real :: Pmax, Tmin, Tmax, P1, P2
    real, parameter :: safetyDt = 1.0e-4
    real, dimension(1) :: param
    type(nonlinear_solver) :: solver_psat
    integer :: ierrBub

    ierr = 0
    call getCriticalParam(1,tci(1),pci(1),oi(1))
    call getCriticalParam(2,tci(2),pci(2),oi(2))
    x = (/1.0,0.0/)
    w = (/0.0,1.0/)
    if (ispec == TSPEC) then
      if (T > min(tci(1),tci(2))) then
        ! No LLE
        ierr = 1
      else
        P1 = safe_bubP(T,x,y,ierrBub)
        if (ierrBub /= 0 .and. ierrBub /= 2) then
          ! Use PureSat as an approximation
          call PureSat(T,P1,x,.false.,ierr=ierrBub)
          if (ierrBub /= 0) then
            call stoperror("initialLLtp failed to converge safe_bubP")
          endif
        endif
        P2 = safe_bubP(T,w,y,ierrBub)
        if (ierrBub /= 0 .and. ierrBub /= 2) then
          ! Use PureSat as an approximation
          call PureSat(T,P2,w,.false.,ierr=ierrBub)
          if (ierrBub /= 0) then
            call stoperror("initialLLtp failed to converge safe_bubP")
          endif
        endif
        P = P1 + P2
      endif
    else !(ispec == PSPEC)
      if (tci(1) > tci(2)) then
        T = tci(2) - safetyDt
        Pmax = safe_bubP(T,w,y,ierrBub) + pci(1)
      else
        T = tci(1) - safetyDt
        Pmax = safe_bubP(T,x,y,ierrBub) + pci(2)
      endif
      if (ierrBub /= 0) then
        call stoperror("initialLLtp failed to converge safe_bubP")
      endif
      if (P > Pmax) then
        ! No LLE
        ierr = 1
      else
        solver_psat%abs_tol = 1.0e-5
        solver_psat%max_it = 1000
        solver_psat%isolver = NS_PEGASUS
        ! Set the constant parameters of the objective function.
        param(1) = P
        call get_templimits(Tmin,Tmax)
        ! Find f=0 inside the bracket.
        call bracketing_solver(Tmin,Tmax,fun_psat,T,solver_psat,param)
        ! Check for successful convergence
        if (solver_psat%exitflag /= 0) then
          ierr = solver_psat%exitflag
        endif
      endif
    endif
  end subroutine initialLLtp

  function fun_psat(T,param) result(f)
    ! Objective function for P = Psat_1(T) + Psat_2(T)
    !
    use thermopack_var, only: nc
    use saturation, only: safe_bubP
    implicit none
    ! Input:
    real,     intent(in)  :: T !< Temperature (K)
    real,     intent(in)  :: param(1) !< Parameter [Pressure (Pa)]
    ! Output:
    real                  :: f
    ! Internal:
    real                  :: x(nc),w(nc),y(nc),P
    ! Read from param
    P = param(1)
    x = (/1.0,0.0/)
    w = (/0.0,1.0/)
    f = (P - safe_bubP(T,x,y) - safe_bubP(T,w,y))/P
  end function fun_psat

  !-------------------------------------------------------------------
  !> Find initial LL composition
  !!
  !! \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine initialComposition(T,P,x,w,isTrivial,isSolved,useDefaultInit)
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    use eos, only: thermo
    use numconstants, only: machine_prec
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    implicit none
    real, intent(in) :: T,P
    real, dimension(nc), intent(inout) :: x,w
    logical, intent(out) :: isTrivial, isSolved
    logical, intent(in) :: useDefaultInit
    ! Locals
    real :: K(nc),lnfug_x(nc),lnfug_w(nc),K_old(nc)
    real, parameter :: rel_tol = machine_prec * 1.0e7
    integer :: i
    type(nonlinear_solver) :: solver
    real :: XX(4),XXmin(4),XXmax(4),param(2)
    ! Test
    !real :: G(4), G2(4), Jac(4,4)

    isSolved = .false.
    isTrivial = .false.

    ! Assume immiscible phases
    if (useDefaultInit) then
      x = (/0.3,0.7/)
      w = (/0.7,0.3/)
    endif

    call thermo(t,p,x,LIQPH,lnfug_x)
    call thermo(t,p,w,LIQPH,lnfug_w)
    K = exp(lnfug_x-lnfug_w)
    K_old = K
    ! Successive substitution
    do i=1,25
      if (abs(K(1)-K(2)) < 1.0e-6 .and.&
          abs(K(1)-1.0) < 1.0e-6) then
        isSolved = .true.
        isTrivial = .true.
        exit
      endif
      x(1) = (K(2) - 1.0)/(K(2) - K(1))
      x(2) = (1.0 - K(1))/(K(2) - K(1))
      w = K*x
      call thermo(t,p,x,LIQPH,lnfug_x)
      call thermo(t,p,w,LIQPH,lnfug_w)
      K = exp(lnfug_x-lnfug_w)
      if (sum(abs(K-K_old)) < rel_tol) then
        isSolved = .true.
        exit
      endif
      K_old = K
    enddo

    if (.not. isSolved) then ! Newton solver
      XXmin = 0.0
      XXmax = 1.0e100
      param(1) = T
      param(2) = P
      XX(1:2) = x
      XX(3:4) = w
      solver%rel_tol = 1.0e-20
      solver%abs_tol = 1.0e-10
      solver%limit_x_values = .true.
      solver%max_it = 50
      solver%ls_max_it = 5

      ! ! Test
      ! call LLComp_fun_newton(G,XX,param)
      ! call LLComp_diff_newton(Jac,XX,param)
      ! XX(4) = w(2) + 1.0e-5
      ! call LLComp_fun_newton(G2,XX,param)
      ! print *,(G2-G)/1.0e-5
      ! print *,Jac(:,4)
      ! stop
      call nonlinear_solve(solver,LLComp_fun_newton,&
           LLComp_diff_newton,LLComp_diff_newton,limit_dx,&
           premReturn,setXv,XX,XXmin,XXmax,param)

      if (solver%exitflag == 0) then
        x = XX(1:2)
        w = XX(3:4)
        isSolved = .true.
        if (maxval(abs(x-w)) < 1.0e-6) then
          isTrivial = .true.
        endif
      endif
    endif

  end subroutine initialComposition

  !-------------------------------------------------------------------
  !> LL-equilibrium function
  !>
  !> \author MH, 2015-09-05
  !-------------------------------------------------------------------
  subroutine LLComp_fun_newton(G,XX,param)
    use thermopack_constants, only: LIQPH
    use thermopack_var, only: nc
    use eos, only: thermo
    implicit none
    real, dimension(4), intent(out) :: G !< Function values
    real, dimension(4), intent(in) :: XX !< Variable vector
    real, dimension(2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: x,w,lnfug_x,lnfug_w,fugx,fugw
    real :: T, P

    T = param(1)
    P = param(2)
    x = XX(1:2)
    w = XX(3:4)
    call thermo(t,p,x,LIQPH,lnfug_x)
    fugx = exp(lnfug_x)
    call thermo(t,p,w,LIQPH,lnfug_w)
    fugw = exp(lnfug_w)
    G(1:2) = x*fugx - w*fugw
    G(3) = x(1) + x(2) - 1.0
    G(4) = w(1) + w(2) - 1.0
  end subroutine LLComp_fun_newton

  !-------------------------------------------------------------------
  !> LL-equilibrium differentials
  !>
  !> \author MH, 2015-09-05
  !-------------------------------------------------------------------
  subroutine LLComp_diff_newton(Jac,XX,param)
    use thermopack_constants, only: LIQPH
    use thermopack_var, only: nc
    use eos, only: thermo
    implicit none
    real, dimension(4,4), intent(out) :: Jac !< Function values
    real, dimension(4), intent(in) :: XX !< Variable vector
    real, dimension(2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: x,w,lnfug_x,lnfug_w,fugx,fugw
    real, dimension(nc,nc) :: dlnfug_x,dlnfug_w,dfugx,dfugw
    real :: T, P

    T = param(1)
    P = param(2)
    x = XX(1:2)
    w = XX(3:4)
    call thermo(t,p,x,LIQPH,lnfug_x,lnfugx=dlnfug_x)
    fugx = exp(lnfug_x)
    call thermo(t,p,w,LIQPH,lnfug_w,lnfugx=dlnfug_w)
    fugw = exp(lnfug_w)

    dfugx(1,:) = dlnfug_x(1,:)*fugx(1)
    dfugx(2,:) = dlnfug_x(2,:)*fugx(2)
    dfugw(1,:) = dlnfug_w(1,:)*fugw(1)
    dfugw(2,:) = dlnfug_w(2,:)*fugw(2)

    Jac(1,1) = fugx(1) + x(1)*dfugx(1,1)
    Jac(1,2) = x(1)*dfugx(1,2)
    Jac(1,3) = -fugw(1) - w(1)*dfugw(1,1)
    Jac(1,4) = - w(1)*dfugw(1,2)

    Jac(2,1) = x(2)*dfugx(2,1)
    Jac(2,2) = fugx(2) +x(2)*dfugx(2,2)
    Jac(2,3) =  - w(2)*dfugw(2,1)
    Jac(2,4) = -fugw(2) - w(2)*dfugw(2,2)

    Jac(3,1) = 1.0
    Jac(3,2) = 1.0
    Jac(3,3) = 0.0
    Jac(3,4) = 0.0

    Jac(4,1) = 0.0
    Jac(4,2) = 0.0
    Jac(4,3) = 1.0
    Jac(4,4) = 1.0
  end subroutine LLComp_diff_newton

  !-------------------------------------------------------------------
  !> Solve for LLE or VLE using NR solver
  !! Extrapolate from last solution using ds
  !! \author MH, 2015-03
  !-------------------------------------------------------------------
  function binaryStep(XX,T,P,x,w,ispec,ispecStep,dlns,&
       dzmax,Pmax,Tmin,phase,dXdS,ierr,Pmin) result(iter)
    use thermopack_constants, only: tpPmax, tpPmin, get_templimits
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    use thermopack_var, only: nc
    implicit none
    integer, intent(in) :: ispec
    integer, intent(inout) :: ispecStep
    real, intent(inout) :: T,P,dlns
    real, intent(in) :: Tmin,Pmax,dzmax
    real, dimension(nc), intent(inout) :: x,w
    integer, dimension(2), intent(in) :: phase
    integer, intent(out) :: ierr
    real, optional, intent(in) :: Pmin ! Override minimum pressure (Pa)
    real, dimension(neq), intent(out) :: dXdS
    real, dimension(neq), intent(inout) :: XX
    integer :: iter
    ! Locals
    type(nonlinear_solver) :: solver
    real, dimension(6) :: param
    real, dimension(neq) :: XXmax, XXmin, XXold
    real :: alpha
    real, parameter :: tuning = 1.2, eps_dlns = 1.0e-7
    integer :: imax, i

    iter = 0
    if (abs(dlns) > 0.0) then
      XXold = XX
      ! Extrapolate old solution
      call newton_extrapolate(T,P,XX,dXdS,ispec,ispecStep,phase,imax)
      if (imax /= ispecStep) then
        ispecStep = imax
        ! Rescaling the sensitivities
        dlns = dlns*sign(1.0,dXdS(ispecStep))
        dXdS = dXdS / dXdS(ispecStep)
      endif
      XX = XXold + dXdS*dlns
      ! Proactive wrt. dzmax
      if (maxval(abs(XX(3:6)-XXold(3:6))) > dzmax) then
        ! Reduse dlns to restrict linear extrapolation step to dzmax
        alpha = 1.0
        do i=3,6
          if (abs(XX(i)-XXold(i)) > dzmax) then
            alpha = min(alpha,dzmax/abs(dXdS(i)*dlns))
          endif
        enddo
        alpha = max(alpha,0.05) ! Avoid doing to small steps
        dlns = dlns*alpha
        XX = XXold + dXdS*dlns
        if (verbose) then
          print *,'Redusing to meet dzmax. alpha = ',alpha
        endif
      endif
      if (minval(XX(3:6)) <= 0.0) then ! Don't allow negative compositions
        alpha = 1.0
        do i=3,6
          if (XX(i) < 0.0) then
            alpha = min(alpha,-XXold(i)/(dXdS(i)*dlns))
          endif
        enddo
        alpha = alpha*(1.0-1.0e-6)
        dlns = dlns*alpha
        XX = XXold + dXdS*dlns
        if (verbose) then
          print *,'Do not allow negative compositions. alpha = ',alpha
        endif
        if (dlns < eps_dlns) then
          ierr = -2
          if (verbose) then
            print *,'Too small step. Solve for single component.'
          endif
          return
        endif
      endif
    else
      dXdS = 0.0
    endif

    XXmin = -1.0e100
    XXmax = 1.0e100
    XXmin(3:6) = 0.0 ! Positive mol number
    if (ispec == TSPEC) then
      if (present(Pmin)) then
        XXmin(neq) = log(max(tpPmin,Pmin)) !Pmin
      else
        XXmin(neq) = log(tpPmin) !Pmin
      endif
      XXmax(neq) = log(min(tpPmax,Pmax)) !Pmax
    else
      call get_templimits(XXmin(neq),XXmax(neq))
      XXmin(neq) = log(max(XXmin(neq),Tmin)) !Tmin
      XXmax(neq) = log(XXmax(neq)) !Tmax
    endif
    ! Validate initial values
    if (XX(neq)<=XXmin(neq)+1.0e-5 .or. XX(neq)>=XXmax(neq)-1.0e-5) then
      ierr = -1 ! Return and solve for limit value
      return
    endif
    call setBinaryStepParam(XX,T,P,ispec,ispecStep,phase,param)

    solver%rel_tol = 1.0e-20
    solver%abs_tol = binaryAbsTol
    solver%limit_x_values = .true.
    solver%max_it = binaryMaxIt
    solver%ls_max_it = 5
    call nonlinear_solve(solver,binary_fun_newton,&
         binary_diff_newton,binary_diff_newton,limit_dx,&
         premReturn,setXv,XX,XXmin,XXmax,param,neq)
    ierr = solver%exitflag
    iter = solver%iter
    if (solver%exitflag == 0) then
      call readX(XX,T,P,x,w,ispec)
      ! Update dXds
      call newton_extrapolate(T,P,XX,dXdS,ispec,ispecStep,phase,imax)
    endif
    if (solver%exitflag /= 0) then
      ierr = solver%exitflag
    else
      if (XX(neq)<=XXmin(neq)+1.0e-5 .or. XX(neq)>=XXmax(neq)-1.0e-5) then
        ierr = -1
      else
        ierr = 0
      endif
    endif

  end function binaryStep

  subroutine setBinaryStepParam(XX,T,P,ispec,ispecStep,phase,param)
    implicit none
    real, intent(in) :: T,P
    integer, intent(in) :: ispec
    integer, intent(in) :: ispecStep
    integer, dimension(2), intent(in) :: phase
    real, dimension(neq), intent(in) :: XX
    ! Locals
    real, dimension(6), intent(out) :: param
    if (ispec == TSPEC) then
      param(1) = T
    else
      param(1) = P
    endif
    param(2) = real(ispec)
    param(3) = real(ispecStep)
    param(4) = XX(ispecStep)
    param(5) = real(phase(1))
    param(6) = real(phase(2))
  end subroutine setBinaryStepParam

  subroutine testJacobian(XX,T,P,ispec,ispecStep,phase)
    implicit none
    integer, intent(in) :: ispec
    integer, intent(in) :: ispecStep
    real, intent(in) :: T,P
    integer, dimension(2), intent(in) :: phase
    real, dimension(neq), intent(in) :: XX
    ! Locals
    real, dimension(6) :: param
    real, dimension(neq) :: XX2, G, G2
    real, dimension(neq,neq) :: Jac
    integer :: i
    real :: dXX, eps
    call setBinaryStepParam(XX,T,P,ispec,ispecStep,phase,param)

    call binary_diff_newton(Jac,XX,param)
    call binary_fun_newton(G,XX,param)

    print *,"ispecStep",ispecStep
    print *,"ln K1=",XX(1)
    print *,"ln K2=",XX(2)
    print *,"   x1=",XX(3)
    print *,"   x2=",XX(4)
    print *,"   y1=",XX(5)
    print *,"   y2=",XX(6)
    print *,"ln TP=",XX(7)
    eps = 1.0e-6
    do i=1,neq
      XX2 = XX
      dXX = XX2(i)*eps
      XX2(i) = XX2(i) + dXX
      call binary_fun_newton(G2,XX2,param)
      print *,i,dXX
      print *,"NUMERICAL : ",(G2-G)/dXX
      print *,"ANALYTICAL: ",Jac(:,i)
    enddo
  end subroutine testJacobian

  !-------------------------------------------------------------------
  !> Saturation line function values for non-linear solver
  !>
  !> \author MH, 2012-03-05
  !-------------------------------------------------------------------
  subroutine binary_fun_newton(G,XX,param)
    use thermopack_var, only: nc
    implicit none
    real, dimension(neq), intent(out) :: G !< Function values
    real, dimension(neq), intent(in) :: XX !< Variable vector
    real, dimension(6) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: x,w
    real, dimension(neq,neq) :: Jac
    integer :: phase(2), ispec, ispecStep
    real :: ln_spec, T, P

    ispec = int(param(2))
    ispecStep = int(param(3))
    ln_spec = param(4)
    phase(1) = int(param(5))
    phase(2) = int(param(6))
    if (ispec == TSPEC) then
      T = param(1)
    else
      P = param(1)
    endif

    call binaryXYfun(XX,T,P,x,w,ispec,ispecStep,ln_spec,G,Jac,phase)

  end subroutine binary_fun_newton

  !-------------------------------------------------------------------
  !> Differentials for saturation line function values for non-linear solver
  !> 
  !> \author MH, 2012-03-05
  !-------------------------------------------------------------------
  subroutine binary_diff_newton(Jac,XX,param)
    use thermopack_var, only: nc
    implicit none
    real, dimension(neq), intent(in) :: XX !< Variable vector
    real, dimension(neq,neq), intent(out) :: Jac !< Function differentials
    real, dimension(6) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: x,w
    real, dimension(neq) :: G
    integer :: phase(2), ispec, ispecStep
    real :: ln_spec, T, P

    ispec = int(param(2))
    ispecStep = int(param(3))
    ln_spec = param(4)
    phase(1) = int(param(5))
    phase(2) = int(param(6))
    if (ispec == TSPEC) then
      T = param(1)
    else
      P = param(1)
    endif

    call binaryXYfun(XX,T,P,x,w,ispec,ispecStep,ln_spec,G,Jac,phase)

  end subroutine binary_diff_newton

  !-------------------------------------------------------------------
  !> Find variable sensitivities for binary line
  !>
  !> \author MH, 2015-03
  !-------------------------------------------------------------------
  subroutine newton_extrapolate(T,P,XX,dXdS,ispec,ispecStep,phase,imax)
    implicit none
    real, intent(in) :: T, P
    real, dimension(neq), intent(out) :: dXdS
    real, dimension(neq), intent(in) :: XX
    integer, dimension(2), intent(in) :: phase
    integer, intent(in) :: ispec, ispecStep
    integer, intent(out) :: imax
    ! Locals
    real, dimension(neq,neq) :: Jac
    integer, dimension(neq) :: INDX
    integer :: INFO
    real, dimension(6) :: param
    real :: smax

    if (ispec == TSPEC) then
      param(1) = T
    else
      param(1) = P
    endif
    param(2) = real(ispec)
    param(3) = real(ispecStep)
    param(4) = XX(ispecStep)
    param(5) = real(phase(1))
    param(6) = real(phase(2))

    call binary_diff_newton(Jac,XX,param)
    dXdS(1:neq-1) = 0.0
    dXdS(neq) = 1.0

    ! Solve equation system
    call DGESV( neq, 1, Jac, neq, INDX, dXdS, neq, INFO )

    imax = 1
    smax = abs(dXdS(1))
    if (abs(dXdS(2)) > smax) then
      imax = 2
      smax = abs(dXdS(2))
    endif
    if (abs(dXdS(7)) > smax) then
      imax = 7
    endif
  end subroutine newton_extrapolate

  !-------------------------------------------------------------------
  !> Solve for vapor phase in equilibrium with liquid phase (x)
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine findVaporPhase(T,P,x,y)
    use eos, only: thermo
    use numconstants, only: machine_prec
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    implicit none
    real, dimension(nc), intent(out) :: y !< Vapor composition
    real, dimension(nc), intent(in) :: x !< Liquid composition
    real, intent(in) :: T,P !< Variable vector
    ! Locals
    real, dimension(nc) :: fug_x, lnfug_x, lnfug_y, lnX, lnY, lnYold
    real, dimension(nc) :: Ymin, Ymax
    real, dimension(2*nc+2) :: param
    real :: error
    real, parameter :: eps = machine_prec*1000.0
    type(nonlinear_solver) :: solver
    integer :: i
    ! Test
    !real :: G(2), G2(2), Y2(2), Jac(2,2)

    call thermo(t,p,x,LIQPH,lnfug_x)
    fug_x = exp(lnfug_x)
    y(1) = x(1)*fug_x(1)/(x(1)*fug_x(1)+x(2)*fug_x(2))
    y(2) = 1.0 - y(1)
    ! Successive substitition
    lnX = log(x)
    lnYold = log(Y)
    do i=1,5
      call thermo(t,p,y,VAPPH,lnfug_y)
      lnY = lnX + lnfug_x - lnfug_y
      error = maxval(abs(lnY-lnYold))
      if (error < eps) then
        exit
      endif
      lnYold = lnY
      y = exp(lnY)
      y = y/sum(y)
    enddo
    if (error >= eps) then
      solver%rel_tol = 1.0e-20
      solver%abs_tol = 1.0e-12
      solver%limit_x_values = .true.
      solver%max_it = 50
      solver%ls_max_it = 5
      param(1:nc) = lnX
      param(nc+1:2*nc) = lnfug_x
      Ymin = 0.0
      Ymax = 1.0e100
      param(2*nc+1) = T
      param(2*nc+2) = P

      ! ! Test
      ! print *,'Testing vapour phase solver'
      ! call vapor_fun_newton(G,y,param)
      ! call vapor_diff_newton(Jac,y,param)
      ! Y2 = Y
      ! Y2(1) = Y2(1) + 1.0e-5
      ! call vapor_fun_newton(G2,y2,param)
      ! print *,(G2-G)/1.0e-5
      ! print *,Jac(1,:)
      ! Y2 = Y
      ! Y2(2) = Y2(2) + 1.0e-5
      ! call vapor_fun_newton(G2,y2,param)
      ! print *,(G2-G)/1.0e-5
      ! print *,Jac(2,:)
      ! stop
      call nonlinear_solve(solver,vapor_fun_newton,vapor_diff_newton,&
           vapor_diff_newton,limit_dx,premReturn,setXv,Y,Ymin,Ymax,param)
      if (solver%exitflag /= 0) then
        call stoperror('Not able to solve for vapor phase')
      endif
    endif
  end subroutine findVaporPhase

  !-------------------------------------------------------------------
  !> Vapor phase function values for non-linear solver
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine vapor_fun_newton(G,y,param)
    use thermopack_constants, only: VAPPH
    use thermopack_var, only: nc
    use eos, only: thermo
    implicit none
    real, dimension(nc), intent(out) :: G !< Function values
    real, dimension(nc), intent(in) :: y !< Variable vector
    real, dimension(2*nc+2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: lnX, lnfug_x, lnfug_y, lnY
    real :: T, P

    lnX = param(1:nc)
    lnfug_x = param(nc+1:2*nc)
    T = param(2*nc+1)
    P = param(2*nc+2)
    call thermo(t,p,y,VAPPH,lnfug_y)
    lnY = log(y)
    G = -lnX - lnfug_x + lnfug_y + lnY
  end subroutine vapor_fun_newton

  !-------------------------------------------------------------------
  !> Vapor phase function values for non-linear solver
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine vapor_diff_newton(Jac,Y,param)
    use thermopack_constants, only: VAPPH
    use thermopack_var, only: nc
    use eos, only: thermo
    implicit none
    real, dimension(nc), intent(in) :: Y !< Variable vector
    real, dimension(nc,nc), intent(out) :: Jac !< Function differentials
    real, dimension(2*nc+2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: lnfug_y
    real :: T, P

    T = param(2*nc+1)
    P = param(2*nc+2)
    call thermo(t,p,y,VAPPH,lnfug_y,lnfugx=Jac)
    Jac(1,1) = Jac(1,1) + 1.0/y(1)
    Jac(2,2) = Jac(2,2) + 1.0/y(2)

  end subroutine vapor_diff_newton

  !--------------------------------------------------------------------
  !> Tangent plane distance for vapor phase
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  function vaporTpd(T,P,x,y) result(tpd)
    use eos, only: thermo
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    implicit none
    real, dimension(nc), intent(in) :: y !< Vapor composition
    real, dimension(nc), intent(in) :: x !< Liquid composition
    real, intent(in) :: T,P !< Variable vector
    real :: tpd
    ! Locals
    real, dimension(nc) :: lnfug_x, lnfug_y, D

    call thermo(t,p,x,LIQPH,lnfug_x)
    D =log(x)+lnfug_x
    call thermo(t,p,y,VAPPH,lnfug_y)
    tpd = 1.0 + sum(y*(log(y) + lnfug_y - D - 1.0))
  end function vaporTpd

  !-------------------------------------------------------------------
  !> Solve for three-phase line using good initialguess
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine LLVEline(T,P,x,w,y,ispec,ierr)
    use numconstants, only: machine_prec
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    use thermopack_constants, only: tpPmax, tpPmin, get_templimits
    use thermopack_var, only: nc
    implicit none
    real, dimension(nc), intent(inout) :: x,y,w !< Phase compositions
    real, intent(inout) :: T,P !<
    integer, intent(in) :: ispec !<
    integer, intent(out) :: ierr !< Error flag
    ! Locals
    real, dimension(2) :: param
    real, dimension(3*nc+1) :: XX,XXmin,XXmax
    !real, parameter :: eps = machine_prec*1000.0
    type(nonlinear_solver) :: solver
    ! Test
    !real :: G(7), G2(7), Jac(7,7)
    !integer :: i

    XX(1:nc) = x
    XX(nc+1:2*nc) = w
    XX(2*nc+1:3*nc) = y
    XXmin = 0.0
    XXmax = 1.0e100
    if (ispec == TSPEC) then
      param(1) = T
      XXmax(3*nc+1) = log(tpPmax)
      XXmin(3*nc+1) = log(tpPmin)
      XX(3*nc+1) = log(P)
    else ! PSPEC
      param(1) = P
      call get_templimits(XXmin(3*nc+1),XXmax(3*nc+1))
      XX(3*nc+1) = log(T)
      XXmin(3*nc+1) = log(XXmin(3*nc+1))
      XXmax(3*nc+1) = log(XXmax(3*nc+1))
    endif
    param(2) = real(ispec)
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-12
    solver%limit_x_values = .true.
    solver%max_it = 50
    solver%ls_max_it = 3

    ! ! Test
    ! print *,'Testing llve line solver!'
    ! call llve_fun_newton(G,XX,param)
    ! call llve_diff_newton(Jac,XX,param)
    ! i = 6
    ! XX(i) = XX(i) + 1.0e-5
    ! call llve_fun_newton(G2,XX,param)
    ! print *,(G2-G)/1.0e-5
    ! print *,Jac(:,i)
    ! stop

    call nonlinear_solve(solver,llve_fun_newton,llve_diff_newton,&
         llve_diff_newton,limit_dx,premReturn,setXv,XX,XXmin,XXmax,param)
    x = XX(1:nc)
    w = XX(nc+1:2*nc)
    y = XX(2*nc+1:3*nc)
    if (ispec == TSPEC) then
      P = exp(XX(3*nc+1))
    else ! PSPEC
      T = exp(XX(3*nc+1))
    endif
    ierr = solver%exitflag

  end subroutine LLVEline

  !-------------------------------------------------------------------
  !> Vapor phase function values for non-linear solver
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine llve_fun_newton(G,XX,param)
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    use eos, only: thermo
    implicit none
    real, dimension(3*nc+1), intent(out) :: G !< Function values
    real, dimension(3*nc+1), intent(in) :: XX !< Variable vector
    real, dimension(2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: lnfug_x, lnfug_y, lnfug_w, lnX, lnW, lnY, x, y, w
    real :: T, P
    integer :: ispec

    ispec = int(param(2))
    if (ispec == TSPEC) then
      T = param(1)
      P = exp(XX(3*nc+1))
    else ! PSPEC
      P = param(1)
      T = exp(XX(3*nc+1))
    endif
    x = XX(1:nc)
    w = XX(nc+1:2*nc)
    y = XX(2*nc+1:3*nc)

    call thermo(t,p,x,LIQPH,lnfug_x)
    call thermo(t,p,w,LIQPH,lnfug_w)
    call thermo(t,p,y,VAPPH,lnfug_y)

    lnX = log(x)
    lnW = log(w)
    lnY = log(y)
    G(1:nc) = lnX + lnfug_x - lnfug_y - lnY
    G(nc+1:2*nc) = lnW + lnfug_w - lnfug_y - lnY
    G(2*nc+1) = sum(x) - 1.0
    G(2*nc+2) = sum(w) - 1.0
    G(2*nc+3) = sum(y) - 1.0
  end subroutine llve_fun_newton

  !-------------------------------------------------------------------
  !> Vapor phase function values for non-linear solver
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine llve_diff_newton(Jac,XX,param)
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    use eos, only: thermo
    implicit none
    real, dimension(3*nc+1), intent(in) :: XX !< Variable vector
    real, dimension(3*nc+1,3*nc+1), intent(out) :: Jac !< Function differentials
    real, dimension(2) :: param !< Parameter vector
    ! Locals
    real, dimension(nc,nc) :: lnfug_xn, lnfug_wn, lnfug_yn
    real, dimension(nc) :: lnfug_xT, lnfug_wT, lnfug_yT
    real, dimension(nc) :: lnfug_xP, lnfug_wP, lnfug_yP
    real, dimension(nc) :: lnfug_x, lnfug_y, lnfug_w, x, y, w
    real :: T, P
    integer :: ispec

    ispec = int(param(2))
    if (ispec == TSPEC) then
      T = param(1)
      P = exp(XX(3*nc+1))
    else ! PSPEC
      P = param(1)
      T = exp(XX(3*nc+1))
    endif
    x = XX(1:nc)
    w = XX(nc+1:2*nc)
    y = XX(2*nc+1:3*nc)

    call thermo(t,p,x,LIQPH,lnfug_x,lnfugT=lnfug_xT,&
         lnfugP=lnfug_xP,lnfugx=lnfug_xn)
    call thermo(t,p,w,LIQPH,lnfug_w,lnfugT=lnfug_wT,&
         lnfugP=lnfug_wP,lnfugx=lnfug_wn)
    call thermo(t,p,y,VAPPH,lnfug_y,lnfugT=lnfug_yT,&
         lnfugP=lnfug_yP,lnfugx=lnfug_yn)

    Jac = 0.0

    Jac(1:nc,1:nc) = lnfug_xn
    Jac(1,1) = Jac(1,1) + 1.0/x(1)
    Jac(2,2) = Jac(2,2) + 1.0/x(2)
    Jac(1:nc,2*nc+1:3*nc) = -lnfug_yn
    Jac(1,2*nc+1) = Jac(1,2*nc+1) - 1.0/y(1)
    Jac(2,3*nc) = Jac(2,3*nc) - 1.0/y(2)

    Jac(nc+1:2*nc,nc+1:2*nc) = lnfug_wn
    Jac(nc+1,nc+1) = Jac(nc+1,nc+1) + 1.0/w(1)
    Jac(2*nc,2*nc) = Jac(2*nc,2*nc) + 1.0/w(2)
    Jac(nc+1:2*nc,2*nc+1:3*nc) = -lnfug_yn
    Jac(nc+1,2*nc+1) = Jac(nc+1,2*nc+1) - 1.0/y(1)
    Jac(2*nc,3*nc) = Jac(2*nc,3*nc) - 1.0/y(2)

    if (ispec == TSPEC) then
      Jac(1:nc,3*nc+1) = P*(lnfug_xP - lnfug_yP)
      Jac(nc+1:2*nc,3*nc+1) = P*(lnfug_wP - lnfug_yP)
    else ! PSPEC
      Jac(1:nc,3*nc+1) = T*(lnfug_xT - lnfug_yT)
      Jac(nc+1:2*nc,3*nc+1) = T*(lnfug_wT - lnfug_yT)
    endif

    Jac(2*nc+1,1) = 1.0
    Jac(2*nc+1,2) = 1.0
    Jac(2*nc+2,3) = 1.0
    Jac(2*nc+2,4) = 1.0
    Jac(2*nc+3,5) = 1.0
    Jac(2*nc+3,6) = 1.0

  end subroutine llve_diff_newton

  !-----------------------------------------------------------------!
  subroutine map_binary_envelope(T0,P0,Tmin,Pmax,dzmax,spec,&
       outfile,dlns_max,plotType,writeSingleFile,Pmin,calcvolumes)
  !-----------------------------------------------------------------!
    use eos, only: getCriticalParam
    use saturation, only: safe_bubP, safe_bubT
    use thermopack_constants, only: clen, BINARY_PL
    use thermopack_var, only: nc
    implicit none
    !
    real, intent(in)                          :: T0       !< Temperature (K)
    real, intent(in)                          :: P0       !< Pressure (Pa)
    character(len=1), intent(in)              :: spec     !< Specification ('T' or 'P')
    real, intent(in)                          :: Tmin     !< Feed compsition
    real, intent(in)                          :: Pmax     !< Maximum pressure (Pa)
    real, intent(in)                          :: dzmax    !< Maximum change in composition between steps (-)
    character(len=*), optional, intent(in)    :: outfile  !< File name
    real, optional, intent(in)                :: dlns_max !< Maximum extrapolation step in binary mapping
    integer, optional, intent(in)             :: plotType !< BINARY_PL or BINARY_VLLE_PL
    logical, optional, intent(in)             :: writeSingleFile !<
    real, optional, intent(in)                :: Pmin !< Optional minimum pressure (Pa)
    logical, optional, intent(in)             :: calcvolumes !< Calculate volumes
    ! Locals
    real :: T,P
    integer :: ispec, plt
    real, dimension(nc) :: x,y
    character(len=clen) :: ofile
    logical :: isSuperCritical

    if (spec == 'P') then
      ofile = 'binaryPxy'
      ispec = PSPEC
    else if (spec == 'T') then
      ofile = 'binaryTxy'
      ispec = TSPEC
    else
      call StopError('map_binary_envelope: wrong specification!')
    endif

    if (present(plotType)) then
      plt = plotType
    else
      plt = BINARY_PL
    endif

    T = T0
    P = P0
    if (plt == BINARY_PL) then
      call initial_point(T,P,x,y,ispec,isSuperCritical)
      if (isSuperCritical) then
        print *,'map_binary_envelope: Both components supercritical and T is specified'
        return
      endif

      if (present(outfile)) then
        ofile = outfile
      else
        ofile = trim(ofile) // '.dat'
      endif

      call binaryXY(T,P,x,y,ispec,Tmin,Pmax,dzmax,ofile,dlns_max,Pmin=Pmin,calcvolumes=calcvolumes)

    else

      if (present(outfile)) then
        ofile = outfile
      endif
      call VLLEBinaryXY(T,P,ispec,Tmin,Pmax,dzmax,ofile,&
           dlns_max,writeSingleFile=writeSingleFile,Pmin=Pmin)
    endif

  end subroutine map_binary_envelope

  !-----------------------------------------------------------------!
  subroutine binaryPxy(T,Pmax,dzmax,outfile,dlns_max,Pmin)
  !-----------------------------------------------------------------!
    implicit none
    !
    real, intent(in)                          :: T        !< Temperature (K)
    real, intent(in)                          :: Pmax     !< Maximum pressure (Pa)
    real, intent(in)                          :: dzmax    !< Maximum change in composition between steps (-)
    character(len=*), optional, intent(in)    :: outfile  !< File name
    real, optional, intent(in)                :: dlns_max !< Maximum extrapolation step in binary mapping
    real, optional, intent(in)                :: Pmin !< Optional minimum pressure (Pa)
    ! Locals
    real :: P,Tmin
    P = 1.0e5
    Tmin = 200.0
    call map_binary_envelope(T,P,Tmin,Pmax,dzmax,'T',outfile,dlns_max,Pmin=Pmin)
  end subroutine binaryPxy

  !-----------------------------------------------------------------!
  subroutine binaryTxy(P,Tmin,dzmax,outfile,dlns_max)
  !-----------------------------------------------------------------!
    implicit none
    !
    real, intent(in)                          :: Tmin     !< Minimu temperature (K)
    real, intent(in)                          :: P        !< Pressure (Pa)
    real, intent(in)                          :: dzmax    !< Maximum change in composition between steps (-)
    character(len=*), optional, intent(in)    :: outfile  !< File name
    real, optional, intent(in)                :: dlns_max !< Maximum extrapolation step in binary mapping
    ! Locals
    real :: T,Pmax
    Pmax = 1.0e5
    T = 200.0
    call map_binary_envelope(T,P,Tmin,Pmax,dzmax,'P',outfile,dlns_max)
  end subroutine binaryTxy

  !-----------------------------------------------------------------!
  subroutine initial_point(T,P,x,y,ispec,isSuperCritical)
  !-----------------------------------------------------------------!
    use eos, only: getCriticalParam
    use saturation, only: safe_bubP, safe_bubT
    use thermopack_var, only: nc
    implicit none
    !
    real, intent(inout)                       :: T       !< Temperature (K)
    real, intent(inout)                       :: P       !< Pressure (Pa)
    integer, intent(in)                       :: ispec   !< Specification TSPEC/ISPEC
    real, dimension(nc), intent(out)          :: x,y     !< Initial composition
    logical, intent(out)                      :: isSuperCritical
    ! Locals
    integer, dimension(1) :: imax,imin
    real, dimension(nc) :: tci,pci,oi
    integer :: ierr

    isSuperCritical = .false.
    call getCriticalParam(1,tci(1),pci(1),oi(1))
    call getCriticalParam(2,tci(2),pci(2),oi(2))
    if (ispec == PSPEC) then
      imax = maxloc(pci)
      imin = minloc(pci)
      if (P > pci(imax(1))) then
        ! Critical
        call stoperror('map_binary_envelope: To be implemented. Both components supercritical and P is specified')
      else
        if (P > pci(imin(1))) then
          x(imin(1)) = 0.0
          x(imax(1)) = 1.0
        else
          x(imin(1)) = 1.0
          x(imax(1)) = 0.0
        endif
        y = x
        T = safe_bubT(P,x,y,ierr)
        if (ierr /= 0) then
          call stoperror("binaryplot::initial_point failed to "//&
               "calculate pure component saturtion (safe_bubT)")
        endif
      endif
    else if (ispec == TSPEC) then
      imax = maxloc(tci)
      imin = minloc(tci)
      if (T > tci(imax(1))) then
        ! Critical
        isSuperCritical = .true.
      else
        if (T > tci(imin(1))) then
          x(imin(1)) = 0.0
          x(imax(1)) = 1.0
        else
          x(imin(1)) = 1.0
          x(imax(1)) = 0.0
        endif
        y = x
        p = safe_bubP(T,x,y,ierr)
        if (ierr /= 0) then
          call stoperror("binaryplot::initial_point failed to "//&
               "calculate pure component saturtion (safe_bubP)")
        endif
      endif
    else
      call StopError('map_binary_envelope: wrong specification!')
    endif

  end subroutine initial_point

  !-------------------------------------------------------------------
  !> Determine initial step sign
  !!
  !! \author MH, 2015-11
  !-------------------------------------------------------------------
  function initialStepSign(T,P,XX,ispec,ispecStep,phase) result(sgn)
    implicit none
    real, dimension(neq), intent(in) :: XX
    integer, intent(in) :: ispec
    integer, intent(in) :: ispecStep
    real, intent(in) :: T,P
    integer, dimension(2), intent(in) :: phase
    real :: sgn
    ! Locals
    real, dimension(neq) :: dXdS
    integer :: imax

    ! Extrapolate old solution
    call newton_extrapolate(T,P,XX,dXdS,ispec,ispecStep,phase,imax)
    if ( (XX(3) == 0.0 .and. dXds(3) < 0.0) .OR. &
         (XX(3) == 1.0 .and. dXds(3) > 0.0)) then
      sgn = -1.0
    else
      sgn = 1.0
    endif

  end function initialStepSign

  !-------------------------------------------------------------------
  !> Set absolute tolerance for binary VLE/LLE points
  !!
  !! \author MH, 2016-01
  !-------------------------------------------------------------------
  subroutine setBinaryAbsTol(tol)
    implicit none
    real, intent(in) :: tol
    binaryAbsTol = tol
  end subroutine setBinaryAbsTol

  !-------------------------------------------------------------------
  !> Solve for three-phase line using good initialguess
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  subroutine LLVEpointTV(P,T,vx,vw,vy,x,w,y,ispec,ierr,iter)
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    use thermopack_constants, only: get_templimits, MINGIBBSPH
    use thermopack_var, only: nc
    use eosTV, only: pressure
    use eos, only: specificVolume, thermo, pseudo_safe
    use single_phase, only: TP_CalcPseudo
    use puresaturation, only: puresat
    use utilities, only: isXwithinBounds
    !$ use omp_lib, only: omp_get_thread_num
    implicit none
    real, dimension(nc), intent(inout) :: x,y,w !< Phase compositions
    real, intent(inout) :: P,T !<
    real, intent(inout) :: vx,vw,vy !<
    integer, intent(in) :: ispec !<
    integer, intent(out) :: ierr !< Error flag
    integer, optional, intent(out) :: iter !< Number of iterantions
    ! Locals
    real, dimension(3) :: param
    real, dimension(10) :: XX,XXmin,XXmax
    real :: delta, lnfug(nc)
    integer :: phase
    type(nonlinear_solver) :: solver
    integer :: i
    real :: tpc,ppc,zpc,vpc
    ! Test
    !real :: G(10), G2(10), Jac(10,10), XX1(10)

    XX(1:2) = log(x)
    XX(3:4) = log(w)
    XX(5:6) = log(y)
    XX(7) = log(vx)
    XX(8) = log(vw)
    XX(9) = log(vy)
    XX(10) = log(T)
    XXmin(1:6) = log(1.0e-30)
    XXmax(1:6) = log(5.0)
    XXmin(7:9) = log(1.0e-8)
    XXmax(7:9) = log(100.0)
    call get_templimits(XXmin(10),XXmax(10))
    XXmin(10) = log(XXmin(10))
    XXmax(10) = log(XXmax(10))
    param(1) = max(1.0e5, p)
    param(2) = ispec
    if (ispec == 11) then
      ! Initial point. Recalculate initial values.
      delta = 0.025*minval(x)
      param(3) = delta
      x(1) = x(1) - delta
      x(2) = x(2) + delta
      y(1) = y(1) + delta
      y(2) = y(2) - delta
      ! Select stable phases
      call thermo(T,P,x,MINGIBBSPH,lnfug,ophase=phase)
      call specificVolume(T,P,x,phase,vx)
      call thermo(T,P,y,MINGIBBSPH,lnfug,ophase=phase)
      call specificVolume(T,P,y,phase,vy)
      XX(1:2) = log(x)
      XX(5:6) = log(y)
      XX(7) = log(vx)
      XX(9) = log(vy)
    else if (ispec == 12) then
      param(3) = p
    else if (ispec == 13) then
      ! Initial point very close to pure fluid
      ! The incipient phase is probably in a solid state
      ! Get saturation state at reduced pressure (0.995*Pc)
      ! Start with establishment of pseudocritical properties
      call pseudo_safe(y,tpc,ppc,zpc,vpc)
      P = ppc
      delta = 0
      do i=1,10
        P = P*0.995
        call PureSat(T,P,y,.true.,ierr)
        if (ierr ==0) then
          ! Calculate liquid and vapor state
          call specificVolume(T,P,x,LIQPH,vx)
          call specificVolume(T,P,y,VAPPH,vy)
          delta = vy - vx
          if (delta > 0) exit
        endif
      enddo
      if (delta == 0) call stoperror("Error for initial point s=13")
      ! Get new density for non-critical  phase
      call specificVolume(T,P,w,LIQPH,vw)
      ! Solve VLLE at the new reduced pressure
      XX(7) = log(vx)
      XX(8) = log(vw)
      XX(9) = log(vy)
      XX(10) = log(T)
      param(3) = p
      param(2) = 12
    else if (ispec == 14) then
      if (abs(y(1) - x(1)) < abs(y(1) - w(1))) then
        param(3) = 1
      else
        param(3) = 3
      endif
    else
      param(3) = XX(ispec)
    endif
    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-10
    solver%limit_x_values = .true.
    solver%max_it = 50
    solver%ls_max_it = 3

    ! Test
    ! print *,'Testing llve line solver!'
    ! call llve_TV_fun_newton(G,XX,param)
    ! call llve_TV_diff_newton(Jac,XX,param)
    ! do i=1,10
    !   XX1 = XX
    !   XX1(i) = XX1(i) + 1.0e-5
    !   call llve_TV_fun_newton(G2,XX1,param)
    !   print *
    !   print *,"i: ",i
    !   print *,(G2-G)/1.0e-5
    !   print *,Jac(:,i)
    ! enddo
    ! stop
    call isXwithinBounds(10,XX,XXmin,XXmax,&
         "ln(x(1:2)),ln(w(1:2)),ln(y(1:2)),ln(vx),ln(vw),ln(vy),ln(T)",&
         "LLVEpointTV: Initial values not within bounds!!")
    call nonlinear_solve(solver,llve_TV_fun_newton,llve_TV_diff_newton,&
         llve_TV_diff_newton,limit_dx,premReturn,setXv,XX,XXmin,XXmax,param)

    x = exp(XX(1:2))
    w = exp(XX(3:4))
    y = exp(XX(5:6))
    !
    vx = exp(XX(7))
    vw = exp(XX(8))
    vy = exp(XX(9))
    !
    T = exp(XX(10))
    P = pressure(T,vx,x)
    ierr = solver%exitflag
    if (present(iter)) iter = solver%iter

  end subroutine LLVEpointTV

  !-------------------------------------------------------------------
  !> Vapor phase function values for non-linear solver
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine llve_TV_fun_newton(G,XX,param)
    use thermopack_var, only: nc
    use eosTV, only: thermo_tv, pressure
    implicit none
    real, dimension(10), intent(out) :: G !< Function values
    real, dimension(10), intent(in) :: XX !< Variable vector
    real, dimension(3) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: lnfx, lnfy, lnfw, x, y, w
    real :: vx, vy, vw, Px, Py, Pw
    real :: T, P
    integer :: is, ix

    x = exp(XX(1:2))
    w = exp(XX(3:4))
    y = exp(XX(5:6))
    !
    vx = exp(XX(7))
    vw = exp(XX(8))
    vy = exp(XX(9))
    !
    T = exp(XX(10))
    !
    P = param(1)

    call thermo_tv(t,vx,x,lnfx)
    call thermo_tv(t,vw,w,lnfw)
    call thermo_tv(t,vy,y,lnfy)

    Px = pressure(t,vx,x)
    Pw = pressure(t,vw,w)
    Py = pressure(t,vy,y)

    ! Pressure
    G(1) = (Px-Pw)/P
    G(2) = (Px-Py)/P
    ! Fugacities
    G(3:4) = lnfx - lnfw
    G(5:6) = lnfx - lnfy
    ! Phase fractions
    G(7) = sum(x) - 1
    G(8) = sum(w) - 1
    G(9) = sum(y) - 1
    ! Specification
    is = nint(param(2))
    if (is <= 10) then
      G(10) = XX(is) - param(3)
    else if (is == 11) then
      G(10) = y(1) - x(1) - 2*param(3)
    else if (is == 12) then
      G(10) = (Px-param(3))/P
    else if (is == 14) then
      ix = nint(param(3))
      G(10) = XX(5) - XX(ix)
    endif

  end subroutine llve_TV_fun_newton

  !-------------------------------------------------------------------
  !> Vapor phase function values for non-linear solver
  !>
  !> \author MH, 2015-04
  !-------------------------------------------------------------------
  subroutine llve_TV_diff_newton(Jac,XX,param)
    use thermopack_var, only: nc
    use eosTV, only: thermo_tv, pressure
    implicit none
    real, dimension(10), intent(in) :: XX !< Variable vector
    real, dimension(10,10), intent(out) :: Jac !< Function differentials
    real, dimension(3) :: param !< Parameter vector
    ! Locals
    real, dimension(nc) :: lnfx, lnfy, lnfw, x, y, w
    real :: vx, vy, vw, Px, Py, Pw, Pxv, Pwv, Pyv, PxT, PyT, PwT
    real, dimension(nc,nc) :: lnfxn, lnfwn, lnfyn
    real, dimension(nc) :: lnfxT, lnfwT, lnfyT
    real, dimension(nc) :: lnfxv, lnfwv, lnfyv
    real, dimension(nc) :: Pxn, Pwn, Pyn
    real :: T, P
    integer :: is, ix

    x = exp(XX(1:2))
    w = exp(XX(3:4))
    y = exp(XX(5:6))
    !
    vx = exp(XX(7))
    vw = exp(XX(8))
    vy = exp(XX(9))
    !
    T = exp(XX(10))
    !
    P = param(1)

    call thermo_tv(t,vx,x,lnfx,lnphit=lnfxT,lnphiv=lnfxv,lnphin=lnfxn)
    call thermo_tv(t,vw,w,lnfw,lnphit=lnfwT,lnphiv=lnfwv,lnphin=lnfwn)
    call thermo_tv(t,vy,y,lnfy,lnphit=lnfyT,lnphiv=lnfyv,lnphin=lnfyn)

    Px = pressure(t,vx,x,dpdv=Pxv,dpdt=PxT,dpdn=Pxn)
    Pw = pressure(t,vw,w,dpdv=Pwv,dpdt=PwT,dpdn=Pwn)
    Py = pressure(t,vy,y,dpdv=Pyv,dpdt=PyT,dpdn=Pyn)

    Jac = 0
    ! Pressure
    Jac(1,1:2) = x*Pxn/P
    Jac(1,3:4) = -w*Pwn/P
    Jac(1,7) = vx*Pxv/P
    Jac(1,8) = -vw*Pwv/P
    Jac(1,10) = T*(PxT - PwT)/P
    !
    Jac(2,1:2) = Jac(1,1:2)
    Jac(2,5:6) = -y*Pyn/P
    Jac(2,7) = Jac(1,7)
    Jac(2,9) = -vy*Pyv/P
    Jac(2,10) = T*(PxT - PyT)/P

    ! Fugacities
    Jac(3,1:2) = x*lnfxn(1,:)
    Jac(4,1:2) = x*lnfxn(2,:)
    Jac(3,3:4) = -w*lnfwn(1,:)
    Jac(4,3:4) = -w*lnfwn(2,:)
    Jac(3:4,7) = vx*lnfxv
    Jac(3:4,8) = -vw*lnfwv
    Jac(3:4,10) = T*(lnfxT - lnfwT)
    !
    Jac(5:6,1:2) = Jac(3:4,1:2)
    Jac(5,5:6) = -y*lnfyn(1,:)
    Jac(6,5:6) = -y*lnfyn(2,:)
    Jac(5:6,7) = vx*lnfxv
    Jac(5:6,9) = -vy*lnfyv
    Jac(5:6,10) = T*(lnfxT - lnfyT)

    ! Phase fractions
    Jac(7,1:2) = x
    Jac(8,3:4) = w
    Jac(9,5:6) = y
    !
    ! Specification
    is = nint(param(2))
    if (is <= 10) then
      Jac(10,is) = 1
    else if (is == 11) then
      Jac(10,1) = -x(1)
      Jac(10,5) = y(1)
    else if (is == 12) then
      Jac(10,1:2) = Jac(1,1:2)
      Jac(10,7) = Jac(1,7)
      Jac(10,10) = T*PxT/P
    else if (is == 14) then
      ix = nint(param(3))
      Jac(10,5) = 1
      Jac(10,ix) = -1
    endif

  end subroutine llve_TV_diff_newton

  !-------------------------------------------------------------------------
  !> Sensitivities of LLVE equation system
  !>
  !>
  !> \author MH, 2019-05
  !-------------------------------------------------------------------------
  subroutine LLVE_TV_sensitivity(P,X,dXdS,s,ierr)
    implicit none
    real, intent(in) :: P !< Pressure
    real, dimension(10), intent(out) :: dXds !< System sensitivities
    real, dimension(10), intent(in) :: X !< Variables
    integer, intent(in) :: s !< Specification
    integer, intent(out) :: ierr !< Error flag
    ! Locals
    real, dimension(3) :: param
    real, dimension(10,10) :: Jac
    integer, dimension(10) :: INDX
    integer :: info
    !real :: G(10), X1(10), G1(10)
    !integer :: i
    !
    param(1) = P
    param(2) = s
    param(3) = X(s)

    call llve_TV_diff_newton(Jac,X,param)
    dXdS = 0.0
    dXdS(10) = 1.0

    ! Solve equation system
    call DGESV( 10, 1, Jac, 10, INDX, dXdS, 10, info )
    if (info /= 0) then
      ierr = 2
    else
      ierr = 0
    endif

    ! Test
    ! print *,'Testing llve line solver!'
    ! call llve_TV_fun_newton(G,X,param)
    ! call llve_TV_diff_newton(Jac,X,param)
    ! do i=10,10
    !   X1 = X
    !   X1(i) = X1(i) + 1.0e-5
    !   call llve_TV_fun_newton(G1,X1,param)
    !   print *,"i: ",i
    !   print *,(G1-G)/1.0e-5
    !   print *,Jac(:,i)
    !   print *,(G1-G)/1.0e-5-Jac(:,i)
    ! enddo
    ! stop

  end subroutine LLVE_TV_sensitivity

  !-------------------------------------------------------------------
  !> Test for phase stabillity in binary system
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  function binary_is_stable_phase(T,P,z,y,phase,tpd) result(isStable)
    use thermopack_var, only: nc
    use eos, only: thermo
    use stability, only: tpd_fun, stabilityLimit, stabcalcW
    implicit none
    real, dimension(nc), intent(in) :: z !< Phase compositions
    real, intent(in) :: T !<
    real, intent(in) :: P !<
    real, dimension(nc), intent(out) :: y !<
    integer, intent(in) :: phase !<
    real, intent(out) :: tpd !<
    logical :: isStable
    ! Locals
    integer :: i, min_i_tpd
    integer, parameter :: n = 51
    real, parameter :: dz = 0.02
    real :: D(nc), lnFugZ(nc), lnFugY(nc), tpd_min
    real, dimension(1,nc) :: XX
    real, parameter :: eps = 1.0e-8
    isStable = .true.
    if (z(1)*z(2) == 0) then
      return
    endif
    call thermo(t,p,Z,LIQPH,lnFugZ)
    D = log(Z)+lnFugZ
    min_i_tpd = -1
    tpd_min = 100.0
    do i=0,n-1
      y(1) = min(max(i*dz,eps),1-eps)
      y(2) = max(1.0-y(1),0.0)
      call thermo(t,p,y,phase,lnFugY)
      tpd = tpd_fun(y,lnFugY,D)
      !print *,tpd,stabilityLimit*100.0
      if (tpd < stabilityLimit*1000.0) then
        min_i_tpd = i
        exit
      else if (tpd < tpd_min) then
        min_i_tpd = i
        tpd_min = tpd
      endif
    enddo
    y(1) = min(max(min_i_tpd*dz,eps),1-eps)
    y(2) = max(1.0-y(1),0.0)

    ! Perform minimization determine stability and to get best possible starting values
    XX(1,:) = Z
    tpd = stabcalcW(1,1,T,P,XX,y,phase,lnFugZ,lnFugY,preTermLim=-1000.0)
    if (tpd < stabilityLimit*1000.0) then
      isStable = .false.
    endif
  end function binary_is_stable_phase

  !-------------------------------------------------------------------
  !> Test for phase stabillity in binary system
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  function binary_is_stable(T,P,z,y,vy) result(isStable)
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    use eos, only: specificvolume
    implicit none
    real, dimension(nc), intent(in) :: z !< Phase compositions
    real, intent(in) :: T !<
    real, intent(in) :: P !<
    real, dimension(nc), intent(out) :: y !<
    real, intent(out) :: vy !<
    logical :: isStable
    !
    real :: tpdl, tpdv, x(nc)
    integer :: phase
    isStable = (binary_is_stable_phase(T,P,z,x,LIQPH,tpdl) .and. &
         binary_is_stable_phase(T,P,z,y,VAPPH,tpdv))

    if (.not. isStable) then
      if (tpdv <= tpdl) then
        phase = VAPPH
      else
        phase = LIQPH
        y = x
      endif
      call specificvolume(T,P,y,phase,vy)
    endif
  end function binary_is_stable

  !-------------------------------------------------------------------
  !> Solve for high ressure critical line
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  function high_pressure_critical_point(Pc,Tc,Zc,Tmin,Tmax) result(found)
    use thermopack_constants, only: tpTmin, tpTmax
    use thermopack_var, only: nc
    use eos, only: specificvolume, thermo
    use critical, only: calcCriticalZ, calcCriticalTV
    use eosTV, only: pressure
    implicit none
    real, dimension(nc), intent(out) :: Zc !< Phase compositions
    real, intent(out) :: Pc !<
    real, intent(out) :: Tc !<
    real, intent(in) :: Tmin !<
    real, optional, intent(in) :: Tmax !<
    logical :: found
    ! Locals
    integer :: ierr
    real :: vc

    found = .false.
    tpTmin = Tmin
    if (present(Tmax)) then
      tpTmax = Tmax
    endif
    Pc = 2.0e8
    Tc = 300.0
    call get_zc_min_lambda(Tc,Pc,Zc)
    if (solve_for_lambda_zero(Pc,Tc,Zc)) then
      call get_zc_min_lambda(Tc,Pc,Zc)
      if (solve_for_lambda_zero(Pc,Tc,Zc)) then
        call specificvolume(Tc,Pc,Zc,LIQPH,vc)
        call calcCriticalZ(Tc,vc,Pc,Zc,4,ierr,1.0e-7)
        if (ierr == 0) then
          found = .true.
        endif
      endif
    endif
  contains
    subroutine get_zc_min_lambda(Tc,Pc,Zc)
      use critical, only: calcStabMinEig
      real, dimension(nc), intent(out) :: Zc !< Phase compositions
      real, intent(in) :: Pc !<
      real, intent(in) :: Tc !<
      ! Locals
      integer, parameter :: n = 51
      real, parameter :: dz = 0.02
      integer :: i, min_i_lambda
      real :: lambda, lambda_min
      min_i_lambda = -1
      lambda_min = 1.0e50
      do i=0,n-1
        Zc(1) = i*dz
        Zc(2) = max(1.0-Zc(1),0.0)
        lambda = calcStabMinEig(Tc,Pc,Zc,LIQPH)
        if (lambda < lambda_min) then
          min_i_lambda = i
          lambda_min = lambda
        endif
      enddo
      Zc(1) = min_i_lambda*dz
      Zc(2) = max(1.0-Zc(1),0.0)
    end subroutine get_zc_min_lambda
  end function high_pressure_critical_point

  !-------------------------------------------------------------------
  !> Solve for spinodal in binary system
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  function solve_for_lambda_zero(Pc,Tc,Zc) result(found)
    use optimizers, only: optimize, optim_param, setX, prematureReturn
    use thermopack_var, only: nc
    use nonlinear_solvers, only: nonlinear_solver, nonlinear_solve, &
         limit_dx, premReturn, setXv
    implicit none
    real, dimension(nc), intent(in) :: Zc !< Phase compositions
    real, intent(in) :: Pc !<
    real, intent(inout) :: Tc !<
    logical :: found
    ! Locals
    real :: param(2)
    real :: T(1), Tmin(1), Tmax(1)
    !type(optim_param) :: optim
    type(nonlinear_solver) :: solver
    param(1) = Zc(1)
    param(2) = Pc
    T = Tc

    solver%rel_tol = 1.0e-20
    solver%abs_tol = 1.0e-8
    solver%limit_x_values = .true.
    solver%max_it = 50
    solver%ls_max_it = 3
    Tmin = 30.0
    Tmax = 999.0
    call nonlinear_solve(solver,lambda_sub,&
         lambda_diff_sub,lambda_diff_sub,limit_dx,&
         premReturn,setXv,T,Tmin,Tmax,param)
    Tc = T(1)
    if (solver%exitflag == 0) then
      found = .true.
    else
      found = .false.
    endif

    ! optim%rel_tol = 1.0e-8
    ! optim%max_line_search_iter = 2
    ! optim%gradient_termination = .true.
    ! optim%max_iter = 100
    ! call optimize(optim,lambda,lambda_diff,T,param,&
    !      limitDeltaTemp,prematureReturn,get_problem_size,setX)
    ! Tc = T(1)
    ! if (optim%exitflag == 0) then
    !   found = .true.
    ! else
    !   found = .false.
    ! endif
  end function solve_for_lambda_zero

  !-----------------------------------------------------------------------------
  !> Calculate lambda_1
  !>
  !> \author MH, 2019-05
  !-----------------------------------------------------------------------------
  subroutine lambda_sub(F,T,param)
    implicit none
    real, dimension(1), intent(out) :: F !< 
    real, dimension(1), intent(in) :: T !< Variable vector
    real, dimension(2), intent(in) :: param !< Parameter vector
    ! Locals
    F = lambda(T,param)
  end subroutine lambda_sub

  !-----------------------------------------------------------------------------
  !> Calculate lambda_1
  !>
  !> \author MH, 2019-04
  !-----------------------------------------------------------------------------
  function lambda(T,param) result(of)
    use eos, only: thermo
    implicit none
    real, dimension(1), intent(in) :: T !< Variable vector
    real, dimension(2), intent(in) :: param !< Parameter vector
    real :: of !< Objective function value
    ! Locals
    real:: p, lnfug(nc), lnfugn(nc,nc)
    real, dimension(nc) :: Z
    !
    p = param(2)
    Z(1) = param(1)
    Z(2) = 1.0 - Z(1)
    !
    call thermo(t(1),p,z,LIQPH,lnfug,lnfugx=lnfugn)
    of = 1.0 - lnfugn(1,2)
    !print *,t(1),of
  end function lambda

  !-----------------------------------------------------------------------------
  !> Calculate lambda_1
  !>
  !> \author MH, 2019-05
  !-----------------------------------------------------------------------------
  subroutine lambda_diff_sub(dF,T,param)
    implicit none
    real, dimension(1,1), intent(out) :: dF !< 
    real, dimension(1), intent(in) :: T !< Variable vector
    real, dimension(2), intent(in) :: param !< Parameter vector
    ! Locals
    real :: F, Hof(1,1)
    call lambda_diff(T,param,F,dF,Hof)
  end subroutine lambda_diff_sub

  !-----------------------------------------------------------------------------
  !> Calculate lambda_1 and its
  !> differentials.
  !>
  !> \author MH, 2019-04
  !-----------------------------------------------------------------------------
  subroutine lambda_diff(T,param,of,dOFdvar,Hof)
    implicit none
    real, dimension(1), intent(in) :: T !< Variable vector
    real, dimension(2), intent(in) :: param !< Parameter vector
    real, dimension(1,1), intent(out) :: Hof !< Hessian matrix of objective function
    real, dimension(1), intent(out) :: dOFdvar !< Differential of objective function with respect to temperature
    real, intent(out) :: of !< Objective function value
    ! Locals
    real:: dT, of1, of2
    real, dimension(1) :: Tp
    !
    dT = T(1)*1.0e-5
    of = lambda(T,param)
    Tp(1) = T(1) - dT
    of1 = lambda(Tp,param)
    Tp(1) = T(1) + dT
    of2 = lambda(Tp,param)

    dOFdvar(1) = (of2 - of1)/(2.0*dT)
    Hof(1,1) = (of2 - 2.0*of + of1)/dT**2
  end subroutine lambda_diff

  !-----------------------------------------------------------------------------
  !> Limit change in temperature.
  !>
  !> \author MH, 2019-04
  !-----------------------------------------------------------------------------
  subroutine limitDeltaTemp(T,param,dT)
    use thermopack_constants, only: get_templimits
    implicit none
    real, dimension(1), intent(in)    :: T !< Variable
    real, dimension(2), intent(in) :: param !< Parameter vector
    real, dimension(1), intent(inout) :: dT !< Calculated change in variables
    ! Locals
    real, parameter :: maxstep_t = 50.0
    real :: T0, T1, Tmin, Tmax
    call get_templimits(Tmin, Tmax)

    T0 = T(1)
    T1 = T(1) + dT(1)
    if (abs(dT(1)) > maxstep_t) then
      if (dT(1) > 0.0) then
        dT(1) = maxstep_t
      else
        dT(1) = - maxstep_t
      endif
      T1 = T0 + dT(1)
    endif
    if (dT(1) > 0.0 .AND. T1 > Tmax) then
      dT(1) = Tmax - T0
    endif
    if (dT(1) < 0.0 .AND. T1 < Tmin) then
      dT(1) = Tmin - T0
    endif
  end subroutine limitDeltaTemp

  !----------------------------------------------------------------------------
  !> Support for variable size in problem
  !>
  !> \author MH, 2019-04
  !-----------------------------------------------------------------------------
  function get_problem_size(param) result(nvar)
    implicit none
    real, dimension(2), intent(in) :: param !< Parameter vector
    integer :: nvar

    nvar = 1

  end function get_problem_size

  !-------------------------------------------------------------------
  !> Loop critical line
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  subroutine loop_critical_line(Pmin,Pmax,Tmin,P0,T0,Z0,v0,initCritLine,iTermination,critline,y,vy,caep)
    use thermopack_var, only: nc
    use eos, only: getCriticalParam
    use critical, only: calcCriticalEndPoint, calcCriticalZ, critZsensitivity, &
         calcCriticalTV
    use eosTV, only: pressure
    use saturation_curve, only: aep, AZ_CAEP
    implicit none
    integer, intent(in) :: initCritLine
    integer, intent(out) :: iTermination
    real, intent(in) :: Pmin,Pmax,Tmin !<
    real, dimension(nc), intent(in) :: Z0 !< Phase compositions
    real, intent(in) :: P0 !<
    real, intent(in) :: T0 !<
    real, intent(in) :: v0 !<
    type(criticalLine), intent(inout) :: critline
    real, dimension(nc), intent(out) :: y !<
    real, intent(out) :: vy !<
    type(aep), optional, intent(out) :: caep
    ! Locals
    real, parameter :: tol = 1.0e-7
    integer :: s, ierr, smax(1), iter, i
    real :: ds, X(4), Xold(4), dXds(4), sgn, Pmax_term
    real :: Zc(nc), Pc, Tc, vc
    real, dimension(nc) ::tci, pci, oi
    real, parameter :: dzLim = 0.02
    real, parameter :: tuning = 1.2, ds_min = 0.001, ds_max = 0.005
    real :: d2pdv2,d2pdv2_old,d3pdv3
    select case(initCritLine)
    case (BCL_PMAX)
      Pmax_term = max(Pmax, P0)
      s = 4
      Tc = T0
      vc = v0
      Pc = P0
      Zc = Z0
      call setX_crit(Tc,vc,Zc,Pc,X)
      ierr = 0
      iTermination = addPoint()
      if (iTermination /= BP_TERM_NONE) return
      sgn = -1.0
      ds = 0.02
    case (BCL_PURE_C1, BCL_PURE_C2)
      call getCriticalParam(1,tci(1),pci(1),oi(1))
      call getCriticalParam(2,tci(2),pci(2),oi(2))
      if (initCritLine == BCL_PURE_C2) then
        if (tci(2) > tci(1)) then
          sgn = 1
          Zc = (/ 0.0, 1.0/)
        else
          sgn = -1
          Zc = (/ 1.0, 0.0/)
        endif
      else ! BCL_PURE_C1
        if (tci(2) > tci(1)) then
          sgn = -1
          Zc = (/ 1.0, 0.0/)
        else
          sgn = 1
          Zc = (/ 0.0, 1.0/)
        endif
      endif
      Tc = sum(Zc*tci)
      vc = -1.0
      Pmax_term = Pmax
      s = 1
      ds = ds_min
      call calcCriticalTV(Tc,vc,Zc,ierr,tol)
      Pc = pressure(Tc,vc,Zc)
      iTermination = addPoint()
      if (iTermination /= BP_TERM_NONE) return
    case default
    end select

    if (present(caep)) then
      if (initCritLine == BCL_PMAX) then
        call calc_pressurediff(Tc,vc,Zc,d2pdv2,d3pdv3)
        d2pdv2_old = d2pdv2
      else
        d2pdv2_old = 0
      endif
      caep%found = .false.
      caep%type = AZ_CAEP
    endif

    ! Loop critical line
    do i=1,10000
      ! Extrapolate state
      call critZsensitivity(Zc,1,X,dXdS,s,ierr)
      smax = maxloc(abs(dXdS))
      if ((.not. smax(1) == s) .and. i > 1) then
        s = smax(1)
        ! Rescaling the sensitivities
        sgn = sign(1.0,X(s) - Xold(s))
        dXdS = dXdS / dXdS(s)
      endif
      Xold = X
      X = Xold + dXdS*dS*sgn
      if (X(1) < dzLim .and. dXdS(1)*sgn < 0.0) then
        s = 1
        if (X(1) < 0.0) then
          dS = -sgn*Xold(1)/dXdS(1)
          X = Xold + dXdS*dS*sgn
          X(1) = 0.0
          call getPropFromX_crit(X,Tc,vc,Zc,Pc)
          call calcCriticalTV(Tc,vc,Zc,ierr,tol)
          Pc = pressure(Tc,vc,Zc)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_SINGLE
          return
        endif
      else if (X(1) > 1.0 - dzLim .and. dXdS(1)*sgn > 0.0) then
        s = 1
        if (X(1) > 1.0) then
          dS = sgn*(1.0 - Xold(1))/dXdS(1)
          X = Xold + dXdS*dS*sgn
          X(1) = 1.0
          call getPropFromX_crit(X,Tc,vc,Zc,Pc)
          call calcCriticalTV(Tc,vc,Zc,ierr,tol)
          Pc = pressure(Tc,vc,Zc)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_SINGLE
          return
        endif
      else if (X(2) < log(Tmin)) then
        s = 2
        dS = sgn*(log(Tmin) - Xold(2))/dXdS(2)
        X = Xold + dXdS*dS*sgn
        call getPropFromX_crit(X,Tc,vc,Zc,Pc)
        call calcCriticalTV(Tc,vc,Zc,ierr,tol)
        Pc = pressure(Tc,vc,Zc)
        iTermination = addPoint()
        if (iTermination /= BP_TERM_NONE) return
        iTermination = BP_TERM_TMIN
        return
      endif
      call getPropFromX_crit(X,Tc,vc,Zc,Pc)
      call calcCriticalZ(Tc,vc,Pc,Zc,s,ierr,tol,iter=iter)
      if (Pc > Pmax_term .and. ierr == 0) then
        s = 4
        Pc = Pmax_term
        call calcCriticalZ(Tc,vc,Pc,Zc,s,ierr,tol)
        iTermination = addPoint()
        if (iTermination /= BP_TERM_NONE) return
        iTermination = BP_TERM_PMAX
        return
      endif
      if (Pc < Pmin .and. ierr == 0) then
        s = 4
        Pc = Pmin
        call calcCriticalZ(Tc,vc,Pc,Zc,s,ierr,tol)
        iTermination = addPoint()
        if (iTermination /= BP_TERM_NONE) return
        iTermination = BP_TERM_PMIN
        return
      endif
      iTermination = addPoint()
      if (iTermination == BP_TERM_ERR .and. ds > 1.05*ds_min) cycle
      if (iTermination /= BP_TERM_NONE) return

      ! Tune dS up or down based on how fast sat_newton converged
      if (iter < 3) then
        ds = ds * tuning
      else if (iter > 5) then
        ds = ds * (2.0 - tuning)
      endif
      ds = max(min(ds,ds_max),ds_min)

      ! Look for CAEP
      if (present(caep)) then
        if (.not. caep%found) then
          call calc_pressurediff(Tc,vc,Zc,d2pdv2,d3pdv3)
          if (d2pdv2_old*d2pdv2 < 0 .and. d3pdv3 < 0) then
            call solve_for_caep(X(s),Xold,dXds,s,caep,ierr,tol)
          endif
          d2pdv2_old = d2pdv2
        endif
      endif

    enddo

    iTermination = BP_TERM_NPOINT

  contains
    function addPoint() result(iTerm)
      integer :: iTerm
      if (ierr /= 0) then
        iTerm = BP_TERM_ERR
      else
        iTerm = BP_TERM_NONE
      endif
      !print *,Tc,Pc,Zc(1),vc
      if (.not. binary_is_stable(Tc,Pc,Zc,y,vy)) then
        call calcCriticalEndPoint(Tc,vc,Zc,y,vy,ierr,tol)
        Pc = pressure(Tc,vc,Zc)
        if (ierr /= 0) then
          iTerm = BP_TERM_ERR
          return
        else
          iTerm = BP_TERM_TPD
        endif
      endif
      if (iTerm /= BP_TERM_ERR) then
        call critline%push_back(Tc,vc,Pc,Zc)
        call setX_crit(Tc,vc,Zc,Pc,X)
      else
        ds = 0.5*ds
        X = Xold
      endif
    end function addPoint
  end subroutine loop_critical_line

  subroutine getPropFromX_crit(X,Tc,vc,Zc,Pc)
    real, intent(out) :: Tc,vc,Zc(2),Pc
    real, intent(in) :: X(4)
    Zc(1) = X(1)
    Zc(2) = max(0.0, 1.0 - Zc(1))
    Tc = exp(X(2))
    vc = exp(X(3))
    Pc = exp(X(4))
  end subroutine getPropFromX_crit

  subroutine setX_crit(Tc,vc,Zc,Pc,X)
    real, intent(in) :: Tc,vc,Zc(2),Pc
    real, intent(out) :: X(4)
    X(1) = Zc(1)
    X(2) = log(Tc)
    X(3) = log(vc)
    X(4) = log(Pc)
    !print *,"Tc,Pc,Zc,vc",Tc,Pc,Zc(1),vc
  end subroutine setX_crit

  subroutine calc_pressurediff(T,v,n,d2pdv2,d3pdv3)
    use eosTV, only: pressure
    real, intent(in) :: t !< K - Temperature
    real, intent(in) :: v !< m3 - Volume
    real, dimension(1:nc), intent(in) :: n !< Mol numbers
    real, intent(out) :: d3pdv3 !< Pa/m9 - Third pressure differential wrpt. specific volume
    real, intent(out) :: d2pdv2 !< Pa/m6 - Second pressure differential wrpt. specific volume
    !
    real :: p, dpdv, eps, dpdv_p, dpdv_m, vp
    eps = 1.0e-5*v
    p = pressure(t,v,n,dpdv=dpdv)
    vp = v + eps
    p = pressure(t,vp,n,dpdv=dpdv_p)
    vp = v - eps
    p = pressure(t,vp,n,dpdv=dpdv_m)
    d2pdv2 = (dpdv_p-dpdv_m)/eps
    d3pdv3 = (dpdv_p+dpdv_m-2*dpdv)/eps**2
  end subroutine calc_pressurediff

  subroutine solve_for_caep(XXs,XXold,dXXds,s,caep,ierr,ctol)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver, NS_PEGASUS
    use saturation_curve, only: aep
    use critical, only: calcCriticalZ
    integer, intent(in) :: s
    real, intent(in) :: XXs, XXold(4), dXXds(4), ctol !<
    type(aep), intent(inout) :: caep
    integer, intent(out) :: ierr          ! error flag
    ! Locals
    real, parameter :: tol = 1.0e-6
    real :: XXs_min, XXs_max, param(10), XXs_var, XX(4), ds
    real :: T,P,v,z(2)
    type(nonlinear_solver) :: solver_cfg
    param(1:4) = XXold
    param(5:8) = dXXds
    param(9) = real(s)
    param(10) = ctol
    if (XXs > XXold(s)) then
      XXs_min = XXold(s)
      XXs_max = XXs
    else
      XXs_min = XXs
      XXs_max = XXold(s)
    endif
        ! Configure solver
    solver_cfg%abs_tol = tol
    solver_cfg%max_it = 1000
    solver_cfg%isolver = NS_PEGASUS
    ! Find f=0 inside the bracket.
    call bracketing_solver(XXs_min,XXs_max,fun_caep,XXs_var,solver_cfg,param)
    ! Check for successful convergence
    if (solver_cfg%exitflag /= 0) then
      if (verbose) write(*,*) "CAEP: Bracketing solver failed."
      ierr = solver_cfg%exitflag
      return
    else
      ierr = 0
      caep%found = .true.
      ds = XXs_var - XXold(s)
      XX = XXold + dXXds*ds
      call getPropFromX_crit(XX,T,v,z,P)
      call calcCriticalZ(T,v,P,Z,s,ierr,ctol)
      caep%x = Z
      caep%T = T
      caep%P = P
      caep%vg = v
      caep%vl = v
    endif

  end subroutine solve_for_caep

  function fun_caep(XXs,param) result(f)
    ! Objective function for d2pdv2 = 0
    !
    use critical, only: calcCriticalZ
    implicit none
    ! Input:
    real,     intent(in)  :: XXs !<
    real,     intent(in)  :: param(10) !< Parameters
    ![i_pure,i_insip,T_guess]
    ! Output:
    real                  :: f !< s_sat - sspec
    ! Internal:
    integer :: s,ierr
    real :: XX(4), XXold(4), dXXds(4), ds
    real :: T,P,v,z(2),d2pdv2,d3pdv3,ctol
    ! Read from param
    XXold = param(1:4)
    dXXds = param(5:8)
    s = nint(param(9))
    ctol = param(10)
    ! Extrapolate state
    ds = XXs - XXold(s)
    XX = XXold + dXXds*ds
    call getPropFromX_crit(XX,T,v,z,P)
    call calcCriticalZ(T,v,P,Z,s,ierr,ctol)
    !
    call calc_pressurediff(T,v,Z,d2pdv2,d3pdv3)
    ! Make objective function
    f = v**2*d2pdv2/p
    !print *,"f",f,ds
  end function fun_caep

  subroutine criticalLine_clear(cLine)
    class(criticalLine), intent(inout) :: cLine
    ! Locals
    integer :: err
    if (allocated(cLine%Tc)) then
      deallocate (cLine%Tc, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate cLine%Tc!')
    endif
    if (allocated(cLine%vc)) then
      deallocate (cLine%vc, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate cLine%vc!')
    endif
    if (allocated(cLine%Pc)) then
      deallocate (cLine%Pc, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate cLine%Pc!')
    endif
    if (allocated(cLine%Zc)) then
      deallocate (cLine%Zc, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate cLine%Zc!')
    endif
  end subroutine criticalLine_clear

  subroutine criticalLine_allocate(cLine,n)
    class(criticalLine), intent(inout) :: cLine
    integer, intent(in) :: n
    ! Locals
    integer :: err
    call cline%clear()
    allocate (cLine%Tc(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate cLine%Tc!')
    allocate (cLine%vc(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate cLine%vc!')
    allocate (cLine%Pc(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate cLine%Pc!')
    allocate (cLine%Zc(n,2), STAT=err)
    if (err /= 0) Call StopError('Could not allocate cLine%Zc!')
    cLine%nPoints = 0
  end subroutine criticalLine_allocate

  subroutine criticalLine_push_back(cLine,T,v,P,z)
    class(criticalLine), intent(inout) :: cLine
    real, intent(in) :: T,v,P,z(2)
    ! Locals
    integer :: n
    type(criticalLine) :: cLineTemp

    if (.not. allocated(cLine%Tc)) then
      n = 1000
      call cline%allocate(n)
    else
      n = size(cLine%Tc)
    endif
    if (cLine%nPoints == n) then
      cLineTemp = cLine
      ! Increase memory
      n = n + 1000
      call cline%allocate(n)
      cLine = cLineTemp
    endif
    cLine%nPoints = cLine%nPoints + 1
    cLine%Tc(cLine%nPoints) = T
    cLine%Pc(cLine%nPoints) = P
    cLine%vc(cLine%nPoints) = v
    cLine%Zc(cLine%nPoints,:) = Z
  end subroutine criticalLine_push_back

  function criticalLine_get_size(cl) result(n)
    class(criticalLine), intent(in) :: cl
    integer :: n
    !
    if (allocated(cl%Tc)) then
      n = size(cl%Tc)
    else
      n = 0
    endif
  end function criticalLine_get_size

  function criticalLine_get_npoints(cl) result(n)
    class(criticalLine), intent(in) :: cl
    integer :: n
    !
    if (allocated(cl%Tc)) then
      n = cl%npoints
    else
      n = 0
    endif
  end function criticalLine_get_npoints

  subroutine criticalLine_assign(cl1,cl2)
    class(criticalLine), intent(inout) :: cl1
    class(criticalLine), intent(in)    :: cl2
    ! Locals
    integer :: n
    n = cl2%get_size()
    if (cl1%get_size() < n) then
      call cl1%allocate(n)
    endif
    ! Copy values
    cl1%Tc(1:n) = cl2%Tc
    cl1%Pc(1:n) = cl2%Pc
    cl1%vc(1:n) = cl2%vc
    cl1%zc(1:n,:) = cl2%zc
    cl1%npoints = n
  end subroutine criticalLine_assign

  !-------------------------------------------------------------------
  !> Loop critical line and possibly LLV lines, to map global binary envelope
  !>
  !> \author MH, 2019-04
  !-------------------------------------------------------------------
  subroutine global_binary_plot(Pmin,Pmax,Tmin,filename,iTermination,Tmax,includeAZ)
    use thermopack_var, only: nc
    use eos, only: specificvolume
    use saturation, only: specP
    use saturation_curve, only: singleCompSaturation, aep
    use thermopack_constants, only: tpTmin
    implicit none
    real, intent(in) :: Pmin, Pmax !<
    real, intent(in) :: Tmin !<
    character(len=*), intent(in) :: filename
    integer, intent(out) :: iTermination
    real, optional, intent(in) :: Tmax !<
    logical, optional, intent(in) :: includeAZ
    ! Locals
    type(criticalLine) :: critlineC2, critlineHP, critlineC1
    type(VLLE_Line) :: lVLLE_C2, lVLLE_HP, lVLLE_C1
    integer, parameter :: CAEP_C2=1, CAEP_HP=2, CAEP_C1=3, PAEP1=4, PAEP2=5
    integer, parameter :: HAEP_C1=7, HAEP_HP=6, HAEP_C2=8, NAEPS = 8
    type aep_pointer
      type(aep), pointer :: p_aep
    end type aep_pointer
    type(aep_pointer) :: aeps(NAEPS)
    type(azeotropicLine) :: azLine(NAEPS)
    integer :: iTermC2, iTermHP, iTermC1
    integer :: iTermLLV_C2, iTermLLV_HP, iTermLLV_C1
    real :: Zc(nc), Pc, Tc, vc
    integer :: type, n1, n2, i, n, k
    real :: y1(nc), vy1, y2(nc), vy2, yHP(nc), vyHP
    integer, parameter :: nmax = 1000
    real :: Ta1(nmax), Pa1(nmax), Ta2(nmax), Pa2(nmax), vla1(nmax)
    real :: vva1(nmax), vla2(nmax), vva2(nmax)
    logical :: incAZ
    incAZ = .false.
    if (present(includeAZ)) then
      incAZ = includeAZ
    endif

    ! Initialize azeotropic end points
    if (incAZ) then
      do i=1,NAEPS
        allocate(aeps(i)%p_aep)
        aeps(i)%p_aep%found = .false.
      enddo
    else
      do i=1,NAEPS
        aeps(i)%p_aep => NULL()
      enddo
    endif

    print *,"Mapping global phase diagram for binary system"
    tpTmin = max(1.0,Tmin-0.1) ! Allow solving for Tmin
    iTermHP = BP_TERM_NONE
    iTermC1 = BP_TERM_NONE
    ! Loop all critical lines
    call loop_critical_line(Pmin,Pmax,Tmin,Pc,Tc,Zc,vc,BCL_PURE_C2,iTermC2,&
         critlineC2,y2,vy2,aeps(CAEP_C2)%p_aep)
    if (iTermC2 == BP_TERM_ERR) then
      iTermination = BP_TERM_ERR
      return
    else if (iTermC2 == BP_TERM_TMIN) then
      print *,"Global phase diagram for binary system: Increase Tmin?"
    endif
    if (iTermC2 /= BP_TERM_PMAX) then
      if (high_pressure_critical_point(Pc,Tc,Zc,Tmin,Tmax)) then
        call specificvolume(Tc,Pc,Zc,LIQPH,vc)
        call loop_critical_line(Pmin,Pmax,Tmin,Pc,Tc,Zc,vc,BCL_PMAX,iTermHP,&
             critlineHP,yHP,vyHP,aeps(CAEP_HP)%p_aep)
      endif
    endif
    if (iTermC2 /= BP_TERM_SINGLE) then
      call loop_critical_line(Pmin,Pmax,Tmin,Pc,Tc,Zc,vc,BCL_PURE_C1,iTermC1,&
           critlineC1,y1,vy1,aeps(CAEP_C1)%p_aep)
    endif

    ! Classify according to Scott and Konynemburg
    select case(iTermC2)
    case(BP_TERM_TPD)
      if (iTermHP /= BP_TERM_NONE) then
        type = 4
      else
        type = 5
      endif
      !call loop_vlle
    case(BP_TERM_PMAX)
      type = 3
    case(BP_TERM_SINGLE)
      if (iTermHP /= BP_TERM_NONE) then
        type = 2
      else
        type = 1
      endif
    case default
      iTermination = BP_TERM_ERR
      return
    end select

    print *,"van Konynenburg and Scott type: ",type
    print *,"Critical lines:"
    print *,"iTermHP: ",trim(print_BP_TERM(iTermHP))
    print *,"iTermC1: ",trim(print_BP_TERM(iTermC1))
    print *,"iTermC2: ",trim(print_BP_TERM(iTermC2))

    ! Loop LLVE lines
    iTermLLV_HP = BP_TERM_NONE
    iTermLLV_C1 = BP_TERM_NONE
    iTermLLV_C2 = BP_TERM_NONE
    if (iTermHP == BP_TERM_TPD) then
      n = critlineHP%get_n_points()
      call lVLLE_HP%push_back(critlineHP%Tc(n),critlineHP%Pc(n),critlineHP%Zc(n,:),&
           yHP,critlineHP%Zc(n,:),critlineHP%vc(n),vyHP,critlineHP%vc(n))
      call loop_LLVE_line(Tmin,Pmin,Pmax,iTermLLV_HP,lVLLE_HP,aeps(HAEP_HP)%p_aep)
    endif
    if (iTermC1 == BP_TERM_TPD) then
      n = critlineC1%get_n_points()
      call lVLLE_C1%push_back(critlineC1%Tc(n),critlineC1%Pc(n),critlineC1%Zc(n,:),&
           y1,critlineC1%Zc(n,:),critlineC1%vc(n),vy1,critlineC1%vc(n))
      call loop_LLVE_line(Tmin,Pmin,Pmax,iTermLLV_C1,lVLLE_C1,aeps(HAEP_C1)%p_aep)
    endif
    if (iTermC2 == BP_TERM_TPD) then
      n = critlineC2%get_n_points()
      call lVLLE_C2%push_back(critlineC2%Tc(n),critlineC2%Pc(n),critlineC2%Zc(n,:),&
           y2,critlineC2%Zc(n,:),critlineC2%vc(n),vy2,critlineC2%vc(n))
      call loop_LLVE_line(Tmin,Pmin,Pmax,iTermLLV_C2,lVLLE_C2,aeps(HAEP_C2)%p_aep)
    endif
    print *,"LLVE lines:"
    print *,"iTermHP: ",trim(print_BP_TERM(iTermLLV_HP))
    print *,"iTermC1: ",trim(print_BP_TERM(iTermLLV_C1))
    print *,"iTermC2: ",trim(print_BP_TERM(iTermLLV_C2))

    ! Lop saturation lines for pure fluids. Start at Pmin
    Zc = (/1.0, 0.0/)
    Tc = 0.0
    Pc = Pmin
    call singleCompSaturation(Zc,Tc,Pc,specP,Ta1,Pa1,nmax,n1,maxDeltaP=0.25e5,&
         paep=aeps(PAEP1)%p_aep,log_linear=.true.)
    do i=1,n1
      call specificvolume(Ta1(i),Pa1(i),Zc,LIQPH,vla1(i))
      call specificvolume(Ta1(i),Pa1(i),Zc,VAPPH,vva1(i))
    enddo
    Zc = (/0.0, 1.0/)
    Tc = 0.0
    Pc = Pmin
    call singleCompSaturation(Zc,Tc,Pc,specP,Ta2,Pa2,nmax,n2,maxDeltaP=0.25e5,&
         paep=aeps(PAEP2)%p_aep,log_linear=.true.)
    do i=1,n2
      call specificvolume(Ta2(i),Pa2(i),Zc,LIQPH,vla2(i))
      call specificvolume(Ta2(i),Pa2(i),Zc,VAPPH,vva2(i))
    enddo

    if (incAZ) then
      print *,"Azeotropic end points located:"
      n = 0
      do i=1,NAEPS
        if (aeps(i)%p_aep%found) then
          print *,trim(print_AEP(i))
          n = n + 1
          !call aeps(i)%p_aep%print()
        endif
      enddo
      if (n == 0) then
        print *,"NONE"
      else
        print *,"Azeotropic lines:"
        do i=1,NAEPS
          if (aeps(i)%p_aep%found) then
            print *,trim(print_AEP(i))
            !call aeps(i)%p_aep%print()
            call loop_azeotropic_line(Pmin,Pmax,Tmin,aeps(i)%p_aep,iTermination,azLine(i))
            print *,"iTermination: ",trim(print_BP_TERM(iTermination))
            if (iTermination == BP_TERM_CRIT) then
              do k=i,CAEP_C1
                if (aeps(k)%p_aep%found) then
                  if (az_line_endpoint_is_aep(azLine(i), aeps(k)%p_aep, reltol=5.0e-2)) then
                    aeps(k)%p_aep%found = .false. ! Disable running AZ line form this point
                    ! Write correct CAEP to line
                    call write_aep_to_az_line_endpoint(azLine(i), aeps(k)%p_aep)
                  endif
                endif
              enddo
            else if (iTermination == BP_TERM_SINGLE .or. &
                 iTermination == BP_TERM_TPD) then
              do k=i,NAEPS
                if (aeps(k)%p_aep%found) then
                  if (az_line_endpoint_is_aep(azLine(i), aeps(k)%p_aep, reltol=1.0e-5)) then
                    aeps(k)%p_aep%found = .false. ! Disable running AZ line form this point
                    call write_aep_to_az_line_endpoint(azLine(i), aeps(k)%p_aep)
                  endif
                endif
              enddo
            endif
          endif
        enddo
      endif
    endif
    !stop
    ! Dump data to file
    call dump_global_plot_to_file()
  contains
    function az_line_endpoint_is_aep(azLn, xaep, reltol) result(isSamePoint)
      use utilities, only: is_numerically_equal
      type(aep), intent(in) :: xaep
      type(azeotropicLine), intent(in) :: azLn
      real, intent(in) :: reltol
      logical :: isSamePoint
      ! Locals
      integer :: n
      n = azLn%get_n_points()
      ! print *,azLn%T(n),xaep%t,is_numerically_equal(azLn%T(n),xaep%t,xaep%t*reltol)
      ! print *,azLn%vg(n),xaep%vg,is_numerically_equal(azLn%vg(n),xaep%vg,xaep%vg*reltol)
      ! print *,azLn%vl(n),xaep%vl,is_numerically_equal(azLn%vl(n),xaep%vl,xaep%vl*reltol)
      ! print *,azLn%p(n),xaep%p,is_numerically_equal(azLn%p(n),xaep%p,xaep%p*reltol)
      ! print *,azLn%z(n,1),xaep%x(1),is_numerically_equal(azLn%z(n,1),xaep%x(1),reltol)
      isSamePoint = (is_numerically_equal(azLn%T(n),xaep%t,xaep%t*reltol) .and. &
           is_numerically_equal(azLn%vg(n),xaep%vg,xaep%vg*reltol) .and. &
           is_numerically_equal(azLn%vl(n),xaep%vl,xaep%vl*reltol) .and. &
           is_numerically_equal(azLn%p(n),xaep%p,xaep%p*reltol) .and. &
           is_numerically_equal(azLn%z(n,1),xaep%x(1),reltol))
    end function az_line_endpoint_is_aep

    subroutine write_aep_to_az_line_endpoint(azLn, xaep)
      type(aep), intent(in) :: xaep
      type(azeotropicLine), intent(inout) :: azLn
      ! Locals
      integer :: n
      n = azLn%get_n_points()
      azLn%T(n) = xaep%t
      azLn%vg(n) = xaep%vg
      azLn%vl(n) = xaep%vl
      azLn%p(n) = xaep%p
      azLn%z(n,:) = xaep%x(:)
    end subroutine write_aep_to_az_line_endpoint

    function print_AEP(iAEP) result(charAEP)
      integer, intent(in) :: iAEP
      character(len=15) :: charAEP
      select case(iAEP)
      case(CAEP_C2)
        charAEP = "CAEP_C2"
      case(CAEP_HP)
        charAEP = "CAEP_HP"
      case(CAEP_C1)
        charAEP = "CAEP_C1"
      case(PAEP1)
        charAEP = "PAEP1"
      case(PAEP2)
        charAEP = "PAEP2"
      case(HAEP_C2)
        charAEP = "HAEP_C2"
      case(HAEP_HP)
        charAEP = "HAEP_HP"
      case(HAEP_C1)
        charAEP = "HAEP_C1"
      case default
        call stoperror("print_AEP: Wrong iAEP")
      end select
    end function print_AEP

    subroutine dump_global_plot_to_file()
      use utilities, only: newunit
      use thermopack_constants, only: clen
      !
      integer :: i, k, nLines, ifile, nCols, nCritLines, nLLVE, nAZ
      character(len=clen) :: binaryline, mergedline
      character(len=*), parameter :: sep = '  '
      character(len=*), parameter :: nod = 'NaN'
      character(len=*), parameter :: empty = nod // sep // nod // sep // nod // sep // nod
      character(len=2) :: n_str
      type(thermo_model), pointer :: act_mod_ptr
      act_mod_ptr => get_active_thermo_model()
      nLines = max(n1,n2)
      nCols = 8
      nCritLines = 0
      nLLVE = 0
      nAZ = 0
      mergedline = "# T (1), P (2), vl (3), vv (4), T (5), P (6), vl (7), vv (8)"
      if (critlineHP%get_n_points() > 0) then
        nCritLines = nCritLines + 1
        nCols = nCols + 4
        nLines = max(nLines,critlineHP%get_n_points())
        write(n_str, "(I2)") nCols
        mergedline = trim(mergedline)//", Tc_HP, Pc_HP, vc_HP, zc_HP ("//n_str//")"
      endif
      if (critlineC2%get_n_points() > 0) then
        nCritLines = nCritLines + 1
        nCols = nCols + 4
        nLines = max(nLines,critlineC2%get_n_points())
        write(n_str, "(I2)") nCols
        mergedline = trim(mergedline)//", Tc_C2, Pc_C2, vc_C2, zc_C2 ("//n_str//")"
      endif
      if (critlineC1%get_n_points() > 0) then
        nCritLines = nCritLines + 1
        nCols = nCols + 4
        nLines = max(nLines,critlineC1%get_n_points())
        write(n_str, "(I2)") nCols
        mergedline = trim(mergedline)//", Tc_C1, Pc_C1, vc_C1, zc_C1 ("//n_str//")"
      endif
      !
      if (lVLLE_HP%get_n_points() > 0) then
        nLLVE = nLLVE + 1
        nCols = nCols + 8
        nLines = max(nLines,lVLLE_HP%get_n_points())
        write(n_str, "(I2)") nCols
        mergedline = trim(mergedline)//", T_HP, P_HP, vx_HP, vw_HP, vy_HP, x_HP, w_HP, y_HP ("//n_str//")"
      endif
      if (lVLLE_C2%get_n_points() > 0) then
        nLLVE = nLLVE + 1
        nCols = nCols + 8
        nLines = max(nLines,lVLLE_C2%get_n_points())
        write(n_str, "(I2)") nCols
        mergedline = trim(mergedline)//", T_C2, P_C2, vx_C2, vw_C2, vy_C2, x_C2, w_C2, y_C2 ("//n_str//")"
      endif
      if (lVLLE_C1%get_n_points() > 0) then
        nLLVE = nLLVE + 1
        nCols = nCols + 8
        nLines = max(nLines,lVLLE_C1%get_n_points())
        write(n_str, "(I2)") nCols
        mergedline = trim(mergedline)//", T_C1, P_C1, vx_C1, vw_C1, vy_C1, x_C1, w_C1, y_C1 ("//n_str//")"
      endif
      do k=1,NAEPS
        if (azLine(k)%get_n_points() > 0) then
          nAZ = nAZ + 1
          nCols = nCols + 5
          nLines = max(nLines,azLine(k)%get_n_points())
          write(n_str, "(I2)") nCols
          mergedline = trim(mergedline)//", T_AZC2, P__AZC2, vl_AZC2, vg_AZC2, x_AZC2 ("//n_str//")"
        endif
      enddo
      ! Dump data to file
      ifile = newunit()
      open(unit=ifile,file=trim(filename))
      write(ifile,'(A)') "#Binary system: "//trim(act_mod_ptr%comps(1)%p_comp%ident)//" "//trim(act_mod_ptr%comps(2)%p_comp%ident)
      write(ifile,'(A,I2)') "#Global phase diagram of type: ", type
      write(ifile,'(A,I2)') "#Number of critical lines: ", nCritLines
      write(ifile,'(A,I2)') "#Number of LLVE lines: ", nLLVE
      write(ifile,'(A,I2)') "#Number of AZ lines: ", nAZ
      write(ifile,'(A)') trim(mergedline)
      do i=1,nLines
        ! Saturation lines
        if (i > n1) then
          mergedline = empty
        else
          write(mergedline,'(4es19.10e3)') Ta1(i),Pa1(i),vla1(i),vva1(i)
        endif
        if (i > n2) then
          binaryline = empty
        else
          write(binaryline,'(4es19.10e3)') Ta2(i),Pa2(i),vla2(i),vva2(i)
        endif
        mergedline = trim(mergedline) // sep // trim(binaryline)
        ! Critical lines
        if (critlineHP%get_n_points() > 0) then
          if (i > critlineHP%get_n_points()) then
            binaryline = empty
          else
            write(binaryline,'(4es19.10e3)') critlineHP%Tc(i), critlineHP%Pc(i), &
                 critlineHP%vc(i), critlineHP%zc(i,1)
          endif
          mergedline = trim(mergedline) // sep // trim(binaryline)
        endif
        if (critlineC2%get_n_points() > 0) then
          if (i > critlineC2%get_n_points()) then
            binaryline = empty
          else
            write(binaryline,'(4es19.10e3)') critlineC2%Tc(i), critlineC2%Pc(i), &
                 critlineC2%vc(i), critlineC2%zc(i,1)
          endif
          mergedline = trim(mergedline) // sep // trim(binaryline)
        endif
        if (critlineC1%get_n_points() > 0) then
          if (i > critlineC1%get_n_points()) then
            binaryline = empty
          else
            write(binaryline,'(4es19.10e3)') critlineC1%Tc(i), critlineC1%Pc(i), &
                 critlineC1%vc(i), critlineC1%zc(i,1)
          endif
          mergedline = trim(mergedline) // sep // trim(binaryline)
        endif
        ! LLVE lines
        if (lVLLE_HP%get_n_points() > 0) then
          if (i > lVLLE_HP%get_n_points()) then
            binaryline = empty // sep // empty
          else
            write(binaryline,'(8es19.10e3)') lVLLE_HP%T(i), lVLLE_HP%P(i), &
                 lVLLE_HP%vx(i), lVLLE_HP%vw(i), lVLLE_HP%vy(i), &
                 lVLLE_HP%x(i,1), lVLLE_HP%w(i,1), lVLLE_HP%y(i,1)
          endif
          mergedline = trim(mergedline) // sep // trim(binaryline)
        endif
        if (lVLLE_C2%get_n_points() > 0) then
          if (i > lVLLE_C2%get_n_points()) then
            binaryline = empty // sep // empty
          else
            write(binaryline,'(8es19.10e3)') lVLLE_C2%T(i), lVLLE_C2%P(i), &
                 lVLLE_C2%vx(i), lVLLE_C2%vw(i), lVLLE_C2%vy(i), &
                 lVLLE_C2%x(i,1), lVLLE_C2%w(i,1), lVLLE_C2%y(i,1)
          endif
          mergedline = trim(mergedline) // sep // trim(binaryline)
        endif
        if (lVLLE_C1%get_n_points() > 0) then
          if (i > lVLLE_C1%get_n_points()) then
            binaryline = empty // sep // empty
          else
            write(binaryline,'(8es19.10e3)') lVLLE_C1%T(i), lVLLE_C1%P(i), &
                 lVLLE_C1%vx(i), lVLLE_C1%vw(i), lVLLE_C1%vy(i), &
                 lVLLE_C1%x(i,1), lVLLE_C1%w(i,1), lVLLE_C1%y(i,1)
          endif
          mergedline = trim(mergedline) // sep // trim(binaryline)
        endif
        do k=1,NAEPS
          if (azLine(k)%get_n_points() > 0) then
            if (i > azLine(k)%get_n_points()) then
              binaryline = empty // sep // nod
            else
              write(binaryline,'(5es19.10e3)') azLine(k)%T(i), azLine(k)%P(i), &
                   azLine(k)%vl(i), azLine(k)%vg(i), azLine(k)%z(i,1)
            endif
            mergedline = trim(mergedline) // sep // trim(binaryline)
          endif
        enddo
        ! Write line to file
        write(ifile,'(A)') trim(mergedline)
      enddo
      close(ifile)
    end subroutine dump_global_plot_to_file
  end subroutine global_binary_plot

  subroutine VLLE_Line_clear(vlleL)
    class(VLLE_Line), intent(inout) :: vlleL
    ! Locals
    integer :: err
    if (allocated(vlleL%T)) then
      deallocate (vlleL%T, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%T!')
    endif
    if (allocated(vlleL%P)) then
      deallocate (vlleL%P, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%P!')
    endif
    if (allocated(vlleL%X)) then
      deallocate (vlleL%X, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%X!')
    endif
    if (allocated(vlleL%W)) then
      deallocate (vlleL%W, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%W!')
    endif
    if (allocated(vlleL%Y)) then
      deallocate (vlleL%Y, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%Y!')
    endif
    if (allocated(vlleL%vx)) then
      deallocate (vlleL%vx, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%vx!')
    endif
    if (allocated(vlleL%vw)) then
      deallocate (vlleL%vw, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%vw!')
    endif
    if (allocated(vlleL%vy)) then
      deallocate (vlleL%vy, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate vlleL%vy!')
    endif
  end subroutine VLLE_Line_clear

  subroutine VLLE_Line_allocate(vlleL,n)
    class(VLLE_Line), intent(inout) :: vlleL
    integer, intent(in) :: n
    ! Locals
    integer :: err
    call vlleL%clear()
    allocate (vlleL%T(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%T!')
    allocate (vlleL%P(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%P!')
    allocate (vlleL%X(n,2), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%X!')
    allocate (vlleL%W(n,2), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%W!')
    allocate (vlleL%Y(n,2), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%Y!')
    allocate (vlleL%vx(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%vx!')
    allocate (vlleL%vw(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%vw!')
    allocate (vlleL%vy(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate vlleL%vy!')
    vlleL%nPoints = 0
  end subroutine VLLE_Line_allocate

  subroutine VLLE_Line_push_back(vlleL,T,P,X,W,Y,vx,vw,vy)
    class(VLLE_Line), intent(inout) :: vlleL
    real, intent(in) :: T,P,X(2),W(2),Y(2),vx,vw,vy
    ! Locals
    integer :: n
    type(VLLE_Line) :: vlleLTemp

    if (.not. allocated(vlleL%T)) then
      n = 1000
      call vlleL%allocate(n)
    else
      n = size(vlleL%T)
    endif
    if (vlleL%nPoints == n) then
      vlleLTemp = vlleL
      ! Increase memory
      n = n + 1000
      call vlleL%allocate(n)
      vlleL = vlleLTemp
    endif
    vlleL%nPoints = vlleL%nPoints + 1
    vlleL%T(vlleL%nPoints) = T
    vlleL%P(vlleL%nPoints) = P
    vlleL%X(vlleL%nPoints,:) = X
    vlleL%W(vlleL%nPoints,:) = W
    vlleL%Y(vlleL%nPoints,:) = Y
    vlleL%vx(vlleL%nPoints) = vx
    vlleL%vw(vlleL%nPoints) = vw
    vlleL%vy(vlleL%nPoints) = vy
    !call vlleL%print_index(vlleL%nPoints)
  end subroutine VLLE_Line_push_back

  subroutine VLLE_Line_print_index(vlleL,idx)
    class(VLLE_Line), intent(in) :: vlleL
    integer, intent(in) :: idx
    ! Locals
    print *,"VLLE point ",idx
    print *,"T: ",vlleL%T(idx)
    print *,"P: ",vlleL%P(idx)
    print *,"X: ",vlleL%X(idx,:)
    print *,"W: ",vlleL%W(idx,:)
    print *,"Y: ",vlleL%Y(idx,:)
    print *,"vx: ",vlleL%vx(idx)
    print *,"vw: ",vlleL%vw(idx)
    print *,"vy: ",vlleL%vy(idx)
  end subroutine VLLE_Line_print_index

  subroutine VLLE_Line_swap_with_last(vlleL,T,P,X,W,Y,vx,vw,vy)
    class(VLLE_Line), intent(inout) :: vlleL
    real, intent(in) :: T,P,X(2),W(2),Y(2),vx,vw,vy
    ! Locals
    real :: Tt,Pt,Xt(2),Wt(2),Yt(2),vxt,vwt,vyt

    Tt = vlleL%T(vlleL%nPoints)
    Pt = vlleL%P(vlleL%nPoints)
    Xt = vlleL%X(vlleL%nPoints,:)
    Wt = vlleL%W(vlleL%nPoints,:)
    Yt = vlleL%Y(vlleL%nPoints,:)
    vxt = vlleL%vx(vlleL%nPoints)
    vwt = vlleL%vw(vlleL%nPoints)
    vyt = vlleL%vy(vlleL%nPoints)

    vlleL%T(vlleL%nPoints) = T
    vlleL%P(vlleL%nPoints) = P
    vlleL%X(vlleL%nPoints,:) = X
    vlleL%W(vlleL%nPoints,:) = W
    vlleL%Y(vlleL%nPoints,:) = Y
    vlleL%vx(vlleL%nPoints) = vx
    vlleL%vw(vlleL%nPoints) = vw
    vlleL%vy(vlleL%nPoints) = vy

    call vlleL%push_back(Tt,Pt,Xt,Wt,Yt,vxt,vwt,vyt)
  end subroutine VLLE_Line_swap_with_last

  function VLLE_Line_get_size(vllel) result(n)
    class(VLLE_Line), intent(in) :: vllel
    integer :: n
    !
    if (allocated(vllel%T)) then
      n = size(vllel%T)
    else
      n = 0
    endif
  end function VLLE_Line_get_size

  function VLLE_Line_get_npoints(vllel) result(n)
    class(VLLE_Line), intent(in) :: vllel
    integer :: n
    !
    if (allocated(vllel%T)) then
      n = vllel%npoints
    else
      n = 0
    endif
  end function VLLE_Line_get_npoints

  subroutine VLLE_Line_assign(vllel1,vllel2)
    class(VLLE_Line), intent(inout) :: vllel1
    class(VLLE_Line), intent(in)    :: vllel2
    ! Locals
    integer :: n
    n = vllel2%get_size()
    if (vllel1%get_size() < n) then
      call vllel1%allocate(n)
    endif
    ! Copy values
    vllel1%T(1:n) = vllel2%T
    vllel1%P(1:n) = vllel2%P
    vllel1%X(1:n,:) = vllel2%X
    vllel1%W(1:n,:) = vllel2%W
    vllel1%Y(1:n,:) = vllel2%Y
    vllel1%vx(1:n) = vllel2%vx
    vllel1%vw(1:n) = vllel2%vw
    vllel1%vy(1:n) = vllel2%vy
    vllel1%npoints = n
  end subroutine VLLE_Line_assign

  !-------------------------------------------------------------------
  !> Loop LLVE line
  !>
  !> \author MH, 2019-05
  !-------------------------------------------------------------------
  subroutine loop_LLVE_line(Tmin,Pmin,Pmax,iTermination,lLLVE,haep)
    !use critical, only: calcCriticalEndPoint
    use eos, only: specificvolume
    use eosTV, only: pressure
    use saturation_curve, only: aep, AZ_HAEP
    implicit none
    integer, intent(out) :: iTermination
    real, intent(in) :: Tmin !<
    real, intent(in) :: Pmax !<
    real, intent(in) :: Pmin !<
    type(VLLE_Line), intent(inout) :: lLLVE
    type(aep), optional, intent(out) :: haep
    ! Locals
    real, parameter :: tol = 1.0e-7
    integer :: s, ierr, iter, i
    real :: ds, XX(10), XXold(10), dXds(10), sgn, XXaep(10)
    real, parameter :: dzLim = 0.02
    real :: T,P,vx,vw,vy,x(2),w(2),y(2),lambda_old
    integer :: smax(1)
    real :: dxvl, dxvl_old
    integer :: i_vap, i_liq

    if (lLLVE%get_n_points() /= 1) then
      call stoperror("loop_LLVE_line: Only initial point must be given.")
    endif
    T = lLLVE%T(1)
    P = lLLVE%P(1)
    vx = lLLVE%vx(1)
    vw = lLLVE%vw(1)
    vy = lLLVE%vy(1)
    x = lLLVE%x(1,:)
    w = lLLVE%w(1,:)
    y = lLLVE%y(1,:)
    if (minval(y) > 1.0e-12) then
      s = 11 !
    else
      s = 13
    endif
    !
    call setX_LLVE(T,vx,vw,vy,x,w,y,XXold)
    call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
    call setX_LLVE(T,vx,vw,vy,x,w,y,XX)
    iTermination = addPoint()
    if (iTermination /= BP_TERM_NONE) return
    s = 10
    ds = 0.01
    sgn = sign(1.0,XX(s)-XXold(s))
    lambda_old = 0.0

    if (present(haep)) then
      i = maxloc(XX(7:9),dim=1) + 6
      if (i == 7) then
        i_vap = 1
        if (abs(XX(1)-XX(3)) < abs(XX(1)-XX(5))) then
          i_liq = 3
        else
          i_liq = 5
        endif
      else if (i == 8) then
        i_vap = 3
        if (abs(XX(1)-XX(3)) < abs(XX(3)-XX(5))) then
          i_liq = 1
        else
          i_liq = 5
        endif
      else
        i_vap = 5
        if (abs(XX(1)-XX(5)) < abs(XX(3)-XX(5))) then
          i_liq = 1
        else
          i_liq = 3
        endif
      endif
      haep%found = .false.
      haep%type = AZ_HAEP
      dxvl_old = XX(i_liq)-XX(i_vap)
    endif

    ! Loop critical line
    do i=1,10000
      ! Extrapolate state
      call LLVE_TV_sensitivity(P,XX,dXdS,s,ierr)
      smax = maxloc(abs(dXdS))
      if (smax(1) /= s) then
        s = smax(1)
        ! Rescaling the sensitivities
        sgn = sign(1.0,XX(s) - XXold(s))
        dXdS = dXdS / dXdS(s)
      endif
      XXold = XX
      XX = XXold + dXdS*dS*sgn
      ! if (X(1) < dzLim .and. dXdS(1)*sgn < 0.0) then
      !   s = 1
      !   if (X(1) < 0.0) then
      !     dS = -sgn*Xold(1)/dXdS(1)
      !     X = Xold + dXdS*dS*sgn
      !     X(1) = 0.0
      !     call getPropFromX(X,Tc,vc,Zc,Pc)
      !     call calcCriticalTV(Tc,vc,Zc,ierr,tol)
      !     Pc = pressure(Tc,vc,Zc)
      !     iTermination = addPoint()
      !     if (iTermination /= BP_TERM_NONE) return
      !     iTermination = BP_TERM_SINGLE
      !     return
      !   endif
      ! else if (X(1) > 1.0 - dzLim .and. dXdS(1)*sgn > 0.0) then
      !   s = 1
      !   if (X(1) > 1.0) then
      !     dS = sgn*(1.0 - Xold(1))/dXdS(1)
      !     X = Xold + dXdS*dS*sgn
      !     X(1) = 1.0
      !     call getPropFromX(X,Tc,vc,Zc,Pc)
      !     call calcCriticalTV(Tc,vc,Zc,ierr,tol)
      !     Pc = pressure(Tc,vc,Zc)
      !     iTermination = addPoint()
      !     if (iTermination /= BP_TERM_NONE) return
      !     iTermination = BP_TERM_SINGLE
      !     return
      !   endif
      ! endif
      call getPropFromX_LLVE(XX,T,vx,vw,vy,x,w,y)
      call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr,iter=iter)
      if (ierr == 0) then
        if (T < Tmin) then
          s = 10
          T = Tmin
          call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_TMIN
          return
        else if (P > Pmax) then
          s = 12
          P = Pmax
          call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_PMAX
          return
        else if (P < Pmin) then
          s = 12
          P = Pmin
          call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_PMIN
          return
        endif
      endif
      iTermination = addPoint()
      if (iTermination /= BP_TERM_NONE) return
      if (present(haep)) then
        if (.not. haep%found) then
          dxvl = XX(i_liq)-XX(i_vap)
          if (dxvl*dxvl_old < 0) then
            ! Search for HAEP
            call solve_for_haep(XXold,XX,XXaep,s,i_vap,i_liq,haep,ierr)
            if (ierr == 0) then
              call getPropFromX_LLVE(XXaep,T,vx,vw,vy,x,w,y)
              P = pressure(T,vx,x)
              call lLLVE%swap_with_last(T,P,X,W,Y,vx,vw,vy)
            endif
            !stop
          endif
          dxvl_old = dxvl
        endif
      endif
    enddo

    iTermination = BP_TERM_NPOINT

  contains
    function addPoint() result(iTerm)
      use eosTV, only: pressure
      use critical, only: calcStabMinEigTV, calcCriticalEndPoint
      integer :: lmax(1)
      integer :: iTerm
      real :: lambda(3),lam,vc,Zc(2),Zy(2),vZy
      iTerm = BP_TERM_NONE
      if (ierr /= 0) then
        iTerm = BP_TERM_ERR
        return
      else
        P = pressure(T,vx,x)
        !print *,"LLVE T,P",T,P
        lambda(1) = calcStabMinEigTV(t,vx,x)
        lambda(2) = calcStabMinEigTV(t,vw,w)
        lambda(3) = calcStabMinEigTV(t,vy,y)
        lam = minval(lambda)
        if (lam < 0.0005 .and. lam < lambda_old) then
          lmax = maxloc(lambda)
          if (lmax(1) == 2) then
            Zy = w
            vZy = vw
            Zc = 0.5*(x+y)
            vc = 0.5*(vx+vy)
          else if (lmax(1) == 3) then
            Zy = y
            vZy = vy
            Zc = 0.5*(x+w)
            vc = 0.5*(vx+vw)
          else
            Zy = x
            vZy = vx
            Zc = 0.5*(w+y)
            vc = 0.5*(vw+vy)
          endif
          call calcCriticalEndPoint(T,vc,Zc,Zy,vZy,ierr,tol)
          P = pressure(T,vc,Zc)
          if (ierr /= 0) then
            iTerm = BP_TERM_ERR
            return
          else
            iTerm = BP_TERM_TPD
            if (lmax(1) == 2) then
              w = Zy
              vw = vZy
              x = Zc
              y = Zc
              vx = vc
              vy = vc
            else if (lmax(1) == 3) then
              y = Zy
              vy = vZy
              w = Zc
              vw = vc
              x = Zc
              vx = vc
            else
              x = Zy
              vx = vZy
              w = Zc
              y = Zc
              vw = vc
              vy = vc
            endif
          endif
        endif
        call lLLVE%push_back(T,P,X,W,Y,vx,vw,vy)
        call setX_LLVE(T,vx,vw,vy,x,w,y,XX)
        lambda_old = minval(lambda)
      endif
    end function addPoint
  end subroutine loop_LLVE_line

  subroutine getPropFromX_LLVE(XX,T,vx,vw,vy,x,w,y)
    real, intent(out) :: T,vx,vw,vy,x(2),w(2),y(2)
    real, intent(in) :: XX(10)
    x(1:2) = exp(XX(1:2))
    w(1:2) = exp(XX(3:4))
    y(1:2) = exp(XX(5:6))
    vx = exp(XX(7))
    vw = exp(XX(8))
    vy = exp(XX(9))
    T = exp(XX(10))
  end subroutine getPropFromX_LLVE

  subroutine setX_LLVE(T,vx,vw,vy,x,w,y,XX)
    real, intent(in) :: T,vx,vw,vy,x(2),w(2),y(2)
    real, intent(out) :: XX(10)
    XX(1:2) = log(x)
    XX(3:4) = log(w)
    XX(5:6) = log(y)
    XX(7) = log(vx)
    XX(8) = log(vw)
    XX(9) = log(vy)
    XX(10) = log(T)
  end subroutine setX_LLVE

  subroutine solve_for_haep(XXold,XXcurrent,XX,s,i_vap,i_liq,haep,ierr)
    use nonlinear_solvers, only: nonlinear_solver, bracketing_solver, NS_PEGASUS
    use saturation_curve, only: aep
    integer, intent(in) :: s,i_vap,i_liq
    real, intent(in) :: XXold(10), XXcurrent(10) !<
    real, intent(out) :: XX(10)
    type(aep), intent(inout) :: haep
    integer, intent(out) :: ierr          ! error flag
    ! Locals
    real, parameter :: tol = 1.0e-6
    real :: s_min, s_max, param(23), ds
    real :: T,P,vx,vw,vy,x(2),w(2),y(2)
    type(nonlinear_solver) :: solver_cfg
    param(1:10) = XXold
    param(11:20) = XXcurrent - XXold
    param(21) = real(s)
    param(22) = real(i_vap)
    param(23) = real(i_liq)
    s_min = 0
    s_max = 1
    ! Configure solver
    solver_cfg%abs_tol = tol
    solver_cfg%max_it = 1000
    solver_cfg%isolver = NS_PEGASUS
    ! Find f=0 inside the bracket.
    call bracketing_solver(s_min,s_max,fun_haep,ds,solver_cfg,param)
    ! Check for successful convergence
    if (solver_cfg%exitflag /= 0) then
      if (verbose) write(*,*) "HAEP: Bracketing solver failed."
      ierr = solver_cfg%exitflag
      return
    else
      ierr = 0
      haep%found = .true.
      XX = XXold + param(11:20)*ds
      call getPropFromX_LLVE(XX,T,vx,vw,vy,x,w,y)
      call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
      call setX_LLVE(T,vx,vw,vy,x,w,y,XX)
      haep%x = exp(XX(i_vap:i_vap+1))
      haep%T = T
      haep%P = P
      if (i_vap == 1) then
        haep%vg = vx
      else if (i_vap == 3) then
        haep%vg = vw
      else
        haep%vg = vy
      endif
      if (i_liq == 1) then
        haep%vl = vx
      else if (i_liq == 3) then
        haep%vl = vw
      else
        haep%vl = vy
      endif
    endif

  end subroutine solve_for_haep

  function fun_haep(ds,param) result(f)
    ! Objective function for x(1) - y(1) = 0
    !
    implicit none
    ! Input:
    real,     intent(in)  :: ds !<
    real,     intent(in)  :: param(23) !< Parameters
    ![i_pure,i_insip,T_guess]
    ! Output:
    real                  :: f !< s_sat - sspec
    ! Internal:
    integer :: s,i_vap,i_liq,ierr
    real :: XX(10), XXold(10), dXXds(10)
    real :: T,P,vx,vw,vy,x(2),w(2),y(2)
    ! Read from param
    XXold = param(1:10)
    dXXds = param(11:20)
    s = nint(param(21))
    i_vap = nint(param(22))
    i_liq = nint(param(23))

    ! Extrapolate state
    XX = XXold + dXXds*ds
    call getPropFromX_LLVE(XX,T,vx,vw,vy,x,w,y)
    call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
    call setX_LLVE(T,vx,vw,vy,x,w,y,XX)
    ! Make objective function
    f = XX(i_liq)-XX(i_vap)
  end function fun_haep

  !-------------------------------------------------------------------
  !> Loop azeotropic line
  !>
  !> \author MH, 2019-10
  !-------------------------------------------------------------------
  subroutine loop_azeotropic_line(Pmin,Pmax,Tmin,xaep,iTermination,azline)
    use thermopack_var, only: nc
    use eos, only: getCriticalParam
    use critical, only: calcCriticalEndPoint, calcCriticalZ, critZsensitivity, &
         calcCriticalTV
    use saturation_curve, only: aep,AZ_PAEP,AZ_CAEP,AZ_HAEP
    use numconstants, only: small
    implicit none
    type(aep), intent(in) :: xaep
    integer, intent(out) :: iTermination
    real, intent(in) :: Pmin,Pmax,Tmin !<
    type(azeotropicLine), intent(inout) :: azline
    ! Locals
    real, parameter :: tol = 1.0e-7
    integer :: s, ierr, smax, iter, i, ic
    real :: ds, X(4), Xold(4), dXds(4), sgn, Pmax_term
    real :: Z(nc), P, T, vg, vl
    real :: vg_ext, vl_ext
    real, parameter :: dzLim = 0.02
    real, parameter :: tuning = 1.2, ds_min = 0.001, ds_max = 0.005
    integer, parameter :: SIG_NO_TPD = -101010
    ! Copy state
    Z = xaep%x
    P = xaep%P
    T = xaep%T
    vg = xaep%vg
    vl = xaep%vl
    ic = 1
    ierr = SIG_NO_TPD
    iTermination = addPoint()
    Xold = X
    select case(xaep%type)
    case (AZ_PAEP)
      s = 2
      if (abs(Z(1)- 1) < small) then
        Z(1) = 0.995
      else
        Z(1) = 0.005
      endif
      Z(2) = 1 - Z(1)
      call calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol)
      iTermination = addPoint()
      if (iTermination /= BP_TERM_NONE) return
      call setXaz(T,vg,vl,Z,X,ic)
      call azSensitivity(1,X,dXdS,s,ierr)
      s = maxloc(abs(dXdS),dim=1)
      sgn = sign(1.0,X(s) - Xold(s))
      ds = ds_min
      Pmax_term = Pmax
    case (AZ_CAEP)
      s = 0
      vg = 1.01*vg
      vl = 0.99*vl
      call calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol)
      iTermination = addPoint()
      if (iTermination /= BP_TERM_NONE) return
      s = 1
      call setXaz(T,vg,vl,Z,X,ic)
      call azSensitivity(1,X,dXdS,s,ierr)
      s = maxloc(abs(dXdS),dim=1)
      sgn = sign(1.0,X(s) - Xold(s))
      ds = ds_min
      Pmax_term = Pmax
    case (AZ_HAEP)
     stop "Should not be required to start from AZ_HAEP"
   case default
     stop "Unknown AEP"
    end select

    ! Loop azeotropic line
    do i=1,10000
      ! Extrapolate state
      call azSensitivity(1,X,dXdS,s,ierr)
      smax = maxloc(abs(dXdS),dim=1)
      if ((.not. smax == s) .and. i > 1) then
        s = smax
        ! Rescaling the sensitivities
        sgn = sign(1.0,X(s) - Xold(s))
        dXdS = dXdS / dXdS(s)
      endif
      Xold = X
      X = Xold + dXdS*dS*sgn
      vl_ext = exp(X(3))
      vg_ext = exp(X(4))
      if (X(2) < dzLim .and. dXdS(2)*sgn < 0.0) then
        s = 2
        if (X(2) < 0.0) then
          dS = -sgn*Xold(2)/dXdS(2)
          X = Xold + dXdS*dS*sgn
          X(2) = 0.0
          call getPropFromXaz(X,T,vg,vl,Z,ic)
          call calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_SINGLE
          return
        endif
      else if (X(2) > 1.0 - dzLim .and. dXdS(2)*sgn > 0.0) then
        s = 2
        if (X(2) > 1.0) then
          dS = sgn*(1.0 - Xold(1))/dXdS(1)
          X = Xold + dXdS*dS*sgn
          X(2) = 1.0
          call getPropFromXaz(X,T,vg,vl,Z,ic)
          call calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol)
          iTermination = addPoint()
          if (iTermination /= BP_TERM_NONE) return
          iTermination = BP_TERM_SINGLE
          return
        endif
      else if (X(1) < log(Tmin)) then
        s = 1
        dS = sgn*(log(Tmin) - Xold(1))/dXdS(1)
        X = Xold + dXdS*dS*sgn
        call getPropFromXaz(X,T,vg,vl,Z,ic)
        call calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol)
        iTermination = addPoint()
        if (iTermination /= BP_TERM_NONE) return
        iTermination = BP_TERM_TMIN
        return
      else if (vg_ext < vl_ext .or. &
           (vg_ext/vl_ext < 1.1 .and. vg_ext/vl_ext < vg/vl) ) then
        ! Approaching critical point
        if (vg_ext/vl_ext < 1.02) then
          ! Solve for CAEP
          ! Extrapolate to vg/vl=1
          ds = (Xold(3) - Xold(4))/(dXdS(4) - dXdS(3))/sgn
          X = Xold + dXdS*dS*sgn
          call getPropFromXaz(X,T,vg,vl,Z,ic)
          ierr = SIG_NO_TPD ! Signal not to do stability check
          iTermination = addPoint()
          iTermination = BP_TERM_CRIT
          return
        else
          ! Specify vg/vl
          s = 0
        endif
      endif
      call getPropFromXaz(X,T,vg,vl,Z,ic)
      call calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol,iter=iter)
      iTermination = addPoint()
      if (iTermination == BP_TERM_ERR .and. ds > 1.05*ds_min) cycle
      if (iTermination /= BP_TERM_NONE) return
      if (P > Pmax_term) then
        iTermination = BP_TERM_PMAX
        return
      endif
      if (P < Pmin) then
        iTermination = BP_TERM_PMIN
        return
      endif

      ! Tune dS up or down based on how fast sat_newton converged
      if (iter < 3) then
        ds = ds * tuning
      else if (iter > 5) then
        ds = ds * (2.0 - tuning)
      endif
      ds = max(min(ds,ds_max),ds_min)
    enddo

    iTermination = BP_TERM_NPOINT

  contains
    function addPoint() result(iTerm)
      integer :: iTerm
      ! Locals
      real :: xl2(nc), vl2, xl(nc)
      if (ierr == 0 .or. ierr == SIG_NO_TPD) then
        iTerm = BP_TERM_NONE
        if (ierr /= SIG_NO_TPD) then ! Disable stability test
          if (.not. az_is_stable(T,P,Z,xl2,vl2)) then
            iTerm = BP_TERM_TPD
            xl = Z
            call LLVEpointTV(P,T,vl,vl2,vg,xl,xl2,Z,ispec=14,ierr=ierr)
            if (ierr /= 0) then
              iTerm = BP_TERM_ERR
              return
            endif
          endif
        endif
      else
        iTerm = BP_TERM_ERR
      endif
      if (iTerm /= BP_TERM_ERR) then
        call azLine%push_back(T,vg,vl,P,Z)
        call setXaz(T,vg,vl,Z,X,ic)
      else
         ds = 0.5*ds
         X = Xold
       endif
    end function addPoint
  end subroutine loop_azeotropic_line

  !-------------------------------------------------------------------
  !> Test for phase stabillity of azeotrop
  !>
  !> \author MH, 2019-10
  !-------------------------------------------------------------------
  function az_is_stable(T,P,z,x,v) result(isStable)
    use thermopack_constants, only: LIQPH, VAPPH
    use thermopack_var, only: nc
    use eos, only: specificvolume, thermo
    use stability, only: stabilityLimit, stabcalcW
    implicit none
    real, dimension(nc), intent(in) :: z !< Phase compositions
    real, intent(in) :: T !<
    real, intent(in) :: P !<
    real, dimension(nc), intent(out) :: x !<
    real, intent(out) :: v !<
    logical :: isStable
    !
    real :: tpd
    real :: lnFugZ(nc), lnFugX(nc)
    real, dimension(1,nc) :: XX

    call thermo(t,p,Z,VAPPH,lnFugZ)
    isStable = .true.
    XX(1,:) = Z

    ! Perform minimization determine stability and to get best possible starting values
    x = 0
    x(1) = 1
    tpd = stabcalcW(1,1,T,P,XX,x,LIQPH,lnFugZ,lnFugX,preTermLim=-1000.0)
    if (tpd < stabilityLimit*1000.0) then
      isStable = .false.
      call specificvolume(T,P,x,LIQPH,v)
      return
    endif

    x = 0
    x(2) = 1
    tpd = stabcalcW(1,1,T,P,XX,x,LIQPH,lnFugZ,lnFugX,preTermLim=-1000.0)
    if (tpd < stabilityLimit*1000.0) then
      isStable = .false.
      call specificvolume(T,P,x,LIQPH,v)
    endif

  end function az_is_stable

  subroutine getPropFromXaz(X,T,vg,vl,Z,ic)
    real, intent(out) :: T,vg,vl,Z(2)
    real, intent(in) :: X(4)
    integer, intent(in) :: ic
    T = exp(X(1))
    Z = 1 - X(2)
    Z(ic) = X(2)
    vl = exp(X(3))
    vg = exp(X(4))
  end subroutine getPropFromXaz

  subroutine setXaz(T,vg,vl,Z,X,ic)
    real, intent(in) :: T,vg,vl,Z(2)
    real, intent(out) :: X(4)
    integer, intent(in) :: ic
    X(1) = log(T)
    X(2) = Z(ic)
    X(3) = log(vl)
    X(4) = log(vg)
  end subroutine setXaz

  !-------------------------------------------------------------------------
  !> Calculate critical point specified z (s=1), T (s=2), V (s=3) or P (s=4)
  !! Good initial values are assumed
  !!
  !! \author MH, 2019-04
  !-------------------------------------------------------------------------
  subroutine calcAzeotropicPoint(t,vg,vl,P,Z,s,ierr,tol,free_comp,iter)
    use thermopack_constants, only: get_templimits
    use eosdata, only: eosCPA
    use numconstants, only: Small
    use utilities, only: isXwithinBounds
    use nonlinear_solvers, only: nonlinear_solver, limit_dx,&
         premterm_at_dx_zero, setXv, nonlinear_solve
    use eosTV, only: pressure
    use thermo_utils, only: get_b_linear_mix
    implicit none
    real, dimension(nc), intent(inout) :: Z !< Trial composition (Overall compozition)
    real, intent(inout) :: t !< Temperature [K]
    real, intent(inout) :: vg,vl !< Volume [m3/mol]
    real, intent(out) :: P !< Pressure [Pa]
    integer, intent(in) :: s !< Specification
    integer, intent(out) :: ierr !< Error flag
    real, optional, intent(in) :: tol !< Toleranse
    integer, optional, intent(in) :: free_comp !< Component variable
    integer, optional, intent(out) :: iter !< Number of iterations
    ! Locals
    real :: t0, vg0, vl0, z0(nc), b
    real, dimension(3) :: param
    real, dimension(4) :: X, xmax, xmin
    !real :: Fun(4), dF(4,4), Fun2(4), numJac(4,4), X1(4), eps
    type(nonlinear_solver) :: solver
    integer :: ic
    logical :: needalt, isCPA
    type(thermo_model), pointer :: act_mod_ptr

    if (nc == 1 .or. nc > 2) then
      call stoperror("calcAzeotropicPoint: Only two components can be active.")
    endif
    act_mod_ptr => get_active_thermo_model()
    needalt = act_mod_ptr%need_alternative_eos
    isCPA = (act_mod_ptr%eosidx == eosCPA)

    ierr = 0
    vg0 = vg
    vl0 = vl
    t0 = t
    z0 = z
    param(3) = real(s)
    if (present(free_comp)) then
      ic = free_comp
    else
      ic = 1
    endif
    param(1) = real(ic) ! Free component
    call setXaz(T,vg,vl,Z,X,ic)

    if (s == 0) then
      param(2) = log(1.01/0.99)
    else
      param(2) = X(s)
    endif
    !...................................
    ! print *,"Testing differentials"
    ! eps = 1.0e-6
    ! call azFun(Fun,X,param)
    ! !print *,'Fun',Fun
    ! call azJac(dF,X,param)
    ! X1 = X
    ! X1(1) = X1(1) + X1(1)*eps
    ! call azFun(Fun2,X1,param)
    ! !print *,'Fun2 z',Fun2
    ! numJac(:,1) = (Fun2-Fun)/(X1(1)*eps)
    ! X1 = X
    ! X1(2) = X1(2) + X1(2)*eps
    ! call azFun(Fun2,X1,param)
    ! !print *,'Fun2 lnT',Fun2
    ! numJac(:,2) = (Fun2-Fun)/(X1(2)*eps)
    ! X1 = X
    ! X1(3) = X1(3) + X1(3)*eps
    ! call azFun(Fun2,X1,param)
    ! !print *,'Fun2 lnV',Fun2
    ! numJac(:,3) = (Fun2-Fun)/(X1(3)*eps)
    ! X1 = X
    ! X1(4) = X1(4) + X1(4)*eps
    ! call azFun(Fun2,X1,param)
    ! !print *,'Fun2 lnP',Fun2
    ! numJac(:,4) = (Fun2-Fun)/(X1(4)*eps)
    ! print *,'jac'
    ! print *,numJac(:,1)
    ! print *,dF(:,1)
    ! print *,numJac(:,2)
    ! print *,dF(:,2)
    ! print *,numJac(:,3)
    ! print *,dF(:,3)
    ! print *,numJac(:,4)
    ! print *,dF(:,4)
    ! call exit(1)

    solver%abs_tol = 1.0e-6
    if (present(tol)) then
      solver%abs_tol = tol
    endif
    solver%rel_tol = 1.0e-20
    solver%max_it = 200
    ! Temperature
    call get_templimits(xmin(1), xmax(1))
    xmin(1) = log(xmin(1))
    xmax(1) = log(xmax(1))
    ! Volumes
    if (needalt .and. .not. isCPA) then
      xmin(3:4) = log(1.0e-8)
    else
      ! Calculate co-volume
      b = get_b_linear_mix(z)
      xmin(3) = b + Small ! m3/mol
      xmin(3:4)= log(xmin(3))
    endif
    xmax(3:4) = 100.0
    ! Composition
    xmax(2) = 1.0
    xmin(2) = 0.0
    call isXwithinBounds(4,X,Xmin,Xmax,"ln(t),z,ln(vl),ln(vg)",&
         "calcAzeotropicPoint: Initial values not withon bounds!!")
    call nonlinear_solve(solver,azFun,azJac,azJac,limit_dx,&
         premterm_at_dx_zero, setXv,x,xmin,xmax,param)
    ierr = solver%exitflag
    if (present(iter)) then
      iter = solver%iter
    endif

    ! Solution
    z = 1.0-X(2)
    z(ic) = X(2)
    t = exp(X(1))
    vl = exp(X(3))
    vg = exp(X(4))
    P = pressure(t,vl,z)
    if (ierr /= 0) then
      if (verbose) then
        print *,'Not able to locate critical point'
        print *,'Initial liquid volume (m3/mol): ', vl0
        print *,'Initial gas volume (m3/mol): ', vg0
        print *,'Initial temperature (K): ', t0
        print *,'Initial composition (-): ', z0
        print *,'Specification : ', s
      endif
    endif
  end subroutine calcAzeotropicPoint

  !-------------------------------------------------------------------------
  !> Function value for calculation of azeotropic point for mixtures.
  !>
  !>
  !> \author MH, 2019-10
  !-------------------------------------------------------------------------
  subroutine azFun(Fun,X,param)
    use eosTV, only: pressure, thermo_tv
    implicit none
    real, dimension(4), intent(out) :: Fun !< Function value
    real, dimension(4), intent(in) :: X !< Variables
    real, dimension(3), intent(in) :: param !< Parameters
    ! Locals
    real, dimension(nc) :: Z !< Compozition
    real :: t !< Temperature [K]
    real :: vg, vl, Pg, Pl
    integer :: ic, s
    real, dimension(nc) :: lnfg,lnfl
    real :: s_spec
    ! Get paramenerts
    ic = nint(param(1))
    s_spec = param(2)
    s = nint(param(3))
    ! Set variables
    call getPropFromXaz(X,T,vg,vl,Z,ic)
    !
    Pg = pressure(t,vg,z)
    call thermo_tv(t,vg,z,lnfg)
    Pl = pressure(t,vl,z)
    call thermo_tv(t,vl,z,lnfl)

    fun(1) = log(Pl) - log(Pg)
    fun(2:3) = lnfl - lnfg
    if (s == 0) then
      fun(4) = X(4) - X(3) - s_spec
    else
      fun(4) = X(s) - s_spec
    endif
  end subroutine azFun

  !-------------------------------------------------------------------------
  !> Differentials of azeotropic equation system.
  !>
  !>
  !> \author MH, 2019-10
  !-------------------------------------------------------------------------
  subroutine azJac(dF,X,param)
    use eosTV, only: pressure, thermo_tv
    implicit none
    real, dimension(4,4), intent(out) :: dF !< Function differential
    real, dimension(4), intent(in) :: X !< Variables
    real, dimension(3), intent(in) :: param !< Parameters
        ! Locals
    real, dimension(nc) :: Z !< Compozition
    real :: t !< Temperature [K]
    real :: vg, vl, Pg, Pl, Pg_T, Pl_T, Pg_v, Pl_v
    integer :: ic, s, is
    real, dimension(nc) :: lnfg,lnfl,lnfg_T,lnfl_T,lnfg_v,lnfl_v,Pg_n,Pl_n
    real, dimension(nc,nc) :: lnfg_n,lnfl_n
    real :: s_spec
    ! Get paramenerts
    ic = nint(param(1))
    if (ic == 1) then
      is = 2
    else
      is = 1
    endif
    s_spec = param(2)
    s = nint(param(3))
    ! Set variables
    call getPropFromXaz(X,T,vg,vl,Z,ic)
    !
    Pg = pressure(t,vg,z,dpdv=Pg_v,dpdt=Pg_T,dpdn=Pg_n)
    call thermo_tv(t,vg,z,lnfg,lnfg_t,lnfg_v,lnfg_n)
    Pl = pressure(t,vl,z,dpdv=Pl_v,dpdt=Pl_T,dpdn=Pl_n)
    call thermo_tv(t,vl,z,lnfl,lnfl_t,lnfl_v,lnfl_n)
    !
    !fun(1) = log(Pl) - log(Pg)
    dF(1,1) = T*(Pl_T/Pl - Pg_T/Pg)
    dF(1,2) = (Pl_n(ic)-Pl_n(is))/Pl - (Pg_n(ic)-Pg_n(is))/Pg
    dF(1,3) = vl*Pl_v/Pl
    dF(1,4) = - vg*Pg_v/Pg

    !fun(2:3) = lnfl - lnfg
    dF(2:3,1) = T*(lnfl_T - lnfg_T)
    dF(2,2) = (lnfl_n(1,ic)-lnfl_n(1,is)) - (lnfg_n(1,ic)-lnfg_n(1,is))
    dF(3,2) = (lnfl_n(2,ic)-lnfl_n(2,is)) - (lnfg_n(2,ic)-lnfg_n(2,is))
    dF(2:3,3) = vl*lnfl_v
    dF(2:3,4) = - vg*lnfg_v

    dF(4,:) = 0
    if (s == 0) then
      !fun(4) = X(4) - X(3) - s_spec
      dF(4,3) = -1
      dF(4,4) = 1
    else
      !fun(4) = X(s) - s_spec
      dF(4,s) = 1
    endif

  end subroutine azJac

  !-------------------------------------------------------------------------
  !> Sensitivities of azeotropic equation system
  !>
  !>
  !> \author MH, 2019-10
  !-------------------------------------------------------------------------
  subroutine azSensitivity(ic,X,dXdS,s,ierr)
    implicit none
    real, dimension(4), intent(out) :: dXds !< System sensitivities
    real, dimension(4), intent(in) :: X !< Variables
    integer, intent(in) :: s !< Specification
    integer, intent(in) :: ic !< Component specification
    integer, intent(out) :: ierr !< Error flag
    ! Locals
    real, dimension(3) :: param
    real, dimension(4,4) :: Jac
    integer, dimension(4) :: INDX
    integer :: info
    !
    param(1) = real(ic)
    param(2) = 0.0
    param(3) = real(s)
    !
    call azJac(Jac,X,param)
    dXdS = 0.0
    dXdS(4) = 1.0

    ! Solve equation system
    call DGESV( 4, 1, Jac, 4, INDX, dXdS, 4, info )
    if (info /= 0) then
      ierr = 2
    endif
  end subroutine azSensitivity

  subroutine azeotropicLine_clear(azLine)
    class(azeotropicLine), intent(inout) :: azLine
    ! Locals
    integer :: err
    if (allocated(azLine%T)) then
      deallocate (azLine%T, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate azLine%T!')
    endif
    if (allocated(azLine%vg)) then
      deallocate (azLine%vg, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate azLine%vg!')
    endif
    if (allocated(azLine%vl)) then
      deallocate (azLine%vl, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate azLine%vl!')
    endif
    if (allocated(azLine%P)) then
      deallocate (azLine%P, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate azLine%P!')
    endif
    if (allocated(azLine%Z)) then
      deallocate (azLine%Z, STAT=err)
      if (err /= 0) Call StopError('Could not deallocate azLine%Z!')
    endif
  end subroutine azeotropicLine_clear

  subroutine azeotropicLine_allocate(azLine,n)
    class(azeotropicLine), intent(inout) :: azLine
    integer, intent(in) :: n
    ! Locals
    integer :: err
    call azLine%clear()
    allocate (azLine%T(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate azLine%T!')
    allocate (azLine%vg(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate azLine%vg!')
    allocate (azLine%vl(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate azLine%vl!')
    allocate (azLine%P(n), STAT=err)
    if (err /= 0) Call StopError('Could not allocate azLine%P!')
    allocate (azLine%Z(n,2), STAT=err)
    if (err /= 0) Call StopError('Could not allocate azLine%Z!')
    azLine%nPoints = 0
  end subroutine azeotropicLine_allocate

  subroutine azeotropicLine_push_back(azLine,T,vg,vl,P,z)
    class(azeotropicLine), intent(inout) :: azLine
    real, intent(in) :: T,vg,vl,P,z(2)
    ! Locals
    integer :: n
    type(azeotropicLine) :: azLineTemp

    if (.not. allocated(azLine%T)) then
      n = 1000
      call azLine%allocate(n)
    else
      n = size(azLine%T)
    endif
    if (azLine%nPoints == n) then
      azLineTemp = azLine
      ! Increase memory
      n = n + 1000
      call azLine%allocate(n)
      azLine = azLineTemp
    endif
    azLine%nPoints = azLine%nPoints + 1
    azLine%T(azLine%nPoints) = T
    azLine%P(azLine%nPoints) = P
    azLine%vg(azLine%nPoints) = vg
    azLine%vl(azLine%nPoints) = vl
    azLine%Z(azLine%nPoints,:) = Z
  end subroutine azeotropicLine_push_back

  function azeotropicLine_get_size(azl) result(n)
    class(azeotropicLine), intent(in) :: azl
    integer :: n
    !
    if (allocated(azl%T)) then
      n = size(azl%T)
    else
      n = 0
    endif
  end function azeotropicLine_get_size

  function azeotropicLine_get_npoints(azl) result(n)
    class(azeotropicLine), intent(in) :: azl
    integer :: n
    !
    if (allocated(azl%T)) then
      n = azl%npoints
    else
      n = 0
    endif
  end function azeotropicLine_get_npoints

  subroutine azeotropicLine_assign(azl1,azl2)
    class(azeotropicLine), intent(inout) :: azl1
    class(azeotropicLine), intent(in)    :: azl2
    ! Locals
    integer :: n
    n = azl2%get_size()
    if (azl1%get_size() < n) then
      call azl1%allocate(n)
    endif
    ! Copy values
    azl1%T(1:n) = azl2%T
    azl1%P(1:n) = azl2%P
    azl1%vg(1:n) = azl2%vg
    azl1%vl(1:n) = azl2%vl
    azl1%z(1:n,:) = azl2%z
    azl1%npoints = n
  end subroutine azeotropicLine_assign

end module binaryPlot

!-------------------------------------------------------------------
!> Test extraploation on critical line
!>
!> \author MH, 2019-05
!-------------------------------------------------------------------
subroutine test_LLVE_TV_sensitivity(P,XX0,s)
  use binaryPlot, only: LLVE_TV_sensitivity, LLVEpointTV, &
       getPropFromX_LLVE, setX_LLVE
  implicit none
  real, intent(in) :: XX0(10)
  ! Locals
  integer :: s, ierr
  real :: ds, XX1(10), XX2(10), dXds(10)
  real :: T,P,vx,vw,vy,x(2),w(2),y(2)
  !
  print *,"Testing LLVE_TV_sensitivity"
  !
  ! Extrapolate state
  call LLVE_TV_sensitivity(P,XX0,dXdS,s,ierr)
  print *,"ierr",ierr
  ds = 1.0e-8
  XX1 = XX0
  XX1(s) = XX0(s) + ds
  call getPropFromX_LLVE(XX1,T,vx,vw,vy,x,w,y)
  call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
  print *,"ierr",ierr
  call setX_LLVE(T,vx,vw,vy,x,w,y,XX1)
  XX2 = XX0
  XX2(s) = XX0(s) - ds
  call getPropFromX_LLVE(XX2,T,vx,vw,vy,x,w,y)
  call LLVEpointTV(P,T,vx,vw,vy,x,w,y,s,ierr)
  print *,"ierr",ierr
  call setX_LLVE(T,vx,vw,vy,x,w,y,XX2)
  ! Differentials
  print *,dXds
  print *,(XX1-XX2)/(2*ds)
  stop
end subroutine test_LLVE_TV_sensitivity
