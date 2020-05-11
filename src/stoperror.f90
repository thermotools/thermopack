module error
  use parameters, only: cLen
  implicit none

  integer :: err=1 !< Used in exit call: call exit(error)
  logical :: writeToFile = .false.   !< .true. Print error message
                                     !< .flase. Write error message to file

  logical :: dostop = .true. !< .true. exit when error occurs
                                        !< .false. try to continue
  logical :: lwin = .false.  !< .true. Stop and wait for keyboard input
  character(len=cLen) :: errorfile = 'error.dat'

contains

  subroutine initErrorHandler(err_in, writeToFile_in, dostop_in, &
       lwin_in, errorfile_in)
    implicit none
    integer, optional :: err_in
    logical, optional :: writeToFile_in
    logical, optional :: dostop_in 
    logical, optional :: lwin_in
    character(len=*), optional :: errorfile_in
    !
    if (present(err_in)) then
      err = err_in
    endif
    if (present(writeToFile_in)) then
      writeToFile = writeToFile_in
    endif
    if (present(dostop_in)) then
      dostop = dostop_in
    endif
    if (present(lwin_in)) then
      lwin = lwin_in
    endif
    if (present(errorfile_in)) then
      errorfile = errorfile_in
    endif
  end subroutine initErrorHandler
end module error

!-----------------------------------------------------------------------------
subroutine StopError(s)
  use error
#ifdef __INTEL_COMPILER
use ifcore, only: tracebackqq
#endif
  ! Called in all cases where the program have error
  ! Have 2 benefits:
  ! Can set breakpoint here and then see where the stop occured
  ! Can see why the stop occure
  !
  ! Now use subroutine exit (intrinsic) to give exit code 1. This 
  ! is beneficial for the nautotester automatic testing routines.
  ! Note: subroutine exit does not seem to be standard Fortran.
  ! However, all of pgf90, gfortran, g95, sunf90 and ifort have it.
  ! Should it not work, we might use 'stop 1' instead, but this does
  ! not always set the exit code, e.g. when using pgf90.
  !
  ! STM, 2009-07-29
  !
  character , intent(IN) :: s*(*)
  ! Using of variable dostop makes it posible to not stop here 
  ! from the debugger
  if (writeToFile) then
     open(15, FILE = trim(errorfile))
     if (len(trim(s)) > 0) then
        write(15,*) s
     else
        write(15,*) 'Unknown error'
     endif
     close(15)
   else 

#ifdef __GNUC__  
#if ((__GNUC__  ==4 ) && (__GNUC_MINOR__ >= 8 )) || (__GNUC__  >= 5 )
call backtrace 
#endif 
#endif

#ifdef __INTEL_COMPILER
call tracebackqq(USER_EXIT_CODE=-1)
#endif

     if (len(trim(s)) > 0) then
        write(*,*) s
     else
        write(*,*) 'Unknown error'
     endif
   endif
   if (lwin) then
     write(*,*) 'Press Return'
     read *
   endif
   if (dostop) call exit(err)
end subroutine StopError
