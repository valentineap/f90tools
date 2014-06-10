module mod_log

  ! Andrew Valentine
  ! Universiteit Utrecht
  ! a.p.valentine@uu.nl
  ! May 2014

use iso_fortran_env,only:error_unit,output_unit,compiler_version,compiler_options
implicit none
private
public::fatal,info,create_logfile,close_logfile,set_verbose

  ! Module to provide logging and message printing facilities.
  ! Model:
  ! - Messages may be 'high' or 'low' priority.
  ! - High priority messages are always printed to stdout
  ! - Low priority messages are only printed to stdout if verbose==.true.
  ! - Call set_verbose(.true.) / set_verbose(.false.) to control verbosity state
  ! - If a log file is created, all subsequent messages (high and low priority) are written to that file
  ! - Messages in the log file may be preceded by a timestamp if desired
  ! - 'Fatal' messages are always written to stderr, and to the log file if it exists.
  ! - Issuing a 'fatal' message leads to immediate termination of the program
  !
  ! Public functions:
  ! - create_logfile(lu,file,header)
  !    - lu                - integer          - 'unit number' for file
  !    - file              - character(len=*) - name of file to create
  !    - header (optional) - character(len=*) - Message to print at beginning of file (Default: "")
  !   Opens log file, writes initial timestamp, writes header if provided, and writes additional information
  !   (username, machine name, current working directory, program invocation arguments, etc.). All subsequent
  !   calls to info() or fatal() will result in log file entries.
  !
  ! - close_logfile()
  !   Terminates logging to file; closes log file.
  !
  ! - set_verbose(switch)
  !    - switch            - logical          - '.true.' to enable verbose mode; '.false.' to disable verbose mode.
  !   Governs whether calls to info() with status INFO_LO are printed to stdout. If switch==.true., such messages
  !   will be displayed. Note that all messages are written to the log file (if open), regardless of verbosity setting.
  !
  ! - info(module,subroutine,message,level,stamp,ierr)
  !    - module            - character(len=*) - Identifier for program unit generating output (e.g. module name;
  !                                             may be empty string)
  !    - subroutine        - character(len=*) - Identifier for function or subroutine generating output
  !    - message           - character(len=*) - String to be displayed and/or logged
  !    - level             - integer          - Importance of message: either INFO_HI or INFO_LO. Messages with
  !                                             status INFO_LO are not written to stdout unless verbose mode has been enabled.
  !    - stamp (optional)  - logical          - Set to .true. to insert timestamp in log file prior to message.
  !                                             (Default: no timestamp)
  !    - ierr (optional)   - integer          - Error/information code to accompany message. (Default: no code displayed.)
  !   Write message to log file (if open); additionally, write message to stdout (as controlled by 'level' and
  !   verbosity settings).
  !
  ! - fatal(module,subroutine,message,ierr)
  !    - module            - character(len=*) - Identifier for program unit generating output (e.g. module name;
  !                                             may be empty string)
  !    - subroutine        - character(len=*) - Identifier for function or subroutine generating output
  !    - message           - character(len=*) - String to be displayed and/or logged
  !    - ierr (optional)   - integer          - Error/information code to accompany message. (Default: no code displayed.)
  !   Announce fatal error and terminate program. If logfile is open, a timestamp is written and the error message; the
  !   file is then closed. Error message is written to stderr. Program terminates with non-zero exit code ('stop 1').
  !



  integer,parameter,public::INFO_HI=1,INFO_LO=2
  integer,parameter::LUERR=error_unit
  integer::LUOUT=output_unit
  integer::LULOG=0
  logical::verbose=.false.
  real::cput0=0,cput1=0,cput2
contains
  subroutine set_verbose(sw)
    logical,intent(in)::sw
    verbose=sw
  end subroutine set_verbose

  subroutine timestamp()
    character(len=8)::date
    character(len=10)::time
    ! Write the following to file:
    ! - current date and time
    ! - total cpu time elapsed (strictly, total cpu time since first call to timestamp())
    ! - cpu time elapsed since previous call to timestamp()
    ! Note that cpu time is obtained via calls to intrinsic function cpu_time() and will
    ! therefore depend upon the accuracy (and any limitations, e.g. thread awareness) of
    ! that function.
    call date_and_time(date,time)
    call cpu_time(cput2)
    write(LULOG,"(80('='))")
    if(cput1.eq.0) then
      write(LULOG,"('Timestamp: ',a4,'-',a2,'-',a2,'  ',a2,':',a2,':',a6)") &
      date(1:4),date(5:6),date(7:8),time(1:2),time(3:4),time(5:10)
      cput0=cput2
      cput1=cput2
    else
      write(LULOG,"('Timestamp: ',a4,'-',a2,'-',a2,'  ',a2,':',a2,':',a6,'  (CPU total=',f0.3,'s; CPU tick=',f0.3's)')") &
      date(1:4),date(5:6),date(7:8),time(1:2),time(3:4),time(5:10),cput2-cput0,cput2-cput1
      cput1=cput2
    end if
    write(LULOG,("(80('-'))"))
  end subroutine timestamp

  subroutine create_logfile(lu,file,header)
    integer,intent(in)::lu
    character(len=*),intent(in)::file
    character(len=*),intent(in),optional::header
    character(len=240)::line
    integer::lline
    character(len=20)::fmt
    open(lu,file=file)
    LULOG=lu
    if(present(header)) write(LULOG,*) header
    call timestamp()
    call get_command(line,lline)
    if(lline.eq.len(trim(line))) then
      write(fmt,"('(a',i03,')')") 11+lline
      write(LULOG,fmt) "Executing: "//trim(line)
    else
      write(fmt,"('(a',i03,')')") 26+len(trim(line))
      write(LULOG,*) "Executing: "//trim(line)//'... [TRUNCATED]'
    end if
    line=compiler_version()
    write(fmt,"('(a',i03,')')") 15+len(trim(line))
    write(LULOG,fmt) "Compiled with: "//trim(line)
    line=compiler_options()
    write(fmt,"('(a',i03,')')") 19+len(trim(line))
    write(LULOG,fmt) "Compilation flags: "//trim(line)
    call get_environment_variable('HOST',line,lline)
    write(fmt,"('(a',i03,')')") 10+lline
    write(LULOG,fmt) "Hostname: "//trim(line)
    call get_environment_variable('USER',line,lline)
    write(fmt,"('(a',i03,')')") 10+lline
    write(LULOG,fmt) "Username: "//trim(line)
    call get_environment_variable('PWD',line,lline)
    if(lline.eq.len(trim(line))) then
      write(fmt,"('(a',i03,')')") 19+lline
      write(LULOG,fmt) "Working directory: "//trim(line)
    else
      write(fmt,"('(a',i03,')')") 34+lline
      write(LULOG,fmt) "Working directory: "//trim(line)//"... [TRUNCATED]"
    end if
    write(LULOG,("(80('-'))"))
    call flush(lu)
  end subroutine create_logfile

  subroutine close_logfile()
    write(LULOG,"(80('='))")
    close(LULOG)
    LULOG=0
  end subroutine close_logfile

  subroutine fatal(mod,func,message,errnum)
    character(len=*),intent(in)::mod,func,message
    integer,intent(in),optional::errnum
    character(len=8)::fmt

    write(LUERR,*)
    if(LULOG.ne.0) call timestamp()
    write(fmt,"('(a',i3.3,')')") len(trim(mod))+len(trim(func))+26
    write(LUERR,fmt) "*** FATAL ERROR in "//trim(mod)//':'//trim(func)//"() ***"
    if(LULOG.ne.0) write(LULOG,fmt) "*** FATAL ERROR in "//trim(mod)//':'//trim(func)//"() ***"
    write(fmt,"('(a',i3.3,')')") len(trim(message))
    write(LUERR,fmt) trim(message)
    if(LULOG.ne.0) write(LULOG,fmt) trim(message)
    if(present(errnum)) then
      write(LUERR,"('[Error code: ',i0,']')") errnum
      if(LULOG.ne.0) write(LULOG,"('[Error code: ',i0,']')") errnum
    end if
    if(LULOG.ne.0) call close_logfile()
    stop 1
  end subroutine fatal

  subroutine info(mod,func,message,level,stamp,errnum)
    character(len=*),intent(in)::mod,func,message
    integer,intent(in)::level
    logical,optional,intent(in)::stamp
    integer,optional,intent(in)::errnum
    character(len=12)::fmt

    if(present(stamp)) then
      if(stamp) call timestamp()
    end if
    if((level.eq.INFO_HI).or.verbose) then
      write(fmt,"('(a',i0,')')") len(trim(message))
      write(LUOUT,fmt) trim(message)
    end if
    if(LULOG.ne.0) then
      if(present(errnum)) then
        write(fmt,"('(a',i0,',i0,a1)')") len(trim(message))+len(trim(func))+len(trim(mod))+12
        write(LULOG,fmt) trim(message)//' ['//trim(mod)//':'//trim(func)//'() - err ',errnum,']'
      else
        write(fmt,"('(a',i0,')')") len(trim(message))+len(trim(mod))+len(trim(func))+6
        write(LULOG,*) trim(message)//' ['//trim(mod)//':'//trim(func)//'()]'
      end if
      call flush(LULOG) ! Important that log file is up-to-date
    end if
  end subroutine info
end module mod_log
