program demo_tools
  use mod_log
  use mod_types
  use mod_foptparse
  use mod_ndk
  implicit none

  ! Define constants to reference each option
  integer, parameter :: OPT_LOGFILE=1
  integer, parameter :: OPT_VERBOSE=OPT_LOGFILE+1
  integer, parameter :: OPT_NDKFILE=OPT_VERBOSE+1
  integer, parameter :: OPT_HELP   =OPT_NDKFILE+1
  integer, parameter :: NOPTIONS=OPT_HELP
  type(fopt_opt),dimension(NOPTIONS)::option

  integer::nargs,iarg,status
  character(len=OPT_LEN_CHARVAL),dimension(:),allocatable::args

  type(ndkrec)::record

  logical::logging

  ! Set up option parser
  call fopt_setup_option(option(OPT_LOGFILE),'log-file',ARG_CHAR,'l',charval='out.log', &
                            helptext='Write log to file FILE',metavar='FILE')
  call fopt_setup_option(option(OPT_VERBOSE),'verbose',ARG_NONE,'v',switchval=.false., &
                            helptext='Enable verbose output')
  call fopt_setup_option(option(OPT_NDKFILE),'ndk-file',ARG_CHAR,charval='',helptext="Path to ndk file")
  call fopt_setup_option(option(OPT_HELP),'help',ARG_HELP,'h',helptext="Display this help")
  ! And parse...
  call fopt_parse(NOPTIONS,option,args,nargs,6,"Usage: demo_tools [options] CMT1 CMT2 ...","")

  if(option(OPT_LOGFILE)%encountered) then
    call create_logfile(7,option(OPT_LOGFILE)%charval)
    logging=.true.
  end if

  if(option(OPT_VERBOSE)%encountered) then
    call set_verbose(.true.)
  else
    call set_verbose(.false.) ! Strictly unnecessary
  end if

  call ndkopen(8,option(OPT_NDKFILE)%charval)

  if(nargs.eq.0) then
    call fatal("","main","No events given!")
  else
    do iarg=1,nargs
      call ndkget(8,trim(args(iarg)),record,status)
      if(status.eq.NDK_SUCCESS) then
        print *, trim(args(iarg))," Lat:",record%evloc%lat,"Lon:",record%evloc%lon,"Dep:",record%evloc%z
      end if
    end do
  end if
  call ndkclose(8)
  if(logging) call close_logfile()

end program
