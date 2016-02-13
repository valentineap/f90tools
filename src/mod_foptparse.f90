module mod_foptparse
  ! Andrew Valentine
  ! Universiteit Utrecht
  ! a.p.valentine@uu.nl
  ! 2014

  ! Module to provide framework for parsing command-line arguments in Fortran.
  ! Somewhat inspired by python's optparse module.

  use mod_log
  implicit none
  private
  public::fopt_setup_option, fopt_parse,fopt_write_help

  ! Basic usage:
  !
  ! program demo_foptparse
  !    integer, parameter::nopts=3,output_unit=6
  !    type(fopt_opt),dimension(nopts)::options
  !    character(len=OPT_LEN_CHARVAL),dimension(:),allocatable::args
  !    integer::nargs,iarg
  !    integer,parameter::OPTION_A=1,OPTION_B=2,OPTION_HELP=3
  !    call fopt_setup_option(options(OPTION_A),'option-a',ARG_INT,shortform='a',intval=42,metavar='A', &
  !                                                               helptext='An option to control parameter A')
  !    call fopt_setup_option(options(OPTION_B),'option-b',ARG_NONE,shortform='b',metavar='B', &
  !                                                               helptext='An option to control parameter B')
  !    call fopt_setup_option(options(OPTION_HELP),'help',ARG_HELP,helptext='Print this help information')
  !    call fopt_parse(nopts,options,args,nargs,output_unit,'Usage: <program> [options] [arg1 [arg2 ...]]', &
  !                                           'A simple program to demonstrate how mod_foptparse may be used')
  !    print *, "Received ",nargs," arguments."
  !    do iarg=1,nargs
  !       print *, trim(args(iarg))
  !    end do
  !    if(options(OPTION_A)%encountered) then
  !       print *, "Option A:",options(OPTION_A)%intval
  !    else
  !       print *, "Option A:",options(OPTION_A)%intval, "[Default value]"
  !    end if
  !    if(options(OPTION_B)%encountered) then
  !       print *, "Option B:",options(OPTION_B)%switchval
  !    else
  !       print *, "Option B:",options(OPTION_B)%switchval, "[Default value]"
  !    end if
  !    deallocate(args)
  ! end program demo_foptparse


  !Possible argtype values
  integer,parameter,public::ARG_NONE=1,ARG_INT=2,ARG_FLOAT=3,ARG_CHAR=4,ARG_HELP=5,ARG_VERSION=6
  !Field lengths for various quantites
  integer,parameter,public::OPT_LEN_LONGFORM=16,OPT_LEN_CHARVAL=80,OPT_LEN_HELPTEXT=160,OPT_LEN_METAVAR=8


  type,public ::fopt_opt                          !Type to encapsulate a single option
    sequence
    character::shortform                          !Single-character flag (e.g. 'a' for '-a')
    character(len=OPT_LEN_LONGFORM)::longform     !Multi-character flag ('help' for '--help')
    integer::argtype                              !One of values defined above
    integer::intval                               !Result if integer
    real::floatval                                !Result if float
    character(len=OPT_LEN_CHARVAL)::charval       !Result if string
    logical::switchval                            !Result if logical
    character(len=OPT_LEN_HELPTEXT)::helptext     !Text to print in help
    character(len=OPT_LEN_METAVAR)::metavar       !String to indicate variable in help (e.g. 'OUTFILE' to give '-o OUTFILE')
    logical::encountered                          !Flag to indicate if switch was provided
  end type fopt_opt

contains
  subroutine fopt_setup_option(option,longform,argtype,shortform,intval,floatval,charval,switchval,helptext,metavar)
    type(fopt_opt),intent(out)::option
    character(len=*),intent(in)::longform
    integer,intent(in)::argtype
    character,intent(in),optional::shortform
    integer,intent(in),optional::intval
    real,intent(in),optional::floatval
    character(len=*),intent(in),optional::charval
    logical,intent(in),optional::switchval
    character(len=*),intent(in),optional::helptext
    character(len=*),intent(in),optional::metavar
    integer::ichar
    option%encountered=.false.

    if(len(longform).gt.OPT_LEN_LONGFORM) then
      stop 'Long form too long!'
    else
      option%longform(1:len(longform))=longform
      do ichar=len(longform)+1,OPT_LEN_LONGFORM
        option%longform(ichar:ichar)=' '
      end do
    end if

    if(present(shortform)) then
      ! Check for stupidity...
      if(shortform.eq.'-') call fatal("mod_foptparse","fopt_setup_option", &
                          "Received unexpected character '-' for short-form of option "//trim(longform))
      option%shortform=shortform

    else
      option%shortform=' '
    end if

    !First initialise everything to zero values
    option%switchval=.false.
    option%intval=0
    option%floatval=0.
    do ichar=1,OPT_LEN_CHARVAL
      option%charval(ichar:ichar)=' '
    end do

    !Now set other default value if provided, and check for stupidity
    select case(argtype)
    case(ARG_NONE)
      option%argtype=ARG_NONE
      if(present(switchval)) option%switchval=switchval
      if(present(intval).or.present(floatval).or.present(charval)) call fatal("mod_foptparse","fopt_setup_option", &
            "Unexpected arguments received when setting up option "//trim(option%longform)//".")
    case(ARG_INT)
      option%argtype=ARG_INT
      if(present(intval)) option%intval=intval
      if(present(switchval).or.present(floatval).or.present(charval)) call fatal("mod_foptparse","fopt_setup_option", &
            "Unexpected arguments received when setting up option "//trim(option%longform)//".")
    case(ARG_FLOAT)
      option%argtype=ARG_FLOAT
      if(present(floatval)) option%floatval=floatval
      if(present(switchval).or.present(intval).or.present(charval)) call fatal("mod_foptparse","fopt_setup_option", &
            "Unexpected arguments received when setting up option "//trim(option%longform)//".")
    case(ARG_CHAR)
      option%argtype=ARG_CHAR
      if(present(charval)) then
        if(len(charval).gt.OPT_LEN_CHARVAL) call fatal("mod_foptparse","fopt_setup_option", &
             "Argument buffer 'charval' too small: Unable to set default value for option "//trim(option%longform)//".")
        option%charval(1:len(charval))=charval
      end if
      if(present(switchval).or.present(intval).or.present(floatval)) call fatal("mod_foptparse","fopt_setup_option", &
              "Unexpected arguments received when setting up option "//trim(option%longform)//".")
    case(ARG_HELP)
      option%argtype=ARG_HELP
      if(present(switchval).or.present(intval).or.present(floatval).or.present(charval)) call fatal("mod_foptparse",&
            "fopt_setup_option","Unexpected arguments received when setting up option "//trim(option%longform)//".")
    case(ARG_VERSION)
      option%argtype=ARG_VERSION
      if(present(switchval).or.present(intval).or.present(floatval).or.present(charval)) call fatal("mod_foptparse", &
            "fopt_setup_option","Unexpected arguments received when setting up option "//trim(option%longform)//".")
    case default
      call fatal("mod_foptparse","fopt_setup_option",&
            "Received unknown value in 'argtype' field for option "//trim(option%longform)//".")
    end select

    !Deal with help text and metavar
    if(present(helptext)) then
      if(len(helptext).gt.OPT_LEN_HELPTEXT) then
        call info('mod_foptparse','fopt_setup_option', &
               "Warning: Help text truncated for option '--"//trim(option%longform)//"'.",INFO_HI)
        option%helptext=helptext(1:OPT_LEN_HELPTEXT)
      else
        option%helptext(1:len(helptext))=helptext
        do ichar=len(helptext)+1,OPT_LEN_HELPTEXT
          option%helptext(ichar:ichar)=' '
        end do
      end if
    else
      do ichar=1,OPT_LEN_HELPTEXT
        option%helptext(ichar:ichar)=' '
      end do
    end if
    if(present(metavar)) then
      if(len(metavar).gt.OPT_LEN_METAVAR) then
        call info('mod_foptparse','fopt_setup_option', &
               "Warning: metavar truncated for option '--"//trim(option%longform)//"'.",INFO_HI)
        option%metavar=metavar(1:OPT_LEN_METAVAR)
      else
        option%metavar(1:len(metavar))=metavar
        do ichar=len(metavar)+1,OPT_LEN_METAVAR
          option%metavar(ichar:ichar)=' '
        end do
      end if
    else
      option%metavar(1:5)='value'
      do ichar=6,OPT_LEN_METAVAR
        option%metavar(ichar:ichar)=' '
      end do
    end if
  end subroutine fopt_setup_option

  subroutine fopt_check_options(nopt,options)
    integer,intent(in)::nopt
    type(fopt_opt),dimension(nopt),intent(in)::options

    integer::iopt,jopt
    logical::stopping
    stopping=.false.
    if(nopt.le.1) return ! No point in checking
    do iopt=1,nopt-1
      do jopt=iopt+1,nopt
        if(options(iopt)%shortform.ne.' '.and.options(iopt)%shortform.eq.options(jopt)%shortform) then
          call info('mod_foptparse','fopt_check_options', &
                  "Warning: Duplicate short form option '-"//trim(options(iopt)%shortform)//"'.",INFO_HI)
          stopping=.true.
        end if
        if(trim(options(iopt)%longform).eq.trim(options(jopt)%longform)) then
          call info('mod_foptparse','fopt_check_options',&
                  "Warning: Duplicate long form option '--"//trim(options(iopt)%longform)//"'.",INFO_HI)
          stopping=.true.
        end if
      end do
    end do
    if(stopping) call fatal('mod_foptparse','fopt_check_options',"Unable to set up option parser")
  end subroutine fopt_check_options

  subroutine fopt_write_help(unit,header,footer,nopt,options,colwidth)
    integer,intent(in)::unit
    character(len=*),intent(in)::header,footer
    integer,intent(in)::nopt
    type(fopt_opt),dimension(nopt),intent(in)::options
    integer,intent(in),optional::colwidth
    integer::ccolwidth,iopt,ichar
    character(len=2)::cshort
    character(len=OPT_LEN_METAVAR+2)::cmeta
    character(len=5+OPT_LEN_LONGFORM+OPT_LEN_METAVAR)::clong
    character(len=30)::fmt
    character(len=14)::valfmt
    character(len=17+(2*OPT_LEN_METAVAR)+OPT_LEN_LONGFORM+OPT_LEN_HELPTEXT)::optionline


    ccolwidth=100
    if(present(colwidth)) ccolwidth=colwidth

    write(fmt,"('(2x,a2,1x,a',i2.2,',2x,a',i3.3,',3x,a',i3.3,')')") OPT_LEN_METAVAR+2, 5+OPT_LEN_LONGFORM+OPT_LEN_METAVAR, &
         OPT_LEN_HELPTEXT
    call write_wrapped_string(unit,header,ccolwidth)
    write(unit,*)
    write(unit,"('Options:')")
    do iopt=1,nopt
      !Prepend hyphens to options
      if(options(iopt)%shortform.eq.' ') then
        cshort='  '
      else
        cshort='-'//options(iopt)%shortform
      end if

      if(opt_takes_value(options(iopt)%argtype)) then
        if(options(iopt)%shortform.eq.' ') then
          cmeta=''
        else
          cmeta='<'//trim(options(iopt)%metavar)//'>'
          do ichar=len(trim(options(iopt)%metavar))+3,OPT_LEN_METAVAR+2
            cmeta(ichar:ichar)=' '
          end do
        end if
        clong='--'//trim(options(iopt)%longform)//'=<'//trim(options(iopt)%metavar)//'>'
        do ichar=len(trim(options(iopt)%longform))+len(trim(options(iopt)%metavar))+6,OPT_LEN_LONGFORM+OPT_LEN_METAVAR+5
          clong(ichar:ichar)=' '
        end do
      else
        do ichar=1,OPT_LEN_METAVAR+2
          cmeta(ichar:ichar)=' '
        end do
        clong='--'//trim(options(iopt)%longform)
        do ichar=len(trim(options(iopt)%longform))+3,OPT_LEN_LONGFORM+OPT_LEN_METAVAR+5
          clong(ichar:ichar)=' '
        end do
      end if
      write(optionline,fmt) cshort,cmeta,clong,options(iopt)%helptext
      call write_wrapped_string(unit,trim(optionline),ccolwidth,17+(2*OPT_LEN_METAVAR)+OPT_LEN_LONGFORM)
      ! Write out current value of option
      if(options(iopt)%argtype.eq.ARG_INT) then
        write(valfmt,"('(',i2,'x,a,i0,a)')") 17+(2*OPT_LEN_METAVAR)+OPT_LEN_LONGFORM
        write(unit,valfmt) '[',options(iopt)%intval,']'
      else if(options(iopt)%argtype.eq.ARG_FLOAT) then
        write(valfmt,"('(',i2,'x,a,f0.0,a)')") 17+(2*OPT_LEN_METAVAR)+OPT_LEN_LONGFORM
        write(unit,valfmt) '[',options(iopt)%floatval,']'
      else if(options(iopt)%argtype.eq.ARG_CHAR.and.len(trim(options(iopt)%charval)).gt.0) then
        write(valfmt,"('(',i2,'x,a,',i3,'a,a)')") 17+(2*OPT_LEN_METAVAR)+OPT_LEN_LONGFORM,len(trim(options(iopt)%charval))
        write(unit,valfmt) '[',trim(options(iopt)%charval),']'
      end if
    end do
    write(unit,*)
    if(len(trim(footer)).gt.0) then
      call write_wrapped_string(unit,footer,ccolwidth)
      write(unit,*)
    end if
  end subroutine fopt_write_help

  subroutine fopt_parse(nopt,options,args,nargs,helpunit,helpheader,helpfooter,helpwidth)
    integer,intent(in)::nopt
    type(fopt_opt),dimension(nopt),intent(inout)::options
    character(len=OPT_LEN_CHARVAL),dimension(:),allocatable,intent(out)::args
    integer,intent(out)::nargs
    integer,intent(in)::helpunit
    character(len=*),intent(in)::helpheader,helpfooter
    integer,intent(in),optional::helpwidth
    integer::ntot,iarg,jarg,iopt,ichar,lcmd
    character(len=3+OPT_LEN_LONGFORM+OPT_LEN_CHARVAL)::workcmd
    character(len=OPT_LEN_CHARVAL)::workcmdval
    logical::shortform

    !Perform sanity check on option definitions
    call fopt_check_options(nopt,options)

    nargs=0 !MUST be zeroed before any early return!
    ntot=command_argument_count()
    if(ntot.eq.0) return !No need to do anything BUT NARGS MUST ALREADY HAVE BEEN ZEROED!!!

    ! First pass deals with flags and counts non-flag arguments (to enable allocation of args)
    ! Second pass populates args
    iarg=1
    optloop: do
      if(iarg.gt.ntot) exit optloop
      call get_command_argument(iarg,workcmd,lcmd)
      if(lcmd.gt.len(workcmd)) call fatal('mod_foptparse','fopt_parse', &
            "Input buffer 'workcmd' too small: Unable to read "//workcmd//"... [TRUNCATED].")
      if(workcmd(1:1).eq.'-') then !Is a flag
        if(workcmd(2:2).ne.'-') then ! Is short form
          if(workcmd(3:3).ne.' ') call fatal('mod_foptparse','fopt_parse', &
                  "Unable to parse option "//trim(workcmd)//". Long-form options must be preceded by '--'.")
          shortform=.true.

          ! Look through options list for match
          iopt=1
          findsopt:do
            if(iopt.gt.nopt) call fatal('mod_foptparse','fopt_parse',"Unrecognised option: "//trim(workcmd))
            if(workcmd(2:2).eq.options(iopt)%shortform) exit findsopt
            iopt=iopt+1
          end do findsopt

          if(opt_takes_value(options(iopt)%argtype)) then ! Consume next command line 'word'
            if(iarg.eq.ntot) call fatal('mod_foptparse','fopt_parse',"Value expected to follow option "//trim(workcmd)//'.')
            call get_command_argument(iarg+1,workcmdval,lcmd)
            if(lcmd.gt.len(workcmdval)) call fatal('mod_foptparse','fopt_parse', &
                     "Input buffer 'workcmdval' too small: unable to read "//workcmdval//"... [TRUNCATED].")
            if(workcmdval(1:1).eq.'-') call info('mod_foptparse','fopt_parse', &
                     "Warning: Treating '"//trim(workcmdval)//"' as argument to option "//trim(workcmd)//".",INFO_HI)
            iarg=iarg+2
          else
            iarg=iarg+1
          end if
          ichar=3 ! No '=' in short-form argument; assignment needed for later logic
        else
          shortform=.false.
          ! Need to parse option of form --opt=val.
          ichar=3
          findeq:do !Find index of '=' in workcmd
            if((workcmd(ichar:ichar).eq.'=').or.ichar.gt.OPT_LEN_LONGFORM+2) exit findeq
            ichar=ichar+1
          end do findeq
          workcmdval=workcmd(ichar+1:) !workcmdval contains substring of workcmd after '='
          iarg=iarg+1
          iopt=1
          findopt:do ! Look through list of options for match
            if(iopt.gt.nopt) call fatal('mod_foptparse','fopt_parse',"Unrecognised option: "//trim(workcmd))
            if(workcmd(3:ichar-1).eq.trim(options(iopt)%longform)) exit findopt
            iopt=iopt+1
          end do findopt
        end if

        !Set 'encountered' flag (and check that this option hasn't already been seen)
        if(options(iopt)%encountered) call fatal('mod_foptparse','fopt_parse',"Encountered more than one instance of option '" &
             //trim(options(iopt)%longform)//"'. This is not currently supported by mod_foptparse.")
        options(iopt)%encountered=.true.


        select case(options(iopt)%argtype)
        case(ARG_HELP)
          call fopt_write_help(helpunit,helpheader,helpfooter,nopt,options,helpwidth)
          stop
        case(ARG_VERSION)
          call fatal('mod_foptparse','fopt_parse',"Support for ARG_VERSION argument type not yet implemented")
        case(ARG_INT)
          if(len(trim(workcmdval)).eq.0) call fatal('mod_foptparse','fopt_parse', &
                  "Error parsing option '"//trim(workcmd)//"': Integer argument expected.")
          read(workcmdval,"(i10)",err=101) options(iopt)%intval
          cycle optloop
101       if(len(trim(workcmdval)).eq.0) call fatal('mod_foptparse','fopt_parse', &
                  "Error parsing option '"//trim(workcmd)//"': Integer argument expected.")
        case(ARG_FLOAT)
          if(len(trim(workcmdval)).eq.0) call fatal('mod_foptparse','fopt_parse', &
                  "Error parsing option '"//trim(workcmd)//"': Float argument expected.")
          read(workcmdval,*,err=102) options(iopt)%floatval
          cycle optloop
102       call fatal('mod_foptparse','fopt_parse', &
                  "Error parsing option '"//trim(workcmd)//"': Float argument expected.")
        case(ARG_CHAR)
          if((.not.shortform).and.(workcmd(ichar:ichar).ne.'=')) call fatal('mod_foptparse','fopt_parse', &
                  "Error parsing option '"//trim(workcmd)//"': String argument expected.")
          !But don't actually require string to have content, to provide a way to pass null string as arg
          options(iopt)%charval=trim(workcmdval)
          cycle optloop
        case(ARG_NONE)
          options(iopt)%switchval=.not.options(iopt)%switchval
          if(workcmd(ichar:ichar).eq.'=') call fatal('mod_foptparse','fopt_parse',&
                  "Unexpected argument received for option '"//trim(workcmd)//"'.")
          cycle optloop
        case default
          call fatal("mod_foptparse","fopt_parse","Program logic error encountered!")
        end select
      else !Should go in args
        nargs=nargs+1
        iarg=iarg+1
      end if
    end do optloop

    allocate(args(nargs))
    iarg=1
    jarg=1
    ! Need to scan through all arguments again to identify options and not-options
    argloop: do
      if(iarg.gt.ntot) exit argloop
      call get_command_argument(iarg,workcmd,lcmd)
      if(workcmd(1:1).eq.'-') then !Is a flag
        if(workcmd(2:2).ne.'-') then ! Is short form
          iopt=1
          findsopt2:do
            if(workcmd(2:2).eq.options(iopt)%shortform) exit findsopt2
            iopt=iopt+1
          end do findsopt2
          if(opt_takes_value(options(iopt)%argtype)) then
            iarg=iarg+2
          else
            iarg=iarg+1
          end if
        else
          iarg=iarg+1
        end if
      else
        if(jarg.gt.nargs) call fatal("mod_foptparse","fopt_parse","Program logic error encountered!")
        call get_command_argument(iarg,args(jarg),lcmd)
        if(lcmd.gt.len(args(jarg))) call fatal("mod_foptparse","fopt_parse", &
                  "Input buffer 'args(:)' too small: Unable to read "//args(jarg)//"... [TRUNCATED].")
        jarg=jarg+1
        iarg=iarg+1
      end if
    end do argloop
  end subroutine fopt_parse

  function wrap_string_index(string,istart,width) result(w)
    ! Function to aid wrapping of long strings for display purposes
    ! Finds last space (' ') in string at or before position 'width' of substring string(istart:)
    ! Returns index of space, or 0 if string is shorter than width.
    character(len=*),intent(in)::string
    integer,intent(in)::istart
    integer,intent(in)::width
    integer::w
    integer::lstring,i
    lstring=len(string)
    if(lstring.le.istart+width-1) then
      w=0
    else
      i=istart+width !Because we can accept the final character of a word being at the extremity of the line
      do while(string(i:i).ne.' ')
        i=i-1
        if(i.eq.0) exit
      end do
      if(i.eq.0) then
        w=istart+width-1 ! If no spaces, truncate string mid-stream
      else
        w=i
      end if
    end if
    return
  end function wrap_string_index

  subroutine write_wrapped_string(unit,string,width,indent)
    integer,intent(in)::unit
    character(len=*),intent(in)::string
    integer,intent(in)::width
    integer,intent(in),optional::indent
    integer::iwrap0,iwrap1,wwidth
    character(len=11)::fmt
    logical::firstline !We don't indent first line

    wwidth=width
    firstline=.true.
    write(fmt,"('(',i3.3,'a)')") wwidth


    iwrap0=1
    do
      iwrap1=wrap_string_index(string,iwrap0,wwidth)
      if(iwrap1.gt.0) then
        write(unit,fmt) string(iwrap0:iwrap1)
        iwrap0=iwrap1+1
      else
        write(unit,fmt) string(iwrap0:)
        exit
      end if
      if(firstline) then
        if(present(indent)) then
          write(fmt,"('(',i3.3,'x,',i3.3,'a)')") indent,wwidth
          wwidth=width-indent
        end if
        firstline=.false.
      end if
    end do
  end subroutine write_wrapped_string

  function opt_takes_value(type) result (l)
    integer,intent(in)::type
    logical::l
    l=.false.
    if((type.eq.ARG_INT).or.(type.eq.ARG_FLOAT).or.(type.eq.ARG_CHAR)) l=.true.
    return
  end function opt_takes_value


end module mod_foptparse
