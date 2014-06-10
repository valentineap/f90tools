module mod_ndk
  ! Andrew Valentine
  ! Universiteit Utrecht
  ! a.p.valentine@uu.nl
  ! 2008-2014

  use mod_log
  use mod_types
  implicit none

  private
  public::ndkeig,ndknp,ndkrec,NDK_SUCCESS,NDK_EOF,NDK_NOTFOUND,ndkopen,ndkclose,ndkget

  ! Module to read data from .ndk source catalogue format. Modified from module ndkutils;
  ! not backwards compatible.
  !
  ! Public functions:
  ! - ndkopen(lu,filename)
  !    - lu                  - integer          - Unit number to be used for file
  !    - filename (optional) - character(len=*) - Path to .ndk catalogue to be used. If no filename is given, or if
  !                                               filename is an empty string, ndkopen will look for an environment
  !                                               variable 'NDKFILE' and attempt to open any file specified there.
  !   Opens specified .ndk file (or file set by environment variable) for reading.
  !
  ! - ndkget(lu,cmtname,record,status)
  !    - lu                  - integer          - Unit number of open ndk file
  !    - cmtname             - character(len=*) - CMT code for event (7- or 13-character string)
  !    - record              - type(ndkrec)     - Object encapsulating all event information. Note that it is
  !                                               important to check the status flag before using the record: if
  !                                               the event code is not found, contents of 'record' are undefined.
  !    - status              - integer          - Flag indicating success (status=NDK_SUCCESS) or failure
  !                                               (status=NDK_NOTFOUND) of search.
  !   Look up details of event specified by CMT identifier code. Note that the approach used here is *not* efficient:
  !   the file is read from the beginning for each call to ndkget(). This could be improved by (e.g.) constructing
  !   an index during ndkopen(). However, for most realistic applications this is unlikely to have significant
  !   impact upon overall program performance.
  !
  ! - ndkclose(lu):
  !    - lu                  - integer          - Unit number of open ndk file
  !   Closes specified .ndk file.

  integer,parameter::NDK_SUCCESS=0,NDK_EOF=-1,NDK_NOTFOUND=-2


  type ndkeig
    real(k_rs) :: eigenvalue
    real(k_rs) :: plunge
    real(k_rs) :: azimuth
  end type ndkeig

  type ndknp
    real(k_rs) :: strike
    real(k_rs) :: dip
    real(k_rs) :: rake
  end type ndknp

  type ndkrec
    character(len=4)          :: hypocat
    type(date)                :: evdate
    type(location)            :: evloc
    real(k_rs)                :: mag1
    real(k_rs)                :: mag2
    character(len=24)         :: locname
    character(len=16)         :: evname
    character(len=44)         :: dataused
    character(len=6)          :: cmttype
    character(len=10)         :: momrate
    type(location)            :: cent
    real(k_rs)                :: cent_tshift
    type(location)            :: err_cent
    real(k_rs)                :: err_cent_tshift
    character(len=4)          :: deptype
    character(len=16)         :: timestamp
    type(moment_tensor)       :: mt
    type(moment_tensor)       :: err_mt
    character(len=3)          :: version
    type(ndkeig),dimension(3) :: eigs
    real(k_rs)                :: scalar_moment
    type(ndknp),dimension(2)  :: nodal_plane
  end type ndkrec
  integer:: iline
contains
  subroutine ndkopen(lundk,ndkfile)
    integer,intent(in)::lundk
    character(len=*),intent(in),optional::ndkfile
    integer::ifail,lndk
    character(len=160)::nndkfile,emsg
    lndk=0
    if (present(ndkfile)) then
      nndkfile=ndkfile
      lndk=len(trim(nndkfile))
      if(len(trim(nndkfile)).ne.len(trim(ndkfile))) call fatal('mod_ndk','ndkopen', &
            "Internal buffer 'nndkfile' too small: Unable to handle filename "//trim(ndkfile))
    end if
    if(lndk.eq.0) then ! Either we didn't get argument ndkfile, OR we got an empty string - look to environment variable
      call get_environment_variable('NDKFILE',nndkfile,lndk)
      if(lndk.eq.0) call fatal('mod_ndk','ndkopen', &
            "Unable to locate ndk file. Set environment variable 'NDKFILE', or provide filename as second argument to ndkopen()")
      if(len(trim(nndkfile)).ne.lndk) call fatal('mod_nkd','ndkopen', &
            "Internal buffer 'nndkfile' too small: Unable to handle filename "//trim(ndkfile))
    end if
    open(lundk,file=trim(nndkfile),status='old',iostat=ifail,iomsg=emsg)
    iline=1 ! Line number that will be read next.
    if(ifail.eq.0) then
      call info("mod_ndk","ndkopen","Successfully opened ndk file at: '"//trim(nndkfile)//"'.",INFO_LO)
    else
      call fatal("mod_ndk","ndkopen","Failed to open ndk file at: '"//trim(nndkfile)//"'. "//trim(emsg),ifail)
    end if
  end subroutine ndkopen

  subroutine ndkclose(lundk)
    integer,intent(in)::lundk
    integer::ifail
    character(len=160)::emsg
    close(lundk,iostat=ifail,iomsg=emsg)
    if(ifail.eq.0) then
      call info("mod_ndk","ndkclose","Successfully closed ndk file.",INFO_LO)
    else
      call fatal("mod_ndk","ndkclose","Failed to close ndk file. "//trim(emsg),ifail)
    end if
  end subroutine ndkclose

  subroutine ndkreset(lundk)
    integer,intent(in)::lundk

    rewind(lundk)
    iline=1
  end subroutine

  subroutine ndkget(lundk,cmtname,record,return_status)
    integer,intent(in)::lundk
    character(len=*) ::cmtname
    type(ndkrec)::record,search
    integer,intent(out)::return_status
    integer::istatus

    call ndkreset(lundk)
    find:do
      call ndkreadnext(lundk,search,istatus)
      if(istatus.eq.NDK_EOF) then
        call info("mod_ndk","ndkget","Unable to find event "//trim(cmtname)//" in catalogue.",INFO_HI)
        return_status=NDK_NOTFOUND
        exit find
      else
        if(search%evname(2:len(trim(cmtname))+1).eq.trim(cmtname)) then
          record=search
          call info("mod_ndk","ndkget","Successfully read information about event "//trim(cmtname)//" from catalogue.",INFO_LO)
          return_status=NDK_SUCCESS
          exit find
        end if
        ! Otherwise go on and read the next record...
      end if
    end do find
  end subroutine ndkget

  subroutine ndkreadnext(lundk,rec,return_status)
    integer,intent(in)::lundk
    type(ndkrec),intent(out)::rec
    integer,intent(out)::return_status
    character(len=80) line
    integer :: istatus
    integer,dimension(12)::itmp
    character(len=160)::emsg,smsg

    read(lundk,"(a80)",iostat=istatus,iomsg=emsg) line
    if(istatus.eq.0) then
      read(line,"(a4,1x,i4,1x,4(i2,1x),f4.1,1x,f6.2,1x,f7.2,1x,f5.1,1x,2(f3.1,1x),a24)",iostat=istatus,iomsg=emsg) &
                                        rec%hypocat &
                                        ,rec%evdate%year,rec%evdate%month,rec%evdate%day &
                                        ,rec%evdate%hour,rec%evdate%minute,rec%evdate%second &
                                        ,rec%evloc%lat,rec%evloc%lon,rec%evloc%z &
                                        ,rec%mag1,rec%mag2,rec%locname
      if(istatus.ne.0) then
        write(smsg,"('Unable to parse line',1x,i0,1x,'of ndk file.')") iline
        call fatal("mod_ndk","ndkreadnext",trim(smsg)//" "//trim(emsg),istatus)
      end if
      iline=iline+1
      read(lundk,"(a80)",iostat=istatus,iomsg=emsg) line
      if(istatus.ne.0) then
        if(is_iostat_end(istatus)) then
          call fatal("mod_ndk","ndkreadnext","Unexpected mid-record EOF in ndkfile. "//trim(emsg),istatus)
        else
          write(smsg,"('Mid-record error when reading from ndk file, line',1x,i0,'.')") iline
          call fatal("mod_ndk","ndkreadnext",trim(smsg)//' '//trim(emsg),istatus)
        end if
      end if
      read(line,"(a16,1x,a44,1x,a6,1x,a10)",iostat=istatus,iomsg=emsg) rec%evname,rec%dataused,rec%cmttype,rec%momrate
      if(istatus.ne.0) then
        write(smsg,"('Unable to parse line',1x,i0,1x,'of ndk file.')") iline
        call fatal("mod_ndk","ndkreadnext",trim(smsg)//" "//trim(emsg),istatus)
      end if
      iline=iline+1
      read(lundk,"(a80)",iostat=istatus,iomsg=emsg) line
      if(istatus.ne.0) then
        if(is_iostat_end(istatus)) then
          call fatal("mod_ndk","ndkreadnext","Unexpected mid-record EOF in ndkfile. "//trim(emsg),istatus)
        else
          write(smsg,"('Mid-record error when reading from ndk file, line',1x,i0,'.')") iline
          call fatal("mod_ndk","ndkreadnext",trim(smsg)//' '//trim(emsg),istatus)
        end if
      end if
      read(line,"(12x,f6.1,1x,f3.1,1x,f6.2,1x,f4.2,1x,f7.2,1x,f4.2,1x,f5.1,1x,f4.1,1x,a4,1x,a16)",iostat=istatus,iomsg=emsg) &
                                        rec%cent_tshift,rec%err_cent_tshift &
                                        ,rec%cent%lat,rec%err_cent%lat &
                                        ,rec%cent%lon,rec%err_cent%lon &
                                        ,rec%cent%z,rec%err_cent%z &
                                        ,rec%deptype,rec%timestamp
      if(istatus.ne.0) then
        write(smsg,"('Unable to parse line',1x,i0,1x,'of ndk file.')") iline
        call fatal("mod_ndk","ndkreadnext",trim(smsg)//" "//trim(emsg),istatus)
      end if
      iline=iline+1
      read(lundk,"(a80)",iostat=istatus,iomsg=emsg) line
      if(istatus.ne.0) then
        if(is_iostat_end(istatus)) then
          call fatal("mod_ndk","ndkreadnext","Unexpected mid-record EOF in ndkfile. "//trim(emsg),istatus)
        else
          write(smsg,"('Mid-record error when reading from ndk file, line',1x,i0,'.')") iline
          call fatal("mod_ndk","ndkreadnext",trim(smsg)//' '//trim(emsg),istatus)
        end if
      end if
      read(line,"(i2,1x,6(f6.3,1x,f5.3,1x))",iostat=istatus,iomsg=emsg) rec%mt%exponent &
                                        ,rec%mt%mrr, rec%err_mt%mrr &
                                        ,rec%mt%mtt, rec%err_mt%mtt &
                                        ,rec%mt%mpp, rec%err_mt%mpp &
                                        ,rec%mt%mrt, rec%err_mt%mrt &
                                        ,rec%mt%mrp, rec%err_mt%mrp &
                                        ,rec%mt%mtp, rec%err_mt%mtp
      if(istatus.ne.0) then
        write(smsg,"('Unable to parse line',1x,i0,1x,'of ndk file.')") iline
        call fatal("mod_ndk","ndkreadnext",trim(smsg)//" "//trim(emsg),istatus)
      end if
      iline=iline+1
      rec%err_mt%exponent=0
      read(lundk,"(a80)",iostat=istatus,iomsg=emsg) line
      if(istatus.ne.0) then
        if(is_iostat_end(istatus)) then
          call fatal("mod_ndk","ndkreadnext","Unexpected mid-record EOF in ndkfile. "//trim(emsg),istatus)
        else
          write(smsg,"('Mid-record error when reading from ndk file, line',1x,i0,'.')") iline
          call fatal("mod_ndk","ndkreadnext",trim(smsg)//' '//trim(emsg),istatus)
        end if
      end if
      read(line,"(a3,1x,3(f7.3,1x,i2,1x,i3,1x),f7.3,1x,2(i3,1x,i2,1x,i4,1x))",iostat=istatus,iomsg=emsg) &
                                        rec%version &
                                        ,rec%eigs(1)%eigenvalue,itmp(1),itmp(2) &
                                        ,rec%eigs(2)%eigenvalue,itmp(3),itmp(4) &
                                        ,rec%eigs(3)%eigenvalue,itmp(5),itmp(6) &
                                        ,rec%scalar_moment &
                                        ,itmp(7),itmp(8),itmp(9),itmp(10),itmp(11),itmp(12)
      if(istatus.ne.0) then
        write(smsg,"('Unable to parse line',1x,i0,1x,'of ndk file.')") iline
        call fatal("mod_ndk","ndkreadnext",trim(smsg)//" "//trim(emsg),istatus)
      end if
      iline=iline+1
      rec%eigs(1)%plunge =real(itmp(1),k_rs)
      rec%eigs(1)%azimuth=real(itmp(2),k_rs)
      rec%eigs(2)%plunge =real(itmp(3),k_rs)
      rec%eigs(2)%azimuth=real(itmp(4),k_rs)
      rec%eigs(3)%plunge =real(itmp(5),k_rs)
      rec%eigs(3)%azimuth=real(itmp(6),k_rs)

      rec%nodal_plane(1)%strike=real(itmp(7), k_rs)
      rec%nodal_plane(1)%dip   =real(itmp(8), k_rs)
      rec%nodal_plane(1)%rake  =real(itmp(9), k_rs)
      rec%nodal_plane(2)%strike=real(itmp(10),k_rs)
      rec%nodal_plane(2)%dip   =real(itmp(11),k_rs)
      rec%nodal_plane(2)%rake  =real(itmp(12),k_rs)
      return_status=NDK_SUCCESS ! Success
    else if(is_iostat_end(istatus)) then
      ! Reached end of file
      ! Allow graceful handling; no need to call fatal() here
      return_status=NDK_EOF
    else
      !Some other error
      write(smsg,"('Error when reading from ndk file, line',1x,i0,'.')") iline
      call fatal("mod_ndk","ndkreadnext",trim(smsg)//' '//trim(emsg),istatus)
    end if
  end subroutine ndkreadnext
end module mod_ndk
