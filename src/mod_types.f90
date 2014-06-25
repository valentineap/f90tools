module mod_types
  ! Andrew Valentine
  ! Universiteit Utrect
  ! a.p.valentine@uu.nl
  ! 2008-2014

  ! Module providing various compound types and functions to operate upon them
  ! Incorporates code from earlier modules:
  ! - datetime
  ! - ah

  use mod_log
  implicit none
  private

  public :: k_i0,k_rs,k_rd,date,julian_date,location,moment_tensor,assignment(=),operator(+),operator(-)
  public :: pi, twopi, sqrt2, euler, pi_d, twopi_d, fourpi_d, bigg


  integer,parameter :: k_i0=kind(1)
  integer,parameter :: k_rs=kind(1.0)  ! single precision
  integer,parameter :: k_rd=kind(1.d0) ! double precision

  ! Numerical constants (based on Numerical Recipes via DA-A)
  real(k_rs), parameter :: pi=3.141592653589793238462643383279502884197_k_rs
  real(k_rs), parameter :: twopi=6.283185307179586476925286766559005768394_k_rs
  real(k_rs), parameter :: sqrt2=1.41421356237309504880168872420969807856967_k_rs
  real(k_rs), parameter :: euler=0.5772156649015328606065120900824024310422_k_rs
  real(k_rd), parameter :: pi_d=3.141592653589793238462643383279502884197_k_rd
  real(k_rd), parameter :: twopi_d=6.283185307179586476925286766559005768394_k_rd
  real(k_rd), parameter :: fourpi_d=12.56637061_k_rd
  real(k_rd), parameter :: bigg=6.6723e-11_k_rd ! this was changed from 6.673e-11_k_rd
  !real(k_rd), parameter :: bigg=6.6723e-21_k_rd ! this was changed from 6.673e-11_k_rd


  ! integer indices for the mode programs
  integer, parameter :: RADIAL_MODE_INDEX           = 1
  integer, parameter :: SPHEROIDAL_MODE_INDEX       = 2
  integer, parameter :: TOROIDAL_MODE_INDEX         = 3
  integer, parameter :: SOLID_REGION_INDEX          = 1
  integer, parameter :: FLUID_REGION_INDEX          = 2
  integer, parameter :: SELF_GRAVITATION_INDEX      = 1
  integer, parameter :: COWLING_APPROXIMATION_INDEX = 2
  integer, parameter :: NO_GRAVITATION_INDEX        = 3
  integer, parameter :: NO_ATTENUATION_INDEX        = 1
  integer, parameter :: LOG_DISPERSION_INDEX        = 2
  integer, parameter :: NO_COUNTING_INDEX           = 1
  integer, parameter :: DO_COUNTING_INDEX           = 1

  type date
    sequence
    integer(k_i0) :: year
    integer(k_i0) :: month
    integer(k_i0) :: day
    integer(k_i0) :: hour
    integer(k_i0) :: minute
    real(k_rs)    :: second
  end type date

  type julian_date
    sequence
    integer(k_i0) :: year
    integer(k_i0) :: day
    integer(k_i0) :: hour
    integer(k_i0) :: minute
    real(k_rs)    :: second
  end type julian_date

  type location
    sequence
    real(k_rs) :: lat
    real(k_rs) :: lon
    real(k_rs) :: z
  end type location

  type moment_tensor
    sequence
    real(k_rs) :: mrr
    real(k_rs) :: mtt
    real(k_rs) :: mpp
    real(k_rs) :: mrt
    real(k_rs) :: mrp
    real(k_rs) :: mtp
    integer(k_i0) :: exponent
  end type moment_tensor

  interface assignment(=)
    module procedure d2j !date to julian
    module procedure j2d !julian to date
  end interface

  interface operator(+) !Add fixed number of seconds to a given date
    module procedure jpdbl !julian plus double
    module procedure jprea !julian plus single
    module procedure jpint !julian plus integer
    module procedure dpdbl !date plus double
    module procedure dprea !etc
    module procedure dpint
  end interface

  interface operator(-) ! Subtract seconds from date; compute seconds between two dates
    module procedure jmdbl !julian minus double
    module procedure jmrea !etc
    module procedure jmint
    module procedure jmj !julian minus julian
    module procedure jmd !etc
    module procedure dmdbl
    module procedure dmrea
    module procedure dmint
    module procedure dmd
    module procedure dmj
  end interface

  integer,dimension(12,0:1),parameter :: daylist=reshape((/0,31,59,90,120,151,181,212,243,273,304,334 &
                                                          ,0,31,60,91,121,152,182,213,244,274,305,335/),(/12,2/))

contains
  function lpyr(year) result(l)
    !Test whether 'year' is  leap year.
    !A year is a leap year if: divisible by 400
    !                          divisible by 4 but not 100 (except as above)
    integer,intent(in) :: year
    integer ::l !We want integer not logical to allow simple lookup in 'daylist'

    if(mod(year,400).eq.0) then
      l=1
    else if(mod(year,4).eq.0.and.mod(year,100).ne.0) then
      l=1
    else
      l=0
    end if
  end function lpyr

  subroutine d2j(myjul,mydate)
    type(date),intent(in) :: mydate
    type(julian_date),intent(out) :: myjul

    myjul%hour=mydate%hour
    myjul%minute=mydate%minute
    myjul%second=mydate%second
    myjul%year=mydate%year
    myjul%day=mydate%day+daylist(mydate%month,lpyr(mydate%year))
  end subroutine d2j

  subroutine j2d(mydate,myjul)
    type(julian_date),intent(in):: myjul
    type(date),intent(out):: mydate

    integer :: isleap

    mydate%hour=myjul%hour
    mydate%minute=myjul%minute
    mydate%second=myjul%second
    mydate%year=myjul%year

    isleap=lpyr(myjul%year)
    mydate%month=min(12,1+myjul%day/30)
    mydate%day=myjul%day-daylist(mydate%month,isleap)
    if(mydate%day.le.0) then
      mydate%month=mydate%month-1
      mydate%day=myjul%day-daylist(mydate%month,isleap)
    end if
  end subroutine j2d

  function jpdbl(myjul,second) result(j)
    type(julian_date),intent(in) :: myjul
    type(julian_date)::j
    real(k_rd),intent(in):: second

    real(k_rd) :: jps
    integer::jpints,minuteadd,houradd,dayadd,iflp

    j%year =myjul%year
    j%day  =myjul%day
    j%hour=myjul%hour
    j%minute =myjul%minute
    jps=real(myjul%second,k_rd)+second

    if(jps.ge.60.d0) then
      jpints=int(jps)
      minuteadd=int(jpints/60)
      j%minute=j%minute+minuteadd
      j%second=real((jps-minuteadd*60.d0),k_rs)
      if(j%second.lt.0.) then
        j%second=j%second+60.
        j%minute=j%minute-1
      end if
      if(j%second.ge.60.) then
        j%second=j%second-60.
        j%minute=j%minute+1
      end if
      if(j%second.lt.0..or.j%second.ge.60.) call fatal("mod_types","jpdbl","Logic error: unexpected value in 'j%second'.")
      if(j%minute.ge.60) then
        houradd=int(j%minute/60)
        j%hour=j%hour+houradd
        j%minute=j%minute-(houradd*60)
        if(j%hour.ge.24) then
          dayadd=int(j%hour/24)
          j%day=j%day+dayadd
          j%hour=j%hour-(24*dayadd)
          fixyr: do
            iflp=lpyr(j%year)
            if(j%day.gt.365+iflp) then
              j%day=j%day-365-iflp
              j%year=j%year+1
              cycle fixyr
            end if
            exit fixyr
          end do fixyr
        end if
      end if
    else if(jps.lt.0.d0) then
      minuteadd=int((-jps/60.d0)+1.d0)
      j%minute=j%minute-minuteadd
      j%second=real((jps+(real(minuteadd,k_rd)*60.d0)),k_rs)
      if(j%second.lt.0.) then
        j%second=j%second+60.
        j%minute=j%minute-1
      end if
      if(j%second.ge.60.) then
        j%second=j%second-60.
        j%minute=j%minute+1
      end if
      if(j%second.lt.0..or.j%second.ge.60.) call fatal("mod_types","jpdbl","Logic error: Unexpected value in 'j%second'.")
      if(j%minute.lt.0) then
        houradd=int((-j%minute-1)/60)+1
        j%hour=j%hour-houradd
        j%minute=j%minute+(60*houradd)
        if(j%minute.lt.0.or.j%minute.ge.60) call fatal("mod_types","jpdbl","Logic error: Unexpected value in 'j%minute'.")
        if(j%hour.lt.0) then
          dayadd=(-j%hour-1)/24+1
          j%day=j%day-dayadd
          j%hour=j%hour+(24*dayadd)
          if(j%hour.lt.0.or.j%hour.ge.24) call fatal("mod_types","jpdbl","Logic error: Unexpected value in 'j%hour'.")
          fixyr2: do
            if(j%day.le.0) then
              j%year=j%year-1
              iflp=lpyr(j%year)
              j%day=j%day+365+iflp
              cycle fixyr2
            end if
            exit fixyr2
          end do fixyr2
        end if
      end if
    else
      j%second=real(jps,k_rs)
    end if
  end function jpdbl

  function jmdbl(myjul,second) result(j)
    type(julian_date),intent(in) :: myjul
    type(julian_date) :: j
    real(k_rd),intent(in) :: second
    j=jpdbl(myjul,-second)
  end function jmdbl

  function jprea(myjul,second) result(j)
    type(julian_date),intent(in) :: myjul
    type(julian_date) :: j
    real(k_rs),intent(in) :: second
    j=jpdbl(myjul,real(second,k_rd))
  end function jprea

  function jmrea(myjul,second) result(j)
    type(julian_date),intent(in) :: myjul
    type(julian_date):: j
    real(k_rs),intent(in) :: second
    j=jpdbl(myjul,-real(second,k_rd))
  end function jmrea


  function jpint(myjul,second) result(j)
    type(julian_date),intent(in) :: myjul
    type(julian_date) ::j
    integer,intent(in) :: second
    j=jpdbl(myjul,real(second,k_rd))
  end function jpint

  function jmint(myjul,second) result(j)
    type(julian_date),intent(in) :: myjul
    type(julian_date)::j
    integer,intent(in)          :: second
    j=jpdbl(myjul,-real(second,k_rd))
  end function jmint

  function jmj(jul1,jul2) result(s)
    !Returns jul1-jul2 in seconds
    type(julian_date),intent(in) :: jul1,jul2
    real(k_rd)::s
    integer :: min,daydiff,i,islp

    daydiff=0
    if(jul1%year.ne.jul2%year) then
      do i=min(jul1%year,jul2%year),max(jul1%year,jul2%year)
        islp=lpyr(i)
        daydiff=daydiff+365+islp
      end do
      if(jul1%year.lt.jul2%year) daydiff=-daydiff
    end if
    s=jul1%second-jul2%second+60.d0*(real(jul1%minute-jul2%minute,k_rd)+60.d0*(real(jul1%hour-jul2%hour,k_rd)+ &
    24.d0*real(daydiff+jul1%day-jul2%day,k_rd)))
  end function jmj

  function jmd(jul1,day2) result(s)
    type(julian_date),intent(in) :: jul1
    real(k_rd)::s
    type(date),intent(in) :: day2
    type(julian_date) :: jul2
    jul2=day2 ! Does type conversion
    s=jul1-jul2
  end function jmd


  function dpdbl(mydat,second) result(d)
    type(date),intent(in) :: mydat
    type(date)::d
    real(k_rd),intent(in) :: second
    type(julian_date) :: myjul
    myjul=mydat
    myjul=myjul+second
    d=myjul
  end function dpdbl

  function dmdbl(mydat,second) result(d)
    type(date),intent(in)   :: mydat
    type(date):: d
    real(k_rd),intent(in) :: second
    d=dpdbl(mydat,-second)
  end function dmdbl

  function dprea(mydat,second) result(d)
    type(date),intent(in) :: mydat
    type(date)::d
    real(k_rs),intent(in) :: second
    type(julian_date) :: myjul
    myjul=mydat
    myjul=myjul+second
    d=myjul
  end function dprea

  function dmrea(mydat,second) result(d)
    type(date),intent(in)   :: mydat
    type(date)::d
    real(k_rs),intent(in) :: second
    d=dpdbl(mydat,-real(second,k_rd))
  end function dmrea

  function dpint(mydat,second) result(d)
    type(date),intent(in) :: mydat
    type(date)::d
    integer,intent(in) :: second

    type(julian_date) :: myjul
    myjul=mydat
    myjul=myjul+second
    d=myjul
  end function dpint

  function dmint(mydat,second) result(d)
    type(date),intent(in)   :: mydat
    integer,intent(in)      :: second
    type(date)::d
    d=mydat+(-real(second,k_rd))
  end function dmint

  function dmd(day1,day2) result(s)
    type(date),intent(in) :: day1
    type(date),intent(in) :: day2
    real(k_rd)::s
    type(julian_date) :: jul1,jul2

    jul1=day1
    jul2=day2

    s=jul1-jul2
  end function dmd

  function dmj(day1,jul2) result(s)
    type(date),intent(in)        :: day1
    type(julian_date),intent(in) :: jul2
    real(k_rd)::s
    type(julian_date) :: jul1
    jul1=day1
    s=jul1-jul2
  end function dmj
end module mod_types
