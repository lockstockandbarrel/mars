#define _WIN32
!>>>>> ././src/constants.f90
!> @file
!! @defgroup group_constants Constants
!! Compile-time constants used throughout the fpx preprocessor.
!!
!! This module centralizes all numerical limits and fixed configuration
!! values required by the implementation. These parameters define the
!! maximum sizes of internal buffers and the allowed nesting depth of
!! various preprocessing constructs.
!!
!! The chosen values aim to balance flexibility and robustness:
!!
!! - large enough to accommodate realistic scientific Fortran code,
!! - small enough to avoid excessive memory consumption,
!! - fixed at compile time to simplify implementation and improve performance.
!!
!! The constants are used across multiple modules, including macro
!! expansion, conditional compilation, tokenization, and the extended
!! #for directive implementation.
!!
!! @section constants_examples Examples
!!
!! -# Limiting nested conditional directives:
!!      @code{.f90}
!!      #if A
!!      #if B
!!      #if C
!!      !some code
!!      #endif
!!      #endif
!!      #endif
!!      ...
!!      @endcode
!!
!!      Nesting is permitted up to @link fpx_constants::max_cond_depth MAX_COND_DEPTH @endlink levels.
!!
!! -# Nested #for loops:
!!      @code{.f90}
!!      #for T in [integer, real]
!!      #for K in [32, 64]
!!      !some code
!!      #endfor
!!      #endfor
!!      ...
!!      @endcode
!!
!!      Loop nesting is limited by @link fpx_constants::max_for_depth MAX_FOR_DEPTH @endlink.
!!
!! -# Long macro expansions:
!!      @code{.f90}
!!      #define MESSAGE "very long text ..."
!!      ...
!!      @endcode
!!
!!      Intermediate buffers are sized according to @link fpx_constants::max_line_len MAX_LINE_LEN @endlink.
module fpx_constants
   implicit none; private

   !> Maximum permitted length of an input or generated line.
   !!
   !! This limit applies to raw source lines, continued lines,
   !! and intermediate results produced during macro expansion.
   !!
   !! The value should be sufficiently large for practically all
   !! modern Fortran source files while preventing unbounded memory use.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: MAX_LINE_LEN = 4096

   !> Maximum nesting depth of generic parser structures.
   !!
   !! Used internally whenever recursive parser constructs require
   !! bounded stack-like storage.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: MAX_DEPTH = 50

   !> Maximum nesting depth of conditional compilation directives.
   !!
   !! Applies to constructs such as:
   !! - `#if`
   !! - `#ifdef`
   !! - `#ifndef`
   !! - `#elif`
   !! - `#else`
   !!
   !! Excessive nesting beyond this limit results in diagnostics.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: MAX_COND_DEPTH = 50

   !> Maximum nesting depth of `#for` loops.
   !!
   !! Applies to the fpx extension:
   !! @code
   !! #for ...
   !! !some code
   !! #endfor
   !! ...
   !! @endcode
   !!
   !! Nested loops exceeding this limit generate an error.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: MAX_FOR_DEPTH = 50

   !> Maximum number of tokens generated during tokenization.
   !!
   !! This value bounds the temporary token buffers used by
   !! expression parsing and macro processing.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: MAX_TOKENS = 100

   !> Maximum number of parameters accepted by a macro definition.
   !!
   !! Applies to function-like macros:
   !! @code
   !! #define F(a,b,c) ...
   !! ...
   !! @endcode
   !!
   !! Variadic arguments count toward this limit.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: MAX_PARAMS = 10

   !> Default chunk size used for internal buffering operations.
   !!
   !! This value is primarily used when processing data incrementally
   !! to avoid frequent reallocations.
   !!
   !! @ingroup group_constants
   integer, parameter, public :: CHKSIZE = 72
end module

!>>>>> ././src/context.f90
!> @file
!! @defgroup group_context Context
!! Source context information used for diagnostics and error reporting.
!!
!! This module defines the lightweight @ref fpx_context::context type used throughout
!! the fpx preprocessor to associate source-location information with
!! diagnostics, warnings, notes, and error messages.
!!
!! Every diagnostic emitted by fpx is accompanied by a context object
!! describing where the event occurred. This enables the generation of
!! modern compiler-style messages containing file names, line numbers,
!! source snippets, and caret annotations.
!!
!! A context captures:
!!
!! - the original source line,
!! - the corresponding 1-based line number,
!! - the path of the source file being processed.
!!
!! The information stored in a context is consumed primarily by the
!! @link fpx_logging fpx_logging @endlink module to produce precise and user-friendly diagnostics.
!!
!! For example:
!!
!! @code
!! error: Undefined macro 'DEBUG'
!! --> src/main.F90:42
!!    |
!! 42 | #ifdef DEBUG
!!    | ^^^^^ not defined
!! @endcode
!!
!! Accurate context information becomes particularly important when
!! processing nested #include files, evaluating conditional directives,
!! or reporting errors originating from macro expansions.
!!
!! @section context_examples Examples
!!
!! 1. Creating a context object:
!! @code{.f90}
!! type(context) :: ctx
!!
!! ctx = context( &
!! content='real :: x = PI*r**2', &
!! line=27, &
!! path='src/utils.F90')
!! ...
!! @endcode
!!
!! 2. Using context when reporting diagnostics:
!! @code{.f90}
!! call printf(render(diagnostic_report( &
!! LEVEL_ERROR, &
!! message='Undefined macro', &
!! source=ctx%path), &
!! ctx%content, ctx%line))
!! ...
!! @endcode
!!
!! 3. Updating context during #include processing:
!! @code{.f90}
!! included_ctx = context( &
!! content=first_line, &
!! line=1, &
!! path=resolved_include_path)
!! ...
!! @endcode
module fpx_context
   implicit none; private

   !> Snapshot of a source location within the preprocessing stream.
   !!
   !! Instances of this type accompany diagnostics throughout fpx and
   !! provide the information required to identify where an event
   !! occurred in the original source.
   !!
   !! The stored line content is typically displayed alongside
   !! highlighted regions when rendering diagnostics.
   !!
   !! @section context_type_examples Examples
   !! @code{.f90}
   !! type(context) :: ctx
   !!
   !! ctx = context('lorem ipsum', 42, 'example.F90')
   !! ...
   !! @endcode
   !!
   !! @section context_type_remarks Remarks
   !! - A new context is typically created for each processed source line.
   !! - Entering an `#include` file naturally creates contexts referring
   !! to the included file.
   !! - Context objects are lightweight and inexpensive to copy.
   !! - They form the foundation of fpx's compiler-style diagnostics.
   !!
   !! @section context_type_constructors Constructors
   !!
   !! Initializes a new instance of the @ref context type.
   !!
   !! @b Constructor
   !! @code{.f90}
   !! type(context) function context(character(*) content, integer line, character(*) path)
   !! @endcode
   !!
   !! @param[in] content
   !!   Source line associated with the diagnostic.
   !! @param[in] line
   !!   One-based line number within the source file.
   !! @param[in] path
   !!   Relative or absolute path of the source file.
   !!
   !! @return Newly constructed context object.
   !!
   !! @ingroup group_context
   type, public :: context
      character(:), allocatable   :: content
      integer                     :: line
      character(:), allocatable   :: path
   end type

end module

!>>>>> ././src/date.f90
!> @file
!! @defgroup group_date Date
!! Lightweight date and time utilities used by the fpx preprocessor.
!!
!! This module provides a compact `datetime` type together with a small set
!! of date/time operations required by fpx. Its primary purpose is to support
!! expansion of the predefined macros:
!!
!! - `__DATE__`
!! - `__TIME__`
!! - `__TIMESTAMP__`
!!
!! Rather than providing a complete calendaring framework, this module focuses
!! on the functionality required by preprocessing tasks:
!!
!! - Retrieval of the current local date and time using `date_and_time()`
!! - Construction of datetime objects from numeric components or strings
!! - Parsing of commonly encountered date/time representations
!! - Flexible formatting through `to_string(fmt)`
!! - Day-of-week computation using Zeller's congruence
!!
!! The implementation deliberately remains lightweight and dependency-free.
!! It is not intended to replace dedicated date/time libraries, but instead
!! provides exactly the capabilities required by fpx while remaining portable
!! across standard-conforming Fortran compilers.
!!
!! @section date_examples Examples
!!
!! 1. Expanding predefined macros:
!! @code{.f90}
!!    type(datetime) :: dt
!!
!!    dt = now()
!!
!!    print *, '__DATE__      -> ', dt%to_string('MMM-dd-yyyy')
!!    print *, '__TIME__      -> ', dt%to_string('HH:mm:ss')
!!    print *, '__TIMESTAMP__ -> ', dt%to_string('ddd-MMM-yyyy HH:mm:ss')
!! ...
!! @endcode
!!
!! 2. Constructing a datetime from a string:
!! @code{.f90}
!!    type(datetime) :: build_time
!!
!!    build_time = datetime('2025-08-12 09:30:00')
!!
!!    print *, build_time%to_string('ddd-MMM-yyyy')
!! ...
!! @endcode
!!
!! 3. Constructing a datetime from components:
!! @code{.f90}
!!    type(datetime) :: epoch
!!
!!    epoch = datetime(1970, 1, 1)
!!
!!    print *, epoch%to_string()
!! ...
!! @endcode
!!
!! 4. Timestamping preprocessing operations:
!! @code{.f90}
!!    type(datetime) :: dt
!!
!!    dt = now()
!!    print *, 'Preprocessing started at ', dt%to_string('HH:mm:ss')
!! ...
!! @endcode
module fpx_date
   use, intrinsic :: iso_fortran_env, only: i1 => int8, i2 => int16
   implicit none; private

   public :: now

   !> Compact representation of date and time
   !! Stores all components in minimal integer kinds to reduce memory usage.
   !! All fields are public for easy access.
   !! <h2  class="groupheader">Examples</h2>
   !! @code{.f90}
   !!    type(datetime) :: bt
   !!    bt = datetime('2025-08-12 09:30:00')
   !!    print *, 'build on: ', bt%to_string('ddd-MMM-yyyy')
   !!    ...
   !! @endcode
   !! @section datetime_type_remarks Remarks
   !! This type intentionally provides only the functionality required by fpx.
   !! It is designed to be compact, portable, and efficient rather than serving
   !! as a comprehensive date/time framework.
   !!
   !! @section datetime_type_constructors Constructors
   !! Initializes a new instance of the @ref datetime class.
   !! Two constructor forms are available:
   !!
   !! - Construction from numeric components
   !! - Construction from a character representation
   !!
   !! @b Constructor
   !! @code{.f90}
   !! type(datetime) function datetime(character(*) string, (optional) character(*) fmt)
   !! @endcode
   !!
   !! @param[in] string
   !!   date as string
   !! @param[in] fmt
   !!   (optional) date format
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(datetime) :: d
   !! d = datetime('2025-08-12 09:30:00')
   !! ...
   !! @endcode
   !!
   !! @b Constructor
   !! @code{.f90}
   !! type(datetime) function datetime((optional) integer year, (optional) integer month, ...)
   !! @endcode
   !!
   !! @param[in] year
   !!   (optional)
   !! @param[in] month
   !!   (optional)
   !! @param[in] day
   !!   (optional)
   !! @param[in] hour
   !!   (optional)
   !! @param[in] minute
   !!   (optional)
   !! @param[in] second
   !!   (optional)
   !! @param[in] millisecond
   !!   (optional)
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(datetime) :: d
   !! d = datetime(1970, 1, 1)
   !! ...
   !! @endcode
   !!
   !! @return The constructed datetime object.
   !!
   !! @ingroup group_date
   type, public :: datetime
      private
      integer(i2), public  :: year    !< Year
      integer(i1), public  :: month   !< Month
      integer(i1), public  :: day     !< Day
      integer(i1), public  :: hour    !< Hour
      integer(i1), public  :: minute  !< Minute
      integer(i1), public  :: second  !< Second
      integer(i2), public  :: millisecond  !< Millisecond
   contains
      procedure, pass(this), public :: to_string => datetime_to_string
      procedure, pass(this), public :: parse => datetime_parse
   end type

   !> Constructor interface for @ref datetime type
   !!
   !! @ingroup group_date
   interface datetime
      !! @cond
      module procedure :: datetime_new, datetime_new_from_string
      !! @endcond
   end interface

contains

   !> Constructor
   elemental function datetime_new(year, month, day, hour, minute, second, millisecond) result(that)
      integer, intent(in), optional   :: year
      integer, intent(in), optional   :: month
      integer, intent(in), optional   :: day
      integer, intent(in), optional   :: hour
      integer, intent(in), optional   :: minute
      integer, intent(in), optional   :: second
      integer, intent(in), optional   :: millisecond
      type(datetime)                  :: that

      that%year = 0_i2; if (present(year)) that%year = int(year, kind=i2)
      that%month = 0_i1; if (present(month)) that%month = int(month, kind=i1)
      that%day = 0_i1; if (present(day)) that%day = int(day, kind=i1)
      that%hour = 0_i1; if (present(hour)) that%hour = int(hour, kind=i1)
      that%minute = 0_i1; if (present(minute)) that%minute = int(minute, kind=i1)
      that%second = 0_i1; if (present(second)) that%second = int(second, kind=i1)
      that%millisecond = 0_i2; if (present(millisecond)) that%millisecond = int(millisecond, kind=i2)
   end function

   elemental function datetime_new_from_string(string, fmt) result(that)
      character(*), intent(in)            :: string
      character(*), intent(in), optional  :: fmt
      type(datetime)                      :: that

      if (present(fmt)) then
         call that%parse(string, fmt)
      else
         call that%parse(string)
      end if
   end function

   !> Return current local date and time
   !! Uses intrinsic `date_and_time()` and populates all fields including milliseconds.
   !! @return the datetime object corresponding to the current time
   !!

   !! @ingroup group_date
   function now() result(res)
      type(datetime)  :: res
      !private
      integer :: values(9)

      call date_and_time(values=values)

      res%year = int(values(1), kind=i2)
      res%month = int(values(2), kind=i1)
      res%day = int(values(3), kind=i1)
      res%hour = int(values(5), kind=i1)
      res%minute = int(values(6), kind=i1)
      res%second = int(values(7), kind=i1)
      res%millisecond = int(values(8), kind=i2)
   end function

   !> Returns the day of the week calculated using Zeller's congruence.
   !! Returned value is an integer scalar in the range [0-6], such that:
   !! - 0: Sunday
   !! - 1: Monday
   !! - 2: Tuesday
   !! - 3: Wednesday
   !! - 4: Thursday
   !! - 5: Friday
   !! - 6: Saturday
   !!
   !! @ingroup group_date
   pure elemental integer function weekday(this)
      class(datetime), intent(in) :: this
      !private
      integer :: year, month, j, k

      year = this%year
      month = this%month

      if (month <= 2) then
         month = month + 12
         year = year - 1
      end if

      j = year / 100
      k = mod(year, 100)

      weekday = mod(this%day + ((month + 1) * 26) / 10 + k + k / 4 + j / 4 + 5 * j, 7) - 1

      if (weekday < 0) weekday = 6
   end function

   !> Parse date/time from string using common formats
   !!
   !! Supports ISO, US, and abbreviated month formats.
   !! On error, defaults to Unix epoch (1970-01-01 00:00:00)
   !! Perform conversion to ISO string
   !! - d: Represents the day of the month as a number from 1 through 31.
   !! - dd: Represents the day of the month as a number from 01 through 31.
   !! - ddd: Represents the abbreviated name of the day (Mon, Tues, Wed, etc).
   !! - dddd: Represents the full name of the day (Monday, Tuesday, etc).
   !! - h: 12-hour clock hour (e.g. 4).
   !! - hh: 12-hour clock, with a leading 0 (e.g. 06)
   !! - H: 24-hour clock hour (e.g. 15)
   !! - HH: 24-hour clock hour, with a leading 0 (e.g. 22)
   !! - m: Minutes
   !! - mm: Minutes with a leading zero
   !! - M: Month number(eg.3)
   !! - MM: Month number with leading zero(eg.04)
   !! - MMM: Abbreviated Month Name (e.g. Dec)
   !! - MMMM: Full month name (e.g. December)
   !! - s: Seconds
   !! - ss: Seconds with leading zero
   !! - t: Abbreviated AM / PM (e.g. A or P)
   !! - tt: AM / PM (e.g. AM or PM
   !! - y: Year, no leading zero (e.g. 2015 would be 15)
   !! - yy: Year, leading zero (e.g. 2015 would be 015)
   !! - yyy: Year, (e.g. 2015)
   !! - yyyy: Year, (e.g. 2015)
   !!
   !! @ingroup group_date
   elemental subroutine datetime_parse(this, string, fmt)
      class(datetime), intent(inout)      :: this
      character(*), intent(in)            :: string
      character(*), intent(in), optional  :: fmt
      !private
      integer :: ierr
      logical :: valid
      character(256) :: errmsg
      character(len(string)) :: tmp
      character(:), allocatable :: dftfmt

      if (present(fmt)) then
         dftfmt = fmt
      else
         if (len_trim(string) == 10) then
            dftfmt = 'yyyy-MM-dd'
         else
            dftfmt = 'yyyy-MM-dd HH:mm:ss'
         end if
      end if

      tmp = string

      this%year = 0_i2; this%month = 0_i1; this%day = 0_i1
      this%hour = 0_i1; this%minute = 0_i1; this%second = 0_i1; this%millisecond = 0_i2

      select case (dftfmt)
       case ('MMM-dd-yyyy')
         select case (tmp(:3))
          case ('Jan'); tmp(:3) = ' 01'
          case ('Feb'); tmp(:3) = ' 02'
          case ('Mar'); tmp(:3) = ' 03'
          case ('Apr'); tmp(:3) = ' 04'
          case ('May'); tmp(:3) = ' 05'
          case ('Jun'); tmp(:3) = ' 06'
          case ('Jul'); tmp(:3) = ' 07'
          case ('Aug'); tmp(:3) = ' 08'
          case ('Sep'); tmp(:3) = ' 09'
          case ('Oct'); tmp(:3) = ' 10'
          case ('Nov'); tmp(:3) = ' 11'
          case ('Dec'); tmp(:3) = ' 12'
         end select
         read(tmp(2:), '(i2.2,1x,i2.2,1x,i4.4)', iostat=ierr, iomsg=errmsg) &
            this%month, &
            this%day, &
            this%year
       case ('MMM-dd-yyyy HH:mm:ss', 'MMM-dd-yyyyTHH:mm:ss')
         select case (tmp(:3))
          case ('Jan'); tmp(:3) = ' 01'
          case ('Feb'); tmp(:3) = ' 02'
          case ('Mar'); tmp(:3) = ' 03'
          case ('Apr'); tmp(:3) = ' 04'
          case ('May'); tmp(:3) = ' 05'
          case ('Jun'); tmp(:3) = ' 06'
          case ('Jul'); tmp(:3) = ' 07'
          case ('Aug'); tmp(:3) = ' 08'
          case ('Sep'); tmp(:3) = ' 09'
          case ('Oct'); tmp(:3) = ' 10'
          case ('Nov'); tmp(:3) = ' 11'
          case ('Dec'); tmp(:3) = ' 12'
         end select
         read(tmp(2:), '(i2.2,1x,i2.2,1x,i4.4,1x,i2.2,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
            this%month, &
            this%day, &
            this%year, &
            this%hour, &
            this%minute, &
            this%second
       case ('yyyy-MM')
         read(tmp, '(i4.4,1x,i2.2)', iostat=ierr, iomsg=errmsg) &
            this%year, &
            this%month
       case ('yyyy-MM-dd')
         read(tmp, '(i4.4,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
            this%year, &
            this%month, &
            this%day
       case ('dd-MM-yyyy')
         read(tmp, '(i2.2,1x,i2.2,1x, i4.4)', iostat=ierr, iomsg=errmsg) &
            this%day, &
            this%month, &
            this%year
       case ('MM-dd-yyyy')
         read(tmp, '(i2.2,1x,i2.2,1x,i4.4)', iostat=ierr, iomsg=errmsg) &
            this%month, &
            this%day, &
            this%year
       case ('yyyy-MM-ddTHH:mm:ss', 'yyyy-MM-dd HH:mm:ss')
         read(tmp, '(i4.4,2(1x,i2.2),1x,i2.2,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
            this%year, &
            this%month, &
            this%day, &
            this%hour, &
            this%minute, &
            this%second
       case ('HH:mm:ss')
         read(tmp, '(i2.2,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
            this%hour, &
            this%minute, &
            this%second
      end select

      if (ierr > 0) then
         this%year = 1970_i2; this%month = 1_i1; this%day = 1_i1
         this%hour = 0_i1; this%minute = 0_i1; this%second = 0_i1; this%millisecond = 0_i2
      end if
   end subroutine

   !> Format datetime as string using flexible format codes
   !! Supports many common patterns including those required for `__DATE__` and `__TIMESTAMP__`.
   !! Default format: 'yyyy-MM-ddTHH:mm:ss'
   !!
   !! @ingroup group_date
   function datetime_to_string(this, fmt) result(res)
      class(datetime), intent(in)          :: this
      character(*), intent(in), optional  :: fmt
      character(:), allocatable           :: res
      !private
      character   :: sep, dash
      character(:), allocatable :: dftfmt, tmp, tmp2
      integer :: ierr
      character(256) :: errmsg

      if (present(fmt)) then
         dftfmt = fmt
      else
         dftfmt = 'yyyy-MM-ddTHH:mm:ss'
      end if
      ! Manager optional parameters
      sep = merge('T', ' ', index(dftfmt, 'T') > 0)
      dash = merge('-', ' ', index(dftfmt, '-') > 0)

      allocate(character(25) :: res)
      ! Perform conversion to ISO string

      select case (this%month)
       case (1); tmp = 'Jan'
       case (2); tmp = 'Feb'
       case (3); tmp = 'Mar'
       case (4); tmp = 'Apr'
       case (5); tmp = 'May'
       case (6); tmp = 'Jun'
       case (7); tmp = 'Jul'
       case (8); tmp = 'Aug'
       case (9); tmp = 'Sep'
       case (10); tmp = 'Oct'
       case (11); tmp = 'Nov'
       case (12); tmp = 'Dec'
      end select
      select case (weekday(this))
       case (0); tmp2 = 'Sun'
       case (1); tmp2 = 'Mon'
       case (2); tmp2 = 'Tue'
       case (3); tmp2 = 'Wed'
       case (4); tmp2 = 'Thu'
       case (5); tmp2 = 'Fri'
       case (6); tmp2 = 'Sat'
      end select

      select case (dftfmt)
       case ('MMM-dd-yyyy', 'MMM dd yyyy')
         write(res, '(a3,a1,i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
            tmp, &
            dash, &
            this%day, &
            dash, &
            this%year
       case ('MMM-ddd-yyyy', 'MMM ddd yyyy')
         write(res, '(a3,a1,a3," ",i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
            tmp, &
            dash, &
            tmp2, &
            this%day, &
            dash, &
            this%year
       case ('MMM-dd-yyyy HH:mm:ss', 'MMM-dd-yyyyTHH:mm:ss', 'MMM dd yyyy HH:mm:ss', 'MMM dd yyyyTHH:mm:ss')
         write(res, '(a3,a1,i2.2,a1,i4.4,a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
            tmp, &
            dash, &
            this%day, &
            dash, &
            this%year, &
            this%hour, &
            this%minute, &
            this%second
       case ('MMM-ddd-yyyy HH:mm:ss', 'MMM-ddd-yyyyTHH:mm:ss', 'MMM ddd yyyy HH:mm:ss', 'MMM ddd yyyyTHH:mm:ss')
         write(res, '(a3,a1,a3," ",i2.2,a1,i4.4,a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
            tmp, &
            dash, &
            tmp2, &
            this%day, &
            dash, &
            this%year, &
            this%hour, &
            this%minute, &
            this%second
       case ('yyyy-MM', 'yyyy MM')
         write(res, '(i4.4,a1,i2.2)', iostat=ierr, iomsg=errmsg) &
            this%year, &
            dash, &
            this%month
       case ('yyyy-MM-dd', 'yyyy MM dd')
         write(res, '(i4.4,2(a1,i2.2))', iostat=ierr, iomsg=errmsg) &
            this%year, &
            dash, &
            this%month, &
            dash, &
            this%day
       case ('yyyy-MM-ddd', 'yyyy MM ddd')
         write(res, '(i4.4,a1,i2.2,a1,a3," ",i2.2)', iostat=ierr, iomsg=errmsg) &
            this%year, &
            dash, &
            this%month, &
            dash, &
            tmp2, &
            this%day
       case ('dd-MM-yyyy', 'dd MM yyyy')
         write(res, '(i2.2,a1,i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
            this%day, &
            dash, &
            this%month, &
            dash, &
            this%year
       case ('ddd-MM-yyyy', 'ddd MM yyyy')
         write(res, '(a3,a1,i2.2," ",i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
            tmp2, &
            dash, &
            this%month, &
            this%day, &
            dash, &
            this%year
       case ('MM-dd-yyyy', 'MM dd yyyy')
         write(res, '(i2.2,a1,i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
            this%month, &
            dash, &
            this%day, &
            dash, &
            this%year
       case ('MM-ddd-yyyy', 'MM ddd yyyy')
         write(res, '(i2.2,a1,a3," ",i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
            this%month, &
            dash, &
            tmp2, &
            this%day, &
            dash, &
            this%year
       case ('yyyy-MM-ddTHH:mm:ss', 'yyyy-MM-dd HH:mm:ss', 'yyyy MM ddTHH:mm:ss', 'yyyy MM dd HH:mm:ss')
         write(res, '(i4.4,2(a1,i2.2),a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
            this%year, &
            dash, &
            this%month, &
            dash, &
            this%day, &
            sep, &
            this%hour, &
            this%minute, &
            this%second
       case ('yyyy-MM-dddTHH:mm:ss', 'yyyy-MM-ddd HH:mm:ss', 'yyyy MM dddTHH:mm:ss', 'yyyy MM ddd HH:mm:ss')
         write(res, '(i4.4,a1,i2.2,a1,a3," ",i2.2,a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
            this%year, &
            dash, &
            this%month, &
            dash, &
            tmp2, &
            this%day, &
            sep, &
            this%hour, &
            this%minute, &
            this%second
       case ('HH:mm:ss')
         write(res, '(i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
            this%hour, &
            this%minute, &
            this%second
      end select
      res = trim(res)
   end function
end module

!>>>>> ././src/graph.f90
!> @file
!! @defgroup group_graph Graph
!! Directed graph utilities used for macro dependency analysis.
!!
!! This module provides a lightweight directed graph implementation used
!! internally by the fpx preprocessor to detect cyclic dependencies during
!! macro expansion.
!!
!! Unlike general-purpose graph libraries, this implementation is optimized
!! for the small graphs typically encountered during preprocessing:
!!
!! - Vertices are represented by 1-based integer identifiers.
!! - Edges are stored in a dense adjacency structure for fast traversal.
!! - Cycle detection uses depth-first search (DFS) with a recursion stack.
!! - Invalid vertices are ignored gracefully.
!! - Memory management is automatic through a finalizer.
!!
!! The primary use case is preventing infinite recursion caused by macros
!! expanding, directly or indirectly, to themselves:
!!
!! @code{.f90}
!!    #define A B
!!    #define B C
!!    #define C A
!! ...
!! @endcode
!!
!! Before expanding a macro, fpx records dependencies in a graph and checks
!! whether introducing a new dependency would create a cycle.
!!
!! @section graph_examples Examples
!!
!! 1. Detecting a circular dependency:
!! @code{.f90}
!!    type(digraph) :: g
!!    logical       :: cycle
!!
!!    g = digraph(3)
!!
!!    call g%add_edge(1, 2)
!!    call g%add_edge(2, 3)
!!    call g%add_edge(3, 1)
!!
!!    cycle = g%is_circular(1)
!!    print *, cycle      ! prints .true.
!! ...
!! @endcode
!!
!! 2. Detecting an acyclic dependency chain:
!! @code{.f90}
!!    type(digraph) :: g
!!
!!    g = digraph(4)
!!
!!    call g%add_edge(1, 2)
!!    call g%add_edge(2, 3)
!!    call g%add_edge(3, 4)
!!
!!    print *, g%is_circular(1)   ! .false.
!! ...
!! @endcode
!!
!! 3. Internal usage during macro expansion:
!! @code{.f90}
!!    call graph%add_edge(current_macro, referenced_macro)
!!
!!    if (graph%is_circular(referenced_macro)) then
!!        ! Prevent recursive expansion
!!    end if
!! ...
!! @endcode
module fpx_graph
   implicit none; private

   !> Directed graph supporting efficient cycle detection.
   !!
   !! The graph stores a fixed number of vertices identified by
   !! integers in the range `[1, vertices]`.
   !!
   !! Edges are represented internally using a dense adjacency
   !! structure together with per-vertex occupancy counters.
   !! This approach avoids repeated allocations and is well suited
   !! to the relatively small dependency graphs encountered by fpx.
   !!
   !! @section digraph_type_examples Examples
   !! @code{.f90}
   !!    type(digraph) :: g
   !!
   !!    g = digraph(2)
   !!    call g%add_edge(1, 2)
   !!
   !!    print *, g%is_circular(1)
   !! ...
   !! @endcode
   !!
   !! @section digraph_type_constructors Constructors
   !! Initializes a new directed graph.
   !!
   !! @b Constructor
   !! @code{.f90}
   !! type(digraph) function digraph(integer vertices)
   !! @endcode
   !!
   !! @param[in] vertices
   !!   Number of vertices in the graph.
   !!
   !! @return A newly constructed directed graph.
   !!
   !! @section digraph_type_remarks Remarks
   !! - Vertices are numbered from 1.
   !! - The number of vertices is fixed after construction.
   !! - Intended primarily for internal use by the macro expander.
   !!
   !! @ingroup group_graph
   type, public :: digraph
      integer, private :: vertices  !< Number of vertices
      integer, allocatable, private :: adjacency_list(:, :)  !< Adjacency list containing the connection information between the vertices.
      integer, allocatable, private :: list_sizes(:)  !< Actually used portion of each row of @ref adjacency_list.
   contains
      private
      procedure, pass(this), public :: add_edge => graph_add_edge
      procedure, pass(this), public :: is_circular => graph_has_cycle_dfs
      final :: graph_final
   end type

   !> Construct a directed graph with a fixed number of vertices.
   !!
   !! Allocates the internal adjacency structures and initializes
   !! the graph without any edges.
   !!
   !! @param[in] vertices
   !!   Number of vertices.
   !!
   !! @return Newly initialized graph.
   !!
   !! @ingroup group_graph
   interface digraph
      !! @cond
      module procedure :: graph_new
      !! @endcond
   end interface

contains

   type(digraph) function graph_new(vertices) result(that)
      integer, intent(in) :: vertices
      integer :: i

      that%vertices = vertices
      allocate(that%adjacency_list(vertices, vertices), source=0)
      allocate(that%list_sizes(vertices), source=0)
   end function

   !> Add a directed edge to the graph.
   !!
   !! Inserts an edge from `source` to `destination`.
   !! If either vertex lies outside the valid range,
   !! the request is ignored.
   !!
   !! @param[inout] this        Graph instance.
   !! @param[in]    source      Source vertex (1-based).
   !! @param[in]    destination Destination vertex (1-based).
   !! @param[out]   overflow    Optional flag indicating whether the
   !!                           insertion position was already occupied.
   !!
   !! @note Duplicate edges are not explicitly filtered.
   !!
   !! @ingroup group_graph
   subroutine graph_add_edge(this, source, destination, overflow)
      class(digraph), intent(inout)   :: this
      integer, intent(in)             :: source
      integer, intent(in)             :: destination
      logical, intent(out), optional  :: overflow

      if (source < 1 .or. source > this%vertices .or. &
         destination < 1 .or. destination > this%vertices) then
         return  ! Skip invalid edges
      end if

      this%list_sizes(source) = this%list_sizes(source) + 1
      if (this%list_sizes(source) <= this%vertices) then
         if (present(overflow)) overflow = this%adjacency_list(source, this%list_sizes(source)) /= 0
         this%adjacency_list(source, this%list_sizes(source)) = destination
      end if
   end subroutine

   !> Determine whether a cycle is reachable from a vertex.
   !!
   !! Performs a depth-first traversal starting from
   !! `start_vertex` and detects back edges using a
   !! recursion stack.
   !!
   !! @param[in] this
   !!   Graph instance.
   !! @param[in] start_vertex
   !!   Vertex from which the search begins.
   !!
   !! @return `.true.` if a cycle exists in the reachable component;
   !!         `.false.` otherwise.
   !!
   !! @ingroup group_graph
   logical function graph_has_cycle_dfs(this, start_vertex) result(has_cycle)
      class(digraph), intent(in) :: this
      integer, intent(in) :: start_vertex
      !private
      logical, allocatable :: visited(:), recursion_stack(:)

      if (start_vertex < 1 .or. start_vertex > this%vertices) then
         has_cycle = .false.
         return
      end if

      allocate(visited(this%vertices), source=.false.)
      allocate(recursion_stack(this%vertices), source=.false.)

      has_cycle = dfs_recursive(this, start_vertex, visited, recursion_stack)

      deallocate(visited, recursion_stack)
   end function

   !> Recursive DFS worker used for cycle detection.
   !!
   !! This routine implements the actual traversal algorithm used
   !! by @ref graph_has_cycle_dfs. It maintains both a visited set
   !! and a recursion stack in order to identify back edges.
   !!
   !! @ingroup group_graph
   recursive logical function dfs_recursive(this, vertex, visited, recursion_stack) result(has_cycle)
      class(digraph), intent(in) :: this
      integer, intent(in) :: vertex
      logical, intent(inout) :: visited(:), recursion_stack(:)
      integer :: neighbor, i

      visited(vertex) = .true.
      recursion_stack(vertex) = .true.

      do i = 1, this%list_sizes(vertex)
         neighbor = this%adjacency_list(vertex, i)
         if (neighbor < 1 .or. neighbor > this%vertices) cycle  ! Skip invalid neighbors
         if (.not. visited(neighbor)) then
            if (dfs_recursive(this, neighbor, visited, recursion_stack)) then
               has_cycle = .true.
               return
            end if
         else if (recursion_stack(neighbor)) then
            has_cycle = .true.
            return
         end if
      end do

      recursion_stack(vertex) = .false.
      has_cycle = .false.
   end function

   !> Finalizer for the directed graph.
   !!
   !! Releases all dynamically allocated storage associated with
   !! the graph when it leaves scope.
   !!
   !! @ingroup group_graph
   subroutine graph_final(this)
      type(digraph), intent(inout) :: this
      if (allocated(this%adjacency_list)) deallocate(this%adjacency_list)
      if (allocated(this%list_sizes)) deallocate(this%list_sizes)
   end subroutine

end module

!>>>>> ././src/logging.f90
!> @file
!! @defgroup group_logging Logging
!! Global logging, ANSI-colored diagnostics, and pretty error/warning reporting for fpx
!!
!! This module is the central place for all human-readable output in the fpx preprocessor.
!! It provides:
!! - Full ANSI color and style support (bold, underline, colors, etc.)
!! - Structured diagnostic messages with source context, line numbers, and caret markers
!! - Pretty-printed multi-line error/warning/help/note/info reports
!! - Label-based highlighting of specific code ranges (like rustc-style diagnostics)
!! - Recursive sub-diagnostic support for nested explanations
!!
!! Designed to produce modern, readable, IDE-friendly output similar to rustc, clang, or cargo.
!! When `nocolor = .true.` (or terminal does not support ANSI), falls back to plain text.
!!
!! @section logging_examples Examples
!!
!! 1. Simple error message
!! @code{.f90}
!! print '(A)', render(diagnostic_report( &
!!     LEVEL_ERROR, &
!!     message='unexpected token', &
!!     label=label_type('expected expression', 8, 3)), &
!!     'a = +')
!! ...
!! @endcode
!!
!! 2. Colored message (used internally for verbose logging):
!! @code{.f90}
!!    use fpx_logging
!!
!!    verbose = .true.
!!    print '(A)', render('Macro expanded: PI = 3.14159')
!! ..
!! @endcode
!!
!! 3. Full diagnostic report (like a compiler error):
!! @code{.f90}
!!  character(*), parameter :: input = &
!!  '# This is a TOML document.' // nl // &
!!  'title = "TOML Example"' // nl // &
!!  '[owner]' // nl // &
!!  'name = "Tom Preston-Werner"' // nl // &
!!  'dob = 1979-05-27T07:32:00-08:00 # First class dates' // nl // &
!!  '[database]' // nl // &
!!  'server = "192.168.1.1"' // nl // &
!!  'ports = [ 8001, 8001, 8002 ]' // nl // &
!!  'connection_max = 5000' // nl // &
!!  'enabled = true' // nl // &
!!  '[servers]' // nl // &
!!  '  # Indentation (tabs and/or spaces) is allowed but not required' // nl // &
!!  '  [servers.alpha]' // nl // &
!!  '  ip = "10.0.0.1"' // nl // &
!!  '  dc = "eqdc10"' // nl // &
!!  '  [servers.beta]' // nl // &
!!  '  ip = "10.0.0.2"' // nl // &
!!  '  dc = "eqdc10"' // nl // &
!!  '[title]' // nl // &
!!  'data = [ ["gamma", "delta"], [1, 2] ]' // nl // &
!!  '# Line breaks are OK when inside arrays' // nl // &
!!  'hosts = [' // nl // &
!!  '  "alpha",' // nl // &
!!  '  "omega"' // nl // &
!!  ']'
!!
!!    print '(A)', render(diagnostic_report(level_error, &
!!       message="duplicated key 'title' found", &
!!       source="example.toml", &
!!       label=[label_type("table 'title' redefined here", 19, 2, 5, .true.), &
!!              label_type("first defined here", 2, 1, 5)]), &
!!              input)
!!    end
!! ...
!! @endcode
!!
!!    Output might look like (colored in terminal):
!! @code
!! error: duplicated key 'title' found
!!  --> example.toml:19:2-6
!!    |
!!  1 | # This is a TOML document.
!!  2 | title = "TOML Example"
!!    | ----- first defined here
!!  3 | [owner]
!!    :
!! 18 |   dc = "eqdc10"
!! 19 | [title]
!!    |  ^^^^^ table 'title' redefined here
!! 20 | data = [ ["gamma", "delta"], [1, 2] ]
!!    |
!! @endcode
!!
!! @par ANSI style & color reference (used internally)
!! - Styles: BOLD_ON, UNDERLINE_ON, INVERSE_ON, STRIKETHROUGH_ON, ...
!! - Foreground: RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, ...
!! - Background: same as foreground but prefixed with BG_
!!
!! @note This code is adapted from [pretty-diagnostics](https://github.com/awvwgk/pretty-diagnostics).
!! The visual presentation is inspired by modern compiler diagnostics
!! such as rustc and clang, while being adapted for Fortran workflows.
module fpx_logging
   use iso_c_binding

   implicit none; private

   public :: render, &
      printf, &
      diagnostic_report, &
      label_type, &
      LEVEL_ERROR, &
      LEVEL_WARNING, &
      LEVEL_HELP, &
      LEVEL_NOTE, &
      LEVEL_INFO

   !> @brief Master switch for verbose diagnostic output
   !! Default value is `.false.` (quiet mode).
   !! Set to `.true.` to get detailed step-by-step information about
   !! preprocessing actions. Safe to modify at any time � the change takes
   !! effect immediately for all subsequent operations.
   !! @ingroup group_logging
   logical, public :: verbose = .false.

   !> @brief Switch for controling the ANSI color output
   !! Default value is `.true.` (color mode on).
   !! Set to `.false.` to get raw string output.
   !! @ingroup group_logging
   logical, public :: nocolor = .false.

   !! @cond
   character(1), parameter :: NL = new_line('a')  !< New line character.
   character(1), parameter :: ESCAPE = achar(27)  !< '\' character.
   character(2), parameter :: CODE_START = ESCAPE // '['  !< Start ansi code, "\[".
   character(1), parameter :: CODE_END = 'm'  !< End ansi code, "m".
   character(4), parameter :: CODE_CLEAR = CODE_START // '0' // CODE_END  !< Clear all styles, "\[0m".

   character(17), parameter :: STYLES(1:2, 1:16) = reshape([ &
      'BOLD_ON          ', '1                ', &
      'ITALICS_ON       ', '3                ', &
      'UNDERLINE_ON     ', '4                ', &
      'INVERSE_ON       ', '7                ', &
      'STRIKETHROUGH_ON ', '9                ', &
      'BOLD_OFF         ', '22               ', &
      'ITALICS_OFF      ', '23               ', &
      'UNDERLINE_OFF    ', '24               ', &
      'INVERSE_OFF      ', '27               ', &
      'STRIKETHROUGH_OFF', '29               ', &
      'FRAMED_ON        ', '51               ', &
      'ENCIRCLED_ON     ', '52               ', &
      'OVERLINED_ON     ', '53               ', &
      'FRAMED_OFF       ', '54               ', &
      'ENCIRCLED_OFF    ', '54               ', &
      'OVERLINED_OFF    ', '55               ' &
      ], [2, 16])  !< Styles.

   character(15), parameter :: COLORS_FG(1:2, 1:17) = reshape([ &
      'BLACK          ', '30             ', &
      'RED            ', '31             ', &
      'GREEN          ', '32             ', &
      'YELLOW         ', '33             ', &
      'BLUE           ', '34             ', &
      'MAGENTA        ', '35             ', &
      'CYAN           ', '36             ', &
      'WHITE          ', '37             ', &
      'DEFAULT        ', '39             ', &
      'BLACK_INTENSE  ', '90             ', &
      'RED_INTENSE    ', '91             ', &
      'GREEN_INTENSE  ', '92             ', &
      'YELLOW_INTENSE ', '93             ', &
      'BLUE_INTENSE   ', '94             ', &
      'MAGENTA_INTENSE', '95             ', &
      'CYAN_INTENSE   ', '96             ', &
      'WHITE_INTENSE  ', '97             ' &
      ], [2, 17])  !< Foreground colors.

   character(15), parameter :: COLORS_BG(1:2, 1:17) = reshape([ &
      'BLACK          ', '40             ', &
      'RED            ', '41             ', &
      'GREEN          ', '42             ', &
      'YELLOW         ', '43             ', &
      'BLUE           ', '44             ', &
      'MAGENTA        ', '45             ', &
      'CYAN           ', '46             ', &
      'WHITE          ', '47             ', &
      'DEFAULT        ', '49             ', &
      'BLACK_INTENSE  ', '100            ', &
      'RED_INTENSE    ', '101            ', &
      'GREEN_INTENSE  ', '102            ', &
      'YELLOW_INTENSE ', '103            ', &
      'BLUE_INTENSE   ', '104            ', &
      'MAGENTA_INTENSE', '105            ', &
      'CYAN_INTENSE   ', '106            ', &
      'WHITE_INTENSE  ', '107            ' &
      ], [2, 17])  !< Background colors.
   !! @endcond

   !> Generic renderer for diagnostics and source excerpts.
   !!
   !! Supported overloads:
   !! - render(diagnostic_report, source)
   !! - render(character)
   !! - render(character, label_type)
   !! - render(character, label_type(:))
   !!
   !! Returns a formatted character string suitable for printing.
   !!
   !! @ingroup group_logging
   interface render
      module procedure :: render_diagnostic
      module procedure :: render_text
      module procedure :: render_text_with_label
      module procedure :: render_text_with_labels
   end interface

   enum, bind(c)
      enumerator :: LEVEL_ERROR = 0
      enumerator :: LEVEL_WARNING = 1
      enumerator :: LEVEL_HELP = 2
      enumerator :: LEVEL_NOTE = 3
      enumerator :: LEVEL_INFO = 4
   end enum

   !> Diagnostic label identifying a region of source text.
   !!
   !! A label highlights a specific character range within a source line
   !! and may carry an explanatory message. Labels are used to produce
   !! compiler-style diagnostics similar to those of rustc or clang.
   !!
   !! Labels may be primary or secondary:
   !! - Primary labels identify the principal cause of the diagnostic.
   !! - Secondary labels provide additional context.
   !!
   !! Primary labels determine the source location shown in the
   !! diagnostic header and are rendered using '^' markers.
   !!
   !! Secondary labels are rendered using '-' markers and provide
   !! supplementary context.
   !! @note
   !! Character positions are 1-based.
   !! The highlighted range spans:
   !!
   !!    first <= position < finish
   !!
   !! where `finish` is exclusive.
   !!
   !! @section label_type_examples Examples
   !! @code{.f90}
   !! type(string) :: s
   !! s = 'foo'
   !! ...
   !! @endcode
   !!
   !! @section label_type_constructor Constructors
   !! Initializes a new instance of the label_type class
   !! @b Constructor
   !! @code{.f90}
   !! type(label_type) function string(character(*) text, integer first, integer length, (optional) integer level)
   !! @endcode
   !!
   !! @param[in] text
   !!   Text displayed next to the label
   !! @param[in] first
   !!   Position of the label
   !! @param[in] length
   !!   Length of the label
   !! @param[in] level
   !!   (optional) Level of the label
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(label_type) :: label
   !! label = label_type('Syntax error', 5, 7)
   !! ...
   !! @endcode
   !! @return The constructed label_type object.
   !!
   !! @b Constructor
   !! @code{.f90}
   !! type(label_type) function label_type(integer line, character(*) text, integer first, integer length, (optional) integer level, (optional) logical primary)
   !! @endcode
   !!
   !! @param[in] line
   !!   line number for the label
   !! @param[in] text
   !!   Text displayed next to the label
   !! @param[in] first
   !!   Position of the label
   !! @param[in] length
   !!   Length of the label
   !! @param[in] level
   !!   (optional) Level of the label
   !! @param[in] primary
   !!   .true. if the label is the primary one
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(label_type) :: label
   !! label = label_type(1, 'Syntax error', 5, 7, LEVEL_ERROR, .true.)
   !! ...
   !! @endcode
   !! @return The constructed label_type object.
   !!
   !! @ingroup group_logging
   type label_type
      !> Level of message
      integer, allocatable        :: level
      !> Primary message
      logical                     :: primary
      !> Line number of message
      integer                     :: line
      !> First character of message
      integer                     :: first
      !> Last character of message
      integer                     :: finish
      !> Message text
      character(:), allocatable   :: text
   end type

   interface label_type
      module procedure :: label_new
      module procedure :: label_new_with_line
   end interface

   !> Structured compiler diagnostic.
   !!
   !! A diagnostic report consists of:
   !!
   !! - a severity level,
   !! - a primary message,
   !! - one or more source labels,
   !! - optional nested diagnostics.
   !!
   !! Reports can be rendered using the generic interface
   !! `render(...)` to produce human-readable output.
   !!
   !! @ingroup group_logging
   type :: diagnostic_report
      !> Level of message
      integer :: level
      !> Primary message
      character(:), allocatable :: message
      !> Context of the diagnostic source
      character(:), allocatable :: source
      !> Messages associated with this diagnostic
      type(label_type), allocatable :: label(:)
      !> Additional diagnostic information
      type(diagnostic_report), allocatable :: sub(:)
   end type

   interface diagnostic_report
      module procedure diagnostic_new
   end interface

   !! @private
   type :: line_token
      integer :: first, finish
   end type

contains

   !> Colorize and stylize strings, DEFAULT kind.
   !! @param[in] string Input string.
   !! @param[in] foreground Foreground color definition.
   !! @param[in] background Background color definition.
   !! @param[in] style Style definition.
   pure function colorize(string, foreground, background, style) result(res)
      character(*), intent(in)           :: string
      character(*), intent(in), optional :: foreground
      character(*), intent(in), optional :: background
      character(*), intent(in), optional :: style
      character(:), allocatable :: res
      !private
      integer :: i

      res = string
      if (nocolor) return
      if (present(foreground)) then
         i = color_index(upper(foreground))
         if (i > 0) res = CODE_START // trim(COLORS_FG(2, i)) // CODE_END // res // CODE_CLEAR
      end if
      if (present(background)) then
         i = color_index(upper(background))
         if (i > 0) res = CODE_START // trim(COLORS_BG(2, i)) // CODE_END // res // CODE_CLEAR
      end if
      if (present(style)) then
         i = style_index(upper(style))
         if (i > 0) res = CODE_START // trim(STYLES(2, i)) // CODE_END // res // CODE_CLEAR
      end if
   end function

   !> Return the array-index corresponding to the queried color.
   !! @note Because Foreground and backround colors lists share the same name,
   !! no matter what array is used to find the color index.
   !! Thus, the foreground array is used.
   elemental integer function color_index(color) result(res)
      character(*), intent(in) :: color  !< Color definition.
      !private
      integer :: i

      res = 0
      do i = 1, size(COLORS_FG, dim=2)
         if (trim(COLORS_FG(1, i)) == trim(adjustl(color))) then
            res = i
            exit
         end if
      end do
   end function

   !> Return the array-index corresponding to the queried style.
   elemental integer function style_index(style) result(res)
      character(*), intent(in) :: style  !< Style definition.
      !private
      integer :: i

      res = 0
      do i = 1, size(STYLES, dim=2)
         if (trim(STYLES(1, i)) == trim(adjustl(style))) then
            res = i
            exit
         end if
      end do
   end function

   !> Return a string with all uppercase characters.
   elemental function upper(string)
      character(*), intent(in) :: string  !< Input string.
      character(len(string)) :: upper  !< Upper case string.
      !private
      integer, parameter :: a = iachar('a'), z = iachar('z'), CASE_DIFF = iachar('a') - iachar('A')
      integer :: i, ichar

      do i = 1, len(string)
         ichar = iachar(string(i:i))
         if (ichar >= a .and. ichar <= z) ichar = ichar - CASE_DIFF
         upper(i:i) = achar(ichar)
      end do
   end function

   type(label_type) pure function label_new(text, first, length, level) result(that)
      character(*), intent(in)            :: text
      integer, intent(in)                 :: first
      integer, intent(in)                 :: length
      integer, intent(in), optional      :: level

      that%text = text
      that%line = 1
      that%first = max(1, first)
      that%finish = that%first + length
      that%primary = .true.
      if (present(level)) that%level = level
   end function

   type(label_type) pure function label_new_with_line(line, text, first, length, primary, level) result(that)
      integer, intent(in)                 :: line
      character(*), intent(in)            :: text
      integer, intent(in)                 :: first
      integer, intent(in)                 :: length
      logical, intent(in), optional       :: primary
      integer, intent(in), optional      :: level

      that%text = text
      that%line = line
      that%first = max(1, first)
      that%finish = that%first + length
      if (present(primary)) then
         that%primary = primary
      else
         that%primary = .true.
      end if
      if (present(level)) that%level = level
   end function

   !> Create new diagnostic message
   !! @param[in] level Level of message
   !! @param[in] message Primary message
   !! @param[in] source Context of the diagnostic source
   !! @param[in] label Messages associated with this diagnostic
   !! @param[in] diagnostic Additional diagnostic information
   type(diagnostic_report) function diagnostic_new(level, message, source, label, diagnostic) result(that)
      integer, intent(in)                             :: level
      character(*), intent(in), optional              :: message
      character(*), intent(in), optional              :: source
      type(label_type), intent(in), optional          :: label(..)
      type(diagnostic_report), intent(in), optional   :: diagnostic(:)
      !private
      integer :: i

      that%level = level
      if (present(message)) that%message = message
      if (present(source)) that%source = source
      if (present(label)) then
         if (allocated(that%label)) deallocate(that%label)
         select rank (label)
         rank(0)
         allocate(that%label(1))
         that%label(1) = label
         if (.not. allocated(that%label(1)%level)) that%label(1)%level = level
         rank(1)
         allocate(that%label, source=label)
         do i = 1, size(label)
            if (.not. allocated(that%label(i)%level)) that%label(i)%level = level
         end do
      end select
   end if
   if (present(diagnostic)) that%sub = diagnostic

   if (allocated(that%label)) then
      if (.not. any(that%label(:)%primary)) then
         that%label(1)%primary = .true.
      end if
   end if
end function

pure function line_tokens(input) result(res)
   character(*), intent(in)        :: input
   type(line_token), allocatable :: res(:)
   !private
   integer :: first, finish

   if (len(input) == 0) then
      allocate(res(1))
      res(1)=line_token(1,0)
      return
   end if

   first = 1; finish = 0
   allocate(res(0))
   do while (first <= len(input))
      finish = index(input(first + 1:), NL) + first - 1
      if (finish < first) then
         finish = len(input)
      end if

      res = [res, line_token(first, finish)]

      first = finish + (1 + len(NL))
   end do
end function

pure recursive function render_diagnostic(diag, input, linemum) result(res)
   type(diagnostic_report), intent(in)     :: diag
   character(*), intent(in)                :: input
   integer, intent(in), optional           :: linemum
   character(:), allocatable :: res
   !private
   integer :: i

   res = render_message(diag%level, diag%message)

   if (allocated(diag%label)) then
      res = res // NL // render_text_with_labels(input, diag%label, source=diag%source, linemum=linemum)
   else
      res = res // NL // render_text_with_labels(input, [label_type('', 1, len_trim(input))], source=diag%source, linemum=&
         linemum)
   end if

   if (allocated(diag%sub)) then
      do i = 1, size(diag%sub)
         res = res // NL // render_diagnostic(diag%sub(i), input, linemum)
      end do
   end if
end function

pure function render_message(level, message) result(res)
   integer, intent(in) :: level
   character(*), intent(in), optional :: message
   character(:), allocatable :: res

   if (present(message)) then
      res = level_name(level) // colorize(': ' // message, style='bold_on')
   else
      res = level_name(level)
   end if
end function

pure function level_name(level) result(res)
   integer, intent(in) :: level
   character(:), allocatable :: res
   !private
   character(:), allocatable :: name, fg

   select case(level)
    case (LEVEL_ERROR)
      name='error';   fg='red'
    case (LEVEL_WARNING)
      name='warning'; fg='yellow'
    case (LEVEL_HELP)
      name='help';    fg='cyan'
    case (LEVEL_NOTE)
      name='note';    fg='blue'
    case (LEVEL_INFO)
      name='info';    fg='magenta'
    case default
      name='unknown'; fg='blue'
   end select

   res = colorize(name, foreground=fg, style='bold_on')
end function

pure function render_source(source, offset) result(res)
   character(*), intent(in)    :: source
   integer, intent(in)         :: offset
   character(:), allocatable :: res

   res = repeat(' ', offset) // colorize('-->', foreground='blue') // ' ' // source
end function

pure function render_text(input, source, linenum) result(res)
   character(*), intent(in)            :: input
   character(*), intent(in), optional  :: source
   integer, intent(in), optional       :: linenum
   character(:), allocatable :: res
   !private
   integer :: i, offset, iline
   type(line_token), allocatable :: token(:)

   iline = 1; if (present(linenum)) iline = linenum
   token = line_tokens(input)
   offset = integer_width(iline)

   if (present(source)) then
      res = render_source(source, offset) // NL // &
         repeat(' ', offset + 1) // colorize('|', foreground='blue')
   else
      res = repeat(' ', offset + 1) // colorize('|', foreground='blue')
   end if

   do i = 1, size(token)
      res = res // NL // render_line(input(token(i)%first:token(i)%finish), to_string(iline + i - 1, offset))
   end do
   res = res // NL // repeat(' ', offset + 1) // colorize('|', foreground='blue')
end function

pure function render_text_with_label(input, label, source, linenum) result(res)
   character(*), intent(in)            :: input
   type(label_type), intent(in)        :: label
   character(*), intent(in), optional  :: source
   integer, intent(in), optional       :: linenum
   character(:), allocatable :: res

   res = render_text_with_labels(input, [label], source, linenum)
end function

pure function render_text_with_labels(input, labels, source, linemum) result(res)
   character(*), intent(in)            :: input
   type(label_type), intent(in)        :: labels(:)
   character(*), intent(in), optional  :: source
   integer, intent(in), optional       :: linemum
   character(:), allocatable :: res
   !private
   integer :: i, j, offset, first, finish, iline
   type(line_token), allocatable :: token(:)
   logical, allocatable :: display(:)

   token = line_tokens(input)
   first = max(1, minval(labels%line) - 1)
   finish = min(size(token), maxval(labels%line) + 1)
   iline = 1; if (present(linemum)) iline = linemum
   offset = integer_width(iline)

   i = 1  ! Without a primary we use the first label
   do j = 1, size(labels)
      if (labels(j)%primary) then
         i = j
         exit
      end if
   end do

   if (present(source)) then
      res = render_source(source, offset) // ':' // &
         to_string(labels(i)%line) // ':' // &
         to_string(labels(i)%first) // '-' // to_string(labels(i)%finish) // NL // &
         repeat(' ', offset + 1) // colorize('|', foreground='blue')
   else
      res = repeat(' ', offset + 1) // colorize('|', foreground='blue')
   end if

   allocate(display(first:finish), source=.false.)
   do j = 1, size(labels)
      display(max(first, labels(j)%line - 1):min(finish, labels(j)%line + 1)) = .true.
   end do

   do i = first, finish
      if (.not. display(i)) then
         if (i > first) then
            if (display(i - 1)) then
               res = res // NL //&
                  repeat(' ', offset + 1) // colorize(':', foreground='blue')
            end if
         end if
         cycle
      end if

      res = res // NL //&
      & render_line(input(token(i)%first:token(i)%finish), &
      &             to_string(iline + i - 1, offset))
      if (any(i == labels%line)) then
         do j = 1, size(labels)
            if (labels(j)%line /= i) cycle
            res = res // NL //&
            & repeat(' ', offset + 1) // colorize('|', foreground='blue') // &
            & render_label(labels(j))
         end do
      end if
   end do
   res = res // NL // repeat(' ', offset + 1) // colorize('|', foreground='blue')
end function

pure function render_label(label) result(res)
   type(label_type), intent(in) :: label
   character(:), allocatable :: res
   !private
   integer :: width
   character(1) :: marker
   character(:), allocatable :: this_color, fg

   marker = merge('^', '-', label%primary)
   width = label%finish - label%first
   fg = 'blue'

   if (allocated(label%level)) then
      select case (label%level)
       case (LEVEL_ERROR)
         fg = 'red'
       case (LEVEL_WARNING)
         fg = 'yellow'
       case (LEVEL_HELP)
         fg = 'cyan'
       case (LEVEL_INFO)
         fg = 'magenta'
      end select
      res = repeat(' ', label%first) // colorize(repeat(marker, width), foreground=fg)
      if (allocated(label%text)) then
         res = res // ' ' // colorize(label%text, foreground=fg)
      end if
   else
      res = repeat(' ', label%first) // repeat(marker, width)
      if (allocated(label%text)) then
         res = res // ' ' // colorize(label%text, foreground='blue')
      end if
   end if
end function

pure function render_line(input, line) result(res)
   character(*), intent(in)    :: input
   character(*), intent(in)    :: line
   character(:), allocatable :: res

   res = line // ' ' // colorize('|', foreground='blue') // ' ' // input
end function

pure integer function integer_width(input) result(res)
   integer, value :: input

   res = 0
   do while (input /= 0)
      input = input / 10
      res = res + 1
   end do

end function

 !> Represent an integer as character sequence.
pure function to_string(val, width) result(res)
   integer, intent(in)             :: val
   integer, intent(in), optional   :: width
   character(:), allocatable :: res
   !private
   integer, parameter :: buffer_len = range(val) + 2
   character(buffer_len) :: buffer
   integer :: n, pos
   character(1), parameter :: numbers(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

   if (val == 0) then
      res = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ''

   pos = buffer_len + 1
   do while (n > 0)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10))
      n = n / 10
   end do
   if (val < 0) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   if (present(width)) then
      res = repeat(' ', max(width - (buffer_len + 1 - pos), 0)) // buffer(pos:)
   else
      res = buffer(pos:)
   end if
end function

 !> Conditionally print a message when verbose logging is enabled.
 !!
 !! This routine is intended for internal tracing and debugging output.
 !! Compiler diagnostics should instead be constructed using
 !! `diagnostic_report` and rendered explicitly.
 !! @param[in] str Input string.
 !! @param[in] fmt (optional) print format.
subroutine printf(str, fmt, unit)
   character(*), intent(in)            :: str
   character(*), intent(in), optional  :: fmt
   integer, intent(in), optional       :: unit

   if (verbose) then
      if (present(fmt)) then
         if (present(unit)) then
            write(unit, fmt) str
         else
            write(*, fmt) str
         end if
      else
         if (present(unit)) then
            write(unit, '(A)') str
         else
            write(*, '(A)') str
         end if
      end if
   end if
end subroutine

end module

!>>>>> ././src/os.f90
!> @file
!! @defgroup group_os OS
!! This module provides portable runtime operating-system detection
!! facilities used throughout the fpx preprocessor.
!!
!! Supported platforms include:
!! - Linux distributions
!! - macOS
!! - Native Microsoft Windows
!! - Cygwin
!! - Solaris/OpenSolaris
!! - FreeBSD
!! - OpenBSD
!!
!! Detection is performed lazily on first use and cached using
!! OpenMP threadprivate storage, ensuring negligible overhead for
!! repeated queries.
!!
!! The implementation relies primarily on environment variables,
!! with fallback detection through the presence of well-known
!! operating-system specific files.
!!
!! This strategy is designed to work reliably in native
!! installations, containers, WSL environments, and most
!! cross-compilation setups.
!!
!! @par Detection Model
!! OS identification is attempted in the following order:
!! 1. Environment variable `OSTYPE`
!! 2. Environment variable `OS`
!! 3. Operating-system specific filesystem probes
!! 4. Fallback to OS_UNKNOWN
!!
!! @section os_examples Examples
!!
!! 1. Basic OS detection:
!! @code{.f90}
!!    integer :: my_os
!!    my_os = get_os_type()
!!    print *, 'Running on: ', os_name(my_os)
!!    !> prints e.g. 'Running on: Linux'
!! @endcode
!!
!! 2. Conditional compilation based on OS:
!! @code{.f90}
!!    !platform specific system call
!!    if (os_is_unix()) then
!!        call system('gcc --version')
!!    else
!!        call execute_command_line('gfortran --version')
!!    end if
!!    ...
!! @endcode
!!
!! 3. Using the cached value explicitly:
!! @code{.f90}
!!    integer :: os_type
!!    os_type = get_os_type()           ! detects and caches
!!    print *, os_is_unix(os_type)      ! fast, no re-detection
!!    ...
!! @endcode
!!
!! 4. Module constants
!!
!! @code{.f90}
!! if (get_os_type() == OS_WINDOWS) then
!!     ...
!! end if
!! ...
!! @endcode
module fpx_os
   implicit none; private

   public ::   get_os_type, &
      os_is_unix, &
      os_name

   !> @brief Unknown / undetected operating system
   !! @ingroup group_os
   integer, parameter, public :: OS_UNKNOWN = 0
   !> @brief Linux (any distribution, including GNU/Linux)
   !! @ingroup group_os
   integer, parameter, public :: OS_LINUX = 1
   !> @brief macOS (Darwin-based Apple operating system)
   !! @ingroup group_os
   integer, parameter, public :: OS_MACOS = 2
   !> @brief Microsoft Windows (native, 32-bit or 64-bit)
   !! @ingroup group_os
   integer, parameter, public :: OS_WINDOWS = 3
   !> @brief Cygwin POSIX environment on Windows
   !! @ingroup group_os
   integer, parameter, public :: OS_CYGWIN = 4
   !> @brief Oracle Solaris / OpenSolaris derivatives
   !! @ingroup group_os
   integer, parameter, public :: OS_SOLARIS = 5
   !> @brief FreeBSD and its direct derivatives
   !! @ingroup group_os
   integer, parameter, public :: OS_FREEBSD = 6
   !> @brief OpenBSD
   !! @ingroup group_os
   integer, parameter, public :: OS_OPENBSD = 7
   !> @brief Native Microsoft Windows running on 32-bit x86 architecture.
   !!
   !! This value is returned when the operating system is identified
   !! as Windows and the PROCESSOR_ARCHITECTURE environment variable
   !! indicates an x86 target.
   !!
   !! It can be used when architecture-specific behavior is required.
   !!
   !! @ingroup group_os
   integer, parameter, public :: OS_WINDOWSx86 = 8

contains

   !> Return a human-readable string describing the OS type flag
   !! Converts any of the OS_* integer constants into its corresponding name.
   !! Accepted values include:
   !! - OS_UNKNOWN
   !! - OS_LINUX
   !! - OS_MACOS
   !! - OS_WINDOWS
   !! - OS_WINDOWSx86
   !! - OS_CYGWIN
   !! - OS_SOLARIS
   !! - OS_FREEBSD
   !! - OS_OPENBSD
   !! Useful for logging, error messages, or user output.
   !! @param[in] os OS identifier from get_os_type()
   !! @return    Allocated character string with the OS name
   !!
   !! @b Examples
   !!
   !! @code{.f90}
   !! print *, os_name(OS_LINUX)
   !! !> prints: Linux
   !! ...
   !! @endcode
   !!
   !! @ingroup group_os
   pure function os_name(os) result(res)
      integer, intent(in) :: os
      character(:), allocatable :: res

      select case (os)
       case (OS_LINUX);   res = 'Linux'
       case (OS_MACOS);   res = 'macOS'
       case (OS_WINDOWS); res = 'Windows'
       case (OS_CYGWIN);  res = 'Cygwin'
       case (OS_SOLARIS); res = 'Solaris'
       case (OS_FREEBSD); res = 'FreeBSD'
       case (OS_OPENBSD); res = 'OpenBSD'
       case (OS_UNKNOWN); res = 'Unknown'
       case default     ; res = 'UNKNOWN'
      end select
   end function

   !> Determine the current operating system type
   !! Returns one of the OS_* constants.
   !!
   !! @par Thread Safety
   !! The detected value is cached independently for each OpenMP thread
   !! using threadprivate storage. Concurrent calls therefore incur no
   !! synchronization overhead after the first query on each thread.
   !!
   !! Detection strategy:
   !! 1. Environment variable `OSTYPE` (common on Unix-like systems)
   !! 2. Environment variable `OS` (set on Windows)
   !! 3. Presence of OS-specific files (/etc/os-release, /usr/bin/sw_vers, etc.)
   !!
   !! Returns OS_UNKNOWN if no reliable indicator is found.
   !!
   !! @return OS identifier (OS_LINUX, OS_MACOS, OS_WINDOWS, ...)
   !!
   !!
   !! @b Examples
   !!
   !! @code{.f90}
   !! select case (get_os_type())
   !! case (OS_WINDOWS)
   !!     print *, 'Windows'
   !! case (OS_LINUX)
   !!     print *, 'Linux'
   !! end select
   !! ...
   !! @endcode
   !!
   !! @ingroup group_os
   integer function get_os_type() result(r)
      character(len=255) :: val
      integer            :: length, rc
      logical            :: file_exists
      logical, save      :: first_run = .true.
      integer, save      :: ret = OS_UNKNOWN
      !$omp threadprivate(ret, first_run)

      if (.not. first_run) then
         r = ret
         return
      end if

      first_run = .false.
      r = OS_UNKNOWN

      ! Check environment variable `OSTYPE`.
      call get_environment_variable('OSTYPE', val, length, rc)

      if (rc == 0 .and. length > 0) then
         ! Linux
         if (index(val, 'linux') > 0) then
            r = OS_LINUX
            ret = r
            return
         end if

         ! macOS
         if (index(val, 'darwin') > 0) then
            r = OS_MACOS
            ret = r
            return
         end if

         ! Windows, MSYS, MinGW, Git Bash
         if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
            r = OS_WINDOWS
            ret = r
            return
         end if

         ! Cygwin
         if (index(val, 'cygwin') > 0) then
            r = OS_CYGWIN
            ret = r
            return
         end if

         ! Solaris, OpenIndiana, ...
         if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
            r = OS_SOLARIS
            ret = r
            return
         end if

         ! FreeBSD
         if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
            r = OS_FREEBSD
            ret = r
            return
         end if

         ! OpenBSD
         if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
            r = OS_OPENBSD
            ret = r
            return
         end if
      end if

      ! Check environment variable `OS`.
      call get_environment_variable('OS', val, length, rc)

      if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
         r = OS_WINDOWS
         ret = r
         call get_environment_variable('PROCESSOR_ARCHITECTURE', val, length, rc)
         if (rc == 0 .and. length > 0 .and. index(val, 'x86') > 0) then
            r = OS_WINDOWSx86
            ret = r
         end if
         return
      end if

      ! Linux
      inquire(file='/etc/os-release', exist=file_exists)

      if (file_exists) then
         r = OS_LINUX
         ret = r
         return
      end if

      ! macOS
      inquire(file='/usr/bin/sw_vers', exist=file_exists)

      if (file_exists) then
         r = OS_MACOS
         ret = r
         return
      end if

      ! FreeBSD
      inquire(file='/bin/freebsd-version', exist=file_exists)

      if (file_exists) then
         r = OS_FREEBSD
         ret = r
         return
      end if
   end function

   !> Return .true. if the current (or supplied) OS is Unix-like
   !! Convenience wrapper that returns .true. for any non-Windows platform.
   !! Useful for writing portable code that needs different handling on Windows.
   !! @param[in] os Optional OS identifier; if absent get_os_type() is called
   !! @return   .true. if OS is not Windows, .false. otherwise
   !!
   !! @b Examples
   !!
   !! @code{.f90}
   !! if (os_is_unix()) then
   !!     call execute_command_line('uname -a')
   !! end if
   !! ...
   !! @endcode
   !!
   !! @ingroup group_os
   logical function os_is_unix(os)
      integer, intent(in), optional :: os
      integer :: build_os
      if (present(os)) then
         build_os = os
      else
         build_os = get_os_type()
      end if
      os_is_unix = build_os /= OS_WINDOWS
   end function
end module

!>>>>> ././src/string.f90
!> @file
!! @defgroup group_string String
!! Minimal yet powerful variable-length string type with modern Fortran features.
!! This module implements a lightweight `string` derived type that behaves like
!! a true variable-length character string while remaining fully compatible with
!! intrinsic Fortran character operations.
!!
!! Features:
!! - Automatic memory management via `allocatable character(:)`
!! - Overloaded assignment (`=`) between `string` and `character(*)`
!! - Overloaded operators: `//` (concatenation), `==` (equality), `.contains.` (membership)
!! - Generic interfaces for `len`, `len_trim`, `trim`
!! - Full support for formatted I/O (`write`, `print`)
!! - Helper routines for parsing Fortran source (line continuation, upper/lower case conversion, etc.)
!!
!! The design is intentionally minimal - it provides only what's necessary for
!! robust string handling in scientific and preprocessing applications,
!! avoiding the bloat of larger string libraries while remaining fast and standards-compliant.
!! @note All procedures are `pure` or `elemental` when possible for maximum performance
!!       and usability in array contexts.
!!
!! @section string_examples Examples
!!
!! @par Basic Usage
!! @code{.f90}
!! type(string) :: s, t
!! character(:), allocatable :: line
!!
!! s = 'Hello'              ! Assignment from literal
!! t = s // ' World!'       ! Concatenation
!! print *, t%chars         ! Output: Hello World!
!!
!! if (s == 'Hello') then
!!      print *, 'Equal'
!! else
!!      print *, 'Case sensitive'
!! endif
!!
!! print *, len(t)          ! -> 12
!! print *, len_trim(t)     ! -> 12
!! ...
!! @endcode
!!
!! @par Array and Container Support
!! @code{.f90}
!! type(string) :: words(3)
!! logical      :: found
!!
!! words = [string('apple'), string('banana'), string('cherry')]
!! found = words .contains. 'banana'     ! --> .true.
!! found = words .contains. string('date') ! --> .false.
!! ...
!! @endcode
!!
!! @par Advanced: Source Code Processing
!! @code{.f90}
!! character(len=:), allocatable :: code_line
!! code_line = uppercase('program hello_world  ! comment')  ! --> 'PROGRAM HELLO_WORLD  ! comment'
!! ...
!! @endcode
module fpx_string
   use fpx_constants

   implicit none; private

   public :: len,          &
      len_trim,     &
      trim,         &
      operator(//), &
      operator(.contains.), &
      index

   public :: starts_with,  &
      head,         &
      tail,         &
      previous,     &
      concat,       &
      writechk,     &
      uppercase,    &
      lowercase

   !> Represents text as a sequence of ASCII code units.
   !!        The derived type wraps an allocatable character array.
   !!
   !! @section string_type_examples Examples
   !!
   !! @code{.f90}
   !! type(string) :: s
   !! s = 'foo'
   !! @endcode
   !!
   !! @section string_type_constructor Constructors
   !! Initializes a new instance of the string class
   !! <h3>string(character(:))</h3>
   !! @verbatim type(string) function string(chars) @endverbatim
   !!
   !! @param[in] chars character(:)
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !! s = string('foo')
   !! @endcode
   !! @return The constructed string object.
   !!
   !! @section string_type_remarks Remarks
   !! The string implementation proposed here is kept at the bare
   !! minimum of what is required by the library. There are many
   !! other implementations that can be found.
   !!
   !! @ingroup group_string
   type, public :: string
      character(:), allocatable :: chars  !< Variable length character array
   contains
      !! @cond
      procedure, pass(lhs), private    :: character_assign_string
      procedure, pass(rhs), private    :: string_assign_character
      procedure, pass(lhs), private    :: string_eq_string  !! Equal to string logical operator.
      procedure, pass(lhs), private    :: string_eq_character  !! Equal to character logical operator.
      procedure, pass(rhs), private    :: character_eq_string  !! Equal to character (inverted) logical operator.
      procedure, pass(dtv), private    :: write_formatted  !! Formatted output.
      !! @endcond
      generic, public :: assignment(=) => character_assign_string, &
         string_assign_character
      generic, public :: operator(==) => string_eq_string, &
         string_eq_character, &
         character_eq_string
      generic, public :: write(formatted) => write_formatted
   end type

   !> Return the length of a @ref string object.
   !!
   !! This generic interface extends the intrinsic Fortran `len` function
   !! to support the fpx @ref string type.
   !!
   !! The returned value corresponds to the full length of the underlying
   !! character storage, including trailing blanks.
   !!
   !! If the string is not allocated, the returned value is zero.
   !!
   !! @section len_examples Examples
   !!
   !! Basic usage:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! s = 'foo'
   !! print *, len(s)          ! 3
   !!
   !! s = 'foo '
   !! print *, len(s)          ! 4
   !! ...
   !! @endcode
   !!
   !! Unallocated strings:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! print *, len(s)          ! 0
   !! ...
   !! @endcode
   !!
   !! @ingroup group_string
   interface len
      module procedure :: string_len
   end interface

   !> Return the trimmed length of a @ref string object.
   !!
   !! This generic interface extends the intrinsic Fortran `len_trim`
   !! function to support the fpx @ref string type.
   !!
   !! The returned value corresponds to the number of characters after
   !! removing trailing blanks.
   !!
   !! If the string is not allocated, the returned value is zero.
   !!
   !! @section len_trim_examples Examples
   !!
   !! Basic usage:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! s = 'foo'
   !! print *, len_trim(s)     ! 3
   !!
   !! s = 'foo '
   !! print *, len_trim(s)     ! 3
   !! ...
   !! @endcode
   !!
   !! Unallocated strings:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! print *, len_trim(s)     ! 0
   !! ...
   !! @endcode
   !!
   !! @ingroup group_string
   interface len_trim
      module procedure :: string_len_trim
   end interface

   !> Remove trailing blanks from a @ref string object.
   !!
   !! This generic interface extends the intrinsic Fortran `trim`
   !! function to support the fpx @ref string type.
   !!
   !! The result is returned as a deferred-length intrinsic character
   !! expression with trailing blanks removed.
   !!
   !! If the string is not allocated, an empty character string is returned.
   !!
   !! @section trim_examples Examples
   !!
   !! Basic usage:
   !! @code{.f90}
   !! type(string) :: s
   !! character(:), allocatable :: c
   !!
   !! s = 'hello   '
   !!
   !! c = trim(s)
   !! print *, '"' // c // '"'      ! "hello"
   !! ...
   !! @endcode
   !!
   !! Unallocated strings:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! print *, len(trim(s))         ! 0
   !! ...
   !! @endcode
   !!
   !! @return Deferred-length character string without trailing blanks.
   !!
   !! @ingroup group_string
   interface trim
      module procedure :: string_trim
   end interface

   !> Concatenate string and character expressions.
   !!
   !! Supports all combinations of:
   !! - string // string
   !! - string // character(*)
   !! - character(*) // string
   !!
   !! The result is returned as a deferred-length character expression.
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! s = 'foo'
   !!
   !! print *, s // 'bar'
   !! print *, '>>' // s
   !! ...
   !! @endcode
   !!
   !! @ingroup group_string
   interface operator(//)
      module procedure :: string_concat_string
      module procedure :: string_concat_character
      module procedure :: character_concat_string
   end interface

   !> Test whether a value is present in an array.
   !!
   !! The `.contains.` operator provides convenient membership testing
   !! between arrays of intrinsic characters and arrays of @ref string
   !! objects.
   !!
   !! Supported combinations are:
   !! - `string(:) .contains. string`
   !! - `string(:) .contains. character(*)`
   !! - `character(:) .contains. string`
   !! - `character(:) .contains. character(*)`
   !!
   !! The comparison uses the overloaded equality operator (`==`)
   !! associated with the involved types.
   !!
   !! @section contains_examples Examples
   !!
   !! Arrays of string:
   !! @code{.f90}
   !! type(string) :: fruits(3)
   !!
   !! fruits = [ string('apple'), &
   !!            string('banana'), &
   !!            string('cherry') ]
   !!
   !! print *, fruits .contains. 'banana'      ! .true.
   !! print *, fruits .contains. 'orange'      ! .false.
   !! ...
   !! @endcode
   !!
   !! Mixed character/string usage:
   !! @code{.f90}
   !! character(10) :: names(2)
   !!
   !! names = ['foo       ', 'bar       ']
   !!
   !! print *, names .contains. string('foo')  ! .true.
   !! ...
   !! @endcode
   !!
   !! Empty arrays:
   !! @code{.f90}
   !! type(string) :: values(0)
   !!
   !! print *, values .contains. 'x'           ! .false.
   !! ...
   !! @endcode
   !!
   !! @return `.true.` if the searched value is present,
   !!         `.false.` otherwise.
   !!
   !! @ingroup group_string
   interface operator(.contains.)
      module procedure :: strings_contain_string
      module procedure :: strings_contain_character
      module procedure :: characters_contain_string
      module procedure :: characters_contain_character
   end interface

   !> Locate the position of a substring.
   !!
   !! This generic interface extends the intrinsic Fortran `index`
   !! function to support the fpx @ref string type.
   !!
   !! Supported combinations are:
   !! - `index(string, string)`
   !! - `index(string, character(*))`
   !! - `index(character(*), string)`
   !!
   !! The optional argument `back` behaves exactly as in the intrinsic
   !! Fortran procedure:
   !! - if absent or `.false.`, the first occurrence is returned;
   !! - if `.true.`, the last occurrence is returned.
   !!
   !! The function returns zero if the substring is not found.
   !!
   !! @section index_examples Examples
   !!
   !! String and character:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! s = 'banana'
   !!
   !! print *, index(s, 'na')               ! 3
   !! print *, index(s, 'xy')               ! 0
   !! ...
   !! @endcode
   !!
   !! String and string:
   !! @code{.f90}
   !! type(string) :: text
   !! type(string) :: sub
   !!
   !! text = 'banana'
   !! sub  = 'na'
   !!
   !! print *, index(text, sub)             ! 3
   !! ...
   !! @endcode
   !!
   !! Search from the end:
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! s = 'banana'
   !!
   !! print *, index(s, 'na', back=.true.)  ! 5
   !! ...
   !! @endcode
   !!
   !! Mixed usage:
   !! @code{.f90}
   !! type(string) :: sub
   !!
   !! sub = 'ana'
   !!
   !! print *, index('banana', sub)         ! 2
   !! ...
   !! @endcode
   !!
   !! @return Position of the matching substring, or zero if not found.
   !!
   !! @ingroup group_string
   interface index
      module procedure :: index_string_string
      module procedure :: index_string_character
      module procedure :: index_character_string
   end interface

contains

   !> Assignment overloading. Assign a character array to a string.
   !! @param[inout] lhs string
   !! @param[in]    rhs character(*)
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !!
   !! s = 'foo'
   !! @endcode
   !!
   !! @ingroup group_string
   subroutine character_assign_string(lhs, rhs)
      class(string), intent(inout)   :: lhs
      character(*), intent(in)       :: rhs

      if (allocated(lhs%chars)) deallocate(lhs%chars)
      allocate(lhs%chars, source=rhs)
   end subroutine

   !> Assignment overloading. Assign a string to a character array.
   !! @param[inout] lhs character(:), allocatable
   !! @param[in]    rhs string
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !! character(:), allocatable :: c
   !!
   !! s = 'foo'
   !! c = s
   !! ! The value of c is now 'foo'
   !! @endcode
   !!
   !! @ingroup group_string
   subroutine string_assign_character(lhs, rhs)
      character(:), allocatable, intent(inout) :: lhs
      class(string), intent(in)                :: rhs

      lhs = rhs%chars
   end subroutine

   !> Length of the string entity.
   !! @param[in] this string
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !! integer :: l
   !!
   !! s = string('foo ')
   !! l = len(s)
   !! ! The value of l is 4
   !! @endcode
   !! @return An integer corresponding to the length of the string.
   !!
   !! @ingroup group_string
   elemental integer function string_len(this) result(res)
      class(string), intent(in) :: this

      if (allocated(this%chars)) then
         res = len(this%chars)
      else
         res = 0
      end if
   end function

   !> Length of the string entity without trailing blanks (len_trim).
   !! @param[in] this string
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !! integer :: l
   !!
   !! s = string('foo ')
   !! l = len_trim(s)
   !! ! The value of l is 3
   !! @endcode
   !! @return An integer corresponding to the trimmed length of the string.
   !!
   !! @ingroup group_string
   pure integer function string_len_trim(this) result(res)
      class(string), intent(in) :: this

      if (allocated(this%chars)) then
         res = len_trim(this%chars)
      else
         res = 0
      end if
   end function

   !> Returns a copy of the string with trailing blanks removed.
   !! @param[in] this string
   !! @return Trimmed character string (deferred length).
   !!
   !! @ingroup group_string
   pure function string_trim(this) result(res)
      class(string), intent(in) :: this
      character(:), allocatable :: res

      if (allocated(this%chars)) then
         res = trim(this%chars)
      else
         res = ''
      end if
   end function

   !> Concatenation of two string objects.
   !! @param[in] lhs left-hand side string
   !! @param[in] rhs right-hand side string
   !! @return New concatenated string.
   !!
   !! @ingroup group_string
   pure function string_concat_string(lhs, rhs) result(res)
      class(string), intent(in) :: lhs
      class(string), intent(in) :: rhs
      character(:), allocatable :: res

      if (allocated(lhs%chars) .and. allocated(rhs%chars)) then
         res = lhs%chars // rhs%chars
      elseif (allocated(lhs%chars)) then
         res = lhs%chars
      elseif (allocated(rhs%chars)) then
         res = rhs%chars
      else
         res = ''
      end if
   end function

   !> Concatenation of string and character expression.
   !! @param[in] lhs string
   !! @param[in] rhs character expression
   !! @return New concatenated string.
   !!
   !! @ingroup group_string
   pure function string_concat_character(lhs, rhs) result(res)
      class(string), intent(in)   :: lhs
      character(*), intent(in)    :: rhs
      character(:), allocatable   :: res

      if (allocated(lhs%chars)) then
         res = lhs%chars // rhs
      else
         res = rhs
      end if
   end function

   !> Concatenation of character expression and string.
   !! @param[in] lhs character expression
   !! @param[in] rhs string
   !! @return New concatenated string.
   !!
   !! @ingroup group_string
   pure function character_concat_string(lhs, rhs) result(res)
      character(*), intent(in)    :: lhs
      class(string), intent(in)   :: rhs
      character(:), allocatable   :: res

      if (allocated(rhs%chars)) then
         res = lhs // rhs%chars
      else
         res = lhs
      end if
   end function

   !> Equality comparison between two string objects.
   !! @param[in] lhs left-hand side
   !! @param[in] rhs right-hand side
   !! @return .true. if the strings are equal, .false. otherwise.
   !!
   !! @ingroup group_string
   elemental function string_eq_string(lhs, rhs) result(res)
      class(string), intent(in) :: lhs  !! Left hand side.
      class(string), intent(in) :: rhs  !! Right hand side.
      logical :: res

      if (.not. allocated(lhs%chars)) then
         res = .not. allocated(rhs%chars)
      else
         res = lhs%chars == rhs%chars
      end if
   end function

   !> Equality comparison between string and character expression.
   !! @param[in] lhs string
   !! @param[in] rhs character expression
   !! @return .true. if equal, .false. otherwise.
   !!
   !! @ingroup group_string
   elemental function string_eq_character(lhs, rhs) result(res)
      class(string), intent(in) :: lhs  !! Left hand side.
      character(*), intent(in) :: rhs  !! Right hand side.
      logical                               :: res  !! Opreator test result.

      if (.not. allocated(lhs%chars)) then
         res = .false.
      else
         res = lhs%chars == rhs
      end if
   end function

   !> Equality comparison (reversed) between character expression and string.
   !! @param[in] lhs character expression
   !! @param[in] rhs string
   !! @return .true. if equal, .false. otherwise.
   !!
   !! @ingroup group_string
   elemental function character_eq_string(lhs, rhs) result(res)
      character(*), intent(in) :: lhs  !! Left hand side.
      class(string), intent(in) :: rhs  !! Right hand side.
      logical                               :: res  !! Operator test result.

      if (.not. allocated(rhs%chars)) then
         res = .false.
      else
         res = rhs%chars == lhs
      end if
   end function

   !> Formatted output procedure for user-defined type @ref string (UDTIO)
   !! This procedure is called automatically when a formatted WRITE statement is used
   !! with a variable of type `string` (when using the DT edit descriptor or default
   !! formatted output for the type).
   !!
   !! It writes the content of the string component `dtv%chars` using a simple `A` format.
   !! If the string is not allocated, an empty string is written.
   !!
   !! @param[in] dtv       The @ref string object to be written (polymorphic dummy argument)
   !! @param[in] unit      Fortran logical unit number
   !! @param[in] iotype    String describing the edit descriptor ('DT' + optional string)
   !! @param[in] v_list    Integer array containing the values from the DT edit descriptor
   !!                      (v_list is empty if no parentheses were used after DT)
   !! @param[out] iostat   I/O status code (0 = success, positive = error, negative = end-of-file/end-of-record)
   !! @param[inout] iomsg  Message describing the I/O error (if any)
   !!
   !! @b Note
   !! - This implementation **ignores** `iotype` and `v_list` parameters
   !!   -> the same simple character output is always performed
   !! - The procedure always uses format `(A)`
   !! - Empty (not allocated) string is written as empty line (zero characters)
   !!
   !! @b Warning
   !! This is a minimal implementation of UDTIO formatted output.
   !! More sophisticated versions could:
   !! - respect `iotype` (DT"..." or LISTDIRECTED)
   !! - use `v_list` for width/precision control
   !! - add quotation marks, escaping, etc.
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(string) :: s
   !! call s%set("Hello formatted world!")
   !!
   !! write(*, *)     s               ! may call write_formatted (depending on compiler)
   !! write(*, '(DT)') s              ! explicitly calls write_formatted
   !! @endcode
   !!
   !! @ingroup group_string
   ! allow(assumed-size-character-intent)
   subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
      class(string), intent(in)   :: dtv
      integer, intent(in)         :: unit  !! Logical unit.
      character(*), intent(in)    :: iotype  !! Edit descriptor.
      integer, intent(in)         :: v_list(:)  !! Edit descriptor list.
      integer, intent(out)        :: iostat  !! IO status code.
      character(*), intent(inout) :: iomsg  !! IO status message.

      if (allocated(dtv%chars)) then
         write(unit, '(A)', iostat=iostat, iomsg=iomsg) dtv%chars
      else
         write(unit, '(A)', iostat=iostat, iomsg=iomsg) ''
      end if
   end subroutine

   !> Checks if a string starts with a given prefix
   !! Returns `.true.` if the string `str` (after trimming leading/trailing whitespace)
   !! begins exactly with the substring `arg1`.
   !! The function uses `index()` after trimming both strings with `trim(adjustl())`.
   !!
   !! @param[in] str    The string to be tested
   !! @param[in] arg1   The prefix to look for at the beginning of `str`
   !! @param[out] idx  (optional) If present, receives the starting position of `arg1` in the trimmed string
   !!                       (will be 1 if the function returns `.true.`, otherwise >1 or 0)
   !!
   !! @return `.true.` if `str` starts with `arg1` (after trimming), `.false.` otherwise
   !!
   !! @b Note
   !! - Leading and trailing whitespace of both `str` and `arg1` is ignored
   !! - Comparison is case-sensitive
   !! - Empty `arg1` will always return `.true.` (any string starts with empty string)
   !!
   !! @b Warning
   !! The returned index (when requested) is the position **after trimming** of the input string,
   !! not in the original untrimmed string.
   !!
   !! @b Examples
   !! @code{.f90}
   !! character(80) :: line = '   hello world  '
   !! logical :: ok
   !! integer :: pos
   !!
   !! ok = starts_with(line, 'hello')               ! -> .true.
   !! ok = starts_with(line, 'hello', pos)          ! -> .true. and pos = 1
   !! ok = starts_with(line, 'world')               ! -> .false.
   !! ok = starts_with('  test123  ', 'test')       ! -> .true.
   !! ...
   !! @endcode
   !!
   !! @ingroup group_string
   !! @ingroup group_string
   logical function starts_with(str, arg1, idx) result(res)
      character(*), intent(in) :: str
      character(*), intent(in) :: arg1
      integer, intent(out), optional :: idx
      !private
      integer :: i

      i = index(trim(adjustl(str)), trim(arg1))
      res = (i == 1)
      if (present(idx)) idx = i
   end function

   !> Returns the first character of the trimmed string.
   !! @param[in] str input string
   !! @return First character (space if empty)
   !!
   !! @ingroup group_string
   character function head(str) result(res)
      character(*), intent(in) :: str

      res = ' '
      if (len_trim(str) == 0) return

      res = str(1:1)
   end function

   !> Returns the last non-blank character of a string.
   !! @param[in] str input string
   !! @return Last character (space if empty)
   !!
   !! @ingroup group_string
   character function tail(str) result(res)
      character(*), intent(in) :: str
      !private
      integer :: n

      res = ' '; n = len_trim(str)
      if (n == 0) return

      res = str(n:n)
   end function

   !> Smart concatenation that removes continuation markers (&) and handles line-continuation rules.
   !! @param[in] str1 first line
   !! @param[in] str2 second line
   !! @return Concatenated string with proper continuation handling
   !!
   !! @ingroup group_string
   function concat(str1, str2) result(res)
      character(*), intent(in) :: str1
      character(*), intent(in) :: str2
      character(:), allocatable :: res
      !private
      integer :: n1, n2

      n1 = len(str1); n2 = 1
      if (head(str1) == '!') then
         n2 = 2
         if (tail(str1) == '&') n1 = len_trim(str1) - 1
         if (starts_with(str2, '!dir$') .or. starts_with(str2, '!DIR$') .or. &
            starts_with(str2, '!dec$') .or. starts_with(str2, '!DEC$') .or. &
            starts_with(str2, '!gcc$') .or. starts_with(str2, '!GCC$') .or. &
            starts_with(str2, '!acc$') .or. starts_with(str2, '!ACC$') .or. &
            starts_with(str2, '!$omp') .or. starts_with(str2, '!$OMP')) then
            n2 = 6
         end if
         if (head(adjustl(str2(n2:))) == '&') then
            n2 = index(str2, '&') + 1
         end if
      else
         if (tail(str1) == '&') n1 = len_trim(str1) - 1
         if (head(trim(str2)) == '&') n2 = index(str2, '&') + 1
         if (tail(str1(:n1)) == '(') n1 = index(str1(:n1), '(', back=.true.)
      end if

      if (len(str1) > 0 .and. len(str2) >= n2) then
         if (str1(n1:n1) == ' ' .and. str2(n2:n2) == ' ') n2 = n2 + 1
      end if
      res = str1(:n1) // str2(n2:)
   end function

   !> Convert string to upper case (respects contents of quotes).
   !! @param[in] str input string
   !! @return Upper-case version of the string
   !!
   !! @b Examples
   !! @code
   !! character(*), parameter :: input = 'test'
   !! character(:), allocatable :: output
   !! output = uppercase(input)
   !! if (output == 'TEST') print*, 'OK'
   !! @endcode
   !!
   !! @ingroup group_string
   pure function uppercase(str) result(res)
      character(*), intent(in) :: str
      character(len_trim(str)) :: res
      !private
      integer :: ilen, ioffset, iquote, iqc, iav, i

      ilen = len_trim(str)
      ioffset = iachar('A') - iachar('a')
      iquote = 0
      res = str
      do i = 1, ilen
         iav = iachar(str(i:i))
         if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
            iquote = 1
            iqc = iav
            cycle
         end if
         if (iquote == 1 .and. iav == iqc) then
            iquote = 0
            cycle
         end if
         if (iquote == 1) cycle
         if (iav >= iachar('a') .and. iav <= iachar('z')) then
            res(i:i) = achar(iav + ioffset)
         else
            res(i:i) = str(i:i)
         end if
      end do
   end function

   !> Convert string to lower case (respects contents of quotes).
   !! @param[in] str input string
   !! @return Lower-case version of the string
   !!
   !! @b Examples
   !! @code
   !! character(*), parameter :: input = 'TEST'
   !! character(:), allocatable :: output
   !! output = lowercase(input)
   !! if (output == 'test') print*, 'OK'
   !! @endcode
   !!
   !! @ingroup group_string
   pure function lowercase(str) result(res)
      character(*), intent(in) :: str
      character(len_trim(str)) :: res
      !private
      integer :: ilen, ioffset, iquote, iqc, iav, i

      ilen = len_trim(str)
      ioffset = iachar('A') - iachar('a')
      iquote = 0
      res = str
      do i = 1, ilen
         iav = iachar(str(i:i))
         if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
            iquote = 1
            iqc = iav
            cycle
         end if
         if (iquote == 1 .and. iav == iqc) then
            iquote = 0
            cycle
         end if
         if (iquote == 1) cycle
         if (iav >= iachar('A') .and. iav <= iachar('Z')) then
            res(i:i) = achar(iav - ioffset)
         else
            res(i:i) = str(i:i)
         end if
      end do
   end function

   !> Write a long line split into chunks of size CHKSIZE with continuation (&).
   !! @param[in] unit logical unit
   !! @param[in] str  string to write
   !!
   !! @ingroup group_string
   subroutine writechk(unit, str)
      integer, intent(in)         :: unit
      character(*), intent(in)    :: str
      !private
      integer :: i, n

      n = 0
      if (head(str) /= '!') then
         n = floor(len(str) / real(CHKSIZE))
         do i = 1, n
            write(unit, '(A)') str((i - 1) * CHKSIZE + 1:i * CHKSIZE) // '&'
         end do
      end if
      write(unit, '(A)') str(n * CHKSIZE + 1:)
   end subroutine

   !> Returns the previous non-blank character before position pos (updates pos).
   !! @param[in]    line input line
   !! @param[inout] pos  current position (moved backward)
   !! @return Previous non-blank character
   !!
   !! @ingroup group_string
   character(1) function previous(line, pos) result(res)
      character(*), intent(in)    :: line
      integer, intent(inout)      :: pos
      !private

      if (pos == 1) then
         res = trim(line(pos:pos))
      else
         do while (line(pos:pos) == ' ')
            pos = pos - 1
            if (pos == 1) exit
         end do
         res = line(pos:pos)
      end if
   end function

   !> Checks whether an array of string contains a given string.
   !! @param[in] lhs array of string
   !! @param[in] rhs string to search for
   !! @return .true. if rhs is present in lhs
   !!
   !! @ingroup group_string
   logical function strings_contain_string(lhs, rhs) result(res)
      type(string), intent(in)    :: lhs(:)
      type(string), intent(in)    :: rhs
      !private
      integer :: i

      res = .false.
      do i = 1, size(lhs)
         if (lhs(i) == rhs) then
            res = .true.
            exit
         end if
      end do
   end function

   !> Checks whether an array of string contains a given character expression.
   !! @param[in] lhs array of string
   !! @param[in] rhs character expression to search for
   !! @return .true. if rhs is present in lhs
   !!
   !! @ingroup group_string
   logical function strings_contain_character(lhs, rhs) result(res)
      type(string), intent(in)    :: lhs(:)
      character(*), intent(in)    :: rhs
      !private
      integer :: i

      res = .false.
      do i = 1, size(lhs)
         if (lhs(i) == rhs) then
            res = .true.
            exit
         end if
      end do
   end function

   !> Checks whether an array of character contains a given character expression.
   !! @param[in] lhs array of character
   !! @param[in] rhs character expression to search for
   !! @return .true. if rhs is present in lhs
   !!
   !! @ingroup group_string
   logical function characters_contain_character(lhs, rhs) result(res)
      character(*), intent(in)    :: lhs(:)
      character(*), intent(in)    :: rhs
      !private
      integer :: i

      res = .false.
      do i = 1, size(lhs)
         if (lhs(i) == rhs) then
            res = .true.
            exit
         end if
      end do
   end function

   !> Checks whether an array of character contains a given string.
   !! @param[in] lhs array of character
   !! @param[in] rhs string to search for
   !! @return .true. if rhs is present in lhs
   !!
   !! @ingroup group_string
   logical function characters_contain_string(lhs, rhs) result(res)
      character(*), intent(in)    :: lhs(:)
      type(string), intent(in)    :: rhs
      !private
      integer :: i

      res = .false.
      do i = 1, size(lhs)
         if (lhs(i) == rhs) then
            res = .true.
            exit
         end if
      end do
   end function

   integer function index_string_string(str, substr, back) result(res)
      class(string), intent(in)           :: str
      class(string), intent(in)           :: substr
      logical, intent(in), optional       :: back

      res = index(str%chars, substr%chars, back=back)
   end function

   integer function index_character_string(str, substr, back) result(res)
      character(*), intent(in)            :: str
      class(string), intent(in)           :: substr
      logical, intent(in), optional       :: back

      res = index(str, substr%chars, back=back)
   end function

   integer function index_string_character(str, substr, back) result(res)
      class(string), intent(in)           :: str
      character(*), intent(in)            :: substr
      logical, intent(in), optional       :: back

      res = index(str%chars, substr, back=back)
   end function

end module

!>>>>> ././src/path.f90
!> @file
!! @defgroup group_path Path
!! A modern, portable Fortran module for path manipulation and basic directory operations.
!! This module provides a clean interface for working with file system paths
!! in a platform-independent way. It correctly handles both Unix ('/') and Windows ('\') path
!! separators through conditional compilation and offers deferred-length character results
!! for maximum flexibility.
!!
!! The module builds upon the @link fpx_string fpx_string @endlink module for @link fpx_string::string string @endlink type support
!! and provides
!! overloads of key procedures to accept either intrinsic `character(*)` or `type(string)`
!! arguments.
!!
!! Features include:
!! - Detection of absolute and rooted paths
!! - Cross-platform path joining
!! - Extraction of directory and filename components
!! - Splitting paths into head/tail elements
!! - Retrieval and modification of the current working directory
!! - Support for both intrinsic CHARACTER and type(string) arguments
!! - Changing the current working directory (`chdir`)
!!
!! @note All path-returning functions return allocatable deferred-length characters.
!! @note The public generic `join` interface works with any combination of `character` and `string`.
!!
!! @section path_examples Examples
!! @code{.f90}
!! character(:), allocatable :: p1, p2, full
!!
!! p1 = '/home/user/docs'
!! p2 = 'report.pdf'
!! full = join(p1, p2)                ! => '/home/user/docs/report.pdf'
!!
!! print *, is_absolute(full)         ! .true.  (on Unix)
!! print *, filename(full)            ! 'report'
!! print *, filename(full,.true.)     ! 'report.pdf'
!! print *, dirpath(full)             ! '/home/user/docs'
!! ...
!! @endcode
!!
!! On Windows:
!! @code{.f90}
!! character(:), allocatable :: p
!! p = join('C:\Users', 'Alice', 'Documents')
!! ! p == 'C:\Users\Alice\Documents'
!! print *, is_absolute(p)   ! .true.
!! ...
!! @endcode
module fpx_path
   use, intrinsic :: iso_c_binding
   use fpx_string

   public :: join, &
      is_absolute, &
      is_rooted, &
      filename, &
      dirpath, &
      dirname, &
      split_path, &
      cwd, &
      chdir

   !! @cond
#ifdef _WIN32
   !> Platform-dependent directory separator.
   !!
   !! '/' on Unix-like systems,
   !! '\' on Windows.
   !!
   !! @ingroup group_path
   character, parameter    :: separator = '\'
   character(*), parameter :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
#else
   !> Platform-dependent directory separator.
   !!
   !! '/' on Unix-like systems,
   !! '\' on Windows.
   !!
   !! @ingroup group_path
   character, parameter :: separator = '/'
#endif

#ifdef _WIN32
   interface
      function getcwd_c(buf, size) bind(C, name='_getcwd') result(r)
         import
         implicit none
         type(c_ptr) :: r
         ! allow(assumed-size, assumed-size-character-intent)
         character(kind=c_char), intent(out) :: buf(*)
         integer(kind=c_size_t), value       :: size
      end function
   end interface
#else
   interface
      function getcwd_c(buf, size) bind(C, name='getcwd') result(r)
         import
         implicit none
         type(c_ptr) :: r
         ! allow(assumed-size, assumed-size-character-intent)
         character(kind=c_char), intent(out) :: buf(*)
         integer(kind=c_size_t), value       :: size
      end function
   end interface
#endif

   interface
      integer(c_int) function chdir_c(path) bind(C, name='chdir')
         import
         implicit none
         ! allow(assumed-size)
         character(kind=c_char), intent(in) :: path(*)
      end function
   end interface
   !! @endcond

   !> Join path components using the platform separator.
   !!
   !! The generic interface accepts any combination of intrinsic
   !! `character(*)` and @link fpx_string::string string @endlink
   !! arguments.
   !!
   !! Supported overloads:
   !! - join(character, character)
   !! - join(character, string)
   !! - join(string, character)
   !! - join(string, string)
   !!
   !! @b Examples
   !! @code{.f90}
   !! character(:), allocatable :: p
   !!
   !! p = join('/usr','bin')
   !! ! '/usr/bin'
   !!
   !! p = join(string('/usr'),'local')
   !! ! '/usr/local'
   !! ...
   !! @endcode
   !! @ingroup group_path
   interface join
      module procedure :: join_character_character
      module procedure :: join_string_character
      module procedure :: join_character_string
      module procedure :: join_string_string
   end interface

contains

   !> Returns .true. if the path is absolute.
   !! On Unix a path is absolute when it starts with '/'.
   !! On Windows a path is absolute when it starts with a drive letter followed by ':\'
   !! (e.g. 'C:\', 'd:/temp').
   !!
   !! @param[in] filepath  Path to test
   !! @return res          .true. if filepath is absolute
   !!
   !! @code{.f90}
   !!    print *, is_absolute('/home/user')   ! .true.  (Unix)
   !!    print *, is_absolute('C:\\Temp')     ! .true.  (Windows)
   !!    print *, is_absolute('docs/..')      ! .false.
   !!    ...
   !! @endcode
   !!
   !! @ingroup group_path
   pure logical function is_absolute(filepath) result(res)
      character(*), intent(in)        :: filepath
#ifdef _WIN32
      if (len(filepath) < 2) then
         res = .false.
         return
      end if
      res = scan(filepath(1:1), alphabet) /= 0 .and. filepath(2:2) == ':'
#else
      res = filepath(1:1) == separator
#endif

   end function

   !> Returns .true. if the path is rooted (starts with a separator) or is absolute.
   !! A rooted path begins with the platform separator ('\' on Windows, '/' elsewhere)
   !! even if it is not a full absolute path (e.g. '/temp' on Linux).
   !!
   !! @param[in] filepath  Path to test
   !! @return res          .true. if filepath is rooted
   !!
   !! @ingroup group_path
   pure logical function is_rooted(filepath) result(res)
      character(*), intent(in)        :: filepath
      !private
      integer :: length

      length = len(filepath)
#ifdef _WIN32
      res = (length >= 1 .and. filepath(1:1) == separator) .or. is_absolute(filepath)
#else
      res = (length > 0 .and. filepath(1:1) == separator)
#endif
   end function

   !! Returns the filename component of a path.
   !!
   !! The directory portion is discarded. By default, the final
   !! extension is removed; when `keepext=.true.` the full filename
   !! is returned unchanged.
   !! By default the extension is stripped. If keepext=.true. the full filename
   !! including extension is returned.
   !!
   !! @param[in] filepath   Full or relative path
   !! @param[in] keepext    Optional; keep extension when .true.
   !! @return res           Filename (without path)
   !!
   !! @code{f.90}
   !!    print *, filename('dir/file.txt')        ! 'file'
   !!    print *, filename('dir/file.txt',.true.) ! 'file.txt'
   !!    print *, filename('archive.tar.gz')      ! 'archive.tar'
   !! @endcode
   !!
   !! @ingroup group_path
   pure function filename(filepath, keepext) result(res)
      character(*), intent(in)        :: filepath
      character(:), allocatable       :: res
      logical, intent(in), optional   :: keepext
      !private
      integer :: ipoint, islash

      ipoint = index(filepath, '.', back=.true.)
      islash = index(filepath, separator, back=.true.)
      if (ipoint < islash) ipoint = len_trim(filepath) + 1
      if (present(keepext)) then
         if (keepext) then
            res = filepath(islash + 1:len_trim(filepath))
         else
            res = filepath(islash + 1: ipoint - 1)
         end if
      else
         res = filepath(islash + 1: ipoint - 1)
      end if
   end function

   !> Implementation of @ref join for character arguments.
   !!
   !! @copydetails join
   !!
   !! @ingroup group_path
   pure function join_character_character(path1, path2) result(res)
      character(*), intent(in) :: path1
      character(*), intent(in) :: path2
      character(:), allocatable :: res
      !private
      character(:), allocatable :: temp

      temp = trim(adjustl(path1))
      if (temp(len(temp):len(temp)) == separator) temp = trim(temp(:len(temp) - 1))

      res = temp // separator // trim(adjustl(path2))
   end function

   !> Implementation of @ref join for character arguments.
   !!
   !! @copydetails join
   !!
   !! @ingroup group_path
   pure function join_character_string(path1, path2) result(res)
      character(*), intent(in) :: path1
      type(string), intent(in) :: path2
      character(:), allocatable :: res
      !private
      character(:), allocatable :: temp

      temp = trim(adjustl(path1))
      if (temp(len(temp):len(temp)) == separator) temp = trim(temp(:len(temp) - 1))

      res = temp // separator // trim(adjustl(path2%chars))
   end function

   !> Implementation of @ref join for character arguments.
   !!
   !! @copydetails join
   !!
   !! @ingroup group_path
   pure function join_string_character(path1, path2) result(res)
      type(string), intent(in) :: path1
      character(*), intent(in) :: path2
      character(:), allocatable :: res
      !private
      character(:), allocatable :: temp

      temp = trim(adjustl(path1%chars))
      if (temp(len(temp):len(temp)) == separator) temp = trim(temp(:len(temp) - 1))

      res = temp // separator // trim(adjustl(path2))
   end function

   !> Implementation of @ref join for character arguments.
   !!
   !! @copydetails join
   !!
   !! @ingroup group_path
   pure function join_string_string(path1, path2) result(res)
      type(string), intent(in) :: path1
      type(string), intent(in) :: path2
      character(:), allocatable :: res
      !private
      character(:), allocatable :: temp

      temp = trim(adjustl(path1%chars))
      if (temp(len(temp):len(temp)) == separator) temp = trim(temp(:len(temp) - 1))

      res = temp // separator // trim(adjustl(path2%chars))
   end function

   !! Returns the directory component of a filesystem path.
   !!
   !! This is equivalent to the "head" returned by split_path().
   !! @param[in] filepath  Path to analyse
   !! @return res          Directory component
   !!
   !! @code {.f90}
   !!    print *, dirpath('/home/user/file.txt')  ! '/home/user'
   !! @endcode
   !!
   !! @ingroup group_path
   pure function dirpath(filepath) result(res)
      character(*), intent(in) :: filepath
      character(:), allocatable :: res
      !private
      character(:), allocatable :: temp

      call split_path(filepath, res, temp)
   end function

   !! Returns the basename component of a filesystem path.
   !!
   !! This is equivalent to the "tail" returned by split_path().
   !! @param[in] filepath  Path to analyse
   !! @return res          Base name component
   !!
   !! @code{.f90}
   !!    print *, dirname('/home/user/file.txt')  ! 'file.txt'
   !! @endcode
   !!
   !! @ingroup group_path
   pure function dirname(filepath) result(res)
      character(*), intent(in) :: filepath
      character(:), allocatable :: res
      !private
      character(:), allocatable :: temp

      call split_path(filepath, temp, res)
   end function

   !> Splits a path into head (directory) and tail (basename) components.
   !! Special cases:
   !! - Empty paths return ('.','')
   !! - Root directories return ('/','')
   !! - Trailing separators are ignored
   !! @param[in]  filepath  Input path
   !! @param[out] head      Directory part (includes trailing separator when appropriate)
   !! @param[out] tail      Base name part
   !!
   !! @ingroup group_path
   pure subroutine split_path(filepath, head, tail)
      character(*), intent(in)                :: filepath
      character(:), allocatable, intent(out)  :: head
      character(:), allocatable, intent(out)  :: tail
      !private
      character(:), allocatable :: temp
      integer :: i, ipoint, isep

      ! Empty string, return (.,'')
      if (len_trim(filepath) == 0) then
         head = '.'; tail = ''
         return
      end if

      ! Remove trailing path separators
      temp = trim(adjustl(filepath))
      if (temp(len(temp):len(temp)) == separator) then
         temp = trim(temp(:len(temp) - 1))
      else
         ipoint = index(filepath, '.', back=.true.)
         isep = index(filepath, separator, back=.true.)
         if (ipoint > isep .and. isep > 0) then
            temp = trim(temp(:isep - 1))
         end if
      end if

      if (len_trim(temp) == 0) then
         head = separator
         tail = ''
         return
      end if

      i = len(temp) - index(temp, separator, back=.true.) + 1

      ! if no `pathsep`, then it probably was a root dir like `C:\`
      if (i == 0) then
         head = temp // separator
         tail = ''
         return
      end if

      head = temp(:len(temp) - i)

      ! child of a root directory
      if (index(temp, separator, back=.true.) == 0) then
         head = head // separator
      end if

      tail = temp(len(temp) - i + 2:)
   end subroutine

   !> Returns the current working directory as a deferred-length character string.
   !! Returns an empty string if the current directory cannot be determined.
   !!
   !! @return res  Current working directory
   !! @code{.f90}
   !!    character(:), allocatable :: here
   !!    here = cwd()
   !!    print *, 'We are in: ', here
   !! @endcode
   !!
   !! @ingroup group_path
#ifdef __GFORTRAN__
   function cwd() result(res)
      character(:), allocatable :: res
      integer :: status
      call getcwd(res,status)
      if(status.ne.0)res=''
   end function
#else
   function cwd() result(res)
      character(:), allocatable :: res
      !private
      character(len=1, kind=c_char) :: buf(256)
      integer :: i, n
      integer(c_size_t) :: s

      s = size(buf, kind=c_size_t)
      if (c_associated(getcwd_c(buf, s))) then
         n = findloc(buf, achar(0), 1)
         allocate(character(n - 1) :: res)
         do i = 1, n - 1
            res(i:i) = buf(i)
         end do
      else
         res = ''
      end if
   end function
#endif

   !> Changes the current working directory.
   !! This is a thin wrapper around the underlying C runtime
   !! `chdir()` implementation.
   !! @param[in]  path  Directory to change to
   !! @param[out] err   Optional integer error code (0 = success, non-zero = failure)
   !! @code{.f90}
   !!    integer :: ierr
   !!    call chdir('/tmp', ierr)
   !!    if (ierr /= 0) stop 'Failed to change directory'
   !! @endcode
   !!
   !! @ingroup group_path
   subroutine chdir(path, err)
      character(*), intent(in)        :: path
      integer, optional, intent(out)  :: err
      integer :: loc_err

      loc_err = chdir_c(path // c_null_char)

      if (present(err)) err = loc_err
   end subroutine
end module

!>>>>> ././src/token.f90
!> @file
!! @defgroup group_token Token
!! @brief Token classification and representation for expression parsing in fpx
!!
!! This module provides the lightweight but robust token infrastructure used by the
!! fpx preprocessor when evaluating constant expressions in `#if` / `#elif` directives.
!!
!! It defines:
!! - A clean enumeration of token kinds (`tokens_enum`)
!! - A simple `token` derived type that carries both the lexical value and its semantic category
!!
!! These types are used internally by `evaluate_expression()` (from `fpx_token`) to parse
!! and compute `#if DEBUG > 1 && defined(USE_MPI)`-style conditions.
!!
!! @par Key design goals
!! - Minimal memory footprint
!! - Clear separation between lexical scanning and semantic interpretation
!! - Easy extensibility for future operators or functions
!!
!! @section token_examples Examples
!!
!! 1. Manual token creation (mostly for testing/debugging):
!! @code{.f90}
!!    use fpx_token
!!
!!    type(token) :: t1, t2, t3
!!
!!    t1 = token('42',   number)         ! numeric literal
!!    t2 = token('DEBUG', identifier)    ! macro name
!!    t3 = token('>',    operator)       ! comparison operator
!!
!!    print *, 'Token: ', t1%value, ' type=', t1%type   ! -> 42 type=0
!! @endcode
!!
!! 2. Typical internal usage during `#if` evaluation:
!! @code{.f90}
!!    ! (inside evaluate_expression)
!!    tokens = tokenize('defined(USE_MPI) && MPI_VERSION >= 3')
!!    ! tokens(1) -> value=vdefined'  type=identifier
!!    ! tokens(2) -> value='('        type=parenthesis
!!    ! tokens(3) -> value='USE_MPI'  type=identifier
!!    ! ...
!! @endcode
!!
!! @par Token kinds overview
!! | Enumerator   | Value | Meaning                                      |
!! |--------------|-------|----------------------------------------------|
!! | `unknown`    | -1    | Invalid / unrecognized token                 |
!! | `number`     |  0    | Integer or floating-point literal            |
!! | `operator`   |  1    | ?:, +, -, *, /, ==, !=, &&, ||, !, >, <, etc.|
!! | `identifier` |  2    | Macro name or function name (e.g. `defined`) |
!! | `parenthesis`|  3    | `(` or `)`                                   |
!! | `defined`    |  4    | Special keyword `defined` (treated specially)|
!!
module fpx_token
   use fpx_constants, only: MAX_TOKENS
   use fpx_string
   use fpx_logging

   implicit none; private

   public :: tokenize, &
      strtol, &
      tokens_enum, &
      unknown, &
      number, &
      operation, &
      identifier, &
      parenthesis, &
      defined

   !> @brief Token kinds used in expression parsing.
   !! Enumeration defining the possible types of tokens recognized by the tokenizer.
   !! @ingroup group_token
   enum, bind(c)
      enumerator :: unknown = -1
      enumerator :: number = 0
      enumerator :: operation = 1
      enumerator :: identifier = 2
      enumerator :: parenthesis = 3
      enumerator :: defined = 4
   end enum

   !> @brief Kind parameter for token type enumeration. Values are (`unknown`, `number`, `operation`, `identifier`, `parenthesis`,
   !! `defined`)
   !! @ingroup group_token
   integer, parameter :: tokens_enum = kind(unknown)

   !> Represents a single token in a parsed expression.
   !! Holds the string value of the token and its classified type.
   !! <h2 class="groupheader">Constructors</h2>
   !! Initializes a new instance of the @link fpx_token::token token @endlink class
   !! <h3>token(character(:), integer)</h3>
   !! @verbatim type(token) function token(character(:) value, integer type) @endverbatim
   !!
   !! @param[in] value
   !! @param[in] type
   !!
   !! @b Examples
   !! @code{.f90}
   !! a = token('9', number)
   !! @endcode
   !!
   !! @ingroup group_token
   type, public :: token
      character(:), allocatable   :: value  !< Token value
      integer(tokens_enum)        :: type  !< Token type, from the enum @ref tokens_enum.
      integer                     :: start
   end type

   !> Converts a string to integer.
   !! <h2 class="groupheader">Methods</h2>
   !!
   !! @code{.f90}strtol(character(*) str, (optional) logical success)@endcode
   !!
   !! @param[in]  str      String to convert
   !! @param[out] success  Optional flag indicating successful conversion
   !! @return Converted integer value
   !!
   !! @code{.f90}strtol(character(*) str, integer base, (optional) logical success)@endcode
   !!
   !! Converts a string to integer with explicit base handling.
   !! Supports base 2, 8, 10, 16 and prefixes `0x`, `0b`.
   !! @param[in]    str      String to convert
   !! @param[inout] base     0 = auto-detect, otherwise forces given base
   !! @param[out]   success  Optional flag indicating successful conversion
   !! @return Converted integer value
   !!
   !! <h2 class="groupheader"> Examples </h2>
   !! The following demonstrate a call to the `strtol` interface.
   !! @code{.f90}
   !!  integer :: i
   !!  logical :: success
   !!
   !!  i = strtol('    123', 0, success = res)
   !!  ! i = 123
   !! @endcode
   !!
   !! @ingroup group_operators
   interface strtol
      !! @cond
      module procedure :: strtol_default
      module procedure :: strtol_with_base
      !! @endcond
   end interface

contains

   !> Tokenizes a preprocessor expression into an array of token structures.
   !! Handles whitespace, multi-character operators (`&&`, `||`, `==`, etc.),
   !! the `defined` operator (with or without parentheses), numbers in various bases,
   !! identifiers, and parentheses.
   !! @param[in]  expr     Expression string to tokenize
   !! @param[out] tokens   Allocated array receiving the tokens
   !! @param[out] ntokens  Number of tokens produced
   !!
   !! @ingroup group_token
   subroutine tokenize(expr, tokens, ntokens)
      character(*), intent(in)                :: expr
      type(token), allocatable, intent(out)   :: tokens(:)
      integer, intent(out)                    :: ntokens
      !private
      character(:), allocatable :: temp
      integer :: i, pos, len_expr
      logical :: in_word
      logical, save :: in_comment

      if (allocated(tokens)) deallocate(tokens)
      allocate(tokens(MAX_TOKENS))
      ntokens = 0
      temp = trim(adjustl(expr)) // ' '
      len_expr = len_trim(temp)
      i = 1
      in_word = .false.

      do while (i <= len_expr)
         if (temp(i:i) == ' ') then
            i = i + 1
            in_word = .false.
            cycle
         end if

         if (.not. in_word) then
            ntokens = ntokens + 1
            if (ntokens > MAX_TOKENS) then
               call printf(render(diagnostic_report(LEVEL_ERROR, &
                  message='The maximum number of tokens has been reached', &
                  label=label_type('Too many tokens in expression.', 1, 1)), &
                  expr))
               return
            end if
            in_word = .true.
         end if

         if (temp(i:i) == '(' .or. temp(i:i) == ')') then
            tokens(ntokens)%value = temp(i:i)
            tokens(ntokens)%type = parenthesis
            tokens(ntokens)%start = i
            i = i + 1
            in_word = .false.
         else if (temp(i:i + 1) == '&&' .or. temp(i:i + 1) == '||' .or. temp(i:i + 1) == '==' .or. &
            temp(i:i + 1) == '!=' .or. temp(i:i + 1) == '<=' .or. temp(i:i + 1) == '>=') then
            tokens(ntokens)%value = temp(i:i + 1)
            tokens(ntokens)%type = operation
            tokens(ntokens)%start = i
            i = i + 2
            in_word = .false.
         else if (temp(i:i) == '!') then
            tokens(ntokens)%value = temp(i:i)
            tokens(ntokens)%type = operation
            tokens(ntokens)%start = i
            i = i + 1
            in_word = .false.
         else if (temp(i:i + 1) == '**') then
            tokens(ntokens)%value = temp(i:i + 1)
            tokens(ntokens)%type = operation
            tokens(ntokens)%start = i
            i = i + 2
            in_word = .false.
         else if (temp(i:i + 1) == '<<' .or. temp(i:i + 1) == '>>') then
            tokens(ntokens)%value = temp(i:i + 1)
            tokens(ntokens)%type = operation
            tokens(ntokens)%start = i
            i = i + 2
            in_word = .false.
         else if (temp(i:i) == '<' .or. temp(i:i) == '>' .or. temp(i:i) == '=' .or. &
            temp(i:i) == '+' .or. temp(i:i) == '-' .or. temp(i:i) == '*' .or. &
            temp(i:i) == '/' .or. temp(i:i) == '%' .or. &
            temp(i:i) == '?' .or. temp(i:i) == ':') then
            tokens(ntokens)%value = temp(i:i)
            tokens(ntokens)%type = operation
            tokens(ntokens)%start = i
            i = i + 1
            in_word = .false.
         else if (temp(i:i) == '&' .or. temp(i:i) == '|' .or. temp(i:i) == '^' .or. &
            temp(i:i) == '~') then
            tokens(ntokens)%value = temp(i:i)
            tokens(ntokens)%type = operation
            tokens(ntokens)%start = i
            i = i + 1
            in_word = .false.
         else if (starts_with(temp(i:), 'defined')) then
            i = i + 7
            do while (i <= len_expr .and. temp(i:i) == ' ')
               i = i + 1
            end do
            if (i <= len_expr .and. temp(i:i) == '(') then
               i = i + 1
               pos = i
               do while (pos <= len_expr .and. temp(pos:pos) /= ')')
                  pos = pos + 1
               end do
               tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
               tokens(ntokens)%type = defined
               tokens(ntokens)%start = i
               i = pos + 1
            else
               pos = i
               do while (pos <= len_expr .and. temp(pos:pos) /= ' ')
                  pos = pos + 1
               end do
               tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
               tokens(ntokens)%type = defined
               tokens(ntokens)%start = i
               i = pos
            end if
            in_word = .false.
         else if (is_typeless(temp(i:), pos)) then
            pos = i + pos
            tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
            tokens(ntokens)%type = number
            tokens(ntokens)%start = i
            i = pos
            in_word = .false.
         else if (is_digit(temp(i:i))) then
            pos = i
            do while (pos <= len_expr .and. is_digit(temp(pos:pos)))
               pos = pos + 1
            end do
            tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
            tokens(ntokens)%type = number
            tokens(ntokens)%start = i
            i = pos
            in_word = .false.
         else
            pos = i
            do while (pos <= len_expr .and. temp(pos:pos) /= ' ' .and. &
               temp(pos:pos) /= '(' .and. temp(pos:pos) /= ')')
               pos = pos + 1
            end do
            tokens(ntokens)%value = trim(temp(i:pos - 1))
            tokens(ntokens)%type = identifier
            tokens(ntokens)%start = i
            i = pos
            in_word = .false.
         end if
      end do
   end subroutine

   !> Tests whether a single character is a decimal digit ('0'-'9').
   !! @param[in] ch Character to test
   !! @return .true. if ch is a digit
   !!
   !! @ingroup group_token
   logical elemental function is_digit(ch) result(res)
      character(*), intent(in) :: ch

      res = verify(ch, '0123456789') == 0
   end function

   !> Detects whether a string starts a typeless constant (hex, octal, binary).
   !! Used to avoid treating them as identifiers during tokenization.
   !! @param[in]  str Input string starting at current position
   !! @param[out] pos Length of the typeless constant (0 if not typeless)
   !! @return .true. if the prefix is a valid typeless constant in non-base-10
   !!
   !! @ingroup group_token
   logical function is_typeless(str, pos) result(res)
      character(*), intent(in)    :: str
      integer, intent(out)        :: pos
      !private
      integer :: i, base, n

      pos = 0; base = 0; n = len(str)
      do i = 1, n
         if (verify(str(i:i), '0123456789xXaAbBcCdDeEfF') /= 0) then
            pos = i
            exit
         end if
      end do
      if (pos > 0) i = strtol(str(:pos - 1), base, success=res)
      if (base == 10) res = .false.
   end function

   !> Implementation of strtol function
   integer function strtol_default(str, success) result(val)
      character(*), intent(in)        :: str
      logical, intent(out), optional  :: success
      !private
      integer :: base

      base = 0
      val = strtol_with_base(str, base, success)
   end function

   !> Implementation of strtol function with a base argument.
   integer function strtol_with_base(str, base, success) result(val)
      character(*), intent(in)        :: str
      integer, intent(inout)          :: base
      logical, intent(out), optional  :: success
      !private
      integer :: i, len, digit
      character :: c
      logical :: is_valid, isdigit, is_lower_hex, is_upper_hex
      character(len=len_trim(str)) :: work_str

      val = 0; is_valid = .true.
      work_str = adjustl(str)  ! Remove leading spaces
      len = len_trim(work_str)

      ! Handle base 0 (auto-detect)
      if (base == 0) then
         if (len >= 2) then
            if (work_str(1:2) == '0x' .or. work_str(1:2) == '0X') then
               base = 16
               work_str = work_str(3:len)
               len = len - 2
            else if (work_str(1:2) == '0b' .or. work_str(1:2) == '0B') then
               base = 2
               work_str = work_str(3:len)
               len = len - 2
            else
               if (len > 1) then
                  if (work_str(1:1) == '0') then
                     base = 8
                  else
                     base = 10
                  end if
               else
                  base = 10
               end if
            end if
         else
            base = 10
         end if
      end if

      ! Validate base
      if (base /= 2 .and. base /= 8 .and. base /= 10 .and. base /= 16) then
         is_valid = .false.
         if (present(success)) success = .false.
         return
      end if

      ! Process each character
      do i = 1, len
         c = work_str(i:i)
         digit = -1  ! Invalid digit marker

         ! Convert character to digit
         isdigit = c >= '0' .and. c <= '9'
         if (isdigit) digit = ichar(c) - ichar('0')

         is_lower_hex = base == 16 .and. c >= 'a' .and. c <= 'f'
         if (is_lower_hex) digit = ichar(c) - ichar('a') + 10

         is_upper_hex = base == 16 .and. c >= 'A' .and. c <= 'F'
         if (is_upper_hex) digit = ichar(c) - ichar('A') + 10

         ! Check if digit is valid
         if (digit == -1) then
            is_valid = .false.
            exit
         end if
         if (digit >= base) then
            is_valid = .false.
            exit
         end if

         ! Check for potential overflow (approximate for 32-bit integer)
         if (val > (huge(val) - digit) / base) then
            is_valid = .false.
            exit
         end if

         ! Accumulate value
         val = val * base + digit
      end do

      ! Set success flag if provided
      if (present(success)) success = is_valid
   end function
end module

!>>>>> ././src/line.f90
!> @file
!! @defgroup group_line Line
!! Standard-compliant handling of the `#line` directive.
!!
!! This module implements support for the ISO C preprocessor `#line`
!! directive, allowing the logical source location used by the preprocessor
!! to be modified during preprocessing.
!!
!! The directive affects the information stored in the active
!! @link fpx_context::context context @endlink object and therefore influences:
!!
!! - Diagnostic messages and source locations,
!! - Expansions of predefined macros such as `__LINE__`,
!! - Expansions of `__FILE__` and `__FILENAME__`,
!! - The apparent origin of generated or transformed source code.
!!
!! This functionality is particularly useful for:
!!
!! - Source-to-source translators,
!! - Code generators,
!! - Literate programming systems,
!! - Template engines,
!! - Tools that emit Fortran intended to preserve original source locations.
!!
!! The following standard forms are supported:
!!
!! - `#line <number>`
!! - `#line <number> "<filename>"`
!!
!! When encountered, the directive immediately updates the logical
!! source position used for all subsequent processing.
!!
!! @note
!! The specified line number refers to the line immediately following
!! the directive itself, matching the behaviour of the ISO C preprocessor.
!!
!! @note
!! Malformed directives generate warnings and are ignored.
!!
!! @section line_examples Examples
!!
!! 1. Reset the logical line number:
!! @code{.f90}
!!    #line 100
!!    print *, "Reported as line 100"
!! ...
!! @endcode
!!
!! 2. Change both line number and filename:
!! @code{.f90}
!!    #line 42 "generated.f90"
!!    integer :: x
!!
!!    ! Diagnostics now refer to generated.f90:42
!! ...
!! @endcode
!!
!! 3. Improve diagnostics in generated code:
!! @code{.f90}
!!    #line 215 "input_template.f90"
!!    call generated_procedure()
!! ...
!! @endcode
module fpx_line
   use fpx_path
   use fpx_logging
   use fpx_context

   implicit none; private

   public :: handle_line

contains

   !> Process a `#line` directive.
   !!
   !! Parses the directive arguments, validates the requested logical
   !! line number, and updates the active @ref context object.
   !!
   !! Supported forms are:
   !!
   !! @code{.txt}
   !! #line <number>
   !! #line <number> "<filename>"
   !! @endcode
   !!
   !! The specified line number becomes the number associated with the
   !! source line immediately following the directive.
   !!
   !! If a filename is supplied, subsequent diagnostics and predefined
   !! file-related macros use the new filename.
   !!
   !! Invalid directives produce warnings and leave the current context
   !! unchanged.
   !!
   !! @param[inout] ctx
   !!    Current source context. Its logical line number and optional
   !!    filename are updated in place.
   !! @param[in] token
   !!    Directive keyword used to identify the `#line` directive,
   !!    typically `"line"`.
   !!
   !! @ingroup group_line
   subroutine handle_line(ctx, token)
      type(context), intent(inout)    :: ctx
      character(*), intent(in)        :: token
      !private
      character(:), allocatable :: temp, num_str, fname
      integer :: pos, iostat, new_line, closing
      logical :: has_filename

      ! Skip #line keyword
      pos = index(lowercase(ctx%content), token) + len(token)
      temp = trim(adjustl(ctx%content(pos:)))

      if (len_trim(temp) == 0) then
         call printf(render(diagnostic_report(LEVEL_WARNING, &
            message='Syntax error', &
            label=label_type('#line directive with no arguments', index(token, lowercase(ctx%content)) + len(token) + 1, 1)&
            , &
            source=ctx%path), &
            trim(ctx%content), ctx%line))
         return
      end if

      ! Extract line number
      pos = index(temp, ' ')
      if (pos > 0) then
         num_str = temp(:pos - 1)
         fname = trim(adjustl(temp(pos:)))
         has_filename = .true.
      else
         num_str = trim(temp)
         has_filename = .false.
      end if

      ! Parse line number
      read(num_str, *, iostat=iostat) new_line
      if (iostat /= 0 .or. new_line < 1) then
         call printf(render(diagnostic_report(LEVEL_WARNING, &
            message='Syntax error', &
            label=label_type('Invalid line number in #line directive', index(token, lowercase(ctx%content)) + len(token) + &
            1, len(num_str)), &
            source=ctx%path), &
            trim(ctx%content), ctx%line))
         return
      end if

      ! Update current line number (subtract 1 because the next line will be +1)
      ctx%line = new_line - 1

      ! Update filename if provided (strip quotes)
      if (has_filename) then
         if (fname(1:1) == '"' .and. len(fname) > 1) then
            closing = index(fname(2:), '"')
            if (closing == 0) then
               call printf(render(diagnostic_report(LEVEL_ERROR, &
                  message='Malformed #line directive', &
                  label=label_type('Missing closing quotation mark', &
                  index(ctx%content,'"'),1), &
                  source=trim(ctx%path)), &
                  ctx%content, ctx%line))
            end if
            fname = fname(2:closing)
         end if
         if (len_trim(fname) > 0) then
            ctx%path = trim(fname)
         end if
      end if
   end subroutine
end module

!>>>>> ././src/macro.f90
!> @file
!! @defgroup group_macro Macro
!! Macro management and expansion core of the fpx Fortran preprocessor
!!
!! This module implements a complete, standards-inspired macro system supporting:
!! - Object-like and function-like macros
!! - Variadic macros (`...` and `__VA_ARGS__`)
!! - C++20/C23-style `__VA_OPT__` handling for optional variadic content
!! - Parameter stringification (`#param`) and token pasting (`##`)
!! - Built-in predefined macros: `__FILE__`, `__FILENAME__`, `__LINE__`, `__DATE__`, `__TIME__`, `__TIMESTAMP__`, `__FUNC__`
!! - Recursive expansion with circular dependency detection via digraph analysis
!! - Dynamic macro table of `macro` objects with efficient addition, lookup, removal
!! - Full support for nested macro calls and proper argument handling
!!
!! The design allows safe, repeated expansion while preventing infinite recursion.
!! All operations are container-agnostic using allocatable dynamic arrays.
!!
!! @par Expansion Model
!! Macros are expanded recursively.
!! Circular dependencies are detected through dependency graph analysis.
!! Macro lookup is currently linear in the number of defined macros.
!!
!! @par Expansion Pipeline
!! Macro processing occurs in two stages:
!! - @link fpx_macro::expand_macros expand_macros @endlink performs recursive expansion of user-defined macros,
!!   including function-like macros, variadic substitutions, token
!!   pasting, stringification, and cycle detection.
!! - @link fpx_macro::expand_all expand_all @endlink subsequently substitutes predefined macros such as
!!   `__FILE__`, `__LINE__`, `__DATE__`, and related extensions.
!!
!! This separation allows internal preprocessing routines to reuse the
!! core expansion engine while selectively enabling predefined tokens.
!!
!! @section macro_examples Examples
!!
!! 1. Define and use simple macros:
!! @code{.f90}
!!    type(macro), allocatable :: macros(:)
!!    call add(macros, macro('PI', '3.1415926535'))
!!    call add(macros, macro('MSG(x)', 'print *, ″Hello ″, x'))
!!    print *, expand_all(context('area = PI * r**2', 10, './circle.F90', 'circle'), macros, stitch, .false., .false., .true.)
!!    !> prints: area = 3.1415926535 * r**2
!! @endcode
!!
!! 2. Variadic macro with stringification and pasting:
!! @code{.f90}
!!    call add(macros, macro('DEBUG_PRINT(...)', 'print *, ″DEBUG[″, __FILE__, ″:″, __LINE__, ″]: ″, __VA_ARGS__'))
!!    print *, expand_all(context('DEBUG_PRINT(″value =″, x)', 42, 'test.F90', 'text'), macros, stitch, .false., .false., .true.)
!!    !> prints: print *, 'DEBUG[', 'test.F90', ':', 42, ']: ', 'value =', x
!! @endcode
!!
!! 3. Token pasting with ##:
!! @code{.f90}
!!    call add(macros, macro('MAKE_VAR(name,num)', 'var_name_##num'))
!!    print *, expand_all(context('real :: MAKE_VAR(temp,42)', 5, 'file.F90', 'file'), macros, stitch, .false., .false.)
!!    !> prints: real :: var_name_42
!! @endcode
module fpx_macro
   use fpx_constants
   use fpx_logging
   use fpx_path
   use fpx_graph
   use fpx_string
   use fpx_date
   use fpx_logging
   use fpx_context

   implicit none; private

   public :: macro, &
      add, &
      get, &
      insert, &
      clear, &
      remove, &
      size_of

   public :: expand_macros, &
      expand_all, &
      is_defined, &
      read_unit, &
      preprocess_line

   !> Representation of a preprocessor macro.
   !!
   !! A macro stores its identifier together with the metadata required
   !! during expansion:
   !! - replacement text,
   !! - formal parameter list,
   !! - variadic status,
   !! - cycle detection flags,
   !! - temporary activation state.
   !!
   !! The type extends @link fpx_string::string string @endlink so that the
   !! macro name itself behaves as a string value.
   !!
   !! @section macro_type_examples Examples
   !!
   !! Object-like macro:
   !! @code{.f90}
   !!    type(macro) :: m
   !!    m = macro('PI', '3.1415926535')
   !! ...
   !! @endcode
   !!
   !! Function-like macro:
   !! @code{.f90}
   !!    type(macro) :: m
   !!    m = macro('SQR(x)', '((x)*(x))')
   !! ...
   !! @endcode
   !!
   !! @section macro_type_constructor Constructors
   !! Initializes a new instance of the @ref macro class
   !!
   !! @b Constructor
   !! @code{.f90}
   !! type(macro) function macro(character(*) name, (optional) character(*) val)
   !! @endcode
   !!
   !! @param[in] name
   !!   macro name
   !! @param[in] val
   !!   (optional) value of the macro
   !!
   !! @b Examples
   !! @code{.f90}
   !! type(macro) :: m
   !! m = macro('_WIN32')
   !! ...
   !! @endcode
   !! @return The constructed macro object.
   !!
   !! @ingroup group_macro
   type, extends(string) :: macro
      character(:), allocatable :: value  !< Value of the macro
      type(string), allocatable :: params(:)  !< List of parameter for function like macros
      logical :: is_variadic  !< Indicate whether the macro is variadic or not.
      logical :: is_cyclic    !< Indicates whether the macro has cyclic dependencies or not.
      logical :: active = .true.
   end type

   !> Construct a new macro definition.
   !!
   !! Creates an initialized @ref macro object with the specified name
   !! and optional replacement text.
   !!
   !! Parameter lists are initialized to empty, variadic expansion is
   !! disabled, and direct self-references are marked as cyclic.
   !!
   !! @param[in] name Macro identifier.
   !! @param[in] val  Replacement text (default: empty).
   !!
   !! @return Initialized macro object.
   !!
   !! @ingroup group_macro
   interface macro
      !! @cond
      module procedure :: macro_new
      !! @endcond
   end interface

   !> Append macros to a macro table.
   !!
   !! Existing definitions with the same name are replaced, while new
   !! definitions are appended to the dynamic array.
   !!
   !! Overloads support:
   !! - insertion of a single @ref macro object,
   !! - insertion by name only,
   !! - insertion by name and replacement text,
   !! - insertion of a range of macros.
   !!
   !! @ingroup group_macro
   interface add
      module procedure :: add_item
      module procedure :: add_item_from_name
      module procedure :: add_item_from_name_and_value
      module procedure :: add_range
   end interface

   !> Remove all macro definitions from a table.
   !!
   !! The table remains allocated as an empty array.
   !!
   !! @ingroup group_macro
   interface clear
      module procedure  :: clear_item
   end interface

   !> Retrieve a macro by index
   !!
   !! @ingroup group_macro
   interface get
      module procedure  :: get_item
   end interface

   !> Insert a macro at a specified position.
   !!
   !! Existing elements are shifted to preserve ordering.
   !!
   !! @ingroup group_macro
   interface insert
      module procedure :: insert_item
   end interface

   !> Remove a macro definition from a table.
   !!
   !! The array is compacted after removal and cyclic dependency
   !! markers are recomputed.
   !!
   !! @ingroup group_macro
   interface remove
      module procedure :: remove_item
   end interface

   !> Return the number of stored macro definitions.
   !!
   !! Convenience wrapper around the intrinsic `size` function that
   !! safely handles non allocated arrays.
   !!
   !! @ingroup group_macro
   interface size_of
      module procedure  :: size_item
   end interface

   !> Abstract interface to the top-level preprocessing routine.
   !!
   !! This callback allows modules such as the include handler to invoke
   !! recursive preprocessing of additional source units without creating
   !! circular module dependencies.
   !!
   !! Implementations are expected to preprocess the contents of the
   !! input unit and emit the resulting output to the specified unit.
   !!
   !! @ingroup group_include
   interface
      subroutine read_unit(iunit, ounit, macros, from_include)
         import macro; implicit none
         integer, intent(in)                     :: iunit
         integer, intent(in)                     :: ounit
         type(macro), allocatable, intent(inout) :: macros(:)
         logical, intent(in)                     :: from_include
      end subroutine
   end interface

   !> Abstract interface for line preprocessing callbacks.
   !!
   !! Implementations process a single source line after directive
   !! handling and macro substitution.
   !!
   !! The callback mechanism is primarily used by nested constructs such
   !! as `#for` expansion, allowing generated lines to re-enter the main
   !! preprocessing pipeline.
   !!
   !! @ingroup group_macro
   interface
      recursive function preprocess_line(current_line, ounit, filepath, linenum, macros, stch) result(rst)
         import macro; implicit none
         character(*), intent(in)                :: current_line
         integer, intent(in)                     :: ounit
         character(*), intent(inout)             :: filepath
         integer, intent(inout)                  :: linenum
         type(macro), allocatable, intent(inout) :: macros(:)
         logical, intent(out)                    :: stch
         character(:), allocatable               :: rst
      end function
   end interface
contains

   !> Construct a new macro object
   !! @param[in] name Mandatory macro name
   !! @param[in] val  Optional replacement text (default: empty)
   !! @return Initialized macro object
   type(macro) function macro_new(name, val) result(that)
      character(*), intent(in)            :: name
      character(*), intent(in), optional  :: val

      that = trim(name)
      if (present(val)) then
         that%value = val
      else
         that%value = ''
      end if
      allocate(that%params(0))
      that%is_variadic = .false.
      that%is_cyclic = that == that%value
      that%active = .true.
   end function

   !> Expand a source line including predefined macros.
   !!
   !! This routine represents the complete user-visible expansion phase.
   !!
   !! Expansion proceeds in two steps:
   !! 1. User-defined macros are expanded recursively through
   !!    @ref expand_macros.
   !! 2. Built-in predefined macros are substituted using the current
   !!    preprocessing context.
   !!
   !! Supported predefined macros include:
   !! - `__FILE__`
   !! - `__LINE__`
   !! - `__DATE__`
   !! - `__TIME__`
   !! - `__FUNC__`
   !! - `__FILENAME__` (extension)
   !! - `__TIMESTAMP__` (extension)
   !!
   !! @param[in]  ctx
   !!   Context
   !! @param[inout]  macros
   !!   Current macro table
   !! @param[out] stitch
   !!   Set to .true.true. if result ends with '&' (Fortran continuation)
   !! @param[in]  has_extra
   !!   Has extra macros (non-standard) like __FILENAME__ and __TIMESTAMP__
   !! @param[in]  implicit_conti
   !!   If .true., implicit continuation is permitted
   !! @param[in]  dollar_insert
   !!   If .true., the syntax ${} is supported for macro insertion
   !! @return Expanded line with all macros and predefined tokens replaced
   !!
   !! @ingroup group_macro
   function expand_all(ctx, macros, stitch, has_extra, implicit_conti, dollar_insert) result(expanded)
      type(context), intent(in)               :: ctx
      type(macro), allocatable, intent(inout) :: macros(:)
      logical, intent(out)                    :: stitch
      logical, intent(in)                     :: has_extra
      logical, intent(in)                     :: implicit_conti
      logical, intent(in)                     :: dollar_insert
      character(:), allocatable :: expanded
      !private
      integer :: pos, start, sep, dot, imacro
      type(datetime) :: date

      if (has_extra) then
         if (.not. is_defined('__FUNC__', macros, imacro)) then
            call add(macros, '__FUNC__', '')
         end if
      end if

      expanded = expand_macros(ctx%content, macros, stitch, implicit_conti, dollar_insert, ctx)

      date = now()

      ! Substitute __FILE__ (relative path to working directory)
      pos = 1
      do while (pos > 0)
         pos = index(expanded, '__FILE__')
         if (pos > 0) then
            start = pos + len('__FILE__')
            expanded = trim(expanded(:pos - 1) // '"' // trim(ctx%path) // '"' // trim(expanded(start:)))
         end if
      end do

      ! Substitute __LINE__
      pos = 1
      do while (pos > 0)
         pos = index(expanded, '__LINE__')
         if (pos > 0) then
            if (pos > 0) then
               start = pos + len('__LINE__')
               expanded = trim(expanded(:pos - 1) // tostring(ctx%line) // trim(expanded(start:)))
            end if
         end if
      end do

      ! Substitute __DATE__
      pos = 1
      do while (pos > 0)
         pos = index(expanded, '__DATE__')
         if (pos > 0) then
            if (pos > 0) then
               start = pos + len('__DATE__')
               expanded = trim(expanded(:pos - 1) // '"' // date%to_string('MMM-dd-yyyy') // '"' // trim(expanded(start:)))
            end if
         end if
      end do

      ! Substitute __TIME__
      pos = 1
      do while (pos > 0)
         pos = index(expanded, '__TIME__')
         if (pos > 0) then
            if (pos > 0) then
               start = pos + len('__TIME__')
               expanded = trim(expanded(:pos - 1) // '"' // date%to_string('HH:mm:ss') // '"' // trim(expanded(start:)))
            end if
         end if
      end do

      if (has_extra) then
         ! Substitute __FILENAME__
         pos = 1; do while (pos > 0)
            pos = index(expanded, '__FILENAME__')
            if (pos > 0) then
               start = pos + len('__FILENAME__')
               expanded = trim(expanded(:pos - 1) // '"' // filename(ctx%path, .true.) // '"' // trim(expanded(start:)))
            end if
         end do

         ! Substitute __TIMESTAMP__
         pos = 1; do while (pos > 0)
            pos = index(expanded, '__TIMESTAMP__')
            if (pos > 0) then
               if (pos > 0) then
                  start = pos + len('__TIMESTAMP__')
                  expanded = trim(expanded(:pos - 1) // '"' // date%to_string('ddd MM yyyy') // ' ' // date%to_string(&
                     'HH:mm:ss'&
                  &) // '"' // trim(expanded(start:)))
               end if
            end if
         end do
      end if
   end function

   !> Recursively expand user-defined macros.
   !!
   !! Implements the core expansion engine used throughout fpx.
   !!
   !! Supported features include:
   !! - object-like macros,
   !! - function-like macros,
   !! - variadic macros,
   !! - `__VA_ARGS__`,
   !! - `__VA_OPT__`,
   !! - parameter stringification,
   !! - token pasting,
   !! - nested expansion,
   !! - optional `${...}` substitutions,
   !! - circular dependency detection.
   !!
   !! Recursive expansion terminates automatically when cyclic
   !! dependencies are detected.
   !!
   !! @param[in]  line
   !!   Line to be expanded
   !! @param[inout]  macros
   !!   Current macro table
   !! @param[out] stitch
   !!   .true. if final line ends with '&'
   !! @param[in]  implicit_conti
   !!   If .true., implicit continuation is permitted
   !! @param[in]  dollar_insert
   !!   If .true., ${} macro substitution is supported
   !! @param[in]  ctx
   !!   Context
   !! @return Line with user-defined macros expanded (predefined tokens untouched)
   !!
   !! @ingroup group_macro
   function expand_macros(line, macros, stitch, implicit_conti, dollar_insert, ctx) result(expanded)
      character(*), intent(in)                :: line
      type(macro), allocatable, intent(inout) :: macros(:)
      logical, intent(out)                    :: stitch
      logical, intent(in)                     :: implicit_conti
      logical, intent(in)                     :: dollar_insert
      type(context), intent(in)               :: ctx
      character(:), allocatable               :: expanded
      !private
      integer :: imacro, paren_level
      type(digraph) :: graph

      imacro = 0; paren_level = 0
      graph = digraph(size(macros))
      stitch = .false.

      expanded = expand_macros_internal(line, imacro, macros)

      if (implicit_conti) then
         stitch = (tail(expanded) == '&') .or. paren_level > 0
      else
         stitch = (tail(expanded) == '&') .and. paren_level > 0
      end if
   contains
      !> @private
      recursive function expand_macros_internal(line, imacro, macros) result(expanded)
         character(*), intent(in)                :: line
         integer, intent(in)                     :: imacro
         type(macro), allocatable, intent(inout) :: macros(:)
         character(:), allocatable :: expanded
         !private
         character(:), allocatable :: args_str, temp, va_args
         character(:), allocatable :: token1, token2, prefix, suffix
         type(string) :: arg_values(MAX_PARAMS)
         integer :: c, i, j, k, n, pos, start, arg_start, nargs
         integer :: m_start, m_end, token1_start, token2_stop
         logical :: isopened, found
         character :: quote
         integer, allocatable :: indexes(:)
         logical :: exists, ok, hasfunc

         expanded = line
         if (size(macros) == 0) return
         isopened = .false.; hasfunc = .false.

         do i = 1, size(macros)
            n = len_trim(macros(i)); if (n == 0) cycle
            c = 0
            do while (c < len_trim(expanded))
               c = c + 1
               if (expanded(c:c) == '"' .or. expanded(c:c) == "'") then
                  if (.not. isopened) then
                     isopened = .true.
                     quote = expanded(c:c)
                  else
                     if (expanded(c:c) == quote) isopened = .false.
                  end if
               end if
               if (isopened) cycle
               if (c + n - 1 > len_trim(expanded)) exit

               if (.not. hasfunc) then
                  call update_func_macro(expanded, macros)
                  hasfunc = .true.
               end if

               ! Placeholder expansion: ${NAME}
               if (dollar_insert) then
                  if (expanded(c:c) == '$') then
                     if (c < len_trim(expanded)) then
                        if (expanded(c + 1:c + 1) == '{') then
                           j = c + 2
                           do while (j <= len_trim(expanded))
                              if (expanded(j:j) == '}') exit
                              j = j + 1
                           end do

                           if (j <= len_trim(expanded)) then
                              token1 = trim(expanded(c + 2:j - 1))
                              if (is_defined(token1, macros, idx=k)) then
                                 temp = macros(k)%value
                                 if (len(temp) == 0 .and. .not. macros(k)%active) then
                                    c = j
                                 else
                                    expanded = expanded(:c - 1) // temp // expanded(j + 1:)
                                    if (len(temp) /= 0) then
                                       c = c + len_trim(temp) - 1
                                    end if
                                 end if
                                 cycle
                              end if
                           end if
                        end if
                     end if
                  end if
               end if

               found = .false.
               if (expanded(c:c + n - 1) == macros(i)) then
                  found = .true.
                  if (len_trim(expanded(c:)) > n) then
                     found = verify(expanded(c + n:c + n), ' ()[]<>&;.,^~!/*-+\="' // "'") == 0
                  end if
                  if (found .and. c > 1) then
                     found = verify(expanded(c - 1:c - 1), ' ()[]<>&;.,^~!/*-+\="' // "'") == 0
                  end if
               end if

               if (found) then
                  pos = c
                  c = c + n - 1
                  m_start = pos
                  start = pos + n
                  ok = allocated(macros(i)%params); if (ok) ok = size(macros(i)%params) > 0
                  if (ok .or. macros(i)%is_variadic) then
                     if (start <= len(expanded)) then
                        if (expanded(start:start) == '(') then
                           paren_level = 1
                           arg_start = start + 1
                           nargs = 0
                           j = arg_start
                           do while (j <= len(expanded) .and. paren_level > 0)
                              if (expanded(j:j) == '(') paren_level = paren_level + 1
                              if (expanded(j:j) == ')') paren_level = paren_level - 1
                              if (paren_level == 1 .and. expanded(j:j) == ',' .or. paren_level == 0) then
                                 if (nargs < MAX_PARAMS) then
                                    nargs = nargs + 1
                                    arg_values(nargs) = trim(adjustl(expanded(arg_start:j - 1)))
                                    arg_start = j + 1
                                 end if
                              end if
                              j = j + 1
                           end do
                           m_end = j - 1
                           args_str = expanded(start:m_end)
                           temp = trim(macros(i)%value)

                           if (macros(i)%is_variadic) then
                              if (nargs < size(macros(i)%params)) then
                                 call printf(render(diagnostic_report(LEVEL_ERROR, &
                                    message='Variadic macro issue', &
                                    label=label_type('Too few arguments for macro ' // macros(i), start, m_end - &
                                    start), &
                                    source=trim(ctx%path)), &
                                    expanded, ctx%line))
                                 cycle
                              end if
                              va_args = ''
                              do j = size(macros(i)%params) + 1, nargs
                                 if (j > size(macros(i)%params) + 1) va_args = va_args // ', '
                                 va_args = va_args // arg_values(j)
                              end do
                           else if (nargs /= size(macros(i)%params)) then
                              call printf(render(diagnostic_report(LEVEL_ERROR, &
                                 message='Function-like macro issue', &
                                 label=label_type('Incorrect number of arguments for macro ' // macros(i), start, &
                                 m_end - start), &
                                 source=trim(ctx%path)), &
                                 expanded, ctx%line))
                              cycle
                           end if

                           ! Substitute regular parameters
                           argbck :block
                              integer :: c1, h1
                              logical :: opened

                              opened = .false.
                              jloop: do j = 1, size(macros(i)%params)
                                 c1 = 0
                                 wloop: do while (c1 < len_trim(temp))
                                    c1 = c1 + 1
                                    if (temp(c1:c1) == '"') opened = .not. opened
                                    if (opened) cycle wloop
                                    if (c1 + len_trim(macros(i)%params(j)) - 1 > len(temp)) cycle wloop

                                    if (temp(c1:c1 + len_trim(macros(i)%params(j)) - 1) == trim(macros(i)%params(j))) &
                                       then
                                       checkbck:block
                                          integer :: cend, l

                                          cend = c1 + len_trim(macros(i)%params(j))
                                          l = len(temp)
                                          if (c1 == 1 .and. cend == l + 1) then
                                             exit checkbck
                                          else if (c1 > 1 .and. l == cend - 1) then
                                             if (verify(temp(c1 - 1:c1 - 1), ' #()[]<>&;.,!/*-+\="' // "'") /= 0) &
                                                cycle wloop
                                          else if (c1 <= 1 .and. cend <= l) then
                                             if (verify(temp(cend:cend), ' #()[]<>&;.,!/*-+\="' // "'") /= 0) cycle &
                                                wloop
                                          else
                                             if (verify(temp(c1 - 1:c1 - 1), ' #()[]<>&;.,!/*-+\="' // "'") /= 0 &
                                                .or. verify(temp(cend:cend), ' #()[]<>$&;.,!/*-+\="' // "'") /=&
                                             & 0) cycle wloop
                                          end if
                                       end block checkbck
                                       pos = c1
                                       c1 = c1 + len_trim(macros(i)%params(j)) - 1
                                       start = pos + len_trim(macros(i)%params(j))
                                       if (pos == 2) then
                                          if (temp(pos - 1:pos - 1) == '#') then
                                             temp = trim(temp(:pos - 2) // '"' // arg_values(j) // '"' // trim(temp(&
                                                start:)))
                                          else
                                             temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:)))
                                          end if
                                       elseif (pos > 2) then
                                          h1 = pos - 1
                                          if (previous(temp, h1) == '#') then
                                             if (h1 == 1) then
                                                temp = trim(temp(:h1 - 1) // '"' // arg_values(j) // '"' // trim(&
                                                   temp(start:)))
                                             else
                                                if (temp(h1 - 1:h1 - 1) /= '#') then
                                                   temp = trim(temp(:h1 - 1) // '"' // arg_values(j) // '"' // &
                                                      trim(temp(start:)))
                                                else
                                                   temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:&
                                                      )))
                                                end if
                                             end if
                                          else
                                             temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:)))
                                          end if
                                       else
                                          temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:)))
                                       end if
                                    end if
                                 end do wloop
                              end do jloop
                           end block argbck

                           ! Handle concatenation (##) first with immediate substitution
                           block
                              pos = 1
                              do while (pos > 0)
                                 pos = index(temp, '##')
                                 if (pos > 0) then
                                    ! Find token1 (before ##)
                                    k = pos - 1
                                    if (k <= 0) then
                                       call printf(render(diagnostic_report(LEVEL_ERROR, &
                                          message='Syntax error', &
                                          label=label_type('No token before ##', pos, 2), &
                                          source=trim(ctx%path)), &
                                          temp, ctx%line))
                                       cycle
                                    end if

                                    token1 = adjustr(temp(:k))
                                    prefix = ''
                                    token1_start = index(token1, ' ')
                                    if (token1_start > 0) then
                                       prefix = token1(:token1_start)
                                       token1 = token1(token1_start + 1:)
                                    end if

                                    ! Find token2 (after ##)
                                    k = pos + 2
                                    if (k > len(temp)) then
                                       call printf(render(diagnostic_report(LEVEL_ERROR, &
                                          message='Syntax error', &
                                          label=label_type('No token after ##', pos, 2), &
                                          source=trim(ctx%path)), &
                                          temp, ctx%line))
                                       cycle
                                    end if

                                    suffix = ''
                                    token2 = adjustl(temp(k:))
                                    token2_stop = index(token2, ' ')
                                    if (token2_stop > 0) then
                                       suffix = token2(token2_stop:)
                                       token2 = token2(:token2_stop - 1)
                                    end if

                                    ! Concatenate, replacing the full 'token1 ## token2' pattern
                                    if (is_defined(token1, macros, idx=k)) &
                                       token1 = expand_macros_internal(token1, imacro, macros)
                                    if (is_defined(token2, macros, idx=k)) &
                                       token2 = expand_macros_internal(token2, imacro, macros)

                                    temp = trim(prefix // trim(token1) // trim(token2) // suffix)
                                 end if
                              end do
                           end block

                           ! Substitute __VA_ARGS__
                           block
                              if (macros(i)%is_variadic) then
                                 pos = 1
                                 do while (pos > 0)
                                    pos = index(temp, '__VA_ARGS__')
                                    if (pos > 0) then
                                       start = pos + len('__VA_ARGS__') - 1
                                       if (start < len(temp) .and. temp(start:start) == '_' &
                                          .and. temp(start + 1:start + 1) == ')') then
                                          temp = trim(temp(:pos - 1) // trim(va_args) // ')')
                                       else
                                          temp = trim(temp(:pos - 1) // trim(va_args) // trim(temp(start + 1:)))
                                       end if

                                       ! Substitute __VA_OPT__
                                       pos = index(temp, '__VA_OPT__')
                                       if (pos > 0) then
                                          start = pos + index(temp(pos:), ')') - 1
                                          if (len_trim(va_args) > 0) then
                                             temp = trim(temp(:pos - 1)) // temp(pos + index(temp(pos:), '('):start &
                                                - 1) // trim(temp(start + 1:))
                                          else
                                             temp = trim(temp(:pos - 1)) // trim(temp(start + 1:))
                                          end if
                                       end if
                                    end if
                                 end do
                              end if
                           end block

                           call graph%add_edge(imacro, i)
                           if (.not. graph%is_circular(i)) then
                              temp = expand_macros_internal(temp, i, macros)  ! Only for nested macros
                           else
                              call printf(render(diagnostic_report(LEVEL_ERROR, &
                                 message='Failed macro expansion', &
                                 label=label_type('Circular macro detected', index(temp, macros(i)), len(macros(i)))&
                                 , &
                                 source=trim(ctx%path)), &
                                 temp, ctx%line))
                              cycle
                           end if
                           expanded = trim(expanded(:m_start - 1) // trim(temp) // expanded(m_end + 1:))
                        end if
                     end if
                  else
                     temp = trim(macros(i)%value)
                     m_end = start - 1
                     call graph%add_edge(imacro, i)
                     if ((.not. graph%is_circular(i)) .and. (.not. macros(i)%is_cyclic)) then
                        expanded = trim(expanded(:m_start - 1) // trim(temp) // expanded(m_end + 1:))
                        expanded = expand_macros_internal(expanded, imacro, macros)
                     else
                        call printf(render(diagnostic_report(LEVEL_ERROR, &
                           message='Failed macro expansion', &
                           label=label_type('Circular macro detected', index(temp, macros(i)), len(macros(i))), &
                           source=trim(ctx%path)), &
                           temp, ctx%line))
                        cycle
                     end if
                  end if
               end if
            end do
         end do
         pos = index(expanded, '&')
         if (index(expanded, '!') > pos .and. pos > 0) expanded = expanded(:pos + 1)
      end function
   end function

   !> Determine whether a macro is currently defined.
   !!
   !! Performs a linear search through the macro table and optionally
   !! returns the corresponding index.
   !!
   !! @param[in] name
   !!   Macro identifier.
   !! @param[in] macros
   !!   Macro table.
   !! @param[out] idx
   !!   Position of the matching entry, if present.
   !! @return `.true.` if the macro exists.
   !!
   !! @ingroup group_macro
   logical function is_defined(name, macros, idx) result(res)
      character(*), intent(in)            :: name
      type(macro), intent(in)             :: macros(:)
      integer, intent(inout), optional    :: idx
      !private
      integer :: i

      res = .false.
      do i = 1, size(macros)
         if (macros(i) == trim(name)) then
            res = .true.
            if (present(idx)) idx = i
            exit
         end if
      end do
   end function

   !> Convert a scalar value to its textual representation.
   !!
   !! Supports intrinsic integer, real, logical, character, and complex
   !! values of common kinds.
   !!
   !! Primarily intended for internal diagnostics and macro processing.
   !! @private
   !! @ingroup group_macro
   function tostring(any)
      class(*), intent(in) :: any
      !private
      character(:), allocatable   :: tostring
      character(4096)             :: line

      call print_any(any); tostring = trim(line)
   contains
      !> @private
      subroutine print_any(any)
         use, intrinsic :: iso_fortran_env, only: int8, &
            int16, &
            int32, &
            int64, &
            real32, &
            real64, &
            real128
         class(*), intent(in)     :: any

         select type (any)
          type is (integer(kind=int8)); write(line, '(i0)') any
          type is (integer(kind=int16)); write(line, '(i0)') any
          type is (integer(kind=int32)); write(line, '(i0)') any
          type is (integer(kind=int64)); write(line, '(i0)') any
          type is (real(kind=real32)); write(line, '(1pg0)') any
          type is (real(kind=real64)); write(line, '(1pg0)') any
          type is (real(kind=real128)); write(line, '(1pg0)') any
          type is (logical); write(line, '(1l)') any
          type is (character(*)); write(line, '(a)') any
          type is (complex(kind=real32)); write(line, '("(",1pg0,",",1pg0,")")') any
          type is (complex(kind=real64)); write(line, '("(",1pg0,",",1pg0,")")') any
          type is (complex(kind=real128)); write(line, '("(",1pg0,",",1pg0,")")') any
         end select
      end subroutine
   end function

   !> Internal helper: grow dynamic macro array in chunks for efficiency
   !! Adds a new macro to the allocatable array.
   !! Also detects direct self-references (A -> A) and marks both sides as cyclic.
   !!
   subroutine add_to(array, val)
      type(macro), allocatable, intent(inout) :: array(:)
      type(macro), intent(in)                 :: val(..)
      !private
      type(macro), allocatable :: tmp(:)
      logical, allocatable :: isdef(:)
      integer :: i, j, n

      n = size_of(array)

      select rank (val)
      rank(0)
      allocate(isdef(1), source=.false.)
      do i = 1, n
         if (array(i) == val) then
            array(i) = val
            isdef(1) = .true.
         end if
      end do
      if (.not. isdef(1)) then
         allocate(tmp(n + 1))
         if (n > 0) tmp(1:n) = array
         tmp(n + 1) = val
         call move_alloc(tmp, array)
         if (allocated(tmp)) deallocate(tmp)
      end if
      rank(1)
      allocate(isdef(size(val)), source=.false.)
      do concurrent (i = 1:n, j = 1:size(val))
         if (array(i) == val(j)) then
            array(i) = val(j)
            isdef(j) = .true.
         end if
      end do
      n = size_of(array); allocate(tmp(n + count(isdef)))
      if (n > 0) tmp(1:n) = array
      tmp(n + 1:) = pack(val, isdef)
      call move_alloc(tmp, array)
      if (allocated(tmp)) deallocate(tmp)
   end select

   do i = 1, size_of(array)
      do j = n + 1, size(array)
         if (i == j) cycle
         if (array(i) == array(j)%value .and. array(i)%value == array(j)) then
            array(i)%is_cyclic = .true.
         end if
      end do
   end do
end subroutine

 !> Add a complete macro object to the table
subroutine add_item(this, m)
   type(macro), intent(inout), allocatable :: this(:)
   type(macro), intent(in)                 :: m

   call add_to(this, m)
end subroutine

 !> Add macro by name only (value = empty)
subroutine add_item_from_name(this, name)
   type(macro), intent(inout), allocatable :: this(:)
   character(*), intent(in)                :: name

   if (.not. allocated(this)) allocate(this(0))
   call add_to(this, macro(name))
end subroutine

 !> Add macro with name and replacement text
subroutine add_item_from_name_and_value(this, name, value)
   type(macro), intent(inout), allocatable :: this(:)
   character(*), intent(in)                :: name
   character(*), intent(in)                :: value

   if (.not. allocated(this)) allocate(this(0))
   call add_to(this, macro(name, value))
end subroutine

 !> Add multiple macros at once
subroutine add_range(this, m)
   type(macro), intent(inout), allocatable :: this(:)
   type(macro), intent(in)                 :: m(:)

   if (.not. allocated(this)) allocate(this(0))
   call add_to(this, m)
end subroutine

 !> Remove all macros from table
subroutine clear_item(this)
   type(macro), intent(inout), allocatable :: this(:)

   if (allocated(this)) deallocate(this)
   allocate(this(0))
end subroutine

 !> Retrieve macro by 1-based index
function get_item(this, key) result(res)
   type(macro), intent(inout)  :: this(:)
   integer, intent(in)         :: key
   type(macro), allocatable    :: res
   !private
   integer :: n

   n = size(this)
   if (key > 0 .and. key <= n) then
      res = this(key)
   end if
end function

 !> Insert macro at specific position
subroutine insert_item(this, i, m)
   type(macro), intent(inout), allocatable :: this(:)
   integer, intent(in)                     :: i
   type(macro), intent(in)                 :: m
   !private
   integer :: j, count

   if (.not. allocated(this)) allocate(this(0))
   count = size(this)
   call add_to(this, m)

   do j = count, i + 1, -1
      this(j) = this(j - 1)
   end do
   this(i) = m
end subroutine

 !> Return number of defined macros
pure integer function size_item(x) result(res)
   class(*), dimension(..), intent(in), optional :: x
   res = 0
   if (present(x)) res = size(x)
end function

 !> Remove macro at given index
subroutine remove_item(this, i)
   type(macro), intent(inout), allocatable :: this(:)
   integer, intent(in)                     :: i
   !private
   type(macro), allocatable :: tmp(:)
   integer :: k, j, n

   if (.not. allocated(this)) allocate(this(0))
   n = size(this)
   if (allocated(this(i)%params)) deallocate(this(i)%params)
   if (n > 1) then
      this(i:n - 1) = this(i + 1:n)
      allocate(tmp(n - 1))
      tmp = this(:n - 1)
      deallocate(this)
      call move_alloc(tmp, this)

      this(:)%is_cyclic = .false.
      do k = 1, size(this)
         do j = 1, size(this)
            if (this(k) == this(j)%value .and. this(k)%value == this(j)) then
               this(i)%is_cyclic = .true.
               this(j)%is_cyclic = .true.
            end if
         end do
      end do
   else
      deallocate(this); allocate(this(0))
   end if
end subroutine

 !> Update the special predefined macro __FUNC__
 !!
 !! Examines the current source line and detects whether it introduces
 !! a Fortran procedure definition (`function` or `subroutine`).
 !! When a procedure declaration is found, the macro `__FUNC__` is
 !! created or updated with the procedure name.
 !!
 !! When an `end function`, `endfunction`, `end subroutine`, or
 !! `endsubroutine` statement is encountered, the macro value is
 !! cleared.
 !!
 !! Detection is token based and therefore supports arbitrary valid
 !! Fortran declaration prefixes such as:
 !! - `recursive function foo()`
 !! - `pure elemental function bar()`
 !! - `type(string) function baz() result(res)`
 !! - `module subroutine solve()`
 !!
 !! The macro value reflects the innermost active procedure and is
 !! automatically cleared when leaving the corresponding scope.
 !!
 !! @param[in]    line
 !!   Current source line after continuation handling
 !! @param[inout] macros
 !!   Current macro table (updated in-place)
 !!
 !! @ingroup group_macro
subroutine update_func_macro(line, macros)
   character(*), intent(in)                :: line
   type(macro), allocatable, intent(inout) :: macros(:)
   !private
   character(:), allocatable :: txt
   character(:), allocatable :: procname
   logical :: leaving
   integer :: imacro

   if (.not. is_defined('__FUNC__', macros, imacro)) return

   txt = lowercase(adjustl(trim(line)))
   procname = extract_proc_name(txt, leaving)

   if (len_trim(procname) > 0) then
      macros(imacro)%value = procname
      return
   end if

   ! Leaving a procedure
   if (starts_with(txt, 'end function') .or. &
      starts_with(txt, 'endfunction') .or. &
      starts_with(txt, 'end subroutine') .or. &
      starts_with(txt, 'endsubroutine')) then

      if (.not. is_defined('__FUNC__', macros, imacro)) then
         call add(macros, '__FUNC__', '')
      else
         macros(imacro)%value = ''
      end if
   end if
end subroutine

 !> Extract the procedure name from a Fortran procedure declaration
 !!
 !! Searches a source line for a standalone `function` or `subroutine`
 !! token and returns the identifier immediately following it.
 !!
 !! The parser is intentionally independent of declaration prefixes,
 !! allowing valid declarations such as:
 !! @code{.f90}
 !!    function foo()
 !!    recursive function foo()
 !!    pure elemental function foo()
 !!    type(string) function foo() result(res)
 !!    module subroutine solve()
 !! @endcode
 !!
 !! End statements (`end function`, `endfunction`,
 !! `end subroutine`, `endsubroutine`) are ignored and return
 !! an unallocated result.
 !!
 !! @param[in] txt
 !!   Source line to analyze
 !! @return Extracted procedure name, or an empty string when no
 !!         procedure declaration is detected.
 !!
 !! @ingroup group_macro
function extract_proc_name(txt, leaving) result(name)
   character(*), intent(in)    :: txt
   logical, intent(out)        :: leaving
   character(:), allocatable   :: name
   !private
   integer :: pos, istart, iend
   character(:), allocatable :: tmp

   name = ''
   tmp = lowercase(adjustl(trim(txt)))

   ! Ignore END FUNCTION / END SUBROUTINE
   if (index(tmp, 'end function') > 0) then
      leaving = .true.
      return
   elseif (index(tmp, 'endfunction') > 0) then
      leaving = .true.
      return
   elseif (index(tmp, 'end subroutine') > 0) then
      leaving = .true.
      return
   elseif (index(tmp, 'endsubroutine') > 0) then
      leaving = .true.
      return
   end if

   ! Search FUNCTION token
   pos = find_token(tmp, 'function')

   if (pos > 0) then
      istart = pos + len('function')
   else
      pos = find_token(tmp, 'subroutine')
      if (pos == 0) return
      istart = pos + len('subroutine')
   end if

   ! Skip whitespace
   do while (istart <= len(tmp))
      if (tmp(istart:istart) /= ' ') exit
      istart = istart + 1
   end do

   if (istart > len(tmp)) return

   iend = istart

   do while (iend <= len(tmp))
      select case (tmp(iend:iend))
       case ('a':'z', 'A':'Z', '0':'9', '_')
         iend = iend + 1
       case default
         exit
      end select
   end do

   name = tmp(istart:iend - 1)
contains
   !> Locate a standalone token within a source line
   !! Searches for a token delimited by non-identifier characters.
   !! The token must not appear as part of a larger identifier.
   !!
   !! Examples:
   !! @code{.f90}
   !!    function foo()      ! match "function"
   !!    subroutine bar()    ! match "subroutine"
   !!    myfunction()        ! no match
   !!    subroutine_name     ! no match
   !! @endcode
   !!
   !! @param[in] line  Source line to search
   !! @param[in] token Token to locate
   !! @return Position of the first valid token occurrence,
   !!         or zero if not found
   !!
   !! @private
   !! @ingroup group_macro
   integer function find_token(line, token) result(pos)
      character(*), intent(in) :: line
      character(*), intent(in) :: token
      !private
      integer :: i, ltok, lline
      logical :: left_ok, right_ok

      pos = 0
      lline = len_trim(line); ltok = len_trim(token)

      if (ltok == 0 .or. lline < ltok) return

      do i = 1, lline - ltok + 1
         if (lowercase(line(i:i + ltok - 1)) /= lowercase(token)) cycle

         ! Check left boundary
         if (i == 1) then
            left_ok = .true.
         else
            left_ok = .not. is_ident(line(i - 1:i - 1))
         end if

         ! Check right boundary
         if (i + ltok - 1 == lline) then
            right_ok = .true.
         else
            right_ok = .not. is_ident(line(i + ltok:i + ltok))
         end if

         if (left_ok .and. right_ok) then
            pos = i
            return
         end if
      end do
   end function

   !> Determine whether a character is a valid identifier character
   !!
   !! Returns `.true.` for characters that may appear in a Fortran
   !! identifier:
   !! - letters (`A-Z`, `a-z`)
   !! - digits (`0-9`)
   !! - underscore (`_`)
   !!
   !! Used internally by token matching routines to verify identifier
   !! boundaries.
   !!
   !! @param[in] ch
   !!   Character to test
   !! @return `.true.` if the character is a valid identifier character
   !!
   !! @private
   !! @ingroup group_macro
   logical function is_ident(ch)
      character(1), intent(in) :: ch

      select case (ch)
       case ('a':'z', 'A':'Z', '0':'9', '_')
         is_ident = .true.
       case default
         is_ident = .false.
      end select
   end function
end function
end module

!>>>>> ././src/global.f90
!> @file
!! @defgroup group_global Global
!! Global configuration and shared runtime state for the fpx preprocessor.
!!
!! This module defines the central configuration object used throughout the
!! entire preprocessing session. A single public instance,
!! @link fpx_global::global global @endlink, stores all persistent settings controlling the behavior of
!! the preprocessor.
!!
!! The global configuration provides:
!!
!! - User-defined macro definitions.
!! - Symbols explicitly excluded via `#undef`.
!! - Additional include search directories.
!! - Feature switches controlling optional extensions.
!! - Behavioural settings affecting parsing and expansion.
!! - Runtime flags used by interactive preprocessing sessions.
!!
!! All fpx components access the same global state, avoiding the need to pass
!! configuration objects through every procedure call.
!!
!! The design assumes the traditional single-threaded preprocessing model.
!! If multiple preprocessing jobs are executed concurrently, each instance
!! should maintain its own independent configuration object.
!!
!! @section global_features Supported configuration options
!!
!! The following settings are available:
!!
!! - `macros(:)`
!!   Collection of predefined macros available before preprocessing begins.
!!
!! - `undef(:)`
!!   Symbols protected from future redefinition through `#define`.
!!
!! - `includedir(:)`
!!   Additional directories searched by `#include`.
!!
!! - `expand_macros`
!!   Enables or disables macro expansion globally.
!!
!! - `exclude_comments`
!!   Controls whether comments are preserved in the generated output.
!!
!! - `implicit_continuation`
!!   Enables implicit continuation during macro expansion.
!!
!! - `line_break`
!!   Interprets a double backslash (`\\`) as an explicit line break.
!!
!! - `extra_macros`
!!   Enables non-standard predefined macros such as:
!!   - `__FILE__`
!!   - `__LINE__`
!!   - `__FUNC__`
!!   - `__TIMESTAMP__`
!!
!! - `interactive`
!!   Enables REPL-style interactive preprocessing.
!!
!! - `support_forloop`
!!   Enables support for the non-standard `#for` / `#endfor` directives.
!!
!! - `disable_continuation`
!!   Disables explicit Fortran continuation handling using trailing `&`.
!!
!! - `support_dollar_insert`
!!   Enables `${NAME}` placeholder substitution during macro expansion.
!!
!! @note
!! All settings can be modified at any time before invoking
!! `preprocess(...)`.
!!
!! @section global_examples Examples
!!
!! 1. Add custom include paths:
!! @code{.f90}
!!    use fpx_global
!!
!!    global%includedir = [ &
!!        string('./include'), &
!!        string('../common'), &
!!        string('/usr/local/include/fpx') ]
!!
!!    call preprocess('main.F90')
!! ...
!! @endcode
!!
!! 2. Predefine macros:
!! @code{.f90}
!!    use fpx_global
!!    use fpx_macro
!!
!!    call add(global%macros, macro('DEBUG','1'))
!!    call add(global%macros, macro('MPI_VERSION','4'))
!!
!!    call preprocess('solver.F90')
!! ...
!! @endcode
!!
!! 3. Disable macro expansion:
!! @code{.f90}
!!    global%expand_macros = .false.
!!
!!    call preprocess('input.F90', 'output.F90')
!! ...
!! @endcode
!!
!! 4. Enable fpx extensions:
!! @code{.f90}
!!    global%support_forloop      = .true.
!!    global%support_dollar_insert = .true.
!!    global%extra_macros         = .true.
!!
!!    call preprocess('templates.F90')
!! ...
!! @endcode
!!
!! 5. Start an interactive preprocessing session:
!! @code
!!    global%interactive = .true.
!!
!!    call preprocess(stdin, stdout)
!! ...
!! @endcode
!!
!! @see
!! <a href="./group__group__macro.html">macro</a>
!! <a href="./group__group__parser.html">parser</a>
!! <a href="./group__group__include.html">include</a>
module fpx_global
   use fpx_constants
   use fpx_string
   use fpx_macro

   implicit none; private

   !> Global preprocessor configuration and shared runtime state.
   !!
   !! This type encapsulates all user-configurable options controlling the
   !! behaviour of the fpx preprocessor.
   !!
   !! A single public instance, @ref global, is provided and used throughout
   !! the library. Applications may modify its components before starting
   !! preprocessing to customize parsing rules, enable extensions, or
   !! predefine symbols.
   !!
   !! @section global_type_examples Examples
   !!
   !! @code{.f90}
   !!    use fpx_global
   !!    use fpx_macro
   !!
   !!    call add(global%macros, macro('__LFORTRAN__','1'))
   !!    global%extra_macros = .true.
   !!    global%support_forloop = .true.
   !! ...
   !! @endcode
   !!
   !! @section global_type_remarks Remarks
   !!
   !! - The settings remain active for the duration of the preprocessing session.
   !! - Components may be modified at any time before calling `preprocess`.
   !! - The global instance is intended for single-threaded use.
   !!
   !! @ingroup group_global
   type, public :: global_settings
      private
      type(macro), allocatable, public    :: macros(:)        !< Predefined macros available before preprocessing begins.
      type(string), allocatable, public   :: undef(:)         !< Symbols protected from future redefinition.
      type(string), allocatable, public   :: includedir(:)    !< Additional directories searched by `#include`.
      logical, public                     :: expand_macros = .true.   !< Enable global macro expansion.
      logical, public                     :: exclude_comments = .false.    !< Preserve comments in the generated output.
      logical, public                     :: implicit_continuation = .false.  !< Enable implicit continuation during macro expansion.
      logical, public                     :: line_break = .false.  !< Treat `\\` as an explicit output line break.
      logical, public                     :: extra_macros = .true.  !< Enable non-standard predefined macros such as `__FILE__`, `__LINE__`, `__FUNC__`, and `__TIMESTAMP__`.
      logical, public                     :: interactive = .false.  !< Enable interactive REPL mode.
      logical, public                     :: support_forloop = .true.  !< Enable support for `#for` and `#endfor`.
      logical, public                     :: disable_continuation = .false.  !< Disable explicit continuation using trailing `&`.
      logical, public                     :: support_dollar_insert = .true.  !< Enable `${NAME}` placeholder substitution.
   end type

   !> Global preprocessor configuration instance.
   !!
   !! This singleton is automatically initialized with sensible default
   !! values and is shared by all fpx modules during preprocessing.
   !!
   !! Applications typically customize this object before invoking
   !! `preprocess(...)`.
   !!
   !! @ingroup group_global
   type(global_settings), public :: global

end module

!>>>>> ././src/define.f90
!> @file
!! @defgroup group_define Define
!! Macro definition and removal directives for the fpx preprocessor.
!!
!! This module implements the `#define` and `#undef` directives used to create,
!! update, and remove preprocessor macros during source preprocessing.
!!
!! Supported macro forms include:
!!
!! - Object-like macros:
!!   `#define NAME value`
!!
!! - Function-like macros:
!!   `#define NAME(arg1,arg2,...) replacement`
!!
!! - Variadic macros:
!!   `#define LOG(level, ...) ...`
!!
!! - Empty definitions:
!!   `#define FEATURE`
!!
!! - Macro redefinition:
!!   Existing definitions are replaced by the most recent one.
!!
!! The parser correctly identifies matching parentheses in function-like macro
!! signatures, allowing nested parentheses inside parameter lists. Whitespace
!! surrounding parameters is ignored, and variadic arguments are detected
!! automatically through the `...` notation.
!!
!! The module also implements `#undef`, allowing previously defined symbols
!! to be removed from the active macro table. Symbols listed in
!! `global%undef` are protected from redefinition and silently ignored.
!!
!! All syntax errors are reported through the diagnostic framework, providing
!! source locations and explanatory messages.
!!
!! @note
!! Macro definitions are local to the current preprocessing context unless
!! explicitly propagated by the caller.
!!
!! @section define_examples Examples
!!
!! 1. Object-like macros:
!! @code{.f90}
!!    #define PI        3.141592653589793
!!    #define DEBUG     1
!!    #define VERSION   "1.2.0"
!! ...
!! @endcode
!!
!! 2. Empty definitions:
!! @code{.f90}
!!    #define USE_MPI
!!
!!    #ifdef USE_MPI
!!       !...
!!    #endif
!! ...
!! @endcode
!!
!! 3. Function-like macros:
!! @code{.f90}
!!    #define SQR(x)        ((x)*(x))
!!    #define MIN(a,b)      ((a)<(b)?(a):(b))
!!    #define CONCAT(a,b)   a ## b
!! ...
!! @endcode
!!
!! 4. Variadic macros:
!! @code{.f90}
!!    #define LOG(level, ...) &
!!        print *, "[", level, "]", __VA_ARGS__
!! @endcode
!!
!! 5. Removing a definition:
!! @code{.f90}
!!    #undef DEBUG
!!
!!    #ifdef DEBUG
!!       ! This block is skipped
!!    #endif
!! ...
!! @endcode
!!
!! 6. Redefinition:
!! @code{.f90}
!!    #define SIZE 128
!!    #define SIZE 256
!!
!!    integer :: buf(SIZE)   ! expands to 256
!! ...
!! @endcode
!!
!! 7. Reserved names:
!! @code{.f90}
!!    #define defined(x) 1
!! ...
!! @endcode
!!
!! produces a diagnostic because `defined` is reserved for conditional
!! expressions.
!!
!! @see
!! <a href="./group__group__macro.html">macro</a> @n
!! <a href="./group__group__global.html">global</a> @n
!! <a href="./group__group__context.html">context</a>
module fpx_define
   use fpx_constants
   use fpx_logging
   use fpx_macro
   use fpx_string
   use fpx_global
   use fpx_context

   implicit none; private

   public :: handle_define, &
      handle_undef

contains

   !> Process a `#define` directive.
   !!
   !! Parses the directive contained in the supplied context and updates the
   !! active macro table accordingly.
   !!
   !! The routine automatically distinguishes between:
   !!
   !! - object-like macros,
   !! - function-like macros,
   !! - variadic macros using `...`,
   !! - empty definitions.
   !!
   !! Function-like signatures are parsed using matching-parenthesis tracking,
   !! ensuring that the closing parenthesis corresponding to the opening `(`
   !! is located correctly even in the presence of nested parentheses.
   !!
   !! Existing definitions are overwritten. Symbols listed in
   !! `global%undef` are ignored. Attempts to define the reserved identifier
   !! `defined` generate an error diagnostic.
   !!
   !! @param[in]    ctx
   !!   Source context containing the complete `#define` directive.
   !! @param[inout] macros
   !!   Active macro table updated in place.
   !! @param[in]    token
   !!   Directive keyword, typically `"define"`.
   !!
   !! @ingroup group_define
   subroutine handle_define(ctx, macros, token)
      type(context), intent(in)                   :: ctx
      type(macro), allocatable, intent(inout)     :: macros(:)
      character(*), intent(in)                    :: token
      !private
      character(:), allocatable :: val, name, temp
      integer :: pos, paren_start, paren_end, i, npar, imacro, level

      pos = index(lowercase(ctx%content), token) + len(token)
      temp = trim(adjustl(ctx%content(pos + 1:)))

      paren_start = index(temp, '(')
      pos = index(temp, ' ')
      if (pos > 0 .and. pos < paren_start) paren_start = 0

      if (paren_start > 0) then
         name = trim(temp(:paren_start - 1))

         if (global%undef .contains. name) return
         paren_end = 0; level = 0
         do i = paren_start, len_trim(temp)
            select case (temp(i:i))
             case ('(')
               level = level + 1
             case (')')
               level = level - 1
               if (level == 0) then
                  paren_end = i
                  exit
               end if
            end select
         end do
         if (paren_end == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Syntax error', &
               label=label_type('Missing closing parenthesis in macro definition', len_trim(ctx%content) + 1, 1), &
               source=ctx%path), &
               trim(ctx%content), ctx%line))
            return
         end if
         val = trim(adjustl(temp(paren_end + 1:)))
         temp = temp(paren_start + 1:paren_end - 1)
         npar = 0
         pos = 1
         do while (pos <= len_trim(temp))
            if (temp(pos:pos) == ',') then
               npar = npar + 1
            end if
            pos = pos + 1
         end do
         if (len_trim(temp) > 0) npar = npar + 1

         if (.not. allocated(macros)) allocate(macros(0))

         if (name == 'defined') then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Reserved macro name', &
               label=label_type('"defined" cannot be used as a macro name', paren_start + 1, len(name)), &
               source=ctx%path), &
               trim(ctx%content), ctx%line))
         end if

         if (.not. is_defined(name, macros, imacro)) then
            call add(macros, name, val)
            imacro = size_of(macros)
         else
            macros(imacro) = macro(name, val)
         end if

         if (index(temp, '...') > 0) then
            macros(imacro)%is_variadic = .true.
            npar = npar - 1
            if (allocated(macros(imacro)%params)) deallocate(macros(imacro)%params)
            allocate(macros(imacro)%params(npar))
            pos = 1
            i = 1
            do while (pos <= len_trim(temp) .and. i <= npar)
               do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                  pos = pos + 1
               end do
               if (pos > len_trim(temp)) exit
               paren_start = pos
               do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',')
                  pos = pos + 1
               end do
               macros(imacro)%params(i) = temp(paren_start:pos - 1)
               i = i + 1
               pos = pos + 1
            end do
         else
            macros(imacro)%is_variadic = .false.
            if (allocated(macros(imacro)%params)) deallocate(macros(imacro)%params)
            allocate(macros(imacro)%params(npar))
            pos = 1
            i = 1
            do while (pos <= len_trim(temp) .and. i <= npar)
               do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                  pos = pos + 1
               end do
               if (pos > len_trim(temp)) exit
               paren_start = pos
               do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',' .and. temp(pos:pos) /= ' ')
                  pos = pos + 1
                  if (pos > len_trim(temp)) exit
               end do
               macros(imacro)%params(i) = temp(paren_start:pos - 1)
               i = i + 1
               if (pos <= len_trim(temp)) then
                  if (temp(pos:pos) == ',') pos = pos + 1
               end if
            end do
         end if
      else
         pos = index(temp, ' ')
         if (pos > 0) then
            name = trim(temp(:pos - 1))
            val = trim(adjustl(temp(pos + 1:)))
         else
            name = trim(temp)
            val = ''
         end if

         if (global%undef .contains. name) return
         if (.not. allocated(macros)) allocate(macros(0))
         if (.not. is_defined(name, macros, imacro)) then
            call add(macros, name, val)
            imacro = size_of(macros)
         else
            macros(imacro) = macro(name, val)
         end if
      end if
   end subroutine

   !> Process a `#undef` directive.
   !!
   !! Removes the specified macro from the active macro table.
   !! If the requested symbol is not currently defined, a warning
   !! diagnostic is emitted.
   !!
   !! @param[in]    ctx
   !!   Source context containing the complete `#undef` directive.
   !! @param[inout] macros
   !!   Active macro table updated in place.
   !! @param[in]    token
   !!   Directive keyword, typically `"undef"`.
   !!
   !! @ingroup group_define
   subroutine handle_undef(ctx, macros, token)
      type(context), intent(in)                   :: ctx
      type(macro), allocatable, intent(inout)     :: macros(:)
      character(*), intent(in)                    :: token
      !private
      character(:), allocatable :: name
      integer :: i, n, pos

      n = size_of(macros)
      pos = index(lowercase(ctx%content), token) + len(token)
      name = trim(adjustl(ctx%content(pos:)))
      do i = 1, n
         if (macros(i) == name) then
            call remove(macros, i)
            exit
         end if
      end do

      if (i > n) then
         call printf(render(diagnostic_report(LEVEL_WARNING, &
            message='Unknown macro', &
            label=label_type(name // ' not found', pos, len(name)), &
            source=ctx%path), &
            trim(ctx%content)))
      end if
   end subroutine
end module

!>>>>> ././src/diagnostics.f90
!> @file
!! @defgroup group_diagnostics Diagnostics
!! Diagnostic directives for the fpx preprocessor.
!!
!! This module implements the handling of the preprocessor directives
!! `#error` and `#warning`, allowing source files to emit user-defined
!! diagnostics during preprocessing.
!!
!! These directives are commonly used to enforce configuration requirements,
!! reject unsupported platforms, report deprecated features, or notify users
!! about assumptions made during compilation.
!!
!! Supported directives:
!!
!! - `#error`
!!   Emits a fatal diagnostic and immediately terminates preprocessing.
!!
!! - `#warning`
!!   Emits a non-fatal diagnostic message while allowing preprocessing to
!!   continue normally.
!!
!! The diagnostic text consists of the remainder of the directive line
!! following the keyword itself.
!!
!! @note
!! The routines implemented in this module do not perform macro expansion on
!! the diagnostic message. Any expansion must have been completed before the
!! directive handler is invoked.
!!
!! @section diagnostics_examples Examples
!!
!! 1. Reject unsupported platforms:
!! @code{.f90}
!!    #ifdef __VAX__
!!    #error "VAX systems are not supported."
!!    #endif
!! ...
!! @endcode
!!
!! 2. Enforce configuration requirements:
!! @code{.f90}
!!    #ifndef MPI_VERSION
!!    #error "MPI support must be enabled."
!!    #endif
!! ...
!! @endcode
!!
!! 3. Warn about deprecated functionality:
!! @code{.f90}
!!    #ifdef USE_LEGACY_SOLVER
!!    #warning "USE_LEGACY_SOLVER is deprecated and will be removed."
!!    #endif
!! ...
!! @endcode
!!
!! 4. Notify users of unusual configurations:
!! @code{.f90}
!!    #if PRECISION > 64
!!    #warning "Using extended precision may affect performance."
!!    #endif
!! ...
!! @endcode
!!
!! 5. Emit custom informational messages:
!! @code{.f90}
!!    #warning "Building experimental version."
!! ...
!! @endcode
!!
!! @see
!! <a href="./group__group__logging.html">logging</a> @n
!! <a href="./group__group__context.html">context</a>
module fpx_diagnostics
   use, intrinsic :: iso_fortran_env, only: stdout => output_unit
   use fpx_logging
   use fpx_macro
   use fpx_global
   use fpx_string
   use fpx_context

   implicit none; private

   public :: handle_error, &
      handle_warning

contains

   !> Process a `#error` directive.
   !!
   !! Extracts the message following the directive keyword and immediately
   !! terminates preprocessing using an `error stop` statement.
   !!
   !! This directive is intended for unrecoverable situations such as
   !! unsupported targets, invalid configurations, or missing prerequisites.
   !!
   !! @param[in]    ctx
   !!   Source context containing the complete `#error` directive.
   !! @param[inout] macros
   !!   Active macro table. Present for interface consistency and not modified.
   !! @param[in]    token
   !!   Directive keyword, typically `"error"`.
   !!
   !! @ingroup group_diagnostics
   subroutine handle_error(ctx, macros, token)
      type(context), intent(in)                   :: ctx
      type(macro), allocatable, intent(inout)     :: macros(:)
      character(*), intent(in)                    :: token
      !private
      integer :: pos

      pos = index(lowercase(ctx%content), token) + len(token)
      error stop trim(adjustl(ctx%content(pos + 1:)))
   end subroutine

   !> Process a `#warning` directive.
   !!
   !! Extracts the message following the directive keyword and writes it to
   !! the standard output stream without interrupting preprocessing.
   !!
   !! This directive is intended for non-fatal conditions such as deprecated
   !! features, unusual build settings, or informational notices.
   !!
   !! @param[in]    ctx
   !!   Source context containing the complete `#warning` directive.
   !! @param[inout] macros
   !!   Active macro table. Present for interface consistency and not modified.
   !! @param[in]    token
   !!   Directive keyword, typically `"warning"`.
   !!
   !! @ingroup group_diagnostics
   subroutine handle_warning(ctx, macros, token)
      type(context), intent(in)                   :: ctx
      type(macro), allocatable, intent(inout)     :: macros(:)
      character(*), intent(in)                    :: token
      !private
      integer :: pos

      pos = index(lowercase(ctx%content), token) + len(token)
      write(stdout, '(A)') trim(adjustl(ctx%content(pos + 1:)))
   end subroutine
end module

!>>>>> ././src/include.f90
!> @file
!! @defgroup group_include Include
!! Include file handling and resolution for the fpx Fortran preprocessor
!!
!! This module implements robust and standard-compliant processing of `#include` directives
!! with full support for:
!! - Both forms: `#include "file.h"` (local/user) and `#include <file.h>` (system)
!! - Proper search order: quotes search source dir first, angle brackets skip source dir
!! - Relative paths resolved against the directory of the parent source file
!! - Search in user-defined include directories (`global%includedir`)
!! - Search in system INCLUDE environment variable directories
!! - Fallback to current working directory
!! - Proper error reporting with file name and line number context
!! - Recursion safety through integration with the main preprocessor loop
!! - Seamless integration via the abstract `preprocess` procedure pointer
!!
!! The routine correctly strips quotes or angle brackets, performs path resolution,
!! checks file existence, opens the file, and recursively invokes the main preprocessing
!! engine on the included content using the same macro environment.
!!
!! @note
!! For `#include "file"`:
!! 1. Directory of the parent source file
!! 2. Directories specified by the -I option (`global%includedir`)
!! 3. Directories in INCLUDE environment variable
!! 4. Current working directory
!!
!! @note
!! For `#include <file>`:
!! 1. Directories specified by the -I option (`global%includedir`)
!! 2. Directories in INCLUDE environment variable
!! 3. Current working directory
!!
!! @note
!! Nested includes are supported. Relative paths inside included
!! files are resolved relative to the directory containing the
!! including file.
!!
!! @section include_examples Examples
!!
!! 1. Include a local header from the same directory using quotes:
!! @code{.f90}
!!    #include "config.h"
!!    !> fpx will look for ./config.h relative to the current source file first
!! @endcode
!!
!! 2. Include a system header using angle brackets:
!! @code{.f90}
!!    #include <stdlib.h>
!!    !> fpx will skip the source directory and search -I paths, then INCLUDE
!! @endcode
!!
!! 3. Using from the driver program (adding include paths):
!! @code{.f90}
!!    global%includedir = ['/usr/include', './include', './headers']
!!    call preprocess('main.F90', 'main.f90')
!!    !> All #include <...> will search these directories in order
!! @endcode
!!
!! 4. Verbose error reporting when a file is not found:
!! @code{.txt}
!!    $ fpx -v src/utils.F90
!!    Error: Cannot find include file 'missing.h' at src/utils.F90:27
!! @endcode
module fpx_include
   use iso_fortran_env, only : iostat_end
   use fpx_constants
   use fpx_logging
   use fpx_path
   use fpx_string
   use fpx_macro
   use fpx_global
   use fpx_context

   implicit none; private

   public :: handle_include

   ! Include directive types
   integer, parameter, private :: INCLUDE_TYPE_SYSTEM = 1  ! < >
   integer, parameter, private :: INCLUDE_TYPE_LOCAL = 2  ! " "
#ifdef _WIN32
   integer, parameter, private :: MAX_PATH_LEN = 256
#else
   integer, parameter, private :: MAX_PATH_LEN = 4096
#endif

contains

   !> Process a #include directive encountered during preprocessing
   !! Resolves the include file name (quoted or angle-bracketed), searches for the file
   !! using standard C preprocessor rules:
   !! - Quoted includes search: parent directory, -I paths, INCLUDE, cwd
   !! - Angle bracket includes search: -I paths, INCLUDE, cwd (skips parent directory)
   !! Opens the file and recursively preprocesses its contents into the output unit.
   !!
   !! @param[in] ctx          Context line containing the #include directive
   !! @param[in] ounit        Output unit where preprocessed content is written
   !! @param[in] preprocess   Procedure pointer to the main line-by-line preprocessor
   !! @param[inout] macros    Current macro table (shared across recursion levels)
   !! @param[in] token        Usually 'include' - the directive keyword
   !!
   !! @ingroup group_include
   recursive subroutine handle_include(ctx, ounit, preprocess, macros, token)
      type(context), intent(in)               :: ctx
      integer, intent(in)                     :: ounit
      procedure(read_unit)                    :: preprocess
      type(macro), allocatable, intent(inout) :: macros(:)
      character(*), intent(in)                :: token
      !private
      type(string), allocatable, save :: include_stack(:)
      character(:), allocatable :: include_file
      character(:), allocatable :: dir, ifile
      integer :: i, iunit, ierr, pos, include_type, closing
      logical :: exists

      ! Extract the directory of the parent file
      dir = dirpath(ctx%path)
      ! Find the position after the #include token
      pos = index(lowercase(ctx%content), token) + len(token)
      include_file = trim(adjustl(ctx%content(pos:)))

      ! Determine include type and extract filename
      if (include_file(1:1) == '"') then
         include_type = INCLUDE_TYPE_LOCAL
         closing = index(include_file(2:), '"')
         if (closing == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Malformed #include directive', &
               label=label_type('Missing closing quotation mark', &
               index(ctx%content,'"'),1), &
               source=trim(ctx%path)), &
               ctx%content, ctx%line))
            return
         end if
         include_file = include_file(2:closing)
      else if (include_file(1:1) == '<') then
         include_type = INCLUDE_TYPE_SYSTEM
         closing = index(include_file(2:), '>')
         if (closing == 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Malformed #include directive', &
               label=label_type('Missing closing quotation mark', &
               index(ctx%content,'"'),1), &
               source=trim(ctx%path)), &
               ctx%content, ctx%line))
            return
         end if
         include_file = include_file(2:closing)
      else
         ! Malformed include directive
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Malformed #include directive', &
            label=label_type('Filepath should either be delimited by "<...>" or "..."', index(ctx%content, include_file), &
            len(include_file)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      ! Handle absolute/rooted paths (same for both types)
      ifile = include_file
      if (is_rooted(ifile)) then
         inquire(file=ifile, exist=exists)
         if (exists) then
            include_file = ifile
         else
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='File not found', &
               label=label_type('Cannot find include file ' // trim(include_file), index(ctx%content, include_file), &
               len(include_file)), &
               source=trim(ctx%path)), &
               ctx%content, ctx%line))
            return
         end if
      else
         ! Relative path - search according to include type
         exists = .false.
         ! For quoted includes (#include "file"), search parent directory first
         if (include_type == INCLUDE_TYPE_LOCAL) then
            ifile = join(dir, include_file)
            inquire(file=ifile, exist=exists)
            if (exists) then
               include_file = ifile
            end if
         end if

         ! If not found yet, search user-specified include directories (-I paths)
         if (.not. exists .and. allocated(global%includedir)) then
            do i = 1, size(global%includedir)
               ifile = join(global%includedir(i), include_file)
               inquire(file=ifile, exist=exists)
               if (exists) then
                  include_file = ifile
                  exit
               end if
            end do
         end if

         ! If still not found, try the INCLUDE environmental variable
         if (.not. exists) then
            block
               character(:), allocatable :: ipaths(:)

               ipaths = get_system_paths()
               do i = 1, size(ipaths)
                  ifile = join(ipaths(i), include_file)
                  inquire(file=ifile, exist=exists)
                  if (exists) then
                     include_file = ifile
                  end if
               end do
            end block
         end if

         ! If still not found, try current working directory as last resort
         if (.not. exists) then
            ifile = join(cwd(), include_file)
            inquire(file=ifile, exist=exists)
            if (exists) then
               include_file = ifile
            end if
         end if

         ! If file was not found anywhere, report error
         if (.not. exists) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='File not found', &
               label=label_type('Cannot find include file ' // trim(include_file), index(ctx%content, include_file), len(&
               include_file)), &
               source=trim(ctx%path)), &
               ctx%content, ctx%line))
            return
         end if
      end if

      if (.not. allocated(include_stack)) allocate(include_stack(0))

      if (include_stack .contains. include_file) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Recursive include detected', &
            label=label_type('File already included in current include chain', &
            index(ctx%content, trim(include_file)), &
            len_trim(include_file)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      ! Open and preprocess the include file
      open(newunit=iunit, file=include_file, status='old', action='read', iostat=ierr)
      if (ierr /= 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='File not found', &
            label=label_type('Cannot open include file ' // trim(include_file), index(ctx%content, include_file), len(&
            include_file)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      include_stack = [include_stack, string(include_file)]

      call preprocess(iunit, ounit, macros, .true.)
      close(iunit)
      if (size(include_stack) > 1) then
         include_stack = include_stack(:size(include_stack)-1)
      else
         deallocate(include_stack)
      end if
   end subroutine

   !> Get system include paths from INCLUDE environment variable
   !! Returns an array of directory paths found in INCLUDE
   !! @return Array of path strings, empty if INCLUDE not set
   !!
   !! @ingroup group_include
   function get_system_paths() result(paths)
      character(:), allocatable :: paths(:)
      !private
      character(:), allocatable :: path_env, tmp(:)
      integer :: lpath, i, n_paths, start_pos, end_pos, count
      character(len=1) :: path_sep

#ifdef _WIN32
      path_sep = ';'  ! Windows path separator
#else
      path_sep = ':'  ! Unix/Linux/Mac path separator
#endif

      ! Get INCLUDE environment variable length
      call get_environment_variable('INCLUDE', length=lpath)
      if (lpath <= 0) then
         allocate(character(len=0) :: paths(0)); return
      end if

      ! Allocate and retrieve INCLUDE value
      allocate(character(len=lpath) :: path_env)
      call get_environment_variable('INCLUDE', value=path_env)

      ! Count number of paths (number of separators + 1)
      n_paths = 1
      do i = 1, len(path_env)
         if (path_env(i:i) == path_sep) n_paths = n_paths + 1
      end do

      ! Allocate temporary array with maximum size
      allocate(character(len=MAX_PATH_LEN) :: tmp(n_paths))

      ! Split INCLUDE into individual directories
      count = 0
      start_pos = 1
      do i = 1, len(path_env) + 1
         if (i > len(path_env) .or. path_env(i:i) == path_sep) then
            if (i > len(path_env)) then
               end_pos = i - 1
            else
               end_pos = i - 1
            end if

            if (end_pos >= start_pos) then
               count = count + 1
               tmp(count) = trim(adjustl(path_env(start_pos:end_pos)))
            end if
            start_pos = i + 1
         end if
      end do

      ! Allocate result array with actual count
      if (count > 0) then
         allocate(character(len=MAX_PATH_LEN) :: paths(count))
         paths(:) = tmp(1:count)
      else
         allocate(character(len=0) :: paths(0))
      end if
   end function

end module

!>>>>> ././src/loop.f90
!> @file
!! @defgroup group_for For
!! Fortran Preprocessor (fpx) - compile-time loop expansion support
!!
!! This module implements the non-standard `#for` / `#endfor` directive pair
!! used by fpx to generate repeated source code from a list of values.
!!
!! Features:
!! - Simple iteration over explicit lists:
!!   `#for T in [integer, real, complex]`
!! - Iteration over macro-expanded lists:
!!   `#define NUMERICS [integer, real, complex]`
!!   `#for T in NUMERICS`
!! - Arbitrary nesting of `#for` blocks
!! - Integration with the normal macro expansion engine
!! - Deferred body collection until matching `#endfor`
!! - Automatic cleanup of loop-local variables
!!
!! During parsing, loop bodies are stored internally and emitted only when
!! the matching `#endfor` is encountered. Each iteration temporarily defines
!! the loop variable as a macro whose value is substituted into the collected
!! body before output.
!!
!! @section for_examples Examples
!!
!! 1. Basic iteration:
!! @code{.f90}
!!    #for T in [integer, real, complex]
!!       type(T) :: value
!!    #endfor
!!
!!    ! Expands to:
!!    type(integer) :: value
!!    type(real)    :: value
!!    type(complex) :: value
!! ...
!! @endcode
!!
!! 2. Using a macro list:
!! @code{.f90}
!!    #define NUMERICS [integer, real, complex]
!!
!!    #for T in NUMERICS
!!       type(T) :: value
!!    #endfor
!! ...
!! @endcode
!!
!! 3. Nested loops:
!! @code{.f90}
!!    #define CONCAT(a,b) a##b
!!    #for T in [integer, real]
!!    #for R in [32,64]
!!       type(CONCAT(T,R)) :: value
!!    #endfor
!!    #endfor
!! ...
!! @endcode
!!
!! 4. Generic procedure generation:
!! @code{.f90}
!!    #define CONCAT(a,b) a##b
!!    #define NUMERICS [integer, real, complex]
!!
!!    #for T in NUMERICS
!!       module procedure CONCAT(add_,T)
!!    #endfor
!! ...
!! @endcode
!!
!! 5. Cartesian product generation:
!! @code{.f90}
!!    #for T in [real, complex]
!!    #for K in [32, 64]
!!       type(T(K)) :: value
!!    #endfor
!!    #endfor
!!
!!    ! Generates:
!!    ! type(real(32))    :: value
!!    ! type(real(64))    :: value
!!    ! type(complex(32)) :: value
!!    ! type(complex(64)) :: value
!! ...
!! @endcode
!!
!! Loop variables behave exactly like temporary object-like macros and
!! therefore participate in all normal macro expansion rules, including
!! nested expansion and token pasting.
!!
!! @note
!! When nested loops are active, generated lines are appended to the
!! enclosing loop body rather than written immediately. This guarantees
!! inside-out expansion semantics.
module fpx_for
   use, intrinsic :: iso_c_binding, only: c_funptr, c_f_procpointer
   use fpx_constants
   use fpx_logging
   use fpx_macro
   use fpx_string
   use fpx_global
   use fpx_context

   implicit none; private

   public :: handle_for, &
      handle_endfor, &
      is_in_forloop, &
      add_to_loop

   !> Internal storage for deferred loop bodies.
   !!
   !! Source lines belonging to a `#for` block are collected until the
   !! corresponding `#endfor` directive is encountered.
   !!
   !! @ingroup group_for
   type :: body
      integer :: nlines = 0
      type(string), allocatable :: lines(:)
   end type

   !! @cond
   integer :: depth = 0
   integer, parameter :: BODY_BUFFER = 50
   type(body) :: bodies(MAX_FOR_DEPTH)
   type(macro), allocatable :: fmacros(:)
   !! @endcond

contains

   !> Process a `#for` directive and initialize a new loop context.
   !!
   !! The directive header is parsed immediately, but the loop body is
   !! not expanded at this stage. Instead, subsequent source lines are
   !! collected until the matching `#endfor` directive is encountered.
   !!
   !! The loop variable behaves as a temporary object-like macro whose
   !! value changes for each iteration.
   !!
   !! Supported syntax:
   !!
   !! @code
   !! #for identifier in [value1, value2, ...]
   !! #for identifier in MACRO_NAME
   !! @endcode
   !!
   !! where `MACRO_NAME` expands to a bracketed list.
   !!
   !! @note
   !! `#for` and `#endfor` are fpx extensions and are not part of the
   !! ISO C preprocessor specification.
   !!
   !! @param[in]    ctx
   !!   Current parsing context
   !! @param[inout] macros
   !!   Active macro table
   !! @param[in]    token
   !!   Directive keyword (`for`)
   !!
   !! @ingroup group_for
   subroutine handle_for(ctx, macros, token)
      type(context), intent(in)                   :: ctx
      type(macro), allocatable, intent(inout)     :: macros(:)
      character(*), intent(in)                    :: token
      !private
      character(:), allocatable :: val, name, temp
      integer :: pos, paren_start, paren_end, i, npar, imacro
      logical :: stitch

      depth = depth + 1
      if (depth > MAX_FOR_DEPTH) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Loop nesting too deep', &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      temp = trim(adjustl(ctx%content(pos + 1:)))

      if (index(temp, ' in ') == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('Missing " in " keyword', pos + 1, 4), &
            source=ctx%path), &
            trim(ctx%content), ctx%line))
         return
      else
         name = trim(adjustl(temp(:index(temp, ' in '))))
         if (global%undef .contains. name) return

         if (name == 'defined') then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Reserved macro name', &
               label=label_type('"defined" cannot be used as a macro name', paren_start + 1, len(name)), &
               source=ctx%path), &
               trim(ctx%content), ctx%line))
         end if
      end if

      pos = index(temp, ' in ') + len(' in ')
      temp = expand_macros(temp(pos:), macros, stitch, global%implicit_continuation, global%support_dollar_insert, ctx)

      paren_start = index(temp, '[')
      if (paren_start == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('Missing opening square bracket in #for expression', 1, 1), &
            source=ctx%path), &
            trim(ctx%content), ctx%line))
         return
      end if

      paren_end = index(temp, ']', back=.true.)
      if (paren_end == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('Missing closing square bracket in #for expression', len_trim(ctx%content) + 1, 1), &
            source=ctx%path), &
            trim(ctx%content), ctx%line))
         return
      end if
      temp = temp(paren_start + 1:paren_end - 1)
      npar = 0
      pos = 1
      do while (pos <= len_trim(temp))
         if (temp(pos:pos) == ',') then
            npar = npar + 1
         end if
         pos = pos + 1
      end do
      if (len_trim(temp) > 0) npar = npar + 1

      if (.not. allocated(fmacros)) allocate(fmacros(0))
      if (.not. is_defined(name, fmacros, imacro)) then
         call add(fmacros, name, '')
         imacro = size_of(fmacros)
      else
         fmacros(imacro) = macro(name, '')
      end if

      fmacros(imacro)%active = .false.
      fmacros(imacro)%is_variadic = .false.
      if (allocated(fmacros(imacro)%params)) deallocate(fmacros(imacro)%params)
      allocate(fmacros(imacro)%params(npar))
      pos = 1
      i = 1
      do while (pos <= len_trim(temp) .and. i <= npar)
         do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
            pos = pos + 1
         end do
         if (pos > len_trim(temp)) exit
         paren_start = pos
         do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',' .and. temp(pos:pos) /= ' ')
            pos = pos + 1
            if (pos > len_trim(temp)) exit
         end do
         fmacros(imacro)%params(i) = temp(paren_start:pos - 1)
         i = i + 1
         if (pos <= len_trim(temp)) then
            if (temp(pos:pos) == ',') pos = pos + 1
         end if
      end do
   end subroutine

   !> Finalize a loop and emit all expanded iterations.
   !!
   !! The collected loop body is expanded once for every value contained in
   !! the loop variable parameter list. Nested loops are handled recursively
   !! by forwarding generated lines to the enclosing loop body when present.
   !!
   !! For each iteration value:
   !! - the loop variable macro is activated,
   !! - the stored body is macro-expanded,
   !! - generated lines are reprocessed by the normal preprocessing engine,
   !! - output is either emitted directly or forwarded to an enclosing loop.
   !!
   !! When the outermost loop terminates, all temporary loop state is
   !! released automatically.
   !!
   !! @param[in] ctx     Current parsing context
   !! @param[in] ounit   Output unit
   !! @param[in] p   preprocessor function pointer
   !! @param[inout] macros  Active macro table
   !! @param[in] token   Directive keyword (`endfor`)
   !!
   !! @ingroup group_for
   subroutine handle_endfor(ctx, ounit, p, macros, token)
      type(context), intent(inout)    :: ctx
      integer, intent(in)             :: ounit
      type(c_funptr), intent(in)      :: p
      type(macro), intent(in)         :: macros(:)
      character(*), intent(in)        :: token
      !private
      integer :: i, j
      character(:), allocatable :: rst, tmp
      logical :: stitch
      type(string), allocatable :: params(:)
      type(macro), allocatable :: ms(:)
      procedure(preprocess_line), pointer :: preprocess => null()

      call c_f_procpointer(p, preprocess)

      tmp = ''
      depth = depth - 1

      if (depth + 1 <= size_of(fmacros)) then
         if (allocated(fmacros(depth + 1)%params)) params = fmacros(depth + 1)%params
         if (allocated(fmacros(depth + 1)%params)) deallocate(fmacros(depth + 1)%params)

         do i = 1, size_of(params)
            fmacros(depth + 1)%value = params(i)
            fmacros(depth + 1)%active = .true.
            ms = [fmacros(depth + 1), macros]
            !do j = 1, bodies(depth + 1)%nlines
            do j = 1, bodies(depth + 1)%nlines  !size_of(bodies(depth + 1)%lines)
               if (head(bodies(depth + 1)%lines(j)%chars) == '#') then
                  if (len(bodies(depth + 1)%lines(j)%chars) == 1) then
                     return
                  else
                     rst = adjustl(expand_macros(bodies(depth + 1)%lines(j)%chars, ms, stitch, &
                        global%implicit_continuation, global%support_dollar_insert, ctx))
                     tmp = preprocess(rst, ounit, ctx%path, ctx%line, ms, stitch)
                  end if
               else
                  rst = adjustl(expand_macros(bodies(depth + 1)%lines(j)%chars, ms, stitch, global%implicit_continuation, &
                     global%support_dollar_insert, ctx))
                  tmp = preprocess(rst, ounit, ctx%path, ctx%line, ms, stitch)
               end if

               if (depth > 0) then
                  if (len_trim(tmp) > 0) then
                     call addline(bodies(depth), string(tmp))
                  end if
               else
                  do
                     if (tmp == rst) exit
                     tmp = preprocess(rst, ounit, ctx%path, ctx%line, ms, stitch)
                     rst = tmp
                  end do
                  write(ounit, '(A)') rst
               end if
            end do
            if (depth > 0) then
               call addline(bodies(depth), string(''))
            else
               write(ounit, '(A)') ''
            end if
         end do
         bodies(depth + 1)%nlines = 0
         if (allocated(bodies(depth + 1)%lines)) deallocate(bodies(depth + 1)%lines)
      end if

      if (allocated(params)) deallocate(params)
      if (allocated(ms)) deallocate(ms)
      nullify(preprocess)

      if (depth < 0) then
         call printf(render(diagnostic_report(LEVEL_WARNING, &
            message='Unbalanced #for expression. Missing #for or #endfor directive.', &
            source=ctx%path), &
            trim(ctx%content)))
         return
      end if

      if (depth == 0) then
         if (allocated(fmacros)) deallocate(fmacros)
         do i = 1, MAX_FOR_DEPTH
            if (allocated(bodies(i)%lines)) deallocate(bodies(i)%lines)
         end do
      end if
   end subroutine

   !> Append a source line to the innermost active loop body.
   !!
   !! Lines are stored verbatim without macro expansion. Expansion is
   !! deferred until the corresponding `#endfor` directive is processed.
   !!
   !! @param[in] line Source line to store
   !!
   !! @ingroup group_for
   subroutine add_to_loop(line)
      character(*), intent(in) :: line

      call addline(bodies(depth), string(line))
   end subroutine

   !> Query whether parsing is currently inside a `#for` block.
   !! This routine is typically used by the main preprocessing engine to
   !! determine whether incoming source lines should be emitted directly
   !! or collected for later expansion.
   !! @return `.true.` when one or more loop contexts are active,
   !!         `.false.` otherwise.
   !!
   !! @ingroup group_for
   logical function is_in_forloop() result(res)
      res = depth > 0
   end function

   subroutine addline(b, line)
      type(body), intent(inout)       :: b
      type(string), intent(in)        :: line
      !private
      type(string), allocatable :: tmp(:)
      integer :: n

      if (.not. allocated(b%lines)) then
         allocate(b%lines(0))
         b%nlines = 0
      end if
      b%nlines = b%nlines + 1
      n = size(b%lines)
      if (b%nlines <= n) then
         b%lines(b%nlines) = line
      else
         allocate(tmp(n + BODY_BUFFER))
         tmp(1:n) = b%lines(1:n)
         tmp(n + 1) = line
         call move_alloc(from=tmp, to=b%lines)
      end if
   end subroutine
end module

!>>>>> ././src/operators.f90
!> @file
!! @defgroup group_operators Operators
!! Module implementing a full C-preprocessor-style constant expression evaluator using a top-down recursive descent parser.
!! The module provides the ability to evaluate integer constant expressions of the kind used in
!! classical preprocessor.
!!
!! This includes support for:
!! - All C-style arithmetic, bitwise, logical, relational, and conditional operators
!! - Operator precedence and associativity
!! - Macro identifier substitution and the special `defined(identifier)` operator
!! - Integer literals in decimal, octal (`0...`), hexadecimal (`0x...`), and binary (`0b...`) bases
!! - Parenthesized sub-expressions and proper handling of unary operators
!!
!! The implementation consists of two major phases:
!!
!! 1. Tokenization
!!    The input string is scanned and converted into a sequence of @link fpx_token::token token @endlink objects.
!!    The tokenizer recognizes multi-character operators ('&&', '||', '==', '!=', '<=', '>=', '<<', '>>', '**'),
!!    the `defined` operator (with or without parentheses), numbers in all supported bases,
!!    identifiers, and parentheses. Whitespace is ignored except as a token separator.
!!
!! 2. Parsing and evaluation via top-down recursive descent
!!    A classic predictive (LL(1)) recursive descent parser is used, where each non-terminal
!!    in the grammar is implemented as a separate parsing function with the exact precedence level.
!!    The grammar is directly derived from the C standard operator precedence table:
!!
!!    parse_expression        ? parse_conditional
!!    parse_conditional       ? parse_or (parse_or '?' parse_expression ':' parse_conditional)
!!    parse_or                ? parse_and ( '||' parse_and )*
!!    parse_and               ? parse_bitwise_or ( '&&' parse_bitwise_or )*
!!    parse_bitwise_or        ? parse_bitwise_xor ( '|' parse_bitwise_xor )*
!!    parse_bitwise_xor       ? parse_bitwise_and ( '^' parse_bitwise_and )*
!!    parse_bitwise_and       ? parse_equality ( '&' parse_equality )*
!!    parse_equality          ? parse_relational ( ('==' | '!=') parse_relational )*
!!    parse_relational        ? parse_shifting ( ('<' | '>' | '<=' | '>=') parse_shifting )*
!!    parse_shifting          ? parse_additive ( ('<<' | '>>') parse_additive )*
!!    parse_additive          ? parse_multiplicative ( ('+' | '-') parse_multiplicative )*
!!    parse_multiplicative    ? parse_power ( ('*' | '/' | '%') parse_unary )*
!!    parse_unary             ? ('!' | '-' | '+' | '~') parse_unary
!!                             | parse_power
!!    parse_power             ? parse_unary ( '**' parse_unary )*          (right-associative)
!!    parse_atom              ? number
!!                             | identifier                              (macro expansion)
!!                             | 'defined' ( identifier ) | 'defined' identifier
!!                             | '(' parse_expression ')'
!!
!!    Each parsing function consumes tokens from the global position `pos` and returns the
!!    integer value of the sub-expression it recognizes. Because the grammar is factored by
!!    precedence, left-associativity is achieved naturally via left-recursive loops,
!!    while right-associativity for the power operator (`**`) is handled by calling
!!    `parse_unary` on the right-hand side first.
!!
!!    Macro expansion occurs lazily inside `parse_atom` when an identifier token is encountered:
!!    - If the identifier is defined, its replacement text is recursively evaluated.
!!    - The special `defined` operator yields 1 or 0 depending on whether the identifier exists.
!!
!!    The parser is fully re-entrant and has no global state.
!!
!! Public entry points:
!! - @link fpx_operators::evaluate_expression evaluate_expression @endlink:
!!   tokenize and evaluate an expression in one call.
!! - @link fpx_operators::parse_expression parse_expression @endlink:
!!   evaluate an already-tokenized expression.
!!
!!    This design guarantees correct operator precedence without the need for an explicit
!!    abstract syntax tree or stack-based shunting-yard algorithm, while remaining easy to
!!    read, maintain, and extend.
!!
!! @section operator_examples Examples
!!
!! 1. Evaluate a simple expression:
!! @code{.f90}
!!    logical :: ok
!!    integer :: value
!!
!!    ok = evaluate_expression('1 + 2 * 3', macros, value)
!!
!!    ! value = 7
!! ...
!! @endcode
!!
!! 2. Use the `defined` operator:
!! @code{.f90}
!!    call add(macros, 'DEBUG')
!!
!!    if (evaluate_expression('defined(DEBUG)', macros)) then
!!       ...
!!    end if
!! ...
!! @endcode
!!
!! 3. Evaluate expressions involving macros:
!! @code{.f90}
!!    call add(macros, 'LEVEL', '2')
!!
!!    if (evaluate_expression('LEVEL >= 2', macros)) then
!!       ...
!!    end if
!! ...
!! @endcode
!!
!! 4. Conditional operator:
!! @code{.f90}
!!    call evaluate_expression('1 ? 10 : 20', macros, value)
!!    ! value = 10
!! ...
!! @endcode
module fpx_operators
   use fpx_global
   use fpx_string
   use fpx_constants
   use fpx_macro
   use fpx_logging
   use fpx_token
   use fpx_context

   implicit none; private

   public :: evaluate_expression, &
      parse_expression

   !> Evaluates a preprocessor-style expression with macro substitution.
   !! Tokenizes the input expression, expands macros where appropriate,
   !! parses it according to operator precedence, and computes the integer result.
   !! Returns .true. if evaluation succeeded and the result is non-zero.
   !!
   !! @section evaluate_expression_examples Examples
   !!
   !! @code{.f90}
   !!    integer :: value
   !!
   !!    call add(macros, 'SIZE', '64')
   !!
   !!    if (evaluate_expression('SIZE >= 32', macros, value)) then
   !!       print *, value      ! 1
   !!    end if
   !! ...
   !! @endcode
   !!
   !! @section evaluate_expression_constructors Constructors
   !!
   !! @b Constructor
   !! @code{.f90}
   !! logical function evaluate(expr, macros, val)
   !! @endcode
   !!
   !! @param[in] expr
   !!   Expression string to evaluate
   !! @param[inout] macros
   !!   Array of defined macros for substitution and `defined()` checks
   !! @param[out] val
   !!   (optional) integer result of the evaluation
   !! @return .true. if the expression evaluated successfully to non-zero, .false. otherwise
   !!
   !! @b Constructor
   !! @code{.f90}
   !! logical function evaluate(expr, macros, ctx, val)
   !! @endcode
   !!
   !! @param[in] expr
   !!   Expression string to evaluate
   !! @param[inout] macros
   !!   Array of defined macros for substitution and `defined()` checks
   !! @param[in] ctx
   !!   Current context
   !! @param[out] val
   !!   (optional) integer result of the evaluation
   !! @return .true. if the expression evaluated successfully to non-zero, .false. otherwise
   !!
   !! @ingroup group_operators
   interface evaluate_expression
      module procedure :: evaluate_expression_default
      module procedure :: evaluate_expression_with_context
   end interface

contains

   !> Evaluates a preprocessor-style expression with macro substitution.
   !! Tokenizes the input expression, expands macros where appropriate,
   !! parses it according to operator precedence, and computes the integer result.
   !! Returns .true. if evaluation succeeded and the result is non-zero.
   !!
   !! @param[in] expr   Expression string to evaluate
   !! @param[inout] macros Array of defined macros for substitution and `defined()` checks
   !! @param[out] val   (optional) integer result of the evaluation
   !! @return .true. if the expression evaluated successfully to non-zero, .false. otherwise
   !!
   !! @ingroup group_operators
   logical function evaluate_expression_default(expr, macros, val) result(res)
      character(*), intent(in)                :: expr
      type(macro), allocatable, intent(inout) :: macros(:)
      integer, intent(out), optional          :: val
      !private
      type(context) :: ctx

      ctx = context(expr, 1, '')
      res = evaluate_expression(expr, macros, ctx, val)
   end function

   !> Evaluates a preprocessor-style expression with macro substitution.
   !! Tokenizes the input expression, expands macros where appropriate,
   !! parses it according to operator precedence, and computes the integer result.
   !! Returns .true. if evaluation succeeded and the result is non-zero.
   !!
   !! @param[in] expr   Expression string to evaluate
   !! @param[inout] macros Array of defined macros for substitution and `defined()` checks
   !! @param[in] ctx Context
   !! @param[out] val   (optional) integer result of the evaluation
   !! @return .true. if the expression evaluated successfully to non-zero, .false. otherwise
   !!
   !! @ingroup group_operators
   logical function evaluate_expression_with_context(expr, macros, ctx, val) result(res)
      character(*), intent(in)                :: expr
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      integer, intent(out), optional          :: val
      !private
      type(token), allocatable :: tokens(:)
      integer :: ntokens, pos, result

      call tokenize(expr, tokens, ntokens)
      if (ntokens == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Tokenization failed', &
            label=label_type('No tokens found', 1, len_trim(expr)), &
            source=trim(ctx%path)), &
            expr, ctx%line))
         res = .false.
         return
      end if

      pos = 1
      result = parse_expression(expr, tokens, ntokens, pos, macros, ctx)
      if (pos <= ntokens) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Tokenization failed', &
            label=label_type('Extra tokens found', tokens(pos)%start, len_trim(tokens(pos)%value)), &
            source=trim(ctx%path)), &
            expr, ctx%line))
         res = .false.
         return
      end if
      res = (result /= 0)
      if (present(val)) val = result
   end function

   !! @name Expression parsing hierarchy
   !! @{
   !> Parse and evaluate an already-tokenized expression.
   !!
   !! This routine implements the top-level non-terminal of the recursive
   !! descent parser. It is primarily intended for internal use by
   !! `evaluate_expression`, but remains public to allow external users to
   !! reuse the parser on custom token streams.
   !!
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_expression(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx

      val = parse_conditional(expr, tokens, ntokens, pos, macros, ctx)
   end function

   !> Parses conditional expressions (?:). Right-associative.
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_conditional(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: condition, true_val, false_val

      ! First parse condition at higher precedence
      condition = parse_or(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = condition
         return
      end if
      ! Check for '?'
      if (pos <= ntokens .and. tokens(pos)%value == '?') then
         pos = pos + 1
         ! Parse true expression (full expression allowed)
         true_val = parse_expression(expr, tokens, ntokens, pos, macros, ctx)

         ! Expect ':'
         if (pos > ntokens .or. tokens(pos)%value /= ':') then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Syntax error', &
               label=label_type('Expected ":" in conditional expression', 1, len(expr)), &
               source=trim(ctx%path)), &
               expr, ctx%line))
            val = 0
            return
         end if

         pos = pos + 1

         ! Parse false expression (right-associative)
         false_val = parse_conditional(expr, tokens, ntokens, pos, macros, ctx)

         ! Evaluate condition
         val = merge(true_val, false_val, condition /= 0)
      else
         val = condition
      end if

   end function

   !> Parses logical OR expressions (`||`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_or(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left

      left = parse_and(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. tokens(pos)%value == '||')
         pos = pos + 1
         val = merge(1, 0, left /= 0 .or. parse_and(expr, tokens, ntokens, pos, macros, ctx) /= 0)
         left = val
      end do
      val = left
   end function

   !> Parses logical AND expressions (`&&`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_and(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left

      left = parse_bitwise_or(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. tokens(pos)%value == '&&')
         pos = pos + 1
         val = merge(1, 0, left /= 0 .and. parse_bitwise_or(expr, tokens, ntokens, pos, macros, ctx) /= 0)
         left = val
      end do
      val = left
   end function

   !> Parses bitwise OR expressions (`|`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_bitwise_or(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left

      left = parse_bitwise_xor(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. tokens(pos)%value == '|')
         pos = pos + 1
         val = parse_bitwise_xor(expr, tokens, ntokens, pos, macros, ctx)
         left = ior(left, val)
      end do
      val = left
   end function

   !> Parses bitwise XOR expressions (`^`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_bitwise_xor(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left

      left = parse_bitwise_and(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. tokens(pos)%value == '^')
         pos = pos + 1
         val = parse_bitwise_and(expr, tokens, ntokens, pos, macros, ctx)
         left = ieor(left, val)
      end do
      val = left
   end function

   !> Parses bitwise AND expressions (`&`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_bitwise_and(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left

      left = parse_equality(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. tokens(pos)%value == '&')
         pos = pos + 1
         val = parse_equality(expr, tokens, ntokens, pos, macros, ctx)
         left = iand(left, val)
      end do
      val = left
   end function

   !> Parses equality/inequality expressions (`==`, `!=`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_equality(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left, right

      left = parse_relational(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. (tokens(pos)%value == '==' .or. tokens(pos)%value == '!='))
         if (tokens(pos)%value == '==') then
            pos = pos + 1
            right = parse_relational(expr, tokens, ntokens, pos, macros, ctx)
            val = merge(1, 0, left == right)
         else
            pos = pos + 1
            right = parse_relational(expr, tokens, ntokens, pos, macros, ctx)
            val = merge(1, 0, left /= right)
         end if
         left = val
      end do
      val = left
   end function

   !> Parses relational expressions (`<`, `>`, `<=`, `>=`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_relational(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left, right

      left = parse_shifting(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. (tokens(pos)%value == '<' .or. tokens(pos)%value == '>' .or. &
         tokens(pos)%value == '<=' .or. tokens(pos)%value == '>='))
         if (tokens(pos)%value == '<') then
            pos = pos + 1
            right = parse_shifting(expr, tokens, ntokens, pos, macros, ctx)
            val = merge(1, 0, left < right)
         else if (tokens(pos)%value == '>') then
            pos = pos + 1
            right = parse_shifting(expr, tokens, ntokens, pos, macros, ctx)
            val = merge(1, 0, left > right)
         else if (tokens(pos)%value == '<=') then
            pos = pos + 1
            right = parse_shifting(expr, tokens, ntokens, pos, macros, ctx)
            val = merge(1, 0, left <= right)
         else
            pos = pos + 1
            right = parse_shifting(expr, tokens, ntokens, pos, macros, ctx)
            val = merge(1, 0, left >= right)
         end if
         left = val
      end do
      val = left
   end function

   !> Parses shift expressions (`<<`, `>>`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_shifting(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left, right

      left = parse_additive(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. (tokens(pos)%value == '<<' .or. tokens(pos)%value == '>>'))
         if (tokens(pos)%value == '<<') then
            pos = pos + 1
            right = parse_additive(expr, tokens, ntokens, pos, macros, ctx)
            val = lshift(left, right)
         else
            pos = pos + 1
            right = parse_additive(expr, tokens, ntokens, pos, macros, ctx)
            val = rshift(left, right)
         end if
         left = val
      end do
      val = left
   end function

   !> Parses additive expressions (`+`, `-`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_additive(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left, right

      left = parse_multiplicative(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. (tokens(pos)%value == '+' .or. tokens(pos)%value == '-'))
         if (tokens(pos)%value == '+') then
            pos = pos + 1
            right = parse_multiplicative(expr, tokens, ntokens, pos, macros, ctx)
            val = left + right
         else
            pos = pos + 1
            right = parse_multiplicative(expr, tokens, ntokens, pos, macros, ctx)
            val = left - right
         end if
         left = val
      end do
      val = left
   end function

   !> Parses multiplicative expressions (`*`, `/`, `%`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx    Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_multiplicative(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left, right

      left = parse_unary(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      do while (pos <= ntokens .and. (tokens(pos)%value == '*' .or. tokens(pos)%value == '/' .or. tokens(pos)%value == '%'))
         if (tokens(pos)%value == '*') then
            pos = pos + 1
            right = parse_unary(expr, tokens, ntokens, pos, macros, ctx)
            val = left * right
         else if (tokens(pos)%value == '/') then
            pos = pos + 1
            right = parse_unary(expr, tokens, ntokens, pos, macros, ctx)
            val = left / right
         else
            pos = pos + 1
            right = parse_unary(expr, tokens, ntokens, pos, macros, ctx)
            val = modulo(left, right)
         end if
         left = val
      end do
      val = left
   end function

   !> Parses exponentiation (`**`). Right-associative.
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx    Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_power(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: left, right

      left = parse_atom(expr, tokens, ntokens, pos, macros, ctx)
      if (pos > ntokens) then
         val = left
         return
      end if
      if (pos <= ntokens .and. tokens(pos)%value == '**') then
         pos = pos + 1
         ! recurse at same precedence level
         right = parse_power(expr, tokens, ntokens, pos, macros, ctx)
         val = left**right
      else
         val = left
      end if
   end function

   !> Parses unary operators (`!`, `-`, `+`, `~`).
   !! @param[in] expr Expression to be processed
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx    Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_unary(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx

      if (pos <= ntokens .and. tokens(pos)%value == '!') then
         pos = pos + 1
         val = merge(0, 1, parse_unary(expr, tokens, ntokens, pos, macros, ctx) /= 0)
      else if (pos <= ntokens .and. tokens(pos)%value == '-') then
         pos = pos + 1
         val = -parse_unary(expr, tokens, ntokens, pos, macros, ctx)
      else if (pos <= ntokens .and. tokens(pos)%value == '+') then
         pos = pos + 1
         val = parse_unary(expr, tokens, ntokens, pos, macros, ctx)
      else if (pos <= ntokens .and. tokens(pos)%value == '~') then
         pos = pos + 1
         val = not(parse_unary(expr, tokens, ntokens, pos, macros, ctx))
      else
         val = parse_power(expr, tokens, ntokens, pos, macros, ctx)
      end if
   end function

   !> Parses primary expressions: numbers, identifiers, `defined(...)`, parentheses.
   !! @param[in] expr      Input expression
   !! @param[in] tokens    Array of tokens to parse
   !! @param[in] ntokens   Number of valid tokens in the array
   !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
   !! @param[inout] macros    Defined macros for expansion and `defined()` checks
   !! @param[in] ctx    Context
   !! @return Integer value of the parsed expression
   !!
   !! @ingroup group_operators
   recursive integer function parse_atom(expr, tokens, ntokens, pos, macros, ctx) result(val)
      character(*), intent(in)                :: expr
      type(token), intent(in)                 :: tokens(:)
      integer, intent(in)                     :: ntokens
      integer, intent(inout)                  :: pos
      type(macro), allocatable, intent(inout) :: macros(:)
      type(context), intent(in)               :: ctx
      !private
      integer :: i
      character(:), allocatable :: expanded
      logical :: stitch

      if (pos > ntokens) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('Unexpected end of expression', pos, 1), &
            source=trim(ctx%path)), &
            expr, ctx%line))
         val = 0
         return
      end if

      if (tokens(pos)%type == 0) then
         val = strtol(tokens(pos)%value)
         pos = pos + 1
      else if (tokens(pos)%type == 2) then
         if (is_defined(tokens(pos)%value, macros)) then
            expanded = expand_macros(tokens(pos)%value, macros, stitch, global%implicit_continuation, &
               global%support_dollar_insert, ctx)
            if (.not. evaluate_expression(expanded, macros, ctx, val)) val = 0
         else
            val = 0
         end if
         pos = pos + 1
      else if (tokens(pos)%value == '(') then
         pos = pos + 1
         val = parse_expression(expr, tokens, ntokens, pos, macros, ctx)
         if (pos > ntokens .or. tokens(pos)%value /= ')') then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Syntax error', &
               label=label_type('Missing closing parenthesis in expression', len(expr), 1), &
               source=trim(ctx%path)), &
               expr, ctx%line))
            val = 0
         else
            pos = pos + 1
         end if
      else if (tokens(pos)%type == 4) then
         expanded = trim(tokens(pos)%value)
         val = merge(1, 0, is_defined(expanded, macros))
         pos = pos + 1
      else
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Invalid expression', &
            label=label_type('Unknown token', 1, len_trim(tokens(pos)%value)), &
            source=trim(ctx%path)), &
            expr, ctx%line))
         val = 0
         pos = pos + 1
      end if
   end function
   !! @}
end module

!>>>>> ././src/conditional.f90
!> @file
!! @defgroup group_conditional Conditional
!! Conditional support for the fpx preprocessor.
!!
!! This module implements the complete conditional compilation machinery used
!! by fpx. It provides functionality equivalent to the traditional C
!! preprocessor directives while also introducing a few convenience extensions.
!!
!! Supported directives include:
!!
!! - `#if` / `#elif`
!!   Evaluate arbitrary constant expressions using
!!   `evaluate_expression()`.
!!
!! - `#ifdef` / `#ifndef`
!!   Test whether a macro has been defined.
!!
!! - `#elifdef` / `#elifndef`
!!   fpx extensions combining `#elif` semantics with macro existence tests.
!!
!! - `#else`
!!   Select the fallback branch when no previous branch in the same
!!   conditional group has been activated.
!!
!! - `#endif`
!!   Terminate the current conditional block.
!!
!! Nested conditional blocks are supported up to
!! `MAX_COND_DEPTH` levels.
!!
!! The implementation follows the standard "first-match" semantics:
!! once a branch of a conditional group evaluates to true,
!! all remaining `#elif`, `#elifdef`, `#elifndef`, and `#else`
!! directives belonging to the same group are ignored.
!!
!! Internally, the module maintains a stack of conditional states
!! (`cond_stack`) together with the current nesting depth
!! (`cond_depth`). The helper function `is_active()` determines
!! whether the current source line belongs to an active branch and
!! therefore should be processed by the remainder of the preprocessor.
!!
!! @section conditional_design Design
!!
!! Each active conditional nesting level stores two pieces of state:
!!
!! - `active`
!!   Indicates whether the current branch should emit code.
!!
!! - `has_met`
!!   Indicates whether a previous branch within the same
!!   `#if`/`#elif`/`#else` group has already been selected.
!!
!! This design allows efficient evaluation of deeply nested
!! conditionals while preserving correct first-match semantics.
!!
!! @section conditional_examples Examples
!!
!! -# Include guard pattern:
!!
!!      @code{.f90}
!!      #ifndef MY_HEADER_H
!!      #define MY_HEADER_H
!!
!!          ! Header contents
!!
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Feature selection using expression evaluation:
!!
!!      @code{.f90}
!!      #if DEBUG >= 2
!!          print *, 'Verbose debugging'
!!      #elif DEBUG == 1
!!          print *, 'Standard debugging'
!!      #else
!!          ! Silent mode
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Platform-dependent compilation:
!!
!!      @code{.f90}
!!      #ifdef _OPENMP
!!          use omp_lib
!!      #else
!!          integer, parameter :: omp_get_thread_num = 0
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Conditional compilation using macro existence:
!!
!!      @code{.f90}
!!      #if defined(USE_MPI) && (MPI_VERSION >= 3)
!!          use mpi_f08
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Using the fpx extension `#elifdef`:
!!
!!      @code{.f90}
!!      #ifdef USE_CUDA
!!          call gpu_backend()
!!      #elifdef USE_OPENMP
!!          call omp_backend()
!!      #else
!!          call serial_backend()
!!      #endif
!!      ...
!!      @endcode
!!
!! -# Nested conditionals:
!!
!!      @code{.f90}
!!      #ifdef DEBUG
!!          #if DEBUG > 1
!!          print *, 'Extra diagnostics'
!!          #endif
!!      #endif
!!      ...
!!      @endcode
module fpx_conditional
   use fpx_constants
   use fpx_logging
   use fpx_string
   use fpx_macro, only: macro, is_defined
   use fpx_operators, only: evaluate_expression
   use fpx_context

   implicit none; private

   public :: handle_if, &
      handle_ifdef, &
      handle_ifndef, &
      handle_elif, &
      handle_else, &
      handle_endif, &
      handle_elifdef, &
      handle_elifndef, &
      is_active

   !> State associated with a single conditional nesting level.
   !!
   !! Each `#if` directive pushes one instance of this type onto
   !! `cond_stack`, and the corresponding `#endif` removes it.
   !!
   !! The combination of `active` and `has_met` implements the
   !! standard first-match semantics of conditional preprocessing.
   !!
   !! @section cond_state_constructors Constructors
   !!
   !! @b Constructor
   !! @code{.f90} type(cond_state) function cond_state(logical active, logical has_met) @endcode
   !! @param[in] active
   !!   Whether the current branch is active and should emit code.
   !! @param[in] has_met
   !!   Whether a previous branch in the same conditional group
   !!   has already evaluated to true.
   !! @return A newly constructed conditional state object.
   !! @ingroup group_conditional
   type, public :: cond_state
      logical, public :: active
      logical, public :: has_met
   end type

   !> @brief Global stack of conditional states (depth-limited)
   !! @ingroup group_conditional
   type(cond_state), public :: cond_stack(MAX_COND_DEPTH)

   !> @brief Current nesting depth of conditional directives (0 = outside any #if)
   !! @ingroup group_conditional
   integer, public :: cond_depth = 0

contains

   !> Determine whether the current source position is active.
   !!
   !! Traverses all enclosing conditional levels and returns `.true.`
   !! only if every surrounding conditional branch is active.
   !!
   !! This routine is used throughout the preprocessing pipeline to
   !! decide whether directives should be executed and whether
   !! ordinary source lines should be emitted.
   !!
   !! @return
   !! `.true.` if the current line belongs to an active branch;
   !! `.false.` otherwise.
   !!
   !! @ingroup group_conditional
   logical function is_active() result(res)
      integer :: i
      res = .true.
      do i = 1, cond_depth + 1
         if (.not. cond_stack(i)%active) then
            res = .false.
            exit
         end if
      end do
   end function

   !> Process a #if directive with constant expression evaluation
   !! Evaluates the expression after #if using `evaluate_expression()` and pushes
   !! a new state onto the conditional stack.
   !! @param[in] ctx       Context source line containing the directive
   !! @param[inout] macros    Current macro table
   !! @param[in] token     Usually 'if'
   !!
   !! @ingroup group_conditional
   subroutine handle_if(ctx, macros, token)
      type(context), intent(in)               :: ctx
      type(macro), allocatable, intent(inout) :: macros(:)
      character(*), intent(in)                :: token
      !private
      character(:), allocatable :: expr
      logical :: result, parent_active
      integer :: pos

      if (cond_depth + 1 > MAX_COND_DEPTH) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Conditional nesting too deep', &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      expr = trim(adjustl(ctx%content(pos:)))
      result = evaluate_expression(expr, macros, ctx)
      parent_active = is_active()
      cond_depth = cond_depth + 1
      cond_stack(cond_depth + 1)%active = result .and. parent_active
      cond_stack(cond_depth + 1)%has_met = result
   end subroutine

   !> Process #ifdef - test if a macro is defined
   !! @param[in] ctx       Context source line containing the directive
   !! @param[in] macros    Current macro table
   !! @param[in] token     Usually 'ifdef'
   !!
   !! @ingroup group_conditional
   subroutine handle_ifdef(ctx, macros, token)
      type(context), intent(in)       :: ctx
      type(macro), intent(in)         :: macros(:)
      character(*), intent(in)        :: token
      !private
      character(:), allocatable :: name
      logical :: defined, parent_active
      integer :: pos

      if (cond_depth + 1 > MAX_COND_DEPTH) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Conditional nesting too deep', &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      name = trim(adjustl(ctx%content(pos:)))
      defined = is_defined(name, macros)
      parent_active = is_active()
      cond_depth = cond_depth + 1
      cond_stack(cond_depth + 1)%active = defined .and. parent_active
      cond_stack(cond_depth + 1)%has_met = defined
   end subroutine

   !> Process #ifndef - test if a macro is NOT defined
   !! @param[in] ctx       Context source line containing the directive
   !! @param[in] macros    Current macro table
   !! @param[in] token     Usually 'ifndef'
   !!
   !! @ingroup group_conditional
   subroutine handle_ifndef(ctx, macros, token)
      type(context), intent(in)       :: ctx
      type(macro), intent(in)         :: macros(:)
      character(*), intent(in)        :: token
      !private
      character(:), allocatable :: name
      logical :: defined, parent_active
      integer :: pos

      if (cond_depth + 1 > MAX_COND_DEPTH) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Conditional nesting too deep', &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      name = trim(adjustl(ctx%content(pos:)))
      defined = is_defined(name, macros)
      parent_active = is_active()
      cond_depth = cond_depth + 1
      cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
      cond_stack(cond_depth + 1)%has_met = .not. defined
   end subroutine

   !> Process #elif - alternative branch after #if/#elif
   !! Only activates if no previous branch in the group was taken.
   !! @param[in] ctx       Context source line containing the directive
   !! @param[inout] macros    Current macro table
   !! @param[in] token     Usually 'elif'
   !!
   !! @ingroup group_conditional
   subroutine handle_elif(ctx, macros, token)
      type(context), intent(in)               :: ctx
      type(macro), allocatable, intent(inout) :: macros(:)
      character(*), intent(in)                :: token
      !private
      character(:), allocatable :: expr
      logical :: result, parent_active
      integer :: pos

      if (cond_depth == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('#elif without matching #if', 1, len_trim(ctx%content)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      expr = trim(adjustl(ctx%content(pos:)))
      result = evaluate_expression(expr, macros, ctx)
      parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
      if (.not. cond_stack(cond_depth + 1)%has_met) then
         cond_stack(cond_depth + 1)%active = result .and. parent_active
         if (result) cond_stack(cond_depth + 1)%has_met = .true.
      else
         cond_stack(cond_depth + 1)%active = .false.
      end if
   end subroutine

   !> Process #elifdef - test if a macro is defined
   !! @param[in] ctx       Context source line containing the directive
   !! @param[in] macros    Current macro table
   !! @param[in] token     Usually 'elifdef'
   !!
   !! @ingroup group_conditional
   subroutine handle_elifdef(ctx, macros, token)
      type(context), intent(in)       :: ctx
      type(macro), intent(in)         :: macros(:)
      character(*), intent(in)        :: token
      !private
      character(:), allocatable :: name
      logical :: defined, parent_active
      integer :: pos

      if (cond_depth == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('#elifdef without matching #if', 1, len_trim(ctx%content)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      name = trim(adjustl(ctx%content(pos:)))
      defined = is_defined(name, macros)
      parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
      if (.not. cond_stack(cond_depth + 1)%has_met) then
         cond_stack(cond_depth + 1)%active = defined .and. parent_active
         if (defined) cond_stack(cond_depth + 1)%has_met = .true.
      else
         cond_stack(cond_depth + 1)%active = .false.
      end if
   end subroutine

   !> Process #elifndef - test if a macro is not defined
   !! @param[in] ctx       Context source line containing the directive
   !! @param[in] macros    Current macro table
   !! @param[in] token     Usually 'elifndef'
   !!
   !! @ingroup group_conditional
   subroutine handle_elifndef(ctx, macros, token)
      type(context), intent(in)       :: ctx
      type(macro), intent(in)         :: macros(:)
      character(*), intent(in)        :: token
      !private
      character(:), allocatable :: name
      logical :: defined, parent_active
      integer :: pos

      if (cond_depth == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('#elifndef without matching #if', 1, len_trim(ctx%content)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      pos = index(lowercase(ctx%content), token) + len(token)
      name = trim(adjustl(ctx%content(pos:)))
      defined = is_defined(name, macros)
      parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
      if (.not. cond_stack(cond_depth + 1)%has_met) then
         cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
         if (.not. defined) cond_stack(cond_depth + 1)%has_met = .true.
      else
         cond_stack(cond_depth + 1)%active = .false.
      end if
   end subroutine

   !> Process #else - final fallback branch
   !! Activates only if no previous #if/#elif branch was true.
   !! @param[in] ctx  Context (for error messages)
   !!
   !! @ingroup group_conditional
   subroutine handle_else(ctx)
      type(context), intent(in) :: ctx
      !private
      logical :: parent_active

      if (cond_depth == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('#else without matching #if', 1, len_trim(ctx%content)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if

      parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
      if (.not. cond_stack(cond_depth + 1)%has_met) then
         cond_stack(cond_depth + 1)%active = parent_active
         cond_stack(cond_depth + 1)%has_met = .true.
      else
         cond_stack(cond_depth + 1)%active = .false.
      end if
   end subroutine

   !> Process #endif - end of conditional block
   !! Pops the top state from the stack. Reports error on unmatched #endif.
   !! @param[in] ctx  Context (for error messages)
   !!
   !! @ingroup group_conditional
   subroutine handle_endif(ctx)
      type(context), intent(in) :: ctx

      if (cond_depth == 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Syntax error', &
            label=label_type('#endif without matching #if', 1, len_trim(ctx%content)), &
            source=trim(ctx%path)), &
            ctx%content, ctx%line))
         return
      end if
      cond_depth = cond_depth - 1
   end subroutine

end module

!>>>>> ././src/parser.f90
!> @file
!! @defgroup group_parser Parser
!! Fortran Preprocessor (fpx) - core parsing and preprocessing module
!!
!! This module implements a full-featured, modern Fortran preprocessor supporting:
!! - C-style line continuations with `\` and `\\`
!! - Fortran-style `&` continuations
!! - `#define`, `#undef`, object-like and function-like macros with variadic support
!! - `#include` with proper path resolution and recursion guard
!! - Conditional compilation: `#if`, `#ifdef`, `#ifndef`, `#elif`, `#else`, `#endif`
!! - Non-standard `#for` directive
!! - C-style `/* ... */` comments (nestable aware)
!! - Macro expansion with argument substitution and stringification (`#`) / token-pasting (`##`)
!! - Interactive REPL mode when reading from stdin
!! - Multiple entry points for file-to-file, unit-to-unit, etc.
!! - Support ${x} for substituting macro name
!!
!! The preprocessor is designed to be standards-conforming where possible while adding
!! useful extensions (variadic macros, better diagnostics, include path handling).
!!
!! @par Processing Pipeline
!! Source files are processed in several stages:
!!
!! 1. Input acquisition from files, units, or stdin
!! 2. C-style continuation handling (`\`, `\\`)
!! 3. Fortran continuation handling (`&`)
!! 4. C-style block comment removal
!! 5. Directive recognition and execution
!! 6. Conditional compilation evaluation
!! 7. Macro expansion
!! 8. Emission to the output stream
!!
!! Include files recursively invoke the same processing pipeline.
!!
!! @par Parser State
!! The parser maintains module-level state describing:
!! - current source file,
!! - line continuation status,
!! - comment state,
!! - deferred reprocessing buffers,
!! - interactive mode output state.
!!
!! This state is reset at the beginning of each top-level preprocessing run.
!!
!! The parser coordinates several specialized subsystems:
!!
!! - @ref group_define     for macro definition directives,
!! - @ref group_include    for include processing,
!! - @ref group_macro      for macro expansion,
!! - @ref group_conditional for conditional compilation,
!! - @ref group_for        for non-standard loop directives,
!! - @ref group_diagnostics for reporting.
!!
!! @par fpx Extensions
!! In addition to standard preprocessing facilities, fpx provides:
!! - `#for` / `#endfor`
!! - `${NAME}` macro insertion
!! - implicit continuation support
!! - interactive REPL mode
!! - enhanced diagnostics
!! @section parser_examples Examples
!!
!! 1. Preprocess a file to stdout:
!! @code{.f90}
!!    call preprocess('input.F90')
!! @endcode
!!
!! 2. Preprocess a file and write to another file:
!! @code{.f90}
!!    call preprocess('src/main.F90', 'preprocessed/main.F90')
!! @endcode
!!
!! 3. Use in a build system with unit numbers:
!! @code{.f90}
!!    integer :: iu, ou
!!    open(newunit=iu, file='input.F90')
!!    open(newunit=ou, file='output.F90')
!!    call preprocess(iu, ou)
!!    close(iu); close(ou)
!!    ...
!! @endcode
!!
!! 4. Interactive mode (stdin to stdout):
!! @code
!!    $ ./fpx
!!     [in]  #define PI 3.1415926535
!!     [out]
!!     [in]  real :: x = PI*2
!!     [out] real :: x = 3.1415926535*2
!!     [in]  (empty line or 'quit' to exit)
!! @endcode
module fpx_parser
   use, intrinsic :: iso_fortran_env, only: stdout => output_unit, iostat_end, stdin => input_unit
   use, intrinsic :: iso_c_binding, only: c_char, c_size_t, c_ptr, c_null_ptr, c_associated, c_funloc
   use fpx_constants
   use fpx_string
   use fpx_logging
   use fpx_macro
   use fpx_conditional
   use fpx_define
   use fpx_diagnostics
   use fpx_include
   use fpx_path
   use fpx_global
   use fpx_context
   use fpx_line
   use fpx_for

   implicit none; private

   public :: preprocess,  &
      global

   !> Generic interface to start preprocessing from various sources/sinks
   !!
   !! Allows preprocessing:
   !! - file to stdout
   !! - file to file
   !! - unit to file
   !! - unit to unit (most flexible, used internally for #include)
   !!
   !! @section preprocess_overloads Overloads
   !!
   !! @code{.f90}preprocess(character(*))@endcode
   !! Preprocess a source file and write to stdout.
   !!
   !! @code{.f90}preprocess(character(*), character(*))@endcode
   !! Preprocess a source file and write to another file.
   !!
   !! @code{.f90}preprocess(integer, integer)@endcode
   !! Preprocess an already-open input unit to an output unit.
   !!
   !! @code{.f90}preprocess(integer, character(*))@endcode
   !! Preprocess an already-open input unit to a file.
   !!
   !! @ingroup group_parser
   interface preprocess
      module procedure :: preprocess_file
      module procedure :: preprocess_file_to_unit
      module procedure :: preprocess_unit_to_file
      module procedure :: preprocess_unit_to_unit
   end interface

   character(256) :: name                              !< Current source file name (without path)
   logical        :: c_continue                        !< Flags for C-style continuation
   logical        :: f_continue                        !< Flags for Fortran-style continuation
   logical        :: in_comment                        !< Internal state flags
   logical        :: reprocess                         !< Internal state flags
   logical        :: stitch                            !< Internal state flags
   character(:), allocatable :: res                    !< Accumulated result line buffers
   character(:), allocatable :: tmp                    !< Accumulated temporary line buffers
   character(MAX_LINE_LEN)   :: line                   !< Raw input line
   character(MAX_LINE_LEN)   :: continued_line         !< Raw and continued input line
   integer :: iline                                    !< Current line number position
   integer :: icontinuation                            !< Continuation position

contains

   !> Preprocess a file and write result to an optional output file (default: stdout)
   !! Opens the input file, determines the base filename for error messages,
   !! opens the output file if requested, and delegates to the unit-to-unit routine.
   !! @param[in] filepath   Path to the input source file
   !! @param[in] outputfile Optional path to the output file; if absent output goes to stdout
   !!
   !! @ingroup group_parser
   subroutine preprocess_file(filepath, outputfile)
      character(*), intent(in)            :: filepath
      character(*), intent(in), optional  :: outputfile
      !private
      integer :: iunit, ierr, n, ounit
      character(len=1, kind=c_char) :: buf(256)

      open(newunit=iunit, file=filepath, status='old', action='read', iostat=ierr)

      if (ierr /= 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Error opening input file: ' // trim(filepath), &
            source=name), &
            ''))
         return
      else
         if (cwd().ne.'')then
            n=index(name,filepath)
            if(n==1)name=filepath(n+1:)
         endif
      end if

      if (present(outputfile)) then
         open(newunit=ounit, file=outputfile, status='replace', action='write', iostat=ierr)
         if (ierr /= 0) then
            call printf(render(diagnostic_report(LEVEL_ERROR, &
               message='Error opening input file: ' // trim(outputfile), &
               source=name), &
               ''))
            close(iunit)
            return
         end if
      else
         ounit = stdout
      end if

      call preprocess(iunit, ounit)
      if (iunit /= stdin) close(iunit)
      if (ounit /= stdout) close(ounit)
   end subroutine

   !> Preprocess from an already-open input unit and write to a file
   !! @param[in] iunit Input unit (must already be open for reading)
   !! @param[in] ofile Output filename
   !!
   !! @ingroup group_parser
   subroutine preprocess_unit_to_file(iunit, ofile)
      integer, intent(in)         :: iunit
      character(*), intent(in)    :: ofile
      !private
      integer :: ierr, ounit

      if (iunit /= stdin) then
         inquire(unit = iunit, name=name)
      end if

      open(newunit=ounit, file=ofile, status='replace', action='write', iostat=ierr)
      if (ierr /= 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Error opening input file: ' // trim(ofile), &
            source=name), &
            ''))
         close(iunit)
         return
      end if

      call preprocess(iunit, ounit)
      if (iunit /= stdin) close(iunit)
      if (ounit /= stdout) close(ounit)
   end subroutine

   !> Preprocess a file and write to an already-open output unit
   !! @param[in] ifile Input filename
   !! @param[in] ounit Output unit (already open for writing)
   !!
   !! @ingroup group_parser
   subroutine preprocess_file_to_unit(ifile, ounit)
      character(*), intent(in)    :: ifile
      integer, intent(in)         :: ounit
      !private
      integer :: iunit, ierr, n
      character(len=1, kind=c_char) :: buf(256)

      open(newunit=iunit, file=ifile, status='old', action='read', iostat=ierr)
      if (ierr /= 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Error opening input file: ' // trim(ifile), &
            source=name), &
            ''))
         return
      else
         ! if name starts with current working directory trim it of cwd (?)
         if (cwd().ne.'')then
            n=index(name,ifile)
            if(n==1)name=ifile(n+1:)
         endif
      end if

      call preprocess(iunit, ounit)
      if (iunit /= stdin) close(iunit)
      if (ounit /= stdout) close(ounit)
   end subroutine

   !> Core preprocessing routine: read from iunit, write to ounit
   !! Sets up a clean macro environment for the top-level file,
   !! resets conditional compilation state, and calls the worker routine.
   !!
   !! A local copy of the global macro table is created so that
   !! preprocessing sessions remain isolated while preserving
   !! command-line definitions.
   !!
   !! @param[in] iunit Input unit
   !! @param[in] ounit Output unit
   !!
   !! @ingroup group_parser
   subroutine preprocess_unit_to_unit(iunit, ounit)
      integer, intent(in) :: iunit
      integer, intent(in) :: ounit
      !private
      type(macro), allocatable :: macros(:)

      if (.not. allocated(global%macros)) allocate(global%macros(0))
      allocate(macros(size_of(global%macros)), source=global%macros)
      if (.not. allocated(global%undef)) allocate(global%undef(0))
      if (.not. allocated(global%includedir)) allocate(global%includedir(0))

      cond_depth = 0
      cond_stack(1)%active = .true.
      cond_stack(1)%has_met = .false.

      reprocess = .false.;  c_continue = .false.; f_continue = .false.
      icontinuation = 1; iline = 0
      continued_line = ''; res = ''

      call preprocess_unit(iunit, ounit, macros, .false.)
      deallocate(macros)
   end subroutine

   !> Worker routine that reads lines, handles continuations, comments and directives
   !! @par Main Loop
   !! The routine repeatedly:
   !! - reads a physical line,
   !! - merges continuations,
   !! - processes directives,
   !! - performs macro expansion,
   !! - handles deferred Fortran continuation stitching,
   !! - emits output.
   !!
   !! @param[in]    iunit       Input unit
   !! @param[in]    ounit       Output unit
   !! @param[inout] macros(:)   Current macro table (passed by value between include levels)
   !! @param[in]    from_include True if called recursively from #include
   !!
   !! @ingroup group_parser
   subroutine preprocess_unit(iunit, ounit, macros, from_include)
      integer, intent(in)                     :: iunit
      integer, intent(in)                     :: ounit
      type(macro), allocatable, intent(inout) :: macros(:)
      logical, intent(in)                     :: from_include
      !private
      integer :: ierr, n

      do
         if (global%interactive) write(*, '(/a)', advance='no') ' [in]  '  ! Command line prompt
         read(iunit, '(A)', iostat=ierr) line

         if (global%interactive) then
            if (line == '') exit
            if (lowercase(trim(adjustl(line))) == 'quit') exit
         end if
         if (ierr /= 0) then
            if (ierr == iostat_end .and. from_include) f_continue = tail(tmp) == '&'
            exit
         end if
         if (.not. from_include) iline = iline + 1

         if (c_continue) then
            continued_line = continued_line(:icontinuation) // trim(adjustl(line))
         else
            continued_line = trim(adjustl(line))
         end if
         n = len_trim(continued_line); if (n == 0) cycle

         ! Check for line continuation with '\'
         if (verify(continued_line(n:n), '\') == 0) then
            ! Check for line break with '\\'
            if (continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\' .and. global%line_break) then
               c_continue = .true.
               continued_line = continued_line(:len_trim(continued_line) - 2) // new_line('A')  ! Strip '\\'
               icontinuation = len_trim(continued_line)
            else
               c_continue = .true.
               icontinuation = len_trim(continued_line) - 1
               continued_line = continued_line(:icontinuation)
            end if
            cycle
         else
            c_continue = .false.

            tmp = process_line(continued_line, ounit, name, iline, macros, stitch)
            if (len_trim(tmp) == 0) cycle

            in_comment = head(tmp) == '!'

            if (merge(head(res) == '!', in_comment, len_trim(res) > 0)) then
               f_continue = tail(tmp) == '&'
            else
               if (in_comment .and. f_continue) cycle
               f_continue = .not. in_comment .and. tail(tmp) == '&'
            end if

            if ((.not. global%disable_continuation) .and. (f_continue .or. stitch)) then
               reprocess = .true.
               res = concat(res, tmp)
            else
               if (reprocess) then
                  if (.not. in_comment .and. head(res) == '!') then
                     if (is_in_forloop()) then
                        call add_to_loop(res)
                     else
                        write(ounit, '(A)') res
                     end if
                     res = process_line(tmp, ounit, name, iline, macros, stitch)
                  else
                     res = process_line(concat(res, tmp), ounit, name, iline, macros, stitch)
                  end if
                  reprocess = .false.
               else
                  res = trim(tmp)
               end if

               if (is_in_forloop()) then
                  call add_to_loop(res)
               else
                  if (global%interactive) write(*, '(/a)', advance='no') ' [out] '  ! Command line prompt
                  write(ounit, '(A)') res
               end if
               res = ''
            end if
         end if
      end do

      if (cond_depth > 0) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Unclosed conditional block at end of file', &
            source=name), &
            trim(line), iline))
      else if (c_continue) then
         call printf(render(diagnostic_report(LEVEL_ERROR, &
            message='Unexpected character', &
            label=label_type('Trailing new line "\"', len(trim(line)), 1), &
            source=name), &
            trim(line), iline))
      end if
   end subroutine

   !> Process a single (possibly continued) line - handles directives and macro expansion
   !! Responsibilities:
   !! - Strip or terminate C-style block comments (`/* ... */`)
   !! - Detect and delegate preprocessor directives (`#define`, `#include`, conditionals, etc.)
   !! - Perform macro expansion when the line is in an active conditional block
   !! - Return whether the next line should be stitched (for Fortran `&` continuation inside macros)
   !! This routine acts as the dispatcher for all preprocessing
   !! directives and ordinary source lines.
   !!
   !! Directives are interpreted immediately, whereas ordinary
   !! lines undergo macro expansion only when the current
   !! conditional compilation state is active.
   !! @param[in]    current_line Input line (already continued and trimmed)
   !! @param[in]    ounit        Output unit (used only for diagnostics inside called routines)
   !! @param[in]    filepath     Current file name (for error messages)
   !! @param[in]    linenum      Current line number (for error messages)
   !! @param[inout] macros(:)    Macro table
   !! @param[out]   stch         Set to .true. if the expanded line ends with `&` (stitch next line)
   !! @return                    Processed line (directives removed, macros expanded)
   !!
   !! @see
   !! @link fpx_macro::expand_all expand_all @endlink
   !! @link fpx_define::handle_define handle_define @endlink
   !! @link fpx_include::handle_include handle_include @endlink
   !!
   !! @ingroup group_parser
   recursive function process_line(current_line, ounit, filepath, linenum, macros, stch) result(rst)
      character(*), intent(in)                :: current_line
      integer, intent(in)                     :: ounit
      character(*), intent(inout)             :: filepath
      integer, intent(inout)                  :: linenum
      type(macro), allocatable, intent(inout) :: macros(:)
      logical, intent(out)                    :: stch
      character(:), allocatable               :: rst
      !private
      character(:), allocatable :: trimmed_line
      logical :: active
      logical, save :: l_in_comment = .false., l_in_loop = .false.
      integer :: idx, comment_start, comment_end, n
      type(context) :: ctx

      trimmed_line = trim(adjustl(current_line))
      rst = ''
      comment_end = index(trimmed_line, '*/')
      if (l_in_comment .and. comment_end > 0) then
         trimmed_line = trimmed_line(comment_end + 2:)
         l_in_comment = .false.
      end if

      if (l_in_comment) return
      comment_start = index(trimmed_line, '/*')
      if (comment_start > 0) then
         trimmed_line = trimmed_line(:comment_start - 1)
         l_in_comment = comment_end == 0
      end if
      n = len(trimmed_line); if (n == 0) return

      active = is_active()
      ctx = context(trimmed_line, linenum, filepath)
      if (head(trimmed_line) == '#') then
         if (len(trimmed_line) == 1) then
            return  !null directive
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'for')) then
            l_in_loop = .true.
            if (global%support_forloop) call handle_for(ctx, macros, 'for')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'endfor')) then
            l_in_loop = .false.
            if (global%support_forloop) call handle_endfor(ctx, ounit, c_funloc(process_line), macros, 'endfor')
            l_in_loop = is_in_forloop()
         else if (l_in_loop) then
            rst = trimmed_line
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'define') .and. active) then
            call handle_define(ctx, macros, 'define')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'undef') .and. active) then
            call handle_undef(ctx, macros, 'undef')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'warning') .and. active) then
            call handle_warning(ctx, macros, 'warning')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'error') .and. active) then
            call handle_error(ctx, macros, 'error')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'include') .and. active) then
            call handle_include(ctx, ounit, preprocess_unit, macros, 'include')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'line')) then
            call handle_line(ctx, 'line')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'ifdef')) then
            call handle_ifdef(ctx, macros, 'ifdef')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'ifndef')) then
            call handle_ifndef(ctx, macros, 'ifndef')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'elifdef')) then
            call handle_elifdef(ctx, macros, 'elifdef')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'elifndef')) then
            call handle_elifndef(ctx, macros, 'elifndef')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'if')) then
            call handle_if(ctx, macros, 'if')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'elif')) then
            call handle_elif(ctx, macros, 'elif')
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'else')) then
            call handle_else(ctx)
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'endif')) then
            call handle_endif(ctx)
         else if (starts_with(lowercase(adjustl(trimmed_line(2:))), 'pragma') .and. active) then
            rst = ctx%content
         else
            return
         end if
      else if (active) then
         if (.not. global%expand_macros .or. is_in_forloop()) then
            rst = trimmed_line
         else
            rst = adjustl(expand_all(ctx, macros, stch, global%extra_macros, global%implicit_continuation, &
               global%implicit_continuation))
         end if
      end if
   end function
end module

!>>>>> app/main.F90
!> @brief Main entry point and command-line driver for the fpx Fortran preprocessor
!! This is the standalone executable program that parses command-line arguments
!! and launches the fpx preprocessing engine. It supports a rich set of options
!! compatible with traditional C/Fortran preprocessors while adding modern features.
!!
!! Supported command-line options:
!! - `-Dname` or `-Dname=value` -> define macro (object-like)
!! - `-Uname`                   -> undefine macro (add to global%undef)
!! - `-Ipath`                   -> add directory to include search path
!! - `-o outfile`               -> specify output file
!! - `-v`                       -> print version and exit
!! - `-h`, `-?`                 -> display help message and exit
!!
!! Input can be a file or stdin; output can be a file or stdout.
!! The program integrates fully with the fpx library: all global settings
!! (`global%macros`, `global%includedir`, etc.) are populated here.
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Basic preprocessing:
!!    $ fpx input.F90 -o output.f90
!!
!! 2. Define macros and add include path:
!!    $ fpx -DDEBUG=1 -DMPI -I./include src/main.F90
!!
!! 3. Predefine version and process from stdin:
!!    $ cat source.F90 | fpx -D_VERSION='"1.5.0"' -o preprocessed.f90
!!
!! 4. Interactive mode (stdin -> stdout):
!!    $ fpx
!!    [in]  #define PI 3.14
!!    [out]
!!    [in]  real :: r = PI
!!    [out] real :: r = 3.14
!!
!! 5. Show version or help:
!!    $ fpx -v
!!    $ fpx -h
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
!
!  Redistributions of source code must retain the above copyright
! notice, this list of conditions and the following disclaimer.
!   Redistributions in binary form must reproduce the above
! copyright notice, this list of conditions and the following disclaimer
! in the documentation and/or other materials provided with the
! distribution.
!   Neither the name of original developer, nor the names of its
! contributors may be used to endorse or promote products derived from
! this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module applibrary
   implicit none; private

   public :: split_commandline_into_args

   public :: trim, &
      index

   interface
      subroutine exit_x(context)
         class(*), intent(inout) :: context
      end subroutine
   end interface

   type, public :: argument
      private
      character(:), allocatable, public :: chars
   contains
      procedure, private, pass(lhs) :: argument_assign_argument
      procedure, private, pass(lhs) :: argument_assign_character
      procedure, private, pass(rhs) :: character_assign_argument
      generic :: assignment(=)      => argument_assign_argument,      &
         argument_assign_character,     &
         character_assign_argument
      procedure, private, pass(dtv) :: argument_read_formatted
      procedure, private, pass(dtv) :: argument_write_formatted
      procedure, private, pass(dtv) :: argument_read_unformatted !! Unformatted input.
      procedure, private, pass(dtv) :: argument_write_unformatted
      generic :: read (formatted)   => argument_read_formatted
      generic :: write (formatted)  => argument_write_formatted
      generic :: read (unformatted) => argument_read_unformatted
      generic :: write (unformatted)=> argument_write_unformatted
   end type

   type, public :: application
      private
      character(:), allocatable           :: username_
      character(:), allocatable           :: location_
      class(*), pointer                   :: context_ => null()
      procedure(exit_x), nopass, pointer  :: onexit_ => null()
   contains
      procedure, pass(this), public :: username => app_username
      procedure, pass(this), public :: location => app_location
      procedure, pass(this), public :: abort => app_abort
      procedure, pass(this), public :: shutdown => app_shutdown
      procedure, pass(this), public :: onexit => app_onexit
      final :: app_finalize
   end type

   interface split_commandline_into_args
      module procedure split_commandline_into_args0
      module procedure split_commandline_into_args1
      module procedure split_commandline_into_args2
      module procedure split_commandline_into_args3
   end interface

   interface trim
      module procedure :: argument_trim
   end interface

   interface index
      module procedure :: argument_index
   end interface

contains

   function app_username(this) result(username)
      class(application), intent(inout) :: this
      !private
      character(:), allocatable :: username

      if (.not. allocated(this%username_)) then
         this%username_ = get_username()
      end if
      username = this%username_
   end function

   function app_location(this) result(location)
      class(application), intent(inout) :: this
      !private
      character(:), allocatable :: location
      integer :: n

      if (.not. allocated(this%location_)) then
         call get_command_argument(0,length=n)
         allocate(character(n) :: this%location_)
         call get_command_argument(0,value=this%location_)
      end if
      location = this%location_
   end function

   subroutine app_abort(this)
      class(application), intent(inout) :: this
#ifndef __GFORTRAN__
      interface
         subroutine abort() bind(C, name="abort")
         end subroutine
      end interface
#endif

      call app_finalize(this)
      call abort()
   end subroutine

   subroutine app_shutdown(this, code)
      class(application), intent(inout) :: this
      integer, intent(in), optional :: code

      call app_finalize(this)

      if (present(code)) then
         error stop code
      else
         stop 0
      end if
   end subroutine

   subroutine app_onexit(this, context, event)
      class(application), intent(inout) :: this
      class(*), intent(in), target :: context
      procedure(exit_x) :: event

      nullify(this%context_); this%context_ => context
      nullify(this%onexit_); this%onexit_ => event
   end subroutine

   subroutine app_finalize(this)
      type(application), intent(inout) :: this

      if (associated(this%onexit_)) then
         call this%onexit_(this%context_)
         nullify(this%context_)
         nullify(this%onexit_)
      end if
   end subroutine

   function split_commandline_into_args0() result(args)
      !private
      character(:), allocatable   :: commandline
      type(argument), allocatable :: args(:)
      !private
      integer :: length

      call get_command(length = length)
#ifdef _WIN32
      if (length > 8191) then
         write(*,*) '<!> The length of the command line exceeds the maximum length allowed on Windows system.'
      end if
#endif
      allocate(character(length) :: commandline)
      call get_command(command = commandline)
      args = split_commandline_into_args1(commandline)
   end function

   function split_commandline_into_args1(commandline) result(args)
      character(*), intent(in)    :: commandline
      type(argument), allocatable :: args(:)

      args = split_commandline_into_args2(commandline, .true.)
   end function

   function split_commandline_into_args2(commandline, removeHashComments) result(args)
      character(*), intent(in)    :: commandline
      logical, intent(in)         :: removeHashComments
      type(argument), allocatable :: args(:)
      !private
      character(1), allocatable   :: dummy(:)

      args = split_commandline_into_args3(commandline, .true., dummy)
   end function

   !> @brief The following command line parsing algorithm is similar to the one
   !! used by Microsoft. The rules used by Microsoft C/C++ code are
   !! Microsoft-specific. The runtime startup code uses these rules when
   !! interpreting arguments given on the operating system command line:
   !!  - Arguments are delimited by white space, which is either a space or a tab.
   !!  - The first argument (args[0]) is treated specially. It represents the program name.
   !!    Because it must be a valid pathname, parts surrounded by double quote marks (")
   !!    are allowed. The double quote marks aren't included in the args[0] output.
   !!    The parts surrounded by double quote marks prevent interpretation of a space or
   !!    tab character as the end of the argument. The later rules in this list don't apply.
   !!  - A string surrounded by double quote marks is interpreted as a single argument,
   !!    which may contain white-space characters. A quoted string can be embedded in
   !!    an argument. The caret (^) isn't recognized as an escape character or delimiter.
   !!    Within a quoted string, a pair of double quote marks is interpreted as a single
   !!    escaped double quote mark. If the command line ends before a closing double quote
   !!    mark is found, then all the characters read so far are output as the last argument.
   !!  - A double quote mark preceded by a backslash (\") is interpreted as a literal
   !!    double quote mark (").
   !!  - Backslashes are interpreted literally, unless they immediately precede a double quote mark.
   !!  - If an even number of backslashes is followed by a double quote mark, then one backslash (\)
   !!    is placed in the argv array for every pair of backslashes (\\), and the double quote mark
   !!    (") is interpreted as a string delimiter.
   !!  - If an odd number of backslashes is followed by a double quote mark, then one backslash (\)
   !!    is placed in the argv array for every pair of backslashes (\\). The double quote mark is
   !!    interpreted as an escape sequence by the remaining backslash, causing a literal double
   !!    quote mark (") to be placed in argv.
   !! @note The original code (in C#) can be found here: https://github.com/dotnet/roslyn/blob/46c8f4f56765e16b92759de8098494b36cfb84f5/src/Compilers/Core/Portable/InternalUtilities/CommandLineUtilities.cs
   function split_commandline_into_args3(commandline, removeHashComments, illegalChar) result(args)
      character(*), intent(in)                :: commandline
      logical, intent(in)                     :: removeHashComments
      character(1), allocatable, intent(out)  :: illegalChar(:)
      type(argument), allocatable             :: args(:)
      !private
      character(:), allocatable :: builder
      integer :: i, j, k, n, quoteCount, slashCount
      character(1) :: current
      character(1), parameter :: SPACE = char(32)
      character(1), parameter :: BACKSLASH = char(92)
      character(1), parameter :: QUOTE = char(34)
      character(1), parameter :: HASH = char(35)
      character(1), parameter :: PIPE = char(124)

      n = len(commandline)
      i = 1; j = 1; quoteCount = 0; slashCount = 0
      if (allocated(illegalChar)) deallocate(illegalChar)
      allocate(args(0))

      do while (i <= n)
         do while (i <= n .and. commandline(i:i) == SPACE)
            i = i + 1; if (i > n) exit
         end do

         if (i > n) exit
         if (commandline(i:i) == HASH .and. removeHashComments) exit

         quoteCount = 0
         j = 1
         if (allocated(builder)) deallocate(builder)
         allocate(character(len(commandline)) :: builder)
         do k = 1, n
            builder(k:k) = SPACE
         end do

         do while (i <= n .and. (commandline(i:i) /= SPACE .or. mod(quoteCount, 2) /= 0))
            current = commandline(i:i)
            select_loop: select case (current)
             case (BACKSLASH)
               slashCount = 0
               do while (i <= n .and. commandline(i:i) == BACKSLASH)
                  builder(j:j) = commandline(i:i); j = j + 1
                  i = i + 1; if (i > n) exit
                  slashCount = slashCount + 1
               end do

               if (i >= n .or. commandLine(i:i) /= QUOTE) exit select_loop

               if (mod(slashCount, 2) == 0) then
                  quoteCount = quoteCount + 1
               end if

               builder(j:j) = QUOTE; j = j + 1
               i = i + 1; if (i > n) exit
               exit select_loop
             case (QUOTE)
               builder(j:j) = current; j = j + 1
               quoteCount = quoteCount + 1
               i = i + 1; if (i > n) exit
               exit select_loop
             case default
               if ((iachar(current) >= 1 .and. iachar(current) <= 31) .or. current == PIPE) then
                  if (.not. allocated(illegalChar)) illegalChar = current
               else
                  builder(j:j) = current; j = j + 1
               end if
               i = i + 1; if (i > n) exit
               exit select_loop
            end select select_loop
         end do

         if (quoteCount == 2 .and. builder(1:1) == QUOTE .and. builder(n:n) == QUOTE) then
            builder(1:1) = SPACE
            builder(n:n) = SPACE
         end if

         builder = trim(adjustl(builder(1:n)))
         if (len(builder) > 0) then
            args = [args, argument(builder)]
         end if
      end do
   end function

   function get_username() result(username)
      character(255) :: tmp
      character(:), allocatable :: username
      integer :: sts
#ifdef _WIN32
      call get_environment_variable(name="USERNAME", value=tmp, status=sts)
#else
      call get_environment_variable(name="USER", value=tmp, status=sts)
#endif
      if (sts == 0) then
         username = trim(tmp)
      else
         username = ''
      end if
   end function

   pure subroutine argument_assign_argument(lhs, rhs)
      class(argument), intent(inout) :: lhs
      type(argument),  intent(in)    :: rhs

      if (allocated(rhs%chars)) lhs%chars = rhs%chars
   end subroutine

   pure subroutine argument_assign_character(lhs, rhs)
      class(argument), intent(inout) :: lhs
      character(*), intent(in)       :: rhs

      lhs%chars = rhs
   end subroutine

   pure subroutine character_assign_argument(lhs, rhs)
      character(:), allocatable, intent(inout) :: lhs
      class(argument),  intent(in)             :: rhs

      lhs = rhs%chars
   end subroutine

   function argument_trim(str) result(res)
      class(argument), intent(in) :: str
      character(:), allocatable   :: res

      res = trim(str%chars)
   end function

   integer function argument_index(str, substr, back) result(res)
      type(argument), intent(in)      :: str
      character(*), intent(in)        :: substr
      logical, optional, intent(in)   :: back

      res = index(str%chars, substr, back)
   end function

   subroutine argument_write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
      class(argument), intent(in) :: dtv !< The argument.
      integer, intent(in)         :: unit !< Logical unit.
      character(*), intent(in)    :: iotype !< Edit descriptor.
      integer, intent(in)         :: v_list(:) !< Edit descriptor list.
      integer, intent(out)        :: iostat !< IO status code.
      character(*), intent(inout) :: iomsg !< IO status message.

      if (allocated(dtv%chars)) then
         write (unit, '(A)', iostat=iostat, iomsg=iomsg) dtv%chars
      else
         write (unit, '(A)', iostat=iostat, iomsg=iomsg) ''
      end if
   end subroutine

   subroutine argument_read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
      class(argument), intent(inout)    :: dtv !< The argument.
      integer, intent(in)             :: unit !< Logical unit.
      character(*), intent(in)        :: iotype !< Edit descriptor.
      integer, intent(in)             :: v_list(:) !< Edit descriptor list.
      integer, intent(out)            :: iostat !< IO status code.
      character(*), intent(inout)     :: iomsg !< IO status message.
      !private
      character(:), allocatable :: buffer
      integer :: n

      inquire(unit, size=n)
      allocate(character(len=n) :: buffer)
      read (unit, '(A)', iostat=iostat, iomsg=iomsg) buffer
      dtv%chars = buffer
   end subroutine

   subroutine argument_read_unformatted(dtv, unit, iostat, iomsg)
      class(argument), intent(inout)  :: dtv !< The argument.
      integer, intent(in)             :: unit !< Logical unit.
      integer, intent(out)            :: iostat !< IO status code.
      character(*), intent(inout)     :: iomsg !< IO status message.
      !private
      character(:), allocatable :: buffer
      integer :: n

      inquire(unit, size=n)
      allocate(character(len=n) :: buffer)
      read (unit, iostat=iostat, iomsg=iomsg) buffer
      dtv%chars = buffer
   end subroutine

   subroutine argument_write_unformatted(dtv, unit, iostat, iomsg)
      class(argument), intent(in)   :: dtv !! The argument.
      integer, intent(in)           :: unit !! Logical unit.
      integer, intent(out)          :: iostat !! IO status code.
      character(*), intent(inout)   :: iomsg !! IO status message.

      if (allocated(dtv%chars)) then
         write (unit, iostat=iostat, iomsg=iomsg) dtv%chars
      else
         write (unit, iostat=iostat, iomsg=iomsg) ''
      end if
   end subroutine
end module

program main;
   use applibrary;
   implicit none;
   type(argument), allocatable :: args(:);
   args = split_commandline_into_args();
   if (size(args) > 1) then;
      call startup(args(2:));
   else;
      deallocate(args); allocate(args(0));
      call startup(args);
   end if;
contains
#define cast(var, T) \
select type (var) ;\
type is (T)
#define endcast end select
   subroutine startup(args)
      type(argument), intent(in) :: args(:);
      type(application) :: app;
      block
         use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit, stdin => input_unit
         use fpx_macro
         use fpx_logging, only: nocolor
         use fpx_parser
         use fpx_string
         use fpx_os

         integer :: i, j, nargs
         character(:), allocatable :: infile, outfile
         character(*), parameter :: version = "1.2.2"

         call add(global%macros, macro('__STDF__','1'))
         call add(global%macros, macro('__FPX__','1'))
         associate(os => get_os_type())
            if (os == OS_WINDOWS .or. os == OS_WINDOWSx86) then
               call add(global%macros, macro('_WIN32'))
               if (os /= OS_WINDOWSx86) call add(global%macros, macro('_WIN64'))
            end if
         end associate

         nargs = size(args)
         i = 1

         do while (i <= nargs)
            if (len(args(i)%chars) < 1) cycle
            if (args(i)%chars(1:1) == '-') then
               select case(args(i)%chars(2:))
                case ('D')
                  if (len(args(i)%chars) > 2) then
                     j = index(args(i)%chars(2:), '=')
                     if (j > 0) then
                        call add(global%macros, macro(args(i)%chars(3:j-1), args(i)%chars(j+1:)))
                     else
                        call add(global%macros, macro(args(i)%chars(3:)))
                     end if
                  end if
                case ('U')
                  if (.not. allocated(global%undef)) allocate(global%undef(0))
                  if (len(args(i)%chars) > 2) then
                     global%undef = [global%undef, string(args(i)%chars(3:))]
                  end if
                case ('I')
                  if (.not. allocated(global%includedir)) allocate(global%includedir(0))
                  if (len(args(i)%chars) > 2) then
                     global%includedir = [global%includedir, string(args(i)%chars(3:))]
                  end if
                case ('v', '-version')
                  write(*, '(*(A,/))') 'Version:          '//version,     &
                     'Description:      Fortran preprocessor in modern Fortran', &
                     'Copyright (C):    2025 davidpfister', &
                     'License:          MIT', &
                     '', &
                     'This is free software; see the source for copying conditions.  There is NO', &
                     'warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'
                  stop 0, quiet = .true.
                case ('h', '?', '-help')
                  write(*, '(*(A,/))') '                              fpx preprocessor help', &
                     '                             =======================', &
                     'fpx is a extended preprocessor for modern Fortran in Fortran.', &
                     '', &
                     '                             Preprocessor Option List', &
                     '                             -----------------------', &
                     '-D<macro>             Define a <macro> with no value.', &
                     '-D<macro>=<val>       Define a <macro> with <val> as its value.', &
                     '-U<macro>             Undefine <macro>.', &
                     '-I<dir>               Add <dir> to the end of the global include paths.', &
                     '-h, -?, --help        Display help messages.', &
                     '-o, --output          Output file path with name and extension.', &
                     '-v, --version         Display the version of the program.', &
                     '--no-color            Disable ANSI coloring.', &
                     '--implicit-conti      Activate implicit continuation line in macro expansion.', &
                     '--no-exlicit-conti    Deactivate explicit continuation line in macro expansion using & symbol.', &
                     '--exclude-comments    Exclude comments from macro expansion.', &
                     '--no-macros           Deactivate macros expansion.', &
                     '--no-$                Deactivate ${} substitution in macro expansion.', &
                     '--std                 Enforce standard compliance.'
                  stop 0, quiet = .true.
                case ('o', '-output')
                  outfile = args(i)
                case ('-no-color')
                  nocolor = .true.
                case ('-implicit-conti')
                  global%implicit_continuation = .true.
                case ('-exclude-comments')
                  global%exclude_comments = .true.
                case ('-no-macros')
                  global%expand_macros = .false.
                case ('-no-$')
                  global%support_dollar_insert = .false.
                case ('-no-exlicit-conti')
                  global%disable_continuation = .true.
                case ('-std')
                  global%support_forloop = .false.
                  global%extra_macros = .false.
                  global%support_dollar_insert = .false.
                  global%implicit_continuation = .false.
                  global%disable_continuation = .false.
               end select
            else
               if (allocated(infile)) then
                  outfile = args(i)
               else
                  infile = args(i)
               end if
            end if
            i = i + 1
         end do

         if (allocated(infile)) then
            if (allocated(outfile)) then
               call preprocess(trim(infile), trim(outfile))
            else
               call preprocess(trim(infile), stdout)
            end if
         else
            if (allocated(outfile)) then
               call preprocess(stdin, trim(outfile))
            else
               global%interactive = .true.

               write(*, '(A)')
               write(*, '(A)') '   Welcome to fpx, the extended Fortran preprocessor. '
               write(*, '(A)') '   The program can be exited at any time by hitting'
               write(*, '(A)') '   "Enter" at the prompt without entering any data, '
               write(*, '(A)') '   or with the "quit" command.'
               write(*, '(A)')
               call preprocess(stdin, stdout)
            end if
         end if

      end block;
   end subroutine
end
