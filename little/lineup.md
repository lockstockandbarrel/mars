### Name
   lineup(1f) - align strings in Fortran declarations
   (LICENSE:PD)

### Description
   A simple filter that finds and aligns a string (defaults to "::") in a
   block of text. 

   The command is typically called to process a range of lines from inside
   an editor.

   Add these to your ~/.exrc (vi config file) or ~/.vimrc (vim config
   file) file:
```text
        map , #
        map #a mamb
        map #b mb
        map #l 'a!'blineup
        map #q 'a!'blineup '", &'
```
   where "^M" is an actual ctrl-M character. In vi/vim in input mode
   enter ctrl-V and then ctrl-M to insert the ctrl-M into the file.

   Then edit a file and enter ",a" (for above) on the top line of the
   region to filter and ",b" (for below) on the lower line and then enter
   ".l" to call `lineup`. 

   A stylistic preference for Fortran declaration statements:

### Sample input:
```fortran
   logical :: glob
   character(len=*) :: tame ! A string without wildcards
   character(len=*) :: wild ! A corresponding wildcards string
   character(len=len(tame)+1) :: tametext
   character(len=len(wild)+1):: wildtext
   character(len=1),parameter :: NULL=char(0)
   integer  :: wlen
   integer:: ti, wi
   integer  :: i
   character(len=:),allocatable :: tbookmark, wbookmark
```
### Sample output:

```fortran
   logical                      :: glob
   character(len=*)             :: tame ! A string without wildcards
   character(len=*)             :: wild ! A corresponding wildcards string
   character(len=len(tame)+1)   :: tametext
   character(len=len(wild)+1)   :: wildtext
   character(len=1),parameter   :: NULL=char(0)
   integer                      :: wlen
   integer                      :: ti, wi
   integer                      :: i
   character(len=:),allocatable :: tbookmark, wbookmark
```
### using _lineup '", &'_

From "A Tale of Two Cities" by Charles Dickens:
```fortran
character(len=*),parameter :: quote(*)=[character len=80) :: &
"""It was the best of times, it was the worst of times,", &
"it was the age of wisdom, it was the age of foolishness,", &
"it was the epoch of belief, it was the epoch of incredulity,", &
"it was the season of Light, it was the season of Darkness,", &
"it was the spring of hope, it was the winter of despair.""", &
```
```fortran
character(len=*),parameter :: quote(*)=[character len=80) :: &
"""It was the best of times, it was the worst of times,       ", &
"it was the age of wisdom, it was the age of foolishness,     ", &
"it was the epoch of belief, it was the epoch of incredulity, ", &
"it was the season of Light, it was the season of Darkness,   ", &
"it was the spring of hope, it was the winter of despair.""   ", &
```

    open [lineup](https://beliavsky.github.io/fortran-playground/#code=module%20m_module%0A%20%20%20use%2C%20intrinsic%20%3A%3A%20iso_fortran_env%2C%20only%20%3A%20stdin%20%3D%3E%20input_unit%2C%20stdout%20%3D%3E%20output_unit%2C%20stderr%20%3D%3E%20error_unit%0A%20%20%20use%2Cintrinsic%20%20%3A%3A%20iso_c_binding%2C%20%20%20only%20%3A%20c_int%2C%20c_char%0A%20%20%20implicit%20none%0A%20%20%20private%0A%20%20%20public%20%3A%3A%20readstdin%2C%20get_arg%2C%20pad%2C%20report%2C%20stdin%2C%20stdout%2C%20stderr%0Acontains%0A%0A%20%20%20function%20get_arg%28n%2Cdefault%29%20result%28arg%29%0A%20%20%20%20%20%20integer%2Cintent%28in%29%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20n%0A%20%20%20%20%20%20character%28len%3D%2A%29%2Cintent%28in%29%20%20%3A%3A%20default%0A%20%20%20%20%20%20integer%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20argument_length%0A%20%20%20%20%20%20character%28len%3D%3A%29%2Callocatable%20%3A%3A%20arg%0A%0A%20%20%20%20%20%20call%20get_command_argument%28%20number%3Dn%2C%20length%3Dargument_length%20%29%0A%20%20%20%20%20%20if%20%28%20allocated%28arg%29%20%29%20deallocate%28%20arg%20%29%0A%20%20%20%20%20%20allocate%28%20character%28len%3Dargument_length%29%20%3A%3A%20arg%20%29%0A%20%20%20%20%20%20call%20get_command_argument%28n%2C%20arg%20%29%0A%20%20%20%20%20%20if%20%28%20arg%20%3D%3D%20%27%27%20%29arg%20%3D%20default%0A%20%20%20end%20function%20get_arg%0A%0A%20%20%20subroutine%20readstdin%28pageout%29%0A%20%20%20%20%20%20character%28len%3D%3A%29%2Callocatable%2Cintent%28out%29%20%3A%3A%20pageout%28%3A%29%0A%20%20%20%20%20%20character%28len%3D1%29%2Callocatable%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20text%28%3A%29%0A%0A%20%20%20%20%20%20call%20filebyte%28text%29%0A%0A%20%20%20%20%20%20if%20%28.not.allocated%28text%29%29%20then%0A%20%20%20%20%20%20%20%20%20call%20report%28%20%27%2Areadstdin%2A%20failed%20to%20load%20file%20%27%29%0A%20%20%20%20%20%20else%0A%20%20%20%20%20%20%20%20%20pageout%20%3D%20page%28text%29%0A%20%20%20%20%20%20%20%20%20deallocate%28text%29%0A%20%20%20%20%20%20endif%0A%0A%20%20%20contains%0A%20%20%20%20%20%20function%20page%28array%29%20%20result%20%28table%29%0A%0A%20%20%20%20%20%20%20%20%20character%28len%3D1%29%2Cintent%28in%29%20%20%3A%3A%20array%28%3A%29%0A%20%20%20%20%20%20%20%20%20character%28len%3D%3A%29%2Callocatable%20%3A%3A%20table%28%3A%29%0A%20%20%20%20%20%20%20%20%20integer%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20i%2C%20linelength%2C%20length%2C%20lines%2C%20linecount%2C%20position%2C%20sz%0A%20%20%20%20%20%20%20%20%20character%28len%3D1%29%2Cparameter%20%20%20%3A%3A%20nl%20%3D%20char%2810%29%2C%20cr%20%3D%20char%2813%29%0A%0A%20%20%20%20%20%20%20%20%20lines%20%3D%200%0A%20%20%20%20%20%20%20%20%20linelength%20%3D%200%0A%20%20%20%20%20%20%20%20%20length%20%3D%200%0A%20%20%20%20%20%20%20%20%20sz%20%3D%20size%28array%29%0A%20%20%20%20%20%20%20%20%20do%20i%20%3D%201%2Csz%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20%28%20array%28i%29%20%3D%3D%20nl%20%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20linelength%20%3D%20max%28linelength%2Clength%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20lines%20%3D%20lines%20%2B%201%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20length%20%3D%200%0A%20%20%20%20%20%20%20%20%20%20%20%20else%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20length%20%3D%20length%20%2B%201%0A%20%20%20%20%20%20%20%20%20%20%20%20endif%0A%20%20%20%20%20%20%20%20%20enddo%0A%20%20%20%20%20%20%20%20%20if%20%28%20sz%20%3E%200%20%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20%28%20array%28sz%29%20%2F%3D%20nl%20%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20lines%20%3D%20lines%2B1%0A%20%20%20%20%20%20%20%20%20%20%20%20endif%0A%20%20%20%20%20%20%20%20%20endif%0A%0A%20%20%20%20%20%20%20%20%20allocate%28character%28len%3Dlinelength%29%20%3A%3A%20table%28lines%29%20%29%0A%20%20%20%20%20%20%20%20%20table%28%3A%29%20%3D%20%27%20%27%0A%0A%20%20%20%20%20%20%20%20%20linecount%20%3D%201%0A%20%20%20%20%20%20%20%20%20position%20%3D%201%0A%20%20%20%20%20%20%20%20%20do%20i%20%3D%201%2Csz%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20%28%20array%28i%29%20%3D%3D%20nl%20%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20linecount%20%3D%20linecount%2B1%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20position%20%3D%201%0A%20%20%20%20%20%20%20%20%20%20%20%20elseif%20%28%20array%28i%29%20%3D%3D%20cr%20%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20elseif%20%28%20linelength%20%2F%3D%200%20%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20table%28linecount%29%28position%3Aposition%29%20%3D%20array%28i%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20position%20%3D%20position%2B1%0A%20%20%20%20%20%20%20%20%20%20%20%20endif%0A%20%20%20%20%20%20%20%20%20enddo%0A%20%20%20%20%20%20end%20function%20page%0A%0A%20%20%20end%20subroutine%20readstdin%0A%0A%20%20%20function%20getchar%28ch%29%0A%20%20%20%20%20%20character%28len%3D1%29%2Cintent%28out%29%20%3A%3A%20ch%0A%20%20%20%20%20%20integer%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20getchar%0A%20%20%20%20%20%20integer%28kind%3Dc_int%29%20%20%20%20%20%20%20%20%20%20%3A%3A%20ich%0A%20%20%20%20%20%20interface%0A%20%20%20%20%20%20%20%20%20integer%28kind%3Dc_int%29%20function%20system_getchar%28%29%20bind%20%28C%2Cname%3D%22getchar%22%29%0A%20%20%20%20%20%20%20%20%20%20%20%20import%20c_int%0A%20%20%20%20%20%20%20%20%20end%20function%20system_getchar%0A%20%20%20%20%20%20end%20interface%0A%20%20%20%20%20%20ich%20%3D%20system_getchar%28%29%0A%20%20%20%20%20%20if%20%28ich%3E%3D0%29%20then%0A%20%20%20%20%20%20%20%20%20ch%20%3D%20char%28ich%29%0A%20%20%20%20%20%20else%0A%20%20%20%20%20%20%20%20%20ch%20%3D%20char%280%29%0A%20%20%20%20%20%20endif%0A%20%20%20%20%20%20getchar%20%3D%20ich%0A%20%20%20end%20function%20getchar%0A%0A%20%20%20subroutine%20filebyte%28text%29%0A%20%20%20%20%20%20character%28len%3D1%29%2Callocatable%2Cintent%28out%29%20%3A%3A%20text%28%3A%29%0A%20%20%20%20%20%20integer%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20nchars%2C%20igetunit%2C%20iostat%0A%20%20%20%20%20%20character%28len%3D256%29%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20message%0A%0A%20%20%20%20%20%20nchars%20%3D%200%0A%20%20%20%20%20%20iostat%20%3D%200%0A%20%20%20%20%20%20message%20%3D%20%27%27%0A%20%20%20%20%20%20call%20copystdin_c%28%29%0A%0A%20%20%20%20%20%20if%20%28iostat%20%3D%3D%200%29%20then%0A%20%20%20%20%20%20%20%20%20inquire%28unit%3Digetunit%2C%20size%3Dnchars%29%0A%20%20%20%20%20%20%20%20%20if%20%28nchars%20%3C%3D%200%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20call%20report%28%20%27%2Afilebyte%2A%20empty%20file%20%27%29%0A%20%20%20%20%20%20%20%20%20%20%20%20return%0A%20%20%20%20%20%20%20%20%20endif%0A%0A%20%20%20%20%20%20%20%20%20allocate%20%28%20text%28nchars%29%20%29%0A%20%20%20%20%20%20%20%20%20read%28igetunit%2Ciostat%3Diostat%2Ciomsg%3Dmessage%29%20text%0A%20%20%20%20%20%20%20%20%20if%20%28iostat%20%2F%3D%200%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20call%20report%28%20%27%2Afilebyte%2A%20bad%20read%20of%20%27%2F%2F%27%3A%27%2F%2Ftrim%28message%29%20%29%0A%20%20%20%20%20%20%20%20%20endif%0A%20%20%20%20%20%20else%0A%20%20%20%20%20%20%20%20%20call%20report%28%27%2Afilebyte%2A%20%27%2F%2Fmessage%29%0A%20%20%20%20%20%20%20%20%20allocate%20%28%20text%280%29%20%29%0A%20%20%20%20%20%20endif%0A%0A%20%20%20%20%20%20close%28iostat%3Diostat%2Cunit%3Digetunit%29%0A%0A%20%20%20contains%0A%20%20%20%20%20%20subroutine%20copystdin_c%28%29%0A%20%20%20%20%20%20%20%20%20integer%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20iostat%0A%20%20%20%20%20%20%20%20%20character%28len%3D1%29%20%20%20%3A%3A%20byte%0A%20%20%20%20%20%20%20%20%20character%28len%3D255%29%20%3A%3A%20iomsg%0A%0A%20%20%20%20%20%20%20%20%20open%28newunit%3Digetunit%2C%20iomsg%3Diomsg%2C%26%0A%20%20%20%20%20%20%20%20%20%26form%3D%22unformatted%22%2C%20access%3D%22stream%22%2Cstatus%3D%27scratch%27%2Ciostat%3Diostat%29%0A%20%20%20%20%20%20%20%20%20open%28unit%3Dstdin%2Cpad%3D%27yes%27%29%0A%0A%20%20%20%20%20%20%20%20%20if%20%28iostat%20%3D%3D%200%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20do%20while%20%28getchar%28byte%29%3E%3D0%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20write%28igetunit%2Ciostat%3Diostat%2Ciomsg%3Diomsg%29byte%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20if%20%28iostat%20%2F%3D%200%29exit%0A%20%20%20%20%20%20%20%20%20%20%20%20enddo%0A%20%20%20%20%20%20%20%20%20endif%0A%0A%20%20%20%20%20%20%20%20%20if%20%28iostat%20%2F%3D%200%29%20then%0A%20%20%20%20%20%20%20%20%20%20%20%20call%20report%28%27%3CERROR%3E%2Acopystdin%2A%20%27%2F%2Ftrim%28iomsg%29%29%0A%20%20%20%20%20%20%20%20%20endif%0A%0A%20%20%20%20%20%20%20%20%20rewind%28igetunit%2Ciostat%3Diostat%2Ciomsg%3Diomsg%29%0A%0A%20%20%20%20%20%20end%20subroutine%20copystdin_c%0A%0A%20%20%20end%20subroutine%20filebyte%0A%0A%20%20%20function%20pad%28line%2Clength%29%20result%28strout%29%0A%0A%20%20%20%20%20%20character%28len%3D%2A%29%2Cintent%28in%29%20%20%20%20%20%20%20%20%20%20%3A%3A%20line%0A%20%20%20%20%20%20integer%2Cintent%28in%29%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20length%0A%20%20%20%20%20%20character%28len%3D%3A%29%2Callocatable%20%20%20%20%20%20%20%20%20%3A%3A%20strout%0A%0A%20%20%20%20%20%20allocate%28character%28len%3Dmax%28length%2Clen%28line%29%29%29%20%3A%3A%20strout%29%0A%20%20%20%20%20%20strout%28%3A%29%20%3D%20line%2F%2Frepeat%28%27%20%27%2Clen%28strout%29%2F2%2B1%29%0A%0A%20%20%20end%20function%20pad%0A%0A%20%20%20subroutine%20report%28message%29%0A%20%20%20%20%20%20character%28len%3D%2A%29%2Cintent%28in%29%20%3A%3A%20message%0A%20%20%20%20%20%20write%28stderr%2C%27%28a%29%27%29trim%28message%29%0A%20%20%20end%20subroutine%20report%0A%0Aend%20module%20m_module%0A%0Aprogram%20lineup%0A%20%20%20use%20m_module%2C%20only%20%3A%20get_arg%2C%20readstdin%2C%20pad%2C%20stdout%0A%0A%20%20%20implicit%20none%0A%20%20%20character%28len%3D%3A%29%2Callocatable%20%3A%3A%20left%2C%20separator%2C%20pageout%28%3A%29%0A%20%20%20integer%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%3A%3A%20i%2C%20j%2C%20imax%0A%0A%20%20%20separator%20%3D%20get_arg%281%2C%27%3A%3A%27%29%0A%20%20%20call%20readstdin%28pageout%29%0A%20%20%20imax%20%3D%200%0A%20%20%20do%20i%20%3D%201%2Csize%28pageout%29%0A%20%20%20%20%20%20j%20%3D%20index%28pageout%28i%29%2Cseparator%29%0A%20%20%20%20%20%20if%20%28j%20%3E%201%29%20then%0A%20%20%20%20%20%20%20%20%20j%20%3D%20len_trim%28pageout%28i%29%28%3Aj-1%29%29%2B2%0A%20%20%20%20%20%20endif%0A%20%20%20%20%20%20imax%20%3D%20max%28imax%2Cj%29%0A%20%20%20enddo%0A%20%20%20do%20i%20%3D%201%2Csize%28pageout%29%0A%20%20%20%20%20%20j%20%3D%20index%28pageout%28i%29%2Cseparator%29%0A%20%20%20%20%20%20if%20%28%20j%20%3E%3D%202%29%20then%0A%20%20%20%20%20%20%20%20%20left%20%3D%20trim%28pageout%28i%29%28%3Aj-1%29%29%0A%20%20%20%20%20%20%20%20%20write%28stdout%2C%27%28%2A%28g0%29%29%27%29%20pad%28left%2C%20imax-1%29%2C%20separator%2Ctrim%28pageout%28i%29%28j%2B2%3A%29%29%0A%20%20%20%20%20%20else%0A%20%20%20%20%20%20%20%20%20write%28stdout%2C%27%28%2A%28g0%29%29%27%29trim%28pageout%28i%29%29%0A%20%20%20%20%20%20endif%0A%20%20%20enddo%0Aend%20program%20lineup%0A)  in Fortran Playground

[details="source of lineup"]
```fortran
module m_module
   use, intrinsic :: iso_fortran_env, only : stdin => input_unit, stdout => output_unit, stderr => error_unit
   use,intrinsic  :: iso_c_binding,   only : c_int, c_char
   implicit none
   private
   public :: readstdin, get_arg, pad, report, stdin, stdout, stderr
contains

   function get_arg(n,default) result(arg)
      integer,intent(in)           :: n
      character(len=*),intent(in)  :: default
      integer                      :: argument_length
      character(len=:),allocatable :: arg

      call get_command_argument( number=n, length=argument_length )
      if ( allocated(arg) ) deallocate( arg )
      allocate( character(len=argument_length) :: arg )
      call get_command_argument(n, arg )
      if ( arg == '' )arg = default
   end function get_arg

   subroutine readstdin(pageout)
      character(len=:),allocatable,intent(out) :: pageout(:)
      character(len=1),allocatable             :: text(:)

      call filebyte(text)

      if (.not.allocated(text)) then
         call report( '*readstdin* failed to load file ')
      else
         pageout = page(text)
         deallocate(text)
      endif

   contains
      function page(array)  result (table)

         character(len=1),intent(in)  :: array(:)
         character(len=:),allocatable :: table(:)
         integer                      :: i, linelength, length, lines, linecount, position, sz
         character(len=1),parameter   :: nl = char(10), cr = char(13)

         lines = 0
         linelength = 0
         length = 0
         sz = size(array)
         do i = 1,sz
            if ( array(i) == nl ) then
               linelength = max(linelength,length)
               lines = lines + 1
               length = 0
            else
               length = length + 1
            endif
         enddo
         if ( sz > 0 ) then
            if ( array(sz) /= nl ) then
               lines = lines+1
            endif
         endif

         allocate(character(len=linelength) :: table(lines) )
         table(:) = ' '

         linecount = 1
         position = 1
         do i = 1,sz
            if ( array(i) == nl ) then
               linecount = linecount+1
               position = 1
            elseif ( array(i) == cr ) then
            elseif ( linelength /= 0 ) then
               table(linecount)(position:position) = array(i)
               position = position+1
            endif
         enddo
      end function page

   end subroutine readstdin

   function getchar(ch)
      character(len=1),intent(out) :: ch
      integer                      :: getchar
      integer(kind=c_int)          :: ich
      interface
         integer(kind=c_int) function system_getchar() bind (C,name="getchar")
            import c_int
         end function system_getchar
      end interface
      ich = system_getchar()
      if (ich>=0) then
         ch = char(ich)
      else
         ch = char(0)
      endif
      getchar = ich
   end function getchar

   subroutine filebyte(text)
      character(len=1),allocatable,intent(out) :: text(:)
      integer                                  :: nchars, igetunit, iostat
      character(len=256)                       :: message

      nchars = 0
      iostat = 0
      message = ''
      call copystdin_c()

      if (iostat == 0) then
         inquire(unit=igetunit, size=nchars)
         if (nchars <= 0) then
            call report( '*filebyte* empty file ')
            return
         endif

         allocate ( text(nchars) )
         read(igetunit,iostat=iostat,iomsg=message) text
         if (iostat /= 0) then
            call report( '*filebyte* bad read of '//':'//trim(message) )
         endif
      else
         call report('*filebyte* '//message)
         allocate ( text(0) )
      endif

      close(iostat=iostat,unit=igetunit)

   contains
      subroutine copystdin_c()
         integer            :: iostat
         character(len=1)   :: byte
         character(len=255) :: iomsg

         open(newunit=igetunit, iomsg=iomsg,&
         &form="unformatted", access="stream",status='scratch',iostat=iostat)
         open(unit=stdin,pad='yes')

         if (iostat == 0) then
            do while (getchar(byte)>=0)
               write(igetunit,iostat=iostat,iomsg=iomsg)byte
               if (iostat /= 0)exit
            enddo
         endif

         if (iostat /= 0) then
            call report('<ERROR>*copystdin* '//trim(iomsg))
         endif

         rewind(igetunit,iostat=iostat,iomsg=iomsg)

      end subroutine copystdin_c

   end subroutine filebyte

   function pad(line,length) result(strout)

      character(len=*),intent(in)          :: line
      integer,intent(in)                   :: length
      character(len=:),allocatable         :: strout

      allocate(character(len=max(length,len(line))) :: strout)
      strout(:) = line//repeat(' ',len(strout)/2+1)

   end function pad

   subroutine report(message)
      character(len=*),intent(in) :: message
      write(stderr,'(a)')trim(message)
   end subroutine report

end module m_module

program lineup
   use m_module, only : get_arg, readstdin, pad, stdout

   implicit none
   character(len=:),allocatable :: left, separator, pageout(:)
   integer                      :: i, j, imax

   separator = get_arg(1,'::')
   call readstdin(pageout)
   imax = 0
   do i = 1,size(pageout)
      j = index(pageout(i),separator)
      if (j > 1) then
         j = len_trim(pageout(i)(:j-1))+2
      endif
      imax = max(imax,j)
   enddo
   do i = 1,size(pageout)
      j = index(pageout(i),separator)
      if ( j >= 2) then
         left = trim(pageout(i)(:j-1))
         write(stdout,'(*(g0))') pad(left, imax-1), separator,trim(pageout(i)(j+2:))
      else
         write(stdout,'(*(g0))')trim(pageout(i))
      endif
   enddo
end program lineup
```
[/details]
