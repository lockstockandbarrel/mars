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
