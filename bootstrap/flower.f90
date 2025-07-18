! d7997ccd-145b-4ca1-9d13-aa166affe43f
!>>>>> build/dependencies/M_CLI2/src/M_CLI2.F90
!VERSION 1.0 20200115
!VERSION 2.0 20200802
!VERSION 3.0 20201021  LONG:SHORT syntax
!VERSION 3.1 20201115  LONG:SHORT:: syntax
!VERSION 3.2 20230205  set_mode()
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_CLI2(3fm) - [ARGUMENTS::M_CLI2::INTRO] command line argument
!!    parsing using a prototype command
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Available procedures and variables:
!!
!!      ! basic procedures
!!      use M_CLI2, only : set_args, get_args, specified, set_mode
!!      ! convenience functions
!!      use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!      use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!      ! variables
!!      use M_CLI2, only : unnamed, remaining, args
!!      ! working with non-allocatable strings and arrays
!!      use M_CLI2, only : get_args_fixed_length, get_args_fixed_size
!!      ! special function for creating subcommands
!!      use M_CLI2, only : get_subcommand(3f)
!!
!!##DESCRIPTION
!!    The M_CLI2 module cracks a Unix-style command line.
!!
!!    Typically one call to SET_ARGS(3f) is made to define the command
!!    arguments, set default values and parse the command line. Then a call
!!    is made to the convenience procedures or GET_ARGS(3f) proper for each
!!    command keyword to obtain the argument values.
!!
!!    Detailed descriptions of each procedure and example programs are
!!    included.
!!
!!##EXAMPLE
!!
!!
!! Sample minimal program which may be called in various ways:
!!
!!     mimimal -x 100.3 -y 3.0e4
!!     mimimal --xvalue=300 --debug
!!     mimimal --yvalue 400
!!     mimimal -x 10 file1 file2 file3
!!
!! Program example:
!!
!!     program minimal
!!     use M_CLI2,  only : set_args, lget, rget, sgets
!!     implicit none
!!     real    :: x, y
!!     integer :: i
!!     character(len=:),allocatable :: filenames(:)
!!        ! define and crack command line
!!        call set_args(' --yvalue:y 0.0 --xvalue:x 0.0 --debug F')
!!        ! get values
!!        x=rget('xvalue')
!!        y=rget('yvalue')
!!        if(lget('debug'))then
!!           write(*,*)'X=',x
!!           write(*,*)'Y=',y
!!           write(*,*)'ATAN2(Y,X)=',atan2(x=x,y=y)
!!        else
!!           write(*,*)atan2(x=x,y=y)
!!        endif
!!        filenames=sgets() ! sget with no name gets "unnamed" values
!!        if(size(filenames) > 0)then
!!           write(*,'(g0)')'filenames:'
!!           write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!        endif
!!     end program minimal
!!
!! Sample program using get_args() and variants
!!
!!     program demo_M_CLI2
!!     use M_CLI2,  only : set_args, get_args
!!     use M_CLI2,  only : filenames=>unnamed
!!     use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     integer,parameter            :: dp=kind(0.0d0)
!!      !
!!      ! Define ARGS
!!     real                         :: x, y, z
!!     logical                      :: l, lbig
!!     character(len=40)            :: label    ! FIXED LENGTH
!!     real(kind=dp),allocatable    :: point(:)
!!     logical,allocatable          :: logicals(:)
!!     character(len=:),allocatable :: title    ! VARIABLE LENGTH
!!     real                         :: p(3)     ! FIXED SIZE
!!     logical                      :: logi(3)  ! FIXED SIZE
!!      !
!!      ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!      !   o set a value for all keywords.
!!      !   o double-quote strings, strings must be at least one space
!!      !     because adjacent double-quotes designate a double-quote
!!      !     in the value.
!!      !   o set all logical values to F
!!      !   o numeric values support an "e" or "E" exponent
!!      !   o for lists delimit with a comma, colon, or space
!!     call set_args('                         &
!!             & -x 1 -y 2 -z 3                &
!!             & -p -1 -2 -3                   &
!!             & --point 11.11, 22.22, 33.33e0 &
!!             & --title "my title" -l F -L F  &
!!             & --logicals  F F F F F         &
!!             & --logi F T F                  &
!!             & --label " " &
!!             ! note space between quotes is required
!!             & ')
!!      ! Assign values to elements using G_ARGS(3f).
!!      ! non-allocatable scalars can be done up to twenty per call
!!     call get_args('x',x, 'y',y, 'z',z, 'l',l, 'L',lbig)
!!      ! As a convenience multiple pairs of keywords and variables may be
!!      ! specified if and only if all the values are scalars and the CHARACTER
!!      ! variables are fixed-length or pre-allocated.
!!      !
!!      ! After SET_ARGS(3f) has parsed the command line
!!      ! GET_ARGS(3f) retrieves the value of keywords accept for
!!      ! two special cases. For fixed-length CHARACTER variables
!!      ! see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
!!      ! GET_ARGS_FIXED_SIZE(3f).
!!      !
!!      ! allocatables should be done one at a time
!!     call get_args('title',title) ! allocatable string
!!     call get_args('point',point) ! allocatable arrays
!!     call get_args('logicals',logicals)
!!      !
!!      ! less commonly ...
!!
!!      ! for fixed-length strings
!!     call get_args_fixed_length('label',label)
!!
!!      ! for non-allocatable arrays
!!     call get_args_fixed_size('p',p)
!!     call get_args_fixed_size('logi',logi)
!!      !
!!      ! all done parsing, use values
!!     write(*,*)'x=',x, 'y=',y, 'z=',z, x+y+z
!!     write(*,*)'p=',p
!!     write(*,*)'point=',point
!!     write(*,*)'title=',title
!!     write(*,*)'label=',label
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     write(*,*)'logicals=',logicals
!!     write(*,*)'logi=',logi
!!      !
!!      ! unnamed strings
!!      !
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!      !
!!     end program demo_M_CLI2
!!
!!##AUTHOR
!!     John S. Urban, 2019
!!##LICENSE
!!     Public Domain
!!##SEE ALSO
!!     + get_args(3f)
!!     + get_args_fixed_size(3f)
!!     + get_args_fixed_length(3f)
!!     + get_subcommand(3f)
!!     + set_mode(3f)
!!     + specified(3f)
!!
!! Note that the convenience routines are described under get_args(3f):
!! dget(3f), iget(3f), lget(3f), rget(3f), sget(3f), cget(3f) dgets(3f),
!! igets(3f), lgets(3f), rgets(3f), sgets(3f), cgets(3f)
!===================================================================================================================================
module M_CLI2
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT, warn=>OUTPUT_UNIT
implicit none
private

integer,parameter,private :: dp=kind(0.0d0)
integer,parameter,private :: sp=kind(0.0)

character(len=*),parameter          :: gen='(*(g0))'
character(len=:),allocatable,public :: unnamed(:)
character(len=:),allocatable,public :: args(:)
character(len=:),allocatable,public :: remaining
public                              :: set_mode
public                              :: set_args
public                              :: get_subcommand
public                              :: get_args
public                              :: get_args_fixed_size
public                              :: get_args_fixed_length
public                              :: specified
public                              :: print_dictionary

public                              :: dget, iget, lget, rget, sget, cget
public                              :: dgets, igets, lgets, rgets, sgets, cgets

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
   logical                  :: mandatory
end type option

character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: shorts(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save          :: counts(:)
logical,allocatable,save          :: present_in(:)
logical,allocatable,save          :: mandatory(:)

logical,save                      :: G_DEBUG=.false.
logical,save                      :: G_UNDERDASH=.false.
logical,save                      :: G_NOSEPARATOR=.false.
logical,save                      :: G_IGNORECASE=.false.      ! ignore case of long keywords
logical,save                      :: G_STRICT=.false.          ! strict short and long rules or allow -longname and --shortname
logical,save                      :: G_APPEND=.true.           ! whether to append or replace when duplicate keywords found

logical,save                      :: G_keyword_single_letter=.true.
character(len=:),allocatable,save :: G_passed_in
logical,save                      :: G_remaining_on, G_remaining_option_allowed
character(len=:),allocatable,save :: G_remaining
character(len=:),allocatable,save :: G_subcommand              ! possible candidate for a subcommand
character(len=:),allocatable,save :: G_STOP_MESSAGE
integer,save                      :: G_STOP
logical,save                      :: G_QUIET
character(len=:),allocatable,save :: G_PREFIX

! try out response files
! CLI_RESPONSE_FILE is left public for backward compatibility, but should be set via "set_mode('response_file')
logical,save,public               :: CLI_RESPONSE_FILE=.false. ! allow @name abbreviations
logical,save                      :: G_OPTIONS_ONLY            ! process response file only looking for options for get_subcommand()
logical,save                      :: G_RESPONSE                ! allow @name abbreviations
character(len=:),allocatable,save :: G_RESPONSE_IGNORED

! return allocatable arrays
interface  get_args;  module  procedure  get_anyarray_d;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_i;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_r;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_x;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_c;  end interface  ! any size array and any length
interface  get_args;  module  procedure  get_anyarray_l;  end interface  ! any size array

! return scalars
interface  get_args;  module  procedure  get_scalar_d;               end interface
interface  get_args;  module  procedure  get_scalar_i;               end interface
interface  get_args;  module  procedure  get_scalar_real;            end interface
interface  get_args;  module  procedure  get_scalar_complex;         end interface
interface  get_args;  module  procedure  get_scalar_logical;         end interface
interface  get_args;  module  procedure  get_scalar_anylength_c;     end interface  ! any length

! multiple scalars
interface  get_args;  module  procedure  many_args;               end  interface

! return non-allocatable arrays
! said in conflict with get_args_*. Using class to get around that.
! that did not work either. Adding size parameter as optional parameter works; but using a different name
interface  get_args_fixed_size;  module procedure get_fixedarray_class;            end interface ! any length, fixed size array
!interface   get_args;           module procedure get_fixedarray_d;                end interface
!interface   get_args;           module procedure get_fixedarray_i;                end interface
!interface   get_args;           module procedure get_fixedarray_r;                end interface
!interface   get_args;           module procedure get_fixedarray_l;                end interface
!interface   get_args;           module procedure get_fixedarray_fixed_length_c;   end interface

interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_a_array; end interface  ! fixed length any size array
interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_scalar_c;  end interface       ! fixed length

! Generic subroutine inserts element into allocatable array at specified position

! find PLACE in sorted character array where value can be found or should be placed
interface  locate_;  module procedure locate_c                            ; end interface

! insert entry into a sorted allocatable array at specified position
interface  insert_;  module procedure insert_c,      insert_i,  insert_l  ; end interface

! replace entry by index from a sorted allocatable array if it is present
interface  replace_; module procedure replace_c,     replace_i, replace_l ; end interface

! delete entry by index from a sorted allocatable array if it is present
interface  remove_;  module procedure remove_c,      remove_i,  remove_l  ; end interface

! convenience functions
interface cgets;module procedure cgs, cg;end interface
interface dgets;module procedure dgs, dg;end interface
interface igets;module procedure igs, ig;end interface
interface lgets;module procedure lgs, lg;end interface
interface rgets;module procedure rgs, rg;end interface
interface sgets;module procedure sgs, sg;end interface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    check_commandline(3f) - [ARGUMENTS:M_CLI2]check command and process
!!    pre-defined options
!!
!!##SYNOPSIS
!!
!!      subroutine check_commandline(help_text,version_text,ierr,errmsg)
!!
!!       character(len=*),intent(in),optional :: help_text(:)
!!       character(len=*),intent(in),optional :: version_text(:)
!!
!!##DESCRIPTION
!!     Checks the commandline  and processes the implicit --help, --version,
!!     --verbose, and --usage parameters.
!!
!!     If the optional text values are supplied they will be displayed by
!!     --help and --version command-line options, respectively.
!!
!!##OPTIONS
!!
!!     HELP_TEXT     if present, will be displayed if program is called with
!!                   --help switch, and then the program will terminate. If
!!                   not supplied, the command line initialized string will be
!!                   shown when --help is used on the commandline.
!!
!!     VERSION_TEXT  if present, will be displayed if program is called with
!!                   --version switch, and then the program will terminate.
!!
!!        If the first four characters of each line are "@(#)" this prefix
!!        will not be displayed and the last non-blank letter will be
!!        removed from each line. This if for support of the SCCS what(1)
!!        command. If you do not have the what(1) command on GNU/Linux and
!!        Unix platforms you can probably see how it can be used to place
!!        metadata in a binary by entering:
!!
!!         strings demo_commandline|grep '@(#)'|tr '>' '\n'|sed -e 's/  */ /g'
!!
!!##EXAMPLE
!!
!!
!! Typical usage:
!!
!!      program check_commandline
!!      use M_CLI2,  only : unnamed, set_args, get_args
!!      implicit none
!!      integer                      :: i
!!      character(len=:),allocatable :: version_text(:), help_text(:)
!!      real               :: x, y, z
!!      character(len=*),parameter :: cmd='-x 1 -y 2 -z 3'
!!         version_text=[character(len=80) :: "version 1.0","author: me"]
!!         help_text=[character(len=80) :: &
!!                 & "wish I put instructions","here","I suppose?"]
!!         call set_args(cmd,help_text,version_text)
!!         call get_args('x',x,'y',y,'z',z)
!!         ! All done cracking the command line. Use the values in your program.
!!         write (*,*)x,y,z
!!         ! the optional unnamed values on the command line are
!!         ! accumulated in the character array "UNNAMED"
!!         if(size(unnamed) > 0)then
!!            write (*,'(a)')'files:'
!!            write (*,'(i6.6,3a)') (i,'[',unnamed(i),']',i=1,size(unnamed))
!!         endif
!!      end program check_commandline
!===================================================================================================================================
subroutine check_commandline(help_text,version_text)
character(len=*),intent(in),optional :: help_text(:)
character(len=*),intent(in),optional :: version_text(:)
character(len=:),allocatable         :: line
integer                              :: i
integer                              :: istart
integer                              :: iback
   if(get('usage') == 'T')then
      call print_dictionary('USAGE:')
      call mystop(32)
      return
   endif
   if(present(help_text))then
      if(get('help') == 'T')then
         do i=1,size(help_text)
            call journal(help_text(i))
         enddo
         call mystop(1,'displayed help text')
         return
      endif
   elseif(get('help') == 'T')then
      call default_help()
      call mystop(2,'displayed default help text')
      return
   endif
   if(present(version_text))then
      if(get('version') == 'T')then
         istart=1
         iback=0
         if(size(version_text) > 0)then
            if(index(version_text(1),'@'//'(#)') == 1)then ! allow for what(1) syntax
               istart=5
               iback=1
            endif
         endif
         do i=1,size(version_text)
            !xINTEL BUG*!call journal(version_text(i)(istart:len_trim(version_text(i))-iback))
            line=version_text(i)(istart:len_trim(version_text(i))-iback)
            call journal(line)
         enddo
         call mystop(3,'displayed version text')
         return
      endif
   elseif(get('version') == 'T')then

      if(G_QUIET)then
         G_STOP_MESSAGE = 'no version text'
      else
         call journal('*check_commandline* no version text')
      endif
      call mystop(4,'displayed default version text')
      return
   endif
contains
subroutine default_help()
character(len=:),allocatable :: cmd_name
integer :: ilength
   call get_command_argument(number=0,length=ilength)
   if(allocated(cmd_name))deallocate(cmd_name)
   allocate(character(len=ilength) :: cmd_name)
   call get_command_argument(number=0,value=cmd_name)
   G_passed_in=G_passed_in//repeat(' ',len(G_passed_in))
   G_passed_in=replace_str(G_passed_in, ' --', NEW_LINE('A')//' --')
   if(.not.G_QUIET)then
      call journal(cmd_name,G_passed_in) ! no help text, echo command and default options
   endif
   deallocate(cmd_name)
end subroutine default_help
end subroutine check_commandline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_args(3f) - [ARGUMENTS:M_CLI2] command line argument parsing
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_args(prototype,help_text,version_text,ierr,errmsg)
!!
!!      character(len=*),intent(in),optional              :: prototype
!!      character(len=*),intent(in),optional              :: help_text(:)
!!      character(len=*),intent(in),optional              :: version_text(:)
!!      integer,intent(out),optional                      :: ierr
!!      character(len=:),intent(out),allocatable,optional :: errmsg
!!##DESCRIPTION
!!
!!    SET_ARGS(3f) requires a unix-like command prototype which defines
!!    the command-line options and their default values. When the program
!!    is executed this and the command-line options are applied and the
!!    resulting values are placed in an internal table for retrieval via
!!    GET_ARGS(3f).
!!
!!    The built-in --help and --version options require optional help_text
!!    and version_text values to be provided to be particularly useful.
!!
!!##OPTIONS
!!
!!    PROTOTYPE   composed of all command arguments concatenated
!!                into a Unix-like command prototype string. For
!!                example:
!!
!!                 call set_args('-L F --ints 1,2,3 --title "my title" -R 10.3')
!!
!!                The following options are predefined for all commands:
!!                '--verbose F --usage F --help F --version F'.
!!
!!                see "DEFINING THE PROTOTYPE" in the next section for
!!                further details.
!!
!!    HELP_TEXT   if present, will be displayed when the program is called with
!!                a --help switch, and then the program will terminate. If
!!                help text is not supplied the command line initialization
!!                string will be echoed.
!!
!!    VERSION_TEXT  if present, any version text defined will be displayed
!!                  when the program is called with a --version switch,
!!                  and then the program will terminate.
!!    IERR          if present a non-zero option is returned when an
!!                  error occurs instead of the program terminating.
!!    ERRMSG        a description of the error if ierr is present.
!!
!!##DEFINING THE PROTOTYPE
!!
!!    o Keywords start with a single dash for short single-character
!!      keywords, and with two dashes for longer keywords.
!!
!!    o all keywords on the prototype MUST get a value.
!!
!!       * logicals must be set to an unquoted F.
!!
!!       * strings must be delimited with double-quotes.
!!         Since internal double-quotes are represented with two
!!         double-quotes the string must be at least one space.
!!
!!    o numeric keywords are not allowed; but this allows
!!      negative numbers to be used as values.
!!
!!    o lists of values should be comma-delimited unless a
!!      user-specified delimiter is used. The prototype
!!      must use the same array delimiters as the call to
!!      get the value.
!!
!!    o to define a zero-length allocatable array make the
!!      value a delimiter (usually a comma) or an empty set
!!      of braces ("[]").
!!
!!    LONG AND SHORT NAMES
!!
!!    Long keywords start with two dashes followed by more than one letter.
!!    Short keywords are a dash followed by a single letter.
!!
!!    o It is recommended long names (--keyword) should be all lowercase
!!      but are case-sensitive by default, unless "set_mode('ignorecase')"
!!      is in effect.
!!
!!    o Long names should always be more than one character.
!!
!!    o The recommended way to have short names is to suffix the long
!!      name with :LETTER in the definition.
!!
!!      If this syntax is used then logical shorts may be combined on the
!!      command line when "set_mode('strict')" is in effect.
!!
!!    SPECIAL BEHAVIORS
!!
!!    o A special behavior occurs if a keyword name ends in ::.
!!      When the program is called the next parameter is taken as
!!      a value even if it starts with -. This is not generally
!!      recommended but is useful in rare cases where non-numeric
!!      values starting with a dash are desired.
!!
!!    o If the prototype ends with "--" a special mode is turned
!!      on where anything after "--" on input goes into the variable
!!      REMAINING with values double-quoted and also into the array ARGS
!!      instead of becoming elements in the UNNAMED array. This is not
!!      needed for normal processing, but was needed for a program that
!!      needed this behavior for its subcommands.
!!
!!      That is, for a normal call all unnamed values go into UNNAMED
!!      and ARGS and REMAINING are ignored. So for
!!
!!          call set_args('-x 10 -y 20 ')
!!
!!      A program invocation such as
!!
!!          xx a b c -- A B C " dd "
!!
!!      results in
!!
!!       UNNAMED= ['a','b','c','A','B','C',' dd']
!!       REMAINING= ''
!!       ARGS= [character(len=0) :: ] ! ie, an empty character array
!!
!!      Whereas
!!
!!       call set_args('-x 10 -y 20 --')
!!
!!      generates the following output from the same program execution:
!!
!!       UNNAMED= ['a','b','c']
!!       REMAINING= '"A" "B" "C" " dd "'
!!       ARGS= ['A','B','C,' dd']
!!
!!##USAGE NOTES
!!      When invoking the program line note the (subject to change)
!!      following restrictions (which often differ between various
!!      command-line parsers):
!!
!!      o values for duplicate keywords are appended together with a space
!!        separator when a command line is executed by default.
!!
!!      o shuffling is not supported. Values immediately follow their
!!        keywords.
!!
!!      o Only short Boolean keywords can be bundled together.
!!        If allowing bundling is desired call "set_mode('strict')".
!!        This will require prefixing long names with "--" and short
!!        names with "-". Otherwise M_CLI2 relaxes that requirement
!!        and mostly does not care what prefix is used for a keyword.
!!        But this would make it unclear what was meant by "-ox" if
!!        allowed options were "-o F -x F --ox F " for example, so
!!        "strict" mode is required to remove the ambiguity.
!!
!!      o if a parameter value of just "-" is supplied it is
!!        converted to the string "stdin".
!!
!!      o values not needed for a keyword value go into the character
!!        array "UNNAMED".
!!
!!        In addition if the keyword "--" is encountered on the command
!!        line the rest of the command line goes into the character array
!!        "UNNAMED".
!!
!!##EXAMPLE
!!
!!
!! Sample program:
!!
!!     program demo_set_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     use M_CLI2,  only : get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real                         :: p(3)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!     integer,allocatable          :: ints(:)
!!     !
!!     !  DEFINE COMMAND (TO SET INITIAL VALUES AND ALLOWED KEYWORDS)
!!     !  AND READ COMMAND LINE
!!     call set_args(' &
!!        ! reals
!!        & -x 1 -y 2.3 -z 3.4e2 &
!!        ! integer array
!!        & -p -1,-2,-3 &
!!        ! always double-quote strings
!!        & --title "my title" &
!!        ! string should be a single character at a minimum
!!        & --label " ", &
!!        ! set all logical values to F
!!        & -l F -L F &
!!        ! set allocatable size to zero if you like by using a delimiter
!!        & --ints , &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     !     SCALARS
!!     call get_args('x',x)
!!     call get_args('y',y)
!!     call get_args('z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     call get_args('ints',ints)      ! ALLOCATABLE ARRAY
!!     call get_args('title',title)    ! ALLOCATABLE STRING
!!     call get_args_fixed_size('p',p) ! NON-ALLOCATABLE ARRAY
!!     ! USE VALUES
!!     write(*,*)'x=',x
!!     write(*,*)'y=',y
!!     write(*,*)'z=',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'ints=',ints
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     ! UNNAMED VALUES
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_set_args
!!
!!##RESPONSE FILES
!!
!!  If you have no interest in using external files as abbreviations
!!  you can ignore this section. Otherwise, before calling set_args(3f)
!!  add:
!!
!!     use M_CLI2, only : set_mode
!!     call set_mode('response_file')
!!
!!  M_CLI2 Response files are small files containing CLI (Command Line
!!  Interface) arguments that end with ".rsp" that can be used when command
!!  lines are so long that they would exceed line length limits or so complex
!!  that it is useful to have a platform-independent method of creating
!!  an abbreviation.
!!
!!  Shell aliases and scripts are often used for similar purposes (and
!!  allow for much more complex conditional execution, of course), but
!!  they generally cannot be used to overcome line length limits and are
!!  typically platform-specific.
!!
!!  Examples of commands that support similar response files are the Clang
!!  and Intel compilers, although there is no standard format for the files.
!!
!!  They are read if you add options of the syntax "@NAME" as the FIRST
!!  parameters on your program command line calls. They are not recursive --
!!  that is, an option in a response file cannot be given the value "@NAME2"
!!  to call another response file.
!!
!!  More than one response name may appear on a command line.
!!
!!  They are case-sensitive names.
!!
!!  Note "@" s a special character in Powershell, and requires being escaped
!!  with a grave character.
!!
!!   LOCATING RESPONSE FILES
!!
!!  A search for the response file always starts with the current directory.
!!  The search then proceeds to look in any additional directories specified
!!  with the colon-delimited environment variable CLI_RESPONSE_PATH.
!!
!!  The first resource file found that results in lines being processed
!!  will be used and processing stops after that first match is found. If
!!  no match is found an error occurs and the program is stopped.
!!
!!   RESPONSE FILE SECTIONS
!!
!!  A simple response file just has options for calling the program in it
!!  prefixed with the word "options".
!!  But they can also contain section headers to denote selections that are
!!  only executed when a specific OS is being used, print messages, and
!!  execute system commands.
!!
!!   SEARCHING FOR OSTYPE IN REGULAR FILES
!!
!!  So assuming the name @NAME was specified on the command line a file
!!  named NAME.rsp will be searched for in all the search directories
!!  and then in that file a string that starts with the string @OSTYPE
!!  (if the environment variables $OS and $OSTYPE are not blank. $OSTYPE
!!  takes precedence over $OS).
!!
!!   SEARCHING FOR UNLABELED DIRECTIVES IN REGULAR FILES
!!
!!  Then, the same files will be searched for lines above any line starting
!!  with "@". That is, if there is no special section for the current OS
!!  it just looks at the top of the file for unlabeled options.
!!
!!   SEARCHING FOR OSTYPE AND NAME IN THE COMPOUND FILE
!!
!!  In addition or instead of files with the same name as the @NAME option
!!  on the command line, you can have one file named after the executable
!!  name that contains multiple abbreviation names.
!!
!!  So if your program executable is named EXEC you create a single file
!!  called EXEC.rsp and can append all the simple files described above
!!  separating them with lines of the form @OSTYPE@NAME or just @NAME.
!!
!!  So if no specific file for the abbreviation is found a file called
!!  "EXEC.rsp" is searched for where "EXEC" is the name of the executable.
!!  This file is always a "compound" response file that uses the following format:
!!
!!  Any compound EXEC.rsp file found in the current or searched directories
!!  will be searched for the string @OSTYPE@NAME first.
!!
!!  Then if nothing is found, the less specific line @NAME is searched for.
!!
!!   THE SEARCH IS OVER
!!
!!  Sounds complicated but actually works quite intuitively. Make a file in
!!  the current directory and put options in it and it will be used. If that
!!  file ends up needing different cases for different platforms add a line
!!  like "@Linux" to the file and some more lines and that will only be
!!  executed if the environment variable OSTYPE or OS is "Linux". If no match
!!  is found for named sections the lines at the top before any "@" lines
!!  will be used as a default if no match is found.
!!
!!  If you end up using a lot of files like this you can combine them all
!!  together and put them into a file called "program_name".rsp and just
!!  put lines like @NAME or @OSTYPE@NAME at that top of each selection.
!!
!!  Now, back to the details on just what you can put in the files.
!!
!!##SPECIFICATION FOR RESPONSE FILES
!!
!!   SIMPLE RESPONSE FILES
!!
!!  The first word of a line is special and has the following meanings:
!!
!!    options|-  Command options following the rules of the SET_ARGS(3f)
!!               prototype. So
!!                o It is preferred to specify a value for all options.
!!                o double-quote strings.
!!                o give a blank string value as " ".
!!                o use F|T for lists of logicals,
!!                o lists of numbers should be comma-delimited.
!!                o --usage, --help, --version, --verbose, and unknown
!!                  options are ignored.
!!
!!    comment|#  Line is a comment line
!!    system|!   System command.
!!               System commands are executed as a simple call to
!!               system (so a cd(1) or setting a shell variable
!!               would not effect subsequent lines, for example)
!!               BEFORE the command being processed.
!!    print|>    Message to screen
!!    stop       display message and stop program.
!!
!!  NOTE: system commands are executed when encountered, but options are
!!  gathered from multiple option lines and passed together at the end of
!!  processing of the block; so all commands will be executed BEFORE the
!!  command for which options are being supplied no matter where they occur.
!!
!!  So if a program that does nothing but echos its parameters
!!
!!    program testit
!!    use M_CLI2, only : set_args, rget, sget, lget, set_mode
!!    implicit none
!!       real :: x,y                           ; namelist/args/ x,y
!!       character(len=:),allocatable :: title ; namelist/args/ title
!!       logical :: big                        ; namelist/args/ big
!!       call set_mode('response_file')
!!       call set_args('-x 10.0 -y 20.0 --title "my title" --big F')
!!       x=rget('x')
!!       y=rget('y')
!!       title=sget('title')
!!       big=lget('big')
!!       write(*,nml=args)
!!    end program testit
!!
!!  And a file in the current directory called "a.rsp" contains
!!
!!     # defaults for project A
!!     options -x 1000 -y 9999
!!     options --title " "
!!     options --big T
!!
!!  The program could be called with
!!
!!     $myprog     # normal call
!!      X=10.0 Y=20.0 TITLE="my title"
!!
!!     $myprog @a  # change defaults as specified in "a.rsp"
!!     X=1000.0 Y=9999.0 TITLE=" "
!!
!!     # change defaults but use any option as normal to override defaults
!!     $myprog @a -y 1234
!!      X=1000.0 Y=1234.0 TITLE=" "
!!
!!   COMPOUND RESPONSE FILES
!!
!!  A compound response file has the same basename as the executable with a
!!  ".rsp" suffix added. So if your program is named "myprg" the filename
!!  must be "myprg.rsp".
!!
!!    Note that here `basename` means the last leaf of the
!!    name of the program as returned by the Fortran intrinsic
!!    GET_COMMAND_ARGUMENT(0,...) trimmed of anything after a period ("."),
!!    so it is a good idea not to use hidden files.
!!
!!  Unlike simple response files compound response files can contain multiple
!!  setting names.
!!
!!  Specifically in a compound file
!!  if the environment variable $OSTYPE (first) or $OS is set the first search
!!  will be for a line of the form (no leading spaces should be used):
!!
!!    @OSTYPE@alias_name
!!
!!  If no match or if the environment variables $OSTYPE and $OS were not
!!  set or a match is not found then a line of the form
!!
!!    @alias_name
!!
!!  is searched for in simple or compound files. If found subsequent lines
!!  will be ignored that start with "@" until a line not starting with
!!  "@" is encountered. Lines will then be processed until another line
!!  starting with "@" is found or end-of-file is encountered.
!!
!!   COMPOUND RESPONSE FILE EXAMPLE
!!  An example compound file
!!
!!    #################
!!    @if
!!    > RUNNING TESTS USING RELEASE VERSION AND ifort
!!    options test --release --compiler ifort
!!    #################
!!    @gf
!!    > RUNNING TESTS USING RELEASE VERSION AND gfortran
!!    options test --release --compiler gfortran
!!    #################
!!    @nv
!!    > RUNNING TESTS USING RELEASE VERSION AND nvfortran
!!    options test --release --compiler nvfortran
!!    #################
!!    @nag
!!    > RUNNING TESTS USING RELEASE VERSION AND nagfor
!!    options test --release --compiler nagfor
!!    #
!!    #################
!!    # OS-specific example:
!!    @Linux@install
!!    #
!!    # install executables in directory (assuming install(1) exists)
!!    #
!!    system mkdir -p ~/.local/bin
!!    options run --release T --runner "install -vbp -m 0711 -t ~/.local/bin"
!!    @install
!!    STOP INSTALL NOT SUPPORTED ON THIS PLATFORM OR $OSTYPE NOT SET
!!    #
!!    #################
!!    @fpm@testall
!!    #
!!    !fpm test --compiler nvfortran
!!    !fpm test --compiler ifort
!!    !fpm test --compiler gfortran
!!    !fpm test --compiler nagfor
!!    STOP tests complete. Any additional parameters were ignored
!!    #################
!!
!!  Would be used like
!!
!!    fpm @install
!!    fpm @nag --
!!    fpm @testall
!!
!!   NOTES
!!
!!    The intel Fortran compiler now calls the response files "indirect
!!    files" and does not add the implied suffix ".rsp" to the files
!!    anymore. It also allows the @NAME syntax anywhere on the command line,
!!    not just at the beginning. -- 20201212
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_args(prototype,help_text,version_text,string,prefix,ierr,errmsg)

! ident_1="@(#) M_CLI2 set_args(3f) parse prototype string"

character(len=*),intent(in)                       :: prototype
character(len=*),intent(in),optional              :: help_text(:)
character(len=*),intent(in),optional              :: version_text(:)
character(len=*),intent(in),optional              :: string
character(len=*),intent(in),optional              :: prefix
integer,intent(out),optional                      :: ierr
character(len=:),intent(out),allocatable,optional :: errmsg
character(len=:),allocatable                      :: hold               ! stores command line argument
integer                                           :: ibig
character(len=:),allocatable                      :: debug_mode

   debug_mode= upper(get_env('CLI_DEBUG_MODE','FALSE'))//' '
   select case(debug_mode(1:1))
   case('Y','T')
      G_DEBUG=.true.
   end select

   G_response=CLI_RESPONSE_FILE
   G_options_only=.false.
   G_passed_in=''
   G_STOP=0
   G_STOP_MESSAGE=''
   if(present(prefix))then
      G_PREFIX=prefix
   else
      G_PREFIX=''
   endif
   if(present(ierr))then
      G_QUIET=.true.
   else
      G_QUIET=.false.
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   IF(ALLOCATED(UNNAMED)) DEALLOCATE(UNNAMED)
   ALLOCATE(CHARACTER(LEN=IBIG) :: UNNAMED(0))
   if(allocated(args)) deallocate(args)
   allocate(character(len=ibig) :: args(0))

   call wipe_dictionary()
   hold='--version F --usage F --help F --version F '//adjustl(prototype)
   call prototype_and_cmd_args_to_nlist(hold,string)
   if(allocated(G_RESPONSE_IGNORED))then
      if(G_DEBUG)write(*,gen)'<DEBUG>SET_ARGS:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
      if(size(unnamed) /= 0)write(*,*)'LOGIC ERROR'
      call split(G_RESPONSE_IGNORED,unnamed)
   endif

   if(.not.allocated(unnamed))then
       allocate(character(len=0) :: unnamed(0))
   endif
   if(.not.allocated(args))then
       allocate(character(len=0) :: args(0))
   endif
   call check_commandline(help_text,version_text) ! process --help, --version, --usage
   if(present(ierr))then
      ierr=G_STOP
   endif
   if(present(errmsg))then
      errmsg=G_STOP_MESSAGE
   endif
end subroutine set_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_subcommand(3f) - [ARGUMENTS:M_CLI2] special-case routine for
!!    handling subcommands on a command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function get_subcommand()
!!
!!     character(len=:),allocatable :: get_subcommand
!!
!!##DESCRIPTION
!!    In the special case when creating a program with subcommands it
!!    is assumed the first word on the command line is the subcommand. A
!!    routine is required to handle response file processing, therefore
!!    this routine (optionally processing response files) returns that
!!    first word as the subcommand name.
!!
!!    It should not be used by programs not building a more elaborate
!!    command with subcommands.
!!
!!##RETURNS
!!    NAME   name of subcommand
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_get_subcommand
!!    !x! SUBCOMMANDS
!!    !x! For a command with subcommands like git(1)
!!    !x! you can make separate namelists for each subcommand.
!!    !x! You can call this program which has two subcommands (run, test),
!!    !x! like this:
!!    !x!    demo_get_subcommand --help
!!    !x!    demo_get_subcommand run -x -y -z --title -l -L
!!    !x!    demo_get_subcommand test --title -l -L --testname
!!    !x!    demo_get_subcommand run --help
!!       implicit none
!!    !x! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES
!!       real               :: x=-999.0,y=-999.0,z=-999.0
!!       character(len=80)  :: title="not set"
!!       logical            :: l=.false.
!!       logical            :: l_=.false.
!!       character(len=80)  :: testname="not set"
!!       character(len=20)  :: name
!!       call parse(name) !x! DEFINE AND PARSE COMMAND LINE
!!       !x! ALL DONE CRACKING THE COMMAND LINE.
!!       !x! USE THE VALUES IN YOUR PROGRAM.
!!       write(*,*)'command was ',name
!!       write(*,*)'x,y,z .... ',x,y,z
!!       write(*,*)'title .... ',title
!!       write(*,*)'l,l_ ..... ',l,l_
!!       write(*,*)'testname . ',testname
!!    contains
!!    subroutine parse(name)
!!    !x! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY
!!    use M_CLI2, only : set_args, get_args, get_args_fixed_length
!!    use M_CLI2, only : get_subcommand, set_mode
!!    character(len=*)              :: name    ! the subcommand name
!!    character(len=:),allocatable  :: help_text(:), version_text(:)
!!       call set_mode('response_file')
!!    ! define version text
!!       version_text=[character(len=80) :: &
!!          '@(#)PROGRAM:     demo_get_subcommand            >', &
!!          '@(#)DESCRIPTION: My demo program  >', &
!!          '@(#)VERSION:     1.0 20200715     >', &
!!          '@(#)AUTHOR:      me, myself, and I>', &
!!          '@(#)LICENSE:     Public Domain    >', &
!!          '' ]
!!        ! general help for "demo_get_subcommand --help"
!!        help_text=[character(len=80) :: &
!!         ' allowed subcommands are          ', &
!!         '   * run  -l -L --title -x -y -z  ', &
!!         '   * test -l -L --title           ', &
!!         '' ]
!!       ! find the subcommand name by looking for first word on command
!!       ! not starting with dash
!!       name = get_subcommand()
!!       select case(name)
!!       case('run')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "run"        ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args( &
!!        & '-x 1 -y 2 -z 3 --title "my title" -l F -L F',&
!!        & help_text,version_text)
!!        call get_args('x',x)
!!        call get_args('y',y)
!!        call get_args('z',z)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!       case('test')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "test"       ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args(&
!!        & '--title "my title" -l F -L F --testname "Test"',&
!!        & help_text,version_text)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!        call get_args_fixed_length('testname',testname)
!!       case default
!!        ! process help and version
!!        call set_args(' ',help_text,version_text)
!!        write(*,'(*(a))')'unknown or missing subcommand [',trim(name),']'
!!        write(*,'(a)')[character(len=80) ::  &
!!        ' allowed subcommands are          ', &
!!        '   * run  -l -L -title -x -y -z   ', &
!!        '   * test -l -L -title            ', &
!!        '' ]
!!        stop
!!       end select
!!    end subroutine parse
!!    end program demo_get_subcommand
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
function get_subcommand() result(sub)

! ident_2="@(#) M_CLI2 get_subcommand(3f) parse prototype string to get subcommand allowing for response files"

character(len=:),allocatable  :: sub
character(len=:),allocatable  :: cmdarg
character(len=:),allocatable  :: array(:)
character(len=:),allocatable  :: prototype
integer                       :: ilongest
integer                       :: i
integer                       :: j
   G_subcommand=''
   G_options_only=.true.
   sub=''

   if(.not.allocated(unnamed))then
      allocate(character(len=0) :: unnamed(0))
   endif

   ilongest=longest_command_argument()
   allocate(character(len=max(63,ilongest)):: cmdarg)
   cmdarg(:) = ''
   ! look for @NAME if CLI_RESPONSE_FILE=.TRUE. AND LOAD THEM
   do i = 1, command_argument_count()
      call get_command_argument(i, cmdarg)
      if(scan(adjustl(cmdarg(1:1)),'@')  ==  1)then
         call get_prototype(cmdarg,prototype)
         call split(prototype,array)
         ! assume that if using subcommands first word not starting with dash is the subcommand
         do j=1,size(array)
            if(adjustl(array(j)(1:1))  /=  '-')then
            G_subcommand=trim(array(j))
            sub=G_subcommand
            exit
         endif
         enddo
      endif
   enddo

   if(G_subcommand /= '')then
      sub=G_subcommand
   elseif(size(unnamed) /= 0)then
      sub=unnamed(1)
   else
      cmdarg(:) = ''
      do i = 1, command_argument_count()
         call get_command_argument(i, cmdarg)
         if(adjustl(cmdarg(1:1))  /=  '-')then
            sub=trim(cmdarg)
           exit
        endif
      enddo
   endif
   G_options_only=.false.
end function get_subcommand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_usage(3f) - [ARGUMENTS:M_CLI2] allow setting a short description
!!    for keywords for the --usage switch
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_usage(keyword,description)
!!
!!      character(len=*),intent(in)     ::  keyword
!!      character(len=*),intent(in)     ::  description
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     KEYWORD      the name of a command keyword
!!     DESCRIPTION  a brief one-line description of the keyword
!!
!!
!!##EXAMPLE
!!
!! sample program:
!!
!!     Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine set_usage(keyword,description,value)
character(len=*),intent(in) :: keyword
character(len=*),intent(in) :: description
character(len=*),intent(in) :: value
write(*,*)keyword
write(*,*)description
write(*,*)value
! store the descriptions in an array and then apply them when set_args(3f) is called.
! alternatively, could allow for a value as well in lieu of the prototype
end subroutine set_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_to_dictionary(3f) - [ARGUMENTS:M_CLI2] parse user command
!!    and store tokens into dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     recursive subroutine prototype_to_dictionary(string)
!!
!!      character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!      given a string of form
!!
!!        -var value -var value
!!
!!      define dictionary of form
!!
!!        keyword(i), value(i)
!!
!!      o  string values
!!
!!          o must be delimited with double quotes.
!!          o adjacent double quotes put one double quote into value
!!          o must not be null. A blank is specified as " ", not "".
!!
!!      o  logical values
!!
!!          o logical values must have a value. Use F.
!!
!!      o  leading and trailing blanks are removed from unquoted values
!!
!!
!!##OPTIONS
!!      STRING   string is character input string to define command
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!! sample program:
!!
!!     call prototype_to_dictionary(' -l F --ignorecase F --title "my title string" -x 10.20')
!!     call prototype_to_dictionary(' --ints 1,2,3,4')
!!
!! Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
recursive subroutine prototype_to_dictionary(string)

! ident_3="@(#) M_CLI2 prototype_to_dictionary(3f) parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy   ! working copy of string
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
character(len=3)                  :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! character to left of CURRNT
character(len=1)                  :: forwrd  ! character to right of CURRNT
integer,dimension(2)              :: ipnt
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: itype
integer,parameter                 :: VAL=1, KEYW=2
integer                           :: ifwd
integer                           :: ibegin
integer                           :: iend
integer                           :: place

   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=adjustl(string)//'  '

   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in keyword
   ipnt(1)=1           ! pointer to position in value
   itype=VAL           ! itype=1 for value, itype=2 for variable

   delmt="off"
   prev=" "

   G_keyword_single_letter=.true.
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)      ! ensure not past end of string
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)

      if((currnt=="-" .and. prev==" " .and. delmt == "off" .and. index("0123456789.",forwrd) == 0).or.ipoint > islen)then
         ! beginning of a keyword
         if(forwrd == '-')then                      ! change --var to -var so "long" syntax is supported
            !x!dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead (was changing it to _)
            G_keyword_single_letter=.false.         ! flag this is a long keyword
         else
            G_keyword_single_letter=.true.          ! flag this is a short (single letter) keyword
         endif
         if(ipnt(1)-1 >= 1)then                     ! position in value
            ibegin=1
            iend=len_trim(value(:ipnt(1)-1))
            TESTIT: do
               if(iend  ==  0)then                  ! len_trim returned 0, value is blank
                  iend=ibegin
                  exit TESTIT
               elseif(value(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit TESTIT
               endif
            enddo TESTIT
            if(keyword /= ' ')then
               if(value=='[]')value=','
               call update(keyword,value)            ! store name and its value
            elseif( G_remaining_option_allowed)then  ! meaning "--" has been encountered
               if(value=='[]')value=','
               call update('_args_',trim(value))
            else
               !x!write(warn,'(*(g0))')'*prototype_to_dictionary* warning: ignoring string [',trim(value),'] for ',trim(keyword)
               G_RESPONSE_IGNORED=TRIM(VALUE)
               if(G_DEBUG)write(*,gen)'<DEBUG>PROTOTYPE_TO_DICTIONARY:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
            endif
         else
            call locate_key(keyword,place)
            if(keyword /= ' '.and.place < 0)then
               call update(keyword,'F')           ! store name and null value (first pass)
            elseif(keyword /= ' ')then
               call update(keyword,' ')           ! store name and null value (second pass)
            elseif(.not.G_keyword_single_letter.and.ipoint-2 == islen) then ! -- at end of line
               G_remaining_option_allowed=.true.  ! meaning for "--" is that everything on commandline goes into G_remaining
            endif
         endif
         itype=KEYW                            ! change to expecting a keyword
         value=""                              ! clear value for this variable
         keyword=""                            ! clear variable name
         ipnt(1)=1                             ! restart variable value
         ipnt(2)=1                             ! restart variable name

      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " " .and. itype  ==  KEYW)then
            ! switch from building a keyword string to building a value string
            itype=VAL
            ! beginning of a delimited value
         elseif(currnt  ==  """".and.itype  ==  VAL)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype == VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
               delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
               delmt="off"
            else
               delmt="on"
            endif
            if(prev /= """")then  ! leave quotes where found them
               if(itype == VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current keyword or value
            if(itype == VAL)then
               value=value//currnt
            else
               keyword=keyword//currnt
            endif
            ipnt(itype)=ipnt(itype)+1
         endif

      endif

      prev=currnt
      if(ipoint <= islen)then
         cycle
      else
         exit
      endif
   enddo

end subroutine prototype_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    specified(3f) - [ARGUMENTS:M_CLI2] return true if keyword was present
!!    on command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure function specified(name)
!!
!!     character(len=*),intent(in) :: name
!!     logical :: specified
!!
!!##DESCRIPTION
!!
!!    specified(3f) returns .true. if the specified keyword was present on
!!    the command line.
!!
!!    M_CLI2 intentionally does not have validators except for SPECIFIED(3f)
!!    and of course a check whether the input conforms to the type when
!!    requesting a value (with get_args(3f) or the convenience functions
!!    like inum(3f)).
!!
!!    Fortran already has powerful validation capabilities. Logical
!!    expressions ANY(3f) and ALL(3f) are standard Fortran features which
!!    easily allow performing the common validations for command line
!!    arguments without having to learn any additional syntax or methods.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to query the presence of. Long
!!           names should always be used.
!!
!!##RETURNS
!!    SPECIFIED  returns .TRUE. if specified NAME was present on the command
!!               line when the program was invoked.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_specified
!!    use, intrinsic :: iso_fortran_env, only : &
!!    & stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
!!    use M_CLI2,  only : set_args, igets, rgets, specified, sget, lget
!!    implicit none
!!
!!    ! Define args
!!    integer,allocatable  :: ints(:)
!!    real,allocatable     :: floats(:)
!!    logical              :: flag
!!    character(len=:),allocatable :: color
!!    character(len=:),allocatable :: list(:)
!!    integer :: i
!!
!!     call set_args('&
!!        & --color:c "red"       &
!!        & --flag:f F            &
!!        & --ints:i 1,10,11      &
!!        & --floats:T 12.3, 4.56 &
!!        & ')
!!     ints=igets('ints')
!!     floats=rgets('floats')
!!     flag=lget('flag')
!!     color=sget('color')
!!
!!     write(*,*)'color=',color
!!     write(*,*)'flag=',flag
!!     write(*,*)'ints=',ints
!!     write(*,*)'floats=',floats
!!
!!     write(*,*)'was -flag specified?',specified('flag')
!!
!!     ! elemental
!!     write(*,*)specified(['floats','ints  '])
!!
!!     ! If you want to know if groups of parameters were specified use
!!     ! ANY(3f) and ALL(3f)
!!     write(*,*)'ANY:',any(specified(['floats','ints  ']))
!!     write(*,*)'ALL:',all(specified(['floats','ints  ']))
!!
!!     ! For mutually exclusive
!!     if (all(specified(['floats','ints  '])))then
!!         write(*,*)'You specified both names --ints and --floats'
!!     endif
!!
!!     ! For required parameter
!!     if (.not.any(specified(['floats','ints  '])))then
!!         write(*,*)'You must specify --ints or --floats'
!!     endif
!!
!!    ! check if all values are in range from 10 to 30 and even
!!    write(*,*)'are all numbers good?',all([ints >= 10,ints <= 30,(ints/2)*2 == ints])
!!
!!    ! perhaps you want to check one value at a time
!!    do i=1,size(ints)
!!       write(*,*)ints(i),[ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]
!!       if(all([ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]) )then
!!          write(*,*)ints(i),'is an even number from 10 to 30 inclusive'
!!       else
!!          write(*,*)ints(i),'is not an even number from 10 to 30 inclusive'
!!       endif
!!    enddo
!!
!!    list = [character(len=10) :: 'red','white','blue']
!!    if( any(color == list) )then
!!       write(*,*)color,'matches a value in the list'
!!    else
!!       write(*,*)color,'not in the list'
!!    endif
!!
!!    if(size(ints).eq.3)then
!!       write(*,*)'ints(:) has expected number of values'
!!    else
!!       write(*,*)'ints(:) does not have expected number of values'
!!    endif
!!
!!    end program demo_specified
!!
!! Default output
!!
!!  > color=red
!!  > flag= F
!!  > ints=           1          10          11
!!  > floats=   12.3000002       4.55999994
!!  > was -flag specified? F
!!  > F F
!!  > ANY: F
!!  > ALL: F
!!  > You must specify --ints or --floats
!!  >           1 F T F
!!  >           1  is not an even number from 10 to 30 inclusive
!!  >          10 T T T
!!  >          10  is an even number from 10 to 30 inclusive
!!  >          11 T T F
!!  >          11  is not an even number from 10 to 30 inclusive
!!  > red matches a value in the list
!!  > ints(:) has expected number of values
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
elemental impure function specified(key)
character(len=*),intent(in) :: key
logical                     :: specified
integer                     :: place
   call locate_key(key,place)                   ! find where string is or should be
   if(place < 1)then
      specified=.false.
   else
      specified=present_in(place)
   endif
end function specified
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    update(3f) - [ARGUMENTS:M_CLI2] update internal dictionary given
!!    keyword and value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine update(key,val)
!!
!!      character(len=*),intent(in)           :: key
!!      character(len=*),intent(in),optional  :: val
!!##DESCRIPTION
!!      Update internal dictionary in M_CLI2(3fm) module.
!!##OPTIONS
!!      key  name of keyword to add, replace, or delete from dictionary
!!      val  if present add or replace value associated with keyword. If not
!!           present remove keyword entry from dictionary.
!!
!!           If "present" is true, a value will be appended
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place, ii
integer                               :: iilen
character(len=:),allocatable          :: val_local
character(len=:),allocatable          :: short
character(len=:),allocatable          :: long
character(len=:),allocatable          :: long_short(:)
integer                               :: isize
logical                               :: set_mandatory
   set_mandatory=.false.
   call split(trim(key),long_short,':',nulls='return') ! split long:short keyword or long:short:: or long:: or short::
   ! check for :: on end
   isize=size(long_short)

   if(isize > 0)then                     ! very special-purpose syntax where if ends in :: next field is a value even
      if(long_short(isize) == '')then    ! if it starts with a dash, for --flags option on fpm(1).
         set_mandatory=.true.
         long_short=long_short(:isize-1)
      endif
   endif

   select case(size(long_short))
   case(0)
      long=''
      short=''
   case(1)
      long=trim(long_short(1))
      if(len_trim(long) == 1)then
         !x!ii= findloc (shorts, long, dim=1) ! if parsing arguments on line and a short keyword look up long value
         ii=maxloc([0,merge(1, 0, shorts == long)],dim=1)
         if(ii > 1)then
            long=keywords(ii-1)
         endif
         short=long
      else
         short=''
      endif
   case(2)
      long=trim(long_short(1))
      short=trim(long_short(2))
   case default
      write(warn,*)'WARNING: incorrect syntax for key: ',trim(key)
      long=trim(long_short(1))
      short=trim(long_short(2))
   end select
   if(G_UNDERDASH) long=replace_str(long,'-','_')
   if(G_NOSEPARATOR)then
      long=replace_str(long,'-','')
      long=replace_str(long,'_','')
   endif
   if(G_IGNORECASE.and.len_trim(long) > 1)long=lower(long)
   if(present(val))then
      val_local=val
      iilen=len_trim(val_local)
      call locate_key(long,place)                  ! find where string is or should be
      if(place < 1)then                                ! if string was not found insert it
         call insert_(keywords,long,iabs(place))
         call insert_(values,val_local,iabs(place))
         call insert_(counts,iilen,iabs(place))
         call insert_(shorts,short,iabs(place))
         call insert_(present_in,.true.,iabs(place))
         call insert_(mandatory,set_mandatory,iabs(place))
      else
         if(present_in(place))then                      ! if multiple keywords append values with space between them
            if(G_append)then
               if(values(place)(1:1) == '"')then
               ! UNDESIRABLE: will ignore previous blank entries
                  val_local='"'//trim(unquote(values(place)))//' '//trim(unquote(val_local))//'"'
               else
                  val_local=values(place)//' '//val_local
               endif
            endif
            iilen=len_trim(val_local)
         endif
         call replace_(values,val_local,place)
         call replace_(counts,iilen,place)
         call replace_(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate_key(long,place)                       ! check name as long and short
      if(place > 0)then
         call remove_(keywords,place)
         call remove_(values,place)
         call remove_(counts,place)
         call remove_(shorts,place)
         call remove_(present_in,place)
         call remove_(mandatory,place)
      endif
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wipe_dictionary(3fp) - [ARGUMENTS:M_CLI2] reset private M_CLI2(3fm)
!!    dictionary to empty
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!      subroutine wipe_dictionary()
!!##DESCRIPTION
!!      reset private M_CLI2(3fm) dictionary to empty
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_wipe_dictionary
!!      use M_CLI2, only : dictionary
!!         call wipe_dictionary()
!!      end program demo_wipe_dictionary
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
   if(allocated(shorts))deallocate(shorts)
   allocate(character(len=0) :: shorts(0))
   if(allocated(present_in))deallocate(present_in)
   allocate(present_in(0))
   if(allocated(mandatory))deallocate(mandatory)
   allocate(mandatory(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get(3f) - [ARGUMENTS:M_CLI2] get dictionary value associated with
!!    key name in private M_CLI2(3fm) dictionary
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!    Get dictionary value associated with key name in private M_CLI2(3fm)
!!    dictionary.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate_key(key,place)
   if(place < 1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_and_cmd_args_to_nlist(3f) - [ARGUMENTS:M_CLI2] convert
!!    Unix-like command arguments to table
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine prototype_and_cmd_args_to_nlist(prototype)
!!
!!      character(len=*) :: prototype
!!##DESCRIPTION
!!    create dictionary with character keywords, values, and value lengths
!!    using the routines for maintaining a list from command line arguments.
!!##OPTIONS
!!      prototype
!!##EXAMPLE
!!
!! Sample program
!!
!!      program demo_prototype_and_cmd_args_to_nlist
!!      use M_CLI2,  only : prototype_and_cmd_args_to_nlist, unnamed
!!      implicit none
!!      character(len=:),allocatable :: readme
!!      character(len=256)           :: message
!!      integer                      :: ios
!!      integer                      :: i
!!      doubleprecision              :: something
!!
!!      ! define arguments
!!      logical            :: l,h,v
!!      real               :: p(2)
!!      complex            :: c
!!      doubleprecision    :: x,y,z
!!
!!      ! uppercase keywords get an underscore to make it easier to remember
!!      logical            :: l_,h_,v_
!!      ! character variables must be long enough to hold returned value
!!      character(len=256) :: a_,b_
!!      integer            :: c_(3)
!!
!!         ! give command template with default values
!!         ! all values except logicals get a value.
!!         ! strings must be delimited with double quotes
!!         ! A string has to have at least one character as for -A
!!         ! lists of numbers should be comma-delimited.
!!         ! No spaces are allowed in lists of numbers
!!         call prototype_and_cmd_args_to_nlist('&
!!         & -l -v -h -LVH -x 0 -y 0.0 -z 0.0d0 -p 0,0 &
!!         & -A " " -B "Value B" -C 10,20,30 -c (-123,-456)',readme)
!!
!!         call get_args('x',x,'y',y,'z',z)
!!            something=sqrt(x**2+y**2+z**2)
!!            write (*,*)something,x,y,z
!!            if(size(unnamed) > 0)then
!!               write (*,'(a)')'files:'
!!               write (*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!            endif
!!      end program demo_prototype_and_cmd_args_to_nlist
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine prototype_and_cmd_args_to_nlist(prototype,string)

! ident_4="@(#) M_CLI2 prototype_and_cmd_args_to_nlist create dictionary from prototype if not null and update from command line"

character(len=*),intent(in)           :: prototype
character(len=*),intent(in),optional  :: string
integer                               :: ibig
integer                               :: itrim
integer                               :: iused

   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:START'
   G_passed_in=prototype                            ! make global copy for printing
   ibig=longest_command_argument()                  ! bug in gfortran. len=0 should be fine
   ibig=max(ibig,1)
   IF(ALLOCATED(UNNAMED))DEALLOCATE(UNNAMED)
   ALLOCATE(CHARACTER(LEN=IBIG) :: UNNAMED(0))
   if(allocated(args))deallocate(args)
   allocate(character(len=ibig) :: args(0))

   G_remaining_option_allowed=.false.
   G_remaining_on=.false.
   G_remaining=''
   if(prototype /= '')then
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype

      ! if short keywords not used by user allow them for standard options

      call locate_key('h',iused)
      if(iused <= 0)then
         call update('help')
         call update('help:h','F')
      endif

      call locate_key('v',iused)
      if(iused <= 0)then
         call update('version')
         call update('version:v','F')
      endif

      call locate_key('V',iused)
      if(iused <= 0)then
         call update('verbose')
         call update('verbose:V','F')
      endif

      call locate_key('u',iused)
      if(iused <= 0)then
         call update('usage')
         call update('usage:u','F')
      endif

      present_in=.false.                            ! reset all values to false so everything gets written
   endif

   if(present(string))then                          ! instead of command line arguments use another prototype string
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL PROTOTYPE_TO_DICTIONARY:STRING=',STRING
      call prototype_to_dictionary(string)          ! build dictionary from prototype
   else
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL CMD_ARGS_TO_DICTIONARY:CHECK=',.true.
      call cmd_args_to_dictionary()
   endif

   if( len(G_remaining) > 1)then                    ! if -- was in prototype then after -- on input return rest in this string
      itrim=len(G_remaining)
      if(G_remaining(itrim:itrim) == ' ')then       ! was adding a space at end as building it, but do not want to remove blanks
         G_remaining=G_remaining(:itrim-1)
      endif
      remaining=G_remaining
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:NORMAL END'
end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_response(name)
character(len=*),intent(in)  :: name
character(len=:),allocatable :: prototype
logical                      :: hold

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:START:NAME=',name

   call get_prototype(name,prototype)

   if(prototype /= '')then
      hold=G_append
      G_append=.false.
      if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:CALL PROTOTYPE_TO_DICTIONARY:PROTOTYPE=',prototype
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype
      G_append=hold
   endif

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'

end subroutine expand_response
!===================================================================================================================================
subroutine get_prototype(name,prototype) ! process @name abbreviations
character(len=*),intent(in) :: name
character(len=:),allocatable,intent(out) :: prototype
character(len=:),allocatable             :: filename
character(len=:),allocatable             :: os
character(len=:),allocatable             :: plain_name
character(len=:),allocatable             :: search_for
integer                                  :: lun
integer                                  :: ios
integer                                  :: itrim
character(len=4096)                      :: line !x! assuming input never this long
character(len=256)                       :: message
character(len=:),allocatable             :: array(:) ! output array of tokens
integer                                  :: lines_processed

   lines_processed=0
   plain_name=name//'  '
   plain_name=trim(name(2:))
   os= '@' // get_env('OSTYPE',get_env('OS'))
   if(G_DEBUG)write(*,gen)'<DEBUG>GET_PROTOTYPE:OS=',OS

   search_for=''
   ! look for NAME.rsp and see if there is an @OS  section in it and position to it and read
   if(os /= '@')then
      search_for=os
      call find_and_read_response_file(plain_name)
      if(lines_processed /= 0)return
   endif

   ! look for NAME.rsp and see if there is anything before an OS-specific section
   search_for=''
   call find_and_read_response_file(plain_name)
   if(lines_processed /= 0)return

   ! look for ARG0.rsp  with @OS@NAME  section in it and position to it
   if(os /= '@')then
      search_for=os//name
      call find_and_read_response_file(basename(get_name(),suffix=.false.))
      if(lines_processed /= 0)return
   endif

   ! look for ARG0.rsp  with a section called @NAME in it and position to it
   search_for=name
   call find_and_read_response_file(basename(get_name(),suffix=.false.))
   if(lines_processed /= 0)return

   write(*,gen)'<ERROR> response name ['//trim(name)//'] not found'
   stop 1
contains
!===================================================================================================================================
subroutine find_and_read_response_file(rname)
! search for a simple file named the same as the @NAME field with one entry assumed in it
character(len=*),intent(in)  :: rname
character(len=:),allocatable :: paths(:)
character(len=:),allocatable :: testpath
character(len=256)           :: message
integer                      :: i
integer                      :: ios
   prototype=''
   ! look for NAME.rsp
   ! assume if have / or \ a full filename was supplied to support ifort(1)
   if((index(rname,'/') /= 0.or.index(rname,'\') /= 0) .and. len(rname) > 1 )then
      filename=rname
      lun=fileopen(filename,message)
      if(lun /= -1)then
         call process_response()
         close(unit=lun,iostat=ios)
      endif
      return
   else
      filename=rname//'.rsp'
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:FILENAME=',filename

   ! look for name.rsp in directories from environment variable assumed to be a colon-separated list of directories
   call split(get_env('CLI_RESPONSE_PATH','~/.local/share/rsp'),paths)
   paths=[character(len=len(paths)) :: ' ',paths]
   if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:PATHS=',paths

   do i=1,size(paths)
      testpath=join_path(paths(i),filename)
      lun=fileopen(testpath,message)
      if(lun /= -1)then
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:SEARCH_FOR=',search_for
         if(search_for /= '') call position_response() ! set to end of file or where string was found
         call process_response()
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:LINES_PROCESSED=',LINES_PROCESSED
         close(unit=lun,iostat=ios)
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:CLOSE:LUN=',LUN,' IOSTAT=',IOS
         if(lines_processed /= 0)exit
      endif
   enddo

end subroutine find_and_read_response_file
!===================================================================================================================================
subroutine position_response()
integer :: ios
   line=''
   INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         if(G_DEBUG)write(*,gen)'<DEBUG>POSITION_RESPONSE:EOF'
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         write(*,gen)'<ERROR>*position_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(line == search_for)return
   enddo INFINITE
end subroutine position_response
!===================================================================================================================================
subroutine process_response()
character(len=:),allocatable :: padded
character(len=:),allocatable :: temp
   line=''
   lines_processed=0
      INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         write(*,gen)'<ERROR>*process_response*:'//trim(message)
         exit INFINITE
      endif
      line=trim(adjustl(line))
      temp=line
      if(index(temp//' ','#') == 1)cycle
      if(temp /= '')then

         if(index(temp,'@') == 1.and.lines_processed /= 0)exit INFINITE

         call split(temp,array) ! get first word
         itrim=len_trim(array(1))+2
         temp=temp(itrim:)

         PROCESS: select case(lower(array(1)))
         case('comment','#','')
         case('system','!','$')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            call execute_command_line(temp)
         case('options','option','-')
            lines_processed= lines_processed+1
            prototype=prototype//' '//trim(temp)
         case('print','>','echo')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            write(*,'(a)')trim(temp)
         case('stop')
            if(G_options_only)exit PROCESS
            write(*,'(a)')trim(temp)
            stop
         case default
            if(array(1)(1:1) == '-')then
               ! assume these are simply options to support ifort(1)
               ! if starts with a single dash must assume a single argument
               ! and rest is value to support -Dname and -Ifile option
               ! which currently is not supported, so multiple short keywords
               ! does not work. Just a ifort(1) test at this point, so do not document
               if(G_options_only)exit PROCESS
               padded=trim(line)//'  '
               if(padded(2:2) == '-')then
                  prototype=prototype//' '//trim(line)
               else
                  prototype=prototype//' '//padded(1:2)//' '//trim(padded(3:))
               endif
               lines_processed= lines_processed+1
            else
               if(array(1)(1:1) == '@')cycle INFINITE !skip adjacent @ lines from first
               lines_processed= lines_processed+1
               write(*,'(*(g0))')'unknown response keyword [',array(1),'] with options of [',trim(temp),']'
            endif
         end select PROCESS

      endif
      enddo INFINITE
end subroutine process_response

end subroutine get_prototype
!===================================================================================================================================
function fileopen(filename,message) result(lun)
character(len=*),intent(in)              :: filename
character(len=*),intent(out),optional    :: message
integer                                  :: lun
integer                                  :: ios
character(len=256)                       :: message_local

   ios=0
   message_local=''
   open(file=filename,newunit=lun,&
    & form='formatted',access='sequential',action='read',&
    & position='rewind',status='old',iostat=ios,iomsg=message_local)

   if(ios /= 0)then
      lun=-1
      if(present(message))then
         message=trim(message_local)
      else
         write(*,gen)trim(message_local)
      endif
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>FILEOPEN:FILENAME=',filename,' LUN=',lun,' IOS=',IOS,' MESSAGE=',trim(message_local)

end function fileopen
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   if(NAME /= '')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
          !x!print *, NAME, " is not defined in the environment. Strange..."
          VALUE=''
      case (2)
          !x!print *, "This processor doesn't support environment variables. Boooh!"
          VALUE=''
      case default
          ! make string to hold value of sufficient size
          if(allocated(value))deallocate(value)
          allocate(character(len=max(howbig,1)) :: VALUE)
          ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
          if(stat /= 0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE == ''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env
!===================================================================================================================================
function join_path(a1,a2,a3,a4,a5) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1 /= '')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   path=adjustl(path//'  ')
   path=path(1:1)//replace_str(path,filesep//filesep,'') ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function join_path
!===================================================================================================================================
function get_name() result(name)
! get the pathname of arg0
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=istat)
   if(istat == 0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=istat)
      if(istat == 0)then
         inquire(file=arg0,iostat=istat,name=long_name)
         name=trim(long_name)
      else
         name=arg0
      endif
   endif
end function get_name
!===================================================================================================================================
function basename(path,suffix) result (base)
    ! Extract filename from path with/without suffix
    !
character(*), intent(In) :: path
logical, intent(in), optional :: suffix
character(:), allocatable :: base

character(:), allocatable :: file_parts(:)
logical :: with_suffix

   if (.not.present(suffix)) then
      with_suffix = .true.
   else
      with_suffix = suffix
   endif

   if (with_suffix) then
      call split(path,file_parts,delimiters='\/')
      if(size(file_parts) > 0)then
         base = trim(file_parts(size(file_parts)))
      else
         base = ''
      endif
   else
      call split(path,file_parts,delimiters='\/.')
      if(size(file_parts) >= 2)then
         base = trim(file_parts(size(file_parts)-1))
      elseif(size(file_parts) == 1)then
         base = trim(file_parts(1))
      else
         base = ''
      endif
   endif
end function basename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function separator() result(sep)
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!   First testing for the existence of "/.",  then if that fails a list
!!   of variable names assumed to contain directory paths {PATH|HOME} are
!!   examined first for a backslash, then a slash. Assuming basically the
!!   choice is a ULS or MSWindows system, and users can do weird things like
!!   put a backslash in a ULS path and break it.
!!
!!   Therefore can be very system dependent. If the queries fail the
!!   default returned is "/".
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator

! use the pathname returned as arg0 to determine pathname separator
integer                      :: ios
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!x!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !x!FORT BUG:if(sep_cache /= ' ')then  ! use cached value.
    !x!FORT BUG:    sep=sep_cache
    !x!FORT BUG:    return
    !x!FORT BUG:endif
    if(isep /= -1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames to do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios == 0)then
        sep='/'
        exit FOUND
    endif
    ! check variables names common to many platforms that usually have a
    ! directory path in them although a ULS file can contain a backslash
    ! and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using
    ! POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME']
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),'\') /= 0)then
          sep='\'
          exit FOUND
       elseif(index(get_env(envnames(i)),'/') /= 0)then
          sep='/'
          exit FOUND
       endif
    enddo

    write(*,*)'<WARNING>unknown system directory path separator'
    sep='\'
    endblock FOUND
    !x!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary()
! convert command line arguments to dictionary entries
!x!logical                      :: guess_if_value
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i, jj, kk
integer                      :: ilength, istatus, imax
character(len=1)             :: letter
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
logical                      :: next_mandatory
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START'
   next_mandatory=.false.
   nomore=.false.
   pointer=0
   lastkeyword=' '
   G_keyword_single_letter=.true.
   i=1
   current_argument=''
   GET_ARGS: do while (get_next_argument()) ! insert and replace entries
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:WHILE:CURRENT_ARGUMENT=',current_argument

      if( current_argument  ==  '-' .and. nomore .eqv. .true. )then   ! sort of
      elseif( current_argument  ==  '-')then                          ! sort of
         current_argument='"stdin"'
      endif
      if( current_argument  ==  '--' .and. nomore .eqv. .true. )then  ! -- was already encountered
      elseif( current_argument  ==  '--' )then                        ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         if(G_remaining_option_allowed)then
            G_remaining_on=.true.
         endif
         cycle GET_ARGS
      endif

      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '

      if(.not.next_mandatory.and..not.nomore.and.current_argument_padded(1:2) == '--')then    ! beginning of long word
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START_LONG:'
         G_keyword_single_letter=.false.
         if(lastkeyword /= '')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(3:),pointer)
         if(pointer <= 0)then
            if(G_QUIET)then
               lastkeyword="UNKNOWN"
               pointer=0
               cycle GET_ARGS
            endif
            call print_dictionary('UNKNOWN LONG KEYWORD: '//current_argument)
            call mystop(1)
            return
         endif
         lastkeyword=trim(current_argument_padded(3:))
         next_mandatory=mandatory(pointer)
      elseif(.not.next_mandatory &
      & .and..not.nomore &
      & .and.current_argument_padded(1:1) == '-' &
      & .and.index("0123456789.",dummy(2:2)) == 0)then
      ! short word
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START_SHORT'
         G_keyword_single_letter=.true.
         if(lastkeyword /= '')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(2:),pointer)
         jj=len(current_argument)
         if( (pointer <= 0.or.jj.ge.3).and.(G_STRICT) )then  ! name not found
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT NOT FOUND:',current_argument_padded(2:)
            ! in strict mode this might be multiple single-character values
            do kk=2,jj
               letter=current_argument_padded(kk:kk)
               call locate_key(letter,pointer)
               if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:LETTER:',letter,pointer
               if(pointer > 0)then
                  call update(keywords(pointer),'T')
               else
                  if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNKNOWN SHORT:',letter
                  call print_dictionary('UNKNOWN SHORT KEYWORD:'//letter) ! //' in '//current_argument)
                  if(G_QUIET)then
                     lastkeyword="UNKNOWN"
                     pointer=0
                     cycle GET_ARGS
                  endif
                  call mystop(2)
                  return
               endif
               current_argument='-'//current_argument_padded(jj:jj)
            enddo
            !--------------
            lastkeyword=""
            pointer=0
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT_END:2:'
            cycle GET_ARGS
            !--------------
         elseif(pointer<0)then
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNKNOWN SHORT_CONFIRMED:',letter
            call print_dictionary('UNKNOWN SHORT KEYWORD:'//current_argument_padded(2:))
            if(G_QUIET)then
               lastkeyword="UNKNOWN"
               pointer=0
               cycle GET_ARGS
            endif
            call mystop(2)
            return
         endif
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT_END:1:'
         lastkeyword=trim(current_argument_padded(2:))
         next_mandatory=mandatory(pointer)
      elseif(pointer == 0)then                                       ! unnamed arguments
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNNAMED ARGUMENT:',current_argument
         if(G_remaining_on)then
            if(len(current_argument) < 1)then
               G_remaining=G_remaining//'"" '
            elseif(current_argument(1:1) == '-')then
               !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
               G_remaining=G_remaining//'"'//current_argument//'" '
            else
               G_remaining=G_remaining//'"'//current_argument//'" '
            endif
            imax=max(len(args),len(current_argument))
            args=[character(len=imax) :: args,current_argument]
         else
            imax=max(len(unnamed),len(current_argument))
            if(scan(current_argument//' ','@') == 1.and.G_response)then
               if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:1:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
               call expand_response(current_argument)
            else
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
         endif
      else
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:FOUND:',current_argument
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1) == '"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue) == 'F'.or.upper(oldvalue) == 'T')then  ! assume boolean parameter
            if(current_argument /= ' ')then
               if(G_remaining_on)then
                  if(len(current_argument) < 1)then
                        G_remaining=G_remaining//'"" '
                  elseif(current_argument(1:1) == '-')then
                       !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  else
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  endif
                  imax=max(len(args),len(current_argument))
                  args=[character(len=imax) :: args,current_argument]
               else
                  imax=max(len(unnamed),len(current_argument))
                  if(scan(current_argument//' ','@') == 1.and.G_response)then
                    if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:2:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
                    call expand_response(current_argument)
                  else
                    unnamed=[character(len=imax) :: unnamed,current_argument]
                  endif
               endif
            endif
            current_argument='T'
         endif
         call update(keywords(pointer),current_argument)
         pointer=0
         lastkeyword=''
         next_mandatory=.false.
      endif
   enddo GET_ARGS
   if(lastkeyword /= '')then
      call ifnull()
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:NORMAL END'

contains

subroutine ifnull()
   oldvalue=get(lastkeyword)//' '
   if(upper(oldvalue) == 'F'.or.upper(oldvalue) == 'T')then
      call update(lastkeyword,'T')
   elseif(oldvalue(1:1) == '"')then
      call update(lastkeyword,'" "')
   else
      call update(lastkeyword,' ')
   endif
end subroutine ifnull

function get_next_argument()
!
! get next argument from command line into allocated variable current_argument
!
logical,save :: hadequal=.false.
character(len=:),allocatable,save :: right_hand_side
logical :: get_next_argument
integer :: iright
integer :: iequal

   if(hadequal)then  ! use left-over value from previous -NAME=VALUE syntax
      current_argument=right_hand_side
      right_hand_side=''
      hadequal=.false.
      get_next_argument=.true.
      ilength=len(current_argument)
      return
   endif

   if(i>command_argument_count())then
      get_next_argument=.false.
      return
   else
      get_next_argument=.true.
   endif

   call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
   if(istatus /= 0) then                                                                          ! on error
      write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
         &'status=',istatus,&
         &'length=',ilength
      get_next_argument=.false.
   else
      ilength=max(ilength,1)
      if(allocated(current_argument))deallocate(current_argument)
      allocate(character(len=ilength) :: current_argument)
      call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
      if(istatus /= 0) then                                                                       ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength,&
            &'target length=',len(current_argument)
         get_next_argument=.false.
       endif

       ! if an argument keyword and an equal before a space split on equal and save right hand side for next call
       if(nomore)then
       elseif( len(current_argument) == 0)then
       else
          iright=index(current_argument,' ')
          if(iright == 0)iright=len(current_argument)
          iequal=index(current_argument(:iright),'=')
          if(next_mandatory)then
          elseif(iequal /= 0.and.current_argument(1:1) == '-')then
             if(iequal /= len(current_argument))then
                right_hand_side=current_argument(iequal+1:)
             else
                right_hand_side=''
             endif
             hadequal=.true.
             current_argument=current_argument(:iequal-1)
          endif
       endif
   endif
   i=i+1
end function get_next_argument

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_dictionary(3f) - [ARGUMENTS:M_CLI2] print internal dictionary
!!    created by calls to set_args(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine print_dictionary(header,stop)
!!
!!      character(len=*),intent(in),optional :: header
!!      logical,intent(in),optional          :: stop
!!##DESCRIPTION
!!    Print the internal dictionary created by calls to set_args(3f).
!!    This routine is intended to print the state of the argument list
!!    if an error occurs in using the set_args(3f) procedure.
!!##OPTIONS
!!     HEADER  label to print before printing the state of the command
!!             argument list.
!!     STOP    logical value that if true stops the program after displaying
!!             the dictionary.
!!##EXAMPLE
!!
!!
!!
!! Typical usage:
!!
!!       program demo_print_dictionary
!!       use M_CLI2,  only : set_args, get_args
!!       implicit none
!!       real :: x, y, z
!!          call set_args('-x 10 -y 20 -z 30')
!!          call get_args('x',x,'y',y,'z',z)
!!          ! all done cracking the command line; use the values in your program.
!!          write(*,*)x,y,z
!!       end program demo_print_dictionary
!!
!!      Sample output
!!
!!      Calling the sample program with an unknown parameter or the --usage
!!      switch produces the following:
!!
!!         $ ./demo_print_dictionary -A
!!         UNKNOWN SHORT KEYWORD: -A
!!         KEYWORD             PRESENT  VALUE
!!         z                   F        [3]
!!         y                   F        [2]
!!         x                   F        [1]
!!         help                F        [F]
!!         version             F        [F]
!!         usage               F        [F]
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine print_dictionary(header,stop)
character(len=*),intent(in),optional :: header
logical,intent(in),optional          :: stop
integer          :: i
   if(G_QUIET)return
   if(present(header))then
      if(header /= '')then
         write(warn,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords) > 0)then
         write(warn,'(a,1x,a,1x,a,1x,a)')atleast('KEYWORD',max(len(keywords),8)),'SHORT','PRESENT','VALUE'
         write(warn,'(*(a,1x,a5,1x,l1,8x,"[",a,"]",/))') &
         & (atleast(keywords(i),max(len(keywords),8)),shorts(i),present_in(i),values(i)(:counts(i)),i=size(keywords),1,-1)
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed) > 0)then
         write(warn,'(a)')'UNNAMED'
         write(warn,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
   if(allocated(args))then
      if(size(args) > 0)then
         write(warn,'(a)')'ARGS'
         write(warn,'(i6.6,3a)')(i,'[',args(i),']',i=1,size(args))
      endif
   endif
   if(G_remaining /= '')then
      write(warn,'(a)')'REMAINING'
      write(warn,'(a)')G_remaining
   endif
   if(present(stop))then
      if(stop) call mystop(5)
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_args(3f) - [ARGUMENTS:M_CLI2] return keyword values when parsing
!!    command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   get_args(3f) and its convenience functions:
!!
!!     use M_CLI2, only : get_args
!!     ! convenience functions
!!     use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!     use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!
!!     subroutine get_args(name,value,delimiters)
!!
!!      character(len=*),intent(in) :: name
!!
!!      type(${TYPE}),allocatable,intent(out) :: value(:)
!!      ! or
!!      type(${TYPE}),allocatable,intent(out) :: value
!!
!!      character(len=*),intent(in),optional :: delimiters
!!
!!      where ${TYPE} may be from the set
!!              {real,doubleprecision,integer,logical,complex,character(len=:)}
!!##DESCRIPTION
!!
!!    GET_ARGS(3f) returns the value of keywords after SET_ARGS(3f) has
!!    been called to parse the command line. For fixed-length CHARACTER
!!    variables see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
!!    GET_ARGS_FIXED_SIZE(3f).
!!
!!    As a convenience multiple pairs of keywords and variables may be
!!    specified if and only if all the values are scalars and the CHARACTER
!!    variables are fixed-length or pre-allocated.
!!
!!##OPTIONS
!!
!!     NAME        name of commandline argument to obtain the value of
!!     VALUE       variable to hold returned value. The kind of the value
!!                 is used to determine the type of returned value. May
!!                 be a scalar or allocatable array. If type is CHARACTER
!!                 the scalar must have an allocatable length.
!!     DELIMITERS  By default the delimiter for array values are comma,
!!                 colon, and whitespace. A string containing an alternate
!!                 list of delimiter characters may be supplied.
!!
!!##CONVENIENCE FUNCTIONS
!!    There are convenience functions that are replacements for calls to
!!    get_args(3f) for each supported default intrinsic type
!!
!!      o scalars -- dget(3f), iget(3f), lget(3f), rget(3f), sget(3f),
!!                   cget(3f)
!!      o vectors -- dgets(3f), igets(3f), lgets(3f), rgets(3f),
!!                   sgets(3f), cgets(3f)
!!
!!    D is for DOUBLEPRECISION, I for INTEGER, L for LOGICAL, R for REAL,
!!    S for string (CHARACTER), and C for COMPLEX.
!!
!!    If the functions are called with no argument they will return the
!!    UNNAMED array converted to the specified type.
!!
!!##EXAMPLE
!!
!!
!! Sample program:
!!
!!     program demo_get_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     implicit none
!!     integer                      :: i
!!      ! Define ARGS
!!     real                         :: x, y, z
!!     real,allocatable             :: p(:)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!      ! Define and parse (to set initial values) command line
!!      !   o only quote strings and use double-quotes
!!      !   o set all logical values to F or T.
!!     call set_args('         &
!!        & -x 1 -y 2 -z 3     &
!!        & -p -1,-2,-3        &
!!        & --title "my title" &
!!        & -l F -L F          &
!!        & --label " "        &
!!        & ')
!!      ! Assign values to elements
!!      ! Scalars
!!     call get_args('x',x,'y',y,'z',z,'l',l,'L',lbig)
!!      ! Allocatable string
!!     call get_args('title',title)
!!      ! Allocatable arrays
!!     call get_args('p',p)
!!      ! Use values
!!     write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_get_args
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_length(3f) - [ARGUMENTS:M_CLI2] return keyword values
!!    for fixed-length string when parsing command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_length(name,value)
!!
!!     character(len=*),intent(in)  :: name
!!     character(len=:),allocatable :: value
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    get_args_fixed_length(3f) returns the value of a string
!!    keyword when the string value is a fixed-length CHARACTER
!!    variable.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to obtain the value of
!!
!!    VALUE  variable to hold returned value.
!!           Must be a fixed-length CHARACTER variable.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_length
!!     use M_CLI2,  only : set_args, get_args_fixed_length
!!     implicit none
!!
!!      ! Define args
!!     character(len=80)   :: title
!!      ! Parse command line
!!     call set_args(' --title "my title" ')
!!      ! Assign values to variables
!!     call get_args_fixed_length('title',title)
!!      ! Use values
!!     write(*,*)'title=',title
!!
!!     end program demo_get_args_fixed_length
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_size(3f) - [ARGUMENTS:M_CLI2] return keyword values
!!    for fixed-size array when parsing command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_size(name,value)
!!
!!     character(len=*),intent(in) :: name
!!     [real|doubleprecision|integer|logical|complex] :: value(NNN)
!!        or
!!     character(len=MMM) :: value(NNN)
!!
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    get_args_fixed_size(3f) returns the value of keywords for fixed-size
!!    arrays after set_args(3f) has been called. On input on the command
!!    line all values of the array must be specified.
!!
!!##OPTIONS
!!    NAME        name of commandline argument to obtain the value of
!!
!!    VALUE       variable to hold returned values. The kind of the value
!!                is used to determine the type of returned value. Must be
!!                a fixed-size array. If type is CHARACTER the length must
!!                also be fixed.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_size
!!     use M_CLI2,  only : set_args, get_args_fixed_size
!!     implicit none
!!     integer,parameter   :: dp=kind(0.0d0)
!!     ! DEFINE ARGS
!!     real                :: x(2)
!!     real(kind=dp)       :: y(2)
!!     integer             :: p(3)
!!     character(len=80)   :: title(1)
!!     logical             :: l(4), lbig(4)
!!     complex             :: cmp(2)
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o only quote strings
!!     !   o set all logical values to F or T.
!!     call set_args(' &
!!        & -x 10.0,20.0 &
!!        & -y 11.0,22.0 &
!!        & -p -1,-2,-3 &
!!        & --title "my title" &
!!        & -l F,T,F,T -L T,F,T,F  &
!!        & --cmp 111,222.0,333.0e0,4444 &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!        call get_args_fixed_size('x',x)
!!        call get_args_fixed_size('y',y)
!!        call get_args_fixed_size('p',p)
!!        call get_args_fixed_size('title',title)
!!        call get_args_fixed_size('l',l)
!!        call get_args_fixed_size('L',lbig)
!!        call get_args_fixed_size('cmp',cmp)
!!     ! USE VALUES
!!        write(*,*)'x=',x
!!        write(*,*)'p=',p
!!        write(*,*)'title=',title
!!        write(*,*)'l=',l
!!        write(*,*)'L=',lbig
!!        write(*,*)'cmp=',cmp
!!     end program demo_get_args_fixed_size
!!   Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine get_fixedarray_class(keyword,generic,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
class(*)                             :: generic(:)
character(len=*),intent(in),optional :: delimiters
   select type(generic)
    type is (character(len=*));  call get_fixedarray_fixed_length_c(keyword,generic,delimiters)
    type is (integer);           call get_fixedarray_i(keyword,generic,delimiters)
    type is (real);              call get_fixedarray_r(keyword,generic,delimiters)
    type is (complex);           call get_fixed_size_complex(keyword,generic,delimiters)
    type is (real(kind=dp));     call get_fixedarray_d(keyword,generic,delimiters)
    type is (logical);           call get_fixedarray_l(keyword,generic,delimiters)
    class default
      call mystop(-7,'*get_fixedarray_class* crud -- procedure does not know about this type')
   end select
end subroutine get_fixedarray_class
!===================================================================================================================================
! return allocatable arrays
!===================================================================================================================================
subroutine get_anyarray_l(keyword,larray,delimiters)

! ident_5="@(#) M_CLI2 get_anyarray_l(3f) given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
character(len=*),intent(in),optional   :: delimiters
character(len=:),allocatable :: carray(:)                  ! convert value to an array
character(len=:),allocatable :: val
integer                      :: i
integer                      :: place
integer                      :: iichar                     ! point to first character of word unless first character is "."
   call locate_key(keyword,place)                          ! find where string is or should be
   if(place > 0)then                                      ! if string was found
      val=values(place)(:counts(place))
      call split(adjustl(upper(val)),carray,delimiters=delimiters)  ! convert value to uppercase, trimmed; then parse into array
   else
      call journal('*get_anyarray_l* unknown keyword',keyword)
      call mystop(8 ,'*get_anyarray_l* unknown keyword '//keyword)
      if(allocated(larray))deallocate(larray)
      allocate(larray(0))
      return
   endif
   if(size(carray) > 0)then                                  ! if not a null string
      if(allocated(larray))deallocate(larray)
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i)(1:1) == '.')then                        ! looking for fortran logical syntax .STRING.
            iichar=2
         else
            iichar=1
         endif
         select case(carray(i)(iichar:iichar))             ! check word to see if true or false
         case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
         case('F','N');     larray(i)=.false.              ! assume this is false or no
         case default
            call journal("*get_anyarray_l* bad logical expression for ",(keyword),'=',carray(i))
         end select
      enddo
   else                                                       ! for a blank string return one T
      if(allocated(larray))deallocate(larray)
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.true.
   endif
end subroutine get_anyarray_l
!===================================================================================================================================
subroutine get_anyarray_d(keyword,darray,delimiters)

! ident_6="@(#) M_CLI2 get_anyarray_d(3f) given keyword fetch dble value array from Language Dictionary (0 on err)"

character(len=*),intent(in)           :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp),allocatable,intent(out) :: darray(:)    ! function type
character(len=*),intent(in),optional  :: delimiters

character(len=:),allocatable          :: carray(:)    ! convert value to an array using split(3f)
integer                               :: i
integer                               :: place
integer                               :: ierr
character(len=:),allocatable          :: val
!-----------------------------------------------------------------------------------------------------------------------------------
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                 ! if string was found
      val=values(place)(:counts(place))
      val=replace_str(val,'(','')
      val=replace_str(val,')','')
      call split(val,carray,delimiters=delimiters)    ! find value associated with keyword and split it into an array
   else
      call journal('*get_anyarray_d* unknown keyword '//keyword)
      call mystop(9 ,'*get_anyarray_d* unknown keyword '//keyword)
      if(allocated(darray))deallocate(darray)
      allocate(darray(0))
      return
   endif
   if(allocated(darray))deallocate(darray)
   allocate(darray(size(carray)))                     ! create the output array
   do i=1,size(carray)
      call a2d(carray(i), darray(i),ierr) ! convert the string to a numeric value
      if(ierr /= 0)then
         call mystop(10 ,'*get_anyarray_d* unreadable value '//carray(i)//' for keyword '//keyword)
      endif
   enddo
end subroutine get_anyarray_d
!===================================================================================================================================
subroutine get_anyarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer,allocatable                  :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   iarray=nint(darray)
end subroutine get_anyarray_i
!===================================================================================================================================
subroutine get_anyarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real,allocatable                     :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   rarray=real(darray)
end subroutine get_anyarray_r
!===================================================================================================================================
subroutine get_anyarray_x(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex(kind=sp),allocatable         :: xarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: half,sz,i
   call get_anyarray_d(keyword,darray,delimiters)
   sz=size(darray)
   half=sz/2
   if(sz /= half+half)then
      call journal('*get_anyarray_x* uneven number of values defining complex value '//keyword)
      call mystop(11,'*get_anyarray_x* uneven number of values defining complex value '//keyword)
      if(allocated(xarray))deallocate(xarray)
      allocate(xarray(0))
   endif

   !x!================================================================================================
   !x!IFORT,GFORTRAN OK, NVIDIA RETURNS NULL ARRAY: xarray=cmplx(real(darray(1::2)),real(darray(2::2)))
   if(allocated(xarray))deallocate(xarray)
   allocate(xarray(half))
   do i=1,sz,2
      xarray((i+1)/2)=cmplx( darray(i),darray(i+1),kind=sp )
   enddo
   !x!================================================================================================

end subroutine get_anyarray_x
!===================================================================================================================================
subroutine get_anyarray_c(keyword,strings,delimiters)

! ident_8="@(#)M_CLI2::get_anyarray_c(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=:),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
integer                              :: place
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings,delimiters=delimiters)   ! find value associated with keyword and split it into an array
   else
      call journal('*get_anyarray_c* unknown keyword '//keyword)
      call mystop(12,'*get_anyarray_c* unknown keyword '//keyword)
      if(allocated(strings))deallocate(strings)
      allocate(character(len=0)::strings(0))
   endif
end subroutine get_anyarray_c
!===================================================================================================================================
!===================================================================================================================================
subroutine get_args_fixed_length_a_array(keyword,strings,delimiters)

! ident_7="@(#) M_CLI2 get_args_fixed_length_a_array(3f) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=*),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: strings_a(:)
integer                              :: place
character(len=:),allocatable         :: val
integer                              :: ibug
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings_a,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      if( len(strings_a) <= len(strings) )then
         strings=strings_a
      else
         ibug=len(strings)
         call journal('*get_args_fixed_length_a_array* values too long. Longest is',len(strings_a),'allowed is',ibug)
         write(*,'("strings=",3x,*(a,1x))')strings
         call journal('*get_args_fixed_length_a_array* keyword='//keyword)
         call mystop(13,'*get_args_fixed_length_a_array* keyword='//keyword)
         strings=[character(len=len(strings)) ::]
      endif
   else
      call journal('*get_args_fixed_length_a_array* unknown keyword '//keyword)
      call mystop(14,'*get_args_fixed_length_a_array* unknown keyword '//keyword)
      strings=[character(len=len(strings)) ::]
   endif
end subroutine get_args_fixed_length_a_array
!===================================================================================================================================
! return non-allocatable arrays
!===================================================================================================================================
subroutine get_fixedarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer                              :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(iarray,dim=1) == dsize)then
      iarray=nint(darray)
   else
      ibug=size(iarray)
      call journal('*get_fixedarray_i* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(33)
      iarray=0
   endif
end subroutine get_fixedarray_i
!===================================================================================================================================
subroutine get_fixedarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real                                 :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real,allocatable                     :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_r(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(rarray,dim=1) == dsize)then
      rarray=darray
   else
      ibug=size(rarray)
      call journal('*get_fixedarray_r* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(33)
      rarray=0.0
   endif
end subroutine get_fixedarray_r
!===================================================================================================================================
subroutine get_fixed_size_complex(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex                              :: xarray(:)
character(len=*),intent(in),optional :: delimiters
complex,allocatable                  :: darray(:)    ! function type
integer                              :: half, sz
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_x(keyword,darray,delimiters)
   dsize=size(darray)
   sz=dsize*2
   half=sz/2
   if(sz /= half+half)then
      call journal('*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      call mystop(15,'*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      xarray=0
      return
   endif
   if(ubound(xarray,dim=1) == dsize)then
      xarray=darray
   else
      ibug=size(xarray)
      call journal('*get_fixed_size_complex* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(34)
      xarray=cmplx(0.0,0.0)
   endif
end subroutine get_fixed_size_complex
!===================================================================================================================================
subroutine get_fixedarray_d(keyword,darr,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                        :: darr(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(darr,dim=1) == dsize)then
      darr=darray
   else
      ibug=size(darr)
      call journal('*get_fixedarray_d* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(35)
      darr=0.0d0
   endif
end subroutine get_fixedarray_d
!===================================================================================================================================
subroutine get_fixedarray_l(keyword,larray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
logical                              :: larray(:)
character(len=*),intent(in),optional :: delimiters
logical,allocatable                  :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_l(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(larray,dim=1) == dsize)then
      larray=darray
   else
      ibug=size(larray)
      call journal('*get_fixedarray_l* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(36)
      larray=.false.
   endif
end subroutine get_fixedarray_l
!===================================================================================================================================
subroutine get_fixedarray_fixed_length_c(keyword,strings,delimiters)

! ident_8="@(#) M_CLI2 get_fixedarray_fixed_length_c(3f) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*)                     :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: str(:)
character(len=*),intent(in)          :: keyword   ! name to look up in dictionary
integer                              :: place
integer                              :: ssize
integer                              :: ibug
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                 ! find where string is or should be
   if(place > 0)then                              ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,str,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      ssize=size(str)
      if(ssize==size(strings))then
         strings(:ssize)=str
      else
         ibug=size(strings)
         call journal('*get_fixedarray_fixed_length_c* wrong number of values for keyword',&
            & keyword,'got',ssize,'expected ',ibug) !,ubound(strings,dim=1)
         call print_dictionary('USAGE:')
         call mystop(30,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
         strings=''
      endif
   else
      call journal('*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      call mystop(16,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      strings=''
   endif
end subroutine get_fixedarray_fixed_length_c
!===================================================================================================================================
! return scalars
!===================================================================================================================================
subroutine get_scalar_d(keyword,d)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                 :: d
real(kind=dp),allocatable     :: darray(:)    ! function type
integer                       :: ibug
   call get_anyarray_d(keyword,darray)
   if(size(darray) == 1)then
      d=darray(1)
   else
      ibug=size(darray)
      call journal('*get_anyarray_d* incorrect number of values for keyword "',keyword,'" expected one found',ibug)
      call print_dictionary('USAGE:')
      call mystop(31,'*get_anyarray_d* incorrect number of values for keyword "'//keyword//'" expected one')
   endif
end subroutine get_scalar_d
!===================================================================================================================================
subroutine get_scalar_real(keyword,r)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real,intent(out)              :: r
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   r=real(d)
end subroutine get_scalar_real
!===================================================================================================================================
subroutine get_scalar_i(keyword,i)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
integer,intent(out)           :: i
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   i=nint(d)
end subroutine get_scalar_i
!===================================================================================================================================
subroutine get_scalar_anylength_c(keyword,string)

! ident_9="@(#) M_CLI2 get_scalar_anylength_c(3f) Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=:),allocatable,intent(out)  :: string
integer                       :: place
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(17,'*get_anyarray_c* unknown keyword '//keyword)
      call journal('*get_anyarray_c* unknown keyword '//keyword)
      string=''
   endif
end subroutine get_scalar_anylength_c
!===================================================================================================================================
elemental impure subroutine get_args_fixed_length_scalar_c(keyword,string)

! ident_10="@(#) M_CLI2 get_args_fixed_length_scalar_c(3f) Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=*),intent(out)  :: string
integer                       :: place
integer                       :: unlen
integer                       :: ibug
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(18,'*get_args_fixed_length_scalar_c* unknown keyword '//keyword)
      string=''
   endif
   unlen=len_trim(unquote(values(place)(:counts(place))))
   if(unlen>len(string))then
      ibug=len(string)
      call journal('*get_args_fixed_length_scalar_c* value too long for',keyword,'allowed is',ibug,&
      & 'input string [',values(place),'] is',unlen)
      call mystop(19,'*get_args_fixed_length_scalar_c* value too long')
      string=''
   endif
end subroutine get_args_fixed_length_scalar_c
!===================================================================================================================================
subroutine get_scalar_complex(keyword,x)
character(len=*),intent(in) :: keyword      ! keyword to retrieve value from dictionary
complex,intent(out)         :: x
real(kind=dp)               :: d(2)
   call get_fixedarray_d(keyword,d)
   x=cmplx(d(1),d(2),kind=sp)
end subroutine get_scalar_complex
!===================================================================================================================================
subroutine get_scalar_logical(keyword,l)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
logical                       :: l
logical,allocatable           :: larray(:)    ! function type
integer                       :: ibug
   l=.false.
   call get_anyarray_l(keyword,larray)
   if(.not.allocated(larray) )then
      call journal('*get_scalar_logical* expected one value found not allocated')
      call mystop(37,'*get_scalar_logical* incorrect number of values for keyword "'//keyword//'"')
   elseif(size(larray) == 1)then
      l=larray(1)
   else
      ibug=size(larray)
      call journal('*get_scalar_logical* expected one value found',ibug)
      call mystop(21,'*get_scalar_logical* incorrect number of values for keyword "'//keyword//'"')
   endif
end subroutine get_scalar_logical
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! THE REMAINDER SHOULD BE ROUTINES EXTRACTED FROM OTHER MODULES TO MAKE THIS MODULE STANDALONE BY POPULAR REQUEST
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!use M_strings,                     only : UPPER, LOWER, QUOTE, REPLACE_STR=>REPLACE, UNQUOTE, SPLIT, STRING_TO_VALUE
!use M_list,                        only : insert, locate, remove, replace
!use M_journal,                     only : JOURNAL

!use M_args,                        only : LONGEST_COMMAND_ARGUMENT
! routines extracted from other modules
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_command_argument(3f) - [ARGUMENTS:M_args] length of longest
!!    argument on command line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function longest_command_argument() result(ilongest)
!!
!!     integer :: ilongest
!!
!!##DESCRIPTION
!!    length of longest argument on command line. Useful when allocating
!!    storage for holding arguments.
!!##RESULT
!!    longest_command_argument  length of longest command argument
!!##EXAMPLE
!!
!! Sample program
!!
!!      program demo_longest_command_argument
!!      use M_args, only : longest_command_argument
!!         write(*,*)'longest argument is ',longest_command_argument()
!!      end program demo_longest_command_argument
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
function longest_command_argument() result(ilongest)
integer :: i
integer :: ilength
integer :: istatus
integer :: ilongest
   ilength=0
   ilongest=0
   GET_LONGEST: do i=1,command_argument_count()                             ! loop throughout command line arguments to find longest
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining length for argument ',i
         exit GET_LONGEST
      elseif(ilength > 0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    journal(3f) - [M_CLI2] converts a list of standard scalar types to a string and writes message
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine journal(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!
!!##DESCRIPTION
!!    journal(3f) builds and prints a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!    sep         separator to place between values. Defaults to a space.
!!##RETURNS
!!    journal     description to print
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_journal
!!     use M_CLI2, only : journal
!!     implicit none
!!     character(len=:),allocatable :: frmt
!!     integer                      :: biggest
!!
!!     call journal('HUGE(3f) integers',huge(0),'and real',&
!!               & huge(0.0),'and double',huge(0.0d0))
!!     call journal('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!     call journal('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!     call journal('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!
!!     end program demo_journal
!!
!!  Output
!!
!!     HUGE(3f) integers 2147483647 and real 3.40282347E+38 and
!!     double 1.7976931348623157E+308
!!     real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!     doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!     12345.678900000001 2.2250738585072014E-308
!!     complex         : (3.40282347E+38,1.17549435E-38)
!!      format=(*(i9:,1x))
!!      program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine journal(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)

! ident_11="@(#) M_CLI2 journal(3fp) writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional         :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable         :: sep_local
character(len=4096)                  :: line
integer                              :: istart
integer                              :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=''
   if(present(g0))call print_generic(g0)
   if(present(g1))call print_generic(g1)
   if(present(g2))call print_generic(g2)
   if(present(g3))call print_generic(g3)
   if(present(g4))call print_generic(g4)
   if(present(g5))call print_generic(g5)
   if(present(g6))call print_generic(g6)
   if(present(g7))call print_generic(g7)
   if(present(g8))call print_generic(g8)
   if(present(g9))call print_generic(g9)
   if(present(ga))call print_generic(ga)
   if(present(gb))call print_generic(gb)
   if(present(gc))call print_generic(gc)
   if(present(gd))call print_generic(gd)
   if(present(ge))call print_generic(ge)
   if(present(gf))call print_generic(gf)
   if(present(gg))call print_generic(gg)
   if(present(gh))call print_generic(gh)
   if(present(gi))call print_generic(gi)
   if(present(gj))call print_generic(gj)
   write(*,'(a)')trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64))
         write(line(istart:),'(1pg0)') generic
      !x! DOES NOT WORK WITH NVFORTRAN: type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical)
         write(line(istart:),'(l1)') generic
      type is (character(len=*))
         write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end subroutine journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function upper(str) result (string)

! ident_12="@(#) M_CLI2 upper(3f) Changes a string to uppercase"

character(*), intent(in)      :: str
character(:),allocatable      :: string
integer                       :: i
   string = str
   do i = 1, len_trim(str)
       select case (str(i:i))
       case ('a':'z')
          string(i:i) = char(iachar(str(i:i))-32)
       end select
   end do
end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lower(str) result (string)

! ident_13="@(#) M_CLI2 lower(3f) Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(:),allocatable     :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2i(chars,valu,ierr)

! ident_14="@(#) M_CLI2 a2i(3fp) subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
integer,parameter           :: ihuge=huge(0)
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8 <= huge(valu))then
      if(valu8 <= huge(valu))then
         valu=int(valu8)
      else
         call journal('*a2i*','- value too large',valu8,'>',ihuge)
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_15="@(#) M_CLI2 a2d(3fp) subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   local_chars=replace_str(local_chars,',','')                  ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         write(frmt,"('(Z',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         write(frmt,"('(B',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         write(frmt,"('(O',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr /= 0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(f3.3)')valu
      endif
      if(local_chars /= 'eod')then                           ! print warning message except for special value "eod"
         call journal('*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg /= '')then
            call journal('*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_CLI2:TOKENS] parse string into an array using specified
!!    delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!##DESCRIPTION
!!    SPLIT(3f) parses a string using specified delimiter characters and
!!    store tokens into an allocatable array
!!
!!##OPTIONS
!!
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!
!!    NULLS IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_split
!!     use M_CLI2, only: split
!!     character(len=*),parameter     :: &
!!     & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!     character(len=:),allocatable :: array(:) ! output array of tokens
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,'(80("="))')
!!        write(*,*)'typical call:'
!!        CALL split(line,array)
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'custom list of delimiters (colon and vertical line):'
!!        CALL split(line,array,delimiters=':|',order='sequential',nulls='ignore')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)&
!!      &'custom list of delimiters, reverse array order and count null fields:'
!!        CALL split(line,array,delimiters=':|',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,*)&
!!        &'default delimiters and reverse array order and return null fields:'
!!        CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!     end program demo_split
!!
!!   Output
!!
!!    > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    > ===========================================================================
!!    >  typical call:
!!    > 1 ==> aBcdef
!!    > 2 ==> ghijklmnop
!!    > 3 ==> qrstuvwxyz
!!    > 4 ==> 1:|:2
!!    > 5 ==> 333|333
!!    > 6 ==> a
!!    > 7 ==> B
!!    > 8 ==> cc
!!    >  SIZE:           8
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters (colon and vertical line):
!!    > 1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    > 2 ==> 2     333
!!    > 3 ==> 333 a B cc
!!    >  SIZE:           3
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters, reverse array order and return null fields:
!!    > 1 ==> 333 a B cc
!!    > 2 ==> 2     333
!!    > 3 ==>
!!    > 4 ==>
!!    > 5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    >  SIZE:           5
!!    > --------------------------------------------------------------------------
!!    >  INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    >  default delimiters and reverse array order and count null fields:
!!    > 1 ==>
!!    > 2 ==>
!!    > 3 ==>
!!    > 4 ==> cc
!!    > 5 ==> B
!!    > 6 ==> a
!!    > 7 ==> 333|333
!!    > 8 ==>
!!    > 9 ==>
!!    > 10 ==>
!!    > 11 ==>
!!    > 12 ==> 1:|:2
!!    > 13 ==>
!!    > 14 ==> qrstuvwxyz
!!    > 15 ==> ghijklmnop
!!    > 16 ==>
!!    > 17 ==>
!!    > 18 ==> aBcdef
!!    > 19 ==>
!!    > 20 ==>
!!    >  SIZE:          20
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_16="@(#) M_CLI2 split(3f) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: iilen                  ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:' ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:'    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ???
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ???
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   iilen=len(input_line)                                          ! IILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
   if(iilen > 0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,iilen,1                                  ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=iilen                                      ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):iilen),dlim(i10:i10))
               IF(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol > iilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   if(allocated(array))deallocate(array)
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace_str(3f) - [M_CLI2:EDITING] function globally replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function replace_str(targetline,old,new,range,ierr) result (newline)
!!
!!     character(len=*)               :: targetline
!!     character(len=*),intent(in)    :: old
!!     character(len=*),intent(in)    :: new
!!     integer,intent(in),optional    :: range(2)
!!     integer,intent(out),optional   :: ierr
!!     logical,intent(in),optional    :: clip
!!     character(len=:),allocatable   :: newline
!!##DESCRIPTION
!!    Globally replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     range       if present, only change range(1) to range(2) of
!!                 occurrences of old string
!!     ierr        error code. If ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!     clip        whether to return trailing spaces or not. Defaults to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!
!!##EXAMPLES
!!
!! Sample Program:
!!
!!       program demo_replace_str
!!       use M_CLI2, only : replace_str
!!       implicit none
!!       character(len=:),allocatable :: targetline
!!
!!       targetline='this is the input string'
!!
!!       call testit('th','TH','THis is THe input string')
!!
!!       ! a null old substring means "at beginning of line"
!!       call testit('','BEFORE:', 'BEFORE:THis is THe input string')
!!
!!       ! a null new string deletes occurrences of the old substring
!!       call testit('i','', 'BEFORE:THs s THe nput strng')
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A')
!!       write(*,*)'replace a with A ['//targetline//']'
!!
!!       write(*,*)'Examples of the use of RANGE='
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A',range=[3,5])
!!       write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','',range=[3,5])
!!       write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!        & 'aa','CCCC',range=[3,5])
!!       write(*,*)'replace aa with CCCC instances 3 to 5 ['//targetline//']'
!!
!!       contains
!!       subroutine testit(old,new,expected)
!!       character(len=*),intent(in) :: old,new,expected
!!       write(*,*)repeat('=',79)
!!       write(*,*)':STARTED ['//targetline//']'
!!       write(*,*)':OLD['//old//']', ' NEW['//new//']'
!!       targetline=replace_str(targetline,old,new)
!!       write(*,*)':GOT     ['//targetline//']'
!!       write(*,*)':EXPECTED['//expected//']'
!!       write(*,*)':TEST    [',targetline == expected,']'
!!       end subroutine testit
!!
!!       end program demo_replace_str
!!
!!   Expected output
!!
!!     ===============================================================================
!!     STARTED [this is the input string]
!!     OLD[th] NEW[TH]
!!     GOT     [THis is THe input string]
!!     EXPECTED[THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [THis is THe input string]
!!     OLD[] NEW[BEFORE:]
!!     GOT     [BEFORE:THis is THe input string]
!!     EXPECTED[BEFORE:THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [BEFORE:THis is THe input string]
!!     OLD[i] NEW[]
!!     GOT     [BEFORE:THs s THe nput strng]
!!     EXPECTED[BEFORE:THs s THe nput strng]
!!     TEST    [ T ]
!!     replace a with A [A b Ab bAAA AAAA]
!!     Examples of the use of RANGE=
!!     replace a with A instances 3 to 5 [a b ab bAAA aaaa]
!!     replace a with null instances 3 to 5 [a b ab b aaaa]
!!     replace aa with CCCC instances 3 to 5 [a b ab baaa aaCCCC CCCC CCCC
!!     a a a aa aaaaaa]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace_str(targetline,old,new,ierr,range) result (newline)

! ident_17="@(#) M_CLI2 replace_str(3f) Globally replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in)            :: old          ! old substring to replace
character(len=*),intent(in)            :: new          ! new substring
integer,intent(out),optional           :: ierr         ! error code. If ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional            :: range(2)     ! start and end of which changes to make
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
integer  :: icount,ichange
integer  :: original_input_length
integer  :: len_old, len_new
integer  :: ladd
integer  :: left_margin, right_margin
integer  :: ind
integer  :: ic
integer  :: iichar
integer  :: range_local(2)
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(range))then
      range_local=range
   else
      range_local=[1,original_input_length]
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(len_new > 0)then
         newline=new(:len_new)//targetline(left_margin:original_input_length)
      else
         newline=targetline(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iichar=left_margin                                  ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1 ! try finding start of OLD in remaining part of input in change window
      if(ind == ic-1.or.ind > right_margin)then           ! did not find old string or found old string past edit window
         exit loop                                        ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind > ic)then                                 ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:iichar-1)//targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(icount >= range_local(1).and.icount <= range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new /= 0)then                                          ! put in new string
            newline=newline(:iichar-1)//new(:len_new)
            iichar=iichar+len_new
         endif
      else
         if(len_old /= 0)then                                          ! put in copy of old string
            newline=newline(:iichar-1)//old(:len_old)
            iichar=iichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline                               ! if no changes made output should be input
   case default
      if(ic <= len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:iichar-1)//targetline(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    quote(3f) - [M_CLI2:QUOTES] add quotes to string as if written with
!!    list-directed input
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str         input string to add quotes to, using the rules of
!!                list-directed input (single quotes are replaced by two
!!                adjacent quotes)
!!    mode        alternate quoting methods are supported:
!!
!!                   DOUBLE   default. replace quote with double quotes
!!                   ESCAPE   replace quotes with backslash-quote instead
!!                            of double quotes
!!
!!    clip        default is to trim leading and trailing spaces from the
!!                string. If CLIP
!!                is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_quote
!!     use M_CLI2, only : quote
!!     implicit none
!!     character(len=:),allocatable :: str
!!     character(len=1024)          :: msg
!!     integer                      :: ios
!!     character(len=80)            :: inline
!!        do
!!           write(*,'(a)',advance='no')'Enter test string:'
!!           read(*,'(a)',iostat=ios,iomsg=msg)inline
!!           if(ios /= 0)then
!!              write(*,*)trim(inline)
!!              exit
!!           endif
!!
!!           ! the original string
!!           write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!           ! the string processed by quote(3f)
!!           str=quote(inline)
!!           write(*,'(a)')'QUOTED     ['//str//']'
!!
!!           ! write the string list-directed to compare the results
!!           write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!           write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!        enddo
!!     end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
logical                              :: clip_local
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(mode))then
      local_mode=mode
   else
      local_mode='DOUBLE'
   endif

   if(present(clip))then
      clip_local=clip
   else
      clip_local=.false.
   endif

   if(clip_local)then
      quoted_str=adjustl(str)
   else
      quoted_str=str
   endif

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','\"'))//double_quote
   case default
      call journal('*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select

end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    unquote(3f) - [M_CLI2:QUOTES] remove quotes from string as if read
!!    with list-directed input
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   pure function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!##EXAMPLE
!!
!! Sample program:
!!
!!       program demo_unquote
!!       use M_CLI2, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!       end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   if(allocated(unquoted_str))deallocate(unquoted_str)
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen >= 1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1) == single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before == iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current == quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before == quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before /= iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_CLI2:BASE] convert whole number string in base
!!                     [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_decodebase
!!      use M_CLI2, only : codebase, decodebase
!!      implicit none
!!      integer           :: ba,bd
!!      character(len=40) :: x,y
!!      integer           :: r
!!
!!      print *,' BASE CONVERSION'
!!      write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!      write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!      INFINITE: do
!!         print *,''
!!         write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!         if(x == '0') exit INFINITE
!!         if(decodebase(x,bd,r)) then
!!            if(codebase(r,ba,y)) then
!!              write(*,'("In base ",I2,": ",A20)')  ba, y
!!            else
!!              print *,'Error in coding number.'
!!            endif
!!         else
!!            print *,'Error in decoding number.'
!!         endif
!!      enddo INFINITE
!!
!!      end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)

! ident_18="@(#) M_CLI2 decodebase(3f) convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein == 0.and.ipound > 1)then                                  ! split string into two values
     call a2i(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local >= 0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        IF(CH == '-'.AND.K == 1)THEN
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    locate_(3f) - [M_CLI2] finds the index where a string is found or
!!                  should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate_(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE_(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update_dic(arr,'b')
!!     call update_dic(arr,'[')
!!     call update_dic(arr,'c')
!!     call update_dic(arr,'ZZ')
!!     call update_dic(arr,'ZZZZ')
!!     call update_dic(arr,'z')
!!
!!     contains
!!     subroutine update_dic(arr,string)
!!     character(len=:),intent(in),allocatable :: arr(:)
!!     character(len=*),intent(in)  :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate_(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end == 0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus == 1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus == end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update_dic
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_19="@(#) M_CLI2 locate_c(3f) find PLACE in sorted character array LIST where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize == 0)then
      maxtry=0
      place=-1
   else
      maxtry=nint(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value == list(PLACE))then
         exit LOOP
      elseif(value > list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin > imax)then
         place=-imin
         if(iabs(place) > arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place > arraysize.or.place <= 0)then
         message='*locate_* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate_* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   elseif(error /= 0)then
      write(warn,*)message//' VALUE=',trim(value)//' PLACE=',place
      call mystop(-24,'(*locate_c* '//message)
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove_(3f) - [M_CLI2] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove_(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!! Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_, remove_
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove_(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove_(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_20="@(#) M_CLI2 remove_c(3fp) remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove_c
subroutine remove_l(list,place)

! ident_21="@(#) M_CLI2 remove_l(3fp) remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_l
subroutine remove_i(list,place)

! ident_22="@(#) M_CLI2 remove_i(3fp) remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace_(3f) - [M_CLI2] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace_(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out) :: place
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_CLI2, only  : insert_, locate_, replace_
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update_dic('b','value of b')
!!     call update_dic('a','value of a')
!!     call update_dic('c','value of c')
!!     call update_dic('c','value of c again')
!!     call update_dic('d','value of d')
!!     call update_dic('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate_key('a',place)
!!     if(place > 0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update_dic(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate_key(key,place)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        call insert_(keywords,key,abs(place))
!!        call insert_(values,val,abs(place))
!!     else ! replace
!!        call replace_(values,val,place)
!!     endif
!!
!!     end subroutine update_dic
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_23="@(#) M_CLI2 replace_c(3fp) replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place < 0.or.place > end)then
           write(warn,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value) <= len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
subroutine replace_l(list,value,place)

! ident_24="@(#) M_CLI2 replace_l(3fp) place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place > 0.and.place <= end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_25="@(#) M_CLI2 replace_i(3fp) place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place > 0.and.place <= end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert_(3f) - [M_CLI2] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert_(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_, insert_
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update_dic(arr,'b')
!!     call update_dic(arr,'[')
!!     call update_dic(arr,'c')
!!     call update_dic(arr,'ZZ')
!!     call update_dic(arr,'ZZZ')
!!     call update_dic(arr,'ZZZZ')
!!     call update_dic(arr,'')
!!     call update_dic(arr,'z')
!!
!!     contains
!!     subroutine update_dic(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate_(arr,string,place)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        call insert_(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update_dic
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_26="@(#) M_CLI2 insert_c(3fp) place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end == 0)then                                        ! empty array
      list=[character(len=ii) :: value ]
   elseif(place == 1)then                                  ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place > end)then                                 ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place >= 2.and.place <= end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(warn,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
subroutine insert_l(list,value,place)

! ident_27="@(#) M_CLI2 insert_l(3fp) place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place == 1)then                                    ! put in front of array
      list=[value, list]
   elseif(place > end)then                                   ! put at end of array
      list=[list, value ]
   elseif(place >= 2.and.place <= end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_28="@(#) M_CLI2 insert_i(3fp) place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place == 1)then                                    ! put in front of array
      list=[value, list]
   elseif(place > end)then                                   ! put at end of array
      list=[list, value ]
   elseif(place >= 2.and.place <= end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine many_args(n0,g0, n1,g1, n2,g2, n3,g3, n4,g4, n5,g5, n6,g6, n7,g7, n8,g8, n9,g9, &
                   & na,ga, nb,gb, nc,gc, nd,gd, ne,ge, nf,gf, ng,gg, nh,gh, ni,gi, nj,gj )

! ident_29="@(#) M_CLI2 many_args(3fp) allow for multiple calls to get_args(3f)"

character(len=*),intent(in)          :: n0, n1
character(len=*),intent(in),optional :: n2, n3, n4, n5, n6, n7, n8, n9, na, nb, nc, nd, ne, nf, ng, nh, ni, nj
class(*),intent(out)           :: g0, g1
class(*),intent(out),optional  :: g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
   call get_generic(n0,g0)
   call get_generic(n1,g1)
   if( present(n2) .and. present(g2) )call get_generic(n2,g2)
   if( present(n3) .and. present(g3) )call get_generic(n3,g3)
   if( present(n4) .and. present(g4) )call get_generic(n4,g4)
   if( present(n5) .and. present(g5) )call get_generic(n5,g5)
   if( present(n6) .and. present(g6) )call get_generic(n6,g6)
   if( present(n7) .and. present(g7) )call get_generic(n7,g7)
   if( present(n8) .and. present(g8) )call get_generic(n8,g8)
   if( present(n9) .and. present(g9) )call get_generic(n9,g9)
   if( present(na) .and. present(ga) )call get_generic(na,ga)
   if( present(nb) .and. present(gb) )call get_generic(nb,gb)
   if( present(nc) .and. present(gc) )call get_generic(nc,gc)
   if( present(nd) .and. present(gd) )call get_generic(nd,gd)
   if( present(ne) .and. present(ge) )call get_generic(ne,ge)
   if( present(nf) .and. present(gf) )call get_generic(nf,gf)
   if( present(ng) .and. present(gg) )call get_generic(ng,gg)
   if( present(nh) .and. present(gh) )call get_generic(nh,gh)
   if( present(ni) .and. present(gi) )call get_generic(ni,gi)
   if( present(nj) .and. present(gj) )call get_generic(nj,gj)
contains
!===================================================================================================================================
function c(generic)
class(*),intent(in) :: generic
character(len=:),allocatable :: c
   select type(generic)
      type is (character(len=*)); c=trim(generic)
      class default
         c='unknown'
         stop 'get_many:: parameter name is not character'
   end select
end function c
!===================================================================================================================================
subroutine get_generic(name,generic)
use,intrinsic :: iso_fortran_env, only : real64
character(len=*),intent(in)  :: name
class(*),intent(out)         :: generic
   select type(generic)
      type is (integer);                        call get_args(name,generic)
      type is (real);                           call get_args(name,generic)
      type is (real(kind=real64));              call get_args(name,generic)
      type is (logical);                        call get_args(name,generic)
      !x!type is (character(len=:),allocatable ::);   call get_args(name,generic)
      type is (character(len=*));
      call get_args_fixed_length(name,generic)
      type is (complex);                        call get_args(name,generic)
      class default
         stop 'unknown type in *get_generic*'
   end select
end subroutine get_generic
!===================================================================================================================================
end subroutine many_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function iget(n); integer                      :: iget; character(len=*),intent(in) :: n; call get_args(n,iget); end function iget
function rget(n); real                         :: rget; character(len=*),intent(in) :: n; call get_args(n,rget); end function rget
function dget(n); real(kind=dp)                :: dget; character(len=*),intent(in) :: n; call get_args(n,dget); end function dget
function sget(n); character(len=:),allocatable :: sget; character(len=*),intent(in) :: n; call get_args(n,sget); end function sget
function cget(n); complex                      :: cget; character(len=*),intent(in) :: n; call get_args(n,cget); end function cget
function lget(n); logical                      :: lget; character(len=*),intent(in) :: n; call get_args(n,lget); end function lget

function igs(n); integer,allocatable          :: igs(:); character(len=*),intent(in) :: n; call get_args(n,igs); end function igs
function rgs(n); real,allocatable             :: rgs(:); character(len=*),intent(in) :: n; call get_args(n,rgs); end function rgs
function dgs(n); real(kind=dp),allocatable    :: dgs(:); character(len=*),intent(in) :: n; call get_args(n,dgs); end function dgs
function sgs(n,delims)
character(len=:),allocatable         :: sgs(:)
character(len=*),optional,intent(in) :: delims
character(len=*),intent(in)          :: n
   call get_args(n,sgs,delims)
end function sgs
function cgs(n); complex,allocatable          :: cgs(:); character(len=*),intent(in) :: n; call get_args(n,cgs); end function cgs
function lgs(n); logical,allocatable          :: lgs(:); character(len=*),intent(in) :: n; call get_args(n,lgs); end function lgs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function ig()
integer,allocatable :: ig(:)
integer             :: i, ierr
   if(allocated(ig))deallocate(ig)
   allocate(ig(size(unnamed)))
   do i=1,size(ig)
      call a2i(unnamed(i),ig(i),ierr)
   enddo
end function ig
!===================================================================================================================================
function rg()
real,allocatable :: rg(:)
   rg=real(dg())
end function rg
!===================================================================================================================================
function dg()
real(kind=dp),allocatable :: dg(:)
integer                   :: i
integer                   :: ierr
   if(allocated(dg))deallocate(dg)
   allocate(dg(size(unnamed)))
   do i=1,size(dg)
      call a2d(unnamed(i),dg(i),ierr)
   enddo
end function dg
!===================================================================================================================================
function lg()
logical,allocatable   :: lg(:)
integer               :: i
integer               :: iichar
character,allocatable :: hold
   if(allocated(lg))deallocate(lg)
   allocate(lg(size(unnamed)))
   do i=1,size(lg)
      hold=trim(upper(adjustl(unnamed(i))))
      if(hold(1:1) == '.')then                 ! looking for fortran logical syntax .STRING.
         iichar=2
      else
         iichar=1
      endif
      select case(hold(iichar:iichar))         ! check word to see if true or false
      case('T','Y',' '); lg(i)=.true.          ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
      case('F','N');     lg(i)=.false.         ! assume this is false or no
      case default
         call journal("*lg* bad logical expression for element",i,'=',hold)
      end select
   enddo
end function lg
!===================================================================================================================================
function cg()
complex,allocatable :: cg(:)
integer             :: i, ierr
real(kind=dp)       :: rc, ic
   if(allocated(cg))deallocate(cg)
   allocate(cg(size(unnamed)))
   do i=1,size(cg),2
      call a2d(unnamed(i),rc,ierr)
      call a2d(unnamed(i+1),ic,ierr)
      cg(i)=cmplx(rc,ic,kind=sp)
   enddo
end function cg
!===================================================================================================================================
! Does not work with gcc 5.3
!function sg()
!character(len=:),allocatable :: sg(:)
!   sg=unnamed
!end function sg

!===================================================================================================================================
function sg()
character(len=:),allocatable :: sg(:)
   if(allocated(sg))deallocate(sg)
   allocate(sg,source=unnamed)
end function sg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mystop(sig,msg)
! negative signal means always stop program
! else do not stop and set G_STOP_MESSAGE if G_QUIET is true
! or
! print message and stop if G_QUIET is false
! the MSG is NOT for displaying except for internal errors when the program will be stopped.
! It is for returning a value when the stop is being ignored
!
integer,intent(in) :: sig
character(len=*),intent(in),optional :: msg
   if(sig < 0)then
      if(present(msg))call journal(msg)
      stop 1
   elseif(.not.G_QUIET)then
      stop
   else
      if(present(msg)) then
         G_STOP_MESSAGE=msg
      else
         G_STOP_MESSAGE=''
      endif
      G_STOP=sig
   endif
end subroutine mystop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_30="@(#) M_strings atleast(3f) return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine locate_key(value,place)

! ident_31="@(#) M_CLI2 locate_key(3f) find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
integer                                 :: ii
character(len=:),allocatable            :: value_local

   if(G_UNDERDASH)then
      value_local=trim(replace_str(value,'-','_'))
   else
      value_local=trim(value)
   endif
   if(G_NOSEPARATOR)then
      value_local=replace_str(value_local,'-','')
      value_local=replace_str(value_local,'_','')
   endif

   if(G_IGNORECASE.and.len_trim(value_local) > 1)value_local=lower(value_local)

   if(len(value_local) == 1)then
      !x!ii=findloc(shorts,value_local,dim=1)
      ii=maxloc([0,merge(1, 0, shorts == value_local)],dim=1)
      if(ii > 1)then
         place=ii-1
      else
         call locate_(keywords,value_local,place)
      endif
   else
      call locate_(keywords,value_local,place)
   endif
end subroutine locate_key
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_mode(3f) - [ARGUMENTS:M_CLI2] turn on optional modes
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine set_mode(key,mode)
!!
!!     character(len=*),intent(in) :: key
!!     logical,intent(in),optional :: mode
!!
!!##DESCRIPTION
!!     Allow optional behaviors.
!!
!!##OPTIONS
!!    KEY    name of option
!!
!!    The following values are allowed:
!!
!!    o  response_file - enable use of response file
!!
!!    o  ignorecase - ignore case in long key names. So the user
!!       does not have to remember if the option is --IgnoreCase
!!       or --ignorecase or --ignoreCase
!!
!!    o  underdash  - treat dash in keyword as an underscore.
!!       So the user should not have to remember if the option is
!!       --ignore_case or --ignore-case.
!!
!!    o  strict - allow Boolean keys to be bundled, but requires
!!       a single dash prefix be used for short key names and
!!       long names must be prefixed with two dashes.
!!
!!    o  lastonly  - when multiple keywords occur keep the rightmost
!!       value specified instead of appending the values together.
!!
!!    MODE   set to .true. to activate the optional mode.
!!           Set to .false. to deactivate the mode.
!!           It is .true. by default.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_set_mode
!!    use M_CLI2,  only : set_args, lget, set_mode
!!    implicit none
!!    character(len=*),parameter :: all='(*(g0))'
!!       !
!!       ! enable use of response files
!!       call set_mode('response_file')
!!       !
!!       ! Any dash in a keyword is treated as an underscore
!!       call set_mode('underdash')
!!       !
!!       ! The case of long keywords are ignored.
!!       ! Values and short names remain case-sensitive
!!       call set_mode('ignorecase')
!!       !
!!       ! short single-character boolean keys may be bundled
!!       ! but it is required that a single dash is used for
!!       ! short keys and a double dash for long keywords.
!!       call set_mode('strict')
!!       !
!!       call set_args(' --switch_X:X F --switch-Y:Y F --ox:O F -t F -x F -o F')
!!       !
!!       print all,'--switch_X or -X ... ',lget('switch_X')
!!       print all,'--switch_Y or -Y ... ',lget('switch_Y')
!!       print all,'--ox or -O       ... ',lget('ox')
!!       print all,'-o               ... ',lget('o')
!!       print all,'-x               ... ',lget('x')
!!       print all,'-t               ... ',lget('t')
!!    end program demo_set_mode
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
elemental impure subroutine set_mode(key,mode)
character(len=*),intent(in) :: key
logical,intent(in),optional :: mode
logical :: local_mode

   if(present(mode))then
      local_mode=mode
   else
      local_mode=.true.
   endif

   select case(lower(key))
   case('response_file','response file'); CLI_RESPONSE_FILE=local_mode
   case('debug');                         G_DEBUG=local_mode
   case('ignorecase');                    G_IGNORECASE=local_mode
   case('underdash');                     G_UNDERDASH=local_mode
   case('noseparator');                   G_NOSEPARATOR=local_mode
   case('strict');                        G_STRICT=local_mode
   case('lastonly');                      G_APPEND=.not.local_mode
   case default
      call journal('*set_mode* unknown key name ',key)
   end select

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'

end subroutine set_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_CLI2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
 
!>>>>> build/dependencies/M_io/src/M_io.F90
!===================================================================================================================================
MODULE M_io
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
private
integer,parameter,private:: sp=kind(1.0), dp=kind(1.0d0)
public uniq
public print_inquire
public notopen
public filename_generator
public number_of_lines
public get_next_char
public dirname
public basename
public splitpath
public joinpath
public fileopen
public filebyte, slurp
public fileread, gulp, swallow
public filewrite
public fileclose
public filedelete
public get_tmp
public read_line
public getline
public read_table
public rd
public separator
public lookfor
public which
public get_env
public getname

! ident_1="@(#) M_io rd(3f) ask for string or number from standard input with user-definable prompt"
interface rd
   module procedure rd_character
   module procedure rd_integer
   module procedure rd_real
   module procedure rd_doubleprecision
   module procedure rd_logical
end interface

! ident_2="@(#) M_io read_table(3f) read file containing a table of numeric values"
interface read_table
   module procedure read_table_i
   module procedure read_table_r
   module procedure read_table_d
end interface

interface filedelete
   module procedure filedelete_filename
   module procedure filedelete_lun
end interface

integer,save,private       :: my_stdout=stdout
logical,save               :: debug=.false.
integer,save               :: last_int=0

interface string_to_value
   module procedure a2d, a2i
end interface

interface v2s
   module procedure i2s
end interface

interface journal
   !!module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   !!module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

interface str
   module procedure msg_scalar, msg_one
end interface str
!-----------------------------------
! old names
interface swallow;  module procedure fileread;  end interface
interface gulp;     module procedure fileread;  end interface
interface slurp;    module procedure filebyte;  end interface
!-----------------------------------
character(len=*),parameter,private :: gen='(*(g0,1x))'

CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      uniq(3f) - [M_io:QUERY] append a number to the end of filename to make
!!                 a unique name if name exists
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      Usage
!!
!!       character(len=:),allocatable function uniq(name,istart,verbose,create)
!!       character(len=*),intent(in) :: name
!!       integer,intent(in),optional :: istart
!!       logical,intent(in),optional :: verbose
!!       logical,intent(in),optional :: create
!!
!!##DESCRIPTION
!!    Given a filename test if it is in use or exists. If it is, or if it
!!    ends in a period add a number to the end of the name and
!!    test if the new name exists. If necessary, increment the number and
!!    try again up to the value 9999999. By default an empty file is created
!!    if an unused name is found.
!!
!!
!!##OPTIONS
!!    name     base input name used to create output filename
!!             If name ends in "." a numeric suffix is always added.
!!    istart   number to start with as a suffix. Default is 1. Must be a
!!             positive integer less than 9999999.
!!    verbose  writes extra messages to stdout. Defaults to .false.
!!    create   create file if a new unused name is successfully
!!             found. Defaults to .true. .
!!
!!##RETURNS
!!    uniq     A unique filename that is the same as the NAME input parameter
!!             except with a number appended at the end if needed. If could
!!             not find a unique name a blank is returned.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_uniq
!!       use M_io, only : uniq
!!       implicit none
!!       character(len=4096) :: myname
!!       integer             :: i
!!          myname=uniq('does_not_exist')
!!          write(*,*)'name stays the same   :',trim(myname)
!!          open(unit=10,file='does_exist')
!!          myname=uniq('does_exist')
!!          write(*,*)'name has suffix added :',trim(myname)
!!          do i=1,10
!!             myname=uniq('does_exist')
!!             write(*,*) 'FILENAME:',trim(myname)
!!             open(unit=20+i,file=myname)
!!          enddo
!!       end program demo_uniq
!!
!!    Expected output
!!
!!     name stays the same does_not_exist
!!     name has suffix added does_exist0001
!!     FILENAME:does_exist0002
!!     FILENAME:does_exist0003
!!     FILENAME:does_exist0004
!!     FILENAME:does_exist0005
!!     FILENAME:does_exist0006
!!     FILENAME:does_exist0007
!!     FILENAME:does_exist0008
!!     FILENAME:does_exist0009
!!     FILENAME:does_exist0010
!!     FILENAME:does_exist0011
!!
!!##AUTHOR
!!    John S. Urban, 1993
!!##LICENSE
!! Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
function uniq(name,istart,verbose,create)
implicit none

! ident_3="@(#) M_io uniq(3f) append a number to the end of filename to make a unique name if name exists"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: name
character(len=:),allocatable :: uniq
integer,intent(in),optional  :: istart
logical,intent(in),optional  :: verbose
logical,intent(in),optional  :: create
!-----------------------------------------------------------------------------------------------------------------------------------
logical                     :: around
integer,save                :: icount=1           ! counter to generate suffix from
character(len=4096),save    :: lastname=' '       ! name called with last time the routine was called
integer                     :: ilen
integer                     :: itimes
integer                     :: iscr
integer                     :: ios
logical                     :: verbose_local
logical                     :: create_local
!-----------------------------------------------------------------------------------------------------------------------------------
   uniq=trim(name)                                   ! the input name will be returned if it passes all the tests
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname /= name)then                          ! if a different input name than last time called reset icount
      lastname=name                                  ! a new name to keep for subsequent calls
      icount=1                                       ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(create))then
      create_local=create
   else
      create_local=.true.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(istart))then
      icount=istart                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                               ! find last non-blank character in file name
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen /= 0)then                                 ! a blank input name so name will just be a suffix
      if(name(ilen:ilen) /= '.')then                 ! always append a number to a file ending in .
         inquire(file=name(:ilen),exist=around)      ! check filename as-is
         if(.not.around)then                         ! file name does not exist, can use it as-is
            uniq=trim(name)
            if(create_local)then
               open(newunit=iscr,file=uniq,iostat=ios,status='new')
               close(unit=iscr,iostat=ios)
            endif
            return
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   itimes=0                                           ! count number of times tried to get a uniq name
   deallocate(uniq)
   allocate(character(len=ilen+8) :: uniq)            ! make it useable with an internal WRITE(3f) with room for a numeric suffix
   uniq(:)=name
   INFINITE: do                                       ! top of loop trying for a unique name
      if(itimes >= 9999999)then                       ! if too many tries to be reasonable give up
         call journal('sc','*uniq* unable to find a unique filename. Too many tries')
         uniq=''
         return
      endif
      if(icount > 9999999) icount=1                  ! reset ICOUNT when it hits arbitrary maximum value
      if(icount <= 9999)then
         write(uniq(ilen+1:),'(i4.4)')icount          ! create name by adding a numeric string to end
      else
         write(uniq(ilen+1:),'(i7.7)')icount          ! create name by adding a numeric string to end
      endif
      icount=icount+1                                 ! increment counter used to come up with suffix
      inquire(file=uniq,exist=around)                 ! see if this filename already exists
      if(.not.around)then                             ! found an unused name
         if(verbose_local)then
            call journal('c',trim('*uniq* name='//trim(uniq))) ! write out message reporting name used
         endif
         if(create_local)then
            open(newunit=iscr,file=uniq,iostat=ios,status='new')
            close(unit=iscr,iostat=ios)
         endif
         uniq=trim(uniq)
         return                                       ! return successfully
      endif
      itimes=itimes+1                                 ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_inquire(3f) - [M_io:QUERY] Do INQUIRE on file by name/number and
!!                        print results
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   Definition:
!!
!!    subroutine print_inquire(lun)
!!      or
!!    subroutine print_inquire(name)
!!    integer,intent(in),optional          :: lun
!!    character(len=*),intent(in),optional :: name
!!
!!##DESCRIPTION
!!    Given either a Fortran file-unit-number or filename, call the
!!    INQUIRE(3f) intrinsic and print typical status information.
!!
!!##OPTIONS
!!    lun    if lun is not equal to -1 then query by number and ignore
!!           filename even if present
!!    name   if lun = -1  or is not present then query by this filename
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_print_inquire
!!    use M_io, only : print_inquire, fileopen
!!    implicit none
!!    character(len=4096)  :: filename
!!    character(len=20)    :: mode
!!    integer              :: ios
!!    character(len=256)   :: message
!!    integer              :: lun
!!       do
!!          write(*,'(a)',advance='no')'enter filename>'
!!          read(*,'(a)',iostat=ios)filename
!!          if(ios /= 0)exit
!!          write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]>'
!!          read(*,'(a)',iostat=ios)mode
!!          if(ios /= 0)exit
!!          lun=fileopen(filename,mode,ios)
!!          if(ios == 0)then
!!             write(*,*)'OPENED'
!!          else
!!             write(*,*)'ERROR: IOS=',ios
!!          endif
!!          if(lun /= -1)then
!!             call print_inquire(lun,'')
!!             close(lun,iostat=ios,iomsg=message)
!!             if(ios /= 0)then
!!                write(*,'(a)')trim(message)
!!             endif
!!          endif
!!       enddo
!!    end program demo_print_inquire
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine print_inquire(lun_in,namein_in) ! Version: JSU-1997-12-31, 2020-01-11

! ident_4="@(#) M_io print_inquire(3f) Do INQUIRE on file by name/number and print results"

integer,intent(in),optional             :: lun_in        ! if unit >= 0 then query by unit number, else by name
character(len=*),intent(in),optional    :: namein_in
integer                        :: ios
character(len=256)             :: message
character(len=:),allocatable   :: namein
integer                        :: lun
!==============================================================================================
!  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
!  ACTION    =  READ        | WRITE         |  READWRITE
!  FORM      =  FORMATTED   |  UNFORMATTED
!  POSITION  =  ASIS        |  REWIND       |  APPEND
!  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
character(len=20)             :: access         ; namelist/inquire/access
character(len=20)             :: asynchronous   ; namelist/inquire/asynchronous
character(len=20)             :: blank          ; namelist/inquire/blank
character(len=20)             :: decimal        ; namelist/inquire/decimal
character(len=20)             :: delim          ; namelist/inquire/delim
character(len=20)             :: direct         ; namelist/inquire/direct
character(len=20)             :: encoding       ; namelist/inquire/encoding
logical                       :: exist          ; namelist/inquire/exist

character(len=20)             :: form           ; namelist/inquire/form
character(len=20)             :: formatted      ; namelist/inquire/formatted
character(len=20)             :: unformatted    ; namelist/inquire/unformatted

integer                       :: id             ; namelist/inquire/id
character(len=20)             :: name           ; namelist/inquire/name
logical                       :: named          ; namelist/inquire/named
integer                       :: nextrec        ; namelist/inquire/nextrec
integer                       :: number         ; namelist/inquire/number
logical                       :: opened         ; namelist/inquire/opened
character(len=20)             :: pad            ; namelist/inquire/pad
logical                       :: pending        ; namelist/inquire/pending
integer                       :: pos            ; namelist/inquire/pos
character(len=20)             :: position       ; namelist/inquire/position

character(len=20)             :: action         ; namelist/inquire/action
character(len=20)             :: read           ; namelist/inquire/read
character(len=20)             :: readwrite      ; namelist/inquire/readwrite
character(len=20)             :: write          ; namelist/inquire/write

integer                       :: recl           ; namelist/inquire/recl
character(len=20)             :: round          ; namelist/inquire/round
character(len=20)             :: sequential     ; namelist/inquire/sequential
character(len=20)             :: sign           ; namelist/inquire/sign
integer                       :: size           ; namelist/inquire/size
character(len=20)             :: stream         ; namelist/inquire/stream
!==============================================================================================
   namein=merge_str(namein_in,'',present(namein_in))
   lun=merge(lun_in,-1,present(lun_in))
   ! exist, opened, and named always become defined unless an error condition occurs.
   !!write(*,*)'LUN=',lun,' FILENAME=',namein
   !-----------------------------------------------------------------------------------------------------------------------------------
   name=''
   if(namein == ''.and.lun /= -1)then
         call journal('sc','*print_inquire* checking unit',lun)
         inquire(unit=lun,                                                                               &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     &   sign=sign,                                                                                      &
     &   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
    elseif(namein /= '')then
         call journal('sc','*print_inquire* checking file:'//namein)
         inquire(file=namein,                                                                            &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     &   sign=sign,                                                                                      &
     &   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
     if(name == '')name=namein
    else
       call journal('sc','*print_inquire* must specify either filename or unit number')
    endif
!-----------------------------------------------------------------------------------------------------------------------------------
   write(*,nml=inquire,delim='none')
   return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
   call journal('sc','*print_inquire* bad inquire')
!  If an error condition occurs during execution of an INQUIRE  statement,
!  all of the inquiry identifiers except ios become undefined.
   call journal('sc','*print_inquire* inquire call failed,iostat=',ios,'message=',message)
end subroutine print_inquire
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    separator(3f) - [M_io:QUERY] try to determine pathname directory
!!                    separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!
!!    Try to determine the separator character used to separate directory
!!    names from file basenames. It is assumed it is either a backslash or
!!    a slash character.
!!
!!    First, the environment variables PATH, HOME, PWD, and  SHELL  are
!!    examined for a backslash, then a slash.
!!
!!    Then, using the name the program was invoked with, then an INQUIRE(3f)
!!    of that name, then ".\NAME" and "./NAME" try to find an expected
!!    separator character.
!!
!!    Can be very system dependent. If the queries fail the default returned
!!    is "/".
!!
!!    The value is cached as a return value for subsequent calls.
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function separator() result(sep)

! use the pathname returned as arg0 to determine pathname separator
implicit none
integer                      :: ios
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!*!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !*!FORT BUG:if(sep_cache /= ' ')then  ! use cached value.
    !*!FORT BUG:    sep=sep_cache
    !*!FORT BUG:    return
    !*!FORT BUG:endif
    if(isep /= -1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames to do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios == 0)then
        sep='/'
        exit FOUND
    endif
    ! check variables names common to many platforms that usually have a
    ! directory path in them although a ULS file can contain a backslash
    ! and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using
    ! POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME']
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),'\') /= 0)then
          sep='\'
          exit FOUND
       elseif(index(get_env(envnames(i)),'/') /= 0)then
          sep='/'
          exit FOUND
       endif
    enddo

    write(*,*)'<WARNING>unknown system directory path separator'
    sep='\'
    endblock FOUND
    !*!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    read_table(3f) - [M_io:READ] read file containing a table of numeric values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine read_table(filename,array,ierr,comment)
!!
!!    character(len=*),intent(in)          :: filename
!!    TYPE,allocatable,intent(out)         :: array(:,:)
!!    integer,intent(out)                  :: ierr
!!    character(len=1,intent(in),optional  :: comment
!!
!!   where TYPE may be REAL, INTEGER, or DOUBLEPRECISION
!!
!!##DESCRIPTION
!!    Read a table from a file that is assumed to be columns of numbers,
!!    ignoring characters not in the set [0-9edED+-.] and requiring each
!!    row contain the same number of values.
!!
!!    The input file is assumed to be of a small enough size that it can
!!    be copied into memory.
!!
!!##OPTIONS
!!    filename   filename to read
!!    array      array to create. May be INTEGER, REAL, or DOUBLEPRECISION
!!    ierr       zero if no error occurred.
!!    comment    ignore lines which contain this as the first non-blank
!!               character. Ignore it and subsequent characters on any line.
!!##EXAMPLES
!!
!!    Sample program, assuming the input file "inputfile" exists:
!!
!!     program demo_read_table
!!     use M_io, only : read_table
!!     implicit none
!!     doubleprecision,allocatable :: array(:,:)
!!     integer :: i, ierr
!!
!!     ! create test file
!!     open(file='inputfile',unit=10,action='write')
!!     write(10,'(a)') [character(len=80):: &
!!      ' ___.___.___                           ', &
!!      '| 1 | 5 | 3 |                          ', &
!!      '|---+---+---|                          ', &
!!      '| 4 | 2 | 6 |                          ', &
!!      ' -----------                           ', &
!!      '    #-----#-----#------#               ', &
!!      '|   | 1   | 3e2 | 4    |               ', &
!!      '|   #-----#-----#------#               ', &
!!      '|   | 2.0 | -5  | +2.2 |               ', &
!!      '    #-----#-----#------#               ', &
!!      '                                       ', &
!!      '#___#___#___#                          ', &
!!      '| 1 | 5 | 3 |                          ', &
!!      '#---#---#---#                          ', &
!!      '| 4 | 2 | 6 |                          ', &
!!      '#---#---#---#                          ', &
!!      '                                       ', &
!!      '1;10;45                                ', &
!!      '10, ,, ,,20    45                      ', &
!!      '  2 20  15                             ', &
!!      ' big=20.345 medium=20  small=15        ', &
!!      '                                       ', &
!!      '30 30e3   0                            ', &
!!      '  4 300.444e-1 -10                     ', &
!!      '40 30.5555d0 -10                       ', &
!!      '  4 300.444E-1 -10                     ', &
!!      '40 30.5555D0 -10                       ', &
!!      '                                       ']
!!     close(unit=10)
!!
!!     ! read file as a table
!!     call read_table('inputfile',array,ierr)
!!
!!     ! print values
!!     write(*,*)'size=       ',size(array)
!!     write(*,*)'size(dim=1)=',size(array,dim=1)
!!     write(*,*)'size=(dim=2)',size(array,dim=2)
!!     do i=1,size(array,dim=1)
!!        write(*,*)array(i,:)
!!     enddo
!!
!!     ! remove sample file
!!     open(file='inputfile',unit=10)
!!     close(unit=10,status='delete')
!!
!!     end program demo_read_table
!!
!!   Results:
!!
!!     size=                 45
!!     size(dim=1)=          15
!!     size=(dim=2)           3
!!       1.000000000000000      5.000000000000000      3.000000000000000
!!       4.000000000000000      2.000000000000000      6.000000000000000
!!       1.000000000000000      300.0000000000000      4.000000000000000
!!       2.000000000000000     -5.000000000000000      2.200000000000000
!!       1.000000000000000      5.000000000000000      3.000000000000000
!!       4.000000000000000      2.000000000000000      6.000000000000000
!!       1.000000000000000      10.00000000000000      45.00000000000000
!!       10.00000000000000      20.00000000000000      45.00000000000000
!!       2.000000000000000      20.00000000000000      15.00000000000000
!!       20.34499999999999      20.00000000000000      15.00000000000000
!!       30.00000000000000      30000.00000000000      0.000000000000000
!!       4.000000000000000      30.04440000000000     -10.00000000000000
!!       40.00000000000000      30.55549999999999     -10.00000000000000
!!       4.000000000000000      30.04440000000000     -10.00000000000000
!!       40.00000000000000      30.55549999999999     -10.00000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine read_table_d(filename,darray,ierr,comment)
! note the array is allocated as text, and then doubleprecision, and then placed in the output array.
! for large files it would be worth it to just determine the file size and allocate and fill the output
! array

character(len=*),intent(in)             :: FILENAME
doubleprecision,allocatable,intent(out) :: darray(:,:)
integer,intent(out)                     :: ierr
character(len=1),intent(in),optional    :: comment
character(len=:),allocatable :: page(:) ! array to hold file in memory
integer                      :: irows,irowsmax
integer                      :: icols
integer                      :: i
doubleprecision,allocatable  :: dline(:)
   ierr=0
   ! allocate character array and copy file into it
   call fileread(FILENAME,page)
   if(.not.allocated(page))then
      write(*,*)'*demo_read_table* failed to load file '//FILENAME
      if(allocated(darray))deallocate(darray)
      allocate(darray(0,0))
      ierr=-1
   else
      call cleanse()
      if(allocated(darray))deallocate(darray)
      if(size(page,dim=1) == 0)then
         allocate(darray(0,0))
      else
         irowsmax=size(page,dim=1)
         icols=size(s2vs(page(1)))
         allocate(darray(irowsmax,icols))
         darray=0.0d0
         irows=0
         do i=1,irowsmax
            dline=s2vs(page(i))
            irows=irows+1
            if(size(dline) /= icols)then
               write(*,gen)page(i),'does not contain',icols,'values'
               ierr=ierr+1
               darray(irows,:min(size(dline),icols))=dline(min(size(dline),icols))
            else
               darray(irows,:)=dline
            endif
         enddo
         if(irows /= irowsmax)then
            darray=darray(:irows,:icols)
         endif
         deallocate(page)  ! release memory
      endif
   endif
contains
    subroutine cleanse()
    integer :: i,j,k
    integer :: ios
    integer :: ikeep
    character(len=:),allocatable :: words(:), line
    doubleprecision :: value
    ikeep=0
    do i=1,size(page,dim=1)
       ! do this more rigourously
       ! [+-]NNNNNN[.NNNN][ED][+-]NN
       line=''
       ! get rid of all characters not in a number and
       ! then split the remaining line and keep only
       ! tokens that can be read as a number
       do j=1,len(page)
          if(present(comment))then
             if(page(i)(j:j) == comment)then
                page(i)(j:)=' '
                exit
             endif
          endif
          select case(page(i)(j:j))
          case('e','E','d','D','+','-','.','0':'9')
          case default
             page(i)(j:j)=' '
          end select
       enddo
       call split(page(i),words)
       do k=1,size(words)
          read(words(k),*,iostat=ios)value
          if(ios == 0)then
             line=line//crop(words(k))//' '
          endif
       enddo
       if(line /= '')then
          ikeep=ikeep+1
          page(ikeep)(:)=line
       endif
    enddo
    page=page(:ikeep)
    end subroutine cleanse
end subroutine read_table_d
!===================================================================================================================================
subroutine read_table_i(filename,array,ierr,comment)
implicit none
character(len=*),intent(in)             :: FILENAME
integer,allocatable,intent(out)         :: array(:,:)
integer,intent(out)                     :: ierr
character(len=1),intent(in),optional    :: comment
doubleprecision,allocatable             :: darray(:,:)
call read_table_d(filename,darray,ierr,comment)
array=nint(darray)
end subroutine read_table_i
!===================================================================================================================================
subroutine read_table_r(filename,array,ierr,comment)
implicit none
character(len=*),intent(in)             :: FILENAME
real,allocatable,intent(out)            :: array(:,:)
integer,intent(out)                     :: ierr
character(len=1),intent(in),optional    :: comment
doubleprecision,allocatable             :: darray(:,:)
call read_table_d(filename,darray,ierr,comment)
array=real(darray)
end subroutine read_table_r
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileread(3f) - [M_io:READ] read a file into a string array
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine fileread(filename,pageout)
!!
!!    character(len=*),intent(in) :: filename
!!      or
!!    integer,intent(in)          :: io
!!
!!    character(len=:),allocatable,intent(out) :: pageout(:)
!!##DESCRIPTION
!!    Read an entire file into memory as a character array, one character
!!    variable per line.
!!
!!    NOTE:
!!
!!    Do not casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!    filename   filename to read into memory, or LUN (Fortran Logical
!!               Unit Number).  If filename is a LUN, file must be opened
!!               with
!!
!!                  form='unformatted',access='stream'
!!
!!               as in
!!
!!                 open(unit=igetunit, file=filename,     &
!!                 & action="read", iomsg=message,        &
!!                 & form="unformatted", access="stream", &
!!                 & status='old',iostat=ios)
!!
!!    pageout    array of characters to hold file
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_fileread
!!    use M_io,      only : fileread
!!    implicit none
!!    character(len=4096)          :: FILENAME   ! file to read
!!    character(len=:),allocatable :: pageout(:) ! array to hold file in memory
!!    integer                      :: longest, lines, i
!!    character(len=*),parameter   :: gen='(*(g0,1x))'
!!       ! get a filename
!!       call get_command_argument(1, FILENAME)
!!       ! allocate character array and copy file into it
!!       call fileread(FILENAME,pageout)
!!       if(.not.allocated(pageout))then
!!          write(*,gen)'*demo_fileread* failed to load file',FILENAME
!!       else
!!          ! write file from last line to first line
!!          longest=len(pageout)
!!          lines=size(pageout)
!!          write(*,gen)'number of lines is',lines
!!          write(*,gen)'and length of lines is',longest
!!          write(*,'(a)')repeat('%',longest+2)
!!          write(*,'("%",a,"%")')(trim(pageout(i)),i=lines,1,-1)
!!          write(*,'(a)')repeat('%',longest+2)
!!          deallocate(pageout)  ! release memory
!!       endif
!!    end program demo_fileread
!!
!!   Given
!!
!!    first line
!!    second line
!!    third line
!!
!!   Expected output
!!
!!    >  number of lines is 3
!!    >  and length of lines is 11
!!    > %%%%%%%%%%%%%
!!    > %third line %
!!    > %second line%
!!    > %first line %
!!    > %%%%%%%%%%%%%
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine fileread(FILENAME,pageout)
implicit none
class(*),intent(in)                      :: FILENAME   ! file to read
character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory

   call filebyte(FILENAME,text) ! allocate character array and copy file into it

   if(.not.allocated(text))then
      select type(FILENAME)
       type is (character(len=*)); write(*,*)'*fileread* failed to load file '//FILENAME
       type is (integer);          write(*,'(a,i0)')'*fileread* failed to load file unit ',FILENAME
      end select
   else  ! convert array of characters to array of lines
      pageout=page(text)
      deallocate(text)     ! release memory
   endif

contains
function page(array)  result (table)

! ident_5="@(#) page(3fp) function to copy char array to page of text"

character(len=1),intent(in)  :: array(:)
character(len=:),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
!!character(len=1),parameter   :: nl=new_line('A')
character(len=1),parameter   :: nl = char(10)
character(len=1),parameter   :: cr = char(13)
   lines = 0
   linelength = 0
   length = 0
   sz=size(array)
   do i = 1,sz
      if( array(i) == nl )then
         linelength = max(linelength,length)
         lines = lines + 1
         length = 0
      else
         length = length + 1
      endif
   enddo
   if( sz > 0 )then
      if( array(sz) /= nl )then
         lines = lines+1
      endif
   endif

   if(allocated(table))deallocate(table)
   allocate(character(len=linelength) :: table(lines))
   table(:) = ' '

   linecount = 1
   position = 1
   do i = 1,sz
      if( array(i) == nl )then
         linecount=linecount+1
         position=1
      elseif( array(i) == cr )then
      elseif( linelength /= 0 )then
         table(linecount)(position:position) = array(i)
         position = position+1
      endif
   enddo
end function page
end subroutine fileread
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filebyte(3f) - [M_io:READ] read a file into a character array
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine filebyte(filename,text,length.lines)
!!
!!    character(len=*),intent(in) :: filename
!!     or
!!    integer,intent(in)          :: filenumber
!!
!!    character(len=1),allocatable,intent(out) :: text(:)
!!    integer,intent(out),optional :: length
!!    integer,intent(out),optional :: lines
!!##DESCRIPTION
!!    Read an entire file as a stream into memory as an array of single
!!    characters, retaining line end terminators.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory or LUN (Fortran Logical
!!                  Unit Number) If a LUN, file must be opened with
!!
!!                     form='unformatted',access='stream'
!!
!!                  as in
!!
!!                    open(unit=igetunit, file=filename,     &
!!                    & action="read", iomsg=message,        &
!!                    & form="unformatted", access="stream", &
!!                    & status='old',iostat=ios)
!!
!!       text       array of characters to hold file
!!       length     returns length of longest line read(Optional).
!!       lines      returns number of lines read(Optional).
!!
!!##EXAMPLES
!!
!!    Sample program, which  creates test input file "inputfile":
!!
!!     program demo_filebyte
!!     use M_io, only      : filebyte
!!     implicit none
!!     character(len=1),allocatable :: text(:) ! array to hold file in memory
!!     character(len=*),parameter :: FILENAME='inputfile' ! file to read
!!
!!     ! create test file
!!     open(file=FILENAME,unit=10,action='write')
!!     write(10,'(a)') new_line('A')//'esrever lliw'
!!     write(10,'(a)') 'margorp elpmas eht taht'
!!     write(10,'(a)') 'elif elpmas a si sihT'
!!     close(unit=10)
!!
!!     call filebyte(FILENAME,text) ! allocate character array and copy file into it
!!
!!     if(.not.allocated(text))then
!!        write(*,*)'*rever* failed to load file '//FILENAME
!!     else
!!        ! write file reversed to stdout
!!        write(*,'(*(a:))',advance='no')text(size(text):1:-1)
!!        deallocate(text)  ! release memory
!!     endif
!!
!!     end program demo_filebyte
!!
!!    Expected output:
!!
!!     >This is a sample file
!!     >that the sample program
!!     >will reverse
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine filebyte(filename,text,length,lines)
implicit none

! ident_6="@(#) M_io filebyte(3f) allocate text array and read file filename into it"

class(*),intent(in)                      :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer,intent(out),optional             :: length      ! length of longest line
integer,intent(out),optional             :: lines       ! number of lines
integer :: nchars=0             ! holds size of file
integer :: igetunit             ! use newunit=igetunit in f08
integer :: ios=0                ! used for I/O error status
integer :: length_local
integer :: lines_local
integer :: i
integer :: icount
character(len=256)  :: message
character(len=4096) :: label
character(len=:),allocatable :: line
   length_local=0
   lines_local=0
   message=''
   select type(FILENAME)
    type is (character(len=*))
       if(filename /= '-') then
          open(newunit=igetunit, file=trim(filename), action="read", iomsg=message,&
           &form="unformatted", access="stream",status='old',iostat=ios)
          label=filename
       else ! copy stdin to a scratch file
          call copystdin()
       endif
    type is (integer)
       if(filename /= stdin) then
          rewind(unit=filename,iostat=ios,iomsg=message)
          igetunit=filename
       else ! copy stdin to a scratch file
          call copystdin()
       endif
       write(label,'("unit ",i0)')filename
   end select
   if(ios == 0)then  ! if file was successfully opened
      inquire(unit=igetunit, size=nchars)
      if(nchars <= 0)then
         call stderr_local( '*filebyte* empty file '//trim(label) )
         return
      endif
      ! read file into text array
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios,iomsg=message) text      ! load input file -> text array
      if(ios /= 0)then
         call stderr_local( '*filebyte* bad read of '//trim(label)//':'//trim(message) )
      endif
   else
      call stderr_local('*filebyte* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif

   close(iostat=ios,unit=igetunit)            ! close if opened successfully or not

   if(present(lines).or.present(length))then  ! get length of longest line and number of lines
      icount=0
      do i=1,nchars
         if(text(i) == NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
            icount=0
         endif
         icount=icount+1
      enddo
      if(nchars /= 0)then
         if(text(nchars) /= NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
         endif
      endif
      if(present(lines))lines=lines_local
      if(present(length))length=length_local
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine copystdin()
integer :: iostat
   open(newunit=igetunit, iomsg=message,&
   &form="unformatted", access="stream",status='scratch',iostat=iostat)
   open(unit=stdin,pad='yes')
   INFINITE: do while (getline(line,iostat=iostat)==0)
      write(igetunit)line//new_line('a')
   enddo INFINITE
   rewind(igetunit,iostat=iostat,iomsg=message)
end subroutine copystdin
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr_local(message)
character(len=*) :: message
   write(stderr,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine filebyte
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    number_of_lines(3f) - [M_io:QUERY] read an open sequential file to get
!!                          number of lines
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function number_of_lines(lun) result(nlines)
!!
!!    integer,intent(in)          :: lun
!!    integer                     :: nlines
!!
!!##DESCRIPTION
!!    Rewind an open sequential file and read through it to count the number
!!    of lines. The file is rewound on exit. If it is not readable -1 is returned.
!!
!!##OPTIONS
!!    lun       logical unit number of open sequential file to count lines in.
!!
!!##RETURNS
!!    nlines    number of lines read. If it is not readable -1 is returned.
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_number_of_lines
!!    use M_io,      only : number_of_lines, fileopen
!!    implicit none
!!    integer :: ios
!!    integer :: lun
!!       lun=fileopen('test.txt','r',ios)
!!       if(ios == 0)then
!!          write(*,*) number_of_lines(lun)
!!       else
!!          write(*,*)'ERROR: IOS=',ios
!!       endif
!!    end program demo_number_of_lines
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function number_of_lines(lun) result(nlines)
!@(#) determine number or lines in file given a LUN to the open file
integer,intent(in) :: lun

integer            :: ios
integer            :: nlines
character(len=256) :: iomsg

   if(lun /= stdin)rewind(lun,iostat=ios,iomsg=iomsg)
   nlines = 0

   do
   read(lun, '(A)', end=99, iostat=ios,iomsg=iomsg)
      if (ios /= 0) then
         write(stderr,gen)'*number_of_lines*:',trim(iomsg)
         nlines=-1
         exit
      endif
      nlines = nlines + 1
   enddo

99 continue

   if(lun /= stdin)rewind(lun,iostat=ios,iomsg=iomsg)

end function number_of_lines
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io:QUERY] Find a FUN/LUN (Fortran-unit-number) that is not in use
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    Usage
!!
!!       integer function notopen(start,end,err)
!!       integer,optional,intent(in)  :: start
!!       integer,optional,intent(in)  :: end
!!       integer,optional,intent(out) :: err
!!##DESCRIPTION
!!    A free FORTRAN unit number is needed to OPEN a file. NOTOPEN() returns
!!    a FORTRAN unit number from START to END not currently associated with
!!    an I/O unit. START and END are expected to be positive integers where
!!    END  >=  START.
!!
!!    If NOTOPEN() returns -1, then no free FORTRAN unit could be found in
!!    the specified range.
!!
!!    Otherwise, NOTOPEN() returns an integer representing a free FORTRAN
!!    logical unit number. Note that NOTOPEN() assumes the following unit
!!    numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module
!!
!!       ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT
!!
!!    are special, and will never return those values.
!!
!!##OPTIONS
!!       start  optional logical unit number to start scan at, defaults to 10.
!!       end    optional logical unit number to stop scan at, defaults to 99.
!!       err    optional error flag returned. ERR will be non-zero if
!!              no errors. If not present and an error occurs the program
!!              will stop instead of returning.
!!
!!##NOTES
!!
!!    Why are the default START and END limits from 10 to 99? the Fortran 77
!!    standard did not specify a specific limit on the upper range limit, but
!!    the LUN range of 1 to 99 was almost always supported in conventional
!!    programming environments. Additionally, units in the range 0-10 have
!!    often been the units used for pre-assigned files. Occasionally 100,
!!    101 and 102 are reserved (for files such as standard input, standard
!!    output, standard error, ...). Therefore, the defaults for START and
!!    END were selected to be 10 and 99. And most programs do not need
!!    more than 90 files simultaneously open, so the defaults work well in
!!    practice with many versions/vintages of Fortran.
!!
!!    Note that an environment may impose a limit on the number of
!!    simultaneously open files (which some compilers work around).
!!
!!    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead
!!    of an open unit locator.
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_notopen ! test the NOTOPEN(3f) function
!!     use m_io, only: notopen
!!     implicit none
!!     integer :: ii, ierr, igot
!!
!!     write(*,*)'check for preassigned files from unit 0 to unit 1000'
!!     write(*,*)'(5 and 6 always return -1)'
!!
!!     do ii=0,1000
!!        if(notopen(ii,ii,ierr)  /=  ii)then
!!           write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
!!        endif
!!     enddo
!!
!!     ! open all files from UNIT=10 to UNIT=30 so have used units
!!     do ii=10,30,1
!!       open(unit=ii,status="scratch")
!!     enddo
!!     ! close UNIT=25
!!     close(25)
!!
!!     ! find open file in range 10 to 30
!!     write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)
!!
!!     close(18)
!!     do ii=10,32
!!       igot=notopen(ii,ii,ierr)
!!       write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
!!     enddo
!!
!!     end program demo_notopen
!!
!!    Expected output(can vary with each programming environment):
!!
!!       check for preassigned files from unit 0 to unit 1000
!!       (5 and 6 always return -1)
!!       INUSE:    0    -1
!!       INUSE:    5    -1
!!       INUSE:    6    -1
!!       Should get 25 for this .. 25
!!       For  unit  10  I  got  -1  with  ERR=  -1
!!       For  unit  11  I  got  -1  with  ERR=  -1
!!       For  unit  12  I  got  -1  with  ERR=  -1
!!       For  unit  13  I  got  -1  with  ERR=  -1
!!       For  unit  14  I  got  -1  with  ERR=  -1
!!       For  unit  15  I  got  -1  with  ERR=  -1
!!       For  unit  16  I  got  -1  with  ERR=  -1
!!       For  unit  17  I  got  -1  with  ERR=  -1
!!       For  unit  18  I  got  18  with  ERR=   0
!!       For  unit  19  I  got  -1  with  ERR=  -1
!!       For  unit  20  I  got  -1  with  ERR=  -1
!!       For  unit  21  I  got  -1  with  ERR=  -1
!!       For  unit  22  I  got  -1  with  ERR=  -1
!!       For  unit  23  I  got  -1  with  ERR=  -1
!!       For  unit  24  I  got  -1  with  ERR=  -1
!!       For  unit  25  I  got  25  with  ERR=   0
!!       For  unit  26  I  got  -1  with  ERR=  -1
!!       For  unit  27  I  got  -1  with  ERR=  -1
!!       For  unit  28  I  got  -1  with  ERR=  -1
!!       For  unit  29  I  got  -1  with  ERR=  -1
!!       For  unit  30  I  got  -1  with  ERR=  -1
!!       For  unit  31  I  got  31  with  ERR=   0
!!       For  unit  32  I  got  32  with  ERR=   0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
integer function notopen(start,end,err)
implicit none

! ident_7="@(#) M_io notopen(3f) find free FORTRAN unit number to OPEN() a file"

integer,optional,intent(in)    :: start                           ! unit number to start looking at
integer,optional,intent(in)    :: end                             ! last unit number to look at
integer,optional,intent(out)   :: err                             ! error flag returned
integer                        :: istart
integer                        :: iend
integer                        :: ierr

integer         :: i10                                            ! counter from start to end
integer         :: ios                                            ! iostatus from INQUIRE
logical         :: lopen                                          ! returned from INQUIRE
logical         :: lexist                                         ! returned from INQUIRE
!-----------------------------------------------------------------------------------------------------------------------------------
   !! IEND=MERGE( END, 99, PRESENT(END)) do not use merge, as TSOURCE must be evaluated before the call
   if(present(start))then; istart=start; else; istart=10; endif
   if(present(end  ))then; iend  =end  ; else; iend  =99; endif
   ierr=0
   notopen=(-1)                                                   ! result if no units are available
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=istart,iend                                             ! check units over selected range
      select case (i10)                                           ! always skip these predefined units
      case(stderr,stdin,stdout)
          cycle
      end select
      inquire( unit=i10, opened=lopen, exist=lexist, iostat=ios )
      if( ios == 0 )then                                          ! no error on inquire
         if(.not. lopen .and. lexist)then                         ! if unit number not in use, return it
            notopen = i10
            exit                                                  ! only need to find one, so return
         endif
      else
         write(stderr,*)'*notopen*:error on unit ',i10,'=',ios
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if (notopen  <  0 )then                                       ! no valid unit was found in given range
      ierr=-1
   else                                                           ! valid value being returned
      ierr=0
   endif
   if(present(err))then                                           ! if error flag is present set it
      err=ierr
   elseif(ierr /= 0)then                                          ! if error occurred and error flag not present stop program
      stop 1
   endif
end function notopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dirname(3f) - [M_io:PATHNAMES] strip last component from filename
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dirname(FILENAME) result (DIRECTORY)
!!
!!      character(len=*),intent(in)  :: FILENAME
!!      character(len=:),allocatable :: DIRECTORY
!!
!!##DESCRIPTION
!!    Output FILENAME with its last non-slash component and trailing slashes
!!    removed. If FILENAME contains no slash or backslash character, output
!!    '.' (meaning the current directory).
!!
!!    Assumes leaf separator is a slash or backslash as determined by
!!    separator(3f) and that FILENAME does not contain trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to remove the last leaf from
!!
!!##RETURNS
!!      DIRECTORY  directory name for pathname
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dirname
!!    use M_io, only : dirname
!!    implicit none
!!    character(len=:),allocatable :: filename
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1 , command_argument_count()
!!       call get_command_argument (i , length=filename_length)
!!       if(allocated(filename))deallocate(filename)
!!       allocate(character(len=filename_length) :: filename)
!!       call get_command_argument (i , value=filename)
!!       write(*,'(a)')dirname(filename)
!!    enddo
!!    end program demo_dirname
!!
!!   Sample program executions:
!!
!!      demo_dirname /usr/bin/          -> "/usr"
!!      demo_dirname dir1/str dir2/str  -> "dir1" followed by "dir2"
!!      demo_dirname stdio.h            -> "."
!!
!!##SEE ALSO
!!    dirname(3c), basename(3c), readlink(3c), realpath(3c)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        dirname(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function dirname(filename) result (directory)
implicit none

! ident_8="@(#) M_io dirname(3f) strip last component from filename"

character(len=*),intent(in)      :: filename
character(len=:),allocatable     :: directory
integer                          :: iend
character(len=1)                 :: sep
!-----------------------------------------------------------------------------------------------------------------------------------
   sep=separator()
   directory=trim(filename)
   call removetail()                         ! trim trailing slashes even if duplicates
   iend=index(directory,sep,back=.true.)     ! find last slash if any
   if(iend == 0)then                         ! filename is a leaf
      directory='.'                          ! special case
   else
      directory=directory(:iend-1)           ! remove leaf
      call removetail()                      ! trim off trailing slashes in case duplicates
   endif
   directory=trim(directory)                 ! clean up return value
contains
   subroutine removetail()              ! replace trailing slashes with spaces even if duplicates
   integer :: right
   do right=len(directory),1,-1
      if(directory(right:right) == sep.or.directory(right:right) == ' ')then
         directory(right:right)=' '
      else
         exit
      endif
   enddo
   end subroutine removetail

end function dirname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    basename(3f) - [M_io:PATHNAMES] return last component from filename
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function basename(FILENAME,SUFFIX) result (LEAF)
!!
!!      character(len=:),allocatable :: FILENAME
!!      character(len=*),intent(in),optional :: SUFFIX
!!      character(len=*),intent(in) :: LEAF
!!
!!##DESCRIPTION
!!    Output LEAF of filename with directory paths removed.
!!
!!    Assumes leaf separator is a slash or backslash as determined by
!!    separator(3f) and that filename does not contain trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to extract the last leaf from
!!      SUFFIX     suffix to remove. If not present
!!                 the rightmost ".string" string is removed.
!!                 If present the LEAF is returned with any matching
!!                 suffix removed.
!!
!!##RETURNS
!!      LEAF  returned leaf name
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_basename
!!    use M_io, only : basename
!!    implicit none
!!    character(len=:),allocatable :: fn
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1, command_argument_count()
!!       call get_command_argument (i, length=filename_length)
!!       if(allocated(fn))deallocate(fn)
!!       allocate(character(len=filename_length) :: fn)
!!       call get_command_argument (i, value=fn)
!!       ! leaf with any suffix removed
!!       ! leaf with suffix retained
!!       ! with suffix unless it is ".f90"
!!       write(*,'(*(a,1x))') basename(fn), basename(fn,''), basename(fn,'.f90')
!!    enddo
!!    end program demo_basename
!!
!!   Sample program executions:
!!
!!     $demo_basename /usr/bin/
!!     bin bin bin
!!     $demo_basename dir1/fred.x dir2/.y
!!     fred fred.x fred.x
!!     .y .y .y
!!     $demo_basename stdio.h
!!     stdio stdio.h stdio.h
!!     $demo_basename /name.f90
!!     name name.f90 name
!!
!!##SEE ALSO
!!    basename(3c), basename(3c), readlink(3c), realpath(3c)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        basename(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function basename(filename,suffix) result (leaf)
implicit none

! ident_9="@(#) M_io basename(3f) strip last component from filename"

character(len=*),intent(in)          :: filename
character(len=*),intent(in),optional :: suffix
character(len=:),allocatable         :: leaf
integer                              :: iend
integer                              :: i
integer,parameter                    :: maxlen=4096
character(len=maxlen)                :: name
character(len=maxlen)                :: bname
character(len=maxlen)                :: extension
character(len=1)                 :: sep
   sep=separator()
   iend=len_trim(filename)
   do i=iend,1,-1
      if(filename(i:i) /= sep)exit
      iend=iend-1
   enddo
   call splitpath(filename(:iend),name=name,basename=bname,ext=extension)
   if(present(suffix))then
      leaf=merge(bname,name,suffix == extension)
   else
      leaf=bname
   endif
   if(leaf == '')leaf=name
   leaf=trim(leaf)
end function basename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileopen(3f) - [M_io] A simple open of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fileopen(filename,mode,ios) result(lun)
!!
!!    character(len=*),intent(in)           :: filename
!!    character(len=*),intent(in),optional  :: mode
!!    integer,intent(out),optional          :: ios
!!    integer                               :: lun
!!
!!##DESCRIPTION
!!    fileopen(3f) is a convenience routine that allows you to open a file
!!    for sequential reading and writing as a text file in a form commonly
!!    found in C and interpreted languages such as shells. See the OPEN(3f)
!!    statement for more demanding I/O specifications (asynchronous, direct,
!!    unformatted, ... ). The documentation for the flexible and powerful
!!    OPEN(3f) statement can be a bit overwhelming; this routine cuts it
!!    down to the just the simple basic functions typically available in
!!    a scripting language such as bash, tcsh, sh, ...
!!
!!    Specify the file's name as the string FILENAME with a shell-like prefix
!!    specifying the access mode, or alternatively specify a plain FILENAME
!!    and the kind of access you need to the file with the string MODE.
!!
!!    Three fundamental kinds of access are available: read, write,
!!    and append.
!!
!!##OPTION
!!   FILENAME  The filename to open. If the beginning of the filename is
!!
!!             <   open for read. File must exist
!!             >   open for write. Will overwrite current file
!!             >>  open for append. Will append to current file
!!
!!             If no prefix exists to specify a file access mode, it
!!             will depend on the values of the MODE argument (meaning
!!             the default will be "readwrite").
!!
!!             A blank filename causes a unit number for a scratch file
!!             to be returned.
!!
!!   MODE     [rwa][tb][+]
!!            An alternate way to specify the file access mode is to specify
!!            a MODE value. It should begin with one of the three characters
!!            "r", "w", or "a". It defaults to 'rw'. It is case-insensitive.
!!
!!
!!        READING PREFIX
!!        r,<   Open the file for reading; the operation will fail if the
!!              file does not exist, or if the host system does not permit
!!              you to read it.
!!
!!        WRITING PREFIXES
!!        w,>   Open a file for writing from the beginning of the file.
!!              If the file whose name you specified already existed,
!!              the call fails.
!!
!!        o     Open the file for writing from the beginning of the file:
!!              effectively, this always creates a new file. If the file
!!              whose name you specified already existed, its old contents
!!              are discarded.
!!
!!        a,<<  Initially open the file for appending data (ie. writing
!!              at the end of file).
!!
!!        SUFFIX
!!
!!        b   Append a "b" to any of the three modes above to specify that
!!            you are opening the file as a "binary file" (the default is
!!            to open the file as a sequential formatted text file. This
!!            switch changes to to an unformatted stream).
!!
!!                   open( ... access='stream';form='unformatted')
!!
!!        t   Append a "t" to any of the three modes (rwa) to specify a
!!            formatted stream
!!
!!                   open( ... access='stream';form='formatted')
!!
!!        +   Finally, you might need to both read and write from the same
!!            file. You can specify "rw" or you can append a `+' to any of
!!            the three primary modes ("rwa") to permit "readwrite" access
!!
!!        v   Additionally, "v" selects verbose mode, which prints the
!!            OPEN(3f) options explicitly selected
!!
!!        NOTES
!!
!!            If you want to append both `b' and `+', you can do it in
!!            either order: for example, "rb+" means the same thing as
!!            "r+b" when used as a mode string.)
!!
!!    IOS    The error code returned by the OPEN(3f) statement ultimately
!!           executed by this function. If not present the program stops on
!!           an error.
!!##RETURNS
!!        FILEOPEN(3f) returns a Fortran unit number which you can use
!!        for other file operations, unless the file you requested could
!!        not be opened; in that situation, the result is -1 (a reserved
!!        value that cannot be returned as a NEWUNIT value on an OPEN(3f))
!!        and IOS will be non-zero.
!!
!!##EXAMPLE
!!
!!  Common usage
!!
!!   READ
!!     R=fileopen('<in.txt')
!!     or
!!     R=fileopen('in.txt','r')
!!
!!   WRITE
!!     W=fileopen('>out.txt')
!!     or
!!     W=fileopen('out.txt','W')
!!
!!   READWRITE
!!     RW=fileopen('inout.txt')
!!
!!   APPEND
!!     A=fileopen('>>inout.txt')
!!     or
!!     A=fileopen('inout.txt','a')
!!
!!   Sample program
!!
!!       program demo_fileopen
!!       use M_io, only : fileopen, fileclose, print_inquire
!!       implicit none
!!       integer :: lun
!!       lun=fileopen('fred.txt')
!!       call print_inquire(lun)
!!       end program demo_fileopen
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileopen(filename,mode,ios) result(lun)
character(len=*),intent(in)           :: filename
character(len=*),intent(in),optional  :: mode
integer,intent(out),optional          :: ios
integer                               :: lun, i, ios_local,ifound,gts
character(len=:),allocatable          :: local_mode
character(len=256)                    :: message
character(len=:),allocatable          :: action, position, access, form, status, file
logical                               :: verbose
   local_mode=lower(merge_str(mode,'',present(mode)))
   file=trim(adjustl(filename))//'   '
   ifound=index(file,'>>')
   if(ifound /= 0)then
      file(ifound:ifound+1)='  '
      local_mode=local_mode//'a'
   endif
   ifound=index(file,'>')
   if(ifound /= 0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'w'
   endif
   ifound=index(file,'<')
   if(ifound /= 0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'r'
   endif
   file=adjustl(file)
   local_mode=merge_str('rw',local_mode,local_mode == '')
   file=trim(file)

   gts=0
   action=''
   position='asis'
   form='formatted'
   access='sequential'
   status='unknown'
   verbose=.false.
   do i=1,len(local_mode) ! create order independence
      select case(local_mode(i:i))
       case('r','<'); if(action /= 'readwrite'.and.action /= 'read')action='read'//action
                      if(status == 'unknown')status='old'
       case('w','>'); if(action /= 'readwrite'.and.action /= 'write')action=action//'write'
                      if(status=='unknown')status='new'
                      if(gts > 0)then
                         position='append'
                      endif
                      gts=gts+1
       case('o');     if(action /= 'readwrite'.and.action /= 'write')action=action//'write'
                      if(status=='unknown')then
                         status='replace'
                      endif
       case('a');     position='append'
                      if(action /= 'readwrite'.and.action /= 'write')action=action//'write'
                      if(status == 'old')status='unknown'
       case('b');     access='stream';form='unformatted'
       case('t');     access='stream';form='formatted'
       case('+');     action='readwrite'
                      status='unknown'
       case('v');     verbose=.true.
       case default
         write(*,'(*(g0))',advance='no')'*fileopen* unknown mode key ',local_mode(i:i)
         write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & ' INPUTNAME=',trim(file), &
         & ' MODE=',trim(local_mode)
      end select
   enddo
   if(action == '')action='readwrite'

   if(verbose)then
      write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & 'INPUTNAME=',trim(file), &
         & 'MODE=',trim(local_mode)
      write(*,'(a)',advance='no')'==>'
      write(*,'(*(:,"[",g0,"=",g0,"]"))') &
         & 'FILE=',trim(file), &
         & 'FORM=',trim(form), &
         & 'ACCESS=',trim(access), &
         & 'ACTION=',trim(action), &
         & 'POSITION=',trim(position), &
         & 'STATUS=',trim(status)
   endif
   if(file /= ' ')then
    open(file=file,newunit=lun,form=form,access=access,action=action,position=position,status=status,iostat=ios_local,iomsg=message)
   else
    open(newunit=lun,form=form,access=access,action=action,status='scratch',iostat=ios_local,iomsg=message)
   endif
   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
   !  ACTION    =  READ|WRITE  |  READWRITE
   !  FORM      =  FORMATTED   |  UNFORMATTED
   !  POSITION  =  ASIS        |  REWIND       |  APPEND
   !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
   if(ios_local /= 0)then
      call journal('sc','*fileopen* ',message)
      lun=-1
   endif
   if(present(ios))then        ! caller has asked for status so let caller process any error
      ios=ios_local
   elseif(ios_local /= 0)then  ! caller did not ask for status so stop program on error
      stop 1
   endif
end function fileopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileclose(3f) - [M_io] A simple close of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     function fileclose(lun) result(ios)
!!
!!      integer,intent(in)       :: lun
!!      integer                  :: ios
!!##DESCRIPTION
!!   A convenience command for closing a file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN unit number to close
!!##RETURNS
!!   IOS status value from CLOSE
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_fileclose
!!     use M_io, only : fileclose, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios, ierr
!!        lun=fileopen('<input.txt',ios=ierr)
!!        if(ierr /= 0)then
!!           write(*,*)'<ERROR> opening file'
!!        endif
!!        ios=fileclose(lun)
!!     end program demo_fileclose
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileclose(lun) result(ios)
integer,intent(in)       :: lun
integer                  :: ios
character(len=256)       :: message
   close(unit=lun,iostat=ios,iomsg=message)
   if(ios /= 0)then
      call journal('sc','*fileclose* ',message)
      stop
   endif
end function fileclose
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filewrite(3f) - [M_io:WRITE] A simple write of a CHARACTER array to a file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     function filewrite(filename,data,status,position) result(ierr)
!!
!!      character(len=*),intent(in) :: filename
!!      character(len=*),intent(in) :: data(:)
!!      character(len=*),intent(in),optional :: status
!!      character(len=*),intent(in),optional :: position
!!      integer                     :: ierr
!!##DESCRIPTION
!!   A convenience procedure for writing a CHARACTER array as
!!   a new file.
!!##OPTION
!!   FILENAME   file to create or write. If the name ends
!!              in ">" the default for STATUS changes to
!!              "REPLACE". If it ends in ">>" STATUS changes to
!!              "UNKNOWN" and the default POSITION changes to "APPEND".
!!   DATA       CHARACTER array to write to file
!!   STATUS     STATUS to use on OPEN(7f). Defaults to "NEW".
!!              Allowed values are  NEW|REPLACE|OLD|SCRATCH|UNKNOWN
!!   POSITION   POSITION to use on OPEN(7f). Defaults to "REWIND".
!!              Allowed values are  ASIS|REWIND|APPEND
!!##RETURNS
!!   IERR       status value. Zero indicates no error occurred
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_filewrite
!!     use M_io, only : filewrite
!!     implicit none
!!     integer :: ierr
!!     character(len=:),allocatable :: data(:)
!!        data=[ character(len=80) :: &
!!             &'This is the text to write  ', &
!!             &'into the file. It will be  ', &
!!             &'trimmed on the right side. ', &
!!             &' ', &
!!             &'     That is all Folks!    ', &
!!             &'']
!!        ierr=filewrite('_scratch.txt',data)
!!     end program demo_filewrite
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function filewrite(filename,filedata,status,position) result (ierr)
! write filedata to file filename
character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
character(len=*),intent(in),optional  :: status
character(len=*),intent(in),optional  :: position
integer                               :: ierr
integer                               :: lun, i, ios, ilen
character(len=256)                    :: message
character(len=:),allocatable          :: file
character(len=:),allocatable          :: local_status
character(len=:),allocatable          :: local_position
character(len=:),allocatable          :: default_status
character(len=:),allocatable          :: default_position
   ierr=0
   default_status='NEW'
   default_position='REWIND'
   file=trim(adjustl(filename))//'  '
   ilen=max(len_trim(file),2)
   if(file(ilen-1:ilen) == '>>')then
      ilen=ilen-2
      file=file(:ilen)
      default_status='UNKNOWN'
      default_position='APPEND'
   elseif(file(ilen:ilen) == '>')then
      ilen=ilen-1
      file=file(:ilen)
      default_status='REPLACE'
   else
      file=trim(file)
   endif
   if(present(position))then; local_position=position; else; local_position=default_position; endif
   if(present(status))then;   local_status=status;     else; local_status=default_status;     endif
   if(file /= ' ')then
      open(file=file, &
      & newunit=lun, &
      & form='formatted', &         !  FORM      =  FORMATTED   |  UNFORMATTED
      & access='sequential', &      !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
      & action='write', &           !  ACTION    =  READ|WRITE  |  READWRITE
      & position=local_position, &  !  POSITION  =  ASIS        |  REWIND       |  APPEND
      & status=local_status, &      !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
      & iostat=ios, &
      & iomsg=message)
   else
      lun=stdout
      ios=0
   endif
   if(ios /= 0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',file,trim(message)
      ierr=ios
   else
      do i=1,size(filedata)                                                    ! write file
         write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
         if(ios /= 0)then
            write(stderr,'(*(a,1x))')'*filewrite* error:',file,trim(message)
            ierr=ios
            exit
         endif
      enddo
   endif
   close(unit=lun,iostat=ios,iomsg=message)                                 ! close file
   if(ios /= 0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',trim(message)
      ierr=ios
   endif
end function filewrite
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filedelete(3f) - [M_io] A simple close of an open file with STATUS='DELETE'
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function filedelete(lun) result(ios)
!!
!!     integer,intent(in)          :: lun
!!       or
!!     character(len=*),intent(in) :: filename
!!     integer                     :: ios
!!
!!##DESCRIPTION
!!   A convenience command for deleting an OPEN(3f) file that leaves an
!!   error message in the current journal file if active
!!##OPTION
!!   LUN  unit number of open file to delete or filename.
!!##RETURNS
!!   IOS  status returned by CLOSE().
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_filedelete
!!     use M_io, only : filedelete, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios
!!        lun=fileopen('<input.txt')
!!        ios=filedelete(lun)
!!     end program demo_filedelete
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function filedelete_lun(lun) result(iostat)
integer,intent(in)    :: lun
integer               :: iostat
character(len=256)    :: message
   close(unit=lun,iostat=iostat,status='delete',iomsg=message)
   if(iostat /= 0)then
      call journal('sc','*filedelete* ',message)
   endif
end function filedelete_lun
function filedelete_filename(filename) result(iostat)
character(len=*),intent(in) :: filename
integer                     :: number
integer                     :: iostat
character(len=256)          :: message
logical                     :: opened
logical                     :: exist
   inquire(file=filename,opened=opened,iostat=iostat,exist=exist,number=number)
   if(exist)then
      if(.not.opened)then
         open(newunit=number,iostat=iostat,file=filename)
      endif
      close(unit=number,iostat=iostat,status='delete',iomsg=message)
      if(iostat /= 0)then
         call journal('sc','*filedelete* ',message)
      endif
   endif
end function filedelete_filename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    joinpath(3f) - [M_io:PATHNAMES] join parts of a pathname together
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9)  result(path)
!!
!!     character(len=*), intent(in)           :: a1, a2
!!     character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
!!     character(len=:), allocatable          :: path
!!##DESCRIPTION
!!##OPTIONS
!!     a1,a2  the first two pathname sections to join. Required
!!     a3-a9  additional optional sections to join
!!##RETURNS
!!     pathname sections joined together with trailing spaces removed from
!!     the ends of sections and a separator (as returned by separator(3f)
!!     ) placed between them, and duplicate adjacent separators removed
!!     accept for one beginning the joined pathname.
!!##EXAMPLE
!!
!!   Sample program
!!
!!      program demo_joinpath
!!      use M_io, only : joinpath
!!      implicit none
!!         write(*,*)joinpath(&
!!         &'/share/user','/man/','man3','joinpath.3m_io'//'.gz' &
!!         &)
!!      end program demo_joinpath
!!
!! Results:
!!
!!      >  /share/user/man/man3/joinpath.3m_io.gz
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1 /= '')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   if (present(a6)) path = path // filesep // trim(a6)
   if (present(a7)) path = path // filesep // trim(a7)
   if (present(a8)) path = path // filesep // trim(a8)
   if (present(a9)) path = path // filesep // trim(a9)
   path=adjustl(path//'  ')
   call substitute(path,filesep//filesep,filesep,start=2) ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function joinpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     splitpath(3f) - [M_io:PATHNAMES] split a Unix pathname into components
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out),optional :: dir
!!    character(len=maxlen),intent(out),optional :: name
!!    character(len=maxlen),intent(out),optional :: basename
!!    character(len=maxlen),intent(out),optional :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash separates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of
!!    the filename components; it merely parses a string.
!!
!!    Assumes leaf separator is a slash or backslash as determined by
!!    separator(3f) and that filename does not contain trailing spaces.
!!
!!##OPTIONS
!!    path      Path to be broken into components. It is assumed
!!
!!              o Forward slashes (/) separate pathname components.
!!              o the name '.' means "current directory"
!!              o the name '..' means "up one directory"
!!              o a pathname ending in a slash is a directory name
!!              o a slash starting the pathname represents the root
!!                directory.
!!              o trailing spaces are insignificant.
!!
!!    Using these rules helps to reduce incorrect parsing, but the
!!    routine is only intended for simple parsing of names of the form
!!    "[dir/]name[.extension].
!!
!!##RESULTS
!!    dir       Path of directories, including the trailing slash.
!!    name      Name of file leaf or, if no file is specified in path,
!!              name of the lowest directory.
!!    basename  NAME with any extension removed
!!    ext       File name extension, if any, including the leading period (.).
!!
!!    The path parameter can be a complete or partial file specification. The
!!    special name "." is assumed to mean the current directory, and the
!!    special name ".." is assumed to mean one directory above the current
!!    directory.
!!
!!##EXAMPLE
!!
!!   program demo_splitpath
!!
!!    use m_io, only : splitpath
!!    implicit none
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),parameter   :: file(*)=[&
!!       & 'dirs/name.ext  ', &
!!       & 'xx/IO/zz/NN.FF ', &
!!       & 'xx/IO/zz/NN    ', &
!!       & '/xx/IO/zz/NN   ', &
!!       & '/xx/IO/zz/     ', &
!!       & '/xx/IO/zz.A/   ', &
!!       & '/xx/IO/zz/.    ', &
!!       & '               ', &
!!       & './             ', &
!!       & '/              ', &
!!       & '/..            ', &
!!       & './..           ', &
!!       & 'name.          ', &
!!       & '.name          ', &
!!       & '.name.         ', &
!!       & '.              ', &
!!       & '..             ', &
!!       & '...            ']
!!
!!    character(len=maxlen)  :: dir
!!    character(len=maxlen)  :: name
!!    character(len=maxlen)  :: basename
!!    character(len=maxlen)  :: ext
!!    integer                :: i
!!    integer                :: longest
!!    longest=maxval(len_trim(file)) ! find longest filename
!!
!!    do i=1,size(file)
!!       call splitpath(file(i), dir, name, basename, ext)
!!       write(*,'(*("| ",a:))')  &
!!       & file(i)(:longest),     &
!!       & dir(:longest),         &
!!       & name(:longest),        &
!!       & basename(:longest),    &
!!       & ext(:longest)
!!    enddo
!!   end program demo_splitpath
!!
!!   Output
!!
!!    | dirs/name.ext | dirs          | name.ext      | name          | .ext
!!    | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
!!    | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
!!    | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
!!    | /xx/IO/zz/    | /xx/IO/zz     |               |               |
!!    | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
!!    | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
!!    |               | .             |               |               |
!!    | ./            | .             |               |               |
!!    | /             | /             |               |               |
!!    | /..           | /             |               |               |
!!    | ./..          | ./..          |               |               |
!!    | name.         |               | name.         | name          | .
!!    | .name         |               | .name         | .name         |
!!    | .name.        |               | .name.        | .name         | .
!!    | .             | .             |               |               |
!!    | ..            |               |               |               |
!!    | ...           |               | ...           | ..            | .
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine splitpath(path,dir,name,basename,ext)
implicit none

! ident_10="@(#) M_io splitpath(3f) split Unix pathname into components (dir name basename extension)"

!===================================================================================================================================
character(len=*),intent(in)           :: path
character(len=*),intent(out),optional :: dir
character(len=*),intent(out),optional :: name
character(len=*),intent(out),optional :: basename
character(len=*),intent(out),optional :: ext
integer,parameter                     :: maxlen=4096
character(len=maxlen)                 :: dir_local
character(len=maxlen)                 :: name_local
character(len=maxlen)                 :: basename_local
character(len=maxlen)                 :: ext_local
character(len=len(path)+1)            :: path_local
integer                               :: where
integer                               :: i
integer                               :: iend
character(len=1)                 :: sep
   sep=separator()
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir_local=''
   name_local=''
   basename_local=''
   ext_local=''
   iend=len_trim(path_local)
   LOCAL : block
!===================================================================================================================================
   if(iend == 0)then                         ! blank input path
      dir_local='.'
      exit LOCAL
   endif
!===================================================================================================================================
   if(path_local(iend:iend) == sep)then      ! assume entire name is a directory if it ends in a slash
      if(iend > 1)then
         dir_local=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir_local=path_local
      endif
      exit LOCAL
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i) == sep)then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend == 0)then                         ! path composed entirely of slashes.
      dir_local=sep
      exit LOCAL
   endif
!===================================================================================================================================
   where=INDEX(path_local,sep,BACK=.true.)   ! find any right-most slash in remaining non-null name_local after trimming trailing slashes
   if(where <= 0)then                        ! no slash in path so everything left is name_local
      name_local=path_local(:iend)                 ! this is name_local unless '.' or '..'
   else                                      ! last slash found
      dir_local=path_local(:where-1)               ! split into directory
      name_local=path_local(where+1:iend)          ! this is name_local unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name_local(1:3))                   ! special cases where name_local is a relative directory name_local '.' or '..'
   case('.  ')
      dir_local=path_local
      name_local=''
   case('.. ')
      if(dir_local == '')then
         if(path_local(1:1) == sep)then
            dir_local=sep
         endif
      else
         dir_local=path_local
      endif
      name_local=''
   case default
   end select
!===================================================================================================================================
   if(name_local == '.')then
      name_local=''
   endif
!===================================================================================================================================
   iend=len_trim(name_local)
   where=INDEX(name_local,'.',BACK=.true.)         ! find any extension
   if(where > 0.and.where /= 1)then         ! only consider a non-blank extension name_local
      ext_local=name_local(where:)
      basename_local=name_local(:where-1)
   else
      basename_local=name_local
   endif
!===================================================================================================================================
   endblock LOCAL
   if(present(dir))dir=dir_local
   if(present(name))name=name_local
   if(present(basename))basename=basename_local
   if(present(ext))ext=ext_local
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getline(3f) - [M_io:READ] read a line from specified LUN into allocatable
!!                  string up to line length limit
!!    (LICENSE:PD)
!!
!!##SYNTAX
!!   function getline(line,lun,iostat) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,intent(out),optional             :: iostat
!!    integer                                  :: ier
!!
!!##DESCRIPTION
!!    Read a line of any length up to programming environment maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The input file must have a PAD attribute of YES for the function
!!    to work properly, which is typically true.
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##OPTIONS
!!    LINE    line read
!!    LUN     optional LUN (Fortran logical I/O unit) number. Defaults
!!            to stdin.
!!    IOSTAT  status returned by READ(IOSTAT=IOS). If not zero, an error
!!            occurred or an end-of-file or end-of-record was encountered.
!!            This is the same value as returned by the function. See the
!!            example program for a usage case.
!!##RETURNS
!!    IER     zero unless an error occurred. If not zero, LINE returns the
!!            I/O error message.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_getline
!!    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
!!    use,intrinsic :: iso_fortran_env, only : iostat_end
!!    use M_io, only : getline
!!    implicit none
!!    integer :: iostat
!!    character(len=:),allocatable :: line
!!       open(unit=stdin,pad='yes')
!!       INFINITE: do while (getline(line,iostat=iostat)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!       if(iostat /= iostat_end)then
!!          write(*,*)'error reading input:',trim(line)
!!       endif
!!    end program demo_getline
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function getline(line,lun,iostat) result(ier)
implicit none

! ident_11="@(#) M_io getline(3f) read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer,intent(out),optional             :: iostat
integer                                  :: ier
character(len=4096)                      :: message

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=buflen)                    :: buffer
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=stdin
   endif

   INFINITE: do                                                      ! read characters from line and append to result
      read(lun_local,pad='yes',iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for files
                                                                     ! other than stdin so system line limit is not limiting
      if(isize > 0)line_local=line_local//buffer(:isize)            ! append what was read to result
      if(is_iostat_eor(ier))then                                     ! if hit EOR reading is complete unless backslash ends the line
         ier=0                                                       ! hitting end of record is not an error for this routine
         exit INFINITE                                               ! end of reading line
     elseif(ier /= 0)then                                            ! end of file or error
        line=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   line=line_local                                                   ! trim line
   if(present(iostat))iostat=ier
end function getline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_line(3f) - [M_io:READ] read a line from specified LUN into allocatable
!!                     string up to line length limit cleaning up input line
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function read_line(line,lun,ios) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,optional                         :: ios
!!    integer                                  :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to the programming environment maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The input file must have a PAD attribute of YES for the function to
!!    work properly, which is typically true but can be set on an open file.
!!
!!    o Append lines that end in a backslash with next line
!!    o Expand tabs
!!    o Replace unprintable characters with spaces
!!    o Remove trailing carriage return characters and white space
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a time
!!    could (depending on the programming environment used) be inefficient,
!!    as it could reallocate and allocate memory used for the output string
!!    with each buffer read.
!!
!!##OPTIONS
!!    LINE   the line read from the file.
!!    LUN    The LUN (logical unit) to read from. Defaults to stdin.
!!    IOS    status returned by READ(IOSTAT=IOS). If not zero, an error
!!           occurred or an end-of-file or end-of-record was encountered.
!!           This is the same value as returned by the function. See the
!!           example program for a usage case.
!!##RETURNS
!!    IER    status returned by READ(IOSTAT=IER). If not zero, an error
!!           occurred or an end-of-file or end-of-record was encountered.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!   Checking the error message and counting lines:
!!
!!     program demo_read_line
!!     use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
!!     use,intrinsic :: iso_fortran_env, only : stderr => error_unit
!!     use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
!!     use M_io, only : read_line
!!     implicit none
!!     character (len =: ), allocatable :: line
!!     integer                          :: stat
!!     integer                          :: icount=0
!!        open(unit=stdin,pad='yes')
!!        INFINITE: do while (read_line(line,ios=stat) == 0)
!!           icount=icount
!!           write (*, '(*(g0))') icount,' [',line,']'
!!        enddo INFINITE
!!        if ( .not.is_iostat_end(stat) ) then
!!           write (stderr, '(*(g0))') &
!!           & 'error: line ',icount,'==>',trim (line)
!!        endif
!!     end program demo_read_line
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function read_line(line,lun,ios) result(ier)
implicit none

! ident_12="@(#) M_io read_line(3f) read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer,optional                         :: ios
integer                                  :: ier

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=256)                       :: message
integer                                  :: biggest
character(len=buflen)                    :: buffer
integer                                  :: last
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   lun_local=merge(lun,stdin,present(lun))
   INFINITE: do                                                           ! read characters from line and append to result
      read(lun_local,pad='yes',iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for
                                                                          ! files other than stdin so system line limit
                                                                          ! is not limiting
      if(isize > 0)line_local=line_local//buffer(:isize)   ! append what was read to result
      if(is_iostat_eor(ier))then                            ! if hit EOR reading is complete unless backslash ends the line
         last=len(line_local)
         if(last /= 0)then
            if(line_local(last:last) == '\')then            ! if line ends in backslash it is assumed a continued line
               line_local=line_local(:last-1)               ! remove backslash
               cycle INFINITE                               ! continue on and read next line and append to result
            endif
         endif
         ier=0                                              ! hitting end of record is not an error for this routine
         exit INFINITE                                      ! end of reading line
     elseif(ier /= 0)then                                   ! end of file or error
        line_local=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   biggest=8*len(line_local)                                ! worst case is raw line is all tab characters
   if(allocated(line))deallocate(line)
   allocate(character(len=biggest) :: line)
   call notabs(line_local,line,last)                        ! expand tabs, trim carriage returns, remove unprintable characters
   line=noesc(line)
   line=trim(line(:last))                                   ! trim line
   if(present(ios))then
      ios=ier
   endif
end function read_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      get_tmp(3f) - [M_io:QUERY] Return the name of the scratch directory
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!     function get_tmp() result(tname)
!!
!!      character(len=:),allocatable :: tname
!!##DESCRIPTION
!!
!!    Return the name of the scratch directory set by the most common
!!    environment variables used to designate a scratch directory.
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1]
!!    to use to specify a temporary directory for scratch space. If $TMPDIR
!!    is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If
!!    nothing is set "/tmp/" is returned. The returned value always ends in
!!    "/". No test is made that the directory exists or is writable.
!!
!!##EXAMPLE
!!
!!
!!   Sample:
!!
!!     program demo_get_tmp
!!     use M_io, only : get_tmp, uniq
!!     implicit none
!!     character(len=:),allocatable :: answer
!!        answer=get_tmp()
!!        write(*,*)'result is ',answer
!!        answer=get_tmp()//uniq('_scratch',create=.false.)
!!        write(*,*)'the file ',answer, &
!!        & ' was a good scratch file name, at least a moment ago'
!!     end program demo_get_tmp
!!
!!   Sample Results:
!!
!!     > result is /cygdrive/c/Users/JSU/AppData/Local/Temp/
!!     >
!!     > the file /cygdrive/c/Users/JSU/AppData/Local/Temp/_scratch
!!     > was a good scratch file name, at least a moment ago
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function get_tmp() result(tname)

! ident_13="@(#) M_io get_tmp(3f) Return the name of the scratch directory"

character(len=:),allocatable :: tname
integer                      :: lngth
character(len=10),parameter  :: names(4)=["TMPDIR    ","TEMP      ","TEMPDIR   ","TMP       "]
integer                      :: i
character(len=1)             :: sep
   sep=separator()
   tname=''
   do i=1,size(names)
      call get_environment_variable(name=names(i), length=lngth)
      if(lngth /= 0)then
         if(allocated(tname))deallocate(tname)
         allocate(character(len=lngth) :: tname)
         call get_environment_variable(name=names(i), value=tname)
         exit
      endif
   enddo
   if(lngth == 0)then
      tname='/tmp'
      lngth=len_trim(tname)
   endif
   if(scan(tname(lngth:lngth),'/\') == 0)then
      tname=tname//sep
   endif
end function get_tmp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! rd(3f) - [M_io:READ] ask for string from standard input with user-definable prompt
!! (LICENSE:PD)
!!
!!   function rd(prompt,default) result(out)
!!
!!    character(len=*),intent(in)              :: prompt
!!
!!   One of
!!
!!    character(len=*),intent(in)              :: default
!!    character(len=:),allocatable,intent(out) :: out
!!
!!    integer,intent(in)                       :: default
!!    integer,intent(out)                      :: out
!!
!!    real,intent(in)                          :: default
!!    real,intent(out)                         :: out
!!
!!    doubleprecision,intent(in)               :: default
!!    doubleprecision,intent(out)              :: out
!!
!!    logical,intent(in)                       :: default
!!    logical,intent(out)                      :: out
!!
!!
!!##DESCRIPTION
!!    Ask for string or value from standard input with user-definable prompt
!!    up to 20 times.
!!
!!    Do not use the function in an I/O statement as not all versions of
!!    Fortran support this form of recursion. Numeric values may be input
!!    in standard INTEGER, REAL, and DOUBLEPRECISION formats or as whole
!!    numbers in base 2 to 36 in the format BASE#VALUE.
!!
!!##OPTIONS
!!    prompt    Prompt string; displayed on same line as input is read from
!!    default   default answer on carriage-return. The type of the default
!!              determines the type of the output.
!!##RETURNS
!!    out       returned string or value. If an end-of-file or system error
!!              is encountered the string "EOF" is returned, or a "Nan"
!!              REAL numeric value, or huge(0), or .false. .
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rd
!!    use M_io, only : rd
!!    implicit none
!!    character(len=:),allocatable :: mystring
!!    doubleprecision              :: d
!!    real                         :: r
!!    integer                      :: i
!!    logical                      :: l
!!
!!    INFINITE: do
!!       mystring=rd('Enter string or "STOP":',default='Today')
!!       if(mystring == 'STOP')stop
!!       i=rd('Enter integer:',default=huge(0))
!!       r=rd('Enter real:',default=huge(0.0))
!!       d=rd('Enter double:',default=huge(0.0d0))
!!       l=rd('Enter logical:',default=.false.)
!!
!!       write(*,*)'I=', i, 'R=', r, 'D=',d,  'MYSTRING=', mystring
!!       write(*,*)'L=', l
!!    enddo INFINITE
!!
!!    end program demo_rd
!!
!!##AUTHOR
!!    John S. Urban, 1993
!!##LICENSE
!!    Public Domain
function rd_logical(prompt,default) result(out)
! 1995 John S. Urban
!
implicit none

! ident_14="@(#) M_io rd_logical(3fp) ask for logical value from standard input with user-definable prompt"

character(len=*),intent(in)  :: prompt
logical,intent(in)           :: default
logical                      :: out

integer                      :: prompt_len
integer                      :: igot
integer                      :: ierr
integer                      :: icount
integer                      :: ios
character(:),allocatable     :: response
character(len=256)           :: iomsg
   out=.false.
   response=''
   prompt_len=len(prompt)
   do icount=1,20                                                 ! prevent infinite loop on error or end-of-file
      if(prompt_len > 0)write(*,'(a,'' '')',advance='no')prompt  ! write prompt
      ierr=getline(response,stdin)                                ! get back string
      igot=len(response)
      if(ierr /= 0)then
         cycle
      elseif(igot == 0.and.prompt_len > 0)then
         out=default
         exit
      elseif(igot <= 0)then
         call journal('*rd* blank string not allowed')
         cycle
      else
         response=response//' '
         select case(response(1:1))
         case('y','Y')
            out=.true.
         case('n','N')
            out=.false.
         case default
            read(response,*,iostat=ios,iomsg=iomsg)out
            if(ios /= 0)then
               write(*,*)trim(iomsg)
               cycle
            endif
         end select
         exit
      endif
   enddo
end function rd_logical
!===================================================================================================================================
function rd_character(prompt,default) result(strout)
! 1995 John S. Urban
!
implicit none

! ident_15="@(#) M_io rd_character(3fp) ask for string from standard input with user-definable prompt"

character(len=*),intent(in)  :: prompt
character(len=*),intent(in)  :: default
character(len=:),allocatable :: strout

integer                      :: len_default
integer                      :: igot
integer                      :: ierr
integer                      :: icount
!===================================================================================================================================
   len_default=len(prompt)
!===================================================================================================================================
   do icount=1,20                                                  ! prevent infinite loop on error or end-of-file
      if(len_default > 0)write(*,'(a,'' '')',advance='no')prompt  ! write prompt
      ierr=getline(strout,stdin)                                  ! get back string
      igot=len(strout)
      if(ierr /= 0)then
         strout='EOF'
         cycle
      elseif(igot == 0.and.len_default > 0)then
         strout=default
         exit
      elseif(igot <= 0)then
         call journal('*rd* blank string not allowed')
         cycle
      else
         exit
      endif
   enddo
end function rd_character
!===================================================================================================================================
function rd_doubleprecision(prompt,default,iostat) result(dvalue)
implicit none

! ident_16="@(#) M_io rd_doubleprecision(3fp) ask for number from standard input with user-definable prompt"

doubleprecision              :: dvalue
integer                      :: ivalue
character(len=*),intent(in)  :: prompt
doubleprecision,intent(in)   :: default
integer,intent(out),optional :: iostat
character(len=:),allocatable :: strout
character(len=:),allocatable :: message
integer                      :: itest

   iostat=0
   dvalue=default
   strout=adjustl(rd_character(prompt,'NaN'))

   ! 1 for an integer [-+]NNNNN
   ! 2 for a whole number [-+]NNNNN.
   ! 3 for a real value [-+]NNNNN.MMMM
   ! 4 for a exponential value [-+]NNNNN.MMMM[-+]LLLL [-+]NNNNN.MMMM[ed][-+]LLLL
   ! values less than 1 represent an error
   if(strout == 'NaN')then
      dvalue=default
   elseif(index(strout,'#') /= 0)then
      if( decodebase(strout,0,ivalue))then
         dvalue=ivalue
      else
         iostat=-1
         write(*,*)'ERROR> could not convert ',strout
      endif
   else
      itest=isnumber(strout,message)
      if(itest > 0)then
         dvalue=s2v(strout,ierr=iostat)
      else
         iostat=-2
         write(*,*)' ERROR> for ',strout,' ',itest,':',trim(message)
      endif
   endif
end function rd_doubleprecision
!===================================================================================================================================
function rd_real(prompt,default,iostat) result(rvalue)
implicit none

! ident_17="@(#) M_io rd_real(3fp) ask for number from standard input with user-definable prompt"

real                         :: rvalue
real(kind=dp)                :: dvalue
character(len=*),intent(in)  :: prompt
real,intent(in)              :: default
integer,intent(out),optional :: iostat
   !*! what about Nan, Inf, -Inf? Likely place for compiler bugs
   dvalue=rd_doubleprecision(prompt,dble(default),iostat)
   if(dvalue /= dvalue)then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] is indefinite'
      rvalue=huge(0.0)
   else
      rvalue=real(dvalue)
   endif
end function rd_real
!===================================================================================================================================
function rd_integer(prompt,default,iostat) result(ivalue)
implicit none

! ident_18="@(#) M_io rd_integer(3fp) ask for number from standard input with user-definable prompt"

integer                      :: ivalue
real(kind=dp)                :: dvalue
character(len=*),intent(in)  :: prompt
integer,intent(in)           :: default
integer,intent(out),optional :: iostat
   dvalue=rd_doubleprecision(prompt,dble(default),iostat)
   !*! what about Nan, Inf, -Inf?
   if(dvalue /= dvalue)then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] is indefinite'
      ivalue=huge(0)
   elseif(dvalue > huge(0))then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] greater than ', huge(0)
      ivalue=huge(0)
   elseif(dvalue < 1-huge(0))then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] less than ', 1-huge(0)
      ivalue=1-huge(0)
   else
      ivalue=nint(dvalue)
   endif
end function rd_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getname(3f) - [M_io:QUERY] get name of the current executable
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function getname() result(name)
!!
!!     character(len=:),allocatable         :: getname
!!
!!##DESCRIPTION
!!    getname(3f) returns the name of the current executable using
!!    get_command_argument(3f) and inquire(3f).
!!
!!##EXAMPLE
!!
!!    Sample getting a pathname of current executable:
!!
!!      program demo_getname
!!      use M_io, only : getname
!!      implicit none
!!         write(*,'(*(a))')'Running ',getname()
!!      end program demo_getname
!!
!!##AUTHOR
!!        John S. Urban
!!
!!##LICENSE
!!        Public Domain
function getname() result(name)
! get the pathname of arg0
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: ios
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=ios)
   if(ios == 0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=ios)
      if(ios == 0)then
         inquire(file=arg0,iostat=ios,name=long_name)
         if(ios == 0)then
            name=trim(long_name)
         else
            name=arg0
         endif
      else
         arg0=''
      endif
   else
      arg0=''
   endif
end function getname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     which(3f) - [M_io:SCANNAMES] given a command name find the pathname
!!                 by searching the directories in the environment variable
!!                 $PATH
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function which(command) result(pathname)
!!
!!    character(len=*),intent(in)  :: command
!!    character(len=:),allocatable :: pathname
!!
!!##DESCRIPTION
!!    Given a command name find the first file with that name in the directories
!!    specified by the environment variable $PATH.
!!
!!##OPTIONS
!!    COMMAND   the command to search for
!!
!!##RETURNS
!!    PATHNAME  the first pathname found in the current user path. Returns blank
!!              if the command is not found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_which
!!     use M_io, only : which
!!     implicit none
!!        write(*,*)'ls is ',which('ls')
!!        write(*,*)'dir is ',which('dir')
!!        write(*,*)'install is ',which('install')
!!     end program demo_which
!!
!!##SEE ALSO
!!    M_system:system_dir(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function which(command) result(pathname)
character(len=*),intent(in)     :: command
character(len=:),allocatable    :: pathname, checkon, paths(:), exts(:)
integer                         :: i, j
   pathname=''
   call split(get_env('PATH'),paths,delimiters=merge(';',':',separator() == '\'))
   SEARCH: do i=1,size(paths)
      checkon=trim(joinpath(trim(paths(i)),command))
      select case(separator())
      case('/')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
      case('\')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
         if(exists(checkon//'.bat'))then
            pathname=checkon//'.bat'
            exit SEARCH
         endif
         if(exists(checkon//'.exe'))then
            pathname=checkon//'.exe'
            exit SEARCH
         endif
         call split(get_env('PATHEXT'),exts,delimiters=';')
         do j=1,size(exts)
            if(exists(checkon//'.'//trim(exts(j))))then
               pathname=checkon//'.'//trim(exts(j))
               exit SEARCH
            endif
         enddo
      end select
   enddo SEARCH
contains
logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function
end function which
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     lookfor(3f) - [M_io:SCANNAMES] look for a filename in a number
!!                   of directories specified by an environment variable
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function lookfor(basename,env) result(pathname)
!!
!!    character(len=:),intent(in)  :: basename
!!    character(len=:),intent(in)  :: env
!!    character(len=:),allocatable :: pathname
!!
!!##DESCRIPTION
!!    Given a base filename find the first file with that name in the directories
!!    specified by the environment variable ENV
!!
!!##OPTIONS
!!    BASENAME   the file to search for
!!    ENV        environment variable name. Separator between directory names is
!!               assumed to be a colon on ULS (Unix-Like Systems) and semi-colon on
!!               MS-Windows machines.
!!
!!##RETURNS
!!    PATHNAME  the first pathname found in the current user path. Returns blank
!!              if the file is not found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_lookfor
!!     use M_io, only : lookfor
!!     implicit none
!!     character(len=:),allocatable :: returned
!!        returned=lookfor('ls','PATH')
!!        write(*,*)'ls is ',returned
!!        returned=lookfor('dir.exe','PATH')
!!        write(*,*)'dir is ',returned
!!     end program demo_lookfor
!!
!!##SEE ALSO
!!    M_system:system_dir(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function lookfor(basename,env) result(pathname)
character(len=*),intent(in)     :: basename
character(len=*),intent(in)     :: env
character(len=:),allocatable    :: pathname, checkon, paths(:)
integer                         :: i
logical                         :: r
   pathname=''
   call split(get_env(env),paths,delimiters=merge(';',':',separator() == '\'))
   if(size(paths) == 0)then
      paths=['']
   endif
   do i=1,size(paths)
      checkon=trim(joinpath(trim(paths(i)),basename))
      inquire(file=checkon, exist=r)
      if(r)then
         pathname=checkon
         exit
      endif
   enddo
end function lookfor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     get_env(3f) - [M_io:QUERY] a function returning the value of
!!                   an environment variable
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!    function get_env(NAME,DEFAULT) result(VALUE)
!!
!!     character(len=*),intent(in)          :: NAME
!!     character(len=*),intent(in),optional :: DEFAULT
!!     character(len=:),allocatable         :: VALUE
!!
!!
!!##DESCRIPTION
!!     Get the value of an environment variable or optionally return a
!!     default value if the returned value would be a blank string.
!!
!!     This is a duplicate of system_getenv(3m_system) used to avoid
!!     some interdependencies.
!!
!!##OPTIONS
!!    NAME     name of environment variable
!!    DEFAULT  value to return if environment variable is not set or set
!!             to an empty string
!!##RETURNS
!!    VALUE    the value of the environment variable or the default
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_get_env
!!       use M_io, only : get_env
!!       character(len=:),allocatable :: HOME
!!          HOME=get_env('HOME','UNKNOWN')
!!          write(*,'(a)')HOME,get_env('PATH')
!!          write(*,'(a)')get_env('HOME'),get_env('PATH')
!!       end program demo_get_env
!!
!!##SEE ALSO
!!    get_environment_variable(3fortran), system_getenv(3m_system),
!!    set_environment_variable(3m_system), system_putenv(3m_system),
!!    system_clearenv(3m_system), system_initenv(3m_system),
!!    system_readenv(3m_system), system_unsetenv(3m_system)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function get_env(NAME, DEFAULT) result(VALUE)
implicit none
character(len=*), intent(in)           :: NAME
character(len=*), intent(in), optional :: DEFAULT
character(len=:), allocatable          :: VALUE
integer                                :: howbig
integer                                :: stat
   if (NAME /= '') then
      call get_environment_variable(NAME, length=howbig, status=stat, trim_name=.true.) ! get length required to hold value
      select case (stat)
      case (1); VALUE = '' ! NAME is not defined in the environment
      case (2); VALUE = '' ! This processor doesn't support environment variables. Boooh!
      case default
         allocate (character(len=max(howbig, 1)) :: VALUE)                         ! make string to hold value of sufficient size
         call get_environment_variable(NAME, VALUE, status=stat, trim_name=.true.) ! get value
         if (stat /= 0) VALUE = ''
      end select
   else
      VALUE = ''
   end if
   if (VALUE == '' .and. present(DEFAULT)) VALUE = DEFAULT
end function get_env
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     get_next_char(3f) - [M_io:READ] read from a file one character at a time
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!    subroutine get_next_char(fd,c,ios)
!!
!!     integer,intent(in)    :: fd
!!     character,intent(out) :: c
!!     integer,intent(out)   :: ios
!!
!!
!!##DESCRIPTION
!!    This reads a file opened with stream access one character at a
!!    time, much like ""read(fd,iostat=ios) c" but with buffering, which
!!    I have found to be up to sixty times faster than such a plain read,
!!    although this varies depending on how or if the programming environment
!!    implements I/O buffering itself.
!!
!!    IT USES SAVED VARIABLES AND CAN ONLY BE USED ON ONE FILE AT A TIME
!!    IN THE CURRENT FORM. A user type including the saved values and the
!!    LUN could easily resolve this.
!!
!!##OPTIONS
!!    FD    A Fortran unit number of a file opened for stream access
!!    C     The next returned character if IOS=0
!!    IOS   The error status returned by the last read. It is zero (0) if
!!          no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_get_next_char
!!    use,intrinsic :: iso_fortran_env, only : iostat_end
!!    use M_io, only : get_next_char
!!    implicit none
!!    character(len=4096) :: filename ! filename to read
!!    character(len=256)  :: message  ! returned error messages
!!    integer             :: fd       ! file descriptor for input file
!!    integer             :: ios,ios1 ! hold I/O error flag
!!    character           :: c1       ! current character read
!!       filename='test.in'
!!       open(unit=fd,file=trim(filename),access='stream',status='old',&
!!       & iostat=ios,action='read',form='unformatted',iomsg=message)
!!       if(ios /= 0)then
!!          write(*,*)&
!!          '*demo_get_next_char* ERROR: could not open '//&
!!          trim(filename)
!!          write(*,*)&
!!          '*demo_get_next_char* ERROR: '//trim(message)
!!          stop 5
!!       endif
!!       ! loop through read of file one character at a time
!!       ONE_CHAR_AT_A_TIME: do
!!          ! get next character from buffered read from file
!!          call get_next_char(fd,c1,ios1)
!!          if(ios1 == iostat_end)then
!!             ! reached end of file so stop
!!             stop
!!          elseif(ios1 /= 0 )then
!!             ! error on file read
!!             write(*,*)&
!!          '*demo_get_next_char* ERROR: before end of '//&
!!          trim(filename)
!!             stop 1
!!          endif
!!          ! do something with the characters
!!          write(*,'(a)',advance='no')c1
!!       enddo ONE_CHAR_AT_A_TIME
!!    end program demo_get_next_char
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine get_next_char(fd,c,ios)
! replace "read(fd,iostat=ios) c" because gfortran on CygWin sixty times slower with plain read (no system buffering?)
! quick buffering read
implicit none
integer,intent(in)          :: fd
character,intent(out)       :: c
integer,intent(out)         :: ios
integer,parameter           :: bufsize=1048576
character(len=1),save       :: buff(bufsize)
integer,save                :: point=0
integer,save                :: filepoint=1
integer,save                :: sz=bufsize

ios=0

do
select case(point)
case(0)                                            ! read a buffer
   read(fd,iostat=ios,pos=filepoint) buff(1:sz)
   if(is_iostat_end(ios))then                      ! this is the last buffer
      if(sz /= 1)then                              ! try again with a smaller buffer
         sz=sz/2
         sz=max(1,sz)
         cycle
      endif
   elseif(ios == 0)then                            ! no error occurred so successfully read a buffer
      c=buff(1)
      filepoint=filepoint+sz
      point=sz-1
   endif
case(1:)                                           ! getting a character from a previous buffer
   point=point-1
   c=buff(sz-point)
case default
   write(*,*)'*get_next_char* internal error '
   read(fd,iostat=ios) c
end select
! assume if IOS is not zero, not called again until new file is started
   if(ios /= 0)then
      filepoint=1
      point=0
      sz=bufsize
   endif
   exit
enddo
end subroutine get_next_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io:FILENAME] generate a filename containing a number
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    Usage
!!
!!       function filename_generator(head,tail,num,lenlimit) result(filename)
!!       character(len=*),intent(in)  :: head
!!       character(len=*),intent(in)  :: tail
!!       integer,intent(in) :: num
!!       integer,intent(in) :: lenlimit
!!       character(len=:),allocatable :: filename
!!
!!##DESCRIPTION
!!
!!    Generate a filename containing a representation of the specified
!!    whole number.  This is useful for generating a series of filenames
!!    differing by a number such as "file1.txt", "file2.txt",
!!    ... .
!!
!!##OPTIONS
!!
!!    head      filename prefix.
!!    tail      filename suffix.
!!    num       number to represent as a string between HEAD and TAIL.
!!    lenlimit  number of digits up to which to zero-pad the string
!!              representing NUM.
!!
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!       program demo_filename_generator
!!       use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
!!       use M_io, only : filename_generator
!!       implicit none
!!
!!           ! no zero-fill
!!           write(*,*) filename_generator("file_",".dat",11)
!!           ! zero-fill till 3 digits
!!           write(*,*) filename_generator("file_",".dat",11,3)
!!           ! zero-fill till 9 digits
!!           write(*,*) filename_generator("file_",".dat",11,9)
!!           ! same as default (no zero-fill)
!!           write(*,*) filename_generator("file_",".dat",11,0)
!!
!!       end program demo_filename_generator
!!
!!    Results
!!
!!       > file_11.dat
!!       > file_011.dat
!!       > file_000000011.dat
!!       > file_11.dat
!!
!!##AUTHOR
!!    Zh, Niu; with modifications by John S. Urban
!!##LICENSE
!!    Public Domain

function filename_generator(head, tail, num, lenlimit) result(filename)
character(*),intent(in)      :: head
character(*),intent(in)      :: tail
integer,intent(in)           :: num
integer,intent(in),optional  :: lenlimit
character(len=:),allocatable :: filename

character(30)                :: fmt
integer                      :: local_lenlimit

   if ( present(lenlimit) ) then
      local_lenlimit = lenlimit
   else
      local_lenlimit = 0
   endif

   fmt = ""
   write(fmt, '("(a,i0.",i2.2,",a)")' ) local_lenlimit
   filename=repeat(' ', len(head) + len(tail) + max(19,local_lenlimit) )
   write(filename(:),fmt) trim(adjustl(head)), num, trim(adjustl(tail))
   filename=trim(filename)
end function filename_generator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! routines from other modules to make this one stand-alone
!     XX                    XX       X
!      X                     X                              X
!      X                     X                              X
!  XXXXX  XX  XX  XXXXXX     X     XXX     XXXXX   XXXX    XXXX    XXXXX   XXXXX
! X    X   X   X   X    X    X       X    X     X      X    X     X     X X     X
! X    X   X   X   X    X    X       X    X        XXXXX    X     XXXXXXX  XXX
! X    X   X   X   X    X    X       X    X       X    X    X     X           XX
! X    X   X  XX   X    X    X       X    X     X X    X    X  X  X     X X     X
!  XXXXXX   XX XX  XXXXX   XXXXX   XXXXX   XXXXX   XXXX X    XX    XXXXX   XXXXX
!                  X
!                 XXX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lenset(line,length) result(strout)

!character(len=*),parameter::ident_36="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2i(chars,valu,ierr)

!character(len=*),parameter::ident_41="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8 <= huge(valu))then
      if(valu8 <= huge(valu))then
         valu=int(valu8)
      else
         write(*,*)'sc','*a2i*','- value too large',valu8,'>',huge(valu)
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

!character(len=*),parameter::ident_42="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero.  if no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr /= 0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars /= 'eod')then                           ! print warning message except for special value "eod"
         write(*,*)'sc','*a2d* - cannot produce number from string ['//trim(chars)//']'
         if(msg /= '')then
            write(*,*)'*a2d* - ['//trim(msg)//']'
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

!character(len=*),parameter::ident_43="@(#)M_strings::s2v(3f): returns doubleprecision number from string"


character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local /= 0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
function s2vs(string,delim) result(darray)

!character(len=*),parameter::ident_55="@(#)M_strings::s2vs(3f): function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function decodebase(string,basein,out_baseten)
implicit none

!character(len=*),parameter::ident_72="@(#)M_strings::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein == 0.and.ipound > 1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local >= 0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch == '-'.and.k == 1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine trimzeros(string)

!character(len=*),parameter::ident_50="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.') == 0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i <= 1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine substitute(targetline,old,new,ierr,start,end)

!character(len=*),parameter::ident_11="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id <= 0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar > maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new > 0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind == ic-1.or.ind > ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind > ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd > maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new > maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new /= 0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      write(*,*)'*substitute* new line will be too long'
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd > maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic < len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    isnumber(3f) - [M_strings:NUMERIC] determine if a string represents a number
!    (LICENSE:PD)
!
! SYNOPSIS
!    function isnumber(str,msg)
!
!     character(len=*),intent(in)  :: str
!     character(len=:),intent(out),allocatable,optional  :: msg
!
! DESCRIPTION
!     ISNUMBER(3f) returns a value greater than zero if the string represents
!     a number, and a number less than or equal to zero if it is a bad number.
!     Blank characters are ignored.
!
! OPTIONS
!     str  the string to evaluate as to whether it represents a numeric value
!          or not
!     msg  An optional message describing the string
!
! RETURNS
!     isnumber  the following values are returned
!
!                1 for an integer             [-+]NNNNN
!                2 for a whole number         [-+]NNNNN.
!                3 for a real value           [-+]NNNNN.MMMM
!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
!
!               values less than 1 represent an error
!
! EXAMPLES
!   As the example shows, you can use an internal READ(3f) along with the
!   IOSTAT= parameter to check (and read) a string as well.
!
!     program demo_isnumber
!     use M_strings, only : isnumber
!     implicit none
!     character(len=256) :: line
!     real               :: value
!     integer            :: ios
!     integer            :: answer
!     character(len=256) :: message
!     character(len=:),allocatable :: description
!        write(*,*)'Begin entering values, one per line'
!        do
!           read(*,'(a)',iostat=ios)line
!           !
!           ! try string as number using list-directed input
!           line=''
!           read(line,*,iostat=ios,iomsg=message) value
!           if(ios == 0)then
!              write(*,*)'VALUE=',value
!           elseif( is_iostat_end(ios) ) then
!              stop 'end of file'
!           else
!              write(*,*)'ERROR:',ios,trim(message)
!           endif
!           !
!           ! try string using isnumber(3f)
!           answer=isnumber(line,msg=description)
!           if(answer > 0)then
!              write(*,*) &
!              & ' for ',trim(line),' ',answer,':',description
!           else
!              write(*,*) &
!              & ' ERROR for ',trim(line),' ',answer,':',description
!           endif
!           !
!        enddo
!     end program demo_isnumber
!
!  Example run
!
!    > Begin entering values
!    > ERROR:          -1 End of file
!    >  ERROR for            -1 :null string
!    >10
!    > VALUE=   10.0000000
!    >  for 10            1 :integer
!    >20
!    > VALUE=   20.0000000
!    >  for 20            1 :integer
!    >20.
!    > VALUE=   20.0000000
!    >  for 20.            2 :whole number
!    >30.1
!    > VALUE=   30.1000004
!    >  for 30.1            3 :real number
!    >3e1
!    > VALUE=   30.0000000
!    >  for 3e1            4 :value with exponent
!    >1-2
!    > VALUE=   9.99999978E-03
!    >  for 1-2            4 :value with exponent
!    >100.22d-4
!    > VALUE=   1.00220004E-02
!    >  for 100.22d-4            4 :value with exponent
!    >1--2
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1--2           -5 :bad number
!    >e
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for e           -6 :missing leading value before exponent
!    >e1
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for e1           -6 :missing leading value before exponent
!    >1e
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1e           -3 :missing exponent
!    >1e+
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1e+           -4 :missing exponent after sign
!    >1e+2.0
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1e+2.0           -5 :bad number
!
! AUTHOR
!    John S. Urban
!
! LICENSE
!    Public Domain
function isNumber(string,msg,verbose)
implicit none

! ident_1="@(#)M_strings::isnumber(3f): Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=s2a(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend == 0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)) /= 0) i=i+1  ! skip optional leading sign
      if(i > iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i > iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i) == '.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i > iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i > iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)) /= 0)then
         i=i+1
         if(i == 2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i > iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)) /= 0) i=i+1
      if(i > iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i > iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i <= iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isdigit(ch) result(res)

! ident_2="@(#)M_strings::isdigit(3f): Returns .true. if ch is a digit (0-9) and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from
!    input string
!    (LICENSE:PD)
!
! SYNOPSIS
!    function nospace(str) - remove all whitespace from input string
!
!     character(len=*),intent(in)          :: str
!     character(len=:),allocatable         :: nospace
!
! DESCRIPTION
!    nospace(3f) removes space, tab, carriage return, new line, vertical
!    tab, formfeed and null characters (called "whitespace"). The output
!    is returned trimmed.
!
! EXAMPLES
!   Sample program:
!
!     program demo_nospace
!     use M_strings, only: nospace
!     implicit none
!     character(len=:),allocatable  :: s
!        s='  This     is      a     test  '
!        write(*,*) 'original input string is ....',s
!        write(*,*) 'processed output string is ...',nospace(s)
!        if(nospace(s) == 'Thisisatest')then
!           write(*,*)'nospace test passed'
!        else
!           write(*,*)'nospace test error'
!        endif
!     end program demo_nospace
!
!   Expected output
!
!     original input string is ....  This     is      a     test
!     processed output string is ...Thisisatest
!     nospace test passed
!
! AUTHOR
!    John S. Urban
!
! LICENSE
!    Public Domain
function nospace(line)

! ident_3="@(#)M_strings::nospace(3f): remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isspace(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     null, space, tab, carriage return, new line, vertical tab, or formfeed
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isspace(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isspace
!!
!!##DESCRIPTION
!!     isspace(3f) returns .true. if character is a null, space, tab,
!!     carriage return, new line, vertical tab, or formfeed
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isspace  returns true if character is ASCII white space
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isspace
!!     use M_strings, only : isspace
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISSPACE: ', &
!!        & ichar(pack( string, isspace(string) ))
!!     end program demo_isspace
!!
!!   Results:
!!
!!    ISSPACE:  0 9 10 11 12 13 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isspace(ch) result(res)

! ident_63="@(#)M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

!character(len=*),parameter::ident_7="&
!&@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol > ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function upper(str,begin,end) result (string)

!character(len=*),parameter::ident_21="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)

!character(len=*),parameter::ident_22="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

!character(len=*),parameter::ident_24="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

!character(len=*),parameter::ident_47="@(#)M_strings::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

!character(len=*),parameter::ident_40="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         write(*,*)'*value_to_string* UNKNOWN TYPE'
         chars=' '
      end select
      if(fmt == '') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.') /= 0) call trimzeros(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local /= 0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

!character(len=*),parameter::ident_37="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in)     :: str1
character(len=*),intent(in)     :: str2
logical,intent(in)              :: expr
character(len=:),allocatable    :: strout
integer                         :: big
   big=max(len(str1),len(str2))
   strout=trim(merge(lenset(str1,big),lenset(str2,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine notabs(INSTR,OUTSTR,ILEN)

!character(len=*),parameter::ident_31="&
!&@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=*),INTENT(OUT)  :: outstr       ! tab-expanded version of INSTR produced
INTEGER,INTENT(OUT)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
INTEGER,PARAMETER             :: tabsize=8    ! assume a tab stop is set every 8th column
INTEGER                       :: ipos         ! position in OUTSTR to put next character of INSTR
INTEGER                       :: lenin        ! length of input string trimmed of trailing spaces
INTEGER                       :: lenout       ! number of characters output string can hold
INTEGER                       :: istep        ! counter that advances thru input string INSTR one character at a time
CHARACTER(LEN=1)              :: c            ! character in input line being processed
INTEGER                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               write(*,*)"*notabs* output string overflow"
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function noesc(INSTR)
! ident_48="@(#) M_strings noesc(3f) convert non-printable characters to a space"
character(len=*),intent(in) :: INSTR      ! string that might contain nonprintable characters
character(len=len(instr))   :: noesc
integer                     :: ic,i10
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=iachar(INSTR(i10:i10))
      if(ic <= 31.or.ic == 127)then       ! find characters with ADE of 0-31, 127
         noesc(I10:I10)=' '               ! replace non-printable characters with a space
      else
         noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
      endif
   enddo
end function noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)
! ident_2="@(#)M_journal::where_write_message(3fp): basic message routine used for journal files"
character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times             ! number of times written to my_stdout
character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O
character(len=4096)                :: mssge
   adv='yes'
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)trim(msg)
         endif
      case('S','s')
         write(my_stdout,'(a)',advance=adv)trim(msg)
         times=times+1
      case('E','e')
         write(stderr,'(a)',advance=adv)trim(msg)
         times=times+1
      case('+'); adv='no'
      case('>'); debug=.true.
      case('<'); debug=.false.
      case('N')                                                   ! new name for my_stdout
         if(msg /= ' '.and.msg /= '#N#'.and.msg /= '"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios == 0)then
               my_stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg == ' ')then
            close(unit=last_int,iostat=ios)
            my_stdout=6
         endif
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)comment,trim(msg)
         elseif(times == 0)then
            !! write(my_stdout,'(2a)',advance=adv)trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)comment,'DEBUG: ',trim(msg)
            elseif(times == 0)then
               write(my_stdout,'(3a)',advance=adv)'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios /= 0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(my_stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
end subroutine where_write_message

subroutine where_write_message_all(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, sep)
implicit none
! ident_5="@(#)M_journal::where_write_message_all(3f): writes a message to a string composed of any standard scalar types"
character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
character(len=*),intent(in),optional :: sep
call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,sep))
end subroutine where_write_message_all

subroutine write_message_only(message)
! ident_6="@(#)M_journal::write_message_only(3fp): calls JOURNAL('sc',message)"
character(len=*),intent(in)          :: message
   call where_write_message('sc',trim(message))
end subroutine write_message_only

function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
! ident_2="@(#)M_msg::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"
class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
         increment=len(sep)+1
         sep_local=sep
   else
      sep_local=' '
      increment=2
   endif
   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_scalar=trim(line)
contains

subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function msg_scalar

function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none
! ident_3="@(#)M_msg::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"
class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:),allocatable  :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      sep_local=' '
      increment=2
   endif
   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains

subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic

end function msg_one
!===================================================================================================================================
function crop(strin) result (strout)

! ident_19="@(#) M_strings crop(3f) trim leading and trailings spaces from resulting string"

character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(strin))
end function crop
!===================================================================================================================================
end module m_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
 
!>>>>> app/flower.f90
! always reducing multiple blank lines to one; maybe only do that if other lines were comments not if originally blank
program flower
! [dependencies]
! M_CLI2         = { git = "https://github.com/urbanjost/M_CLI2.git" }
! M_strings      = { git = "https://github.com/urbanjost/M_strings.git" }
! M_io           = { git = "https://github.com/urbanjost/M_io.git" }

use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
use M_io,                         only : get_next_char
use M_CLI2,                       only : set_args,lget,sget,sgets, filenames=>unnamed
implicit none

! ident_1="@(#) flower(1f) convert basic free-format Fortran file to lowercase or uppercase"

character(len=:),allocatable    :: filename
character(len=:),allocatable    :: help_text(:)
character(len=:),allocatable    :: version_text(:)
character(len=:),allocatable    :: outline
character(len=:),allocatable    :: outlinel
character(len=256)              :: iomsg             ! message field for returned messages
integer                         :: fd                ! file descriptor for file currently being read
integer                         :: ios               ! hold I/O error flag
character                       :: c1                ! current character read
character                       :: previous          ! previous significant character
character,parameter             :: nl=new_line('A')
integer                         :: ios1              ! hold I/O error flag
integer                         :: icount  = 0       ! number of characters read from file
integer                         :: icount_comm  = 0  ! number of characters read from file that are comments
integer                         :: idiff   = 0       ! number of characters different in files
integer                         :: ilines  = 0       ! number of newline characters encountered in first file
integer                         :: iposition  = 0    ! number of characters read from current line
integer                         :: atleast
integer                         :: i
integer                         :: io
logical                         :: incomment
logical                         :: insingle
logical                         :: indouble
logical                         :: tolower
logical                         :: ifblank
logical                         :: verbose
logical                         :: nocomment
logical                         :: nocode
logical                         :: nocont
character(len=3)                :: advance
integer                         :: ilen

call setup()
call set_args('--toupper F --nocomment F --nocode F --nocont F --stat F',help_text,version_text)
verbose=lget('verbose')
nocomment=lget('nocomment')
nocode=lget('nocode')
tolower=.not.lget('toupper')
nocont=lget('nocont')
if(lget('stat'))then
  verbose=.true.
  nocomment=.true.
  nocode=.true.
endif
if(nocomment.and.nocode)verbose=.true.
ifblank=.false.

if(size(filenames).lt.1)then
   filenames=['-']
   !write(stderr,'(a)')'*flower* ERROR: missing filename.'
   !stop 4
endif
FILES: do i=1,size(filenames)
   icount  = 0       ! number of characters read from file
   icount_comm  = 0  ! number of characters read from file that are comments
   idiff   = 0       ! number of characters different in files
   ilines  = 0       ! number of newline characters encountered in first file
   filename=trim(filenames(i))
   outline=''

   if(filename.eq.'-')then
      ! currently does not seem to be a way to open stdin as a stream so kludge using non-advancing I/O
      fd=stdin
   else
      fd=10
      close(unit=fd,iostat=ios)
      open(unit=fd,file=filename,access='stream',status='old',iostat=ios,action='read',form='unformatted',iomsg=iomsg)
      if(ios.ne.0)then
         write(stderr,'(a)') '*flower* ERROR: could not open '//filename
         write(stderr,'(a)') '*flower* ERROR: '//trim(iomsg)
         cycle FILES
      endif
   endif

   if(verbose)then
      if(nocomment.and.nocode)then
         continue
      elseif(nocomment)then
         write(stdout,'(/,a,a)') '!filename:',filename
      elseif(nocode)then
         write(stdout,'(/,a,a)') 'filename: ',filename
      endif
   endif

   ! reading one character at a time is much more limiting than reading lines or tokens
   ! but is sufficient for the vast majority of cases
   !
   iposition=0
   previous=achar(0)
   incomment=.false.
   insingle=.false.
   indouble=.false.
   advance='yes'
   iomsg=''
   ONE_CHAR_AT_A_TIME: do                                                ! loop through read of file one character at a time
      if(fd.eq.stdin)then
         read(fd,'(a)',advance='no',iostat=ios1,iomsg=iomsg)c1
      else
         call get_next_char(fd,c1,ios1)                                  ! get next character from buffered read from file
      endif
      if(ios1.eq.iostat_end)then                                         ! reached end of file so stop
         if(verbose)then
            if(nocode.and.nocomment)then
               io=stdout
            else
               io=stderr
            endif
            if(size(filenames).gt.1)then
               atleast=max(len(filenames),4)
               write(io,'(g0:,1x)',advance='no')[character(len=atleast) :: filename]
               write(io,'(g0:,1x,i8)',advance='no')' lines', ilines
               write(io,'(g0:,1x,i8,"b")',advance='no')' total ', icount
               write(io,'(g0:,1x,i8,"b")',advance='no')' comments', icount_comm
               write(io,'(g0:,1x,f5.2)',advance='no')' % comments', real(icount_comm*100)/real(icount)
               write(io,'(g0:,1x,i8,"b")',advance='no')' case changes', idiff
               write(io,*)
            else
               write(io,'(g0:,1x)',advance='no')filename
               write(io,'(g0:,1x,i0)',advance='no')' lines', ilines
               write(io,'(g0:,1x,i0,"b")',advance='no')' total ', icount
               write(io,'(g0:,1x,i0,"b")',advance='no')' comments', icount_comm
               write(io,'(g0:,1x,f5.2)',advance='no')' % comments', real(icount_comm*100)/real(icount)
               write(io,'(g0:,1x,i0,"b")',advance='no')' case changes', idiff
               write(io,*)
            endif
         endif
         cycle FILES
      elseif(fd.eq.stdin.and.iostat_eor.eq.ios1)then
         c1=nl
      elseif(ios1.ne.0)then                                             ! error or end of file
         write(stderr,*)'*flower* ERROR: EOF or error on '//filename//' before end of '//filename
         cycle FILES
      endif
      iposition=iposition+1
      icount=icount+1                                                    ! increment count of characters read
      select case(c1)
      case('#','$')
         if(iposition.eq.1)then  ! will still not save macros, but as a kluge treat these lines as comments (?)
            incomment=.true.
         endif
      case('!')
         if(any([insingle,indouble]))then
            continue
          else
            incomment=.true.
         endif
      case('&')
      case('"')
         if(any([incomment,insingle]))then
            continue
         elseif(indouble)then
            indouble=.false.
         else
            indouble=.true.
         endif
      case("'")
         if(any([incomment,indouble]))then
            continue
         elseif(insingle)then
            insingle=.false.
         else
            insingle=.true.
         endif
      case(nl)
         iposition=0
         ilines=ilines+1                             ! increment count of newline characters in file
         if(previous.ne.'&')then
            incomment=.false.
            insingle=.false.
            indouble=.false.
         elseif(incomment)then
            incomment=.false.
         endif
      case('a':'z')
         if(.not.tolower)then
            if(.not.any([incomment,insingle,indouble]))then
               c1=char(iachar(c1)-32)
               idiff=idiff+1
            endif
         endif
      case('A':'Z')
         if(tolower)then
            if(.not.any([incomment,insingle,indouble]))then
               c1=char(iachar(c1)+32)
               idiff=idiff+1
            endif
         endif
      case default
      end select

      if(incomment)then
         icount_comm=icount_comm+1
      endif

      if (c1.eq.nl)then                     ! remove adjacent blank lines
         if(nocomment.and.nocode)then
    continue
         else
            if(nocode) then
       outline=outline//' '
               outline=trim(outline(max(1,verify(outline,'!')):)) ! trim leading exclamations
            endif
            if(incomment)then
               continue
            elseif(previous.eq.'&'.and.nocont)then
               advance='no'
               outline=trim(outline)
               ilen=len(outline)
               if(outline(ilen:ilen).eq.'&')then
                  outline=outline(:ilen-1)
               endif
            else
               advance='yes'
            endif
            if(outline.eq.''.or.adjustl(outline).eq.achar(10))then  ! if a blank line output if previous line not a blank line
               if(ifblank)then
                  continue
               else
                  write(*,'(a)',advance=advance)
               endif
               ifblank=.true.
            else                                                    ! print a non-blank line
               outlinel=adjustl(outline)
               if(index(outlinel,'&').eq.1.and.nocont)then
                 outline='#'//outlinel(2:)
               endif
               write(*,'(a)',advance=advance)outline
               ifblank=.false.
            endif
         endif
         outline=''
      elseif(nocomment.and.incomment)then
         continue
      elseif(nocode.and..not.incomment)then
         continue
      elseif(incomment.or.insingle.or.indouble)then
         outline=outline//c1
      elseif(c1.eq.'&'.and.nocont)then
         continue
      else
         outline=outline//c1
      endif

      if(c1.ne.' ')then
         if(.not.any([incomment]).and.previous.ne.' ')then
            previous=c1
         endif
      endif

   enddo ONE_CHAR_AT_A_TIME

   if(idiff.ne.0)then
      stop 1
   else
      stop 0
   endif

enddo FILES

contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'flower(1f) - [DEVELOPER] change case of free-format Fortran file',&
'             or remove code or remove comments',&
'             (LICENSE:PD)',&
'SYNOPSIS',&
'   flower [ --stat|[ [--nocomment|--nocode] [--toupper]|[--verbose] ]',&
'          FILENAMES(s) ]|[--help|--version|--usage]',&
'DESCRIPTION',&
'flower(1) depends on the input being plain standard free-format Fortran',&
'so the output should be carefully checked.',&
'',&
'1. flower(1) will convert free-format Fortran code to lowercase.',&
'   It can also convert the code to uppercase. In each case comments and',&
'   quoted text are left as-is.',&
'',&
'2. flower(3f) can additionally generate simple statistics about',&
'   what percentage of the code is comments.',&
'',&
'3. flower(1) can strip comments from the code.',&
'',&
'4. Alternatively, the code can be removed so the comments can be used',&
'   for documentation or run through utilities like spell checkers.',&
'',&
'',&
'This is a basic program that writes its results to stdout and does not',&
'recognize Hollerith strings and preprocessor directives as special cases.',&
'',&
'Tabs should be expanded before processing the file.',&
'',&
'OPTIONS',&
'     FILENAME     Fortran source file which is to be converted to lowercase',&
'     --nocomment  remove comment characters',&
'     --nocode     remove code characters',&
'     --toupper    convert code characters to uppercase instead of lowercase',&
'     --verbose    turn on verbose mode including file statistics. Note',&
'                  that if --nocomment and --nocode are selected --verbose',&
'                  is implied.',&
'     --stat       is the same as --nocomment --nocode --verbose, meaning',&
'                  no other output than file statistics will be produced.',&
'                  If present, --nocomment, --nocode, and --verbose are',&
'                  ignored.',&
'',&
'     --help       display help text and exit',&
'     --version    display version text and exit',&
'',&
'EXAMPLES',&
'   Typical usage',&
'',&
'     # convert all code to lowercase',&
'     flower sample.f90 > sample_new.f90',&
'',&
'     # show stats for files measuring percent of comments',&
'     flower --stat *.f90 *.F90',&
'',&
'     # check spelling on comments',&
'     flower --nocode *.f90 *.F90|spell',&
'',&
'     # grep code ignoring comments',&
'     flower --nocomment *.f90 *.F90|grep -iw contains',&
'',&
'EXIT STATUS',&
'   The following exit values are returned:',&
'',&
'       0  no differences were found',&
'       1  differences were found',&
'                                                                         ',&
'                      _(_)_                          wWWWw   _           ',&
'          @@@@       (_)@(_)   vVVVv     _     @@@@  (___) _(_)_         ',&
'         @@()@@ wWWWw  (_)\    (___)   _(_)_  @@()@@   Y  (_)@(_)        ',&
'          @@@@  (___)     `|/    Y    (_)@(_)  @@@@   \|/   (_)\         ',&
'           /      Y       \|    \|/    /(_)    \|      |/      |         ',&
'        \ |     \ |/       | / \ | /  \|/       |/    \|      \|/        ',&
'       jgs|//   \\|///  \\\|//\\\|/// \|///  \\\|//  \\|//  \\\|//       ',&
'       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^     ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        flower(1)>',&
'@(#)DESCRIPTION:    convert free-format Fortran source to lowercase>',&
'@(#)VERSION:        1.0-20171126>',&
'@(#)AUTHOR:         John S. Urban>',&
'']
end subroutine setup

end program flower
 
