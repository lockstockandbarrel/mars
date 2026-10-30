! 2026-10-30
! git clone https://github.com/jacobwilliams/fformat
!
! MIT License
! 
! Copyright (c) 2026 Jacob Williams
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!>>>>> ././src/ffmt_fs.f90
module ffmt_fs
!! Portable file system access that standard Fortran lacks: listing a directory and telling
!! directories from files. Written in Fortran with interfaces to `opendir`, `readdir` and `closedir`,
!! which POSIX systems and MinGW-w64 (gfortran on Windows) provide.
!!
!! The layout of `struct dirent` differs between systems, and gfortran's preprocessor does not say
!! which system it is compiling for, so the layout is found at run time: the offset of `d_name` is
!! where the `.` and `..` entries, which every listing has, show their names. The known layouts
!! are Linux (offset 19), macOS (21), FreeBSD (24), and MinGW or macOS with 32-bit inodes (8).
!! Where the layout has a `d_type` field it is used; otherwise a directory is recognized by opening it.
use, intrinsic :: iso_c_binding
implicit none
private

public :: list_directory, is_directory, join_path, current_directory, absolute_path, parent_directory

integer, parameter, public :: ENTRY_FILE = 1      !! a regular file (or a link to one)
integer, parameter, public :: ENTRY_DIR = 2       !! a directory
integer, parameter, public :: ENTRY_OTHER = 3     !! a link to a directory, or anything else

type, public :: dir_entry_t
    !! One entry of a directory listing.
    character(len=:), allocatable :: name   !! the entry's name, without the directory
    integer :: kind = ENTRY_OTHER           !! `ENTRY_FILE`, `ENTRY_DIR` or `ENTRY_OTHER`
end type dir_entry_t

integer, parameter :: NAME_OFFSETS(*) = [19, 21, 24, 8]   !! offsets of `d_name` to try
integer, parameter :: TYPE_OFFSETS(*) = [18, 20, 20, -1]  !! matching offsets of `d_type` (-1: none)
integer, parameter :: DT_UNKNOWN = 0, DT_DIR = 4, DT_REG = 8, DT_LNK = 10
integer, parameter :: MAX_NAME = 1024

integer, save :: layout = 0      !! index into NAME_OFFSETS once known

interface
    function c_opendir(name) bind(c, name='opendir') result(dir)
        !! C `opendir`: open directory `name` for reading; a null pointer if it cannot be opened.
        import :: c_char, c_ptr
        character(kind=c_char), intent(in) :: name(*)   !! the path, NUL-terminated
        type(c_ptr) :: dir
    end function c_opendir
    function c_readdir(dir) bind(c, name='readdir') result(entry)
        !! C `readdir`: the next `struct dirent` of `dir`, or a null pointer at the end.
        import :: c_ptr
        type(c_ptr), value :: dir                       !! a directory opened by `c_opendir`
        type(c_ptr) :: entry
    end function c_readdir
    function c_closedir(dir) bind(c, name='closedir') result(status)
        !! C `closedir`: close `dir`; 0 on success.
        import :: c_ptr, c_int
        type(c_ptr), value :: dir                       !! a directory opened by `c_opendir`
        integer(c_int) :: status
    end function c_closedir
    function c_getcwd(buf, size) bind(c, name='getcwd') result(p)
        !! C `getcwd`: write the current directory into `buf`, NUL-terminated; a null pointer if
        !! it does not fit or cannot be determined.
        import :: c_char, c_ptr, c_size_t
        character(kind=c_char), intent(out) :: buf(*)   !! receives the path
        integer(c_size_t), value :: size                !! size of `buf` in bytes
        type(c_ptr) :: p
    end function c_getcwd
end interface

contains

subroutine list_directory(path, entries, stat)
    !! The entries of directory `path`, without `.` and `..`, in the order the system returns them.
    !! Links to directories are reported as `ENTRY_OTHER`, so a walk does not follow them. `stat` is
    !! nonzero if the directory cannot be read, or its entries cannot be decoded.
    character(len=*), intent(in) :: path   !! the directory
    type(dir_entry_t), allocatable, intent(out) :: entries(:)   !! the entries; empty if `stat` is nonzero
    integer, intent(out) :: stat        !! 0, or 1 if the listing failed
    type(c_ptr) :: dir, p
    type(dir_entry_t) :: e
    integer :: dtype, status

    allocate (entries(0))
    stat = 1
    if (layout == 0) call find_layout(path)
    if (layout == 0) return
    dir = c_opendir(c_string(path))
    if (.not. c_associated(dir)) return
    stat = 0
    do
        p = c_readdir(dir)
        if (.not. c_associated(p)) exit
        e%name = entry_name(p)
        if (e%name == '.' .or. e%name == '..') cycle
        dtype = DT_UNKNOWN
        if (TYPE_OFFSETS(layout) >= 0) dtype = byte_at(p, TYPE_OFFSETS(layout))
        select case (dtype)
        case (DT_DIR)
            e%kind = ENTRY_DIR
        case (DT_REG)
            e%kind = ENTRY_FILE
        case (DT_LNK)
            e%kind = merge(ENTRY_OTHER, ENTRY_FILE, is_directory(join_path(path, e%name)))
        case (DT_UNKNOWN)
            e%kind = merge(ENTRY_DIR, ENTRY_FILE, is_directory(join_path(path, e%name)))
        case default
            e%kind = ENTRY_OTHER
        end select
        entries = [entries, e]
    end do
    status = c_closedir(dir)
end subroutine list_directory

subroutine find_layout(path)
    !! Find the offset of `d_name` from the `.` and `..` entries of directory `path`.
    character(len=*), intent(in) :: path   !! a directory to list
    type(c_ptr) :: dir, p
    logical :: dot(size(NAME_OFFSETS)), dotdot(size(NAME_OFFSETS))
    integer :: k, status

    dir = c_opendir(c_string(path))
    if (.not. c_associated(dir)) return
    dot = .false.
    dotdot = .false.
    do
        p = c_readdir(dir)
        if (.not. c_associated(p)) exit
        do k = 1, size(NAME_OFFSETS)
            if (byte_at(p, NAME_OFFSETS(k)) /= iachar('.')) cycle
            if (byte_at(p, NAME_OFFSETS(k) + 1) == 0) dot(k) = .true.
            if (byte_at(p, NAME_OFFSETS(k) + 1) == iachar('.') .and. byte_at(p, NAME_OFFSETS(k) + 2) == 0) &
                dotdot(k) = .true.
        end do
    end do
    status = c_closedir(dir)
    do k = 1, size(NAME_OFFSETS)
        if (dot(k) .and. dotdot(k)) then
            layout = k
            return
        end if
    end do
end subroutine find_layout

function entry_name(p) result(name)
    !! The NUL-terminated name in the directory entry at `p`.
    type(c_ptr), intent(in) :: p        !! a `struct dirent` returned by `c_readdir`, with the layout known
    character(len=:), allocatable :: name
    character(kind=c_char), pointer :: raw(:)
    integer :: n, start

    start = NAME_OFFSETS(layout)
    call c_f_pointer(p, raw, [start + MAX_NAME])
    n = 0
    do while (n < MAX_NAME)
        if (raw(start + n + 1) == c_null_char) exit
        n = n + 1
    end do
    allocate (character(len=n) :: name)
    name = transfer(raw(start + 1:start + n), name)
end function entry_name

integer function byte_at(p, offset) result(b)
    !! The byte at `offset` in the memory at `p`, as a number from 0 to 255.
    type(c_ptr), intent(in) :: p        !! start of the memory
    integer, intent(in) :: offset       !! in bytes, 0 for the first byte
    integer(c_int8_t), pointer :: raw(:)
    call c_f_pointer(p, raw, [offset + 1])
    b = iand(int(raw(offset + 1)), 255)
end function byte_at

logical function is_directory(path)
    !! True if `path` is a directory (or a link to one): if it can be opened as a directory.
    character(len=*), intent(in) :: path   !! the path to test
    type(c_ptr) :: dir
    integer :: status
    dir = c_opendir(c_string(path))
    is_directory = c_associated(dir)
    if (is_directory) status = c_closedir(dir)
end function is_directory

function current_directory() result(path)
    !! The current working directory, or `.` if it cannot be determined.
    character(len=:), allocatable :: path
    character(kind=c_char), allocatable :: buf(:)
    integer :: n
    allocate (buf(4096))
    if (.not. c_associated(c_getcwd(buf, int(size(buf), c_size_t)))) then
        path = '.'
        return
    end if
    n = 0
    do while (n < size(buf))
        if (buf(n + 1) == c_null_char) exit
        n = n + 1
    end do
    allocate (character(len=n) :: path)
    path = transfer(buf(1:n), path)
end function current_directory

function absolute_path(path) result(abs)
    !! `path` made absolute by prefixing the current directory, unless it already is
    !! (starts with `/` or `\`, or a drive letter such as `C:`).
    character(len=*), intent(in) :: path   !! a relative or absolute path
    character(len=:), allocatable :: abs
    if (is_absolute(path)) then
        abs = path
    else if (path == '.') then
        abs = current_directory()
    else
        abs = join_path(current_directory(), path)
    end if
end function absolute_path

pure logical function is_absolute(path)
    !! True if `path` starts with `/` or `\`, or has a drive letter such as `C:`.
    character(len=*), intent(in) :: path   !! the path to test
    is_absolute = .false.
    if (len(path) == 0) return
    is_absolute = path(1:1) == '/' .or. path(1:1) == '\'
    if (len(path) >= 2) is_absolute = is_absolute .or. path(2:2) == ':'
end function is_absolute

pure function parent_directory(path) result(parent)
    !! The directory containing `path` (`/a/b` for `/a/b/c`), or `''` if `path` has no parent.
    character(len=*), intent(in) :: path   !! a file or directory path
    character(len=:), allocatable :: parent
    integer :: n, k
    n = len(path)
    do while (n > 1)                         ! ignore trailing separators
        if (index('/\', path(n:n)) == 0) exit
        n = n - 1
    end do
    k = scan(path(1:n), '/\', back=.true.)
    if (k == 0 .or. n <= 1) then
        parent = ''
    else if (k == 1) then
        parent = path(1:1)                   ! the root
    else if (k == 3 .and. path(2:2) == ':') then
        parent = path(1:3)                   ! a drive root, `C:\`
    else
        parent = path(1:k - 1)
    end if
end function parent_directory

pure function join_path(dir, name) result(path)
    !! `dir/name`, without doubling a separator. A trailing `\` or `:` (Windows) also counts.
    character(len=*), intent(in) :: dir, name   !! the directory, and the name to append to it
    character(len=:), allocatable :: path
    if (len(dir) == 0) then
        path = name
    else if (index('/\:', dir(len(dir):len(dir))) > 0) then
        path = dir//name
    else
        path = dir//'/'//name
    end if
end function join_path

pure function c_string(s) result(c)
    !! `s` as a NUL-terminated C string.
    character(len=*), intent(in) :: s   !! the Fortran string
    character(kind=c_char) :: c(len(s) + 1)
    integer :: k
    do k = 1, len(s)
        c(k) = s(k:k)
    end do
    c(len(s) + 1) = c_null_char
end function c_string

end module ffmt_fs


!>>>>> ././src/ffmt_options.f90
module ffmt_options
!! Formatting options: all the settings, with their defaults, and the modes they take. The modules
!! that apply them ([[ffmt_whitespace]], [[ffmt_case]], [[ffmt_relational]], [[ffmt_layout]]) use
!! this module, not the other way round, so it depends on nothing.
implicit none
private

! layout of continuation lines (see [[ffmt_layout]])
integer, parameter, public :: CONT_ALIGN = 1    !! align continuation lines with the first item after the
                                                !! innermost open bracket, or after the first `=`
integer, parameter, public :: CONT_INDENT = 2   !! indent continuation lines by a fixed amount

! whitespace rule modes (see [[ffmt_whitespace]])
integer, parameter, public :: WS_KEEP = 0     !! leave the blanks as they are
integer, parameter, public :: WS_ADD = 1      !! at least one blank on each side
integer, parameter, public :: WS_REMOVE = 2   !! no blanks

! letter case modes (see [[ffmt_case]])
integer, parameter, public :: CASE_KEEP = 0     !! leave the case as it is
integer, parameter, public :: CASE_LOWER = 1    !! lowercase
integer, parameter, public :: CASE_UPPER = 2    !! uppercase

! spellings of relational operators (see [[ffmt_relational]])
integer, parameter, public :: REL_KEEP = 0      !! leave the spelling as it is
integer, parameter, public :: REL_SYMBOLS = 1   !! `.lt.` becomes `<`, ...
integer, parameter, public :: REL_LETTERS = 2   !! `<` becomes `.lt.`, ...

type, public :: whitespace_options_t
    !! Modes of the whitespace rules. The defaults follow the most common style in practice where it is
    !! clear-cut, and keep the rest.
    integer :: assignment = WS_ADD     !! `=` outside brackets: assignments, initializations, `do i = 1, n`
    integer :: keyword_args = WS_REMOVE !! `=` inside brackets: `f(x=1)`, `intent=`, `len=*`
    integer :: pointer = WS_ADD        !! `=>`: pointer assignment, renames, `associate`
    integer :: declarations = WS_ADD   !! `::`
    integer :: relational = WS_KEEP    !! `== /= < <= > >=` and `.eq.` etc.
    integer :: logical = WS_KEEP       !! `.and. .or. .eqv. .neqv.`
    integer :: plus_minus = WS_KEEP    !! binary `+ -`
    integer :: mult_div = WS_KEEP      !! binary `* /`
    integer :: power = WS_REMOVE       !! `**`
    integer :: concat = WS_REMOVE      !! `//`
    integer :: comma = WS_KEEP         !! `,`: with `WS_ADD`, no blank before and at least one after
    integer :: slice_colon = WS_REMOVE !! `:` inside brackets: `a(1:n)`
    integer :: percent = WS_REMOVE     !! `%`
    integer :: control_paren = WS_ADD  !! between `if`, `while`, `select case`, `where`, ... and `(`, and
                                       !! between `)` and a final `then`
    integer :: conditional = WS_ADD    !! `?` and `:` of a conditional expression `(c ? a : b)` (F2023)
    integer :: defined_operators = WS_KEEP !! binary user-defined operators: `a .plus. b`
end type whitespace_options_t

type, public :: case_options_t
    !! Letter case of keywords, dot operators and logical constants, intrinsic procedures, the constants
    !! of the intrinsic modules, and kinds.
    integer :: keywords = CASE_KEEP    !! `if`, `end do`, `integer`, `intent(in)`, ...
    integer :: operators = CASE_KEEP   !! `.and.`, `.eq.`, `.true.`, ...
    integer :: procedures = CASE_KEEP  !! intrinsic procedures where they are called: `size(a)`, `call cpu_time(t)`
    integer :: constants = CASE_KEEP   !! constants and types of `iso_fortran_env` and `iso_c_binding`:
                                       !! `output_unit`, `int64`, `c_int`, `c_ptr`
    integer :: types = CASE_KEEP       !! kinds: `kind=` values and `real(dp)` in declarations, literal kind
                                       !! suffixes `2_int64` and prefixes `ck_'a'`, exponent letters `1.0e-3`
end type case_options_t

type, public :: format_options_t
    !! All options that control formatting. The defaults give 4-space indentation with the bodies
    !! of modules and programs not indented, and aligned continuation lines.
    integer :: indent = 4                   !! spaces per indentation level
    integer :: base_indent = 0              !! columns added to the indentation of every line, so that even
                                            !! program units are indented (4 for code that starts in column 5)
    logical :: indent_units = .false.       !! indent the bodies of modules, submodules and main programs
    logical :: indent_procedures = .true.   !! indent the bodies of subroutines and functions
    logical :: indent_fypp = .false.        !! indent the code inside fypp blocks (`#:if`, `#:for`, `#:def`, ...),
                                            !! and the fypp directive lines with the code around them
    integer :: continuation = CONT_ALIGN    !! layout of continuation lines (`CONT_ALIGN` or `CONT_INDENT`)
    integer :: continuation_indent = 4      !! extra indentation of continuation lines, used with `CONT_INDENT`
                                            !! and when there is nothing to align with
    logical :: indent_comments = .true.     !! re-indent comment lines like the code around them
    logical :: keep_column1_comments = .false. !! leave comment lines that start in column 1 there (with
                                            !! `indent_comments`)
    integer :: line_length = 132            !! maximum line length (0 for no limit). Alignment that would make a
                                            !! line longer falls back to a smaller indentation.
    logical :: split_lines = .true.         !! split code lines that are longer than `line_length`
    integer :: relational = REL_KEEP        !! spelling of relational operators: `REL_KEEP`, `REL_SYMBOLS`
                                            !! (`.lt.` becomes `<`) or `REL_LETTERS` (`<` becomes `.lt.`)
    logical :: sort_use = .false.           !! sort the `use` statements of each group by module name
    logical :: sort_use_only = .false.      !! sort the names in `only:` lists
    integer :: comment_spacing = -1         !! blanks before a trailing comment, or -1 to keep them; comments
                                            !! aligned with one on the line before or after are kept
    type(whitespace_options_t) :: spaces    !! whitespace rules within lines
    type(case_options_t) :: letter_case     !! letter case of keywords and operators
end type format_options_t

end module ffmt_options


!>>>>> ././src/ffmt_strings.f90
module ffmt_strings
!! String helpers.
implicit none
private

public :: utf8_width, to_s, lower_char, lowercase, make_lowercase

type, public :: string_t
    !! A variable-length string, for arrays of strings of different lengths.
    character(len=:), allocatable :: s      !! the string
end type string_t

type, public :: string_builder_t
    !! Builds a string by appending pieces in amortized constant time.
    private
    character(len=:), allocatable :: buf    !! storage; only `buf(1:n)` is in use
    integer :: n = 0                        !! length of the string so far
contains
    procedure :: append => builder_append
    procedure :: get => builder_get
    procedure :: length => builder_length
end type string_builder_t

contains

pure integer function utf8_width(text) result(w)
    !! Number of characters (code points) in UTF-8 `text`: bytes that are not continuation bytes.
    character(len=*), intent(in) :: text   !! UTF-8 bytes
    integer :: k, c
    w = 0
    do k = 1, len(text)
        c = iand(ichar(text(k:k)), 255)
        if (c < 128 .or. c >= 192) w = w + 1
    end do
end function utf8_width

pure function to_s(i) result(s)
    !! Integer to string, without padding. Digits are built by hand: an internal `write` costs
    !! far more, and this is called for every token by the safety check.
    integer, intent(in) :: i            !! the number
    character(len=:), allocatable :: s
    character(len=12) :: buf
    integer :: k, n

    n = i                                   ! not abs(i), which overflows for -huge(i) - 1
    k = len(buf) + 1
    do
        k = k - 1
        buf(k:k) = achar(iachar('0') + abs(mod(n, 10)))
        n = n/10
        if (n == 0) exit
    end do
    if (i < 0) then
        k = k - 1
        buf(k:k) = '-'
    end if
    s = buf(k:)
end function to_s

elemental character function lower_char(c)
    !! `c` in lowercase (ASCII letters only; other bytes, including UTF-8, are unchanged).
    character, intent(in) :: c   !! the character
    if (c >= 'A' .and. c <= 'Z') then
        lower_char = achar(iachar(c) + 32)
    else
        lower_char = c
    end if
end function lower_char

pure function lowercase(s) result(l)
    !! `s` in lowercase (see [[lower_char]]).
    character(len=*), intent(in) :: s   !! the text
    character(len=len(s)) :: l
    integer :: k
    do k = 1, len(s)
        l(k:k) = lower_char(s(k:k))
    end do
end function lowercase

pure subroutine make_lowercase(s)
    !! Turn `s` into lowercase in place, without a temporary copy (see [[lower_char]]).
    character(len=*), intent(inout) :: s   !! the text
    integer :: k
    do k = 1, len(s)
        if (s(k:k) >= 'A' .and. s(k:k) <= 'Z') s(k:k) = achar(iachar(s(k:k)) + 32)
    end do
end subroutine make_lowercase

pure subroutine builder_append(self, str)
    !! Append `str` to the string, doubling the storage when it is full.
    class(string_builder_t), intent(inout) :: self   !! the builder
    character(len=*), intent(in) :: str   !! the piece to append
    character(len=:), allocatable :: tmp

    if (len(str) == 0) return
    if (.not. allocated(self%buf)) allocate (character(len=max(256, len(str))) :: self%buf)
    if (self%n + len(str) > len(self%buf)) then
        allocate (character(len=max(2*len(self%buf), self%n + len(str))) :: tmp)
        tmp(1:self%n) = self%buf(1:self%n)
        call move_alloc(tmp, self%buf)
    end if
    self%buf(self%n + 1:self%n + len(str)) = str
    self%n = self%n + len(str)
end subroutine builder_append

pure function builder_get(self) result(str)
    !! The string built so far.
    class(string_builder_t), intent(in) :: self   !! the builder
    character(len=:), allocatable :: str
    if (allocated(self%buf)) then
        str = self%buf(1:self%n)
    else
        str = ''
    end if
end function builder_get

pure integer function builder_length(self) result(n)
    !! Length of the string built so far.
    class(string_builder_t), intent(in) :: self   !! the builder
    n = self%n
end function builder_length

end module ffmt_strings


!>>>>> build/dependencies/fregex/src/fregex_charset.f90
module fregex_charset
!! Sets of byte values (0-255), used for character classes such as `[a-z]` and `\w`.
use iso_fortran_env, only: int64
implicit none
private

public :: digit_set, word_set, space_set, full_set

type, public :: charset_t
    !! A 256-bit set of byte values.
    integer(int64) :: bits(0:3) = 0_int64
contains
    procedure :: add => charset_add
    procedure :: add_range => charset_add_range
    procedure :: add_set => charset_add_set
    procedure :: has => charset_has
    procedure :: negate => charset_negate
    procedure :: fold_case => charset_fold_case
    procedure :: table => charset_table
end type charset_t

contains

pure subroutine charset_add(self, c)
    class(charset_t), intent(inout) :: self
    integer, intent(in) :: c
    if (c < 0 .or. c > 255) return
    self%bits(c/64) = ibset(self%bits(c/64), mod(c, 64))
end subroutine charset_add

pure subroutine charset_add_range(self, lo, hi)
    class(charset_t), intent(inout) :: self
    integer, intent(in) :: lo, hi
    integer :: c
    do c = lo, hi
        call self%add(c)
    end do
end subroutine charset_add_range

pure subroutine charset_add_set(self, other)
    class(charset_t), intent(inout) :: self
    type(charset_t), intent(in) :: other
    self%bits = ior(self%bits, other%bits)
end subroutine charset_add_set

pure logical function charset_has(self, c) result(has)
    !! True if byte `c` is in the set. Out-of-range values (e.g. -1 for "end of string") are never members.
    class(charset_t), intent(in) :: self
    integer, intent(in) :: c
    if (c < 0 .or. c > 255) then
        has = .false.
    else
        has = btest(self%bits(c/64), mod(c, 64))
    end if
end function charset_has

pure function charset_table(self) result(table)
    !! The set as a lookup table indexed by byte value. Index -1 ("end of string") is never a member.
    class(charset_t), intent(in) :: self
    logical :: table(-1:255)
    integer :: c
    table(-1) = .false.
    do c = 0, 255
        table(c) = btest(self%bits(c/64), mod(c, 64))
    end do
end function charset_table

pure subroutine charset_negate(self)
    class(charset_t), intent(inout) :: self
    self%bits = not(self%bits)
end subroutine charset_negate

pure subroutine charset_fold_case(self)
    !! Add the other ASCII case of every letter in the set.
    class(charset_t), intent(inout) :: self
    integer :: c
    do c = iachar('a'), iachar('z')
        if (self%has(c) .or. self%has(c - 32)) then
            call self%add(c)
            call self%add(c - 32)
        end if
    end do
end subroutine charset_fold_case

pure function digit_set() result(set)
    !! `\d`: `[0-9]`
    type(charset_t) :: set
    call set%add_range(iachar('0'), iachar('9'))
end function digit_set

pure function word_set() result(set)
    !! `\w`: `[A-Za-z0-9_]`
    type(charset_t) :: set
    call set%add_range(iachar('0'), iachar('9'))
    call set%add_range(iachar('A'), iachar('Z'))
    call set%add_range(iachar('a'), iachar('z'))
    call set%add(iachar('_'))
end function word_set

pure function space_set() result(set)
    !! `\s`: `[ \t\n\r\f\v]`
    type(charset_t) :: set
    call set%add_range(9, 13)
    call set%add(iachar(' '))
end function space_set

pure function full_set() result(set)
    type(charset_t) :: set
    set%bits = not(0_int64)
end function full_set

end module fregex_charset


!>>>>> build/dependencies/fregex/src/fregex_string.f90
module fregex_string
!! String helpers shared by the fregex modules.
implicit none
private

public :: string_builder_t
public :: byte_at, to_lower_code, is_word_code, is_alpha_code, is_digit_code, to_str

type, public :: regex_string_t
    !! A variable-length string, so that arrays of strings can hold different lengths.
    character(len=:), allocatable :: s
end type regex_string_t

type :: string_builder_t
    !! Builds a string by appending pieces in amortized constant time.
    private
    character(len=:), allocatable :: buf
    integer :: n = 0
contains
    procedure :: append => builder_append
    procedure :: get => builder_get
    procedure :: clear => builder_clear
end type string_builder_t

contains

pure integer function byte_at(s, i) result(c)
    !! Byte value (0-255) of `s(i:i)`, or -1 if `i` is outside the string.
    character(len=*), intent(in) :: s
    integer, intent(in) :: i
    if (i < 1 .or. i > len(s)) then
        c = -1
    else
        c = iand(ichar(s(i:i)), 255)
    end if
end function byte_at

elemental integer function to_lower_code(c) result(l)
    !! ASCII lowercase of a byte value; other values are returned unchanged.
    integer, intent(in) :: c
    if (c >= iachar('A') .and. c <= iachar('Z')) then
        l = c + 32
    else
        l = c
    end if
end function to_lower_code

elemental logical function is_alpha_code(c)
    integer, intent(in) :: c
    is_alpha_code = (c >= iachar('a') .and. c <= iachar('z')) .or. (c >= iachar('A') .and. c <= iachar('Z'))
end function is_alpha_code

elemental logical function is_digit_code(c)
    integer, intent(in) :: c
    is_digit_code = c >= iachar('0') .and. c <= iachar('9')
end function is_digit_code

elemental logical function is_word_code(c)
    !! True for ASCII word characters `[A-Za-z0-9_]`.
    integer, intent(in) :: c
    is_word_code = is_alpha_code(c) .or. is_digit_code(c) .or. c == iachar('_')
end function is_word_code

pure function to_str(i) result(str)
    !! Integer to string, without padding.
    integer, intent(in) :: i
    character(len=:), allocatable :: str
    character(len=16) :: buf
    write (buf, '(i0)') i
    str = trim(buf)
end function to_str

pure subroutine builder_append(self, str)
    class(string_builder_t), intent(inout) :: self
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: tmp

    if (len(str) == 0) return
    if (.not. allocated(self%buf)) allocate (character(len=max(64, len(str))) :: self%buf)
    if (self%n + len(str) > len(self%buf)) then
        allocate (character(len=max(2*len(self%buf), self%n + len(str))) :: tmp)
        tmp(1:self%n) = self%buf(1:self%n)
        call move_alloc(tmp, self%buf)
    end if
    self%buf(self%n + 1:self%n + len(str)) = str
    self%n = self%n + len(str)
end subroutine builder_append

pure subroutine builder_clear(self)
    class(string_builder_t), intent(inout) :: self
    self%n = 0
end subroutine builder_clear

pure function builder_get(self) result(str)
    class(string_builder_t), intent(in) :: self
    character(len=:), allocatable :: str
    if (allocated(self%buf)) then
        str = self%buf(1:self%n)
    else
        str = ''
    end if
end function builder_get

end module fregex_string


!>>>>> build/dependencies/toml-f/src/tomlf/constants.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module tomlf_constants
   use, intrinsic :: iso_fortran_env, only : output_unit
   implicit none
   private

   !> Single precision real numbers
   integer, public, parameter :: tf_sp = selected_real_kind(6)

   !> Double precision real numbers
   integer, public, parameter :: tf_dp = selected_real_kind(15)

   !> Char length for integers
   integer, public, parameter :: tf_i1 = selected_int_kind(2)

   !> Short length for integers
   integer, public, parameter :: tf_i2 = selected_int_kind(4)

   !> Length of default integers
   integer, public, parameter :: tf_i4 = selected_int_kind(9)

   !> Long length for integers
   integer, public, parameter :: tf_i8 = selected_int_kind(18)


   !> Default character kind
   integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

   !> Default float precision, IEEE 754 binary64 values expected
   integer, public, parameter :: tfr = tf_dp

   !> Default integer precision, 64 bit (signed long) range expected
   integer, public, parameter :: tfi = tf_i8

   !> Default output channel
   integer, public, parameter :: tfout = output_unit


   !> Possible escape characters in TOML
   type :: enum_escape

      !> Backslash is used to escape other characters
      character(kind=tfc, len=1) :: backslash = achar(92, kind=tfc)

      !> Double quotes signal strings with escape characters enabled
      character(kind=tfc, len=1) :: dquote = tfc_'"'

      !> Single quotes signal strings without escape characters enabled
      character(kind=tfc, len=1) :: squote = tfc_''''

      !> Newline character
      character(kind=tfc, len=1) :: newline = achar(10, kind=tfc)

      !> Formfeed character is allowed in strings
      character(kind=tfc, len=1) :: formfeed = achar(12, kind=tfc)

      !> Carriage return is allowed as part of the newline and in strings
      character(kind=tfc, len=1) :: carriage_return = achar(13, kind=tfc)

      !> Backspace is allowed in strings
      character(kind=tfc, len=1) :: bspace = achar(8, kind=tfc)

      !> Tabulators are allowed as whitespace and in strings
      character(kind=tfc, len=1) :: tabulator = achar(9, kind=tfc)

   end type enum_escape

   !> Actual enumerator with TOML escape characters
   type(enum_escape), public, parameter :: toml_escape = enum_escape()


   !> Possible kinds of TOML values in key-value pairs
   type :: enum_type

      !> Invalid type
      integer :: invalid = 100

      !> String type
      integer :: string = 101

      !> Boolean type
      integer :: boolean = 102

      !> Integer type
      integer :: int = 103

      !> Float type
      integer :: float = 104

      !> Datetime type
      integer :: datetime = 105

   end type enum_type

   !> Actual enumerator with TOML value types
   type(enum_type), public, parameter :: toml_type = enum_type()


   !> Single quotes denote literal strings
   character(kind=tfc, len=*), public, parameter :: TOML_SQUOTE = "'"
   !> Double quotes denote strings (with escape character possible)
   character(kind=tfc, len=*), public, parameter :: TOML_DQUOTE = '"'
   character(kind=tfc, len=*), public, parameter :: TOML_NEWLINE = new_line('a') ! \n
   character(kind=tfc, len=*), public, parameter :: TOML_TABULATOR = achar(9) ! \t
   character(kind=tfc, len=*), public, parameter :: TOML_FORMFEED = achar(12) ! \f
   character(kind=tfc, len=*), public, parameter :: TOML_CARRIAGE_RETURN = achar(13) ! \r
   character(kind=tfc, len=*), public, parameter :: TOML_BACKSPACE = achar(8) ! \b
   character(kind=tfc, len=*), public, parameter :: TOML_ESC = achar(27) ! \e
   character(kind=tfc, len=*), public, parameter :: TOML_LOWERCASE = &
      & 'abcdefghijklmnopqrstuvwxyz'
   character(kind=tfc, len=*), public, parameter :: TOML_UPPERCASE = &
      & 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(kind=tfc, len=*), public, parameter :: TOML_LETTERS = &
      & TOML_LOWERCASE//TOML_UPPERCASE
   !> Whitespace in TOML are blanks and tabs.
   character(kind=tfc, len=*), public, parameter :: TOML_WHITESPACE = &
      & ' '//toml_escape%tabulator
   character(kind=tfc, len=*), public, parameter :: TOML_DIGITS = '0123456789'
   character(kind=tfc, len=*), public, parameter :: TOML_BINDIGITS = &
      & '01'
   character(kind=tfc, len=*), public, parameter :: TOML_OCTDIGITS = &
      & '01234567'
   character(kind=tfc, len=*), public, parameter :: TOML_HEXDIGITS = &
      & '0123456789ABCDEFabcdef'
   character(kind=tfc, len=*), public, parameter :: TOML_TIMESTAMP = &
      & TOML_DIGITS//'.:+-T Zz'
   !> Allowed characters in TOML bare keys.
   character(kind=tfc, len=*), public, parameter :: TOML_BAREKEY = &
      & TOML_LETTERS//TOML_DIGITS//'_-'
   character(kind=tfc, len=*), public, parameter :: TOML_LITERALS = &
      & TOML_LETTERS//TOML_DIGITS//'_-+.'

end module tomlf_constants


!>>>>> build/dependencies/toml-f/src/tomlf/version.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Version information on TOML-Fortran
module tomlf_version
   implicit none
   private

   public :: get_tomlf_version
   public :: tomlf_version_string, tomlf_version_compact


   !> String representation of the TOML-Fortran version
   character(len=*), parameter :: tomlf_version_string = "0.5.2"

   !> Major version number of the above TOML-Fortran version
   integer, parameter :: tomlf_major = 0

   !> Minor version number of the above TOML-Fortran version
   integer, parameter :: tomlf_minor = 5

   !> Patch version number of the above TOML-Fortran version
   integer, parameter :: tomlf_patch = 2

   !> Compact numeric representation of the TOML-Fortran version
   integer, parameter :: tomlf_version_compact = &
      & tomlf_major*10000 + tomlf_minor*100 + tomlf_patch


contains


!> Getter function to retrieve TOML-Fortran version
subroutine get_tomlf_version(major, minor, patch, string)

   !> Major version number of the TOML-Fortran version
   integer, intent(out), optional :: major

   !> Minor version number of the TOML-Fortran version
   integer, intent(out), optional :: minor

   !> Patch version number of the TOML-Fortran version
   integer, intent(out), optional :: patch

   !> String representation of the TOML-Fortran version
   character(len=:), allocatable, intent(out), optional :: string

   if (present(major)) then
      major = tomlf_major
   end if
   if (present(minor)) then
      minor = tomlf_minor
   end if
   if (present(patch)) then
      patch = tomlf_patch
   end if
   if (present(string)) then
      string = tomlf_version_string
   end if

end subroutine get_tomlf_version


end module tomlf_version


!>>>>> build/dependencies/toml-f/src/tomlf/de/token.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Provides a definition for a token
module tomlf_de_token
   implicit none
   private

   public :: toml_token, stringify, token_kind, resize


   !> Possible token kinds
   type :: enum_token
      !> Invalid token found
      integer :: invalid = -1
      !> End of file
      integer :: eof = -2
      !> Unclosed group from inline table or array
      integer :: unclosed = -3
      !> Whitespace (space, tab)
      integer :: whitespace = 0
      !> Newline character (\r\n, \n)
      integer :: newline = 1
      !> Comments (#)
      integer :: comment = 2
      !> Separator in table path (.)
      integer :: dot = 3
      !> Separator in inline arrays and inline tables (,)
      integer :: comma = 4
      !> Separator in key-value pairs (=)
      integer :: equal = 5
      !> Beginning of an inline table ({)
      integer :: lbrace = 6
      !> End of an inline table (})
      integer :: rbrace = 7
      !> Beginning of an inline array or table header ([)
      integer :: lbracket = 8
      !> End of an inline array or table header (])
      integer :: rbracket = 9
      !> String literal
      integer :: string = 10
      !> String literal
      integer :: mstring = 11
      !> String literal
      integer :: literal = 12
      !> String literal
      integer :: mliteral = 13
      !> String literal
      integer :: keypath = 14
      !> Floating point value
      integer :: float = 15
      !> Integer value
      integer :: int = 16
      !> Boolean value
      integer :: bool = 17
      !> Datetime value
      integer :: datetime = 18
      !> Absence of value
      integer :: nil = 19
   end type enum_token

   !> Actual enumerator for token kinds
   type(enum_token), parameter :: token_kind = enum_token()

   !> Token containing
   type :: toml_token
      !> Kind of token
      integer :: kind = token_kind%newline
      !> Starting position of the token in character stream
      integer :: first = 0
      !> Last position of the token in character stream
      integer :: last = 0
      !> Identifier for the chunk index in case of buffered reading
      integer :: chunk = 0
   end type toml_token

   !> Reallocate a list of tokens
   interface resize
      module procedure :: resize_token
   end interface

contains

!> Reallocate list of tokens
pure subroutine resize_token(var, n)
   !> Instance of the array to be resized
   type(toml_token), allocatable, intent(inout) :: var(:)
   !> Dimension of the final array size
   integer, intent(in), optional :: n

   type(toml_token), allocatable :: tmp(:)
   integer :: this_size, new_size
   integer, parameter :: initial_size = 8

   if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
   else
      this_size = initial_size
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(var(new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
   end if

end subroutine resize_token

!> Represent a token as string
pure function stringify(token) result(str)
   !> Token to represent as string
   type(toml_token), intent(in) :: token
   !> String representation of token
   character(len=:), allocatable :: str

   select case(token%kind)
   case default; str = "unknown"
   case(token_kind%invalid); str = "invalid sequence"
   case(token_kind%eof); str = "end of file"
   case(token_kind%unclosed); str = "unclosed group"
   case(token_kind%whitespace); str = "whitespace"
   case(token_kind%comment); str = "comment"
   case(token_kind%newline); str = "newline"
   case(token_kind%dot); str = "dot"
   case(token_kind%comma); str = "comma"
   case(token_kind%equal); str = "equal"
   case(token_kind%lbrace); str = "opening brace"
   case(token_kind%rbrace); str = "closing brace"
   case(token_kind%lbracket); str = "opening bracket"
   case(token_kind%rbracket); str = "closing bracket"
   case(token_kind%string); str = "string"
   case(token_kind%mstring); str = "multiline string"
   case(token_kind%literal); str = "literal"
   case(token_kind%mliteral); str = "multiline-literal"
   case(token_kind%keypath); str = "keypath"
   case(token_kind%int); str = "integer"
   case(token_kind%float); str = "float"
   case(token_kind%bool); str = "bool"
   case(token_kind%datetime); str = "datetime"
   case(token_kind%nil); str = "nil"
   end select
end function stringify

end module tomlf_de_token


!>>>>> ././src/ffmt_diff.f90
module ffmt_diff
!! Unified diffs of two texts, for `--diff`.
!!
!! Lines are matched with a longest common subsequence computed by Hirschberg's algorithm, which
!! needs memory linear in the number of lines, so a diff of a file whose every line changed is
!! still cheap. Lines are compared as integers after hashing.
use, intrinsic :: iso_fortran_env, only: int64
use ffmt_strings, only: string_t, string_builder_t, to_s
implicit none
private

public :: unified_diff

integer, parameter :: OP_EQUAL = 0, OP_DELETE = 1, OP_INSERT = 2

type :: edit_t
    !! The edits that turn the lines of one text into those of the other, in order.
    integer, allocatable :: op(:)   !! OP_EQUAL, OP_DELETE (line of a) or OP_INSERT (line of b)
    integer :: n = 0                !! number of edits in `op`
end type edit_t

contains

function unified_diff(a, b, name, context) result(diff)
    !! The changes from `a` to `b` as a unified diff with `context` lines of context (default 3),
    !! with `name` in the headers. Empty if the texts are equal.
    character(len=*), intent(in) :: a       !! the old text
    character(len=*), intent(in) :: b       !! the new text
    character(len=*), intent(in) :: name    !! file name for the `---` and `+++` headers
    integer, intent(in), optional :: context   !! number of unchanged lines shown around each change
    character(len=:), allocatable :: diff

    type(string_t), allocatable :: la(:), lb(:)
    integer, allocatable :: ia(:), ib(:)
    type(edit_t) :: e
    type(string_builder_t) :: buf
    integer :: ctx, k, first, last, i, j, ka, kb, na, nb, start_a, start_b

    diff = ''
    if (a == b .and. len(a) == len(b)) return
    ctx = 3
    if (present(context)) ctx = context
    la = split_lines(a)
    lb = split_lines(b)
    call line_ids(la, lb, ia, ib)
    allocate (e%op(size(ia) + size(ib)))
    call lcs(ia, 1, size(ia), ib, 1, size(ib), e)

    call buf%append('--- '//name//new_line('a')//'+++ '//name//new_line('a'))
    ! hunks: runs of edits with at most 2*ctx equal lines between them, plus context around
    k = 1
    i = 1           ! next line of a
    j = 1           ! next line of b
    do while (k <= e%n)
        if (e%op(k) == OP_EQUAL) then
            k = k + 1
            i = i + 1
            j = j + 1
            cycle
        end if
        first = k
        last = k
        do while (last < e%n)          ! extend over changes separated by short runs of equal lines
            ka = last + 1
            do while (ka <= e%n)
                if (e%op(ka) /= OP_EQUAL) exit
                ka = ka + 1
            end do
            if (ka > e%n .or. ka - last - 1 > 2*ctx) exit
            last = ka
        end do
        ! the hunk covers edits first-ctx .. last+ctx (equal lines at both ends)
        ka = first
        nb = 0
        do while (ka > 1 .and. nb < ctx)
            if (e%op(ka - 1) /= OP_EQUAL) exit
            ka = ka - 1
            nb = nb + 1
        end do
        start_a = i - nb
        start_b = j - nb
        kb = last
        nb = 0
        do while (kb < e%n .and. nb < ctx)
            if (e%op(kb + 1) /= OP_EQUAL) exit
            kb = kb + 1
            nb = nb + 1
        end do
        na = count(e%op(ka:kb) /= OP_INSERT)
        nb = count(e%op(ka:kb) /= OP_DELETE)
        call buf%append('@@ -'//range_text(start_a, na)//' +'//range_text(start_b, nb)//' @@'//new_line('a'))
        i = start_a
        j = start_b
        do k = ka, kb
            select case (e%op(k))
            case (OP_EQUAL)
                call add_line(' ', la(i)%s)
                i = i + 1
                j = j + 1
            case (OP_DELETE)
                call add_line('-', la(i)%s)
                i = i + 1
            case (OP_INSERT)
                call add_line('+', lb(j)%s)
                j = j + 1
            end select
        end do
        k = kb + 1
    end do
    diff = buf%get()

contains

    subroutine add_line(mark, line)
        !! Append `line` to the diff after `mark`, with a "No newline" note if it has no line ending.
        character, intent(in) :: mark           !! ` ` for context, `-` for a deleted line, `+` for an inserted one
        character(len=*), intent(in) :: line    !! the line, with its line ending
        call buf%append(mark//line)
        if (len(line) == 0) then
            call buf%append(new_line('a')//'\ No newline at end of file'//new_line('a'))
        else if (line(len(line):len(line)) /= achar(10)) then
            call buf%append(new_line('a')//'\ No newline at end of file'//new_line('a'))
        end if
    end subroutine add_line

end function unified_diff

pure function range_text(start, n) result(text)
    !! `start,n` for a hunk header; `start` is the line before the hunk when it is empty.
    integer, intent(in) :: start    !! first line of the hunk
    integer, intent(in) :: n        !! number of lines in the hunk
    character(len=:), allocatable :: text
    if (n == 0) then
        text = to_s(start - 1)//',0'
    else if (n == 1) then
        text = to_s(start)
    else
        text = to_s(start)//','//to_s(n)
    end if
end function range_text

function split_lines(text) result(lines)
    !! The lines of `text`, each with its line ending (the last one may have none).
    character(len=*), intent(in) :: text   !! the text
    type(string_t), allocatable :: lines(:)
    integer :: i, j, n

    n = count([(text(i:i) == achar(10), i=1, len(text))])
    if (len(text) > 0) then
        if (text(len(text):len(text)) /= achar(10)) n = n + 1
    end if
    allocate (lines(n))
    i = 1
    do j = 1, n
        n = index(text(i:), achar(10))
        if (n == 0) then
            lines(j)%s = text(i:)
        else
            lines(j)%s = text(i:i + n - 1)
            i = i + n
        end if
    end do
end function split_lines

subroutine line_ids(la, lb, ia, ib)
    !! Number the distinct lines, so that equal lines have equal numbers.
    type(string_t), intent(in) :: la(:), lb(:)          !! the lines of the two texts
    integer, allocatable, intent(out) :: ia(:), ib(:)   !! the number of each line of `la` and `lb`
    integer, allocatable :: slot(:)     ! hash table: id of the line in each slot, or 0
    type(string_t), allocatable :: seen(:)
    integer :: k, nseen, mask

    mask = 1
    do while (mask < 2*(size(la) + size(lb)) + 1)
        mask = 2*mask
    end do
    allocate (slot(0:mask - 1), seen(size(la) + size(lb)), ia(size(la)), ib(size(lb)))
    mask = mask - 1
    slot = 0
    nseen = 0
    do k = 1, size(la)
        ia(k) = id_of(la(k)%s)
    end do
    do k = 1, size(lb)
        ib(k) = id_of(lb(k)%s)
    end do

contains

    integer function id_of(line)
        !! The number of `line`: that of an equal line seen before, or the next new number.
        character(len=*), intent(in) :: line   !! a line of `la` or `lb`
        integer :: h
        h = int(iand(hash(line), int(mask, int64)))
        do
            if (slot(h) == 0) then
                nseen = nseen + 1
                seen(nseen)%s = line
                slot(h) = nseen
                id_of = nseen
                return
            end if
            if (len(seen(slot(h))%s) == len(line)) then
                if (seen(slot(h))%s == line) then
                    id_of = slot(h)
                    return
                end if
            end if
            h = iand(h + 1, mask)
        end do
    end function id_of

end subroutine line_ids

pure integer(int64) function hash(s) result(h)
    !! FNV-1a hash.
    character(len=*), intent(in) :: s   !! the bytes to hash
    integer :: k
    h = 1469598103_int64
    do k = 1, len(s)
        h = ieor(h, int(iachar(s(k:k)), int64))
        h = h*16777619_int64
        h = iand(h, 4294967295_int64)
    end do
end function hash

recursive subroutine lcs(a, a0, a1, b, b0, b1, e)
    !! Append the edits that turn `a(a0:a1)` into `b(b0:b1)` along a longest common subsequence
    !! (Hirschberg's algorithm).
    integer, intent(in) :: a(:), b(:)           !! line numbers of the two texts, from `line_ids`
    integer, intent(in) :: a0, a1, b0, b1       !! the ranges of `a` and `b` to compare
    type(edit_t), intent(inout) :: e            !! the edits so far; `e%op` must have room for the new ones
    integer :: lo_a, lo_b, hi_a, hi_b, nsuffix, k

    ! common prefix and suffix, without recursion
    lo_a = a0
    lo_b = b0
    do while (lo_a <= a1 .and. lo_b <= b1)
        if (a(lo_a) /= b(lo_b)) exit
        call push(OP_EQUAL)
        lo_a = lo_a + 1
        lo_b = lo_b + 1
    end do
    hi_a = a1
    hi_b = b1
    nsuffix = 0
    do while (hi_a >= lo_a .and. hi_b >= lo_b)
        if (a(hi_a) /= b(hi_b)) exit
        hi_a = hi_a - 1
        hi_b = hi_b - 1
        nsuffix = nsuffix + 1
    end do
    call middle(lo_a, hi_a, lo_b, hi_b)
    do k = 1, nsuffix
        call push(OP_EQUAL)
    end do

contains

    recursive subroutine middle(a0, a1, b0, b1)
        !! The edits for `a(a0:a1)` and `b(b0:b1)`, which differ at both ends.
        integer, intent(in) :: a0, a1, b0, b1   !! the ranges of `a` and `b`
        integer :: fwd(0:b1 - b0 + 1), bwd(0:b1 - b0 + 1)   ! (not allocatable: assignment would make them 1-based)
        integer :: mid, k, best, split, j
        if (a0 > a1) then
            do j = b0, b1
                call push(OP_INSERT)
            end do
            return
        end if
        if (b0 > b1) then
            do j = a0, a1
                call push(OP_DELETE)
            end do
            return
        end if
        if (a0 == a1) then
            ! one line of a: keep it if it occurs in b
            do j = b0, b1
                if (b(j) == a(a0)) then
                    do k = b0, j - 1
                        call push(OP_INSERT)
                    end do
                    call push(OP_EQUAL)
                    do k = j + 1, b1
                        call push(OP_INSERT)
                    end do
                    return
                end if
            end do
            call push(OP_DELETE)
            do k = b0, b1
                call push(OP_INSERT)
            end do
            return
        end if

        mid = (a0 + a1)/2
        fwd = lcs_lengths(a(a0:mid), b(b0:b1))
        bwd = lcs_lengths(a(a1:mid + 1:-1), b(b1:b0:-1))
        best = -1
        split = 0
        do k = 0, b1 - b0 + 1
            if (fwd(k) + bwd(b1 - b0 + 1 - k) > best) then
                best = fwd(k) + bwd(b1 - b0 + 1 - k)
                split = k
            end if
        end do
        call lcs(a, a0, mid, b, b0, b0 + split - 1, e)
        call lcs(a, mid + 1, a1, b, b0 + split, b1, e)
    end subroutine middle

    subroutine push(op)
        !! Append edit `op` (`OP_EQUAL`, `OP_DELETE` or `OP_INSERT`) to `e`.
        integer, intent(in) :: op       !! the edit
        e%n = e%n + 1
        e%op(e%n) = op
    end subroutine push

end subroutine lcs

pure function lcs_lengths(a, b) result(row)
    !! `row(k)`: the length of the longest common subsequence of `a` and `b(1:k)`.
    integer, intent(in) :: a(:), b(:)   !! line numbers of the two texts
    integer :: row(0:size(b))
    integer :: prev(0:size(b))
    integer :: i, k
    row = 0
    do i = 1, size(a)
        prev = row
        do k = 1, size(b)
            if (a(i) == b(k)) then
                row(k) = prev(k - 1) + 1
            else
                row(k) = max(row(k - 1), prev(k))
            end if
        end do
    end do
end function lcs_lengths

end module ffmt_diff


!>>>>> ././src/ffmt_io.f90
module ffmt_io
!! Reading and writing whole files as bytes, so that line endings are preserved on every platform.
implicit none
private

public :: read_file, write_file, read_stdin

contains

subroutine read_file(path, text, stat, errmsg)
    !! All bytes of file `path`.
    character(len=*), intent(in) :: path   !! the file to read
    character(len=:), allocatable, intent(out) :: text      !! the contents; `''` if the file cannot be opened
    integer, intent(out) :: stat                            !! `iostat` of the open or read: nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg    !! the system's message, if `stat` is nonzero
    integer :: unit, nbytes
    character(len=256) :: msg

    text = ''
    open (newunit=unit, file=path, access='stream', form='unformatted', status='old', &
          action='read', iostat=stat, iomsg=msg)
    if (stat /= 0) then
        errmsg = trim(msg)
        return
    end if
    inquire (unit=unit, size=nbytes)
    if (nbytes > 0) then
        deallocate (text)
        allocate (character(len=nbytes) :: text)
        read (unit, iostat=stat, iomsg=msg) text
        if (stat /= 0) errmsg = trim(msg)
    end if
    close (unit)
end subroutine read_file

subroutine write_file(path, text, stat, errmsg)
    !! Replace the contents of file `path` (created if needed) with the bytes of `text`.
    character(len=*), intent(in) :: path   !! the file to write
    character(len=*), intent(in) :: text   !! the new contents
    integer, intent(out) :: stat                            !! `iostat` of the open or write: nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg    !! the system's message, if `stat` is nonzero
    integer :: unit
    character(len=256) :: msg

    open (newunit=unit, file=path, access='stream', form='unformatted', status='replace', &
          action='write', iostat=stat, iomsg=msg)
    if (stat /= 0) then
        errmsg = trim(msg)
        return
    end if
    write (unit, iostat=stat, iomsg=msg) text
    if (stat /= 0) errmsg = trim(msg)
    close (unit)
end subroutine write_file

subroutine read_stdin(text)
    !! All of standard input. Lines are read as text, so each line ends with a line feed, except a
    !! last line without one; carriage returns of CRLF line endings are kept as part of the line.
    use, intrinsic :: iso_fortran_env, only: input_unit
    use ffmt_strings, only: string_builder_t
    character(len=:), allocatable, intent(out) :: text      !! the input
    type(string_builder_t) :: buf
    character(len=4096) :: chunk
    integer :: ios, n

    do
        read (input_unit, '(a)', advance='no', iostat=ios, size=n) chunk
        call buf%append(chunk(1:n))
        if (is_iostat_eor(ios)) then
            call buf%append(achar(10))
        else if (ios /= 0) then
            exit                                ! end of input
        end if
    end do
    text = buf%get()
end subroutine read_stdin

end module ffmt_io


!>>>>> ././src/ffmt_settings.f90
module ffmt_settings
!! Named settings, as written in a config file or given on the command line, applied to the options.
!!
!! | Setting | Values |
!! |---|---|
!! | `indent`, `base-indent`, `continuation-indent` | 0 to 64 |
!! | `indent-units`, `indent-procedures`, `indent-fypp`, `indent-comments`, `split`, `sort-use`, `sort-use-only` | `true`, `false` |
!! | `continuation` | `align`, `indent` |
!! | `column1-comments` | `indent` (like the code around them), `keep` (stay in column 1) |
!! | `line-length` | 0 (no limit) to 100000 |
!! | `relational` | `symbols`, `letters`, `keep` |
!! | `comment-spacing` | 0 to 64, or `keep` |
!! | `spaces.RULE` (or `spaces.all`) | `add`, `remove`, `keep` |
!! | `case.WHAT`: `keywords`, `operators`, `procedures`, `constants`, `types` or `all` | `lower`, `upper`, `keep` |
use ffmt_options
use ffmt_strings, only: to_s
implicit none
private

public :: apply_setting

type, public :: setting_t
    !! A named setting with its value as text.
    character(len=:), allocatable :: key        !! the name, e.g. `indent` or `spaces.comma`
    character(len=:), allocatable :: value      !! the value, e.g. `4`, `true` or `add`
end type setting_t

contains

subroutine apply_setting(opts, key, value, stat, errmsg)
    !! Apply one setting to `opts`. `stat` is nonzero, with a message, for an unknown setting or a bad value.
    type(format_options_t), intent(inout) :: opts           !! the options to change
    character(len=*), intent(in) :: key                     !! setting name, as in the table above
    character(len=*), intent(in) :: value                   !! its value as text
    integer, intent(out) :: stat        !! 0, or 1 for an unknown setting or a bad value
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, if `stat` is nonzero

    stat = 0
    select case (key)
    case ('indent')
        call read_count(64, opts%indent)
    case ('base-indent')
        call read_count(64, opts%base_indent)
    case ('continuation-indent')
        call read_count(64, opts%continuation_indent)
    case ('line-length')
        call read_count(100000, opts%line_length)
    case ('indent-units')
        call read_flag(opts%indent_units)
    case ('indent-procedures')
        call read_flag(opts%indent_procedures)
    case ('indent-fypp')
        call read_flag(opts%indent_fypp)
    case ('indent-comments')
        call read_flag(opts%indent_comments)
    case ('column1-comments')
        select case (value)
        case ('keep')
            opts%keep_column1_comments = .true.
        case ('indent')
            opts%keep_column1_comments = .false.
        case default
            call fail('"keep" or "indent"')
        end select
    case ('split')
        call read_flag(opts%split_lines)
    case ('sort-use')
        call read_flag(opts%sort_use)
    case ('sort-use-only')
        call read_flag(opts%sort_use_only)
    case ('comment-spacing')
        if (value == 'keep') then
            opts%comment_spacing = -1
        else
            call read_count(64, opts%comment_spacing)
            if (stat /= 0) errmsg = 'expected a number from 0 to 64 or keep for "'//key//'", not "'//value//'"'
        end if
    case ('continuation')
        select case (value)
        case ('align')
            opts%continuation = CONT_ALIGN
        case ('indent')
            opts%continuation = CONT_INDENT
        case default
            call fail('"align" or "indent"')
        end select
    case ('relational')
        select case (value)
        case ('symbols')
            opts%relational = REL_SYMBOLS
        case ('letters')
            opts%relational = REL_LETTERS
        case ('keep')
            opts%relational = REL_KEEP
        case default
            call fail('"symbols", "letters" or "keep"')
        end select
    case default
        if (index(key, 'spaces.') == 1) then
            call set_spaces(key(8:))
        else if (index(key, 'case.') == 1) then
            call set_case(key(6:))
        else
            stat = 1
            errmsg = 'unknown setting "'//key//'"'
        end if
    end select

contains

    subroutine fail(expected)
        !! Report a bad value for the setting.
        character(len=*), intent(in) :: expected    !! description of the values allowed
        stat = 1
        errmsg = 'expected '//expected//' for "'//key//'", not "'//value//'"'
    end subroutine fail

    subroutine read_count(max_value, n)
        !! Read the value as a whole number from 0 to `max_value` into `n`; `n` is unchanged if it is not one.
        integer, intent(in) :: max_value    !! largest value allowed
        integer, intent(inout) :: n         !! the option to set
        integer :: ios, v
        if (verify(value, '0123456789') == 0 .and. len(value) > 0 .and. len(value) <= 9) then
            read (value, *, iostat=ios) v
            if (ios == 0 .and. v <= max_value) then
                n = v
                return
            end if
        end if
        call fail('a number from 0 to '//to_s(max_value))
    end subroutine read_count

    subroutine read_flag(flag)
        !! Read the value, `true` or `false`, into `flag`; `flag` is unchanged if it is neither.
        logical, intent(inout) :: flag      !! the option to set
        select case (value)
        case ('true')
            flag = .true.
        case ('false')
            flag = .false.
        case default
            call fail('true or false')
        end select
    end subroutine read_flag

    subroutine set_spaces(rule)
        !! Set whitespace rule `rule` of `opts%spaces` (all rules for `all`) to the value: `add`,
        !! `remove` or `keep`.
        character(len=*), intent(in) :: rule    !! rule name, the key without `spaces.`, e.g. `comma`
        integer :: mode
        select case (value)
        case ('add')
            mode = WS_ADD
        case ('remove')
            mode = WS_REMOVE
        case ('keep')
            mode = WS_KEEP
        case default
            call fail('add, remove or keep')
            return
        end select
        associate (ws => opts%spaces)
            select case (rule)
            case ('all')
                ws = whitespace_options_t(mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, mode, &
                                          mode, mode, mode, mode)
            case ('assignment')
                ws%assignment = mode
            case ('keyword-args')
                ws%keyword_args = mode
            case ('pointer')
                ws%pointer = mode
            case ('declarations')
                ws%declarations = mode
            case ('relational')
                ws%relational = mode
            case ('logical')
                ws%logical = mode
            case ('plus-minus')
                ws%plus_minus = mode
            case ('mult-div')
                ws%mult_div = mode
            case ('power')
                ws%power = mode
            case ('concat')
                ws%concat = mode
            case ('comma')
                ws%comma = mode
            case ('slice-colon')
                ws%slice_colon = mode
            case ('percent')
                ws%percent = mode
            case ('control-paren')
                ws%control_paren = mode
            case ('conditional')
                ws%conditional = mode
            case ('defined-operators')
                ws%defined_operators = mode
            case default
                stat = 1
                errmsg = 'unknown whitespace rule "'//rule//'"'
            end select
        end associate
    end subroutine set_spaces

    subroutine set_case(what)
        !! Set the letter case of `what` in `opts%letter_case` to the value: `lower`, `upper` or `keep`.
        character(len=*), intent(in) :: what    !! `keywords`, `operators`, ... or `all`: the key without `case.`
        integer :: mode
        select case (value)
        case ('lower')
            mode = CASE_LOWER
        case ('upper')
            mode = CASE_UPPER
        case ('keep')
            mode = CASE_KEEP
        case default
            call fail('lower, upper or keep')
            return
        end select
        select case (what)
        case ('all')
            opts%letter_case = case_options_t(mode, mode, mode, mode, mode)
        case ('keywords')
            opts%letter_case%keywords = mode
        case ('operators')
            opts%letter_case%operators = mode
        case ('procedures')
            opts%letter_case%procedures = mode
        case ('constants')
            opts%letter_case%constants = mode
        case ('types')
            opts%letter_case%types = mode
        case default
            stat = 1
            errmsg = 'unknown case setting "'//what//'"'
        end select
    end subroutine set_case

end subroutine apply_setting

end module ffmt_settings


!>>>>> ././src/ffmt_tokens.f90
module ffmt_tokens
!! Tokens of free-form Fortran source.
!!
!! Every byte of the source belongs to exactly one token, including blanks, comments, line breaks
!! and continuation markers, so joining the text of all tokens reproduces the source exactly.
use ffmt_strings, only: make_lowercase
implicit none
private

public :: token_text, token_lower, is_chevron, detokenize

! token kinds
integer, parameter, public :: TK_SPACE = 1      !! blanks and tabs
integer, parameter, public :: TK_NEWLINE = 2    !! LF, CRLF or CR
integer, parameter, public :: TK_COMMENT = 3    !! from `!` to the end of the line
integer, parameter, public :: TK_CONT = 4       !! continuation `&`
integer, parameter, public :: TK_PREPROC = 5    !! a cpp or fypp directive line, with its continuation lines
integer, parameter, public :: TK_NAME = 6       !! names and keywords
integer, parameter, public :: TK_INT = 7        !! integer literal, possibly with a kind (`10_ik`)
integer, parameter, public :: TK_REAL = 8       !! real literal (`1.0`, `.5e-3_dp`)
integer, parameter, public :: TK_STRING = 9     !! character literal, possibly with a kind prefix
integer, parameter, public :: TK_BOZ = 10       !! `b'0101'`, `o'17'`, `z'ff'`
integer, parameter, public :: TK_DOTOP = 11     !! `.and.`, `.eq.`, `.true.`, user-defined `.op.`
integer, parameter, public :: TK_OP = 12        !! operators and punctuation: `**`, `//`, `==`, `=>`, `::`, `(`, `%`, `?`, ...
integer, parameter, public :: TK_FYPP = 13      !! inline fypp: `${...}$`, `@{...}@`, `#{...}#`
integer, parameter, public :: TK_OTHER = 14     !! any other character
integer, parameter, public :: TK_BOM = 15       !! UTF-8 byte order mark at the start of the file

logical, parameter, public :: is_code_kind(0:15) = [.true., &                      ! 0: no kind yet
                                                    .false., .false., .false., .false., .false., &  ! space .. preproc
                                                    .true., .true., .true., .true., .true., .true., &
                                                    .true., .true., .true., &     ! name .. other
                                                    .false.]                      ! byte order mark
    !! True for the kinds of tokens that are part of the code, as opposed to layout (blanks, line
    !! breaks, continuation markers), comments and preprocessor lines. Indexed by kind,
    !! `is_code_kind(t%kind)` reads like a function call, and as a constant it is inlined, where a
    !! function in another module is not.

! comment sub-kinds
integer, parameter, public :: CM_PLAIN = 0        !! ordinary comment
integer, parameter, public :: CM_DIRECTIVE = 1    !! compiler directive: `!$omp`, `!$acc`, `!dir$`, `!gcc$`, ...
integer, parameter, public :: CM_CONDITIONAL = 2  !! OpenMP conditional compilation: `!$ code`
integer, parameter, public :: CM_FORMATTER = 3    !! formatter directive: `!&`, `!&<`, `!&>`

! continuation sub-kinds
integer, parameter, public :: CT_TRAILING = 1     !! `&` ending a line
integer, parameter, public :: CT_LEADING = 2      !! `&` starting a continuation line

! string sub-kinds (bit flags)
integer, parameter, public :: STR_UNTERMINATED = 1  !! no closing quote before the end of the line
integer, parameter, public :: STR_MULTILINE = 2     !! continued across lines
integer, parameter, public :: STR_RAW_CONT = 4      !! a continuation line without a leading `&`: its blanks are in the string

type, public :: token_t
    !! One token: a byte range of the source with its kind.
    integer :: kind = 0       !! one of the TK_* constants
    integer :: first = 1      !! first byte in the source
    integer :: last = 0       !! last byte in the source
    integer :: line = 1       !! line of the first byte
    integer :: sub = 0        !! sub-kind (CM_*, CT_*, STR_*)
    logical :: glued = .false. !! joined to the previous code token across a line break, with no blank allowed between
end type token_t

type, public :: token_list_t
    !! The tokens of a source text, in source order.
    type(token_t), allocatable :: tok(:)   !! storage; only `tok(1:n)` is in use
    integer :: n = 0                       !! number of tokens
contains
    procedure :: push                      !! append a token
end type token_list_t

contains

subroutine push(self, t)
    !! Append token `t`, growing the storage as needed.
    class(token_list_t), intent(inout) :: self   !! the token list
    type(token_t), intent(in) :: t               !! token to append
    type(token_t), allocatable :: tmp(:)

    if (.not. allocated(self%tok)) allocate (self%tok(256))
    if (self%n == size(self%tok)) then
        allocate (tmp(2*self%n))
        tmp(1:self%n) = self%tok(1:self%n)
        call move_alloc(tmp, self%tok)
    end if
    self%n = self%n + 1
    self%tok(self%n) = t
end subroutine push

pure function token_text(src, t) result(text)
    !! Token text, as it appears in the source.
    character(len=*), intent(in) :: src   !! the source the token was read from
    type(token_t), intent(in) :: t        !! a token of `src`
    character(len=:), allocatable :: text
    text = src(t%first:t%last)
end function token_text

pure function token_lower(src, t) result(text)
    !! Token text in lowercase (ASCII).
    character(len=*), intent(in) :: src   !! the source the token was read from
    type(token_t), intent(in) :: t        !! a token of `src`
    character(len=:), allocatable :: text
    text = src(t%first:t%last)
    call make_lowercase(text)
end function token_lower

pure logical function is_chevron(src, tokens, t)
    !! True if token `t` is a `<` or `>` operator directly next to another one, as in the CUDA
    !! kernel launch `call k<<<grid, block>>>(x)`, where they are not relational operators.
    character(len=*), intent(in) :: src          !! the source the tokens were read from
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    integer, intent(in) :: t                     !! index into `tokens`
    is_chevron = .false.
    if (.not. is_angle(t)) return
    if (t > 1) is_chevron = is_angle(t - 1)
    if (t < tokens%n) is_chevron = is_chevron .or. is_angle(t + 1)

contains

    pure logical function is_angle(k)
        !! True if token `k` is a `<` or `>` operator.
        integer, intent(in) :: k   !! index into `tokens`
        is_angle = .false.
        if (tokens%tok(k)%kind /= TK_OP) return
        is_angle = src(tokens%tok(k)%first:tokens%tok(k)%last) == '<' .or. &
                   src(tokens%tok(k)%first:tokens%tok(k)%last) == '>'
    end function is_angle

end function is_chevron

function detokenize(src, tokens) result(text)
    !! Join the text of all tokens.
    character(len=*), intent(in) :: src          !! the source the tokens were read from
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    character(len=:), allocatable :: text
    integer :: k, n, total

    total = 0
    do k = 1, tokens%n
        total = total + tokens%tok(k)%last - tokens%tok(k)%first + 1
    end do
    allocate (character(len=total) :: text)
    n = 0
    do k = 1, tokens%n
        associate (t => tokens%tok(k))
            text(n + 1:n + t%last - t%first + 1) = src(t%first:t%last)
            n = n + t%last - t%first + 1
        end associate
    end do
end function detokenize

end module ffmt_tokens


!>>>>> build/dependencies/fregex/src/fregex_match.f90
module fregex_match
!! The result of a successful match: the spans of the whole match and of each capture group.
use fregex_string, only: regex_string_t, to_str
implicit none
private

public :: new_match, init_match

type, public :: match_t
    !! Group spans are 1-based and inclusive, so `string(m%start(k):m%end(k))` is the text of
    !! group `k`. An empty match has `end = start - 1`. A group that did not take part in the
    !! match has `start = 0` and `end = -1`, so its text is the empty string.
    private
    character(len=:), allocatable :: text        !! the part of the subject that the groups cover;
                                                 !! allocated only after a successful match
    integer :: offset = 0                        !! position in the subject of `text(1:1)`, minus 1
    integer :: start0 = 0, end0 = -1             !! span of group 0, kept apart from `spans` so that
                                                 !! a pattern without groups needs no array
    integer :: ngroups = 0
    integer, allocatable :: spans(:, :)          !! (2, 1:ngroups), allocated only if ngroups > 0
    type(regex_string_t), allocatable :: names(:)      !! (1:ngroups)
contains
    procedure, private :: group_by_index, group_by_name
    generic :: group => group_by_index, group_by_name
    procedure, private :: start_by_index, start_by_name
    generic :: start => start_by_index, start_by_name
    procedure, private :: end_by_index, end_by_name
    generic :: end => end_by_index, end_by_name
    procedure, private :: matched_by_index, matched_by_name
    generic :: matched => matched_by_index, matched_by_name
    procedure :: group_count
    procedure :: group_index
end type match_t

contains

pure function new_match(subject, caps, names) result(m)
    !! Build a match from the VM's capture slots (start, one-past-end; 0 if unset).
    character(len=*), intent(in) :: subject
    integer, intent(in) :: caps(0:)
    type(regex_string_t), intent(in) :: names(:)
    type(match_t) :: m
    call init_match(m, subject, caps, names)
end function new_match

pure subroutine init_match(m, subject, caps, names)
    !! Like new_match, but fills `m` in place, which avoids copying the result.
    type(match_t), intent(out) :: m
    character(len=*), intent(in) :: subject
    integer, intent(in) :: caps(0:)
    type(regex_string_t), intent(in) :: names(:)
    integer :: k, lo, hi

    ! names are only needed for lookups by name, so they are not copied if no group has one
    do k = 1, size(names)
        if (len(names(k)%s) > 0) then
            m%names = names
            exit
        end if
    end do
    m%start0 = caps(0)
    m%end0 = caps(1) - 1
    lo = m%start0
    hi = m%end0
    m%ngroups = size(caps)/2 - 1
    if (m%ngroups > 0) then
        allocate (m%spans(2, m%ngroups))
        do k = 1, m%ngroups
            if (caps(2*k) > 0 .and. caps(2*k + 1) > 0) then
                m%spans(:, k) = [caps(2*k), caps(2*k + 1) - 1]
                lo = min(lo, m%spans(1, k))
                hi = max(hi, m%spans(2, k))
            else
                m%spans(:, k) = [0, -1]
            end if
        end do
    end if
    ! Only the covered text is kept (groups in a lookaround can lie outside group 0), so that
    ! find_all does not copy the whole subject for every match.
    m%offset = lo - 1
    m%text = subject(lo:hi)
end subroutine init_match

pure integer function group_count(self)
    !! Number of capture groups in the pattern (not counting group 0, the whole match).
    class(match_t), intent(in) :: self
    group_count = self%ngroups
end function group_count

pure integer function group_index(self, name) result(g)
    !! Number of the group called `name`, or 0 if there is none.
    class(match_t), intent(in) :: self
    character(len=*), intent(in) :: name
    if (allocated(self%names)) then
        do g = 1, size(self%names)
            if (len(self%names(g)%s) == len(name)) then
                if (self%names(g)%s == name) return
            end if
        end do
    end if
    g = 0
end function group_index

pure function group_by_index(self, k) result(text)
    !! Text of group `k` (default 0, the whole match).
    class(match_t), intent(in) :: self
    integer, intent(in), optional :: k
    character(len=:), allocatable :: text
    integer :: g
    g = checked_index(self, k)
    text = self%text(span_start(self, g) - self%offset:span_end(self, g) - self%offset)
end function group_by_index

pure function group_by_name(self, name) result(text)
    class(match_t), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: text
    text = self%group_by_index(checked_name(self, name))
end function group_by_name

pure integer function start_by_index(self, k) result(i)
    class(match_t), intent(in) :: self
    integer, intent(in), optional :: k
    i = span_start(self, checked_index(self, k))
end function start_by_index

pure integer function start_by_name(self, name) result(i)
    class(match_t), intent(in) :: self
    character(len=*), intent(in) :: name
    i = span_start(self, checked_name(self, name))
end function start_by_name

pure integer function end_by_index(self, k) result(i)
    class(match_t), intent(in) :: self
    integer, intent(in), optional :: k
    i = span_end(self, checked_index(self, k))
end function end_by_index

pure integer function end_by_name(self, name) result(i)
    class(match_t), intent(in) :: self
    character(len=*), intent(in) :: name
    i = span_end(self, checked_name(self, name))
end function end_by_name

pure logical function matched_by_index(self, k) result(matched)
    !! True if group `k` took part in the match.
    class(match_t), intent(in) :: self
    integer, intent(in), optional :: k
    matched = span_start(self, checked_index(self, k)) > 0
end function matched_by_index

pure logical function matched_by_name(self, name) result(matched)
    class(match_t), intent(in) :: self
    character(len=*), intent(in) :: name
    matched = span_start(self, checked_name(self, name)) > 0
end function matched_by_name

pure integer function checked_index(self, k) result(g)
    class(match_t), intent(in) :: self
    integer, intent(in), optional :: k
    if (.not. allocated(self%text)) error stop 'fregex: match_t used without a successful match'
    g = 0
    if (present(k)) g = k
    if (g < 0 .or. g > self%ngroups) error stop 'fregex: no such group: '//to_str(g)
end function checked_index

pure integer function span_start(self, g) result(i)
    class(match_t), intent(in) :: self
    integer, intent(in) :: g
    if (g == 0) then
        i = self%start0
    else
        i = self%spans(1, g)
    end if
end function span_start

pure integer function span_end(self, g) result(i)
    class(match_t), intent(in) :: self
    integer, intent(in) :: g
    if (g == 0) then
        i = self%end0
    else
        i = self%spans(2, g)
    end if
end function span_end

pure integer function checked_name(self, name) result(g)
    class(match_t), intent(in) :: self
    character(len=*), intent(in) :: name
    g = self%group_index(name)
    if (g == 0) error stop 'fregex: no such group: '//name
end function checked_name

end module fregex_match


!>>>>> build/dependencies/fregex/src/fregex_parser.f90
module fregex_parser
!! Regular expression parser: turns a pattern string into an abstract syntax tree (AST).
!!
!! The syntax follows Python's `re` module for byte strings (ASCII semantics). Backslashes are
!! literal in Fortran strings, so patterns are written exactly like Python raw strings.
use fregex_charset, only: charset_t, digit_set, word_set, space_set
use fregex_string, only: regex_string_t, to_str, is_alpha_code, is_digit_code, is_word_code
implicit none
private

public :: parse_pattern

! node kinds
integer, parameter, public :: N_EMPTY = 0    !! matches the empty string
integer, parameter, public :: N_CHAR = 1     !! a = byte, b = 1 if case-insensitive
integer, parameter, public :: N_ANY = 2      !! a = 1 if it also matches a newline
integer, parameter, public :: N_SET = 3      !! a = index into ast%sets
integer, parameter, public :: N_ASSERT = 4   !! a = assertion kind (A_*)
integer, parameter, public :: N_CONCAT = 5   !! kids matched in sequence
integer, parameter, public :: N_ALT = 6      !! kids tried in order
integer, parameter, public :: N_GROUP = 7    !! a = group number, kids(1) = body
integer, parameter, public :: N_REPEAT = 8   !! a = min, b = max (or INF), c = R_* mode, kids(1) = body
integer, parameter, public :: N_LOOK = 9     !! a = 1 lookahead / 0 lookbehind, b = 1 if negated, kids(1) = body
integer, parameter, public :: N_ATOMIC = 10  !! kids(1) = body
integer, parameter, public :: N_BACKREF = 11 !! a = group number, b = 1 if case-insensitive

! assertion kinds
integer, parameter, public :: A_BOL = 1      !! `^`
integer, parameter, public :: A_BOL_M = 2    !! `^` in multiline mode
integer, parameter, public :: A_EOL = 3      !! `$`
integer, parameter, public :: A_EOL_M = 4    !! `$` in multiline mode
integer, parameter, public :: A_BOS = 5      !! `\A`
integer, parameter, public :: A_EOS = 6      !! `\Z`
integer, parameter, public :: A_WORDB = 7    !! `\b`
integer, parameter, public :: A_NWORDB = 8   !! `\B`

! repeat modes
integer, parameter, public :: R_GREEDY = 0, R_LAZY = 1, R_POSSESSIVE = 2

integer, parameter, public :: INF = -1       !! unbounded repeat count or width

! flags
integer, parameter, public :: F_IGNORECASE = 1
integer, parameter, public :: F_MULTILINE = 2
integer, parameter, public :: F_DOTALL = 4

integer, parameter :: MAX_REPEAT = 100000

type, public :: node_t
    integer :: kind = N_EMPTY
    integer :: a = 0, b = 0, c = 0
    integer, allocatable :: kids(:)
end type node_t

type, public :: ast_t
    type(node_t), allocatable :: nodes(:)
    integer :: n = 0
    integer :: root = 0
    type(charset_t), allocatable :: sets(:)
    integer :: nsets = 0
    integer :: ngroups = 0
    type(regex_string_t), allocatable :: names(:)  !! (1:ngroups), empty for unnamed groups
    logical, allocatable :: closed(:)        !! (1:ngroups), true once the group's `)` is parsed
end type ast_t

type :: parser_t
    character(len=:), allocatable :: p
    integer :: i = 1
    type(ast_t) :: ast
    character(len=:), allocatable :: errmsg
end type parser_t

contains

subroutine parse_pattern(pattern, flags, ast, errmsg)
    !! Parse `pattern` into `ast`. On a syntax error, `errmsg` is allocated with a description.
    character(len=*), intent(in) :: pattern
    integer, intent(in) :: flags
    type(ast_t), intent(out) :: ast
    character(len=:), allocatable, intent(out) :: errmsg

    type(parser_t) :: ps
    integer :: f

    ps%p = pattern
    allocate (ps%ast%nodes(16), ps%ast%sets(4), ps%ast%names(0), ps%ast%closed(0))
    f = flags
    ps%ast%root = parse_alt(ps, f)
    ! only an unmatched ')' can stop the top-level alternation early
    if (.not. failed(ps) .and. .not. at_end(ps)) call set_error(ps, 'unbalanced parenthesis', ps%i)
    if (failed(ps)) then
        errmsg = ps%errmsg
        return
    end if
    ast = ps%ast
end subroutine parse_pattern

recursive function parse_alt(ps, flags) result(id)
    !! alternation := concat ('|' concat)*
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer :: id
    integer, allocatable :: kids(:)
    integer :: k

    id = 0
    k = parse_concat(ps, flags)
    if (failed(ps)) return
    kids = [k]
    do
        if (at_end(ps)) exit
        if (peek(ps) /= '|') exit
        ps%i = ps%i + 1
        k = parse_concat(ps, flags)
        if (failed(ps)) return
        kids = [kids, k]
    end do
    if (size(kids) == 1) then
        id = kids(1)
    else
        id = new_node(ps, N_ALT)
        ps%ast%nodes(id)%kids = kids
    end if
end function parse_alt

recursive function parse_concat(ps, flags) result(id)
    !! concat := quantified*
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer :: id
    integer, allocatable :: kids(:)
    integer :: k

    id = 0
    allocate (kids(0))
    do
        if (at_end(ps)) exit
        if (peek(ps) == '|' .or. peek(ps) == ')') exit
        k = parse_quantified(ps, flags)
        if (failed(ps)) return
        if (k > 0) kids = [kids, k]
    end do
    select case (size(kids))
    case (0)
        id = new_node(ps, N_EMPTY)
    case (1)
        id = kids(1)
    case default
        id = new_node(ps, N_CONCAT)
        ps%ast%nodes(id)%kids = kids
    end select
end function parse_concat

recursive function parse_quantified(ps, flags) result(id)
    !! quantified := atom quantifier?
    !!
    !! Returns 0 when the atom produced no node (an inline flag group or a comment).
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer :: id
    integer :: body, qmin, qmax, mode, qstart
    logical :: bare_assert

    bare_assert = is_bare_assertion(ps)
    body = parse_atom(ps, flags)
    id = body
    if (failed(ps) .or. at_end(ps)) return

    qstart = ps%i
    if (.not. parse_quantifier(ps, qmin, qmax)) return
    if (failed(ps)) return
    if (body == 0 .or. bare_assert) then
        call set_error(ps, 'nothing to repeat', qstart)
        return
    end if

    mode = R_GREEDY
    if (.not. at_end(ps)) then
        if (peek(ps) == '?') then
            mode = R_LAZY
            ps%i = ps%i + 1
        else if (peek(ps) == '+') then
            mode = R_POSSESSIVE
            ps%i = ps%i + 1
        end if
    end if
    id = new_node(ps, N_REPEAT, qmin, qmax, mode)
    ps%ast%nodes(id)%kids = [body]

    if (looks_like_quantifier(ps)) call set_error(ps, 'multiple repeat', ps%i)
end function parse_quantified

pure logical function is_bare_assertion(ps)
    !! True if the next atom is an assertion that cannot be repeated (`^`, `$`, `\b`, ...).
    type(parser_t), intent(in) :: ps
    is_bare_assertion = .false.
    if (at_end(ps)) return
    select case (peek(ps))
    case ('^', '$')
        is_bare_assertion = .true.
    case ('\')
        select case (peek(ps, 1))
        case ('b', 'B', 'A', 'Z')
            is_bare_assertion = .true.
        end select
    end select
end function is_bare_assertion

logical function parse_quantifier(ps, qmin, qmax) result(found)
    !! Parse `*`, `+`, `?` or `{m,n}` at the current position, advancing past it.
    !! A `{` that does not form a valid quantifier is not consumed (it is then a literal).
    type(parser_t), intent(inout) :: ps
    integer, intent(out) :: qmin, qmax

    found = .true.
    select case (peek(ps))
    case ('*')
        qmin = 0
        qmax = INF
    case ('+')
        qmin = 1
        qmax = INF
    case ('?')
        qmin = 0
        qmax = 1
    case ('{')
        found = parse_braces(ps, qmin, qmax)
        return
    case default
        found = .false.
        return
    end select
    ps%i = ps%i + 1
end function parse_quantifier

logical function parse_braces(ps, qmin, qmax) result(found)
    !! `{n}`, `{n,}`, `{,m}`, `{n,m}` or `{,}`.
    type(parser_t), intent(inout) :: ps
    integer, intent(out) :: qmin, qmax
    integer :: j, lo_start, lo_end, hi_start, hi_end
    logical :: comma

    found = .false.
    qmin = 0
    qmax = INF
    j = ps%i + 1
    lo_start = j
    do while (j <= len(ps%p))
        if (.not. is_digit_code(iachar(ps%p(j:j)))) exit
        j = j + 1
    end do
    lo_end = j - 1
    comma = .false.
    hi_start = j
    hi_end = j - 1
    if (j <= len(ps%p)) then
        if (ps%p(j:j) == ',') then
            comma = .true.
            j = j + 1
            hi_start = j
            do while (j <= len(ps%p))
                if (.not. is_digit_code(iachar(ps%p(j:j)))) exit
                j = j + 1
            end do
            hi_end = j - 1
        end if
    end if
    if (j > len(ps%p)) return
    if (ps%p(j:j) /= '}') return
    if (lo_end < lo_start .and. .not. comma) return   ! "{}" is a literal

    found = .true.
    if (lo_end >= lo_start) qmin = read_count(ps, ps%p(lo_start:lo_end), lo_start)
    if (comma) then
        if (hi_end >= hi_start) qmax = read_count(ps, ps%p(hi_start:hi_end), hi_start)
    else
        qmax = qmin
    end if
    if (failed(ps)) return
    if (qmax /= INF .and. qmax < qmin) then
        call set_error(ps, 'min repeat greater than max repeat', hi_start)
        return
    end if
    ps%i = j + 1
end function parse_braces

integer function read_count(ps, digits, pos) result(count)
    type(parser_t), intent(inout) :: ps
    character(len=*), intent(in) :: digits
    integer, intent(in) :: pos
    if (len(digits) > 6) then
        count = MAX_REPEAT + 1
    else
        read (digits, *) count
    end if
    if (count > MAX_REPEAT) call set_error(ps, 'the repetition number is too large', pos)
end function read_count

logical function looks_like_quantifier(ps)
    !! True if a quantifier starts at the current position (without consuming it).
    type(parser_t), intent(inout) :: ps
    integer :: save_i, qmin, qmax
    character(len=:), allocatable :: save_err

    looks_like_quantifier = .false.
    if (at_end(ps)) return
    select case (peek(ps))
    case ('*', '+', '?')
        looks_like_quantifier = .true.
    case ('{')
        save_i = ps%i
        if (allocated(ps%errmsg)) save_err = ps%errmsg
        looks_like_quantifier = parse_braces(ps, qmin, qmax)
        ps%i = save_i
        if (allocated(save_err)) then
            ps%errmsg = save_err
        else if (allocated(ps%errmsg)) then
            deallocate (ps%errmsg)
        end if
    end select
end function looks_like_quantifier

recursive function parse_atom(ps, flags) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer :: id

    id = 0
    select case (peek(ps))
    case ('(')
        id = parse_group(ps, flags)
    case ('[')
        id = parse_class(ps, flags)
    case ('.')
        ps%i = ps%i + 1
        id = new_node(ps, N_ANY, merge(1, 0, iand(flags, F_DOTALL) /= 0))
    case ('^')
        ps%i = ps%i + 1
        id = new_node(ps, N_ASSERT, merge(A_BOL_M, A_BOL, iand(flags, F_MULTILINE) /= 0))
    case ('$')
        ps%i = ps%i + 1
        id = new_node(ps, N_ASSERT, merge(A_EOL_M, A_EOL, iand(flags, F_MULTILINE) /= 0))
    case ('\')
        id = parse_escape(ps, flags)
    case ('*', '+', '?')
        call set_error(ps, 'nothing to repeat', ps%i)
    case ('{')
        if (looks_like_quantifier(ps)) then
            call set_error(ps, 'nothing to repeat', ps%i)
        else
            ps%i = ps%i + 1
            id = char_node(ps, iachar('{'), flags)
        end if
    case default
        id = char_node(ps, iachar(ps%p(ps%i:ps%i)), flags)
        ps%i = ps%i + 1
    end select
end function parse_atom

function parse_escape(ps, flags) result(id)
    !! A backslash escape outside a character class.
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: flags
    integer :: id
    integer :: start, code, g
    character :: c
    type(charset_t) :: set

    id = 0
    start = ps%i
    ps%i = ps%i + 1
    if (at_end(ps)) then
        call set_error(ps, 'bad escape (end of pattern)', start)
        return
    end if
    c = peek(ps)
    ps%i = ps%i + 1
    select case (c)
    case ('d', 'D', 'w', 'W', 's', 'S')
        set = class_escape_set(c)
        id = set_node(ps, set)
    case ('b')
        id = new_node(ps, N_ASSERT, A_WORDB)
    case ('B')
        id = new_node(ps, N_ASSERT, A_NWORDB)
    case ('A')
        id = new_node(ps, N_ASSERT, A_BOS)
    case ('Z')
        id = new_node(ps, N_ASSERT, A_EOS)
    case ('0')
        code = read_octal(ps, 0, 2)
        id = char_node(ps, code, flags)
    case ('1':'9')
        if (is_octal(peek(ps)) .and. is_octal(peek(ps, 1)) .and. is_octal(c)) then
            code = read_octal(ps, iachar(c) - iachar('0'), 2)
            if (code > 255) then
                call set_error(ps, 'octal escape value outside of range 0-0o377', start)
                return
            end if
            id = char_node(ps, code, flags)
            return
        end if
        g = iachar(c) - iachar('0')
        if (.not. at_end(ps)) then
            if (is_digit_code(iachar(peek(ps)))) then
                g = 10*g + iachar(peek(ps)) - iachar('0')
                ps%i = ps%i + 1
            end if
        end if
        id = backref_node(ps, g, flags, start)
    case default
        code = simple_escape(ps, c, start, in_class=.false.)
        if (failed(ps)) return
        id = char_node(ps, code, flags)
    end select
end function parse_escape

integer function simple_escape(ps, c, start, in_class) result(code)
    !! Escapes that stand for one byte: `\n`, `\t`, `\xhh`, `\.` etc. Unknown letters are errors.
    type(parser_t), intent(inout) :: ps
    character, intent(in) :: c
    integer, intent(in) :: start
    logical, intent(in) :: in_class
    integer :: j, h

    code = iachar(c)
    select case (c)
    case ('n')
        code = 10
    case ('t')
        code = 9
    case ('r')
        code = 13
    case ('f')
        code = 12
    case ('v')
        code = 11
    case ('a')
        code = 7
    case ('b')
        if (in_class) then
            code = 8
        else
            call set_error(ps, 'bad escape \b', start)
        end if
    case ('x')
        code = 0
        do j = 1, 2
            h = hex_value(peek(ps))
            if (at_end(ps) .or. h < 0) then
                call set_error(ps, 'incomplete escape \x', start)
                return
            end if
            code = 16*code + h
            ps%i = ps%i + 1
        end do
    case default
        if (is_alpha_code(iachar(c))) call set_error(ps, 'bad escape \'//c, start)
    end select
end function simple_escape

function parse_class(ps, flags) result(id)
    !! `[...]` character class.
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: flags
    integer :: id
    type(charset_t) :: set, item_set, hi_set
    integer :: start, lo, hi, range_start
    logical :: negate, first, lo_is_set, hi_is_set

    id = 0
    start = ps%i
    ps%i = ps%i + 1
    negate = .false.
    if (peek(ps) == '^' .and. .not. at_end(ps)) then
        negate = .true.
        ps%i = ps%i + 1
    end if
    first = .true.
    do
        if (at_end(ps)) then
            call set_error(ps, 'unterminated character set', start)
            return
        end if
        if (peek(ps) == ']' .and. .not. first) then
            ps%i = ps%i + 1
            exit
        end if
        first = .false.
        range_start = ps%i
        call class_item(ps, lo, lo_is_set, item_set)
        if (failed(ps)) return
        if (peek(ps) == '-' .and. ps%i + 1 <= len(ps%p) .and. peek(ps, 1) /= ']') then
            if (lo_is_set) then
                call set_error(ps, 'bad character range', range_start)
                return
            end if
            ps%i = ps%i + 1
            call class_item(ps, hi, hi_is_set, hi_set)
            if (failed(ps)) return
            if (hi_is_set .or. hi < lo) then
                call set_error(ps, 'bad character range', range_start)
                return
            end if
            call set%add_range(lo, hi)
        else if (lo_is_set) then
            call set%add_set(item_set)
        else
            call set%add(lo)
        end if
    end do
    if (iand(flags, F_IGNORECASE) /= 0) call set%fold_case()
    if (negate) call set%negate()
    id = set_node(ps, set)
end function parse_class

subroutine class_item(ps, code, is_set, set)
    !! One member of a character class: a byte (`code`) or a predefined class (`set`).
    type(parser_t), intent(inout) :: ps
    integer, intent(out) :: code
    logical, intent(out) :: is_set
    type(charset_t), intent(out) :: set
    integer :: start
    character :: c

    is_set = .false.
    code = iachar(peek(ps))
    if (peek(ps) /= '\') then
        ps%i = ps%i + 1
        return
    end if
    start = ps%i
    ps%i = ps%i + 1
    if (at_end(ps)) then
        call set_error(ps, 'unterminated character set', start)
        return
    end if
    c = peek(ps)
    ps%i = ps%i + 1
    select case (c)
    case ('d', 'D', 'w', 'W', 's', 'S')
        is_set = .true.
        set = class_escape_set(c)
    case ('0':'7')
        code = read_octal(ps, iachar(c) - iachar('0'), 2)
        if (code > 255) call set_error(ps, 'octal escape value outside of range 0-0o377', start)
    case ('8', '9')
        call set_error(ps, 'bad escape \'//c, start)
    case default
        code = simple_escape(ps, c, start, in_class=.true.)
    end select
end subroutine class_item

pure function class_escape_set(c) result(set)
    character, intent(in) :: c
    type(charset_t) :: set
    select case (c)
    case ('d', 'D')
        set = digit_set()
    case ('w', 'W')
        set = word_set()
    case ('s', 'S')
        set = space_set()
    end select
    if (c == 'D' .or. c == 'W' .or. c == 'S') call set%negate()
end function class_escape_set

recursive function parse_group(ps, flags) result(id)
    !! Groups and extensions: `(...)`, `(?:...)`, `(?P<name>...)`, lookarounds, `(?>...)`, flags, comments.
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer :: id
    integer :: start, local, inner
    logical :: negated

    id = 0
    start = ps%i
    ps%i = ps%i + 1
    if (peek(ps) /= '?' .or. at_end(ps)) then
        id = parse_capture(ps, flags, start, '')
        return
    end if
    ps%i = ps%i + 1
    if (at_end(ps)) then
        call set_error(ps, 'unexpected end of pattern', ps%i)
        return
    end if
    select case (peek(ps))
    case (':')
        ps%i = ps%i + 1
        local = flags
        id = parse_body(ps, local, start)
    case ('P')
        ps%i = ps%i + 1
        if (peek(ps) == '<') then
            ps%i = ps%i + 1
            id = parse_named_group(ps, flags, start)
        else if (peek(ps) == '=') then
            ps%i = ps%i + 1
            id = parse_named_backref(ps, flags, start)
        else
            call set_error(ps, 'unknown extension ?P'//peek(ps), start + 1)
        end if
    case ('<')
        ps%i = ps%i + 1
        if (peek(ps) == '=' .or. peek(ps) == '!') then
            negated = peek(ps) == '!'
            ps%i = ps%i + 1
            local = flags
            inner = parse_body(ps, local, start)
            if (failed(ps)) return
            id = new_node(ps, N_LOOK, 0, merge(1, 0, negated))
            ps%ast%nodes(id)%kids = [inner]
        else
            id = parse_named_group(ps, flags, start)
        end if
    case ('=', '!')
        negated = peek(ps) == '!'
        ps%i = ps%i + 1
        local = flags
        inner = parse_body(ps, local, start)
        if (failed(ps)) return
        id = new_node(ps, N_LOOK, 1, merge(1, 0, negated))
        ps%ast%nodes(id)%kids = [inner]
    case ('>')
        ps%i = ps%i + 1
        local = flags
        inner = parse_body(ps, local, start)
        if (failed(ps)) return
        id = new_node(ps, N_ATOMIC)
        ps%ast%nodes(id)%kids = [inner]
    case ('#')
        do while (.not. at_end(ps))
            if (peek(ps) == ')') exit
            ps%i = ps%i + 1
        end do
        if (at_end(ps)) then
            call set_error(ps, 'missing ), unterminated comment', start)
            return
        end if
        ps%i = ps%i + 1
    case default
        id = parse_flag_group(ps, flags, start)
    end select
end function parse_group

recursive function parse_body(ps, flags, start) result(inner)
    !! The body of a group up to and including its closing `)`.
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer, intent(in) :: start
    integer :: inner

    inner = parse_alt(ps, flags)
    if (failed(ps)) return
    if (at_end(ps)) then
        call set_error(ps, 'missing ), unterminated subpattern', start)
    else if (peek(ps) /= ')') then
        call set_error(ps, 'missing ), unterminated subpattern', start)
    else
        ps%i = ps%i + 1
    end if
end function parse_body

recursive function parse_capture(ps, flags, start, name) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: flags
    integer, intent(in) :: start
    character(len=*), intent(in) :: name
    integer :: id
    integer :: g, local, inner

    id = 0
    ps%ast%ngroups = ps%ast%ngroups + 1
    g = ps%ast%ngroups
    ps%ast%names = [ps%ast%names, regex_string_t(name)]
    ps%ast%closed = [ps%ast%closed, .false.]
    local = flags
    inner = parse_body(ps, local, start)
    if (failed(ps)) return
    ps%ast%closed(g) = .true.
    id = new_node(ps, N_GROUP, g)
    ps%ast%nodes(id)%kids = [inner]
end function parse_capture

recursive function parse_named_group(ps, flags, start) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: flags
    integer, intent(in) :: start
    integer :: id
    character(len=:), allocatable :: name

    id = 0
    name = read_name(ps, '>')
    if (failed(ps)) return
    if (find_group(ps%ast, name) > 0) then
        call set_error(ps, "redefinition of group name '"//name//"'", start)
        return
    end if
    id = parse_capture(ps, flags, start, name)
end function parse_named_group

function parse_named_backref(ps, flags, start) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: flags
    integer, intent(in) :: start
    integer :: id
    character(len=:), allocatable :: name
    integer :: g

    id = 0
    name = read_name(ps, ')')
    if (failed(ps)) return
    g = find_group(ps%ast, name)
    if (g == 0) then
        call set_error(ps, "unknown group name '"//name//"'", start)
        return
    end if
    id = backref_node(ps, g, flags, start)
end function parse_named_backref

recursive function parse_flag_group(ps, flags, start) result(id)
    !! Inline flags `(?im)` (apply to the rest of the enclosing group) or scoped `(?i-m:...)`.
    type(parser_t), intent(inout) :: ps
    integer, intent(inout) :: flags
    integer, intent(in) :: start
    integer :: id
    integer :: on, off, bit, local
    logical :: in_off

    id = 0
    on = 0
    off = 0
    in_off = .false.
    do
        if (at_end(ps)) then
            call set_error(ps, 'missing -, : or )', ps%i)
            return
        end if
        bit = 0
        select case (peek(ps))
        case ('i')
            bit = F_IGNORECASE
        case ('m')
            bit = F_MULTILINE
        case ('s')
            bit = F_DOTALL
        case ('a', 'u', 'L')
            ! ASCII semantics are the only ones supported; accept and ignore the flags
            if (in_off) then
                call set_error(ps, 'bad inline flags: cannot turn off flags ''a'', ''u'' and ''L''', ps%i)
                return
            end if
        case ('-')
            if (in_off) then
                call set_error(ps, 'bad inline flags: flag turned on and off', ps%i)
                return
            end if
            in_off = .true.
        case (':')
            ps%i = ps%i + 1
            local = iand(ior(flags, on), not(off))
            id = parse_body(ps, local, start)
            return
        case (')')
            if (in_off) then
                call set_error(ps, 'missing :', ps%i)
                return
            end if
            ps%i = ps%i + 1
            flags = ior(flags, on)
            return
        case default
            if (is_alpha_code(iachar(peek(ps)))) then
                call set_error(ps, 'unknown flag', ps%i)
            else
                call set_error(ps, 'unknown extension ?'//peek(ps), start + 1)
            end if
            return
        end select
        if (in_off) then
            off = ior(off, bit)
        else
            on = ior(on, bit)
        end if
        ps%i = ps%i + 1
    end do
end function parse_flag_group

function read_name(ps, terminator) result(name)
    !! Read a group name up to `terminator` and check that it is an identifier.
    type(parser_t), intent(inout) :: ps
    character, intent(in) :: terminator
    character(len=:), allocatable :: name
    integer :: j, k

    name = ''
    j = index(ps%p(ps%i:), terminator)
    if (j == 0) then
        call set_error(ps, 'missing '//terminator//', unterminated name', ps%i)
        return
    end if
    name = ps%p(ps%i:ps%i + j - 2)
    if (len(name) == 0) then
        call set_error(ps, 'missing group name', ps%i)
        return
    end if
    do k = 1, len(name)
        if (.not. is_word_code(iachar(name(k:k))) .or. (k == 1 .and. is_digit_code(iachar(name(1:1))))) then
            call set_error(ps, "bad character in group name '"//name//"'", ps%i)
            return
        end if
    end do
    ps%i = ps%i + j
end function read_name

pure integer function find_group(ast, name) result(g)
    type(ast_t), intent(in) :: ast
    character(len=*), intent(in) :: name
    do g = 1, ast%ngroups
        if (ast%names(g)%s == name .and. len(ast%names(g)%s) == len(name)) return
    end do
    g = 0
end function find_group

function backref_node(ps, g, flags, start) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: g, flags, start
    integer :: id

    id = 0
    if (g > ps%ast%ngroups) then
        call set_error(ps, 'invalid group reference '//to_str(g), start)
    else if (.not. ps%ast%closed(g)) then
        call set_error(ps, 'cannot refer to an open group', start)
    else
        id = new_node(ps, N_BACKREF, g, merge(1, 0, iand(flags, F_IGNORECASE) /= 0))
    end if
end function backref_node

function char_node(ps, code, flags) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: code, flags
    integer :: id
    logical :: icase
    icase = iand(flags, F_IGNORECASE) /= 0 .and. is_alpha_code(code)
    id = new_node(ps, N_CHAR, code, merge(1, 0, icase))
end function char_node

function set_node(ps, set) result(id)
    type(parser_t), intent(inout) :: ps
    type(charset_t), intent(in) :: set
    integer :: id
    type(charset_t), allocatable :: tmp(:)

    if (ps%ast%nsets == size(ps%ast%sets)) then
        allocate (tmp(2*size(ps%ast%sets)))
        tmp(1:ps%ast%nsets) = ps%ast%sets(1:ps%ast%nsets)
        call move_alloc(tmp, ps%ast%sets)
    end if
    ps%ast%nsets = ps%ast%nsets + 1
    ps%ast%sets(ps%ast%nsets) = set
    id = new_node(ps, N_SET, ps%ast%nsets)
end function set_node

function new_node(ps, kind, a, b, c) result(id)
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: kind
    integer, intent(in), optional :: a, b, c
    integer :: id
    type(node_t), allocatable :: tmp(:)

    if (ps%ast%n == size(ps%ast%nodes)) then
        allocate (tmp(2*size(ps%ast%nodes)))
        tmp(1:ps%ast%n) = ps%ast%nodes(1:ps%ast%n)
        call move_alloc(tmp, ps%ast%nodes)
    end if
    ps%ast%n = ps%ast%n + 1
    id = ps%ast%n
    ps%ast%nodes(id)%kind = kind
    if (present(a)) ps%ast%nodes(id)%a = a
    if (present(b)) ps%ast%nodes(id)%b = b
    if (present(c)) ps%ast%nodes(id)%c = c
end function new_node

integer function read_octal(ps, code, max_digits) result(val)
    !! Read up to `max_digits` more octal digits, starting from the value `code`.
    type(parser_t), intent(inout) :: ps
    integer, intent(in) :: code, max_digits
    integer :: k
    val = code
    do k = 1, max_digits
        if (at_end(ps)) exit
        if (.not. is_octal(peek(ps))) exit
        val = 8*val + iachar(peek(ps)) - iachar('0')
        ps%i = ps%i + 1
    end do
end function read_octal

elemental logical function is_octal(c)
    character, intent(in) :: c
    is_octal = c >= '0' .and. c <= '7'
end function is_octal

elemental integer function hex_value(c) result(h)
    character, intent(in) :: c
    select case (c)
    case ('0':'9')
        h = iachar(c) - iachar('0')
    case ('a':'f')
        h = iachar(c) - iachar('a') + 10
    case ('A':'F')
        h = iachar(c) - iachar('A') + 10
    case default
        h = -1
    end select
end function hex_value

pure character function peek(ps, offset)
    !! Character at the current position plus `offset`, or `achar(0)` past the end.
    !! Callers that must tell a real NUL from the end use [[at_end]].
    type(parser_t), intent(in) :: ps
    integer, intent(in), optional :: offset
    integer :: j
    j = ps%i
    if (present(offset)) j = j + offset
    if (j > len(ps%p)) then
        peek = achar(0)
    else
        peek = ps%p(j:j)
    end if
end function peek

pure logical function at_end(ps)
    type(parser_t), intent(in) :: ps
    at_end = ps%i > len(ps%p)
end function at_end

pure logical function failed(ps)
    type(parser_t), intent(in) :: ps
    failed = allocated(ps%errmsg)
end function failed

subroutine set_error(ps, msg, pos)
    !! Record the first error only, with its 1-based position in the pattern.
    type(parser_t), intent(inout) :: ps
    character(len=*), intent(in) :: msg
    integer, intent(in) :: pos
    if (.not. allocated(ps%errmsg)) ps%errmsg = msg//' at position '//to_str(pos)
end subroutine set_error

end module fregex_parser


!>>>>> build/dependencies/fregex/src/fregex_program.f90
module fregex_program
!! Compiled form of a regular expression: a list of instructions for the backtracking VM.
use fregex_string, only: regex_string_t
implicit none
private

integer, parameter, public :: NOPERANDS = 5

! Instructions. Operands are code(1:NOPERANDS, pc); "pc" values are instruction indices.
integer, parameter, public :: OP_MATCH = 1        !! whole pattern matched
integer, parameter, public :: OP_CHAR = 2         !! 1: byte
integer, parameter, public :: OP_CHARI = 3        !! 1: lowercase byte, compared case-insensitively
integer, parameter, public :: OP_ANY = 4          !! any byte except newline
integer, parameter, public :: OP_ANYNL = 5        !! any byte
integer, parameter, public :: OP_SET = 6          !! 1: index into sets
integer, parameter, public :: OP_SPLIT = 7        !! 1: preferred pc, 2: alternative pc
integer, parameter, public :: OP_JMP = 8          !! 1: pc
integer, parameter, public :: OP_SAVE = 9         !! 1: capture slot
integer, parameter, public :: OP_MARK = 10        !! 1: register; stores the current position
integer, parameter, public :: OP_CHECK_EXIT = 11  !! 1: register, 2: pc to jump to if nothing was consumed since the mark
integer, parameter, public :: OP_BOL = 12
integer, parameter, public :: OP_BOL_M = 13
integer, parameter, public :: OP_EOL = 14
integer, parameter, public :: OP_EOL_M = 15
integer, parameter, public :: OP_BOS = 16
integer, parameter, public :: OP_EOS = 17
integer, parameter, public :: OP_WORDB = 18
integer, parameter, public :: OP_NWORDB = 19
integer, parameter, public :: OP_BACKREF = 20     !! 1: group, 2: 1 if case-insensitive
integer, parameter, public :: OP_LOOKAHEAD = 21   !! 1: pc after the body, 2: 1 if negated
integer, parameter, public :: OP_LOOKBEHIND = 22  !! 1: pc after the body, 2: negated, 3: min width, 4: max width, 5: register
integer, parameter, public :: OP_ENDAT = 23       !! 1: register; succeeds only at the stored position
integer, parameter, public :: OP_ATOMIC = 24      !! 1: pc after the body
integer, parameter, public :: OP_SUCCEED = 25     !! end of a lookaround or atomic body
integer, parameter, public :: OP_REPEAT = 26      !! repeat of one byte test: 1: test op (OP_CHAR, OP_CHARI, OP_ANY,
                                                  !! OP_ANYNL or OP_SET), 2: its operand, 3: min, 4: max or
                                                  !! UNBOUNDED, 5: REP_GREEDY, REP_LAZY or REP_POSSESSIVE

integer, parameter, public :: REP_GREEDY = 0, REP_LAZY = 1, REP_POSSESSIVE = 2

integer, parameter, public :: UNBOUNDED = -1      !! lookbehind with no maximum width

type, public :: program_t
    integer :: n = 0
    integer, allocatable :: code(:, :)          !! (0:NOPERANDS, capacity); code(0, pc) is the opcode
    logical, allocatable :: sets(:, :)          !! (-1:255, nsets): byte tables of the character classes
    integer :: ngroups = 0                      !! capturing groups, not counting group 0
    integer :: nregs = 0
    type(regex_string_t), allocatable :: names(:)     !! (1:ngroups)
    logical :: anchored = .false.               !! can only match at the start of the string
    logical :: line_start = .false.             !! can only match at the start of a line (`(?m)^...`)
    logical :: use_first = .false.              !! a match must start with a byte in `first`
    logical :: first(-1:255) = .false.
    character(len=:), allocatable :: prefix     !! literal text at the start of every match (may be empty)
    integer :: prefix_offset = 0                !! how many bytes of `prefix` come before the match
                                                !! (from a leading lookbehind such as `(?<=call )`)
contains
    procedure :: emit
    procedure :: new_reg
end type program_t

contains

function emit(self, op, a, b, c, d, e) result(pc)
    !! Append an instruction and return its pc.
    class(program_t), intent(inout) :: self
    integer, intent(in) :: op
    integer, intent(in), optional :: a, b, c, d, e
    integer :: pc
    integer, allocatable :: tmp(:, :)

    if (.not. allocated(self%code)) allocate (self%code(0:NOPERANDS, 64))
    if (self%n == size(self%code, 2)) then
        allocate (tmp(0:NOPERANDS, 2*self%n))
        tmp(:, 1:self%n) = self%code(:, 1:self%n)
        call move_alloc(tmp, self%code)
    end if
    self%n = self%n + 1
    pc = self%n
    self%code(:, pc) = 0
    self%code(0, pc) = op
    if (present(a)) self%code(1, pc) = a
    if (present(b)) self%code(2, pc) = b
    if (present(c)) self%code(3, pc) = c
    if (present(d)) self%code(4, pc) = d
    if (present(e)) self%code(5, pc) = e
end function emit

integer function new_reg(self) result(reg)
    class(program_t), intent(inout) :: self
    self%nregs = self%nregs + 1
    reg = self%nregs
end function new_reg

end module fregex_program


!>>>>> build/dependencies/fregex/src/fregex_template.f90
module fregex_template
!! Replacement templates for substitution, e.g. `'\1-\g<name>'`, with Python `re.sub` syntax.
use fregex_string, only: regex_string_t, string_builder_t, to_str, is_alpha_code, is_digit_code
implicit none
private

public :: parse_template, expand_template

type, public :: template_t
    !! A sequence of pieces, each either literal text (`group == -1`) or a group reference.
    integer, allocatable :: group(:)
    type(regex_string_t), allocatable :: text(:)
end type template_t

contains

subroutine parse_template(repl, names, tmpl, errmsg)
    !! Parse `repl`. Supports `\1`..`\99`, `\g<n>`, `\g<name>`, the escapes `\n \t \r \f \v \a \b \\`,
    !! and octal `\0`, `\0oo`, `\ooo`. Other non-letter escapes are kept as written.
    character(len=*), intent(in) :: repl
    type(regex_string_t), intent(in) :: names(:)
    type(template_t), intent(out) :: tmpl
    character(len=:), allocatable, intent(out) :: errmsg

    type(string_builder_t) :: lit
    integer :: i, j, g, code, ngroups
    character :: d
    character(len=:), allocatable :: name

    ngroups = size(names)
    allocate (tmpl%group(0), tmpl%text(0))
    i = 1
    do while (i <= len(repl))
        if (repl(i:i) /= '\') then
            call lit%append(repl(i:i))
            i = i + 1
            cycle
        end if
        if (i == len(repl)) then
            errmsg = 'bad escape (end of pattern) at position '//to_str(i)
            return
        end if
        d = repl(i + 1:i + 1)
        select case (d)
        case ('g')
            if (i + 2 > len(repl)) then
                errmsg = 'missing < at position '//to_str(i + 2)
                return
            end if
            if (repl(i + 2:i + 2) /= '<') then
                errmsg = 'missing < at position '//to_str(i + 2)
                return
            end if
            j = index(repl(i + 3:), '>')
            if (j == 0) then
                errmsg = 'missing >, unterminated name at position '//to_str(i + 3)
                return
            end if
            name = repl(i + 3:i + j + 1)
            if (len(name) == 0) then
                errmsg = 'missing group name at position '//to_str(i + 3)
                return
            end if
            if (verify(name, '0123456789') == 0) then
                if (len(name) > 6) then
                    g = huge(1)
                else
                    read (name, *) g
                end if
            else
                do g = 1, ngroups
                    if (len(names(g)%s) == len(name)) then
                        if (names(g)%s == name) exit
                    end if
                end do
                if (g > ngroups) then
                    errmsg = "unknown group name '"//name//"' at position "//to_str(i + 3)
                    return
                end if
            end if
            if (g > ngroups) then
                errmsg = 'invalid group reference '//name//' at position '//to_str(i + 3)
                return
            end if
            call add_group(g)
            i = i + j + 3
        case ('0')
            code = 0
            i = i + 2
            do j = 1, 2
                if (i > len(repl)) exit
                if (repl(i:i) < '0' .or. repl(i:i) > '7') exit
                code = 8*code + iachar(repl(i:i)) - iachar('0')
                i = i + 1
            end do
            call lit%append(achar(code))
        case ('1':'9')
            if (i + 3 <= len(repl)) then
                if (verify(repl(i + 1:i + 3), '01234567') == 0) then
                    read (repl(i + 1:i + 3), '(o3)') code
                    if (code > 255) then
                        errmsg = 'octal escape value outside of range 0-0o377 at position '//to_str(i)
                        return
                    end if
                    call lit%append(achar(code))
                    i = i + 4
                    cycle
                end if
            end if
            g = iachar(d) - iachar('0')
            j = i + 2
            if (j <= len(repl)) then
                if (is_digit_code(iachar(repl(j:j)))) then
                    g = 10*g + iachar(repl(j:j)) - iachar('0')
                    j = j + 1
                end if
            end if
            if (g > ngroups) then
                errmsg = 'invalid group reference '//to_str(g)//' at position '//to_str(i + 1)
                return
            end if
            call add_group(g)
            i = j
        case default
            select case (d)
            case ('n')
                call lit%append(achar(10))
            case ('t')
                call lit%append(achar(9))
            case ('r')
                call lit%append(achar(13))
            case ('f')
                call lit%append(achar(12))
            case ('v')
                call lit%append(achar(11))
            case ('a')
                call lit%append(achar(7))
            case ('b')
                call lit%append(achar(8))
            case ('\')
                call lit%append('\')
            case default
                if (is_alpha_code(iachar(d))) then
                    errmsg = 'bad escape \'//d//' at position '//to_str(i)
                    return
                end if
                call lit%append('\'//d)
            end select
            i = i + 2
        end select
    end do
    call flush_literal()

contains

    subroutine flush_literal()
        character(len=:), allocatable :: text
        text = lit%get()
        if (len(text) == 0) return
        tmpl%group = [tmpl%group, -1]
        tmpl%text = [tmpl%text, regex_string_t(text)]
        call lit%clear()
    end subroutine flush_literal

    subroutine add_group(k)
        integer, intent(in) :: k
        call flush_literal()
        tmpl%group = [tmpl%group, k]
        tmpl%text = [tmpl%text, regex_string_t('')]
    end subroutine add_group

end subroutine parse_template

pure subroutine expand_template(tmpl, subject, caps, out)
    !! Append the replacement text for one match to `out`. `caps` holds VM capture positions
    !! (0 if unset).
    type(template_t), intent(in) :: tmpl
    character(len=*), intent(in) :: subject
    integer, intent(in) :: caps(0:)
    type(string_builder_t), intent(inout) :: out
    integer :: k, g

    do k = 1, size(tmpl%group)
        g = tmpl%group(k)
        if (g < 0) then
            call out%append(tmpl%text(k)%s)
        else if (caps(2*g) > 0 .and. caps(2*g + 1) > 0) then
            call out%append(subject(caps(2*g):caps(2*g + 1) - 1))
        end if
    end do
end subroutine expand_template

end module fregex_template


!>>>>> build/dependencies/toml-f/src/tomlf/datetime.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML datetime value representation
!>
!> This module provides the [[toml_datetime]] type for representing TOML
!> datetime values. TOML supports four datetime formats:
!>
!> - Offset date-time: `1979-05-27T07:32:00Z`
!> - Local date-time: `1979-05-27T07:32:00`
!> - Local date: `1979-05-27`
!> - Local time: `07:32:00`
!>
!> The [[toml_datetime]] type combines [[toml_date]] and [[toml_time]]
!> components to represent any of these formats.
module tomlf_datetime
   use tomlf_constants, only : tfc
   implicit none
   private

   public :: toml_datetime, toml_time, toml_date, to_string, has_date, has_time
   public :: operator(==)


   !> TOML time value (HH:MM:SS.sssssZ...)
   type :: toml_time
      integer :: hour = -1
      integer :: minute = -1
      integer :: second = -1
      integer :: msec = -1
      character(len=:), allocatable :: zone
   end type

   interface toml_time
      module procedure :: new_toml_time
   end interface toml_time


   !> TOML date value (YYYY-MM-DD)
   type :: toml_date
      integer :: year = -1
      integer :: month = -1
      integer :: day = -1
   end type


   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date) :: date
      type(toml_time) :: time
   end type


   !> Create a new TOML datetime value
   interface toml_datetime
      module procedure :: new_datetime
      module procedure :: new_datetime_from_string
   end interface toml_datetime


   interface operator(==)
      module procedure :: compare_datetime
   end interface operator(==)


   interface to_string
      module procedure :: to_string_datetime
   end interface to_string


contains


pure function new_datetime(year, month, day, hour, minute, second, msecond, zone) &
      & result(datetime)
   integer, intent(in), optional :: year
   integer, intent(in), optional :: month
   integer, intent(in), optional :: day
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: msecond
   character(len=*), intent(in), optional :: zone
   type(toml_datetime) :: datetime

   if (present(year) .and. present(month) .and. present(day)) then
      datetime%date%year = year
      datetime%date%month = month
      datetime%date%day = day
   end if

   if (present(hour) .and. present(minute) .and. present(second)) then
      datetime%time%hour = hour
      datetime%time%minute = minute
      datetime%time%second = second
      if (present(msecond)) then
         datetime%time%msec = msecond
      end if
      if (present(zone)) then
         datetime%time%zone = zone
      end if
   end if
end function new_datetime


pure function new_datetime_from_string(string) result(datetime)
   character(len=*), intent(in) :: string
   type(toml_datetime) :: datetime

   type(toml_date) :: date
   type(toml_time) :: time

   integer :: it, tmp, first
   character(*, tfc), parameter :: num = "0123456789"
   integer, allocatable :: msec(:)
   logical :: has_seconds

   first = 0

   if (all([string(first+5:first+5), string(first+8:first+8)] == "-")) then
      date%year = 0
      do it = first + 1, first + 4
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         date%year = date%year * 10 + tmp
      end do

      date%month = 0
      do it = first + 6, first + 7
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         date%month = date%month * 10 + tmp
      end do

      date%day = 0
      do it = first + 9, first + 10
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         date%day = date%day * 10 + tmp
      end do

      first = first + 11
      datetime%date = date
   end if

   if (first >= len(string)) return
   ! Check for time: HH:MM format (colon at position 3)
   if (string(first+3:first+3) == ":") then
      time%hour = 0
      do it = first + 1, first + 2
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         time%hour = time%hour * 10 + tmp
      end do

      time%minute = 0
      do it = first + 4, first + 5
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         time%minute = time%minute * 10 + tmp
      end do

      has_seconds = first + 6 <= len(string) .and. string(first+6:first+6) == ":"
      if (has_seconds) then
         time%second = 0
         do it = first + 7, first + 8
            tmp = scan(num, string(it:it)) - 1
            if (tmp < 0) exit
            time%second = time%second * 10 + tmp
         end do
         first = first + 8
      else
         first = first + 5
      end if

      if (first < len(string)) then
         if (string(first+1:first+1) == ".") then
            msec = [integer::]
            do it = first + 2, len(string)
               tmp = scan(num, string(it:it)) - 1
               if (tmp < 0) exit
               msec = [msec, tmp]
            end do
            first = it - 1

            msec = [msec, 0, 0, 0, 0, 0, 0]
            time%msec = sum(msec(1:6) * [100000, 10000, 1000, 100, 10, 1])
         end if
      end if

      if (first < len(string)) then
         time%zone = ""
         do it = first + 1, len(string)
            time%zone = time%zone // string(it:it)
         end do
         if (time%zone == "z") time%zone = "Z"
      end if
      datetime%time = time
   end if

end function new_datetime_from_string


pure function to_string_datetime(datetime) result(str)
   type(toml_datetime), intent(in) :: datetime
   character(kind=tfc, len=:), allocatable :: str

   str = ""
   if (has_date(datetime)) then
      str = str // to_string_date(datetime%date)
   end if

   if (has_time(datetime)) then
      if (has_date(datetime)) then
         str = str // 'T'
      end if
      str = str // to_string_time(datetime%time)
   end if
end function to_string_datetime

pure function to_string_date(date) result(str)
   type(toml_date), intent(in) :: date
   character(:, tfc), allocatable :: str

   allocate(character(10, tfc) :: str)
   write(str, '(i4.4,"-",i2.2,"-",i2.2)') &
      &  date%year, date%month, date%day
end function to_string_date

pure function to_string_time(time) result(str)
   type(toml_time), intent(in) :: time
   character(:, tfc), allocatable :: str

   integer :: second, msec, width
   character(1), parameter :: places(6) = ["1", "2", "3", "4", "5", "6"]

   ! TOML 1.1 allows omitted seconds in the input representation, but the
   ! compliance JSON format requires canonical RFC 3339 output with seconds.
   if (time%second < 0) then
      second = 0
   else
      second = time%second
   end if

   if (time%msec < 0) then
      allocate(character(8, tfc) :: str)
      write(str, '(i2.2,":",i2.2,":",i2.2)') &
         &  time%hour, time%minute, second
   else
      width = 6
      msec = time%msec
      do while(mod(msec, 10) == 0 .and. width > 3)
         width = width - 1
         msec = msec / 10
      end do
      allocate(character(9 + width, tfc) :: str)
      write(str, '(i2.2,":",i2.2,":",i2.2,".",i'//places(width)//'.'//places(width)//')') &
         &  time%hour, time%minute, second, msec
   end if
   if (allocated(time%zone)) str = str // trim(time%zone)
end function to_string_time


pure function has_date(datetime)
   class(toml_datetime), intent(in) :: datetime
   logical :: has_date
   has_date = (datetime%date%year >= 0) .and. &
      & (datetime%date%month >= 0) .and. &
      & (datetime%date%day >= 0)
end function has_date

pure function has_time(datetime)
   class(toml_datetime), intent(in) :: datetime
   logical :: has_time
   has_time = (datetime%time%hour >= 0) .and. &
      & (datetime%time%minute >= 0)
end function has_time


!> Constructor for toml_time type, necessary due to PGI bug in NVHPC 20.7 and 20.9
elemental function new_toml_time(hour, minute, second, msec, zone) &
      & result(self)
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: msec
   character(len=*), intent(in), optional :: zone
   type(toml_time) :: self
   if (present(hour)) self%hour = hour
   if (present(minute)) self%minute = minute
   if (present(second)) self%second = second
   if (present(msec)) self%msec = msec
   if (present(zone)) self%zone = zone
end function new_toml_time


pure function compare_datetime(lhs, rhs) result(match)
   type(toml_datetime), intent(in) :: lhs
   type(toml_datetime), intent(in) :: rhs
   logical :: match

   match = (has_date(lhs) .eqv. has_date(rhs)) &
      & .and. (has_time(lhs) .eqv. has_time(rhs))
   if (has_date(lhs) .and. has_date(rhs)) then
      match = match .and. compare_date(lhs%date, rhs%date)
   end if

   if (has_time(lhs) .and. has_time(rhs)) then
      match = match .and. compare_time(lhs%time, rhs%time)
   end if
end function compare_datetime


pure function compare_date(lhs, rhs) result(match)
   type(toml_date), intent(in) :: lhs
   type(toml_date), intent(in) :: rhs
   logical :: match

   match = lhs%year == rhs%year .and. lhs%month == rhs%month .and. lhs%day == rhs%day
end function compare_date


pure function compare_time(lhs, rhs) result(match)
   type(toml_time), intent(in) :: lhs
   type(toml_time), intent(in) :: rhs
   logical :: match

   integer :: lms, rms

   lms = max(lhs%msec, 0)
   rms = max(rhs%msec, 0)

   match = lhs%hour == rhs%hour .and. lhs%minute == rhs%minute .and. lhs%second == rhs%second &
      & .and. lms == rms .and. allocated(lhs%zone) .eqv. allocated(rhs%zone)

   if (allocated(lhs%zone) .and. allocated(rhs%zone)) then
      match = match .and. lhs%zone == rhs%zone
   end if
end function compare_time


end module tomlf_datetime


!>>>>> build/dependencies/toml-f/src/tomlf/error.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Error handling for TOML Fortran
!>
!> This module provides the [[toml_error]] type for error reporting and
!> the [[toml_stat]] enumerator for status codes returned by various
!> TOML Fortran procedures.
!>
!> ## Error Handling
!>
!> Most parsing and access functions accept an optional `error` argument
!> of type [[toml_error]]. If an error occurs, this will be allocated
!> and contain a descriptive message:
!>
!>```fortran
!> type(toml_error), allocatable :: error
!> call toml_load(table, "config.toml", error=error)
!> if (allocated(error)) print '(a)', error%message
!>```
!>
!> ## Status Codes
!>
!> The [[toml_stat]] enumerator provides named constants for common
!> error conditions like `toml_stat%duplicate_key` or `toml_stat%type_mismatch`.
module tomlf_error
   use tomlf_constants, only : tfc, TOML_NEWLINE
   implicit none
   private

   public :: toml_stat, toml_error, make_error


   !> Possible TOML Fortran status codes
   type :: enum_stat

      !> Successful run
      integer :: success = 0

      !> Internal error:
      !>
      !> General undefined error state, usually caused by algorithmic errors.
      integer :: fatal = -1

      !> Duplicate key encountered
      integer :: duplicate_key = -2

      !> Incorrect type when reading a value
      integer :: type_mismatch = -3

      !> Conversion error when downcasting a value
      integer :: conversion_error = -4

      !> Key not present in table
      integer :: missing_key = -5

   end type enum_stat

   !> Actual enumerator for return states
   !>
   !> | Name | Description |
   !> |------|-------------|
   !> | `success` | Operation completed successfully |
   !> | `fatal` | Internal error or undefined error state |
   !> | `duplicate_key` | Duplicate key encountered in table |
   !> | `type_mismatch` | Incorrect type when reading a value |
   !> | `conversion_error` | Error when converting or downcasting a value |
   !> | `missing_key` | Requested key not present in table |
   type(enum_stat), parameter :: toml_stat = enum_stat()


   !> Error message produced by TOML-Fortran
   type :: toml_error

      !> Error code
      integer :: stat = toml_stat%fatal

      !> Payload of the error
      character(kind=tfc, len=:), allocatable :: message

   end type toml_error


contains

!> Create new error message
subroutine make_error(error, message, stat)
   !> Error report
   type(toml_error), allocatable, intent(out) :: error
   !> Message for the error
   character(*, tfc), intent(in) :: message
   !> Status code
   integer, intent(in), optional :: stat

   allocate(error)
   error%message = message
   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%fatal
   end if
end subroutine make_error

end module tomlf_error


!>>>>> build/dependencies/toml-f/src/tomlf/utils/io.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Utilities for handling input and output operations
module tomlf_utils_io
   use tomlf_constants, only : tfc
   implicit none
   private

   public :: read_whole_file, read_whole_line


contains

!> Read a whole file into an array of characters
subroutine read_whole_file(filename, string, stat)
   !> File to read
   character(*, tfc), intent(in) :: filename
   !> Array of characters representing the file
   character(:, tfc), allocatable, intent(out) :: string
   !> Error status
   integer, intent(out) :: stat

   integer :: io, length

   open(file=filename, &
      & status="old", &
      & access="stream", &
      & position="append", &
      & newunit=io, &
      & iostat=stat)
   if (stat == 0) then
      inquire(unit=io, pos=length)
      allocate(character(length-1, tfc) :: string, stat=stat)
   end if
   if (stat == 0) then
      read(io, pos=1, iostat=stat) string(:length-1)
   end if
   if (stat == 0) then
      close(io)
   end if
end subroutine read_whole_file

!> Read a whole line from a formatted unit into a deferred length character variable
subroutine read_whole_line(io, string, stat)
   !> Formatted IO unit
   integer, intent(in) :: io
   !> Line to read
   character(:, tfc), allocatable, intent(out) :: string
   !> Status of operation
   integer, intent(out) :: stat

   integer, parameter :: bufsize = 4096
   character(bufsize, tfc) :: buffer, msg
   integer :: chunk
   logical :: opened

   if (io /= -1) then
      inquire(unit=io, opened=opened)
   else
      opened = .false.
   end if

   if (opened) then
      open(unit=io, pad="yes", iostat=stat)
   else
      stat = 1
      msg = "Unit is not connected"
   end if

   string = ""
   do while (stat == 0)
      read(io, '(a)', advance='no', iostat=stat, size=chunk) buffer
      if (stat > 0) exit
      string = string // buffer(:chunk)
   end do
   if (is_iostat_eor(stat)) stat = 0
end subroutine read_whole_line

end module tomlf_utils_io


!>>>>> ././src/ffmt_lexer.f90
module ffmt_lexer
!! Lexer for free-form Fortran.
!!
!! The lexer never fails: malformed input (e.g. an unterminated string) still produces tokens
!! that cover every byte, flagged where relevant, so the formatter can leave such code alone.
use ffmt_tokens
use ffmt_strings, only: lower_char, lowercase
implicit none
private

public :: tokenize

character, parameter :: LF = achar(10), CR = achar(13), TAB = achar(9)

type :: lexer_t
    !! Lexer state for one source text.
    integer :: line = 1               !! current line
    logical :: line_start = .true.    !! only blanks so far on the current line
    logical :: glue_pending = .false. !! the previous line ended with a code character directly followed by `&`
    logical :: glue_next = .false.    !! the next code token continues the previous one
end type lexer_t

contains

subroutine tokenize(src, tokens)
    !! Split `src` into tokens.
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(out) :: tokens    !! its tokens, covering every byte

    type(lexer_t) :: lx
    integer :: i, j, n, kind, sub
    logical :: at_start
    character :: c

    n = len(src)
    i = 1
    if (n >= 3) then
        if (src(1:3) == achar(239)//achar(187)//achar(191)) then
            call add(tokens, lx, src, TK_BOM, 1, 3)
            i = 4
        end if
    end if
    do while (i <= n)
        c = src(i:i)
        sub = 0

        if (c == LF .or. c == CR) then
            j = i
            if (c == CR .and. peek(src, i + 1) == LF) j = i + 1
            call add(tokens, lx, src, TK_NEWLINE, i, j)
            lx%line_start = .true.
            i = j + 1
            cycle
        end if
        if (c == ' ' .or. c == TAB) then
            j = i
            do while (is_blank(peek(src, j + 1)))
                j = j + 1
            end do
            call add(tokens, lx, src, TK_SPACE, i, j)
            i = j + 1
            cycle
        end if

        at_start = lx%line_start
        lx%line_start = .false.
        if (at_start .and. is_preproc_start(src, i)) then
            call add(tokens, lx, src, TK_PREPROC, i, preproc_end(src, i))
            i = tokens%tok(tokens%n)%last + 1
            cycle
        end if

        select case (c)
        case ('!')
            j = line_end(src, i)
            call add(tokens, lx, src, TK_COMMENT, i, j, comment_kind(src(i:j)))
        case ('&')
            j = i
            if (at_start) then
                sub = CT_LEADING
            else
                sub = CT_TRAILING
            end if
            call add(tokens, lx, src, TK_CONT, i, j, sub)
        case ("'", '"')
            j = string_end(src, i, sub)
            call add(tokens, lx, src, TK_STRING, i, j, sub)
        case ('0':'9')
            call lex_number(src, i, j, kind)
            if (peek(src, j + 1) == '_' .and. is_quote(peek(src, j + 2))) then
                ! kind parameter prefix of a string: 1_'abc'
                j = string_end(src, j + 2, sub)
                kind = TK_STRING
            end if
            call add(tokens, lx, src, kind, i, j, sub)
        case ('.')
            if (is_digit(peek(src, i + 1))) then
                call lex_number(src, i, j, kind)
                call add(tokens, lx, src, kind, i, j)
            else if (dotop_at(src, i)) then
                j = dotop_end(src, i)
                call add(tokens, lx, src, TK_DOTOP, i, j)
            else
                j = i
                call add(tokens, lx, src, TK_OTHER, i, j)
            end if
        case ('a':'z', 'A':'Z', '_')   ! a leading `_` is not standard, but appears in generated code
            j = name_end(src, i)
            kind = TK_NAME
            if (is_quote(peek(src, j + 1))) then
                if (j == i .and. index('bozBOZ', c) > 0) then
                    kind = TK_BOZ
                    j = string_end(src, j + 1, sub)
                else if (src(j:j) == '_') then
                    kind = TK_STRING
                    j = string_end(src, j + 1, sub)
                end if
            end if
            call add(tokens, lx, src, kind, i, j, sub)
        case default
            j = fypp_inline_end(src, i)
            if (j > 0) then
                call add(tokens, lx, src, TK_FYPP, i, j)
            else
                j = operator_end(src, i)
                if (j > 0) then
                    call add(tokens, lx, src, TK_OP, i, j)
                else
                    j = i
                    call add(tokens, lx, src, TK_OTHER, i, j)
                end if
            end if
        end select
        i = j + 1
    end do
end subroutine tokenize

subroutine add(tokens, lx, src, kind, first, last, sub)
    !! Append a token, maintaining the line count and the glue state for split tokens.
    type(token_list_t), intent(inout) :: tokens   !! token list to append to
    type(lexer_t), intent(inout) :: lx            !! lexer state
    character(len=*), intent(in) :: src           !! source text
    integer, intent(in) :: kind                   !! token kind, one of the TK_* constants
    integer, intent(in) :: first                  !! first byte of the token in `src`
    integer, intent(in) :: last                   !! last byte of the token in `src`
    integer, intent(in), optional :: sub          !! sub-kind (CM_*, CT_*, STR_*); 0 if absent
    type(token_t) :: t
    integer :: k

    t%kind = kind
    t%first = first
    t%last = last
    t%line = lx%line
    if (present(sub)) t%sub = sub

    if (kind == TK_CONT) then
        if (t%sub == CT_TRAILING) then
            lx%glue_pending = first > 1 .and. .not. is_blank(peek(src, first - 1))
        else
            ! a leading `&` directly followed by code joins it to the end of the previous line
            lx%glue_next = lx%glue_pending .and. is_code_char(peek(src, last + 1))
            lx%glue_pending = .false.
        end if
    else if (is_code_kind(kind)) then
        t%glued = lx%glue_next
        lx%glue_next = .false.
        lx%glue_pending = .false.
    end if

    do k = first, last
        if (src(k:k) == LF) then
            lx%line = lx%line + 1
        else if (src(k:k) == CR .and. peek(src, k + 1) /= LF) then
            lx%line = lx%line + 1
        end if
    end do
    call tokens%push(t)
end subroutine add

integer function string_end(src, i, flags) result(j)
    !! Last byte of a string literal whose opening quote is at `i`. Handles doubled quotes and
    !! continuation lines; `flags` reports STR_* conditions.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte of the opening quote
    integer, intent(out) :: flags         !! STR_* bit flags of the literal
    character :: q, c
    integer :: k

    flags = 0
    q = src(i:i)
    j = i + 1
    do
        if (j > len(src)) then
            flags = ior(flags, STR_UNTERMINATED)
            j = len(src)
            return
        end if
        c = src(j:j)
        if (c == q) then
            if (peek(src, j + 1) /= q) return
            j = j + 2
        else if (c == LF .or. c == CR) then
            flags = ior(flags, STR_UNTERMINATED)
            j = j - 1
            return
        else if (c == '&' .and. only_blanks_to_eol(src, j + 1)) then
            ! character context continued on the next line
            k = line_end(src, j) + 1          ! the line break
            if (k > len(src)) then
                j = len(src)
                flags = ior(flags, STR_UNTERMINATED)
                return
            end if
            if (src(k:k) == CR .and. peek(src, k + 1) == LF) k = k + 1
            k = k + 1                          ! start of the continuation line
            flags = ior(flags, STR_MULTILINE)
            j = k
            do while (is_blank(peek(src, j)))
                j = j + 1
            end do
            if (peek(src, j) == '&') then
                j = j + 1
            else
                flags = ior(flags, STR_RAW_CONT)
                j = k
            end if
        else
            j = j + 1
        end if
    end do
end function string_end

subroutine lex_number(src, i, j, kind)
    !! Integer or real literal starting at `i` (a digit, or `.` followed by a digit).
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first byte of the literal
    integer, intent(out) :: j             !! last byte of the literal, including any kind suffix
    integer, intent(out) :: kind          !! `TK_INT` or `TK_REAL`
    integer :: k
    character :: c

    kind = TK_INT
    j = digits_end(src, i)
    if (peek(src, j + 1) == '.' .and. .not. dotop_at(src, j + 1)) then
        kind = TK_REAL
        j = digits_end(src, j + 2)
    end if
    c = lower_char(peek(src, j + 1))
    if (c == 'e' .or. c == 'd' .or. c == 'q') then
        k = j + 2
        if (peek(src, k) == '+' .or. peek(src, k) == '-') k = k + 1
        if (is_digit(peek(src, k))) then
            kind = TK_REAL
            j = digits_end(src, k)
        end if
    end if
    if (peek(src, j + 1) == '_' .and. is_name_char(peek(src, j + 2))) j = name_end(src, j + 2)
end subroutine lex_number

integer function digits_end(src, i) result(j)
    !! Last digit of the run starting at `i`, or `i - 1` if there is none.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte where the run of digits starts
    j = i - 1
    do while (is_digit(peek(src, j + 1)))
        j = j + 1
    end do
end function digits_end

integer function name_end(src, i) result(j)
    !! Last byte of the name starting at `i`, stopping before inline fypp `${`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first byte of the name
    j = i
    do while (is_name_char(peek(src, j + 1)))
        if (peek(src, j + 1) == '$' .and. peek(src, j + 2) == '{') exit   ! inline fypp: name_${x}$
        j = j + 1
    end do
end function name_end

logical function dotop_at(src, i)
    !! True if a dot operator such as `.and.` or `.true.` starts at `i`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to look at
    integer :: j
    dotop_at = .false.
    if (peek(src, i) /= '.') return
    j = i + 1
    do while (is_letter(peek(src, j)))
        j = j + 1
    end do
    dotop_at = j > i + 1 .and. peek(src, j) == '.'
end function dotop_at

integer function dotop_end(src, i) result(j)
    !! Last byte of the dot operator at `i`, including a kind suffix on `.true._lk` / `.false._lk`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte of the opening `.`
    character(len=:), allocatable :: word
    j = i + 1
    do while (peek(src, j) /= '.')
        j = j + 1
    end do
    word = lowercase(src(i:j))
    if ((word == '.true.' .or. word == '.false.') .and. peek(src, j + 1) == '_' .and. &
        is_name_char(peek(src, j + 2))) j = name_end(src, j + 2)
end function dotop_end

integer function operator_end(src, i) result(j)
    !! Last byte of a two- or one-character operator at `i`, or 0 if there is none.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to look at
    character(len=*), parameter :: two(*) = ['**', '//', '==', '/=', '<=', '>=', '=>', '::']
    character(len=*), parameter :: one = '=+-*/<>()[],:;%?'
    character(len=2) :: pair
    integer :: k

    pair = src(i:min(i + 1, len(src)))
    do k = 1, size(two)
        if (pair == two(k)) then
            j = i + 1
            return
        end if
    end do
    j = 0
    if (index(one, src(i:i)) > 0) j = i
end function operator_end

integer function fypp_inline_end(src, i) result(j)
    !! Last byte of inline fypp (`${...}$`, `@{...}@`, `#{...}#`) starting at `i`, or 0.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to look at
    character :: c
    integer :: k

    j = 0
    c = src(i:i)
    if (index('$@#', c) == 0 .or. peek(src, i + 1) /= '{') return
    k = index(src(i + 2:), '}'//c)
    if (k > 0) j = i + 2 + k
end function fypp_inline_end

logical function is_preproc_start(src, i)
    !! True if a preprocessor line starts at `i` (the first non-blank of the line): cpp `#...`,
    !! fypp `#:`, `#!`, `$:` and `@:`. Inline fypp `#{` is not a directive line.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first non-blank byte of the line
    character :: c
    c = peek(src, i + 1)
    select case (src(i:i))
    case ('#')
        is_preproc_start = c /= '{' .and. c /= '}'
    case ('$', '@')
        is_preproc_start = c == ':'
    case default
        is_preproc_start = .false.
    end select
end function is_preproc_start

integer function preproc_end(src, i) result(j)
    !! Last byte of a preprocessor line at `i`, including continuation lines: a trailing `\` for
    !! cpp, a trailing `&` for fypp. The final line break is not included.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! first byte of the directive (`#`, `$` or `@`)
    logical :: fypp
    integer :: k

    fypp = src(i:i) /= '#' .or. peek(src, i + 1) == ':' .or. peek(src, i + 1) == '!'
    j = line_end(src, i)
    do
        k = last_nonblank(src, i, j)
        if (k == 0) exit
        if (fypp) then
            if (src(k:k) /= '&') exit
        else
            if (src(k:k) /= '\') exit
        end if
        k = j + 1                              ! the line break
        if (k > len(src)) exit
        if (src(k:k) == CR .and. peek(src, k + 1) == LF) k = k + 1
        if (k + 1 > len(src)) exit
        j = line_end(src, k + 1)
        if (j < k + 1) j = k                   ! an empty continuation line
    end do
end function preproc_end

integer function comment_kind(text) result(kind)
    !! Comment sub-kind (one of the CM_* constants) of a comment.
    character(len=*), intent(in) :: text   !! comment text, from `!` to the end of the line
    character(len=:), allocatable :: low
    character(len=*), parameter :: prefixes(*) = [character(len=6) :: &
                                                  '!dir$', '!dec$', '!gcc$', '!ibm*', '!cdir$', '!pgi$', '!ms$', '!nec$']
    integer :: k

    kind = CM_PLAIN
    low = lowercase(text)
    if (len(low) >= 2) then
        select case (low(2:2))
        case ('$')
            if (is_letter(peek(low, 3))) then
                kind = CM_DIRECTIVE
            else
                kind = CM_CONDITIONAL
            end if
            return
        case ('&')
            kind = CM_FORMATTER
            return
        end select
    end if
    do k = 1, size(prefixes)
        if (index(low, trim(prefixes(k))) == 1) then
            kind = CM_DIRECTIVE
            return
        end if
    end do
end function comment_kind

pure integer function line_end(src, i) result(j)
    !! Index of the last byte before the line break at or after `i`.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to start from
    j = i
    do while (j <= len(src))
        if (src(j:j) == LF .or. src(j:j) == CR) exit
        j = j + 1
    end do
    j = j - 1
end function line_end

pure integer function last_nonblank(src, i, j) result(k)
    !! Last non-blank byte in `src(i:j)`, or 0.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i, j           !! byte range to search
    do k = j, i, -1
        if (.not. is_blank(src(k:k))) return
    end do
    k = 0
end function last_nonblank

pure logical function only_blanks_to_eol(src, i)
    !! True if there are only blanks from `i` to the end of the line.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte to start from
    integer :: j
    j = line_end(src, i)
    only_blanks_to_eol = last_nonblank(src, i, j) == 0
end function only_blanks_to_eol

pure character function peek(src, i)
    !! Character at `i`, or NUL outside the string.
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: i              !! byte index, possibly outside `src`
    if (i < 1 .or. i > len(src)) then
        peek = achar(0)
    else
        peek = src(i:i)
    end if
end function peek

elemental logical function is_blank(c)
    !! True for a blank or tab.
    character, intent(in) :: c   !! the character
    is_blank = c == ' ' .or. c == TAB
end function is_blank

elemental logical function is_digit(c)
    !! True for `0` to `9`.
    character, intent(in) :: c   !! the character
    is_digit = c >= '0' .and. c <= '9'
end function is_digit

elemental logical function is_letter(c)
    !! True for an ASCII letter.
    character, intent(in) :: c   !! the character
    is_letter = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
end function is_letter

elemental logical function is_name_char(c)
    !! Characters allowed after the first letter of a name (`$` is a common extension).
    character, intent(in) :: c   !! the character
    is_name_char = is_letter(c) .or. is_digit(c) .or. c == '_' .or. c == '$'
end function is_name_char

elemental logical function is_quote(c)
    !! True for `'` or `"`.
    character, intent(in) :: c   !! the character
    is_quote = c == "'" .or. c == '"'
end function is_quote

elemental logical function is_code_char(c)
    !! A character that can be part of a code token (not a blank, line break or comment).
    character, intent(in) :: c   !! the character
    is_code_char = .not. (is_blank(c) .or. c == LF .or. c == CR .or. c == '!' .or. c == achar(0))
end function is_code_char

end module ffmt_lexer


!>>>>> ././src/ffmt_statements.f90
module ffmt_statements
!! Groups tokens into statements and standalone lines.
!!
!! A code statement runs from its first token to the line break that ends it, including any
!! continuation lines and the comment, blank and preprocessor lines between them. Statements
!! separated by `;` on one line are split after the `;`. Lines outside statements that hold only
!! blanks, a comment or a preprocessor directive become items of their own. Together the items
!! cover every token exactly once, in order.
use ffmt_tokens
use ffmt_strings, only: make_lowercase
implicit none
private

public :: split_statements, code_tokens, statement_view, set_view

! item kinds
integer, parameter, public :: SK_CODE = 1      !! a code statement
integer, parameter, public :: SK_BLANK = 2     !! a line with only blanks
integer, parameter, public :: SK_COMMENT = 3   !! a line with only a comment
integer, parameter, public :: SK_PREPROC = 4   !! a preprocessor directive line

type, public :: statement_t
    !! An item: a code statement or a standalone line, as a range of the token list.
    integer :: kind = SK_CODE   !! one of the SK_* constants
    integer :: first = 1    !! first token
    integer :: last = 0     !! last token
end type statement_t

type, public :: statement_list_t
    !! The items of a source text, in order.
    type(statement_t), allocatable :: s(:)   !! storage; only `s(1:n)` is in use
    integer :: n = 0                         !! number of items
contains
    procedure :: push                        !! append an item
end type statement_list_t

type, public :: statement_view_t
    !! The code tokens of one statement, with their lowercase text and bracket depth: what the rules
    !! that read a statement word by word need. The procedures take an index `j` into `code(:)`;
    !! outside `1:n` a word is `''` and its kind 0, so a rule can look before and after a token
    !! without checking the bounds.
    integer, allocatable :: code(:)         !! indices into the token list of the code tokens
                                            !! (see [[code_tokens]])
    integer :: n = 0                        !! number of code tokens
    integer :: first = 1                    !! first code token after a statement label (`10 continue`)
    integer, allocatable :: kinds(:)        !! kind of each code token (`TK_*`)
    integer, allocatable :: depth(:)        !! open `(` and `[` around each code token; a bracket is
                                            !! outside its own pair
    character(len=:), allocatable :: low    !! lowercase text of the code tokens, one after another
    integer, allocatable :: lo(:)           !! where the text of each code token starts in `low`
    integer, allocatable :: hi(:)           !! where it ends
contains
    procedure :: word => view_word          !! lowercase text of a code token
    procedure :: is => view_is              !! whether a code token's lowercase text is a given one
    procedure :: kind_of => view_kind_of    !! kind of a code token
    procedure :: closing => view_closing    !! the `)` that closes a `(`
end type statement_view_t

contains

subroutine split_statements(src, tokens, stmts)
    !! Group `tokens` into code statements and standalone blank, comment and preprocessor lines.
    character(len=*), intent(in) :: src              !! source text
    type(token_list_t), intent(in) :: tokens         !! tokens of `src`
    type(statement_list_t), intent(out) :: stmts     !! the items, covering every token once
    integer :: i, lend, start, semi, kind

    allocate (stmts%s(64))
    i = 1
    do while (i <= tokens%n)
        lend = line_end_token(tokens, i)
        kind = line_kind(tokens, i, lend)
        if (kind /= SK_CODE) then
            call stmts%push(statement_t(kind, i, lend))
            i = lend + 1
            cycle
        end if

        start = i
        do
            ! a `;` with more code after it ends the statement within the line
            semi = find_semicolon(src, tokens, i, lend)
            if (semi > 0) then
                call stmts%push(statement_t(SK_CODE, start, semi))
                i = semi + 1
                start = i
                cycle
            end if
            if (.not. ends_with_continuation(tokens, i, lend)) exit
            ! continued: take in the next code line and any comment, blank or preprocessor lines before it
            do
                i = lend + 1
                if (i > tokens%n) exit
                lend = line_end_token(tokens, i)
                if (line_kind(tokens, i, lend) == SK_CODE) exit
            end do
            if (i > tokens%n) then
                lend = tokens%n
                exit
            end if
        end do
        call stmts%push(statement_t(SK_CODE, start, lend))
        i = lend + 1
    end do
end subroutine split_statements

function code_tokens(src, tokens, stmt) result(idx)
    !! Indices of the code tokens of a statement: no blanks, line breaks, comments, continuation
    !! markers, preprocessor lines or `;` separators.
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    type(statement_t), intent(in) :: stmt        !! the statement
    integer, allocatable :: idx(:)
    integer :: k, n

    allocate (idx(stmt%last - stmt%first + 1))
    n = 0
    do k = stmt%first, stmt%last
        if (.not. is_code_kind(tokens%tok(k)%kind)) cycle
        if (tokens%tok(k)%kind == TK_OP) then
            if (src(tokens%tok(k)%first:tokens%tok(k)%last) == ';') cycle
        end if
        n = n + 1
        idx(n) = k
    end do
    idx = idx(1:n)
end function code_tokens

function statement_view(src, tokens, stmt) result(v)
    !! The view of code statement `stmt` (see [[set_view]], which fills one in place, without a copy).
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    type(statement_t), intent(in) :: stmt        !! the statement
    type(statement_view_t) :: v
    call set_view(src, tokens, stmt, v)
end function statement_view

subroutine set_view(src, tokens, stmt, v)
    !! Make `v` the view of code statement `stmt`.
    character(len=*), intent(in) :: src          !! source text
    type(token_list_t), intent(in) :: tokens     !! tokens of `src`
    type(statement_t), intent(in) :: stmt        !! the statement
    type(statement_view_t), intent(out) :: v     !! the view
    integer :: j, p, d, total

    ! the code tokens, as code_tokens finds them, counted first to allocate once
    v%n = 0
    do j = stmt%first, stmt%last
        if (is_statement_code(j)) v%n = v%n + 1
    end do
    allocate (v%code(v%n), v%kinds(v%n), v%depth(v%n), v%lo(v%n), v%hi(v%n))
    p = 0
    do j = stmt%first, stmt%last
        if (.not. is_statement_code(j)) cycle
        p = p + 1
        v%code(p) = j
    end do
    total = 0
    do j = 1, v%n
        total = total + tokens%tok(v%code(j))%last - tokens%tok(v%code(j))%first + 1
    end do
    allocate (character(len=total) :: v%low)
    p = 0
    d = 0
    do j = 1, v%n
        associate (t => tokens%tok(v%code(j)))
            v%kinds(j) = t%kind
            v%lo(j) = p + 1
            v%low(p + 1:p + t%last - t%first + 1) = src(t%first:t%last)
            call make_lowercase(v%low(p + 1:p + t%last - t%first + 1))
            p = p + t%last - t%first + 1
            v%hi(j) = p
        end associate
        v%depth(j) = d
        if (v%kinds(j) /= TK_OP) cycle
        select case (v%low(v%lo(j):v%hi(j)))
        case ('(', '[')
            d = d + 1
        case (')', ']')
            d = max(0, d - 1)
            v%depth(j) = d
        end select
    end do
    if (v%n > 1) then
        if (v%kinds(1) == TK_INT) v%first = 2
    end if

contains

    logical function is_statement_code(k)
        !! True if token `k` is one of the code tokens of the statement (not a `;` separator).
        integer, intent(in) :: k   !! index into the token list
        is_statement_code = is_code_kind(tokens%tok(k)%kind)
        if (is_statement_code .and. tokens%tok(k)%kind == TK_OP) &
            is_statement_code = src(tokens%tok(k)%first:tokens%tok(k)%last) /= ';'
    end function is_statement_code

end subroutine set_view

pure function view_word(v, j) result(w)
    !! Lowercase text of code token `j`, or `''` outside the statement.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    character(len=:), allocatable :: w
    if (j < 1 .or. j > v%n) then
        w = ''
    else
        w = v%low(v%lo(j):v%hi(j))
    end if
end function view_word

pure logical function view_is(v, j, text)
    !! True if the lowercase text of code token `j` is `text`, without making a copy of it. Outside
    !! the statement only `''` matches.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    character(len=*), intent(in) :: text       !! the text, in lowercase
    if (j < 1 .or. j > v%n) then
        view_is = len(text) == 0
    else
        view_is = v%hi(j) - v%lo(j) + 1 == len(text)
        if (view_is) view_is = v%low(v%lo(j):v%hi(j)) == text
    end if
end function view_is

pure integer function view_kind_of(v, j) result(kind)
    !! Kind (`TK_*`) of code token `j`, or 0 outside the statement.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    kind = 0
    if (j >= 1 .and. j <= v%n) kind = v%kinds(j)
end function view_kind_of

pure integer function view_closing(v, j) result(c)
    !! Index of the `)` that closes the `(` at code token `j` (counting only parentheses), or 0 if
    !! token `j` is not `(` or it is not closed.
    class(statement_view_t), intent(in) :: v   !! the statement
    integer, intent(in) :: j                   !! index into `v%code(:)`
    integer :: d
    if (.not. v%is(j, '(')) then
        c = 0
        return
    end if
    d = 0
    do c = j, v%n
        if (v%is(c, '(')) then
            d = d + 1
        else if (v%is(c, ')')) then
            d = d - 1
            if (d == 0) return
        end if
    end do
    c = 0
end function view_closing

pure integer function line_end_token(tokens, i) result(j)
    !! Index of the line break that ends the line containing token `i`, or the last token.
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: i                   !! a token at or after the start of the line
    do j = i, tokens%n
        if (tokens%tok(j)%kind == TK_NEWLINE) return
    end do
    j = tokens%n
end function line_end_token

pure integer function line_kind(tokens, i, lend) result(kind)
    !! Kind of the line `i:lend` from its first non-blank token.
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: i, lend             !! first and last token of the line
    integer :: k
    kind = SK_BLANK
    do k = i, lend
        select case (tokens%tok(k)%kind)
        case (TK_SPACE, TK_NEWLINE, TK_BOM)
            cycle
        case (TK_COMMENT)
            kind = SK_COMMENT
        case (TK_PREPROC)
            kind = SK_PREPROC
        case default
            kind = SK_CODE
        end select
        return
    end do
end function line_kind

pure integer function find_semicolon(src, tokens, i, lend) result(semi)
    !! A `;` in `i:lend` that is followed by more code on the line, or 0.
    character(len=*), intent(in) :: src        !! source text
    type(token_list_t), intent(in) :: tokens   !! tokens of `src`
    integer, intent(in) :: i, lend             !! tokens to search, up to the end of the line
    integer :: k, m
    do k = i, lend
        if (tokens%tok(k)%kind /= TK_OP) cycle
        if (src(tokens%tok(k)%first:tokens%tok(k)%last) /= ';') cycle
        do m = k + 1, lend
            select case (tokens%tok(m)%kind)
            case (TK_SPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                semi = k
                return
            end select
        end do
    end do
    semi = 0
end function find_semicolon

pure logical function ends_with_continuation(tokens, i, lend)
    !! True if the last token of the line before any comment is a continuation `&`.
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: i, lend             !! tokens to search, up to the end of the line
    integer :: k
    ends_with_continuation = .false.
    do k = lend, i, -1
        select case (tokens%tok(k)%kind)
        case (TK_SPACE, TK_NEWLINE, TK_COMMENT)
            cycle
        case (TK_CONT)
            ends_with_continuation = tokens%tok(k)%sub == CT_TRAILING
            return
        case default
            return
        end select
    end do
end function ends_with_continuation

subroutine push(self, s)
    !! Append item `s`, growing the storage as needed.
    class(statement_list_t), intent(inout) :: self   !! the item list
    type(statement_t), intent(in) :: s               !! item to append
    type(statement_t), allocatable :: tmp(:)
    if (.not. allocated(self%s)) allocate (self%s(64))
    if (self%n == size(self%s)) then
        allocate (tmp(2*self%n))
        tmp(1:self%n) = self%s(1:self%n)
        call move_alloc(tmp, self%s)
    end if
    self%n = self%n + 1
    self%s(self%n) = s
end subroutine push

end module ffmt_statements


!>>>>> build/dependencies/fregex/src/fregex_compiler.f90
module fregex_compiler
!! Compiles a parsed regular expression (AST) into a VM program.
use fregex_parser
use fregex_program
use fregex_charset, only: charset_t, full_set
use fregex_string, only: to_lower_code, is_alpha_code
implicit none
private

public :: compile_ast

integer, parameter :: MAX_PROGRAM_SIZE = 1000000
integer, parameter :: BIG_WIDTH = huge(1)/4   !! widths above this are treated as unbounded

contains

subroutine compile_ast(ast, prog, errmsg)
    type(ast_t), intent(in) :: ast
    type(program_t), intent(out) :: prog
    character(len=:), allocatable, intent(out) :: errmsg
    integer :: pc, k
    logical :: nullable
    type(charset_t) :: first

    allocate (prog%sets(-1:255, ast%nsets))
    do k = 1, ast%nsets
        prog%sets(:, k) = ast%sets(k)%table()
    end do
    prog%ngroups = ast%ngroups
    prog%names = ast%names

    pc = prog%emit(OP_SAVE, 0)
    call emit_node(ast, prog, ast%root, errmsg)
    if (allocated(errmsg)) return
    pc = prog%emit(OP_SAVE, 1)
    pc = prog%emit(OP_MATCH)

    prog%anchored = is_anchored(ast, ast%root, .false.)
    prog%line_start = .not. prog%anchored .and. is_anchored(ast, ast%root, .true.)
    call first_set(ast, ast%root, first, nullable)
    prog%first = first%table()
    prog%use_first = .not. nullable
    call literal_prefix(ast, ast%root, prog%prefix, prog%prefix_offset)
end subroutine compile_ast

recursive subroutine emit_node(ast, prog, id, errmsg)
    type(ast_t), intent(in) :: ast
    type(program_t), intent(inout) :: prog
    integer, intent(in) :: id
    character(len=:), allocatable, intent(inout) :: errmsg
    integer :: k, pc, wmin, wmax, reg
    integer, allocatable :: jumps(:)

    if (allocated(errmsg)) return
    if (prog%n > MAX_PROGRAM_SIZE) then
        errmsg = 'pattern too large after expanding repeat counts'
        return
    end if

    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_EMPTY)
            continue
        case (N_CHAR)
            if (node%b == 1) then
                pc = prog%emit(OP_CHARI, to_lower_code(node%a))
            else
                pc = prog%emit(OP_CHAR, node%a)
            end if
        case (N_ANY)
            pc = prog%emit(merge(OP_ANYNL, OP_ANY, node%a == 1))
        case (N_SET)
            pc = prog%emit(OP_SET, node%a)
        case (N_ASSERT)
            pc = prog%emit(assert_op(node%a))
        case (N_CONCAT)
            do k = 1, size(node%kids)
                call emit_node(ast, prog, node%kids(k), errmsg)
            end do
        case (N_ALT)
            ! SPLIT next, alt2; body1; JMP end; SPLIT next, alt3; body2; JMP end; ... bodyN
            allocate (jumps(size(node%kids) - 1))
            do k = 1, size(node%kids) - 1
                pc = prog%emit(OP_SPLIT, prog%n + 2)
                call emit_node(ast, prog, node%kids(k), errmsg)
                jumps(k) = prog%emit(OP_JMP)
                prog%code(2, pc) = prog%n + 1
            end do
            call emit_node(ast, prog, node%kids(size(node%kids)), errmsg)
            do k = 1, size(jumps)
                prog%code(1, jumps(k)) = prog%n + 1
            end do
        case (N_GROUP)
            pc = prog%emit(OP_SAVE, 2*node%a)
            call emit_node(ast, prog, node%kids(1), errmsg)
            pc = prog%emit(OP_SAVE, 2*node%a + 1)
        case (N_REPEAT)
            if (is_single_byte(ast, node%kids(1))) then
                call emit_single_repeat(ast, prog, node%kids(1), node%a, node%b, node%c)
            else if (node%c == R_POSSESSIVE) then
                pc = prog%emit(OP_ATOMIC)
                call emit_repeat(ast, prog, node%kids(1), node%a, node%b, .true., errmsg)
                k = prog%emit(OP_SUCCEED)
                prog%code(1, pc) = prog%n + 1
            else
                call emit_repeat(ast, prog, node%kids(1), node%a, node%b, node%c == R_GREEDY, errmsg)
            end if
        case (N_LOOK)
            if (node%a == 1) then
                pc = prog%emit(OP_LOOKAHEAD, 0, node%b)
                call emit_node(ast, prog, node%kids(1), errmsg)
            else
                call node_width(ast, node%kids(1), wmin, wmax)
                reg = prog%new_reg()
                pc = prog%emit(OP_LOOKBEHIND, 0, node%b, wmin, merge(UNBOUNDED, wmax, wmax == INF), reg)
                call emit_node(ast, prog, node%kids(1), errmsg)
                k = prog%emit(OP_ENDAT, reg)
            end if
            k = prog%emit(OP_SUCCEED)
            prog%code(1, pc) = prog%n + 1
        case (N_ATOMIC)
            pc = prog%emit(OP_ATOMIC)
            call emit_node(ast, prog, node%kids(1), errmsg)
            k = prog%emit(OP_SUCCEED)
            prog%code(1, pc) = prog%n + 1
        case (N_BACKREF)
            pc = prog%emit(OP_BACKREF, node%a, node%b)
        end select
    end associate
end subroutine emit_node

recursive subroutine emit_repeat(ast, prog, body, qmin, qmax, greedy, errmsg)
    !! Emit `body{qmin,qmax}`.
    !!
    !! The first `qmin` copies are emitted unconditionally. The rest are either a loop (unbounded) or
    !! `qmax-qmin` optional copies. If the body can match the empty string, an iteration that consumes
    !! nothing ends the repetition (as in Python's `re`), which also prevents infinite loops.
    type(ast_t), intent(in) :: ast
    type(program_t), intent(inout) :: prog
    integer, intent(in) :: body, qmin, qmax
    logical, intent(in) :: greedy
    character(len=:), allocatable, intent(inout) :: errmsg
    integer :: k, pc, loop, reg, exit_pc
    integer, allocatable :: splits(:), checks(:)
    logical :: nullable

    nullable = can_be_empty(ast, body)
    reg = 0
    if (nullable) reg = prog%new_reg()

    do k = 1, qmin
        call emit_node(ast, prog, body, errmsg)
        if (allocated(errmsg)) return
    end do

    if (qmax == INF) then
        allocate (splits(1), checks(0))
        loop = prog%emit(OP_SPLIT)
        splits(1) = loop
        call emit_iteration(ast, prog, body, reg, checks, errmsg)
        pc = prog%emit(OP_JMP, loop)
    else
        allocate (splits(qmax - qmin), checks(0))
        do k = 1, qmax - qmin
            splits(k) = prog%emit(OP_SPLIT)
            call emit_iteration(ast, prog, body, reg, checks, errmsg)
            if (allocated(errmsg)) return
        end do
    end if

    exit_pc = prog%n + 1
    do k = 1, size(splits)
        if (greedy) then
            prog%code(1:2, splits(k)) = [splits(k) + 1, exit_pc]
        else
            prog%code(1:2, splits(k)) = [exit_pc, splits(k) + 1]
        end if
    end do
    do k = 1, size(checks)
        prog%code(2, checks(k)) = exit_pc
    end do
end subroutine emit_repeat

recursive subroutine emit_iteration(ast, prog, body, reg, checks, errmsg)
    !! One optional iteration of a repeat. If `reg` is nonzero (the body can match the empty
    !! string), the iteration ends with an OP_CHECK_EXIT, whose pc is added to `checks` so that
    !! emit_repeat can point it at the end of the repeat.
    !!
    !!@note This is a module procedure rather than an internal procedure of the recursive [[emit_repeat]],
    !! because some compilers (LFortran 0.67) give an internal procedure the variables of the
    !! most recent call of its host instead of the call it was invoked from.
    type(ast_t), intent(in) :: ast
    type(program_t), intent(inout) :: prog
    integer, intent(in) :: body, reg
    integer, allocatable, intent(inout) :: checks(:)
    character(len=:), allocatable, intent(inout) :: errmsg
    integer :: pc

    if (reg /= 0) pc = prog%emit(OP_MARK, reg)
    call emit_node(ast, prog, body, errmsg)
    if (reg /= 0) then
        pc = prog%emit(OP_CHECK_EXIT, reg)
        if (allocated(checks)) then
            checks = [checks, pc]
        else
            checks = [pc]
        end if
    end if
end subroutine emit_iteration

subroutine emit_single_repeat(ast, prog, body, qmin, qmax, mode)
    !! Emit `body{qmin,qmax}` for a body that matches exactly one byte, as one OP_REPEAT. The VM
    !! then consumes the bytes in a loop and keeps one backtracking entry instead of one per byte.
    type(ast_t), intent(in) :: ast
    type(program_t), intent(inout) :: prog
    integer, intent(in) :: body, qmin, qmax, mode
    integer :: test, arg, pc

    associate (node => ast%nodes(body))
        select case (node%kind)
        case (N_CHAR)
            if (node%b == 1) then
                test = OP_CHARI
                arg = to_lower_code(node%a)
            else
                test = OP_CHAR
                arg = node%a
            end if
        case (N_ANY)
            test = merge(OP_ANYNL, OP_ANY, node%a == 1)
            arg = 0
        case default   ! N_SET
            test = OP_SET
            arg = node%a
        end select
    end associate
    pc = prog%emit(OP_REPEAT, test, arg, qmin, merge(UNBOUNDED, qmax, qmax == INF), rep_mode(mode))
end subroutine emit_single_repeat

pure integer function rep_mode(mode)
    integer, intent(in) :: mode
    select case (mode)
    case (R_LAZY)
        rep_mode = REP_LAZY
    case (R_POSSESSIVE)
        rep_mode = REP_POSSESSIVE
    case default
        rep_mode = REP_GREEDY
    end select
end function rep_mode

logical function is_single_byte(ast, id)
    !! True if node `id` always matches exactly one byte.
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    select case (ast%nodes(id)%kind)
    case (N_CHAR, N_ANY, N_SET)
        is_single_byte = .true.
    case default
        is_single_byte = .false.
    end select
end function is_single_byte

pure integer function assert_op(kind) result(op)
    integer, intent(in) :: kind
    select case (kind)
    case (A_BOL)
        op = OP_BOL
    case (A_BOL_M)
        op = OP_BOL_M
    case (A_EOL)
        op = OP_EOL
    case (A_EOL_M)
        op = OP_EOL_M
    case (A_BOS)
        op = OP_BOS
    case (A_EOS)
        op = OP_EOS
    case (A_WORDB)
        op = OP_WORDB
    case default
        op = OP_NWORDB
    end select
end function assert_op

logical function can_be_empty(ast, id)
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    integer :: wmin, wmax
    call node_width(ast, id, wmin, wmax)
    can_be_empty = wmin == 0
end function can_be_empty

recursive subroutine node_width(ast, id, wmin, wmax)
    !! Minimum and maximum number of bytes a node can match (`wmax` = INF if unbounded).
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    integer, intent(out) :: wmin, wmax
    integer :: k, kmin, kmax

    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_CHAR, N_ANY, N_SET)
            wmin = 1
            wmax = 1
        case (N_CONCAT)
            wmin = 0
            wmax = 0
            do k = 1, size(node%kids)
                call node_width(ast, node%kids(k), kmin, kmax)
                wmin = add_width(wmin, kmin)
                wmax = add_width(wmax, kmax)
            end do
        case (N_ALT)
            call node_width(ast, node%kids(1), wmin, wmax)
            do k = 2, size(node%kids)
                call node_width(ast, node%kids(k), kmin, kmax)
                wmin = min(wmin, kmin)
                if (wmax /= INF) wmax = merge(INF, max(wmax, kmax), kmax == INF)
            end do
        case (N_GROUP, N_ATOMIC)
            call node_width(ast, node%kids(1), wmin, wmax)
        case (N_REPEAT)
            call node_width(ast, node%kids(1), kmin, kmax)
            wmin = mul_width(kmin, node%a)
            if (kmax == 0 .or. node%b == 0) then
                wmax = 0
            else if (kmax == INF .or. node%b == INF) then
                wmax = INF
            else
                wmax = mul_width(kmax, node%b)
            end if
        case (N_BACKREF)
            wmin = 0
            wmax = INF
        case default   ! N_EMPTY, N_ASSERT, N_LOOK
            wmin = 0
            wmax = 0
        end select
    end associate
end subroutine node_width

pure integer function add_width(a, b) result(w)
    integer, intent(in) :: a, b
    if (a == INF .or. b == INF) then
        w = INF
    else
        w = min(a + b, BIG_WIDTH)
        if (w == BIG_WIDTH) w = INF
    end if
end function add_width

pure integer function mul_width(a, b) result(w)
    integer, intent(in) :: a, b
    if (a == 0 .or. b == 0) then
        w = 0
    else if (a > BIG_WIDTH/b) then
        w = INF
    else
        w = a*b
    end if
end function mul_width

subroutine literal_prefix(ast, id, prefix, offset)
    !! Case-sensitive literal text found at the start of every match, e.g. `ab` for `ab\d+`.
    !! Only a leading run of plain characters is recognized, which covers the common cases. A
    !! leading positive lookbehind whose body is plain characters counts too, as text just before
    !! the match: `(?<=call )\w+` gives `call ` with `offset` 5.
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    character(len=:), allocatable, intent(out) :: prefix
    integer, intent(out) :: offset
    integer :: k, kid
    integer, allocatable :: kids(:)

    prefix = ''
    offset = 0
    if (ast%nodes(id)%kind == N_CONCAT) then
        kids = ast%nodes(id)%kids
    else
        kids = [id]
    end if
    k = 1
    associate (look => ast%nodes(kids(1)))
        if (look%kind == N_LOOK .and. look%a == 0 .and. look%b == 0) then   ! (?<=...)
            if (plain_text(ast, look%kids(1), prefix)) then
                offset = len(prefix)
                k = 2
            end if
        end if
    end associate
    do k = k, size(kids)
        kid = kids(k)
        if (ast%nodes(kid)%kind /= N_CHAR .or. ast%nodes(kid)%b /= 0) exit
        prefix = prefix//achar(ast%nodes(kid)%a)
    end do
end subroutine literal_prefix

logical function plain_text(ast, id, text) result(ok)
    !! True if node `id` is one or more case-sensitive plain characters, returned in `text`.
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    character(len=:), allocatable, intent(inout) :: text
    integer :: k

    text = ''
    ok = .false.
    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_CHAR)
            if (node%b /= 0) return
            text = achar(node%a)
        case (N_CONCAT)
            do k = 1, size(node%kids)
                associate (kid => ast%nodes(node%kids(k)))
                    if (kid%kind /= N_CHAR .or. kid%b /= 0) then
                        text = ''
                        return
                    end if
                    text = text//achar(kid%a)
                end associate
            end do
        case default
            return
        end select
    end associate
    ok = .true.
end function plain_text

recursive logical function is_anchored(ast, id, lines) result(anchored)
    !! True if every match must start at the beginning of the string (`\A...` or `^...` without
    !! MULTILINE), or with `lines`, at the beginning of the string or of a line (also `^...` with
    !! MULTILINE).
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    logical, intent(in) :: lines
    integer :: k

    anchored = .false.
    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_ASSERT)
            anchored = node%a == A_BOS .or. node%a == A_BOL .or. (lines .and. node%a == A_BOL_M)
        case (N_CONCAT)
            anchored = is_anchored(ast, node%kids(1), lines)
        case (N_GROUP, N_ATOMIC)
            anchored = is_anchored(ast, node%kids(1), lines)
        case (N_REPEAT)
            anchored = node%a >= 1 .and. is_anchored(ast, node%kids(1), lines)
        case (N_ALT)
            anchored = .true.
            do k = 1, size(node%kids)
                if (.not. is_anchored(ast, node%kids(k), lines)) then
                    anchored = .false.
                    exit
                end if
            end do
        end select
    end associate
end function is_anchored

recursive subroutine first_set(ast, id, set, nullable)
    !! Add to `set` every byte a match of node `id` can start with. `nullable` is true if the
    !! node can match without consuming anything, in which case the following node's first
    !! bytes matter too. Zero-width assertions are treated as transparent, so the result is a
    !! superset, which is all the search loop needs.
    type(ast_t), intent(in) :: ast
    integer, intent(in) :: id
    type(charset_t), intent(inout) :: set
    logical, intent(out) :: nullable
    integer :: k, c
    logical :: kid_nullable
    type(charset_t) :: any_set

    associate (node => ast%nodes(id))
        select case (node%kind)
        case (N_CHAR)
            call set%add(node%a)
            if (node%b == 1) then
                c = to_lower_code(node%a)
                call set%add(c)
                call set%add(c - 32)
            end if
            nullable = .false.
        case (N_ANY)
            any_set = full_set()
            call set%add_set(any_set)
            nullable = .false.
        case (N_SET)
            call set%add_set(ast%sets(node%a))
            nullable = .false.
        case (N_CONCAT)
            nullable = .true.
            do k = 1, size(node%kids)
                call first_set(ast, node%kids(k), set, kid_nullable)
                if (.not. kid_nullable) then
                    nullable = .false.
                    exit
                end if
            end do
        case (N_ALT)
            nullable = .false.
            do k = 1, size(node%kids)
                call first_set(ast, node%kids(k), set, kid_nullable)
                nullable = nullable .or. kid_nullable
            end do
        case (N_GROUP, N_ATOMIC)
            call first_set(ast, node%kids(1), set, nullable)
        case (N_REPEAT)
            if (node%b == 0) then
                nullable = .true.
            else
                call first_set(ast, node%kids(1), set, kid_nullable)
                nullable = kid_nullable .or. node%a == 0
            end if
        case (N_BACKREF)
            any_set = full_set()
            call set%add_set(any_set)
            nullable = .true.
        case default   ! N_EMPTY, N_ASSERT, N_LOOK
            nullable = .true.
        end select
    end associate
end subroutine first_set

end module fregex_compiler


!>>>>> build/dependencies/fregex/src/fregex_vm.f90
module fregex_vm
!! Backtracking virtual machine that runs a compiled program against a string.
!!
!! Choice points and the old values of captures and registers are kept on one stack. On failure
!! the stack is popped: saved values are restored (undoing the failed path) until a choice point
!! is found to resume from. Lookarounds and atomic groups run their body in a nested call.
use iso_fortran_env, only: int64
use fregex_program
use fregex_string, only: to_lower_code, is_word_code
implicit none
private

public :: vm_exec

integer, parameter, public :: MODE_SEARCH = 0     !! find the leftmost match at or after `start`
integer, parameter, public :: MODE_MATCH = 1      !! match must start at `start`
integer, parameter, public :: MODE_FULLMATCH = 2  !! match must start at `start` and end at the end of the string

integer, parameter :: BT_BRANCH = 1   !! resume at pc x, position y
integer, parameter :: BT_CAP = 2      !! restore capture slot x to y
integer, parameter :: BT_REG = 3      !! restore register x to y
integer, parameter :: BT_GREEDY = 4   !! OP_REPEAT at pc x is at position y and may give back bytes down to z
integer, parameter :: BT_LAZY = 5     !! OP_REPEAT at pc x is at position y and may take more bytes up to z

type, public :: vm_t
    !! Work space for vm_exec. It can be reused for several searches with the same program, which
    !! saves allocating it for every match in find_all, split and sub.
    private
    integer, allocatable :: caps(:)        !! (0:2*ngroups+1), positions; 0 if unset
    integer, allocatable :: regs(:)
    integer, allocatable :: stack(:, :)    !! (4, capacity): kind, x, y, z
    integer :: top = 0
    integer(int64) :: steps = 0
    integer(int64) :: step_limit = 0
    logical :: overflow = .false.
    integer :: forbid_empty_at = 0         !! reject an empty match at this position
    logical :: require_end = .false.
end type vm_t

contains

subroutine vm_exec(prog, s, start, mode, forbid_empty_at, step_limit, vm, found, caps, overflow)
    !! Run `prog` on `s`. On success `caps` holds the capture positions: slot `2k` is the first
    !! index of group k, slot `2k+1` is one past its last index, and 0 means the group did not match.
    !! `caps` is only reallocated if its size is wrong, and is left unchanged if nothing is found.
    !!
    !! `vm` is work space, which may be reused from an earlier call with the same program.
    !! `overflow` is set if the step limit was exceeded before an answer was found.
    type(program_t), intent(in) :: prog
    character(len=*), intent(in) :: s
    integer, intent(in) :: start
    integer, intent(in) :: mode
    integer, intent(in) :: forbid_empty_at
    integer(int64), intent(in) :: step_limit
    type(vm_t), intent(inout) :: vm
    logical, intent(out) :: found
    integer, allocatable, intent(inout) :: caps(:)
    logical, intent(out) :: overflow

    integer :: st, last, sp_end, k

    found = .false.
    overflow = .false.
    ! Most programs use no registers, and `caps` may have been handed to the caller last time.
    if (.not. allocated(vm%caps)) allocate (vm%caps(0:2*prog%ngroups + 1))
    if (.not. allocated(vm%stack)) allocate (vm%stack(4, 64))
    if (prog%nregs > 0 .and. .not. allocated(vm%regs)) allocate (vm%regs(prog%nregs))
    vm%steps = 0
    vm%overflow = .false.
    vm%step_limit = step_limit
    vm%forbid_empty_at = forbid_empty_at
    vm%require_end = mode == MODE_FULLMATCH

    last = len(s) + 1
    if (mode /= MODE_SEARCH) last = start
    if (prog%anchored) last = min(last, 1)

    ! A failed attempt pops everything it pushed, which restores the captures and registers, so
    ! they only need clearing once.
    vm%caps = 0
    if (allocated(vm%regs)) vm%regs = 0
    vm%top = 0
    st = max(1, start)
    do while (st <= last)
        ! skip to the next position where a match can start
        if (prog%line_start .and. st > 1) then
            if (at(s, st - 1) /= 10) then
                k = find_prefix(s, achar(10), st, last - 1)
                if (k == 0) exit
                st = k + 1
            end if
        end if
        if (len(prog%prefix) > 0) then
            ! the first `prefix_offset` bytes of the prefix come before the match
            k = find_prefix(s, prog%prefix, max(1, st - prog%prefix_offset), last - prog%prefix_offset)
            if (k == 0) exit
            st = k + prog%prefix_offset
        else if (prog%use_first) then
            do while (st <= last)
                if (prog%first(at(s, st))) exit
                st = st + 1
            end do
            if (st > last) exit
        end if
        if (run(prog, s, vm, 1, st, sp_end)) then
            found = .true.
            ! Hand over the array if the caller has none yet, which saves an allocation and
            ! a copy for one-off searches. A reused `vm` then allocates it again next time.
            if (allocated(caps)) then
                caps = vm%caps
            else
                call move_alloc(vm%caps, caps)
            end if
            return
        end if
        if (vm%overflow) then
            overflow = .true.
            return
        end if
        st = st + 1
    end do
end subroutine vm_exec

recursive function run(prog, s, vm, pc0, sp0, sp_out) result(ok)
    !! Run from instruction `pc0` at position `sp0` until OP_MATCH or OP_SUCCEED (true, with the
    !! final position in `sp_out`) or until every alternative is exhausted (false). Only stack
    !! entries pushed by this call are popped, so nested calls leave the caller's state alone.
    type(program_t), intent(in) :: prog
    character(len=*), intent(in) :: s
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: pc0, sp0
    integer, intent(out) :: sp_out
    logical :: ok

    integer :: base, pc, sp, n, c, k, t0, st, lo, hi, sub_sp, gs, ge, glen, mode
    logical :: fail, sub_ok, negated

    n = len(s)
    base = vm%top
    pc = pc0
    sp = sp0
    sp_out = sp0
    ok = .false.

    do
        vm%steps = vm%steps + 1
        if (vm%steps > vm%step_limit) then
            vm%overflow = .true.
            return
        end if

        fail = .false.
        select case (prog%code(0, pc))
        case (OP_CHAR)
            if (at(s, sp) == prog%code(1, pc)) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_CHARI)
            if (to_lower_code(at(s, sp)) == prog%code(1, pc)) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_ANY)
            c = at(s, sp)
            if (c >= 0 .and. c /= 10) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_ANYNL)
            if (sp <= n) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_SET)
            if (prog%sets(at(s, sp), prog%code(1, pc))) then
                sp = sp + 1
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_REPEAT)
            ! lo and hi: the positions the repeat may end at
            lo = sp + prog%code(3, pc)
            if (prog%code(4, pc) == UNBOUNDED) then
                hi = n + 1
            else
                hi = min(n + 1, sp + prog%code(4, pc))
            end if
            mode = prog%code(5, pc)
            if (mode == REP_LAZY) then
                k = scan_repeat(prog, pc, s, sp, lo)
                if (k < lo) then
                    fail = .true.
                else
                    if (lo < hi) call push(vm, BT_LAZY, pc, lo, hi)
                    sp = lo
                    pc = pc + 1
                end if
            else
                k = scan_repeat(prog, pc, s, sp, hi)
                if (k < lo) then
                    fail = .true.
                else
                    if (mode == REP_GREEDY .and. k > lo) call push(vm, BT_GREEDY, pc, k, lo)
                    sp = k
                    pc = pc + 1
                end if
            end if
        case (OP_SPLIT)
            call push(vm, BT_BRANCH, prog%code(2, pc), sp, 0)
            pc = prog%code(1, pc)
        case (OP_JMP)
            pc = prog%code(1, pc)
        case (OP_SAVE)
            k = prog%code(1, pc)
            call push(vm, BT_CAP, k, vm%caps(k), 0)
            vm%caps(k) = sp
            pc = pc + 1
        case (OP_MARK)
            k = prog%code(1, pc)
            call push(vm, BT_REG, k, vm%regs(k), 0)
            vm%regs(k) = sp
            pc = pc + 1
        case (OP_CHECK_EXIT)
            if (sp == vm%regs(prog%code(1, pc))) then
                pc = prog%code(2, pc)
            else
                pc = pc + 1
            end if
        case (OP_BOL, OP_BOL_M, OP_EOL, OP_EOL_M, OP_BOS, OP_EOS, OP_WORDB, OP_NWORDB)
            if (check_assertion(prog%code(0, pc), s, sp)) then
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_BACKREF)
            k = prog%code(1, pc)
            gs = vm%caps(2*k)
            ge = vm%caps(2*k + 1)
            fail = .true.
            if (gs > 0 .and. ge > 0) then
                glen = ge - gs
                if (sp + glen - 1 <= n) then
                    if (prog%code(2, pc) == 1) then
                        fail = .false.
                        do c = 0, glen - 1
                            if (to_lower_code(at(s, sp + c)) /= to_lower_code(at(s, gs + c))) then
                                fail = .true.
                                exit
                            end if
                        end do
                    else
                        fail = s(sp:sp + glen - 1) /= s(gs:ge - 1)
                    end if
                end if
            end if
            if (.not. fail) then
                sp = sp + glen
                pc = pc + 1
            end if
        case (OP_LOOKAHEAD)
            t0 = vm%top
            negated = prog%code(2, pc) == 1
            sub_ok = run(prog, s, vm, pc + 1, sp, sub_sp)
            if (vm%overflow) return
            if (sub_ok .neqv. negated) then
                if (sub_ok) call compact(vm, t0)
                pc = prog%code(1, pc)
            else
                if (sub_ok) call unwind(vm, t0)
                fail = .true.
            end if
        case (OP_LOOKBEHIND)
            t0 = vm%top
            negated = prog%code(2, pc) == 1
            k = prog%code(5, pc)
            call push(vm, BT_REG, k, vm%regs(k), 0)
            vm%regs(k) = sp
            ! try every start position allowed by the body's width, nearest first
            hi = sp - prog%code(3, pc)
            if (prog%code(4, pc) == UNBOUNDED) then
                lo = 1
            else
                lo = max(1, sp - prog%code(4, pc))
            end if
            sub_ok = .false.
            do st = hi, lo, -1
                sub_ok = run(prog, s, vm, pc + 1, st, sub_sp)
                if (vm%overflow) return
                if (sub_ok) exit
            end do
            if (sub_ok .neqv. negated) then
                if (sub_ok) then
                    call compact(vm, t0)
                else
                    call unwind(vm, t0)
                end if
                pc = prog%code(1, pc)
            else
                call unwind(vm, t0)
                fail = .true.
            end if
        case (OP_ENDAT)
            if (sp == vm%regs(prog%code(1, pc))) then
                pc = pc + 1
            else
                fail = .true.
            end if
        case (OP_ATOMIC)
            t0 = vm%top
            sub_ok = run(prog, s, vm, pc + 1, sp, sub_sp)
            if (vm%overflow) return
            if (sub_ok) then
                call compact(vm, t0)
                sp = sub_sp
                pc = prog%code(1, pc)
            else
                fail = .true.
            end if
        case (OP_SUCCEED)
            ok = .true.
            sp_out = sp
            return
        case (OP_MATCH)
            if (vm%require_end .and. sp /= n + 1) then
                fail = .true.
            else if (sp == vm%forbid_empty_at .and. vm%caps(0) == sp) then
                fail = .true.
            else
                ok = .true.
                sp_out = sp
                return
            end if
        end select

        if (fail) then
            ! backtrack: undo saved values until a choice point is found
            do
                if (vm%top <= base) return
                k = vm%top
                vm%top = vm%top - 1
                select case (vm%stack(1, k))
                case (BT_BRANCH)
                    pc = vm%stack(2, k)
                    sp = vm%stack(3, k)
                    exit
                case (BT_GREEDY)   ! give back one byte, keeping the entry while more can be given back
                    pc = vm%stack(2, k) + 1
                    sp = vm%stack(3, k) - 1
                    if (prog%code(0, pc) == OP_CHAR) then
                        ! give back straight to where the literal byte that follows can match
                        c = prog%code(1, pc)
                        do while (sp >= vm%stack(4, k))
                            if (at(s, sp) == c) exit
                            sp = sp - 1
                        end do
                        if (sp < vm%stack(4, k)) cycle   ! nowhere: drop the entry
                    end if
                    if (sp > vm%stack(4, k)) then
                        vm%stack(3, k) = sp
                        vm%top = k
                    end if
                    exit
                case (BT_LAZY)   ! take one more byte if it matches, else keep backtracking
                    sp = vm%stack(3, k)
                    if (byte_matches(prog, vm%stack(2, k), at(s, sp))) then
                        pc = vm%stack(2, k) + 1
                        sp = sp + 1
                        if (sp < vm%stack(4, k)) then
                            vm%stack(3, k) = sp
                            vm%top = k
                        end if
                        exit
                    end if
                case (BT_CAP)
                    vm%caps(vm%stack(2, k)) = vm%stack(3, k)
                case (BT_REG)
                    vm%regs(vm%stack(2, k)) = vm%stack(3, k)
                end select
            end do
        end if
    end do
end function run

pure logical function check_assertion(op, s, sp) result(ok)
    integer, intent(in) :: op
    character(len=*), intent(in) :: s
    integer, intent(in) :: sp
    integer :: n

    n = len(s)
    select case (op)
    case (OP_BOL, OP_BOS)
        ok = sp == 1
    case (OP_BOL_M)
        ok = sp == 1 .or. at(s, sp - 1) == 10
    case (OP_EOL)
        ok = sp == n + 1 .or. (sp == n .and. at(s, sp) == 10)
    case (OP_EOL_M)
        ok = sp == n + 1 .or. at(s, sp) == 10
    case (OP_EOS)
        ok = sp == n + 1
    case (OP_WORDB)
        ok = is_word_code(at(s, sp - 1)) .neqv. is_word_code(at(s, sp))
    case default   ! OP_NWORDB; like Python, never matches in an empty string
        ok = n > 0 .and. (is_word_code(at(s, sp - 1)) .eqv. is_word_code(at(s, sp)))
    end select
end function check_assertion

pure integer function at(s, i) result(c)
    !! Byte value (0-255) of `s(i:i)`, or -1 if `i` is outside the string. The same as
    !! fregex_string's byte_at, but in this module so that the compiler can inline it.
    character(len=*), intent(in) :: s
    integer, intent(in) :: i
    if (i < 1 .or. i > len(s)) then
        c = -1
    else
        c = iand(ichar(s(i:i)), 255)
    end if
end function at

pure integer function find_prefix(s, prefix, from, last) result(pos)
    !! The first position in `from:last` where `prefix` occurs in `s`, or 0 if there is none.
    !! This looks for the first byte in a tight loop and only then compares the rest, which is
    !! several times faster than gfortran's `index`.
    character(len=*), intent(in) :: s, prefix
    integer, intent(in) :: from, last
    integer :: np
    np = len(prefix)
    do pos = from, min(last, len(s) - np + 1)
        if (s(pos:pos) == prefix(1:1)) then
            if (s(pos:pos + np - 1) == prefix) return
        end if
    end do
    pos = 0
end function find_prefix

pure logical function byte_matches(prog, pc, c) result(ok)
    !! True if byte `c` (-1 at the end of the string) passes the test of the OP_REPEAT at `pc`.
    type(program_t), intent(in) :: prog
    integer, intent(in) :: pc, c
    select case (prog%code(1, pc))
    case (OP_CHAR)
        ok = c == prog%code(2, pc)
    case (OP_CHARI)
        ok = to_lower_code(c) == prog%code(2, pc)
    case (OP_ANY)
        ok = c >= 0 .and. c /= 10
    case (OP_ANYNL)
        ok = c >= 0
    case default   ! OP_SET
        ok = prog%sets(c, prog%code(2, pc))
    end select
end function byte_matches

pure integer function scan_repeat(prog, pc, s, from, upto) result(p)
    !! The first position in `from:upto-1` whose byte fails the test of the OP_REPEAT at `pc`,
    !! or `upto` if they all pass.
    type(program_t), intent(in) :: prog
    integer, intent(in) :: pc, from, upto
    character(len=*), intent(in) :: s
    integer :: b
    p = from
    b = prog%code(2, pc)
    select case (prog%code(1, pc))
    case (OP_SET)
        do while (p < upto)
            if (.not. prog%sets(at(s, p), b)) exit
            p = p + 1
        end do
    case (OP_CHAR)
        do while (p < upto)
            if (at(s, p) /= b) exit
            p = p + 1
        end do
    case (OP_ANYNL)
        p = max(from, min(upto, len(s) + 1))
    case default
        do while (p < upto)
            if (.not. byte_matches(prog, pc, at(s, p))) exit
            p = p + 1
        end do
    end select
end function scan_repeat

pure subroutine push(vm, kind, x, y, z)
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: kind, x, y, z
    integer, allocatable :: tmp(:, :)

    if (vm%top == size(vm%stack, 2)) then
        allocate (tmp(4, 2*vm%top))
        tmp(:, 1:vm%top) = vm%stack(:, 1:vm%top)
        call move_alloc(tmp, vm%stack)
    end if
    vm%top = vm%top + 1
    vm%stack(1, vm%top) = kind
    vm%stack(2, vm%top) = x
    vm%stack(3, vm%top) = y
    vm%stack(4, vm%top) = z
end subroutine push

pure subroutine unwind(vm, t0)
    !! Pop everything above `t0`, restoring saved values.
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: t0
    integer :: k
    do k = vm%top, t0 + 1, -1
        select case (vm%stack(1, k))
        case (BT_CAP)
            vm%caps(vm%stack(2, k)) = vm%stack(3, k)
        case (BT_REG)
            vm%regs(vm%stack(2, k)) = vm%stack(3, k)
        end select
    end do
    vm%top = t0
end subroutine unwind

pure subroutine compact(vm, t0)
    !! Drop the choice points above `t0` but keep the saved values, so that a successful
    !! lookaround or atomic group cannot be re-entered, yet its effects can still be undone.
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: t0
    integer :: k, j
    j = t0
    do k = t0 + 1, vm%top
        if (vm%stack(1, k) == BT_CAP .or. vm%stack(1, k) == BT_REG) then
            j = j + 1
            vm%stack(:, j) = vm%stack(:, k)
        end if
    end do
    vm%top = j
end subroutine compact

end module fregex_vm


!>>>>> build/dependencies/toml-f/src/tomlf/utils.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module tomlf_utils
   use tomlf_constants
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time, to_string
   use tomlf_utils_io, only : read_whole_file, read_whole_line
   implicit none
   private

   public :: toml_escape_string
   public :: to_string
   public :: read_whole_file, read_whole_line


   interface to_string
      module procedure :: to_string_i1
      module procedure :: to_string_i2
      module procedure :: to_string_i4
      module procedure :: to_string_i8
      module procedure :: to_string_r8
   end interface to_string


contains


!> Escape all special characters in a TOML string
subroutine toml_escape_string(raw, escaped, multiline)

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw

   !> Escaped view of the TOML string
   character(kind=tfc, len=:), allocatable, intent(out) :: escaped

   !> Preserve newline characters
   logical, intent(in), optional :: multiline

   integer :: i
   logical :: preserve_newline

   preserve_newline = .false.
   if (present(multiline)) preserve_newline = multiline

   escaped = '"'
   do i = 1, len(raw)
      select case(raw(i:i))
      case default; escaped = escaped // raw(i:i)
      case('\'); escaped = escaped // '\\'
      case('"'); escaped = escaped // '\"'
      case(TOML_NEWLINE)
         if (preserve_newline) then
            escaped = escaped // raw(i:i)
         else
            escaped = escaped // '\n'
         end if
      case(TOML_FORMFEED); escaped = escaped // '\f'
      case(TOML_CARRIAGE_RETURN); escaped = escaped // '\r'
      case(TOML_TABULATOR); escaped = escaped // '\t'
      case(TOML_BACKSPACE); escaped = escaped // '\b'
      end select
   end do
   escaped = escaped // '"'

end subroutine toml_escape_string


!> Represent an integer as character sequence.
pure function to_string_i1(val) result(string)
   integer, parameter :: ik = tf_i1
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i1


!> Represent an integer as character sequence.
pure function to_string_i2(val) result(string)
   integer, parameter :: ik = tf_i2
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i2


!> Represent an integer as character sequence.
pure function to_string_i4(val) result(string)
   integer, parameter :: ik = tf_i4
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i4


!> Represent an integer as character sequence.
pure function to_string_i8(val) result(string)
   integer, parameter :: ik = tf_i8
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i8

!> Represent an real as character sequence.
pure function to_string_r8(val) result(string)
   integer, parameter :: rk = tfr
   !> Real value to create string from
   real(rk), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   character(128, tfc) :: buffer

   if (val > huge(val)) then
      string = "+inf"
   else if (val < -huge(val)) then
      string = "-inf"
   else if (val /= val) then
      string = "nan"
   else
      if (abs(val) >= 1.0e+100_rk) then
         write(buffer, '(es24.16e3)') val
      else if (abs(val) >= 1.0e+10_rk) then
         write(buffer, '(es24.16e2)') val
      else if (abs(val) >= 1.0e+3_rk) then
         write(buffer, '(es24.16e1)') val
      else
         write(buffer, '(f24.16)') val
      end if
      string = trim(adjustl(buffer))
   end if
end function to_string_r8

end module tomlf_utils


!>>>>> build/dependencies/toml-f/src/tomlf/de/abc.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Defines the abstract base class which is implemented by the TOML lexer.
module tomlf_de_abc
   use tomlf_constants, only : tfc, tfi, tfr
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_token, only : toml_token
   implicit none
   private

   public :: abstract_lexer


   !> Abstract base class for TOML lexers.
   type, abstract :: abstract_lexer
   contains
      !> Obtain the next token
      procedure(next), deferred :: next
      !> Extract a token
      generic :: extract => &
         & extract_string, extract_integer, extract_float, extract_bool, extract_datetime
      !> Extract a string from a token
      procedure(extract_string), deferred :: extract_string
      !> Extract an integer from a token
      procedure(extract_integer), deferred :: extract_integer
      !> Extract a float from a token
      procedure(extract_float), deferred :: extract_float
      !> Extract a boolean from a token
      procedure(extract_bool), deferred :: extract_bool
      !> Extract a timestamp from a token
      procedure(extract_datetime), deferred :: extract_datetime
      !> Get information about the source
      procedure(get_info), deferred :: get_info
   end type abstract_lexer


   abstract interface
      !> Advance the lexer to the next token.
      subroutine next(lexer, token)
         import :: abstract_lexer, toml_token
         !> Instance of the lexer
         class(abstract_lexer), intent(inout) :: lexer
         !> Current lexeme
         type(toml_token), intent(inout) :: token
      end subroutine next

      !> Extract string value of token, works for keypath, string, multiline string, literal,
      !> and mulitline literal tokens.
      subroutine extract_string(lexer, token, string)
         import :: abstract_lexer, toml_token, tfc
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract string value from
         type(toml_token), intent(in) :: token
         !> String value of token
         character(:, tfc), allocatable, intent(out) :: string
      end subroutine extract_string

      !> Extract integer value of token
      subroutine extract_integer(lexer, token, val)
         import :: abstract_lexer, toml_token, tfi
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract integer value from
         type(toml_token), intent(in) :: token
         !> Integer value of token
         integer(tfi), intent(out) :: val
      end subroutine extract_integer

      !> Extract floating point value of token
      subroutine extract_float(lexer, token, val)
         import :: abstract_lexer, toml_token, tfr
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract floating point value from
         type(toml_token), intent(in) :: token
         !> Floating point value of token
         real(tfr), intent(out) :: val
      end subroutine extract_float

      !> Extract boolean value of token
      subroutine extract_bool(lexer, token, val)
         import :: abstract_lexer, toml_token
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract boolean value from
         type(toml_token), intent(in) :: token
         !> Boolean value of token
         logical, intent(out) :: val
      end subroutine extract_bool

      !> Extract datetime value of token
      subroutine extract_datetime(lexer, token, val)
         import :: abstract_lexer, toml_token, toml_datetime
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract datetime value from
         type(toml_token), intent(in) :: token
         !> Datetime value of token
         type(toml_datetime), intent(out) :: val
      end subroutine extract_datetime

      !> Extract information about the source
      subroutine get_info(lexer, meta, output)
         import :: abstract_lexer, tfc
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Query about the source
         character(*, tfc), intent(in) :: meta
         !> Metadata about the source
         character(:, tfc), allocatable, intent(out) :: output
      end subroutine get_info
   end interface

end module tomlf_de_abc


!>>>>> ././src/ffmt_constructs.f90
module ffmt_constructs
!! Classifies statements by their role in the block structure: opening a construct or program
!! unit, continuing one (`else`, `case`, `contains`, ...), or closing one.
!!
!! Classification looks only at a statement's code tokens. Keywords are not reserved in Fortran,
!! so a keyword followed by `=`, `=>` or `%` is treated as a variable (`end = 1`).
use ffmt_tokens
use ffmt_statements, only: statement_view_t
use ffmt_strings, only: string_t
implicit none
private

public :: classify_statement, construct_label, closed_construct

! roles
integer, parameter, public :: ROLE_NONE = 0
integer, parameter, public :: ROLE_OPEN = 1
integer, parameter, public :: ROLE_MIDDLE = 2
integer, parameter, public :: ROLE_CLOSE = 3

! constructs
integer, parameter, public :: C_NONE = 0         !! no construct
integer, parameter, public :: C_ANY = 1          !! a bare `end`
integer, parameter, public :: C_PROGRAM = 2
integer, parameter, public :: C_MODULE = 3
integer, parameter, public :: C_SUBMODULE = 4
integer, parameter, public :: C_SUBROUTINE = 5
integer, parameter, public :: C_FUNCTION = 6
integer, parameter, public :: C_PROCEDURE = 7    !! separate module procedure (`module procedure name` in a submodule)
integer, parameter, public :: C_BLOCK_DATA = 8
integer, parameter, public :: C_INTERFACE = 9
integer, parameter, public :: C_TYPE = 10
integer, parameter, public :: C_ENUM = 11
integer, parameter, public :: C_IF = 12
integer, parameter, public :: C_DO = 13
integer, parameter, public :: C_SELECT = 14
integer, parameter, public :: C_ASSOCIATE = 15
integer, parameter, public :: C_BLOCK = 16
integer, parameter, public :: C_CRITICAL = 17
integer, parameter, public :: C_TEAM = 18
integer, parameter, public :: C_WHERE = 19
integer, parameter, public :: C_FORALL = 20
integer, parameter, public :: C_CONTAINS = 21    !! `contains` (a middle statement of program units and types)
integer, parameter, public :: C_ENUMERATION = 22 !! `enumeration type` (F2023)

integer, parameter :: NCONSTRUCTS = 22
character(len=*), parameter :: LABELS(0:NCONSTRUCTS) = [character(len=16) :: &
                                                        'none', 'end', 'program', 'module', 'submodule', 'subroutine', 'function', &
                                                        'procedure', &
                                                        'block data', 'interface', 'type', 'enum', 'if', 'do', 'select', &
                                                        'associate', 'block', &
                                                        'critical', 'change team', 'where', 'forall', 'contains', &
                                                        'enumeration type']

type, public :: stmt_info_t
    !! What a statement does to the block structure, as found by `classify_statement`.
    integer :: role = ROLE_NONE      !! `ROLE_OPEN`, `ROLE_MIDDLE`, `ROLE_CLOSE`, or `ROLE_NONE` for other statements
    integer :: construct = C_NONE    !! the construct opened, continued or closed: one of the `C_*` constants
    integer :: label = 0         !! statement label, or 0
    integer :: do_label = 0      !! for `do 10 ...`: the label of the statement that ends the loop
    character(len=:), allocatable :: name  !! construct name, as in `outer: do`
end type stmt_info_t

type :: words_t
    !! The words of a statement, with combined keywords such as `endif` split into `end`, `if`.
    type(string_t), allocatable :: w(:)  !! the words of the first token; the others are its code tokens
    integer, allocatable :: tok(:)     !! index into the code token list for each word
    integer :: n = 0                   !! number of words
end type words_t

contains

pure function construct_label(construct) result(label)
    !! Name of a construct for messages (`'if'`, `'select'`, `'end'` for a bare `end`), or `'?'`.
    integer, intent(in) :: construct   !! one of the `C_*` constants
    character(len=:), allocatable :: label
    if (construct < 0 .or. construct > NCONSTRUCTS) then
        label = '?'
    else
        label = trim(LABELS(construct))
    end if
end function construct_label

function classify_statement(src, tokens, v, in_interface) result(info)
    !! Classify a code statement. `in_interface` is true directly inside an interface block,
    !! where `module procedure` lists procedures instead of starting one.
    character(len=*), intent(in) :: src           !! source text
    type(token_list_t), intent(in) :: tokens      !! tokens of `src`
    type(statement_view_t), intent(in) :: v       !! the statement
    logical, intent(in) :: in_interface           !! the innermost open construct is an interface block
    type(stmt_info_t) :: info

    type(words_t) :: wd
    integer :: k, j, ios

    if (v%n == 0) return
    k = v%first
    if (k == 2) read (v%low(v%lo(1):v%hi(1)), *, iostat=ios) info%label
    ! construct name, possibly partly generated by fypp: `outer: do`, `get_${x}$: if (...) then`
    j = k
    do while (j < v%n)
        if (.not. is_name(j)) exit
        j = j + 1
    end do
    if (j > k .and. j < v%n .and. v%is(j, ':')) then
        info%name = src(tokens%tok(v%code(k))%first:tokens%tok(v%code(j - 1))%last)
        k = j + 1
    end if
    if (.not. is_name(k)) return
    ! an assignment to a variable named like a keyword, or to an element of one (`rank (1) = n`)
    j = k + 1
    if (v%is(j, '(')) then
        if (v%closing(j) > 0) j = v%closing(j) + 1
    end if
    if (v%is(j, '=') .or. v%is(j, '=>') .or. v%is(j, '%')) return
    call split_words()
    call classify_words()

contains

    logical function is_name(j)
        !! True if code token `j` is a name, or inline fypp that generates one (`${name}$`).
        integer, intent(in) :: j   !! index into `v%code(:)`
        is_name = v%kind_of(j) == TK_NAME .or. v%kind_of(j) == TK_FYPP
    end function is_name

    function word(j) result(t)
        !! Lowercase word `j`, or '' past the end.
        integer, intent(in) :: j   !! index into the words `wd`
        character(len=:), allocatable :: t
        if (j > wd%n) then
            t = ''
        else if (j <= size(wd%w)) then
            t = wd%w(j)%s
        else
            t = v%word(wd%tok(j))
        end if
    end function word

    integer function after_parens(j) result(next)
        !! Index of the code token after the parenthesis that opens at word `j`, or 0 if word `j`
        !! is not `(` or the parenthesis is not closed.
        integer, intent(in) :: j   !! index into the words `wd`
        next = 0
        if (j > wd%n) return
        next = v%closing(wd%tok(j))
        if (next > 0) next = next + 1
    end function after_parens

    logical function is_name_word(j)
        !! True if word `j` is a name, or inline fypp that generates one (`${name}$`).
        integer, intent(in) :: j   !! index into the words `wd`
        is_name_word = .false.
        if (j > wd%n) return
        is_name_word = is_name(wd%tok(j))
    end function is_name_word

    logical function has_attribute(attr)
        !! True if `attr` appears among the words before `::`.
        character(len=*), intent(in) :: attr   !! the attribute, in lowercase
        integer :: j
        has_attribute = .false.
        do j = 2, wd%n
            if (word(j) == '::') return
            if (word(j) == attr) then
                has_attribute = .true.
                return
            end if
        end do
    end function has_attribute

    integer function word_at(c) result(j)
        !! Index of the first word at or after code token `c`, or `wd%n + 1` if there is none.
        integer, intent(in) :: c   !! index into `v%code(:)`
        do j = 1, wd%n
            if (wd%tok(j) >= c) return
        end do
        j = wd%n + 1
    end function word_at

    subroutine split_words()
        !! Fill `wd` with the words of the statement from code token `k` on, splitting the first
        !! one with `expand_keyword`.
        type(string_t), allocatable :: parts(:)
        integer :: j, np

        ! only `end...`, `else...`, `select...`, `block...`, `change...` and `double...` can be
        ! combined keywords; other first words are read from the view, like the rest (see `word`)
        associate (first => v%low(v%lo(k):v%hi(k)))
            if (index(first, 'end') == 1 .or. index(first, 'else') == 1 .or. index(first, 'select') == 1 .or. &
                index(first, 'block') == 1 .or. index(first, 'change') == 1 .or. index(first, 'double') == 1) then
                parts = expand_keyword(first)
                if (size(parts) > 1) call move_alloc(parts, wd%w)
            end if
        end associate
        if (.not. allocated(wd%w)) allocate (wd%w(0))
        np = max(1, size(wd%w))
        allocate (wd%tok(np + v%n - k))
        wd%tok(1:np) = k
        wd%n = np
        do j = k + 1, v%n
            wd%n = wd%n + 1
            wd%tok(wd%n) = j
        end do
    end subroutine split_words

    subroutine classify_words()
        !! Set `info` from the words in `wd`, by the first word.
        integer :: c
        character(len=:), allocatable :: label_text

        select case (word(1))
        case ('end')
            c = closed_construct(word(2), word(3))
            if (c /= C_NONE) call set(info, ROLE_CLOSE, c)
        case ('else')
            select case (word(2))
            case ('', 'if')
                call set(info, ROLE_MIDDLE, C_IF)
            case ('where')
                call set(info, ROLE_MIDDLE, C_WHERE)
            case default
                if (wd%n == 2) call set(info, ROLE_MIDDLE, C_IF)   ! else construct-name
            end select
        case ('if')
            c = after_parens(2)
            if (c > 0 .and. c == v%n) then
                if (v%is(c, 'then')) call set(info, ROLE_OPEN, C_IF)
            end if
        case ('do')
            call set(info, ROLE_OPEN, C_DO)
            if (wd%n >= 2) then
                if (v%kind_of(wd%tok(2)) == TK_INT) then
                    label_text = word(2)
                    read (label_text, *, iostat=ios) info%do_label
                end if
            end if
        case ('select')
            select case (word(2))
            case ('case', 'type', 'rank')
                call set(info, ROLE_OPEN, C_SELECT)
            end select
        case ('case', 'rank')
            if (word(2) == '(' .or. word(2) == 'default') call set(info, ROLE_MIDDLE, C_SELECT)
        case ('type')
            if (word(2) == 'is' .and. word(3) == '(') then
                call set(info, ROLE_MIDDLE, C_SELECT)
            else if (word(2) == '(') then
                call check_procedure(1)
            else if (wd%n >= 2 .and. .not. has_attribute('deferred')) then
                ! `type, deferred :: T` declares a type parameter of a template (F202Y)
                call set(info, ROLE_OPEN, C_TYPE)
            end if
        case ('class')
            if ((word(2) == 'is' .and. word(3) == '(') .or. (word(2) == 'default' .and. wd%n == 2)) then
                call set(info, ROLE_MIDDLE, C_SELECT)
            else
                call check_procedure(1)
            end if
        case ('module')
            if (word(2) == 'procedure') then
                if (.not. in_interface) call set(info, ROLE_OPEN, C_PROCEDURE)
            else
                call check_procedure(1)
                if (info%role == ROLE_NONE .and. is_name_word(2)) call set(info, ROLE_OPEN, C_MODULE)
            end if
        case ('submodule')
            if (word(2) == '(') call set(info, ROLE_OPEN, C_SUBMODULE)
        case ('program')
            call set(info, ROLE_OPEN, C_PROGRAM)
        case ('block')
            if (wd%n == 1) then
                call set(info, ROLE_OPEN, C_BLOCK)
            else if (word(2) == 'data') then
                call set(info, ROLE_OPEN, C_BLOCK_DATA)
            end if
        case ('interface')
            call set(info, ROLE_OPEN, C_INTERFACE)
        case ('abstract')
            if (word(2) == 'interface') call set(info, ROLE_OPEN, C_INTERFACE)
        case ('enum')
            call set(info, ROLE_OPEN, C_ENUM)
        case ('enumeration')
            if (word(2) == 'type') call set(info, ROLE_OPEN, C_ENUMERATION)
        case ('associate')
            if (word(2) == '(') call set(info, ROLE_OPEN, C_ASSOCIATE)
        case ('critical')
            call set(info, ROLE_OPEN, C_CRITICAL)
        case ('change')
            if (word(2) == 'team') call set(info, ROLE_OPEN, C_TEAM)
        case ('where')
            if (after_parens(2) == v%n + 1) call set(info, ROLE_OPEN, C_WHERE)
        case ('forall')
            if (after_parens(2) == v%n + 1) call set(info, ROLE_OPEN, C_FORALL)
        case ('contains')
            if (wd%n == 1) call set(info, ROLE_MIDDLE, C_CONTAINS)
        case default
            call check_procedure(1)
        end select
    end subroutine classify_words

    subroutine check_procedure(j0)
        !! `[prefixes] [type-spec] function name` or `[prefixes] subroutine name`, starting at word `j0`:
        !! if the words match, set `info` to open a function or subroutine.
        integer, intent(in) :: j0   !! index of the first word to look at
        integer :: j, c

        j = j0
        do while (j <= wd%n)
            if (v%kind_of(wd%tok(j)) == TK_FYPP) then
                j = j + 1   ! a type or prefix generated by fypp, e.g. `${t}$ function f()`
                cycle
            end if
            select case (word(j))
            case ('pure', 'impure', 'elemental', 'recursive', 'non_recursive', 'module', 'simple')
                j = j + 1
            case ('integer', 'real', 'complex', 'logical', 'character', 'doubleprecision', 'doublecomplex')
                j = j + 1
                call skip_selector(j)
            case ('double')
                j = j + 1
                if (word(j) == 'precision' .or. word(j) == 'complex') j = j + 1
            case ('type', 'class')
                c = after_parens(j + 1)
                if (c == 0) return
                j = word_at(c)
            case ('function', 'subroutine')
                if (j + 1 > wd%n) return
                if (.not. is_name_word(j + 1)) return
                call set(info, ROLE_OPEN, merge(C_FUNCTION, C_SUBROUTINE, word(j) == 'function'))
                return
            case default
                ! a prefix made by a cpp macro: `PURE_IF_NO_DEBUG FUNCTION f(x)`
                if (.not. is_name_word(j)) return
                if (word(j + 1) /= 'function' .and. word(j + 1) /= 'subroutine') return
                j = j + 1
            end select
        end do
    end subroutine check_procedure

    subroutine skip_selector(j)
        !! Skip a kind or length selector after an intrinsic type: `(kind=8)`, `(len=*)`, `*8`, `*(*)`.
        integer, intent(inout) :: j   !! word index: the word after the type name, then the word after the selector
        integer :: c
        if (word(j) == '(') then
            c = after_parens(j)
            if (c > 0) j = word_at(c)
        else if (word(j) == '*') then
            j = j + 1
            if (word(j) == '(') then
                c = after_parens(j)
                if (c > 0) j = word_at(c)
            else
                j = j + 1
            end if
        end if
    end subroutine skip_selector

end function classify_statement

pure integer function closed_construct(second, third) result(construct)
    !! The construct that `end second third` closes (`C_ANY` for a bare `end`), or `C_NONE` if
    !! `second` names no construct, so that the `end` is not a closing statement.
    character(len=*), intent(in) :: second   !! the word after `end`, in lowercase, or `''`
    character(len=*), intent(in) :: third    !! the word after that, or `''` (`data` in `end block data`)
    select case (second)
    case ('')
        construct = C_ANY
    case ('if')
        construct = C_IF
    case ('do')
        construct = C_DO
    case ('select')
        construct = C_SELECT
    case ('associate')
        construct = C_ASSOCIATE
    case ('block')
        construct = merge(C_BLOCK_DATA, C_BLOCK, third == 'data')
    case ('blockdata')
        construct = C_BLOCK_DATA
    case ('critical')
        construct = C_CRITICAL
    case ('team')
        construct = C_TEAM
    case ('where')
        construct = C_WHERE
    case ('forall')
        construct = C_FORALL
    case ('enum')
        construct = C_ENUM
    case ('enumeration')
        construct = merge(C_ENUMERATION, C_NONE, third == 'type')
    case ('interface')
        construct = C_INTERFACE
    case ('type')
        construct = C_TYPE
    case ('function')
        construct = C_FUNCTION
    case ('subroutine')
        construct = C_SUBROUTINE
    case ('procedure')
        construct = C_PROCEDURE
    case ('module')
        construct = C_MODULE
    case ('submodule')
        construct = C_SUBMODULE
    case ('program')
        construct = C_PROGRAM
    case default
        construct = C_NONE
    end select
end function closed_construct

pure subroutine set(info, role, construct)
    !! Set the role and construct of `info`.
    type(stmt_info_t), intent(inout) :: info   !! the classification to update
    integer, intent(in) :: role                !! one of the `ROLE_*` constants
    integer, intent(in) :: construct           !! one of the `C_*` constants
    info%role = role
    info%construct = construct
end subroutine set

pure function expand_keyword(w) result(parts)
    !! Split a keyword that may be written as one word (`endif`, `elseif`, `selectcase`,
    !! `endblockdata`, ...) into its parts. Other words are returned unchanged.
    character(len=*), intent(in) :: w   !! the word, in lowercase
    type(string_t), allocatable :: parts(:)
    character(len=*), parameter :: ends(*) = [character(len=11) :: &
                                              'associate', 'blockdata', 'block', 'critical', 'do', 'enum', 'forall', 'function', &
                                              'if', &
                                              'interface', 'module', 'procedure', 'program', 'select', 'submodule', 'subroutine', &
                                              'team', &
                                              'type', 'where']
    integer :: k

    if (len(w) > 3 .and. w(1:min(3, len(w))) == 'end') then
        do k = 1, size(ends)
            if (w(4:) == trim(ends(k))) then
                if (w(4:) == 'blockdata') then
                    parts = [string_t('end'), string_t('block'), string_t('data')]
                else
                    parts = [string_t('end'), string_t(w(4:))]
                end if
                return
            end if
        end do
    end if
    select case (w)
    case ('elseif')
        parts = [string_t('else'), string_t('if')]
    case ('elsewhere')
        parts = [string_t('else'), string_t('where')]
    case ('selectcase')
        parts = [string_t('select'), string_t('case')]
    case ('selecttype')
        parts = [string_t('select'), string_t('type')]
    case ('selectrank')
        parts = [string_t('select'), string_t('rank')]
    case ('blockdata')
        parts = [string_t('block'), string_t('data')]
    case ('changeteam')
        parts = [string_t('change'), string_t('team')]
    case ('doubleprecision')
        parts = [string_t('double'), string_t('precision')]
    case default
        parts = [string_t(w)]
    end select
end function expand_keyword

end module ffmt_constructs


!>>>>> ././src/ffmt_directives.f90
module ffmt_directives
!! Formatter directives in comments, which protect code from formatting:
!!
!! * `!&` at the end of a statement keeps the statement exactly as it is.
!! * `!&<` on a line of its own starts a region that is kept exactly as it is, and `!&>` ends it.
use ffmt_tokens
use ffmt_statements
implicit none
private

public :: protected_items

integer, parameter :: DIR_NONE = 0, DIR_KEEP = 1, DIR_OFF = 2, DIR_ON = 3   !! none, `!&`, `!&<`, `!&>`

contains

function protected_items(src, tokens, stmts) result(keep)
    !! True for each item that must be written exactly as it is: statements with a `!&` comment, and
    !! everything from a `!&<` line to the next `!&>` line, both included.
    character(len=*), intent(in) :: src              !! source text
    type(token_list_t), intent(in) :: tokens         !! tokens of `src`
    type(statement_list_t), intent(in) :: stmts      !! items of `tokens`
    logical, allocatable :: keep(:)
    logical :: off
    integer :: k, j, d

    allocate (keep(stmts%n))
    off = .false.
    do k = 1, stmts%n
        keep(k) = off
        do j = stmts%s(k)%first, stmts%s(k)%last
            if (tokens%tok(j)%kind /= TK_COMMENT) cycle
            d = directive(src, tokens%tok(j))
            if (stmts%s(k)%kind == SK_COMMENT .and. d == DIR_OFF) then
                off = .true.
                keep(k) = .true.
            else if (stmts%s(k)%kind == SK_COMMENT .and. d == DIR_ON) then
                off = .false.
                keep(k) = .true.
            else if (stmts%s(k)%kind == SK_CODE .and. d == DIR_KEEP) then
                keep(k) = .true.
            end if
        end do
    end do
end function protected_items

integer function directive(src, t)
    !! Formatter directive (one of the DIR_* constants) of a comment token.
    character(len=*), intent(in) :: src   !! source text
    type(token_t), intent(in) :: t        !! a comment token of `src`
    character(len=:), allocatable :: text
    integer :: n
    directive = DIR_NONE
    if (t%sub /= CM_FORMATTER) return
    text = token_text(src, t)
    n = len(text)
    do while (n > 0)                        ! without trailing blanks and tabs
        if (text(n:n) /= ' ' .and. text(n:n) /= achar(9)) exit
        n = n - 1
    end do
    text = text(1:n)
    if (text == '!&<') then
        directive = DIR_OFF
    else if (text == '!&>') then
        directive = DIR_ON
    else if (len(text) == 2) then
        directive = DIR_KEEP
    else if (text(3:3) == ' ') then
        directive = DIR_KEEP
    end if
end function directive

end module ffmt_directives


!>>>>> build/dependencies/fregex/src/fregex.f90
module fregex
!! Backtracking regular expressions for modern Fortran.
!!
!! The pattern syntax follows Python's `re` module applied to byte strings: ASCII semantics,
!! `\d \w \s \b`, groups (named and non-capturing), alternation, greedy, lazy and possessive
!! quantifiers, lookahead, lookbehind, atomic groups, backreferences and inline flags.
!! Lookbehind may also have a variable width, which Python does not allow.
!!
!! Positions are 1-based and spans are inclusive, so `string(m%start():m%end())` is the match.
!!
!! Procedures that can fail take optional `stat` and `errmsg` arguments. If `stat` is absent
!! and an error occurs, the program stops with an error message.
use iso_fortran_env, only: int64
use fregex_string, only: regex_string_t, string_builder_t
use fregex_parser, only: ast_t, parse_pattern, F_IGNORECASE, F_MULTILINE, F_DOTALL
use fregex_program, only: program_t
use fregex_compiler, only: compile_ast
use fregex_vm, only: vm_t, vm_exec, MODE_SEARCH, MODE_MATCH, MODE_FULLMATCH
use fregex_match, only: match_t, new_match, init_match
use fregex_template, only: template_t, parse_template, expand_template
implicit none
private

public :: regex_t, match_t, regex_string_t
public :: regex
public :: re_search, re_match, re_fullmatch, re_find_all, re_split, re_sub, re_escape
public :: replacement_fn

! flags (combine with ior or +)
integer, parameter, public :: RE_IGNORECASE = F_IGNORECASE   !! case-insensitive matching (ASCII)
integer, parameter, public :: RE_MULTILINE = F_MULTILINE     !! `^` and `$` also match at line breaks
integer, parameter, public :: RE_DOTALL = F_DOTALL           !! `.` also matches a newline

! status codes
integer, parameter, public :: RE_OK = 0
integer, parameter, public :: RE_ERR_SYNTAX = 1        !! invalid pattern
integer, parameter, public :: RE_ERR_STEP_LIMIT = 2    !! matching took more steps than the limit
integer, parameter, public :: RE_ERR_NOT_COMPILED = 3  !! the regex was used before a successful compile
integer, parameter, public :: RE_ERR_TEMPLATE = 4      !! invalid replacement template

integer(int64), parameter, public :: RE_DEFAULT_STEP_LIMIT = 10000000_int64

type :: regex_t
    !! A compiled regular expression.
    private
    type(program_t) :: prog
    character(len=:), allocatable :: pat
    integer :: flags = 0
    integer(int64) :: step_limit = RE_DEFAULT_STEP_LIMIT
    logical :: compiled = .false.
contains
    procedure :: compile => regex_compile
    procedure :: is_compiled => regex_is_compiled
    procedure :: pattern => regex_pattern
    procedure :: group_count => regex_group_count
    procedure :: group_index => regex_group_index
    procedure :: set_step_limit => regex_set_step_limit
    procedure :: search => regex_search
    procedure :: match => regex_match
    procedure :: fullmatch => regex_fullmatch
    procedure :: find_all => regex_find_all
    procedure :: split => regex_split
    procedure, private :: sub_template, sub_callback
    generic :: sub => sub_template, sub_callback
    procedure, private :: exec, exec_one, substitute
end type regex_t

abstract interface
    function replacement_fn(m) result(text)
        !! Computes the replacement text for a match, for `regex_t%sub`.
        import :: match_t
        type(match_t), intent(in) :: m
        character(len=:), allocatable :: text
    end function replacement_fn
end interface

type, abstract :: replacer_t
    !! Produces the replacement text for each match in `substitute`.
contains
    procedure(replace_iface), deferred :: replace
end type replacer_t

abstract interface
    subroutine replace_iface(self, subject, caps, names, out)
        !! Append the replacement for the match in `caps` to `out`. Appending, rather than
        !! returning the text, saves allocating a string for every match.
        import :: replacer_t, regex_string_t, string_builder_t
        class(replacer_t), intent(in) :: self
        character(len=*), intent(in) :: subject
        integer, intent(in) :: caps(0:)
        type(regex_string_t), intent(in) :: names(:)
        type(string_builder_t), intent(inout) :: out
    end subroutine replace_iface
end interface

type, extends(replacer_t) :: template_replacer_t
    type(template_t) :: tmpl
contains
    procedure :: replace => replace_template
end type template_replacer_t

type, extends(replacer_t) :: callback_replacer_t
    procedure(replacement_fn), pointer, nopass :: fn => null()
contains
    procedure :: replace => replace_callback
end type callback_replacer_t

contains

function regex(pattern, flags, stat, errmsg) result(re)
    !! Compile `pattern` into a new regex.
    character(len=*), intent(in) :: pattern
    integer, intent(in), optional :: flags
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    type(regex_t) :: re
    call re%compile(pattern, flags, stat, errmsg)
end function regex

subroutine regex_compile(self, pattern, flags, stat, errmsg)
    class(regex_t), intent(inout) :: self
    character(len=*), intent(in) :: pattern
    integer, intent(in), optional :: flags
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg

    type(ast_t) :: ast
    character(len=:), allocatable :: msg

    self%compiled = .false.
    self%flags = 0
    if (present(flags)) self%flags = flags
    call parse_pattern(pattern, self%flags, ast, msg)
    if (.not. allocated(msg)) call compile_ast(ast, self%prog, msg)
    if (allocated(msg)) then
        call report(RE_ERR_SYNTAX, msg, stat, errmsg)
        return
    end if
    self%pat = pattern
    self%compiled = .true.
    if (present(stat)) stat = RE_OK
end subroutine regex_compile

pure logical function regex_is_compiled(self)
    class(regex_t), intent(in) :: self
    regex_is_compiled = self%compiled
end function regex_is_compiled

pure function regex_pattern(self) result(pattern)
    !! The source pattern.
    class(regex_t), intent(in) :: self
    character(len=:), allocatable :: pattern
    pattern = ''
    if (allocated(self%pat)) pattern = self%pat
end function regex_pattern

pure integer function regex_group_count(self)
    !! Number of capturing groups.
    class(regex_t), intent(in) :: self
    regex_group_count = self%prog%ngroups
end function regex_group_count

pure integer function regex_group_index(self, name) result(g)
    !! Number of the group called `name`, or 0 if there is none.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: name
    do g = 1, self%prog%ngroups
        if (len(self%prog%names(g)%s) == len(name)) then
            if (self%prog%names(g)%s == name) return
        end if
    end do
    g = 0
end function regex_group_index

pure subroutine regex_set_step_limit(self, limit)
    !! Limit the work done by one search (default RE_DEFAULT_STEP_LIMIT), which protects
    !! against patterns with exponential backtracking.
    class(regex_t), intent(inout) :: self
    integer(int64), intent(in) :: limit
    self%step_limit = limit
end subroutine regex_set_step_limit

logical function regex_search(self, string, m, pos, stat, errmsg) result(found)
    !! Find the leftmost match at or after position `pos` (default 1).
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: pos
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    found = self%exec_one(string, MODE_SEARCH, m, pos, stat, errmsg)
end function regex_search

logical function regex_match(self, string, m, pos, stat, errmsg) result(found)
    !! Match only at position `pos` (default 1).
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: pos
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    found = self%exec_one(string, MODE_MATCH, m, pos, stat, errmsg)
end function regex_match

logical function regex_fullmatch(self, string, m, pos, stat, errmsg) result(found)
    !! Match from position `pos` (default 1) to the end of the string.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: pos
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    found = self%exec_one(string, MODE_FULLMATCH, m, pos, stat, errmsg)
end function regex_fullmatch

function regex_find_all(self, string, stat, errmsg) result(matches)
    !! All non-overlapping matches, left to right (like Python's `finditer`).
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    type(match_t), allocatable :: matches(:)

    type(vm_t) :: vm
    integer, allocatable :: caps(:), all_caps(:, :), tmp(:, :)
    integer :: pos, forbid, nfound, ierr, k
    logical :: found
    character(len=:), allocatable :: msg

    nfound = 0
    pos = 1
    forbid = 0
    do
        call self%exec(string, pos, MODE_SEARCH, forbid, vm, found, caps, ierr, msg)
        if (ierr /= RE_OK .or. .not. found) exit
        if (.not. allocated(all_caps)) allocate (all_caps(0:size(caps) - 1, 16))
        if (nfound == size(all_caps, 2)) then
            allocate (tmp(0:size(caps) - 1, 2*nfound))
            tmp(:, 1:nfound) = all_caps(:, 1:nfound)
            call move_alloc(tmp, all_caps)
        end if
        nfound = nfound + 1
        all_caps(:, nfound) = caps
        call advance(caps, pos, forbid)
    end do
    ! The matches are built at the end, so that growing the list only copies integers.
    allocate (matches(nfound))
    do k = 1, nfound
        call init_match(matches(k), string, all_caps(:, k), self%prog%names)
    end do
    call finish(ierr, msg, stat, errmsg)
end function regex_find_all

function regex_split(self, string, maxsplit, stat, errmsg) result(parts)
    !! Split `string` at each match. Text of capture groups is included between the pieces,
    !! with an empty string for a group that did not match. At most `maxsplit` splits are
    !! made if it is present and positive.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    integer, intent(in), optional :: maxsplit
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    type(regex_string_t), allocatable :: parts(:)

    type(vm_t) :: vm
    integer, allocatable :: caps(:), spans(:, :)
    integer :: pos, forbid, last, nsplit, limit, g, ierr, nparts, k
    logical :: found
    character(len=:), allocatable :: msg

    limit = 0
    if (present(maxsplit)) limit = maxsplit
    allocate (spans(2, 16))
    nparts = 0
    last = 1
    nsplit = 0
    pos = 1
    forbid = 0
    ierr = RE_OK
    do
        if (limit > 0 .and. nsplit >= limit) exit
        call self%exec(string, pos, MODE_SEARCH, forbid, vm, found, caps, ierr, msg)
        if (ierr /= RE_OK .or. .not. found) exit
        call add(last, caps(0) - 1)
        do g = 1, self%prog%ngroups
            if (caps(2*g) > 0 .and. caps(2*g + 1) > 0) then
                call add(caps(2*g), caps(2*g + 1) - 1)
            else
                call add(1, 0)
            end if
        end do
        last = caps(1)
        nsplit = nsplit + 1
        call advance(caps, pos, forbid)
    end do
    call add(last, len(string))
    ! The pieces are built at the end, so that growing the list only copies integers.
    allocate (parts(nparts))
    do k = 1, nparts
        parts(k)%s = string(spans(1, k):spans(2, k))
    end do
    call finish(ierr, msg, stat, errmsg)

contains

    subroutine add(i1, i2)
        !! Record the piece `string(i1:i2)`, doubling the list when it is full.
        integer, intent(in) :: i1, i2
        integer, allocatable :: tmp(:, :)
        if (nparts == size(spans, 2)) then
            allocate (tmp(2, 2*nparts))
            tmp(:, 1:nparts) = spans(:, 1:nparts)
            call move_alloc(tmp, spans)
        end if
        nparts = nparts + 1
        spans(:, nparts) = [i1, i2]
    end subroutine add

end function regex_split

function sub_template(self, string, repl, count, nsubs, stat, errmsg) result(out)
    !! Replace matches with the template `repl` (`\1`, `\g<name>`, `\n`, ...). At most `count`
    !! replacements are made if it is present and positive; `nsubs` returns how many were made.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: repl
    integer, intent(in), optional :: count
    integer, intent(out), optional :: nsubs
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    character(len=:), allocatable :: out

    type(template_t) :: tmpl
    character(len=:), allocatable :: msg

    out = string
    if (present(nsubs)) nsubs = 0
    if (.not. self%compiled) then
        call report(RE_ERR_NOT_COMPILED, 'regex is not compiled', stat, errmsg)
        return
    end if
    call parse_template(repl, self%prog%names, tmpl, msg)
    if (allocated(msg)) then
        call report(RE_ERR_TEMPLATE, msg, stat, errmsg)
        return
    end if
    out = self%substitute(string, template_replacer_t(tmpl), count, nsubs, stat, errmsg)
end function sub_template

function sub_callback(self, string, fn, count, nsubs, stat, errmsg) result(out)
    !! Replace matches with the text returned by `fn` for each match.
    !!
    !! The string comes before `fn`: gfortran 15 miscompiles a dummy procedure with a
    !! deferred-length result that precedes a `character(*)` dummy argument.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    procedure(replacement_fn) :: fn
    integer, intent(in), optional :: count
    integer, intent(out), optional :: nsubs
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    character(len=:), allocatable :: out
    type(callback_replacer_t) :: replacer
    replacer%fn => fn
    out = self%substitute(string, replacer, count, nsubs, stat, errmsg)
end function sub_callback

function substitute(self, string, replacer, count, nsubs, stat, errmsg) result(out)
    !! Shared loop for both kinds of substitution.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    class(replacer_t), intent(in) :: replacer
    integer, intent(in), optional :: count
    integer, intent(out), optional :: nsubs
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    character(len=:), allocatable :: out

    type(string_builder_t) :: buf
    type(vm_t) :: vm
    integer, allocatable :: caps(:)
    integer :: pos, forbid, last, n, limit, ierr
    logical :: found
    character(len=:), allocatable :: msg

    limit = 0
    if (present(count)) limit = count
    last = 1
    n = 0
    pos = 1
    forbid = 0
    ierr = RE_OK
    do
        if (limit > 0 .and. n >= limit) exit
        call self%exec(string, pos, MODE_SEARCH, forbid, vm, found, caps, ierr, msg)
        if (ierr /= RE_OK .or. .not. found) exit
        call buf%append(string(last:caps(0) - 1))
        call replacer%replace(string, caps, self%prog%names, buf)
        last = caps(1)
        n = n + 1
        call advance(caps, pos, forbid)
    end do
    call buf%append(string(last:))
    out = buf%get()
    if (present(nsubs)) nsubs = n
    if (ierr /= RE_OK) out = string
    call finish(ierr, msg, stat, errmsg)
end function substitute

subroutine replace_template(self, subject, caps, names, out)
    class(template_replacer_t), intent(in) :: self
    character(len=*), intent(in) :: subject
    integer, intent(in) :: caps(0:)
    type(regex_string_t), intent(in) :: names(:)
    type(string_builder_t), intent(inout) :: out
    call expand_template(self%tmpl, subject, caps, out)
end subroutine replace_template

subroutine replace_callback(self, subject, caps, names, out)
    class(callback_replacer_t), intent(in) :: self
    character(len=*), intent(in) :: subject
    integer, intent(in) :: caps(0:)
    type(regex_string_t), intent(in) :: names(:)
    type(string_builder_t), intent(inout) :: out
    call out%append(self%fn(new_match(subject, caps, names)))
end subroutine replace_callback

logical function exec_one(self, string, mode, m, pos, stat, errmsg) result(found)
    !! One search, match or fullmatch, with error reporting.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    integer, intent(in) :: mode
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: pos
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg

    type(vm_t) :: vm
    integer, allocatable :: caps(:)
    integer :: start, ierr
    character(len=:), allocatable :: msg

    start = 1
    if (present(pos)) start = max(1, pos)
    call self%exec(string, start, mode, 0, vm, found, caps, ierr, msg)
    if (found .and. present(m)) call init_match(m, string, caps, self%prog%names)
    call finish(ierr, msg, stat, errmsg)
end function exec_one

subroutine exec(self, string, start, mode, forbid, vm, found, caps, ierr, msg)
    !! Run the VM. `ierr` is RE_OK or an error code, with `msg` describing it. `vm` is work
    !! space that callers making several searches reuse.
    class(regex_t), intent(in) :: self
    character(len=*), intent(in) :: string
    integer, intent(in) :: start, mode, forbid
    type(vm_t), intent(inout) :: vm
    logical, intent(out) :: found
    integer, allocatable, intent(inout) :: caps(:)
    integer, intent(out) :: ierr
    character(len=:), allocatable, intent(inout) :: msg
    logical :: overflow

    found = .false.
    ierr = RE_OK
    if (.not. self%compiled) then
        ierr = RE_ERR_NOT_COMPILED
        msg = 'regex is not compiled'
        return
    end if
    if (start > len(string) + 1) return
    call vm_exec(self%prog, string, start, mode, forbid, self%step_limit, vm, found, caps, overflow)
    if (overflow) then
        found = .false.
        ierr = RE_ERR_STEP_LIMIT
        msg = 'step limit exceeded while matching pattern: '//self%pat
    end if
end subroutine exec

pure subroutine advance(caps, pos, forbid)
    !! Where to search next after a match. After an empty match the next search starts at the
    !! same position but may not produce another empty match there (Python 3.7+ semantics).
    integer, intent(in) :: caps(0:)
    integer, intent(out) :: pos, forbid
    pos = caps(1)
    forbid = merge(caps(1), 0, caps(0) == caps(1))
end subroutine advance

subroutine finish(ierr, msg, stat, errmsg)
    integer, intent(in) :: ierr
    character(len=:), allocatable, intent(in) :: msg
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    if (ierr /= RE_OK) then
        call report(ierr, msg, stat, errmsg)
    else if (present(stat)) then
        stat = RE_OK
    end if
end subroutine finish

subroutine report(ierr, msg, stat, errmsg)
    integer, intent(in) :: ierr
    character(len=*), intent(in) :: msg
    integer, intent(out), optional :: stat
    character(len=:), allocatable, intent(out), optional :: errmsg
    if (.not. present(stat)) error stop 'fregex: '//msg
    stat = ierr
    if (present(errmsg)) errmsg = msg
end subroutine report

!--------------------------------------------------------------------------------------------
! Convenience procedures that compile the pattern on each call. Invalid patterns stop the
! program; compile a regex_t to handle errors or to reuse a pattern.
!--------------------------------------------------------------------------------------------

logical function re_search(pattern, string, m, flags) result(found)
    character(len=*), intent(in) :: pattern, string
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: flags
    type(regex_t) :: re
    call re%compile(pattern, flags)
    found = re%search(string, m)
end function re_search

logical function re_match(pattern, string, m, flags) result(found)
    character(len=*), intent(in) :: pattern, string
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: flags
    type(regex_t) :: re
    call re%compile(pattern, flags)
    found = re%match(string, m)
end function re_match

logical function re_fullmatch(pattern, string, m, flags) result(found)
    character(len=*), intent(in) :: pattern, string
    type(match_t), intent(out), optional :: m
    integer, intent(in), optional :: flags
    type(regex_t) :: re
    call re%compile(pattern, flags)
    found = re%fullmatch(string, m)
end function re_fullmatch

function re_find_all(pattern, string, flags) result(matches)
    character(len=*), intent(in) :: pattern, string
    integer, intent(in), optional :: flags
    type(match_t), allocatable :: matches(:)
    type(regex_t) :: re
    call re%compile(pattern, flags)
    matches = re%find_all(string)
end function re_find_all

function re_split(pattern, string, maxsplit, flags) result(parts)
    character(len=*), intent(in) :: pattern, string
    integer, intent(in), optional :: maxsplit, flags
    type(regex_string_t), allocatable :: parts(:)
    type(regex_t) :: re
    call re%compile(pattern, flags)
    parts = re%split(string, maxsplit)
end function re_split

function re_sub(pattern, string, repl, count, flags) result(out)
    character(len=*), intent(in) :: pattern, string, repl
    integer, intent(in), optional :: count, flags
    character(len=:), allocatable :: out
    type(regex_t) :: re
    call re%compile(pattern, flags)
    out = re%sub(string, repl, count)
end function re_sub

pure function re_escape(string) result(escaped)
    !! Escape the characters that have a special meaning in patterns.
    character(len=*), intent(in) :: string
    character(len=:), allocatable :: escaped
    character(len=*), parameter :: special = '()[]{}?*+-|^$\.&~# '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13)
    type(string_builder_t) :: buf
    integer :: i
    do i = 1, len(string)
        if (index(special, string(i:i)) > 0) call buf%append('\')
        call buf%append(string(i:i))
    end do
    escaped = buf%get()
end function re_escape

end module fregex


!>>>>> build/dependencies/toml-f/src/tomlf/terminal.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a terminal to provide ANSI escape sequences
!>
!> ANSI escape codes for producing terminal colors. The `ansi_code` derived
!> type is used to store ANSI escape codes and can be combined with other
!> codes or applied to strings by concatenation. The default or uninitialized
!> `ansi_code` is a stub and does not produce escape sequences when applied
!> to a string.
!>
!> Available colors are
!>
!> color          | foreground            | background
!> -------------- | --------------------- | ------------------------
!> black          | `black` (30)          | `bg_black` (40)
!> red            | `red` (31)            | `bg_red` (41)
!> green          | `green` (32)          | `bg_green` (42)
!> yellow         | `yellow` (33)         | `bg_yellow` (43)
!> blue           | `blue` (34)           | `bg_blue` (44)
!> magenta        | `magenta` (35)        | `bg_magenta` (45)
!> cyan           | `cyan` (36)           | `bg_cyan` (46)
!> white          | `white` (37)          | `bg_white` (47)
!> gray           | `gray` (90)           | `bg_gray` (100)
!> bright red     | `bright_red` (91)     | `bg_bright_red` (101)
!> bright green   | `bright_green` (92)   | `bg_bright_green` (102)
!> bright yellow  | `bright_yellow` (93)  | `bg_bright_yellow` (103)
!> bright blue    | `bright_blue` (94)    | `bg_bright_blue` (104)
!> bright magenta | `bright_magenta` (95) | `bg_bright_magenta` (105)
!> bright cyan    | `bright_cyan` (96)    | `bg_bright_cyan` (106)
!> bright white   | `bright_white` (97)   | `bg_bright_white` (107)
!>
!> Available styles are
!>
!> style       |
!> ------------| ---------------
!> reset       | `reset` (0)
!> bold        | `bold` (1)
!> dim         | `dim` (2)
!> italic      | `italic` (3)
!> underline   | `underline` (4)
!> blink       | `blink` (5)
!> blink rapid | `blink_rapid` (6)
!> reverse     | `reverse` (7)
!> hidden      | `hidden` (8)
!> crossed     | `crossed` (9)
module tomlf_terminal
   use tomlf_utils, only : to_string
   implicit none
   private

   public :: toml_terminal
   public :: ansi_code, escape, operator(+), operator(//)


   !> Char length for integers
   integer, parameter :: i1 = selected_int_kind(2)

   !> Container for terminal escape code
   type :: ansi_code
      private
      !> Style descriptor
      integer(i1) :: style = -1_i1
      !> Background color descriptor
      integer(i1) :: bg = -1_i1
      !> Foreground color descriptor
      integer(i1) :: fg = -1_i1
   end type

   interface operator(+)
      module procedure :: add
   end interface operator(+)

   interface operator(//)
      module procedure :: concat_left
      module procedure :: concat_right
   end interface operator(//)

   interface escape
      module procedure :: escape
   end interface escape

   type(ansi_code), public, parameter :: &
      reset = ansi_code(style=0_i1), &
      bold = ansi_code(style=1_i1), &
      dim = ansi_code(style=2_i1), &
      italic = ansi_code(style=3_i1), &
      underline = ansi_code(style=4_i1), &
      blink = ansi_code(style=5_i1), &
      blink_rapid = ansi_code(style=6_i1), &
      reverse = ansi_code(style=7_i1), &
      hidden = ansi_code(style=8_i1), &
      crossed = ansi_code(style=9_i1)

   type(ansi_code), public, parameter :: &
      black = ansi_code(fg=30_i1), &
      red = ansi_code(fg=31_i1), &
      green = ansi_code(fg=32_i1), &
      yellow = ansi_code(fg=33_i1), &
      blue = ansi_code(fg=34_i1), &
      magenta = ansi_code(fg=35_i1), &
      cyan = ansi_code(fg=36_i1), &
      white = ansi_code(fg=37_i1), &
      gray = ansi_code(fg=90_i1), &
      bright_red = ansi_code(fg=91_i1), &
      bright_green = ansi_code(fg=92_i1), &
      bright_yellow = ansi_code(fg=93_i1), &
      bright_blue = ansi_code(fg=94_i1), &
      bright_magenta = ansi_code(fg=95_i1), &
      bright_cyan = ansi_code(fg=96_i1), &
      bright_white = ansi_code(fg=97_i1)

   type(ansi_code), public, parameter :: &
      bg_black = ansi_code(bg=40_i1), &
      bg_red = ansi_code(bg=41_i1), &
      bg_green = ansi_code(bg=42_i1), &
      bg_yellow = ansi_code(bg=43_i1), &
      bg_blue = ansi_code(bg=44_i1), &
      bg_magenta = ansi_code(bg=45_i1), &
      bg_cyan = ansi_code(bg=46_i1), &
      bg_white = ansi_code(bg=47_i1), &
      bg_gray = ansi_code(bg=100_i1), &
      bg_bright_red = ansi_code(bg=101_i1), &
      bg_bright_green = ansi_code(bg=102_i1), &
      bg_bright_yellow = ansi_code(bg=103_i1), &
      bg_bright_blue = ansi_code(bg=104_i1), &
      bg_bright_magenta = ansi_code(bg=105_i1), &
      bg_bright_cyan = ansi_code(bg=106_i1), &
      bg_bright_white = ansi_code(bg=107_i1)


   !> Terminal wrapper to handle color escape sequences, must be initialized with
   !> color support to provide colorful output. Default and uninitialized instances
   !> will remain usable but provide only stubs and do not produce colorful output.
   !> This behavior is useful for creating applications which can toggle color support.
   type :: toml_terminal
      type(ansi_code) :: &
         reset = ansi_code(), &
         bold = ansi_code(), &
         dim = ansi_code(), &
         italic = ansi_code(), &
         underline = ansi_code(), &
         blink = ansi_code(), &
         blink_rapid = ansi_code(), &
         reverse = ansi_code(), &
         hidden = ansi_code(), &
         crossed = ansi_code()

      type(ansi_code) :: &
         black = ansi_code(), &
         red = ansi_code(), &
         green = ansi_code(), &
         yellow = ansi_code(), &
         blue = ansi_code(), &
         magenta = ansi_code(), &
         cyan = ansi_code(), &
         white = ansi_code(), &
         gray = ansi_code(), &
         bright_red = ansi_code(), &
         bright_green = ansi_code(), &
         bright_yellow = ansi_code(), &
         bright_blue = ansi_code(), &
         bright_magenta = ansi_code(), &
         bright_cyan = ansi_code(), &
         bright_white = ansi_code()

      type(ansi_code) :: &
         bg_black = ansi_code(), &
         bg_red = ansi_code(), &
         bg_green = ansi_code(), &
         bg_yellow = ansi_code(), &
         bg_blue = ansi_code(), &
         bg_magenta = ansi_code(), &
         bg_cyan = ansi_code(), &
         bg_white = ansi_code(), &
         bg_gray = ansi_code(), &
         bg_bright_red = ansi_code(), &
         bg_bright_green = ansi_code(), &
         bg_bright_yellow = ansi_code(), &
         bg_bright_blue = ansi_code(), &
         bg_bright_magenta = ansi_code(), &
         bg_bright_cyan = ansi_code(), &
         bg_bright_white = ansi_code()
   end type toml_terminal

   !> Constructor to create new terminal
   interface toml_terminal
      module procedure :: new_terminal
   end interface toml_terminal

contains

!> Create new terminal
pure function new_terminal(use_color) result(new)
   !> Enable color support in terminal
   logical, intent(in) :: use_color
   !> New terminal instance
   type(toml_terminal) :: new

   if (use_color) then
      new%reset = reset
      new%bold = bold
      new%dim = dim
      new%italic = italic
      new%underline = underline
      new%blink = blink
      new%blink_rapid = blink_rapid
      new%reverse = reverse
      new%hidden = hidden
      new%crossed = crossed

      new%black = black
      new%red = red
      new%green = green
      new%yellow = yellow
      new%blue = blue
      new%magenta = magenta
      new%cyan = cyan
      new%white = white
      new%gray  = gray
      new%bright_red  = bright_red
      new%bright_green  = bright_green
      new%bright_yellow  = bright_yellow
      new%bright_blue  = bright_blue
      new%bright_magenta  = bright_magenta
      new%bright_cyan  = bright_cyan
      new%bright_white  = bright_white

      new%bg_black = bg_black
      new%bg_red = bg_red
      new%bg_green = bg_green
      new%bg_yellow = bg_yellow
      new%bg_blue = bg_blue
      new%bg_magenta = bg_magenta
      new%bg_cyan = bg_cyan
      new%bg_white = bg_white
      new%bg_gray = bg_gray
      new%bg_bright_red = bg_bright_red
      new%bg_bright_green = bg_bright_green
      new%bg_bright_yellow = bg_bright_yellow
      new%bg_bright_blue = bg_bright_blue
      new%bg_bright_magenta = bg_bright_magenta
      new%bg_bright_cyan = bg_bright_cyan
      new%bg_bright_white = bg_bright_white
   end if
end function new_terminal

!> Add two escape sequences, attributes in the right value override the left value ones.
pure function add(lval, rval) result(code)
   !> First escape code
   type(ansi_code), intent(in) :: lval
   !> Second escape code
   type(ansi_code), intent(in) :: rval
   !> Combined escape code
   type(ansi_code) :: code

   code%style = merge(rval%style, lval%style, rval%style >= 0)
   code%fg = merge(rval%fg, lval%fg, rval%fg >= 0)
   code%bg = merge(rval%bg, lval%bg, rval%bg >= 0)
end function add


!> Concatenate an escape code with a string and turn it into an actual escape sequence
pure function concat_left(lval, code) result(str)
   !> String to add the escape code to
   character(len=*), intent(in) :: lval
   !> Escape sequence
   type(ansi_code), intent(in) :: code
   !> Concatenated string
   character(len=:), allocatable :: str

   str = lval // escape(code)
end function concat_left

!> Concatenate an escape code with a string and turn it into an actual escape sequence
pure function concat_right(code, rval) result(str)
   !> String to add the escape code to
   character(len=*), intent(in) :: rval
   !> Escape sequence
   type(ansi_code), intent(in) :: code
   !> Concatenated string
   character(len=:), allocatable :: str

   str = escape(code) // rval
end function concat_right


!> Transform a color code into an actual ANSI escape sequence
pure function escape(code) result(str)
   !> Color code to be used
   type(ansi_code), intent(in) :: code
   !> ANSI escape sequence representing the color code
   character(len=:), allocatable :: str

   if (anycolor(code)) then
      str = achar(27) // "[0"  ! Always reset the style
      if (code%style > 0) str = str // ";" // to_string(code%style)
      if (code%fg >= 0) str = str // ";" // to_string(code%fg)
      if (code%bg >= 0) str = str // ";" // to_string(code%bg)
      str = str // "m"
   else
      str = ""
   end if
end function escape

!> Check whether the code describes any color or is just a stub
pure function anycolor(code)
   !> Escape sequence
   type(ansi_code), intent(in) :: code
   !> Any color / style is active
   logical :: anycolor

   anycolor = code%fg >= 0 .or. code%bg >= 0 .or. code%style >= 0
end function anycolor

end module tomlf_terminal


!>>>>> build/dependencies/toml-f/src/tomlf/type/value.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Abstract base types for TOML values
!>
!> This module defines the abstract [[toml_value]] base type from which all
!> concrete TOML types ([[toml_table]], [[toml_array]], [[toml_keyval]])
!> inherit. It also provides the [[toml_visitor]] abstract type for
!> implementing the visitor pattern.
!>
!> Most users will not need to work with these types directly, but they
!> are useful for implementing custom algorithms that traverse TOML
!> data structures.
module tomlf_type_value
   use tomlf_constants, only : tfc, TOML_BAREKEY
   use tomlf_utils, only : toml_escape_string
   implicit none
   private

   public :: toml_value, toml_visitor, toml_key


   !> Abstract base value for TOML data types
   type, abstract :: toml_value

      !> Raw representation of the key to the TOML value
      character(kind=tfc, len=:), allocatable :: key

      !> Original source of the value
      integer :: origin = 0

   contains

      !> Accept a visitor to transverse the data structure
      procedure :: accept

      !> Get escaped key to TOML value
      procedure :: get_key

      !> Compare raw key of TOML value to input key
      procedure :: match_key

      !> Release allocation hold by TOML value
      procedure(destroy), deferred :: destroy

   end type toml_value


   !> Abstract visitor for TOML values
   type, abstract :: toml_visitor
   contains

      !> Visitor visiting a TOML value
      procedure(visit), deferred :: visit

   end type toml_visitor


   !> Thin wrapper around the deferred-size character intrinisc
   type :: toml_key

      !> Raw representation of the key to the TOML value
      character(kind=tfc, len=:), allocatable :: key

      !> Original source of the value
      integer :: origin = 0

   end type toml_key


   abstract interface
      !> Accept a visitor to transverse the data structure
      recursive subroutine visit(self, val)
         import toml_value, toml_visitor

         !> Instance of the visitor
         class(toml_visitor), intent(inout) :: self

         !> Value to visit
         class(toml_value), intent(inout) :: val
      end subroutine visit

      !> Deconstructor to cleanup allocations (optional)
      subroutine destroy(self)
         import toml_value

         !> Instance of the TOML value
         class(toml_value), intent(inout) :: self

      end subroutine destroy

   end interface


contains


!> Accept a visitor to transverse the data structure
recursive subroutine accept(self, visitor)

   !> Instance of the TOML value
   class(toml_value), intent(inout) :: self

   !> Visitor for this value
   class(toml_visitor), intent(inout) :: visitor

   call visitor%visit(self)

end subroutine accept


!> Get escaped key to TOML value
subroutine get_key(self, key)

   !> TOML value instance.
   class(toml_value), intent(in) :: self

   !> Contains valid TOML key on exit
   character(kind=tfc, len=:), allocatable :: key

   if (allocated(self%key)) then
      if (verify(self%key, TOML_BAREKEY) == 0 .and. len(self%key) > 0) then
         key = self%key
      else
         call toml_escape_string(self%key, key)
      end if
   end if

end subroutine get_key


!> Compare raw key of TOML value to input key
pure function match_key(self, key) result(match)

   !> TOML value instance.
   class(toml_value), intent(in) :: self

   !> TOML raw key to compare to
   character(kind=tfc, len=*), intent(in) :: key

   logical :: match

   if (allocated(self%key)) then
      match = key == self%key
   else
      match = .false.
   end if

end function match_key


end module tomlf_type_value


!>>>>> ././src/ffmt_source.f90
module ffmt_source
!! A source text read for formatting: its tokens, its statements and which of them formatter
!! directives protect. Every pass that works on the text (formatting, line splitting, relational
!! spelling, `use` sorting) starts from one of these.
use ffmt_tokens, only: token_list_t
use ffmt_lexer, only: tokenize
use ffmt_statements, only: statement_list_t, statement_view_t, set_view, split_statements, SK_CODE
use ffmt_directives, only: protected_items
implicit none
private

public :: parse_source, statement_views, set_views

type, public :: source_t
    !! A source text with its tokens and statements.
    character(len=:), allocatable :: text   !! the text
    type(token_list_t) :: tokens            !! its tokens
    type(statement_list_t) :: stmts         !! its statements and standalone lines (items)
    logical, allocatable :: keep(:)         !! for each item: protected by a formatter directive, so it
                                            !! is written as it is
end type source_t

contains

subroutine parse_source(text, s)
    !! Read `text` into `s`: tokenize it, group the tokens into items and find the protected ones.
    character(len=*), intent(in) :: text   !! the source text
    type(source_t), intent(out) :: s       !! the result
    s%text = text
    call tokenize(s%text, s%tokens)
    call split_statements(s%text, s%tokens, s%stmts)
    s%keep = protected_items(s%text, s%tokens, s%stmts)
end subroutine parse_source

function statement_views(s) result(views)
    !! The view of each item of `s` (see [[set_views]], which fills them in place, without a copy).
    type(source_t), intent(in) :: s     !! the source
    type(statement_view_t), allocatable :: views(:)
    call set_views(s, views)
end function statement_views

subroutine set_views(s, views)
    !! Make `views` the view of each item of `s` (see [[statement_view_t]]); empty for items that are
    !! not code.
    type(source_t), intent(in) :: s                                 !! the source
    type(statement_view_t), allocatable, intent(out) :: views(:)    !! the views
    integer :: k
    allocate (views(s%stmts%n))
    do k = 1, s%stmts%n
        if (s%stmts%s(k)%kind == SK_CODE) call set_view(s%text, s%tokens, s%stmts%s(k), views(k))
    end do
end subroutine set_views

end module ffmt_source


!>>>>> ././src/ffmt_walk.f90
module ffmt_walk
!! Finding the Fortran files under a directory.
!!
!! Files are selected by extension. Entries are excluded by glob patterns: `*` and `?` match within
!! a path component, `**` across components, and `[abc]` / `[!abc]` match one character. A pattern
!! without a `/` is matched against an entry's name, one with a `/` against its path relative to
!! the directory being searched. Links to directories are not followed.
use fregex, only: regex_t, regex, re_escape
use ffmt_fs
use ffmt_strings, only: string_t
implicit none
private

public :: collect_files, glob_to_regex, default_extensions, default_exclude

integer, parameter :: MAX_DEPTH = 100

contains

function default_extensions() result(list)
    !! Extensions of free-form Fortran files.
    type(string_t), allocatable :: list(:)
    list = [string_t('f90'), string_t('F90'), string_t('f95'), string_t('F95'), string_t('f03'), &
            string_t('F03'), string_t('f08'), string_t('F08'), string_t('f18'), string_t('F18')]
end function default_extensions

function default_exclude() result(list)
    !! Hidden files and directories, and fpm's build directory.
    type(string_t), allocatable :: list(:)
    list = [string_t('.*'), string_t('build')]
end function default_exclude

subroutine collect_files(root, extensions, exclude, files)
    !! The files under directory `root` with one of the `extensions` (without the dot) that are not
    !! excluded, sorted by path within each directory.
    character(len=*), intent(in) :: root   !! the directory to search
    type(string_t), intent(in) :: extensions(:)   !! extensions of the files to collect, without the dot
    type(string_t), intent(in) :: exclude(:)                !! glob patterns of the files and directories to skip
    type(string_t), allocatable, intent(out) :: files(:)    !! paths of the files, `root` joined with their
                                                            !! relative paths (just those if `root` is `.`)
    type(regex_t), allocatable :: patterns(:)
    logical, allocatable :: by_path(:)
    integer :: k

    allocate (files(0), patterns(size(exclude)), by_path(size(exclude)))
    do k = 1, size(exclude)
        patterns(k) = regex(glob_to_regex(exclude(k)%s))
        by_path(k) = index(exclude(k)%s, '/') > 0
    end do
    call walk(root, '', 0)

contains

    recursive subroutine walk(dir, rel, depth)
        !! Add the files under directory `dir` to `files`, descending at most `MAX_DEPTH` levels.
        character(len=*), intent(in) :: dir     !! the directory's path
        character(len=*), intent(in) :: rel     !! its path relative to `root` (`''` for `root`)
        integer, intent(in) :: depth            !! its depth below `root`
        type(dir_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: rel_path, path
        integer :: k, stat

        if (depth > MAX_DEPTH) return
        call list_directory(dir, entries, stat)
        if (stat /= 0) return
        call sort_entries(entries)
        do k = 1, size(entries)
            if (len(rel) == 0) then
                rel_path = entries(k)%name
            else
                rel_path = rel//'/'//entries(k)%name
            end if
            if (excluded(entries(k)%name, rel_path)) cycle
            path = child(dir, entries(k)%name)
            select case (entries(k)%kind)
            case (ENTRY_DIR)
                call walk(path, rel_path, depth + 1)
            case (ENTRY_FILE)
                if (has_extension(entries(k)%name)) files = [files, string_t(path)]
            end select
        end do
    end subroutine walk

    logical function excluded(name, rel_path)
        !! True if an exclude pattern matches the entry: its `name`, or its `rel_path` for a
        !! pattern with a `/`.
        character(len=*), intent(in) :: name        !! the entry's name
        character(len=*), intent(in) :: rel_path    !! its path relative to `root`
        integer :: k
        excluded = .false.
        do k = 1, size(patterns)
            if (by_path(k)) then
                excluded = patterns(k)%fullmatch(rel_path)
            else
                excluded = patterns(k)%fullmatch(name)
            end if
            if (excluded) return
        end do
    end function excluded

    logical function has_extension(name)
        !! True if file name `name` ends with `.` and one of the `extensions`, after at least one
        !! other character.
        character(len=*), intent(in) :: name   !! the file name
        integer :: k, n
        has_extension = .false.
        do k = 1, size(extensions)
            n = len(extensions(k)%s) + 1
            if (len(name) > n) then
                if (name(len(name) - n + 1:) == '.'//extensions(k)%s) then
                    has_extension = .true.
                    return
                end if
            end if
        end do
    end function has_extension

end subroutine collect_files

pure function child(dir, name) result(path)
    !! `dir/name`, or just `name` in the current directory.
    character(len=*), intent(in) :: dir, name   !! the directory, and the name of an entry in it
    character(len=:), allocatable :: path
    if (dir == '.') then
        path = name
    else
        path = join_path(dir, name)
    end if
end function child

subroutine sort_entries(entries)
    !! Sort by name (insertion sort: directories are small).
    type(dir_entry_t), intent(inout) :: entries(:)      !! a directory listing
    type(dir_entry_t) :: e
    integer :: i, j
    do i = 2, size(entries)
        e = entries(i)
        j = i - 1
        do while (j >= 1)
            if (.not. llt(e%name, entries(j)%name)) exit
            entries(j + 1) = entries(j)
            j = j - 1
        end do
        entries(j + 1) = e
    end do
end subroutine sort_entries

function glob_to_regex(glob) result(re)
    !! A regular expression (for a full match) equivalent to the glob pattern `glob`.
    character(len=*), intent(in) :: glob   !! the glob pattern
    character(len=:), allocatable :: re
    integer :: i, j
    re = ''
    i = 1
    do while (i <= len(glob))
        select case (glob(i:i))
        case ('*')
            if (i < len(glob)) then
                if (glob(i + 1:i + 1) == '*') then
                    if (i + 1 < len(glob)) then
                        if (glob(i + 2:i + 2) == '/') then
                            re = re//'(?:.*/)?'          ! `**/`: any number of directories
                            i = i + 3
                            cycle
                        end if
                    end if
                    re = re//'.*'
                    i = i + 2
                    cycle
                end if
            end if
            re = re//'[^/]*'
        case ('?')
            re = re//'[^/]'
        case ('[')
            j = index(glob(i + 1:), ']')
            if (j > 1) then
                if (glob(i + 1:i + 1) == '!') then
                    re = re//'[^'//glob(i + 2:i + j)
                else
                    re = re//glob(i:i + j)
                end if
                i = i + j + 1
                cycle
            end if
            re = re//'\['
        case default
            re = re//re_escape(glob(i:i))
        end select
        i = i + 1
    end do
end function glob_to_regex

end module ffmt_walk


!>>>>> build/dependencies/toml-f/src/tomlf/diagnostic.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Diagnostic message support for TOML Fortran
module tomlf_diagnostic
   use tomlf_terminal, only : toml_terminal, ansi_code, operator(//), operator(+)
   implicit none
   private

   public :: render
   public :: toml_diagnostic, toml_label


   interface render
      module procedure render_diagnostic
      module procedure render_text
      module procedure render_text_with_label
      module procedure render_text_with_labels
   end interface render


   !> Enumerator for diagnostic levels
   type :: level_enum
      integer :: error = 0
      integer :: warning = 1
      integer :: help = 2
      integer :: note = 3
      integer :: info = 4
   end type level_enum

   !> Actual enumerator values
   type(level_enum), parameter, public :: toml_level = level_enum()


   type toml_label
      !> Level of message
      integer :: level
      !> Primary message
      logical :: primary
      !> First and last character of message
      integer :: first, last
      !> Message text
      character(len=:), allocatable :: text
      !> Identifier of context
      character(len=:), allocatable :: source
   end type toml_label

   interface toml_label
      module procedure new_label
   end interface toml_label


   !> Definition of diagnostic message
   type :: toml_diagnostic
      !> Level of message
      integer :: level
      !> Primary message
      character(len=:), allocatable :: message
      !> Context of the diagnostic source
      character(len=:), allocatable :: source
      !> Messages associated with this diagnostic
      type(toml_label), allocatable :: label(:)
   end type toml_diagnostic

   interface toml_diagnostic
      module procedure new_diagnostic
   end interface toml_diagnostic


   type :: line_token
      integer :: first, last
   end type line_token

   character(len=*), parameter :: nl = new_line('a')


contains


pure function new_label(level, first, last, text, primary) result(new)
   integer, intent(in) :: level
   integer, intent(in) :: first, last
   character(len=*), intent(in), optional :: text
   logical, intent(in), optional :: primary
   type(toml_label) :: new

   if (present(text)) new%text = text
   new%level = level
   new%first = first
   new%last = last
   if (present(primary)) then
      new%primary = primary
   else
      new%primary = .false.
   end if
end function new_label


!> Create new diagnostic message
pure function new_diagnostic(level, message, source, label) result(new)
   !> Level of message
   integer, intent(in) :: level
   !> Primary message
   character(len=*), intent(in), optional :: message
   !> Context of the diagnostic source
   character(len=*), intent(in), optional :: source
   !> Messages associated with this diagnostic
   type(toml_label), intent(in), optional :: label(:)
   type(toml_diagnostic) :: new

   new%level = level
   if (present(message)) new%message = message
   if (present(source)) new%source = source
   if (present(label)) new%label = label
end function new_diagnostic


pure function line_tokens(input) result(token)
   character(len=*), intent(in) :: input
   type(line_token), allocatable :: token(:)

   integer :: first, last

   first = 1
   last = 1
   allocate(token(0))
   do while (first <= len(input))
      if (input(last:last) /= nl) then
         last = last + 1
         cycle
      end if

      token = [token, line_token(first, last-1)]
      first = last + 1
      last = first
   end do
end function line_tokens

recursive pure function render_diagnostic(diag, input, color) result(string)
   character(len=*), intent(in) :: input
   type(toml_diagnostic), intent(in) :: diag
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   string = &
      render_message(diag%level, diag%message, color)

   if (allocated(diag%label)) then
      string = string // nl // &
         render_text_with_labels(input, diag%label, color, source=diag%source)
   end if
end function render_diagnostic

pure function render_message(level, message, color) result(string)
   integer, intent(in) :: level
   character(len=*), intent(in), optional :: message
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   if (present(message)) then
      string = &
         level_name(level, color) // color%bold // ": " // message // color%reset
   else
      string = &
         level_name(level, color)
   end if
end function render_message

pure function level_name(level, color) result(string)
   integer, intent(in) :: level
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   select case(level)
   case(toml_level%error)
      string = color%bold + color%red // "error" // color%reset
   case(toml_level%warning)
      string = color%bold + color%yellow // "warning" // color%reset
   case(toml_level%help)
      string = color%bold + color%cyan // "help" // color%reset
   case(toml_level%note)
      string = color%bold + color%blue // "note" // color%reset
   case(toml_level%info)
      string = color%bold + color%magenta // "info" // color%reset
   case default
      string = color%bold + color%blue // "unknown" // color%reset
   end select
end function level_name

pure function render_source(source, offset, color) result(string)
   character(len=*), intent(in) :: source
   integer, intent(in) :: offset
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   string = &
      & repeat(" ", offset) // (color%bold + color%blue) // "-->" // color%reset // " " // source
end function render_source

function render_text(input, color, source) result(string)
   character(len=*), intent(in) :: input
   type(toml_terminal), intent(in) :: color
   character(len=*), intent(in), optional :: source
   character(len=:), allocatable :: string

   integer :: it, offset
   type(line_token), allocatable :: token(:)

   allocate(token(0))  ! avoid compiler warning
   token = line_tokens(input)
   offset = integer_width(size(token))

   if (present(source)) then
      string = render_source(source, offset, color) // nl // &
         & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset
   else
      string = &
         & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset
   end if

   do it = 1, size(token)
      string = string // nl //&
         & render_line(input(token(it)%first:token(it)%last), to_string(it, offset), color)
   end do
   string = string // nl // &
      repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

end function render_text

function render_text_with_label(input, label, color, source) result(string)
   character(len=*), intent(in) :: input
   type(toml_label), intent(in) :: label
   type(toml_terminal), intent(in) :: color
   character(len=*), intent(in), optional :: source
   character(len=:), allocatable :: string

   integer :: it, offset, first, last, line, shift
   type(line_token), allocatable :: token(:)

   allocate(token(0))  ! avoid compiler warning
   token = line_tokens(input)
   line = count(token%first < label%first)
   shift = token(line)%first - 1
   first = max(1, line - 1)
   last = min(size(token), line + 1)
   offset = integer_width(last)

   if (present(source)) then
      string = render_source(source, offset, color) // ":" // &
         & to_string(line) // ":" // &
         & to_string(label%first)
      if (label%first /= label%last) then
         string = string // "-" // to_string(label%last)
      end if
   end if
   string = string // nl // &
      & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

   do it = first, last
      string = string // nl //&
         & render_line(input(token(it)%first:token(it)%last), &
         &             to_string(it, offset), color)
      if (it == line) then
         string = string // nl //&
            & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset // &
            & render_label(label, shift, color)
      end if
   end do
   string = string // nl // &
      repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

end function render_text_with_label

pure function render_text_with_labels(input, label, color, source) result(string)
   character(len=*), intent(in) :: input
   type(toml_label), intent(in) :: label(:)
   type(toml_terminal), intent(in) :: color
   character(len=*), intent(in), optional :: source
   character(len=:), allocatable :: string

   integer :: it, il, offset, first, last, line(size(label)), shift(size(label))
   type(line_token), allocatable :: token(:)
   logical, allocatable :: display(:)

   allocate(token(0))  ! avoid compiler warning
   allocate(character(len=0) :: string) ! Allocate to avoid referencing an unallocated variable
   token = line_tokens(input)
   line(:) = [(count(token%first <= label(it)%first), it = 1, size(label))]
   shift(:) = token(line)%first - 1
   first = max(1, minval(line))
   last = min(size(token), maxval(line))
   offset = integer_width(last)

   it = 1  ! Without a primary we use the first label
   do il = 1, size(label)
      if (label(il)%primary) then
         it = il
         exit
      end if
   end do

   if (present(source)) then
      string = render_source(source, offset, color) // ":" // &
         & to_string(line(it)) // ":" // &
         & to_string(label(it)%first-shift(it))
      if (label(it)%first /= label(it)%last) then
         string = string // "-" // to_string(label(it)%last-shift(it))
      end if
   end if
   string = string // nl // &
      & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

   allocate(display(first:last), source=.false.)
   do il = 1, size(label)
      ! display(max(first, line(il) - 1):min(last, line(il) + 1)) = .true.
      display(line(il)) = .true.
   end do

   do it = first, last
      if (.not.display(it)) then
         if (display(it-1) .and. count(display(it:)) > 0) then
            string = string // nl //&
               & repeat(" ", offset + 1) // (color%bold + color%blue) // ":" // color%reset
         end if
         cycle
      end if

      string = string // nl //&
         & render_line(input(token(it)%first:token(it)%last), &
         &             to_string(it, offset), color)
      if (any(it == line)) then
         do il = 1, size(label)
            if (line(il) /= it) cycle
            string = string // nl //&
               & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset // &
               & render_label(label(il), shift(il), color)
         end do
      end if
   end do
   string = string // nl // &
      repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

end function render_text_with_labels

pure function render_label(label, shift, color) result(string)
   type(toml_label), intent(in) :: label
   integer, intent(in) :: shift
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   integer :: width
   character :: marker
   type(ansi_code) :: this_color

   marker = merge("^", "-", label%primary)
   width = label%last - label%first + 1
   this_color = level_color(label%level, color)

   string = &
      & repeat(" ", label%first - shift) // this_color // repeat(marker, width) // color%reset
   if (allocated(label%text)) then
      string = string // &
         & " " // this_color // label%text // color%reset
   end if

end function render_label

pure function level_color(level, color) result(this_color)
   integer, intent(in) :: level
   type(toml_terminal), intent(in) :: color
   type(ansi_code) :: this_color

   select case(level)
   case(toml_level%error)
      this_color = color%bold + color%red
   case(toml_level%warning)
      this_color = color%bold + color%yellow
   case(toml_level%help)
      this_color = color%bold + color%cyan
   case(toml_level%info)
      this_color = color%bold + color%magenta
   case default
      this_color = color%bold + color%blue
   end select
end function level_color

pure function render_line(input, line, color) result(string)
   character(len=*), intent(in) :: input
   character(len=*), intent(in) :: line
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   string = &
      & line // " " // (color%bold + color%blue) // "|" // color%reset // " " // input
end function render_line

pure function integer_width(input) result(width)
   integer, intent(in) :: input
   integer :: width

   integer :: val

   val = input
   width = 0
   do while (val /= 0)
      val = val / 10
      width = width + 1
   end do

end function integer_width

!> Represent an integer as character sequence.
pure function to_string(val, width) result(string)
   integer, intent(in) :: val
   integer, intent(in), optional :: width
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10))
      n = n/10
   end do
   if (val < 0) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   if (present(width)) then
      string = repeat(" ", max(width-(buffer_len+1-pos), 0)) // buffer(pos:)
   else
      string = buffer(pos:)
   end if
end function to_string


end module tomlf_diagnostic


!>>>>> build/dependencies/toml-f/src/tomlf/structure/list.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Abstract base class definitions for data structures to store TOML values
module tomlf_structure_list
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_list_structure


   !> Ordered data structure, allows iterations
   type, abstract :: toml_list_structure
   contains

      !> Get number of TOML values in the structure
      procedure(get_len), deferred :: get_len

      !> Push back a TOML value to the structure
      procedure(push_back), deferred :: push_back

      !> Remove the first element from the structure
      procedure(shift), deferred :: shift

      !> Remove the last element from the structure
      procedure(pop), deferred :: pop

      !> Get TOML value at a given index
      procedure(get), deferred :: get

      !> Destroy the data structure
      procedure(destroy), deferred :: destroy

   end type toml_list_structure


   abstract interface
      !> Get number of TOML values in the structure
      pure function get_len(self) result(length)
         import :: toml_list_structure

         !> Instance of the structure
         class(toml_list_structure), intent(in), target :: self

         !> Current length of the ordered structure
         integer :: length
      end function get_len


      !> Get TOML value at a given index
      subroutine get(self, idx, ptr)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> Position in the ordered structure
         integer, intent(in) :: idx

         !> Pointer to the stored value at given index
         class(toml_value), pointer, intent(out) :: ptr
      end subroutine get


      !> Push back a TOML value to the structure
      subroutine push_back(self, val)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> TOML value to be stored
         class(toml_value), allocatable, intent(inout) :: val

      end subroutine push_back


      !> Remove the first element from the data structure
      subroutine shift(self, val)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> TOML value to be retrieved
         class(toml_value), allocatable, intent(out) :: val

      end subroutine shift


      !> Remove the last element from the data structure
      subroutine pop(self, val)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> TOML value to be retrieved
         class(toml_value), allocatable, intent(out) :: val

      end subroutine pop


      !> Delete TOML value at a given key
      subroutine delete(self, key)
         import :: toml_list_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

      end subroutine delete


      !> Deconstructor for data structure
      subroutine destroy(self)
         import :: toml_list_structure

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

      end subroutine destroy

   end interface


end module tomlf_structure_list


!>>>>> build/dependencies/toml-f/src/tomlf/structure/map.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Abstract base class definitions for data structures to store TOML values
module tomlf_structure_map
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_map_structure


   !> Abstract data structure
   type, abstract :: toml_map_structure
   contains

      !> Get TOML value at a given key
      procedure(get), deferred :: get

      !> Push back a TOML value to the structure
      procedure(push_back), deferred :: push_back

      !> Get list of all keys in the structure
      procedure(get_keys), deferred :: get_keys

      !> Remove TOML value at a given key and return it
      procedure(pop), deferred :: pop

      !> Delete TOML value at a given key
      procedure(delete), deferred :: delete

      !> Destroy the data structure
      procedure(destroy), deferred :: destroy

   end type toml_map_structure


   abstract interface
      !> Get TOML value at a given key
      subroutine get(self, key, ptr)
         import :: toml_map_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

         !> Pointer to the stored value at given key
         class(toml_value), pointer, intent(out) :: ptr
      end subroutine get


      !> Push back a TOML value to the structure
      subroutine push_back(self, val)
         import :: toml_map_structure, toml_value

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> TOML value to be stored
         class(toml_value), allocatable, intent(inout) :: val

      end subroutine push_back


      !> Get list of all keys in the structure
      subroutine get_keys(self, list)
         import :: toml_map_structure, toml_key

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> List of all keys
         type(toml_key), allocatable, intent(out) :: list(:)

      end subroutine get_keys


      !> Remove TOML value at a given key and return it
      subroutine pop(self, key, val)
         import :: toml_map_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

         !> Removed TOML value
         class(toml_value), allocatable, intent(out) :: val

      end subroutine pop


      !> Delete TOML value at a given key
      subroutine delete(self, key)
         import :: toml_map_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

      end subroutine delete


      !> Deconstructor for data structure
      subroutine destroy(self)
         import :: toml_map_structure

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

      end subroutine destroy

   end interface


end module tomlf_structure_map


!>>>>> build/dependencies/toml-f/src/tomlf/structure/node.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a basic storage structure as pointer list of pointers.
!>
!> This implementation does purposely not use pointer attributes in the
!> datastructure to make it safer to work with.
module tomlf_structure_node
   use tomlf_type_value, only : toml_value
   implicit none
   private

   public :: toml_node, resize


   !> Wrapped TOML value to generate pointer list
   type :: toml_node

      !> TOML value payload
      class(toml_value), allocatable :: val

   end type toml_node


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Change size of the TOML value list
subroutine resize(list, n)

   !> Array of TOML values to be resized
   type(toml_node), allocatable, intent(inout), target :: list(:)

   !> New size of the list
   integer, intent(in) :: n

   type(toml_node), allocatable, target :: tmp(:)
   integer :: i


   if (allocated(list)) then
      call move_alloc(list, tmp)
      allocate(list(n))

      do i = 1, min(size(tmp), n)
         if (allocated(tmp(i)%val)) then
            call move_alloc(tmp(i)%val, list(i)%val)
         end if
      end do

      do i = n+1, size(tmp)
         if (allocated(tmp(i)%val)) then
            call tmp(i)%val%destroy
            deallocate(tmp(i)%val)
         end if
      end do

      deallocate(tmp)
   else
      allocate(list(n))
   end if

end subroutine resize

end module tomlf_structure_node


!>>>>> build/dependencies/toml-f/src/tomlf/type/keyval.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML key-value pair data type
!>
!> A [[toml_keyval]] represents a single key-value pair in a TOML document.
!> It can hold any of the TOML value types: strings, integers, floats,
!> booleans, and datetimes.
!>
!> Key-value pairs are typically accessed through their parent table using
!> [[get_value]] rather than directly manipulating this type.
module tomlf_type_keyval
   use tomlf_constants, only : tfc, tfr, tfi, toml_type
   use tomlf_datetime, only : toml_datetime
   use tomlf_type_value, only : toml_value, toml_visitor
   implicit none
   private

   public :: toml_keyval, new_keyval, new


   !> Generic TOML value
   type, abstract :: generic_value
   end type generic_value

   !> TOML real value
   type, extends(generic_value) :: float_value
      real(tfr) :: raw
   end type float_value

   !> TOML integer value
   type, extends(generic_value) :: integer_value
      integer(tfi) :: raw
   end type integer_value

   !> TOML boolean value
   type, extends(generic_value) :: boolean_value
      logical :: raw
   end type boolean_value

   !> TOML datetime value
   type, extends(generic_value) :: datetime_value
      type(toml_datetime) :: raw
   end type datetime_value

   !> TOML string value
   type, extends(generic_value) :: string_value
      character(:, tfc), allocatable :: raw
   end type string_value



   !> TOML key-value pair
   type, extends(toml_value) :: toml_keyval

      !> Actual TOML value
      class(generic_value), allocatable :: val

      !> Origin of value
      integer :: origin_value = 0

   contains

      !> Get the value stored in the key-value pair
      generic :: get => get_float, get_integer, get_boolean, get_datetime, get_string
      procedure :: get_float
      procedure :: get_integer
      procedure :: get_boolean
      procedure :: get_datetime
      procedure :: get_string

      !> Set the value for the key-value pair
      generic :: set => set_float, set_integer, set_boolean, set_datetime, set_string
      procedure :: set_float
      procedure :: set_integer
      procedure :: set_boolean
      procedure :: set_datetime
      procedure :: set_string

      !> Get the type of the value stored in the key-value pair
      procedure :: get_type

      !> Release allocation hold by TOML key-value pair
      procedure :: destroy

   end type toml_keyval


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_keyval
   end interface


contains


!> Constructor to create a new TOML key-value pair
subroutine new_keyval(self)

   !> Instance of the TOML key-value pair
   type(toml_keyval), intent(out) :: self

   associate(self => self); end associate

end subroutine new_keyval


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%val)) then
      deallocate(self%val)
   end if

end subroutine destroy


!> Obtain real value from TOML key-value pair
subroutine get_float(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   real(tfr), pointer, intent(out) :: val

   val => cast_float(self%val)
end subroutine get_float


!> Obtain integer value from TOML key-value pair
subroutine get_integer(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   integer(tfi), pointer, intent(out) :: val

   val => cast_integer(self%val)
end subroutine get_integer


!> Obtain boolean value from TOML key-value pair
subroutine get_boolean(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   logical, pointer, intent(out) :: val

   val => cast_boolean(self%val)
end subroutine get_boolean


!> Obtain datetime value from TOML key-value pair
subroutine get_datetime(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   type(toml_datetime), pointer, intent(out) :: val

   val => cast_datetime(self%val)
end subroutine get_datetime


!> Obtain datetime value from TOML key-value pair
subroutine get_string(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   character(:, tfc), pointer, intent(out) :: val

   val => cast_string(self%val)
end subroutine get_string


!> Obtain real value from TOML key-value pair
subroutine set_float(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   real(tfr), intent(in) :: val

   type(float_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_float


!> Obtain integer value from TOML key-value pair
subroutine set_integer(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   integer(tfi), intent(in) :: val

   type(integer_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_integer


!> Obtain boolean value from TOML key-value pair
subroutine set_boolean(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   logical, intent(in) :: val

   type(boolean_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_boolean


!> Obtain datetime value from TOML key-value pair
subroutine set_datetime(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   type(toml_datetime), intent(in) :: val

   type(datetime_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_datetime


!> Obtain datetime value from TOML key-value pair
subroutine set_string(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   character(*, tfc), intent(in) :: val

   type(string_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_string


!> Get the type of the value stored in the key-value pair
pure function get_type(self) result(value_type)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value type
   integer :: value_type

   select type(val => self%val)
   class default
      value_type = toml_type%invalid
   type is(float_value)
      value_type = toml_type%float
   type is(integer_value)
      value_type = toml_type%int
   type is(boolean_value)
      value_type = toml_type%boolean
   type is(datetime_value)
      value_type = toml_type%datetime
   type is(string_value)
      value_type = toml_type%string
   end select
end function get_type


function cast_float(val) result(ptr)
   class(generic_value), intent(in), target :: val
   real(tfr), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(float_value)
      ptr => val%raw
   end select
end function cast_float

function cast_integer(val) result(ptr)
   class(generic_value), intent(in), target :: val
   integer(tfi), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(integer_value)
      ptr => val%raw
   end select
end function cast_integer

function cast_boolean(val) result(ptr)
   class(generic_value), intent(in), target :: val
   logical, pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(boolean_value)
      ptr => val%raw
   end select
end function cast_boolean

function cast_datetime(val) result(ptr)
   class(generic_value), intent(in), target :: val
   type(toml_datetime), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(datetime_value)
      ptr => val%raw
   end select
end function cast_datetime

function cast_string(val) result(ptr)
   class(generic_value), intent(in), target :: val
   character(:, tfc), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(string_value)
      ptr => val%raw
   end select
end function cast_string

end module tomlf_type_keyval


!>>>>> build/dependencies/toml-f/src/tomlf/utils/sort.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Sorting algorithms to work with hash maps
module tomlf_utils_sort
   use tomlf_type_value, only : toml_key
   implicit none
   private

   public :: sort, compare_less


   !> Create overloaded interface for export
   interface sort
      module procedure :: sort_keys
   end interface


   abstract interface
      !> Define order relation between two TOML keys
      pure function compare_less(lhs, rhs) result(less)
         import :: toml_key
         !> Left hand side TOML key in comparison
         type(toml_key), intent (in) :: lhs
         !> Right hand side TOML key in comparison
         type(toml_key), intent (in) :: rhs
         !> Comparison result
         logical :: less
      end function compare_less
   end interface


contains


   !> Entry point for sorting algorithm
   pure subroutine sort_keys(list, idx, compare)

      !> List of TOML keys to be sorted
      type(toml_key), intent(inout) :: list(:)

      !> Optionally, mapping from unsorted list to sorted list
      integer, intent(out), optional :: idx(:)

      !> Function implementing the order relation between two TOML keys
      procedure(compare_less), optional :: compare

      integer  :: low, high, i
      type(toml_key), allocatable  :: sorted(:)
      integer, allocatable :: indexarray(:)

      low = 1
      high = size(list)

      allocate(sorted, source=list)

      allocate(indexarray(high), source=[(i, i=low, high)])

      if (present(compare)) then
         call quicksort(sorted, indexarray, low, high, compare)
      else
         call quicksort(sorted, indexarray, low, high, compare_keys_less)
      end if

      do i = low, high
         list(i) = sorted(indexarray(i))
      end do

      if (present(idx)) then
         idx = indexarray
      end if

   end subroutine sort_keys


   !> Actual quick sort implementation
   pure recursive subroutine quicksort(list, idx, low, high, less)
      type(toml_key), intent(inout) :: list(:)
      integer, intent(inout) :: idx(:)
      integer, intent(in) :: low, high
      procedure(compare_less) :: less

      integer :: i, last
      integer :: pivot

      if (low < high) then

         call swap(idx(low), idx((low + high)/2))
         last = low
         do i = low + 1, high
            if (less(list(idx(i)), list(idx(low)))) then
               last = last + 1
               call swap(idx(last), idx(i))
            end if
         end do
         call swap(idx(low), idx(last))
         pivot = last

         call quicksort(list, idx, low, pivot - 1, less)
         call quicksort(list, idx, pivot + 1, high, less)
      end if

   end subroutine quicksort


   !> Swap two integer values
   pure subroutine swap(lhs, rhs)
      integer, intent(inout) :: lhs
      integer, intent(inout) :: rhs

      integer :: tmp

      tmp = lhs
      lhs = rhs
      rhs = tmp

   end subroutine swap


   !> Default comparison between two TOML keys
   pure function compare_keys_less(lhs, rhs) result(less)
      type(toml_key), intent (in) :: lhs
      type(toml_key), intent (in) :: rhs
      logical :: less

      less = lhs%key < rhs%key

   end function compare_keys_less


end module tomlf_utils_sort


!>>>>> ././src/ffmt_case.f90
module ffmt_case
!! Letter case of keywords, dot operators, intrinsic procedures, the constants of the intrinsic
!! modules, and kinds.
!!
!! Fortran names are not reserved, so a word is only treated as a keyword where its position makes it
!! one: the leading words of a statement (`end subroutine`, `integer`, `if`, `else if`, `select case`,
!! `implicit none`, ...), the attributes of a declaration before `::`, `in`/`out`/`inout` in `intent()`,
!! `kind=`/`len=` in a type, `then`, `only:`, `result()` and `bind()`. A word that names something
!! (`call open(...)`, `subroutine real`, `end function do`) keeps its case. Changing the case of a name
!! never changes the meaning of the code, so a misclassified word is a matter of style only.
!!
!! Intrinsic procedures are recognized where they are called (followed by `(`, or after `call`), and the
!! constants and types of `iso_fortran_env` and `iso_c_binding` wherever they are used, but not as the
!! first word of a statement, after `%`, as an entity being declared, or as the name of a procedure
!! being defined. Kinds are the values of `kind=`, the kind of a declaration's type (`real(dp) :: x`),
!! the kind of a literal (`2_int64`, `.true._lk`, `ck_'a'`) and exponent letters (`1.0e-3`).
use ffmt_tokens
use ffmt_statements
use ffmt_options, only: case_options_t, CASE_KEEP
use ffmt_source, only: source_t
implicit none
private

public :: compute_case, kind_part

type, public :: case_marks_t
    !! The letter case wanted for each token (`CASE_KEEP`, `CASE_LOWER` or `CASE_UPPER`).
    integer, allocatable :: whole(:)   !! for the whole text of each token
    integer, allocatable :: part(:)    !! for the kind part of a token only (see [[kind_part]]), applied
                                       !! after `whole`
end type case_marks_t

! words that are keywords at the start of a statement (or of a declaration attribute)
character(len=*), parameter :: KEYWORDS(*) = [character(len=16) :: &
                                              'abstract', 'allocatable', 'allocate', 'assign', 'associate', &
                                              'asynchronous', 'backspace', 'bind', 'block', 'call', 'case', &
                                              'change', 'character', 'class', 'close', 'codimension', &
                                              'common', 'complex', 'concurrent', 'contains', 'contiguous', &
                                              'continue', 'critical', 'cycle', 'data', 'deallocate', &
                                              'default', 'deferred', 'dimension', 'do', 'double', &
                                              'doubleprecision', 'elemental', 'else', 'elseif', 'elsewhere', &
                                              'end', 'endfile', 'entry', 'enum', 'enumerator', &
                                              'enumeration', 'equivalence', 'error', 'exit', 'extends', 'external', &
                                              'final', 'flush', 'forall', 'function', 'generic', 'go', &
                                              'goto', 'if', 'implicit', 'import', 'impure', 'include', &
                                              'inquire', 'integer', 'intent', 'interface', 'intrinsic', &
                                              'is', 'logical', 'module', 'namelist', 'non_intrinsic', &
                                              'non_overridable', 'non_recursive', 'none', 'nopass', &
                                              'nullify', 'open', 'optional', 'parameter', 'pass', 'pause', &
                                              'pointer', 'precision', 'print', 'private', 'procedure', &
                                              'program', 'protected', 'public', 'pure', 'rank', 'read', &
                                              'real', 'recursive', 'return', 'rewind', 'save', 'select', &
                                              'selectcase', 'selectrank', 'selecttype', 'sequence', &
                                              'simple', 'stop', 'submodule', 'subroutine', 'sync', 'target', &
                                              'team', 'then', 'type', 'use', 'value', 'volatile', 'wait', &
                                              'where', 'while', 'write']

! keywords followed by a name: the leading words of a statement end after them
character(len=*), parameter :: INTRODUCERS(*) = [character(len=12) :: &
                                                 'call', 'subroutine', 'function', 'program', 'module', 'submodule', 'use', &
                                                 'entry', &
                                                 'procedure', 'interface', 'go', 'goto', 'cycle', 'exit', 'data', 'common', &
                                                 'namelist', 'is']

! intrinsic procedures (F2023 table 16.1), specific names, and the procedures of the intrinsic modules
character(len=*), parameter :: INTRINSIC_PROCEDURES(*) = [character(len=24) :: &
                                                          'abs', 'achar', 'acos', 'acosd', 'acosh', 'acospi', 'adjustl', &
                                                          'adjustr', 'aimag', &
                                                          'aint', 'all', 'allocated', 'alog', 'alog10', 'amax0', 'amax1', 'amin0', &
                                                          'amin1', 'amod', &
                                                          'anint', 'any', 'asin', 'asind', 'asinh', 'asinpi', 'associated', &
                                                          'atan', 'atan2', &
                                                          'atan2d', 'atan2pi', 'atand', 'atanh', 'atanpi', 'atomic_add', &
                                                          'atomic_and', &
                                                          'atomic_cas', 'atomic_define', 'atomic_fetch_add', 'atomic_fetch_and', &
                                                          'atomic_fetch_or', &
                                                          'atomic_fetch_xor', 'atomic_or', 'atomic_ref', 'atomic_xor', &
                                                          'bessel_j0', 'bessel_j1', &
                                                          'bessel_jn', 'bessel_y0', 'bessel_y1', 'bessel_yn', 'bge', 'bgt', &
                                                          'bit_size', 'ble', &
                                                          'blt', 'btest', 'c_associated', 'c_f_pointer', 'c_f_procpointer', &
                                                          'c_f_strpointer', &
                                                          'c_funloc', 'c_loc', 'c_sizeof', 'cabs', 'ccos', 'ceiling', 'cexp', &
                                                          'char', 'clog', &
                                                          'cmplx', 'co_broadcast', 'co_max', 'co_min', 'co_reduce', 'co_sum', &
                                                          'command_argument_count', 'compiler_options', 'compiler_version', &
                                                          'conjg', 'cos', 'cosd', &
                                                          'cosh', 'coshape', 'cospi', 'count', 'cpu_time', 'cshift', 'csin', &
                                                          'csqrt', 'dabs', &
                                                          'dacos', 'dasin', 'datan', 'datan2', 'date_and_time', 'dble', 'dcos', &
                                                          'dcosh', 'ddim', &
                                                          'dexp', 'digits', 'dim', 'dint', 'dlog', 'dlog10', 'dmax1', 'dmin1', &
                                                          'dmod', 'dnint', &
                                                          'dot_product', 'dprod', 'dshiftl', 'dshiftr', 'dsign', 'dsin', 'dsinh', &
                                                          'dsqrt', 'dtan', &
                                                          'dtanh', 'eoshift', 'epsilon', 'erf', 'erfc', 'erfc_scaled', &
                                                          'event_query', &
                                                          'execute_command_line', 'exp', 'exponent', 'extends_type_of', &
                                                          'f_c_string', &
                                                          'failed_images', 'findloc', 'float', 'floor', 'fraction', 'gamma', &
                                                          'get_command', &
                                                          'get_command_argument', 'get_environment_variable', 'get_team', 'huge', &
                                                          'hypot', 'iabs', &
                                                          'iachar', 'iall', 'iand', 'iany', 'ibclr', 'ibits', 'ibset', 'ichar', &
                                                          'idim', 'idint', &
                                                          'idnint', 'ieor', 'ifix', 'image_index', 'image_status', 'index', 'int', &
                                                          'ior', &
                                                          'iparity', 'is_contiguous', 'is_iostat_end', 'is_iostat_eor', 'ishft', &
                                                          'ishftc', 'isign', &
                                                          'kind', 'lbound', 'lcobound', 'leadz', 'len', 'len_trim', 'lge', 'lgt', &
                                                          'lle', 'llt', &
                                                          'log', 'log10', 'log_gamma', 'logical', 'maskl', 'maskr', 'matmul', &
                                                          'max', 'max0', &
                                                          'max1', 'maxexponent', 'maxloc', 'maxval', 'merge', 'merge_bits', 'min', &
                                                          'min0', 'min1', &
                                                          'minexponent', 'minloc', 'minval', 'mod', 'modulo', 'move_alloc', &
                                                          'mvbits', 'nearest', &
                                                          'new_line', 'nint', 'norm2', 'not', 'null', 'num_images', &
                                                          'out_of_range', 'pack', &
                                                          'parity', 'popcnt', 'poppar', 'precision', 'present', 'product', &
                                                          'radix', 'random_init', &
                                                          'random_number', 'random_seed', 'range', 'rank', 'real', 'reduce', &
                                                          'repeat', 'reshape', &
                                                          'rrspacing', 'same_type_as', 'scale', 'scan', 'selected_char_kind', &
                                                          'selected_int_kind', &
                                                          'selected_logical_kind', 'selected_real_kind', 'set_exponent', 'shape', &
                                                          'shifta', &
                                                          'shiftl', 'shiftr', 'sign', 'sin', 'sind', 'sinh', 'sinpi', 'size', &
                                                          'sngl', 'spacing', &
                                                          'split', 'spread', 'sqrt', 'stopped_images', 'storage_size', 'sum', &
                                                          'system_clock', &
                                                          'tan', 'tand', 'tanh', 'tanpi', 'team_number', 'this_image', 'tiny', &
                                                          'tokenize', &
                                                          'trailz', 'transfer', 'transpose', 'trim', 'ubound', 'ucobound', &
                                                          'unpack', 'verify']

! named constants and types of iso_fortran_env and iso_c_binding
character(len=*), parameter :: INTRINSIC_CONSTANTS(*) = [character(len=28) :: &
                                                         'atomic_int_kind', 'atomic_logical_kind', 'c_alert', 'c_backspace', &
                                                         'c_bool', &
                                                         'c_carriage_return', 'c_char', 'c_double', 'c_double_complex', 'c_float', &
                                                         'c_float128', &
                                                         'c_float128_complex', 'c_float_complex', 'c_form_feed', 'c_funptr', &
                                                         'c_horizontal_tab', &
                                                         'c_int', 'c_int128_t', 'c_int16_t', 'c_int32_t', 'c_int64_t', 'c_int8_t', &
                                                         'c_int_fast128_t', 'c_int_fast16_t', 'c_int_fast32_t', 'c_int_fast64_t', &
                                                         'c_int_fast8_t', &
                                                         'c_int_least128_t', 'c_int_least16_t', 'c_int_least32_t', &
                                                         'c_int_least64_t', &
                                                         'c_int_least8_t', 'c_intmax_t', 'c_intptr_t', 'c_long', 'c_long_double', &
                                                         'c_long_double_complex', 'c_long_long', 'c_new_line', 'c_null_char', &
                                                         'c_null_funptr', &
                                                         'c_null_ptr', 'c_ptr', 'c_ptrdiff_t', 'c_short', 'c_signed_char', &
                                                         'c_size_t', &
                                                         'c_vertical_tab', 'character_kinds', 'character_storage_size', &
                                                         'current_team', &
                                                         'error_unit', 'event_type', 'file_storage_size', 'initial_team', &
                                                         'input_unit', 'int16', &
                                                         'int32', 'int64', 'int8', 'integer_kinds', 'iostat_end', 'iostat_eor', &
                                                         'iostat_inquire_internal_unit', 'lock_type', 'logical16', 'logical32', &
                                                         'logical64', &
                                                         'logical8', 'logical_kinds', 'notify_type', 'numeric_storage_size', &
                                                         'output_unit', &
                                                         'parent_team', 'real128', 'real16', 'real32', 'real64', 'real_kinds', &
                                                         'stat_failed_image', 'stat_locked', 'stat_locked_other_image', &
                                                         'stat_stopped_image', &
                                                         'stat_unlocked', 'stat_unlocked_failed_image', 'team_type']

contains

function compute_case(source, views, opts) result(marks)
    !! The letter case wanted for each token.
    type(source_t), intent(in) :: source            !! the source
    type(statement_view_t), intent(in) :: views(:)  !! the view of each item (see [[statement_views]])
    type(case_options_t), intent(in) :: opts        !! the case wanted for each category
    type(case_marks_t) :: marks
    integer :: k, t, a, b

    allocate (marks%whole(source%tokens%n), marks%part(source%tokens%n))
    marks%whole = CASE_KEEP
    marks%part = CASE_KEEP
    do t = 1, source%tokens%n
        associate (tok => source%tokens%tok(t))
            select case (tok%kind)
            case (TK_DOTOP)
                marks%whole(t) = opts%operators
            case (TK_INT, TK_REAL)
                marks%whole(t) = opts%types          ! their only letters: an exponent and a kind
            end select
            if (opts%types /= CASE_KEEP .and. (tok%kind == TK_DOTOP .or. tok%kind == TK_STRING)) then
                call kind_part(source%text(tok%first:tok%last), tok%kind, a, b)
                if (b >= a) marks%part(t) = opts%types
            end if
        end associate
    end do
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind /= SK_CODE) cycle
        if (opts%keywords /= CASE_KEEP) call statement_case(views(k), opts%keywords, marks%whole)
        if (opts%procedures /= CASE_KEEP .or. opts%constants /= CASE_KEEP .or. opts%types /= CASE_KEEP) &
            call names_case(views(k), opts, marks%whole)
    end do
end function compute_case

pure subroutine kind_part(text, kind, a, b)
    !! Where the kind is in the text of a token of kind `kind`: `text(a:b)`, or `b < a` if it has none.
    !! For a logical constant it is the suffix (`_lk` in `.true._lk`), for a string the prefix (`ck` in
    !! `ck_'a'`). Numbers have letters only in their exponent and kind, so they are cased whole.
    character(len=*), intent(in) :: text   !! the token's text
    integer, intent(in) :: kind            !! its kind (`TK_*`)
    integer, intent(out) :: a, b           !! the range of the kind in `text`
    integer :: p
    a = 1
    b = 0
    select case (kind)
    case (TK_DOTOP)
        p = index(text, '.', back=.true.)
        if (p > 1 .and. p < len(text)) then
            if (text(p + 1:p + 1) == '_') then
                a = p + 2
                b = len(text)
            end if
        end if
    case (TK_STRING)
        p = scan(text, '"'//"'")
        if (p > 2) then
            if (text(p - 1:p - 1) == '_') b = p - 2
        end if
    end select
end subroutine kind_part

subroutine names_case(v, opts, mode)
    !! Mark the intrinsic procedures, the constants of the intrinsic modules and the kinds named in
    !! one code statement.
    type(statement_view_t), intent(in) :: v      !! the statement
    type(case_options_t), intent(in) :: opts     !! the case wanted for each category
    integer, intent(inout) :: mode(:)            !! case mode of each token, set for the names
    integer :: j, first
    logical :: decl, use_stmt, entity

    if (v%n == 0) return
    first = v%first
    if (first + 1 < v%n) then
        if (v%kind_of(first) == TK_NAME .and. v%is(first + 1, ':')) first = first + 2   ! construct name
    end if
    use_stmt = v%is(first, 'use')
    decl = .false.
    if (.not. use_stmt) decl = any([(v%is(j, '::') .and. v%depth(j) == 0, j=1, v%n)])
    entity = .false.             ! after `::`, the next top-level name is an entity being declared
    do j = first, v%n
        if (decl .and. v%depth(j) == 0) then
            if (v%is(j, '::') .or. (entity .and. v%is(j, ','))) then
                entity = .true.
                cycle
            end if
        end if
        if (v%kinds(j) /= TK_NAME) cycle
        if (j == first) cycle
        if (v%is(j - 1, '%')) cycle
        if (decl .and. entity .and. v%depth(j) == 0 .and. (v%is(j - 1, '::') .or. v%is(j - 1, ','))) cycle
        select case (v%word(j - 1))
        case ('function', 'subroutine', 'procedure', 'entry', 'program', 'module', 'submodule', 'interface')
            cycle
        end select
        if (opts%procedures /= CASE_KEEP .and. (v%is(j + 1, '(') .or. v%is(j - 1, 'call'))) then
            if (any(INTRINSIC_PROCEDURES == v%word(j))) mode(v%code(j)) = opts%procedures
        end if
        if (opts%constants /= CASE_KEEP .and. .not. v%is(j + 1, '=')) then
            if (any(INTRINSIC_CONSTANTS == v%word(j))) mode(v%code(j)) = opts%constants
        end if
        if (opts%types /= CASE_KEEP .and. is_kind(j)) mode(v%code(j)) = opts%types
    end do

contains

    logical function is_kind(j)
        !! True if name `j` is a kind: the value of `kind=`, or the kind of the type that starts a
        !! declaration (`real(dp) :: x`, `integer(int64), parameter :: n = 1`).
        integer, intent(in) :: j   !! index into `v%code(:)`
        is_kind = v%is(j - 1, '=') .and. v%is(j - 2, 'kind')
        if (is_kind .or. .not. decl) return
        if (j - 2 /= first .or. .not. v%is(j - 1, '(') .or. .not. v%is(j + 1, ')')) return
        select case (v%word(first))
        case ('integer', 'real', 'complex', 'logical', 'character')
            is_kind = .true.
        end select
    end function is_kind

end subroutine names_case

subroutine statement_case(v, kw_mode, mode)
    !! Mark the keywords of one code statement with `kw_mode`.
    type(statement_view_t), intent(in) :: v    !! the statement
    integer, intent(in) :: kw_mode             !! case mode for keywords
    integer, intent(inout) :: mode(:)          !! case mode of each token, set for the keywords
    integer :: k

    if (v%n == 0) return
    k = v%first
    if (k + 1 < v%n) then
        if (v%kind_of(k) == TK_NAME .and. v%is(k + 1, ':')) k = k + 2   ! construct name
    end if
    if (v%is(k, 'format')) return
    call leading(k)
    call attributes(k)
    call trailing(k)

contains

    subroutine attributes(k)
        !! Mark the attributes of a declaration: the word after each top-level `,` before `::`, and
        !! `in`, `out` and `inout` in `intent(...)`.
        integer, intent(in) :: k   !! index into `v%code(:)` of the first word of the statement
        integer :: j
        if (.not. any([(v%is(j, '::') .and. v%depth(j) == 0, j=1, v%n)])) return
        do j = k, v%n
            if (v%depth(j) > 0) cycle
            if (v%is(j, '::')) exit
            if (v%is(j, ',') .and. is_keyword(j + 1)) then
                call mark(j + 1)
                if (v%is(j + 1, 'intent')) call intent_args(j + 2)
            end if
        end do
    end subroutine attributes

    subroutine trailing(k)
        !! Mark the keywords after the leading ones: `only` of a `use` statement, and `result` and
        !! `bind` after a parenthesis.
        integer, intent(in) :: k   !! index into `v%code(:)` of the first word of the statement
        integer :: j
        do j = k + 1, v%n
            if (v%depth(j) > 0) cycle
            select case (v%word(j))
            case ('only')
                if (v%is(j + 1, ':') .and. v%is(j - 1, ',')) call mark(j)
            case ('result', 'bind')
                if (v%is(j - 1, ')') .and. v%is(j + 1, '(')) call mark(j)
            end select
        end do
    end subroutine trailing

    subroutine mark(j)
        !! Give code token `j` the keyword case mode, if it is inside the statement.
        integer, intent(in) :: j   !! index into `v%code(:)`
        if (j >= 1 .and. j <= v%n) mode(v%code(j)) = kw_mode
    end subroutine mark

    logical function is_keyword(j)
        !! True if code token `j` is a name spelled like a keyword (including `endif`, `enddo`, ...).
        integer, intent(in) :: j   !! index into `v%code(:)`
        character(len=:), allocatable :: w
        is_keyword = .false.
        if (v%kind_of(j) /= TK_NAME) return
        w = v%word(j)
        if (any(KEYWORDS == w)) then
            is_keyword = .true.
        else if (len(w) > 3) then
            if (w(1:3) == 'end') is_keyword = any(KEYWORDS == w(4:)) .or. w(4:) == 'blockdata'
        end if
        if (w == 'blockdata') is_keyword = .true.
    end function is_keyword

    integer function after_parens(j) result(next)
        !! Index after the parenthesis group that opens at `j`, or `j` if there is none there.
        integer, intent(in) :: j   !! index into `v%code(:)`
        next = j
        if (.not. v%is(j, '(')) return
        next = v%closing(j)
        if (next == 0) next = v%n
        next = next + 1
    end function after_parens

    logical function is_assignment(j)
        !! True if the statement starting at `j` assigns to a variable: `name [(...)] =`, `=>` or `%`.
        integer, intent(in) :: j   !! index into `v%code(:)` of the first word
        integer :: m
        m = after_parens(j + 1)
        is_assignment = v%is(m, '=') .or. v%is(m, '=>') .or. v%is(m, '%') .or. &
                        v%is(j + 1, '=') .or. v%is(j + 1, '=>') .or. v%is(j + 1, '%')
    end function is_assignment

    subroutine intent_args(j)
        !! `in`, `out` and `inout` in `intent(...)` starting at the `(` at `j`.
        integer, intent(in) :: j   !! index into `v%code(:)`
        integer :: m
        if (.not. v%is(j, '(')) return
        do m = j + 1, v%n
            select case (v%word(m))
            case ('in', 'out', 'inout')
                call mark(m)
            case (')')
                return
            end select
        end do
    end subroutine intent_args

    subroutine type_params(j)
        !! `kind=` and `len=` in a type selector starting at the `(` at `j`.
        integer, intent(in) :: j   !! index into `v%code(:)`
        integer :: m, last
        last = v%closing(j)
        if (last == 0) then
            if (.not. v%is(j, '(')) return
            last = v%n
        end if
        do m = j + 1, last - 1
            if (v%depth(m) /= v%depth(j) + 1) cycle      ! directly inside the parentheses
            if ((v%is(m, 'kind') .or. v%is(m, 'len')) .and. v%is(m + 1, '=')) call mark(m)
        end do
    end subroutine type_params

    recursive subroutine leading(start)
        !! Mark the leading keywords of the statement starting at code token `start`.
        integer, intent(in) :: start   !! index into `v%code(:)`
        integer :: j, m
        character(len=:), allocatable :: w

        if (v%kind_of(start) /= TK_NAME) return
        if (is_assignment(start)) return
        j = start
        do while (j <= v%n)
            if (.not. is_keyword(j)) return
            w = v%word(j)
            call mark(j)
            if (w == 'go' .and. v%is(j + 1, 'to')) call mark(j + 1)
            if (any(INTRODUCERS == w)) return
            select case (w)
            case ('if', 'elseif', 'where', 'forall', 'while', 'case', 'rank')
                ! a condition or selector in parentheses, then `then` or another statement
                m = after_parens(j + 1)
                if (m == j + 1) then
                    j = j + 1
                    cycle
                end if
                if (v%is(m, 'then')) then
                    call mark(m)
                else if (w == 'if' .or. w == 'where' .or. w == 'forall') then
                    call leading(m)                  ! one-line `if (x) call f()`
                end if
                return
            case ('integer', 'real', 'complex', 'logical', 'character', 'type', 'class', 'precision', 'procedure')
                call type_params(j + 1)
                m = after_parens(j + 1)
                if (v%is(m, '*')) m = after_parens(m + 1) + merge(0, 1, v%is(m + 1, '('))
                j = m
                ! after a type, only a procedure prefix or `function` can follow; a name is declared
                select case (v%word(j))
                case ('function', 'pure', 'impure', 'elemental', 'recursive', 'non_recursive', 'module', 'simple')
                    continue
                case default
                    return
                end select
            case ('intent')
                call intent_args(j + 1)
                return
            case ('select')
                j = j + 1
            case default
                if (v%is(j + 1, '(')) return       ! `write(...)`, `allocate(...)`, `dimension(...)`
                j = j + 1
            end select
        end do
    end subroutine leading

end subroutine statement_case

end module ffmt_case


!>>>>> ././src/ffmt_linebreak.f90
module ffmt_linebreak
!! Splitting code lines that are longer than the line length.
!!
!! A line is split at a token boundary, preferring (in this order) after a comma, after an operator,
!! `=` or `::`, after an opening bracket, and anywhere between two tokens. Within the best kind of
!! break that fits, the rightmost is used. The first part ends with ` &` and the rest goes on a new
!! line; the layout pass then aligns it. Only the code counts: a trailing comment may extend past
!! the limit and moves with the last part. Lines are not split inside tokens (strings in particular),
!! in protected code, in comment or preprocessor lines, or when a token spans lines.
use ffmt_tokens
use ffmt_statements
use ffmt_source, only: source_t, parse_source
use ffmt_strings, only: string_builder_t, utf8_width
implicit none
private

public :: split_long_lines, long_code_lines

integer, parameter, public :: MAX_CONTINUATIONS = 255   !! continuation lines a statement may have before
                                                        !! Fortran 2023 (F2018 6.3.2.1); splitting stops there

contains

function split_long_lines(src, line_length, indent, nsplit, changed) result(out)
    !! `src` with each code line longer than `line_length` split once. The new continuation lines are indented
    !! by `indent` more than the line they come from if it is the first line of a statement, and like
    !! it if it is a continuation line, which the layout pass will usually leave in place. `nsplit`
    !! returns the number of splits.
    character(len=*), intent(in) :: src           !! the source text
    integer, intent(in) :: line_length            !! maximum line width, in characters
    integer, intent(in) :: indent                 !! indentation step, in blanks
    integer, intent(out) :: nsplit                !! number of lines split
    integer, allocatable, intent(out), optional :: changed(:, :)   !! where the statements that were split
                                                  !! are in `out`: bytes `changed(1, j):changed(2, j)`.
                                                  !! Not allocated if one of them cannot be laid out
                                                  !! again on its own (see [[stands_alone]])
    character(len=:), allocatable :: out

    type(source_t) :: source
    type(string_builder_t) :: buf
    character(len=:), allocatable :: nl
    integer :: k, j, lend, a, p, q, base, start_col, offset, ncont, before, start, nranges
    integer, allocatable :: ranges(:, :)
    logical :: alone

    nsplit = 0
    call parse_source(src, source)
    nl = achar(10)
    do j = 1, source%tokens%n
        if (source%tokens%tok(j)%kind == TK_NEWLINE) then
            nl = token_text(src, source%tokens%tok(j))
            exit
        end if
    end do

    allocate (ranges(2, 16))
    nranges = 0
    alone = .true.
    do k = 1, source%stmts%n
        before = nsplit
        start = buf%length() + 1
        if (source%keep(k) .or. source%stmts%s(k)%kind /= SK_CODE) then
            call copy(source%stmts%s(k)%first, source%stmts%s(k)%last)
            cycle
        end if
        associate (s => source%stmts%s(k))
            ncont = count(source%tokens%tok(s%first:s%last)%kind == TK_CONT .and. &
                          source%tokens%tok(s%first:s%last)%sub == CT_TRAILING)
        end associate
        j = source%stmts%s(k)%first
        do while (j <= source%stmts%s(k)%last)
            lend = j
            do while (lend < source%stmts%s(k)%last)
                if (source%tokens%tok(lend)%kind == TK_NEWLINE) exit
                lend = lend + 1
            end do
            ! split the line once; the rest is aligned by the next layout pass and split again if needed
            a = j
            start_col = column(j)
            base = indentation(j)
            offset = merge(indent, 0, j == source%stmts%s(k)%first)
            p = 0
            if (ncont < MAX_CONTINUATIONS) p = break_point(source%tokens, src, a, lend, start_col, line_length)
            if (p > 0) then
                ncont = ncont + 1
                call copy(a, p)
                call buf%append(' &'//nl//repeat(' ', base + offset))
                nsplit = nsplit + 1
                q = p + 1
                do while (source%tokens%tok(q)%kind == TK_SPACE)
                    q = q + 1
                end do
                a = q
            end if
            call copy(a, lend)
            j = lend + 1
        end do
        if (nsplit > before) then
            if (nranges == size(ranges, 2)) ranges = reshape([ranges, ranges], [2, 2*nranges])
            nranges = nranges + 1
            ranges(:, nranges) = [start, buf%length()]
            alone = alone .and. stands_alone(source, k)
        end if
    end do
    out = buf%get()
    if (present(changed) .and. alone) changed = ranges(:, 1:nranges)

contains

    subroutine copy(first, last)
        !! Append the text of tokens `first:last` to the output.
        integer, intent(in) :: first, last   !! token range
        integer :: t
        do t = first, last
            call buf%append(token_text(src, source%tokens%tok(t)))
        end do
    end subroutine copy

    integer function column(t)
        !! Column of token `t` in `src`.
        integer, intent(in) :: t   !! index into the token list
        integer :: i
        i = scan(src(1:source%tokens%tok(t)%first - 1), achar(10)//achar(13), back=.true.)
        column = utf8_width(src(i + 1:source%tokens%tok(t)%first - 1))
    end function column

    integer function indentation(t)
        !! Indentation of the line containing token `t`.
        integer, intent(in) :: t   !! index into the token list
        integer :: i, m
        i = scan(src(1:source%tokens%tok(t)%first - 1), achar(10)//achar(13), back=.true.) + 1
        m = verify(src(i:), ' '//achar(9))
        if (m == 0) then
            indentation = 0
        else
            indentation = m - 1
        end if
    end function indentation

end function split_long_lines

pure logical function stands_alone(source, k)
    !! True if statement `k` of `source`, a code statement, can be laid out again on its own after
    !! one of its lines is split, giving what laying out the whole text would: it takes up whole
    !! lines (no `;` before or after it on its lines, no byte order mark), has no statement label
    !! (which stays at the start of the line), and is not followed by a comment line that may be
    !! lined up with a comment at the end of its last line.
    type(source_t), intent(in) :: source   !! the source
    integer, intent(in) :: k               !! index into `source%stmts`
    integer :: j, f

    stands_alone = .false.
    associate (s => source%stmts%s(k), tok => source%tokens%tok)
        if (s%first > 1) then
            if (tok(s%first - 1)%kind /= TK_NEWLINE) return
        end if
        if (tok(s%last)%kind /= TK_NEWLINE .and. s%last < source%tokens%n) return
        if (any(tok(s%first:s%last)%kind == TK_BOM)) return
        f = s%first
        do while (f < s%last .and. tok(f)%kind == TK_SPACE)
            f = f + 1
        end do
        if (tok(f)%kind == TK_INT) return                   ! a statement label
        if (k < source%stmts%n) then
            if (source%stmts%s(k + 1)%kind == SK_COMMENT) then
                ! a comment at the end of the last line, before its line break
                j = s%last
                if (tok(j)%kind == TK_NEWLINE) j = j - 1
                if (j >= s%first .and. tok(j)%kind == TK_COMMENT) return
            end if
        end if
    end associate
    stands_alone = .true.
end function stands_alone

integer function break_point(tokens, src, a, lend, start_col, line_length) result(best)
    !! The token after which to split tokens `a:lend` of a line that starts at token `a` in column
    !! `start_col`, or 0 if the code fits or cannot be split.
    type(token_list_t), intent(in) :: tokens      !! the tokens of `src`
    character(len=*), intent(in) :: src           !! the source text
    integer, intent(in) :: a                      !! first token of the line
    integer, intent(in) :: lend                   !! last token of the line, its line break if it has one
    integer, intent(in) :: start_col              !! column of token `a`
    integer, intent(in) :: line_length            !! maximum line width, in characters
    integer :: t, cend, p, q, prio, best_prio, end_col

    best = 0
    cend = 0
    do t = a, lend
        if (scan(token_text(src, tokens%tok(t)), achar(10)//achar(13)) > 0 .and. tokens%tok(t)%kind /= TK_NEWLINE) &
            return                                      ! a token continued on the next line
        if (tokens%tok(t)%kind == TK_COMMENT) exit
        if (is_code_kind(tokens%tok(t)%kind) .or. tokens%tok(t)%kind == TK_CONT) cend = t
    end do
    if (cend == 0) return
    if (end_column(cend) <= line_length) return

    best_prio = huge(1)
    do p = a, cend - 1
        if (.not. is_code_kind(tokens%tok(p)%kind)) cycle
        if (p == a .and. tokens%tok(p)%kind == TK_INT) cycle    ! a statement label
        q = p + 1
        do while (q <= cend)
            if (tokens%tok(q)%kind /= TK_SPACE) exit
            q = q + 1
        end do
        if (q > cend) cycle
        if (.not. is_code_kind(tokens%tok(q)%kind) .or. tokens%tok(q)%glued) cycle
        end_col = end_column(p)
        if (end_col + 2 > line_length) exit                     ! ` &` would not fit
        prio = priority(token_lower(src, tokens%tok(p)), tokens%tok(p)%kind)
        if (prio <= best_prio) then
            best_prio = prio
            best = p
        end if
    end do

contains

    integer function end_column(t)
        !! Column just after token `t`, for a line starting at token `a` in column `start_col`.
        integer, intent(in) :: t   !! index into the token list
        end_column = start_col + utf8_width(src(tokens%tok(a)%first:tokens%tok(t)%last))
    end function end_column

end function break_point

pure integer function priority(text, kind) result(prio)
    !! How good a place the end of a token with this text is for a line break (1 is best).
    character(len=*), intent(in) :: text   !! lowercase text of the token
    integer, intent(in) :: kind            !! token kind (`TK_*`)
    prio = 4
    select case (kind)
    case (TK_OP)
        select case (text)
        case (',')
            prio = 1
        case ('(', '[')
            prio = 3
        case (')', ']', '%', ':')
            prio = 4
        case default
            prio = 2
        end select
    case (TK_DOTOP)
        if (text /= '.true.' .and. text /= '.false.' .and. text /= '.not.') prio = 2
    end select
end function priority

function long_code_lines(src, line_length) result(lines)
    !! Line numbers of code lines whose code (without a trailing comment) is longer than `line_length`,
    !! outside protected code.
    character(len=*), intent(in) :: src           !! the source text
    integer, intent(in) :: line_length            !! maximum line width, in characters; 0 or less for no limit
    integer, allocatable :: lines(:)
    type(source_t) :: source
    integer :: k, t, col, last_line, line, p, q

    allocate (lines(0))
    if (line_length <= 0) return
    call parse_source(src, source)
    last_line = 0
    do k = 1, source%stmts%n
        if (source%keep(k) .or. source%stmts%s(k)%kind /= SK_CODE) cycle
        col = 0
        do t = source%stmts%s(k)%first, source%stmts%s(k)%last
            associate (tk => source%tokens%tok(t))
                if (tk%kind == TK_NEWLINE) then
                    col = 0
                    cycle
                end if
                if (t == source%stmts%s(k)%first) col = start_column(t)
                if (tk%kind == TK_COMMENT .or. tk%kind == TK_PREPROC) cycle
                ! a token continued on later lines (a string) is measured line by line
                line = tk%line
                p = tk%first
                do
                    q = index(src(p:tk%last), achar(10))
                    if (q == 0) exit
                    q = p + q - 1                     ! the line feed
                    if (q > p) then
                        if (src(q - 1:q - 1) == achar(13)) q = q - 1
                    end if
                    call check_width(tk, col + utf8_width(src(p:q - 1)), line)
                    col = 0
                    line = line + 1
                    p = p + index(src(p:tk%last), achar(10))
                end do
                col = col + utf8_width(src(p:tk%last))
                call check_width(tk, col, line)
            end associate
        end do
    end do

contains

    subroutine check_width(tk, width, line)
        !! Record `line` if it holds code token `tk` and its code is `width` characters wide, too long.
        type(token_t), intent(in) :: tk   !! the token
        integer, intent(in) :: width      !! width of the line up to the end of `tk` on it
        integer, intent(in) :: line       !! line number
        if (is_code_kind(tk%kind) .and. width > line_length .and. line /= last_line) then
            lines = [lines, line]
            last_line = line
        end if
    end subroutine check_width

    integer function start_column(t)
        !! Column of token `t`: the width of the text before it on its line.
        integer, intent(in) :: t   !! index into the token list
        integer :: i
        i = scan(src(1:source%tokens%tok(t)%first - 1), achar(10)//achar(13), back=.true.)
        start_column = utf8_width(src(i + 1:source%tokens%tok(t)%first - 1))
    end function start_column

end function long_code_lines

end module ffmt_linebreak


!>>>>> ././src/ffmt_relational.f90
module ffmt_relational
!! Spelling of the relational operators: `==` / `.eq.`, `/=` / `.ne.`, `<` / `.lt.`, `<=` / `.le.`,
!! `>` / `.gt.` and `>=` / `.ge.`. The two spellings are the same operator, also in `operator(...)`.
!!
!! CUDA kernel launches (`<<< ... >>>`) and code protected by formatter directives are not changed.
use ffmt_tokens
use ffmt_statements
use ffmt_source, only: source_t, parse_source
use ffmt_strings, only: string_builder_t
use ffmt_options, only: REL_KEEP, REL_SYMBOLS
implicit none
private

public :: convert_relational, canonical_relational

character(len=*), parameter :: SYMBOLS(6) = [character(len=2) :: '==', '/=', '<', '<=', '>', '>=']
character(len=*), parameter :: LETTERS(6) = [character(len=4) :: '.eq.', '.ne.', '.lt.', '.le.', '.gt.', '.ge.']

contains

function convert_relational(src, style) result(out)
    !! `src` with every relational operator written in `style` (`REL_SYMBOLS` or `REL_LETTERS`).
    character(len=*), intent(in) :: src   !! source text
    integer, intent(in) :: style          !! one of the REL_* constants; `REL_KEEP` returns `src`
    character(len=:), allocatable :: out

    type(source_t) :: source
    type(string_builder_t) :: buf
    integer :: k, j, r

    if (style == REL_KEEP) then
        out = src
        return
    end if
    call parse_source(src, source)
    do k = 1, source%stmts%n
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            r = 0
            if (.not. source%keep(k) .and. source%stmts%s(k)%kind == SK_CODE) r = operator_index(j)
            if (r == 0) then
                call buf%append(token_text(src, source%tokens%tok(j)))
            else if (style == REL_SYMBOLS) then
                call buf%append(trim(SYMBOLS(r)))
            else
                call buf%append(trim(LETTERS(r)))
            end if
        end do
    end do
    out = buf%get()

contains

    integer function operator_index(j)
        !! Index of token `j` in the operator tables, or 0 if it is not a relational operator.
        integer, intent(in) :: j   !! index into the token list
        character(len=:), allocatable :: t
        operator_index = 0
        if (source%tokens%tok(j)%kind /= TK_OP .and. source%tokens%tok(j)%kind /= TK_DOTOP) return
        t = token_lower(src, source%tokens%tok(j))
        if (source%tokens%tok(j)%kind == TK_OP) then
            operator_index = findloc(SYMBOLS, t, dim=1)
            if (is_chevron(src, source%tokens, j)) operator_index = 0
        else
            operator_index = findloc(LETTERS, t, dim=1)
        end if
    end function operator_index

end function convert_relational

pure function canonical_relational(text) result(t)
    !! The symbol spelling of a lowercase relational operator; other text is returned unchanged.
    character(len=*), intent(in) :: text   !! operator text, in lowercase
    character(len=:), allocatable :: t
    integer :: r
    r = findloc(LETTERS, text, dim=1)
    if (r > 0) then
        t = trim(SYMBOLS(r))
    else
        t = text
    end if
end function canonical_relational

end module ffmt_relational


!>>>>> ././src/ffmt_structure.f90
module ffmt_structure
!! Block structure of a file: the nesting depth of every statement, from a stack of open
!! constructs, plus diagnostics for statements that do not fit (an `end` with nothing to close,
!! an `else` outside an `if`, a construct still open at the end of the file, ...).
use ffmt_tokens
use ffmt_statements
use ffmt_constructs
use ffmt_source, only: source_t
use ffmt_strings, only: to_s
implicit none
private

public :: analyze_structure

type, public :: diagnostic_t
    !! A problem with the block structure, such as an `end` that matches no open construct.
    integer :: line = 0                       !! source line of the statement concerned
    character(len=:), allocatable :: message  !! what is wrong, e.g. `'if is not closed'`
end type diagnostic_t

type, public :: structure_t
    !! The block structure of a source: for each item (see [[ffmt_statements]]) what it is and how
    !! deeply it is nested.
    type(stmt_info_t), allocatable :: info(:)   !! classification of each item (`ROLE_NONE` for non-code)
    integer, allocatable :: depth(:)            !! nesting depth of each item: the constructs around it
    integer, allocatable :: units(:)            !! how many of those are program units (module, submodule
                                                !! or main program)
    integer, allocatable :: procedures(:)       !! how many are procedure definitions (subroutine,
                                                !! function or separate module procedure, but not
                                                !! interface bodies)
    integer, allocatable :: fypp(:)             !! fypp blocks (`#:if`, `#:for`, `#:def`, ...) around each
                                                !! item; a directive line is outside its own block
end type structure_t

type :: scope_t
    !! An open construct on the scope stack.
    integer :: construct = C_NONE   !! one of the `C_*` constants
    integer :: do_label = 0         !! for `do 10 ...`: the label of the statement that ends the loop, else 0
    integer :: line = 0             !! source line of the opening statement
end type scope_t

type :: scope_stack_t
    !! The open constructs, outermost first.
    type(scope_t), allocatable :: s(:)   !! storage; only `s(1:top)` is in use
    integer :: top = 0                   !! number of open constructs
contains
    procedure :: push => stack_push          !! open a construct
    procedure :: restore => stack_restore    !! replace the open constructs by saved ones
    procedure :: snapshot => stack_snapshot  !! the open constructs, to save
end type scope_stack_t

type :: saved_stack_t
    !! Scope stacks for one preprocessor conditional: as it was at the `#if` (restored at each
    !! `#elif` / `#else`) and as it was at the end of the first branch (used after `#endif`).
    type(scope_t), allocatable :: at_if(:)        !! the stack at the `#if`
    type(scope_t), allocatable :: after_first(:)  !! the stack at the end of the first branch; unallocated until then
end type saved_stack_t

! preprocessor conditionals
integer, parameter :: PP_OTHER = 0, PP_IF = 1, PP_ELSE = 2, PP_ENDIF = 3

! fypp block directives
integer, parameter :: FY_OTHER = 0   !! not a block directive (`#:set`, `$:`, cpp, ...)
integer, parameter :: FY_OPEN = 1    !! `#:if`, `#:for`, `#:def`, `#:block`, `#:call`, `#:mute`
integer, parameter :: FY_MIDDLE = 2  !! `#:elif`, `#:else`, `#:nextarg`
integer, parameter :: FY_CLOSE = 3   !! `#:endif`, `#:endfor`, ...

contains

subroutine analyze_structure(source, views, st, diags)
    !! Classify every statement and compute its depth: the number of constructs enclosing it, and
    !! how many of those are program units and procedures, so callers can choose not to indent
    !! their bodies.
    !! An opening statement is at the depth outside its construct, the statements inside are one
    !! deeper, and middle (`else`, `contains`, ...) and closing statements are at the opening depth.
    !! Non-code items get the depth of the code around them.
    !!
    !! Branches of preprocessor conditionals (cpp `#if`/`#ifdef`/`#elif`/`#else`/`#endif` and
    !! fypp `#:if`/`#:elif`/`#:else`/`#:endif`) are treated as alternatives: each branch starts
    !! from the structure at the `#if`, and the structure at the end of the first branch carries
    !! on after the `#endif`. That keeps code such as `#ifdef X` / `do` / `#endif` ... `#ifdef X` /
    !! `end do` / `#endif` balanced.
    type(source_t), intent(in) :: source                          !! the source
    type(statement_view_t), intent(in) :: views(:)                !! the view of each item (see [[statement_views]])
    type(structure_t), intent(out) :: st                          !! the structure
    type(diagnostic_t), allocatable, intent(out) :: diags(:)      !! structure problems found, in the order found

    type(scope_stack_t) :: stack
    type(saved_stack_t), allocatable :: saved(:)
    type(stmt_info_t), allocatable :: info(:)
    integer, allocatable :: depth(:), nunits(:), nprocs(:)
    integer :: k, line, nsaved
    logical :: in_interface

    associate (n => source%stmts%n)
        allocate (info(n), depth(n), nunits(n), nprocs(n), diags(0), saved(0))
    end associate
    nsaved = 0
    do k = 1, source%stmts%n
        depth(k) = stack%top
        if (source%stmts%s(k)%kind == SK_PREPROC) call preprocessor(k)
        if (source%stmts%s(k)%kind /= SK_CODE) then
            call count_units(k)
            cycle
        end if
        line = source%tokens%tok(source%stmts%s(k)%first)%line
        in_interface = .false.
        if (stack%top > 0) in_interface = stack%s(stack%top)%construct == C_INTERFACE
        info(k) = classify_statement(source%text, source%tokens, views(k), in_interface)

        select case (info(k)%role)
        case (ROLE_OPEN)
            call stack%push(scope_t(info(k)%construct, info(k)%do_label, line))
        case (ROLE_MIDDLE)
            if (stack%top == 0 .and. info(k)%construct == C_CONTAINS) then
                ! a main program without a `program` statement
                call stack%push(scope_t(C_PROGRAM, 0, line))
            else if (stack%top == 0) then
                call diagnose(line, construct_label(info(k)%construct)//' statement outside any construct')
            else if (.not. middle_fits(info(k)%construct, stack%s(stack%top)%construct)) then
                call diagnose(line, construct_label(info(k)%construct)//' statement inside '// &
                              opened(stack%s(stack%top)))
            else
                depth(k) = stack%top - 1
            end if
        case (ROLE_CLOSE)
            call close_scope(info(k)%construct, line)
            depth(k) = stack%top
        end select
        call count_units(k)

        ! the labeled statement that ends `do 10 ...` loops is part of the innermost loop body
        if (info(k)%label > 0) then
            do while (stack%top > 0)
                if (stack%s(stack%top)%construct /= C_DO .or. stack%s(stack%top)%do_label /= info(k)%label) exit
                stack%top = stack%top - 1
            end do
        end if
    end do

    do k = stack%top, 1, -1
        call diagnose(stack%s(k)%line, construct_label(stack%s(k)%construct)//' is not closed')
    end do
    call move_alloc(info, st%info)
    call move_alloc(depth, st%depth)
    call move_alloc(nunits, st%units)
    call move_alloc(nprocs, st%procedures)
    st%fypp = fypp_depths(source)

contains

    subroutine count_units(k)
        !! Record how many of the scopes enclosing item `k` are program units, and procedures.
        integer, intent(in) :: k   !! index into `stmts`
        integer :: j
        nunits(k) = 0
        nprocs(k) = 0
        do j = 1, depth(k)
            select case (stack%s(j)%construct)
            case (C_MODULE, C_SUBMODULE, C_PROGRAM)
                nunits(k) = nunits(k) + 1
            case (C_SUBROUTINE, C_FUNCTION, C_PROCEDURE)
                ! interface bodies are declarations, not procedure definitions
                if (j == 1) then
                    nprocs(k) = nprocs(k) + 1
                else if (stack%s(j - 1)%construct /= C_INTERFACE) then
                    nprocs(k) = nprocs(k) + 1
                end if
            end select
        end do
    end subroutine count_units

    subroutine preprocessor(k)
        !! Save or restore the scope stack at the preprocessor conditionals in item `k`, and set
        !! the depth of the item to that of the restored stack.
        integer, intent(in) :: k   !! index into `stmts` of a preprocessor item
        integer :: j
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            if (source%tokens%tok(j)%kind /= TK_PREPROC) cycle
            select case (conditional_kind(token_lower(source%text, source%tokens%tok(j))))
            case (PP_IF)
                nsaved = nsaved + 1
                if (nsaved > size(saved)) saved = [saved, saved_stack_t()]
                saved(nsaved)%at_if = stack%snapshot()
                if (allocated(saved(nsaved)%after_first)) deallocate (saved(nsaved)%after_first)
            case (PP_ELSE)
                if (nsaved > 0) then
                    if (.not. allocated(saved(nsaved)%after_first)) saved(nsaved)%after_first = stack%snapshot()
                    call stack%restore(saved(nsaved)%at_if)
                    depth(k) = stack%top
                end if
            case (PP_ENDIF)
                if (nsaved > 0) then
                    if (allocated(saved(nsaved)%after_first)) call stack%restore(saved(nsaved)%after_first)
                    depth(k) = stack%top
                    nsaved = nsaved - 1
                end if
            end select
        end do
    end subroutine preprocessor

    subroutine close_scope(construct, line)
        !! Pop the construct closed by an `end` statement, with a diagnostic if it does not match
        !! the innermost open construct. If an outer one matches, everything down to it is closed.
        integer, intent(in) :: construct   !! construct named by the `end` (`C_ANY` for a bare `end`)
        integer, intent(in) :: line        !! source line of the `end` statement
        integer :: j

        if (stack%top == 0) then
            ! `end` or `end program` may close a main program without a `program` statement
            if (construct /= C_ANY .and. construct /= C_PROGRAM) &
                call diagnose(line, 'end '//construct_label(construct)//' without an open construct')
            return
        end if
        if (closes(construct, stack%s(stack%top)%construct)) then
            stack%top = stack%top - 1
            return
        end if
        ! recover from a missing `end`: close down to a matching construct if there is one
        do j = stack%top - 1, 1, -1
            if (closes(construct, stack%s(j)%construct)) then
                call diagnose(line, 'end '//construct_label(construct)//' closes '// &
                              opened(stack%s(stack%top)))
                stack%top = j - 1
                return
            end if
        end do
        call diagnose(line, 'end '//construct_label(construct)//' does not match '// &
                      opened(stack%s(stack%top)))
    end subroutine close_scope

    function opened(sc) result(text)
        !! `sc` for messages: `do (opened on line 12)`.
        type(scope_t), intent(in) :: sc   !! an open construct
        character(len=:), allocatable :: text
        text = construct_label(sc%construct)//' (opened on line '//to_s(sc%line)//')'
    end function opened

    subroutine diagnose(line, message)
        !! Add a diagnostic to `diags`.
        integer, intent(in) :: line             !! source line
        character(len=*), intent(in) :: message !! text of the warning
        diags = [diags, diagnostic_t(line, message)]
    end subroutine diagnose

end subroutine analyze_structure

pure subroutine stack_push(self, sc)
    !! Open construct `sc`, growing the storage as needed.
    class(scope_stack_t), intent(inout) :: self  !! the stack
    type(scope_t), intent(in) :: sc               !! the construct to open
    type(scope_t), allocatable :: tmp(:)
    if (.not. allocated(self%s)) allocate (self%s(16))
    if (self%top == size(self%s)) then
        allocate (tmp(2*self%top))
        tmp(1:self%top) = self%s(1:self%top)
        call move_alloc(tmp, self%s)
    end if
    self%top = self%top + 1
    self%s(self%top) = sc
end subroutine stack_push

pure subroutine stack_restore(self, scopes)
    !! Replace the open constructs by `scopes` (outermost first).
    class(scope_stack_t), intent(inout) :: self  !! the stack
    type(scope_t), intent(in) :: scopes(:)        !! a saved stack (see [[stack_snapshot]])
    integer :: j
    self%top = 0
    do j = 1, size(scopes)
        call self%push(scopes(j))
    end do
end subroutine stack_restore

pure function stack_snapshot(self) result(scopes)
    !! The open constructs, outermost first.
    class(scope_stack_t), intent(in) :: self  !! the stack
    type(scope_t), allocatable :: scopes(:)
    if (allocated(self%s)) then
        scopes = self%s(1:self%top)
    else
        allocate (scopes(0))
    end if
end function stack_snapshot

function fypp_depths(source) result(fypp)
    !! The number of fypp blocks around each item of `source`. The directives are counted wherever
    !! they are, also inside a continued statement; a directive line itself is at the level outside
    !! its block, like `else` and `end` statements.
    type(source_t), intent(in) :: source   !! the source
    integer, allocatable :: fypp(:)
    integer :: k, j, d

    allocate (fypp(source%stmts%n))
    d = 0
    do k = 1, source%stmts%n
        fypp(k) = d
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            if (source%tokens%tok(j)%kind /= TK_PREPROC) cycle
            select case (fypp_kind(token_lower(source%text, source%tokens%tok(j))))
            case (FY_OPEN)
                d = d + 1
            case (FY_MIDDLE)
                if (source%stmts%s(k)%kind == SK_PREPROC) fypp(k) = max(0, d - 1)
            case (FY_CLOSE)
                d = max(0, d - 1)
                if (source%stmts%s(k)%kind == SK_PREPROC) fypp(k) = d
            end select
        end do
    end do
end function fypp_depths

pure integer function fypp_kind(text) result(kind)
    !! Kind of a preprocessor line (lowercase text) as a fypp block directive: one of the FY_*
    !! constants.
    character(len=*), intent(in) :: text   !! text of the preprocessor token, in lowercase
    integer :: i, j

    kind = FY_OTHER
    if (len(text) < 3) return
    if (text(1:2) /= '#:') return
    i = 3
    do while (i <= len(text))
        if (text(i:i) /= ' ' .and. text(i:i) /= achar(9)) exit
        i = i + 1
    end do
    j = i
    do while (j <= len(text))
        if (text(j:j) < 'a' .or. text(j:j) > 'z') exit
        j = j + 1
    end do
    select case (text(i:j - 1))
    case ('if', 'for', 'def', 'block', 'call', 'mute')
        kind = FY_OPEN
    case ('elif', 'else', 'nextarg')
        kind = FY_MIDDLE
    case ('endif', 'endfor', 'enddef', 'endblock', 'endcall', 'endmute')
        kind = FY_CLOSE
    end select
end function fypp_kind

pure integer function conditional_kind(text) result(kind)
    !! Kind of a preprocessor line (lowercase text): `PP_IF`, `PP_ELSE` (`#elif`, `#else`), `PP_ENDIF`
    !! or `PP_OTHER`.
    character(len=*), intent(in) :: text   !! text of the preprocessor token, in lowercase
    character(len=:), allocatable :: directive
    integer :: i, j

    kind = PP_OTHER
    i = 2                                   ! after '#'
    if (len(text) < 2 .or. text(1:1) /= '#') return
    if (text(2:2) == ':') i = 3             ! fypp
    do while (i <= len(text))
        if (text(i:i) /= ' ' .and. text(i:i) /= achar(9)) exit
        i = i + 1
    end do
    j = i
    do while (j <= len(text))
        if (text(j:j) < 'a' .or. text(j:j) > 'z') exit
        j = j + 1
    end do
    directive = text(i:j - 1)
    select case (directive)
    case ('if', 'ifdef', 'ifndef')
        kind = PP_IF
    case ('elif', 'else')
        kind = PP_ELSE
    case ('endif')
        kind = PP_ENDIF
    end select
end function conditional_kind

pure logical function closes(closing, open)
    !! True if an `end` of kind `closing` ends the open construct `open`.
    integer, intent(in) :: closing   !! construct named by the `end` (`C_ANY` for a bare `end`)
    integer, intent(in) :: open      !! construct that is open
    closes = closing == open .or. closing == C_ANY
    ! `end function` / `end subroutine` may close a separate module procedure, and vice versa
    if (closing == C_PROCEDURE) closes = closes .or. open == C_FUNCTION .or. open == C_SUBROUTINE
    if (open == C_PROCEDURE) closes = closes .or. closing == C_FUNCTION .or. closing == C_SUBROUTINE
end function closes

pure logical function middle_fits(middle, open)
    !! True if a middle statement of construct `middle` (`else` of `C_IF`, `contains`, ...) may
    !! appear directly inside the open construct `open`.
    integer, intent(in) :: middle   !! construct of the middle statement
    integer, intent(in) :: open     !! construct that is open
    select case (middle)
    case (C_CONTAINS)
        select case (open)
        case (C_PROGRAM, C_MODULE, C_SUBMODULE, C_SUBROUTINE, C_FUNCTION, C_PROCEDURE, C_TYPE)
            middle_fits = .true.
        case default
            middle_fits = .false.
        end select
    case default
        middle_fits = middle == open
    end select
end function middle_fits

end module ffmt_structure


!>>>>> ././src/ffmt_use_sort.f90
module ffmt_use_sort
!! Sorting `use` statements, and the names in their `only:` lists, alphabetically ignoring case.
!!
!! A group of `use` statements is a run of them with nothing between them: a blank line, comment,
!! preprocessor line or any other statement ends the group, so statements in different `#if`
!! branches never mix. A statement moves with its continuation lines and trailing comment.
!! Statements that share a line with another (`;`) or that are protected by formatter directives
!! stay where they are and end the group. Statements of one group that use the same module keep
!! their order.
!!
!! The names of an `only:` list are sorted by their local name (`c => x` sorts as `c`). A list that
!! is not sorted yet is rewritten: on one line, for line splitting to break again, or broken at
!! commas to fit a given width, with its continuation lines starting below the first name. A
!! statement with a comment or preprocessor line before its last line is left alone.
use ffmt_tokens
use ffmt_statements
use ffmt_source, only: source_t, parse_source
use ffmt_strings, only: string_t, string_builder_t
implicit none
private

public :: sort_use, is_use_statement, only_items, sorted_order

contains

function sort_use(src, statements, only_lists, width) result(out)
    !! `src` with the `use` statements of each group sorted by module name (if `statements`) and
    !! the names of `only:` lists sorted (if `only_lists`).
    character(len=*), intent(in) :: src          !! source text
    logical, intent(in) :: statements            !! sort the `use` statements of each group
    logical, intent(in) :: only_lists            !! sort the names of `only:` lists
    integer, intent(in) :: width   !! line length to fit a rewritten `only:` list into, or 0 to write it on one line
    character(len=:), allocatable :: out

    type(source_t) :: source
    type(statement_view_t) :: v
    logical, allocatable :: movable(:)
    type(string_t), allocatable :: text(:), keys(:)
    integer, allocatable :: order(:)
    type(string_builder_t) :: buf
    integer :: k, m, j

    if (.not. (statements .or. only_lists)) then
        out = src
        return
    end if
    call parse_source(src, source)
    associate (tokens => source%tokens, stmts => source%stmts)
        allocate (text(stmts%n), keys(stmts%n), movable(stmts%n))
        do k = 1, stmts%n
            text(k)%s = src(tokens%tok(stmts%s(k)%first)%first:tokens%tok(stmts%s(k)%last)%last)
            keys(k)%s = ''
            movable(k) = .false.
            if (stmts%s(k)%kind /= SK_CODE .or. source%keep(k)) cycle
            call set_view(src, tokens, stmts%s(k), v)
            if (.not. is_use_statement(v)) cycle
            if (only_lists) call sort_only_list(src, tokens, stmts%s(k), v, width, text(k)%s)
            keys(k)%s = module_key(v)
            movable(k) = statements .and. whole_lines(tokens, stmts%s(k))
        end do
    end associate

    k = 1
    do while (k <= size(movable))
        if (.not. movable(k)) then
            call buf%append(text(k)%s)
            k = k + 1
            cycle
        end if
        m = k
        do while (m < size(movable))
            if (.not. movable(m + 1)) exit
            m = m + 1
        end do
        order = sorted_order(keys(k:m))
        do j = 1, size(order)
            call buf%append(text(k + order(j) - 1)%s)
        end do
        k = m + 1
    end do
    out = buf%get()
end function sort_use

pure logical function is_use_statement(v)
    !! True if statement `v` is a `use` statement.
    type(statement_view_t), intent(in) :: v   !! the statement
    is_use_statement = .false.
    if (v%n < 2) return
    if (v%kinds(1) /= TK_NAME .or. .not. v%is(1, 'use')) return
    select case (v%kinds(2))
    case (TK_NAME)
        is_use_statement = .true.
    case (TK_OP)
        is_use_statement = v%is(2, ',') .or. v%is(2, '::')
    end select
end function is_use_statement

function only_items(v) result(starts)
    !! Positions in `v%code` where the items of a `use` statement's `only:` list start. Each item
    !! ends before the comma in front of the next one, the last at the end of the statement. Empty
    !! if there is no `only:` list, the list is empty, or it has an empty item.
    type(statement_view_t), intent(in) :: v   !! the `use` statement
    integer, allocatable :: starts(:)
    integer :: p, q, n, depth

    allocate (starts(0))
    n = v%n
    do p = 3, n - 1
        if (is_op(p - 1, ',') .and. is_op(p + 1, ':') .and. v%is(p, 'only')) exit
    end do
    if (p >= n .or. p + 2 > n) return
    starts = [p + 2]
    depth = 0
    do q = p + 2, n
        if (v%kinds(q) /= TK_OP) cycle
        if (v%is(q, '(') .or. v%is(q, '[')) then
            depth = depth + 1
        else if (v%is(q, ')') .or. v%is(q, ']')) then
            depth = depth - 1
        else if (v%is(q, ',') .and. depth == 0) then
            if (q == n .or. q == starts(size(starts))) then   ! an empty item
                deallocate (starts)
                allocate (starts(0))
                return
            end if
            starts = [starts, q + 1]
        end if
    end do

contains

    logical function is_op(j, text)
        !! True if code token `j` is the operator `text`.
        integer, intent(in) :: j                 !! index into `v%code(:)`
        character(len=*), intent(in) :: text     !! the operator, e.g. `,`
        is_op = v%kinds(j) == TK_OP .and. v%is(j, text)
    end function is_op

end function only_items

function sorted_order(keys) result(order)
    !! The order in which `keys` are sorted (a stable sort, comparing in ASCII order).
    type(string_t), intent(in) :: keys(:)   !! the sort keys
    integer, allocatable :: order(:)
    integer :: i, j, t

    order = [(i, i=1, size(keys))]
    do i = 2, size(keys)
        t = order(i)
        j = i - 1
        do while (j >= 1)
            if (.not. llt(keys(t)%s, keys(order(j))%s)) exit
            order(j + 1) = order(j)
            j = j - 1
        end do
        order(j + 1) = t
    end do
end function sorted_order

subroutine sort_only_list(src, tokens, stmt, v, width, text)
    !! Replace `text`, the text of `stmt`, by the statement with its `only:` list sorted, unless the
    !! list is sorted already or the statement cannot be rewritten safely. With `width > 0`, the
    !! list is broken after commas so that its lines fit in `width` columns where possible.
    character(len=*), intent(in) :: src                      !! source text
    type(token_list_t), intent(in) :: tokens                 !! tokens of `src`
    type(statement_t), intent(in) :: stmt                    !! the `use` statement
    type(statement_view_t), intent(in) :: v                  !! the view of `stmt`
    integer, intent(in) :: width                             !! line length to fit the list into, or 0
    character(len=:), allocatable, intent(inout) :: text     !! text of `stmt`, from its first to its last token

    integer, allocatable :: starts(:), ends(:), order(:)
    type(string_t), allocatable :: keys(:)
    type(string_builder_t) :: buf
    character(len=:), allocatable :: sep, prefix, item, nl
    logical :: commented
    integer :: j, n, ns, column, used

    starts = only_items(v)
    ns = size(starts)
    if (ns < 2) return
    n = v%n

    ! comments are only allowed on the last line; no preprocessor lines or tokens split across lines
    commented = .false.
    do j = stmt%first, stmt%last
        select case (tokens%tok(j)%kind)
        case (TK_COMMENT)
            commented = .true.
        case (TK_NEWLINE)
            if (commented .and. j /= stmt%last) return
        case (TK_PREPROC)
            return
        end select
    end do
    if (any(tokens%tok(v%code(starts(1):n))%glued)) return

    ends = [starts(2:) - 2, n]
    allocate (keys(ns))
    do j = 1, ns
        keys(j)%s = local_name(starts(j), ends(j))
    end do
    order = sorted_order(keys)
    if (all(order == [(j, j=1, ns)])) return

    prefix = src(tokens%tok(stmt%first)%first:tokens%tok(v%code(starts(1) - 1))%last)
    if (separated(starts(1))) prefix = prefix//' '
    call buf%append(prefix)
    sep = ','
    if (separated(starts(2))) sep = ', '
    nl = achar(10)
    if (tokens%tok(stmt%last)%kind == TK_NEWLINE) nl = token_text(src, tokens%tok(stmt%last))
    ! the column of the first name, where the continuation lines start too
    column = len(prefix) - max(index(prefix, achar(10), back=.true.), index(prefix, achar(13), back=.true.))
    used = column
    do j = 1, ns
        item = joined(starts(order(j)), ends(order(j)))
        if (j > 1) then
            ! room for the item and a `, &` after it
            if (width > 0 .and. used + len(sep) + len(item) + 3 > width) then
                call buf%append(', &'//nl//repeat(' ', column))
                used = column
            else
                call buf%append(sep)
                used = used + len(sep)
            end if
        end if
        call buf%append(item)
        used = used + len(item)
    end do
    call buf%append(src(tokens%tok(v%code(n))%last + 1:tokens%tok(stmt%last)%last))
    text = buf%get()

contains

    logical function separated(p)
        !! True if anything (blanks, a line break) comes between code tokens `p - 1` and `p`.
        integer, intent(in) :: p   !! index into `v%code(:)`
        separated = v%code(p) /= v%code(p - 1) + 1
    end function separated

    function joined(first, last) result(s)
        !! The text of code tokens `first:last`, with a blank wherever they were separated.
        integer, intent(in) :: first, last   !! range of indices into `v%code(:)`
        character(len=:), allocatable :: s
        integer :: p
        s = token_text(src, tokens%tok(v%code(first)))
        do p = first + 1, last
            if (separated(p)) s = s//' '
            s = s//token_text(src, tokens%tok(v%code(p)))
        end do
    end function joined

    function local_name(first, last) result(s)
        !! Sort key of an item: its tokens before `=>`, in lowercase.
        integer, intent(in) :: first, last   !! range of indices into `v%code(:)` of the item
        character(len=:), allocatable :: s
        integer :: p
        s = ''
        do p = first, last
            if (v%kinds(p) == TK_OP .and. v%is(p, '=>')) exit
            s = s//v%word(p)
        end do
    end function local_name

end subroutine sort_only_list

function module_key(v) result(key)
    !! Sort key of a `use` statement: the module name in lowercase.
    type(statement_view_t), intent(in) :: v   !! the `use` statement
    character(len=:), allocatable :: key
    integer :: p

    p = 2
    if (v%is(2, '::')) then
        p = 3
    else if (v%is(2, ',')) then
        ! `use, intrinsic :: name`
        do p = 3, v%n
            if (v%is(p - 1, '::')) exit
        end do
    end if
    key = ''
    do while (p <= v%n)
        if (v%is(p, ',')) exit
        key = key//v%word(p)
        p = p + 1
    end do
end function module_key

pure logical function whole_lines(tokens, stmt)
    !! True if `stmt` takes up whole lines on its own, ending with a line break, so that it can
    !! be moved.
    type(token_list_t), intent(in) :: tokens   !! tokens of the source
    type(statement_t), intent(in) :: stmt      !! the statement, a range of `tokens`
    whole_lines = .false.
    if (tokens%tok(stmt%last)%kind /= TK_NEWLINE) return
    if (stmt%first > 1) then
        if (tokens%tok(stmt%first - 1)%kind /= TK_NEWLINE) return
    end if
    whole_lines = .not. any(tokens%tok(stmt%first:stmt%last)%kind == TK_PREPROC .or. &
                            tokens%tok(stmt%first:stmt%last)%kind == TK_BOM)
end function whole_lines

end module ffmt_use_sort


!>>>>> ././src/ffmt_whitespace.f90
module ffmt_whitespace
!! Whitespace rules within lines: the blanks wanted between pairs of adjacent tokens.
!!
!! Each rule has a mode: `WS_ADD` makes sure there is at least one blank on each side (more are
!! kept, so column alignment survives), `WS_REMOVE` removes the blanks, and `WS_KEEP` leaves them as
!! they are. Rules only look at tokens on the same line; blanks are never added or removed next to a
!! line break, a continuation `&` or a comment.
!!
!! Statements where operators have other meanings are recognized: unary `+`/`-`, `*` in `real*8`,
!! `print *` and `(*)`, `/` in array constructors, and in `data`, `common`, `namelist`, `save` and
!! `equivalence` statements. `format` statements are left alone.
use ffmt_tokens
use ffmt_statements
use ffmt_options, only: whitespace_options_t, WS_ADD, WS_REMOVE
use ffmt_source, only: source_t
use ffmt_strings, only: utf8_width
implicit none
private

public :: compute_gaps

! gaps: the blanks wanted before a token
integer, parameter, public :: GAP_KEEP = -1   !! as in the input
integer, parameter, public :: GAP_MIN1 = -2   !! at least one blank

contains

function compute_gaps(source, views, ws, comment_spacing) result(gap)
    !! The gap wanted before each token (`GAP_KEEP`, `GAP_MIN1` or a number of blanks).
    type(source_t), intent(in) :: source                !! the source
    type(statement_view_t), intent(in) :: views(:)      !! the view of each item (see [[statement_views]])
    type(whitespace_options_t), intent(in) :: ws        !! modes of the whitespace rules
    integer, intent(in) :: comment_spacing              !! blanks before a trailing comment, or -1 to keep them
    integer, allocatable :: gap(:)
    integer :: k

    allocate (gap(source%tokens%n))
    gap = GAP_KEEP
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind == SK_CODE) call statement_gaps(source%text, source%tokens, views(k), ws, gap)
    end do
    if (comment_spacing >= 0) call comment_gaps(source, comment_spacing, gap)
end function compute_gaps

subroutine comment_gaps(source, spacing, gap)
    !! Ask for `spacing` blanks before each trailing comment (one after code or a continuation `&`),
    !! unless a trailing comment on the line before or after starts in the same column: comments
    !! lined up in a column are left as they are.
    type(source_t), intent(in) :: source   !! the source
    integer, intent(in) :: spacing         !! blanks wanted before a trailing comment
    integer, intent(inout) :: gap(:)       !! gap wanted before each token, set for the comments
    integer, allocatable :: tok(:), line(:), col(:)
    integer :: t, k, n, p

    allocate (tok(source%tokens%n), line(source%tokens%n), col(source%tokens%n))
    n = 0
    do t = 2, source%tokens%n
        if (source%tokens%tok(t)%kind /= TK_COMMENT) cycle
        p = t - 1
        if (source%tokens%tok(p)%kind == TK_SPACE .and. p > 1) p = p - 1
        if (.not. (is_code_kind(source%tokens%tok(p)%kind) .or. source%tokens%tok(p)%kind == TK_CONT)) cycle
        if (source%tokens%tok(p)%kind == TK_SPACE) cycle
        n = n + 1
        tok(n) = t
        line(n) = source%tokens%tok(t)%line
        col(n) = column(source%tokens%tok(t)%first)
    end do
    do k = 1, n
        if (k > 1) then
            if (line(k - 1) == line(k) - 1 .and. col(k - 1) == col(k)) cycle
        end if
        if (k < n) then
            if (line(k + 1) == line(k) + 1 .and. col(k + 1) == col(k)) cycle
        end if
        gap(tok(k)) = spacing
    end do

contains

    integer function column(i)
        !! Input column of byte `i`: the characters before it on its line.
        integer, intent(in) :: i   !! index into the source text
        integer :: j
        j = scan(source%text(1:i - 1), achar(10)//achar(13), back=.true.)
        column = utf8_width(source%text(j + 1:i - 1))
    end function column

end subroutine comment_gaps

subroutine statement_gaps(src, tokens, v, ws, gap)
    !! Set the gaps wanted before the tokens of one code statement.
    character(len=*), intent(in) :: src                 !! the source text
    type(token_list_t), intent(in) :: tokens            !! its tokens
    type(statement_view_t), intent(in) :: v             !! the statement
    type(whitespace_options_t), intent(in) :: ws        !! modes of the whitespace rules
    integer, intent(inout) :: gap(:)                    !! gap wanted before each token, set where a rule applies

    integer :: i
    logical :: slash_lists

    if (v%n == 0) return
    if (v%is(v%first, 'format')) return
    ! `data x /1/`, `common /blk/ a`, ...: slashes are delimiters, not division
    select case (v%word(v%first))
    case ('data', 'common', 'namelist', 'save', 'equivalence')
        slash_lists = .true.
    case default
        slash_lists = .false.
    end select

    do i = 1, v%n
        if (v%kinds(i) == TK_OP) then
            associate (t => v%low(v%lo(i):v%hi(i)))
                select case (t)
                case ('(')
                    if (is_control_paren(i)) call set_before(i, ws%control_paren)
                case ('=')
                    if (v%depth(i) == 0) then
                        call around(i, ws%assignment)
                    else
                        call around(i, ws%keyword_args)
                    end if
                case ('=>')
                    call around(i, ws%pointer)
                case ('::')
                    call around(i, ws%declarations)
                case ('==', '/=', '<=', '>=')
                    call around(i, ws%relational)
                case ('<', '>')
                    if (.not. is_chevron(src, tokens, v%code(i))) call around(i, ws%relational)
                case ('+', '-')
                    if (binary(i)) call around(i, ws%plus_minus)
                case ('*')
                    if (binary(i) .and. .not. type_star(i)) call around(i, ws%mult_div)
                case ('/')
                    if (binary(i) .and. .not. slash_lists) call around(i, ws%mult_div)
                case ('**')
                    if (binary(i)) call around(i, ws%power)
                case ('//')
                    if (binary(i) .and. .not. slash_lists) call around(i, ws%concat)
                case ('%')
                    call around(i, ws%percent)
                case ('?')
                    call around(i, ws%conditional)
                case (':')
                    if (conditional_colon(i)) then
                        call around(i, ws%conditional)
                    else if (v%depth(i) > 0) then
                        ! a slice `a(1 : n, :)`: only between the colon and its bounds
                        if (ends_operand(i - 1)) call set_before(i, ws%slice_colon)
                        if (starts_operand(i + 1)) call set_before(i + 1, ws%slice_colon)
                    end if
                case (',')
                    select case (ws%comma)
                    case (WS_ADD)
                        call set_gap(i, 0)
                        call set_gap(i + 1, GAP_MIN1)
                    case (WS_REMOVE)
                        call set_gap(i, 0)
                        call set_gap(i + 1, 0)
                    end select
                end select
            end associate
        else if (v%kinds(i) == TK_NAME) then
            ! `if (x)then`: the blank before a final `then` goes with the one before the condition
            if (i == v%n .and. v%is(i, 'then') .and. v%is(i - 1, ')')) call set_before(i, ws%control_paren)
        else if (v%kinds(i) == TK_DOTOP) then
            select case (v%word(i))
            case ('.eq.', '.ne.', '.lt.', '.le.', '.gt.', '.ge.')
                call around(i, ws%relational)
            case ('.and.', '.or.', '.eqv.', '.neqv.')
                call around(i, ws%logical)
            case ('.not.', '.true.', '.false.')
                continue
            case default
                ! a user-defined operator, unless it is a logical constant with a kind (`.true._lk`)
                if (index(v%word(i), '._') == 0 .and. binary(i)) call around(i, ws%defined_operators)
            end select
        end if
    end do

contains

    logical function adjacent(j)
        !! True if code tokens `j - 1` and `j` are on the same line with only blanks between them.
        integer, intent(in) :: j   !! index into `code(:)`
        integer :: k
        adjacent = .false.
        if (j < 2 .or. j > v%n) return
        if (tokens%tok(v%code(j))%glued) return
        do k = v%code(j - 1) + 1, v%code(j) - 1
            if (tokens%tok(k)%kind /= TK_SPACE) return
        end do
        adjacent = .true.
    end function adjacent

    subroutine set_gap(j, g)
        !! Ask for gap `g` before code token `j`, if it is on the same line as the previous token and the
        !! two tokens can be written next to each other without becoming different tokens.
        integer, intent(in) :: j   !! index into `code(:)`
        integer, intent(in) :: g   !! the gap: `GAP_MIN1` or a number of blanks
        if (.not. adjacent(j)) return
        if (g == 0 .and. .not. can_join(j)) return
        gap(v%code(j)) = g
    end subroutine set_gap

    subroutine set_before(j, mode)
        !! Apply rule mode `mode` to the blanks before code token `j`.
        integer, intent(in) :: j      !! index into `code(:)`
        integer, intent(in) :: mode   !! `WS_KEEP`, `WS_ADD` or `WS_REMOVE`
        select case (mode)
        case (WS_ADD)
            call set_gap(j, GAP_MIN1)
        case (WS_REMOVE)
            call set_gap(j, 0)
        end select
    end subroutine set_before

    subroutine around(j, mode)
        !! Apply `mode` on both sides of operator `j`.
        integer, intent(in) :: j      !! index into `code(:)`
        integer, intent(in) :: mode   !! `WS_KEEP`, `WS_ADD` or `WS_REMOVE`
        call set_before(j, mode)
        call set_before(j + 1, mode)
    end subroutine around

    logical function can_join(j)
        !! True if tokens `j - 1` and `j` keep their meaning when written without a blank between them.
        integer, intent(in) :: j   !! index into `code(:)`
        integer :: ka, kb
        ka = v%kinds(j - 1)
        kb = v%kinds(j)
        can_join = .true.
        if (is_word(ka) .and. is_word(kb)) can_join = .false.           ! `a b`, `1 e5`
        if (ka == TK_OP .and. kb == TK_OP) then                          ! `* *` would become `**`
            select case (v%word(j - 1)//v%word(j))
            case ('**', '//', '==', '/=', '<=', '>=', '=>', '::', '(/', '/)')
                can_join = .false.
            end select
        end if
        if ((ka == TK_INT .or. ka == TK_REAL) .and. v%low(v%lo(j):v%lo(j)) == '.') can_join = .false.   ! `1 .5`
        if (ka == TK_DOTOP .and. kb == TK_DOTOP) can_join = .false.
    end function can_join

    logical function binary(j)
        !! True if operator `j` has an operand on both sides (not unary `-x` or `print *`).
        integer, intent(in) :: j   !! index into `code(:)`
        binary = ends_operand(j - 1) .and. starts_operand(j + 1)
    end function binary

    logical function ends_operand(j)
        !! True if code token `j` can end an operand: a name, literal, inline fypp, logical constant,
        !! `)` or `]`, but not a statement label.
        integer, intent(in) :: j   !! index into `code(:)`
        ends_operand = .false.
        select case (v%kind_of(j))
        case (TK_NAME, TK_INT, TK_REAL, TK_STRING, TK_BOZ, TK_FYPP)
            ends_operand = j > v%first .or. v%kinds(j) /= TK_INT   ! not a statement label
        case (TK_DOTOP)
            ends_operand = v%is(j, '.true.') .or. v%is(j, '.false.') .or. index(v%word(j), '._') > 0
        case (TK_OP)
            ends_operand = v%is(j, ')') .or. v%is(j, ']')
        end select
    end function ends_operand

    logical function starts_operand(j)
        !! True if code token `j` can start an operand: a name, literal, inline fypp, dot operator,
        !! `(`, `[` or a sign.
        integer, intent(in) :: j   !! index into `code(:)`
        starts_operand = .false.
        select case (v%kind_of(j))
        case (TK_NAME, TK_INT, TK_REAL, TK_STRING, TK_BOZ, TK_FYPP, TK_DOTOP)
            starts_operand = .true.
        case (TK_OP)
            starts_operand = v%is(j, '(') .or. v%is(j, '[') .or. v%is(j, '+') .or. v%is(j, '-')
        end select
    end function starts_operand

    logical function type_star(j)
        !! True for the `*` of a length or kind selector: `real*8`, `character*(*)`, `character*10`.
        integer, intent(in) :: j   !! index into `code(:)`
        select case (v%word(j - 1))
        case ('real', 'integer', 'complex', 'logical', 'character', 'precision')
            type_star = .true.
        case default
            type_star = .false.
        end select
    end function type_star

    logical function conditional_colon(j)
        !! True if the `:` at `j` belongs to a conditional expression: a `?` comes before it at the same
        !! bracket depth, inside the same brackets.
        integer, intent(in) :: j   !! index into `v%code(:)` of a `:`
        integer :: k
        conditional_colon = .false.
        if (v%depth(j) == 0) return
        do k = j - 1, 1, -1
            if (v%depth(k) < v%depth(j)) return          ! the opening bracket
            if (v%depth(k) == v%depth(j) .and. v%is(k, '?')) then
                conditional_colon = .true.
                return
            end if
        end do
    end function conditional_colon

    logical function is_control_paren(j)
        !! True if `(` at `j` follows a control keyword: `if (`, `else if (`, `do while (`,
        !! `select case (`, `where (`, `forall (`, `case (`, `rank (`, `do concurrent (`.
        integer, intent(in) :: j   !! index into `code(:)`
        is_control_paren = .false.
        if (j < 2) return
        select case (v%word(j - 1))
        case ('if', 'elseif')
            is_control_paren = j - 1 == v%first .or. v%is(j - 2, 'else') .or. v%is(j - 2, ':')
        case ('while', 'concurrent')
            is_control_paren = v%is(j - 2, 'do')
        case ('case', 'rank')
            is_control_paren = v%is(j - 2, 'select') .or. j - 1 == v%first
        case ('type')
            is_control_paren = v%is(j - 2, 'select')
        case ('selectcase', 'selecttype', 'selectrank', 'where', 'forall')
            is_control_paren = j - 1 == v%first .or. v%is(j - 2, ':')
        end select
    end function is_control_paren

end subroutine statement_gaps

elemental logical function is_word(kind)
    !! Tokens that would run together with a following word: names, numbers and BOZ literals.
    integer, intent(in) :: kind   !! token kind (`TK_*`)
    is_word = kind == TK_NAME .or. kind == TK_INT .or. kind == TK_REAL .or. kind == TK_BOZ
end function is_word

end module ffmt_whitespace


!>>>>> build/dependencies/toml-f/src/tomlf/de/context.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Provides a container to store tokens for later use
module tomlf_de_context
   use tomlf_constants, only : tfc
   use tomlf_de_token, only : toml_token, resize
   use tomlf_diagnostic, only : toml_diagnostic, toml_label, render, toml_level
   use tomlf_terminal, only : toml_terminal
   implicit none
   private

   public :: toml_context

   !> Container storing tokens
   type :: toml_context
      !> Filename of the input
      character(:, tfc), allocatable :: filename
      !> Actual source
      character(:, tfc), allocatable :: source
      !> Stack of stored tokens
      type(toml_token), allocatable :: token(:)
      !> Last stored token
      integer :: top = 0
   contains
      !> Push a new token to the stack
      procedure :: push_back
      !> Create a report
      generic :: report => report1, report2
      !> Create a report with a single label
      procedure :: report1
      !> Create a report with a two labels
      procedure :: report2
   end type toml_context

contains

!> Push a new token to the stack
subroutine push_back(self, token)
   !> Instance of the token storage
   class(toml_context), intent(inout) :: self
   !> New token to be added
   type(toml_token), intent(in) :: token

   if (.not.allocated(self%token)) call resize(self%token)
   if (self%top >= size(self%token)) call resize(self%token)

   self%top = self%top + 1
   self%token(self%top) = token
end subroutine push_back

!> Create a report with a single label
pure function report1(self, message, origin, label, level, color) result(string)
   !> Instance of the token storage
   class(toml_context), intent(in) :: self
   !> Message for the report
   character(*, tfc), intent(in) :: message
   !> Position to report at
   integer, intent(in) :: origin
   !> String for the label
   character(*, tfc), intent(in), optional :: label
   !> Highlight level
   integer, intent(in), optional :: level
   !> Color terminal
   type(toml_terminal), intent(in), optional :: color
   !> Final rendered report
   character(:, tfc), allocatable :: string

   type(toml_diagnostic) :: diagnostic
   type(toml_label), allocatable :: labels(:)
   integer :: level_

   level_ = toml_level%error
   if (present(level)) level_ = level

   if (origin > 0 .and. origin <= self%top) then
      allocate(labels(1))
      labels(1) = toml_label(level_, &
         &  self%token(origin)%first, self%token(origin)%last, label, .true.)
   end if

   diagnostic = toml_diagnostic( &
      & level_, &
      & message, &
      & self%filename, &
      & labels)

   if (.not.present(color)) then
      string = render(diagnostic, self%source, toml_terminal(.false.))
   else
      string = render(diagnostic, self%source, color)
   end if
end function report1

!> Create a report with two labels
pure function report2(self, message, origin1, origin2, label1, label2, level1, level2, color) &
      & result(string)
   !> Instance of the token storage
   class(toml_context), intent(in) :: self
   !> Message for the report
   character(*, tfc), intent(in) :: message
   !> Position to report at
   integer, intent(in) :: origin1, origin2
   !> String for the label
   character(*, tfc), intent(in), optional :: label1, label2
   !> Highlight level
   integer, intent(in), optional :: level1, level2
   !> Color terminal
   type(toml_terminal), intent(in), optional :: color
   !> Final rendered report
   character(:, tfc), allocatable :: string

   type(toml_diagnostic) :: diagnostic
   type(toml_label), allocatable :: labels(:)
   integer :: level1_, level2_

   level1_ = toml_level%error
   if (present(level1)) level1_ = level1
   level2_ = toml_level%info
   if (present(level2)) level2_ = level2

   if (origin1 > 0 .and. origin1 <= self%top &
      & .and. origin2 > 0 .and. origin2 <= self%top) then
      allocate(labels(2))
      labels(1) = toml_label(level1_, &
         &  self%token(origin1)%first, self%token(origin1)%last, label1, .true.)
      labels(2) = toml_label(level2_, &
         &  self%token(origin2)%first, self%token(origin2)%last, label2, .false.)
   end if

   diagnostic = toml_diagnostic( &
      & level1_, &
      & message, &
      & self%filename, &
      & labels)

   if (.not.present(color)) then
      string = render(diagnostic, self%source, toml_terminal(.false.))
   else
      string = render(diagnostic, self%source, color)
   end if
end function report2

end module tomlf_de_context


!>>>>> build/dependencies/toml-f/src/tomlf/structure/array_list.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a basic storage structure as pointer list of pointers.
!>
!> This implementation does purposely not use pointer attributes in the
!> datastructure to make it safer to work with.
module tomlf_structure_array_list
   use tomlf_constants, only : tfc
   use tomlf_structure_list, only : toml_list_structure
   use tomlf_structure_node, only : toml_node, resize
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_array_list, new_array_list


   !> Stores TOML values in a list of pointers
   type, extends(toml_list_structure) :: toml_array_list

      !> Current number of stored TOML values
      integer :: n = 0

      !> List of TOML values
      type(toml_node), allocatable :: lst(:)

   contains

      !> Get number of TOML values in the structure
      procedure :: get_len

      !> Get TOML value at a given index
      procedure :: get

      !> Push back a TOML value to the structure
      procedure :: push_back

      !> Remove the first element from the structure
      procedure :: shift

      !> Remove the last element from the structure
      procedure :: pop

      !> Destroy the data structure
      procedure :: destroy

   end type toml_array_list


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Constructor for the storage data structure
subroutine new_array_list(self, n)

   !> Instance of the structure
   type(toml_array_list), intent(out) :: self

   !> Initial storage capacity
   integer, intent(in), optional :: n

   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if

end subroutine new_array_list


!> Get number of TOML values in the structure
pure function get_len(self) result(length)

   !> Instance of the structure
   class(toml_array_list), intent(in), target :: self

   !> Current length of the ordered structure
   integer :: length

   length = self%n

end function get_len


!> Get TOML value at a given index
subroutine get(self, idx, ptr)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> Position in the ordered structure
   integer, intent(in) :: idx

   !> Pointer to the stored value at given index
   class(toml_value), pointer, intent(out) :: ptr

   nullify(ptr)

   if (idx > 0 .and. idx <= self%n) then
      if (allocated(self%lst(idx)%val)) then
         ptr => self%lst(idx)%val
      end if
   end if

end subroutine get


!> Push back a TOML value to the structure
subroutine push_back(self, val)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> TOML value to be stored
   class(toml_value), allocatable, intent(inout) :: val

   integer :: m

   if (.not.allocated(self%lst)) then
      call resize(self%lst, initial_size)
   end if

   m = size(self%lst)
   if (self%n >= m) then
      call resize(self%lst, m + m/2 + 1)
   end if

   self%n = self%n + 1
   call move_alloc(val, self%lst(self%n)%val)

end subroutine push_back


!> Remove the first element from the data structure
subroutine shift(self, val)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   integer :: i

   if (self%n > 0) then
      call move_alloc(self%lst(1)%val, val)
      do i = 2, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine shift


!> Remove the last element from the data structure
subroutine pop(self, val)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   if (self%n > 0) then
      call move_alloc(self%lst(self%n)%val, val)
      self%n = self%n - 1
   end if

end subroutine pop


!> Deconstructor for data structure
subroutine destroy(self)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   integer :: i

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do

   deallocate(self%lst)
   self%n = 0

end subroutine destroy


end module tomlf_structure_array_list


!>>>>> build/dependencies/toml-f/src/tomlf/structure/ordered_map.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a basic storage structure as pointer list of pointers.
!>
!> This implementation does purposely not use pointer attributes in the
!> datastructure to make it safer to work with.
module tomlf_structure_ordered_map
   use tomlf_constants, only : tfc
   use tomlf_structure_map, only : toml_map_structure
   use tomlf_structure_node, only : toml_node, resize
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_ordered_map, new_ordered_map


   !> Stores TOML values in a list of pointers
   type, extends(toml_map_structure) :: toml_ordered_map

      !> Current number of stored TOML values
      integer :: n = 0

      !> List of TOML values
      type(toml_node), allocatable :: lst(:)

   contains

      !> Get TOML value at a given key
      procedure :: get

      !> Push back a TOML value to the structure
      procedure :: push_back

      !> Remove TOML value at a given key and return it
      procedure :: pop

      !> Get list of all keys in the structure
      procedure :: get_keys

      !> Delete TOML value at a given key
      procedure :: delete

      !> Destroy the data structure
      procedure :: destroy

   end type toml_ordered_map


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Constructor for the storage data structure
subroutine new_ordered_map(self, n)

   !> Instance of the structure
   type(toml_ordered_map), intent(out) :: self

   !> Initial storage capacity
   integer, intent(in), optional :: n

   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if

end subroutine new_ordered_map


!> Get TOML value at a given key
subroutine get(self, key, ptr)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the stored value at given key
   class(toml_value), pointer, intent(out) :: ptr

   integer :: i

   nullify(ptr)

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (self%lst(i)%val%match_key(key)) then
            ptr => self%lst(i)%val
            exit
         end if
      end if
   end do

end subroutine get


!> Push back a TOML value to the structure
subroutine push_back(self, val)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> TOML value to be stored
   class(toml_value), allocatable, intent(inout) :: val

   integer :: m

   if (.not.allocated(self%lst)) then
      call resize(self%lst, initial_size)
   end if

   m = size(self%lst)
   if (self%n >= m) then
      call resize(self%lst, m + m/2 + 1)
   end if

   self%n = self%n + 1
   call move_alloc(val, self%lst(self%n)%val)

end subroutine push_back


!> Get list of all keys in the structure
subroutine get_keys(self, list)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   integer :: i

   allocate(list(self%n))

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (allocated(self%lst(i)%val%key)) then
            list(i)%key = self%lst(i)%val%key
            list(i)%origin = self%lst(i)%val%origin
         end if
      end if
   end do

end subroutine get_keys


!> Remove TOML value at a given key and return it
subroutine pop(self, key, val)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Removed TOML value
   class(toml_value), allocatable, intent(out) :: val

   integer :: idx, i

   idx = 0
   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (self%lst(i)%val%match_key(key)) then
            idx = i
            exit
         end if
      end if
   end do

   if (idx > 0) then
      call move_alloc(self%lst(idx)%val, val)
      do i = idx+1, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine pop


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   class(toml_value), allocatable :: val

   call self%pop(key, val)
   if (allocated(val)) then
      call val%destroy()
   end if

end subroutine delete


!> Deconstructor for data structure
subroutine destroy(self)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   integer :: i

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do

   deallocate(self%lst)
   self%n = 0

end subroutine destroy


end module tomlf_structure_ordered_map


!>>>>> ././src/ffmt_layout.f90
module ffmt_layout
!! Writes the formatted text of a tokenized file: statements indented by their nesting depth,
!! continuation lines aligned or indented by a fixed amount, comment lines indented like the code
!! around them, and within each line the blanks and letter case that [[compute_gaps]] and
!! [[compute_case]] ask for. Trailing blanks are removed.
!!
!! Items protected by formatter directives (see [[ffmt_directives]]) are written as they are.
use ffmt_tokens
use ffmt_statements
use ffmt_source, only: source_t
use ffmt_structure, only: structure_t
use ffmt_options
use ffmt_whitespace, only: GAP_KEEP, GAP_MIN1
use ffmt_case, only: case_marks_t, kind_part
use ffmt_strings, only: string_builder_t, utf8_width
implicit none
private

public :: layout


type :: writer_t
    !! Output being built, and the alignment state of the current statement.
    type(string_builder_t) :: buf               !! the output text
    integer :: col = 0                          !! output column: characters since the last line break
    integer :: nbrackets = 0                    !! open brackets in the current statement
    integer, allocatable :: bracket_first(:)    !! column of the first item after each open bracket, or -1;
                                                !! only `bracket_first(1:nbrackets)` is in use
    integer :: anchor_col(3) = -1               !! column of the first item after an assignment `=` / `=>`,
                                                !! a `use` statement's `only:` and a declaration's `::`, or -1
    integer :: pending_anchor = 0               !! the next code token is the first item after this anchor
    logical :: pending_bracket = .false.        !! the next code token is the first item after a bracket
    logical :: separated = .false.              !! a top-level `,` or `:` has been seen in the statement
    integer :: prev = 0                         !! the previous code token of the statement (index into the
                                                !! token list), or 0
    integer :: comment_in = -1                  !! input column of the comment on the previous line, or -1
    integer :: comment_out = -1                 !! output column of that comment
    logical :: line_comment = .false.           !! a comment has been written on the current line
    integer :: stmt_in = -1                     !! input column of the last statement, while comment lines
                                                !! directly follow it, or -1
    integer :: stmt_out = -1                    !! output column of that statement
    logical :: generic = .false.                !! the current statement is a `generic` binding
    logical :: declaration = .false.            !! a top-level `::` has been seen in the current statement
    integer :: line_length = 0                  !! maximum line length, or 0
    logical :: keep_original = .true.           !! fall back to a code line's original indentation when the
                                                !! new one would make it too long (when lines are not split)
    integer, allocatable :: long_lines(:)       !! input lines that kept their indentation to stay within the line length
    integer, allocatable :: gap(:)              !! blanks wanted before each token (see [[compute_gaps]])
    type(case_marks_t) :: cases                 !! letter case wanted for each token (see [[compute_case]])
end type writer_t

type :: piece_t
    !! A piece of an output line: a token, or the blanks before one. Its text is `src(first:last)`
    !! (with the token's letter case applied), or `blanks` blanks if that is positive; it is not
    !! copied, so that measuring a line allocates nothing.
    integer :: tok = 0                          !! token index, or 0 for blanks
    integer :: first = 1                        !! first byte of the text in the source
    integer :: last = 0                         !! last byte of the text in the source
    integer :: blanks = 0                       !! if positive, the piece is this many blanks instead
end type piece_t

! anchors for aligning continuation lines outside brackets, in order of preference
integer, parameter :: ANCHOR_ASSIGN = 1, ANCHOR_ONLY = 2, ANCHOR_DECL = 3

contains

function layout(source, st, gap, cases, opts, long_lines) result(out)
    !! The formatted text of `source`, from its structure ([[analyze_structure]]), the gaps
    !! ([[compute_gaps]]) and the letter case ([[compute_case]]) wanted.
    type(source_t), intent(in) :: source          !! the source
    type(structure_t), intent(in) :: st           !! its block structure
    integer, intent(in) :: gap(:)                 !! blanks wanted before each token
    type(case_marks_t), intent(in) :: cases       !! letter case wanted for each token
    type(format_options_t), intent(in) :: opts    !! the formatting options
    integer, allocatable, intent(out), optional :: long_lines(:)   !! input lines that kept their indentation
                                                                   !! because the new one would make them longer
                                                                   !! than `opts%line_length`
    character(len=:), allocatable :: out

    type(writer_t) :: w
    integer :: k, level, ind

    w%line_length = opts%line_length
    w%keep_original = .not. opts%split_lines
    w%gap = gap
    w%cases = cases
    allocate (w%long_lines(0))
    associate (src => source%text, tokens => source%tokens)
        do k = 1, source%stmts%n
            level = st%depth(k)
            if (.not. opts%indent_units) level = level - st%units(k)
            if (.not. opts%indent_procedures) level = level - st%procedures(k)
            if (opts%indent_fypp) level = level + st%fypp(k)
            ind = opts%base_indent + max(level, 0)*opts%indent
            associate (s => source%stmts%s(k))
                if (source%keep(k)) then                   ! a formatter directive: written as it is
                    call emit_range(w, src, tokens, s%first, s%last)
                    w%comment_in = -1
                    w%stmt_in = -1
                    cycle
                end if
                select case (s%kind)
                case (SK_BLANK)
                    call emit_blank(w, src, tokens, s)
                    w%comment_in = -1
                    w%stmt_in = -1
                case (SK_PREPROC)
                    if (opts%indent_fypp) then
                        call emit_fypp_line(w, src, tokens, s, ind)
                    else
                        call emit_range(w, src, tokens, s%first, s%last)
                    end if
                    w%comment_in = -1
                case (SK_COMMENT)
                    call emit_comment_line(w, src, tokens, s, ind, opts)
                case (SK_CODE)
                    call emit_statement(w, src, tokens, s, st%info(k)%label > 0, ind, opts)
                end select
            end associate
        end do
    end associate
    out = w%buf%get()
    if (present(long_lines)) long_lines = w%long_lines
end function layout

integer function fitting_column(w, src, tokens, f, lend, candidates, from, prefix, always_original) result(col)
    !! The first of the `candidates` columns at which the output line fits within the line length, or
    !! the smallest if none fits. The last candidate is the line's original column: if only that one
    !! fits, the line is recorded in `long_lines`. When lines are split (`w%keep_original` false), the
    !! original column is not a candidate for code, since splitting will make the line fit; it is
    !! still used if `always_original` is true (for comments, which are not split). Also when lines are
    !! split, a candidate other than the last is accepted if the line can be split to fit there: if
    !! its first piece up to a possible break fits.
    !!
    !! The input line starts with token `f`. The output line is `prefix` characters (e.g. `& ` for a
    !! leading continuation marker) followed by tokens `from:lend` (default `f:lend`).
    type(writer_t), intent(inout) :: w                 !! the output and the alignment state
    character(len=*), intent(in) :: src                !! the source text
    type(token_list_t), intent(in) :: tokens           !! its tokens
    integer, intent(in) :: f                           !! first non-blank token of the input line
    integer, intent(in) :: lend                        !! last token of the line, its line break if it has one
    integer, intent(in) :: candidates(:)               !! columns to try, best first; negative ones are skipped
    integer, intent(in), optional :: from              !! first token written after the prefix; requires `prefix`
    integer, intent(in), optional :: prefix            !! width of the text written before token `from`
    logical, intent(in), optional :: always_original   !! try the original column even when lines are split
    integer :: k, width, ncand, first_piece
    logical :: original

    col = max(0, candidates(1))
    if (w%line_length <= 0) return
    original = w%keep_original
    if (present(always_original)) original = original .or. always_original
    ncand = size(candidates)
    if (.not. original .and. ncand > 1) ncand = ncand - 1
    if (present(from)) then
        width = prefix + line_width(w, src, tokens, from, lend)
    else
        width = line_width(w, src, tokens, f, lend)
    end if
    first_piece = -1
    do k = 1, ncand
        if (candidates(k) < 0) cycle
        if (.not. original .and. k < ncand .and. candidates(k) + width > w%line_length) then
            if (first_piece < 0) then
                if (present(from)) then
                    first_piece = prefix + first_piece_width(w, src, tokens, from, lend)
                else
                    first_piece = first_piece_width(w, src, tokens, f, lend)
                end if
            end if
            if (candidates(k) + first_piece + 2 <= w%line_length) then   ! ` &` after the first piece
                col = candidates(k)
                return
            end if
        end if
        if (candidates(k) + width <= w%line_length) then
            col = candidates(k)
            if (original .and. k == size(candidates) .and. k > 1 .and. col /= candidates(1)) &
                w%long_lines = [w%long_lines, tokens%tok(f)%line]
            return
        end if
    end do
    col = max(0, minval(candidates(1:ncand)))     ! too long: it is split later, or was too long already
end function fitting_column

integer function first_piece_width(w, src, tokens, first, lend) result(width)
    !! Width of tokens `first:lend` up to the end of the first code token that has another code token
    !! after it on the line (the shortest first part of a split), or a huge value if there is none.
    type(writer_t), intent(in) :: w            !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    integer, intent(in) :: first               !! first token of the part of the line to measure
    integer, intent(in) :: lend                !! last token of the line, its line break if it has one
    type(piece_t), allocatable :: pieces(:)
    integer :: k, m, total

    width = huge(1)/2
    call line_pieces(w, src, tokens, first, lend, pieces)
    total = 0
    do k = 1, size(pieces)
        associate (text => src(pieces(k)%first:pieces(k)%last))
            if (pieces(k)%blanks > 0) then
                total = total + pieces(k)%blanks
            else
                if (scan(text, achar(10)//achar(13)) > 0) return
                total = total + utf8_width(text)
            end if
        end associate
        if (pieces(k)%tok == 0) cycle
        if (.not. is_code_kind(tokens%tok(pieces(k)%tok)%kind)) cycle
        if (pieces(k)%tok == first .and. tokens%tok(first)%kind == TK_INT) cycle   ! a statement label
        do m = k + 1, size(pieces)
            if (pieces(m)%tok == 0) cycle
            if (is_code_kind(tokens%tok(pieces(m)%tok)%kind) .and. .not. tokens%tok(pieces(m)%tok)%glued) then
                width = total
                return
            end if
            exit
        end do
    end do
end function first_piece_width

integer function line_width(w, src, tokens, first, lend) result(width)
    !! Width of tokens `first:lend` as they will be written on one line, without the line break and
    !! without a trailing comment (compilers only limit the length of the code). A comment on a line
    !! of its own counts.
    type(writer_t), intent(in) :: w            !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    integer, intent(in) :: first               !! first token of the part of the line to measure
    integer, intent(in) :: lend                !! last token of the line, its line break if it has one
    type(piece_t), allocatable :: pieces(:)
    integer :: k, j, before_blank

    call line_pieces(w, src, tokens, first, lend, pieces)
    width = 0
    before_blank = 0
    do k = 1, size(pieces)
        if (pieces(k)%tok > 0 .and. k > 1) then
            if (tokens%tok(pieces(k)%tok)%kind == TK_COMMENT) then
                width = before_blank            ! a trailing comment, and the blanks before it
                return
            end if
        end if
        if (pieces(k)%blanks > 0) then
            width = width + pieces(k)%blanks
            cycle
        end if
        associate (text => src(pieces(k)%first:pieces(k)%last))
            if (pieces(k)%tok > 0) before_blank = width + utf8_width(text)
            j = scan(text, achar(10)//achar(13))
            if (j > 0) then                       ! a line break, or a token continued on the next line
                width = width + utf8_width(text(1:j - 1))
                return
            end if
            width = width + utf8_width(text)
        end associate
    end do
end function line_width

subroutine line_pieces(w, src, tokens, first, lend, pieces)
    !! The pieces of output for tokens `first:lend` of a line: the tokens, with the blanks between them
    !! adjusted by the whitespace rules and trailing blanks removed.
    type(writer_t), intent(in) :: w                        !! the output and the alignment state
    character(len=*), intent(in) :: src                    !! the source text
    type(token_list_t), intent(in) :: tokens               !! its tokens
    integer, intent(in) :: first                           !! first token of the part of the line
    integer, intent(in) :: lend                            !! last token of the line, its line break if it has one
    type(piece_t), allocatable, intent(out) :: pieces(:)   !! the pieces, in order
    integer :: t, n, g, last

    allocate (pieces(2*(lend - first + 1) + 1))
    n = 0
    do t = first, lend
        if (tokens%tok(t)%kind == TK_SPACE) then
            if (t == lend) cycle
            if (tokens%tok(t + 1)%kind == TK_NEWLINE) cycle
            g = -1
            ! blanks between two code tokens may be adjusted by the rule for the second one, and
            ! blanks before a trailing comment by the comment spacing
            if (t > first .and. t < lend) then
                if (is_code_kind(tokens%tok(t - 1)%kind) .and. is_code_kind(tokens%tok(t + 1)%kind)) then
                    g = w%gap(t + 1)
                else if (tokens%tok(t + 1)%kind == TK_COMMENT) then
                    g = w%gap(t + 1)
                end if
                if (g == 0) cycle
            end if
            call add(0, tokens%tok(t)%first, tokens%tok(t)%last, max(g, 0))
            cycle
        end if
        if (t > first) then
            if (is_code_kind(tokens%tok(t)%kind) .and. is_code_kind(tokens%tok(t - 1)%kind)) then
                g = w%gap(t)                                   ! no blank in the input
                if (g == GAP_MIN1) then
                    call add(0, 1, 0, 1)
                else if (g > 0) then
                    call add(0, 1, 0, g)
                end if
            else if (tokens%tok(t)%kind == TK_COMMENT .and. tokens%tok(t - 1)%kind /= TK_SPACE) then
                if (w%gap(t) > 0) call add(0, 1, 0, w%gap(t))  ! a trailing comment right after the code
            end if
        end if
        last = tokens%tok(t)%last
        if (tokens%tok(t)%kind == TK_COMMENT) then       ! without trailing blanks and tabs
            do while (last >= tokens%tok(t)%first)
                if (src(last:last) /= ' ' .and. src(last:last) /= achar(9)) exit
                last = last - 1
            end do
        end if
        call add(t, tokens%tok(t)%first, last, 0)
    end do
    pieces = pieces(1:n)

contains

    subroutine add(tok, from, to, blanks)
        !! Append a piece with these components (see [[piece_t]]).
        integer, intent(in) :: tok, from, to, blanks   !! its `tok`, `first`, `last` and `blanks`
        n = n + 1
        pieces(n) = piece_t(tok, from, to, blanks)
    end subroutine add

end subroutine line_pieces

subroutine put(w, text)
    !! Append `text` and track the output column.
    type(writer_t), intent(inout) :: w     !! the output and the alignment state
    character(len=*), intent(in) :: text   !! text to append; it may contain line breaks
    integer :: j
    call w%buf%append(text)
    j = scan(text, achar(10)//achar(13), back=.true.)
    if (j > 0) then
        w%col = utf8_width(text(j + 1:))
    else
        w%col = w%col + utf8_width(text)
    end if
end subroutine put

subroutine emit_range(w, src, tokens, first, last)
    !! Append tokens unchanged.
    type(writer_t), intent(inout) :: w         !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    integer, intent(in) :: first, last         !! the tokens `first:last` to append
    integer :: j
    do j = first, last
        call put(w, token_text(src, tokens%tok(j)))
    end do
end subroutine emit_range

subroutine emit_fypp_line(w, src, tokens, s, ind)
    !! A preprocessor line: a fypp directive (`#:`, `#!`, `$:`, `@:`) is indented by `ind`; a cpp line
    !! is written as it is. Continuation lines of the directive are not changed.
    type(writer_t), intent(inout) :: w            !! the output and the alignment state
    character(len=*), intent(in) :: src           !! the source text
    type(token_list_t), intent(in) :: tokens      !! its tokens
    type(statement_t), intent(in) :: s            !! the line, a preprocessor item
    integer, intent(in) :: ind                    !! indentation, in blanks
    integer :: f
    f = first_non_blank(tokens, s%first, s%last)
    if (f > s%last) then
        call emit_range(w, src, tokens, s%first, s%last)
        return
    end if
    associate (t => tokens%tok(f))
        if (t%kind /= TK_PREPROC .or. t%last == t%first) then
            call emit_range(w, src, tokens, s%first, s%last)
        else if (src(t%first:t%first) == '#' .and. index(':!', src(t%first + 1:t%first + 1)) == 0) then
            call emit_range(w, src, tokens, s%first, s%last)         ! cpp
        else
            call put(w, repeat(' ', ind))
            call emit_range(w, src, tokens, f, s%last)
        end if
    end associate
end subroutine emit_fypp_line

subroutine emit_blank(w, src, tokens, s)
    !! A blank line: only its line break (and a byte order mark) is kept.
    type(writer_t), intent(inout) :: w         !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    type(statement_t), intent(in) :: s         !! the blank line item
    integer :: j
    do j = s%first, s%last
        if (tokens%tok(j)%kind == TK_NEWLINE .or. tokens%tok(j)%kind == TK_BOM) &
            call put(w, token_text(src, tokens%tok(j)))
    end do
end subroutine emit_blank

subroutine emit_comment_line(w, src, tokens, s, ind, opts)
    !! A comment line, re-indented to `ind`. A comment that lines up with the comment on the line
    !! above (a continued trailing comment) stays lined up with it. Comment lines directly after a
    !! statement that are indented more than it keep their extra indentation. With `keep_column1_comments`, a
    !! comment that starts in column 1 stays there.
    type(writer_t), intent(inout) :: w           !! the output and the alignment state
    character(len=*), intent(in) :: src          !! the source text
    type(token_list_t), intent(in) :: tokens     !! its tokens
    type(statement_t), intent(in) :: s           !! the comment line item
    integer, intent(in) :: ind                   !! indentation of the code around it, in blanks
    type(format_options_t), intent(in) :: opts   !! the formatting options
    integer :: j, col

    if (.not. opts%indent_comments) then
        call emit_range(w, src, tokens, s%first, s%last)
        w%comment_in = -1
        return
    end if
    do j = s%first, s%last
        select case (tokens%tok(j)%kind)
        case (TK_BOM, TK_NEWLINE)
            call put(w, token_text(src, tokens%tok(j)))
        case (TK_COMMENT)
            if (opts%keep_column1_comments .and. input_column(src, tokens%tok(j)) == 0) then
                col = 0                         ! a comment in column 1 stays there
                w%comment_in = -1
            else
                col = ind
                ! comments directly after a statement and indented deeper than it (such as FORD
                ! `!!` documentation) keep their extra indentation
                if (w%stmt_in >= 0) col = max(ind, w%stmt_out + input_column(src, tokens%tok(j)) - w%stmt_in)
                col = comment_column(w, src, tokens, j, col)
            end if
            call put(w, repeat(' ', col))
            call put(w, trim_blanks(token_text(src, tokens%tok(j))))
        end select
    end do
end subroutine emit_comment_line

integer function comment_column(w, src, tokens, j, default) result(col)
    !! Output column for the comment-only line with comment token `j`: lined up with the comment on
    !! the line above if it was lined up with it in the input, otherwise `default`. Records the
    !! comment for the next line.
    type(writer_t), intent(inout) :: w         !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    integer, intent(in) :: j                   !! the comment token
    integer, intent(in) :: default             !! column to use if it is not lined up with the comment above
    integer :: in_col
    in_col = input_column(src, tokens%tok(j))
    col = default
    if (w%comment_in >= 0 .and. in_col == w%comment_in) col = w%comment_out
    col = fitting_column(w, src, tokens, j, j, [col, default, in_col], always_original=.true.)
    w%comment_in = in_col
    w%comment_out = col
end function comment_column

pure integer function input_column(src, t) result(col)
    !! Column of token `t` in the input (characters before it on its line).
    character(len=*), intent(in) :: src   !! the source text
    type(token_t), intent(in) :: t        !! a token of `src`
    integer :: j
    j = scan(src(1:t%first - 1), achar(10)//achar(13), back=.true.)
    col = utf8_width(src(j + 1:t%first - 1))
end function input_column

subroutine emit_statement(w, src, tokens, s, labeled, ind, opts)
    !! A code statement, with its continuation lines and the comment, blank and preprocessor lines
    !! between them. The first line is indented to `ind`; a statement label stays at the start of
    !! the line.
    type(writer_t), intent(inout) :: w           !! the output and the alignment state
    character(len=*), intent(in) :: src          !! the source text
    type(token_list_t), intent(in) :: tokens     !! its tokens
    type(statement_t), intent(in) :: s           !! the code statement
    logical, intent(in) :: labeled               !! the statement has a statement label
    integer, intent(in) :: ind                   !! indentation of the statement, in blanks
    type(format_options_t), intent(in) :: opts   !! the formatting options
    integer :: j, lend, f
    logical :: first_line, mid_line

    w%nbrackets = 0
    w%anchor_col = -1
    w%pending_anchor = 0
    w%pending_bracket = .false.
    w%separated = .false.
    w%declaration = .false.
    w%prev = 0
    ! a statement after `;` starts in the middle of a line
    mid_line = .false.
    if (s%first > 1) mid_line = tokens%tok(s%first - 1)%kind /= TK_NEWLINE .and. tokens%tok(s%first - 1)%kind /= TK_BOM
    first_line = .true.
    j = s%first
    do while (j <= s%last)
        lend = j
        do while (lend < s%last)
            if (tokens%tok(lend)%kind == TK_NEWLINE) exit
            lend = lend + 1
        end do
        if (tokens%tok(j)%kind == TK_BOM) then
            call put(w, token_text(src, tokens%tok(j)))
            j = j + 1
        end if
        f = first_non_blank(tokens, j, lend)
        w%line_comment = .false.

        if (f > lend) then
            continue                                            ! nothing left on the line
        else if (tokens%tok(f)%kind == TK_NEWLINE) then
            call put(w, token_text(src, tokens%tok(f)))         ! blank line
        else if (tokens%tok(f)%kind == TK_PREPROC) then
            call emit_range(w, src, tokens, j, lend)
        else if (tokens%tok(f)%kind == TK_COMMENT) then
            call put(w, repeat(' ', comment_column(w, src, tokens, f, ind + opts%continuation_indent)))
            call emit_body(w, src, tokens, f, lend)
            w%line_comment = .true.
        else if (first_line) then
            if (mid_line) then
                call emit_body(w, src, tokens, j, lend)
            else if (labeled .and. tokens%tok(f)%kind == TK_INT) then
                call put(w, token_text(src, tokens%tok(f)))
                call put(w, repeat(' ', max(1, ind - w%col)))
                call start_statement(first_non_blank(tokens, f + 1, lend))   ! the code after the label
                call emit_body(w, src, tokens, first_non_blank(tokens, f + 1, lend), lend)
            else
                call put(w, repeat(' ', fitting_column(w, src, tokens, f, lend, &
                                                       [ind, input_column(src, tokens%tok(f))])))
                call start_statement(f)
                call emit_body(w, src, tokens, f, lend)
            end if
            first_line = .false.
        else
            call emit_continuation(w, src, tokens, f, lend, continuation_target(w, ind, opts), &
                                   ind + opts%continuation_indent)
        end if
        if (.not. w%line_comment) w%comment_in = -1
        w%pending_anchor = 0
        w%pending_bracket = .false.
        j = lend + 1
    end do
contains

    subroutine start_statement(f)
        !! Record where the statement starting with token `f` begins, for comment lines after it.
        integer, intent(in) :: f   !! first code token of the statement, after any label
        w%stmt_in = -1
        if (f > s%last) return
        w%stmt_in = input_column(src, tokens%tok(f))
        w%stmt_out = w%col
        w%generic = token_lower(src, tokens%tok(f)) == 'generic'
    end subroutine start_statement

end subroutine emit_statement

pure integer function continuation_target(w, ind, opts) result(target)
    !! Column for the code of a continuation line: the first item after the innermost open
    !! bracket; outside brackets, the first item after an assignment, a `use` statement's `only:`
    !! or a declaration's `::`; otherwise a fixed indent.
    type(writer_t), intent(in) :: w              !! the output and the alignment state
    integer, intent(in) :: ind                   !! indentation of the statement, in blanks
    type(format_options_t), intent(in) :: opts   !! the formatting options
    integer :: a
    target = ind + opts%continuation_indent
    if (opts%continuation /= CONT_ALIGN) return
    if (w%nbrackets > 0) then
        if (w%bracket_first(w%nbrackets) >= 0) target = w%bracket_first(w%nbrackets)
        return
    end if
    do a = 1, size(w%anchor_col)
        if (w%anchor_col(a) >= 0) then
            target = w%anchor_col(a)
            return
        end if
    end do
end function continuation_target

subroutine emit_continuation(w, src, tokens, f, lend, aligned, indented)
    !! A continuation line whose first non-blank token is `f`, with its code starting at the column
    !! `aligned`, or at `indented` or its original column if that would make the line too long.
    type(writer_t), intent(inout) :: w         !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    integer, intent(in) :: f                   !! first non-blank token of the line
    integer, intent(in) :: lend                !! last token of the line, its line break if it has one
    integer, intent(in) :: aligned             !! preferred column for the code (see [[continuation_target]])
    integer, intent(in) :: indented            !! column for the code with a fixed continuation indent
    integer :: g, target, orig

    if (tokens%tok(f)%kind /= TK_CONT) then
        target = fitting_column(w, src, tokens, f, lend, [aligned, indented, input_column(src, tokens%tok(f))])
        call put(w, repeat(' ', target))
        call emit_body(w, src, tokens, f, lend)
        return
    end if
    ! a leading `&`, placed so that the code after it starts at the target column
    orig = input_column(src, tokens%tok(f))
    g = f + 1
    if (g <= lend) then
        if (is_code_kind(tokens%tok(g)%kind)) then
            ! `&code`: keep them together
            target = fitting_column(w, src, tokens, f, lend, [aligned - 1, indented - 1, orig], g, 1)
            call put(w, repeat(' ', target)//'&')
            call emit_body(w, src, tokens, g, lend)
            return
        end if
    end if
    g = first_non_blank(tokens, g, lend)
    target = fitting_column(w, src, tokens, f, lend, [aligned - 2, indented - 2, orig], g, 2)
    call put(w, repeat(' ', target)//'&')
    if (g <= lend) then
        if (tokens%tok(g)%kind /= TK_NEWLINE) call put(w, ' ')
    end if
    call emit_body(w, src, tokens, g, lend)
end subroutine emit_continuation

subroutine emit_body(w, src, tokens, first, lend)
    !! Tokens `first:lend` of a line (see [[line_pieces]]), while tracking open brackets and the
    !! anchors for aligning continuation lines.
    type(writer_t), intent(inout) :: w         !! the output and the alignment state
    character(len=*), intent(in) :: src        !! the source text
    type(token_list_t), intent(in) :: tokens   !! its tokens
    integer, intent(in) :: first               !! first token to write
    integer, intent(in) :: lend                !! last token of the line, its line break if it has one
    type(piece_t), allocatable :: pieces(:)
    integer :: k, t, ka, kb
    character(len=:), allocatable :: text

    call line_pieces(w, src, tokens, first, lend, pieces)
    do k = 1, size(pieces)
        t = pieces(k)%tok
        if (pieces(k)%blanks > 0) then
            call put(w, repeat(' ', pieces(k)%blanks))
            cycle
        end if
        if (t == 0) then
            call put(w, src(pieces(k)%first:pieces(k)%last))
            cycle
        end if
        if (w%cases%whole(t) == CASE_KEEP .and. w%cases%part(t) == CASE_KEEP) then
            call write_token(t, src(pieces(k)%first:pieces(k)%last))     ! without a copy
            cycle
        end if
        text = src(pieces(k)%first:pieces(k)%last)
        if (w%cases%whole(t) /= CASE_KEEP) text = to_case(text, w%cases%whole(t) == CASE_UPPER)
        if (w%cases%part(t) /= CASE_KEEP) then
            call kind_part(text, tokens%tok(t)%kind, ka, kb)
            if (kb >= ka) text(ka:kb) = to_case(text(ka:kb), w%cases%part(t) == CASE_UPPER)
        end if
        call write_token(t, text)
    end do

contains

    subroutine write_token(t, text)
        !! Write token `t` as `text` (its text with the letter case applied), tracking brackets,
        !! anchors and trailing comments.
        integer, intent(in) :: t                 !! index into the token list
        character(len=*), intent(in) :: text     !! the text to write
        if (tokens%tok(t)%kind == TK_COMMENT) then
            if (.not. w%line_comment) then           ! a trailing comment: later lines may line up with it
                w%comment_in = input_column(src, tokens%tok(t))
                w%comment_out = w%col
                w%line_comment = .true.
            end if
        end if
        if (.not. is_code_kind(tokens%tok(t)%kind)) then
            call put(w, text)
            return
        end if
        if (w%pending_bracket) w%bracket_first(w%nbrackets) = w%col
        if (w%pending_anchor > 0) w%anchor_col(w%pending_anchor) = w%col
        w%pending_bracket = .false.
        w%pending_anchor = 0
        call put(w, text)
        if (tokens%tok(t)%kind == TK_OP) then
            select case (text)
            case ('(', '[')
                if (.not. allocated(w%bracket_first)) allocate (w%bracket_first(16))
                if (w%nbrackets == size(w%bracket_first)) w%bracket_first = [w%bracket_first, w%bracket_first]
                w%nbrackets = w%nbrackets + 1
                w%bracket_first(w%nbrackets) = -1
                w%pending_bracket = .true.
            case (')', ']')
                if (w%nbrackets > 0) w%nbrackets = w%nbrackets - 1
            case default
                if (w%nbrackets == 0) call top_level_operator(text)
            end select
        end if
        w%prev = t
    end subroutine write_token

    subroutine top_level_operator(op)
        !! Record alignment anchors at a top-level operator.
        character(len=*), intent(in) :: op   !! text of the operator
        select case (op)
        case ('=', '=>')
            ! an assignment, unless a `,` or `:` came first (`use m, only: a => b`, `integer :: n = 1`),
            ! or the bindings of `generic :: name => a, b`
            if (.not. w%separated) then
                call anchor(ANCHOR_ASSIGN)
            else if (op == '=>' .and. w%generic .and. w%declaration) then
                call anchor(ANCHOR_ASSIGN)
            end if
        case ('::')
            call anchor(ANCHOR_DECL)
            w%separated = .true.
            w%declaration = .true.
        case (':')
            if (w%prev > 0) then
                if (token_lower(src, tokens%tok(w%prev)) == 'only') call anchor(ANCHOR_ONLY)
            end if
            w%separated = .true.
        case (',')
            w%separated = .true.
        end select
    end subroutine top_level_operator

    subroutine anchor(a)
        !! Make the next code token the first item after anchor `a`, unless that anchor is already set.
        integer, intent(in) :: a   !! `ANCHOR_ASSIGN`, `ANCHOR_ONLY` or `ANCHOR_DECL`
        if (w%anchor_col(a) < 0) w%pending_anchor = a
    end subroutine anchor
end subroutine emit_body

pure integer function first_non_blank(tokens, first, last) result(f)
    !! First token in `first:last` that is not a blank, or `last + 1`.
    type(token_list_t), intent(in) :: tokens   !! the token list
    integer, intent(in) :: first, last         !! the token range to search
    do f = first, last
        if (tokens%tok(f)%kind /= TK_SPACE) return
    end do
    f = last + 1
end function first_non_blank

pure function to_case(text, upper) result(t)
    !! `text` in ASCII upper or lower case.
    character(len=*), intent(in) :: text   !! the text to convert
    logical, intent(in) :: upper           !! true for upper case, false for lower case
    character(len=len(text)) :: t
    integer :: k, c
    t = text
    do k = 1, len(t)
        c = iachar(t(k:k))
        if (upper .and. c >= iachar('a') .and. c <= iachar('z')) t(k:k) = achar(c - 32)
        if (.not. upper .and. c >= iachar('A') .and. c <= iachar('Z')) t(k:k) = achar(c + 32)
    end do
end function to_case

pure function trim_blanks(text) result(t)
    !! `text` without trailing blanks and tabs.
    character(len=*), intent(in) :: text   !! the text to trim
    character(len=:), allocatable :: t
    integer :: n
    n = len(text)
    do while (n > 0)
        if (text(n:n) /= ' ' .and. text(n:n) /= achar(9)) exit
        n = n - 1
    end do
    t = text(1:n)
end function trim_blanks

end module ffmt_layout


!>>>>> build/dependencies/toml-f/src/tomlf/structure.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Abstraction layer for the actual storage of the data structure.
!>
!> The structure implementations provide the actual storage for TOML values, with
!> a generic enough interface to make the definition of the TOML data structures
!> independent of the actual algorithm used for storing the TOML values.
!>
!> Every data structure defined here should strive to only use allocatable
!> data types and limit the use of pointer attributes as they interfer with
!> the automatic memory management of Fortran. A well defined data structure
!> in allocatables allows deep-copying of TOML values by assignment, data structures
!> requiring pointer attributes have to define an assignment(=) interface to
!> allow deep-copying of TOML values.
module tomlf_structure
   use tomlf_structure_list, only : toml_list_structure
   use tomlf_structure_map, only : toml_map_structure
   use tomlf_structure_array_list, only : toml_array_list, new_array_list
   use tomlf_structure_ordered_map, only : toml_ordered_map, new_ordered_map
   implicit none
   private

   public :: toml_list_structure, toml_map_structure
   public :: new_list_structure, new_map_structure


contains


!> Constructor for the ordered storage data structure
subroutine new_list_structure(self)

   !> Instance of the structure
   class(toml_list_structure), allocatable, intent(out) :: self

   block
      type(toml_array_list), allocatable :: list

      allocate(list)
      call new_array_list(list)
      call move_alloc(list, self)
   end block

end subroutine new_list_structure


!> Constructor for the storage data structure
subroutine new_map_structure(self)

   !> Instance of the structure
   class(toml_map_structure), allocatable, intent(out) :: self

   block
      type(toml_ordered_map), allocatable :: map

      allocate(map)
      call new_ordered_map(map)
      call move_alloc(map, self)
   end block

end subroutine new_map_structure


end module tomlf_structure


!>>>>> build/dependencies/toml-f/src/tomlf/de/lexer.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Provides tokenization for TOML documents.
!>
!> The lexer provides a way to turn a stream of characters into tokens which
!> are further processed by the parser and turned into actual TOML data structures.
!> In the current structure no knowledge about the character stream is required
!> in the parser to generate the data structures.
!>
!> The validity of all tokens can be guaranteed by the lexer, however syntax errors
!> and semantic errors are not detected until the parser is run. Identification of
!> invalid tokens and recovery of the tokenization is done on a best effort basis.
!>
!> To avoid overflows in the parser due to deeply nested but unclosed groups, the
!> lexer will always tokenize a complete group to verify it is closed properly.
!> Unclosed groups will lead to the first token of the group getting invalidated,
!> to allow reporting in the parsing phase.
module tomlf_de_lexer
   use tomlf_constants, only : tfc, tfi, tfr, TOML_BACKSPACE, TOML_TABULATOR, TOML_NEWLINE, &
      & TOML_CARRIAGE_RETURN, TOML_FORMFEED, TOML_ESC
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_de_abc, only : abstract_lexer
   use tomlf_de_context, only : toml_context
   use tomlf_de_token, only : toml_token, stringify, token_kind, resize
   use tomlf_error, only : toml_error, toml_stat, make_error
   use tomlf_utils, only : read_whole_file, read_whole_line
   implicit none
   private

   public :: toml_lexer, new_lexer_from_file, new_lexer_from_unit, new_lexer_from_string
   public :: toml_token, stringify, token_kind


   !> Possible characters encountered in a lexeme
   type :: enum_char
      character(1, tfc) :: space = tfc_" "
      character(1, tfc) :: hash = tfc_"#"
      character(1, tfc) :: squote = tfc_"'"
      character(3, tfc) :: squote3 = repeat(tfc_"'", 3)
      character(1, tfc) :: dquote = tfc_""""
      character(3, tfc) :: dquote3 = repeat(tfc_"""", 3)
      character(1, tfc) :: backslash = achar(92, kind=tfc)
      character(1, tfc) :: dot = tfc_"."
      character(1, tfc) :: comma = tfc_","
      character(1, tfc) :: equal = tfc_"="
      character(1, tfc) :: lbrace = tfc_"{"
      character(1, tfc) :: rbrace = tfc_"}"
      character(1, tfc) :: lbracket = tfc_"["
      character(1, tfc) :: rbracket = tfc_"]"
      character(1, tfc) :: newline = achar(10, kind=tfc)
      character(1, tfc) :: formfeed = achar(12, kind=tfc)
      character(1, tfc) :: carriage_return = achar(13, kind=tfc)
      character(1, tfc) :: bspace = achar(8, kind=tfc)
      character(1, tfc) :: tab = achar(9, kind=tfc)
      character(1, tfc) :: plus = tfc_"+"
      character(1, tfc) :: minus = tfc_"-"
      character(12, tfc) :: literal = tfc_"0123456789-_"
   end type enum_char

   !> Actual enumerator for possible characters
   type(enum_char), parameter :: char_kind = enum_char()

   !> Set of characters marking a terminated lexeme, mainly used for values and to
   !> obtain boundaries of invalid tokens.
   character(*, tfc), parameter :: terminated = &
      & char_kind%space//char_kind%tab//char_kind%newline//char_kind%carriage_return//&
      & char_kind%hash//char_kind%rbrace//char_kind%rbracket//char_kind%comma//&
      & char_kind%equal

   !> Scopes to identify the state of the lexer.
   type :: enum_scope
      !> Table scopes allow keypaths, in this scenario only bare keys, strings and
      !> literals are allowed, furthermore dots become special characters to separate
      !> the keypaths.
      integer :: table = 1
      !> Terminates a table scope and opens a value scope. Here usual values, like integer,
      !> floats or strings are allowed.
      integer :: equal = 2
      !> Opens an array scope, similar to the value scope for allowed characters but with
      !> simplified closing rules to allow handling of values and inline tables in arrays.
      integer :: array = 3
   end type enum_scope

   !> Actual enumerator for auxiliary scopes
   type(enum_scope), parameter :: lexer_scope = enum_scope()

   !> Item identifying the scope and the corresponding token index
   type :: stack_item
      !> Current scope of the item, can only be removed with matching scope
      integer :: scope
      !> Token index in the buffer of the lexer, used for invalidation of unclosed groups
      integer :: token
   end type stack_item

   !> Reallocate the stack of scopes
   interface resize
      module procedure :: resize_scope
   end interface


   !> Tokenizer for TOML documents.
   type, extends(abstract_lexer) :: toml_lexer
      !> Name of the source file, used for error reporting
      character(len=:), allocatable :: filename
      !> Current internal position in the source chunk
      integer :: pos = 0
      !> Current source chunk, for convenience stored as character array rather than string
      character(:, tfc), allocatable :: chunk
      !> Last scope of the lexer
      integer :: top = 0
      !> Stack of scopes, used to identify the current state of the lexer
      type(stack_item), allocatable :: stack(:)
      !> Index in the buffer queue
      integer :: buffer = 0
      !> Douple-ended queue for buffering tokens
      type(toml_context) :: context
   contains
      !> Obtain the next token
      procedure :: next
      !> Extract a string from a token
      procedure :: extract_string
      !> Extract an integer from a token
      procedure :: extract_integer
      !> Extract a float from a token
      procedure :: extract_float
      !> Extract a boolean from a token
      procedure :: extract_bool
      !> Extract a timestamp from a token
      procedure :: extract_datetime
      !> Get information about source
      procedure :: get_info
   end type toml_lexer

contains

!> Create a new instance of a lexer by reading from a file
subroutine new_lexer_from_file(lexer, filename, error)
   !> Instance of the lexer
   type(toml_lexer), intent(out) :: lexer
   !> Name of the file to read from
   character(len=*), intent(in) :: filename
   !> Error code
   type(toml_error), allocatable, intent(out) :: error

   integer :: stat

   lexer%pos = 0
   lexer%filename = filename
   call resize(lexer%stack)
   call read_whole_file(filename, lexer%chunk, stat)

   if (stat /= 0) then
      call make_error(error, "Could not open file '"//filename//"'")
   end if
end subroutine new_lexer_from_file

!> Create a new instance of a lexer by reading from a unit.
!>
!> Currently, only sequential access units can be processed by this constructor.
subroutine new_lexer_from_unit(lexer, io, error)
   !> Instance of the lexer
   type(toml_lexer), intent(out) :: lexer
   !> Unit to read from
   integer, intent(in) :: io
   !> Error code
   type(toml_error), allocatable, intent(out) :: error

   character(:, tfc), allocatable :: source, line
   integer, parameter :: bufsize = 512
   character(bufsize, tfc) :: filename, mode
   integer :: stat

   inquire(unit=io, access=mode, name=filename)
   select case(trim(mode))
   case default
      stat = 1

   case("sequential", "SEQUENTIAL")
      allocate(character(len=0, kind=tfc) :: source)
      do
         call read_whole_line(io, line, stat)
         if (stat > 0) exit
         source = source // line // TOML_NEWLINE
         if (stat < 0) then
            if (is_iostat_end(stat)) stat = 0
            exit
         end if
      end do
      call new_lexer_from_string(lexer, source)
   end select
   if (len_trim(filename) > 0) lexer%filename = trim(filename)

   if (stat /= 0) then
      call make_error(error, "Failed to read from unit")
   end if
end subroutine new_lexer_from_unit

!> Create a new instance of a lexer by reading from a string.
subroutine new_lexer_from_string(lexer, string)
   !> Instance of the lexer
   type(toml_lexer), intent(out) :: lexer
   !> String to read from
   character(*, tfc), intent(in) :: string

   integer :: length

   length = len(string)
   lexer%pos = 0
   lexer%buffer = 0
   allocate(character(len=length, kind=tfc) :: lexer%chunk)
   lexer%chunk(:length) = string
   call resize(lexer%stack)
end subroutine new_lexer_from_string


!> Advance the lexer to the next token.
subroutine next(lexer, token)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   if (lexer%buffer >= lexer%context%top) then
      call fill_buffer(lexer)
   end if

   lexer%buffer = lexer%buffer + 1
   token = lexer%context%token(lexer%buffer)
end subroutine next

!> Fill the buffer with tokens, this routine will attempt to create as many tokens as
!> necessary to determine whether all opened groups are closed properly.
!>
!> The state of the buffer can be changed while this routine is running, therefore
!> accessing the buffer concurrently is not allowed.
subroutine fill_buffer(lexer)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   type(toml_token) :: token
   integer :: stack_top, it

   lexer%buffer = 0
   lexer%context%top = 0
   stack_top = lexer%top

   ! Tokenization will cover always a complete scope
   do while(lexer%top >= stack_top .and. token%kind /= token_kind%eof)
      call next_token(lexer, token)
      call lexer%context%push_back(token)
   end do

   ! Flag all incomplete inline table and array scopes for the parser
   if (lexer%top > stack_top) then
      do it = lexer%top, stack_top + 1, -1
         select case(lexer%stack(it)%scope)
         case(lexer_scope%table, lexer_scope%array)
            lexer%context%token(lexer%stack(it)%token)%kind = token_kind%unclosed
         end select
      end do
   end if
end subroutine fill_buffer

!> Actually generate the next token, unbuffered version
subroutine next_token(lexer, token)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   integer :: prev, pos

   ! Consume current token
   lexer%pos = lexer%pos + token%last - token%first + 1
   prev = lexer%pos
   pos = lexer%pos

   ! If lexer is exhausted, return EOF as early as possible
   if (pos > len(lexer%chunk)) then
      call pop(lexer, lexer_scope%equal)
      token = toml_token(token_kind%eof, prev, pos)
      return
   end if

   select case(peek(lexer, pos))
   case(char_kind%hash)
      do while(all(peek(lexer, pos+1) /= [char_kind%carriage_return, char_kind%newline]) &
            & .and. pos <= len(lexer%chunk))
         pos = pos + 1
      end do
      token = toml_token(token_kind%comment, prev, pos)

   case(char_kind%space, char_kind%tab)
      do while(any(match(lexer, pos+1, [char_kind%space, char_kind%tab])) &
            & .and. pos <= len(lexer%chunk))
         pos = pos + 1
      end do
      token = toml_token(token_kind%whitespace, prev, pos)

   case(char_kind%newline)
      call pop(lexer, lexer_scope%equal)
      token = toml_token(token_kind%newline, prev, pos)

   case(char_kind%carriage_return)
      if (match(lexer, pos+1, char_kind%newline)) then
         pos = pos + 1
         call pop(lexer, lexer_scope%equal)
         token = toml_token(token_kind%newline, prev, pos)
      else
         token = toml_token(token_kind%invalid, prev, pos)
      end if

   case(char_kind%dot)
      if (view_scope(lexer) == lexer_scope%table) then
         token = toml_token(token_kind%dot, prev, pos)
      else
         token = toml_token(token_kind%invalid, prev, pos)
      end if

   case(char_kind%comma)
      call pop(lexer, lexer_scope%equal)
      token = toml_token(token_kind%comma, prev, pos)

   case(char_kind%equal)
      token = toml_token(token_kind%equal, prev, pos)
      call push_back(lexer, lexer_scope%equal, lexer%context%top + 1)

   case(char_kind%lbrace)
      token = toml_token(token_kind%lbrace, prev, pos)
      call push_back(lexer, lexer_scope%table, lexer%context%top + 1)

   case(char_kind%rbrace)
      call pop(lexer, lexer_scope%equal)
      call pop(lexer, lexer_scope%table)
      token = toml_token(token_kind%rbrace, prev, pos)

   case(char_kind%lbracket)
      token = toml_token(token_kind%lbracket, prev, pos)
      if (any(view_scope(lexer) == [lexer_scope%equal, lexer_scope%array])) then
         call push_back(lexer, lexer_scope%array, lexer%context%top + 1)
      end if

   case(char_kind%rbracket)
      call pop(lexer, lexer_scope%array)
      token = toml_token(token_kind%rbracket, prev, pos)

   case(char_kind%squote)
      call next_sstring(lexer, token)

   case(char_kind%dquote)
      call next_dstring(lexer, token)

   case default
      if (view_scope(lexer) == lexer_scope%table) then
         call next_keypath(lexer, token)
      else
         call next_literal(lexer, token)
      end if

   end select
end subroutine next_token

!> Process next literal string token, can produce normal literals and multiline literals
subroutine next_sstring(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   character(1, tfc) :: ch
   integer :: prev, pos, it
   logical :: valid

   prev = lexer%pos
   pos = lexer%pos

   if (all(match(lexer, [pos+1, pos+2], char_kind%squote))) then
      pos = pos + 3

      pos = strstr(lexer%chunk(pos:), char_kind%squote3) + pos - 1
      if (pos < prev + 3) then
         token = toml_token(token_kind%invalid, prev, len(lexer%chunk))
         return
      end if

      do it = 1, 2
         if (match(lexer, pos+3, char_kind%squote)) pos = pos + 1
      end do

      valid = .true.
      do it = prev + 3, pos - 1
         ch = peek(lexer, it)
         valid = valid .and. valid_string(ch)
      end do

      token = toml_token(merge(token_kind%mliteral, token_kind%invalid, valid), prev, pos+2)
      return
   end if

   valid = .true.

   do while(pos < len(lexer%chunk))
      pos = pos + 1
      ch = peek(lexer, pos)
      valid = valid .and. valid_string(ch)
      if (ch == char_kind%squote) exit
      if (ch == char_kind%newline) then
         pos = pos - 1
         valid = .false.
         exit
      end if
   end do

   valid = valid .and. peek(lexer, pos) == char_kind%squote .and. pos /= prev
   token = toml_token(merge(token_kind%literal, token_kind%invalid, valid), prev, pos)
end subroutine next_sstring

!> Process next string token, can produce normal string and multiline string tokens
subroutine next_dstring(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   character(1, tfc) :: ch
   character(*, tfc), parameter :: hexnum = "0123456789abcdefABCDEF", valid_escape = "betnfr\"""
   integer :: prev, pos, expect, it, hex
   logical :: escape, valid, space

   prev = lexer%pos
   pos = lexer%pos
   hex = 0

   if (all(match(lexer, [pos+1, pos+2], char_kind%dquote))) then
      pos = pos + 3

      do
         it = strstr(lexer%chunk(pos:), char_kind%dquote3)
         pos = it + pos - 1
         if (pos < prev + 3 .or. it == 0) then
            token = toml_token(token_kind%invalid, prev, len(lexer%chunk))
            return
         end if

         if (match(lexer, pos-1, char_kind%backslash)) then
            pos = pos + 1
            cycle
         end if

         do it = 1, 2
            if (match(lexer, pos+3, char_kind%dquote)) pos = pos + 1
         end do
         exit
      end do

      valid = .true.
      escape = .false.
      space = .false.
      expect = 0

      do it = prev + 3, pos - 1
         ch = peek(lexer, it)
         if (escape) then
            space = verify(ch, char_kind%space//char_kind%tab//&
               & char_kind%carriage_return//char_kind%newline) == 0
         end if
         if (space) then
            escape = .false.
            if (ch == char_kind%newline) then
               if (expect > 0) expect = expect - 1
               space = .false.
               cycle
            end if
            if (verify(ch, char_kind%space//char_kind%tab) == 0 .and. expect == 0) cycle
            if (ch == char_kind%carriage_return) then
               expect = 1
               cycle
            end if
            valid = .false.
            space = .false.
            expect = 0
            cycle
         end if
         valid = valid .and. valid_string(ch)
         if (escape) then
            escape = .false.
            space = .false.
            if (verify(ch, valid_escape) == 0) cycle
            if (ch == "x") then
               expect = 2
               hex = pos + 1
               cycle
            end if
            if (ch == "u") then
               expect = 4
               hex = it + 1
               cycle
            end if
            if (ch == "U") then
               expect = 8
               hex = it + 1
               cycle
            end if
            valid = .false.
            cycle
         end if
         if (expect > 0) then
            expect = expect - 1
            valid = valid .and. verify(ch, hexnum) == 0
            if (expect == 0) valid = valid .and. verify_ucs(lexer%chunk(hex:pos))
            cycle
         end if
         escape = ch == char_kind%backslash
      end do

      ! Check for any unfinished escape sequences
      valid = valid .and. expect == 0 .and. .not.(escape.or.space)

      token = toml_token(merge(token_kind%mstring, token_kind%invalid, valid), prev, pos+2)
      return
   end if

   valid = .true.
   escape = .false.
   expect = 0

   do while(pos < len(lexer%chunk))
      pos = pos + 1
      ch = peek(lexer, pos)
      valid = valid .and. valid_string(ch)
      if (escape) then
         escape = .false.
         if (verify(ch, valid_escape) == 0) cycle
         if (ch == "x") then
            expect = 2
            hex = pos + 1
            cycle
         end if
         if (ch == "u") then
            expect = 4
            hex = pos + 1
            cycle
         end if
         if (ch == "U") then
            expect = 8
            hex = pos + 1
            cycle
         end if
         valid = .false.
         cycle
      end if
      if (expect > 0) then
         expect = expect - 1
         valid = valid .and. verify(ch, hexnum) == 0
         if (expect == 0) valid = valid .and. verify_ucs(lexer%chunk(hex:pos))
         cycle
      end if
      escape = ch == char_kind%backslash
      if (ch == char_kind%dquote) exit
      if (ch == char_kind%newline) then
         pos = pos - 1
         valid = .false.
         exit
      end if
   end do

   valid = valid .and. peek(lexer, pos) == char_kind%dquote .and. pos /= prev
   token = toml_token(merge(token_kind%string, token_kind%invalid, valid), prev, pos)
end subroutine next_dstring

!> Validate characters in string, non-printable characters are invalid in this context
pure function valid_string(ch) result(valid)
   character(1, tfc), intent(in) :: ch
   logical :: valid

   character(1, tfc), parameter :: x00 = achar(int(z"00")), x08 = achar(int(z"08")), &
      & x0b = achar(int(z"0b")), x1f = achar(int(z"1f")), x7f = achar(int(z"7f"))

   valid = &
      & .not.(x00 <= ch .and. ch <= x08) .and. &
      & .not.(x0b <= ch .and. ch <= x1f) .and. &
      & ch /= x7f
end function

!> Process next bare key token, produces keypath tokens.
subroutine next_keypath(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   logical :: valid
   integer :: prev, pos
   character(1, tfc) :: ch

   prev = lexer%pos
   pos = lexer%pos
   ch = peek(lexer, pos)

   valid = (tfc_"A" <= ch .and. ch <= tfc_"Z") &
      & .or. (tfc_"a" <= ch .and. ch <= tfc_"z") &
      & .or. (verify(ch, char_kind%literal) == 0)
   do while(verify(peek(lexer, pos+1), terminated//char_kind%dot) > 0)
      pos = pos + 1
      ch = peek(lexer, pos)

      if (tfc_"A" <= ch .and. ch <= tfc_"Z") cycle
      if (tfc_"a" <= ch .and. ch <= tfc_"z") cycle
      if (verify(ch, char_kind%literal) == 0) cycle

      valid = .false.
      cycle
   end do

   token = toml_token(merge(token_kind%keypath, token_kind%invalid, valid), prev, pos)
end subroutine next_keypath

!> Identify literal values, produces integer, float, boolean, and datetime tokens.
subroutine next_literal(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   integer :: prev, pos
   integer, parameter :: offset(*) = [0, 1, 2, 3, 4, 5]
   character(1, tfc), parameter :: &
      & true(4) = ["t", "r", "u", "e"], false(5) = ["f", "a", "l", "s", "e"]

   prev = lexer%pos
   pos = lexer%pos

   select case(peek(lexer, pos))
   case("t")
      if (match_all(lexer, pos+offset(:4), true) .and. &
         & verify(peek(lexer, pos+4), terminated) == 0) then
         token = toml_token(token_kind%bool, prev, pos+3)
         return
      end if

   case("f")
      if (match_all(lexer, pos+offset(:5), false) .and. &
         & verify(peek(lexer, pos+5), terminated) == 0) then
         token = toml_token(token_kind%bool, prev, pos+4)
         return
      end if

   case default
      call next_datetime(lexer, token)
      if (token%kind == token_kind%datetime) return

      call next_integer(lexer, token)
      if (token%kind == token_kind%int) return

      call next_float(lexer, token)
      if (token%kind == token_kind%float) return

   end select

   ! If the current token is invalid, advance to the next terminator
   do while(verify(peek(lexer, pos+1), terminated) > 0)
      pos = pos + 1
   end do
   token = toml_token(token_kind%invalid, prev, pos)
end subroutine next_literal

!> Process integer tokens and binary, octal, and hexadecimal literals.
subroutine next_integer(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   character(*, tfc), parameter :: toml_base(4) = [&
      & "0123456789abcdefABCDEF", &
      & "0123456789000000000000", &
      & "0123456700000000000000", &
      & "0100000000000000000000"]
   integer, parameter :: b10 = 2, b16 = 1, b8 = 3, b2 = 4

   character(1, tfc) :: ch
   integer :: prev, pos, base
   logical :: underscore, okay

   prev = lexer%pos
   pos = lexer%pos
   okay = .true.
   underscore = .true.
   base = b10

   if (any(match(lexer, pos, ["+", "-"]))) then
      pos = pos + 1
   end if

   if (match(lexer, pos, "0")) then
      select case(peek(lexer, pos+1))
      case("x")
         okay = pos == prev
         base = b16
         pos = pos + 2
      case("o")
         okay = pos == prev
         base = b8
         pos = pos + 2
      case("b")
         okay = pos == prev
         base = b2
         pos = pos + 2
      case(char_kind%space, char_kind%tab, char_kind%newline, char_kind%carriage_return, &
         & char_kind%hash, char_kind%rbrace, char_kind%rbracket, char_kind%comma)
         token = toml_token(token_kind%int, prev, pos)
         return
      case default
         do while(verify(peek(lexer, pos), terminated) > 0)
            pos = pos + 1
         end do
         token = toml_token(token_kind%invalid, prev, pos-1)
         return
      end select
   end if


   do while(pos <= len(lexer%chunk))
      ch = peek(lexer, pos)
      if (ch == "_") then
         if (underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         underscore = .true.
         pos = pos + 1
         cycle
      end if

      if (verify(ch, toml_base(base)) == 0) then
         pos = pos + 1
         underscore = .false.
         cycle
      end if

      okay = okay .and. verify(ch, terminated) == 0
      exit
   end do

   okay = .not.underscore .and. okay
   token = toml_token(merge(token_kind%int, token_kind%invalid, okay), prev, pos-1)
end subroutine next_integer

!> Process float tokens.
subroutine next_float(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   integer :: prev, pos
   logical :: plus_minus, underscore, point, expo, okay, zero, first
   character(1, tfc) :: ch
   integer, parameter :: offset(*) = [0, 1, 2]
   character(1, tfc), parameter :: nan(3) = ["n", "a", "n"], inf(3) = ["i", "n", "f"]

   prev = lexer%pos
   pos = lexer%pos
   point = .false.
   expo = .false.
   zero = .false.
   first = .true.
   underscore = .true.
   plus_minus = any(match(lexer, pos, ["+", "-"]))
   if (plus_minus) pos = pos + 1

   if (match_all(lexer, pos+offset, nan) .and. &
      & verify(peek(lexer, pos+3), terminated) == 0) then
      token = toml_token(token_kind%float, prev, pos+2)
      return
   end if

   if (match_all(lexer, pos+offset, inf) .and. &
      & verify(peek(lexer, pos+3), terminated) == 0) then
      token = toml_token(token_kind%float, prev, pos+2)
      return
   end if

   do while(pos <= len(lexer%chunk))
      ch = peek(lexer, pos)
      if (ch == "_") then
         if (underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         underscore = .true.
         pos = pos + 1
         cycle
      end if

      if (ch == ".") then
         if (point .or. expo .or. underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         zero = .false.
         underscore = .true.
         point = .true.
         pos = pos + 1
         cycle
      end if

      if (ch == "e" .or. ch == "E") then
         if (expo .or. underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         zero = .false.
         underscore = .true.
         expo = .true.
         pos = pos + 1
         cycle
      end if

      if (ch == "+" .or. ch == "-") then
         if (.not.any(match(lexer, pos-1, ["e", "E"]))) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         underscore = .true.
         pos = pos + 1
         cycle
      end if

      if (verify(ch, "0123456789") == 0) then
         if (zero) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         zero = first .and. ch == "0"
         first = .false.
         pos = pos + 1
         underscore = .false.
         cycle
      end if

      exit
   end do

   okay = .not.underscore .and. (expo .or. point)
   token = toml_token(merge(token_kind%float, token_kind%invalid, okay), prev, pos-1)
end subroutine next_float

!> Find the next datetime expression
subroutine next_datetime(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   logical :: has_date, has_time, has_millisec, has_local, okay, has_seconds
   logical :: has_explicit_time
   integer :: prev, pos, it, time_len
   integer, parameter :: offset(*) = [(it, it = 0, 10)], &
      & offset_date = 10, offset_time = 8, offset_time_no_sec = 5, offset_local = 6
   character(*, tfc), parameter :: num = "0123456789"

   prev = lexer%pos
   pos = lexer%pos

   has_date = valid_date(peek(lexer, pos+offset(:offset_date)))
   has_explicit_time = has_date .and. pos + offset_date + 1 <= len(lexer%chunk) &
      & .and. verify(peek(lexer, pos+offset_date), "Tt ") == 0 &
      & .and. verify(peek(lexer, pos+offset_date + 1), num) == 0
   if (has_explicit_time) then
      pos = pos + offset_date + 1
   end if

   ! Validate a strict TOML time with seconds.
   call valid_time(peek(lexer, pos+offset(:offset_time)), has_time, has_seconds)
   if (has_explicit_time .and. .not.has_time) then
      token = toml_token(token_kind%invalid, prev, prev)
      return
   end if

   if (has_time) then
      time_len = merge(offset_time, offset_time_no_sec, has_seconds)
      pos = pos + time_len - 1
      if (has_seconds .and. match(lexer, pos+1, char_kind%dot)) then
         it = 1
         do while(verify(peek(lexer, pos+it+1), num) == 0)
            it = it + 1
         end do
         has_millisec = it > 1
         if (.not.has_millisec) then
            token = toml_token(token_kind%invalid, prev, prev)
            return
         end if

         pos = pos + it
      end if

      has_local = valid_local(peek(lexer, pos+offset(:offset_local)+1))
      if (has_local) then
         if (.not.has_date) then
            token = toml_token(token_kind%invalid, prev, prev)
            return
         end if
         pos = pos + offset_local
      else if (verify(peek(lexer, pos+1), "zZ") == 0) then
         pos = pos + 1
      end if
   end if

   if (.not.(has_time.or.has_date)) then
      token = toml_token(token_kind%invalid, prev, prev)
      return
   end if

   if (.not.has_time.and.has_date) pos = pos + offset_date - 1
   okay = verify(peek(lexer, pos+1), terminated) == 0 .and. pos <= len(lexer%chunk)
   token = toml_token(merge(token_kind%datetime, token_kind%invalid, okay), prev, pos)
end subroutine next_datetime

!> Validate a string as date
pure function valid_date(string) result(valid)
   !> Input string, 10 characters
   character(1, tfc), intent(in) :: string(:)
   !> Valid date
   logical :: valid

   integer :: it, val
   character(*, tfc), parameter :: num = "0123456789"
   integer :: year, month, day, mday
   logical :: leap

   valid = .false.
   if (any(string([5, 8]) /= "-")) return

   year = 0
   do it = 1, 4
      val = scan(num, string(it)) - 1
      if (val < 0) return
      year = year * 10 + val
   end do

   month = 0
   do it = 6, 7
      val = scan(num, string(it)) - 1
      if (val < 0) return
      month = month * 10 + val
   end do

   day = 0
   do it = 9, 10
      val = scan(num, string(it)) - 1
      if (val < 0) return
      day = day * 10 + val
   end do

   mday = 0
   select case(month)
   case(1, 3, 5, 7, 8, 10, 12)
      mday = 31
   case(2)
      leap = mod(year, 4) == 0 .and. (mod(year, 100) /= 0 .or. mod(year, 400) == 0)
      mday = merge(29, 28, leap)
   case(4, 6, 9, 11)
      mday = 30
   end select
   valid = day >= 1 .and. day <= mday
end function valid_date


!> Validate a string as time (HH:MM[:SS])
subroutine valid_time(string, valid, has_seconds)
   !> Input string, 5 characters (HH:MM) or 8 characters (HH:MM:SS)
   character(1, tfc), intent(in) :: string(:)
   !> Valid time
   logical, intent(out) :: valid
   !> Whether the time has seconds
   logical, intent(out) :: has_seconds

   integer :: it, val
   character(*, tfc), parameter :: num = "0123456789"
   integer :: hour, minute, second

   valid = .false.
   has_seconds = .false.
   if (size(string) < 5 .or. string(3) /= ":") return

   hour = 0
   do it = 1, 2
      val = scan(num, string(it)) - 1
      if (val < 0) return
      hour = hour * 10 + val
   end do

   minute = 0
   do it = 4, 5
      val = scan(num, string(it)) - 1
      if (val < 0) return
      minute = minute * 10 + val
   end do

   if (size(string) >= 8 .and. string(6) == ":") then
      second = 0
      do it = 7, 8
         val = scan(num, string(it)) - 1
         if (val < 0) return
         second = second * 10 + val
      end do
      if (second < 0 .or. second >= 60) return
      has_seconds = .true.
   end if

   valid = minute >= 0 .and. minute < 60 &
      & .and. hour >= 0 .and. hour < 24
end subroutine valid_time


!> Validate a string as timezone
function valid_local(string) result(valid)
   !> Input string, 6 characters
   character(1, tfc), intent(in) :: string(:)
   !> Valid timezone
   logical :: valid

   integer :: it, val
   character(*, tfc), parameter :: num = "0123456789"
   integer :: hour, minute

   valid = .false.
   if (string(4) /= ":" .or. all(string(1) /= ["+", "-"])) return

   hour = 0
   do it = 2, 3
      val = scan(num, string(it)) - 1
      if (val < 0) return
      hour = hour * 10 + val
   end do

   minute = 0
   do it = 5, 6
      val = scan(num, string(it)) - 1
      if (val < 0) return
      minute = minute * 10 + val
   end do

   valid = minute >= 0 .and. minute < 60 &
      & .and. hour >= 0 .and. hour < 24
end function valid_local


!> Show current character
elemental function peek(lexer, pos) result(ch)
   !> Instance of the lexer
   type(toml_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos
   !> Character found
   character(1, tfc) :: ch

   if (pos <= len(lexer%chunk)) then
      ch = lexer%chunk(pos:pos)
   else
      ch = char_kind%space
   end if
end function peek

!> Compare a character
elemental function match(lexer, pos, kind)
   !> Instance of the lexer
   type(toml_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos
   !> Character to compare against
   character(1, tfc), intent(in) :: kind
   !> Characters match
   logical :: match

   match = peek(lexer, pos) == kind
end function match

!> Compare a set of characters
pure function match_all(lexer, pos, kind) result(match)
   !> Instance of the lexer
   type(toml_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos(:)
   !> Character to compare against
   character(1, tfc), intent(in) :: kind(:)
   !> Characters match
   logical :: match

   match = all(peek(lexer, pos) == kind)
end function match_all

pure function strstr(string, pattern) result(res)
   character(*, tfc), intent(in) :: string
   character(*, tfc), intent(in) :: pattern
   integer :: lps_array(len(pattern))
   integer :: res, s_i, p_i, length_string, length_pattern
   res = 0
   length_string = len(string)
   length_pattern = len(pattern)

   if (length_pattern > 0 .and. length_pattern <= length_string) then
      lps_array = compute_lps(pattern)

      s_i = 1
      p_i = 1
      do while(s_i <= length_string)
         if (string(s_i:s_i) == pattern(p_i:p_i)) then
            if (p_i == length_pattern) then
               res = s_i - length_pattern + 1
               exit
            end if
            s_i = s_i + 1
            p_i = p_i + 1
         else if (p_i > 1) then
            p_i = lps_array(p_i - 1) + 1
         else
            s_i = s_i + 1
         end if
      end do
   end if

contains

   pure function compute_lps(string) result(lps_array)
      character(*, tfc), intent(in) :: string
      integer :: lps_array(len(string))
      integer :: i, j, length_string

      length_string = len(string)

      if (length_string > 0) then
         lps_array(1) = 0

         i = 2
         j = 1
         do while (i <= length_string)
            if (string(j:j) == string(i:i)) then
               lps_array(i) = j
               i = i + 1
               j = j + 1
            else if (j > 1) then
               j = lps_array(j - 1) + 1
            else
               lps_array(i) = 0
               i = i + 1
            end if
         end do
      end if

   end function compute_lps

end function strstr

!> Extract string value of token, works for keypath, string, multiline string, literal,
!> and mulitline literal tokens.
subroutine extract_string(lexer, token, string)
   !> Instance of the lexer
   class(toml_lexer), intent(in) :: lexer
   !> Token to extract string value from
   type(toml_token), intent(in) :: token
   !> String value of token
   character(kind=tfc, len=:), allocatable, intent(out) :: string

   integer :: it, length, j
   logical :: escape, leading_newline
   character(1, tfc) :: ch
   character(kind=tfc, len=:), allocatable :: raw

   length = token%last - token%first + 1

   select case(token%kind)
   case(token_kind%string)
      string = ""
      escape = .false.
      it = token%first + 1
      do while(it <= token%last - 1)
         ch = peek(lexer, it)
         if (escape) then
            escape = .false.
            select case(ch)
            case("""", "\"); string = string // ch
            case("b"); string = string // TOML_BACKSPACE
            case("e"); string = string // TOML_ESC
            case("t"); string = string // TOML_TABULATOR
            case("n"); string = string // TOML_NEWLINE
            case("r"); string = string // TOML_CARRIAGE_RETURN
            case("f"); string = string // TOML_FORMFEED
            case("x"); string = string // convert_ucs(lexer%chunk(it+1:it+2)); it = it + 2
            case("u"); string = string // convert_ucs(lexer%chunk(it+1:it+4)); it = it + 4
            case("U"); string = string // convert_ucs(lexer%chunk(it+1:it+8)); it = it + 8
            case default; string = string // ch
            end select
            it = it + 1
            cycle
         end if
         escape = ch == char_kind%backslash
         if (.not.escape) string = string // ch
         it = it + 1
      end do
   case(token_kind%mstring)
      leading_newline = peek(lexer, token%first+3) == char_kind%newline
      allocate(character(len=token%last - token%first - merge(5, 4, leading_newline), kind=tfc) :: raw)
      raw = lexer%chunk(token%first + merge(4, 3, leading_newline):token%last - 3)
      string = ""
      escape = .false.
      it = 1
      do while(it <= len(raw))
         ch = raw(it:it)
         if (escape) then
            escape = .false.
            if (any(ch == [char_kind%space, char_kind%tab]) .and. it < len(raw)) then
               j = it
               do while(j <= len(raw) .and. any(raw(j:j) == [char_kind%space, char_kind%tab]))
                  j = j + 1
               end do
               if (j <= len(raw) .and. (raw(j:j) == char_kind%newline .or. raw(j:j) == char_kind%carriage_return)) then
                  ch = raw(j:j)
                  it = j
               end if
            end if
            if (ch == char_kind%newline .or. ch == char_kind%carriage_return) then
               if (ch == char_kind%carriage_return .and. it < len(raw) .and. raw(it+1:it+1) == char_kind%newline) then
                  it = it + 1
               end if
               it = it + 1
               do while(it <= len(raw) .and. any(raw(it:it) == [char_kind%space, char_kind%tab, &
                  & char_kind%newline, char_kind%carriage_return]))
                  if (raw(it:it) == char_kind%carriage_return .and. it < len(raw) .and. raw(it+1:it+1) == char_kind%newline) then
                     it = it + 2
                  else
                     it = it + 1
                  end if
               end do
               cycle
            end if
            select case(ch)
            case("""", "\"); string = string // ch
            case("b"); string = string // TOML_BACKSPACE
            case("e"); string = string // TOML_ESC
            case("t"); string = string // TOML_TABULATOR
            case("n"); string = string // TOML_NEWLINE
            case("r"); string = string // TOML_CARRIAGE_RETURN
            case("f"); string = string // TOML_FORMFEED
            case("x"); string = string // convert_ucs(raw(it+1:it+2)); it = it + 2
            case("u"); string = string // convert_ucs(raw(it+1:it+4)); it = it + 4
            case("U"); string = string // convert_ucs(raw(it+1:it+8)); it = it + 8
            case default; string = string // ch
            end select
            it = it + 1
            cycle
         end if
         escape = ch == char_kind%backslash
         if (.not.escape) string = string // ch
         it = it + 1
      end do
   case(token_kind%literal)
      allocate(character(len=length - 2, kind=tfc) :: string)
      string = lexer%chunk(token%first+1:token%last-1)
   case(token_kind%mliteral)
      leading_newline = peek(lexer, token%first+3) == char_kind%newline
      allocate(character(len=length - merge(7, 6, leading_newline), kind=tfc) :: string)
      string = lexer%chunk(token%first+merge(4, 3, leading_newline):token%last-3)
   case(token_kind%keypath)
      allocate(character(len=length, kind=tfc) :: string)
      string = lexer%chunk(token%first:token%last)
   end select

end subroutine extract_string

!> Extract integer value of token
subroutine extract_integer(lexer, token, val)
   !> Instance of the lexer
   class(toml_lexer), intent(in) :: lexer
   !> Token to extract integer value from
   type(toml_token), intent(in) :: token
   !> Integer value of token
   integer(tfi), intent(out) :: val

   integer :: first, base, it, tmp
   logical :: minus
   character(1, tfc) :: ch
   character(*, tfc), parameter :: num = "0123456789abcdef"

   if (token%kind /= token_kind%int) return

   val = 0
   base = 10
   first = token%first

   if (any(peek(lexer, first) == ["+", "-"])) first = first + 1

   if (peek(lexer, first) == "0") then
      select case(peek(lexer, first + 1))
      case("x")
         first = first + 2
         base = 16
      case("o")
         first = first + 2
         base = 8
      case("b")
         first = first + 2
         base = 2
      case default
         return
      end select
   end if

   minus = match(lexer, token%first, char_kind%minus)

   do it = first, token%last
      ch = peek(lexer, it)
      if ("A" <= ch .and. ch <= "Z") ch = achar(iachar(ch) - iachar("A") + iachar("a"))
      tmp = scan(num(:abs(base)), ch) - 1
      if (tmp < 0) cycle
      val = val * base + merge(-tmp, tmp, minus)
   end do
end subroutine extract_integer

!> Extract floating point value of token
subroutine extract_float(lexer, token, val)
   ! Not useable since unsupported with GFortran on some platforms (MacOS/ppc)
   ! use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quite_nan, &
   !    & ieee_positive_inf, ieee_negative_inf
   !> Instance of the lexer
   class(toml_lexer), intent(in) :: lexer
   !> Token to extract floating point value from
   type(toml_token), intent(in) :: token
   !> Floating point value of token
   real(tfr), intent(out) :: val

   integer :: first, it, ic
   character(len=token%last - token%first + 1) :: buffer
   character(1, tfc) :: ch

   if (token%kind /= token_kind%float) return

   first = token%first

   if (any(peek(lexer, first) == ["+", "-"])) first = first + 1

   if (match(lexer, first, "n")) then
      ! val = ieee_value(val, ieee_quite_nan)
      buffer = "NaN"
      read(buffer, *, iostat=ic) val
      return
   end if

   if (match(lexer, first, "i")) then
      ! val = ieee_value(val, ieee_positive_inf)
      buffer = "Inf"
      read(buffer, *, iostat=ic) val
      if (match(lexer, token%first, char_kind%minus)) val = -val
      return
   end if

!   ival = 0
!   idot = 0
!
!   do it = first, token%last
!      ch = peek(lexer, it)
!      if (any(ch == [".", "e", "E"])) exit
!      tmp = scan(num(:base), ch) - 1
!      if (tmp < 0) cycle
!      ival = ival * base + tmp
!   end do
!   first = it
!
!   if (ch == ".") then
!      idot = 0
!      do it = first, token%last
!         ch = peek(lexer, it)
!         if (any(ch == ["e", "E"])) exit
!         tmp = scan(num(:base), ch) - 1
!         if (tmp < 0) cycle
!         idot = idot + 1
!         ival = ival * base + tmp
!      end do
!      first = it
!   end if
!
!   expo = 0
!   if (any(ch == ["e", "E"])) then
!      first = first + 1
!      do it = first, token%last
!         ch = peek(lexer, it)
!         tmp = scan(num(:base), ch) - 1
!         if (tmp < 0) cycle
!         expo = expo * base + tmp
!      end do
!      if (match(lexer, first, char_kind%minus)) expo = -expo
!   end if
!   expo = expo - idot
!   val = ival * 10.0_tfr ** expo  ! FIXME
!
!   if (match(lexer, token%first, char_kind%minus)) val = -val

   ic = 0
   do it = token%first, token%last
      ch = peek(lexer, it)
      if (ch == "_") cycle
      ic = ic + 1
      buffer(ic:ic) = ch
   end do

   read(buffer(:ic), *, iostat=it) val
end subroutine extract_float

!> Extract boolean value of token
subroutine extract_bool(lexer, token, val)
   !> Instance of the lexer
   class(toml_lexer), intent(in) :: lexer
   !> Token to extract boolean value from
   type(toml_token), intent(in) :: token
   !> Boolean value of token
   logical, intent(out) :: val

   if (token%kind /= token_kind%bool) return

   val = peek(lexer, token%first) == "t"
end subroutine extract_bool

!> Extract datetime value of token
subroutine extract_datetime(lexer, token, val)
   !> Instance of the lexer
   class(toml_lexer), intent(in) :: lexer
   !> Token to extract datetime value from
   type(toml_token), intent(in) :: token
   !> Datetime value of token
   type(toml_datetime), intent(out) :: val

   if (token%kind /= token_kind%datetime) return

   val = toml_datetime(lexer%chunk(token%first:token%last))
end subroutine extract_datetime


!> Push a new scope onto the lexer stack and record the token
pure subroutine push_back(lexer, scope, token)
   type(toml_lexer), intent(inout) :: lexer
   integer, intent(in) :: scope
   integer, intent(in) :: token

   lexer%top = lexer%top + 1
   if (lexer%top > size(lexer%stack)) call resize(lexer%stack)
   lexer%stack(lexer%top) = stack_item(scope, token)
end subroutine push_back

!> Pop a scope from the lexer stack in case the topmost scope matches the requested scope
subroutine pop(lexer, scope)
   type(toml_lexer), intent(inout) :: lexer
   integer, intent(in) :: scope

   if (lexer%top > 0) then
      if (lexer%stack(lexer%top)%scope == scope) lexer%top = lexer%top - 1
   end if
end subroutine pop

!> Peek at the topmost scope on the lexer stack
pure function view_scope(lexer) result(scope)
   type(toml_lexer), intent(in) :: lexer
   integer :: scope

   if (lexer%top > 0) then
      scope = lexer%stack(lexer%top)%scope
   else
      scope = lexer_scope%table
   end if
end function view_scope


!> Reallocate list of scopes
pure subroutine resize_scope(var, n)
   !> Instance of the array to be resized
   type(stack_item), allocatable, intent(inout) :: var(:)
   !> Dimension of the final array size
   integer, intent(in), optional :: n

   type(stack_item), allocatable :: tmp(:)
   integer :: this_size, new_size
   integer, parameter :: initial_size = 8

   if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
   else
      this_size = initial_size
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(var(new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
   end if

end subroutine resize_scope


!> Extract information about the source
subroutine get_info(lexer, meta, output)
   !> Instance of the lexer
   class(toml_lexer), intent(in) :: lexer
   !> Query about the source
   character(*, tfc), intent(in) :: meta
   !> Metadata about the source
   character(kind=tfc, len=:), allocatable, intent(out) :: output

   select case(meta)
   case("source")
      output = lexer%chunk // TOML_NEWLINE
   case("filename")
      if (allocated(lexer%filename)) output = lexer%filename
   end select
end subroutine get_info


function hex_to_int(hex) result(val)
   character(*, tfc), intent(in) :: hex
   integer(tfi) :: val
   integer :: i
   character(1, tfc) :: ch
   character(*, tfc), parameter :: hex_digits = "0123456789abcdef"

   val = 0_tfi
   do i = 1, len(hex)
      ch = hex(i:i)
      if ("A" <= ch .and. ch <= "Z") ch = achar(iachar(ch) - iachar("A") + iachar("a"))
      val = val * 16 + max(index(hex_digits, ch) - 1, 0)
   end do
end function hex_to_int


function verify_ucs(escape) result(valid)
   character(*, tfc), intent(in) :: escape
   logical :: valid
   integer(tfi) :: code

   code = hex_to_int(escape)

   valid = code >= 0_tfi .and. code < int(z"7FFFFFFF", tfi) &
      & .and. (code < int(z"d800", tfi) .or. code > int(z"dfff", tfi)) &
      & .and. (code < int(z"fffe", tfi) .or. code > int(z"ffff", tfi))
end function verify_ucs


function convert_ucs(escape) result(str)
   character(*, tfc), intent(in) :: escape
   character(:, tfc), allocatable :: str
   integer(tfi) :: code

   code = hex_to_int(escape)

   select case(code)
   case(int(z"00000000", tfi):int(z"0000007f", tfi))
      str = achar(code, kind=tfc)
   case(int(z"00000080", tfi):int(z"000007ff", tfi))
      str = &
         achar(ior(int(z"c0", tfi), ishft(code, -6)), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(code, int(z"3f", tfi))), kind=tfc)
   case(int(z"00000800", tfi):int(z"0000ffff", tfi))
      str = &
         achar(ior(int(z"e0", tfi), ishft(code, -12)), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -6), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(code, int(z"3f", tfi))), kind=tfc)
   case(int(z"00010000", tfi):int(z"001fffff", tfi))
      str = &
         achar(ior(int(z"f0", tfi), ishft(code, -18)), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -12), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -6), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(code, int(z"3f", tfi))), kind=tfc)
   case(int(z"00200000", tfi):int(z"03ffffff", tfi))
      str = &
         achar(ior(int(z"f8", tfi), ishft(code, -24)), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -18), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -12), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -6), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(code, int(z"3f", tfi))), kind=tfc)
   case(int(z"04000000", tfi):int(z"7fffffff", tfi))
      str = &
         achar(ior(int(z"fc", tfi), ishft(code, -30)), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -24), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -18), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -12), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(ishft(code, -6), int(z"3f", tfi))), kind=tfc) // &
         achar(ior(int(z"80", tfi), iand(code, int(z"3f", tfi))), kind=tfc)
   end select
end function convert_ucs


end module tomlf_de_lexer


!>>>>> ././src/ffmt_format.f90
module ffmt_format
!! Formatting a whole source text: the main library entry point.
use ffmt_tokens
use ffmt_lexer, only: tokenize
use ffmt_statements, only: statement_t, statement_view_t, set_view, SK_CODE, SK_PREPROC
use ffmt_source, only: source_t, parse_source, set_views
use ffmt_structure, only: analyze_structure, diagnostic_t, structure_t
use ffmt_layout, only: layout
use ffmt_whitespace, only: compute_gaps
use ffmt_case, only: compute_case
use ffmt_relational, only: convert_relational, canonical_relational
use ffmt_linebreak, only: split_long_lines, long_code_lines, MAX_CONTINUATIONS
use ffmt_options, only: format_options_t
use ffmt_use_sort, only: sort_use, is_use_statement, only_items, sorted_order
use ffmt_strings, only: to_s, string_t, string_builder_t, lower_char, lowercase
implicit none
private

public :: format_text, same_code, longest_code
public :: diagnostic_t, format_options_t

integer, parameter, public :: FMT_OK = 0              !! formatted
integer, parameter, public :: FMT_ERR_INTERNAL = 1    !! formatting would have changed the code; the text is returned unchanged

contains

subroutine format_text(src, opts, out, stat, errmsg, diags)
    !! Format `src`. On success `out` is the formatted text and `stat` is `FMT_OK`.
    !!
    !! As a safety check, the code tokens of the result are compared with those of the input. If they
    !! differ, which would be a bug in the formatter, `out` is the unchanged input and `stat` is
    !! `FMT_ERR_INTERNAL`.
    character(len=*), intent(in) :: src                    !! source text to format
    type(format_options_t), intent(in) :: opts             !! formatting options
    character(len=:), allocatable, intent(out) :: out      !! the formatted text
    integer, intent(out) :: stat                           !! `FMT_OK` or `FMT_ERR_INTERNAL`
    character(len=:), allocatable, intent(out) :: errmsg   !! error message; unallocated if `stat` is `FMT_OK`
    type(diagnostic_t), allocatable, intent(out), optional :: diags(:)   !! problems found while formatting

    ! each pass splits each long line once, and a statement gets at most MAX_CONTINUATIONS
    ! continuation lines, so this many passes split every line that can be split
    integer, parameter :: MAX_PASSES = MAX_CONTINUATIONS + 1
    type(diagnostic_t), allocatable :: d(:), unused(:)
    type(token_list_t) :: tokens
    integer, allocatable :: long(:), changed(:, :)
    character(len=:), allocatable :: prepared, split
    integer :: k, nsplit

    ! a sorted `only:` list is written on one line for splitting, or broken to fit if splitting is off
    prepared = convert_relational(sort_use(src, opts%sort_use, opts%sort_use_only, &
                                           merge(0, opts%line_length, opts%split_lines)), opts%relational)
    call format_pass(prepared, opts, out, d, tokens)
    ! no code longer than the limit, as most files: nothing to split, and no need to look for
    ! code lines that are too long
    if (opts%split_lines .and. opts%line_length > 0 .and. too_long(out, opts%line_length)) then
        ! split long lines and format again, until every line fits or nothing more can be split; only
        ! the statements that were split need laying out again, unless one of them depends on the
        ! lines around it
        do k = 1, MAX_PASSES
            split = split_long_lines(out, opts%line_length, opts%continuation_indent, nsplit, changed)
            if (nsplit == 0) exit
            if (allocated(changed) .and. opts%comment_spacing < 0) then
                out = relayout(split, changed, opts)
            else
                call format_pass(split, opts, out, unused)
            end if
            if (.not. too_long(out, opts%line_length)) exit
        end do
        if (too_long(out, opts%line_length)) then
            long = long_code_lines(out, opts%line_length)
            do k = 1, size(long)
                d = [d, diagnostic_t(long(k), 'line '//to_s(long(k))//' of the formatted file is longer than '// &
                                     to_s(opts%line_length)//' characters and cannot be split')]
            end do
        end if
    end if
    if (opts%comment_spacing >= 0) then
        ! comments are kept where they line up in the input; once the code is formatted, some of
        ! them may no longer line up, so decide again on the result (which is then stable)
        split = out
        call format_pass(split, opts, out, unused)
    end if
    d = [limit_diagnostics(src), d]
    if (present(diags)) diags = d

    stat = FMT_OK
    ! the tokens of the first pass are those of `src` if sorting and relational spelling changed nothing
    if (len(prepared) /= len(src) .or. prepared /= src) call tokenize(src, tokens)
    if (.not. same_code(src, out, opts%sort_use .or. opts%sort_use_only, tokens)) then
        stat = FMT_ERR_INTERNAL
        errmsg = 'internal error: formatting would change the code, so the file was left unchanged'
        out = src
    end if
end subroutine format_text

subroutine format_pass(src, opts, out, diags, src_tokens)
    !! One pass of the formatter: structure, whitespace, case and layout.
    character(len=*), intent(in) :: src                         !! source text
    type(format_options_t), intent(in) :: opts                  !! formatting options
    character(len=:), allocatable, intent(out) :: out           !! the formatted text
    type(diagnostic_t), allocatable, intent(out) :: diags(:)    !! problems found in this pass
    type(token_list_t), intent(out), optional :: src_tokens     !! the tokens of `src`

    type(source_t) :: source
    type(statement_view_t), allocatable :: views(:)
    type(structure_t) :: st
    integer, allocatable :: long_lines(:)
    integer :: k

    call parse_source(src, source)
    call set_views(source, views)
    call analyze_structure(source, views, st, diags)
    out = layout(source, st, compute_gaps(source, views, opts%spaces, opts%comment_spacing), &
                 compute_case(source, views, opts%letter_case), opts, long_lines)
    do k = 1, size(long_lines)
        diags = [diags, diagnostic_t(long_lines(k), 'indentation kept: the line would be longer than '// &
                                     to_s(opts%line_length)//' characters')]
    end do
    if (present(src_tokens)) then
        call move_alloc(source%tokens%tok, src_tokens%tok)
        src_tokens%n = source%tokens%n
    end if
end subroutine format_pass

function limit_diagnostics(src) result(diags)
    !! Warnings for input beyond the limits of the Fortran 2023 standard (6.3.2.1): lines longer than
    !! 10,000 characters, and statements of more than 1,000,000 characters (counted without comments
    !! and line breaks).
    character(len=*), intent(in) :: src      !! source text
    type(diagnostic_t), allocatable :: diags(:)
    integer, parameter :: MAX_LINE = 10000, MAX_STATEMENT = 1000000
    type(source_t) :: source
    integer :: k, j, n, line, width, c

    allocate (diags(0))
    if (longest_line(src) > MAX_LINE) then
        line = 1
        width = 0
        do k = 1, len(src)
            c = iand(ichar(src(k:k)), 255)
            if (c == 10) then
                line = line + 1
                width = 0
            else if (c < 128 .or. c >= 192) then
                width = width + 1
                if (width == MAX_LINE + 1) diags = [diags, diagnostic_t(line, 'line '//to_s(line)// &
                                                                        ' is longer than '//to_s(MAX_LINE)//' characters, '// &
                                                                        'the limit of the Fortran standard')]
            end if
        end do
    end if
    if (len(src) <= MAX_STATEMENT) return
    call parse_source(src, source)
    do k = 1, source%stmts%n
        if (source%stmts%s(k)%kind /= SK_CODE) cycle
        n = 0
        do j = source%stmts%s(k)%first, source%stmts%s(k)%last
            associate (t => source%tokens%tok(j))
                if (t%kind /= TK_COMMENT .and. t%kind /= TK_NEWLINE) n = n + t%last - t%first + 1
            end associate
        end do
        if (n > MAX_STATEMENT) then
            line = source%tokens%tok(source%stmts%s(k)%first)%line
            diags = [diags, diagnostic_t(line, 'the statement on line '//to_s(line)//' has more than '// &
                                         to_s(MAX_STATEMENT)//' characters, the limit of the Fortran standard')]
        end if
    end do
end function limit_diagnostics

function relayout(text, changed, opts) result(out)
    !! `text` with the statements at `changed` (byte ranges, in order) laid out again, each on its own
    !! at the column where it starts. `text` is formatted apart from those statements, and they do
    !! not depend on the lines around them (see [[stands_alone]]), so this gives what formatting the
    !! whole text again would, in a fraction of the time.
    character(len=*), intent(in) :: text                 !! the text, after lines were split
    integer, intent(in) :: changed(:, :)                 !! byte ranges of the statements to lay out
    type(format_options_t), intent(in) :: opts           !! the formatting options
    character(len=:), allocatable :: out
    type(format_options_t) :: alone
    type(diagnostic_t), allocatable :: unused(:)
    type(string_builder_t) :: buf
    character(len=:), allocatable :: piece
    integer :: j, next

    alone = opts
    next = 1
    do j = 1, size(changed, 2)
        associate (a => changed(1, j), b => changed(2, j))
            call buf%append(text(next:a - 1))
            alone%base_indent = verify(text(a:b)//'x', ' ') - 1    ! its indentation
            call format_pass(text(a:b), alone, piece, unused)
            call buf%append(piece)
            next = b + 1
        end associate
    end do
    call buf%append(text(next:))
    out = buf%get()
end function relayout

pure logical function too_long(text, limit)
    !! True if a code line of `text` may be longer than `limit` characters (see [[longest_code]]).
    !! A line has no more characters than bytes, so the scan is only needed when a line has more
    !! bytes than that, which finding the line breaks shows quickly.
    character(len=*), intent(in) :: text   !! the formatted text
    integer, intent(in) :: limit           !! the line length
    integer :: p, q
    too_long = .false.
    p = 1
    do while (p <= len(text))
        q = index(text(p:), achar(10))
        if (q == 0) q = len(text) - p + 2
        if (q - 1 > limit) then
            too_long = longest_code(text) > limit
            return
        end if
        p = p + q
    end do
end function too_long

pure integer function longest_code(text) result(width)
    !! An upper bound of the width of the longest code line of `text`: the characters of each line
    !! before a comment, without the blanks before it, as [[long_code_lines]] counts them.
    !! Preprocessor lines do not count. Strings are followed across continuation lines, so that a
    !! `!` inside one does not end the code, but the scan does not tokenize, so it may count more
    !! than a token-based count would; never less.
    character(len=*), intent(in) :: text   !! the formatted text
    integer :: k, c, w, code_w, last_nonblank
    character :: quote
    logical :: line_start, skip, in_comment

    width = 0
    w = 0
    code_w = 0
    quote = ' '
    line_start = .true.
    skip = .false.
    in_comment = .false.
    last_nonblank = 0
    do k = 1, len(text)
        c = iand(ichar(text(k:k)), 255)
        if (c == 13 .and. k < len(text)) then
            if (text(k + 1:k + 1) == achar(10)) cycle      ! CRLF: the line ends at the LF
        end if
        if (c == 10 .or. c == 13) then
            width = max(width, code_w)
            ! a string continued on the next line ends this one with `&`
            if (quote /= ' ' .and. .not. (last_nonblank > 0 .and. text(max(1, last_nonblank):max(1, last_nonblank)) == '&')) &
                quote = ' '
            w = 0
            code_w = 0
            line_start = .true.
            skip = .false.
            in_comment = .false.
            last_nonblank = 0
            cycle
        end if
        if (skip .or. in_comment) cycle
        if (c >= 128 .and. c < 192) cycle                  ! a UTF-8 continuation byte
        w = w + 1
        if (text(k:k) == ' ' .or. text(k:k) == achar(9)) cycle
        if (line_start) then
            line_start = .false.
            if (quote == ' ' .and. (text(k:k) == '#' .or. (index('$@', text(k:k)) > 0 .and. &
                                                           text(min(k + 1, len(text)):min(k + 1, len(text))) == ':'))) then
                skip = .true.                              ! a preprocessor line
                cycle
            end if
        end if
        if (quote /= ' ') then
            if (text(k:k) == quote) quote = ' '            ! a doubled quote reopens at once
        else if (text(k:k) == '"' .or. text(k:k) == "'") then
            quote = text(k:k)
        else if (text(k:k) == '!') then
            in_comment = .true.
            cycle
        end if
        code_w = w
        last_nonblank = k
    end do
    width = max(width, code_w)
end function longest_code

pure integer function longest_line(text) result(width)
    !! Width of the longest line of `text`, in characters (as [[utf8_width]] counts them).
    character(len=*), intent(in) :: text   !! text whose lines end with LF
    integer :: k, c, w
    width = 0
    w = 0
    do k = 1, len(text)
        c = iand(ichar(text(k:k)), 255)
        if (c == 10) then
            w = 0
        else if (c < 128 .or. c >= 192) then
            w = w + 1
            width = max(width, w)
        end if
    end do
end function longest_line

logical function same_code(a, b, reordered_use, a_tokens)
    !! True if `a` and `b` have the same code tokens, in the same order, with the same text and the
    !! same joins across line breaks (blanks, comments and line layout may differ). Names and dot
    !! operators are compared ignoring case, as Fortran does, and the two spellings of a relational
    !! operator (`<` and `.lt.`) are the same.
    !!
    !! With `reordered_use`, `use` statements may also be in a different order within a run of them
    !! (not across other statements or preprocessor lines), and so may the items of an `only:` list.
    character(len=*), intent(in) :: a                     !! first source text
    character(len=*), intent(in) :: b                     !! second source text
    logical, intent(in), optional :: reordered_use        !! allow reordered `use` statements; default false
    type(token_list_t), intent(in), optional :: a_tokens    !! the tokens of `a`, if already known
    type(token_list_t) :: ta, tb

    if (present(reordered_use)) then
        if (reordered_use) then
            same_code = code_signature(a) == code_signature(b)
            return
        end if
    end if
    ! token by token, without building signatures
    call tokenize(b, tb)
    if (present(a_tokens)) then
        same_code = same_lists(a_tokens)          ! without a copy of the token list
    else
        call tokenize(a, ta)
        same_code = same_lists(ta)
    end if

contains

    logical function same_lists(ta)
        !! True if the code tokens of `a` (`ta`) match those of `b` (`tb`).
        type(token_list_t), intent(in) :: ta   !! the tokens of `a`
        integer :: i, j
        same_lists = .false.
        i = 0
        j = 0
        do
            i = next_code(ta, i)
            j = next_code(tb, j)
            if (i > ta%n .or. j > tb%n) exit
            if (.not. same_token(a, ta%tok(i), b, tb%tok(j))) return
        end do
        same_lists = i > ta%n .and. j > tb%n
    end function same_lists

    pure integer function next_code(tokens, k) result(next)
        !! The index of the first code token after token `k`, or `tokens%n + 1`.
        type(token_list_t), intent(in) :: tokens   !! the token list
        integer, intent(in) :: k                   !! index into `tokens`, or 0 to start at the beginning
        next = k + 1
        do while (next <= tokens%n)
            if (is_code_kind(tokens%tok(next)%kind)) return
            next = next + 1
        end do
    end function next_code

end function same_code

pure logical function same_token(sa, s, sb, t)
    !! True if code token `s` of `sa` matches code token `t` of `sb`, as their keys in
    !! [[code_signature]] would: same kind (operators all alike), same glue, and the same text,
    !! ignoring case where Fortran does (see [[folded_length]]), with relational operators in their
    !! symbol spelling.
    character(len=*), intent(in) :: sa   !! source text of `s`
    character(len=*), intent(in) :: sb   !! source text of `t`
    type(token_t), intent(in) :: s       !! a code token of `sa`
    type(token_t), intent(in) :: t       !! a code token of `sb`
    integer :: ks, kt, k, nfold
    character(len=:), allocatable :: ca, cb

    ks = s%kind
    kt = t%kind
    if (ks == TK_DOTOP) ks = TK_OP
    if (kt == TK_DOTOP) kt = TK_OP
    same_token = .false.
    if (ks /= kt .or. (s%glued .neqv. t%glued)) return
    if (s%last - s%first == t%last - t%first) then
        nfold = folded_length(sa(s%first:s%last), s%kind)
        same_token = .true.
        do k = 0, s%last - s%first
            if (k < nfold) then
                if (lower_char(sa(s%first + k:s%first + k)) == lower_char(sb(t%first + k:t%first + k))) cycle
            else
                if (sa(s%first + k:s%first + k) == sb(t%first + k:t%first + k)) cycle
            end if
            same_token = .false.
            exit
        end do
        if (same_token) return
    end if
    ! `.lt.` and `<`: rare, so the allocating comparison is fine here
    if (ks == TK_OP) then
        ca = canonical_relational(token_lower(sa, s))
        cb = canonical_relational(token_lower(sb, t))
        same_token = len(ca) == len(cb) .and. ca == cb
    end if
end function same_token

pure integer function folded_length(text, kind) result(n)
    !! How much of a token's text, from its start, Fortran reads ignoring case: all of a name, an
    !! operator or a number (exponent letters and kinds), the kind prefix of a string, and none of
    !! anything else.
    character(len=*), intent(in) :: text   !! the token's text
    integer, intent(in) :: kind            !! its kind (`TK_*`)
    select case (kind)
    case (TK_NAME, TK_OP, TK_DOTOP, TK_INT, TK_REAL)
        n = len(text)
    case (TK_STRING)
        n = max(0, scan(text, '"'//"'") - 1)
    case default
        n = 0
    end select
end function folded_length

function code_signature(src) result(sig)
    !! The code tokens of `src` as one string that [[same_code]] compares when `use` statements may
    !! be reordered: for each token its kind (operators all alike), whether it is glued to the
    !! previous token, and its text as `key` gives it. The signatures of `use` statements in a run
    !! are sorted, and so are those of the items of each `only:` list.
    character(len=*), intent(in) :: src   !! source text
    character(len=:), allocatable :: sig

    type(source_t) :: source
    type(statement_view_t) :: v
    type(string_builder_t) :: buf
    type(string_t), allocatable :: run(:)
    type(string_t) :: item
    integer :: k, j

    call parse_source(src, source)
    allocate (run(0))
    associate (tokens => source%tokens, stmts => source%stmts)
        do k = 1, stmts%n
            if (stmts%s(k)%kind == SK_PREPROC) call flush_run()
            if (stmts%s(k)%kind /= SK_CODE) cycle
            call set_view(src, tokens, stmts%s(k), v)
            if (is_use_statement(v)) then
                ! through a variable: a gfortran 15 internal compiler error otherwise
                item%s = use_signature(stmts%s(k), v)
                run = [run, item]
            else
                call flush_run()
                do j = stmts%s(k)%first, stmts%s(k)%last
                    if (is_code_kind(tokens%tok(j)%kind)) call buf%append(key(tokens%tok(j)))
                end do
            end if
        end do
    end associate
    call flush_run()
    sig = buf%get()

contains

    subroutine flush_run()
        !! Append the signatures of the pending run of `use` statements, sorted, and empty the run.
        integer, allocatable :: order(:)
        integer :: p
        order = sorted_order(run)
        do p = 1, size(order)
            call buf%append(run(order(p))%s)
        end do
        deallocate (run)
        allocate (run(0))
    end subroutine flush_run

    function use_signature(stmt, v) result(s)
        !! Signature of a `use` statement, with the items of its `only:` list sorted. Any `;` that
        !! ends it comes last.
        type(statement_t), intent(in) :: stmt          !! the `use` statement
        type(statement_view_t), intent(in) :: v        !! its view
        character(len=:), allocatable :: s
        integer, allocatable :: starts(:), ends(:), order(:)
        type(string_t), allocatable :: items(:)
        integer :: p, q, first

        starts = only_items(v)
        first = v%n + 1
        if (size(starts) > 0) first = starts(1)
        s = ''
        do p = 1, first - 1
            s = s//key(source%tokens%tok(v%code(p)))
        end do
        if (size(starts) > 0) then
            ends = [starts(2:) - 2, v%n]
            allocate (items(size(starts)))
            do p = 1, size(starts)
                items(p)%s = ''
                do q = starts(p), ends(p)
                    items(p)%s = items(p)%s//key(source%tokens%tok(v%code(q)))
                end do
            end do
            order = sorted_order(items)
            do p = 1, size(order)
                s = s//items(order(p))%s
                if (p < size(order)) s = s//key(source%tokens%tok(v%code(starts(2) - 1)))   ! the comma
            end do
        end if
        do p = stmt%first, stmt%last
            if (source%tokens%tok(p)%kind == TK_OP) then
                if (token_text(src, source%tokens%tok(p)) == ';') s = s//key(source%tokens%tok(p))
            end if
        end do
    end function use_signature

    function key(t) result(k)
        !! What must match: the kind, the glue, and the text, ignoring case for names and operators,
        !! with relational operators in their symbol spelling. The length is included, since `==`
        !! pads with blanks.
        type(token_t), intent(in) :: t   !! a code token of `src`
        character(len=:), allocatable :: k
        character :: kind, glue
        select case (t%kind)
        case (TK_DOTOP, TK_OP)
            k = canonical_relational(token_lower(src, t))
        case default
            k = token_text(src, t)
            associate (n => folded_length(k, t%kind))
                k(1:n) = lowercase(k(1:n))
            end associate
        end select
        kind = achar(64 + t%kind)
        if (t%kind == TK_DOTOP) kind = achar(64 + TK_OP)
        glue = merge('g', ' ', t%glued)
        k = kind//glue//to_s(len(k))//':'//k
    end function key

end function code_signature

end module ffmt_format


!>>>>> build/dependencies/toml-f/src/tomlf/type/array.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML array data type
!>
!> A [[toml_array]] represents a TOML array, which is an ordered sequence
!> of values. TOML arrays can contain values of any type, including nested
!> arrays and tables (arrays of tables).
!>
!> Use [[get_value]] from the build module to retrieve array elements by
!> index, or the type-bound procedures for direct access. The intrinsic
!> [[len]] function is overloaded to return the number of elements.
module tomlf_type_array
   use tomlf_error, only : toml_stat
   use tomlf_type_value, only : toml_value, toml_visitor
   use tomlf_structure, only : toml_list_structure, new_list_structure
   implicit none
   private

   public :: toml_array, new_array, new, initialized, len


   !> TOML array
   type, extends(toml_value) :: toml_array

      !> Is an inline array rather than an array of tables
      logical :: inline = .true.

      !> Storage unit for TOML values of this array
      class(toml_list_structure), allocatable, private :: list

   contains

      !> Get the TOML value at a given index
      procedure :: get

      !> Append value to array
      procedure :: push_back

      !> Remove the first element from the array
      procedure :: shift

      !> Remove the last element from the array
      procedure :: pop

      !> Release allocation hold by TOML array
      procedure :: destroy

   end type toml_array


   !> Create standard constructor
   interface toml_array
      module procedure :: new_array_func
   end interface toml_array


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_array
   end interface


   !> Overload len function
   interface len
      module procedure :: get_len
   end interface


   !> Check whether data structure is initialized properly
   interface initialized
      module procedure :: array_initialized
   end interface initialized


contains


!> Constructor to create a new TOML array and allocate the internal storage
subroutine new_array(self)

   !> Instance of the TOML array
   type(toml_array), intent(out) :: self

   call new_list_structure(self%list)

end subroutine new_array


!> Default constructor for TOML array type
function new_array_func() result(self)

   !> Instance of the TOML array
   type(toml_array) :: self

   call new_array(self)

end function new_array_func


!> Check whether data structure is initialized properly
pure function array_initialized(self) result(okay)

   !> Instance of the TOML array
   type(toml_array), intent(in) :: self

   !> Data structure is initialized
   logical :: okay

   okay = allocated(self%list)
end function array_initialized


!> Get number of TOML values in the array
pure function get_len(self) result(length)

   !> Instance of the TOML array
   class(toml_array), intent(in) :: self

   !> Current length of the array
   integer :: length

   length = self%list%get_len()

end function get_len


!> Get the TOML value at the respective index
subroutine get(self, idx, ptr)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> Index to the TOML value
   integer, intent(in) :: idx

   !> Pointer to the TOML value
   class(toml_value), pointer, intent(out) :: ptr

   call self%list%get(idx, ptr)

end subroutine get


!> Push back a TOML value to the array
subroutine push_back(self, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to append to array
   class(toml_value), allocatable, intent(inout) :: val

   !> Status of operation
   integer, intent(out) :: stat

   if (allocated(val%key)) then
      stat = toml_stat%fatal
      return
   end if

   call self%list%push_back(val)

   stat = toml_stat%success

end subroutine push_back


!> Remove the first element from the data structure
subroutine shift(self, val)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   call self%list%shift(val)

end subroutine shift


!> Remove the last element from the data structure
subroutine pop(self, val)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   call self%list%pop(val)

end subroutine pop


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%list)) then
      call self%list%destroy
      deallocate(self%list)
   end if

end subroutine destroy


end module tomlf_type_array


!>>>>> build/dependencies/toml-f/src/tomlf/type/table.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML table data type
!>
!> A [[toml_table]] represents a TOML table (also known as a hash table or
!> dictionary). Every TOML document contains at least one root table which
!> holds key-value pairs, arrays, and nested tables.
!>
!> Tables are the primary way to access parsed TOML data. Use [[get_value]]
!> from the build module to retrieve values by key, or the type-bound
!> procedures for direct access.
module tomlf_type_table
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   use tomlf_structure, only : toml_map_structure, new_map_structure
   implicit none
   private

   public :: toml_table, new_table, new, initialized


   !> TOML table
   type, extends(toml_value) :: toml_table

      !> Table was implictly created
      logical :: implicit = .false.

      !> Is an inline table and is therefore non-extendable
      logical :: inline = .false.

      !> Storage unit for TOML values of this table
      class(toml_map_structure), allocatable, private :: map

   contains

      !> Get the TOML value associated with the respective key
      procedure :: get

      !> Get list of all keys in this table
      procedure :: get_keys

      !> Check if key is already present in this table instance
      procedure :: has_key

      !> Append value to table (checks automatically for key)
      procedure :: push_back

      !> Remove TOML value at a given key and return it
      procedure :: pop

      !> Delete TOML value at a given key
      procedure :: delete

      !> Release allocation hold by TOML table
      procedure :: destroy

   end type toml_table


   !> Create standard constructor
   interface toml_table
      module procedure :: new_table_func
   end interface toml_table


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_table
   end interface


   !> Check whether data structure is initialized properly
   interface initialized
      module procedure :: table_initialized
   end interface initialized


contains


!> Constructor to create a new TOML table and allocate the internal storage
subroutine new_table(self)

   !> Instance of the TOML table
   type(toml_table), intent(out) :: self

   call new_map_structure(self%map)

end subroutine new_table


!> Default constructor for TOML table type
function new_table_func() result(self)

   !> Instance of the TOML table
   type(toml_table) :: self

   call new_table(self)

end function new_table_func


!> Check whether data structure is initialized properly
pure function table_initialized(self) result(okay)

   !> Instance of the TOML table
   type(toml_table), intent(in) :: self

   !> Data structure is initialized
   logical :: okay

   okay = allocated(self%map)
end function table_initialized


!> Get the TOML value associated with the respective key
subroutine get(self, key, ptr)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the TOML value
   class(toml_value), pointer, intent(out) :: ptr

   call self%map%get(key, ptr)

end subroutine get


!> Get list of all keys in this table
subroutine get_keys(self, list)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   call self%map%get_keys(list)

end subroutine get_keys


!> Check if a key is present in the table
function has_key(self, key) result(found)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> TOML value is present in table
   logical :: found

   class(toml_value), pointer :: ptr

   call self%map%get(key, ptr)

   found = associated(ptr)

end function has_key


!> Push back a TOML value to the table
subroutine push_back(self, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> TOML value to append to table
   class(toml_value), allocatable, intent(inout) :: val

   !> Status of operation
   integer, intent(out) :: stat

   class(toml_value), pointer :: ptr

   if (.not.allocated(val)) then
      stat = merge(self%origin, toml_stat%fatal, self%origin > 0)
      return
   end if

   if (.not.allocated(val%key)) then
      stat = merge(val%origin, toml_stat%fatal, val%origin > 0)
      return
   end if

   call self%get(val%key, ptr)
   if (associated(ptr)) then
      stat = merge(ptr%origin, toml_stat%duplicate_key, ptr%origin > 0)
      return
   end if

   call self%map%push_back(val)

   stat = toml_stat%success

end subroutine push_back


!> Remove TOML value at a given key and return it
subroutine pop(self, key, val)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Removed TOML value to return
   class(toml_value), allocatable, intent(out) :: val

   call self%map%pop(key, val)

end subroutine pop


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   call self%map%delete(key)

end subroutine delete


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%map)) then
      call self%map%destroy
      deallocate(self%map)
   end if

end subroutine destroy


end module tomlf_type_table


!>>>>> build/dependencies/toml-f/src/tomlf/type.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Collection of the central datatypes to define TOML data structures
!>
!> All TOML data types should inherit from an abstract value allowing to generate
!> a generic interface to deal with all more specialized TOML data types, while
!> the abstract value is interesting for developing algorithms in TOML-Fortran,
!> the user of TOML-Fortran will usually only care about TOML tables and possibly
!> arrays.
!>
!> The TOML types defined here should implement the TOML data structures (mostly)
!> without taking the actual implementation of the data structures into account.
!> This is done by providing a bare minimum interface using type bound procedures
!> to minimize the interdependencies between the datatypes.
!>
!> To make the data types extendable a visitor pattern allows access to the TOML
!> data types and can be used to implement further algorithms.
module tomlf_type
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type_array, only : toml_array, new_array, new, initialized, len
   use tomlf_type_keyval, only : toml_keyval, new_keyval, new
   use tomlf_type_table, only : toml_table, new_table, new, initialized
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   implicit none
   private

   public :: toml_value, toml_visitor, toml_table, toml_array, toml_keyval
   public :: toml_key
   public :: new, new_table, new_array, new_keyval, initialized, len
   public :: add_table, add_array, add_keyval
   public :: is_array_of_tables
   public :: cast_to_table, cast_to_array, cast_to_keyval


   !> Interface to build new tables
   interface add_table
      module procedure :: add_table_to_table
      module procedure :: add_table_to_table_key
      module procedure :: add_table_to_array
   end interface add_table


   !> Interface to build new arrays
   interface add_array
      module procedure :: add_array_to_table
      module procedure :: add_array_to_table_key
      module procedure :: add_array_to_array
   end interface add_array


   !> Interface to build new key-value pairs
   interface add_keyval
      module procedure :: add_keyval_to_table
      module procedure :: add_keyval_to_table_key
      module procedure :: add_keyval_to_array
   end interface add_keyval


contains


!> Create a new TOML table inside an existing table
subroutine add_table_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_table)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_table_to_table


!> Create a new TOML table inside an existing table
subroutine add_table_to_table_key(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new table
   type(toml_key), intent(in) :: key

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   call add_table(table, key%key, ptr, stat)
   if (associated(ptr)) ptr%origin = key%origin
end subroutine add_table_to_table_key


!> Create a new TOML array inside an existing table
subroutine add_array_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new array
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_array_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_array)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_array_to_table


!> Create a new TOML array inside an existing table
subroutine add_array_to_table_key(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new array
   type(toml_key), intent(in) :: key

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   call add_array(table, key%key, ptr, stat)
   if (associated(ptr)) ptr%origin = key%origin
end subroutine add_array_to_table_key


!> Create a new key-value pair inside an existing table
subroutine add_keyval_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new key-value pair
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_keyval_to_table


!> Create a new key-value pair inside an existing table
subroutine add_keyval_to_table_key(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new key-value pair
   type(toml_key), intent(in) :: key

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   call add_keyval(table, key%key, ptr, stat)
   if (associated(ptr)) ptr%origin = key%origin
end subroutine add_keyval_to_table_key


!> Create a new TOML table inside an existing array
subroutine add_table_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_table)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_table_to_array


!> Create a new TOML array inside an existing array
subroutine add_array_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   allocate(toml_array :: val)
   call new_array_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_array)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_array_to_array


!> Create a new key-value pair inside an existing array
subroutine add_keyval_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_keyval_to_array


!> Wrapped constructor to create a new TOML table on an abstract TOML value
subroutine new_table_(self)

   !> Newly created TOML table
   class(toml_value), allocatable, intent(out) :: self

   type(toml_table), allocatable :: val

   allocate(val)
   call new_table(val)
   call move_alloc(val, self)

end subroutine new_table_


!> Wrapped constructor to create a new TOML array on an abstract TOML value
subroutine new_array_(self)

   !> Newly created TOML array
   class(toml_value), allocatable, intent(out) :: self

   type(toml_array), allocatable :: val

   allocate(val)
   call new_array(val)
   call move_alloc(val, self)

end subroutine new_array_


!> Wrapped constructor to create a new TOML array on an abstract TOML value
subroutine new_keyval_(self)

   !> Newly created key-value pair
   class(toml_value), allocatable, intent(out) :: self

   type(toml_keyval), allocatable :: val

   allocate(val)
   call new_keyval(val)
   call move_alloc(val, self)

end subroutine new_keyval_


!> Determine if array contains only tables
function is_array_of_tables(array) result(only_tables)

   !> TOML value to visit
   class(toml_array), intent(inout) :: array

   !> Array contains only tables
   logical :: only_tables

   class(toml_value), pointer :: ptr
   integer :: i, n


   n = len(array)
   only_tables = n > 0

   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      type is(toml_table)
         cycle
      class default
         only_tables = .false.
         exit
      end select
   end do

end function is_array_of_tables


!> Cast an abstract TOML value to a TOML array
function cast_to_array(ptr) result(array)
   !> TOML value to be casted
   class(toml_value), intent(in), target :: ptr
   !> TOML array view, nullified if the value is not an array
   type(toml_array), pointer :: array

   nullify(array)
   select type(ptr)
   type is(toml_array)
      array => ptr
   end select
end function cast_to_array

!> Cast an abstract TOML value to a TOML table
function cast_to_table(ptr) result(table)
   !> TOML value to be casted
   class(toml_value), intent(in), target :: ptr
   !> TOML table view, nullified if the value is not a table
   type(toml_table), pointer :: table

   nullify(table)
   select type(ptr)
   type is(toml_table)
      table => ptr
   end select
end function cast_to_table

!> Cast an abstract TOML value to a TOML key-value pair
function cast_to_keyval(ptr) result(kval)
   !> TOML value to be casted
   class(toml_value), intent(in), target :: ptr
   !> TOML key-value view, nullified if the value is not a table
   type(toml_keyval), pointer :: kval

   nullify(kval)
   select type(ptr)
   type is(toml_keyval)
      kval => ptr
   end select
end function cast_to_keyval


end module tomlf_type


!>>>>> build/dependencies/toml-f/src/tomlf/ser.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML serialization module
!>
!> This module provides interfaces for serializing TOML data structures
!> back to TOML format. The primary interfaces are:
!>
!> - [[toml_dump]]: Write a TOML table to a file or I/O unit
!> - [[toml_dumps]]: Serialize a TOML table to a string
!> - [[toml_serialize]]: Low-level serialization using the visitor pattern
!>
!> The [[toml_serializer]] type implements the visitor pattern and can be
!> used directly for custom serialization workflows.
module tomlf_ser
   use tomlf_constants, only : tfc, tfi, tfr, tfout, toml_type
   use tomlf_datetime, only : toml_datetime, to_string
   use tomlf_error, only : toml_error, toml_stat, make_error
   use tomlf_type, only : toml_value, toml_visitor, toml_key, toml_table, &
      & toml_array, toml_keyval, is_array_of_tables, len
   use tomlf_utils, only : to_string, toml_escape_string
   implicit none
   private

   public :: toml_serializer, new_serializer, new
   public :: toml_dump, toml_dumps, toml_serialize


   interface toml_dumps
      module procedure :: toml_dump_to_string
   end interface toml_dumps

   interface toml_dump
      module procedure :: toml_dump_to_file
      module procedure :: toml_dump_to_unit
   end interface toml_dump


   !> Configuration for JSON serializer
   type :: toml_ser_config

      !> Indentation
      character(len=:), allocatable :: indent

   end type toml_ser_config


   !> TOML serializer to produduce a TOML document from a datastructure
   type, extends(toml_visitor) :: toml_serializer
      private

      !> Output string
      character(:), allocatable :: output

      !> Configuration for serializer
      type(toml_ser_config) :: config

      !> Special mode for printing array of tables
      logical, private :: array_of_tables

      !> Special mode for printing inline arrays
      logical, private :: inline_array

      !> Top of the key stack
      integer, private :: top

      !> Key stack to create table headers
      type(toml_key), allocatable, private :: stack(:)

   contains

      !> Visit a TOML value
      procedure :: visit

   end type toml_serializer


   !> Create standard constructor
   interface toml_serializer
      module procedure :: new_serializer_func
   end interface toml_serializer


   !> Overloaded constructor for TOML serializers
   interface new
      module procedure :: new_serializer
   end interface


   !> Initial size of the key path stack
   integer, parameter :: initial_size = 8


contains


!> Serialize a JSON value to a string and return it.
!>
!> In case of an error this function will invoke an error stop.
function toml_serialize(val, config) result(string)
   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   !> Serialized JSON value
   character(len=:), allocatable :: string

   type(toml_error), allocatable :: error

   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) then
      print '(a)', "Error: " // error%message
      error stop 1
   end if
end function toml_serialize


!> Create a string representing the JSON value
subroutine toml_dump_to_string(val, string, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Formatted unit to write to
   character(:), allocatable, intent(out) :: string

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   type(toml_serializer) :: ser

   ser = toml_serializer(config=config)
   call val%accept(ser)
   string = ser%output
end subroutine toml_dump_to_string


!> Write string representation of JSON value to a connected formatted unit
subroutine toml_dump_to_unit(val, io, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Formatted unit to write to
   integer, intent(in) :: io

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   character(len=:), allocatable :: string
   character(512) :: msg
   integer :: stat

   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) return
   write(io, '(a)', iostat=stat, iomsg=msg) string
   if (stat /= 0) then
      call make_error(error, trim(msg))
      return
   end if
end subroutine toml_dump_to_unit


!> Write string representation of JSON value to a file
subroutine toml_dump_to_file(val, filename, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> File name to write to
   character(*), intent(in) :: filename

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   integer :: io
   integer :: stat
   character(512) :: msg

   open(file=filename, newunit=io, iostat=stat, iomsg=msg)
   if (stat /= 0) then
      call make_error(error, trim(msg))
      return
   end if
   call toml_dump(val, io, error, config=config)
   close(unit=io, iostat=stat, iomsg=msg)
   if (.not.allocated(error) .and. stat /= 0) then
      call make_error(error, trim(msg))
   end if
end subroutine toml_dump_to_file


!> Constructor to create new serializer instance
subroutine new_serializer(self, config)

   !> Instance of the TOML serializer
   type(toml_serializer), intent(out) :: self

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   self%output = ""
   self%array_of_tables = .false.
   self%inline_array = .false.
   self%top = 0
   self%config = toml_ser_config()
   if (present(config)) self%config = config
end subroutine new_serializer


!> Default constructor for TOML serializer
function new_serializer_func(config) result(self)

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   !> Instance of the TOML serializer
   type(toml_serializer) :: self

   call new_serializer(self, config)
end function new_serializer_func


!> Visit a TOML value
recursive subroutine visit(self, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: self

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   select type(val)
   class is(toml_keyval)
      call visit_keyval(self, val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select

end subroutine visit


!> Visit a TOML key-value pair
subroutine visit_keyval(visitor, keyval)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_keyval), intent(inout) :: keyval

   character(kind=tfc, len=:), allocatable :: key, str
   type(toml_datetime), pointer :: dval
   character(:, tfc), pointer :: sval
   integer(tfi), pointer :: ival
   real(tfr), pointer :: rval
   logical, pointer :: lval

   call keyval%get_key(key)

   select case(keyval%get_type())
   case(toml_type%string)
      call keyval%get(sval)
      call toml_escape_string(sval, str)
   case(toml_type%int)
      call keyval%get(ival)
      str = to_string(ival)
   case(toml_type%float)
      call keyval%get(rval)
      str = to_string(rval)
   case(toml_type%boolean)
      call keyval%get(lval)
      if (lval) then
         str = "true"
      else
         str = "false"
      end if
   case(toml_type%datetime)
      call keyval%get(dval)
      str = to_string(dval)
   end select

   if (visitor%inline_array) then
      visitor%output = visitor%output // " "
   end if
   visitor%output = visitor%output // key // " = " // str
   if (.not.visitor%inline_array) then
      visitor%output = visitor%output // new_line('a')
   end if

end subroutine visit_keyval


!> Visit a TOML array
recursive subroutine visit_array(visitor, array)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key, str
   type(toml_datetime), pointer :: dval
   character(:, tfc), pointer :: sval
   integer(tfi), pointer :: ival
   real(tfr), pointer :: rval
   logical, pointer :: lval
   integer :: i, n

   if (visitor%inline_array) visitor%output = visitor%output // " ["
   n = len(array)
   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      class is(toml_keyval)

         select case(ptr%get_type())
         case(toml_type%string)
            call ptr%get(sval)
            call toml_escape_string(sval, str)
         case(toml_type%int)
            call ptr%get(ival)
            str = to_string(ival)
         case(toml_type%float)
            call ptr%get(rval)
            str = to_string(rval)
         case(toml_type%boolean)
            call ptr%get(lval)
            if (lval) then
               str = "true"
            else
               str = "false"
            end if
         case(toml_type%datetime)
            call ptr%get(dval)
            str = to_string(dval)
         end select

         visitor%output = visitor%output // " " // str
         if (i /= n) visitor%output = visitor%output // ","
      class is(toml_array)
         call ptr%accept(visitor)
         if (i /= n) visitor%output = visitor%output // ","
      class is(toml_table)
         if (visitor%inline_array) then
            visitor%output = visitor%output // " {"
            call ptr%accept(visitor)
            visitor%output = visitor%output // " }"
            if (i /= n) visitor%output = visitor%output // ","
         else
            visitor%array_of_tables = .true.
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call array%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end if
      end select
   end do
   if (visitor%inline_array) visitor%output = visitor%output // " ]"

end subroutine visit_array


!> Visit a TOML table
recursive subroutine visit_table(visitor, table)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_key), allocatable :: list(:)
   logical, allocatable :: defer(:)
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   call table%get_keys(list)

   n = size(list, 1)
   allocate(defer(n))

   if (.not.allocated(visitor%stack)) then
      call resize(visitor%stack)
   else
      if (.not.(visitor%inline_array .or. table%implicit)) then
         visitor%output = visitor%output // "["
         if (visitor%array_of_tables) visitor%output = visitor%output // "["
         do i = 1, visitor%top-1
            visitor%output = visitor%output // visitor%stack(i)%key // "."
         end do
         visitor%output = visitor%output // visitor%stack(visitor%top)%key
         visitor%output = visitor%output // "]"
         if (visitor%array_of_tables) visitor%output = visitor%output // "]"
         visitor%output = visitor%output // new_line('a')
         visitor%array_of_tables = .false.
      end if
   end if

   do i = 1, n
      defer(i) = .false.
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class is(toml_keyval)
         call ptr%accept(visitor)
         if (visitor%inline_array) then
            if (i /= n) visitor%output = visitor%output // ","
         end if
      class is(toml_array)
         if (visitor%inline_array) then
            call ptr%get_key(key)
            visitor%output = visitor%output // " " // key // " ="
            call ptr%accept(visitor)
            if (i /= n) visitor%output = visitor%output // ","
         else
            if (is_array_of_tables(ptr)) then
               ! Array of tables open a new section
               ! -> cannot serialize them before all key-value pairs are done
               defer(i) = .true.
            else
               visitor%inline_array = .true.
               call ptr%get_key(key)
               visitor%output = visitor%output // key // " ="
               call ptr%accept(visitor)
               visitor%inline_array = .false.
               visitor%output = visitor%output // new_line('a')
            end if
         end if
      class is(toml_table)
         ! Subtables open a new section
         ! -> cannot serialize them before all key-value pairs are done
         defer(i) = .true.
      end select
   end do

   do i = 1, n
      if (defer(i)) then
         call table%get(list(i)%key, ptr)
         select type(ptr)
         class is(toml_keyval)
            call ptr%accept(visitor)
            if (visitor%inline_array) then
               if (i /= n) visitor%output = visitor%output // ","
            end if
         class is(toml_array)
            if (visitor%inline_array) then
               call ptr%get_key(key)
               visitor%output = visitor%output // " " // key // " ="
               call ptr%accept(visitor)
               if (i /= n) visitor%output = visitor%output // ","
            else
               if (is_array_of_tables(ptr)) then
                  call ptr%accept(visitor)
               else
                  visitor%inline_array = .true.
                  call ptr%get_key(key)
                  visitor%output = visitor%output // key // " ="
                  call ptr%accept(visitor)
                  visitor%inline_array = .false.
                  visitor%output = visitor%output // new_line('a')
               end if
            end if
         class is(toml_table)
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call ptr%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end select
      end if
   end do

   if (.not.visitor%inline_array .and. visitor%top == 0) then
      deallocate(visitor%stack)
   end if

end subroutine visit_table


!> Change size of the stack
subroutine resize(stack, n)

   !> Stack of keys to be resized
   type(toml_key), allocatable, intent(inout) :: stack(:)

   !> New size of the stack
   integer, intent(in), optional :: n

   type(toml_key), allocatable :: tmp(:)
   integer :: m

   if (present(n)) then
      m = n
   else
      if (allocated(stack)) then
         m = size(stack)
         m = m + m/2 + 1
      else
         m = initial_size
      end if
   end if

   if (allocated(stack)) then
      call move_alloc(stack, tmp)
      allocate(stack(m))

      m = min(size(tmp), m)
      stack(:m) = tmp(:m)

      deallocate(tmp)
   else
      allocate(stack(m))
   end if

end subroutine resize


end module tomlf_ser


!>>>>> build/dependencies/toml-f/src/tomlf/build/keyval.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build a TOML values
!>
!> The build module defines an interface to work with TOML values instead
!> of accessing the raw value directly. Both setter and getter routines defined
!> here are rarely needed in any user context, but serve as a basic building
!> block to define uniform access methods for TOML tables and arrays.
module tomlf_build_keyval
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp, TOML_NEWLINE
   use tomlf_datetime, only : toml_datetime
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, len
   use tomlf_utils, only : toml_escape_string, to_string
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML values
   interface set_value
      module procedure :: set_value_float_sp
      module procedure :: set_value_float_dp
      module procedure :: set_value_integer_i1
      module procedure :: set_value_integer_i2
      module procedure :: set_value_integer_i4
      module procedure :: set_value_integer_i8
      module procedure :: set_value_bool
      module procedure :: set_value_datetime
      module procedure :: set_value_string
   end interface set_value


   !> Getter functions to manipulate TOML values
   interface get_value
      module procedure :: get_value_float_sp
      module procedure :: get_value_float_dp
      module procedure :: get_value_integer_i1
      module procedure :: get_value_integer_i2
      module procedure :: get_value_integer_i4
      module procedure :: get_value_integer_i8
      module procedure :: get_value_bool
      module procedure :: get_value_datetime
      module procedure :: get_value_string
   end interface get_value


   !> Length for the static character variables
   integer, parameter :: buffersize = 128


contains


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_value_float_sp(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   real(tfr), pointer :: dummy
   integer(tfi), pointer :: idummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = real(dummy, tf_sp)
      info = toml_stat%success
   else
      call self%get(idummy)
      if (associated(idummy)) then
         val = real(idummy, tf_sp)
         if (nint(val, tfi) == idummy) then
            info = toml_stat%success
         else
            info = toml_stat%conversion_error
         end if
      else
         info = toml_stat%type_mismatch
      end if
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_value_float_dp(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   real(tfr), pointer :: dummy
   integer(tfi), pointer :: idummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = real(dummy, tf_dp)
      info = toml_stat%success
   else
      call self%get(idummy)
      if (associated(idummy)) then
         val = real(idummy, tf_dp)
         if (nint(val, tfi) == idummy) then
            info = toml_stat%success
         else
            info = toml_stat%conversion_error
         end if
      else
         info = toml_stat%type_mismatch
      end if
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_value_integer_i1(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   integer(tfi), pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = int(dummy, tf_i1)
      if (dummy <= huge(val) .and. dummy >= -huge(val)-1) then
         info = toml_stat%success
      else
         info = toml_stat%conversion_error
      end if
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_value_integer_i2(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   integer(tfi), pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = int(dummy, tf_i2)
      if (dummy <= huge(val) .and. dummy >= -huge(val)-1) then
         info = toml_stat%success
      else
         info = toml_stat%conversion_error
      end if
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_value_integer_i4(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   integer(tfi), pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = int(dummy, tf_i4)
      if (dummy <= huge(val) .and. dummy >= -huge(val)-1) then
         info = toml_stat%success
      else
         info = toml_stat%conversion_error
      end if
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_value_integer_i8(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   integer(tfi), pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = int(dummy, tf_i8)
      info = toml_stat%success
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_value_bool(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Boolean value
   logical, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   logical, pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = dummy
      info = toml_stat%success
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_bool


!> Retrieve TOML value as datetime
subroutine get_value_datetime(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   type(toml_datetime), pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = dummy
      info = toml_stat%success
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_value_string(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: info
   character(:, tfc), pointer :: dummy

   call self%get(dummy)
   if (associated(dummy)) then
      val = dummy
      info = toml_stat%success
   else
      info = toml_stat%type_mismatch
   end if

   if (present(stat)) stat = info
   if (present(origin)) origin = self%origin_value
end subroutine get_value_string


!> Set TOML value to single precision float
subroutine set_value_float_sp(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(real(val, tfr))
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_float_sp


!> Set TOML value to double precision float
subroutine set_value_float_dp(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(real(val, tfr))
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_float_dp


!> Set TOML value to one byte integer
subroutine set_value_integer_i1(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(int(val, tfi))
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_value_integer_i2(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(int(val, tfi))
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_value_integer_i4(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(int(val, tfi))
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_value_integer_i8(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(int(val, tfi))
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_integer_i8


!> Set TOML value to logical
subroutine set_value_bool(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(val)
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_bool


!> Set TOML value to datetime
subroutine set_value_datetime(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(val)
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_datetime


!> Set TOML value to deferred-length character
subroutine set_value_string(self, val, stat, origin)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call self%set(val)
   if (present(stat)) stat = toml_stat%success

   self%origin_value = 0
   if (present(origin)) origin = self%origin
end subroutine set_value_string


end module tomlf_build_keyval


!>>>>> build/dependencies/toml-f/src/tomlf/build/merge.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Merge TOML data structures, the merge policy can be adjusted.
!>
!> Note that the context information cannot be preserved.
module tomlf_build_merge
   use tomlf_constants, only : tfc
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_value, &
      & toml_key, cast_to_keyval, len
   implicit none
   private

   public :: merge_table, merge_array, merge_policy, toml_merge_config


   !> Possible merge policies
   type :: enum_policy

      !> Overwrite existing values
      integer :: overwrite = 1

      !> Preserve existing values
      integer :: preserve = 2

      !> Append to existing values
      integer :: append = 3
   end type enum_policy

   !> Actual enumerator for merging data structures
   type(enum_policy), parameter :: merge_policy = enum_policy()


   !> Configuration for merging data structures
   type :: toml_merge_config

      !> Policy for merging tables
      integer :: table = merge_policy%append

      !> Policy for merging arrays
      integer :: array = merge_policy%preserve

      !> Policy for merging values
      integer :: keyval = merge_policy%preserve
   end type toml_merge_config

   !> Constructor for merge configuration
   interface toml_merge_config
      module procedure :: new_merge_config
   end interface toml_merge_config


contains


!> Create a new merge configuration
pure function new_merge_config(table, array, keyval) result(config)

   !> Policy for merging tables
   character(*), intent(in), optional :: table

   !> Policy for merging arrays
   character(*), intent(in), optional :: array

   !> Policy for merging values
   character(*), intent(in), optional :: keyval

   !> Merge policy
   type(toml_merge_config) :: config

   if (present(table)) call set_enum(config%table, table)
   if (present(array)) call set_enum(config%array, array)
   if (present(keyval)) call set_enum(config%keyval, keyval)

contains

   pure subroutine set_enum(enum, str)
      character(*), intent(in) :: str
      integer, intent(inout) :: enum

      select case(str)
      case("append")
         enum = merge_policy%append
      case("overwrite")
         enum = merge_policy%overwrite
      case("preserve")
         enum = merge_policy%preserve
      end select
   end subroutine set_enum

end function new_merge_config


!> Merge TOML tables by appending their values
recursive subroutine merge_table(lhs, rhs, config)

   !> Instance of table to merge into
   class(toml_table), intent(inout) :: lhs

   !> Instance of table to be merged
   class(toml_table), intent(inout) :: rhs

   !> Merge policy
   type(toml_merge_config), intent(in), optional :: config

   type(toml_merge_config) :: policy
   type(toml_key), allocatable :: list(:)
   class(toml_value), pointer :: ptr1, ptr2
   class(toml_keyval), pointer :: kv
   class(toml_value), allocatable :: tmp
   logical :: has_key
   integer :: i, n, stat

   policy = toml_merge_config()
   if (present(config)) policy = config

   call rhs%get_keys(list)
   n = size(list, 1)

   do i = 1, n
      if (allocated(tmp)) deallocate(tmp)
      call rhs%get(list(i)%key, ptr1)
      has_key = lhs%has_key(list(i)%key)
      select type(ptr1)
      class is(toml_keyval)
         if (has_key .and. policy%keyval == merge_policy%overwrite) then
            call lhs%delete(list(i)%key)
            has_key = .false.
         end if
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            kv => cast_to_keyval(tmp)
            kv%origin_value = 0
            kv%origin = 0
            call lhs%push_back(tmp, stat)
         end if

      class is(toml_array)
         if (has_key .and. policy%array == merge_policy%overwrite) then
            call lhs%delete(list(i)%key)
            has_key = .false.
         end if
         if (has_key .and. policy%array == merge_policy%append) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_array)
               call merge_array(ptr2, ptr1)
            end select
         end if
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            tmp%origin = 0
            call lhs%push_back(tmp, stat)
         end if

      class is(toml_table)
         if (has_key .and. policy%table == merge_policy%overwrite) then
            call lhs%delete(list(i)%key)
            has_key = .false.
         end if
         if (has_key .and. policy%table == merge_policy%append) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_table)
               call merge_table(ptr2, ptr1, policy)
            end select
         end if
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            tmp%origin = 0
            call lhs%push_back(tmp, stat)
         end if
      end select
   end do

end subroutine merge_table


!> Append values from one TOML array to another
recursive subroutine merge_array(lhs, rhs)

   !> Instance of array to merge into
   class(toml_array), intent(inout) :: lhs

   !> Instance of array to be merged
   class(toml_array), intent(inout) :: rhs

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: tmp
   integer :: n, i, stat

   n = len(rhs)

   do i = 1, n
      call rhs%get(i, ptr)
      if (allocated(tmp)) deallocate(tmp)
      allocate(tmp, source=ptr)
      call lhs%push_back(tmp, stat)
   end do

end subroutine merge_array


end module tomlf_build_merge


!>>>>> build/dependencies/toml-f/src/tomlf/de/parser.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a parser for transforming a token stream to TOML datastructures.
module tomlf_de_parser
   use tomlf_constants, only : tfc, tfr, tfi, TOML_NEWLINE
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_context, only : toml_context
   use tomlf_de_abc, only : toml_lexer => abstract_lexer
   use tomlf_de_token, only : toml_token, token_kind, stringify
   use tomlf_diagnostic, only : render, toml_diagnostic, toml_label, toml_level
   use tomlf_terminal, only : toml_terminal
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_value, toml_key, &
      & add_table, add_array, add_keyval, cast_to_table, cast_to_array, len
   implicit none
   private

   public :: toml_parser, toml_parser_config, parse


   !> Configuration of the TOML parser
   type :: toml_parser_config
      !> Use colorful output for diagnostics
      type(toml_terminal) :: color = toml_terminal()
      !> Record all tokens
      integer :: context_detail = 0
   end type toml_parser_config

   interface toml_parser_config
      module procedure :: new_parser_config
   end interface toml_parser_config

   !> TOML parser
   type :: toml_parser
      !> Current token
      type(toml_token) :: token
      !> Table containing the document root
      type(toml_table), allocatable :: root
      !> Pointer to the currently processed table
      type(toml_table), pointer :: current
      !> Diagnostic produced while parsing
      type(toml_diagnostic), allocatable :: diagnostic
      !> Context for producing diagnostics
      type(toml_context) :: context
      !> Configuration of the parser
      type(toml_parser_config) :: config
   end type toml_parser

contains

!> Create a new instance of the TOML parser
subroutine new_parser(parser, config)
   !> Instance of the parser
   type(toml_parser), intent(out), target :: parser
   !> Configuration of the parser
   type(toml_parser_config), intent(in), optional :: config

   parser%token = toml_token(token_kind%newline, 0, 0)
   parser%root = toml_table()
   parser%current => parser%root
   parser%config = toml_parser_config()
   if (present(config)) parser%config = config
end subroutine new_parser

!> Create new configuration for the TOML parser
pure function new_parser_config(color, context_detail) result(config)
   !> Configuration of the parser
   type(toml_parser_config) :: config
   !> Color support for diagnostics
   logical, intent(in), optional :: color
   !> Record all tokens
   integer, intent(in), optional :: context_detail

   if (present(color)) config%color = toml_terminal(color)
   if (present(context_detail)) config%context_detail = context_detail
end function new_parser_config

!> Parse TOML document and return root table
subroutine parse(lexer, table, config, context, error)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> TOML data structure
   type(toml_table), allocatable, intent(out) :: table
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handler
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_parser) :: parser

   call new_parser(parser, config)
   call parse_root(parser, lexer)

   if (present(error) .and. allocated(parser%diagnostic)) then
      call make_error(error, parser%diagnostic, lexer, parser%config%color)
   end if
   if (allocated(parser%diagnostic)) return

   call move_alloc(parser%root, table)

   if (present(context)) then
      context = parser%context
      call lexer%get_info("filename", context%filename)
      call lexer%get_info("source", context%source)
   end if
end subroutine parse

!> Parse the root table
subroutine parse_root(parser, lexer)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   do while(.not.allocated(parser%diagnostic) .and. parser%token%kind /= token_kind%eof)
      select case(parser%token%kind)
      case(token_kind%newline, token_kind%whitespace, token_kind%comment)
         call next_token(parser, lexer)

      case(token_kind%keypath, token_kind%string, token_kind%literal)
         call parse_keyval(parser, lexer, parser%current)

      case(token_kind%lbracket)
         call parse_table_header(parser, lexer)

      case default
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Invalid syntax", &
            & "unexpected "//stringify(parser%token))
      end select
   end do
end subroutine parse_root


!> Parse a table or array of tables header
subroutine parse_table_header(parser, lexer)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   type(toml_array), pointer :: array
   type(toml_table), pointer :: table
   class(toml_value), pointer :: ptr
   type(toml_key) :: key
   logical :: array_of_tables

   integer, parameter :: initial_size = 8
   integer :: top
   type(toml_key), allocatable :: stack(:)
   type(toml_token), allocatable :: leading_whitespace, trailing_whitespace


   call consume(parser, lexer, token_kind%lbracket)
   if (allocated(parser%diagnostic)) return

   if (parser%token%kind == token_kind%whitespace) then
      leading_whitespace = parser%token
      call next_token(parser, lexer)
   end if

   array_of_tables = parser%token%kind == token_kind%lbracket

   if (array_of_tables) then
      call next_token(parser, lexer)
      if (parser%token%kind == token_kind%whitespace) then
         call next_token(parser, lexer)
      end if
   end if

   call fill_stack(lexer, parser, top, stack)
   if (allocated(parser%diagnostic)) return

   key = stack(top)
   top = top - 1

   call walk_stack(parser, top, stack)

   if (array_of_tables) then
      call parser%current%get(key%key, ptr)
      if (associated(ptr)) then
         array => cast_to_array(ptr)
         if (.not.associated(array)) then
            call duplicate_key_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(ptr%origin), &
               & "Key '"//key%key//"' already exists")
            return
         end if
         if (array%inline) then
            call semantic_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(array%origin), &
               & "Array of tables cannot extend inline array", &
               & "extended here", &
               & "defined as inline")
            return
         end if
      else
         call add_array(parser%current, key, array)
         array%inline = .false.
      end if
      call add_table(array, table)
   else
      call parser%current%get(key%key, ptr)
      if (associated(ptr)) then
         table => cast_to_table(ptr)
         if (associated(table)) then
            if (.not.table%implicit) nullify(table)
         end if

         if (.not.associated(table)) then
            call duplicate_key_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(ptr%origin), &
               & "Key '"//key%key//"' already exists")
            return
         end if
      else
         call add_table(parser%current, key, table)
      end if
   end if

   parser%current => table

   call consume(parser, lexer, token_kind%rbracket)
   if (allocated(parser%diagnostic)) return

   if (array_of_tables) then
      if (parser%token%kind == token_kind%whitespace) then
         trailing_whitespace = parser%token
         call next_token(parser, lexer)
      end if
      call consume(parser, lexer, token_kind%rbracket)
      if (allocated(parser%diagnostic)) return
   end if

   if (array_of_tables .and. allocated(leading_whitespace)) then
      call syntax_error(parser%diagnostic, lexer, leading_whitespace, &
         & "Malformatted array of table header encountered", &
         & "whitespace not allowed in header")
      return
   end if

   if (array_of_tables .and. allocated(trailing_whitespace)) then
      call syntax_error(parser%diagnostic, lexer, trailing_whitespace, &
         & "Malformatted array of table header encountered", &
         & "whitespace not allowed in header")
      return
   end if

   do while(parser%token%kind == token_kind%whitespace)
      call next_token(parser, lexer)
   end do

   if (parser%token%kind == token_kind%comment) then
      call next_token(parser, lexer)
   end if

   if (all(parser%token%kind /= [token_kind%newline, token_kind%eof])) then
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Unexpected "//stringify(parser%token)//" after table header", &
         & "expected newline")
   end if

contains

   !> Fill the stack with tokens
   subroutine fill_stack(lexer, parser, top, stack)
      class(toml_lexer), intent(inout) :: lexer
      type(toml_parser), intent(inout) :: parser
      !> Depth of the table key stack
      integer, intent(out) :: top
      !> Stack of all keys in the table header
      type(toml_key), allocatable, intent(out) :: stack(:)

      top = 0
      allocate(stack(initial_size))

      do
         if (top >= size(stack)) then
            call resize(stack)
         end if

         if (all(parser%token%kind /= [token_kind%string, token_kind%literal, &
            & token_kind%keypath])) then
            call syntax_error(parser%diagnostic, lexer, parser%token, &
               & "Missing key for table header", &
               & "unexpected "//stringify(parser%token))
            return
         end if

         top = top + 1
         call extract_key(parser, lexer, stack(top))

         call next_token(parser, lexer)
         if (parser%token%kind == token_kind%whitespace) &
            & call next_token(parser, lexer)

         if (parser%token%kind == token_kind%rbracket) exit

         call consume(parser, lexer, token_kind%dot)
         if (allocated(parser%diagnostic)) return
         if (parser%token%kind == token_kind%whitespace) &
            & call next_token(parser, lexer)
      end do

      if (top <= 0) then
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Empty table header", &
            & "expected table header")
      end if

   end subroutine fill_stack

   !> Walk the key stack to fetch the correct table, create implicit tables as necessary
   subroutine walk_stack(parser, top, stack)
      type(toml_parser), intent(inout), target :: parser
      !> Depth of the table key stack
      integer, intent(in) :: top
      !> Stack of all keys in the table header
      type(toml_key), intent(in), target :: stack(:)

      type(toml_table), pointer :: table, tmp_tbl
      type(toml_array), pointer :: array
      type(toml_key), pointer :: key
      class(toml_value), pointer :: ptr
      integer :: it

      table => parser%root

      do it = 1, top
         key => stack(it)

         if (.not.table%has_key(key%key)) then
            call add_table(table, key, tmp_tbl)
            if (associated(tmp_tbl)) then
               tmp_tbl%implicit = .true.
            end if
         end if
         call table%get(key%key, ptr)

         table => cast_to_table(ptr)
         if (.not.associated(table)) then
            array => cast_to_array(ptr)
            if (associated(array) .and. len(array) > 0) then
               call array%get(len(array), ptr)
               if (associated(ptr)) table => cast_to_table(ptr)
            end if
            if (.not.associated(table)) then
               if (associated(ptr)) then
                  call duplicate_key_error(parser%diagnostic, lexer, &
                     & parser%context%token(key%origin), &
                     & parser%context%token(ptr%origin), &
                     & "Key '"//key%key//"' already exists")
               else
                  call duplicate_key_error(parser%diagnostic, lexer, &
                     & parser%context%token(key%origin), &
                     & parser%context%token(key%origin), &
                     & "Key '"//key%key//"' already exists")
               end if
               return
            end if
         end if

         if (table%inline) then
            call semantic_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(table%origin), &
               & "Inline table '"//key%key//"' cannot be used as a key", &
               & "inline table cannot be extended", &
               & "defined as inline first")
         end if
      end do

      parser%current => table
   end subroutine walk_stack

   !> Change size of the stack
   subroutine resize(stack, n)
      !> Stack of keys to be resized
      type(toml_key), allocatable, intent(inout) :: stack(:)
      !> New size of the stack
      integer, intent(in), optional :: n

      type(toml_key), allocatable :: tmp(:)
      integer :: m

      if (present(n)) then
         m = n
      else
         if (allocated(stack)) then
            m = size(stack)
            m = m + m/2 + 1
         else
            m = initial_size
         end if
      end if

      if (allocated(stack)) then
         call move_alloc(stack, tmp)
         allocate(stack(m))

         m = min(size(tmp), m)
         stack(:m) = tmp(:m)

         deallocate(tmp)
      else
         allocate(stack(m))
      end if
   end subroutine resize

end subroutine parse_table_header

!> Parse key value pairs in a table body
recursive subroutine parse_keyval(parser, lexer, table)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current table
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   type(toml_key) :: key

   call extract_key(parser, lexer, key)
   call next_token(parser, lexer)
   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   if (parser%token%kind == token_kind%dot) then
      call table%get(key%key, ptr)
      if (associated(ptr)) then
         tptr => cast_to_table(ptr)
         if (.not.associated(tptr)) then
            call duplicate_key_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(ptr%origin), &
               & "Key '"//key%key//"' already exists")
            return
         end if
      else
         call add_table(table, key, tptr)
      end if

      if (.not.associated(tptr)) then
         call duplicate_key_error(parser%diagnostic, lexer, &
            & parser%context%token(key%origin), &
            & parser%context%token(key%origin), &
            & "Key '"//key%key//"' already exists")
         return
      end if

      if (tptr%inline) then
         call semantic_error(parser%diagnostic, lexer, &
            & parser%context%token(key%origin), &
            & parser%context%token(tptr%origin), &
            & "Cannot add keys to inline tables", &
            & "inline table cannot be extended", &
            & "defined as inline first")
         return
      end if

      call next_token(parser, lexer)
      if (parser%token%kind == token_kind%whitespace) &
         call next_token(parser, lexer)

      if (any(parser%token%kind == [token_kind%keypath, token_kind%string, &
         & token_kind%literal])) then
         call parse_keyval(parser, lexer, tptr)
      else
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Invalid syntax", &
            & "expected key")
      end if
      return
   end if

   call consume(parser, lexer, token_kind%equal)
   if (allocated(parser%diagnostic)) return

   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   call table%get(key%key, ptr)
   if (associated(ptr)) then
      call duplicate_key_error(parser%diagnostic, lexer, &
         & parser%context%token(key%origin), &
         & parser%context%token(ptr%origin), &
         & "Key '"//key%key//"' already exists")
      return
   end if

   select case(parser%token%kind)
   case default
      call add_keyval(table, key, vptr)
      call parse_value(parser, lexer, vptr)

   case(token_kind%nil)
      call next_token(parser, lexer)

   case(token_kind%lbracket)
      call add_array(table, key, aptr)
      call parse_inline_array(parser, lexer, aptr)

   case(token_kind%lbrace)
      call add_table(table, key, tptr)
      call parse_inline_table(parser, lexer, tptr)

   end select
   if (allocated(parser%diagnostic)) return

   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   if (parser%token%kind == token_kind%comment) &
      call next_token(parser, lexer)
end subroutine parse_keyval

recursive subroutine parse_inline_array(parser, lexer, array)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current array
   type(toml_array), intent(inout) :: array

   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   integer, parameter :: skip_tokens(*) = &
      [token_kind%whitespace, token_kind%comment, token_kind%newline]

   array%inline = .true.
   call consume(parser, lexer, token_kind%lbracket)

   inline_array: do while(.not.allocated(parser%diagnostic))
      do while(any(parser%token%kind == skip_tokens))
         call next_token(parser, lexer)
      end do

      select case(parser%token%kind)
      case(token_kind%rbracket)
         exit inline_array

      case default
         call add_keyval(array, vptr)
         call parse_value(parser, lexer, vptr)

      case(token_kind%nil)
         call next_token(parser, lexer)

      case(token_kind%lbracket)
         call add_array(array, aptr)
         call parse_inline_array(parser, lexer, aptr)

      case(token_kind%lbrace)
         call add_table(array, tptr)
         call parse_inline_table(parser, lexer, tptr)

      end select
      if (allocated(parser%diagnostic)) exit inline_array

      do while(any(parser%token%kind == skip_tokens))
         call next_token(parser, lexer)
      end do

      if (parser%token%kind == token_kind%comma) then
         call next_token(parser, lexer)
         cycle inline_array
      end if
      exit inline_array
   end do inline_array
   if (allocated(parser%diagnostic)) return

   call consume(parser, lexer, token_kind%rbracket)
end subroutine parse_inline_array

recursive subroutine parse_inline_table(parser, lexer, table)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current table
   type(toml_table), intent(inout) :: table

   integer, parameter :: skip_tokens(*) = &
      [token_kind%whitespace, token_kind%comment, token_kind%newline]

   table%inline = .true.
   call consume(parser, lexer, token_kind%lbrace)

   inline_table: do while(.not.allocated(parser%diagnostic))
      do while(any(parser%token%kind == skip_tokens))
         call next_token(parser, lexer)
      end do

      select case(parser%token%kind)
      case(token_kind%rbrace)
         exit inline_table

      case default
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Invalid character in inline table", &
            & "unexpected "//stringify(parser%token))

      case(token_kind%keypath, token_kind%string, token_kind%literal)
         call parse_keyval(parser, lexer, table)

      end select
      if (allocated(parser%diagnostic)) exit inline_table

      do while(any(parser%token%kind == skip_tokens))
         call next_token(parser, lexer)
      end do

      if (parser%token%kind == token_kind%comma) then
         call next_token(parser, lexer)
         cycle inline_table
      end if
      exit inline_table
   end do inline_table
   if (allocated(parser%diagnostic)) return

   call consume(parser, lexer, token_kind%rbrace)
end subroutine parse_inline_table

subroutine parse_value(parser, lexer, kval)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current key value pair
   type(toml_keyval), intent(inout) :: kval

   select case(parser%token%kind)
   case default
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Invalid expression for value", &
         & "unexpected "//stringify(parser%token))

   case(token_kind%unclosed)
      ! Handle runaway expressions separately
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Inline expression contains unclosed or runaway group", &
         & "unclosed inline expression")

   case(token_kind%string, token_kind%mstring, token_kind%literal, token_kind%mliteral, &
         & token_kind%int, token_kind%float, token_kind%bool, token_kind%datetime)
      call extract_value(parser, lexer, kval)

      call next_token(parser, lexer)
   end select
end subroutine parse_value

!> Check whether the current token is the expected one and advance the lexer
subroutine consume(parser, lexer, kind)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Expected token kind
   integer, intent(in) :: kind

   if (parser%token%kind /= kind) then
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Invalid syntax in this context", &
         & "expected "//stringify(toml_token(kind)))
      return
   end if

   call next_token(parser, lexer)
end subroutine consume

!> Create diagnostic for invalid syntax
subroutine syntax_error(diagnostic, lexer, token, message, label)
   !> Diagnostic for the syntax error
   type(toml_diagnostic), allocatable, intent(out) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(inout) :: lexer
   !> Token that caused the error
   type(toml_token), intent(in) :: token
   !> Message for the error
   character(len=*), intent(in) :: message
   !> Label for the token
   character(len=*), intent(in) :: label

   character(:, tfc), allocatable :: filename

   call lexer%get_info("filename", filename)

   allocate(diagnostic)
   diagnostic = toml_diagnostic( &
      & toml_level%error, &
      & message, &
      & filename, &
      & [toml_label(toml_level%error, token%first, token%last, label, .true.)])
end subroutine syntax_error

!> Create diagnostic for incorrect semantics
subroutine semantic_error(diagnostic, lexer, token1, token2, message, label1, label2)
   !> Diagnostic for the duplicate key error
   type(toml_diagnostic), allocatable, intent(out) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(inout) :: lexer
   !> Token identifying the duplicate key
   type(toml_token), intent(in) :: token1
   !> Token identifying the original key
   type(toml_token), intent(in) :: token2
   !> Message for the error
   character(len=*), intent(in) :: message
   !> Label for the first token
   character(len=*), intent(in) :: label1
   !> Label for the second token
   character(len=*), intent(in) :: label2

   character(:, tfc), allocatable :: filename

   call lexer%get_info("filename", filename)

   allocate(diagnostic)
   diagnostic = toml_diagnostic( &
      & toml_level%error, &
      & message, &
      & filename, &
      & [toml_label(toml_level%error, token1%first, token1%last, label1, .true.), &
      &  toml_label(toml_level%info, token2%first, token2%last, label2, .false.)])
end subroutine semantic_error

!> Create a diagnostic for a duplicate key entry
subroutine duplicate_key_error(diagnostic, lexer, token1, token2, message)
   !> Diagnostic for the duplicate key error
   type(toml_diagnostic), allocatable, intent(out) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(inout) :: lexer
   !> Token identifying the duplicate key
   type(toml_token), intent(in) :: token1
   !> Token identifying the original key
   type(toml_token), intent(in) :: token2
   !> Message for the error
   character(len=*), intent(in) :: message

   call semantic_error(diagnostic, lexer, token1, token2, &
      & message, "key already used", "first defined here")
end subroutine duplicate_key_error

!> Create an error from a diagnostic
subroutine make_error(error, diagnostic, lexer, color)
   !> Error to be created
   type(toml_error), allocatable, intent(out) :: error
   !> Diagnostic to be used
   type(toml_diagnostic), intent(in) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(in) :: lexer
   !> Use colorful error messages
   type(toml_terminal), intent(in) :: color

   character(len=:), allocatable :: str

   allocate(error)
   call lexer%get_info("source", str)
   error%message = render(diagnostic, str, color)
   error%stat = toml_stat%fatal
end subroutine make_error

!> Wrapper around the lexer to retrieve the next token.
!> Allows to record the tokens for keys and values in the parser context
subroutine next_token(parser, lexer)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   call lexer%next(parser%token)

   select case(parser%token%kind)
   case(token_kind%keypath, token_kind%string, token_kind%literal, token_kind%int, &
         & token_kind%float, token_kind%bool, token_kind%datetime)
      call parser%context%push_back(parser%token)
   case(token_kind%newline, token_kind%dot, token_kind%comma, token_kind%equal, &
         & token_kind%lbrace, token_kind%rbrace, token_kind%lbracket, token_kind%rbracket)
      if (parser%config%context_detail > 0) &
         call parser%context%push_back(parser%token)
   case default
      if (parser%config%context_detail > 1) &
         call parser%context%push_back(parser%token)
   end select
end subroutine next_token

!> Extract key from token
subroutine extract_key(parser, lexer, key)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Key to be extracted
   type(toml_key), intent(out) :: key

   call lexer%extract(parser%token, key%key)
   key%origin = parser%context%top
end subroutine extract_key

!> Extract value from token
subroutine extract_value(parser, lexer, kval)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Value to be extracted
   type(toml_keyval), intent(inout) :: kval

   character(:, tfc), allocatable :: sval
   real(tfr) :: rval
   integer(tfi) :: ival
   logical :: bval
   type(toml_datetime) :: dval

   kval%origin_value = parser%context%top

   select case(parser%token%kind)
   case(token_kind%string, token_kind%literal, token_kind%mstring, token_kind%mliteral)
      call lexer%extract_string(parser%token, sval)
      call kval%set(sval)

   case(token_kind%int)
      call lexer%extract_integer(parser%token, ival)
      call kval%set(ival)

   case(token_kind%float)
      call lexer%extract_float(parser%token, rval)
      call kval%set(rval)

   case(token_kind%bool)
      call lexer%extract_bool(parser%token, bval)
      call kval%set(bval)

   case(token_kind%datetime)
      call lexer%extract_datetime(parser%token, dval)
      call kval%set(dval)
   end select
end subroutine extract_value

!> Try to retrieve TOML table with key or create it
subroutine get_table(table, key, ptr, stat)
   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table
   !> Key for the new table
   type(toml_key), intent(in) :: key
   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr
   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)
   call table%get(key%key, tmp)

   if (associated(tmp)) then
      ptr => cast_to_table(tmp)
      if (present(stat)) stat = merge(toml_stat%success, toml_stat%fatal, associated(ptr))
   else
      call add_table(table, key, ptr, stat)
   end if
end subroutine get_table

end module tomlf_de_parser


!>>>>> build/dependencies/toml-f/src/tomlf/de.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML deserialization module
!>
!> This module provides interfaces for loading and parsing TOML documents
!> from various sources (files, strings, and I/O units).
!>
!> The primary interfaces are:
!>
!> - [[toml_load]]: Load TOML from a file path or connected unit
!> - [[toml_loads]]: Parse TOML from a string
!>
!> All parsing functions return an allocatable [[toml_table]] that contains
!> the parsed document. If parsing fails, the table will not be allocated
!> and error information is provided via the optional error argument.
module tomlf_de
   use tomlf_constants, only : tfc, TOML_NEWLINE
   use tomlf_de_context, only : toml_context
   use tomlf_de_lexer, only : toml_lexer, new_lexer_from_string, new_lexer_from_unit, &
      & new_lexer_from_file
   use tomlf_de_parser, only : parse, toml_parser_config
   use tomlf_diagnostic, only : toml_level
   use tomlf_error, only : toml_error
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: toml_parse
   public :: toml_load, toml_loads
   public :: toml_context, toml_parser_config, toml_level


   !> Parse a TOML document.
   !>
   !> This interface is deprecated in favor of [[toml_load]] and [[toml_loads]]
   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse

   !> Load a TOML data structure from the provided source
   interface toml_load
      module procedure :: toml_load_file
      module procedure :: toml_load_unit
   end interface toml_load

   !> Load a TOML data structure from a string
   interface toml_loads
      module procedure :: toml_load_string
   end interface toml_loads


contains


!> Parse a TOML input from a given IO unit.
!>
!> @note This procedure is deprectated
subroutine toml_parse_unit(table, unit, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> Unit to read from
   integer, intent(in) :: unit
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   call toml_load(table, unit, error=error)
end subroutine toml_parse_unit

!> Wrapper to parse a TOML string.
!>
!> @note This procedure is deprectated
subroutine toml_parse_string(table, string, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> String containing TOML document
   character(len=*), intent(in), target :: string
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   call toml_loads(table, string, error=error)
end subroutine toml_parse_string

!> Load TOML data structure from file
subroutine toml_load_file(table, filename, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   character(*, tfc), intent(in) :: filename
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_file(lexer, filename, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine toml_load_file

!> Load TOML data structure from unit
subroutine toml_load_unit(table, io, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> Unit to read from
   integer, intent(in) :: io
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_unit(lexer, io, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine toml_load_unit

!> Load TOML data structure from string
subroutine toml_load_string(table, string, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> String containing TOML document
   character(*, tfc), intent(in) :: string
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer

   call new_lexer_from_string(lexer, string)
   call parse(lexer, table, config, context, error)
end subroutine toml_load_string

end module tomlf_de


!>>>>> build/dependencies/toml-f/src/tomlf/build/array.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build TOML arrays.
!>
!> This build module defines a high level interface to work with TOML arrays
!> and construct them in a convenient way.
!>
!> The access to the array elements happens by position in the array, the indexing
!> is one based, following the language convention of Fortran. All functions
!> will only allow access of elements within the bounds of the array, specifying
!> indices out-of-bounds should be save, as it only sets the status of operation.
!> The getter functions allow access to other tables and arrays as well as
!> convenient wrappers to retrieve value data
!>
!> The setter functions are somewhat weaker compared to the setter functions
!> available for TOML tables. To limit the potential havoc this routines can
!> cause they can only access the array within its bounds. Setting a value to
!> another value will overwrite it, while setting a value to a table or an array
!> will fail, for safety reasons.
!>
!> To (re)build an array appending to it is the best choice, tables and arrays
!> should always be create by using the corresponding `add_table` and `add_array`
!> function. While this can become cumbersome for values, the setter routines
!> allow out-of-bound access to for the next element in an array and will indeed
!> just append a new value to it.
module tomlf_build_array
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_datetime, only : toml_datetime
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, &
      & cast_to_table, cast_to_array, cast_to_keyval, initialized, len
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML arrays
   interface set_value
      module procedure :: set_elem_value_string
      module procedure :: set_elem_value_float_sp
      module procedure :: set_elem_value_float_dp
      module procedure :: set_elem_value_int_i1
      module procedure :: set_elem_value_int_i2
      module procedure :: set_elem_value_int_i4
      module procedure :: set_elem_value_int_i8
      module procedure :: set_elem_value_bool
      module procedure :: set_elem_value_datetime
      module procedure :: set_array_value_float_sp
      module procedure :: set_array_value_float_dp
      module procedure :: set_array_value_int_i1
      module procedure :: set_array_value_int_i2
      module procedure :: set_array_value_int_i4
      module procedure :: set_array_value_int_i8
      module procedure :: set_array_value_bool
      module procedure :: set_array_value_datetime
   end interface set_value


   !> Getter functions to manipulate TOML arrays
   interface get_value
      module procedure :: get_elem_table
      module procedure :: get_elem_array
      module procedure :: get_elem_keyval
      module procedure :: get_elem_value_string
      module procedure :: get_elem_value_float_sp
      module procedure :: get_elem_value_float_dp
      module procedure :: get_elem_value_int_i1
      module procedure :: get_elem_value_int_i2
      module procedure :: get_elem_value_int_i4
      module procedure :: get_elem_value_int_i8
      module procedure :: get_elem_value_bool
      module procedure :: get_elem_value_datetime
      module procedure :: get_array_value_float_sp
      module procedure :: get_array_value_float_dp
      module procedure :: get_array_value_int_i1
      module procedure :: get_array_value_int_i2
      module procedure :: get_array_value_int_i4
      module procedure :: get_array_value_int_i8
      module procedure :: get_array_value_bool
      module procedure :: get_array_value_datetime
   end interface get_value


contains


subroutine get_elem_table(array, pos, ptr, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   class(toml_value), pointer :: tmp

   if (.not.initialized(array)) call new_array(array)

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      ptr => cast_to_table(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (present(stat)) stat = toml_stat%fatal
      if (present(origin)) origin = array%origin
   end if

end subroutine get_elem_table


subroutine get_elem_array(array, pos, ptr, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   class(toml_value), pointer :: tmp

   if (.not.initialized(array)) call new_array(array)

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      ptr => cast_to_array(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (present(stat)) stat = toml_stat%fatal
      if (present(origin)) origin = array%origin
   end if

end subroutine get_elem_array


subroutine get_elem_keyval(array, pos, ptr, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   class(toml_value), pointer :: tmp

   if (.not.initialized(array)) call new_array(array)

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      ptr => cast_to_keyval(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (present(stat)) stat = toml_stat%fatal
      if (present(origin)) origin = array%origin
   end if

end subroutine get_elem_keyval


!> Retrieve TOML value as deferred-length character
subroutine get_elem_value_string(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_string


!> Retrieve TOML value as single precision floating point number
subroutine get_elem_value_float_sp(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_sp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine get_elem_value_float_dp(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_dp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_float_dp


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i1(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i1


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i2(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i2


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i4(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i4


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i8(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i8


!> Retrieve TOML value as boolean
subroutine get_elem_value_bool(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   logical, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_bool


!> Retrieve TOML value as datetime
subroutine get_elem_value_datetime(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   type(toml_datetime), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine set_elem_value_string(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_string


!> Retrieve TOML value as single precision floating point number
subroutine set_elem_value_float_sp(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine set_elem_value_float_dp(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_float_dp


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i1(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i1


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i2(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i2


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i4(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i4


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i8(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i8


!> Retrieve TOML value as boolean value
subroutine set_elem_value_bool(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_bool


!> Retrieve TOML value as datetime value
subroutine set_elem_value_datetime(array, pos, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat, origin)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_datetime


!> Retrieve TOML value as single precision floating point number
subroutine get_array_value_float_sp(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Floating point value
   real(tf_sp), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine get_array_value_float_dp(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Floating point value
   real(tf_dp), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_float_dp


!> Retrieve TOML value as integer value
subroutine get_array_value_int_i1(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i1), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_int_i1


!> Retrieve TOML value as integer value
subroutine get_array_value_int_i2(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i2), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_int_i2


!> Retrieve TOML value as integer value
subroutine get_array_value_int_i4(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i4), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_int_i4


!> Retrieve TOML value as integer value
subroutine get_array_value_int_i8(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i8), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_int_i8


!> Retrieve TOML value as boolean
subroutine get_array_value_bool(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   logical, allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_bool


!> Retrieve TOML value as datetime
subroutine get_array_value_datetime(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   type(toml_datetime), allocatable, intent(out) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it, info

   info = 0
   allocate(val(len(array)))
   do it = 1, size(val)
      call get_value(array, it, val(it), info, origin)
      if (info /= 0) exit
   end do
   if (info /= 0) deallocate(val)
   if (present(stat)) stat = info
   if (present(origin) .and. info == 0) origin = array%origin

end subroutine get_array_value_datetime


!> Retrieve TOML value as single precision floating point number
subroutine set_array_value_float_sp(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Floating point value
   real(tf_sp), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine set_array_value_float_dp(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Floating point value
   real(tf_dp), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_float_dp


!> Retrieve TOML value as integer value
subroutine set_array_value_int_i1(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i1), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_int_i1


!> Retrieve TOML value as integer value
subroutine set_array_value_int_i2(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i2), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_int_i2


!> Retrieve TOML value as integer value
subroutine set_array_value_int_i4(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i4), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_int_i4


!> Retrieve TOML value as integer value
subroutine set_array_value_int_i8(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Integer value
   integer(tf_i8), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_int_i8


!> Retrieve TOML value as boolean value
subroutine set_array_value_bool(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Boolean value
   logical, intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_bool


!> Retrieve TOML value as datetime value
subroutine set_array_value_datetime(array, val, stat, origin)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Datetime value
   type(toml_datetime), intent(in) :: val(:)

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   class(toml_value), allocatable :: ptr

   do while(len(array) > size(val))
      call array%pop(ptr)
   end do

   do it = 1, size(val)
      call set_value(array, it, val(it), stat, origin)
   end do
   if (present(origin)) origin = array%origin

end subroutine set_array_value_datetime


end module tomlf_build_array


!>>>>> build/dependencies/toml-f/src/tomlf/build/table.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build TOML tables
!>
!> The build module defines a high level interface to work with TOML tables
!> and construct them in a convenient way.
!>
!> The getter functions allow to both retrieve and set values, to easily
!> support default values when reading from a TOML data structure.
!> Using the getter function with a default value specified will request
!> the respective setter function to add it to the table if it was not
!> found in the first place.
!>
!> This allows to build a TOML table using only the getter functions, which
!> represents the finally read values for the applications.
!>
!> Note that neither setter nor getter functions can overwrite existing
!> TOML values for safety reasons, request the deletion on the respective
!> key from the TOML table and than set it. The deletion of a subtable or
!> array will recursively destroy the contained data nodes.
module tomlf_build_table
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_datetime, only : toml_datetime
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, &
      & toml_key, cast_to_table, cast_to_array, cast_to_keyval, initialized, &
      & len
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML tables
   interface set_value
      module procedure :: set_child_value_float_sp
      module procedure :: set_child_value_float_dp
      module procedure :: set_child_value_integer_i1
      module procedure :: set_child_value_integer_i2
      module procedure :: set_child_value_integer_i4
      module procedure :: set_child_value_integer_i8
      module procedure :: set_child_value_bool
      module procedure :: set_child_value_datetime
      module procedure :: set_child_value_string
      module procedure :: set_key_value_float_sp
      module procedure :: set_key_value_float_dp
      module procedure :: set_key_value_integer_i1
      module procedure :: set_key_value_integer_i2
      module procedure :: set_key_value_integer_i4
      module procedure :: set_key_value_integer_i8
      module procedure :: set_key_value_bool
      module procedure :: set_key_value_datetime
      module procedure :: set_key_value_string
   end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
      module procedure :: get_child_table
      module procedure :: get_child_array
      module procedure :: get_child_keyval
      module procedure :: get_child_value_float_sp
      module procedure :: get_child_value_float_dp
      module procedure :: get_child_value_integer_i1
      module procedure :: get_child_value_integer_i2
      module procedure :: get_child_value_integer_i4
      module procedure :: get_child_value_integer_i8
      module procedure :: get_child_value_bool
      module procedure :: get_child_value_datetime
      module procedure :: get_child_value_string
      module procedure :: get_key_table
      module procedure :: get_key_array
      module procedure :: get_key_keyval
      module procedure :: get_key_value_float_sp
      module procedure :: get_key_value_float_dp
      module procedure :: get_key_value_integer_i1
      module procedure :: get_key_value_integer_i2
      module procedure :: get_key_value_integer_i4
      module procedure :: get_key_value_integer_i8
      module procedure :: get_key_value_bool
      module procedure :: get_key_value_datetime
      module procedure :: get_key_value_string
   end interface get_value


contains


subroutine get_key_table(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, ptr, requested, stat, origin)

end subroutine get_key_table


subroutine get_key_array(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, ptr, requested, stat, origin)

end subroutine get_key_array


subroutine get_key_keyval(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, ptr, requested, stat, origin)

end subroutine get_key_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_key_value_float_sp(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_key_value_float_dp(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_key_value_integer_i1(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_key_value_integer_i2(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_key_value_integer_i4(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_key_value_integer_i8(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_key_value_bool(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_bool


!> Retrieve TOML value as datetime
subroutine get_key_value_datetime(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Default datetime value
   type(toml_datetime), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_key_value_string(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_string


!> Set TOML value to single precision float
subroutine set_key_value_float_sp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_float_sp


!> Set TOML value to double precision float
subroutine set_key_value_float_dp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_float_dp


!> Set TOML value to one byte integer
subroutine set_key_value_integer_i1(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_key_value_integer_i2(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_key_value_integer_i4(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_key_value_integer_i8(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i8


!> Set TOML value to logical
subroutine set_key_value_bool(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_bool


!> Set TOML value to datetime
subroutine set_key_value_datetime(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_datetime


!> Set TOML value to deferred-length character
subroutine set_key_value_string(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_string


subroutine get_child_table(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (.not.initialized(table)) call new_table(table)

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      ptr => cast_to_table(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (is_requested) then
         call add_table(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
      if (present(origin)) origin = table%origin
   end if

end subroutine get_child_table


subroutine get_child_array(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (.not.initialized(table)) call new_table(table)

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      ptr => cast_to_array(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (is_requested) then
         call add_array(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
      if (present(origin)) origin = table%origin
   end if

end subroutine get_child_array


subroutine get_child_keyval(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (.not.initialized(table)) call new_table(table)

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      ptr => cast_to_keyval(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (is_requested) then
         call add_keyval(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
      if (present(origin)) origin = table%origin
   end if

end subroutine get_child_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_child_value_float_sp(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_child_value_float_dp(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_child_value_integer_i1(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_child_value_integer_i2(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_child_value_integer_i4(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_child_value_integer_i8(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_child_value_bool(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_bool


!> Retrieve TOML value as datetime
subroutine get_child_value_datetime(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Default datetime value
   type(toml_datetime), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_child_value_string(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   else if (.not.present(default)) then
      if (present(stat)) stat = merge(toml_stat%missing_key, stat, stat == toml_stat%success)
   end if

end subroutine get_child_value_string


!> Set TOML value to single precision float
subroutine set_child_value_float_sp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_sp


!> Set TOML value to double precision float
subroutine set_child_value_float_dp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_dp


!> Set TOML value to one byte integer
subroutine set_child_value_integer_i1(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_child_value_integer_i2(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_child_value_integer_i4(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_child_value_integer_i8(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i8


!> Set TOML value to logical
subroutine set_child_value_bool(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_bool


!> Set TOML value to datetime
subroutine set_child_value_datetime(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_datetime


!> Set TOML value to deferred-length character
subroutine set_child_value_string(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_string


end module tomlf_build_table


!>>>>> build/dependencies/toml-f/src/tomlf/build/path.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Support for retrieving and setting values using a key path.
module tomlf_build_path
   use tomlf_build_table, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_datetime, only : toml_datetime
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_key
   implicit none
   private

   public :: toml_path, get_value, set_value


   !> Setter functions to manipulate TOML tables
   interface set_value
      module procedure :: set_path_value_float_sp
      module procedure :: set_path_value_float_dp
      module procedure :: set_path_value_integer_i1
      module procedure :: set_path_value_integer_i2
      module procedure :: set_path_value_integer_i4
      module procedure :: set_path_value_integer_i8
      module procedure :: set_path_value_bool
      module procedure :: set_path_value_datetime
      module procedure :: set_path_value_string
   end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
      module procedure :: get_path_table
      module procedure :: get_path_array
      module procedure :: get_path_keyval
      module procedure :: get_path_value_float_sp
      module procedure :: get_path_value_float_dp
      module procedure :: get_path_value_integer_i1
      module procedure :: get_path_value_integer_i2
      module procedure :: get_path_value_integer_i4
      module procedure :: get_path_value_integer_i8
      module procedure :: get_path_value_bool
      module procedure :: get_path_value_datetime
      module procedure :: get_path_value_string
   end interface get_value


   !> Wrapper for storing key paths
   type :: toml_path
      !> Path components
      type(toml_key), allocatable :: path(:)
   end type toml_path


   !> Convenience constructors for building key paths from strings instead of keys
   interface toml_path
      module procedure :: new_path2
      module procedure :: new_path3
      module procedure :: new_path4
   end interface toml_path


contains


!> Create a new path with two components
pure function new_path2(key1, key2) result(path)

   !> First key to retrieve
   character(*), intent(in) :: key1

   !> Second key to retrieve
   character(*), intent(in) :: key2

   !> New path
   type(toml_path) :: path

   allocate(path%path(2))
   path%path(:) = [toml_key(key1), toml_key(key2)]
end function new_path2


!> Create a new path with three components
pure function new_path3(key1, key2, key3) result(path)

   !> First key to retrieve
   character(*, tfc), intent(in) :: key1

   !> Second key to retrieve
   character(*, tfc), intent(in) :: key2

   !> Third key to retrieve
   character(*, tfc), intent(in) :: key3

   !> New path
   type(toml_path) :: path

   allocate(path%path(3))
   path%path(:) = [toml_key(key1), toml_key(key2), toml_key(key3)]
end function new_path3


!> Create a new path with three components
pure function new_path4(key1, key2, key3, key4) result(path)

   !> First key to retrieve
   character(*, tfc), intent(in) :: key1

   !> Second key to retrieve
   character(*, tfc), intent(in) :: key2

   !> Third key to retrieve
   character(*, tfc), intent(in) :: key3

   !> Forth key to retrieve
   character(*, tfc), intent(in) :: key4

   !> New path
   type(toml_path) :: path

   allocate(path%path(4))
   path%path(:) = [toml_key(key1), toml_key(key2), toml_key(key3), toml_key(key4)]
end function new_path4


subroutine get_path_table(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout), target :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child
   logical :: is_requested

   is_requested = .true.
   if (present(requested)) is_requested = requested

   nullify(ptr)
   call walk_path(table, path, child, is_requested, stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), ptr, is_requested, stat, origin)
   else
      if (.not.is_requested .and. present(stat)) stat = toml_stat%success
   end if
end subroutine get_path_table


subroutine get_path_array(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child
   logical :: is_requested

   is_requested = .true.
   if (present(requested)) is_requested = requested

   nullify(ptr)
   call walk_path(table, path, child, is_requested, stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), ptr, is_requested, stat, origin)
   else
      if (.not.is_requested .and. present(stat)) stat = toml_stat%success
   end if
end subroutine get_path_array


subroutine get_path_keyval(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child
   logical :: is_requested

   is_requested = .true.
   if (present(requested)) is_requested = requested

   nullify(ptr)
   call walk_path(table, path, child, is_requested, stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), ptr, is_requested, stat, origin)
   else
      if (.not.is_requested .and. present(stat)) stat = toml_stat%success
   end if
end subroutine get_path_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_path_value_float_sp(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_path_value_float_dp(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_path_value_integer_i1(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_path_value_integer_i2(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_path_value_integer_i4(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_path_value_integer_i8(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_path_value_bool(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_bool


!> Retrieve TOML value as datetime
subroutine get_path_value_datetime(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Default datetime value
   type(toml_datetime), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_path_value_string(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_string


!> Set TOML value to single precision float
subroutine set_path_value_float_sp(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_float_sp


!> Set TOML value to double precision float
subroutine set_path_value_float_dp(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_float_dp


!> Set TOML value to one byte integer
subroutine set_path_value_integer_i1(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_path_value_integer_i2(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_path_value_integer_i4(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_path_value_integer_i8(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i8


!> Set TOML value to logical
subroutine set_path_value_bool(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_bool


!> Set TOML value to datetime
subroutine set_path_value_datetime(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_datetime


!> Set TOML value to deferred-length character
subroutine set_path_value_string(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_string


subroutine walk_path(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout), target :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   type(toml_table), pointer :: current, next

   nullify(ptr)
   if (.not.allocated(path%path)) then
      if (present(stat)) stat = toml_stat%fatal
      if (present(origin)) origin = table%origin
      return
   end if

   current => table
   do it = 1, size(path%path) - 1
      call get_value(current, path%path(it)%key, next, requested, stat, origin)
      if (.not.associated(next)) then
         if (present(stat)) stat = toml_stat%fatal
         if (present(origin)) origin = current%origin
         return
      end if
      current => next
   end do
   ptr => current
end subroutine walk_path


end module tomlf_build_path


!>>>>> build/dependencies/toml-f/src/tomlf/build.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build a TOML data structures
!>
!> The build module defines a high level interface to work with TOML data structures
!> and construct them in a convenient way.
module tomlf_build
   use tomlf_build_array, only : get_value, set_value
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_build_merge, only : merge_table, merge_array, merge_policy, toml_merge_config
   use tomlf_build_path, only : get_value, set_value, toml_path
   use tomlf_build_table, only : get_value, set_value
   implicit none
   private

   public :: get_value, set_value
   public :: merge_table, merge_array, merge_policy, toml_merge_config
   public :: toml_path

end module tomlf_build


!>>>>> build/dependencies/toml-f/src/tomlf.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Public API for TOML Fortran
!>
!> This module provides the main entry point to the TOML Fortran library.
!> It re-exports all public types and procedures needed for parsing, manipulating,
!> and serializing TOML documents.
!>
!> ## Parsing TOML
!>
!> Use [[toml_load]] to load a TOML document from a file or unit, or [[toml_loads]]
!> to parse a TOML string directly:
!>
!>```fortran
!> type(toml_table), allocatable :: table
!> call toml_load(table, "config.toml")
!>```
!>
!> ## Accessing Values
!>
!> Use [[get_value]] to retrieve values from tables and arrays, and [[set_value]]
!> to modify or create new values:
!>
!>```fortran
!> character(len=:), allocatable :: name
!> call get_value(table, "name", name)
!>```
!>
!> ## Serialization
!>
!> Use [[toml_dump]] to write a table to a file or [[toml_dumps]] to serialize
!> to a string:
!>
!>```fortran
!> character(len=:), allocatable :: output
!> call toml_dumps(table, output)
!>```
module tomlf
   use tomlf_build, only : get_value, set_value, toml_path
   use tomlf_datetime, only : toml_datetime, to_string
   use tomlf_de, only : toml_parse, toml_load, toml_loads, &
      & toml_context, toml_parser_config, toml_level
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_ser, only : toml_serializer, toml_serialize, toml_dump, toml_dumps
   use tomlf_terminal, only : toml_terminal
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_key, toml_value, &
      & is_array_of_tables, new_table, add_table, add_array, add_keyval, len
   use tomlf_utils_sort, only : sort
   use tomlf_version, only : tomlf_version_string, tomlf_version_compact, &
      & get_tomlf_version
   implicit none
   public

end module tomlf


!>>>>> ././src/ffmt_config.f90
module ffmt_config
!! Configuration files.
!!
!! Settings (see [[ffmt_settings]]) are read from the nearest of these, searching from a file's
!! directory upward:
!!
!! * `.fformat.toml`
!! * `fpm.toml`, in an `[extra.fformat]` table
!!
!! Settings are written as in the table of [[ffmt_settings]], with `spaces` and `case` as tables:
!!
!! ```toml
!! indent = 3
!! line-length = 100
!! [spaces]
!! comma = "add"
!! [case]
!! keywords = "lower"
!! [files]
!! extensions = ["f90", "F90"]
!! exclude = ["build", "legacy/*.f90"]
!! ```
!!
!! The `[files]` table controls which files are formatted when a directory is given.
use tomlf, only: toml_table, toml_array, toml_keyval, toml_key, toml_value, toml_error, toml_load, get_value, len
use tomlf_constants, only: toml_type
use ffmt_settings, only: setting_t
use ffmt_fs, only: join_path, parent_directory
use ffmt_strings, only: to_s, string_t
implicit none
private

public :: find_config, load_config

character(len=*), parameter :: CONFIG_NAME = '.fformat.toml'

type, public :: config_t
    !! The settings of one configuration file.
    character(len=:), allocatable :: path               !! the file, or `''` if there is none
    type(setting_t), allocatable :: settings(:)         !! the formatting settings, in the order they are applied
    type(string_t), allocatable :: extensions(:)      !! `files.extensions`; not allocated if the file does not set it
    type(string_t), allocatable :: exclude(:)           !! `files.exclude`, likewise
end type config_t

contains

subroutine find_config(dir, cfg, stat, errmsg)
    !! The configuration for files in directory `dir` (an absolute path): from the nearest config file
    !! in `dir` or its parents. `cfg%path` is `''` if there is none.
    character(len=*), intent(in) :: dir   !! absolute path of the directory
    type(config_t), intent(out) :: cfg   !! the configuration found
    integer, intent(out) :: stat                            !! nonzero if the config file found cannot be read
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, with the file name, if `stat` is nonzero
    character(len=:), allocatable :: d, candidate
    logical :: exists, found

    stat = 0
    d = dir
    do while (len(d) > 0)
        candidate = join_path(d, CONFIG_NAME)
        inquire (file=candidate, exist=exists)
        if (exists) then
            call load_config(candidate, .false., cfg, found, stat, errmsg)
            return
        end if
        candidate = join_path(d, 'fpm.toml')
        inquire (file=candidate, exist=exists)
        if (exists) then
            call load_config(candidate, .true., cfg, found, stat, errmsg)
            if (found .or. stat /= 0) return
        end if
        if (parent_directory(d) == d) exit
        d = parent_directory(d)
    end do
    call empty(cfg)
end subroutine find_config

subroutine load_config(path, fpm, cfg, found, stat, errmsg)
    !! Read the configuration in file `path`: the whole file, or with `fpm` its
    !! `[extra.fformat]` table. `found` is false if an `fpm.toml` has no such table.
    character(len=*), intent(in) :: path                    !! the TOML file
    logical, intent(in) :: fpm                              !! `path` is an `fpm.toml`
    type(config_t), intent(out) :: cfg                      !! the configuration; empty if not `found`
    logical, intent(out) :: found                           !! the file has a configuration
    integer, intent(out) :: stat                            !! nonzero for bad TOML, an unknown key or a bad value
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, with the file name, if `stat` is nonzero

    type(toml_table), allocatable, target :: top
    type(toml_table), pointer :: extra, table
    type(toml_error), allocatable :: error

    stat = 0
    found = .false.
    call empty(cfg)
    call toml_load(top, path, error=error)
    if (allocated(error)) then
        stat = 1
        errmsg = path//': '//error%message
        return
    end if
    if (fpm) then
        call get_value(top, 'extra', extra, requested=.false.)
        if (.not. associated(extra)) return
        call get_value(extra, 'fformat', table, requested=.false.)
        if (.not. associated(table)) return
    else
        table => top
    end if
    found = .true.
    cfg%path = path
    call read_table(table, '')

contains

    recursive subroutine read_table(t, prefix)
        !! Add the settings in table `t`, with keys prefixed by `prefix`.
        type(toml_table), intent(inout) :: t   !! the table to read
        character(len=*), intent(in) :: prefix  !! `''` for the top level, or `spaces.` / `case.`
        type(toml_key), allocatable :: keys(:)
        class(toml_value), pointer :: v
        type(toml_table), pointer :: sub
        character(len=:), allocatable :: key, text
        integer :: k

        call t%get_keys(keys)
        do k = 1, size(keys)
            key = keys(k)%key
            call t%get(key, v)
            select type (v)
            type is (toml_table)
                sub => v
                if (prefix == '' .and. key == 'files') then
                    call read_files(sub)
                else if (prefix == '' .and. (key == 'spaces' .or. key == 'case')) then
                    call read_table(sub, key//'.')
                else
                    call bad('unknown table "'//prefix//key//'"')
                end if
            type is (toml_keyval)
                text = value_text(t, key, v%get_type())
                if (stat /= 0) return
                ! `all` comes first, so that the other entries of the table override it
                if (key == 'all') then
                    cfg%settings = [setting_t(prefix//key, text), cfg%settings]
                else
                    cfg%settings = [cfg%settings, setting_t(prefix//key, text)]
                end if
            class default
                call bad('unexpected value for "'//prefix//key//'"')
            end select
            if (stat /= 0) return
        end do
    end subroutine read_table

    function value_text(t, key, kind) result(text)
        !! A string, integer or boolean value as text.
        type(toml_table), intent(inout) :: t    !! the table holding the value
        character(len=*), intent(in) :: key     !! the value's key in `t`
        integer, intent(in) :: kind             !! the value's type, a `toml_type` member
        character(len=:), allocatable :: text
        integer :: i
        logical :: flag
        text = ''
        if (kind == toml_type%string) then
            call get_value(t, key, text)
        else if (kind == toml_type%int) then
            call get_value(t, key, i)
            text = to_s(i)
        else if (kind == toml_type%boolean) then
            call get_value(t, key, flag)
            text = merge('true ', 'false', flag)
            text = trim(text)
        else
            call bad('expected a string, number or true/false for "'//key//'"')
        end if
    end function value_text

    subroutine read_files(t)
        !! Read the `[files]` table `t` into `cfg%extensions` and `cfg%exclude`.
        type(toml_table), intent(inout) :: t   !! the `[files]` table
        type(toml_key), allocatable :: keys(:)
        integer :: k
        call t%get_keys(keys)
        do k = 1, size(keys)
            select case (keys(k)%key)
            case ('extensions')
                call read_list(t, 'extensions', cfg%extensions)
            case ('exclude')
                call read_list(t, 'exclude', cfg%exclude)
            case default
                call bad('unknown setting "files.'//keys(k)%key//'"')
            end select
            if (stat /= 0) return
        end do
    end subroutine read_files

    subroutine read_list(t, key, list)
        !! An array of strings.
        type(toml_table), intent(inout) :: t                    !! the `[files]` table
        character(len=*), intent(in) :: key                     !! the array's key in `t`
        type(string_t), allocatable, intent(inout) :: list(:)   !! the strings, replacing any earlier list
        type(toml_array), pointer :: arr
        character(len=:), allocatable :: item
        integer :: i, ios
        call get_value(t, key, arr, requested=.false.)
        if (.not. associated(arr)) then
            call bad('expected a list of strings for "files.'//key//'"')
            return
        end if
        if (allocated(list)) deallocate (list)
        allocate (list(0))
        do i = 1, len(arr)
            call get_value(arr, i, item, stat=ios)
            if (ios /= 0 .or. .not. allocated(item)) then
                call bad('expected a list of strings for "files.'//key//'"')
                return
            end if
            list = [list, string_t(item)]
        end do
    end subroutine read_list

    subroutine bad(msg)
        !! Make loading fail with message `msg`, prefixed with the file name.
        character(len=*), intent(in) :: msg   !! what is wrong
        stat = 1
        errmsg = path//': '//msg
    end subroutine bad

end subroutine load_config

subroutine empty(cfg)
    !! A configuration with nothing set.
    type(config_t), intent(out) :: cfg      !! set to have no file, no settings and no `[files]` lists
    cfg%path = ''
    allocate (cfg%settings(0))
end subroutine empty

end module ffmt_config


!>>>>> ././src/ffmt_cli.f90
module ffmt_cli
!! Command-line interface: argument parsing and processing of files and directories.
use iso_fortran_env, only: output_unit, error_unit
use ffmt_options
use ffmt_settings, only: setting_t, apply_setting
use ffmt_config, only: config_t, find_config, load_config
use ffmt_format, only: format_text, diagnostic_t, FMT_OK
use ffmt_io, only: read_file, write_file, read_stdin
use ffmt_fs, only: is_directory, absolute_path, parent_directory
use ffmt_walk, only: collect_files, default_extensions, default_exclude
use ffmt_diff, only: unified_diff
use ffmt_strings, only: string_t, to_s
implicit none
private

public :: parse_args, run, usage, string_t

! exit codes
integer, parameter, public :: EXIT_OK = 0         !! success
integer, parameter, public :: EXIT_CHANGED = 1    !! with `--check`: some files are not formatted
integer, parameter, public :: EXIT_ERROR = 2      !! bad arguments, unreadable files or internal errors

character(len=*), parameter, public :: VERSION = '1.0.1'

type, public :: cli_t
    !! Parsed command line.
    type(setting_t), allocatable :: settings(:)     !! formatting settings given on the command line, in order
    type(string_t), allocatable :: paths(:)         !! files, directories, or `-` for standard input
    type(string_t), allocatable :: exclude(:)       !! `--exclude` patterns, added to the configured ones
    type(string_t), allocatable :: extensions(:)    !! `--extensions`; not allocated if not given
    character(len=:), allocatable :: config         !! `--config` file, or `''`
    logical :: no_config = .false.                  !! ignore configuration files
    logical :: stdout = .false.                     !! write results to standard output
    logical :: check = .false.                      !! report files that would change; write nothing
    logical :: diff = .false.                       !! write a diff of the changes; write nothing else
    logical :: quiet = .false.                      !! do not report warnings
    logical :: help = .false.                       !! show the `--help` text and do nothing else
    logical :: version = .false.                    !! show the version and do nothing else
end type cli_t

type :: config_cache_t
    !! Configurations already found, by directory.
    type(string_t), allocatable :: dirs(:)          !! absolute paths of the directories looked up
    type(config_t), allocatable :: configs(:)       !! the configuration that applies in each of `dirs`
end type config_cache_t

contains

function usage() result(text)
    !! The `--help` text.
    character(len=:), allocatable :: text
    character, parameter :: nl = new_line('a')
    text = 'Usage: fformat [OPTIONS] PATH...'//nl// &
           nl// &
           'Format free-form Fortran source files in place. A directory is searched recursively for'//nl// &
           'Fortran files; "-" reads standard input and writes standard output.'//nl// &
           nl// &
           'Formatting:'//nl// &
           '  -i, --indent N              spaces per indentation level (default 4)'//nl// &
           '      --base-indent N         columns added to every line, program units included (default 0)'//nl// &
           '      --indent-units          indent the bodies of modules, submodules and programs'//nl// &
           '      --no-indent-procedures  do not indent the bodies of subroutines and functions'//nl// &
           '      --indent-fypp           indent the code inside fypp blocks (#:if, #:for, #:def, ...)'//nl// &
           '                              and the fypp directive lines with it'//nl// &
           '      --continuation STYLE    continuation lines: "align" with brackets and "=" (default),'//nl// &
           '                              or "indent" by a fixed amount'//nl// &
           '      --continuation-indent N extra indentation of continuation lines (default 4)'//nl// &
           '      --no-indent-comments    leave the indentation of comment lines unchanged'//nl// &
           '      --keep-column1-comments leave comment lines that start in column 1 there'//nl// &
           '      --comment-spacing N     blanks before a comment at the end of a line, unless it is'//nl// &
           '                              aligned with the comment on the line before or after'//nl// &
           '  -l, --line-length N         maximum line length (default 132, 0 for no limit): longer code'//nl// &
           '                              lines are split, and alignment uses less indentation'//nl// &
           '                              rather than exceed it'//nl// &
           '      --no-split              do not split long lines'//nl// &
           '      --sort-use              sort each group of use statements by module name'//nl// &
           '      --sort-use-only         sort the names in only: lists'//nl// &
           '      --relational STYLE      spelling of relational operators: "symbols" (.lt. becomes <),'//nl// &
           '                              "letters" (< becomes .lt.) or "keep" (default)'//nl// &
           '      --spaces RULE=MODE,...  blanks around operators; MODE is add (at least one blank),'//nl// &
           '                              remove or keep. RULE is one of assignment, keyword-args,'//nl// &
           '                              pointer, declarations, relational, logical, plus-minus,'//nl// &
           '                              mult-div, power, concat, comma, slice-colon, percent,'//nl// &
           '                              control-paren, conditional, defined-operators, or all'//nl// &
           '      --case WHAT=MODE,...    letter case; WHAT is keywords, operators, procedures'//nl// &
           '                              (intrinsic), constants (of iso_fortran_env and iso_c_binding),'//nl// &
           '                              types (kinds and exponents) or all, MODE is lower, upper or'//nl// &
           '                              keep (default keep)'//nl// &
           '      --set KEY=VALUE         any setting, as in a configuration file (e.g. spaces.comma=add)'//nl// &
           nl// &
           'Files:'//nl// &
           '      --config FILE           read settings from FILE instead of searching for one'//nl// &
           '      --no-config             do not read configuration files'//nl// &
           '      --exclude PATTERN       skip matching files and directories in searched directories'//nl// &
           '                              (repeatable; hidden entries and "build" are skipped by default)'//nl// &
           '      --extensions LIST       extensions of the files to format in searched directories'//nl// &
           '                              (default f90,F90,f95,F95,f03,F03,f08,F08,f18,F18)'//nl// &
           nl// &
           'Output:'//nl// &
           '  -s, --stdout                write the results to standard output'//nl// &
           '  -d, --diff                  write the changes as a unified diff; change no files'//nl// &
           '  -c, --check                 change no files; exit with status 1 if a file would change'//nl// &
           '  -q, --quiet                 do not report warnings'//nl// &
           '  -h, --help                  show this help'//nl// &
           '      --version               show the version'//nl// &
           nl// &
           'Settings come from the defaults, then the nearest .fformat.toml (or an [extra.fformat]'//nl// &
           'table in fpm.toml) in or above the file''s directory, then the command line.'//nl// &
           nl// &
           'Formatter directives: a statement with a "!&" comment is left unchanged, and lines'//nl// &
           'between "!&<" and "!&>" are left unchanged.'//nl// &
           nl// &
           'Exit status: 0 on success, 1 if --check found files to format, 2 on errors.'
end function usage

subroutine parse_args(args, cli, stat, errmsg)
    !! Parse command-line arguments (without the program name).
    type(string_t), intent(in) :: args(:)                   !! the arguments, in order
    type(cli_t), intent(out) :: cli                         !! the parsed command line
    integer, intent(out) :: stat                            !! 0, or `EXIT_ERROR` for a bad command line
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, if `stat` is nonzero
    type(format_options_t) :: scratch
    integer :: k, j
    character(len=:), allocatable :: arg
    logical :: only_paths

    stat = 0
    allocate (cli%settings(0), cli%paths(0), cli%exclude(0))
    cli%config = ''
    only_paths = .false.
    k = 0
    do while (k < size(args))
        k = k + 1
        arg = args(k)%s
        if (only_paths .or. len(arg) == 0 .or. arg == '-') then
            cli%paths = [cli%paths, string_t(arg)]
            cycle
        end if
        if (arg(1:1) /= '-') then
            cli%paths = [cli%paths, string_t(arg)]
            cycle
        end if
        select case (arg)
        case ('--')
            only_paths = .true.
        case ('-h', '--help')
            cli%help = .true.
        case ('--version')
            cli%version = .true.
        case ('-s', '--stdout')
            cli%stdout = .true.
        case ('-c', '--check')
            cli%check = .true.
        case ('-d', '--diff')
            cli%diff = .true.
        case ('-q', '--quiet')
            cli%quiet = .true.
        case ('--no-config')
            cli%no_config = .true.
        case ('--indent-units')
            call add('indent-units', 'true')
        case ('--indent-fypp')
            call add('indent-fypp', 'true')
        case ('--no-indent-procedures')
            call add('indent-procedures', 'false')
        case ('--no-indent-comments')
            call add('indent-comments', 'false')
        case ('--keep-column1-comments')
            call add('column1-comments', 'keep')
        case ('--no-split')
            call add('split', 'false')
        case ('--sort-use')
            call add('sort-use', 'true')
        case ('--sort-use-only')
            call add('sort-use-only', 'true')
        case default
            if (.not. takes_value(arg)) then
                call fail('unknown option '//arg)
                return
            end if
            if (k == size(args)) then
                call fail('missing value for '//arg)
                return
            end if
            k = k + 1
            call option_value(arg, args(k)%s)
            if (stat /= 0) return
        end select
    end do

    ! check the settings now, so that a mistake is reported before any file is changed
    do j = 1, size(cli%settings)
        call apply_setting(scratch, cli%settings(j)%key, cli%settings(j)%value, stat, errmsg)
        if (stat /= 0) then
            stat = EXIT_ERROR
            return
        end if
    end do
    if (.not. (cli%help .or. cli%version) .and. size(cli%paths) == 0) call fail('no files given')

contains

    logical function takes_value(opt)
        !! True if option `opt` (as written, e.g. `-i` or `--indent`) is followed by a value.
        character(len=*), intent(in) :: opt   !! the argument to test
        select case (opt)
        case ('-i', '--indent', '--base-indent', '--continuation', '--continuation-indent', '-l', '--line-length', '--relational', &
              '--spaces', '--case', '--set', '--config', '--exclude', '--extensions', '--comment-spacing')
            takes_value = .true.
        case default
            takes_value = .false.
        end select
    end function takes_value

    subroutine option_value(opt, value)
        !! Record option `opt`, one for which `takes_value` is true, with `value`, the argument after it.
        character(len=*), intent(in) :: opt     !! the option, as written
        character(len=*), intent(in) :: value   !! its value
        select case (opt)
        case ('-i', '--indent')
            call add('indent', value)
        case ('--base-indent')
            call add('base-indent', value)
        case ('--continuation')
            call add('continuation', value)
        case ('--continuation-indent')
            call add('continuation-indent', value)
        case ('--comment-spacing')
            call add('comment-spacing', value)
        case ('-l', '--line-length')
            call add('line-length', value)
        case ('--relational')
            call add('relational', value)
        case ('--spaces')
            call add_list('spaces.', value)
        case ('--case')
            call add_list('case.', value)
        case ('--set')
            call add_list('', value)
        case ('--config')
            cli%config = value
        case ('--exclude')
            cli%exclude = [cli%exclude, string_t(value)]
        case ('--extensions')
            call extension_list(value)
        end select
    end subroutine option_value

    subroutine add(key, value)
        !! Append the setting `key` with `value` to `cli%settings`.
        character(len=*), intent(in) :: key     !! setting name, as in a configuration file
        character(len=*), intent(in) :: value   !! its value as text
        cli%settings = [cli%settings, setting_t(key, value)]
    end subroutine add

    subroutine add_list(prefix, text)
        !! Comma-separated `NAME=VALUE` items.
        character(len=*), intent(in) :: prefix  !! prepended to each `NAME` to make the setting name, e.g. `spaces.`
        character(len=*), intent(in) :: text    !! the items
        integer :: i, j, eq
        character(len=:), allocatable :: item
        i = 1
        do while (i <= len(text))
            j = index(text(i:), ',')
            if (j == 0) then
                j = len(text) + 1
            else
                j = i + j - 1
            end if
            item = text(i:j - 1)
            eq = index(item, '=')
            if (eq == 0) then
                call fail('expected NAME=VALUE for '//arg//', not "'//item//'"')
                return
            end if
            call add(prefix//item(1:eq - 1), item(eq + 1:))
            i = j + 1
        end do
    end subroutine add_list

    subroutine extension_list(text)
        !! Set `cli%extensions` from the comma-separated list `text`, dropping a leading `.` of each
        !! extension and skipping empty items.
        character(len=*), intent(in) :: text   !! the value of `--extensions`
        integer :: i, j
        allocate (cli%extensions(0))
        i = 1
        do while (i <= len(text))
            j = index(text(i:), ',')
            if (j == 0) then
                j = len(text) + 1
            else
                j = i + j - 1
            end if
            if (j > i) then
                if (text(i:i) == '.') then
                    cli%extensions = [cli%extensions, string_t(text(i + 1:j - 1))]
                else
                    cli%extensions = [cli%extensions, string_t(text(i:j - 1))]
                end if
            end if
            i = j + 1
        end do
    end subroutine extension_list

    subroutine fail(msg)
        !! Make parsing fail with `EXIT_ERROR` and message `msg`.
        character(len=*), intent(in) :: msg   !! what is wrong
        stat = EXIT_ERROR
        errmsg = msg
    end subroutine fail

end subroutine parse_args

integer function run(cli) result(status)
    !! Format the paths of `cli` and return the exit status.
    type(cli_t), intent(in) :: cli      !! the parsed command line
    type(config_cache_t) :: cache
    type(string_t), allocatable :: files(:)
    integer :: k, j

    if (cli%help) then
        write (output_unit, '(a)') usage()
        status = EXIT_OK
        return
    end if
    if (cli%version) then
        write (output_unit, '(a)') 'fformat '//VERSION
        status = EXIT_OK
        return
    end if
    allocate (cache%dirs(0), cache%configs(0))
    status = EXIT_OK
    do k = 1, size(cli%paths)
        associate (path => cli%paths(k)%s)
            if (path == '-') then
                status = max(status, process_stdin(cli, cache))
            else if (is_directory(path)) then
                call directory_files(cli, cache, path, files, status)
                do j = 1, size(files)
                    status = max(status, process_file(cli, cache, files(j)%s))
                end do
            else
                status = max(status, process_file(cli, cache, path))
            end if
        end associate
    end do
end function run

subroutine directory_files(cli, cache, dir, files, status)
    !! The Fortran files under `dir`, selected with the `[files]` settings of its configuration and
    !! the command line.
    type(cli_t), intent(in) :: cli                          !! the parsed command line
    type(config_cache_t), intent(inout) :: cache            !! configurations found so far
    character(len=*), intent(in) :: dir                     !! the directory, as given on the command line
    type(string_t), allocatable, intent(out) :: files(:)    !! paths of the files found
    integer, intent(inout) :: status                        !! exit status so far; set to `EXIT_ERROR` if the
                                                            !! configuration cannot be read
    type(config_t) :: cfg
    type(string_t), allocatable :: extensions(:), exclude(:)
    integer :: stat
    character(len=:), allocatable :: errmsg

    extensions = default_extensions()
    exclude = default_exclude()
    call config_for(cli, cache, absolute_path(dir), cfg, stat, errmsg)
    if (stat /= 0) then
        write (error_unit, '(a)') 'error: '//errmsg
        status = EXIT_ERROR
        allocate (files(0))
        return
    end if
    if (allocated(cfg%extensions)) extensions = cfg%extensions
    if (allocated(cfg%exclude)) exclude = cfg%exclude
    if (allocated(cli%extensions)) extensions = cli%extensions
    exclude = [exclude, cli%exclude]
    call collect_files(dir, extensions, exclude, files)
end subroutine directory_files

subroutine config_for(cli, cache, dir, cfg, stat, errmsg)
    !! The configuration that applies in directory `dir` (absolute).
    type(cli_t), intent(in) :: cli                          !! the parsed command line (`--config`, `--no-config`)
    type(config_cache_t), intent(inout) :: cache            !! configurations found so far; `cfg` is added
    character(len=*), intent(in) :: dir   !! absolute path of the directory
    type(config_t), intent(out) :: cfg                      !! the configuration; `cfg%path` is `''` if there is none
    integer, intent(out) :: stat                            !! nonzero if a configuration file cannot be read
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, if `stat` is nonzero
    logical :: found
    integer :: k

    stat = 0
    if (cli%no_config) then
        cfg%path = ''
        allocate (cfg%settings(0))
        return
    end if
    if (len(cli%config) > 0) then
        call load_config(cli%config, ends_with(cli%config, 'fpm.toml'), cfg, found, stat, errmsg)
        if (stat == 0 .and. .not. found) then
            stat = 1
            errmsg = cli%config//': no [extra.fformat] table'
        end if
        return
    end if
    do k = 1, size(cache%dirs)
        if (cache%dirs(k)%s == dir .and. len(cache%dirs(k)%s) == len(dir)) then
            cfg = cache%configs(k)
            return
        end if
    end do
    call find_config(dir, cfg, stat, errmsg)
    if (stat /= 0) return
    cache%dirs = [cache%dirs, string_t(dir)]
    cache%configs = [cache%configs, cfg]
end subroutine config_for

subroutine options_for(cli, cache, dir, opts, stat, errmsg)
    !! The options for a file in directory `dir`: defaults, then configuration, then command line.
    type(cli_t), intent(in) :: cli                          !! the parsed command line
    type(config_cache_t), intent(inout) :: cache            !! configurations found so far
    character(len=*), intent(in) :: dir                     !! the file's directory, relative or absolute
    type(format_options_t), intent(out) :: opts             !! the options
    integer, intent(out) :: stat                            !! nonzero for an unreadable configuration or a bad setting
    character(len=:), allocatable, intent(out) :: errmsg    !! what is wrong, if `stat` is nonzero
    type(config_t) :: cfg
    integer :: k

    call config_for(cli, cache, absolute_path(dir), cfg, stat, errmsg)
    if (stat /= 0) return
    do k = 1, size(cfg%settings)
        call apply_setting(opts, cfg%settings(k)%key, cfg%settings(k)%value, stat, errmsg)
        if (stat /= 0) then
            errmsg = cfg%path//': '//errmsg
            return
        end if
    end do
    do k = 1, size(cli%settings)
        call apply_setting(opts, cli%settings(k)%key, cli%settings(k)%value, stat, errmsg)
        if (stat /= 0) return
    end do
end subroutine options_for

integer function process_stdin(cli, cache) result(status)
    !! Format standard input to standard output (or report it with `--check` / `--diff`), with
    !! the configuration of the current directory, and return the exit status.
    type(cli_t), intent(in) :: cli                  !! the parsed command line
    type(config_cache_t), intent(inout) :: cache    !! configurations found so far
    character(len=:), allocatable :: text
    call read_stdin(text)
    status = process_text(cli, cache, '<stdin>', '.', text, .true.)
end function process_stdin

integer function process_file(cli, cache, path) result(status)
    !! Format file `path` in place (or write it to standard output, or report it with `--check` /
    !! `--diff`), with the configuration of its directory, and return the exit status.
    type(cli_t), intent(in) :: cli                  !! the parsed command line
    type(config_cache_t), intent(inout) :: cache    !! configurations found so far
    character(len=*), intent(in) :: path   !! the file, as given on the command line or found in a directory
    character(len=:), allocatable :: text, errmsg, dir
    integer :: stat

    call read_file(path, text, stat, errmsg)
    if (stat /= 0) then
        write (error_unit, '(a)') path//': error: '//errmsg
        status = EXIT_ERROR
        return
    end if
    dir = parent_directory(absolute_path(path))
    status = process_text(cli, cache, path, dir, text, cli%stdout)
end function process_file

integer function process_text(cli, cache, name, dir, text, to_stdout) result(status)
    !! Format `text` from file `name` in directory `dir`, and write or report the result.
    type(cli_t), intent(in) :: cli                  !! the parsed command line
    type(config_cache_t), intent(inout) :: cache    !! configurations found so far
    character(len=*), intent(in) :: name            !! the file, for messages and diff headers and to write
                                                    !! the result to; `<stdin>` for standard input
    character(len=*), intent(in) :: dir             !! the directory whose configuration applies
    character(len=*), intent(in) :: text            !! the source as read
    logical, intent(in) :: to_stdout                !! write the result to standard output instead of to `name`
    type(format_options_t) :: opts
    character(len=:), allocatable :: out, errmsg
    type(diagnostic_t), allocatable :: diags(:)
    integer :: stat, d
    logical :: changed

    status = EXIT_OK
    call options_for(cli, cache, dir, opts, stat, errmsg)
    if (stat /= 0) then
        write (error_unit, '(a)') name//': error: '//errmsg
        status = EXIT_ERROR
        return
    end if
    call format_text(text, opts, out, stat, errmsg, diags)
    if (.not. cli%quiet) then
        do d = 1, size(diags)
            write (error_unit, '(a)') name//':'//to_s(diags(d)%line)//': warning: '//diags(d)%message
        end do
    end if
    if (stat /= FMT_OK) then
        write (error_unit, '(a)') name//': error: '//errmsg
        status = EXIT_ERROR
        return
    end if

    changed = out /= text .or. len(out) /= len(text)
    if (cli%diff .and. changed) write (output_unit, '(a)', advance='no') unified_diff(text, out, name)
    if (cli%check) then
        if (changed) then
            if (.not. cli%diff) write (output_unit, '(a)') name//': would be reformatted'
            status = EXIT_CHANGED
        end if
    else if (cli%diff) then
        continue
    else if (to_stdout) then
        write (output_unit, '(a)', advance='no') out
    else if (changed) then
        call write_file(name, out, stat, errmsg)
        if (stat /= 0) then
            write (error_unit, '(a)') name//': error: '//errmsg
            status = EXIT_ERROR
        end if
    end if
end function process_text

pure logical function ends_with(s, suffix)
    !! True if string `s` ends with `suffix`.
    character(len=*), intent(in) :: s, suffix   !! the string, and the ending to look for
    ends_with = .false.
    if (len(s) >= len(suffix)) ends_with = s(len(s) - len(suffix) + 1:) == suffix
end function ends_with

end module ffmt_cli


!>>>>> app/main.f90
program fformat
!! The `fformat` command. See [[ffmt_cli]].
use iso_fortran_env, only: error_unit
use ffmt_cli, only: cli_t, string_t, parse_args, run, EXIT_OK
implicit none

type(cli_t) :: cli
type(string_t), allocatable :: args(:)
character(len=:), allocatable :: errmsg
integer :: k, n, stat

allocate (args(command_argument_count()))
do k = 1, size(args)
    call get_command_argument(k, length=n)
    allocate (character(len=n) :: args(k)%s)
    call get_command_argument(k, args(k)%s)
end do

call parse_args(args, cli, stat, errmsg)
if (stat /= 0) then
    write (error_unit, '(a)') 'fformat: '//errmsg
    write (error_unit, '(a)') 'Try "fformat --help".'
    stop stat, quiet = .true.
end if
stat = run(cli)
if (stat /= EXIT_OK) stop stat, quiet = .true.
end program fformat

