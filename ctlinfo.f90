module ctlinfo

    use caseconverter, only : to_lower

    implicit none

    private
    public :: 


    integer, parameter :: string_max = 512


    type ctl
        private
        character(256) :: ctlname
        character(string_max), allocatable :: ctl_all(:)
        integer :: dset
        integer :: title
        integer :: undef
        integer :: options
        integer :: nx
        integer :: ny
        integer :: nz
        integer :: nt
        integer :: vars
        integer :: endvars

        contains

        procedure, public :: get_dset
        procedure, public :: get_title
        procedure, public :: get_undef
        procedure, public :: get_options
        procedure, public :: get_nx
        procedure, public :: get_ny
        procedure, public :: get_lats
        procedure, public :: get_nz
        procedure, public :: get_levs
        procedure, public :: get_nt
        procedure, public :: get_tini
        procedure, public :: get_dt
        procedure, public :: get_nvars
        procedure, public :: get_var_idx
        procedure, public :: get_var_description
        procedure, public :: get_var_name


    end type ctl


    contains


    function init(ctlname, linemax) result(output)
        character(256), intent(in) :: ctlname
        integer       , intent(in), optional :: linemax

        type(ctl) :: output

        integer :: iostat
        integer :: unit
        character(64) :: iomsg

        integer :: i
        integer :: EOF
        integer :: lmax
        integer :: lines

        if (present(linemax)) then
            if (linemax <= 0) then
                write(0,'(A)') '<ERROR STOP>'
                write(0,'(A)') 'Invalid linemax to read ' // trim(ctlname)
                write(0,'(A,I0)') 'Specified : ', linemax
                ERROR STOP
            endif
            lmax = linemax
        else
            lmax = 100
        endif

        allocate(output%ctl_all(lmax))

        open(NEWUNIT=unit   , &
           & FILE   =ctlname, &
           & ACTION ='READ' , &
           & IOSTAT =iostat , &
           & IOMSG  =iomsg    )

        if (iostat /= 0) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') trim(ctlname)
            write(0,'(A)') trim(iomsg)
            ERROR STOP
        endif

        do i = 1, lmax
            read(unit,'(A)',iostat=EOF) output%ctl_all(i)
            if (EOF /= 0) then
                lines = i - 1
                exit
            endif
        enddo

        output%ctl_all(1:lines) = del_left_space(output%ctl_all(1:lines))

        call get_line(FLAG='dset'                    , &  !! IN
                    & LINES=lines                    , &  !! IN
                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                    & LINE   =output%dset              )  !! OUT

    end function init


    subroutine get_line(flag, lines, ctl_all, line)
        character(*)         , intent(in)  :: flag
        integer              , intent(in)  :: lines
        character(string_max), intent(in)  :: ctl_all
        integer              , intent(out) :: line

        character(string_max) :: string
        character(64)         :: line_flag
        character(64)         :: flag_lower
        integer :: where_space
        integer :: i

        flag_lower = to_lower(flag)

        do i = 1, lines
            string = ctl_all(i)
            where_space = index(string, ' ')
            if (where_space == 0) then
                line_flag = string(1:where_space)
                line_flag = to_lower(line_flag)
                if (trim(flag_lower) == trim(line_flag)) then
                    line = i
                    exit
                endif
            endif
        enddo

        line = 0

    end subroutine get_line


    pure elemental function del_left_space(string) result(output)
        character(*), intent(in) :: string
        character(*) :: output
         
        integer :: i

        output = ''
        do i = 1, len_trim(string)
            if (string(i) == ' ') then
                cycle
            endif
            output = trim(string(i:))
            return
        enddo

    end function del_lef_space


