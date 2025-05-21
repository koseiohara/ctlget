module ctlinfo

    use, intrinsic :: iso_fortran_env, only : real32
    use caseconverter, only : to_lower

    implicit none

    private
    public :: ctl


    integer, parameter :: string_max = 512


    type ctl
        private
        character(256) :: ctlname
        character(string_max), allocatable :: ctl_all(:)
        integer :: dset
        integer :: title
        integer :: undef
        integer :: options
        integer :: xdef
        integer :: ydef
        integer :: zdef
        integer :: tdef
        integer :: vars

        contains

        procedure, pass, public  :: get_dset
        procedure, pass, public  :: get_title
        procedure, pass, public  :: get_undef
        procedure, pass, public  :: get_options
        procedure, pass, public  :: get_gridnum
        procedure, pass, private :: get_n
        !procedure, pass, public  :: get_lats
        !procedure, pass, public  :: get_levs
        !procedure, pass, public  :: get_nt
        !procedure, pass, public  :: get_tini
        !procedure, pass, public  :: get_dt
        !procedure, pass, public  :: get_nvars
        !procedure, pass, public  :: get_var_idx
        !procedure, pass, public  :: get_var_description
        !procedure, pass, public  :: get_var_name

        final :: del

    end type ctl


    interface ctl
        module procedure init
    end interface ctl


    contains


    ! Constructor
    function init(ctlname, linemax) result(output)
        character(*), intent(in) :: ctlname
        integer     , intent(in), optional :: linemax     ! DEFAULT : 100

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
            ! end the loop when EOF was found
            if (EOF /= 0) then
                lines = i - 1
                exit
            endif
        enddo

        ! delete spaces from left of each line
        output%ctl_all(1:lines) = del_left_space(output%ctl_all(1:lines))

        call get_line_number(FLAG   ='dset'                 , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%dset              )  !! OUT

        call get_line_number(FLAG   ='title'                , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%title             )  !! OUT

        call get_line_number(FLAG   ='undef'                , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%undef             )  !! OUT

        call get_line_number(FLAG   ='options'              , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%options           )  !! OUT

        call get_line_number(FLAG   ='xdef'                 , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%xdef              )  !! OUT

        call get_line_number(FLAG   ='ydef'                 , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%ydef              )  !! OUT

        call get_line_number(FLAG   ='zdef'                 , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%zdef              )  !! OUT

        call get_line_number(FLAG   ='tdef'                 , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%tdef              )  !! OUT

        call get_line_number(FLAG   ='vars'                 , &  !! IN
                           & LINES  =lines                  , &  !! IN
                           & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                           & LINE   =output%vars              )  !! OUT

    end function init


    ! Destructor : deallocate ctl_all
    subroutine del(self)
        type(ctl), intent(inout) :: self

        if (allocated(self%ctl_all)) then
            deallocate(self%ctl_all)
            write(*,*) 'Destructed'
        endif

    end subroutine del


    subroutine get_line_number(flag, lines, ctl_all, line)
        character(*), intent(in)  :: flag
        integer     , intent(in)  :: lines
        character(*), intent(in)  :: ctl_all(lines)
        integer     , intent(out) :: line

        character(string_max) :: string
        character(64)         :: line_flag
        character(64)         :: flag_lower
        integer :: where_space
        integer :: i

        ! flag to lower case
        flag_lower = to_lower(flag)

        do i = 1, lines
            ! get a line
            string = ctl_all(i)

            ! find the first space
            where_space = index(string, ' ')
            ! if space was found
            if (where_space /= 0) then

                ! get the first column
                line_flag = string(1:where_space)
                ! the first column to lower case
                line_flag = to_lower(line_flag)

                if (trim(flag_lower) == trim(line_flag)) then
                    ! if the flag was found from the first column, get the line number and end the routine
                    line = i
                    write(*,*) trim(flag) // ' is at line ', line
                    return
                endif
            endif
        enddo

        ! if the flag was not found, return 0
        line = 0

        write(*,*) trim(flag) // ' is at line ', line

    end subroutine get_line_number


    subroutine get_dset(self, output)
        class(ctl)  , intent(in)  :: self
        character(*), intent(out) :: output

        character(string_max) :: line
        character(256)        :: work_filename
        character(256)        :: ctl_dir
        integer :: filename_end

        ! get the dset line
        line = trim(self%ctl_all(self%dset))

        ! trimming
        line          = del_left_space(line(5:string_max))
        filename_end  = index(line(1:string_max), ' ') - 1
        ! if space is not found, the last character is selected
        if (filename_end == -1) then
            filename_end = string_max
        endif
        work_filename = line(1:filename_end)

        if (work_filename(1:1) == '^') then
            ! if binary name is written by relative path, delete '^' and add the path of control file
            work_filename = trim(work_filename(2:256))

            call get_ctl_dir(self%ctlname, &  !! IN
                           & ctl_dir       )  !! OUT

            output = trim(ctl_dir) // trim(work_filename)
        else
            ! if binary file is written by absolute path
            output = trim(work_filename)
        endif

    end subroutine get_dset


    subroutine get_title(self, output)
        class(ctl), intent(in)    :: self
        character(*), intent(out) :: output

        character(string_max) :: line
        integer :: title_end

        ! get the title line
        line = trim(self%ctl_all(self%title))

        ! trimming
        line      = del_left_space(line(6:string_max))
        title_end = index(line(1:string_max), '*') - 1
        ! if space is not found, the last character is selected
        if (title_end == -1) then
            title_end = string_max
        endif
        output = trim(line(1:title_end))

    end subroutine get_title


    subroutine get_undef(self, output, output_char)
        class(ctl), intent(in) :: self
        real(real32), intent(out), optional :: output
        character(*), intent(out), optional :: output_char

        character(string_max) :: line
        character(64) :: work_undef
        integer :: undef_end

        ! get the undef line
        line = trim(self%ctl_all(self%undef))

        ! trimming
        line = del_left_space(line(6:string_max))
        undef_end = index(line(1:string_max), ' ') - 1
        ! if space is not found, the last character is selected
        if (undef_end == -1) then
            undef_end = string_max
        endif
        work_undef = trim(line(1:undef_end))
        
        ! get undef in real32
        if (present(output)) then
            read(work_undef,*) output
        endif

        ! get undef in char
        if (present(output_char)) then
            output_char = trim(work_undef)
        endif

    end subroutine get_undef


    subroutine get_options(self, output)
        class(ctl)  , intent(in)  :: self
        character(*), intent(out) :: output

        character(string_max) :: line
        integer :: option_end

        line = trim(self%ctl_all(self%options))

        ! trimming
        line = del_left_space(line(8:string_max))
        option_end = index(line(1:string_max), '*') - 1
        ! if space is not found, the last character is selected
        if (option_end == -1) then
            option_end = string_max
        endif

        output = trim(line(1:option_end))

    end subroutine get_options


    subroutine get_gridnum(self, nx, ny, nz)
        class(ctl), intent(in) :: self
        integer   , intent(out), optional :: nx
        integer   , intent(out), optional :: ny
        integer   , intent(out), optional :: nz

        if (present(nx)) then
            call self%get_n(self%xdef, &  !! OUT
                          & nx         )  !! IN
        endif

        if (present(ny)) then
            call self%get_n(self%ydef, &  !! OUT
                          & ny         )  !! IN
        endif

        if (present(nz)) then
            call self%get_n(self%zdef, &  !! OUT
                          & nz         )  !! IN
        endif

    end subroutine get_gridnum


    subroutine get_n(self, line_number, output)
        class(ctl), intent(in)  :: self
        integer   , intent(in)  :: line_number
        integer   , intent(out) :: output

        character(string_max) :: line
        character(16)         :: work_n
        integer :: n_end

        line = trim(self%ctl_all(line_number))

        ! trimming
        line = del_left_space(line(5:string_max))
        n_end = index(line(1:string_max), ' ') - 1
        ! if space is not found, the last character is selected
        if (n_end == -1) then
            n_end = string_max
        endif
        work_n = trim(line(1:n_end))

        read(work_n,*) output

    end subroutine get_n


    pure elemental function del_left_space(string) result(output)
        character(*), intent(in) :: string
        character(len(string))   :: output
         
        integer :: i

        output = ''
        do i = 1, len_trim(string)
            ! get the first non-space character
            if (string(i:i) == ' ') then
                cycle
            endif
            output = trim(string(i:))
            return
        enddo

    end function del_left_space


    subroutine get_ctl_dir(ctlname, ctldir)
        character(*), intent(in)  :: ctlname
        character(*), intent(out) :: ctldir
        integer :: where_slash

        where_slash = index(ctlname, '/', back=.TRUE.)

        ctldir = ctlname(1:where_slash)

    end subroutine get_ctl_dir


end module ctlinfo

