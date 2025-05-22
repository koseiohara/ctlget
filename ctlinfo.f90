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
        integer :: lines
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

        procedure, nopass, private :: get_line_number
        procedure, pass, public  :: get_dset
        procedure, pass, public  :: get_title
        procedure, pass, public  :: get_undef
        procedure, pass, public  :: get_options
        procedure, pass, public  :: get_gridnum
        procedure, pass, public  :: get_nt
        procedure, pass, private :: get_n
        procedure, pass, public  :: get_x
        procedure, pass, public  :: get_y
        procedure, pass, public  :: get_z
        procedure, pass, private :: get_coordinate
        procedure, pass, public  :: get_tini
        procedure, pass, public  :: get_dt
        procedure, pass, public  :: get_nvars
        procedure, pass, public  :: get_var_idx
        !procedure, pass, public  :: get_var_description
        !procedure, pass, public  :: get_var_name
        procedure, nopass, private :: get_ctl_dir
        procedure, nopass, private :: skip_column

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
        output%lines = lines

        ! delete spaces from left of each line
        output%ctl_all(1:lines) = adjustl(output%ctl_all(1:lines))

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
        line          = adjustl(line(5:string_max))
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
        line      = adjustl(line(6:string_max))
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
        line = adjustl(line(6:string_max))
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
        line = adjustl(line(8:string_max))
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
            call self%get_n(self%xdef, &  !! IN
                          & nx         )  !! OUT
        endif

        if (present(ny)) then
            call self%get_n(self%ydef, &  !! IN
                          & ny         )  !! OUT
        endif

        if (present(nz)) then
            call self%get_n(self%zdef, &  !! IN
                          & nz         )  !! OUT
        endif

    end subroutine get_gridnum


    subroutine get_nt(self, output)
        class(ctl), intent(in)  :: self
        integer   , intent(out) :: output
        
        call self%get_n(self%tdef, &  !! IN
                      & output     )  !! OUT

    end subroutine get_nt


    subroutine get_n(self, line_number, output)
        class(ctl), intent(in)  :: self
        integer   , intent(in)  :: line_number
        integer   , intent(out) :: output

        character(string_max) :: line
        character(16)         :: work_n
        integer :: n_end

        line = trim(self%ctl_all(line_number))

        ! trimming
        line = adjustl(line(5:string_max))
        n_end = index(line(1:string_max), ' ') - 1
        ! if space is not found, the last character is selected
        if (n_end == -1) then
            n_end = string_max
        endif
        work_n = trim(line(1:n_end))

        read(work_n,*) output

    end subroutine get_n


    subroutine get_x(self, output)
        class(ctl)  , intent(in)  :: self
        real(real32), intent(out) :: output(:)

        integer :: n

        n = size(output)

        call self%get_coordinate(self%xdef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

    end subroutine get_x


    subroutine get_y(self, output)
        class(ctl)  , intent(in)  :: self
        real(real32), intent(out) :: output(:)

        integer :: n

        n = size(output)

        call self%get_coordinate(self%ydef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

    end subroutine get_y


    subroutine get_z(self, output)
        class(ctl)  , intent(in)  :: self
        real(real32), intent(out) :: output(:)

        integer :: n

        n = size(output)

        call self%get_coordinate(self%zdef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

    end subroutine get_z


    subroutine get_coordinate(self, line_number, n, output)
        class(ctl)  , intent(in)  :: self
        integer     , intent(in)  :: line_number
        integer     , intent(in)  :: n
        real(real32), intent(out) :: output(n)

        character(string_max)   :: line
        character(string_max*2) :: line_levels
        character(8)            :: specify_method
        character(8)            :: n_c
        real(real32)            :: min
        real(real32)            :: delta
        integer :: i
        integer :: where_method
        integer :: levels_start

        line = self%ctl_all(line_number)
        line = adjustl(line(5:string_max))

        read(line,*) n_c, specify_method
        if (to_lower(trim(specify_method)) == 'linear') then
            ! if coordinate is 'linear', compute it
            read(line,*) n_c, specify_method, min, delta
            output(1:n) = [(min+delta*real(i, kind=real32), i = 0, n-1)]
            return
        else
            ! if coordinate is 'levels', concatenate def and the next line and get $n numbers of levels
            where_method = index(to_lower(line), 'levels')
            levels_start = where_method + 6
            line_levels = trim(line) // ' ' // trim(self%ctl_all(line_number+1))
            line_levels = line_levels(levels_start:)

            read(line_levels,*) output(1:n)
            return
        endif

    end subroutine get_coordinate


    subroutine get_tini(self, calendar)
        class(ctl), intent(in)  :: self
        integer   , intent(out) :: calendar(5)

        character(string_max) :: line
        character(16) :: cal_str
        character(4)  :: month_list(12) = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
        integer :: cal_len
        integer :: where_month
        integer :: where_z
        integer :: where_colon
        integer :: month

        line = self%ctl_all(self%tdef)
        cal_str = trim(skip_column(line, 4, ' '))
        cal_str = to_lower(cal_str)
        cal_len = len_trim(cal_str)

        ! detect month
        do month = 1, 12
            where_month = index(cal_str, trim(month_list(month)))
            if (where_month /= 0) then
                where_month = where_month
                ! get month
                calendar(2) = month
                exit
            endif
        enddo

        ! get year
        read(cal_str(where_month+3:cal_len),*) calendar(1)
        ! If only the last two digits of the year are written, convert them into a four-digit year
        if (calendar(1) >= 50 .AND. calendar(1) <= 99) then
            calendar(1) = calendar(1) + 1900
        else if (calendar(1) >= 0 .AND. calendar(1) <= 49) then
            calendar(1) = calendar(1) + 2000
        endif

        where_z = index(cal_str, 'z')
        if (where_z == 0) then
            ! if minute and hour are not written
            if (where_month > 1) then
                ! if day is written
                read(cal_str(:where_month-1),*) calendar(3)
            else
                calendar(3) = 1
            endif
            calendar(4) = 0
            calendar(5) = 0
        else
            ! get day
            read(cal_str(where_z+1:where_month-1),*) calendar(3)
            read(cal_str(1:2),*) calendar(4)
            where_colon = index(cal_str, ':')
            if (where_colon == 0) then
                ! if minute is not written
                calendar(5) = 0
            else
                read(cal_str(where_colon+1:where_z-1),*) calendar(5)
            endif
        endif

    end subroutine get_tini


    subroutine get_dt(self, dt, unit)
        class(ctl)  , intent(in)  :: self
        integer     , intent(out) :: dt
        character(*), intent(out), optional :: unit

        character(string_max) :: line
        character(4)          :: dt_c
        integer               :: dt_len

        ! get delta_t column in lower cases
        line = self%ctl_all(self%tdef)
        dt_c = trim(skip_column(line, 5, ' '))
        dt_c = to_lower(dt_c)

        dt_len = len_trim(dt_c)
        ! get integer delta
        read(dt_c(1:dt_len-2),*) dt

        ! get unit of delta
        if (present(unit)) then
            unit = dt_c(dt_len-1:dt_len)
        endif

    end subroutine get_dt


    subroutine get_nvars(self, output)
        class(ctl), intent(in)  :: self
        integer   , intent(out) :: output

        character(string_max) :: line
        integer :: n_end

        line = self%ctl_all(self%vars)

        ! trimming
        line = adjustl(line(5:string_max))
        n_end = index(line(1:string_max), '*') - 1
        ! if space is not found, the last character is selected
        if (n_end == -1) then
            n_end = string_max
        endif
        line = trim(line(1:n_end))

        read(line,*) output

    end subroutine get_nvars


    subroutine get_var_idx(self, var, output)
        class(ctl)  , intent(in)  :: self
        character(*), intent(in)  :: var
        integer     , intent(out) :: output

        ! get the line $var is defined in
        call get_line_number(var                       , &
                           & self%lines                , &
                           & self%ctl_all(1:self%lines), &
                           & output                      )

        if (output == 0) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') trim(var) // ' was not found in ' // trim(self%ctlname)
            ERROR STOP
        endif

        output = output - self%vars

    end subroutine get_var_idx


    subroutine get_ctl_dir(ctlname, ctldir)
        character(*), intent(in)  :: ctlname
        character(*), intent(out) :: ctldir
        integer :: where_slash

        where_slash = index(ctlname, '/', back=.TRUE.)

        ctldir = ctlname(1:where_slash)

    end subroutine get_ctl_dir


    pure elemental function skip_column(input, target_column, delimiter) result(output)
        character(*), intent(in) :: input
        integer     , intent(in) :: target_column
        character(*), intent(in) :: delimiter
        character(len(input)) :: output
        character(len(input)) :: input_cp
        integer :: column_end
        integer :: column

        input_cp = adjustl(input)
        column_end = index(input_cp, '*') - 1
        ! delete comments
        if (column_end /= -1) then
            input_cp = input_cp(1:column_end)
        endif
            
        column_end = index(input_cp, delimiter) - 1
        ! delimiter was not found => end
        if (column_end == -1) then
            output = input_cp
            return
        endif

        do column = 1, len(input)
            output = adjustl(input_cp(1:column_end))
            !if (column_end == 0) then
            !    output = ''
            !endif
            
            if (column == target_column) then
                return
            endif
            input_cp = adjustl(input_cp(column_end+2:))
            column_end = index(input_cp, delimiter) - 1
        enddo

        ! Error
        output = ''

    end function skip_column


end module ctlinfo

