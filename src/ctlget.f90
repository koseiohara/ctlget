module ctlget

    use, intrinsic :: iso_fortran_env, only : real32, real64
    use caseconverter, only : to_lower

    implicit none

    private
    public :: ctl


    type ctl
        private
        character(256) :: ctlname                               ! File name of the control file
        character(len=:), allocatable :: ctl_all(:)             ! All lines of control file
        integer      :: number_of_variables                     ! Number of variables defined in the file
        integer      :: cmax                                    ! Acceptable maximum length of each line
        integer      :: lines                                   ! Number of lines of the control file
        logical      :: option_read                             ! Flag whether option has already read
        logical      :: yrev                                    ! wheter data is yrev
        logical      :: zrev                                    ! wheter data is zrev
        logical      :: calendar_365                            ! Whether data include leap days
        character(8) :: endian                                  ! Endian of binary file
        integer      :: dset                                    ! The line number dset statement is written
        integer      :: title                                   ! The line number title statement is written
        integer      :: undef                                   ! The line number undef statement is written
        integer      :: options                                 ! The line number options statement is written
        integer      :: xdef                                    ! The line number xdef statement is written
        integer      :: ydef                                    ! The line number ydef statement is written
        integer      :: zdef                                    ! The line number zdef statement is written
        integer      :: tdef                                    ! The line number tdef statement is written
        integer      :: vars                                    ! The line number vars statement is written
        logical      :: readable

        contains

        procedure, pass  , public  :: free
        procedure, pass  , public  :: get_dset
        procedure, pass  , public  :: get_title
        procedure, pass  , public  :: get_options
        procedure, pass  , public  :: isYrev
        procedure, pass  , public  :: isZrev
        procedure, pass  , public  :: includeLeap
        procedure, pass  , public  :: get_endian
        procedure, pass  , public  :: get_gridnum
        procedure, pass  , public  :: get_nt
        procedure, pass  , public  :: get_tini
        procedure, pass  , public  :: get_dt
        procedure, pass  , public  :: get_nvars
        procedure, pass  , public  :: get_var_idx
        procedure, pass  , public  :: get_var_name
        procedure, pass  , public  :: get_var_nz
        procedure, pass  , public  :: get_var_description
        procedure, pass  , private :: get_line_number
        procedure, nopass, private :: get_number_of_variables
        procedure, pass  , private :: get_n
        procedure, nopass, private :: get_ctl_dir
        procedure, nopass, private :: skip_column
        procedure, pass  , private :: memcheck

        generic, public :: get_undef      => get_undef_s, get_undef_d
        generic, public :: get_x          => get_x_s, get_x_d
        generic, public :: get_y          => get_y_s, get_y_d
        generic, public :: get_z          => get_z_s, get_z_d
        generic, public :: get_coordinate => get_coordinate_s, get_coordinate_d
        generic, public :: get_xinfo      => get_xinfo_s, get_xinfo_d
        generic, public :: get_yinfo      => get_yinfo_s, get_yinfo_d
        generic, public :: get_zinfo      => get_zinfo_s, get_zinfo_d
        generic, public :: get_axis_info  => get_axis_info_s , get_axis_info_d

        procedure, pass  , public  :: get_undef_s
        procedure, pass  , public  :: get_undef_d
        procedure, pass  , public  :: get_x_s
        procedure, pass  , public  :: get_x_d
        procedure, pass  , public  :: get_y_s
        procedure, pass  , public  :: get_y_d
        procedure, pass  , public  :: get_z_s
        procedure, pass  , public  :: get_z_d
        procedure, pass  , private :: get_coordinate_s
        procedure, pass  , private :: get_coordinate_d
        procedure, pass  , public  :: get_xinfo_s
        procedure, pass  , public  :: get_xinfo_d
        procedure, pass  , public  :: get_yinfo_s
        procedure, pass  , public  :: get_yinfo_d
        procedure, pass  , public  :: get_zinfo_s
        procedure, pass  , public  :: get_zinfo_d
        procedure, pass  , private :: get_axis_info_s
        procedure, pass  , private :: get_axis_info_d
    end type ctl


    interface ctl
        module procedure init
    end interface ctl


 
    contains


    ! Constructor
    function init(ctlname, linemax, columnmax) result(output)
        character(*), intent(in) :: ctlname
        integer     , intent(in), optional :: linemax       ! DEFAULT : 100
        integer     , intent(in), optional :: columnmax     ! DEFAULT : 256

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

        if (present(columnmax)) then
            if (columnmax <= 0) then
                write(0,'(A)') '<ERROR STOP>'
                write(0,'(A)') 'Invalid columnmax to read ' // trim(ctlname)
                write(0,'(A,I0)') 'Specified : ', columnmax
                ERROR STOP
            endif
            output % cmax = columnmax
        else
            output % cmax = 256
        endif

        allocate(character(output % cmax) :: output%ctl_all(lmax))
        output%readable = .TRUE.

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
            lines = i
            ! end the loop when EOF was found
            if (EOF /= 0) then
                lines = lines - 1
                exit
            endif
        enddo

        close(unit)

        ! delete spaces from left of each line
        output%ctl_all(1:lines) = adjustl(output%ctl_all(1:lines))

        call output % get_line_number(FLAG   ='dset'                 , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%dset              )  !! OUT

        call output % get_line_number(FLAG   ='title'                , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%title             )  !! OUT

        call output % get_line_number(FLAG   ='undef'                , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%undef             )  !! OUT

        call output % get_line_number(FLAG   ='options'              , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%options           )  !! OUT

        call output % get_line_number(FLAG   ='xdef'                 , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%xdef              )  !! OUT

        call output % get_line_number(FLAG   ='ydef'                 , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%ydef              )  !! OUT

        call output % get_line_number(FLAG   ='zdef'                 , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%zdef              )  !! OUT

        call output % get_line_number(FLAG   ='tdef'                 , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%tdef              )  !! OUT

        call output % get_line_number(FLAG   ='vars'                 , &  !! IN
                                    & LINES  =lines                  , &  !! IN
                                    & CTL_ALL=output%ctl_all(1:lines), &  !! IN
                                    & LINE   =output%vars              )  !! OUT

        call get_number_of_variables(output)  !! INOUT

        ! Name of control file
        output%ctlname = trim(ctlname)
        ! Number of lines in control file
        output%lines = lines
        ! set status whether option has already read
        output%option_read = .FALSE.

    end function init


    !! Destructor : deallocate ctl_all
    !subroutine del(self)
    !    !type(ctl), intent(inout) :: self
    !    class(ctl), intent(inout) :: self

    !    if (allocated(self%ctl_all)) then
    !        deallocate(self%ctl_all)
    !    endif

    !end subroutine del


    subroutine free(self)
        class(ctl), intent(inout) :: self

        if (allocated(self%ctl_all)) then
            deallocate(self%ctl_all)
            self%readable = .FALSE.
        endif

    end subroutine free


    subroutine get_dset(self, output)
        class(ctl)  , intent(in)  :: self
        character(*), intent(out) :: output

        character(self%cmax) :: line
        character(256)       :: work_filename
        character(256)       :: ctl_dir
        integer :: filename_end

        call self%memcheck('get_dset')  !! IN

        ! get the dset line
        line = trim(self%ctl_all(self%dset))

        ! trimming
        line          = adjustl(line(5:self%cmax))
        filename_end  = index(line(1:self%cmax), ' ') - 1
        ! if space is not found, the last character is selected
        if (filename_end == -1) then
            filename_end = self%cmax
        endif
        work_filename = line(1:filename_end)

        if (work_filename(1:1) == '^') then
            ! if binary name is written by relative path, delete '^' and add the path of control file
            work_filename = trim(work_filename(2:256))
            if (work_filename(1:2) == './') then
                work_filename = work_filename(3:256)
            endif

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

        character(self%cmax) :: line
        integer :: title_end

        call self%memcheck('get_title')  !! IN

        ! get the title line
        line = trim(self%ctl_all(self%title))

        ! trimming
        line      = adjustl(line(6:self%cmax))
        title_end = index(line(1:self%cmax), '*') - 1
        ! if space is not found, the last character is selected
        if (title_end == -1) then
            title_end = self%cmax
        endif
        output = trim(line(1:title_end))

    end subroutine get_title


    subroutine get_undef_s(self, undef, undef_char)
        integer, parameter :: lrk=real32
        class(ctl)  , intent(in)  :: self
        real(lrk)   , intent(out) :: undef
        character(*), intent(out), optional :: undef_char

        character(self%cmax) :: line
        character(64) :: work_undef
        integer :: undef_end

        call self%memcheck('get_undef')  !! IN

        ! get the undef line
        line = trim(self%ctl_all(self%undef))

        ! trimming
        line = adjustl(line(6:self%cmax))
        undef_end = index(line(1:self%cmax), ' ') - 1
        ! if space is not found, the last character is selected
        if (undef_end == -1) then
            undef_end = self%cmax
        endif
        work_undef = trim(line(1:undef_end))
        
        ! get undef in real32
        read(work_undef,*) undef

        ! get undef in char
        if (present(undef_char)) then
            undef_char = trim(work_undef)
        endif

    end subroutine get_undef_s


    subroutine get_undef_d(self, undef, undef_char)
        integer, parameter :: lrk=real64
        class(ctl)  , intent(in)  :: self
        real(lrk)   , intent(out) :: undef
        character(*), intent(out), optional :: undef_char

        character(self%cmax) :: line
        character(64) :: work_undef
        integer :: undef_end

        call self%memcheck('get_undef')  !! IN

        ! get the undef line
        line = trim(self%ctl_all(self%undef))

        ! trimming
        line = adjustl(line(6:self%cmax))
        undef_end = index(line(1:self%cmax), ' ') - 1
        ! if space is not found, the last character is selected
        if (undef_end == -1) then
            undef_end = self%cmax
        endif
        work_undef = trim(line(1:undef_end))
        
        ! get undef in real64
        read(work_undef,*) undef

        ! get undef in char
        if (present(undef_char)) then
            undef_char = trim(work_undef)
        endif

    end subroutine get_undef_d


    subroutine get_options(self, output)
        class(ctl)  , intent(inout) :: self
        character(*), intent(out)   :: output

        character(self%cmax) :: line
        character(self%cmax) :: option_cp
        integer :: option_end

        call self%memcheck('get_options')  !! IN

        line = trim(self%ctl_all(self%options))

        ! trimming
        line = adjustl(line(8:self%cmax))
        option_end = index(line(1:self%cmax), '*') - 1
        ! if space is not found, the last character is selected
        if (option_end == -1) then
            option_end = self%cmax
        endif

        output = trim(line(1:option_end))

        if (.NOT. self%option_read) then
            ! set option related variables of ctl
            option_cp = to_lower(trim(line(1:option_end)))
            if (index(option_cp, 'yrev') == 0) then
                self%yrev = .FALSE.
            else
                self%yrev = .TRUE.
            endif

            if (index(option_cp, 'zrev') == 0) then
                self%zrev = .FALSE.
            else
                self%zrev = .TRUE.
            endif

            if (index(option_cp, '365_day_calendar') == 0) then
                self%calendar_365 = .FALSE.
            else
                self%calendar_365 = .TRUE.
            endif

            if (index(option_cp, 'little_endian') > 0) then
                self%endian = 'little'
            else if (index(option_cp, 'big_endian') > 0) then
                self%endian = 'big'
            else
                self%endian = 'native'
            endif
            
            self%option_read = .TRUE.
        endif

    end subroutine get_options


    function isYrev(self) result(output)
        class(ctl), intent(inout) :: self

        logical      :: output
        character(1) :: dummy

        call self%memcheck('isYrev')  !! IN

        if (.NOT. self%option_read) then
            call self%get_options(dummy)  !! OUT
        endif

        output = self%yrev

    end function isYrev


    function isZrev(self) result(output)
        class(ctl), intent(inout) :: self

        logical      :: output
        character(1) :: dummy

        call self%memcheck('isZrev')  !! IN

        if (.NOT. self%option_read) then
            call self%get_options(dummy)  !! OUT
        endif

        output = self%zrev

    end function isZrev


    function includeLeap(self) result(output)
        class(ctl), intent(inout) :: self

        logical      :: output
        character(1) :: dummy

        call self%memcheck('includeLeap')  !! IN

        if (.NOT. self%option_read) then
            call self%get_options(dummy)  !! OUT
        endif

        output = (.NOT. self%calendar_365)

    end function includeLeap


    subroutine get_endian(self, output)
        class(ctl) , intent(inout) :: self
        character(*), intent(out)  :: output

        character(1) :: dummy

        call self%memcheck('get_endian')  !! IN

        if (.NOT. self%option_read) then
            call self%get_options(dummy)  !! OUT
        endif

        output = self%endian

    end subroutine get_endian


    subroutine get_gridnum(self, nx, ny, nz)
        class(ctl), intent(in) :: self
        integer   , intent(out), optional :: nx
        integer   , intent(out), optional :: ny
        integer   , intent(out), optional :: nz

        call self%memcheck('get_gridnum')  !! IN

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
        
        call self%memcheck('get_nt')  !! IN

        call self%get_n(self%tdef, &  !! IN
                      & output     )  !! OUT

    end subroutine get_nt


    subroutine get_x_s(self, output)
        class(ctl)  , intent(in)  :: self
        real(real32), intent(out) :: output(:)

        integer :: n

        call self%memcheck('get_x')  !! IN

        n = size(output)

        call self%get_coordinate(self%xdef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

    end subroutine get_x_s


    subroutine get_x_d(self, output)
        class(ctl)  , intent(in)  :: self
        real(real64), intent(out) :: output(:)

        integer :: n

        call self%memcheck('get_x')  !! IN

        n = size(output)

        call self%get_coordinate(self%xdef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

    end subroutine get_x_d


    subroutine get_y_s(self, output)
        class(ctl)  , intent(inout) :: self
        real(real32), intent(out)   :: output(:)

        integer :: n

        call self%memcheck('get_y')  !! IN

        n = size(output)

        call self%get_coordinate(self%ydef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

        if (self.isYrev()) then
            output(1:n) = output(n:1:-1)
        endif

    end subroutine get_y_s


    subroutine get_y_d(self, output)
        class(ctl)  , intent(inout) :: self
        real(real64), intent(out)   :: output(:)

        integer :: n

        call self%memcheck('get_y')  !! IN

        n = size(output)

        call self%get_coordinate(self%ydef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

        if (self.isYrev()) then
            output(1:n) = output(n:1:-1)
        endif

    end subroutine get_y_d


    subroutine get_z_s(self, output)
        class(ctl)  , intent(inout) :: self
        real(real32), intent(out)   :: output(:)

        integer :: n

        call self%memcheck('get_z')  !! IN

        n = size(output)

        call self%get_coordinate(self%zdef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

        if (self.isZrev()) then
            output(1:n) = output(n:1:-1)
        endif

    end subroutine get_z_s


    subroutine get_z_d(self, output)
        class(ctl)  , intent(inout) :: self
        real(real64), intent(out)   :: output(:)

        integer :: n

        call self%memcheck('get_z')  !! IN

        n = size(output)

        call self%get_coordinate(self%zdef  , &  !! IN
                               & n          , &  !! IN
                               & output(1:n)  )  !! OUT

        if (self.isZrev()) then
            output(1:n) = output(n:1:-1)
        endif

    end subroutine get_z_d


    subroutine get_xinfo_s(self, xmin, dx, islinear)
        integer, parameter :: lrk=real32
        class(ctl), intent(in)  :: self
        real(lrk) , intent(out) :: xmin
        real(lrk) , intent(out) :: dx
        logical   , intent(out), optional :: islinear

        character(8) :: method

        call self%memcheck('get_xinfo')  !! IN

        call self%get_axis_info(self%xdef, &  !! IN
                              & method   , &  !! OUT
                              & xmin     , &  !! OUT
                              & dx         )  !! OUT

        if (present(islinear)) then
            method = to_lower(method)
            islinear = (trim(method) == 'linear')
        endif

    end subroutine get_xinfo_s


    subroutine get_xinfo_d(self, xmin, dx, islinear)
        integer, parameter :: lrk=real64
        class(ctl), intent(in)  :: self
        real(lrk) , intent(out) :: xmin
        real(lrk) , intent(out) :: dx
        logical   , intent(out), optional :: islinear

        character(8) :: method

        call self%memcheck('get_xinfo')  !! IN

        call self%get_axis_info(self%xdef, &  !! IN
                              & method   , &  !! OUT
                              & xmin     , &  !! OUT
                              & dx         )  !! OUT

        if (present(islinear)) then
            method = to_lower(method)
            islinear = (trim(method) == 'linear')
        endif

    end subroutine get_xinfo_d


    subroutine get_yinfo_s(self, ymin, dy, islinear)
        integer, parameter :: lrk=real32
        class(ctl), intent(in)  :: self
        real(lrk) , intent(out) :: ymin
        real(lrk) , intent(out) :: dy
        logical   , intent(out), optional :: islinear

        character(8) :: method

        call self%memcheck('get_yinfo')  !! IN

        call self%get_axis_info(self%ydef, &  !! IN
                              & method   , &  !! OUT
                              & ymin     , &  !! OUT
                              & dy         )  !! OUT

        if (present(islinear)) then
            method = to_lower(method)
            islinear = (trim(method) == 'linear')
        endif

    end subroutine get_yinfo_s


    subroutine get_yinfo_d(self, ymin, dy, islinear)
        integer, parameter :: lrk=real64
        class(ctl), intent(in)  :: self
        real(lrk) , intent(out) :: ymin
        real(lrk) , intent(out) :: dy
        logical   , intent(out), optional :: islinear

        character(8) :: method

        call self%memcheck('get_yinfo')  !! IN

        call self%get_axis_info(self%ydef, &  !! IN
                              & method   , &  !! OUT
                              & ymin     , &  !! OUT
                              & dy         )  !! OUT

        if (present(islinear)) then
            method = to_lower(method)
            islinear = (trim(method) == 'linear')
        endif

    end subroutine get_yinfo_d


    subroutine get_zinfo_s(self, zmin, dz, islinear)
        integer, parameter :: lrk=real32
        class(ctl), intent(in)  :: self
        real(lrk) , intent(out) :: zmin
        real(lrk) , intent(out) :: dz
        logical   , intent(out), optional :: islinear

        character(8) :: method

        call self%memcheck('get_zinfo')  !! IN

        call self%get_axis_info(self%zdef, &  !! IN
                              & method   , &  !! OUT
                              & zmin     , &  !! OUT
                              & dz         )  !! OUT

        if (present(islinear)) then
            method = to_lower(method)
            islinear = (trim(method) == 'linear')
        endif

    end subroutine get_zinfo_s


    subroutine get_zinfo_d(self, zmin, dz, islinear)
        integer, parameter :: lrk=real64
        class(ctl), intent(in)  :: self
        real(lrk) , intent(out) :: zmin
        real(lrk) , intent(out) :: dz
        logical   , intent(out), optional :: islinear

        character(8) :: method

        call self%memcheck('get_zinfo')  !! IN

        call self%get_axis_info(self%zdef, &  !! IN
                              & method   , &  !! OUT
                              & zmin     , &  !! OUT
                              & dz         )  !! OUT

        if (present(islinear)) then
            method = to_lower(method)
            islinear = (trim(method) == 'linear')
        endif

    end subroutine get_zinfo_d


    subroutine get_tini(self, calendar)
        class(ctl), intent(in)  :: self
        integer   , intent(out) :: calendar(5)

        character(self%cmax) :: line
        character(16) :: cal_str
        character(4)  :: month_list(12) = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
        integer :: cal_len
        integer :: where_month
        integer :: where_z
        integer :: where_colon
        integer :: month

        call self%memcheck('get_tini')  !! IN

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

        character(self%cmax) :: line
        character(4)         :: dt_c
        integer              :: dt_len

        call self%memcheck('get_dt')  !! IN

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

        call self%memcheck('get_nvars')  !! IN

        output = self%number_of_variables

    end subroutine get_nvars


    subroutine get_var_idx(self, var, output)
        class(ctl)  , intent(in)  :: self
        character(*), intent(in)  :: var
        integer     , intent(out) :: output

        call self%memcheck('get_var_idx')  !! IN

        ! get the line $var is defined in
        call self % get_line_number(var                       , &
                                  & self%lines                , &
                                  & self%ctl_all(1:self%lines), &
                                  & output                      )

        output = output - self%vars

    end subroutine get_var_idx


    subroutine get_var_name(self, idx, output)
        class(ctl)  , intent(in)  :: self
        integer     , intent(in)  :: idx
        character(*), intent(out) :: output

        character(self%cmax) :: line
        integer              :: var_end

        call self%memcheck('get_var_name')  !! IN

        if (idx > self%number_of_variables) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'In get_var_name() : The specified index exceeds the number of variables'
            write(0,'(A,I0)') 'Number of Variables Defined in This File : ', self%number_of_variables
            write(0,'(A,I0)') 'Specified Index                          : ', idx
            ERROR STOP
        endif

        line = self%ctl_all(self%vars + idx)
        var_end = index(line, ' ') - 1

        output = line(1:var_end)

    end subroutine get_var_name


    subroutine get_var_nz(self, output, idx, var)
        class(ctl)  , intent(in)  :: self
        integer     , intent(out) :: output
        integer     , intent(in), optional :: idx
        character(*), intent(in), optional :: var

        character(self%cmax) :: line
        character(8)         :: nz_str
        integer :: idx_cp

        call self%memcheck('get_var_nz')  !! IN

        if (present(idx)) then
            if (idx > self%number_of_variables) then
                write(0,'(A)') '<ERROR STOP>'
                write(0,'(A)') 'In get_var_nz() : The specified index exceeds the number of variables'
                write(0,'(A,I0)') 'Number of Variables Defined in This File : ', self%number_of_variables
                write(0,'(A,I0)') 'Specified Index                          : ', idx
                ERROR STOP
            endif

            idx_cp = idx
        else if (present(var)) then
            ! get the line $var is defined in
            call self % get_line_number(var                       , &
                                      & self%lines                , &
                                      & self%ctl_all(1:self%lines), &
                                      & idx_cp                      )

            if (idx_cp == 0) then
                write(0,'(A)') '<ERROR STOP>'
                write(0,'(A)') trim(var) // ' was not found in ' // trim(self%ctlname)
                ERROR STOP
            endif

            idx_cp = idx_cp - self%vars
        else
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'In get_var_nz() : Both "idx" and "var" were not provided'
            ERROR STOP
        endif
        line   = self%ctl_all(self%vars + idx_cp)
        nz_str = skip_column(line, 2, ' ')

        read(nz_str,*) output

    end subroutine get_var_nz


    subroutine get_var_description(self, output, idx, var)
        class(ctl)  , intent(in)  :: self
        character(*), intent(out) :: output
        integer     , intent(in), optional :: idx
        character(*), intent(in), optional :: var

        character(self%cmax) :: line
        integer :: idx_cp
        integer :: where_space

        call self%memcheck('get_var_description')  !! IN

        if (present(idx)) then
            if (idx > self%number_of_variables) then
                write(0,'(A)') '<ERROR STOP>'
                write(0,'(A)') 'In get_var_description() : The specified index exceeds the number of variables'
                write(0,'(A,I0)') 'Number of Variables Defined in This File : ', self%number_of_variables
                write(0,'(A,I0)') 'Specified Index                          : ', idx
                ERROR STOP
            endif

            idx_cp = idx
        else if (present(var)) then
            ! get the line $var is defined in
            call self % get_line_number(var                       , &
                                      & self%lines                , &
                                      & self%ctl_all(1:self%lines), &
                                      & idx_cp                      )

            if (idx_cp == 0) then
                write(0,'(A)') '<ERROR STOP>'
                write(0,'(A)') trim(var) // ' was not found in ' // trim(self%ctlname)
                ERROR STOP
            endif

            idx_cp = idx_cp - self%vars
        else
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'In get_var_description() : Both "idx" and "var" were not provided'
            ERROR STOP
        endif

        line = self%ctl_all(self%vars + idx_cp)

        ! delete the variable-name column
        where_space = index(line, ' ')
        line = adjustl(line(where_space+1:self%cmax))
        ! delete the number-of-layers column
        where_space = index(line, ' ')
        line = adjustl(line(where_space+1:self%cmax))
        ! delete the unit column
        where_space = index(line, ' ')
        line = adjustl(line(where_space+1:self%cmax))

        output = trim(line)

    end subroutine get_var_description


    subroutine get_line_number(self, flag, lines, ctl_all, line)
        class(ctl)  , intent(in)  :: self
        character(*), intent(in)  :: flag
        integer     , intent(in)  :: lines
        character(*), intent(in)  :: ctl_all(lines)
        integer     , intent(out) :: line

        character(self%cmax) :: string
        character(64)        :: line_flag
        character(64)        :: flag_lower
        integer :: where_space
        integer :: i

        call self%memcheck('get_line_number')  !! IN

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
                    return
                endif
            endif
        enddo

        ! if the flag was not found, return 0
        line = 0

    end subroutine get_line_number


    subroutine get_number_of_variables(self)
        class(ctl), intent(inout) :: self

        character(self%cmax) :: line
        integer :: n_end

        line = self%ctl_all(self%vars)

        ! trimming
        line = adjustl(line(5:self%cmax))
        n_end = index(line(1:self%cmax), '*') - 1
        ! if space is not found, the last character is selected
        if (n_end == -1) then
            n_end = self%cmax
        endif
        line = trim(line(1:n_end))

        read(line,*) self%number_of_variables

    end subroutine get_number_of_variables


    subroutine get_n(self, line_number, output)
        class(ctl), intent(in)  :: self
        integer   , intent(in)  :: line_number
        integer   , intent(out) :: output

        character(self%cmax) :: line
        character(16)        :: work_n
        integer :: n_end

        line = trim(self%ctl_all(line_number))

        ! trimming
        line = adjustl(line(5:self%cmax))
        n_end = index(line(1:self%cmax), ' ') - 1
        ! if space is not found, the last character is selected
        if (n_end == -1) then
            n_end = self%cmax
        endif
        work_n = trim(line(1:n_end))

        read(work_n,*) output

    end subroutine get_n


    subroutine get_coordinate_s(self, line_number, n, output)
        integer, parameter :: lrk=real32
        class(ctl), intent(in)  :: self
        integer   , intent(in)  :: line_number
        integer   , intent(in)  :: n
        real(lrk) , intent(out) :: output(n)

        character(self%cmax)   :: line
        character(self%cmax*2) :: line_levels
        character(8)           :: specify_method
        character(8)           :: n_c
        real(lrk)              :: min
        real(lrk)              :: delta
        integer :: i
        integer :: where_method
        integer :: levels_start

        line = self%ctl_all(line_number)
        line = adjustl(line(5:self%cmax))

        read(line,*) n_c, specify_method
        if (to_lower(trim(specify_method)) == 'linear') then
            ! if coordinate is 'linear', compute it
            read(line,*) n_c, specify_method, min, delta
            output(1:n) = [(min+delta*real(i, kind=lrk), i = 0, n-1)]
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

    end subroutine get_coordinate_s


    subroutine get_coordinate_d(self, line_number, n, output)
        integer, parameter :: lrk=real64
        class(ctl), intent(in)  :: self
        integer   , intent(in)  :: line_number
        integer   , intent(in)  :: n
        real(lrk) , intent(out) :: output(n)

        character(self%cmax)   :: line
        character(self%cmax*2) :: line_levels
        character(8)           :: specify_method
        character(8)           :: n_c
        real(lrk)              :: min
        real(lrk)              :: delta
        integer :: i
        integer :: where_method
        integer :: levels_start

        line = self%ctl_all(line_number)
        line = adjustl(line(5:self%cmax))

        read(line,*) n_c, specify_method
        if (to_lower(trim(specify_method)) == 'linear') then
            ! if coordinate is 'linear', compute it
            read(line,*) n_c, specify_method, min, delta
            output(1:n) = [(min+delta*real(i, kind=lrk), i = 0, n-1)]
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

    end subroutine get_coordinate_d


    subroutine get_axis_info_s(self, line_number, method, minimum, delta)
        integer, parameter :: lrk=real32
        class(ctl)  , intent(in)  :: self
        integer     , intent(in)  :: line_number
        character(*), intent(out) :: method
        real(lrk)   , intent(out) :: minimum
        real(lrk)   , intent(out) :: delta

        character(self%cmax) :: line
        integer      :: dummy

        line = self%ctl_all(line_number)
        line = adjustl(line(5:self%cmax))

        read(line,*) dummy, method

        if (to_lower(trim(method)) == 'linear') then
            read(line,*) dummy, method, minimum, delta
        endif

    end subroutine get_axis_info_s


    subroutine get_axis_info_d(self, line_number, method, minimum, delta)
        integer, parameter :: lrk=real64
        class(ctl)  , intent(in)  :: self
        integer     , intent(in)  :: line_number
        character(*), intent(out) :: method
        real(lrk)   , intent(out) :: minimum
        real(lrk)   , intent(out) :: delta

        character(self%cmax) :: line
        integer      :: dummy

        line = self%ctl_all(line_number)
        line = adjustl(line(5:self%cmax))

        read(line,*) dummy, method

        if (to_lower(trim(method)) == 'linear') then
            read(line,*) dummy, method, minimum, delta
        endif

    end subroutine get_axis_info_d


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
            if (column == target_column) then
                output = adjustl(input_cp(1:column_end))
                return
            endif
            input_cp = adjustl(input_cp(column_end+2:))
            column_end = index(input_cp, delimiter) - 1
        enddo

        ! Error
        output = ''

    end function skip_column


    subroutine memcheck(self, func)
        use, intrinsic :: iso_fortran_env, only : err=>error_unit
        class(ctl)  , intent(in) :: self
        character(*), intent(in) :: func

        if (.NOT. self%readable) then
            write(err,'(A)') '<ERROR STOP>'
            write(err,'(A)') 'In ' // trim(func) // '(): memory was already released'
            ERROR STOP
        endif

    end subroutine memcheck

end module ctlget

