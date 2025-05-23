
program sample_ctlget
    
    use, intrinsic :: iso_fortran_env
    use ctlget, only : ctl

    implicit none
    
    type(ctl) :: input_ctl
    character(256), parameter :: ctlname='../ctl/sample.ctl'
    character(256) :: binname
    character(256) :: title
    real(real32)   :: undef
    character(16)  :: undef_char
    character(64)  :: options
    character(8)   :: endian
    integer        :: nx
    integer        :: ny
    integer        :: nz
    integer        :: nt
    real(real32), allocatable :: x(:)
    real(real32), allocatable :: y(:)
    real(real32), allocatable :: z(:)
    integer        :: initial_datetime(5)
    integer        :: dt
    character(2)   :: dt_unit
    integer        :: nvars
    character(16)  :: var
    integer        :: idx
    character(128) :: description

    input_ctl = ctl(ctlname=ctlname, &  !! IN : File name of the target control file
                  & linemax=100      )  !! IN : Maximum allowable number of lines in the file (default : 100)

    write(*,*)
    call input_ctl%get_dset(binname)  !! OUT : File name of the target binary file (absolute path)
    write(*,'(A)') 'Result of get_dset() : Binary file is ' // trim(binname)

    write(*,*)
    call input_ctl%get_title(title)  !! OUT : Title written in the control file
    write(*,'(A)') 'Result of get_title() : Title is ' // trim(title)

    write(*,*)
    call input_ctl%get_undef(undef     =undef     , &  !! OUT : Optional. Undef value. real32
                           & undef_char=undef_char  )  !! OUT : Optional. Undef value. character
    write(*,'(A,ES12.4)') 'Result of get_undef() : real32    = ', undef
    write(*,'(A)')       'Result of get_undef() : character = ' // trim(undef_char)

    write(*,*)
    call input_ctl%get_options(options)  !! OUT : Options written in the control file
    write(*,'(A)') 'Result of get_options() : ' // trim(options)

    write(*,*)
    if (input_ctl%isYrev()) then
        write(*,'(A)') 'YREV = True'
    else
        write(*,'(A)') 'YREV = False'
    endif

    if (input_ctl%isZrev()) then
        write(*,'(A)') 'ZREV = True'
    else
        write(*,'(A)') 'ZREV = False'
    endif

    if (input_ctl%includeLeap()) then
        write(*,'(A)') 'Data include leap days'
    else
        write(*,'(A)') 'Data do not include leap days'
    endif

    call input_ctl%get_endian(endian)
    write(*,'(A)') 'Data are written in ' // trim(endian) // '-endian'

    write(*,*)
    call input_ctl%get_gridnum(nx=nx, &  !! OUT : Optional. Number of grids in x-direction
                             & ny=ny, &  !! OUT : Optional. Number of grids in y-direction
                             & nz=nz  )  !! OUT : Optional. Number of grids in z-direction
    write(*,'(A,I0)') 'Result of get_gridnum() : NX = ', nx
    write(*,'(A,I0)') 'Result of get_gridnum() : NY = ', ny
    write(*,'(A,I0)') 'Result of get_gridnum() : NZ = ', nz

    call input_ctl%get_nt(nt)  !! OUT : Number of time steps
    write(*,'(A,I0)') 'Result of get_nt() : NT = ', nt

    allocate(x(nx))
    allocate(y(ny))
    allocate(z(nz))
    
    write(*,*)
    call input_ctl%get_x(x(1:nx))  !! OUT : x-coordinate
    write(*,'(A)') 'Result of get_x()'
    write(*,'(A,*(ES12.4,:,",  "))') 'X = ', x(1:nx)
    
    write(*,*)
    call input_ctl%get_y(y(1:ny))  !! OUT : x-coordinate
    write(*,'(A)') 'Result of get_y()'
    write(*,'(A,*(ES12.4,:,",  "))') 'Y = ', y(1:ny)
    
    write(*,*)
    call input_ctl%get_z(z(1:nz))  !! OUT : x-coordinate
    write(*,'(A)') 'Result of get_z()'
    write(*,'(A,*(ES12.4,:,",  "))') 'Z = ', z(1:nz)

    write(*,*)
    call input_ctl%get_tini(initial_datetime(1:5))  !! OUT : Date and time of the first record
    write(*,'(A,I0.4,"/",I0.2,"/",I0.2,X,I0.2,":",I0.2)') 'Result of get_tini() : Initial datetime is ',  initial_datetime(1:5)

    write(*,*)
    call input_ctl%get_dt(dt  =dt     , &  !! OUT : Increment to t
                        & unit=dt_unit  )  !! OUT : Optional. Unit of dt (mn, hr, dy, mo, or yr)
    write(*,'(A,I0,A)') 'Result of get_dt() : Increment to datetime is ', dt, trim(dt_unit)

    write(*,*)
    call input_ctl%get_nvars(nvars)  !! OUT : Number of variables defined in each time step
    write(*,'(A,I0)') 'Result of get_nvars() : Number of variables is ', nvars

    write(*,*)
    var = 'variable1'
    call input_ctl%get_var_idx(var, &  !! IN  : Name of the target variable
                             & idx  )  !! OUT : Index of the target variable
    write(*,'(A,I0)') 'Result of get_var_idx() : Index of ' // trim(var) // ' is ', idx

    write(*,*)
    idx = 3
    call input_ctl%get_var_name(idx, &  !! IN  : Index of the target variable
                              & var  )  !! OUT : Name of the target variable
    write(*,'(A,I0,A)') 'Result of get_var_name() : Name of index ', idx, ' is ' // trim(var)
    
    write(*,*)
    idx = 2
    call input_ctl%get_var_description(description, &  !! OUT
                                     & idx=idx      )  !! IN
    write(*,'(A,I0,A)') 'Result of get_var_description() : Description of index ', idx, ' is'
    write(*,'(A)')      trim(description)
    
    write(*,*)
    var = 'variable5'
    call input_ctl%get_var_description(description, &  !! OUT
                                     & var=var      )  !! IN
    write(*,'(A)') 'Result of get_var_description() : Description of ' // trim(var) // ' is'
    write(*,'(A)') trim(description)


    deallocate(x)
    deallocate(y)
    deallocate(z)


end program sample_ctlget
    


