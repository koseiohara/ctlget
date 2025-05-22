program test
    
    use, intrinsic :: iso_fortran_env
    use ctlinfo, only : ctl

    implicit none
    
    type(ctl) :: input
    character(256) :: ctlname='/mnt/jet11/kosei/mim/mim_modern/output/JRA3Q/JRA3Q_1990_2020_ZONAL_366.ctl'
    character(256) :: filename
    character(256) :: title
    character(256) :: undef_c
    real(real32)  :: undef
    character(256) :: options
    integer :: nx
    integer :: ny
    integer :: nz
    integer :: nt
    real(real32), allocatable :: x(:)
    real(real32), allocatable :: y(:)
    real(real32), allocatable :: z(:)

    integer :: calendar(5)
    integer :: dt
    character(8) :: dt_unit
    integer :: vars

    character(16) :: var
    integer :: idx

    character(128) :: descr

    ctlname='JRA3Q_1990_2020_ZONAL_366.ctl'


    input = ctl(ctlname, 200)
    call input%get_dset(filename)
    write(*,'(A)') 'FILE : ' // trim(filename)
    call input%get_title(title)
    write(*,'(A)') 'TITLE : ' // trim(title)
    call input%get_undef(undef, undef_c)
    write(*,'(A,ES0.7)') 'UNDEF : ', undef
    write(*,'(A)') 'UNDEF : ' // trim(undef_c)

    if (input%includeLeap()) then
        write(*,*) 'File includes Leap days'
    else
        write(*,*) 'File do not includes Leap days'
    endif

    if (input%isZrev()) then
        write(*,*) 'File is Zrev'
    else
        write(*,*) 'File is not Zrev'
    endif

    call input%get_undef(output_char=undef_c)
    call input%get_options(options)
    write(*,'(A)') 'OPTIONS : ' // trim(options)
    call input%get_gridnum(nx=nx, ny=ny, nz=nz)
    write(*,'(A,I0)') 'NX : ', nx
    write(*,'(A,I0)') 'NY : ', ny
    write(*,'(A,I0)') 'NZ : ', nz
    call input%get_nt(nt)
    write(*,'(A,I0)') 'NT : ', nt

    if (input%isYrev()) then
        write(*,*) 'File is Yrev'
    else
        write(*,*) 'File is not Yrev'
    endif

    write(*,*) trim(input%getEndian()) // '-endian'

    allocate(x(nx))
    allocate(y(ny))
    allocate(z(nz))



    call input%get_x(x(1:nx))
    write(*,'(A,*(ES0.3,:,", "))') 'X : ', x(1:nx)
    call input%get_y(y(1:ny))
    write(*,'(A,*(ES0.3,:,", "))') 'Y : ', y(1:ny)
    call input%get_z(z(1:nz))
    write(*,'(A,*(ES0.3,:,", "))') 'Z : ', z(1:nz)

    call input%get_tini(calendar(1:5))
    write(*,'(*(I0,:,"/"))') calendar(1:5)

    call input%get_dt(dt)
    write(*,'(I0,A)') dt, trim(dt_unit)

    call input%get_nvars(vars)
    write(*,'(A,I0)') 'VARS : ', vars

    var = 'kz'
    call input%get_var_idx(var, idx)
    write(*,'(A,I0)') 'index of ' // trim(var) // ' is ', idx

    idx = 64
    call input%get_var_name(idx, var)
    write(*,'(A,I0)') 'index of ' // trim(var) // ' is ', idx

    call input%get_var_description(descr, var=var)
    write(*,'(A)') trim(descr)

end program test

