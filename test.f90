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

    ctlname='JRA3Q_1990_2020_ZONAL_366.ctl'


    input = ctl(ctlname, 200)
    call input%get_dset(filename)
    call input%get_title(title)
    call input%get_undef(undef, undef_c)
    !call input%get_undef(output_char=undef_c)
    call input%get_options(options)
    call input%get_gridnum(nx=nx, ny=ny, nz=nz)
    call input%get_nt(nt)

    write(*,'(A)') 'FILE : ' // trim(filename)
    write(*,'(A)') 'TITLE : ' // trim(title)
    write(*,'(A,ES0.7)') 'UNDEF : ', undef
    write(*,'(A)') 'UNDEF : ' // trim(undef_c)
    write(*,'(A)') 'OPTIONS : ' // trim(options)
    write(*,'(A,I0)') 'NX : ', nx
    write(*,'(A,I0)') 'NY : ', ny
    write(*,'(A,I0)') 'NZ : ', nz
    write(*,'(A,I0)') 'NT : ', nt

end program test

