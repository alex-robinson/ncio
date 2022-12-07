

program test

    use ncio 

    implicit none

    character(len=256) :: filename

    integer :: nx, ny, nz, nt 

    double precision, allocatable :: x(:), y(:), z(:), time(:) 

    integer,            allocatable :: i3D(:,:,:)
    real,               allocatable :: r3D(:,:,:)
    double precision,   allocatable :: d3D(:,:,:)
    logical,            allocatable :: l3d(:,:,:)
    character(len=256), allocatable :: c2D(:,:)
    
    integer :: i, j, k, t, testval 
    character(len=256) :: testchar

    integer :: ndims
    character(len=32), allocatable :: dimnames(:)
    integer, allocatable :: dimlens(:)

    ! Define array sizes and allocate arrays
    nx = 8
    ny = 10
    nz =  6
    nt =  4

    allocate(i3D(nx,ny,nz))
    allocate(r3D(nx,ny,nz))
    allocate(d3D(nx,ny,nz))
    allocate(l3D(nx,ny,nz))
    allocate(c2D(nx,ny))

    ! Fill in arrays with checkerboard patterns
    do i = 1, nx 
        do j = 1, ny 
            do k = 1, nz
                testval = mod(i+j+k,2) 

                i3D(i,j,k) = testval 
                r3D(i,j,k) = real(testval)
                d3D(i,j,k) = dble(testval)
                l3D(i,j,k) = testval .eq. 0 
            end do 
        end do 
    end do 

    ! Fill in character array with words 
    c2D = "testing" 

    write(*,*)
    write(*,*) "====== WRITING ======"
    write(*,*)

    ! Create test output file 
    filename = "out_ncio.nc"

    ! Create the netcdf file, write global attributes
    call nc_create(filename,overwrite=.TRUE.,netcdf4=.TRUE.)
    call nc_write_attr(filename,"title","Checkerboard output")
    call nc_write_attr(filename,"institution", &
                       "Universidad Complutense de Madrid; Potsdam Institute for Climate Impact Research")

    ! Write a projection map (not used) centered at [-39E,90N] and no easting or northing offset
    call nc_write_map(filename,"polar_stereographic",lambda=-39.d0,phi=90.d0,x_e=0.d0,y_n=0.d0) 

    ! Write the dimensions (p, x, y, z, time), defined inline
    call nc_write_dim(filename,"p",x=1,units="-")
    call nc_write_dim(filename,"x",x=1.d0,dx=1.d0,nx=nx,units="kilometers")
    call nc_write_dim(filename,"y",x=1.d0,dx=1.d0,nx=ny,units="kilometers")
    call nc_write_dim(filename,"z",x=1.d0,dx=2.d0,nx=nz,units="meters")
    call nc_write_dim(filename,"time",x=[0.d0,5.d0,100.d0,150.d0], &
                      units="years",calendar="360_day", unlimited=.TRUE.)
    
    ! Test writing: integers
    call nc_write(filename,"ip", [1],dim1="p")
    call nc_write(filename,"i1D",i3D(:,1,1),dim1="x")
    call nc_write(filename,"i2D",i3D(:,:,1),dim1="x",dim2="y")
    call nc_write(filename,"i3D",i3D(:,:,:),dim1="x",dim2="y",dim3="z")

    ! Test writing: integer time slices 
    do k = 1, nt 
        call nc_write(filename,"i2Dt",i3D(:,:,1)+k,dim1="x",dim2="y",dim3="time", &
                      start=[1,1,k],count=[nx,ny,1])
    end do 
    
    ! Test writing: overwrite values in range x=[3:5] with -1 
    call nc_write(filename,"i2Dt",i3D(3:5,:,1:nt)*0-1,dim1="x",dim2="y",dim3="time", &
                      start=[3,1,1],count=[3,ny,nt])

    ! Test writing: real
    call nc_write(filename,"rp", [1.0],dim1="p")
    call nc_write(filename,"r1D",r3D(:,1,1),dim1="x")
    call nc_write(filename,"r2D",r3D(:,:,1),dim1="x",dim2="y")
    call nc_write(filename,"r3D",r3D(:,:,:),dims=["x","y","z"])

    ! Test writing: double precision
    call nc_write(filename,"dp", [1.d0],dim1="p")
    call nc_write(filename,"d1D",d3D(:,1,1),dim1="x")
    call nc_write(filename,"d2D",d3D(:,:,1),dim1="x",dim2="y")
    call nc_write(filename,"d3D",d3D(:,:,:),dims=["x","y","z"])

    ! Test writing: logical
    call nc_write(filename,"lp", [.TRUE.],dim1="p")
    call nc_write(filename,"l1D",l3D(:,1,1),dim1="x")
    call nc_write(filename,"l2D",l3D(:,:,1),dim1="x",dim2="y")
    call nc_write(filename,"l3D",l3D(:,:,:),dims=["x","y","z"])

    ! Test writing: Character strings 
    call nc_write(filename,"c1D",c2D(:,1))
    call nc_write(filename,"c2D",c2D)  ! Should give a warning, since 2D char arrays are not supported!

    write(*,*)
    write(*,*) "====== READING ======"
    write(*,*)

    ! Read in netcdf information and print to screen 
    ! to confirm it matches original data
    call nc_read_attr(filename, "title", testchar)
    write(*,*) "Title: ", trim(testchar)
    
    call nc_read_attr(filename, "institution", testchar)
    write(*,*) "Institution: ", trim(testchar)
    
    allocate(x(nx),y(ny),z(nz),time(nt))
    call nc_read(filename, "x",x)
    call nc_read(filename, "y",y)
    call nc_read(filename, "z",z)
    call nc_read(filename, "time",time)
    
    write(*,"(a10,100f8.1)") "x:    ", x 
    write(*,"(a10,100f8.1)") "y:    ", y
    write(*,"(a10,100f8.1)") "z:    ", z  
    write(*,"(a10,100f8.1)") "time: ", time 

    ! Read 1D character array back in 
    c2D(:,1) = "X"
    call nc_read(filename,"c1D",c2D(:,1))
    write(*,"(a10,100a10)") "c1D: ", c2D(:,1)

    write(*,*)
    write(*,*) "====== DONE ======"
    write(*,*)

!     ndims = nc_ndims(filename,"d3D")
!     allocate(dimnames(ndims))
!     dimnames = nc_dimnames(filename,"d3D",ndims)
    
    call nc_dims(filename,"d3D",dimnames,dimlens)
!     write(*,*) "ndims= ", ndims
    write(*,*) "dimnames= ", dimnames
    write(*,*) "dimlens=  ", dimlens 

    deallocate(i3D)
    deallocate(r3D)
    deallocate(d3D)
    deallocate(l3D)
    deallocate(c2D)
    deallocate(x,y,z,time)

end program 
