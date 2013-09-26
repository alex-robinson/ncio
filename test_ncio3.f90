
!! TO COMPILE : 
!! gfortran -o test_ncio3.x -fcheck=all -I/opt/local/include ncio3.f90 test_ncio3.f90 -L/opt/local/lib -lnetcdff -lnetcdf
!! or
!! ifort -g -I/home/robinson/apps/netcdf/netcdf/include -o test_ncio3.x ncio3.f90 test_ncio3.f90 -L/home/robinson/apps/netcdf/netcdf/lib -lnetcdf

program test

    use ncio 

    implicit none

    integer :: t

    character(len=256) :: fnm, fnm_in, fnm_out, mapping

    integer :: nx, ny, nk
    double precision, allocatable, dimension(:,:) :: lon,lat,zs
    integer, allocatable, dimension(:,:) :: mask 
    double precision, allocatable, dimension(:,:,:) :: vx
    double precision, allocatable, dimension(:,:,:,:) :: vx4D 
    logical, allocatable, dimension(:,:) :: masklogic
    character(len=256), allocatable, dimension(:,:) :: char2D

    integer :: i, j 

    nx = 76
    ny = 141 
    nk = 5
    allocate( lon(nx,ny))
    allocate( lat(nx,ny))
    allocate(  zs(nx,ny))
    allocate(mask(nx,ny))
    allocate(  vx(nx,ny,nk))
    allocate(  vx4D(nx,ny,nk,1))
    allocate(masklogic(nx,ny))
    allocate(char2D(nx,ny))

    ! Load data
    fnm_in = "topo.20km.nc"
    call nc_read(fnm_in,lon,"lon")
    call nc_read(fnm_in,lat,"lat")
    call nc_read(fnm_in,mask,"mask")

    fnm_out = "out_ncio3.nc"
    mapping = "stereographic"

    ! Create the netcdf file and the dimension variables
    call nc_create(fnm_out)
    call nc_write_global(fnm_out,"title","Greenland simulation")
    call nc_write_global(fnm_out,"institution", &
                         "Universidad Complutense de Madrid; Potsdam Institute for Climate Impact Research")
    
    call nc_write_dim(fnm_out,"xc",x0=-800.d0, dx=20d0,nx=nx,units="kilometers")
    call nc_write_dim(fnm_out,"yc",x0=-3400.d0,dx=20d0,nx=ny,units="kilometers")
    call nc_write_dim(fnm_out,"time",x=(/ 0.d0,5.d0,100.d0 /),units="years",calendar="360_day")
    call nc_write_dim(fnm_out,"parameter",x=(/ 1.d0 /),units="none")
    call nc_write_dim(fnm_out,"kc",x0=1.d0,nx=nk,units="none")

    call nc_write_map(fnm_out,mapping,lambda=-39.d0,phi=90.d0,x_e=0.d0,y_n=0.d0)

    ! Writing a parameter value
    call nc_write(fnm_out,(/15/),"p1",dim1="parameter")

    ! Update time 
    call nc_write(fnm_out,(/15.d0/),"time",dim1="time",start=(/2/))

    ! Writing a 2D mask and some slices
    !mask = 0
    call nc_write(fnm_out,mask,         "m2",dim1="xc",dim2="yc",grid_mapping=mapping,units="none")
    call nc_write(fnm_out,mask(:,1)*0+2,"m2",dim1="xc",dim2="yc")
    call nc_write(fnm_out,mask(1,:)*0+3,"m2",dim1="xc",dim2="yc",start=(/10,1/),count=(/1,ny/))

    ! Write a double array
    call nc_write(fnm_out,lon,"lon",dim1="xc",dim2="yc",grid_mapping=mapping)
    call nc_write(fnm_out,lat,"lat",dim1="xc",dim2="yc",grid_mapping=mapping)

    ! Write 2D time slices
    do i = 1, 3
        call nc_write(fnm_out,mask*0+i,"m3",dim1="xc",dim2="yc",dim3="time",start=(/1,1,i/),&
                      grid_mapping=mapping,units="none")
        call nc_write(fnm_out,dble(mask*0+i+0.1*i),"m3d",dim1="xc",dim2="yc",dim3="time",start=(/1,1,i/),&
                      grid_mapping=mapping,units="none")
    end do 

    ! Write a 3D array 
    call nc_write(fnm_out,vx,"vx",dim1="xc",dim2="yc",dim3="kc",grid_mapping=mapping)

    ! Write a 4D array 
    call nc_write(fnm_out,vx4D,"vx4D",dim1="xc",dim2="yc",dim3="kc",dim4="time",grid_mapping=mapping)
    call nc_write(fnm_out,real(vx4D),"vx4Dr",dim1="xc",dim2="yc",dim3="kc",dim4="time",grid_mapping=mapping)

    ! Read a 4D array 
    call nc_read(fnm_out,vx4D,"vx4D")

    ! Write a logical 2D array
    masklogic = .FALSE.
    masklogic(20:30,20:30) = .TRUE.
    call nc_write(fnm_out,masklogic,"m2l",dim1="xc",dim2="yc",grid_mapping=mapping)

    ! Read a logical 2D array
    call nc_read(fnm_out,masklogic,"m2l")

    !! Testing for character arrays
    char2D(:,:) = trim(fnm_out)

    ! Write some strings
    call nc_write_char(fnm_out,"All the king's men!","test")
    call nc_write_char(fnm_out,char2D(1:5,1),"char1D")
    call nc_write_char(fnm_out,char2D(1:2,:),"char2D")
    
    ! Make sure I can read the strings back
    call nc_read_char(fnm_out,char2D(1,1),"char1D")
    write(*,*) "My var: ",trim(char2D(1,1))
    char2D(:,:) = ""
    call nc_read_char(fnm_out,char2D(1:5,1),"char1D")
    do i = 1,5
        write(*,*) trim(char2D(i,1))
    end do 

    write(*,*)
end program