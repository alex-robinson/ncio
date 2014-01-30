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

    integer :: tmp(5) 
    double precision :: tmpd 

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
    call nc_read(fnm_in,"lon",lon)
    call nc_read(fnm_in,"lat",lat)
    call nc_read(fnm_in,"mask",mask)

    fnm_out = "out_ncio.nc"
    mapping = "stereographic"

    ! Create the netcdf file and the dimension variables
    call nc_create(fnm_out)
    call nc_write_global(fnm_out,"title","Greenland simulation")
    call nc_write_global(fnm_out,"institution", &
                         "Universidad Complutense de Madrid; Potsdam Institute for Climate Impact Research")
    
    call nc_write_dim(fnm_out,"xc",x=-800.d0, dx=20d0,nx=nx,units="kilometers")
    call nc_write_dim(fnm_out,"yc",x=-3400.d0,dx=20d0,nx=ny,units="kilometers")
    call nc_write_dim(fnm_out,"time",x=(/ 0.d0,5.d0,100.d0 /),units="years",calendar="360_day")
    call nc_write_dim(fnm_out,"parameter",x=1,units="none")
    call nc_write_dim(fnm_out,"kc",x=1,nx=nk,units="none")

    call nc_write_map(fnm_out,mapping,lambda=-39.d0,phi=90.d0,x_e=0.d0,y_n=0.d0)

    ! Writing a parameter value
    call nc_write(fnm_out,"p1",(/15/),dim1="parameter")
    call nc_write(fnm_out,"p1",20,dim1="parameter")

    ! Write some integers with missing data 
    call nc_write(fnm_out,"test1",(/1,2,3,999,5/),dim1="kc",missing_value=999)
    call nc_read(fnm_out,"test1",tmp,missing_value=-99)
    write(*,*) "test1 min/max: ",minval(tmp),maxval(tmp)
    
    call nc_read(fnm_out,"p1",mask(15,15))
    write(*,*) "mask(15,15)=",mask(15,15)

    ! Update time 
    call nc_write(fnm_out,"time",(/15.d0/),dim1="time",start=(/2/))

    ! Writing a 2D mask and some slices
    !mask = 0
    call nc_write(fnm_out,"m2",mask,         dim1="xc",dim2="yc",grid_mapping=mapping,units="none")
    call nc_write(fnm_out,"m2",mask(:,1)*0+2,dim1="xc",dim2="yc")
    call nc_write(fnm_out,"m2",mask(1,:)*0+3,dim1="xc",dim2="yc",start=(/10,1/),count=(/1,ny/))

    ! Write a double array
    call nc_write(fnm_out,"lon",lon,dim1="xc",dim2="yc",grid_mapping=mapping)
    call nc_write(fnm_out,"lat",lat,dim1="xc",dim2="yc",grid_mapping=mapping)

    ! Write 2D time slices
    do i = 1, 3
        call nc_write(fnm_out,"m3",mask*0+i,dim1="xc",dim2="yc",dim3="time",start=(/1,1,i/),&
                      grid_mapping=mapping,units="none")
        call nc_write(fnm_out,"m3d",dble(mask*0+i+0.1*i),dim1="xc",dim2="yc",dim3="time",start=(/1,1,i/),&
                      grid_mapping=mapping,units="none")
    end do 

    ! Write a 3D array
    vx(:,:,1) = 1.d0 
    vx(:,:,2) = 2.d0  
    call nc_write(fnm_out,"vx",vx,dim1="xc",dim2="yc",dim3="kc",grid_mapping=mapping)
    vx(:,:,3) = 3.d0
    call nc_write(fnm_out,"vx",vx,dims=["xc","yc","kc"],grid_mapping=mapping)

    write(*,*) "Testing new interface!"
!     call nc4_write_internal(fnm_out,"vx",pack(vx,.TRUE.),size_in=ubound(vx),actual_range=[minval(vx),maxval(vx)], &
!                              dim1="xc",dim2="yc",dim3="kc",grid_mapping=mapping)
    write(*,*) "It worked?"

    ! Write a 4D array 
    call nc_write(fnm_out,"vx4D",vx4D,dim1="xc",dim2="yc",dim3="kc",dim4="time",grid_mapping=mapping)
    call nc_write(fnm_out,"vx4Dr",real(vx4D),dim1="xc",dim2="yc",dim3="kc",dim4="time",grid_mapping=mapping)

    ! Read a 4D array 
    call nc_read(fnm_out,"vx4D",vx4D)

    ! Write a logical 2D array
    masklogic = .FALSE.
    masklogic(20:30,20:30) = .TRUE.
    call nc_write(fnm_out,"m2l",masklogic,dim1="xc",dim2="yc",grid_mapping=mapping)

    ! Read a logical 2D array
    call nc_read(fnm_out,"m2l",masklogic)

    !! Testing for character arrays
    char2D(:,:) = trim(fnm_out)

    ! Write some strings
    call nc_write(fnm_out,"All the king's men!","test")
    call nc_write(fnm_out,"char1D",char2D(1:5,1))
    call nc_write(fnm_out,"char2D",char2D(1:2,:))
    
    ! Make sure I can read the strings back
    call nc_read(fnm_out,"char1D",char2D(1,1))
    write(*,*) "My var: ",trim(char2D(1,1))
    char2D(:,:) = ""
    call nc_read(fnm_out,"char1D",char2D(1:5,1))
    do i = 1,5
        write(*,*) trim(char2D(i,1))
    end do 

    write(*,*)
end program
