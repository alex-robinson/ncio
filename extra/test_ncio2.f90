program test

    use ncio 

    implicit none

    integer :: t

    character(len=256) :: fnm, fnm_in, fnm_out, mapping

    integer :: nx, ny, nk
    double precision, allocatable, dimension(:,:) :: lon,lat,zs, dist
    integer, allocatable, dimension(:,:) :: mask 
    double precision, allocatable, dimension(:,:,:) :: vx
    double precision, allocatable, dimension(:,:,:,:) :: vx4D 
    double precision, allocatable, dimension(:,:,:,:,:,:) :: vx6D 
    logical, allocatable, dimension(:,:) :: masklogic
    character(len=256), allocatable, dimension(:,:) :: char2D
    character(len=256) :: testchar

    integer :: tmp(5) 
    double precision :: tmpd 

    integer :: distdim(3)

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

    allocate(vx6D(nx,ny,nk,1,2,3))

    ! Write a map array like in coordinates package
    distdim = [28,28,10]
    allocate(dist(distdim(1)*distdim(2),distdim(3)))
    dist = 0.d0
    do i =1, distdim(3)
        dist(:,i) = dble(i)
    end do

    ! Load data
    fnm_in = "extra/topo.20km.nc"
    call nc_read(fnm_in,"lon",lon)
    call nc_read(fnm_in,"lat",lat)
    call nc_read(fnm_in,"mask",mask)

    fnm_out = "extra/out_ncio2.nc"
    mapping = "stereographic"

    ! Create the netcdf file and the dimension variables
    call nc_create(fnm_out)
    call nc_write_attr(fnm_out,"title","Greenland simulation")
    call nc_write_attr(fnm_out,"institution", &
                       "Universidad Complutense de Madrid; Potsdam Institute for Climate Impact Research")

    call nc_read_attr(fnm_out, "institution", testchar)
    write(*,*) "Institution: ", trim(testchar)
    
    call nc_write_dim(fnm_out,"xc",x=-800.d0, dx=20d0,nx=nx,units="kilometers")
    call nc_write_dim(fnm_out,"yc",x=-3400.d0,dx=20d0,nx=ny,units="kilometers")
    call nc_write_dim(fnm_out,"time",x=[0.d0,5.d0,100.d0],units="years",calendar="360_day", unlimited=.TRUE.)
    call nc_write_dim(fnm_out,"parameter",x=1,units="none")
    call nc_write_dim(fnm_out,"kc",x=1,nx=nk,units="none")
    call nc_write_dim(fnm_out,"d4",x=1,nx=1,units="none")
    call nc_write_dim(fnm_out,"d5",x=1,nx=2,units="none")
    call nc_write_dim(fnm_out,"d6",x=1,nx=3,units="none")

    call nc_write_dim(fnm_out,"xcd",x=1,nx=distdim(1),units="none")
    call nc_write_dim(fnm_out,"ycd",x=1,nx=distdim(2),units="none")
    call nc_write_dim(fnm_out,"zcd",x=1,nx=distdim(3),units="none")
    call nc_write_dim(fnm_out,"xcd1",x=1,nx=distdim(1)*distdim(2),units="none")

    call nc_write_map(fnm_out,mapping,lambda=-39.d0,phi=90.d0,x_e=0.d0,y_n=0.d0) 

    call nc_write(fnm_out,"dist",dist,dim1="xcd",dim2="ycd",dim3="zcd") !,start=[1,1,1],count=[28,28,10])


    ! Writing a parameter value
    call nc_write(fnm_out,"p1",[15],dim1="parameter")
    call nc_write(fnm_out,"p1",20,dim1="parameter")

    ! Write some integers with missing data 
    call nc_write(fnm_out,"test1",[1,2,3,999,5],dim1="kc",missing_value=999)
    call nc_read(fnm_out,"test1",tmp,missing_value=-99)
    write(*,*) "test1 min/max: ",minval(tmp),maxval(tmp)
    
    call nc_read(fnm_out,"p1",mask(15,15))
    write(*,*) "mask(15,15)=",mask(15,15)

    ! Update time 
    call nc_write(fnm_out,"time",[15.d0],dim1="time",start=[2])

    write (*,*) "TIME DIMENSION SIZE", nc_size(fnm_out, "time")

    ! Writing a 2D mask and some slices
    !mask = 0
    call nc_write(fnm_out,"m2",mask,         dim1="xc",dim2="yc",grid_mapping=mapping,units="none")
    call nc_write(fnm_out,"m2",mask(:,1)*0+2,dim1="xc",dim2="yc")
    call nc_write(fnm_out,"m2",mask(1,:)*0+3,dim1="xc",dim2="yc",start=[10,1],count=[1,ny])

    ! Write some non-standard variable attribute
    call nc_write_attr(fnm_out, "m2", "desc", "This is the mask")
    call nc_read_attr(fnm_out, "m2", "desc", testchar)
    write(*,*) "m2 desc: ", trim(testchar)

    ! Write a double array
    call nc_write(fnm_out,"lon",lon,dim1="xc",dim2="yc",grid_mapping=mapping)
    call nc_write(fnm_out,"lat",lat,dim1="xc",dim2="yc",grid_mapping=mapping)

    vx(:,:,1) = 1.d0 
    vx(:,:,2) = 2.d0  
    vx(:,:,3) = 3.d0
    vx(:,:,4) = -1.d0

    ! Write 2D time slices
    do i = 1, 3
        call nc_write(fnm_out,"m3",mask*0+i,dim1="xc",dim2="yc",dim3="time",start=[1,1,i],&
                      grid_mapping=mapping,units="none")
!         call nc_write(fnm_out,"m3d",mask*0+i+0.1*dble(i),dim1="xc",dim2="yc",dim3="time",start=[1,1,i],&
!                       grid_mapping=mapping,units="none")
        call nc_write(fnm_out,"m3d",vx(:,:,4),dim1="xc",dim2="yc",dim3="time",start=[1,1,i],&
                      grid_mapping=mapping,units="none")
    end do 

    ! Write a 3D array
    call nc_write(fnm_out,"vx",vx,dim1="xc",dim2="yc",dim3="kc",grid_mapping=mapping)
    vx(:,:,3) = 6.d0
    call nc_write(fnm_out,"vx",vx,dims=["xc","yc","kc"],grid_mapping=mapping)

    ! Write a 4D array 
    call nc_write(fnm_out,"vx4D",vx4D,dim1="xc",dim2="yc",dim3="kc",dim4="time",grid_mapping=mapping)
    call nc_write(fnm_out,"vx4Dr",real(vx4D),dim1="xc",dim2="yc",dim3="kc",dim4="time",grid_mapping=mapping)

    ! Read a 4D array 
    call nc_read(fnm_out,"vx4D",vx4D)

    ! Write a 6D array 
    vx6D = 66.d0
    write(*,*) "Writing 6D!"
    call nc_write(fnm_out,"vx6D",vx6D,dim1="xc",dim2="yc",dim3="kc",dim4="d4",dim5="d5",dim6="d6", &
                  grid_mapping=mapping)

    write(*,*) "Reading 6D!"
    call nc_read(fnm_out,"vx6D",vx6D)
    
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

    ! Write attributes on recognized standard dimensions
    call nc_write_attr_std_dim(fnm_out)

    write(*,*)
end program
