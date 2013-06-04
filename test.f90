
!! TO COMPILE : 
!! gfortran -o a.out -fcheck=all -I/opt/local/include ncio2.f90 test.f90 -L/opt/local/lib -lnetcdff -lnetcdf
!! or
!! ifort -g -L/home/robinson/apps/netcdf/netcdf/lib -lnetcdf -I/home/robinson/apps/netcdf/netcdf/include -o a.out ncio2.f90 test.f90

program test

    use ncio 

    implicit none

    integer :: t

    character(len=256) :: fnm, fnm_in, fnm_out, mapping, dims(3)

    integer :: nx, ny 
    double precision, allocatable, dimension(:,:) :: lon,lat,zs
    logical, allocatable, dimension(:,:) :: mask 

    integer :: i, j 

    nx = 76
    ny = 141 
    allocate( lon(nx,ny))
    allocate( lat(nx,ny))
    allocate(  zs(nx,ny))
    allocate(mask(nx,ny))

    i = 40
    j = 50

    !=========== NETCDF TESTING ===========
    fnm_in  = "topo.20km.nc"
    fnm_out = "out.nc"

    ! Read input fields
    call nc_read(fnm_in,1,"lon",v2D=lon)
    call nc_read(fnm_in,1,"lat",v2D=lat)
    call nc_read(fnm_in,1,"zs", v2D=zs)

    write(*,*) "zs(i,j) =",zs(i,j)

    mask = .FALSE.
    where(zs .gt. 0.d0) mask = .TRUE. 

    mapping = "stereographic"

    ! Create the netcdf file and the dimension variables
    call nc_create(fnm_out,title="Greenland simulation", &
                   institution="Universidad Complutense de Madrid; Potsdam Institute for Climate Impact Research")
    
    call nc_write_dim(fnm_out,"xc",x0=-800.d0, dx=20d0,nx=nx,units="kilometers")
    call nc_write_dim(fnm_out,"yc",x0=-3400.d0,dx=20d0,nx=ny,units="kilometers")
    call nc_write_dim(fnm_out,"time",x=(/ 0.d0,5.d0,100.d0 /),units="years",calendar="360_day")
    
    call nc_add_grid_mapping(fnm_out,mapping,lambda0=-39.d0,phi0=90.d0,x0=0.d0,y0=0.d0)

    dims(1) = "xc"
    dims(2) = "yc"
    dims(3) = "time"

    call nc_write(fnm_out,"lon",v2D=lon,dims=dims(1:2),x0=1,y0=1,grid_mapping=mapping)
    call nc_write(fnm_out,"lat",v2D=lat,dims=dims(1:2),x0=1,y0=1,grid_mapping=mapping)
    call nc_write(fnm_out,"mask",v2Dm=mask,dims=dims(1:2),x0=1,y0=1,grid_mapping=mapping)

    call nc_update_1D(fnm_out,"time",1,(/101.d0/))
    call nc_write(fnm_out,"zs", v2D=zs, dims=dims,x0=1,y0=1,t0=1,grid_mapping=mapping, &
                  units="m",long_name="Surface elevation")
    call nc_update_1D(fnm_out,"time",2,(/102.d0/))
    call nc_write(fnm_out,"zs", v2D=zs, dims=dims,x0=1,y0=1,t0=2)
    call nc_update_1D(fnm_out,"time",3,(/103.d0/))
    call nc_write(fnm_out,"zs", v2D=zs, dims=dims,x0=1,y0=1,t0=3)
    
    !======================================

    ! Test reading the logical mask 
    mask = .FALSE.
    call nc_read(fnm_out,ndat=1,name="mask",v2Dm=mask)
    write(*,*) "Total mask points =",count(mask)

end program