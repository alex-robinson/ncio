module ncio

    use netcdf

    implicit none

    type ncvar
        character (len=256) :: name, long_name, standard_name, units
        character (len=256) :: dataset, level_desc
        character (len=256) :: axis, calendar, grid_mapping
        character (len=256), allocatable :: dims(:)
        integer, allocatable :: dlen(:)
        integer :: n, dimid, varid
        double precision :: add_offset, scale_factor, missing_value, FillValue
        double precision :: actual_range(2)
        logical :: missing_set, FillValue_set
        double precision, allocatable :: dim(:)
    end type

    private 
    public :: nc_create, nc_add_grid_mapping, nc_write_dim, &
              nc_write, nc_update_1d, nc_read

contains

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ c r e a t e
    ! Author     :  Alex Robinson
    ! Purpose    :  Create a new netcdf file with some global information
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_create(filename,title,institution)

        implicit none 

        character(len=*) :: filename
        character(len=*), optional :: title, institution

        type args_type
            character(len=1024) :: title, history, institution, references
            character(len=1024) :: Conventions
        end type 

        type(args_type) :: args 

        integer :: ncid

        args%title         = ""
        args%institution   = "" 

        if ( present(institution) )   args%institution   = trim(institution) 
        if ( present(title) )         args%title         = trim(title) 
        
        ! Additional global dataset definitions (later from parameter file)
        args%history     = "Dataset generated using ncio (version 2)."
        args%references  = "http://www.unidata.ucar.edu/netcdf/conventions.html"
        args%Conventions = "CF-1.6" 

        !args%institution = "Universidad Complutense de Madrid; Potsdam Institute for Climate Impact Research" 

        ! Create the new empty file and close it (necessary to avoid errors with dim vars)
        call nc_check( nf90_create(filename, nf90_clobber, ncid) )
        call nc_check( nf90_enddef(ncid) )
        call nc_check( nf90_close(ncid) )

        ! Open the file again and set for redefinition
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_check( nf90_redef(ncid) )

        ! Add dataset global attributes if available
        ! (For now, hard coded, later use parameter file)
        call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "title",       trim(args%title)) )
        call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "history",     trim(args%history)) )
        call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "institution", trim(args%institution)) )
        call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "references",  trim(args%references)) )
        call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "Conventions", trim(args%Conventions)) )
        
        ! End define mode and close the file.
        call nc_check( nf90_enddef(ncid) )
        call nc_check( nf90_close(ncid) )

        write(*,"(a,a10,i6)") "ncio:: nc_create:: "//trim(filename)
        
        return

    end subroutine nc_create

    subroutine nc_add_grid_mapping(filename,mapping,lambda0,phi0,x0,y0)

        implicit none 

        character(len=*) :: filename, mapping

        integer :: ncid, varid, stat
        double precision, optional :: lambda0, phi0, x0, y0

        integer, parameter :: noerr = NF90_NOERR

        ! Open the file, set for redefinition
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_check( nf90_redef(ncid) )
        
        ! Check if grid mapping has been defined in this file
        ! (if not, define it according to input arguments)
        stat = nf90_inq_varid(ncid, trim(mapping), varid)

        if ( stat .ne. noerr ) then
            ! Define the mapping variable as an integer with no dimensions,
            ! and include the grid mapping name
            call nc_check( nf90_def_var(ncid, trim(mapping), NF90_INT, varid) )
            call nc_check( nf90_put_att(ncid,varid, "grid_mapping_name", trim(mapping)) )

            ! Add grid attributes depending on grid_mapping type
            select case(trim(mapping))

                case("stereographic")
                    call nc_check( nf90_put_att(ncid,varid, "longitude_of_projection_origin", lambda0) )
                    call nc_check( nf90_put_att(ncid,varid, "latitude_of_projection_origin", phi0) )
                    call nc_check( nf90_put_att(ncid,varid, "scale_factor_at_projection_origin", 1.d0) )
                    call nc_check( nf90_put_att(ncid,varid, "false_easting",  x0) )
                    call nc_check( nf90_put_att(ncid,varid, "false_northing", y0) )

                case("polar_stereographic")
                    call nc_check( nf90_put_att(ncid,varid, "straight_vertical_longitude_from_pole", lambda0) )
                    call nc_check( nf90_put_att(ncid,varid, "latitude_of_projection_origin", phi0) )
                    call nc_check( nf90_put_att(ncid,varid, "scale_factor_at_projection_origin", 1.d0) )
                    call nc_check( nf90_put_att(ncid,varid, "false_easting",  x0) )
                    call nc_check( nf90_put_att(ncid,varid, "false_northing", y0) )

                case DEFAULT
                    ! Do nothing

            end select

            write(*,"(a,a)") "ncio:: nc_add_grid_mapping:: "//trim(filename)//" : ",trim(mapping)
        
        end if 

        ! Close the file
        call nc_check( nf90_close(ncid) )

        return 

    end subroutine nc_add_grid_mapping

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ w r i t e _ d i m
    ! Author     :  Alex Robinson
    ! Purpose    :  Write a coordinate var to a netcdf file
    !               and make a new file if needed
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_write_dim(filename,name,long_name,standard_name,units,axis,calendar, &
                            x,x0,nx,dx)

        implicit none

        integer :: ncid, i

        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar
        integer,  optional :: nx
        double precision, optional :: x0, dx, x(:)

        type(ncvar) :: v

        type args_type
            character(len=256) :: long_name, standard_name, units, axis
            character(len=1024) :: title, history, institution, references
            character(len=1024) :: Conventions
            character(len=256) :: calendar
        end type 

        type(args_type) :: args 
       
        double precision :: x0_tmp, dx_tmp

        !! First assign default values for optional arguments
        args%long_name     = trim(name) 
        args%standard_name = trim(name)
        args%units         = ""
        args%axis          = ""
        args%calendar      = "" 

        !! Now fill in values of arguments that are present
        if ( present(long_name) )     args%long_name     = trim(long_name)
        if ( present(standard_name) ) args%standard_name = trim(standard_name)
        if ( present(units) )         args%units         = trim(units)
        if ( present(axis) )          args%axis          = trim(axis)
        if ( present(calendar) )      args%calendar      = trim(calendar) 
        
        !! Initialize netcdf variable info
        call nc_init_v(v,name=trim(name),long_name=args%long_name, &
                       standard_name=args%standard_name, &
                       units=args%units,axis=args%axis,calendar=args%calendar)
        

        !! Clear the variable dim vector and store/generate appropriate values
        if (allocated(v%dim)) deallocate(v%dim)

        if (present(x)) then 
        ! Use x values given as argument
          
            v%n = size(x)
            allocate(v%dim(v%n))
            v%dim = x

        else if (present(nx) ) then  
        ! generate x values based on arguments or defaults
          
            allocate(v%dim(nx))
            v%n = nx

            x0_tmp = 0.d0
            dx_tmp = 1.d0

            if (present(x0)) x0_tmp = x0
            if (present(dx)) dx_tmp = dx

            ! Calculate each value (and round off to eliminate small errors)
            do i = 1, nx
                v%dim(i) = x0_tmp + (i-1)*dx_tmp
                v%dim(i) = nint(v%dim(i)*1d4)/1d4
            end do

        else
          
            write(*,*) "ncio:: nc_create:: error, no length given to define dimension var."
            stop
          
        end if

        ! Get the range from the x values
        v%actual_range = (/ minval(v%dim), maxval(v%dim) /)
        v%add_offset   = 0.d0
        v%scale_factor = 1.d0

        ! END VARIABLE SETUP

        ! Open the file, set for redefinition
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_check( nf90_redef(ncid) )

        !! Define the variable in the file
        if ( trim(v%name) .eq. "time" ) then
            call nc_check( nf90_def_dim(ncid, trim(v%name), NF90_UNLIMITED, v%dimid) )
        else
            call nc_check( nf90_def_dim(ncid, trim(v%name), v%n, v%dimid) )
        end if

        ! Assign attributes to coordinate variable.
        call nc_put_att(ncid, v, coord=.TRUE., double=.FALSE.)

        ! End define mode.
        call nc_check( nf90_enddef(ncid) )

        ! Put the variable's values in the file 
        call nc_check( nf90_put_var(ncid, v%varid, v%dim) )

        ! Close the file
        call nc_check( nf90_close(ncid) )

        write(*,"(a,a10,i6)") "ncio:: nc_write_dim:: "//trim(filename)//" : ",trim(v%name),size(v%dim)
        
        return

    end subroutine nc_write_dim 

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ w r i t e
    ! Author     :  Alex Robinson
    ! Purpose    :  Write a variable to a netcdf file
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_write(filename,name, &
                        v1D,v2D,v3D,v4D,v2Dm,dims, &
                        x0,y0,z0,t0,long_name,standard_name,grid_mapping,units,prec)

        implicit none

        integer :: ncid
        integer :: i, j, k, m

        character (len=*) :: filename, name
        integer :: ndims, dim_in

        character (len=*),   optional :: long_name, standard_name, grid_mapping, units, prec
        character (len=256), optional :: dims(:) 

        type args_type
            character(len=256) :: long_name, standard_name, grid_mapping, units, prec
            logical :: is_double
        end type 

        type(args_type) :: args 

        type(ncvar) :: v

        double precision, optional :: v1D(:), v2D(:,:), v3D(:,:,:), v4D(:,:,:,:)
        logical, optional :: V2Dm(:,:)
        integer, optional :: x0, y0, z0, t0
        double precision :: actual_range(2)
        integer :: stat

        real (4), allocatable, dimension(:,:,:,:) :: var4

        ! netCDF needed counters, array, and names of dims
        integer, allocatable :: dimids(:), start(:), count(:)

        ! Open the file in nowrite mode
        call nc_check( nf90_open(filename, nf90_nowrite, ncid) )

        ! Get var info
        args%long_name     = trim(name)
        args%standard_name = trim(name)
        args%grid_mapping  = ""
        args%units         = ""
        args%is_double     = .FALSE.

        if ( present(long_name) )     args%long_name     = trim(long_name)
        if ( present(standard_name) ) args%standard_name = trim(standard_name)
        if ( present(grid_mapping) )  args%grid_mapping  = trim(grid_mapping)
        if ( present(units) )         args%units         = trim(units)
        if ( present(prec) ) then
            if ( trim(prec) .eq. "double" ) args%is_double = .TRUE.
        end if

        call nc_get_att(ncid,v,name=trim(name),long_name=args%long_name, &
                        standard_name=args%standard_name,&
                        grid_mapping=args%grid_mapping,units=args%units)    

        ! Close the file
        call nc_check( nf90_close(ncid) )

        ! Get dimensions of actual subroutine argument
        dim_in = 2
        if (present(v1D)) dim_in = 1
        if (present(v3D)) dim_in = 3
        if (present(v4D)) dim_in = 4
            
        ! Determine size of nc variable dimensions and allocate
        if (present(dims)) then
            ndims = size(dims)
        else
            ndims = 0
            if (present(x0)) ndims = ndims + 1
            if (present(y0)) ndims = ndims + 1
            if (present(z0)) ndims = ndims + 1
            if (present(t0)) ndims = ndims + 1
        end if

        allocate( dimids(ndims), start(ndims), count(ndims) )
        if (allocated(v%dims)) deallocate(v%dims); allocate(v%dims(ndims))


        do i = 1, ndims

            ! Determine the name of the current dim
            if (present(dims)) then
                v%dims(i) = trim(dims(i))
            else
                if (i .eq. 1) then
                    if (present(x0)) then
                        v%dims(i) = "xc"
                    else if (present(y0)) then
                        v%dims(i) = "yc"
                    else if (present(z0)) then
                        v%dims(i) = "z"
                    else
                        v%dims(i) = "time"
                    end if
                else if (i .eq. 2) then
                    if (present(y0)) then
                        v%dims(i) = "yc"
                    else if (present(z0)) then
                        v%dims(i) = "z"
                    else
                        v%dims(i) = "time"
                    end if
                else if (i .eq. 3) then
                    if (present(z0)) then
                        v%dims(i) = "z"
                    else
                        v%dims(i) = "time"
                    end if
                else
                    v%dims(i) = "time"
                end if
            end if

        end do

        select case(dim_in)
            case(1)
                allocate(var4(size(v1D),1,1,1))
                var4(:,1,1,1) = v1D

            case(2)
                if (present(v2Dm)) then 
                    allocate(var4(size(v2Dm,1),size(v2Dm,2),1,1))
                    var4 = 0.d0 
                    where(v2Dm) var4(:,:,1,1) = 1.d0
                else
                    allocate(var4(size(v2D,1),size(v2D,2),1,1))
                    var4(:,:,1,1) = v2D
                end if

            case(3)
                allocate(var4(size(v3D,1),size(v3D,2),size(v3D,3),1))
                var4(:,:,:,1) = v3D
            
            case(4)
                allocate(var4(size(v4D,1),size(v4D,2),size(v4D,3),size(v4D,4)))
                var4 = v4D 

        end select

        ! For each dimension determine the start and count value
        start = 1; count = 1
        do i = 1, ndims
            select case(trim(v%dims(i)))
                case("x","xc","lon","longitude")
                    start(i) = x0
                case("y","yc","lat","latitude")
                    start(i) = y0
                case("t","time")
                    start(i) = t0
                case DEFAULT
                    start(i) = z0
            end select
          
            count(i) = size(var4,i)
        end do

        ! Reset or initialize the actual range of the variable
        actual_range = (/ minval(var4), maxval(var4) /)
        if (present(t0)) then
            if (t0 .ne. 1) then
                v%actual_range(1) = min(v%actual_range(1),actual_range(1))
                v%actual_range(2) = max(v%actual_range(2),actual_range(2))
            else
                v%actual_range = actual_range
            end if
        end if

        ! Modify the variable according to scale and offset
        if (v%missing_set) then
            where (var4 .ne. v%missing_value) var4 = (var4-v%add_offset)/v%scale_factor
        else    
            ! Apply the scalar and offset if available
            var4 = (var4-v%add_offset)/v%scale_factor
        end if

        ! Open the file
        call nc_check( nf90_open(filename, nf90_write, ncid) )

        ! Define / update the netCDF variable for the data.
        call nc_check( nf90_redef(ncid) )
        call nc_put_att(ncid, v, double=args%is_double)
        call nc_check( nf90_enddef(ncid) )

        ! Write the data to the netcdf file
        call nc_check( nf90_put_var(ncid, v%varid, var4,start,count) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        call nc_check( nf90_close(ncid) )

        return

    end subroutine nc_write

    !! Update a 1D variable (like time!)
    subroutine nc_update_1D(filename,name,ndat,x0)

        implicit none

        character (len=*) :: filename, name
        integer :: ndat, ncid, varid, stat
        double precision :: x0(:)

        integer, parameter :: noerr = NF90_NOERR

        call nc_check( nf90_open(filename, nf90_write, ncid) )

        ! Get the varid (should already exist since we are just updating
        stat = nf90_inq_varid(ncid, trim(name), varid)

        if ( stat .eq. noerr ) then

            ! Write the data to the netcdf file
            call nc_check( nf90_put_var(ncid, varid, x0, (/ndat/), (/size(x0)/) ) )

            ! Close the file. This causes netCDF to flush all buffers and make
            ! sure your data are really written to disk.
            call nc_check( nf90_close(ncid) )

        else
          
            write(*,*) "ncio:: nc_update_1D:: variable not found in:"//trim(filename)//" : ",trim(name)
            stop

        end if

        return

    end subroutine nc_update_1D

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ r e a d
    ! Author     :  Alex Robinson
    ! Purpose    :  Read a variable from a netcdf file
    !               (only one time step 'ndat' at a time)
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_read(filename,ndat,name,v1D,v2D,v3D,v4D,v2Dm,start,count,order)

        implicit none

        integer :: ncid, stat, ndim, ndat, dim_in, n1
        integer, optional    :: start(:), count(:), order

        character (len=*) :: filename, name
        type(ncvar) :: v

        double precision, optional :: v1D(:), v2D(:,:), v3D(:,:,:), v4D(:,:,:,:)
        logical, optional :: v2Dm(:,:)
        double precision, allocatable :: v2D_dble(:,:) 

        real (4), allocatable, dimension(:,:,:,:) :: var4

        double precision    :: tmp
        character (len=256) :: tmpstr

        integer, dimension(nf90_max_var_dims) :: dimIDs

        type args_type
            integer :: order
            integer, allocatable :: start(:), count(:)
        end type

        type(args_type) :: args 

        ! Loop indices
        integer :: lev, lat, lon, rec, i, j, k, nt

        ! Initializing
        v%name = name
        write(*,"(a,a,2x,$)") "ncio: read netcdf file: ", trim(filename)//" : "//trim(v%name)

        ! Open the file. 
        call nc_check( nf90_open(filename, nf90_nowrite, ncid) )

        ! == Desired var ==
        call nc_get_att(ncid,v)

        ! Get variable dimension
        ndim = size(v%dims)

        ! Arguments
        if ( allocated(args%start) ) deallocate(args%start)
        if ( allocated(args%count) ) deallocate(args%count)

        if (present(start)) then
            allocate(args%start(size(start)))
            args%start = start 
        else 
            allocate(args%start(ndim))
        end if 

        if (present(count)) then
            allocate(args%count(size(count)))
            args%count = count 
        else 
            allocate(args%count(ndim))
        end if 

        if (present(order)) then
            args%order = order 
        else 
            args%order = 1 
        end if 

        ! Allocated real4 array for reading in data
        if (allocated(var4)) deallocate(var4)
        select case(ndim)
          case(1)
            allocate(var4(v%dlen(1),1,1,1))
          case(2)
            allocate(var4(v%dlen(1),v%dlen(2),1,1))
          case(3)
            allocate(var4(v%dlen(1),v%dlen(2),v%dlen(3),1))
          case(4)
            allocate(var4(v%dlen(1),v%dlen(2),v%dlen(3),v%dlen(4)))
        end select 

        ! Assign the counting arrays for netcdf reading
        do i = 1, ndim
          args%start(i) = 1
          args%count(i) = size(var4,i)
        end do

        ! Assign which time index to start from
        args%start(ndim) = ndat
        !args%count(ndims) = nt

        ! Read the variable from the file
        call nc_check( nf90_get_var(ncid, v%varid, var4, args%start, args%count) )

        ! Close the file. This frees up any internal netCDF resources
        ! associated with the file.
        call nc_check( nf90_close(ncid) )

        if (v%missing_set) then
          where (var4 .ne. v%missing_value) var4 = var4*v%scale_factor + v%add_offset
        else    
          ! Apply the scalar and offset if available
          var4 = var4*v%scale_factor + v%add_offset
        end if

        ! Determine size of input variable dimensions
        dim_in = 2
        if (present(v1D)) dim_in = 1
        if (present(v3D)) dim_in = 3
        if (present(v4D)) dim_in = 4

        if (present(v2D))  allocate(V2D_dble(size(V2D,1),size(V2D,2)))
        if (present(v2Dm)) allocate(V2D_dble(size(V2Dm,1),size(V2Dm,2)))

        ! ### Copy real(4) to appropriate output var  ###
        select case(dim_in)
            case(1)
                n1 = min(size(var4,1),size(v1D))
                v1D(1:n1) = var4(1:n1,1,1,1)
            case(2)
                ! Move to 2d array from 4d array
                if (args%order .eq. 2) then
                    n1 = min(size(var4,2),size(v2D_dble,1))
                    do i = 1, size(var4,1)
                        v2D_dble(1:n1,i) = var4(i,1:n1,1,1)
                    end do
                else
                    n1 = min(size(var4,2),size(v2D_dble,2))
                    do i = 1, size(var4,1)
                      v2D_dble(i,1:n1) = var4(i,1:n1,1,1)
                    end do
                end if

            case(3)
                ! Move to 3d array from 4d array
                if (args%order .eq. 2) then
                    n1 = min(size(var4,3),size(v3D,1))
                    do i = 1, size(var4,1)  
                      do j = 1, size(var4,2) 
                          v3D(1:n1,j,i) = var4(i,j,1:n1,1)
                      end do
                    end do
                else
                    n1 = min(size(var4,3),size(v3D,3))
                    do i = 1, size(var4,1)  
                      do j = 1, size(var4,2) 
                          v3D(i,j,1:n1) = var4(i,j,1:n1,1)
                      end do
                    end do
                end if
            case(4)
                ! Move to 4d array from 4d array
                if (args%order .eq. 2) then
                    n1 = min(size(var4,4),size(v4D,1))
                    do i = 1, size(var4,1)  
                        do j = 1, size(var4,2)
                          do k = 1, size(var4,3)
                              v4D(1:n1,k,j,i) = var4(i,j,k,1:n1)
                          end do
                        end do
                    end do
                else
                    n1 = min(size(var4,4),size(v4D,4))
                    do i = 1, size(var4,1)  
                        do j = 1, size(var4,2)
                          do k = 1, size(var4,3)
                              v4D(i,j,k,1:n1) = var4(i,j,k,1:n1)
                          end do
                        end do
                    end do
                end if
        end select

        ! Store data in logical array if needed
        if (present(v2Dm)) then 
            V2Dm = .FALSE.
            where (V2D_dble == 1.d0) V2Dm = .TRUE. 
        else if (present(v2D)) then
            V2D = V2D_dble 
        end if 

        ! If we got this far, everything worked as expected. Yipee! 
        write(*,*) " ..done."

        return

    end subroutine nc_read


!! Private functions 

    ! Internal ncio function to read 1D array
    ! (should be called from nc_read)
    subroutine nc_read_1D(var,ndat,dimlen)

        implicit none 

        integer :: ndat, dimlen(:)
        double precision :: var(:)

        ! netCDF access variables
        integer :: ndims
        real (4), allocatable :: var4(:)
        integer, allocatable :: start(:), count(:)

        integer :: i 


        if (allocated(var4)) deallocate(var4)
        allocate(var4(dimlen(1)))

        ndims = size(dimlen)
        if (allocated(start)) deallocate(start)
        if (allocated(count)) deallocate(count)
        allocate( start(ndims), count(ndims) )

        ! Assign the counting arrays for netcdf reading
        do i = 1, ndims
          start(i) = 1
          count(i) = size(var4,i)
        end do

        ! Assign which time index to start from
        start(ndims) = ndat


        
        return

    end subroutine nc_read_1D




    !! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !! Subroutine :  c h e c k
    !! Author     :  Alex Robinson
    !! Purpose    :  Wrap a netcdf function to perform error checking
    !! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_check(status,stat)

        implicit none

        integer, intent ( in) :: status
        integer, intent (out), optional :: stat

        if(status /= nf90_noerr .and. (.not. present(stat)) ) then 
            write(*,*) "ncio:: error: "//trim(nf90_strerror(status))
            stop "stopped by ncio."
        end if

        if (present(stat)) then 
            stat = 0
            if(status /= nf90_noerr) stat = -1
        end if

        return

    end subroutine nc_check

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  c h e c k _ a t t
    ! Author     :  Alex Robinson
    ! Purpose    :  Wrap an attribute function to perform error checking
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    function nc_check_att(status)

        implicit none

        integer, intent (in) :: status
        integer :: nc_check_att
        integer, parameter :: noerr = NF90_NOERR

        nc_check_att = noerr

        if(status /= nf90_noerr) then 
            if (trim(nf90_strerror(status)) .eq. "NetCDF: Attribute not found") then
                nc_check_att = -1
            else
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end if

        return 

    end function nc_check_att
  
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  a t t r _ p r i n t
    ! Author     :  Alex Robinson
    ! Purpose    :  Print all major netcdf attributes
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_print_attr(v)

        implicit none

        type(ncvar) :: v

        write(*,*) 
        write(*,*) "  ncvar: ", trim(v%name)
        write(*,"(10x,a20,a1,2x,a)")      "long_name",":",     trim(v%long_name)
        write(*,"(10x,a20,a1,2x,a)")      "standard_name",":", trim(v%standard_name)
        write(*,"(10x,a20,a1,2x,a)")      "units",    ":",     trim(v%units)
        write(*,"(10x,a20,a1,2x,a)")      "dataset",":",       trim(v%dataset)
        write(*,"(10x,a20,a1,2x,a)")      "level_desc",":",    trim(v%level_desc)
        write(*,"(10x,a20,a1,2x,a)")      "axis",":",          trim(v%axis)
        write(*,"(10x,a20,a1,2x,2e12.4)") "actual_range",":",  v%actual_range
        write(*,"(10x,a20,a1,2x,e12.4)")  "add_offset",":",    v%add_offset
        write(*,"(10x,a20,a1,2x,e12.4)")  "scale_factor",":",  v%scale_factor
        write(*,"(10x,a20,a1,2x,e12.4)")  "missing_value",":", v%missing_value
        write(*,"(10x,a20,a1,2x,L2)")     "missing_set",":",   v%missing_set
        write(*,"(10x,a20,a1,2x,a)")      "grid_mapping",":",  trim(v%grid_mapping)
        write(*,*)
        return

    end subroutine nc_print_attr
    
    !! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !! Subroutine :  nc_init_v
    !! Author     :  Alex Robinson
    !! Purpose    :  Make some default initializations of netcdf dim vars
    !! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_init_v(v,name,long_name,standard_name,grid_mapping,units,axis,calendar)

        implicit none

        type(ncvar) :: v

        character(len=*), optional :: name
        character(len=*), optional :: long_name, standard_name
        character(len=*), optional :: grid_mapping, units, axis, calendar

        integer :: i

        ! Intialize all variable information with default values
        v%name          = ""
        v%long_name     = ""
        v%standard_name = ""
        v%units         = ""
        v%axis          = ""
        v%level_desc    = ""
        v%dataset       = ""
        v%calendar      = ""
        v%grid_mapping  = ""
        v%missing_value = -9999d0
        v%missing_set   = .TRUE.
        v%add_offset    = 0.d0
        v%scale_factor  = 1.d0
        v%FillValue     = v%missing_value
        v%FillValue_set = .FALSE.
        v%actual_range  = (/ 0.d0, 0.d0 /)

        ! If args are present, reassign these variables
        if ( present(name) )          v%name           = trim(name)
        if ( present(long_name) )     v%long_name      = trim(long_name)
        if ( present(standard_name) ) v%standard_name  = trim(standard_name)
        if ( present(grid_mapping) )  v%grid_mapping   = trim(grid_mapping)
        if ( present(units) )         v%units          = trim(units)
        if ( present(axis) )          v%axis           = trim(axis)
        if ( present(calendar) .and. trim(name) == "time" )  &
                                      v%calendar       = trim(calendar)

        ! Update some variable info based on further default cases
        ! (Currently overrides input arguments to comply with CF-1.6)
        select case(trim(name))

            case("x","xc")
                v%long_name     = "x-coordinate in Cartesian system"
                v%standard_name = "projection_x_coordinate"
                v%units = "kilometers"
                v%axis  = "X"
            case("y","yc")
                v%long_name     = "y-coordinate in Cartesian system"
                v%standard_name = "projection_y_coordinate"
                v%units = "kilometers"
                v%axis  = "Y"
            case("z","lev")
                v%units = "meters"
                v%axis  = "Z"
            case("kc","kt","kr")
                v%axis  = "Z"
            case("time")
                v%units = "years"
                v%axis  = "T"
            case("lon","longitude")
                v%long_name = "longitude"
                v%standard_name = "longitude"
                v%units     = "degrees_east"
            case("lat","latitude")
                v%long_name = "latitude"
                v%standard_name = "latitude"
                v%units     = "degrees_north"
            case default
                ! Do nothing

        end select

        ! Additionally make sure time dimension loosely follows CF conventions
        if (trim(v%name) .eq. "time") v%units = trim(v%units)//" since 0-0-0"

        return

    end subroutine nc_init_v

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  p u t _ a t t
    ! Author     :  Alex Robinson
    ! Purpose    :  Put an attribute into netcdf file for a given variable
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_put_att(ncid, v, coord, double)

        implicit none

        integer :: i, ncid, stat, ndims

        integer, allocatable :: dimids(:)
        logical, optional :: coord, double
        type(ncvar) :: v
        logical :: is_double

        integer, parameter :: noerr = NF90_NOERR

        ndims = size(v%dims)

        is_double = .TRUE.
        if ( present(double) ) is_double = double

        ! Proceed as normal
          
        ! Check if variable already exists - if so, gets the varid
        stat = nf90_inq_varid(ncid, trim(v%name), v%varid)
          
        ! Define the variable if it doesn't exist
        if ( stat .ne. noerr ) then
          
            if ( present(coord) ) then

                ! Define the coordinate variable
                if ( is_double ) then
                    call nc_check( nf90_def_var(ncid, v%name, NF90_DOUBLE, v%dimid, v%varid) )
                else
                    call nc_check( nf90_def_var(ncid, v%name, NF90_REAL, v%dimid, v%varid) )
                end if
            else

                ! Get the dimension ids for the variable to be defined
                allocate(dimids(ndims))
                do i = 1, ndims
                    call nc_check ( nf90_inq_varid(ncid, trim(v%dims(i)), dimids(i)) )
                end do

                ! Define the variable
                if ( is_double ) then
                    call nc_check( nf90_def_var(ncid, trim(v%name), NF90_DOUBLE, dimids, v%varid) )
                else
                    call nc_check( nf90_def_var(ncid, trim(v%name), NF90_REAL, dimids, v%varid) )
                end if

            end if

            call nc_check( nf90_put_att(ncid, v%varid, "long_name",     trim(v%long_name)) )
            call nc_check( nf90_put_att(ncid, v%varid, "standard_name", trim(v%standard_name)) )

            if (trim(v%units) .ne. "") then
                call nc_check( nf90_put_att(ncid, v%varid, "units", trim(v%units) ) )
            end if

            if (trim(v%axis) .ne. "") then
                call nc_check( nf90_put_att(ncid, v%varid, "axis", trim(v%axis) ) )
            end if

            if (trim(v%calendar) .ne. "") then
                call nc_check( nf90_put_att(ncid, v%varid, "calendar", trim(v%calendar) ) )
            end if

            if (trim(v%grid_mapping) .ne. "") then
                call nc_check( nf90_put_att(ncid, v%varid, "grid_mapping", trim(v%grid_mapping) ) )
            end if

            if (trim(v%dataset) .ne. "") then
                call nc_check( nf90_put_att(ncid, v%varid, "dataset", trim(v%dataset) ) )
            end if
            if (trim(v%level_desc) .ne. "") then
                call nc_check( nf90_put_att(ncid, v%varid, "level_desc", trim(v%level_desc) ) )
            end if

            call nc_check( nf90_put_att(ncid, v%varid, "scale_factor", v%scale_factor) )
            call nc_check( nf90_put_att(ncid, v%varid, "add_offset", v%add_offset) )

            if (v%missing_set) then
                call nc_check( nf90_put_att(ncid, v%varid, "missing_value", v%missing_value) )
            end if

            if (v%FillValue_set) then
                call nc_check( nf90_put_att(ncid, v%varid, "FillValue", v%FillValue) )
            end if

        end if

        ! Always update the actual range (whether new or not)
        if (v%actual_range(1) .ne. 0.d0 .and. v%actual_range(2) .ne. 0.d0) then
            call nc_check( nf90_put_att(ncid, v%varid, "actual_range", v%actual_range) )
        end if

        return

    end subroutine nc_put_att
  
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  g e t _ a t t
    ! Author     :  Alex Robinson
    ! Purpose    :  Get attributes from a netcdf file for a given variable
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_get_att(ncid, v, name, long_name, standard_name, grid_mapping, units)

        implicit none

        integer :: ncid, stat, i
        double precision :: tmp, tmp2(2)
        character(len=256) :: tmpstr
        character(len=*), optional :: name, long_name, standard_name, grid_mapping, units
        type(ncvar) :: v

        integer :: ndims
        integer, allocatable :: dimids(:)

        type args_type
            character(len=256) :: long_name, standard_name, grid_mapping, units
        end type 

        type(args_type) :: args 

        integer, parameter :: noerr = NF90_NOERR

        ! Get var info
        args%long_name     = trim(name)
        args%standard_name = trim(name)
        args%grid_mapping  = ""
        args%units         = ""

        if ( present(long_name) )     args%long_name     = trim(long_name)
        if ( present(standard_name) ) args%standard_name = trim(standard_name)
        if ( present(grid_mapping) )  args%grid_mapping  = trim(grid_mapping)
        if ( present(units) )         args%units         = trim(units)

        ! Initialize the variable
        if (present(name)) v%name = trim(name)
        
        call nc_init_v(v,name=trim(v%name),long_name=args%long_name, &
                       standard_name=args%standard_name, &
                       grid_mapping=args%grid_mapping,units=args%units)

        ! Get the current variable's id, if it exists
        stat = nf90_inq_varid(ncid, trim(v%name), v%varid)

        ! If variable exists, get attributes from file instead
        if ( stat .eq. noerr ) then
          
            ! Get number of dimensions and dimids
            call nc_check ( nf90_inquire_variable(ncid, v%varid, ndims=ndims) )
            if (allocated(dimids)) deallocate(dimids); allocate(dimids(ndims))
            call nc_check ( nf90_inquire_variable(ncid, v%varid, dimids=dimids) )

            ! Re-allocate dimnames for current variable
            if (allocated(v%dims)) deallocate(v%dims); allocate(v%dims(ndims))
            if (allocated(v%dlen)) deallocate(v%dlen); allocate(v%dlen(ndims))

            ! Loop over dimensions and get the dimension names
            do i = 1, ndims
                call nc_check ( nf90_inquire_dimension(ncid,dimids(i),name=v%dims(i),len=v%dlen(i)) )
            end do

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "units", tmpstr) )
            if (stat .eq. noerr) v%units = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "long_name", tmpstr) )
            if (stat .eq. noerr) v%long_name = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "standard_name", tmpstr) )
            if (stat .eq. noerr) v%standard_name = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "axis", tmpstr) )
            if (stat .eq. noerr) v%axis = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "calendar", tmpstr) )
            if (stat .eq. noerr) v%calendar = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "grid_mapping", tmpstr) )
            if (stat .eq. noerr) v%grid_mapping = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "dataset", tmpstr) )
            if (stat .eq. noerr) v%dataset = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "level_desc", tmpstr) )
            if (stat .eq. noerr) v%level_desc = trim(tmpstr)

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "actual_range", tmp2) )
            if (stat .eq. noerr) v%actual_range = tmp2

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "scale_factor", tmp) )
            if (stat .eq. noerr) v%scale_factor = tmp

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "add_offset", tmp) )
            if (stat .eq. noerr) v%add_offset = tmp

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "missing_value", tmp) )
            if (stat .eq. noerr) then
                v%missing_value = tmp
                v%missing_set = .TRUE.
            end if

            stat = nc_check_att( nf90_get_att(ncid, v%varid, "FillValue", tmp) )
            if (stat .eq. noerr) then
                v%FillValue = tmp
                v%FillValue_set = .TRUE.
            end if
          
        end if

        return

    end subroutine nc_get_att
  


end module ncio