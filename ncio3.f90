module ncio

    use netcdf

    implicit none

    double precision, parameter :: NCIO_VERSION = 3.0d0

    integer, parameter :: NC_STRLEN = 256 
    integer, parameter :: NC_STRLEN_MAX = 10000
    character(len=2)   :: NC_STR_SEP = "; "

    character(len=NC_STRLEN), parameter :: NC_CHARDIM = "strlen"

    type ncvar
        character (len=NC_STRLEN) :: name, long_name, standard_name, units
        character (len=NC_STRLEN) :: dataset, level_desc
        character (len=NC_STRLEN) :: axis, calendar, grid_mapping
        character (len=NC_STRLEN) :: xtype, dims_in(4)
        character (len=NC_STRLEN), allocatable :: dims(:)
        integer, allocatable :: dlen(:), start(:), count(:)
        integer :: n, dimid, varid, ndims_in
        double precision :: add_offset, scale_factor, missing_value, FillValue
        double precision :: actual_range(2)
        logical :: missing_set, FillValue_set
        double precision, allocatable :: dim(:)
        logical :: coord
    end type

    interface nc_write
        module procedure    nc_write_int_1D, nc_write_int_2D, &
                            nc_write_int_3D , nc_write_int_4D  
        module procedure    nc_write_double_1D, nc_write_double_2D, &
                            nc_write_double_3D, nc_write_double_4D
        module procedure    nc_write_float_1D, nc_write_float_2D, &
                            nc_write_float_3D, nc_write_float_4D
        module procedure    nc_write_logical_1D, nc_write_logical_2D, &
                            nc_write_logical_3D, nc_write_logical_4D
    end interface  

    interface nc_read 
        module procedure    nc_read_int_1D, nc_read_int_2D, &
                            nc_read_int_3D, nc_read_int_4D
        module procedure    nc_read_double_1D, nc_read_double_2D, &
                            nc_read_double_3D, nc_read_double_4D
        module procedure    nc_read_float_1D, nc_read_float_2D, &
                            nc_read_float_3D, nc_read_float_4D 
        module procedure    nc_read_logical_1D, nc_read_logical_2D, &
                            nc_read_logical_3D, nc_read_logical_4D                   
    end interface 

    ! Character writing and reading are given a separate interface,  
    ! since they do not follow the conventions of the other data types
    interface nc_write_char
        module procedure    nc_write_internal_char
        module procedure    nc_write_char_1D
        module procedure    nc_write_char_2D, nc_write_char_3D, nc_write_char_4D   ! Dummy procedures
    end interface

    interface nc_read_char 
        module procedure    nc_read_internal_char
        module procedure    nc_read_char_1D
    end interface

    private 
    public :: nc_create, nc_write_global, nc_write_map, nc_write_dim
    public :: nc_write, nc_read, nc_write_char, nc_read_char

contains

    !! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !! Subroutine :  nc_v_init
    !! Author     :  Alex Robinson
    !! Purpose    :  Make some default initializations of netcdf dim vars
    !! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_v_init(v,name,xtype,ndims_in,long_name,standard_name, &
                         grid_mapping,units,axis,calendar,coord)

        implicit none

        type(ncvar) :: v

        character(len=*) :: name
        character(len=*), optional :: xtype
        integer,          optional :: ndims_in 
        character(len=*), optional :: long_name, standard_name
        character(len=*), optional :: grid_mapping, units, axis, calendar
        logical, optional :: coord 

        integer :: i

        ! Intialize all variable information with default values
        v%name          = trim(name)
        v%long_name     = ""
        v%standard_name = ""
        v%units         = ""
        v%axis          = ""
        v%level_desc    = ""
        v%dataset       = ""
        v%calendar      = ""
        v%add_offset    = 0.d0
        v%scale_factor  = 1.d0
        v%actual_range  = (/ 0.d0, 0.d0 /)
        v%missing_set   = .TRUE.
        v%missing_value = -9999d0
        v%FillValue     = v%missing_value
        v%FillValue_set = .FALSE.

        v%xtype = "NF90_DOUBLE"
        v%coord = .FALSE. 

        v%grid_mapping  = ""

        ! If args are present, reassign these variables        
        if ( present(long_name) )     v%long_name      = trim(long_name)
        if ( present(standard_name) ) v%standard_name  = trim(standard_name)
        if ( present(grid_mapping) )  v%grid_mapping   = trim(grid_mapping)
        if ( present(units) )         v%units          = trim(units)
        if ( present(axis) )          v%axis           = trim(axis)
        if ( present(calendar) .and. trim(name) == "time" )  &
                                      v%calendar       = trim(calendar)

        if ( present(coord)) v%coord = coord 
        if ( present(xtype)) v%xtype = trim(xtype)

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

        ! Deallocate all arrays
        if (allocated(v%dim)) deallocate(v%dim)
        if (allocated(v%dims)) deallocate(v%dims)
        if (allocated(v%dlen)) deallocate(v%dlen)

        do i = 1, 4
            v%dims_in(i) = ""
        end do 
        v%ndims_in = 0 
        if (present(ndims_in)) v%ndims_in = ndims_in 

        return

    end subroutine nc_v_init

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
        write(*,*) "  ncvar: ", trim(v%name), trim(v%xtype)
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

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  p u t _ a t t
    ! Author     :  Alex Robinson
    ! Purpose    :  Put a set of attribute into netcdf file for a given variable
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_put_att(ncid, v)

        implicit none

        integer :: i, ncid, stat, ndims

        integer, allocatable :: dimids(:)
        type(ncvar) :: v

        integer, parameter :: noerr = NF90_NOERR

        ndims = size(v%dims)

        ! Check if variable already exists - if so, gets the varid
        stat = nf90_inq_varid(ncid, trim(v%name), v%varid)

        ! Define the variable if it doesn't exist
        if ( stat .ne. noerr ) then
            
            ! Check if it's a dimension (coordinate) variable or a data variable
            ! Get the dimension ids for the variable to be defined
            if ( v%coord ) then    
                ! This is a coordinate variable (ie, a dimension defintion)
                ! Only one dimid needed (that of current variable)
                allocate(dimids(1))
                dimids(1) = v%dimid 
            else 
                ! This is a data variable
                ! Determine ids of dimensions
                allocate(dimids(ndims))
                do i = 1, ndims
                    call nc_check ( nf90_inq_varid(ncid, trim(v%dims(i)), dimids(i)) )
                end do
            end if

            ! Define the variable
            select case(trim(v%xtype))
                case("NF90_INT")
                    call nc_check( nf90_def_var(ncid,name=trim(v%name),xtype=NF90_INT,dimids=dimids,varid=v%varid) )
                case("NF90_FLOAT")
                    call nc_check( nf90_def_var(ncid,name=trim(v%name),xtype=NF90_FLOAT,dimids=dimids,varid=v%varid) )
                case("NF90_DOUBLE")
                    call nc_check( nf90_def_var(ncid,name=trim(v%name),xtype=NF90_DOUBLE,dimids=dimids,varid=v%varid) )
                case("NF90_CHAR")
                    call nc_check( nf90_def_var(ncid,name=trim(v%name),xtype=NF90_CHAR,dimids=dimids,varid=v%varid) )
                case DEFAULT
                    write(*,*) "nc_put_att:: Error, wrong xtype defined:"//trim(v%xtype)
                    stop 
            end select 

            if (trim(v%xtype) .ne. "NF90_CHAR") then
                    
                    if (v%scale_factor .ne. 1.d0 .and. v%add_offset .ne. 0.d0) then 
                        call nc_check( nf90_put_att(ncid, v%varid, "scale_factor", v%scale_factor) )
                        call nc_check( nf90_put_att(ncid, v%varid, "add_offset",   v%add_offset) )
                    end if 

                    if (v%missing_set) &
                        call nc_check( nf90_put_att(ncid, v%varid, "missing_value", v%missing_value) )
                    if (v%FillValue_set) &
                        call nc_check( nf90_put_att(ncid, v%varid, "FillValue", v%FillValue) )
            end if

            ! Add additional variable attributes 

            if (trim(v%long_name) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "long_name",     trim(v%long_name)) )
            if (trim(v%standard_name) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "standard_name", trim(v%standard_name)) )

            if (trim(v%units) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "units", trim(v%units) ) )

            if (trim(v%axis) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "axis", trim(v%axis) ) )

            if (trim(v%calendar) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "calendar", trim(v%calendar) ) )

            if (trim(v%grid_mapping) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "grid_mapping", trim(v%grid_mapping) ) )

            if (trim(v%dataset) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "dataset", trim(v%dataset) ) )

            if (trim(v%level_desc) .ne. "") &
                call nc_check( nf90_put_att(ncid, v%varid, "level_desc", trim(v%level_desc) ) )

        end if

        ! ! Always update the actual range (whether new or not) if it exists
        if (v%actual_range(1) .ne. 0.d0 .and. v%actual_range(2) .ne. 0.d0) then
            select case(trim(v%xtype))
                case("NF90_INT")
                    call nc_check( nf90_put_att(ncid, v%varid, "actual_range", int(v%actual_range)) )
                case("NF90_FLOAT")
                    call nc_check( nf90_put_att(ncid, v%varid, "actual_range", real(v%actual_range)) )
                case("NF90_DOUBLE")
                    call nc_check( nf90_put_att(ncid, v%varid, "actual_range", v%actual_range) )
                case("NF90_CHAR")
                    v%actual_range = (/ 0.d0, 0.d0 /)
                case DEFAULT
                    write(*,*) "nc_put_att:: Error, wrong xtype defined:"//trim(v%xtype)
                    stop 
            end select 
        end if 

        return

    end subroutine nc_put_att
  
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  g e t _ a t t
    ! Author     :  Alex Robinson
    ! Purpose    :  Get attributes from a netcdf file for a given variable
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_get_att(ncid, v)

        implicit none

        integer :: ncid, stat, i
        double precision :: tmp, tmp2(2)
        integer :: tmpi, tmpi2(2) 

        character(len=NC_STRLEN) :: tmpstr
        type(ncvar) :: v

        integer :: ndims
        integer, allocatable :: dimids(:)

        integer, parameter :: noerr = NF90_NOERR

        ! Get the current variable's id, if it exists
        stat = nf90_inq_varid(ncid, trim(v%name), v%varid)

        ! If variable exists, get attributes from file
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

            select case(trim(v%xtype))
                case("NF90_INT")

                    stat = nc_check_att( nf90_get_att(ncid, v%varid, "actual_range", tmpi2) )
                    if (stat .eq. noerr) v%actual_range = dble(tmpi2)

                    stat = nc_check_att( nf90_get_att(ncid, v%varid, "scale_factor", tmpi) )
                    if (stat .eq. noerr) v%scale_factor = dble(tmpi)

                    stat = nc_check_att( nf90_get_att(ncid, v%varid, "add_offset", tmpi) )
                    if (stat .eq. noerr) v%add_offset = dble(tmpi)

                    stat = nc_check_att( nf90_get_att(ncid, v%varid, "missing_value", tmpi) )
                    if (stat .eq. noerr) then
                        v%missing_value = dble(tmpi)
                        v%missing_set = .TRUE.
                    end if

                    stat = nc_check_att( nf90_get_att(ncid, v%varid, "FillValue", tmpi) )
                    if (stat .eq. noerr) then
                        v%FillValue = dble(tmpi)
                        v%FillValue_set = .TRUE.
                    end if

                case DEFAULT

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

                end select
          
        end if

        return

    end subroutine nc_get_att

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ c r e a t e
    ! Author     :  Alex Robinson
    ! Purpose    :  Create a new empty netcdf file
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_create(filename,conventions)

        implicit none 

        character(len=*) :: filename
        character(len=*), optional :: conventions
        integer :: ncid

        character(len=1024), parameter :: refs = "http://www.unidata.ucar.edu/netcdf/conventions.html"
        character(len=1024), parameter :: conv = "CF-1.6"
        character(len=1024) :: history 

        ! Get ncio version for writing
        write(history,"(a,f4.2)") "Dataset generated using ncio v", NCIO_VERSION

        ! Create the new empty file and close it (necessary to avoid errors with dim vars)
        call nc_check( nf90_create(filename, nf90_clobber, ncid) )
        call nc_check( nf90_enddef(ncid) )
        call nc_check( nf90_close(ncid) )

        ! Open the file again and set for redefinition
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_check( nf90_redef(ncid) )

        ! Add dataset global attributes
        call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "references",  trim(refs)) )

        if (present(conventions)) then
            call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "Conventions", trim(conventions)) )
        else ! Default
            call nc_check( nf90_put_att(ncid, NF90_GLOBAL, "Conventions", trim(conv)) )
        end if 

        ! End define mode and close the file.
        call nc_check( nf90_enddef(ncid) )
        call nc_check( nf90_close(ncid) )


        write(*,"(a6,a20,a)") "ncio::","nc_create:: ",trim(filename)
        
        return

    end subroutine nc_create

    subroutine nc_write_global(filename,name,dat)

        implicit none 

        character(len=*) :: filename, name, dat 
        integer :: ncid

        ! Open the file again and set for redefinition
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_check( nf90_redef(ncid) )

        call nc_check( nf90_put_att(ncid, NF90_GLOBAL,trim(name),trim(dat)) )

        ! End define mode and close the file.
        call nc_check( nf90_enddef(ncid) )
        call nc_check( nf90_close(ncid) )

        write(*,"(a6,a20,a)") "ncio::","nc_write_global:: ", &
                              trim(filename)//" : "//trim(name)//" = "//trim(dat)
        
        return

    end subroutine nc_write_global

    subroutine nc_write_map(filename,name,lambda,phi,x_e,y_n)

        implicit none 

        character(len=*) :: filename, name

        integer :: ncid, varid, stat
        double precision, optional :: lambda, phi, x_e, y_n

        integer, parameter :: noerr = NF90_NOERR

        ! Open the file, set for redefinition
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_check( nf90_redef(ncid) )
        
        ! Check if grid mapping has been defined in this file
        ! (if not, define it according to input arguments)
        stat = nf90_inq_varid(ncid, trim(name), varid)

        if ( stat .ne. noerr ) then
            ! Define the mapping variable as an integer with no dimensions,
            ! and include the grid mapping name
            call nc_check( nf90_def_var(ncid, trim(name), NF90_INT, varid) )
            call nc_check( nf90_put_att(ncid,varid, "grid_mapping_name", trim(name)) )

            ! Add grid attributes depending on grid_mapping type
            select case(trim(name))

                case("stereographic")
                    call nc_check( nf90_put_att(ncid,varid, "longitude_of_projection_origin", lambda) )
                    call nc_check( nf90_put_att(ncid,varid, "latitude_of_projection_origin", phi) )
                    call nc_check( nf90_put_att(ncid,varid, "scale_factor_at_projection_origin", 1.d0) )
                    call nc_check( nf90_put_att(ncid,varid, "false_easting",  x_e) )
                    call nc_check( nf90_put_att(ncid,varid, "false_northing", y_n) )

                case("polar_stereographic")
                    call nc_check( nf90_put_att(ncid,varid, "straight_vertical_longitude_from_pole", lambda) )
                    call nc_check( nf90_put_att(ncid,varid, "latitude_of_projection_origin", phi) )
                    call nc_check( nf90_put_att(ncid,varid, "scale_factor_at_projection_origin", 1.d0) )
                    call nc_check( nf90_put_att(ncid,varid, "false_easting",  x_e) )
                    call nc_check( nf90_put_att(ncid,varid, "false_northing", y_n) )

                case DEFAULT
                    ! Do nothing

            end select

            write(*,"(a6,a20,a,a)") "ncio::","nc_write_map:: ",trim(filename)//" : ",trim(name)
        
        end if 

        ! Close the file
        call nc_check( nf90_close(ncid) )

        return 

    end subroutine nc_write_map

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ w r i t e _ d i m
    ! Author     :  Alex Robinson
    ! Purpose    :  Write a coordinate var to a netcdf file
    !               and make a new file if needed
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_write_dim( filename,name,xtype,x,x0,nx,dx, &
                             long_name,standard_name,units,axis,calendar )

        implicit none

        integer :: ncid, i

        character(len=*):: filename, name
        character(len=*), optional :: xtype,long_name, standard_name, units, axis
        character(len=*), optional :: calendar
        integer,  optional :: nx
        double precision, optional :: x0, dx, x(:)
        character(len=14) :: tmpchar

        type(ncvar) :: v
       
        double precision :: x0_tmp, dx_tmp

        call nc_v_init(v,name=trim(name),xtype=xtype,coord=.TRUE.)

        !! Now fill in values of arguments that are present
        if ( present(long_name) )     v%long_name     = trim(long_name)
        if ( present(standard_name) ) v%standard_name = trim(standard_name)
        if ( present(units) )         v%units         = trim(units)
        if ( present(axis) )          v%axis          = trim(axis)
        if ( present(calendar) )      v%calendar      = trim(calendar) 

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
          
            write(*,"(a6,a20,a)") "ncio::","nc_create:: ","error, no length given to define dimension var."
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
        call nc_put_att(ncid, v)

        ! End define mode.
        call nc_check( nf90_enddef(ncid) )

        ! Put the variable's values in the file 
        call nc_check( nf90_put_var(ncid, v%varid, v%dim) )

        ! Close the file
        call nc_check( nf90_close(ncid) )

        tmpchar = trim(v%name)
        write(*,"(a6,a20,a,a14,i6)") "ncio::","nc_write_dim:: ",trim(filename)//" : ",adjustl(tmpchar),size(v%dim)
        
        return

    end subroutine nc_write_dim 

! ================================
!
!      INTEGERS 
!
! ================================

    subroutine nc_write_int_1D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        integer :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 1 
        character (len=*) :: dim1
        character (len=*), optional :: dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))
        dat4D(:,1,1,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_int_1D

    subroutine nc_write_int_2D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        integer :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 2 
        character (len=*) :: dim1, dim2
        character (len=*), optional :: dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))
        dat4D(:,:,1,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_int_2D

    subroutine nc_write_int_3D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 3 
        character (len=*) :: dim1, dim2, dim3
        character (len=*), optional :: dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))
        dat4D(:,:,:,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_int_3D

    subroutine nc_write_int_4D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 4 
        character (len=*) :: dim1, dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))
        dat4D(:,:,:,:) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_int_4D

! ================================
!
!      DOUBLES 
!
! ================================
    
    subroutine nc_write_double_1D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        double precision :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"
        integer,            parameter :: ndims_in = 1 
        character (len=*) :: dim1
        character (len=*), optional :: dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))
        dat4D(:,1,1,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_double_1D

    subroutine nc_write_double_2D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        double precision :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"
        integer,            parameter :: ndims_in = 2 
        character (len=*) :: dim1, dim2
        character (len=*), optional :: dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))
        dat4D(:,:,1,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_double_2D

    subroutine nc_write_double_3D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"
        integer,            parameter :: ndims_in = 3 
        character (len=*) :: dim1, dim2, dim3
        character (len=*), optional :: dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))
        dat4D(:,:,:,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_double_3D

    subroutine nc_write_double_4D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"
        integer,            parameter :: ndims_in = 4 
        character (len=*) :: dim1, dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))
        dat4D(:,:,:,:) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_double_4D

! ================================
!
!      FLOATS 
!
! ================================
    
    subroutine nc_write_float_1D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        real(4) :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"
        integer,            parameter :: ndims_in = 1 
        character (len=*) :: dim1
        character (len=*), optional :: dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))
        dat4D(:,1,1,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_float_1D

    subroutine nc_write_float_2D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"
        integer,            parameter :: ndims_in = 2 
        character (len=*) :: dim1, dim2
        character (len=*), optional :: dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))
        dat4D(:,:,1,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_float_2D

    subroutine nc_write_float_3D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"
        integer,            parameter :: ndims_in = 3 
        character (len=*) :: dim1, dim2, dim3
        character (len=*), optional :: dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))
        dat4D(:,:,:,1) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_float_3D

    subroutine nc_write_float_4D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"
        integer,            parameter :: ndims_in = 4 
        character (len=*) :: dim1, dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))
        dat4D(:,:,:,:) = dble(dat)

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_float_4D

    ! ================================
    !
    !      LOGICALS
    !
    ! ================================

    subroutine nc_write_logical_1D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        logical :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 1 
        character (len=*) :: dim1
        character (len=*), optional :: dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))
        dat4D(:,:,:,:) = 0.d0
        where(dat) dat4D(:,1,1,1) = 1.d0 

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_1D

    subroutine nc_write_logical_2D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        logical :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 2 
        character (len=*) :: dim1, dim2
        character (len=*), optional :: dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))
        dat4D(:,:,:,:) = 0.d0
        where(dat) dat4D(:,:,1,1) = 1.d0 

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_2D

    subroutine nc_write_logical_3D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 3 
        character (len=*) :: dim1, dim2, dim3
        character (len=*), optional :: dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))
        dat4D(:,:,:,:) = 0.d0
        where(dat) dat4D(:,:,:,1) = 1.d0 

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_3D

    subroutine nc_write_logical_4D(filename,dat,name,dim1,dim2,dim3,dim4,start,count, &
                               long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"
        integer,            parameter :: ndims_in = 4 
        character (len=*) :: dim1, dim2, dim3, dim4 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))
        dat4D(:,:,:,:) = 0.d0
        where(dat) dat4D(:,:,:,:) = 1.d0 

        ! Finally call the internal writing routine
        call nc_write_internal_numeric(filename,dat4D,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                       start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_4D

! =====================
! =====================
!
!  READING
!
! =====================
! =====================

! ================================
!
!      INTS 
!
! ================================
    subroutine nc_read_int_1D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        integer :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = int(dat4D(:,1,1,1))

        return

    end subroutine nc_read_int_1D

    subroutine nc_read_int_2D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        integer :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = int(dat4D(:,:,1,1))

        return

    end subroutine nc_read_int_2D

    subroutine nc_read_int_3D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = int(dat4D(:,:,:,1))

        return

    end subroutine nc_read_int_3D

    subroutine nc_read_int_4D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = int(dat4D(:,:,:,:))

        return

    end subroutine nc_read_int_4D

! ================================
!
!      DOUBLES 
!
! ================================

    subroutine nc_read_double_1D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        double precision :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = dble(dat4D(:,1,1,1))

        return

    end subroutine nc_read_double_1D

    subroutine nc_read_double_2D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        double precision :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = dble(dat4D(:,:,1,1))

        return

    end subroutine nc_read_double_2D

    subroutine nc_read_double_3D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = dble(dat4D(:,:,:,1))

        return

    end subroutine nc_read_double_3D

    subroutine nc_read_double_4D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = dble(dat4D(:,:,:,:))

        return

    end subroutine nc_read_double_4D

! ================================
!
!      FLOATS
!
! ================================

    subroutine nc_read_float_1D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        real(4) :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = real(dat4D(:,1,1,1))

        return

    end subroutine nc_read_float_1D

    subroutine nc_read_float_2D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = real(dat4D(:,:,1,1))

        return

    end subroutine nc_read_float_2D

    subroutine nc_read_float_3D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = real(dat4D(:,:,:,1))

        return

    end subroutine nc_read_float_3D

    subroutine nc_read_float_4D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = real(dat4D(:,:,:,:))

        return

    end subroutine nc_read_float_4D

! ================================
!
!      INTS 
!
! ================================
    subroutine nc_read_logical_1D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        logical :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),1,1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(dat4D(:,1,1,1) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_1D

    subroutine nc_read_logical_2D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        logical :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),1,1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(dat4D(:,:,1,1) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_2D

    subroutine nc_read_logical_3D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),1))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(dat4D(:,:,:,1) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_3D

    subroutine nc_read_logical_4D(filename,dat,name,start,count)

        implicit none 

        double precision, dimension(:,:,:,:), allocatable :: dat4D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:)
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat4D)) deallocate(dat4D)
        allocate(dat4D(size(dat,1),size(dat,2),size(dat,3),size(dat,4)))

        ! Finally call the internal writing routine
        call nc_read_internal_numeric(filename,dat4D,name,start,count)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(dat4D(:,:,:,:) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_4D

! =================================
!
!   INTERNAL WRITE/READ FUNCTIONS
!
! =================================

    subroutine nc_write_internal_numeric(filename,dat,name,xtype,ndims_in,dim1,dim2,dim3,dim4, &
                                         start,count,long_name,standard_name,grid_mapping,units)

        implicit none 

        double precision :: dat(:,:,:,:)
        character (len=*), optional :: dim1, dim2, dim3, dim4 
        integer, optional :: start(:), count(:)

        character (len=*) :: filename, name, xtype
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        integer :: ndims_in 

        type(ncvar) :: v

        ! netCDF needed counters, array, and names of dims
        integer :: ncid, stat

        ! Additional helper variables
        integer :: i, j, k, m, ndims
        double precision :: actual_range(2)

        ! Initialize ncvar type
        call nc_v_init(v,trim(name),xtype=trim(xtype),ndims_in=ndims_in)

        ! Add extra var info if available from arguments
        if ( present(long_name) )     v%long_name     = trim(long_name)
        if ( present(standard_name) ) v%standard_name = trim(standard_name)
        if ( present(grid_mapping) )  v%grid_mapping  = trim(grid_mapping)
        if ( present(units) )         v%units         = trim(units)

        ! Open the file in nowrite mode
        ! and get attributes if variable already exist
        call nc_check( nf90_open(filename, nf90_nowrite, ncid) )
        call nc_get_att(ncid,v)    
        call nc_check( nf90_close(ncid) )

        ! Determine number of dims in file from arguments
        ndims = 1
        if (present(dim2)) ndims = 2
        if (present(dim3)) ndims = 3
        if (present(dim4)) ndims = 4

        ! Initialize the start and count arrays
        allocate(v%start(ndims))
        v%start(:) = 1
        if (present(start)) v%start = start

        ! Initiliaze count such that the entire input array will be stored in file
        ! unless count argument is given
        allocate(v%count(ndims))
        do i = 1, ndims 
            v%count(i) = size(dat,i)
            if (v%count(i) .eq. 0) v%count(i) = 1   ! In case ndims_in < ndims
        end do 
        if (present(count)) v%count = count

        ! Allocate dimensions of variable on file
        if (allocated(v%dims)) deallocate(v%dims)
        allocate( v%dims(ndims) )
        do i = 1, ndims 
            select case(i)
                case(1)
                    v%dims(i) = trim(dim1)
                case(2)
                    v%dims(i) = trim(dim2)
                case(3)
                    v%dims(i) = trim(dim3)
                case(4)
                    v%dims(i) = trim(dim4)
            end select
        end do

        ! Reset or initialize the actual range of the variable
        actual_range = (/ dble(minval(dat)), dble(maxval(dat)) /)
        if (trim(v%dims(ndims)) == "time") then
            if (v%start(ndims) .ne. 1) then
                v%actual_range(1) = min(v%actual_range(1),actual_range(1))
                v%actual_range(2) = max(v%actual_range(2),actual_range(2))
            else
                v%actual_range = actual_range
            end if
        end if

        ! Modify the variable according to scale and offset (if working with real or double data)
        if (trim(v%xtype) .eq. "NF90_FLOAT" .or. trim(v%xtype) .eq. "NF90_DOUBLE") then
            if (v%missing_set) then
                where (dat .ne. v%missing_value) dat = (dat-v%add_offset)/v%scale_factor
            else    
                ! Apply the scalar and offset if available
                dat = (dat-v%add_offset)/v%scale_factor
            end if
        end if
        
        ! Open the file
        call nc_check( nf90_open(filename, nf90_write, ncid) )

        ! Define / update the netCDF variable for the data.
        call nc_check( nf90_redef(ncid) )
        call nc_put_att(ncid, v)
        call nc_check( nf90_enddef(ncid) )
        
        ! Write the data to the netcdf file
        ! (NF90 converts dat to proper type (int, real, dble)
        call nc_check( nf90_put_var(ncid, v%varid, dat,v%start,v%count) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        call nc_check( nf90_close(ncid) )

        !write(*,"(a6,a20,a,a14)") "ncio::","nc_write:: ",trim(filename)//" : ",trim(v%name)
        
        return 

    end subroutine nc_write_internal_numeric

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ r e a d
    ! Author     :  Alex Robinson
    ! Purpose    :  Read a variable from a netcdf file
    !               (only one time step 'ndat' at a time)
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_read_internal_numeric(filename,dat4D,name,start,count)

        implicit none

        integer :: ncid, stat, ndims, ndat, dim_in, n1
        integer, optional    :: start(:), count(:)

        character (len=*) :: filename, name
        type(ncvar) :: v

        double precision, allocatable, dimension(:,:,:,:) :: dat4D

        double precision    :: tmp
        character (len=NC_STRLEN) :: tmpstr

        ! Loop indices
        integer :: lev, lat, lon, rec, i, j, k, nt

        ! Initializing
        v%name = name
        
        ! Open the file. 
        call nc_check( nf90_open(filename, nf90_nowrite, ncid) )

        ! Initialize the netcdf variable info and load attributes
        call nc_v_init(v,name)
        call nc_get_att(ncid,v)

        ! Get variable dimension
        ndims = size(v%dims)

        if (present(start)) then
            allocate(v%start(size(start)))
            v%start = start 
        else 
            allocate(v%start(ndims))
            v%start(:) = 1
        end if 

        if (present(count)) then
            allocate(v%count(size(count)))
            v%count = count 
        else 
            allocate(v%count(ndims))
            do i = 1, ndims
              v%count(i) = size(dat4D,i)
            end do
        end if 

        
        ! Read the variable data from the file
        ! (NF90 converts dat to proper type (int, real, dble)
        call nc_check( nf90_get_var(ncid, v%varid, dat4D, v%start, v%count) )

        ! Close the file. This frees up any internal netCDF resources
        ! associated with the file.
        call nc_check( nf90_close(ncid) )

        if (v%missing_set) then
          where (dat4D .ne. v%missing_value) dat4D = dat4D*v%scale_factor + v%add_offset
        else    
          ! Apply the scalar and offset if available
          if (v%scale_factor .ne. 1.d0 .and. v%add_offset .ne. 0.d0) &
              dat4D = dat4D*v%scale_factor + v%add_offset
        end if

        write(*,"(a6,a20,a,a)") "ncio::","nc_read:: ",trim(filename)//" : ",trim(v%name)

        return

    end subroutine nc_read_internal_numeric

    ! ================================
!
!      CHARACTERS 
!
! ================================

    subroutine nc_write_char_1D(filename,dat,name)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:)
        character (len=*) :: filename, name
        integer :: i 

        ! Convert the character array into a long string
        string = trim(dat(1))
        do i = 2, size(dat)
            string = trim(string)//NC_STR_SEP//trim(dat(i))
        end do

        ! Finally call the internal writing routine
        call nc_write_internal_char(filename,string,name)

        return

    end subroutine nc_write_char_1D

    subroutine nc_write_char_2D(filename,dat,name)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:,:)
        character (len=*) :: filename, name
        integer :: i, j

        ! Convert the character array into a long string
        write(*,"(a6,a20,a,a14)") "ncio::","nc_write_char:: ", &
                                  "warning: 2D character array could not be written: "//trim(name)
        return

    end subroutine nc_write_char_2D

    subroutine nc_write_char_3D(filename,dat,name)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:,:,:)
        character (len=*) :: filename, name
        integer :: i, j

        ! Convert the character array into a long string
        write(*,"(a6,a20,a,a14)") "ncio::","nc_write_char:: ", &
                                  "warning: 3D character array could not be written: "//trim(name)
        return

    end subroutine nc_write_char_3D

    subroutine nc_write_char_4D(filename,dat,name)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:,:,:,:)
        character (len=*) :: filename, name
        integer :: i, j

        ! Convert the character array into a long string
        write(*,"(a6,a20,a,a14)") "ncio::","nc_write_char:: ", &
                                  "warning: 4D character array could not be written: "//trim(name)
        return

    end subroutine nc_write_char_4D

    subroutine nc_read_char_1D(filename,dat,name,sep)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:)
        character(len=*), optional :: sep 
        character (len=*) :: filename, name
        integer :: i, j, nsep 

        character(len=10) :: separator 

        ! Determine the separator used to parse the elements of the
        ! array from the string saved in the netcdf file
        separator = trim(NC_STR_SEP)
        if (present(sep)) separator = trim(sep)
        nsep = len_trim(separator)

        ! Call the internal reading routine to get a long string
        call nc_read_internal_char(filename,string,name)

        ! Convert the string into a character array
        do i = 1, size(dat)-1
            string = adjustl(string)
            j = index(string, trim(separator))-1
            dat(i) = string(1:j)
            string(1:j+nsep) = "" !repeat(" ",j+nsep)
        end do
        dat(size(dat)) = trim(adjustl(string))

        return

    end subroutine nc_read_char_1D

    subroutine nc_write_internal_char(filename,string,name)

        implicit none 

        character (len=*) :: string
        character (len=*) :: filename, name
        character (len=256) :: dimname 

        type(ncvar) :: v

        ! netCDF needed counters, array, and names of dims
        integer :: ncid, stat, dimid, str_len

        ! Initialize ncvar type
        call nc_v_init(v,trim(name),xtype="NF90_CHAR")

        dimname = trim(v%name)//"_len"
        str_len = len_trim(string) 

        ! Open the file
        call nc_check( nf90_open(filename, nf90_write, ncid) )

        ! Define / update the netCDF variable for the data.
        call nc_check( nf90_redef(ncid) )
        call nc_check( nf90_def_dim(ncid, trim(dimname), str_len, dimid) )
        call nc_check( nf90_def_var(ncid, trim(v%name), NF90_CHAR, (/ dimid /), v%varid) )
        call nc_check( nf90_enddef(ncid) )
        
        ! Write the data to the netcdf file
        ! (NF90 converts dat to proper type (int, real, dble)
        call nc_check( nf90_put_var(ncid, v%varid, trim(string) ) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        call nc_check( nf90_close(ncid) )

        !write(*,"(a6,a20,a,a14)") "ncio::","nc_write_char:: ",trim(filename)//" : ",trim(v%name)
        
        return 

    end subroutine nc_write_internal_char

    subroutine nc_read_internal_char(filename,string,name)

        implicit none 

        character (len=*) :: string
        character (len=*) :: filename, name

        type(ncvar) :: v

        ! netCDF needed counters, array, and names of dims
        integer :: ncid, stat, dimid, str_len

        ! Initialize ncvar type
        call nc_v_init(v,trim(name),xtype="NF90_CHAR")

        ! Open the file
        call nc_check( nf90_open(filename, nf90_write, ncid) )
        call nc_get_att(ncid,v) 

        ! Read the string from the netcdf file
        call nc_check( nf90_get_var(ncid, v%varid, string(1:v%dlen(1))) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        call nc_check( nf90_close(ncid) )

        !write(*,"(a6,a20,a,a14)") "ncio::","nc_read_char:: ",trim(filename)//" : ",trim(v%name)
        
        return 

    end subroutine nc_read_internal_char


end module ncio
