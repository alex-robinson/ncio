! The MIT License (MIT)
!
! Copyright (c) 2014 Alexander Robinson and Mahé Perrette
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module ncio

    use netcdf

    implicit none

    double precision, parameter :: NCIO_VERSION = 0.2d0

    integer, parameter :: NC_STRLEN = 256 
    integer, parameter :: NC_STRLEN_MAX = 10000
    character(len=2)   :: NC_STR_SEP = "; "
    double precision, parameter :: NC_TOL = 1d-7
    double precision, parameter :: NC_LIM = 1d25

    character(len=NC_STRLEN), parameter :: NC_CHARDIM = "strlen"

    type ncvar
        character (len=NC_STRLEN) :: name, long_name, standard_name, units
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
        module procedure    nc_write_int_pt, &
                            nc_write_int_1D, nc_write_int_2D, &
                            nc_write_int_3D, nc_write_int_4D  
        module procedure    nc_write_double_pt, &
                            nc_write_double_1D, nc_write_double_2D, &
                            nc_write_double_3D, nc_write_double_4D, &
                            nc_write_double_5D, nc_write_double_6D
        module procedure    nc_write_float_pt, &
                            nc_write_float_1D, nc_write_float_2D, &
                            nc_write_float_3D, nc_write_float_4D
        module procedure    nc_write_logical_pt, &
                            nc_write_logical_1D, nc_write_logical_2D, &
                            nc_write_logical_3D, nc_write_logical_4D
        
        ! Note: character writing and reading do not 
        ! follow the conventions of the other data types
        module procedure    nc_write_internal_char, &
                            nc_write_char_1D, nc_write_char_2D, &
                            nc_write_char_3D, nc_write_char_4D   ! Dummy procedures
    
    end interface  

    interface nc_read 
        module procedure    nc_read_int_pt, &
                            nc_read_int_1D, nc_read_int_2D, &
                            nc_read_int_3D, nc_read_int_4D, &
                            nc_read_int_5D, nc_read_int_6D 
        module procedure    nc_read_double_pt, &
                            nc_read_double_1D, nc_read_double_2D, &
                            nc_read_double_3D, nc_read_double_4D, &
                            nc_read_double_5D, nc_read_double_6D 
        module procedure    nc_read_float_pt, &
                            nc_read_float_1D, nc_read_float_2D, &
                            nc_read_float_3D, nc_read_float_4D, &
                            nc_read_float_5D, nc_read_float_6D  
        module procedure    nc_read_logical_pt, &
                            nc_read_logical_1D, nc_read_logical_2D, &
                            nc_read_logical_3D, nc_read_logical_4D, &
                            nc_read_logical_5D, nc_read_logical_6D                    

        ! Note: character writing and reading do not 
        ! follow the conventions of the other data types
        module procedure    nc_read_internal_char, &
                            nc_read_char_1D
    end interface

    interface nc_write_dim
        module procedure    nc_write_dim_int_pt, nc_write_dim_int_1D
        module procedure    nc_write_dim_double_pt, nc_write_dim_double_1D
        module procedure    nc_write_dim_float_pt, nc_write_dim_float_1D
    end interface

    interface nc_write_attr 
        module procedure    nc_write_attr_global, nc_write_attr_variable
    end interface 

    interface nc_read_attr 
        module procedure    nc_read_attr_global, nc_read_attr_variable
    end interface 
    
    private
    public :: nc_read, nc_read_attr  
    public :: nc_create, nc_write_map, nc_write_dim
    public :: nc_open, nc_close 
    public :: nc_write, nc_write_attr, nc_size

    public :: nc_write_attr_std_dim

contains

! =================================
!
!   INTERNAL WRITE/READ FUNCTIONS
!
! =================================
    
    subroutine nc4_write_internal(filename,name,dat,xtype,size_in,&
                                  ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                  start,count,long_name,standard_name,grid_mapping,units, &
                                  missing_value_int,missing_value_float,missing_value_double)
                        

        implicit none 

        type(ncvar) :: v

        double precision :: dat(:)
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        integer, optional :: start(:), count(:)

        character (len=*) :: filename, name, xtype
        character (len=*),   optional :: long_name, standard_name, grid_mapping, units
        integer,          optional :: missing_value_int
        real(4),          optional :: missing_value_float
        double precision, optional :: missing_value_double
        integer :: size_in(:)
        integer, allocatable :: size_var(:)

        integer, optional :: ncid 
        integer :: nc_id 

        ! netCDF needed counters, array, and names of dims
        integer :: stat, ncount, nvar, RecordDimID

        ! Additional helper variables
        integer :: i, j, k, m, ndims, dimid
        double precision :: actual_range(2)

        ! Initialize ncvar type
        call nc_v_init(v,trim(name),xtype=trim(xtype))

        ! Add extra var info if available from arguments
        if ( present(long_name) )     v%long_name     = trim(long_name)
        if ( present(standard_name) ) v%standard_name = trim(standard_name)
        if ( present(grid_mapping) )  v%grid_mapping  = trim(grid_mapping)
        if ( present(units) )         v%units         = trim(units)

        if (present(missing_value_int)) then 
            v%missing_set = .TRUE.
            v%missing_value = dble(missing_value_int)
        else if (present(missing_value_float)) then 
            v%missing_set = .TRUE.
            v%missing_value = dble(missing_value_float)
        else if (present(missing_value_double)) then 
            v%missing_set = .TRUE.
            v%missing_value = missing_value_double
        end if 

        ! Open the file in write mode from filename or ncid
        call nc_check_open(filename, ncid, nf90_write, nc_id)
        ! and get attributes if variable already exist
        call nc_get_att(nc_id,v)    
        call nc_check( nf90_inquire(nc_id, unlimitedDimID=RecordDimID) ) ! Get Unlimited dimension ID if any

        ! Determine number of dims in file from arguments
        if (present(dims)) then
            ndims = size(dims)

            ! Consistency check
            if (present(dim1)) then
                write(*,*) "nc_write:: Warning, either `dims` or `dim1,dim2,...` arguments "
                write(*,*) "           should be specified - not both. Using `dims`."
            end if
        else
            ndims = 1
            if (present(dim2)) ndims = 2
            if (present(dim3)) ndims = 3
            if (present(dim4)) ndims = 4
            if (present(dim5)) ndims = 5
            if (present(dim6)) ndims = 6
        end if 

        ! Allocate dimensions of variable on file
        if (allocated(v%dims)) deallocate(v%dims)
        allocate( v%dims(ndims) )
        if (present(dims)) then
            do i = 1, ndims
                v%dims(i) = trim(dims(i))
            end do 
        else
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
                    case(5)
                        v%dims(i) = trim(dim5)
                    case(6)
                        v%dims(i) = trim(dim6)
                end select
            end do
        end if

        ! Initialize the start and count arrays
        allocate(v%start(ndims),v%count(ndims),size_var(ndims))
        v%start = 1
        if (present(start)) v%start = start

        nvar = 1
        do i = 1, ndims
            size_var(i) = nc_size(filename,v%dims(i),ncid=nc_id)
            nvar = nvar*size_var(i)
        end do

        ! Initialize count such that the entire input array will be stored in file
        ! unless count argument is given
        v%count = 1
        if (present(count)) then
            ! Assign argument as count
            v%count = count   
        else if (size(dat) .eq. nvar) then
            ! Assign count from size of variable on file
            v%count = size_var 
        else
            ! Assign count from input dimension size
            v%count(1:size(size_in)) = size_in 
        end if 

        ! Reset or initialize the actual range of the variable
        actual_range = [minval(dat),maxval(dat)]
!         if (trim(v%dims(ndims)) == "time") then
            if (v%start(ndims) .ne. 1) then
                v%actual_range(1) = min(v%actual_range(1),actual_range(1))
                v%actual_range(2) = max(v%actual_range(2),actual_range(2))
            else
                v%actual_range = actual_range
            end if
!         end if
        
        ! Check if the variable gave us reasonable values
        if (maxval(dabs(v%actual_range)) .gt. 1d98) then 
            write(*,*) "ncio:: nc_write:: warning: actual range too high,&
                            & variable may not be defined: "//trim(v%name)
            v%actual_range = [0.d0,0.d0]
        end if

        ! Modify the variable according to scale and offset (if working with real or double data)
        if (trim(v%xtype) .eq. "NF90_FLOAT" .or. trim(v%xtype) .eq. "NF90_DOUBLE") then
            if (v%missing_set) then
                where( dabs(dat-v%missing_value) .gt. NC_TOL ) dat = (dat-v%add_offset)/v%scale_factor
            else    
                ! Apply the scalar and offset if available
                dat = (dat-v%add_offset)/v%scale_factor
            end if
        end if
        
        ! Summarize what we'll do (diagnostics!!)  
!         call nc_print_attr(v)
!         write(*,*) "ubound(dat): ",ubound(dat)
!         write(*,*) "size(dat):   ",size(dat)
!         write(*,*) "dat range:",minval(dat),maxval(dat)
!         write(*,*) "start:",v%start 
!         write(*,*) "count:",v%count 
!         write(*,*) "size_in:",size_in 
!         write(*,*) "shape dat:",shape(dat)

        ! Make sure count size makes sense
        ncount = 1
        do i = 1, ndims 
            ncount = ncount*v%count(i)
        end do 
        if (size(dat) .ne. ncount) then
            write(*,*) "ncio:: error: "// &
                       "The input variable size does not match the count of values to write to file."
            write(*,*) trim(filename)//": "//trim(v%name)
            write(*,*) "Size of variable: ",size(dat)
            write(*,*) "Size of count:    ",ncount 
            write(*,*) "start: ",start 
            write(*,*) "count: ",count 
            stop
        end if 

        ! Open the file
!         call nc_check( nf90_open(filename, nf90_write, nc_id) )

        do i = 1, ndims

            call nc_check( nf90_inq_dimid(nc_id, v%dims(i), dimid) )

            ! If unlimited dimension, the size does not matter
            if (dimid .eq. RecordDimID) cycle

            call nc_check( nf90_inquire_dimension(nc_id, dimid, len=size_var(i)) )

            if (v%count(i) .gt. size_var(i)) then 
                write(*,*) "ncio:: error: "// &
                           "count exceeds this dimension length."
                write(*,*) trim(filename)//": "//trim(v%name)
                write(*,*) "Dimension exceeded: ",trim(v%dims(i)), size_var(i)," < ",v%count(i)
                write(*,*) "Are the data values a different shape than the file dimensions?"
                write(*,*) "   In that case, specify start+count as arguments."
                stop 
            end if 
        end do 

        ! Define / update the netCDF variable for the data.
        call nc_check( nf90_redef(nc_id) )
        call nc_put_att(nc_id, v)
        call nc_check( nf90_enddef(nc_id) )

        ! Write the data to the netcdf file
        ! Note: NF90 converts dat to proper type (int, real, dble) and shape
        call nc_check( nf90_put_var(nc_id, v%varid, dat,v%start,v%count) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        if (.not. present(ncid)) call nc_check( nf90_close(nc_id) )

!         write(*,"(a,a,a14)") "ncio:: nc_write:: ",trim(filename)//" : ",trim(v%name)
        
        return 

    end subroutine nc4_write_internal

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ r e a d
    ! Author     :  Alex Robinson
    ! Purpose    :  Read a variable from a netcdf file
    !               (only one time step 'ndat' at a time)
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc4_read_internal_numeric(filename,name,dat,size_in,start,count,xtype, &
                                         missing_value_int,missing_value_float,missing_value_double, ncid)

        implicit none

        integer :: nc_id, stat, ndims, ndat, dim_in, n1
        integer, optional    :: start(:), count(:), ncid

        character (len=*) :: filename, name, xtype
        type(ncvar) :: v

        double precision, dimension(:) :: dat
        integer :: size_in(:)

        integer,          optional :: missing_value_int
        real(4),          optional :: missing_value_float
        double precision, optional :: missing_value_double

        double precision    :: tmp
        character (len=NC_STRLEN) :: tmpstr
        integer :: i

        ! Open the file. 
        call nc_check_open(filename, ncid, nf90_nowrite, nc_id)

        ! TO DO: Add check to make sure variable exists !!!!
        ! Exit if not!

        ! Initialize the netcdf variable info and load attributes
        call nc_v_init(v,name)
        call nc_get_att(nc_id,v,readmeta=.TRUE.)
        v%xtype = trim(xtype) 

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
            if (.not. ndims .eq. size(size_in)) then 
                write(*,"(a)") "nc_read:: ","Warning: "// &
                "Variable dimensions in the file do not match those being read in."
                write(*,*) trim(filename)//": ",trim(name)
            end if 
            allocate(v%count(ndims))
            v%count = 1 
            do i = 1, size(size_in)
              v%count(i) = size_in(i)
            end do
        end if 

        ! Read the variable data from the file
        ! (NF90 converts dat to proper type (int, real, dble)
        call nc_check( nf90_get_var(nc_id, v%varid, dat, v%start, v%count) )

        ! Close the file. This frees up any internal netCDF resources
        ! associated with the file.
        if (.not. present(ncid)) call nc_check( nf90_close(nc_id) )
        
        if (v%missing_set) then
            where( dabs(dat-v%missing_value) .gt. NC_TOL ) dat = dat*v%scale_factor + v%add_offset

            ! Fill with user-desired missing value 
            if (present(missing_value_int)) &
                where( dabs(dat-v%missing_value) .le. NC_TOL ) dat = dble(missing_value_int)
            if (present(missing_value_float)) &
                where( dabs(dat-v%missing_value) .le. NC_TOL ) dat = dble(missing_value_float)
            if (present(missing_value_double)) &
                where( dabs(dat-v%missing_value) .le. NC_TOL ) dat = dble(missing_value_double)

        else    
            ! Apply the scalar and offset if available
            if (v%scale_factor .ne. 1.d0 .and. v%add_offset .ne. 0.d0) &
              dat = dat*v%scale_factor + v%add_offset
        end if

        ! Also eliminate crazy values (in case they are not handled by missing_value for some reason)
        ! Fill with user-desired missing value 
        if (present(missing_value_int)) &
            where( dabs(dat) .ge. NC_LIM ) dat = dble(missing_value_int)
        if (present(missing_value_float)) &
            where( dabs(dat) .ge. NC_LIM ) dat = dble(missing_value_float)
        if (present(missing_value_double)) &
            where( dabs(dat) .ge. NC_LIM ) dat = dble(missing_value_double)

!         write(*,"(a,a,a)") "ncio:: nc_read:: ",trim(filename)//" : ",trim(v%name)

        return

    end subroutine nc4_read_internal_numeric

    ! Open a netcdf file and return a valid ncid
    subroutine nc_open(filename,ncid,writable)

        implicit none 

        character(len=*) filename 
        logical, optional :: writable 
        logical :: is_writable 
        integer :: ncid 

        is_writable = .TRUE. 
        if (present(writable)) is_writable = writable

        if (is_writable) then 
            call nc_check( nf90_open(filename, nf90_write, ncid) )
        else 
            call nc_check( nf90_open(filename, nf90_nowrite, ncid) )
        end if 

        return 

    end subroutine nc_open 

    ! Close a netcdf file returning the ncid=0
    subroutine nc_close(ncid)

        implicit none 

        integer :: ncid 
        call nc_check( nf90_close(ncid) )

        ncid = 0 

        return 

    end subroutine nc_close 

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
        if ( present(grid_mapping) )  v%grid_mapping   = trim(grid_mapping)
        if ( present(units) )         v%units          = trim(units)
        if ( present(axis) )          v%axis           = trim(axis)
        if ( present(calendar) .and. trim(name) == "time" )  &
                                      v%calendar       = trim(calendar)

        if ( present(coord)) v%coord = coord 
        if ( present(xtype)) v%xtype = trim(xtype)

        ! Deallocate all arrays
        if (allocated(v%dim))  deallocate(v%dim)
        if (allocated(v%dims)) deallocate(v%dims)
        if (allocated(v%dlen)) deallocate(v%dlen)

        do i = 1, 4
            v%dims_in(i) = ""
        end do 
        v%ndims_in = 0 
        if (present(ndims_in)) v%ndims_in = ndims_in 

        return

    end subroutine nc_v_init

    !
    ! Write attributes for standard dimensions found in a netCDF file 
    ! (if attribute not already defined)
    !
    subroutine nc_write_attr_std_dim(filename, ncid)

        integer :: nc_id, ndims, varid, stat, i
        integer, allocatable :: dimids(:)
        integer, optional, intent(in) :: ncid

        character (len=*), intent(in) :: filename
        character (len=NC_STRLEN) :: name

        ! Open netCDF file and get file ID
        call nc_check_open(filename, ncid, nf90_write, nc_id)

        ! Make netCDF file ready for writing attributes
        call nc_check( nf90_redef(nc_id) )

        ! Loop over dimensions and update attributes
        do i = 1, 99

            ! Check whether a dimension corresponds to this ID
            ! exit loop if not
            stat = nf90_inquire_dimension(nc_id,i,name=name) 
            if ( stat .ne. NF90_NOERR) then
                exit
            endif

            ! Check whether a variable is associated with this dimension
            stat = nf90_inq_varid(nc_id, trim(name), varid) 
            if ( varid .ne. NF90_NOERR) cycle ! if dimension is not defined as variable, skip it

            ! Update some variable info based on further default cases
            select case(trim(name))

                case("x","xc")
                    !call nc_check( nf90_put_att(nc_id, varid, "long", v%scale_factor) )
                    call put_att_check(nc_id, varid, "long_name"     , "x-coordinate in Cartesian system")
                    call put_att_check(nc_id, varid, "standard_name" , "projection_x_coordinate")
                    call put_att_check(nc_id, varid, "units" , "kilometers")
                    call put_att_check(nc_id, varid, "axis"  , "X")
                case("y","yc")
                    call put_att_check(nc_id, varid, "long_name"     , "y-coordinate in Cartesian system")
                    call put_att_check(nc_id, varid, "standard_name" , "projection_y_coordinate")
                    call put_att_check(nc_id, varid, "units" , "kilometers")
                    call put_att_check(nc_id, varid, "axis"  , "Y")
                case("z","lev")
                    call put_att_check(nc_id, varid, "units" , "meters")
                    call put_att_check(nc_id, varid, "axis"  , "Z")
                case("kc","kt","kr")
                    call put_att_check(nc_id, varid, "axis"  , "Z")
                case("time")
                    call put_att_check(nc_id, varid, "units" , "years since 0-0-0")
                    call put_att_check(nc_id, varid, "axis"  , "T")
                case("lon","longitude")
                    call put_att_check(nc_id, varid, "long_name" , "longitude")
                    call put_att_check(nc_id, varid, "standard_name" , "longitude")
                    call put_att_check(nc_id, varid, "units"     , "degrees_east")
                case("lat","latitude")
                    call put_att_check(nc_id, varid, "long_name" , "latitude")
                    call put_att_check(nc_id, varid, "standard_name" , "latitude")
                    call put_att_check(nc_id, varid, "units"     , "degrees_north")
                case default
                    ! Do nothing

            end select

        end do

        ! End definition mode and close file
        call nc_check( nf90_enddef(nc_id) )
        if (.not.present(ncid))  call nc_check( nf90_close(nc_id) )

    end subroutine

    ! write a variable attribute if not already defined
    subroutine put_att_check(ncid, varid, name, value)

        integer, intent(in) :: ncid, varid
        character (len=*), intent(in) :: name, value

        ! internal variables
        integer :: stat
        character (len=NC_STRLEN) :: tmpstr

        ! Try to retrieve attribute
        stat = nf90_get_att(ncid, varid, name, tmpstr)
    
        ! Write new attribute only if not already defined
        if ( stat .ne. NF90_NOERR) then
            call nc_check( nf90_put_att(ncid, varid, name, value) )
        endif

    end subroutine


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
    ! Purpose    :  Open file when necessary, informative error message
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_check_open(filename, ncid, mode, nc_id)

        implicit none

        character(len=*), optional, intent ( in) :: filename
        integer, optional, intent ( in) :: ncid
        integer, intent (in) :: mode   
        integer, intent (out) :: nc_id

        integer stat

        ! Open the file. 
        if (present(ncid)) then 
          nc_id = ncid
        else
          if (.not. present(filename)) then
                write(*, *) "ncio :: neither filename nor ncid provided, stop"
                stop
          endif
          stat = nf90_open(filename, mode, nc_id) 
          if (stat .ne. NF90_NOERR) then
              write(*, *) "ncio :: error when opening file, no such file or directory? :: ",trim(filename)
              stop
          endif
        end if 

        return

    end subroutine nc_check_open
  
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  a t t r _ p r i n t
    ! Author     :  Alex Robinson
    ! Purpose    :  Print all major netcdf attributes
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_print_attr(v)

        implicit none

        type(ncvar) :: v
        integer :: i 
        character(len=512) :: dims_char

        write(dims_char,*) (trim(v%dims(i))//"  ", i=1,size(v%dims))

        write(*,*) 
        write(*,*) "nc_print_attr::"
        write(*,"(10x,a20,a1,2x,a)") "name", ":",trim(v%name)
        write(*,"(10x,a20,a1,2x,a)") "xtype",":",trim(v%xtype)
        write(*,"(10x,a20,a1,2x,a)") "dims", ":",trim(dims_char)
        if (.not. trim(v%long_name) .eq. "") &
            write(*,"(10x,a20,a1,2x,a)")      "long_name",":",     trim(v%long_name)
        if (.not. trim(v%standard_name) .eq. "") &
            write(*,"(10x,a20,a1,2x,a)")      "standard_name",":", trim(v%standard_name)
        if (.not. trim(v%units) .eq. "") &
            write(*,"(10x,a20,a1,2x,a)")      "units",    ":",     trim(v%units)
        if (.not. trim(v%axis) .eq. "") &
            write(*,"(10x,a20,a1,2x,a)")      "axis",":",          trim(v%axis)
        write(*,"(10x,a20,a1,2x,2e12.4)") "actual_range",":",  v%actual_range
        write(*,"(10x,a20,a1,2x,e12.4)")  "add_offset",":",    v%add_offset
        write(*,"(10x,a20,a1,2x,e12.4)")  "scale_factor",":",  v%scale_factor
        write(*,"(10x,a20,a1,2x,e12.4)")  "missing_value",":", v%missing_value
        write(*,"(10x,a20,a1,2x,L2)")     "missing_set",":",   v%missing_set
        if (.not. trim(v%grid_mapping) .eq. "") &
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
                    call nc_check ( nf90_inq_dimid(ncid, v%dims(i), dimids(i)) )
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

                    if (v%missing_set) then
                        select case(trim(v%xtype))
                            case("NF90_INT")
                                call nc_check( nf90_put_att(ncid, v%varid, "missing_value", int(v%missing_value)) )
                            case("NF90_FLOAT")
                                call nc_check( nf90_put_att(ncid, v%varid, "missing_value", real(v%missing_value)) )
                            case("NF90_DOUBLE")
                                call nc_check( nf90_put_att(ncid, v%varid, "missing_value", v%missing_value) )
                        end select 
                    end if 

                    if (v%FillValue_set) then
                        select case(trim(v%xtype))
                            case("NF90_INT")
                                call nc_check( nf90_put_att(ncid, v%varid, "FillValue", int(v%FillValue)) )
                            case("NF90_FLOAT")
                                call nc_check( nf90_put_att(ncid, v%varid, "FillValue", real(v%FillValue)) )
                            case("NF90_DOUBLE")
                                call nc_check( nf90_put_att(ncid, v%varid, "FillValue", v%FillValue) )
                        end select 
                    end if 

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
    subroutine nc_get_att(ncid, v, readmeta)

        implicit none

        integer :: ncid, stat, i
        double precision :: tmp, tmp2(2)
        integer :: tmpi, tmpi2(2) 

        character(len=NC_STRLEN) :: tmpstr
        type(ncvar) :: v

        integer :: ndims
        integer, allocatable :: dimids(:)

        integer, parameter :: noerr = NF90_NOERR

        logical, optional :: readmeta 
        logical :: read_meta 

        read_meta = .TRUE. 
        if (present(readmeta)) read_meta = readmeta 

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

            if ( read_meta ) then 
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

                select case(trim(v%xtype))
                    case("NF90_INT")

                        stat = nc_check_att( nf90_get_att(ncid, v%varid, "actual_range", tmpi2) )
                        if (stat .eq. noerr) v%actual_range = dble(tmpi2)

                        stat = nc_check_att( nf90_get_att(ncid, v%varid, "scale_factor", tmpi) )
                        if (stat .eq. noerr) v%scale_factor = dble(tmpi)

                        stat = nc_check_att( nf90_get_att(ncid, v%varid, "add_offset", tmpi) )
                        if (stat .eq. noerr) v%add_offset = dble(tmpi)

                        call nc_get_att_double(ncid,v%varid,"missing_value",v%missing_value,stat)
                        if (stat .eq. noerr) v%missing_set = .TRUE. 

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

                        call nc_get_att_double(ncid,v%varid,"missing_value",v%missing_value,stat)
                        if (stat .eq. noerr) v%missing_set = .TRUE. 

                        stat = nc_check_att( nf90_get_att(ncid, v%varid, "FillValue", tmp) )
                        if (stat .eq. noerr) then
                            v%FillValue = tmp
                            v%FillValue_set = .TRUE.
                        end if

                end select
            end if 

        end if

        return

    end subroutine nc_get_att

    subroutine nc_get_att_double(ncid,varid,name,val,stat)

        implicit none 

        integer :: ncid, varid 
        character(len=*) :: name  
        double precision :: val 
        integer :: stat, xtype, len 
        character(len=256) :: val_s 

        integer, parameter :: noerr = NF90_NOERR

        stat = nc_check_att( nf90_inquire_attribute(ncid, varid, name, xtype, len) )
        if (stat .eq. noerr) then

            select case(xtype)
                case(NF90_INT,NF90_FLOAT,NF90_DOUBLE,NF90_SHORT) 
                    stat = nf90_get_att(ncid, varid, trim(name), val)
                case(NF90_CHAR)
                    stat = nf90_get_att(ncid, varid, trim(name), val_s(1:len))
                    val = str_to_num(val_s(1:len)) 
                case DEFAULT
                    write(*,*) "GET_ATT: "//trim(name)//": ",xtype, len
            end select

        end if 

        return
    end subroutine nc_get_att_double 

    ! Return the size of a dimension in netcdf file
    function nc_size(filename,name,ncid)

        implicit none

        integer :: nc_size
        integer :: dimid, dimlen, stat
        character (len=*) :: filename, name

        integer, optional :: ncid 
        integer :: nc_id 

        ! Open the file if needed
        call nc_check_open(filename, ncid, nf90_nowrite, nc_id)

        !if ( name == "Time" .or. name == "time" ) then
        !    call nc_check( nf90_inquire(ncid, unlimitedDimId = dimid) )
        !else
        !    call nc_check( nf90_inq_dimid(ncid, name, dimid) )
        !end if
        call nc_check( nf90_inq_dimid(nc_id, name, dimid) )

        ! Get the dimension length and close the file
        call nc_check( nf90_inquire_dimension(nc_id, dimid, len=dimlen) )
        
        if (.not. present(ncid)) call nc_check( nf90_close(nc_id) )

        nc_size = dimlen

        return

    end function nc_size

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ c r e a t e
    ! Author     :  Alex Robinson
    ! Purpose    :  Create a new empty netcdf file
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_create(filename, author, creation_date, institution, description)

        implicit none 

        character(len=*) :: filename
        integer :: ncid, status
        character(len=1024) :: history
        character(len=*), optional, intent(in) :: author, creation_date, institution, description

        ! Get ncio version for writing
        write(history,"(a,f5.2)") "Dataset generated using ncio v", NCIO_VERSION

        ! Create the new empty file and close it (necessary to avoid errors with dim vars)
        status = nf90_create(trim(adjustl(filename)), nf90_clobber, ncid) 
        if (status .ne. NF90_NOERR) then
            write(*, *) "ncio :: error when creating file, no such file or directory? :: ",trim(filename)
            stop
        endif
        call nc_check( nf90_close(ncid) )

        if (present(author))        call nc_write_attr(filename, 'author', author)
        if (present(creation_date)) call nc_write_attr(filename, 'creation_date', creation_date)
        if (present(institution))   call nc_write_attr(filename, 'institution', institution)
        if (present(description))   call nc_write_attr(filename, 'description', description)

        write(*,"(a,a)") "ncio:: nc_create   :: ",trim(filename)
        
        return

    end subroutine nc_create

    subroutine nc_write_attr_global(filename,name,value,ncid)

        implicit none 

        character(len=*) :: filename, name, value 
        integer :: nc_id
        integer, optional, intent(in) :: ncid

        ! Open the file again and set for redefinition
        call nc_check_open(filename, ncid, nf90_write, nc_id)
        call nc_check( nf90_redef(nc_id) )

        call nc_check( nf90_put_att(nc_id, NF90_GLOBAL,trim(name),trim(value)) )

        ! End define mode and close the file.
        call nc_check( nf90_enddef(nc_id) )
        if (.not.present(ncid)) call nc_check( nf90_close(nc_id) )

        write(*,"(a,a)") "ncio:: nc_write_attr_global:: ", &
                              trim(filename)//" : "//trim(name)//" = "//trim(value)
        
        return

    end subroutine nc_write_attr_global

    subroutine nc_read_attr_global(filename,name,value, ncid)

        implicit none 

        character(len=*), intent(in) :: filename, name
        character(len=*), intent(out) :: value 
        integer, intent(in), optional :: ncid
        integer :: nc_id

        ! Open the file again and set for redefinition
        call nc_check_open(filename, ncid, nf90_nowrite, nc_id)
        call nc_check( nf90_get_att(nc_id, NF90_GLOBAL,trim(name), value) )
        if (.not.present(ncid)) call nc_check( nf90_close(nc_id) )
        
        return

    end subroutine nc_read_attr_global


    subroutine nc_write_attr_variable(filename,varname,name,value, ncid)

        implicit none 

        character(len=*) :: filename, varname, name, value 
        integer :: nc_id, varid, stat
        integer, parameter :: noerr = NF90_NOERR
        integer, intent(in), optional :: ncid

        ! Open the file again and set for redefinition
        call nc_check_open(filename, ncid, nf90_write, nc_id)
        call nc_check( nf90_redef(nc_id) )

        ! Inquire variable id and put attribute name
        stat = nf90_inq_varid(nc_id, trim(varname), varid)
        call nc_check( nf90_put_att(nc_id, varid,trim(name),trim(value)) )

        ! End define mode and close the file.
        call nc_check( nf90_enddef(nc_id) )
        if (.not.present(ncid)) call nc_check( nf90_close(nc_id) )

        write(*,"(a,a)") "ncio:: nc_write_attr:: ", &
                              trim(filename)//", "//trim(varname)//" : "//trim(name)//" = "//trim(value)
        
        return

    end subroutine nc_write_attr_variable

    subroutine nc_read_attr_variable(filename,varname,name,value, ncid)

        implicit none 

        character(len=*), intent(in) :: filename, varname, name
        character(len=*), intent(out) :: value 
        integer :: nc_id, varid, stat
        integer, optional :: ncid

        call nc_check_open(filename, ncid, nf90_nowrite, nc_id)
        stat = nf90_inq_varid(nc_id, trim(varname), varid)
        call nc_check( nf90_get_att(nc_id, varid ,trim(name), value) )
        if (.not.present(ncid)) call nc_check( nf90_close(nc_id) )
        
        return

    end subroutine nc_read_attr_variable

    subroutine nc_write_map(filename,name,lambda,phi,x_e,y_n, ncid)

        implicit none 

        character(len=*) :: filename, name

        integer :: nc_id, varid, stat
        integer, intent(in), optional :: ncid
        double precision, optional :: lambda, phi, x_e, y_n

        integer, parameter :: noerr = NF90_NOERR

        ! Open the file, set for redefinition
        call nc_check_open(filename, ncid, nf90_write, nc_id)
        call nc_check( nf90_redef(nc_id) )
        
        ! Check if grid mapping has been defined in this file
        ! (if not, define it according to input arguments)
        stat = nf90_inq_varid(nc_id, trim(name), varid)

        if ( stat .ne. noerr ) then
            ! Define the mapping variable as an integer with no dimensions,
            ! and include the grid mapping name
            call nc_check( nf90_def_var(nc_id, trim(name), NF90_INT, varid) )
            call nc_check( nf90_put_att(nc_id,varid, "grid_mapping_name", trim(name)) )

            ! Add grid attributes depending on grid_mapping type
            select case(trim(name))

                case("stereographic")
                    call nc_check( nf90_put_att(nc_id,varid, "longitude_of_projection_origin", lambda) )
                    call nc_check( nf90_put_att(nc_id,varid, "latitude_of_projection_origin", phi) )
                    call nc_check( nf90_put_att(nc_id,varid, "scale_factor_at_projection_origin", 1.d0) )
                    call nc_check( nf90_put_att(nc_id,varid, "false_easting",  x_e) )
                    call nc_check( nf90_put_att(nc_id,varid, "false_northing", y_n) )

                case("polar_stereographic")
                    call nc_check( nf90_put_att(nc_id,varid, "straight_vertical_longitude_from_pole", lambda) )
                    call nc_check( nf90_put_att(nc_id,varid, "latitude_of_projection_origin", phi) )
                    call nc_check( nf90_put_att(nc_id,varid, "scale_factor_at_projection_origin", 1.d0) )
                    call nc_check( nf90_put_att(nc_id,varid, "false_easting",  x_e) )
                    call nc_check( nf90_put_att(nc_id,varid, "false_northing", y_n) )

                case DEFAULT
                    ! Do nothing

            end select

            write(*,"(a,a,a)") "ncio:: nc_write_map:: ",trim(filename)//" : ",trim(name)
        
        end if 

        ! Close the file
        if (.not.present(ncid)) call nc_check( nf90_close(nc_id) )

        return 

    end subroutine nc_write_map

    !> Write an integer value (one point) dimension to a NetCDF file.
    !! @param filename name of the NetCDF file in which to define dimension
    !! @param name name of the dimension to be defined in NetCDF file
    !! @param x Fortran data type (point or vector) specifying values of dimension. 
    !! If nx is present and size(x)==1, x specifies the starting point of the dimension variable
    !! @param dx distance between each dimension value (optional)
    !! @param nx length of dimension variable (optional) 
    !! @param long_name NetCDF attribute, a long descriptive name of the variable (optional)
    !! @param standard_name NetCDF attribute specifying the CF convention standard name of the variable (optional)
    !! @param units NetCDF attribute of the units of the variable (optional)
    !! @param axis  NetCDF attribute of the standard axis of the variable (optional)
    !! @param calendar NetCDF attribute of the calendar type to be used for time dimensions (optional)

    subroutine nc_write_dim_int_pt(filename,name,x,dx,nx, &
                                     long_name,standard_name,units,axis,calendar,unlimited, ncid)

        implicit none

        integer :: i

        integer :: x
        integer, optional :: ncid
        integer, optional :: dx
        integer, optional :: nx
        double precision, allocatable :: xvec(:)
        double precision :: dx_tmp 

        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar

        logical, optional :: unlimited

        if (present(nx)) then
            allocate(xvec(nx))
            dx_tmp = 1.d0
            if (present(dx)) dx_tmp = dble(dx)
            do i = 1, nx 
                xvec(i) = x + (i-1)*dx_tmp 
            end do 
        else
            allocate(xvec(1))
            xvec(1) = x 
        end if

        call nc_write_dim_internal(filename,name,"NF90_INT",xvec, &
                                   long_name=long_name,standard_name=standard_name, &
                                   units=units,axis=axis,calendar=calendar,unlimited=unlimited,ncid=ncid)

        return

    end subroutine nc_write_dim_int_pt

    subroutine nc_write_dim_int_1D(filename,name,x, &
                                      long_name,standard_name,units,axis,calendar,unlimited,ncid)

        implicit none

        integer :: i
        integer, optional, intent(in) :: ncid

        integer :: x(:)
        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar 

        logical, optional :: unlimited

        call nc_write_dim_internal(filename,name,"NF90_INT",x=dble(x), &
                                   long_name=long_name,standard_name=standard_name, &
                                   units=units,axis=axis,calendar=calendar,unlimited=unlimited, ncid=ncid)

        return

    end subroutine nc_write_dim_int_1D 

    subroutine nc_write_dim_double_pt(filename,name,x,dx,nx, &
                                     long_name,standard_name,units,axis,calendar,unlimited,ncid)

        implicit none

        integer :: i
        integer, optional, intent(in) :: ncid

        double precision :: x
        double precision, optional :: dx
        integer, optional :: nx
        double precision, allocatable :: xvec(:)
        double precision :: dx_tmp 

        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar
        logical, optional :: unlimited

        if (present(nx)) then
            allocate(xvec(nx))
            dx_tmp = 1.d0
            if (present(dx)) dx_tmp = dx 
            do i = 1, nx 
                xvec(i) = x + (i-1)*dx_tmp 
            end do 
        else
            allocate(xvec(1))
            xvec(1) = x 
        end if

        call nc_write_dim_internal(filename,name,"NF90_DOUBLE",xvec, &
                                   long_name=long_name,standard_name=standard_name, &
                                   units=units,axis=axis,calendar=calendar,unlimited=unlimited, ncid=ncid)

        return

    end subroutine nc_write_dim_double_pt

    subroutine nc_write_dim_double_1D(filename,name,x, &
                                      long_name,standard_name,units,axis,calendar, unlimited ,ncid)

        implicit none

        integer :: i
        integer, optional, intent(in) :: ncid

        double precision :: x(:)
        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar
        logical, optional :: unlimited

        call nc_write_dim_internal(filename,name,"NF90_DOUBLE",x=dble(x), &
                                   long_name=long_name,standard_name=standard_name, &
                                   units=units,axis=axis,calendar=calendar,unlimited=unlimited, ncid=ncid)

        return

    end subroutine nc_write_dim_double_1D

    subroutine nc_write_dim_float_pt(filename,name,x,dx,nx, &
                                     long_name,standard_name,units,axis,calendar,unlimited,ncid)

        implicit none

        integer :: i
        integer, optional, intent(in) :: ncid

        real(4) :: x
        real(4), optional :: dx
        integer, optional :: nx
        double precision, allocatable :: xvec(:)
        double precision :: dx_tmp 

        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar

        logical, optional :: unlimited

        if (present(nx)) then
            allocate(xvec(nx))
            dx_tmp = 1.d0
            if (present(dx)) dx_tmp = dble(dx) 
            do i = 1, nx 
                xvec(i) = x + (i-1)*dx_tmp 
            end do 
        else
            allocate(xvec(1))
            xvec(1) = x 
        end if

        call nc_write_dim_internal(filename,name,"NF90_FLOAT",xvec, &
                                   long_name=long_name,standard_name=standard_name, &
                                   units=units,axis=axis,calendar=calendar,unlimited=unlimited, ncid=ncid)

        return

    end subroutine nc_write_dim_float_pt

    subroutine nc_write_dim_float_1D(filename,name,x, &
                                      long_name,standard_name,units,axis,calendar,unlimited,ncid)

        implicit none

        integer :: i
        integer, optional, intent(in) :: ncid

        real(4) :: x(:)
        character(len=*):: filename, name
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar
 
        logical, optional :: unlimited

        call nc_write_dim_internal(filename,name,"NF90_FLOAT",x=dble(x), &
                                   long_name=long_name,standard_name=standard_name, &
                                   units=units,axis=axis,calendar=calendar,unlimited=unlimited, ncid=ncid)

        return

    end subroutine nc_write_dim_float_1D

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Subroutine :  n c _ w r i t e _ d i m
    ! Author     :  Alex Robinson
    ! Purpose    :  Write a coordinate var to a netcdf file
    !               and make a new file if needed
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine nc_write_dim_internal(filename,name,xtype,x, &
                                     long_name,standard_name,units,axis,calendar,unlimited, ncid)

        ! Note: an error can occur: "NetCDF: Invalid dimension ID or name"
        ! This happens when writing a dimension after some data has already
        ! been written to the file. A warning is needed for this case.
        ! How can this be checked?
        ! ajr, 2013-09-29

        implicit none

        integer :: nc_id, i
        integer, optional, intent(in) :: ncid

        character(len=*):: filename, name, xtype
        double precision :: x(:)
        character(len=*), optional :: long_name, standard_name, units, axis
        character(len=*), optional :: calendar
        character(len=14) :: tmpchar

        logical, optional :: unlimited  
        logical :: unlimited_i

        type(ncvar) :: v

        ! By default do NOT define dimension as unlimited
        if (trim(name) .eq. "time") then
            unlimited_i = .TRUE.
        else
            unlimited_i = .FALSE.
        endif

        call nc_v_init(v,name=trim(name),xtype=xtype,coord=.TRUE.)

        !! Now fill in values of arguments that are present
        if ( present(long_name) )     v%long_name     = trim(long_name)
        if ( present(standard_name) ) v%standard_name = trim(standard_name)
        if ( present(units) )         v%units         = trim(units)
        if ( present(axis) )          v%axis          = trim(axis)
        if ( present(calendar) )      v%calendar      = trim(calendar) 
        if ( present(unlimited) )      unlimited_i    = unlimited

        !! Clear the variable dim vector and store/generate appropriate values
        if (allocated(v%dim)) deallocate(v%dim)

        v%n = size(x)
        allocate(v%dim(v%n))
        v%dim = x

        ! Get the range from the x values
        v%actual_range = (/ minval(v%dim), maxval(v%dim) /)
        v%add_offset   = 0.d0
        v%scale_factor = 1.d0

        ! END VARIABLE SETUP

        ! Open the file, set for redefinition
        call nc_check_open(filename, ncid, nf90_write, nc_id)
        call nc_check( nf90_redef(nc_id) )

        !! Define the variable in the file
        !if ( trim(v%name) .eq. "time" ) then
        if ( unlimited_i ) then
            call nc_check( nf90_def_dim(nc_id, trim(v%name), NF90_UNLIMITED, v%dimid) )
        else
            call nc_check( nf90_def_dim(nc_id, trim(v%name), v%n, v%dimid) )
        end if

        ! Assign attributes to coordinate variable.
        call nc_put_att(nc_id, v)

        ! End define mode.
        call nc_check( nf90_enddef(nc_id) )

        ! Put the variable's values in the file 
        call nc_check( nf90_put_var(nc_id, v%varid, v%dim) )

        ! Close the file
        if (.not. present(ncid)) call nc_check( nf90_close(nc_id) )

        tmpchar = trim(v%name)
        write(*,"(a,a,a14,i6)") "ncio:: nc_write_dim:: ",trim(filename)//" : ",adjustl(tmpchar),size(v%dim)
        
        return

    end subroutine nc_write_dim_internal

! ================================
!
!      INTEGERS 
!
! ================================
    
    !> Write an integer value (one point) to a NetCDF file.
    !! @param filename name of the NetCDF file in which to write data
    !! @param dat Fortran data type of data to be written
    !! @param name name of the variable in NetCDF file to be written
    !! @param dim1 name of first dimension of variable in NetCDF file
    !! @param dim2 name of second dimension of variable in NetCDF file (optional for variables < 2D)
    !! @param dim3 name of third dimension of variable in NetCDF file (optional for variables < 3D)
    !! @param dim4 name of fourth dimension of variable in NetCDF file (optional for variables < 4D)
    !! @param start vector of values specifying starting indices for reading data from each dimension (optional)
    !! @param count vector of values specifying how many values to read in each dimension (optional)
    !! @param long_name NetCDF attribute, a long descriptive name of variable (optional)
    !! @param standard_name NetCDF attribute specifying the CF convention standard name of the variable (optional)
    !! @param grid_mapping name of the grid this variable is mapped on (optional)
    !! @param units NetCDF attribute of the units of the variable (optional)
    subroutine nc_write_int_pt(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid 
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units


        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dble([dat]),"NF90_INT",shape([dat]), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_pt

    subroutine nc_write_int_1D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat(:)
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_1D

    subroutine nc_write_int_2D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat(:,:)
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_2D

    subroutine nc_write_int_3D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat(:,:,:)
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units

        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_3D

    subroutine nc_write_int_4D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat(:,:,:,:)
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_4D

    subroutine nc_write_int_5D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat(:,:,:,:,:)
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_5D

    subroutine nc_write_int_6D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        integer :: dat(:,:,:,:,:,:)
        integer, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_int=missing_value)

        return

    end subroutine nc_write_int_6D

! ================================
!
!      DOUBLES 
!
! ================================
    
    subroutine nc_write_double_pt(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units


        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dble([dat]),"NF90_DOUBLE",shape([dat]), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_pt

    subroutine nc_write_double_1D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat(:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_DOUBLE",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_1D

    subroutine nc_write_double_2D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat(:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_DOUBLE",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_2D

    subroutine nc_write_double_3D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat(:,:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units

        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_DOUBLE",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_3D

    subroutine nc_write_double_4D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat(:,:,:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_DOUBLE",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_4D

    subroutine nc_write_double_5D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat(:,:,:,:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_DOUBLE",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_5D

    subroutine nc_write_double_6D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        double precision :: dat(:,:,:,:,:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_DOUBLE",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_double=missing_value)

        return

    end subroutine nc_write_double_6D

! ================================
!
!      FLOATS 
!
! ================================
    
    subroutine nc_write_float_pt(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units


        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dble([dat]),"NF90_FLOAT",shape([dat]), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_pt

    subroutine nc_write_float_1D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat(:)
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_FLOAT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_1D

    subroutine nc_write_float_2D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat(:,:)
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_FLOAT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_2D

    subroutine nc_write_float_3D(filename,name,dat,dims,ncid,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat(:,:,:)
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units

        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_FLOAT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_3D

    subroutine nc_write_float_4D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat(:,:,:,:)
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_FLOAT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_4D

    subroutine nc_write_float_5D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat(:,:,:,:,:)
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_FLOAT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_5D

    subroutine nc_write_float_6D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        implicit none 

        ! Arguments
        real(4) :: dat(:,:,:,:,:,:)
        real(4), optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 

        allocate(dat1D(size(dat)))
        dat1D = pack(dble(dat),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_FLOAT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units, &
                                missing_value_float=missing_value)

        return

    end subroutine nc_write_float_6D

    ! ================================
    !
    !      LOGICALS
    !
    ! ================================

    subroutine nc_write_logical_pt(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        integer :: dati
         
        dati = 0
        if (dat) dati = 1  

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dble([dati]),"NF90_INT",shape([dat]), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_pt

    subroutine nc_write_logical_1D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat(:)

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 
        integer, allocatable :: dati(:) 
        
        allocate(dati(size(dat)),dat1D(size(dat)))
        dati = 0
        where (dat) dati = 1 
        dat1D = pack(dble(dati),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_1D

    subroutine nc_write_logical_2D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat(:,:)

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 
        integer, allocatable :: dati(:,:)

        allocate(dati(size(dat,1),size(dat,2)),dat1D(size(dat)))
        dati = 0
        where (dat) dati = 1 
        dat1D = pack(dble(dati),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_2D

    subroutine nc_write_logical_3D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat(:,:,:)

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 
        integer, allocatable :: dati(:,:,:)

        allocate(dati(size(dat,1),size(dat,2),size(dat,3)),dat1D(size(dat)))
        dati = 0
        where (dat) dati = 1 
        dat1D = pack(dble(dati),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_3D

    subroutine nc_write_logical_4D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat(:,:,:,:)

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 
        integer, allocatable :: dati(:,:,:,:)

        allocate(dati(size(dat,1),size(dat,2),size(dat,3),size(dat,4)),dat1D(size(dat)))
        dati = 0
        where (dat) dati = 1 
        dat1D = pack(dble(dati),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_4D

    subroutine nc_write_logical_5D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat(:,:,:,:,:)

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 
        integer, allocatable :: dati(:,:,:,:,:)

        allocate(dati(size(dat,1),size(dat,2),size(dat,3),size(dat,4),size(dat,5)))
        allocate(dat1D(size(dat)))
        dati = 0
        where (dat) dati = 1 
        dat1D = pack(dble(dati),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_5D

    subroutine nc_write_logical_6D(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                   long_name,standard_name,grid_mapping,units)

        implicit none 

        ! Arguments
        logical :: dat(:,:,:,:,:,:)

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        double precision, allocatable :: dat1D(:) 
        integer, allocatable :: dati(:,:,:,:,:,:)

        allocate(dati(size(dat,1),size(dat,2),size(dat,3),size(dat,4),size(dat,5),size(dat,6)))
        allocate(dat1D(size(dat)))
        dati = 0
        where (dat) dati = 1 
        dat1D = pack(dble(dati),.TRUE.)    ! Pack here to avoid segmentation faults!

        ! Finally call the internal writing routine
        call nc4_write_internal(filename,name,dat1D,"NF90_INT",shape(dat), &
                                ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6, &
                                start,count,long_name,standard_name,grid_mapping,units)

        return

    end subroutine nc_write_logical_6D

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
    !> Read an integer value (one point) from a NetCDF file.
    !! @param filename name of the file to read from
    !! @param dat Fortran data type into which data will be loaded
    !! @param name name of the variable in NetCDF file to be read
    !! @param start vector of values specifying starting indices for reading data from each dimension
    !! @param count vector of values specifying how many values to read in each dimension
    subroutine nc_read_int_pt(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat1D and store input data to faciliate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(1))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,[1],start,count,xtype=xtype, &
                                      missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(dat1D(1))

        return

    end subroutine nc_read_int_pt

    subroutine nc_read_int_1D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat1D and store input data to faciliate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(dat1D)

        return

    end subroutine nc_read_int_1D

    subroutine nc_read_int_2D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat4D and store input data to faciliate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_int_2D

    subroutine nc_read_int_3D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat1D and store input data to faciliate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_int_3D

    subroutine nc_read_int_4D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat1D and store input data to faciliate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_int_4D

    subroutine nc_read_int_5D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_int_5D

    subroutine nc_read_int_6D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        integer :: dat(:,:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        integer, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)*size(dat,6)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_int=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = int(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_int_6D

! ================================
!
!      DOUBLES 
!
! ================================
    
    subroutine nc_read_double_pt(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(1))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,[1],start,count,xtype=xtype, &
                                      missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(dat1D(1))

        return

    end subroutine nc_read_double_pt

    subroutine nc_read_double_1D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(dat1D)

        return

    end subroutine nc_read_double_1D

    subroutine nc_read_double_2D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_double_2D

    subroutine nc_read_double_3D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_double_3D

    subroutine nc_read_double_4D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_double_4D

    subroutine nc_read_double_5D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_double_5D

    subroutine nc_read_double_6D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        double precision :: dat(:,:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_DOUBLE"

        double precision, optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)*size(dat,6)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_double=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = dble(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_double_6D

! ================================
!
!      FLOATS
!
! ================================
    
    subroutine nc_read_float_pt(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(1))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,[1],start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(dat1D(1))

        return

    end subroutine nc_read_float_pt

    subroutine nc_read_float_1D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(dat1D)

        return

    end subroutine nc_read_float_1D

    subroutine nc_read_float_2D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_float_2D

    subroutine nc_read_float_3D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_float_3D

    subroutine nc_read_float_4D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_float_4D

    subroutine nc_read_float_5D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_float_5D

    subroutine nc_read_float_6D(filename,name,dat,start,count,missing_value, ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:), ncid
        
        !! Arguments related to data size and type
        real(4) :: dat(:,:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_FLOAT"

        real(4), optional :: missing_value 

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)*size(dat,6)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, &
                                       missing_value_float=missing_value, ncid=ncid)

        ! Store data that was read from file in output array
        dat = real(reshape(dat1D,shape(dat)))

        return

    end subroutine nc_read_float_6D

! ================================
!
!      LOGICALS 
!
! ================================

    subroutine nc_read_logical_pt(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(1))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,[1],start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        if (dat1D(1) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_pt

    subroutine nc_read_logical_1D(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat(:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(dat1D .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_1D

    subroutine nc_read_logical_2D(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat(:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(reshape(dat1D,shape(dat)) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_2D

    subroutine nc_read_logical_3D(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(reshape(dat1D,shape(dat)) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_3D

    subroutine nc_read_logical_4D(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(reshape(dat1D,shape(dat)) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_4D

    subroutine nc_read_logical_5D(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(reshape(dat1D,shape(dat)) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_5D

    subroutine nc_read_logical_6D(filename,name,dat,start,count,ncid)

        implicit none 

        double precision, dimension(:), allocatable :: dat1D

        ! Arguments
        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        
        !! Arguments related to data size and type
        logical :: dat(:,:,:,:,:,:)
        character(len=NC_STRLEN), parameter :: xtype    = "NF90_INT"

        ! Allocate dat1D and store input data to facilitate calling internal write subroutine
        if (allocated(dat1D)) deallocate(dat1D)
        allocate(dat1D(size(dat,1)*size(dat,2)*size(dat,3)*size(dat,4)*size(dat,5)*size(dat,6)))

        ! Finally call the internal writing routine
        call nc4_read_internal_numeric(filename,name,dat1D,shape(dat),start,count,xtype=xtype, ncid=ncid)

        ! Store data that was read from file in output array
        dat = .FALSE.
        where(reshape(dat1D,shape(dat)) .gt. 0.d0) dat = .TRUE.

        return

    end subroutine nc_read_logical_6D

! ================================
!
!      CHARACTERS 
!
! ================================

    subroutine nc_write_char_1D(filename,name,dat,ncid)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:)
        character (len=*) :: filename, name
        integer :: i 
        integer, intent(in), optional :: ncid

        ! Convert the character array into a long string
        string = trim(dat(1))
        do i = 2, size(dat)
            string = trim(string)//NC_STR_SEP//trim(dat(i))
        end do

        ! Finally call the internal writing routine
        call nc_write_internal_char(filename,name,string,ncid=ncid)

        return

    end subroutine nc_write_char_1D

    subroutine nc_write_char_2D(filename,name,dat, ncid)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:,:)
        character (len=*) :: filename, name
        integer :: i, j
        integer, intent(in), optional :: ncid

        ! Convert the character array into a long string
        write(*,"(a,a,a14)") "ncio:: nc_write_char:: ", &
                                  "warning: 2D character array could not be written: "//trim(name)
        return

    end subroutine nc_write_char_2D

    subroutine nc_write_char_3D(filename,name,dat, ncid)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:,:,:)
        character (len=*) :: filename, name
        integer :: i, j
        integer, intent(in), optional :: ncid

        ! Convert the character array into a long string
        write(*,"(a,a,a14)") "ncio:: nc_write_char:: ", &
                                  "warning: 3D character array could not be written: "//trim(name)
        return

    end subroutine nc_write_char_3D

    subroutine nc_write_char_4D(filename,name,dat, ncid)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:,:,:,:)
        character (len=*) :: filename, name
        integer :: i, j
        integer, intent(in), optional :: ncid

        ! Convert the character array into a long string
        write(*,"(a,a,a14)") "ncio:: nc_write_char:: ", &
                                  "warning: 4D character array could not be written: "//trim(name)
        return

    end subroutine nc_write_char_4D

    subroutine nc_read_char_1D(filename,name,dat,sep,ncid)

        implicit none 

        character(len=NC_STRLEN_MAX) :: string

        ! Arguments
        character(len=*) :: dat(:)
        character(len=*), optional :: sep 
        character (len=*) :: filename, name
        integer :: i, j, nsep 
        integer, intent(in), optional :: ncid

        character(len=10) :: separator 

        ! Determine the separator used to parse the elements of the
        ! array from the string saved in the netcdf file
        separator = trim(NC_STR_SEP)
        if (present(sep)) separator = trim(sep)
        nsep = len_trim(separator)

        ! Call the internal reading routine to get a long string
        call nc_read_internal_char(filename,name,string,ncid=ncid)

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

    subroutine nc_write_internal_char(filename,name,string, ncid)

        implicit none 

        character (len=*), intent(in) :: string
        character (len=*), intent(in) :: filename, name
        integer, intent(in), optional :: ncid
        character (len=256) :: dimname 

        type(ncvar) :: v

        ! netCDF needed counters, array, and names of dims
        integer :: nc_id, stat, dimid, str_len

        ! Initialize ncvar type
        call nc_v_init(v,trim(name),xtype="NF90_CHAR")

        dimname = trim(v%name)//"_len"
        str_len = len_trim(string) 

        ! Open the file
        call nc_check_open(filename, ncid, nf90_write, nc_id)

        ! Define / update the netCDF variable for the data.
        call nc_check( nf90_redef(nc_id) )
        call nc_check( nf90_def_dim(nc_id, trim(dimname), str_len, dimid) )
        call nc_check( nf90_def_var(nc_id, trim(v%name), NF90_CHAR, (/ dimid /), v%varid) )
        call nc_check( nf90_enddef(nc_id) )
        
        ! Write the data to the netcdf file
        ! (NF90 converts dat to proper type (int, real, dble)
        call nc_check( nf90_put_var(nc_id, v%varid, trim(string) ) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        if (.not. present(ncid)) call nc_check( nf90_close(nc_id) )

        !write(*,"(a,a,a14)") "ncio:: nc_write_char:: ",trim(filename)//" : ",trim(v%name)
        
        return 

    end subroutine nc_write_internal_char

    subroutine nc_read_internal_char(filename,name,string, ncid)

        implicit none 

        character (len=*) :: string
        character (len=*) :: filename, name

        type(ncvar) :: v

        ! netCDF needed counters, array, and names of dims
        integer, optional :: ncid
        integer :: nc_id, stat, dimid, str_len

        ! Initialize ncvar type
        call nc_v_init(v,trim(name),xtype="NF90_CHAR")

        ! Open the file if necessary
        call nc_check_open(filename, ncid, nf90_write, nc_id)
        call nc_get_att(nc_id,v) 

        ! Read the string from the netcdf file
        call nc_check( nf90_get_var(nc_id, v%varid, string(1:v%dlen(1))) )

        ! Close the file. This causes netCDF to flush all buffers and make
        ! sure your data are really written to disk.
        if (.not. present(ncid)) call nc_check( nf90_close(nc_id) )

        !write(*,"(a,a,a14)") "ncio:: nc_read_char:: ",trim(filename)//" : ",trim(v%name)
        
        return 

    end subroutine nc_read_internal_char

    function str_to_num(str)
        implicit none 
        character(len=*), intent(IN) :: str 
        character(len=100) :: tmpstr 
        integer :: stat, n
        double precision :: str_to_num, x 

        tmpstr = trim(adjustl(str))
        n      = len_trim(tmpstr)

        read(tmpstr(1:n),*,IOSTAT=stat) x

        str_to_num = 0
        if (stat .eq. 0) then 
            str_to_num = x 
        else
            n = len_trim(tmpstr)-1
            READ(tmpstr(1:n),*,IOSTAT=stat) x
            if (stat .ne. 0) then 
                write(*,*) "ncio::str_to_num:: ","Error converting string to number!"
                write(*,*) "|",trim(tmpstr),"|",n,stat,x
            else
                str_to_num = x 
            end if 
        end if 

        return 
    end function str_to_num 

end module ncio
