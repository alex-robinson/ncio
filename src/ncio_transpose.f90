module ncio_transpose
    
    use ncio 

    implicit none 

    interface nc_write_t
        module procedure    nc_write_double_2D_t
        module procedure    nc_write_double_3D_t
    end interface

    interface nc_read_t
        module procedure    nc_read_double_2D_t
        module procedure    nc_read_double_3D_t
    end interface

    private
    public :: nc_read_t
    public :: nc_write_t 

contains 

    subroutine nc_read_double_2D_t(filename,name,dat,start,count,missing_value, ncid, iostat)
        ! read [i,j] into [j,i]

        implicit none

        character (len=*) :: filename, name
        double precision  :: dat(:,:)
        integer, optional :: start(:), count(:), ncid
        integer, optional :: iostat
        double precision, optional :: missing_value

        ! Local variables
        double precision, allocatable :: dat_t(:,:)
        integer :: i, j, ni, nj 

        ni = size(dat,2)
        nj = size(dat,1)

        allocate(dat_t(ni,nj))

        ! Load data with right order dimensions (x,y)
        call nc_read(filename,name,dat_t,start,count,missing_value, ncid, iostat)

        ! Transpose data for output
        do i = 1, ni 
        do j = 1, nj
            dat(j,i) = dat_t(i,j)
        end do 
        end do 
         
        return 

    end subroutine nc_read_double_2D_t

    subroutine nc_read_double_3D_t(filename,name,dat,start,count,missing_value, ncid, iostat)
        ! read [i,j,k] into [k,j,i]

        implicit none

        character (len=*) :: filename, name
        double precision  :: dat(:,:,:)
        integer, optional :: start(:), count(:), ncid
        integer, optional :: iostat
        double precision, optional :: missing_value

        ! Local variables
        double precision, allocatable :: dat_t(:,:,:)
        integer :: i, j, k, ni, nj, nk 

        ni = size(dat,3)
        nj = size(dat,2)
        nk = size(dat,1)
        
        allocate(dat_t(ni,nj,nk))

        ! Load data with right order dimensions (x,y,time)
        call nc_read(filename,name,dat_t,start,count,missing_value, ncid, iostat)

        ! Transpose data for output
        
        do i = 1, ni 
        do j = 1, nj
        do k = 1, nk 
            dat(k,j,i) = dat_t(i,j,k)
        end do 
        end do
        end do 
            
        return 

    end subroutine nc_read_double_3D_t

    subroutine nc_write_double_2D_t(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)
        ! Write [j,i] to [i,j]
        
        implicit none 

        ! Arguments
        double precision :: dat(:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        
        ! Local variables 
        double precision, allocatable :: dat_t(:,:) 
        integer :: i, j, ni, nj 

        ni = size(dat,2)
        nj = size(dat,1) 

        allocate(dat_t(ni,nj))

        ! Transpose data for writing 
        do i = 1, ni 
        do j = 1, nj 
            dat_t(i,j) = dat(j,i) 
        end do 
        end do

        ! Write the data like normal 
        call nc_write(filename,name,dat_t,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        return 

    end subroutine nc_write_double_2D_t

    subroutine nc_write_double_3D_t(filename,name,dat,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)
        ! Write [k,j,i] to [i,j,k]

        implicit none 

        ! Arguments
        double precision :: dat(:,:,:)
        double precision, optional :: missing_value

        character (len=*) :: filename, name
        integer, optional :: start(:), count(:),ncid
        character (len=*), optional :: dims(:), dim1, dim2, dim3, dim4, dim5, dim6
        character (len=*), optional :: long_name, standard_name, grid_mapping, units
        
        ! Local variables 
        double precision, allocatable :: dat_t(:,:,:) 
        integer :: i, j, k, ni, nj, nk 

        ni = size(dat,3)
        nj = size(dat,2) 
        nk = size(dat,1)

        allocate(dat_t(ni,nj,nk))

        ! Transpose data for writing 
        do i = 1, ni 
        do j = 1, nj 
        do k = 1, nk 
            dat_t(i,j,k) = dat(k,j,i) 
        end do 
        end do

        end do 

        ! Write the data like normal [i,j,k]
        call nc_write(filename,name,dat_t,ncid,dims,dim1,dim2,dim3,dim4,dim5,dim6,start,count, &
                                  long_name,standard_name,grid_mapping,units,missing_value)

        return 

    end subroutine nc_write_double_3D_t

end module ncio_transpose 
