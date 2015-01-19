
# NCIO: NetCDF Input-Output

Welcome to the ncio library.

## Test programs

Two example programs are avaiable to test the functionality of ncio:
```test_ncio.f90``` and ```pres_temp_4D_wr_compare.f90```. These programs can be compiled using the Makefile and run from the command line (see below). The user may need to modify the Makefile so that the locations of the NetCDF library installed on their machine are properly referenced. These are currently defined as:

```
LIB = /opt/local/lib
INC = /opt/local/include
```

Find these lines in the Makefile and modify as needed. 

### test_ncio.f90 

```
make test 
./test_ncio.x 
```

This program is an example of how to simply write various size arrays and dimensions to an output netcdf file (out_ncio.nc). Generally the program writes checkerboard pattern data to the file highlighting different functionality of the ncio library. 

### pres_temp_4D_wr_compare.f90 

```
make compare 
./pres_temp_4D_wr.x
./pres_temp_4D_wr.x 10 
```

This program is identical to the example code provided by Unidata [pres_temp_4D_wr.f90](http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial), however it includes the same example using the ncio library, and using the ncio library and maintaining the NetCDF file open throughout the program via an `ncid` variable.

The comparison program can run multiple loops to test timing. In the above example, the first program call will run through the program and calculate the timing for 1 iteration, the second program call will calculate the timing for 10 iterations. A timing table is output at the end of the program. 

## NCIO API documentation

**The NCIO API is properly documented in the GMDD manuscript found [here](http://www.geosci-model-dev.net/), however a summary of the public functions available through the ncio module can be found below:

```
nc_read
nc_size
nc_read_attr

nc_create
nc_write_attr
nc_write_map
nc_write_dim
nc_write

nc_open
nc_close
```

The first two are useful for reading variable attributes and data from an existing NetCDF file. The remaining functions are used for creating and writing to NetCDF files. 

```fortran
subroutine nc_read(filename,dat,name,[start],[count],[missing value])
```
```
filename        name of the NetCDF data file to read from
dat             Fortran data type into which data will be loaded
name            name of the variable in NetCDF file to be read
start           vector of values specifying starting indices for reading data from each dimension (optional) 
count           vector of values specifying how many values to read in each dimension (optional)
missing value   Value to assign to missing data read from the file (optional)
```

```fortran
function nc_size(filename,name,[ncid]) result(size)
```
```
filename        name of the NetCDF data file to read from
name            name of the dimension variable in NetCDF file of which to determine size
size            Integer size (length) of the dimension variable returned from the function
ncid            File ID for a file that remains open for various NCIO calls (optional)
```

```fortran
subroutine nc_read_attr(filename,[varname],name,value)
```
```
filename        name of the NetCDF data file to read from
varname         name of the variable of interest (optional, if not given global a global attribute is expected).
name            name of the variable or global attribute to be read
value           character value of the attribute read from the file
ncid            File ID for a file that remains open for various NCIO calls (optional)
```

```fortran
subroutine nc_create(filename,[author],[creation\_date],[institution],[description])
```
```
filename        name of the NetCDF file to be created
author          name of the author of the file (optional)
creation_date   date of the file creation, string format (optional)
institution     name of the author's institution (optional)
```

```fortran
subroutine nc_write_attr(filename,[varname],name,value,[ncid])
```
```
filename        name of the NetCDF file in which to write attribute
varname         name of the variable in which to write the attribute (optional)
name            name of the attribute to be written
value           value of the attribute to be written
ncid            File ID for a file that remains open for various NCIO calls (optional)
```

```fortran
subroutine nc_write_map(filename,name,[lambda],[phi],[x_e],[y_n],[ncid])
```
```
filename        name of the NetCDF file in which to write the grid map definition
name            name of the grid mapping to be defined
lambda          longitude of projection origin (optional)
phi             latitude of projection origin (optional)
x_e             false easting (optional)
y_n             false northing (optional)
ncid            File ID for a file that remains open for various NCIO calls (optional)
```

```fortran
subroutine nc_write_dim(filename,name,[x],[dx],[nx],[long_name],[standard_name],[units],[axis],[calendar],[ncid])
```
```
filename        name of the NetCDF file in which to define dimension
name            name of the dimension to be defined in NetCDF file
x               Fortran data type (scalar or vector) specifying values of dimension. If nx is present and size(x)==1, x specifies the starting point of the dimension variable
dx              distance between each dimension value (optional)
nx              length of dimension variable (optional)
long_name       NetCDF attribute, a long descriptive name of the variable (optional)
standard_name   NetCDF attribute specifying the CF convention standard name of the variable (optional)
units           NetCDF attribute of the units of the variable (optional)
axis            NetCDF attribute of the standard axis of the variable (optional)
calendar        NetCDF attribute of the calendar type to be used for time dimensions (optional)
ncid            File ID for a file that remains open for various NCIO calls (optional)
```

```fortran
subroutine nc_write(filename,name,dat,[dims],[dim1,...,dim6],[start],[count],[long_name],[standard_name],[grid_mapping],[units],[missing_value],[ncid])
```
```
filename        name of the NetCDF file in which to write data
name            name of the variable in NetCDF file to be written
dat             Fortran data type of data to be written
dims            vector of dimension names of the variable in NetCDF file (optional)
dim1,...,dim6   individual dimension names of the variable in NetCDF file (optional)
start           vector of values specifying starting indices for reading data from each dimension (optional)
count           vector of values specifying how many values to read in each dimension (optional)
long_name       NetCDF attribute, a long descriptive name of variable (optional)
standard_name   NetCDF attribute specifying the CF convention standard name of the variable (optional)
grid_mapping    name of the grid this variable is mapped on (optional)
units           NetCDF attribute of the units of the variable (optional)
missing_value   Value of missing data to be written to file (optional)
ncid            File ID for a file that remains open for various NCIO calls (optional) 
```

```fortran
subroutine nc_open(filename,ncid,[writable])
```
```
filename        name of the NetCDF file from which to read attribute
ncid            integer variable to identify a NetCDF file through multiple NCIO calls
writable        Switch to determine whether file should be opened for writing (optional, default TRUE)
```

```fortran
subroutine nc_close(ncid)
```
```
ncid            integer variable to identify a NetCDF file to be closed
```
