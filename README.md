
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

## NCIO API (documentation under construction)

**The NCIO API is properly documented in the GMDD manuscript found [here](http://www.geosci-model-dev.net/).

Below are the public functions available through the ncio module:

```fortran
nc_read
nc_read_attr
nc_size

nc_create
nc_write_attr
nc_write_map
nc_write_dim
nc_write

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
subroutine nc_read_attr(filename,[varname],name,value)
```
```
filename        name of the NetCDF data file to read from
varname         name of the variable of interest (optional, if not given global a global attribute is expected).
name            name of the variable or global attribute to be read
value           character value of the attribute read from the file
```

