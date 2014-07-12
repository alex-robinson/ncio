
# NCIO: NetCDF Input-Output

Welcome to the ncio library.



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

