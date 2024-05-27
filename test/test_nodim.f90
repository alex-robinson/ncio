program test
   use ncio

   call nc_create("test.nc",overwrite=.true.,netcdf4=.true.)
   call nc_write("test.nc","int_var",1)

end program test
