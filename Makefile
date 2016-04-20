.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make test       : compiles the test program test_ncio.x"
	@echo " make f2py       : compiles the ncio source for use as a Python module using f2py."
	@echo " make clean      : cleans object and executable files"
	@echo ""


objdir = .obj

# Command-line options at make call
ifort ?= 0
debug ?= 0 

## GFORTRAN OPTIONS (default) ##
FC = gfortran
LIB = /opt/local/lib
INC = /opt/local/include
# LIB = /usr/lib
# INC = /usr/include

FLAGS  = -I$(objdir) -J$(objdir) -I$(INC)
LFLAGS = -L$(LIB) -lnetcdff -lnetcdf

DFLAGS = -O3
ifeq ($(debug), 1)
    DFLAGS   = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all
endif

ifeq ($(ifort),1) 
	## IFORT OPTIONS ##
    FC = ifort 
    LIB = /home/robinson/apps/netcdf/netcdf/lib
    INC = /home/robinson/apps/netcdf/netcdf/include

	FLAGS        = -module $(objdir) -L$(objdir) -I$(INC)
	LFLAGS		 = -L$(LIB) -lnetcdf

	DFLAGS   = -O3
	ifeq ($(debug), 1)
	    DFLAGS   =-C -traceback -ftrapuv -fpe0 -check all
	    # -w 
	endif
endif

## Individual libraries or modules ##
$(objdir)/ncio.o: ncio.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Share library 
$(objdir)/ncio.so: ncio.f90
	$(FC) -c -shared -fPIC $(DFLAGS) $(FLAGS) -o ncio.so $<

## Complete programs

test: $(objdir)/ncio.o
	$(FC) $(DFLAGS) $(FLAGS) -o test_ncio.x $^ test_ncio.f90 $(LFLAGS)
	@echo " "
	@echo "    test_ncio.x is ready."
	@echo " "

test-extra: $(objdir)/ncio.o
	$(FC) $(DFLAGS) $(FLAGS) -o test_ncio2.x $^ extra/test_ncio2.f90 $(LFLAGS)
	@echo " "
	@echo "    test_ncio2.x is ready."
	@echo " "

compare: $(objdir)/ncio.o
	$(FC) $(DFLAGS) $(FLAGS) -o pres_temp_4D_wr.x $^ pres_temp_4D_wr_compare.f90 $(LFLAGS)
	@echo " "
	@echo "    pres_temp_4D_wr.x is ready."
	@echo " "

clean:
	rm -f test_ncio.x test_ncio2.x pres_temp_4D_wr.x $(objdir)/*.o $(objdir)/*.mod

