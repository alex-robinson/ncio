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

ifort ?= 0
debug ?= 0 

ifeq ($(ifort),1)
    FC = ifort 
    LIB = /home/robinson/apps/netcdf/netcdf/lib
    INC = /home/robinson/apps/netcdf/netcdf/include
else
    FC = gfortran
    #LIB = /usr/lib
    #INC = /usr/include
    LIB = /opt/local/lib
    INC = /opt/local/include
endif 

ifeq ($(ifort),1)
	## IFORT OPTIONS ##
	FLAGS        = -module $(objdir) -L$(objdir) -I$(INC)
	LFLAGS		 = -L$(LIB) -lnetcdf

	ifeq ($(debug), 1)
	    DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0
	    # -w 
	else
	    DFLAGS   = -vec-report0 -O3
	endif
else
	## GFORTRAN OPTIONS ##
	FLAGS        = -I$(objdir) -J$(objdir) -I$(INC)
	LFLAGS		 = -L$(LIB) -lnetcdff -lnetcdf

	ifeq ($(debug), 1)
	    DFLAGS   = -w -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all
	else
	    DFLAGS   = -O3
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

