# Configuration variables

PACKPROBLEM = packminc

TANGOLIB = 

# Create or modify the file below to set user-defined variables or
# change the above-defined variables

-include user_variables.mk

# Compiler options

CC  = gcc
FC  = gfortran
FCC = -O2

###########################################
# Modify below only if you are an expert! #
###########################################

# Directories

PACKUDIR = $(CURDIR)
BUILDDIR = $(PACKUDIR)/build
SRCDIR = $(PACKUDIR)/src
TESTDIR = $(PACKUDIR)/tests

export

lib: libpacku.a

libpacku.a: packdat.o items.o containers.o
	ar crv $@ $^

packu: $(PACKPROBLEM).o packdat.o
	gfortran $^ -L$(TANGOLIB) -lalgencan -o $@
	mkdir -p $(BUILDDIR)
	mv -f $@ $(BUILDDIR)/.

tests: lib python_tests
	$(MAKE) -C $(TESTDIR) clean run

python_tests:
	$(MAKE) -C $(TESTDIR) pythontests

%.o: %.f90
	gfortran -c $^

packminc.o: packminc.f90 packdat.mod
	gfortran -c $<

packminw.o: packminw.f90 packdat.mod
	gfortran -c $<

packdat.mod: packdat.f90
	gfortran -c $^

items.mod: items.f90
	gfortran -c $^

containers.mod: containers.f90 items.mod 
	gfortran -c $<

clean:
	rm -f *.mod *.o *.a solution.csv solution.pdf \
	sol[0-9][0-9][0-9].* build/*
	$(MAKE) -C $(TESTDIR) clean
	$(MAKE) -C $(SRCDIR) clean
