TANGOLIB = /opt/tango/algencan-3.0.0/lib

PACKPROBLEM = packminc

pack: $(PACKPROBLEM).o packdat.o
	gfortran $^ -L$(TANGOLIB) -lalgencan -o $@

%.o: %.f90
	gfortran -c $^

packminc.o: packminc.f90 packdat.mod
	gfortran -c $<

packminw.o: packminw.f90 packdat.mod
	gfortran -c $<

packdat.mod: packdat.f90
	gfortran -c $^

clean:
	rm -f *.mod *.o solution.csv solution.pdf \
	sol[0-9][0-9][0-9].* pack
