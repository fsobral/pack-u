# Those unit tests use FRUIT version 3.4.3
# https://sourceforge.net/projects/fortranxunit/
#
# In order to make it works, it is necessary to follow the steps
#
# 1. Install Ruby
#
# 2. Install rake
#
# 3. Install fruit_processor_gem (see Fruit's README). It is possible
#    that you'll have to have superuser access
#
# 4. Copy file fruit.f90 to $(TESTS) directory (if not already there)
#
# 5. Type `make run`
#

# TODO: Move this variables to an upper Makefile
BUILD = $(PACKUDIR)

FC = gfortran

FOPT = -O2

# Add each test module in the current directory

TESTS_CODE = $(wildcard *_test.f90)

UNITTEST = unit_tests.bin

# Fruit specific files

FRUIT_CODE = fruit.f90
FRUIT_GEN_CODE = fruit_basket_gen.f90 fruit_driver_gen.f90


run: $(UNITTEST)
	./$(UNITTEST)

$(UNITTEST): $(FRUIT_CODE) $(TESTS_CODE) $(FRUIT_GEN_CODE)
	$(FC) $(FOPT) -I$(BUILD) -L$(BUILD) $^ -lpacku -o $@

fruit_basket_gen.f90 : $(TESTS_CODE)
	ruby fruit_generator.rb

fruit_driver_gen.f90 : $(TESTS_CODE)
	ruby fruit_generator.rb

# %_test.o: %_test.f90
# 	$(FC) $(FOPT) -I$(BUILD) -c $<

# fruit.o: fruit.f90
# 	$(FC) $(FOPT) $< -c

# fruit_basket_gen.o: fruit_basket_gen.f90
# 	$(FC) $(FOPT) $< -c

# fruit_driver_gen.o: fruit_driver_gen.f90
# 	$(FC) $(FOPT) -c $<

pythontests:
	export PYTHONPATH=$(PYTHONPATH):$(PACKUDIR)/src/python; \
	       python3 packu_tests.py

clean : 
	rm -f *.o *.mod *.obj *.bin
	rm -f $(FRUIT_GEN_CODE)
	rm -f result.xml result_tmp.xml

.PHONY : all clean run
