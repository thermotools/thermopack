CC=g++
FC=gfortran
DEF=
UNAME := $(shell uname 2>/dev/null || echo Unknown)
LTP=-lthermopack_debug_gfortran_$(UNAME)
LIBS=$(LTP) -llapack -lblas -lstdc++

CFLAGS=-fPIC -cpp -Wall -g -fsignaling-nans
CFLAGS_RELEASE=-fPIC -cpp -O3 -fsignaling-nans

test: test.o
	$(FC) $(MAIN) test.o -o test -L../../bin $(LIBS)

release-test:
	$(MAKE) CFLAGS="$(CFLAGS_RELEASE)" \
	LTP=-lthermopack_optim_gfortran_$(UNAME) test

ifort-test:
	$(MAKE) test FC=ifort DEF=-D__INTEL_THERMOPACK__ \
	MAIN=-nofor_main \
	LTP=-lthermopack_debug_ifort_$(UNAME)

release-ifort-test:
	$(MAKE) FC=ifort CFLAGS="$(CFLAGS_RELEASE)" \
	DEF=-D__INTEL_THERMOPACK__ MAIN=-nofor_main \
	LTP=-lthermopack_optim_ifort_$(UNAME) \
	test

clean:
	 rm -f -r *.o *.a *.obj test

%.o: %.cpp
	$(CC) $(CFLAGS) $(DEF) -c -o $*.o $(<:.mod=.cpp)
