CC=g++
FC=gfortran
DEF=
LTP=-lthermopack_debug_gfortran_Linux
LIBS=$(LTP) -llapack -lblas -lstdc++

CFLAGS=-fPIC -cpp -Wall -g -mieee-fp -fsignaling-nans
CFLAGS_RELEASE=-fPIC -cpp -O3 -mieee-fp -fsignaling-nans

test: test.o
	$(FC) $(MAIN) test.o -o test -L../../bin $(LIBS)

release-test:
	$(MAKE) CFLAGS="$(CFLAGS_RELEASE)" \
	LTP=-lthermopack_optim_gfortran_Linux test

ifort-test:
	$(MAKE) test FC=ifort DEF=-D__INTEL_THERMOPACK__ \
	MAIN=-nofor_main \
	LTP=-lthermopack_debug_ifort_Linux

release-ifort-test:
	$(MAKE) FC=ifort CFLAGS="$(CFLAGS_RELEASE)" \
	DEF=-D__INTEL_THERMOPACK__ MAIN=-nofor_main \
	LTP=-lthermopack_optim_ifort_Linux \
	test

clean:
	 rm -f -r *.o *.a *.obj test

%.o: %.cpp
	$(CC) $(CFLAGS) $(DEF) -c -o $*.o $(<:.mod=.cpp)
