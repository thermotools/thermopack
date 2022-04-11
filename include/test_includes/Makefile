CC=g++
FC=gfortran
DEF=
LTP=-lthermopack_debug_gfortran_Linux
LIBS=$(LTP) -llapack -lblas -lstdc++

CFLAGS=-fPIC -cpp -Wall -g -mieee-fp -fsignaling-nans $(DEF)

test: test.o
	$(FC) $(MAIN) test.o -o test -L../../bin $(LIBS)

clean:
	 rm -f -r *.o *.a *.obj test

%.o: %.cpp
	$(CC) $(CFLAGS) -c -o $*.o $(<:.mod=.cpp)

ifort-test:
	$(MAKE) test FC=ifort DEF=-D__INTEL_THERMOPACK__ MAIN=-nofor_main \
	LTP=-lthermopack_debug_ifort_Linux
