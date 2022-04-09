CC=g++
CFLAGS=-fPIC -cpp -Wall -g -mieee-fp -fsignaling-nans -D_GLIBCXX_USE_CXX11_ABI=0
#CFLAGS=-fPIC -cpp -O3 -funroll-loops -D_GLIBCXX_USE_CXX11_ABI=0

test: test.o
	$(CC) $(FFLAGS) test.o -o test -L../../bin -lthermopack_debug_gfortran_Linux -llapack -lblas -lgfortran

clean:
	 rm -f -r *.o *.a *.obj

%.o: %.cpp
	$(CC) $(CFLAGS) -c -o $*.o $(<:.mod=.cpp)
#$< -o $@
