.PHONY: clean tests

# Decide the file extensions.
ifdef $(PFUNIT)
include $(PFUNIT)/include/extensions.mk
else
OBJ_EXT = .o
endif

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))
OBJ_DIR = ../$(ODIR)

# To avoid deletion of intermediate files:
.SECONDARY: $(SRCS:.pf=.F90)

EXTRA_FLAGS = -I$(OBJ_DIR) $(INCDIR) -I$(PFUNIT)/mod
ifeq ($(FC),nagfor)
   EXTRA_FLAGS += -fpp
endif

tests: testSuites.inc $(OBJS)

testSuites.inc: $(SRCS)

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%$(OBJ_EXT) %.mod: %.F90
	$(FC) -c $(FFLAGS) $(EXTRA_FLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod
