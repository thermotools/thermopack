#=============================================================================
#
# A Makefile with compiler and operative-system options.
# The objects and libraries are defined in Makefile.code.
#
#-----------------------------------------------------------------------------
# KYL 2009-03-16: Created a new, more generic system
#
#=============================================================================
# Set some general variables
#=============================================================================
PROC = $(shell arch)

# Detect OS into UNAME
UNAME := $(shell uname 2>/dev/null || echo Unknown)
UNAME := $(patsubst CYGWIN%,Cygwin,$(UNAME))
UNAME := $(patsubst MSYS%,MSYS,$(UNAME))
UNAME := $(patsubst MINGW%,MSYS,$(UNAME))

# Reduce flavors of OS
OSTYPE := Unix
ifeq ($(UNAME),MSYS)
  OSTYPE := MINGW
else ifeq ($(UNAME),MINGW)
  OSTYPE := MINGW
endif

export OSTYPE
export UNAME
export PROC

modes = debug profile normal optim r16 debug_irg openmp openmpprofile
targets =
compilers =
default = gfortran

#=============================================================================
# Define some special targets
#=============================================================================
.PHONY: clean help

help:
	@echo "Usage: make <target>"
	@echo "<target> is one of the following:"
	@echo
	@echo "  help"
	@echo "  clean"
	@echo -e " $(foreach target,$(sort $(targets)), $(target)\n)"
	@echo "The default compiler is $(default)."
	@echo "OS = $(UNAME), PROC = $(PROC)"
	@echo "Compilers: $(compilers)"

clean:
	$(MAKE) -f Makefile.code clean

all: all_debug all_optim all_openmp profile_ifort_Linux

all_debug:
	$(MAKE) $(filter debug_%, $(targets_filtered))

all_optim:
	$(MAKE) $(filter optim_%, $(targets_filtered))

all_openmp:
	$(MAKE) $(filter openmp_%, $(targets_filtered))

unittests_all: unittests_all_debug unittests_all_optim unittests_all_openmp

unittests_all_debug:
	$(MAKE) $(filter unittests_debug_%, $(targets_filtered))

unittests_all_optim:
	$(MAKE) $(filter unittests_optim_%, $(targets_filtered))

unittests_all_openmp:
	$(MAKE) $(filter unittests_openmp_%, $(targets_filtered))

#=============================================================================
# Setup the compiler list and define the compiler flags
#=============================================================================
# Targets will be automatically created based on the "OSTYPE" variable, the
# "compilers" list and the "modes" list. Only the targets for the current
# "OSTYPE"
# is available. Each real target is defined by specifying the compiler flags in
# a variable $(mode)_$(compiler)_flags.
ifeq ($(OSTYPE),Unix)
  compilers += gfortran

  # Define gfortran flags
  gf_common := -cpp -fPIC -fdefault-real-8 -fdefault-double-8 -frecursive
  ifeq ($(PROC),arm64)
    gf_proc = -arch arm64
    gf_march = -arch arm64
  else
    gf_proc = -mieee-fp
    gf_march = -march=x86-64 -msse2
  endif
  debug_gfortran_flags = "$(gf_proc) $(gf_common) \
                          -g -fbounds-check -fbacktrace \
                          -ffpe-trap=invalid,zero,overflow \
                          -Wno-unused-dummy-argument -Wall" \
                         NOWARN_FFLAGS="-Wno-all"
  profile_gfortran_flags = "$(gf_common) -g -pg"
  normal_gfortran_flags  = "$(gf_common)"
  optim_gfortran_flags   = "$(gf_common) -O3 $(gf_march) -funroll-loops"
  openmp_gfortran_flags  = $(optim_gfortran_flags)" -fopenmp -frecursive"
  openmpprofile_gfortran_flags = "$(gf_common) -fopenmp -frecursive -gomp"

  # Define ifort flags
  ifneq ($(shell command -v ifort 2> /dev/null),)
    # Make script freeze when runnign "ifort --version" without contact to licence server
    ifneq ($(shell timeout 0.2 ifort --version 2> /dev/null),)
      compilers += ifort
      omp_ifort = "-openmp"
      ifort_is_bleeding := $(shell expr `ifort --version 2>/dev/null \
                           | grep -o "[0-9]\.[0-9]\.[0-9]" | tail -1` \
                           \>= 18.0.0 2>/dev/null)
      ifeq ($(ifort_is_bleeding),1)
        omp_ifort = "-qopenmp"
      endif
    endif
  endif

  debug_ifort_flags   = "-fpp -r8 -fpe0 -g -fp-model precise -warn all \
                         -check all,noarg_temp_created,nopointers \
                         -traceback -ftz -auto -fPIC -no-wrap-margin" \
                         NOWARN_FFLAGS="-warn none"
  profile_ifort_flags = "-fpp -r8 -fpe0 -g -pg -auto -fPIC -no-wrap-margin"
  normal_ifort_flags  = "-fpp -r8 -fpe0 -warn all -auto -fPIC -no-wrap-margin"
  optim_ifort_flags   = "-fpp -r8 -O3 -ax -fp-model precise -fpe0 -w -ftz -auto -check uninit -fPIC -no-wrap-margin"
  r16_ifort_flags     = "-fpp -r16 -fpe0 -warn all -auto -fPIC -no-wrap-margin"
  openmp_ifort_flags = "-fpp $(omp_ifort) -r8 -O3 -ax -fp-model precise -fpe0 -w -ftz -auto -fPIC -no-wrap-margin"

else ifeq ($(OSTYPE),MINGW)
  compilers += gfortran

  # Define gfortran flags
  gf_common := -cpp -fPIC -fdefault-real-8 -fdefault-double-8
  debug_gfortran_flags  = "$(gf_common) -fbounds-check -Wall -g \
                           -ffpe-trap=invalid,zero,overflow -mieee-fp"
  normal_gfortran_flags = "$(gf_common)"
  optim_gfortran_flags = "$(gf_common) -O3 -funroll-loops"
endif

#=============================================================================
# Define the targets
#=============================================================================
# 'create_targets_phony' is a canned sequence that defines phony targets. For
# instance, it links the phony target "debug_gfortran" to the actual target
# "debug_gfortran_Linux".
#
# 'create_targets_real' is a canned sequence that defines real targets, such as
# "debug_gfortran_Linux".
#
# Note:
# * When the corresponding flags for the specified compiler and mode are not
#   defined, the targets will not be created.
# * When a target is created, it is automatically added to the list of targets.

define create_targets_default
  ifdef $(1)_$(default)_flags
    targets += $(1) unittests_$(1)
$(1): $(1)_$(default)
unittests_$(1): unittests_$(1)_$(default)
  endif
endef

define create_targets_phony
  ifdef $(1)_$(2)_flags
    targets += $(1)_$(2) unittests_$(1)_$(2)
$(1)_$(2): $(1)_$(2)_$(UNAME)
unittests_$(1)_$(2): unittests_$(1)_$(2)_$(UNAME)
  endif
endef

define create_targets_real
  ifdef $(2)_$(3)_flags
    targets += $(2)_$(3)_$(1) unittests_$(2)_$(3)_$(1)
$(2)_$(3)_$(1):
	+$(MAKE) FC=$(3) MODE=$(2) FFLAGS=$($(2)_$(3)_flags) \
	  CFLAGS=$($(2)_cpp) TARGET=$$@ LEXT=$(3)_$(1) \
	  -f Makefile.code -e thermopack
unittests_$(2)_$(3)_$(1):
	+$(MAKE) FC=$(3) MODE=$(2) FFLAGS=$($(2)_$(3)_flags) \
	  TARGET=$(2)_$(3)_$(1) LEXT=$(3)_$(1) \
	  -f Makefile.code -e run_unittests
  endif
endef


$(foreach mode,$(modes),\
  $(eval $(call create_targets_default,$(mode))))

$(foreach mode,$(modes),$(foreach comp,$(compilers),\
  $(eval $(call create_targets_phony,$(mode),$(comp)))\
  $(eval $(call create_targets_real,$(UNAME),$(mode),$(comp)))))


# Create list of filtered targets for the "all" family of targets
targets_filtered := $(filter-out %_$(UNAME), $(targets))
