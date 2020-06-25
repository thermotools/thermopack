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
ifeq ($(shell uname -o),Msys)
  OS = MINGW32
else
  OS := $(shell uname -s)
endif

bit64 := $(shell uname -a | grep x86_64 | wc -c)
ifeq ($(bit64),0)
  PROC = i686
else
  PROC = x86_64
endif

modes     = debug profile normal optim r16 debug_irg openmp openmpprofile
targets   = help clean
compilers = dummy
default   = gfortran

ifeq ($(OS),OSF1)
  default = OSF1
endif

gfortran_is_bleeding := $(shell expr `gfortran --version \
                          | grep -o "[0-9]\.[0-9]\.[0-9]" | tail -1` \
                          \>= 4.6.2)
ifeq ($(gfortran_is_bleeding),1)
  extra_flags_gfortran = "-fdefault-double-8"
else
  extra_flags_gfortran = ""
endif

#=============================================================================
# Define architecture spesific flags
#=============================================================================
# Note: march1 is march settings for gfortran
march1=-march=i686 -msse2
ifneq ($(OS),MINGW32)
  ifneq ($(shell cat /proc/cpuinfo|grep Opteron),)
    march1=-march=opteron -msse3
  else ifeq ($(PROC),x86_64)
    march1=-march=native -msse3
  endif
endif

#=============================================================================
# Define some special targets
#=============================================================================
.PHONY: clean help

help:
	@echo "Usage: make <target>"
	@echo "<target> is one of the following:"
	@echo
	@echo -e " $(foreach target,$(targets),  $(target)\n)"
	@echo " The default compiler is $(default)."
	@echo "OS = $(OS), PROC = $(PROC)"
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
# - Based on the operating system reported by uname, a list of
#   compilers will be made available through the variables
#   default, compilers and modes. 'default' is the default
#   compiler (the one used when only typing e.g. 'make debug'),
#   compilers is a list of target compilers and modes is a list
#   of compile modes.
# - For each target specified by a combination of the compilers
#   and modes, there is also defined a variable that contains
#   the compiler flags: <mode>_<compiler>_flags. This must be
#   defined if the target is to be created. For OSF1 (and similar
#   OSes that might be added in the future), the flags are in
#   variables named: <mode>_<OS>_flags.
#
ifeq ($(OS),Linux)
  compilers += gfortran

  # Define gfortran flags
  debug_gfortran_flags   = "-cpp -fPIC -fdefault-real-8 $(extra_flags_gfortran) -g -fbounds-check -fbacktrace\
			    -ffpe-trap=invalid,zero,overflow \
			    -mieee-fp -Wno-unused-dummy-argument -Wall" \
			    NOWARN_FFLAGS="-Wno-all"
  profile_gfortran_flags = "-cpp -fPIC -fdefault-real-8 $(extra_flags_gfortran) -g -pg"
  normal_gfortran_flags  = "-cpp -fPIC -fdefault-real-8 $(extra_flags_gfortran)"
  optim_gfortran_flags   = "-cpp -fPIC -fdefault-real-8 $(extra_flags_gfortran) -O3 $(march1) \
			    -funroll-loops"
  openmp_gfortran_flags  = "-cpp -fPIC -fdefault-real-8 $(extra_flags_gfortran) -O3 $(march1) -funroll-loops -fopenmp -frecursive"
  #openmp_gfortran_flags  = "-fdefault-real-8 $(extra_flags_gfortran) -fopenmp -frecursive"
  openmpprofile_gfortran_flags = "-cpp -fPIC -fdefault-real-8 $(extra_flags_gfortran) -fopenmp -frecursive -gomp"

  # Define ifort flags
  ifneq ($(shell command -v ifort 2> /dev/null),)
    compilers += ifort
    omp_ifort = "-openmp"
    ifort_is_bleeding := $(shell expr `ifort --version 2>/dev/null \
                           | grep -o "[0-9]\.[0-9]\.[0-9]" | tail -1` \
                           \>= 18.0.0 2>/dev/null)
    ifeq ($(ifort_is_bleeding),1)
      omp_ifort = "-qopenmp"
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

  # Set some processor specific flags (e.g. 64bit flags)
  # Notes:
  # * For optim_ifort: -axN ? (is also compatible with generic processor)
  ifeq ($(PROC),i686)
    optim_ifort_flags = "-r8 -O3 -xN -fpe0 -fPIC -w -fpp"
  endif

  # Define pgf90 flags
  ifneq ($(shell command -v pgf90 2> /dev/null),)
    compilers += pgf90
  endif

  # Notes:
  # * Option '-Mprof=lines' might be used for profiling
  # use  -lf2c -lg2c if linking g77-compiled programs
  debug_pgf90_flags   = "-r8 -g -Kieee -fPIC -Ktrap=inv,divz,ovf -Mbounds \
			 -Mchkfpstk -Minform=inform -Mpreprocess" \
	                 NOWARN_FFLAGS="-Minform=severe"
  profile_pgf90_flags = "-r8 -pg -O0 -Mpreprocess"
  normal_pgf90_flags  = "-r8 -fPIC -Ktrap=inv,divz,ovf -Mpreprocess"
  optim_pgf90_flags   = "-r8 -fPIC -O3 -Kieee -Ktrap=inv,divz,ovf -Mpreprocess"

  else ifeq ($(OS),WindowsNT)
  # Set list of compilers, default compiler and names of compiler binaries
  #---------------------------------------------------------------------------
  # (UnxUtils with cmd.exe)
  # Note: -lg2c ?
  # use -lg2c if linking in TPlib compiled with g77 (Doesn't work on Windows?)
  #
  compilers += gfortran
  #
  # Define gfortran flags
  #---------------------------------------------------------------------------
  debug_gfortran_flags  = "-cpp -fPIC -fdefault-real-8 -fbounds-check -g -Wall \
                           -ffpe-trap=invalid,zero,overflow -mieee-fp"
  normal_gfortran_flags = "-cpp -fPIC -fdefault-real-8"
  optim_gfortran_flags  = "-cpp -fPIC -fdefault-real-8 -O3 $(march1) -funroll-loops \
			   -malign-double"
  #
  #
  # Set some processor specific flags (e.g. 64bit flags)
  #---------------------------------------------------------------------------
  ifeq ($(PROC),x86_64)
    optim_gfortran_flags = "-cpp -fPIC -fdefault-real-8 -fdefault-double-8 O3 $(march1) -funroll-loops"
  endif

  #
  # MinGW/Msys
  #***************************************************************************
  else ifeq ($(OS),MINGW32)
  # Set list of compilers, default compiler and names of compiler binaries
  #---------------------------------------------------------------------------
  # Note: -lg2c ?
  # use -lg2c if linking in TPlib compiled with g77 (Doesn't work on Windows?)
  #
  compilers += gfortran
  #
  # Define gfortran flags
  #---------------------------------------------------------------------------
  debug_gfortran_flags  = "-cpp -fPIC -fdefault-real-8 -fdefault-double-8 -fbounds-check            \
                           -ffpe-trap=invalid,zero,overflow -mieee-fp \
                           -Wall -g"
#                           -Wall -g -L$(mingw_path)"
  normal_gfortran_flags = "-cpp -fPIC -fdefault-real-8 -fdefault-double-8"
  optim_gfortran_flags  = "-cpp -fPIC -fdefault-real-8 -fdefault-double-8 -O3 -funroll-loops \
			   -malign-double"
  #
  #
  # Set some processor specific flags (e.g. 64bit flags)
  #---------------------------------------------------------------------------
  ifeq ($(PROC),x86_64)
    optim_gfortran_flags = "-cpp -fPIC -fdefault-real-8 -fdefault-double-8 -O3 -funroll-loops"
  endif

  #
  # OSF1 targets
  #***************************************************************************
  else ifeq ($(OS),OSF1)
  # Set compiler variables
  #---------------------------------------------------------------------------
  # Since there is only one compiler on OSF1, we don't use the compiler name in
  # the targets.  Instead, we use the name of the OS.  This is done by letting
  # the default compiler be set to the OS name, and by having only the dummy
  # compiler in the compiler list.  The only variable that needs to be set is
  # the fc variable, which determines the compile program.
  #
  fc = f90
  #
  # Define OSF1 flags
  #---------------------------------------------------------------------------
  # For profiling, OSF1_flags =  Run ./run-profile_OSF1 (not the run link)
  # /usr/bin/gprof cmfluid-profile_OSF1 >profil.txt
  debug_OSF1_flags     = "-g -C -r8 -V -show include"
  debug_irg_OSF1_flags = "-arch host -tune host -r8 -align records       \
                          -align dcommons -warn nouncalled -g2 -fpe0     \
                          -check overflow -check underflow -check bounds \
                          -check format -check output_conversion         \
                          -warn truncated_source -synchronous_exceptions"
  normal_OSF1_flags    = "-r8"
  r16_OSF1_flags       = "-r16"
  optim_OSF1_flags     = "-arch host -tune host -r8 -O5 -fast -warn nouncalled"
  profile_OSF1_flags   = "-pg -g3 -r8"
endif

# g++ options
debug_cpp = "-fPIC -Wall -g -mieee-fp -fsignaling-nans -D_GLIBCXX_USE_CXX11_ABI=0"
optim_cpp = "-fPIC -O3 -funroll-loops -D_GLIBCXX_USE_CXX11_ABI=0"
openmp_cpp = "-fPIC -O3 -DUSEOMP -fopenmp -funroll-loops -D_GLIBCXX_USE_CXX11_ABI=0"
profile_cpp = "-fPIC -g -pg -D_GLIBCXX_USE_CXX11_ABI=0"
normal_cpp = "-D_GLIBCXX_USE_CXX11_ABI=0"
openmpprofile_cpp = "-fPIC -O3 -DUSEOMP -fopenmp -D_GLIBCXX_USE_CXX11_ABI=0"
CC=g++

#=============================================================================
# Define the canned sequences that creates targets
#=============================================================================
#
# 'create_phony_targets' is a canned sequence that creates the dummy targets.
# If the corresponding flags are not defined, the targets will not be created.
# when a target is created, it is automatically added to the list of targets.
#
define create_phony_targets
  ifeq ($(2),dummy)
    ifdef $(1)_$(default)_flags
      targets += $(1)
$(1): $(1)_$(default)
    endif
  else
    ifdef $(1)_$(2)_flags
      targets += $(1)_$(2)
$(1)_$(2): $(1)_$(2)_$(OS)
    endif
  endif
endef
#
# 'create_target' is a canned sequence that is used to define targets based on
# three arguments: 'os', 'mode' and 'compiler' respectively. There are three
# different uses based on the content of 'compiler':
#
#    "dummy"  : Create target <mode>_<os>
#    <comp>   : Create target <mode>_<compiler>_<os>
#
# where <comp> is the name of any compiler, e.g. gfortran. The intent of
# "dummy" is to create targets for OSF1, and the intent of "dummy2" is to
# create targets for special modes (e.g. r16_ifort_Linux). The targets are only
# created if there exist corresponding flags:
#
#    <mode>_<comp>_flags : when compiler is not "dummy"
#    <mode>_<os>_flags   : else
#
# One example is in order. The following will create the targets
# optim_ifort_Linux, debug_OSF1 and r16_ifort_Linux, respectively:
#
#    $(eval $(call create_target,Linux,optim,ifort))
#    $(eval $(call create_target,OSF1 ,debug,dummy))
#    $(eval $(call create_target,Linux,r16_ifort,dummy2))
#
define create_target
ifeq ($(3),dummy)
  ifdef $(2)_$(1)_flags
    targets += $(2)_$(1)
$(2)_$(1):
	+$(MAKE) FC=$(fc) FFLAGS=$($(2)_$(1)_flags) CFLAGS=$($(2)_cpp) TARGET=$$@ \
	        LEXT=$(1) -f Makefile.code -e run
  endif
else
  ifdef $(2)_$(3)_flags
    targets += $(2)_$(3)_$(1)
$(2)_$(3)_$(1):
	+$(MAKE) FC=$(3) MODE=$(2) FFLAGS=$($(2)_$(3)_flags) CFLAGS=$($(2)_cpp) TARGET=$$@ \
	        LEXT=$(3)_$(1) -f Makefile.code -e run
    endif
  endif
endef

#=============================================================================
# Define the targets
#=============================================================================
#
# First we create the dummy targets that points to the real targets
$(foreach mode,$(modes),$(foreach comp,$(compilers),\
  $(eval $(call create_phony_targets,$(mode),$(comp)))))
#
# Then we create the real targets
$(foreach mode,$(modes),$(foreach comp,$(compilers),\
  $(eval $(call create_target,$(OS),$(mode),$(comp)))))

# Create list of filtered targets for the "all" family of targets
targets_filtered := $(filter-out %_pgf90, $(filter-out %_Linux, $(targets)))
