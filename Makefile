
# Makefile for Clawpack code in this directory.
# This version only sets the local files and frequently changed
# options, and then includes the standard makefile pointed to by CLAWMAKE.
CLAWMAKE = $(CLAW)/clawutil/src/Makefile.common

# See the above file for details and a list of make options, or type
#   make .help
# at the unix prompt.


# Adjust these variables if desired:
# ----------------------------------

CLAW_PKG = geoclaw                  # Clawpack package to use
EXE = xgeoclaw                 # Executable to create
SETRUN_FILE = setrun.py        # File containing function to make data
OUTDIR = _output               # Directory for output
SETPLOT_FILE = setplot.py      # File containing function to set plots
PLOTDIR = _plots               # Directory for plots

# NetCDF support
# Environment variable NETCDF_F_ROOT should be set
# to NetCDF Fortran installation
NF_CONFIG = $(NETCDF_F_ROOT)/bin/nf-config
LIB_PATHS = $(shell $(NF_CONFIG) --prefix)/lib
INCLUDE = $(shell $(NF_CONFIG) --fflags)
NC_LINK_CMD = $(shell $(NF_CONFIG) --flibs)
NC_LINK_CMD += $(shell $(NF_CONFIG) --cflags)

# Compiler flags
MPFLAG = -fopenmp
NCFLAGS = -DNETCDF $(NC_LINK_CMD) 
# Debugging & profiling options
#DEBUG = 1# uncomment to debug
#PROFILE = 1# uncomment for profiling
#
ifeq ($(DEBUG),1)
    LDFLAGS = -g
    # Environment variable FC should be set to fortran compiler, e.g. gfortran
    ifeq ($(FC),gfortran)
	DFLAGS = -g -fbacktrace -fbounds-check -ffpe-trap=invalid,overflow,zero
    else ifeq ($(FC),ifort)
	#DEBUG = -g -C -fpe0 -traceback -ftrapuv -warn all
	DFLAGS = -g -C -fpe0 -traceback -nogen-interface
    endif
else ifeq ($(PROFILE),1)
    LDFLAGS = -pg
    DFLAGS = -pg -O2 -profile-functions -profile-loops=all -profile-loops-report=2
    MPFLAG = 
else
    DFLAGS = -O2
endif
FFLAGS ?= $(MPFLAG) $(DFLAGS) $(NCFLAGS)
LFLAGS ?= $(NC_LINK_CMD) $(MPFLAG) $(LDFLAGS)

# ---------------------------------
# package sources for this program:
# ---------------------------------

GEOLIB = $(CLAW)/geoclaw/src/2d/shallow
include $(GEOLIB)/Makefile.geoclaw

# ---------------------------------------
# package sources specifically to exclude
# (i.e. if a custom replacement source 
#  under a different name is provided)
# ---------------------------------------

EXCLUDE_MODULES = \
    $(GEOLIB)/surge/data_storm_module.f90 \
    $(GEOLIB)/surge/storm_module.f90 \
    $(GEOLIB)/gauges_module.f90 \
    $(GEOLIB)/multilayer/multilayer_module.f90 \

EXCLUDE_SOURCES = \
    $(AMRLIB)/outmsh.f \

# ----------------------------------------
# List of custom sources for this program:
# ----------------------------------------

RIEMANN = $(CLAW)/riemann/src

MODULES = \
  ./mod/data_storm_module_wrfclaw.f90 \
  ./mod/storm_module_wrfclaw.f90 \
  $(GEOLIB)/gauges_module.f90 \
  $(GEOLIB)/multilayer/multilayer_module.f90 \

SOURCES = \
  $(RIEMANN)/rpn2_geoclaw.f \
  $(RIEMANN)/rpt2_geoclaw.f \
  $(RIEMANN)/geoclaw_riemann_utils.f \
  ./src/outmsh.f \

#-------------------------------------------------------------------
# Include Makefile containing standard definitions and make options:
include $(CLAWMAKE)

.PHONY: storm

storm:
	tar -zxvf forcing/storm.tgz -C forcing

### DO NOT remove this line - make depends on it ###
