#############################
# GNU MAKEFILE FOR GWM-2005
#############################
#
# :Created by: Mike Toews, GNS Science <M.Toews@gns.cri.nz>
#
# Requirements:
#  - GNU Make (tested with version 3.81)
#  - A Fortran compiler that supports at least Fortran 90 standards
#
# A typical build/install requires at least two commands.
#
# 1) Build:
#     $ make
#
# 2) Install, as root (you may need to prefix with "sudo" for some systems):
#     $ make install
#
# Or to install in an alternative install prefix
#     $ make install prefix=/path/to/dir
# Or put the binaries in one place
#     $ make install bindir=/path/to/bin
#
# To build with a specific compiler, either uncomment one of the blocks
# below, or use this unmodified makefile with variables like this:
#     $ make FC=gfortran FFLAGS=-O1 PROGRAM=gwm2005-gnu

# Name of executable
PROGRAM = gwm2005

###########
# COMPILER
###########

## GNU
CC = gcc
FC = gfortran

## Intel
#CC = icc
#FC = ifort

# Define the Fortran compiler flags
CFLAGS += -g -O0 -D_UF
FFLAGS += -g -O0 -fbacktrace -fcheck=all


# Default install location
prefix = /usr/local
bindir = $(prefix)/bin

# Linker is the same as the Fortran compiler
LD = $(FC)

#############################################################
# This file does not normally need editing beyond this point


################
# PATTERN RULES
################

.SUFFIXES : .c .h .f .for .f90 .mod .inc .o

%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@

%.o : %.f
	$(FC) -c $(FFLAGS) $< -o $@

%.o : %.for
	$(FC) -c $(FFLAGS) $< -o $@

%.o : %.f90
	$(FC) -c $(FFLAGS) $< -o $@


##########
# OBJECTS
##########

OBJECTS = \
    ccfd.o \
    de47.o \
    gmg7.o \
    gwf2bas7.o \
    gwf2bcf7.o \
    gwf2bfh2.o \
    gwf2chd7.o \
    gwf2drn7.o \
    gwf2drt7.o \
    gwf2ets7.o \
    gwf2evt7.o \
    gwf2fhb7.o \
    gwf2gag7.o \
    gwf2ghb7.o \
    gwf2hfb7.o \
    gwf2huf7.o \
    gwf2hydmod7.o \
    gwf2ibs7.o \
    gwf2lak7.o \
    gwf2lgr2.o \
    gwf2lpf7.o \
    gwf2mnw17.o \
    gwf2mnw27.o \
    gwf2mnw2i7.o \
    gwf2rch7.o \
    gwf2res7.o \
    gwf2riv7.o \
    gwf2sfr7.o \
    gwf2str7.o \
    gwf2sub7.o \
    gwf2swt7.o \
    gwf2uzf1.o \
    gwf2wel7.o \
    gwflgrmodule.o \
    gwfsfrmodule.o \
    gwfuzfmodule.o \
    GWM1BAS3.o \
    GWM1BAS3SUBS.o \
    GWM1DCC3.o \
    gwm1dcv3mnw.o \
    GWM1DCV3.o \
    GWM1HDC3.o \
    GWM1OBJ3.o \
    GWM1RMS3MOD.o \
    GWM1RMS3.o \
    GWM1SMC3.o \
    GWM1STA3.o \
    GWM1STC3.o \
    gwm2005.o \
    gwm2005_stop.o \
    gwm_subs.o \
    hufutl7.o \
    lmt7.o \
    mf2005_utls.o \
    mf2kgmg.o \
    mhc7.o \
    obs2bas7.o \
    obs2chd7.o \
    obs2drn7.o \
    obs2ghb7.o \
    obs2riv7.o \
    obs2str7.o \
    parutl7.o \
    pcg7.o \
    pcgn2.o \
    pcgn_solve2.o \
    r_vector.o \
    sip7.o \
    solvers.o \
    utl7.o


##########
# TARGETS
##########

# First and default, i.e. 'make'
all : $(PROGRAM)

$(PROGRAM) : $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

###############
# DEPENDENCIES
###############

# de4module.mod from de47.f
gwf2mnw17.o gwf2mnw27.o gwm2005.o : de47.o

# global.mod from gwf2bas7.f
GWM1BAS3SUBS.o GWM1DCV3.o GWM1HDC3.o GWM1RMS3.o GWM1STA3.o GWM1STC3.o de47.o \
    gmg7.o gwf2bcf7.o gwf2bfh2.o gwf2chd7.o gwf2drn7.o gwf2drt7.o gwf2ets7.o \
    gwf2evt7.o gwf2fhb7.o gwf2gag7.o gwf2ghb7.o gwf2hfb7.o gwf2huf7.o \
    gwf2hydmod7.o gwf2ibs7.o gwf2lak7.o gwf2lgr2.o gwf2lpf7.o gwf2mnw17.o \
    gwf2mnw27.o gwf2mnw2i7.o gwf2rch7.o gwf2res7.o gwf2riv7.o gwf2sfr7.o \
    gwf2str7.o gwf2sub7.o gwf2swt7.o gwf2uzf1.o gwf2wel7.o gwm1dcv3mnw.o \
    gwm2005.o lmt7.o obs2bas7.o obs2chd7.o obs2drn7.o obs2ghb7.o obs2riv7.o \
    obs2str7.o pcg7.o pcgn2.o sip7.o : gwf2bas7.o

# gmgmodule.mod from gmg7.f
gwf2mnw17.o gwm2005.o : gmg7.o

# gwfbasmodule.mod from gwf2bas7.f
GWM1BAS3SUBS.o GWM1DCV3.o GWM1STA3.o gwf2bcf7.o gwf2bfh2.o gwf2chd7.o \
    gwf2drn7.o gwf2drt7.o gwf2ets7.o gwf2evt7.o gwf2fhb7.o gwf2gag7.o \
    gwf2ghb7.o gwf2huf7.o gwf2hydmod7.o gwf2ibs7.o gwf2lak7.o gwf2lgr2.o \
    gwf2lpf7.o gwf2mnw17.o gwf2mnw27.o gwf2mnw2i7.o gwf2rch7.o gwf2res7.o \
    gwf2riv7.o gwf2sfr7.o gwf2str7.o gwf2sub7.o gwf2swt7.o gwf2uzf1.o \
    gwf2wel7.o gwm2005.o lmt7.o obs2chd7.o : gwf2bas7.o

# gwfbcfmodule.mod from gwf2bcf7.f
GWM1STA3.o gwf2bfh2.o gwf2lak7.o gwf2lgr2.o gwf2mnw17.o gwf2mnw27.o \
    gwf2sfr7.o gwf2uzf1.o lmt7.o : gwf2bcf7.o

# gwfdrnmodule.mod from gwf2drn7.f
GWM1STA3.o lmt7.o obs2drn7.o : gwf2drn7.o

# gwfdrtmodule.mod from gwf2drt7.f
lmt7.o : gwf2drt7.o

# gwfetsmodule.mod from gwf2ets7.f
lmt7.o : gwf2ets7.o

# gwfevtmodule.mod from gwf2evt7.f
gwf2lgr2.o gwm2005.o lmt7.o : gwf2evt7.o

# gwffhbmodule.mod from gwf2fhb7.f
lmt7.o : gwf2fhb7.o

# gwfghbmodule.mod from gwf2ghb7.f
lmt7.o obs2ghb7.o : gwf2ghb7.o

# gwfhufmodule.mod from gwf2huf7.f
gwf2bfh2.o gwf2lak7.o gwf2lgr2.o gwf2mnw17.o gwf2mnw27.o gwf2sfr7.o \
    gwf2uzf1.o gwm2005.o hufutl7.o lmt7.o : gwf2huf7.o

# gwfibsmodule.mod from gwf2ibs7.f
gwf2hydmod7.o : gwf2ibs7.o

# gwflakmodule.mod from gwf2lak7.f
gwf2gag7.o gwf2sfr7.o gwf2uzf1.o gwm2005.o : gwf2lak7.o

# gwflpfmodule.mod from gwf2lpf7.f
GWM1STA3.o gwf2bfh2.o gwf2lak7.o gwf2lgr2.o gwf2mnw17.o gwf2mnw27.o \
    gwf2sfr7.o gwf2uzf1.o lmt7.o : gwf2lpf7.o

# gwfmnw1module.mod from gwf2mnw17.f
lmt7.o : gwf2mnw17.o

# gwfmnw2module.mod from gwf2mnw27.f
GWM1HDC3.o GWM1RMS3.o GWM1STA3.o gwf2mnw2i7.o gwm1dcv3mnw.o lmt7.o \
    : gwf2mnw27.o

# gwfrchmodule.mod from gwf2rch7.f
gwf2lgr2.o gwm2005.o lmt7.o : gwf2rch7.o

# gwfresmodule.mod from gwf2res7.f
gwf2lgr2.o lmt7.o : gwf2res7.o

# gwfrivmodule.mod from gwf2riv7.f
lmt7.o obs2riv7.o : gwf2riv7.o

# gwfsfrmodule.mod from gwfsfrmodule.f
GWM1STA3.o GWM1STC3.o gwf2gag7.o gwf2hydmod7.o gwf2lak7.o gwf2sfr7.o \
    gwf2uzf1.o gwm2005.o : gwfsfrmodule.o

# gwfstrmodule.mod from gwf2str7.f
GWM1STA3.o GWM1STC3.o gwf2hydmod7.o lmt7.o obs2str7.o : gwf2str7.o

# gwfsubmodule.mod from gwf2sub7.f
gwf2hydmod7.o : gwf2sub7.o

# gwfuzfmodule.mod from gwfuzfmodule.f
gwf2lak7.o gwf2sfr7.o gwf2uzf1.o gwm2005.o : gwfuzfmodule.o

# gwfwelmodule.mod from gwf2wel7.f
GWM1DCV3.o lmt7.o : gwf2wel7.o

# gwm1bas3.mod from GWM1BAS3.for
GWM1BAS3SUBS.o GWM1DCC3.o GWM1DCV3.o GWM1HDC3.o GWM1OBJ3.o GWM1RMS3.o \
    GWM1SMC3.o GWM1STA3.o GWM1STC3.o gwm1dcv3mnw.o : GWM1BAS3.o

# gwm1bas3subs.mod from GWM1BAS3SUBS.for
gwm2005.o : GWM1BAS3SUBS.o

# gwm1dcc3.mod from GWM1DCC3.for
GWM1BAS3SUBS.o GWM1RMS3.o : GWM1DCC3.o

# gwm1dcv3.mod from GWM1DCV3.for
GWM1BAS3SUBS.o GWM1DCC3.o GWM1HDC3.o GWM1OBJ3.o GWM1RMS3.o GWM1SMC3.o \
    GWM1STA3.o GWM1STC3.o gwm1dcv3mnw.o gwm2005.o : GWM1DCV3.o

# gwm1dcv3mnw.mod from gwm1dcv3mnw.for
gwm2005.o : gwm1dcv3mnw.o

# gwm1hdc3.mod from GWM1HDC3.for
GWM1BAS3SUBS.o GWM1RMS3.o gwm1dcv3mnw.o gwm2005.o : GWM1HDC3.o

# gwm1obj3.mod from GWM1OBJ3.for
GWM1BAS3SUBS.o GWM1HDC3.o GWM1RMS3.o GWM1STC3.o : GWM1OBJ3.o

# gwm1rms3.mod from GWM1RMS3MOD.for
GWM1BAS3SUBS.o GWM1DCC3.o GWM1DCV3.o GWM1HDC3.o GWM1OBJ3.o GWM1RMS3.o \
    GWM1SMC3.o GWM1STA3.o GWM1STC3.o gwm2005.o : GWM1RMS3MOD.o

# gwm1rms3subs.mod from GWM1RMS3.for
GWM1BAS3SUBS.o gwm2005.o : GWM1RMS3.o

# gwm1smc3.mod from GWM1SMC3.for
GWM1BAS3SUBS.o GWM1RMS3.o : GWM1SMC3.o

# gwm1sta3.mod from GWM1STA3.for
GWM1BAS3SUBS.o GWM1OBJ3.o GWM1RMS3.o GWM1SMC3.o gwm1dcv3mnw.o gwm2005.o \
    : GWM1STA3.o

# gwm1stc3.mod from GWM1STC3.for
GWM1BAS3SUBS.o GWM1RMS3.o gwm2005.o : GWM1STC3.o

# gwm_stop.mod from gwm2005_stop.f90
GWM1BAS3.o GWM1BAS3SUBS.o GWM1DCC3.o GWM1DCV3.o GWM1HDC3.o GWM1OBJ3.o \
    GWM1RMS3.o GWM1SMC3.o GWM1STA3.o GWM1STC3.o gwm1dcv3mnw.o : gwm2005_stop.o

# gwm_subs.mod from gwm_subs.f90
GWM1BAS3SUBS.o GWM1DCC3.o GWM1DCV3.o GWM1HDC3.o GWM1OBJ3.o GWM1RMS3.o \
    GWM1SMC3.o GWM1STA3.o GWM1STC3.o gwm1dcv3mnw.o gwm2005_stop.o : gwm_subs.o

# lgrmodule.mod from gwflgrmodule.f
gwf2bcf7.o gwf2huf7.o gwf2lgr2.o gwf2sfr7.o gwm2005.o : gwflgrmodule.o

# mf2005_utls.mod from mf2005_utls.f90
GWM1BAS3SUBS.o GWM1DCC3.o GWM1DCV3.o GWM1HDC3.o GWM1OBJ3.o GWM1RMS3.o \
    GWM1SMC3.o GWM1STA3.o GWM1STC3.o : mf2005_utls.o

# obsbasmodule.mod from obs2bas7.f
obs2chd7.o obs2drn7.o obs2ghb7.o obs2riv7.o obs2str7.o : obs2bas7.o

# parammodule.mod from gwf2bas7.f
GWM1BAS3SUBS.o gwf2drt7.o gwf2hfb7.o gwf2huf7.o gwf2lpf7.o gwf2sfr7.o \
    hufutl7.o parutl7.o : gwf2bas7.o

# pcg_main.mod from pcgn_solve2.f90
pcgn2.o : pcgn_solve2.o

# pcgmodule.mod from pcg7.f
gwf2mnw17.o gwf2mnw27.o gwm2005.o : pcg7.o

# pcgn.mod from pcgn2.f90
gwf2mnw17.o gwm2005.o : pcgn2.o

# sipmodule.mod from sip7.f
gwf2mnw17.o gwf2mnw27.o gwf2sub7.o gwm2005.o : sip7.o


#########################
# CLEANUP & INSTALLATION
#########################

clean :
	rm -f $(PROGRAM)
	rm -f $(OBJECTS)
	rm -f *.mod

install : all
	install $(PROGRAM) $(bindir)

uninstall :
	cd $(bindir)
	rm -f $(PROGRAM)

.PHONY : all clean install uninstall
