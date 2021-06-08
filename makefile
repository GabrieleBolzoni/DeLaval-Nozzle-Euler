############################################################
#                     MAKEFILE FOR F95                     #
############################################################

# Zero axiom: the file name of a makefile must be mandatorily
#
#              makefile    OR    Makefile

# To start the compilation, type in terminal: make

# The names for aliases are completely free!

# FUNDAMENTAL WARNINGS
#
# The actions $(CMP) and $(LNK) must be NECESSARILY
# preceded by a DAMNED! TAB character
#
# The DAMNED! TAB character is needed also in continuation
# line of actions

############################################################
#
# Definition of VARIABLES (usually in CAPITALS)
#
# Compiler and linker
#
# The following definitions are for Fortran 95 only, but they
# are completely customizable if another compiler is used.

# LNK alias to be used as $(LNK) to compile and link main files .f90
# CMP alias to be used as $(CMP) to compile the source files .f90
# OPT alias to be used as $(OPT) to select the compilation options

# NAG Compiler
# ============
#
# CMP = f95 -c
# LNK = f95
# OPT = -kind=byte ### -kind=byte
# OPT = -gline ### -kind=byte  # Line number is indicated for errors
                               # occurring in runtime (for debugging)
# OPT = -O3    ### -kind=byte  # Execution optimization


# Intel Compiler
# ==============
#
# CMP = ifort  -c  -heap-arrays 100
# LNK = ifort
# OPT =
# OPT = -g  -DD  -debug -traceback  -fpe0  -check all
# OPT = -fast
# OPT = -O3


# GNU ortran Compiler
# ===================
#
  CMP = gfortran  -c
  LNK = gfortran
 #OPT = -fno-automatic  -fbounds-check  -ffpe   \
 #      trap=invalid,zero,overflow  -ggdb3
 #OPT = -fno-automatic  -O3

############################################################
#
# Objects: list of all objects *.o

OBJS = compressible_flow_lib.o   \
       nonlinear_equations.o     \
       nozzle_geometry.o         \
       ugello.o                  \
       euler_flux_jacobian.o      \
       linearization.o            \
       numerical_fluxes.o         \
       boundary.o                 \
       gnufor.o   		  \
       exact_riemann_problem.o    \
       thermodynamics.o


# OBJS alias to be used as $(OBJS)


############################################################
# Executable generation by the linker / the effect of command
#                                     / make starts from here

main:         main.o  $(OBJS)
	$(LNK) $(OPT)  main.o  $(OBJS)  \
               -o EULERO_nozzle.exe  # Name chosen for program
### NOMORE #  ^
### NOMORE #  |
### NOMORE # A DAMNED! TAB character is leading the previous line
### NOMORE # (see WARNINGS)

############################################################
# Objects generation

main.o:                  main.f90  $(OBJS)
	        $(CMP) $(OPT)  main.f90


ugello.o:                ugello.f90  nonlinear_equations.o nozzle_geometry.o compressible_flow_lib.o
	       $(CMP) $(OPT)  ugello.f90

nonlinear_equations.o:   nonlinear_equations.f90 
	       $(CMP) $(OPT)  nonlinear_equations.f90 

nozzle_geometry.o:       nozzle_geometry.f90
	       $(CMP) $(OPT)  nozzle_geometry.f90

	
compressible_flow_lib.o: compressible_flow_lib.f90     thermodynamics.o
	       $(CMP) $(OPT)  compressible_flow_lib.f90


thermodynamics.o:        thermodynamics.f90
	        $(CMP) $(OPT)  thermodynamics.f90


euler_flux_jacobian.o:   euler_flux_jacobian.f90 thermodynamics.o
	        $(CMP) $(OPT)  euler_flux_jacobian.f90


numerical_fluxes.o:	     numerical_fluxes.f90  thermodynamics.o  euler_flux_jacobian.o linearization.o   exact_riemann_problem.o
	        $(CMP) $(OPT)  numerical_fluxes.f90
			       
linearization.o:         linearization.f90  euler_flux_jacobian.o \
                         thermodynamics.o 
	        $(CMP) $(OPT)  linearization.f90  

boundary.o:              boundary.f90   euler_flux_jacobian.o
					$(CMP) $(OPT)  boundary.f90

gnufor.o:                gnufor.f90
	        $(CMP) $(OPT)  gnufor.f90


exact_riemann_problem.o: exact_riemann_problem.f90 euler_flux_jacobian.o thermodynamics.o
	        $(CMP) $(OPT)  exact_riemann_problem.f90



############################################################
# Cleaning command to remove all objects *.o, files *.mod and
# the executables *.exe

clean:
	@echo cleaning objects, modules and executables
	rm  *.o  *.mod   .*  video/*  *.txt  EULERO_nozzle.exe
	rm -d video

# The command "make clean" deletes all the indicated files

############################################################
