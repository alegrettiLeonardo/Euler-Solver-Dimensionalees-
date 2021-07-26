#  EXEC = path to executable
#  O    = extension of object files (Unix: .o / DOS: .obj)
#  RM   = command to delete files
#
EXEC = main.out
O    = *.o
MOD  = *.mod
RM   = rm
PLT  = *.xyz
Q    = *.q   

#### PC - GNU Fortran
# -Wall -        Short for "warn about all" this flag tells gfortran to generate warnings about many common sources
#                of bugs, such as having a subroutine of function with the same name as a built-in one, or passing
#                the same variable as a intent(in) and a intent(out) argument of the same subroutine. In spite of
#                its name, this does not turn all possible -W options on.
# -Wextra -      In conjunction with -Wall, gives warnings about even more potential problems. In particular,
#                -Wextra warns about subroutine arguments that are never used, which is almost always a bug.    
# -Wconversion - Warns about implicit conversion. For example, if you want a double precision variable sqrt2 to 
#                to hold an accurate value for the square root of 2, you might write by accident sqrt2=sqrt(2.).
#                Since 2. is a single-precision value, the single-precision sqrt function will be used, and the 
#                value of sqrt2 will not be as accurate as it could be. -Wconversion will generate a warning 
#                here, because the single-precision result of sqrt is implicitly converted into a double-precision
#                value
# -pedantic -    Generate warnings about language features that are supported by gfortran but are not part of the 
#                official Fortran 95 standard. Useful if you want to be sure your code will work with any Fortran
#                95 compiler

LD      = gfortran
FC      = gfortran
FFLAGS  = -O3 -Wall -Wextra -Wconversion -pedantic -fimplicit-none -fbacktrace -ffree-line-length-none -fcheck=all -c
LDFLAGS = -O3 -s -o
DBFLAGS = -g -ffpe-trap=zero,invalid,overflow,underflow

SOURCES = vars.f90 bcfx.f90 bcpv.f90 firs.f90 grid.f90 high.f90 hllc.f90 \
          init.f90 post.f90 solv.f90 main.f90 

OBJECTS = ${SOURCES:.f=$(O)}
.SUFFIXES: .f $(O)

.f$(O):
	$(FC) $(FFLAGS) $*.f

$(EXEC): $(OBJECTS)
	$(FC) $(LDFLAGS) $(EXEC) $(OBJECTS)

clean:
	$(RM) $(EXEC) $(MOD) $(PLT) $(Q) $(O)
