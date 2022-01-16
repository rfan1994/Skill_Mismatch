# This is how you comment in makefile
FC = ifort -parallel
FFLAGS = -c

# You can change the name of the executable
EXECUTABLE = Simulation
F90_FILES := $(wildcard *.f90)
OBJECTS := $(patsubst %.f90, %.o, $(F90_FILES))
$(EXECUTABLE) : $(OBJECTS)
	$(FC) -o $@ $^
%.o: %.f90
	$(FC) $(FFLAGS) $< -o $@

# Below you need to list all the dependencies
main.o: Control.o
main.o: Global_Variable.o
main.o: My_Function.o
main.o: Value_Function.o
main.o: Simulation.o
My_Function.o: Control.o
My_Function.o: Global_Variable.o
Value_Function.o: Global_Variable.o
Value_Function.o: Control.o
Value_Function.o: My_Function.o
Simulation.o: Control.o
Simulation.o: Global_Variable.o
Simulation.o: My_Function.o
Simulation.o: Value_Function.o
clean :
	rm -f *.o *.mod $(EXECUTABLE)
