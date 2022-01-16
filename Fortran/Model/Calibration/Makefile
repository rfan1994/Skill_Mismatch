# This is how you comment in makefile
FC = ifort -parallel
FFLAGS = -I /N/soft/rhel7/nlopt/2.4.2/include -c
FFLAGS2 = -I /N/soft/rhel7/nlopt/2.4.2/include
FFLAGS3 = -L /N/soft/rhel7/nlopt/2.4.2/lib -lnlopt -lm 

# You can change the name of the executable
EXECUTABLE = Calibration
F90_FILES := $(wildcard *.f90)
OBJECTS := $(patsubst %.f90, %.o, $(F90_FILES))
$(EXECUTABLE) : $(OBJECTS)
	$(FC) $(FFLAGS2) -o $@ $^ $(FFLAGS3)
%.o: %.f90
	$(FC) $(FFLAGS) $< -o $@

# Below you need to list all the dependencies
main.o: Control.o
main.o: Global_Variable.o
main.o: My_Function.o
main.o: Value_Function.o
main.o: Simulation.o
main.o: nlopt.o
My_Function.o: Control.o
My_Function.o: Global_Variable.o
Value_Function.o: Control.o
Value_Function.o: Global_Variable.o
Value_Function.o: My_Function.o
Simulation.o: Control.o
Simulation.o: Global_Variable.o
Simulation.o: My_Function.o
Simulation.o: Value_Function.o
nlopt.o: Control.o
nlopt.o: func_est_nlopt.o
func_est_nlopt.o: Control.o
func_est_nlopt.o: Global_Variable.o
func_est_nlopt.o: My_Function.o
func_est_nlopt.o: Value_Function.o

clean :
	rm -f *.o *.mod $(EXECUTABLE)
