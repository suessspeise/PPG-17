#Compiler
FC="gfortran"
FC="mpif90"
#Number of used Processors
KC=4

main.x: main.f95 mod_calc.o
	$(FC) -o main.x main.f95 mod_calc.o 

mod_calc.o: mod_calc.f95
	$(FC) -c mod_calc.f95 

run: main.x
	mpiexec -n $(KC) ./main.x 

clean:
	rm  main.x *.mod *.o
