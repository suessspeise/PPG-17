#Compiler
FC="gfortran"
FC="mpif90"
#Number of used Processors
KC=11

main.x: main.f95 
	$(FC) -o main.x main.f95 

run: main.x
	mpiexec -n $(KC) ./main.x 

clean:
	rm  main.x *.mod *.o
