#Compiler
FC="gfortran"
#Number of used Processors
KC=4

# 1zu1 wie im Script....
main.x: main.f95
        mpif90 -o main.x main.f95

# mod ausgelagert funktioniert nicht....
#main.x: main.f95 mod_calc.o
#       mpif90 -o main.x main.f95 mod_calc.o

#mod_calc.o: mod_calc.f95
#       $(FC) -c mod_calc.f95

run: main.x
        mpiexec -n $(KC) ./main.x

clean:
        rm *.mod *.o main.x
