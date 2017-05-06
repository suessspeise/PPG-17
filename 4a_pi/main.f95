program main
	use mod_calc
	use mpi
	
	implicit none
	!
	integer(kind=4), parameter :: numberOfSegments = int(1e9) ! Anzahl der Stützstellen für das Integral
	double precision, parameter :: 	lowerBoundary = 0 ! Untergrenze für das Integral
	double precision, parameter :: 	upperBoundary = 1 ! Obergrenze für das Integral
	!
	double precision :: temp ! Zwischenspeicher für Ergebnis
	integer :: i ! Schleifenvariable
	integer :: ierr ! Fehlercode für MPI
	
	integer(kind=4) :: numbers = int(1e9) ! Anzahl der Stützstellen für das Integral

	call MPI_INIT(ierr) 

	numbers = 5
    call integral(lowerBoundary, upperBoundary, numbers, temp)
	print *, "precision: ", numbers, "pi: ", temp
	numbers = 10
    call integral(lowerBoundary, upperBoundary, numbers, temp)
	print *, "precision: ", numbers, "pi: ", temp
	numbers = 20
    call integral(lowerBoundary, upperBoundary, numbers, temp)
	print *, "precision: ", numbers, "pi: ", temp
	numbers = 1000
    call integral(lowerBoundary, upperBoundary, numbers, temp)
	print *, "precision: ", numbers, "pi: ", temp
	numbers = 1000000
    call integral(lowerBoundary, upperBoundary, numbers, temp)
	print *, "precision: ", numbers, "pi: ", temp
	numbers = 10000000
    call integral(lowerBoundary, upperBoundary, numbers, temp)
	print *, "precision: ", numbers, "pi: ", temp
	
	
	call MPI_FINALIZE(ierr)        
	
	
!	print *, "von google:", 0, "pi: ", "   3.14159265359"
	print *, "von pibel: ", 0, "pi: ", "   3.141592653589793238"
	
	
	print *, "ENDE"

end program
