program main
	use mpi
	use mod_calc
	!
	implicit none
	! for MPI
	integer :: ierr ! Fehlercode
	integer :: rank, status(MPI_STATUS_SIZE)
	!
	integer :: cpus !number of used cpus
	integer(kind=4), parameter :: numberOfSegments = int(1e9) ! Anzahl der Stützstellen für das Integral
	double precision, parameter :: lowerBoundary = 0 ! Untergrenze für das Integral
	double precision, parameter :: upperBoundary = 1 ! Obergrenze für das Integral
	double precision :: temp, summe ! Zwischenspeicher für Ergebnis
	integer :: i !Schleifencounter
	double precision :: stepSize ! Größe der Abschnitte die einzeln gerechnet werden
	
	! performance measurement:
	real :: startTime, endTime
	double precision, parameter :: pibel = 3.1415926535897932 ! Pi aus von pible.de zur Messung der Genauigkeit
	call cpu_time(starttime)
	
	
	call mpi_init(ierr)
	call  mpi_comm_size(mpi_comm_world, cpus, ierr) !size
	call mpi_comm_rank(mpi_comm_world, rank, ierr) !rank
	stepSize = (upperBoundary - lowerBoundary)/cpus !stepSize wird erst hier festgelegt, weil Anzahl der CPUs mit MPI-methode bestimmt wird

	! Eigentliches Rechnen, jeder Thread
	call integral((rank * stepSize), ((rank + 1) * stepSize), (numberOfSegments/cpus), temp)

	select case (rank)
	case (0) !master
		summe = temp
		! Master empfängt
		do i = 1, (cpus -1)
			call mpi_recv(temp, 1, mpi_double, i, 2017, mpi_comm_world, status, ierr)
			summe = summe + temp
		end do
		! Ausgabe
		print *, "Errechneter Wert für Pi:", summe
		
		! performance measurement: 
		call cpu_time(endtime)
		print *, "Zeit für diese Rechnung:", (endtime - starttime), "Sekunden"
		print *, "Abweichung von Pi:      ", (pibel - summe), "(Referenzwert für Pi von pibel.de)"
	case default
		! Slaves senden
		call mpi_send(temp, 1, mpi_double, 0, 2017, mpi_comm_world, ierr)
	end select
	
	call mpi_finalize(ierr)
	

end program
