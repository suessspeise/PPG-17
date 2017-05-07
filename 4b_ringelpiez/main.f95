program main
	use mpi
	!
	implicit none
	! for MPI
	integer :: ierr ! Fehlercode
	integer :: rank, status(MPI_STATUS_SIZE)
	integer :: cpus !number of used cpus
	!
	integer :: i !Schleifencounter
	integer :: summe = 0, plumpsack = 0
	logical :: weiterMachen = .true. 
	integer :: L, R ! ID des Linken und des Rechten Nachbarn
	
	! MPI initialisieren
	call mpi_init(ierr)
	call  mpi_comm_size(mpi_comm_world, cpus, ierr) !size
	call mpi_comm_rank(mpi_comm_world, rank, ierr) !rank

	! Feststellen der IDs der Nachbarn
	L = rank - 1
	if (L < 0) then 
		L = cpus - 1
	end if
	R = rank + 1
	if (R >= cpus) then
		R = 0
	end if

	plumpsack = rank
	! Dreh dich nicht um denn der Plumpsack geht um!
	ringelpiez: do while (weiterMachen)
		! Send and receive
		call mpi_send(plumpsack, 1, mpi_integer, R, 2017, mpi_comm_world, ierr)
		call mpi_recv(plumpsack, 1, mpi_integer, L, 2017, mpi_comm_world, status, ierr)
		
		! Abbruchbedingungen checken, setzen
		if (plumpsack == rank) then
			weiterMachen = .false.
		end if
		
		!Aufsummieren
		summe = summe + plumpsack
	end do ringelpiez
	
	! Ausgabe
	print *, "Prozess:", rank, "Summe:", summe
	
	call mpi_finalize(ierr)
end program
