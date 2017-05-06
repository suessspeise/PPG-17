
program main
        use mpi
        use mod_calc
        !
        implicit none
        ! for MPI
        integer :: ierr ! Fehlercode
        integer :: rank, status(MPI_STATUS_SIZE)
        !
        integer, parameter :: cpus = 4 !number of used cpus
        integer(kind=4), parameter :: numberOfSegments = int(1e9) ! Anzahl der Stützstellen für das Integral
        double precision, parameter :: lowerBoundary = 0 ! Untergrenze für das Integral
        double precision, parameter :: upperBoundary = 1 ! Obergrenze für das Integral
        double precision :: temp, summe ! Zwischenspeicher für Ergebnis
        integer :: i !Schleifencounter
        double precision :: stepSize ! Größe der Abschnitte die einzeln gerechnet werden
        stepSize = (upperBoundary - lowerBoundary)/cpus


        call mpi_init(ierr)
        call mpi_comm_rank(mpi_comm_world, rank, ierr)

        call integral((rank * stepSize), ((rank + 1) * stepSize), (numberOfSegments/cpus), temp)

        select case (rank)
        case (0) !master
                summe = temp
                do i = 1, (cpus -1)
                        call mpi_recv(temp, 1, mpi_double, i, 2017, mpi_comm_world, status, ierr)
                        summe = summe + temp
                end do
                print *, "pi: ", summe
        case default
                call mpi_send(temp, 1, mpi_double, 0, 2017, mpi_comm_world, ierr)
        end select

        call mpi_finalize(ierr)

end program
