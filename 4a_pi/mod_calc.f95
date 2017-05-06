module mod_calc
	implicit none

contains
	! subroutine polynom(x)
	! subroutine sliceRec(lowerBoundary, upperBoundary, result)
	! subroutine sliceTri(lowerBoundary, upperBoundary, result)
	! subroutine integral(lowerBoundary, upperBoundary, numberSlices, result)

	! Berechnet den Wert der Funktion:
	!	f(x) = 4(1 + x*x)
	! Rückgabe über x!
	subroutine polynom(x)
		double precision, intent(inout) :: x  ! x = y
        
        x = 4/(1 + x*x)
	end subroutine
	
	! Rechteckmethode
	! Integral über Interval zwischen lowerBoundary und upperBoundary
	!	über das Polynom der subroutine polynom(x)
	! Rückgabe über result
	subroutine sliceRec(lowerBoundary, upperBoundary, result)
		double precision, intent(in) :: 	lowerBoundary, upperBoundary ! Ober- und Untergrenze des Integrals
		double precision, intent(inout) :: 	result ! Rückgabewert
		
		! Bildung des arithmetischen Mittels zwischen Ober- und Untergrenze
		result = (lowerBoundary + upperBoundary) /2
		! Funktionswert für den Mittelwert
		call polynom(result)
		! Multiplikation mit Breite des Abschnitts
		result = result * (upperBoundary - lowerBoundary)
	end subroutine sliceRec
	
	! Trapezmethode
	! Integral über Interval zwischen lowerBoundary und upperBoundary
	!	über das Polynom der subroutine polynom(x)
	! Rückgabe über result
	subroutine sliceTri(lowerBoundary, upperBoundary, result)
		double precision, intent(in) :: 	lowerBoundary, upperBoundary ! Ober- und Untergrenze des Integrals
		double precision, intent(inout) :: 	result ! Rückgabewert
		double precision :: y1, y2 ! Zwischenspeicher für Funktionswerte
		
		! Funktionswert für die Unergrenze
		result = lowerBoundary
		call polynom(result)
		! Multiplikation mit Breite des Abschnitts
		result = result * (upperBoundary - lowerBoundary)
		! Aufrechnen des Dreiecks
		y1 = lowerBoundary
		y2 = upperBoundary
		call polynom(y1)
		call polynom(y2)
		result = result + (upperBoundary - lowerBoundary) * abs(y1 - y2)
	end subroutine sliceTri
	
	
	subroutine integral(lowerBoundary, upperBoundary, numberSlices, result)
		double precision, intent(in) :: 	lowerBoundary, upperBoundary ! Ober- und Untergrenze des Integrals
		double precision, intent(inout) :: 	result ! Rückgabewert 
		double precision :: temp ! Zwischenspeicher für einzelne Slices 
		integer(kind=4) :: numberSlices ! Anzahl der Stützstellen für die Berechnung des Integrals
		integer(kind=4) :: i ! Schleifenvariable
		
		! Reset der Rückgabevariablen
		result = 0
		! Iteration über Teilabschnitte
		do i = 1, numberSlices
			! Integral für einzelnen Abschnitt
			call sliceRec(dble(i -1)/numberSlices, dble(i)/numberSlices, temp)
			! Aufsummieren
			result = result + temp
		end do
	end subroutine integral

	
end module
