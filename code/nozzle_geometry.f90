MODULE nozzle_geometry


   ! La forma dell'ugello è specificata assegnando l'area
   ! delle sezioni di ingresso (Ai), di gola (Ag) e di
   ! uscita (Au) rispettivamente nei punti xi, xg e xu.
   !
   !                         ____
   !    _____          _____/
   !         \___     /
   !             \___/
   !
   !  -- -- -- -- -- -- -- -- -- --
   !    xi         xg           xu


   IMPLICIT NONE

   ! PRIVATE ! le variabili del preambolo sono privatizzate

                             ! ingresso     gola              uscita
   REAL(KIND=8), PARAMETER ::  xi = 0,    xg = 4,    xu = 10,  &
                               Ai = 2,    Ag = 1,    Au = 3

   INTEGER, PUBLIC, PARAMETER :: Np = 499 !DEVE ESSERE DISPARI
   INTEGER, PUBLIC, PARAMETER :: N_nodes = ((Np-1)/2) + 2 !+2 per aggiungere le mezze celle
   INTEGER, PUBLIC, PARAMETER :: N_int = (Np+1)/2


!   REAL(KIND=8), PUBLIC, PARAMETER :: nozzle_length = xu - xi

   PUBLIC :: gen_nozzle_geometry, &
                 nozzle_section_area, &
                 nozzle_section_area_derivative

CONTAINS


SUBROUTINE  gen_nozzle_geometry(xx, AA, a, b)

   ! Genera la geometria dell'ugello
   !
   !   xx - coordinata assiale
   !   AA - area della sezione
   !
   ! La distribuzione dei punti xx(i) puo' essere non uniforme
   !
   ! Se sono presenti i parametri OPZIONALI a e/o b,
   ! viene generato solo il tratto dell'ugello con x
   ! nell'intervallo [a, b] oppure [xi, b] o [a, xu]

   ! Variabili globali usate: xi, xg, xu

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(INOUT) :: xx, AA
   REAL(KIND=8), OPTIONAL,     INTENT(IN)    :: a, b

   REAL(KIND=8) :: xa, xb
   INTEGER, DIMENSION (1) :: i_1
   INTEGER :: i
   INTEGER :: i_g

   IF (SIZE(xx) /= SIZE(AA)) THEN

      WRITE(*,*) 'Nella subroutine gen_nozzle_geometry, '
      WRITE(*,*) 'le dimensioni del vettore xx sono diverse'
      WRITE(*,*) 'da quelle del vettore aa. STOP'
      STOP

   ENDIF

   ! Shift di mezza unità per avere la prima cella nell'origine
   xa = xi + (xb - xa)/(2*(SIZE(xx) - 1));      xb = xu + (xb - xa)/(2*(SIZE(xx) - 1))

   IF (PRESENT(a)) THEN
      xa = a
   ENDIF

   IF (PRESENT(b)) THEN
      xb = b
   ENDIF

   ! Reticolo uniforme

   xx = [ (xa + (i - 1)*(xb - xa)/(SIZE(xx) - 1), i = 1, SIZE(xx)) ]


   ! Se l'intervallo [xa, xb] contiene la sezione di gola,
   ! il punto xx(?) piu' vicino a xg viene spostato in xg
   IF (xa < xg  .AND.  xb > xg) THEN

      i_1 = MINLOC(ABS(xx - xg));   i_g = i_1(1)
      ! La solita stramaledizione: MINLOC genera un vettore
      ! (in questo caso di dimensione 1)

      xx(i_g) = xg
      ! Il reticolo non è più uniforme

   ENDIF

   DO i = 1, SIZE(xx)

      AA(i) = nozzle_section_area(xx(i))

   ENDDO

END SUBROUTINE  gen_nozzle_geometry

!------------------------------------------------------------

FUNCTION nozzle_section_area(x) RESULT(Area)

   IMPLICIT NONE

   REAL(KIND=8), INTENT(IN) :: x
   REAL(KIND=8) :: Area

   ! Variabili globali usate: xi, xg, xu, Ai, Ag, Au

   IF (x <= xg) THEN

      ! Sezione convergente
      Area = cubic_zero_slopes(x, xi, Ai, xg, Ag)

      ! ATTENZIONE INEFFICIENZA: i coefficienti del polinomio
      ! cubico sono ricalcolati per ogni valore della coordinata x

   ELSE

      ! Sezione divergente
      Area = cubic_zero_slopes(x, xg, Ag, xu, Au)

   ENDIF


   CONTAINS


   FUNCTION cubic_zero_slopes(x, x0, A0, x1, A1) RESULT(Ax)

      ! Calcola il valore della funzione Ax = A(x) nel punto x
      ! dove A(x) è un polimonio di terzo grado in x passante
      ! per i punti (x0, A0) e (x1, A1) e con pendenza
      ! nulla in entrambi questi punti

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: x, x0, A0, x1, A1
      REAL(KIND=8) :: Ax

      REAL(KIND=8) :: alpha, a, b, c, d

      alpha = - (A1 - A0) / (x1 - x0)**3

      a =  2 * alpha
      b = -3 * alpha * (x0 + x1)
      c =  6 * alpha *  x0*x1
      d = (x1**2 * (x1 - 3*x0)*a0  -  x0**2 * (x0 - 3*x1)*a1) / (x1 - x0)**3

      Ax = ((a*x + b)*x + c)*x + d

   END FUNCTION cubic_zero_slopes


END FUNCTION nozzle_section_area

!------------------------------------------------------------

FUNCTION nozzle_section_area_derivative(x) RESULT(dArea_dx)

   IMPLICIT NONE

   REAL(KIND=8), INTENT(IN) :: x
   REAL(KIND=8) :: dArea_dx

   ! Variabili globali usate: xi, xg, xu, Ai, Ag, Au

   IF (x <= xg) THEN

      ! Sezione convergente
      dArea_dx = cubic_zero_slopes_derivative(x, xi, Ai, xg, Ag)

   ELSE

      ! Sezione divergente
      dArea_dx = cubic_zero_slopes_derivative(x, xg, Ag, xu, Au)

   ENDIF


   CONTAINS


   FUNCTION cubic_zero_slopes_derivative(x, x0, A0, x1, A1) RESULT(dA_dx)

      ! Calcola il valore della derivata dA(x)/dx nel punto x,
      ! dove A(x) è un polimonio di terzo grado in x passante
      ! per i punti (x0, A0) e (x1, A1) e con pendenza nulla
      ! in entrambi questi punti

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: x, x0, A0, x1, A1
      REAL(KIND=8) :: dA_dx

      REAL(KIND=8) :: alpha, a, b, c

      alpha = - (A1 - A0) / (x1 - x0)**3

      a =  2 * alpha
      b = -3 * alpha * (x0 + x1)
      c =  6 * alpha *  x0 * x1

      dA_dx = (3*a*x + 2*b)*x + c

   END FUNCTION cubic_zero_slopes_derivative

END FUNCTION nozzle_section_area_derivative



END MODULE nozzle_geometry
