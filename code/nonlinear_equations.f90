MODULE nonlinear_equations

   IMPLICIT NONE

! Preambolo 


   INTERFACE bisection
   
      MODULE PROCEDURE bisection_, bisection_shift
      
   END INTERFACE bisection
   
   
   PRIVATE :: bisection_, bisection_shift


CONTAINS

FUNCTION bisection_(f, a, b, rel_tol) RESULT(root)          

   IMPLICIT NONE

   ! Calcolo della radice "root" della funzione f(x) = 0 ricercata
   ! nell'intervallo [a,b] mediante il metodo di bisezione

   INTERFACE     ! f è una fottutissima funzione

      FUNCTION f(x) RESULT(f_value)
          REAL(KIND=8), INTENT(IN) :: x
          REAL(KIND=8) :: f_value
      END FUNCTION f

   END INTERFACE

   REAL(KIND=8), INTENT(IN) :: a, b, rel_tol
   REAL(KIND=8) :: root

 


   ! Dichiarazione variabili interne

   INTEGER      :: nmax = 1000, n
   REAL(KIND=8) :: xa, xb, xm, fa, fb, fm

   fa = f(a)
   
   IF (fa == 0) THEN    ! È deprecabile il confronto di uguaglianza tra quantità reali
      root = a
      RETURN
   ENDIF

   fb = f(b)

   IF (fb == 0) THEN    ! È deprecabile il confronto di uguaglianza tra quantità reali
      root = b
      RETURN
   ENDIF

   IF (fa*fb > 0) THEN
      WRITE(*,*) 'I valori della funzione agli estremi hanno lo stesso segno'
      WRITE(*,*) 'Il metodo di bisezione non può funzionare: STOP '
      STOP
   ENDIF

   
   xa = a;  xb = b
   
   DO n = 1, nmax
      
      xm = (xa + xb)/2
      fm = f(xm)
      
      IF (ABS(xb-xa) <= rel_tol*ABS(xm)) THEN 
         
         root = xm
         RETURN
      
      ENDIF
       
       
      IF (fa*fm > 0) THEN
      
         xa = xm   !;   fa = fm ! picchieremo a sangue Quartapelle
         
      ELSE
      
         xb = xm   !;   fb = fm
      
      ENDIF
      
   
   ENDDO
   
   
   WRITE(*,*) 'Numero di iterazioni massime ', nmax, ' raggiunto '
   WRITE(*,*) 'STOP in FUNCTION bisection'
   STOP      
   
   
END FUNCTION bisection_


FUNCTION bisection_shift(f, f_shift, a, b, rel_tol) RESULT(root)          

   IMPLICIT NONE

   ! Calcolo della radice "root" della funzione f(x) = 0 ricercata
   ! nell'intervallo [a,b] mediante il metodo di bisezione

   INTERFACE     ! f è una fottutissima funzione

      FUNCTION f(x) RESULT(f_value)
          REAL(KIND=8), INTENT(IN) :: x
          REAL(KIND=8) :: f_value 
      END FUNCTION f

   END INTERFACE

   REAL(KIND=8), INTENT(IN) :: f_shift, a, b, rel_tol
   REAL(KIND=8) :: root

 


   ! Dichiarazione variabili interne

   INTEGER :: nmax = 1000, n
   REAL(KIND=8) :: xa, xb, xm, fa, fb, fm

   fa = f(a) - f_shift
   
   IF (fa == 0) THEN    ! È deprecabile il confronto di uguaglianza tra quantità reali
      root = a
      RETURN
   ENDIF

   fb = f(b) - f_shift

   IF (fb == 0) THEN    ! È deprecabile il confronto di uguaglianza tra quantità reali
      root = b
      RETURN
   ENDIF

   IF (fa*fb > 0) THEN
      WRITE(*,*) 'I valori della funzione agli estremi hanno lo stesso segno'
      WRITE(*,*) 'Il metodo di bisezione non può funzionare: STOP '
      STOP
   ENDIF

   
   xa = a;  xb = b
   
   DO n = 1, nmax
      
      xm = (xa + xb)/2
      fm = f(xm) - f_shift
      
      IF (ABS(xb - xa) <= rel_tol*ABS(xm)) THEN 
         
         root = xm
         RETURN
      
      ENDIF
       
       
      IF (fa*fm > 0) THEN
      
         xa = xm   !;   fa = fm 
         
      ELSE
      
         xb = xm   !;   fb = fm
      
      ENDIF
      
   
   ENDDO
   
   
   WRITE(*,*) 'Numero di iterazioni massime ', nmax, ' raggiunto '
   WRITE(*,*) 'STOP in FUNCTION bisection_shift'
   STOP      
   
   
END FUNCTION bisection_shift


END MODULE nonlinear_equations
