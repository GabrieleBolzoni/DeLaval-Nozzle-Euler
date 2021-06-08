MODULE linearization

   USE Euler_flux_jacobian

   USE thermodynamics

   IMPLICIT NONE


CONTAINS


FUNCTION roe_jacobian(u, ht) RESULT (A)

   IMPLICIT NONE

   REAL(KIND=8), INTENT(IN) :: u, ht ! medie alla Roe
   REAL(KIND=8), DIMENSION(order, order) :: A

   a(1,1) = 0;                              a(1,2) = 1;                         a(1,3) = 0
   a(2,1) = ((gamma - 3)/2) * u**2;         a(2,2) = -(gamma - 3) * u;          a(2,3) = gamma - 1
   a(3,1) = ((gamma - 1)/2) * u**3 - ht*u;  a(3,2) = -(gamma - 1) * u**2 + ht;  a(3,3) = gamma * u

END FUNCTION roe_jacobian


! Valuto autovalori di Roe

FUNCTION roe_eigenvalues(u, ht) RESULT (lambda)

   IMPLICIT NONE

   REAL(KIND=8), INTENT(IN) :: u, ht ! medie alla Roe
   REAL(KIND=8), DIMENSION(order) :: lambda

   REAL(KIND=8) :: c

   c = SQRT((gamma - 1) * (ht - u**2/2))

   lambda(1) = u - c
   lambda(2) = u
   lambda(3) = u + c

END FUNCTION roe_eigenvalues


SUBROUTINE roe_eigenstructure (wl, wr,  A, lambda, L, R)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:),   INTENT(IN) :: wl, wr
   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: A, L, R
   REAL(KIND=8), DIMENSION(:),   INTENT(OUT) :: lambda

   REAL(KIND=8) :: rhol, ul, Etl, htl, Pl, Sl, &
                   rhor, ur, Etr, htr, Pr, Sr, &
                   u, ht, c, u2, gm1, cgm1

   ! Calcolo della velocità u tramite media pesata di Roe in cui il peso
   ! è la radice quadrata della densità

   rhol = wl(1);       rhor = wr(1)

   ul = wl(2)/rhol;    ur = wr(2)/rhor

   Etl = wl(3);        Etr = wr(3)

   Pl = Pgreco(wl);    Pr = Pgreco(wr)

   htl = (Etl + Pl) / rhol;      htr = (Etr + Pr) / rhor

   Sl = SQRT(rhol);   Sr = SQRT(rhor)

   u  = ( ul * Sl  +   ur * Sr)/(Sl + Sr) ! la media di Roe
   ht = (htl * Sl  +  htr * Sr)/(Sl + Sr) ! la media di Roe

   c = SQRT((gamma - 1) * (ht - u**2/2))

   A = roe_jacobian(u, ht)

   lambda = roe_eigenvalues(u, ht)

   gm1 = gamma - 1
   u2 = u**2

   cgm1 = c / gm1

   l(1,1) =  u2/2 + u*cgm1;   l(1,2) = -u - cgm1;   l(1,3) = 1
   l(2,1) = -u2 + 2*c*cgm1;   l(2,2) = 2*u;         l(2,3) = -2
   l(3,1) =  u2/2 - u*cgm1;   l(3,2) = -u + cgm1;   l(3,3) = 1

   L = L * (gm1 / (2 * c**2))

   r(1,1) = 1;          r(1,2) = 1;        r(1,3) = 1
   r(2,1) = u - c;      r(2,2) = u;        r(2,3) = u + c
   r(3,1) = ht - c*u;   r(3,2) = u**2/2;   r(3,3) = ht + c*u

END SUBROUTINE roe_eigenstructure


END MODULE linearization
