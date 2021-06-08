MODULE Euler_flux_jacobian

   USE thermodynamics

   IMPLICIT NONE

   INTEGER, PARAMETER :: order = 3

   INTERFACE flux

      MODULE PROCEDURE flux_v, flux_a

   END INTERFACE flux

   PRIVATE flux_v, flux_a


   INTERFACE jacobian

      MODULE PROCEDURE jacobian_v, jacobian_a

   END INTERFACE jacobian

   PRIVATE jacobian_v, jacobian_a


   INTERFACE Pgreco

      MODULE PROCEDURE Pgreco_v, Pgreco_a

   END INTERFACE Pgreco

   PRIVATE Pgreco_v, Pgreco_a


   INTERFACE dPgreco_dw

      MODULE PROCEDURE dPgreco_dw_v, dPgreco_dw_a

   END INTERFACE dPgreco_dw

   PRIVATE dPgreco_dw_v, dPgreco_dw_a


   INTERFACE eigenvalues

      MODULE PROCEDURE eigenvalues_v, eigenvalues_a

   END INTERFACE eigenvalues

   PRIVATE eigenvalues_v, eigenvalues_a


   INTERFACE conservative

      MODULE PROCEDURE conservative_v, conservative_a

   END INTERFACE conservative

   PRIVATE conservative_v, conservative_a


   INTERFACE primitive

      MODULE PROCEDURE primitive_v, primitive_a

   END INTERFACE primitive

   PRIVATE primitive_v, primitive_a



CONTAINS

FUNCTION flux_v(w) RESULT (f)

	! w è il vettore delle variabili conservative

	IMPLICIT NONE

	REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w
	REAL(KIND=8), DIMENSION(SIZE(w)) :: f

	REAL(KIND=8) :: u, P

	u = w(2)/w(1)
	P = Pgreco(w)

	f(1) = w(2)
	f(2) = w(2)*u + P
	f(3) = (w(3) + P) * u

END FUNCTION flux_v


FUNCTION flux_a(w) RESULT (f)

	IMPLICIT NONE

	REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: w
	REAL(KIND=8), DIMENSION(SIZE(w,1), SIZE(w,2)) :: f

	REAL(KIND=8), DIMENSION(SIZE(w,2)) :: u, P

	u = w(2,:)/w(1,:)
	P = Pgreco(w)

	f(1,:) = w(2,:)
	f(2,:) = w(2,:)*u + P
	f(3,:) = (w(3,:) + P) * u

END FUNCTION flux_a


FUNCTION jacobian_v(w) RESULT (A)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w !vettore di stato
   REAL(KIND=8), DIMENSION(SIZE(w), SIZE(w)) :: A

   REAL(KIND=8), DIMENSION(SIZE(w)) :: dP_dw

   REAL(KIND=8) :: rho, m, Et, u, ht

   rho = w(1)
   m   = w(2)
   Et  = w(3)

   u  = m/rho
   ht = (Et + Pgreco(w)) / rho
   ! dobbiamo calcolare le derivate della pressione: dPg_drho, dPg_dm, dPg_dEt

   dP_dw = dPgreco_dw(w)

   a(1,1) = 0;                   a(1,2) = 1;                 a(1,3) = 0
   a(2,1) =  dP_dw(1) - u**2;    a(2,2) = dP_dw(2) + 2*u;    a(2,3) =  dP_dw(3)
   a(3,1) = (dP_dw(1) - ht)*u;   a(3,2) = dP_dw(2)*u + ht;   a(3,3) = (dP_dw(3) + 1)*u

END FUNCTION jacobian_v


FUNCTION jacobian_a(w) RESULT (A)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: w !vettore di stato
   REAL(KIND=8), DIMENSION(SIZE(w,1), SIZE(w,1), SIZE(w,2)) :: A

   REAL(KIND=8), DIMENSION(SIZE(w,1), SIZE(w,2)) :: dP_dw

   REAL(KIND=8), DIMENSION(SIZE(w,2)) :: rho, m, Et, u, ht

   rho = w(1,:)
   m   = w(2,:)
   Et  = w(3,:)

   u  = m/rho
   ht = (Et + Pgreco(w)) / rho

   ! dobbiamo calcolare le derivate della pressione: dPg_drho, dPg_dm, dPg_dEt

   dP_dw = dPgreco_dw(w)

   a(1,1,:) = 0;                     a(1,2,:) = 1;                   a(1,3,:) = 0
   a(2,1,:) =  dP_dw(1,:) - u**2;    a(2,2,:) = dP_dw(2,:) + 2*u;    a(2,3,:) =  dP_dw(3,:)
   a(3,1,:) = (dP_dw(1,:) - ht)*u;   a(3,2,:) = dP_dw(2,:)*u + ht;   a(3,3,:) = (dP_dw(3,:) + 1)*u

END FUNCTION jacobian_a



FUNCTION Pgreco_v(w) RESULT(P)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w
   REAL(KIND=8) :: P

   REAL(KIND=8) :: e, rho, u

   rho = w(1)
   u   = w(2)/w(1)
   e   = w(3)/rho - u**2/2

   P = pressure(e, rho)

END FUNCTION Pgreco_v


FUNCTION Pgreco_a(w) RESULT(P)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: w
   REAL(KIND=8), DIMENSION(SIZE(w,2)) :: P

   REAL(KIND=8), DIMENSION(SIZE(w,2)) :: rho, u, e

   rho = w(1,:)
   u   = w(2,:)/w(1,:)
   e   = w(3,:)/rho - u**2/2

   P = pressure(e, rho)

END FUNCTION Pgreco_a


FUNCTION dPgreco_dw_v(w) RESULT(dP_dw)   ! vettore delle derivate parziali

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w
   REAL(KIND=8), DIMENSION(SIZE(w)) :: dP_dw

   REAL(KIND=8) :: rho, u, Et, e, dP_de, dP_drho

   rho = w(1)
   u   = w(2)/w(1)
   Et  = w(3)
   e   = Et/rho - u**2/2

   ! P = pressure(e, rho)

   CALL dpressure_de_drho(e, rho,  dP_de, dP_drho)

   dP_dw(1) = dP_de*(-Et/rho**2 + u**2) + dP_drho
   dP_dw(2) = -dP_de*u/rho
   dP_dw(3) = dP_de/rho

END FUNCTION dPgreco_dw_v


FUNCTION dPgreco_dw_a(w) RESULT(dP_dw)   ! vettore delle derivate parziali

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: w
   REAL(KIND=8), DIMENSION(SIZE(w,1), SIZE(w,2)) :: dP_dw

   REAL(KIND=8), DIMENSION(SIZE(w,2)) :: rho, u, Et, e, dP_de, dP_drho

   rho = w(1,:)
   u   = w(2,:)/w(1,:)
   Et  = w(3,:)

   e = Et/rho - u**2/2

   ! P = pressure(e, rho)

   CALL dpressure_de_drho(e, rho,  dP_de, dP_drho)

   dP_dw(1,:) = dP_de*(-Et/rho**2 + u**2) + dP_drho
   dP_dw(2,:) = -dP_de*u/rho
   dP_dw(3,:) = dP_de/rho

END FUNCTION dPgreco_dw_a


FUNCTION eigenvalues_v(w) RESULT (lambda) !vettore degli autovalori   ordinati in modo crescente

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w !vettore di stato
   REAL(KIND=8), DIMENSION(SIZE(w)) :: lambda

   REAL(KIND=8) :: rho, u, Et, e, c

   rho = w(1)
   u  = w(2)/w(1)
   Et = w(3)

   e  = Et/rho - u**2/2
   c  = sound_speed(e, rho)

   lambda(1) = u - c
   lambda(2) = u
   lambda(3) = u + c

END FUNCTION eigenvalues_v


FUNCTION eigenvalues_a(w) RESULT (lambda) !vettore degli autovalori   ordinati in modo crescente

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: w !vettore di stato
   REAL(KIND=8), DIMENSION(SIZE(w,1), SIZE(w,2)) :: lambda

   REAL(KIND=8), DIMENSION(SIZE(w,2)) :: rho, u, Et, e, c

   rho = w(1,:)
   u  = w(2,:)/w(1,:)
   Et = w(3,:)

   e = Et/rho - u**2/2
   c = sound_speed(e, rho)

   lambda(1,:) = u - c
   lambda(2,:) = u
   lambda(3,:) = u + c

END FUNCTION eigenvalues_a


SUBROUTINE eigenstructure (w,  A, lambda, L, R)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:),   INTENT(IN)  :: w

   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: A, L, R
   REAL(KIND=8), DIMENSION(:),   INTENT(OUT) :: lambda

   A = jacobian(w)

   lambda = eigenvalues(w)

   L = matrix_left_eigenvectors(w)
   R = matrix_right_eigenvectors(w)

END SUBROUTINE eigenstructure


FUNCTION matrix_right_eigenvectors(w) RESULT (R)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w !vettore di stato
   REAL(KIND=8), DIMENSION(SIZE(w),SIZE(w)) :: R

   REAL(KIND=8), DIMENSION(SIZE(w)) :: P_w

   REAL(KIND=8) :: rho, m, Et, u, e, c, ht, ratio

   rho = w(1);   m = w(2);   Et = w(3)

   u = m / rho

   e = Et/rho - u**2/2
   c = sound_speed(e, rho)
   ht = (Et + Pgreco(w)) / rho

   P_w = dPgreco_dw(w)

   ratio = P_w(1) / P_w(3)

   r(1,1) = rho;            r(1,2) = rho;    r(1,3) = rho

   r(2,1) = m - rho*c;      r(2,2) = m;      r(2,3) = m + rho*c

   r(3,1) = rho*ht - m*c;   r(3,2) = m**2/rho - rho * ratio

                                             r(3,3) = rho*ht + m *c

END FUNCTION matrix_right_eigenvectors


FUNCTION matrix_left_eigenvectors(w) RESULT (L)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w !vettore di stato
   REAL(KIND=8), DIMENSION(SIZE(w),SIZE(w)) :: L

   REAL(KIND=8), DIMENSION(SIZE(w)) :: P_w

   REAL(KIND=8) :: rho, m, Et, u, e, c

   rho = w(1);   m = w(2);   Et = w(3)

   u = m / rho

   e = Et/rho - u**2/2
   c = sound_speed(e, rho)

   P_w = dPgreco_dw(w)


   l(1,1) = P_w(1) + u*c;          l(1,2) = P_w(2) - c;     l(1,3) = P_w(3)

   l(2,1) = -2 * (P_w(1) - c**2);  l(2,2) = -2 * P_w(2);    l(2,3) = -2 * P_w(3)

   l(3,1) = P_w(1) - c*u;          l(3,2) = P_w(2) + c;     l(3,3) = P_w(3)

   L = L / (2*rho*c**2)

END FUNCTION matrix_left_eigenvectors

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION conservative_v(p) RESULT(w)

   ! p(1) = rho,   p(2) = u,   p(3) = P

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: p
   REAL(KIND=8), DIMENSION(SIZE(p)) :: w

   w(1) = p(1)

   w(2) = p(1)*p(2)

                ! specific_energy(P, rho)
   w(3) = p(1) * (specific_energy(p(3), p(1))  +  p(2)**2 / 2)

END FUNCTION conservative_v


FUNCTION conservative_a(p) RESULT(w)

   ! p(1) = rho,   p(2) = u,   p(3) = P

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: p
   REAL(KIND=8), DIMENSION(SIZE(p,1), SIZE(p,2)) :: w

   w(1,:) = p(1,:)

   w(2,:) = p(1,:)*p(2,:)

                ! specific_energy(P, rho)
   w(3,:) = p(1,:) * (specific_energy(p(3,:), p(1,:))  +  p(2,:)**2 / 2)

END FUNCTION conservative_a


FUNCTION primitive_v(w) RESULT(p)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w
   REAL(KIND=8), DIMENSION(SIZE(w)) :: p
   REAL(KIND=8) :: e ! Internal energy

   p(1) = w(1)
   p(2) = w(2)/w(1)
   e = w(3)/w(1)  -  p(2)**2 / 2

   p(3) = pressure(e, w(1))

END FUNCTION primitive_v


FUNCTION primitive_a(w) RESULT(p)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: w
   REAL(KIND=8), DIMENSION(SIZE(w,1),SIZE(w,2)) :: p
   REAL(KIND=8), DIMENSION(SIZE(w,2)) :: e ! Internal energy

   p(1,:) = w(1,:)
   p(2,:) = w(2,:)/w(1,:)
   e = w(3,:)/w(1,:) - p(2,:)**2 / 2

          ! pressure(e, rho)
   p(3,:) = pressure(e, w(1,:))

END FUNCTION primitive_a


END MODULE Euler_flux_jacobian
