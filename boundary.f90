MODULE boundary

   USE Euler_flux_jacobian

   IMPLICIT NONE

CONTAINS


SUBROUTINE left_boundary (time, p_ext, A_l, wl,  F_bar)

   IMPLICIT NONE

   REAL(KIND=8),                      INTENT(IN)  :: time
   REAL(KIND=8), DIMENSION(:),        INTENT(IN)  :: p_ext, A_l, wl
   REAL(KIND=8), DIMENSION(SIZE(wl)), INTENT(OUT) :: F_bar

   REAL(KIND=8), DIMENSION(SIZE(wl), SIZE(wl)) :: L, A, R
   REAL(KIND=8), DIMENSION(SIZE(wl)) :: lambda, w_ext,  &
                                        Dw, Dv, Dv_bar, Dw_bar

   CALL eigenstructure (wl, A, lambda, L, R)

   w_ext = conservative(p_ext) * A_l
   Dw = w_ext - wl

   Dv = MATMUL(L, Dw) ! variazioni caratteristiche

   WHERE (lambda <= 0)
      Dv_bar = 0
   ELSEWHERE
      Dv_bar = Dv
   ENDWHERE

   Dw_bar = MATMUL(R, Dv_bar)

   F_bar = flux(wl + Dw_bar)

END SUBROUTINE left_boundary



SUBROUTINE right_boundary (time, p_ext, A_r, wr,  F_bar)

   IMPLICIT NONE

   REAL(KIND=8),                      INTENT(IN)  :: time
   REAL(KIND=8), DIMENSION(:),        INTENT(IN)  :: p_ext, A_r, wr
   REAL(KIND=8), DIMENSION(SIZE(wr)), INTENT(OUT) :: F_bar

   REAL(KIND=8), DIMENSION(SIZE(wr), SIZE(wr)) :: L, A, R
   REAL(KIND=8), DIMENSION(SIZE(wr)) :: lambda, w_ext,  &
                                        Dw, Dv, Dv_bar, Dw_bar

   CALL eigenstructure (wr,  A, lambda, L, R)

   w_ext = conservative(p_ext) * A_r
   Dw = w_ext - wr

   Dv = MATMUL(L, Dw) ! variazioni caratteristiche

   WHERE (lambda >= 0)
      Dv_bar = 0
   ELSEWHERE
      Dv_bar = Dv
   END WHERE

   Dw_bar = MATMUL(R, Dv_bar)

   F_bar = flux(wr + Dw_bar)

END SUBROUTINE right_boundary


END MODULE boundary
