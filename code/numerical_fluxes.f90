MODULE numerical_fluxes

   USE Euler_flux_jacobian

   USE exact_riemann_problem

  USE linearization ! USED solo in Roe e nell'alta risoluzione

  USE thermodynamics

   IMPLICIT NONE

CONTAINS

! flusso di Godunov


SUBROUTINE Godunov_flux_STUD (W,  F_G) ! STUD

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN)  :: W

   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: F_G

   REAL(KIND=8), DIMENSION(SIZE(W,1)) :: wl, wr, w_vert,  &
                                         lambda_l, lambda_r, wCl, wCr
   INTEGER :: i, jl, jr

   REAL(KIND=8), PARAMETER :: rel_tol = 1.0d-8


   DO i = 1, SIZE(F_G, 2)

      jl = i;   jr = i + 1

      wl = W(:,jl);      wr = W(:,jr)

      ! controlliamo che ci sia salto
      IF (SQRT(SUM((wr - wl)**2))  <  SQRT(SUM((wl + wr)**2)) * rel_tol) THEN

         w_vert = (wl + wr)/2

      ELSE

write (*,*) 'i = ', i
         ! calcoliamo gli autovalori, funzione eigenvalues data dall'esterno
         lambda_l = eigenvalues(wl)
         lambda_r = eigenvalues(wr)

         ! escludiamo i casi semplici e, se serve, risolviamo riemann
         IF (lambda_l(1) > 0  .AND.  lambda_r(1) > 0) THEN

            w_vert = wl ! tira tutto a destra

         ELSEIF (lambda_l(3) < 0  .AND.  lambda_r(3) < 0) THEN

            w_vert = wr ! tira tutto a sinistra

         ELSE ! necessario risolvere il problema di Riemann

            CALL exact_riemann (wl, wr,  wCl, wCr)

            w_vert = vertical_state(wl, wCl, wCr, wr)

         ENDIF

      ENDIF

      F_G(:,i) = flux(w_vert)

   END DO


CONTAINS ! INTERNAL FUNCTION AND SUBROUTINE


FUNCTION vertical_state(wl, wCl, wCr, wr) RESULT(w_vert)

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: wl, wCl, wCr, wr
   REAL(KIND=8), DIMENSION(SIZE(wl)) :: w_vert

   REAL(KIND=8), DIMENSION(SIZE(wl)) :: lambda_l,  lambda_Cl, lambda_Cr,  lambda_r

   REAL(KIND=8) :: rhol, rho_Cl, rho_Cr, rhor, ml, m_Cl, m_Cr, mr, &
                   sl_b,  sl_e,  sr_b,  sr_e, s_c

   rhol   = wl(1);       ml = wl(2)
   rho_Cl = wCl(1);    m_Cl = wCl(2)
   rho_Cr = wCr(1);    m_Cr = wCr(2)
   rhor   = wr(1);       mr = wr(2)

   lambda_l = eigenvalues(wl)
   lambda_Cl = eigenvalues(wCl)
   lambda_Cr = eigenvalues(wCr)
   lambda_r = eigenvalues(wr)

   ! PRIMA ONDA: sistemiamo inizio/fine del ventaglio o urto

   IF (rho_Cl > rhol) THEN ! urto

      sl_b = (m_Cl - ml) / (rho_Cl - rhol)

      sl_e = sl_b

   ELSE ! rarefazione

      sl_b = lambda_l(1)
      sl_e = lambda_Cl(1)

   END IF


   ! CONTATTO
   s_c = lambda_Cl(2)

   ! terza ONDA: sistemiamo inizio/fine ventaglio o urto

   IF (rho_Cr > rhor) THEN ! urto

      sr_b = (mr - m_Cr) / (rhor - rho_Cr)

      sr_e = sr_b

   ELSE ! rarefazione

      sr_b = lambda_Cr(3)
      sr_e = lambda_r(3)

   END IF


   IF (sl_e < 0  .AND.  sr_b > 0) THEN

      IF (s_c >= 0) THEN
          w_vert = wCl
      ELSE
          w_vert = wCr
      ENDIF

   ELSEIF (sl_b > 0) THEN

      w_vert = wl

   ELSEIF (sr_e < 0) THEN

      w_vert = wr

   ELSEIF (sl_e > 0) THEN ! ventaglio transonico sinistra

      w_vert = transonic_rarefaction(1, wl)

   ELSEIF (sr_b < 0) THEN ! ventaglio transonico a destra

      w_vert = transonic_rarefaction(3, wr)

   END IF


END FUNCTION vertical_state


FUNCTION transonic_rarefaction(i, w) RESULT(ws) ! Sonic means: csi = 0

    IMPLICIT NONE

    INTEGER,                    INTENT(IN) :: i
    REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w

    REAL(KIND=8), DIMENSION(SIZE(w)) :: prim, ws
    REAL(KIND=8) :: rho_rar, u_rar, P_rar

  ! lambda = eigenvalues(w)
    prim = primitive(w)
  ! e = w(3)/w(1)  -  (w(2)/w(1))**2 / 2

    rho_rar = rarefaction_density (i, w, 0.0d00)
    u_rar   = rarefaction_velocity(i, w, 0.0d00)
    P_rar   = pressure_RAR(prim(3), 1/w(1), 1/rho_rar)

    ws = conservative([rho_rar, u_rar, P_rar])

END FUNCTION transonic_rarefaction


END SUBROUTINE Godunov_flux_STUD



SUBROUTINE  godunov_flux (W,  F_G)

   ! ORDERED GRID

   ! Given the array  of the cell average of the conservative
   ! variables, the program calculates the Godunov vertical flux
   ! for the Euler equations of gasdynamics

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN)  :: W
   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: F_G

   REAL(KIND=8), DIMENSION(SIZE(W,1)) :: wl, wr, wCl, wCr, w_vert,  &
                                         lambda_l, lambda_r, lambda

   REAL(KIND=8) :: vl, ul,  vr, ur,  vCl, vCr, uC,  ss ! shock speed

   INTEGER :: i, j, jl, jr


   DO i = 1, SIZE(F_G, 2);   j = i;   jl = i;   jr = i + 1

      ! Special situations in which the solution
      ! of the Riemann problem can be avoided

      wl = W(:, jl);   wr = W(:, jr)

      ! skip the solution of the Riemann problem
      ! when the two states are identical

      IF (SQRT(SUM((wr - wl)**2))  <  &
          SQRT(SUM((wl + wr)**2)) * 1.0d-13) THEN ! ZERO JUMP

          w_vert = wl ! IDEM:  wr

          F_G(:, i) = flux(w_vert)

          CYCLE

      ENDIF

write (*,*) 'i = ', i

      vl = 1/wl(1);   ul = wl(2)/wl(1)

      ! Solution of the Riemann problem

      CALL exact_Riemann (wl, wr,  wCl, wCr)

      uC = wCl(2)/wCl(1) ! ;  PC = Pi(wCl)


      IF (uC > 0) THEN  ! The contact discontinuity propagates to
                        ! the right: the third wave is not relevant

         vCl = 1/wCl(1)

         IF (vCl < vl) THEN ! LEFT SHOCK

            ! Compute the shock speed
            ss = (vl*uC - vCl*ul)/(vl - vCl)

            IF (ss > 0) THEN ! right propagating shock
               w_vert = wl
            ELSE             ! left propagating shock
               w_vert = wCl
            ENDIF

         ELSE ! LEFT RAREFACTION

            lambda_l = eigenvalues(wl)
            lambda   = eigenvalues(wCl)

            IF (lambda_l(1) >= 0) THEN ! right propagating fan
               w_vert = wl
            ELSEIF (lambda(1) <= 0) THEN ! left propagating fan
               w_vert = wCl
            ELSE ! transonic rarefaction for the first wave
               w_vert = ws_sonic_state(1, wl)
            ENDIF

         ENDIF ! alternative shock  vs  fan for the first wave


      ELSE ! (uC < 0) The contact discontinuity propagates to
           !          the left: the first wave is not relevant

         vCr = 1/wCr(1)

         IF (vCr < vr) THEN ! RIGHT SHOCK

            ! Compute the shock speed
            ss = (vr*uC - vCr*ur)/(vr - vCr)

            IF (ss > 0) THEN ! right propagating shock
              w_vert = wCr
            ELSE             ! left propagating shock
              w_vert = wr
            ENDIF

         ELSE ! RIGHT RAREFACTION

            lambda   = eigenvalues(wCr)
            lambda_r = eigenvalues(wr)

            IF (lambda(3) >= 0) THEN ! right propagating fan
               w_vert = wCr
            ELSEIF (lambda_r(3) <= 0) THEN ! left propagating fan
               w_vert = wr
            ELSE ! transonic rarefaction for the third wave
               w_vert = ws_sonic_state(3, wr)
            ENDIF

        ENDIF ! alternative shock  vs  fan for the third wave


      ENDIF ! alternative uC >  or  uC <= 0

      F_G(:, i) = flux(w_vert)

   ENDDO ! cycle on the interfaces


END SUBROUTINE  godunov_flux



! flusso di Roe

SUBROUTINE roe_flux (W,  F_R)

   USE linearization

   IMPLICIT NONE

   REAL(KIND=8), DIMENSION(:,:), INTENT(IN)  :: W      ! W(order, n_p)
   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: F_R    ! F_R(order, n_i)

   ! Dichiarazione delle variabili interne
   REAL(KIND=8), DIMENSION(SIZE(W,1), SIZE(W,2)) :: f ! tabella

   REAL(KIND=8), DIMENSION(SIZE(W,1), SIZE(W,1)) :: At, Rt, Lt, &
                                                    ABS_at, ABS_lambdat
   REAL(KIND=8), DIMENSION(SIZE(W,1)) :: lambdat, A_lambdat, Dv, w_cl, w_cr, &
                                         lambda_l, lambda_r, lambda_cl, lambda_cr
   REAL(KIND=8) :: N_l, N_r, P_l, P_r


   REAL(KIND=8) :: rhol, rhor, ul, ur, sul, sur, ut
   INTEGER      :: i, jl, jr, k
   LOGICAL, PARAMETER :: ENTROPY_FIX = .true.

   f = flux(W) ! flussi nodali

   ABS_lambdat = 0.0d00

   ! Ciclo sulle interfacce

   DO i = 1, SIZE(F_R, 2)

      ! Calcolo della velocità u tramite media pesata di Roe in cui il peso
      ! è la radice quadrata della densità

      jl = i;   jr = i + 1

      CALL roe_eigenstructure (W(:,jl), W(:,jr), At, lambdat, Lt, Rt)

      A_lambdat = ABS(lambdat)

      IF (ENTROPY_FIX) THEN
         Dv = MATMUL(Lt, w(:,jr) - w(:,jl))
         w_cl = w(:,jl) + Rt(:,1)*Dv(1)
         w_cr = w(:,jr) - Rt(:,3)*Dv(3)

         lambda_l = eigenvalues(w(:,jl))
         lambda_r = eigenvalues(w(:,jr))

         lambda_cl = eigenvalues(w_cl)
         lambda_cr = eigenvalues(w_cr)

         !lambda_i = eigenvalues(wi)

         N_l = MIN(lambda_l(1), 0.d00);    N_r = MIN(lambda_cr(3), 0.d00)
         P_l = MAX(lambda_cl(1), 0.d00);   P_r = MAX(lambda_r(3), 0.d00)

         IF (N_l /= P_l) &
         A_lambdat(1) = ((N_l + P_l)*lambdat(1) - 2*N_l*P_l)/(P_l - N_l)
         IF (N_r /= P_r) &
         A_lambdat(3) = ((N_r + P_r)*lambdat(3) - 2*N_r*P_r)/(P_r - N_r)
      ENDIF


      DO k = 1, SIZE(lambdat)
        ABS_lambdat(k,k) = A_lambdat(k)
      END DO


      ABS_at = MATMUL(MATMUL(Rt, ABS_lambdat), Lt)

      F_R(:,i) = (f(:,jl) + f(:,jr)) / 2 &
                 - MATMUL(ABS_at, W(:,jr) - W(:,jl)) / 2

   END DO

END SUBROUTINE roe_flux



! Lax-Wendroff conservativo

SUBROUTINE LWc_flux (Dt, Dx, W,  F_LW)

   REAL(KIND=8),                 INTENT(IN)  :: Dt, Dx  ! reticolo uniforme
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN)  :: W ! W(order, n_p)
   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: F_LW
                                              ! F_LW(order,n_i)

   REAL(KIND=8), DIMENSION(SIZE(W,1), SIZE(W,2)) :: f ! tabella
   REAL(KIND=8), DIMENSION(SIZE(W,1), SIZE(W,1)) :: A_ave

   INTEGER :: i, jl, jr

   f = flux(W) ! calcolo dei flussi nodali, una volta per tutte

   DO i = 1, SIZE(F_LW, 2)

      jl = i;   jr = i + 1

      A_ave = jacobian((W(:,jl) + W(:,jr)) / 2)

      F_LW(:,i) = (f(:,jl) + f(:,jr)) / 2 &

                - (Dt/(2*Dx)) * MATMUL(A_ave, f(:,jr) - f(:,jl))

   END DO

END SUBROUTINE LWc_flux

!Flusso numerico ad alta risoluzione

SUBROUTINE UHR_flux (Dt, Dx, W,  F_HR, limiter)

   USE linearization

   REAL(KIND=8),                 INTENT(IN)  :: Dt, Dx  ! reticolo uniforme
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN)  :: W
   REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: F_HR
   INTEGER,                      INTENT(IN)  :: limiter

   REAL(KIND=8), DIMENSION(SIZE(W,1), SIZE(W,2)) :: f

   REAL(KIND=8), DIMENSION(SIZE(W,1), SIZE(W,1)) :: At, Rt, Lt, &
                                                    ABS_At, ABS_lambdat

   REAL(KIND=8), DIMENSION(SIZE(W,1)) :: wl, wr, w_cl, w_cr,  Dv, Dvl, Dvr,  &
                                         lambda_l, lambda_r, lambda_cl, lambda_cr,  &
                                         lambdat, A_lambdat

   REAL(KIND=8) :: N_l,  N_r,  P_l,  P_r,  Dvc_p,  Dvupw_p

   INTEGER :: i, j, jl, jr, k, p

   LOGICAL, PARAMETER :: ENTROPY_FIX = .true.


   f = flux(W) ! flussi nodali

   ABS_lambdat = 0

   ! Ciclo sulle interfacce

   DO i = 1, SIZE(F_HR, 2);  j = i !sfasatura di 1/2

      ! Calcolo della velocità u tramite media pesata di Roe
      ! in cui il peso è la radice quadrata della densità

      jl = j;   jr = j + 1

      wl = W(:,jl);   wr = W(:,jr)

      CALL roe_eigenstructure (wl, wr,  At, lambdat, Lt, Rt)

      A_lambdat = ABS(lambdat)

      IF (ENTROPY_FIX) THEN
         Dv = MATMUL(Lt, W(:,jr) - W(:,jl))
         w_cl = W(:,jl) + Rt(:,1)*Dv(1)
         w_cr = W(:,jr) - Rt(:,3)*Dv(3)

         lambda_l = eigenvalues(W(:,jl))
         lambda_r = eigenvalues(W(:,jr))

         lambda_cl = eigenvalues(w_cl)
         lambda_cr = eigenvalues(w_cr)

         !lambda_i = eigenvalues(wi)

         N_l = MIN(lambda_l(1), 0.d00);    N_r = MIN(lambda_cr(3), 0.d00)
         P_l = MAX(lambda_cl(1), 0.d00);   P_r = MAX(lambda_r(3), 0.d00)

         IF (N_l /= P_l) &
         A_lambdat(1) = ((N_l + P_l)*lambdat(1) - 2*N_l*P_l)/(P_l - N_l)
         IF (N_r /= P_r) &
         A_lambdat(3) = ((N_r + P_r)*lambdat(3) - 2*N_r*P_r)/(P_r - N_r)
      ENDIF

      DO k = 1, SIZE(lambdat)
        ABS_lambdat(k,k) = A_lambdat(k)
      END DO


      ABS_At = MATMUL(MATMUL(Rt, ABS_lambdat), Lt)

      ! contributo del flusso numerico di basso ordine

      F_HR(:,i) = (f(:,jl) + f(:,jr)  -  MATMUL(ABS_At, wr - wl)) / 2

      ! calcolo della correzione del secondo ordine limitata

      Dv = MATMUL(Lt, wr - wl)

      IF (i /= 1) THEN
          Dvl = MATMUL(Lt, wl - W(:,jl-1))
      ELSE
          Dvl = Dv ! prima interfaccia: estrapolazione lineare
      ENDIF

      IF (i /= SIZE(F_HR,2)) THEN
          Dvr = MATMUL(Lt, W(:,jr+1) - wr)
      ELSE
          Dvr = Dv ! ultima interfaccia: estrapolazione lineare
      ENDIF


      DO p = 1, SIZE(W,1)

         Dvc_p = Dv(p)

         Dvupw_p = (Dvl(p) + Dvr(p))/2  +  (Dvl(p) - Dvr(p)) * SIGN(1.d0, lambdat(p))/2


         F_HR(:,i) = F_HR(:,i)  +  Rt(:,p) * ((A_lambdat(p) - (Dt/Dx) * lambdat(p)**2) &

                                              * psi_lim(Dvc_p, Dvupw_p, limiter))/ 2

      END DO


   END DO !ciclo sulle interfacce


END SUBROUTINE UHR_flux

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION  psi_lim(a, b, limiter)  RESULT(psi)

   ! Limiter function in Rebay's form, as a function
   ! of two variables:  a = Du^centred,  b = Du^upwind
   !
   ! limiter is an OPTIONAL parameter
   !
   !    DEFAULT     --->  van Leer
   !
   !  limiter == -2 --->  Second-order scheme
   !  limiter == 0  --->  no limiter, first-order upwind
   !
   !  limiter == 1  --->  van Leer
   !  limiter == 2  --->  minmod
   !  limiter == 3  --->  superbee
   !  limiter == 4  --->  Monotonized Central
   !

   IMPLICIT NONE

   REAL(KIND=8),      INTENT(IN) :: a, b
   INTEGER, OPTIONAL, INTENT(IN) :: limiter
   REAL(KIND=8) :: psi

   REAL (KIND=8), PARAMETER :: zero = 0,  half = 0.5d0

   IF (PRESENT(limiter)) THEN

      SELECT CASE(limiter)

         CASE(-2) ! Second-order scheme
             psi = a

         CASE(0)  ! no limiter, first-order upwind
             psi = 0

         CASE(1)  ! van Leer
             psi = (a*ABS(b) + ABS(a)*b)/(ABS(a) + ABS(b) + 1.0d-8)

         CASE(2)  ! minmod
             psi = (SIGN(half,a) + SIGN(half,b)) * MIN(ABS(a), ABS(b))

         CASE(3)  ! superbee
             psi = (SIGN(half,a) + SIGN(half,b))  &
                 *  MAX( MIN(ABS(a), 2*ABS(b)),  MIN(2*ABS(a), ABS(b)) )

         CASE(4)  ! Monotonized Central
             psi = MAX( zero,  MIN((a+b)/2, 2*a, 2*b) )  &
                 + MIN( zero,  MAX((a+b)/2, 2*a, 2*b) )

         CASE DEFAULT
             WRITE (*,*) 'Unknown limiter specified';  STOP

      END SELECT

   ELSE  ! default limiter: van Leer

      psi = (a*ABS(b) + ABS(a)*b)/(ABS(a) + ABS(b) + 1.0d-8)

   ENDIF

END FUNCTION  psi_lim




END MODULE numerical_fluxes
