MODULE ugello

   USE compressible_flow_lib
   USE nozzle_geometry
   USE nonlinear_equations

   IMPLICIT NONE

   REAL(KIND=8) :: P_0 = 1d00 * 101325
   REAL(KIND=8) :: T_0 = 295d00
   REAL(KIND=8) :: P_amb = 8d-1 * 101325
   REAL(KIND=8) :: R = 287d00
   REAL(KIND=8) :: rho_0


 CONTAINS

  SUBROUTINE exact_nozzle (x, AA,  M_ex, P_ex, T_ex, rho_ex)

     IMPLICIT NONE


     REAL(KIND=8), DIMENSION(:), INTENT(IN) :: x, AA
     REAL(KIND=8), DIMENSION(SIZE(x)), INTENT(OUT) :: M_ex, P_ex, T_ex, rho_ex

     REAL(KIND=8), DIMENSION(SIZE(x)) :: M, P_r, T_r, rho_r
     REAL(KIND=8) :: beta, beta_1, beta_2, beta_3,    &
                     P_02, rho_02,                    &
                     M_1, M_3, M_u,                   &
     		             A_ratio

     INTEGER :: N_sw


  ! ACQUISIZIONE DATI

     !   - P_0     pressione di ristagno   (nel serbatoio)
     !   - P_amb   pressione ambiente

     !   - T_0     temperatura nel serbatoio
     !   - rho_0   densità nel seratoio
     !   - R       costante del gas

     !   - A(x)    funzione dell'area dell'ugello   (definita nel modulo nozzle_geometry)
     !   - xi      ingresso ugello                  (definita nel modulo nozzle_geometry)
     !   - xu      uscita ugello                    (definita nel modulo nozzle_geometry)
     !   - gamma   rapporto cp/cv del gas           (definito nel modulo compressible_flow_lib)


     rho_0 = P_0 / (R*T_0)

     ! rapporto di compressione (ambiente/serbatoio):
     beta = P_amb / P_0

     ! discretizzazione della geometria dell'ugello:
!     CALL  gen_nozzle_geometry(x, AA)

     ! rapporto delle aree (uscita/gola):
     A_ratio = Au / Ag


  ! CALCOLO DEI VALORI NOTEVOLI: beta_1 > beta_2 > beta_3

     ! Caso subsonico:
     M_1 = bisection(Area_ratio, A_ratio, 0.001d00, 1.0d00, 1.0d-8)
     beta_1 = P_ratio_isentropic(M_1)

     ! Caso supersonico:
     M_3 = bisection(Area_ratio, A_ratio, 1.0d00, 10.0d00, 1.0d-8)
     beta_3 = P_ratio_isentropic(M_3)

     ! Caso con urto nella sezione di uscita:
     beta_2 = beta_3 * P_ratio_shock(M_3)

     WRITE(*,*) '-------------------------------------------------------------------'
     WRITE(*,*) 'RAPPORTI DI COMPRESSIONE:'
     WRITE(*,*) 'beta   =', beta
     WRITE(*,*) 'beta_1 =', beta_1
     WRITE(*,*) 'beta_2 =', beta_2
     WRITE(*,*) 'beta_3 =', beta_3
     WRITE(*,*) ''

     ! Test di correttezza dell'ordine dei valori
     IF (beta_1 < beta_2  .OR.  beta_1 < beta_3  .OR.  beta_2 < beta_3) THEN
        WRITE(*,*) 'STOP: errore nel calcolo dei valori notevoli di beta!'
        WRITE(*,*) ''
        STOP
     ELSE
        ! OK: i valori notevoli calcolati sono ordinati correttamente
     ENDIF


  ! INDIVIDUAZIONE DEL TIPO DI CORRENTE E CALCOLO
  ! DELLE VARIABILI LUNGO L'ASSE DELL'UGELLO
  ! Paragono il rapporto di compressione con i valori notevoli.

  WRITE(*,*) '-------------------------------------------------------------------'
  WRITE(*,*) 'INDIVIDUAZIONE DEL TIPO DI CORRENTE:'

     IF (beta > 1  .OR.   beta == 1) THEN

        WRITE(*,*) 'P_amb > P_0   =>   NO CORRENTE!'
        WRITE(*,*) 'STOP!'
        WRITE(*,*) ''
        STOP

     ! Corrente subsonica:
     ELSEIF (beta > beta_1) THEN

        WRITE(*,*) 'beta > beta_1   =>   corrente isentropica subsonica'
        WRITE(*,*) ''

        ! Impongo che P_u = P_amb e calcolo il Mach di uscita:
        M_u = inverse_P_ratio_isentropic(P_amb/P_0)

        ! Calcolo l'andamento del numero di Mach lungo l'asse dell'ugello
        M = Mach_subsonic_flow(AA, Au, M_u)

        ! Calcolo l'andamento del rapporto di pressione, temperatura e densità
        ! lungo l'asse dell'ugello rispetto ai valori nel serbatoio
        P_r = P_ratio_isentropic(M)
        T_r = T_ratio_isentropic(M)
        rho_r = rho_ratio_isentropic(M)
        P_02 = P_0; rho_02 = rho_0


     ! Urto nel divergente:
     ELSEIF (beta < beta_1 .AND. beta > beta_2) THEN

        WRITE(*,*) 'beta_2 < beta < beta_1       =>   onda d''urto nel divergente'

        ! Calcolo dell'andamento del Mach lungo l'asse dell'ugello
        CALL Mach_shock_flow(x, AA,  M, N_sw, P_02, rho_02)

        ! Calcolo l'andamento del rapporto di pressione, temperatura e densità
        ! lungo l'asse dell'ugello rispetto ai valori nel serbatoio
        P_r = P_ratio_isentropic(M)
        T_r = T_ratio_isentropic(M)
        rho_r = rho_ratio_isentropic(M)


     ! Corrente isentropica sub-supersonica:
     ELSEIF (beta < beta_2) THEN

        WRITE(*,*) 'beta < beta_2   =>   corrente isentropica sub-supersonica'
        WRITE(*,*) ''

        ! Calcolo dell'andamento del Mach lungo l'asse dell'ugello
        CALL Mach_subsupersonic_flow(x, AA,  M)

        ! Calcolo l'andamento del rapporto di pressione, temperatura e densità
        ! lungo l'asse dell'ugello rispetto ai valori nel serbatoio
        P_r = P_ratio_isentropic(M)
        T_r = T_ratio_isentropic(M)
        rho_r = rho_ratio_isentropic(M)
        P_02 = P_0; rho_02 = rho_0

     ENDIF

     M_ex = M

     P_ex(1:N_sw) = P_r(1:N_sw) * P_0
     P_ex(N_sw+1 : N_nodes) = P_r(N_sw+1 : N_nodes) * P_02

     T_ex(1:N_sw) = T_r * T_0

     rho_ex(1:N_sw) =  rho_r(1:N_sw) * rho_0
     rho_ex(N_sw+1 : N_nodes) = rho_r(N_sw+1 : N_nodes) * rho_02


   END SUBROUTINE exact_nozzle


   ! Generazione della distribuzione del Mach lungo l'asse
   ! dell'ugello per corrente solo subsonica.
   FUNCTION Mach_subsonic_flow(AA, A_ref, M_ref) RESULT(M)

      IMPLICIT NONE

      REAL(KIND=8), DIMENSION(:), INTENT(IN) :: AA
      REAL(KIND=8),               INTENT(IN) :: A_ref, M_ref
      REAL(KIND=8), DIMENSION(SIZE(AA)) :: M

      REAL(KIND=8) :: A_star
      INTEGER :: i

      A_star = A_ref/Area_ratio(M_ref)

      DO i = 1, SIZE(AA)

         M(i) = bisection(Area_ratio, AA(i)/A_star, &
                          0.001d00, 1.0d00, 1.0d-8)

      ENDDO

   END FUNCTION Mach_subsonic_flow


   ! Generazione della distribuzione del Mach lungo l'asse
    SUBROUTINE Mach_subsupersonic_flow(xx, AA,  M)

      IMPLICIT NONE

      REAL(KIND=8), DIMENSION(:), INTENT(IN) :: xx, AA
      REAL(KIND=8), DIMENSION(SIZE(AA)), INTENT(OUT) :: M

      INTEGER, DIMENSION (1) :: i_1
      INTEGER :: i
      INTEGER :: i_g

      ! Calcolo l'indice di gola
      i_1 = MINLOC(ABS(xx - xg));   i_g = i_1(1)

      ! Range subsonico
      DO i = 1, i_g - 1

         M(i) = bisection(Area_ratio, AA(i)/Ag, &
                          0.001d00, 1.0d00, 1.0d-8)

      ENDDO

      M(i_g) = 1   ! Gola

      ! Range supersonico
      DO i = i_g + 1, size(AA)

         M(i) = bisection(Area_ratio, AA(i)/Ag, &
                          1.0d00, 10.0d00, 1.0d-8)

      ENDDO

   END SUBROUTINE Mach_subsupersonic_flow


   ! Generazione della distribuzione del Mach lungo l'asse
   ! dell'ugello per corrente con urto nel divergente
   SUBROUTINE Mach_shock_flow(x, AA, M, N_sw, P_02, rho_02)

      IMPLICIT NONE

      REAL(KIND=8), DIMENSION(:), INTENT(IN) :: x, AA
      REAL(KIND=8), DIMENSION(SIZE(AA)),INTENT(OUT) :: M
      REAL(KIND=8), INTENT(OUT) :: P_02, rho_02
      INTEGER, INTENT(OUT) :: N_sw

      REAL(KIND=8) :: g_m1, g_p1, B, M_u, A_sw, M_1sw, x_sw

      g_m1 = gamma-1
      g_p1 = gamma+1
      B = (P_0/P_amb) * (Ag/Au) * (2/g_p1)**(g_p1/(2*g_m1))

      M_u = SQRT( (-1 + SQRT(1 + 2*g_m1*B*B)) / g_m1 )

      P_02 = P_amb / P_ratio_isentropic(M_u)
      rho_02 = P_02 / (R*T_0)

      M_1sw = bisection(P_TOT_ratio_shock, P_02/P_0, &
                        1.0d00, 10.0d00, 1.0d-8)

      ! Trovo l'area corrispondente a M_1sw e la corrispondente
      ! coordinata x (posizione dell'urto):
      A_sw = Ag * Area_ratio(M_1sw)
      x_sw = bisection(nozzle_section_area, A_sw, &
                       xg, xu, 1.0d-8)
      N_sw = N_nodes * (x_sw - xi)/(xu - xi)

!      WRITE(*,*) 'posizione dell''onda d''urto   =>   ', 'x_sw = ', x_sw
      		        		     				                  ! opzionale
  !    CALL gen_nozzle_geometry(x(1:N_sw),   AA(1:N_sw),    b = x_sw)
  !    CALL gen_nozzle_geometry(x(N_sw+1:N), AA(N_sw+1:N),  a = x_sw)

      ! Alternativa di Davide Marchesoli-Quartapelle: divido in tre tratti uniformi
      ! CALL gen_nozzle_geometry(x(1:i_g),    AA(1:i_g),             b = xg)
      ! CALL gen_nozzle_geometry(x(i_g:N_sw), AA(i_g:N_sw),  a = xg, b = x_sw)
      ! CALL gen_nozzle_geometry(x(N_sw+1:N), AA(N_sw+1:N),  a = x_sw)
      ! Da provare

      ! Calcolo la corrente isentropica prima e dopo l'urto:
      CALL Mach_subsupersonic_flow(x, AA(1:N_sw),  M(1:N_sw))
      M(N_sw+1 : N_nodes) = Mach_subsonic_flow(AA(N_sw+1 : N_nodes), Au, M_u)

   END SUBROUTINE Mach_shock_flow


END MODULE ugello
