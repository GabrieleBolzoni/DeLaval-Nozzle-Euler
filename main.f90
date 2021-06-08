PROGRAM main

   USE ugello
   USE compressible_flow_lib
   USE nozzle_geometry
   USE nonlinear_equations

   USE Euler_flux_jacobian
   USE thermodynamics
   USE numerical_fluxes
   USE exact_riemann_problem
   USE boundary
   USE gnufor

   IMPLICIT NONE

   LOGICAL, PARAMETER :: MOVIES = .true.

   ! IL NUMERO DI PUNTI CON CUI DISCRETIZZO L'UGELLO È NELLA nozzle_geometry
   REAL(KIND=8), DIMENSION(Np) :: xx, AA

   REAL(KIND=8), DIMENSION(N_nodes) :: rho_ex, P_ex, M_ex, T_ex, u_ex, &
                                       xgrid, dA_dx

   REAL(KIND=8), DIMENSION(order,N_nodes) :: W, W_ex, Res,  &
                                             cell_size, lambda, A_nodes

   REAL(KIND=8), DIMENSION(order,N_int) :: F

   REAL(KIND=8), DIMENSION(order) :: F_bar, W_in, W_out, &
                                     p_ext_l, p_ext_r

   REAL(KIND=8) :: lambda_max

   REAL(KIND=8) :: Dx, t, Dt, t_end, &

                   CFL = 0.85d0

   INTEGER :: i, j, jl, jr, n

   INTEGER :: limiter = 2 ! minmod


   !============
   ! Video stuff

   REAL :: gap_time = 0.02  ! tempo di ritardo, in secondi
   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: vid_q
   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: vid_xx

   INTEGER :: n_max

   n_max = 10000
   !plotto una variabile alla volta per via delle scale delle grandezze
   ALLOCATE (vid_q(N_nodes  * 2,  n_max),  vid_xx(N_nodes,1))
   vid_q = 0
   ! end of Video stuff
   !===================

   ! E' un tempo fittizio, perché è un problema stazionario: sono iterazioni
   t_end = 0.3d00


   ! Generazione della geometria
   CALL  gen_nozzle_geometry(xx, AA)

   ! cell_size e Area_nodes sono tabelle per evitare di usare SPREAD
   DO j = 2, N_nodes-1
     xgrid(j) = xx( 2*(j-1) ) !per prendere le coordinate delle celle
     A_nodes(:,j) = AA( 2*(j-1) )
     cell_size(:,j) = xx( 2*(j-1) +1 ) - xx( 2*(j-1) -1 )
     dA_dx(j) = ( AA( 2*(j-1) +1 ) - AA( 2*(j-1) -1 )  )/ cell_size(1,j)
   ENDDO

   ! per avere le mezze celle all'inizio e alla fine
   Dx = 2 * (xu - xi)/(Np - 1)
   dA_dx(1) = 0;  dA_dx(N_nodes) = 0;

   xgrid(1) = 0 ;                     xgrid(N_nodes) = xx(Np) + Dx/2
   A_nodes(:,1) = AA(1) ;             A_nodes(:,N_nodes) = AA(Np)
   cell_size(:, N_nodes) = Dx/2;      cell_size(:,1) = Dx/2


   ! Risoluzione problema esatto, con condizioni di serbatoio e pressione esterna
   ! dati e definiti nel modulo ugello.o
   CALL exact_nozzle (xgrid, A_nodes(1,:),  M_ex, P_ex, T_ex, rho_ex)
   u_ex = M_ex * SQRT(gamma*R*T_ex)


   ! condizioni iniziali, partendo dalla soluzione esatta
   W_in = conservative( [rho_ex(1), u_ex(1), P_ex(1)] )
   W_out = conservative( [rho_ex(N_nodes), u_ex(N_nodes), P_ex(N_nodes)] )

   ! Scalino posto a 100 celle dall'uscita
   WHERE (xgrid <  xgrid(N_nodes - 100))
     W(1,:)= W_in(1)
     W(2,:)= W_in(2)
     W(3,:)= W_in(3)
   ELSEWHERE
     W(1,:) = W_out(1)
     W(2,:) = W_out(2)
     W(3,:) = W_out(3)
   END WHERE


   ! inizializzazione delle variabili per problema quasi-1D: risolvo un problema
   ! 1D con variabili: " var_1d * area_cella ". Aggiungo solo una ODE per
   ! l'equazione del momento, risolta in uno step subito successivo
   W = W * A_nodes

   ! ciclo sul tempo

   t = 0
   n = 0

   DO

      n = n + 1

      ! calcolo Dt per stabilità numerica

      lambda = eigenvalues(W)
      lambda_max = MAXVAL(ABS(lambda))

      Dt = Dx * CFL / lambda_max
      Dt = MIN(Dt, t_end - t)

      t = t + Dt

      WRITE(*,*) " n = ", n, "t = ", t,  "Dt = ", Dt

      ! Flussi numerici
      !CALL LWc_flux (Dt, Dx, W,  F)
      !CALL Roe_flux (W,  F)
      !CALL UHR_flux (Dt, Dx, W,  F, limiter)
      CALL UHR_flux (0d0, Dx, W,  F, limiter) ! vita DURA. L'idea di utilizzare
      !metodo ad alta risoluzione non derivante da Lax_Wendroff crea riflessione di
      !oscillazioni.

      Res = 0

      p_ext_l = primitive(W_in) ! impongo le condizioni di inflow esatte

      p_ext_r = primitive( W(:,N_nodes)/A_nodes(:,N_nodes) ) !prendo rho e u calcolate allo step prima
      p_ext_r(3) = P_ex(N_nodes)                             ! impongo solo la pressione di uscita


      ! a BOUNDARY devo passare l'area perché per la trasformazione da variabili
      ! primitive a conservative devo moltiplicare anche per l'area
      CALL left_boundary  (t, p_ext_l, A_nodes(:,1), W(:,1),  F_bar)
      Res(:,1) = Res(:,1)  +  F_bar

      CALL right_boundary (t, p_ext_r, A_nodes(:,N_nodes), W(:,N_nodes),  F_bar)
      Res(:, N_nodes) = Res(:, N_nodes)  -  F_bar


      ! STEP 1: soluzione del problema 1D con le variabili nuove
      DO i = 1, N_int ! ciclo sulle interfacce

         jl = i;   jr = i + 1

         Res(:,jl)  =  Res(:,jl)  -  F(:,i)

         Res(:,jr)  =  Res(:,jr)  +  F(:,i)

      END DO

      ! aggiornamento soluzione

      W = W  +  Dt * Res / cell_size


      ! STEP 2: ciclo sulle celle per soluzione ODE nella variabile m
      DO j = 1, N_nodes

        W(2,j) = W(2,j) + Dt * Pgreco( W(:,j)/A_nodes(:,j) ) * dA_dx(j)

      ENDDO

      ! check sulla pressione e densità
      IF (MINVAL( W(1,:)/A_nodes(1,:) ) <= 0) THEN
         WRITE(*,*) "Densità negativa o nulla, STOP"
         STOP
      ENDIF

      IF (MINVAL( Pgreco(W/A_nodes) ) <= 0) THEN
         WRITE(*,*) "Pressione negativa o nulla, STOP"
         STOP
      ENDIF

      !============
      ! Video stuff
      IF (MOVIES) THEN

        IF(MODULO(n,5) == 0) THEN

         vid_q(:, n/5) = [ & ! W(1,:)/A_nodes(1,:), rho_ex ] !, &
                         W(2,:)/W(1,:), u_ex ] !&
  !                        Pgreco( W/A_nodes ), P_ex]
         ENDIF

      ENDIF
      ! end of Video stuff
      !===================


      IF (t == t_end) THEN
         WRITE(*,*) "TEMPO FINALE RAGGIUNTO"
         EXIT
      END IF

   END DO ! fine del ciclo temporale

   ! Recupero delle variabili conservative del problema 1D
   W = W / A_nodes


   CALL plot_two_functions (xgrid, W(1,:), rho_ex)
   CALL plot_two_functions (xgrid, W(2,:)/W(1,:), u_ex)
   CALL plot_two_functions (xgrid, Pgreco(W), P_ex)


   !============
   ! Video stuff
   IF (MOVIES) THEN

      vid_xx(:, 1) = xgrid

      CALL plot_video (vid_xx, vid_q, '__', gap_time)

   ENDIF
   ! end of Video stuff
   !===================


END PROGRAM main
