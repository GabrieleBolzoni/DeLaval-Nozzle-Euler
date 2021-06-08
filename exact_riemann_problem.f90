MODULE exact_riemann_problem

   USE Euler_flux_jacobian

   USE thermodynamics


   INTERFACE rarefaction_density

      MODULE PROCEDURE rarefaction_density_s, rarefaction_density_v

   END INTERFACE rarefaction_density

   PRIVATE rarefaction_density_s, rarefaction_density_v


   INTERFACE rarefaction_velocity

      MODULE PROCEDURE rarefaction_velocity_s, rarefaction_velocity_v

   END INTERFACE rarefaction_velocity

   PRIVATE rarefaction_velocity_s, rarefaction_velocity_v


   INTERFACE rarefaction_pressure

      MODULE PROCEDURE rarefaction_pressure_s, rarefaction_pressure_v

   END INTERFACE rarefaction_pressure

   PRIVATE rarefaction_pressure_s, rarefaction_pressure_v



CONTAINS


SUBROUTINE exact_riemann (wl, wr,  wCl, wCr)

   ! risolve il sistema di equazioni non lineari:

   !               P(vCl, wl)  =    P(vCr, wr)
   !             u_1(vCl, wl)  =  u_3(vCr, wr)
   !

   ! in realtà il sistema che risolviamo è:

   !             P(vCl, wl)  -    P(vCr, wr)  =  0
   !           u_1(vCl, wl)  -  u_3(vCr, wr)  =  0

   IMPLICIT NONE   

   REAL(KIND=8), DIMENSION(:), INTENT(IN)  :: wl, wr ! stati destro e sinistro
   REAL(KIND=8), DIMENSION(:), INTENT(OUT) :: wCl, wCr ! 2 stati intermedi

   REAL(KIND=8) :: nu,  nu_vacuum,  &
                   rhol, vl, ml, ul, el, cl, & 
                   rhor, vr, mr, ur, er, cr, &
                   u1, v1, P1, Dv1, & 
                   u2, v2, P2, Dv2, &
                   a11, a21, a12, a22, rhs1, rhs2, det

   REAL(KIND=8), PARAMETER :: rel_tol = 1.0d-10
   INTEGER, PARAMETER :: nmax = 1000

   INTEGER :: n

   rhol = wl(1);   ml = wl(2);   vl = 1/rhol;   ul = ml * vl
   rhor = wr(1);   mr = wr(2);   vr = 1/rhor;   ur = mr * vr

   ! parametri limite
   ! due fan
   !    nu_2r =
   ! due shock
   !    nu_2s =

   el = wl(3) / rhol  -  ul**2 / 2;   cl = sound_speed(el, rhol)
   er = wr(3) / rhor  -  ur**2 / 2;   cr = sound_speed(er, rhor)

   nu_vacuum = 2 * (cl + cr) / (gamma -1)

   nu = ur - ul ! il confronto fra le velocità limite potrebbe
                ! essere usato per scegliere il guess iniziale
                ! lo faremo dopo.
                ! per ora usiamo la media

   IF (nu >= nu_vacuum) THEN

      WRITE(*,*) 'formazione del vuoto'
      WRITE(*,*) 'STOP'
      STOP

   ENDIF

   ! guess iniziale media per entrambe le incognite del sistema 2x2
   v1 = (vl + vr) / 2
   v2 = (vl + vr) / 2

   DO n = 1, nmax

      ! rete di protezione per evitare di andare al di sotto
      ! del limite dell'adiabatica di RH del gas ideal politropico
      !v_asintoto = ((gamma-1)/(gamma+1)) * v_
      !maledetto gamma
      !questi controlli sono limitati al modello del PIG

      IF (v1 < ((gamma-1)/(gamma+1)) * vl) THEN

         v1 = (gamma/(gamma+1)) * vl

         WRITE(*,*) 'metodo di Newton nella subroutine'
         WRITE(*,*) 'exact_riemann genera un volume specifico v1'
         WRITE(*,*) 'inferiore al limite asintotico della adiabatica di RH'
         WRITE(*,*) 'correzione ad hoc forzata in modo ordinario'
         WRITE(*,*) 'v1 = (gamma/(gamma+1)) * vl'

      ENDIF

      CALL loci_rar_RH (1, wl, v1,  P1, u1, a11, a21)

      IF (v2 < ((gamma-1)/(gamma+1)) * vr) THEN

         v2 = (gamma/(gamma+1)) * vr

         WRITE(*,*) 'metodo di Newton nella subroutine'
         WRITE(*,*) 'exact_riemann genera un volume specifico v2'
         WRITE(*,*) 'inferiore al limite asintotico della adiabatica di RH'
         WRITE(*,*) 'correzione ad hoc forzata in modo ordinario'
         WRITE(*,*) 'v2 = (gamma/(gamma+1)) * vr'

      ENDIF

      CALL loci_rar_RH (3, wr, v2,  P2, u2, a12, a22)

      ! devo cambiare il segno di a11 e a22
      a12 = -a12;   a22 = -a22

      ! metodo di Newton in forma incrementale per sistema
      rhs1 = - (P1 - P2)
      rhs2 = - (u1 - u2)
      ! dobbiamo risolvere un sistema lineare 2x2

      det = a11 * a22  -  a12 * a21

      Dv1 = (a22*rhs1 - a12*rhs2) / det ! regola di Cramer
      Dv2 = (a11*rhs2 - a21*rhs1) / det

      v1 = v1 + Dv1
      v2 = v2 + Dv2

      IF (SQRT(Dv1**2 + Dv2**2) <=  rel_tol * SQRT(v1**2 + v2**2)) THEN

		     wCl(1) = 1/v1;     wCr(1) = 1/v2

		     CALL loci_rar_RH(1, wl, v1,  P1, u1, a11, a21)
		     ! non servono a11 e a21

         wCl(2) = u1/v1;    wCr(2) = u1/v2  ! u2 == u1

         wCl(3) = (specific_energy(P1, 1/v1)  +  u1**2/2) / v1 ! controllare se e(P, v) o e(P, rho)

         wCr(3) = (specific_energy(P1, 1/v2)  +  u1**2/2) / v2 ! P1 == P2
         
         WRITE(*,*) 'metodo di Newton converge in', n, 'iterazioni'
         WRITE(*,*) 
         
         RETURN

	    ENDIF
      
   END DO
   
   WRITE(*,*)
   WRITE(*,*) 'metodo di Newton non converge in', nmax, 'iterazioni'
   WRITE(*,*) 'STOP nella SUBROUTINE exact_riemann'
   STOP

END SUBROUTINE exact_riemann


SUBROUTINE loci_rar_RH (i, w_, v,  P, u, dP_dv, du_dv)

   IMPLICIT NONE

   INTEGER,                    INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_  ! stato pivot
   REAL(KIND=8),               INTENT(IN) :: v ! var indipendente
   REAL(KIND=8),               INTENT(OUT) :: P, u, dP_dv, du_dv

   REAL(KIND=8) :: s, v_, u_, P_, DP, Dv, S_DD

   SELECT CASE (i)

      CASE(1);  s = -1

      CASE(3);  s = 1

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in loci_rar_RH'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   ! definire energia di stato post discontinuità in funzione
   ! di volume specifico e pressione

   v_ = 1/w_(1)
   u_ = w_(2)/w_(1)

   P_ = Pgreco(w_)
 
   IF (v >= v_) THEN ! rarefazione

      P = pressure_RAR(P_, v_, v, dP_dv)

      u = u_  -  s * integral_c_v_isentropic(P_, v_, v)

      du_dv = -s * sound_speed_isentropic(P_, v_, v) / v

   ELSE ! urto

      P = pressure_RH(P_, v_, v, dP_dv)

      DP = P - P_;   Dv = v - v_

      S_DD = SQRT(-DP * Dv)
        
      u = u_  +  s * S_DD

      IF (S_DD /= 0) THEN  
         du_dv = -s * (Dv * dP_dv  +  DP)/(2 * S_DD)
      ELSE 
         du_dv = 0 
      ENDIF 

   END IF

END SUBROUTINE loci_rar_RH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION rarefaction_density_s(i, w_, csi) RESULT(rho)

   IMPLICIT NONE

   INTEGER,                    INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_
   REAL(KIND=8),               INTENT(IN) :: csi
   REAL(KIND=8) ::rho

   REAL(KIND=8), DIMENSION(SIZE(w_)) :: prim_, lambda_
   REAL(KIND=8) :: s, rho_, P_, c_, csi_

   SELECT CASE (i)

      CASE(1);  s = -1

      CASE(3);  s = 1

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in loci_rar_RH'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   prim_= primitive(w_)

   rho_ = prim_(1)
   P_   = prim_(3)

   c_ = sound_speed(specific_energy(P_, rho_), rho_)

   lambda_ = eigenvalues(w_)

   csi_ = lambda_(i)

   rho = rho_ * (1 + s *((gamma - 1)/(gamma + 1)) * (csi - csi_)/c_)**(2/(gamma-1))

END FUNCTION rarefaction_density_s



FUNCTION rarefaction_density_v(i, w_, csi) RESULT(rho)

   IMPLICIT NONE

   INTEGER,                    INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: csi
   REAL(KIND=8), DIMENSION(SIZE(csi)) :: rho

   REAL(KIND=8), DIMENSION(SIZE(w_)) :: prim_, lambda_
   REAL(KIND=8) :: s, rho_, P_, c_, csi_

   SELECT CASE (i)

      CASE(1);  s = -1

      CASE(3);  s = 1

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in loci_rar_RH'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   prim_= primitive(w_)

   rho_ = prim_(1)
   P_   = prim_(3)

   c_ = sound_speed(specific_energy(P_, rho_), rho_)

   lambda_ = eigenvalues(w_)

   csi_ = lambda_(i)

   rho = rho_ * (1 + s *((gamma - 1)/(gamma + 1)) * (csi - csi_)/c_)**(2/(gamma-1))

END FUNCTION rarefaction_density_v




FUNCTION rarefaction_velocity_s(i, w_, csi) RESULT(u)

   IMPLICIT NONE

   INTEGER,      INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_
   REAL(KIND=8),               INTENT(IN) :: csi
   REAL(KIND=8) :: u

   REAL(KIND=8), DIMENSION(SIZE(w_)) :: prim_, lambda_
   REAL(KIND=8) :: u_, csi_

   SELECT CASE (i)

      CASE(1)

      CASE(3)

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in loci_rar_RH'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   prim_= primitive(w_)

   u_ = prim_(2)

   lambda_ = eigenvalues(w_)

   csi_ = lambda_(i)

   u = u_  +  (2/(gamma + 1)) * (csi - csi_)

END FUNCTION rarefaction_velocity_s


FUNCTION rarefaction_velocity_v(i, w_, csi) RESULT(u)

   IMPLICIT NONE

   INTEGER,      INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: csi
   REAL(KIND=8), DIMENSION(SIZE(csi)) :: u

   REAL(KIND=8), DIMENSION(SIZE(w_)) :: prim_, lambda_
   REAL(KIND=8) :: u_, csi_

   SELECT CASE (i)

      CASE(1)

      CASE(3)

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in loci_rar_RH'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   prim_= primitive(w_)

   u_ = prim_(2)

   lambda_ = eigenvalues(w_)

   csi_ = lambda_(i)

   u = u_  +  (2/(gamma + 1)) * (csi - csi_)

END FUNCTION rarefaction_velocity_v


FUNCTION rarefaction_pressure_s(i, w_, csi) RESULT(P)

   IMPLICIT NONE

   INTEGER,      INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_
   REAL(KIND=8),               INTENT(IN) :: csi
   REAL(KIND=8) :: P

   REAL(KIND=8), DIMENSION(SIZE(w_)) :: prim_
   REAL(KIND=8) :: rho_, P_, rho

   SELECT CASE (i)

      CASE(1)

      CASE(3)

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in rarefaction_pressure_s'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   rho_ = w_(1)

   prim_= primitive(w_)

   P_ = prim_(3)

   rho = rarefaction_density(i, w_, csi)

   P = P_ * (rho/rho_)**gamma

END FUNCTION rarefaction_pressure_s


FUNCTION rarefaction_pressure_v(i, w_, csi) RESULT(P)

   IMPLICIT NONE

   INTEGER,      INTENT(IN) :: i
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: w_
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: csi
   REAL(KIND=8), DIMENSION(SIZE(csi)) :: P

   REAL(KIND=8), DIMENSION(SIZE(csi)) :: rho
   REAL(KIND=8), DIMENSION(SIZE(w_)) :: prim_
   REAL(KIND=8) :: rho_, P_

   SELECT CASE (i)

      CASE(1)

      CASE(3)

      CASE DEFAULT

         WRITE(*,*) 'i deve essere 1 o 3 in rarefaction_pressure_v'
         WRITE(*,*) 'STOP'
         STOP

   END SELECT

   rho_ = w_(1)

   prim_= primitive(w_)

   P_ = prim_(3)

   rho = rarefaction_density(i, w_, csi)

   P = P_ * (rho/rho_)**gamma

END FUNCTION rarefaction_pressure_v


!================================================================

FUNCTION ws_sonic_state(i, w) RESULT(ws)
 
   ! Sonic values of the rarefaction wave 
   ! similarity solution at xi = 0
             
   IMPLICIT NONE

   INTEGER,                    INTENT(IN)  :: i 
   REAL(KIND=8), DIMENSION(:), INTENT(IN)  :: w
   REAL(KIND=8), DIMENSION(SIZE(w)) :: ws
   
   REAL(KIND=8) :: s,  rho, u, e, P, c, lambda,  & 
                   rhos, us, Ets, Ps
   
   
   SELECT CASE(i)
  
      CASE(1);  s = -1   
      CASE(3);  s = 1 
       
      CASE DEFAULT

         WRITE (*,*) 'In FUNCTION ws_sonic_state, i'
         WRITE (*,*) 'must be either 1 or 3.  STOP.' 
         STOP 
             
   END SELECT
   
   
   rho = w(1);   u = w(2)/w(1)
   
   e = w(3)/w(1) - u**2/2   
   
   P = Pgreco(w)
   
   c = sound_speed(e, rho)
   
   lambda = u  +  s * c
   
   rhos = rho * (1  -  s * ((gamma-1)/(gamma+1)) * lambda/c)**(2/(gamma-1)) 
   
   us = u  -  (2/(gamma+1)) * lambda 
  
   Ps = P * (rhos/rho)**gamma 
  
   Ets = rhos * (specific_energy(Ps, rhos)  +  us**2/2) 
   
   ws = [rhos, rhos*us, Ets]
           
END FUNCTION ws_sonic_state 

!================================================================

END MODULE exact_riemann_problem
