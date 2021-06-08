MODULE compressible_flow_lib

! Formule delle correnti comprimibili per un PIG
   USE thermodynamics

   IMPLICIT NONE

CONTAINS


   FUNCTION Area_ratio(M) RESULT(A_r)

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M
      REAL(KIND=8) :: A_r

      REAL(KIND=8) :: g_m1, g_p1

      g_m1 = gamma - 1
      g_p1 = gamma + 1

      A_r = (1/M) * ((2/g_p1) * (1 + (g_m1/2) * M**2))**(g_p1/(2*g_m1))

   END FUNCTION Area_ratio


   ELEMENTAL FUNCTION P_ratio_isentropic(M) RESULT(P_r)

      ! Rapporto delle pressioni in una corrente isentropica
      ! in funzione del numero di Mach: P/P_ristagno

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M
      REAL(KIND=8) :: P_r

      P_r = (1  +  M**2 * (gamma-1)/2)**(-gamma/(gamma-1))

   END FUNCTION P_ratio_isentropic


   FUNCTION inverse_P_ratio_isentropic(P_r) RESULT(M)

      ! Funzione inversa del rapporto delle pressioni in una corrente
      ! isentropica: funzione di M = Mach_isentropic_flow(P_r)

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: P_r
      REAL(KIND=8) :: M

      M = SQRT((2/(gamma-1)) * (P_r**((1-gamma)/gamma)  -  1))

   END FUNCTION inverse_P_ratio_isentropic


   FUNCTION P_ratio_shock(M) RESULT(P_r_sw)

      ! Rapporto delle pressioni a cavallo di un urto normale
      ! in funzione del numero di Mach: P2/P1 = P_post/P_pre

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M
      REAL(KIND=8) :: P_r_sw

      P_r_sw = (2*gamma*M**2 - gamma + 1) / (gamma + 1)

   END FUNCTION P_ratio_shock


   FUNCTION P_TOT_ratio_shock(M) RESULT(P_TOT_r_sw)

      ! Rapporto delle pressioni totali a cavallo di un urto normale
      ! in funzione del numero di Mach: P02/P01 = P_TOT_post/P_TOT_pre

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M
      REAL(KIND=8) :: P_TOT_r_sw

      REAL(KIND=8) :: g_m1, g_p1, num, den

      g_m1 = gamma - 1
      g_p1 = gamma + 1

      num = (g_p1 * M**2 / (g_m1 * M**2 + 2))**gamma
      den = 1 + (2*gamma/g_p1) * (M**2 - 1)

      P_TOT_r_sw = (num/den)**(1/g_m1)

   END FUNCTION P_TOT_ratio_shock


   ELEMENTAL FUNCTION T_ratio_isentropic(M) RESULT(T_r)

      ! Rapporto fra la temperatura lungo l'asse dell'ugello
      ! e la temperatura nel serbatoio per flusso isentropico

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M
      REAL(KIND=8) :: T_r

      T_r = (1 + M**2*(gamma-1)/2)**(-1)

   END FUNCTION T_ratio_isentropic


   ELEMENTAL FUNCTION rho_ratio_isentropic(M) RESULT(rho_r)

      ! Rapporto fra la densità lungo l'asso dell'ugello
      ! e la densità nel serbatoio per flusso isentropico (rho/rho_0)

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M
      REAL(KIND=8) :: rho_r

      rho_r = (1 + M**2*(gamma-1)/2)**(-1/(gamma-1))

   END FUNCTION rho_ratio_isentropic


   FUNCTION rho_ratio_shock(M_1sw) RESULT(rho_r_sw)

      ! Rapporto delle densità a valle e a monte
      ! di un'onda d'urto (rho_post/rho_pre)

      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: M_1sw
      REAL(KIND=8) :: rho_r_sw

      REAL(KIND=8) :: g_m1, g_p1

      g_m1 = gamma - 1
      g_p1 = gamma + 1

      rho_r_sw = (g_p1 * M_1sw**2) / (g_m1 * M_1sw**2  +  2)

   END FUNCTION rho_ratio_shock


END MODULE compressible_flow_lib
