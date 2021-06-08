MODULE thermodynamics

   ! Equazioni di stato della termodinamica di
   ! un gas ideale politropico

   IMPLICIT NONE
   
   REAL(KIND=8), PARAMETER :: gamma = 1.4d00 
                            
   INTERFACE dpressure_de_drho

      MODULE PROCEDURE dpressure_de_drho_s, dpressure_de_drho_v

   END INTERFACE dpressure_de_drho

   PRIVATE dpressure_de_drho_s, dpressure_de_drho_v
   
   
   INTERFACE pressure_RAR

      MODULE PROCEDURE pressure_RAR_S, pressure_RAR_V

   END INTERFACE pressure_RAR

   PRIVATE pressure_RAR_S, pressure_RAR_V
   
CONTAINS

ELEMENTAL FUNCTION pressure(e, rho) RESULT(P)

    IMPLICIT NONE
   
    REAL(KIND=8), INTENT(IN)  :: e, rho
    REAL(KIND=8) :: P
                
    P = (gamma - 1) * rho * e       
    
END FUNCTION pressure

! Forse bisogna fare una versione elementale
SUBROUTINE dpressure_de_drho_s (e, rho,  dP_de, dP_drho)
 
   IMPLICIT NONE
  
   REAL(KIND=8), INTENT(IN)  :: e, rho
   REAL(KIND=8), INTENT(OUT) :: dP_de,  dp_drho
    
   dP_de   = (gamma - 1) * rho 
   dP_drho = (gamma - 1) * e     
 
END SUBROUTINE dpressure_de_drho_s


SUBROUTINE dpressure_de_drho_v (e, rho,  dP_de, dP_drho)
 
   IMPLICIT NONE
  
   REAL(KIND=8), DIMENSION(:), INTENT(IN)  :: e, rho
   REAL(KIND=8), DIMENSION(:), INTENT(OUT) :: dP_de,  dp_drho
    
   dP_de   = (gamma - 1) * rho 
   dP_drho = (gamma - 1) * e     
 
END SUBROUTINE dpressure_de_drho_v
   
   
ELEMENTAL FUNCTION specific_energy(P, rho) RESULT(e)
 
   IMPLICIT NONE
  
   REAL(KIND=8), INTENT(IN) :: P, rho
   REAL(KIND=8) :: e
    
   e = P / ((gamma - 1) * rho) 
    
END FUNCTION specific_energy


ELEMENTAL FUNCTION sound_speed(e, rho) RESULT(c)

   IMPLICIT NONE
  
   REAL(KIND=8), INTENT(IN) :: e, rho
   REAL(KIND=8) :: c
    
   c = SQRT(gamma * (gamma - 1) * e) 
    
END FUNCTION sound_speed
   
   
FUNCTION pressure_RAR_S(P_, v_, v,  dP_dv) RESULT(P)

    IMPLICIT NONE
   
    REAL(KIND=8), INTENT(IN) :: P_, v_, v
    REAL(KIND=8), INTENT(OUT), OPTIONAL :: dP_dv
    REAL(KIND=8) :: P
        
    P = P_ * (v_ / v)**gamma 
    
    IF (PRESENT(dP_dv)) dP_dv = - gamma * P/v
     
END FUNCTION pressure_RAR_S


FUNCTION pressure_RAR_V(P_, v_, v,  dP_dv) RESULT(P)

    IMPLICIT NONE
   
    REAL(KIND=8),               INTENT(IN) :: P_, v_
    REAL(KIND=8), DIMENSION(:), INTENT(IN) ::  v
    REAL(KIND=8), DIMENSION(:), INTENT(OUT), OPTIONAL :: dP_dv
    REAL(KIND=8), DIMENSION(SIZE(v)) :: P
        
    P = P_ * (v_/v)**gamma 
    
    IF (PRESENT(dP_dv)) dP_dv = - gamma * P/v
     
END FUNCTION pressure_RAR_V


FUNCTION integral_c_v_isentropic(P_, v_, v) RESULT(int)

    ! calcolo di int_{v_}^v c(s_, v)/v dv
    
    IMPLICIT NONE
   
    REAL(KIND=8), INTENT(IN) :: P_, v_, v
    REAL(KIND=8) :: int
    
    int =  (2*SQRT(gamma*P_*v_)/(gamma-1)) * (1 - (v_/v)**((gamma-1)/2))
     
END FUNCTION integral_c_v_isentropic


FUNCTION sound_speed_isentropic(P_, v_, v) RESULT(c_isent)
    
    IMPLICIT NONE
   
    REAL(KIND=8), INTENT(IN) :: P_, v_, v
    REAL(KIND=8) :: c_isent
    
    c_isent = SQRT(gamma*P_*v_) * (v_/v)**((gamma-1)/2)
     
END FUNCTION sound_speed_isentropic


FUNCTION pressure_RH(P_, v_, v,  dP_dv) RESULT(P)   

    IMPLICIT NONE
   
    REAL(KIND=8), INTENT(IN) :: P_, v_, v
    REAL(KIND=8), INTENT(OUT), OPTIONAL :: dP_dv
    REAL(KIND=8) :: P
    
    REAL(KIND=8) :: NUM, DEN
    
    ! Controllo che v non sia inferiore all'asintoto (Vedi testo)
    
    NUM = (gamma + 1) * v_  -  (gamma - 1) * v
    DEN = (gamma + 1) * v   -  (gamma - 1) * v_ 
    
    P = P_ * NUM/DEN   
    
    IF (PRESENT(dP_dv)) dP_dv = -4 * gamma * P_ * v_/DEN**2 

END FUNCTION pressure_RH


END MODULE thermodynamics
