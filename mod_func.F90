MODULE MOD_FUNC
    
USE NRTYPE   , ONLY: WP, SP

IMPLICIT NONE

    CONTAINS
!==================== 2D INHOMOGENEOUS STRUCTURE =======================
!------------------------------------------------------- S-wave velocity
!------------------------------------------------------- m-parameter
  FUNCTION FUNM(RHOF, T, PHI)
  
    USE NRTYPE, ONLY: WP
    
    REAL(WP), INTENT(IN) :: RHOF, T, PHI
    REAL(WP)             :: FUNM
    
    IF (PHI == 0) THEN
        FUNM = 0._WP
    ELSE
        FUNM = RHOF*T/PHI
    END IF
  
  END FUNCTION FUNM
  
  !------------------------------------------------------ M-parameter
  FUNCTION FUNCAP_M(KS, KF, KD, PHI)
  
    USE NRTYPE, ONLY: WP
    
    REAL(WP), INTENT(IN) :: KS, KF, KD, PHI
    REAL(WP)             :: FUNCAP_M
    
    IF (PHI == 0) THEN
        FUNCAP_M = 0._WP
    ELSE
        FUNCAP_M = KS/(1._WP - PHI - (KD/KS) + (PHI*KS/KF))
    END IF
    
  END FUNCTION FUNCAP_M
  
  !------------------------------------------------------ ALPHA-parameter
  FUNCTION FUNALPHA(KS, KD)
  
     USE NRTYPE, ONLY: WP
     
     REAL(WP), INTENT(IN) :: KS, KD
     REAL(WP)             :: FUNALPHA
      
     FUNALPHA = 1._WP - (KD/KS)
      
  END FUNCTION FUNALPHA
  
  !------------------------------------------------------- b-parameter
  FUNCTION FUNB(ETA, KAPPA)
  
     USE NRTYPE, ONLY: WP
     
     REAL(WP), INTENT(IN) :: ETA, KAPPA
     REAL(WP)             :: FUNB
     
     IF (ETA == 0) THEN
         FUNB = 0._WP
     ELSE
         FUNB = ETA/KAPPA
     END IF
     
  END FUNCTION FUNB
     
  !------------------------------------------------------- RHO-parameter
  FUNCTION FUNRHO(RHOS, RHOF, PHI)
  
     USE NRTYPE, ONLY: WP
     
     REAL(WP), INTENT(IN) :: RHOS, RHOF, PHI
     REAL(WP)             :: FUNRHO
     
     FUNRHO = (PHI*RHOF) + ((1._WP - PHI)*RHOS)
     
  END FUNCTION FUNRHO   
  
  !-------------------------------------------------------LAM_PORO - parameter
  FUNCTION FUNLAM_PORO(KD, MI_P)
  
     USE NRTYPE, ONLY: WP
     
     REAL(WP), INTENT(IN) :: KD, MI_P
     REAL(WP)             :: FUNLAM_PORO
     
     FUNLAM_PORO = KD - (2._WP/3._WP)*MI_P
     
  END FUNCTION FUNLAM_PORO
  
  !------------------------------------------------------- LAM - parameter
  FUNCTION FUNLAM(KE, MI_E)
  
     USE NRTYPE, ONLY: WP
     
     REAL(WP), INTENT(IN) :: KE, MI_E
     REAL(WP)             :: FUNLAM
     
     FUNLAM = KE - (2._WP/3._WP)*MI_E
     
  END FUNCTION FUNLAM
  
  !------------------------------------------------------- LAMC-parmater
  FUNCTION FUNLAMC(LAM_PORO, ALPHA, CAP_M)
     
     USE NRTYPE, ONLY: WP
     
     REAL(WP), INTENT(IN) :: LAM_PORO, ALPHA, CAP_M
     REAL(WP)             :: FUNLAMC
     
     FUNLAMC = LAM_PORO + (CAP_M*ALPHA*ALPHA)
     
  END FUNCTION FUNLAMC
  
  END MODULE MOD_FUNC