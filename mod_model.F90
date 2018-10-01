MODULE MOD_MODEL

  USE NRTYPE, ONLY: WP, SP

! module contains procedures, function and variables for
! creating and working with aproximation of sediments/bedrock interface

!------------------------------------------------------------- constants
  IMPLICIT NONE

  INTEGER, PARAMETER :: LAYER_1 = 1
  INTEGER, PARAMETER :: LAYER_2 = 2


!------------------------------------------------------ public variables

  INTEGER      :: NPX, NX10, MX, MZ, NI, MX_G, NX10_G, X_MIN                  
                                                                             
                                                                            
  
  
  REAL(SP)     :: H, DT1, DT2, DT_VS                                        
  REAL(WP)     :: H2, H10
  REAL(SP)     :: ZBMIN, XBMIN_G, XBMAX_G
  REAL(WP)     :: XBMIN, XBMAX, XIMIN_G, XIMAX_G                            
  REAL(WP)     :: XIMIN, XIMAX, MAXDEPTH                                     
  REAL(SP)     :: FLTR_RANGE                                                
  REAL(WP)     :: RHOS1, RHOF1, MI1, &
                  KS1, KD1, KF1, PHI1, KAPPA1, ETA1, T1
  REAL(WP)     :: RHOS2, RHOF2, MI2, &
                  KS2, KD2, KF2, PHI2, KAPPA2, ETA2, T2
  REAL(WP)     :: CAP_M1, M1, ALPHA1, B1, RHO1, LAMP1, LAMC1
  REAL(WP)     :: CAP_M2, M2, ALPHA2, B2, RHO2, LAMP2, LAMC2
  LOGICAL      :: KEY_FILTER                                                
  REAL(WP)     :: AUXIL_W, AUXIL_QZ
                                                                             
  
  REAL(SP), ALLOCATABLE, DIMENSION (:,:) :: ITF1

END MODULE MOD_MODEL
