!-----------------------------------------------------------------------
! subroutine reads program configuration, set some run-time constants
! and check if some parameters fulfill required conditions
!-----------------------------------------------------------------------
SUBROUTINE SETPARAMS

  USE NRTYPE   , ONLY: WP
  USE MOD_MODEL           
  USE MOD_FILES      , ONLY: JMH_FILE_NAME,                            &
                             MO_FILE_NAME, JOB_NAME, FSED
  USE MOD_INTEGRATION, ONLY: PTS                                              
  USE MOD_INTERFACES , ONLY: PRINT_INFO, TEST_PARAMETERS

  USE MOD_FUNC            , ONLY: FUNM, FUNCAP_M, FUNALPHA, FUNB,      &
                                  FUNRHO, FUNLAM_PORO, FUNLAM, FUNLAMC
!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER :: I

  REAL(WP), PARAMETER :: PI = 3.1415926535897932384626433832795

  REAL(WP):: S1, S2, S3, S123, S3_2

  NAMELIST /CONTROL/ H, H10, PTS, KEY_FILTER, NPX, NI,           &
                     FLTR_RANGE

  NAMELIST /BOUNDS/ XBMIN, XBMAX, ZBMIN

  NAMELIST /MODEL/  RHOS1, RHOF1, MI1, KS1, KD1, KF1, PHI1, KAPPA1, ETA1, T1,  &
                    RHOS2, RHOF2, MI2, KS2, KD2, KF2, PHI2, KAPPA2, ETA2, T2

  NAMELIST /OUT_FILES/ JMH_FILE_NAME, MO_FILE_NAME

  NAMELIST /NAMES/     JMH_FILE_NAME, MO_FILE_NAME

  NAMELIST /CONTROLDATA2/ MX, MZ,  H, DT_VS, XBMIN

!----------------------------------------------------------------------

    H10        = -1.
    NI         = 1
    KEY_FILTER = .FALSE.  
    PTS        = 8

    OPEN  (10, FILE='HF_2DFD_VS_PORO', STATUS='OLD' )
    READ  (10,'(A)') JOB_NAME
    CLOSE (10)

    JMH_FILE_NAME = TRIM(JOB_NAME)//'.JMH'
     MO_FILE_NAME = TRIM(JOB_NAME)//'.MO'

    OPEN(10, FILE='MODEL.IN', STATUS='OLD' )
    READ(10, NML = CONTROL  )
    READ(10, NML = BOUNDS   )
    READ(10, NML = MODEL    )
    READ(10, NML = OUT_FILES)
    IF ( H10        < 0  ) H10        = H/10.
    
    ALLOCATE(FSED(NI))
    DO I = 1, NI
      READ(10,*)FSED(I)
    END DO
    CLOSE (10)

    XBMAX = H*INT((XBMAX-XBMIN)/H)+XBMIN
    ZBMIN = H*INT((     -ZBMIN)/H)
    !H10   = H/NINT(H/H10)
    XBMIN_G = XBMIN
    XBMAX_G = XBMAX


!----------------------------------------- set some run-time constants

  XIMIN = XBMIN-H
  XIMAX = XBMAX+H

  XIMIN_G = XBMIN_G-H
  XIMAX_G = XBMAX_G+H

                          !-------------------------- model parameters
  MX   = (XBMAX  -XBMIN  )/H  + 1
  MX_G = (XBMAX_G-XBMIN_G)/H  + 1

  MZ   = ABS(-ZBMIN)/H

                          !--number of points in aproximated interface
  NX10 = (XIMAX-XIMIN)/H10 + 1

  NX10_G = (XIMAX_G-XIMIN_G)/H10 + 1

  H2     = H /2._WP

  !------------------------------------ tests of some input parameters
    CALL TEST_PARAMETERS

  !---------------------------------- print info about model on screen
    CALL PRINT_INFO
    
    CAP_M1 = FUNCAP_M(KS1, KF1, KD1, PHI1)
    M1     = FUNM(RHOF1, T1, PHI1)
    ALPHA1 = FUNALPHA(KS1, KD1)
    B1     = FUNB(ETA1, KAPPA1)
    RHO1   = FUNRHO(RHOS1, RHOF1, PHI1)
    LAMP1  = FUNLAM_PORO(KD1, MI1)
    LAMC1  = FUNLAMC(LAMP1, ALPHA1, CAP_M1)
    
    CAP_M2 = FUNCAP_M(KS2, KF2, KD2, PHI2)
    M2     = FUNM(RHOF2, T2, PHI2)
    ALPHA2 = FUNALPHA(KS2, KD2)
    B2     = FUNB(ETA2, KAPPA2)
    RHO2   = FUNRHO(RHOS2, RHOF2, PHI2)
    LAMP2  = FUNLAM_PORO(KD2, MI2)
    LAMC2  = FUNLAMC(LAMP2, ALPHA2, CAP_M2)
    

    
        S1 = (M1*RHO1) - (RHOF1*RHOF1)
        S2 = M1*(LAMC1 + 2._WP*MI1) + (RHO1*CAP_M1) - (2._WP*ALPHA1*CAP_M1*RHOF1)
        S3 = (CAP_M1*LAMC1) + (2._WP*MI1*CAP_M1) - (ALPHA1*ALPHA1*CAP_M1*CAP_M1)
        S123 = SQRT(S2 - SQRT(S2*S2 - (4._WP*S1*S3)))
        S3_2 = SQRT(2._WP*S3)
        
        DT1 = 9._WP*6._WP*H*S123/7._WP/SQRT(2._WP)/S3_2/10._WP
        
        S1 = (M2*RHO2) - (RHOF2*RHOF2)
        S2 = M2*(LAMC2 + 2._WP*MI2) + (RHO2*CAP_M2) - (2._WP*ALPHA2*CAP_M2*RHOF2)
        S3 = (CAP_M2*LAMC2) + (2._WP*MI2*CAP_M2) - (ALPHA2*ALPHA2*CAP_M2*CAP_M2)
        S123 = SQRT(S2 - SQRT(S2*S2 - (4._WP*S1*S3)))
        S3_2 = SQRT(2._WP*S3)
        
        DT2 = 9._WP*6._WP*H*S123/7._WP/SQRT(2._WP)/S3_2/10._WP
        
    IF (DT1 < DT2) THEN
        DT_VS = INT(DT1/(10.**(FLOOR(LOG10(DT1))-1)))*(10.**(FLOOR(LOG10(DT1))-1))
    ELSE
        DT_VS = INT(DT2/(10.**(FLOOR(LOG10(DT2))-1)))*(10.**(FLOOR(LOG10(DT2))-1))
        DT1 = DT2
    END IF

    MX   = (XBMAX_G-XBMIN_G)/H  + 1
    
    AUXIL_W = (LAMC1 - ALPHA1 * ALPHA1 * CAP_M1)/(LAMC1 + 2._WP * MI1 - ALPHA1 * ALPHA1 * CAP_M1)
    AUXIL_QZ = ALPHA1 * 2._WP * MI1/(LAMC1 + 2._WP * MI1 - ALPHA1 * ALPHA1 * CAP_M1)

    OPEN  ( 10, FILE=TRIM(JOB_NAME)//'.INM', STATUS = 'NEW' )
    WRITE ( 10, NML = NAMES )
    WRITE ( 10, NML = CONTROLDATA2 )
    CLOSE ( 10 )
    MX   = (XBMAX  -XBMIN  )/H  + 1

END SUBROUTINE SETPARAMS

