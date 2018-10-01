MODULE MOD_EFFMATPAR

CONTAINS


FUNCTION MUXZ (II,LI,MU)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS_1, PTS_2, PTS2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: MUXZ

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: MU

  INTEGER              :: I, L
  REAL(WP)             :: SUM2

!----------------------------------------------------------------------

  SUM2 = 0.

  DO L = -PTS2+1, PTS2
    DO I = -PTS2+1, PTS2
      SUM2 = SUM2 + 1._WP/MU(II+I, LI+L)
    END DO
  END DO

  MUXZ = PTS_2/SUM2

END FUNCTION MUXZ

FUNCTION PI_X (II,LI,LAMP_LAMP_LAM, LAMP_LAM, LAM)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS_1, PTS_2, PTS2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: PI_X

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: LAMP_LAMP_LAM, LAMP_LAM, LAM

  INTEGER              :: I
  REAL(WP)             :: SUM1

!----------------------------------------------------------------------

  SUM1 = 0.

  DO I = -PTS2+1, PTS2
     SUM1 = SUM1 + 1._WP/L2M_Z()
  END DO
  
  PI_X = PTS_1/SUM1
  
  CONTAINS

  FUNCTION L2M_Z()

    REAL(WP)             :: L2M_Z

    INTEGER              :: L
    REAL(WP)             :: S1, S2, S3

  !----------------------------------------------------------------------

    S1 = 0.
    S2 = 0.
    S3 = 0.
    DO L = -PTS2+1, PTS2
      S1 = S1 + LAMP_LAMP_LAM(II+I, LI+L)
      S2 = S2 + LAMP_LAM     (II+I, LI+L)
      S3 = S3 + 1._WP/(LAM      (II+I, LI+L))
    END DO

    S1 = S1/PTS_1
    S2 = S2/PTS_1
    S3 = S3/PTS_1

    L2M_Z = S1 + S2*S2/S3

  END FUNCTION L2M_Z

END FUNCTION PI_X
  
!======================================================================

FUNCTION PI_Z (II,LI,LAMP_LAMP_LAM, LAMP_LAM, LAM)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS_1, PTS_2, PTS2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: PI_Z

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: LAMP_LAMP_LAM, LAMP_LAM, LAM

  INTEGER              :: L
  REAL(WP)             :: SUM1

!----------------------------------------------------------------------

  SUM1 = 0.

  DO L = -PTS2+1, PTS2
     SUM1 = SUM1 + 1._WP/L2M_X()
  END DO
  
  PI_Z = PTS_1/SUM1
  
  CONTAINS

  FUNCTION L2M_X()

    REAL(WP)             :: L2M_X

    INTEGER              :: I
    REAL(WP)             :: S1, S2, S3

  !----------------------------------------------------------------------

    S1 = 0.
    S2 = 0.
    S3 = 0.
    DO I = -PTS2+1, PTS2
      S1 = S1 + LAMP_LAMP_LAM(II+I, LI+L)
      S2 = S2 + LAMP_LAM     (II+I, LI+L)
      S3 = S3 + 1._WP/(LAM      (II+I, LI+L))
    END DO

    S1 = S1/PTS_1
    S2 = S2/PTS_1
    S3 = S3/PTS_1

    L2M_X = S1 + S2*S2/S3

  END FUNCTION L2M_X

END FUNCTION PI_Z

  !======================================================================
  
FUNCTION XXP (II,LI,LAM_ALPHA_LAMP, ALPHA_LAM, LAMP_LAM, LAM, ALPHA_ALPHA)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS_1, PTS_2, PTS2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: XXP

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: LAM_ALPHA_LAMP, ALPHA_LAM, LAMP_LAM, LAM, ALPHA_ALPHA

  INTEGER              :: I
  REAL(WP)             :: SUM1, SUM2

!----------------------------------------------------------------------

  SUM1 = 0.
  SUM2 = 0.

  DO I = -PTS2+1, PTS2
     SUM1 = SUM1 + 1._WP/TEMP_Z1()
     SUM2 = SUM2 +    TEMP_Z2()
  END DO
  
  XXP = (PTS_1/SUM1) * (SUM2/PTS_1)
  
  CONTAINS

  FUNCTION TEMP_Z1()

    REAL(WP)             :: TEMP_Z1

    INTEGER              :: L
    REAL(WP)             :: S1, S2, S3, S4

  !----------------------------------------------------------------------

    S1 = 0.
    S2 = 0.
    S3 = 0.
    S4 = 0.
    DO L = -PTS2+1, PTS2
      S1 = S1 + LAM_ALPHA_LAMP(II+I, LI+L)
      S2 = S2 + ALPHA_LAM     (II+I, LI+L)
      S3 = S3 + LAMP_LAM      (II+I, LI+L)
      S4 = S4 + 1._WP/(LAM       (II+I, LI+L))
    END DO

    S1 = S1/PTS_1
    S2 = S2/PTS_1
    S3 = S3/PTS_1
    S4 = S4/PTS_1

    TEMP_Z1 = S1 + S2*S3/S4

  END FUNCTION TEMP_Z1
  
  FUNCTION TEMP_Z2()

    REAL(WP)             :: TEMP_Z2

    INTEGER              :: L
    REAL(WP)             :: S1, S2, S3, S4, S5

  !----------------------------------------------------------------------

    S1 = 0.
    S2 = 0.
    S3 = 0.
    S4 = 0.
    S5 = 0.
    DO L = -PTS2+1, PTS2
      S1 = S1 + ALPHA_ALPHA    (II+I, LI+L)
      S2 = S2 + ALPHA_LAM      (II+I, LI+L)
      S3 = S3 + LAMP_LAM       (II+I, LI+L)
      S4 = S4 + 1._WP/(LAM        (II+I, LI+L))
      S5 = S5 + LAM_ALPHA_LAMP (II+I, LI+L)
    END DO

    S1 = S1/PTS_1
    S2 = S2/PTS_1
    S3 = S3/PTS_1
    S4 = S4/PTS_1
    S5 = S5/PTS_1

    TEMP_Z2 = (- S1 - S2*S3/S4)/(S5 + S2*S3/S4)

  END FUNCTION TEMP_Z2

END FUNCTION XXP
  
   !======================================================================
  
FUNCTION ZZP (II,LI,LAM_ALPHA_LAMP, ALPHA_LAM, LAMP_LAM, LAM, ALPHA_ALPHA)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS_1, PTS_2, PTS2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: ZZP

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: LAM_ALPHA_LAMP, ALPHA_LAM, LAMP_LAM, LAM, ALPHA_ALPHA

  INTEGER              :: L
  REAL(WP)             :: SUM1, SUM2

!----------------------------------------------------------------------

  SUM1 = 0.
  SUM2 = 0.

  DO L = -PTS2+1, PTS2
     SUM1 = SUM1 + 1._WP/TEMP_X1()
     SUM2 = SUM2 +    TEMP_X2()
  END DO
  
  ZZP = (PTS_1/SUM1) * (SUM2/PTS_1)
  
  CONTAINS

  FUNCTION TEMP_X1()

    REAL(WP)             :: TEMP_X1

    INTEGER              :: I
    REAL(WP)             :: S1, S2, S3, S4

  !----------------------------------------------------------------------

    S1 = 0.
    S2 = 0.
    S3 = 0.
    S4 = 0.
    DO I = -PTS2+1, PTS2
      S1 = S1 + LAM_ALPHA_LAMP(II+I, LI+L)
      S2 = S2 + ALPHA_LAM     (II+I, LI+L)
      S3 = S3 + LAMP_LAM      (II+I, LI+L)
      S4 = S4 + 1._WP/(LAM       (II+I, LI+L))
    END DO

    S1 = S1/PTS_1
    S2 = S2/PTS_1
    S3 = S3/PTS_1
    S4 = S4/PTS_1

    TEMP_X1 = S1 + S2*S3/S4

  END FUNCTION TEMP_X1
  
  FUNCTION TEMP_X2()

    REAL(WP)             :: TEMP_X2

    INTEGER              :: I
    REAL(WP)             :: S1, S2, S3, S4, S5

  !----------------------------------------------------------------------

    S1 = 0.
    S2 = 0.
    S3 = 0.
    S4 = 0.
    S5 = 0.
    DO I = -PTS2+1, PTS2
      S1 = S1 + ALPHA_ALPHA    (II+I, LI+L)
      S2 = S2 + ALPHA_LAM      (II+I, LI+L)
      S3 = S3 + LAMP_LAM       (II+I, LI+L)
      S4 = S4 + 1._WP/(LAM        (II+I, LI+L))
      S5 = S5 + LAM_ALPHA_LAMP (II+I, LI+L)
    END DO

    S1 = S1/PTS_1
    S2 = S2/PTS_1
    S3 = S3/PTS_1
    S4 = S4/PTS_1
    S5 = S5/PTS_1

    TEMP_X2 = (- S1 - S2*S3/S4)/(S5 + S2*S3/S4)

  END FUNCTION TEMP_X2

  END FUNCTION ZZP
  
  
  FUNCTION INT2DZX (II, LI, NUMRTR, DENMNTR)
  
    USE NRTYPE         , ONLY: WP
    USE MOD_INTEGRATION, ONLY: PTS_1, PTS2
  
  !----------------------------------------------------------------------
  
    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: II, LI
    REAL(WP)             :: INT2DZX

    REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: NUMRTR, DENMNTR

    INTEGER              :: L
    REAL(WP)             :: SUM

!----------------------------------------------------------------------
  
    SUM  = 0.
    
    DO L = -PTS2+1, PTS2
       SUM = SUM + INT1DX()
    END DO
    INT2DZX = SUM/PTS_1
    
    CONTAINS

    FUNCTION INT1DX()

     REAL(WP)             :: INT1DX

     INTEGER              :: I
     REAL(WP)             :: S1, S2

   !----------------------------------------------------------------------

     S1 = 0.
     S2 = 0.
     DO I = -PTS2+1, PTS2
        S1 = S1 + NUMRTR  (II+I, LI+L)
        S2 = S2 + DENMNTR (II+I, LI+L)
     END DO

     S1 = S1/PTS_1
     S2 = S2/PTS_1

     INT1DX = S1/S2

    END FUNCTION INT1DX

  END FUNCTION INT2DZX
  
  FUNCTION INT2DZX_1 (II, LI, DENMNTR)
  
    USE NRTYPE         , ONLY: WP
    USE MOD_INTEGRATION, ONLY: PTS_1, PTS2
  
  !----------------------------------------------------------------------
  
    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: II, LI
    REAL(WP)             :: INT2DZX_1

    REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: DENMNTR

    INTEGER              :: L
    REAL(WP)             :: SUM

!----------------------------------------------------------------------
  
    SUM  = 0.
    
    DO L = -PTS2+1, PTS2
       SUM = SUM + INT1DX_1()
    END DO
    INT2DZX_1 = SUM/PTS_1
    
    CONTAINS

    FUNCTION INT1DX_1()

     REAL(WP)             :: INT1DX_1

     INTEGER              :: I
     REAL(WP)             :: S1

   !----------------------------------------------------------------------

     S1 = 0.
     DO I = -PTS2+1, PTS2
        S1 = S1 + DENMNTR  (II+I, LI+L)
     END DO

     S1 = S1/PTS_1

     INT1DX_1 = 1._WP/S1

    END FUNCTION INT1DX_1

  END FUNCTION INT2DZX_1
  
  FUNCTION INT2DXZ (II, LI, NUMRTR, DENMNTR)
  
    USE NRTYPE         , ONLY: WP
    USE MOD_INTEGRATION, ONLY: PTS_1, PTS2
  
  !----------------------------------------------------------------------
  
    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: II, LI
    REAL(WP)             :: INT2DXZ

    REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: NUMRTR, DENMNTR

    INTEGER              :: I
    REAL(WP)             :: SUM

!----------------------------------------------------------------------
  
    SUM  = 0.
    
    DO I = -PTS2+1, PTS2
       SUM = SUM + INT1DZ()
    END DO
    INT2DXZ = SUM/PTS_1
    
    CONTAINS

    FUNCTION INT1DZ()

     REAL(WP)             :: INT1DZ

     INTEGER              :: L
     REAL(WP)             :: S1, S2

   !----------------------------------------------------------------------

     S1 = 0.
     S2 = 0.
     DO L = -PTS2+1, PTS2
        S1 = S1 + NUMRTR  (II+I, LI+L)
        S2 = S2 + DENMNTR (II+I, LI+L)
     END DO

     S1 = S1/PTS_1
     S2 = S2/PTS_1

     INT1DZ = S1/S2

    END FUNCTION INT1DZ

  END FUNCTION INT2DXZ
  
  FUNCTION INT2DXZ_1 (II, LI, DENMNTR)
  
    USE NRTYPE         , ONLY: WP
    USE MOD_INTEGRATION, ONLY: PTS_1, PTS2
  
  !----------------------------------------------------------------------
  
    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: II, LI
    REAL(WP)             :: INT2DXZ_1

    REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: DENMNTR

    INTEGER              :: I
    REAL(WP)             :: SUM

!----------------------------------------------------------------------
  
    SUM  = 0.
    
    DO I = -PTS2+1, PTS2
       SUM = SUM + INT1DZ_1()
    END DO
    INT2DXZ_1 = SUM/PTS_1
    
    CONTAINS

    FUNCTION INT1DZ_1()

     REAL(WP)             :: INT1DZ_1

     INTEGER              :: L
     REAL(WP)             :: S1

   !----------------------------------------------------------------------

     S1 = 0.
     DO L = -PTS2+1, PTS2
        S1 = S1 + DENMNTR  (II+I, LI+L)
     END DO

     S1 = S1/PTS_1

     INT1DZ_1 = 1._WP/S1

    END FUNCTION INT1DZ_1

  END FUNCTION INT2DXZ_1
    
  FUNCTION INT2D_QX_ZX (II, LI, NUMRTR, DENMNTR)
  
    USE NRTYPE         , ONLY: WP
    USE MOD_INTEGRATION, ONLY: PTS_1, PTS2
  
  !----------------------------------------------------------------------
  
    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: II, LI
    REAL(WP)             :: INT2D_QX_ZX

    REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: NUMRTR, DENMNTR

    INTEGER              :: L
    REAL(WP)             :: SUM

!----------------------------------------------------------------------
  
    SUM  = 0.
    
    DO L = -PTS2+1, PTS2
       SUM = SUM + INT1D_QX_X()
    END DO
    INT2D_QX_ZX = SUM/PTS_1
    
    CONTAINS

    FUNCTION INT1D_QX_X()

     REAL(WP)             :: INT1D_QX_X

     INTEGER              :: I
     REAL(WP)             :: S1, S2

   !----------------------------------------------------------------------

     S1 = 0.
     S2 = 0.
     DO I = -PTS2+1, PTS2
        S1 = S1 +        NUMRTR  (II+I, LI+L)
        S2 = S2 + (1._WP/DENMNTR (II+I, LI+L))
     END DO

     S1 = 1._WP/(S1/PTS_1)
     S2 = S2/PTS_1

     INT1D_QX_X = S1/S2

    END FUNCTION INT1D_QX_X

  END FUNCTION INT2D_QX_ZX
    
FUNCTION INT2D_QZ_XZ (II, LI, NUMRTR, DENMNTR)
  
    USE NRTYPE         , ONLY: WP
    USE MOD_INTEGRATION, ONLY: PTS_1, PTS2
  
  !----------------------------------------------------------------------
  
    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: II, LI
    REAL(WP)             :: INT2D_QZ_XZ

    REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: NUMRTR, DENMNTR

    INTEGER              :: I
    REAL(WP)             :: SUM

!----------------------------------------------------------------------
  
    SUM  = 0.
    
    DO I = -PTS2+1, PTS2
       SUM = SUM + INT1D_QZ_Z()
    END DO
    INT2D_QZ_XZ = SUM/PTS_1
    
    CONTAINS

    FUNCTION INT1D_QZ_Z()

     REAL(WP)             :: INT1D_QZ_Z

     INTEGER              :: L
     REAL(WP)             :: S1, S2

   !----------------------------------------------------------------------

     S1 = 0.
     S2 = 0.
     DO L = -PTS2+1, PTS2
        S1 = S1 +        NUMRTR  (II+I, LI+L)
        S2 = S2 + (1._WP/DENMNTR (II+I, LI+L))
     END DO

     S1 = 1._WP/(S1/PTS_1)
     S2 = S2/PTS_1

     INT1D_QZ_Z = S1/S2

    END FUNCTION INT1D_QZ_Z

 END FUNCTION INT2D_QZ_XZ
    
 FUNCTION INT3DZX(II,LI,FIELD)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS, PTS2, PTS_2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: INT3DZX

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: FIELD

  INTEGER              :: I, L
  REAL(WP)             :: SUM

!----------------------------------------------------------------------

  SUM = 0._WP

  DO L = -PTS2+1, PTS2

    DO I = -PTS2+1, PTS2

      SUM = SUM + 1._WP/FIELD(II+I, LI+L)

    END DO
  END DO

  INT3DZX = SUM/PTS_2

 END FUNCTION INT3DZX
 
 FUNCTION INT3DXZ(II,LI,FIELD)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS, PTS2, PTS_2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI
  REAL(WP)             :: INT3DXZ

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: FIELD

  INTEGER              :: I, L
  REAL(WP)             :: SUM

!----------------------------------------------------------------------

  SUM = 0._WP

  DO I = -PTS2+1, PTS2

    DO L = -PTS2+1, PTS2

      SUM = SUM + 1._WP/FIELD(II+I, LI+L)

    END DO
  END DO

  INT3DXZ = SUM/PTS_2

 END FUNCTION INT3DXZ

END MODULE MOD_EFFMATPAR
