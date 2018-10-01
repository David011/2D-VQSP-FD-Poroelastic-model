!-----------------------------------------------------------------------
! function returns aproximated value of volume integral of field
! FIELD, where volume is cube with side H/PTS and center with
! coordinates (II,LI)
! note: integral is aproximated by PTS points per each side of cube
!       (that means by PTS^2 points in whole volume)
!-----------------------------------------------------------------------
FUNCTION INT3D (II,LI,FIELD)

  USE NRTYPE         , ONLY: WP
  USE MOD_INTEGRATION, ONLY: PTS, PTS2, PTS_2

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: II, LI      
  REAL(WP)             :: INT3D

  REAL(WP), DIMENSION(:,:), INTENT(INOUT) :: FIELD  

  INTEGER              :: I, L
  REAL(WP)             :: SUM

!----------------------------------------------------------------------

  SUM = 0._WP

  DO L = -PTS2+1, PTS2 

    DO I = -PTS2+1, PTS2 

      SUM = SUM + FIELD(II+I, LI+L)

    END DO
  END DO

  INT3D = SUM/PTS_2

END FUNCTION INT3D
