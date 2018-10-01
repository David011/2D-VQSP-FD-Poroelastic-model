!-----------------------------------------------------------------------
! function returns layer, into which point x,y,z belongs
!-----------------------------------------------------------------------
FUNCTION GET_LAYER (X,Z)

  USE NRTYPE   , ONLY: WP
  USE MOD_MODEL, ONLY: LAYER_1, LAYER_2, NI                     
  USE MOD_INTERFACES, ONLY: GETDEPTH

!----------------------------------------------------------------------

  IMPLICIT NONE

  REAL(WP), INTENT(IN) :: X, Z
  INTEGER              :: GET_LAYER

  REAL(WP) :: ZS

!----------------------------------------------------------------------

  ZS = GETDEPTH(X,NI) 

  IF ( Z > ZS ) THEN
                          ! in sediments (1st layer)
    GET_LAYER = LAYER_1
    
    RETURN
  ELSE
                          ! in bedrock   (2nd layer)
    GET_LAYER = LAYER_2
    
    RETURN
  END IF

  
  
END FUNCTION GET_LAYER
