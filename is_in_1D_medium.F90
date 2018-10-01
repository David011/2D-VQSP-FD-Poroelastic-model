!***********************************************************************
! function returns true if whole volume over which avarage of any
! parameter should be calculated is inside 1D-inhomogeneous medium
! (lies inside 1D-IH bedrock)
! (X,Y,Z) - position of "free point" in FD cell
!***********************************************************************
FUNCTION IS_IN_1D_MEDIUM(X,Z)

  USE NRTYPE        , ONLY: WP
  USE MOD_MODEL     , ONLY: H, H2, NI
  USE MOD_INTERFACES, ONLY: GETDEPTH

!----------------------------------------------------------------------

  IMPLICIT NONE

  REAL(WP), INTENT(IN) :: X, Z
  LOGICAL              :: IS_IN_1D_MEDIUM

  REAL(WP) :: X1,X2,GH2,Z0,Z1

!----------------------------------------------------------------------

  X1 = X - H2
  X2 = X + H
  GH2 = H2


  Z0 = GETDEPTH(X1,NI) - GH2
  IF (Z0 > Z)  THEN
      
    Z0 = GETDEPTH(X2,ni) - GH2
    
    IF (Z0 > Z) THEN
      IS_IN_1D_MEDIUM = .TRUE.
      
    ELSE
      IS_IN_1D_MEDIUM = .FALSE.
      
    END IF
  ELSE
    IS_IN_1D_MEDIUM = .FALSE.
    
  END IF

END FUNCTION IS_IN_1D_MEDIUM
