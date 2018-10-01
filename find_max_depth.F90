!-----------------------------------------------------------------------
! function returns the maximum depth of sediments/bedrock interface
! (i.e. interface NI)
!-----------------------------------------------------------------------
SUBROUTINE FIND_MAX_DEPTH

  USE NRTYPE   , ONLY: WP
  USE MOD_MODEL, ONLY: MAXDEPTH, ITF1, NI

!----------------------------------------------------------------------

  IMPLICIT NONE

!----------------------------------------------------------------------

  MAXDEPTH = REAL(MINVAL(ITF1(:,NI)),WP)

END SUBROUTINE FIND_MAX_DEPTH
