!-----------------------------------------------------------------------
! tests 2 FD cells
!-----------------------------------------------------------------------
LOGICAL FUNCTION TEST_CELLS( LAST_CELL, CELL )

  USE NRTYPE              , ONLY: WP, SP
  USE MOD_CONST           , ONLY: N_E_PAR
  USE MOD_PARAMETERIZATION, ONLY: CELLS

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER ,                     INTENT(IN) :: LAST_CELL
  REAL(SP), DIMENSION(N_E_PAR), INTENT(IN) :: CELL

  INTEGER :: I

!----------------------------------------------------------------------

  DO I = 1, N_E_PAR
	  IF ( CELLS(LAST_CELL,I)/=CELL(I) ) THEN  
      TEST_CELLS = .FALSE.
      RETURN
    END IF
  END DO

  TEST_CELLS = .TRUE.

END FUNCTION TEST_CELLS
