!-----------------------------------------------------------------------
! adds new cell type
!-----------------------------------------------------------------------
SUBROUTINE ADD_NEW_CELL_TYPE

  USE MOD_PARAMETERIZATION, ONLY: N_CELLS, MAXCELLS,                   &
                                  CELLS, CELL

!----------------------------------------------------------------------

  IMPLICIT NONE

!----------------------------------------------------------------------

  N_CELLS = N_CELLS + 1

  IF ( N_CELLS > MAXCELLS ) THEN
    CALL ENLARGE
  END IF

  CELLS(N_CELLS,:) = CELL
  
END SUBROUTINE ADD_NEW_CELL_TYPE

