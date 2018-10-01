!-----------------------------------------------------------------------
! function returns index if FD cell with the same parameters already
! have one, othewise it return 0 (that means: new FD cell type found)
!-----------------------------------------------------------------------
FUNCTION GET_FD_CELL_INDEX()

  USE MOD_PARAMETERIZATION, ONLY: CELL, LAST_CELL, N_CELLS
  USE MOD_INTERFACES      , ONLY: TEST_CELLS,                      &
                                  ADD_NEW_CELL_TYPE

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER :: GET_FD_CELL_INDEX
  INTEGER :: J

!----------------------------------------------------------------------
                                        
                                        
  IF ( N_CELLS > 0 ) THEN
    IF (TEST_CELLS( LAST_CELL, CELL)) THEN   
        GET_FD_CELL_INDEX = LAST_CELL
        RETURN
    END IF                               

    DO J = 1, N_CELLS
      IF (TEST_CELLS( J, CELL )) THEN
          GET_FD_CELL_INDEX = J
          LAST_CELL         = J
          RETURN
      END IF
    END DO

  END IF

  CALL ADD_NEW_CELL_TYPE

  LAST_CELL         = N_CELLS  
                               
  GET_FD_CELL_INDEX = N_CELLS

END FUNCTION GET_FD_CELL_INDEX

