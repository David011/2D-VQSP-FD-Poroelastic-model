!-----------------------------------------------------------------------
! enlarges array CELLS
!-----------------------------------------------------------------------
SUBROUTINE ENLARGE
                                                                            
                                                                            
  USE NRTYPE              , ONLY: SP
  USE MOD_CONST           , ONLY: N_E_PAR,                      &
                                  NUM_OF_CELLS_TO_ENLARGE
  USE MOD_PARAMETERIZATION, ONLY: CELLS, MAXCELLS

!----------------------------------------------------------------------

  IMPLICIT NONE

  REAL(SP), DIMENSION(:,:  ), ALLOCATABLE :: TMP 
  REAL(SP), DIMENSION(:,:,:), ALLOCATABLE :: TMPY

!----------------------------------------------------------------------

  ALLOCATE ( TMP(MAXCELLS, N_E_PAR) )
  TMP = CELLS
  DEALLOCATE ( CELLS )

  

  ALLOCATE( CELLS (MAXCELLS + NUM_OF_CELLS_TO_ENLARGE, N_E_PAR) )
  

  CELLS (1:MAXCELLS,: ) = TMP
  DEALLOCATE ( TMP )

  

  MAXCELLS = MAXCELLS + NUM_OF_CELLS_TO_ENLARGE 

END SUBROUTINE ENLARGE

