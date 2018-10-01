!***********************************************************************
! allocates arrays for physical parameters
!***********************************************************************
SUBROUTINE ALLOCATE_ARRAYS

  USE MOD_CONST           , ONLY: N_E_PAR
  USE MOD_MODEL           , ONLY: MX, MZ
  USE MOD_PARAMETERIZATION, ONLY: JMH,       CELLS, MAXCELLS,  &
                                  N_CELLS

!----------------------------------------------------------------------

  IMPLICIT NONE

!----------------------------------------------------------------------

  ALLOCATE( CELLS (MX, N_E_PAR) )
  N_CELLS = 0
  MAXCELLS = MX
  ALLOCATE( JMH  (MX, 0:MZ) )

END SUBROUTINE ALLOCATE_ARRAYS

