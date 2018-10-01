!-----------------------------------------------------------------------
! main procedure of this module
!-----------------------------------------------------------------------
SUBROUTINE PARAMETERIZATION

  USE MOD_MODEL           , ONLY: MZ
  USE MOD_PARAMETERIZATION, ONLY: LSTART,  N_PLANES
  USE MOD_INTERFACES      , ONLY: ALLOCATE_ARRAYS
!----------------------------------------------------------------------

  IMPLICIT NONE

!----------------------------------------------------------------------

  INTERFACE
    SUBROUTINE     PARAMETERIZE_GRID
    END SUBROUTINE PARAMETERIZE_GRID


    SUBROUTINE     PREPARE_INT
    END SUBROUTINE PREPARE_INT
  END INTERFACE

!----------------------------------------------------------------------

    WRITE(*,'(A)')'Parameterize medium using avaraging of physical &
                  &parameters'


  CALL PREPARE_INT

  CALL ALLOCATE_ARRAYS

  LSTART = 1

  N_PLANES = MZ + 1

  CALL PARAMETERIZE_GRID

END SUBROUTINE PARAMETERIZATION
