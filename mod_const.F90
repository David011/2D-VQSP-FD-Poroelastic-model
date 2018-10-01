MODULE MOD_CONST

! module contains all build-time constants
  USE NRTYPE, ONLY: WP

  IMPLICIT NONE

  INTEGER, PARAMETER  :: N_FREQ  = 3 ,                                 &
                         N_E_PAR = 24 ,                                &
                         N_Y_PAR = 6 ,                                 &
                         I2N_FREQ1 = 5,                                &
                         NUM_OF_CELLS_TO_ENLARGE = 50000

END MODULE MOD_CONST
