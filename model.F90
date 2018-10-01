PROGRAM MODEL_2D

USE MOD_MODEL     , ONLY: KEY_FILTER
USE MOD_INTERFACES, ONLY: SETPARAMS, PRINT_INFO,                       &
                          MODEL_LIN, PARAMETERIZATION, FIND_MAX_DEPTH, &
                          WRITE_PARAM


!-----------------------------------------------------------------------

IMPLICIT NONE

REAL :: TIME, TIMEB

!-----------------------------------------------------------------------



CALL CPU_TIME (TIMEB)


CALL SETPARAMS

CALL MODEL_LIN



!----- filter aproximated interface

IF (KEY_FILTER) CALL FILTER

!----- write aproximated interface into file

CALL FIND_MAX_DEPTH

CALL PARAMETERIZATION

CALL WRITE_PARAM

CALL PRINT_INFO

CALL LOG_INFO

CALL CPU_TIME(TIME)

TIME = TIME-TIMEB
PRINT *,'CPU TIME: ',TIME,' [s]'

END PROGRAM MODEL_2D
