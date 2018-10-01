!-----------------------------------------------------------------------
! tests of some input parameters
!-----------------------------------------------------------------------
SUBROUTINE TEST_PARAMETERS

  USE NRTYPE   , ONLY: WP
  USE MOD_MODEL, ONLY: XBMIN_G, XBMAX_G,               H,       ZBMIN, &
                       H10,                            NI
  USE MOD_INTEGRATION, ONLY: PTS
  USE MOD_FILES

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER :: I
  LOGICAL :: ERROR,EX
  REAL(WP):: TMP1, TMP2

!----------------------------------------------------------------------

  ERROR = .FALSE.

  IF ( NI < 0 ) THEN
    WRITE(*,'(A)')'NI must be >= 0'
    ERROR = .TRUE.
  END IF

  TMP1 = (XBMAX_G-XBMIN_G)/H
  TMP2 = ABS((TMP1 - ANINT(TMP1))/TMP1)
  IF ( TMP2 > 1.0E-4_WP ) THEN
    WRITE(*,*)'(XBMAX - XBMIN)/H must be a whole number (required precision 0.01%).'
    WRITE(*,'(A,D15.6)')'  actual precision [%]: ',TMP1*100.
    ERROR = .TRUE.
  END IF

  TMP1 = ABS(- ZBMIN)/H
  TMP2 = ABS((TMP1 - ANINT(TMP1))/TMP1)
  IF ( TMP2 > 1.0E-4_WP ) THEN
    WRITE(*,*)'ABS(-ZBMIN)/H must be a whole number (required precision 0.01%).'
    WRITE(*,'(A,D15.6)')'  actual precision [%]: ',TMP1*100.
    ERROR = .TRUE.
  END IF

  TMP1 = H/H10
  TMP2 = ABS((TMP1 - ANINT(TMP1))/TMP1)
  IF ( TMP2 > 1.0E-4_WP ) THEN
    WRITE(*,*)'H/H10 must be a whole number (required precision 0.01%).'
    WRITE(*,'(A,D15.6)')'  actual precision [%]: ',TMP1*100.
    ERROR = .TRUE.
  END IF

  IF ( (PTS <= 0 ) .OR. ( MOD(PTS,2) /=0 ) ) THEN
    WRITE(*,'(A)')'PTS must be a positive even number'
    WRITE(*,'(A,I5)')'  actual value: ',PTS
    ERROR = .TRUE.
  END IF

  DO I=1,NI
    INQUIRE(FILE=FSED(I),EXIST=EX)
    IF (.NOT.EX) THEN
      WRITE(*,'(A,A)')'Required file not found: ',FSED(I)
      ERROR=.TRUE.
    END IF
  END DO

  INQUIRE(FILE=JMH_FILE_NAME,EXIST=EX)
  IF (EX) THEN
    WRITE(*,'(A,A)')'File exists: ',JMH_FILE_NAME
    ERROR=.TRUE.
  END IF

  INQUIRE(FILE=MO_FILE_NAME,EXIST=EX)
  IF (EX) THEN
    WRITE(*,'(A,A)')'File exists: ',MO_FILE_NAME
    ERROR=.TRUE.
  END IF

  IF (ERROR) STOP 'Problems encountered (modify MODEL file).'

  WRITE(*,'(A)')'Testing parameters: ... Ok.'

END SUBROUTINE TEST_PARAMETERS
