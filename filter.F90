!-----------------------------------------------------------------------
! subroutine implements running avarage on aproximated sediments/bedrock
! interface with window width = RANGE
!-----------------------------------------------------------------------
SUBROUTINE FILTER

  USE NRTYPE   , ONLY: WP, SP
  USE MOD_MODEL, ONLY: NX10, FLTR_RANGE, H10, ITF1, NI

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER  :: I, IM, IMP, RACNT, RACNT_START, RACNT_STOPX, NORM, k
  REAL(WP) :: SUM

  REAL(WP), DIMENSION(:), ALLOCATABLE :: OUT

!----------------------------------------------------------------------

  DO K = 1, NI
!    WRITE(*,'(A)')'Filtering interface(-s)...'

    ALLOCATE(OUT(NX10))

    OUT = 100. !bad value to indicate bad filtering

    RACNT = INT(FLTR_RANGE/2./H10+1.)

    RACNT_START = RACNT + 1
    RACNT_STOPX = NX10-RACNT

    ! Inner points
    NORM = (2*RACNT+1)
    !(i,j) is position of point in which calculating running avarage
                                         !cycle through all inner points
    DO I = RACNT_START, RACNT_STOPX
      SUM = 0.
                                    !cycle through all adjacent points
        DO IM = -RACNT,RACNT
          SUM = SUM + ITF1(I+IM,k)
        END DO

      OUT(I) = SUM/REAL(NORM,SP)

    END DO

    ! Right margin (x around ximax)
    IMP = RACNT-1
    DO I = RACNT_STOPX+1, NX10
      NORM = (RACNT+1+IMP)
      SUM  = 0.
      DO IM = -RACNT,IMP
        SUM = SUM + ITF1(I+IM,k)
      END DO
      OUT(I) = SUM/REAL(NORM,SP)
      IMP = IMP-1
    END DO

    ! Left margin (x around ximin)
    IMP = 0
    DO I = 1, RACNT
      NORM = (RACNT+1+IMP)
      SUM  = 0.
      DO IM = -IMP,RACNT
        SUM = SUM + ITF1(I+IM,k)
      END DO
      OUT(I) = SUM/REAL(NORM,SP)
      IMP = IMP+1
    END DO

    ITF1(:,k) = OUT

    DEALLOCATE(OUT)

!    WRITE(*,'(A)')'OK'

  END DO

END SUBROUTINE FILTER

