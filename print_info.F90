!-----------------------------------------------------------------------
! print info about model on screen
!-----------------------------------------------------------------------
SUBROUTINE PRINT_INFO

  USE MOD_MODEL, ONLY: H,     H10, KEY_FILTER,      MX_G,              &
                       XIMIN,        XIMAX,              MZ,           &
                       XBMIN_G, XBMAX_G, ZBMIN, XIMAX_G

  IMPLICIT NONE

  WRITE(*,*)'Coordinate system: x -> east'
  WRITE(*,*)'                   z -> up'
  WRITE(*,*)'Grid spacing           : h   = ',H
  WRITE(*,*)'Spacing for integration: h10 = ',H10
  IF (KEY_FILTER) THEN
    WRITE(*,*)'Smooth interface sediments/bedrock is ON'
  ELSE
    WRITE(*,*)'Smooth interface sediments/bedrock is OFF'
  END IF
  WRITE(*,*)'Bounderies for interface sediments/bedrock aproximation:'
  WRITE(*,*)'  ximin = ',XIMIN,' ximax = ',XIMAX_G
  WRITE(*,*)'Calculation model boundaries: '
  WRITE(*,*)'  xbmin = ',XBMIN_G,' xbmax = ',XBMAX_G
  WRITE(*,*)'  zbmin = ',ZBMIN
  WRITE(*,*)'Number of grid cells in '
  WRITE(*,*) MX_G , MZ+1
  WRITE(*,*)'Model parameters: MX   = ',MX_G
  WRITE(*,*)'                  MZ   = ',MZ
  WRITE(*,*)' '
END SUBROUTINE PRINT_INFO

!-----------------------------------------------------------------------
! logs info about model into a file
!-----------------------------------------------------------------------
SUBROUTINE LOG_INFO

  USE MOD_PARAMETERIZATION, ONLY: N_CELLS_G
  USE MOD_MODEL, ONLY: H,     H10, KEY_FILTER,      MX_G,              &
                       XIMIN,        XIMAX,              MZ,           &
                       XBMIN_G, XBMAX_G, ZBMIN, XIMAX_G

  IMPLICIT NONE

  OPEN(99,file='model.log')
  WRITE(99,*)'Coordinate system: x -> east'
  WRITE(99,*)'                   z -> up'
  WRITE(99,*)'Grid spacing           : h   = ',H
  WRITE(99,*)'Spacing for integration: h10 = ',H10
  IF (KEY_FILTER) THEN
    WRITE(99,*)'Smooth interface sediments/bedrock is ON'
  ELSE
    WRITE(99,*)'Smooth interface sediments/bedrock is OFF'
  END IF
  WRITE(99,*)'Bounderies for interface sediments/bedrock aproximation:'
  WRITE(99,*)'  ximin = ',XIMIN,' ximax = ',XIMAX_G
  WRITE(99,*)'Calculation model boundaries: '
  WRITE(99,*)'  xbmin = ',XBMIN_G,' xbmax = ',XBMAX_G
  WRITE(99,*)'  zbmin = ',ZBMIN
  WRITE(99,*)'Number of grid cells in '
  WRITE(99,*) MX_G , MZ+1
  WRITE(99,*)'Model parameters: MX   = ',MX_G
  WRITE(99,*)'                  MZ   = ',MZ
  WRITE(99,*)' '
  WRITE(99,*)'TYPES OF MAT. CELLS: ',N_CELLS_G
  WRITE(99,*)' '

END SUBROUTINE LOG_INFO
