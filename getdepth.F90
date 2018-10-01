!-----------------------------------------------------------------------
! function returns depth of sediments/bedrock interface
! (i.e. interface NI) in point (x)
!-----------------------------------------------------------------------
FUNCTION GETDEPTH (X,NINF)                                              
  USE NRTYPE   , ONLY: WP
  USE MOD_MODEL, ONLY: XIMAX,        XIMIN,        ITF1, H10, NI, H

!----------------------------------------------------------------------

  IMPLICIT NONE

  REAL(WP),INTENT(IN) :: X
  INTEGER, intent(in) :: NINF
  REAL(WP)            :: GETDEPTH
  REAL(WP)            :: XA
  INTEGER             :: XIND

!----------------------------------------------------------------------

  XA = X

  IF ( XA > XIMAX ) THEN
    XA = XIMAX
  END IF

  IF ( XA < XIMIN ) THEN
    XA = XIMIN
  END IF

  XIND = INT( (XA-XIMIN) / H10  + 1._WP )
  IF (XIND == 0) XIND = 1
  GETDEPTH=REAL(ITF1(XIND,NINF),WP)
  
 
!!!    IF ( GETDEPTH > -2.*H  ) GETDEPTH = 0._WP

END FUNCTION GETDEPTH
