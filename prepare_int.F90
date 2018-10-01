!-----------------------------------------------------------------------
! subroutine calculates some expresions needed in numerical integration
!-----------------------------------------------------------------------
SUBROUTINE PREPARE_INT
  USE NRTYPE         , ONLY: WP
  USE MOD_MODEL      , ONLY: H, MX
  USE MOD_INTEGRATION


!----------------------------------------------------------------------

  IMPLICIT NONE

!----------------------------------------------------------------------

  PTS2 = PTS/2
  PTS_1= REAL(PTS,WP)
  PTS_3= PTS_1*PTS_1*PTS_1
  PTS_2= PTS_1*PTS_1

  ALLOCATE (                                                            &
    MI_FINE             ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    LAMP_LAMP_LAM_FINE  ( MX *PTS+PTS2,              PTS+PTS2            ),   &  
    LAMP_LAM_FINE       ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    LAM_FINE            ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    LAM_ALPHA_LAMP_FINE ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    ALPHA_LAM_FINE      ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    ALPHA_ALPHA_FINE    ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    CAP_M_FINE          ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    M_FINE              ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    RHO_FINE            ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    B_FINE              ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    RHOF_FINE           ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    RHO_RHOF_FINE       ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    RHOF_M_FINE         ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    INV_RHOF_FINE       ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    INV_M_FINE          ( MX *PTS+PTS2,              PTS+PTS2            ),   &
    B_M_FINE            ( MX *PTS+PTS2,              PTS+PTS2            ))

END SUBROUTINE PREPARE_INT
