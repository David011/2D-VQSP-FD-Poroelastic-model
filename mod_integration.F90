MODULE MOD_INTEGRATION

  USE NRTYPE   , ONLY: WP

  IMPLICIT NONE

  INTEGER  :: PTS, PTS2
  REAL(WP) :: PTS_1, PTS_2, PTS_3

  REAL(WP)   , DIMENSION (:,  :)  , ALLOCATABLE ::                     &
               MI_FINE, LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE,   &
               LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, ALPHA_ALPHA_FINE,  &
               CAP_M_FINE,                                             &    
               M_FINE, RHO_FINE, RHOF_FINE, B_FINE,                    &
               RHO_RHOF_FINE, RHOF_M_FINE, INV_RHOF_FINE, INV_M_FINE,  &
               B_M_FINE
END MODULE MOD_INTEGRATION
