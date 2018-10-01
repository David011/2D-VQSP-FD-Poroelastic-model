!-----------------------------------------------------------------------
! function returns value of MU, LAMBDA and RHO as function
! of coordinates x,z
!-----------------------------------------------------------------------
FUNCTION FUNC_MU_LA_RHO(X,Z)

  USE NRTYPE              , ONLY: WP
  USE MOD_MODEL           , ONLY: LAYER_1, LAYER_2,                                     &
                                  DT1,                                                  &
                                  CAP_M1, M1, ALPHA1, B1, RHO1, LAMP1, MI1,             &
                                  RHOF1, RHOS1, PHI1,                                   &
                                  CAP_M2, M2, ALPHA2, B2, RHO2, LAMP2, MI2,             &
                                  RHOF2, RHOS2, PHI2
      
  USE MOD_INTERFACES      , ONLY: GET_LAYER

!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL(WP), INTENT(IN) :: X,Z
  REAL(WP)             :: FUNC_MU_LA_RHO(17)

  INTEGER  :: LAYER

!-----------------------------------------------------------------------

  LAYER = GET_LAYER(X,Z)
 

  SELECT CASE (LAYER)
  CASE(LAYER_1)  ! in sediments (1st layer)
        FUNC_MU_LA_RHO (1)  = MI1
        FUNC_MU_LA_RHO (2)  = LAMP1 + 2._WP * MI1 - (LAMP1 * LAMP1 / (LAMP1 + 2._WP * MI1))
        FUNC_MU_LA_RHO (3)  = LAMP1 / (LAMP1 + 2._WP * MI1)
        FUNC_MU_LA_RHO (4)  = LAMP1 + 2._WP * MI1
        FUNC_MU_LA_RHO (5)  = LAMP1 + 2._WP * MI1 - (ALPHA1 * LAMP1 / (LAMP1 + 2._WP * MI1))
        FUNC_MU_LA_RHO (6)  = ALPHA1 / (LAMP1 + 2._WP * MI1)
        FUNC_MU_LA_RHO (7)  = ALPHA1 - (ALPHA1 * LAMP1 / (LAMP1 + 2._WP * MI1))
        FUNC_MU_LA_RHO (8)  = (1._WP/CAP_M1) + (ALPHA1 * ALPHA1 / (LAMP1 + 2._WP * MI1))
        FUNC_MU_LA_RHO (9)  = M1
        FUNC_MU_LA_RHO (10) = RHO1
        FUNC_MU_LA_RHO (11) = RHOF1
        FUNC_MU_LA_RHO (12) = B1
        
        FUNC_MU_LA_RHO (13) = RHO1/RHOF1
        FUNC_MU_LA_RHO (14) = RHOF1/M1
        FUNC_MU_LA_RHO (15) = 1._WP/RHOF1
        FUNC_MU_LA_RHO (16) = 1._WP/M1
        FUNC_MU_LA_RHO (17) = B1/M1
        RETURN
  CASE(LAYER_2)  ! in bedrock   (2nd layer)
        FUNC_MU_LA_RHO (1)  = MI2
        FUNC_MU_LA_RHO (2)  = LAMP2 + 2._WP * MI2 - (LAMP2 * LAMP2 / (LAMP2 + 2._WP * MI2))
        FUNC_MU_LA_RHO (3)  = LAMP2 / (LAMP2 + 2._WP * MI2)
        FUNC_MU_LA_RHO (4)  = LAMP2 + 2._WP * MI2
        FUNC_MU_LA_RHO (5)  = LAMP2 + 2._WP * MI2 - (ALPHA2 * LAMP2 / (LAMP2 + 2._WP * MI2))
        FUNC_MU_LA_RHO (6)  = ALPHA2 / (LAMP2 + 2._WP * MI2)
        FUNC_MU_LA_RHO (7)  = ALPHA2 - (ALPHA2 * LAMP2 / (LAMP2 + 2._WP * MI2))
        FUNC_MU_LA_RHO (8)  = (1._WP/CAP_M2) + (ALPHA2 * ALPHA2 / (LAMP2 + 2._WP * MI2))
        FUNC_MU_LA_RHO (9)  = M2
        FUNC_MU_LA_RHO (10) = RHO2
        FUNC_MU_LA_RHO (11) = RHOF2
        FUNC_MU_LA_RHO (12) = B2
        
        FUNC_MU_LA_RHO (13) = RHO2/RHOF2
        FUNC_MU_LA_RHO (14) = RHOF2/M2
        FUNC_MU_LA_RHO (15) = 1._WP/RHOF2
        FUNC_MU_LA_RHO (16) = 1._WP/M2
        FUNC_MU_LA_RHO (17) = B2/M2
    RETURN
  END SELECT


END FUNCTION FUNC_MU_LA_RHO
