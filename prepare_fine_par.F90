SUBROUTINE PREPARE_FINE_PAR (MXT, L, H, Z0)

  USE NRTYPE         , ONLY: WP, SP
  USE MOD_MODEL      , ONLY: XBMIN
  USE MOD_INTEGRATION, ONLY: PTS, PTS2,                                              &
                             MI_FINE, LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE,   &
                             LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, ALPHA_ALPHA_FINE,  &
                             CAP_M_FINE,                                             &   
                             M_FINE, RHO_FINE, RHOF_FINE, B_FINE,                    &
                             RHO_RHOF_FINE, RHOF_M_FINE, INV_RHOF_FINE, INV_M_FINE,  &
                             B_M_FINE
  USE MOD_INTERFACES , ONLY: FUNC_MU_LA_RHO
!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER , INTENT(IN) :: MXT, L
  REAL(SP), INTENT(IN) :: H, Z0

  INTEGER              :: II, LI

  REAL(WP)             :: HPTS_H, H2_H, XSTART, ZSTART, PAR(17), X, Z

!----------------------------------------------------------------------

  HPTS_H  =  H/REAL(PTS,WP)
  H2_H    = (H - HPTS_H)/2._WP

  ZSTART = Z0              - REAL(L -1,WP)*H + H2_H
  XSTART = XBMIN - H/2._WP                   - H2_H

  DO LI = PTS+PTS2, PTS2+1, -1
    Z = ZSTART - REAL(LI-1,WP)*HPTS_H

    DO II = 1, MXT*PTS+PTS2
      X = XSTART + REAL(II-1,WP)*HPTS_H

      PAR = FUNC_MU_LA_RHO (X,Z)
       MI_FINE             (II,LI) = PAR(1)
       LAMP_LAMP_LAM_FINE  (II,LI) = PAR(2)
       LAMP_LAM_FINE       (II,LI) = PAR(3)
       LAM_FINE            (II,LI) = PAR(4)
       LAM_ALPHA_LAMP_FINE (II,LI) = PAR(5)
       ALPHA_LAM_FINE      (II,LI) = PAR(6)
       ALPHA_ALPHA_FINE    (II,LI) = PAR(7)
       CAP_M_FINE          (II,LI) = PAR(8)
       M_FINE              (II,LI) = PAR(9)
       RHO_FINE            (II,LI) = PAR(10)
       RHOF_FINE           (II,LI) = PAR(11)
       B_FINE              (II,LI) = PAR(12)
       
       RHO_RHOF_FINE       (II,LI) = PAR(13)
       RHOF_M_FINE         (II,LI) = PAR(14)
       INV_RHOF_FINE       (II,LI) = PAR(15)
       INV_M_FINE          (II,LI) = PAR(16)
       B_M_FINE            (II,LI) = PAR(17)
    END DO
    
  END DO

  IF ( L == 1 ) THEN

    DO LI = 1, PTS2
       MI_FINE             (:,LI) = PAR(1)
       LAMP_LAMP_LAM_FINE  (:,LI) = PAR(2)
       LAMP_LAM_FINE       (:,LI) = PAR(3)
       LAM_FINE            (:,LI) = PAR(4)
       LAM_ALPHA_LAMP_FINE (:,LI) = PAR(5)
       ALPHA_LAM_FINE      (:,LI) = PAR(6)
       ALPHA_ALPHA_FINE    (:,LI) = PAR(7)
       CAP_M_FINE          (:,LI) = PAR(8)
       M_FINE              (:,LI) = PAR(9)
       RHO_FINE            (:,LI) = PAR(10)
       RHOF_FINE           (:,LI) = PAR(11)
       B_FINE              (:,LI) = PAR(12)
       
       RHO_RHOF_FINE       (:,LI) = PAR(13)
       RHOF_M_FINE         (:,LI) = PAR(14)
       INV_RHOF_FINE       (:,LI) = PAR(15)
       INV_M_FINE          (:,LI) = PAR(16)
       B_M_FINE            (:,LI) = PAR(17)
    END DO

  ELSE

    DO LI = 1, PTS2
      Z = ZSTART - REAL(LI-1,WP)*HPTS_H

      DO II = 1, MXT*PTS+PTS2
        X = XSTART + REAL(II-1,WP)*HPTS_H

        PAR = FUNC_MU_LA_RHO (X,Z)
        MI_FINE             (II,LI) = PAR(1)
        LAMP_LAMP_LAM_FINE  (II,LI) = PAR(2)
        LAMP_LAM_FINE       (II,LI) = PAR(3)
        LAM_FINE            (II,LI) = PAR(4)
        LAM_ALPHA_LAMP_FINE (II,LI) = PAR(5)
        ALPHA_LAM_FINE      (II,LI) = PAR(6)
        ALPHA_ALPHA_FINE    (II,LI) = PAR(7)
        CAP_M_FINE          (II,LI) = PAR(8)
        M_FINE              (II,LI) = PAR(9)
        RHO_FINE            (II,LI) = PAR(10)
        RHOF_FINE           (II,LI) = PAR(11)
        B_FINE              (II,LI) = PAR(12)
        
        RHO_RHOF_FINE       (II,LI) = PAR(13)
        RHOF_M_FINE         (II,LI) = PAR(14)
        INV_RHOF_FINE       (II,LI) = PAR(15)
        INV_M_FINE          (II,LI) = PAR(16)
        B_M_FINE            (II,LI) = PAR(17)
      END DO
    END DO

  END IF

END SUBROUTINE PREPARE_FINE_PAR
