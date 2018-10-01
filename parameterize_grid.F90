!***********************************************************************
! SUBROUTINE PARAMETERIZES GRID
!***********************************************************************
SUBROUTINE PARAMETERIZE_GRID

  USE NRTYPE   , ONLY: WP, SP
  USE MOD_MODEL, ONLY: XBMIN,        H , H2 ,      MX ,                                   &
                       MAXDEPTH,       ZBMIN, AUXIL_W, AUXIL_QZ, DT1
  USE MOD_INTEGRATION     , ONLY: PTS, PTS2,                                              &
                                  MI_FINE, LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE,   &
                                  LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, ALPHA_ALPHA_FINE,  &
                                  CAP_M_FINE,                                             &   
                                  M_FINE, RHO_FINE, RHOF_FINE, B_FINE,                    &
                                  RHO_RHOF_FINE, RHOF_M_FINE, INV_RHOF_FINE, INV_M_FINE,  &
                                  B_M_FINE

  USE MOD_PARAMETERIZATION, ONLY: LSTART, LSTOP,                                          &
                                  CELL, N_PLANES,                                         &
                                  JMH , N_CELLS
  USE MOD_EFFMATPAR       , ONLY: MUXZ, PI_X, PI_Z, XXP, ZZP,                             &
                                  INT2DZX, INT2DZX_1, INT2DXZ, INT2DXZ_1,                 &
                                  INT2D_QX_ZX, INT2D_QZ_XZ, INT3DZX, INT3DXZ

  USE MOD_INTERFACES      , ONLY: IS_IN_1D_MEDIUM,                                        &
                                  PREPARE_FINE_PAR,                                       &
                                  INT3D, GET_FD_CELL_INDEX
!----------------------------------------------------------------------

  IMPLICIT NONE

  LOGICAL  :: FIRST1D
  INTEGER  :: I, L, J, II, JM
  REAL(WP) :: XSTART,         XA,   ZA,                                                   &
              R1, R2, THE11, THE12, THE21, THE22, POFRJ, POFRJ2P1,                        &
              MU, L2M
  REAL(WP) :: PIX, PIZ, LAMXZ, XX_P, ZZ_P, A_INV,                                         &
              DENOM_U, DENOM_W, DENOM_QX, DENOM_QZ
  REAL(WP) :: MUXZ_1D,                                                                    &
              SIG_XX_1_1D, SIG_XX_2_1D, SIG_XX_3_1D,                                      &
              SIG_ZZ_1_1D, SIG_ZZ_2_1D, SIG_ZZ_3_1D,                                      &
              PRES_1_1D, PRES_2_1D, PRES_3_1D,                                            &
              REL1_U_1D, REL1_W_1D, REL2_U_1D, REL2_W_1D, REL3_U_1D, REL3_W_1D,           &
              REL1_QX_1D, REL1_QZ_1D, REL2_QX_1D, REL2_QZ_1D, REL3_QX_1D, REL3_QZ_1D,     &
              AUXIL_W_1D, AUXIL_QZ_1D
               
!----------------------------------------------------------------------
  XSTART = XBMIN - H2
  LSTOP  = ABS((ZBMIN)/H) + 1

  DO L = LSTART, LSTOP

    WRITE(*,'(A21,I6,A6,I6)') 'GRID, PLANE: ',L,' FROM ',N_PLANES
    
   

    ZA =  - REAL(L-1,WP)*H
    FIRST1D = .TRUE.

   

      IF ( ZA < (MAXDEPTH-H2 ) ) THEN

        CALL   PREPARE_FINE_PAR   ( 1, L, H, 0.)
        

        II = PTS
        
                                                                    
        CELL(1)         = MUXZ      (II-PTS2, PTS2, MI_FINE            )
                                                                    
        PIX             = PI_X      (II     , PTS , LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE)
                                                                    
        PIZ             = PI_Z      (II     , PTS , LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE)
                                                                    
        LAMXZ           = INT3D     (II     , PTS , LAMP_LAM_FINE) * MUXZ      (II     , PTS , LAM_FINE)
                                                                    
        XX_P            = XXP       (II     , PTS ,LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, LAMP_LAM_FINE, LAM_FINE, ALPHA_ALPHA_FINE )
                                                                    
        ZZ_P            = ZZP       (II     , PTS ,LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, LAMP_LAM_FINE, LAM_FINE, ALPHA_ALPHA_FINE )
                                                                    
        A_INV           = 1._WP/( INT3D     (II     , PTS , CAP_M_FINE) - INT3D     (II     , PTS , ALPHA_LAM_FINE) * INT3D     (II     , PTS , ALPHA_LAM_FINE) * MUXZ      (II     , PTS , LAM_FINE) )
        
                                                                    
        CELL(2) = PIX    + A_INV * XX_P * XX_P
                                                                    
        CELL(3) = LAMXZ  + A_INV * XX_P * ZZ_P
                                                                    
        CELL(4) =        - A_INV * XX_P
                                                                    
        CELL(5) = LAMXZ  + A_INV * ZZ_P * XX_P
                                                                    
        CELL(6) = PIZ    + A_INV * ZZ_P * ZZ_P
                                                                    
        CELL(7) =        - A_INV * ZZ_P
                                                                    
        CELL(8) =          A_INV * XX_P
                                                                    
        CELL(9) =          A_INV * ZZ_P
                                                                    
        CELL(10) =       - A_INV      
  
  DENOM_U  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) - INT2DZX(II-PTS2, PTS , RHOF_FINE, M_FINE)
  DENOM_W  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) - INT2DXZ(II     , PTS2, RHOF_FINE, M_FINE)
  
                                                                    
        CELL(11)  =  INT2DZX_1(II-PTS2, PTS , RHOF_FINE)/DENOM_U
                                                                    
        CELL(12)  =  INT2DXZ_1(II     , PTS2, RHOF_FINE)/DENOM_W
                                                                    
        CELL(13)  =  INT2DZX(II-PTS2, PTS , B_FINE, M_FINE)/DENOM_U
                                                                    
        CELL(14)  =  INT2DXZ(II     , PTS2, B_FINE, M_FINE)/DENOM_W
                                                                    
        CELL(15)  =  INT2DZX_1(II-PTS2, PTS , M_FINE)/DENOM_U     
                                                                    
        CELL(16)  =  INT2DXZ_1(II     , PTS2, M_FINE)/DENOM_W  
                                                                    
        CELL(17)  =  INT3DZX(II-PTS2, PTS , RHOF_FINE) * INT2D_QX_ZX(II-PTS2, PTS , M_FINE, RHOF_FINE)/DENOM_U
                                                                    
        CELL(18)  =  INT3DXZ(II     , PTS2, RHOF_FINE) * INT2D_QZ_XZ(II     , PTS2, M_FINE, RHOF_FINE)/DENOM_W
                                                                    
        CELL(19)  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) * INT2DZX_1(II-PTS2, PTS , M_FINE)/DENOM_U  
                                                                    
        CELL(20)  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) * INT2DXZ_1(II     , PTS2, M_FINE)/DENOM_W
                                                                    
        CELL(21)  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) * INT2DZX(II-PTS2, PTS , B_FINE , M_FINE)/DENOM_U 
                                                                    
        CELL(22)  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) * INT2DXZ(II     , PTS2, B_FINE, M_FINE)/DENOM_W
                                                                    
        CELL(23)  = AUXIL_W
                                                                    
        CELL(24)  = AUXIL_QZ
        
        JM = GET_FD_CELL_INDEX()
        JMH  (:,L-1) = JM

      ELSE  

        CALL   PREPARE_FINE_PAR   (MX , L, H, 0.) 
                                                  

          DO I = 1, MX
            XA = XSTART + REAL(I-1,WP)*H
            II = I*PTS

            IF (IS_IN_1D_MEDIUM(XA,ZA)) THEN
        !------------------------------------------------1D-IH BEDROCK
              IF (FIRST1D) THEN

                                                                    
              MUXZ_1D         = MUXZ      (II-PTS2, PTS2, MI_FINE            )
                                                                    
              PIX             = PI_X      (II     , PTS , LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE)
                                                                    
              PIZ             = PI_Z      (II     , PTS , LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE)
                                                                    
              LAMXZ           = INT3D     (II     , PTS , LAMP_LAM_FINE) * MUXZ      (II     , PTS , LAM_FINE)
                                                                    
              XX_P            = XXP       (II     , PTS ,LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, LAMP_LAM_FINE, LAM_FINE, ALPHA_ALPHA_FINE )
                                                                    
              ZZ_P            = ZZP       (II     , PTS ,LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, LAMP_LAM_FINE, LAM_FINE, ALPHA_ALPHA_FINE )
                                                                    
              A_INV           = 1._WP/( INT3D     (II     , PTS , CAP_M_FINE) - INT3D     (II     , PTS , ALPHA_LAM_FINE) * INT3D     (II     , PTS , ALPHA_LAM_FINE) * MUXZ      (II     , PTS , LAM_FINE) )
        
                                                                    
              SIG_XX_1_1D = PIX    + A_INV * XX_P * XX_P
                                                                    
              SIG_XX_2_1D = LAMXZ  + A_INV * XX_P * ZZ_P
                                                                    
              SIG_XX_3_1D =        - A_INV * XX_P
                                                                    
              SIG_ZZ_1_1D = LAMXZ  + A_INV * ZZ_P * XX_P
                                                                    
              SIG_ZZ_2_1D = PIZ    + A_INV * ZZ_P * ZZ_P
                                                                    
              SIG_ZZ_3_1D =        - A_INV * ZZ_P
                                                                    
              PRES_1_1D =          A_INV * XX_P
                                                                    
              PRES_2_1D =          A_INV * ZZ_P
                                                                    
              PRES_3_1D =       - A_INV
              
  DENOM_U  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) - INT2DZX(II-PTS2, PTS , RHOF_FINE, M_FINE)
  DENOM_W  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) - INT2DXZ(II     , PTS2, RHOF_FINE, M_FINE)
  
                                                                    
        REL1_U_1D   =  INT2DZX_1(II-PTS2, PTS , RHOF_FINE)/DENOM_U
                                                                    
        REL1_W_1D   =  INT2DXZ_1(II     , PTS2, RHOF_FINE)/DENOM_W
                                                                    
        REL2_U_1D   =  INT2DZX(II-PTS2, PTS , B_FINE, M_FINE)/DENOM_U
                                                                    
        REL2_W_1D   =  INT2DXZ(II     , PTS2, B_FINE, M_FINE)/DENOM_W
                                                                    
        REL3_U_1D   =  INT2DZX_1(II-PTS2, PTS , M_FINE)/DENOM_U
                                                                    
        REL3_W_1D   =  INT2DXZ_1(II     , PTS2, M_FINE)/DENOM_W
                                                                    
        REL1_QX_1D  =  INT3DZX(II-PTS2, PTS , RHOF_FINE) * INT2D_QX_ZX(II-PTS2, PTS , M_FINE, RHOF_FINE)/DENOM_U 
                                                                    
        REL1_QZ_1D  =  INT3DXZ(II     , PTS2, RHOF_FINE) * INT2D_QZ_XZ(II     , PTS2, M_FINE, RHOF_FINE)/DENOM_W
                                                                    
        REL2_QX_1D  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) * INT2DZX_1(II-PTS2, PTS , M_FINE)/DENOM_U 
                                                                    
        REL2_QZ_1D  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) * INT2DXZ_1(II     , PTS2, M_FINE)/DENOM_W
                                                                    
        REL3_QX_1D  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) * INT2DZX(II-PTS2, PTS , B_FINE , M_FINE)/DENOM_U 
                                                                    
        REL3_QZ_1D  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) * INT2DXZ(II     , PTS2, B_FINE, M_FINE)/DENOM_W
                                                                    
        AUXIL_W_1D  = AUXIL_W
                                                                    
        AUXIL_QZ_1D = AUXIL_QZ
                
              FIRST1D = .FALSE.

              END IF
              
              CELL(1)   = MUXZ_1D
              
              CELL(2)   = SIG_XX_1_1D
              
              CELL(3)   = SIG_XX_2_1D
              
              CELL(4)   = SIG_XX_3_1D
              
              CELL(5)   = SIG_ZZ_1_1D
              
              CELL(6)   = SIG_ZZ_2_1D
              
              CELL(7)   = SIG_ZZ_3_1D
              
              CELL(8)   = PRES_1_1D
              
              CELL(9)   = PRES_2_1D
              
              CELL(10)  = PRES_3_1D
              
              CELL(11)  = REL1_U_1D
              
              CELL(12)  = REL1_W_1D
              
              CELL(13)  = REL2_U_1D
              
              CELL(14)  = REL2_W_1D
              
              CELL(15)  = REL3_U_1D
              
              CELL(16)  = REL3_W_1D
              
              CELL(17)  = REL1_QX_1D
              
              CELL(18)  = REL1_QZ_1D
              
              CELL(19)  = REL2_QX_1D
              
              CELL(20)  = REL2_QZ_1D
              
              CELL(21)  = REL3_QX_1D
              
              CELL(22)  = REL3_QZ_1D
              
              CELL(23)  = AUXIL_W_1D
              
              CELL(24)  = AUXIL_QZ_1D

              JM = GET_FD_CELL_INDEX()
              JMH  (I,L-1) = JM

            ELSE
        !----------------------------------------------2D-IH STRUCTURE

                                                                    
        CELL(1)         = MUXZ      (II-PTS2, PTS2, MI_FINE            )
                                                                    
        PIX             = PI_X      (II     , PTS , LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE)
                                                                    
        PIZ             = PI_Z      (II     , PTS , LAMP_LAMP_LAM_FINE, LAMP_LAM_FINE, LAM_FINE)
                                                                    
        LAMXZ           = INT3D     (II     , PTS , LAMP_LAM_FINE) * MUXZ      (II     , PTS , LAM_FINE)
                                                                    
        XX_P            = XXP       (II     , PTS ,LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, LAMP_LAM_FINE, LAM_FINE, ALPHA_ALPHA_FINE )
                                                                    
        ZZ_P            = ZZP       (II     , PTS ,LAM_ALPHA_LAMP_FINE, ALPHA_LAM_FINE, LAMP_LAM_FINE, LAM_FINE, ALPHA_ALPHA_FINE )
                                                                    
        A_INV           = 1._WP/( INT3D     (II     , PTS , CAP_M_FINE) - INT3D     (II     , PTS , ALPHA_LAM_FINE) * INT3D     (II     , PTS , ALPHA_LAM_FINE) * MUXZ      (II     , PTS , LAM_FINE) )
        
                                                                   
        CELL(2) = PIX    + A_INV * XX_P * XX_P
                                                                    
        CELL(3) = LAMXZ  + A_INV * XX_P * ZZ_P
                                                                    
        CELL(4) =        - A_INV * XX_P
                                                                    
        CELL(5) = LAMXZ  + A_INV * ZZ_P * XX_P
                                                                    
        CELL(6) = PIZ    + A_INV * ZZ_P * ZZ_P
                                                                    
        CELL(7) =        - A_INV * ZZ_P
                                                                    
        CELL(8) =          A_INV * XX_P
                                                                    
        CELL(9) =          A_INV * ZZ_P
                                                                    
        CELL(10) =       - A_INV
        
  DENOM_U  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) - INT2DZX(II-PTS2, PTS , RHOF_FINE, M_FINE)
  DENOM_W  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) - INT2DXZ(II     , PTS2, RHOF_FINE, M_FINE)
  
                                                                   
        CELL(11)  =  INT2DZX_1(II-PTS2, PTS , RHOF_FINE)/DENOM_U
                                                                    
        CELL(12)  =  INT2DXZ_1(II     , PTS2, RHOF_FINE)/DENOM_W
                                                                    
        CELL(13)  =  INT2DZX(II-PTS2, PTS , B_FINE, M_FINE)/DENOM_U
                                                                    
        CELL(14)  =  INT2DXZ(II     , PTS2, B_FINE, M_FINE)/DENOM_W
                                                                    
        CELL(15)  =  INT2DZX_1(II-PTS2, PTS , M_FINE)/DENOM_U     
                                                                    
        CELL(16)  =  INT2DXZ_1(II     , PTS2, M_FINE)/DENOM_W  
                                                                    
        CELL(17)  =  INT3DZX(II-PTS2, PTS , RHOF_FINE) * INT2D_QX_ZX(II-PTS2, PTS , M_FINE, RHOF_FINE)/DENOM_U 
                                                                    
        CELL(18)  =  INT3DXZ(II     , PTS2, RHOF_FINE) * INT2D_QZ_XZ(II     , PTS2, M_FINE, RHOF_FINE)/DENOM_W 
                                                                    
        CELL(19)  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) * INT2DZX_1(II-PTS2, PTS , M_FINE)/DENOM_U  
                                                                    
        CELL(20)  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) * INT2DXZ_1(II     , PTS2, M_FINE)/DENOM_W  
                                                                    
        CELL(21)  =  INT2DZX(II-PTS2, PTS , RHO_FINE, RHOF_FINE) * INT2DZX(II-PTS2, PTS , B_FINE , M_FINE)/DENOM_U 
                                                                    
        CELL(22)  =  INT2DXZ(II     , PTS2, RHO_FINE, RHOF_FINE) * INT2DXZ(II     , PTS2, B_FINE , M_FINE)/DENOM_W
                                                                    
        CELL(23)  = AUXIL_W
                                                                    
        CELL(24)  = AUXIL_QZ

              JM = GET_FD_CELL_INDEX()
              JMH  (I,L-1) = JM

            END IF

          END DO

      END IF

    WRITE(*,'(A20,I8)') 'TYPES OF MAT. CELLS: ',N_CELLS
  END DO

END SUBROUTINE PARAMETERIZE_GRID
