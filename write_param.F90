SUBROUTINE WRITE_PARAM

  USE NRTYPE              , ONLY: SP
  USE MOD_CONST           , ONLY: N_E_PAR
  USE MOD_PARAMETERIZATION, ONLY: N_CELLS, JMH,       CELLS,           &
                                  N_CELLS_G, JMH_G,         CELLS_G
  USE MOD_MODEL           , ONLY: MX,                     MZ,          &
                                  MX_G
  USE MOD_FILES           , ONLY: JMH_FILE_NAME,                       &
                                  MO_FILE_NAME, JOB_NAME

!----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER :: JM1, L, TMP, NC, I, istat1, status

  INTEGER , ALLOCATABLE :: BUF_I (:,:)

  REAL(SP), ALLOCATABLE :: BUF(:,:), BUFY(:,:,:)
  CHARACTER(LEN=20) :: FMT
  CHARACTER (LEN = 11) :: STRING
  CHARACTER(LEN=8), DIMENSION(N_E_PAR) :: CELLS_NAMES = (/'MI',                                               &
                                                          'SIG_XX_1', 'SIG_XX_2', 'SIG_XX_3',                 &
                                                          'SIG_ZZ_1', 'SIG_ZZ_2', 'SIG_ZZ_3',                 &
                                                          'PRES_1', 'PRES_2','PRES_3',                        &
                                                          'REL1_U','REL1_W','REL2_U',                         &
                                                          'REL2_W','REL3_U','REL3_W',                         &
                                                          'REL1_QX','REL1_QZ','REL2_QX',                      &
                                                          'REL2_QZ','REL3_QX','REL3_QZ',                      &
                                                          'AUXIL_W', 'AUXIL_QZ'                               /)

!----------------------------------------------------------------------OK

  N_CELLS_G = N_CELLS

    OPEN ( 13, FILE   = JMH_FILE_NAME, FORM   = 'UNFORMATTED',         &
                                       STATUS ='NEW' )
    
    DO L = 0, MZ  
      WRITE ( 13 )  JMH   ( 1:MX_G, L)
    END DO

    CLOSE( 13 )

    OPEN ( 14, FILE = MO_FILE_NAME, FORM = 'UNFORMATTED', STATUS='NEW' )
    
    OPEN ( 41, FILE = TRIM(JOB_NAME)//'.dat', STATUS='NEW' )



    WRITE( 14 ) -N_CELLS_G
    WRITE( 41,*,IOSTAT=istat1 ) -N_CELLS_G


    WRITE( 14 ) ( CELLS  (JM1, 1) , CELLS  (JM1, 2) , CELLS  (JM1, 3) ,   &
                  CELLS  (JM1, 4) , CELLS  (JM1, 5) , CELLS  (JM1, 6) ,   &
                  CELLS  (JM1, 7) , CELLS  (JM1, 8) , CELLS  (JM1, 9) ,   &
                  CELLS  (JM1, 10), CELLS  (JM1, 11), CELLS  (JM1, 12),   &
                  CELLS  (JM1, 13), CELLS  (JM1, 14), CELLS  (JM1, 15),   &
                  CELLS  (JM1, 16), CELLS  (JM1, 17), CELLS  (JM1, 18),   &
                  CELLS  (JM1, 19), CELLS  (JM1, 20), CELLS  (JM1, 21),   &
                  CELLS  (JM1, 22), CELLS  (JM1, 23), CELLS  (JM1, 24),   &
                                                        JM1 = 1, N_CELLS  )
    
   WRITE( 41,1001,IOSTAT=istat1 ) ( CELLS_NAMES(I), I = 1, N_E_PAR)
   1001 FORMAT (24(2X,A13),/)   
    
   DO JM1 = 1, N_CELLS
     WRITE( 41,1000,IOSTAT=istat1 ) ( CELLS(JM1,I), I = 1, N_E_PAR)
     1000 FORMAT (32E15.7,/)  
    END DO

    CLOSE(14)
    CLOSE(41)

END SUBROUTINE WRITE_PARAM
