!-----------------------------------------------------------------------
!
! This module linearly interpolate NI input grids.
! Input grids are characterized by number of points
! in X direction NPX.
! Steps in X direction can variate.
!
! Output is spatial grid with step H10.
! Whole output region have to be inside the input mesh.
!
!-----------------------------------------------------------------------

SUBROUTINE MODEL_LIN

  USE NRTYPE   , ONLY: WP, SP
  USE MOD_MODEL, ONLY: NPX,      NX10,       H, H10, NX10_G,           &
                       XIMIN, XIMAX,               ITF1, NI,           &
                       XIMIN_G, XIMAX_G
  USE MOD_FILES, ONLY: FSED

  !----------------------------------------------------------- variables

  IMPLICIT NONE

  REAL(SP), ALLOCATABLE :: ITF1_R(:), BUF_S (:), BUF(:)
  REAL(WP), ALLOCATABLE :: FILE_CONTENT(:,:)

  INTEGER :: P, NP, I, EX, LI, IMIN, IMAX, I_OVERLAP
  REAL(WP) :: XD, ZD, X1, X2, Z1, Z2, XA, XMIN, XMAX

  !---------------------------------------------------- subroutines body
  

  ALLOCATE( ITF1( NX10, NI ), BUF (NX10) )  
                                            
    ALLOCATE ( ITF1_R (NX10_G) )  
                                  
    WRITE(*,'(A)')'Linear model of sediments/bedrock interface...'

    DO LI = 1, NI

    !----------------- read input file
      OPEN( 10, FILE=FSED(LI), STATUS='OLD')

      NP=0

    !--------------- count number of points in mesh
      DO
        READ(10,FMT=*,IOSTAT=EX)XD,ZD
        IF (EX/=0 ) EXIT
        NP = NP+1
      END DO

      REWIND(10)

    !--------------- test if correct number of input planes was set
      IF (NPX/=NP) THEN
        WRITE(*,*)
        WRITE(*,'(A,A)')'NUMBER OF POINTS IN FILE ',FSED(LI)
        WRITE(*,'(A)')'DIFFER FROM NUMBER CALCULATED FROM NUMBERS OF &
                      &INPUT PLANES:'
        WRITE(*,'(A,I8,A,I8)')'POINTS IN FILE: ',NP,'NPX = ',NPX
        STOP
      END IF

      ALLOCATE( FILE_CONTENT(NP,2) )

      DO I = 1, NP
        READ(10,*) FILE_CONTENT(I,:)
      END DO

    !--------------- test if area that have to be aproximated lie inside
    !--------------- input mesh
      XMIN = MINVAL(FILE_CONTENT(:,1))
      XMAX = MAXVAL(FILE_CONTENT(:,1))

      IF ( (XMIN>XIMIN_G).OR. &
           (XMAX<XIMAX_G)     ) THEN
        WRITE(*,*)' '
        WRITE(*,'(A)')'AREA TO BE APROXIMATED DOES NOT LIE &
                      &INSIDE KNOWN INTERFACE BOUNDARIES.'
        WRITE(*,'(A,F23.10,A,F23.10,A)')'KNOWN INTERFACE:        BOX &
                &[ ',XMIN ,' ]'
        WRITE(*,'(A,F23.10,A,F23.10,A)')'                            &
                &[ ',XMAX ,' ]'
        WRITE(*,'(A,F23.10,A,F23.10,A)')'AREA TO BE APROXIMATED: BOX &
                &[ ',XIMIN_G,' ]'
        WRITE(*,'(A,F23.10,A,F23.10,A)')'                            &
                &[ ',XIMAX_G,' ]'
        STOP
      END IF

      CLOSE(10)

 !============================================== aproximation of mesh

      P=1 
      DO I = 1, NX10_G                                 
        
        XA = XIMIN_G + DBLE(I-1)*H10                  
        
        DO
          X1 = FILE_CONTENT(P  , 1)
          X2 = FILE_CONTENT(P+1, 1)
          IF ((X1<=XA).AND.(XA<X2)) EXIT
          P = P + 1
        END DO
        Z1 = FILE_CONTENT(P  , 2)
        Z2 = FILE_CONTENT(P+1, 2)

        ITF1_R( I ) = ( (Z2-Z1)*XA + (Z1*X2-Z2*X1) ) / ( X2-X1 )  
                                                                  

      END DO

      ITF1 (:,LI) = ITF1_R (:) 
      
      DEALLOCATE( FILE_CONTENT )

      WRITE(*,'(A,I2,A)')'Interface #',LI,'OK'

    END DO

END SUBROUTINE MODEL_LIN
