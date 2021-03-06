MODULE NRUTIL
	USE NRTYPE
	IMPLICIT NONE
	INTEGER(I4B), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
	INTEGER(I4B), PARAMETER :: NPAR_GEOP=4,NPAR2_GEOP=2
	INTEGER(I4B), PARAMETER :: NPAR_CUMSUM=16
	INTEGER(I4B), PARAMETER :: NPAR_CUMPROD=8
	INTEGER(I4B), PARAMETER :: NPAR_POLY=8
	INTEGER(I4B), PARAMETER :: NPAR_POLYTERM=8
	INTERFACE ARRAY_COPY
		MODULE PROCEDURE ARRAY_COPY_R, ARRAY_COPY_D, ARRAY_COPY_I
	END INTERFACE
	INTERFACE SWAP
		MODULE PROCEDURE SWAP_I,SWAP_R,SWAP_RV,SWAP_C, &
			SWAP_CV,SWAP_CM,SWAP_Z,SWAP_ZV,SWAP_ZM, &
			MASKED_SWAP_RS,MASKED_SWAP_RV,MASKED_SWAP_RM, SWAP_DP
	END INTERFACE
	INTERFACE REALLOCATE
		MODULE PROCEDURE REALLOCATE_RV,REALLOCATE_RM,&
			REALLOCATE_IV,REALLOCATE_IM,REALLOCATE_HV
	END INTERFACE
	INTERFACE IMAXLOC
		MODULE PROCEDURE IMAXLOC_R,IMAXLOC_I
	END INTERFACE
	INTERFACE ASSERT
		MODULE PROCEDURE ASSERT1,ASSERT2,ASSERT3,ASSERT4,ASSERT_V
	END INTERFACE
	INTERFACE ASSERT_EQ
		MODULE PROCEDURE ASSERT_EQ2,ASSERT_EQ3,ASSERT_EQ4,ASSERT_EQN
	END INTERFACE
	INTERFACE ARTH
		MODULE PROCEDURE ARTH_R, ARTH_D, ARTH_I
	END INTERFACE
	INTERFACE GEOP
		MODULE PROCEDURE GEOP_R, GEOP_D, GEOP_I, GEOP_C, GEOP_DV
	END INTERFACE
	INTERFACE CUMSUM
		MODULE PROCEDURE CUMSUM_R,CUMSUM_I
	END INTERFACE
	INTERFACE POLY
		MODULE PROCEDURE POLY_RR,POLY_RRV,POLY_DD,POLY_DDV,&
			POLY_RC,POLY_CC,POLY_MSK_RRV,POLY_MSK_DDV
	END INTERFACE
	INTERFACE POLY_TERM
		MODULE PROCEDURE POLY_TERM_RR,POLY_TERM_CC
	END INTERFACE
	INTERFACE OUTERPROD
		MODULE PROCEDURE OUTERPROD_R,OUTERPROD_D
	END INTERFACE
	INTERFACE OUTERDIFF
		MODULE PROCEDURE OUTERDIFF_R,OUTERDIFF_D,OUTERDIFF_I
	END INTERFACE
	INTERFACE SCATTER_ADD
		MODULE PROCEDURE SCATTER_ADD_R,SCATTER_ADD_D
	END INTERFACE
	INTERFACE SCATTER_MAX
		MODULE PROCEDURE SCATTER_MAX_R,SCATTER_MAX_D
	END INTERFACE
	INTERFACE DIAGADD
		MODULE PROCEDURE DIAGADD_RV,DIAGADD_R
	END INTERFACE
	INTERFACE DIAGMULT
		MODULE PROCEDURE DIAGMULT_RV,DIAGMULT_R
	END INTERFACE
	INTERFACE GET_DIAG
		MODULE PROCEDURE GET_DIAG_RV, GET_DIAG_DV
	END INTERFACE
	INTERFACE PUT_DIAG
		MODULE PROCEDURE PUT_DIAG_RV, PUT_DIAG_R
	END INTERFACE
CONTAINS
!BL
	SUBROUTINE ARRAY_COPY_R(SRC,DEST,N_COPIED,N_NOT_COPIED)
	REAL(SP), DIMENSION(:), INTENT(IN) :: SRC
	REAL(SP), DIMENSION(:), INTENT(OUT) :: DEST
	INTEGER(I4B), INTENT(OUT) :: N_COPIED, N_NOT_COPIED
	N_COPIED=MIN(SIZE(SRC),SIZE(DEST))
	N_NOT_COPIED=SIZE(SRC)-N_COPIED
	DEST(1:N_COPIED)=SRC(1:N_COPIED)
	END SUBROUTINE ARRAY_COPY_R
!BL
	SUBROUTINE ARRAY_COPY_D(SRC,DEST,N_COPIED,N_NOT_COPIED)
	REAL(DP), DIMENSION(:), INTENT(IN) :: SRC
	REAL(DP), DIMENSION(:), INTENT(OUT) :: DEST
	INTEGER(I4B), INTENT(OUT) :: N_COPIED, N_NOT_COPIED
	N_COPIED=MIN(SIZE(SRC),SIZE(DEST))
	N_NOT_COPIED=SIZE(SRC)-N_COPIED
	DEST(1:N_COPIED)=SRC(1:N_COPIED)
	END SUBROUTINE ARRAY_COPY_D
!BL
	SUBROUTINE ARRAY_COPY_I(SRC,DEST,N_COPIED,N_NOT_COPIED)
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: SRC
	INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: DEST
	INTEGER(I4B), INTENT(OUT) :: N_COPIED, N_NOT_COPIED
	N_COPIED=MIN(SIZE(SRC),SIZE(DEST))
	N_NOT_COPIED=SIZE(SRC)-N_COPIED
	DEST(1:N_COPIED)=SRC(1:N_COPIED)
	END SUBROUTINE ARRAY_COPY_I
!BL
!BL
	SUBROUTINE SWAP_I(A,B)
	INTEGER(I4B), INTENT(INOUT) :: A,B
	INTEGER(I4B) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_I
!BL
	SUBROUTINE SWAP_R(A,B)
	REAL(SP), INTENT(INOUT) :: A,B
	REAL(SP) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_R
!BL
	SUBROUTINE SWAP_DP(A,B)
	REAL(DP), INTENT(INOUT) :: A,B
	REAL(DP) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_DP
!BL
	SUBROUTINE SWAP_RV(A,B)
	REAL(SP), DIMENSION(:), INTENT(INOUT) :: A,B
	REAL(SP), DIMENSION(SIZE(A)) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_RV
!BL
	SUBROUTINE SWAP_C(A,B)
	COMPLEX(SPC), INTENT(INOUT) :: A,B
	COMPLEX(SPC) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_C
!BL
	SUBROUTINE SWAP_CV(A,B)
	COMPLEX(SPC), DIMENSION(:), INTENT(INOUT) :: A,B
	COMPLEX(SPC), DIMENSION(SIZE(A)) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_CV
!BL
	SUBROUTINE SWAP_CM(A,B)
	COMPLEX(SPC), DIMENSION(:,:), INTENT(INOUT) :: A,B
	COMPLEX(SPC), DIMENSION(SIZE(A,1),SIZE(A,2)) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_CM
!BL
	SUBROUTINE SWAP_Z(A,B)
	COMPLEX(DPC), INTENT(INOUT) :: A,B
	COMPLEX(DPC) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_Z
!BL
	SUBROUTINE SWAP_ZV(A,B)
	COMPLEX(DPC), DIMENSION(:), INTENT(INOUT) :: A,B
	COMPLEX(DPC), DIMENSION(SIZE(A)) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_ZV
!BL
	SUBROUTINE SWAP_ZM(A,B)
	COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: A,B
	COMPLEX(DPC), DIMENSION(SIZE(A,1),SIZE(A,2)) :: DUM
	DUM=A
	A=B
	B=DUM
	END SUBROUTINE SWAP_ZM
!BL
	SUBROUTINE MASKED_SWAP_RS(A,B,MASK)
	REAL(SP), INTENT(INOUT) :: A,B
	LOGICAL(LGT), INTENT(IN) :: MASK
	REAL(SP) :: SWP
	IF (MASK) THEN
		SWP=A
		A=B
		B=SWP
	END IF
	END SUBROUTINE MASKED_SWAP_RS
!BL
	SUBROUTINE MASKED_SWAP_RV(A,B,MASK)
	REAL(SP), DIMENSION(:), INTENT(INOUT) :: A,B
	LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: MASK
	REAL(SP), DIMENSION(SIZE(A)) :: SWP
	WHERE (MASK)
		SWP=A
		A=B
		B=SWP
	END WHERE
	END SUBROUTINE MASKED_SWAP_RV
!BL
	SUBROUTINE MASKED_SWAP_RM(A,B,MASK)
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: A,B
	LOGICAL(LGT), DIMENSION(:,:), INTENT(IN) :: MASK
	REAL(SP), DIMENSION(SIZE(A,1),SIZE(A,2)) :: SWP
	WHERE (MASK)
		SWP=A
		A=B
		B=SWP
	END WHERE
	END SUBROUTINE MASKED_SWAP_RM
!BL
!BL
	FUNCTION REALLOCATE_RV(P,N)
	REAL(SP), DIMENSION(:), POINTER :: P, REALLOCATE_RV
	INTEGER(I4B), INTENT(IN) :: N
	INTEGER(I4B) :: NOLD,IERR
	ALLOCATE(REALLOCATE_RV(N),STAT=IERR)
	IF (IERR /= 0) CALL &
		NRERROR('REALLOCATE_RV: PROBLEM IN ATTEMPT TO ALLOCATE MEMORY')
	IF (.NOT. ASSOCIATED(P)) RETURN
	NOLD=SIZE(P)
	REALLOCATE_RV(1:MIN(NOLD,N))=P(1:MIN(NOLD,N))
	DEALLOCATE(P)
	END FUNCTION REALLOCATE_RV
!BL
	FUNCTION REALLOCATE_IV(P,N)
	INTEGER(I4B), DIMENSION(:), POINTER :: P, REALLOCATE_IV
	INTEGER(I4B), INTENT(IN) :: N
	INTEGER(I4B) :: NOLD,IERR
	ALLOCATE(REALLOCATE_IV(N),STAT=IERR)
	IF (IERR /= 0) CALL &
		NRERROR('REALLOCATE_IV: PROBLEM IN ATTEMPT TO ALLOCATE MEMORY')
	IF (.NOT. ASSOCIATED(P)) RETURN
	NOLD=SIZE(P)
	REALLOCATE_IV(1:MIN(NOLD,N))=P(1:MIN(NOLD,N))
	DEALLOCATE(P)
	END FUNCTION REALLOCATE_IV
!BL
	FUNCTION REALLOCATE_HV(P,N)
	CHARACTER(1), DIMENSION(:), POINTER :: P, REALLOCATE_HV
	INTEGER(I4B), INTENT(IN) :: N
	INTEGER(I4B) :: NOLD,IERR
	ALLOCATE(REALLOCATE_HV(N),STAT=IERR)
	IF (IERR /= 0) CALL &
		NRERROR('REALLOCATE_HV: PROBLEM IN ATTEMPT TO ALLOCATE MEMORY')
	IF (.NOT. ASSOCIATED(P)) RETURN
	NOLD=SIZE(P)
	REALLOCATE_HV(1:MIN(NOLD,N))=P(1:MIN(NOLD,N))
	DEALLOCATE(P)
	END FUNCTION REALLOCATE_HV
!BL
	FUNCTION REALLOCATE_RM(P,N,M)
	REAL(SP), DIMENSION(:,:), POINTER :: P, REALLOCATE_RM
	INTEGER(I4B), INTENT(IN) :: N,M
	INTEGER(I4B) :: NOLD,MOLD,IERR
	ALLOCATE(REALLOCATE_RM(N,M),STAT=IERR)
	IF (IERR /= 0) CALL &
		NRERROR('REALLOCATE_RM: PROBLEM IN ATTEMPT TO ALLOCATE MEMORY')
	IF (.NOT. ASSOCIATED(P)) RETURN
	NOLD=SIZE(P,1)
	MOLD=SIZE(P,2)
	REALLOCATE_RM(1:MIN(NOLD,N),1:MIN(MOLD,M))=&
		P(1:MIN(NOLD,N),1:MIN(MOLD,M))
	DEALLOCATE(P)
	END FUNCTION REALLOCATE_RM
!BL
	FUNCTION REALLOCATE_IM(P,N,M)
	INTEGER(I4B), DIMENSION(:,:), POINTER :: P, REALLOCATE_IM
	INTEGER(I4B), INTENT(IN) :: N,M
	INTEGER(I4B) :: NOLD,MOLD,IERR
	ALLOCATE(REALLOCATE_IM(N,M),STAT=IERR)
	IF (IERR /= 0) CALL &
		NRERROR('REALLOCATE_IM: PROBLEM IN ATTEMPT TO ALLOCATE MEMORY')
	IF (.NOT. ASSOCIATED(P)) RETURN
	NOLD=SIZE(P,1)
	MOLD=SIZE(P,2)
	REALLOCATE_IM(1:MIN(NOLD,N),1:MIN(MOLD,M))=&
		P(1:MIN(NOLD,N),1:MIN(MOLD,M))
	DEALLOCATE(P)
	END FUNCTION REALLOCATE_IM
!BL
	FUNCTION IFIRSTLOC(MASK)
	LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: MASK
	INTEGER(I4B) :: IFIRSTLOC
	INTEGER(I4B), DIMENSION(1) :: LOC
	LOC=MAXLOC(MERGE(1,0,MASK))
	IFIRSTLOC=LOC(1)
	IF (.NOT. MASK(IFIRSTLOC)) IFIRSTLOC=SIZE(MASK)+1
	END FUNCTION IFIRSTLOC
!BL
	FUNCTION IMAXLOC_R(ARR)
	REAL(SP), DIMENSION(:), INTENT(IN) :: ARR
	INTEGER(I4B) :: IMAXLOC_R
	INTEGER(I4B), DIMENSION(1) :: IMAX
	IMAX=MAXLOC(ARR(:))
	IMAXLOC_R=IMAX(1)
	END FUNCTION IMAXLOC_R
!BL
	FUNCTION IMAXLOC_I(IARR)
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: IARR
	INTEGER(I4B), DIMENSION(1) :: IMAX
	INTEGER(I4B) :: IMAXLOC_I
	IMAX=MAXLOC(IARR(:))
	IMAXLOC_I=IMAX(1)
	END FUNCTION IMAXLOC_I
!BL
	FUNCTION IMINLOC(ARR)
	REAL(DP), DIMENSION(:), INTENT(IN) :: ARR
	INTEGER(I4B), DIMENSION(1) :: IMIN
	INTEGER(I4B) :: IMINLOC
	IMIN=MINLOC(ARR(:))
	IMINLOC=IMIN(1)
	END FUNCTION IMINLOC
!BL
	SUBROUTINE ASSERT1(N1,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	LOGICAL, INTENT(IN) :: N1
	IF (.NOT. N1) THEN
		WRITE (*,*) 'NRERROR: AN ASSERTION FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT1'
	END IF
	END SUBROUTINE ASSERT1
!BL
	SUBROUTINE ASSERT2(N1,N2,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	LOGICAL, INTENT(IN) :: N1,N2
	IF (.NOT. (N1 .AND. N2)) THEN
		WRITE (*,*) 'NRERROR: AN ASSERTION FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT2'
	END IF
	END SUBROUTINE ASSERT2
!BL
	SUBROUTINE ASSERT3(N1,N2,N3,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	LOGICAL, INTENT(IN) :: N1,N2,N3
	IF (.NOT. (N1 .AND. N2 .AND. N3)) THEN
		WRITE (*,*) 'NRERROR: AN ASSERTION FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT3'
	END IF
	END SUBROUTINE ASSERT3
!BL
	SUBROUTINE ASSERT4(N1,N2,N3,N4,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	LOGICAL, INTENT(IN) :: N1,N2,N3,N4
	IF (.NOT. (N1 .AND. N2 .AND. N3 .AND. N4)) THEN
		WRITE (*,*) 'NRERROR: AN ASSERTION FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT4'
	END IF
	END SUBROUTINE ASSERT4
!BL
	SUBROUTINE ASSERT_V(N,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	LOGICAL, DIMENSION(:), INTENT(IN) :: N
	IF (.NOT. ALL(N)) THEN
		WRITE (*,*) 'NRERROR: AN ASSERTION FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT_V'
	END IF
	END SUBROUTINE ASSERT_V
!BL
	FUNCTION ASSERT_EQ2(N1,N2,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	INTEGER, INTENT(IN) :: N1,N2
	INTEGER :: ASSERT_EQ2
	IF (N1 == N2) THEN
		ASSERT_EQ2=N1
	ELSE
		WRITE (*,*) 'NRERROR: AN ASSERT_EQ FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT_EQ2'
	END IF
	END FUNCTION ASSERT_EQ2
!BL
	FUNCTION ASSERT_EQ3(N1,N2,N3,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	INTEGER, INTENT(IN) :: N1,N2,N3
	INTEGER :: ASSERT_EQ3
	IF (N1 == N2 .AND. N2 == N3) THEN
		ASSERT_EQ3=N1
	ELSE
		WRITE (*,*) 'NRERROR: AN ASSERT_EQ FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT_EQ3'
	END IF
	END FUNCTION ASSERT_EQ3
!BL
	FUNCTION ASSERT_EQ4(N1,N2,N3,N4,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	INTEGER, INTENT(IN) :: N1,N2,N3,N4
	INTEGER :: ASSERT_EQ4
	IF (N1 == N2 .AND. N2 == N3 .AND. N3 == N4) THEN
		ASSERT_EQ4=N1
	ELSE
		WRITE (*,*) 'NRERROR: AN ASSERT_EQ FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT_EQ4'
	END IF
	END FUNCTION ASSERT_EQ4
!BL
	FUNCTION ASSERT_EQN(NN,STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	INTEGER, DIMENSION(:), INTENT(IN) :: NN
	INTEGER :: ASSERT_EQN
	IF (ALL(NN(2:) == NN(1))) THEN
		ASSERT_EQN=NN(1)
	ELSE
		WRITE (*,*) 'NRERROR: AN ASSERT_EQ FAILED WITH THIS TAG:', &
			STRING
		STOP 'PROGRAM TERMINATED BY ASSERT_EQN'
	END IF
	END FUNCTION ASSERT_EQN
!BL
	SUBROUTINE NRERROR(STRING)
	CHARACTER(LEN=*), INTENT(IN) :: STRING
	WRITE (*,*) 'NRERROR: ',STRING
	STOP 'PROGRAM TERMINATED BY NRERROR'
	END SUBROUTINE NRERROR
!BL
	FUNCTION ARTH_R(FIRST,INCREMENT,N)
	REAL(SP), INTENT(IN) :: FIRST,INCREMENT
	INTEGER(I4B), INTENT(IN) :: N
	REAL(SP), DIMENSION(N) :: ARTH_R
	INTEGER(I4B) :: K,K2
	REAL(SP) :: TEMP
	IF (N > 0) ARTH_R(1)=FIRST
	IF (N <= NPAR_ARTH) THEN
		DO K=2,N
			ARTH_R(K)=ARTH_R(K-1)+INCREMENT
		END DO
	ELSE
		DO K=2,NPAR2_ARTH
			ARTH_R(K)=ARTH_R(K-1)+INCREMENT
		END DO
		TEMP=INCREMENT*NPAR2_ARTH
		K=NPAR2_ARTH
		DO
			IF (K >= N) EXIT
			K2=K+K
			ARTH_R(K+1:MIN(K2,N))=TEMP+ARTH_R(1:MIN(K,N-K))
			TEMP=TEMP+TEMP
			K=K2
		END DO
	END IF
	END FUNCTION ARTH_R
!BL
	FUNCTION ARTH_D(FIRST,INCREMENT,N)
	REAL(DP), INTENT(IN) :: FIRST,INCREMENT
	INTEGER(I4B), INTENT(IN) :: N
	REAL(DP), DIMENSION(N) :: ARTH_D
	INTEGER(I4B) :: K,K2
	REAL(DP) :: TEMP
	IF (N > 0) ARTH_D(1)=FIRST
	IF (N <= NPAR_ARTH) THEN
		DO K=2,N
			ARTH_D(K)=ARTH_D(K-1)+INCREMENT
		END DO
	ELSE
		DO K=2,NPAR2_ARTH
			ARTH_D(K)=ARTH_D(K-1)+INCREMENT
		END DO
		TEMP=INCREMENT*NPAR2_ARTH
		K=NPAR2_ARTH
		DO
			IF (K >= N) EXIT
			K2=K+K
			ARTH_D(K+1:MIN(K2,N))=TEMP+ARTH_D(1:MIN(K,N-K))
			TEMP=TEMP+TEMP
			K=K2
		END DO
	END IF
	END FUNCTION ARTH_D
!BL
	FUNCTION ARTH_I(FIRST,INCREMENT,N)
	INTEGER(I4B), INTENT(IN) :: FIRST,INCREMENT,N
	INTEGER(I4B), DIMENSION(N) :: ARTH_I
	INTEGER(I4B) :: K,K2,TEMP
	IF (N > 0) ARTH_I(1)=FIRST
	IF (N <= NPAR_ARTH) THEN
		DO K=2,N
			ARTH_I(K)=ARTH_I(K-1)+INCREMENT
		END DO
	ELSE
		DO K=2,NPAR2_ARTH
			ARTH_I(K)=ARTH_I(K-1)+INCREMENT
		END DO
		TEMP=INCREMENT*NPAR2_ARTH
		K=NPAR2_ARTH
		DO
			IF (K >= N) EXIT
			K2=K+K
			ARTH_I(K+1:MIN(K2,N))=TEMP+ARTH_I(1:MIN(K,N-K))
			TEMP=TEMP+TEMP
			K=K2
		END DO
	END IF
	END FUNCTION ARTH_I
!BL
!BL
	FUNCTION GEOP_R(FIRST,FACTOR,N)
	REAL(SP), INTENT(IN) :: FIRST,FACTOR
	INTEGER(I4B), INTENT(IN) :: N
	REAL(SP), DIMENSION(N) :: GEOP_R
	INTEGER(I4B) :: K,K2
	REAL(SP) :: TEMP
	IF (N > 0) GEOP_R(1)=FIRST
	IF (N <= NPAR_GEOP) THEN
		DO K=2,N
			GEOP_R(K)=GEOP_R(K-1)*FACTOR
		END DO
	ELSE
		DO K=2,NPAR2_GEOP
			GEOP_R(K)=GEOP_R(K-1)*FACTOR
		END DO
		TEMP=FACTOR**NPAR2_GEOP
		K=NPAR2_GEOP
		DO
			IF (K >= N) EXIT
			K2=K+K
			GEOP_R(K+1:MIN(K2,N))=TEMP*GEOP_R(1:MIN(K,N-K))
			TEMP=TEMP*TEMP
			K=K2
		END DO
	END IF
	END FUNCTION GEOP_R
!BL
	FUNCTION GEOP_D(FIRST,FACTOR,N)
	REAL(DP), INTENT(IN) :: FIRST,FACTOR
	INTEGER(I4B), INTENT(IN) :: N
	REAL(DP), DIMENSION(N) :: GEOP_D
	INTEGER(I4B) :: K,K2
	REAL(DP) :: TEMP
	IF (N > 0) GEOP_D(1)=FIRST
	IF (N <= NPAR_GEOP) THEN
		DO K=2,N
			GEOP_D(K)=GEOP_D(K-1)*FACTOR
		END DO
	ELSE
		DO K=2,NPAR2_GEOP
			GEOP_D(K)=GEOP_D(K-1)*FACTOR
		END DO
		TEMP=FACTOR**NPAR2_GEOP
		K=NPAR2_GEOP
		DO
			IF (K >= N) EXIT
			K2=K+K
			GEOP_D(K+1:MIN(K2,N))=TEMP*GEOP_D(1:MIN(K,N-K))
			TEMP=TEMP*TEMP
			K=K2
		END DO
	END IF
	END FUNCTION GEOP_D
!BL
	FUNCTION GEOP_I(FIRST,FACTOR,N)
	INTEGER(I4B), INTENT(IN) :: FIRST,FACTOR,N
	INTEGER(I4B), DIMENSION(N) :: GEOP_I
	INTEGER(I4B) :: K,K2,TEMP
	IF (N > 0) GEOP_I(1)=FIRST
	IF (N <= NPAR_GEOP) THEN
		DO K=2,N
			GEOP_I(K)=GEOP_I(K-1)*FACTOR
		END DO
	ELSE
		DO K=2,NPAR2_GEOP
			GEOP_I(K)=GEOP_I(K-1)*FACTOR
		END DO
		TEMP=FACTOR**NPAR2_GEOP
		K=NPAR2_GEOP
		DO
			IF (K >= N) EXIT
			K2=K+K
			GEOP_I(K+1:MIN(K2,N))=TEMP*GEOP_I(1:MIN(K,N-K))
			TEMP=TEMP*TEMP
			K=K2
		END DO
	END IF
	END FUNCTION GEOP_I
!BL
	FUNCTION GEOP_C(FIRST,FACTOR,N)
	COMPLEX(SP), INTENT(IN) :: FIRST,FACTOR
	INTEGER(I4B), INTENT(IN) :: N
	COMPLEX(SP), DIMENSION(N) :: GEOP_C
	INTEGER(I4B) :: K,K2
	COMPLEX(SP) :: TEMP
	IF (N > 0) GEOP_C(1)=FIRST
	IF (N <= NPAR_GEOP) THEN
		DO K=2,N
			GEOP_C(K)=GEOP_C(K-1)*FACTOR
		END DO
	ELSE
		DO K=2,NPAR2_GEOP
			GEOP_C(K)=GEOP_C(K-1)*FACTOR
		END DO
		TEMP=FACTOR**NPAR2_GEOP
		K=NPAR2_GEOP
		DO
			IF (K >= N) EXIT
			K2=K+K
			GEOP_C(K+1:MIN(K2,N))=TEMP*GEOP_C(1:MIN(K,N-K))
			TEMP=TEMP*TEMP
			K=K2
		END DO
	END IF
	END FUNCTION GEOP_C
!BL
	FUNCTION GEOP_DV(FIRST,FACTOR,N)
	REAL(DP), DIMENSION(:), INTENT(IN) :: FIRST,FACTOR
	INTEGER(I4B), INTENT(IN) :: N
	REAL(DP), DIMENSION(SIZE(FIRST),N) :: GEOP_DV
	INTEGER(I4B) :: K,K2
	REAL(DP), DIMENSION(SIZE(FIRST)) :: TEMP
	IF (N > 0) GEOP_DV(:,1)=FIRST(:)
	IF (N <= NPAR_GEOP) THEN
		DO K=2,N
			GEOP_DV(:,K)=GEOP_DV(:,K-1)*FACTOR(:)
		END DO
	ELSE
		DO K=2,NPAR2_GEOP
			GEOP_DV(:,K)=GEOP_DV(:,K-1)*FACTOR(:)
		END DO
		TEMP=FACTOR**NPAR2_GEOP
		K=NPAR2_GEOP
		DO
			IF (K >= N) EXIT
			K2=K+K
			GEOP_DV(:,K+1:MIN(K2,N))=GEOP_DV(:,1:MIN(K,N-K))*&
				SPREAD(TEMP,2,SIZE(GEOP_DV(:,1:MIN(K,N-K)),2))
			TEMP=TEMP*TEMP
			K=K2
		END DO
	END IF
	END FUNCTION GEOP_DV
!BL
!BL
	RECURSIVE FUNCTION CUMSUM_R(ARR,SEED) RESULT(ANS)
	REAL(SP), DIMENSION(:), INTENT(IN) :: ARR
	REAL(SP), OPTIONAL, INTENT(IN) :: SEED
	REAL(SP), DIMENSION(SIZE(ARR)) :: ANS
	INTEGER(I4B) :: N,J
	REAL(SP) :: SD
	N=SIZE(ARR)
	IF (N == 0_I4B) RETURN
	SD=0.0_SP
	IF (PRESENT(SEED)) SD=SEED
	ANS(1)=ARR(1)+SD
	IF (N < NPAR_CUMSUM) THEN
		DO J=2,N
			ANS(J)=ANS(J-1)+ARR(J)
		END DO
	ELSE
		ANS(2:N:2)=CUMSUM_R(ARR(2:N:2)+ARR(1:N-1:2),SD)
		ANS(3:N:2)=ANS(2:N-1:2)+ARR(3:N:2)
	END IF
	END FUNCTION CUMSUM_R
!BL
	RECURSIVE FUNCTION CUMSUM_I(ARR,SEED) RESULT(ANS)
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: ARR
	INTEGER(I4B), OPTIONAL, INTENT(IN) :: SEED
	INTEGER(I4B), DIMENSION(SIZE(ARR)) :: ANS
	INTEGER(I4B) :: N,J,SD
	N=SIZE(ARR)
	IF (N == 0_I4B) RETURN
	SD=0_I4B
	IF (PRESENT(SEED)) SD=SEED
	ANS(1)=ARR(1)+SD
	IF (N < NPAR_CUMSUM) THEN
		DO J=2,N
			ANS(J)=ANS(J-1)+ARR(J)
		END DO
	ELSE
		ANS(2:N:2)=CUMSUM_I(ARR(2:N:2)+ARR(1:N-1:2),SD)
		ANS(3:N:2)=ANS(2:N-1:2)+ARR(3:N:2)
	END IF
	END FUNCTION CUMSUM_I
!BL
!BL
	RECURSIVE FUNCTION CUMPROD(ARR,SEED) RESULT(ANS)
	REAL(SP), DIMENSION(:), INTENT(IN) :: ARR
	REAL(SP), OPTIONAL, INTENT(IN) :: SEED
	REAL(SP), DIMENSION(SIZE(ARR)) :: ANS
	INTEGER(I4B) :: N,J
	REAL(SP) :: SD
	N=SIZE(ARR)
	IF (N == 0_I4B) RETURN
	SD=1.0_SP
	IF (PRESENT(SEED)) SD=SEED
	ANS(1)=ARR(1)*SD
	IF (N < NPAR_CUMPROD) THEN
		DO J=2,N
			ANS(J)=ANS(J-1)*ARR(J)
		END DO
	ELSE
		ANS(2:N:2)=CUMPROD(ARR(2:N:2)*ARR(1:N-1:2),SD)
		ANS(3:N:2)=ANS(2:N-1:2)*ARR(3:N:2)
	END IF
	END FUNCTION CUMPROD
!BL
!BL
	FUNCTION POLY_RR(X,COEFFS)
	REAL(SP), INTENT(IN) :: X
	REAL(SP), DIMENSION(:), INTENT(IN) :: COEFFS
	REAL(SP) :: POLY_RR
	REAL(SP) :: POW
	REAL(SP), DIMENSION(:), ALLOCATABLE :: VEC
	INTEGER(I4B) :: I,N,NN
	N=SIZE(COEFFS)
	IF (N <= 0) THEN
		POLY_RR=0.0_SP
	ELSE IF (N < NPAR_POLY) THEN
		POLY_RR=COEFFS(N)
		DO I=N-1,1,-1
			POLY_RR=X*POLY_RR+COEFFS(I)
		END DO
	ELSE
		ALLOCATE(VEC(N+1))
		POW=X
		VEC(1:N)=COEFFS
		DO
			VEC(N+1)=0.0_SP
			NN=ISHFT(N+1,-1)
			VEC(1:NN)=VEC(1:N:2)+POW*VEC(2:N+1:2)
			IF (NN == 1) EXIT
			POW=POW*POW
			N=NN
		END DO
		POLY_RR=VEC(1)
		DEALLOCATE(VEC)
	END IF
	END FUNCTION POLY_RR
!BL
	FUNCTION POLY_DD(X,COEFFS)
	REAL(DP), INTENT(IN) :: X
	REAL(DP), DIMENSION(:), INTENT(IN) :: COEFFS
	REAL(DP) :: POLY_DD
	REAL(DP) :: POW
	REAL(DP), DIMENSION(:), ALLOCATABLE :: VEC
	INTEGER(I4B) :: I,N,NN
	N=SIZE(COEFFS)
	IF (N <= 0) THEN
		POLY_DD=0.0_DP
	ELSE IF (N < NPAR_POLY) THEN
		POLY_DD=COEFFS(N)
		DO I=N-1,1,-1
			POLY_DD=X*POLY_DD+COEFFS(I)
		END DO
	ELSE
		ALLOCATE(VEC(N+1))
		POW=X
		VEC(1:N)=COEFFS
		DO
			VEC(N+1)=0.0_DP
			NN=ISHFT(N+1,-1)
			VEC(1:NN)=VEC(1:N:2)+POW*VEC(2:N+1:2)
			IF (NN == 1) EXIT
			POW=POW*POW
			N=NN
		END DO
		POLY_DD=VEC(1)
		DEALLOCATE(VEC)
	END IF
	END FUNCTION POLY_DD
!BL
	FUNCTION POLY_RC(X,COEFFS)
	COMPLEX(SPC), INTENT(IN) :: X
	REAL(SP), DIMENSION(:), INTENT(IN) :: COEFFS
	COMPLEX(SPC) :: POLY_RC
	COMPLEX(SPC) :: POW
	COMPLEX(SPC), DIMENSION(:), ALLOCATABLE :: VEC
	INTEGER(I4B) :: I,N,NN
	N=SIZE(COEFFS)
	IF (N <= 0) THEN
		POLY_RC=0.0_SP
	ELSE IF (N < NPAR_POLY) THEN
		POLY_RC=COEFFS(N)
		DO I=N-1,1,-1
			POLY_RC=X*POLY_RC+COEFFS(I)
		END DO
	ELSE
		ALLOCATE(VEC(N+1))
		POW=X
		VEC(1:N)=COEFFS
		DO
			VEC(N+1)=0.0_SP
			NN=ISHFT(N+1,-1)
			VEC(1:NN)=VEC(1:N:2)+POW*VEC(2:N+1:2)
			IF (NN == 1) EXIT
			POW=POW*POW
			N=NN
		END DO
		POLY_RC=VEC(1)
		DEALLOCATE(VEC)
	END IF
	END FUNCTION POLY_RC
!BL
	FUNCTION POLY_CC(X,COEFFS)
	COMPLEX(SPC), INTENT(IN) :: X
	COMPLEX(SPC), DIMENSION(:), INTENT(IN) :: COEFFS
	COMPLEX(SPC) :: POLY_CC
	COMPLEX(SPC) :: POW
	COMPLEX(SPC), DIMENSION(:), ALLOCATABLE :: VEC
	INTEGER(I4B) :: I,N,NN
	N=SIZE(COEFFS)
	IF (N <= 0) THEN
		POLY_CC=0.0_SP
	ELSE IF (N < NPAR_POLY) THEN
		POLY_CC=COEFFS(N)
		DO I=N-1,1,-1
			POLY_CC=X*POLY_CC+COEFFS(I)
		END DO
	ELSE
		ALLOCATE(VEC(N+1))
		POW=X
		VEC(1:N)=COEFFS
		DO
			VEC(N+1)=0.0_SP
			NN=ISHFT(N+1,-1)
			VEC(1:NN)=VEC(1:N:2)+POW*VEC(2:N+1:2)
			IF (NN == 1) EXIT
			POW=POW*POW
			N=NN
		END DO
		POLY_CC=VEC(1)
		DEALLOCATE(VEC)
	END IF
	END FUNCTION POLY_CC
!BL
	FUNCTION POLY_RRV(X,COEFFS)
	REAL(SP), DIMENSION(:), INTENT(IN) :: COEFFS,X
	REAL(SP), DIMENSION(SIZE(X)) :: POLY_RRV
	INTEGER(I4B) :: I,N,M
	M=SIZE(COEFFS)
	N=SIZE(X)
	IF (M <= 0) THEN
		POLY_RRV=0.0_SP
	ELSE IF (M < N .OR. M < NPAR_POLY) THEN
		POLY_RRV=COEFFS(M)
		DO I=M-1,1,-1
			POLY_RRV=X*POLY_RRV+COEFFS(I)
		END DO
	ELSE
		DO I=1,N
			POLY_RRV(I)=POLY_RR(X(I),COEFFS)
		END DO
	END IF
	END FUNCTION POLY_RRV
!BL
	FUNCTION POLY_DDV(X,COEFFS)
	REAL(DP), DIMENSION(:), INTENT(IN) :: COEFFS,X
	REAL(DP), DIMENSION(SIZE(X)) :: POLY_DDV
	INTEGER(I4B) :: I,N,M
	M=SIZE(COEFFS)
	N=SIZE(X)
	IF (M <= 0) THEN
		POLY_DDV=0.0_DP
	ELSE IF (M < N .OR. M < NPAR_POLY) THEN
		POLY_DDV=COEFFS(M)
		DO I=M-1,1,-1
			POLY_DDV=X*POLY_DDV+COEFFS(I)
		END DO
	ELSE
		DO I=1,N
			POLY_DDV(I)=POLY_DD(X(I),COEFFS)
		END DO
	END IF
	END FUNCTION POLY_DDV
!BL
	FUNCTION POLY_MSK_RRV(X,COEFFS,MASK)
	REAL(SP), DIMENSION(:), INTENT(IN) :: COEFFS,X
	LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: MASK
	REAL(SP), DIMENSION(SIZE(X)) :: POLY_MSK_RRV
	POLY_MSK_RRV=UNPACK(POLY_RRV(PACK(X,MASK),COEFFS),MASK,0.0_SP)
	END FUNCTION POLY_MSK_RRV
!BL
	FUNCTION POLY_MSK_DDV(X,COEFFS,MASK)
	REAL(DP), DIMENSION(:), INTENT(IN) :: COEFFS,X
	LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: MASK
	REAL(DP), DIMENSION(SIZE(X)) :: POLY_MSK_DDV
	POLY_MSK_DDV=UNPACK(POLY_DDV(PACK(X,MASK),COEFFS),MASK,0.0_DP)
	END FUNCTION POLY_MSK_DDV
!BL
!BL
	RECURSIVE FUNCTION POLY_TERM_RR(A,B) RESULT(U)
	REAL(SP), DIMENSION(:), INTENT(IN) :: A
	REAL(SP), INTENT(IN) :: B
	REAL(SP), DIMENSION(SIZE(A)) :: U
	INTEGER(I4B) :: N,J
	N=SIZE(A)
	IF (N <= 0) RETURN
	U(1)=A(1)
	IF (N < NPAR_POLYTERM) THEN
		DO J=2,N
			U(J)=A(J)+B*U(J-1)
		END DO
	ELSE
		U(2:N:2)=POLY_TERM_RR(A(2:N:2)+A(1:N-1:2)*B,B*B)
		U(3:N:2)=A(3:N:2)+B*U(2:N-1:2)
	END IF
	END FUNCTION POLY_TERM_RR
!BL
	RECURSIVE FUNCTION POLY_TERM_CC(A,B) RESULT(U)
	COMPLEX(SPC), DIMENSION(:), INTENT(IN) :: A
	COMPLEX(SPC), INTENT(IN) :: B
	COMPLEX(SPC), DIMENSION(SIZE(A)) :: U
	INTEGER(I4B) :: N,J
	N=SIZE(A)
	IF (N <= 0) RETURN
	U(1)=A(1)
	IF (N < NPAR_POLYTERM) THEN
		DO J=2,N
			U(J)=A(J)+B*U(J-1)
		END DO
	ELSE
		U(2:N:2)=POLY_TERM_CC(A(2:N:2)+A(1:N-1:2)*B,B*B)
		U(3:N:2)=A(3:N:2)+B*U(2:N-1:2)
	END IF
	END FUNCTION POLY_TERM_CC
!BL
!BL
	FUNCTION ZROOTS_UNITY(N,NN)
	INTEGER(I4B), INTENT(IN) :: N,NN
	COMPLEX(SPC), DIMENSION(NN) :: ZROOTS_UNITY
	INTEGER(I4B) :: K
	REAL(SP) :: THETA
	ZROOTS_UNITY(1)=1.0
	THETA=TWOPI/N
	K=1
	DO
		IF (K >= NN) EXIT
		ZROOTS_UNITY(K+1)=CMPLX(COS(K*THETA),SIN(K*THETA),SPC)
		ZROOTS_UNITY(K+2:MIN(2*K,NN))=ZROOTS_UNITY(K+1)*&
			ZROOTS_UNITY(2:MIN(K,NN-K))
		K=2*K
	END DO
	END FUNCTION ZROOTS_UNITY
!BL
	FUNCTION OUTERPROD_R(A,B)
	REAL(SP), DIMENSION(:), INTENT(IN) :: A,B
	REAL(SP), DIMENSION(SIZE(A),SIZE(B)) :: OUTERPROD_R
	OUTERPROD_R = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) * &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERPROD_R
!BL
	FUNCTION OUTERPROD_D(A,B)
	REAL(DP), DIMENSION(:), INTENT(IN) :: A,B
	REAL(DP), DIMENSION(SIZE(A),SIZE(B)) :: OUTERPROD_D
	OUTERPROD_D = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) * &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERPROD_D
!BL
	FUNCTION OUTERDIV(A,B)
	REAL(SP), DIMENSION(:), INTENT(IN) :: A,B
	REAL(SP), DIMENSION(SIZE(A),SIZE(B)) :: OUTERDIV
	OUTERDIV = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) / &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERDIV
!BL
	FUNCTION OUTERSUM(A,B)
	REAL(SP), DIMENSION(:), INTENT(IN) :: A,B
	REAL(SP), DIMENSION(SIZE(A),SIZE(B)) :: OUTERSUM
	OUTERSUM = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) + &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERSUM
!BL
	FUNCTION OUTERDIFF_R(A,B)
	REAL(SP), DIMENSION(:), INTENT(IN) :: A,B
	REAL(SP), DIMENSION(SIZE(A),SIZE(B)) :: OUTERDIFF_R
	OUTERDIFF_R = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) - &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERDIFF_R
!BL
	FUNCTION OUTERDIFF_D(A,B)
	REAL(DP), DIMENSION(:), INTENT(IN) :: A,B
	REAL(DP), DIMENSION(SIZE(A),SIZE(B)) :: OUTERDIFF_D
	OUTERDIFF_D = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) - &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERDIFF_D
!BL
	FUNCTION OUTERDIFF_I(A,B)
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: A,B
	INTEGER(I4B), DIMENSION(SIZE(A),SIZE(B)) :: OUTERDIFF_I
	OUTERDIFF_I = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) - &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERDIFF_I
!BL
	FUNCTION OUTERAND(A,B)
	LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: A,B
	LOGICAL(LGT), DIMENSION(SIZE(A),SIZE(B)) :: OUTERAND
	OUTERAND = SPREAD(A,DIM=2,NCOPIES=SIZE(B)) .AND. &
		SPREAD(B,DIM=1,NCOPIES=SIZE(A))
	END FUNCTION OUTERAND
!BL
	SUBROUTINE SCATTER_ADD_R(DEST,SOURCE,DEST_INDEX)
	REAL(SP), DIMENSION(:), INTENT(OUT) :: DEST
	REAL(SP), DIMENSION(:), INTENT(IN) :: SOURCE
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: DEST_INDEX
	INTEGER(I4B) :: M,N,J,I
	N=ASSERT_EQ2(SIZE(SOURCE),SIZE(DEST_INDEX),'SCATTER_ADD_R')
	M=SIZE(DEST)
	DO J=1,N
		I=DEST_INDEX(J)
		IF (I > 0 .AND. I <= M) DEST(I)=DEST(I)+SOURCE(J)
	END DO
	END SUBROUTINE SCATTER_ADD_R
	SUBROUTINE SCATTER_ADD_D(DEST,SOURCE,DEST_INDEX)
	REAL(DP), DIMENSION(:), INTENT(OUT) :: DEST
	REAL(DP), DIMENSION(:), INTENT(IN) :: SOURCE
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: DEST_INDEX
	INTEGER(I4B) :: M,N,J,I
	N=ASSERT_EQ2(SIZE(SOURCE),SIZE(DEST_INDEX),'SCATTER_ADD_D')
	M=SIZE(DEST)
	DO J=1,N
		I=DEST_INDEX(J)
		IF (I > 0 .AND. I <= M) DEST(I)=DEST(I)+SOURCE(J)
	END DO
	END SUBROUTINE SCATTER_ADD_D
	SUBROUTINE SCATTER_MAX_R(DEST,SOURCE,DEST_INDEX)
	REAL(SP), DIMENSION(:), INTENT(OUT) :: DEST
	REAL(SP), DIMENSION(:), INTENT(IN) :: SOURCE
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: DEST_INDEX
	INTEGER(I4B) :: M,N,J,I
	N=ASSERT_EQ2(SIZE(SOURCE),SIZE(DEST_INDEX),'SCATTER_MAX_R')
	M=SIZE(DEST)
	DO J=1,N
		I=DEST_INDEX(J)
		IF (I > 0 .AND. I <= M) DEST(I)=MAX(DEST(I),SOURCE(J))
	END DO
	END SUBROUTINE SCATTER_MAX_R
	SUBROUTINE SCATTER_MAX_D(DEST,SOURCE,DEST_INDEX)
	REAL(DP), DIMENSION(:), INTENT(OUT) :: DEST
	REAL(DP), DIMENSION(:), INTENT(IN) :: SOURCE
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: DEST_INDEX
	INTEGER(I4B) :: M,N,J,I
	N=ASSERT_EQ2(SIZE(SOURCE),SIZE(DEST_INDEX),'SCATTER_MAX_D')
	M=SIZE(DEST)
	DO J=1,N
		I=DEST_INDEX(J)
		IF (I > 0 .AND. I <= M) DEST(I)=MAX(DEST(I),SOURCE(J))
	END DO
	END SUBROUTINE SCATTER_MAX_D
!BL
	SUBROUTINE DIAGADD_RV(MAT,DIAG)
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: MAT
	REAL(SP), DIMENSION(:), INTENT(IN) :: DIAG
	INTEGER(I4B) :: J,N
	N = ASSERT_EQ2(SIZE(DIAG),MIN(SIZE(MAT,1),SIZE(MAT,2)),'DIAGADD_RV')
	DO J=1,N
		MAT(J,J)=MAT(J,J)+DIAG(J)
	END DO
	END SUBROUTINE DIAGADD_RV
!BL
	SUBROUTINE DIAGADD_R(MAT,DIAG)
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: MAT
	REAL(SP), INTENT(IN) :: DIAG
	INTEGER(I4B) :: J,N
	N = MIN(SIZE(MAT,1),SIZE(MAT,2))
	DO J=1,N
		MAT(J,J)=MAT(J,J)+DIAG
	END DO
	END SUBROUTINE DIAGADD_R
!BL
	SUBROUTINE DIAGMULT_RV(MAT,DIAG)
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: MAT
	REAL(SP), DIMENSION(:), INTENT(IN) :: DIAG
	INTEGER(I4B) :: J,N
	N = ASSERT_EQ2(SIZE(DIAG),MIN(SIZE(MAT,1),SIZE(MAT,2)),'DIAGMULT_RV')
	DO J=1,N
		MAT(J,J)=MAT(J,J)*DIAG(J)
	END DO
	END SUBROUTINE DIAGMULT_RV
!BL
	SUBROUTINE DIAGMULT_R(MAT,DIAG)
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: MAT
	REAL(SP), INTENT(IN) :: DIAG
	INTEGER(I4B) :: J,N
	N = MIN(SIZE(MAT,1),SIZE(MAT,2))
	DO J=1,N
		MAT(J,J)=MAT(J,J)*DIAG
	END DO
	END SUBROUTINE DIAGMULT_R
!BL
	FUNCTION GET_DIAG_RV(MAT)
	REAL(SP), DIMENSION(:,:), INTENT(IN) :: MAT
	REAL(SP), DIMENSION(SIZE(MAT,1)) :: GET_DIAG_RV
	INTEGER(I4B) :: J
	J=ASSERT_EQ2(SIZE(MAT,1),SIZE(MAT,2),'GET_DIAG_RV')
	DO J=1,SIZE(MAT,1)
		GET_DIAG_RV(J)=MAT(J,J)
	END DO
	END FUNCTION GET_DIAG_RV
!BL
	FUNCTION GET_DIAG_DV(MAT)
	REAL(DP), DIMENSION(:,:), INTENT(IN) :: MAT
	REAL(DP), DIMENSION(SIZE(MAT,1)) :: GET_DIAG_DV
	INTEGER(I4B) :: J
	J=ASSERT_EQ2(SIZE(MAT,1),SIZE(MAT,2),'GET_DIAG_DV')
	DO J=1,SIZE(MAT,1)
		GET_DIAG_DV(J)=MAT(J,J)
	END DO
	END FUNCTION GET_DIAG_DV
!BL
	SUBROUTINE PUT_DIAG_RV(DIAGV,MAT)
	REAL(SP), DIMENSION(:), INTENT(IN) :: DIAGV
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: MAT
	INTEGER(I4B) :: J,N
	N=ASSERT_EQ2(SIZE(DIAGV),MIN(SIZE(MAT,1),SIZE(MAT,2)),'PUT_DIAG_RV')
	DO J=1,N
		MAT(J,J)=DIAGV(J)
	END DO
	END SUBROUTINE PUT_DIAG_RV
!BL
	SUBROUTINE PUT_DIAG_R(SCAL,MAT)
	REAL(SP), INTENT(IN) :: SCAL
	REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: MAT
	INTEGER(I4B) :: J,N
	N = MIN(SIZE(MAT,1),SIZE(MAT,2))
	DO J=1,N
		MAT(J,J)=SCAL
	END DO
	END SUBROUTINE PUT_DIAG_R
!BL
	SUBROUTINE UNIT_MATRIX(MAT)
	REAL(SP), DIMENSION(:,:), INTENT(OUT) :: MAT
	INTEGER(I4B) :: I,N
	N=MIN(SIZE(MAT,1),SIZE(MAT,2))
	MAT(:,:)=0.0_SP
	DO I=1,N
		MAT(I,I)=1.0_SP
	END DO
	END SUBROUTINE UNIT_MATRIX
!BL
	FUNCTION UPPER_TRIANGLE(J,K,EXTRA)
	INTEGER(I4B), INTENT(IN) :: J,K
	INTEGER(I4B), OPTIONAL, INTENT(IN) :: EXTRA
	LOGICAL(LGT), DIMENSION(J,K) :: UPPER_TRIANGLE
	INTEGER(I4B) :: N
	N=0
	IF (PRESENT(EXTRA)) N=EXTRA
	UPPER_TRIANGLE=(OUTERDIFF(ARTH_I(1,1,J),ARTH_I(1,1,K)) < N)
	END FUNCTION UPPER_TRIANGLE
!BL
	FUNCTION LOWER_TRIANGLE(J,K,EXTRA)
	INTEGER(I4B), INTENT(IN) :: J,K
	INTEGER(I4B), OPTIONAL, INTENT(IN) :: EXTRA
	LOGICAL(LGT), DIMENSION(J,K) :: LOWER_TRIANGLE
	INTEGER(I4B) :: N
	N=0
	IF (PRESENT(EXTRA)) N=EXTRA
	LOWER_TRIANGLE=(OUTERDIFF(ARTH_I(1,1,J),ARTH_I(1,1,K)) > -N)
	END FUNCTION LOWER_TRIANGLE
!BL
	FUNCTION VABS(V)
	REAL(SP), DIMENSION(:), INTENT(IN) :: V
	REAL(SP) :: VABS
	VABS=SQRT(DOT_PRODUCT(V,V))
	END FUNCTION VABS
!BL
END MODULE NRUTIL
