SUBROUTINE dqrank (a, lda, m, n, tol, kr, jpvt, qraux, work)
!################################################################################
IMPLICIT NONE
!################################################################################
REAL(kind=real64) ::             a(lda,n)
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             tol
INTEGER(kind=int32) ::           kr
INTEGER(kind=int32) ::           jpvt(n)
REAL(kind=real64) ::             qraux(n)
REAL(kind=real64) ::             work(n)
!################################################################################
INTEGER(kind=int32) ::           j
INTEGER(kind=int32) ::           job
INTEGER(kind=int32) ::           k
!################################################################################
jpvt(1:n) = 0
job = 1
CALL dqrdc ( a, lda, m, n, qraux, jpvt, work, job )
kr = 0
k = min ( m, n )
DO j = 1, k
  IF(abs( a(j,j) ) <= tol * abs ( a(1,1) ))THEN
    RETURN
  END IF
  kr = j
END DO
RETURN
!################################################################################
END SUBROUTINE dqrank 