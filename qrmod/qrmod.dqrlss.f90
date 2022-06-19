SUBROUTINE dqrlss(a, lda, m, n, kr, b, x, r, jpvt, qraux)
!################################################################################
IMPLICIT NONE
!################################################################################
REAL(kind=real64) ::             a(lda,n)
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
INTEGER(kind=int32) ::           kr
REAL(kind=real64) ::             b(m)
REAL(kind=real64) ::             x(n)
REAL(kind=real64) ::             r(m)
INTEGER(kind=int32) ::           jpvt(n)
REAL(kind=real64) ::             qraux(n)
!################################################################################
INTEGER(kind=int32) ::           info
INTEGER(kind=int32) ::           j
INTEGER(kind=int32) ::           job
INTEGER(kind=int32) ::           k
REAL(kind=real64) ::             t
!################################################################################
IF ( kr /= 0 ) THEN
  job = 110
  call dqrsl ( a, lda, m, kr, qraux, b, r, r, x, r, r, job, info )
END IF
jpvt(1:n) = - jpvt(1:n)
x(kr+1:n) = 0.0D+00
do j = 1, n
  if ( jpvt(j) <= 0 ) then
    k = -jpvt(j)
    jpvt(j) = k
    do while ( k /= j )
      t = x(j)
      x(j) = x(k)
      x(k) = t
      jpvt(k) = -jpvt(k)
      k = jpvt(k)
    end do
  end if
end do
RETURN
!################################################################################
END SUBROUTINE dqrlss