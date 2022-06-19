SUBROUTINE svd_solve (a, b, x )
!################################################################################
IMPLICIT NONE
!################################################################################
REAL(kind=real64) ::             a(:,:)
REAL(kind=real64) ::             b(:)
REAL(kind=real64) ::             x(:)
!################################################################################
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             a_copy(SIZE(a, 1), SIZE(a, 2))
REAL(kind=real64) ::             e(max(SIZE(a, 1)+1, SIZE(a, 2)))
INTEGER(kind=int32) ::           i
INTEGER(kind=int32) ::           info
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           ldu
INTEGER(kind=int32) ::           ldv
INTEGER(kind=int32) ::           job
REAL(kind=real64) ::             sdiag(max(SIZE(a, 1)+1, SIZE(a, 2)))
REAL(kind=real64) ::             smax
REAL(kind=real64) ::             stol
REAL(kind=real64) ::             sub(SIZE(a, 2))
REAL(kind=real64) ::             u(SIZE(a, 1), SIZE(a, 1))
REAL(kind=real64) ::             ub(SIZE(a, 1))
REAL(kind=real64) ::             v(SIZE(a, 2), SIZE(a, 2))
REAL(kind=real64) ::             work(SIZE(a, 1))
!################################################################################
m = SIZE(a, 1)
n = SIZE(a, 2)
!  Get the SVD.
a_copy(1:m,1:n) = a(1:m,1:n)
lda = m
ldu = m
ldv = n
job = 11
CALL dsvdc ( a_copy, lda, m, n, sdiag, e, u, ldu, v, ldv, work, job, info )
if ( info /= 0 ) then
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SVD_SOLVE - Failure!'
  write ( *, '(a)' ) '  The SVD could not be calculated.'
  write ( *, '(a)' ) '  LINPACK routine DSVDC returned a nonzero'
  write ( *, '(a,i8)' ) '  value of the error flag, INFO = ', info
  stop
end if
ub(1:m) = matmul ( transpose ( u(1:m,1:m) ), b(1:m) )
sub(1:n) = 0.0D+00
!  For singular problems, there may be tiny but nonzero singular values
!  that should be ignored.  This is a reasonable attempt to avoid such 
!  problems, although in general, the user might wish to control the tolerance.
smax = maxval ( sdiag(1:n) )
if ( smax <= epsilon ( smax ) ) then
  smax = 1.0D+00
end if
stol = epsilon ( smax ) * smax
do i = 1, n
  if ( i <= m ) then
    if ( stol <= sdiag(i) ) then
      sub(i) = ub(i) / sdiag(i)
    end if
  end if
end do
x(1:n) = matmul ( v(1:n,1:n), sub(1:n) )
RETURN
!################################################################################
END SUBROUTINE svd_solve

