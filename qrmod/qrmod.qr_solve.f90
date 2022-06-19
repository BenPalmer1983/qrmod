SUBROUTINE qr_solve( a, b, x )
!################################################################################
IMPLICIT NONE
!################################################################################
REAL(kind=real64) ::             a(:,:)
REAL(kind=real64) ::             b(:)
REAL(kind=real64) ::             x(:)
!################################################################################
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
INTEGER(kind=int32) ::           ind
INTEGER(kind=int32) ::           itask
INTEGER(kind=int32) ::           jpvt(SIZE(a, 1))
INTEGER(kind=int32) ::           kr
INTEGER(kind=int32) ::           lda
REAL(kind=real64) ::             qraux(SIZE(a, 1))
REAL(kind=real64) ::             r(SIZE(a, 1))
REAL(kind=real64) ::             tol
REAL(kind=real64) ::             work(SIZE(a, 1))
REAL(kind=real64) ::             a_qr(1:SIZE(a, 1),1:SIZE(a, 1))
!################################################################################
m = SIZE(a, 1)
n = SIZE(a, 1)
a_qr(1:m,1:n) = a(1:m,1:n)
lda = m
tol = epsilon ( tol ) / maxval ( abs ( a_qr(1:m,1:n) ) )
itask = 1
call dqrls (a_qr, lda, m, n, tol, kr, b, x, r, jpvt, qraux, work, &
itask, ind)
RETURN
!################################################################################
END SUBROUTINE qr_solve

