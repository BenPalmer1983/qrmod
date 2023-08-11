! Solve using QR Decomposition
! Q - orthonormal matrix        (Q^T Q = I)
! R - upper triangular matrix


SUBROUTINE qr_solve(a, b, x)
!################################################################################
REAL(kind=real64), intent(in) ::                 a(:,:)
REAL(kind=real64), intent(in) ::                 b(:)
REAL(kind=real64), intent(inout) ::              x(:)
!################################################################################
INTEGER(kind=int32) ::                           m
INTEGER(kind=int32) ::                           n
INTEGER(kind=int32) ::                           ind
INTEGER(kind=int32) ::                           itask
INTEGER(kind=int32) ::                           jpvt(SIZE(a, 1))
INTEGER(kind=int32) ::                           kr
INTEGER(kind=int32) ::                           lda
REAL(kind=real64) ::                             qraux(SIZE(a, 1))
REAL(kind=real64) ::                             r(SIZE(a, 1))
REAL(kind=real64) ::                             tol
REAL(kind=real64) ::                             work(SIZE(a, 1))
REAL(kind=real64) ::                             a_qr(1:SIZE(a, 1),1:SIZE(a, 1))
!################################################################################

! matrix sizes
m = SIZE(a, 1)
n = SIZE(a, 1)
a_qr = a
lda = m
tol = epsilon(tol) / maxval(abs(a_qr))
itask = 1
call dqrls(a_qr, lda, m, n, tol, kr, b, x, r, jpvt, qraux, work, itask, ind)
RETURN
!################################################################################
END SUBROUTINE qr_solve

