
!################################################################################
SUBROUTINE lu_solve(a, b, x)
!################################################################################
REAL(kind=real64), INTENT(IN) ::          a(:,:)
REAL(kind=real64), INTENT(IN) ::          b(:)
REAL(kind=real64), INTENT(INOUT) ::       x(:)
!################################################################################
INTEGER(kind=int32) ::                    i, j, n
INTEGER(kind=int32) ::                    p(1:SIZE(a,1), 1:SIZE(a,2))
REAL(kind=real64) ::                      aa(1:SIZE(a,1), 1:SIZE(a,2))
REAL(kind=real64) ::                      lower(1:SIZE(a,1), 1:SIZE(a,2))
REAL(kind=real64) ::                      upper(1:SIZE(a,1), 1:SIZE(a,2))
INTEGER(kind=int32) ::                    ipiv(1:SIZE(a,1))
REAL(kind=real64) ::                      bpiv(1:SIZE(a,1))
REAL(kind=real64) ::                      y(1:SIZE(a,1))
REAL(kind=real64) :: sum_term
!################################################################################
x(:) = 0.0d0    ! A x = b
y(:) = 0.0d0    ! L y = b,    U x = y
n = size(a,1)
do concurrent (j=1:n, i=1:n)
  aa(i,j) = a(i,j)
  upper(i,j) = 0.0d0
  p(i,j) = merge(1, 0, i.eq.j)
  lower(i,j) = merge(1d0,0d0,i.eq.j)
end do
call ludecomp(aa, ipiv)
DO i = 1,n
  lower(i, :i-1) = aa(i, :i-1)
  upper(i,i:   ) = aa(i,i:   )
END DO
p(ipiv,:) = p
bpiv = b
bpiv = MATMUL(p, b)
!# Solve Ly = b
DO i = 1, n
  sum_term = SUM(lower(i, 1:i-1) * y(1:i-1))
  y(i) = (bpiv(i) - sum_term) / lower(i, i)
END DO
!# Solve Ux=y
DO i = n, 1, -1
  sum_term = SUM(upper(i, i:n) * x(i:n))  
  x(i) = (y(i) - sum_term) / upper(i, i)
END DO
RETURN
!################################################################################
END SUBROUTINE lu_solve