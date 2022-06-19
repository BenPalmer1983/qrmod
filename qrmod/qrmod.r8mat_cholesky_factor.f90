

SUBROUTINE r8mat_cholesky_factor (n, a, c, flag)
!################################################################################
IMPLICIT NONE
!################################################################################
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             a(n,n)
REAL(kind=real64) ::             c(n,n)
INTEGER(kind=int32) ::           flag
!################################################################################
INTEGER(kind=int32) ::           i
INTEGER(kind=int32) ::           j
REAL(kind=real64) ::             sum2
!################################################################################
flag = 0

c(1:n,1:n) = a(1:n,1:n)

do j = 1, n
  c(1:j-1,j) = 0.0D+00
  do i = j, n
    sum2 = c(j,i) - dot_product ( c(j,1:j-1), c(i,1:j-1) )
    if ( i == j ) then
      if ( sum2 <= 0.0D+00 ) then
        flag = 1
        return
      else
        c(i,j) = sqrt ( sum2 )
      end if
    else
      if ( c(j,j) /= 0.0D+00 ) then
        c(i,j) = sum2 / c(j,j)
      else
        c(i,j) = 0.0D+00
      end if
    end if
  end do
end do
RETURN
!################################################################################
END SUBROUTINE r8mat_cholesky_factor



