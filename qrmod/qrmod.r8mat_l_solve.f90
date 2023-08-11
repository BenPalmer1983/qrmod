SUBROUTINE r8mat_l_solve (n, a, b, x)
!################################################################################
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             a(n,n)
REAL(kind=real64) ::             b(n)
REAL(kind=real64) ::             x(n)
!################################################################################
INTEGER(kind=int32) ::           i
!################################################################################
! Solve L * x = b.
do i=1, n 
    x(i) = ( b(i) - dot_product( a(i,1:i-1), x(1:i-1) ) ) / a(i,i)
end do
RETURN
END SUBROUTINE r8mat_l_solve 