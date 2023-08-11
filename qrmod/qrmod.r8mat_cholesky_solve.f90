SUBROUTINE r8mat_cholesky_solve ( n, l, b, x )
!################################################################################
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             l(n,n)
REAL(kind=real64) ::             b(n)
REAL(kind=real64) ::             x(n)
!################################################################################
!  Solve L * y = b.
call r8mat_l_solve ( n, l, b, x )
!  Solve L' * x = y.
call r8mat_lt_solve ( n, l, x, x )
RETURN
END SUBROUTINE r8mat_cholesky_solve