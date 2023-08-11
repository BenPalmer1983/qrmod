SUBROUTINE normal_solve(a, b, x)
!################################################################################
REAL(kind=real64) ::             a(:,:)
REAL(kind=real64) ::             b(:)
REAL(kind=real64) ::             x(:)
!################################################################################
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             ata(SIZE(a, 2),SIZE(a, 2))
REAL(kind=real64) ::             ata_c(SIZE(a, 2),SIZE(a, 2))
REAL(kind=real64) ::             atb(SIZE(a, 2))
INTEGER(kind=int32) ::           flag
!################################################################################
m = SIZE(a, 1)
n = SIZE(a, 2)
flag = 0
IF( m < n )THEN
  flag = 1
  RETURN
END IF
ata = matmul(transpose(a), a)
atb = matmul(transpose(a), b)
CALL r8mat_cholesky_factor(n, ata, ata_c, flag)
IF( flag /= 0 )THEN
  RETURN
END IF
CALL r8mat_cholesky_solve ( n, ata_c, atb, x )
RETURN
!################################################################################
END SUBROUTINE normal_solve




