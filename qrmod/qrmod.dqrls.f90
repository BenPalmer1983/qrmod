SUBROUTINE dqrls ( a, lda, m, n, tol, kr, b, x, r, jpvt, qraux, work, &
  itask, ind )
!################################################################################
REAL(kind=real64) ::             a(lda,n)
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             tol
INTEGER(kind=int32) ::           kr
REAL(kind=real64) ::             b(m)
REAL(kind=real64) ::             x(n)
REAL(kind=real64) ::             r(m)
INTEGER(kind=int32) ::           jpvt(n)
REAL(kind=real64) ::             qraux(n)
REAL(kind=real64) ::             work(n)
INTEGER(kind=int32) ::           itask
INTEGER(kind=int32) ::           ind
!################################################################################
IF(lda < m)THEN
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DQRLS - Fatal error!'
  write ( *, '(a)' ) '  LDA < M.'
  STOP
END IF
IF(n <= 0)THEN
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DQRLS - Fatal error!'
  write ( *, '(a)' ) '  N <= 0.'
  STOP
END IF
IF(itask < 1)THEN
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DQRLS - Fatal error!'
  write ( *, '(a)' ) '  ITASK < 1.'
  STOP
END IF
ind = 0
!  Factor the matrix.
IF(itask == 1)THEN
  CALL dqrank ( a, lda, m, n, tol, kr, jpvt, qraux, work )
END IF
!  Solve the least-squares problem.
CALL dqrlss ( a, lda, m, n, kr, b, x, r, jpvt, qraux )
RETURN
END SUBROUTINE dqrls