


FUNCTION ddot(n, dx, incx, dy, incy)
!################################################################################
IMPLICIT NONE
!################################################################################
REAL(kind=real64) ::             ddot
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             dx(*)
INTEGER(kind=int32) ::           incx
REAL(kind=real64) ::             dy(*)
INTEGER(kind=int32) ::           incy
!################################################################################
REAL(kind=real64) ::             dtemp
INTEGER(kind=int32) ::           i
INTEGER(kind=int32) ::           ix
INTEGER(kind=int32) ::           iy
INTEGER(kind=int32) ::           m
!################################################################################
ddot = 0.0D+00
dtemp = 0.0D+00
IF(n <= 0)THEN
  RETURN
END IF
!
!  Code for unequal increments or equal increments
!  not equal to 1.
!
IF ( incx /= 1 .OR. incy /= 1 ) THEN
  IF ( 0 <= incx ) THEN
    ix = 1
  ELSE
    ix = ( - n + 1 ) * incx + 1
  END IF
  IF ( 0 <= incy ) THEN
    iy = 1
  ELSE
    iy = ( - n + 1 ) * incy + 1
  END IF
  DO i = 1, n
    dtemp = dtemp + dx(ix) * dy(iy)
    ix = ix + incx
    iy = iy + incy
  END DO
!
!  Code for both increments equal to 1.
!
ELSE
  m = mod ( n, 5 )
  dtemp = SUM(dx(1:m) * dy(1:m))
  DO i = m+1, n, 5
    dtemp = dtemp + SUM(dx(i:i+4) * dy(i:i+4))
  END DO
END IF
ddot = dtemp
RETURN
!################################################################################
END FUNCTION ddot