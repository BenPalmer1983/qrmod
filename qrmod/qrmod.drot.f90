SUBROUTINE drot(n, x, incx, y, incy, c, s)
!################################################################################
IMPLICIT NONE
!################################################################################
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             x(*)
INTEGER(kind=int32) ::           incx
REAL(kind=real64) ::             y(*)
INTEGER(kind=int32) ::           incy
REAL(kind=real64) ::             c
REAL(kind=real64) ::             s
!################################################################################
INTEGER(kind=int32) ::           i
INTEGER(kind=int32) ::           ix
INTEGER(kind=int32) ::           iy
REAL(kind=real64) ::             stemp
!################################################################################
if ( n <= 0 ) then

else if ( incx == 1 .and. incy == 1 ) then
  do i = 1, n
    stemp = c * x(i) + s * y(i)
    y(i) = c * y(i) - s * x(i)
    x(i) = stemp
  end do
else
  if ( 0 <= incx ) then
    ix = 1
  else
    ix = ( - n + 1 ) * incx + 1
  end if
  if ( 0 <= incy ) then
    iy = 1
  else
    iy = ( - n + 1 ) * incy + 1
  end if
  do i = 1, n
    stemp = c * x(ix) + s * y(iy)
    y(iy) = c * y(iy) - s * x(ix)
    x(ix) = stemp
    ix = ix + incx
    iy = iy + incy
  end do
end if
RETURN
END SUBROUTINE drot