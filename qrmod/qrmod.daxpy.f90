!
!   y + alpha x  --->  x
!
!	n      number of elements in vectors x and y
!	da     alpha - scalar 
!   dx     x
!   incx   stride for vector x
!   dy     y
!   incy   stride for vector y


SUBROUTINE daxpy(n, da, dx, incx, dy, incy)
!################################################################################
INTEGER(kind=int32), intent(in) ::               n
REAL(kind=real64), intent(in) ::                 da
REAL(kind=real64), intent(in) ::                 dx(*)
INTEGER(kind=int32), intent(in) ::               incx
REAL(kind=real64), intent(inout) ::              dy(*)
INTEGER(kind=int32), intent(in) ::               incy
!################################################################################
INTEGER(kind=int32) ::                           i
INTEGER(kind=int32) ::                           ix
INTEGER(kind=int32) ::                           iy
INTEGER(kind=int32) ::                           m
!################################################################################

IF(n <= 0)THEN
  RETURN
END IF

IF(da == 0.0d0)THEN
  RETURN
END IF

IF( incx /= 1 .or. incy /= 1 )THEN
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
		dy(iy) = dy(iy) + da * dx(ix)
		ix = ix + incx
		iy = iy + incy
	end do
else
	m = mod(n, 4)
	dy(1:m) = dy(1:m) + da * dx(1:m)
	DO i = m+1, n, 4
		dy(i  ) = dy(i  ) + da * dx(i  )
		dy(i+1) = dy(i+1) + da * dx(i+1)
		dy(i+2) = dy(i+2) + da * dx(i+2)
		dy(i+3) = dy(i+3) + da * dx(i+3)
	END DO
END IF

RETURN

END SUBROUTINE daxpy

























