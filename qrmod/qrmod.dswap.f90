SUBROUTINE dswap(n, x, incx, y, incy)
!################################################################################
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             x(*)
INTEGER(kind=int32) ::           incx
REAL(kind=real64) ::             y(*)
INTEGER(kind=int32) ::           incy
!################################################################################
INTEGER(kind=int32) ::           i
INTEGER(kind=int32) ::           ix
INTEGER(kind=int32) ::           iy
INTEGER(kind=int32) ::           m
REAL(kind=real64) ::             temp
!################################################################################
if ( n <= 0 ) then

else if ( incx == 1 .and. incy == 1 ) then
  m = mod ( n, 3 )

  do i = 1, m
    temp = x(i)
    x(i) = y(i)
    y(i) = temp
  end do

  do i = m + 1, n, 3
    temp = x(i)
    x(i) = y(i)
    y(i) = temp
    temp = x(i+1)
    x(i+1) = y(i+1)
    y(i+1) = temp
    temp = x(i+2)
    x(i+2) = y(i+2)
    y(i+2) = temp
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
    temp = x(ix)
    x(ix) = y(iy)
    y(iy) = temp
    ix = ix + incx
    iy = iy + incy
  end do
end if
RETURN
!################################################################################
END SUBROUTINE dswap  
