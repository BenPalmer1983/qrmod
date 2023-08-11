SUBROUTINE dscal(n, sa, x, incx)
!################################################################################
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             sa
REAL(kind=real64) ::             x(*)
INTEGER(kind=int32) ::           incx
!################################################################################
INTEGER(kind=int32) ::           i
INTEGER(kind=int32) ::           ix
INTEGER(kind=int32) ::           m
!################################################################################
if ( n <= 0 ) then
else if ( incx == 1 ) then
  m = mod ( n, 5 )
  x(1:m) = sa * x(1:m)
  do i = m+1, n, 5
    x(i)   = sa * x(i)
    x(i+1) = sa * x(i+1)
    x(i+2) = sa * x(i+2)
    x(i+3) = sa * x(i+3)
    x(i+4) = sa * x(i+4)
  end do
else
  if ( 0 <= incx ) then
    ix = 1
  else
    ix = ( - n + 1 ) * incx + 1
  end if
  do i = 1, n
    x(ix) = sa * x(ix)
    ix = ix + incx
  end do
end if
RETURN
!################################################################################
END SUBROUTINE dscal