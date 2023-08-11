pure function dnrm2 (n, x, incx)
!################################################################################
INTEGER(kind=int32), intent(in) ::               n
REAL(kind=real64), intent(in) ::                 x(*)
INTEGER(kind=int32), intent(in) ::               incx
!################################################################################
REAL(kind=real64) ::                             absxi
REAL(kind=real64) ::                             dnrm2
INTEGER(kind=int32) ::                           ix
REAL(kind=real64) ::                             norm
REAL(kind=real64) ::                             scale
REAL(kind=real64) ::                             ssq
!################################################################################
IF(n < 1 .OR. incx < 1)THEN
  norm  = 0.0D+00
ELSE IF(n == 1)THEN
  norm  = ABS(x(1))
ELSE
  scale = 0.0D+00
  ssq = 1.0D+00
  DO ix = 1, 1 + ( n - 1 ) * incx, incx
    IF ( x(ix) /= 0.0D+00 ) THEN
      absxi = abs ( x(ix) )
      if ( scale < absxi ) then
        ssq = 1.0D+00 + ssq * ( scale / absxi )**2
        scale = absxi
      else
        ssq = ssq + ( absxi / scale )**2
      end if
    end if
  end do
  norm  = scale * sqrt ( ssq )
end if
dnrm2 = norm
RETURN
!################################################################################
end function dnrm2 