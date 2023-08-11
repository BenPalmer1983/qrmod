SUBROUTINE drotg(sa, sb, c, s)
!################################################################################
REAL(kind=real64) ::             sa
REAL(kind=real64) ::             sb
REAL(kind=real64) ::             c
REAL(kind=real64) ::             s
!################################################################################
REAL(kind=real64) ::             r
REAL(kind=real64) ::             roe
REAL(kind=real64) ::             scale
REAL(kind=real64) ::             z
!################################################################################
IF(abs(sb) < abs (sa)) then
  roe = sa
else
  roe = sb
end if
scale = abs ( sa ) + abs ( sb )
if ( scale == 0.0D+00 ) then
  c = 1.0D+00
  s = 0.0D+00
  r = 0.0D+00
else
  r = scale * sqrt ( ( sa / scale )**2 + ( sb / scale )**2 )
  r = sign ( 1.0D+00, roe ) * r
  c = sa / r
  s = sb / r
end if
if ( 0.0D+00 < abs ( c ) .and. abs ( c ) <= s ) then
  z = 1.0D+00 / c
else
  z = s
end if
sa = r
sb = z
RETURN
!################################################################################
END SUBROUTINE drotg 