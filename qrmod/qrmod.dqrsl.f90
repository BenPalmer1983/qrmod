SUBROUTINE dqrsl ( a, lda, n, k, qraux, y, qy, qty, b, rsd, ab, job, info )
!################################################################################
REAL(kind=real64) ::             a(lda,*)
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           n
INTEGER(kind=int32) ::           k
REAL(kind=real64) ::             qraux(*)
REAL(kind=real64) ::             y(n)
REAL(kind=real64) ::             qy(n)
REAL(kind=real64) ::             qty(n)
REAL(kind=real64) ::             b(k)
REAL(kind=real64) ::             rsd(n)
REAL(kind=real64) ::             ab(n)
INTEGER(kind=int32) ::           job
INTEGER(kind=int32) ::           info
!################################################################################
LOGICAL ::                       cab
LOGICAL ::                       cb
LOGICAL ::                       cqty
LOGICAL ::                       cqy
LOGICAL ::                       cr
INTEGER(kind=int32) ::           j
INTEGER(kind=int32) ::           jj
INTEGER(kind=int32) ::           ju
INTEGER(kind=int32) ::           kp1
REAL(kind=real64) ::             t
REAL(kind=real64) ::             temp
!################################################################################
!  set info flag.
info = 0
!  Determine what is to be computed.
cqy =        job / 10000         /= 0
cqty = mod ( job,  10000 )       /= 0
cb =   mod ( job,   1000 ) / 100 /= 0
cr =   mod ( job,    100 ) /  10 /= 0
cab =  mod ( job,     10 )       /= 0
ju = min ( k, n-1 )
!  Special action when N = 1.
if ( ju == 0 ) then
  if ( cqy ) then
    qy(1) = y(1)
  end if
  if ( cqty ) then
    qty(1) = y(1)
  end if
  if ( cab ) then
    ab(1) = y(1)
  end if
  if ( cb ) then
    if ( a(1,1) == 0.0D+00 ) then
      info = 1
    else
      b(1) = y(1) / a(1,1)
    end if
  end if
  if ( cr ) then
    rsd(1) = 0.0D+00
  end if
  RETURN
end if
!  Set up to compute QY or QTY.
if ( cqy ) then
  qy(1:n) = y(1:n)
end if
if ( cqty ) then
  qty(1:n) = y(1:n)
end if
!  Compute QY.
if ( cqy ) then
  do jj = 1, ju
    j = ju - jj + 1
    if ( qraux(j) /= 0.0D+00 ) then
      temp = a(j,j)
      a(j,j) = qraux(j)
      t = - ddot ( n-j+1, a(j,j), 1, qy(j), 1 ) / a(j,j)
      call daxpy ( n-j+1, t, a(j,j), 1, qy(j), 1 )
      a(j,j) = temp
    end if
  end do
end if
!  Compute Q'*Y.
if ( cqty ) then
  do j = 1, ju
    if ( qraux(j) /= 0.0D+00 ) then
      temp = a(j,j)
      a(j,j) = qraux(j)
      t = - ddot ( n-j+1, a(j,j), 1, qty(j), 1 ) / a(j,j)
      call daxpy ( n-j+1, t, a(j,j), 1, qty(j), 1 )
      a(j,j) = temp
    end if
  end do
end if
!  Set up to compute B, RSD, or AB.
if ( cb ) then
  b(1:k) = qty(1:k)
end if
kp1 = k + 1
if ( cab ) then
  ab(1:k) = qty(1:k)
end if
if ( cr .and. k < n ) then
  rsd(k+1:n) = qty(k+1:n)
end if
IF(cab .and. k+1 <= n)then
  ab(k+1:n) = 0.0D+00
end if
if ( cr ) then
  rsd(1:k) = 0.0D+00
END IF
!  Compute B.
if (cb) then
  do jj = 1, k
    j = k - jj + 1
    if ( a(j,j) == 0.0D+00 ) then
      info = j
      EXIT
    end if
    b(j) = b(j)/a(j,j)
    if ( j /= 1 ) then
      t = -b(j)
      call daxpy ( j-1, t, a(1,j), 1, b, 1 )
    end if
  end do
end if
if ( cr .or. cab ) then
!  Compute RSD or AB as required.
  do jj = 1, ju
    j = ju - jj + 1
    if ( qraux(j) /= 0.0D+00 ) then
      temp = a(j,j)
      a(j,j) = qraux(j)
      if ( cr ) then
        t = - ddot ( n-j+1, a(j,j), 1, rsd(j), 1 ) / a(j,j)
        call daxpy ( n-j+1, t, a(j,j), 1, rsd(j), 1 )
      end if
      if(cab)then
        t = - ddot ( n-j+1, a(j,j), 1, ab(j), 1 ) / a(j,j)
        call daxpy ( n-j+1, t, a(j,j), 1, ab(j), 1 )
      end if
      a(j,j) = temp
    end if
  end do
end if
RETURN
!################################################################################
END SUBROUTINE dqrsl