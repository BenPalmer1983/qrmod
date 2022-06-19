SUBROUTINE dsvdc ( a, lda, m, n, s, e, u, ldu, v, ldv, work, job, info )
!################################################################################
IMPLICIT NONE
!################################################################################
INTEGER(kind=int32) , PARAMETER :: maxit = 30
!################################################################################
REAL(kind=real64) ::             a(lda,n)
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           m
INTEGER(kind=int32) ::           n
REAL(kind=real64) ::             s(*)
REAL(kind=real64) ::             e(*)
REAL(kind=real64) ::             u(ldu,m)
INTEGER(kind=int32) ::           ldu
REAL(kind=real64) ::             v(ldv,n)
INTEGER(kind=int32) ::           ldv
REAL(kind=real64) ::             work(m)
INTEGER(kind=int32) ::           job
INTEGER(kind=int32) ::           info
!################################################################################
REAL(kind=real64) ::             b
REAL(kind=real64) ::             c
REAL(kind=real64) ::             cs
REAL(kind=real64) ::             el
REAL(kind=real64) ::             emm1
REAL(kind=real64) ::             f
REAL(kind=real64) ::             g
INTEGER(kind=int32) ::           iter
INTEGER(kind=int32) ::           j
INTEGER(kind=int32) ::           jobu
INTEGER(kind=int32) ::           k
INTEGER(kind=int32) ::           kase
INTEGER(kind=int32) ::           kk
INTEGER(kind=int32) ::           l
INTEGER(kind=int32) ::           ll
INTEGER(kind=int32) ::           lls
INTEGER(kind=int32) ::           ls
INTEGER(kind=int32) ::           lu
INTEGER(kind=int32) ::           mm
INTEGER(kind=int32) ::           mm1
INTEGER(kind=int32) ::           mn
INTEGER(kind=int32) ::           nct
INTEGER(kind=int32) ::           nctp1
INTEGER(kind=int32) ::           ncu
INTEGER(kind=int32) ::           nrt
INTEGER(kind=int32) ::           nrtp1
REAL(kind=real64) ::             scale
REAL(kind=real64) ::             shift
REAL(kind=real64) ::             sl
REAL(kind=real64) ::             sm
REAL(kind=real64) ::             smm1
REAL(kind=real64) ::             sn
REAL(kind=real64) ::             t
REAL(kind=real64) ::             t1
REAL(kind=real64) ::             test
LOGICAL ::                       wantu
LOGICAL ::                       wantv
REAL(kind=real64) ::             ztest
!################################################################################
!  Determine what is to be computed.
  wantu = .false.
  wantv = .false.
  jobu = mod ( job, 100 ) / 10

  if ( 1 < jobu ) then
    ncu = min ( m, n )
  else
    ncu = m
  end if

  if ( jobu /= 0 ) then
    wantu = .true.
  end if

  if ( mod ( job, 10 ) /= 0 ) then
    wantv = .true.
  end if
!
!  Reduce A to bidiagonal form, storing the diagonal elements
!  in S and the super-diagonal elements in E.
!
  info = 0
  nct = min ( m-1, n )
  nrt = max ( 0, min ( m, n-2 ) )
  lu = max ( nct, nrt )

  do l = 1, lu
!
!  Compute the transformation for the L-th column and
!  place the L-th diagonal in S(L).
!
    if ( l <= nct ) then

      s(l) = dnrm2 ( m-l+1, a(l,l), 1 )

      if ( s(l) /= 0.0D+00 ) then
        if ( a(l,l) /= 0.0D+00 ) then
          s(l) = sign ( s(l), a(l,l) )
        end if
        call dscal ( m-l+1, 1.0D+00 / s(l), a(l,l), 1 )
        a(l,l) = 1.0D+00 + a(l,l)
      end if

      s(l) = -s(l)

    end if

    do j = l+1, n
!
!  Apply the transformation.
!
      if ( l <= nct .and. s(l) /= 0.0D+00 ) then
        t = -ddot ( m-l+1, a(l,l), 1, a(l,j), 1 ) / a(l,l)
        call daxpy ( m-l+1, t, a(l,l), 1, a(l,j), 1 )
      end if
!
!  Place the L-th row of A into E for the
!  subsequent calculation of the row transformation.
!
      e(j) = a(l,j)

    end do
!
!  Place the transformation in U for subsequent back multiplication.
!
    if ( wantu .and. l <= nct ) then
      u(l:m,l) = a(l:m,l)
    end if
!
!  Compute the L-th row transformation and place the
!  L-th superdiagonal in E(L).
!
    if ( l <= nrt ) then

      e(l) = dnrm2 ( n-l, e(l+1), 1 )

      if ( e(l) /= 0.0D+00 ) then
        if ( e(l+1) /= 0.0D+00 ) then
          e(l) = sign ( e(l), e(l+1) )
        end if
        call dscal ( n-l, 1.0D+00 / e(l), e(l+1), 1 )
        e(l+1) = 1.0D+00 + e(l+1)
      end if

      e(l) = -e(l)
!
!  Apply the transformation.
!
      if ( l + 1 <= m .and. e(l) /= 0.0D+00 ) then

        work(l+1:m) = 0.0D+00

        do j = l+1, n
          call daxpy ( m-l, e(j), a(l+1,j), 1, work(l+1), 1 )
        end do

        do j = l+1, n
          call daxpy ( m-l, -e(j)/e(l+1), work(l+1), 1, a(l+1,j), 1 )
        end do

      end if
!
!  Place the transformation in V for subsequent back multiplication.
!
      if ( wantv ) then
        v(l+1:n,l) = e(l+1:n)
      end if

    end if

  end do
!
!  Set up the final bidiagonal matrix of order MN.
!
  mn = min ( m + 1, n )
  nctp1 = nct + 1
  nrtp1 = nrt + 1

  if ( nct < n ) then
    s(nctp1) = a(nctp1,nctp1)
  end if

  if ( m < mn ) then
    s(mn) = 0.0D+00
  end if

  if ( nrtp1 < mn ) then
    e(nrtp1) = a(nrtp1,mn)
  end if

  e(mn) = 0.0D+00
!
!  If required, generate U.
!
  if ( wantu ) then

    u(1:m,nctp1:ncu) = 0.0D+00

    do j = nctp1, ncu
      u(j,j) = 1.0D+00
    end do

    do ll = 1, nct

      l = nct - ll + 1

      if ( s(l) /= 0.0D+00 ) then

        do j = l+1, ncu
          t = -ddot ( m-l+1, u(l,l), 1, u(l,j), 1 ) / u(l,l)
          call daxpy ( m-l+1, t, u(l,l), 1, u(l,j), 1 )
        end do

        u(l:m,l) = -u(l:m,l)
        u(l,l) = 1.0D+00 + u(l,l)
        u(1:l-1,l) = 0.0D+00

      else

        u(1:m,l) = 0.0D+00
        u(l,l) = 1.0D+00

      end if

    end do

  end if
!
!  If it is required, generate V.
!
  if ( wantv ) then

    do ll = 1, n

      l = n - ll + 1

      if ( l <= nrt .and. e(l) /= 0.0D+00 ) then

        do j = l + 1, n
          t = -ddot ( n-l, v(l+1,l), 1, v(l+1,j), 1 ) / v(l+1,l)
          call daxpy ( n-l, t, v(l+1,l), 1, v(l+1,j), 1 )
        end do

      end if

      v(1:n,l) = 0.0D+00
      v(l,l) = 1.0D+00

    end do

  end if
!
!  Main iteration loop for the singular values.
!
  mm = mn
  iter = 0

  do while ( 0 < mn )
!
!  If too many iterations have been performed, set flag and return.
!
    if ( maxit <= iter ) then
      info = mn
      return
    end if
!
!  This section of the program inspects for
!  negligible elements in the S and E arrays.
!
!  On completion the variables KASE and L are set as follows:
!
!  KASE = 1     if S(MN) and E(L-1) are negligible and L < MN
!  KASE = 2     if S(L) is negligible and L < MN
!  KASE = 3     if E(L-1) is negligible, L < MN, and
!               S(L), ..., S(MN) are not negligible (QR step).
!  KASE = 4     if E(MN-1) is negligible (convergence).
!
    do ll = 1, mn

      l = mn - ll

      if ( l == 0 ) then
        exit
      end if

      test = abs ( s(l) ) + abs ( s(l+1) )
      ztest = test + abs ( e(l) )

      if ( ztest == test ) then
        e(l) = 0.0D+00
        exit
      end if

    end do

    if ( l == mn - 1 ) then

      kase = 4

    else

      do lls = l + 1, mn + 1

        ls = mn - lls + l + 1

        if ( ls == l ) then
          exit
        end if

        test = 0.0D+00
        if ( ls /= mn ) then
          test = test + abs ( e(ls) )
        end if

        if ( ls /= l + 1 ) then
          test = test + abs ( e(ls-1) )
        end if

        ztest = test + abs ( s(ls) )

        if ( ztest == test ) then
          s(ls) = 0.0D+00
          exit
        end if

      end do

      if ( ls == l ) then
        kase = 3
      else if ( ls == mn ) then
        kase = 1
      else
        kase = 2
        l = ls
      end if

    end if

    l = l + 1
!
!  Deflate negligible S(MN).
!
    if ( kase == 1 ) then

      mm1 = mn - 1
      f = e(mn-1)
      e(mn-1) = 0.0D+00

      do kk = l, mm1

        k = mm1 - kk + l
        t1 = s(k)
        call drotg ( t1, f, cs, sn )
        s(k) = t1

        if ( k /= l ) then
          f = -sn * e(k-1)
          e(k-1) = cs * e(k-1)
        end if

        if ( wantv ) then
          call drot ( n, v(1,k), 1, v(1,mn), 1, cs, sn )
        end if

      end do
!
!  Split at negligible S(L).
!
    else if ( kase == 2 ) then

      f = e(l-1)
      e(l-1) = 0.0D+00

      do k = l, mn

        t1 = s(k)
        call drotg ( t1, f, cs, sn )
        s(k) = t1
        f = -sn * e(k)
        e(k) = cs * e(k)
        if ( wantu ) then
          call drot ( m, u(1,k), 1, u(1,l-1), 1, cs, sn )
        end if

      end do
!
!  Perform one QR step.
!
    else if ( kase == 3 ) then
!
!  Calculate the shift.
!
      scale = max ( abs ( s(mn) ), abs ( s(mn-1) ), abs ( e(mn-1) ), &
                    abs ( s(l) ), abs ( e(l) ) )

      sm = s(mn) / scale
      smm1 = s(mn-1) / scale
      emm1 = e(mn-1) / scale
      sl = s(l) / scale
      el = e(l) / scale
      b = ( ( smm1 + sm ) * ( smm1 - sm ) + emm1 * emm1 ) / 2.0D+00
      c = sm  * sm * emm1 * emm1
      shift = 0.0D+00

      if ( b /= 0.0D+00 .or. c /= 0.0D+00 ) then
        shift = sqrt ( b * b + c )
        if ( b < 0.0D+00 ) then
          shift = -shift
        end if
        shift = c / ( b + shift )
      end if

      f = ( sl + sm ) * ( sl - sm ) + shift
      g = sl * el
!
!  Chase zeros.
!
      mm1 = mn - 1
      do k = l, mm1
        call drotg ( f, g, cs, sn )
        if ( k /= l ) then
          e(k-1) = f
        end if
        f = cs * s(k) + sn * e(k)
        e(k) = cs * e(k) - sn * s(k)
        g = sn * s(k+1)
        s(k+1) = cs * s(k+1)
        if ( wantv ) then
          call drot ( n, v(1,k), 1, v(1,k+1), 1, cs, sn )
        end if
        call drotg ( f, g, cs, sn )
        s(k) = f
        f = cs * e(k) + sn * s(k+1)
        s(k+1) = -sn * e(k) + cs * s(k+1)
        g = sn * e(k+1)
        e(k+1) = cs * e(k+1)
        if ( wantu .and. k < m ) then
          call drot ( m, u(1,k), 1, u(1,k+1), 1, cs, sn )
        end if
      end do
      e(mn-1) = f
      iter = iter + 1
!
!  Convergence.
!
    else if ( kase == 4 ) then
!
!  Make the singular value nonnegative.
!
      if ( s(l) < 0.0D+00 ) then
        s(l) = -s(l)
        if ( wantv ) then
          v(1:n,l) = -v(1:n,l)
        end if
      end if
!
!  Order the singular value.
!
      do
        if ( l == mm ) then
          exit
        end if
        if ( s(l+1) <= s(l) ) then
          exit
        end if
        t = s(l)
        s(l) = s(l+1)
        s(l+1) = t
        if ( wantv .and. l < n ) then
          call dswap ( n, v(1,l), 1, v(1,l+1), 1 )
        end if
        if ( wantu .and. l < m ) then
          call dswap ( m, u(1,l), 1, u(1,l+1), 1 )
        end if
        l = l + 1
      end do
      iter = 0
      mn = mn - 1
    end if
  end do
RETURN  
!################################################################################
END SUBROUTINE dsvdc
