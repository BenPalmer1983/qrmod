SUBROUTINE dqrdc ( a, lda, n, p, qraux, jpvt, work, job )
!################################################################################
REAL(kind=real64) ::             a(lda,p)
INTEGER(kind=int32) ::           lda
INTEGER(kind=int32) ::           n
INTEGER(kind=int32) ::           p
REAL(kind=real64) ::             qraux(p)
INTEGER(kind=int32) ::           jpvt(p)
REAL(kind=real64) ::             work(p)
INTEGER(kind=int32) ::           job
!################################################################################
INTEGER(kind=int32) ::           j
INTEGER(kind=int32) ::           jp
INTEGER(kind=int32) ::           l
INTEGER(kind=int32) ::           lup
INTEGER(kind=int32) ::           maxj
REAL(kind=real64) ::             maxnrm
REAL(kind=real64) ::             nrmxl
INTEGER(kind=int32) ::           pl
INTEGER(kind=int32) ::           pu
LOGICAL ::                       swapj
REAL(kind=real64) ::             t
REAL(kind=real64) ::             tt
!################################################################################
pl = 1
pu = 0
!  If pivoting is requested, rearrange the columns.
IF(job /= 0)THEN
  DO j = 1, p
    swapj = 0 < jpvt(j)
    IF ( jpvt(j) < 0 ) THEN
      jpvt(j) = - j
    ELSE
      jpvt(j) = j
    END IF
    IF ( swapj ) THEN
      IF ( j /= pl ) THEN
        CALL dswap( n, a(1,pl), 1, a(1,j), 1)
      END IF
      jpvt(j) = jpvt(pl)
      jpvt(pl) = j
      pl = pl + 1
    END IF
  END DO
  pu = p
  DO j = p, 1, -1
    IF ( jpvt(j) < 0 ) THEN
      jpvt(j) = - jpvt(j)
      IF ( j /= pu ) THEN
        CALL dswap(n, a(1,pu), 1, a(1,j), 1)
        jp = jpvt(pu)
        jpvt(pu) = jpvt(j)
        jpvt(j) = jp
      END IF
      pu = pu - 1
    END IF
  END DO
END IF
!  Compute the norms of the free columns.
do concurrent(j = pl:pu)
  qraux(j) = dnrm2(n, a(1,j), 1)
end do
work(pl:pu) = qraux(pl:pu)
!  Perform the Householder reduction of A.
lup = min(n, p)
DO l = 1, lup
!  Bring the column of largest norm into the pivot position.
  IF ( pl <= l .AND. l < pu ) THEN
    maxnrm = 0.0D+00
    maxj = l
    DO j = l, pu
      IF ( maxnrm < qraux(j) ) THEN
        maxnrm = qraux(j)
        maxj = j
      END IF
    END DO
    IF ( maxj /= l ) THEN
      CALL dswap ( n, a(1,l), 1, a(1,maxj), 1 )
      qraux(maxj) = qraux(l)
      work(maxj) = work(l)
      jp = jpvt(maxj)
      jpvt(maxj) = jpvt(l)
      jpvt(l) = jp
    END IF
  END IF
!  Compute the Householder transformation for column L.
  qraux(l) = 0.0D+00
  IF ( l /= n ) THEN
    nrmxl = dnrm2 ( n-l+1, a(l,l), 1 )
    IF ( nrmxl /= 0.0D+00 ) THEN
      IF ( a(l,l) /= 0.0D+00 ) THEN
        nrmxl = sign ( nrmxl, a(l,l) )
      END IF
      CALL dscal ( n-l+1, 1.0D+00 / nrmxl, a(l,l), 1 )
        a(l,l) = 1.0D+00 + a(l,l)
!  Apply the transformation to the remaining columns, updating the norms.
      DO j = l + 1, p
        t = - ddot ( n-l+1, a(l,l), 1, a(l,j), 1 ) / a(l,l)
        CALL daxpy ( n-l+1, t, a(l,l), 1, a(l,j), 1 )
        IF ( pl <= j .AND. j <= pu ) THEN
          IF ( qraux(j) /= 0.0D+00 ) THEN
            tt = 1.0D+00 - ( abs ( a(l,j) ) / qraux(j) )**2
            tt = max ( tt, 0.0D+00 )
            t = tt
            tt = 1.0D+00 + 0.05D+00 * tt * ( qraux(j) / work(j) )**2
            IF ( tt /= 1.0D+00 ) THEN
              qraux(j) = qraux(j) * sqrt ( t )
            ELSE
              qraux(j) = dnrm2 ( n-l, a(l+1,j), 1 )
              work(j) = qraux(j)
            END IF
          END IF
        END IF
      END DO
!  Save the transformation.
      qraux(l) = a(l,l)
      a(l,l) = - nrmxl
    END IF
  END IF
END DO
RETURN
END SUBROUTINE dqrdc