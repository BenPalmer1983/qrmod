
!############################################################
SUBROUTINE ludecomp(a, p)
!############################################################
REAL(kind=real64), intent(inout) ::     a(:,:)
INTEGER(kind=int32), intent(out) ::     p(:)
!############################################################
INTEGER(kind=int32) ::                  n, i, j, k, kmax
!############################################################
n = size(a,1)
p = [ ( i, i=1,n ) ]
DO k = 1,n-1
  kmax = maxloc(abs(a(p(k:),k)),1) + k-1
  IF (kmax /= k ) THEN
    p([k, kmax]) = p([kmax, k])
    a([k, kmax],:) = a([kmax, k],:)
  END IF
  a(k+1:,k) = a(k+1:,k) / a(k,k)
  forall (j=k+1:n) a(k+1:,j) = a(k+1:,j) - a(k,j)*a(k+1:,k)
END DO
END SUBROUTINE ludecomp