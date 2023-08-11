MODULE qrmod

USE kinds

IMPLICIT NONE

!############################################################
CONTAINS
!############################################################

INCLUDE "qrmod.comments.f90"
INCLUDE "qrmod.daxpy.f90"
INCLUDE "qrmod.ddot.f90"
INCLUDE "qrmod.dnrm2.f90"
INCLUDE "qrmod.dqrank.f90"
INCLUDE "qrmod.dqrdc.f90"
INCLUDE "qrmod.dqrls.f90"
INCLUDE "qrmod.dqrlss.f90"
INCLUDE "qrmod.dqrsl.f90"
INCLUDE "qrmod.drot.f90"
INCLUDE "qrmod.drotg.f90"
INCLUDE "qrmod.dscal.f90"
INCLUDE "qrmod.dsvdc.f90"
INCLUDE "qrmod.dswap.f90"
INCLUDE "qrmod.ludecomp.f90"
INCLUDE "qrmod.lu_solve.f90"
INCLUDE "qrmod.normal_solve.f90"
INCLUDE "qrmod.qr_solve.f90"
INCLUDE "qrmod.svd_solve.f90"
INCLUDE "qrmod.r8mat_cholesky_factor.f90"
INCLUDE "qrmod.r8mat_cholesky_solve.f90"
INCLUDE "qrmod.r8mat_l_solve.f90"
INCLUDE "qrmod.r8mat_lt_solve.f90"


END MODULE qrmod