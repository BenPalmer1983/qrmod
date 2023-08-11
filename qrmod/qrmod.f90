module qrmod

use kinds

implicit none
! private

! public :: qr_solve, normal_solve, svd_solve

!############################################################
contains
!############################################################

include "qrmod.comments.f90"
include "qrmod.daxpy.f90"
include "qrmod.ddot.f90"
include "qrmod.dnrm2.f90"
include "qrmod.dqrank.f90"
include "qrmod.dqrdc.f90"
include "qrmod.dqrls.f90"
include "qrmod.dqrlss.f90"
include "qrmod.dqrsl.f90"
include "qrmod.drot.f90"
include "qrmod.drotg.f90"
include "qrmod.dscal.f90"
include "qrmod.dsvdc.f90"
include "qrmod.dswap.f90"
include "qrmod.normal_solve.f90"
include "qrmod.qr_solve.f90"
include "qrmod.svd_solve.f90"
include "qrmod.r8mat_cholesky_factor.f90"
include "qrmod.r8mat_cholesky_solve.f90"
include "qrmod.r8mat_l_solve.f90"
include "qrmod.r8mat_lt_solve.f90"


end module qrmod