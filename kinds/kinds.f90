MODULE kinds

IMPLICIT NONE

INTEGER, PARAMETER :: real32 = Selected_Real_Kind(6,37)
INTEGER, PARAMETER :: real64 = Selected_Real_Kind(15,307)
INTEGER, PARAMETER :: real128 = Selected_Real_Kind(33,4931) 

INTEGER, PARAMETER :: int8 = Selected_Int_Kind(1) 
INTEGER, PARAMETER :: int16 = Selected_Int_Kind(4)
INTEGER, PARAMETER :: int32 = Selected_Int_Kind(8)
INTEGER, PARAMETER :: int64 = Selected_Int_Kind(12)
INTEGER, PARAMETER :: int128 = Selected_Int_Kind(32) 

END MODULE kinds
