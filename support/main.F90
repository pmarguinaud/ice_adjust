PROGRAM MAIN

USE XRD_GETOPTIONS
USE GETDATA_MOD

IMPLICIT NONE

INTEGER      :: KLON 
INTEGER      :: KIDIA  
INTEGER      :: KFDIA  
INTEGER      :: KLEV  
INTEGER      :: KRR  

REAL, ALLOCATABLE   :: PZZF           (:,:,:,:)   
REAL, ALLOCATABLE   :: PRHODJ         (:,:,:,:)   
REAL, ALLOCATABLE   :: PEXNREF        (:,:,:,:)   
REAL, ALLOCATABLE   :: PRHODREF       (:,:,:,:)   
REAL, ALLOCATABLE   :: PPABSM         (:,:,:,:)   
REAL, ALLOCATABLE   :: PTHT           (:,:,:,:)   
REAL, ALLOCATABLE   :: PSIGS          (:,:,:,:)   
REAL, ALLOCATABLE   :: PMFCONV        (:,:,:,:)   
REAL, ALLOCATABLE   :: PRC_MF         (:,:,:,:)   
REAL, ALLOCATABLE   :: PRI_MF         (:,:,:,:)   
REAL, ALLOCATABLE   :: PCF_MF         (:,:,:,:)   
REAL, ALLOCATABLE   :: PTHS           (:,:,:,:)   
REAL, ALLOCATABLE   :: PRS            (:,:,:,:,:) 
REAL, ALLOCATABLE   :: PSRCS          (:,:,:,:)   
REAL, ALLOCATABLE   :: PCLDFR         (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLC_HRC       (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLC_HCF       (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLI_HRI       (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLI_HCF       (:,:,:,:)   
REAL, ALLOCATABLE   :: ZRS            (:,:,:,:,:) 
REAL, ALLOCATABLE   :: ZZZ            (:,:,:,:)   

REAL, ALLOCATABLE   :: PRS_OUT        (:,:,:,:,:) 
REAL, ALLOCATABLE   :: PSRCS_OUT      (:,:,:,:)   
REAL, ALLOCATABLE   :: PCLDFR_OUT     (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLC_HRC_OUT   (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLC_HCF_OUT   (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLI_HRI_OUT   (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLI_HCF_OUT   (:,:,:,:)   

INTEGER :: NPROMA, NGPBLKS, NFLEVG
INTEGER :: IBL

CALL INITOPTIONS ()
NGPBLKS = 296
CALL GETOPTION ("--blocks", NGPBLKS)
NPROMA = 32
CALL GETOPTION ("--nproma", NPROMA)
NFLEVG = -1
CALL GETOPTION ("--nflevg", NFLEVG)
CALL CHECKOPTIONS ()

CALL GETDATA (NPROMA, NGPBLKS, NFLEVG, PZZF, PRHODJ, PEXNREF, PRHODREF, PPABSM,   &
& PTHT, PSIGS, PMFCONV, PRC_MF, PRI_MF, PCF_MF, PTHS, PRS, PSRCS, PCLDFR, PHLC_HRC, &
& PHLC_HCF, PHLI_HRI, PHLI_HCF, ZRS, ZZZ, PRS_OUT, PSRCS_OUT, PCLDFR_OUT, &
& PHLC_HRC_OUT, PHLC_HCF_OUT, PHLI_HRI_OUT, PHLI_HCF_OUT)

KLEV = SIZE (PRS, 2)
KRR  = SIZE (PRS, 3)

PRINT *, LBOUND (PRS)
PRINT *, UBOUND (PRS)

PRINT *, " P = ", PRS (1, 1, :, 1, 1)

END
