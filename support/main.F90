PROGRAM MAIN

USE XRD_GETOPTIONS
USE GETDATA_MOD
USE MODI_ICE_ADJUST
USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
USE MODD_CST,        ONLY: CST_t, CST
USE MODD_NEB,        ONLY: NEB_t, NEB
USE MODD_RAIN_ICE_PARAM, ONLY : RAIN_ICE_PARAM_t
USE MODI_INI_CST
USE MODI_INI_NEB
USE OMP_LIB


IMPLICIT NONE

INTEGER      :: KLON 
INTEGER      :: KIDIA  
INTEGER      :: KFDIA  
INTEGER      :: KLEV  
INTEGER      :: KRR  

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
REAL, ALLOCATABLE   :: ZSIGQSAT       (:,:,:)   
REAL, ALLOCATABLE   :: ZICE_CLD_WGT   (:,:,:)   

REAL, ALLOCATABLE   :: PRS_OUT        (:,:,:,:,:) 
REAL, ALLOCATABLE   :: PSRCS_OUT      (:,:,:,:)   
REAL, ALLOCATABLE   :: PCLDFR_OUT     (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLC_HRC_OUT   (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLC_HCF_OUT   (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLI_HRI_OUT   (:,:,:,:)   
REAL, ALLOCATABLE   :: PHLI_HCF_OUT   (:,:,:,:)   

INTEGER :: NPROMA, NGPBLKS, NFLEVG
INTEGER :: IBL, JLON, JLEV

TYPE(DIMPHYEX_t)         :: D
TYPE(RAIN_ICE_PARAM_t)   :: ICEP
CHARACTER(LEN=1)         :: HFRAC_ICE
CHARACTER(LEN=80)        :: HCONDENS
CHARACTER(LEN=4)         :: HLAMBDA3 
CHARACTER(LEN=4)         :: HBUNAME  
LOGICAL                  :: OSUBG_COND
LOGICAL                  :: OSIGMAS  
LOGICAL                  :: OCND2    
LOGICAL                  :: LMFCONV
CHARACTER(LEN=80)        :: HSUBG_MF_PDF
REAL                     :: PTSTEP    
LOGICAL                  :: LLCHECK
INTEGER                  :: IBLOCK1, IBLOCK2

REAL(KIND=8) :: TS,TE
REAL(KIND=8) :: TSC, TEC, TSD, TED, ZTC, ZTD 
INTEGER :: ITIME, NTIME
LOGICAL :: LLVERBOSE, LLSTAT


CALL INITOPTIONS ()
NGPBLKS = 296
CALL GETOPTION ("--blocks", NGPBLKS)
NPROMA = 32
CALL GETOPTION ("--nproma", NPROMA)
NFLEVG = -1
CALL GETOPTION ("--nflevg", NFLEVG)
CALL GETOPTION ("--check",  LLCHECK)
IBLOCK1 = 1
CALL GETOPTION ("--check-block-1", IBLOCK1)
IBLOCK2 = NGPBLKS
CALL GETOPTION ("--check-block-2", IBLOCK2)
CALL GETOPTION ("--stat", LLSTAT)
NTIME = 1
CALL GETOPTION ("--times", NTIME)
CALL GETOPTION ("--verbose", LLVERBOSE)
CALL CHECKOPTIONS ()

CALL GETDATA (NPROMA, NGPBLKS, NFLEVG, PRHODJ, PEXNREF, PRHODREF, PPABSM, PTHT, ZICE_CLD_WGT,     &
& ZSIGQSAT, PSIGS, PMFCONV, PRC_MF, PRI_MF, PCF_MF, PTHS, PRS, PSRCS, PCLDFR, PHLC_HRC, PHLC_HCF, &
& PHLI_HRI, PHLI_HCF, ZRS, ZZZ, PRS_OUT, PSRCS_OUT, PCLDFR_OUT, PHLC_HRC_OUT, PHLC_HCF_OUT,       &
& PHLI_HRI_OUT, PHLI_HCF_OUT, LLVERBOSE)


KLEV = SIZE (PRS, 3)
KRR  = SIZE (PRS, 4)

IF (LLVERBOSE) PRINT *, " KLEV = ", KLEV, " KRR = ", KRR

CALL INI_CST
CALL INI_NEB

! Taken from ini_rain_ice.F90; we only need these for ice_adjust.F90
ICEP%XCRIAUTI  = 0.2E-4 
ICEP%XCRIAUTC  = 0.5E-3
ICEP%XACRIAUTI = 0.06
ICEP%XBCRIAUTI = -3.5

HFRAC_ICE    = 'S'
HCONDENS     = 'CB02'
HLAMBDA3     = 'CB'
HBUNAME      = 'DEPI'
OSUBG_COND   = .TRUE.
OSIGMAS      = .TRUE.
OCND2        = .FALSE.
HSUBG_MF_PDF = 'TRIANGLE'
PTSTEP       = 50.000000000000000    
LMFCONV      = .TRUE.

D%NIT  = NPROMA
D%NIB  = 1
D%NIE  = NPROMA
D%NJT  = 1
D%NJB  = 1
D%NJE  = 1
D%NKL  = -1
D%NKT  = KLEV
D%NKA  = KLEV
D%NKU  = 1
D%NKB  = KLEV 
D%NKE  = 1
D%NKTB = 1
D%NKTE = KLEV

TS = OMP_GET_WTIME ()

ZTD = 0.
ZTC = 0.


DO ITIME = 1, NTIME

  TSD = OMP_GET_WTIME ()
!
  TSC = OMP_GET_WTIME ()

  !$OMP PARALLEL DO PRIVATE (IBL)
  DO IBL = 1, NGPBLKS
    CALL ICE_ADJUST (D, CST, ICEP, NEB, KRR, HFRAC_ICE, HCONDENS, HLAMBDA3, HBUNAME, OSUBG_COND,                                &
    & OSIGMAS, OCND2, HSUBG_MF_PDF, PTSTEP, ZSIGQSAT (:, :, IBL), PRHODJ=PRHODJ (:, :, :, IBL), PEXNREF=PEXNREF (:, :, :, IBL), &
    & PRHODREF=PRHODREF (:, :, :, IBL), PSIGS=PSIGS (:, :, :, IBL), LMFCONV=LMFCONV, PMFCONV=PMFCONV (:, :, :, IBL),            &
    & PPABST=PPABSM (:, :, :, IBL), PZZ=ZZZ (:, :, :, IBL), PEXN=PEXNREF (:, :, :, IBL), PCF_MF=PCF_MF (:, :, :, IBL),          &
    & PRC_MF=PRC_MF (:, :, :, IBL), PRI_MF=PRI_MF  (:, :, :, IBL), PRV=ZRS(:, :, :, 1, IBL), PRC=ZRS(:, :, :, 2, IBL),          &
    & PRVS=PRS(:, :, :, 1, IBL), PRCS=PRS(:, :, :, 2, IBL), PTH=ZRS(:, :, :, 0, IBL), PTHS=PTHS (:, :, :, IBL),                 &
    & PSRCS=PSRCS (:, :, :, IBL), PCLDFR=PCLDFR (:, :, :, IBL), PRR=ZRS(:, :, :, 3, IBL), PRI=ZRS(:, :, :, 4, IBL),             &
    & PRIS=PRS(:, :, :, 4, IBL), PRS=ZRS(:, :, :, 5, IBL), PRG=ZRS(:, :, :, 6, IBL), PHLC_HRC=PHLC_HRC(:, :, :, IBL),           &
    & PHLC_HCF=PHLC_HCF(:, :, :, IBL), PHLI_HRI=PHLI_HRI(:, :, :, IBL), PHLI_HCF=PHLI_HCF(:, :, :, IBL),                        &
    & PICE_CLD_WGT=ZICE_CLD_WGT(:, :, IBL))
  ENDDO
  !$OMP END PARALLEL DO

  TEC = OMP_GET_WTIME ()
!
  TED = OMP_GET_WTIME ()

  ZTC = ZTC + (TEC - TSC)
  ZTD = ZTD + (TED - TSD)

ENDDO

TE = OMP_GET_WTIME()

WRITE (*,'(A,F8.2,A)') 'elapsed time : ',TE-TS,' s'
WRITE (*,'(A,F8.4,A)') '          i.e. ',1000.*(TE-TS)/(NPROMA*NGPBLKS)/NTIME,' ms/gp'

PRINT *, " ZTD = ", ZTD, ZTD / REAL (NPROMA*NGPBLKS*NTIME)
PRINT *, " ZTC = ", ZTC, ZTC / REAL (NPROMA*NGPBLKS*NTIME)


IF (LLCHECK .OR. LLSTAT) THEN
  DO IBL = IBLOCK1, IBLOCK2
    PRINT *, " IBL = ", IBL
    CALL DIFF ("PSRCS",    PSRCS_OUT    (:,:,:,IBL), PSRCS    (:,:,:,IBL))
    CALL DIFF ("PCLDFR",   PCLDFR_OUT   (:,:,:,IBL), PCLDFR   (:,:,:,IBL))
    CALL DIFF ("PHLC_HRC", PHLC_HRC_OUT (:,:,:,IBL), PHLC_HRC (:,:,:,IBL))
    CALL DIFF ("PHLC_HCF", PHLC_HCF_OUT (:,:,:,IBL), PHLC_HCF (:,:,:,IBL))
    CALL DIFF ("PHLI_HRI", PHLI_HRI_OUT (:,:,:,IBL), PHLI_HRI (:,:,:,IBL))
    CALL DIFF ("PHLI_HCF", PHLI_HCF_OUT (:,:,:,IBL), PHLI_HCF (:,:,:,IBL))
  ENDDO
ENDIF

STOP

CONTAINS

SUBROUTINE DIFF (CDNAME, PREF, POUT)

CHARACTER (LEN=*) :: CDNAME
REAL :: PREF (:,:,:)
REAL :: POUT (:,:,:)

INTEGER :: JLON, JLEV

PRINT *, CDNAME
IF (LLSTAT) THEN
  PRINT *, MINVAL (PREF), MAXVAL (PREF), SUM (PREF) / SIZE (PREF)
  PRINT *, MINVAL (POUT), MAXVAL (POUT), SUM (POUT) / SIZE (POUT)
ENDIF

IF (LLCHECK) THEN
  WRITE (*, '(A4)', ADVANCE='NO') ""
  DO JLON = 1, NPROMA
    WRITE (*, '("|",I12,A12)', ADVANCE='NO') JLON, ""
  ENDDO
  WRITE (*, '("|")')
  DO JLEV = 1, KLEV
    WRITE (*, '(I4)', ADVANCE='NO') JLEV
    DO JLON = 1, NPROMA
      IF (ABS (PREF (JLON, 1, JLEV)) + ABS (POUT (JLON, 1, JLEV)) == 0.) THEN
      WRITE (*, '("|",2A12)', ADVANCE='NO') "", ""
      ELSE
      WRITE (*, '("|",2E12.5)', ADVANCE='NO') PREF (JLON, 1, JLEV), POUT (JLON, 1, JLEV)
      ENDIF
    ENDDO
    WRITE (*, '("|")')
  ENDDO
ENDIF

END SUBROUTINE


END
