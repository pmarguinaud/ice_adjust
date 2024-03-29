!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###############
      MODULE MODD_CST      
!     ###############
!
!!****  *MODD_CST* - declaration of Physic constants 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     Physics constants.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_CST)
!!          
!!    AUTHOR
!!    ------
!!      V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/05/94  
!!      J. Stein    02/01/95  add xrholw                    
!!      J.-P. Pinty 13/12/95  add XALPI,XBETAI,XGAMI
!!      J. Stein    25/07/97  add XTH00                    
!!      V. Masson   05/10/98  add XRHOLI
!!      C. Mari     31/10/00  add NDAYSEC
!!      V. Masson   01/03/03  add conductivity of ice
!!      R. El Khatib 04/08/14 add pre-computed quantities
!!      J.Escobar : 10/2017 : for real*4 , add XMNH_HUGE_12_LOG
!!      J.L. Redelsperger 03/2021  add constants for ocean penetrating solar
!!      S. Riette:  Jan 2022: introduction of a strucuture
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
TYPE CST_t
REAL :: XPI                ! Pi
!
REAL :: XDAY,XSIYEA,XSIDAY ! day duration, sideral year duration,
                                ! sideral day duration
!
REAL :: XKARMAN            ! von karman constant
REAL :: XLIGHTSPEED        ! light speed
REAL :: XPLANCK            ! Planck constant
REAL :: XBOLTZ             ! Boltzman constant 
REAL :: XAVOGADRO          ! Avogadro number
!
REAL :: XRADIUS,XOMEGA     ! Earth radius, earth rotation
REAL :: XG                 ! Gravity constant
!
REAL :: XP00               ! Reference pressure
REAL :: XP00OCEAN          ! Reference pressure for ocean model
REAL :: XRH00OCEAN         ! Reference density for ocean model
!
REAL :: XSTEFAN,XI0        ! Stefan-Boltzman constant, solar constant
!
REAL :: XMD,XMV            ! Molar mass of dry air and molar mass of vapor
REAL :: XRD,XRV            ! Gaz constant for dry air, gaz constant for vapor
REAL :: XEPSILO            ! XMV/XMD
REAL :: XCPD,XCPV          ! Cpd (dry air), Cpv (vapor)
REAL :: XRHOLW             ! Volumic mass of liquid water
REAL :: XCL,XCI            ! Cl (liquid), Ci (ice)
REAL :: XTT                ! Triple point temperature
REAL :: XLVTT              ! Vaporization heat constant
REAL :: XLSTT              ! Sublimation heat constant
REAL :: XLMTT              ! Melting heat constant
REAL :: XESTT              ! Saturation vapor pressure  at triple point
                                ! temperature  
REAL :: XALPW,XBETAW,XGAMW ! Constants for saturation vapor 
                                !  pressure  function 
REAL :: XALPI,XBETAI,XGAMI ! Constants for saturation vapor
                                !  pressure  function over solid ice
REAL :: XCONDI             ! thermal conductivity of ice (W m-1 K-1)
REAL :: XALPHAOC           ! thermal expansion coefficient for ocean (K-1)
REAL :: XBETAOC             ! Haline contraction coeff for ocean (S-1)
REAL :: XTH00              ! reference value  for the potential temperature
REAL :: XTH00OCEAN         ! Ref value for pot temp in ocean model
REAL :: XSA00OCEAN         ! Ref value for SAlinity in ocean model
REAL :: XROC=0.69! 3 coeffs for SW penetration in  Ocean (Hoecker et al)
REAL :: XD1=1.1
REAL :: XD2=23.
! Values used in SURFEX CMO
!REAL :: XROC=0.58
!REAL :: XD1=0.35
!REAL :: XD2=23.

REAL :: XRHOLI             ! Volumic mass of liquid water
!
INTEGER :: NDAYSEC        ! Number of seconds in a day
!
REAL :: RDSRV              !  XRD/XRV
REAL :: RDSCPD             !  XRD/XCPD
REAL :: RINVXP00           !  1./XP00
!
!   Some machine precision value depending of real4/8 use  
!
REAL :: XMNH_TINY          ! minimum real on this machine
REAL :: XMNH_TINY_12       ! sqrt(minimum real on this machine)
REAL :: XMNH_EPSILON       ! minimum space with 1.0
REAL :: XMNH_HUGE          ! maximum real on this machine
REAL :: XMNH_HUGE_12_LOG   ! maximum log(sqrt(real)) on this machine

REAL :: XEPS_DT            ! default value for DT test 
REAL :: XRES_FLAT_CART     ! default     flat&cart residual tolerance
REAL :: XRES_OTHER         ! default not flat&cart residual tolerance
REAL :: XRES_PREP          ! default     prep      residual tolerance
END TYPE CST_t

TYPE(CST_t), TARGET, SAVE :: CST

REAL, POINTER :: XPI
REAL, POINTER :: XDAY, XSIYEA, XSIDAY
REAL, POINTER :: XKARMAN
REAL, POINTER :: XLIGHTSPEED
REAL, POINTER :: XPLANCK
REAL, POINTER :: XBOLTZ
REAL, POINTER :: XAVOGADRO
REAL, POINTER :: XRADIUS, XOMEGA
REAL, POINTER :: XG
REAL, POINTER :: XP00
REAL, POINTER :: XP00OCEAN
REAL, POINTER :: XRH00OCEAN
REAL, POINTER :: XSTEFAN, XI0
REAL, POINTER :: XMD, XMV
REAL, POINTER :: XRD, XRV
REAL, POINTER :: XEPSILO
REAL, POINTER :: XCPD, XCPV
REAL, POINTER :: XRHOLW
REAL, POINTER :: XCL, XCI
REAL, POINTER :: XTT
REAL, POINTER :: XLVTT
REAL, POINTER :: XLSTT
REAL, POINTER :: XLMTT
REAL, POINTER :: XESTT
REAL, POINTER :: XALPW, XBETAW, XGAMW
REAL, POINTER :: XALPI, XBETAI, XGAMI
REAL, POINTER :: XCONDI
REAL, POINTER :: XALPHAOC
REAL, POINTER :: XBETAOC
REAL, POINTER :: XTH00
REAL, POINTER :: XTH00OCEAN
REAL, POINTER :: XSA00OCEAN
REAL, POINTER :: XROC
REAL, POINTER :: XD1
REAL, POINTER :: XD2
REAL, POINTER :: XRHOLI
INTEGER, POINTER :: NDAYSEC
REAL, POINTER :: RDSRV
REAL, POINTER :: RDSCPD
REAL, POINTER :: RINVXP00
REAL, POINTER :: XMNH_TINY
REAL, POINTER :: XMNH_TINY_12
REAL, POINTER :: XMNH_EPSILON
REAL, POINTER :: XMNH_HUGE
REAL, POINTER :: XMNH_HUGE_12_LOG
REAL, POINTER :: XEPS_DT
REAL, POINTER :: XRES_FLAT_CART
REAL, POINTER :: XRES_OTHER
REAL, POINTER :: XRES_PREP
!
END MODULE MODD_CST
