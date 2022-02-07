!MNH_LIC Copyright 2006-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
      ELEMENTAL SUBROUTINE COMPUTE_FRAC_ICE(HFRAC_ICE,NEB,PFRAC_ICE,PT,KERR)

! ******* TO BE INCLUDED IN THE *CONTAINS* OF A SUBROUTINE, IN ORDER TO EASE AUTOMATIC INLINING ******
! => Don't use drHook !!!
!
!!****  *COMPUTE_FRAC_ICE* - computes ice fraction
!
!!    AUTHOR
!!    ------
!!      Julien PERGAUD      * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         13/03/06
!!      S. Riette        April 2011 optimisation
!!      S. Riette        08/2016 add option O
!!      R. El Khatib     12-Aug-2021 written as a include file
!
!! --------------------------------------------------------------------------
USE MODD_NEB, ONLY : NEB_t
USE MODD_CST, ONLY : XTT
!
IMPLICIT NONE
!
CHARACTER(LEN=1), INTENT(IN)    :: HFRAC_ICE       ! scheme to use
TYPE(NEB_t),      INTENT(IN)    :: NEB
REAL,             INTENT(IN)    :: PT              ! temperature
REAL,             INTENT(INOUT) :: PFRAC_ICE       ! Ice fraction (1 for ice only, 0 for liquid only)
INTEGER,          INTENT(OUT)   :: KERR            ! Error code in return
!
!------------------------------------------------------------------------

!                1. Compute FRAC_ICE
!
KERR=0
SELECT CASE(HFRAC_ICE)
  CASE ('T') !using Temperature
    PFRAC_ICE = MAX( 0., MIN(1., (( NEB%XTMAXMIX - PT ) / ( NEB%XTMAXMIX - NEB%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    PFRAC_ICE = MAX( 0., MIN(1., (( XTT - PT ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    PFRAC_ICE = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    PFRAC_ICE = MAX( 0., MIN(1., PFRAC_ICE ) )
  CASE DEFAULT
    KERR=1
END SELECT

END SUBROUTINE COMPUTE_FRAC_ICE
