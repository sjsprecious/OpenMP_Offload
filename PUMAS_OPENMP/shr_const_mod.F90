!KGEN-generated Fortran source file 
  
!Generated at : 2021-06-24 13:54:09 
!KGEN version : 0.9.0 
  
!===============================================================================
! SVN $Id: shr_const_mod.F90 61510 2014-06-26 21:58:56Z tcraig $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/trunk_tags/share3_150116/shr/shr_const_mod.F90 $
!===============================================================================


MODULE shr_const_mod

    USE shr_kind_mod, ONLY: r8 => shr_kind_r8 
   !----------------------------------------------------------------------------
   ! physical constants (all data public)
   !----------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 

    PRIVATE r8 
    PUBLIC 

   real(R8),parameter :: SHR_CONST_PI      = 3.14159265358979323846_R8  ! pi


   ! sub-ice-shelf ocean cavities ~ C
   ! pressure in the freezing temperature in sub-ice-shelf ocean cavities. ~ C Pa^{-1}
   ! the freezing temperature in sub-ice-ice ocean cavities ~ C PSU^{-1}
   ! pressure in the freezing temperature in sub-ice-shelf ocean cavities ~ C PSU^{-1} Pa^{-1}

   !Water Isotope Ratios in Vienna Standard Mean Ocean Water (VSMOW):

   ! For best numerics in CAM5

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


END MODULE shr_const_mod
