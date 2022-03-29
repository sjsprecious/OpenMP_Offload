!KGEN-generated Fortran source file 
  
!Generated at : 2021-06-24 13:54:09 
!KGEN version : 0.9.0 
  


MODULE shr_kind_mod
  !----------------------------------------------------------------------------
  ! precision/kind constants add data public
  !----------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 

    PUBLIC 
  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real

END MODULE shr_kind_mod
