!KGEN-generated Fortran source file 
  
!Generated at : 2021-06-24 13:54:09 
!KGEN version : 0.9.0 
  


module micro_mg_cam
!---------------------------------------------------------------------------------
!  CAM Interfaces for MG microphysics
!---------------------------------------------------------------------------------
! How to add new packed MG inputs to micro_mg_cam_tend:
! If you have an input with first dimension [psetcols, pver], the procedure
! for adding inputs is as follows:
! 1) In addition to any variables you need to declare for the "unpacked"
!    (CAM format) version, you must declare an array for the "packed" 
!    (MG format) version.
! 2) Add a call similar to the following line (look before the
!    micro_mg_tend calls to see similar lines):
!      packed_array = packer%pack(original_array)
!    The packed array can then be passed into any of the MG schemes.
! This same procedure will also work for 1D arrays of size psetcols, 3-D
! arrays with psetcols and pver as the first dimensions, and for arrays of
! dimension [psetcols, pverp]. You only have to modify the allocation of
! the packed array before the "pack" call.
!---------------------------------------------------------------------------------
! How to add new packed MG outputs to micro_mg_cam_tend:
! 1) As with inputs, in addition to the unpacked outputs you must declare
!    an array for packed data. The unpacked and packed arrays must *also* 
!    be targets or pointers (but cannot be both).
! 2) Add the field to post-processing as in the following line (again,
!    there are many examples before the micro_mg_tend calls):
!      call post_proc%add_field(p(final_array),p(packed_array))
!    *** IMPORTANT ** If the fields are only being passed to a certain version of
!    MG, you must only add them if that version is being called (see
!    the "if (micro_mg_version >1)" sections below
!    This registers the field for post-MG averaging, and to scatter to the
!    final, unpacked version of the array.
!    By default, any columns/levels that are not operated on by MG will be
!    set to 0 on output; this value can be adjusted using the "fillvalue"
!    optional argument to post_proc%add_field.
!    Also by default, outputs from multiple substeps will be averaged after
!    MG's substepping is complete. Passing the optional argument
!    "accum_method=accum_null" will change this behavior so that the last
!    substep is always output.
! This procedure works on 1-D and 2-D outputs. Note that the final,
! unpacked arrays are not set until the call to
! "post_proc%process_and_unpack", which sets every single field that was
! added with post_proc%add_field.
!---------------------------------------------------------------------------------

!
!
!
!
!
!
!
!
!
!
!
!
!
!
!  
!
!
!
!
!

    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
#if defined(__OPENACC__)
    USE openacc
#endif

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

#if defined(__OPENACC__)
    integer :: ngpus,gpunum
#endif

#ifdef _MPI
    include 'mpif.h'
#endif

integer :: num_steps ! Number of MG substeps

! Namelist variables for option to specify constant cloud droplet/ice number

! parameters for specified ice and droplet number concentration
! note: these are local in-cloud values, not grid-mean
      

! switches for IFS like behavior


! Physics buffer indices for fields registered by this module

! Fields for UNICON

! Fields needed as inputs to COSP

! Fields needed by Park macrophysics

! Used to replace aspects of MG microphysics
! (e.g. by CARMA)

! Index fields for precipitation efficiency.

! Physics buffer indices for fields registered by other modules

! Pbuf fields needed for subcol_SILHS


! pbuf fields for heterogeneous freezing


!===============================================================================
PUBLIC kr_externs_in_micro_mg_cam 
PUBLIC kr_externs_out_micro_mg_cam 
PUBLIC micro_mg_cam_tend_pack 


contains
!===============================================================================


!================================================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


SUBROUTINE micro_mg_cam_tend_pack(kgen_unit, kgen_measure, kgen_isverified, kgen_filepath, dtime, nlev, mgncol) 


    USE micro_mg3_0, ONLY: micro_mg_tend3_0 => micro_mg_tend 

!   use perf_mod
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE micro_mg3_0, ONLY: kr_externs_out_micro_mg3_0 
    USE micro_mg_utils, ONLY: kr_externs_out_micro_mg_utils,mg_liq_props,mg_ice_props, &
    &mg_rain_props,mg_graupel_props,mg_hail_props,mg_snow_props
    USE wv_sat_methods, ONLY: kr_externs_out_wv_sat_methods 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_init_verify, kgen_tolerance, kgen_minvalue, kgen_verboselevel, &
    &CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 

    REAL(KIND=r8), INTENT(INOUT) :: dtime 

    INTEGER, INTENT(INOUT) :: nlev 
    INTEGER, INTENT(INOUT) :: mgncol 
   ! Local variables


!Hail/Graupel Output 
!Hail/Graupel Process Rates                
   ! Object that packs columns with clouds/precip.

   ! Packed versions of inputs.

    REAL(KIND=r8) :: packed_t(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_q(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_qc(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_nc(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_qi(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_ni(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_qr(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_nr(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_qs(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_ns(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_qg(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_ng(DFACT*mgncol,nlev) 

    REAL(KIND=r8) :: packed_relvar(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_accre_enhan(DFACT*mgncol,nlev) 

    REAL(KIND=r8) :: packed_p(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_pdel(DFACT*mgncol,nlev) 

    REAL(KIND=r8) :: packed_cldn(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_liqcldf(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_icecldf(DFACT*mgncol,nlev) 
    REAL(KIND=r8), allocatable :: packed_qsatfac(:,:) 

    REAL(KIND=r8) :: packed_naai(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_npccn(DFACT*mgncol,nlev) 

    REAL(KIND=r8), allocatable :: packed_rndst(:,:,:) 
    REAL(KIND=r8), allocatable :: packed_nacon(:,:,:) 
   ! Optional outputs.

    REAL(KIND=r8) :: packed_tnd_qsnow(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_tnd_nsnow(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_re_ice(DFACT*mgncol,nlev) 

    REAL(KIND=r8) :: packed_frzimm(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_frzcnt(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: packed_frzdep(DFACT*mgncol,nlev) 
   ! Output field post-processing.

   ! Packed versions of outputs.

    REAL(KIND=r8), target :: packed_rate1ord_cw2pr_st(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_tlat(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qvlat(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qctend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qitend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nctend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nitend(DFACT*mgncol,nlev) 

    REAL(KIND=r8), target :: packed_qrtend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qstend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nrtend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nstend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qgtend(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ngtend(DFACT*mgncol,nlev) 

    REAL(KIND=r8), target :: packed_prect(DFACT*mgncol) 
    REAL(KIND=r8), target :: packed_preci(DFACT*mgncol) 
    REAL(KIND=r8), target :: packed_nevapr(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_am_evp_st(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_evapsnow(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prain(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prodsnow(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_cmeout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qsout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_cflx(DFACT*mgncol,nlev+1) 
    REAL(KIND=r8), target :: packed_iflx(DFACT*mgncol,nlev+1) 
    REAL(KIND=r8), target :: packed_rflx(DFACT*mgncol,nlev+1) 
    REAL(KIND=r8), target :: packed_sflx(DFACT*mgncol,nlev+1) 
    REAL(KIND=r8), target :: packed_gflx(DFACT*mgncol,nlev+1) 
    REAL(KIND=r8), target :: packed_qrout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qcsevap(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qisevap(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qvres(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_cmei(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_vtrmc(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_vtrmi(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qcsedten(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qisedten(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qrsedten(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qssedten(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qgsedten(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_umg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_umr(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ums(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_pra(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prc(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mnuccc(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mnucct(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_msacwi(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_psacws(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_bergs(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_berg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_melt(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_homo(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qcres(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prci(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prai(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qires(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mnuccr(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mnuccri(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mnudeptot(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_meltgtot(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_meltstot(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_pracs(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_meltsdt(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_frzrdt(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mnuccd(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nrout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nsout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_refl(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_arefl(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_areflz(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_frefl(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_csrfl(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_acsrfl(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_fcsrfl(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_rercld(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ncai(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ncal(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qrout2(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qsout2(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nrout2(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nsout2(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_freqs(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_freqr(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_freqg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nfice(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prer_evap(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qcrat(DFACT*mgncol,nlev) 

    REAL(KIND=r8), target :: packed_rel(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_rei(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_sadice(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_sadsnow(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_lambdac(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_mu(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_des(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_dei(DFACT*mgncol,nlev) 
!Hail/Graupel Output

    REAL(KIND=r8), target :: packed_qgout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ngout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_dgout(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qgout2(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ngout2(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_dgout2(DFACT*mgncol,nlev) 
!Hail/Graupel Process Rates                
    REAL(KIND=r8), target :: packed_psacr(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_pracg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_psacwg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_pgsacw(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_pgracs(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_prdg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qmultg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_qmultrg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_npracg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nscng(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_ngracs(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nmultg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_nmultrg(DFACT*mgncol,nlev) 
    REAL(KIND=r8), target :: packed_npsacwg(DFACT*mgncol,nlev) 
   ! Dummy arrays for cases where we throw away the MG version and
   ! recalculate sizes on the CAM grid to avoid time/subcolumn averaging
   ! issues.

    REAL(KIND=r8) :: rel_fn_dum(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: dsout2_dum(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: drout_dum(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: reff_rain_dum(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: reff_snow_dum(DFACT*mgncol,nlev) 
    REAL(KIND=r8) :: reff_grau_dum(DFACT*mgncol,nlev) 
   ! Heterogeneous-only version of mnuccdo.

   ! physics buffer fields for COSP simulator

   ! physics buffer fields used with CARMA


                                               ! strat. cloud water to precip (1/s)    ! rce 2010/05/01


   ! variables for heterogeneous freezing


   ! A local copy of state is used for diagnostic calculations


   ! Averaging arrays for effective radius and number....

!  Averaging arrays for supercooled liquid


   ! Cloud fraction used for precipitation.

   ! Average cloud top radius & number


   ! Variables for precip efficiency calculation


   ! Variables for liquid water path and column condensation


   ! variables for autoconversion and accretion vertical averages


   


    CHARACTER(LEN=128) :: errstring 
   ! For rrtmg optics. specified distribution.


   !-------------------------------------------------------------------------------
    INTEGER, INTENT(IN) :: kgen_unit 
    REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
    LOGICAL, INTENT(OUT) :: kgen_isverified 
    CHARACTER(LEN=*), INTENT(IN) :: kgen_filepath 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: kgen_intvar, kgen_ierr 
    INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
    LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    COMMON / kgen_state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    INTEGER, PARAMETER :: KGEN_MAXITER = 500 
      
    TYPE(check_t) :: check_status 
    INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
    REAL(KIND=kgen_dp) :: gkgen_measure 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_rate1ord_cw2pr_st 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_tlat 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qvlat 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qctend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qitend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nctend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nitend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qrtend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qstend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nrtend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nstend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qgtend 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ngtend 
    REAL(KIND=r8), dimension(mgncol) :: kgenref_packed_prect 
    REAL(KIND=r8), dimension(mgncol) :: kgenref_packed_preci 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nevapr 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_am_evp_st 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_evapsnow 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prain 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prodsnow 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_cmeout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qsout 
    REAL(KIND=r8), dimension(mgncol,nlev+1) :: kgenref_packed_cflx 
    REAL(KIND=r8), dimension(mgncol,nlev+1) :: kgenref_packed_iflx 
    REAL(KIND=r8), dimension(mgncol,nlev+1) :: kgenref_packed_rflx 
    REAL(KIND=r8), dimension(mgncol,nlev+1) :: kgenref_packed_sflx 
    REAL(KIND=r8), dimension(mgncol,nlev+1) :: kgenref_packed_gflx 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qrout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qcsevap 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qisevap 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qvres 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_cmei 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_vtrmc 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_vtrmi 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qcsedten 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qisedten 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qrsedten 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qssedten 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qgsedten 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_umg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_umr 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ums 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_pra 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prc 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mnuccc 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mnucct 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_msacwi 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_psacws 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_bergs 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_berg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_melt 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_homo 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qcres 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prci 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prai 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qires 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mnuccr 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mnuccri 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mnudeptot 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_meltgtot 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_meltstot 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_pracs 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_meltsdt 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_frzrdt 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mnuccd 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nrout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nsout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_refl 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_arefl 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_areflz 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_frefl 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_csrfl 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_acsrfl 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_fcsrfl 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_rercld 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ncai 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ncal 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qrout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qsout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nrout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nsout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_freqs 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_freqr 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_freqg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nfice 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prer_evap 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qcrat 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_rel 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_rei 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_sadice 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_sadsnow 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_lambdac 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_mu 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_des 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_dei 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qgout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ngout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_dgout 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qgout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ngout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_dgout2 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_psacr 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_pracg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_psacwg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_pgsacw 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_pgracs 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_prdg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qmultg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_qmultrg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_npracg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nscng 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_ngracs 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nmultg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_nmultrg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_packed_npsacwg 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_rel_fn_dum 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_dsout2_dum 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_drout_dum 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_reff_rain_dum 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_reff_snow_dum 
    REAL(KIND=r8), dimension(mgncol,nlev) :: kgenref_reff_grau_dum 
    CHARACTER(LEN=128) :: kgenref_errstring 

#ifdef _MPI
    integer :: myrank, mpisize, info
    call mpi_comm_rank(mpi_comm_world, myrank, info)
    call mpi_comm_size(mpi_comm_world, mpisize, info)
#else
    integer :: myrank
    !parent block preprocessing 
    kgen_mpirank = 0 
    myrank = 0
#endif

    !local input variables 
    call read2Darray(kgen_unit,"packed_t",mgncol,nlev,packed_t)
    call read2Darray(kgen_unit,"packed_q",mgncol,nlev,packed_q)
    call read2Darray(kgen_unit,"packed_qc",mgncol,nlev,packed_qc)
    call read2Darray(kgen_unit,"packed_nc",mgncol,nlev,packed_nc)
    call read2Darray(kgen_unit,"packed_qi",mgncol,nlev,packed_qi)
    call read2Darray(kgen_unit,"packed_ni",mgncol,nlev,packed_ni)
    call read2Darray(kgen_unit,"packed_qr",mgncol,nlev,packed_qr)
    call read2Darray(kgen_unit,"packed_nr",mgncol,nlev,packed_nr)
    call read2Darray(kgen_unit,"packed_qs",mgncol,nlev,packed_qs)
    call read2Darray(kgen_unit,"packed_ns",mgncol,nlev,packed_ns)
    call read2Darray(kgen_unit,"packed_qg",mgncol,nlev,packed_qg)
    call read2Darray(kgen_unit,"packed_ng",mgncol,nlev,packed_ng)
    call read2Darray(kgen_unit,"packed_relvar",mgncol,nlev,packed_relvar)
    call read2Darray(kgen_unit,"packed_accre_enhan",mgncol,nlev,packed_accre_enhan)
    call read2Darray(kgen_unit,"packed_p",mgncol,nlev,packed_p)
    call read2Darray(kgen_unit,"packed_pdel",mgncol,nlev,packed_pdel)
    call read2Darray(kgen_unit,"packed_cldn",mgncol,nlev,packed_cldn)
    call read2Darray(kgen_unit,"packed_liqcldf",mgncol,nlev,packed_liqcldf)
    call read2Darray(kgen_unit,"packed_icecldf",mgncol,nlev,packed_icecldf)

    call kr_micro_mg_cam_tend_pack_real__r8_dim2(packed_qsatfac, kgen_unit, "packed_qsatfac", .FALSE.) 

    call read2Darray(kgen_unit,"packed_naai",mgncol,nlev,packed_naai)
    call read2Darray(kgen_unit,"packed_npccn",mgncol,nlev,packed_npccn)

    call  kr_micro_mg_cam_tend_pack_real__r8_dim3(packed_rndst, kgen_unit, "packed_rndst", .FALSE.) 
    call  kr_micro_mg_cam_tend_pack_real__r8_dim3(packed_nacon, kgen_unit, "packed_nacon", .FALSE.) 

    call read2Darray(kgen_unit,"packed_tnd_qsnow",mgncol,nlev,packed_tnd_qsnow)
    call read2Darray(kgen_unit,"packed_tnd_nsnow",mgncol,nlev,packed_tnd_nsnow)
    call read2Darray(kgen_unit,"packed_re_ice",mgncol,nlev,packed_re_ice)
    call read2Darray(kgen_unit,"packed_frzimm",mgncol,nlev,packed_frzimm)
    call read2Darray(kgen_unit,"packed_frzcnt",mgncol,nlev,packed_frzcnt)
    call read2Darray(kgen_unit,"packed_frzdep",mgncol,nlev,packed_frzdep)
    call read2Darray(kgen_unit,"packed_rate1ord_cw2pr_st",mgncol,nlev,packed_rate1ord_cw2pr_st)
    call read2Darray(kgen_unit,"packed_tlat",mgncol,nlev,packed_tlat)
    call read2Darray(kgen_unit,"packed_qvlat",mgncol,nlev,packed_qvlat)
    call read2Darray(kgen_unit,"packed_qctend",mgncol,nlev,packed_qctend)
    call read2Darray(kgen_unit,"packed_qitend",mgncol,nlev,packed_qitend)
    call read2Darray(kgen_unit,"packed_nctend",mgncol,nlev,packed_nctend)
    call read2Darray(kgen_unit,"packed_nitend",mgncol,nlev,packed_nitend)
    call read2Darray(kgen_unit,"packed_qrtend",mgncol,nlev,packed_qrtend)
    call read2Darray(kgen_unit,"packed_qstend",mgncol,nlev,packed_qstend)
    call read2Darray(kgen_unit,"packed_nrtend",mgncol,nlev,packed_nrtend)
    call read2Darray(kgen_unit,"packed_nstend",mgncol,nlev,packed_nstend)
    call read2Darray(kgen_unit,"packed_qgtend",mgncol,nlev,packed_qgtend)
    call read2Darray(kgen_unit,"packed_ngtend",mgncol,nlev,packed_ngtend)

    call read1Darray(kgen_unit,"packed_prect",mgncol,packed_prect)
    call read1Darray(kgen_unit,"packed_preci",mgncol,packed_preci)

    call read2Darray(kgen_unit,"packed_nevapr",mgncol,nlev,packed_nevapr)
    call read2Darray(kgen_unit,"packed_am_evp_st",mgncol,nlev,packed_am_evp_st)
    call read2Darray(kgen_unit,"packed_evapsnow",mgncol,nlev,packed_evapsnow)
    call read2Darray(kgen_unit,"packed_prain",mgncol,nlev,packed_prain)
    call read2Darray(kgen_unit,"packed_prodsnow",mgncol,nlev,packed_prodsnow)
    call read2Darray(kgen_unit,"packed_cmeout",mgncol,nlev,packed_cmeout)
    call read2Darray(kgen_unit,"packed_qsout",mgncol,nlev,packed_qsout)
    call read2Darray(kgen_unit,"packed_cflx",mgncol,nlev+1,packed_cflx)
    call read2Darray(kgen_unit,"packed_iflx",mgncol,nlev+1,packed_iflx)
    call read2Darray(kgen_unit,"packed_rflx",mgncol,nlev+1,packed_rflx)
    call read2Darray(kgen_unit,"packed_sflx",mgncol,nlev+1,packed_sflx)
    call read2Darray(kgen_unit,"packed_gflx",mgncol,nlev+1,packed_gflx)
    call read2Darray(kgen_unit,"packed_qrout",mgncol,nlev,packed_qrout)
    call read2Darray(kgen_unit,"packed_qcsevap",mgncol,nlev,packed_qcsevap)
    call read2Darray(kgen_unit,"packed_qisevap",mgncol,nlev,packed_qisevap)
    call read2Darray(kgen_unit,"packed_qvres",mgncol,nlev,packed_qvres)
    call read2Darray(kgen_unit,"packed_cmei",mgncol,nlev,packed_cmei)
    call read2Darray(kgen_unit,"packed_vtrmc",mgncol,nlev,packed_vtrmc)
    call read2Darray(kgen_unit,"packed_vtrmi",mgncol,nlev,packed_vtrmi)
    call read2Darray(kgen_unit,"packed_qcsedten",mgncol,nlev,packed_qcsedten)
    call read2Darray(kgen_unit,"packed_qisedten",mgncol,nlev,packed_qisedten)
    call read2Darray(kgen_unit,"packed_qrsedten",mgncol,nlev,packed_qrsedten)
    call read2Darray(kgen_unit,"packed_qssedten",mgncol,nlev,packed_qssedten)
    call read2Darray(kgen_unit,"packed_qgsedten",mgncol,nlev,packed_qgsedten)
    call read2Darray(kgen_unit,"packed_umg",mgncol,nlev,packed_umg)
    call read2Darray(kgen_unit,"packed_umr",mgncol,nlev,packed_umr)
    call read2Darray(kgen_unit,"packed_ums",mgncol,nlev,packed_ums)
    call read2Darray(kgen_unit,"packed_pra",mgncol,nlev,packed_pra)
    call read2Darray(kgen_unit,"packed_prc",mgncol,nlev,packed_prc)
    call read2Darray(kgen_unit,"packed_mnuccc",mgncol,nlev,packed_mnuccc)
    call read2Darray(kgen_unit,"packed_mnucct",mgncol,nlev,packed_mnucct)
    call read2Darray(kgen_unit,"packed_msacwi",mgncol,nlev,packed_msacwi)
    call read2Darray(kgen_unit,"packed_psacws",mgncol,nlev,packed_psacws)
    call read2Darray(kgen_unit,"packed_bergs",mgncol,nlev,packed_bergs)
    call read2Darray(kgen_unit,"packed_berg",mgncol,nlev,packed_berg)
    call read2Darray(kgen_unit,"packed_melt",mgncol,nlev,packed_melt)
    call read2Darray(kgen_unit,"packed_homo",mgncol,nlev,packed_homo)
    call read2Darray(kgen_unit,"packed_qcres",mgncol,nlev,packed_qcres)
    call read2Darray(kgen_unit,"packed_prci",mgncol,nlev,packed_prci)
    call read2Darray(kgen_unit,"packed_prai",mgncol,nlev,packed_prai)
    call read2Darray(kgen_unit,"packed_qires",mgncol,nlev,packed_qires)
    call read2Darray(kgen_unit,"packed_mnuccr",mgncol,nlev,packed_mnuccr)
    call read2Darray(kgen_unit,"packed_mnuccri",mgncol,nlev,packed_mnuccri)
    call read2Darray(kgen_unit,"packed_mnudeptot",mgncol,nlev,packed_mnudeptot)
    call read2Darray(kgen_unit,"packed_meltgtot",mgncol,nlev,packed_meltgtot)
    call read2Darray(kgen_unit,"packed_meltstot",mgncol,nlev,packed_meltstot)
    call read2Darray(kgen_unit,"packed_pracs",mgncol,nlev,packed_pracs)
    call read2Darray(kgen_unit,"packed_meltsdt",mgncol,nlev,packed_meltsdt)
    call read2Darray(kgen_unit,"packed_frzrdt",mgncol,nlev,packed_frzrdt)
    call read2Darray(kgen_unit,"packed_mnuccd",mgncol,nlev,packed_mnuccd)
    call read2Darray(kgen_unit,"packed_nrout",mgncol,nlev,packed_nrout)
    call read2Darray(kgen_unit,"packed_nsout",mgncol,nlev,packed_nsout)
    call read2Darray(kgen_unit,"packed_refl",mgncol,nlev,packed_refl)
    call read2Darray(kgen_unit,"packed_arefl",mgncol,nlev,packed_arefl)
    call read2Darray(kgen_unit,"packed_areflz",mgncol,nlev,packed_areflz)
    call read2Darray(kgen_unit,"packed_frefl",mgncol,nlev,packed_frefl)
    call read2Darray(kgen_unit,"packed_csrfl",mgncol,nlev,packed_csrfl)
    call read2Darray(kgen_unit,"packed_acsrfl",mgncol,nlev,packed_acsrfl)
    call read2Darray(kgen_unit,"packed_fcsrfl",mgncol,nlev,packed_fcsrfl)
    call read2Darray(kgen_unit,"packed_rercld",mgncol,nlev,packed_rercld)
    call read2Darray(kgen_unit,"packed_ncai",mgncol,nlev,packed_ncai)
    call read2Darray(kgen_unit,"packed_ncal",mgncol,nlev,packed_ncal)

    call read2Darray(kgen_unit,"packed_qrout2",mgncol,nlev,packed_qrout2)
    call read2Darray(kgen_unit,"packed_qsout2",mgncol,nlev,packed_qsout2)
    call read2Darray(kgen_unit,"packed_nrout2",mgncol,nlev,packed_nrout2)
    call read2Darray(kgen_unit,"packed_nsout2",mgncol,nlev,packed_nsout2)
    call read2Darray(kgen_unit,"packed_freqs",mgncol,nlev,packed_freqs)
    call read2Darray(kgen_unit,"packed_freqr",mgncol,nlev,packed_freqr)
    call read2Darray(kgen_unit,"packed_freqg",mgncol,nlev,packed_freqg)
    call read2Darray(kgen_unit,"packed_nfice",mgncol,nlev,packed_nfice)
    call read2Darray(kgen_unit,"packed_prer_evap",mgncol,nlev,packed_prer_evap)
    call read2Darray(kgen_unit,"packed_qcrat",mgncol,nlev,packed_qcrat)
    call read2Darray(kgen_unit,"packed_rel",mgncol,nlev,packed_rel)
    call read2Darray(kgen_unit,"packed_rei",mgncol,nlev,packed_rei)
    call read2Darray(kgen_unit,"packed_sadice",mgncol,nlev,packed_sadice)
    call read2Darray(kgen_unit,"packed_sadsnow",mgncol,nlev,packed_sadsnow)
    call read2Darray(kgen_unit,"packed_lambdac",mgncol,nlev,packed_lambdac)
    call read2Darray(kgen_unit,"packed_mu",mgncol,nlev,packed_mu)
    call read2Darray(kgen_unit,"packed_des",mgncol,nlev,packed_des)
    call read2Darray(kgen_unit,"packed_dei",mgncol,nlev,packed_dei)
    call read2Darray(kgen_unit,"packed_qgout",mgncol,nlev,packed_qgout)
    call read2Darray(kgen_unit,"packed_ngout",mgncol,nlev,packed_ngout)
    call read2Darray(kgen_unit,"packed_dgout",mgncol,nlev,packed_dgout)
    call read2Darray(kgen_unit,"packed_qgout2",mgncol,nlev,packed_qgout2)
    call read2Darray(kgen_unit,"packed_ngout2",mgncol,nlev,packed_ngout2)
    call read2Darray(kgen_unit,"packed_dgout2",mgncol,nlev,packed_dgout2)
    call read2Darray(kgen_unit,"packed_psacr",mgncol,nlev,packed_psacr)
    call read2Darray(kgen_unit,"packed_pracg",mgncol,nlev,packed_pracg)
    call read2Darray(kgen_unit,"packed_psacwg",mgncol,nlev,packed_psacwg)
    call read2Darray(kgen_unit,"packed_pgsacw",mgncol,nlev,packed_pgsacw)
    call read2Darray(kgen_unit,"packed_pgracs",mgncol,nlev,packed_pgracs)
    call read2Darray(kgen_unit,"packed_prdg",mgncol,nlev,packed_prdg)
    call read2Darray(kgen_unit,"packed_qmultg",mgncol,nlev,packed_qmultg)
    call read2Darray(kgen_unit,"packed_qmultrg",mgncol,nlev,packed_qmultrg)
    call read2Darray(kgen_unit,"packed_npracg",mgncol,nlev,packed_npracg)
    call read2Darray(kgen_unit,"packed_nscng",mgncol,nlev,packed_nscng)
    call read2Darray(kgen_unit,"packed_ngracs",mgncol,nlev,packed_ngracs)
    call read2Darray(kgen_unit,"packed_nmultg",mgncol,nlev,packed_nmultg)
    call read2Darray(kgen_unit,"packed_nmultrg",mgncol,nlev,packed_nmultrg)
    call read2Darray(kgen_unit,"packed_npsacwg",mgncol,nlev,packed_npsacwg)
    call read2Darray(kgen_unit,"rel_fn_dum",mgncol,nlev,rel_fn_dum)
    call read2Darray(kgen_unit,"dsout2_dum",mgncol,nlev,dsout2_dum)
    call read2Darray(kgen_unit,"drout_dum",mgncol,nlev,drout_dum)
    call read2Darray(kgen_unit,"reff_rain_dum",mgncol,nlev,reff_rain_dum)
    call read2Darray(kgen_unit,"reff_snow_dum",mgncol,nlev,reff_snow_dum)
    call read2Darray(kgen_unit,"reff_grau_dum",mgncol,nlev,reff_snow_dum)

    READ (UNIT = kgen_unit) errstring 
      
    !extern output variables 
    CALL kr_externs_out_micro_mg_cam(kgen_unit) 
    CALL kr_externs_out_micro_mg3_0(kgen_unit) 
    CALL kr_externs_out_micro_mg_utils(kgen_unit) 
    CALL kr_externs_out_wv_sat_methods(kgen_unit) 
      
    !local output variables 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rate1ord_cw2pr_st 
        CALL kgen_array_sumcheck("kgenref_packed_rate1ord_cw2pr_st", kgen_array_sum, DBLE(SUM(kgenref_packed_rate1ord_cw2pr_st, &
        &mask=(kgenref_packed_rate1ord_cw2pr_st .eq. kgenref_packed_rate1ord_cw2pr_st))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_tlat 
        CALL kgen_array_sumcheck("kgenref_packed_tlat", kgen_array_sum, DBLE(SUM(kgenref_packed_tlat, mask=(kgenref_packed_tlat &
        &.eq. kgenref_packed_tlat))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qvlat 
        CALL kgen_array_sumcheck("kgenref_packed_qvlat", kgen_array_sum, DBLE(SUM(kgenref_packed_qvlat, &
        &mask=(kgenref_packed_qvlat .eq. kgenref_packed_qvlat))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qctend 
        CALL kgen_array_sumcheck("kgenref_packed_qctend", kgen_array_sum, DBLE(SUM(kgenref_packed_qctend, &
        &mask=(kgenref_packed_qctend .eq. kgenref_packed_qctend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qitend 
        CALL kgen_array_sumcheck("kgenref_packed_qitend", kgen_array_sum, DBLE(SUM(kgenref_packed_qitend, &
        &mask=(kgenref_packed_qitend .eq. kgenref_packed_qitend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nctend 
        CALL kgen_array_sumcheck("kgenref_packed_nctend", kgen_array_sum, DBLE(SUM(kgenref_packed_nctend, &
        &mask=(kgenref_packed_nctend .eq. kgenref_packed_nctend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nitend 
        CALL kgen_array_sumcheck("kgenref_packed_nitend", kgen_array_sum, DBLE(SUM(kgenref_packed_nitend, &
        &mask=(kgenref_packed_nitend .eq. kgenref_packed_nitend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrtend 
        CALL kgen_array_sumcheck("kgenref_packed_qrtend", kgen_array_sum, DBLE(SUM(kgenref_packed_qrtend, &
        &mask=(kgenref_packed_qrtend .eq. kgenref_packed_qrtend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qstend 
        CALL kgen_array_sumcheck("kgenref_packed_qstend", kgen_array_sum, DBLE(SUM(kgenref_packed_qstend, &
        &mask=(kgenref_packed_qstend .eq. kgenref_packed_qstend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nrtend 
        CALL kgen_array_sumcheck("kgenref_packed_nrtend", kgen_array_sum, DBLE(SUM(kgenref_packed_nrtend, &
        &mask=(kgenref_packed_nrtend .eq. kgenref_packed_nrtend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nstend 
        CALL kgen_array_sumcheck("kgenref_packed_nstend", kgen_array_sum, DBLE(SUM(kgenref_packed_nstend, &
        &mask=(kgenref_packed_nstend .eq. kgenref_packed_nstend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qgtend 
        CALL kgen_array_sumcheck("kgenref_packed_qgtend", kgen_array_sum, DBLE(SUM(kgenref_packed_qgtend, &
        &mask=(kgenref_packed_qgtend .eq. kgenref_packed_qgtend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ngtend 
        CALL kgen_array_sumcheck("kgenref_packed_ngtend", kgen_array_sum, DBLE(SUM(kgenref_packed_ngtend, &
        &mask=(kgenref_packed_ngtend .eq. kgenref_packed_ngtend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prect 
        CALL kgen_array_sumcheck("kgenref_packed_prect", kgen_array_sum, DBLE(SUM(kgenref_packed_prect, &
        &mask=(kgenref_packed_prect .eq. kgenref_packed_prect))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_preci 
        CALL kgen_array_sumcheck("kgenref_packed_preci", kgen_array_sum, DBLE(SUM(kgenref_packed_preci, &
        &mask=(kgenref_packed_preci .eq. kgenref_packed_preci))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nevapr 
        CALL kgen_array_sumcheck("kgenref_packed_nevapr", kgen_array_sum, DBLE(SUM(kgenref_packed_nevapr, &
        &mask=(kgenref_packed_nevapr .eq. kgenref_packed_nevapr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_am_evp_st 
        CALL kgen_array_sumcheck("kgenref_packed_am_evp_st", kgen_array_sum, DBLE(SUM(kgenref_packed_am_evp_st, &
        &mask=(kgenref_packed_am_evp_st .eq. kgenref_packed_am_evp_st))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_evapsnow 
        CALL kgen_array_sumcheck("kgenref_packed_evapsnow", kgen_array_sum, DBLE(SUM(kgenref_packed_evapsnow, &
        &mask=(kgenref_packed_evapsnow .eq. kgenref_packed_evapsnow))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prain 
        CALL kgen_array_sumcheck("kgenref_packed_prain", kgen_array_sum, DBLE(SUM(kgenref_packed_prain, &
        &mask=(kgenref_packed_prain .eq. kgenref_packed_prain))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prodsnow 
        CALL kgen_array_sumcheck("kgenref_packed_prodsnow", kgen_array_sum, DBLE(SUM(kgenref_packed_prodsnow, &
        &mask=(kgenref_packed_prodsnow .eq. kgenref_packed_prodsnow))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_cmeout 
        CALL kgen_array_sumcheck("kgenref_packed_cmeout", kgen_array_sum, DBLE(SUM(kgenref_packed_cmeout, &
        &mask=(kgenref_packed_cmeout .eq. kgenref_packed_cmeout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qsout 
        CALL kgen_array_sumcheck("kgenref_packed_qsout", kgen_array_sum, DBLE(SUM(kgenref_packed_qsout, &
        &mask=(kgenref_packed_qsout .eq. kgenref_packed_qsout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_cflx 
        CALL kgen_array_sumcheck("kgenref_packed_cflx", kgen_array_sum, DBLE(SUM(kgenref_packed_cflx, mask=(kgenref_packed_cflx &
        &.eq. kgenref_packed_cflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_iflx 
        CALL kgen_array_sumcheck("kgenref_packed_iflx", kgen_array_sum, DBLE(SUM(kgenref_packed_iflx, mask=(kgenref_packed_iflx &
        &.eq. kgenref_packed_iflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rflx 
        CALL kgen_array_sumcheck("kgenref_packed_rflx", kgen_array_sum, DBLE(SUM(kgenref_packed_rflx, mask=(kgenref_packed_rflx &
        &.eq. kgenref_packed_rflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_sflx 
        CALL kgen_array_sumcheck("kgenref_packed_sflx", kgen_array_sum, DBLE(SUM(kgenref_packed_sflx, mask=(kgenref_packed_sflx &
        &.eq. kgenref_packed_sflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_gflx 
        CALL kgen_array_sumcheck("kgenref_packed_gflx", kgen_array_sum, DBLE(SUM(kgenref_packed_gflx, mask=(kgenref_packed_gflx &
        &.eq. kgenref_packed_gflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrout 
        CALL kgen_array_sumcheck("kgenref_packed_qrout", kgen_array_sum, DBLE(SUM(kgenref_packed_qrout, &
        &mask=(kgenref_packed_qrout .eq. kgenref_packed_qrout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcsevap 
        CALL kgen_array_sumcheck("kgenref_packed_qcsevap", kgen_array_sum, DBLE(SUM(kgenref_packed_qcsevap, &
        &mask=(kgenref_packed_qcsevap .eq. kgenref_packed_qcsevap))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qisevap 
        CALL kgen_array_sumcheck("kgenref_packed_qisevap", kgen_array_sum, DBLE(SUM(kgenref_packed_qisevap, &
        &mask=(kgenref_packed_qisevap .eq. kgenref_packed_qisevap))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qvres 
        CALL kgen_array_sumcheck("kgenref_packed_qvres", kgen_array_sum, DBLE(SUM(kgenref_packed_qvres, &
        &mask=(kgenref_packed_qvres .eq. kgenref_packed_qvres))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_cmei 
        CALL kgen_array_sumcheck("kgenref_packed_cmei", kgen_array_sum, DBLE(SUM(kgenref_packed_cmei, mask=(kgenref_packed_cmei &
        &.eq. kgenref_packed_cmei))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_vtrmc 
        CALL kgen_array_sumcheck("kgenref_packed_vtrmc", kgen_array_sum, DBLE(SUM(kgenref_packed_vtrmc, &
        &mask=(kgenref_packed_vtrmc .eq. kgenref_packed_vtrmc))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_vtrmi 
        CALL kgen_array_sumcheck("kgenref_packed_vtrmi", kgen_array_sum, DBLE(SUM(kgenref_packed_vtrmi, &
        &mask=(kgenref_packed_vtrmi .eq. kgenref_packed_vtrmi))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcsedten 
        CALL kgen_array_sumcheck("kgenref_packed_qcsedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qcsedten, &
        &mask=(kgenref_packed_qcsedten .eq. kgenref_packed_qcsedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qisedten 
        CALL kgen_array_sumcheck("kgenref_packed_qisedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qisedten, &
        &mask=(kgenref_packed_qisedten .eq. kgenref_packed_qisedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrsedten 
        CALL kgen_array_sumcheck("kgenref_packed_qrsedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qrsedten, &
        &mask=(kgenref_packed_qrsedten .eq. kgenref_packed_qrsedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qssedten 
        CALL kgen_array_sumcheck("kgenref_packed_qssedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qssedten, &
        &mask=(kgenref_packed_qssedten .eq. kgenref_packed_qssedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qgsedten 
        CALL kgen_array_sumcheck("kgenref_packed_qgsedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qgsedten, &
        &mask=(kgenref_packed_qgsedten .eq. kgenref_packed_qgsedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_umg 
        CALL kgen_array_sumcheck("kgenref_packed_umg", kgen_array_sum, DBLE(SUM(kgenref_packed_umg, mask=(kgenref_packed_umg .eq. &
        &kgenref_packed_umg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_umr 
        CALL kgen_array_sumcheck("kgenref_packed_umr", kgen_array_sum, DBLE(SUM(kgenref_packed_umr, mask=(kgenref_packed_umr .eq. &
        &kgenref_packed_umr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ums 
        CALL kgen_array_sumcheck("kgenref_packed_ums", kgen_array_sum, DBLE(SUM(kgenref_packed_ums, mask=(kgenref_packed_ums .eq. &
        &kgenref_packed_ums))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pra 
        CALL kgen_array_sumcheck("kgenref_packed_pra", kgen_array_sum, DBLE(SUM(kgenref_packed_pra, mask=(kgenref_packed_pra .eq. &
        &kgenref_packed_pra))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prc 
        CALL kgen_array_sumcheck("kgenref_packed_prc", kgen_array_sum, DBLE(SUM(kgenref_packed_prc, mask=(kgenref_packed_prc .eq. &
        &kgenref_packed_prc))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccc 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccc", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccc, &
        &mask=(kgenref_packed_mnuccc .eq. kgenref_packed_mnuccc))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnucct 
        CALL kgen_array_sumcheck("kgenref_packed_mnucct", kgen_array_sum, DBLE(SUM(kgenref_packed_mnucct, &
        &mask=(kgenref_packed_mnucct .eq. kgenref_packed_mnucct))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_msacwi 
        CALL kgen_array_sumcheck("kgenref_packed_msacwi", kgen_array_sum, DBLE(SUM(kgenref_packed_msacwi, &
        &mask=(kgenref_packed_msacwi .eq. kgenref_packed_msacwi))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_psacws 
        CALL kgen_array_sumcheck("kgenref_packed_psacws", kgen_array_sum, DBLE(SUM(kgenref_packed_psacws, &
        &mask=(kgenref_packed_psacws .eq. kgenref_packed_psacws))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_bergs 
        CALL kgen_array_sumcheck("kgenref_packed_bergs", kgen_array_sum, DBLE(SUM(kgenref_packed_bergs, &
        &mask=(kgenref_packed_bergs .eq. kgenref_packed_bergs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_berg 
        CALL kgen_array_sumcheck("kgenref_packed_berg", kgen_array_sum, DBLE(SUM(kgenref_packed_berg, mask=(kgenref_packed_berg &
        &.eq. kgenref_packed_berg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_melt 
        CALL kgen_array_sumcheck("kgenref_packed_melt", kgen_array_sum, DBLE(SUM(kgenref_packed_melt, mask=(kgenref_packed_melt &
        &.eq. kgenref_packed_melt))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_homo 
        CALL kgen_array_sumcheck("kgenref_packed_homo", kgen_array_sum, DBLE(SUM(kgenref_packed_homo, mask=(kgenref_packed_homo &
        &.eq. kgenref_packed_homo))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcres 
        CALL kgen_array_sumcheck("kgenref_packed_qcres", kgen_array_sum, DBLE(SUM(kgenref_packed_qcres, &
        &mask=(kgenref_packed_qcres .eq. kgenref_packed_qcres))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prci 
        CALL kgen_array_sumcheck("kgenref_packed_prci", kgen_array_sum, DBLE(SUM(kgenref_packed_prci, mask=(kgenref_packed_prci &
        &.eq. kgenref_packed_prci))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prai 
        CALL kgen_array_sumcheck("kgenref_packed_prai", kgen_array_sum, DBLE(SUM(kgenref_packed_prai, mask=(kgenref_packed_prai &
        &.eq. kgenref_packed_prai))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qires 
        CALL kgen_array_sumcheck("kgenref_packed_qires", kgen_array_sum, DBLE(SUM(kgenref_packed_qires, &
        &mask=(kgenref_packed_qires .eq. kgenref_packed_qires))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccr 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccr", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccr, &
        &mask=(kgenref_packed_mnuccr .eq. kgenref_packed_mnuccr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccri 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccri", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccri, &
        &mask=(kgenref_packed_mnuccri .eq. kgenref_packed_mnuccri))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnudeptot 
        CALL kgen_array_sumcheck("kgenref_packed_mnudeptot", kgen_array_sum, DBLE(SUM(kgenref_packed_mnudeptot, &
        &mask=(kgenref_packed_mnudeptot .eq. kgenref_packed_mnudeptot))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_meltgtot 
        CALL kgen_array_sumcheck("kgenref_packed_meltgtot", kgen_array_sum, DBLE(SUM(kgenref_packed_meltgtot, &
        &mask=(kgenref_packed_meltgtot .eq. kgenref_packed_meltgtot))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_meltstot 
        CALL kgen_array_sumcheck("kgenref_packed_meltstot", kgen_array_sum, DBLE(SUM(kgenref_packed_meltstot, &
        &mask=(kgenref_packed_meltstot .eq. kgenref_packed_meltstot))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pracs 
        CALL kgen_array_sumcheck("kgenref_packed_pracs", kgen_array_sum, DBLE(SUM(kgenref_packed_pracs, &
        &mask=(kgenref_packed_pracs .eq. kgenref_packed_pracs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_meltsdt 
        CALL kgen_array_sumcheck("kgenref_packed_meltsdt", kgen_array_sum, DBLE(SUM(kgenref_packed_meltsdt, &
        &mask=(kgenref_packed_meltsdt .eq. kgenref_packed_meltsdt))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_frzrdt 
        CALL kgen_array_sumcheck("kgenref_packed_frzrdt", kgen_array_sum, DBLE(SUM(kgenref_packed_frzrdt, &
        &mask=(kgenref_packed_frzrdt .eq. kgenref_packed_frzrdt))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccd 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccd", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccd, &
        &mask=(kgenref_packed_mnuccd .eq. kgenref_packed_mnuccd))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nrout 
        CALL kgen_array_sumcheck("kgenref_packed_nrout", kgen_array_sum, DBLE(SUM(kgenref_packed_nrout, &
        &mask=(kgenref_packed_nrout .eq. kgenref_packed_nrout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nsout 
        CALL kgen_array_sumcheck("kgenref_packed_nsout", kgen_array_sum, DBLE(SUM(kgenref_packed_nsout, &
        &mask=(kgenref_packed_nsout .eq. kgenref_packed_nsout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_refl 
        CALL kgen_array_sumcheck("kgenref_packed_refl", kgen_array_sum, DBLE(SUM(kgenref_packed_refl, mask=(kgenref_packed_refl &
        &.eq. kgenref_packed_refl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_arefl 
        CALL kgen_array_sumcheck("kgenref_packed_arefl", kgen_array_sum, DBLE(SUM(kgenref_packed_arefl, &
        &mask=(kgenref_packed_arefl .eq. kgenref_packed_arefl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_areflz 
        CALL kgen_array_sumcheck("kgenref_packed_areflz", kgen_array_sum, DBLE(SUM(kgenref_packed_areflz, &
        &mask=(kgenref_packed_areflz .eq. kgenref_packed_areflz))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_frefl 
        CALL kgen_array_sumcheck("kgenref_packed_frefl", kgen_array_sum, DBLE(SUM(kgenref_packed_frefl, &
        &mask=(kgenref_packed_frefl .eq. kgenref_packed_frefl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_csrfl 
        CALL kgen_array_sumcheck("kgenref_packed_csrfl", kgen_array_sum, DBLE(SUM(kgenref_packed_csrfl, &
        &mask=(kgenref_packed_csrfl .eq. kgenref_packed_csrfl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_acsrfl 
        CALL kgen_array_sumcheck("kgenref_packed_acsrfl", kgen_array_sum, DBLE(SUM(kgenref_packed_acsrfl, &
        &mask=(kgenref_packed_acsrfl .eq. kgenref_packed_acsrfl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_fcsrfl 
        CALL kgen_array_sumcheck("kgenref_packed_fcsrfl", kgen_array_sum, DBLE(SUM(kgenref_packed_fcsrfl, &
        &mask=(kgenref_packed_fcsrfl .eq. kgenref_packed_fcsrfl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rercld 
        CALL kgen_array_sumcheck("kgenref_packed_rercld", kgen_array_sum, DBLE(SUM(kgenref_packed_rercld, &
        &mask=(kgenref_packed_rercld .eq. kgenref_packed_rercld))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ncai 
        CALL kgen_array_sumcheck("kgenref_packed_ncai", kgen_array_sum, DBLE(SUM(kgenref_packed_ncai, mask=(kgenref_packed_ncai &
        &.eq. kgenref_packed_ncai))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ncal 
        CALL kgen_array_sumcheck("kgenref_packed_ncal", kgen_array_sum, DBLE(SUM(kgenref_packed_ncal, mask=(kgenref_packed_ncal &
        &.eq. kgenref_packed_ncal))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrout2 
        CALL kgen_array_sumcheck("kgenref_packed_qrout2", kgen_array_sum, DBLE(SUM(kgenref_packed_qrout2, &
        &mask=(kgenref_packed_qrout2 .eq. kgenref_packed_qrout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qsout2 
        CALL kgen_array_sumcheck("kgenref_packed_qsout2", kgen_array_sum, DBLE(SUM(kgenref_packed_qsout2, &
        &mask=(kgenref_packed_qsout2 .eq. kgenref_packed_qsout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nrout2 
        CALL kgen_array_sumcheck("kgenref_packed_nrout2", kgen_array_sum, DBLE(SUM(kgenref_packed_nrout2, &
        &mask=(kgenref_packed_nrout2 .eq. kgenref_packed_nrout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nsout2 
        CALL kgen_array_sumcheck("kgenref_packed_nsout2", kgen_array_sum, DBLE(SUM(kgenref_packed_nsout2, &
        &mask=(kgenref_packed_nsout2 .eq. kgenref_packed_nsout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_freqs 
        CALL kgen_array_sumcheck("kgenref_packed_freqs", kgen_array_sum, DBLE(SUM(kgenref_packed_freqs, &
        &mask=(kgenref_packed_freqs .eq. kgenref_packed_freqs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_freqr 
        CALL kgen_array_sumcheck("kgenref_packed_freqr", kgen_array_sum, DBLE(SUM(kgenref_packed_freqr, &
        &mask=(kgenref_packed_freqr .eq. kgenref_packed_freqr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_freqg 
        CALL kgen_array_sumcheck("kgenref_packed_freqg", kgen_array_sum, DBLE(SUM(kgenref_packed_freqg, &
        &mask=(kgenref_packed_freqg .eq. kgenref_packed_freqg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nfice 
        CALL kgen_array_sumcheck("kgenref_packed_nfice", kgen_array_sum, DBLE(SUM(kgenref_packed_nfice, &
        &mask=(kgenref_packed_nfice .eq. kgenref_packed_nfice))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prer_evap 
        CALL kgen_array_sumcheck("kgenref_packed_prer_evap", kgen_array_sum, DBLE(SUM(kgenref_packed_prer_evap, &
        &mask=(kgenref_packed_prer_evap .eq. kgenref_packed_prer_evap))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcrat 
        CALL kgen_array_sumcheck("kgenref_packed_qcrat", kgen_array_sum, DBLE(SUM(kgenref_packed_qcrat, &
        &mask=(kgenref_packed_qcrat .eq. kgenref_packed_qcrat))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rel 
        CALL kgen_array_sumcheck("kgenref_packed_rel", kgen_array_sum, DBLE(SUM(kgenref_packed_rel, mask=(kgenref_packed_rel .eq. &
        &kgenref_packed_rel))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rei 
        CALL kgen_array_sumcheck("kgenref_packed_rei", kgen_array_sum, DBLE(SUM(kgenref_packed_rei, mask=(kgenref_packed_rei .eq. &
        &kgenref_packed_rei))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_sadice 
        CALL kgen_array_sumcheck("kgenref_packed_sadice", kgen_array_sum, DBLE(SUM(kgenref_packed_sadice, &
        &mask=(kgenref_packed_sadice .eq. kgenref_packed_sadice))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_sadsnow 
        CALL kgen_array_sumcheck("kgenref_packed_sadsnow", kgen_array_sum, DBLE(SUM(kgenref_packed_sadsnow, &
        &mask=(kgenref_packed_sadsnow .eq. kgenref_packed_sadsnow))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_lambdac 
        CALL kgen_array_sumcheck("kgenref_packed_lambdac", kgen_array_sum, DBLE(SUM(kgenref_packed_lambdac, &
        &mask=(kgenref_packed_lambdac .eq. kgenref_packed_lambdac))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mu 
        CALL kgen_array_sumcheck("kgenref_packed_mu", kgen_array_sum, DBLE(SUM(kgenref_packed_mu, mask=(kgenref_packed_mu .eq. &
        &kgenref_packed_mu))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_des 
        CALL kgen_array_sumcheck("kgenref_packed_des", kgen_array_sum, DBLE(SUM(kgenref_packed_des, mask=(kgenref_packed_des .eq. &
        &kgenref_packed_des))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_dei 
        CALL kgen_array_sumcheck("kgenref_packed_dei", kgen_array_sum, DBLE(SUM(kgenref_packed_dei, mask=(kgenref_packed_dei .eq. &
        &kgenref_packed_dei))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qgout 
        CALL kgen_array_sumcheck("kgenref_packed_qgout", kgen_array_sum, DBLE(SUM(kgenref_packed_qgout, &
        &mask=(kgenref_packed_qgout .eq. kgenref_packed_qgout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ngout 
        CALL kgen_array_sumcheck("kgenref_packed_ngout", kgen_array_sum, DBLE(SUM(kgenref_packed_ngout, &
        &mask=(kgenref_packed_ngout .eq. kgenref_packed_ngout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_dgout 
        CALL kgen_array_sumcheck("kgenref_packed_dgout", kgen_array_sum, DBLE(SUM(kgenref_packed_dgout, &
        &mask=(kgenref_packed_dgout .eq. kgenref_packed_dgout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qgout2 
        CALL kgen_array_sumcheck("kgenref_packed_qgout2", kgen_array_sum, DBLE(SUM(kgenref_packed_qgout2, &
        &mask=(kgenref_packed_qgout2 .eq. kgenref_packed_qgout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ngout2 
        CALL kgen_array_sumcheck("kgenref_packed_ngout2", kgen_array_sum, DBLE(SUM(kgenref_packed_ngout2, &
        &mask=(kgenref_packed_ngout2 .eq. kgenref_packed_ngout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_dgout2 
        CALL kgen_array_sumcheck("kgenref_packed_dgout2", kgen_array_sum, DBLE(SUM(kgenref_packed_dgout2, &
        &mask=(kgenref_packed_dgout2 .eq. kgenref_packed_dgout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_psacr 
        CALL kgen_array_sumcheck("kgenref_packed_psacr", kgen_array_sum, DBLE(SUM(kgenref_packed_psacr, &
        &mask=(kgenref_packed_psacr .eq. kgenref_packed_psacr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pracg 
        CALL kgen_array_sumcheck("kgenref_packed_pracg", kgen_array_sum, DBLE(SUM(kgenref_packed_pracg, &
        &mask=(kgenref_packed_pracg .eq. kgenref_packed_pracg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_psacwg 
        CALL kgen_array_sumcheck("kgenref_packed_psacwg", kgen_array_sum, DBLE(SUM(kgenref_packed_psacwg, &
        &mask=(kgenref_packed_psacwg .eq. kgenref_packed_psacwg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pgsacw 
        CALL kgen_array_sumcheck("kgenref_packed_pgsacw", kgen_array_sum, DBLE(SUM(kgenref_packed_pgsacw, &
        &mask=(kgenref_packed_pgsacw .eq. kgenref_packed_pgsacw))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pgracs 
        CALL kgen_array_sumcheck("kgenref_packed_pgracs", kgen_array_sum, DBLE(SUM(kgenref_packed_pgracs, &
        &mask=(kgenref_packed_pgracs .eq. kgenref_packed_pgracs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prdg 
        CALL kgen_array_sumcheck("kgenref_packed_prdg", kgen_array_sum, DBLE(SUM(kgenref_packed_prdg, mask=(kgenref_packed_prdg &
        &.eq. kgenref_packed_prdg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qmultg 
        CALL kgen_array_sumcheck("kgenref_packed_qmultg", kgen_array_sum, DBLE(SUM(kgenref_packed_qmultg, &
        &mask=(kgenref_packed_qmultg .eq. kgenref_packed_qmultg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qmultrg 
        CALL kgen_array_sumcheck("kgenref_packed_qmultrg", kgen_array_sum, DBLE(SUM(kgenref_packed_qmultrg, &
        &mask=(kgenref_packed_qmultrg .eq. kgenref_packed_qmultrg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_npracg 
        CALL kgen_array_sumcheck("kgenref_packed_npracg", kgen_array_sum, DBLE(SUM(kgenref_packed_npracg, &
        &mask=(kgenref_packed_npracg .eq. kgenref_packed_npracg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nscng 
        CALL kgen_array_sumcheck("kgenref_packed_nscng", kgen_array_sum, DBLE(SUM(kgenref_packed_nscng, &
        &mask=(kgenref_packed_nscng .eq. kgenref_packed_nscng))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ngracs 
        CALL kgen_array_sumcheck("kgenref_packed_ngracs", kgen_array_sum, DBLE(SUM(kgenref_packed_ngracs, &
        &mask=(kgenref_packed_ngracs .eq. kgenref_packed_ngracs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nmultg 
        CALL kgen_array_sumcheck("kgenref_packed_nmultg", kgen_array_sum, DBLE(SUM(kgenref_packed_nmultg, &
        &mask=(kgenref_packed_nmultg .eq. kgenref_packed_nmultg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nmultrg 
        CALL kgen_array_sumcheck("kgenref_packed_nmultrg", kgen_array_sum, DBLE(SUM(kgenref_packed_nmultrg, &
        &mask=(kgenref_packed_nmultrg .eq. kgenref_packed_nmultrg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_npsacwg 
        CALL kgen_array_sumcheck("kgenref_packed_npsacwg", kgen_array_sum, DBLE(SUM(kgenref_packed_npsacwg, &
        &mask=(kgenref_packed_npsacwg .eq. kgenref_packed_npsacwg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_rel_fn_dum 
        CALL kgen_array_sumcheck("kgenref_rel_fn_dum", kgen_array_sum, DBLE(SUM(kgenref_rel_fn_dum, mask=(kgenref_rel_fn_dum .eq. &
        &kgenref_rel_fn_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_dsout2_dum 
        CALL kgen_array_sumcheck("kgenref_dsout2_dum", kgen_array_sum, DBLE(SUM(kgenref_dsout2_dum, mask=(kgenref_dsout2_dum .eq. &
        &kgenref_dsout2_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_drout_dum 
        CALL kgen_array_sumcheck("kgenref_drout_dum", kgen_array_sum, DBLE(SUM(kgenref_drout_dum, mask=(kgenref_drout_dum .eq. &
        &kgenref_drout_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_reff_rain_dum 
        CALL kgen_array_sumcheck("kgenref_reff_rain_dum", kgen_array_sum, DBLE(SUM(kgenref_reff_rain_dum, &
        &mask=(kgenref_reff_rain_dum .eq. kgenref_reff_rain_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_reff_snow_dum 
        CALL kgen_array_sumcheck("kgenref_reff_snow_dum", kgen_array_sum, DBLE(SUM(kgenref_reff_snow_dum, &
        &mask=(kgenref_reff_snow_dum .eq. kgenref_reff_snow_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_reff_grau_dum 
        CALL kgen_array_sumcheck("kgenref_reff_grau_dum", kgen_array_sum, DBLE(SUM(kgenref_reff_grau_dum, &
        &mask=(kgenref_reff_grau_dum .eq. kgenref_reff_grau_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgenref_errstring 


   ! Set the col_type flag to grid or subcolumn dependent on the value of use_subcol_microp

   !-----------------------
   ! These physics buffer fields are read only and not set in this parameterization
   ! If these fields do not have subcolumn data, copy the grid to the subcolumn if subcolumns is turned on
   ! If subcolumns is not turned on, then these fields will be grid data


   !-----------------------
   ! These physics buffer fields are calculated and set in this parameterization
   ! If subcolumns is turned on, then these fields will be calculated on a subcolumn grid, otherwise they will be a normal grid


   !-----------------------
   ! If subcolumns is turned on, all calculated fields which are on subcolumns
   ! need to be retrieved on the grid as well for storing averaged values


   !-----------------------
   ! These are only on the grid regardless of whether subcolumns are turned on or not


   !-------------------------------------------------------------------------------------
   ! Microphysics assumes 'liquid stratus frac = ice stratus frac
   !                      = max( liquid stratus frac, ice stratus frac )'.
   
   ! Output initial in-cloud LWP (before microphysics)


   ! Initialize local state from input.

   ! Because of the of limited vertical resolution, there can be a signifcant
   ! warm bias at the cold point tropopause, which can create a wet bias in the
   ! stratosphere. For the microphysics only, update the cold point temperature, with
   ! an estimate of the coldest point between the model layers.


   ! Initialize ptend for output.


   ! the name 'cldwat' triggers special tests on cldliq
   ! and cldice in physics_update


   ! The following are all variables related to sizes, where it does not
   ! necessarily make sense to average over time steps. Instead, we keep
   ! the value from the last substep, which is what "accum_null" does.

   ! Pack input variables that are not updated during substeps.


    IF (kgen_evalstage) THEN 
    END IF   
    IF (kgen_warmupstage) THEN 
    END IF   
    IF (kgen_mainstage) THEN 
    END IF   
 
#if defined(__OPENACC__)
    ngpus = acc_get_num_devices(acc_device_default)
    gpunum = MOD(myrank,ngpus)
    if (kgen_evalstage) then
       print *,'# of GPUs: ',ngpus
       print *,'GPU id: ',gpunum
    endif
    call acc_set_device_num(gpunum,acc_device_default)
#endif
     
    !Uncomment following call statement to turn on perturbation experiment. 
    !Adjust perturbation value and/or kind parameter if required. 
    !CALL kgen_perturb_real( your_variable, 1.0E-15_8 ) 
      
    !$acc data copyin  (packed_t,packed_q,packed_qc,packed_qi,packed_nc,packed_ni, &
    !$acc               packed_qr,packed_qs,packed_nr,packed_ns,packed_qg,packed_ng, &
    !$acc               packed_relvar,packed_accre_enhan,packed_p,packed_pdel, &
    !$acc               packed_cldn,packed_liqcldf,packed_icecldf,packed_qsatfac, &
    !$acc               packed_naai,packed_npccn,packed_rndst,packed_nacon, &
    !$acc               packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice, &
    !$acc               packed_frzimm,packed_frzcnt,packed_frzdep, &
    !$acc               mg_liq_props,mg_ice_props,mg_rain_props,mg_graupel_props, &
    !$acc               mg_hail_props,mg_snow_props) &
    !$acc      copyout (packed_rate1ord_cw2pr_st,packed_tlat,packed_qvlat,packed_qctend, &
    !$acc               packed_qitend,packed_nctend,packed_nitend,packed_qrtend, &
    !$acc               packed_qstend,packed_nrtend,packed_nstend,packed_qgtend, &
    !$acc               packed_ngtend,packed_rel,rel_fn_dum,packed_rei,packed_sadice, &
    !$acc               packed_sadsnow,packed_prect,packed_preci,packed_nevapr, &
    !$acc               packed_evapsnow,packed_am_evp_st,packed_prain,packed_prodsnow, &
    !$acc               packed_cmeout,packed_dei,packed_mu,packed_lambdac,packed_qsout, &
    !$acc               packed_des,packed_iflx,packed_gflx,packed_rflx,packed_sflx, &
    !$acc               packed_cflx,packed_qrout,reff_rain_dum,reff_snow_dum,reff_grau_dum, &
    !$acc               packed_qcsevap,packed_qisevap,packed_qvres,packed_cmei, &
    !$acc               packed_vtrmc,packed_vtrmi,packed_umr,packed_ums,packed_umg, &
    !$acc               packed_qgsedten,packed_qcsedten,packed_qisedten,packed_qrsedten, &
    !$acc               packed_qssedten,packed_pra,packed_prc,packed_mnuccc,packed_mnucct, &
    !$acc               packed_mnuccc,packed_mnucct,packed_msacwi,packed_psacws,packed_bergs, &
    !$acc               packed_berg,packed_melt,packed_meltstot,packed_meltgtot, &
    !$acc               packed_mnudeptot,packed_homo,packed_qcres,packed_prci,packed_prai, & 
    !$acc               packed_qires,packed_mnuccr,packed_mnuccri,packed_pracs, &
    !$acc               packed_meltsdt,packed_frzrdt,packed_mnuccd,packed_pracg,packed_psacwg, &
    !$acc               packed_pgsacw,packed_pgracs,packed_prdg,packed_qmultg,packed_qmultrg, &
    !$acc               packed_psacr,packed_npracg,packed_nscng,packed_ngracs, &
    !$acc               packed_nmultg,packed_nmultrg,packed_npsacwg,packed_nrout, &
    !$acc               packed_nsout,packed_refl,packed_arefl,packed_areflz, &
    !$acc               packed_frefl,packed_csrfl,packed_acsrfl,packed_fcsrfl, &
    !$acc               packed_rercld,packed_ncai,packed_ncal,packed_qrout2, &
    !$acc               packed_qsout2,packed_nrout2,packed_nsout2,drout_dum,dsout2_dum, &
    !$acc               packed_freqs,packed_freqr,packed_nfice,packed_qcrat,packed_qgout, &
    !$acc               packed_dgout,packed_ngout,packed_qgout2,packed_ngout2,packed_dgout2, &
    !$acc               packed_freqg,packed_prer_evap)
#if defined(OPENMP_GPU)
!$omp target data map(to:packed_t,packed_q,packed_qc,packed_qi,&
!$omp packed_nc,packed_ni,packed_qr,packed_qs,packed_nr,packed_ns,&
!$omp packed_qg,packed_ng,packed_relvar,packed_accre_enhan,packed_p,&
!$omp packed_pdel,packed_cldn,packed_liqcldf,packed_icecldf,&
!$omp packed_qsatfac,packed_naai,packed_npccn,packed_rndst,&
!$omp packed_nacon,packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
!$omp packed_frzimm,packed_frzcnt,packed_frzdep,mg_liq_props,&
!$omp mg_ice_props,mg_rain_props,mg_graupel_props,mg_hail_props,&
!$omp mg_snow_props) map(from:packed_rate1ord_cw2pr_st,packed_tlat,&
!$omp packed_qvlat,packed_qctend,packed_qitend,packed_nctend,&
!$omp packed_nitend,packed_qrtend,packed_qstend,packed_nrtend,&
!$omp packed_nstend,packed_qgtend,packed_ngtend,packed_rel,rel_fn_dum,&
!$omp packed_rei,packed_sadice,packed_sadsnow,packed_prect,&
!$omp packed_preci,packed_nevapr,packed_evapsnow,packed_am_evp_st,&
!$omp packed_prain,packed_prodsnow,packed_cmeout,packed_dei,packed_mu,&
!$omp packed_lambdac,packed_qsout,packed_des,packed_iflx,packed_gflx,&
!$omp packed_rflx,packed_sflx,packed_cflx,packed_qrout,reff_rain_dum,&
!$omp reff_snow_dum,reff_grau_dum,packed_qcsevap,packed_qisevap,&
!$omp packed_qvres,packed_cmei,packed_vtrmc,packed_vtrmi,packed_umr,&
!$omp packed_ums,packed_umg,packed_qgsedten,packed_qcsedten,&
!$omp packed_qisedten,packed_qrsedten,packed_qssedten,packed_pra,&
!$omp packed_prc,packed_mnuccc,packed_mnucct,packed_mnuccc,&
!$omp packed_mnucct,packed_msacwi,packed_psacws,packed_bergs,&
!$omp packed_berg,packed_melt,packed_meltstot,packed_meltgtot,&
!$omp packed_mnudeptot,packed_homo,packed_qcres,packed_prci,&
!$omp packed_prai,packed_qires,packed_mnuccr,packed_mnuccri,&
!$omp packed_pracs,packed_meltsdt,packed_frzrdt,packed_mnuccd,&
!$omp packed_pracg,packed_psacwg,packed_pgsacw,packed_pgracs,&
!$omp packed_prdg,packed_qmultg,packed_qmultrg,packed_psacr,&
!$omp packed_npracg,packed_nscng,packed_ngracs,packed_nmultg,&
!$omp packed_nmultrg,packed_npsacwg,packed_nrout,packed_nsout,&
!$omp packed_refl,packed_arefl,packed_areflz,packed_frefl,packed_csrfl,&
!$omp packed_acsrfl,packed_fcsrfl,packed_rercld,packed_ncai,&
!$omp packed_ncal,packed_qrout2,packed_qsout2,packed_nrout2,&
!$omp packed_nsout2,drout_dum,dsout2_dum,packed_freqs,packed_freqr,&
!$omp packed_nfice,packed_qcrat,packed_qgout,packed_dgout,packed_ngout,&
!$omp packed_qgout2,packed_ngout2,packed_dgout2,packed_freqg,&
!$omp packed_prer_evap)
#endif // defined(OPENMP_GPU)

    !call to kgen kernel 
         call micro_mg_tend3_0( &
              DFACT*mgncol,         nlev,           dtime/num_steps,&
              packed_t,               packed_q,               &
              packed_qc,              packed_qi,              &
              packed_nc,              packed_ni,              &
              packed_qr,              packed_qs,              &
              packed_nr,              packed_ns,              &
              packed_qg,              packed_ng,              &
              packed_relvar,          packed_accre_enhan,     &
              packed_p,               packed_pdel,            &
              packed_cldn, packed_liqcldf, packed_icecldf, packed_qsatfac, &
              packed_rate1ord_cw2pr_st,                       &
              packed_naai,            packed_npccn,           &
              packed_rndst,           packed_nacon,           &
              packed_tlat,            packed_qvlat,           &
              packed_qctend,          packed_qitend,          &
              packed_nctend,          packed_nitend,          &
              packed_qrtend,          packed_qstend,          &
              packed_nrtend,          packed_nstend,          &
              packed_qgtend,          packed_ngtend,          &
              packed_rel,     rel_fn_dum,     packed_rei,     &
              packed_sadice,          packed_sadsnow,         &
              packed_prect,           packed_preci,           &
              packed_nevapr,          packed_evapsnow,        &
              packed_am_evp_st,                               &
              packed_prain,           packed_prodsnow,        &
              packed_cmeout,          packed_dei,             &
              packed_mu,              packed_lambdac,         &
              packed_qsout,           packed_des,             &
              packed_qgout,   packed_ngout,   packed_dgout,   &
              packed_cflx,    packed_iflx,                    &
              packed_gflx,                                    &
              packed_rflx,    packed_sflx,    packed_qrout,   &
              reff_rain_dum,          reff_snow_dum,   reff_grau_dum,       &
              packed_qcsevap, packed_qisevap, packed_qvres,   &
              packed_cmei,    packed_vtrmc,   packed_vtrmi,   &
              packed_umr,             packed_ums,             &
              packed_umg,             packed_qgsedten,        &
              packed_qcsedten,        packed_qisedten,        &
              packed_qrsedten,        packed_qssedten,        &
              packed_pra,             packed_prc,             &
              packed_mnuccc,  packed_mnucct,  packed_msacwi,  &
              packed_psacws,  packed_bergs,   packed_berg,    &
              packed_melt,    packed_meltstot, packed_meltgtot,           packed_homo,            &
              packed_qcres,   packed_prci,    packed_prai,    &
              packed_qires,   packed_mnuccr,  packed_mnudeptot, packed_mnuccri, packed_pracs,   &
              packed_meltsdt, packed_frzrdt,  packed_mnuccd,  &
              packed_pracg,   packed_psacwg,  packed_pgsacw,  &
              packed_pgracs,  packed_prdg,   &
              packed_qmultg,  packed_qmultrg, packed_psacr,   &
              packed_npracg,  packed_nscng,   packed_ngracs,  &
              packed_nmultg,  packed_nmultrg, packed_npsacwg, & 
              packed_nrout,           packed_nsout,           &
              packed_refl,    packed_arefl,   packed_areflz,  &
              packed_frefl,   packed_csrfl,   packed_acsrfl,  &
              packed_fcsrfl,          packed_rercld,          &
              packed_ncai,            packed_ncal,            &
              packed_qrout2,          packed_qsout2,          &
              packed_nrout2,          packed_nsout2,          &
              drout_dum,              dsout2_dum,             &
              packed_qgout2, packed_ngout2, packed_dgout2, packed_freqg,   &
              packed_freqs,           packed_freqr,           &
              packed_nfice,           packed_qcrat,           &
              errstring, &
              packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
              packed_prer_evap,                                     &
              packed_frzimm,  packed_frzcnt,  packed_frzdep   )

    !$acc end data
#if defined(OPENMP_GPU)
!$omp end target data
#endif // defined(OPENMP_GPU)

         IF (kgen_mainstage) THEN 
               
             !verify init 
             CALL kgen_init_verify(tolerance=2.D-9, minvalue=1.D-14, verboseLevel=1) 
#ifdef _MPI
             CALL kgen_init_check(check_status, rank=myrank)
#else
             CALL kgen_init_check(check_status, rank=kgen_mpirank) 
#endif               
             !extern verify variables 
               
             !local verify variables 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_rate1ord_cw2pr_st", check_status, packed_rate1ord_cw2pr_st(1:mgncol,1:nlev), &
             &kgenref_packed_rate1ord_cw2pr_st) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_tlat", check_status, packed_tlat(1:mgncol,1:nlev), kgenref_packed_tlat) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qvlat", check_status, packed_qvlat(1:mgncol,1:nlev), kgenref_packed_qvlat) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qctend", check_status, packed_qctend(1:mgncol,1:nlev), kgenref_packed_qctend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qitend", check_status, packed_qitend(1:mgncol,1:nlev), kgenref_packed_qitend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nctend", check_status, packed_nctend(1:mgncol,1:nlev), kgenref_packed_nctend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nitend", check_status, packed_nitend(1:mgncol,1:nlev), kgenref_packed_nitend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qrtend", check_status, packed_qrtend(1:mgncol,1:nlev), kgenref_packed_qrtend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qstend", check_status, packed_qstend(1:mgncol,1:nlev), kgenref_packed_qstend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nrtend", check_status, packed_nrtend(1:mgncol,1:nlev), kgenref_packed_nrtend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nstend", check_status, packed_nstend(1:mgncol,1:nlev), kgenref_packed_nstend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qgtend", check_status, packed_qgtend(1:mgncol,1:nlev), kgenref_packed_qgtend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ngtend", check_status, packed_ngtend(1:mgncol,1:nlev), kgenref_packed_ngtend) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim1("packed_prect", check_status, packed_prect(1:mgncol), kgenref_packed_prect) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim1("packed_preci", check_status, packed_preci(1:mgncol), kgenref_packed_preci) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nevapr", check_status, packed_nevapr(1:mgncol,1:nlev), kgenref_packed_nevapr) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_am_evp_st", check_status, packed_am_evp_st(1:mgncol,1:nlev), &
             &kgenref_packed_am_evp_st) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_evapsnow", check_status, packed_evapsnow(1:mgncol,1:nlev), &
             &kgenref_packed_evapsnow) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prain", check_status, packed_prain(1:mgncol,1:nlev), kgenref_packed_prain) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prodsnow", check_status, packed_prodsnow(1:mgncol,1:nlev), &
             &kgenref_packed_prodsnow) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_cmeout", check_status, packed_cmeout(1:mgncol,1:nlev), kgenref_packed_cmeout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qsout", check_status, packed_qsout(1:mgncol,1:nlev), kgenref_packed_qsout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_cflx", check_status, packed_cflx(1:mgncol,1:nlev), kgenref_packed_cflx) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_iflx", check_status, packed_iflx(1:mgncol,1:nlev), kgenref_packed_iflx) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_rflx", check_status, packed_rflx(1:mgncol,1:nlev), kgenref_packed_rflx) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_sflx", check_status, packed_sflx(1:mgncol,1:nlev), kgenref_packed_sflx) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_gflx", check_status, packed_gflx(1:mgncol,1:nlev), kgenref_packed_gflx) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qrout", check_status, packed_qrout(1:mgncol,1:nlev), kgenref_packed_qrout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qcsevap", check_status, packed_qcsevap(1:mgncol,1:nlev), kgenref_packed_qcsevap) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qisevap", check_status, packed_qisevap(1:mgncol,1:nlev), kgenref_packed_qisevap) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qvres", check_status, packed_qvres(1:mgncol,1:nlev), kgenref_packed_qvres) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_cmei", check_status, packed_cmei(1:mgncol,1:nlev), kgenref_packed_cmei) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_vtrmc", check_status, packed_vtrmc(1:mgncol,1:nlev), kgenref_packed_vtrmc) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_vtrmi", check_status, packed_vtrmi(1:mgncol,1:nlev), kgenref_packed_vtrmi) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qcsedten", check_status, packed_qcsedten(1:mgncol,1:nlev), &
             &kgenref_packed_qcsedten) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qisedten", check_status, packed_qisedten(1:mgncol,1:nlev), &
             &kgenref_packed_qisedten) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qrsedten", check_status, packed_qrsedten(1:mgncol,1:nlev), &
             &kgenref_packed_qrsedten) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qssedten", check_status, packed_qssedten(1:mgncol,1:nlev), &
             &kgenref_packed_qssedten) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qgsedten", check_status, packed_qgsedten(1:mgncol,1:nlev), &
             &kgenref_packed_qgsedten) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_umg", check_status, packed_umg(1:mgncol,1:nlev), kgenref_packed_umg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_umr", check_status, packed_umr(1:mgncol,1:nlev), kgenref_packed_umr) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ums", check_status, packed_ums(1:mgncol,1:nlev), kgenref_packed_ums) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_pra", check_status, packed_pra(1:mgncol,1:nlev), kgenref_packed_pra) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prc", check_status, packed_prc(1:mgncol,1:nlev), kgenref_packed_prc) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mnuccc", check_status, packed_mnuccc(1:mgncol,1:nlev), kgenref_packed_mnuccc) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mnucct", check_status, packed_mnucct(1:mgncol,1:nlev), kgenref_packed_mnucct) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_msacwi", check_status, packed_msacwi(1:mgncol,1:nlev), kgenref_packed_msacwi) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_psacws", check_status, packed_psacws(1:mgncol,1:nlev), kgenref_packed_psacws) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_bergs", check_status, packed_bergs(1:mgncol,1:nlev), kgenref_packed_bergs) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_berg", check_status, packed_berg(1:mgncol,1:nlev), kgenref_packed_berg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_melt", check_status, packed_melt(1:mgncol,1:nlev), kgenref_packed_melt) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_homo", check_status, packed_homo(1:mgncol,1:nlev), kgenref_packed_homo) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qcres", check_status, packed_qcres(1:mgncol,1:nlev), kgenref_packed_qcres) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prci", check_status, packed_prci(1:mgncol,1:nlev), kgenref_packed_prci) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prai", check_status, packed_prai(1:mgncol,1:nlev), kgenref_packed_prai) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qires", check_status, packed_qires(1:mgncol,1:nlev), kgenref_packed_qires) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mnuccr", check_status, packed_mnuccr(1:mgncol,1:nlev), kgenref_packed_mnuccr) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mnuccri", check_status, packed_mnuccri(1:mgncol,1:nlev), kgenref_packed_mnuccri) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mnudeptot", check_status, packed_mnudeptot(1:mgncol,1:nlev), &
             &kgenref_packed_mnudeptot) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_meltgtot", check_status, packed_meltgtot(1:mgncol,1:nlev), &
             &kgenref_packed_meltgtot) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_meltstot", check_status, packed_meltstot(1:mgncol,1:nlev), &
             &kgenref_packed_meltstot) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_pracs", check_status, packed_pracs(1:mgncol,1:nlev), kgenref_packed_pracs) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_meltsdt", check_status, packed_meltsdt(1:mgncol,1:nlev), kgenref_packed_meltsdt) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_frzrdt", check_status, packed_frzrdt(1:mgncol,1:nlev), kgenref_packed_frzrdt) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mnuccd", check_status, packed_mnuccd(1:mgncol,1:nlev), kgenref_packed_mnuccd) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nrout", check_status, packed_nrout(1:mgncol,1:nlev), kgenref_packed_nrout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nsout", check_status, packed_nsout(1:mgncol,1:nlev), kgenref_packed_nsout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_refl", check_status, packed_refl(1:mgncol,1:nlev), kgenref_packed_refl) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_arefl", check_status, packed_arefl(1:mgncol,1:nlev), kgenref_packed_arefl) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_areflz", check_status, packed_areflz(1:mgncol,1:nlev), kgenref_packed_areflz) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_frefl", check_status, packed_frefl(1:mgncol,1:nlev), kgenref_packed_frefl) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_csrfl", check_status, packed_csrfl(1:mgncol,1:nlev), kgenref_packed_csrfl) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_acsrfl", check_status, packed_acsrfl(1:mgncol,1:nlev), kgenref_packed_acsrfl) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_fcsrfl", check_status, packed_fcsrfl(1:mgncol,1:nlev), kgenref_packed_fcsrfl) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_rercld", check_status, packed_rercld(1:mgncol,1:nlev), kgenref_packed_rercld) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ncai", check_status, packed_ncai(1:mgncol,1:nlev), kgenref_packed_ncai) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ncal", check_status, packed_ncal(1:mgncol,1:nlev), kgenref_packed_ncal) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qrout2", check_status, packed_qrout2(1:mgncol,1:nlev), kgenref_packed_qrout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qsout2", check_status, packed_qsout2(1:mgncol,1:nlev), kgenref_packed_qsout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nrout2", check_status, packed_nrout2(1:mgncol,1:nlev), kgenref_packed_nrout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nsout2", check_status, packed_nsout2(1:mgncol,1:nlev), kgenref_packed_nsout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_freqs", check_status, packed_freqs(1:mgncol,1:nlev), kgenref_packed_freqs) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_freqr", check_status, packed_freqr(1:mgncol,1:nlev), kgenref_packed_freqr) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_freqg", check_status, packed_freqg(1:mgncol,1:nlev), kgenref_packed_freqg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nfice", check_status, packed_nfice(1:mgncol,1:nlev), kgenref_packed_nfice) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prer_evap", check_status, packed_prer_evap(1:mgncol,1:nlev), &
             &kgenref_packed_prer_evap) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qcrat", check_status, packed_qcrat(1:mgncol,1:nlev), kgenref_packed_qcrat) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_rel", check_status, packed_rel(1:mgncol,1:nlev), kgenref_packed_rel) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_rei", check_status, packed_rei(1:mgncol,1:nlev), kgenref_packed_rei) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_sadice", check_status, packed_sadice(1:mgncol,1:nlev), kgenref_packed_sadice) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_sadsnow", check_status, packed_sadsnow(1:mgncol,1:nlev), kgenref_packed_sadsnow) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_lambdac", check_status, packed_lambdac(1:mgncol,1:nlev), kgenref_packed_lambdac) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_mu", check_status, packed_mu(1:mgncol,1:nlev), kgenref_packed_mu) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_des", check_status, packed_des(1:mgncol,1:nlev), kgenref_packed_des) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_dei", check_status, packed_dei(1:mgncol,1:nlev), kgenref_packed_dei) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qgout", check_status, packed_qgout(1:mgncol,1:nlev), kgenref_packed_qgout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ngout", check_status, packed_ngout(1:mgncol,1:nlev), kgenref_packed_ngout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_dgout", check_status, packed_dgout(1:mgncol,1:nlev), kgenref_packed_dgout) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qgout2", check_status, packed_qgout2(1:mgncol,1:nlev), kgenref_packed_qgout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ngout2", check_status, packed_ngout2(1:mgncol,1:nlev), kgenref_packed_ngout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_dgout2", check_status, packed_dgout2(1:mgncol,1:nlev), kgenref_packed_dgout2) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_psacr", check_status, packed_psacr(1:mgncol,1:nlev), kgenref_packed_psacr) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_pracg", check_status, packed_pracg(1:mgncol,1:nlev), kgenref_packed_pracg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_psacwg", check_status, packed_psacwg(1:mgncol,1:nlev), kgenref_packed_psacwg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_pgsacw", check_status, packed_pgsacw(1:mgncol,1:nlev), kgenref_packed_pgsacw) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_pgracs", check_status, packed_pgracs(1:mgncol,1:nlev), kgenref_packed_pgracs) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_prdg", check_status, packed_prdg(1:mgncol,1:nlev), kgenref_packed_prdg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qmultg", check_status, packed_qmultg(1:mgncol,1:nlev), kgenref_packed_qmultg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_qmultrg", check_status, packed_qmultrg(1:mgncol,1:nlev), kgenref_packed_qmultrg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_npracg", check_status, packed_npracg(1:mgncol,1:nlev), kgenref_packed_npracg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nscng", check_status, packed_nscng(1:mgncol,1:nlev), kgenref_packed_nscng) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_ngracs", check_status, packed_ngracs(1:mgncol,1:nlev), kgenref_packed_ngracs) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nmultg", check_status, packed_nmultg(1:mgncol,1:nlev), kgenref_packed_nmultg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_nmultrg", check_status, packed_nmultrg(1:mgncol,1:nlev), kgenref_packed_nmultrg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("packed_npsacwg", check_status, packed_npsacwg(1:mgncol,1:nlev), kgenref_packed_npsacwg) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("rel_fn_dum", check_status, rel_fn_dum(1:mgncol,1:nlev), kgenref_rel_fn_dum) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("dsout2_dum", check_status, dsout2_dum(1:mgncol,1:nlev), kgenref_dsout2_dum) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("drout_dum", check_status, drout_dum(1:mgncol,1:nlev), kgenref_drout_dum) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("reff_rain_dum", check_status, reff_rain_dum(1:mgncol,1:nlev), kgenref_reff_rain_dum) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("reff_snow_dum", check_status, reff_snow_dum(1:mgncol,1:nlev), kgenref_reff_snow_dum) 
             CALL kv_micro_mg_cam_tend_pack_real__r8_dim2("reff_grau_dum", check_status, reff_grau_dum(1:mgncol,1:nlev), kgenref_reff_grau_dum) 
             CALL kv_micro_mg_cam_tend_pack_character_128_("errstring", check_status, errstring, kgenref_errstring) 
             IF (check_status%rank == 0) THEN 
                 WRITE (*, *) "" 
             END IF   
             IF (kgen_verboseLevel > 0) THEN 
                 IF (check_status%rank == 0) THEN 
                     WRITE (*, *) "Number of output variables: ", check_status%numTotal 
                     WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
                     WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
                     WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
                     WRITE (*, *) "Tolerance: ", kgen_tolerance 
                 END IF   
             END IF   
             IF (check_status%rank == 0) THEN 
                 WRITE (*, *) "" 
             END IF   
             IF (check_status%numOutTol > 0) THEN 
                 IF (check_status%rank == 0) THEN 
                     WRITE (*, *) "Verification FAILED with" // TRIM(ADJUSTL(kgen_filepath)) 
                 END IF   
                 check_status%Passed = .FALSE. 
                 kgen_isverified = .FALSE. 
             ELSE 
                 IF (check_status%rank == 0) THEN 
                     WRITE (*, *) "Verification PASSED with " // TRIM(ADJUSTL(kgen_filepath)) 
                 END IF   
                 check_status%Passed = .TRUE. 
                 kgen_isverified = .TRUE. 
             END IF   
             IF (check_status%rank == 0) THEN 
                 WRITE (*, *) "" 
             END IF   

#ifdef _MPI
             call MPI_Barrier(MPI_COMM_WORLD,info)
#endif
  !$acc data copyin  (packed_t,packed_q,packed_qc,packed_qi,packed_nc,packed_ni, &
  !$acc               packed_qr,packed_qs,packed_nr,packed_ns,packed_qg,packed_ng, &
  !$acc               packed_relvar,packed_accre_enhan,packed_p,packed_pdel, &
  !$acc               packed_cldn,packed_liqcldf,packed_icecldf,packed_qsatfac, &
  !$acc               packed_naai,packed_npccn,packed_rndst,packed_nacon, &
  !$acc               packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice, &
  !$acc               packed_frzimm,packed_frzcnt,packed_frzdep, &
  !$acc               mg_liq_props,mg_ice_props,mg_rain_props,mg_graupel_props, &
  !$acc               mg_hail_props,mg_snow_props) &
  !$acc      copyout (packed_rate1ord_cw2pr_st,packed_tlat,packed_qvlat,packed_qctend, &
  !$acc               packed_qitend,packed_nctend,packed_nitend,packed_qrtend, &
  !$acc               packed_qstend,packed_nrtend,packed_nstend,packed_qgtend, &
  !$acc               packed_ngtend,packed_rel,rel_fn_dum,packed_rei,packed_sadice, &
  !$acc               packed_sadsnow,packed_prect,packed_preci,packed_nevapr, &
  !$acc               packed_evapsnow,packed_am_evp_st,packed_prain,packed_prodsnow, &
  !$acc               packed_cmeout,packed_dei,packed_mu,packed_lambdac,packed_qsout, &
  !$acc               packed_des,packed_iflx,packed_gflx,packed_rflx,packed_sflx, &
  !$acc               packed_cflx,packed_qrout,reff_rain_dum,reff_snow_dum,reff_grau_dum, &
  !$acc               packed_qcsevap,packed_qisevap,packed_qvres,packed_cmei, &
  !$acc               packed_vtrmc,packed_vtrmi,packed_umr,packed_ums,packed_umg, &
  !$acc               packed_qgsedten,packed_qcsedten,packed_qisedten,packed_qrsedten, &
  !$acc               packed_qssedten,packed_pra,packed_prc,packed_mnuccc,packed_mnucct, &
  !$acc               packed_mnuccc,packed_mnucct,packed_msacwi,packed_psacws,packed_bergs, &
  !$acc               packed_berg,packed_melt,packed_meltstot,packed_meltgtot, &
  !$acc               packed_mnudeptot,packed_homo,packed_qcres,packed_prci,packed_prai, & 
  !$acc               packed_qires,packed_mnuccr,packed_mnuccri,packed_pracs, &
  !$acc               packed_meltsdt,packed_frzrdt,packed_mnuccd,packed_pracg,packed_psacwg, &
  !$acc               packed_pgsacw,packed_pgracs,packed_prdg,packed_qmultg,packed_qmultrg, &
  !$acc               packed_psacr,packed_npracg,packed_nscng,packed_ngracs, &
  !$acc               packed_nmultg,packed_nmultrg,packed_npsacwg,packed_nrout, &
  !$acc               packed_nsout,packed_refl,packed_arefl,packed_areflz, &
  !$acc               packed_frefl,packed_csrfl,packed_acsrfl,packed_fcsrfl, &
  !$acc               packed_rercld,packed_ncai,packed_ncal,packed_qrout2, &
  !$acc               packed_qsout2,packed_nrout2,packed_nsout2,drout_dum,dsout2_dum, &
  !$acc               packed_freqs,packed_freqr,packed_nfice,packed_qcrat,packed_qgout, &
  !$acc               packed_dgout,packed_ngout,packed_qgout2,packed_ngout2,packed_dgout2, &
  !$acc               packed_freqg,packed_prer_evap)
#if defined(OPENMP_GPU)
!$omp target data map(to:packed_t,packed_q,packed_qc,packed_qi,&
!$omp packed_nc,packed_ni,packed_qr,packed_qs,packed_nr,packed_ns,&
!$omp packed_qg,packed_ng,packed_relvar,packed_accre_enhan,packed_p,&
!$omp packed_pdel,packed_cldn,packed_liqcldf,packed_icecldf,&
!$omp packed_qsatfac,packed_naai,packed_npccn,packed_rndst,&
!$omp packed_nacon,packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
!$omp packed_frzimm,packed_frzcnt,packed_frzdep,mg_liq_props,&
!$omp mg_ice_props,mg_rain_props,mg_graupel_props,mg_hail_props,&
!$omp mg_snow_props) map(from:packed_rate1ord_cw2pr_st,packed_tlat,&
!$omp packed_qvlat,packed_qctend,packed_qitend,packed_nctend,&
!$omp packed_nitend,packed_qrtend,packed_qstend,packed_nrtend,&
!$omp packed_nstend,packed_qgtend,packed_ngtend,packed_rel,rel_fn_dum,&
!$omp packed_rei,packed_sadice,packed_sadsnow,packed_prect,&
!$omp packed_preci,packed_nevapr,packed_evapsnow,packed_am_evp_st,&
!$omp packed_prain,packed_prodsnow,packed_cmeout,packed_dei,packed_mu,&
!$omp packed_lambdac,packed_qsout,packed_des,packed_iflx,packed_gflx,&
!$omp packed_rflx,packed_sflx,packed_cflx,packed_qrout,reff_rain_dum,&
!$omp reff_snow_dum,reff_grau_dum,packed_qcsevap,packed_qisevap,&
!$omp packed_qvres,packed_cmei,packed_vtrmc,packed_vtrmi,packed_umr,&
!$omp packed_ums,packed_umg,packed_qgsedten,packed_qcsedten,&
!$omp packed_qisedten,packed_qrsedten,packed_qssedten,packed_pra,&
!$omp packed_prc,packed_mnuccc,packed_mnucct,packed_mnuccc,&
!$omp packed_mnucct,packed_msacwi,packed_psacws,packed_bergs,&
!$omp packed_berg,packed_melt,packed_meltstot,packed_meltgtot,&
!$omp packed_mnudeptot,packed_homo,packed_qcres,packed_prci,&
!$omp packed_prai,packed_qires,packed_mnuccr,packed_mnuccri,&
!$omp packed_pracs,packed_meltsdt,packed_frzrdt,packed_mnuccd,&
!$omp packed_pracg,packed_psacwg,packed_pgsacw,packed_pgracs,&
!$omp packed_prdg,packed_qmultg,packed_qmultrg,packed_psacr,&
!$omp packed_npracg,packed_nscng,packed_ngracs,packed_nmultg,&
!$omp packed_nmultrg,packed_npsacwg,packed_nrout,packed_nsout,&
!$omp packed_refl,packed_arefl,packed_areflz,packed_frefl,packed_csrfl,&
!$omp packed_acsrfl,packed_fcsrfl,packed_rercld,packed_ncai,&
!$omp packed_ncal,packed_qrout2,packed_qsout2,packed_nrout2,&
!$omp packed_nsout2,drout_dum,dsout2_dum,packed_freqs,packed_freqr,&
!$omp packed_nfice,packed_qcrat,packed_qgout,packed_dgout,packed_ngout,&
!$omp packed_qgout2,packed_ngout2,packed_dgout2,packed_freqg,&
!$omp packed_prer_evap)
#endif // defined(OPENMP_GPU)

             CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
             DO kgen_intvar = 1, KGEN_MAXITER 
         call micro_mg_tend3_0( &
              DFACT*mgncol,         nlev,           dtime/num_steps,&
              packed_t,               packed_q,               &
              packed_qc,              packed_qi,              &
              packed_nc,              packed_ni,              &
              packed_qr,              packed_qs,              &
              packed_nr,              packed_ns,              &
              packed_qg,              packed_ng,              &
              packed_relvar,          packed_accre_enhan,     &
              packed_p,               packed_pdel,            &
              packed_cldn, packed_liqcldf, packed_icecldf, packed_qsatfac, &
              packed_rate1ord_cw2pr_st,                       &
              packed_naai,            packed_npccn,           &
              packed_rndst,           packed_nacon,           &
              packed_tlat,            packed_qvlat,           &
              packed_qctend,          packed_qitend,          &
              packed_nctend,          packed_nitend,          &
              packed_qrtend,          packed_qstend,          &
              packed_nrtend,          packed_nstend,          &
              packed_qgtend,          packed_ngtend,          &
              packed_rel,     rel_fn_dum,     packed_rei,     &
              packed_sadice,          packed_sadsnow,         &
              packed_prect,           packed_preci,           &
              packed_nevapr,          packed_evapsnow,        &
              packed_am_evp_st,                               &
              packed_prain,           packed_prodsnow,        &
              packed_cmeout,          packed_dei,             &
              packed_mu,              packed_lambdac,         &
              packed_qsout,           packed_des,             &
              packed_qgout,   packed_ngout,   packed_dgout,   &
              packed_cflx,    packed_iflx,                    &
              packed_gflx,                                    &
              packed_rflx,    packed_sflx,    packed_qrout,   &
              reff_rain_dum,          reff_snow_dum,   reff_grau_dum,       &
              packed_qcsevap, packed_qisevap, packed_qvres,   &
              packed_cmei,    packed_vtrmc,   packed_vtrmi,   &
              packed_umr,             packed_ums,             &
              packed_umg,             packed_qgsedten,        &
              packed_qcsedten,        packed_qisedten,        &
              packed_qrsedten,        packed_qssedten,        &
              packed_pra,             packed_prc,             &
              packed_mnuccc,  packed_mnucct,  packed_msacwi,  &
              packed_psacws,  packed_bergs,   packed_berg,    &
              packed_melt,    packed_meltstot, packed_meltgtot,           packed_homo,            &
              packed_qcres,   packed_prci,    packed_prai,    &
              packed_qires,   packed_mnuccr,  packed_mnudeptot, packed_mnuccri, packed_pracs,   &
              packed_meltsdt, packed_frzrdt,  packed_mnuccd,  &
              packed_pracg,   packed_psacwg,  packed_pgsacw,  &
              packed_pgracs,  packed_prdg,   &
              packed_qmultg,  packed_qmultrg, packed_psacr,   &
              packed_npracg,  packed_nscng,   packed_ngracs,  &
              packed_nmultg,  packed_nmultrg, packed_npsacwg, & 
              packed_nrout,           packed_nsout,           &
              packed_refl,    packed_arefl,   packed_areflz,  &
              packed_frefl,   packed_csrfl,   packed_acsrfl,  &
              packed_fcsrfl,          packed_rercld,          &
              packed_ncai,            packed_ncal,            &
              packed_qrout2,          packed_qsout2,          &
              packed_nrout2,          packed_nsout2,          &
              drout_dum,              dsout2_dum,             &
              packed_qgout2, packed_ngout2, packed_dgout2, packed_freqg,   &
              packed_freqs,           packed_freqr,           &
              packed_nfice,           packed_qcrat,           &
              errstring, &
              packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
              packed_prer_evap,                                     &
              packed_frzimm,  packed_frzcnt,  packed_frzdep   )
             END DO   
             CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
  !$acc end data
#if defined(OPENMP_GPU)
!$omp end target data
#endif // defined(OPENMP_GPU)
             kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock*KGEN_MAXITER) 
#ifdef _MPI
            call MPI_AllReduce(kgen_measure,gkgen_measure,1,MPI_REAL8,MPI_MAX,MPI_COMM_WORLD,info)
            IF (check_status%rank==0) THEN 
               WRITE (*, 200) DFACT*mgncol, gkgen_measure
               kgen_measure=gkgen_measure
            END IF
#else
            IF (check_status%rank==0) THEN 
               WRITE (*, 200) DFACT*mgncol, kgen_measure
            END IF   
#endif
         END IF
         IF (kgen_warmupstage) THEN 
         END IF   
         IF (kgen_evalstage) THEN 
         END IF   

   200 format(" mg3_tend: DFACT*mgncol: ",i8," Time per call (usec): ",f11.2)
   ! Divide ptend by substeps.

   ! Use summed outputs to produce averages


   ! Check to make sure that the microphysics code is respecting the flags that control
   ! whether MG should be prognosing cloud ice and cloud liquid or not.


   !add condensate fluxes for MG2 (ice and snow already added for MG1)


   !add graupel fluxes for MG3 to snow flux 


   !! calculate effective radius of convective liquid and ice using dcon and deicon (not used by code, not useful for COSP)
   !! hard-coded as average of hard-coded values used for deep/shallow convective detrainment (near line 1502/1505)

   ! Reassign rate1 if modal aerosols


   ! Sedimentation velocity for liquid stratus cloud droplet

   ! Microphysical tendencies for use in the macrophysics at the next time step

   ! Net micro_mg_cam condensation rate


   ! For precip, accumulate only total precip in prec_pcw and snow_pcw variables.
   ! Other precip output variables are set to 0
   ! Do not subscript by ncol here, because in physpkg we divide the whole
   ! array and need to avoid an FPE due to uninitialized data.


   ! ------------------------------------------------------------ !
   ! Compute in cloud ice and liquid mixing ratios                !
   ! Note that 'iclwp, iciwp' are used for radiation computation. !
   ! ------------------------------------------------------------ !


   ! Calculate cloud fraction for prognostic precip sizes.


   ! ------------------------------------------------------ !
   ! ------------------------------------------------------ !
   ! All code from here to the end is on grid columns only  !
   ! ------------------------------------------------------ !
   ! ------------------------------------------------------ !
   ! Average the fields which are needed later in this paramterization to be on the grid


   ! If on subcolumns, average the rest of the pbuf fields which were modified on subcolumns but are not used further in
   ! this parameterization  (no need to assign in the non-subcolumn case -- the else step)


   ! ------------------------------------- !
   ! Size distribution calculation         !
   ! ------------------------------------- !
   ! Calculate rho (on subcolumns if turned on) for size distribution
   ! parameter calculations and average it if needed
   ! State instead of state_loc to preserve answers for MG1 (and in any
   ! case, it is unlikely to make much difference).


   !

   ! Effective radius for cloud liquid, fixed number.


   ! Effective radius for cloud liquid, and size parameters
   ! mu_grid and lambdac_grid.

   ! Calculate ncic on the grid


   ! Rain/Snow effective diameter.


! Graupel/Hail size distribution Placeholder


   ! Effective radius and diameter for cloud ice.


   ! Limiters for low cloud fraction.


   ! ------------------------------------- !
   ! Precipitation efficiency Calculation  !
   ! ------------------------------------- !
   !-----------------------------------------------------------------------
   ! Liquid water path
   ! Compute liquid water paths, and column condensation


   ! note: 1e-6 kgho2/kgair/s * 1000. pa / (9.81 m/s2) / 1000 kgh2o/m3 = 1e-7 m/s
   ! this is 1ppmv of h2o in 10hpa
   ! alternatively: 0.1 mm/day * 1.e-4 m/mm * 1/86400 day/s = 1.e-9
   !-----------------------------------------------------------------------
   ! precipitation efficiency calculation  (accumulate cme and precip)


   ! zero out precip efficiency and total averaged precip

   ! accumulate precip and condensation


   !-----------------------------------------------------------------------
   ! vertical average of non-zero accretion, autoconversion and ratio.
   ! vars: vprco_grid(i),vprao_grid(i),racau_grid(i),cnt_grid


   ! --------------------- !
   ! History Output Fields !
   ! --------------------- !
   ! Column droplet concentration


   ! Averaging for new output fields


   ! Cloud top effective radius and number.


   ! Evaporation of stratiform precipitation fields for UNICON


   ! Assign the values to the pbuf pointers if they exist in pbuf


   ! --------------------------------------------- !
   ! General outfield calls for microphysics       !
   ! --------------------------------------------- !
   ! Output a handle of variables which are calculated on the fly


   ! Output fields which have not been averaged already, averaging if use_subcol_microp is true


   ! Example subcolumn outfld call


   ! Output fields which are already on the grid


   ! ptend_loc is deallocated in physics_update above

           
         CONTAINS 
           

         !read state subroutine for kr_micro_mg_cam_tend_pack_real__r8_dim2 
         SUBROUTINE kr_micro_mg_cam_tend_pack_real__r8_dim2(var, kgen_unit, printname, printvar) 
             REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
             INTEGER, INTENT(IN) :: kgen_unit 
             CHARACTER(LEN=*), INTENT(IN) :: printname 
             LOGICAL, INTENT(IN), OPTIONAL :: printvar 
             LOGICAL :: kgen_istrue 
             REAL(KIND=8) :: kgen_array_sum 
             INTEGER :: idx1, idx2 
             INTEGER :: i
             INTEGER, DIMENSION(2,2) :: kgen_bound 
             REAL(KIND=r8), allocatable,  DIMENSION(:,:) :: var_io

             READ (UNIT = kgen_unit) kgen_istrue
             IF (kgen_istrue) THEN 
                 IF (ALLOCATED( var )) THEN 
                     DEALLOCATE (var) 
                 END IF   
                 READ (UNIT = kgen_unit) kgen_array_sum 
                 READ (UNIT = kgen_unit) kgen_bound(1, 1) 
                 READ (UNIT = kgen_unit) kgen_bound(2, 1) 
                 READ (UNIT = kgen_unit) kgen_bound(1, 2) 
                 READ (UNIT = kgen_unit) kgen_bound(2, 2) 
                 ALLOCATE (var(kgen_bound(1,1):DFACT*kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
                 ALLOCATE (var_io(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2)))
                 READ (UNIT = kgen_unit) var_io
                 CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var_io, mask=.TRUE.)), .TRUE.)
                 do i = 1, DFACT
                    var((i-1)*kgen_bound(2,1)+1:i*kgen_bound(2,1),:) = var_io(:,:)
                 end do
                 IF (PRESENT( printvar ) .AND. printvar) THEN 
                     WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var_io, mask=.TRUE.)) 
                 END IF   
                 deallocate(var_io)
             END IF   
         END SUBROUTINE kr_micro_mg_cam_tend_pack_real__r8_dim2 
           
         !read state subroutine for kr_micro_mg_cam_tend_pack_real__r8_dim3 
         SUBROUTINE kr_micro_mg_cam_tend_pack_real__r8_dim3(var, kgen_unit, printname, printvar) 
             REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var 
             INTEGER, INTENT(IN) :: kgen_unit 
             CHARACTER(LEN=*), INTENT(IN) :: printname 
             LOGICAL, INTENT(IN), OPTIONAL :: printvar 
             LOGICAL :: kgen_istrue 
             REAL(KIND=8) :: kgen_array_sum 
             INTEGER :: idx1, idx2, idx3 
             INTEGER, DIMENSION(2,3) :: kgen_bound
             INTEGER :: i
             REAL(KIND=r8), ALLOCATABLE, DIMENSION(:,:,:) :: var_io

             READ (UNIT = kgen_unit) kgen_istrue 
             IF (kgen_istrue) THEN 
                 IF (ALLOCATED( var )) THEN 
                     DEALLOCATE (var) 
                 END IF   
                 READ (UNIT = kgen_unit) kgen_array_sum 
                 READ (UNIT = kgen_unit) kgen_bound(1, 1) 
                 READ (UNIT = kgen_unit) kgen_bound(2, 1) 
                 READ (UNIT = kgen_unit) kgen_bound(1, 2) 
                 READ (UNIT = kgen_unit) kgen_bound(2, 2) 
                 READ (UNIT = kgen_unit) kgen_bound(1, 3) 
                 READ (UNIT = kgen_unit) kgen_bound(2, 3) 
                 ALLOCATE (var(kgen_bound(1,1):DFACT*kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), &
                 &kgen_bound(1,3):kgen_bound(2,3)))
                 ALLOCATE (var_io(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), &
                 &kgen_bound(1,3):kgen_bound(2,3)))
                 READ (UNIT = kgen_unit) var_io
                 do i = 1, DFACT
                    var((i-1)*kgen_bound(2,1)+1:i*kgen_bound(2,1),:,:) = var_io(:,:,:)
                 end do
                 CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var_io, mask=.TRUE.)), .TRUE.) 
                 IF (PRESENT( printvar ) .AND. printvar) THEN 
                     WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var_io, mask=.TRUE.)) 
                 END IF
                 DEALLOCATE(var_io)
             END IF   
         END SUBROUTINE kr_micro_mg_cam_tend_pack_real__r8_dim3 
           
         !verify state subroutine for kv_micro_mg_cam_tend_pack_real__r8_dim2 
         RECURSIVE SUBROUTINE kv_micro_mg_cam_tend_pack_real__r8_dim2(varname, check_status, var, kgenref_var) 
             CHARACTER(LEN=*), INTENT(IN) :: varname 
             TYPE(check_t), INTENT(INOUT) :: check_status 
             REAL(KIND=r8), INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
             INTEGER :: check_result 
             LOGICAL :: is_print = .FALSE. 
               
             INTEGER :: idx1, idx2 
             INTEGER :: n 
             real(KIND=r8) :: nrmsdiff, rmsdiff 
             real(KIND=r8), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
               
             check_status%numTotal = check_status%numTotal + 1 
               
             IF (ALL(var == kgenref_var)) THEN 
                 check_status%numIdentical = check_status%numIdentical + 1 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                     END IF   
                 END IF   
                 check_result = CHECK_IDENTICAL 
             ELSE 
                 ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                 ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                 n = SIZE(var) 
                 WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                     buf1 = ((var-kgenref_var)/kgenref_var)**2 
                     buf2 = (var-kgenref_var)**2 
                 ELSEWHERE 
                     buf1 = (var-kgenref_var)**2 
                     buf2 = buf1 
                 END WHERE   
                 nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
                 rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
                 IF (rmsdiff > kgen_tolerance) THEN 
                     check_status%numOutTol = check_status%numOutTol + 1 
                     IF (kgen_verboseLevel > 0) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                         END IF   
                     END IF   
                     check_result = CHECK_OUT_TOL 
                 ELSE 
                     check_status%numInTol = check_status%numInTol + 1 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                         END IF   
                     END IF   
                     check_result = CHECK_IN_TOL 
                 END IF   
             END IF   
             IF (check_result == CHECK_IDENTICAL) THEN 
                 IF (kgen_verboseLevel > 2) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                         WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                         WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                         WRITE (*, *) "RMS of difference is ", 0 
                         WRITE (*, *) "Normalized RMS of difference is ", 0 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                 IF (kgen_verboseLevel > 0) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                         WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                         WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                         WRITE (*, *) "RMS of difference is ", rmsdiff 
                         WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             ELSE IF (check_result == CHECK_IN_TOL) THEN 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                         WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                         WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                         WRITE (*, *) "RMS of difference is ", rmsdiff 
                         WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             END IF   
               
         END SUBROUTINE kv_micro_mg_cam_tend_pack_real__r8_dim2 
           
         !verify state subroutine for kv_micro_mg_cam_tend_pack_real__r8_dim1 
         RECURSIVE SUBROUTINE kv_micro_mg_cam_tend_pack_real__r8_dim1(varname, check_status, var, kgenref_var) 
             CHARACTER(LEN=*), INTENT(IN) :: varname 
             TYPE(check_t), INTENT(INOUT) :: check_status 
             REAL(KIND=r8), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
             INTEGER :: check_result 
             LOGICAL :: is_print = .FALSE. 
               
             INTEGER :: idx1 
             INTEGER :: n 
             real(KIND=r8) :: nrmsdiff, rmsdiff 
             real(KIND=r8), ALLOCATABLE :: buf1(:), buf2(:) 
               
             check_status%numTotal = check_status%numTotal + 1 
               
             IF (ALL(var == kgenref_var)) THEN 
                 check_status%numIdentical = check_status%numIdentical + 1 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                     END IF   
                 END IF   
                 check_result = CHECK_IDENTICAL 
             ELSE 
                 ALLOCATE (buf1(SIZE(var,dim=1))) 
                 ALLOCATE (buf2(SIZE(var,dim=1))) 
                 n = SIZE(var) 
                 WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                     buf1 = ((var-kgenref_var)/kgenref_var)**2 
                     buf2 = (var-kgenref_var)**2 
                 ELSEWHERE 
                     buf1 = (var-kgenref_var)**2 
                     buf2 = buf1 
                 END WHERE   
                 nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
                 rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
                 IF (rmsdiff > kgen_tolerance) THEN 
                     check_status%numOutTol = check_status%numOutTol + 1 
                     IF (kgen_verboseLevel > 0) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                         END IF   
                     END IF   
                     check_result = CHECK_OUT_TOL 
                 ELSE 
                     check_status%numInTol = check_status%numInTol + 1 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                         END IF   
                     END IF   
                     check_result = CHECK_IN_TOL 
                 END IF   
             END IF   
             IF (check_result == CHECK_IDENTICAL) THEN 
                 IF (kgen_verboseLevel > 2) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                         WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                         WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                         WRITE (*, *) "RMS of difference is ", 0 
                         WRITE (*, *) "Normalized RMS of difference is ", 0 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                 IF (kgen_verboseLevel > 0) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                         WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                         WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                         WRITE (*, *) "RMS of difference is ", rmsdiff 
                         WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             ELSE IF (check_result == CHECK_IN_TOL) THEN 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                         WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                         WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                         WRITE (*, *) "RMS of difference is ", rmsdiff 
                         WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             END IF   
               
         END SUBROUTINE kv_micro_mg_cam_tend_pack_real__r8_dim1 
           
         !verify state subroutine for kv_micro_mg_cam_tend_pack_character_128_ 
         RECURSIVE SUBROUTINE kv_micro_mg_cam_tend_pack_character_128_(varname, check_status, var, kgenref_var) 
             CHARACTER(LEN=*), INTENT(IN) :: varname 
             TYPE(check_t), INTENT(INOUT) :: check_status 
             CHARACTER(LEN=128), INTENT(IN) :: var, kgenref_var 
             INTEGER :: check_result 
             LOGICAL :: is_print = .FALSE. 
               
             character(LEN=128) :: diff 
               
             check_status%numTotal = check_status%numTotal + 1 
               
             IF (var == kgenref_var) THEN 
                 check_status%numIdentical = check_status%numIdentical + 1 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                     END IF   
                 END IF   
                 check_result = CHECK_IDENTICAL 
             ELSE 
                 check_status%numOutTol = check_status%numOutTol + 1 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
                     END IF   
                 END IF   
                 check_result = CHECK_OUT_TOL 
             END IF   
             IF (check_result == CHECK_IDENTICAL) THEN 
                 IF (kgen_verboseLevel > 2) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) "NOT IMPLEMENTED" 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                 IF (kgen_verboseLevel > 0) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) "NOT IMPLEMENTED" 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             ELSE IF (check_result == CHECK_IN_TOL) THEN 
                 IF (kgen_verboseLevel > 1) THEN 
                     IF (check_status%rank == 0) THEN 
                         WRITE (*, *) "NOT IMPLEMENTED" 
                         WRITE (*, *) "" 
                     END IF   
                 END IF   
             END IF   
               
         END SUBROUTINE kv_micro_mg_cam_tend_pack_character_128_ 
           
END SUBROUTINE micro_mg_cam_tend_pack 


!read state subroutine for kr_externs_in_micro_mg_cam 
SUBROUTINE kr_externs_in_micro_mg_cam(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) num_steps 
END SUBROUTINE kr_externs_in_micro_mg_cam 
  
!read state subroutine for kr_externs_out_micro_mg_cam 
SUBROUTINE kr_externs_out_micro_mg_cam(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_micro_mg_cam 

subroutine read1Darray(funit,name,dim1,var)
   integer, intent(in) :: funit
   character(len=*), intent(in) :: name
   integer, intent(in) :: dim1
   real(kind=r8), intent(inout), dimension(DFACT*dim1) :: var

   !local
   logical istrue
   real(kind=r8) array_sum
   real(kind=r8), dimension(dim1) :: tmp

   integer i

   read (unit=funit) istrue
   if (istrue) then
      READ (unit=funit) array_sum
      READ (unit=funit) tmp
      call kgen_array_sumcheck(TRIM(name), array_sum, DBLE(SUM(tmp,mask=.true.)), .true.)
      do i = 1, DFACT
        var((i-1)*dim1+1:i*dim1) = tmp(1:dim1)
      end do
   endif
end subroutine read1Darray

subroutine read2Darray(funit,name,dim1,dim2,var)
   integer, intent(in) :: funit
   character(len=*), intent(in) :: name
   integer, intent(in) :: dim1, dim2
   real(kind=r8), intent(inout), dimension(DFACT*dim1,dim2) :: var

   !local
   logical istrue
   real(kind=r8) array_sum
   real(kind=r8), dimension(dim1,dim2) :: tmp

   integer i

   read (unit=funit) istrue
   if (istrue) then
      READ (unit=funit) array_sum
      READ (unit=funit) tmp
      call kgen_array_sumcheck(TRIM(name), array_sum, DBLE(SUM(tmp,mask=.true.)), .true.)
      do i = 1, DFACT
        var((i-1)*dim1+1:i*dim1,1:dim2) = tmp(1:dim1,1:dim2)
      end do
   end if
end subroutine read2Darray

end module micro_mg_cam
