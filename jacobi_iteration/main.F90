program jacobi_iteration 

#ifdef _OPENACC
   use openacc
#endif

   use omp_lib

   implicit none

   integer, parameter :: wp = selected_real_kind(15)

   character(10) :: rowsChar
   character(10) :: colsChar
   integer, parameter :: DEFAULT_DIM = 1024
   integer, parameter :: ITER_MAX = 50000 
   real(wp), parameter :: BC = 10._wp
   real(wp), parameter :: VERIF_TOL = 1.e-2_wp   ! tolerance for verification test
   integer :: i, j, iter, rows, cols, ii, jj
   real(wp) :: t1, t2, dt, error
   real(wp), allocatable, dimension(:,:) :: a_new, a_cpu, a_gpu

   if( COMMAND_ARGUMENT_COUNT() .EQ. 0 ) then
        rows = DEFAULT_DIM
        cols = DEFAULT_DIM 
   else if( COMMAND_ARGUMENT_COUNT() .EQ. 2 ) then
        call GET_COMMAND_ARGUMENT( 1, rowsChar )   !first, read in the two values
        call GET_COMMAND_ARGUMENT( 2, colsChar )
        read( rowsChar, * ) rows
        read( colsChar, * ) cols
        ! Sanity Check
        if( cols .le. 0 ) then
          write(*,*) 'ERROR, number of columns (besides left/right boundary) must be larger than 0'
          stop
        endif
        if( rows .le. 0 ) then
          write(*,*) 'ERROR, number of rows (besides top/bottom boundary) must be larger than 0'
          stop
        endif
   else
        write(*,*)'ERROR, Usage: ./jacobi_iteration.exe rows cols'
        stop
   endif

!Initialize timing information

   t1 = omp_get_wtime()

   allocate( a_cpu(0:rows+1,0:cols+1), a_gpu(0:rows+1,0:cols+1), a_new(rows,cols) )

! Initialize inner elements of a_cpu and a_gpu with zero

   do j = 1, cols
      do i = 1, rows
         a_cpu(i,j) = 0._wp
         a_gpu(i,j) = 0._wp
      end do
   end do

! Initialize the boundary conditions  with fixed values

   do j = 1, cols     
      a_cpu(0,j) = BC
      a_cpu(rows+1,j) = BC
      a_gpu(0,j) = BC
      a_gpu(rows+1,j) = BC
   end do

   do i = 1, rows
      a_cpu(i,0) = BC
      a_cpu(i,cols+1) = BC
      a_gpu(i,0) = BC
      a_gpu(i,cols+1) = BC
   end do

   t2 = omp_get_wtime()

   dt = t2-t1
   write(*,"('Initialization done for domain size ',i6,' x ',i6,' in ',f12.5,'secs')") rows+2,cols+2,dt

! Compute Jacobi iteration on CPU

   t1 = omp_get_wtime()

   do iter = 1, ITER_MAX
      do j = 1, cols
         do i = 1, rows
            a_new(i,j) = 0.25_wp * (a_cpu(i,j-1) + &
                                    a_cpu(i-1,j) + &
                                    a_cpu(i+1,j) + &
                                    a_cpu(i,j+1))
         end do
      end do

      do j = 1, cols
         do i = 1, rows
            a_cpu(i,j) = a_new(i,j)
         end do
      end do
   end do 
    
   t2 = omp_get_wtime()

   dt = t2-t1
   write(*,"('CPU Jacobi iteration completed in ',f12.5,' secs with ',i6,' iterations')") dt, iter 

#ifdef _OPENACC
! Compute Jacobi iteration on GPU (OpenACC)

   t1 = omp_get_wtime()

   !$acc data copy (a_gpu) create(a_new)
   do iter = 1, ITER_MAX
      !$acc parallel vector_length(128) default(present)
      !$acc loop gang vector collapse (2)
      do j = 1, cols
         do i = 1, rows
            a_new(i,j) = 0.25_wp * (a_gpu(i,j-1) + &
                                    a_gpu(i-1,j) + &
                                    a_gpu(i+1,j) + &
                                    a_gpu(i,j+1))
         end do
      end do

      !$acc loop gang vector collapse (2)
      do j = 1, cols
         do i = 1, rows
            a_gpu(i,j) = a_new(i,j)
         end do
      end do
      !$acc end parallel

   end do
   !$acc end data

   t2 = omp_get_wtime()

   dt = t2-t1
   write(*,"('GPU Jacobi iteration completed in ',f12.5,' secs with ',i6,' iterations')") dt, iter
#endif

#ifdef _OPENMP
! Compute Jacobi iteration on GPU (OpenMP)

   t1 = omp_get_wtime()

   !$omp target data map (tofrom:a_gpu) map (alloc:a_new)
   do iter = 1, ITER_MAX
      !$omp target teams distribute parallel do simd collapse(2)
      do j = 1, cols
         do i = 1, rows
            a_new(i,j) = 0.25_wp * (a_gpu(i,j-1) + &
                                    a_gpu(i-1,j) + &
                                    a_gpu(i+1,j) + &
                                    a_gpu(i,j+1))
         end do
      end do
      !$omp end target teams distribute parallel do simd

      !$omp target teams distribute parallel do simd collapse(2)
      do j = 1, cols
         do i = 1, rows
            a_gpu(i,j) = a_new(i,j)
         end do
      end do
      !$omp end target teams distribute parallel do simd
   end do
   !$omp end target data

   t2 = omp_get_wtime()

   dt = t2-t1
   write(*,"('GPU Jacobi iteration completed in ',f12.5,' secs with ',i6,' iterations')") dt, iter
#endif

! Verify GPU results against CPU for inner elements only
   error = 0._wp
   ii = 0
   jj = 0
   do j = 1, cols
      do i = 1, rows
         if (abs(a_gpu(i,j)-a_cpu(i,j)) > error) then
             ii = i
             jj = j
             error = abs(a_gpu(i,j)-a_cpu(i,j))
         end if
      end do
   end do

   if ( error < VERIF_TOL ) then 
      write(*,"('Verification passed')")
      write(*,"('   Max abs error = ',f15.8,' at ii = ',i6,', jj = ',i6,'')") error, ii, jj
   else
      write(*,"('Verification failed')")
      write(*,"('   Max relative error > tolerance encountered at A_CPU[',i6,'][',i6,']')") ii, jj
      write(*,"('   A_CPU[',i6,'][',i6,']=',G15.8,'')") ii,jj,a_cpu(ii,jj)
      write(*,"('   A_GPU[',i6,'][',i6,']=',G15.8,'')") ii,jj,a_gpu(ii,jj)
      write(*,"('   ABS(A_GPU-A_CPU) =',f15.8,'')") error 
   end if

!Release Memory to cleanup program
   deallocate(a_cpu,a_gpu,a_new)

end program jacobi_iteration 
