program matrix_mult
   use openacc
   implicit none
   character(10) :: rowsAChar
   character(10) :: colsAChar
   character(10) :: rowsBChar
   character(10) :: colsBChar
   integer, parameter:: DEFAULT_DIM=1024
   integer, parameter:: LOOP_COUNT=10
   real, parameter:: MAT_A_VAL=3.0
   real, parameter:: MAT_B_VAL=2.0
   real, parameter:: VERIF_TOL=1.0E-6
   integer :: i, j, k, n, ii, jj, rowsA, colsA, rowsB, colsB
   real :: t1, t2, dt
   real, allocatable, dimension(:,:) :: a, b, c_cpu, c_gpu
   real :: tmp, secs, error

   if(COMMAND_ARGUMENT_COUNT().EQ.0) then
        rowsA = DEFAULT_DIM
        colsA = DEFAULT_DIM 
        rowsB = DEFAULT_DIM
        colsB = DEFAULT_DIM
   else if(COMMAND_ARGUMENT_COUNT().EQ.4) then
        call GET_COMMAND_ARGUMENT(1,rowsAChar)   !first, read in the two values
        call GET_COMMAND_ARGUMENT(2,colsAChar)
        call GET_COMMAND_ARGUMENT(3,rowsBChar)
        call GET_COMMAND_ARGUMENT(4,colsBChar)
        read(rowsAChar,*)rowsA
        read(colsAChar,*)colsA
        read(rowsBChar,*)rowsB
        read(colsBChar,*)colsB
        !Check if the multiplication is possible 
        if(colsA .NE. rowsB) then
          write(*,*)'ERROR, Inner dimension mismatch. # Columns of Mat A must equal # Rows of Mat B'
          write(*,*)'Usage: ./matrix_mult.exe rowsA colsA rowsB colsB'
          stop
        endif
   else
        write(*,*)'ERROR, Usage: ./matrix_mult.exe rowsA colsA rowsB colsB'
        stop
   endif

!Initialize timing information

      call cpu_time(t1)

      allocate( a(rowsA,colsA), b(rowsB,colsB), c_cpu(rowsA,colsB), c_gpu(rowsA,colsB) )

! Initialize matrices
      do j=1,colsA
         do i=1,rowsA
            a(i,j) = MAT_A_VAL
         enddo
      enddo

      do j=1,colsB
         do i=1,rowsB
            b(i,j) = MAT_B_VAL
         enddo
      enddo

      call cpu_time(t2)
      dt = t2-t1
      secs = dt
      write(*,"('Initialized Mat A, size ',i6,' x ',i6,' and ')") rowsA,colsA
      write(*,"('Initialized Mat B, size ',i6,' x ',i6,' in ',f12.5,'secs')") rowsB,colsB, secs

! Compute matrix addition on CPU

#ifdef _CPU

      call cpu_time(t1)

      do n = 1, LOOP_COUNT
         do j=1,colsB
            do i=1,rowsA
               tmp = 0.0
               do k=1,rowsB
                   tmp = tmp + a(i,k) * b(k,j)
               enddo
               c_cpu(i,j) = tmp
            enddo
         enddo
      enddo
    
      call cpu_time(t2)
      dt = t2-t1
      secs = dt/real(LOOP_COUNT)
      write(*,"('CPU Matrix Multiplication completed in ',f12.5,' secs')") secs

#endif
 
! Compute matrix addition on GPU (OPENACC)

#ifdef _OPENACC

      call cpu_time(t1)

      do n = 1, LOOP_COUNT
         !$acc data copyin(a,b) copyout(c_gpu)
         !$acc parallel vector_length(128)
         !$acc loop gang vector collapse(2) reduction(+:tmp)
         do j=1,colsB
            do i=1,rowsA
               tmp = 0.0
               !$acc loop vector reduction(+:tmp)
               do k=1,rowsB
                   tmp = tmp + a(i,k) * b(k,j)
               enddo
               c_gpu(i,j) = tmp
            enddo
         enddo
         !$acc end parallel
         !$acc end data
      enddo

      call cpu_time(t2)
      dt = t2-t1
      secs = dt/real(LOOP_COUNT)
      write(*,"('GPU Matrix Multiplication completed in ',f12.5,' secs')") secs

#endif

! Compute matrix addition on GPU (OPENMP)

#ifdef _OPENMP

      call cpu_time(t1)

      do n = 1, LOOP_COUNT
         !$omp target data map(to:a,b) map(from:c_gpu)
         !$omp target teams distribute parallel do simd collapse (2) reduction(+:tmp)
         do j=1,colsB
            do i=1,rowsA
               tmp = 0.0
               do k=1,rowsB
                   tmp = tmp + a(i,k) * b(k,j)
               enddo
               c_gpu(i,j) = tmp
            enddo
         enddo
         !$omp end target teams distribute parallel do simd
         !$omp end target data
      enddo

      call cpu_time(t2)
      dt = t2-t1
      secs = dt/real(LOOP_COUNT)
      write(*,"('GPU Matrix Multiplication completed in ',f12.5,' secs')") secs

#endif

! Verify GPU results against CPU
      error = 0.d0
      ii = 0
      jj = 0
      jloop: do j=1,colsB
         do i=1,rowsA
            if (abs(c_gpu(i,j)-c_cpu(i,j)) > error) then
                ii = i
                jj = j
                error = abs(c_gpu(i,j)-c_cpu(i,j))
            end if
         end do
      end do jloop
   
      if ( error < VERIF_TOL ) then
         write(*,"('Verification passed')")
         write(*,"('   Max abs error = ',f25.16,' at ii = ',i6,', jj = ',i6,'')") error, ii, jj
      else
         write(*,"('Verification failed')")
         write(*,"('   Max relative error > tolerance encountered at C_CPU[',i6,'][',i6,']')") ii, jj
         write(*,"('   C_CPU[',i6,'][',i6,']=',G25.16,'')") ii,jj,c_cpu(ii,jj)
         write(*,"('   C_GPU[',i6,'][',i6,']=',G25.16,'')") ii,jj,c_gpu(ii,jj)
         write(*,"('   ABS(C_GPU-C_CPU) =',f25.16,'')") error
      end if

! Uncomment section to print CPU and GPU results 
     
!      write(*,"('CPU Results: ')")
!      do i=1,rowsA
!          do j=1,colsB
!               write(*,fmt="(f0.2, tr2)",advance="no") c_cpu(i,j)
!          enddo
!               write(*,"(' ')")
!      enddo
!      write(*,"('GPU Results: ')")
!      do i=1,rowsA
!          do j=1,colsB
!               write(*,fmt="(f0.2, tr2)",advance="no") c_gpu(i,j)
!          enddo
!               write(*,"(' ')")
!      enddo

!Release Memory to cleanup program
      deallocate(a, b,c_cpu,c_gpu)
end program matrix_mult

