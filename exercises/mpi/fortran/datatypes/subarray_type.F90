program datatype2

  use mpi

  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for block datatype
integer :: subarray_type
integer :: status(MPI_STATUS_SIZE)
  integer, dimension(2) :: sizes, subsizes, offsets
  integer :: i, j
  
  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if


  !TODO: create a datatype for a subblock [2:5][3:5] of the 8x8 matrix
  sizes(1) = 8
  sizes(2) = 8
  subsizes(1) = 4
  subsizes(2) = 3
  offsets(1) = 0
  offsets(2) = 0

call mpi_type_create_subarray(2, sizes, subsizes, offsets, MPI_ORDER_FORTRAN, mpi_integer, subarray_type, ierr)
call mpi_type_commit(subarray_type, ierr)

  !TODO: send a block of a matrix from rank 0 to rank 1
if(rank == 0) then
call mpi_send(array(2, 3), 1, subarray_type, 1, 0, MPI_COMM_WORLD, ierr)
else if (rank == 1) then
call mpi_recv(array(2, 3), 1, subarray_type, 0, 0, MPI_COMM_WORLD, status, ierr)
end if

  ! Print out the result
  if (rank == 1) then
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if
      
  !TODO: free mpi datatype	
call mpi_type_free(subarray_type, ierr)

  call mpi_finalize(ierr)

end program datatype2
