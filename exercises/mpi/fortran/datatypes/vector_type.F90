program datatype1
  use mpi
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  integer :: row_type
  integer :: i, j
integer :: status

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

  !TODO: create datatype describing one row, use mpi_type_vector
  call mpi_type_vector(8, 1, 8, mpi_integer, row_type, ierr)
  call mpi_type_commit(row_type, ierr)

  !TODO: send first row of matrix from rank 0 to 1
  if(rank == 0) then
    call mpi_send(array(2,1), 1, row_type, 1, 0, MPI_COMM_WORLD, ierr)
  end if
  if(rank == 1) then
    call mpi_recv(array(3,1), 1, row_type, 0, 0, MPI_COMM_WORLD, status, ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype
  call mpi_type_free(row_type, ierr)

  call mpi_finalize(ierr)

end program datatype1
