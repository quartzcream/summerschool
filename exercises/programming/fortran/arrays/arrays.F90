program arrays
  implicit none
  integer :: nx, ny
  integer :: i, alloc_stat
  real, allocatable :: arr(:,:)
  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(arr(0:nx+1, 0:ny+1), stat=alloc_stat)
  if(alloc_stat /= 0) call abort()
  arr(:, :) = -1
  arr(1:nx, 1:ny) = 1
  do i = 0, nx+1
    write(*,'(*(F6.1))') arr(i,:)
  end do


end program arrays
