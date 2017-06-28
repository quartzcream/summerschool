program loops
  implicit none
  integer, parameter :: nx = 10, ny = 10
  real :: arr(0:nx+1, 0:ny+1)
  ! TODO define parameters nx and ny
  ! TODO: define real-valued array A
  integer :: i, j

  ! TODO initialize array A here
  arr(1:nx, 0) = 10
  arr(1:nx, ny+1) = 30
  arr(0, 1:ny) = 20
  arr(nx+1, 1:ny) = 40
  arr(1:nx, 1:ny) = 1

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 0, nx+1
     write(*, '(12F6.1)') arr(i,:)
  end do

end program loops
