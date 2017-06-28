program laplacian
  implicit none

  integer, parameter :: nx = 10, ny = 10
  real, dimension(0:nx+1, 0:ny+1) :: prev, lapl
  integer :: i, j

  real, parameter :: dx = 0.01, dy = 0.01



  ! initialize prev array with varying boundaries
  prev(:,:)  = 65.0 ! middle
  prev(:,0)  = 20.0 ! left
  prev(:,ny+1) = 70.0 ! right
  prev(0,:)  = 85.0 ! top
  prev(nx+1,:) = 5.0  ! bottom

  ! initialize lapl array to zeros
  lapl(:,:)  = 0.0  ! middle


  do j=1,ny 
    do i=1,nx
      lapl(i,j) = (prev(i-1, j) - 2*prev(i,j) + prev(i+1, j))/dx**2 + &
        (prev(i, j-1) - 2*prev(i,j) + prev(i, j+1))/dy**2
    end do
  end do
  !-------------------------------------------------- 
  ! TODO: implement Laplacian in double do-loop using prev 
  ! and saving to lapl array. Remember to evaluate it only
  ! at the inner points.









  !--------------------------------------------------
  ! Printing of the prev and lapl arrays
  write(*,*) "Previous array:"
  do i = 0, nx+1
    write(*,'(*(G10.1))') prev(i,:)
  end do

  write(*,*) "Laplacian of the array:"
  do i = 0, nx+1
    write(*,'(*(G10.1))') lapl(i,:)
  end do

end program laplacian
