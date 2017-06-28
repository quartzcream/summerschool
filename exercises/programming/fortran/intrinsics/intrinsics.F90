program intrinsics
  implicit none
  integer :: nx, ny
  integer :: i, alloc_stat
  real, dimension(:,:), allocatable :: A

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(A(nx,ny), stat = alloc_stat)
  if (alloc_stat /= 0) call abort()

  ! Initializing array
  A(:,:)  = 65.0 ! middle
  A(:,1)  = 20.0 ! left
  A(:,ny) = 70.0 ! right
  A(1,:)  = 85.0 ! top
  A(nx,:) = 5.0  ! bottom

  !--------------------------------------------------
  ! Print out the array
  do i = 1, nx
    write(*,'(*(F6.1))') A(i,:)
  end do


  !--------------------------------------------------
  ! TODO: use array intrinsics to probe elements of A
  write(*,'(F6.1)') sum(A, 2)
  write(*,*) maxloc(A)
  write(*,*) -maxval(A, A<0)
  write(*,'(F6.1)') min(-maxval(A, A<0), minval(A, A>=0))
  write(*,*) minval(A) >= 0
  write(*,*) count(A >= 0.5)




end program intrinsics
