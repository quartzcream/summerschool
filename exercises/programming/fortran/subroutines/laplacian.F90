module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
    real, intent(out) :: field0(0:,0:)

    integer :: ex, ey
    ex = ubound(field0, 1)
    ey = ubound(field0, 2)
    field0(:, :) = 1
    field0(:, 0) = 10
    field0(0, :) = 20
    field0(:, ey) = 30
    field0(ex, :) = 40
    ! TODO: implement a subroutine that initializes the input array

  end subroutine initialize

  subroutine laplacian(curr, prev)
    real, intent(in)  :: prev(0:,0:)
    real, intent(out) :: curr(0:,0:)

    integer :: i, j
    curr(:,:)  = 0.0  ! middle

    do j=1, ubound(prev,2)-1
      do i=1, ubound(prev,1)-1
        curr(i,j) = (prev(i-1, j) - 2*prev(i,j) + prev(i+1, j))/dx**2 + &
        (prev(i, j-1) - 2*prev(i,j) + prev(i, j+1))/dy**2
      end do
    end do
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
  end subroutine laplacian

  subroutine write_field(array)
    real, intent(in) :: array(0:,0:)

    integer :: i
    do i=0, ubound(array, 1)
      write(*,'(*(E10.1))') array(i,:)
    end do
! TODO: write a subroutine that prints "array" on screen
  end subroutine write_field

end module laplacian_mod
