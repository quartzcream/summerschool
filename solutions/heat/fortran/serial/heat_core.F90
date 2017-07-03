module heat_core_mod
  use iso_fortran_env, only : REAL64
  implicit none 
contains
  recursive subroutine split_block(side_t, side_x, side_y, multiplier, rec_lengths)
                                           ! indexing
    real(REAL64), intent(inout), dimension(0:, 0:) :: side_t     ! x, y
    real(REAL64), intent(inout), dimension(size(side_t, 1):, 0:, 0:) :: side_x     ! x, y, t
    real(REAL64), intent(inout), dimension(0:, size(side_t, 2):, 0:) :: side_y     ! x(includes lower corner), y, t
    integer :: dims(3) ! x, y, t
    real(REAL64), intent(in) :: multiplier
    integer, intent(in), dimension(1:, 0:) :: rec_lengths ! (x, y, t), small-to-big

    integer, dimension(3) :: cur_split
    integer, dimension(3) :: cur_coord
    integer, dimension(3) :: first_coord
    integer, dimension(3) :: last_coord
    integer               :: first_side_y, last_side_y

    dims(1) = size(side_t, 1)
    dims(2) = size(side_t, 2)
    dims(3) = size(side_x, 3)
    cur_split(:) = rec_lengths(: , size(rec_lengths, 2)-1)
    cur_coord(1:2) = dims(1:2)
    cur_coord(3) = 0
    do while (cur_coord(3)<dims(3))
      cur_coord(2) = dims(2)
      do while (cur_coord(2)>0)
        cur_coord(1) = dims(1)
        do while (cur_coord(1)>0)
          first_coord(:) = cur_coord(:)
          first_coord(1:2) = first_coord(1:2) - cur_split(1:2)
          last_coord(:) = cur_coord(:)
          last_coord(1:2) = last_coord(1:2) - 1
          last_coord(3) = last_coord(3) + cur_split(3) - 1
          first_side_y = (cur_coord(1)-cur_split(1))/rec_lengths(1, 0)*(rec_lengths(1, 0)+2)
          last_side_y = cur_coord(1)/rec_lengths(1, 0)*(rec_lengths(1, 0)+2)-1
          if (size(rec_lengths, 2) == 1) then
            call calc_block(side_t(first_coord(1):last_coord(1), first_coord(2):last_coord(2)), &
              side_x(:, first_coord(2):last_coord(2), first_coord(3):last_coord(3)), &
              side_y(first_side_y:last_side_y, :, first_coord(3):last_coord(3)), &
              multiplier)
          else
            call split_block(side_t(first_coord(1):last_coord(1), first_coord(2):last_coord(2)), &
              side_x(:, first_coord(2):last_coord(2), first_coord(3):last_coord(3)), &
              side_y(first_side_y:last_side_y, :, first_coord(3):last_coord(3)), &
              multiplier, rec_lengths(:, 0:size(rec_lengths, 2)-2))
          end if
          cur_coord(1) = cur_coord(1) - cur_split(1)
        end do
        cur_coord(2) = cur_coord(2) - cur_split(2)
      end do
      cur_coord(3) = cur_coord(3) + cur_split(3)
    end do
  end subroutine split_block
  subroutine calc_block(side_t, side_x, side_y, multiplier)
                                           ! indexing
    real(REAL64), intent(inout), dimension(0:, 0:) :: side_t     ! x, y
    real(REAL64), intent(inout), dimension(size(side_t, 1):, 0:, 0:) :: side_x     ! x, y, t
    real(REAL64), intent(inout), dimension(0:, size(side_t, 2):, 0:) :: side_y     ! x(includes lower corner), y, t
    real(REAL64), intent(in) :: multiplier
    
    real(REAL64), allocatable , dimension(:, :) :: tmp_side_t   ! x, y
    integer :: alloc_stat
    integer :: dims(3) ! x, y, t
    integer :: t

    dims(1) = size(side_t, 1)
    dims(2) = size(side_t, 2)
    dims(3) = size(side_x, 3)
    allocate(tmp_side_t(0:dims(1)-1, 0:dims(2)-1))

    do t=0,dims(3)-1
      ! major area
      tmp_side_t(0:dims(1)-3, 0:dims(2)-3) = side_t(1:dims(1)-2, 1:dims(2)-2) + &
        multiplier*(side_t(0:dims(1)-3, 1:dims(2)-2) + side_t(1:dims(1)-2, 0:dims(2)-3) + &
        side_t(2:dims(1)-1, 1:dims(2)-2) + side_t(1:dims(1)-2, 2:dims(2)-1) - &
        4*side_t(1:dims(1)-2, 1:dims(2)-2))
      ! 2nd in from x border
      tmp_side_t(dims(1)-2, 0:dims(2)-3) = side_t(dims(1)-1, 1:dims(2)-2) + &
        multiplier*(side_t(dims(1)-2, 1:dims(2)-2) + side_t(dims(1)-1, 0:dims(2)-3) + &
        side_x(dims(1), 1:dims(2)-2, t) + side_t(dims(1)-1, 2:dims(2)-1) - &
        4*side_t(dims(1)-1, 1:dims(2)-2))
      ! x border
      tmp_side_t(dims(1)-1, 0:dims(2)-3) = side_x(dims(1), 1:dims(2)-2, t) + &
        multiplier*(side_t(dims(1)-1, 1:dims(2)-2) + side_x(dims(1), 0:dims(2)-3, t) + &
        side_x(dims(1)+1, 1:dims(2)-2, t) + side_x(dims(1), 2:dims(2)-1, t) - &
        4*side_x(dims(1), 1:dims(2)-2, t))
      ! 2nd in from y border
      tmp_side_t(0:dims(1)-3, dims(2)-2) = side_t(1:dims(1)-2, dims(2)-1) + &
        multiplier*(side_t(0:dims(1)-3, dims(2)-1) + side_t(1:dims(1)-2, dims(2)-2) + &
        side_t(2:dims(1)-1, dims(2)-1) + side_y(1:dims(1)-2, dims(2), t) - &
        4*side_t(1:dims(1)-2, dims(2)-1))
      ! y border
      tmp_side_t(0:dims(1)-2, dims(2)-1) = side_y(1:dims(1)-1, dims(2), t) + &
        multiplier*(side_y(0:dims(1)-2, dims(2), t) + side_t(1:dims(1)-1, dims(2)-1) + &
        side_y(2:dims(1), dims(2), t) + side_y(1:dims(1)-1, dims(2)+1, t) - &
        4*side_y(1:dims(1)-1, dims(2), t))
      ! corner inner x, inner y
      tmp_side_t(dims(1)-2, dims(2)-2) = side_t(dims(1)-1, dims(2)-1) + &
        multiplier*(side_t(dims(1)-2, dims(2)-1) + side_t(dims(1)-1, dims(2)-2) + &
        side_x(dims(1), dims(2)-1, t) + side_y(dims(1)-1, dims(2), t) - &
        4*side_t(dims(1)-1, dims(2)-1))
      ! corner outer x, inner y
      tmp_side_t(dims(1)-1, dims(2)-2) = side_x(dims(1), dims(2)-1, t) + &
        multiplier*(side_t(dims(1)-1, dims(2)-1) + side_x(dims(1), dims(2)-2, t) + &
        side_x(dims(1)+1, dims(2)-1, t) + side_y(dims(1), dims(2), t) - &
        4*side_x(dims(1), dims(2)-1, t))
      ! corner outer x, outer y
      tmp_side_t(dims(1)-1, dims(2)-1) = side_y(dims(1), dims(2), t) + &
        multiplier*(side_y(dims(1)-1, dims(2), t) + side_x(dims(1), dims(2)-1, t) + &
        side_y(dims(1)+1, dims(2), t) + side_y(dims(1), dims(2)+1, t) - &
        4*side_y(dims(1), dims(2), t))
      side_y(0:dims(1)-1, :, t) = side_t(:, 0:1)
      side_y(dims(1):dims(1)+1, :, t) = side_x(:, 0:1, t)
      side_x(:, :, t) = side_t(0:1, :)
      side_t = tmp_side_t
    end do
  end subroutine calc_block
end module heat_core_mod
