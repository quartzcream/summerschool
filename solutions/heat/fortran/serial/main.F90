! Heat equation solver in 2D.

program heat_solve
  use mpi

  use heat_core_mod
  use heat
  use core
  use io
  use setup
  use utilities

  implicit none

  real(dp), parameter :: a = 0.5 ! Diffusion constant
  type(field) :: current, previous    ! Current and previus temperature fields

  real(dp) :: dt     ! Time step
  integer :: nsteps       ! Number of time steps
  integer, parameter :: image_interval = 10 ! Image output interval

  ! for optimized version trial
  integer, parameter :: jump_steps = 128
  integer, parameter :: loc_len = 1024
  real(dp), allocatable, dimension(:, :)    :: side_t     ! x, y
  real(dp), allocatable, dimension(:, :, :) :: side_x     ! x, y, t
  real(dp), allocatable, dimension(:, :, :) :: side_y     ! x, y, t
  real(dp), allocatable, dimension(:, :, :) :: tmp_side_x     ! x, y, t
  real(dp), allocatable, dimension(:, :, :) :: tmp_side_y     ! x, y, t
  integer, dimension(3, 3) :: rec_lengths ! (x, y, t), small-to-big

  integer :: err_mpi
  integer, dimension(MPI_STATUS_SIZE) :: status
  integer :: task_cnt, my_id
  integer :: cart_comm
  integer, dimension(2) :: dims
  integer, dimension(2) :: periods = (\ 1, 1 \)
  real(dp), pointer, dimension(0:, 0:) :: p_big_side_t
  real(dp), allocatable, dimension(:, :, :)    :: mpi_side_t     ! x, y
  integer :: i, j
  integer :: tarket_id

  integer :: len = 0
  integer :: iter

  real :: start, stop ! Timers

  call mpi_initialize(err_mpi)
  call mpi_comm_size(task_cnt, err_mpi)
  do while ((len+1)**2 <= task_cnt)
    len = len + 1
  end do
  dims = len
  call mpi_cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, cart_comm)
  call mpi_comm_rank(my_id, err_mpi)

  if (my_id >= len**2) then
    call mpi_finalize(err_mpi)
    return
  end if
  if (my_id == 0) then
    call initialize(current, previous, nsteps)
    p_big_side_t => current%data(lbound(current%data, 1)+1:lbound(current%data, 1)+loc_len*len, &
       lbound(current%data, 2)+1:lbound(current%data, 2)+loc_len*len)
    allocate(mpi_side_t(loc_len, loc_len, len**2))
    do i=0, len-1
      do j=0, len-1
        mpi_cart_rank(cart_comm, (\ i, j \), tarket_id, err_mpi)
        mpi_side_t(:, :, tarket_id) = p_big_side_t(i*loc_len:(i+1)*loc_len-1, j*loc_len:(j+1)*loc_len-1)
      end do
    end do
  end if
  allocate(side_t(0:loc_len-1, 0:loc_len-1))
  mpi_scatter(mpi_side_t, len**2, mpi_double, side_t, loc_len**2, mpi_double, 0, cart_comm, err_mpi)
  

  rec_lengths = reshape((/ &
                            32,  32,  16, &
                            64,  64,  64, &
                           256, 256,  128  &
                          /), shape(rec_lengths))

  allocate(side_x(2, loc_len, jump_steps))
  allocate(side_y(loc_len/rec_lengths(1,1)*(rec_lengths(1,1)+2), 2, jump_steps))

  allocate(tmp_side_x(2, loc_len, jump_steps))
  allocate(tmp_side_y(loc_len/rec_lengths(1,1)*(rec_lengths(1,1)+2), 2, jump_steps))

  ! Draw the picture of the initial state
  if (my_id == 0) then
    call write_field(current, 0)
  end if

  ! Largest stable time step
  dt = current%dx**2 * current%dy**2 / &
       & (2.0 * a * (current%dx**2 + current%dy**2))

  ! Main iteration loop, save a picture every
  ! image_interval steps

  call cpu_time(start)
  do iter = 1, nsteps/jump_steps
     call split_block(side_t, &
       side_x, side_y, a*dt/current%dx**2, rec_lengths)
     !     call calc_block(current%data(:ubound(current%data, 1)-2, :ubound(current%data, 2)-2), side_x, side_y, 0*a*dt/current%dx**2)
     !! call evolve(current, previous, a, dt)
     !if (mod(iter, image_interval) == 0) then
     !   call write_field(current, iter)
     !end if
     !! call swap_fields(current, previous)
  end do

  call cpu_time(stop)
  call write_field(current, iter*jump_steps-nsteps)

  write(*,'(A,F7.3,A,I3)') 'Iteration took ', stop - start, ' seconds for rank ', my_id
  write(*,'(A,G0)') 'Reference value at 5,5: ', previous % data(5,5)
  
  call finalize(current, previous)

  call mpi_finalize(err_mpi)

end program heat_solve
