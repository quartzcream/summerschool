! Heat equation solver in 2D.

program heat_solve
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
  real(dp), allocatable, dimension(:, :, :) :: side_x     ! x, y, t
  real(dp), allocatable, dimension(:, :, :) :: side_y     ! x, y, t
  integer, dimension(3, 3) :: rec_lengths ! (x, y, t), small-to-big

  integer :: iter

  real :: start, stop ! Timers

  call initialize(current, previous, nsteps)

  rec_lengths = reshape((/ &
                            32,  32,   16, &
                            64,  64,   64, &
                           256, 256,  128  &
                          /), shape(rec_lengths))

  allocate(side_x(2, current%ny, jump_steps))
  allocate(side_y(current%nx/rec_lengths(1,1)*(rec_lengths(1,1)+2), 2, jump_steps))

  ! Draw the picture of the initial state
  call write_field(current, 0)

  ! Largest stable time step
  dt = current%dx**2 * current%dy**2 / &
       & (2.0 * a * (current%dx**2 + current%dy**2))

  ! Main iteration loop, save a picture every
  ! image_interval steps

  call cpu_time(start)
  do iter = 1, nsteps/jump_steps
     call split_block(current%data(lbound(current%data, 1)+1:ubound(current%data, 1)-1, &
       lbound(current%data, 2)+1:ubound(current%data, 2)-1), &
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

  write(*,'(A,F7.3,A)') 'Iteration took ', stop - start, ' seconds.'
  write(*,'(A,G0)') 'Reference value at 5,5: ', previous % data(5,5)
  
  call finalize(current, previous)

end program heat_solve
