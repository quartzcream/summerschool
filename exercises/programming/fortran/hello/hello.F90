program hello
  implicit none
  integer :: n

  read (*,*) n
  write (*,*) 'Hello world from Fortran!'
  write (*,'(I5.5)') n
end program hello
