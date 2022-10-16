module gnlink

implicit none
private 
real, public, parameter :: &
pi = 3.1415 , &
e = 2.7183 , &
gamma = 0.57722

contains

   subroutine sub1()
      print *, "In sub1"
   end subroutine sub1

   subroutine Polar_rect(llen,langle)
      real(kind=8), intent(in) :: llen,langle
      ! real intent(out) :: link
      ! Don't do anything
   end subroutine Polar_rect

end module gnlink