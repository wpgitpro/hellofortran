module fame

implicit none
! real(kind=8) :: pi = 3.141592653589793d0

contains 

subroutine fsub(x,f)
  ! compute f(x) = x**2 for all elements of the array x. 
  implicit none
  real(kind=8), dimension(3), intent(in) :: x
  real(kind=8), dimension(size(x)), intent(out) :: f
  f = x**2
end subroutine fsub

end module fame