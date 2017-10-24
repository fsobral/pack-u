module items
  
  implicit none

  ! This module implements the type 'Item' and its associate utility
  ! subroutines.
  
  type Item

     ! Item identification
     integer :: number
     ! Item type
     integer :: type
     ! Item's dimensions
     real(8) :: length, width
     ! Item position
     real(8) :: x, y
     
  end type Item

contains

  ! ***********************************************************
  ! ***********************************************************

  subroutine createSameItems(n, type, len, wid, nStart, v)

    ! This subroutine populates a (previously allocated) vector 'v'
    ! with 'n' items of type 'type' and given dimensions 'len' and
    ! 'wid'. Their identifying numbers will be generated from 'nStart'
    ! until 'nStart' + 'n' - 1.
    
    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: n, type, nStart
    real(8) :: len, wid
    
    ! ARRAY ARGUMENTS
    type(Item) :: v(n)

    intent(in)  :: n, type, nStart, len, wid
    intent(out) :: v
    
    ! LOCAL SCALARS
    integer :: i

    do i = 1, n

       v(i) = Item(nStart + i - 1, type, len, wid, 0.0D0, 0.0D0)
       
    end do
    
  end subroutine createSameItems
  
end module items
