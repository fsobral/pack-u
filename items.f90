module items
  
  implicit none

  ! This module implements the type 'Item' and its associate utility
  ! subroutines.
  
  type Item

     integer :: number, type
     real(8) :: length, width
     
  end type Item

contains

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

    ! LOCAL SCALARS
    integer :: i

    do i = 1, n

       v(i) = Item(nStart + i - 1, type, len, wid)
       
    end do
    
  end subroutine createSameItems
  
end module items
