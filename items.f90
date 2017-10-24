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

  ! ***********************************************************
  ! ***********************************************************

  subroutine swapItems(v, i, j)

    ! Swaps positions 'i' and 'j' in the vector of items 'v'
    
    implicit none

    ! SCALAR ARGUMENTS
    integer :: i, j
    
    ! ARRAY ARGUMENTS
    type(Item) :: v(:)

    intent(in)    :: i, j
    intent(inout) :: v
    
    ! LOCAL SCALARS
    type(Item) :: tmp

    tmp = v(i)

    v(i) = v(j)

    v(j) = tmp
    
  end subroutine swapItems

  subroutine toVector(nItems, v, n, x)

    ! This subroutine converts an array of items to a vector of x and
    ! y positions.

    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: nItems, n

    ! ARRAY ARGUMENTS
    type(Item) :: v(nItems)
    real(8)    :: x(n)

    intent(in)  :: nItems, v, n
    intent(out) :: x

    ! LOCAL SCALARS
    integer :: i

    if (n .ne. 2 * nItems) then

       write(*, *) 'ERROR: mismatching dimensions in toVector(..)!'

       return

    end if

    do i = 1, nItems

       x(2 * i - 1) = v(i)%x

       x(2 * i)     = v(i)%y

    end do
    
  end subroutine toVector
  
end module items
