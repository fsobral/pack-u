module items
  
  implicit none

  ! This module implements the type 'Item' and its associate utility
  ! subroutines.

  type ItemType

     sequence

     ! Item ID
     integer :: id
     
     ! Item class
     integer :: class
     
     ! Item's dimensions
     real(8) :: length, width

  end type ItemType
  
  type Item

     sequence
     
     ! Item identification
     integer :: number
     
     ! Item type
     type(ItemType), pointer :: type => NULL()
     
     ! Item position
     real(8) :: x, y
     
  end type Item

  public
  
contains

! ******************************************************************
! ******************************************************************

  subroutine createSameItems(n, itype, nStart, v)

    ! This subroutine populates a (previously allocated) vector 'v'
    ! with 'n' items of type 'itype' and given dimensions 'len' and
    ! 'wid'. Their identifying numbers will be generated from 'nStart'
    ! until 'nStart' + 'n' - 1.
    
    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: n, nStart
    real(8) :: len, wid
    type(ItemType), target :: itype
    
    ! ARRAY ARGUMENTS
    type(Item) :: v(n)

    intent(in)  :: n, itype, nStart
    intent(out) :: v
    
    ! LOCAL SCALARS
    integer :: i

    do i = 1, n

       v(i) = Item(nStart + i - 1, itype, 0.0D0, 0.0D0)
       
    end do
    
  end subroutine createSameItems

! ******************************************************************
! ******************************************************************

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

! ******************************************************************
! ******************************************************************

  subroutine toVector(nItems, v, n, x, inform)

    ! This subroutine converts an array of items to a vector of x and
    ! y positions.

    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: nItems, n
    integer, optional :: inform

    ! ARRAY ARGUMENTS
    type(Item) :: v(nItems)
    real(8)    :: x(n)

    intent(in)  :: nItems, v, n
    intent(out) :: x, inform

    ! LOCAL SCALARS
    integer :: i

    if (n .ne. 2 * nItems) then

       write(*, *) 'ERROR: mismatching dimensions in toVector(..)!'

       if (present(inform)) inform = 1
       
       return

    end if

    do i = 1, nItems

       x(2 * i - 1) = v(i)%x

       x(2 * i)     = v(i)%y

    end do

    if (present(inform)) inform = 0
    
  end subroutine toVector

! ******************************************************************
! ******************************************************************

  function getIArea(it)

    ! This function return the area of item 'it'.

    implicit none

    ! SCALAR ARGUMENT
    type(Item), intent(in) :: it

    ! RETURN
    real(8) :: getIArea

    getIArea = getTypeArea(it%type)

  end function getIArea

! ******************************************************************
! ******************************************************************

  function getTypeArea(itype)

    ! This function return the area of item 'it'.

    implicit none

    ! SCALAR ARGUMENT
    type(ItemType), intent(in) :: itype

    ! RETURN
    real(8) :: getTypeArea

    getTypeArea = itype%length * itype%width

  end function getTypeArea

end module items
