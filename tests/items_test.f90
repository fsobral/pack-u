module items_test

  use fruit

  implicit none
  
contains

! ******************************************************************
! ******************************************************************

  subroutine test_create_simple_item

    use items, only : Item, ItemType

    implicit none

    type(Item) :: it

    type(ItemType), target :: itp

    itp = ItemType(1, 0, 1.5D0, 5.0D0)

    it = Item(10, itp, 1.0D0, 2.0D0)

    call assert_equals(10, it%number)

    call assert_item_type_equals(itp, it%type)

    call assert_equals(1.0D0, it%x)

    call assert_equals(2.0D0, it%y)
    
  end subroutine test_create_simple_item

! ******************************************************************
! ******************************************************************

  subroutine test_create_same_items

    use items, only : Item, ItemType, createSameItems

    implicit none

    ! LOCAL SCALARS
    integer :: i, n
    type(Item) :: it
    type(ItemType), target :: itp

    !LOCAL ARRAYS
    type(Item), allocatable :: vItems(:)

    itp = ItemType(5, 0, 1.5D0, 3.0D0)
    
    n = 15
    
    allocate(vItems(n))

    call createSameItems(n, itp, 17, vItems)

    do i = 1, n

       it = vItems(i)
       
       call assert_equals(17 + i - 1, it%number)

       call assert_item_type_equals(itp, it%type)

       call assert_equals(0.0D0, it%x)

       call assert_equals(0.0D0, it%y)

    end do

  end subroutine test_create_same_items

! ******************************************************************
! ******************************************************************

  subroutine test_swap_items

    use items, only : Item, ItemType, createSameItems, swapItems

    implicit none

    ! LOCAL SCALARS
    type(Item) :: it1, it2, it3

    !LOCAL ARRAYS
    type(Item) :: vItems(3)
    type(ItemType), target :: itp

    itp = ItemType(2, 1, 1.5D0, 3.0D0)

    call createSameItems(3, itp, 1, vItems)

    it1 = vItems(1)

    it2 = vItems(3)

    it3 = vItems(2)
    
    call swapItems(vItems, 1, 3)

    call assert_items_equals(it1, vItems(3))

    call assert_items_equals(it2, vItems(1))

    call assert_items_equals(it3, vItems(2))

  end subroutine test_swap_items

! ******************************************************************
! ******************************************************************

  subroutine test_to_vector

    use items, only : Item, ItemType, toVector, createSameItems

    implicit none

    ! LOCAL SCALARS
    integer i, n, inform
    type(Item) :: it
    type(ItemType), target :: itp
    
    ! LOCAL ARRAYS
    type(Item) :: vItems(4)
    real(8) :: x(2 * 4)

    itp = ItemType(1, 0, 5.0D0, 3.0D0)
    
    n = 4
    
    call createSameItems(n, itp, 1, vItems)

    call toVector(n, vItems, 2 * n, x, inform)

    do i = 1, n

       it = vItems(i)

       call assert_equals(it%x, x(2 * i - 1))

       call assert_equals(it%y, x(2 * i))

    end do

    call assert_equals(0, inform)
    
  end subroutine test_to_vector
  
! ******************************************************************
! ******************************************************************

  subroutine test_to_vector_wrong_size

    use items, only : Item, ItemType, toVector, createSameItems

    implicit none

    ! LOCAL SCALARS
    integer i, n, inform
    type(Item) :: it
    type(ItemType), target :: itp
    
    ! LOCAL ARRAYS
    type(Item) :: vItems(4)
    real(8) :: x(4)

    itp = ItemType(3, 0, 5.0D0, 3.0D0)
    
    n = 4
    
    call createSameItems(n, itp, 1, vItems)

    call toVector(n, vItems, n, x, inform)

    call assert_equals(1, inform)
        
  end subroutine test_to_vector_wrong_size

! ******************************************************************
! ******************************************************************

  subroutine test_area

    use items, only : Item, ItemType, getIArea

    ! LOCAL SCALARS
    type(Item) :: it
    type(ItemType), target :: itp

    itp = ItemType(1, 0, 10.0D0, 2.0D0)

    it = Item(1, itp, 0.0D0, 0.0D0)

    call assert_equals(10.0D0 * 2.0D0, getIArea(it))
    
  end subroutine test_area

! ******************************************************************
! ******************************************************************

  subroutine assert_items_equals(it1, it2)

    ! Auxiliary subroutine for testing two items

    use items, only : Item

    implicit none

    ! SCALAR ARGUMENTS
    type(Item), intent(in) :: it1, it2

    call assert_equals(it1%number, it2%number)

    call assert_item_type_equals(it1%type, it2%type)

    call assert_equals(it1%x, it2%x)

    call assert_equals(it1%y, it2%y)

  end subroutine assert_items_equals
  
! ******************************************************************
! ******************************************************************

  subroutine assert_item_type_equals(itp1, itp2)

    ! Auxiliary subroutine for testing two items

    use items, only : ItemType

    implicit none

    ! SCALAR ARGUMENTS
    type(ItemType), intent(in) :: itp1, itp2

    call assert_equals(itp1%id, itp2%id)
    
    call assert_equals(itp1%class, itp2%class)

    call assert_equals(itp1%length, itp2%length)

    call assert_equals(itp1%width, itp2%width)

  end subroutine assert_item_type_equals

! ******************************************************************
! ******************************************************************

  subroutine test_change_item_type_online

    use items, only : Item, ItemType

    type(ItemType), target :: itt

    type(Item) :: it

    itt = ItemType(5, 1, 0.0D0, 0.0D0)

    it = Item(10, itt, 0.0D0, 0.0D0)

    call assert_equals(itt%width, it%type%width)

    itt%width = 1.0D0
    
    call assert_equals(itt%width, it%type%width)

  end subroutine test_change_item_type_online

! ******************************************************************
! ******************************************************************

  subroutine test_is_type_a_pointer

    use items, only : Item

    type(Item) :: it1, it2
    
    type(Item), target :: it3
    
    type(Item), pointer :: pit

    it1 = Item(1, NULL(), 0.0D0, 0.0D0)

    it2 = it1

    it1%x = 1.0D0

    call assert_not_equals(it1%x, it2%x)

    it3 = Item(2, NULL(), 2.0D0, 2.0D0)

    pit => it3

    it3%y = 0.0D0

    call assert_equals(it3%y, pit%y)

  end subroutine test_is_type_a_pointer
    
end module items_test
