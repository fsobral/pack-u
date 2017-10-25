module items_test

  use fruit

  implicit none
  
contains

! ******************************************************************
! ******************************************************************

  subroutine test_create_simple_item

    use items

    implicit none

    type(Item) :: it

    it = Item(10, 1, 1.5D0, 5.0D0, 1.0D0, 2.0D0)

    call assert_equals(10, it%number)

    call assert_equals(1, it%type)

    call assert_equals(1.5D0, it%length)

    call assert_equals(5.0D0, it%width)

    call assert_equals(1.0D0, it%x)

    call assert_equals(2.0D0, it%y)
    
  end subroutine test_create_simple_item

! ******************************************************************
! ******************************************************************

  subroutine test_create_same_items

    use items, only : Item, createSameItems

    implicit none

    ! LOCAL SCALARS
    integer :: i, n
    type(Item) :: it

    !LOCAL ARRAYS
    type(Item), allocatable :: vItems(:)

    n = 15

    allocate(vItems(n))

    call createSameItems(n, 3, 1.5D0, 3.0D0, 17, vItems)

    do i = 1, n

       it = vItems(i)
       
       call assert_equals(17 + i - 1, it%number)

       call assert_equals(3, it%type)

       call assert_equals(1.5D0, it%length)

       call assert_equals(3.0D0, it%width)

       call assert_equals(0.0D0, it%x)

       call assert_equals(0.0D0, it%y)

    end do

  end subroutine test_create_same_items

! ******************************************************************
! ******************************************************************

  subroutine test_swap_items

    use items, only : Item, createSameItems, swapItems

    implicit none

    ! LOCAL SCALARS
    type(Item) :: it1, it2, it3

    !LOCAL ARRAYS
    type(Item) :: vItems(3)

    call createSameItems(3, 1, 1.5D0, 3.0D0, 1, vItems)

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

    use items, only : Item, toVector, createSameItems

    implicit none

    ! LOCAL SCALARS
    integer i, n, inform
    type(Item) :: it
    
    ! LOCAL ARRAYS
    type(Item) :: vItems(4)
    real(8) :: x(2 * 4)

    n = 4
    
    call createSameItems(n, 2, 5.0D0, 3.0D0, 1, vItems)

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

    use items, only : Item, toVector, createSameItems

    implicit none

    ! LOCAL SCALARS
    integer i, n, inform
    type(Item) :: it
    
    ! LOCAL ARRAYS
    type(Item) :: vItems(4)
    real(8) :: x(4)

    n = 4
    
    call createSameItems(n, 2, 5.0D0, 3.0D0, 1, vItems)

    call toVector(n, vItems, n, x, inform)

    call assert_equals(1, inform)
        
  end subroutine test_to_vector_wrong_size

! ******************************************************************
! ******************************************************************

  subroutine test_area

    use items, only : Item, getIArea

    ! LOCAL SCALARS
    type(Item) :: it

    it = Item(1, 1, 10.0D0, 2.0D0, 0.0D0, 0.0D0)

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

    call assert_equals(it1%type, it2%type)

    call assert_equals(it1%length, it2%length)

    call assert_equals(it1%width, it2%width)

    call assert_equals(it1%x, it2%x)

    call assert_equals(it1%y, it2%y)

  end subroutine assert_items_equals
  
end module items_test
