module items_test

  use fruit

  implicit none
  
contains

  subroutine test_create_simple_item

    use items

    implicit none

    type(Item) :: it

    it = Item(10, 1, 1.5D0, 5.0D0, 1.0D0, 2.0D0)

    call assert_equals(it%number, 10)

    call assert_equals(it%type, 1)

    call assert_equals(it%length, 1.5D0)

    call assert_equals(it%width, 5.0D0)

    call assert_equals(it%x, 1.0D0)

    call assert_equals(it%y, 2.0D0)
    
  end subroutine test_create_simple_item

end module items_test
