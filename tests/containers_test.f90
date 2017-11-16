module containers_test

  use fruit

  implicit none
  
contains

  subroutine test_empty_container

    use containers

    type(Container) :: c

    c = emptyContainer(1, 0, 10.0D0, 5.0D0)

    call assert_equals(1, c%type)

    call assert_equals(0, c%class)

    call assert_equals(10.0D0, c%length)

    call assert_equals(5.0D0, c%width)

    call assert_equals(0, c%nItems)
    
  end subroutine test_empty_container
  
! ******************************************************************
! ******************************************************************

  subroutine test_simple_container_no_items

    use containers

    type(Container) :: c

    c%type = 1

    c%class = 2

    c%length = 10.0D0

    c%width = 20.0D0

    c%nItems = 0

    call assert_equals(1, c%type)

    call assert_equals(2, c%class)

    call assert_equals(10.0D0, c%length)

    call assert_equals(20.0D0, c%width)

    call assert_equals(0, c%nItems)

    call assert_false(allocated(c%pItems))

  end subroutine test_simple_container_no_items

end module containers_test
