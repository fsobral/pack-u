module containers_test

  use fruit

  implicit none
  
contains
  
  subroutine test_simple_container_no_items

    use containers

    type(Container) :: c

    c%type = 1

    c%length = 10.0D0

    c%width = 20.0D0

    c%nItems = 0

    call assert_equals(1, c%type)

    call assert_equals(10.0D0, c%length)

    call assert_equals(20.0D0, c%width)

    call assert_equals(0, c%nItems)

    call assert_false(allocated(c%pItems))

  end subroutine test_simple_container_no_items

end module containers_test
