module packdat_test

  use fruit

  implicit none
  
contains

! ******************************************************************
! ******************************************************************

  subroutine setSeed(s)

    implicit none

    integer, intent(in) :: s

    integer :: i, nseed
    
    integer, allocatable :: seed(:)

    call random_seed(SIZE=nseed)

    allocate(seed(nseed))

    do i = 1,nseed
       seed(i) = s
    end do

    call random_seed(PUT=seed)

  end subroutine setSeed

! ******************************************************************
! ******************************************************************
  
  subroutine createFiles(it, nit, cont, ncont, qIt)

    use items, only : Item

    use containers, only : Container
    
    implicit none

    type(Container) :: cont(ncont)

    type(Item) :: it(nit)
    
    integer :: nit, ncont, qIt(nit)

    intent(in) :: it, nit, cont, ncont, qIt
    
    integer :: i

    real(8) :: typ

    ! Generate random items
    
    open(99, FILE = "items.txt")

    open(98, FILE = "data.txt")

    write(99, *) nit
    
    do i = 1, nit
       
       call random_number(typ)

       write(99, FMT = 100) it(i)%length, it(i)%width, &
            NINT(2.0D0 * typ)

       write(98, *) qIt(i)

    end do

    close(99)

    close(98)

    ! Generate random containers
    
    open(99, FILE = "containers.txt")

    write(99, *) ncont
    
    do i = 1, ncont

       call random_number(typ)
       
       write(99, FMT = 100) cont(i)%length, &
            cont(i)%width, NINT(2.0D0 * typ)

    end do

    close(99)

100 FORMAT(F5.1,1X,F5.1,1X,I2)
    
  end subroutine createFiles
  
! ******************************************************************
! ******************************************************************

  subroutine test_load_data

    use packdat, only : loadData, nTItems, nItems

    use items, only : Item

    use containers, only : Container, emptyContainer

    integer, parameter :: nit = 4, ncont = 3
    
    character(80) :: filename

    integer :: i
    
    integer :: qIt(4) = (/10, 20, 30, 40/)

    type(Item) :: it(nit)

    type(Container) :: cont(ncont)
    
    call setSeed(543210)

    do i = 1, nit

       it(i) = Item(0, i, 0, 10.0D0 + i, 20.0D0 + i, 0.0D0, 0.0D0)

    end do

    do i = 1, ncont

       cont(i) = emptyContainer(i, 100.0D0 + i, 200.0D0 - i)

    end do

    filename = "data.txt"
    
    call createFiles(it, nit, cont, ncont, qIt)
    
    call loadData(filename)
    
    call assert_equals(sum(qIt), nTItems)

    call assert_equals(nItems, nTItems)

    ! TODO: test types, sizes, etc.

  end subroutine test_load_data

end module packdat_test
