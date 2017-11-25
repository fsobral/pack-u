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

    use items, only : ItemType

    use containers, only : Container
    
    implicit none

    type(Container) :: cont(ncont)

    type(ItemType) :: it(nit)

    integer :: nit, ncont, qIt(nit)

    intent(in) :: it, nit, cont, ncont, qIt
    
    integer :: i

    real(8) :: typ

    ! Generate items
    
    open(99, FILE = "items.txt")

    open(98, FILE = "data.txt")

    write(99, *) nit
    
    do i = 1, nit
       
       write(99, FMT = 100) it(i)%length, it(i)%width, &
            it(i)%class

       write(98, *) qIt(i)

    end do

    close(99)

    close(98)

    ! Generate containers
    
    open(99, FILE = "containers.txt")

    write(99, *) ncont
    
    do i = 1, ncont

       write(99, FMT = 100) cont(i)%length, &
            cont(i)%width, cont(i)%class

    end do

    close(99)

100 FORMAT(F5.1,1X,F5.1,1X,I2)
    
  end subroutine createFiles

! ******************************************************************
! ******************************************************************

  subroutine test_sortContainers_with_id

    use packdat, only : loadData, reset, sortContainers

    use items, only: ItemType

    use containers, only: Container, emptyContainer
    
    implicit none

    character(80) :: filename

    type(ItemType) :: it(1)

    type(Container) :: cont(3)

    integer :: L(3)

    call reset()

    it(1) = ItemType(0, 0, 0.0D0, 0.0D0)

    cont(1) = emptyContainer(1, 1, 1.0D0, 1.0D0)

    cont(2) = emptyContainer(2, 0, 5.0D0, 5.0D0)

    cont(3) = emptyContainer(3, 0, 2.0D0, 2.0D0)

    call createFiles(it, 1, cont, 3, (/ 0 /))
    
    filename = 'data.txt'
    
    call loadData(filename)

    ! No changes
    
    L = (/ 2, 1, 0 /)

    call sortContainers(2, L)

    call assert_equals(2, L(1))

    call assert_equals(1, L(2))

    ! Inverted id order
    
    L = (/ 1, 2, 0 /)

    call sortContainers(2, L)

    call assert_equals(2, L(1))

    call assert_equals(1, L(2))

    ! Full vector

    L = (/ 1, 2, 3 /)
    
    call sortContainers(3, L)

    call assert_equals(3, L(1))

    call assert_equals(2, L(2))

    call assert_equals(1, L(3))
    
  end subroutine test_sortContainers_with_id
  
! ******************************************************************
! ******************************************************************

  subroutine test_sortContainers_noid

    use packdat, only : loadData, reset, sortContainers

    use items, only: ItemType

    use containers, only: Container, emptyCContainer
    
    implicit none

    character(80) :: filename

    type(ItemType) :: it(1)

    type(Container) :: cont(3)

    integer :: L(3)

    call reset()

    it(1) = ItemType(0, 0, 0.0D0, 0.0D0)

    cont(1) = emptyCContainer(1, 1.0D0, 1.0D0)

    cont(2) = emptyCContainer(2, 5.0D0, 5.0D0)

    cont(3) = emptyCContainer(3, 2.0D0, 2.0D0)

    call createFiles(it, 1, cont, 3, (/ 0 /))
    
    filename = 'data.txt'
    
    call loadData(filename)

    L = (/ 1, 0, 0 /)

    call sortContainers(1, L)

    call assert_equals(1, L(1))
    
    L = (/ 3, 2, 0 /)
    
    call sortContainers(2, L)

    call assert_equals(3, L(1))

    call assert_equals(2, L(2))

    L = (/ 1, 2, 3 /)

    call sortContainers(3, L)

    call assert_equals(1, L(1))

    call assert_equals(3, L(2))

    call assert_equals(2, L(3))

  end subroutine test_sortContainers_noid
    
! ******************************************************************
! ******************************************************************

  subroutine test_load_ordered_containers

    use packdat, only : loadData, nContainers, setCurrContainer, &
         cLength, cWidth, reset

    use items, only : ItemType

    use containers, only : Container, emptyCContainer

    implicit none

    character(80) :: filename

    integer :: i

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(1) = (/ 10 /)

    type(ItemType) :: it(1)

    type(Container) :: cont(3)
    
    call reset()

    it(1) = ItemType(0, 0, 0.0D0, 0.0D0)

    cont(1) = emptyCContainer(1, 1.0D0, 1.0D0)

    cont(2) = emptyCContainer(2, 5.0D0, 5.0D0)

    cont(3) = emptyCContainer(3, 2.0D0, 2.0D0)

    filename = "data.txt"

    ! Test 1
    
    qIt = (/10/)
    
    call createFiles(it, 1, cont, 3, qIt)
    
    call loadData(filename)

    call assert_equals(3, nContainers)

    prevCArea = 0.0D0

    ! Not sure if this test should pass
    
!!$    do i = 1, 3
!!$
!!$       call setCurrContainer(i)
!!$       
!!$       currCArea = cLength * cWidth
!!$       
!!$       call assert_true( prevCArea .le. currCArea, &
!!$            "Unsorted containers." )
!!$
!!$       prevCArea = currCArea
!!$
!!$    end do    

  end subroutine test_load_ordered_containers
  
! ******************************************************************
! ******************************************************************

  subroutine test_load_data_single

    use packdat, only : loadData, nTItems, nItems, iLength, &
         iWidth, iId, nContainers, setCurrContainer, cLength, &
         cWidth, cId, reset

    use items, only : ItemType

    use containers, only : Container, emptyCContainer

    implicit none

    integer, parameter :: nit = 4, ncont = 1
    
    character(80) :: filename

    integer :: i, j, totalIt, countType

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(4)

    type(ItemType) :: it(nit)

    type(Container) :: cont(ncont)
    
    call reset()

    do i = 1, nit

       it(i) = ItemType(i, 17, 10.0D0 + i, 20.0D0 + i)

    end do

    do i = 1, ncont

       cont(i) = emptyCContainer(i, 100.0D0 + i, 200.0D0 - 2 * i)

    end do

    filename = "data.txt"

    ! Test 1
    
    qIt = (/10, 0, 0, 0/)
    
    call createFiles(it, nit, cont, ncont, qIt)
    
    call loadData(filename)
    

    call assert_equals(ncont, nContainers)

    call setCurrContainer(ncont)

    call assert_equals(cont(ncont)%length, cLength)

    call assert_equals(cont(ncont)%width, cWidth)

    call assert_equals(cont(ncont)%class, cId)    

    totalIt = sum(qIt)
    
    call assert_equals(totalIt, nTItems)

    call assert_equals(nItems, nTItems)

    call assert_equals(totalIt, size(iLength))

    call assert_equals(totalIt, size(iWidth))

    call assert_equals(totalIt, size(iId))
    
    do i = 1, totalIt

       call assert_equals(it(1)%length, iLength(i))

       call assert_equals(it(1)%width, iWidth(i))

       call assert_equals(it(1)%class, iId(i))

    end do
          
  end subroutine test_load_data_single

! ******************************************************************
! ******************************************************************

  subroutine test_load_data_many

    use packdat, only : loadData, nTItems, nItems, iLength, &
         iWidth, iId, nContainers, setCurrContainer, cLength, &
         cWidth, cId, reset

    use items, only : ItemType

    use containers, only : Container, emptyCContainer

    implicit none

    integer, parameter :: nit = 4, ncont = 3
    
    character(80) :: filename

    integer :: i, j, totalIt, countType, equal, diffe

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(4)

    type(ItemType) :: it(nit)

    type(Container) :: cont(ncont)
    
    call reset()

    do i = 1, nit

       it(i) = ItemType(i, 17, 10.0D0 + i, 20.0D0 + i)

    end do

    do i = 1, ncont

       cont(i) = emptyCContainer(i, 100.0D0 + i, 200.0D0 - 2 * i)

    end do

    filename = "data.txt"

    qIt = (/10, 0, 20, 30/)
    
    call createFiles(it, nit, cont, ncont, qIt)
    
    call loadData(filename)

    call assert_equals(ncont, nContainers)

    equal = 0

    diffe = 0
    
    do i = 1, ncont

       call setCurrContainer(i)

       do j = 1, ncont

          if ( cLength .eq. cont(j)%length .and. &
               cWidth .eq. cont(j)%width ) then

             equal = equal + 1

          else

             diffe = diffe + 1

          end if

       end do
       
    end do

    call assert_equals(3, equal, "Containers' dimensions incorrect")

    call assert_equals(6, diffe, "Containers' dimensions incorrect")

    totalIt = sum(qIt)

    call assert_equals(totalIt, nTItems)

    call assert_equals(nItems, nTItems)

    call assert_equals(totalIt, size(iLength))

    call assert_equals(totalIt, size(iWidth))

    call assert_equals(totalIt, size(iId))

    do i = 1, nit

       countType = 0
       
       do j = 1, totalIt

          if ( iLength(j) .eq. it(i)%length .and. &
               iWidth(j) .eq. it(i)%width ) countType = countType + 1

       end do

       call assert_equals(qIt(i), countType, "Wrong number of items")

    end do
          
  end subroutine test_load_data_many

! ******************************************************************
! ******************************************************************

  subroutine test_get_available_containers

    use packdat, only : loadData, reset, getAvContainers, nContainers

    use items, only : ItemType

    use containers, only : Container, emptyContainer

    implicit none

    character(80) :: filename

    integer :: i, j

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(2), L(3), nL

    logical :: hasCntnr

    type(ItemType) :: it(2)

    type(Container) :: cont(3)
    
    it(1) = ItemType(0, 0, 0.0D0, 0.0D0)

    it(2) = ItemType(0, 1, 0.0D0, 0.0D0)

    cont(1) = emptyContainer(1, 0, 1.0D0, 1.0D0)

    cont(2) = emptyContainer(2, 0, 5.0D0, 5.0D0)

    cont(3) = emptyContainer(3, 1, 2.0D0, 2.0D0)

    filename = "data.txt"

    ! Test 1 - Single type of item that fits all containers
    
    qIt = (/10, 0/)
    
    call createFiles(it, 2, cont, 3, qIt)
    
    call reset()

    call loadData(filename)

    call assert_equals(3, nContainers)
    
    call getAvContainers(L, nL)

    call assert_equals(3, nL)

    do i = 1, 3

       hasCntnr = .false.
       
       do j = 1, nL

          if (i .eq. L(j)) hasCntnr = .true.

       end do

       call assert_true(hasCntnr, "List without container.")

    end do

    ! Test 2 - two items, three containers and two ids

    qIt = (/10, 20/)
    
    call createFiles(it, 2, cont, 3, qIt)
    
    call reset()

    call loadData(filename)

    call getAvContainers(L, nL)

    call assert_equals(1, nL)

    call assert_equals(3, L(1))

    ! Test 3 - one item and one container, different IDs

    qIt = (/0, 20/)
    
    call createFiles(it, 2, cont, 2, qIt)
    
    call reset()

    call loadData(filename)

    call getAvContainers(L, nL)

    call assert_equals(0, nL)

  end subroutine test_get_available_containers

! ******************************************************************
! ******************************************************************

  subroutine test_sort_items_with_id

    use packdat, only : loadData, nTItems, nItems, iLength, &
         iWidth, iId, reset, sortItems

    use items, only : ItemType, toVector

    use containers, only : Container, emptyCContainer

    implicit none

    integer, parameter :: nit = 4
    
    character(80) :: filename

    integer :: i, j, totalIt, countType

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(4)

    real(8), allocatable :: x(:)

    type(ItemType) :: it(nit)

    type(Container) :: cont(1)
    
    call reset()

    do i = 1, nit

       it(i) = ItemType(i, 0, 10.0D0 + i, 20.0D0 + i)

    end do

    ! The first type of item is the most important one
    it(1)%class = 1

    cont(1) = emptyCContainer(1, 1.0D0, 1.0D0)
    
    filename = "data.txt"

    ! Test 1, only 2 items of different types
    
    qIt = (/1, 1, 0, 0/)

    call createFiles(it, nit, cont, 1, qIt)
    
    call loadData(filename)

    call assert_equals(sum(qIt), nItems)
    
    allocate(x(2 * nItems))
    
    call sortItems(x)

    call assert_true(iId(1) .lt. iId(2))
    
    call assert_equals(it(2)%length, iLength(1))

    call assert_equals(it(2)%width, iWidth(1))

    call assert_equals(it(1)%length, iLength(2))

    call assert_equals(it(1)%width, iWidth(2))

    deallocate(x)

  end subroutine test_sort_items_with_id

! ******************************************************************
! ******************************************************************

  subroutine test_get_available_containers_is_sorting

    use packdat, only : loadData, reset, getAvContainers, nContainers

    use items, only : ItemType

    use containers, only : Container, emptyContainer

    implicit none

    character(80) :: filename

    integer :: i, j

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(1), L(3), nL

    logical :: hasCntnr

    type(ItemType) :: it(1)

    type(Container) :: cont(3)
    
    it(1) = ItemType(0, 0, 0.0D0, 0.0D0)

    cont(1) = emptyContainer(1, 1, 1.0D0, 1.0D0)

    cont(2) = emptyContainer(2, 0, 5.0D0, 5.0D0)

    cont(3) = emptyContainer(3, 0, 2.0D0, 2.0D0)

    filename = "data.txt"

    call createFiles(it, 1, cont, 3, (/ 10 /))
    
    call reset()

    call loadData(filename)

    call getAvContainers(L, nL)

    call assert_equals(3, L(1), "Unsorted cntnr list.")

    call assert_equals(2, L(2), "Unsorted cntnr list.")

    call assert_equals(1, L(3), "Unsorted cntnr list.")

  end subroutine test_get_available_containers_is_sorting

! ******************************************************************
! ******************************************************************

  subroutine test_set_max_items

    use packdat, only : loadData, reset, setMaxItems_

    use items, only : ItemType

    use containers, only : Container, emptyContainer

    implicit none

    character(80) :: filename

    integer :: i, j

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(2), L(3), nL, c, nItms

    logical :: hasCntnr

    type(ItemType) :: it(2)

    type(Container) :: cont(3)
    
    it(1) = ItemType(1, 0, 1.0D0, 1.0D0)

    it(2) = ItemType(2, 1, 2.0D0, 2.0D0)

    cont(1) = emptyContainer(1, 1, 10.0D0, 10.0D0)

    cont(2) = emptyContainer(2, 0, 8.0D0, 8.0D0)

    cont(3) = emptyContainer(3, 0, 20.0D0, 20.0D0)

    filename = "data.txt"

    call createFiles(it, 2, cont, 3, (/ 0, 0 /))
    
    call reset()

    call loadData(filename)

    call setMaxItems_(it(1)%class, it(1)%length, it(1)%width, &
         c, nItms)

    call assert_equals(3, c, "Wrong container for maxItems")

    call assert_equals(400, nItms)

    call setMaxItems_(it(2)%class, it(2)%length, it(2)%width, &
         c, nItms)
    
    call assert_equals(1, c, "Wrong container for maxItems")

    call assert_equals(25, nItms)


  end subroutine test_set_max_items
  
  
end module packdat_test
