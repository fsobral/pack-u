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

    ! Generate random items
    
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

    use packdat, only : loadData, nTItems, nItems, iLength, &
         iWidth, iId, nContainers, setCurrContainer, cLength, &
         cWidth, cId, shutdown

    use items, only : ItemType

    use containers, only : Container, emptyContainer

    implicit none

    integer, parameter :: nit = 4, ncont = 3
    
    character(80) :: filename

    integer :: i, j, totalIt, countType

    real(8) :: prevCArea, currCArea
    
    integer :: qIt(4)

    type(ItemType) :: it(nit)

    type(Container) :: cont(ncont)
    
    do i = 1, nit

       it(i) = ItemType(i, 17, 10.0D0 + i, 20.0D0 + i)

    end do

    do i = 1, ncont

       cont(i) = emptyContainer(i, 100.0D0 + i, 200.0D0 - 2 * i)

    end do

    filename = "data.txt"

    ! Test 1
    
    qIt = (/10, 0, 0, 0/)
    
    call createFiles(it, nit, cont, 1, qIt)
    
    call loadData(filename)
    

    call assert_equals(1, nContainers)

    call setCurrContainer(1)

    call assert_equals(cont(1)%length, cLength)

    call assert_equals(cont(1)%width, cWidth)
    

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

    ! Test 2

    call shutdown()
    
    qIt = (/10, 0, 20, 30/)
    
    call createFiles(it, nit, cont, ncont, qIt)
    
    call loadData(filename)


    call assert_equals(ncont, nContainers)

    prevCArea = 0.0D0
    
    do i = 1, ncont

       call setCurrContainer(i)
       
       currCArea = cLength * cWidth
       
       call assert_true( prevCArea .le. currCArea )

       prevCArea = currCArea

    end do    

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

       call assert_equals(qIt(i), countType)

    end do
          

  end subroutine test_load_data

end module packdat_test
