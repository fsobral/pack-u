module packdat

  implicit none

  ! COMMONS SCALARS

  ! Number of current items
  integer :: nItems
  ! Current container's dimensions
  real(8) :: cLength, cWidth

  ! PRIVATE PARAMETERS

  ! Configuration file: containers' dimensions
  character(80), parameter :: c_filename = 'containers.txt'
  ! Configuration file: items' dimensions
  character(80), parameter :: i_filename = 'items.txt'
  ! Statistics information file
  character(80), parameter :: stats_filename = 'stats.csv'

  ! PRIVATE SCALARS

  ! Number of containers
  integer :: nContainers
  ! Current container
  integer :: currContainer
  ! Total number of items
  integer :: nTItems
  ! Total number of types of items
  integer :: nTypes
  ! Total number of boxes used
  integer :: nTContainers
  ! Active items
  integer :: iini, iend

  ! PRIVATE ARRAYS

  ! Falta colocar ponteiros para o tipo e o numero!

  ! Container's type
  integer, allocatable :: cType(:)
  ! Container's dimensions
  real(8), target, allocatable :: cLength_(:)
  real(8), target, allocatable :: cWidth_(:)
  ! Item's type
  integer, pointer :: iType(:)
  integer, target, allocatable :: iType_(:)
  ! Item's number
  integer, pointer :: iNumber(:)
  integer, target, allocatable :: iNumber_(:)
  ! Item's dimensions
  real(8), target, allocatable :: iLength_(:)
  real(8), target, allocatable :: iWidth_(:)

  ! Maximum number of items in the largest Container
  integer, allocatable :: maxItems(:)
  ! Type of container for each type of item
  integer, allocatable :: contType(:)

  ! Types of containers used
  integer, pointer :: cTypeUsed_(:)
  ! Index to the items in the container
  integer, pointer :: cStartEnd_(:)

  ! COMMON ARRAYS

  ! Pointer to current item's dimensions
  real(8), pointer :: iLength(:), iWidth(:)

  !Id for containers and items
  integer, allocatable :: cId(:), iId(:)

  private

  public :: iLength, iWidth, cLength, cWidth, nItems, nTItems,    &
            nContainers, evalover, evaldover, setCurrContainer,   &
            initialpoint, loadData, drawSol, extractBoxes,        &
            removeAppendedBox, appendBox, removeBoxes, sortItems, &
            reduction, printStats, remainingItems, resetPacking,  &
            saveSol,cId,iId

contains

! ******************************************************************
! ******************************************************************

  subroutine resetPacking()

    ! This subroutine cancels the current packing and allows a whole
    ! new packing to start using the available items.

    call removeItems(0)

  end subroutine resetPacking

! ******************************************************************
! ******************************************************************

  function remainingItems()

    ! This function return the number of remaining items to be packed.

    implicit none

    ! RETURN
    integer :: remainingItems

    remainingItems = nTItems - iend

  end function remainingItems

! ******************************************************************
! ******************************************************************

  subroutine printStats(elapsedTime)

    ! This subroutine prints final statistics on the screen and also
    ! in an output file.

    implicit none

    ! SCALAR ARGUMENTS
    real(kind=8), intent(in) :: elapsedTime

    ! LOCAL SCALARS
    integer :: i, c
    real(8) :: tiArea, tbArea

    tiArea = 0.0D0
    
    do i = 1, nTItems

       tiArea = tiArea + iWidth_(i) * iLength_(i)

    end do

    c = 0.0D0
    
    do i = 1, nTContainers

       c = cTypeUsed_(i)

       tbArea = tbArea + cWidth_(c) * cLength_(c)

    end do

    write(*,FMT=0080) nTContainers, nTItems, tbArea,  &
         tiArea, (tbArea / tiArea - 1.0D0) * 100.0D0, &
         elapsedTime * 1.0D+02

    ! Save STATS in file

    open(99, FILE=stats_filename)
    
    write(99, FMT=0090) nTContainers, nTItems, tbArea, &
         tiArea, (tbArea / tiArea - 1.0D0) * 100.0D0,  &
         elapsedTime * 1.0D+02

    close(99)

0080 FORMAT(/, /, 'FINAL INFORMATION', /, 17('-'), /, /, &
          'Number of containers:', 35X, I4,/             &
          'Number of items:', 37X, I7,/                  &
          'Total area:', 39X, F10.2, /                   &
          'Total item area:', 34X, F10.2, /,             &
          'Waste ratio (%):', 34X, F10.2, /,             &
          'Elapsed time (in ms):', 19X, F20.6)
0090 FORMAT(I7, ',', I7, ',', F10.2, ',', F10.2, ',', &
          F20.4, ',', F20.6)

  end subroutine printStats

! ******************************************************************
! ******************************************************************

  function evalover(x, i, j)

    ! Evaluates the overlapping between item i and j

    implicit none

    ! SCALAR ARGUMENTS
    integer :: i, j

    ! ARRAY ARGUMENTS
    real(8) :: x(:)

    ! OUTPUT
    real(8) :: evalover

    intent(in) :: i, j, x

    ! LOCAL SCALARS
    integer :: pi, pj

    pi = 2 * (i - 1) + 1

    pj = 2 * (j - 1) + 1

    evalover = max(0.0D0, x(pj    ) + iLength(j) - x(pi    )) * &
               max(0.0D0, x(pi    ) + iLength(i) - x(pj    )) * &
               max(0.0D0, x(pj + 1) + iWidth(j)  - x(pi + 1)) * &
               max(0.0D0, x(pi + 1) + iWidth(i)  - x(pj + 1))

    evalover = evalover ** 2.0D0
    
    return

  end function evalover

! ******************************************************************
! ******************************************************************

  subroutine evaldover(x, i, j, g)

    ! Evaluates the derivatives of the overlapping function between
    ! item i and j

    implicit none

    ! SCALAR ARGUMENTS
    integer :: i, j

    ! ARRAY ARGUMENTS
    real(8) :: g(:), x(:)

    intent(in ) :: i, j, x
    intent(out) :: g

    ! LOCAL SCALARS
    integer :: pi, pj
    real(8) :: t, t1, t2, t3, t4

    pi = 2 * (i - 1) + 1

    pj = 2 * (j - 1) + 1

    t1 = max(0.0D0, x(pj    ) + iLength(j) - x(pi    ))

    t2 = max(0.0D0, x(pi    ) + iLength(i) - x(pj    ))

    t3 = max(0.0D0, x(pj + 1) + iWidth(j)  - x(pi + 1))

    t4 = max(0.0D0, x(pi + 1) + iWidth(i)  - x(pj + 1))


    t = - 2.0D0 * t1 * (t2 * t3 * t4) ** 2.0D0 &
        + 2.0D0 * t2 * (t1 * t3 * t4) ** 2.0D0 

    g(pi    ) = g(pi    ) + t

    g(pj    ) = g(pj    ) - t
                

    t = - 2.0D0 * t3 * (t1 * t2 * t4) ** 2.0D0 &
        + 2.0D0 * t4 * (t1 * t2 * t3) ** 2.0D0

    g(pi + 1) = g(pi + 1) + t

    g(pj + 1) = g(pj + 1) - t
    
  end subroutine evaldover
  
! ******************************************************************
! ******************************************************************

  subroutine setCurrContainer(c)

    ! This subroutine sets the current container and exports its
    ! dimensions

    ! SCALAR ARGUMENTS
    integer, intent(in) :: c

    if ( c .le. 0 .or. c .gt. nContainers ) then

       write(*,*) 'Wrong container number'

       stop

    end if

    currContainer = c

    cWidth = cWidth_(c)

    cLength = cLength_(c)

  end subroutine setCurrContainer

! ******************************************************************
! ******************************************************************

  subroutine reduction(numberOfSolutions, csvoutput)

    ! This subroutine applies the reduction heuristic. It uses the
    ! values given by vector 'maxItems' to quickly eliminates items of
    ! the same type.

    ! TODO: It is a good idea to identify when different items have
    ! the same sizes.

    implicit none

    ! SCALAR ARGUMENTS
    character(80), intent(in) :: csvoutput
    integer, intent(out) :: numberOfSolutions

    ! LOCAL SCALARS
    integer :: c, qot, curr_item, i, tmpIt
    character(80) :: filename

    ! LOCAL ARRAYS
    real(8) :: x(2 * nItems)

    numberOfSolutions = 1

    do curr_item = 1,4

       ! Count the number of items

       c = 0

       do i = 1, nItems

          if ( iType(i) .eq. curr_item ) c = c + 1

       end do

       ! Calculate the number of containers
       
       qot = INT(c / maxitems(curr_item))
       
       ! Com 'gordura'
       ! qot = qot - 1 ! It can be negative

       c = 0

       do i = 1, nItems

          if ( iType(i) .eq. curr_item ) then

             if ( c .gt. qot * maxitems(curr_item) ) exit

             c = c + 1

             call swap(x, iWidth, iLength, iType, iNumber, i, c)

          end if

       end do

       ! Draw pattern

       write(*,*) 'Eliminating', max(0, qot) * maxitems(curr_item), 'items of ', &
            'type', curr_item, 'in', max(0, qot), 'containers of type',          &
            conttype(curr_item), '.'

       do i = 1, qot

          write(filename, FMT="('sol',I0.3,'.asy')") numberOfSolutions

          call drawPattern(filename, maxitems(curr_item), iWidth(1), &
                           iLength(1), conttype(curr_item))

          cTypeUsed_(numberOfSolutions) = conttype(curr_item)

          ! TODO: improve saveSol

          tmpIt = nItems
          
          nItems = maxitems(curr_item)
       
          call saveSol(csvoutput)

          nItems = tmpIt

          numberOfSolutions = numberOfSolutions + 1

       end do

       ! Update the number of containers

       nTContainers = numberOfSolutions - 1

       ! Remove items

       if ( qot .gt. 0 ) call removeItems(qot * maxitems(curr_item))

    end do

  end subroutine reduction

! ******************************************************************
! ******************************************************************

  subroutine drawPattern(filename, nit, itwid, itlen, container)

    ! This subroutine draws a pre-optimized pattern for the given item
    ! type. This pattern was found by the heuristic procedure of
    ! subroutine 'reduction'. It is an easy way to set up the current
    ! solution data and call subroutine 'drawSol'.
    !
    ! filename: the output filename
    !
    ! nit: number of items of the current type
    !
    ! itwid: item's width
    !
    ! itlen: item's length
    !
    ! container: type of the container onto which the items will be
    !            packed

    implicit none

    ! SCALAR ARGUMENTS
    integer :: nit, container
    real(8) :: itwid, itlen
    character(80) :: filename

    intent(in) :: nit, container, itwid, itlen, filename

    ! LOCAL ARRAYS
    real(8) :: x(2 * nit)

    ! LOCAL SCALARS
    integer :: i, prevNItems
    real(8) :: dx, dy

    call setCurrContainer(container)

    dx = 0.0D0

    dy = 0.0D0

    do i = 1, nit

       if ( dy + itwid .gt. cWidth ) then 

          dx = dx + itlen

          dy = 0.0D0

       end if

       x(2 * (i - 1) + 1) = dx
       
       x(2 * i) = dy

       dy = dy + itwid

    end do

    prevNItems = nItems

    nItems = nit
    
    call drawSol(x, filename, .false.)

    nItems = prevNItems

  end subroutine drawPattern
  
! ******************************************************************
! ******************************************************************

  subroutine sortItems(x)

    ! This subroutine sort items in ascending form according to their
    ! area.

    implicit none

    ! ARRAY ARGUMENTS
    real(8), intent(inout) :: x(:)

    ! LOCAL SCALARS
    integer :: i, j, min_j
    real(8) :: curr_area, min_area    
    
    do i = 1, nItems - 1

       min_area = iWidth(i) * iLength(i)

       min_j    = i

       do j = i + 1, nItems

          curr_area = iWidth(j) * iLength(j)

          if ( curr_area .le. min_area ) then

             min_area = curr_area

             min_j    = j

          end if

       end do

       if ( min_j .ne. i ) then
          
          call swap(x, iWidth, iLength, iType, iNumber, i, min_j)

       end if

    end do

  end subroutine sortItems

! ******************************************************************
! ******************************************************************

  subroutine initialpoint(x, s, strip)

    ! This subroutine sets up an random initial position of the items,
    ! in order to solve a non-linear programming problem.
    !
    ! x: On output, it contains the position of the items to be
    !    packed
    !
    ! s: the seed to be used to call the random intrinsic functions
    !
    ! strip: if '.true.', the problem of minimizing a strip is
    !        considered, which means that the last position of vector
    !        'x' is the length of the strip and also has to be
    !        randomly initialized.

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in ) :: s
    logical, intent(in ) :: strip

    ! ARRAY ARGUMENTS
    real(8), intent(out) :: x(:)

    ! LOCAL SCALARS
    integer :: i, j, nseed, itmp, ri
    real(8) :: rnumber, cl, maxd, dx, dy

    ! LOCAL ARRAYS
    integer, allocatable :: seed(:), ritems(:)

    ! Initialize the random structure

    call random_seed(SIZE=nseed)

    allocate(seed(nseed), ritems(nItems))

    do i = 1,nseed
       seed(i) = s
    end do

    call random_seed(PUT=seed)

    deallocate(seed)

    ! Select the items with the largest area first.  If a random
    ! initial point is wanted, then comment this procedure and
    ! uncomment the random procedure below.

    do i = 1, nItems

       rItems(i) = nItems - (i - 1)

    end do

!!$    ! Randomly select the order of the items
!!$
!!$    do i = 1, nItems
!!$
!!$       call random_number(rnumber)
!!$
!!$       ri = i + INT((nItems - i) * rnumber)
!!$
!!$       itmp = rItems(i)
!!$
!!$       rItems(i) = rItems(ri)
!!$
!!$       rItems(ri) = itmp
!!$
!!$    end do

    ! Allocate items in random order with low overlapping

    dy = 0.0D0

    dx = 0.0D0

    maxd = 0.0D0

    do i = 1, nItems

       j = rItems(i)

       if ( dy + iWidth(j) .gt. cWidth ) then 

          call random_number(rnumber)

          dx = dx + maxd

          dy = 0.0D0

          maxd = 0.0D0

       end if

       x(2 * (j - 1) + 1) = dx
       
       x(2 * j) = dy

       call random_number(rnumber)

       dy = dy + iWidth(j)

       maxd = max(maxd, iLength(j))

    end do

    if ( strip ) x(2 * nItems + 1) = dx + maxd

    deallocate(ritems)

  end subroutine initialpoint

! ******************************************************************
! ******************************************************************

  subroutine setMaxItems(itemType, itemL, itemW)

    ! This subroutine calculates the maximum number of items of type
    ! 'itemType' that can be packed onto the largest container. It
    ! automatically initializes internal vectors 'maxItems' and
    ! 'contType'.

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: itemType
    real(kind=8), intent(in) :: itemL, itemW

    ! LOCAL SCALARS
    integer :: maxW, maxL

    maxW = INT(cWidth_(1) / itemW)

    maxL = INT(cLength_(1) / itemL)
    
    maxItems(itemType) = maxW * maxL

    contType(itemType) = 1

  end subroutine setMaxItems

! ******************************************************************
! ******************************************************************

  subroutine loadData(filename)

    ! This subroutine loads the problem's data and initializes all the
    ! structure of the packing problem.
    !
    ! filename: the name of the data file, which contains the number
    !           of each type of item to be packed

    implicit none

    ! LOCAL SCALARS
    integer :: i, t, iprev
    character(80) :: filename

    ! LOCAL ARRAYS
    integer, allocatable :: types(:)
    real(8), allocatable :: tW(:), tL(:)

    ! Container data

    open(99, FILE=c_filename)

    read(99, *) nContainers

    allocate(clength_(nContainers), cWidth_(nContainers),cId(nContainers))

    do i = 1, nContainers

       read(99, *) cLength_(i), cWidth_(i), cId(i)

    end do

    close(99)

    currContainer = 1
    
    ! Item data

    open(99, FILE=i_filename)

    read(99, *) nTypes

    ! TODO: test allocation error
    allocate(types(nTypes), tL(nTypes), tW(nTypes), maxItems(nTypes), &
             contType(nTypes), iId(nTypes))

    ! Load items sizes and initializes the maximum number of items in
    ! the largest container
    ! We inserted the iId input here. Note that the weight option 
    ! was excluded

    do t = 1, nTypes

       read(99, *) tL(t), tW(t), iId(t)

       call setMaxItems(t, tL(t), tW(t))

    end do

    close(99)

    ! Read the number of items

    open(99, FILE=filename)

    nTItems = 0

    do t = 1, nTypes

       read(99, *) types(t)

       write(*,*) types(t), 'elements of type', t

       nTItems = nTItems + types(t)

    end do

    close(99)

    ! Initialize the number of used containers

    nTContainers = 0

    ! Create all items

    ! TODO: test allocation error
    allocate(iType_(nTItems), iLength_(nTItems), iWidth_(nTItems), &
             iNumber_(nTItems), cTypeUsed_(nTItems), cStartEnd_(nTItems))

    iprev = 1

    do t = 1, nTypes

       do i = iprev, iprev + types(t) - 1

          iType_(i) = t

          iNumber_(i) = i

          iLength_(i) = tL(t)

          iWidth_(i) = tW(t)

       end do

       iprev = iprev + types(t)

    end do

    call updateCurrItems(1, nTItems)

! For testing if the input was read we create a file called testing.txt 
! TODO : test setup error

open(10,FILE='testing.txt')

write(10,*) 'Container id'

    do i =1, nContainers

      write(10,*) cId(i)
    
    end do
    
    write(10,*) 'Item Id'
      
    do i=1, nTypes

        write(10,*) iId(i)

    end do

    close(10)
!!$    iini = 1
!!$
!!$    iend = nTItems
!!$
!!$    nItems = nTItems
!!$
!!$    ! Initialize common pointers
!!$
!!$    iType   => iType_(iini:iend)
!!$
!!$    iNumber => iNumber_(iini:iend)
!!$
!!$    iLength => iLength_(iini:iend)
!!$
!!$    iWidth  =>  iWidth_(iini:iend)

    deallocate(types, tL, tW)

  end subroutine loadData

! ******************************************************************
! ******************************************************************

  subroutine removeAppendedBox()

    ! This subroutine removes the last item (which was most recently
    ! appended)

    implicit none

    write(*,*) 'Removing item', iNumber(nItems)

    call updateCurrItems(iini, iend - 1)

!!$    nItems = nItems - 1
!!$
!!$    iend = iend - 1
!!$
!!$    iType   => iType_(iini:iend)
!!$
!!$    iNumber => iNumber_(iini:iend)
!!$
!!$    iWidth  => iWidth_(iini:iend)
!!$
!!$    iLength => iLength_(iini:iend)

  end subroutine removeAppendedBox

! ******************************************************************
! ******************************************************************

  subroutine appendBox(x, rArea, lItArea, hasAppended)

    ! This subroutine appends a new item

    implicit none

    ! SCALAR ARGUMENTS
    real(8) :: rArea, lItArea
    logical :: hasAppended

    ! ARRAY ARGUMENTS
    real(8) :: x(:)

    intent(in   ) :: rArea, lItArea
    intent(out  ) :: hasAppended
    intent(inout) :: x

    ! LOCAL SCALARS
    integer :: i, item
    real(8) :: area, cArea

    item = - 1

    area = - 1.0D+20

    hasAppended = .false.

    do i = iend + 1, nTItems

       cArea = iLength_(i) * iWidth_(i)

       if ( cArea .lt. min(lItArea, rArea) .and. cArea .gt. area ) then

          area = cArea

          item = i

          hasAppended = .true.

       end if

    end do

    if ( hasAppended ) then

       write(*,*) 'Appending item', iNumber_(item)

       call swap(x, iWidth_, iLength_, iType_, iNumber_, item, iend + 1)

       call updateCurrItems(iini, iend + 1)

!!$       nItems = nItems + 1
!!$
!!$       iend = iend + 1
!!$       
!!$       iType   => iType_(iini:iend)
!!$       
!!$       iNumber => iNumber_(iini:iend)
!!$       
!!$       iWidth  => iWidth_(iini:iend)
!!$       
!!$       iLength => iLength_(iini:iend)

       !write(*,*), iNumber_(item), iNumber(nItems)

    end if

  end subroutine appendBox

! ******************************************************************
! ******************************************************************

  subroutine dswap(v, i, j)

    ! Performs a swap of positions 'i' and 'j' in a real vector

    implicit none

    ! SCALAR ARGUMENTS
    integer :: i, j

    ! ARRAY ARGUMENTS
    real(8) :: v(:)

    ! LOCAL SCALARS
    real(8) :: tmp

    tmp = v(i)

    v(i) = v(j)

    v(j) = tmp

  end subroutine dswap

! ******************************************************************
! ******************************************************************

  subroutine iswap(v, i, j)

    ! Performs a swap of positions 'i' and 'j' in a real vector

    implicit none

    ! SCALAR ARGUMENTS
    integer :: i, j

    ! ARRAY ARGUMENTS
    integer :: v(:)

    ! LOCAL SCALARS
    integer :: tmp

    tmp = v(i)

    v(i) = v(j)

    v(j) = tmp

  end subroutine iswap

! ******************************************************************
! ******************************************************************

  subroutine swap(x, vw, vl, vt, vn, it1, it2)

    ! Swaps positions related to items it1 and it2 in real vectors vw,
    ! vl and integer vectors vt, vn

    implicit none

    ! SCALAR ARGUMENTS
    integer :: it1, it2

    ! ARRAY ARGUMENTS
    real(8) :: vw(:), vl(:), x(:)
    integer :: vt(:), vn(:)

    intent(in   ) :: it1, it2
    intent(inout) :: vw, vl, vt, vn, x

    ! LOCAL SCALARS
    integer :: itmp
    real(8) :: dtmp

    call dswap(x, 2 * (it1 - 1) + 1, 2 * (it2 - 1) + 1)

    call dswap(x, 2 * it1, 2 * it2)

    ! Common data swap

    call dswap(vl, it1, it2)

    call dswap(vw, it1, it2)

    call iswap(vt, it1, it2)

    call iswap(vn, it1, it2)

  end subroutine swap

! ******************************************************************
! ******************************************************************

  subroutine extractBoxes(x)

    ! This subroutine select a subset of items to be fit inside the
    ! current container. This subroutine should be called only in the
    ! 'strip' problem, but can be used for general purposes.

    implicit none

    ! ARRAY ARGUMENTS
    real(8), intent(inout) :: x(:)

    ! LOCAL SCALARS
    integer :: i, j, pos
    real(8) :: tmp

    pos = 1

    ! Select items

    write(*,*) 'Selected items:'

    do i = 1, nItems

       if ( x(2 * (i - 1) + 1) + iLength(i) .le. cLength ) then

          write(*,*) 'Item', iNumber(i)

          call swap(x, iLength, iWidth, iType, iNumber, pos, i)

          pos = pos + 1

       end if 

    end do

    ! Remove overlapped items, if any

    pos = pos - 1

    nItems = pos

    do i = nItems, 1, -1

       do j = i - 1, 1, -1

          if ( evalover(x, i, j) .gt. min(iWidth(i), iWidth(j)) ** 2.0D0 ) then

             write(*, *) 'Detected overlapping between', iNumber(i), 'and', iNumber(j)

             call swap(x, iLength, iWidth, iType, iNumber, i, pos)

             pos = pos - 1

             exit

          end if

       end do

    end do

    ! Update the number of items and selected items

    call updateCurrItems(iini, iini + pos - 1)

!!$    nItems = pos
!!$
!!$    iend = iini + nItems - 1
!!$
!!$    iType   => iType_(iini:iend)
!!$
!!$    iNumber => iNumber_(iini:iend)
!!$
!!$    iWidth  => iWidth_(iini:iend)
!!$
!!$    iLength => iLength_(iini:iend)

  end subroutine extractBoxes

! ******************************************************************
! ******************************************************************

  subroutine removeItems(nit)

    ! This subroutine removes the first 'nit' items. In practice, this
    ! means that it shifts 'nit' positions in all the inner vectors
    ! and updates the inner limits.

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: nit

    if ( nit .gt. nItems ) write(*,*) 'WARNING: Number of removed', &
         'items greater than total number of items!'

    if ( nit .gt. 0 ) then

       nTContainers = nTContainers + 1

       cTypeUsed_(nTContainers) = currContainer

    end if

!!$    iini = iini + nit
!!$
!!$    iend = nTItems
!!$
!!$    nItems = iend - iini + 1
!!$
!!$    iType   => iType_(iini:iend)
!!$
!!$    iNumber => iNumber_(iini:iend)
!!$
!!$    iWidth  => iWidth_(iini:iend)
!!$
!!$    iLength => iLength_(iini:iend)

    call updateCurrItems(iini + nit, nTItems)

  end subroutine removeItems
  
! ******************************************************************
! ******************************************************************

  subroutine updateCurrItems(niini, niend)

    ! This subroutine updates which items are currently being
    ! considered for optimization and packing.

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: niini, niend

    iini = niini

    iend = niend

    nItems = iend - iini + 1

    iType   => iType_(iini:iend)

    iNumber => iNumber_(iini:iend)

    iWidth  => iWidth_(iini:iend)

    iLength => iLength_(iini:iend)

  end subroutine updateCurrItems

! ******************************************************************
! ******************************************************************

  subroutine removeBoxes()

    ! This subroutine remove the items packed in the current
    ! container.

    implicit none

    ! Removes the first nItems items

    call removeItems(nItems)

  end subroutine removeBoxes

! ******************************************************************
! ******************************************************************

  subroutine saveSol(output)

    ! This subroutine saves the solution in CSV format. The format of
    ! the solutions is
    !
    ! CONTAINER_TYPE, IT_TYPE1, NUMB_IT1, IT_TYPE2, NUMB_IT2, ...


    ! ARRAY ARGUMENTS
    character(80) :: output

    ! LOCAL SCALARS
    integer :: i

    ! LOCAL ARRAYS
    integer :: counttItems(nTypes)
    
    do i = 1, nTypes

       counttItems(i) = 0

    end do

    do i = 1, nItems

       counttItems(iType(i)) = counttItems(iType(i)) + 1

    end do

    open(99, file=output, access='APPEND')

    write(99, FMT=9001, advance='no') currContainer

    do i = 1, nTypes

       write(99, FMT=9002, advance='no') counttItems(i)

    end do

    close(99)
    
9001 FORMAT(I3,',')
9002 FORMAT(I5,',')

  end subroutine saveSol

! ******************************************************************
! ******************************************************************

  subroutine drawSol(x, output, strip)

    ! This subroutine creates a .ASY file in order to draw the
    ! solution. If 'strip = .true.' then the strip problem is being
    ! considered and the last coordinate of 'x' is the strip's length.

    implicit none

    ! SCALAR ARGUMENTS
    logical :: strip

    ! ARRAY ARGUMENTS
    character(80) :: output
    real(8)       :: x(:)

    intent(in) :: x, output

    ! LOCAL SCALARS
    integer :: i
    real(8) :: scale

    ! Try to find a reasonable scale for drawing

    scale = max(1.0D-4, min(1.0D0, 30.0D0 / maxval(x(1:nItems))))

    ! Draws the solution in ASY format

    open(99, file=output)

    write(99, FMT=0001) scale

    ! Draw items

    do i = 1, nItems

       write(99, FMT=0003) x(2 * (i - 1) + 1), x(2 * i), &
            iLength(i), iWidth(i), iType(i), iNumber(i)

    end do

    ! Draw containers

    write(99, FMT=0002) cLength, cWidth

    if ( strip ) write(99, FMT=0004) x(2 * nItems + 1), cWidth

    close(99)

0001 FORMAT('import packlib;',/,/,'settings.outformat = "pdf";',/, &
          'unitsize(',F10.5,'cm);')
0002 FORMAT('draw(box((0,0), (',F10.5,',',F10.5,')), currentpen + dashed + 2);')
0004 FORMAT('draw(box((0,0), (',F10.5,',',F10.5,')), currentpen + 2);')
0003 FORMAT('drawBox((',F10.5,',',F10.5,'), ',F10.5,', ',F10.5, &
            ', cor(',I2,'), currentpen, "', I4, '");')

  end subroutine drawSol

end module packdat
