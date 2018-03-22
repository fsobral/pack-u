module packdat

  implicit none

  ! COMMONS SCALARS

  ! Number of current items
  integer :: nItems
  ! Current container's dimensions
  real(8) :: cLength, cWidth
  ! Current container's id
  integer :: cId

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

  ! Output information
  logical :: VERBOSE = .true.

  ! PRIVATE ARRAYS

  ! Falta colocar ponteiros para o tipo e o numero!

  ! Container's type
  integer, allocatable :: cType(:)
  !Container's id
  integer, allocatable :: cId_(:)
  ! Container's dimensions
  real(8), target, allocatable :: cLength_(:)
  real(8), target, allocatable :: cWidth_(:)
  ! Item's type
  integer, pointer :: iType(:) => NULL()
  integer, target, allocatable :: iType_(:)
  ! Item's number
  integer, pointer :: iNumber(:) => NULL()
  integer, target, allocatable :: iNumber_(:)
  ! Item's dimensions
  real(8), target, allocatable :: iLength_(:)
  real(8), target, allocatable :: iWidth_(:)
  !Item's Id
  integer, target, allocatable :: iId_(:)
  ! Maximum number of items in the largest Container
  integer, allocatable :: maxItems(:)
  ! Type of container for each type of item
  integer, allocatable :: contType(:)

  ! Types of containers used
  integer, allocatable :: cTypeUsed_(:)
  ! Index to the items in the container
  integer, allocatable :: cStartEnd_(:)

  ! COMMON ARRAYS

  ! Pointer to current item's dimensions
  real(8), pointer :: iLength(:) => NULL(), iWidth(:) => NULL()

  ! Pointer Id for containers and items
  ! TODO: check if iId must be public
  integer, pointer :: iId(:) => NULL()
  
  private

  public :: iLength, iWidth, cLength, cWidth, nItems, nTItems,    &
            nContainers, evalover, evaldover, setCurrContainer,   &
            initialpoint, loadData, drawSol, extractBoxes,        &
            removeAppendedBox, appendBox, removeBoxes, sortItems, &
            reduction, printStats, remainingItems, resetPacking,  &
            saveSol, reset, cId, iId, sortContainers, &
            getAvContainers, setMaxItems_, &
            setVerbose

contains

! ******************************************************************
! ******************************************************************

  subroutine setVerbose(verb)

    ! Sets the output level.
    
    implicit none

    ! SCALAR ARGUMENTS
    logical, intent(in) :: verb

    VERBOSE = verb
    
  end subroutine setVerbose
  
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

    if ( VERBOSE ) &
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

       if ( VERBOSE ) then
          
          write(*,FMT=1000) 'SETCURRCONTAINER'
          
1000      format('ERROR in subr. ',A10,': Wrong container number.')

       end if

       stop

    end if

    currContainer = c

    cWidth = cWidth_(c)

    cLength = cLength_(c)

    cId = cId_(c)

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

    do curr_item = 1, nTypes

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

             call swap(x, iWidth, iLength, iType, iNumber,iId, i, c)

          end if

       end do

       ! Draw pattern

       if ( VERBOSE ) then
       
          write(*, FMT=1000) max(0, qot) * maxitems(curr_item), &
               curr_item, max(0, qot), conttype(curr_item)

1000      format('INFO: Eliminating ',I6,' items of type ',I3 &
                 ' in ',I6,' containers of type ',I3,'.')

       end if

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
    integer :: i, j, min_j,k
    real(8) :: curr_area, min_area    

    do i = 1, nItems - 1

       min_area = iWidth(i) * iLength(i)

       min_j    = i
       
       do j = i + 1, nItems

          curr_area = iWidth(j) * iLength(j)

          if ( (iId(j) .lt. iId(min_j)) .or. &
               ((iId(j) .eq. iId(min_j)) .and. &
                (curr_area .le. min_area)) ) then

             min_area = curr_area

             min_j    = j

          end if

       end do

       if ( min_j .ne. i ) then
          
          call swap(x, iWidth, iLength, iType, iNumber, iId, i, min_j)

       end if

    end do

  end subroutine sortItems

! ******************************************************************
! ******************************************************************

  recursive subroutine recInitialPoint(itemsOrder, pstart, pend, &
       xo, yo, width, length, x, nPlaced)

    ! This subroutine recursivelly tries to pack some items in the
    ! container given by the dimensions 'width' and 'length'. In the
    ! first call, the 'length' can be a very large number, in order to
    ! fit all items, before optimizing.
    
    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: nPlaced, pstart, pend
    real(8) :: xo, yo, width, length

    ! ARRAY ARGUMENTS
    integer :: itemsOrder(:)
    real(8) :: x(:)

    intent(in   ) :: itemsOrder, pstart, pend, xo, yo, width, length
    intent(out  ) :: x
    intent(inout) :: nPlaced
    
    ! LOCAL SCALARS
    integer :: i, j, howMany
    real(8) :: dx, dy, prevLen

    nPlaced = 0
    
    ! Return if there is no more items
    if ( pstart .gt. pend ) return

    dy = yo

    dx = xo

    i = pstart
    
    prevLen = iLength(itemsOrder(i))

    do while ( i .le. pend )

       j = itemsOrder(i)

       ! TODO: Think about the case when the width is small for the
       ! first item
       if ( dy + iWidth(j) .gt. width ) then

          prevLen = iLength(j)
             
          dx = dx + iLength(j)

          dy = yo

       end if

       ! Stop if the next item does not fit in the new column
       if ( dx + iLength(j) .gt. length ) return
       
       ! Never enters in the first iteration or in a new column
       if ( iLength(j) .ne. prevLen ) then

          ! If the size of the item has changed (decreased), it may be
          ! possible to pack items in the remaining space
          call recInitialPoint(itemsOrder, i, pend, dx, dy, &
               width, dx + prevLen, x, howMany)

          ! When returning, two cases can occurr
          ! 1. No item was placed - begin new column
          ! 2. Some items were placed - update prevLen and
          !    start new column
          ! In both cases, cycle the loop
          
          ! Update the number of placed items
          nPlaced = nPlaced + howMany
          
          i = i + howMany

          dy = yo

          dx = dx + prevLen

          if ( i .le. pend ) prevLen = iLength(itemsOrder(i))

          cycle

       end if

       ! Place the item
       
       prevLen = iLength(j)

       x(2 * (j - 1) + 1) = dx
       
       x(2 * j) = dy

       ! Prepare for the next iteration
       
       dy = dy + iWidth(j)

       i = i + 1

       nPlaced = nPlaced + 1

    end do

  end subroutine recInitialPoint
  
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
    integer :: i, nseed, itmp, ri
    real(8) :: rnumber, dx, dy

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

    call recInitialPoint(rItems, 1, nItems, dx, dy, &
         cWidth, 10.0D0 * cLength, x, itmp)

    if ( strip ) x(2 * nItems + 1) = x(2 * nItems - 1)

    deallocate(rItems)

  end subroutine initialpoint

! ******************************************************************
! ******************************************************************

  subroutine setMaxItems_(itemId, itemL, itemW, c, nItms)

    ! This subroutine calculates the maximum number of items of the
    ! given size and 'itemId' that can be packed onto the largest
    ! available container.
    !
    ! It uses 'itemId' and do not select a container of different id
    ! of the item.

    implicit none

    ! SCALAR ARGUMENTS
    integer :: itemId, c, nItms
    real(kind=8) :: itemL, itemW

    intent(in ) :: itemId, itemL, itemW
    intent(out) :: c, nItms

    ! LOCAL ARRAYS
    integer :: lC(nContainers)
    
    ! LOCAL SCALARS
    integer :: i, cntrn, maxW, maxL

    do i = 1, nContainers
       
       lC(i) = i
       
    end do

    call sortContainers(nContainers, lC)

    c = - 1

    nItms = 0
    
    do i = nContainers, 1, -1

       cntrn = lC(i)

       ! TODO: Check if it could be .le.
       if ( itemId .eq. cId_(cntrn) ) then

          maxW = AINT(cWidth_(cntrn) / itemW)

          maxL = AINT(cLength_(cntrn) / itemL)
          
          if ( maxW * maxL .gt. nItms ) then
    
             nItms = maxW * maxL

             c = cntrn

          end if

       end if

    end do

  end subroutine setMaxItems_

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
    integer :: i, t, iprev, k
    character(80) :: filename

    ! LOCAL ARRAYS
    integer, allocatable :: types(:),tId(:), tcl(:)
    real(8), allocatable :: tW(:), tL(:)

    ! Container data

    open(99, FILE=c_filename)

    read(99, *) nContainers

    allocate(clength_(nContainers), cWidth_(nContainers), cId_(nContainers), &
             tcl(nContainers))

    do i = 1, nContainers

       read(99, *) cLength_(i), cWidth_(i), cId_(i)

       tcl(i) = i

    end do

    close(99)

!!$    ! Sort containers according to our rules.
!!$    !
!!$    !I don't know if this is a good idea. It can save some
!!$    ! computation, but should be discussed.
!!$    
!!$    call sortContainers(nContainers, tcl)
!!$
!!$    do i = 1, nContainers
!!$
!!$       if (tcl(i) .eq. i) continue
!!$
!!$       call dswap(cLength_, i, tcl(i))
!!$
!!$       call dswap(cWidth_, i, tcl(i))
!!$
!!$       call iswap(cId_, i, tcl(i))
!!$
!!$       ! Since each position of 'tcl' is the position (AND identifier)
!!$       ! of a container, when the container moves, due a swap, we have
!!$       ! to change its index in 'tcl'. This should be done just here, because
!!$       ! we are changing the real container's properties.
!!$       do k = i + 1, nContainers
!!$
!!$          if (tcl(k) .eq. i) then
!!$
!!$             tcl(k) = tcl(i)
!!$
!!$             exit
!!$
!!$          end if             
!!$
!!$       end do         
!!$
!!$    end do
    
    currContainer = 1
    
    ! Item data

    open(99, FILE=i_filename)

    read(99, *) nTypes

    ! TODO: test allocation error
    allocate(types(nTypes), tL(nTypes), tW(nTypes), maxItems(nTypes), &
             contType(nTypes), tId(nTypes))

    ! Load items sizes and initializes the maximum number of items in
    ! the largest container
    ! We inserted the iId input here. Note that the weight option 
    ! was excluded

    do t = 1, nTypes

       read(99, *) tL(t), tW(t), tId(t)

       call setMaxItems_(tId(t), tL(t), tW(t), &
                         contType(t), maxItems(t))
       
       ! TODO: make loadData return an error flag.
       if ( contType(t) .eq. -1 ) then

          if ( VERBOSE ) write(*, FMT=1000) 'LOADDATA'

          stop

       end if
       
    end do

    close(99)

    ! Read the number of items

    open(99, FILE=filename)

    nTItems = 0

    do t = 1, nTypes

       read(99, *) types(t)

       if ( VERBOSE ) write(*,FMT=1001) types(t), t

       nTItems = nTItems + types(t)

    end do

    close(99)

    ! Initialize the number of used containers

    nTContainers = 0

    ! Create all items

    ! TODO: test allocation error
    allocate(iType_(nTItems), iLength_(nTItems), iWidth_(nTItems), &
             iNumber_(nTItems), cTypeUsed_(nTItems), cStartEnd_(nTItems),&
             iId_(nTItems))

    iprev = 1

    do t = 1, nTypes

       do i = iprev, iprev + types(t) - 1

          iType_(i) = t

          iNumber_(i) = i

          iLength_(i) = tL(t)

          iWidth_(i) = tW(t)

          iId_(i)=tId(t)

       end do

       iprev = iprev + types(t)

    end do

    call updateCurrItems(1, nTItems)

    ! For testing if the input was read we create a file called testing.txt 
    ! TODO : test setup error
    
    open(10,FILE='testing.txt')

    write(10,*) 'Container id'

    do i =1, nContainers

       write(10,*) cId_(i)

    end do

    write(10,*) 'Item Id in list ', ' |   type   | ', ' |   id   |'

    k=1

    do t=1, nTypes

       do i=1, types(t)

          write(10,*) 'item type ', t, iId_(k)

          k=k+1

       end do

    end do

    close(10)

    deallocate(types, tL, tW, tId, tcl)

1000 format('ERROR in subr. ',A10,': Item without container.')

1001 format('INFO: ',I6,' elements of type ',I3,'.')
    
  end subroutine loadData

! ******************************************************************
! ******************************************************************

  subroutine removeAppendedBox()

    ! This subroutine removes the last item (which was most recently
    ! appended)

    implicit none

    if ( VERBOSE ) write(*, FMT=1000) iNumber(nItems)

1000 format('INFO: Removing item ',I6,'.')
    
    call updateCurrItems(iini, iend - 1)

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

       if ( VERBOSE ) write(*, FMT=1000) iNumber_(item)

1000   format('INFO: Appending item ',I6,'.')

       call swap(x, iWidth_, iLength_, iType_, iNumber_,iId_ ,item, iend + 1)

       call updateCurrItems(iini, iend + 1)

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

    ! Performs a swap of positions 'i' and 'j' in an integer vector

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

  subroutine swap(x, vw, vl, vt, vn,vid,it1, it2)

    ! Swaps positions related to items it1 and it2 in real vectors vw,
    ! vl and integer vectors vt, vn

    implicit none

    ! SCALAR ARGUMENTS
    integer :: it1, it2

    ! ARRAY ARGUMENTS
    real(8) :: vw(:), vl(:), x(:)
    integer :: vt(:), vn(:),vid(:)

    intent(in   ) :: it1, it2
    intent(inout) :: vw, vl, vt, vn, x,vid

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

    call iswap(vid,it1, it2)

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

    if ( VERBOSE ) write(*, FMT=1000)

    do i = 1, nItems

       if ( x(2 * (i - 1) + 1) + iLength(i) .le. cLength ) then

          if ( VERBOSE ) write(*, FMT=1001) iNumber(i)

          call swap(x, iLength, iWidth, iType, iNumber, iId, pos, i)

          pos = pos + 1

       end if 

    end do

    ! Remove overlapped items, if any

    pos = pos - 1

    nItems = pos

    do i = nItems, 1, -1

       do j = i - 1, 1, -1

          if ( evalover(x, i, j) .gt. &
               min(iWidth(i), iWidth(j)) ** 2.0D0 ) then

             if ( VERBOSE ) write(*, FMT=1002) iNumber(i), iNumber(j)

             call swap(x, iLength, iWidth, iType, iNumber,iId, i, pos)

             pos = pos - 1

             exit

          end if

       end do

    end do

    ! Update the number of items and selected items

    call updateCurrItems(iini, iini + pos - 1)

1000 format('INFO: Selected items:')

1001 format(6X,'Item',I6)

1002 format('INFO: Detected overlapping between',I6,' and ',I6,'.')

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

    if ( nit .gt. nItems ) then
       
       if ( VERBOSE ) write(*, FMT=1000)

    end if

    call updateCurrItems(iini + nit, nTItems)

1000 format('WARNING: Number of removed items greater than ', &
            'total number of items!')

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

    iId =>iId_(iini:iend) 

  end subroutine updateCurrItems

! ******************************************************************
! ******************************************************************

  subroutine removeBoxes()

    ! This subroutine remove the items packed in the current
    ! container.

    implicit none

    ! Removes the first nItems items

    nTContainers = nTContainers + 1
       
    cTypeUsed_(nTContainers) = currContainer

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
  
! ******************************************************************
! ******************************************************************

  subroutine sortContainers(size, list)

    ! This subroutine returns in 'list' the number of the containers
    ! in a non-decreasing order of importance.
    
    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: size

    ! ARRAY ARGUMENTS
    integer :: list(size)

    intent(in   ) :: size
    intent(inout) :: list

    ! LOCAL SCALARS
    integer :: i, j, tmp, minI, minC, currC

    do i = 1, size

       minI = i

       minC = list(i)
    
       do j = i + 1, size
       
          currC = list(j)
       
          ! Select the smallest and least important container

          if ( (cId_(currC) .lt. cId_(minC)) .or. &
               ( (cId_(currC) .eq. cId_(minC)) .and. &
               (cWidth_(currC) * cLength_(currC)) .lt. &
               (cWidth_(minC) * cLength_(minC))) ) then

             minC = currC

             minI = j

          end if

       end do

       if ( i .ne. minI ) then

          tmp = list(i)

          list(i) = list(minI)

          list(minI) = tmp

       end if

    end do

  end subroutine sortContainers
  
! ******************************************************************
! ******************************************************************

  subroutine getAvContainers(L, n)

    ! This subroutine returns in 'L' the number of the containers that
    ! can be used to pack the current types of items. The containers
    ! in 'L' are sorted according to our sorting rules.
    !
    ! The criterium used is the item's and container's ID. An item can
    ! be packed inside a container if the container's ID is greater or
    ! equal than the item's ID.

    ! SCALAR ARGUMENTS
    integer :: n

    ! ARRAY ARGUMENTS
    integer :: L(:)

    intent(out) :: n, L

    ! LOCAL SCALARS
    integer :: i, j
    logical :: remove, sameid

    n = 0
    
    do j = 1, nContainers

       ! Try to insert container j in the list
       L(n + 1) = j

       remove = .false.

       sameid = .false.
       
       do i = 1, nItems

          ! If there is an item which cannot be packed inside
          ! container j, then remove it from the list
          if (cId_(j) .lt. iId(i)) then

             remove = .true.

             exit

          end if

          ! Only insert containers whose id was found among the
          ! available items
          if (cId_(j) .eq. iId(i)) sameid = .true.

       end do

       if ((.not. remove) .and. sameid) n = n + 1
       
    end do

    ! TODO: Maybe there is no need to sort containers by id.
    call sortContainers(n, L)

    do i = n + 1, nContainers

       L(i) = 0

    end do
    
  end subroutine getAvContainers
  
! ******************************************************************
! ******************************************************************

  subroutine reset()

    ! This subroutine deallocates all vectors and deassociates all
    ! the pointers
    !
    ! TODO: test deallocation errors.
    ! TODO: test if was allocated!

    ! deallocate(cType)

    if ( allocated(cLength_) ) deallocate(cLength_)
    
    if ( allocated(cWidth_) ) deallocate(cWidth_)
    
    if ( allocated(iType_) ) deallocate(iType_)
    
    if ( allocated(iNumber_) ) deallocate(iNumber_)
    
    if ( allocated(iLength_) ) deallocate(iLength_)
    
    if ( allocated(iWidth_) ) deallocate(iWidth_)
    
    if ( allocated(maxItems) ) deallocate(maxItems)
    
    if ( allocated(contType) ) deallocate(contType)
    
    if ( allocated(cTypeUsed_) ) deallocate(cTypeUsed_)
    
    if ( allocated(cStartEnd_) ) deallocate(cStartEnd_)
    
    if ( allocated(cId_) ) deallocate(cId_)
    
    if ( allocated(iId_) ) deallocate(iId_)
    
    iType => NULL()

    iNumber => NULL()

    iLength => NULL()

    iWidth => NULL()

    iId => NULL()

  end subroutine reset
  
end module packdat
