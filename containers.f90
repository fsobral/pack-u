module containers

  ! This module creates the type 'Container' which contains
  ! information about a container and the items that are packed inside
  ! it. It also provides some utility subroutines for printing, saving
  ! and working with containers.
  
  use items
  
  implicit none

  type Container

     ! Type of the container
     integer :: type
     
     ! Dimensions of the container
     real(8) :: length, width
     
     ! Number of packed items
     integer :: nItems
     
     ! List of packed items
     type(Item), allocatable :: pItems(:)
     
  end type Container
  
contains


  ! ***********************************************************
  ! ***********************************************************

  subroutine drawSol(c, output)

    ! This subroutine creates a .ASY file in order to draw the
    ! solution contained in a Container 'c'.

    implicit none

    ! SCALAR ARGUMENTS
    logical :: strip

    ! ARRAY ARGUMENTS
    character(80) :: output
    type(Container) :: c

    intent(in) :: c, output

    ! LOCAL SCALARS
    integer :: i
    real(8) :: scale
    type(Item) :: it

    ! Try to find a reasonable scale for drawing

    scale = max(1.0D-4, min(1.0D0, 30.0D0 / max(c%length, c%width)))

    ! Draws the solution in ASY format

    open(99, file=output)

    write(99, FMT=0001) scale

    ! Draw items

    do i = 1, c%nItems

       it = c%pItems(i)
       
       write(99, FMT=0003) it%x, it%y, it%length, it%width, &
            it%type, it%number

    end do

    ! Draw containers

    write(99, FMT=0002) c%length, c%width

    close(99)

0001 FORMAT('import packlib;',/,/,'settings.outformat = "pdf";',/, &
          'unitsize(',F10.5,'cm);')
0002 FORMAT('draw(box((0,0), (',F10.5,',',F10.5,')), currentpen + dashed + 2);')
0004 FORMAT('draw(box((0,0), (',F10.5,',',F10.5,')), currentpen + 2);')
0003 FORMAT('drawBox((',F10.5,',',F10.5,'), ',F10.5,', ',F10.5, &
            ', cor(',I2,'), currentpen, "', I4, '");')

  end subroutine drawSol
  
end module containers
