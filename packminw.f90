! ******************************************************************
! ******************************************************************

program algencanma

  use packdat

  implicit none

  ! LOCAL SCALARS
  logical      :: checkder, hasAppended, useReduction
  integer      :: allocerr,hnnzmax,inform,jcnnzmax,m,n,nvparam,i,j, &
       trials, it, container, numberOfSolutions
  real(kind=8) :: cnorm,efacc,efstain,eoacc,eostain,epsfeas,epsopt, &
       f,nlpsupn,snorm,fbest,remArea,limItArea, area

  ! LOCAL ARRAYS
  character(80)         :: filename, solfname
  character(len=15)     :: strtmp
  character(len=80)     :: specfnm,outputfnm,vparam(10)
  logical               :: coded(11)
  logical,      pointer :: equatn(:),linear(:)
  real(kind=8), pointer :: l(:),lambda(:),u(:),x(:),lbest(:)

  ! EXTERNAL SUBROUTINES
  external :: myevalf,myevalg,myevalh,myevalc,myevaljac,myevalhc, &
              myevalfc,myevalgjac,myevalgjacp,myevalhl,myevalhlp, &
              alevalfc,alevalgjac

  ! Set problem's data

  filename = 'data.txt'

  solfname = 'solution.csv'

  open(99, file=solfname)

  write(99, *) ''

  close(99)

  call loadData(filename)

  useReduction = .true.

  ! Apply heuristics

  numberOfSolutions = 0

  if ( useReduction ) call reduction(numberOfSolutions, solfname)

  it = numberOfSolutions

  area = 0.0D0

  do i = 1, nItems

     area = area + iWidth(i) * iLength(i)

  end do 
  write(*,*) 'Items total area:', area

!  call setCurrContainer(3)

  trials = 5

  allocate(lbest(2 * nTItems + 1), x(2 * nTItems + 1), l(2 * nTItems + 1), &
           u(2 * nTItems), stat=allocerr)

  if ( allocerr .ne. 0 ) then

     write(*,*) 'Allocation error in main program'
     
     stop
     
  end if

  allocate(equatn(nTItems),linear(nTItems),lambda(nTItems),stat=allocerr)

  if ( allocerr .ne. 0 ) then

     write(*,*) 'Allocation error in main program'

     stop

  end if

  do while ( nItems .gt. 0 )

     write(*,*) 'Select a container'
     read(*,*) container

     call setCurrContainer(container)
     
     ! Number of variables
     
     n = 2 * nItems + 1

     ! Set lower bounds, upper bounds, and initial guess

     do i = 1, nItems

        l(2 * (i - 1) + 1) = 0.0D0

        u(2 * (i - 1) + 1) = 1.0D+20

        l(2 * i)           = 0.0D0

        u(2 * i) = cWidth - iWidth(i)

     end do

     l(2 * nItems + 1) = 0.0D0

     u(2 * nItems + 1) = 1.0D+20

     ! Constraints

     m = nItems

     equatn(1:m) = .false.

     lambda(1:m) = 0.0d0

     linear(1:m) = .true.

     ! Coded subroutines

     coded(1:11) = .false.

     coded(7:8)  = .true.

     ! Upper bounds on the number of sparse-matrices non-null elements

     jcnnzmax = 2 * nItems

     hnnzmax  = 0

     ! Checking derivatives?

     checkder = .false.

     ! Parameters setting

     epsfeas   = 1.0d-05
     epsopt    = 1.0d-05

     efstain   = sqrt( epsfeas )
     eostain   = epsopt ** 1.5d0

     efacc     = sqrt( epsfeas )
     eoacc     = sqrt( epsopt )

     outputfnm = ''
     specfnm   = ''

     nvparam = 0

     ! Optimize

     filename = 'sol1.asy'

     fbest = 1.0D+20

     write(*,*)
     write(*,*) 'Solving strip problem'
     write(*,*)

     call sortItems(x)

     call initialpoint(x, i * 123455, .false.)

!!$     do i = 1, trials
!!$
!!$        call initialpoint(x, i * 123456, .true.)
!!$
!!$        call drawsol(x, filename, .false.)
!!$
!!$        call algencan(myevalf,myevalg,myevalh,myevalc,myevaljac,myevalhc, &
!!$             myevalfc,myevalgjac,myevalgjacp,myevalhl,myevalhlp,jcnnzmax, &
!!$             hnnzmax,epsfeas,epsopt,efstain,eostain,efacc,eoacc,outputfnm, &
!!$             specfnm,nvparam,vparam,n,x,l,u,m,lambda,equatn,linear,coded, &
!!$             checkder,f,cnorm,snorm,nlpsupn,inform)
!!$
!!$        f = x(n)
!!$
!!$        if (f .lt. fbest) then
!!$
!!$           fbest = f
!!$
!!$           do j = 1, n
!!$
!!$              lbest(j) = x(j)
!!$
!!$           end do
!!$
!!$           call drawSol(lbest, filename, .true.)
!!$
!!$        end if
!!$
!!$     end do

     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!! !
     ! Deal with only one container !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!! !

     ! This step reduces the number of items

!!$     call extractBoxes(lbest)

     call extractBoxes(x)

     write(filename, FMT="('sol',I0.3,'.asy')") it

     limItArea = 1.0D+20

     hasAppended = .true.

     write(*,*)
     write(*,*) 'Solving allocation problem'
     write(*,*)

     do while ( .true. )

        ! Number of variables

        n = 2 * nItems

        ! Set lower bounds, upper bounds, and initial guess

        do i = 1, nItems

           l(2 * (i - 1) + 1) = 0.0D0

           u(2 * (i - 1) + 1) = cLength - iLength(i)

           l(2 * i)           = 0.0D0

           u(2 * i)           = cWidth - iWidth(i)

        end do

        ! Constraints

        m = 0

        equatn(1:m) = .false.

        lambda(1:m) = 0.0d0

        linear(1:m) = .true.

        ! Coded subroutines

        coded(1:11) = .false.

        coded(7:8)  = .true.

        ! Upper bounds on the number of sparse-matrices non-null elements

        jcnnzmax = 0

        hnnzmax  = 0

        ! Checking derivatives?

        checkder = .false.

        ! Parameters setting

        epsfeas   = 1.0d-12
        epsopt    = 1.0d-12

        efstain   = sqrt( epsfeas )
        eostain   = epsopt ** 1.5d0

        efacc     = sqrt( epsfeas )
        eoacc     = sqrt( epsopt )

        outputfnm = ''
        specfnm   = ''

        nvparam = 0

        ! Optimize

        fbest = 1.0D+20

        do i = 1, 20

           call initialpoint(x, i * 123456, .false.)

           call algencan(myevalf,myevalg,myevalh,myevalc,myevaljac,myevalhc, &
                alevalfc,alevalgjac,myevalgjacp,myevalhl,myevalhlp,jcnnzmax, &
                hnnzmax,epsfeas,epsopt,efstain,eostain,efacc,eoacc,outputfnm, &
                specfnm,nvparam,vparam,n,x,l,u,m,lambda,equatn,linear,coded, &
                checkder,f,cnorm,snorm,nlpsupn,inform)

           if (f .lt. fbest) then

              fbest = f

              do j = 1, n

                 lbest(j) = x(j)

              end do

              call drawSol(lbest, filename, .false.)

           end if

           if ( fbest .le. 1.0D-08 ) exit

        end do

        if ( .not. hasAppended ) exit

        if ( fbest .le. 1.0D-08 ) then

           remArea = cWidth * cLength

           do i = 1, nItems

              remArea = remArea - iWidth(i) * iLength(i)

           end do
        
           call appendBox(lbest, remArea, limItArea, hasAppended)

        else

           limItArea = iWidth(nItems) * iLength(nItems)

           call removeAppendedBox()

           remArea = cWidth * cLength

           do i = 1, nItems

              remArea = remArea - iWidth(i) * iLength(i)

           end do
        
           call appendBox(lbest, remArea, limItArea, hasAppended)

        end if

     end do

     call removeBoxes()

     write(*, *)
     write(*, *) 'Remaining items: ', nItems
     write(*, *)

     it = it + 1

  end do

  call printStats()

  deallocate(x,l,lbest,u,lambda,equatn,linear,stat=allocerr)

  if ( allocerr .ne. 0 ) then

     write(*,*) 'Deallocation error in main program'

     stop

  end if

  stop

end program algencanma

! ******************************************************************
! ******************************************************************

!!$subroutine beautyPack()
!!$
!!$  ! Number of variables
!!$  
!!$  n = 2 * nItems
!!$
!!$  ! Set lower bounds, upper bounds, and initial guess
!!$
!!$  do i = 1, nFixed - 1
!!$
!!$     l(2 * (i - 1) + 1) = x(2 * (i - 1) + 1)
!!$
!!$     u(2 * (i - 1) + 1) = x(2 * (i - 1) + 1)
!!$
!!$     l(2 * i)           = x(2 * i)
!!$
!!$     u(2 * i)           = x(2 * i)
!!$
!!$  end do
!!$     
!!$  do i = nFixed, nItems
!!$
!!$     l(2 * (i - 1) + 1) = 0.0D0
!!$
!!$     u(2 * (i - 1) + 1) = cLength - iLength(i)
!!$
!!$     l(2 * i)           = 0.0D0
!!$
!!$     u(2 * i)           = cWidth - iWidth(i)
!!$
!!$  end do
!!$
!!$  ! Constraints
!!$
!!$  m = 0
!!$
!!$  equatn(1:m) = .false.
!!$
!!$  lambda(1:m) = 0.0d0
!!$
!!$  linear(1:m) = .true.
!!$
!!$  ! Coded subroutines
!!$
!!$  coded(1:11) = .false.
!!$
!!$  coded(7:8)  = .true.
!!$
!!$  ! Upper bounds on the number of sparse-matrices non-null elements
!!$
!!$  jcnnzmax = 0
!!$
!!$  hnnzmax  = 0
!!$
!!$  ! Checking derivatives?
!!$
!!$  checkder = .false.
!!$
!!$  ! Parameters setting
!!$
!!$  epsfeas   = 1.0d-12
!!$  epsopt    = 1.0d-12
!!$
!!$  efstain   = sqrt( epsfeas )
!!$  eostain   = epsopt ** 1.5d0
!!$
!!$  efacc     = sqrt( epsfeas )
!!$  eoacc     = sqrt( epsopt )
!!$
!!$  outputfnm = ''
!!$  specfnm   = ''
!!$
!!$  nvparam = 0
!!$
!!$  ! Optimize
!!$
!!$  fbest = 1.0D+20
!!$
!!$  do i = 1, 20
!!$
!!$     call initialpoint(x, i * 123456, .false.)
!!$
!!$     call algencan(myevalf,myevalg,myevalh,myevalc,myevaljac,myevalhc, &
!!$          fievalfc,fievalgjac,myevalgjacp,myevalhl,myevalhlp,jcnnzmax, &
!!$          hnnzmax,epsfeas,epsopt,efstain,eostain,efacc,eoacc,outputfnm, &
!!$          specfnm,nvparam,vparam,n,x,l,u,m,lambda,equatn,linear,coded, &
!!$          checkder,f,cnorm,snorm,nlpsupn,inform)
!!$
!!$     if (f .lt. fbest) then
!!$
!!$        fbest = f
!!$
!!$        do j = 1, n
!!$
!!$           lbest(j) = x(j)
!!$
!!$        end do
!!$
!!$        call drawSol(lbest, filename, .false.)
!!$
!!$     end if
!!$
!!$     if ( fbest .le. 1.0D-08 ) exit
!!$
!!$  end do
!!$
!!$  ! Fixar se deu certo
!!$
!!$  ! Para se deu errado
!!$
!!$  ! Adicionar proximo item semelhante ou o proximo maior
!!$
!!$end do
!!$
!!$end subroutine beautyPack

! ******************************************************************
! ******************************************************************

subroutine myevalf(n,x,f,flag)

  implicit none

  ! SCALAR ARGUMENTS
  integer,      intent(in)  :: n
  integer,      intent(out) :: flag
  real(kind=8), intent(out) :: f

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in) :: x(n)

  flag = - 1

end subroutine myevalf

! ******************************************************************
! ******************************************************************

subroutine myevalg(n,x,g,flag)

  implicit none

  ! SCALAR ARGUMENTS
  integer,      intent(in)  :: n
  integer,      intent(out) :: flag

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: g(n)

  flag = - 1

end subroutine myevalg

! ******************************************************************
! ******************************************************************

subroutine myevalh(n,x,hrow,hcol,hval,hnnz,lim,lmem,flag)

  implicit none

  ! SCALAR ARGUMENTS
  logical,      intent(out) :: lmem
  integer,      intent(in)  :: lim,n
  integer,      intent(out) :: flag,hnnz

  ! ARRAY ARGUMENTS
  integer,      intent(out) :: hcol(lim),hrow(lim)
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: hval(lim)

  flag = - 1

  lmem = .false.

end subroutine myevalh

! ******************************************************************
! ******************************************************************

subroutine myevalc(n,x,ind,c,flag)

  implicit none

  ! SCALAR ARGUMENTS
  integer,      intent(in)  :: ind,n
  integer,      intent(out) :: flag
  real(kind=8), intent(out) :: c

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in)  :: x(n)

  flag = - 1

end subroutine myevalc

! ******************************************************************
! ******************************************************************

subroutine myevaljac(n,x,ind,jcvar,jcval,jcnnz,lim,lmem,flag)

  implicit none

  ! SCALAR ARGUMENTS
  logical, intent(out) :: lmem
  integer, intent(in)  :: ind,lim,n
  integer, intent(out) :: flag,jcnnz

  ! ARRAY ARGUMENTS
  integer,      intent(out) :: jcvar(lim)
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: jcval(lim)

  flag = - 1

  lmem = .false.

end subroutine myevaljac

! ******************************************************************
! ******************************************************************

subroutine myevalhc(n,x,ind,hcrow,hccol,hcval,hcnnz,lim,lmem,flag)

  implicit none

  ! SCALAR ARGUMENTS
  logical, intent(out) :: lmem
  integer, intent(in)  :: ind,lim,n
  integer, intent(out) :: flag,hcnnz

  ! ARRAY ARGUMENTS
  integer,      intent(out) :: hccol(lim),hcrow(lim)
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: hcval(lim)

  flag = - 1

  lmem = .false.

end subroutine myevalhc

! ******************************************************************
! ******************************************************************

subroutine myevalfc(n,x,f,m,c,flag)

  use packdat, only: evalover, nItems, iLength

  implicit none

  ! SCALAR ARGUMENTS
  integer,      intent(in)  :: m,n
  integer,      intent(out) :: flag
  real(kind=8), intent(out) :: f

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: c(m)

  ! LOCAL SCALARS
  integer :: i, j

  flag = 0

  f = x(2 * nItems + 1)

  do i = 1, nItems

     do j = i + 1, nItems

        f = f + evalover(x, i, j)

     end do

     c(i) = x(2 * (i - 1) + 1) + iLength(i) - x(2 * nItems + 1)

  end do

end subroutine myevalfc

! ******************************************************************
! ******************************************************************

subroutine alevalfc(n,x,f,m,c,flag)

  use packdat, only: evalover, nItems, iLength

  implicit none

  ! SCALAR ARGUMENTS
  integer,      intent(in)  :: m,n
  integer,      intent(out) :: flag
  real(kind=8), intent(out) :: f

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: c(m)

  ! LOCAL SCALARS
  integer :: i, j

  flag = 0

  f = 0.0D0

  do i = 1, nItems

     do j = i + 1, nItems

        f = f + evalover(x, i, j)

     end do

  end do

end subroutine alevalfc

! ******************************************************************
! ******************************************************************

subroutine myevalgjac(n,x,g,m,jcfun,jcvar,jcval,jcnnz,lim,lmem,flag)

  use packdat, only : evaldover, nItems

  implicit none

  ! SCALAR ARGUMENTS
  logical,      intent(out) :: lmem
  integer,      intent(in)  :: lim,m,n
  integer,      intent(out) :: flag,jcnnz

  ! ARRAY ARGUMENTS
  integer,      intent(out) :: jcfun(lim),jcvar(lim)
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: g(n),jcval(lim)

  ! LOCAL SCALARS
  integer :: i, j

  flag = 0

  lmem = .false.

  do i = 1, 2 * nItems + 1

     g(i) = 0.0D0

  end do

  jcnnz = 0

  do i = 1, nItems

     do j = i + 1, nItems

        call evaldover(x, i, j, g)

     end do

     jcnnz = jcnnz + 1

     jcfun(jcnnz) = i
     jcvar(jcnnz) = 2 * (i - 1) + 1
     jcval(jcnnz) = 1.0D0

     jcnnz = jcnnz + 1

     jcfun(jcnnz) = i
     jcvar(jcnnz) = 2 * nItems + 1
     jcval(jcnnz) = - 1.0D0

  end do

  g(2 * nItems + 1) = 1.0D0

end subroutine myevalgjac

! ******************************************************************
! ******************************************************************

subroutine alevalgjac(n,x,g,m,jcfun,jcvar,jcval,jcnnz,lim,lmem,flag)

  use packdat, only : evaldover, nItems

  implicit none

  ! SCALAR ARGUMENTS
  logical,      intent(out) :: lmem
  integer,      intent(in)  :: lim,m,n
  integer,      intent(out) :: flag,jcnnz

  ! ARRAY ARGUMENTS
  integer,      intent(out) :: jcfun(lim),jcvar(lim)
  real(kind=8), intent(in)  :: x(n)
  real(kind=8), intent(out) :: g(n),jcval(lim)

  ! LOCAL SCALARS
  integer :: i, j

  flag = 0

  lmem = .false.

  do i = 1, 2 * nItems

     g(i) = 0.0D0

  end do

  do i = 1, nItems

     do j = i + 1, nItems

        call evaldover(x, i, j, g)

     end do

  end do

  jcnnz = 0

end subroutine alevalgjac

! ******************************************************************
! ******************************************************************

subroutine myevalgjacp(n,x,g,m,p,q,work,gotj,flag)

  implicit none

  ! SCALAR ARGUMENTS
  logical,   intent(inout) :: gotj
  integer,   intent(in)    :: m,n
  integer,   intent(out)   :: flag
  character, intent(in)    :: work

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in)    :: x(n)
  real(kind=8), intent(inout) :: p(m),q(n)
  real(kind=8), intent(out)   :: g(n)

  flag = - 1

end subroutine myevalgjacp

! ******************************************************************
! ******************************************************************

subroutine myevalhl(n,x,m,lambda,sf,sc,hlrow,hlcol,hlval,hlnnz,lim,lmem,flag)

  implicit none

  ! SCALAR ARGUMENTS
  logical,      intent(out) :: lmem
  integer,      intent(in)  :: lim,m,n
  integer,      intent(out) :: flag,hlnnz
  real(kind=8), intent(in)  :: sf

  ! ARRAY ARGUMENTS
  integer,      intent(out) :: hlcol(lim),hlrow(lim)
  real(kind=8), intent(in)  :: lambda(m),sc(m),x(n)
  real(kind=8), intent(out) :: hlval(lim)

  flag = - 1

end subroutine myevalhl

! ******************************************************************
! ******************************************************************

subroutine myevalhlp(n,x,m,lambda,sf,sc,p,hp,goth,flag)

  implicit none

  ! SCALAR ARGUMENTS
  logical,      intent(inout) :: goth
  integer,      intent(in)    :: m,n
  integer,      intent(out)   :: flag
  real(kind=8), intent(in)    :: sf

  ! ARRAY ARGUMENTS
  real(kind=8), intent(in)  :: lambda(m),p(n),sc(m),x(n)
  real(kind=8), intent(out) :: hp(n)

  flag = - 1

end subroutine myevalhlp

