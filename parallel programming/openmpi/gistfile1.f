! ============================================================================
! Name        : ex5.f90
! Author      : Sajjad Rizvi
! Version     :
! Copyright   : 
! Description : Hello MPI World in Fortran
! ============================================================================

use mpi
implicit none

integer, parameter :: LEN = 100               ! message length

integer            :: ierror                  ! error code
integer            :: my_rank                 ! rank of process
integer            :: p                       ! number of processes
integer            :: source                  ! rank of sender
integer            :: dest                    ! rank of receiver
integer            :: tag                     ! tag for messages
character(len=LEN) :: message                 ! storage for message
integer            :: status(MPI_STATUS_SIZE) ! return status for receive

integer, parameter :: n=16, r = 16, m = 16
integer, dimension(n,r) :: A
integer, dimension(r,m) :: B
integer, dimension(n,m) :: C
integer, allocatable, dimension(:) :: col, row
integer, allocatable, dimension(:,:) :: subBlockA, subBlockB, subBlockC
integer :: bs, rw, cl
integer :: i , j, k, src, ind


tag = 0

! start up MPI

call MPI_Init(ierror)

! find out process rank
call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

! find out number of processes
call MPI_Comm_size(MPI_COMM_WORLD, p, ierror)


!if (my_rank .ne. 0) then
!    ! create message
!    write (message, *) "Hello MPI World from process ", my_rank
!    dest = 0
!    call MPI_Send(message, LEN, MPI_CHARACTER, &
!            dest, tag, MPI_COMM_WORLD, ierror)
!else
!    print *, "Hello MPI World From process 0: Num processes: ", p
!    do source = 1, p-1
!        call MPI_Recv(message, LEN, MPI_CHARACTER, source, tag, &
!                MPI_COMM_WORLD, status, ierror)
!        print *, message
!    end do
!end if




!///////////////////////////////

do i=1,n
  do j=1,r
    A(i,j) = 1 !i*10 + j
    B(i,j) = 1 !i*10 + j
  enddo
enddo

bs = n/p
allocate(subBlockA(bs,r), subBlockB(r, bs), subBlockC(bs, bs))
!allocate(subBlockB(r,bs))
!allocate(subBlockC(bs,bs))

!print *, A
!print *, B

!allocate(col(n/p))
!allocate(row(m/p))

!allocate(col(bs))
!allocate(row(bs))

!do i=1,r
!  row(i) = A(my_rank+1,i)
!  col(i) = B(i, my_rank+1)
!enddo

!print *, my_rank,' - ', col
!print *, my_rank, ' - ', row
rw = 1
cl = 1

!print *, ((my_rank*bs)+1), ' - ', ((my_rank+1)*bs)

do i=(my_rank*bs)+1, (my_rank+1)*bs !((my_rank+1)*bs)-bs,(my_rank+1)*bs
  do j=1, r
!    print *, my_rank, ' - ', i, ' - ', j
    subBlockA(rw,cl) = A(i,j)
!!    subBlockB(cl,rw) = B(j,i)

!	print *, cl
    cl = cl + 1
  enddo
!  print *, '  ', rw
  rw = rw + 1
  cl = 1
enddo

!subBlockC = matmul(subBlockA, subBlockB)

if (my_rank == 0) then
  dest = p-1
else
  dest = my_rank-1
endif

if (my_rank == p-1) then
  src = 0
else
  src = my_rank+1
endif

!print *, my_rank, ' - ', subBlockA
!print *, my_rank, ' - ', subBlockB
!print *, subBlockC

ind = my_rank

do k=1, p
  rw = 1
  cl = 1

  do i=(ind*bs)+1, (ind+1)*bs !((my_rank+1)*bs)-bs,(my_rank+1)*bs
    do j=1, r
!      print *, my_rank, '-', i, '-', j, '-', rw, '-', cl
!      subBlockA(rw,cl) = A(i,j)
      subBlockB(cl,rw) = B(j,i)
      cl = cl + 1
    enddo
    rw = rw + 1
    cl = 1
  enddo

  subBlockC = matmul(subBlockA, subBlockB)

!  print *, 'subA ', subBlockA
!  print *, 'subB ', subBlockB
!if (my_rank == 0) then
!  print *, 'subC ', subBlockC
!endif

  rw=1
  cl=1

print *, my_rank, ((ind*bs)+1), ((ind+1)*bs), k, ind, bs

  do i=(ind*bs)+1, (ind+1)*bs
    do j=(ind*bs)+1, (ind+1)*bs
!      print *, my_rank, i, j, rw, cl

!      print *, my_rank, subBlockC(rw, cl)

      C(i,j) = subBlockC(rw,cl)

!print *,my_rank, i, j, rw, cl, C(i, j)
print *, my_rank, C(i-1, j-1), C(i, j)

!if (my_rank == 0) then
!      print *, i, j, C(i,j)
!endif

      cl = cl + 1
    enddo
    rw = rw + 1
    cl = 1
  enddo

print *, ' '

!  print *,my_rank, 'dest=', dest
!  print *,my_rank, 'src=', src

!  if (my_rank == 1) then
!    print *, subBlockA
!  endif

  call MPI_SEND(subBlockA, bs*r, MPI_INTEGER, dest, 0, MPI_COMM_WORLD, ierror)
  call MPI_RECV(subBlockA, bs*r, MPI_INTEGER, src, 0, MPI_COMM_WORLD, ierror)

!  if (my_rank == 0) then
!    print *, subBlockA
!  endif

  ind = ind - 1
  if (ind < 0) then
    ind = 7
  endif
enddo

call MPI_Barrier(MPI_COMM_WORLD, ierror)

!if (my_rank == 7) then
  print *, C
!endif




call MPI_Finalize(ierror)

stop
end program

