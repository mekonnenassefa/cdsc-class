program main
  implicit none
  include "mpif.h"
  integer myid, master, numprocs, ierr
  integer, dimension(MPI_STATUS_SIZE) :: stat
  call MPI_INIT( ierr)
  call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
  call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr)
  master = 0
  if ( myid == master ) then
    call master_func(myid, master, numprocs, ierr, stat)
  else
    continue
!   call slave_func(myid, master, numprocs, ierr, stat)
  end if
  call MPI_FINALIZE ( ierr )
  stop
end program

subroutine master_func(myid, master, numprocs, ierr, stat)
  implicit none
  include "mpif.h"
  integer i,j
  integer myid, master, numprocs, ierr
  integer rows1, cols1, rows2, cols2, rows3, cols3
  integer, dimension(MPI_STATUS_SIZE) :: stat
  real, allocatable, dimension(:,:) :: A
  real, allocatable, dimension(:,:) :: B
  real, allocatable, dimension(:,:) :: C
  do while (.true.)
    print *, "Enter rows1 cols1 rows2 cols2, zero or negative for exit"
    read (*,*) rows1, cols1, rows2, cols2
    if (cols1 <= 0 .or. rows1 <= 0 .or. &
             cols2 <= 0 .or. rows2 <= 0) then
      print *, "Exiting"
      return
    else if (cols1 /= rows2) then
      print *, "cols1 must equal rows2."
      cycle
    end if
    rows3 = rows1
    cols3 = cols2
    allocate(A(1:rows1, 1:cols1), B(1:rows2, 1:cols2), C(1:rows3,1:cols3))
    do i = 1,rows1
      do j = 1,cols1
        if (i == j) then
          A(i,j) = 1
        else
          A(i,j) = 0
        end if
      end do
    end do
    do i = 1,rows2
      do j = 1,cols2
        B(i,j) = i + j
      end do
    end do
    call multiply(rows1, cols1, A, rows2, cols2, B, rows3, cols3, C, &
                  myid, master, numprocs, ierr, stat)
    call print_matrix(rows3, cols3, C)
    deallocate(A, B, C)
  end do
end subroutine

subroutine multiply(rowsA, colsA, A, rowsB, colsB, B, rowsC, colsC, C, &
                    myid, master, numprocs, ierr, stat)
  implicit none
  include "mpif.h"
  integer :: myid, master, numprocs, ierr
  integer :: colsA, rowsA, colsB, rowsB, rowsC, colsC
  integer :: num_sent, num_received, tag
  integer :: i, j, k
  integer, dimension(MPI_STATUS_SIZE) :: stat
  real, dimension(rowsA, colsA) :: A
  real, dimension(rowsB, colsB) :: B
  real, dimension(rowsC, colsC) :: C
  real, dimension(colsC) :: buffer
  ! First distribute the requisite parameter
  call MPI_BCAST(rowsA, 1, MPI_REAL, master, MPI_COMM_WORLD, ierr)
  call MPI_BCAST(colsA, 1, MPI_REAL, master, MPI_COMM_WORLD, ierr)
  call MPI_BCAST(rowsB, 1, MPI_REAL, master, MPI_COMM_WORLD, ierr)
  call MPI_BCAST(colsB, 1, MPI_REAL, master, MPI_COMM_WORLD, ierr)

  ! Distibute B to all slaves
  call MPI_BCAST(B, rowsB*colsB, MPI_REAL, master, MPI_COMM_WORLD, ierr)

  ! For each row, distribute a row of A to a supplicant slave
  num_sent = 0
  num_received = 0
  do i = 1,min(numprocs - 1, rowsC)
    call MPI_SEND(A(i,:), colsA, i, i, MPI_COMM_WORLD, ierr)
    num_sent = num_sent + 1
  end do

  ! Now, while there are remaining rows, grab incoming messages and pass rows out
  do while (num_sent < rowsC)
    call MPI_RECV(buffer, colsC, MPI_ANY_SOURCE, MPI_ANY_TAG, stat, ierr)
    num_received = num_received + 1
    C(:, stat(MPI_TAG)) = buffer(:)
    call MPI_SEND(A(num_sent+1,:), colsA, MPI_REAL, stat(MPI_SOURCE), num_sent+1, &
                  MPI_COMM_WORLD, ierr)
    num_sent = num_sent + 1
  end do

  ! Now receive the remaining messages, terminating processes as we go
  do while (num_received < rowsC)
    call MPI_RECV(buffer, colsC, MPI_ANY_SOURCE, MPI_ANY_TAG, stat, ierr)
    num_received = num_received + 1
    C(:, stat(MPI_TAG)) = buffer(:)
  end do

  do i = 1:numprocs - 1
    call MPI_SEND(1.0, 0, MPI_REAL, stat(MPI_SOURCE), 0, MPI_COMM_WORLD, ierr)
  end do
end subroutine

subroutine slave_func(myid, master, numprocs, ierr, stat)
  implicit none
  include "mpif.h"
  integer :: myid, master, numprocs, ierr
  integer :: colsA, rowsA, colsB, rowsB, rowsC, colsC
  integer :: i, j, k
  integer, dimension(MPI_STATUS_SIZE) :: stat
  real, allocatable, dimension(:,:) :: B
  real, allocatable, dimension(:) :: data_buffer
  real, allocatable, dimension(:) :: answer_buffer
  do while (.true.)
    call MPI_RECV(rowsA, 1, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, stat, ierr)
    call MPI_RECV(colsA, 1, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, stat, ierr)
    call MPI_RECV(rowsB, 1, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, stat, ierr)
    call MPI_RECV(colsB, 1, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, stat, ierr)

    allocate(B(1:rowsB, 1:colsB), data_buffer(colsA), answer_buffer(colsB)
    call MPI_RECV(B, rowsB*colsB, MPI_REAL, master, MPI_ANY_TAG, MPI_COMM_WORLD, stat, ierr)
    

  end do
end subroutine

subroutine print_matrix(rows, cols, matrix)
  implicit none
  integer :: rows, cols, i, j
  real, dimension(rows, cols) :: matrix
  do i = 1,rows
    do j = 1,cols
      write (*, "(f10.2)", advance="no") matrix(i,j)
    end do
    print *, ""
  end do
end subroutine
