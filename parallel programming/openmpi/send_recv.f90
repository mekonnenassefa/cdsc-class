! MPI Program for basic send and receive call
! Author : Mekonnen Assefa
! June 2021


program hello_mpi

  !include MPI module
  !use mpi
  include "mpif.h"

  !implicit none

  ! Data Declaration for MPI
  integer:: ierr           ! error signal variable standard value = 0
  integer:: rank           ! the process ID #
  integer:: nprocs         ! number of processe

  ! Status variable for send/receive calls

  integer, dimension(MPI_STATUS_SIZE):: status1

  ! Arrangment for saving hostname
  character*(MPI_MAX_PROCESSOR_NAME):: hostname
  integer:: namesize
  
  ! Test data
  ! Data 0 for process 0,  Data 1 for process 1
  integer:: data0 = 0, data1 = 0

  ! Initialize MPI
  ! Initialize subroutine
  
  call MPI_INIT(ierr)

  ! Setup communicator size
  ! Vairiable order : Communicator, number of processes, ierr

  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

  ! Setup Ranks for each process
  ! Variable order: Communicator, rank variable, ierr

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  ! Get hostname for each process

  call MPI_GET_PROCESSOR_NAME(hostname, namesize, ierr)

  
  ! The running code 
  print *, "Hello I am ", hostname(1:namesize), " with rank ", rank, " of ", nprocs, "processes"
  ! Invoke master process
  if ( rank == 0 ) then

    ! Assign a data value
    data0 = 50
    print *, "Rank ", rank, "Modified Data 0 =", data0

    ! send data to rank 1
    ! Syntax: call MPI_SEND(start_address, count, datatype, destination pid/rank, tag, communicator, ierr)
    ! Tag is an unique identifier. Integer value-0 - 32767. Tag pairs up send and receive calls.

    call MPI_SEND(data0, 1, MPI_INT, 1, 1, MPI_COMM_WORLD, ierr)
    print *, "Rank ", rank, "sent data 0 to rank 1"

    ! Receive Data 1 from rank 1
    ! syntax call MPI_RECV(start_address, count, datatype, source, tag, communicator, status, ierr)

    call MPI_RECV(data0, 1, MPI_INT, 1, 2, MPI_COMM_WORLD, status1, ierr)
    print *, "Rank ", rank, "Received Data 1 from Rank 1 as Data 0 = ", data0

  end if

  ! Invoke Non-Master Process of rank = 1

  if ( rank == 1) then

    ! Receive Data 0 from Rank 0

    call MPI_RECV(data1, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, status1, ierr)
    print *, "Rank ", rank, "Received Data 0 from Rank 0 as Data 1 = ", data1

    ! Assign a new data value
    data1 = 100
    print *, "Rank ", rank, "Modified Data 1 = ", data1

    ! Send data to Rank 0
    call MPI_SEND(data1, 1, MPI_INT, 0, 2, MPI_COMM_WORLD, ierr)
    print *, "Rank ", rank, "Sent Data 1 to Rank 0"

  end if

  ! Finalize MPI
  ! Finalizer subroutine

  call MPI_FINALIZE(ierr)

end program hello_mpi

