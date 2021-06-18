! MPI Program for Broadcast subroutine 
! Author : Mekonnen Assefa
! June 2021


program bcast
  
  !include MPI module
  use mpi
  !include "mpif.h"

  !implicit none

  ! Data Declaration for MPI
  integer:: ierr           ! error signal variable standard value = 0
  integer:: rank           ! the process ID #
  integer:: nprocs         ! number of processe

  ! Status variable for send/receive calls

  integer, dimension(MPI_STATUS_SIZE):: status1

  ! Data for broadcasting
  integer:: x1 = 0

  ! Rank that will broadcast
  integer:: choice = 2

  call MPI_INIT (ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  ! Main code

  print *, "Before broadcast, Rank, Data = ", rank, x1

  ! Broadcasting process modifies Data

  if ( rank == choice ) then
    x1 = (rank+2)**2
  end if

  ! Broadcasting processs broadcasting data
  ! Syntax: call MPI_BCAST(start, count, datatype, root, comm, ierr)

  call MPI_BCAST(x1, 1, MPI_INT, choice, MPI_COMM_WORLD, ierr)

  print *, "After broadcast, Rank, Data = ", rank, x1

  ! Finalize MPI
  ! Finalizer subroutine

  call MPI_FINALIZE(ierr)

end program bcast 

