! Hello World Program in Fortran using MPI
! Author : Mekonnen Assefa
! June 2021


program hello_mpi

  !include MPI module
  use mpi

  implicit none

  ! Data Declaration for MPI
  integer:: ierr           ! error signal variable standard value = 0
  integer:: rank           ! the process ID #
  integer:: nprocs         ! number of processe

  ! Initialize MPI
  ! Initialize subroutine

  call MPI_INIT(ierr)

  ! Setup communicator size
  ! Vairiable order : Communicator, number of processes, ierr

  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

  ! Setup Ranks for each process
  ! Variable order: Communicator, rank variable, ierr

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  ! The running code 
  print *, "Hello world  ! I am process ", rank, "of", nprocs, "Processes"
  ! print *, "Rank", rank, "from", nprocs

  ! Finalize MPI
  ! Finalizer subroutine

  call MPI_FINALIZE(ierr)

end program hello_mpi


