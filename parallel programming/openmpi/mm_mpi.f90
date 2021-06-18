! MPI program for matrix multiplication
! Author: Mekonnen Assefa
! Date: June 2021


program matrix_mul
  
  ! Include MPI Module
  use mpi
  implicit none
  
  ! Data declarations for MPI
  integer:: ierr        ! error signal variable. Standard Value = 0
  integer:: rank        ! process ID (pid) / Number
  integer:: nprocs      ! number of processes

  ! status variable-tells the status of send/receive calls
  integer, dimension(MPI_STATUS_SIZE):: status1

  ! Parameter Declaration

  parameter (NRA = 62)          ! number of rows in matrix A
  parameter (NCA = 15)          ! number of columns in matrix A
  parameter (NCB = 7)           ! number of columns in matrix B
  parameter (MASTER = 0)        ! Process ID for the first Task
  parameter (FROM_MASTER = 1)   ! Setting a message type
  parameter (FROM_WORKER = 2)   ! setting a message type


  integer numworkers, source, dest, mtype, cols, avecol, extra, offset
  integer i, j, k

  real*8 a(NRA, NCA), b(NCA, NCB), c(NRA, NCB)


  ! Initialize MPI
  call MPI_INIT(ierr)
  
  ! Setup Communicator Size 
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

  ! Setup Ranks/IDs for each process
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  ! get hostname for each process
  !call MPI_GET_PROCESSOR_NAME(hostname, namesize, ierr)

  numworkers = nprocs-1
  print *, "Process ID =", rank

  
  ! ========== MAIN PROGRAM =====================
  
  if ( rank == MASTER ) then 
  ! Initialize A and B
  do 30 1 = 1, NRA
    do 30 j = 1, NCA
      a(i, j) = (i-1) + (j-1)
  
  30 continue
  
  do 40 i = 1, NCA
    do 40 j = 1, NCB
      b(i, j) = (i-1)*(j-1)

  40 continue

  

  ! 


  end if 


  call MPI_FINALIZE(ieirr)


end program matrix_mul

