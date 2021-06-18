program ping_pong

  use mpi
  implicit none

  integer:: ierr
  integer:: rank
  integer:: nprocs

  integer, dimension(MPI_STATUS_SIZE):: status1

  integer:: left = 0, center = 0, right = 0

  integer:: ping, pong

  call MPI_INIT(ierr)

  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)


  left = rank-1
  center = rank
  right = rank+1

  ! PING PONG Game

  if ( rank == 0 ) then

    !ping = center+1
    ping = 66

    call MPI_SEND(ping, 1, MPI_INT, right, center, MPI_COMM_WORLD, ierr)
    print *, "Ping from Rank", rank, "Ping =", ping

  else if ( rank == nprocs-1 ) then

    call MPI_RECV(pong, 1, MPI_INT, left, left, MPI_COMM_WORLD, status1, ierr)
    print *, "Pong from Rank", rank, "Pong =", pong

  else
    
    call MPI_RECV(pong, 1, MPI_INT, left, left, MPI_COMM_WORLD, status1, ierr)
    print *, "Pong from Rank", rank, "Pong =", pong

    ping = pong+1

    call MPI_SEND(ping, 1, MPI_INT, right, center, MPI_COMM_WORLD, ierr)
    print *, "Ping from Rank", rank, "Ping =", ping 

  end if

call MPI_FINALIZE(ierr)

end program ping_pong

