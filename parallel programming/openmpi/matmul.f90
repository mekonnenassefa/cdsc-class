      program matmul

      include 'mpif.h'

      parameter (NRA = 62)
      parameter (NCA = 15)
      parameter (NCB = 7)
      parameter (MASTER = 0)
      parameter (FROM_MASTER = 1)
      parameter (FROM_WORKER = 2)

      integer 	numtasks,taskid,numworkers,source,dest,mtype,
     &          cols,avecol,extra, offset,i,j,k,ierr
      integer status(MPI_STATUS_SIZE)
      real*8	a(NRA,NCA), b(NCA,NCB), c(NRA,NCB)

C  *------------------->
C *---> Initialize MPI
C  *------------------->

      call MPI_INIT(ierr)

C  *------------------------------------------------->
C *---> Determine my rank in the global communicator
C  *------------------------------------------------->

      call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierr)

C  *-------------------------------------------------->
C *---> Determine the size of the global communicator
C  *-------------------------------------------------->

      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      numworkers = numtasks-1
      
C  *---------------------->
C ***-=-=-=-> MASTER TASK
C  *---------------------->

      if (taskid .eq. MASTER) then

C  *----------------------->
C *---> Initialize A and B
C  *----------------------->

        do 30 i=1, NRA
          do 30 j=1, NCA
          a(i,j) = (i-1)+(j-1)
 30     continue
        do 40 i=1, NCA
          do 40 j=1, NCB
	    b(i,j) = (i-1)*(j-1)
 40     continue

C  *----------------------------------------->
C *---> Send matrix data to the worker tasks
C  *----------------------------------------->

        avecol = NCB/numworkers
        extra = mod(NCB, numworkers)
        offset = 1
        mtype = FROM_MASTER
        do 50 dest=1, numworkers
          if (dest .le. extra) then
            cols = avecol + 1
          else
            cols = avecol
          endif
          write(*,*)'   sending',cols,' cols to task',dest

C  *-------------------------------------------------------------------->
C *---> Send info telling each processor where to find data in matrix A
C  *-------------------------------------------------------------------->

          call MPI_SEND(offset, 1, MPI_INTEGER, dest, mtype, 
     &                  MPI_COMM_WORLD, ierr)

C  *------------------------------------------------------------------->
C *---> Send each processor the # of columns they will need to compute
C  *------------------------------------------------------------------->

          call MPI_SEND(cols, 1, MPI_INTEGER, dest, mtype, 
     &                  MPI_COMM_WORLD, ierr)

C  *------------------------------------->
C *---> Send each processor the matrix A
C  *------------------------------------->

          call MPI_SEND(a, NRA*NCA, MPI_DOUBLE_PRECISION, dest, mtype, 
     &                  MPI_COMM_WORLD, ierr)

C  *------------------------------------------------------------------------>
C *---> Send each processor the offset portion of matrix B to be calculated
C  *------------------------------------------------------------------------>

          call MPI_SEND(b(1,offset), cols*NCA, MPI_DOUBLE_PRECISION,
     &                  dest, mtype, MPI_COMM_WORLD, ierr)
          offset = offset + cols
 50     continue

C  *-------------------------------------->
C *---> Receive results from worker tasks
C  *-------------------------------------->

        mtype = FROM_WORKER
        do 60 i=1, numworkers
          source = i

C  *---------------------------------------------------------->
C *---> Receive the offset postition from the sending process
C  *---------------------------------------------------------->

          call MPI_RECV( offset, 1, MPI_INTEGER, source,
     &                   mtype, MPI_COMM_WORLD, status, ierr )

C  *---------------------------------------------------------------->
C *---> Receive the # of columns computed by the sender in matrix C
C  *---------------------------------------------------------------->

          call MPI_RECV( cols, 1, MPI_INTEGER, source,
     &                   mtype, MPI_COMM_WORLD, status, ierr )

C  *--------------------------------------------------------------->
C *---> Receive the final matrix C starting at the offset position
C  *--------------------------------------------------------------->

          call MPI_RECV( c(1,offset), cols*NRA, MPI_DOUBLE_PRECISION, 
     &                   source, mtype, MPI_COMM_WORLD, status, ierr )
 60     continue

C  *------------------>
C *---> Print results
C  *------------------>

        print*, 'Here are the first 30 rows of the result matrix: '
        do 90 i=1, 30
          do 80 j = 1, NCB
            write(*,70)c(i,j)
  70        format(2x,f8.2,$)
  80      continue
          print *, ' '
  90    continue
      endif

C  *=-=-=-=-=-=-=-=-=-=-=-=>
C ***-=-=-=-> WORKER TASKS
C  *=-=-=-=-=-=-=-=-=-=-=-=>

      if (taskid .gt. MASTER) then

C  *----------------------------------------->
C *---> Receive matrix data from master task
C  *----------------------------------------->

        mtype = FROM_MASTER

C  *----------------------------------------------------------------->
C *---> Receive the postion in matrix B where I am supposed to start
C  *----------------------------------------------------------------->

        call MPI_RECV(offset, 1, MPI_INTEGER, MASTER, mtype,
     &                MPI_COMM_WORLD, status, ierr)

C  *----------------------------------------------------------->
C *---> Receive the # of columns that I am required to compute
C  *----------------------------------------------------------->

        call MPI_RECV(cols, 1, MPI_INTEGER, MASTER, mtype,
     &                MPI_COMM_WORLD, status, ierr)

C  *----------------------------------------->
C *---> Receive the matrix A from the master
C  *----------------------------------------->

        call MPI_RECV(a, NRA*NCA, MPI_DOUBLE_PRECISION, MASTER, mtype,
     &                MPI_COMM_WORLD, status, ierr)

C  *----------------------------------------------------------------->
C *---> Receive the portion of matrix B used to compute final values
C  *----------------------------------------------------------------->

        call MPI_RECV(b, cols*NCA, MPI_DOUBLE_PRECISION, MASTER, mtype,
     &                MPI_COMM_WORLD, status, ierr)

C  *----------------------->
C *---> Do matrix multiply
C  *----------------------->

        do 100 k=1, cols
          do 100 i=1, NRA
            c(i,k) = 0.0
            do 100 j=1, NCA
              c(i,k) = c(i,k) + a(i,j) * b(j,k)
  100   continue

C  *------------------------------------->
C *---> Send results back to master task
C  *------------------------------------->

        mtype = FROM_WORKER

C  *--------------------------------------------->
C *---> Send the offset value back to the master
C  *--------------------------------------------->

        call MPI_SEND(offset, 1, MPI_INTEGER, MASTER, mtype, 
     &                MPI_COMM_WORLD, ierr)

C  *------------------------------------------------------------------------->
C *---> Send the # of columns used to compute the final values to the master
C  *------------------------------------------------------------------------->

        call MPI_SEND(cols, 1, MPI_INTEGER, MASTER, mtype, 
     &                MPI_COMM_WORLD, ierr)

C  *------------------------------------------------------------->
C *---> Send my portion of the final matrix C back to the master
C  *------------------------------------------------------------->

        call MPI_SEND(c, cols*NRA, MPI_DOUBLE_PRECISION, MASTER, mtype,
     &                MPI_COMM_WORLD, ierr)
      endif

C  *----------------->
C *---> Finalize MPI
C  *----------------->

      call MPI_FINALIZE(ierr)
      end
