program fair_dice_simulation

!*****************************************************************************80
!
!! FAIR_DICE_SIMULATION simulates N throws of two fair dice.
!
!  Usage:
!
!    fair_dice n
!
!    where
!
!    * n is the number of times the dice should be thrown.
!
!*****************************************************************************80
!  Date Created: 25th Jan 2021
!  Last Modified:
!
!  Author:
!
!    Mekonnen Assefa
!
!  Addis Ababa University
!
!   School of Natrual and Computational Science
!   Department of Computational Data Science
!
!  Course:
!
!   Introduction to Computational Science and Basics of Computer Programming
!
!  Course ID: (CDSC 601)
!
!  Instructor: Addisu G. Semie, PhD
!     Asst. Prof.,Computational Data Science Program,
!     Email: addisu.semie@aau.edu.e!
!*****************************************************************************80
!  Parameters:
!
!    Command line, integer ( kind = 4 ) N, the number of times the dice
!    are thrown.
!
!*****************************************************************************80

   implicit none

   integer(kind=4) arg_num
   character(len=255) data_filename
   integer(kind=4) data_unit
   integer(kind=4) die1
   integer(kind=4) die2
   integer(kind=4) i
   integer(kind=4) i4_uniform
   integer(kind=4) iarg
   integer(kind=4) ierror
   integer(kind=4) last
   integer(kind=4) n
   integer(kind=4) seed
   integer(kind=4) score
   integer(kind=4) score_count(12)
   character(len=80) string

   integer, dimension(8) :: time_values
   integer :: start_time, end_time, count_rate


   call date_and_time(VALUES=time_values)

   write (*, '(a)') ''
   write (*, '(a)') 'FAIR_DICE_SIMULATION:'
   write (*, '(a)') '  FORTRAN90 version'
   write (*, '(a)') '  Simulate N throws of a pair of fair dice.'
!
!  Get the number of command line arguments.
!
   arg_num = iargc()
!
!  Get the quadrature file root name:
!
   if (1 <= arg_num) then

      iarg = 1
      call getarg(iarg, string)
      call s_to_i4(string, n, ierror, last)
   else

      write (*, '(a)') ' '
      write (*, '(a)') '  Enter N, the number of times the dice are thrown: '

      read (*, *) n

   end if
!
!  Intialize the sytem wall time
!

   call system_clock(start_time, count_rate)

!
!  Initialize the random number generator.
!
   call get_seed(seed)
!
!  For convenience, include slots for 0 and 1, even though they never occur.
!
   score_count(1:12) = 0
!
!  Roll N times.
!
   do i = 1, n
      die1 = i4_uniform(1, 6, seed)
      die2 = i4_uniform(1, 6, seed)
      score = die1 + die2
      score_count(score) = score_count(score) + 1
   end do
!
!  Write the data file.
!
   data_filename = 'fair_dice_data.txt'
   call get_unit(data_unit)
   open (unit=data_unit, file=data_filename, status='replace')

   do score = 2, 12
      write (data_unit, '(2x,i2,2x,i6)') score, score_count(score)
   end do

   close (unit=data_unit)
   write (*, '(a)') ' '
   write (*, '(a)') '  Created data file "'//trim(data_filename)//'".'
!
!  Terminate.
!
   write (*, '(a)') ''
   write (*, '(a)') 'FAIR_DICE_SIMULATION:'
   write (*, '(a)') ' - Normal end of execution.'
   write (*, '(a)') ''

! 
! PROGRAM FINISHED DATE AND TIME 
!
   
  write(*, '(a,/,5x,2(a,i2.2),a,i4.4,/,5x,3(a,i2.2))') &
        '-------Program finished--------', & 
        'Date: ',time_values(3),'/',time_values(2),'/',time_values(1), &
        'Time: ',time_values(5),':',time_values(6),':',time_values(7)
  write(*, '(a31)') '-----------------------------------------------'
!
! Process WALL TIME
!
  call system_clock(end_time)
  write(*,'(a,f10.3,a)') 'Total Wall time:',dble(end_time -  &
        start_time)/dble(count_rate),' sec'

  write(*, '(a31)') '-----------------------------------------------'

end program fair_dice_simulation

subroutine get_seed(seed)

!*****************************************************************************80
!
!! GET_SEED returns a seed for the random number generator.
!
!
!    The seed depends on the current time, and ought to be (slightly)
!    different every millisecond.  Once the seed is obtained, a random
!    number generator should be called a few times to further process
!    the seed.
!
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) SEED, a pseudorandom seed value.
!
   implicit none

   integer(kind=4), parameter :: i4_huge = 2147483647
   integer(kind=4) seed
   real(kind=8) temp
   character(len=10) time
   character(len=8) today
   integer(kind=4) values(8)
   character(len=5) zone

   call date_and_time(today, time, zone, values)

   temp = 0.0D+00

   temp = temp + real(values(2) - 1, kind=8)/11.0D+00
   temp = temp + real(values(3) - 1, kind=8)/30.0D+00
   temp = temp + real(values(5), kind=8)/23.0D+00
   temp = temp + real(values(6), kind=8)/59.0D+00
   temp = temp + real(values(7), kind=8)/59.0D+00
   temp = temp + real(values(8), kind=8)/999.0D+00
   temp = temp/6.0D+00

   do while (temp <= 0.0D+00)
      temp = temp + 1.0D+00
   end do

   do while (1.0D+00 < temp)
      temp = temp - 1.0D+00
   end do

   seed = int(real(i4_huge, kind=8)*temp)
!
!  Never use a seed of 0 or maximum integer.
!
   if (seed == 0) then
      seed = 1
   end if

   if (seed == i4_huge) then
      seed = seed - 1
   end if

   return
end subroutine get_seed

subroutine get_unit(iunit)


!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
   implicit none

   integer(kind=4) i
   integer(kind=4) ios
   integer(kind=4) iunit
   logical lopen

   iunit = 0

   do i = 1, 99

      if (i /= 5 .and. i /= 6 .and. i /= 9) then

         inquire (unit=i, opened=lopen, iostat=ios)

         if (ios == 0) then
            if (.not. lopen) then
               iunit = i
               return
            end if
         end if

      end if

   end do

   return
   
end subroutine get_unit

function i4_uniform(a, b, seed)


!*****************************************************************************80
!
!! I4_UNIFORM returns a scaled pseudorandom I4.
!
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM, a number between A and B.
!
   implicit none

   integer(kind=4) a
   integer(kind=4) b
   integer(kind=4), parameter :: i4_huge = 2147483647
   integer(kind=4) i4_uniform
   integer(kind=4) k
   real(kind=4) r
   integer(kind=4) seed
   integer(kind=4) value

   if (seed == 0) then
      write (*, '(a)') ' '
      write (*, '(a)') 'I4_UNIFORM - Fatal error!'
      write (*, '(a)') '  Input value of SEED = 0.'
      stop
   end if

   k = seed/127773

   seed = 16807*(seed - k*127773) - k*2836

   if (seed < 0) then
      seed = seed + i4_huge
   end if

   r = real(seed, kind=4)*4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
   r = (1.0E+00 - r)*(real(min(a, b), kind=4) - 0.5E+00) &
       + r*(real(max(a, b), kind=4) + 0.5E+00)
!
!  Use rounding to convert R to an integer between A and B.
!
   value = nint(r, kind=4)

   value = max(value, min(a, b))
   value = min(value, max(a, b))

   i4_uniform = value

   return

end function i4_uniform


subroutine s_to_i4(s, ival, ierror, length)

!*****************************************************************************80
!
!! S_TO_I4 reads an I4 from a string.
!
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) IVAL, the integer value read from the string.
!    If the string is blank, then IVAL will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters of S
!    used to make IVAL.
!
   implicit none

   character c
   integer(kind=4) i
   integer(kind=4) ierror
   integer(kind=4) isgn
   integer(kind=4) istate
   integer(kind=4) ival
   integer(kind=4) length
   character(len=*) s

   ierror = 0
   istate = 0
   isgn = 1
   ival = 0

   do i = 1, len_trim(s)

      c = s(i:i)
!
!  Haven't read anything.
!
      if (istate == 0) then

         if (c == ' ') then

         else if (c == '-') then
            istate = 1
            isgn = -1
         else if (c == '+') then
            istate = 1
            isgn = +1
         else if (lle('0', c) .and. lle(c, '9')) then
            istate = 2
            ival = ichar(c) - ichar('0')
         else
            ierror = 1
            return
         end if
!
!  Have read the sign, expecting digits.
!
      else if (istate == 1) then

         if (c == ' ') then

         else if (lle('0', c) .and. lle(c, '9')) then
            istate = 2
            ival = ichar(c) - ichar('0')
         else
            ierror = 1
            return
         end if
!
!  Have read at least one digit, expecting more.
!
      else if (istate == 2) then

         if (lle('0', c) .and. lle(c, '9')) then
            ival = 10*ival + ichar(c) - ichar('0')
         else
            ival = isgn*ival
            length = i - 1
            return
         end if

      end if

   end do
!
!  If we read all the characters in the string, see if we're OK.
!
   if (istate == 2) then
      ival = isgn*ival
      length = len_trim(s)
   else
      ierror = 1
      length = 0
   end if

   return

end subroutine s_to_i4
