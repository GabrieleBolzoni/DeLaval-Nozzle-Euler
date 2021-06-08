MODULE gnufor

   IMPLICIT NONE

   PRIVATE get_unit, run_gnuplot, timestamp, write_vector_data, &
           write_vector_plot, write_xy_data, write_xy_data_video, &
           write_xy_plot, write_xy_plot_video

   ! useful functions
   PUBLIC plot_function, plot_video

CONTAINS

! LQ AG

subroutine plot_function (x, y)

  implicit none

  real(kind=8), dimension(:), intent(in) :: x, y

  character ( len = 100 ) :: command_file_name = 'plot_function_commands.txt'
  character ( len = 100 ) :: data_file_name = 'plot_function_data.txt'

  integer :: ierror

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'plot_function'
  write ( *, '(a)' ) '  To plot a simple set of (X,Y) data,'
  write ( *, '(a)' ) '  WRITE_XY_DATA writes the data file,'
  write ( *, '(a)' ) '  WRITE_XY_PLOT writes the plot command file.'

  call write_xy_data ( data_file_name, size(x), x, y, ierror)

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_function'
    write ( *, '(a,i6)' ) '  WRITE_XY_DATA returned IERROR = ', ierror
  end if

  call write_xy_plot ( command_file_name, data_file_name, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_function'
    write ( *, '(a,i6)' ) '  WRITE_XY_PLOT returned IERROR = ', ierror
  end if

  call run_gnuplot ( command_file_name )

end subroutine plot_function

subroutine plot_two_functions (x, y1, y2)
!luigi vigevano
  implicit none

  real(kind=8), dimension(:), intent(in) :: x, y1, y2

  character ( len = 100 ) :: command_file_name = 'plot_function_commands.txt'
  character ( len = 100 ) :: data_file_name = 'plot_function_data.txt'

  integer :: ierror

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'plot_function'
  write ( *, '(a)' ) '  To plot a simple set of (X,Y) data,'
  write ( *, '(a)' ) '  WRITE_XY_DATAA writes the data file,'
  write ( *, '(a)' ) '  WRITE_XY_PLOTT writes the plot command file.'

  call write_xy_dataa ( data_file_name, size(x), x, y1, y2, ierror)

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_function'
    write ( *, '(a,i6)' ) '  WRITE_XY_DATAA returned IERROR = ', ierror
  end if

  call write_xy_plott ( command_file_name, data_file_name, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_function'
    write ( *, '(a,i6)' ) '  WRITE_XY_PLOTT returned IERROR = ', ierror
  end if

  call run_gnuplot ( command_file_name )

end subroutine plot_two_functions


subroutine plot_functionn (x, y)

  implicit none

  real(kind=8), dimension(:), intent(in) :: x, y

  character ( len = 100 ) :: command_file_name = 'plot_function_commands.txt'
  character ( len = 100 ) :: data_file_name = 'plot_function_data.txt'

  integer :: ierror

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'plot_function'
  write ( *, '(a)' ) '  To plot a simple set of (X,Y) data,'
  write ( *, '(a)' ) '  WRITE_XY_DATA writes the data file,'
  write ( *, '(a)' ) '  WRITE_XY_PLOT writes the plot command file.'

  call write_xy_data ( data_file_name, size(x), x, y, ierror)

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_function'
    write ( *, '(a,i6)' ) '  WRITE_XY_DATA returned IERROR = ', ierror
  end if

  !call write_xy_plott ( command_file_name, data_file_name, ierror )

  !if ( ierror /= 0 ) then
  !  write ( *, '(a)' ) ' '
  !  write ( *, '(a)' ) 'plot_function'
  !  write ( *, '(a,i6)' ) '  WRITE_XY_PLOT returned IERROR = ', ierror
  !end if

  !call run_gnuplot ( command_file_name )

end subroutine plot_functionn

!*******************************************************************************
! RA, JB   Riccardo Albi e Jacopo Banchetti 7 gennaio 2015

subroutine plot_video (XX, YY,  fn, t, save_flag)

    ! INPUT:
    ! X, matrice (eventualmente di una sola colonna)
    ! Y, matrice
    !
    ! fn, radice del filename, opzionale
    ! t, tempo di pausa, opzionale
    ! save_flag, tipo logico, se vero salva un file contenente
    !            il video senza pero' mostrare subito l'animazione.
    !
    ! Mostra un video, dove ogni fotogramma e' un grafico 2D
    ! composto da una colonna della matrice XX e dalla corrispondente
    ! colonna della matrice YY.
    !
    ! Se XX contiene UNA sola colonna, i grafici saranno disegnati
    ! utilizzando la stessa colonna XX(:,1) per tutte le colonne
    ! della matrice YY.
    !
    ! La funzione salva in ogni caso i dati relativi ai grafici
    ! per un successivo eventuale uso.
    ! Il parametro opzionale t permette di impostare la pausa
    ! tra un fotogramma e il successivo.
    ! Il parametro save_flag permette di salvare un video ottenuto
    ! dall'unione dei vari grafici in formato .avi.
    ! Ogni fotogramma viene salvato in formato .png prima di essere
    ! assemblato con l'aiuto del programma 'ffmpeg'.
    !
    ! NB: 'ffmpeg' deve essere installato per salvare il video.

  implicit none

  real(kind=8),     intent(in), dimension(:,:) :: XX ! puo' essere (:,1)
  real(kind=8),     intent(in), dimension(:,:) :: YY
  character(len=*), intent(in), optional :: fn
  real,             intent(in), optional :: t
  logical,          intent(in), optional :: save_flag

  character(len=64) :: fn_
  real              :: t_

  character (len = 100) :: command_file_name = 'video/plot_function_commands.txt'
  character (len = 100) :: data_file_name = 'video/plot_function_data.txt'

  integer :: ierror, n_active_columns, n

  ! Se trova una colonna di 0 sulle YY, non plotta da quella colonna in poi
  ! LQ

  n_active_columns = SIZE(YY, 2)

  do n = 1,  SIZE(YY, 2)

    if (COUNT(YY(:, n) == 0) == SIZE(YY,1)) then
       n_active_columns = n - 1
       exit
    end if

  end do
!  RA
!  do n_active_columns = 0,  SIZE(YY, 2)
!    if (COUNT(YY(:, n_active_columns + 1) == 0) == SIZE(YY,1))  exit
!  end do


  if (PRESENT(fn)) THEN
    fn_ = fn
  else
    fn_ = "__ddd"   ! nome file di default
  end if

  call write_xy_data_video (size(YY,1), n_active_columns, XX, YY, ierror, fn_)

  if (ierror /= 0) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_video'
    write ( *, '(a,i6)' ) '  WRITE_XY_DATA_VIDEO returned IERROR = ', ierror
  end if

  if (PRESENT(t)) THEN
    if (t < 0  .OR.  t > 10) THEN
      write (*, *) '[gnufor.f90] WARNING: pause time must be in the 0-10 interval'
      t_ = 0.1
    else
      t_= t
    end if
  else
    t_ = 0.1   ! tempo di default tra i fotogrammi
  end if

  call write_xy_plot_video (XX, YY, command_file_name, data_file_name,  &
                            ierror, n_active_columns, fn_, t_, save_flag)

  if (ierror /= 0) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'plot_video'
    write ( *, '(a,i6)' ) '  WRITE_XY_PLOT_VIDEO returned IERROR = ', ierror
  end if

  call run_gnuplot ( command_file_name )

  if (PRESENT(save_flag) .AND. save_flag) then

    write(*, *)  'ffmpeg -i video/'// fn_ // '%05d.png' // fn_ // '.avi'
    call system ('ffmpeg -i video/'// TRIM(fn_) // '%05d.png ' // TRIM(fn_) // '.avi')

  end if

end subroutine plot_video

!*******************************************************************************
!
subroutine get_unit ( iunit )
!
!*******************************************************************************
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5 and 6).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
  implicit none
!
  integer i
  integer ios
  integer iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

end subroutine get_unit

function pi ( )
!
!*******************************************************************************
!
!! PI returns the value of pi.
!
!
!  Modified:
!
!    04 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real PI, the value of pi.
!
  implicit none
!
  real(kind=8) :: pi
!
  pi = 3.14159265358979323846264338327950288419716939937510E+00

end function pi

subroutine run_gnuplot ( command_file_name )

! LQ AG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! NECESSARY ONLY FOR NAGWARE FORTRAN
!
! Use added for system call using function  SYSTEM
!                                           ======
 !!!!!!!!!!!!!!!1 USE f90_unix_proc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! LQ AG

!
!*******************************************************************************
!
!! RUN_GNUPLOT runs GNUPLOT with a given command file.
!
!
!  Discussion:
!
!    The GNUPLOT program, version 3.7, must be available.  To check whether
!    this is so, try typing
!
!      which gnuplot
!
!    If the response is
!
!      gnuplot: command not found
!
!    then you're going to have to make GNUPLOT available.
!
!    At ISU, this may require that you issue the command
!
!      add gnu
!
!    You may need to set the environment variable GNUTERM:
!
!      setenv GNUTERM x11
!
!    so that GNUPLOT automatically displays to your X window terminal.
!
!
!    This routine expects that there is a text file containing the appropriate
!    commands to GNUPLOT to display your picture.  There are a number of
!    routines in this package that will do this for simple plotting tasks.
!    Most of them require that you also set up a file of data to be plotted.
!
!    Once this routine invokes GNUPLOT, a graphics window should open
!    up, and the FORTRAN program will pause.  Hitting RETURN should advance
!    to the next picture, or terminate the window at the end, allowing the
!    FORTRAN routine to proceed.
!
!
!    You can look at the data and command files created by the routines.
!    Moreover, you can easily modify the command file to change the options
!    used in GNUPLOT, and then run GNUPLOT interactively, as in:
!
!      gnuplot commands
!
!    In particular, if you want a PostScript version of your graphics files,
!    insert the command "set term postscript" at the beginning of the command
!    file and run gnuplot as follows:
!
!      gnuplot commands > mypicture.ps
!
!    You will also have to hit RETURN once for each plot that is made.
!
!  Modified:
!
!    21 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
  implicit none
!
  character ( len = 100 ) command
  character ( len = * ) command_file_name

! LQ AG  integer status
! LQ AG  integer system

!
!  Issue a command to the system that will startup GNUPLOT, using
!  the file we just wrote as input.
!
  write ( command, * ) 'gnuplot -persist ' // trim ( command_file_name )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'Issuing the command:' // trim ( command )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Press RETURN to proceed.'

! LQ AG  status = system ( trim ( command ) )

  !call system ( trim ( command ) )
  call EXECUTE_COMMAND_LINE ( trim ( command ) )

! LQ AG
!
!  if ( status == 0 ) then
!    write ( *, '(a)' ) ' '
!    write ( *, '(a)' ) 'RUN_GNUPLOT:'
!    write ( *, '(a)' ) '  Normal end of execution of the GNUPLOT program.'
!  else
!    write ( *, '(a)' ) ' '
!    write ( *, '(a)' ) 'RUN_GNUPLOT - Fatal error!'
!    write ( *, '(a)' ) '  An error code was returned when the GNUPLOT command'
!    write ( *, '(a)' ) '  was issued.  Perhaps GNUPLOT is not in your path.'
!    write ( *, '(a)' ) '  Type "which gnuplot" to check this.'
!    stop
!  end if
! LQ AG

end subroutine run_gnuplot

subroutine timestamp ( )
!
!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none
!
  character ( len = 8 ) ampm
  integer d
  character ( len = 8 ) date
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  character ( len = 10 )  time
  integer values(8)
  integer y
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

end subroutine timestamp

subroutine write_vector_data ( data_file_name, n, x, y, dx, dy, ierror )
!
!*******************************************************************************
!
!! WRITE_VECTOR_DATA writes vector data to a file, for plotting by GNUPLOT.
!
!
!  Discussion:
!
!    Each vector is described by 4 values, X, Y, dX, dY, indicating that
!    a vector is to be drawn from (X,Y) to (X+dX,Y+dY).
!
!  Modified:
!
!    22 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer N, the number of vectors.
!
!    Input, real X(N), Y(N), DX(N), DY(N), the vector data.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  integer n
!
  character ( len = * ) data_file_name
  real(kind=8) :: dx(n)
  real(kind=8) :: dy(n)
  integer file_unit
  integer i
  integer ierror
  integer ios
  real(kind=8) :: x(n)
  real(kind=8) :: y(n)
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_VECTOR_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = data_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_VECTOR_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  do i = 1, n
    write ( file_unit, * ) x(i), y(i), dx(i), dy(i)
  end do

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_VECTOR_DATA:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT vector data file "' // &
    trim ( data_file_name ) // '"'

end subroutine write_vector_data

subroutine write_vector_plot ( command_file_name, data_file_name, &
  ierror )
!
!*******************************************************************************
!
!! WRITE_VECTOR_PLOT writes GNUPLOT commands to plot vectors.
!
!
!  Modified:
!
!    22 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
  integer ierror
  integer ios
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_VECTOR_PLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_VECTOR_PLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  write ( file_unit, '(a)' ) 'set data style vector'
  write ( file_unit, '(a,i2,a)' ) 'plot "' // trim ( data_file_name )
  write ( file_unit, '(a)' ) 'pause -1'
  write ( file_unit, '(a)' ) 'q'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_VECTOR_PLOT:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT table plots command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_vector_plot

subroutine write_xy_data ( data_file_name, n, x, y, ierror )
!
!*******************************************************************************
!
!! WRITE_XY_DATA writes X(1:N), Y(1:N) data to a file.
!
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer N, the number of data items.
!
!    Input, real X(N), Y(N), the X and Y data
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  integer n
!
  character ( len = * ) data_file_name
  integer file_unit
  integer i
  integer ierror
  integer ios
  real(kind=8) :: x(n)
  real(kind=8) :: y(n)
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = data_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  do i = 1, n
    write ( file_unit, * ) x(i), y(i)
  end do

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XY_DATA:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XY data file "' // &
    trim ( data_file_name ) // '"'

end subroutine write_xy_data


subroutine write_xy_dataa ( data_file_name, n, x, y1, y2, ierror )
!
!*******************************************************************************
!
!! WRITE_XY_DATA writes X(1:N), Y(1:N) data to a file.
!
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer N, the number of data items.
!
!    Input, real X(N), Y(N), the X and Y data
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  integer n
!
  character ( len = * ) data_file_name
  integer file_unit
  integer i
  integer ierror
  integer ios
  real(kind=8) :: x(n)
  real(kind=8) :: y1(n), y2(n)
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_DATAA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = data_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_DATAA - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  do i = 1, n
    write ( file_unit, * ) x(i), y1(i), y2(i)
  end do

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XY_DATAA:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XY data file "' // &
    trim ( data_file_name ) // '"'

end subroutine write_xy_dataa


! RA, JB

subroutine write_xy_data_video ( nr, nc,  x, y, ierror , data_file_name_base)

  implicit none

  integer, intent(in) :: nr, nc
  real(kind=8), dimension(:, :), intent(in) :: x
  real(kind=8), dimension(:, :), intent(in) :: y
  integer, intent(out) :: ierror

  character ( len = * ), intent(in) :: data_file_name_base

  character ( len = 128 ) data_file_name_current
  character ( len = * ), parameter :: base_dir = "video/"
  integer file_unit
  integer i, j, k
  integer ios

  ierror = 0

  write(*,*) "Creating directory for frames storage: "//base_dir
  call system ( "mkdir -p "//base_dir )

  call get_unit ( file_unit )


  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  if(SIZE(X, 2) == SIZE(Y, 2)) then

  ! many X versus many Y

    do j = 1, nc

      WRITE(data_file_name_current, '(a, a, i5.5)') TRIM(base_dir), TRIM(data_file_name_base), j

      open ( unit = file_unit, file = data_file_name_current, status = 'replace', &
          iostat = ios )
      if ( ios /= 0 ) then
        ierror = 2
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WRITE_XY_DATA - Fatal error!'
        write ( *, '(a)' ) '  Could not open the output file.'
        return
      end if

! LQ
      do i = 1, nr
        k = ((i-1)/size(x(:,j))) * size(x(:,j))    ! stile anti Auteri
        write ( file_unit, * ) x(i-k, 1), y(i, j)
        ! write ( file_unit, * ) x(i-((i-1)/size(x(:,j))) * size(x(:,j)), 1), y(i, j)
      end do


      close ( unit = file_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WRITE_XY_DATA_VIDEO:'
      write ( *, '(a)' ) ' writing "' // trim ( data_file_name_current ) // '"'

    end do

  else if (size(X, 2) == 1) then

    ! print many Y versus one X only

    do j = 1, nc

      WRITE(data_file_name_current, '(a, a, i5.5)') TRIM(base_dir), TRIM(data_file_name_base), j

      open ( unit = file_unit, file = data_file_name_current, status = 'replace', &
        iostat = ios )
      if ( ios /= 0 ) then
        ierror = 2
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WRITE_XY_DATA_VIDEO - Fatal error!'
        write ( *, '(a)' ) '  Could not open the output file.'
        return
      end if

! LQ
      do i = 1, nr
        k = ((i-1)/size(x(:,1))) * size(x(:,1))    ! stile anti Auteri
        write ( file_unit, * ) x(i-k, 1), y(i, j)
      end do

      close ( unit = file_unit )
!
!     write ( *, '(a)' ) ' '
!     write ( *, '(a)' ) 'WRITE_XY_DATA_VIDEO:'
!     write ( *, '(a)' ) ' writing "' // trim ( data_file_name_current ) // '"'
!
   end do

  else

    ierror = 3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_DATA_VIDEO - Fatal error!'
    write ( *, '(a)' ) '  dimension mismatch'
    return

  end if

end subroutine write_xy_data_video

subroutine write_xy_plott ( command_file_name, data_file_name, ierror )
!
!*******************************************************************************
!
!! WRITE_XY_PLOT writes GNUPLOT commands to plot X(1:N), Y(1:N) data.
!
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
! LQ  integer i
  integer ierror
  integer ios
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_PLOTT - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_PLOTT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  write ( file_unit, '(a, i2, a)' ) 'plot "' // trim ( data_file_name ) // &
    '" using 1:2 with lines lc 1 , "'// trim ( data_file_name ) // &
    '" using 1:3 with lines lc 2'
  !write ( file_unit, '(a)' ) 'pause(-1)'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XY_PLOTT:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XY plot command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_xy_plott



subroutine write_xy_plot ( command_file_name, data_file_name, ierror )
!
!*******************************************************************************
!
!! WRITE_XY_PLOT writes GNUPLOT commands to plot X(1:N), Y(1:N) data.
!
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
! LQ  integer i
  integer ierror
  integer ios
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_PLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_PLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  write ( file_unit, '(a, i2, a)' ) 'plot "' // trim ( data_file_name ) // &
    '" using 1:2 with lines lc 1'
  write ( file_unit, '(a)' ) 'pause mouse'
  write ( file_unit, '(a)' ) 'q'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XY_PLOT:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XY plot command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_xy_plot

! RA, JB

subroutine write_xy_plot_video (x, y, command_file_name, data_file_name, ierror, nc, fn, t, save_flag)

  implicit none

  real(kind=8), dimension(:, :) :: x, y
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer ierror

  character ( len = * ) fn

  integer file_unit
  integer ios
  integer nc
  integer j

  real a1, b1, a2, b2, c1, c2

  integer int_inf, int_sup, o
  logical, optional :: save_flag

  real t

  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_PLOT_VIDEO - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XY_PLOT_VIDEO - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'cd "video/"'
  write ( file_unit, '(a)' ) 'set key off'
  write ( file_unit, '(a)' ) 'set title "' // trim(fn) // '"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'

  if(size(x, 2) == 1) then
    a1 = minval(x(:, 1)) * (1 - sign(0.05d0, minval(x(:, 1))))
    b1 = maxval(x(:, 1)) * (1 + sign(0.05d0, maxval(x(:, 1))))
    write ( file_unit, '(a, f10.2, a, f10.2,a)' ) 'set xrange [', a1, ':', b1 , ']'
  else
    a2 = minval(x(:, 1:nc)) * (1 - sign(0.05d0, minval(x(:, 1:nc))))
    b2 = maxval(x(:, 1:nc)) * (1 + sign(0.05d0, maxval(x(:, 1:nc))))
    write ( file_unit, '(a, f10.2, a, f10.2,a)' ) 'set xrange [', a2,':', b2,']'
  end if

  c1 = minval(y(:, 1:nc)) * (1 - sign(0.05d0,minval(y(:, 1:nc))))
  c2 = maxval(y(:, 1:nc)) * (1 + sign(0.05d0, maxval(y(:, 1:nc))))

  write ( file_unit, '(a, f10.2, a, f10.2,a)' ) 'set yrange [', c1,':',c2,']'


  if (PRESENT(save_flag)) THEN
    if (save_flag) THEN
      write ( file_unit, '(a)' ) 'set terminal png size 1024,768' ! 'set term png'
    end if
  end if

  do j=1, nc

      write (data_file_name, '(a, i5.5)') TRIM(fn), j


      if(PRESENT(save_flag) .AND. save_flag) THEN
        write ( file_unit, '(a)' ) 'set output ''' // trim ( data_file_name ) // '.png'''
      end if

      if (size(x, 1) /= size(y, 1)) then

        write ( file_unit, '(a)' ) 'set multiplot '

        do o = 1, size(y, 1)/size(x, 1)

          int_inf = (o - 1)*size(x, 1)
          int_sup = o*size(x, 1) - 1
          !int_sup = int_inf + 1
          write ( file_unit, '(a, i4, a, i4, a, i1, a)' ) 'plot "' // trim ( data_file_name ) // &
               '" every ::',int_inf, '::',int_sup,' using 1:2 with lines lc ',(o+1),' lw 2'

	end do

        if (PRESENT(save_flag) .AND. save_flag) THEN
          write ( file_unit, '(a)' ) 'unset multiplot '
        end if

      else
        write ( file_unit, '(a, i2, a)' ) 'plot "' // trim ( data_file_name ) // &
        '" using 1:2 with lines lc 1'
      end if

      if(.NOT. PRESENT(save_flag) .OR. (.NOT. save_flag)) THEN
        write ( file_unit, '(a, F4.2)' ) 'pause ', t
      end if

  end do

  if(.NOT.PRESENT(save_flag).OR. (.NOT. save_flag)) THEN
    write ( file_unit, '(a)' ) 'pause -1'
  end if

  close ( unit = file_unit )

end subroutine write_xy_plot_video

subroutine write_xyy_data ( data_file_name, lda, nrow, ncol, x, ierror )
!
!*******************************************************************************
!
!! WRITE_XYY_DATA writes a table of data to a file, for plotting by GNUPLOT.
!
!
!  Discussion:
!
!    The first column of data is assumed to be the independent variable, X.
!    Separate plots are made of X versus all the other columns of data.
!
!  Modified:
!
!    21 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer LDA, the leading dimension of X.
!
!    Input, integer NROW, NCOL, the dimensions of X.
!
!    Input, real X(LDA,NCOL), the NROW by NCOL data to be written.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  integer lda
  integer ncol
!
  character ( len = * ) data_file_name
  integer file_unit
  integer i
  integer ierror
  integer ios
  integer nrow
  real(kind=8) :: x(lda,ncol)
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYY_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = data_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYY_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  do i = 1, nrow
    write ( file_unit, * ) x(i,1:ncol)
  end do

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XYY_DATA:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XYY data file "' // &
    trim ( data_file_name ) // '"'

end subroutine write_xyy_data

subroutine write_xyy_plots ( command_file_name, data_file_name, &
  ncol, ierror )
!
!*******************************************************************************
!
!! WRITE_XYY_PLOTS writes GNUPLOT commands to make multiple (X,Y) plots.
!
!
!  Discussion:
!
!    The first column of data is assumed to be the independent variable, X.
!    Separate plots are made of X versus all the other columns of data.
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer NCOL, the number of columns of data.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
  integer i
  integer ierror
  integer ios
  integer ncol
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYY_PLOTS - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYY_PLOTS - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  do i = 2, ncol
    write ( file_unit, '(a,i2,a)' ) 'plot "' // trim ( data_file_name ) // &
      '" using ', i, ' with lines 1'
    write ( file_unit, '(a)' ) 'pause -1'
  end do
  write ( file_unit, '(a)' ) 'q'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XYY_PLOTS:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XYY plots command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_xyy_plots

subroutine write_xyz_data ( data_file_name, n, x, y, z, ierror )
!
!*******************************************************************************
!
!! WRITE_XYZ_DATA writes X(1:N), Y(1:N), Z(1:N) data to a file.
!
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer N, the number of data items.
!
!    Input, real X(N), Y(N), Z(N), the X, Y, Z data
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  integer n
!
  character ( len = * ) data_file_name
  integer file_unit
  integer i
  integer ierror
  integer ios
  real(kind=8) :: x(n)
  real(kind=8) :: y(n)
  real(kind=8) :: z(n)
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZ_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = data_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZ_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  do i = 1, n
    write ( file_unit, * ) x(i), y(i), z(i)
  end do

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XYZ_DATA:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XYZ data file "' // &
    trim ( data_file_name ) // '"'

end subroutine write_xyz_data

subroutine write_xyz_plot ( command_file_name, data_file_name, ierror )
!
!*******************************************************************************
!
!! WRITE_XYZ_PLOT writes commands to plot parametric (X,Y,Z) data.
!
!
!  Discussion:
!
!    This routine tries to write a command file suitable for displaying
!    a 3D arc specified by points (X,Y,Z).  A grid data file, containing
!    values of X, Y and Z, will also be needed.
!
!  Modified:
!
!    22 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
! LQ  integer i
  integer ierror
  integer ios
! LQ  integer ncol
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZ_PLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZ_PLOT - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  write ( file_unit, '(a)' ) 'set parametric'
  write ( file_unit, '(a)' ) 'splot "' // trim ( data_file_name ) // &
      '" using 1:2:3 with lines 1'
  write ( file_unit, '(a)' ) 'pause -1'
  write ( file_unit, '(a)' ) 'q'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XYZ_PLOT:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT SPLOT command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_xyz_plot

subroutine write_xyzgrid_contour ( command_file_name, data_file_name, ierror )
!
!*******************************************************************************
!
!! WRITE_XYZGRID_CONTOUR writes commands to plot contours of Z(X,Y).
!
!
!  Discussion:
!
!    This routine tries to write a command file suitable for displaying
!    contours of Z(X,Y) gridded data.  A grid data file, containing values
!    of X, Y and Z, will also be needed.
!
!  Modified:
!
!    22 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
! LQ  integer i
  integer ierror
  integer ios
! LQ   integer ncol
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZGRID_CONTOUR - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZGRID_CONTOUR - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  write ( file_unit, '(a)' ) 'set parametric'
  write ( file_unit, '(a)' ) 'set nosurface'
  write ( file_unit, '(a)' ) 'set contour'
  write ( file_unit, '(a)' ) 'set cntrparam levels 10'
  write ( file_unit, '(a)' ) 'set term table'
  write ( file_unit, '(a)' ) 'set out "table.txt"'
  write ( file_unit, '(a)' ) 'splot "' // trim ( data_file_name ) // &
    '" using 1:2:3 with lines'
  write ( file_unit, '(a)' ) 'set term x11'
  write ( file_unit, '(a)' ) 'plot "table.txt" with lines'
  write ( file_unit, '(a)' ) 'pause -1'
  write ( file_unit, '(a)' ) 'q'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XYZGRID_CONTOUR:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XYZGRID contour plot command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_xyzgrid_contour

subroutine write_xyzgrid_data ( data_file_name, nx, ny, xyz, ierror )
!
!*******************************************************************************
!
!! WRITE_XYZGRID_DATA writes a file of XYZ grid data.
!
!
!  Discussion:
!
!    It is assumed that values of Z are available on a regular NX by NY grid
!    of (X,Y) points.
!
!    The form of the data file requires that all the data for a given value
!    of Y be listed, followed by a blank line, followed by the data for
!    another value of Y.
!
!  Example:
!
!    Here is a grid data file for a 3 by 3 grid, with Z = X + Y.
!
!    0.0 0.0 0.0
!    1.0 0.0 1.0
!    2.0 0.0 2.0
!
!    0.0 1.0 1.0
!    1.0 1.0 2.0
!    2.0 1.0 3.0
!
!    0.0 2.0 2.0
!    1.0 2.0 3.0
!    2.0 2.0 4.0
!
!  Modified:
!
!    23 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Input, integer NX, NY, the dimensions of the grid.
!
!    Input, real XYZ(3,NX,NY), the XYZ grid data to be written.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  integer nx
  integer ny
!
  character ( len = * ) data_file_name
  integer file_unit
  integer i
  integer ierror
  integer ios
  integer j
  real(kind=8) :: xyz(3,nx,ny)
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZGRID_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = data_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZGRID_DATA - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  do j = 1, ny
    do i = 1, nx
      write ( file_unit, * ) xyz(1:3,i,j)
    end do
    write ( file_unit, '(a)' )
  end do

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_XYZGRID_DATA:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT XYZ grid data file "' // &
    trim ( data_file_name ) // '"'

end subroutine write_xyzgrid_data

subroutine write_xyzgrid_surface ( command_file_name, data_file_name, ierror )
!
!*******************************************************************************
!
!! WRITE_XYZGRID_SURFACE writes a file of GNUPLOT commands to plot a 3D surface.
!
!
!  Discussion:
!
!    This routine tries to write a command file suitable for displaying
!    a surface Z(X,Y).  A grid data file, containing values of X, Y and Z,
!    will also be needed.  The routine WRITE_XYZGRID_DATA can write this file.
!
!  Modified:
!
!    22 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) COMMAND_FILE_NAME, the name of the
!    command file.
!
!    Input, character ( len = * ) DATA_FILE_NAME, the name of the data file.
!
!    Output, integer IERROR, nonzero if an error occurred.
!
  implicit none
!
  character ( len = * ) command_file_name
  character ( len = * ) data_file_name
  integer file_unit
! LQ  integer i
  integer ierror
  integer ios
! LQ  integer ncol
!
!  Write the data file.
!
  ierror = 0

  call get_unit ( file_unit )

  if ( file_unit == 0 ) then
    ierror = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZGRID_SURFACE - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    return
  end if

  open ( unit = file_unit, file = command_file_name, status = 'replace', &
    iostat = ios )

  if ( ios /= 0 ) then
    ierror = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WRITE_XYZGRID_SURFACE - Fatal error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    return
  end if

  write ( file_unit, '(a)' ) 'set title "GNUFOR plot"'
  write ( file_unit, '(a)' ) 'set xlabel "x"'
  write ( file_unit, '(a)' ) 'set ylabel "y"'
  write ( file_unit, '(a)' ) 'set parametric'
  write ( file_unit, '(a)' ) 'set hidden3d'
  write ( file_unit, '(a)' ) 'set contour'
  write ( file_unit, '(a)' ) 'splot "' // trim ( data_file_name ) // &
    '" using 1:2:3 with lines'
  write ( file_unit, '(a)' ) 'pause -1'
  write ( file_unit, '(a)' ) 'q'

  close ( unit = file_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WRITE_SURFACE_COMMANDS:'
  write ( *, '(a)' ) '  Wrote the GNUPLOT surface plot command file "' // &
    trim ( command_file_name ) // '"'

end subroutine write_xyzgrid_surface

! LQ AG

END MODULE gnufor

! LQ AG
