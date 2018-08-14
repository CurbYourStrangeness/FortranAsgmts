program Fileio
  implicit none 
  integer :: units, n
  character(8) :: fname
  character(3) :: stat
  double precision, dimension(:), allocatable :: arrName
  
  call open_file(units, n, arrName, fname, stat)!return a variable with the open file

  
  !**************************************************************

contains

  subroutine open_file(units, n, arrName, fname, stat)
    
    character(8), intent(inout) :: fname
    character(3), intent(inout) :: stat
    integer, intent(out) :: units, n
    double precision, dimension(:), allocatable, intent(inout) :: arrName
    integer :: i, j
    double precision :: avg, total, a, b, c
    units = 99
    do i = 1,2,1
    print '(A)', "Please enter the name of a file to be read"
    read *, fname
    

    if (fname .EQ. "use1.dat") then
       open(unit = units, file = fname, status = "old", action='read')
       call read_file(units, arrName, n)
       call sort_list(arrName)

    else if (fname .EQ. "use2.dat") then
       
       print '(F12.8)', arrName
       print '(A, I3)', "The count is: ", n

       do j = 1, n, 1
       total = total + arrName(j)
       enddo

       print '(A, F12.8)', "The total is: ", total

       a = mean(total)

       b = stDev(arrName, n, avg)

       c =  median(arrName, n)
       
       call write_out(arrName, fname, n)
             
    else
      print '(A)', "You entered the wrong filename!"
    endif
    enddo
  end subroutine open_file

  subroutine read_file(units, arrName, n)
    integer, intent(inout) :: n
    integer, intent(in) :: units
  double precision, dimension(:), allocatable, intent(out) :: arrName
  
  read(units, *) n
  allocate(arrName(n))
  read(units, *) arrName
  write(*,*) arrName

  close(unit=units,status='keep')
  end subroutine read_file
  
  subroutine sort_list(arrName)
  implicit none
  double precision, dimension(:), intent(inout) :: arrName
  integer :: i, minIndex, temp
  
  do i = 1, SIZE(arrName)-1
     minIndex = MINLOC(arrName(i:), 1) + i - 1
     if (arrName(i) > arrName(minIndex)) then
        temp = arrName(i)
        arrName(i) = arrName(minIndex)
        arrName(minIndex) = temp
     endif
  enddo
  end subroutine sort_list

  double precision function mean(total)
    double precision, intent(in) :: total
    double precision :: avg
    avg = total / n
  print '(A, F12.8)', "The mean is: ", avg
  end function mean
  
  double precision function median(arrName, n)
  double precision, dimension(:), intent(in) :: arrName
  integer, intent(in) :: n
  integer :: store
  double precision :: storage
  store = n/2
  storage = arrName(store)
  print '(A, F8.5)', "The median is: ", storage
  end function median

  double precision function stDev(arrName, n, avg)
  double precision, dimension(:), intent(in) :: arrName
  integer, intent(in) :: n
  double precision, intent(in) :: avg
  double precision :: uses
   
   uses = Sum((arrName - avg) ** 2.0)/n
  print '(A, F15.7)', "The standard deviation is: ", uses
  end function stDev
  
  subroutine write_out(arrName, fname, n)
  double precision, dimension (:), intent(in) :: arrName
  character(8), intent(in) :: fname
  integer, intent(in) :: n
  integer :: i
  open(unit=17, FILE ="use2.dat", status="replace", action="write")
  WRITE(17,'(I3)')n
  WRITE(17, '(F12.8)')arrName
  close(unit=17, status="keep")
  end subroutine write_out
  
end program Fileio
