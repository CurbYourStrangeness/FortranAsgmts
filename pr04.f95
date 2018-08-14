program Arry
  double precision, dimension(12) :: arrFill
  double precision :: high, low, total
  integer :: count
  
  call read_list(arrFill) 
  call output_data(arrFill, total, high, low, count)
  !call print_reverse(arrFill)

!********************************************************************
contains
  subroutine read_list(arrFill)
    double precision, dimension(12), intent(out) :: arrFill
    do loop = 1,12,1
       print '(A,I3)', 'Enter a number for element (The max is 9999):', loop
       read *, arrFill(loop)
    enddo
  end subroutine read_list

  function find_high(arrFill, high)
    double precision, dimension(12), intent(out) :: arrFill
    double precision, intent(out) :: high
    high = 0
    do loop = 12,1, -1
       if (high < arrFill(loop)) then
          high = arrFill(loop)
       endif
    enddo
    print '(A, 8F10.5)', "The high Value is: ", high
  end function find_high

  function find_low(arrFill, low)
    double precision, dimension(12), intent(out) :: arrFill
    double precision, intent(out) :: low
    low = 999999999999.9999999999999999
    do loop = 12,1,-1
       if (low > arrFill(loop)) then
          low = arrFill(loop)
       endif
    enddo
    print '(A, 8F10.5)', "The low value is: ", low
  end function find_low

  subroutine output_data(arrFill, total, high, low, count)
  double precision, dimension(12), intent(out) :: arrFill 
  double precision, intent (out) :: total, high, low
  integer, intent (out) :: count
  total = 0  

  !Loop through the array to find the total 
  do loop = 12,1,-1
     total = total + arrFill(loop)
     count = count + 1
  enddo
  print '(A, I3)', "The count is: ", count  
  print '(A, 8F10.3)', "The total is: ", total 

  high = find_high(arrFill, high)
  low = find_low(arrFill, low)
  
  call print_reverse(arrFill)
  print *, ' '
 end subroutine output_data

  subroutine print_reverse(arrFill)
   double precision, dimension(12), intent(out) :: arrFill 
   do loop = 12, 1, -1
     print '(8F10.3)', arrFill(loop)
   enddo
  end subroutine print_reverse
  
end program Arry
