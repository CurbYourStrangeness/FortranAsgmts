
program grading
implicit none
integer :: units = 16
integer :: uniti = 20
integer :: stuNum
character(12) :: fname0


TYPE struct1
   integer :: stNum
   character(31) :: name
   character(23) :: locat
   character(7) :: phone
   character(1) :: gender
   character(1) :: classLevel
   character(4) :: Major
   integer :: credits
   integer :: cmGPA
ENDTYPE struct1

TYPE(struct1), dimension(56) :: records 


print *, "Enter the filename to be read"
read *, fname0


call fopen(units, fname0)
call populateType(units, records)
call sortRec(records)

print *, "Please enter a student number"
read *, stuNum

if(stuNum /=  -1) then
call searchStu(records, stuNum)
endif

call honorWrite(uniti, units, fname0)
!************************************************************************
contains

subroutine fopen(units, fname0)
  integer, intent(in) :: units
  character(12), intent(in) :: fname0
  integer :: test
  
  open(unit=units, file=fname0, status="old", action="read", form = "formatted", iostat=test)
  if(test /= 0) STOP "Error opening"


end subroutine fopen
!************************************************************************
subroutine populateType(units, records)
  integer, intent(in) :: units
  TYPE(struct1), dimension(56), intent(out) :: records
  integer :: test, loop   
  
 do loop = 1, 56, 1
    read(units, 15, iostat=test)records(loop)
    if(test > 0) stop "input error"
    if (test < 0) then
       print *, "reached end of file"
       exit
    endif    
 enddo
close(units, status="keep")  
15 format(I5,A31,A23,A7,A1,A1,A4,I3,I3)     

end subroutine populateType
!************************************************************************

subroutine sortRec(records)
  TYPE(struct1), dimension(56), intent(inout) :: records
  TYPE(struct1) :: temp
  Integer :: i, minIndex, hold, sizea, loop
  sizea = SIZE(records)

do i = 1, sizea-1
  minIndex = MINLOC(records(i:)%stNum, 1) + i -1
 if (records(i)%stNum > records(minIndex)%stNum) then
     temp = records(i)
     records(i) = records(minIndex)
     records(minIndex) = temp
  endif
enddo

  do loop = 1, 56, 1
  print 20, records(loop)   
enddo

20 format(I5,x,A31,x,A23,x,A7,x,A1,x,A1,x,A4,I3,x,I3)
end subroutine sortRec

!************************************************************************
subroutine searchStu(records, stuNum)
integer, intent(out) :: stuNum
integer :: names, loop, i
TYPE(struct1), dimension(56), intent(in) :: records

print *, "Searching for student record"
do while(stuNum .NE. -1)

do loop = 1, 56, 1
if (stuNum .EQ. records(loop)%stNum ) then
   print *, "Found It"
   print '(I5, A)', records(loop)%stNum, ","
   print '(A31, A)', records(loop)%name, ","
   print '(I3, A)', records(loop)%credits, ","
   print '(I3, A)', records(loop)%cmGPA, ","
   print '(A4)', records(loop)%Major
   i = 1
endif
enddo
if (i /= 1) then
print *, "You have entered a number that does not match any of the numbers on the list."
endif
i = 0
print *, "Please enter a new stuNum"
read *, stuNum
enddo

end subroutine searchStu
!************************************************************************
subroutine honorWrite(uniti, units, fname0)
  integer, intent(in) :: uniti, units
  character(12), intent(in) :: fname0
 character(16) :: hfName
 integer :: loop, test
 TYPE(struct1), dimension(56) :: record

 open(unit=units, file=fname0, status="old", action="read", iostat=test)
 if(test /= 0) stop "error opening"
 
 do loop = 1,56,1
    read(units, 20, iostat = test)record(loop)
    if (test > 0) then
       print *, test
       STOP "Reding Error"
       endif
 enddo
 
 print *, "Please enter the name of the honor file"
 read *, hfName

 open(unit=uniti, file=hfName, status="replace", action="write")

 loop = 0
 test = 0
 
 do loop = 1, 56, 1
    if (record(loop)%cmGPA .GE. 350) then
    write(uniti, 20, iostat=test)record(loop)
 endif
 enddo

 20 format(I5,A31,A23,A7,A1,A1,A4,I3,I3)
 close(units, status="keep")
 close(uniti, status="keep")
end subroutine honorWrite
!*************************************************************************
end program grading
