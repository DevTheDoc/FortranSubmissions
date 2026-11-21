implicit none

integer currentLoopCount
integer LoopCount
currentLoopCount=0
print *, "Count till??"
read (*,*)LoopCount
1 if(currentLoopCount<LoopCount)then
        print*, currentLoopCount+1, "using goto"
        currentLoopCount=currentLoopCount+1
        goto 1
 endif
 currentLoopCount=0
 do while(currentLoopCount<LoopCount)
 print *, currentLoopCount+1, "Using do while"
 currentLoopCount = currentLoopCount +1
 end do
 end
