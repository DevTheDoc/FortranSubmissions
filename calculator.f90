implicit none
real firstNum, secondNum, operatorCode
integer NumOfNumbers
integer i,j,m,n
real temp
real, allocatable, dimension(:)::ArrayOfMaxMinNums
real, allocatable, dimension(:,:)::MatrixA, MatrixB, ResultantMatrix
character(len=50)::CharacterVariable
1 print *, "HI!!"
print *, "Enter the operation to perform, press 1=Addition, 2=Subtraction, 3=Multiplication, 4=Division, 5=Maxima and Minima,6 = matrix operations,  10=quit"
read(*,*) operatorCode
if(operatorCode/=10 .and. operatorCode>=1 .and. operatorCode<=4) then
        print *, "Enter the First Number"
        read(*,*) firstNum
        print *, "Enter the Second Number"
        read(*,*) secondNum
        if(operatorCode == 1) then
                print *,"Sum of ", firstNum," and ",secondNum," is: ",firstNum+secondNum
                goto 1
        elseif(operatorCode == 2) then
                print *,"Difference of ", firstNum," and ",secondNum," is: ",firstNum-secondNum
                goto 1
        elseif(operatorCode ==  3)  then
                print *,"Product of ", firstNum," and ",secondNum," is: ",firstNum*secondNum
                goto 1
        elseif(operatorCode == 4) then
                if(secondNum==0) then
                        print*, "You know we can't divide by Zero :("
                        goto 1
                else
                        print *,"Sum of ", firstNum," and ",secondNum," is: ",firstNum/secondNum
                        goto 1
                endif
        endif
elseif(operatorCode==5) then
        print *, "How many numbers do you have"

        read(*,*)NumOfNumbers
        allocate(ArrayOfMaxMinNums(NumOfNumbers))
        print *, "Type out your numbers"
        read(*,*)(ArrayOfMaxMinNums(i), i=1,NumOfNumbers)
        print*,(ArrayOfMaxMinNums(i), i=1, NumOfNumbers)
        !allocate(AscendingArrayOfMaxMinNums(NumOfNumbers))
        !print*, AscendingArrayOfMaxMinNums
        3 i=1
        j=1
        do i=1, NumOfNumbers-1
                if(ArrayOfMaxMinNums(i)>ArrayOfMaxMinNums(i+1)) then
                        ArrayOfMaxMinNums(i) = ArrayOfMaxMinNums(i)+ArrayOfMaxMinNums(i+1)
                        ArrayOfMaxMinNums(i+1) = ArrayOfMaxMinNums(i)-ArrayOfMaxMinNums(i+1)
                        ArrayOfMaxMinNums(i)= ArrayOfMaxMinNums(i)-ArrayOfMaxMinNums(i+1)
                        goto 3
                endif
        end do
        print *, "Your List in Ascending Order", ArrayOfMaxMinNums
        print *, "Minima= ", ArrayOfMaxMinNums(1), "Maxima= ", ArrayOfMaxMinNums(NumOfNumbers)
        deallocate(ArrayOfMaxMinNums)
        goto 1

else if(operatorCode==6)then
        print *, "Enter Dimensions of matrix A and Matrix B as 'i j'(can only operate matrices with same Dimensions)"
        read(*,*)i,j
        allocate(MatrixA(i,j))
        allocate(MatrixB(i,j))
        allocate(ResultantMatrix(i,j))
        print *, "Enter matrix A row-wise"
        do m=1,i
                read(*,*)(MatrixA(m,n),n=1,j)
        enddo
        print *, "Enter matrix B row-wise"
        do m=1,i
                read(*,*)(MatrixB(m,n),n=1,j)
        enddo
        print *, "Enter The matrix operation you would Like to perform (+,-,*,/)"
        read(*,*)CharacterVariable
        if(CharacterVariable=="+")ResultantMatrix = MatrixA + MatrixB
        if(CharacterVariable=="-")ResultantMatrix = MatrixA - MatrixB
        if(CharacterVariable=="*")ResultantMatrix = MatrixA * MatrixB
        if(CharacterVariable == "/")ResultantMatrix = MatrixA / MatrixB
        do m=1,i
                print*,(MatrixA(m,n),n=1,j)
        enddo
        print *, CharacterVariable
        do m=1,i
                print*,(MatrixB(m,n),n=1,j)
        enddo
        print *, "="
        do m=1,i
                print*,(ResultantMatrix(m,n),n=1,j)
        enddo
        deallocate(MatrixA)
        deallocate(MatrixB)
        deallocate(ResultantMatrix)
        goto 1




elseif(operatorCode==10) then
        goto 7
else
        print *, "EHH??"
        goto 1
endif
7 end
