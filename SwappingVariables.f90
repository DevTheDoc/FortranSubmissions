implicit none
real a,b
a= 2
b=3
print *, "a=",a,"b=",b
a= a+b
b= a-b
a= a-b
print *, "a=",a,"b=",b
end
