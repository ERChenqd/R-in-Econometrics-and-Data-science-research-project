x=11

x1=c(1,3,5,7,9) #a vector "x1" containing numerical values is created

gender=c("male","female") #a vector called "gender" containing 2 characters is created

x=2:7 # the colon ":" could create an integer sequence running from 2 to 7 in that case of 2:7

seq(from=1, to=7, by=1) #it creates a sequence running from 1 to 7 in increments of 1
seq(from=1, to=7, by=1/3) #it creates a sequence running from 1 to 7 in increments of 1/3 etc
seq(from=5,to=10, by=0.75)

rep(1,times=10) #repeat 1 for 10 times
rep("erwen",times=x)#repeat 'erwen' for x times
rep(1:5,times=x)#repeat integer sequences running from 1 to 5 for x times.
rep(seq(from=3, to=5,by=0.314),times=5) # repeat a sequence for 5 times
rep(c("m","f"), times=x) # repeat a vector containing "m" and "f" for x times

x=3:6
x
y=C(1,3,5,7,9,11,13)
y
x+10 #the various calculations in vector
x*10
x/10
exp(x)
log(x)#log(x,base=y)
sqrt(x)
abs(x)
y^0.5

# if the two vectors are in the same length, they can add, subtract, multiple, divide etc the arithmetic operations with their corresponding elements in R studio
z=1:5
z+x1
z-x1
z*x1
z/x1

A=seq(from=2, to=10,by=2)
B=seq(from=12,to=20,by=2)
A[2] #this function shows the 2nd element of sequence A, which is "4"
B[5] #this function shows the 5th element of sequence B, which is "5"
A[-2] # this function shows all the elements of sequence A except the 2nd element
B[-5] # this function shows all the elements of sequence B except the 5th element
A[1:3] # show the first 3 elements of sequence A
B[2:4] # show the part of sequence B from the 2nd to the 4th element
A[c(1,5)] #show the 1st and the 5th element of sequence A
B[c(2,3)] #show the 2nd and the 3rd element of sequence B
A[-c(1,5)] #show all elements of sequence A except the 1st and the 5th element
B[-c(2,3)] # show all elements of sequence B except the 2nd and the 3rd element
A[A<6] #show all elements of sequence A that are smaller than 6
B[B>=16] #show all elements of sequence B that are bigger than or equal to 16

matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,byrow=TRUE) #it would create a 3X3 matrix, 'nrow=3' tells R that we would have 3 rows in matrix
#'byrow=TRUE' tells R that the values should be entered in a row-wise order(数值是横着写)
matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,byrow=FALSE) # 'byrow=FALSE' tells R that the values should be entered in a column-wise order(数值是竖着写)
matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=5,byrow=TRUE) #'nrow=5' makes there 5 rows and 2 columns in matrix
matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=2,byrow=TRUE) #'nrow=2' makes there 2 rows and 5 columns in matrix

# For matrix in R studio, one element wihin has coordinate (A,B), A is the row coordinate (说明他在哪一个横行，在左边标注，从上往下数), B is the column coordinate （说明他在哪一个竖列，从左往右数）

ERC=matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=2,byrow=TRUE)
ERC[1,2] #show the element with coordinate (1,2) of a matrix called ERC
ERC[c(1,2),5] #show the elements with row coordinate is '1' or '2' in condition of column coordinate '5'
ERC[2,c(1,5)] #show the elements with row coordinate '2' in condition of column coordinate is '1' or '5'
ERC[c(1,2),c(1,3,5)] #show the elements with row coordinate '1' or '2' in condition of column coordinate is '1' or '3' or '5'
ERC[2,] #Show all elements have row coordinate '2'
ERC[,5] #show all elements have column coordinate '5'
ERC*10 #multiply the all elements of "ERC" matrix by 10
