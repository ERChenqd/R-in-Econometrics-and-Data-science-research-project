#For Loops: produce the output presented all at separate lines

#Looping(.v循环) through a vector
loopVec <- c(10,20,30,40,50,60)
# method 1
for (loopItem in loopVec) {
  print(loopItem)
} #print the items in the loopVec

#method 2, same output as the method 1
for (loopItem2 in 1:length(loopVec)) {
  print(loopVec[loopItem2])
}

#To create a for loop which produces every square root number in a vector, use the command: 
Vector <- c(2,4,6,8,10,12)
sVector<-sqrt(Vector)
for (i in 1:length(sVector)){
  print(sVector[i])
}

#Looping over a list
mylist <- list(20,40,60,80,100)
for (i in mylist) {
  print(i)
}

list <- list(9, 16, 25, 36, 49)
for (x in list) {
  print(sqrt(x))
}
# Using a for loop for a list is no different than using it for a vector

#Looping over a matrix
myMatrix <- matrix(1:50, nrow=5)
for (matNumber in myMatrix) {
  print(matNumber^2) #prints each squared element of MyMatrix on a separate line
}
#When looping over a matrix, if not specified, 
#the loop will iterate column by column 
#(go from top to bottom through the first column, then the second, and so on)

#Nested Loops
for (row in 1:nrow(myMatrix)) {
  for (col in 1:ncol(myMatrix)) {
    print(myMatrix[row, col])
  }
}# show the elements of myMatrix row by row

for (row in 1:nrow(myMatrix)) {
  for (col in 1:ncol(myMatrix)) {
    print(paste('Row is = ', row, 
                'and column = ', col, 
                'and the value is ', myMatrix[row, col]))
  }
} #same output as the codes above but it add some characters between each data for coherence


#[1] "Row  1 and column 1 have the value of  10"

#[1] "Row  1 and column 2 have the value of  14"

#[1] "Row  2 and column 1 have the value of  12"

#[1] "Row 2 and column  2 have the value of  16"
mat <- matrix(data = seq(10, 16, by=2), nrow = 2, ncol =2)
for (row in 1:nrow(mat)) {
  for (column in 1:ncol(mat)) {
    print(paste('Row', row, 'and column', column, 'have the value of ', mat[row, column]))
  }
}