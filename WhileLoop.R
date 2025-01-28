#While Loops: run a block of codes continuously if a certain condition is met
#we have to make sure the condition would be achieved at some point otherwise the CPU will be ruined.

# Define a string and a numeric vector
string <- "Hello, World!"
numbers <- 1:5
# Use cat to print them with a custom separator
cat(string, numbers, sep = " - ") 
#Hence, the cat function works to print contents with a custom(n. 传统，习惯) separator

val <- 0
while(val < =30) {
  
  cat("The value of the VAL variable is:", val)
  
  print("Our value is still less than 30, we are going to add 1 to it")
  
  if(val==5){
    
    print("The value has been found at 5")
    
    break #the program would stop running at val=5. "break" command terminates the program
    #The break command breaks out of the nearest loop  
    
  }
  
  val <- val+1
  
}

#To produce a list of integers from 0 to 5 using the "while" loop
x=0
while(x<6){
  print(x)
  x=x+1
}