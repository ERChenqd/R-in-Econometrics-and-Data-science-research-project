sprintf("My student ID is: %i", 5527700)#to print a string and value of a variable, 5527700 can be replaced by any object such as x, y, x.1, etc

X=6 #input X
x=11 #input x
print(x)#or x both work to output x,"X" and "x" are different, be capital-sensitive
2*x # would output 22
y<--7 #the other way to input, e.g y
y<-3*5# it would output "15", and if you reassign a new value to y, the old value will be erased
ls() #display all the objects/variables in the Environment
rm()#could remove the object you don't want from Environment, in that case I used rm(X) to remove object "X"

#For naming objects,you can name "x.1" but you can't do "1.x", the number cannot be in front of the letter
#We can assigned values to the object in terms of texts with quotes, for example,

ERC="Erwen Chen"
ERC="陈尔文"
yy="3.1415926"
# The code x=2 assgins a numerical value to x, while the code x="2" assigns a character value to x

11+14 #the output would directly show the result of the calculation, which is 25

7*9

x

y

x+y

z=x+y

z*y

x*y

x/y

x-y

y^2

y^2+x^2

sqrt(y)# or y^(0.5)

log(y)#log y of base 10

exp(y)# you can get the exponent of y etc

log2(2)#we can also calculate the log of other bases

#if you want to calculate the log of other bases such as 3,4,5, you have to compute like this:
log(x,base=y)

#calculate the absolute value of x
abs(x)

#sqrt(y)# in the first line of the command, if we do not finish the first line and press "enter",the next line would appear "+"
# as long as we finish the command ending up ')', the "+" would disappear.

# the text on the right of the '#' would be recognized as note, which will not read as command by R.
"sqrt(x)"=sqrt(x)# assign the square root of x to an object called "sqrt(x)"