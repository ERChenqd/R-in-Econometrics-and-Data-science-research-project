#Getting help with certain command
?read.table
help(read.table)

#How to import dataset，R studio can only read the following formats of data.
LungCap <-read.csv(file.choose(), header=T) # import data in csv format
LungCapTXT <-read.delim(file.choose(), header=T) # import data in TXT
LungCapCSV  <- read.table(file.choose(), header=T, sep=",") #sep="," for comma seperate data, mainly use this function to import
LungCapTXT <- read.table(file.choose(), header=T, sep="\t")#for tab delimited data/for both CSV and TXT data
#file.choose() specifies the path of document,
#header=T tells the first row is the data variable
# I should use Excel to convert the data document format

#Remove Dataset from workspace
rm(LungCapCSV) 

#View Data
dim(LungCapCSV) #to see the dimensions of data set
head(LungCapCSV) #show the variables of data set and show the first 6 rows of each variable
tail(LungCapCSV) #show the variables and the last 6 rows of data set
View(LungCapCSV) # direct to data display page
LungCapCSV[1,4] # show the data in the first row and the 4th column
LungCapCSV[5:9,] #show all the data from row 5 to row 9 in each column
LungCapCSV[5:9,1] #show the data from row 5 to row 9 in the first column variable
LungCapCSV[-(5:720),] #show all the data not from row 5 to row 720 in each column variable

mean(Age) #returns error as Age is not stored in Rstudio
#1 Solution
mean(LungCapCSV$Age)
#2 Solution
attach(LungCapCSV)
mean(Age)
summary(Height) #or just like solution 1, you can simply type summary(LungCapCSV$Height)

class(Age) #to check the type of data for variable "Age", an integer variable
class(Height) #numeric variable
class(Gender) # it is a character variable
#To successfully apply the command summary(variable) on a categorical variable 
#described by 0's and 1's (e.g. Gender where 0 represents male and 1 represents female), 
#first you must run the command variable <- as.factor(variable)

Gender <-as.factor(Gender)#convert Gender,a character variable or other numeric variable to a factor variable so that levels()can be used
levels(Gender)#check the number and type of categories in categorical variable Gender
summary(LungCapCSV)
summary(Gender) #display general summary statistics for variable Gender

detach(LungCapCSV) # end the attached data set environment