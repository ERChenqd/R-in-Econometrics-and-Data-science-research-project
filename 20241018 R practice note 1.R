rm(list=ls()) # deletes objects in the memory *add quotes
getwd() # show the director you are working
#question 2
setwd() #set the working directory you wish to work.
load() #load the data set
#question 3
View() #view the data set

str(qlfs) #show the framework of the data "qlfs"

summary(qlfs) # summary the data statistic of "qlfs" data set

summary(qlfs$hourpay) #summary the variable 'hourpay' of the data set,
#Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    NA's 
# 0.03    8.72   12.40   14.88   18.02  972.22   16659 
#NA's: missing value

mean(qlfs$hourpay, na.rm=TRUE) #calculate the mean of the variable in the data set, na.rm removes the missing values
sd(qlfs$hourpay, na.rm=TRUE) # the standard deviation
var(qlfs$hourpay, na.rm=TRUE) #variance
min(qlfs$hourpay, na.rm=TRUE) #minimum value
max(qlfs$hourpay, na.rm=TRUE) #maximum value

install.packages("psych") #this package gives you some research calculations
library(psych)

describe(qlfs$hourpay)

hist(qlfs$hourpay,freq=FALSE,breaks=seq(from=0,to=1000,by=2),xlab = "$",main="Sample histogram")
#freq= FALSE gives the vertical axis the 'density' , TRUE gives 'frequency'
#xlab shows the x-axis title


#how to remove outliers in one column data
qlfs$hpay=qlfs$hourpay
plp99=quantile(qlfs$hpay,probs=c(0.01,0.99),na.rm=TRUE) #remove missing values in the dataset in the variable.
#the function above calculates the two values of the variable 'qlfs$hpay' at the 1st percentile and at the 99th percentile
plp99
plp99[1] #the first value in plp99, the first percentile value in hourpay column
plp99[2] #the 99th percentile value, the 99th percentile value in hourpay column

trim=qlfs$hpay<=plp99[1]|qlfs$hpay>=plp99[2]
#Check if the values of hpay are less than or equal to the first element of plp99 which is the 1st percentile value
#and if the values of hpay are greater than or equal to the second element of plp99 which is the 99th percentile value
#'|'is the logical operator of "or" that combines the two conditions above.
#the result vector trim will be TRUE when either condition is met, and FALSE if neither conditions met.

trim[is.na(trim)]<-FALSE #against missing values, assign FALSE to missing values in the trim vector
#is.na(trim): Identifies the positions in the trim vector that have NA values.
#trim[is.na(trim)]: Subsets the trim vector to select only the elements that are missing values.
#<- FALSE: Assigns the value FALSE to those positions, ensuring no missing values in the vector
#remove the missing values without removing the observations

qlfs[trim,"hpay"]<-NA #against outliers, assign NA to true values in the trim vector
#qlfs[trim, ]specifies the rows where meet the conditions of 'trim',"hpay" specifies the operation targets to the column data
#and assign NA values to the selected rows in the 'hpay' column
#trimming outliers by setting them to missing value.

summary(qlfs$hpay)
hist(qlfs$hpay,freq=FALSE,breaks=seq(from=0,to=50,by=2),xlab="$",main="sample histogram(2)")
