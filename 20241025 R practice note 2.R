# continue from 20241018 seminar 1
#to save graph
png("sample histogram(2)",width=720,height=720) #open PNG device yo save
dev.off() #close the device, finalizing the save image

qlfs$lhpay=log(qlfs$hpay) #we might need to use log to include very big numbers
summary(qlfs$lhpay)
hist(qlfs$lhpay,freq=FALSE,breaks=seq(from=1,to=4,by=0.1),xlab="log $",main="sample histogram(3)")

#
categories=unique(qlfs$marsta) #give the unique values in the column data, which are categories of data inside
levels(qlfs$marsta)
levels(categories) #these two levels functions would give the same output
summary(qlfs$marsta)

#single
summary(qlfs[qlfs$marsta==categories[1],"hpay"]) #i.e. the hourly payment for people who are single
#this filters the qlfs data frame to rows where the 'hpay' column matches the first element of the categories vector.
# the first element in 'categories' vector is 'Single, never married' 

#married
summary(qlfs[qlfs$marsta==categories[2],"hapy"])
#this filters the qlfs data frame to rows where the 'hpay' column matches the 2nd element of the categories vector.
#the 2nd element in 'categories' vector is 'Married,living with spouse' 


keep_vec=qlfs$marsta==categories[1]|qlfs$marsta==categories[2]
#qlfs$marsta==categories[2] and qlfs$marsta==categories[1] would return logical values TRUE or FALSE according to the equality set up.
qlfs1=qlfs[keep_vec,]
# This would create a new data frame qlfs containing only the rows where marsta matches either of the two specified categories
#lfs[keep_vec, ] returns all columns for the rows where keep_vec is TRUE. The comma is necessary to separate the row and column indexing, 
#indicating that you're subsetting by rows without restricting the columns. Without the comma, R would interpret it as extracting a column, not a row subset.
#a standard form of subsetting data frame can be indexed: [row_index,column_index]

qlfs$married=factor(qlfs$marsta==categories[2],levels=c(FALSE,TRUE),labels = c("Single","Married"))
#This converts the logical vector to a factor with specified levels and labels:
#When one of the 'qlfs$marsta' is not equal to categories[2], the correspondence in 'qlfs$married' will be labeled as "Single" (corresponding to FALSE), 
#and when it matches categories[2], it will be labeled as "Married" (corresponding to TRUE).

install.packages("ggplot2")
library(ggplot2)
ggplot(data=qlfs1,mapping = aes(x=lhpay))+
  geom_histogram(aes(y=after_stat(density)),fill="white",colour="black",binwidth=0.1)+
  facet_wrap(~marsta)
#data = qlfs: Specifies the data frame to use (qlfs).
#aes(x = lhpay): Maps the lhpay column to the x-axis.
#aes(y = after_stat(density)): Normalizes the histogram to show density instead of counts.
#fill = "white" and colour = "black": Sets the fill color to white and the border color to black.
#binwidth = 0.1: Sets the width of each bin in the histogram.
#facet_wrap(~marsta): Facets the histogram by the marsta variable, creating separate plots for each level of marsta.
#The ~marsta notation in facet_wrap() specifies that the plots should be faceted by the marsta variable.

race_categories=unique(qlfs$eth01)
qlfs$white=factor(qlfs$eth01==race_categories[1],levels=c(FALSE,TRUE),labels=c("Non-white","White"))
table(qlfs$white)
#'factor()' converts the results to a factor variable 

#what if missing values
qlfs$white = factor(ifelse(is.na(qlfs$eth), "Unknown", "Known")) #The 2nd part is where TRUE, the 3rd part is where FALSE
#pending for modified further
