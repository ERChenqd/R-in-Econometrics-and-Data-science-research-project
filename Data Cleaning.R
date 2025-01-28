View(LungCapMissing)

install.packages("tidyverse")
library(tidyverse)
# this topic works to clean the missing values in the data set
#Pipe Operator (Shift+Control+M) %>% 
LungCapMissing %>% 
  filter(Height>50 & Age<20) %>% 
  mutate(Height_in_meters = Height/100) %>% #create new variable
  select(Height_in_meters, Age) %>% #select the 2 variables to create a new data set.
  arrange(Age) %>% #arrange data based on Age, giving you in ascending order
  View()

#Data Cleaning 

#1. Select Variables
LungCapMissing %>% 
  select(LungCap, Height, Age)
LungCapMissing %>% 
  select(ends_with("e"))
#2. Changing Variable Order
LungCapMissing %>% 
  select(Height, LungCap, Age, everything()) %>% 
  View()
#3. Changing the variable name
LungCapMissing %>% 
  rename("SMOKE"="Smoke") %>% 
  head()
#4. Filter rows
LungCapMissing %>% 
  filter(Age>18 & Gender=="male")
#5. Recode data
LungCapMissing %>% 
  mutate(Gender=recode(Gender, 
                       "male"="man",
                       "female"="woman"
                       ))
#6. Dealing with missing data
missing <- !complete.cases(LungCapMissing)
LungCapMissing[missing,]
mean(LungCapMissing$Age)
mean(LungCapMissing$Age, na.rm=T)
#7. Dealing with duplicate data
name <- c("Anna", "Ben", "Cindy", "Anna")
age <- c(22, 33, 44, 22)
data <- data.frame(name, age)
View(data)
data<-distinct(data)
#8. Conditional Change
LungCapMissing %>% 
  mutate(tallness=
           if_else(Height>65, 
                   "tall", "short"))
LungCapMissing %>% 
  mutate(tallness=
           if_else(Height>65, 
                   "tall", if_else(Height>55, "medium", "short")))