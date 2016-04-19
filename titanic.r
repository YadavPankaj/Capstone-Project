setwd('C:/Users/yadavps/Desktop/Data Science Project/Data Wrangling')
list.files()
titanicMain <- read.csv('titanic_original.csv',header = TRUE)
str(titanicMain)
View(titanicMain)
table(titanicMain$embarked)
library(dplyr)

summary(titanicMain$embarked)
names(titanicMain)

summary(titanicMain$embarked)
table(titanicMain$embarked)

# assign blank Embarked column as "S"

titanicMain$embarked[titanicMain$embarked==""] <- "S"
summary(titanicMain$age)

# assign mean age to the fields where age is not mentioned
meanOfAge <- mean(!is.na(titanicMain$age))
titanicMain$age[is.na(titanicMain$age)] <- mean(!is.na(titanicMain$age))
table(titanicMain$sex)

table(titanicMain$boat)

# Filling missing values in boat column with NA
titanicMain$boat[titanicMain$boat==''] <- "NA"

titanicMain$has_cabin_number

str(titanicMain$cabin)

# new column named has_cabin_number to be created. Its field value should be 1 in case a column cabin contains number
titanicMain$cabin <-  as.character(titanicMain$cabin)
checking_cabin_number <- function(x){
  if(nchar(x)>0){return(1)}
  else {return(0)}
 
}

titanicMain$has_cabin_number <- sapply(titanicMain$cabin, FUN=checking_cabin_number)

# saving the cleaned up data into titanic_clean.csv file..
tempTitanicFile <- data.frame(lapply(titanicMain, as.character),stringsAsFactors = FALSE)
write.csv(tempTitanicFile,file='titanic_clean.csv')
