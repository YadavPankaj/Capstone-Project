# loading file for data wrangling
# setup workstation directory

#list all files present on the working directory
list.files()
library(dplyr)
# loading csv file into the data frame
mainDB <- read.csv('refine_original.csv',header = TRUE,stringsAsFactors = FALSE)
dim(mainDB)

#creating function to clean up brand names
head(mainDB)
tail(mainDB)
names(mainDB)
mainDB$company

companyName <- function(s){
  ## this function gets a company name (s) and retuns the correct company name, one of these: philips, akzo, van houten and unilever
  s <- tolower(s)
  n <- nchar(s)
  first_2 <- substr(s,1,2)
  last_2 <- substr(s,n-1,n)
  
  if(last_2 == 'ps'){
    return('phillips')
  }else if(first_2 == 'ak'){
    return('akzo')
  }else if(first_2 == 'va'){
    return('van houten')
  }else{
    return('unilever')
  }
}

companyName(mainDB$company[7])

#apply a function over a list or vector
mainDB$company <- sapply(mainDB$company,FUN=companyName)
mainDB$companys
View(mainDB)

mainDB$company <- as.factor(mainDB$company)

# Separate product code and number
mainDB$productCodes <- strsplit(mainDB$Product.code...number,split = '-')
mainDB$product_code <- sapply(mainDB$productCodes,FUN=function(x)x[1])
mainDB$produc_number <- sapply(mainDB$productCodes, FUN=function(x)x[2])


# add product categories.

productCategory <- function(x){
  if (x=="p") { return('Smartphone')}
  else if (x=='v') {return('TV')}
  else if(x=='x'){return('Laptop')}
  else{return('Tablet')}
}

mainDB$produc_category <- sapply(mainDB$product_code,FUN =productCategory )

# dropping a column from a data frame
mainDB$produc_number <- NULL


# add full address for geocoding
mainDB$full_address <- paste(mainDB$address,mainDB$city,mainDB$country,sep = ',')


#create dummy variables for company and product category
# for phillips
companyBinaryPhilips <- function (cp){
if(cp=='phillips'){return(1)}
else return(0)
}

mainDB$company_philips <- sapply(mainDB$company,FUN=companyBinaryPhilips)

# for Akzo
companyBinaryAkzo <- function (cp){
  if(cp=='akzo'){return(1)}
  else return(0)
}

mainDB$company_akzo <- sapply(mainDB$company,FUN=companyBinaryAkzo)


# for van houten
companyBinaryhouten <- function (cp){
  if(cp=='van houten'){return(1)}
  else return(0)
}

mainDB$company_van_houten <- sapply(mainDB$company,FUN=companyBinaryhouten)

# for Unilever
companyBinaryUnilever <- function (cp){
  if(cp=='unilever'){return(1)}
  else return(0)
}

mainDB$company_unilever <- sapply(mainDB$company,FUN=companyBinaryUnilever)

# for smartphone
BinarySmartPhone <- function (cp){
  if(cp=='Smartphone'){return(1)}
  else return(0)
}

mainDB$product_smartphone <- sapply(mainDB$produc_category,FUN=BinarySmartPhone)

# for TV
BinaryTV <- function (cp){
  if(cp=='TV'){return(1)}
  else return(0)
}
mainDB$product_tv <- sapply(mainDB$produc_category,FUN=BinaryTV)

# for laptop
BinaryLaptop <- function (cp){
  if(cp=='Laptop'){return(1)}
  else return(0)
}
mainDB$product_laptop <- sapply(mainDB$produc_category,FUN=BinaryLaptop)

# for tablet
Binarytablet <- function (cp){
  if(cp=='Tablet'){return(1)}
  else return(0)
}
mainDB$product_tablet <- sapply(mainDB$produc_category,FUN=Binarytablet)

## storing the output in an excel file as csv
#Below codes are not working
setwd('C:/Users/yadavps/Desktop/Data Science Project/Data Wrangling')
write.table(mainDB,file = 'C:/Users/yadavps/Desktop/Data Science Project/Data Wrangling/refine_clean.csv',sep = '\t')
write.table(mainDB,file = 'refine_clean.csv',col.names = TRUE)
write.csv(mainDB,file='refine_clean.csv')

write.table(mainDB,file = 'refine_output.csv',sep = '\t',row.names = FALSE,col.names = TRUE)

# the output csv file is created because the lists are converted to characters
testFile <- data.frame(lapply(mainDB, as.character),stringsAsFactors = FALSE)
write.csv(testFile,file = 'refine_clean.csv')
