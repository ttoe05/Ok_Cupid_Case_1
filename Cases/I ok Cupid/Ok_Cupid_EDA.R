#OK cupid case 1 assignment

library(dplyr)
library(stringr)
# Load the dataset address
getwd()
setwd("C:/Users/student/Documents/Terrill/Harvard DS/Data_Mining_Business/Harvard_DataMining_Business_Student/Cases/I Ok Cupid"
)
df_address <- read.csv('addr.csv')
#Count of null values per column
df_address %>% summarise(across(everything(), ~ sum(is.na(.))))
# Get the dimensions of the dataframe
dim(df_address)
# Check locations is a unique column
unique(df_address$location) %>% length()
# Return the indexes where the city is null
na_indxes_city <- which(is.na(df_address$city))
#iterate over the indexes and split the location into city and state to populate
# the city column

for (indx in na_indxes_city){
  loc_split <- strsplit(str_to_title(df_address$location[indx]), split=", ")
  df_address$city[indx] <- loc_split[[1]][1]
}

# Return the indexes where the State is null and populate the state column the same as above
na_indxes_st <- which(is.na(df_address$state))
#Adding to be pushed
for (indx in na_indxes_st){
  loc_split <- strsplit(str_to_title(df_address$location[indx]), split=", ")
  df_address$state[indx] <- loc_split[[1]][2]
}
# Check for unique number of counties
unique(df_address$county) %>% length()
# check for unique number of postalcodes
unique(df_address$postalCode) %>% length()
# Check for unique cities
unique(df_address$city) %>% length()
####################################################################################

df_latlon <- read.csv("Latlon.csv")


# Check if postal codes are similar in the data set for counties
d <- wrapr::build_frame(
  "x1", "x2", "x3" |
    1 , 4 , "A" |
    NA , 5 , "B" |
    3 , 6 , NA |
    3 , 6 , "B" )
plan <- design_missingness_treatment(d)
prepare(plan, d)
prepare(plan, data.frame(x1=NA, x2=NA, x3="E"))