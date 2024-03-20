

#                                  PIN match for nationality RUNS


###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)

#data wrangling/ analysis package
library(tidyverse)
# Set working directory to get other files
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")

nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")


#Set the working directory
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")

#Looks through entire working directory and adds each csv to a list

#########    MUST BE IN ORDER ALPHABETICALLY AND CHRONOLOGICALLY 
file_list <- list.files(pattern='*.csv')


#### Define/ call the function from a different R script

#### Make sure to change the location to whatever function you want to parse through.

source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Nurse-Pins/Matching function.R")


####   Start a loop

#Define the starting point of the loop
i <- 1

#Define the ending point of the loop- may need to change depending on monthly or yearly
j <- length(file_list)


while (i <= j) {
  
  #Assigns earliest two entries to Raw_Data to allow to parse
  Raw_Data <- read_csv(file_list[1])
  
  #Call function
  #################   Change function depending on what you need
  matching_function()
  
  #Reassign globally after deleting entry
  file_list <<- file_list[-1]
  i <- i + 1
}

