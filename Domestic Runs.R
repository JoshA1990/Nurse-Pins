#                                  MULTIPLE RUNS #


###################################Install and load the packages###################################


#data wrangling/ analysis package
library(tidyverse)

#First set the working directory and read in base files
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")

nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("ORG Codes NHS Digital.csv")

#Set second working directory for list manipulation
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")


#########    MUST BE IN ORDER ALPHABETICALLY AND CHRONOLOGICALLY 
file_list <- list.files(pattern='*.csv')


#### Define/ call the function from a different R script

#### Make sure to change the location to whatever function you want to parse through.

source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Nurse-Pins/NQN age function.R")
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce Variables.R')
occ_codes()

####   Start a loop

#Define the starting point of the loop
i <- 1

#Define the ending point of the loop- may need to change depending on monthly or yearly
j <- length(file_list)


while (i < j) {
  
  #Assigns earliest two entries to Raw_Data to allow to parse
  Raw_Data_y1 <- read_csv(file_list[1])
  Raw_Data_y2 <- read_csv(file_list[2])
  #Call function
  #################   Change function depending on what you need
  nqn_age_function()
  
  #Reassign globally after deleting entry
  file_list <<- file_list[-1]
  i <- i + 1
}

