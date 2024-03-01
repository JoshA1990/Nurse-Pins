
# Attempt to get massive csv file with all ESR records since 2018

# Import packages

library(tidyverse)

#Set the working directory
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")

# Create a variable of the list of all files in the folder
file_list <- list.files(pattern = "*.csv")
file_list <- file_list[-1:-48]

# Create a blank dataframe to append onto.
df <- data.frame()

#If splitting up, remove first n entries from list to allow for smaller loops.
#Read in the base file which all others will be binded onto
df <- read_csv(file_list[1])


#Remove first item (base) from the file list.
file_list <- file_list[-1]
file_list <- file_list[-12:-60]


# Define a variable to equal 1 so can track if and where it goes wrong.

i <- 1

#Read in each file in folder and append to bottom. Have to do in 3 amounts as processing exceeds computing power.
for (file in file_list) {
  add <- read_csv(file)
  df <- df |> rbind(add)
  print(i)
  i <- i+1
  }

#Export
write_csv(df, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/Records 5.csv")
