
#Import tidyverse in
library(tidyverse)


#First read in file to collapse into one column
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Joiners by Registration/")

raw_data <- read_csv("Complete.csv")

#Change all NA's to 0s

data <- raw_data %>% 
  mutate(Total = rowSums(select_if(., is.numeric), na.rm = TRUE))%>%
  select(c(1,2,ncol(raw_data)+1))

                      