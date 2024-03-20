#                                  MULTIPLE RUNS #


###################################Install and load the packages###################################


#data wrangling/ analysis package
library(tidyverse)
library(openxlsx)

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

source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Nurse-Pins/Full Joiners.R")
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce Variables.R')
nurse_codes()

####   Start a loop

#Define the starting point of the loop
i <- 1

#Define the ending point of the loop- may need to change depending on monthly or yearly
j <- length(file_list)

#Create a blank dataframe to add all the entries to.
full_pin_joiners <- data.frame()


while (i < j) {
  
  #Assigns earliest two entries to Raw_Data to allow to parse
  Raw_Data_y1 <- read_csv(file_list[1])
  Raw_Data_y2 <- read_csv(file_list[2])
  #Call function
  #################   Change function depending on what you need
  full_joiners_function()
  
  full_pin_joiners <- bind_rows(full_pin_joiners, appended_nurses)
  #Reassign globally after deleting entry
  file_list <<- file_list[-1]
  i <- i + 1
}

write.csv(full_pin_joiners, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Full joiners/Full Joiners.csv"))

# Start wrangling and writing data ---------------------------------------------
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Full joiners")


#Read file in

full_pin_joiners <- read_csv("Full Joiners.csv")

full_pin_joiners$current_month <- as.Date(full_pin_joiners$current_month)

# Get a unique list of direct joiners
full_pin_direct_joiners <- full_pin_joiners |>
  filter(joiner == 1)|>
  distinct(Unique_Nhs_Identifier, .keep_all = TRUE)

write.csv(full_pin_direct_joiners, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Full joiners/Unique Direct Joiners.csv"))

# Get a unique list of occ joiners
full_pin_occ_joiners <- full_pin_joiners |>
  filter(occ_joiner == 1)|>
  distinct(Unique_Nhs_Identifier, .keep_all = TRUE)

write.csv(full_pin_occ_joiners, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Full joiners/Unique Occ Joiners.csv"))

#Get a unique list of joiners who have been registered since 2018
registration_2018_joiners <- full_pin_joiners |>
  filter(registration_date >= as.Date("2018-01-01"))|>
  distinct(Unique_Nhs_Identifier, .keep_all = TRUE)


# Create excel workbook --------------------------------------------------------
#Create Workbook

wb <- createWorkbook()

# Set a sheet number to tick up
sheet_number = 1

# Create function to add dataframes to new worksheet

add_dataframe_to_wb <- function(sheet_name, dataframe) {
  addWorksheet(wb, sheet_name)
  writeDataTable(wb, sheet = sheet_number, dataframe)
  saveWorkbook(wb, "Full Joiners.xlsx", overwrite = TRUE)
  sheet_number <<- sheet_number + 1
}



# Create summaries for grouping ------------------------------------------------

# Create summaries for Unique direct joiners

direct_joiners_split <- full_pin_direct_joiners |>
  group_by(current_month, country_of_training)|>
  summarise(joiner = sum(joiner))

# Recode date column to character to allow to be joined with totals
direct_joiners_split$current_month <- as.character(direct_joiners_split$current_month)

# Create total dataframe and appends summary onto
direct_joiner_total <- full_pin_direct_joiners |>
  summarise(joiner = sum(joiner) + sum(occ_joiner))|>
  mutate(current_month = "All")|>
  mutate(country_of_training = "All")|>
  select(2,3,1)|>
  bind_rows(direct_joiners_split)

# Add dataframe to new sheet in workbook
add_dataframe_to_wb("Direct Summary", direct_joiner_total)


# Create summaries for Occ direct joiners --------------------------------------

occ_joiners_split <- full_pin_occ_joiners |>
  group_by(current_month, country_of_training)|>
  summarise(joiner = sum(joiner))

# Recode date column to character to allow to be joined with totals
occ_joiners_split$current_month <- as.character(occ_joiners_split$current_month)

# Create total dataframe and appends summary onto
occ_joiner_total <- full_pin_occ_joiners |>
  summarise(joiner = sum(joiner) + sum(occ_joiner))|>
  mutate(current_month = "All")|>
  mutate(country_of_training = "All")|>
  select(2,3,1)|>
  bind_rows(occ_joiners_split)

# Add dataframe to new sheet in workbook
add_dataframe_to_wb("Occ Summary", occ_joiner_total)


# Create summaries for joiners post 2018 ---------------------------------------

# Joiners by registration date

reg_date_split <- registration_2018_joiners |>
  group_by(registration_date)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))

# Recode date column to character to allow to be joined with totals
reg_date_split$registration_date <- as.character(reg_date_split$registration_date)

# Create total dataframe and appends summary onto
reg_date_total <- registration_2018_joiners |>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))|>
  mutate(registration_date = "All")|>
  select(3,1,2)|>
  bind_rows(reg_date_split)

# Add dataframe to new sheet in workbook
add_dataframe_to_wb("Reg Date Summary", reg_date_total)


# Joiners by country of training -----------------------------------------------

country_training_split <- registration_2018_joiners |>
  group_by(current_month, country_of_training)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))

# Recode date column to character to allow to be joined with totals
country_training_split$current_month <- as.character(country_training_split$current_month)

# Create total dataframe and appends summary onto
country_training_total <- registration_2018_joiners |>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))|>
  mutate(country_of_training = "All")|>
  mutate(current_month = "All")|>
  select(4,3,1,2)|>
  bind_rows(country_training_split)

add_dataframe_to_wb("Training Country Dates", country_training_total)

#Save workbook------------------------------------------------------------------

saveWorkbook(wb, "Full Joiners.xlsx", overwrite = TRUE)

# Age at time of joining  ------------------------------------------------------


reg_age_split <- registration_2018_joiners |>
  group_by(Age_In_Years_y2)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))

# Recode date column to character to allow to be joined with totals
reg_age_split$Age_In_Years_y2 <- as.character(reg_age_split$Age_In_Years_y2)

# Create total dataframe and appends summary onto
reg_age_total <- registration_2018_joiners |>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))|>
  mutate(Age_In_Years_y2 = "All")|>
  select(3,1,2)|>
  bind_rows(reg_age_split)

add_dataframe_to_wb("Reg Age", reg_age_total)


# Transition time by age -------------------------------------------------------

trans_age_split <- registration_2018_joiners |>
  group_by(Age_In_Years_y2, NMC_to_ESR)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))

# Recode date column to character to allow to be joined with totals
trans_age_split$Age_In_Years_y2 <- as.character(trans_age_split$Age_In_Years_y2)

# Create total dataframe and appends summary onto
trans_age_total <- registration_2018_joiners |>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))|>
  mutate(Age_In_Years_y2 = "All")|>
  mutate(NMC_to_ESR = "All")|>
  select(3,4,1,2)|>
  bind_rows(trans_age_split)

date_order <- c(
  "Immediate Joiner",
  "1 Month",
  "1 to 2 Months",
  "2 to 3 Months",
  "3 to 4 Months",
  "4 to 5 Months",
  "5 to 6 Months",
  "6 to 7 Months",
  "7 to 8 Months",
  "8 to 9 Months",
  "9 to 10 Months",
  "10 to 11 Months",
  "11 to 12 Months",
  "12 to 18 Months",
  "18 to 24 Months",
  "2 to 5 years",
  "5 to 10 years",
  "Over 10 years",
  "All"
)

age_order = as.character(c(20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,
                         39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,
                         58,59,60,61,62,63,64,65,66,67,68,69,70))

trans_age_total <- trans_age_total |>
  mutate(NMC_to_ESR = factor(NMC_to_ESR, levels = date_order))|>
  mutate(Age_In_Years_y2 = factor(Age_In_Years_y2, levels = age_order))|>
  arrange(Age_In_Years_y2, NMC_to_ESR)

add_dataframe_to_wb("Transition Age", trans_age_total)

#Save workbook------------------------------------------------------------------

saveWorkbook(wb, "Full Joiners.xlsx", overwrite = TRUE)

# Those NQNs not joining at base of band 5 (if any)

band_5_joiners <- registration_2018_joiners |>
  filter(AfC_Band_y2 != "Band 5")

wrong_band_joiners <- band_5_joiners |>
  group_by(AfC_Band_y2 ,Afc_Spinal_Point_y2)|>
  summarise(joiner = sum(joiner) + sum(occ_joiner))

add_dataframe_to_wb("Non Band 5", wrong_band_joiners)


# Nationality of joiners -------------------------------------------------------

nationality_split <- registration_2018_joiners |>
  group_by(Nationality_y2)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))

# Create total dataframe and appends summary onto
nationality_total <- registration_2018_joiners |>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))|>
  mutate(Nationality_y2 = "All")|>
  select(3,1,2)|>
  bind_rows(nationality_split)

add_dataframe_to_wb("Nationality", nationality_total)

# Nationality of joiners and training country ----------------------------------

# First create a list where the country of training is simpilised
sorted_2018_joiners <- registration_2018_joiners |>
  mutate(country_of_training = recode(country_of_training,
                                      "England" = "UK",
                                      "Northern Ireland" = "UK",
                                      "Scotland" = "UK",
                                      "Wales"= "UK"))

#Summarise into split nationalities
training_nationality_split <- sorted_2018_joiners |>
  group_by(Nationality_y2, country_of_training)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))

#Summarise into total nationalities and bind
training_nationality_total <- sorted_2018_joiners|>
  group_by(country_of_training)|>
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))|>
  mutate(Nationality_y2 = "All")|>
  select(1,4,2,3)|>
  bind_rows(training_nationality_split)

#Create a factor ready for ordering
factor_training = c("UK",
                    "Overseas (non EU)",
                    "EU")

# Reorder the columns
training_nationality_total <- training_nationality_total|>
  mutate(country_of_training = factor(country_of_training, levels = factor_training))|>
  arrange(country_of_training)

add_dataframe_to_wb("Training by Nationality", training_nationality_total)

#Save workbook------------------------------------------------------------------

saveWorkbook(wb, "Full Joiners.xlsx", overwrite = TRUE)
