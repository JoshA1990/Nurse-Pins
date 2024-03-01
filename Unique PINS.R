
# File to get full list of unique PINS since 2018

# First import library
library(tidyverse)

# Set working directory

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS")

# Read in all the files, ready to concatenate

data_part_1 <- read_csv("Records 1.csv")
data_part_2 <- read_csv("Records 2.csv")
data_part_3 <- read_csv("records 3.csv")

#Bind all files together
df <- bind_rows(data_part_1, data_part_2, data_part_3)

#Rename columns to make it easier to manipulate
colnames(df) <- str_replace_all(colnames(df), " ", "_")


# Filter all entries that do not have a registration number
df <- df |> filter(is.na(Registration_Number) == FALSE) |>
  #Filter those that only have the occ code for nursing
  filter(Occupation_Code %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                             "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                             "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                             "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                             "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                             "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                             "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A","N1A","NNN")) |>
  #Filter out the registration numbers that don't meet the required format
  filter(str_length(Registration_Number) == 8) |>
  #Filter only the registration numbers who begin with two numbers.
  filter(substr(Registration_Number, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0)) |>
  filter(substr(Registration_Number, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0)) |>
  #Filter only the registration numbers that have a legitimate country code
  filter(toupper(substr(Registration_Number, 8,8)) %in% c("A", "C", "D", "E", "N", "O", "S", "W")) |>
  #Filter only the registration numbers that have the correct month format
  filter(toupper(substr(Registration_Number, 3,3)) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "Y", "U")) |>
  #Recode the month and year 
  dplyr::mutate(registration_year = str_sub(Registration_Number, 1,2))|>
  dplyr::mutate(registration_month = str_sub(Registration_Number, 3,3))|>
  dplyr::mutate(registration_month = recode(registration_month,
                                            "A" = 01,
                                            "B" = 02,
                                            "C" = 03,
                                            "D" = 04,
                                            "E" = 05,
                                            "F" = 06,
                                            "G" = 07,
                                            "H" = 08,
                                            "I" = 09,
                                            "J" = 10,
                                            "K" = 11,
                                            "L" = 12
                                            )) |>
  
  #Create a new column in the format ready to be converted to a datetime
  dplyr::mutate(registration_date = if_else(registration_year < 25, paste0(20,registration_year,"-",registration_month,"-01"),
                                            paste0(19,registration_year,"-",registration_month,"-01")))
  
#Convert to a datetime
df$registration_date <- ymd(df$registration_date)

#Remove the unused columns now we have a year
df <- df |> select(c(1,2,3,6))

#Filter out those that have a registration PIN date of before 2018
df_2018 <- df |> filter(registration_date >= "2018-01-01") |>
  #Create a list of Unique NHS numbers
  distinct(Unique_Nhs_Identifier, registration_date, Registration_Number)



# Code in the way to extract country of training from letter at end of Registration number
df_summary <- df_2018 |> dplyr::mutate(country_of_training = str_sub(Registration_Number,-1)) |> 
  dplyr::mutate(country_of_training = recode(toupper(country_of_training), 
                                             A = "UK", 
                                             C = "EU",
                                             D = "UK",
                                             E = "England",
                                             N = "Northern Ireland",
                                             O = "Overseas (non EU)",
                                             S = "Scotland",
                                             W = "Wales"
  )) |>
  group_by(country_of_training) |>
  #Will summarise totals
  summarise(count = n_distinct(Unique_Nhs_Identifier))

#Summarise by time and country of training
df_summary_time <- df_2018 |> dplyr::mutate(country_of_training = str_sub(Registration_Number,-1)) |> 
  dplyr::mutate(country_of_training = recode(toupper(country_of_training), 
                                             A = "UK", 
                                             C = "EU",
                                             D = "UK",
                                             E = "England",
                                             N = "Northern Ireland",
                                             O = "Overseas (non EU)",
                                             S = "Scotland",
                                             W = "Wales"
  )) |>
  group_by(registration_date, country_of_training) |>
  summarise(count = n_distinct(Unique_Nhs_Identifier))

#Export 
write_csv(df_summary, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Total joiners since 2018.csv")

#Export
write_csv(df_summary_time, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Total joiners by time.csv")


# Tests ------------------------------------------------------------------------

df_tests <- bind_rows(data_part_1, data_part_2, data_part_3)

colnames(df_tests) <- str_replace_all(colnames(df_tests), " ", "_")

df_suffix <- df_tests |> filter(is.na(Registration_Number) == FALSE) |>
  #Filter those that only have the occ code for nursing
  filter(Occupation_Code %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A","N1A","NNN")) |>
  #Filter out the registration numbers that don't meet the required format
  filter(str_length(Registration_Number) == 8) |>
  #Filter only the registration numbers who begin with two numbers.
  filter(substr(Registration_Number, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0)) |>
  filter(substr(Registration_Number, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0)) |>
  #Filter only the registration numbers that have the correct month format
  filter(substr(Registration_Number, 3,3) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")) |>
  mutate(country_of_training = str_sub(Registration_Number, -1))|>
  group_by(country_of_training) |>
  summarise(count = n_distinct(Unique_Nhs_Identifier))


