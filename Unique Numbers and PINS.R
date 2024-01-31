#Import all 
library(tidyverse)

library(janitor)

library(lubridate)

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")

#Import and clean names at same time using janitor code
nov_pins <- clean_names(read_csv("Staff in Post 202311 - Workforce with PINS.csv"))

#Filter to get a dataframe only containing the registration number and Unique ID ready to join.
only_pins <- nov_pins %>% filter(is.na(registration_number) == FALSE)%>%
  #Enforce the length of registration number has to equal 8
  filter(str_length(registration_number) == 8)%>%
  #Filter only the registration numbers who begin with two numbers.
  filter(substr(registration_number, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
  filter(substr(registration_number, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
  #Filter only the registration numbers that have a legitimate country code
  filter(substr(registration_number, 8,8) %in% c("A", "C", "D", "E", "N", "O", "S", "W"))%>%
  #Filter only the registration numbers that have the correct month format
  filter(substr(registration_number, 3,3) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"))%>%
  #Select only the unique identifier and the reg number for easy joining.
  select(c(unique_nhs_identifier, registration_number))

#Make the unique nhs number columns match
Data <- Data %>% rename(unique_nhs_identifier = Unique_Nhs_Identifier)

#Join the Data dataframe only by unique NHS identifier.
Data_test <- merge(only_pins, Data, by = "unique_nhs_identifier", all.y = TRUE)%>%
  #Find only the reg numbers that are not NA
  filter(is.na(registration_number) == FALSE) %>%
  #Remove duplicates
  distinct(unique_nhs_identifier, .keep_all = TRUE)%>%
  #Recode the letter indicating country
  dplyr::mutate(home_country = str_sub(registration_number,-1)) %>% 
  dplyr::mutate(home_country = recode(home_country, 
                                           A = "UK", 
                                           C = "EU",
                                           D = "UK",
                                           E = "England",
                                           N = "Northern Ireland",
                                           O = "Overseas (non EU)",
                                           S = "Scotland",
                                           W = "Wales")
  )%>%
  #Recode the month and year 
  dplyr::mutate(registration_year = str_sub(registration_number, 1,2))%>%
  dplyr::mutate(registration_month = str_sub(registration_number, 3,3))%>%
  dplyr::mutate(registration_month = recode(registration_month,
                                            A = 01,
                                            B = 02,
                                            C = 03,
                                            D = 04,
                                            E = 05,
                                            F = 06,
                                            G = 07,
                                            H = 08,
                                            I = 09,
                                            J = 10,
                                            K = 11,
                                            L = 12))%>%
  #Create a new column in the format ready to be converted to a datetime
  dplyr::mutate(registration_date = if_else(registration_year < 25, paste0(20,registration_year,"-",registration_month,"-01"),
                                            paste0(19,registration_year,"-",registration_month,"-01")))

#Convert both registration date and date of birth to datetime
Data_test$registration_date <- ymd(Data_test$registration_date)
Data_test$Date_Of_Birth_y2 <- ymd(Data_test$Date_Of_Birth_y2) 

#Filter out the registration numbers that are before the date of birth
Data_birth_filter <- Data_test %>% filter(Date_Of_Birth_y2 < registration_date)%>%
  mutate(joiner = ifelse(joiner > 0, 1, 0))%>%
  group_by(registration_date, joiner, occ_joiner)%>%
  summarise(total = sum(NHSD_trust_or_CCG_y2))
  


