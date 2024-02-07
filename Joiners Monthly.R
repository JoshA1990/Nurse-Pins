
# Summary of joiners by PINS

###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)

#data wrangling/ analysis package
library(tidyverse)


###################################Set working directory and load data###################################
#Note, you'll need to set the working directory to where the files are saved
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")

Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202310 with PINS.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202311 with PINS.csv")

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")
nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("ORG Codes NHS Digital.csv")

###################################Data wrangling###################################
#Amend column names so the dots are replaced with spaces
colnames(Raw_Data_y1) <- str_replace_all(colnames(Raw_Data_y1), " ", "_")
colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")

#join NHS organisation codes and nationality
Raw_Data_y1 <- full_join(Raw_Data_y1,NHS_orgs)
Raw_Data_y1 <- full_join(Raw_Data_y1,nationality)
Raw_Data_y2 <- full_join(Raw_Data_y2,NHS_orgs)
Raw_Data_y2 <- full_join(Raw_Data_y2,nationality)

#Adding sufixes to variables to separate variable names between datasets
colnames(Raw_Data_y1) <- paste(colnames(Raw_Data_y1), "y1", sep = "_")
colnames(Raw_Data_y2) <- paste(colnames(Raw_Data_y2), "y2", sep = "_")



#Rename unique identifier to remove the suffix so it's easier to merge datasets later on
Raw_Data_y1 <- rename(Raw_Data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
Raw_Data_y2 <- rename(Raw_Data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)

#create flags for active nurses, on both datasets, will be important depending on whether looking at joiners or leavers later
Raw_Data_y1 <- Raw_Data_y1 %>% 
  mutate(Staff_group_y1 = if_else(substr(Occupation_Code_y1, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                  if_else(Occupation_Code_y1 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                                                    "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                    "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                    "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                    "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                    "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                                                    "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A","N1A","NNN"), "Nurse", 
                                          if_else(Occupation_Code_y1 %in% c("N2C", "N2J", "N2L"),"x.midwife",
                                                  if_else(Occupation_Code_y1 %in% "N3H", "x.health_visitor",
                                                          if_else(Occupation_Code_y1 %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%
  mutate(Status_orig_y1 = Status_y1) %>%
  mutate(Status_y1 = if_else(Status_y1 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
  mutate(Asg_Type_Of_Contract_y1 = if_else(Asg_Type_Of_Contract_y1 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
  filter (Asg_Type_Of_Contract_y1=='Permanent/fixed term/locum') %>%
  mutate (Nationality_grouping_y1 =if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
  mutate (Nationality_grouping_y1_v2 = if_else(Nationality_grouping_y1 %in% c('ROW','EU'), 'IR',
                                               if_else(Nationality_grouping_y1 %in% c('UK','Unknown'),'Domestic','Other')))

Raw_Data_y2 <- Raw_Data_y2 %>%
  mutate(Staff_group_y2 = if_else(substr(Occupation_Code_y2, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                  if_else(Occupation_Code_y2 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                                                    "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                    "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                    "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                    "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                    "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                                                    "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A","N1A","NNN"), "Nurse", 
                                          if_else(Occupation_Code_y2 %in% c("N2C", "N2J", "N2L"),"x.midwife",
                                                  if_else(Occupation_Code_y2 %in% "N3H", "x.health_visitor",
                                                          if_else(Occupation_Code_y2 %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%  
  mutate(Status_orig_y2 = Status_y2) %>%
  mutate(Status_y2 = if_else(Status_y2 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
  mutate(Asg_Type_Of_Contract_y2 = if_else(Asg_Type_Of_Contract_y2 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
  filter (Asg_Type_Of_Contract_y2=='Permanent/fixed term/locum') %>%
  mutate (Nationality_grouping_y2 =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
  mutate (Nationality_grouping_y2_v2 = if_else(Nationality_grouping_y2 %in% c('ROW','EU'), 'IR',
                                               if_else(Nationality_grouping_y2 %in% c('UK','Unknown'),'Domestic','Other')))

#ordering data by fte and then staff group
Raw_Data_y1 <- Raw_Data_y1[order(Raw_Data_y1$Staff_group_y1,-Raw_Data_y1$Contracted_Wte_y1),]
Raw_Data_y2 <- Raw_Data_y2[order(Raw_Data_y2$Staff_group_y2,-Raw_Data_y2$Contracted_Wte_y2),]


###################   New code to try and get headcount over FTE    ########################

#Raw_Data_y1$Contracted_Wte_y1 <- ifelse(Raw_Data_y1$Contracted_Wte_y1 > 0, 1, Raw_Data_y1$Contracted_Wte_y1)
#Raw_Data_y2$Contracted_Wte_y2 <- ifelse(Raw_Data_y2$Contracted_Wte_y2 > 0, 1, Raw_Data_y2$Contracted_Wte_y2)


#removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
Raw_Data_y1_dedup <- Raw_Data_y1[ !duplicated(Raw_Data_y1$Unique_Nhs_Identifier), ]
Raw_Data_y2_dedup <- Raw_Data_y2[ !duplicated(Raw_Data_y2$Unique_Nhs_Identifier), ]

#Join datasets
Data <- full_join(Raw_Data_y1_dedup, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")

#get rid of unsused data
rm(Raw_Data_y1_dedup)

#merge nationality into a single field and override NA nationality with Unknowns and NA NHS providers with 0s
Data <- Data %>%
  mutate (Nationality = if_else(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)) %>%
  mutate (Nationality_grouping =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)) %>%
  mutate (Nationality_grouping = if_else(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)) %>%
  mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                            if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))) %>%
  mutate (NHSD_trust_or_CCG_y1 = if_else(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)) %>% 
  mutate (NHSD_trust_or_CCG_y2 = if_else(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0))


#joiner/ leaver flags
Data <- Data %>%
  #joiner flags
  mutate(joiner = if_else(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
  mutate(occ_joiner = if_else(Staff_group_y1 != "Nurse" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0))


#duplication check
Data$Unique_Nhs_Identifier[duplicated(Data$Unique_Nhs_Identifier)]

#Join datasets to overwrite Y1 nationality
Raw_Data_y1 <- left_join(Raw_Data_y1, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")

#get rid of unsused data
rm(Raw_Data_y2_dedup)

#overwrite Y1 nationality
Raw_Data_y1<- Raw_Data_y1 %>%
  mutate (Nationality_grouping_y1_v2_2=if_else(is.na(Nationality_grouping_y2_v2)==FALSE,Nationality_grouping_y2_v2,Nationality_grouping_y1_v2)) %>%
  mutate (Nationality_grouping_y1_v2=if_else(is.na(Nationality_grouping_y2)==FALSE,Nationality_grouping_y2,Nationality_grouping_y1))

#Create flags for Registration info

pin_data <- Data %>% filter(is.na(Registration_Number_y2) == FALSE)%>%
  #Enforce the length of registration number has to equal 8
  filter(str_length(Registration_Number_y2) == 8)%>%
  #Filter only the registration numbers who begin with two numbers.
  filter(substr(Registration_Number_y2, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
  filter(substr(Registration_Number_y2, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
  #Filter only the registration numbers that have a legitimate country code
  filter(substr(Registration_Number_y2, 8,8) %in% c("A", "C", "D", "E", "N", "O", "S", "W"))%>%
  #Filter only the registration numbers that have the correct month format
  filter(substr(Registration_Number_y2, 3,3) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"))%>%
  #Recode all the Reg numbers for easier groupings.
  dplyr::mutate(country_of_training = str_sub(Registration_Number_y2,-1)) %>% 
  dplyr::mutate(country_of_training = recode(country_of_training, 
                                      A = "UK", 
                                      C = "EU",
                                      D = "UK",
                                      E = "England",
                                      N = "Northern Ireland",
                                      O = "Overseas (non EU)",
                                      S = "Scotland",
                                      W = "Wales"
                                      )
  )%>%
  #Recode the month and year 
  dplyr::mutate(registration_year = str_sub(Registration_Number_y2, 1,2))%>%
  dplyr::mutate(registration_month = str_sub(Registration_Number_y2, 3,3))%>%
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
                                            "L" = 12))%>%
  #Create a new column in the format ready to be converted to a datetime
  dplyr::mutate(registration_date = if_else(registration_year < 25, paste0(20,registration_year,"-",registration_month,"-01"),
                                            paste0(19,registration_year,"-",registration_month,"-01")))%>%
  select(-c(registration_month, registration_year))%>%
  filter(joiner > 0 | occ_joiner > 0)%>%
  #Recode the current ESR month to a numeric number
  mutate(current_month = paste0(as.numeric(substr(Tm_Year_Month_y2,1,4)),"-",ifelse(substr(Tm_Year_Month_y2,6,8) == "JAN", 01,
                                                                        ifelse(substr(Tm_Year_Month_y2,6,8) == "FEB", 02,
                                                                               ifelse(substr(Tm_Year_Month_y2,6,8) == "MAR", 03,
                                                                                      ifelse(substr(Tm_Year_Month_y2,6,8) == "APR", 04,
                                                                                             ifelse(substr(Tm_Year_Month_y2,6,8) == "MAY", 05,
                                                                                                    ifelse(substr(Tm_Year_Month_y2,6,8) == "JUN", 06,
                                                                                                           ifelse(substr(Tm_Year_Month_y2,6,8) == "JUL", 07,
                                                                                                                  ifelse(substr(Tm_Year_Month_y2,6,8) == "AUG", 08,
                                                                                                                         ifelse(substr(Tm_Year_Month_y2,6,8) == "SEP", 09,
                                                                                                                                ifelse(substr(Tm_Year_Month_y2,6,8) == "OCT", 10,
                                                                                                                                       ifelse(substr(Tm_Year_Month_y2,6,8) == "NOV", 11, 12
                                                                                                                                       ))))))))))),"-01")

  )




#Convert registration date, date of birth and current month to datetime
pin_data$registration_date <- ymd(pin_data$registration_date)
pin_data$Date_Of_Birth_y2 <- ymd(pin_data$Date_Of_Birth_y2)
pin_data$current_month <- ymd(pin_data$current_month)

#Filter out the registration numbers that are before the date of birth and before the current month
Data_birth_filter <- pin_data %>% filter(Date_Of_Birth_y2 < registration_date)%>%
  filter(registration_date <= current_month)

#Set periods of 3, 6 and 12 months prior to current month to allow grouping
months_1 <- max(Data_birth_filter$current_month) %m-% months(1)
months_2 <- max(Data_birth_filter$current_month) %m-% months(2)
months_3 <- max(Data_birth_filter$current_month) %m-% months(3)
months_6 <- max(Data_birth_filter$current_month) %m-% months(6)
months_9 <- max(Data_birth_filter$current_month) %m-% months(9)
months_12 <- max(Data_birth_filter$current_month) %m-% months(12)
months_24 <- max(Data_birth_filter$current_month) %m-% months(24)
months_60 <- max(Data_birth_filter$current_month) %m-% months(60)




#Mutate the NMC dates to time periods before joining ESR.
Data_esr_grouped <- Data_birth_filter %>%
  mutate(NMC_to_ESR = ifelse(registration_date == current_month, "Immediate Joiner",
                             ifelse(registration_date >= months_1 & registration_date < current_month, "1 Month",
                                    ifelse(months_1 > registration_date & registration_date >= months_2, " 1 to 2 Months",
                                           ifelse(months_2 > registration_date & registration_date >= months_3, "2 to 3 Months",
                                                  ifelse(months_3 > registration_date & registration_date >= months_6, "3 to 6 Months",
                                                         ifelse(months_6 > registration_date & registration_date >= months_12, "6 to 12 Months",
                                                                ifelse(months_12 > registration_date & registration_date >= months_24, "1 to 2 years",
                                                                       ifelse(months_24 > registration_date & registration_date >= months_60, "2 to 5 years", "Over 5 years")))))))))
                                            


###################################joiner/ leaver summaries###################################
#Create total for country
summary_totals_country <- Data_esr_grouped %>%
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))%>%
  mutate(country_of_training = "All")%>%
  select(c(3,1,2))

#Summarise for all country of training and append total onto the bottom
summary_by_training_country <- Data_esr_grouped %>%
  group_by(country_of_training) %>%
  summarise (joiner=sum(joiner),
             occ_joiner=sum(occ_joiner)
             )%>%
  bind_rows(summary_totals_country)%>%
  pivot_longer(c(2:3))

#Create total for date
summary_totals_date <- Data_esr_grouped %>%
  summarise(joiner = sum(joiner),
            occ_joiner = sum(occ_joiner))%>%
  mutate(NMC_to_ESR = "All")%>%
  select(c(3,1,2))

#Create vector to sort NMC to ESR column into defined order
date_order <- c("Immediate Joiner",
                "1 Month",
                "1 to 2 Months",
                "2 to 3 Months",
                "3 to 6 Months",
                "6 to 12 Months",
                "1 to 2 years",
                "2 to 5 years",
                "Over 5 years",
                "All")

#Summarise for all dates and append total onto the bottom
summary_by_date <- Data_esr_grouped %>%
  group_by(NMC_to_ESR) %>%
  summarise(joiner=sum(joiner),
            occ_joiner = sum(occ_joiner))%>%
  bind_rows(summary_totals_date)%>%
  pivot_longer(c(2:3))





###################################Pull joiner/ leaver period name###################################
##preparing extract for nurses dashboard time series
summary_training_country <- summary_training_country %>%
  mutate(Date_from = paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2])) %>%
  mutate(Date = as.Date(paste0(substr(Date_from,13,16),"-", substr(Date_from,6,8), "-01"),"%Y-%b-%d")) %>%
  select(1,4,2,3,5)

summary_by_date <- summary_by_date %>%
  mutate(Date_from = paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2])) %>%
  mutate(Date = as.Date(paste0(substr(Date_from,13,16),"-", substr(Date_from,6,8), "-01"),"%Y-%b-%d")) %>%
  select(1,4,2,3,5)

################################### Exports ###################################

#export to shared area with today's date
write.csv(pivot_final, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nurses/Nurse",str_replace_all(Sys.Date(),"-",""),"joiners_leavers",long_format$Date_from[1],".csv"))

#export to shared area with today's date
write.csv(pivot_final, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Dashboard/Data for dashboard/ESR Joiners Leavers/",str_replace_all(Sys.Date(),"-",""),"_joiners_leavers",long_format$Date_from[1],".csv"))

#export to shared area with today's date
write.csv(Reasonleaving, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Dashboard/Data for dashboard/Reason for leaving/reason_for_leaving_",long_format$Date_from[1],".csv"))

#export to shared area with today's date
write.csv(Country, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Dashboard/Data for dashboard/Joiners by Country/Country_joiners_",long_format$Date_from[1],".csv"))

