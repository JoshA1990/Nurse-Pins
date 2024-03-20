

full_joiners_function <- function() {
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
                                    if_else(Occupation_Code_y1 %in% nurse_staff_codes, "Nurse", 
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
                                    if_else(Occupation_Code_y2 %in% nurse_staff_codes, "Nurse", 
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
  
  # Converting to headcount
  Raw_Data_y1$Contracted_Wte_y1 <- ifelse(Raw_Data_y1$Contracted_Wte_y1 > 0, 1, Raw_Data_y1$Contracted_Wte_y1)
  Raw_Data_y2$Contracted_Wte_y2 <- ifelse(Raw_Data_y2$Contracted_Wte_y2 > 0, 1, Raw_Data_y2$Contracted_Wte_y2)
  
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
    mutate(joiner = if_else(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1" & Contracted_Wte_y2 > 0, 1, 0)) %>%
    mutate(occ_joiner = if_else(Staff_group_y1 != "Nurse" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1" & Contracted_Wte_y2 > 0 , 1, 0))
  
  
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
  
  ##################   Create flags for Registration info   ##################
  convert_to_number <- function(letter) {
    if (toupper(letter) %in% LETTERS[1:12]) {
      return(match(toupper(letter), LETTERS[1:12]))
    } else {
      return(sample(1:12,1))
    }
  }
  
  pin_data <- Data %>% filter(is.na(Registration_Number_y2) == FALSE)%>%
    #Enforce the length of registration number has to equal 8
    filter(str_length(Registration_Number_y2) == 8)%>%
    #Filter only the registration numbers who begin with two numbers.
    filter(substr(Registration_Number_y2, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
    filter(substr(Registration_Number_y2, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
    #Filter only the registration numbers that have a legitimate country code
    filter(toupper(substr(Registration_Number_y2, 8,8)) %in% c("A", "C", "D", "E", "N", "O", "S", "W", 0))%>%
    #Filter only the registration numbers that have the correct month format
    filter(toupper(substr(Registration_Number_y2, 3,3)) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "Y", "U"))%>%
    #Recode all the Reg numbers for easier groupings.
    dplyr::mutate(country_of_training = str_sub(Registration_Number_y2,-1)) %>% 
    dplyr::mutate(country_of_training = recode(toupper(country_of_training), 
                                               A = "UK", 
                                               C = "EU",
                                               D = "UK",
                                               E = "England",
                                               N = "Northern Ireland",
                                               O = "Overseas (non EU)",
                                               '0' = 'Overseas (non EU)',
                                               S = "Scotland",
                                               W = "Wales"
    )
    )%>%
    #Recode the month and year 
    dplyr::mutate(registration_year = str_sub(Registration_Number_y2, 1,2))%>%
    dplyr::mutate(registration_month = str_sub(Registration_Number_y2, 3,3))%>%
    dplyr::mutate(registration_letter = registration_month)
  
  pin_data$registration_month <- sapply(pin_data$registration_month, convert_to_number)
  
  pin_data <- pin_data %>%
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
  
  #Add a column to calculate age at date of registration
  pin_data$age_check <- as.numeric(difftime(pin_data$registration_date,
                                            pin_data$Date_Of_Birth_y2,
                                            unit = "weeks"))/52.25
  
  #Filter out the registration numbers that are before the date of birth and before the current month
  #And people under the age of 16
  Data_birth_filter <- pin_data %>% filter(Date_Of_Birth_y2 < registration_date)%>%
    filter(registration_date <= current_month)%>%
    filter(age_check > 16)
  
  
  
  
  # Set periods prior to current month to allow grouping
  months_1 <- max(pin_data$current_month) %m-% months(1)
  months_2 <- max(pin_data$current_month) %m-% months(2)
  months_3 <- max(pin_data$current_month) %m-% months(3)
  months_4 <- max(pin_data$current_month) %m-% months(4)
  months_5 <- max(pin_data$current_month) %m-% months(5)
  months_6 <- max(pin_data$current_month) %m-% months(6)
  months_7 <- max(pin_data$current_month) %m-% months(7)
  months_8 <- max(pin_data$current_month) %m-% months(8)
  months_9 <- max(pin_data$current_month) %m-% months(9)
  months_10 <- max(pin_data$current_month) %m-% months(10)
  months_11 <- max(pin_data$current_month) %m-% months(11)
  months_12 <- max(pin_data$current_month) %m-% months(12)
  months_18 <- max(pin_data$current_month) %m-% months(18)
  months_24 <- max(pin_data$current_month) %m-% months(24)
  months_60 <- max(pin_data$current_month) %m-% months(60)
  months_120 <- max(pin_data$current_month) %m-% months(120)
  
  
  # Calculate joining period
  Data_esr_grouped <- Data_birth_filter |>
    # Mutate the NMC dates to time periods before joining ESR
    mutate(
      NMC_to_ESR = case_when(
        .data$registration_date == .data$current_month ~ "Immediate Joiner",
        .data$registration_date >= months_1 ~ "1 Month",
        .data$registration_date >= months_2 ~ "1 to 2 Months",
        .data$registration_date >= months_3 ~ "2 to 3 Months",
        .data$registration_date >= months_4 ~ "3 to 4 Months",
        .data$registration_date >= months_5 ~ "4 to 5 Months",
        .data$registration_date >= months_6 ~ "5 to 6 Months",
        .data$registration_date >= months_7 ~ "6 to 7 Months",
        .data$registration_date >= months_8 ~ "7 to 8 Months",
        .data$registration_date >= months_9 ~ "8 to 9 Months",
        .data$registration_date >= months_10 ~ "9 to 10 Months",
        .data$registration_date >= months_11 ~ "10 to 11 Months",
        .data$registration_date >= months_12 ~ "11 to 12 Months",
        .data$registration_date >= months_18 ~ "12 to 18 Months",
        .data$registration_date >= months_24 ~ "18 to 24 Months",
        .data$registration_date >= months_60 ~ "2 to 5 years",
        .data$registration_date >= months_120 ~ "5 to 10 years",
        TRUE ~ "Over 10 years"
      ))
  
  ################################### Exports ###################################
  
  #export to own area with today's date
  write.csv(Data_esr_grouped, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Full joiners/",substr(Data$Tm_Year_Month_y2,1,8)[1],".csv"))
  
  appended_nurses <<- Data_esr_grouped
  appended_nurses$Afc_Spinal_Point_y1 <<- as.numeric(appended_nurses$Afc_Spinal_Point_y1)
  appended_nurses$Afc_Spinal_Point_y2 <<- as.numeric(appended_nurses$Afc_Spinal_Point_y2)
  
  rm(list=ls()[! ls() %in% c("nationality","NHS_orgs", "Data_esr_grouped")])
  
  
}

