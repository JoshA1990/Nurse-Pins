
# Code to call in multiple runs of the matching function

matching_function <- function() {
  #Amend column names so the dots are replaced with spaces
  colnames(Raw_Data) <- str_replace_all(colnames(Raw_Data), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  
  ##### Data Wrangling -----------------------------------------------------------
  
  #filter to nurses only in active assignment
  Nurses <- Raw_Data %>% 
    mutate(Staff_group = if_else(substr(Occupation_Code, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                 if_else(Occupation_Code %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "Nurse", 
                                         if_else(Occupation_Code %in% c("N2C", "N2J", "N2L"),"x.midwife",
                                                 if_else(Occupation_Code %in% "N3H", "x.health_visitor",
                                                         if_else(Occupation_Code %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%
    filter(Status %in% c("Active Assignment", "Internal Secondment", "Acting Up"))%>%
    filter(Asg_Type_Of_Contract %in% c("Locum", "Fixed Term Temp", "Permanent")) %>%
    filter(Staff_group=='Nurse')
  
  ### Activate for headcount ###
  Nurses$Contracted_Wte <- ifelse(Nurses$Contracted_Wte > 0, 1, Nurses$Contracted_Wte)
  
  #merge on NHS D organisation codes and naionality file
  Nurses <- full_join(Nurses,NHS_orgs)
  Nurses <- full_join(Nurses,nationality)
  
  #filter to only NHS trusts and CCGs and override NA nationality with Unknowns
  Nurses <- Nurses %>% filter(NHSD_trust_or_CCG==1 & is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate(Nationality_grouping = if_else(is.na(Nationality_grouping)==TRUE, 'Unknown',Nationality_grouping)) %>%
    filter(is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic',
                                              if_else(Nationality_grouping %in% c('EU','ROW'),'IR','Other')))
  
  
  ##################   Create flags for Registration info   ##################
  convert_to_number <- function(letter) {
    if (toupper(letter) %in% LETTERS[1:12]) {
      return(match(toupper(letter), LETTERS[1:12]))
    } else {
      return(sample(1:12,1))
    }
  }
  
  pin_data <- Nurses %>% filter(is.na(Registration_Number) == FALSE)%>%
    #Enforce the length of registration number has to equal 8
    filter(str_length(Registration_Number) == 8)%>%
    #Filter only the registration numbers who begin with two numbers.
    filter(substr(Registration_Number, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
    filter(substr(Registration_Number, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
    #Filter only the registration numbers that have a legitimate country code
    filter(toupper(substr(Registration_Number, 8,8)) %in% c("A", "C", "D", "E", "N", "O", "S", "W", 0))%>%
    #Filter only the registration numbers that have the correct month format
    filter(toupper(substr(Registration_Number, 3,3)) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "Y", "U"))%>%
    #Recode all the Reg numbers for easier groupings.
    dplyr::mutate(country_of_training = str_sub(Registration_Number,-1)) %>% 
    dplyr::mutate(country_of_training = recode(toupper(country_of_training), 
                                               A = "UK", 
                                               C = "EU",
                                               D = "UK",
                                               E = "UK",
                                               N = "UK",
                                               O = "ROW",
                                               '0' = 'ROW',
                                               S = "UK",
                                               W = "UK"
    )
    )%>%
    #Recode the month and year 
    dplyr::mutate(registration_year = str_sub(Registration_Number, 1,2))%>%
    dplyr::mutate(registration_month = str_sub(Registration_Number, 3,3))%>%
    dplyr::mutate(registration_letter = registration_month)
  
  pin_data$registration_month <- sapply(pin_data$registration_month, convert_to_number)
  
  pin_data <- pin_data %>%
    dplyr::mutate(registration_date = if_else(registration_year < 25, paste0(20,registration_year,"-",registration_month,"-01"),
                                              paste0(19,registration_year,"-",registration_month,"-01")))%>%
    select(-c(registration_month, registration_year))%>%
    #Recode the current ESR month to a numeric number
    mutate(current_month = paste0(as.numeric(substr(Tm_Year_Month,1,4)),"-",ifelse(substr(Tm_Year_Month,6,8) == "JAN", 01,
                                                                                   ifelse(substr(Tm_Year_Month,6,8) == "FEB", 02,
                                                                                          ifelse(substr(Tm_Year_Month,6,8) == "MAR", 03,
                                                                                                 ifelse(substr(Tm_Year_Month,6,8) == "APR", 04,
                                                                                                        ifelse(substr(Tm_Year_Month,6,8) == "MAY", 05,
                                                                                                               ifelse(substr(Tm_Year_Month,6,8) == "JUN", 06,
                                                                                                                      ifelse(substr(Tm_Year_Month,6,8) == "JUL", 07,
                                                                                                                             ifelse(substr(Tm_Year_Month,6,8) == "AUG", 08,
                                                                                                                                    ifelse(substr(Tm_Year_Month,6,8) == "SEP", 09,
                                                                                                                                           ifelse(substr(Tm_Year_Month,6,8) == "OCT", 10,
                                                                                                                                                  ifelse(substr(Tm_Year_Month,6,8) == "NOV", 11, 12
                                                                                                                                                  ))))))))))),"-01")
           
    )
  
  
  
  
  #Convert registration date, date of birth and current month to datetime
  pin_data$registration_date <- ymd(pin_data$registration_date)
  pin_data$Date_Of_Birth_y2 <- ymd(pin_data$Date_Of_Birth)
  pin_data$current_month <- ymd(pin_data$current_month)
  
  #Add a column to calculate age at date of registration
  pin_data$age_check <- as.numeric(difftime(pin_data$registration_date,
                                            pin_data$Date_Of_Birth,
                                            unit = "weeks"))/52.25
  
  #Filter out the registration numbers that are before the date of birth and before the current month
  #And people under the age of 16
  Data_birth_filter <- pin_data %>% filter(Date_Of_Birth < registration_date)%>%
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
  
  #Filter out all the entries that the country of training does NOT match nationality
  training_non_matches <- Data_esr_grouped |> filter(Nationality_grouping != country_of_training)
  training_matches <- Data_esr_grouped |> filter(Nationality_grouping == country_of_training)
  ###########################   Joiner summaries   ##############################
  
  #Create Summary for the non-matches of training country and nationality
  
  non_matches_summary <- training_non_matches |>
    group_by(Nationality_grouping, country_of_training) |>
    summarise(total = sum(Contracted_Wte))
  
  matches_summary <- training_matches |>
    group_by(Nationality_grouping, country_of_training) |>
    summarise(total = sum(Contracted_Wte))
  
  summary <- bind_rows(non_matches_summary, matches_summary)
  
  summary <- summary |> mutate(nationality_to_training = paste(Nationality_grouping, "nationality and", country_of_training, "trained")) |>
    ungroup() |>
    select(4,3)
  
  summary_total <- summary |> summarise(across(where(is.numeric), sum),
                                        across(where(is.character), ~ "Total Nurses"))
  
  summary <- bind_rows(summary, summary_total)
  
  ###############################################################################
  
  
  
  
  ###################################Pull joiner/ leaver period name###################################
  ##preparing extract for nurses dashboard time series
  
  colnames(summary) <- c("nationality to training", paste(substr(Nurses$Tm_Year_Month,1,8)[1]))
  
  ################################### Exports ###################################
  
  #export to own area with today's date
  write.csv(summary, paste0("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NMC Pins/Training nationality mismatch/",substr(Nurses$Tm_Year_Month,1,8)[1],".csv"))
  
  rm(list=ls()[! ls() %in% c("nationality","NHS_orgs")])
  
  
}