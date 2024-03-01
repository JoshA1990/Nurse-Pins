library(dplyr)
library(lubridate)
library(readr)
library(rlang)
library(tidyr)
library(tidyverse)


# Define supporting functions --------------------------------------------------

wrangle_annual_data <- function(df_, df_nations_, df_orgs_, suffix_) {
  # Specify staff groups -------------------------------------------------------
  
  grpcode_medical <- c(0, 1, 2, 8, 9)
  grpcode_nurse <- c(
    "N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
    "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
    "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
    "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
    "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
    "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
    "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J", "P2A", "P3A",
    "N1A", "NNN"
  )
  grpcode_midwife <- c("N2C", "N2J", "N2L")
  grpcode_visitor <- "N3H"
  grpcode_support <- c("H1A", "N9A", "NFA")
  
  # Specify status -------------------------------------------------------------
  
  status_active <- c("Active Assignment", "Internal Secondment", "Acting Up")
  
  # Specify contract status ----------------------------------------------------
  
  contract_notother <- c("Locum", "Fixed Term Temp", "Permanent")
  
  # Format annual data ---------------------------------------------------------
  
  annual_data <- df_ |>
    # Amend column names so the dots are replaced with spaces
    rename_with(\(x) str_replace_all(x, " ", "_")) |>
    # Join NHS organisation codes and nationality
    full_join(df_nations_) |>
    full_join(df_orgs_) |>
    # Create flags for active nurses - this will be important depending on
    # whether looking at joiners or leavers later
    mutate(
      Staff_group = case_when(
        str_sub(.data$Occupation_Code, 1, 1) %in% grpcode_medical ~ "x.Medical",
        .data$Occupation_Code %in% grpcode_nurse ~ "Nurse",
        .data$Occupation_Code %in% grpcode_midwife ~ "x.midwife",
        .data$Occupation_Code %in% grpcode_visitor ~ "x.health_visitor",
        .data$Occupation_Code %in% grpcode_support ~ "Support",
        TRUE ~ "x.Other"
      )
    ) |>
    mutate(Status_orig = .data$Status) |>
    mutate(
      Status = if_else(.data$Status %in% status_active, "Active", "Not active")
    ) |>
    filter(.data$Asg_Type_Of_Contract %in% contract_notother) |>
    mutate(
      Nationality_grouping = coalesce(.data$Nationality_grouping, "Unknown")
    ) |>
    mutate(
      Nationality_grouping_v2 = case_match(
        .data$Nationality_grouping,
        "ROW" ~ "IR",
        "IR" ~ "IR",
        "UK" ~ "Domestic",
        "Unknown" ~ "Domestic",
        .default = "Other"
      )
    ) |>
    # Order data by staff group and FTE
    arrange(.data$Staff_group, desc(.data$Contracted_Wte)) |>
    # Add suffixes to supporting variables to distinguish between datasets.
    # Variable used for later merging will be excluded
    rename_with(\(x) paste(x, suffix_, sep = "_"), -"Unique_Nhs_Identifier")
  
  # Return formatted data ------------------------------------------------------
  
  return(annual_data)
}

calculate_pin_joiners <- function(df1_, df2_, df_nations_, df_orgs_, outdir_) {
  # Initial data wrangling -----------------------------------------------------
  
  data_y1 <- wrangle_annual_data(df1_, df_nations_, df_orgs_, "y1")
  data_y2 <- wrangle_annual_data(df2_, df_nations_, df_orgs_, "y2")
  
  # Remove all duplicates in Unique_Nhs_Identifier
  data_y1_dedup <- data_y1 |>
    distinct(.data$Unique_Nhs_Identifier, .keep_all = TRUE)
  data_y2_dedup <- data_y2 |>
    distinct(.data$Unique_Nhs_Identifier, .keep_all = TRUE)
  
  # Fill in missing data
  data_y1 <- data_y1 |>
    left_join(data_y2_dedup, by = "Unique_Nhs_Identifier") |>
    # Overwrite Y1 nationality
    mutate(
      Nationality_grouping_v2_y1_2 = coalesce(
        .data$Nationality_grouping_v2_y2,
        .data$Nationality_grouping_v2_y1
      )
    ) |>
    mutate(
      Nationality_grouping_v2_y1 = coalesce(
        .data$Nationality_grouping_y2,
        .data$Nationality_grouping_y1
      )
    )
  
  # Join data
  full_data <- data_y1_dedup |>
    full_join(data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  # Cleanup
  rm(data_y1_dedup, data_y2_dedup)
  
  # Continue data wrangling ----------------------------------------------------
  
  # Combine variables in joined data
  full_data <- full_data |>
    # Merge nationality into a single field, replacing NAs with Unknown
    mutate(
      Nationality_grouping = coalesce(
        .data$Nationality_y2,
        .data$Nationality_y1,
        "Unknown"
      )
    ) |>
    mutate(
      Nationality_grouping_v2 = case_match(
        .data$Nationality_grouping,
        "ROW" ~ "IR",
        "IR" ~ "IR",
        "UK" ~ "Domestic",
        "Unknown" ~ "Domestic",
        .default = "Other"
      )
    ) |>
    # Replace NA NHS providers with 0s
    mutate(NHSD_trust_or_CCG_y1 = coalesce(.data$NHSD_trust_or_CCG_y1, 0)) |>
    mutate(NHSD_trust_or_CCG_y2 = coalesce(.data$NHSD_trust_or_CCG_y2, 0)) |>
    # Add joiner/leaver flags
    mutate(
      joiner = if_else(
        is.na(.data$Staff_group_y1) &
          .data$Staff_group_y2 == "Nurse" &
          .data$Status_y2 == "Active" &
          .data$NHSD_trust_or_CCG_y2 == "1",
        .data$Contracted_Wte_y2,
        0
      )
    ) |>
    mutate(
      occ_joiner = if_else(
        .data$Staff_group_y1 != "Nurse" &
          .data$NHSD_trust_or_CCG_y1 == "1" &
          .data$Staff_group_y2 == "Nurse" &
          .data$Status_y2 == "Active" &
          .data$NHSD_trust_or_CCG_y2 == "1",
        .data$Contracted_Wte_y2,
        0
      )
    )
  
  # Perform duplication check
  full_data |>
    group_by(.data$Unique_Nhs_Identifier) |>
    filter(row_number() > 1) |>
    pull("Unique_Nhs_Identifier") |>
    print()
  
  # Create flags for registration info -----------------------------------------
  
  # Specify valid country codes
  valid_countries <- c("A", "C", "D", "E", "N", "O", "S", "W")
  
  # Create pin data
  pin_data <- full_data |>
    filter(!is.na(.data$Registration_Number_y2)) |>
    # Enforce the length of registration number has to equal 8
    filter(str_length(.data$Registration_Number_y2) == 8) %>%
    # Filter only the registration numbers who begin with two numbers
    filter(str_starts(.data$Registration_Number_y2, "\\d{2}")) |>
    # Filter only the registration numbers that have a legitimate country code
    filter(str_sub(.data$Registration_Number_y2, 8, 8) %in% valid_countries) |>
    # Filter only the registration numbers that have the correct month format
    filter(str_sub(.data$Registration_Number_y2, 3, 3) %in% LETTERS[1:12]) |>
    # Recode all the Reg numbers for easier groupings.
    mutate(country_of_training = str_sub(.data$ Registration_Number_y2, -1)) |>
    mutate(
      country_of_training = case_match(
        .data$country_of_training,
        "A" ~ "UK",
        "C" ~ "EU",
        "D" ~ "UK",
        "E" ~ "England",
        "N" ~ "Northern Ireland",
        "O" ~ "Overseas (non EU)",
        "S" ~ "Scotland",
        "W" ~ "Wales"
      )
    ) |>
    # Recode the month and year
    mutate(registration_year = str_sub(.data$Registration_Number_y2, 1, 2)) |>
    mutate(registration_month = str_sub(.data$Registration_Number_y2, 3, 3)) |>
    mutate(
      registration_month = str_pad(
        match(.data$registration_month, LETTERS),
        width = 2,
        side = "left",
        pad = "0"
      )
    ) |>
    # Create a new column in the format ready to be converted to a datetime
    mutate(
      registration_year = if_else(
        .data$registration_year < 25,
        paste0(20, .data$registration_year),
        paste0(19, .data$registration_year)
      )
    ) |>
    mutate(
      registration_date = paste(
        .data$registration_year,
        .data$registration_month,
        "01",
        sep = "-"
      )
    ) |>
    select(-c("registration_month", "registration_year")) |>
    filter(.data$joiner > 0 | .data$occ_joiner > 0) |>
    # Recode ESR data to a date-like string
    mutate(
      current_month = paste0(
        str_sub(.data$Tm_Year_Month_y2, 1, 4),
        case_match(
          str_sub(.data$Tm_Year_Month_y2, 6, 8),
          "JAN" ~ "01",
          "FEB" ~ "02",
          "MAR" ~ "03",
          "APR" ~ "04",
          "MAY" ~ "05",
          "JUN" ~ "06",
          "JUL" ~ "07",
          "AUG" ~ "08",
          "SEP" ~ "09",
          "OCT" ~ "10",
          "NOV" ~ "11",
          "DEC" ~ "12"
        ),
        "01",
        sep = "-"
      )
    ) |>
    # Convert registration date, date of birth and current month to datetime
    mutate(registration_date = ymd(.data$registration_date)) |>
    mutate(Date_Of_Birth_y2 = ymd(.data$Date_Of_Birth_y2)) |>
    mutate(current_month = ymd(.data$current_month)) |>
    # Add a column to calculate age at date of registration
    mutate(
      age_check = time_length(
        interval(.data$Date_Of_Birth_y2, .data$registration_date),
        "years"
      )
    ) |>
    # Filter out the registration numbers that are before the date of birth and
    # before the current month
    filter(.data$registration_date <= .data$current_month) |>
    # Filter out individuals under 16
    filter(.data$age_check > 16)
  
  # Set periods prior to current month to allow grouping
  months_1 <- max(pin_data$current_month) %m-% months(1)
  months_2 <- max(pin_data$current_month) %m-% months(2)
  months_3 <- max(pin_data$current_month) %m-% months(3)
  months_6 <- max(pin_data$current_month) %m-% months(6)
  months_12 <- max(pin_data$current_month) %m-% months(12)
  months_24 <- max(pin_data$current_month) %m-% months(24)
  months_60 <- max(pin_data$current_month) %m-% months(60)
  
  # Calculate joining period
  data_esr_grouped <- pin_data |>
    # Mutate the NMC dates to time periods before joining ESR
    mutate(
      NMC_to_ESR = case_when(
        .data$registration_date == .data$current_month ~ "Immediate Joiner",
        .data$registration_date >= months_1 ~ "1 Month",
        .data$registration_date >= months_2 ~ "1 to 2 Months",
        .data$registration_date >= months_3 ~ "2 to 3 Months",
        .data$registration_date >= months_6 ~ "3 to 6 Months",
        .data$registration_date >= months_12 ~ "6 to 12 Months",
        .data$registration_date >= months_24 ~ "1 to 2 years",
        .data$registration_date >= months_60 ~ "2 to 5 years",
        TRUE ~ "Over 5 years"
      )
    )
  
  # Create summary data --------------------------------------------------------
  
  # Create vector to sort NMC to ESR column into defined order
  date_order <- c(
    "Immediate Joiner",
    "1 Month",
    "1 to 2 Months",
    "2 to 3 Months",
    "3 to 6 Months",
    "6 to 12 Months",
    "1 to 2 years",
    "2 to 5 years",
    "Over 5 years",
    "All"
  )
  
  range_name <- full_data |>
    slice_head(n = 1) |>
    select("Tm_Year_Month_y1", "Tm_Year_Month_y2") |>
    str_sub(1, 8) |>
    paste(collapse = " to ")
  
  # Create total for country
  summary_totals_country <- data_esr_grouped |>
    summarise(joiner = sum(.data$joiner), occ_joiner = sum(.data$occ_joiner)) |>
    mutate(country_of_joining = "All") |>
    select("country_of_joining", "joiner", "occ_joiner")
  
  # Summarise for all country of training and append total onto the bottom
  summary_by_training_country <- data_esr_grouped %>%
    group_by(.data$country_of_training) %>%
    summarise(joiner = sum(.data$joiner), occ_joiner = sum(.data$occ_joiner)) |>
    bind_rows(summary_totals_country) |>
    pivot_longer(c("joiner", "occ_joiner"), values_to = range_name)
  
  # Create total for transition date
  summary_totals_transition_date <- data_esr_grouped |>
    summarise(joiner = sum(.data$joiner), occ_joiner = sum(.data$occ_joiner)) |>
    mutate(NMC_to_ESR = "All") |>
    select("NMC_to_ESR", "joiner", "occ_joiner")
  
  # Summarise for all dates and append total onto the bottom
  summary_by_transition_date <- data_esr_grouped |>
    group_by(.data$NMC_to_ESR) |>
    summarise(joiner = sum(.data$joiner), occ_joiner = sum(.data$occ_joiner)) |>
    bind_rows(summary_totals_transition_date) |>
    pivot_longer(c("joiner", "occ_joiner"), values_to = range_name) |>
    mutate(NMC_to_ESR = factor(.data$NMC_to_ESR, levels = date_order)) |>
    arrange(.data$NMC_to_ESR)
  
  # Summarise for registration date
  summary_by_registration_date <- data_esr_grouped |>
    group_by(.data$registration_date) |>
    summarise(joiner = sum(.data$joiner), occ_joiner = sum(.data$occ_joiner)) |>
    pivot_longer(c("joiner", "occ_joiner"), values_to = range_name)
  
  # Export data ----------------------------------------------------------------
  
  file_name <- full_data |>
    slice_head(n = 1) |>
    select("Tm_Year_Month_y2") |>
    pull() |>
    str_sub(1, 8) |>
    paste0(".csv")
  
  # Write data
  write_csv(
    summary_by_transition_date,
    file.path(outdir_, "Joiners by Date", file_name)
  )
  write_csv(
    summary_by_training_country,
    file.path(outdir_, "Country of Training", file_name)
  )
  write_csv(
    summary_by_registration_date,
    file.path(outdir_, "Joiners by Registration", file_name)
  )
  
  # Return NULL ----------------------------------------------------------------
  
  return(invisible(NULL))
}

# Define core function ---------------------------------------------------------

produce_pin_summary <- function(coredir_, datadir_, outdir_) {
  # Read in core data ----------------------------------------------------------
  
  df_nations <- read_csv(file.path(coredir_, "Nationality groupings.csv")) |>
    rename_with(\(x) str_replace_all(x, " ", "_"))
  df_orgs <- read_csv(file.path(coredir_, "ORG Codes NHS Digital.csv")) |>
    rename_with(\(x) str_replace_all(x, " ", "_"))
  
  # Identify all data files ----------------------------------------------------
  
  # FILES MUST BE IN ORDER ALPHABETICALLY AND CHRONOLOGICALLY
  file_list <- list.files(path = datadir_, pattern = ".csv", full.names = TRUE)
  
  # Create data for each file pair ---------------------------------------------
  
  for (i in 1:(length(file_list) - 1)) {
    
    # Give useful message
    inform(
      paste(
        "Comparing",
        basename(file_list[i]),
        "to",
        basename(file_list[i + 1])
      )
    )
    
    # Load datafiles
    data1 <- read_csv(file_list[i])
    data2 <- read_csv(file_list[i + 1])
    
    # Undertake calculation
    calculate_pin_joiners(data1, data2, df_nations, df_orgs, outdir_)
  }
  
  # Return NULL ----------------------------------------------------------------
  
  return(invisible(NULL))
}

# Run core function ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(readr)
library(rlang)
library(tidyr)
library(tidyverse)

coredir <- file.path(
  "C:",
  "Users",
  "Josh.Andrews",
  "OneDrive - Department of Health and Social Care",
  "Nurse Data"
)
datadir <- file.path(
  "C:",
  "Users",
  "Josh.Andrews",
  "OneDrive - Department of Health and Social Care",
  "wf",
  "Cross-cutting work",
  "Brexit",
  "Nursing",
  "NMC PINS",
  "ESR records with PINS"
)
outdir <- file.path(
  "C:",
  "Users",
  "Josh.Andrews",
  "OneDrive - Department of Health and Social Care",
  "Nurse Data",
  "Outputs",
  "Tests"
)

produce_pin_summary(coredir, datadir, outdir)
