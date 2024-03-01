# First import library
library(tidyverse)

# Set working directory

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS")

# Read in all the files, ready to concatenate

df <- read_csv("Records 6.csv")





#Read in Org files
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")
NHS_orgs <- read_csv("ORG Codes NHS Digital.csv")
nationality <- read_csv("Nationality groupings.csv")


#Rename columns to make it easier to manipulate
colnames(df) <- str_replace_all(colnames(df), " ", "_")
colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
#Join in org files to main document
df <- full_join(df, NHS_orgs)
df <- full_join(df, nationality)

#Filter the nurse codes
df_filtered_6 <- df |> filter(Occupation_Code %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                          "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                          "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                          "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                          "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                          "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                          "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A","N1A","NNN")) |>
  #Filter the rows that don't have 8 characters or are NA.
  filter(!str_length(Registration_Number) == 8 |
           is.na(Registration_Number) == TRUE |
           str_length(Registration_Number) == 8 & !substr(Registration_Number, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0) |
           str_length(Registration_Number) == 8 & !substr(Registration_Number, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0) |
           str_length(Registration_Number) == 8 & !toupper(substr(Registration_Number, 8,8)) %in% c("A", "C", "D", "E", "N", "O", "S", "W") |
           str_length(Registration_Number) == 8 & !toupper(substr(Registration_Number, 3,3)) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "Y","U")
         ) |>
  filter(Status %in% c("Active Assignment", "Internal Secondment", "Acting Up") & Asg_Type_Of_Contract %in% c("Locum", "Fixed Term Temp", "Permanent")) |>
  filter(NHSD_trust_or_CCG == 1) |>
  mutate (Nationality_grouping =if_else(is.na(Nationality_grouping) == FALSE, Nationality_grouping, 'Unknown')) |>
  mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                               if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other')))
rm(df)

df_filtered_6 <- df_filtered_6[-16]

df_filtered <- rbind(df_filtered_1,
                     df_filtered_2,
                     df_filtered_3,
                     df_filtered_4,
                     df_filtered_5,
                     df_filtered_6)


#Filter to unique NHS numbers and summarise.
df_unique_1 <- df_filtered |> distinct(Unique_Nhs_Identifier, .keep_all = TRUE) |>
  mutate(no_registration = if_else(is.na(Registration_Number) == TRUE, Contracted_Wte, 0)) |>
  mutate(incorrect_registration = if_else(is.na(Registration_Number) == FALSE, Contracted_Wte, 0)) |>
  group_by(Nationality_grouping) |>
  summarise(no_registration = sum(no_registration),
            incorrect_registration = sum(incorrect_registration))

write_csv(df_unique_1, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/No PIN.csv")
