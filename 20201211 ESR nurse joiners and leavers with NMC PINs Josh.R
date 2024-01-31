
#NMC PIN number includes where a Nurse qualified (last letter: E, S, I ,W (UK) O (Overseas) 
#Numbers show Year and letter A-L Month of qualification Jan-Dec, plus Y Return to Practice.

# Summary of joiner/ leavers by nationality with NMC PIN numbers

###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)
#Working with strings package
#install.packages ("stringr")
library(stringr)

#data wrangling/ analysis package
library(dplyr)

#data wrangling/ analysis package
library(tidyverse)

#Dates package
library(lubridate)

#visualisation
library(eeptools)
library(ggplot2)

#data table
#install.packages("data.table")
# latest development version:
#data.table::update.dev.pkg()
library(data.table)

###################################Set working directory and load data###################################
#Note, you'll need to set the working directory to where the files are saved
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/PINS")

#specify Year and month of extract you want to run
y1 <- as.Date("2022-10-01")
y2 <- as.Date("2023-10-01")

#load main data, which will be used to observe flows
Raw_Data_y1 <- fread("Staff in Post - Workforce Oct 2022 with PINS.csv")
Raw_Data_y2 <- fread("Staff in Post - Workforce Oct 2023 with PINS.csv")

#setwd("C:/R/20200928 Nurse data - ESR/R run")

#helper files for nationality grouping and organisational files
nationality <- fread("Nationality groupings.csv")
NHS_orgs <- fread("Org Codes NHS Digital.csv")

#load monthly data inbetween the core flow comparison
Raw_Data_mid1 <- fread("Staff in Post - Workforce Nov 2022 with PINS.csv")
Raw_Data_mid2 <- fread("Staff in Post - Workforce Dec 2022 with PINS.csv")
Raw_Data_mid3 <- fread("Staff in Post - Workforce Jan 2023 with PINS.csv")
Raw_Data_mid4 <- fread("Staff in Post - Workforce Feb 2023 with PINS.csv")
Raw_Data_mid5 <- fread("Staff in Post - Workforce Mar 2023 with PINS.csv")  
Raw_Data_mid6 <- fread("Staff in Post - Workforce Apr 2023 with PINS.csv")
Raw_Data_mid7 <- fread("Staff in Post - Workforce May 2023 with PINS.csv")  
Raw_Data_mid8 <- fread("Staff in Post - Workforce Jun 2023 with PINS.csv")
Raw_Data_mid9 <- fread("Staff in Post - Workforce Jul 2023 with PINS.csv")
Raw_Data_mid10 <- fread("Staff in Post - Workforce Aug 2023 with PINS.csv")
Raw_Data_mid11 <- fread("Staff in Post - Workforce Sep 2023 with PINS.csv")


#union inbetween datasets to check if previously worked as a non nurse in NHS
occ_check <- bind_rows(Raw_Data_mid1,Raw_Data_mid2,Raw_Data_mid3,Raw_Data_mid4,Raw_Data_mid5,Raw_Data_mid6,Raw_Data_mid7,Raw_Data_mid8,Raw_Data_mid9,Raw_Data_mid10,Raw_Data_mid11)

#remove individual datasets
rm(Raw_Data_mid1,Raw_Data_mid2,Raw_Data_mid3,Raw_Data_mid4,Raw_Data_mid5,Raw_Data_mid6,Raw_Data_mid7,Raw_Data_mid8,Raw_Data_mid9,Raw_Data_mid10,Raw_Data_mid11)
rm(y1,y2)

#useful function that does the inverse of %in%, so all not contained in certain variable
'%!in%' <- function(x,y)!('%in%'(x,y))

###################################Data wrangling###################################
#Amend column names so the dots are replaced with spaces
colnames(Raw_Data_y1) <- str_replace_all(colnames(Raw_Data_y1), " ", "_")
colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
colnames(occ_check) <- str_replace_all(colnames(occ_check), " ", "_")

#occ_check %>% group_by(Tm_Year_Month) %>% summarise (sum(Contracted_Wte))

#remove unused columns
occ_check <- occ_check %>% select(-Organisation_Type, -Nationality, -Age_In_Years, -User_Person_Type, -Reason_For_Leaving)

#set keys and join NHS organisation codes and nationality (left joins)
setkey(Raw_Data_y1, "Ocs_Code")
setkey(Raw_Data_y2, "Ocs_Code")
setkey(occ_check, "Ocs_Code")
setkey(NHS_orgs, "Ocs_Code")

Raw_Data_y1 <- NHS_orgs[Raw_Data_y1]
Raw_Data_y2 <- NHS_orgs[Raw_Data_y2]
occ_check <- NHS_orgs[occ_check]

setkey(Raw_Data_y1, "Nationality")
setkey(Raw_Data_y2, "Nationality")
setkey(nationality, "Nationality")

Raw_Data_y1 <- nationality[Raw_Data_y1]
Raw_Data_y2 <- nationality[Raw_Data_y2]

#get rid of unsused data
rm(NHS_orgs,nationality)

#Adding sufixes to variables to separate variable names between datasets
colnames(Raw_Data_y1) <- paste(colnames(Raw_Data_y1), "y1", sep = "_")
colnames(Raw_Data_y2) <- paste(colnames(Raw_Data_y2), "y2", sep = "_")

#Rename unique identifier to remove the suffix so it's easier to merge datasets later on
Raw_Data_y1 <- rename(Raw_Data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
Raw_Data_y2 <- rename(Raw_Data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)


#create flags for active nurses, on both datasets, will be important depending on whether looking at joiners or leavers later
#Year1
#Are you a nurse or not flag?
Raw_Data_y1[, Staff_group_y1 := ifelse(substr(Occupation_Code_y1, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                       ifelse(Occupation_Code_y1 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                                                        "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                        "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                        "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                        "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                        "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                                                        "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "Nurse", 
                                              ifelse(Occupation_Code_y1 %in% c("N2C", "N2J", "N2L"),"x.midwife",
                                                     ifelse(Occupation_Code_y1 %in% "N3H", "x.health_visitor",
                                                            ifelse(Occupation_Code_y1 %in% c('H1A','H1B','H1C','H1D','H1E','H1F','H1P','H2A',
                                                                                             'H2B','H2C','H2D','H2E','H2F','H2P','N8A','N8B',
                                                                                             'N8C','N8D','N8E','N8F','N8G','N8H','N8K','N8L',
                                                                                             'N9A','N9B','N9C','N9D','N9E','N9F','N9G','N9H',
                                                                                             'N9J','N9K','N9L','NFA','NFB','NFC','NFD','NFE',
                                                                                             'NFF','NFG','NFH','NFJ','NFK','NFL','NGA','NGB',
                                                                                             'NGC','NGD','NGE','NGF','NGG','NGH','NHA','NHB',
                                                                                             'NHC','NHD','NHE','NHF','NHG','NHH'), "Support","x.Other")))))]
#Creating new variable to preserve original status
Raw_Data_y1[, Status_orig_y1 := Status_y1]
#Creating new variable  for active vs non active
Raw_Data_y1[, Status_y1 := ifelse(Status_y1 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active")]
#Creating new variable  for contract type
Raw_Data_y1[, Asg_Type_Of_Contract_y1 := ifelse(Asg_Type_Of_Contract_y1 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")]
#filtering to only permamnent contract type
Raw_Data_y1 <- subset(Raw_Data_y1, Asg_Type_Of_Contract_y1=='Permanent/fixed term/locum')
#Overriding NAs with Unknown
Raw_Data_y1[, Nationality_grouping_y1 := ifelse(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')]
#Creating another nationality variable grouping for IR vs Domestic split
Raw_Data_y1[, Nationality_grouping_y1_v2 := ifelse(Nationality_grouping_y1 %in% c('ROW','EU'), 'IR',
                                                   ifelse(Nationality_grouping_y1 %in% c('UK','Unknown'),'Domestic','Other'))]

#Year2
#Are you a nurse or not flag?
Raw_Data_y2[, Staff_group_y2 := ifelse(substr(Occupation_Code_y2, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                       ifelse(Occupation_Code_y2 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                                                        "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                        "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                        "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                        "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                        "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                                                        "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "Nurse", 
                                              ifelse(Occupation_Code_y2 %in% c("N2C", "N2J", "N2L"),"x.midwife",
                                                     ifelse(Occupation_Code_y2 %in% "N3H", "x.health_visitor",
                                                            ifelse(Occupation_Code_y2 %in% c('H1A','H1B','H1C','H1D','H1E','H1F','H1P','H2A',
                                                                                             'H2B','H2C','H2D','H2E','H2F','H2P','N8A','N8B',
                                                                                             'N8C','N8D','N8E','N8F','N8G','N8H','N8K','N8L',
                                                                                             'N9A','N9B','N9C','N9D','N9E','N9F','N9G','N9H',
                                                                                             'N9J','N9K','N9L','NFA','NFB','NFC','NFD','NFE',
                                                                                             'NFF','NFG','NFH','NFJ','NFK','NFL','NGA','NGB',
                                                                                             'NGC','NGD','NGE','NGF','NGG','NGH','NHA','NHB',
                                                                                             'NHC','NHD','NHE','NHF','NHG','NHH'), "Support","x.Other")))))]
#Creating new variable to preserve original status
Raw_Data_y2[, Status_orig_y2 := Status_y2]
#Creating new variable  for active vs non active
Raw_Data_y2[, Status_y2 := ifelse(Status_y2 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active")]
#Creating new variable  for contract type
Raw_Data_y2[, Asg_Type_Of_Contract_y2 := ifelse(Asg_Type_Of_Contract_y2 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")]
#filtering to only permanent contract type
Raw_Data_y2 <- subset(Raw_Data_y2, Asg_Type_Of_Contract_y2=='Permanent/fixed term/locum')
#Overriding NAs with Unknown
Raw_Data_y2[, Nationality_grouping_y2 := ifelse(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')]
#Creating another nationality variable grouping for IR vs Domestic split
Raw_Data_y2[, Nationality_grouping_y2_v2 := ifelse(Nationality_grouping_y2 %in% c('ROW','EU'), 'IR',
                                                   ifelse(Nationality_grouping_y2 %in% c('UK','Unknown'),'Domestic','Other'))]


#dataset to check if a nurse previously worked as something other than a nurse within the year
#filtering to only permanent contract type
occ_check <- subset(occ_check, Asg_Type_Of_Contract %in% c("Locum", "Fixed Term Temp", "Permanent"))
#filtering to active status 
occ_check <- subset(occ_check, Status %in% c("Active Assignment", "Internal Secondment", "Acting Up"))
#filtering to nurses only
occ_check <- subset(occ_check, Occupation_Code %!in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L",
                                                       "N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                       "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                       "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                       "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                       "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL",
                                                       "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"))
#select only variables needed
occ_check <- subset(occ_check, select = c(Unique_Nhs_Identifier, Occupation_Code, Contracted_Wte, NHSD_trust_or_CCG))
#Overriding NAs with Unknown
occ_check[, NHSD_trust_or_CCG := ifelse(is.na(NHSD_trust_or_CCG)==FALSE,NHSD_trust_or_CCG,0)]
#A flag for non-nurses
occ_check[, Non_nurse := 1]
#Staff group flag for non-nurses
occ_check[, Staff_group := ifelse(substr(Occupation_Code, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical",
                                  ifelse(Occupation_Code %in% c("N2C", "N2J", "N2L"),"x.midwife",
                                         ifelse(Occupation_Code %in% "N3H", "x.health_visitor",
                                                ifelse(Occupation_Code %in% c('H1A','H1B','H1C','H1D','H1E','H1F','H1P','H2A',
                                                                              'H2B','H2C','H2D','H2E','H2F','H2P','N8A','N8B',
                                                                              'N8C','N8D','N8E','N8F','N8G','N8H','N8K','N8L',
                                                                              'N9A','N9B','N9C','N9D','N9E','N9F','N9G','N9H',
                                                                              'N9J','N9K','N9L','NFA','NFB','NFC','NFD','NFE',
                                                                              'NFF','NFG','NFH','NFJ','NFK','NFL','NGA','NGB',
                                                                              'NGC','NGD','NGE','NGF','NGG','NGH','NHA','NHB',
                                                                              'NHC','NHD','NHE','NHF','NHG','NHH'), "Support","x.Other"))))]

###########NMC data checks###########
#Conclusion: need to merge on pay data to see if there's miscoding of nurses, which may explain why there are a few thousand missing PINs
#3-6k missing NMC status
Raw_Data_y1 %>%
  filter(Staff_group_y1=='Nurse') %>%
  filter(Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
  group_by(Registration_Status_y1) %>%
  summarise (sum(Contracted_Wte_y1))

Raw_Data_y2 %>%
  filter(Staff_group_y2=='Nurse') %>%
  filter(Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
  group_by(Registration_Status_y2) %>%
  summarise (sum(Contracted_Wte_y2)) 

#check how many with missing PINs
#Raw_Data_y1 %>%
#  filter(Staff_group_y1=='Nurse') %>%
#  mutate(Missing_PIN = ifelse((is.na(Registration_Number_y1)==TRUE| Registration_Number_y1==""),1,0)) %>%
#  summarise (sum(Missing_PIN))

#test1 <- Raw_Data_y1 %>%
#  filter(Staff_group_y1=='Nurse') %>%
#  mutate(Missing_PIN = ifelse((is.na(Registration_Number_y1)==TRUE| Registration_Number_y1==""),1,0)) %>%
#  filter(Missing_PIN==1)

#Raw_Data_y1 %>%
#  filter(Staff_group_y1=='Nurse') %>%
#  mutate(Missing_PIN = ifelse((is.na(Registration_Number_y1)==TRUE| Registration_Number_y1==""),1,0)) %>%
  #group_by(NHSD_trust_or_CCG_y1) %>%
  #group_by(Status_y1) %>%
  #group_by(Occupation_Code_y1) %>%
  #group_by(Ocs_Code_y1) %>%
  #summarise (sum(Missing_PIN))

#rm(test)

#Raw_Data_y2 %>%
#  filter(Staff_group_y2=='Nurse') %>%
#  mutate(Missing_PIN = ifelse((is.na(Registration_Number_y2)==TRUE| Registration_Number_y2==""),1,0)) %>%
#  summarise (sum(Missing_PIN))

#test2 <- Raw_Data_y2 %>%
#  filter(Staff_group_y2=='Nurse') %>%
#  mutate(Missing_PIN = ifelse((is.na(Registration_Number_y2)==TRUE| Registration_Number_y2==""),1,0)) %>%
#  filter(Missing_PIN==1)

#Raw_Data_y2 %>%
#  filter(Staff_group_y2=='Nurse') %>%
#  mutate(Missing_PIN = ifelse((is.na(Registration_Number_y2)==TRUE| Registration_Number_y2==""),1,0)) %>%
  #group_by(NHSD_trust_or_CCG_y2) %>%
  #group_by(Status_y2) %>%
  #group_by(Occupation_Code_y2) %>%
  #group_by(Ocs_Code_y2) %>%
#  summarise (sum(Missing_PIN))

#rm(test1,test2,test)
############################################

###########NMC data wrangling###########
#filter down to active NMC status and get rid of missing PINs for now
Raw_Data_y1[, Missing_PINs := ifelse((is.na(Registration_Status_y1)==TRUE | Registration_Status_y1=="" & Staff_group_y1=='Nurse'), 1,0)]
Raw_Data_y1[, Inactive_NMC_status := ifelse((Registration_Status_y1=="Expired" & Staff_group_y1=='Nurse'),1,0)]
#filtering out missing PINs and inactive status
#Raw_Data_y1 <- subset(Raw_Data_y1, Missing_PINs==0 & Inactive_NMC_status==0)
Raw_Data_y1 <- subset(Raw_Data_y1, Inactive_NMC_status==0)

#filter down to active NMC status and get rid of missing PINs for now
Raw_Data_y2[, Missing_PINs := ifelse((is.na(Registration_Status_y2)==TRUE | Registration_Status_y2=="" & Staff_group_y2=='Nurse'), 1,0)]
Raw_Data_y2[, Inactive_NMC_status := ifelse((Registration_Status_y2=="Expired" & Staff_group_y2=='Nurse'),1,0)]
#filtering out missing PINs and inactive status
#Raw_Data_y2 <- subset(Raw_Data_y2, Missing_PINs==0 & Inactive_NMC_status==0)
Raw_Data_y2 <- subset(Raw_Data_y2, Inactive_NMC_status==0)
############################################

#group to remove trust level dups
Raw_Data_y1 <- Raw_Data_y1[,sum(Contracted_Wte_y1),by=c("Unique_Nhs_Identifier", "Tm_Year_Month_y1" ,"Status_y1", "Nationality_y1", "NHSD_trust_or_CCG_y1", "Nationality_grouping_y1", "Staff_group_y1", "Nationality_grouping_y1_v2", "Registration_Number_y1")]
Raw_Data_y2 <- Raw_Data_y2[,sum(Contracted_Wte_y2),by=c("Unique_Nhs_Identifier", "Tm_Year_Month_y2" ,"Status_y2", "Nationality_y2", "NHSD_trust_or_CCG_y2", "Nationality_grouping_y2", "Staff_group_y2", "Nationality_grouping_y2_v2", "Registration_Number_y2")]

#rename created FTE variable
setnames(Raw_Data_y1,"V1","Contracted_Wte_y1")
setnames(Raw_Data_y2,"V1","Contracted_Wte_y2")

#ordering data by fte and then staff group
Raw_Data_y1 <- Raw_Data_y1[order(Raw_Data_y1$Staff_group_y1,Raw_Data_y1$Status_y1,-Raw_Data_y1$Contracted_Wte_y1),]
Raw_Data_y2 <- Raw_Data_y2[order(Raw_Data_y2$Staff_group_y2,Raw_Data_y2$Status_y2,-Raw_Data_y2$Contracted_Wte_y2),]
occ_check <- occ_check[order(-occ_check$Contracted_Wte),]

#removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
Raw_Data_y1_dedup <- Raw_Data_y1[ !duplicated(Raw_Data_y1$Unique_Nhs_Identifier), ]
Raw_Data_y2_dedup <- Raw_Data_y2[ !duplicated(Raw_Data_y2$Unique_Nhs_Identifier), ]
occ_check <- occ_check[ !duplicated(occ_check$Unique_Nhs_Identifier), ]

#set keys and join datasets (full join on y1 and y2 datasets to check joiners and leavers and left join on occ_check)
setkey(Raw_Data_y1_dedup, "Unique_Nhs_Identifier")
setkey(Raw_Data_y2_dedup, "Unique_Nhs_Identifier")
Data <- merge(Raw_Data_y1_dedup, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier", all=TRUE)

setkey(Data, "Unique_Nhs_Identifier")
setkey(occ_check, "Unique_Nhs_Identifier")
Data <- occ_check[Data]

#get rid of unsused data
rm(occ_check,Raw_Data_y1_dedup)

#merge nationality into a single field and override NAs with Unknowns/ 0s
Data[, Nationality := ifelse(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)]
Data[, Nationality_grouping := ifelse(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)]
Data[, Nationality_grouping := ifelse(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)]
Data[, Nationality_grouping_v2 := ifelse(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                         ifelse(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))]
Data[, NHSD_trust_or_CCG_y1 := ifelse(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)]
Data[, NHSD_trust_or_CCG_y2 := ifelse(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0)]
Data[, Non_nurse := ifelse(is.na(Non_nurse) == FALSE, Non_nurse, 0)]
Data[, NHSD_trust_or_CCG := ifelse(is.na(NHSD_trust_or_CCG) == FALSE, NHSD_trust_or_CCG, 0)]
Data <- subset(Data, select = c(-Contracted_Wte,-Occupation_Code))


#joiner/ leaver flags
#old joiner definition
#Data[, joiner_old := ifelse(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)]
#direct joiner
Data[, joiner := ifelse(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1" & Non_nurse==0, Contracted_Wte_y2, 0)]
#old occ joiner definition
#Data[, occ_joiner_old := ifelse(Staff_group_y1 != "Nurse" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)]
#occupational joiner
Data[, occ_joiner := ifelse((Staff_group_y1 != "Nurse"
                             & NHSD_trust_or_CCG_y1 == "1"
                             & Staff_group_y2 == c("Nurse")
                             & Status_y2 %in% c("Active")
                             & NHSD_trust_or_CCG_y2 == "1") | (is.na(Staff_group_y1) == TRUE
                                                               & Staff_group_y2 == c("Nurse")
                                                               & Status_y2 %in% c("Active")
                                                               & NHSD_trust_or_CCG_y2 == "1"
                                                               & Non_nurse==1
                                                               & NHSD_trust_or_CCG==1), Contracted_Wte_y2, 0)]
Data[, occ_joiner_support := ifelse((Staff_group_y1 != "Nurse"
                                     & Staff_group_y1 == 'Support'
                                     & NHSD_trust_or_CCG_y1 == "1"
                                     & Staff_group_y2 == c("Nurse")
                                     & Status_y2 %in% c("Active")
                                     & NHSD_trust_or_CCG_y2 == "1") | (is.na(Staff_group_y1) == TRUE
                                                                       & Staff_group_y2 == c("Nurse")
                                                                       & Status_y2 %in% c("Active")
                                                                       & NHSD_trust_or_CCG_y2 == "1"
                                                                       & Non_nurse==1
                                                                       & Staff_group=='Support'
                                                                       & NHSD_trust_or_CCG==1), Contracted_Wte_y2, 0)]
Data[, occ_joiner_other := ifelse((Staff_group_y1 != "Nurse"
                                   & Staff_group_y1 != "Support"
                                   & NHSD_trust_or_CCG_y1 == "1"
                                   & Staff_group_y2 == c("Nurse")
                                   & Status_y2 %in% c("Active")
                                   & NHSD_trust_or_CCG_y2 == "1") | (is.na(Staff_group_y1) == TRUE
                                                                     & Staff_group_y2 == c("Nurse")
                                                                     & Status_y2 %in% c("Active")
                                                                     & NHSD_trust_or_CCG_y2 == "1"
                                                                     & Non_nurse==1
                                                                     & Staff_group!='Support'
                                                                     & NHSD_trust_or_CCG==1), Contracted_Wte_y2, 0)]
#maternity / career breaks/ within NHS/ other joiners
Data[, non_active_to_active := ifelse(Staff_group_y1 == "Nurse" & Status_y1 != "Active" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == "Nurse" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)]
Data[, nhs_provider_joiner := ifelse(Staff_group_y1 == "Nurse" & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == "Nurse" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)]
Data[, other_joiner := ifelse(Staff_group_y1 != "Nurse" & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == "Nurse" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)]
#old leaver definition
#Data[, leaver_old := ifelse(is.na(Staff_group_y2) == TRUE & Staff_group_y1 %in% c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)]
#leaver
Data[, leaver := ifelse(is.na(Staff_group_y2) == TRUE & Staff_group_y1 %in% c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1" & Non_nurse==0, Contracted_Wte_y1*-1, 0)]
#old occ leaver definition
#Data[, occ_leaver_old := ifelse(Staff_group_y2 != "Nurse" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)]
#occupational leaver
Data[, occ_leaver := ifelse((Staff_group_y2 != "Nurse"
                             & NHSD_trust_or_CCG_y2 == "1"
                             & Staff_group_y1 == c("Nurse")
                             & Status_y1 %in% c("Active")
                             & NHSD_trust_or_CCG_y1 == "1") | (is.na(Staff_group_y2) == TRUE
                                                               & Staff_group_y1 == c("Nurse")
                                                               & Status_y1 %in% c("Active")
                                                               & NHSD_trust_or_CCG_y1 == "1"
                                                               & Non_nurse==1
                                                               & NHSD_trust_or_CCG==1) , Contracted_Wte_y1*-1, 0)]
#maternity / career breaks/ within NHS/ other leavers
Data[, active_to_non_active := ifelse(Staff_group_y2 == "Nurse" & Status_y2 != "Active" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)]
Data[, nhs_provider_leaver := ifelse(Staff_group_y2 == "Nurse" & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == "Nurse" & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)]
Data[, other_leaver := ifelse(Staff_group_y2 != "Nurse" & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == "Nurse" & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)]
#FTE change
Data[, FTE_change := ifelse(Staff_group_y2 == "Nurse" & Staff_group_y1 == "Nurse" & Status_y1 == "Active" & Status_y2 == "Active" & NHSD_trust_or_CCG_y1 == "1" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2-Contracted_Wte_y1, 0)]

#we can identify NA to RN from occ codes, review
#Data %>% summarise (sum(joiner),sum(occ_joiner),sum(occ_joiner_support),sum(occ_joiner_other))
#Data %>% filter (occ_joiner_other>0) %>% group_by(Staff_group) %>% summarise (sum(occ_joiner_other))

#override NAs in joiner/ leaver flags
Data[, nhs_provider_joiner := ifelse(is.na(nhs_provider_joiner)==FALSE,nhs_provider_joiner,0)]
Data[, other_joiner := ifelse(is.na(other_joiner)==FALSE,other_joiner,0)]
Data[, nhs_provider_leaver := ifelse(is.na(nhs_provider_leaver)==FALSE,nhs_provider_leaver,0)]
Data[, other_leaver := ifelse(is.na(other_leaver)==FALSE,other_leaver,0)]

#set keys and join Y1 and 2 datasets to overwrite Y1 nationality to take the latest nationality field (left joins)
#Join datasets to overwrite Y1 nationality
setkey(Raw_Data_y1, "Unique_Nhs_Identifier")
setkey(Raw_Data_y2_dedup, "Unique_Nhs_Identifier")
Raw_Data_y1 <- Raw_Data_y2_dedup[Raw_Data_y1]

#get rid of unsused data
rm(Raw_Data_y2_dedup)

#overwrite Y1 nationality
Raw_Data_y1[, Nationality_grouping_y1_v2_2 := ifelse(is.na(Nationality_grouping_y2_v2)==FALSE,Nationality_grouping_y2_v2,Nationality_grouping_y1_v2)]
Raw_Data_y1[, Nationality_grouping_y1_v2 := ifelse(is.na(Nationality_grouping_y2)==FALSE,Nationality_grouping_y2,Nationality_grouping_y1)]

#take latests PIN number
Data[, PIN := ifelse(is.na(Registration_Number_y2)==FALSE,Registration_Number_y2,Registration_Number_y1)]

#NMC PIN number includes where a Nurse qualified (last letter: E, S, I ,W (UK) O (Overseas) 
#Numbers show Year and letter A-L Month of qualification Jan-Dec, plus Y Return to Practice.

NMC_test <- Data %>%
  mutate (Reg_year = (substr(PIN, 1, 2))) %>%
  mutate(Reg_month = (substr(PIN, 3, 3))) %>%
  mutate(Reg_country = (substr(PIN, 8, 8)))

a <- NMC_test %>% group_by(Reg_year) %>% summarise(sum(joiner))
a <- NMC_test %>% group_by(Reg_month) %>% summarise(sum(joiner))
a <- NMC_test %>% group_by(Reg_country) %>% summarise(sum(joiner))

a <- NMC_test %>% filter(Reg_country=="O" | Reg_country=="o" ) %>% group_by(Nationality_grouping) %>% summarise(sum(joiner))
a <- NMC_test %>% filter(Nationality_grouping_v2=='IR') %>% group_by(Reg_country) %>% summarise(sum(joiner), sum(occ_joiner))

a <- NMC_test %>% filter(Nationality_grouping_v2=='Domestic') %>% group_by(Reg_year) %>% summarise(sum(joiner), sum(occ_joiner))

a <- NMC_test %>% filter(PIN!="")

a <- NMC_test %>% filter(Reg_year=="C1")

###################################joiner/ leaver summaries###################################
#Total joiners/ leavers
summary <- Data %>%
  summarise (joiner=sum(joiner),
             occ_joiner=sum(occ_joiner),
             non_active_to_active=sum(non_active_to_active),
             nhs_provider_joiner=sum(nhs_provider_joiner),
             other_joiner=sum(other_joiner),
             leaver=sum(leaver),
             occ_leaver=sum(occ_leaver),
             active_to_non_active=sum(active_to_non_active),
             nhs_provider_leaver=sum(nhs_provider_leaver),
             other_leaver=sum(other_leaver),
             FTE_change=sum(FTE_change)
  )

#insert nationality column
summary <- summary %>% mutate (Nationality_grouping="All") %>% select (12,1:11)

#Split by nationality
summary_nat <- Data %>%
  group_by(Nationality_grouping) %>%
  summarise (joiner=sum(joiner),
             occ_joiner=sum(occ_joiner),
             non_active_to_active=sum(non_active_to_active),
             nhs_provider_joiner=sum(nhs_provider_joiner),
             other_joiner=sum(other_joiner),
             leaver=sum(leaver),
             occ_leaver=sum(occ_leaver),
             active_to_non_active=sum(active_to_non_active),
             nhs_provider_leaver=sum(nhs_provider_leaver),
             other_leaver=sum(other_leaver),
             FTE_change=sum(FTE_change)
  )

summary_nat_group <- Data %>%
  group_by(Nationality_grouping_v2) %>%
  summarise (joiner=sum(joiner),
             occ_joiner=sum(occ_joiner),
             non_active_to_active=sum(non_active_to_active),
             nhs_provider_joiner=sum(nhs_provider_joiner),
             other_joiner=sum(other_joiner),
             leaver=sum(leaver),
             occ_leaver=sum(occ_leaver),
             active_to_non_active=sum(active_to_non_active),
             nhs_provider_leaver=sum(nhs_provider_leaver),
             other_leaver=sum(other_leaver),
             FTE_change=sum(FTE_change)
  )

#rename nationality grouping to match above summaries
summary_nat_group <- rename (summary_nat_group, Nationality_grouping=Nationality_grouping_v2)

#combine total with nat split
summary <- bind_rows(summary_nat,summary,summary_nat_group) 

#remove helper tables
rm(summary_nat,summary_nat_group)


###################################FTE summaries###################################
#FTE - year 1
#total
FTE_y1_1 <- Raw_Data_y1 %>%
  filter(Staff_group_y1 %in% c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
  summarise (FTE_y1 = sum(Contracted_Wte_y1))

FTE_y1_1 <- FTE_y1_1 %>% mutate (Nationality_grouping='All')

#nationality split 1
FTE_y1_2 <- Raw_Data_y1 %>%
  filter(Staff_group_y1 %in% c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
  #mutate (Nationality_grouping_y1 = if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
  group_by(Nationality_grouping_y1_v2) %>%
  summarise (FTE_y1 = sum(Contracted_Wte_y1))

FTE_y1_2 <- rename (FTE_y1_2, Nationality_grouping=Nationality_grouping_y1_v2)

#nationality split 2
FTE_y1_3 <- Raw_Data_y1 %>%
  filter(Staff_group_y1 %in% c("Nurse") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
  group_by(Nationality_grouping_y1_v2_2) %>%
  summarise (FTE_y1 = sum(Contracted_Wte_y1))

FTE_y1_3 <- rename (FTE_y1_3, Nationality_grouping=Nationality_grouping_y1_v2_2)

#combine all FTE
FTE_y1 <- bind_rows(FTE_y1_2,FTE_y1_1,FTE_y1_3) 


#FTE - year 2
#total
FTE_y2_1 <- Raw_Data_y2 %>%
  filter(Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
  summarise (FTE_y2 = sum(Contracted_Wte_y2))

FTE_y2_1 <- FTE_y2_1 %>% mutate (Nationality_grouping='All')

#nationality split 1
FTE_y2_2 <- Raw_Data_y2 %>%
  filter(Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
  #mutate (Nationality_grouping_y2 = if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
  group_by(Nationality_grouping_y2) %>%
  summarise (FTE_y2 = sum(Contracted_Wte_y2))

FTE_y2_2 <- rename (FTE_y2_2, Nationality_grouping=Nationality_grouping_y2)

#nationality split 2
FTE_y2_3 <- Raw_Data_y2 %>%
  filter(Staff_group_y2 %in% c("Nurse") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
  group_by(Nationality_grouping_y2_v2) %>%
  summarise (FTE_y2 = sum(Contracted_Wte_y2))

FTE_y2_3 <- rename (FTE_y2_3, Nationality_grouping=Nationality_grouping_y2_v2)

#combine all FTE
FTE_y2 <- bind_rows(FTE_y2_2,FTE_y2_1,FTE_y2_3) 


###################################Final combined summary output###################################
#combine joiners/ leavers with FTE
summary <- bind_cols(summary,FTE_y1,FTE_y2) %>%
  select (1:12,14,16)

#remove helper tables
rm(FTE_y1_1,FTE_y1_2,FTE_y1_3,FTE_y2_1,FTE_y2_2,FTE_y2_3,FTE_y1,FTE_y2)

#leaver rates
summary <- summary %>%
  mutate(leaver_rate = as.numeric(leaver)/as.numeric(FTE_y1)) %>%
  mutate(leaver_rate_occ = (as.numeric(occ_leaver))/as.numeric(FTE_y1))

#pivot data into long format
pivot <- pivot_longer(summary, c(2:16))


###################################Pull joiner/ leaver period name###################################
#extract joiner/ leaver period name
pivot_final <- pivot
colnames(pivot_final) <- c("Nationality_grouping", "name", paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2]))

#export to shared area with today's date
write.csv(pivot_final, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/",str_replace_all(Sys.Date(),"-",""),"joiners_leavers",long_format$Date_from[1],".csv"))

##preparing extract for nurses dashboard time series
#long_format <- pivot %>%
  mutate(Estimated = value) %>%
  mutate(Estimated_name = paste(name, " estimated")) %>%
  mutate(value=0) %>%
  mutate(Date_from = paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2])) %>%
  mutate(Date = as.Date(paste0(substr(Date_from,13,16),"-", substr(Date_from,6,8), "-01"),"%Y-%b-%d")) %>%
  select(1,6,2,3,7,4,5)

#export to shared area with today's date
#write.csv(long_format, paste("C:/R/20200918 Nurse analysis/",str_replace_all(Sys.Date(),"-",""),"joiners_leavers_dashboard",long_format$Date_from[1],".csv"))

