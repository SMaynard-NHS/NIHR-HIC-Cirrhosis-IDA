# # Name: S Maynard
# Project: Liver HIC Project i 
# Version Number: V2.0
# Date: 2025/04/04
# File type - .R
# Project_i_liver_IDA_classification ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Data Source - Project i
# Run project i files for data 

# Load Packages
if(F){
  install.packages("readr")
  install.packages("data.table")
  install.packages("tidyverse")
  install.packages("rmarkdown")
  install.packages("tableone")
}

library(readr)
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(tableone)

# Turn off scientific notation
options(scipen = 999)

## IMPORT DATA
icd_diagnosis.data <- read_csv("~/Project i/Data/project_i_icd_diagnosis.csv", col_names = T)
Demographics.data <- read.csv("~/Project i/Data/project_i_demographics.csv")
Demographics.definitions <- read.csv("~/Project i/Data/definitions/dt_ethnic_category.csv")
riskfactor.data <- read.csv("~/Project i/Data/project_i_risk_factor.csv") 
Labtest.data <- read.csv("~/Project i/Data/project_i_lab_test.csv")
death.data <- read.csv("~/Project i/Data/project_i_death.csv")


## COHORT SELECTION

### Cirrhosis ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1,645009 obs (Nov 2024)

## Date and time
#Convert date time format for diagnosis date and admission date
icd_diagnosis.data$diagnosis_date <- ymd(icd_diagnosis.data$diagnosis_date)
icd_diagnosis.data$admission_date <- ymd(icd_diagnosis.data$admission_date)

# Unique study ID within ICD codes - 28,694 (Nov 2024) patients
length(unique(icd_diagnosis.data$study_subject_id))

# Column NAs
na_counts <- icd_diagnosis.data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "NA_Count")
print(na_counts)

rm(na_counts)

# NB 781017 (nov 2024) have no diagnosis date
# 859044 have no admission date

# Take earliest as 'first record of diagnosis'
# Pmin provides earliest date ignoring NAs and delivers this 

icd_diagnosis.data %>%
  mutate(earliest_diagnosis = pmin(diagnosis_date, admission_date, na.rm = T)
  ) -> icd_diagnosis.data

sum(is.na(icd_diagnosis.data$earliest_diagnosis))

# Now only 30,173 without diagnosis date

# 3 methods of diagnosis - ICD10 code/ SNOMED/ description
# No. of patient entries missing
# 1. ICD_10 code = 30615
# 2. snomed_code = 1584459
# 3. description = 384453

# Exclude those which have NA in ALL (i.e insufficient data for diagnosis/exclusion of cirrhosis)

icd_diagnosis.data %>%
  mutate (exclude = if_else(is.na(icd_code) & is.na(snomed_code) & is.na(diagnosis_description), T, F)
  ) -> icd_diagnosis.data
table(icd_diagnosis.data$exclude) # No observations have NA for all

# Remove exclude column
icd_diagnosis.data %>%
  select(-exclude) -> icd_diagnosis.data

# New column to icd_diagnosis.data with patients who have diagnosis in keeping cirrhosis from:

# 1. ICD 10 code ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

table(icd_diagnosis.data$icd_code)

# ICD codes in keeping cirrhosis 
icd_codes <- c("I85", "I850", "I859","I864", "I982", "I983", "K703", "K717","K744","K745","K746", "K766", "K767", "R18") # NB K74 removed as no 'K74' in isolation
pattern <- paste0("(?i)", paste(icd_codes, collapse = "|")) # (?i) makes it case-insensitive

# At individual icd code level
icd_diagnosis.data <- icd_diagnosis.data %>%
  mutate(
    # Check if any Icd code matches the pattern and assign binary indicator variable if TRUE
    cirrhosis_icd = if_else(
      str_detect(str_replace_all(icd_code,"[.xX]",""),  pattern),
      1L,  # Snomed cirrho code match
      0L   # No snomed cirrho code match
    )
  )

icd_diagnosis.data <- icd_diagnosis.data %>%
  mutate(
    # Check if any Icd code matches the pattern and pull in earliest date if TRUE
    cirrhosis_icd_date = if_else(
      str_detect(str_replace_all(icd_code,"[.xX]",""),  pattern),
      earliest_diagnosis,  # Icd cirrho code match
      as.Date(NA)   # No Icd cirrho code match
    )
  )

# Check unique patients captured with icd code in keeping cirrhosis

table(icd_diagnosis.data$cirrhosis_icd)

icd_diagnosis.data %>%
  group_by(cirrhosis_icd) %>%
  summarise(patient_count = n_distinct(study_subject_id)) -> cirrhosis_by_icd # 5271 patients have cirrhosis using icd10 codes

# check what codes
icd_diagnosis.data %>%
  filter(cirrhosis_icd == T) %>%
  group_by(icd_code) %>%
  summarise(observations = n()) -> cirrhosis_code_count 

# K74 codes excluded other than K744, K745, K746 (K740 Hepatic fibrosis and # K743 Primary Biliary Cirrhosis,
# K741 Hepatic sclerosis (n=4) K742 Hepatic fibrosis with hepatic sclerosis (n=6) --> removes only 4 unique patients)

# tidy environment
rm(cirrhosis_by_icd)
rm(cirrhosis_code_count)

# 2. Description -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

table(icd_diagnosis.data$diagnosis_description)

description_in <- 'end stage liver disease|cirrho|chronic hepatic failure|varice|portal hypertension|ascit|hepatic encephalopathy|hepatorenal syndrome|child pugh|meld'
description_out <- 'Genital varices in pregnancy|Exposure to varicella|Hepatic fibrosis|Hepatic fibrosis with hepatic sclerosis|Malignant ascites|Pelvic varices|Requires varicella vaccination|Scrotal varices|
VZV - Varicella-zoster virus|Varicella|Varicella encephalitis|Varicella pneumonitis|Varicella without complication|Varicella-zoster virus infection|Vulval varices|Sublingual varices|
Primary biliary cirrhosis'

# Convert the exclude_pattern to exact matches for each term
exact_description_out <- description_out %>%
  str_split("\\|", simplify = TRUE) %>%                # Split the string by "|"
  paste0("^", ., "$") %>%                              # Wrap each term in ^ and $
  paste(collapse = "|")                                # Collapse back into a single string

include_pattern <- regex(description_in, ignore_case = T)
exclude_pattern <- regex(exact_description_out, ignore_case = T)

# At individual diagnosis level to generate binary indicator variable
icd_diagnosis.data <- icd_diagnosis.data %>%
  mutate(
    # Check if any diagnosis matches the inclusion pattern after exclusion of exact patterns
    cirrhosis_desc = if_else(
      str_detect(diagnosis_description, include_pattern) & 
        !str_detect(diagnosis_description, exclude_pattern),
      1L,  # Include patient if any diagnosis matches after exclusion
      0L   # Exclude patient if none of the diagnoses match
    )
  )

# At individual diagnosis level to pull date of diagnosis
icd_diagnosis.data <- icd_diagnosis.data %>%
  mutate(
    # Check if any diagnosis matches the inclusion pattern after exclusion of exact patterns
    cirrhosis_desc_date = if_else(
      str_detect(diagnosis_description, include_pattern) & 
        !str_detect(diagnosis_description, exclude_pattern),
      earliest_diagnosis,  # Date if any diagnosis matches after exclusion
      as.Date(NA)   # NA if none of the diagnoses match
    )
  )


# Check unique patients who have diagnosis in keeping cirrhosis

table(icd_diagnosis.data$cirrhosis_desc)
icd_diagnosis.data %>%
  group_by(cirrhosis_desc) %>%
  summarise(patient_count = n_distinct(study_subject_id)) -> cirrhosis_by_desc # 3389 patients have a diagnosis that meets inclusion criteria after applying exclusion criteria (by both methods!!!)

# check what descriptions (when looking at individual diagnoses)
icd_diagnosis.data %>%
  filter(cirrhosis_desc == 1) %>%
  group_by(diagnosis_description) %>%
  summarise(observations = n()) -> cirrhosis_description_count 

# tidy environment
rm(cirrhosis_by_desc, cirrhosis_description_count)



# 3. Snomedct ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

table(icd_diagnosis.data$snomed_code)

# original snomedct codes (n= 294)
#snomed_cirrho <- "72541600|707420003|776981000000103|1010616001|78208005|109819003|111370006|123604002|123716002|197291001|197301006|197553002|19943007|235894003|235896001|266469006|271440004|308129003|309783001|
#1082601000119104|1092801000119102|123605001|123606000|12368000|123717006|128072003|15999000|16070004|197293003|197294009|197296006|197299004|197303009|197305002|197306001|197362001|21861000|235895002|
#235897005|266468003|266470007|266471006|27156006|33144001|347891000119103|371139006|37688005|419728003|420054005|43904005|45256007|470971000000107|589551000000103|671731000000104|699189004|702377007|
#710066005|710067001|725938001|735733008|74669004|831000119103|86454000|871619002|89580002|897004000|1259465009|1263507006|235886005|1197694007|1197696009|1197697000|1197698005|1197700001|1197736003|
#197279005|24807004|235881000|34742003|59927004|14223005|173617001|173621008|173661009|195476002|213230009|213231008|906081000006105|399126000|28670008|173660005|197284004|29633007|17709002|195474004|
#173641003|51292008|173639004|197300007|195475003|67364009|88469006|675401000000103|1763641000006109|173679008|13920009|20415001|303082009|123604002|12368000|14223005|15999000|173617001|589511000000102|
#235880004|41309000|307757001|235899008|235901004|1082611000119101|1085021000119106|1085091000119108|146371000119104|149322009|149326007|149327003|153091000119109|155479001|155809006|155811002|155814005|
#155821005|16093661000119105|173618006|173622001|173623006|188446007|195473005|195477006|195478001|195479009|195483009|195643006|197280008|197290000|197292008|197295005|197298007|197302004|197307005|
#197308000|197309008|197311004|235184005|235201007|235212007|235213002|235891006|235892004|236004002|236067006|264974007|265349006|265863006|266467008|266472004|266537000|312980002|38436007|417371000000102|
#424340000|446739005|446742004|449902003|59701000119109|62216007|652571000000102|663501000000108|696141000000105|699074005|713652000|721160006|721206006|722867009|73282006|75393009|764962002|843571000000100|
#426841006|173621008|173639004|173641003|173660005|173661009|17709002|195474004|195475003|195476002|197279005|197291001|197293003|197294009|197296006|197299004|197300007|197301006|197303009|197305002
##|197310003|197362001|19943007|20415001|213231008|21861000|235881000|235886005|235895002|235896001|235897005|266468003|266469006|266470007|266471006|271440004|28670008|308129003|309783001|34742003|419728003|
#426841006|43904005|51292008|67364009|7.77E+14|91109007|1.24E+15|230364000|91109007|22508003|713542007|717865007|227071000000107|256971000000107|662331000000101|1148573000|1197664001|31005002|13124004|
#140519000|140521005|140522003|140523008|140524002|150557006|158530005|163309001|163311005|163312003|163313008|163314002|178012008|178013003|178014009|207252009|207254005|274433001|302455000|307311001|
#389026000|397041000000101|442039000|470757009|470758004|499461000000105|533781000000107|539081000000106|89305009"

# 245 unique codes (removed 49 repeats)
snomed_cirrho <- "72541600|707420003|776981000000103|1010616001|78208005|109819003|111370006|123604002|123716002|197291001|197301006|197553002|19943007|235894003|235896001|266469006|271440004|308129003|309783001|425413006|536002|589541000000101|589561000000100|6183001|662341000000105|708248004|710065009|715864007|716203000|725939009|725940006|76301009|778131000000103|897005004|103611000119102|10690671000119109|1082601000119104|1092801000119102|123605001|123606000|12368000|123717006|128072003|15999000|16070004|197293003|197294009|197296006|197299004|197303009|197305002|197306001|197362001|21861000|235895002|235897005|266468003|266470007|266471006|27156006|33144001|347891000119103|371139006|37688005|419728003|420054005|43904005|45256007|470971000000107|589551000000103|671731000000104|699189004|702377007|710066005|710067001|725938001|735733008|74669004|831000119103|86454000|871619002|89580002|897004000|1259465009|1263507006|235886005|1197694007|1197696009|1197697000|1197698005|1197700001|1197736003|197279005|24807004|235881000|34742003|59927004|14223005|173617001|173621008|173661009|195476002|213230009|213231008|906081000006105|399126000|28670008|173660005|197284004|29633007|17709002|195474004|173641003|51292008|173639004|197300007|195475003|67364009|88469006|675401000000103|1763641000006109|173679008|13920009|20415001|303082009|589511000000102|235880004|41309000|307757001|235899008|235901004|1082611000119101|1085021000119106|1085091000119108|146371000119104|149322009|149326007|149327003|153091000119109|155479001|155809006|155811002|155814005|155821005|16093661000119105|173618006|173622001|173623006|188446007|195473005|195477006|195478001|195479009|195483009|195643006|197280008|197290000|197292008|197295005|197298007|197302004|197307005|197308000|197309008|197311004|235184005|235201007|235212007|235213002|235891006|235892004|236004002|236067006|264974007|265349006|265863006|266467008|266472004|266537000|312980002|38436007|417371000000102|424340000|446739005|446742004|449902003|59701000119109|62216007|652571000000102|663501000000108|696141000000105|699074005|713652000|721160006|721206006|722867009|73282006|75393009|764962002|843571000000100|426841006|7.77E+14|91109007|1.24E+15|230364000|22508003|713542007|717865007|227071000000107|256971000000107|662331000000101|1148573000|1197664001|31005002|13124004|140519000|140521005|140522003|140523008|140524002|150557006|158530005|163309001|163311005|163312003|163313008|163314002|178012008|178013003|178014009|207252009|207254005|274433001|302455000|307311001|389026000|397041000000101|442039000|470757009|470758004|499461000000105|533781000000107|539081000000106|89305009"


pattern <- regex(snomed_cirrho, ignore_case = T) 

# At individual snomed code level -> to generate binary indicator variable 1 or 0
icd_diagnosis.data <- icd_diagnosis.data %>%
  mutate(
    # Check if any snomedct matches the pattern
    cirrhosis_snomed = if_else(
      str_detect(snomed_code, pattern),
      1L,  # Snomed cirrho code match
      0L   # No snomed cirrho code match
    )
  )

# At individual snomed code level -> to pull date of diagnosis
icd_diagnosis.data <- icd_diagnosis.data %>%
  mutate(
    # Check if any snomedct matches the pattern
    cirrhosis_snomed_date = if_else(
      str_detect(snomed_code, pattern),
      earliest_diagnosis,  # Snomed cirrho code match
      as.Date(NA)   # No snomed cirrho code match
    )
  )

# Check unique patients who have snomed code in keeping cirrhosis

table(icd_diagnosis.data$cirrhosis_snomed)
icd_diagnosis.data %>%
  group_by(cirrhosis_snomed) %>%
  summarise(patient_count = n_distinct(study_subject_id)) -> cirrhosis_by_snomed # 184 patients have a snomed cirrhosis code

# check what descriptions (when looking at individual diagnoses)
icd_diagnosis.data %>%
  filter(cirrhosis_snomed == 1) %>%
  group_by(snomed_code) %>%
  summarise(observations = n()) -> cirrhosis_snomed_count

# tidy environment
rm(cirrhosis_by_snomed, cirrhosis_snomed_count)


## Group at patient level 
# Columns for icd/desc/snomed diagnosis and date of each
# Additional column with cirrhosis diagnosis (any) and earliest date of recorded diagnosis

# Group by patient_id and aggregate using max to set to 1 if any observation meets inclusion (1)
cirrho_code_pt <- icd_diagnosis.data %>%
  group_by(study_subject_id) %>%
  summarise(
    ptcirrhosis_icd = max(cirrhosis_icd, na.rm = FALSE),
    ptcirrhosis_desc = max(cirrhosis_desc, na.rm = FALSE),
    ptcirrhosis_snomed = max(cirrhosis_snomed, na.rm = FALSE),
    date_diagnosis = if_else(all(is.na(c(cirrhosis_icd_date,cirrhosis_desc_date,cirrhosis_snomed_date))), 
                             NA,  # Replace Inf with NA if all dates are missing
                             as.Date(min(c(cirrhosis_icd_date,cirrhosis_desc_date,cirrhosis_snomed_date), na.rm = TRUE))),  # Get the earliest date if not all NA 
    ptcirrhosis = if_else(max(c(cirrhosis_icd,cirrhosis_desc,cirrhosis_snomed),na.rm=T) == 1, 1, 0)
  )

# View the result
print(cirrho_code_pt)
table(cirrho_code_pt$ptcirrhosis) # 5302 with recorded coded diagnosis in keeping cirrhosis

# How many have no diagnosis date
cirrho_missing_date <- cirrho_code_pt %>%
  group_by(ptcirrhosis) %>%
  summarise(na_count = sum(is.na(date_diagnosis))) # n= 126 pt with cirrhosis missing date of recorded diagnosis

# For merge
cirrho_cohort_diagnosis <- cirrho_code_pt %>%
  select (-c(ptcirrhosis_icd,ptcirrhosis_desc,ptcirrhosis_snomed)) %>%
  filter(ptcirrhosis ==1 )

# tidy environment
rm(cirrho_missing_date)

# CIRRHO COHORT = ALL PATIENTS WITH CIRRHOSIS DIAGNOSIS AT ANY TIME n = 5302


# Extract age  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create d.f. with all pt w/ cirrhosis diagnosis and date of liver Tx (if any)

cirrho_cohort_diagnosis_birth <- cirrho_cohort_diagnosis %>%
  left_join(Demographics.data %>% select(study_subject_id, year_of_birth),
            by = 'study_subject_id'
  )

# Add year turns 18 (ie potential filter for results only after that year)

cirrho_cohort_diagnosis_age <- cirrho_cohort_diagnosis_birth %>%
  mutate(
    year_18 = year_of_birth + 18,
    cutoff_date_18 = make_date(year_18, 12, 31)
  ) %>%
  select(-c(year_18, year_of_birth, ptcirrhosis))

rm(cirrho_cohort_diagnosis_birth)


# Liver transplant extraction -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# SNOMED codes to include (numeric)
snomed_tx_keep <- c(
  18027006, 27280000, 28009009, 33167004, 737297006, 174425003, 174426002,
  174427006, 213153001, 235910007, 235911006, 235912004, 316061004, 426356008,
  428198008, 431222008, 432772009, 432777003, 432908002, 702777009,
  226941000000100, 226951000000102, 233331000000102, 257811000000106,
  257821000000100, 464381000000109, 538521000000107, 595961000000105,
  853761000000103, 885861000000104
)

# Patterns
desc_anyorder <- regex(
  "(liver.*transplant|transplant.*liver|hepat.*transplant|transplant.*hepat)",
  ignore_case = TRUE
)
desc_pre <- regex("pre[-\\s]?transplant|await|planned", ignore_case = TRUE)
icd_pat  <- regex("T86\\.?4|T864|Z94\\.?4|Z944", ignore_case = TRUE) 
# icd 10 codes  T86.4    T864   Z94.4    Z944
#  NOT  Z76.8 (Z76.82 awaiting) or Z01.8 (non-specific examination) or IMO0001 (invalid)

icd_diagnosis.data_transplant <- icd_diagnosis.data %>%
  filter(
    # description path: must match the liver/hepat + transplant pattern,
    # but NOT contain "pre-transplant"
    (
      coalesce(str_detect(diagnosis_description, desc_anyorder), FALSE) &
        !coalesce(str_detect(diagnosis_description, desc_pre), FALSE)
    ) |
      # OR ICD path: contains any of the target codes (dot optional)
      coalesce(str_detect(as.character(icd_code), icd_pat), FALSE) |
      # OR SNOMED path: code is in the provided list
      (snomed_code %in% snomed_tx_keep)
  )


# Add all description relevant to liver transplant to cirrho cohort
# Convert both data frames to data.tables
setDT(icd_diagnosis.data_transplant)
setDT(cirrho_cohort_diagnosis)

# Subset cirrho_cohort to only required columns
cirrho_cohort_subset <- cirrho_cohort_diagnosis[, .(study_subject_id, date_diagnosis)]

# Subset icd transplant data to only required columns
icd_diagnosis.data_transplant_subset <- icd_diagnosis.data_transplant[, .(study_subject_id, diagnosis_date, icd_code, snomed_code, diagnosis_description)]

# Perform efficient join and filtering
cirrho_transplant_cirrhosis <- icd_diagnosis.data_transplant_subset[cirrho_cohort_subset, on = "study_subject_id"]

# Check all cirrho patients included (n = 5302)
length(unique(cirrho_transplant_cirrhosis$study_subject_id))

# Check what descriptions/icd codes/ snomed codes
table(cirrho_transplant_cirrhosis$diagnosis_description)
table(cirrho_transplant_cirrhosis$icd_code)
table(cirrho_transplant_cirrhosis$snomed_code)

# If diagnosis_date is a proper NA (not the string "NA")
cirrho_transplant_cirrhosis %>%
  group_by(study_subject_id) %>%
  summarise(all_dates_na = all(is.na(diagnosis_date)), .groups = "drop") %>%
  filter(all_dates_na) %>%
  summarise(n_patients = n())
# n = 4627 so 675 patients have record relating to a liver transplant

# Endoffu should be date of liver transplant date IF earliest liver transplant is AFTER diagnosis of cirrhosis

# Merge earliest liver transplant record date with cirrho_cohort


# If you want the earliest date from your previously filtered liver_tx_filtered:
tx_dates <- cirrho_transplant_cirrhosis %>%
  # Parse dates robustly if they're character; remove this line if already Date
  mutate(diagnosis_date = as.Date(diagnosis_date)) %>%
  filter(!is.na(diagnosis_date)) %>%
  group_by(study_subject_id) %>%
  summarise(date_transplant = min(diagnosis_date), .groups = "drop")

# Merge onto your cirrhosis cohort (one row per study_subject_id)
cirrho_cohort_diagnosis_age_transplant <- cirrho_cohort_diagnosis_age %>%
  left_join(tx_dates, by = "study_subject_id")

# Add last data extract date
cirrho_cohort_diagnosis_age_transplant <- cirrho_cohort_diagnosis_age_transplant %>%
  mutate(extract_date = dmy("31/08/2024"))

# Final cirrho cohort
cirrho_cohort <- cirrho_cohort_diagnosis_age_transplant %>%
  filter(cutoff_date_18 < extract_date)
# patients not 18 by last data extract n = 1 

# CIRRHO COHORT WITH AGE 18 CUTOFF AND TX DATE = ALL PATIENTS WITH EARLIEST CIRRHOSIS DIAGNOSIS, DATE AGED 18 PLUS EARLIEST RECORDED TRANSPLANT DATE n = 5301


## CLEAN VARIABLES OF INTEREST -----------------------------------------------------------------------------------------------------------------------------------------------------------
### Demographics ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Unique study ID within demographics
length(unique(Demographics.data$study_subject_id))

#### Ethnicity 

# Table of ethnic code id
table(Demographics.data$ethnic_code_id)

library(data.table)
setDT(Demographics.data)
Demographics.data[, ethnic_group := fcase(
  ethnic_code_id %in% c('A','B','C'), 'White',
  ethnic_code_id %in% c('D','E','F','G'), 'Mixed',
  ethnic_code_id %in% c('H','J','K','L','R'), 'Asian',
  ethnic_code_id %in% c('M','N','P'), 'Black',
  ethnic_code_id %in% c('S'), 'Other',
  ethnic_code_id %in% c('Z','99'), 'Not stated'
)]
table(Demographics.data$ethnic_group)

#### Gender 

table(Demographics.data$gender_id)
Demographics.data$gender_id_uncoded <- gsub('1|M|Male', 'Male',Demographics.data$gender_id)
Demographics.data$gender_id_uncoded <- gsub('2|F|Female', 'Female',Demographics.data$gender_id_uncoded)
Demographics.data$gender_id_uncoded <- gsub('9', 'Indeterminate',Demographics.data$gender_id_uncoded)
Demographics.data$gender_id_uncoded <- gsub('X', 'Not Known',Demographics.data$gender_id_uncoded)
table(Demographics.data$gender_id_uncoded)

# For merge 
m_demographics <- Demographics.data %>%
  select(study_subject_id, year_of_birth, ethnic_group, gender_id_uncoded)


# Filter demographics to those who have cirrhosis
cirrho_demographics <- m_demographics %>%
filter(study_subject_id %in% cirrho_cohort$study_subject_id)

# NB no gender indeterminate or missing

# Tidy environment
rm(Demographics.definitions)

### Risk factors ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extract those of cirrhosis cohort
riskfactor_cirrho <- riskfactor.data %>%
  filter(study_subject_id %in% cirrho_cohort$study_subject_id)

table(riskfactor_cirrho$name_id)

# Convert date and value_numeric
riskfactor_cirrho <- riskfactor_cirrho %>%
  mutate(date = as.Date(date_risk_factor_recorded),
         final_value = as.numeric(value_numeric) )

# Explore alcohol consumption
alcohol_cirrho <- riskfactor_cirrho %>%
  filter(name_id == 'Alcohol')

# Alcohol data very hetergenous - unsure of validity

# Explore smoking status 
smoking_cirrho <- riskfactor_cirrho %>%
  filter(name_id == 'Smoking') # %>%
  # summarise(n_distinct(study_subject_id)) # n = 1,769

# Smoking data could be categorised into 2/1/0 smoker/ex-smoker/non-smoker - unsure of clinical utility 
# and at least 50% will be missing
table(smoking_cirrho$value)


# Explore bmi data
bmi_cirrho <- riskfactor_cirrho %>%
  filter(name_id == 'BMI') #%>%
 #summarise(n_distinct(study_subject_id)) # n = 3,428

summary(bmi_cirrho$final_value)
hist(bmi_cirrho$final_value)

bmi_cirrho <- bmi_cirrho %>%
  filter(final_value <= 150)
n_distinct(bmi_cirrho$study_subject_id) # n = 3,296

bmi_cirrho <- bmi_cirrho %>%
  filter(final_value >= 5)
n_distinct(bmi_cirrho$study_subject_id) # n =3,285
  
hist(bmi_cirrho$final_value)  

# Could explore BMI as covariate



### Lab tests all ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extract those of cirrhosis cohort
labtest_cirrho <- Labtest.data %>%
  filter(study_subject_id %in% cirrho_cohort$study_subject_id)

# Unique study ID within lab tests
length(unique(labtest_cirrho$study_subject_id)) # 3,154,986 obs Nov 2024 # n = 5,266 unique patients

#Date and time 
sum(is.na(labtest_cirrho$date_time))

# Convert to Date and Time format and columns
labtest_cirrho <- labtest_cirrho %>%
  mutate(
    Date = as.Date(ymd_hms(date_time)),  # Extract the date part
    Time = format(ymd_hms(date_time), "%H:%M:%S")  # Extract the time part
  )

#Relocate so date and time follow date_time
labtest_cirrho <- labtest_cirrho %>% 
  relocate("Date", .after="date_time")
labtest_cirrho <- labtest_cirrho %>% 
  relocate("Time", .after="Date")

# Remove ymd_hms(date_time) colum
labtest_cirrho %>%
  dplyr::select(-`date_time`) -> labtest_cirrho

# NOT YET Filter for all lab test data following or 90 days prior to cirrhosis diagnosis per patient

# Convert both data frames to data.tables
# setDT(labtest_cirrho)
# setDT(cirrho_cohort)

# Subset cirrho_cohort to only required columns
# cirrho_cohort_subset <- cirrho_cohort[, .(study_subject_id, date_diagnosis)]

# Perform efficient join and filtering
# labtest_cirrho_during_study <- labtest_cirrho[cirrho_cohort_subset, 
#                                          on = "study_subject_id"][Date >= (date_diagnosis - 90)]

# Unique study ID within lab tests filtered
# length(unique(labtest_cirrho_during_study$study_subject_id)) # n = 5077




### Haemoglobin ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only HB 

cirrho_hb <- filter(labtest_cirrho, test_name_id == "HB")
length(unique(cirrho_hb$study_subject_id))

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_hb))
print(missing_counts)
rm(missing_counts)

##### Combine HB results 

# Remove non-numeric characters except decimals and turn to numeric

cirrho_hb <- cirrho_hb %>%
  mutate(
    result = as.numeric(gsub("[^0-9.]", "", result)), 
    result_numerical = as.numeric(gsub("[^0-9.]", "", result_numerical))
  )


# Check if results columns are identical 
# (ignoring NA)

# Number of HB results where result and numerical match
cirrho_hb <- cirrho_hb %>% 
  mutate(HB_match = result == result_numerical)
cirrho_hb$HB_match[is.na(cirrho_hb$HB_match)] <- FALSE
table(cirrho_hb$HB_match)

# Number of HB results where result and numerical both contain numerical values 
# (i.e ignoring NA values)
cirrho_hb <- cirrho_hb %>%
  mutate(bothHB_numeric = complete.cases(result_numerical, result))
table(cirrho_hb$bothHB_numeric)

# Check that all cases where there are numerical HB results in result and
# numerical also meet the condition that they are the same values

cirrho_hb <- cirrho_hb %>% 
  mutate(HB_both_true = ifelse(HB_match & bothHB_numeric, TRUE, FALSE))
table(cirrho_hb$HB_both_true)

#Success! The 169837 Hb values entered into both result and result_numerical 
#are the same

#Can safely merge result (numerical form of result) and result_numerical 
#columns to generate merged Hb list (as all those unmatched are due to no numerical value in one column)
cirrho_hb$HB_merged <- coalesce(cirrho_hb$result, cirrho_hb$result_numerical)
length(cirrho_hb$HB_merged)

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_hb))
print(missing_counts)
rm(missing_counts)

# Remove rows with missing Hb_merged value
cirrho_hb <- cirrho_hb %>%
  filter(!is.na(HB_merged))

# Drop unecessary columns
cirrho_hb <- cirrho_hb %>%
  select(study_subject_id, test_name_id, Date, Time, HB_merged, units, lower_reference_value, lower_reference_value_numerical, upper_reference_value, upper_reference_value_numerical)

##### Reference ranges
## To aid downstream subsetting 
## Merge upper and lower ref values to make one numeric column for each

# Check and ref ranges which will lose numeric in mutate
table(cirrho_hb$lower_reference_value)
table(cirrho_hb$lower_reference_value_numerical)
table(cirrho_hb$upper_reference_value)
table(cirrho_hb$upper_reference_value_numerical)

# First convert upper reference and lower reference to numerical 
cirrho_hb <- cirrho_hb %>% 
  mutate(lower_ref_numeric1 = as.numeric(lower_reference_value))
cirrho_hb <- cirrho_hb %>% 
  mutate(upper_ref_numeric1 = as.numeric(upper_reference_value))
cirrho_hb <- cirrho_hb %>% 
  mutate(lower_ref_numeric2 = as.numeric(lower_reference_value_numerical))
cirrho_hb <- cirrho_hb %>% 
  mutate(upper_ref_numeric2 = as.numeric(upper_reference_value_numerical))

# Check ref ranges where ref_numeric1 and ref_numeric2 match

# Lower
cirrho_hb <- cirrho_hb %>% 
  mutate(Lower_ref_match = lower_ref_numeric1 == lower_ref_numeric2)
cirrho_hb$Lower_ref_match[is.na(cirrho_hb$Lower_ref_match)] <- FALSE
table(cirrho_hb$Lower_ref_match)

#Upper
cirrho_hb <- cirrho_hb %>% 
  mutate(Upper_ref_match = upper_ref_numeric1 == upper_ref_numeric2)
cirrho_hb$Upper_ref_match[is.na(cirrho_hb$Upper_ref_match)] <- FALSE
table(cirrho_hb$Upper_ref_match)

# Number of ref results where ref_numeric1 and ref_numeric2 both contain 
#numerical values (i.e ignoring NA values)

#Lower
cirrho_hb <- cirrho_hb %>%
  mutate(both_lowerref_numeric = complete.cases(lower_ref_numeric1, lower_ref_numeric2))
table(cirrho_hb$both_lowerref_numeric)

#Upper
cirrho_hb <- cirrho_hb %>%
  mutate(both_upperref_numeric = complete.cases(upper_ref_numeric1, upper_ref_numeric2))
table(cirrho_hb$both_upperref_numeric)

# Check that all cases where numerical ref range results in both ref_numeric1
# and ref_numeric2 also meet the condition that they are the same values

#Lower
cirrho_hb <- cirrho_hb %>% 
  mutate(Lowerref_both_true = ifelse(Lower_ref_match & both_lowerref_numeric, TRUE, FALSE))
table(cirrho_hb$Lowerref_both_true)

#Upper
cirrho_hb <- cirrho_hb %>% 
  mutate(Upperref_both_true = ifelse(Upper_ref_match & both_upperref_numeric, TRUE, FALSE))
table(cirrho_hb$Upperref_both_true)

#Success! The 115269 ref range values entered into both ref range columns are 
#the same for both lower and upper ref ranges

#Can safely merge ref ranges (numerical form) for lower and upper columns to 
#generate single merged ref range

#Lower
cirrho_hb$Lowerref_merged <- coalesce(cirrho_hb$lower_ref_numeric1, cirrho_hb$lower_ref_numeric2)
length(cirrho_hb$Lowerref_merged)
table(cirrho_hb$Lowerref_merged)

#Upper
cirrho_hb$Upperref_merged <- coalesce(cirrho_hb$upper_ref_numeric1, cirrho_hb$upper_ref_numeric2)
length(cirrho_hb$Upperref_merged)
table(cirrho_hb$Upperref_merged)

# Select columns required
cirrho_hb <- cirrho_hb %>%
  select(-c(lower_reference_value, lower_reference_value_numerical, upper_reference_value, upper_reference_value_numerical, lower_ref_numeric1, lower_ref_numeric2, Lower_ref_match, Lowerref_both_true, upper_ref_numeric1, upper_ref_numeric2, Upper_ref_match, Upperref_both_true, both_lowerref_numeric, both_upperref_numeric))

##### Units 
## All Hb values need to be in the same unit form of g/L
table(cirrho_hb$units)

# Check CC_UM_G/L values
cirrho_hb_um <- cirrho_hb %>%
  filter(units=='CC_UM_G/L')
summary(cirrho_hb_um)
hist(cirrho_hb_um$HB_merged)
rm(cirrho_hb_um) # all inkeeping g/L

# Change all l to L
cirrho_hb$units <- gsub('l','L',cirrho_hb$units)
table(cirrho_hb$units)

# Change all CC_UM_G/L to g/L
cirrho_hb$units <- gsub('CC_UM_G/L','g/L',cirrho_hb$units)
table(cirrho_hb$units)


## Identify Hb values where g/dL CORRECT and INCORRECT

# New df with all values g/dL
cirrho_hb_gdL <- cirrho_hb[cirrho_hb$units == 'g/dL', ]
hist(cirrho_hb_gdL$HB_merged)

# If Hb <30 and units g/dL likely CORRECT, convert to g/L and * 10
cirrho_hb <- cirrho_hb %>%
  mutate(HB_mod1 = if_else(HB_merged < 30 & units == "g/dL", HB_merged * 10, HB_merged),
         units_mod1 = if_else(HB_merged < 30 & units == "g/dL","g/L", units))

# Review df with all values g/dL
cirrho_hb_gdL <- cirrho_hb[cirrho_hb$units_mod1 == 'g/dL', ]
hist(cirrho_hb_gdL$HB_merged)

# Hb g/dL likely INCORRECT if result outside biologically plausible range (>30g/dL) n = 44,668

# Count values where Hb >=30 g/dL AND upper and lower reference value in keeping g/L 
cirrho_hb_gdL <- cirrho_hb_gdL %>% 
  mutate(HB30_ref_gL = if_else(Upperref_merged > 100 & Lowerref_merged > 100, TRUE, FALSE, missing = NA))
table(cirrho_hb_gdL$HB30_ref_gL,useNA = 'always')

# check values where return is NA
cirrho_hb_gdL_NA <- cirrho_hb_gdL %>%
  filter(is.na(HB30_ref_gL))
rm(cirrho_hb_gdL_NA)

# Counts: 44,665 of 44,668 values Hb >30 have ref range values in keeping g/L (i.e >100)
# remaining 3 Hb >30 have no reference ranges but Hb values in keeping g/L
# NO recorded values HB >30 g/dL have reference ranges for g/dL

# ALL HB >30 G/DL TREATED AS G/L

cirrho_hb <- cirrho_hb %>%
  mutate(units_mod1 = if_else(units_mod1=='g/dL','g/L',units_mod1) )
table(cirrho_hb$units_mod1)

# Remove unuecessary d.f
rm(cirrho_hb_gdL)


# REVIEW VALUES WITH NO UNITS

# Merge no units into NULL
cirrho_hb <- cirrho_hb %>%
  mutate(units_mod1 = if_else(units_mod1=='','NULL',units_mod1))
table(cirrho_hb$units_mod1)

# New d.f of results with NULL units
cirrho_hb_null <- cirrho_hb %>%
  filter(units_mod1 == 'NULL')
summary(cirrho_hb_null)
hist(cirrho_hb_null$HB_mod1)

# In those with units = NULL: Hb value < 20 * 10, if >= 20 keep n = 64,574
cirrho_hb <- cirrho_hb %>%
  mutate(HB_mod2 = if_else(HB_mod1 <20 & units_mod1 == 'NULL',HB_mod1*10,HB_mod1),
         units_mod2 = if_else(units_mod1=='NULL','g/L',units_mod1))

# Tidy d.f 
rm(cirrho_hb_null)
cirrho_hb <- cirrho_hb %>%
  select(study_subject_id, test_name_id, Date, HB_mod2, units_mod2) %>%
  dplyr::rename(hb_value = HB_mod2,
         units = units_mod2)


##### Spurious results, outliers, and missing data 
summary(cirrho_hb)

# Remove clinically unfeasible values (>300g/L or <20 g/L)
cirrho_hb <- cirrho_hb %>%
  filter(hb_value <= 300 & hb_value >=20) # n = 

summary(cirrho_hb)

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_hb))
print(missing_counts)
rm(missing_counts)

####Final HB data frame for ML analysis
cirrho_hb_during_study <- cirrho_hb
rm(cirrho_hb)

summary(cirrho_hb_during_study)
hist(cirrho_hb_during_study$hb_value)
length(unique(cirrho_hb_during_study$study_subject_id))


### Ferritin ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### Ferritin (CFER) 
labtest_cirrho %>%
  filter(test_name_id == "CFER") -> Labtest_CFER
length(unique(Labtest_CFER$study_subject_id)) # n = 4377

##### Combine CFER results

table(Labtest_CFER$result)
sum(is.na(Labtest_CFER$result))
table(Labtest_CFER$result_numerical)
sum(is.na(Labtest_CFER$result_numerical))

# 2 columns with CFER results - result and result_numerical
# view non-numeric contents of CFER result and result_numerical
# Filter for non-numerical data using grepl
non_numeric_CFER_data <- Labtest_CFER$result[!grepl("^[0-9.]+$", Labtest_CFER$result)]
non_numeric_CFER_data2 <- Labtest_CFER$result_numerical[!grepl("^[0-9.]+$", Labtest_CFER$result_numerical)|is.na(Labtest_CFER$result_numerical)]

# Create a table of non-numerical values
table(non_numeric_CFER_data)
table(non_numeric_CFER_data2)

# Create new column of result (character) to result (numerical) 

# NB No. (159) of "result" contained > or < which need to be flagged as may bias descriptive stats
count <- sum(grepl("[<>]", Labtest_CFER$result))
count
Labtest_CFER_bound <- Labtest_CFER[grepl("[<>]", Labtest_CFER$result), ]

# Exclude if bound does not commit result to group n=2 (<1300)
Labtest_CFER$result <- if_else(Labtest_CFER$result =='<1300', NA, Labtest_CFER$result)
count <- sum(grepl("[<>]", Labtest_CFER$result))
count

# Where result value contains bound (157 results after removing 2 = '<1300', Nov 2024), make new variable to flag CFER_bound
Labtest_CFER$CFER_bound <- if_else(grepl("[<>]", Labtest_CFER$result), T, F, F)

#Now convert result and result_numerical into numerical
Labtest_CFER <- Labtest_CFER %>%
  mutate(Result2 = as.numeric(gsub("<|>", "", result, ignore.case = T)))
sum(is.na(Labtest_CFER$Result2))

sum(is.na(Labtest_CFER$result_numerical))
Labtest_CFER <- Labtest_CFER %>%
  mutate(result_numerical = as.numeric(result_numerical))
sum(is.na(Labtest_CFER$result_numerical))

# Now we have 2 numerical columns of results and can find if they are identical 
# (ignoring NA)

# Number of CFER results where Result2 and result_numerical match
Labtest_CFER <- Labtest_CFER %>% 
  mutate(CFER_match = Result2 == result_numerical)
table(Labtest_CFER$CFER_match)

# As there are no FALSE values, suggests that wherever both contain numeric value, they are the same. 
# Therefore removed step performed with HB to convert NAs to FALSE 

# Number of FER results where Result2 and result_numerical both contain numerical values 
# (i.e ignoring NA values)
Labtest_CFER <- Labtest_CFER %>%
  mutate(bothCFER_numeric = complete.cases(result_numerical, Result2))
table(Labtest_CFER$bothCFER_numeric)

# Check that all cases where there are numerical FER results in Result2 and
# numerical also meet the condition that they are the same values

Labtest_CFER <- Labtest_CFER %>% 
  mutate(CFER_both_true = ifelse(CFER_match & bothCFER_numeric, TRUE, FALSE))
table(Labtest_CFER$CFER_both_true)

#Success! The 16880 CFER values entered into both result and result_numerical 
#are the same

#Can safely merge result2 (numerical form of result) and result_numerical 
#columns to generate merged CFER list
Labtest_CFER$CFER_merged <- coalesce(Labtest_CFER$Result2, Labtest_CFER$result_numerical)
length(Labtest_CFER$CFER_merged)
sum(is.na(Labtest_CFER$CFER_merged))

##### Reference ranges

# Check any ref ranges which will lose numeric in mutate
table(Labtest_CFER$lower_reference_value)
table(Labtest_CFER$lower_reference_value_numerical)
table(Labtest_CFER$upper_reference_value)
table(Labtest_CFER$upper_reference_value_numerical)

## Merge upper and lower ref values to make one numeric column for each

# First convert upper reference and lower reference to numerical 
Labtest_CFER <- Labtest_CFER %>% 
  mutate(lower_ref_numeric1 = as.numeric(lower_reference_value))
Labtest_CFER <- Labtest_CFER %>% 
  mutate(upper_ref_numeric1 = as.numeric(upper_reference_value))
Labtest_CFER <- Labtest_CFER %>% 
  mutate(lower_ref_numeric2 = as.numeric(lower_reference_value_numerical))
Labtest_CFER <- Labtest_CFER %>% 
  mutate(upper_ref_numeric2 = as.numeric(upper_reference_value_numerical))

# Check ref ranges where ref_numeric1 and ref_numeric2 match

# Lower
Labtest_CFER <- Labtest_CFER %>% 
  mutate(Lower_ref_match = lower_ref_numeric1 == lower_ref_numeric2)
Labtest_CFER$Lower_ref_match[is.na(Labtest_CFER$Lower_ref_match)] <- FALSE
table(Labtest_CFER$Lower_ref_match)

#Upper
Labtest_CFER <- Labtest_CFER %>% 
  mutate(Upper_ref_match = upper_ref_numeric1 == upper_ref_numeric2)
Labtest_CFER$Upper_ref_match[is.na(Labtest_CFER$Upper_ref_match)] <- FALSE
table(Labtest_CFER$Upper_ref_match)

# Number of ref results where ref_numeric1 and ref_numeric2 both contain 
#numerical values (i.e ignoring NA values)

#Lower
Labtest_CFER <- Labtest_CFER %>%
  mutate(both_lowerref_numeric = complete.cases(lower_ref_numeric1, lower_ref_numeric2))
table(Labtest_CFER$both_lowerref_numeric)

#Upper
Labtest_CFER <- Labtest_CFER %>%
  mutate(both_upperref_numeric = complete.cases(upper_ref_numeric1, upper_ref_numeric2))
table(Labtest_CFER$both_upperref_numeric)

# Check that all cases where numerical ref range results in both ref_numeric1
# and ref_numeric2 also meet the condition that they are the same values

#Lower
Labtest_CFER <- Labtest_CFER %>% 
  mutate(Lowerref_both_true = ifelse(Lower_ref_match & both_lowerref_numeric, TRUE, FALSE))
table(Labtest_CFER$Lowerref_both_true)

#Upper
Labtest_CFER <- Labtest_CFER %>% 
  mutate(Upperref_both_true = ifelse(Upper_ref_match & both_upperref_numeric, TRUE, FALSE))
table(Labtest_CFER$Upperref_both_true)

#Success! The 11657 ref range values entered into both ref range columns are 
#the same for both lower and upper ref ranges

#Can safely merge ref ranges (numerical form) for lower and upper columns to 
#generate single merged ref range

#Lower
Labtest_CFER$Lowerref_merged <- coalesce(Labtest_CFER$lower_ref_numeric1, Labtest_CFER$lower_ref_numeric2)
length(Labtest_CFER$Lowerref_merged)
table(Labtest_CFER$Lowerref_merged)

#Upper
Labtest_CFER$Upperref_merged <- coalesce(Labtest_CFER$upper_ref_numeric1, Labtest_CFER$upper_ref_numeric2)
length(Labtest_CFER$Upperref_merged)
table(Labtest_CFER$Upperref_merged)

##### Units
table(Labtest_CFER$units,useNA = 'always')

# Check mean of each units
Labtest_CFER %>%
  group_by(units) %>%
  summarise(count= sum(!is.na(CFER_merged)), median(CFER_merged,na.rm = T)) -> CFER_units_medians # Medians are all within expected ranges if units ug/L for all


# All forms of units equivalent to ug/l
Labtest_CFER$units <- gsub('CC_UM_UG/L|micro g/L|microg/L|ng/mL|NULL|ug/l|UG/L','ug/L',Labtest_CFER$units)
table(Labtest_CFER$units)
sum(is.na(Labtest_CFER$units))


##### Spurious results, outliers, and missing data 
# Hard to determine upper limit of ferritin
# 9 results >300,000

# Try convert all >15,000
# Also why reference ranges different?

sum(is.na(Labtest_CFER$CFER_merged))

# Total missing data = 237 values

## USE COMPLETE CASE ANALYSIS 
# Omit na.s from data frame
Labtest_CFER %>%
  filter(!is.na(CFER_merged)) -> CFER_noNA
sum(is.na(CFER_noNA$CFER_merged))
CFER_noNA %>% 
  summary(study_subject_id)

####Final CFER data frame for analysis
cirrho_cfer_during_study <- CFER_noNA[,c('study_subject_id','test_name_id','Date','CFER_merged','units')]
summary(cirrho_cfer_during_study)
length(unique(cirrho_cfer_during_study$study_subject_id)) # n = 4356

#Tidy d.fs
rm(CFER_noNA, CFER_units_medians, Labtest_CFER_bound)


### INR ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

table(labtest_cirrho$test_name)

labtest_cirrho %>%
  filter(test_name_id == "HINR") -> Labtest_HINR
length(unique(Labtest_HINR$study_subject_id)) #n = 4852

##### Combine HINR results
table(Labtest_HINR$result)
sum(is.na(Labtest_HINR$result))
table(Labtest_HINR$result_numerical)
sum(is.na(Labtest_HINR$result_numerical))

# Filter for non-numerical data using grepl
non_numeric_HINR_data <- Labtest_HINR$result[!grepl("^[0-9.]+$", Labtest_HINR$result)]
non_numeric_HINR_data2 <- Labtest_HINR$result_numerical[!grepl("^[0-9.]+$", Labtest_HINR$result_numerical)|is.na(Labtest_HINR$result_numerical)]

# Create a table of non-numerical values
table(non_numeric_HINR_data)
table(non_numeric_HINR_data2)

# One HINR_result has a ,. Replace with.
Labtest_HINR$result <- gsub(",", ".", Labtest_HINR$result)
Labtest_HINR$result <- gsub(" ", "", Labtest_HINR$result)

# NB No. (139) of "result" contained > or < which need to be flagged as may bias descriptive stats
count <- sum(grepl("[<>]", Labtest_HINR$result))
count
Labtest_HINR_bound <- Labtest_HINR[grepl("[<>]", Labtest_HINR$result), ]

# Require 2 columns with HINR results - result and result_numerical

# All bounds commit result to group (<0.7 n=1, >8 n=138)

# Where result value contains bound (all 138 results, Nov 2024), make new variable to flag HINR_bound
Labtest_HINR$HINR_bound <- if_else(grepl("[<>]", Labtest_HINR$result), T, F, F)

# And remove bound (>|<) to allow conversion to numerical
Labtest_HINR$result <- gsub("<|>", "", Labtest_HINR$result)
count <- sum(grepl("[<>]", Labtest_HINR$result))
count

# Now create new column of result (character) to result2 (numerical) 
# AND result numerical to numerical

Labtest_HINR <- Labtest_HINR %>%
  mutate(Result2 = as.numeric(result))
sum(is.na(Labtest_HINR$Result2))

sum(is.na(Labtest_HINR$result_numerical))
Labtest_HINR <- Labtest_HINR %>%
  mutate(result_numerical = as.numeric(result_numerical))
sum(is.na(Labtest_HINR$result_numerical)) # This is as expected from non-numeric data in this column (blank 38548 + NULL 7746 = 46294)

# Now we have 2 numerical columns of results and can find if they are identicalwhere they contain values 
# (i.e ignoring NA)

# Number of INR results where Result2 and numerical match
Labtest_HINR <- Labtest_HINR %>% 
  mutate(HINR_match = Result2 == result_numerical)
table(Labtest_HINR$HINR_match)

# As there are no FALSE values, suggests that wherever both contain numeric value, they are the same. 
# Therefore removed step performed with HB to convert NAs to FALSE 

# Number of HINR results where Result2 and numerical both contain numerical values 
# (i.e ignoring NA values)
Labtest_HINR <- Labtest_HINR %>%
  mutate(bothHINR_numeric = complete.cases(result_numerical, Result2))
table(Labtest_HINR$bothHINR_numeric)

# Check that all cases where there are numerical HINR results in Result2 and
# numerical also meet the condition that they are the same values

Labtest_HINR <- Labtest_HINR %>% 
  mutate(HINR_both_true = ifelse(HINR_match & bothHINR_numeric, TRUE, FALSE))
table(Labtest_HINR$HINR_both_true)

#Success! The 104390 HINR values entered into both result and result_numerical 
#are the same

#Can safely merge result2 (numerical form of result) and result_numerical 
#columns to generate merged HINR list
Labtest_HINR$HINR_merged <- coalesce(Labtest_HINR$Result2, Labtest_HINR$result_numerical)
length(Labtest_HINR$HINR_merged)
sum(is.na(Labtest_HINR$HINR_merged))

##### Reference ranges

# Check and ref ranges which will lose numeric in mutate
table(Labtest_HINR$lower_reference_value)
table(Labtest_HINR$lower_reference_value_numerical)
table(Labtest_HINR$upper_reference_value)
table(Labtest_HINR$upper_reference_value_numerical)

## Merge upper and lower ref values to make one numeric column for each

# First convert upper reference and lower reference to numerical 
Labtest_HINR <- Labtest_HINR %>% 
  mutate(lower_ref_numeric1 = as.numeric(lower_reference_value))
Labtest_HINR <- Labtest_HINR %>% 
  mutate(upper_ref_numeric1 = as.numeric(upper_reference_value))
Labtest_HINR <- Labtest_HINR %>% 
  mutate(lower_ref_numeric2 = as.numeric(lower_reference_value_numerical))
Labtest_HINR <- Labtest_HINR %>% 
  mutate(upper_ref_numeric2 = as.numeric(upper_reference_value_numerical))

# Check ref ranges where ref_numeric1 and ref_numeric2 match

# Lower
Labtest_HINR <- Labtest_HINR %>% 
  mutate(Lower_ref_match = lower_ref_numeric1 == lower_ref_numeric2)
table(Labtest_HINR$Lower_ref_match)

#Upper
Labtest_HINR <- Labtest_HINR %>% 
  mutate(Upper_ref_match = upper_ref_numeric1 == upper_ref_numeric2)
table(Labtest_HINR$Upper_ref_match)

# Number of ref results where ref_numeric1 and ref_numeric2 both contain 
#numerical values (i.e ignoring NA values)

#Lower
Labtest_HINR <- Labtest_HINR %>%
  mutate(both_lowerref_numeric = complete.cases(lower_ref_numeric1, lower_ref_numeric2))
table(Labtest_HINR$both_lowerref_numeric)

#Upper
Labtest_HINR <- Labtest_HINR %>%
  mutate(both_upperref_numeric = complete.cases(upper_ref_numeric1, upper_ref_numeric2))
table(Labtest_HINR$both_upperref_numeric)

# Check that all cases where numerical ref range results in both ref_numeric1
# and ref_numeric2 also meet the condition that they are the same values

#Lower
Labtest_HINR <- Labtest_HINR %>% 
  mutate(Lowerref_both_true = ifelse(Lower_ref_match & both_lowerref_numeric, TRUE, FALSE))
table(Labtest_HINR$Lowerref_both_true)

#Upper
Labtest_HINR <- Labtest_HINR %>% 
  mutate(Upperref_both_true = ifelse(Upper_ref_match & both_upperref_numeric, TRUE, FALSE))
table(Labtest_HINR$Upperref_both_true)

#Success! The ref range values entered into both ref range columns are 
#the same for both lower and upper ref ranges

#Can safely merge ref ranges (numerical form) for lower and upper columns to 
#generate single merged ref range

#Lower
Labtest_HINR$Lowerref_merged <- coalesce(Labtest_HINR$lower_ref_numeric1, Labtest_HINR$lower_ref_numeric2)
length(Labtest_HINR$Lowerref_merged)
table(Labtest_HINR$Lowerref_merged)

#Upper
Labtest_HINR$Upperref_merged <- coalesce(Labtest_HINR$upper_ref_numeric1, Labtest_HINR$upper_ref_numeric2)
length(Labtest_HINR$Upperref_merged)
table(Labtest_HINR$Upperref_merged)

##### Units
table(Labtest_HINR$units,useNA = 'always')
summary(Labtest_HINR$HINR_merged)

# Check mean of each units
Labtest_HINR %>%
  group_by(units) %>%
  summarise(count= sum(!is.na(HINR_merged)), median(HINR_merged,na.rm = T)) -> HINR_units_means # Means are all within expected ranges if units ug/L for all


# All forms of units equivalent and can be ignored

##### Spurious results, outliers, and missing data

summary(Labtest_HINR$HINR_merged)

# Consider HINR >50 as erroneous (n=22) and exclude
sum(Labtest_HINR$HINR_merged >20, na.rm=TRUE)
Labtest_HINR <- Labtest_HINR %>%
  mutate(HINR_clinical = if_else(HINR_merged<20, HINR_merged, NA))

# Total missing data = 3238 values 
sum(is.na(Labtest_HINR$HINR_merged))
sum(is.na(Labtest_HINR$HINR_clinical))

## Omit na.s from data frame
HINR_noNA <- Labtest_HINR[!is.na(Labtest_HINR$HINR_clinical), ]
sum(is.na(HINR_noNA$HINR_clinical))
HINR_noNA %>% 
  summary(study_subject_id)

####Final HINR data frame for analysis
cirrho_hinr_during_study <- HINR_noNA[,c('study_subject_id','test_name_id','Date','HINR_clinical','Lowerref_merged','Upperref_merged', 'HINR_bound')]
length(unique(cirrho_hinr_during_study$study_subject_id)) # n = 4850


### Platelets ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### Platelets (PL)
Labtest_PL <- filter(labtest_cirrho, test_name_id == "PL")
length(unique(Labtest_PL$study_subject_id))

##### Combine PL results

table(Labtest_PL$result)
sum(is.na(Labtest_PL$result))
table(Labtest_PL$result_numerical)
sum(is.na(Labtest_PL$result_numerical))

# Filter for non-numerical data using grepl
non_numeric_PL_data <- Labtest_PL$result[!grepl("^[0-9.]+$", Labtest_PL$result)|is.na(Labtest_PL$result)]
non_numeric_PL_data2 <- Labtest_PL$result_numerical[!grepl("^[0-9.]+$", Labtest_PL$result_numerical)|is.na(Labtest_PL$result_numerical)]

# Table non-numeric values for result and result2
table(non_numeric_PL_data)
table(non_numeric_PL_data2)

# NB No. (1) of "result" contained < which need to be flagged as may bias descriptive stats
count <- sum(grepl("[<>]", Labtest_PL$result))
count
Labtest_PL_bound <- Labtest_HINR[grepl("[<>]", Labtest_HINR$result), ]

# All bounds commit result to group (<2 n=1)
# Where result value contains bound (n=1, noc 2024) make new variable to flag PL_bound
Labtest_PL$PL_bound <- if_else(grepl("[<>]", Labtest_PL$result), T, F, F)

# And remove bound (>|<) to allow conversion to numerical
Labtest_PL$result <- gsub("<|>", "", Labtest_PL$result)
count <- sum(grepl("[<>]", Labtest_PL$result))
count

# Remove all non-numeric characters from result to leave only numbers or .
Labtest_PL$result <- gsub("[^0-9.]", "", Labtest_PL$result)
table(Labtest_PL$result)

# Re-check non-numeric contents of 'result'
non_numeric_PL_data <- Labtest_PL$result[!grepl("^[0-9.]+$", Labtest_PL$result)]
table(non_numeric_PL_data)
sum(is.na(Labtest_PL$result))

# Now within 'result' only the blank 'non numerical' entries remain (698988 Nov 2024)

# Check 'numeric' contents of 'result'
numeric_PL_data <-  Labtest_PL$result[grepl("^[0-9.]+$", Labtest_PL$result)]
table(numeric_PL_data)

# There are 451 (nov 2024) values of '.' only or '.' either side of number within 'numeric' result

# Therefore total expected NAs when converting 'result' to numeric will be 698988 + 451 = 699439
698988 + 451

# There are 163676 blank and 5018 NULL obs within 'result_numerical'
# Therefore total expected NAs when converting 'result_numerical' to numeric will be 163676 + 5018 = 168694
163676 + 5018

# Convert PL result and result_numerical to numeric

Labtest_PL <- Labtest_PL %>%
  mutate(Result2 = as.numeric(result))
table(Labtest_PL$Result2)
sum(is.na(Labtest_PL$Result2))

sum(is.na(Labtest_PL$result_numerical))
Labtest_PL <- Labtest_PL %>%
  mutate(result_numerical = as.numeric(result_numerical))
sum(is.na(Labtest_PL$result_numerical))

# Now we have 2 numerical columns of results and can find if they are identical 
# (ignoring NA)

# Number of HB results where Result2 and numerical match
Labtest_PL <- Labtest_PL %>% 
  mutate(PL_match = Result2 == result_numerical)
table(Labtest_PL$PL_match)

# As there are no FALSE values, suggests that wherever both contain numeric value, they are the same. 
# Therefore removed step performed with HB to convert NAs to FALSE 

# Number of PL results where Result2 and numerical both contain numerical values 
# (i.e ignoring NA values)
Labtest_PL <- Labtest_PL %>%
  mutate(bothPL_numeric = complete.cases(result_numerical, Result2))
table(Labtest_PL$bothPL_numeric)

# Check that all cases where there are numerical PL results in Result2 and
# numerical also meet the condition that they are the same values

Labtest_PL <- Labtest_PL %>% 
  mutate(PL_both_true = ifelse(PL_match & bothPL_numeric, TRUE, FALSE))
table(Labtest_PL$PL_both_true)

#Success! The 167963  PL values entered into both result and result_numerical 
#are the same

#Can safely merge result2 (numerical form of result) and result_numerical 
#columns to generate merged PL list
Labtest_PL$PL_merged <- coalesce(Labtest_PL$Result2, Labtest_PL$result_numerical)
length(Labtest_PL$PL_merged)
sum(is.na(Labtest_PL$PL_merged))

##### Reference ranges

# Check and ref ranges which will lose numeric in mutate
table(Labtest_PL$lower_reference_value)
table(Labtest_PL$lower_reference_value_numerical)
table(Labtest_PL$upper_reference_value)
table(Labtest_PL$upper_reference_value_numerical)

## Merge upper and lower ref values to make one numeric column for each

# First convert upper reference and lower reference to numerical 
Labtest_PL <- Labtest_PL %>% 
  mutate(lower_ref_numeric1 = as.numeric(lower_reference_value))
Labtest_PL <- Labtest_PL %>% 
  mutate(upper_ref_numeric1 = as.numeric(upper_reference_value))
Labtest_PL <- Labtest_PL %>% 
  mutate(lower_ref_numeric2 = as.numeric(lower_reference_value_numerical))
Labtest_PL <- Labtest_PL %>% 
  mutate(upper_ref_numeric2 = as.numeric(upper_reference_value_numerical))

# Check ref ranges where ref_numeric1 and ref_numeric2 match

# Lower
Labtest_PL <- Labtest_PL %>% 
  mutate(Lower_ref_match = lower_ref_numeric1 == lower_ref_numeric2)
table(Labtest_PL$Lower_ref_match)

#Upper
Labtest_PL <- Labtest_PL %>% 
  mutate(Upper_ref_match = upper_ref_numeric1 == upper_ref_numeric2)
table(Labtest_PL$Upper_ref_match)

# Number of ref results where ref_numeric1 and ref_numeric2 both contain 
#numerical values (i.e ignoring NA values)

#Lower
Labtest_PL <- Labtest_PL %>%
  mutate(both_lowerref_numeric = complete.cases(lower_ref_numeric1, lower_ref_numeric2))
table(Labtest_PL$both_lowerref_numeric)

#Upper
Labtest_PL <- Labtest_PL %>%
  mutate(both_upperref_numeric = complete.cases(upper_ref_numeric1, upper_ref_numeric2))
table(Labtest_PL$both_upperref_numeric)

# Check that all cases where numerical ref range results in both ref_numeric1
# and ref_numeric2 also meet the condition that they are the same values

#Lower
Labtest_PL <- Labtest_PL %>% 
  mutate(Lowerref_both_true = ifelse(Lower_ref_match & both_lowerref_numeric, TRUE, FALSE))
table(Labtest_PL$Lowerref_both_true)

#Upper
Labtest_PL <- Labtest_PL %>% 
  mutate(Upperref_both_true = ifelse(Upper_ref_match & both_upperref_numeric, TRUE, FALSE))
table(Labtest_PL$Upperref_both_true)

#Success! The 198490 ref range values entered into both ref range columns are 
#the same for both lower and upper ref ranges

#Can safely merge ref ranges (numerical form) for lower and upper columns to 
#generate single merged ref range

#Lower
Labtest_PL$Lowerref_merged <- coalesce(Labtest_PL$lower_ref_numeric1, Labtest_PL$lower_ref_numeric2)
length(Labtest_PL$Lowerref_merged)
table(Labtest_PL$Lowerref_merged)

#Upper
Labtest_PL$Upperref_merged <- coalesce(Labtest_PL$upper_ref_numeric1, Labtest_PL$upper_ref_numeric2)
length(Labtest_PL$Upperref_merged)
table(Labtest_PL$Upperref_merged)

##### Units
table(Labtest_PL$units,useNA = 'always')

# All forms of units equivalent to x10*9/L except ?'10' and blank
Labtest_PL$units <- gsub('x10^9/L','10*9/L',Labtest_PL$units, ignore.case = T)
table(Labtest_PL$units)
sum(is.na(Labtest_PL$units))

# Check mean and median values grouped by units
Labtest_PL %>%
  group_by(units) %>%
  summarise(mean(PL_merged,na.rm = T), median(PL_merged,na.rm=T)) -> Labtest_PL_table # All clinically feasible


# Therefore just change all units to x10*9/L
Labtest_PL$units <- 'x10*9/L'


##### Spurious results, outliers, and missing data 

summary(Labtest_PL$PL_merged)

# Consider plt count >3000 as erroneous (n=31) and exclude
sum(Labtest_PL$PL_merged >3000, na.rm=TRUE)
Labtest_PL <- Labtest_PL %>%
  mutate(PL_clinical = if_else(PL_merged<3000, PL_merged, NA))

# Total missing data = 7134 values (7103 na + 31 excluded) 
sum(is.na(Labtest_PL$PL_merged))
sum(is.na(Labtest_PL$PL_clinical))

## Omit na.s from data frame
PL_noNA <- Labtest_PL[!is.na(Labtest_PL$PL_clinical), ]
sum(is.na(PL_noNA$PL_clinical))
PL_noNA %>% 
  summary(study_subject_id)

####Final PL data frame for analysis
cirrho_pl_during_study <- PL_noNA[,c('study_subject_id','test_name_id','Date','PL_clinical','units','PL_bound')]


### Mean cell volume (MCV) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only MCV

cirrho_mcv <- filter(labtest_cirrho, test_name_id == "MCV")
length(unique(cirrho_mcv$study_subject_id))

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_mcv))
print(missing_counts)
rm(missing_counts)

# Merge result and result numerical

# Check Non-numeric mcv
non_numeric_cirrho_mcv <- cirrho_mcv %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_mcv$result) # no numeric results with associated text therefore OK to become NAs

non_numeric_cirrho_mcv <- cirrho_mcv %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_mcv$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Make values numeric
cirrho_mcv <- cirrho_mcv %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_mcv))
print(missing_counts) # result na = 60 803, result_numerical na = 17193 as expected
rm(missing_counts)

# Explore raw values
summary(cirrho_mcv)
hist(cirrho_mcv$result_numerical)

# Remove biologically implausible values 
cirrho_mcv <- cirrho_mcv %>%
  filter(result >= 50 & result <= 150 | result_numerical >= 50 & result_numerical <=150)

# Explore biologically plausible values
summary(cirrho_mcv)
hist(cirrho_mcv$result_numerical)
hist(cirrho_mcv$result) # normally distributed

# Merge 

cirrho_mcv <- cirrho_mcv %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_mcv$mismatch_flag) # no mismatches
sum(is.na(cirrho_mcv$result_final))

cirrho_mcv_during_study <- cirrho_mcv

# Remove unecessary d.fs
rm(HINR_noNA, HINR_units_means, Labtest_CFER, Labtest_HINR, Labtest_HINR_bound, Labtest_PL, Labtest_PL_bound, Labtest_PL_table, PL_noNA)

### C reactive protein (CRP) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only CRP

cirrho_crp <- filter(labtest_cirrho, test_name_id == "CRP")
length(unique(cirrho_crp$study_subject_id))

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_crp))
print(missing_counts)
rm(missing_counts)

# Merge result and result numerical

# Check Non-numeric crp
non_numeric_cirrho_crp <- cirrho_crp %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_crp$result) # no numeric results with associated text therefore OK to become NAs

non_numeric_cirrho_crp <- cirrho_crp %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_crp$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Make values numeric
cirrho_crp <- cirrho_crp %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_crp))
print(missing_counts) # result na = 52 867, result_numerical na = 5237 as expected
rm(missing_counts)

# Explore raw values
summary(cirrho_crp)
hist(cirrho_crp$result_numerical)

# Explore biologically plausible values
hist(cirrho_crp$result) # normally distributed

# Merge 

cirrho_crp <- cirrho_crp %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_crp$mismatch_flag) # no mismatches
sum(is.na(cirrho_crp$result_final))

# Remove biologically implausible values 
cirrho_crp <- cirrho_crp %>%
  filter(result_final <= 700) #n = 1

cirrho_crp_during_study <- cirrho_crp



### Serum creatinine (creat) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only creat

cirrho_creat <- filter(labtest_cirrho, test_name_id == "BCRE")
length(unique(cirrho_creat$study_subject_id))

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_creat))
print(missing_counts)
rm(missing_counts)

# Merge result and result numerical

# Check Non-numeric creat
non_numeric_cirrho_creat <- cirrho_creat %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_creat$result) # no numeric results with associated text therefore OK to become NAs

non_numeric_cirrho_creat <- cirrho_creat %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_creat$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Make values numeric
cirrho_creat <- cirrho_creat %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_creat))
print(missing_counts) # result na = 59 301, result_numerical na = 16876 as expected
rm(missing_counts)

# Explore raw values
summary(cirrho_creat)
hist(cirrho_creat$result_numerical)
hist(cirrho_creat$result)

# No biologically implausible values 

# Check units
table(cirrho_creat$units) # all umol/L except 14 mmol/L (1mmol/L = 1000 umol/L)
cirrho_creat %>%
  filter(units == 'mmol/L') 

# Results recorded as mmol/L dont make sense to convert and no reference ranges
# Therefore exclude 

cirrho_creat <- cirrho_creat %>%
  filter(units != 'mmol/L') # lose 14 results

# Merge 

cirrho_creat <- cirrho_creat %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_creat$mismatch_flag) # no mismatches
sum(is.na(cirrho_creat$result_final))

cirrho_creat_during_study <- cirrho_creat

rm(cirrho_creat)


### Bilirubin (bili) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only bili

cirrho_bili <- filter(labtest_cirrho, test_name_id == "XBIL")
length(unique(cirrho_bili$study_subject_id))

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_bili))
print(missing_counts)
rm(missing_counts)

# Merge result and result numerical

# Check Non-numeric bili
non_numeric_cirrho_bili <- cirrho_bili %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_bili$result) # no numeric results with associated text therefore OK to become NAs

non_numeric_cirrho_bili <- cirrho_bili %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_bili$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Make values numeric
cirrho_bili <- cirrho_bili %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_bili))
print(missing_counts) # result na = 54 594, result_numerical na = 11 292 as expected
rm(missing_counts)

# Explore raw values
summary(cirrho_bili)
hist(cirrho_bili$result_numerical)
hist(cirrho_bili$result) 

# No biologically implausible values 

# Merge 

cirrho_bili <- cirrho_bili %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_bili$mismatch_flag) # no mismatches
sum(is.na(cirrho_bili$result_final))

# Remove biologically implausible values 
cirrho_bili <- cirrho_bili %>%
  filter(result_final <= 1200) #n = 1

cirrho_bili_during_study <- cirrho_bili
rm(cirrho_bili)

### Mean Cell Hb (MCH) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only mch

table(Labtest.data$test_name_id)
cirrho_mch <- filter(labtest_cirrho, test_name_id == "MC")
length(unique(cirrho_mch$study_subject_id))

# Merge result and result numerical

# Check Non-numeric mch
non_numeric_cirrho_mch <- cirrho_mch %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_mch$result) # # >40.0 n = 3 >400 n= 2 

non_numeric_cirrho_mch <- cirrho_mch %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_mch$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Remove bound (>|<) from result to allow conversion to numerical
cirrho_mch$result <- gsub("<|>", "", cirrho_mch$result)
count <- sum(grepl("[<>]", cirrho_mch$result))
rm(count)

# Make values numeric
cirrho_mch <- cirrho_mch %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_mch))
print(missing_counts) # result na = 60,633, result_numerical na = 17,026 as expected
rm(missing_counts)

# Explore raw values
summary(cirrho_mch)
hist(cirrho_mch$result_numerical)
hist(cirrho_mch$result) 

# Merge 
cirrho_mch <- cirrho_mch %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_mch$mismatch_flag) # no mismatches
sum(is.na(cirrho_mch$result_final))

# Explore raw values
summary(cirrho_mch)
hist(cirrho_mch$result_final)
cirrho_mch %>%
  filter(result_final >= 500| result_final <= 10) #n = 4 and n= 10

# Biologically implausible > 500 (or NA)
cirrho_mch <- cirrho_mch %>%
  filter(result_final < 500 & result_final > 10  )

hist(cirrho_mch$result_final)

# two clearly distinct populations - one is * 10 expected values

# Check units
table(cirrho_mch$units)
cirrho_mch %>%
  filter(units == '/cu.mm') # n = 12 NB to convert cubic mm to 10^9/L divide by 100 but values don't make sense

# Distribution of each
ggplot(cirrho_mch, aes(x = result_final)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ units, scales = "free_y") +  # Creates separate histograms by 'units'
  labs(title = "Histograms of result_final by Units", x = "Result Final", y = "Frequency") +
  theme_minimal()

# results > 100 are in g/L or NULL and are all *10 expected values
cirrho_mch %>% 
  filter(result_final>=100) #n = 72,779

# divide by 10 if result is >100
cirrho_mch <- cirrho_mch %>%
  mutate(result_final = if_else(result_final >= 100, result_final / 10, result_final))

# Distribution of each
ggplot(cirrho_mch, aes(x = result_final)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ units, scales = "free_y") +  # Creates separate histograms by 'units'
  labs(title = "Histograms of result_final by Units", x = "Result Final", y = "Frequency") +
  theme_minimal()

# Overall data
summary(cirrho_mch)

# All within biologically plausible range equivalent to pg

cirrho_mch_during_study <- cirrho_mch
rm(cirrho_mch)


### White blood cells (WB) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only wbc

cirrho_wbc <- filter(labtest_cirrho, test_name_id == "WB")
length(unique(cirrho_wbc$study_subject_id))

# Merge result and result numerical

# Check Non-numeric wbc
non_numeric_cirrho_wbc <- cirrho_wbc %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_wbc$result) # # no relevant numeric results with associated text therefore OK to become NAs

non_numeric_cirrho_wbc <- cirrho_wbc %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_wbc$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Make values numeric
cirrho_wbc <- cirrho_wbc %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_wbc))
print(missing_counts) # result na = 10,490, result_numerical na = 18 195 as expected
rm(missing_counts)


# Explore raw values
summary(cirrho_wbc)
hist(cirrho_wbc$result_numerical)
hist(cirrho_wbc$result) 


# Merge 

cirrho_wbc <- cirrho_wbc %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_wbc$mismatch_flag) # no mismatches
sum(is.na(cirrho_wbc$result_final))

# Explore raw values
summary(cirrho_wbc)
hist(cirrho_wbc$result_final)
cirrho_wbc %>%
  filter(result_final > 1000| is.na(result_final)) #n = 1672

# Biologically implausible > 1,000 or NA
cirrho_wbc <- cirrho_wbc %>%
  filter(result_final <= 1000 )

hist(cirrho_wbc$result_final)

# Check units
table(cirrho_wbc$units)
cirrho_wbc %>%
  filter(units == '/cu.mm') # n = 12 NB to convert cubic mm to 10^9/L divide by 100 but values don't make sense

# Remove values in cu/mm
cirrho_wbc <- cirrho_wbc %>%
  filter(!grepl("/cu.mm", units))

hist(cirrho_wbc$result_final)
cirrho_50 <- cirrho_wbc %>%
  filter(result_final == 50 & grepl('RYJ', study_subject_id )) # n= 2,479 observation where WCC = 50 from same site with no other info
rm(cirrho_50)

# Remove erroneous entries of '50' where site is RYJ
cirrho_wbc <- cirrho_wbc %>%
  filter(!(result_final == 50 & grepl('RYJ', study_subject_id )))

hist(cirrho_wbc$result_final)

# Same with 100
cirrho_wbc_100 <- cirrho_wbc %>%
  filter(result_final ==100 & grepl('RYJ', study_subject_id ))
rm(cirrho_wbc_100)

# Remove erroneous entries of '100' where site is RYJ
cirrho_wbc <- cirrho_wbc %>%
  filter(!(result_final == 100 & grepl('RYJ', study_subject_id )))

hist(cirrho_wbc$result_final)

# create cleaned df
cirrho_wbc_during_study <- cirrho_wbc
rm(cirrho_wbc)


### Neutrophils (NEUB) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create new data set with only neut

cirrho_neut <- filter(labtest_cirrho, test_name_id == "NEUB")
length(unique(cirrho_neut$study_subject_id))

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_neut))
print(missing_counts)
rm(missing_counts)

# Merge result and result numerical

# Check Non-numeric neut
non_numeric_cirrho_neut <- cirrho_neut %>%
  filter(!grepl("^[0-9. ]+$", result))
table(non_numeric_cirrho_neut$result) # some with % prior to count

neut_percent <- cirrho_neut %>%
  filter(grepl("%", result ))

# Remove all values prior to %, keep remaining
cirrho_neut$result <- sub(".*%\\s*", "", cirrho_neut$result)
neut_percent <- cirrho_neut %>%
  filter(grepl("%", result )) # n = 30, units are appropriate (x109)

# Check non-numeric within result_numerical
non_numeric_cirrho_neut <- cirrho_neut %>%
  filter(!grepl("^[0-9. ]+$", result_numerical))
table(non_numeric_cirrho_neut$result_numerical) # no numeric results with associated text therefore OK to become NAs

# Make values numeric
cirrho_neut <- cirrho_neut %>%
  mutate(result_numerical = as.numeric(result_numerical),
         result = as.numeric(result))

# Show missing value counts to as expected (Nas introduced by coercion = those with non-numeric content)
missing_counts <- colSums(is.na(cirrho_neut))
print(missing_counts) # result na = 58 910, result_numerical na = 17 146 as expected
rm(missing_counts)


# Explore raw values
summary(cirrho_neut)
hist(cirrho_neut$result_numerical)
hist(cirrho_neut$result) 


# Merge 

cirrho_neut <- cirrho_neut %>%
  mutate(
    result_final = case_when(
      is.na(result_numerical) ~ result,  # Use result when result_numerical is missing
      is.na(result) ~ result_numerical,  # Use result_numerical when result is missing
      result == result_numerical ~ result,  # Use either if they are the same
      TRUE ~ NA_real_  # Placeholder for mismatched cases (numeric NA)
    ),
    mismatch_flag = ifelse(!is.na(result) & !is.na(result_numerical) & result != result_numerical, 1, 0)
  )
table(cirrho_neut$mismatch_flag) # no mismatches
sum(is.na(cirrho_neut$result_final))

# Explore raw values
summary(cirrho_neut)
hist(cirrho_neut$result_final)
cirrho_neut %>%
  filter(result_final > 1000 | is.na(result_final)) #n = 695

# Biologically implausible > 1,000 or NA
cirrho_neut <- cirrho_neut %>%
  filter(result_final <= 1000 )

hist(cirrho_neut$result_final)

# Check units
table(cirrho_neut$units)
cirrho_neut %>%
  filter(units == '%') # Need to exclude as % values, cannot infer absolute counts

# Remove values in %
cirrho_neut <- cirrho_neut %>%
  filter(!grepl("%", units))

cirrho_neut_during_study <- cirrho_neut
rm(cirrho_neut)

hist(cirrho_neut_during_study$result_final)

### GENERATE DATA FRAME WITH INCLUSION/END OF FU FOR ADULT CIRRHOSIS PT WITHIN STUDY WINDOW   ---------------------------------------------------------


# Add inclusion date (latest of diagnosis cirrhosis or >18 years)

cirrho_cohort_incl <- cirrho_cohort %>%
  left_join(Demographics.data %>% select(study_subject_id, year_of_birth),
            by = 'study_subject_id') %>%
  mutate(cutoff_birth = make_date(year_of_birth, 12, 31),
         inclusion_date = pmax(date_diagnosis, cutoff_date_18),
         age_at_inclusion = floor(time_length(interval(cutoff_birth, inclusion_date), "years"))) %>%
  select(-c(cutoff_birth, year_of_birth))

# Add end of follow up date (earliest of death, liver transplant, last data extract)
cirrho_cohort_incl_endoffu <- cirrho_cohort_incl %>%
  left_join(death.data %>%  select(study_subject_id, year_of_death),
            by = 'study_subject_id') %>%
  mutate(cutoff_death = make_date(year_of_death, 12, 31),
         end_of_fu = pmin(
           if_else(date_transplant >= date_diagnosis, date_transplant, as.Date(NA), missing = as.Date(NA)),
           extract_date, cutoff_death, na.rm = TRUE
         )
  ) 


# Final cirrho cohort with inclusion (latest of cirrhosis diagnosis and age >18 years) and end of fu date (earliest of liver transplant, death or last data extract)
cirrho_cohort_final <- cirrho_cohort_incl_endoffu %>%
  select(c(study_subject_id, inclusion_date, end_of_fu))


