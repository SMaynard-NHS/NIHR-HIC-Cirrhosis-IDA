# # Name: S Maynard
# Project: Liver HIC Project i 
# Version Number: V2.0
# Date: 2025/04/08
# File type - .R
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Data Source - Project i First part Project_i_IDA_ML: cirrho_cohort_final 

# CIRRHO COHORT FINAL = ALL ADULT PATIENTS WITH CIRRHOSIS DIAGNOSIS AT ANY TIME WITH INCL and END OF FU DATE

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

### GENERATE DESCRIPTIVE DATA FRAME WITH VARIABLES (ferritin, Hb, CRP) FOR ADULT CIRRHOSIS PT RELEVANT TO FERRITIN WITHIN STUDY WINDOW   ---------------------------------------------------------

# STEP 1: Filter variables 

#rm(cirrho_hb_desc)
#rm(cirrho_crp_desc)
#rm(cirrho_cfer_desc)

# Hb results
# Convert both data frames to data.tables
setDT(cirrho_hb_during_study)
setDT(cirrho_cohort_final)

# Perform efficient join and filtering for Hb results within study window
cirrho_hb_desc <- cirrho_hb_during_study[cirrho_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu] 


# CFER results
# Convert both data frames to data.tables
setDT(cirrho_cfer_during_study)
setDT(cirrho_cohort_final)

# Perform efficient join and filtering for Hb results within study window
cirrho_cfer_desc <- cirrho_cfer_during_study[cirrho_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu] 


# CRP results
# Convert both data frames to data.tables
setDT(cirrho_crp_during_study)
setDT(cirrho_cohort_final)

# Perform efficient join and filtering for Hb results within study window
cirrho_crp_desc <- cirrho_crp_during_study[cirrho_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu] 

#--------------------------------------

# Step 2: add to d.f

#rm(cirrho_desc)

## Add age at inclusion
cirrho_desc <- cirrho_cohort_final %>%
  left_join(cirrho_cohort_incl %>% select(study_subject_id, age_at_inclusion), by = 'study_subject_id')

hist(cirrho_desc$age_at_inclusion) # normally distributed

## Add gender
cirrho_desc <- cirrho_desc %>%
  left_join(cirrho_demographics %>% select(study_subject_id, gender_id_uncoded), by = 'study_subject_id')

## Add ethnicity
cirrho_desc <- cirrho_desc %>%
  left_join(cirrho_demographics %>% select(study_subject_id,ethnic_group), by ="study_subject_id")

#### Add nadir hb results

cirrho_desc <- cirrho_desc %>%
  left_join(cirrho_hb_desc %>% dplyr::select(study_subject_id, hb_value, Date), by = 'study_subject_id') %>%
  group_by(study_subject_id) %>%
  arrange(hb_value) %>%
  dplyr::slice(1) %>%
  dplyr::rename(hb_min = hb_value,
                hb_date = Date) %>%
  ungroup()


## Add anaemia groups based on Hb thresholds, age (all >18), and gender
cirrho_desc <- cirrho_desc %>%
  mutate(
    anaemia = case_when(
      is.na(hb_min) ~ "No Hb",                 # For missing ferritin 
      # Men
      gender_id_uncoded == "Male" & hb_min >= 110 & hb_min < 130 ~ "Mild",
      gender_id_uncoded == "Male" & hb_min >= 80 & hb_min < 110 ~ "Moderate",
      gender_id_uncoded == "Male" & hb_min < 80 ~ "Severe",
      
      # Women
      gender_id_uncoded == "Female" & hb_min >= 110 & hb_min < 120 ~ "Mild",
      gender_id_uncoded == "Female" & hb_min >= 80 & hb_min < 110 ~ "Moderate",
      gender_id_uncoded == "Female" & hb_min < 80 ~ "Severe",
      
      # Default to "No Anaemia" if Hb doesn't meet any criteria
      TRUE ~ "No Anaemia"
    )
  )
table(cirrho_desc$anaemia) # no hb n = 256


## add nadir ferritin results
cirrho_desc <- cirrho_desc %>%
  left_join(cirrho_cfer_desc %>% dplyr::select(study_subject_id, CFER_merged, Date), by = 'study_subject_id') %>%
  group_by(study_subject_id) %>%
  arrange(CFER_merged) %>%
  dplyr::slice(1) %>%
  dplyr::rename(cfer_min = CFER_merged,
                cfer_date = Date) %>%
  ungroup()

##add column ferritin yes or no
cirrho_desc <- cirrho_desc %>%
  mutate(ferritin_yn = if_else(!is.na(cfer_min),1,0))


## Classify ferritin 
cirrho_desc <- cirrho_desc %>%
  mutate(
    ferritin_group = case_when(
      is.na(cfer_min) ~ "No ferritin",                 # For missing ferritin 
      cfer_min < 30 ~ "<30",               
      cfer_min >= 30 & cfer_min < 50 ~ "<50",        
      cfer_min >= 50 & cfer_min <100 ~ "<100",
      TRUE ~ ">=100"                            
    )
  )
table(cirrho_desc$ferritin_group) # ferritin <30 n = 817, no ferritin n = 1799

# remove date cols
cirrho_desc<- cirrho_desc %>%
  select(-c(hb_date, cfer_date))


## Add length of follow up
cirrho_desc <- cirrho_desc %>%
  mutate(length_fu_years = round(time_length(interval(inclusion_date, end_of_fu), "years"),1))

hist(cirrho_desc$length_fu_years) # non-normal


## Add number of tests per patient
# Count tests per patient in each results table
hb_counts <- cirrho_hb_desc %>%
  count(study_subject_id, name = "hb_tests")

cfer_counts <- cirrho_cfer_desc %>%
  count(study_subject_id, name = "cfer_tests")  # prefer underscore in names

# Join into your patient-level table and replace NAs with 0
cirrho_desc <- cirrho_desc %>%
  left_join(hb_counts,   by = "study_subject_id") %>%
  left_join(cfer_counts, by = "study_subject_id") %>%
  mutate(
    hb_tests   = coalesce(hb_tests,   0L),
    cfer_tests = coalesce(cfer_tests, 0L)
  )

hist(cirrho_desc$hb_tests) # non-normal
hist(cirrho_desc$cfer_tests) # non-normal

# total tests

summary_table <- cirrho_desc %>%
  summarise(
    across(
      c(hb_tests, cfer_tests),
      list(
        total  = ~ sum(., na.rm = TRUE),
        q1     = ~ quantile(., 0.25, na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        q3     = ~ quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )) 

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# DESC TABLE 1 SPLIT BY FERRITIN TEST Y/N

# Order factor levels
cirrho_desc$anaemia <- factor(cirrho_desc$anaemia, levels = c("No Anaemia","Mild","Moderate","Severe","No Hb"))
cirrho_desc$ferritin_group <- factor(cirrho_desc$ferritin_group, levels = c("<30","<50","<100",">=100","No ferritin"))
cirrho_desc$ethnic_group <- factor(cirrho_desc$ethnic_group, levels = c("White","Asian","Black","Mixed","Other","Not stated"))


desc_table_one <- CreateTableOne(vars = c("age_at_inclusion","gender_id_uncoded","ethnic_group", "anaemia", "ferritin_group","length_fu_years"), 
                                 strata = "ferritin_yn",
                                 includeNA = T, 
                                 data = cirrho_desc,
                                 test = F,
                                 addOverall = T)


print(desc_table_one,
      nonnormal = "length_fu_years")

# Convert TableOne to a data frame (or matrix)
desc_table_one_df <- as.data.frame(print(desc_table_one,nonnormal = "length_fu_years", quote = FALSE, noSpaces = TRUE))

# Export the data frame to CSV
write.csv(desc_table_one_df, "desc_table_one.csv", row.names = T)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------


# BURDEN OF IDA
# No. pt who had ferritin and Hb within 90d prior to 14d post during study period

# Rename date cols to allow identification after join
# dplyr::rename columns
cirrho_cfer_desc <- cirrho_cfer_desc %>%
  select(-test_name_id) %>%
  dplyr::rename(cfer_date = Date,
                cfer_value = CFER_merged,
                cfer_units = units)

cirrho_hb_desc <- cirrho_hb_desc %>%
  select(-test_name_id) %>%
  dplyr::rename(hb_date = Date,
                hb_units = units)

## JOIN HB AND FERRITIN
# Add hb_value from within study window (inclusion - 90 to endoffu) to ferritin data based on matching study_subject_id and date 90 days prior or 14 days after (inclusive)
# RATIONALE: Beyond 14 days after may be treated with iron and 'baseline' HB/MCV/MCH etc values not representative of iron deficiency

# If date 2 (hb_date) is BEFORE date 1 (cfer_date) date_diff will be POSITIVE
# If date 2 (hb_date) is AFTER date 1 (cfer_date)) date_diff will be negative


#rm(cirrho_cfer_hb_desc)
cirrho_cfer_hb_desc <- cirrho_cfer_desc %>%
  left_join(cirrho_hb_desc %>% select(-c(inclusion_date, end_of_fu)), by = "study_subject_id", relationship = "many-to-many") %>%
  filter(difftime(cfer_date, hb_date, units = "days") <= 90 & difftime(cfer_date, hb_date, units = "days") >= 0) %>% # NB only 3 additional patients captured if extend to Hb up to 90d post ferritin
  mutate(cfer_hb_diffdays = difftime(cfer_date, hb_date, units = "days"))


# Label yn IDA diagnosis per patient at different cutoffs

# Binary outcome for every ferritin Hb pair within 90d : 1 if ferritin <30, 0 if not
# Of all with ferritin <30, binary outcome for closest Hb: 1 if Hb also anaemic, 0 if not

# Per patient, take closest Hb to each ferritin
# Where IDA criteria met, keep Hb-ferritin pair
# Where no IDA take closest Hb-ferritin pair

# Join gender for anaemia diagnosis 
cirrho_cfer_hb_desc <- cirrho_cfer_hb_desc %>%
  left_join(cirrho_demographics %>% select (study_subject_id, gender_id_uncoded), by = 'study_subject_id')


# For ferritin <30
cirrho_cfer_hb_closest <- cirrho_cfer_hb_desc %>%
  # Step 1: Create row-level binary outcome for fer level and IDA yn
  mutate(
    ida_30yn = if_else(
      (cfer_value < 30 & hb_value <= 120 & gender_id_uncoded == "Female") |
        (cfer_value < 30 & hb_value <= 130 & gender_id_uncoded == "Male"), 1, 0, missing = 0),
    ida_50yn = if_else(
      (cfer_value < 50 & hb_value <= 120 & gender_id_uncoded == "Female") |
        (cfer_value < 50 & hb_value <= 130 & gender_id_uncoded == "Male"), 1, 0, missing = 0),
    ida_100yn = if_else(
      (cfer_value < 100 & hb_value <= 120 & gender_id_uncoded == "Female") |
        (cfer_value < 100 & hb_value <= 130 & gender_id_uncoded == "Male"), 1, 0, missing = 0)
  ) %>%
  # Step 3: Select smallest diff days per patient per ferritin
  group_by(study_subject_id, cfer_date) %>% # results return one row per ferritin date for each patient
  arrange(abs(cfer_hb_diffdays), desc(cfer_hb_diffdays)) %>%  # Prioritise smallest diffdays, prefer positive on ties
  dplyr::slice(1) %>%
  ungroup() 


set.seed(123)  # for reproducible random tie-breaks

cirrho_desc_ida_yn <- cirrho_cfer_hb_closest %>%
  mutate(.priority = if_else(ida_30yn == 1L, 0L, 1L)) %>%                 # prefer ida_30yn==1
  arrange(study_subject_id, .priority, abs(cfer_hb_diffdays), runif(dplyr::n())) %>%
  distinct(study_subject_id, .keep_all = TRUE) %>%                      # keep first per patient
  select(-.priority)


table(cirrho_desc_ida_yn$ida_30yn) # IDA = 654 no IDA = 2816 (total n = 3470, IDA = 18.8%) NB only lose further 19 patients by removing Hb after, 2 patients by not extending Hb to beyond -90 days
table(cirrho_desc_ida_yn$ida_50yn) # IDA <50 n = 791
table(cirrho_desc_ida_yn$ida_100yn) # IDA <100 n = 973


## Add variable indicating if Hb 90d prior to 14d post ferritin to cirrho_desc d.f
cirrho_desc <- cirrho_desc %>%
  mutate(hb_fer_90 = ifelse(study_subject_id %in% cirrho_cfer_hb_desc$study_subject_id, 1, 0))

table(cirrho_desc$hb_fer_90) # hb_fer_90 n = 3470 as expected

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ADDITIONAL CODE 05/03/2026 - DISTRIBUTION SMALLEST DATE DIFF BETWEEN FERRITIN-HB PAIRS
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# scatter of date diff (x) by number of observations (y)
library(ggplot2)

str(cirrho_desc_ida_yn)
  
# Convert difftime to numeric (days)
cirrho_desc_ida_yn_plot <- cirrho_desc_ida_yn %>% 
  mutate(cfer_hb_diffdays_num = as.numeric(cfer_hb_diffdays))

# Create frequency table stratified by IDA
cirrho_desc_ida_yn_plot_data <- cirrho_desc_ida_yn_plot %>%
  group_by(ida_30yn, cfer_hb_diffdays_num) %>%
  summarise(frequency = n(), .groups = "drop")

# Scatter plot
ggplot(cirrho_desc_ida_yn_plot_data,
       aes(x = cfer_hb_diffdays_num,
           y = frequency,
           color = factor(ida_30yn))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    x = "cfer_hb_diffdays (days)",
    y = "Frequency",
    color = "IDA Status",
    title = "Frequency of cfer_hb_diffdays by IDA Status"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("0" = "No IDA", "1" = "IDA")) +
  theme_minimal()

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# REMAINING CODE REMOVED - NO LONGER UTILISED IN ANALYSIS BUT AVAILABLE FROM PREVIOUS SCRIPTS (latest: Project_i_cirrho_desc_1aNOPRIOR_20251117.R)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------