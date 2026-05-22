# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# GENERATE ML DATA FRAME WITH VARIABLES ANCHORED TO CFER IN STUDY WINDOW
# NB need to limit other variables to same end of fu date
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# NB CIRRHO_DESC_IDA_YN = ALL ELIGIBLE PATIENTS (DIGANOSIS CIRRHOSIS AND >18Y) WITH FERRITIN WITHIN STUDY PERIOD (90d PRIOR TO ELIGIBILITY TO END OF FU) AND HB WITHIN 90D PRIOR OR 14D POST FERRITIN (but prior to endoffu)
# is appropriate data frame to filter IDA_ML d.f on
# total n = 3470, IDA FER 30 = 654  


## QUICK CHECKS
# Unique study ID within filtered CFER to study window
length(unique(cirrho_cfer_desc$study_subject_id)) # n = 3502

# Explore proportion of patients who have ferritin <30 recorded at any time
cirrho_cfer_desc %>%
  group_by(study_subject_id) %>%
  summarise(low_cfer = any(cfer_value < 30, na.rm = TRUE), .groups = "drop") %>%
  summarise(
    n_patients   = n(),
    n_low_cfer   = sum(low_cfer),
    pct_with_low = 100 * n_low_cfer / n_patients
  ) 
# n = 817 ferritin <30 of n = 3502 with cfer result (23.3%)
# (With tx - n = 925 ferritin <30 of n = 3641 with cfer result (25.4%))


cirrho_cfer_hb_desc %>%
  summarise(n_distinct(study_subject_id)) # n = 3470 
# NB only lose 12 patients form where ferritin has no clinically relevant Hb (from total 3502 with cfer result)


# Explore proportion with ferritin <30 of all with clinically relevant Hb
cirrho_cfer_hb_desc %>%
  group_by(study_subject_id) %>%
  summarise(low_cfer = any(cfer_value < 30, na.rm = TRUE), .groups = "drop") %>%
  summarise(
    n_patients   = n(),
    n_low_cfer   = sum(low_cfer),
    pct_with_low = 100 * n_low_cfer / n_patients
  ) 

# n = 811 ferritin <30 of n = 3470 with cfer-Hb paired result (23.4%)
# only only 5 patients with ferritin <30 and cirrhosis diagnosis did not have Hb within clinically relevant time frame (<=90d prior or same day))
# (with tx - n = 923 ferritin <30 of n = 3631 with cfer result (25.4%))
# (with tx - only 2 patients with ferritin <30 and cirrhosis diagnosis did not have Hb within clinically relevant time frame (<=90d prior or <=14d post))


# Use cirrho_desc_ida_yn for yn IDA diagnosis per patient 
cirrho_desc_ida_yn %>%
  summarise(n_distinct(study_subject_id))
table(cirrho_desc_ida_yn$ida_30yn)
# total n = 3470, ida = 654, no ida = 2816 (therefore 157 have NAID fer <30 (811-654))
# (with tx - total n = 3631, ida = 778, no ida = 2853 (therefore 145 have NAID fer <30 (923-778)))

# Check diff days overall
cirrho_desc_ida_yn %>%
  filter(abs(cfer_hb_diffdays) > 14) # Nov '25 n = 26 patients with IDA who's Hb and ferritin are more than 14 days apart

hist(as.numeric(cirrho_desc_ida_yn$cfer_hb_diffdays, units = "days")) # "Histogram of Days Between Hb and Ferritin results"

# Check diff days between inclusion date and ferritin date
cirrho_desc_ida_yn <- cirrho_desc_ida_yn %>%
  mutate(cfer_incl = cfer_date - inclusion_date)
cirrho_desc_ida_yn %>%
  filter(cfer_incl <= -45) # n = 275 with remaining look back only 45 days in initial study window

##### CREATE NEW DF CIRRHO_IDA_ML -------------------------------------------------------------------

# cirrho cohort IDA subset
cirrho_ida_cohort_final <- cirrho_desc_ida_yn %>%
  select("study_subject_id","inclusion_date","end_of_fu")

# study_subject_id, gender_id_uncoded, year_of_birth, ethnicity, hb_date, hb_value, cfer_date, cfer_value, ida_yn
cirrho_ida_ml <- cirrho_desc_ida_yn %>%
  left_join(cirrho_demographics %>% dplyr::select(study_subject_id, year_of_birth, ethnic_group), by = 'study_subject_id') %>%
  dplyr::select(study_subject_id, gender_id_uncoded, year_of_birth, ethnic_group, hb_date, hb_value, cfer_date, cfer_value, ida_30yn) %>%
  rename(ida_yn = 'ida_30yn')


# Add variables:
# Age at ferritin
cirrho_ida_ml <- cirrho_ida_ml %>%
  mutate(cfer_year = year(cfer_date), #Extract year from cfer date numeric
         age = cfer_year - year_of_birth) %>%
  dplyr::select(-c(cfer_year, year_of_birth)) %>%
  relocate(age, .after =3) %>%
  dplyr::rename(hb_fer_date = hb_date,
                hb_fer_value = hb_value)

n_distinct(cirrho_ida_ml$study_subject_id)

# Hb >= 90d prior

# Filter all Hb during study to prior to endoffu
# Convert both data frames to data.tables
setDT(cirrho_hb_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_hb_endfu <- cirrho_hb_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date <= end_of_fu] %>%
  rename(hb_date = 'Date')


# Join 
cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_hb_endfu %>% dplyr::select(study_subject_id, hb_date, hb_value), by = 'study_subject_id') %>%
  mutate(hb_3m_yn = if_else (
    difftime(hb_fer_date, hb_date, units = "days") >= 90, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  dplyr::group_by(study_subject_id) %>%
  mutate(number_hb_3m = sum(hb_3m_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(hb_3m_yn >= 1 | number_hb_3m == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  dplyr::group_by(study_subject_id) %>%
  arrange(desc(hb_date)) %>%  # Prioritise most recent date
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not 90d prior make NA
  mutate(hb_value = if_else(hb_3m_yn == 1, hb_value, NA, missing = NA))

# Delta Hb
cirrho_ida_ml <- cirrho_ida_ml %>%
  mutate(delta_hb = ifelse(number_hb_3m >=1, hb_fer_value - hb_value, NA))

# Remove unecessary columns
cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(hb_date, hb_value, hb_3m_yn, number_hb_3m))



# MCV during study window and within 90d prior or day of CFER
# Filter all MCV during study to within study window

# Convert both data frames to data.tables
setDT(cirrho_mcv_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_mcv_desc <- cirrho_mcv_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(mcv_date = 'Date',
         mcv_value = 'result_final')

# Extract closest MCV to ferritin
cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_mcv_desc %>% dplyr::select(study_subject_id, mcv_date, mcv_value), by = 'study_subject_id') %>%
  mutate(mcv_cfer_diffdays = difftime(cfer_date, mcv_date, units = "days"),
         mcv_cfer_yn = if_else (mcv_cfer_diffdays <= 90 & mcv_cfer_diffdays>=0, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(number_mcv_cfer = sum(mcv_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(mcv_cfer_yn == 1 | number_mcv_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(mcv_cfer_diffdays), desc(mcv_cfer_diffdays)) %>%  # Prioritise smallest absolute date diff, prefer positive on ties (e.g +2 before -2)
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value and date diff not within 90d prior make NA
  mutate(mcv_value = if_else(mcv_cfer_yn == 1, mcv_value, NA, missing = NA),
         mcv_cfer_diffdays = if_else(mcv_cfer_yn == 1, mcv_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$mcv_cfer_yn) # only 9 have no mcv within 90d prior or day of ferritin (lost 4 by limiting to study window, additional 3 by keeping to 90d pre or day of)
hist(as.numeric(cirrho_ida_ml$mcv_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$mcv_cfer_diffdays)
range(cirrho_ida_ml$mcv_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::rename(mcv_cfer_date = mcv_date,
                mcv_cfer_value = mcv_value) %>%
  dplyr::select(-c(mcv_cfer_diffdays, number_mcv_cfer)) # remove unnecessary columns


# MCV > 90d prior

# Filter all Hb during study to prior to endoffu
# Convert both data frames to data.tables
setDT(cirrho_mcv_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_mcv_during_study <- cirrho_mcv_during_study %>%
  rename(mcv_date = 'Date',
         mcv_value = 'result_final')

cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_mcv_during_study %>% dplyr::select(study_subject_id, mcv_date, mcv_value), by = 'study_subject_id') %>%
  mutate(mcv_3m_yn = if_else (
    difftime(mcv_cfer_date, mcv_date, units = "days") >= 90, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(number_mcv_3m = sum(mcv_3m_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(mcv_3m_yn == 1 | number_mcv_3m == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(desc(mcv_date)) %>%  # Prioritise most recent date
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not 90d prior make NA
  mutate(mcv_value = if_else(mcv_3m_yn == 1, mcv_value, NA, missing = NA))


table(cirrho_ida_ml$mcv_3m_yn) # n = 2850 have mcv >=90 day prior

# check to see distribution of missingness in IDA y/n
cirrho_ida_ml %>%
  count(mcv_3m_yn, ida_yn) %>%
  group_by(ida_yn) %>%
  mutate(pct = 100 * n / sum(n))


# Delta MCV
cirrho_ida_ml <- cirrho_ida_ml %>%
  mutate(delta_mcv = ifelse(mcv_3m_yn ==1 & mcv_cfer_yn == 1, mcv_cfer_value - mcv_value, NA))

# Remove unnecessary columns
cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(mcv_cfer_yn, mcv_date, mcv_value, mcv_3m_yn, number_mcv_3m, mcv_cfer_date)) 

# MCH within 90d prior or dya of CFER during study window

# Filter all MCV during study to within study window
# Convert both data frames to data.tables
setDT(cirrho_mch_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_mch_desc <- cirrho_mch_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(mch_date = 'Date',
         mch_value = 'result_final')


# Add mch_value to Hb_fer based on matching study_subject_id and date within 90 days
cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_mch_desc %>% dplyr::select(study_subject_id, mch_date, mch_value), by = 'study_subject_id') %>%
  mutate(mch_cfer_diffdays = difftime(cfer_date, mch_date, units = "days"),
         mch_cfer_yn = if_else (mch_cfer_diffdays <= 90 & mch_cfer_diffdays>=0, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(number_mch_cfer = sum(mch_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(mch_cfer_yn == 1 | number_mch_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(mch_cfer_diffdays), desc(mch_cfer_diffdays)) %>%  # Prioritise smallest date diff then prioritise positives if ties
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value and date diff not within 90d prior or 14d post make NA
  mutate(mch_value = if_else(mch_cfer_yn == 1, mch_value, NA, missing = NA),
         mch_cfer_diffdays = if_else(mch_cfer_yn == 1, mch_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$mch_cfer_yn) # only 11 have no mch within 90d prior or 14d post ferritin (lost 4 by limiting to study window, addiitonal 2 not after cfer)
hist(as.numeric(cirrho_ida_ml$mch_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$mch_cfer_diffdays)
range(cirrho_ida_ml$mch_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::rename(mch_cfer_date = mch_date,
                mch_cfer_value = mch_value) %>%
  dplyr::select(-c(mch_cfer_diffdays, number_mch_cfer)) # remove unnecessary columns


# mch > 90d prior
cirrho_mch_during_study <- cirrho_mch_during_study %>%
  dplyr::rename(mch_date = Date,
                mch_value = result_final)

cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_mch_during_study %>% dplyr::select(study_subject_id, mch_date, mch_value), by = 'study_subject_id') %>%
  mutate(mch_3m_yn = if_else (
    difftime(mch_cfer_date, mch_date, units = "days") >= 90, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(number_mch_3m = sum(mch_3m_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(mch_3m_yn == 1 | number_mch_3m == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(desc(mch_date)) %>%  # Prioritise most recent date
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not 90d prior make NA
  mutate(mch_value = if_else(mch_3m_yn == 1, mch_value, NA, missing = NA))


table(cirrho_ida_ml$mch_3m_yn) # n = 2850 

# Delta mch
cirrho_ida_ml <- cirrho_ida_ml %>%
  mutate(delta_mch = ifelse(mch_3m_yn ==1 & mch_cfer_yn == 1, mch_cfer_value - mch_value, NA))

# Remove unnecessary columns
cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(mch_cfer_yn, mch_date, mch_value, mch_3m_yn, number_mch_3m, mch_cfer_date)) 



# Pl within 90d ferritin

# Add pl_value to cfer based on matching study_subject_id and date within 90 days prior or day of within study window

# Filter all MCV during study to within study window
# Convert both data frames to data.tables
setDT(cirrho_pl_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_pl_desc <- cirrho_pl_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(pl_date = 'Date',
         pl_value = 'PL_clinical')


cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_pl_desc %>% dplyr::select(study_subject_id, pl_date, pl_value), by = 'study_subject_id') %>%
  mutate(pl_cfer_diffdays = difftime(cfer_date, pl_date, units = "days"),
         pl_cfer_yn = if_else (pl_cfer_diffdays <= 90 & pl_cfer_diffdays>=0, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(number_pl_cfer = sum(pl_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(pl_cfer_yn == 1 | number_pl_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(pl_cfer_diffdays), desc(pl_cfer_diffdays)) %>%  # Prioritise smallest date diff then by positive if ties
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value and date diff not within 90d prior or 14d post make NA
  mutate(pl_value = if_else(pl_cfer_yn == 1, pl_value, NA, missing = NA),
         pl_cfer_diffdays = if_else(pl_cfer_yn == 1, pl_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$pl_cfer_yn) # only 9 have no pl within 90d of cfer (same, 3 less without using results after cfer)
hist(as.numeric(cirrho_ida_ml$pl_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$pl_cfer_diffdays)
range(cirrho_ida_ml$pl_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::rename(pl_fer_date = pl_date,
                pl_fer_value = pl_value) %>%
  dplyr::select(-c(pl_cfer_diffdays, number_pl_cfer)) # remove unnecessary columns


# pl > 90d prior
cirrho_pl_during_study <- cirrho_pl_during_study %>%
  dplyr::rename(pl_date = Date,
                pl_value = PL_clinical)

cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_pl_during_study %>% dplyr::select(study_subject_id, pl_date, pl_value), by = 'study_subject_id') %>%
  mutate(pl_3m_yn = if_else (
    difftime(pl_fer_date, pl_date, units = "days") >= 90, 1, 0, missing = 0 )
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(number_pl_3m = sum(pl_3m_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(pl_3m_yn == 1 | number_pl_3m == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(desc(pl_date)) %>%  # Prioritise most recent date
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not 90d prior make NA
  mutate(pl_value = if_else(pl_3m_yn == 1, pl_value, NA, missing = NA))


table(cirrho_ida_ml$pl_3m_yn) # n = 2847 pl within 3m 

# Delta pl # NB unlikely clinically useful
cirrho_ida_ml <- cirrho_ida_ml %>%
  mutate(delta_pl = ifelse(pl_3m_yn ==1 & pl_cfer_yn == 1, pl_fer_value - pl_value, NA))

# remove columns
cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(pl_cfer_yn, pl_date, pl_value, pl_3m_yn, number_pl_3m, pl_fer_date)) 

# INR (within 90d ferritin)

# Filter all INR during study to within study window
# Convert both data frames to data.tables
setDT(cirrho_hinr_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_hinr_desc <- cirrho_hinr_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(hinr_date = 'Date',
         hinr_value = 'HINR_clinical')


cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_hinr_desc %>% dplyr::select(study_subject_id, hinr_date, hinr_value), by = 'study_subject_id') %>%
  mutate(hinr_cfer_diffdays = difftime(cfer_date, hinr_date, units = "days"),
         hinr_cfer_yn = if_else (hinr_cfer_diffdays <= 90 & hinr_cfer_diffdays >=0, 1, 0, missing = 0)
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(patient_level_hinr_cfer = max(hinr_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(hinr_cfer_yn == 1 | patient_level_hinr_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(hinr_cfer_diffdays), desc(hinr_cfer_diffdays)) %>%  # Prioritise smallest date diff then prioritise positive
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not within 90d make NA
  mutate(hinr_value = if_else(hinr_cfer_yn == 1, hinr_value, NA, missing = NA),
         hinr_cfer_diffdays = if_else(hinr_cfer_yn == 1, hinr_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$hinr_cfer_yn) # 712 have no INR within 90d of cfer (addiitonal308 lost)
hist(as.numeric(cirrho_ida_ml$hinr_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$hinr_cfer_diffdays)
range(cirrho_ida_ml$hinr_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(hinr_cfer_diffdays, patient_level_hinr_cfer, hinr_date, hinr_cfer_diffdays, hinr_cfer_yn)) # remove unnecessary columns


# CRP within 2 weeks of ferritin

# Filter all INR during study to within study window
# Convert both data frames to data.tables
setDT(cirrho_crp_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_crp_desc <- cirrho_crp_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(crp_date = 'Date',
         crp_value = 'result_final')


cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_crp_desc %>% dplyr::select(study_subject_id, crp_date, crp_value), by = 'study_subject_id') %>%
  mutate(crp_cfer_diffdays = difftime(cfer_date, crp_date, units = "days"),
         crp_cfer_yn = if_else (crp_cfer_diffdays <= 14 & crp_cfer_diffdays >=0, 1, 0, missing = 0)
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(patient_level_crp_cfer = max(crp_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(crp_cfer_yn == 1 | patient_level_crp_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(crp_cfer_diffdays), desc(crp_cfer_diffdays)) %>%  # Prioritise smallest date diff, positive if ties
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not within 90d make NA
  mutate(crp_value = if_else(crp_cfer_yn == 1, crp_value, NA, missing = NA),
         crp_cfer_diffdays = if_else(crp_cfer_yn == 1, crp_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$crp_cfer_yn) # 1539 have no CRP within 90d of cfer (additional 233 lost)
hist(as.numeric(cirrho_ida_ml$crp_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$crp_cfer_diffdays)
range(cirrho_ida_ml$crp_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(crp_cfer_diffdays, patient_level_crp_cfer, crp_date, crp_cfer_diffdays, crp_cfer_yn)) # remove unnecessary columns

# Creat (closest within 90d ferritin)

# Filter all creat during study to within study window
# Convert both data frames to data.tables
setDT(cirrho_creat_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_creat_desc <- cirrho_creat_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(creat_date = 'Date',
         creat_value = 'result_final')

cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_creat_desc %>% dplyr::select(study_subject_id, creat_date, creat_value), by = 'study_subject_id') %>%
  mutate(creat_cfer_diffdays = difftime(cfer_date, creat_date, units = "days"),
         creat_cfer_yn = if_else (creat_cfer_diffdays <= 90 &creat_cfer_diffdays >=0, 1, 0, missing = 0)
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(patient_level_creat_cfer = max(creat_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(creat_cfer_yn == 1 | patient_level_creat_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(creat_cfer_diffdays), desc(creat_cfer_diffdays)) %>%  # Prioritise smallest date diff then prioritise positive if ties
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not within 180d make NA
  mutate(creat_value = if_else(creat_cfer_yn == 1, creat_value, NA, missing = NA),
         creat_cfer_diffdays = if_else(creat_cfer_yn == 1, creat_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$creat_cfer_yn) # n= 1367 no creat within 90d cfer (NB 1432 have no creat within 180d of cfer, additional 6 lost by not including results post cfer)
hist(as.numeric(cirrho_ida_ml$creat_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$creat_cfer_diffdays)
range(cirrho_ida_ml$creat_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(creat_cfer_diffdays, patient_level_creat_cfer, creat_date, creat_cfer_diffdays, creat_cfer_yn)) # remove unnecessary colum

# Bilirubin (closest within 90d to ferritin)

# Filter all bili during study to within study window
# Convert both data frames to data.tables
setDT(cirrho_bili_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_bili_desc <- cirrho_bili_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(bili_date = 'Date',
         bili_value = 'result_final')

cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_bili_desc %>% dplyr::select(study_subject_id, bili_date, bili_value), by = 'study_subject_id') %>%
  mutate(bili_cfer_diffdays = difftime(cfer_date, bili_date, units = "days"),
         bili_cfer_yn = if_else (bili_cfer_diffdays <= 90 & bili_cfer_diffdays >=0, 1, 0, missing = 0)
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(patient_level_bili_cfer = max(bili_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(bili_cfer_yn == 1 | patient_level_bili_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(bili_cfer_diffdays), desc(bili_cfer_diffdays)) %>%  # Prioritise smallest date diff then prioritise positive if ties
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not within 90d make NA
  mutate(bili_value = if_else(bili_cfer_yn == 1, bili_value, NA, missing = NA),
         bili_cfer_diffdays = if_else(bili_cfer_yn == 1, bili_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$bili_cfer_yn) # n= 141 no bili within 90d cfer (additional 15 lost not using results post cfer)
hist(as.numeric(cirrho_ida_ml$bili_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$bili_cfer_diffdays)
range(cirrho_ida_ml$bili_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(bili_cfer_diffdays, patient_level_bili_cfer, bili_date, bili_cfer_diffdays, bili_cfer_yn)) # remove unnecessary column


# Bmi (closest within 90d to ferritin)
# Filter all BMI during study to within study window
# Convert both data frames to data.tables
setDT(bmi_cirrho)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_bmi_desc <- bmi_cirrho[cirrho_ida_cohort_final, on = "study_subject_id"][date >= (inclusion_date - 90) & date <= end_of_fu]%>%
  rename(bmi_date = 'date',
         bmi_value = 'final_value')


cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_bmi_desc %>% dplyr::select(study_subject_id, bmi_date, bmi_value), by = 'study_subject_id') %>%
  mutate(bmi_cfer_diffdays = difftime(cfer_date, bmi_date, units = "days"),
         bmi_cfer_yn = if_else (bmi_cfer_diffdays <= 90 & bmi_cfer_diffdays >=-90, 1, 0, missing = 0)
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(patient_level_bmi_cfer = max(bmi_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(bmi_cfer_yn == 1 | patient_level_bmi_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(bmi_cfer_diffdays), desc(bmi_cfer_diffdays)) %>%  # Prioritise smallest date diff then by most recent date
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not within 90d make NA
  mutate(bmi_value = if_else(bmi_cfer_yn == 1, bmi_value, NA, missing = NA),
         bmi_cfer_diffdays = if_else(bmi_cfer_yn == 1, bmi_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$bmi_cfer_yn) # n= 1857 no bmi within 90d cfer
hist(as.numeric(cirrho_ida_ml$bmi_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$bmi_cfer_diffdays)
range(cirrho_ida_ml$bmi_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(bmi_cfer_diffdays, patient_level_bmi_cfer, bmi_date, bmi_cfer_yn)) # remove unnecessary column

# wbc (closest within 90d to ferritin)

# Filter all wbc during study to within study window
# Convert both data frames to data.tables
#setDT(cirrho_wbc_during_study)
#setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
#cirrho_wbc_desc <- cirrho_wbc_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
#  rename(wbc_date = 'Date',
#         wbc_value = 'result_final')

# Add wbc_value to cfer based on matching study_subject_id and closest date
#cirrho_ida_ml <- cirrho_ida_ml %>%
#  left_join(cirrho_wbc_desc %>% dplyr::select(study_subject_id, wbc_date, wbc_value), by = 'study_subject_id') %>%
#  mutate(wbc_cfer_diffdays = difftime(cfer_date, wbc_date, units = "days"),
#         wbc_cfer_yn = if_else (wbc_cfer_diffdays <= 90 & wbc_cfer_diffdays >=0, 1, 0, missing = 0)
#  )  %>%
  # Step 2: Assign patient-level outcome
#  group_by(study_subject_id) %>%
#  mutate(patient_level_wbc_cfer = max(wbc_cfer_yn)) %>%  # 1 if any row is 1, else 0
#  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
#  filter(wbc_cfer_yn == 1 | patient_level_wbc_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
#  group_by(study_subject_id) %>%
#  arrange(abs(wbc_cfer_diffdays), desc(wbc_cfer_diffdays)) %>%  # Prioritise smallest date diff then by most recent date
#  dplyr::slice(1) %>%
#  ungroup() %>%
  # Step 5: If value not within 90d make NA
#  mutate(wbc_value = if_else(wbc_cfer_yn == 1, wbc_value, NA, missing = NA),
#         wbc_cfer_diffdays = if_else(wbc_cfer_yn == 1, wbc_cfer_diffdays, NA, missing = NA))

#table(cirrho_ida_ml$wbc_cfer_yn) # n= 839 no wbc within 90d cfer (same)
#hist(as.numeric(cirrho_ida_ml$wbc_cfer_diffdays, units = "days"))
#summary(cirrho_ida_ml$wbc_cfer_diffdays)
#range(cirrho_ida_ml$wbc_cfer_diffdays, na.rm = TRUE)

#cirrho_ida_ml <- cirrho_ida_ml %>%
#  dplyr::select(-c(wbc_cfer_diffdays, patient_level_wbc_cfer, wbc_date, wbc_cfer_diffdays, wbc_cfer_yn)) # remove unnecessary column

# neut (closest within 90d to ferritin)

# Convert both data frames to data.tables
setDT(cirrho_neut_during_study)
setDT(cirrho_ida_cohort_final)

# Perform efficient join and filtering for Hb results prior to endoffu
cirrho_neut_desc <- cirrho_neut_during_study[cirrho_ida_cohort_final, on = "study_subject_id"][Date >= (inclusion_date - 90) & Date <= end_of_fu]%>%
  rename(neut_date = 'Date',
         neut_value = 'result_final')

# Add neut_value to cfer based on matching study_subject_id and closest date

cirrho_ida_ml <- cirrho_ida_ml %>%
  left_join(cirrho_neut_desc %>% dplyr::select(study_subject_id, neut_date, neut_value), by = 'study_subject_id') %>%
  mutate(neut_cfer_diffdays = difftime(cfer_date, neut_date, units = "days"),
         neut_cfer_yn = if_else (neut_cfer_diffdays <= 90 & neut_cfer_diffdays >=0, 1, 0, missing = 0)
  )  %>%
  # Step 2: Assign patient-level outcome
  group_by(study_subject_id) %>%
  mutate(patient_level_neut_cfer = max(neut_cfer_yn)) %>%  # 1 if any row is 1, else 0
  ungroup() %>%
  # Step 3: Keep only relevant rows for filtering
  filter(neut_cfer_yn == 1 | patient_level_neut_cfer == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  # Step 4: dplyr::select best row per patient
  group_by(study_subject_id) %>%
  arrange(abs(neut_cfer_diffdays), desc(neut_cfer_diffdays)) %>%  # Prioritise smallest date diff then by most recent date
  dplyr::slice(1) %>%
  ungroup() %>%
  # Step 5: If value not within 90d make NA
  mutate(neut_value = if_else(neut_cfer_yn == 1, neut_value, NA, missing = NA),
         neut_cfer_diffdays = if_else(neut_cfer_yn == 1, neut_cfer_diffdays, NA, missing = NA))

table(cirrho_ida_ml$neut_cfer_yn) # n= 458 no neut within 90d cfer
hist(as.numeric(cirrho_ida_ml$neut_cfer_diffdays, units = "days"))
summary(cirrho_ida_ml$neut_cfer_diffdays)
range(cirrho_ida_ml$neut_cfer_diffdays, na.rm = TRUE)
IQR(cirrho_ida_ml$neut_cfer_diffdays, na.rm = TRUE)

cirrho_ida_ml <- cirrho_ida_ml %>%
  dplyr::select(-c(neut_cfer_diffdays, patient_level_neut_cfer, neut_date, neut_cfer_diffdays, neut_cfer_yn)) # remove unnecessary column


# Move outcome to end
cirrho_ida_ml <- cirrho_ida_ml %>%
  relocate(ida_yn, .after = last_col())


# CCA and predicted useful variables for ML
ml_cca <- cirrho_ida_ml %>%
  filter(!is.na(delta_hb) & !is.na(mcv_cfer_value) & !is.na(delta_mcv) & !is.na(pl_fer_value)) %>%
  dplyr::select(c(study_subject_id, gender_id_uncoded, ethnic_group, age, hb_fer_value, delta_hb, mcv_cfer_value, delta_mcv, pl_fer_value, cfer_value, ida_yn))

table(ml_cca$ida_yn) # total n = 2835, no ida n = 2252, ida n = 583 (lost 9 patients from CCA)


# explore cirrhosis diagnoses
#cirrho_cohort_diagnoses <- cirrho_cohort_final %>%
#  left_join(icd_diagnosis.data, by = 'study_subject_id') %>%
#  filter(cirrhosis_icd == 1 | cirrhosis_desc ==1 | cirrhosis_snomed ==1) %>%
#  dplyr::select(study_subject_id, date_diagnosis, icd_code, cirrhosis_icd_date, snomed_code, cirrhosis_snomed_date, diagnosis_description, cirrhosis_desc_date) #%>%
#summarise(n_distinct(study_subject_id)) # n = 5302

#table(cirrho_cohort_diagnoses$icd_code)

# Coded bleeding varices (up to date of ferritin) - icd - I850, I864, I982, snomedct - , diag desc - bleed varice
# Cause of cirrhosis
# Evidence decomp/comp (up to date of ferritin)

### All with IDA vs not
table(cirrho_ida_ml$ida_yn) # n = 654

#### Anaemic patients as total cohort ?limit to those with anaemia
#anaemia_cirrho_ida_ml <- cirrho_ida_ml %>%
# filter(hb_fer_value <= 130)  # n = 2343
#table(anaemia_cirrho_ida_ml$ida_yn) # ida = 778 no ida = 1554



##### Descriptive groups -----------------------------------------------------------------------------------

# Table one for IDA vs No IDA ------------------------------------------------------------------------------

# Set factor levels
cirrho_ida_ml$ethnic_group <- factor(cirrho_ida_ml$ethnic_group, levels = c("White","Asian","Black","Mixed","Other","Not stated"))

# Define variable lists
cat_vars_ml <- names(cirrho_ida_ml)[grepl("gender|ethnic", names(cirrho_ida_ml), ignore.case = TRUE)]
cont_vars_ml <- c("age", "hb_fer_value", "delta_hb", "mcv_cfer_value", "delta_mcv", "mch_cfer_value", "delta_mch", "pl_fer_value", "delta_pl", "hinr_value", "bili_value", "bmi_value", "crp_value", "creat_value", "neut_value")


# Define non-normal vars (from histograms)
nonnormal_vars <- c('bili_value','creat_value','crp_value','hinr_value','neut_value','pl_fer_value')


library(tableone)
table_one_ml <- CreateTableOne(vars = c(cat_vars_ml, cont_vars_ml), 
                               strata = "ida_yn",
                               includeNA = F, 
                               data = cirrho_ida_ml, 
                               test = F,
                               addOverall = F)
summary(table_one_ml)
print(table_one_ml, nonnormal = nonnormal_vars, showAllLevels = T, missing = T)

# Convert TableOne to a data frame (or matrix)
table_one_ml_df <- as.data.frame(print(table_one_ml, nonnormal = nonnormal_vars, quote = FALSE, noSpaces = TRUE, showAllLevels = T, missing = T))

# Export the data frame to CSV
write.csv(table_one_ml_df, "table_one_ml_no_prior.csv", row.names = T)
# Bmi, creat not as signif (P>0.01), ethnicity, age, plt, delta plt and INR p>0.05
# Keep age regardless as relevant to threshold

# Continuous variables----------------------------------------------------------------------------------

# Select numeric variables excluding 'ida_yn'
cirrho_ida_ml_long <- cirrho_ida_ml %>%
  pivot_longer(cols = c(age, hb_fer_value, delta_hb, mcv_cfer_value, delta_mcv, mch_cfer_value, delta_mch, pl_fer_value, delta_pl, hinr_value, bili_value, bmi_value, crp_value, creat_value, neut_value), 
               names_to = "Variable", 
               values_to = "Value")

# Summarise 
summary(cirrho_ida_ml)

# Checking normality

# Shapiro test
# Pick out numeric (continuous) variables
num_vars <- sapply(cirrho_ida_ml, is.numeric)

# Run Shapiro-Wilk test on each numeric variable
shapiro_list <- lapply(cirrho_ida_ml[, num_vars], shapiro.test)

# Tidy the results into a data frame
shapiro_results <- data.frame(
  variable = names(shapiro_list),
  W        = sapply(shapiro_list, function(x) unname(x$statistic)),
  p_value  = sapply(shapiro_list, function(x) x$p.value),
  row.names = NULL
)

shapiro_results # significant for all (!!)

# Histogram for all continuous variables
ggplot(cirrho_ida_ml_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Histograms of Variables",
       x = "Value",
       y = "Count") +
  theme_minimal()

# QQ plots of all continuous variables
# Select numeric (continuous) variables
num_vars <- sapply(cirrho_ida_ml, is.numeric)
num_names <- names(cirrho_ida_ml)[num_vars]

# Set layout: e.g. 2x2 plots per page
par(mfrow = c(2, 2))

for (v in num_names) {
  qqnorm(cirrho_ida_ml[[v]], 
         main = paste("Q-Q plot of", v))
  qqline(cirrho_ida_ml[[v]])
}


# Non-normal: bili, creatinine, crp, inr (hinr),bmi (NB ferritin excluded as not covariate)  
# Some positive skew neut, plt
# NB creat, crp, bmi excluded due to missingness
# **remaining non-normal predictors are bili, inr, neut, plt**

# Log transform non-normal variable within cirrho_ida_ml
#cirrho_ida_ml <- cirrho_ida_ml %>%
#  mutate(across(
#    .cols = all_of(c('bili_value','creat_value','crp_value','hinr_value','neut_value','pl_fer_value')),
#    .fns = ~ if_else(.x > 0, log(.x), NA_real_), # exclude any '0' results (only single neut result)
#    .names = "log_{.col}"
#  ))

# Return to long format with included predictors
#cirrho_ida_ml_long <- cirrho_ida_ml %>%
#  select(-any_of(c('bili_value','creat_value','crp_value','hinr_value','neut_value','pl_fer_value','bmi_value','log_crp_value','log_creat_value'))) %>%
#  pivot_longer(cols = c(age, hb_fer_value, delta_hb, mcv_cfer_value, delta_mcv, mch_cfer_value, delta_mch, log_pl_fer_value, delta_pl, log_hinr_value, log_bili_value, log_neut_value), 
#               names_to = "Variable", 
#               values_to = "Value")

# Histogram for normal and transformed variables
#ggplot(cirrho_ida_ml_long, aes(x = Value)) +
#  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
#  facet_wrap(~Variable, scales = "free") +
#  labs(title = "Histograms of Variables",
#       x = "Value",
#       y = "Count") +
#  theme_minimal()

# Filter for selected predictor variables
cirrho_ida_ml_long <- cirrho_ida_ml %>%
  select(-any_of(c('creat_value','crp_value','bmi_value'))) %>%
  pivot_longer(cols = c(age, hb_fer_value, delta_hb, mcv_cfer_value, delta_mcv, mch_cfer_value, delta_mch, pl_fer_value, delta_pl, hinr_value, bili_value, neut_value), 
               names_to = "Variable", 
               values_to = "Value")


# 1. Define which variables are "normal" (t-test) and "non-normal" (Wilcoxon)
normal_vars <- c("age", "delta_hb", "delta_mch", "delta_mcv",
                 "delta_pl", "hb_fer_value", "mch_cfer_value", "mcv_cfer_value")

nonnormal_vars <- c("bili_value", "hinr_value", "neut_value", "pl_fer_value")

# 2. Function to perform the appropriate test per variable
perform_tests <- function(df) {
  df %>%
    group_by(Variable) %>%
    group_modify(~ {
      var_name <- .y$Variable[[1]]
      
      if (var_name %in% normal_vars) {
        # t-test for normally distributed variables
        res <- t.test(Value ~ ida_yn, data = .x)
        tibble(
          test_type   = "t_test",
          p_value     = res$p.value
        )
        
      } else if (var_name %in% nonnormal_vars) {
        # Wilcoxon rank-sum test for non-normal variables
        res <- wilcox.test(Value ~ ida_yn, data = .x, exact = FALSE)
        tibble(
          test_type   = "wilcox",
          p_value     = res$p.value
        )
        
      } else {
        # Optional: if a variable is in neither list
        tibble(
          test_type   = NA_character_,
          p_value     = NA_real_
        )
      }
    }) %>%
    ungroup() %>%
    mutate(
      significance = case_when(
        is.na(p_value)        ~ "",
        p_value < 0.001       ~ "***",
        p_value < 0.01        ~ "**",
        p_value < 0.05        ~ "*",
        TRUE                  ~ "ns"
      )
    )
}

# 3. Run test function on your long data
test_results <- perform_tests(cirrho_ida_ml_long)
print(test_results)

# 4. Merge the significance results with the original long dataset (if you want it there too)
cirrho_ida_ml_long <- cirrho_ida_ml_long %>%
  left_join(test_results, by = "Variable")

# 5. Get max y per Variable for label positioning
test_results <- test_results %>%
  left_join(
    cirrho_ida_ml_long %>%
      group_by(Variable) %>%
      summarise(y_max = max(Value, na.rm = TRUE), .groups = "drop"),
    by = "Variable"
  )

# 6. Updated ggplot with stars and correct tests (and formatted a bit)
ggplot(
  cirrho_ida_ml_long,
  aes(x = factor(ida_yn), y = Value, fill = factor(ida_yn))
) +
  # background purple dots (observations)
  geom_jitter(
    color = "purple3",
    alpha = 0.3,
    width = 0.1,
    size  = 0.8,
    show.legend = FALSE
  ) +
  # boxplots on top: narrower + thicker outline
  geom_boxplot(
    outlier.shape = NA,
    width  = 0.4,
    colour = "black",
    size   = 0.7
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 6        # <- more columns = each panel narrower
  ) +
  theme_bw(base_size = 10) +
  labs(
    x = NULL,
    y = "Value"
  ) +
  theme(
    # remove x-axis labels and ticks
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text   = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 10),
    
    # make panels closer together
    panel.spacing.x = unit(0.4, "lines"),
    panel.spacing.y = unit(0.4, "lines"),
    
    # reduce outer plot margins
    plot.margin = margin(2, 2, 2, 2, "pt"),
    
    # optional: move legend and keep it compact
    legend.position = "bottom"
  ) +
  # relabel legend for ida_yn
  scale_fill_discrete(
    name   = "IDA status",
    labels = c(`0` = "No IDA", `1` = "IDA")
  ) +
  # significance stars
  geom_text(
    data = test_results,
    aes(x = 1.5, y = y_max, label = significance),
    inherit.aes = FALSE,
    size = 4
  )

##### ML PIPELINE -----------------------------------------------------------------------------------------------------------------------------
# d.f = cirrho_ida_ml
# all patients with cirrhosis who have outcome recorded (Ferritin and Hb day of or within 90d prior)

# remove.packages('caret')

table(cirrho_ida_ml$ida_yn) # total without tx  n = 3470, no ida n = 2816, ida n = 654
# (total with tx  n = 3631, no ida n = 2853, ida n = 778) 


### DATA PRE-PROCESSING ----------------------------------------------------------------------------------------------------------------------

## MISSING VALUES ----------------------------------------------------------------------------------------------------------------------------
# Detect columns with missing values
missing_cols <- colnames(cirrho_ida_ml)[colSums(is.na(cirrho_ida_ml)) > 0]

# Display columns with missing values
print(missing_cols)

# Show missing value counts
missing_counts <- colSums(is.na(cirrho_ida_ml))
print(missing_counts)

# Set threshold (e.g., drop columns with more than 30% missing values)
missing_threshold <- 0.3

# Calculate proportion of missing values per column
missing_proportion <- colMeans(is.na(cirrho_ida_ml))

# Identify columns to keep (those with <= threshold missing proportion)
cols_to_keep <- names(missing_proportion[missing_proportion <= missing_threshold])

# Subset the data frame to keep only the desired columns
cirrho_ida_ml_cleaned <- cirrho_ida_ml[, cols_to_keep, drop = FALSE] # crp # creat # bmi >30% missingness therefore removed

# Log transform non-normal variable within cirrho_ida_ml_cleaned
cirrho_ida_ml_cleaned <- cirrho_ida_ml_cleaned %>%
  mutate(across(
    .cols = all_of(c('bili_value','hinr_value','neut_value','pl_fer_value')),
    .fns = ~ if_else(.x > 0, log(.x), NA_real_), # exclude any '0' results (only single neut result)
    .names = "log_{.col}"
  ))

# Select columns for model (should be 16 columns)
cirrho_ida_ml_cleaned <- cirrho_ida_ml_cleaned %>%
  dplyr::select(-c(cfer_date, cfer_value, hb_fer_date, bili_value, hinr_value, pl_fer_value, neut_value))

# Relocate outcome variable to end
cirrho_ida_ml_cleaned <- cirrho_ida_ml_cleaned %>%
  relocate(ida_yn, .after = last_col())

## ONE-HOT ENCODING ----------------------------------------------------------------------------------------------------------------------------
# Encode categorical variables 'gender' and 'ethnicity' using one-hot encoding
install.packages('fastDummies')
library(fastDummies)

cirrho_ida_ml_cleaned$ethnic_group <- as.factor(cirrho_ida_ml_cleaned$ethnic_group)
cirrho_ida_ml_cleaned$gender_id_uncoded <- as.factor(cirrho_ida_ml_cleaned$gender_id_uncoded)
str(cirrho_ida_ml_cleaned)

cirrho_ida_ml_encoded <- dummy_cols(cirrho_ida_ml_cleaned, 
                                    select_columns = c("ethnic_group", "gender_id_uncoded"), 
                                    #select_columns = "gender_id_uncoded",
                                    remove_first_dummy = FALSE, # Keeps all categories
                                    remove_selected_columns = TRUE)  # Drops original columns



## DATA SPLITTING ----------------------------------------------------------------------------------------------------------------------------
# Split data using stratified sampling: 80% train 20% test
install.packages('caret') # for ML models
install.packages('VIM') # for KNN imputation
library(caret)
library(VIM)

set.seed(123)  # Ensures reproducibility
train_index <- createDataPartition(cirrho_ida_ml_encoded$ida_yn, p = 0.8, list = FALSE)

# Split the dataset
train_data <- cirrho_ida_ml_encoded[train_index, ]
test_data <- cirrho_ida_ml_encoded[-train_index, ]

# Check class distribution
table(cirrho_ida_ml_encoded$ida_yn) / nrow(cirrho_ida_ml_encoded)  # Overall distribution no ida 81.2% ida 18.8% (with tx no ida 78.6% IDA 21.4%)
table(train_data$ida_yn) / nrow(train_data)        # Train set distribution no ida 81.4% ida 18.6% (with tx - no ida 78.5% ida 21.5%)
table(test_data$ida_yn) / nrow(test_data)          # Test set distribution no ida 80.0% ida 20.0% (with tx no ida 79.1% no IDA 20.9%)


## CORRELATION MATRIX ---------------------------------------------------------------------------------------------------------------------
# Compute correlation matrix with the target variable (ida_yn)
#cor_matrix <- cor(train_data_rm use = "complete.obs")

# Select features with high correlation (absolute value > 0.5) with mpg
#selected_features <- names(which(abs(cor_matrix["ida_yn", ]) > 0.5))
#selected_features <- setdiff(selected_features, "ida_yn")  # Remove target variable

# Filter dataset with selected features
#filtered_data <- data %>% select(all_of(c("ida_yn", selected_features)))

# Print selected features
#print(selected_features)

## IMPUTATION ----------------------------------------------------------------------------------------------------------------------------
# KNN imputation
# (NB No imputation required for XGBoost)
# Can also try multiple imputation (check if within caret)

# Perform KNN imputation on the training data (replace k=5 with desired k)
set.seed(123)
imputed_train_data <- kNN(train_data, k = 5)

# Remove all columns with the "_imp" suffix
imputed_train_data_cleaned <- imputed_train_data[, !grepl("_imp", names(imputed_train_data))]

# Check the structure of the imputed training data
str(imputed_train_data_cleaned)

# Perform KNN imputation on the test data using the same model (same k=5 as training data)
set.seed(123)
imputed_test_data <- kNN(test_data, k = 5)

# Remove all columns with the "_imp" suffix
imputed_test_data_cleaned <- imputed_test_data[, !grepl("_imp", names(imputed_test_data))]

# Check the structure of the imputed test data
str(imputed_test_data_cleaned)

## NORMALISATION ----------------------------------------------------------------------------------------------------------------------------
# as suitable for combining models (RF, XGBOOST and logistic regression)
# Compute Min and Max from training data (not all normally distributed despite log transformation so min max better)
min_vals <- apply(imputed_train_data_cleaned[,2:14], 2, min)
max_vals <- apply(imputed_train_data_cleaned[,2:14], 2, max)

# Function to normalise using training min/max
normalize <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}

# Apply the same normalisation to train and test sets
train_data[, 2:14] <- as.data.frame(mapply(normalize, imputed_train_data_cleaned[, 2:14], min_vals, max_vals))
test_data[, 2:14] <- as.data.frame(mapply(normalize, imputed_test_data_cleaned[, 2:14], min_vals, max_vals))

# Relocate outcome to end
train_data <- train_data %>% relocate(ida_yn, .after = last_col())
test_data <- test_data %>% relocate(ida_yn, .after = last_col())

train_data_rm <- train_data %>%
  dplyr::select(-c(study_subject_id))

# Ensure data in correct format
train_data_rm <- train_data_rm %>%
  dplyr::rename(ethnic_group_Not_stated = 'ethnic_group_Not stated')

train_data <- train_data %>%
  dplyr::rename(ethnic_group_Not_stated = 'ethnic_group_Not stated')

test_data <- test_data %>%
  dplyr::rename(ethnic_group_Not_stated = 'ethnic_group_Not stated')

str(train_data_rm)
train_data_rm$ida_yn <- as.factor(train_data_rm$ida_yn)

str(train_data)
train_data$ida_yn <- as.factor(train_data_rm$ida_yn)

str(test_data)
test_data$ida_yn <- as.factor(test_data$ida_yn)

setDT(train_data_rm)[,.(count=.N), by=.(ida_yn, `ethnic_group_Not_stated`)]

# Convert ida_yn to factor with 'safe' non-numeric labels
train_data_rm <- train_data_rm %>%
  mutate(ida_yn = factor(ida_yn, levels = c(1, 0), labels = c("Class1", "Class0")))

#train_data <- train_data %>%
# mutate(ida_yn = factor(ida_yn, levels = c(1, 0), labels = c("Class1", "Class0")))

test_data <- test_data %>%
  mutate(ida_yn = factor(ida_yn, levels = c(1, 0), labels = c("Class1", "Class0")))


# -----------------------------------------------------------------------------------------------------------------------------------------------
## FEATURE LISTS
# -----------------------------------------------------------------------------------------------------------------------------------------------
# Of the features available, test subset for clinical feasibility in different clinical settings e.g. in lab


# Define named list of different feature sets
feature_sets <- list(
  allfeatures = names(train_data_rm)[!names(train_data_rm) %in% "ida_yn"],
  coreplusprior = c("age", "gender_id_uncoded_Female", "gender_id_uncoded_Male", "hb_fer_value", "mcv_cfer_value", "mch_cfer_value", "log_pl_fer_value", "log_neut_value", "delta_mcv", "delta_hb", "delta_mch", "delta_pl"),
  corenoprior = c("age", "gender_id_uncoded_Female", "gender_id_uncoded_Male", "hb_fer_value", "mcv_cfer_value", "mch_cfer_value", "log_pl_fer_value", "log_neut_value")
)



# -----------------------------------------------------------------------------------------------------------------------------------------------
## MODEL LISTS
# -----------------------------------------------------------------------------------------------------------------------------------------------
# Logistic regression as baseline, tree based models due to ability to handle cont and categorical data, relative flexibility, innate feature ranking

model_list <- list(
  glmnet = "glmnet",
  ranger = "ranger", 
  rpart = "rpart",
  xgbTree = "xgbTree"
)

#-----------------------------------------------------------------------------------------------------------------------------------------------
## MODEL DEVELOPMENT 1: TRAINING + TUNING
#-----------------------------------------------------------------------------------------------------------------------------------------------
install.packages("MLmetrics")

library(xgboost)#  needed for xgbTree as per caret github 
library(pROC)
library(PRROC)
library(MLmetrics)

# Create F1 function NO LONGER REQUIRED AS OPTIMISING ON PR AUC
#f1 <- function (data, lev = NULL, model = NULL) {
# precision <- posPredValue(data$pred, data$obs, positive = "Class1")
#recall  <- sensitivity(data$pred, data$obs, positive = "Class1")
#f1 <- (2 * precision * recall) / (precision + recall)
#c("F1" = f1)
#} 


# Step 1: Set up training control (with class probabilities, random search and F1 as tuning parameter)
set.seed(123)
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  #index = createFolds(train_data_rm$hospital_site, k = *), # If site variable available
  search = 'random',
  classProbs = TRUE,
  summaryFunction = prSummary, # Optimises PR 
  savePredictions = TRUE
)


# Step 2: Set up training results and final model containers
results_list <- list()
final_models <- list()

# Step 3: Loop over each model and each feature set
for (model_name in names(model_list)) {
  for (feature_name in names(feature_sets)) {
    features <- feature_sets[[feature_name]]
    
    cat("Training model:", model_name, "| Feature set:", feature_name, "\n")
    
    # Create class weights
    class_counts <- table(train_data_rm$ida_yn)
    total <- sum(class_counts)
    class_weights <- total / (2 * class_counts) # inverse class weights
    weights <- ifelse(
      train_data_rm$ida_yn == "Class1",
      class_weights["Class1"],
      class_weights["Class0"]
    )
    
    # Train model
    set.seed(123)
    model <- caret::train(
      form = as.formula(paste("ida_yn ~", paste(features, collapse = "+"))),
      data = train_data_rm,
      method = model_name,
      metric = "AUC", # optimise PR AUC from prSummary
      tuneLength = 10,
      trControl = train_control,
      weights = weights
    )
    
    key <- paste(feature_name, model_name, sep = "_")
    
    results_list[[key]] <- model$results %>%
      filter(AUC == max(AUC)) %>%
      mutate(feature_set = feature_name, model = model_name)
    
    final_models[[key]] <- model
  }
}

train_summary <- bind_rows(results_list)
print(train_summary)

# prSummary returns AUC (PR AUC), Precision, Recall, and F1 score
train_summary_final <- train_summary %>%
  select(feature_set, model, AUC, Precision, Recall, F) %>%
  arrange(feature_set)
print(train_summary_final)


# -----------------------------------------------------------------------------------------------------------------------------------------------
## THRESHOLD OPTIMISATION FOR BEST F1 SCORE
# -----------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Find an optimal threshold per (feature_set, model) to maximise F1 with cutpointr
# -------------------------------------------------------------------------------
library(cutpointr)
library(stringr)

# helper: safe F1 (used only for verification, cutpointr uses its own F1_score)
f1_safe <- function(tp, fp, fn, tn) {
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  if ((precision + recall) == 0) return(0)
  2 * precision * recall / (precision + recall)
}

threshold_objects <- list()
threshold_summary <- list()

for (key in names(final_models)) {
  m <- final_models[[key]]
  stopifnot(!is.null(m$pred))  # need saved CV predictions
  
  # Positive class is the first level (you relevelled to "Class1")
  pos_class <- m$levels[1]
  
  # Keep only rows for best-tuned params (defensive even though savePredictions="final")
  preds <- m$pred
  bt    <- m$bestTune
  if (!is.null(bt) && nrow(bt) > 0 && all(names(bt) %in% names(preds))) {
    preds <- dplyr::semi_join(preds, bt, by = names(bt))
  }
  
  # Probability column for the positive class has the same name as that class
  prob_col <- pos_class
  if (!prob_col %in% names(preds)) {
    stop(sprintf("Couldn't find probability column '%s' in predictions for %s.", prob_col, key))
  }
  
  df <- preds %>%
    dplyr::select(obs, !!rlang::sym(prob_col)) %>%
    dplyr::rename(truth = obs, score = !!rlang::sym(prob_col)) %>%
    dplyr::mutate(truth = forcats::fct_relevel(truth, pos_class))
  
  # Use cutpointr to maximise F1 on the out-of-fold predictions
  cp <- cutpointr(
    data       = df,
    x          = score,
    class      = truth,
    pos_class  = pos_class,
    direction  = ">=",
    method     = maximize_metric,
    metric     = F1_score         # built-in metric in cutpointr
  )
  
  # Store full object (plots, bootstrap, etc. can be done later)
  threshold_objects[[key]] <- cp
  
  # Derive a concise summary row
  opt_thr <- cp$optimal_cutpoint
  
  # Compute precision/recall/F1 at the chosen threshold (for a clear summary table)
  pred_lbl <- factor(ifelse(df$score >= opt_thr, pos_class, setdiff(levels(df$truth), pos_class)[1]),
                     levels = levels(df$truth))
  tp <- sum(pred_lbl == pos_class & df$truth == pos_class)
  fp <- sum(pred_lbl == pos_class & df$truth != pos_class)
  fn <- sum(pred_lbl != pos_class & df$truth == pos_class)
  tn <- sum(pred_lbl != pos_class & df$truth != pos_class)
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1_val    <- f1_safe(tp, fp, fn, tn)
  
  # Parse feature_set and model from the key "<feature_set>_<model>"
  parts <- strsplit(key, "_")[[1]]
  feature_set_i <- parts[1]
  model_i       <- paste(parts[-1], collapse = "_")
  
  threshold_summary[[key]] <- dplyr::tibble(
    feature_set        = feature_set_i,
    model              = model_i,
    optimal_threshold  = opt_thr,
    F1_oof             = f1_val,
    precision_oof      = precision,
    recall_oof         = recall,
    tp = tp, fp = fp, fn = fn, tn = tn
  )
}

threshold_summary <- dplyr::bind_rows(threshold_summary) %>%
  dplyr::arrange(dplyr::desc(F1_oof))

print(threshold_summary)


# --------------------------------------------------------------------------------
## MODEL DEVELOPMENT 2: CV PERFORMANCE EVALUATION SUMMARY FOR BEST TUNES AT OPTIMAL THRESHOLD (mean + 95% CI)
# --------------------------------------------------------------------------------
library(purrr)
library(rlang)
library(tibble)
library(forcats)

# Helpers ------------------------------------------------------------------------
safe_div <- function(num, den) ifelse(den == 0, NA_real_, num / den)
f1_from_counts <- function(tp, fp, fn) {
  precision <- safe_div(tp, tp + fp)
  recall    <- safe_div(tp, tp + fn)
  ifelse((precision + recall) == 0, NA_real_, 2 * precision * recall / (precision + recall))
}
summarise_ci_df <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  mu <- mean(x)
  if (n > 1) {
    s  <- sd(x)
    se <- s / sqrt(n)
    tcrit <- qt(0.975, df = n - 1)
    tibble(estimate = mu, ci_lower = mu - tcrit * se, ci_upper = mu + tcrit * se, n_folds = n)
  } else {
    tibble(estimate = mu, ci_lower = NA_real_, ci_upper = NA_real_, n_folds = n)
  }
}

# Core computation ---------------------------------------------------------------
perf_rows <- map(names(final_models), function(key) {
  m <- final_models[[key]]
  pos_class <- m$levels[1]
  neg_class <- setdiff(m$levels, pos_class)[1]
  
  parts <- strsplit(key, "_")[[1]]
  feature_set_i <- parts[1]
  model_i       <- paste(parts[-1], collapse = "_")
  
  # Threshold selected for this pair
  thr_row <- threshold_summary %>%
    dplyr::filter(feature_set == feature_set_i, model == model_i)
  if (nrow(thr_row) != 1) return(NULL)
  thr <- thr_row$optimal_threshold[[1]]
  
  # Keep only predictions for the best-tuned params
  preds <- m$pred
  bt <- m$bestTune
  if (!is.null(bt) && nrow(bt) > 0 && all(names(bt) %in% names(preds))) {
    preds <- dplyr::semi_join(preds, bt, by = names(bt))
  }
  
  # Ensure the fold column exists
  stopifnot("Resample" %in% names(preds))
  
  score <- preds[[pos_class]]
  truth <- forcats::fct_relevel(preds$obs, pos_class)
  
  df <- tibble(
    Resample = preds$Resample, # <- keep caret's exact column name
    truth    = truth,
    score    = score
  )
  
  # Per-fold metrics (threshold-dependent + threshold-independent)
  fold_metrics <- split(df, df$Resample) %>%
    map_dfr(function(d) {
      pred_pos <- d$score >= thr
      tp <- sum(pred_pos & d$truth == pos_class)
      fp <- sum(pred_pos & d$truth == neg_class)
      fn <- sum(!pred_pos & d$truth == pos_class)
      tn <- sum(!pred_pos & d$truth == neg_class)
      
      sensitivity <- safe_div(tp, tp + fn)   # TPR / Recall
      specificity <- safe_div(tn, tn + fp)   # TNR
      precision   <- safe_div(tp, tp + fp)   # PPV
      f1          <- f1_from_counts(tp, fp, fn)
      
      # PR AUC & ROC AUC (skip if single-class fold)
      has_pos <- any(d$truth == pos_class)
      has_neg <- any(d$truth == neg_class)
      pr_auc  <- NA_real_
      roc_auc <- NA_real_
      if (has_pos && has_neg) {
        pr_auc <- tryCatch({
          PRROC::pr.curve(
            scores.class0 = d$score[d$truth == pos_class],
            scores.class1 = d$score[d$truth == neg_class],
            curve = FALSE
          )$auc.integral
        }, error = function(e) NA_real_)
        roc_auc <- tryCatch({
          as.numeric(pROC::auc(
            response  = d$truth,
            predictor = d$score,
            levels    = c(neg_class, pos_class)
          ))
        }, error = function(e) NA_real_)
      }
      
      tibble(
        Resample   = unique(d$Resample),
        Sensitivity = sensitivity,
        Specificity = specificity,
        Precision   = precision,
        F1          = f1,
        `PR AUC`    = pr_auc,
        `ROC AUC`   = roc_auc
      )
    })
  
  # Mean & 95% CI per metric across folds
  summary_long <- fold_metrics %>%
    select(-Resample) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    summarise(summarise_ci_df(value), .groups = "drop") %>%
    mutate(
      feature_set       = feature_set_i,
      model             = model_i,
      optimal_threshold = thr
    ) %>%
    select(feature_set, model, optimal_threshold, metric, estimate, ci_lower, ci_upper)
  
  summary_long
}) %>%
  compact() %>%
  bind_rows() %>%
  arrange(feature_set, model, metric) %>%
  mutate(
    estimate = round(estimate, 3),
    ci_lower = round(ci_lower, 3),
    ci_upper = round(ci_upper, 3)
  )

perf_table <- perf_rows

# Display grouped by feature_set
perf_table %>%
  group_by(feature_set) %>%
  arrange(model, metric, .by_group = TRUE) %>%
  print(n = Inf)


# Build a robust metric order (use your preferred order when present; append any extras)
metrics_available <- perf_table %>%
  distinct(metric) %>%
  pull(metric) %>%
  as.character()

default_order <- c("Sensitivity","Specificity","Precision","F1","PR AUC","ROC AUC")
metric_order <- c(intersect(default_order, metrics_available),
                  setdiff(metrics_available, default_order))

# 1) Wide table: three columns per metric (_estimate, _ci_lower, _ci_upper)
perf_wide <- perf_table %>%
  mutate(metric = as.character(metric)) %>%
  pivot_wider(
    id_cols     = c(feature_set, model, optimal_threshold),
    names_from  = metric,
    values_from = c(estimate, ci_lower, ci_upper),
    names_glue  = "{metric}_{.value}"   # e.g., "Sensitivity_estimate"
  ) %>%
  { 
    ci_parts  <- c("estimate","ci_lower","ci_upper")
    col_order <- c("feature_set","model","optimal_threshold",
                   as.vector(sapply(metric_order, function(m) paste0(m, "_", ci_parts))))
    select(., any_of(col_order))
  } %>%
  arrange(feature_set, model)

# 2) Wide "pretty" table: one formatted column per metric, e.g. 0.842 (0.802 - 0.881)
perf_wide_pretty <- perf_table %>%
  mutate(
    metric = as.character(metric),
    pretty = sprintf("%.2f (%.2f-%.2f)", estimate, ci_lower, ci_upper)
  ) %>%
  select(feature_set, model, optimal_threshold, metric, pretty) %>%
  pivot_wider(
    id_cols     = c(feature_set, model, optimal_threshold),
    names_from  = metric,
    values_from = pretty
  ) %>%
  select(feature_set, model, optimal_threshold, all_of(metric_order)) %>%
  arrange(feature_set, model)

# Inspect
perf_wide
perf_wide_pretty

# Export 
install.packages(c("officer","flextable"))
library(officer)
library(flextable)
trained_models_evaluation_20251205 <- perf_wide_pretty
ft <- flextable(trained_models_evaluation_20251205)
ft <- theme_vanilla(ft)
ft <- autofit(ft)
ft <- align(ft, align = "center", part = "header")

# 3) write to Word with a heading
doc <- read_docx()
doc <- body_add_par(doc, "trained_models_evaluation_20251205", style = "heading 1")
doc <- body_add_flextable(doc, ft)

print(doc, target = "trained_model_evaluation.docx")



# -----------------------------------------------------------------------------------------------------------------------------------------------
## FINAL MODEL = CORE PLUS PRIOR, RANGER (coreplusprior_ranger)
# -----------------------------------------------------------------------------------------------------------------------------------------------
# Choice is a pragmatic decision
# Based on technical performance: F1 score 0.699 (0.689 - 0.709) NB overlaps with 'best performing' all features ranger
# AND clinical usability: Less features, bili and INR have degree of missingness (more so in primary care) therefore limit inclusion/generalisability/ opportunity for external validation




# -------------------------------------------------------------------------------
# EVALUATION ON TEST SET: final ranger model with 'coreplusprior' features
# Metrics: Sensitivity, Specificity, Precision, F1, PR AUC, ROC AUC
# 95% CIs via bootstrap resampling
# -------------------------------------------------------------------------------

library(dplyr)
library(tibble)
library(forcats)
library(pROC)   # ROC AUC
library(PRROC)  # PR AUC
library(purrr)
library(rlang)

# 1) Identify the trained model & its optimal threshold -------------------------
model_key <- "coreplusprior_ranger"
stopifnot(model_key %in% names(final_models))

final_ranger <- final_models[[model_key]]

opt_thr <- threshold_summary %>%
  dplyr::filter(feature_set == "coreplusprior", model == "ranger") %>%
  dplyr::pull(optimal_threshold) %>%
  .[[1]]

# 2) Build the test frame (same features used in training) ----------------------
features <- feature_sets$coreplusprior

test_core <- test_data %>%
  dplyr::select(dplyr::all_of(features), ida_yn) %>%
  dplyr::mutate(
    # Make sure the positive class is first ("Class1"), consistent with training
    ida_yn = forcats::fct_relevel(ida_yn, "Class1")
  )

pos_class <- "Class1"
neg_class <- "Class0"

# 3) Predict probabilities for the positive class on the test set --------------
test_scores <- predict(final_ranger, newdata = test_core, type = "prob")[, pos_class]
truth       <- test_core$ida_yn

# 4) Helper to compute all metrics on a given sample ---------------------------
safe_div <- function(a, b) ifelse(b == 0, NA_real_, a / b)

compute_metrics <- function(truth, scores, thr, pos = pos_class, neg = neg_class) {
  pred_pos <- scores >= thr
  tp <- sum(pred_pos & truth == pos)
  fp <- sum(pred_pos & truth == neg)
  fn <- sum(!pred_pos & truth == pos)
  tn <- sum(!pred_pos & truth == neg)
  
  sensitivity <- safe_div(tp, tp + fn)              # recall / TPR
  specificity <- safe_div(tn, tn + fp)              # TNR
  precision   <- safe_div(tp, tp + fp)              # PPV
  f1 <- if (is.na(precision) || is.na(sensitivity) || (precision + sensitivity) == 0) {
    NA_real_
  } else {
    2 * precision * sensitivity / (precision + sensitivity)
  }
  
  # PR AUC (needs both classes to be present; return NA if not)
  pr_auc <- tryCatch({
    PRROC::pr.curve(
      scores.class0 = scores[truth == pos],   # PRROC uses this naming quirk
      scores.class1 = scores[truth == neg],
      curve = FALSE
    )$auc.integral
  }, error = function(e) NA_real_)
  
  # ROC AUC (also needs both classes)
  roc_auc <- tryCatch({
    as.numeric(pROC::auc(
      response  = truth,
      predictor = scores,
      levels    = c(neg, pos)
    ))
  }, error = function(e) NA_real_)
  
  c(
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision   = precision,
    F1          = f1,
    `PR AUC`    = pr_auc,
    `ROC AUC`   = roc_auc
  )
}

# 5) Point estimates on the full test set --------------------------------------
point_est <- compute_metrics(truth, test_scores, opt_thr)

# 6) Bootstrap 95% CIs (percentile method) -------------------------------------
set.seed(123)       # reproducible bootstrap
B <- 1000           # increase/decrease for precision vs. speed
n <- length(truth)

boot_mat <- replicate(B, {
  idx <- sample.int(n, replace = TRUE)
  # compute on the bootstrap sample (may yield NAs if only one class sampled)
  compute_metrics(truth[idx], test_scores[idx], opt_thr)
})

# Summarise CIs (ignore NAs caused by all-one-class bootstrap samples)
metric_names <- names(point_est)
ci_bounds <- t(apply(boot_mat, 1, function(x) {
  stats::quantile(x, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
}))
colnames(ci_bounds) <- c("ci_lower", "ci_upper")
rownames(ci_bounds) <- metric_names

# 7) Nice, readable results table ----------------------------------------------
test_eval_tbl <- tibble::tibble(
  metric   = metric_names,
  estimate = as.numeric(point_est[metric_names]),
  ci_lower = ci_bounds[metric_names, "ci_lower"],
  ci_upper = ci_bounds[metric_names, "ci_upper"]
) %>%
  dplyr::mutate(
    estimate = round(estimate, 3),
    ci_lower = round(ci_lower, 3),
    ci_upper = round(ci_upper, 3)
  )

test_eval_tbl_pretty <- test_eval_tbl %>%
  dplyr::mutate(value = sprintf("%.3f (%.3f-%.3f)", estimate, ci_lower, ci_upper)) %>%
  dplyr::select(Metric = metric, `Estimate (95% CI)` = value)

# Display
cat(sprintf("\nFinal model: ranger | Feature set: coreplusprior | Optimal threshold = %.3f\n\n", opt_thr))
print(test_eval_tbl_pretty, n = Inf)


# -------------------------------------------------------------------------------
# CONFUSION MATRIX (TEST SET) FOR FINAL RANGER (coreplusprior)
# - Pretty ggplot heatmap with counts + row %
# - Optional gt table with totals
# - Saves PNG/PDF for thesis
# -------------------------------------------------------------------------------

install.packages("gt")
library(scales)
library(gt)

# 0) Predicted labels at the chosen threshold -----------------------------------
pred_label <- factor(
  ifelse(test_scores >= opt_thr, pos_class, neg_class),
  levels = c(pos_class, neg_class)  # keep positive class first
)

# 1) caret confusionMatrix (for verification / appendix) ------------------------
cm_caret <- caret::confusionMatrix(
  data      = pred_label,
  reference = truth,
  positive  = pos_class
)
cm_caret

# 2) Tidy counts + row/overall percentages -------------------------------------
cm_df <- tibble(truth = truth, pred = pred_label) %>%
  count(truth, pred, name = "n") %>%
  complete(truth = c(pos_class, neg_class),
           pred  = c(pos_class, neg_class),
           fill  = list(n = 0)) %>%
  group_by(truth) %>%
  mutate(row_total = sum(n),
         row_pct   = n / row_total) %>%
  ungroup() %>%
  mutate(total_n = sum(n),
         overall_pct = n / total_n,
         cell = dplyr::case_when(
           truth == pos_class & pred == pos_class ~ "TP",
           truth == pos_class & pred == neg_class ~ "FN",
           truth == neg_class & pred == pos_class ~ "FP",
           truth == neg_class & pred == neg_class ~ "TN",
           TRUE ~ NA_character_
         ))

# 3) Publication-quality heatmap ------------------------------------------------
#    - Annotates each tile with count and row %
#    - Saves PNG + PDF for insertion into your thesis
heatmap_plot <- ggplot(cm_df, aes(x = pred, y = truth, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(n, "\n", percent(row_pct, accuracy = 0.1))),
            size = 5, lineheight = 0.95) +
  scale_fill_gradient(low = "#e8eef9", high = "#1f77b4") +
  scale_x_discrete(position = "top",
                   labels = c(setNames(c("Predicted: IDA", "Predicted: No IDA"),
                                       c(pos_class, neg_class)))) +
  scale_y_discrete(labels = c(setNames(c("Actual: IDA", "Actual: No IDA"),
                                       c(pos_class, neg_class)))) +
  labs(
    title    = "Confusion Matrix - Final Ranger (coreplusprior) on Test Set",
    subtitle = sprintf("Threshold = %.3f | Row labels show actual class; tile text: count and row percentage", opt_thr),
    x = NULL, y = NULL, fill = "Count"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

# Display in R session
heatmap_plot

# Save high-res figures for thesis
ggsave("confusion_matrix_ranger_coreplusprior_test.png", heatmap_plot, width = 6.5, height = 5.5, dpi = 300)
ggsave("confusion_matrix_ranger_coreplusprior_test.pdf", heatmap_plot, width = 6.5, height = 5.5)

# 4) Optional: a clean table with totals using gt --------------------------------
cm_table <- cm_df %>%
  select(truth, pred, n) %>%
  mutate(truth = recode(truth, !!pos_class := "Actual: IDA", !!neg_class := "Actual: No IDA"),
         pred  = recode(pred,  !!pos_class := "Predicted: IDA", !!neg_class := "Predicted: No IDA")) %>%
  pivot_wider(names_from = pred, values_from = n) %>%
  mutate(Total = rowSums(across(starts_with("Predicted")))) %>%
  arrange(desc(truth))  # puts "Actual: IDA" first

# Column totals
col_totals <- cm_table %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(truth = "Total")

cm_table_totals <- bind_rows(cm_table, col_totals)

confusion_gt <- cm_table_totals %>%
  gt(rowname_col = "truth") %>%
  tab_header(
    title = md("**Confusion Matrix - Final Ranger (coreplusprior) on Test Set**"),
    subtitle = md(sprintf("Threshold = %.3f", opt_thr))
  ) %>%
  fmt_number(everything(), decimals = 0) %>%
  tab_style(
    style = list(cell_fill(color = "#f5f7fb")),
    locations = cells_body(rows = truth == "Total")
  ) %>%
  cols_label(
    `Predicted: IDA` = "Predicted: IDA",
    `Predicted: No IDA` = "Predicted: No IDA",
    Total = "Row total"
  ) %>%
  tab_options(table.font.size = px(14))

confusion_gt

# Optionally save the table to PNG (requires webshot2)
# install.packages("webshot2")
# webshot2::install_phantomjs()
# gtsave(confusion_gt, "confusion_matrix_ranger_coreplusprior_test_table.png")


# ------------------------------------------------------------------------------
# PR and ROC curves for final ranger (coreplusprior) on TEST set
# - Uses PRROC (PR) and pROC (ROC)
# - Annotates the operating point at your chosen threshold
# - Saves PNG + PDF figures for your thesis
# ------------------------------------------------------------------------------

# 0) Helpful quantities ---------------------------------------------------------
prev <- mean(truth == pos_class)  # class prevalence (baseline for PR curve)

# Operating point (thresholded prediction)
pred_pos <- test_scores >= opt_thr
tp <- sum(pred_pos & truth == pos_class)
fp <- sum(pred_pos & truth == neg_class)
fn <- sum(!pred_pos & truth == pos_class)
tn <- sum(!pred_pos & truth == neg_class)

precision_op <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))
recall_op    <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))

# 1) Precision-Recall curve -----------------------------------------------------
# PRROC expects "scores.class0" = scores for POSITIVES, and "scores.class1" = scores for NEGATIVES
pr <- PRROC::pr.curve(
  scores.class0 = test_scores[truth == pos_class],
  scores.class1 = test_scores[truth == neg_class],
  curve = TRUE
)
pr_auc <- pr$auc.integral
pr_df  <- tibble(recall = pr$curve[, 1], precision = pr$curve[, 2])

pr_plot <- ggplot(pr_df, aes(x = recall, y = precision)) +
  geom_line(linewidth = 1.2, color = "#1f77b4") +
  geom_hline(yintercept = prev, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = recall_op, y = precision_op),
             size = 3.5, shape = 21, fill = "#d62728", color = "white", stroke = 1) +
  annotate("label", x = recall_op, y = precision_op,
           label = sprintf("Thr = %.3f", opt_thr), vjust = -1, size = 4) +
  labs(
    title    = "Precision-Recall Curve - Final Ranger (coreplusprior) on Test Set",
    subtitle = sprintf("PR AUC = %.3f   |   Prevalence = %.1f%%", pr_auc, 100 * prev),
    x = "Recall (Sensitivity)",
    y = "Precision (PPV)"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

pr_plot
ggsave("PR_curve_ranger_coreplusprior_test.png", pr_plot, width = 6.5, height = 5.5, dpi = 300)
ggsave("PR_curve_ranger_coreplusprior_test.pdf", pr_plot, width = 6.5, height = 5.5)

# -----------------------------------------------------------------------------------------------------------------------------------------------
## FEATURE IMPORTANCE OF FINAL MODEL 
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# SHAP explanations for final ranger (coreplusprior) on TEST set
# - Mean |SHAP| importance bar plot
# - SHAP beeswarm (each point = a patient)
# - Saves PNG/PDF figures for the thesis
# ------------------------------------------------------------------------------

# =======================
# SHAP for FINAL MODEL on TEST SET (ranger + coreplusprior)
# =======================

# If needed (run once):
# install.packages(c("fastshap", "shapviz"))

library(dplyr)
library(ggplot2)
library(fastshap)
library(shapviz)

set.seed(123)

# 0) Sanity checks --------------------------------------------------------------
stopifnot(exists("final_ranger"))
stopifnot(exists("test_core"))
stopifnot(exists("feature_sets"))
features <- feature_sets$coreplusprior

# 1) Test-set feature matrix (drop outcome) ------------------------------------
X_test <- test_core %>% dplyr::select(dplyr::all_of(features))

# 2) Prediction wrapper: return P(IDA = Class1) --------------------------------
pfun <- function(object, newdata) {
  # 'final_ranger' is a caret::train (method = ranger)
  predict(object, newdata = newdata, type = "prob")[, "Class1"]
}

# 3) Compute SHAP values on the TEST set ---------------------------------------
#    nsim controls Monte Carlo accuracy; increase (e.g., 500-1000) for smoother results.
shap_test <- fastshap::explain(
  object       = final_ranger,  # caret model
  X            = X_test,        # rows to explain (TEST patients)
  pred_wrapper = pfun,
  nsim         = 300
)
# shap_test is a data frame: rows = patients, columns = features.
# Each cell is the SHAP contribution to P(IDA) for that feature & patient.

# 4) Build a shapviz object for easy plotting ----------------------------------
# Baseline = mean predicted probability in the TEST set (optional but nice for interpretability)
baseline_prob <- mean(pfun(final_ranger, X_test))
sv <- shapviz::shapviz(shap_test, X = X_test, baseline = baseline_prob)

# 5) Global importance: mean(|SHAP|) per feature (bar plot) --------------------
p_imp <- sv_importance(sv, kind = "bar") +
  ggplot2::labs(
    title = "Mean |SHAP| (Global Importance)",
    subtitle = "Final model: ranger (coreplusprior), evaluated on TEST set",
    x = "Average absolute SHAP contribution to P(IDA)"
  )
p_imp

# Optional: a tidy importance table you can export
imp_tbl <- shap_test |>
  tibble::as_tibble() |>
  tidyr::pivot_longer(everything(),
                      names_to = "feature",
                      values_to = "shap") |>
  group_by(feature) |>
  summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE), .groups = "drop") |>
  arrange(desc(mean_abs_shap))
# readr::write_csv(imp_tbl, "shap_importance_table_test.csv")

# 6) Beeswarm plot: patient-level effects per feature --------------------------
p_bee <- sv_importance(sv, kind = "beeswarm") +
  ggplot2::labs(
    title = "SHAP Beeswarm (TEST set)",
    subtitle = "Each dot = one patient; colour = feature value; position = effect on P(IDA)"
  )
p_bee

# 7) Save figures for reports/thesis -------------------------------------------
ggplot2::ggsave("shap_importance_bar_test_20251205.png", p_imp, width = 6.5, height = 5.5, dpi = 300)
ggplot2::ggsave("shap_beeswarm_test_20251205.png", p_bee, width = 6.5, height = 5.5, dpi = 300)
ggplot2::ggsave("shap_importance_bar_test_20251205.pdf", p_imp, width = 6.5, height = 5.5)
ggplot2::ggsave("shap_beeswarm_test_20251205.pdf", p_bee, width = 6.5, height = 5.5)


# -----------------------------------------------------------------------------------------------------------------------------------------------
## ERROR ANALYSIS 
# -----------------------------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------------------------------------------------
## CLINICAL IMPLEMENTATION
# -----------------------------------------------------------------------------------------------------------------------------------------------


# IF CONSIDERING SENSITIVITY ANALYSIS INCLUDING ONLY THOSE WITH ANAEMIA

# What proportion of patients with a ferritin >30 and Hb pair are anaemic? or what proportion of anaemic patients with a ferritin pair have a ferritin >30?
cirrho_cfer_hb_during_study_anaemia <- cirrho_cfer_hb_during_study2 %>%
  filter((hb_value <= 130 & gender_id_uncoded == "Male")|(hb_value <= 120 & gender_id_uncoded == "Female")) %>%
  mutate(ida_yn = if_else(cfer_value < 30, 1, 0)) %>%
  group_by(study_subject_id) %>%
  mutate(patient_level_ida = max(ida_yn)) %>%
  ungroup() %>%
  filter(ida_yn == 1 | patient_level_ida == 0) %>%  # Keep all 1s, or 0s if no 1 exists
  group_by(study_subject_id) %>%
  arrange(abs(cfer_hb_diffdays), cfer_date) %>%  # Prioritise smallest absolute diffdays (closest to zero) then earliest ferritin
  dplyr::slice(1)

table(cirrho_cfer_hb_during_study_anaemia$patient_level_ida)
# n = 2483
# IDA = 649 (26.1%)
# Non-IDA = 1834 (73.9%)


# -----------------------------------------------------------------------------------------------------------------------------------------------
## CLINICAL IMPLEMENTATION
# -----------------------------------------------------------------------------------------------------------------------------------------------

