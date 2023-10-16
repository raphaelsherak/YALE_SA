library(readr)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(gt)
library(parsedate)
library(janitor)
library(labelled)
library(parameters)
library(survival)
library(ggsurvfit)
library(table1)

all_pts <- read_csv("SA_pts_reformatted.csv")
# set TZ to east coast standard
time_zone <- "America/New_York"
#pathway went live on 7/13/21
pathway_start <- mdy("7/13/21", tz = time_zone)
#Story board notification went live on 9/8/21
sb_start <- mdy("9/8/21", tz = time_zone)
day.arrival.start <- hms::as_hms("06:00:00")
night.arrival.start <- hms::as_hms("18:00:00")
# making outcome variables
all_pts <- all_pts %>% mutate(
  azithro_or_doxy = case_when(
    azithromycin_ordered_num | doxycycline_ordered_num == 1 ~ 1,
    azithromycin_ordered_num == 0 &
      doxycycline_ordered_num == 0 ~ 0,
    .default = 0
  )
) %>% mutate(
  sti_tx = case_when(
    gender == "Male" &
      ceftriaxone_ordered_num == 1 & azithro_or_doxy == 1 ~ 1,
    gender == "Male" &
      ceftriaxone_ordered_num | azithro_or_doxy == 0  ~ 0,
    gender == "Female" &
      ceftriaxone_ordered_num == 1 &
      azithro_or_doxy == 1 &
      flagyl_or_metronidazole_ordered_num == 1 ~ 1,
    gender == "Female" &
      ceftriaxone_ordered_num |
      azithro_or_doxy |
      flagyl_or_metronidazole_ordered_num == 0 ~ 0,
    .default = 0
  ),
  chem_ordered = case_when(
    cmp_ordered_yn == "Y" | bmp_ordered_yn == "Y" ~ 1,
    cmp_ordered_yn == "N" & bmp_ordered_yn == "N" ~ 0,
    .default = NA
  ),
  abx_given = case_when(
    ceftriaxone_ordered_num == 0 &
      azithro_or_doxy == 0 &
      flagyl_or_metronidazole_ordered_num == 0 ~ 0,
    ceftriaxone_ordered_num == 1 |
      azithro_or_doxy == 1 |
      flagyl_or_metronidazole_ordered_num == 1 ~ 1
  ),
  lfts_ordered = case_when(
    cmp_ordered_yn == "Y" | lft_ordered_yn == "Y" ~ 1,
    cmp_ordered_yn == "N" & lft_ordered_yn == "N" ~ 0,
    .default = NA
  )
) %>% mutate(
  hiv_labs = case_when(
    hiv_pep_kit_ordered_num == 1 &
      chem_ordered == 1 & lfts_ordered == 1 ~ 1,
    hiv_pep_kit_ordered_num == 1 &
      (chem_ordered == 0| lfts_ordered == 0) ~ 0,
    .default = 0
  ),
  prevent.preg = case_when(
    plan_b_or_levonorgestrel_ordered_num == 1 | ulipristal_ordered_num == 1 ~ 1,
    plan_b_or_levonorgestrel_ordered_num == 0 & ulipristal_ordered_num == 0 ~ 0,
    .default = NA
  ),
  advocate_offerred = case_when(
    patient_advocate_called == 1 | patient_advocate_called == 3 ~ "Advocate Contacted",
    patient_advocate_called == 0 | patient_advocate_called == 2 ~ "No Documentation of Pt Advocate",
    .default = NA
  ),
  sane_kit_YN = case_when(
    sane_kit_done == 0 ~ "No",
    sane_kit_done == 1 ~ "Yes",
    sane_kit_done == 2 ~ "Offered, but declined",
    sane_kit_done == 3 ~ "Outside 120 hr Window",
    sane_kit_done == 4 ~ "Not done due to concurrent psychiatric concerns"
  ),
  sane_kit_missed = case_when(
    sane_kit_done == 0 ~ "SANE kit not done/documented",
    sane_kit_done %in% c(1,2,3) ~ "SANE kit done/not indicated",
    sane_kit_done == 4 ~ "Not done due to concurrent psychiatric concerns"
  )
) %>% 
  mutate(
    tbl_sti_tx = case_when(
      sti_tx == 1 & abx_given == 1 ~ 1,
      sti_tx == 0 & abx_given == 1 ~ 0,
      .default = NA
    ),
    tbl_hiv_labs = case_when(
      hiv_pep_kit_ordered_num == 1 & hiv_labs == 1 ~ 1,
      hiv_pep_kit_ordered_num == 1 & hiv_labs == 0 ~ 0,
      .default = NA
    ),
    tbl_preg_test = case_when(
      female_u55 == 1 & pregnancy_test_ordered_num == 1 ~ 1,
      female_u55 == 1 & pregnancy_test_ordered_num == 0 ~ 0,
      .default = NA
    ),
    tbl_prevent_preg = case_when(
      female_u55 == 1 & prevent.preg == 1 ~ 1,
      female_u55 == 1 & prevent.preg == 0 ~ 0,
      .default = NA
    ),
    under120h = case_when(
      time_since_sexual_assault_if_number_k_is_y == 0 |  time_since_sexual_assault_if_number_k_is_y == 1 ~ TRUE,
      time_since_sexual_assault_if_number_k_is_y == 2 |  time_since_sexual_assault_if_number_k_is_y == 3 ~ FALSE,
      .default = NA
    ),
    english = case_when(
      patient_language == "English" ~ 1,
      is.na(patient_language) ~ NA,
      .default = 0
    ),
    ambulance = if_else(method_of_arrival == 1, 1, 0),
    relationship_with_patient = factor(relationship_with_patient, levels = c(0,1,2,3,4,NA), labels = c("Unknown","Aqauintance", "Family member", "Stranger", "Intimate Partner", NA), exclude = NULL, ordered = FALSE ),
    insurance_3 = case_when(
      insurance == "Private Insurance" ~ "Private",
      insurance %in% c("Medicaid", "Medicare", "Sexual Assault") ~ "Public",
      insurance == "Uninsured/Self-Pay" ~ "Uninsured"
    )
  ) %>% 
  mutate(relationship_4_with_pt = case_when(
    relationship_with_patient == "Family member" ~ "Family",
    relationship_with_patient == "Aqauintance" ~ "Aquaintaince",
    relationship_with_patient == "Intimate Partner" ~ "Intimate Partner",
    relationship_with_patient == "Unknown" |relationship_with_patient == "Stranger" ~ "Stranger/Unknown"
  )) %>% 
  mutate(arrive_time = as_hms(arrive_dt)) %>% 
  mutate(day_night = case_when(
    arrive_time >= day.arrival.start & arrive_time <= night.arrival.start ~ "Day",
    arrive_time < day.arrival.start | arrive_time > night.arrival.start ~ "Night",
    .default = "ERROR"
  ))
all_pts$insurance_3 <- as_factor(all_pts$insurance_3)
all_pts$insurance_3 <- relevel(all_pts$insurance_3, ref = "Public")

between_pts <- all_pts %>% filter(between == 1)
#will keep "between" patients since only looking at day arrival vs night arrival for outcomes of SANE and advocate. 
n_between <- n_distinct(between_pts$pat_enc_csn_id, na.rm=TRUE) %>% as.character()
d.n.cohort1 <- all_pts %>% filter(age > 17) # %>% filter(between == 0)
excluded_patients <- d.n.cohort1  %>% filter(!is.na(exclude))
d.n.cohort2 <- d.n.cohort1  %>% filter(is.na(exclude))
excluded_patients <- excluded_patients %>% add_value_labels(reason_to_exclude = c( "Seen earlier" = 1 , "Patient reports not being assaulted" = 2, "Psych" = 3, "not excluded" = 4, "Eloped" = 5)) %>% to_factor()
excluded_patients %>% group_by(reason_to_exclude) %>% summarise(n=n())
n_excluded <-n_distinct(excluded_patients$pat_enc_csn_id, na.rm=TRUE) %>% as.character()
cat(" ", n_between, "patients who arrived between 7/13/21 and 9/8/21 were included for this analysis")
cat("-", n_excluded, "patients were excluded based on chart review.")
number_of_minors <-all_pts %>% filter(between == 0) %>% filter(age < 18)
number_of_minors <- n_distinct(number_of_minors$pat_enc_csn_id, na.rm = TRUE) %>% as.character()
cat("  ", number_of_minors, "patients under 18 YO excluded")
rm(n_excluded, number_of_minors, n_between)
table.1 <- d.n.cohort2 %>% 
  select(day_night, age, female, race_bwo, under120h, english, ambulance, insurance, relationship_with_patient, advocate_offerred, sane_kit_YN) %>% 
  set_variable_labels( day_night = "Time of arrival", age = "Age",  female = "Female", race_bwo = "Race", under120h = "Presented within 120 of assault", ambulance = "Arrived by EMS", english = "English speaking", insurance = "Insurance", relationship_with_patient = "Relation of assailant to patient", sane_kit_YN = "SANE kit done?") %>% 
  tbl_summary(
    by = day_night,
    missing = "ifany", 
    percent = "column",
    statistic = (age ~ "{median} ({p25}, {p75})")
  ) %>% 
  add_p() %>% separate_p_footnotes() %>%  as_gt() 
table.1

### Regression TIME

## Outcome: advocate PRESENT

d.n.cohort2 <- d.n.cohort2 %>% 
  mutate(advocate_present = case_when(
    patient_advocate_present = 1 ~ TRUE,
    patient_advocate_present = 0 ~ FALSE,
    .default = NA
  ))
