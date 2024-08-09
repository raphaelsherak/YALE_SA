
# Supplemental Analysis: LOS


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
library(hms)
all_pts <- read_csv("~/Documents/Current_Projects/Sexual_Assault/new_SA_YNHH/Updated YNHH SA code/SA_pts_reformatted.csv")
# set TZ to east coast standard
time_zone <- "America/New_York"
#pathway went live on 7/13/21
pathway_start <- mdy("7/13/21", tz = time_zone)
#Story board notification went live on 9/8/21
sb_start <- mdy("9/8/21", tz = time_zone)
day.arrival.start <- hms::as_hms("03:00:00") #likely Day nurse will do kit (comes in within 4 hours of arrival time)
night.arrival.start <- hms::as_hms("15:00:00") #Unlikely for day nurse to do kit because closes to end of shift



# making outcome variable for if correct STI tx: CTX, azithromycin or doxy (and flagyl if female) are ordered 
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
  advocate_offered = case_when(
    patient_advocate_called == 1 | patient_advocate_called == 3 ~ "Advocate Offered",
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
    ),
    SA_insurance = if_else(
      insurance == "Sexual Assault", 1, 0
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
# limit to adults
adults <- all_pts %>% filter(age > 17) #678 adults
cohort <- adults %>% filter(ed_disposition == "Discharge") %>% filter(!is.na(ambulance)) #filters out 10 with NA for ambulance

cohort.time <- cohort %>% 
  mutate(seen_to_avs = case_when(
    !is.na(avs_dt) ~ as.numeric(as.duration(avs_dt - pt_seen_dt), "minutes"),
    ed_depart_dt < dispo_dt ~ as.numeric(as.duration(ed_depart_dt - pt_seen_dt), "minutes"),
    is.na(avs_dt) ~ as.numeric(as.duration(dispo_dt - pt_seen_dt), "minutes")
  ), seen_to_dispo =  as.numeric(as.duration(dispo_dt - pt_seen_dt), "minutes"),
  status = 1
  ) %>% filter(is.na(observation_admit_order_yn)) %>% # 8 patients were placed in psych obs so 
  mutate(end_time = case_when(
    is.na(provider_to_avs_printed_time) ~ seen_to_dispo,
    !is.na(provider_to_avs_printed_time) ~ provider_to_avs_printed_time)) %>% mutate(seen_to_avs = if_else(seen_to_avs > 0, seen_to_avs, seen_to_dispo)) %>% 
  mutate(short_LOS = pmin(seen_to_avs, seen_to_dispo, end_time)) %>% 
  mutate(time_hours = short_LOS/60,
         injury = case_when(
           Major_injury + Minor_injury == 0 ~ 0,
           Major_injury + Minor_injury > 0 ~ 1
         ),
         intox = case_when(
           Etoh + Drugs == 0 ~ 0,
           Etoh + Drugs > 0 ~ 1
         )) %>% 
  filter(!is.na(end_time)) #one patient's end time is NA


cohort.time %>% group_by(ambulance) %>% summarise(n = n(), los_dispo = mean(seen_to_dispo), los_to_avs = mean(seen_to_avs), mean_time= mean(short_LOS), median_time = median(short_LOS), medianseentoavs = median(seen_to_avs))

cohort.time <- cohort.time %>% mutate(arrival = factor(ambulance, levels = c(0,1), labels = c("Walk in", "EMS")))
ggplot(cohort.time, aes(x = arrival, y=short_LOS)) + geom_boxplot()

time.to.dc <- survfit2(Surv(time_hours, status) ~ arrival, data = cohort.time)
time.to.dc %>% ggsurvfit(type = "risk") + labs(y = "percent discharged", x = "hours since seen by provider") + add_quantile(y_value = 0.5, linewidth = 0.5, color = "grey40") + add_quantile(y_value = 0.75, linewidth = 0.5, color = "grey80")


time.to.dc.cox.multi <- survfit2(Surv(short_LOS, status) ~ ambulance + age + injury + intox + esi_level + sane_kit_YN, data = cohort.time) 
atf.model <- flexsurvreg(formula = Surv(short_LOS, status) ~ arrival + age + injury + intox + esi_level + sane_kit_YN, data = cohort.time, dist = "weibull")
summary(atf.model)
exp(coef(atf.model))
tbl_regression(atf.model)
cox.ph.model <- coxph(formula = Surv(time_hours, status) ~ arrival + age + injury + intox + esi_level + sane_kit_YN, data = cohort.time)
summary(cox.ph.model)
exp(coef(cox.ph.model))
tbl_regression(cox.ph.model, exponentiate = TRUE)

surv.object <- Surv(cohort.time$time_hours, cohort.time$status)
cox.time.to.dc <- coxph(formula = Surv(time_hours, status) ~ strata(arrival) + age + injury + intox + esi_level + sane_kit_YN, data = cohort.time) %>% 
  survfit2() %>% 
  ggsurvfit(type = "risk") + add_confidence_interval(type = "lines") + add_quantile(y_value = 0.5, linetype = "dotted", color = "grey30", linewidth = 0.8) + scale_ggsurvfit() + coord_cartesian(xlim = c(0, 4)) 
cox.time.to.dc
tbl_regression(cox.time.to.dc)

atf.model.w <- survfit(Surv(short_LOS, status) ~ 1, data = cohort.time)
log_log_s <- log(-log(atf.model.w$surv))
log_t <- log(atf.model.w$time)


table.1 <- cohort.time %>% 
  select(arrival, age, female, injury, intox, esi_level, sane_kit_YN, ) %>% 
  set_variable_labels(arrival = "Method of Arrival", age = "Age", female = "Female Identified", intox = "Diagnosis of Drug/Alcohol Intoxication", injury = "Traumatic Injury Diagnosis", esi_level = "ESI Level", sane_kit_YN = "Sane Kit Done") %>% 
  tbl_summary(
    by = arrival,
    missing = "ifany", 
    percent = "column",
    statistic = (age ~ "{median} ({p25}, {p75})")
  ) %>% 
  add_p() %>% separate_p_footnotes() %>%  as_gt() 

km_fit_1_lm <- lm(log_log_s ~ log_t)
km_fit_1_lm %>%
  broom::augment() %>%
  ggplot(aes(x = log_t)) +
  geom_point(aes(y = log_log_s)) +
  geom_line(aes(y = .fitted), linetype = 2, color = "goldenrod") +
  theme_light()
