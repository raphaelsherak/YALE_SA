all_pts_bw <-
  all_pts %>% filter(race_bwo != "Other" &
                       age > 17 &
                       is.na(exclude)) %>% rename(Race = race_bwo) %>%
  mutate(
    seen_to_avs = case_when(
      !is.na(avs_dt) ~ as.numeric(as.duration(avs_dt - pt_seen_dt), "minutes"),
      ed_depart_dt < dispo_dt ~ as.numeric(as.duration(ed_depart_dt - pt_seen_dt), "minutes"),
      is.na(avs_dt) ~ as.numeric(as.duration(dispo_dt - pt_seen_dt), "minutes")
    ),
    seen_to_dispo =  as.numeric(as.duration(dispo_dt - pt_seen_dt), "minutes"),
    status = 1
  ) %>%
  mutate(end_time = case_when(
    is.na(provider_to_avs_printed_time) ~ seen_to_dispo,
    !is.na(provider_to_avs_printed_time) ~ provider_to_avs_printed_time
  )) %>% mutate(seen_to_avs = if_else(seen_to_avs > 0, seen_to_avs, seen_to_dispo)) %>%
  mutate(short_LOS = pmin(seen_to_avs, seen_to_dispo, end_time)) %>%
  mutate(time_hours = short_LOS / 60) %>% 
  mutate(sane_kit_u120 = case_when(
    under120h == TRUE & sane_kit_done == 1 ~ 1,
    under120h == TRUE & sane_kit_done != 1 ~ 0,
    .default = NA))

bw_tbl1 <- all_pts_bw %>%
  select(
    Race,
    age,
    female,
    english,
    under120h,
    u72,
    ambulance,
    short_LOS,
    intoxicated,
    Psych,
    ipv,
    female_u55,
    advocate_offered,
    tbl_sti_tx,
    tbl_preg_test,
    tbl_prevent_preg,
    sane_kit_YN,
    sane_kit_u120,
    follow_up_at_discharge,
    ct_400_forensic_toxiclogy
  ) %>%
  set_variable_labels(
    age = "Age",
    female = "Female",
    ambulance = "Arrived by EMS",
    english = "English Speaking",
    ipv = "Assailant was Intimate Partner",
    intoxicated = "Diagnosis of Intoxication",
    under120h = "Presented Within 120hr of Assault",
    u72 =  "Presented Within 72hr of Assault",
    Psych = "Psychiatric Diagnosis",
    advocate_offered = "Documentation that Advocate was Offered",
    ct_400_forensic_toxiclogy = "CT 400 Kit Done",
    sane_kit_YN = "SANE kit done",
    sane_kit_u120 = "Under 120 hours and SANE kit done",
    female_u55 = "Female under 55",
    follow_up_at_discharge = "Has after discharge follow up",
    short_LOS = "Length of Stay (minutes)",
    tbl_sti_tx = "Given Post-Exposure Antibiotics",
    tbl_prevent_preg = "Given Indicated Pregnancy Prophylaxis",
    tbl_preg_test = "Had Indicated Pregnancy Test"
  ) %>%
  tbl_summary(
    by = Race,
    missing = "no",
    percent = "column",
    statistic = list(
      age ~ "{median} ({p25}, {p75})",
      short_LOS ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  add_p() %>% separate_p_footnotes()
