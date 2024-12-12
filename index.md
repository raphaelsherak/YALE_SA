---
title: "SA Paper 1 Analysis"
output:
  html_document: 
    toc: yes
    theme: journal
    keep_md: yes
    code_folding: hide
    fig_width: 9
    fig_height: 6
    df_print: default
  html_notebook:
    toc: yes
  pdf_document:
    toc: yes
  word_document:
    toc: yes
---

## Prep Code


``` r
library(readr)
library(logistf)
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
library(readxl)
library(ggborderline)
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()
load(
  "~/Documents/Current_Projects/Sexual_Assault/new_SA_YNHH/Updated YNHH SA code/all_pts.RData"
)
# set TZ to east coast standard
time_zone <- "America/New_York"
#pathway went live on 7/13/21
pathway_start <- mdy("7/13/21", tz = time_zone)
#Story board notification went live on 9/8/21
sb_start <- mdy("9/8/21", tz = time_zone)
day.arrival.start <- hms::as_hms("03:00:00")
night.arrival.start <- hms::as_hms("15:00:00")
all_pts$insurance_3 <- as_factor(all_pts$insurance_3)
all_pts$insurance_3 <- relevel(all_pts$insurance_3, ref = "Public")
all_pts <- all_pts %>% mutate(
    race_eth = case_when(
      patient_ethnicity == "Hispanic or Latina/o/x" ~ "Hispanic or Latina/o/x",
      patient_race == "Black or African American" ~ "Non-Hispanic Black",
      patient_race == "White" ~ "Non-Hispanic White",
      patient_ethnicity != "Hispanic or Latina/o/x" &
        patient_race != "Black or African American" &
        patient_race != "White" ~ "Other",
      .default = patient_race
    )
  ) %>% 
  mutate(
    race_eth_num = case_when(
      race_eth == "Non-Hispanic White" ~ 0,
      race_eth == "Non-Hispanic Black" ~ 1,
      race_eth == "Hispanic or Latina/o/x" ~ 3,
      race_eth == "Other" ~ 4
    )
  ) %>% 
  set_value_labels(
    race_eth_num = c(
      "Non-Hispanic White" = 0,
      "Non-Hispanic Black" = 1,
      "Hispanic or Latina/o/x" = 3,
      "Other" = 4
    )
  )
all_pts$race_eth_num <- to_factor(all_pts$race_eth_num, ordered = FALSE)
all_excluded <- all_pts %>%
  mutate(exclude = case_when(age < 18 ~ 1,
                             between == 1 ~ 1,
                             sane_kit_done == 4 ~ 1,
                             exclude == 1 ~ 1)) %>%
  mutate(
    reason_to_exclude = case_when(
      age < 18 ~ 8,
      is.na(reason_to_exclude) & between == 1 ~ 7,
      is.na(reason_to_exclude) & sane_kit_done == 4 ~ 3,
      is.na(reason_to_exclude) ~ 6,
      .default = reason_to_exclude
    )
  ) %>% add_value_labels(
    reason_to_exclude = c(
      "Seen earlier" = 1 ,
      "Patient reports not being assaulted" = 2,
      "Psych" = 3,
      "not excluded" = 4,
      "Eloped" = 5,
      "NA/Not Listed" = 6,
      "Between Period" = 7,
      "Under 18 YO" = 8
    )
  ) %>% to_factor()
all_excluded <- all_excluded %>% filter(exclude == 1)
excluded_plot <-
  all_excluded %>% ggplot(aes(x = reason_to_exclude)) + geom_bar(fill = "darkred") + geom_text(stat =
                                                                                                 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3.5) + theme_minimal() + labs(x = "Reason to Exclude", y = "Count", title = "Excluded Patients by Reason") + theme(axis.text.x = element_text(angle = 35, vjust = .6, hjust= .8))

excluded_plot
```

![](index_files/figure-html/Initial Data and Package Prep-1.png)<!-- -->

``` r
all_excluded %>% group_by(reason_to_exclude) %>% summarise(n = n()) %>% print()
```

```
## # A tibble: 6 × 2
##   reason_to_exclude                       n
##   <fct>                               <int>
## 1 Seen earlier                           16
## 2 Patient reports not being assaulted    14
## 3 Psych                                  33
## 4 Eloped                                 14
## 5 Between Period                         49
## 6 Under 18 YO                           264
```

``` r
number_of_minors <-
  all_excluded %>% filter(age < 18) #changed it so age is first exclusion criteria
number_of_minors <-
  n_distinct(number_of_minors$pat_enc_csn_id, na.rm = TRUE) %>% as.character()
cat("  ", number_of_minors, "patients under 18 YO excluded")
```

```
##    264 patients under 18 YO excluded
```

``` r
n_excluded <-
  all_excluded %>% filter(
    reason_to_exclude %in% c(
      "Seen earlier",
      "Patient reports not being assaulted",
      "Psych",
      "Eloped",
      "NA/Not Listed"
    )
  )
n_excluded <-
  n_distinct(n_excluded$pat_enc_csn_id, na.rm = TRUE) %>% as.character()
n_between <-
  all_excluded %>% filter(reason_to_exclude == "Between Period")
n_between <-
  n_distinct(n_between$pat_enc_csn_id, na.rm = TRUE) %>% as.character()

cat("-", n_excluded, "patients were excluded based on chart review.")
```

```
## - 77 patients were excluded based on chart review.
```

``` r
cat(" ",
    n_between,
    "patients who arrived between 7/13/21 and 9/8/21 were excluded")
```

```
##   49 patients who arrived between 7/13/21 and 9/8/21 were excluded
```

``` r
rm(n_excluded, number_of_minors, n_between)
#pre-post cohort
pp.cohort.1 <-
  all_pts %>% filter(between == 0) %>% filter(age > 17) %>% filter(is.na(exclude), sane_kit_done != 4) %>% mutate(exposure.tab = exposure.char)
#pathway v no pathway cohort
cohort.1 <-
  all_pts %>% filter(ed_arrival_date > sb_start) %>% filter(age > 17) %>% filter(is.na(exclude)) %>% mutate(exposure.tab = pathway)
#pre vs no pathway cohort
pp.cohort.2 <- pp.cohort.1 %>% filter(agile_md_used_num != 1)
```

942 patient encounters from 2019-12-31 to 2022-12-24\
In order of exclusion: age -\> chart review -\> between period

## Table 1 and 2
### Table 1



```{=html}
<div id="fulgznqczn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fulgznqczn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#fulgznqczn thead, #fulgznqczn tbody, #fulgznqczn tfoot, #fulgznqczn tr, #fulgznqczn td, #fulgznqczn th {
  border-style: none;
}

#fulgznqczn p {
  margin: 0;
  padding: 0;
}

#fulgznqczn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fulgznqczn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fulgznqczn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fulgznqczn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fulgznqczn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fulgznqczn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fulgznqczn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fulgznqczn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fulgznqczn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fulgznqczn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fulgznqczn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fulgznqczn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fulgznqczn .gt_spanner_row {
  border-bottom-style: hidden;
}

#fulgznqczn .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fulgznqczn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fulgznqczn .gt_from_md > :first-child {
  margin-top: 0;
}

#fulgznqczn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fulgznqczn .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fulgznqczn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#fulgznqczn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fulgznqczn .gt_row_group_first td {
  border-top-width: 2px;
}

#fulgznqczn .gt_row_group_first th {
  border-top-width: 2px;
}

#fulgznqczn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#fulgznqczn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fulgznqczn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fulgznqczn .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fulgznqczn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#fulgznqczn .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fulgznqczn .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#fulgznqczn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fulgznqczn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fulgznqczn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fulgznqczn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#fulgznqczn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fulgznqczn .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#fulgznqczn .gt_left {
  text-align: left;
}

#fulgznqczn .gt_center {
  text-align: center;
}

#fulgznqczn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fulgznqczn .gt_font_normal {
  font-weight: normal;
}

#fulgznqczn .gt_font_bold {
  font-weight: bold;
}

#fulgznqczn .gt_font_italic {
  font-style: italic;
}

#fulgznqczn .gt_super {
  font-size: 65%;
}

#fulgznqczn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#fulgznqczn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fulgznqczn .gt_indent_1 {
  text-indent: 5px;
}

#fulgznqczn .gt_indent_2 {
  text-indent: 10px;
}

#fulgznqczn .gt_indent_3 {
  text-indent: 15px;
}

#fulgznqczn .gt_indent_4 {
  text-indent: 20px;
}

#fulgznqczn .gt_indent_5 {
  text-indent: 25px;
}

#fulgznqczn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#fulgznqczn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0_1"><span class='gt_from_md'><strong>Overall</strong><br />
N = 552</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1_1"><span class='gt_from_md'><strong>Post-intervention</strong><br />
N = 252</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2_1"><span class='gt_from_md'><strong>Pre-intervention</strong><br />
N = 300</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1_2"><span class='gt_from_md'><strong>Used Pathway</strong><br />
N = 128</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2_2"><span class='gt_from_md'><strong>Didn’t Use Pathway</strong><br />
N = 124</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age, Median (Q1, Q3)</td>
<td headers="stat_0_1" class="gt_row gt_center">27 (21, 38)</td>
<td headers="stat_1_1" class="gt_row gt_center">27 (22, 38)</td>
<td headers="stat_2_1" class="gt_row gt_center">27 (21, 39)</td>
<td headers="p.value_1" class="gt_row gt_center">>0.99<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">27 (22, 35)</td>
<td headers="stat_2_2" class="gt_row gt_center">27 (21, 40)</td>
<td headers="p.value_2" class="gt_row gt_center">0.57<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">520 (94)</td>
<td headers="stat_1_1" class="gt_row gt_center">237 (94)</td>
<td headers="stat_2_1" class="gt_row gt_center">283 (94)</td>
<td headers="p.value_1" class="gt_row gt_center">0.89<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">117 (91)</td>
<td headers="stat_2_2" class="gt_row gt_center">120 (97)</td>
<td headers="p.value_2" class="gt_row gt_center">0.072<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.015<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center"><br /></td>
<td headers="stat_2_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.16<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="stat_0_1" class="gt_row gt_center">260 (47)</td>
<td headers="stat_1_1" class="gt_row gt_center">121 (48)</td>
<td headers="stat_2_1" class="gt_row gt_center">139 (46)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">54 (42)</td>
<td headers="stat_2_2" class="gt_row gt_center">67 (54)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="stat_0_1" class="gt_row gt_center">135 (24)</td>
<td headers="stat_1_1" class="gt_row gt_center">47 (19)</td>
<td headers="stat_2_1" class="gt_row gt_center">88 (29)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">23 (18)</td>
<td headers="stat_2_2" class="gt_row gt_center">24 (19)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="stat_0_1" class="gt_row gt_center">118 (21)</td>
<td headers="stat_1_1" class="gt_row gt_center">64 (25)</td>
<td headers="stat_2_1" class="gt_row gt_center">54 (18)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">39 (30)</td>
<td headers="stat_2_2" class="gt_row gt_center">25 (20)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_0_1" class="gt_row gt_center">39 (7.1)</td>
<td headers="stat_1_1" class="gt_row gt_center">20 (7.9)</td>
<td headers="stat_2_1" class="gt_row gt_center">19 (6.3)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">12 (9.4)</td>
<td headers="stat_2_2" class="gt_row gt_center">8 (6.5)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Presented within 120h of assault, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">504 (91)</td>
<td headers="stat_1_1" class="gt_row gt_center">228 (90)</td>
<td headers="stat_2_1" class="gt_row gt_center">276 (92)</td>
<td headers="p.value_1" class="gt_row gt_center">0.44<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">121 (95)</td>
<td headers="stat_2_2" class="gt_row gt_center">107 (86)</td>
<td headers="p.value_2" class="gt_row gt_center">0.026<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0_1" class="gt_row gt_center">1</td>
<td headers="stat_1_1" class="gt_row gt_center">0</td>
<td headers="stat_2_1" class="gt_row gt_center">1</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center"><br /></td>
<td headers="stat_2_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Presented within 72h of assault, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">453 (82)</td>
<td headers="stat_1_1" class="gt_row gt_center">200 (79)</td>
<td headers="stat_2_1" class="gt_row gt_center">253 (85)</td>
<td headers="p.value_1" class="gt_row gt_center">0.11<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">108 (84)</td>
<td headers="stat_2_2" class="gt_row gt_center">92 (74)</td>
<td headers="p.value_2" class="gt_row gt_center">0.046<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0_1" class="gt_row gt_center">1</td>
<td headers="stat_1_1" class="gt_row gt_center">0</td>
<td headers="stat_2_1" class="gt_row gt_center">1</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center"><br /></td>
<td headers="stat_2_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">English speaking, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">527 (95)</td>
<td headers="stat_1_1" class="gt_row gt_center">240 (95)</td>
<td headers="stat_2_1" class="gt_row gt_center">287 (96)</td>
<td headers="p.value_1" class="gt_row gt_center">0.81<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">121 (95)</td>
<td headers="stat_2_2" class="gt_row gt_center">119 (96)</td>
<td headers="p.value_2" class="gt_row gt_center">0.59<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Arrived by EMS, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">215 (39)</td>
<td headers="stat_1_1" class="gt_row gt_center">96 (38)</td>
<td headers="stat_2_1" class="gt_row gt_center">119 (40)</td>
<td headers="p.value_1" class="gt_row gt_center">0.71<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">52 (41)</td>
<td headers="stat_2_2" class="gt_row gt_center">44 (35)</td>
<td headers="p.value_2" class="gt_row gt_center">0.40<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diagnosis of Intoxication, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">196 (36)</td>
<td headers="stat_1_1" class="gt_row gt_center">84 (33)</td>
<td headers="stat_2_1" class="gt_row gt_center">112 (37)</td>
<td headers="p.value_1" class="gt_row gt_center">0.33<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">52 (41)</td>
<td headers="stat_2_2" class="gt_row gt_center">32 (26)</td>
<td headers="p.value_2" class="gt_row gt_center">0.013<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Psychiatric Diagnosis, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">125 (23)</td>
<td headers="stat_1_1" class="gt_row gt_center">41 (16)</td>
<td headers="stat_2_1" class="gt_row gt_center">84 (28)</td>
<td headers="p.value_1" class="gt_row gt_center">0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">20 (16)</td>
<td headers="stat_2_2" class="gt_row gt_center">21 (17)</td>
<td headers="p.value_2" class="gt_row gt_center">0.78<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">42 (7.6)</td>
<td headers="stat_1_1" class="gt_row gt_center">14 (5.6)</td>
<td headers="stat_2_1" class="gt_row gt_center">28 (9.3)</td>
<td headers="p.value_1" class="gt_row gt_center">0.10<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">7 (5.5)</td>
<td headers="stat_2_2" class="gt_row gt_center">7 (5.6)</td>
<td headers="p.value_2" class="gt_row gt_center">0.95<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Wilcoxon rank sum test</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Pearson’s Chi-squared test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```
#### Supplemental Table 1


``` r
pp.cohort.2 <- pp.cohort.2 %>% mutate(exposure = exposure.num) %>% set_variable_labels(sane_ED = "ED with SANE program", race_eth_num = "Race and Ethnicity", exposure = "Exposure", age = "Age", ipv = "Assailant was Intimate Partner", intoxicated= "Intoxicated")

supplment.table1 <- pp.cohort.2 %>% mutate(exposure = exposure.num) %>% mutate(exposure.tab = if_else(exposure == 0, "Pre-intervention", "No Pathway")) %>% tbl.one.fxn()
supplment.table1
```

```{=html}
<div id="gwienbdhhx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gwienbdhhx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gwienbdhhx thead, #gwienbdhhx tbody, #gwienbdhhx tfoot, #gwienbdhhx tr, #gwienbdhhx td, #gwienbdhhx th {
  border-style: none;
}

#gwienbdhhx p {
  margin: 0;
  padding: 0;
}

#gwienbdhhx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gwienbdhhx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gwienbdhhx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gwienbdhhx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gwienbdhhx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwienbdhhx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwienbdhhx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwienbdhhx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gwienbdhhx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gwienbdhhx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gwienbdhhx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gwienbdhhx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gwienbdhhx .gt_spanner_row {
  border-bottom-style: hidden;
}

#gwienbdhhx .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gwienbdhhx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gwienbdhhx .gt_from_md > :first-child {
  margin-top: 0;
}

#gwienbdhhx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gwienbdhhx .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gwienbdhhx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gwienbdhhx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gwienbdhhx .gt_row_group_first td {
  border-top-width: 2px;
}

#gwienbdhhx .gt_row_group_first th {
  border-top-width: 2px;
}

#gwienbdhhx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwienbdhhx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gwienbdhhx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gwienbdhhx .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwienbdhhx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwienbdhhx .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gwienbdhhx .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gwienbdhhx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gwienbdhhx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwienbdhhx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwienbdhhx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwienbdhhx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwienbdhhx .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwienbdhhx .gt_left {
  text-align: left;
}

#gwienbdhhx .gt_center {
  text-align: center;
}

#gwienbdhhx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gwienbdhhx .gt_font_normal {
  font-weight: normal;
}

#gwienbdhhx .gt_font_bold {
  font-weight: bold;
}

#gwienbdhhx .gt_font_italic {
  font-style: italic;
}

#gwienbdhhx .gt_super {
  font-size: 65%;
}

#gwienbdhhx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gwienbdhhx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gwienbdhhx .gt_indent_1 {
  text-indent: 5px;
}

#gwienbdhhx .gt_indent_2 {
  text-indent: 10px;
}

#gwienbdhhx .gt_indent_3 {
  text-indent: 15px;
}

#gwienbdhhx .gt_indent_4 {
  text-indent: 20px;
}

#gwienbdhhx .gt_indent_5 {
  text-indent: 25px;
}

#gwienbdhhx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#gwienbdhhx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>No Pathway</strong><br />
N = 124</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Pre-intervention</strong><br />
N = 300</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age, Median (Q1, Q3)</td>
<td headers="stat_1" class="gt_row gt_center">27 (21, 40)</td>
<td headers="stat_2" class="gt_row gt_center">27 (21, 39)</td>
<td headers="p.value" class="gt_row gt_center">0.74<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">120 (97)</td>
<td headers="stat_2" class="gt_row gt_center">283 (94)</td>
<td headers="p.value" class="gt_row gt_center">0.29<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.20<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="stat_1" class="gt_row gt_center">67 (54)</td>
<td headers="stat_2" class="gt_row gt_center">139 (46)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="stat_1" class="gt_row gt_center">24 (19)</td>
<td headers="stat_2" class="gt_row gt_center">88 (29)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="stat_1" class="gt_row gt_center">25 (20)</td>
<td headers="stat_2" class="gt_row gt_center">54 (18)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_1" class="gt_row gt_center">8 (6.5)</td>
<td headers="stat_2" class="gt_row gt_center">19 (6.3)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Presented within 120h of assault, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">107 (86)</td>
<td headers="stat_2" class="gt_row gt_center">276 (92)</td>
<td headers="p.value" class="gt_row gt_center">0.054<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Presented within 72h of assault, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">92 (74)</td>
<td headers="stat_2" class="gt_row gt_center">253 (85)</td>
<td headers="p.value" class="gt_row gt_center">0.012<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">0</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">English speaking, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">119 (96)</td>
<td headers="stat_2" class="gt_row gt_center">287 (96)</td>
<td headers="p.value" class="gt_row gt_center">0.89<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Arrived by EMS, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">44 (35)</td>
<td headers="stat_2" class="gt_row gt_center">119 (40)</td>
<td headers="p.value" class="gt_row gt_center">0.42<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diagnosis of Intoxication, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">32 (26)</td>
<td headers="stat_2" class="gt_row gt_center">112 (37)</td>
<td headers="p.value" class="gt_row gt_center">0.023<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Psychiatric Diagnosis, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">21 (17)</td>
<td headers="stat_2" class="gt_row gt_center">84 (28)</td>
<td headers="p.value" class="gt_row gt_center">0.016<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">7 (5.6)</td>
<td headers="stat_2" class="gt_row gt_center">28 (9.3)</td>
<td headers="p.value" class="gt_row gt_center">0.21<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Wilcoxon rank sum test</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Pearson’s Chi-squared test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```
### Table 2



```{=html}
<div id="mhchixphqq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mhchixphqq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mhchixphqq thead, #mhchixphqq tbody, #mhchixphqq tfoot, #mhchixphqq tr, #mhchixphqq td, #mhchixphqq th {
  border-style: none;
}

#mhchixphqq p {
  margin: 0;
  padding: 0;
}

#mhchixphqq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mhchixphqq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mhchixphqq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mhchixphqq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mhchixphqq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mhchixphqq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mhchixphqq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mhchixphqq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mhchixphqq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mhchixphqq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mhchixphqq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mhchixphqq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mhchixphqq .gt_spanner_row {
  border-bottom-style: hidden;
}

#mhchixphqq .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#mhchixphqq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mhchixphqq .gt_from_md > :first-child {
  margin-top: 0;
}

#mhchixphqq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mhchixphqq .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mhchixphqq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mhchixphqq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mhchixphqq .gt_row_group_first td {
  border-top-width: 2px;
}

#mhchixphqq .gt_row_group_first th {
  border-top-width: 2px;
}

#mhchixphqq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhchixphqq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mhchixphqq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mhchixphqq .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mhchixphqq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhchixphqq .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mhchixphqq .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mhchixphqq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mhchixphqq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mhchixphqq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mhchixphqq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhchixphqq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mhchixphqq .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#mhchixphqq .gt_left {
  text-align: left;
}

#mhchixphqq .gt_center {
  text-align: center;
}

#mhchixphqq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mhchixphqq .gt_font_normal {
  font-weight: normal;
}

#mhchixphqq .gt_font_bold {
  font-weight: bold;
}

#mhchixphqq .gt_font_italic {
  font-style: italic;
}

#mhchixphqq .gt_super {
  font-size: 65%;
}

#mhchixphqq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mhchixphqq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mhchixphqq .gt_indent_1 {
  text-indent: 5px;
}

#mhchixphqq .gt_indent_2 {
  text-indent: 10px;
}

#mhchixphqq .gt_indent_3 {
  text-indent: 15px;
}

#mhchixphqq .gt_indent_4 {
  text-indent: 20px;
}

#mhchixphqq .gt_indent_5 {
  text-indent: 25px;
}

#mhchixphqq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mhchixphqq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0_1"><span class='gt_from_md'><strong>Overall</strong><br />
N = 552</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1_1"><span class='gt_from_md'><strong>Pre-intervention</strong><br />
N = 300</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2_1"><span class='gt_from_md'><strong>Post-intervention</strong><br />
N = 252</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1_2"><span class='gt_from_md'><strong>Used Pathway</strong><br />
N = 128</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2_2"><span class='gt_from_md'><strong>Didn’t Use Pathway</strong><br />
N = 124</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Documentation of advocate offerred, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">322 (58)</td>
<td headers="stat_1_1" class="gt_row gt_center">164 (55)</td>
<td headers="stat_2_1" class="gt_row gt_center">158 (63)</td>
<td headers="p.value_1" class="gt_row gt_center">0.057<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">98 (77)</td>
<td headers="stat_2_2" class="gt_row gt_center">60 (48)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">PEP antibiotics ordered, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">383 (69)</td>
<td headers="stat_1_1" class="gt_row gt_center">200 (67)</td>
<td headers="stat_2_1" class="gt_row gt_center">183 (73)</td>
<td headers="p.value_1" class="gt_row gt_center">0.13<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">108 (84)</td>
<td headers="stat_2_2" class="gt_row gt_center">75 (60)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">HIV PEP ordered (if &lt; 72h from assault, n/N Non-missing (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">111/453 (25%)</td>
<td headers="stat_1_1" class="gt_row gt_center">35/253 (14%)</td>
<td headers="stat_2_1" class="gt_row gt_center">76/200 (38%)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">63/108 (58%)</td>
<td headers="stat_2_2" class="gt_row gt_center">13/92 (14%)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female under 55, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">492 (89)</td>
<td headers="stat_1_1" class="gt_row gt_center">270 (90)</td>
<td headers="stat_2_1" class="gt_row gt_center">222 (88)</td>
<td headers="p.value_1" class="gt_row gt_center">0.47<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">112 (88)</td>
<td headers="stat_2_2" class="gt_row gt_center">110 (89)</td>
<td headers="p.value_2" class="gt_row gt_center">0.77<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pregnancy test ordered, n/N Non-missing (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">362/492 (74%)</td>
<td headers="stat_1_1" class="gt_row gt_center">191/270 (71%)</td>
<td headers="stat_2_1" class="gt_row gt_center">171/222 (77%)</td>
<td headers="p.value_1" class="gt_row gt_center">0.12<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">96/112 (86%)</td>
<td headers="stat_2_2" class="gt_row gt_center">75/110 (68%)</td>
<td headers="p.value_2" class="gt_row gt_center">0.002<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pregnancy prophalaxis ordered, n/N Non-missing (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">204/492 (41%)</td>
<td headers="stat_1_1" class="gt_row gt_center">104/270 (39%)</td>
<td headers="stat_2_1" class="gt_row gt_center">100/222 (45%)</td>
<td headers="p.value_1" class="gt_row gt_center">0.14<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">63/112 (56%)</td>
<td headers="stat_2_2" class="gt_row gt_center">37/110 (34%)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">285 (52)</td>
<td headers="stat_1_1" class="gt_row gt_center">158 (53)</td>
<td headers="stat_2_1" class="gt_row gt_center">127 (50)</td>
<td headers="p.value_1" class="gt_row gt_center">0.60<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">95 (74)</td>
<td headers="stat_2_2" class="gt_row gt_center">32 (26)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">SANE kit done, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.25<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center"><br /></td>
<td headers="stat_2_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0_1" class="gt_row gt_center">25 (4.5)</td>
<td headers="stat_1_1" class="gt_row gt_center">17 (5.7)</td>
<td headers="stat_2_1" class="gt_row gt_center">8 (3.2)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">1 (0.8)</td>
<td headers="stat_2_2" class="gt_row gt_center">7 (5.6)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Offered, but declined</td>
<td headers="stat_0_1" class="gt_row gt_center">128 (23)</td>
<td headers="stat_1_1" class="gt_row gt_center">62 (21)</td>
<td headers="stat_2_1" class="gt_row gt_center">66 (26)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">25 (20)</td>
<td headers="stat_2_2" class="gt_row gt_center">41 (33)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outside 120 hr Window</td>
<td headers="stat_0_1" class="gt_row gt_center">46 (8.3)</td>
<td headers="stat_1_1" class="gt_row gt_center">24 (8.0)</td>
<td headers="stat_2_1" class="gt_row gt_center">22 (8.7)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">7 (5.5)</td>
<td headers="stat_2_2" class="gt_row gt_center">15 (12)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0_1" class="gt_row gt_center">353 (64)</td>
<td headers="stat_1_1" class="gt_row gt_center">197 (66)</td>
<td headers="stat_2_1" class="gt_row gt_center">156 (62)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="stat_1_2" class="gt_row gt_center">95 (74)</td>
<td headers="stat_2_2" class="gt_row gt_center">61 (49)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Under 120 hours and SANE kit done, n/N Non-missing (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">350/504 (69%)</td>
<td headers="stat_1_1" class="gt_row gt_center">195/276 (71%)</td>
<td headers="stat_2_1" class="gt_row gt_center">155/228 (68%)</td>
<td headers="p.value_1" class="gt_row gt_center">0.52<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">95/121 (79%)</td>
<td headers="stat_2_2" class="gt_row gt_center">60/107 (56%)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Has after discharge follow up, n (%)</td>
<td headers="stat_0_1" class="gt_row gt_center">515 (93)</td>
<td headers="stat_1_1" class="gt_row gt_center">273 (91)</td>
<td headers="stat_2_1" class="gt_row gt_center">242 (96)</td>
<td headers="p.value_1" class="gt_row gt_center">0.029<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="stat_1_2" class="gt_row gt_center">124 (97)</td>
<td headers="stat_2_2" class="gt_row gt_center">118 (95)</td>
<td headers="p.value_2" class="gt_row gt_center">0.71<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Pearson’s Chi-squared test</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Fisher’s exact test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```
#### Supplmenetal Table 2


``` r
supplment.table2 <- pp.cohort.2 %>% mutate(exposure = exposure.num) %>% mutate(exposure.tab = if_else(exposure == 0, "Pre-intervention", "No Pathway")) %>% tbl.two.fxn()
supplment.table2
```

```{=html}
<div id="shhxtqiyea" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#shhxtqiyea table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#shhxtqiyea thead, #shhxtqiyea tbody, #shhxtqiyea tfoot, #shhxtqiyea tr, #shhxtqiyea td, #shhxtqiyea th {
  border-style: none;
}

#shhxtqiyea p {
  margin: 0;
  padding: 0;
}

#shhxtqiyea .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#shhxtqiyea .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#shhxtqiyea .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#shhxtqiyea .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#shhxtqiyea .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#shhxtqiyea .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#shhxtqiyea .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#shhxtqiyea .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#shhxtqiyea .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#shhxtqiyea .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#shhxtqiyea .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#shhxtqiyea .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#shhxtqiyea .gt_spanner_row {
  border-bottom-style: hidden;
}

#shhxtqiyea .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#shhxtqiyea .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#shhxtqiyea .gt_from_md > :first-child {
  margin-top: 0;
}

#shhxtqiyea .gt_from_md > :last-child {
  margin-bottom: 0;
}

#shhxtqiyea .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#shhxtqiyea .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#shhxtqiyea .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#shhxtqiyea .gt_row_group_first td {
  border-top-width: 2px;
}

#shhxtqiyea .gt_row_group_first th {
  border-top-width: 2px;
}

#shhxtqiyea .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#shhxtqiyea .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#shhxtqiyea .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#shhxtqiyea .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#shhxtqiyea .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#shhxtqiyea .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#shhxtqiyea .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#shhxtqiyea .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#shhxtqiyea .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#shhxtqiyea .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#shhxtqiyea .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#shhxtqiyea .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#shhxtqiyea .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#shhxtqiyea .gt_left {
  text-align: left;
}

#shhxtqiyea .gt_center {
  text-align: center;
}

#shhxtqiyea .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#shhxtqiyea .gt_font_normal {
  font-weight: normal;
}

#shhxtqiyea .gt_font_bold {
  font-weight: bold;
}

#shhxtqiyea .gt_font_italic {
  font-style: italic;
}

#shhxtqiyea .gt_super {
  font-size: 65%;
}

#shhxtqiyea .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#shhxtqiyea .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#shhxtqiyea .gt_indent_1 {
  text-indent: 5px;
}

#shhxtqiyea .gt_indent_2 {
  text-indent: 10px;
}

#shhxtqiyea .gt_indent_3 {
  text-indent: 15px;
}

#shhxtqiyea .gt_indent_4 {
  text-indent: 20px;
}

#shhxtqiyea .gt_indent_5 {
  text-indent: 25px;
}

#shhxtqiyea .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#shhxtqiyea div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>No Pathway</strong><br />
N = 124</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Pre-intervention</strong><br />
N = 300</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Documentation of advocate offerred, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">60 (48)</td>
<td headers="stat_2" class="gt_row gt_center">164 (55)</td>
<td headers="p.value" class="gt_row gt_center">0.24<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">PEP antibiotics ordered, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">75 (60)</td>
<td headers="stat_2" class="gt_row gt_center">200 (67)</td>
<td headers="p.value" class="gt_row gt_center">0.23<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">HIV PEP ordered (if &lt; 72h from assault, n/N Non-missing (%)</td>
<td headers="stat_1" class="gt_row gt_center">13/92 (14%)</td>
<td headers="stat_2" class="gt_row gt_center">35/253 (14%)</td>
<td headers="p.value" class="gt_row gt_center">0.94<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female under 55, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">110 (89)</td>
<td headers="stat_2" class="gt_row gt_center">270 (90)</td>
<td headers="p.value" class="gt_row gt_center">0.69<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pregnancy test ordered, n/N Non-missing (%)</td>
<td headers="stat_1" class="gt_row gt_center">75/110 (68%)</td>
<td headers="stat_2" class="gt_row gt_center">191/270 (71%)</td>
<td headers="p.value" class="gt_row gt_center">0.62<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pregnancy prophalaxis ordered, n/N Non-missing (%)</td>
<td headers="stat_1" class="gt_row gt_center">37/110 (34%)</td>
<td headers="stat_2" class="gt_row gt_center">104/270 (39%)</td>
<td headers="p.value" class="gt_row gt_center">0.37<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">32 (26)</td>
<td headers="stat_2" class="gt_row gt_center">158 (53)</td>
<td headers="p.value" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">SANE kit done, n (%)</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.011<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">7 (5.6)</td>
<td headers="stat_2" class="gt_row gt_center">17 (5.7)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Offered, but declined</td>
<td headers="stat_1" class="gt_row gt_center">41 (33)</td>
<td headers="stat_2" class="gt_row gt_center">62 (21)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outside 120 hr Window</td>
<td headers="stat_1" class="gt_row gt_center">15 (12)</td>
<td headers="stat_2" class="gt_row gt_center">24 (8.0)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">61 (49)</td>
<td headers="stat_2" class="gt_row gt_center">197 (66)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Under 120 hours and SANE kit done, n/N Non-missing (%)</td>
<td headers="stat_1" class="gt_row gt_center">60/107 (56%)</td>
<td headers="stat_2" class="gt_row gt_center">195/276 (71%)</td>
<td headers="p.value" class="gt_row gt_center">0.007<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Has after discharge follow up, n (%)</td>
<td headers="stat_1" class="gt_row gt_center">118 (95)</td>
<td headers="stat_2" class="gt_row gt_center">273 (91)</td>
<td headers="p.value" class="gt_row gt_center">0.21<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Pearson’s Chi-squared test</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Fisher’s exact test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

## Regression Outcomes

### Primary Outcome: *Advocate Contacted*


``` r
advocate.regression<-function(data, exposure){
  data$advocate_offered <- as_factor(data$advocate_offered )
  data$advocate_offered <- relevel(data$advocate_offered, ref= "No Documentation of Pt Advocate")
  model <-  glm(advocate_offered ~ exposure + age + race_eth_num + ipv+ intoxicated + sane_ED, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```

All patients


```{=html}
<div id="lbpvwoityq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lbpvwoityq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lbpvwoityq thead, #lbpvwoityq tbody, #lbpvwoityq tfoot, #lbpvwoityq tr, #lbpvwoityq td, #lbpvwoityq th {
  border-style: none;
}

#lbpvwoityq p {
  margin: 0;
  padding: 0;
}

#lbpvwoityq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lbpvwoityq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lbpvwoityq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lbpvwoityq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lbpvwoityq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbpvwoityq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbpvwoityq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbpvwoityq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lbpvwoityq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lbpvwoityq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lbpvwoityq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lbpvwoityq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lbpvwoityq .gt_spanner_row {
  border-bottom-style: hidden;
}

#lbpvwoityq .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lbpvwoityq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lbpvwoityq .gt_from_md > :first-child {
  margin-top: 0;
}

#lbpvwoityq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lbpvwoityq .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lbpvwoityq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lbpvwoityq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lbpvwoityq .gt_row_group_first td {
  border-top-width: 2px;
}

#lbpvwoityq .gt_row_group_first th {
  border-top-width: 2px;
}

#lbpvwoityq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbpvwoityq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lbpvwoityq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lbpvwoityq .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbpvwoityq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbpvwoityq .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lbpvwoityq .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lbpvwoityq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lbpvwoityq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbpvwoityq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbpvwoityq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbpvwoityq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbpvwoityq .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbpvwoityq .gt_left {
  text-align: left;
}

#lbpvwoityq .gt_center {
  text-align: center;
}

#lbpvwoityq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lbpvwoityq .gt_font_normal {
  font-weight: normal;
}

#lbpvwoityq .gt_font_bold {
  font-weight: bold;
}

#lbpvwoityq .gt_font_italic {
  font-style: italic;
}

#lbpvwoityq .gt_super {
  font-size: 65%;
}

#lbpvwoityq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lbpvwoityq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lbpvwoityq .gt_indent_1 {
  text-indent: 5px;
}

#lbpvwoityq .gt_indent_2 {
  text-indent: 10px;
}

#lbpvwoityq .gt_indent_3 {
  text-indent: 15px;
}

#lbpvwoityq .gt_indent_4 {
  text-indent: 20px;
}

#lbpvwoityq .gt_indent_5 {
  text-indent: 25px;
}

#lbpvwoityq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lbpvwoityq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of documentation of offering call to PT advocate</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.46 (1.02 to 2.10)</td>
<td headers="p.value_1" class="gt_row gt_center">0.039</td>
<td headers="estimate_2" class="gt_row gt_center">2.52 (1.35 to 4.76)</td>
<td headers="p.value_2" class="gt_row gt_center">0.004</td>
<td headers="estimate_3" class="gt_row gt_center">0.96 (0.61 to 1.50)</td>
<td headers="p.value_3" class="gt_row gt_center">0.85</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate_1" class="gt_row gt_center">0.99 (0.97 to 1.00)</td>
<td headers="p.value_1" class="gt_row gt_center">0.12</td>
<td headers="estimate_2" class="gt_row gt_center">0.99 (0.97 to 1.01)</td>
<td headers="p.value_2" class="gt_row gt_center">0.31</td>
<td headers="estimate_3" class="gt_row gt_center">0.99 (0.97 to 1.00)</td>
<td headers="p.value_3" class="gt_row gt_center">0.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.53</td>
<td headers="estimate_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.45</td>
<td headers="estimate_3" class="gt_row gt_center"><br /></td>
<td headers="p.value_3" class="gt_row gt_center">0.47</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate_1" class="gt_row gt_center">0.81 (0.51 to 1.27)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.31 (0.62 to 2.82)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">0.77 (0.47 to 1.26)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate_1" class="gt_row gt_center">0.96 (0.60 to 1.54)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.16 (0.58 to 2.33)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">0.85 (0.50 to 1.46)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate_1" class="gt_row gt_center">1.42 (0.69 to 3.06)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">2.56 (0.80 to 9.97)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.46 (0.64 to 3.52)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate_1" class="gt_row gt_center">1.90 (0.95 to 3.97)</td>
<td headers="p.value_1" class="gt_row gt_center">0.068</td>
<td headers="estimate_2" class="gt_row gt_center">2.61 (0.72 to 12.5)</td>
<td headers="p.value_2" class="gt_row gt_center">0.15</td>
<td headers="estimate_3" class="gt_row gt_center">1.69 (0.82 to 3.60)</td>
<td headers="p.value_3" class="gt_row gt_center">0.16</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate_1" class="gt_row gt_center">0.88 (0.60 to 1.29)</td>
<td headers="p.value_1" class="gt_row gt_center">0.51</td>
<td headers="estimate_2" class="gt_row gt_center">0.71 (0.38 to 1.30)</td>
<td headers="p.value_2" class="gt_row gt_center">0.26</td>
<td headers="estimate_3" class="gt_row gt_center">0.91 (0.59 to 1.39)</td>
<td headers="p.value_3" class="gt_row gt_center">0.65</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program</td>
<td headers="estimate_1" class="gt_row gt_center">2.99 (2.08 to 4.33)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">2.26 (1.21 to 4.25)</td>
<td headers="p.value_2" class="gt_row gt_center">0.010</td>
<td headers="estimate_3" class="gt_row gt_center">2.32 (1.53 to 3.55)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```
#### Unadjusted Odds of Offering to Contact Advocate


``` r
ua.advocate.regression<-function(data, exposure){
  data$advocate_offered <- as_factor(data$advocate_offered )
  data$advocate_offered <- relevel(data$advocate_offered, ref= "No Documentation of Pt Advocate")
  model <-  glm(advocate_offered ~ exposure, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```


``` r
ua.tbl.reg.path.adv <-ua.advocate.regression(cohort.1, cohort.1$exposure)
ua.tbl.reg.pp.adv <-ua.advocate.regression(pp.cohort.1, pp.cohort.1$exposure)
ua.tbl.reg.prepn.adv <-ua.advocate.regression(pp.cohort.2, pp.cohort.2$exposure)
ua.tbl.all.adv <- tbl_merge(list(ua.tbl.reg.pp.adv, ua.tbl.reg.path.adv, ua.tbl.reg.prepn.adv), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Unadjusted Odds of documentation of offering call to PT advocate")
ua.tbl.all.adv
```

```{=html}
<div id="hjuirzikom" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hjuirzikom table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hjuirzikom thead, #hjuirzikom tbody, #hjuirzikom tfoot, #hjuirzikom tr, #hjuirzikom td, #hjuirzikom th {
  border-style: none;
}

#hjuirzikom p {
  margin: 0;
  padding: 0;
}

#hjuirzikom .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hjuirzikom .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hjuirzikom .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hjuirzikom .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hjuirzikom .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hjuirzikom .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hjuirzikom .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hjuirzikom .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hjuirzikom .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hjuirzikom .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hjuirzikom .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hjuirzikom .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hjuirzikom .gt_spanner_row {
  border-bottom-style: hidden;
}

#hjuirzikom .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hjuirzikom .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hjuirzikom .gt_from_md > :first-child {
  margin-top: 0;
}

#hjuirzikom .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hjuirzikom .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hjuirzikom .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hjuirzikom .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hjuirzikom .gt_row_group_first td {
  border-top-width: 2px;
}

#hjuirzikom .gt_row_group_first th {
  border-top-width: 2px;
}

#hjuirzikom .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjuirzikom .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hjuirzikom .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hjuirzikom .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hjuirzikom .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjuirzikom .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hjuirzikom .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hjuirzikom .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hjuirzikom .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hjuirzikom .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hjuirzikom .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjuirzikom .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hjuirzikom .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjuirzikom .gt_left {
  text-align: left;
}

#hjuirzikom .gt_center {
  text-align: center;
}

#hjuirzikom .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hjuirzikom .gt_font_normal {
  font-weight: normal;
}

#hjuirzikom .gt_font_bold {
  font-weight: bold;
}

#hjuirzikom .gt_font_italic {
  font-style: italic;
}

#hjuirzikom .gt_super {
  font-size: 65%;
}

#hjuirzikom .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hjuirzikom .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hjuirzikom .gt_indent_1 {
  text-indent: 5px;
}

#hjuirzikom .gt_indent_2 {
  text-indent: 10px;
}

#hjuirzikom .gt_indent_3 {
  text-indent: 15px;
}

#hjuirzikom .gt_indent_4 {
  text-indent: 20px;
}

#hjuirzikom .gt_indent_5 {
  text-indent: 25px;
}

#hjuirzikom .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hjuirzikom div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Unadjusted Odds of documentation of offering call to PT advocate</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.39 (0.99 to 1.96)</td>
<td headers="p.value_1" class="gt_row gt_center">0.056</td>
<td headers="estimate_2" class="gt_row gt_center">3.48 (2.05 to 6.04)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.78 (0.51 to 1.18)</td>
<td headers="p.value_3" class="gt_row gt_center">0.24</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

### Primary Outcome: *SANE Kit*

note: restricted to pts \<120 hours from assault









SANE Kit Done?


``` r
sane.did.reg<-function(data, exposure){
  model <-  glm(did_SANE_kit ~ exposure + age + race_eth_num + ipv+ intoxicated + sane_ED, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```


``` r
pp.cohort.1.u120 <- pp.cohort.1.u120 %>% mutate(did_SANE_kit = if_else(sane_kit_YN == "Yes", 1, 0))
pp.cohort.2.u120 <- pp.cohort.2.u120 %>% mutate(did_SANE_kit = if_else(sane_kit_YN == "Yes", 1, 0))
path.u120 <- path.u120 %>% mutate(did_SANE_kit = if_else(sane_kit_YN == "Yes", 1, 0))

pp.sk.did <- sane.did.reg(pp.cohort.1.u120, pp.cohort.1.u120$exposure)
prenp.sk.did <- sane.did.reg(pp.cohort.2.u120, pp.cohort.2.u120$exposure)
path.sk.did <-sane.did.reg(path.u120, path.u120$exposure)
table.reg.sk <- tbl_merge(list(pp.sk.did, path.sk.did, prenp.sk.did), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Odds of Doing SANE Kit if <120hrs")
table.reg.sk
```

```{=html}
<div id="rammdssiqp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rammdssiqp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rammdssiqp thead, #rammdssiqp tbody, #rammdssiqp tfoot, #rammdssiqp tr, #rammdssiqp td, #rammdssiqp th {
  border-style: none;
}

#rammdssiqp p {
  margin: 0;
  padding: 0;
}

#rammdssiqp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rammdssiqp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rammdssiqp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rammdssiqp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rammdssiqp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rammdssiqp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rammdssiqp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rammdssiqp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rammdssiqp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rammdssiqp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rammdssiqp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rammdssiqp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rammdssiqp .gt_spanner_row {
  border-bottom-style: hidden;
}

#rammdssiqp .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rammdssiqp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rammdssiqp .gt_from_md > :first-child {
  margin-top: 0;
}

#rammdssiqp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rammdssiqp .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rammdssiqp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rammdssiqp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rammdssiqp .gt_row_group_first td {
  border-top-width: 2px;
}

#rammdssiqp .gt_row_group_first th {
  border-top-width: 2px;
}

#rammdssiqp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rammdssiqp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rammdssiqp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rammdssiqp .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rammdssiqp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rammdssiqp .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rammdssiqp .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rammdssiqp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rammdssiqp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rammdssiqp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rammdssiqp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rammdssiqp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rammdssiqp .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rammdssiqp .gt_left {
  text-align: left;
}

#rammdssiqp .gt_center {
  text-align: center;
}

#rammdssiqp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rammdssiqp .gt_font_normal {
  font-weight: normal;
}

#rammdssiqp .gt_font_bold {
  font-weight: bold;
}

#rammdssiqp .gt_font_italic {
  font-style: italic;
}

#rammdssiqp .gt_super {
  font-size: 65%;
}

#rammdssiqp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rammdssiqp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rammdssiqp .gt_indent_1 {
  text-indent: 5px;
}

#rammdssiqp .gt_indent_2 {
  text-indent: 10px;
}

#rammdssiqp .gt_indent_3 {
  text-indent: 15px;
}

#rammdssiqp .gt_indent_4 {
  text-indent: 20px;
}

#rammdssiqp .gt_indent_5 {
  text-indent: 25px;
}

#rammdssiqp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rammdssiqp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Doing SANE Kit if &lt;120hrs</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">0.83 (0.56 to 1.24)</td>
<td headers="p.value_1" class="gt_row gt_center">0.37</td>
<td headers="estimate_2" class="gt_row gt_center">2.71 (1.37 to 5.48)</td>
<td headers="p.value_2" class="gt_row gt_center">0.004</td>
<td headers="estimate_3" class="gt_row gt_center">0.52 (0.31 to 0.85)</td>
<td headers="p.value_3" class="gt_row gt_center">0.010</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate_1" class="gt_row gt_center">0.99 (0.97 to 1.00)</td>
<td headers="p.value_1" class="gt_row gt_center">0.10</td>
<td headers="estimate_2" class="gt_row gt_center">0.99 (0.97 to 1.02)</td>
<td headers="p.value_2" class="gt_row gt_center">0.54</td>
<td headers="estimate_3" class="gt_row gt_center">0.99 (0.97 to 1.00)</td>
<td headers="p.value_3" class="gt_row gt_center">0.13</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.18</td>
<td headers="estimate_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.94</td>
<td headers="estimate_3" class="gt_row gt_center"><br /></td>
<td headers="p.value_3" class="gt_row gt_center">0.12</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate_1" class="gt_row gt_center">0.67 (0.41 to 1.10)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.90 (0.41 to 2.00)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">0.61 (0.35 to 1.05)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate_1" class="gt_row gt_center">1.23 (0.73 to 2.11)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.00 (0.47 to 2.16)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.28 (0.70 to 2.40)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate_1" class="gt_row gt_center">1.35 (0.59 to 3.36)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.71 (0.23 to 2.38)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.41 (0.54 to 4.14)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate_1" class="gt_row gt_center">1.31 (0.62 to 2.96)</td>
<td headers="p.value_1" class="gt_row gt_center">0.49</td>
<td headers="estimate_2" class="gt_row gt_center">1.09 (0.30 to 4.53)</td>
<td headers="p.value_2" class="gt_row gt_center">0.90</td>
<td headers="estimate_3" class="gt_row gt_center">1.18 (0.54 to 2.73)</td>
<td headers="p.value_3" class="gt_row gt_center">0.68</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate_1" class="gt_row gt_center">0.49 (0.32 to 0.75)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.45 (0.23 to 0.87)</td>
<td headers="p.value_2" class="gt_row gt_center">0.017</td>
<td headers="estimate_3" class="gt_row gt_center">0.47 (0.29 to 0.76)</td>
<td headers="p.value_3" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program</td>
<td headers="estimate_1" class="gt_row gt_center">2.31 (1.54 to 3.51)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">1.66 (0.84 to 3.30)</td>
<td headers="p.value_2" class="gt_row gt_center">0.14</td>
<td headers="estimate_3" class="gt_row gt_center">1.72 (1.07 to 2.79)</td>
<td headers="p.value_3" class="gt_row gt_center">0.026</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

##### Unadjusted OR for doing SAFE KIT 


``` r
ua.pp.sk.did <- glm(did_SANE_kit ~ exposure, family = "binomial", data = pp.cohort.1.u120) %>%
  tbl_regression(exponentiate = T)

ua.prenp.sk.did <- glm(did_SANE_kit ~ exposure, family = "binomial", data = pp.cohort.2.u120) %>%
  tbl_regression(exponentiate = T)

ua.path.sk.did <- glm(did_SANE_kit ~ exposure, family = "binomial", data = path.u120) %>%
  tbl_regression(exponentiate = T)

ua.table.reg.sk <- tbl_merge(list(ua.pp.sk.did, ua.path.sk.did, ua.prenp.sk.did), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Unadjusted Odds of Doing SANE Kit if <120hrs")
ua.table.reg.sk
```

```{=html}
<div id="baynthsavm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#baynthsavm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#baynthsavm thead, #baynthsavm tbody, #baynthsavm tfoot, #baynthsavm tr, #baynthsavm td, #baynthsavm th {
  border-style: none;
}

#baynthsavm p {
  margin: 0;
  padding: 0;
}

#baynthsavm .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#baynthsavm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#baynthsavm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#baynthsavm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#baynthsavm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#baynthsavm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#baynthsavm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#baynthsavm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#baynthsavm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#baynthsavm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#baynthsavm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#baynthsavm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#baynthsavm .gt_spanner_row {
  border-bottom-style: hidden;
}

#baynthsavm .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#baynthsavm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#baynthsavm .gt_from_md > :first-child {
  margin-top: 0;
}

#baynthsavm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#baynthsavm .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#baynthsavm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#baynthsavm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#baynthsavm .gt_row_group_first td {
  border-top-width: 2px;
}

#baynthsavm .gt_row_group_first th {
  border-top-width: 2px;
}

#baynthsavm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#baynthsavm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#baynthsavm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#baynthsavm .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#baynthsavm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#baynthsavm .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#baynthsavm .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#baynthsavm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#baynthsavm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#baynthsavm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#baynthsavm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#baynthsavm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#baynthsavm .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#baynthsavm .gt_left {
  text-align: left;
}

#baynthsavm .gt_center {
  text-align: center;
}

#baynthsavm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#baynthsavm .gt_font_normal {
  font-weight: normal;
}

#baynthsavm .gt_font_bold {
  font-weight: bold;
}

#baynthsavm .gt_font_italic {
  font-style: italic;
}

#baynthsavm .gt_super {
  font-size: 65%;
}

#baynthsavm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#baynthsavm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#baynthsavm .gt_indent_1 {
  text-indent: 5px;
}

#baynthsavm .gt_indent_2 {
  text-indent: 10px;
}

#baynthsavm .gt_indent_3 {
  text-indent: 15px;
}

#baynthsavm .gt_indent_4 {
  text-indent: 20px;
}

#baynthsavm .gt_indent_5 {
  text-indent: 25px;
}

#baynthsavm .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#baynthsavm div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Unadjusted Odds of Doing SANE Kit if &lt;120hrs</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">0.88 (0.60 to 1.29)</td>
<td headers="p.value_1" class="gt_row gt_center">0.52</td>
<td headers="estimate_2" class="gt_row gt_center">2.86 (1.62 to 5.15)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.53 (0.33 to 0.84)</td>
<td headers="p.value_3" class="gt_row gt_center">0.007</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

### Secondary Outcome: *For Women =< 55 Presenting < 120 Hours*


``` r
preg.test.reg<-function(data, exposure){
  model<-glm(pregnancy_test_ordered_num ~ exposure + age + race_eth_num + ipv+ intoxicated + sane_ED, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}

preg.prev.reg<-function(data, exposure){
   model<-glm(prevent.preg ~ exposure + age + race_eth_num + ipv+ intoxicated + sane_ED, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```

Pregnancy test ordered?


``` r
preg.test.pp<-pp.cohort.1.u120 %>% filter(female == 1, age <= 55) %>% preg.test.reg(., .$exposure)
preg.test.prenp<-pp.cohort.2.u120 %>% filter(female == 1, age <= 55) %>% preg.test.reg(., .$exposure)
preg.test.path<-path.u120 %>% filter(female == 1, age <= 55) %>% preg.test.reg(., .$exposure)
table.reg.pregtest <- tbl_merge(list(preg.test.pp, preg.test.path, preg.test.prenp), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Odds of Pregnancy Test Ordered (if =/<55 & <120 hrs)")
table.reg.pregtest
```

```{=html}
<div id="xrigdwzwwa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xrigdwzwwa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xrigdwzwwa thead, #xrigdwzwwa tbody, #xrigdwzwwa tfoot, #xrigdwzwwa tr, #xrigdwzwwa td, #xrigdwzwwa th {
  border-style: none;
}

#xrigdwzwwa p {
  margin: 0;
  padding: 0;
}

#xrigdwzwwa .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xrigdwzwwa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xrigdwzwwa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xrigdwzwwa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xrigdwzwwa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xrigdwzwwa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrigdwzwwa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xrigdwzwwa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xrigdwzwwa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xrigdwzwwa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xrigdwzwwa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xrigdwzwwa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xrigdwzwwa .gt_spanner_row {
  border-bottom-style: hidden;
}

#xrigdwzwwa .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#xrigdwzwwa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xrigdwzwwa .gt_from_md > :first-child {
  margin-top: 0;
}

#xrigdwzwwa .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xrigdwzwwa .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xrigdwzwwa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#xrigdwzwwa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#xrigdwzwwa .gt_row_group_first td {
  border-top-width: 2px;
}

#xrigdwzwwa .gt_row_group_first th {
  border-top-width: 2px;
}

#xrigdwzwwa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrigdwzwwa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xrigdwzwwa .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xrigdwzwwa .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrigdwzwwa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrigdwzwwa .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xrigdwzwwa .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xrigdwzwwa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xrigdwzwwa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrigdwzwwa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xrigdwzwwa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrigdwzwwa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xrigdwzwwa .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrigdwzwwa .gt_left {
  text-align: left;
}

#xrigdwzwwa .gt_center {
  text-align: center;
}

#xrigdwzwwa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xrigdwzwwa .gt_font_normal {
  font-weight: normal;
}

#xrigdwzwwa .gt_font_bold {
  font-weight: bold;
}

#xrigdwzwwa .gt_font_italic {
  font-style: italic;
}

#xrigdwzwwa .gt_super {
  font-size: 65%;
}

#xrigdwzwwa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xrigdwzwwa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xrigdwzwwa .gt_indent_1 {
  text-indent: 5px;
}

#xrigdwzwwa .gt_indent_2 {
  text-indent: 10px;
}

#xrigdwzwwa .gt_indent_3 {
  text-indent: 15px;
}

#xrigdwzwwa .gt_indent_4 {
  text-indent: 20px;
}

#xrigdwzwwa .gt_indent_5 {
  text-indent: 25px;
}

#xrigdwzwwa .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#xrigdwzwwa div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Pregnancy Test Ordered (if =/&lt;55 &amp; &lt;120 hrs)</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.41 (0.92 to 2.18)</td>
<td headers="p.value_1" class="gt_row gt_center">0.12</td>
<td headers="estimate_2" class="gt_row gt_center">5.38 (2.31 to 13.5)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.70 (0.41 to 1.20)</td>
<td headers="p.value_3" class="gt_row gt_center">0.20</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate_1" class="gt_row gt_center">0.98 (0.96 to 1.00)</td>
<td headers="p.value_1" class="gt_row gt_center">0.056</td>
<td headers="estimate_2" class="gt_row gt_center">0.98 (0.94 to 1.02)</td>
<td headers="p.value_2" class="gt_row gt_center">0.35</td>
<td headers="estimate_3" class="gt_row gt_center">0.98 (0.95 to 1.00)</td>
<td headers="p.value_3" class="gt_row gt_center">0.10</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.41</td>
<td headers="estimate_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.73</td>
<td headers="estimate_3" class="gt_row gt_center"><br /></td>
<td headers="p.value_3" class="gt_row gt_center">0.39</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate_1" class="gt_row gt_center">1.40 (0.82 to 2.42)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.40 (0.57 to 3.66)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.33 (0.75 to 2.42)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate_1" class="gt_row gt_center">1.54 (0.88 to 2.79)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.55 (0.63 to 4.10)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.59 (0.83 to 3.16)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate_1" class="gt_row gt_center">1.13 (0.51 to 2.74)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.88 (0.25 to 3.59)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">0.81 (0.33 to 2.08)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate_1" class="gt_row gt_center">1.11 (0.52 to 2.54)</td>
<td headers="p.value_1" class="gt_row gt_center">0.79</td>
<td headers="estimate_2" class="gt_row gt_center">0.64 (0.17 to 2.78)</td>
<td headers="p.value_2" class="gt_row gt_center">0.53</td>
<td headers="estimate_3" class="gt_row gt_center">1.23 (0.55 to 2.98)</td>
<td headers="p.value_3" class="gt_row gt_center">0.62</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate_1" class="gt_row gt_center">0.78 (0.49 to 1.25)</td>
<td headers="p.value_1" class="gt_row gt_center">0.30</td>
<td headers="estimate_2" class="gt_row gt_center">0.57 (0.25 to 1.27)</td>
<td headers="p.value_2" class="gt_row gt_center">0.17</td>
<td headers="estimate_3" class="gt_row gt_center">0.73 (0.43 to 1.23)</td>
<td headers="p.value_3" class="gt_row gt_center">0.24</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program</td>
<td headers="estimate_1" class="gt_row gt_center">0.75 (0.49 to 1.16)</td>
<td headers="p.value_1" class="gt_row gt_center">0.20</td>
<td headers="estimate_2" class="gt_row gt_center">0.45 (0.19 to 1.03)</td>
<td headers="p.value_2" class="gt_row gt_center">0.059</td>
<td headers="estimate_3" class="gt_row gt_center">0.56 (0.34 to 0.93)</td>
<td headers="p.value_3" class="gt_row gt_center">0.023</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

Pregnancy prophylaxis ordered?


``` r
preg.prev.pp<-pp.cohort.1.u120 %>% filter(female == 1, age <= 55) %>% preg.prev.reg(., .$exposure)
preg.prev.prenp<-pp.cohort.2.u120 %>% filter(female == 1, age <= 55) %>% preg.prev.reg(., .$exposure)
preg.prev.path<-path.u120 %>% filter(female == 1, age <= 55) %>% preg.prev.reg(., .$exposure)
table.reg.pregprev <- tbl_merge(list(preg.prev.pp, preg.prev.path, preg.prev.prenp), tab_spanner = c("**Pre v. Post**", "**Pathway v. No Pathway**", "**Pre v. No Pathway**")) %>% as_gt() %>% tab_header("Odds of Pregnancy Prophylaxis Ordered (if =/<55 & <120 hrs)")
table.reg.pregprev
```

```{=html}
<div id="xrxpwhjiem" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xrxpwhjiem table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xrxpwhjiem thead, #xrxpwhjiem tbody, #xrxpwhjiem tfoot, #xrxpwhjiem tr, #xrxpwhjiem td, #xrxpwhjiem th {
  border-style: none;
}

#xrxpwhjiem p {
  margin: 0;
  padding: 0;
}

#xrxpwhjiem .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xrxpwhjiem .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xrxpwhjiem .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xrxpwhjiem .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xrxpwhjiem .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xrxpwhjiem .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrxpwhjiem .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xrxpwhjiem .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xrxpwhjiem .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xrxpwhjiem .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xrxpwhjiem .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xrxpwhjiem .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xrxpwhjiem .gt_spanner_row {
  border-bottom-style: hidden;
}

#xrxpwhjiem .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#xrxpwhjiem .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xrxpwhjiem .gt_from_md > :first-child {
  margin-top: 0;
}

#xrxpwhjiem .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xrxpwhjiem .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xrxpwhjiem .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#xrxpwhjiem .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#xrxpwhjiem .gt_row_group_first td {
  border-top-width: 2px;
}

#xrxpwhjiem .gt_row_group_first th {
  border-top-width: 2px;
}

#xrxpwhjiem .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrxpwhjiem .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xrxpwhjiem .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xrxpwhjiem .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrxpwhjiem .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrxpwhjiem .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xrxpwhjiem .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xrxpwhjiem .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xrxpwhjiem .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrxpwhjiem .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xrxpwhjiem .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrxpwhjiem .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xrxpwhjiem .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrxpwhjiem .gt_left {
  text-align: left;
}

#xrxpwhjiem .gt_center {
  text-align: center;
}

#xrxpwhjiem .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xrxpwhjiem .gt_font_normal {
  font-weight: normal;
}

#xrxpwhjiem .gt_font_bold {
  font-weight: bold;
}

#xrxpwhjiem .gt_font_italic {
  font-style: italic;
}

#xrxpwhjiem .gt_super {
  font-size: 65%;
}

#xrxpwhjiem .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xrxpwhjiem .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xrxpwhjiem .gt_indent_1 {
  text-indent: 5px;
}

#xrxpwhjiem .gt_indent_2 {
  text-indent: 10px;
}

#xrxpwhjiem .gt_indent_3 {
  text-indent: 15px;
}

#xrxpwhjiem .gt_indent_4 {
  text-indent: 20px;
}

#xrxpwhjiem .gt_indent_5 {
  text-indent: 25px;
}

#xrxpwhjiem .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#xrxpwhjiem div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Pregnancy Prophylaxis Ordered (if =/&lt;55 &amp; &lt;120 hrs)</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. Post**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. Post</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. No Pathway</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.25 (0.86 to 1.83)</td>
<td headers="p.value_1" class="gt_row gt_center">0.25</td>
<td headers="estimate_2" class="gt_row gt_center">3.23 (1.63 to 6.67)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.82 (0.49 to 1.36)</td>
<td headers="p.value_3" class="gt_row gt_center">0.43</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate_1" class="gt_row gt_center">0.98 (0.96 to 1.00)</td>
<td headers="p.value_1" class="gt_row gt_center">0.10</td>
<td headers="estimate_2" class="gt_row gt_center">1.00 (0.96 to 1.03)</td>
<td headers="p.value_2" class="gt_row gt_center">0.92</td>
<td headers="estimate_3" class="gt_row gt_center">0.97 (0.95 to 1.00)</td>
<td headers="p.value_3" class="gt_row gt_center">0.027</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.48</td>
<td headers="estimate_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.24</td>
<td headers="estimate_3" class="gt_row gt_center"><br /></td>
<td headers="p.value_3" class="gt_row gt_center">0.52</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate_1" class="gt_row gt_center">1.00 (0.62 to 1.63)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.18 (0.55 to 2.53)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.01 (0.58 to 1.76)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate_1" class="gt_row gt_center">1.45 (0.89 to 2.38)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.28 (0.62 to 2.66)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.48 (0.82 to 2.68)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate_1" class="gt_row gt_center">1.08 (0.51 to 2.28)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.39 (0.11 to 1.21)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.43 (0.58 to 3.47)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate_1" class="gt_row gt_center">0.35 (0.14 to 0.75)</td>
<td headers="p.value_1" class="gt_row gt_center">0.007</td>
<td headers="estimate_2" class="gt_row gt_center">0.57 (0.14 to 2.03)</td>
<td headers="p.value_2" class="gt_row gt_center">0.39</td>
<td headers="estimate_3" class="gt_row gt_center">0.27 (0.09 to 0.66)</td>
<td headers="p.value_3" class="gt_row gt_center">0.003</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate_1" class="gt_row gt_center">1.03 (0.67 to 1.57)</td>
<td headers="p.value_1" class="gt_row gt_center">0.91</td>
<td headers="estimate_2" class="gt_row gt_center">0.74 (0.37 to 1.44)</td>
<td headers="p.value_2" class="gt_row gt_center">0.37</td>
<td headers="estimate_3" class="gt_row gt_center">1.12 (0.67 to 1.87)</td>
<td headers="p.value_3" class="gt_row gt_center">0.68</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program</td>
<td headers="estimate_1" class="gt_row gt_center">1.06 (0.72 to 1.55)</td>
<td headers="p.value_1" class="gt_row gt_center">0.78</td>
<td headers="estimate_2" class="gt_row gt_center">0.58 (0.28 to 1.14)</td>
<td headers="p.value_2" class="gt_row gt_center">0.12</td>
<td headers="estimate_3" class="gt_row gt_center">0.91 (0.57 to 1.45)</td>
<td headers="p.value_3" class="gt_row gt_center">0.69</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

#### Unadjusted Outcomes: Pregnancy Test and Prevention



``` r
ua.preg.test.reg<-function(data, exposure){
  model<-glm(pregnancy_test_ordered_num ~ exposure, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}

ua.preg.prev.reg<-function(data, exposure){
   model<-glm(prevent.preg ~ exposure, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```

*Unadjusted* Pregnancy test ordered?


``` r
ua.preg.test.pp<-pp.cohort.1.u120 %>% filter(female == 1, age <= 55) %>% ua.preg.test.reg(., .$exposure)
ua.preg.test.prenp<-pp.cohort.2.u120 %>% filter(female == 1, age <= 55) %>% ua.preg.test.reg(., .$exposure)
ua.preg.test.path<-path.u120 %>% filter(female == 1, age <= 55) %>% ua.preg.test.reg(., .$exposure)
ua.table.reg.pregtest <- tbl_merge(list(ua.preg.test.pp, ua.preg.test.path, ua.preg.test.prenp), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Unadjusted Odds of Pregnancy Test Ordered (if =/<55 & <120 hrs)")
ua.table.reg.pregtest
```

```{=html}
<div id="hjmujjtldd" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hjmujjtldd table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hjmujjtldd thead, #hjmujjtldd tbody, #hjmujjtldd tfoot, #hjmujjtldd tr, #hjmujjtldd td, #hjmujjtldd th {
  border-style: none;
}

#hjmujjtldd p {
  margin: 0;
  padding: 0;
}

#hjmujjtldd .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hjmujjtldd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hjmujjtldd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hjmujjtldd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hjmujjtldd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hjmujjtldd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hjmujjtldd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hjmujjtldd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hjmujjtldd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hjmujjtldd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hjmujjtldd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hjmujjtldd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hjmujjtldd .gt_spanner_row {
  border-bottom-style: hidden;
}

#hjmujjtldd .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hjmujjtldd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hjmujjtldd .gt_from_md > :first-child {
  margin-top: 0;
}

#hjmujjtldd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hjmujjtldd .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hjmujjtldd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hjmujjtldd .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hjmujjtldd .gt_row_group_first td {
  border-top-width: 2px;
}

#hjmujjtldd .gt_row_group_first th {
  border-top-width: 2px;
}

#hjmujjtldd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjmujjtldd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hjmujjtldd .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hjmujjtldd .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hjmujjtldd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjmujjtldd .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hjmujjtldd .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hjmujjtldd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hjmujjtldd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hjmujjtldd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hjmujjtldd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjmujjtldd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hjmujjtldd .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hjmujjtldd .gt_left {
  text-align: left;
}

#hjmujjtldd .gt_center {
  text-align: center;
}

#hjmujjtldd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hjmujjtldd .gt_font_normal {
  font-weight: normal;
}

#hjmujjtldd .gt_font_bold {
  font-weight: bold;
}

#hjmujjtldd .gt_font_italic {
  font-style: italic;
}

#hjmujjtldd .gt_super {
  font-size: 65%;
}

#hjmujjtldd .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hjmujjtldd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hjmujjtldd .gt_indent_1 {
  text-indent: 5px;
}

#hjmujjtldd .gt_indent_2 {
  text-indent: 10px;
}

#hjmujjtldd .gt_indent_3 {
  text-indent: 15px;
}

#hjmujjtldd .gt_indent_4 {
  text-indent: 20px;
}

#hjmujjtldd .gt_indent_5 {
  text-indent: 25px;
}

#hjmujjtldd .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hjmujjtldd div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Unadjusted Odds of Pregnancy Test Ordered (if =/&lt;55 &amp; &lt;120 hrs)</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.43 (0.94 to 2.20)</td>
<td headers="p.value_1" class="gt_row gt_center">0.10</td>
<td headers="estimate_2" class="gt_row gt_center">3.32 (1.68 to 6.87)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.85 (0.52 to 1.40)</td>
<td headers="p.value_3" class="gt_row gt_center">0.51</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

*Unadjusted* Pregnancy prophylaxis ordered?


``` r
ua.preg.prev.pp<-pp.cohort.1.u120 %>% filter(female == 1, age <= 55) %>% ua.preg.prev.reg(., .$exposure)
ua.preg.prev.prenp<-pp.cohort.2.u120 %>% filter(female == 1, age <= 55) %>% ua.preg.prev.reg(., .$exposure)
ua.preg.prev.path<-path.u120 %>% filter(female == 1, age <= 55) %>% ua.preg.prev.reg(., .$exposure)
ua.table.reg.pregprev <- tbl_merge(list(ua.preg.prev.pp, ua.preg.prev.path, ua.preg.prev.prenp), tab_spanner = c("**Pre v. Post**", "**Pathway v. No Pathway**", "**Pre v. No Pathway**")) %>% as_gt() %>% tab_header("Unadjusted Odds of Pregnancy Prophylaxis Ordered (if =/<55 & <120 hrs)")
ua.table.reg.pregprev
```

```{=html}
<div id="vislpyqhrf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vislpyqhrf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vislpyqhrf thead, #vislpyqhrf tbody, #vislpyqhrf tfoot, #vislpyqhrf tr, #vislpyqhrf td, #vislpyqhrf th {
  border-style: none;
}

#vislpyqhrf p {
  margin: 0;
  padding: 0;
}

#vislpyqhrf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vislpyqhrf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vislpyqhrf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vislpyqhrf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vislpyqhrf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vislpyqhrf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vislpyqhrf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vislpyqhrf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vislpyqhrf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vislpyqhrf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vislpyqhrf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vislpyqhrf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vislpyqhrf .gt_spanner_row {
  border-bottom-style: hidden;
}

#vislpyqhrf .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vislpyqhrf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vislpyqhrf .gt_from_md > :first-child {
  margin-top: 0;
}

#vislpyqhrf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vislpyqhrf .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vislpyqhrf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vislpyqhrf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vislpyqhrf .gt_row_group_first td {
  border-top-width: 2px;
}

#vislpyqhrf .gt_row_group_first th {
  border-top-width: 2px;
}

#vislpyqhrf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#vislpyqhrf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vislpyqhrf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vislpyqhrf .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vislpyqhrf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#vislpyqhrf .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vislpyqhrf .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vislpyqhrf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vislpyqhrf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vislpyqhrf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vislpyqhrf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#vislpyqhrf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vislpyqhrf .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#vislpyqhrf .gt_left {
  text-align: left;
}

#vislpyqhrf .gt_center {
  text-align: center;
}

#vislpyqhrf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vislpyqhrf .gt_font_normal {
  font-weight: normal;
}

#vislpyqhrf .gt_font_bold {
  font-weight: bold;
}

#vislpyqhrf .gt_font_italic {
  font-style: italic;
}

#vislpyqhrf .gt_super {
  font-size: 65%;
}

#vislpyqhrf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vislpyqhrf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vislpyqhrf .gt_indent_1 {
  text-indent: 5px;
}

#vislpyqhrf .gt_indent_2 {
  text-indent: 10px;
}

#vislpyqhrf .gt_indent_3 {
  text-indent: 15px;
}

#vislpyqhrf .gt_indent_4 {
  text-indent: 20px;
}

#vislpyqhrf .gt_indent_5 {
  text-indent: 25px;
}

#vislpyqhrf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#vislpyqhrf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Unadjusted Odds of Pregnancy Prophylaxis Ordered (if =/&lt;55 &amp; &lt;120 hrs)</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. Post**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. Post</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. No Pathway</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.33 (0.92 to 1.94)</td>
<td headers="p.value_1" class="gt_row gt_center">0.13</td>
<td headers="estimate_2" class="gt_row gt_center">2.31 (1.33 to 4.07)</td>
<td headers="p.value_2" class="gt_row gt_center">0.003</td>
<td headers="estimate_3" class="gt_row gt_center">0.86 (0.53 to 1.38)</td>
<td headers="p.value_3" class="gt_row gt_center">0.54</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


### Secondary Outcome: *HIV PEP*


``` r
hiv.pep.reg<-function(data, exposure){
  model<-glm(hiv_pep_kit_ordered_num ~ exposure + age + race_eth_num + ipv + intoxicated + sane_ED, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```


``` r
pathway.u72 <- cohort.1 %>% filter(u72 == 1)
pp.u72 <- pp.cohort.1 %>% filter(u72 == 1)
prenp.u72 <- pp.cohort.2 %>% filter(u72 == 1)
tbl.pep.pp <- hiv.pep.reg(pp.u72, pp.u72$exposure)
tbl.pep.path <- hiv.pep.reg(pathway.u72, pathway.u72$exposure)
#### WARNING. Non-convirgence for no path vs pre. has to do with zero cell in ipv variable. does it matter?
tbl.pep.prenp <- hiv.pep.reg(prenp.u72, prenp.u72$exposure)
tbl.pep.hiv <- tbl_merge(list(tbl.pep.pp, tbl.pep.path, tbl.pep.prenp), tab_spanner = c("**Pre v. Post**", "**Pathway v. No Pathway**", "**Pre v. No Pathway**")) %>% as_gt() %>% tab_header("Odds of Receiving HIV PEP if Presented to ED <72 Hours Since Assault")
tbl.pep.hiv
```

```{=html}
<div id="zotorwnqlo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zotorwnqlo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zotorwnqlo thead, #zotorwnqlo tbody, #zotorwnqlo tfoot, #zotorwnqlo tr, #zotorwnqlo td, #zotorwnqlo th {
  border-style: none;
}

#zotorwnqlo p {
  margin: 0;
  padding: 0;
}

#zotorwnqlo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zotorwnqlo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zotorwnqlo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zotorwnqlo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zotorwnqlo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zotorwnqlo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zotorwnqlo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zotorwnqlo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zotorwnqlo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zotorwnqlo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zotorwnqlo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zotorwnqlo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zotorwnqlo .gt_spanner_row {
  border-bottom-style: hidden;
}

#zotorwnqlo .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zotorwnqlo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zotorwnqlo .gt_from_md > :first-child {
  margin-top: 0;
}

#zotorwnqlo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zotorwnqlo .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zotorwnqlo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zotorwnqlo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zotorwnqlo .gt_row_group_first td {
  border-top-width: 2px;
}

#zotorwnqlo .gt_row_group_first th {
  border-top-width: 2px;
}

#zotorwnqlo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zotorwnqlo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zotorwnqlo .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zotorwnqlo .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zotorwnqlo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zotorwnqlo .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zotorwnqlo .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zotorwnqlo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zotorwnqlo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zotorwnqlo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zotorwnqlo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zotorwnqlo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zotorwnqlo .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zotorwnqlo .gt_left {
  text-align: left;
}

#zotorwnqlo .gt_center {
  text-align: center;
}

#zotorwnqlo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zotorwnqlo .gt_font_normal {
  font-weight: normal;
}

#zotorwnqlo .gt_font_bold {
  font-weight: bold;
}

#zotorwnqlo .gt_font_italic {
  font-style: italic;
}

#zotorwnqlo .gt_super {
  font-size: 65%;
}

#zotorwnqlo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zotorwnqlo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zotorwnqlo .gt_indent_1 {
  text-indent: 5px;
}

#zotorwnqlo .gt_indent_2 {
  text-indent: 10px;
}

#zotorwnqlo .gt_indent_3 {
  text-indent: 15px;
}

#zotorwnqlo .gt_indent_4 {
  text-indent: 20px;
}

#zotorwnqlo .gt_indent_5 {
  text-indent: 25px;
}

#zotorwnqlo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zotorwnqlo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Receiving HIV PEP if Presented to ED &lt;72 Hours Since Assault</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. Post**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. Post</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. No Pathway</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">3.85 (2.42 to 6.22)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">8.70 (3.93 to 20.7)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.86 (0.41 to 1.73)</td>
<td headers="p.value_3" class="gt_row gt_center">0.68</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate_1" class="gt_row gt_center">0.98 (0.95 to 1.00)</td>
<td headers="p.value_1" class="gt_row gt_center">0.022</td>
<td headers="estimate_2" class="gt_row gt_center">0.99 (0.96 to 1.01)</td>
<td headers="p.value_2" class="gt_row gt_center">0.32</td>
<td headers="estimate_3" class="gt_row gt_center">0.98 (0.95 to 1.01)</td>
<td headers="p.value_3" class="gt_row gt_center">0.24</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.91</td>
<td headers="estimate_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.49</td>
<td headers="estimate_3" class="gt_row gt_center"><br /></td>
<td headers="p.value_3" class="gt_row gt_center">0.87</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate_1" class="gt_row gt_center">1.15 (0.64 to 2.05)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.54 (0.64 to 3.69)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.37 (0.63 to 2.94)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate_1" class="gt_row gt_center">1.22 (0.68 to 2.17)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.95 (0.42 to 2.16)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.19 (0.50 to 2.68)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate_1" class="gt_row gt_center">1.04 (0.39 to 2.52)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.52 (0.13 to 1.93)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.01 (0.22 to 3.39)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate_1" class="gt_row gt_center">0.37 (0.08 to 1.12)</td>
<td headers="p.value_1" class="gt_row gt_center">0.081</td>
<td headers="estimate_2" class="gt_row gt_center">0.74 (0.13 to 3.72)</td>
<td headers="p.value_2" class="gt_row gt_center">0.71</td>
<td headers="estimate_3" class="gt_row gt_center">0.00 (0.00 to 10,725,108)</td>
<td headers="p.value_3" class="gt_row gt_center">0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate_1" class="gt_row gt_center">0.93 (0.56 to 1.53)</td>
<td headers="p.value_1" class="gt_row gt_center">0.78</td>
<td headers="estimate_2" class="gt_row gt_center">0.57 (0.27 to 1.17)</td>
<td headers="p.value_2" class="gt_row gt_center">0.13</td>
<td headers="estimate_3" class="gt_row gt_center">0.78 (0.37 to 1.58)</td>
<td headers="p.value_3" class="gt_row gt_center">0.50</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program</td>
<td headers="estimate_1" class="gt_row gt_center">1.62 (1.01 to 2.61)</td>
<td headers="p.value_1" class="gt_row gt_center">0.043</td>
<td headers="estimate_2" class="gt_row gt_center">1.39 (0.65 to 2.92)</td>
<td headers="p.value_2" class="gt_row gt_center">0.39</td>
<td headers="estimate_3" class="gt_row gt_center">0.63 (0.31 to 1.22)</td>
<td headers="p.value_3" class="gt_row gt_center">0.17</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


#### Unadjusted OR for HIV PEP


``` r
ua.hiv.pep.reg<-function(data, exposure){
  model<-glm(hiv_pep_kit_ordered_num ~ exposure, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```


``` r
ua.tbl.pep.pp <- ua.hiv.pep.reg(pp.u72, pp.u72$exposure)
ua.tbl.pep.path <- ua.hiv.pep.reg(pathway.u72, pathway.u72$exposure)
ua.tbl.pep.prenp <- ua.hiv.pep.reg(prenp.u72, prenp.u72$exposure)
ua.tbl.pep.hiv <- tbl_merge(list(ua.tbl.pep.pp, ua.tbl.pep.path, ua.tbl.pep.prenp), tab_spanner = c("**Pre v. Post**", "**Pathway v. No Pathway**", "**Pre v. No Pathway**")) %>% as_gt() %>% tab_header("Unadjusted Odds of Receiving HIV PEP if Presented to ED <72 Hours Since Assault")
ua.tbl.pep.hiv
```

```{=html}
<div id="ifbiurzzzk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ifbiurzzzk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ifbiurzzzk thead, #ifbiurzzzk tbody, #ifbiurzzzk tfoot, #ifbiurzzzk tr, #ifbiurzzzk td, #ifbiurzzzk th {
  border-style: none;
}

#ifbiurzzzk p {
  margin: 0;
  padding: 0;
}

#ifbiurzzzk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ifbiurzzzk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ifbiurzzzk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ifbiurzzzk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ifbiurzzzk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ifbiurzzzk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ifbiurzzzk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ifbiurzzzk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ifbiurzzzk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ifbiurzzzk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ifbiurzzzk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ifbiurzzzk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ifbiurzzzk .gt_spanner_row {
  border-bottom-style: hidden;
}

#ifbiurzzzk .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ifbiurzzzk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ifbiurzzzk .gt_from_md > :first-child {
  margin-top: 0;
}

#ifbiurzzzk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ifbiurzzzk .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ifbiurzzzk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ifbiurzzzk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ifbiurzzzk .gt_row_group_first td {
  border-top-width: 2px;
}

#ifbiurzzzk .gt_row_group_first th {
  border-top-width: 2px;
}

#ifbiurzzzk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifbiurzzzk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ifbiurzzzk .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ifbiurzzzk .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ifbiurzzzk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifbiurzzzk .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ifbiurzzzk .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ifbiurzzzk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ifbiurzzzk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ifbiurzzzk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ifbiurzzzk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifbiurzzzk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ifbiurzzzk .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifbiurzzzk .gt_left {
  text-align: left;
}

#ifbiurzzzk .gt_center {
  text-align: center;
}

#ifbiurzzzk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ifbiurzzzk .gt_font_normal {
  font-weight: normal;
}

#ifbiurzzzk .gt_font_bold {
  font-weight: bold;
}

#ifbiurzzzk .gt_font_italic {
  font-style: italic;
}

#ifbiurzzzk .gt_super {
  font-size: 65%;
}

#ifbiurzzzk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ifbiurzzzk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ifbiurzzzk .gt_indent_1 {
  text-indent: 5px;
}

#ifbiurzzzk .gt_indent_2 {
  text-indent: 10px;
}

#ifbiurzzzk .gt_indent_3 {
  text-indent: 15px;
}

#ifbiurzzzk .gt_indent_4 {
  text-indent: 20px;
}

#ifbiurzzzk .gt_indent_5 {
  text-indent: 25px;
}

#ifbiurzzzk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ifbiurzzzk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Unadjusted Odds of Receiving HIV PEP if Presented to ED &lt;72 Hours Since Assault</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. Post**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. Post</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pre v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pre v. No Pathway</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">3.82 (2.43 to 6.08)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">8.51 (4.34 to 17.7)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">1.02 (0.50 to 1.99)</td>
<td headers="p.value_3" class="gt_row gt_center">0.94</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

### Secondary Outcome: *ABX given*

refers to any abx given


``` r
abx.pep.reg<-function(data, exposure){
  model<-glm(abx_given ~ exposure + age + race_eth_num + ipv + intoxicated + sane_ED, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```


``` r
tbl.reg.path.abx <-abx.pep.reg(cohort.1, cohort.1$exposure)
tbl.reg.pp.abx <-abx.pep.reg(pp.cohort.1, pp.cohort.1$exposure)
tbl.reg.prepn.abx <-abx.pep.reg(pp.cohort.2, pp.cohort.2$exposure)
tbl.all.abx <- tbl_merge(list(tbl.reg.pp.abx, tbl.reg.path.abx, tbl.reg.prepn.abx), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Odds of Ordering Antibiotics")
tbl.all.abx
```

```{=html}
<div id="twlezhoonw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#twlezhoonw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#twlezhoonw thead, #twlezhoonw tbody, #twlezhoonw tfoot, #twlezhoonw tr, #twlezhoonw td, #twlezhoonw th {
  border-style: none;
}

#twlezhoonw p {
  margin: 0;
  padding: 0;
}

#twlezhoonw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#twlezhoonw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#twlezhoonw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#twlezhoonw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#twlezhoonw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#twlezhoonw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#twlezhoonw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#twlezhoonw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#twlezhoonw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#twlezhoonw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#twlezhoonw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#twlezhoonw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#twlezhoonw .gt_spanner_row {
  border-bottom-style: hidden;
}

#twlezhoonw .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#twlezhoonw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#twlezhoonw .gt_from_md > :first-child {
  margin-top: 0;
}

#twlezhoonw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#twlezhoonw .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#twlezhoonw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#twlezhoonw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#twlezhoonw .gt_row_group_first td {
  border-top-width: 2px;
}

#twlezhoonw .gt_row_group_first th {
  border-top-width: 2px;
}

#twlezhoonw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#twlezhoonw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#twlezhoonw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#twlezhoonw .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#twlezhoonw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#twlezhoonw .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#twlezhoonw .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#twlezhoonw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#twlezhoonw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#twlezhoonw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#twlezhoonw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#twlezhoonw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#twlezhoonw .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#twlezhoonw .gt_left {
  text-align: left;
}

#twlezhoonw .gt_center {
  text-align: center;
}

#twlezhoonw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#twlezhoonw .gt_font_normal {
  font-weight: normal;
}

#twlezhoonw .gt_font_bold {
  font-weight: bold;
}

#twlezhoonw .gt_font_italic {
  font-style: italic;
}

#twlezhoonw .gt_super {
  font-size: 65%;
}

#twlezhoonw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#twlezhoonw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#twlezhoonw .gt_indent_1 {
  text-indent: 5px;
}

#twlezhoonw .gt_indent_2 {
  text-indent: 10px;
}

#twlezhoonw .gt_indent_3 {
  text-indent: 15px;
}

#twlezhoonw .gt_indent_4 {
  text-indent: 20px;
}

#twlezhoonw .gt_indent_5 {
  text-indent: 25px;
}

#twlezhoonw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#twlezhoonw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Ordering Antibiotics</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.28 (0.87 to 1.88)</td>
<td headers="p.value_1" class="gt_row gt_center">0.21</td>
<td headers="estimate_2" class="gt_row gt_center">5.26 (2.54 to 11.4)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.75 (0.47 to 1.20)</td>
<td headers="p.value_3" class="gt_row gt_center">0.23</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate_1" class="gt_row gt_center">0.97 (0.96 to 0.99)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.98 (0.96 to 1.00)</td>
<td headers="p.value_2" class="gt_row gt_center">0.078</td>
<td headers="estimate_3" class="gt_row gt_center">0.97 (0.96 to 0.99)</td>
<td headers="p.value_3" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate_1" class="gt_row gt_center"><br /></td>
<td headers="p.value_1" class="gt_row gt_center">0.47</td>
<td headers="estimate_2" class="gt_row gt_center"><br /></td>
<td headers="p.value_2" class="gt_row gt_center">0.050</td>
<td headers="estimate_3" class="gt_row gt_center"><br /></td>
<td headers="p.value_3" class="gt_row gt_center">0.49</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate_1" class="gt_row gt_center">—</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">—</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">—</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate_1" class="gt_row gt_center">1.13 (0.70 to 1.84)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">1.62 (0.69 to 4.07)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.14 (0.68 to 1.92)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate_1" class="gt_row gt_center">1.34 (0.81 to 2.28)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.86 (0.40 to 1.90)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">1.55 (0.87 to 2.85)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate_1" class="gt_row gt_center">0.73 (0.35 to 1.56)</td>
<td headers="p.value_1" class="gt_row gt_center"><br /></td>
<td headers="estimate_2" class="gt_row gt_center">0.26 (0.08 to 0.81)</td>
<td headers="p.value_2" class="gt_row gt_center"><br /></td>
<td headers="estimate_3" class="gt_row gt_center">0.91 (0.39 to 2.22)</td>
<td headers="p.value_3" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate_1" class="gt_row gt_center">0.32 (0.16 to 0.61)</td>
<td headers="p.value_1" class="gt_row gt_center"><0.001</td>
<td headers="estimate_2" class="gt_row gt_center">0.25 (0.07 to 0.85)</td>
<td headers="p.value_2" class="gt_row gt_center">0.026</td>
<td headers="estimate_3" class="gt_row gt_center">0.33 (0.16 to 0.67)</td>
<td headers="p.value_3" class="gt_row gt_center">0.002</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate_1" class="gt_row gt_center">0.78 (0.52 to 1.16)</td>
<td headers="p.value_1" class="gt_row gt_center">0.21</td>
<td headers="estimate_2" class="gt_row gt_center">0.57 (0.29 to 1.10)</td>
<td headers="p.value_2" class="gt_row gt_center">0.094</td>
<td headers="estimate_3" class="gt_row gt_center">0.77 (0.49 to 1.20)</td>
<td headers="p.value_3" class="gt_row gt_center">0.25</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ED with SANE program</td>
<td headers="estimate_1" class="gt_row gt_center">1.38 (0.94 to 2.03)</td>
<td headers="p.value_1" class="gt_row gt_center">0.10</td>
<td headers="estimate_2" class="gt_row gt_center">0.69 (0.33 to 1.39)</td>
<td headers="p.value_2" class="gt_row gt_center">0.30</td>
<td headers="estimate_3" class="gt_row gt_center">1.18 (0.76 to 1.85)</td>
<td headers="p.value_3" class="gt_row gt_center">0.46</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

#### Unadjusted OR for abx admin


``` r
ua.abx.pep.reg<-function(data, exposure){
  model<-glm(abx_given ~ exposure, family = "binomial", data = data)
  model.results <- tbl_regression(model, exponentiate = TRUE) %>% add_global_p()
  return(model.results)
}
```


``` r
ua.tbl.reg.path.abx <-ua.abx.pep.reg(cohort.1, cohort.1$exposure)
ua.tbl.reg.pp.abx <-ua.abx.pep.reg(pp.cohort.1, pp.cohort.1$exposure)
ua.tbl.reg.prepn.abx <-ua.abx.pep.reg(pp.cohort.2, pp.cohort.2$exposure)
ua.tbl.all.abx <- tbl_merge(list(ua.tbl.reg.pp.abx, ua.tbl.reg.path.abx, ua.tbl.reg.prepn.abx), tab_spanner = c("**Post v. Pre**", "**Pathway v. No Pathway**", "**No Pathway v. Pre**")) %>% as_gt() %>% tab_header("Unadjusted Odds of Ordering Antibiotics")
ua.tbl.all.abx
```

```{=html}
<div id="rcrjtbovyd" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rcrjtbovyd table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rcrjtbovyd thead, #rcrjtbovyd tbody, #rcrjtbovyd tfoot, #rcrjtbovyd tr, #rcrjtbovyd td, #rcrjtbovyd th {
  border-style: none;
}

#rcrjtbovyd p {
  margin: 0;
  padding: 0;
}

#rcrjtbovyd .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rcrjtbovyd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rcrjtbovyd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rcrjtbovyd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rcrjtbovyd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rcrjtbovyd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rcrjtbovyd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rcrjtbovyd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rcrjtbovyd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rcrjtbovyd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rcrjtbovyd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rcrjtbovyd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rcrjtbovyd .gt_spanner_row {
  border-bottom-style: hidden;
}

#rcrjtbovyd .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rcrjtbovyd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rcrjtbovyd .gt_from_md > :first-child {
  margin-top: 0;
}

#rcrjtbovyd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rcrjtbovyd .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rcrjtbovyd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rcrjtbovyd .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rcrjtbovyd .gt_row_group_first td {
  border-top-width: 2px;
}

#rcrjtbovyd .gt_row_group_first th {
  border-top-width: 2px;
}

#rcrjtbovyd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rcrjtbovyd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rcrjtbovyd .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rcrjtbovyd .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rcrjtbovyd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rcrjtbovyd .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rcrjtbovyd .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rcrjtbovyd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rcrjtbovyd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rcrjtbovyd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rcrjtbovyd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rcrjtbovyd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rcrjtbovyd .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#rcrjtbovyd .gt_left {
  text-align: left;
}

#rcrjtbovyd .gt_center {
  text-align: center;
}

#rcrjtbovyd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rcrjtbovyd .gt_font_normal {
  font-weight: normal;
}

#rcrjtbovyd .gt_font_bold {
  font-weight: bold;
}

#rcrjtbovyd .gt_font_italic {
  font-style: italic;
}

#rcrjtbovyd .gt_super {
  font-size: 65%;
}

#rcrjtbovyd .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rcrjtbovyd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rcrjtbovyd .gt_indent_1 {
  text-indent: 5px;
}

#rcrjtbovyd .gt_indent_2 {
  text-indent: 10px;
}

#rcrjtbovyd .gt_indent_3 {
  text-indent: 15px;
}

#rcrjtbovyd .gt_indent_4 {
  text-indent: 20px;
}

#rcrjtbovyd .gt_indent_5 {
  text-indent: 25px;
}

#rcrjtbovyd .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rcrjtbovyd div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Unadjusted Odds of Ordering Antibiotics</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Post v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Post v. Pre</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Pathway v. No Pathway**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Pathway v. No Pathway</strong></span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**No Pathway v. Pre**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>No Pathway v. Pre</strong></span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_1"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_1"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_2"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_2"><span class='gt_from_md'><strong>p-value</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate_3"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value_3"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate_1" class="gt_row gt_center">1.33 (0.92 to 1.92)</td>
<td headers="p.value_1" class="gt_row gt_center">0.13</td>
<td headers="estimate_2" class="gt_row gt_center">3.53 (1.97 to 6.53)</td>
<td headers="p.value_2" class="gt_row gt_center"><0.001</td>
<td headers="estimate_3" class="gt_row gt_center">0.77 (0.50 to 1.18)</td>
<td headers="p.value_3" class="gt_row gt_center">0.23</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


### Tertiary Outcome: *Follow Up at Discharge?*

note: removed outcome on 8/22/24




### Restricted to YSC and SRC


``` r
#create cohort restricted to YSC and SRC (SAFE program sites)

SAFE_Site_Cohort <- pp.cohort.1 %>% 
  filter(sane_ED == 1)
SAFE_Site_Cohort_u120 <- SAFE_Site_Cohort %>% 
   mutate(did_SANE_kit = if_else(sane_kit_YN == "Yes", 1, 0)) %>%
  filter(under120h == TRUE) #383 patients u120 at SAFE sites
SAFE_Site_Cohort_u120.f <- SAFE_Site_Cohort_u120 %>% filter(female_u55 == 1)
SAFE_Site_Cohort_u72 <- SAFE_Site_Cohort %>% filter(u72 == 1)
# number of patients who had pathway used
SAFE_Site_Cohort %>% filter(agile_md_used_num == 1) %>% nrow() %>% print()
```

```
## [1] 95
```

``` r
SAFE_Site_Cohort_u120 %>% filter(agile_md_used_num == 1) %>% nrow() %>% print()
```

```
## [1] 89
```

``` r
# SAFE Kit Done; restricted to u120
reg.ss.sk.did <-  glm(did_SANE_kit ~ exposure + age + race_eth_num + ipv+ intoxicated, family = "binomial", data = SAFE_Site_Cohort_u120)
tbl.reg.ss.sk.did <- tbl_regression(reg.ss.sk.did, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds of Doing SAFE Kit (Restricted to YSC/SRC)")
tbl.reg.ss.sk.did
```

```{=html}
<div id="aqpnucnikq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#aqpnucnikq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#aqpnucnikq thead, #aqpnucnikq tbody, #aqpnucnikq tfoot, #aqpnucnikq tr, #aqpnucnikq td, #aqpnucnikq th {
  border-style: none;
}

#aqpnucnikq p {
  margin: 0;
  padding: 0;
}

#aqpnucnikq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#aqpnucnikq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#aqpnucnikq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#aqpnucnikq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#aqpnucnikq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#aqpnucnikq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aqpnucnikq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#aqpnucnikq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#aqpnucnikq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#aqpnucnikq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#aqpnucnikq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#aqpnucnikq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#aqpnucnikq .gt_spanner_row {
  border-bottom-style: hidden;
}

#aqpnucnikq .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#aqpnucnikq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#aqpnucnikq .gt_from_md > :first-child {
  margin-top: 0;
}

#aqpnucnikq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#aqpnucnikq .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#aqpnucnikq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#aqpnucnikq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#aqpnucnikq .gt_row_group_first td {
  border-top-width: 2px;
}

#aqpnucnikq .gt_row_group_first th {
  border-top-width: 2px;
}

#aqpnucnikq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#aqpnucnikq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#aqpnucnikq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#aqpnucnikq .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aqpnucnikq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#aqpnucnikq .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#aqpnucnikq .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#aqpnucnikq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#aqpnucnikq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aqpnucnikq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#aqpnucnikq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#aqpnucnikq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#aqpnucnikq .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#aqpnucnikq .gt_left {
  text-align: left;
}

#aqpnucnikq .gt_center {
  text-align: center;
}

#aqpnucnikq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#aqpnucnikq .gt_font_normal {
  font-weight: normal;
}

#aqpnucnikq .gt_font_bold {
  font-weight: bold;
}

#aqpnucnikq .gt_font_italic {
  font-style: italic;
}

#aqpnucnikq .gt_super {
  font-size: 65%;
}

#aqpnucnikq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#aqpnucnikq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#aqpnucnikq .gt_indent_1 {
  text-indent: 5px;
}

#aqpnucnikq .gt_indent_2 {
  text-indent: 10px;
}

#aqpnucnikq .gt_indent_3 {
  text-indent: 15px;
}

#aqpnucnikq .gt_indent_4 {
  text-indent: 20px;
}

#aqpnucnikq .gt_indent_5 {
  text-indent: 25px;
}

#aqpnucnikq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#aqpnucnikq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Doing SAFE Kit (Restricted to YSC/SRC)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate" class="gt_row gt_center">1.06 (0.58 to 1.93)</td>
<td headers="p.value" class="gt_row gt_center">0.86</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">1.01 (0.98 to 1.04)</td>
<td headers="p.value" class="gt_row gt_center">0.46</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.62</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate" class="gt_row gt_center">0.79 (0.40 to 1.57)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate" class="gt_row gt_center">1.07 (0.48 to 2.48)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">2.00 (0.50 to 13.4)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">1.41 (0.43 to 6.38)</td>
<td headers="p.value" class="gt_row gt_center">0.60</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate" class="gt_row gt_center">0.37 (0.19 to 0.69)</td>
<td headers="p.value" class="gt_row gt_center">0.002</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

``` r
# advocate offered

  SAFE_Site_Cohort$advocate_offered <- as_factor(SAFE_Site_Cohort$advocate_offered )
  SAFE_Site_Cohort$advocate_offered <- relevel(SAFE_Site_Cohort$advocate_offered, ref= "No Documentation of Pt Advocate")
  reg.ss.adv <-  glm(advocate_offered ~ exposure + age + race_eth_num + ipv+ intoxicated, family = "binomial", data = SAFE_Site_Cohort)
  tbl.reg.ss.adv <- tbl_regression(reg.ss.adv, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds of Offering Advocate (Restricted to YSC/SRC)")
  tbl.reg.ss.adv
```

```{=html}
<div id="zqswoxtctv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zqswoxtctv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zqswoxtctv thead, #zqswoxtctv tbody, #zqswoxtctv tfoot, #zqswoxtctv tr, #zqswoxtctv td, #zqswoxtctv th {
  border-style: none;
}

#zqswoxtctv p {
  margin: 0;
  padding: 0;
}

#zqswoxtctv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zqswoxtctv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zqswoxtctv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zqswoxtctv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zqswoxtctv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zqswoxtctv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zqswoxtctv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zqswoxtctv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zqswoxtctv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zqswoxtctv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zqswoxtctv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zqswoxtctv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zqswoxtctv .gt_spanner_row {
  border-bottom-style: hidden;
}

#zqswoxtctv .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zqswoxtctv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zqswoxtctv .gt_from_md > :first-child {
  margin-top: 0;
}

#zqswoxtctv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zqswoxtctv .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zqswoxtctv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zqswoxtctv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zqswoxtctv .gt_row_group_first td {
  border-top-width: 2px;
}

#zqswoxtctv .gt_row_group_first th {
  border-top-width: 2px;
}

#zqswoxtctv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zqswoxtctv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zqswoxtctv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zqswoxtctv .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zqswoxtctv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zqswoxtctv .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zqswoxtctv .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zqswoxtctv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zqswoxtctv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zqswoxtctv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zqswoxtctv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zqswoxtctv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zqswoxtctv .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zqswoxtctv .gt_left {
  text-align: left;
}

#zqswoxtctv .gt_center {
  text-align: center;
}

#zqswoxtctv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zqswoxtctv .gt_font_normal {
  font-weight: normal;
}

#zqswoxtctv .gt_font_bold {
  font-weight: bold;
}

#zqswoxtctv .gt_font_italic {
  font-style: italic;
}

#zqswoxtctv .gt_super {
  font-size: 65%;
}

#zqswoxtctv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zqswoxtctv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zqswoxtctv .gt_indent_1 {
  text-indent: 5px;
}

#zqswoxtctv .gt_indent_2 {
  text-indent: 10px;
}

#zqswoxtctv .gt_indent_3 {
  text-indent: 15px;
}

#zqswoxtctv .gt_indent_4 {
  text-indent: 20px;
}

#zqswoxtctv .gt_indent_5 {
  text-indent: 25px;
}

#zqswoxtctv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zqswoxtctv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Offering Advocate (Restricted to YSC/SRC)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate" class="gt_row gt_center">1.69 (0.99 to 2.91)</td>
<td headers="p.value" class="gt_row gt_center">0.053</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">1.01 (0.99 to 1.04)</td>
<td headers="p.value" class="gt_row gt_center">0.42</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.78</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate" class="gt_row gt_center">0.89 (0.48 to 1.64)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate" class="gt_row gt_center">1.03 (0.50 to 2.18)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.59 (0.20 to 1.78)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">2.91 (0.94 to 12.8)</td>
<td headers="p.value" class="gt_row gt_center">0.066</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate" class="gt_row gt_center">1.06 (0.60 to 1.88)</td>
<td headers="p.value" class="gt_row gt_center">0.83</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

``` r
# pregnancy test ordered
   reg.ss.preg <-glm(pregnancy_test_ordered_num ~ exposure + age + race_eth_num + ipv+ intoxicated, family = "binomial", data = SAFE_Site_Cohort_u120.f)
  tbl.reg.ss.preg <- tbl_regression(reg.ss.preg, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds of Ordering Pregnancy Test (Restricted to YSC/SRC)")
  tbl.reg.ss.preg
```

```{=html}
<div id="zdnakzysnl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zdnakzysnl table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zdnakzysnl thead, #zdnakzysnl tbody, #zdnakzysnl tfoot, #zdnakzysnl tr, #zdnakzysnl td, #zdnakzysnl th {
  border-style: none;
}

#zdnakzysnl p {
  margin: 0;
  padding: 0;
}

#zdnakzysnl .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zdnakzysnl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zdnakzysnl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zdnakzysnl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zdnakzysnl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zdnakzysnl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdnakzysnl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zdnakzysnl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zdnakzysnl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zdnakzysnl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zdnakzysnl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zdnakzysnl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zdnakzysnl .gt_spanner_row {
  border-bottom-style: hidden;
}

#zdnakzysnl .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zdnakzysnl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zdnakzysnl .gt_from_md > :first-child {
  margin-top: 0;
}

#zdnakzysnl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zdnakzysnl .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zdnakzysnl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zdnakzysnl .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zdnakzysnl .gt_row_group_first td {
  border-top-width: 2px;
}

#zdnakzysnl .gt_row_group_first th {
  border-top-width: 2px;
}

#zdnakzysnl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdnakzysnl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zdnakzysnl .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zdnakzysnl .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdnakzysnl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdnakzysnl .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zdnakzysnl .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zdnakzysnl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zdnakzysnl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdnakzysnl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zdnakzysnl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdnakzysnl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zdnakzysnl .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdnakzysnl .gt_left {
  text-align: left;
}

#zdnakzysnl .gt_center {
  text-align: center;
}

#zdnakzysnl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zdnakzysnl .gt_font_normal {
  font-weight: normal;
}

#zdnakzysnl .gt_font_bold {
  font-weight: bold;
}

#zdnakzysnl .gt_font_italic {
  font-style: italic;
}

#zdnakzysnl .gt_super {
  font-size: 65%;
}

#zdnakzysnl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zdnakzysnl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zdnakzysnl .gt_indent_1 {
  text-indent: 5px;
}

#zdnakzysnl .gt_indent_2 {
  text-indent: 10px;
}

#zdnakzysnl .gt_indent_3 {
  text-indent: 15px;
}

#zdnakzysnl .gt_indent_4 {
  text-indent: 20px;
}

#zdnakzysnl .gt_indent_5 {
  text-indent: 25px;
}

#zdnakzysnl .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zdnakzysnl div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Ordering Pregnancy Test (Restricted to YSC/SRC)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate" class="gt_row gt_center">1.95 (1.07 to 3.64)</td>
<td headers="p.value" class="gt_row gt_center">0.030</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">0.98 (0.95 to 1.01)</td>
<td headers="p.value" class="gt_row gt_center">0.27</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.25</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate" class="gt_row gt_center">2.02 (0.99 to 4.24)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate" class="gt_row gt_center">1.13 (0.53 to 2.47)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.70 (0.53 to 6.61)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">0.90 (0.31 to 2.87)</td>
<td headers="p.value" class="gt_row gt_center">0.86</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate" class="gt_row gt_center">0.83 (0.43 to 1.61)</td>
<td headers="p.value" class="gt_row gt_center">0.58</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

``` r
# EC ordered
  reg.ss.ec<-glm(prevent.preg ~ exposure + age + race_eth_num + ipv+ intoxicated, family = "binomial", data = SAFE_Site_Cohort_u120.f)
  tbl.reg.ss.ec <- tbl_regression(reg.ss.ec, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds of Ordering Emergency Contraception (Restricted to YSC/SRC)")
  tbl.reg.ss.ec
```

```{=html}
<div id="hwkqgmzsvs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hwkqgmzsvs table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hwkqgmzsvs thead, #hwkqgmzsvs tbody, #hwkqgmzsvs tfoot, #hwkqgmzsvs tr, #hwkqgmzsvs td, #hwkqgmzsvs th {
  border-style: none;
}

#hwkqgmzsvs p {
  margin: 0;
  padding: 0;
}

#hwkqgmzsvs .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hwkqgmzsvs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hwkqgmzsvs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hwkqgmzsvs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hwkqgmzsvs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hwkqgmzsvs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwkqgmzsvs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hwkqgmzsvs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hwkqgmzsvs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hwkqgmzsvs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hwkqgmzsvs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hwkqgmzsvs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hwkqgmzsvs .gt_spanner_row {
  border-bottom-style: hidden;
}

#hwkqgmzsvs .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hwkqgmzsvs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hwkqgmzsvs .gt_from_md > :first-child {
  margin-top: 0;
}

#hwkqgmzsvs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hwkqgmzsvs .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hwkqgmzsvs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hwkqgmzsvs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hwkqgmzsvs .gt_row_group_first td {
  border-top-width: 2px;
}

#hwkqgmzsvs .gt_row_group_first th {
  border-top-width: 2px;
}

#hwkqgmzsvs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwkqgmzsvs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hwkqgmzsvs .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hwkqgmzsvs .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwkqgmzsvs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwkqgmzsvs .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hwkqgmzsvs .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hwkqgmzsvs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hwkqgmzsvs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwkqgmzsvs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hwkqgmzsvs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwkqgmzsvs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hwkqgmzsvs .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwkqgmzsvs .gt_left {
  text-align: left;
}

#hwkqgmzsvs .gt_center {
  text-align: center;
}

#hwkqgmzsvs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hwkqgmzsvs .gt_font_normal {
  font-weight: normal;
}

#hwkqgmzsvs .gt_font_bold {
  font-weight: bold;
}

#hwkqgmzsvs .gt_font_italic {
  font-style: italic;
}

#hwkqgmzsvs .gt_super {
  font-size: 65%;
}

#hwkqgmzsvs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hwkqgmzsvs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hwkqgmzsvs .gt_indent_1 {
  text-indent: 5px;
}

#hwkqgmzsvs .gt_indent_2 {
  text-indent: 10px;
}

#hwkqgmzsvs .gt_indent_3 {
  text-indent: 15px;
}

#hwkqgmzsvs .gt_indent_4 {
  text-indent: 20px;
}

#hwkqgmzsvs .gt_indent_5 {
  text-indent: 25px;
}

#hwkqgmzsvs .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hwkqgmzsvs div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Ordering Emergency Contraception (Restricted to YSC/SRC)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate" class="gt_row gt_center">1.22 (0.71 to 2.09)</td>
<td headers="p.value" class="gt_row gt_center">0.47</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">0.98 (0.95 to 1.01)</td>
<td headers="p.value" class="gt_row gt_center">0.19</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.72</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate" class="gt_row gt_center">0.92 (0.48 to 1.74)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate" class="gt_row gt_center">1.30 (0.64 to 2.63)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.46 (0.50 to 4.31)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">0.55 (0.17 to 1.57)</td>
<td headers="p.value" class="gt_row gt_center">0.27</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate" class="gt_row gt_center">0.99 (0.54 to 1.82)</td>
<td headers="p.value" class="gt_row gt_center">0.98</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

``` r
# HIV Prophylaxis ordered
  reg.ss.hiv <-glm(hiv_pep_kit_ordered_num ~ exposure + age + race_eth_num + ipv + intoxicated, family = "binomial", data = SAFE_Site_Cohort_u72)
  tbl.reg.ss.hiv <- tbl_regression(reg.ss.hiv, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds of Ordering HIV PEP (Restricted to YSC/SRC)")
  tbl.reg.ss.hiv
```

```{=html}
<div id="oplmywsolg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#oplmywsolg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#oplmywsolg thead, #oplmywsolg tbody, #oplmywsolg tfoot, #oplmywsolg tr, #oplmywsolg td, #oplmywsolg th {
  border-style: none;
}

#oplmywsolg p {
  margin: 0;
  padding: 0;
}

#oplmywsolg .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#oplmywsolg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#oplmywsolg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oplmywsolg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oplmywsolg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#oplmywsolg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oplmywsolg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#oplmywsolg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#oplmywsolg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#oplmywsolg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oplmywsolg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oplmywsolg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#oplmywsolg .gt_spanner_row {
  border-bottom-style: hidden;
}

#oplmywsolg .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#oplmywsolg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#oplmywsolg .gt_from_md > :first-child {
  margin-top: 0;
}

#oplmywsolg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oplmywsolg .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#oplmywsolg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#oplmywsolg .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#oplmywsolg .gt_row_group_first td {
  border-top-width: 2px;
}

#oplmywsolg .gt_row_group_first th {
  border-top-width: 2px;
}

#oplmywsolg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#oplmywsolg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#oplmywsolg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#oplmywsolg .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oplmywsolg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#oplmywsolg .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oplmywsolg .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#oplmywsolg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oplmywsolg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oplmywsolg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#oplmywsolg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#oplmywsolg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#oplmywsolg .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#oplmywsolg .gt_left {
  text-align: left;
}

#oplmywsolg .gt_center {
  text-align: center;
}

#oplmywsolg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oplmywsolg .gt_font_normal {
  font-weight: normal;
}

#oplmywsolg .gt_font_bold {
  font-weight: bold;
}

#oplmywsolg .gt_font_italic {
  font-style: italic;
}

#oplmywsolg .gt_super {
  font-size: 65%;
}

#oplmywsolg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#oplmywsolg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#oplmywsolg .gt_indent_1 {
  text-indent: 5px;
}

#oplmywsolg .gt_indent_2 {
  text-indent: 10px;
}

#oplmywsolg .gt_indent_3 {
  text-indent: 15px;
}

#oplmywsolg .gt_indent_4 {
  text-indent: 20px;
}

#oplmywsolg .gt_indent_5 {
  text-indent: 25px;
}

#oplmywsolg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#oplmywsolg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Ordering HIV PEP (Restricted to YSC/SRC)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate" class="gt_row gt_center">9.73 (4.92 to 20.4)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">0.96 (0.93 to 0.99)</td>
<td headers="p.value" class="gt_row gt_center">0.009</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.98</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate" class="gt_row gt_center">1.03 (0.47 to 2.27)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate" class="gt_row gt_center">0.94 (0.39 to 2.21)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.75 (0.17 to 2.94)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">0.43 (0.06 to 1.93)</td>
<td headers="p.value" class="gt_row gt_center">0.29</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate" class="gt_row gt_center">0.86 (0.42 to 1.77)</td>
<td headers="p.value" class="gt_row gt_center">0.69</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

``` r
# Abx ordered
   reg.ss.abx <-glm(abx_given ~ exposure + age + race_eth_num + ipv + intoxicated, family = "binomial", data = SAFE_Site_Cohort)
  tbl.reg.ss.abx <- tbl_regression(reg.ss.abx, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds of Ordering Antibiotics (Restricted to YSC/SRC)")
  tbl.reg.ss.abx
```

```{=html}
<div id="bljrkxczou" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bljrkxczou table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bljrkxczou thead, #bljrkxczou tbody, #bljrkxczou tfoot, #bljrkxczou tr, #bljrkxczou td, #bljrkxczou th {
  border-style: none;
}

#bljrkxczou p {
  margin: 0;
  padding: 0;
}

#bljrkxczou .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bljrkxczou .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bljrkxczou .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bljrkxczou .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bljrkxczou .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bljrkxczou .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bljrkxczou .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bljrkxczou .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bljrkxczou .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bljrkxczou .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bljrkxczou .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bljrkxczou .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bljrkxczou .gt_spanner_row {
  border-bottom-style: hidden;
}

#bljrkxczou .gt_group_heading {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#bljrkxczou .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bljrkxczou .gt_from_md > :first-child {
  margin-top: 0;
}

#bljrkxczou .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bljrkxczou .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bljrkxczou .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#bljrkxczou .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#bljrkxczou .gt_row_group_first td {
  border-top-width: 2px;
}

#bljrkxczou .gt_row_group_first th {
  border-top-width: 2px;
}

#bljrkxczou .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#bljrkxczou .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bljrkxczou .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bljrkxczou .gt_last_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bljrkxczou .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#bljrkxczou .gt_first_grand_summary_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bljrkxczou .gt_last_grand_summary_row_top {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bljrkxczou .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bljrkxczou .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bljrkxczou .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bljrkxczou .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#bljrkxczou .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bljrkxczou .gt_sourcenote {
  font-size: 90%;
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 5px;
  padding-right: 5px;
}

#bljrkxczou .gt_left {
  text-align: left;
}

#bljrkxczou .gt_center {
  text-align: center;
}

#bljrkxczou .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bljrkxczou .gt_font_normal {
  font-weight: normal;
}

#bljrkxczou .gt_font_bold {
  font-weight: bold;
}

#bljrkxczou .gt_font_italic {
  font-style: italic;
}

#bljrkxczou .gt_super {
  font-size: 65%;
}

#bljrkxczou .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bljrkxczou .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bljrkxczou .gt_indent_1 {
  text-indent: 5px;
}

#bljrkxczou .gt_indent_2 {
  text-indent: 10px;
}

#bljrkxczou .gt_indent_3 {
  text-indent: 15px;
}

#bljrkxczou .gt_indent_4 {
  text-indent: 20px;
}

#bljrkxczou .gt_indent_5 {
  text-indent: 25px;
}

#bljrkxczou .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bljrkxczou div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Ordering Antibiotics (Restricted to YSC/SRC)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>OR</strong> <strong>(95% CI)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Exposure</td>
<td headers="estimate" class="gt_row gt_center">1.45 (0.84 to 2.55)</td>
<td headers="p.value" class="gt_row gt_center">0.18</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">0.97 (0.95 to 1.0)</td>
<td headers="p.value" class="gt_row gt_center">0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race and Ethnicity</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.82</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-Hispanic Black</td>
<td headers="estimate" class="gt_row gt_center">1.09 (0.59 to 2.06)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic or Latina/o/x</td>
<td headers="estimate" class="gt_row gt_center">0.94 (0.45 to 2.00)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.77 (0.52 to 8.20)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Assailant was Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">0.52 (0.20 to 1.34)</td>
<td headers="p.value" class="gt_row gt_center">0.17</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Intoxicated</td>
<td headers="estimate" class="gt_row gt_center">0.88 (0.50 to 1.56)</td>
<td headers="p.value" class="gt_row gt_center">0.67</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>OR = Odds Ratio, CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


## Graphs

### Updated CI Graph


``` r
OR_Data<- read_excel("~/Documents/Current_Projects/Sexual_Assault/new_SA_YNHH/Updated YNHH SA code/OR_Table_Data.xlsx") %>% mutate(lnOR = log(OR), lnLCI = log(LCI), lnUCI=log(UCI))
OR_Data <- OR_Data %>%  mutate(Measure = fct_relevel(Measure, "Antibiotics Ordered", "HIV Prophylaxis Ordered", "Emergency Contraception Ordered", "Pregnancy Test Ordered","SAFE Kit Done", "Advocate Offerred" )) 
FinalArrowMsg = "Less likely                        OR                        More likely"
TitleStr <-  paste("Association Between", sep = " ", collapse = NULL)
TitleStr <- paste(TitleStr,'Exposure and ED Outcome Measures', sep = " ", collapse = NULL)
pd <- position_dodge(width = 0.6)
  p <- ggplot(OR_Data, aes(OR, Measure, group = Exposure))
  CI_Log_Graph <- p + geom_point(position = pd, aes(shape=Exposure, color=Exposure)) + geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = Exposure),height = 0.2,position = pd) + labs(x =FinalArrowMsg, y = "ED Outcome Measures",color = "Exposure", shape= "Exposure")+
    theme(
      plot.title = element_text(size=15),
      axis.title.x = element_text(size=13),
      axis.title.y = element_text(size=14),
      plot.caption = element_text(hjust = 0, face= "italic",size=10),
      axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),ends = "both")),
      axis.text.y = element_text(size=12),
      axis.text.x = element_text(size=10),
      legend.text=element_text(size=12),
      legend.title = element_text(size = 14),
      legend.key.height=unit(1, "cm")
    ) +  
    geom_vline(xintercept = 1)  + 
    scale_x_log10() + 
    labs(caption = "Note: Horizontal lines are 95% confidence intervals")+
    ggtitle(TitleStr) + theme(plot.title = element_text(hjust = 0.5))+
    guides(color = guide_legend(reverse=TRUE),shape =guide_legend(reverse=TRUE))
CI_Log_Graph
```

![](index_files/figure-html/updated CI graph-1.png)<!-- -->

``` r
CI_Log_Graph + scale_x_log10(limits = c(0.05, 25), n.breaks = 6)
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

![](index_files/figure-html/updated CI graph-2.png)<!-- -->

``` r
  CI_Graph <- p + geom_point(position = pd,aes(shape=Exposure, color=Exposure)) + geom_errorbarh(aes(xmax = UCI, xmin = LCI, color = Exposure),height = 0.1,position = pd) + labs(x =FinalArrowMsg, y = "ED Outcome Measures",color = "Exposure", shape= "Exposure")+
    theme(
      plot.title = element_text(size=15),
      axis.title.x = element_text(size=12, vjust = -.05),
      axis.title.y = element_text(size=14),
      plot.caption = element_text(hjust = 0, face= "italic",size=10),
      axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),ends = "both")),
      axis.text.y = element_text(size=12),
      axis.text.x = element_text(size=11),
      legend.text=element_text(size=12),
      legend.title = element_text(size = 14),
      legend.key.height=unit(1, "cm")
    ) +  
    geom_vline(xintercept = 1)  + 
    labs(caption = "Note: Horizontal lines are 95% confidence intervals")+
    ggtitle(TitleStr) + theme(plot.title = element_text(hjust = 0.5)) +
    guides(color = guide_legend(reverse=TRUE),shape =guide_legend(reverse=TRUE))

CI_Graph
```

![](index_files/figure-html/updated CI graph-3.png)<!-- -->

CI Graph for Post V. Pre for SAFE Sites (defined as YSC or SRC discharge)


``` r
SAFE_SITE_OR_Data <- read_excel("~/Documents/Current_Projects/Sexual_Assault/new_SA_YNHH/Updated YNHH SA code/SAFE_SITE_OR_Data.xlsx")

ggplot(SAFE_SITE_OR_Data, aes(OR, Measure)) + geom_errorbar(aes(xmax = UCI, xmin = LCI), color = "blue", height = 0.1) + geom_vline(xintercept = 1) + geom_point(shape = "square") + scale_x_log10() + ggtitle("CI Graph for Post V. Pre for SAFE Sites")+ scale_x_log10(limits = c(0.05, 25), n.breaks = 6)
```

```
## Warning in geom_errorbar(aes(xmax = UCI, xmin = LCI), color = "blue", height =
## 0.1): Ignoring unknown parameters: `height`
```

```
## Scale for x is already present.
## Adding another scale for x, which will replace the existing scale.
```

![](index_files/figure-html/CI graph SAFE SITES-1.png)<!-- -->


### Percent pathway, sane kit done and advocate offered over time


``` r
month_line_df <-
  pp.cohort.1 %>% mutate(
    month = ceiling_date(arrive_dt, unit = "month"),
    advocate = case_when(
      advocate_offered == "Advocate Offered" ~ 1,
      advocate_offered == "No Documentation of Pt Advocate" ~ 0,
      .default = NA
    ),
    did_SANE_kit = case_when(
      under120h == TRUE & sane_kit_done == 1 ~ 1,
      under120h == TRUE & sane_kit_done != 1 ~ 0,
      .default = NA
    ),
  ) %>%
  group_by(month) %>% summarise(
    Percent_Advocate = mean(advocate),
    Percent_Pathway = mean(agile_md_used_num, na.rm = FALSE),
    Percent_Sane = mean(did_SANE_kit, na.rm = TRUE)
  ) %>% gather(key = "Metric", value = "Percent", -month) %>% filter(Percent !=0) %>% mutate(Metric = factor(Metric, labels = c("Percent_Advocate" = "% Advocate",  "Percent_Pathway" = "% Pathway", "Percent_Sane" = "% SAFE Kit")))

month_line_df %>% ggplot(aes(x = month, y = Percent, color = Metric)) + geom_point() + geom_path() + scale_y_continuous(labels = scales::percent) + geom_vline(xintercept = pathway_start, color = "black", linetype= 'longdash') + geom_vline(xintercept = sb_start, color= "darkgrey", linetype= 'longdash') + theme_minimal()
```

![](index_files/figure-html/line over time graphs-1.png)<!-- -->

``` r
quarter_line_df <-
  pp.cohort.1 %>% mutate(
    quarter = ceiling_date(arrive_dt, unit = "quarter"),
    advocate = case_when(
      advocate_offered == "Advocate Offered" ~ 1,
      advocate_offered == "No Documentation of Pt Advocate" ~ 0,
      .default = NA
    ),
    did_SANE_kit = case_when(
      under120h == TRUE & sane_kit_done == 1 ~ 1,
      under120h == TRUE & sane_kit_done != 1 ~ 0,
      .default = NA
    )) %>%
  group_by(quarter) %>% summarise(
    Percent_Advocate = mean(advocate),
    Percent_Pathway = mean(agile_md_used_num),
    Percent_Sane = mean(did_SANE_kit, na.rm = TRUE)
  ) %>% gather(key = "Metric", value = "Percent", -quarter) %>% filter(Percent !=0) %>% mutate(Metric = factor(Metric, labels = c("Percent_Advocate" = "% Advocate",  "Percent_Pathway" = "% Pathway", "Percent_Sane" = "% SAFE Kit")))


quarter_percent_plot<- quarter_line_df %>% ggplot(aes(x = quarter, y = Percent, color = Metric)) + geom_point() + geom_path() + theme_minimal() + scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) + geom_vline(xintercept = pathway_start, color = "black", linetype= 'longdash') + geom_vline(xintercept = sb_start, color= "darkgrey", linetype= 'longdash')
quarter_percent_plot + xlab("Quarter")
```

![](index_files/figure-html/line over time graphs-2.png)<!-- -->

### Percent pathway utilization over time


``` r
time_graph_1 <- all_pts %>% mutate(month = ceiling_date(arrive_dt, "month")) %>% ggplot() + geom_bar(mapping = aes(x = month, fill = pathway)) + theme_minimal() + scale_fill_brewer(palette = "Set1") + labs(title = "Patients Per Month", y = "Number of Visits", x= "Month", fill = NULL) + geom_vline(xintercept = pathway_start, color = "black", linetype= 'longdash', alpha = 0.7)
time_graph_1
```

![](index_files/figure-html/visits-over-time-month-1.png)<!-- -->


``` r
time_graph_2 <- all_pts %>% mutate(quarter = ceiling_date(arrive_dt, "quarter")) %>% ggplot() + geom_bar(mapping = aes(x = quarter, fill = pathway)) + theme_minimal() + scale_fill_brewer(palette = "Set1") + labs(title = "Patients Per Quarter", y = "Number of Visits", x= "Quarter", fill = NULL) + geom_vline(xintercept = pathway_start, color = "black", linetype= 'longdash', alpha = 0.7)
time_graph_2
```

![](index_files/figure-html/visits-over-time-quarter-1.png)<!-- -->






