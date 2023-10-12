YNHH Cohort ACEP presentaiton Analysis
================

## creating cohorts

\###First creation of cohort by removing:  
*-patients before story board start date 2021-09-08*  
*-minors*  
*-excluded patients*

``` r
cohort.1 <- all_pts %>% filter(ed_arrival_date > sb_start) %>% filter(age > 17)
excluded_patients <- cohort.1  %>% filter(!is.na(exclude))
cohort.2 <- cohort.1  %>% filter(is.na(exclude))
excluded_patients <- excluded_patients %>% add_value_labels(reason_to_exclude = c( "Seen earlier" = 1 , "Patient reports not being assaulted" = 2, "Psych" = 3, "not excluded" = 4, "Eloped" = 5)) %>% to_factor()
excluded_patients %>% group_by(reason_to_exclude) %>% summarise(n=n())
```

    ## # A tibble: 4 × 2
    ##   reason_to_exclude                       n
    ##   <fct>                               <int>
    ## 1 Seen earlier                            5
    ## 2 Patient reports not being assaulted    10
    ## 3 Psych                                   1
    ## 4 Eloped                                  2

``` r
n_excluded <-n_distinct(excluded_patients$pat_enc_csn_id, na.rm=TRUE) %>% as.character()
cat("-", n_excluded, "patients were excluded based on chart review.")
```

    ## - 18 patients were excluded based on chart review.

``` r
number_of_minors <-all_pts %>% filter(ed_arrival_date > sb_start) %>% filter(age < 18)
number_of_minors <- n_distinct(number_of_minors$pat_enc_csn_id, na.rm = TRUE) %>% as.character()
cat("  ", number_of_minors, "patients under 18 YO excluded")
```

    ##    105 patients under 18 YO excluded

``` r
rm(n_excluded, number_of_minors)
```

<div id="eohwcgzdiz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#eohwcgzdiz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#eohwcgzdiz thead, #eohwcgzdiz tbody, #eohwcgzdiz tfoot, #eohwcgzdiz tr, #eohwcgzdiz td, #eohwcgzdiz th {
  border-style: none;
}
&#10;#eohwcgzdiz p {
  margin: 0;
  padding: 0;
}
&#10;#eohwcgzdiz .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#eohwcgzdiz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#eohwcgzdiz .gt_title {
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
&#10;#eohwcgzdiz .gt_subtitle {
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
&#10;#eohwcgzdiz .gt_heading {
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
&#10;#eohwcgzdiz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#eohwcgzdiz .gt_col_headings {
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
&#10;#eohwcgzdiz .gt_col_heading {
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
&#10;#eohwcgzdiz .gt_column_spanner_outer {
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
&#10;#eohwcgzdiz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#eohwcgzdiz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#eohwcgzdiz .gt_column_spanner {
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
&#10;#eohwcgzdiz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#eohwcgzdiz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#eohwcgzdiz .gt_empty_group_heading {
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
&#10;#eohwcgzdiz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#eohwcgzdiz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#eohwcgzdiz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#eohwcgzdiz .gt_stub {
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
&#10;#eohwcgzdiz .gt_stub_row_group {
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
&#10;#eohwcgzdiz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#eohwcgzdiz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#eohwcgzdiz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eohwcgzdiz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#eohwcgzdiz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#eohwcgzdiz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#eohwcgzdiz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eohwcgzdiz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#eohwcgzdiz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#eohwcgzdiz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#eohwcgzdiz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#eohwcgzdiz .gt_footnotes {
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
&#10;#eohwcgzdiz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eohwcgzdiz .gt_sourcenotes {
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
&#10;#eohwcgzdiz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eohwcgzdiz .gt_left {
  text-align: left;
}
&#10;#eohwcgzdiz .gt_center {
  text-align: center;
}
&#10;#eohwcgzdiz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#eohwcgzdiz .gt_font_normal {
  font-weight: normal;
}
&#10;#eohwcgzdiz .gt_font_bold {
  font-weight: bold;
}
&#10;#eohwcgzdiz .gt_font_italic {
  font-style: italic;
}
&#10;#eohwcgzdiz .gt_super {
  font-size: 65%;
}
&#10;#eohwcgzdiz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#eohwcgzdiz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#eohwcgzdiz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#eohwcgzdiz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#eohwcgzdiz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#eohwcgzdiz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#eohwcgzdiz .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Didn’t Use Pathway&lt;/strong&gt;, N = 134&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Didn’t Use Pathway</strong>, N = 134<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Used Pathway&lt;/strong&gt;, N = 128&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Used Pathway</strong>, N = 128<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">29 (22, 40)</td>
<td headers="stat_2" class="gt_row gt_center">27 (22, 35)</td>
<td headers="p.value" class="gt_row gt_center">0.2<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female</td>
<td headers="stat_1" class="gt_row gt_center">128 (96%)</td>
<td headers="stat_2" class="gt_row gt_center">117 (91%)</td>
<td headers="p.value" class="gt_row gt_center">0.2<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.12<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="stat_1" class="gt_row gt_center">33 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">28 (22%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_1" class="gt_row gt_center">19 (14%)</td>
<td headers="stat_2" class="gt_row gt_center">31 (24%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="stat_1" class="gt_row gt_center">82 (61%)</td>
<td headers="stat_2" class="gt_row gt_center">69 (54%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Presented within 120 of assault</td>
<td headers="stat_1" class="gt_row gt_center">112 (85%)</td>
<td headers="stat_2" class="gt_row gt_center">120 (94%)</td>
<td headers="p.value" class="gt_row gt_center">0.030<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">3</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">English speaking</td>
<td headers="stat_1" class="gt_row gt_center">126 (96%)</td>
<td headers="stat_2" class="gt_row gt_center">121 (95%)</td>
<td headers="p.value" class="gt_row gt_center">0.5<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">3</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Arrived by EMS</td>
<td headers="stat_1" class="gt_row gt_center">49 (37%)</td>
<td headers="stat_2" class="gt_row gt_center">52 (41%)</td>
<td headers="p.value" class="gt_row gt_center">0.5<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Insurance</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.006<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>4</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medicaid</td>
<td headers="stat_1" class="gt_row gt_center">24 (18%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (13%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medicare</td>
<td headers="stat_1" class="gt_row gt_center">10 (7.5%)</td>
<td headers="stat_2" class="gt_row gt_center">3 (2.3%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private Insurance</td>
<td headers="stat_1" class="gt_row gt_center">8 (6.0%)</td>
<td headers="stat_2" class="gt_row gt_center">6 (4.7%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sexual Assault</td>
<td headers="stat_1" class="gt_row gt_center">85 (63%)</td>
<td headers="stat_2" class="gt_row gt_center">102 (80%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Uninsured/Self-Pay</td>
<td headers="stat_1" class="gt_row gt_center">7 (5.2%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Relation of assailant to patient</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.10<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>4</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">26 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">12 (9.4%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Aqauintance</td>
<td headers="stat_1" class="gt_row gt_center">72 (54%)</td>
<td headers="stat_2" class="gt_row gt_center">80 (63%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Family member</td>
<td headers="stat_1" class="gt_row gt_center">2 (1.5%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Stranger</td>
<td headers="stat_1" class="gt_row gt_center">26 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">29 (23%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Intimate Partner</td>
<td headers="stat_1" class="gt_row gt_center">7 (5.3%)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5.5%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); n (%)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> Wilcoxon rank sum test</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span> Pearson’s Chi-squared test</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>4</sup></span> Fisher’s exact test</td>
    </tr>
  </tfoot>
</table>
</div>
<div id="piadhhxbop" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#piadhhxbop table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#piadhhxbop thead, #piadhhxbop tbody, #piadhhxbop tfoot, #piadhhxbop tr, #piadhhxbop td, #piadhhxbop th {
  border-style: none;
}
&#10;#piadhhxbop p {
  margin: 0;
  padding: 0;
}
&#10;#piadhhxbop .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#piadhhxbop .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#piadhhxbop .gt_title {
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
&#10;#piadhhxbop .gt_subtitle {
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
&#10;#piadhhxbop .gt_heading {
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
&#10;#piadhhxbop .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#piadhhxbop .gt_col_headings {
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
&#10;#piadhhxbop .gt_col_heading {
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
&#10;#piadhhxbop .gt_column_spanner_outer {
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
&#10;#piadhhxbop .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#piadhhxbop .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#piadhhxbop .gt_column_spanner {
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
&#10;#piadhhxbop .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#piadhhxbop .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#piadhhxbop .gt_empty_group_heading {
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
&#10;#piadhhxbop .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#piadhhxbop .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#piadhhxbop .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#piadhhxbop .gt_stub {
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
&#10;#piadhhxbop .gt_stub_row_group {
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
&#10;#piadhhxbop .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#piadhhxbop .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#piadhhxbop .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#piadhhxbop .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#piadhhxbop .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#piadhhxbop .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#piadhhxbop .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#piadhhxbop .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#piadhhxbop .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#piadhhxbop .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#piadhhxbop .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#piadhhxbop .gt_footnotes {
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
&#10;#piadhhxbop .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#piadhhxbop .gt_sourcenotes {
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
&#10;#piadhhxbop .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#piadhhxbop .gt_left {
  text-align: left;
}
&#10;#piadhhxbop .gt_center {
  text-align: center;
}
&#10;#piadhhxbop .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#piadhhxbop .gt_font_normal {
  font-weight: normal;
}
&#10;#piadhhxbop .gt_font_bold {
  font-weight: bold;
}
&#10;#piadhhxbop .gt_font_italic {
  font-style: italic;
}
&#10;#piadhhxbop .gt_super {
  font-size: 65%;
}
&#10;#piadhhxbop .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#piadhhxbop .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#piadhhxbop .gt_indent_1 {
  text-indent: 5px;
}
&#10;#piadhhxbop .gt_indent_2 {
  text-indent: 10px;
}
&#10;#piadhhxbop .gt_indent_3 {
  text-indent: 15px;
}
&#10;#piadhhxbop .gt_indent_4 {
  text-indent: 20px;
}
&#10;#piadhhxbop .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Didn’t Use Pathway&lt;/strong&gt;, N = 134&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Didn’t Use Pathway</strong>, N = 134<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Used Pathway&lt;/strong&gt;, N = 128&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Used Pathway</strong>, N = 128<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Documentation of advocate contacted</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Advocate Contacted</td>
<td headers="stat_1" class="gt_row gt_center">62 (46%)</td>
<td headers="stat_2" class="gt_row gt_center">98 (77%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No Documentation of Pt Advocate</td>
<td headers="stat_1" class="gt_row gt_center">72 (54%)</td>
<td headers="stat_2" class="gt_row gt_center">30 (23%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Received any antibiotics</td>
<td headers="stat_1" class="gt_row gt_center">76 (57%)</td>
<td headers="stat_2" class="gt_row gt_center">108 (84%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Recieved CDC reccomended STI prophalaxis</td>
<td headers="stat_1" class="gt_row gt_center">48</td>
<td headers="stat_2" class="gt_row gt_center">88</td></tr>
    <tr><td headers="label" class="gt_row gt_left">HIV PEP ordered</td>
<td headers="stat_1" class="gt_row gt_center">15 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">70 (55%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Had indicated labs drawn if given HIV PEP</td>
<td headers="stat_1" class="gt_row gt_center">7</td>
<td headers="stat_2" class="gt_row gt_center">57</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female under 55</td>
<td headers="stat_1" class="gt_row gt_center">116 (87%)</td>
<td headers="stat_2" class="gt_row gt_center">112 (88%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pregnancy test ordered</td>
<td headers="stat_1" class="gt_row gt_center">81</td>
<td headers="stat_2" class="gt_row gt_center">97</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Pregnancy prophalaxis ordered</td>
<td headers="stat_1" class="gt_row gt_center">38</td>
<td headers="stat_2" class="gt_row gt_center">64</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SANE kit done</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">8 (6.0%)</td>
<td headers="stat_2" class="gt_row gt_center">1 (0.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not done due to concurrent psychiatric concerns</td>
<td headers="stat_1" class="gt_row gt_center">9 (6.7%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Offered, but declined</td>
<td headers="stat_1" class="gt_row gt_center">41 (31%)</td>
<td headers="stat_2" class="gt_row gt_center">25 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outside 120 hr Window</td>
<td headers="stat_1" class="gt_row gt_center">15 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">61 (46%)</td>
<td headers="stat_2" class="gt_row gt_center">95 (74%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Under 120 hours and SANE kit not done</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not done due to concurrent psychiatric concerns</td>
<td headers="stat_1" class="gt_row gt_center">9 (6.7%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    SANE kit done/not indicated</td>
<td headers="stat_1" class="gt_row gt_center">117 (87%)</td>
<td headers="stat_2" class="gt_row gt_center">127 (99%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    SANE kit not done/documented</td>
<td headers="stat_1" class="gt_row gt_center">8 (6.0%)</td>
<td headers="stat_2" class="gt_row gt_center">1 (0.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Has after discharge follow up</td>
<td headers="stat_1" class="gt_row gt_center">127 (95%)</td>
<td headers="stat_2" class="gt_row gt_center">124 (97%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); n</td>
    </tr>
  </tfoot>
</table>
</div>

# Primary outcome- ADVOCATE CONTACTED

``` r
cohort.2$race_bwo <- as_factor(cohort.2$race_bwo)
cohort.2$race_bwo <- relevel(cohort.2$race_bwo, ref= "White")
cohort.2$contacted_advocate <- (as_factor(cohort.2$contacted_advocate))
cohort.2$contacted_advocate <- relevel(cohort.2$contacted_advocate, ref= "No Documentation of Pt Advocate")
model.primary <-  glm(contacted_advocate ~ pathway + age + female + race_bwo + insurance_3 + english,  family = "binomial", data = cohort.2)
primary.results <- tbl_regression(model.primary, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header( "Odds of documentation of offering call to PT advocate") 
summary(model.primary)
```

    ## 
    ## Call:
    ## glm(formula = contacted_advocate ~ pathway + age + female + race_bwo + 
    ##     insurance_3 + english, family = "binomial", data = cohort.2)
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.42330    1.05717   0.400   0.6889    
    ## pathwayUsed Pathway   1.22496    0.28930   4.234 2.29e-05 ***
    ## age                  -0.02177    0.01112  -1.957   0.0504 .  
    ## female                0.92161    0.57054   1.615   0.1062    
    ## race_bwoOther         0.80157    0.43485   1.843   0.0653 .  
    ## race_bwoBlack         0.20654    0.33467   0.617   0.5371    
    ## insurance_3Private   -1.43200    0.63634  -2.250   0.0244 *  
    ## insurance_3Uninsured -1.95038    1.21960  -1.599   0.1098    
    ## english              -0.77471    0.81142  -0.955   0.3397    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 345.49  on 258  degrees of freedom
    ## Residual deviance: 301.43  on 250  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 319.43
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
primary.results
```

<div id="svggdbjybo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#svggdbjybo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#svggdbjybo thead, #svggdbjybo tbody, #svggdbjybo tfoot, #svggdbjybo tr, #svggdbjybo td, #svggdbjybo th {
  border-style: none;
}
&#10;#svggdbjybo p {
  margin: 0;
  padding: 0;
}
&#10;#svggdbjybo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#svggdbjybo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#svggdbjybo .gt_title {
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
&#10;#svggdbjybo .gt_subtitle {
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
&#10;#svggdbjybo .gt_heading {
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
&#10;#svggdbjybo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#svggdbjybo .gt_col_headings {
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
&#10;#svggdbjybo .gt_col_heading {
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
&#10;#svggdbjybo .gt_column_spanner_outer {
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
&#10;#svggdbjybo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#svggdbjybo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#svggdbjybo .gt_column_spanner {
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
&#10;#svggdbjybo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#svggdbjybo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#svggdbjybo .gt_empty_group_heading {
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
&#10;#svggdbjybo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#svggdbjybo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#svggdbjybo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#svggdbjybo .gt_stub {
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
&#10;#svggdbjybo .gt_stub_row_group {
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
&#10;#svggdbjybo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#svggdbjybo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#svggdbjybo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#svggdbjybo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#svggdbjybo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#svggdbjybo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#svggdbjybo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#svggdbjybo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#svggdbjybo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#svggdbjybo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#svggdbjybo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#svggdbjybo .gt_footnotes {
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
&#10;#svggdbjybo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#svggdbjybo .gt_sourcenotes {
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
&#10;#svggdbjybo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#svggdbjybo .gt_left {
  text-align: left;
}
&#10;#svggdbjybo .gt_center {
  text-align: center;
}
&#10;#svggdbjybo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#svggdbjybo .gt_font_normal {
  font-weight: normal;
}
&#10;#svggdbjybo .gt_font_bold {
  font-weight: bold;
}
&#10;#svggdbjybo .gt_font_italic {
  font-style: italic;
}
&#10;#svggdbjybo .gt_super {
  font-size: 65%;
}
&#10;#svggdbjybo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#svggdbjybo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#svggdbjybo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#svggdbjybo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#svggdbjybo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#svggdbjybo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#svggdbjybo .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of documentation of offering call to PT advocate</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">3.40</td>
<td headers="ci" class="gt_row gt_center">1.95, 6.07</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">0.98</td>
<td headers="ci" class="gt_row gt_center">0.96, 1.00</td>
<td headers="p.value" class="gt_row gt_center">0.048</td></tr>
    <tr><td headers="label" class="gt_row gt_left">female</td>
<td headers="estimate" class="gt_row gt_center">2.51</td>
<td headers="ci" class="gt_row gt_center">0.81, 7.86</td>
<td headers="p.value" class="gt_row gt_center">0.11</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">2.23</td>
<td headers="ci" class="gt_row gt_center">0.98, 5.46</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">1.23</td>
<td headers="ci" class="gt_row gt_center">0.64, 2.39</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">insurance_3</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private</td>
<td headers="estimate" class="gt_row gt_center">0.24</td>
<td headers="ci" class="gt_row gt_center">0.06, 0.80</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Uninsured</td>
<td headers="estimate" class="gt_row gt_center">0.14</td>
<td headers="ci" class="gt_row gt_center">0.01, 1.15</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">english</td>
<td headers="estimate" class="gt_row gt_center">0.46</td>
<td headers="ci" class="gt_row gt_center">0.08, 2.07</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

## Primary outcome, sub-outcome- ADVOCATE CONTACTED *IF* SANE KIT DONE

``` r
cohort.2$race_bwo <- as_factor(cohort.2$race_bwo)
cohort.2$race_bwo <- relevel(cohort.2$race_bwo, ref= "White")
cohort.2$contacted_advocate <- (as_factor(cohort.2$contacted_advocate))
cohort.2$contacted_advocate <- relevel(cohort.2$contacted_advocate, ref= "No Documentation of Pt Advocate")
cohort.2.2 <- cohort.2 %>% filter(sane_kit_done == 1) #156 patients
model.primary.2 <-  glm(contacted_advocate ~ pathway + age + female + race_bwo + insurance_3 + english,  family = "binomial", data = cohort.2.2)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
primary.results.2 <- tbl_regression(model.primary, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header( "Odds of documentation of offering call to PT advocate") 
summary(model.primary.2)
```

    ## 
    ## Call:
    ## glm(formula = contacted_advocate ~ pathway + age + female + race_bwo + 
    ##     insurance_3 + english, family = "binomial", data = cohort.2.2)
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)           1.629e+01  1.368e+03   0.012   0.9905  
    ## pathwayUsed Pathway   1.110e+00  4.407e-01   2.518   0.0118 *
    ## age                   7.515e-03  1.965e-02   0.382   0.7021  
    ## female               -1.125e-01  1.158e+00  -0.097   0.9226  
    ## race_bwoOther         6.726e-01  6.147e-01   1.094   0.2738  
    ## race_bwoBlack         8.133e-01  6.066e-01   1.341   0.1800  
    ## insurance_3Private    1.499e+01  2.109e+03   0.007   0.9943  
    ## insurance_3Uninsured  5.626e-01  4.186e+03   0.000   0.9999  
    ## english              -1.584e+01  1.368e+03  -0.012   0.9908  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 146.43  on 154  degrees of freedom
    ## Residual deviance: 132.52  on 146  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 150.52
    ## 
    ## Number of Fisher Scoring iterations: 16

``` r
primary.results.2
```

<div id="lkqsmsqfbo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lkqsmsqfbo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lkqsmsqfbo thead, #lkqsmsqfbo tbody, #lkqsmsqfbo tfoot, #lkqsmsqfbo tr, #lkqsmsqfbo td, #lkqsmsqfbo th {
  border-style: none;
}
&#10;#lkqsmsqfbo p {
  margin: 0;
  padding: 0;
}
&#10;#lkqsmsqfbo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#lkqsmsqfbo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lkqsmsqfbo .gt_title {
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
&#10;#lkqsmsqfbo .gt_subtitle {
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
&#10;#lkqsmsqfbo .gt_heading {
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
&#10;#lkqsmsqfbo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lkqsmsqfbo .gt_col_headings {
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
&#10;#lkqsmsqfbo .gt_col_heading {
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
&#10;#lkqsmsqfbo .gt_column_spanner_outer {
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
&#10;#lkqsmsqfbo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lkqsmsqfbo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lkqsmsqfbo .gt_column_spanner {
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
&#10;#lkqsmsqfbo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lkqsmsqfbo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#lkqsmsqfbo .gt_empty_group_heading {
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
&#10;#lkqsmsqfbo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lkqsmsqfbo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lkqsmsqfbo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#lkqsmsqfbo .gt_stub {
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
&#10;#lkqsmsqfbo .gt_stub_row_group {
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
&#10;#lkqsmsqfbo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lkqsmsqfbo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lkqsmsqfbo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lkqsmsqfbo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lkqsmsqfbo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lkqsmsqfbo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lkqsmsqfbo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lkqsmsqfbo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lkqsmsqfbo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lkqsmsqfbo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lkqsmsqfbo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lkqsmsqfbo .gt_footnotes {
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
&#10;#lkqsmsqfbo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lkqsmsqfbo .gt_sourcenotes {
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
&#10;#lkqsmsqfbo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lkqsmsqfbo .gt_left {
  text-align: left;
}
&#10;#lkqsmsqfbo .gt_center {
  text-align: center;
}
&#10;#lkqsmsqfbo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lkqsmsqfbo .gt_font_normal {
  font-weight: normal;
}
&#10;#lkqsmsqfbo .gt_font_bold {
  font-weight: bold;
}
&#10;#lkqsmsqfbo .gt_font_italic {
  font-style: italic;
}
&#10;#lkqsmsqfbo .gt_super {
  font-size: 65%;
}
&#10;#lkqsmsqfbo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lkqsmsqfbo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lkqsmsqfbo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lkqsmsqfbo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lkqsmsqfbo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lkqsmsqfbo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lkqsmsqfbo .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of documentation of offering call to PT advocate</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">3.40</td>
<td headers="ci" class="gt_row gt_center">1.95, 6.07</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">0.98</td>
<td headers="ci" class="gt_row gt_center">0.96, 1.00</td>
<td headers="p.value" class="gt_row gt_center">0.048</td></tr>
    <tr><td headers="label" class="gt_row gt_left">female</td>
<td headers="estimate" class="gt_row gt_center">2.51</td>
<td headers="ci" class="gt_row gt_center">0.81, 7.86</td>
<td headers="p.value" class="gt_row gt_center">0.11</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">2.23</td>
<td headers="ci" class="gt_row gt_center">0.98, 5.46</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">1.23</td>
<td headers="ci" class="gt_row gt_center">0.64, 2.39</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">insurance_3</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private</td>
<td headers="estimate" class="gt_row gt_center">0.24</td>
<td headers="ci" class="gt_row gt_center">0.06, 0.80</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Uninsured</td>
<td headers="estimate" class="gt_row gt_center">0.14</td>
<td headers="ci" class="gt_row gt_center">0.01, 1.15</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">english</td>
<td headers="estimate" class="gt_row gt_center">0.46</td>
<td headers="ci" class="gt_row gt_center">0.08, 2.07</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

# Secondary Outcome: SANE Kit Done/ offered if eligible

restricted to patients presenting within 120 hours *this regression has
issues with some predictors due to small sample size. Controlling for
sex leads to problem since all 12 men had SANE kit offered. controlling
for english vs other language also has problem as all 11 non-English
speakers had sane kit offered to them. same for all 2 patients that were
assualted by family members (all offerred kit)* *we will need to
consider which confoudners we control for/ if we care about the effect
estimates for confounding factors*

``` r
#removing patients where they presented > 120 hours into stay OR did not qualify for sane kit because of concurrent psych issues.
cohort.3 <- cohort.2 %>% filter(under120h == TRUE, sane_kit_missed != "Not done due to concurrent psychiatric concerns") %>% 
  mutate(sane_kit_offered = if_else(sane_kit_done == 0, "No", "Offered"))
cohort.3$race_bwo <- as_factor(cohort.3$race_bwo)
cohort.3$race_bwo <- relevel(cohort.3$race_bwo, ref= "White")
cohort.3$sane_kit_offered <- as_factor(cohort.3$sane_kit_offered)
cohort.3$sane_kit_offered <- relevel(cohort.3$sane_kit_offered, ref = "No")
cohort.3$relationship_4_with_pt <- as_factor(cohort.3$relationship_4_with_pt)
cohort.3$relationship_4_with_pt <- relevel(cohort.3$relationship_4_with_pt, ref = "Stranger/Unknown")
model.secondary.1 <-  glm(sane_kit_offered ~ pathway + age + race_bwo + insurance_3,  family = "binomial", data = cohort.3)
secondary.results.1 <- tbl_regression(model.secondary.1, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header( "Odds of Offering SANE Kit if Eligible") 
summary(model.secondary.1)
```

    ## 
    ## Call:
    ## glm(formula = sane_kit_offered ~ pathway + age + race_bwo + insurance_3, 
    ##     family = "binomial", data = cohort.3)
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           4.60387    1.38889   3.315 0.000917 ***
    ## pathwayUsed Pathway   1.71740    1.13157   1.518 0.129086    
    ## age                  -0.02377    0.03116  -0.763 0.445668    
    ## race_bwoOther         0.09267    1.25067   0.074 0.940931    
    ## race_bwoBlack        -1.29348    0.91680  -1.411 0.158283    
    ## insurance_3Private   -2.76648    1.05302  -2.627 0.008609 ** 
    ## insurance_3Uninsured -2.16878    1.34954  -1.607 0.108042    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 62.363  on 224  degrees of freedom
    ## Residual deviance: 49.254  on 218  degrees of freedom
    ## AIC: 63.254
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
secondary.results.1
```

<div id="jbdzuzpduh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jbdzuzpduh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jbdzuzpduh thead, #jbdzuzpduh tbody, #jbdzuzpduh tfoot, #jbdzuzpduh tr, #jbdzuzpduh td, #jbdzuzpduh th {
  border-style: none;
}
&#10;#jbdzuzpduh p {
  margin: 0;
  padding: 0;
}
&#10;#jbdzuzpduh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#jbdzuzpduh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jbdzuzpduh .gt_title {
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
&#10;#jbdzuzpduh .gt_subtitle {
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
&#10;#jbdzuzpduh .gt_heading {
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
&#10;#jbdzuzpduh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jbdzuzpduh .gt_col_headings {
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
&#10;#jbdzuzpduh .gt_col_heading {
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
&#10;#jbdzuzpduh .gt_column_spanner_outer {
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
&#10;#jbdzuzpduh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jbdzuzpduh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jbdzuzpduh .gt_column_spanner {
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
&#10;#jbdzuzpduh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jbdzuzpduh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#jbdzuzpduh .gt_empty_group_heading {
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
&#10;#jbdzuzpduh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jbdzuzpduh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jbdzuzpduh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#jbdzuzpduh .gt_stub {
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
&#10;#jbdzuzpduh .gt_stub_row_group {
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
&#10;#jbdzuzpduh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jbdzuzpduh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jbdzuzpduh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jbdzuzpduh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jbdzuzpduh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jbdzuzpduh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jbdzuzpduh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jbdzuzpduh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jbdzuzpduh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jbdzuzpduh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jbdzuzpduh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jbdzuzpduh .gt_footnotes {
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
&#10;#jbdzuzpduh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jbdzuzpduh .gt_sourcenotes {
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
&#10;#jbdzuzpduh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jbdzuzpduh .gt_left {
  text-align: left;
}
&#10;#jbdzuzpduh .gt_center {
  text-align: center;
}
&#10;#jbdzuzpduh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jbdzuzpduh .gt_font_normal {
  font-weight: normal;
}
&#10;#jbdzuzpduh .gt_font_bold {
  font-weight: bold;
}
&#10;#jbdzuzpduh .gt_font_italic {
  font-style: italic;
}
&#10;#jbdzuzpduh .gt_super {
  font-size: 65%;
}
&#10;#jbdzuzpduh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jbdzuzpduh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jbdzuzpduh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jbdzuzpduh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jbdzuzpduh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jbdzuzpduh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jbdzuzpduh .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds of Offering SANE Kit if Eligible</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.083</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">5.57</td>
<td headers="ci" class="gt_row gt_center">0.82, 111</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">0.98</td>
<td headers="ci" class="gt_row gt_center">0.92, 1.05</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.10</td>
<td headers="ci" class="gt_row gt_center">0.11, 24.9</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">0.27</td>
<td headers="ci" class="gt_row gt_center">0.04, 1.71</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">insurance_3</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.032</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Public</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Private</td>
<td headers="estimate" class="gt_row gt_center">0.06</td>
<td headers="ci" class="gt_row gt_center">0.01, 0.58</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Uninsured</td>
<td headers="estimate" class="gt_row gt_center">0.11</td>
<td headers="ci" class="gt_row gt_center">0.01, 2.84</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

Pregnancy related outcomes *note- used 120 hour as threshold for
eligibility for EC since ulipristol can be used up to 120 hours*

``` r
tertiary.cohort <- cohort.2 %>%  filter(female_u55 == 1)
quatrinary.cohort <- tertiary.cohort %>% filter(under120h = TRUE)
model.tertiary <-  glm(pregnancy_test_ordered_num ~ pathway + age + race_bwo + english + ambulance, family = "binomial", data = tertiary.cohort)
summary(model.tertiary)
```

    ## 
    ## Call:
    ## glm(formula = pregnancy_test_ordered_num ~ pathway + age + race_bwo + 
    ##     english + ambulance, family = "binomial", data = tertiary.cohort)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)          2.63555    1.25652   2.097  0.03595 * 
    ## pathwayUsed Pathway  0.91561    0.34747   2.635  0.00841 **
    ## age                 -0.03752    0.01829  -2.051  0.04027 * 
    ## race_bwoOther        0.92110    0.58296   1.580  0.11410   
    ## race_bwoBlack        0.51027    0.40543   1.259  0.20818   
    ## english             -0.88085    1.11569  -0.790  0.42981   
    ## ambulance           -0.15369    0.35153  -0.437  0.66195   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 241.36  on 225  degrees of freedom
    ## Residual deviance: 221.03  on 219  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 235.03
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
tertiary.results <- tbl_regression(model.tertiary, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Secondary Outcome (did women under 55 get preg test?)")
tertiary.results
```

<div id="rgjtovrrye" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rgjtovrrye table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rgjtovrrye thead, #rgjtovrrye tbody, #rgjtovrrye tfoot, #rgjtovrrye tr, #rgjtovrrye td, #rgjtovrrye th {
  border-style: none;
}
&#10;#rgjtovrrye p {
  margin: 0;
  padding: 0;
}
&#10;#rgjtovrrye .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#rgjtovrrye .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rgjtovrrye .gt_title {
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
&#10;#rgjtovrrye .gt_subtitle {
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
&#10;#rgjtovrrye .gt_heading {
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
&#10;#rgjtovrrye .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rgjtovrrye .gt_col_headings {
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
&#10;#rgjtovrrye .gt_col_heading {
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
&#10;#rgjtovrrye .gt_column_spanner_outer {
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
&#10;#rgjtovrrye .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rgjtovrrye .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rgjtovrrye .gt_column_spanner {
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
&#10;#rgjtovrrye .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rgjtovrrye .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#rgjtovrrye .gt_empty_group_heading {
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
&#10;#rgjtovrrye .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rgjtovrrye .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rgjtovrrye .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#rgjtovrrye .gt_stub {
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
&#10;#rgjtovrrye .gt_stub_row_group {
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
&#10;#rgjtovrrye .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rgjtovrrye .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rgjtovrrye .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rgjtovrrye .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rgjtovrrye .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rgjtovrrye .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rgjtovrrye .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rgjtovrrye .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rgjtovrrye .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rgjtovrrye .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rgjtovrrye .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rgjtovrrye .gt_footnotes {
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
&#10;#rgjtovrrye .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rgjtovrrye .gt_sourcenotes {
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
&#10;#rgjtovrrye .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rgjtovrrye .gt_left {
  text-align: left;
}
&#10;#rgjtovrrye .gt_center {
  text-align: center;
}
&#10;#rgjtovrrye .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rgjtovrrye .gt_font_normal {
  font-weight: normal;
}
&#10;#rgjtovrrye .gt_font_bold {
  font-weight: bold;
}
&#10;#rgjtovrrye .gt_font_italic {
  font-style: italic;
}
&#10;#rgjtovrrye .gt_super {
  font-size: 65%;
}
&#10;#rgjtovrrye .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rgjtovrrye .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rgjtovrrye .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rgjtovrrye .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rgjtovrrye .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rgjtovrrye .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rgjtovrrye .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Secondary Outcome (did women under 55 get preg test?)</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.007</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">2.50</td>
<td headers="ci" class="gt_row gt_center">1.28, 5.04</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">0.96</td>
<td headers="ci" class="gt_row gt_center">0.93, 1.00</td>
<td headers="p.value" class="gt_row gt_center">0.040</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.15</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">2.51</td>
<td headers="ci" class="gt_row gt_center">0.88, 9.11</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">1.67</td>
<td headers="ci" class="gt_row gt_center">0.77, 3.83</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">english</td>
<td headers="estimate" class="gt_row gt_center">0.41</td>
<td headers="ci" class="gt_row gt_center">0.02, 2.64</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ambulance</td>
<td headers="estimate" class="gt_row gt_center">0.86</td>
<td headers="ci" class="gt_row gt_center">0.43, 1.72</td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
model.quatro <-  glm(prevent.preg ~ pathway + age + race_bwo +  english + ambulance, family = "binomial", data = quatrinary.cohort)
summary(model.quatro)
```

    ## 
    ## Call:
    ## glm(formula = prevent.preg ~ pathway + age + race_bwo + english + 
    ##     ambulance, family = "binomial", data = quatrinary.cohort)
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          0.906563   0.891174   1.017 0.309026    
    ## pathwayUsed Pathway  1.018903   0.283823   3.590 0.000331 ***
    ## age                 -0.008492   0.016067  -0.529 0.597127    
    ## race_bwoOther       -0.212694   0.393847  -0.540 0.589169    
    ## race_bwoBlack        0.139215   0.337357   0.413 0.679852    
    ## english             -1.413313   0.731115  -1.933 0.053225 .  
    ## ambulance           -0.149143   0.297312  -0.502 0.615923    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 310.30  on 225  degrees of freedom
    ## Residual deviance: 292.36  on 219  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 306.36
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
quatro.results <- tbl_regression(model.quatro, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Secondary Outcome (did women under 55 have pregnancy prevention ordered?)")
quatro.results
```

<div id="pamdktbgkw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pamdktbgkw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#pamdktbgkw thead, #pamdktbgkw tbody, #pamdktbgkw tfoot, #pamdktbgkw tr, #pamdktbgkw td, #pamdktbgkw th {
  border-style: none;
}
&#10;#pamdktbgkw p {
  margin: 0;
  padding: 0;
}
&#10;#pamdktbgkw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#pamdktbgkw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#pamdktbgkw .gt_title {
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
&#10;#pamdktbgkw .gt_subtitle {
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
&#10;#pamdktbgkw .gt_heading {
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
&#10;#pamdktbgkw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pamdktbgkw .gt_col_headings {
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
&#10;#pamdktbgkw .gt_col_heading {
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
&#10;#pamdktbgkw .gt_column_spanner_outer {
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
&#10;#pamdktbgkw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#pamdktbgkw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#pamdktbgkw .gt_column_spanner {
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
&#10;#pamdktbgkw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#pamdktbgkw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#pamdktbgkw .gt_empty_group_heading {
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
&#10;#pamdktbgkw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#pamdktbgkw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#pamdktbgkw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#pamdktbgkw .gt_stub {
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
&#10;#pamdktbgkw .gt_stub_row_group {
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
&#10;#pamdktbgkw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#pamdktbgkw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#pamdktbgkw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pamdktbgkw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#pamdktbgkw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#pamdktbgkw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pamdktbgkw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pamdktbgkw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#pamdktbgkw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#pamdktbgkw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#pamdktbgkw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pamdktbgkw .gt_footnotes {
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
&#10;#pamdktbgkw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pamdktbgkw .gt_sourcenotes {
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
&#10;#pamdktbgkw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pamdktbgkw .gt_left {
  text-align: left;
}
&#10;#pamdktbgkw .gt_center {
  text-align: center;
}
&#10;#pamdktbgkw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#pamdktbgkw .gt_font_normal {
  font-weight: normal;
}
&#10;#pamdktbgkw .gt_font_bold {
  font-weight: bold;
}
&#10;#pamdktbgkw .gt_font_italic {
  font-style: italic;
}
&#10;#pamdktbgkw .gt_super {
  font-size: 65%;
}
&#10;#pamdktbgkw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#pamdktbgkw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#pamdktbgkw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#pamdktbgkw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#pamdktbgkw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#pamdktbgkw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#pamdktbgkw .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Secondary Outcome (did women under 55 have pregnancy prevention ordered?)</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">2.77</td>
<td headers="ci" class="gt_row gt_center">1.60, 4.87</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">0.99</td>
<td headers="ci" class="gt_row gt_center">0.96, 1.02</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.7</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.81</td>
<td headers="ci" class="gt_row gt_center">0.37, 1.74</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">1.15</td>
<td headers="ci" class="gt_row gt_center">0.59, 2.23</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">english</td>
<td headers="estimate" class="gt_row gt_center">0.24</td>
<td headers="ci" class="gt_row gt_center">0.05, 0.94</td>
<td headers="p.value" class="gt_row gt_center">0.041</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ambulance</td>
<td headers="estimate" class="gt_row gt_center">0.86</td>
<td headers="ci" class="gt_row gt_center">0.48, 1.54</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

# Secondary Outcome: Correct STI tx if given *any* antibiotics

Meaning: Women got Metronidizole + CTX + Azithromycin or Doxy only one
male patient received incorrect abx for STI treatment causing 0 cell
problem so had to restrict to female patients

``` r
#need to first restrict to patients who got any antibiotics
abx.cohort <- cohort.2 %>% filter(abx_given == 1) # 184 pts
model.sti.tx <- glm(tbl_sti_tx ~ pathway + age + race_bwo + english + relationship_4_with_pt, family = "binomial", data = abx.cohort)
summary(model.sti.tx)
```

    ## 
    ## Call:
    ## glm(formula = tbl_sti_tx ~ pathway + age + race_bwo + english + 
    ##     relationship_4_with_pt, family = "binomial", data = abx.cohort)
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)                            -1.29473    1.03049  -1.256   0.2090  
    ## pathwayUsed Pathway                     0.86144    0.36642   2.351   0.0187 *
    ## age                                     0.03535    0.01966   1.798   0.0721 .
    ## race_bwoOther                           0.43012    0.55569   0.774   0.4389  
    ## race_bwoBlack                          -0.99355    0.41080  -2.419   0.0156 *
    ## english                                 1.16008    0.76089   1.525   0.1273  
    ## relationship_4_with_ptFamily           -1.10250    1.46607  -0.752   0.4520  
    ## relationship_4_with_ptIntimate Partner  0.56296    1.14383   0.492   0.6226  
    ## relationship_4_with_ptStranger/Unknown -0.01791    0.39853  -0.045   0.9642  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 205.20  on 180  degrees of freedom
    ## Residual deviance: 186.56  on 172  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 204.56
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
sti.model.results <- tbl_regression(model.sti.tx, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Odds that Correct STI Tx was given if pt received ABX")
sti.model.results
```

<div id="ztpfhmwiom" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ztpfhmwiom table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ztpfhmwiom thead, #ztpfhmwiom tbody, #ztpfhmwiom tfoot, #ztpfhmwiom tr, #ztpfhmwiom td, #ztpfhmwiom th {
  border-style: none;
}
&#10;#ztpfhmwiom p {
  margin: 0;
  padding: 0;
}
&#10;#ztpfhmwiom .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#ztpfhmwiom .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ztpfhmwiom .gt_title {
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
&#10;#ztpfhmwiom .gt_subtitle {
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
&#10;#ztpfhmwiom .gt_heading {
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
&#10;#ztpfhmwiom .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztpfhmwiom .gt_col_headings {
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
&#10;#ztpfhmwiom .gt_col_heading {
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
&#10;#ztpfhmwiom .gt_column_spanner_outer {
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
&#10;#ztpfhmwiom .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ztpfhmwiom .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ztpfhmwiom .gt_column_spanner {
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
&#10;#ztpfhmwiom .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ztpfhmwiom .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#ztpfhmwiom .gt_empty_group_heading {
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
&#10;#ztpfhmwiom .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ztpfhmwiom .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ztpfhmwiom .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#ztpfhmwiom .gt_stub {
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
&#10;#ztpfhmwiom .gt_stub_row_group {
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
&#10;#ztpfhmwiom .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ztpfhmwiom .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ztpfhmwiom .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztpfhmwiom .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ztpfhmwiom .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ztpfhmwiom .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztpfhmwiom .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztpfhmwiom .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ztpfhmwiom .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztpfhmwiom .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ztpfhmwiom .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztpfhmwiom .gt_footnotes {
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
&#10;#ztpfhmwiom .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztpfhmwiom .gt_sourcenotes {
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
&#10;#ztpfhmwiom .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztpfhmwiom .gt_left {
  text-align: left;
}
&#10;#ztpfhmwiom .gt_center {
  text-align: center;
}
&#10;#ztpfhmwiom .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ztpfhmwiom .gt_font_normal {
  font-weight: normal;
}
&#10;#ztpfhmwiom .gt_font_bold {
  font-weight: bold;
}
&#10;#ztpfhmwiom .gt_font_italic {
  font-style: italic;
}
&#10;#ztpfhmwiom .gt_super {
  font-size: 65%;
}
&#10;#ztpfhmwiom .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ztpfhmwiom .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ztpfhmwiom .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ztpfhmwiom .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ztpfhmwiom .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ztpfhmwiom .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ztpfhmwiom .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Odds that Correct STI Tx was given if pt received ABX</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.018</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">2.37</td>
<td headers="ci" class="gt_row gt_center">1.16, 4.90</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">1.04</td>
<td headers="ci" class="gt_row gt_center">1.00, 1.08</td>
<td headers="p.value" class="gt_row gt_center">0.059</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">1.54</td>
<td headers="ci" class="gt_row gt_center">0.54, 4.94</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">0.37</td>
<td headers="ci" class="gt_row gt_center">0.16, 0.83</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">english</td>
<td headers="estimate" class="gt_row gt_center">3.19</td>
<td headers="ci" class="gt_row gt_center">0.68, 14.5</td>
<td headers="p.value" class="gt_row gt_center">0.14</td></tr>
    <tr><td headers="label" class="gt_row gt_left">relationship_4_with_pt</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.8</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Aquaintaince</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Family</td>
<td headers="estimate" class="gt_row gt_center">0.33</td>
<td headers="ci" class="gt_row gt_center">0.01, 8.98</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Intimate Partner</td>
<td headers="estimate" class="gt_row gt_center">1.76</td>
<td headers="ci" class="gt_row gt_center">0.25, 35.4</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Stranger/Unknown</td>
<td headers="estimate" class="gt_row gt_center">0.98</td>
<td headers="ci" class="gt_row gt_center">0.45, 2.17</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

# Secondary Outcome: Correct labs ordered if patient given PEP

restricted to those presenting within 72 hours

``` r
cohort.u72 <- cohort.2 %>% filter(time_since_sexual_assault_if_number_k_is_y == 0) #restricting to those <72 hours. 206 patients. 
cohort.u72.pep <- cohort.u72 %>% filter(hiv_pep_kit_ordered_num ==1) #72 patients only
model.hiv.labs <-  glm(hiv_labs ~ pathway + age + female + race_bwo, family = "binomial", data = cohort.u72.pep)
summary(model.hiv.labs)
```

    ## 
    ## Call:
    ## glm(formula = hiv_labs ~ pathway + age + female + race_bwo, family = "binomial", 
    ##     data = cohort.u72.pep)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)         -1.17714    1.87265  -0.629  0.52961   
    ## pathwayUsed Pathway  2.14640    0.77877   2.756  0.00585 **
    ## age                  0.02536    0.03183   0.797  0.42555   
    ## female               0.89853    1.30748   0.687  0.49194   
    ## race_bwoOther       -1.19306    0.83167  -1.435  0.15142   
    ## race_bwoBlack       -1.74145    0.73340  -2.375  0.01757 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 83.207  on 75  degrees of freedom
    ## Residual deviance: 69.016  on 70  degrees of freedom
    ## AIC: 81.016
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
hiv.labs.results <- tbl_regression(model.hiv.labs, exponentiate = TRUE) %>% add_global_p() %>% as_gt() %>% tab_header("Secondary Outcome (did patients who got HIV PEP have LFTs ordered?)")
hiv.labs.results
```

<div id="hpbyyumnss" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hpbyyumnss table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#hpbyyumnss thead, #hpbyyumnss tbody, #hpbyyumnss tfoot, #hpbyyumnss tr, #hpbyyumnss td, #hpbyyumnss th {
  border-style: none;
}
&#10;#hpbyyumnss p {
  margin: 0;
  padding: 0;
}
&#10;#hpbyyumnss .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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
&#10;#hpbyyumnss .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#hpbyyumnss .gt_title {
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
&#10;#hpbyyumnss .gt_subtitle {
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
&#10;#hpbyyumnss .gt_heading {
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
&#10;#hpbyyumnss .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hpbyyumnss .gt_col_headings {
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
&#10;#hpbyyumnss .gt_col_heading {
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
&#10;#hpbyyumnss .gt_column_spanner_outer {
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
&#10;#hpbyyumnss .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#hpbyyumnss .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#hpbyyumnss .gt_column_spanner {
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
&#10;#hpbyyumnss .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#hpbyyumnss .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#hpbyyumnss .gt_empty_group_heading {
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
&#10;#hpbyyumnss .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#hpbyyumnss .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#hpbyyumnss .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
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
&#10;#hpbyyumnss .gt_stub {
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
&#10;#hpbyyumnss .gt_stub_row_group {
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
&#10;#hpbyyumnss .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#hpbyyumnss .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#hpbyyumnss .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hpbyyumnss .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#hpbyyumnss .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#hpbyyumnss .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hpbyyumnss .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hpbyyumnss .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#hpbyyumnss .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#hpbyyumnss .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#hpbyyumnss .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hpbyyumnss .gt_footnotes {
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
&#10;#hpbyyumnss .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hpbyyumnss .gt_sourcenotes {
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
&#10;#hpbyyumnss .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hpbyyumnss .gt_left {
  text-align: left;
}
&#10;#hpbyyumnss .gt_center {
  text-align: center;
}
&#10;#hpbyyumnss .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#hpbyyumnss .gt_font_normal {
  font-weight: normal;
}
&#10;#hpbyyumnss .gt_font_bold {
  font-weight: bold;
}
&#10;#hpbyyumnss .gt_font_italic {
  font-style: italic;
}
&#10;#hpbyyumnss .gt_super {
  font-size: 65%;
}
&#10;#hpbyyumnss .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#hpbyyumnss .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#hpbyyumnss .gt_indent_1 {
  text-indent: 5px;
}
&#10;#hpbyyumnss .gt_indent_2 {
  text-indent: 10px;
}
&#10;#hpbyyumnss .gt_indent_3 {
  text-indent: 15px;
}
&#10;#hpbyyumnss .gt_indent_4 {
  text-indent: 20px;
}
&#10;#hpbyyumnss .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Secondary Outcome (did patients who got HIV PEP have LFTs ordered?)</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">pathway</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Didn't Use Pathway</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Used Pathway</td>
<td headers="estimate" class="gt_row gt_center">8.55</td>
<td headers="ci" class="gt_row gt_center">1.96, 44.3</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">1.03</td>
<td headers="ci" class="gt_row gt_center">0.97, 1.10</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">female</td>
<td headers="estimate" class="gt_row gt_center">2.46</td>
<td headers="ci" class="gt_row gt_center">0.10, 27.3</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">race_bwo</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.038</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="estimate" class="gt_row gt_center">0.30</td>
<td headers="ci" class="gt_row gt_center">0.06, 1.58</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="estimate" class="gt_row gt_center">0.18</td>
<td headers="ci" class="gt_row gt_center">0.04, 0.69</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>
