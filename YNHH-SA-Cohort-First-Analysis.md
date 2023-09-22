YNHH Cohort First Analysis
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
    ## 1 Seen earlier                            4
    ## 2 Patient reports not being assaulted     9
    ## 3 Psych                                   1
    ## 4 Eloped                                  1

``` r
n_excluded <-n_distinct(excluded_patients$pat_enc_csn_id, na.rm=TRUE) %>% as.character()
cat("-", n_excluded, "patients were excluded based on chart review.")
```

    ## - 15 patients were excluded based on chart review.

``` r
rm(n_excluded)
```

<div id="spsrtzxlem" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#spsrtzxlem table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#spsrtzxlem thead, #spsrtzxlem tbody, #spsrtzxlem tfoot, #spsrtzxlem tr, #spsrtzxlem td, #spsrtzxlem th {
  border-style: none;
}
&#10;#spsrtzxlem p {
  margin: 0;
  padding: 0;
}
&#10;#spsrtzxlem .gt_table {
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
&#10;#spsrtzxlem .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#spsrtzxlem .gt_title {
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
&#10;#spsrtzxlem .gt_subtitle {
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
&#10;#spsrtzxlem .gt_heading {
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
&#10;#spsrtzxlem .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#spsrtzxlem .gt_col_headings {
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
&#10;#spsrtzxlem .gt_col_heading {
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
&#10;#spsrtzxlem .gt_column_spanner_outer {
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
&#10;#spsrtzxlem .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#spsrtzxlem .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#spsrtzxlem .gt_column_spanner {
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
&#10;#spsrtzxlem .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#spsrtzxlem .gt_group_heading {
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
&#10;#spsrtzxlem .gt_empty_group_heading {
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
&#10;#spsrtzxlem .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#spsrtzxlem .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#spsrtzxlem .gt_row {
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
&#10;#spsrtzxlem .gt_stub {
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
&#10;#spsrtzxlem .gt_stub_row_group {
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
&#10;#spsrtzxlem .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#spsrtzxlem .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#spsrtzxlem .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#spsrtzxlem .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#spsrtzxlem .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#spsrtzxlem .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#spsrtzxlem .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#spsrtzxlem .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#spsrtzxlem .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#spsrtzxlem .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#spsrtzxlem .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#spsrtzxlem .gt_footnotes {
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
&#10;#spsrtzxlem .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#spsrtzxlem .gt_sourcenotes {
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
&#10;#spsrtzxlem .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#spsrtzxlem .gt_left {
  text-align: left;
}
&#10;#spsrtzxlem .gt_center {
  text-align: center;
}
&#10;#spsrtzxlem .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#spsrtzxlem .gt_font_normal {
  font-weight: normal;
}
&#10;#spsrtzxlem .gt_font_bold {
  font-weight: bold;
}
&#10;#spsrtzxlem .gt_font_italic {
  font-style: italic;
}
&#10;#spsrtzxlem .gt_super {
  font-size: 65%;
}
&#10;#spsrtzxlem .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#spsrtzxlem .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#spsrtzxlem .gt_indent_1 {
  text-indent: 5px;
}
&#10;#spsrtzxlem .gt_indent_2 {
  text-indent: 10px;
}
&#10;#spsrtzxlem .gt_indent_3 {
  text-indent: 15px;
}
&#10;#spsrtzxlem .gt_indent_4 {
  text-indent: 20px;
}
&#10;#spsrtzxlem .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Overall&lt;/strong&gt;, N = 265&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Overall</strong>, N = 265<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Didn’t Use Pathway&lt;/strong&gt;, N = 137&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Didn’t Use Pathway</strong>, N = 137<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Used Pathway&lt;/strong&gt;, N = 128&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Used Pathway</strong>, N = 128<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_0" class="gt_row gt_center">27 (22, 39)</td>
<td headers="stat_1" class="gt_row gt_center">29 (22, 40)</td>
<td headers="stat_2" class="gt_row gt_center">27 (22, 35)</td>
<td headers="p.value" class="gt_row gt_center">0.2<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age Group</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.071<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18 - 55</td>
<td headers="stat_0" class="gt_row gt_center">247 (93%)</td>
<td headers="stat_1" class="gt_row gt_center">124 (91%)</td>
<td headers="stat_2" class="gt_row gt_center">123 (96%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55+</td>
<td headers="stat_0" class="gt_row gt_center">18 (6.8%)</td>
<td headers="stat_1" class="gt_row gt_center">13 (9.5%)</td>
<td headers="stat_2" class="gt_row gt_center">5 (3.9%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Female</td>
<td headers="stat_0" class="gt_row gt_center">248 (94%)</td>
<td headers="stat_1" class="gt_row gt_center">131 (96%)</td>
<td headers="stat_2" class="gt_row gt_center">117 (91%)</td>
<td headers="p.value" class="gt_row gt_center">0.2<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.14<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="stat_0" class="gt_row gt_center">62 (23%)</td>
<td headers="stat_1" class="gt_row gt_center">34 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">28 (22%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_0" class="gt_row gt_center">51 (19%)</td>
<td headers="stat_1" class="gt_row gt_center">20 (15%)</td>
<td headers="stat_2" class="gt_row gt_center">31 (24%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="stat_0" class="gt_row gt_center">152 (57%)</td>
<td headers="stat_1" class="gt_row gt_center">83 (61%)</td>
<td headers="stat_2" class="gt_row gt_center">69 (54%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">contacted_advocate</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Advocate Contacted</td>
<td headers="stat_0" class="gt_row gt_center">129 (58%)</td>
<td headers="stat_1" class="gt_row gt_center">55 (46%)</td>
<td headers="stat_2" class="gt_row gt_center">74 (73%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No Documentation of Pt Advocate</td>
<td headers="stat_0" class="gt_row gt_center">92 (42%)</td>
<td headers="stat_1" class="gt_row gt_center">65 (54%)</td>
<td headers="stat_2" class="gt_row gt_center">27 (27%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">44</td>
<td headers="stat_1" class="gt_row gt_center">17</td>
<td headers="stat_2" class="gt_row gt_center">27</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">sane_kit_YN</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>4</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">9 (4.1%)</td>
<td headers="stat_1" class="gt_row gt_center">8 (6.6%)</td>
<td headers="stat_2" class="gt_row gt_center">1 (1.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Note done due to concurrent psychiatric concerns</td>
<td headers="stat_0" class="gt_row gt_center">9 (4.1%)</td>
<td headers="stat_1" class="gt_row gt_center">9 (7.4%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Offered, but declined</td>
<td headers="stat_0" class="gt_row gt_center">57 (26%)</td>
<td headers="stat_1" class="gt_row gt_center">34 (28%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (23%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outside 120 hr Window</td>
<td headers="stat_0" class="gt_row gt_center">20 (9.0%)</td>
<td headers="stat_1" class="gt_row gt_center">13 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">7 (6.9%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_center">127 (57%)</td>
<td headers="stat_1" class="gt_row gt_center">57 (47%)</td>
<td headers="stat_2" class="gt_row gt_center">70 (69%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">43</td>
<td headers="stat_1" class="gt_row gt_center">16</td>
<td headers="stat_2" class="gt_row gt_center">27</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">sane_kit_missed</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.037<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>4</sup></span></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sane kit done/ Sane kit not indicated</td>
<td headers="stat_0" class="gt_row gt_center">204 (96%)</td>
<td headers="stat_1" class="gt_row gt_center">104 (93%)</td>
<td headers="stat_2" class="gt_row gt_center">100 (99%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sane kit not done/documented</td>
<td headers="stat_0" class="gt_row gt_center">9 (4.2%)</td>
<td headers="stat_1" class="gt_row gt_center">8 (7.1%)</td>
<td headers="stat_2" class="gt_row gt_center">1 (1.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">52</td>
<td headers="stat_1" class="gt_row gt_center">25</td>
<td headers="stat_2" class="gt_row gt_center">27</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); n (%)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> Wilcoxon rank sum test</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span> Pearson’s Chi-squared test</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>4</sup></span> Fisher’s exact test</td>
    </tr>
  </tfoot>
</table>
</div>
