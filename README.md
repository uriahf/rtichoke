
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtichoke

<!-- badges: start -->

[![R-CMD-check](https://github.com/uriahf/rtichoke/workflows/R-CMD-check/badge.svg)](https://github.com/uriahf/rtichoke/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rtichoke)](https://CRAN.R-project.org/package=rtichoke)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/uriahf/rtichoke/branch/main/graph/badge.svg)](https://codecov.io/gh/uriahf/rtichoke?branch=main)
<!-- badges: end -->

The goal of rtichoke is to …

## Installation

<!-- You can install the released version of rtichoke from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("rtichoke") -->
<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uriahf/rtichoke")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rtichoke)

prepare_performance_data(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  real = example_dat$outcome
) %>% 
  head() %>% 
  render_performance_table()
#> Joining, by = c("Model", "key")
```

<div id="cldkrhbguy" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cldkrhbguy .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFBF3;
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

#cldkrhbguy .gt_heading {
  background-color: #FFFBF3;
  text-align: center;
  border-bottom-color: #FFFBF3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cldkrhbguy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFBF3;
  border-bottom-width: 0;
}

#cldkrhbguy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFBF3;
  border-top-width: 0;
}

#cldkrhbguy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cldkrhbguy .gt_col_headings {
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

#cldkrhbguy .gt_col_heading {
  color: #333333;
  background-color: #FFFBF3;
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

#cldkrhbguy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFBF3;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cldkrhbguy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cldkrhbguy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cldkrhbguy .gt_column_spanner {
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

#cldkrhbguy .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFBF3;
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
}

#cldkrhbguy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFBF3;
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

#cldkrhbguy .gt_from_md > :first-child {
  margin-top: 0;
}

#cldkrhbguy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cldkrhbguy .gt_row {
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

#cldkrhbguy .gt_stub {
  color: #333333;
  background-color: #FFFBF3;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#cldkrhbguy .gt_summary_row {
  color: #333333;
  background-color: #FFFBF3;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cldkrhbguy .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#cldkrhbguy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFBF3;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cldkrhbguy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cldkrhbguy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cldkrhbguy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cldkrhbguy .gt_footnotes {
  color: #333333;
  background-color: #FFFBF3;
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

#cldkrhbguy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#cldkrhbguy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFBF3;
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

#cldkrhbguy .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#cldkrhbguy .gt_left {
  text-align: left;
}

#cldkrhbguy .gt_center {
  text-align: center;
}

#cldkrhbguy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cldkrhbguy .gt_font_normal {
  font-weight: normal;
}

#cldkrhbguy .gt_font_bold {
  font-weight: bold;
}

#cldkrhbguy .gt_font_italic {
  font-style: italic;
}

#cldkrhbguy .gt_super {
  font-size: 65%;
}

#cldkrhbguy .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table" style="table-layout: fixed;">
  <colgroup>
    <col/>
    <col/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
  </colgroup>
  <thead class="gt_header">
    <tr>
      <th colspan="13" class="gt_heading gt_title gt_font_normal" style><strong>Performanc Metrics for Different Thresholds</strong></th>
    </tr>
    <tr>
      <th colspan="13" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><strong>First Model</strong> model (Prevalence: 0.33)</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">Model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">Threshold</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner">Confusion Matrix</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6">
        <span class="gt_column_spanner">Performance Metrics</span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">Predicted Positives</th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">TP</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">FP</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">TN</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">FN</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Sens</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Lift</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Spec</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">PPV</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">NPV</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">NB</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">First Model</td>
<td class="gt_row gt_left">0.00</td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">50</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 66.6666666666667%">100</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 55.3333333333333%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 0%">0.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">0.33</span></td>
<td class="gt_row gt_left"></td>
<td class="gt_row gt_center"><span style="display: inline-block;background: linear-gradient(90deg, 
transparent 50%, lightgreen 50%, lightgreen 66.6666666666667%, transparent 66.6666666666667%) center center / 98% 88% no-repeat;
border-radius: 4px; flex: 100 0 auto; width: 100px;">0.33</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgrey; color: black;
width: 100%">150 (100%)</span></td></tr>
    <tr><td class="gt_row gt_left">First Model</td>
<td class="gt_row gt_left">0.01</td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">50</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 32.6666666666667%">49</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 34%">51</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 83.8383838383838%">1.52</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 51%">0.51</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 50.5050505050505%">0.51</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_center"><span style="display: inline-block;background: linear-gradient(90deg, 
transparent 50%, lightgreen 50%, lightgreen 66.5016835016835%, transparent 66.5016835016835%) center center / 98% 88% no-repeat;
border-radius: 4px; flex: 100 0 auto; width: 100px;">0.33</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgrey; color: black;
width: 66%">99 (66%)</span></td></tr>
    <tr><td class="gt_row gt_left">First Model</td>
<td class="gt_row gt_left">0.02</td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">50</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 30%">45</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 36.6666666666667%">55</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 87.3684210526316%">1.58</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 55%">0.55</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 52.6315789473684%">0.53</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_center"><span style="display: inline-block;background: linear-gradient(90deg, 
transparent 50%, lightgreen 50%, lightgreen 66.3605442176871%, transparent 66.3605442176871%) center center / 98% 88% no-repeat;
border-radius: 4px; flex: 100 0 auto; width: 100px;">0.33</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgrey; color: black;
width: 63.3333333333333%">95 (63.3%)</span></td></tr>
    <tr><td class="gt_row gt_left">First Model</td>
<td class="gt_row gt_left">0.03</td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">50</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 26%">39</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 40.6666666666667%">61</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 93.2584269662921%">1.69</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 61%">0.61</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 56.1797752808989%">0.56</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_center"><span style="display: inline-block;background: linear-gradient(90deg, 
transparent 50%, lightgreen 50%, lightgreen 66.2646048109966%, transparent 66.2646048109966%) center center / 98% 88% no-repeat;
border-radius: 4px; flex: 100 0 auto; width: 100px;">0.33</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgrey; color: black;
width: 59.3333333333333%">89 (59.3%)</span></td></tr>
    <tr><td class="gt_row gt_left">First Model</td>
<td class="gt_row gt_left">0.04</td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">50</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 22%">33</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 44.6666666666667%">67</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.81</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 67%">0.67</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 60.2409638554217%">0.60</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_center"><span style="display: inline-block;background: linear-gradient(90deg, 
transparent 50%, lightgreen 50%, lightgreen 66.2083333333333%, transparent 66.2083333333333%) center center / 98% 88% no-repeat;
border-radius: 4px; flex: 100 0 auto; width: 100px;">0.32</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgrey; color: black;
width: 55.3333333333333%">83 (55.3%)</span></td></tr>
    <tr><td class="gt_row gt_left">First Model</td>
<td class="gt_row gt_left">0.05</td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 33.3333333333333%">50</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 22%">33</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 44.6666666666667%">67</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: pink; color: black;
width: 0%">0</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.81</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 67%">0.67</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 60.2409638554217%">0.60</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgreen; color: black;
width: 100%">1.00</span></td>
<td class="gt_row gt_center"><span style="display: inline-block;background: linear-gradient(90deg, 
transparent 50%, lightgreen 50%, lightgreen 66.0877192982456%, transparent 66.0877192982456%) center center / 98% 88% no-repeat;
border-radius: 4px; flex: 100 0 auto; width: 100px;">0.32</span></td>
<td class="gt_row gt_left"><span style="display: inline-block;direction: ltr;
background-color: lightgrey; color: black;
width: 55.3333333333333%">83 (55.3%)</span></td></tr>
  </tbody>
  
  
</table>
</div>

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
