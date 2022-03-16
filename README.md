
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

## Installation

<!-- You can install the released version of rtichoke from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("rtichoke") -->
<!-- ``` -->

You can install rtichoke from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uriahf/rtichoke")
```

<!-- TODO change to good model, bad model and random guess -->

## Overview:

rtichoke is a package designed for interactive visualization for
performance metrics of prediction models with a binary outcome.

## Getting started

### Naming Convention

<div id="jlpoomltxf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jlpoomltxf .gt_table {
  display: table;
  border-collapse: collapse;
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
  border-bottom-width: 3px;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jlpoomltxf .gt_heading {
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

#jlpoomltxf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jlpoomltxf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jlpoomltxf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jlpoomltxf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #FFFFFF;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jlpoomltxf .gt_col_heading {
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

#jlpoomltxf .gt_column_spanner_outer {
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

#jlpoomltxf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jlpoomltxf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jlpoomltxf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jlpoomltxf .gt_group_heading {
  padding: 8px;
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
}

#jlpoomltxf .gt_empty_group_heading {
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

#jlpoomltxf .gt_from_md > :first-child {
  margin-top: 0;
}

#jlpoomltxf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jlpoomltxf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jlpoomltxf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jlpoomltxf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jlpoomltxf .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jlpoomltxf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jlpoomltxf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jlpoomltxf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jlpoomltxf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jlpoomltxf .gt_footnotes {
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

#jlpoomltxf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jlpoomltxf .gt_sourcenotes {
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

#jlpoomltxf .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jlpoomltxf .gt_left {
  text-align: left;
}

#jlpoomltxf .gt_center {
  text-align: center;
}

#jlpoomltxf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jlpoomltxf .gt_font_normal {
  font-weight: normal;
}

#jlpoomltxf .gt_font_bold {
  font-weight: bold;
}

#jlpoomltxf .gt_font_italic {
  font-style: italic;
}

#jlpoomltxf .gt_super {
  font-size: 65%;
}

#jlpoomltxf .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;">Predictions and Outcomes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;">Performance Data</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left gt_stub">Performance Data</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>prepare_performance_data()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'></div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">ROC</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_roc_curve()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>plot_roc_curve()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Lift</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_lift_curve()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>plot_lift_curve()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Gains</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_gains_curve()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>plot_gains_curve()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Precision Recall</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_precision_recall_curve()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>plot_precision_recall_curve()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Decision</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_decision_curve()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>plot_decision_curve()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Calibration</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_calibration_curve()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'></div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Performance Table</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_performance_table()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>render_performance_table()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_left gt_stub">Summary Report</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>create_summary_report()</code></p>
</div></td>
<td class="gt_row gt_left"><div class='gt_from_md'></div></td></tr>
  </tbody>
  
  
</table>
</div>

### Curves based on Performance Metrics

<div id="hwmtrlqyel" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hwmtrlqyel .gt_table {
  display: table;
  border-collapse: collapse;
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
  border-bottom-width: 3px;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hwmtrlqyel .gt_heading {
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

#hwmtrlqyel .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hwmtrlqyel .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hwmtrlqyel .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwmtrlqyel .gt_col_headings {
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #FFFFFF;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hwmtrlqyel .gt_col_heading {
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

#hwmtrlqyel .gt_column_spanner_outer {
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

#hwmtrlqyel .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hwmtrlqyel .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hwmtrlqyel .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hwmtrlqyel .gt_group_heading {
  padding: 8px;
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
}

#hwmtrlqyel .gt_empty_group_heading {
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

#hwmtrlqyel .gt_from_md > :first-child {
  margin-top: 0;
}

#hwmtrlqyel .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hwmtrlqyel .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hwmtrlqyel .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hwmtrlqyel .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwmtrlqyel .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hwmtrlqyel .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwmtrlqyel .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hwmtrlqyel .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hwmtrlqyel .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwmtrlqyel .gt_footnotes {
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

#hwmtrlqyel .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hwmtrlqyel .gt_sourcenotes {
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

#hwmtrlqyel .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hwmtrlqyel .gt_left {
  text-align: left;
}

#hwmtrlqyel .gt_center {
  text-align: center;
}

#hwmtrlqyel .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hwmtrlqyel .gt_font_normal {
  font-weight: normal;
}

#hwmtrlqyel .gt_font_bold {
  font-weight: bold;
}

#hwmtrlqyel .gt_font_italic {
  font-style: italic;
}

#hwmtrlqyel .gt_super {
  font-size: 65%;
}

#hwmtrlqyel .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">Sens</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">Spec</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">PPV</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">PPCR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">Lift</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">NB</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;">P. Thr</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left gt_stub">ROC</td>
<td class="gt_row gt_center">y</td>
<td class="gt_row gt_center">x</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td></tr>
    <tr><td class="gt_row gt_left gt_stub">Lift</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center">x</td>
<td class="gt_row gt_center">y</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td></tr>
    <tr><td class="gt_row gt_left gt_stub">Gains</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center">y</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center">x</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td></tr>
    <tr><td class="gt_row gt_left gt_stub">Precision Recall</td>
<td class="gt_row gt_center">x</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center">y</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td></tr>
    <tr><td class="gt_row gt_left gt_stub">Decision</td>
<td class="gt_row gt_center">x</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center">y</td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center"> </td>
<td class="gt_row gt_center">y</td>
<td class="gt_row gt_center">x</td></tr>
  </tbody>
  
  
</table>
</div>

### Predictions and Outcomes as input

In order to use rtichoke you need to have

-   `probs`: Estimated Probabilities as predictions (0 ≤ *p̂* ≤ 1).
-   `real`: Binary Outcomes (*y* ∈ {1, 0}).

There are 3 different cases and for each one of them rtichoke requires a
different kind of input:

### One model for One Population:

**Why?** In order to examine performance of a single model for one
population.

**How?** The user is required to provide one vector for the model’s
predictions and one vector for the outcome of the population.

<div id="yzesyyichz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yzesyyichz .gt_table {
  display: table;
  border-collapse: collapse;
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

#yzesyyichz .gt_heading {
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

#yzesyyichz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yzesyyichz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yzesyyichz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yzesyyichz .gt_col_headings {
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

#yzesyyichz .gt_col_heading {
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

#yzesyyichz .gt_column_spanner_outer {
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

#yzesyyichz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yzesyyichz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yzesyyichz .gt_column_spanner {
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

#yzesyyichz .gt_group_heading {
  padding: 8px;
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
}

#yzesyyichz .gt_empty_group_heading {
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

#yzesyyichz .gt_from_md > :first-child {
  margin-top: 0;
}

#yzesyyichz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yzesyyichz .gt_row {
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

#yzesyyichz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#yzesyyichz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yzesyyichz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#yzesyyichz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yzesyyichz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yzesyyichz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yzesyyichz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yzesyyichz .gt_footnotes {
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

#yzesyyichz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#yzesyyichz .gt_sourcenotes {
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

#yzesyyichz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#yzesyyichz .gt_left {
  text-align: left;
}

#yzesyyichz .gt_center {
  text-align: center;
}

#yzesyyichz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yzesyyichz .gt_font_normal {
  font-weight: normal;
}

#yzesyyichz .gt_font_bold {
  font-weight: bold;
}

#yzesyyichz .gt_font_italic {
  font-style: italic;
}

#yzesyyichz .gt_super {
  font-size: 65%;
}

#yzesyyichz .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">probs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">real</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center">0.82</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_center">0.48</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_center">0.06</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_center">0.85</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_center">0.34</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_center">0.16</td>
<td class="gt_row gt_center">0</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
create_roc_curve(probs = example_dat$bad_model,
                 real = example_dat$outcome,
                 interactive = TRUE)
```

Alternatively the vector of the model predictions can be in a list:

``` r
create_roc_curve(probs = list("Bad Model" = example_dat$bad_model),
                 real = example_dat$outcome,
                 interactive = TRUE)
```

### Several Models for One Population

**Why?** In order to compare performance for several different models
for the same population.

**How?** The user is required to provide one vector of predictions for
each model in a list and one vector for the outcome of the population.

<div id="oxgdyentzr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#oxgdyentzr .gt_table {
  display: table;
  border-collapse: collapse;
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

#oxgdyentzr .gt_heading {
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

#oxgdyentzr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oxgdyentzr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oxgdyentzr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oxgdyentzr .gt_col_headings {
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

#oxgdyentzr .gt_col_heading {
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

#oxgdyentzr .gt_column_spanner_outer {
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

#oxgdyentzr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oxgdyentzr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oxgdyentzr .gt_column_spanner {
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

#oxgdyentzr .gt_group_heading {
  padding: 8px;
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
}

#oxgdyentzr .gt_empty_group_heading {
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

#oxgdyentzr .gt_from_md > :first-child {
  margin-top: 0;
}

#oxgdyentzr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oxgdyentzr .gt_row {
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

#oxgdyentzr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#oxgdyentzr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oxgdyentzr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#oxgdyentzr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oxgdyentzr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oxgdyentzr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oxgdyentzr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oxgdyentzr .gt_footnotes {
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

#oxgdyentzr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#oxgdyentzr .gt_sourcenotes {
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

#oxgdyentzr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#oxgdyentzr .gt_left {
  text-align: left;
}

#oxgdyentzr .gt_center {
  text-align: center;
}

#oxgdyentzr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oxgdyentzr .gt_font_normal {
  font-weight: normal;
}

#oxgdyentzr .gt_font_bold {
  font-weight: bold;
}

#oxgdyentzr .gt_font_italic {
  font-style: italic;
}

#oxgdyentzr .gt_super {
  font-size: 65%;
}

#oxgdyentzr .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Good Model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Bad Model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Random Guess</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">real</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center">0.60</td>
<td class="gt_row gt_center">0.38</td>
<td class="gt_row gt_center">0.16</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_center">0.24</td>
<td class="gt_row gt_center">0.45</td>
<td class="gt_row gt_center">0.66</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">0.31</td>
<td class="gt_row gt_center">0.59</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_center">0.00</td>
<td class="gt_row gt_center">0.24</td>
<td class="gt_row gt_center">0.48</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_center">0.24</td>
<td class="gt_row gt_center">0.46</td>
<td class="gt_row gt_center">0.68</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_center">0.06</td>
<td class="gt_row gt_center">0.34</td>
<td class="gt_row gt_center">0.63</td>
<td class="gt_row gt_center">0</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
create_roc_curve(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model" = example_dat$bad_model,
    "Random Guess" = example_dat$random_guess
  ),
  real = as.numeric(rtichoke::example_dat$outcome),
  interactive = TRUE
)
```

### Several Populations

*Why?* In order to compare performance for different populations, like
in Train / Test split or in order to check the fairness of the
algorithms.

*How?* The user is required to provide one vector of predictions for
each population in a list and one vector for each outcome of the
population in another list.

<div id="oxgdyentzr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#oxgdyentzr .gt_table {
  display: table;
  border-collapse: collapse;
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

#oxgdyentzr .gt_heading {
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

#oxgdyentzr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oxgdyentzr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oxgdyentzr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oxgdyentzr .gt_col_headings {
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

#oxgdyentzr .gt_col_heading {
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

#oxgdyentzr .gt_column_spanner_outer {
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

#oxgdyentzr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oxgdyentzr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oxgdyentzr .gt_column_spanner {
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

#oxgdyentzr .gt_group_heading {
  padding: 8px;
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
}

#oxgdyentzr .gt_empty_group_heading {
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

#oxgdyentzr .gt_from_md > :first-child {
  margin-top: 0;
}

#oxgdyentzr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oxgdyentzr .gt_row {
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

#oxgdyentzr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#oxgdyentzr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oxgdyentzr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#oxgdyentzr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oxgdyentzr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oxgdyentzr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oxgdyentzr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oxgdyentzr .gt_footnotes {
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

#oxgdyentzr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#oxgdyentzr .gt_sourcenotes {
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

#oxgdyentzr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#oxgdyentzr .gt_left {
  text-align: left;
}

#oxgdyentzr .gt_center {
  text-align: center;
}

#oxgdyentzr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oxgdyentzr .gt_font_normal {
  font-weight: normal;
}

#oxgdyentzr .gt_font_bold {
  font-weight: bold;
}

#oxgdyentzr .gt_font_italic {
  font-style: italic;
}

#oxgdyentzr .gt_super {
  font-size: 65%;
}

#oxgdyentzr .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><strong>Train Set</strong></th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">probs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">real</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.99</td>
<td class="gt_row gt_right">1</td></tr>
    <tr><td class="gt_row gt_right">0.16</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.35</td>
<td class="gt_row gt_right">1</td></tr>
    <tr><td class="gt_row gt_right">0.06</td>
<td class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>
<div id="emetbhczaj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#emetbhczaj .gt_table {
  display: table;
  border-collapse: collapse;
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

#emetbhczaj .gt_heading {
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

#emetbhczaj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#emetbhczaj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#emetbhczaj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#emetbhczaj .gt_col_headings {
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

#emetbhczaj .gt_col_heading {
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

#emetbhczaj .gt_column_spanner_outer {
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

#emetbhczaj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#emetbhczaj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#emetbhczaj .gt_column_spanner {
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

#emetbhczaj .gt_group_heading {
  padding: 8px;
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
}

#emetbhczaj .gt_empty_group_heading {
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

#emetbhczaj .gt_from_md > :first-child {
  margin-top: 0;
}

#emetbhczaj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#emetbhczaj .gt_row {
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

#emetbhczaj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#emetbhczaj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#emetbhczaj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#emetbhczaj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#emetbhczaj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#emetbhczaj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#emetbhczaj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#emetbhczaj .gt_footnotes {
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

#emetbhczaj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#emetbhczaj .gt_sourcenotes {
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

#emetbhczaj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#emetbhczaj .gt_left {
  text-align: left;
}

#emetbhczaj .gt_center {
  text-align: center;
}

#emetbhczaj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#emetbhczaj .gt_font_normal {
  font-weight: normal;
}

#emetbhczaj .gt_font_bold {
  font-weight: bold;
}

#emetbhczaj .gt_font_italic {
  font-style: italic;
}

#emetbhczaj .gt_super {
  font-size: 65%;
}

#emetbhczaj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><strong>Test Set</strong></th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">probs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">real</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.98</td>
<td class="gt_row gt_right">1</td></tr>
    <tr><td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.06</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.24</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
create_roc_curve(
  probs = list(
    "Train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities)
  ),
  real = list(
    "Train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome)
  ),
  interactive = TRUE
)
```

## Performance Data as input

For some outputs in rtichoke you can alternatively prepare a performance
data and use it as an input: instead of `create_*_curve` use
`plot_*_curve` and instead of `create_performance_table` use
`render_performance_table`:

``` r
one_pop_one_model_as_a_vector %>%
  plot_roc_curve(interactive = TRUE)
```

### Summary Report

In order to get all the supported outputs of rtichoke in one html file
the user can call `create_summary_report()`.

### Getting help

If you encounter a bug please fill an issue with a minimal reproducible
example, it will be easier for me to help you and it might help others
in the future. Alternatively you are welcome to contact me personally:
<ufinkel@gmail.com>
