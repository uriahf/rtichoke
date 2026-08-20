# Create Table for AUC

Create Table for AUC

## Usage

``` r
create_table_for_auc(
  probs,
  reals,
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123")
)
```

## Arguments

- probs:

  a list of vectors of estimated probabilities (one for each model or
  one for each population)

- reals:

  a list of vectors of binary outcomes (one for each population)

- color_values:

  color palette

## Examples

``` r

rtichoke:::create_table_for_auc(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)
#> Setting levels: control = FALSE, case = TRUE
#> Setting direction: controls < cases

{"x":{"tag":{"name":"Reactable","attribs":{"data":{"population":["Model 1"],"auc":[0.9586]},"columns":[{"id":"population","name":"Model","type":"factor","show":false,"cell":[{"name":"Fragment","attribs":[],"children":[{"name":"span","attribs":{"style":{"display":"inline-block","marginRight":"8px","width":"9px","height":"9px","backgroundColor":"#1b9e77","borderRadius":"50%"}},"children":[]},"Model 1"]}],"minWidth":300},{"id":"auc","name":"AUROC","type":"numeric","cell":[{"name":"div","attribs":{"style":{"display":"flex","alignItems":"center"}},"children":["0.96",{"name":"div","attribs":{"style":{"flexGrow":1,"marginLeft":"8px","background":"#e1e1e1"}},"children":[{"name":"div","attribs":{"style":{"background":"green","width":"95.86%","height":"16px"}},"children":[]}]}]}],"minWidth":300,"align":"left"}],"sortable":false,"inline":true,"dataKey":"773ea409e056024a275861d642e39335"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}

rtichoke:::create_table_for_auc(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)
#> Setting levels: control = FALSE, case = TRUE
#> Setting direction: controls < cases
#> Setting levels: control = FALSE, case = TRUE
#> Setting direction: controls > cases

{"x":{"tag":{"name":"Reactable","attribs":{"data":{"population":["First Model","Second Model"],"auc":[0.9586,0.5354]},"columns":[{"id":"population","name":"Model","type":"factor","show":true,"cell":[{"name":"Fragment","attribs":[],"children":[{"name":"span","attribs":{"style":{"display":"inline-block","marginRight":"8px","width":"9px","height":"9px","backgroundColor":"#1b9e77","borderRadius":"50%"}},"children":[]},"First Model"]},{"name":"Fragment","attribs":[],"children":[{"name":"span","attribs":{"style":{"display":"inline-block","marginRight":"8px","width":"9px","height":"9px","backgroundColor":"#d95f02","borderRadius":"50%"}},"children":[]},"Second Model"]}],"minWidth":300},{"id":"auc","name":"AUROC","type":"numeric","cell":[{"name":"div","attribs":{"style":{"display":"flex","alignItems":"center"}},"children":["0.96",{"name":"div","attribs":{"style":{"flexGrow":1,"marginLeft":"8px","background":"#e1e1e1"}},"children":[{"name":"div","attribs":{"style":{"background":"green","width":"95.86%","height":"16px"}},"children":[]}]}]},{"name":"div","attribs":{"style":{"display":"flex","alignItems":"center"}},"children":["0.54",{"name":"div","attribs":{"style":{"flexGrow":1,"marginLeft":"8px","background":"#e1e1e1"}},"children":[{"name":"div","attribs":{"style":{"background":"green","width":"53.54%","height":"16px"}},"children":[]}]}]}],"minWidth":300,"align":"left"}],"sortable":false,"inline":true,"dataKey":"6ead96aba78fee73432f65b04e3b39a1"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}

rtichoke:::create_table_for_auc(
  probs = list(
    "train" = example_dat |>
      dplyr::filter(type_of_set == "train") |>
      dplyr::pull(estimated_probabilities),
    "test" = example_dat |>
      dplyr::filter(type_of_set == "test") |>
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat |>
      dplyr::filter(type_of_set == "train") |>
      dplyr::pull(outcome),
    "test" = example_dat |>
      dplyr::filter(type_of_set == "test") |>
      dplyr::pull(outcome)
  )
)
#> Setting levels: control = FALSE, case = TRUE
#> Setting direction: controls < cases
#> Setting levels: control = FALSE, case = TRUE
#> Setting direction: controls < cases

{"x":{"tag":{"name":"Reactable","attribs":{"data":{"population":["train","test"],"auc":[0.962447478991597,0.948863636363636]},"columns":[{"id":"population","name":"Population","type":"factor","show":true,"cell":[{"name":"Fragment","attribs":[],"children":[{"name":"span","attribs":{"style":{"display":"inline-block","marginRight":"8px","width":"9px","height":"9px","backgroundColor":"#1b9e77","borderRadius":"50%"}},"children":[]},"train"]},{"name":"Fragment","attribs":[],"children":[{"name":"span","attribs":{"style":{"display":"inline-block","marginRight":"8px","width":"9px","height":"9px","backgroundColor":"#d95f02","borderRadius":"50%"}},"children":[]},"test"]}],"minWidth":300},{"id":"auc","name":"AUROC","type":"numeric","cell":[{"name":"div","attribs":{"style":{"display":"flex","alignItems":"center"}},"children":["0.96",{"name":"div","attribs":{"style":{"flexGrow":1,"marginLeft":"8px","background":"#e1e1e1"}},"children":[{"name":"div","attribs":{"style":{"background":"green","width":"96.2447478991597%","height":"16px"}},"children":[]}]}]},{"name":"div","attribs":{"style":{"display":"flex","alignItems":"center"}},"children":["0.95",{"name":"div","attribs":{"style":{"flexGrow":1,"marginLeft":"8px","background":"#e1e1e1"}},"children":[{"name":"div","attribs":{"style":{"background":"green","width":"94.8863636363636%","height":"16px"}},"children":[]}]}]}],"minWidth":300,"align":"left"}],"sortable":false,"inline":true,"dataKey":"c6c5f313cc8b936f4859fcdd545fd7f5"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}
```
