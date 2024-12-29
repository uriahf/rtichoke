confusion_matrix_header_maker <- function() {
  paste('function(column) {
    
    if ( column.column.id === "predicted_positives" ) {

        
        return `<input type="checkbox" id="predicted_positives_checkbox" checked = true 
        onclick="(function() { 

        
        ', onclick_reactable_setMeta(
          highlightedMetrics = list(
            checked = list("TP" =  "true", "TN" = "false", "FP" = "true", "FN" = "false"),
            nonchecked = list("TP" =  "false", "TN" = "prevMeta", "FP" = "false", "FN" = "prevMeta")
          ), checkboxId = "\'predicted_positives_checkbox\'") , '
        console.log(1)
        })()"
        >
Predicted Positives`
      } else if ( column.column.id === "predicted_negatives" ) {
    return `<input type="checkbox" id="predicted_negatives_checkbox" checked = true 
        onclick="(function() { 
        
        ', onclick_reactable_setMeta(
          highlightedMetrics = list(
            checked = list("TP" =  "false", "TN" = "true", "FP" = "false", "FN" = "true"),
            nonchecked = list("TP" =  "prevMeta", "TN" = "false", "FP" = "prevMeta", "FN" = "false")
          ), checkboxId = "\'predicted_negatives_checkbox\'"), '
        console.log(1)
        })()"
        >
Predicted Negatives` } else if ( column.column.id === "type" ) {
    
        return `<input type="checkbox" id="all_observations_checkbox" checked = true 
        onclick="(function() { 
        ', onclick_reactable_setMeta(
          highlightedMetrics = list(
            checked = list("TP" =  "true", "TN" = "true", "FP" = "true", "FN" = "true"),
            nonchecked = list("TP" =  "false", "TN" = "false", "FP" = "false", "FN" = "false")
          ), checkboxId = "\'all_observations_checkbox\'"), '
        
        console.log(1)
        })()"
        >
<br>All<br>Observations` }
      }')
}


paste_highlighted_metric_text <- function(
    highlighted_metric, highlighted_metric_name) {
  
  if ( highlighted_metric %in% c("!prevMeta", "prevMeta") ) {
    
    paste(
      highlighted_metric_name,
      paste(
        highlighted_metric, 
        "highlightedMetrics", highlighted_metric_name, sep = "."), 
      sep = ": ")    
    
    
  } else if ( highlighted_metric %in% c("true", "false") ) {
    
    paste(
      highlighted_metric_name,
      highlighted_metric, 
      sep = ": ") 
    
  }
}


onclick_reactable_setMeta <- function(highlightedMetrics = list(
  checked = list(
    "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
  ), nonchecked = list(
    "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
  )), checkboxId = "\'predicted_positives_checkbox\'") {
  
  paste(
    "Reactable.setMeta(\'cars-colors-table\', function(prevMeta) {
    
    const checkboxIds = [
        'tn-checkbox', 'fn-checkbox',
        'tp-checkbox', 'fp-checkbox',
        'predicted_positives_checkbox', 'predicted_negatives_checkbox',
        'real_positives_checkbox', 'real_negatives_checkbox'
    ]

        
        console.log(prevMeta)
        
        var checkBox = document.getElementById(",checkboxId,")
        
        
        if ( checkBox.id === \'predicted_positives_checkbox\' ) {
        
        var checkBoxTP = document.getElementById(\'tp-checkbox\')
        var checkBoxFP = document.getElementById(\'fp-checkbox\')
        var checkBoxRP = document.getElementById(\'real_positives_checkbox\')
        var checkBoxRN = document.getElementById(\'real_negatives_checkbox\')
        var checkBoxN = document.getElementById(\'all_observations_checkbox\')

        checkBoxTP.checked = checkBox.checked
        checkBoxFP.checked = checkBox.checked
        checkBoxRP.checked = false
        checkBoxRN.checked = false
        checkBoxN.checked = false
        
        if (checkBox.checked) {
        
        var checkBoxPN = document.getElementById(\'predicted_negatives_checkbox\')
        
        var checkBoxTN = document.getElementById(\'tn-checkbox\')
        var checkBoxFN = document.getElementById(\'fn-checkbox\')
        

        checkBoxPN.checked = false
        checkBoxTN.checked = false
        checkBoxFN.checked = false
        
        }
        

        }
        
        if ( checkBox.id === \'predicted_negatives_checkbox\' ) {
        
        var checkBoxTN = document.getElementById(\'tn-checkbox\')
        var checkBoxFN = document.getElementById(\'fn-checkbox\')
        var checkBoxRP = document.getElementById(\'real_positives_checkbox\')
        var checkBoxRN = document.getElementById(\'real_negatives_checkbox\')
        var checkBoxN = document.getElementById(\'all_observations_checkbox\')

        checkBoxTN.checked = checkBox.checked
        checkBoxFN.checked = checkBox.checked
        checkBoxRP.checked = false
        checkBoxRN.checked = false
        checkBoxN.checked = false
        
        if (checkBox.checked) {
        
        
        var checkBoxPP = document.getElementById(\'predicted_positives_checkbox\')
        var checkBoxTP = document.getElementById(\'tp-checkbox\')
        var checkBoxFP = document.getElementById(\'fp-checkbox\')
        
        checkBoxTP.checked = false
        checkBoxFP.checked = false
        checkBoxPP.checked = false
        
        }
        

        }
        
        if ( checkBox.id === \'real_positives_checkbox\' ) {
        
        console.log(\'chose real positives\')
        
        var checkBoxTP = document.getElementById(\'tp-checkbox\')
        var checkBoxFN = document.getElementById(\'fn-checkbox\')
        var checkBoxPP = document.getElementById(\'predicted_positives_checkbox\')
        var checkBoxPN = document.getElementById(\'predicted_negatives_checkbox\')
        var checkBoxN = document.getElementById(\'all_observations_checkbox\')

        checkBoxTP.checked = checkBox.checked
        checkBoxFN.checked = checkBox.checked
        checkBoxPP.checked = false
        checkBoxPN.checked = false
        checkBoxN.checked = false
        
        if (checkBox.checked) {
        
        
        var checkBoxRN = document.getElementById(\'real_negatives_checkbox\')
        var checkBoxTN = document.getElementById(\'tn-checkbox\')
        var checkBoxFP = document.getElementById(\'fp-checkbox\')
        
        checkBoxRN.checked = false
        checkBoxTN.checked = false
        checkBoxFP.checked = false
        
        }
        

        }
        
        if ( checkBox.id === \'real_negatives_checkbox\' ) {
        
        console.log(\'chose real positives\')
        
        var checkBoxTN = document.getElementById(\'tn-checkbox\')
        var checkBoxFP = document.getElementById(\'fp-checkbox\')
        var checkBoxPP = document.getElementById(\'predicted_positives_checkbox\')
        var checkBoxPN = document.getElementById(\'predicted_negatives_checkbox\')
        var checkBoxN = document.getElementById(\'all_observations_checkbox\')

        checkBoxTN.checked = checkBox.checked
        checkBoxFP.checked = checkBox.checked
        checkBoxPP.checked = false
        checkBoxPN.checked = false
        checkBoxN.checked = false
        
        if (checkBox.checked) {
        
        
        var checkBoxRP = document.getElementById(\'real_positives_checkbox\')
        var checkBoxTP = document.getElementById(\'tp-checkbox\')
        var checkBoxFN = document.getElementById(\'fn-checkbox\')
        
        checkBoxRP.checked = false
        checkBoxTP.checked = false
        checkBoxFN.checked = false
        
        }
        

        }
        
        if ( checkBox.id === \'all_observations_checkbox\' ) {
        
        var checkBoxTN = document.getElementById(\'tn-checkbox\')
        var checkBoxFN = document.getElementById(\'fn-checkbox\')

        checkBoxTN.checked = checkBox.checked
        checkBoxFN.checked = checkBox.checked
                
        var checkBoxTP = document.getElementById(\'tp-checkbox\')
        var checkBoxFP = document.getElementById(\'fp-checkbox\')

        checkBoxTP.checked = checkBox.checked
        checkBoxFP.checked = checkBox.checked
        
        var checkBoxPP = document.getElementById(\'predicted_positives_checkbox\')
        var checkBoxPN = document.getElementById(\'predicted_negatives_checkbox\')
        
        checkBoxPP.checked = checkBox.checked
        checkBoxPN.checked = checkBox.checked
        
        var checkBoxRN = document.getElementById(\'real_positives_checkbox\')
        var checkBoxRP = document.getElementById(\'real_negatives_checkbox\')
        
        checkBoxRN.checked = checkBox.checked
        checkBoxRP.checked = checkBox.checked
        
        }
        
        
        if (checkBox.checked) {
        
        
        
        console.log('checkbox is checked')
        
          return {
          highlightedMetrics: {",
    purrr::map2_chr(
      c(highlightedMetrics$checked, "true"), 
      c(names(highlightedMetrics$checked), "PP"), 
      ~paste_highlighted_metric_text(.x, .y)
    ) |>
      unlist() |> 
      paste(collapse = ", "),
    "}}
    
    } else {
    
      console.log('checkbox is not checked')
        
        return {
          highlightedMetrics: {",
    purrr::map2_chr(
      c(highlightedMetrics$nonchecked, "true"), 
      c(names(highlightedMetrics$nonchecked), "PP"), 
      ~paste_highlighted_metric_text(.x, .y)
    ) |>
      unlist() |> 
      paste(collapse = ", "),
    "}}}})"
  )
  
}


confusion_matrix_real_cell_maker <- function(){
  
  paste(
    '
    function(cellInfo) {
      
      return `<input type="checkbox" checked = true 
      id=${cellInfo.value.toLowerCase().replace(/ /g, "_") + "_checkbox"}
 onclick="(function(cellInfo, state) {
    const { currency, exchangeRates } = state.meta;
    const cellInfoValueMetric = cellInfo.value
    
    console.log(cellInfoValueMetric)
 
 ', onclick_reactable_setMeta(
   highlightedMetrics = list(
     checked = list(
       "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
     ),
     nonchecked = list(
       "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
     )        ), checkboxId = "\'real_positives_checkbox\'"), '
    
      console.log(cellInfo.value)
    
    })">${cellInfo.value}`
    } 
  ')
  
}


confusion_matrix_cell_maker <- function() {
  paste(
    'function(cellInfo, state) {
    const { currency, exchangeRates } = state.meta;
    const cellInfoValueMetric = cellInfo.value
    
    console.log(cellInfoValueMetric.toLowerCase() === "tp")

if (cellInfoValueMetric.toLowerCase() === "tp") {

    return `<input type="checkbox" id="tp-checkbox" 
      name="vehicle1" value="Bike" checked = true onclick="(function(cellInfoValueMetric) {
      ', onclick_reactable_setMeta(
        highlightedMetrics = list(
          checked = list(
            "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
          ),
          nonchecked = list(
            "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
          )        ), checkboxId = "\'tp-checkbox\'"), '
      
    console.log(\'Internal function is called\');
  })()">
      ${cellInfo.value}`;
  } else if (cellInfoValueMetric.toLowerCase() === "fp") {
  
  console.log("This is not TP")
  
  return `<input type="checkbox" id="fp-checkbox" 
      name="vehicle1" value="Bike" checked = true onclick="(function(cellInfoValueMetric) {
      ', onclick_reactable_setMeta(
        highlightedMetrics = list(
          checked = list(
            "TP" = "prevMeta", "TN" = "prevMeta", "FP" = "!prevMeta", "FN" = "prevMeta"
          ),
          nonchecked = list(
            "TP" = "prevMeta", "TN" = "prevMeta", "FP" = "!prevMeta", "FN" = "prevMeta"
          )        ), checkboxId = "\'fp-checkbox\'"),'
    console.log(\'Internal function is called\');
  })()">
      ${cellInfo.value}`;
    
  } else if (cellInfoValueMetric.toLowerCase() === "tn") {
  
  return `<input type="checkbox" id="tn-checkbox" 
      name="vehicle1" value="Bike" checked = true onclick="(function(cellInfoValueMetric) {
      
          console.log(cellInfoValueMetric)

      ', onclick_reactable_setMeta(
        highlightedMetrics = list(
          checked = list(
            "TP" = "prevMeta", "TN" = "!prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
          ),
          nonchecked = list(
            "TP" = "prevMeta", "TN" = "!prevMeta", "FP" = "prevMeta", "FN" = "prevMeta"
          )        ), checkboxId = "\'tn-checkbox\'"),'
      
    console.log(\'Internal function is called\');
  })()">
      ${cellInfo.value}`;
  
  
  } else if (cellInfoValueMetric.toLowerCase() === "fn") {
  
   console.log("This is not TP")
  
  return `<input type="checkbox" id="fn-checkbox" 
      name="vehicle1" value="Bike" checked = true onclick="(function(cellInfoValueMetric) {
      
          console.log(cellInfoValueMetric)

      //console.log(cellInfo)
      //console.log(cellInfo)
      
      ', onclick_reactable_setMeta(
        highlightedMetrics = list(
          checked = list(
            "TP" = "prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "!prevMeta"
          ),
          nonchecked = list(
            "TP" = "prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "!prevMeta"
          )        ), checkboxId = "\'fn-checkbox\'"),'
    console.log(\'Internal function is called\');
  })()">
      ${cellInfo.value}`;
  
  
  } else if (cellInfoValueMetric.toLowerCase() === "real positives") {

    return `<input type="checkbox" id="real_positives_checkbox" 
      name="vehicle1" value="Bike" checked = true onclick="(function(cellInfoValueMetric) {
      ', onclick_reactable_setMeta(
        highlightedMetrics = list(
          checked = list(
            "TP" = "true", "TN" = "false", "FP" = "false", "FN" = "true"
          ),
          nonchecked = list(
            "TP" = "!prevMeta", "TN" = "prevMeta", "FP" = "prevMeta", "FN" = "!prevMeta"
          )        ), checkboxId = "\'real_positives_checkbox\'"), '
      
    console.log(\'Internal function is called\');
  })()">
      ${cellInfo.value}`;
  } else if (cellInfoValueMetric.toLowerCase() === "real negatives") {

    return `<input type="checkbox" id="real_negatives_checkbox" 
      name="vehicle1" value="Bike" checked = true onclick="(function(cellInfoValueMetric) {
      ', onclick_reactable_setMeta(
        highlightedMetrics = list(
          checked = list(
            "TP" = "false", "TN" = "true", "FP" = "true", "FN" = "false"
          ),
          nonchecked = list(
            "TP" = "prevMeta", "TN" = "!prevMeta", "FP" = "!prevMeta", "FN" = "prevMeta"
          )        ), checkboxId = "\'real_negatives_checkbox\'"), '
      
    console.log(\'Internal function is called\');
  })()">
      ${cellInfo.value}`;
  }
  }')
  
  
}

confusion_matrix_style_maker <- function(predicted){
  
  if (predicted == "predicted_positives") {
    
    "function(rowInfo, column, state) {
      
      const { showColors, mpgColors } = state.meta
      const value = rowInfo.values['predicted_positives']
      console.log('value')
      console.log(value)
      
      
        if (state.meta.highlightedMetrics.TP & value == 'TP') {
        console.log('TP Manipulation checked')
          return { backgroundColor: `#009e73`, fontWeight: `600` }
        } 
      if (!state.meta.highlightedMetrics.TP & value == 'TP') {
      
      console.log('TP Manipulation not checked')
          return { backgroundColor: `#F4FFF0`, fontWeight: `600` }
      }
        
       if (state.meta.highlightedMetrics.FP & value == 'FP') {
       console.log('FP Manipulation checked')
          return { backgroundColor: `#FAC8CD`, fontWeight: `600` }
        } 
      if (!state.meta.highlightedMetrics.FP & value == 'FP') {
      console.log('FP Manipulation not checked')
          return { backgroundColor: `#FFF7F8`, fontWeight: `600` }
        }  
        
      }"
    
    
  } else {
    
    "function(rowInfo, column, state) {
        const { showColors, mpgColors } = state.meta
      const value = rowInfo.values['predicted_negatives']
      
        if (state.meta.highlightedMetrics.TN & value == 'TN') {
        console.log('TN Manipulation checked')
          return { backgroundColor: `#009e73`, fontWeight: `600` }
        } 
      if (!state.meta.highlightedMetrics.TN & value == 'TN') {
        console.log('TN Manipulation not checked')
          return { backgroundColor: `#F4FFF0`, fontWeight: `600` }
      }
        
       if (state.meta.highlightedMetrics.FN & value == 'FN') {
               console.log('FN Manipulation checked')

          return { backgroundColor: `#FAC8CD`, fontWeight: `600` }
        } 
      if (!state.meta.highlightedMetrics.FN & value == 'FN') {
              console.log('FN Manipulation not checked')

          return { backgroundColor: `#FFF7F8`, fontWeight: `600`}
        }  
        
      }"
    
  }
  
}

create_highlighted_confusion_matrix_as_html_input <- function(){
  
  tibble::tribble(
    ~"type", ~"predicted_positives", ~"predicted_negatives",
    "Real Positives", "TP", "FN",
    "Real Negatives", "FP", "TN"
  ) |> 
    reactable(
      sortable = FALSE,
      fullWidth = FALSE,
      borderless = FALSE,
      defaultColDef = reactable::colDef(
        html = TRUE,
        align = "center",
        headerStyle  = list(fontWeight = 100),
        header = reactable::JS(confusion_matrix_header_maker())
      ),
      columns = list(
        predicted_positives = colDef(
          name = "Predicted Positives", 
          style = JS(confusion_matrix_style_maker("predicted_positives")), 
          cell = reactable::JS(confusion_matrix_cell_maker())
        ),
        type = reactable::colDef(
          name = "<br>All<br>Observations", 
          # cell = reactable::JS(confusion_matrix_real_cell_maker()),
          cell = reactable::JS(confusion_matrix_cell_maker()),
          align = "left",
          style = list(fontWeight = 100),
          minWidth = 140
        ),
        predicted_negatives = reactable::colDef(
          name = "Predicted Negatives", 
          cell = reactable::JS(confusion_matrix_cell_maker()),
          style = JS(confusion_matrix_style_maker("predicted_negatives"))
        )
      ),
      meta = list(
        highlightedMetrics = list(
          TP = TRUE, FP = TRUE, TN = TRUE, FN = TRUE, N = TRUE, PP = TRUE, PN = TRUE, RP = TRUE, RN = TRUE
        )),
      elementId = "cars-colors-table"
    )
  
}