filter_checkbox_rtichoke <- function(id,
                                     label,
                                     sharedData,
                                     group,
                                     allLevels = FALSE,
                                     inline = FALSE,
                                     columns = 1,
                                     labels_values) {
  options <- makeGroupOptions_rtichoke(
    sharedData, group, allLevels,
    labels_values
  )

  labels <- options$items$label
  values <- options$items$value
  options$items <- NULL # Doesn't need to be serialized for this type of control

  makeCheckbox <- if (inline) inlineCheckbox else blockCheckbox

  htmltools::browsable(attachDependencies(
    tags$div(
      id = id,
      class = "form-group crosstalk-input-checkboxgroup crosstalk-input",
      tags$label(class = "control-label", `for` = id, label),
      tags$div(
        class = "crosstalk-options-group",
        columnize(
          columns,
          mapply(labels, values, FUN = function(label, value) {
            makeCheckbox(id, value, label)
          }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )
      ),
      tags$script(
        type = "application/json",
        `data-for` = id,
        jsonlite::toJSON(options, dataframe = "columns", pretty = TRUE)
      )
    ),
    c(list(jqueryLib()), crosstalk::crosstalkLibs())
  ))
}

makeGroupOptions_rtichoke <- function(sharedData, group, allLevels,
                                      labels_values) {
  df <- sharedData$data(
    withSelection = FALSE,
    withFilter = FALSE,
    withKey = TRUE
  )

  if (inherits(group, "formula")) {
    group <- lazyeval::f_eval(group, df)
  }

  if (length(group) < 1) {
    stop("Can't form options with zero-length group vector")
  }

  lvls <- if (is.factor(group)) {
    if (allLevels) {
      levels(group)
    } else {
      levels(droplevels(group))
    }
  } else {
    sort(unique(group))
  }
  matches <- match(group, lvls)
  vals <- lapply(seq_len(length(lvls)), function(i) {
    df$key_[which(matches == i)]
  })

  lvls_str <- as.character(lvls)

  # print(lvls_str)

  options <- list(
    items = data.frame(
      value = lvls_str,
      label = labels_values,
      stringsAsFactors = FALSE
    ),
    map = setNames(vals, lvls_str),
    group = sharedData$groupName()
  )

  # print(options)
  options
}

inlineCheckbox <- function(id, value, label) {
  tags$label(
    class = "checkbox-inline",
    tags$input(type = "checkbox", name = id, value = value),
    tags$span(label)
  )
}


jqueryLib <- function() {
  htmlDependency(
    name = "jquery",
    version = "3.5.1",
    package = "crosstalk",
    src = "lib/jquery",
    script = "jquery.min.js"
  )
}


columnize <- function(columnCount, elements) {
  if (columnCount <= 1 || length(elements) <= 1) {
    return(elements)
  }

  columnSize <- ceiling(length(elements) / columnCount)
  lapply(1:ceiling(length(elements) / columnSize), function(i) {
    tags$div(class = "crosstalk-options-column", {
      start <- (i - 1) * columnSize + 1
      end <- i * columnSize
      elements[start:end]
    })
  })
}
