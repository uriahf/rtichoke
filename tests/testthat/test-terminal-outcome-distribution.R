test_that("terminal outcome preparation matches independent reference for all cases and rows", {
  ref_path <- testthat::test_path("../fixtures/terminal_event/reference.json")
  skip_if_not(file.exists(ref_path), "Reference artifact not found")

  ref <- jsonlite::fromJSON(ref_path)

  for (case_name in names(ref$cases)) {
    case <- ref$cases[[case_name]]
    raw_df <- as.data.frame(case$data)
    # reference has id, time, event
    df <- data.frame(
      id = raw_df$id,
      reals = raw_df$event,
      times = raw_df$time,
      stringsAsFactors = FALSE
    )

    populations <- stats::setNames(list(df), case_name)
    prepared <- prepare_terminal_outcome_distributions(
      populations,
      case$horizons
    )

    expected_variants <- case$variants
    # Map expected rows by (variant, horizon)
    expected_map <- list()
    for (v_name in names(expected_variants)) {
      v_rows <- expected_variants[[v_name]]
      for (i in seq_len(nrow(v_rows))) {
        r <- as.list(v_rows[i, ])
        h_val <- as.numeric(r$horizon)
        key <- paste(v_name, h_val, sep = "@")
        expected_map[[key]] <- r
      }
    }

    expect_equal(length(prepared), length(expected_map))

    for (row in prepared) {
      key <- paste(
        paste(row$censoring, row$competing, sep = ":"),
        row$horizon,
        sep = "@"
      )
      exp_row <- expected_map[[key]]
      expect_false(
        is.null(exp_row),
        info = paste("Missing expected row for", key)
      )

      expect_identical(row$evaluation_id, "evaluation-1")
      expect_identical(row$population, case_name)
      expect_identical(
        as.character(row$retained_ids),
        as.character(unlist(exp_row$retained_ids))
      )
      expect_identical(
        as.character(row$censored_excluded_ids),
        as.character(unlist(exp_row$censored_excluded_ids))
      )
      expect_identical(
        as.character(row$competing_excluded_ids),
        as.character(unlist(exp_row$competing_excluded_ids))
      )
      expect_identical(row$original_n, as.integer(exp_row$original_n))
      expect_identical(row$retained_n, as.integer(exp_row$retained_n))
      expect_identical(
        row$censored_excluded_count,
        as.integer(exp_row$censored_excluded_count)
      )
      expect_identical(
        row$competing_excluded_count,
        as.integer(exp_row$competing_excluded_count)
      )
      expect_identical(
        row$retained_n +
          row$censored_excluded_count +
          row$competing_excluded_count,
        row$original_n
      )
      expect_equal(
        row$censored_excluded_fraction,
        row$censored_excluded_count / row$original_n
      )
      expect_equal(
        row$competing_excluded_fraction,
        row$competing_excluded_count / row$original_n
      )
      expect_identical(row$status, as.character(exp_row$status))

      expected_estimator <- if (row$competing == "adjusted_as_negative") {
        "aalen_johansen"
      } else {
        "kaplan_meier"
      }
      expect_identical(row$estimator, expected_estimator)

      # Probabilities check
      exp_probs <- exp_row$probabilities
      if (row$status == "undefined_empty_population") {
        for (st in names(row$probabilities)) {
          expect_null(row$probabilities[[st]])
        }
      } else {
        for (st in names(row$probabilities)) {
          act_p <- row$probabilities[[st]]
          exp_p <- as.numeric(exp_probs[[st]])
          expect_true(
            abs(act_p - exp_p) < 1e-12,
            info = paste(key, st, act_p, exp_p)
          )
        }
      }
    }
  }
})

test_that("input validation strictly enforces types, constraints, and uniqueness", {
  d <- data.frame(id = 1:3, reals = c(0, 1, 2), times = c(1, 2, 3))

  expect_error(
    prepare_terminal_outcome_distributions(d, c(1, 2)),
    "`populations` must be a non-empty named list"
  )
  expect_error(
    prepare_terminal_outcome_distributions(list(d), c(1, 2)),
    "named list with non-empty string names"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      stats::setNames(list(d), ""),
      c(1, 2)
    ),
    "named list with non-empty string names"
  )
  expect_error(
    prepare_terminal_outcome_distributions(list(a = d, a = d), c(1, 2)),
    "unique names"
  )

  expect_error(
    prepare_terminal_outcome_distributions(list(a = d), numeric(0)),
    "non-empty sequence"
  )
  expect_error(
    prepare_terminal_outcome_distributions(list(a = d), c(1, -0.5)),
    "non-negative"
  )
  expect_error(
    prepare_terminal_outcome_distributions(list(a = d), c(1, Inf)),
    "finite"
  )
  expect_error(
    prepare_terminal_outcome_distributions(list(a = d), c(1, NA)),
    "finite"
  )

  # Empty frame or missing columns
  expect_error(
    prepare_terminal_outcome_distributions(list(a = d[0, ]), 1),
    "non-empty data frame"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, times = 1)),
      1
    ),
    "columns"
  )

  # ID null or duplicate
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = c(1, NA), reals = c(0, 1), times = c(1, 2))),
      1
    ),
    "non-null"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = c(1, 1), reals = c(0, 1), times = c(1, 2))),
      1
    ),
    "unique"
  )

  # Reals invalid, factor, logical
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = factor(1), times = 1)),
      1
    ),
    "numeric"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = TRUE, times = 1)),
      1
    ),
    "numeric"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = 3, times = 1)),
      1
    ),
    "0, 1, or 2"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = -1, times = 1)),
      1
    ),
    "0, 1, or 2"
  )

  # Times non-numeric, NA, negative
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = 1, times = "1")),
      1
    ),
    "finite non-negative"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = 1, times = -1)),
      1
    ),
    "finite non-negative"
  )
  expect_error(
    prepare_terminal_outcome_distributions(
      list(a = data.frame(id = 1, reals = 1, times = NA_real_)),
      1
    ),
    "finite non-negative"
  )
})

test_that("semantic population identity is preserved and evaluation IDs are deterministic", {
  d <- data.frame(id = 1:2, reals = c(1, 0), times = c(1, 2))
  # Two identical datasets with different semantic names
  prepared <- prepare_terminal_outcome_distributions(
    list(pop_alpha = d, pop_beta = d),
    c(1, 2)
  )

  expect_equal(length(prepared), 2 * 8 * 2)
  eval_ids <- unique(sapply(prepared, function(r) r$evaluation_id))
  expect_identical(eval_ids, c("evaluation-1", "evaluation-2"))

  alpha_rows <- Filter(function(r) r$population == "pop_alpha", prepared)
  beta_rows <- Filter(function(r) r$population == "pop_beta", prepared)
  expect_true(all(sapply(alpha_rows, function(r) {
    r$evaluation_id == "evaluation-1"
  })))
  expect_true(all(sapply(beta_rows, function(r) {
    r$evaluation_id == "evaluation-2"
  })))
})

test_that("canonical serialization produces valid structure and respects roundoff bounds", {
  d <- data.frame(id = 1:3, reals = c(0, 1, 2), times = c(1, 2, 3))
  prepared <- prepare_terminal_outcome_distributions(
    list(test_pop = d),
    c(1, 2)
  )
  spec <- rtichoke_viz_terminal_outcome_distribution_v2_spec(prepared)

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "outcome_distribution")
  expect_identical(spec$title, "Outcome Distribution")
  expect_identical(length(spec$variants), 8L)
  expect_true("adjusted:adjusted_as_negative" %in% names(spec$variants))

  # Test endpoint roundoff boundary
  eps <- .Machine$double.eps
  expect_equal(.canonical_probability(1 + 4 * eps), 1.0)
  expect_equal(.canonical_probability(-4 * eps), 0.0)
  expect_error(
    .canonical_probability(1 + 10 * eps),
    "finite probability in \\[0, 1\\]"
  )
  expect_error(
    .canonical_probability(-10 * eps),
    "finite probability in \\[0, 1\\]"
  )
  expect_error(
    .canonical_probability(NA_real_),
    "finite probability in \\[0, 1\\]"
  )
  expect_error(.canonical_probability(NULL), "must not be NULL")
})

test_that("vendored v0.24.2 JS schema and referential integrity validations pass for all reference cases", {
  skip_if(
    Sys.which("node") == "",
    "Node.js is not available for vendored JS validation"
  )

  ref_path <- testthat::test_path("../fixtures/terminal_event/reference.json")
  skip_if_not(file.exists(ref_path), "Reference artifact not found")

  ref <- jsonlite::fromJSON(ref_path)
  vendor_js <- normalizePath(
    file.path(
      testthat::test_path("../../inst/rtichoke-viz"),
      "rtichoke-viz.js"
    ),
    mustWork = TRUE
  )
  skip_if_not(file.exists(vendor_js), "rtichoke-viz.js not found")

  for (case_name in names(ref$cases)) {
    case <- ref$cases[[case_name]]
    raw_df <- as.data.frame(case$data)
    df <- data.frame(
      id = raw_df$id,
      reals = raw_df$event,
      times = raw_df$time,
      stringsAsFactors = FALSE
    )

    prepared <- prepare_terminal_outcome_distributions(
      stats::setNames(list(df), case_name),
      case$horizons
    )
    spec <- rtichoke_viz_terminal_outcome_distribution_v2_spec(prepared)

    spec_json <- jsonlite::toJSON(
      spec,
      auto_unbox = TRUE,
      null = "null",
      digits = 16
    )
    temp_json_file <- tempfile(fileext = ".json")
    writeLines(spec_json, temp_json_file)

    node_script <- sprintf(
      '
import * as viz from %s;
import fs from "fs";

const raw = fs.readFileSync(%s, "utf8");
const spec = JSON.parse(raw);

// 1. Verify schema export existence
if (!viz.OutcomeDistributionSpecSchema) {
  throw new Error("Missing OutcomeDistributionSpecSchema export");
}

// 2. Validate referential integrity
viz.assertOutcomeDistributionReferentialIntegrity(spec);

// 3. Test report integration referential integrity
const report = {
  schemaVersion: "1.1",
  type: "report",
  heuristics: {
    active: "adjusted:adjusted_as_negative",
    available: Object.keys(spec.variants)
  },
  sections: [
    {
      id: "outcome-distribution-section",
      title: "Outcome Distribution",
      items: [
        {
          type: "component",
          id: "outcome-dist-comp",
          spec: spec
        }
      ]
    }
  ]
};

viz.assertReportReferentialIntegrity(report);
',
      jsonlite::toJSON(vendor_js, auto_unbox = TRUE),
      jsonlite::toJSON(temp_json_file, auto_unbox = TRUE)
    )

    temp_script <- tempfile(fileext = ".mjs")
    writeLines(node_script, temp_script)

    res <- system2("node", args = c(temp_script), stdout = TRUE, stderr = TRUE)
    unlink(c(temp_json_file, temp_script))

    status <- attr(res, "status")
    if (!is.null(status) && status != 0) {
      fail(paste(
        "Node validation failed for case",
        case_name,
        ":",
        paste(res, collapse = "\n")
      ))
    }
    expect_true(is.null(status) || status == 0)
  }
})

test_that("canonical spec validates state distributions using assertStateDistributionValid", {
  skip_if(
    Sys.which("node") == "",
    "Node.js is not available for vendored JS validation"
  )

  vendor_js <- normalizePath(
    file.path(
      testthat::test_path("../../inst/rtichoke-viz"),
      "rtichoke-viz.js"
    ),
    mustWork = TRUE
  )
  d <- data.frame(id = 1:3, reals = c(0, 1, 2), times = c(1, 2, 3))
  prepared <- prepare_terminal_outcome_distributions(
    list(test_pop = d),
    c(1, 2)
  )
  spec <- rtichoke_viz_terminal_outcome_distribution_v2_spec(prepared)

  spec_json <- jsonlite::toJSON(
    spec,
    auto_unbox = TRUE,
    null = "null",
    digits = 16
  )
  temp_json_file <- tempfile(fileext = ".json")
  writeLines(spec_json, temp_json_file)

  node_script <- sprintf(
    '
import * as viz from %s;
import fs from "fs";

const raw = fs.readFileSync(%s, "utf8");
const spec = JSON.parse(raw);

for (const dist of spec.stateDistributions) {
  viz.assertStateDistributionValid(dist);
}
for (const variant of Object.values(spec.variants)) {
  for (const dist of variant.stateDistributions) {
    viz.assertStateDistributionValid(dist);
  }
}
',
    jsonlite::toJSON(vendor_js, auto_unbox = TRUE),
    jsonlite::toJSON(temp_json_file, auto_unbox = TRUE)
  )

  temp_script <- tempfile(fileext = ".mjs")
  writeLines(node_script, temp_script)

  res <- system2("node", args = c(temp_script), stdout = TRUE, stderr = TRUE)
  unlink(c(temp_json_file, temp_script))

  status <- attr(res, "status")
  expect_true(is.null(status) || status == 0)
})
