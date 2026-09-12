validate_prediction_distribution_spec <- function(spec) {
  schema_path <- system.file(
    "rtichoke-viz",
    "rtichoke-viz-report.schema.json",
    package = "rtichoke"
  )
  if (!file.exists(schema_path)) {
    stop("rtichoke-viz-report.schema.json not found", call. = FALSE)
  }

  json_str <- jsonlite::toJSON(
    spec,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )

  ctx <- V8::v8()
  ctx$assign("schemaJson", readChar(schema_path, file.info(schema_path)$size))
  ctx$assign("specJson", json_str)

  script <- "
    const schema = JSON.parse(schemaJson);
    const predSchema = schema.anyOf[0].properties.components.items.properties.spec.anyOf[3];
    const spec = JSON.parse(specJson);

    function validateObj(obj, sch, path = '') {
      if (!sch) return;
      if (sch.type === 'object') {
        if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
          throw new Error(path + ' expected object, got ' + typeof obj);
        }
        if (sch.required) {
          for (const req of sch.required) {
            if (!(req in obj)) {
              throw new Error(path + ' missing required property: ' + req);
            }
          }
        }
        if (sch.properties) {
          for (const [k, v] of Object.entries(sch.properties)) {
            if (k in obj) {
              validateObj(obj[k], v, path + '.' + k);
            }
          }
        }
      } else if (sch.type === 'array') {
        if (!Array.isArray(obj)) {
          throw new Error(path + ' expected array, got ' + typeof obj);
        }
        if (sch.minItems !== undefined && obj.length < sch.minItems) {
          throw new Error(path + ' array length ' + obj.length + ' < minItems ' + sch.minItems);
        }
        if (sch.items) {
          obj.forEach((item, idx) => validateObj(item, sch.items, path + '[' + idx + ']'));
        }
      } else if (sch.const !== undefined) {
        if (obj !== sch.const) {
          throw new Error(path + ' const mismatch: expected ' + sch.const + ', got ' + obj);
        }
      } else if (sch.anyOf) {
        let matched = false;
        for (const sub of sch.anyOf) {
          try {
            validateObj(obj, sub, path);
            matched = true;
            break;
          } catch (e) {}
        }
        if (!matched) {
          throw new Error(path + ' failed to match anyOf schema option: ' + JSON.stringify(obj));
        }
      }
    }

    validateObj(spec, predSchema);
    'VALID';
  "

  res <- ctx$eval(script)
  invisible(TRUE)
}

test_that("prediction distribution adapter builds exact canonical spec and validates against schema", {
  probs <- list("Model A" = c(0, 0.2, 0.5, 1))
  reals <- list(c(0, 1, 0, 1))
  by <- 0.5

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by
  )
  p_perf <- prepare_performance_data(probs = probs, reals = reals, by = by)
  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "prediction_distribution")
  expect_identical(spec$operatingPoint$dimension, "probability_threshold")
  expect_identical(
    spec$evaluations,
    list(list(
      id = "evaluation-1",
      population = "population",
      model = "Model A"
    ))
  )
  expect_named(
    spec$bins[[1]],
    c(
      "evaluationId",
      "lower",
      "upper",
      "includeLower",
      "includeUpper",
      "nPositive",
      "nNegative"
    )
  )
  expect_named(
    spec$rankBins[[1]],
    c(
      "evaluationId",
      "rankLower",
      "rankUpper",
      "positiveMass",
      "negativeMass"
    )
  )
  expect_named(
    spec$operatingPoints[[1]],
    c("evaluationId", "type", "value", "cutoff", "realizedPpcr", "performance")
  )

  # Check performance metric structure
  perf_entries <- spec$operatingPoints[[1]]$performance
  expect_length(perf_entries, 9L)
  expect_identical(
    vapply(perf_entries, `[[`, character(1), "metricId"),
    c(
      "true_positives",
      "true_negatives",
      "false_positives",
      "false_negatives",
      "sensitivity",
      "specificity",
      "ppv",
      "npv",
      "lift"
    )
  )

  # Schema validation
  expect_true(validate_prediction_distribution_spec(spec))
})

test_that("frozen tied rank-bin golden fixture matches expected rankBins and literal PPCR operating points", {
  probs <- list(c(0.00, 0.15, 0.30, 0.50, 0.50, 0.50, 0.65, 0.80, 1.00))
  reals <- list(c(0, 1, 0, 1, 0, 1, 1, 0, 1))
  by <- 0.20

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = "ppcr"
  )
  p_perf <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = "ppcr"
  )
  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  rank_df <- do.call(rbind, lapply(spec$rankBins, as.data.frame))

  expected_rank_lower <- c(0.00, 0.20, 0.40, 0.60, 0.80)
  expected_rank_upper <- c(0.20, 0.40, 0.60, 0.80, 1.00)
  expected_pos_mass <- c(1, 2, 0, 1, 1)
  expected_neg_mass <- c(1, 2, 0, 0, 1)

  expect_equal(rank_df$rankLower, expected_rank_lower)
  expect_equal(rank_df$rankUpper, expected_rank_upper)
  expect_equal(rank_df$positiveMass, expected_pos_mass)
  expect_equal(rank_df$negativeMass, expected_neg_mass)
  expect_equal(rank_df$positiveMass[3], 0) # empty stratum preserved

  # Literal canonical operating points at PPCR 0.40, 0.60, and 0.80
  get_op <- function(v) {
    Filter(function(x) abs(x$value - v) < 1e-6, spec$operatingPoints)[[1]]
  }
  get_metric <- function(op, metric_id) {
    Filter(function(m) m$metricId == metric_id, op$performance)[[1]]$estimate
  }

  op_40 <- get_op(0.40)
  expect_equal(op_40$value, 0.40)
  expect_equal(op_40$cutoff, 0.50)
  expect_equal(op_40$realizedPpcr, 3 / 9, tolerance = 1e-4)
  expect_equal(get_metric(op_40, "true_positives"), 2L)
  expect_equal(get_metric(op_40, "false_positives"), 1L)
  expect_equal(get_metric(op_40, "true_negatives"), 3L)
  expect_equal(get_metric(op_40, "false_negatives"), 3L)

  op_60 <- get_op(0.60)
  expect_equal(op_60$value, 0.60)
  expect_equal(op_60$cutoff, 0.50)
  expect_equal(op_60$realizedPpcr, 3 / 9, tolerance = 1e-4)
  expect_equal(get_metric(op_60, "true_positives"), 2L)
  expect_equal(get_metric(op_60, "false_positives"), 1L)
  expect_equal(get_metric(op_60, "true_negatives"), 3L)
  expect_equal(get_metric(op_60, "false_negatives"), 3L)

  op_80 <- get_op(0.80)
  expect_equal(op_80$value, 0.80)
  expect_equal(op_80$cutoff, 0.24)
  expect_equal(op_80$realizedPpcr, 7 / 9, tolerance = 1e-4)
  expect_equal(get_metric(op_80, "true_positives"), 4L)
  expect_equal(get_metric(op_80, "false_positives"), 3L)
  expect_equal(get_metric(op_80, "true_negatives"), 1L)
  expect_equal(get_metric(op_80, "false_negatives"), 1L)
})

test_that("N < q fixture retains all empty labelled strata in rankBins", {
  probs <- list(c(0.2, 0.8))
  reals <- list(c(0, 1))
  by <- 0.1 # q = 10, N = 2

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by
  )
  p_perf <- prepare_performance_data(probs = probs, reals = reals, by = by)
  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  expect_length(spec$rankBins, 10L)
  total_pos <- sum(vapply(spec$rankBins, `[[`, numeric(1), "positiveMass"))
  total_neg <- sum(vapply(spec$rankBins, `[[`, numeric(1), "negativeMass"))
  expect_equal(total_pos, 1)
  expect_equal(total_neg, 1)
})

test_that("rankBins invariants hold: mass conservation and order invariance", {
  set.seed(42)
  p_vec <- runif(50)
  r_vec <- rbinom(50, 1, 0.5)

  by <- 0.05
  p_dist1 <- prepare_probs_distribution_data(
    probs = list(p_vec),
    reals = list(r_vec),
    by = by,
    stratified_by = "probability_threshold"
  )
  p_perf1 <- prepare_performance_data(
    probs = list(p_vec),
    reals = list(r_vec),
    by = by,
    stratified_by = "probability_threshold"
  )
  spec1 <- rtichoke_viz_prediction_distribution_spec(p_dist1, p_perf1)

  p_dist2 <- prepare_probs_distribution_data(
    probs = list(p_vec),
    reals = list(r_vec),
    by = by,
    stratified_by = "ppcr"
  )
  p_perf2 <- prepare_performance_data(
    probs = list(p_vec),
    reals = list(r_vec),
    by = by,
    stratified_by = "ppcr"
  )
  spec2 <- rtichoke_viz_prediction_distribution_spec(p_dist2, p_perf2)

  # rankBins identical across threshold and PPCR modes
  expect_identical(spec1$rankBins, spec2$rankBins)

  # Mass conservation
  pos_mass <- sum(vapply(spec1$rankBins, `[[`, numeric(1), "positiveMass"))
  neg_mass <- sum(vapply(spec1$rankBins, `[[`, numeric(1), "negativeMass"))
  expect_equal(pos_mass, sum(r_vec == 1))
  expect_equal(neg_mass, sum(r_vec == 0))

  # Order invariance
  perm <- sample(length(p_vec))
  p_dist_perm <- prepare_probs_distribution_data(
    probs = list(p_vec[perm]),
    reals = list(r_vec[perm]),
    by = by
  )
  p_perf_perm <- prepare_performance_data(
    probs = list(p_vec[perm]),
    reals = list(r_vec[perm]),
    by = by
  )
  spec_perm <- rtichoke_viz_prediction_distribution_spec(
    p_dist_perm,
    p_perf_perm
  )
  expect_identical(spec1$rankBins, spec_perm$rankBins)
})

test_that("producer-owned performance values override naive bin reconstruction", {
  probs <- list(c(0.1, 0.2, 0.8))
  reals <- list(c(0, 1, 1))
  by <- 0.5

  p_dist <- prepare_probs_distribution_data(
    probs = list(probs[[1]]),
    reals = list(reals[[1]]),
    by = by
  )
  p_perf <- prepare_performance_data(
    probs = list(probs[[1]]),
    reals = list(reals[[1]]),
    by = by
  )

  # Deliberately modify p_perf TP value to 999 to test producer win
  p_perf$TP[p_perf$probability_threshold == 0.5] <- 999L

  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  op_50 <- Filter(function(op) op$value == 0.5, spec$operatingPoints)[[1]]
  tp_entry <- Filter(
    function(m) m$metricId == "true_positives",
    op_50$performance
  )[[1]]

  expect_equal(tp_entry$estimate, 999L)
})

test_that("operating-point performance alignment is 1-to-1 for threshold and PPCR modes", {
  probs <- list(c(0.00, 0.15, 0.30, 0.50, 0.50, 0.50, 0.65, 0.80, 1.00))
  reals <- list(c(0, 1, 0, 1, 0, 1, 1, 0, 1))
  by <- 0.20

  # Probability Threshold mode
  p_dist_thresh <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = "probability_threshold"
  )
  p_perf_thresh <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = "probability_threshold"
  )
  spec_thresh <- rtichoke_viz_prediction_distribution_spec(
    p_dist_thresh,
    p_perf_thresh
  )
  expect_length(spec_thresh$operatingPoints, 6L)

  # PPCR mode
  p_dist_ppcr <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = "ppcr"
  )
  p_perf_ppcr <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = "ppcr"
  )
  spec_ppcr <- rtichoke_viz_prediction_distribution_spec(
    p_dist_ppcr,
    p_perf_ppcr
  )
  expect_length(spec_ppcr$operatingPoints, 6L)
})

test_that("operating-point join error is raised if unmatched or duplicate performance row", {
  probs <- list(c(0.1, 0.9))
  reals <- list(c(0, 1))
  by <- 0.5

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by
  )
  p_perf <- prepare_performance_data(probs = probs, reals = reals, by = by)

  # Duplicate a row to force duplicate match error
  p_perf_dup <- rbind(p_perf, p_perf[1, ])
  expect_error(
    rtichoke_viz_prediction_distribution_spec(p_dist, p_perf_dup),
    "Operating point join failed"
  )

  # Remove a row to force missing-row unmatched error
  p_perf_missing <- p_perf[-1, , drop = FALSE]
  expect_error(
    rtichoke_viz_prediction_distribution_spec(p_dist, p_perf_missing),
    "Operating point join failed"
  )
})

test_that("canonical adapter handles multiple models sharing one population", {
  probs <- list("Model 1" = c(0.1, 0.9), "Model 2" = c(0.2, 0.8))
  reals <- list(c(0, 1))
  by <- 0.5

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by
  )
  p_perf <- prepare_performance_data(probs = probs, reals = reals, by = by)
  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  expect_length(spec$evaluations, 2L)
  expect_identical(
    spec$evaluations[[1]],
    list(id = "evaluation-1", population = "population", model = "Model 1")
  )
  expect_identical(
    spec$evaluations[[2]],
    list(id = "evaluation-2", population = "population", model = "Model 2")
  )

  # Verify 1-to-1 operating point alignment across multiple models
  eval_ids <- vapply(spec$operatingPoints, `[[`, character(1), "evaluationId")
  expect_equal(sum(eval_ids == "evaluation-1"), 3L)
  expect_equal(sum(eval_ids == "evaluation-2"), 3L)
  expect_true(validate_prediction_distribution_spec(spec))
})

test_that("canonical adapter handles multiple populations with unequal sample sizes", {
  probs <- list("Population 1" = c(0.1, 0.2, 0.9), "Population 2" = c(0.3, 0.7))
  reals <- list("Population 1" = c(0, 0, 1), "Population 2" = c(0, 1))
  by <- 0.5

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by
  )
  p_perf <- prepare_performance_data(probs = probs, reals = reals, by = by)
  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  expect_length(spec$evaluations, 2L)
  # Model field omitted when comparing populations without model names
  expect_identical(
    spec$evaluations[[1]],
    list(id = "evaluation-1", population = "Population 1")
  )
  expect_identical(
    spec$evaluations[[2]],
    list(id = "evaluation-2", population = "Population 2")
  )

  eval_ids <- vapply(spec$operatingPoints, `[[`, character(1), "evaluationId")
  expect_equal(sum(eval_ids == "evaluation-1"), 3L)
  expect_equal(sum(eval_ids == "evaluation-2"), 3L)
  expect_true(validate_prediction_distribution_spec(spec))
})

test_that("non-finite performance metric estimates serialize to NULL", {
  # At cutoff 1, PPV may be 0 / 0 = NaN
  probs <- list(c(0.1, 0.5))
  reals <- list(c(0, 0)) # No positives
  by <- 0.5

  p_dist <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by
  )
  p_perf <- prepare_performance_data(probs = probs, reals = reals, by = by)
  spec <- rtichoke_viz_prediction_distribution_spec(p_dist, p_perf)

  op_last <- spec$operatingPoints[[length(spec$operatingPoints)]]
  ppv_entry <- Filter(function(m) m$metricId == "ppv", op_last$performance)[[1]]

  expect_null(ppv_entry$estimate)

  json_str <- jsonlite::toJSON(
    spec,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  expect_match(json_str, '"metricId":"ppv","estimate":null', fixed = TRUE)
})

test_that("threshold regressions: cutoff zero, positive cutoff, ties, score one, multiple evaluations", {
  # Cutoff zero classifying exact-zero probability as positive
  p_dist0 <- prepare_probs_distribution_data(
    probs = list(c(0, 0.5)),
    reals = list(c(1, 1)),
    by = 0.5
  )
  p_perf0 <- prepare_performance_data(
    probs = list(c(0, 0.5)),
    reals = list(c(1, 1)),
    by = 0.5
  )
  spec0 <- rtichoke_viz_prediction_distribution_spec(p_dist0, p_perf0)

  op0 <- Filter(function(op) op$cutoff == 0, spec0$operatingPoints)[[1]]
  expect_equal(op0$realizedPpcr, 1)

  # Score 1 and ties
  p_dist_ties <- prepare_probs_distribution_data(
    probs = list(c(0.5, 0.5, 1.0)),
    reals = list(c(0, 1, 1)),
    by = 0.5
  )
  p_perf_ties <- prepare_performance_data(
    probs = list(c(0.5, 0.5, 1.0)),
    reals = list(c(0, 1, 1)),
    by = 0.5
  )
  spec_ties <- rtichoke_viz_prediction_distribution_spec(
    p_dist_ties,
    p_perf_ties
  )
  expect_true(validate_prediction_distribution_spec(spec_ties))
})

test_that("create_probs_histogram returns a standalone browser component", {
  histogram <- create_probs_histogram(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    by = 0.1
  )
  html <- as.character(histogram)

  expect_s3_class(histogram, "shiny.tag.list")
  expect_match(html, "renderPredictionDistribution", fixed = TRUE)
  expect_match(html, '"type":"prediction_distribution"', fixed = TRUE)
  expect_match(html, "URL.createObjectURL", fixed = TRUE)
  expect_length(htmltools::htmlDependencies(histogram), 0L)
})

test_that("prediction histogram browser dependencies save with standalone HTML", {
  output_dir <- tempfile("rtichoke-probs-histogram-")
  dir.create(output_dir)
  output_file <- file.path(output_dir, "probs-histogram.html")

  histogram <- create_probs_histogram(
    probs = list(c(0, 0.2, 0.5, 0.8, 1)),
    reals = list(c(0, 0, 1, 1, 1)),
    by = 0.25
  )
  htmltools::save_html(histogram, output_file)

  saved_html <- paste(readLines(output_file, warn = FALSE), collapse = "\n")
  expect_true(file.exists(output_file))
  expect_match(saved_html, "renderPredictionDistribution", fixed = TRUE)
  expect_match(saved_html, "URL.createObjectURL", fixed = TRUE)
  expect_match(saved_html, "rtichoke-prediction-distribution", fixed = TRUE)
  expect_false(dir.exists(file.path(output_dir, "lib")))
})
