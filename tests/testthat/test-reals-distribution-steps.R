test_that("create_reals_distribution_steps public API is exported and exists", {
  expect_true(exists(
    "create_reals_distribution_steps",
    mode = "function"
  ))
})

test_that("create_reals_distribution_steps returns shiny.tag.list for raw excluded mode", {
  df <- data.frame(
    id = rep(1:5, each = 3),
    step = rep(1:3, 5),
    reals = c(
      "A",
      "A",
      "B",
      "A",
      "B",
      "B",
      "A",
      "A",
      "A",
      "B",
      "B",
      "B",
      "A",
      "A",
      "B"
    )
  )

  res <- create_reals_distribution_steps(
    data = df,
    id_col = "id",
    step_col = "step",
    reals_col = "reals",
    censoring_heuristic = "excluded"
  )

  expect_s3_class(res, "shiny.tag.list")

  html <- as.character(res)
  expect_match(html, '"type":"outcome_distribution"', fixed = TRUE)
  expect_match(html, '"xAxisKind":"step"', fixed = TRUE)
  expect_match(html, '"estimateOrigin":"observed_step"', fixed = TRUE)
  expect_match(html, '"estimator":"raw"', fixed = TRUE)
  expect_match(html, '"stateScale":"nominal"', fixed = TRUE)
})

test_that("create_reals_distribution_steps returns shiny.tag.list for adjusted Aalen-Johansen mode", {
  df <- data.frame(
    id = rep(1:5, each = 3),
    step = rep(1:3, 5),
    reals = c(
      0,
      0,
      1,
      0,
      1,
      1,
      0,
      0,
      0,
      1,
      1,
      1,
      0,
      0,
      1
    )
  )

  res <- create_reals_distribution_steps(
    data = df,
    id_col = "id",
    step_col = "step",
    reals_col = "reals",
    censoring_heuristic = "adjusted"
  )

  expect_s3_class(res, "shiny.tag.list")

  html <- as.character(res)
  expect_match(html, '"type":"outcome_distribution"', fixed = TRUE)
  expect_match(html, '"xAxisKind":"step"', fixed = TRUE)
  expect_match(html, '"estimateOrigin":"observed_step"', fixed = TRUE)
  expect_match(html, '"estimator":"aalen_johansen"', fixed = TRUE)
  expect_match(html, '"stateScale":"ordinal"', fixed = TRUE)
})

test_that("input validation catches invalid arguments and data structures", {
  df <- data.frame(
    id = rep(1:3, each = 2),
    step = rep(1:2, 3),
    reals = c(0, 1, 0, 1, 1, 1)
  )

  # Non-dataframe
  expect_error(
    create_reals_distribution_steps(data = c(1, 2, 3)),
    "`data` must be a data frame or tibble"
  )

  # Missing column
  expect_error(
    create_reals_distribution_steps(data = df, reals_col = "missing_col"),
    "missing required column"
  )

  # Missing ID values
  df_na_id <- df
  df_na_id$id[1] <- NA
  expect_error(
    create_reals_distribution_steps(data = df_na_id),
    "`id_col` contains missing values"
  )

  # Invalid steps (negative, fractional, NA)
  df_neg_step <- df
  df_neg_step$step[1] <- -1
  expect_error(
    create_reals_distribution_steps(data = df_neg_step),
    "`step_col` must contain non-missing, non-negative, finite integer values"
  )

  df_frac_step <- df
  df_frac_step$step[1] <- 1.5
  expect_error(
    create_reals_distribution_steps(data = df_frac_step),
    "`step_col` must contain non-missing, non-negative, finite integer values"
  )

  # Duplicate id-step pairs
  df_dup <- rbind(df, df[1, ])
  expect_error(
    create_reals_distribution_steps(data = df_dup),
    "Each subject-step combination in `data` must be unique"
  )

  # Invalid censoring heuristic
  expect_error(
    create_reals_distribution_steps(
      data = df,
      censoring_heuristic = "invalid"
    ),
    '`censoring_heuristic` must be either "excluded" or "adjusted"'
  )

  # Invalid reals scale
  expect_error(
    create_reals_distribution_steps(data = df, reals_scale = "invalid"),
    '`reals_scale` must be "auto", "nominal", or "ordinal"'
  )

  # Unsupported renderer
  expect_error(
    create_reals_distribution_steps(data = df, renderer = "plotly"),
    'Only `renderer = "browser"` is supported'
  )
})

test_that("state scale and category ordering follow established semantics", {
  # Character -> nominal
  df_char <- data.frame(
    id = c(1, 1, 2, 2),
    step = c(1, 2, 1, 2),
    reals = c("Low", "High", "Low", "Low")
  )
  spec_char <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df_char,
    reals_scale = "auto"
  )
  expect_equal(spec_char$stateScale, "nominal")

  # Numeric -> ordinal
  df_num <- data.frame(
    id = c(1, 1, 2, 2),
    step = c(1, 2, 1, 2),
    reals = c(0, 1, 0, 0)
  )
  spec_num <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df_num,
    reals_scale = "auto"
  )
  expect_equal(spec_num$stateScale, "ordinal")

  # Explicit reals_scale overrides auto
  spec_override <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df_char,
    reals_scale = "ordinal"
  )
  expect_equal(spec_override$stateScale, "ordinal")

  # Explicit reals_order controls order and preserves unobserved states
  spec_order <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df_char,
    reals_order = c("High", "Medium", "Low")
  )
  state_ids <- vapply(spec_order$states, `[[`, character(1), "id")
  expect_equal(state_ids, c("High", "Medium", "Low"))
})

test_that("excluded raw mode computes exact state counts and excludes NAs", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    step = c(1, 2, 1, 2, 1, 2),
    reals = c("A", "A", "A", "B", "B", NA)
  )

  spec <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df,
    censoring_heuristic = "excluded"
  )

  expect_equal(length(spec$stateDistributions), 2) # steps 1 and 2

  # Step 1: 3 subjects observed (A=2, B=1)
  sd1 <- spec$stateDistributions[[1]]
  expect_equal(sd1$horizon, 1)
  counts1 <- vapply(sd1$states, `[[`, integer(1), "count")
  names(counts1) <- vapply(sd1$states, `[[`, character(1), "stateId")
  expect_equal(counts1[["A"]], 2L)
  expect_equal(counts1[["B"]], 1L)

  estimates1 <- vapply(sd1$states, `[[`, numeric(1), "estimate")
  names(estimates1) <- vapply(sd1$states, `[[`, character(1), "stateId")
  expect_equal(estimates1[["A"]], 2 / 3)
  expect_equal(estimates1[["B"]], 1 / 3)

  # Step 2: 2 subjects observed (1 NA excluded), A=1, B=1
  sd2 <- spec$stateDistributions[[2]]
  expect_equal(sd2$horizon, 2)
  counts2 <- vapply(sd2$states, `[[`, integer(1), "count")
  names(counts2) <- vapply(sd2$states, `[[`, character(1), "stateId")
  expect_equal(counts2[["A"]], 1L)
  expect_equal(counts2[["B"]], 1L)

  estimates2 <- vapply(sd2$states, `[[`, numeric(1), "estimate")
  names(estimates2) <- vapply(sd2$states, `[[`, character(1), "stateId")
  expect_equal(estimates2[["A"]], 0.5)
  expect_equal(estimates2[["B"]], 0.5)
})

test_that("adjusted Aalen-Johansen mode handles terminal dropout at midpoint censoring", {
  # Subj 1: full panel (A->A->B)
  # Subj 2: baseline-only (A at step 1, drops out before step 2)
  # Subj 3: drops out after step 2 (B->B at step 1 and 2, drops out before step 3)
  # Subj 4: full panel (A->B->B)
  df <- data.frame(
    id = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
    step = c(1, 2, 3, 1, 1, 2, 1, 2, 3),
    reals = c("A", "A", "B", "A", "B", "B", "A", "B", "B")
  )

  spec <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df,
    censoring_heuristic = "adjusted"
  )

  expect_equal(length(spec$stateDistributions), 3)

  # Baseline p0: 3 in A, 1 in B -> p(A)=0.75, p(B)=0.25
  sd1 <- spec$stateDistributions[[1]]
  p1 <- vapply(sd1$states, `[[`, numeric(1), "estimate")
  names(p1) <- vapply(sd1$states, `[[`, character(1), "stateId")
  expect_equal(p1[["A"]], 0.75)
  expect_equal(p1[["B"]], 0.25)

  # Step 2: Subj 2 is censored at t=1.5 (midpoint between step 1 and 2).
  # At step 2, risk set is 3 subjects (Subj 1 in A, Subj 3 in B, Subj 4 in A).
  # Subj 4 moves A->B (1 event out of 2 in A -> P(A->B)=1/2).
  # P(A, t=2) = 0.75 * (1/2) = 0.375.
  # P(B, t=2) = 0.25 + 0.75 * (1/2) = 0.625.
  sd2 <- spec$stateDistributions[[2]]
  p2 <- vapply(sd2$states, `[[`, numeric(1), "estimate")
  names(p2) <- vapply(sd2$states, `[[`, character(1), "stateId")
  expect_equal(p2[["A"]], 0.375)
  expect_equal(p2[["B"]], 0.625)

  # Step 3: Subj 3 is censored at t=2.5 (midpoint between step 2 and 3).
  # At step 3, risk set is 2 subjects (Subj 1 in A, Subj 4 in B).
  # Subj 1 moves A->B (1 event out of 1 in A -> P(A->B)=1.0).
  # P(A, t=3) = 0, P(B, t=3) = 1.0.
  sd3 <- spec$stateDistributions[[3]]
  p3 <- vapply(sd3$states, `[[`, numeric(1), "estimate")
  names(p3) <- vapply(sd3$states, `[[`, character(1), "stateId")
  expect_equal(p3[["A"]], 0.0)
  expect_equal(p3[["B"]], 1.0)
})

test_that("adjusted mode handles heterogeneous baseline and terminal dropout regression fixture", {
  # Exact dataset from review comment:
  # id=c(1,1,1,2,2,2,3,3,4,4,4), step=c(1,2,3,1,2,3,1,2,1,2,3), reals=c('A','A','B','A','B','B','B','B','A','A','A')
  df_regr <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4),
    step = c(1, 2, 3, 1, 2, 3, 1, 2, 1, 2, 3),
    reals = c("A", "A", "B", "A", "B", "B", "B", "B", "A", "A", "A")
  )

  spec <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df_regr,
    censoring_heuristic = "adjusted"
  )

  expect_equal(length(spec$stateDistributions), 3)

  # Step 1: 3 in A, 1 in B -> P(A)=0.75, P(B)=0.25
  p1 <- vapply(
    spec$stateDistributions[[1]]$states,
    `[[`,
    numeric(1),
    "estimate"
  )
  names(p1) <- vapply(
    spec$stateDistributions[[1]]$states,
    `[[`,
    character(1),
    "stateId"
  )
  expect_equal(p1[["A"]], 0.75)
  expect_equal(p1[["B"]], 0.25)
  expect_equal(p1[["A"]] + p1[["B"]], 1.0)

  # Step 2: Subj 2 moves A->B, Subj 3 is in B, Subj 1 & 4 in A -> P(A)=0.50, P(B)=0.50
  p2 <- vapply(
    spec$stateDistributions[[2]]$states,
    `[[`,
    numeric(1),
    "estimate"
  )
  names(p2) <- vapply(
    spec$stateDistributions[[2]]$states,
    `[[`,
    character(1),
    "stateId"
  )
  expect_equal(p2[["A"]], 0.50)
  expect_equal(p2[["B"]], 0.50)
  expect_equal(p2[["A"]] + p2[["B"]], 1.0)

  # Step 3: Subj 3 midpoint censored at 2.5. Subj 1 moves A->B, Subj 4 stays in A -> P(A)=0.25, P(B)=0.75
  p3 <- vapply(
    spec$stateDistributions[[3]]$states,
    `[[`,
    numeric(1),
    "estimate"
  )
  names(p3) <- vapply(
    spec$stateDistributions[[3]]$states,
    `[[`,
    character(1),
    "stateId"
  )
  expect_equal(p3[["A"]], 0.25)
  expect_equal(p3[["B"]], 0.75)
  expect_equal(p3[["A"]] + p3[["B"]], 1.0)
})

test_that("adjusted mode handles irregular global step spacing with midpoint censoring", {
  # Irregular global steps: c(1, 5, 10)
  df_irreg <- data.frame(
    id = c(1, 1, 1, 2, 3, 3),
    step = c(1, 5, 10, 1, 1, 5),
    reals = c("A", "A", "B", "A", "B", "B")
  )

  spec <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df_irreg,
    censoring_heuristic = "adjusted"
  )

  expect_equal(length(spec$stateDistributions), 3)

  # Independent survival::survfit calculation on exact counting-process intervals
  all_categories <- c("A", "B")
  all_steps <- c(1, 5, 10)
  min_step <- 1
  p0 <- c("A" = 2 / 3, "B" = 1 / 3)

  cp_df <- data.frame(
    id = c("1", "1", "2", "3", "3"),
    tstart = c(1, 5, 1, 1, 5),
    tstop = c(5, 10, 3.0, 5, 7.5),
    istate = factor(c("A", "A", "A", "B", "B"), levels = all_categories),
    event = factor(
      c(
        "censor_no_event",
        "B",
        "censor_no_event",
        "censor_no_event",
        "censor_no_event"
      ),
      levels = c("censor_no_event", all_categories)
    )
  )

  fit <- survival::survfit(
    survival::Surv(tstart, tstop, event) ~ 1,
    data = cp_df,
    id = id,
    istate = istate,
    p0 = p0
  )

  fit_times <- fit$time
  fit_states <- fit$states
  fit_pstate <- fit$pstate

  if (!is.matrix(fit_pstate)) {
    fit_pstate <- matrix(
      fit_pstate,
      nrow = length(fit_times),
      ncol = length(fit_states),
      byrow = FALSE
    )
  }
  colnames(fit_pstate) <- fit_states

  for (i in seq_along(all_steps)) {
    s <- all_steps[i]
    sd <- spec$stateDistributions[[i]]
    expect_equal(sd$horizon, s)

    p_spec <- vapply(sd$states, `[[`, numeric(1), "estimate")
    names(p_spec) <- vapply(sd$states, `[[`, character(1), "stateId")

    if (s == min_step) {
      vals <- p0
    } else {
      prior_indices <- which(fit_times <= s)
      if (length(prior_indices) == 0L) {
        vals <- p0
      } else {
        row_idx <- max(prior_indices)
        vals <- fit_pstate[row_idx, ]
      }
    }

    expect_equal(p_spec[["A"]], unname(vals["A"]))
    expect_equal(p_spec[["B"]], unname(vals["B"]))
  }
})

test_that("adjusted mode throws explicit error when subject observation gaps or NA intermediate rows exist", {
  df_gap <- data.frame(
    id = c(1, 1, 2, 2, 2),
    step = c(1, 3, 1, 2, 3),
    reals = c("A", "B", "A", "A", "B")
  )

  expect_error(
    create_reals_distribution_steps(
      data = df_gap,
      censoring_heuristic = "adjusted"
    ),
    "Observation gaps or missing intermediate steps per subject are not supported in adjusted mode"
  )

  df_na_middle <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    step = c(1, 2, 3, 1, 2, 3),
    reals = c("A", NA, "B", "A", "A", "B")
  )

  expect_error(
    create_reals_distribution_steps(
      data = df_na_middle,
      censoring_heuristic = "adjusted"
    ),
    "Observation gaps or missing intermediate steps per subject are not supported in adjusted mode"
  )
})

test_that("Aalen-Johansen handles multi-state reversible transitions", {
  df <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    step = c(1, 2, 3, 1, 2, 3),
    reals = c("A", "B", "A", "B", "A", "B")
  )

  spec <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = df,
    censoring_heuristic = "adjusted"
  )

  expect_equal(length(spec$stateDistributions), 3)

  p1 <- vapply(
    spec$stateDistributions[[1]]$states,
    `[[`,
    numeric(1),
    "estimate"
  )
  names(p1) <- vapply(
    spec$stateDistributions[[1]]$states,
    `[[`,
    character(1),
    "stateId"
  )
  expect_equal(p1[["A"]], 0.5)
  expect_equal(p1[["B"]], 0.5)

  p2 <- vapply(
    spec$stateDistributions[[2]]$states,
    `[[`,
    numeric(1),
    "estimate"
  )
  names(p2) <- vapply(
    spec$stateDistributions[[2]]$states,
    `[[`,
    character(1),
    "stateId"
  )
  expect_equal(p2[["A"]], 0.5)
  expect_equal(p2[["B"]], 0.5)
})

test_that("only supplied steps are present and step 0 is not manufactured", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    step = c(5, 10, 5, 10),
    reals = c("A", "B", "A", "A")
  )

  spec <- rtichoke:::rtichoke_viz_outcome_distribution_steps_v2_spec(data = df)

  horizons <- vapply(spec$stateDistributions, `[[`, numeric(1), "horizon")
  expect_equal(horizons, c(5, 10))
  expect_equal(spec$xAxis$domain, c(5, 10))
})
