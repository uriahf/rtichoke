test_that("prepare_continuous_reals_distribution_steps_data exists and is exported internally", {
  expect_true(exists(
    "prepare_continuous_reals_distribution_steps_data",
    where = asNamespace("rtichoke"),
    mode = "function"
  ))
})

test_that("1. Complete longitudinal panel computes exact equal-width histogram counts and proportions", {
  df <- data.frame(
    id = rep(1:5, each = 3),
    step = rep(1:3, 5),
    reals = c(
      10.0,
      12.0,
      15.0, # subj 1
      11.0,
      13.0,
      16.0, # subj 2
      20.0,
      22.0,
      25.0, # subj 3
      21.0,
      23.0,
      26.0, # subj 4
      30.0,
      32.0,
      35.0 # subj 5
    )
  )

  df_copy <- df

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    id_col = "id",
    step_col = "step",
    reals_col = "reals",
    bins = 5
  )

  expect_equal(df, df_copy) # Input data invariance
  expect_equal(res$n_reference_subjects, 5L)
  expect_equal(res$steps, c(1, 2, 3))
  expect_equal(length(res$bin_edges), 6L)
  expect_equal(res$bin_edges[1], 10.0)
  expect_equal(res$bin_edges[6], 35.0)

  expect_equal(dim(res$counts), c(3L, 5L))
  expect_equal(dim(res$proportions), c(3L, 5L))
  expect_equal(res$observed_denominators, c("1" = 5L, "2" = 5L, "3" = 5L))
  expect_equal(res$unobserved_counts, c("1" = 0L, "2" = 0L, "3" = 0L))

  for (s_idx in seq_along(res$steps)) {
    expect_equal(
      sum(res$counts[s_idx, ]),
      unname(res$observed_denominators[s_idx])
    )
    expect_equal(sum(res$proportions[s_idx, ]), 1.0, tolerance = 1e-12)
    expect_equal(
      unname(res$observed_denominators[s_idx] + res$unobserved_counts[s_idx]),
      res$n_reference_subjects
    )
  }
})

test_that("2. Missing outcome values are excluded from denominators and histogram counts", {
  df <- data.frame(
    id = rep(1:4, each = 2),
    step = rep(1:2, 4),
    reals = c(
      10.0,
      20.0,
      12.0,
      NA,
      14.0,
      22.0,
      NaN,
      24.0
    )
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = 2
  )

  expect_equal(res$n_reference_subjects, 4L)
  expect_equal(res$observed_denominators, c("1" = 3L, "2" = 3L))
  expect_equal(res$unobserved_counts, c("1" = 1L, "2" = 1L))

  # Step 1: 10, 12, 14 (3 observed)
  expect_equal(sum(res$counts["1", ]), 3L)
  expect_equal(sum(res$proportions["1", ]), 1.0)

  # Step 2: 20, 22, 24 (3 observed)
  expect_equal(sum(res$counts["2", ]), 3L)
  expect_equal(sum(res$proportions["2", ]), 1.0)
})

test_that("3. Absent subject-step rows are tracked in unobserved_counts", {
  # Subj 1 has steps 1, 2. Subj 2 has step 1 only. Subj 3 has step 2 only.
  df <- data.frame(
    id = c(1, 1, 2, 3),
    step = c(1, 2, 1, 2),
    reals = c(10.0, 15.0, 12.0, 18.0)
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = 2
  )

  expect_equal(res$n_reference_subjects, 3L)
  expect_equal(res$observed_denominators, c("1" = 2L, "2" = 2L))
  expect_equal(res$unobserved_counts, c("1" = 1L, "2" = 1L))
})

test_that("4. Nonconsecutive steps and 5. No step zero are preserved as supplied", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    step = c(5, 15, 5, 15),
    reals = c(10.0, 20.0, 12.0, 22.0)
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = 2
  )

  expect_equal(res$steps, c(5, 15))
  expect_equal(rownames(res$counts), c("5", "15"))
})

test_that("6. Empty bins are preserved in matrix columns", {
  # All outcomes are concentrated in bin 1 or 3, leaving bin 2 empty
  df <- data.frame(
    id = c(1, 2, 3, 4),
    step = c(1, 1, 1, 1),
    reals = c(10.0, 10.5, 29.5, 30.0)
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = 3
  )

  expect_equal(ncol(res$counts), 3L)
  expect_equal(res$counts["1", 2L], 0L)
  expect_equal(res$proportions["1", 2L], 0.0)
})

test_that("7. Values exactly on bin boundaries follow left-closed, right-open rules with right-closed endpoint", {
  # Bin edges: c(10, 20, 30)
  # Bin 1: [10, 20) -> 10, 15
  # Bin 2: [20, 30] -> 20, 25, 30
  df <- data.frame(
    id = 1:5,
    step = rep(1, 5),
    reals = c(10.0, 15.0, 20.0, 25.0, 30.0)
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = c(10.0, 20.0, 30.0)
  )

  expect_equal(res$bin_edges, c(10.0, 20.0, 30.0))
  expect_equal(res$counts["1", 1L], 2L) # 10, 15
  expect_equal(res$counts["1", 2L], 3L) # 20, 25, 30
})

test_that("8. Constant-valued outcomes construct [v - 0.5, v + 0.5] bin boundaries", {
  df <- data.frame(
    id = 1:4,
    step = rep(1, 4),
    reals = c(15.0, 15.0, 15.0, 15.0)
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = 4
  )

  expect_equal(res$bin_edges, seq(14.5, 15.5, length.out = 5L))
  expect_equal(sum(res$counts["1", ]), 4L)
  expect_equal(sum(res$proportions["1", ]), 1.0)
})

test_that("9. All outcomes missing at one supplied step sets proportions to NA_real_", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    step = c(1, 2, 1, 2),
    reals = c(10.0, NA, 12.0, NA)
  )

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = 2
  )

  expect_equal(res$observed_denominators, c("1" = 2L, "2" = 0L))
  expect_equal(res$unobserved_counts, c("1" = 0L, "2" = 2L))

  # Step 1: positive denominator -> valid proportions
  expect_equal(sum(res$proportions["1", ]), 1.0)

  # Step 2: 0 denominator -> NA_real_ proportions
  expect_true(all(is.na(res$proportions["2", ])))
  expect_equal(sum(res$counts["2", ]), 0L)
})

test_that("10. Input validation rejects duplicate subject-step, infinite outcomes, and invalid bins", {
  df_valid <- data.frame(
    id = c(1, 2),
    step = c(1, 1),
    reals = c(10.0, 20.0)
  )

  # Duplicate (subject, step)
  df_dup <- data.frame(
    id = c(1, 1),
    step = c(1, 1),
    reals = c(10.0, 20.0)
  )
  expect_error(
    rtichoke:::prepare_continuous_reals_distribution_steps_data(df_dup),
    "Each subject-step combination in `data` must be unique"
  )

  # Infinite outcomes
  df_inf <- data.frame(
    id = c(1, 2),
    step = c(1, 1),
    reals = c(10.0, Inf)
  )
  expect_error(
    rtichoke:::prepare_continuous_reals_distribution_steps_data(df_inf),
    "`reals_col` contains infinite values"
  )

  # Fractional scalar bins
  expect_error(
    rtichoke:::prepare_continuous_reals_distribution_steps_data(
      df_valid,
      bins = 10.5
    ),
    "Scalar `bins` must be a single positive integer"
  )

  # Non-strictly increasing explicit edges
  expect_error(
    rtichoke:::prepare_continuous_reals_distribution_steps_data(
      df_valid,
      bins = c(10, 10, 20)
    ),
    "`bins` vector must be strictly increasing"
  )

  # All outcomes missing
  df_all_na <- data.frame(
    id = c(1, 2),
    step = c(1, 1),
    reals = c(NA_real_, NA_real_)
  )
  expect_error(
    rtichoke:::prepare_continuous_reals_distribution_steps_data(df_all_na),
    "All outcomes in `data` are missing"
  )
})

test_that("11. Out-of-range values with explicit edges are rejected", {
  df <- data.frame(
    id = c(1, 2, 3),
    step = c(1, 1, 1),
    reals = c(5.0, 15.0, 25.0)
  )

  # Range c(10, 20) excludes 5.0 and 25.0
  expect_error(
    rtichoke:::prepare_continuous_reals_distribution_steps_data(
      df,
      bins = c(10.0, 20.0)
    ),
    "Observed outcomes fall outside the explicit `bins` range"
  )
})

test_that("12. Row-order invariance: shuffling rows produces identical output", {
  df1 <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    step = c(1, 2, 1, 2, 1, 2),
    reals = c(10.0, 20.0, 12.0, 22.0, 14.0, 24.0)
  )

  set.seed(42)
  df2 <- df1[sample(nrow(df1)), ]

  res1 <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    df1,
    bins = 3
  )
  res2 <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    df2,
    bins = 3
  )

  expect_equal(res1, res2)
})

test_that("Verification against base R histogram calculations with aligned bin edges", {
  set.seed(123)
  df <- data.frame(
    id = rep(1:20, each = 3),
    step = rep(1:3, 20),
    reals = rnorm(60, mean = 100, sd = 15)
  )

  edges <- c(50.0, 75.0, 100.0, 125.0, 150.0)

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = df,
    bins = edges
  )

  # Check each step against base R hist()
  for (s_val in 1:3) {
    step_obs <- df$reals[df$step == s_val]
    h_base <- hist(
      step_obs,
      breaks = edges,
      plot = FALSE,
      include.lowest = TRUE,
      right = FALSE
    )

    s_str <- as.character(s_val)
    expect_equal(as.numeric(res$counts[s_str, ]), as.numeric(h_base$counts))
  }
})

test_that("Oxboys reference dataset fixture verification", {
  ox_fixture_path <- test_path("fixtures", "oxboys_fixture.rds")
  expect_true(file.exists(ox_fixture_path))

  ox_df <- readRDS(ox_fixture_path)

  # Oxboys has 26 subjects and 9 occasions (steps)
  expect_equal(nrow(ox_df), 234L)
  expect_equal(length(unique(ox_df$Subject)), 26L)
  expect_equal(length(unique(ox_df$Occasion)), 9L)

  res <- rtichoke:::prepare_continuous_reals_distribution_steps_data(
    data = ox_df,
    id_col = "Subject",
    step_col = "Occasion",
    reals_col = "height",
    bins = 10
  )

  expect_equal(res$n_reference_subjects, 26L)
  expect_equal(length(res$steps), 9L)
  expect_equal(res$steps, 1:9)
  expect_equal(length(res$bin_edges), 11L)

  # Complete follow-up: 26 observed at each occasion
  expect_equal(unname(res$observed_denominators), rep(26L, 9))
  expect_equal(unname(res$unobserved_counts), rep(0L, 9))

  for (s_idx in 1:9) {
    expect_equal(sum(res$counts[s_idx, ]), 26L)
    expect_equal(sum(res$proportions[s_idx, ]), 1.0, tolerance = 1e-12)
  }
})
