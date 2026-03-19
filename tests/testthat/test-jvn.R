# Test file for jvn.R
# Tests for jvn_nowcast and related functions
#' @srrstats {G5.2} Error and warning behavior tested
#' @srrstats {G5.2a} Every error message is unique
#' @srrstats {G5.2b} Tests demonstrate conditions triggering messages
#' @srrstats {G5.4} Correctness tests against fixed test data
#' @srrstats {G5.5} Tests run with fixed random seed
#' @srrstats {G5.6} Parameter recovery tests

# ===== Setup Test Data =====

# Create synthetic vintage data for testing
set.seed(123)
n_obs <- 30
n_vint <- 4 # Increased to 4 vintages to support tests with e = 2

# Generate true values following AR(2) process
ar_coefs <- c(0.5, 0.3)
true_values <- numeric(n_obs)
true_values[1:2] <- rnorm(2)

for (t in 3:n_obs) {
  true_values[t] <- ar_coefs[1] *
    true_values[t - 1] +
    ar_coefs[2] * true_values[t - 2] +
    rnorm(1, 0, 0.5)
}

# Create vintages with news and noise
vintages <- matrix(NA, n_obs, n_vint)
colnames(vintages) <- paste0("release_", 0:(n_vint - 1))

# Add news component (cumulative)
news <- matrix(rnorm(n_obs * n_vint, 0, 0.2), n_obs, n_vint)
news <- t(apply(news, 1, cumsum))

# Add noise component (transitory)
noise <- matrix(rnorm(n_obs * n_vint, 0, 0.1), n_obs, n_vint)

# Combine
for (v in 1:n_vint) {
  vintages[, v] <- true_values + news[, v] + noise[, v]
}

# Create data frame
df_test <- data.frame(
  time = seq.Date(
    from = as.Date("2020-01-01"),
    by = "quarter",
    length.out = n_obs
  ),
  vintages
)

# Create wide format version
df_test_wide <- df_test

# Create long format version
df_test_long <- tidyr::pivot_longer(
  df_test,
  cols = -time,
  names_to = "release",
  values_to = "value"
)

jvn_fit <- function(key, ..., solver_options = list(trace = 0)) {
  args <- list(...)
  args$solver_options <- modifyList(list(trace = 0), solver_options)

  cached_fixture(
    paste0("jvn-", key),
    function() do.call(jvn_nowcast, args)
  )
}

fit_jvn_long <- function() {
  jvn_fit(
    "long",
    df = df_test_long,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(return_states = FALSE)
  )
}

fit_jvn_news_fast <- function() {
  jvn_fit(
    "news-fast",
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(return_states = FALSE)
  )
}

fit_jvn_noise_fast <- function() {
  jvn_fit(
    "noise-fast",
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = FALSE,
    include_noise = TRUE,
    solver_options = list(return_states = FALSE)
  )
}

fit_jvn_ar3_fast <- function() {
  jvn_fit(
    "ar3-fast",
    df = df_test_wide,
    e = 2,
    ar_order = 3,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(return_states = FALSE)
  )
}

fit_jvn_stateful <- function() {
  jvn_fit(
    "stateful",
    df = df_test_wide,
    e = 2,
    ar_order = 2,
    h = 4,
    include_news = TRUE,
    include_noise = TRUE
  )
}

fit_jvn_spillovers_full <- function() {
  jvn_fit(
    "spillovers-full",
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = TRUE,
    include_spillovers = TRUE,
    spillover_news = TRUE,
    spillover_noise = TRUE,
    solver_options = list(return_states = FALSE)
  )
}

fit_jvn_spillovers_news_only <- function() {
  jvn_fit(
    "spillovers-news",
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = TRUE,
    include_spillovers = TRUE,
    spillover_news = TRUE,
    spillover_noise = FALSE,
    solver_options = list(return_states = FALSE)
  )
}

# ===== Tests for jvn_nowcast =====

test_that("jvn_nowcast returns correct structure", {
  result <- fit_jvn_stateful()

  expect_s3_class(result, "jvn_model")
  expect_true("states" %in% names(result))
  expect_true("params" %in% names(result))
  expect_true("loglik" %in% names(result))
  expect_true("aic" %in% names(result))
  expect_true("bic" %in% names(result))
  expect_true("convergence" %in% names(result))
})

test_that("jvn_nowcast handles long format data", {
  result <- fit_jvn_long()

  expect_s3_class(result, "jvn_model")
  expect_true(nrow(result$params) > 0)
})

test_that("jvn_nowcast validates e parameter", {
  expect_error(
    jvn_nowcast(df = df_test_wide, e = 0, ar_order = 1),
    "must be a single number > 0"
  )

  expect_error(
    jvn_nowcast(df = df_test_wide, e = 5, ar_order = 1),
    "Not enough vintage columns"
  )
})

test_that("jvn_nowcast validates ar_order parameter", {
  expect_error(
    jvn_nowcast(df = df_test_wide, e = 1, ar_order = 0),
    "must be a single number > 0"
  )
})

test_that("jvn_init_params falls back to finite defaults for short samples", {
  model_struct <- reviser:::jvn_matrices(
    n_obs = 3,
    n_vint = 2,
    ar_order = 1,
    include_news = TRUE,
    include_noise = TRUE,
    include_spillovers = FALSE,
    spillover_news = TRUE,
    spillover_noise = TRUE
  )
  y_short <- matrix(
    c(1, 2, 3, 1.1, 2.1, 3.1),
    ncol = 2
  )

  expect_warning(
    params <- reviser:::jvn_init_params(model_struct, y_short),
    "Insufficient data for smart initialization"
  )
  expect_length(params, model_struct$n_params)
  expect_true(all(is.finite(params)))
})

test_that("jvn_nowcast validates h parameter", {
  expect_error(
    jvn_nowcast(df = df_test_wide, e = 1, ar_order = 1, h = -1),
    "must be a single number >= 0"
  )
})

test_that("jvn_nowcast validates solver_options parameter", {
  expect_error(
    jvn_nowcast(
      df = df_test_wide,
      e = 1,
      ar_order = 1,
      solver_options = "not_a_list"
    ),
    "must be a list"
  )
})

test_that("jvn_nowcast validates solver_options names", {
  expect_error(
    jvn_nowcast(
      df = df_test_wide,
      e = 1,
      ar_order = 1,
      solver_options = list(invalid_option = 123)
    ),
    "Invalid `solver_options`"
  )
})

test_that("jvn_nowcast handles news-only model", {
  result <- fit_jvn_news_fast()

  expect_s3_class(result, "jvn_model")
  expect_true(all(
    grepl("sigma_nu", result$params$Parameter) |
      grepl("rho|sigma_e", result$params$Parameter)
  ))
})

test_that("jvn_nowcast handles noise-only model", {
  result <- fit_jvn_noise_fast()

  expect_s3_class(result, "jvn_model")
  expect_true(all(
    grepl("sigma_zeta", result$params$Parameter) |
      grepl("rho|sigma_e", result$params$Parameter)
  ))
})

test_that("jvn_nowcast rejects models without news and noise", {
  expect_error(
    jvn_nowcast(
      df = df_test_wide,
      e = 2,
      ar_order = 1,
      h = 0,
      include_news = FALSE,
      include_noise = FALSE,
      solver_options = list(trace = 0)
    ),
    "At least one of `include_news` or `include_noise` must be TRUE"
  )
})

test_that("jvn_nowcast handles different AR orders", {
  result_ar1 <- fit_jvn_news_fast()
  result_ar3 <- fit_jvn_ar3_fast()

  expect_equal(sum(grepl("^rho_", result_ar1$params$Parameter)), 1)
  expect_equal(sum(grepl("^rho_", result_ar3$params$Parameter)), 3)
})

test_that("jvn_nowcast produces forecasts when h > 0", {
  result <- fit_jvn_stateful()

  expect_s3_class(result, "jvn_model")

  # Check that out-of-sample forecasts exist
  oos_data <- result$states[result$states$sample == "out_of_sample", ]
  expect_gt(nrow(oos_data), 0)
  expect_equal(length(unique(oos_data$time)), 4)
})

test_that("jvn_nowcast handles spillovers", {
  result <- fit_jvn_spillovers_full()

  expect_s3_class(result, "jvn_model")
  expect_true(any(grepl("^T_nu_", result$params$Parameter)))
  expect_true(any(grepl("^T_zeta_", result$params$Parameter)))
})

test_that("jvn_nowcast handles spillovers for news only", {
  result <- fit_jvn_spillovers_news_only()

  expect_s3_class(result, "jvn_model")
  expect_true(any(grepl("^T_nu_", result$params$Parameter)))
  expect_false(any(grepl("^T_zeta_", result$params$Parameter)))
})

test_that("jvn_nowcast optimization methods work", {
  skip_if_not_reviser_full_tests()
  methods <- c("L-BFGS-B", "BFGS", "Nelder-Mead", "nlminb")

  for (meth in methods) {
    result <- jvn_nowcast(
      df = df_test_wide,
      e = 2,
      ar_order = 1,
      h = 0,
      include_news = TRUE,
      include_noise = FALSE,
      solver_options = list(
        trace = 0,
        method = meth,
        maxiter = 60,
        return_states = FALSE
      )
    )

    expect_s3_class(result, "jvn_model")
  }
})

test_that("jvn_nowcast two-step method works", {
  skip_if_not_reviser_full_tests()
  result <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      method = "two-step",
      maxiter = 60,
      return_states = FALSE
    )
  )

  expect_s3_class(result, "jvn_model")
})

test_that("jvn_nowcast multi-start optimization works", {
  skip_if_not_reviser_full_tests()
  result <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      n_starts = 2,
      maxiter = 60,
      return_states = FALSE
    )
  )

  expect_s3_class(result, "jvn_model")
})

test_that("jvn_nowcast handles custom starting values", {
  skip_if_not_reviser_full_tests()
  # Get default parameter count
  temp_result <- fit_jvn_news_fast()

  n_params <- nrow(temp_result$params)
  custom_start <- rep(0.1, n_params)

  result <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      startvals = custom_start,
      maxiter = 60,
      return_states = FALSE
    )
  )

  expect_s3_class(result, "jvn_model")
})

test_that("jvn_nowcast validates custom starting values length", {
  expect_error(
    jvn_nowcast(
      df = df_test_wide,
      e = 2,
      ar_order = 1,
      h = 0,
      include_news = TRUE,
      include_noise = FALSE,
      solver_options = list(startvals = c(0.1, 0.2))
    ),
    "`startvals` must have length"
  )
})

test_that("jvn_nowcast transform_se option works", {
  skip_if_not_reviser_full_tests()
  result_transform <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      transform_se = TRUE,
      return_states = FALSE
    )
  )

  result_no_transform <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      transform_se = FALSE,
      return_states = FALSE
    )
  )

  expect_s3_class(result_transform, "jvn_model")
  expect_s3_class(result_no_transform, "jvn_model")
})

test_that("jvn_nowcast supports se_method none and qml", {
  skip_if_not_reviser_full_tests()
  result_none <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      se_method = "none",
      return_states = FALSE
    )
  )

  result_qml <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      se_method = "qml",
      return_states = FALSE
    )
  )

  expect_true(all(is.na(result_none$params$Std.Error)))
  expect_true(any(is.finite(result_qml$params$Std.Error)))
})

test_that("jvn_nowcast can skip state extraction", {
  result <- fit_jvn_news_fast()

  expect_null(result$states)
})

test_that("jvn_nowcast states output has correct structure", {
  result <- fit_jvn_stateful()

  states <- result$states

  expect_true("time" %in% colnames(states))
  expect_true("state" %in% colnames(states))
  expect_true("estimate" %in% colnames(states))
  expect_true("lower" %in% colnames(states))
  expect_true("upper" %in% colnames(states))
  expect_true("filter" %in% colnames(states))
  expect_true("sample" %in% colnames(states))

  expect_true(all(states$filter %in% c("filtered", "smoothed")))
  expect_true(all(states$sample %in% c("in_sample", "out_of_sample")))
})

test_that("jvn_nowcast parameter estimates are finite", {
  result <- fit_jvn_stateful()

  expect_true(all(is.finite(result$params$Estimate)))
})

test_that("jvn_nowcast information criteria are calculated", {
  result <- fit_jvn_news_fast()

  expect_true(is.finite(result$aic))
  expect_true(is.finite(result$bic))
  expect_true(is.finite(result$loglik))
  expect_gt(result$aic, 0)
  expect_gt(result$bic, 0)
})

test_that("jvn_nowcast handles irregular time series error", {
  # Create truly irregular time series by mixing monthly and quarterly spacing
  # This will cause the frequency check to fail when h > 0
  df_irregular <- df_test_wide
  # Change spacing: first part quarterly, second part monthly
  df_irregular$time <- c(
    seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = n_obs / 2),
    seq.Date(as.Date("2024-01-01"), by = "month", length.out = n_obs / 2)
  )

  suppressWarnings(
    expect_error(
      jvn_nowcast(
        df = df_irregular,
        e = 2,
        ar_order = 1,
        h = 2,
        include_news = TRUE,
        include_noise = FALSE,
        solver_options = list(trace = 0)
      ),
      "Time index appears irregular"
    )
  )
})

# ===== Tests for summary.jvn_model =====

test_that("summary.jvn_model produces output", {
  result <- fit_jvn_news_fast()

  # Capture output
  output <- utils::capture.output(summary(result))

  expect_gt(length(output), 0)
  expect_true(any(grepl("Jacobs-Van Norden Model", output)))
  expect_true(any(grepl("Convergence", output)))
  expect_true(any(grepl("Log-likelihood", output)))
  expect_true(any(grepl("AIC", output)))
  expect_true(any(grepl("BIC", output)))
})

test_that("summary.jvn_model returns invisibly", {
  result <- fit_jvn_news_fast()

  returned <- summary(result)
  expect_identical(returned, result)
})

# ===== Tests for print.jvn_model =====

test_that("print.jvn_model produces output", {
  result <- fit_jvn_news_fast()

  output <- utils::capture.output(print(result))

  expect_gt(length(output), 0)
  expect_true(any(grepl("Jacobs-Van Norden Model", output)))
})

test_that("print.jvn_model returns invisibly", {
  result <- fit_jvn_news_fast()

  returned <- print(result)
  expect_identical(returned, result)
})

# ===== Tests for plot.jvn_model =====

test_that("plot.jvn_model returns ggplot object", {
  result <- fit_jvn_stateful()

  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot.jvn_model handles different states", {
  result <- fit_jvn_stateful()

  # Plot true_lag_0
  p1 <- plot(result, state = "true_lag_0")
  expect_s3_class(p1, "ggplot")

  # Plot news state
  p2 <- plot(result, state = "news_vint1")
  expect_s3_class(p2, "ggplot")

  # Plot noise state
  p3 <- plot(result, state = "noise_vint1")
  expect_s3_class(p3, "ggplot")
})

test_that("plot.jvn_model handles filtered vs smoothed", {
  result <- fit_jvn_stateful()

  p_filtered <- plot(result, type = "filtered")
  p_smoothed <- plot(result, type = "smoothed")

  expect_s3_class(p_filtered, "ggplot")
  expect_s3_class(p_smoothed, "ggplot")
})

# ===== Integration Tests =====

test_that("full workflow with all components", {
  result <- fit_jvn_stateful()

  # Test structure
  expect_s3_class(result, "jvn_model")

  # Test methods work
  output_summary <- utils::capture.output(summary(result))
  expect_gt(length(output_summary), 0)

  output_print <- utils::capture.output(print(result))
  expect_gt(length(output_print), 0)

  p <- plot(result)
  expect_s3_class(p, "ggplot")

  # Test convergence
  expect_true(result$convergence %in% c(0, 1))

  # Test parameter table structure
  expect_true("Parameter" %in% colnames(result$params))
  expect_true("Estimate" %in% colnames(result$params))
  expect_true("Std.Error" %in% colnames(result$params))
})

test_that("jvn_nowcast seed is reproducible and preserves RNG state", {
  skip_if_not_reviser_full_tests()
  baseline_rng <- function() {
    set.seed(2024)
    rnorm(3)
    rnorm(3)
  }

  post_fit_rng <- function() {
    set.seed(2024)
    rnorm(3)
    jvn_nowcast(
      df = df_test_wide,
      e = 2,
      ar_order = 1,
      h = 0,
      include_news = TRUE,
      include_noise = FALSE,
      solver_options = list(
        trace = 0,
        n_starts = 2,
        seed = 99,
        return_states = FALSE
      )
    )
    rnorm(3)
  }

  result1 <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      n_starts = 2,
      seed = 123,
      return_states = FALSE
    )
  )

  set.seed(999)
  result2 <- jvn_nowcast(
    df = df_test_wide,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(
      trace = 0,
      n_starts = 2,
      seed = 123,
      return_states = FALSE
    )
  )

  expect_identical(baseline_rng(), post_fit_rng())
  expect_equal(
    result1$params$Estimate,
    result2$params$Estimate,
    tolerance = 1e-10
  )
})

# ===== Edge Cases =====

test_that("jvn_nowcast handles minimal data", {
  df_small <- df_test_wide[1:15, ]

  result <- jvn_nowcast(
    df = df_small,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(trace = 0, maxiter = 100)
  )

  expect_s3_class(result, "jvn_model")
})

test_that("jvn_nowcast handles data with NAs", {
  df_na <- df_test_wide
  df_na[1:3, 2] <- NA

  result <- jvn_nowcast(
    df = df_na,
    e = 2,
    ar_order = 1,
    h = 0,
    include_news = TRUE,
    include_noise = FALSE,
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "jvn_model")
})

test_that("jvn_nowcast rejects e = 0", {
  expect_error(
    jvn_nowcast(
      df = df_test_wide,
      e = 0,
      ar_order = 1,
      h = 0,
      include_news = TRUE,
      include_noise = FALSE,
      solver_options = list(trace = 0)
    ),
    "must be a single number > 0"
  )
})
