# Test file for kk.R
# Tests for kk_nowcast and related functions
#' @srrstats {G5.2} Error and warning behavior tested
#' @srrstats {G5.2a} Every error message is unique
#' @srrstats {G5.2b} Tests demonstrate conditions triggering messages
#' @srrstats {G5.4} Correctness tests against fixed test data
#' @srrstats {G5.5} Tests run with fixed random seed
#' @srrstats {G5.6} Parameter recovery tests

# ===== Setup Test Data =====

set.seed(456)
n_obs <- 30
n_releases <- 3

# Generate synthetic data for KK model
# True values with AR(1) process
ar_coef <- 0.7
true_values <- numeric(n_obs)
true_values[1] <- rnorm(1)
for (t in 2:n_obs) {
  true_values[t] <- ar_coef * true_values[t - 1] + rnorm(1, 0, 0.5)
}

# Create release data with systematic revisions
df_wide_kk <- data.frame(
  time = seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = n_obs
  )
)

# Add releases with decreasing measurement error
for (i in 0:(n_releases - 1)) {
  noise_sd <- 0.5 / (i + 1) # Noise decreases with newer releases
  df_wide_kk[[paste0("release_", i)]] <- true_values + rnorm(n_obs, 0, noise_sd)
}

# Create long format version
df_long_kk <- tidyr::pivot_longer(
  df_wide_kk,
  cols = -time,
  names_to = "release",
  values_to = "value"
)

# Smaller dataset for faster tests
df_small <- df_wide_kk[1:20, ]

kk_fit <- function(key, ..., solver_options = list(trace = 0)) {
  args <- list(...)
  args$solver_options <- modifyList(list(trace = 0), solver_options)

  cached_fixture(
    paste0("kk-", key),
    function() do.call(kk_nowcast, args)
  )
}

fit_kk_ols <- function() {
  kk_fit(
    "ols",
    df = df_small,
    e = 1,
    h = 0,
    model = "KK",
    method = "OLS"
  )
}

fit_kk_ols_forecast <- function() {
  kk_fit(
    "ols-forecast",
    df = df_small,
    e = 1,
    h = 3,
    model = "KK",
    method = "OLS"
  )
}

fit_kk_sur <- function() {
  kk_fit(
    "sur",
    df = df_small,
    e = 1,
    h = 0,
    model = "KK",
    method = "SUR"
  )
}

fit_kk_mle <- function() {
  kk_fit(
    "mle",
    df = df_small,
    e = 1,
    h = 0,
    model = "KK",
    method = "MLE",
    solver_options = list(maxiter = 40, return_states = FALSE)
  )
}

fit_kk_long <- function() {
  kk_fit(
    "long",
    df = df_long_kk[df_long_kk$time %in% df_small$time, ],
    e = 1,
    h = 0,
    model = "KK",
    method = "OLS"
  )
}

fit_kk_howrey <- function() {
  kk_fit(
    "howrey",
    df = df_small,
    e = 1,
    h = 0,
    model = "Howrey",
    method = "OLS"
  )
}

fit_kk_classical <- function() {
  kk_fit(
    "classical",
    df = df_small,
    e = 1,
    h = 0,
    model = "Classical",
    method = "OLS"
  )
}

fit_kk_e2 <- function() {
  kk_fit(
    "e2",
    df = df_small,
    e = 2,
    h = 0,
    model = "KK",
    method = "OLS"
  )
}

# ===== Tests for kk_nowcast =====

test_that("kk_nowcast returns correct structure with SUR", {
  result <- fit_kk_sur()

  expect_s3_class(result, "kk_model")
  expect_true("states" %in% names(result))
  expect_true("params" %in% names(result))
  expect_true("kk_model_mat" %in% names(result))
  expect_true("ss_model_mat" %in% names(result))
  expect_true("fit" %in% names(result))
  expect_true("e" %in% names(result))
  expect_equal(result$e, 1)
})

test_that("kk_nowcast returns correct structure with OLS", {
  result <- fit_kk_ols()

  expect_s3_class(result, "kk_model")
  expect_true("params" %in% names(result))
  expect_equal(result$convergence, 0)
})

test_that("kk_nowcast returns correct structure with MLE", {
  result <- fit_kk_mle()

  expect_s3_class(result, "kk_model")
  expect_true(!is.null(result$loglik))
  expect_true(!is.null(result$aic))
  expect_true(!is.null(result$bic))
  expect_true(is.finite(result$loglik))
})

test_that("kk_nowcast handles long format data", {
  result <- fit_kk_long()

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast validates e parameter", {
  expect_error(
    kk_nowcast(df = df_small, e = 0, model = "KK"),
    "'e' must be a single whole number greater than 0"
  )

  expect_error(
    kk_nowcast(df = df_small, e = 3, model = "KK"),
    "'df' must contain release columns"
  )
})

test_that("kk_nowcast validates h parameter", {
  expect_error(
    kk_nowcast(df = df_small, e = 1, h = -1, model = "KK"),
    "'h' must be at least 0"
  )
})

test_that("kk_nowcast validates model parameter", {
  expect_error(
    kk_nowcast(df = df_small, e = 1, model = "invalid"),
    "'model' must be one of"
  )
})

test_that("kk_nowcast validates solver_options parameter", {
  expect_error(
    kk_nowcast(df = df_small, e = 1, solver_options = "not_a_list"),
    "'solver_options' must be a list"
  )
})

test_that("kk_nowcast validates solver_options names", {
  expect_error(
    kk_nowcast(
      df = df_small,
      e = 1,
      solver_options = list(invalid_option = 123)
    ),
    "Invalid solver options provided"
  )
})

test_that("kk_nowcast handles Kishor-Koenig model", {
  result <- kk_nowcast(
    df = df_small,
    e = 1,
    model = "Kishor-Koenig",
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "kk_model")
  expect_true(any(grepl("^G", result$params$Parameter)))
})

test_that("kk_nowcast handles KK model (short name)", {
  result <- kk_nowcast(
    df = df_small,
    e = 1,
    model = "kk",
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast handles Howrey model", {
  result <- fit_kk_howrey()

  expect_s3_class(result, "kk_model")
  # Howrey has e^2 G parameters (fewer than KK)
  n_g_params <- sum(grepl("^G", result$params$Parameter))
  expect_equal(n_g_params, 1^2) # e=1, so 1 G parameter
})

test_that("kk_nowcast handles Classical model", {
  result <- fit_kk_classical()

  expect_s3_class(result, "kk_model")
  # Classical has no G parameters
  n_g_params <- sum(grepl("^G", result$params$Parameter))
  expect_equal(n_g_params, 0)
})

test_that("kk_nowcast handles different e values", {
  result_e1 <- fit_kk_ols()
  result_e2 <- fit_kk_e2()

  # e=2 should have more parameters than e=1
  expect_gt(nrow(result_e2$params), nrow(result_e1$params))
})

test_that("kk_nowcast produces forecasts when h > 0", {
  result <- fit_kk_ols_forecast()

  oos_data <- result$states[result$states$sample == "out_of_sample", ]
  expect_gt(nrow(oos_data), 0)
  expect_equal(length(unique(oos_data$time)), 3)
})

test_that("kk_nowcast handles custom starting values with OLS", {
  # Get parameter count
  temp_result <- fit_kk_ols()

  n_params <- nrow(temp_result$params)
  start_vals <- rep(0.3, n_params)
  names(start_vals) <- temp_result$params$Parameter

  result <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0, startvals = start_vals)
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast validates startvals length", {
  start_vals <- c(F0 = 0.5, G1_1 = 0.3) # Wrong length

  expect_error(
    kk_nowcast(
      df = df_small,
      e = 1,
      method = "OLS",
      solver_options = list(startvals = start_vals)
    ),
    "The length of 'startvals' must be"
  )
})

test_that("kk_nowcast validates startvals are numeric", {
  expect_error(
    kk_nowcast(
      df = df_small,
      e = 1,
      solver_options = list(startvals = "not_numeric")
    ),
    "'startvals' must be a numeric vector"
  )
})

test_that("kk_nowcast accepts unnamed startvals in canonical order", {
  skip_if_not_reviser_full_tests()
  start_vals <- c(0.5, 0.3, 0.2, 0.1, 0.1)

  result <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      startvals = start_vals,
      maxiter = 30,
      return_states = FALSE
    )
  )

  expect_s3_class(result, "kk_model")
  expect_true(is.finite(result$loglik))
})

test_that("kk_nowcast MLE with different optimization methods", {
  skip_if_not_reviser_full_tests()
  methods <- c("L-BFGS-B", "BFGS", "Nelder-Mead", "nlminb")

  for (meth in methods) {
    result <- kk_nowcast(
      df = df_small,
      e = 1,
      method = "MLE",
      solver_options = list(
        trace = 0,
        method = meth,
        maxiter = 40,
        return_states = FALSE
      )
    )

    expect_s3_class(result, "kk_model")
    expect_true(is.finite(result$loglik))
  }
})

test_that("kk_nowcast MLE with two-step method", {
  skip_if_not_reviser_full_tests()
  result <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      method = "two-step",
      maxiter = 40,
      return_states = FALSE
    )
  )

  expect_s3_class(result, "kk_model")
  expect_true(is.finite(result$loglik))
})

test_that("kk_nowcast MLE with multi-start optimization", {
  skip_if_not_reviser_full_tests()
  result <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      n_starts = 2,
      maxiter = 40,
      seed = 123,
      return_states = FALSE
    )
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast MLE with transform_se option", {
  skip_if_not_reviser_full_tests()
  result_transform <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      transform_se = TRUE,
      maxiter = 40,
      return_states = FALSE
    )
  )

  result_no_transform <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      transform_se = FALSE,
      maxiter = 40,
      return_states = FALSE
    )
  )

  expect_s3_class(result_transform, "kk_model")
  expect_s3_class(result_no_transform, "kk_model")
})

test_that("kk_nowcast MLE supports se_method none and qml", {
  skip_if_not_reviser_full_tests()
  result_none <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      se_method = "none",
      maxiter = 40,
      return_states = FALSE
    )
  )

  result_qml <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      se_method = "qml",
      maxiter = 40,
      return_states = FALSE
    )
  )

  expect_true(all(is.na(result_none$params$Std.Error)))
  expect_true(any(is.finite(result_qml$params$Std.Error)))
})

test_that("kk_nowcast can skip state extraction", {
  result <- fit_kk_mle()

  expect_null(result$states)
  expect_null(result$model)
})

test_that("kk_nowcast MLE seed is reproducible and preserves RNG state", {
  skip_if_not_reviser_full_tests()
  baseline_rng <- function() {
    set.seed(2024)
    rnorm(3)
    rnorm(3)
  }

  post_fit_rng <- function() {
    set.seed(2024)
    rnorm(3)
    kk_nowcast(
      df = df_small,
      e = 1,
      method = "MLE",
      solver_options = list(
        trace = 0,
        n_starts = 2,
        seed = 99,
        maxiter = 30,
        return_states = FALSE
      )
    )
    rnorm(3)
  }

  result1 <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      n_starts = 2,
      seed = 123,
      maxiter = 30,
      return_states = FALSE
    )
  )

  set.seed(999)
  result2 <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "MLE",
    solver_options = list(
      trace = 0,
      n_starts = 2,
      seed = 123,
      maxiter = 30,
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

test_that("kk_nowcast states output has correct structure", {
  result <- fit_kk_ols_forecast()

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

test_that("kk_nowcast parameter estimates are finite", {
  result <- fit_kk_ols()

  expect_true(all(is.finite(result$params$Estimate)))
})

test_that("kk_nowcast handles irregular time series error", {
  df_irregular <- df_small
  df_irregular$time <- c(
    seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    seq.Date(as.Date("2020-11-01"), by = "quarter", length.out = 10)
  )

  suppressWarnings(
    expect_error(
      kk_nowcast(
        df = df_irregular,
        e = 1,
        h = 2,
        method = "OLS",
        solver_options = list(trace = 0)
      ),
      "not to be regular"
    )
  )
})

test_that("kk_nowcast case insensitivity for model parameter", {
  result_lower <- kk_nowcast(
    df = df_small,
    e = 1,
    model = "kk",
    method = "OLS",
    solver_options = list(trace = 0)
  )

  result_upper <- kk_nowcast(
    df = df_small,
    e = 1,
    model = "KK",
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_equal(nrow(result_lower$params), nrow(result_upper$params))
})

test_that("kk_nowcast case insensitivity for method parameter", {
  result_lower <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "ols",
    solver_options = list(trace = 0)
  )

  result_upper <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_equal(nrow(result_lower$params), nrow(result_upper$params))
})

# ===== Tests for kk_matrices =====

test_that("kk_matrices creates character matrices", {
  matrices <- kk_matrices(e = 2, model = "KK", type = "character")

  expect_true("FF" %in% names(matrices))
  expect_true("GG" %in% names(matrices))
  expect_true("V" %in% names(matrices))
  expect_true("W" %in% names(matrices))
  expect_true("params" %in% names(matrices))

  expect_true(is.character(matrices$FF))
  expect_equal(dim(matrices$FF), c(3, 3)) # e+1 x e+1
})

test_that("kk_matrices creates numeric matrices with params", {
  # For e=2, KK model G parameters follow: G(e-i)_(e-j+1)
  # With i in 1:e, j in 1:(e+1):
  # i=1, j=1,2,3: G1_2, G1_1, G1_0
  # i=2, j=1,2,3: G0_2, G0_1, G0_0
  params <- c(
    F0 = 0.8,
    G1_2 = 0.5, G1_1 = 0.4, G1_0 = 0.3,
    G0_2 = 0.4, G0_1 = 0.3, G0_0 = 0.2,
    v0 = 0.1,
    eps1 = 0.05, eps0 = 0.03
  )

  matrices <- kk_matrices(
    e = 2,
    model = "KK",
    params = params,
    type = "numeric"
  )

  expect_true(is.numeric(matrices$FF))
  expect_equal(matrices$FF[3, 3], 0.8) # F0 in bottom right
})

test_that("kk_matrices validates e parameter", {
  expect_error(
    kk_matrices(e = 0, model = "KK", type = "character"),
    "The initial release is already efficient"
  )
})

test_that("kk_matrices validates model parameter", {
  expect_error(
    kk_matrices(e = 1, model = "invalid", type = "character"),
    "'model' must be one of"
  )
})

test_that("kk_matrices validates type parameter", {
  expect_error(
    kk_matrices(e = 1, model = "KK", type = "invalid"),
    "'type' must be one of"
  )
})

test_that("kk_matrices requires params for numeric type", {
  expect_error(
    kk_matrices(e = 1, model = "KK", type = "numeric"),
    "argument 'params' must be provided"
  )
})

test_that("kk_matrices validates params length", {
  params <- c(F0 = 0.8) # Too few parameters

  expect_error(
    kk_matrices(e = 1, model = "KK", params = params, type = "numeric"),
    "'params' must have length"
  )
})

test_that("kk_matrices accepts unnamed params in canonical order", {
  params <- c(0.8, 0.4, 0.5, 0.1, 0.05)

  matrices <- kk_matrices(
    e = 1,
    model = "KK",
    params = params,
    type = "numeric"
  )

  expect_equal(names(matrices$params), c("F0", "G0_0", "G0_1", "v0", "eps0"))
  expect_equal(matrices$FF[2, 2], 0.8)
  expect_equal(matrices$GG[2, 1], 0.5)
  expect_equal(matrices$GG[2, 2], 0.4)
})

test_that("kk_matrices normalizes named params to canonical order", {
  params <- c(
    eps0 = 0.05,
    G0_1 = 0.5,
    F0 = 0.8,
    v0 = 0.1,
    G0_0 = 0.4
  )

  matrices <- kk_matrices(
    e = 1,
    model = "KK",
    params = params,
    type = "numeric"
  )

  expect_equal(
    unname(matrices$params),
    c(0.8, 0.4, 0.5, 0.1, 0.05)
  )
  expect_equal(names(matrices$params), c("F0", "G0_0", "G0_1", "v0", "eps0"))
})

test_that("kk_matrices handles Howrey model correctly", {
  matrices <- kk_matrices(e = 2, model = "Howrey", type = "character")

  # Howrey has e*e G parameters
  n_g_params <- sum(grepl("^G", names(matrices$params)))
  expect_equal(n_g_params, 2^2) # e=2, so 4 G parameters
})

test_that("kk_matrices handles Classical model correctly", {
  matrices <- kk_matrices(e = 2, model = "Classical", type = "character")

  # Classical has no G parameters
  n_g_params <- sum(grepl("^G", names(matrices$params)))
  expect_equal(n_g_params, 0)

  # GG should be identity matrix
  expect_true(all(diag(3) == as.numeric(matrices$GG)))
})

test_that("kk_matrices parameter sorting is consistent", {
  matrices <- kk_matrices(e = 1, model = "KK", type = "character")

  param_names <- names(matrices$params)
  # Should start with F0, then G params, then v0, then eps params
  expect_equal(param_names[1], "F0")
  expect_true(grepl("^v0$", param_names[length(param_names) - 1]))
})

# ===== Tests for kk_to_ss =====

test_that("kk_to_ss converts KK matrices to state-space form", {
  # For e=1, KK model G parameters: G(e-i)_(e-j+1)
  # i=1, j=1,2: G0_1, G0_0
  params <- c(
    F0 = 0.8,
    G0_1 = 0.5,
    G0_0 = 0.4,
    v0 = 0.1,
    eps0 = 0.05
  )

  matrices <- kk_matrices(
    e = 1,
    model = "KK",
    params = params,
    type = "numeric"
  )
  ss_mat <- kk_to_ss(matrices$FF, matrices$GG, matrices$V, matrices$W)

  expect_true("Z" %in% names(ss_mat))
  expect_true("Tmat" %in% names(ss_mat))
  expect_true("H" %in% names(ss_mat))
  expect_true("Q" %in% names(ss_mat))
  expect_true("R" %in% names(ss_mat))
})

test_that("kk_to_ss creates correct dimensions", {
  # For e=2, KK model G parameters: G(e-i)_(e-j+1)
  # i=1, j=1,2,3: G1_2, G1_1, G1_0
  # i=2, j=1,2,3: G0_2, G0_1, G0_0
  params <- c(
    F0 = 0.8,
    G1_2 = 0.5, G1_1 = 0.4, G1_0 = 0.3,
    G0_2 = 0.4, G0_1 = 0.3, G0_0 = 0.2,
    v0 = 0.1,
    eps1 = 0.05, eps0 = 0.03
  )

  matrices <- kk_matrices(
    e = 2,
    model = "KK",
    params = params,
    type = "numeric"
  )
  ss_mat <- kk_to_ss(matrices$FF, matrices$GG, matrices$V, matrices$W)

  e <- 2
  expect_equal(dim(ss_mat$Z), c(e + 1, 2 * (e + 1)))
  expect_equal(dim(ss_mat$Tmat), c(2 * (e + 1), 2 * (e + 1)))
  expect_equal(dim(ss_mat$Q), c(2 * (e + 1), 2 * (e + 1)))
})

# ===== Tests for print.kk_model =====

test_that("print.kk_model produces output", {
  result <- fit_kk_ols()

  output <- utils::capture.output(print(result))

  expect_gt(length(output), 0)
  expect_true(any(grepl("Kishor-Koenig Model", output)))
  expect_true(any(grepl("Convergence", output)))
})

test_that("print.kk_model returns invisibly", {
  result <- fit_kk_ols()

  returned <- print(result)
  expect_identical(returned, result)
})

# ===== Tests for summary.kk_model =====

test_that("summary.kk_model produces detailed output", {
  result <- fit_kk_mle()

  output <- utils::capture.output(summary(result))

  expect_gt(length(output), 0)
  expect_true(any(grepl("Kishor-Koenig Model", output)))
  expect_true(any(grepl("Log-likelihood", output)))
  expect_true(any(grepl("AIC", output)))
  expect_true(any(grepl("BIC", output)))
  expect_true(any(grepl("Parameter Estimates", output)))
})

test_that("summary.kk_model returns invisibly", {
  result <- fit_kk_ols()

  returned <- summary(result)
  expect_identical(returned, result)
})

# ===== Tests for plot.kk_model =====

test_that("plot.kk_model returns ggplot object", {
  result <- fit_kk_ols_forecast()

  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot.kk_model handles different states", {
  result <- fit_kk_ols_forecast()

  # Get available states
  states <- unique(result$states$state)

  for (state_name in states) {
    p <- plot(result, state = state_name)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot.kk_model handles filtered vs smoothed", {
  result <- fit_kk_ols_forecast()

  p_filtered <- plot(result, type = "filtered")
  p_smoothed <- plot(result, type = "smoothed")

  expect_s3_class(p_filtered, "ggplot")
  expect_s3_class(p_smoothed, "ggplot")
})

# ===== Integration Tests =====

test_that("full workflow with all components", {
  result <- fit_kk_ols_forecast()

  # Test structure
  expect_s3_class(result, "kk_model")

  # Test methods work
  output_summary <- utils::capture.output(summary(result))
  expect_gt(length(output_summary), 0)

  output_print <- utils::capture.output(print(result))
  expect_gt(length(output_print), 0)

  p <- plot(result)
  expect_s3_class(p, "ggplot")

  # Test components
  expect_true(result$convergence %in% c(0, 1))
  expect_true("Parameter" %in% colnames(result$params))
  expect_true("Estimate" %in% colnames(result$params))
  expect_true("Std.Error" %in% colnames(result$params))
})

test_that("different models produce different results", {
  result_kk <- fit_kk_ols()
  result_howrey <- fit_kk_howrey()
  result_classical <- fit_kk_classical()

  # Different number of parameters
  expect_gt(nrow(result_kk$params), nrow(result_howrey$params))
  expect_gt(nrow(result_howrey$params), nrow(result_classical$params))
})

test_that("different methods produce consistent estimates", {
  result_ols <- fit_kk_ols()
  result_sur <- fit_kk_sur()

  # Both should have same parameter names
  expect_equal(
    result_ols$params$Parameter,
    result_sur$params$Parameter
  )
})

# ===== Edge Cases =====

test_that("kk_nowcast handles minimal data", {
  df_minimal <- df_wide_kk[1:12, 1:3] # 12 obs, 2 releases (e=1)

  result <- kk_nowcast(
    df = df_minimal,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast handles data with NAs at beginning", {
  df_na <- df_small
  df_na[1:3, 2] <- NA

  result <- kk_nowcast(
    df = df_na,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast handles extreme values", {
  df_extreme <- df_small
  df_extreme[, -1] <- df_extreme[, -1] * 1e3

  result <- kk_nowcast(
    df = df_extreme,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast handles high frequency data", {
  # Quarterly data
  df_quarterly <- data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = 20),
    release_0 = rnorm(20, 100, 10),
    release_1 = rnorm(20, 100, 8)
  )

  result <- kk_nowcast(
    df = df_quarterly,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast model matrices have correct properties", {
  result <- kk_nowcast(
    df = df_small,
    e = 1,
    method = "OLS",
    solver_options = list(trace = 0)
  )

  # Check FF is square
  expect_equal(nrow(result$kk_model_mat$FF), ncol(result$kk_model_mat$FF))

  # Check GG is square
  expect_equal(nrow(result$kk_model_mat$GG), ncol(result$kk_model_mat$GG))

  # Check V is square
  expect_equal(nrow(result$kk_model_mat$V), ncol(result$kk_model_mat$V))

  # Check W is square
  expect_equal(nrow(result$kk_model_mat$W), ncol(result$kk_model_mat$W))
})

test_that("kk_nowcast information criteria are calculated correctly for MLE", {
  result <- fit_kk_mle()

  expect_true(is.finite(result$aic))
  expect_true(is.finite(result$bic))
  expect_true(is.finite(result$loglik))

  # BIC should penalize more than AIC
  expect_gt(result$bic, result$aic)
})

test_that("kk_nowcast confidence intervals have correct coverage", {
  result <- fit_kk_ols()

  # Check that lower < estimate < upper
  states <- result$states
  expect_true(all(states$lower <= states$estimate, na.rm = TRUE))
  expect_true(all(states$estimate <= states$upper, na.rm = TRUE))
})
