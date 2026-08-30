# Tests for the shared `revision_model` class system.
#
# `kk_model` and `jvn_model` inherit their extractor, print, summary and plot
# methods from the common parent class `revision_model`. These tests check the
# inheritance itself rather than the numerical content of either fit, which is
# covered in test-kk.R and test-jvn.R.
#' @srrstats {TS4.2} Return classes of the implemented methods are tested
#' @srrstats {TS5.0} Class system and its inherited methods are tested

set.seed(789)

n_obs <- 24
true_values <- as.numeric(stats::arima.sim(list(ar = 0.6), n = n_obs))

df_shared <- data.frame(
  time = seq.Date(as.Date("2015-01-01"), by = "quarter", length.out = n_obs)
)
for (i in 0:2) {
  df_shared[[paste0("release_", i)]] <-
    true_values + stats::rnorm(n_obs, 0, 0.4 / (i + 1))
}

fit_shared_kk <- function() {
  cached_fixture("shared-kk", function() {
    kk_nowcast(
      df_shared,
      e = 2,
      h = 2,
      model = "KK",
      method = "OLS",
      solver_options = list(trace = 0)
    )
  })
}

fit_shared_kk_mle <- function() {
  cached_fixture("shared-kk-mle", function() {
    kk_nowcast(
      df_shared,
      e = 2,
      h = 2,
      model = "KK",
      method = "MLE",
      solver_options = list(trace = 0)
    )
  })
}

# ===== Class hierarchy =====

test_that("fitted models inherit from the shared revision_model class", {
  fit <- fit_shared_kk()

  expect_s3_class(fit, "kk_model")
  expect_s3_class(fit, "revision_model")
  # The parent must sit behind the child, so child methods would win.
  expect_identical(class(fit), c("kk_model", "revision_model", "list"))
})

test_that("the shared methods are registered on the parent, not the children", {
  # This is the point of the refactor: one registration per generic, inherited
  # by every model family, instead of one registration per family.
  generics <- c(
    "coef", "vcov", "logLik", "nobs", "fitted", "residuals", "predict",
    "states", "print", "summary", "plot"
  )

  for (g in generics) {
    expect_true(
      reviser_has_method(g, "revision_model"),
      info = paste0("no ", g, ".revision_model method")
    )
    for (child in c("kk_model", "jvn_model")) {
      expect_false(
        reviser_has_method(g, child),
        info = paste0("unexpected ", g, ".", child, " method")
      )
    }
  }
})

test_that("dispatch on a fitted object reaches the inherited methods", {
  fit <- fit_shared_kk()

  expect_type(coef(fit), "double")
  expect_type(nobs(fit), "integer")
  expect_s3_class(fitted(fit), "tbl_df")
  expect_named(residuals(fit), c("time", "residual"))
  expect_named(predict(fit), c("time", "estimate", "lower", "upper"))
  expect_s3_class(states(fit), "tbl_df")
  expect_s3_class(plot(fit), "ggplot")

  mle <- fit_shared_kk_mle()
  expect_s3_class(logLik(mle), "logLik")
  expect_equal(AIC(mle), mle$aic)
  expect_equal(BIC(mle), mle$bic)
})

test_that("the inherited accessors fail informatively when data is absent", {
  # An OLS fit carries no likelihood; the shared method must say so rather
  # than returning NULL.
  expect_error(logLik(fit_shared_kk()), "not fitted by MLE")
})

test_that("every state-dependent method names return_states = FALSE", {
  no_states <- kk_nowcast(
    df_shared,
    e = 2,
    model = "KK",
    method = "OLS",
    solver_options = list(trace = 0, return_states = FALSE)
  )

  # plot() reads the same component as the accessors, so it has to report the
  # same cause. It previously failed inside nrow(NULL) with an unrelated
  # "argument is of length zero".
  for (f in list(states, fitted, residuals, predict, plot)) {
    expect_error(f(no_states), "return_states = FALSE")
  }

  # Everything that does not depend on the states keeps working.
  expect_no_error(coef(no_states))
  expect_no_error(capture.output(print(no_states)))
  expect_no_error(capture.output(summary(no_states)))
})

test_that("predict says why it has nothing to return at h = 0", {
  no_horizon <- kk_nowcast(
    df_shared,
    e = 2,
    h = 0,
    model = "KK",
    method = "OLS",
    solver_options = list(trace = 0)
  )

  expect_message(out <- predict(no_horizon), "`h = 0`")
  expect_identical(nrow(out), 0L)
})

# ===== Family-specific dispatch behind the shared methods =====

test_that("the family-specific generics resolve for both model families", {
  # The five internal generics are the only places the families differ, and
  # the only methods a new family has to supply.
  internal_generics <- c(
    "model_family", "spec_lines", "signal_state", "target_column",
    "default_plot_state"
  )

  for (g in internal_generics) {
    for (child in c("kk_model", "jvn_model")) {
      expect_true(
        reviser_has_method(g, child),
        info = paste0("no ", g, ".", child, " method")
      )
    }
    # The parent deliberately has no fallback: a new family must be explicit.
    expect_false(reviser_has_method(g, "revision_model"))
  }
})

test_that("summary reports the family that was fitted", {
  fit <- fit_shared_kk()

  output <- utils::capture.output(summary(fit))
  expect_true(any(grepl("Kishor-Koenig Model", output, fixed = TRUE)))
  expect_true(any(grepl("Specification:", output, fixed = TRUE)))
})

test_that("print and summary agree", {
  fit <- fit_shared_kk()

  expect_identical(
    utils::capture.output(print(fit)),
    utils::capture.output(summary(fit))
  )
  expect_identical(summary(fit), fit)
})

test_that("residuals are measured against the family's target release", {
  fit <- fit_shared_kk()

  fit_vals <- fitted(fit)
  resid <- residuals(fit)

  observed <- data.frame(
    time = fit$data$time,
    obs = fit$data[[paste0("release_", fit$e)]]
  )
  merged <- merge(observed, as.data.frame(fit_vals), by = "time")

  expect_equal(
    resid$residual[order(resid$time)],
    (merged$obs - merged$estimate)[order(merged$time)]
  )
})

test_that("fitted and predict split the sample at the forecast horizon", {
  fit <- fit_shared_kk()

  in_sample <- fitted(fit)
  out_sample <- predict(fit)

  expect_gt(nrow(in_sample), 0)
  expect_identical(nrow(out_sample), 2L) # h = 2
  expect_true(all(out_sample$time > max(in_sample$time)))
})
