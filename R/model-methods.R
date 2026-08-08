#' Extract the latent state estimates of a revision model
#'
#' Accessor for the state paths of a fitted revision-nowcasting model.
#' Provides programmatic access to the estimated states instead of reaching
#' into the object with `fit$states`.
#'
#' @param object A fitted model object, such as `kk_model` or `jvn_model`.
#' @param filter Which state estimates to return: `"smoothed"` (default) uses
#'   the full sample, `"filtered"` uses information available up to each date,
#'   and `"all"` returns both.
#' @param state Optional character vector of state names to keep. Defaults to
#'   all states.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble with columns `time`, `state`, `estimate`, `lower`,
#'   `upper`, `filter` and `sample`.
#'
#' @srrstats {TS4.2} Explicitly documents the type and class of return values
#' @srrstats {TS5.0} Provides accessor methods for the implemented classes
#'
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' head(states(fit))
#' head(states(fit, filter = "filtered", state = "true_lag_0"))
#' }
#'
#' @family revision nowcasting
#' @export
states <- function(object, ...) {
  UseMethod("states")
}

#' Shared implementation of the `states()` accessor
#'
#' @param object A fitted model object carrying a `states` tibble.
#' @param filter,state See [states()].
#' @param what Label used in error messages.
#' @return A tibble of state estimates.
#' @keywords internal
#' @noRd
states_impl <- function(
  object,
  filter = c("smoothed", "filtered", "all"),
  state = NULL,
  what = "model"
) {
  filter <- match.arg(filter)

  if (is.null(object$states)) {
    rlang::abort(paste0(
      "This ",
      what,
      " was fitted with `return_states = FALSE`, so no state estimates ",
      "are available. Refit with `return_states = TRUE`."
    ))
  }

  out <- object$states

  if (filter != "all") {
    out <- out[out$filter == filter, , drop = FALSE]
  }

  if (!is.null(state)) {
    unknown <- setdiff(state, unique(object$states$state))
    if (length(unknown) > 0) {
      rlang::abort(paste0(
        "Unknown state(s): ",
        paste(unknown, collapse = ", "),
        ". Available: ",
        paste(unique(object$states$state), collapse = ", "),
        "."
      ))
    }
    out <- out[out$state %in% state, , drop = FALSE]
  }

  out
}

#' Build a `logLik` object from a fitted revision model
#'
#' `df` and `nobs` are taken from the quantities the model actually used for
#' its information criteria, so that [stats::AIC()] and [stats::BIC()] on the
#' returned object reproduce the `aic` and `bic` shown by `summary()`.
#'
#' @param object A fitted model object.
#' @return An object of class `logLik`.
#' @keywords internal
#' @noRd
loglik_impl <- function(object) {
  if (is.null(object$loglik)) {
    rlang::abort(
      "No log-likelihood available; the model was not fitted by MLE."
    )
  }

  out <- object$loglik
  attr(out, "df") <- rlang::`%||%`(object$n_param, nrow(object$params))
  attr(out, "nobs") <- rlang::`%||%`(object$n_ic, nrow(object$data))
  class(out) <- "logLik"
  out
}

#' Shared implementation of `coef()` for fitted revision models
#'
#' @param object A fitted model object with a `params` table.
#' @return A named numeric vector of parameter estimates.
#' @keywords internal
#' @noRd
coef_impl <- function(object) {
  out <- object$params$Estimate
  names(out) <- object$params$Parameter
  out
}

#' Shared implementation of `vcov()` for fitted revision models
#'
#' @param object A fitted model object with a `cov` matrix.
#' @return The parameter covariance matrix, with dimnames.
#' @keywords internal
#' @noRd
vcov_impl <- function(object) {
  if (is.null(object$cov)) {
    rlang::abort(
      paste(
        "No parameter covariance matrix available; the model was fitted",
        "with `se_method = \"none\"` or standard errors could not be",
        "computed."
      )
    )
  }

  out <- object$cov
  nms <- object$params$Parameter

  if (!is.null(nms) && length(nms) == nrow(out)) {
    dimnames(out) <- list(nms, nms)
  }

  out
}

#' Extract the estimated latent signal of a fitted revision model
#'
#' @param object A fitted model object.
#' @param state_name Name of the state holding the latent signal.
#' @param sample Which observations to return.
#' @return A tibble with `time` and `estimate`.
#' @keywords internal
#' @noRd
signal_impl <- function(object, state_name, sample = "in_sample") {
  out <- states_impl(object, filter = "smoothed", state = state_name)
  out <- out[out$sample %in% sample, , drop = FALSE]
  out[order(out$time), c("time", "estimate", "lower", "upper")]
}

#' Name of the state holding the efficient estimate of a KK model
#'
#' @param object A `kk_model`.
#' @return A single string.
#' @keywords internal
#' @noRd
kk_signal_state <- function(object) {
  paste0("release_", object$e, "_lag_0")
}

# ---- kk_model methods -------------------------------------------------------

#' @rdname states
#' @method states kk_model
#' @export
states.kk_model <- function(
  object,
  filter = c("smoothed", "filtered", "all"),
  state = NULL,
  ...
) {
  states_impl(object, filter = filter, state = state, what = "kk_model")
}

#' Extract parameter estimates from a KK model
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return A named numeric vector of parameter estimates.
#' @method coef kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, model = "KK", method = "OLS")
#' coef(fit)
#' @family revision nowcasting
#' @export
coef.kk_model <- function(object, ...) {
  coef_impl(object)
}

#' Extract the parameter covariance matrix of a KK model
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return The estimated parameter covariance matrix.
#' @method vcov kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, model = "KK", method = "MLE")
#' vcov(fit)
#' @family revision nowcasting
#' @export
vcov.kk_model <- function(object, ...) {
  vcov_impl(object)
}

#' Extract the log-likelihood of a KK model
#'
#' The returned object carries the degrees of freedom and effective number of
#' observations used by the model, so [stats::AIC()] and [stats::BIC()]
#' reproduce the values reported by `summary()`.
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return An object of class `logLik`.
#' @method logLik kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, model = "KK", method = "MLE")
#' logLik(fit)
#' AIC(fit)
#' BIC(fit)
#' @family revision nowcasting
#' @export
logLik.kk_model <- function(object, ...) {
  loglik_impl(object)
}

#' Number of observations used to fit a KK model
#'
#' Returns the effective number of observations behind the reported
#' information criteria. Under the default `ic_n = "Tp"` this is the number
#' of time periods times the number of releases modeled.
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return A single integer.
#' @method nobs kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, model = "KK", method = "MLE")
#' nobs(fit)
#' @family revision nowcasting
#' @export
nobs.kk_model <- function(object, ...) {
  as.integer(rlang::`%||%`(object$n_ic, nrow(object$data)))
}

#' Fitted efficient estimates from a KK model
#'
#' Returns the smoothed estimate of the latent efficient value for the
#' in-sample periods, i.e. the model's revision-adjusted signal.
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time`, `estimate`, `lower` and `upper`.
#' @method fitted kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, model = "KK", method = "MLE")
#' head(fitted(fit))
#' @family revision nowcasting
#' @export
fitted.kk_model <- function(object, ...) {
  signal_impl(object, kk_signal_state(object))
}

#' Residuals of a KK model
#'
#' Difference between the observed efficient release and the smoothed
#' estimate of the latent efficient value. These are measurement residuals of
#' the release used as the model's target, not one-step-ahead prediction
#' errors.
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time` and `residual`.
#' @method residuals kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, model = "KK", method = "MLE")
#' head(residuals(fit))
#' @family revision nowcasting
#' @export
residuals.kk_model <- function(object, ...) {
  fit_vals <- fitted(object)
  target_col <- paste0("release_", object$e)

  observed <- object$data[, c("time", target_col)]
  names(observed) <- c("time", "observed")

  merged <- merge(as.data.frame(observed), as.data.frame(fit_vals), by = "time")

  dplyr::tibble(
    time = merged$time,
    residual = merged$observed - merged$estimate
  )
}

#' Forecasts from a KK model
#'
#' Returns the out-of-sample estimates of the latent efficient value produced
#' by the forecast horizon `h` supplied to [kk_nowcast()]. The horizon is
#' fixed at estimation time, so refit with a different `h` to change it.
#'
#' @param object An object of class `kk_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time`, `estimate`, `lower` and `upper`.
#'   Has zero rows when the model was fitted with `h = 0`.
#' @method predict kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, h = 2, model = "KK", method = "MLE")
#' predict(fit)
#' @family revision nowcasting
#' @export
predict.kk_model <- function(object, ...) {
  signal_impl(object, kk_signal_state(object), sample = "out_of_sample")
}

# ---- jvn_model methods ------------------------------------------------------

#' @rdname states
#' @method states jvn_model
#' @export
states.jvn_model <- function(
  object,
  filter = c("smoothed", "filtered", "all"),
  state = NULL,
  ...
) {
  states_impl(object, filter = filter, state = state, what = "jvn_model")
}

#' Extract parameter estimates from a JVN model
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return A named numeric vector of parameter estimates.
#' @method coef jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' coef(fit)
#' }
#' @family revision nowcasting
#' @export
coef.jvn_model <- function(object, ...) {
  coef_impl(object)
}

#' Extract the parameter covariance matrix of a JVN model
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return The estimated parameter covariance matrix.
#' @method vcov jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' vcov(fit)
#' }
#' @family revision nowcasting
#' @export
vcov.jvn_model <- function(object, ...) {
  vcov_impl(object)
}

#' Extract the log-likelihood of a JVN model
#'
#' The returned object carries the degrees of freedom and effective number of
#' observations used by the model, so [stats::AIC()] and [stats::BIC()]
#' reproduce the values reported by `summary()`.
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return An object of class `logLik`.
#' @method logLik jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' logLik(fit)
#' AIC(fit)
#' BIC(fit)
#' }
#' @family revision nowcasting
#' @export
logLik.jvn_model <- function(object, ...) {
  loglik_impl(object)
}

#' Number of observations used to fit a JVN model
#'
#' Returns the effective number of observations behind the reported
#' information criteria. Under the default `ic_n = "Tp"` this is the number
#' of time periods times the number of vintages modeled.
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return A single integer.
#' @method nobs jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' nobs(fit)
#' }
#' @family revision nowcasting
#' @export
nobs.jvn_model <- function(object, ...) {
  as.integer(rlang::`%||%`(object$n_ic, nrow(object$data)))
}

#' Fitted true values from a JVN model
#'
#' Returns the smoothed estimate of the latent true value for the in-sample
#' periods, i.e. the model's revision-adjusted signal.
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time`, `estimate`, `lower` and `upper`.
#' @method fitted jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' head(fitted(fit))
#' }
#' @family revision nowcasting
#' @export
fitted.jvn_model <- function(object, ...) {
  signal_impl(object, "true_lag_0")
}

#' Residuals of a JVN model
#'
#' Difference between the most mature release included in the estimation and
#' the smoothed estimate of the latent true value. These are measurement
#' residuals of that release, not one-step-ahead prediction errors.
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time` and `residual`.
#' @method residuals jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(df = df, e = 4, ar_order = 2, include_noise = FALSE)
#' head(residuals(fit))
#' }
#' @family revision nowcasting
#' @export
residuals.jvn_model <- function(object, ...) {
  fit_vals <- fitted(object)

  release_cols <- grep("^release_", names(object$data), value = TRUE)
  target_col <- release_cols[length(release_cols)]

  observed <- object$data[, c("time", target_col)]
  names(observed) <- c("time", "observed")

  merged <- merge(as.data.frame(observed), as.data.frame(fit_vals), by = "time")

  dplyr::tibble(
    time = merged$time,
    residual = merged$observed - merged$estimate
  )
}

#' Forecasts from a JVN model
#'
#' Returns the out-of-sample estimates of the latent true value produced by
#' the forecast horizon `h` supplied to [jvn_nowcast()]. The horizon is fixed
#' at estimation time, so refit with a different `h` to change it.
#'
#' @param object An object of class `jvn_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time`, `estimate`, `lower` and `upper`.
#'   Has zero rows when the model was fitted with `h = 0`.
#' @method predict jvn_model
#' @examples
#' \donttest{
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(reviser::gdp),
#'   id == "EA",
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:3)
#'
#' fit <- jvn_nowcast(
#'   df = df, e = 4, ar_order = 2, h = 2, include_noise = FALSE
#' )
#' predict(fit)
#' }
#' @family revision nowcasting
#' @export
predict.jvn_model <- function(object, ...) {
  signal_impl(object, "true_lag_0", sample = "out_of_sample")
}
