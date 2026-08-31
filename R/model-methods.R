#' Fitted Revision Models
#'
#' @description
#' `reviser` represents every fitted revision-nowcasting model as an S3 object
#' that inherits from the common parent class `revision_model`. The two
#' concrete classes are [kk_nowcast()], which returns a `kk_model`, and
#' [jvn_nowcast()], which returns a `jvn_model`; both carry
#' `c("<family>_model", "revision_model", "list")` as their class attribute.
#'
#' The parent class holds everything the two families share. The standard
#' extractor generics [coef()], [vcov()], [logLik()], [nobs()], [fitted()],
#' [residuals()], [predict()] and the `reviser` generic [states()], together
#' with [print()], [summary()] and [plot()], are defined once for
#' `revision_model` and inherited by both families. Only the handful of
#' behaviors that genuinely differ between the families are dispatched
#' separately, through the internal generics `model_family()`, `spec_lines()`,
#' `signal_state()`, `target_column()` and `default_plot_state()`.
#'
#' A fitted object is a list with at least the components `params` (a data
#' frame with columns `Parameter`, `Estimate` and `Std.Error`), `states` (a
#' long tibble of state estimates, or `NULL` when the model was fitted with
#' `return_states = FALSE`), `loglik`, `n_param`, `n_ic`, `cov` and `data`.
#' A new model family becomes a full citizen of this system by returning an
#' object with those components, prepending `"revision_model"` to its class
#' attribute, and supplying methods for the five internal generics above.
#'
#' @return This topic documents a class rather than a function. [kk_nowcast()]
#'   returns a list of the components described above with class attribute
#'   `c("kk_model", "revision_model", "list")`, and [jvn_nowcast()] returns the
#'   same with `"jvn_model"` in place of `"kk_model"`.
#'
#' @srrstats {TS4.2} Explicitly documents the type and class of return values
#' @srrstats {TS5.0} Documents the class system implemented for model results
#'
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
#'
#' # The fitted object carries the shared parent class.
#' class(fit)
#' inherits(fit, "revision_model")
#'
#' # The extractor generics are inherited from that parent.
#' head(coef(fit))
#' head(states(fit))
#'
#' @name revision_model
#' @family revision nowcasting
#' @seealso [kk_nowcast()], [jvn_nowcast()], [states()]
NULL


# ---- internal extension points ----------------------------------------------
#
# The parent-class methods below are written once against these five internal
# generics. They are the only points at which the KK and JVN families differ,
# and the only methods a new revision-model family has to supply.

#' Display name of a fitted model's family
#'
#' Used to build the header line printed by `summary.revision_model()`.
#'
#' @param object A fitted `revision_model`.
#' @return A single string, e.g. `"Kishor-Koenig"`.
#' @keywords internal
#' @noRd
model_family <- function(object) {
  UseMethod("model_family")
}

#' Specification lines printed by `summary.revision_model()`
#'
#' Returns the family-specific block that follows the header, as a character
#' vector of complete lines. Always starts with the `Specification:` line.
#'
#' @param object A fitted `revision_model`.
#' @return A character vector of lines.
#' @keywords internal
#' @noRd
spec_lines <- function(object) {
  UseMethod("spec_lines")
}

#' Name of the state holding a model's latent signal
#'
#' The state that [fitted()] and [predict()] report: the estimate of the
#' latent quantity the model treats as the truth behind the observed releases.
#'
#' @param object A fitted `revision_model`.
#' @return A single string.
#' @keywords internal
#' @noRd
signal_state <- function(object) {
  UseMethod("signal_state")
}

#' Column of `object$data` that [residuals()] measures against
#'
#' @param object A fitted `revision_model`.
#' @return A single string naming a column of `object$data`.
#' @keywords internal
#' @noRd
target_column <- function(object) {
  UseMethod("target_column")
}

#' State plotted by `plot.revision_model()` when none is given
#'
#' @param object A fitted `revision_model`.
#' @param type Either `"filtered"` or `"smoothed"`.
#' @return A single string.
#' @keywords internal
#' @noRd
default_plot_state <- function(object, type) {
  UseMethod("default_plot_state")
}


# ---- kk_model ---------------------------------------------------------------

#' @keywords internal
#' @noRd
model_family.kk_model <- function(object) {
  "Kishor-Koenig"
}

#' @keywords internal
#' @noRd
spec_lines.kk_model <- function(object) {
  # Fall back to the family name for objects fitted before `model_type` was
  # recorded.
  paste("Specification:", rlang::`%||%`(object$model_type, "Kishor-Koenig"))
}

#' @keywords internal
#' @noRd
signal_state.kk_model <- function(object) {
  paste0("release_", object$e, "_lag_0")
}

#' @keywords internal
#' @noRd
target_column.kk_model <- function(object) {
  paste0("release_", object$e)
}

#' @keywords internal
#' @noRd
default_plot_state.kk_model <- function(object, type) {
  object$states[object$states$filter == type, ]$state[1]
}


# ---- jvn_model --------------------------------------------------------------

#' @keywords internal
#' @noRd
model_family.jvn_model <- function(object) {
  "Jacobs-Van Norden"
}

#' @keywords internal
#' @noRd
spec_lines.jvn_model <- function(object) {
  # Fall back for objects fitted before `model_type` was recorded.
  out <- paste(
    "Specification:",
    rlang::`%||%`(object$model_type, "news and noise")
  )

  if (!is.null(object$spec)) {
    out <- c(
      out,
      paste("AR order:", object$spec$ar_order),
      paste(
        "Components: news =", object$spec$include_news,
        "| noise =", object$spec$include_noise,
        "| spillovers =", object$spec$include_spillovers
      )
    )
  }

  if (isTRUE(object$p0_regularized)) {
    out <- c(
      out,
      paste(
        "Note: the initial-state covariance required numerical",
        "regularization at the converged estimate (a near-nonstationary",
        "fit); see ?jvn_nowcast."
      )
    )
  }

  out
}

#' @keywords internal
#' @noRd
signal_state.jvn_model <- function(object) {
  "true_lag_0"
}

#' @keywords internal
#' @noRd
target_column.jvn_model <- function(object) {
  release_cols <- grep("^release_", names(object$data), value = TRUE)
  release_cols[length(release_cols)]
}

#' @keywords internal
#' @noRd
default_plot_state.jvn_model <- function(object, type) {
  signal_state(object)
}


# ---- shared helpers ---------------------------------------------------------

#' Extract the smoothed latent signal of a fitted revision model
#'
#' @param object A fitted `revision_model`.
#' @param sample Which observations to return.
#' @return A tibble with `time`, `estimate`, `lower` and `upper`.
#' @keywords internal
#' @noRd
signal_path <- function(object, sample = "in_sample") {
  out <- states(object, filter = "smoothed", state = signal_state(object))
  out <- out[out$sample %in% sample, , drop = FALSE]
  out[order(out$time), c("time", "estimate", "lower", "upper")]
}


# ---- shared methods ---------------------------------------------------------

#' Extract the Latent State Estimates of a Revision Model
#'
#' Accessor for the state paths of a fitted revision-nowcasting model.
#' Provides programmatic access to the estimated states instead of reaching
#' into the object with `fit$states`. The method is defined once for the
#' parent class [revision_model] and is inherited by `kk_model` and
#' `jvn_model` objects alike.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
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

#' Abort when a fitted model did not retain its state estimates
#'
#' `return_states = FALSE` drops the component that [states()], [fitted()],
#' [residuals()], [predict()] and [plot()] all read. The single definition
#' here is what keeps every one of them reporting the actual cause, rather
#' than failing later on a `NULL` with an unrelated message.
#'
#' @param object A fitted `revision_model`.
#' @return `invisible(NULL)`, or an error.
#' @keywords internal
#' @noRd
require_states <- function(object) {
  if (is.null(object$states)) {
    rlang::abort(
      paste0(
        "This ",
        class(object)[1],
        " was fitted with `return_states = FALSE`, so no state estimates ",
        "are available. Refit with `return_states = TRUE`."
      ),
      call = rlang::caller_env()
    )
  }

  invisible(NULL)
}

#' @rdname states
#' @method states revision_model
#' @export
states.revision_model <- function(
  object,
  filter = c("smoothed", "filtered", "all"),
  state = NULL,
  ...
) {
  filter <- match.arg(filter)

  require_states(object)

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

#' Extract Parameter Estimates from a Revision Model
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return A named numeric vector of parameter estimates.
#' @method coef revision_model
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
coef.revision_model <- function(object, ...) {
  out <- object$params$Estimate
  names(out) <- object$params$Parameter
  out
}

#' Extract the Parameter Covariance Matrix of a Revision Model
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return The estimated parameter covariance matrix.
#' @method vcov revision_model
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
vcov.revision_model <- function(object, ...) {
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

#' Extract the Log-Likelihood of a Revision Model
#'
#' The returned object carries the degrees of freedom and effective number of
#' observations used by the model, so [stats::AIC()] and [stats::BIC()]
#' reproduce the values reported by `summary()`.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return An object of class `logLik`.
#' @method logLik revision_model
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
logLik.revision_model <- function(object, ...) {
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

#' Number of Observations Used to Fit a Revision Model
#'
#' Returns the effective number of observations behind the reported
#' information criteria. Under the default `ic_n = "Tp"` this is the number
#' of time periods times the number of releases (KK) or vintages (JVN)
#' modeled.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return A single integer.
#' @method nobs revision_model
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
nobs.revision_model <- function(object, ...) {
  as.integer(rlang::`%||%`(object$n_ic, nrow(object$data)))
}

#' Fitted Latent Values from a Revision Model
#'
#' Returns the smoothed estimate of the model's latent signal for the
#' in-sample periods, i.e. the revision-adjusted series. The signal is the
#' latent efficient value for a `kk_model` and the latent true value for a
#' `jvn_model`.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time`, `estimate`, `lower` and `upper`.
#' @method fitted revision_model
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
fitted.revision_model <- function(object, ...) {
  signal_path(object)
}

#' Residuals of a Revision Model
#'
#' Difference between the observed target release and the smoothed estimate of
#' the model's latent signal. These are measurement residuals of the release
#' the model treats as its target -- the efficient release for a `kk_model`,
#' the most mature release included in the estimation for a `jvn_model` --
#' not one-step-ahead prediction errors.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time` and `residual`.
#' @method residuals revision_model
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
residuals.revision_model <- function(object, ...) {
  fit_vals <- fitted(object)

  observed <- object$data[, c("time", target_column(object))]
  names(observed) <- c("time", "observed")

  merged <- merge(as.data.frame(observed), as.data.frame(fit_vals), by = "time")

  dplyr::tibble(
    time = merged$time,
    residual = merged$observed - merged$estimate
  )
}

#' Forecasts from a Revision Model
#'
#' Returns the out-of-sample estimates of the model's latent signal produced
#' by the forecast horizon `h` supplied to [kk_nowcast()] or [jvn_nowcast()].
#' The horizon is fixed at estimation time, so refit with a different `h` to
#' change it.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Ignored.
#'
#' @return A tibble with columns `time`, `estimate`, `lower` and `upper`.
#'   Has zero rows when the model was fitted with `h = 0`.
#' @method predict revision_model
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
predict.revision_model <- function(object, ...) {
  out <- signal_path(object, sample = "out_of_sample")

  # An empty result here is not an error -- the horizon is simply zero -- but
  # silently returning no rows reads like a failure, so say which argument
  # decides it.
  if (nrow(out) == 0) {
    rlang::inform(paste0(
      "This ",
      class(object)[1],
      " was fitted with `h = 0`, so it has no out-of-sample estimates. ",
      "Refit with `h > 0` to forecast beyond the observed sample."
    ))
  }

  out
}

#' Summary Method for Revision Models
#'
#' @description Computes and displays a summary of a fitted revision model,
#' including the estimated specification, convergence status, information
#' criteria, and parameter estimates. Defined once for the parent class
#' [revision_model]; the family-specific header and specification block are
#' supplied by the concrete class.
#'
#' @param object A fitted model object inheriting from [revision_model], such
#'   as a `kk_model` or a `jvn_model`.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The input `object`, invisibly.
#' @method summary revision_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, h = 2, model = "Kishor-Koenig", method = "MLE")
#' summary(fit)
#' @family revision nowcasting
#' @export
summary.revision_model <- function(object, ...) {
  cat(paste0("\n=== ", model_family(object), " Model ===\n\n"))

  for (line in spec_lines(object)) {
    cat(line, "\n")
  }

  if (!is.null(object$method)) {
    cat("Estimation method:", toupper(object$method), "\n")
  }

  if (!is.null(object$convergence)) {
    cat(
      "Convergence:",
      ifelse(
        object$convergence == 0,
        "Success",
        "Failed"
      ),
      "\n"
    )
  }

  if (!is.null(object$loglik)) {
    cat("Log-likelihood:", round(object$loglik, 2), "\n")
  }

  if (!is.null(object$aic)) {
    cat("AIC:", round(object$aic, 2), "\n")
  }

  if (!is.null(object$bic)) {
    cat("BIC:", round(object$bic, 2), "\n")
  }

  cat("\nParameter Estimates:\n")
  df_print <- object$params
  df_print$Estimate <- sprintf("%.3f", df_print$Estimate)
  df_print$Std.Error <- sprintf("%.3f", df_print$Std.Error)
  print(df_print, row.names = FALSE, quote = FALSE)

  cat("\n")
  invisible(object)
}

#' Print Method for Revision Models
#'
#' @description Default print method for objects inheriting from
#' [revision_model]. Dispatches to [summary.revision_model()] for a consistent
#' console display.
#'
#' @param x A fitted model object inheriting from [revision_model], such as a
#'   `kk_model` or a `jvn_model`.
#' @param ... Additional arguments passed to [summary.revision_model()].
#'
#' @return The input `x`, invisibly.
#' @method print revision_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- na.omit(dplyr::select(df, -c("id", "pub_date")))
#' fit <- kk_nowcast(df, e = 1, h = 2, model = "Kishor-Koenig", method = "MLE")
#' fit
#' @family revision nowcasting
#' @export
print.revision_model <- function(x, ...) {
  summary(x, ...)
}
