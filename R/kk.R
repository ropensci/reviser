#' Generalized Kishor-Koenig Model for Nowcasting
#'
#' Implements a generalized Kishor-Koenig (KK) model for nowcasting and
#' forecasting with state-space models, allowing for multiple vintages of data,
#' efficient estimation, and Kalman filtering and smoothing.
#'
#' @param df A data frame or single-ID vintages object in either long or wide
#'   format. Long-format vintages data are converted internally. After
#'   preprocessing, the data must contain a `time` column and at least `e + 1`
#'   releases.
#' @param e An integer indicating the number of data vintages to include in the
#'  model. Must be greater than 0.
#' @param h An integer specifying the forecast horizon. Default is 0, which
#' implies no forecasts. Must be greater than or equal to 0.
#' @param model A string specifying the type of model to use. Options are:
#'
#'  - "Kishor-Koenig" or "KK" (default): Full Kishor-Koenig model.
#'  - "Howrey": Howrey's simplified framework.
#'  - "Classical": Classical model without vintage effects.
#' @param method A string specifying the estimation method to use. Options are
#' "MLE" (Maximum Likelihood, default), "SUR", and "OLS".
#' @param alpha Significance level for confidence intervals (default = 0.05).
#' @param solver_options A named list controlling the SUR and MLE routines.
#'   Valid names are `trace`, `maxiter`, `startvals`, `solvtol`, `gradtol`,
#'   `steptol`, `transform_se`, `method`, `se_method`, `n_starts`, `seed`,
#'   `return_states`, `qml_eps`, `qml_score_method`, `qml_scale`,
#'   `sigma_lower`, `sigma_upper`, and `ic_n`. Supported entries are:
#'   \itemize{
#'     \item `trace`: integer controlling console output.
#'     \item `maxiter`: maximum number of optimizer iterations.
#'     \item `startvals`: optional numeric vector of starting values. Unnamed
#'       vectors are interpreted in the canonical internal order returned by
#'       `kk_matrices(e, model, type = "character")$params`. Named vectors are
#'       reordered to that same canonical order.
#'     \item `solvtol`: tolerance passed to `systemfit::nlsystemfit()` in the
#'       SUR estimator.
#'     \item `gradtol`: gradient tolerance passed to `systemfit::nlsystemfit()`
#'       in the SUR estimator.
#'     \item `steptol`: step-length tolerance passed to
#'       `systemfit::nlsystemfit()` in the SUR estimator.
#'     \item `transform_se`: logical; whether variance parameters are optimized
#'       on the log scale in MLE.
#'     \item `method`: optimization method for MLE; one of `"L-BFGS-B"`,
#'       `"BFGS"`, `"Nelder-Mead"`, `"nlminb"`, or `"two-step"`.
#'     \item `se_method`: standard-error method for MLE; one of `"hessian"`,
#'       `"qml"`, or `"none"`.
#'     \item `n_starts`: number of random starting points used by the MLE
#'       multi-start routine.
#'     \item `seed`: optional random seed used for the MLE multi-start
#'       perturbations.
#'     \item `return_states`: logical; whether filtered and smoothed states and
#'       the fitted KFAS model should be returned.
#'     \item `qml_eps`: finite-difference step size used in QML covariance
#'       estimation.
#'     \item `qml_score_method`: score approximation method; `"forward"` or
#'       `"central"`.
#'     \item `qml_scale`: scaling convention for the QML covariance; `"sum"`,
#'       `"mean"`, or `"hc"`.
#'     \item `sigma_lower`: lower bound for variance parameters on the natural
#'       scale when `transform_se = TRUE`.
#'     \item `sigma_upper`: upper bound for variance parameters on the natural
#'       scale when `transform_se = TRUE`.
#'     \item `ic_n`: sample-size convention used for BIC; `"T"` or `"Tp"`.
#'   }
#'   For backward compatibility, the legacy aliases `score_method` and
#'   `score_eps` are also accepted and mapped to `qml_score_method` and
#'   `qml_eps`.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{states}{A tibble containing filtered and smoothed state estimates. If
#'   `solver_options$return_states = FALSE`, this is `NULL`.}
#'   \item{kk_model_mat}{A list of KK model matrices, such as transition
#'   and observation matrices.}
#'   \item{ss_model_mat}{A list of state-space model matrices derived
#'   from the KK model.}
#'   \item{model}{The fitted `KFAS::SSModel` object. If
#'   `solver_options$return_states = FALSE`, this is `NULL`.}
#'   \item{params}{Estimated model parameters with standard errors.}
#'   \item{fit}{The raw fit object returned by the selected estimator.}
#'   \item{loglik}{Log-likelihood value for MLE fits; otherwise `NULL`.}
#'   \item{aic}{Akaike information criterion for MLE fits; otherwise `NULL`.}
#'   \item{bic}{Bayesian information criterion for MLE fits; otherwise `NULL`.}
#'   \item{convergence}{Convergence status.}
#'   \item{e}{The number of the efficient release (0-indexed).}
#'   \item{data}{The input data after preprocessing to wide format.}
#'   \item{se_method}{The standard-error method used by MLE; otherwise `NULL`.}
#'   \item{cov}{Estimated covariance matrix of the parameter estimates, if
#'   available; otherwise `NULL`.}
#' }
#'
#'
#' @examples
#' # Example usage:
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1 # Number of efficient release
#' h <- 2 # Forecast horizon
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")
#'
#' result$params
#'
#' @references Kishor, N. Kundan and Koenig, Evan F., "VAR Estimation and
#' Forecasting When Data Are Subject to Revision", Journal of Business and
#' Economic Statistics, 2012.
#' @srrstats {G1.0} academic literature.
#'
#' @details
#' The function supports multiple models, including the full Kishor-Koenig
#' framework, Howrey's model, and a classical approach. It handles data
#' preprocessing, estimation of system equations using Seemingly Unrelated
#' Regressions (SUR), and application of the Kalman filter. This is
#' the first openly available implementation of the Kishor-Koenig model (See
#' the vignette \code{vignette("nowcasting_revisions")} for more details).
#' @srrstats {G1.1} first implementation of a novel algorithm
#' @srrstats {G1.3} Terminology explained here and in vignette.
#' @srrstats {G2.3b} use `tolower()`
#' @srrstats {TS4.3} return data contains time colum
#' @srrstats {TS4.6} Time Series Software which implements or otherwise
#' enables forecasting should return either:
#' @srrstats {TS4.6b} filtered/forecasted point estimates
#' @srrstats {TS4.6c} Error indication for forecast estimates (confidence
#'   intervals)
#' @srrstats {TS4.7c} Distinguishes model vs forecast values (sample column)
#' @srrstats {TS4.7} forecast values and models separately returned
#' @srrstats {TS4.7a} only forecast values returned
#' @srrstats {TS4.7b} forecast values and models separately returned
#' @srrstats {G5.9a} `.Machine$double.eps` to data does not meaningfully
#' change results
#'
#' @family revision nowcasting
#' @export
kk_nowcast <- function(
  df,
  e,
  h = 0,
  model = "Kishor-Koenig",
  method = "MLE",
  alpha = 0.05,
  solver_options = list()
) {
  abort_scalar <- function(x, nm, cond, msg_extra = NULL) {
    if (length(x) != 1L || !is.finite(x) || !cond(x)) {
      msg <- paste0("'", nm, "' ", rlang::`%||%`(msg_extra, "is invalid."))
      rlang::abort(msg, call = rlang::caller_env())
    }
  }

  abort_flag <- function(x, nm) {
    if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
      rlang::abort(
        paste0("'", nm, "' must be a single TRUE/FALSE."),
        call = rlang::caller_env()
      )
    }
  }

  default_solver_options <- list(
    trace = 0,
    maxiter = 1000,
    startvals = NULL,
    solvtol = .Machine$double.eps,
    gradtol = 1e-6,
    steptol = 1e-6,
    transform_se = TRUE,
    method = "L-BFGS-B",
    se_method = "hessian",
    n_starts = 1,
    seed = NULL,
    return_states = TRUE,
    qml_eps = 1e-4,
    qml_score_method = "forward",
    qml_scale = "sum",
    sigma_lower = 1e-3,
    sigma_upper = 100,
    ic_n = "Tp"
  )

  model <- tolower(model)
  method <- tolower(method)

  if (!is.list(solver_options)) {
    rlang::abort("'solver_options' must be a list!")
  }

  if (!is.null(solver_options$score_method) &&
      is.null(solver_options$qml_score_method)) {
    solver_options$qml_score_method <- solver_options$score_method
  }

  if (!is.null(solver_options$score_eps) &&
      is.null(solver_options$qml_eps)) {
    solver_options$qml_eps <- solver_options$score_eps
  }

  invalid_solver_options <- setdiff(
    names(solver_options),
    names(default_solver_options)
  )
  if (length(invalid_solver_options) > 0) {
    rlang::abort(paste0(
      "Invalid solver options provided. Valid options are: ",
      paste(names(default_solver_options), collapse = ", ")
    ))
  }

  if (length(solver_options) > 0) {
    for (name in names(solver_options)) {
      default_solver_options[[name]] <- solver_options[[name]]
    }
  }

  abort_scalar(
    e,
    "e",
    \(x) x > 0 && x %% 1 == 0,
    "must be a single whole number greater than 0."
  )
  e <- as.integer(e)

  if (e == 0L) {
    rlang::abort("The initial release is already efficient, 'e' is equal to 0!")
  }

  abort_scalar(
    h,
    "h",
    \(x) x >= 0 && x %% 1 == 0,
    "must be at least 0."
  )
  h <- as.integer(h)

  abort_scalar(alpha, "alpha", \(x) x > 0 && x < 1, "must be in (0, 1).")

  if (!model %in% tolower(c("Kishor-Koenig", "KK", "Howrey", "Classical"))) {
    rlang::abort(
      "'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!"
    )
  }

  if (!method %in% tolower(c("SUR", "MLE", "OLS"))) {
    rlang::abort("'method' must be one of 'SUR', 'MLE', or 'OLS'!")
  }

  abort_flag(default_solver_options$transform_se, "solver_options$transform_se")
  abort_flag(
    default_solver_options$return_states,
    "solver_options$return_states"
  )

  default_solver_options$method <- match.arg(
    default_solver_options$method,
    c("L-BFGS-B", "BFGS", "Nelder-Mead", "nlminb", "two-step")
  )
  default_solver_options$se_method <- match.arg(
    default_solver_options$se_method,
    c("hessian", "qml", "none")
  )
  default_solver_options$qml_score_method <- match.arg(
    default_solver_options$qml_score_method,
    c("forward", "central")
  )
  default_solver_options$qml_scale <- match.arg(
    default_solver_options$qml_scale,
    c("sum", "mean", "hc")
  )
  default_solver_options$ic_n <- match.arg(
    default_solver_options$ic_n,
    c("T", "Tp")
  )

  abort_scalar(
    default_solver_options$trace,
    "solver_options$trace",
    \(x) x >= 0,
    "must be a single number >= 0."
  )
  default_solver_options$trace <- as.integer(
    round(default_solver_options$trace)
  )

  abort_scalar(
    default_solver_options$maxiter,
    "solver_options$maxiter",
    \(x) x >= 1 && x %% 1 == 0,
    "must be a single integer >= 1."
  )
  default_solver_options$maxiter <- as.integer(
    round(default_solver_options$maxiter)
  )

  abort_scalar(
    default_solver_options$n_starts,
    "solver_options$n_starts",
    \(x) x >= 1 && x %% 1 == 0,
    "must be a single integer >= 1."
  )
  default_solver_options$n_starts <- as.integer(
    round(default_solver_options$n_starts)
  )

  abort_scalar(
    default_solver_options$qml_eps,
    "solver_options$qml_eps",
    \(x) x > 0,
    "must be > 0."
  )
  abort_scalar(
    default_solver_options$sigma_lower,
    "solver_options$sigma_lower",
    \(x) x > 0,
    "must be > 0."
  )
  abort_scalar(
    default_solver_options$sigma_upper,
    "solver_options$sigma_upper",
    \(x) x > default_solver_options$sigma_lower,
    "must be > solver_options$sigma_lower."
  )

  if (!is.null(default_solver_options$seed)) {
    abort_scalar(
      default_solver_options$seed,
      "solver_options$seed",
      \(x) x %% 1 == 0,
      "must be a single integer."
    )
    default_solver_options$seed <- as.integer(
      round(default_solver_options$seed)
    )
  }

  if (!is.null(default_solver_options$startvals) &&
      !is.numeric(default_solver_options$startvals)) {
    rlang::abort("'startvals' must be a numeric vector.")
  }

  n_param_mat <- dplyr::if_else(
    model %in% tolower(c("Kishor-Koenig", "KK")),
    1 + e + e^2,
    dplyr::if_else(
      model == tolower("Howrey"),
      1 + e^2,
      1
    )
  )
  n_param_cov <- e + 1
  n_param <- n_param_mat + n_param_cov

  kk_mat_sur <- kk_matrices(e = e, model = model, type = "character")
  param_names <- names(kk_mat_sur$params)

  start_params <- NULL
  if (!is.null(default_solver_options$startvals)) {
    if (length(default_solver_options$startvals) != n_param) {
      rlang::abort(paste0(
        "The length of 'startvals' must be ",
        n_param,
        " if 'model' = ",
        model,
        " and e = ",
        e
      ))
    }
    start_params <- kk_matrices(
      e = e,
      model = model,
      params = default_solver_options$startvals,
      type = "numeric"
    )$params
  }

  start_mat <- if (is.null(start_params)) {
    out <- rep(0.4, n_param_mat)
    names(out) <- param_names[seq_len(n_param_mat)]
    out
  } else {
    start_params[seq_len(n_param_mat)]
  }

  check <- vintages_check(df)

  if (is.list(check)) {
    if (length(check) > 1) {
      rlang::abort(paste0(
        "'df' must contain a single ID, but ",
        length(check),
        " IDs were provided: ",
        paste(names(check), collapse = ", ")
      ))
    }
    df <- df[[1]]
    check <- check[[1]]
  }

  if (check == "long") {
    if ("id" %in% colnames(df) && length(unique(df$id)) > 1) {
      rlang::abort(paste0(
        "'df' contains ",
        length(unique(df$id)),
        " different IDs. Filter to a single ID first."
      ))
    }
    df <- suppressWarnings(vintages_wide(df, names_from = "release"))
    if (is.list(df) && !is.data.frame(df)) df <- df[[1]]
  }

  if (!("time" %in% names(df))) {
    rlang::abort(
      "After preprocessing, 'df' must contain a 'time' column."
    )
  }

  df <- df[, c("time", setdiff(names(df), "time")), drop = FALSE]

  time_in <- df$time
  if ((inherits(time_in, "Date") ||
        inherits(time_in, "POSIXt") ||
        is.numeric(time_in) ||
        is.integer(time_in)) &&
      any(diff(as.numeric(time_in)) <= 0, na.rm = TRUE)) {
    rlang::abort("'time' must be strictly increasing.")
  }

  required_release_cols <- paste0("release_", 0:e)
  missing_release_cols <- setdiff(required_release_cols, colnames(df))
  if (length(missing_release_cols) > 0) {
    rlang::abort(paste0(
      "'df' must contain release columns ",
      paste(required_release_cols, collapse = ", "),
      " for e = ",
      e,
      ". Missing: ",
      paste(missing_release_cols, collapse = ", ")
    ))
  }

  z_names <- c(paste0("release_", e, "_lag_", e:0))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))
  n_states <- length(c(z_names, y_names))

  equations <- kk_equations(kk_mat_sur = kk_mat_sur)
  sur_data <- kk_arrange_data(df = df, e = e)
  Ymat <- sur_data |>
    dplyr::select(dplyr::all_of(y_names)) |>
    as.matrix()

  kk_spec <- list(
    e = e,
    model = model,
    n_states = n_states,
    param_names = param_names,
    n_diffuse = ncol(Ymat),
    n_contrib = nrow(Ymat) * ncol(Ymat) - ncol(Ymat)
  )

  ar_order <- 1
  if (default_solver_options$trace > 0) {
    cat(
      "Estimating",
      model,
      "model with",
      n_param,
      "parameters...\n"
    )
    cat("Estimation:", method, "\n")
    cat("AR order:", ar_order, "\n")
  }

  params <- NULL
  fit <- NULL
  loglik <- NULL
  aic <- NULL
  bic <- NULL
  convergence <- NULL
  params_raw <- NULL
  se_raw <- NULL
  se <- NULL
  cov_used <- NULL
  se_method_used <- NULL

  if (method == "sur") {
    fit <- systemfit::nlsystemfit(
      equations,
      method = "SUR",
      data = sur_data,
      startvals = start_mat,
      print.level = default_solver_options$trace,
      maxiter = default_solver_options$maxiter,
      solvtol = default_solver_options$solvtol,
      steptol = default_solver_options$steptol,
      gradtol = default_solver_options$gradtol
    )

    parm_cov <- diag(fit$rcov)
    names(parm_cov) <- c("v0", paste0("eps", (e - 1):0))
    params <- kk_matrices(
      e = e,
      model = model,
      params = c(fit$b, parm_cov),
      type = "numeric"
    )$params

    if (!is.null(fit$se)) {
      se_raw <- fit$se
    } else {
      se_raw <- rep(NA_real_, length(fit$b))
      rlang::warn(
        "Could not extract standard errors from SUR estimation",
        call. = FALSE
      )
    }

    n_obs_sur <- nrow(sur_data)
    k_params <- length(fit$b)
    var_se <- parm_cov * sqrt(2 / (n_obs_sur - k_params))
    se_raw <- c(se_raw, var_se)
    convergence <- fit$convergence
  } else if (method == "ols") {
    ols_estim <- kk_ols_estim(
      equations = equations,
      model = model,
      data = sur_data
    )

    fit <- ols_estim$fit
    params <- kk_matrices(
      e = e,
      model = model,
      params = ols_estim$params,
      type = "numeric"
    )$params

    se_raw <- numeric(length(params))
    idx <- 1
    for (i in seq_along(fit)) {
      if (!is.null(fit[[i]])) {
        model_se <- summary(fit[[i]])$coefficients[, "Std. Error"]
        se_raw[idx:(idx + length(model_se) - 1)] <- model_se
        idx <- idx + length(model_se)
      }
    }

    var_idx <- grep("v0|eps", names(params))
    if (length(var_idx) > 0) {
      n_obs_ols <- nrow(fit[[1]]$model)
      k_params <- length(fit[[1]]$coefficients)
      se_raw[var_idx] <- params[var_idx] * sqrt(2 / (n_obs_ols - k_params))
    }

    convergence <- 0
  } else if (method == "mle") {
    start_mat_mle <- if (is.null(start_params)) {
      kk_matrices(
        e = e,
        model = model,
        params = kk_ols_estim(equations, sur_data, model)$params,
        type = "numeric"
      )$params
    } else {
      start_params
    }

    param_groups <- kk_param_groups(names(start_mat_mle))
    if (isTRUE(default_solver_options$transform_se) &&
        length(param_groups$var_idx) > 0) {
      start_mat_mle[param_groups$var_idx] <- log(
        pmax(
          start_mat_mle[param_groups$var_idx],
          default_solver_options$sigma_lower
        )
      )
    }

    lower_bounds <- rep(-Inf, length(start_mat_mle))
    upper_bounds <- rep(Inf, length(start_mat_mle))
    if (isTRUE(default_solver_options$transform_se) &&
        length(param_groups$var_idx) > 0) {
      lower_bounds[param_groups$var_idx] <- log(
        default_solver_options$sigma_lower
      )
      upper_bounds[param_groups$var_idx] <- log(
        default_solver_options$sigma_upper
      )
    }
    if (length(param_groups$dyn_idx) > 0) {
      lower_bounds[param_groups$dyn_idx] <- -0.99
      upper_bounds[param_groups$dyn_idx] <- 0.99
    }

    reviser_with_seed(default_solver_options$seed, {
      n_starts <- default_solver_options$n_starts
      all_results <- vector("list", n_starts)
      all_values <- rep(NA_real_, n_starts)

      if (n_starts > 1 && default_solver_options$trace > 0) {
        cat(
          "\nUsing multi-start optimization with",
          n_starts,
          "starting points\n"
        )
        cat("Method:", default_solver_options$method, "\n\n")
      }

      for (start_idx in seq_len(n_starts)) {
        if (start_idx == 1L) {
          current_init <- start_mat_mle
        } else {
          current_init <- start_mat_mle +
            stats::rnorm(length(start_mat_mle), 0, 0.5)

          if (isTRUE(default_solver_options$transform_se) &&
              length(param_groups$var_idx) > 0) {
            current_init[param_groups$var_idx] <- pmax(
              pmin(
                current_init[param_groups$var_idx],
                log(default_solver_options$sigma_upper)
              ),
              log(default_solver_options$sigma_lower)
            )
          }
          if (length(param_groups$dyn_idx) > 0) {
            current_init[param_groups$dyn_idx] <- pmax(
              pmin(current_init[param_groups$dyn_idx], 0.99),
              -0.99
            )
          }
        }

        opt_method <- default_solver_options$method

        if (opt_method == "two-step") {
          opt_nm <- stats::optim(
            par = current_init,
            fn = kk_negloglik,
            kk_spec = kk_spec,
            Ymat = Ymat,
            transform_se = default_solver_options$transform_se,
            method = "Nelder-Mead",
            control = list(
              trace = max(0, default_solver_options$trace),
              maxit = 500
            )
          )

          current_result <- stats::optim(
            par = opt_nm$par,
            fn = kk_negloglik,
            kk_spec = kk_spec,
            Ymat = Ymat,
            transform_se = default_solver_options$transform_se,
            method = "BFGS",
            control = list(
              trace = max(0, default_solver_options$trace),
              maxit = default_solver_options$maxiter
            ),
            hessian = FALSE
          )
        } else if (opt_method == "L-BFGS-B") {
          current_result <- stats::optim(
            par = current_init,
            fn = kk_negloglik,
            kk_spec = kk_spec,
            Ymat = Ymat,
            transform_se = default_solver_options$transform_se,
            method = "L-BFGS-B",
            lower = lower_bounds,
            upper = upper_bounds,
            control = list(
              trace = max(0, default_solver_options$trace),
              maxit = default_solver_options$maxiter
            ),
            hessian = FALSE
          )
        } else if (opt_method == "nlminb") {
          opt_result_nlminb <- stats::nlminb(
            start = current_init,
            objective = kk_negloglik,
            kk_spec = kk_spec,
            Ymat = Ymat,
            transform_se = default_solver_options$transform_se,
            lower = lower_bounds,
            upper = upper_bounds,
            control = list(
              trace = max(0, default_solver_options$trace),
              eval.max = default_solver_options$maxiter * 2,
              iter.max = default_solver_options$maxiter
            )
          )

          current_result <- list(
            par = opt_result_nlminb$par,
            value = opt_result_nlminb$objective,
            convergence = opt_result_nlminb$convergence,
            message = opt_result_nlminb$message
          )
        } else {
          current_result <- stats::optim(
            par = current_init,
            fn = kk_negloglik,
            kk_spec = kk_spec,
            Ymat = Ymat,
            transform_se = default_solver_options$transform_se,
            method = opt_method,
            control = list(
              trace = max(0, default_solver_options$trace),
              maxit = default_solver_options$maxiter
            ),
            hessian = FALSE
          )
        }

        all_results[[start_idx]] <- current_result
        all_values[start_idx] <- current_result$value

        if (n_starts > 1 && default_solver_options$trace > 0) {
          cat("Negative log-likelihood:", round(current_result$value, 4), "\n")
          cat("Convergence:", current_result$convergence, "\n\n")
        }
      }

      best_idx <- which.min(ifelse(is.finite(all_values), all_values, Inf))
      opt_result <- all_results[[best_idx]]

      if (n_starts > 1 && default_solver_options$trace > 0) {
        cat("=== Multi-start Summary ===\n")
        cat("Best result from starting point", best_idx, "\n")
        cat("Negative log-likelihoods across starts:\n")
        for (i in seq_len(n_starts)) {
          marker <- if (i == best_idx) " <- BEST" else ""
          cat(sprintf("  Start %d: %.4f%s\n", i, all_values[i], marker))
        }
        cat("\n")
      }

      params_raw <- opt_result$par
      loglik <- -opt_result$value
      convergence <- opt_result$convergence
      fit <- opt_result
    })

    se_method_used <- default_solver_options$se_method
    se_warning <- NULL
    cov_raw <- NULL
    se_raw <- rep(NA_real_, length(params_raw))

    if (se_method_used == "hessian") {
      h_res <- tryCatch(
        kk_compute_hessian_cov(
          theta_hat = params_raw,
          kk_spec = kk_spec,
          Ymat = Ymat,
          transform_se = default_solver_options$transform_se
        ),
        error = function(e) {
          list(
            cov = NULL,
            warning = paste0("Hessian calculation failed: ", e$message)
          )
        }
      )
      cov_raw <- h_res$cov
      se_warning <- h_res$warning
      if (!is.null(cov_raw)) {
        se_raw <- sqrt(pmax(diag(cov_raw), 0))
        se_raw[is.nan(se_raw)] <- NA_real_
      }
    } else if (se_method_used == "qml") {
      qml_res <- tryCatch(
        kk_qml_covariance(
          theta_hat = params_raw,
          kk_spec = kk_spec,
          Ymat = Ymat,
          transform_se = default_solver_options$transform_se,
          score_eps = default_solver_options$qml_eps,
          score_method = default_solver_options$qml_score_method,
          qml_scale = default_solver_options$qml_scale
        ),
        error = function(e) {
          se_warning <<- paste0("QML covariance failed: ", e$message)
          NULL
        }
      )

      if (!is.null(qml_res) && !is.null(qml_res$cov)) {
        cov_raw <- qml_res$cov
        se_raw <- sqrt(pmax(diag(cov_raw), 0))
      } else if (is.null(se_warning)) {
        se_warning <- "QML covariance returned NULL/invalid; SEs unavailable."
      }
    }

    tr <- kk_transform_params_and_cov(
      params_raw = params_raw,
      cov_raw = cov_raw,
      param_names = kk_spec$param_names,
      transform_se = default_solver_options$transform_se
    )

    params <- kk_matrices(
      e = e,
      model = model,
      params = tr$params,
      type = "numeric"
    )$params
    cov_used <- tr$cov
    se <- tr$se

    if (is.null(cov_used) && any(is.finite(se_raw))) {
      se <- se_raw
      if (isTRUE(default_solver_options$transform_se) &&
          length(tr$idx_sd) > 0) {
        se[tr$idx_sd] <- exp(params_raw[tr$idx_sd]) * se_raw[tr$idx_sd]
      }
    }

    if (!is.null(se_warning) && default_solver_options$trace > 0) {
      warning(se_warning, call. = FALSE)
    }

    n_ic <- if (default_solver_options$ic_n == "Tp") {
      nrow(Ymat) * ncol(Ymat)
    } else {
      nrow(Ymat)
    }
    aic <- -2 * loglik + 2 * n_param
    bic <- -2 * loglik + log(n_ic) * n_param
  }

  se_out <- if (method == "mle") se else se_raw
  param_table <- data.frame(
    Parameter = names(params),
    Estimate = unname(params),
    Std.Error = unname(se_out),
    row.names = NULL
  )

  kk_mat_hat <- kk_matrices(
    e = e,
    model = model,
    params = params,
    type = "numeric"
  )
  sur_ss_mat <- kk_to_ss(
    FF = kk_mat_hat$FF,
    GG = kk_mat_hat$GG,
    V = kk_mat_hat$V,
    W = kk_mat_hat$W,
    epsilon = 1e-6
  )
  kk_mat_hat$params <- NULL

  if (!isTRUE(default_solver_options$return_states)) {
    results <- list(
      states = NULL,
      kk_model_mat = kk_mat_hat,
      ss_model_mat = sur_ss_mat,
      model = NULL,
      params = param_table,
      fit = fit,
      loglik = loglik,
      aic = aic,
      bic = bic,
      convergence = convergence,
      e = e,
      data = df,
      se_method = se_method_used,
      cov = cov_used
    )
    class(results) <- c("kk_model", class(results))
    return(results)
  }

  if (h > 0) {
    frequency <- unique(round(as.numeric(diff(df$time)) / 30))
    if (length(frequency) > 1) {
      rlang::abort(
        paste(
          "The time series seems not to be regular.",
          "Please provide a regular time series!"
        )
      )
    }

    forecast_dates <- seq.Date(
      df$time[nrow(df)],
      by = paste0(frequency, " months"),
      length.out = h + 1
    )[2:(h + 1)]

    output_dates <- c(as.Date(rownames(Ymat)), forecast_dates)
    y_kfas <- rbind(Ymat, matrix(NA_real_, h, ncol(Ymat)))
  } else {
    output_dates <- c(as.Date(rownames(Ymat)))
    forecast_dates <- as.Date(character(0))
    y_kfas <- Ymat
  }

  model_kfas <- KFAS::SSModel(
    y_kfas ~
      -1 +
        SSMcustom(
          Z = sur_ss_mat$Z,
          T = sur_ss_mat$Tmat,
          R = sur_ss_mat$R,
          Q = sur_ss_mat$Q,
          a1 = c(rep(0.2, n_states / 2), rep(0, n_states / 2)),
          P1inf = diag(c(rep(1, n_states / 2), rep(0, n_states / 2)), n_states),
          P1 = diag(c(rep(0, n_states / 2), rep(1, n_states / 2)), n_states),
          index = seq_len(ncol(y_kfas))
        ),
    H = sur_ss_mat$H
  )

  kalman <- KFAS::KFS(model_kfas)

  state_results <- vector("list", length(z_names))
  for (i in seq_along(z_names)) {
    filtered_est <- kalman$att[, i]
    filtered_se <- sqrt(kalman$Ptt[i, i, ])
    smoothed_est <- kalman$alphahat[, i]
    smoothed_se <- sqrt(kalman$V[i, i, ])

    filtered_df <- dplyr::tibble(
      time = output_dates,
      state = z_names[i],
      estimate = filtered_est,
      lower = filtered_est - stats::qnorm(1 - alpha / 2) * filtered_se,
      upper = filtered_est + stats::qnorm(1 - alpha / 2) * filtered_se,
      filter = "filtered",
      sample = dplyr::if_else(
        output_dates %in% forecast_dates,
        "out_of_sample",
        "in_sample"
      )
    )

    smoothed_df <- dplyr::tibble(
      time = output_dates,
      state = z_names[i],
      estimate = smoothed_est,
      lower = smoothed_est - stats::qnorm(1 - alpha / 2) * smoothed_se,
      upper = smoothed_est + stats::qnorm(1 - alpha / 2) * smoothed_se,
      filter = "smoothed",
      sample = dplyr::if_else(
        output_dates %in% forecast_dates,
        "out_of_sample",
        "in_sample"
      )
    )

    state_results[[i]] <- dplyr::bind_rows(filtered_df, smoothed_df)
  }

  states_long <- dplyr::bind_rows(state_results) |>
    dplyr::as_tibble() |>
    dplyr::arrange(.data$filter, .data$state, .data$time)

  results <- list(
    states = states_long,
    kk_model_mat = kk_mat_hat,
    ss_model_mat = sur_ss_mat,
    model = model_kfas,
    params = param_table,
    fit = fit,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = convergence,
    e = e,
    data = df,
    se_method = se_method_used,
    cov = cov_used
  )
  class(results) <- c("kk_model", class(results))
  results
}

#' Internal parameter grouping for KK estimation
#'
#' @keywords internal
#' @noRd
kk_param_names <- function(e, model) {
  model <- tolower(model)

  g_names <- character(0)
  if (model %in% tolower(c("Kishor-Koenig", "KK"))) {
    for (i in seq_len(e)) {
      for (j in seq_len(e + 1)) {
        g_names <- c(g_names, paste0("G", e - i, "_", e - j + 1))
      }
    }
  } else if (model == tolower("Howrey")) {
    for (i in seq_len(e)) {
      for (j in seq_len(e)) {
        g_names <- c(g_names, paste0("G", e - i, "_", e - j + 1))
      }
    }
  }

  c("F0", sort(g_names), "v0", paste0("eps", seq.int(0, e - 1)))
}

#' Normalize KK parameters to the canonical internal order
#'
#' @keywords internal
#' @noRd
kk_normalize_params <- function(params, param_names) {
  input_names <- names(params)
  params <- as.numeric(params)
  names(params) <- input_names

  if (is.null(input_names)) {
    names(params) <- param_names
    return(params)
  }

  if (anyNA(input_names) || any(input_names == "")) {
    rlang::abort(
      "If 'params' is named, all parameter names must be non-empty."
    )
  }

  if (anyDuplicated(input_names)) {
    rlang::abort("Parameter names must be unique.")
  }

  missing_names <- setdiff(param_names, input_names)
  extra_names <- setdiff(input_names, param_names)

  if (length(missing_names) > 0 || length(extra_names) > 0) {
    details <- c(
      if (length(missing_names) > 0) {
        paste0("missing: ", paste(missing_names, collapse = ", "))
      },
      if (length(extra_names) > 0) {
        paste0("unexpected: ", paste(extra_names, collapse = ", "))
      }
    )
    rlang::abort(paste(
      "Named 'params' must match the canonical parameter names for this model.",
      paste(details, collapse = "; ")
    ))
  }

  params[param_names]
}

#' Internal parameter grouping for KK estimation
#'
#' @keywords internal
#' @noRd
kk_param_groups <- function(param_names) {
  list(
    var_idx = grep("v0|eps", param_names),
    dyn_idx = grep("^F0$|^G", param_names)
  )
}

#' Build the KFAS model used by KK estimation
#'
#' @keywords internal
#' @noRd
kk_build_kfas_model <- function(params, kk_spec, Ymat, transform_se = TRUE) {
  theta <- as.numeric(params)
  names(theta) <- kk_spec$param_names

  groups <- kk_param_groups(kk_spec$param_names)

  if (isTRUE(transform_se) && length(groups$var_idx) > 0) {
    theta[groups$var_idx] <- exp(theta[groups$var_idx])
  }

  if (length(groups$var_idx) > 0 &&
      any(!is.finite(theta[groups$var_idx]) | theta[groups$var_idx] <= 0)) {
    return(NULL)
  }

  if (length(groups$dyn_idx) > 0 &&
      any(!is.finite(theta[groups$dyn_idx]) |
          abs(theta[groups$dyn_idx]) >= 0.999)) {
    return(NULL)
  }

  kk_mat <- tryCatch(
    kk_matrices(
      e = kk_spec$e,
      model = kk_spec$model,
      params = theta,
      type = "numeric"
    ),
    error = function(e) NULL
  )
  if (is.null(kk_mat)) {
    return(NULL)
  }

  ss_mat <- tryCatch(
    kk_to_ss(kk_mat$FF, kk_mat$GG, kk_mat$V, kk_mat$W),
    error = function(e) NULL
  )
  if (is.null(ss_mat)) {
    return(NULL)
  }

  n_states <- kk_spec$n_states
  model_kfas <- tryCatch(
    KFAS::SSModel(
      Ymat ~ -1 +
        SSMcustom(
          Z = ss_mat$Z,
          T = ss_mat$Tmat,
          R = ss_mat$R,
          Q = ss_mat$Q,
          a1 = c(rep(0.2, n_states / 2), rep(0, n_states / 2)),
          P1inf = diag(c(rep(1, n_states / 2), rep(0, n_states / 2)), n_states),
          P1 = diag(c(rep(0, n_states / 2), rep(1, n_states / 2)), n_states),
          index = seq_len(ncol(Ymat))
        ),
      H = ss_mat$H
    ),
    error = function(e) NULL
  )

  if (is.null(model_kfas)) {
    return(NULL)
  }

  list(
    params = kk_mat$params,
    ss_mat = ss_mat,
    model = model_kfas
  )
}

#' Sequential negative log-likelihood contributions for the KK model
#'
#' @keywords internal
#' @noRd
kk_negloglik_contrib <- function(params, kk_spec, Ymat, transform_se = TRUE) {
  n_contrib <- max(1L, kk_spec$n_contrib)
  build <- kk_build_kfas_model(params, kk_spec, Ymat, transform_se)
  if (is.null(build)) {
    return(rep(1e8, n_contrib))
  }

  kfs <- tryCatch(
    KFAS::KFS(build$model, filtering = "state", smoothing = "none"),
    error = function(e) NULL
  )
  if (is.null(kfs)) {
    return(rep(1e8, n_contrib))
  }

  contrib_mat <- matrix(NA_real_, nrow = nrow(Ymat), ncol = ncol(Ymat))
  log2pi <- log(2 * pi)

  for (t in seq_len(nrow(Ymat))) {
    for (i in seq_len(ncol(Ymat))) {
      F_ti <- kfs$F[i, t]
      v_ti <- kfs$v[t, i]
      if (!is.finite(F_ti) || F_ti <= 0 || !is.finite(v_ti)) {
        contrib_mat[t, i] <- NA_real_
      } else {
        contrib_mat[t, i] <- 0.5 * (log(F_ti) + v_ti^2 / F_ti + log2pi)
      }
    }
  }

  contrib <- as.vector(t(contrib_mat))
  n_diffuse <- sum(kfs$Finf > 0, na.rm = TRUE)
  if (n_diffuse > 0) {
    contrib <- contrib[-seq_len(min(n_diffuse, length(contrib)))]
  }

  if (length(contrib) != n_contrib) {
    return(rep(1e8, n_contrib))
  }

  contrib[!is.finite(contrib)] <- 1e8
  contrib
}

#' Total negative log-likelihood for the KK model
#'
#' @keywords internal
#' @noRd
kk_negloglik <- function(params, kk_spec, Ymat, transform_se = TRUE) {
  contrib <- kk_negloglik_contrib(
    params = params,
    kk_spec = kk_spec,
    Ymat = Ymat,
    transform_se = transform_se
  )

  contrib <- as.numeric(contrib)
  if (length(contrib) == 0L || any(!is.finite(contrib))) {
    return(1e10)
  }

  obj <- sum(contrib)
  if (!is.finite(obj)) 1e10 else obj
}

#' Hessian covariance helper for KK estimation
#'
#' @keywords internal
#' @noRd
kk_compute_hessian_cov <- function(
  theta_hat,
  kk_spec,
  Ymat,
  transform_se = TRUE
) {
  H <- numDeriv::hessian(
    func = kk_negloglik,
    x = theta_hat,
    kk_spec = kk_spec,
    Ymat = Ymat,
    transform_se = transform_se,
    method.args = list(eps = 1e-4, d = 0.01, r = 6)
  )

  cond_num <- tryCatch(kappa(H, exact = FALSE), error = function(e) Inf)
  if (!is.finite(cond_num) || cond_num > 1e10) {
    return(list(
      cov = NULL,
      warning = paste0(
        "Hessian is poorly conditioned",
        if (is.finite(cond_num)) {
          paste0(
            " (cond = ",
            format(cond_num, scientific = TRUE, digits = 2),
            ")"
          )
        } else {
          ""
        },
        "; standard errors may be unreliable."
      )
    ))
  }

  invH <- tryCatch(
    solve(H),
    error = function(e) {
      ridge <- 1e-6 * mean(abs(diag(H)))
      tryCatch(solve(H + ridge * diag(nrow(H))), error = function(e2) NULL)
    }
  )

  if (is.null(invH)) {
    return(list(
      cov = NULL,
      warning = "Failed to invert Hessian; standard errors unavailable."
    ))
  }

  invH <- (invH + t(invH)) / 2
  list(cov = invH, warning = NULL)
}

#' Transform KK parameters and covariance back to the natural scale
#'
#' @keywords internal
#' @noRd
kk_transform_params_and_cov <- function(
  params_raw,
  cov_raw,
  param_names,
  transform_se = TRUE
) {
  params <- as.numeric(params_raw)
  names(params) <- param_names
  cov_used <- cov_raw

  idx_sd <- grep("v0|eps", param_names)

  if (isTRUE(transform_se) && length(idx_sd) > 0) {
    params[idx_sd] <- exp(params_raw[idx_sd])

    if (!is.null(cov_raw)) {
      J <- diag(length(params_raw))
      J[idx_sd, idx_sd] <- diag(exp(params_raw[idx_sd]))
      cov_used <- J %*% cov_raw %*% t(J)
      cov_used <- (cov_used + t(cov_used)) / 2
    } else {
      cov_used <- NULL
    }
  }

  se <- rep(NA_real_, length(params_raw))
  if (!is.null(cov_used)) {
    se <- sqrt(pmax(diag(cov_used), 0))
    se[is.nan(se)] <- NA_real_
  }

  list(params = params, cov = cov_used, se = se, idx_sd = idx_sd)
}

#' QML covariance for the KK estimator
#'
#' @keywords internal
#' @noRd
kk_qml_covariance <- function(
  theta_hat,
  kk_spec,
  Ymat,
  transform_se = TRUE,
  score_eps = 1e-4,
  score_method = c("central", "forward"),
  qml_scale = c("sum", "mean", "hc"),
  hess_args = list(method.args = list(
    eps = 1e-4,
    d = 0.01,
    r = 6
  )),
  ridge_factor = 1e-6
) {
  score_method <- match.arg(score_method)
  qml_scale <- match.arg(qml_scale)

  H <- do.call(
    numDeriv::hessian,
    c(list(
      func = kk_negloglik,
      x = theta_hat,
      kk_spec = kk_spec,
      Ymat = Ymat,
      transform_se = transform_se
    ), hess_args)
  )

  cond_num <- tryCatch(kappa(H, exact = FALSE), error = function(e) Inf)

  base_contrib <- kk_negloglik_contrib(theta_hat, kk_spec, Ymat, transform_se)
  nT <- length(base_contrib)
  k <- length(theta_hat)

  total_nll <- kk_negloglik(theta_hat, kk_spec, Ymat, transform_se)
  sum_contrib <- sum(base_contrib)

  scale_factor <- 1
  if (is.finite(total_nll) && is.finite(sum_contrib) && sum_contrib != 0) {
    rel_gap <- abs(total_nll - sum_contrib) / max(1, abs(total_nll))
    if (rel_gap > 1e-6) {
      scale_factor <- total_nll / sum_contrib
    }
  }

  scores <- matrix(0, nrow = nT, ncol = k)
  for (j in seq_len(k)) {
    step <- rep(0, k)
    step[j] <- score_eps

    c_plus <- kk_negloglik_contrib(
      theta_hat + step, kk_spec, Ymat, transform_se
    )
    if (length(c_plus) != nT) {
      stop(
        paste(
          "kk_negloglik_contrib returned different length under perturbation;",
          "cannot form scores."
        )
      )
    }

    if (score_method == "central") {
      c_minus <- kk_negloglik_contrib(
        theta_hat - step, kk_spec, Ymat, transform_se
      )
      if (length(c_minus) != nT) {
        stop(
          paste(
            "kk_negloglik_contrib returned different length under",
            "perturbation;",
            "cannot form scores."
          )
        )
      }
      scores[, j] <- (c_plus - c_minus) / (2 * score_eps)
    } else {
      scores[, j] <- (c_plus - base_contrib) / score_eps
    }
  }

  if (!isTRUE(all.equal(scale_factor, 1))) {
    scores <- scores * scale_factor
  }

  Xproduct <- crossprod(scores)
  Tobs <- nrow(scores)
  p <- ncol(scores)

  if (qml_scale == "mean") {
    Xproduct <- Xproduct / Tobs
  } else if (qml_scale == "hc") {
    if (Tobs <= p) stop("hc scaling requires Tobs > number of parameters.")
    Xproduct <- (Tobs / (Tobs - p)) * (Xproduct / Tobs)
  }

  invH <- tryCatch(
    solve(H),
    error = function(e) {
      ridge <- ridge_factor * mean(abs(diag(H)))
      solve(H + ridge * diag(nrow(H)))
    }
  )

  invH <- (invH + t(invH)) / 2
  cov_qml <- invH %*% Xproduct %*% invH
  cov_qml <- (cov_qml + t(cov_qml)) / 2

  list(
    cov = cov_qml,
    H = H,
    Xproduct = Xproduct,
    cond_num = cond_num,
    scale_factor = scale_factor
  )
}

#' @title Create Equations for Kishor-Koenig (KK) Models
#'
#' @description
#' This function generates a list of formula objects representing the equations
#' for the Kishor-Koenig (KK) models based on the provided KK matrix structure.
#'
#' @param kk_mat_sur A list containing the KK matrix structure,
#' including matrices `FF`, `II`, and `GG`.
#'
#' @return A list of formula objects representing the equations of the KK model.
#'
#' @details
#' The function constructs the equations based on the dimensions of the input
#' matrices and generates formulas for each equation. It utilizes lagged
#' variables and matrix operations to form the relationships.
#'
#' @keywords internal
#' @noRd
kk_equations <- function(kk_mat_sur) {
  e <- dim(kk_mat_sur$FF)[1] - 1

  II <- diag(e + 1)

  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))
  y_lag_names <- c(paste0("release_", e:0, "_lag_", (e + 1):1))

  lhs1 <- z_names
  rhs1 <- kk_mat_sur$FF %mx% z_lag_names

  lhs2 <- (y_names)
  rhs2 <- (
    ((II %diff% kk_mat_sur$GG) %prod% kk_mat_sur$FF) %mx% y_lag_names
  ) %sum%
    (kk_mat_sur$GG %mx% z_names)

  equations <- list()
  formula <- stats::as.formula(paste0(lhs1[e + 1], " ~ ", rhs1[e + 1]))
  equations[[paste0("eq", 1)]] <- formula
  eq <- 2
  for (i in 2:(e + 1)) {
    formula <- stats::as.formula(paste0(lhs2[i], " ~ ", rhs2[i]))
    equations[[paste0("eq", eq)]] <- formula
    eq <- eq + 1
  }
  return(equations)
}

#' @title Arrange Data for Kishor-Koenig (KK) Models
#'
#' @description
#' This function arranges the input data frame into a format suitable for
#' estimating  Kishor-Koenig (KK) models. It generates lagged variables and
#' combines them into a data frame.
#'
#' @param df A data frame containing the original data.
#' @param e An integer indicating the efficient release.
#'
#' @return A data frame with lagged variables, prepared for KK model estimation.
#'
#' @details
#' The function creates lagged versions of the release variables up to
#' the specified lag `e`. It constructs variables named `release_e_lag_e:0`,
#' `release_e_lag_(e+1):1`, `release_e:0_lag_e:0`, and
#' `release_e:0_lag_(e+1):1`. The function then combines these variables into
#' a single data frame, removing rows with missing values.
#'
#' @keywords internal
#' @noRd
kk_arrange_data <- function(df, e) {
  dates <- df$time
  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))

  # Arrange data
  z <- array(NA, c(nrow(df), e + 1))
  z_lag <- array(NA, c(nrow(df), e + 1))
  for (j in (e):0) {
    z[, (e + 1) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", e)]), j)
    z_lag[, (e + 1) - j] <- dplyr::lag(
      dplyr::pull(df[paste0("release_", e)]),
      j + 1
    )
  }
  z <- tibble::tibble(as.data.frame(z))
  colnames(z) <- z_names
  z_lag <- tibble::tibble(as.data.frame(z_lag))
  colnames(z_lag) <- z_lag_names

  y <- array(NA, c(nrow(df), e))
  y_lag <- array(NA, c(nrow(df), e))
  for (j in (e - 1):0) {
    y[, (e) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", j)]), j)
    y_lag[, (e) - j] <- dplyr::lag(
      dplyr::pull(df[paste0("release_", j)]),
      j + 1
    )
  }

  y <- tibble::tibble(as.data.frame(y))
  y_lag <- tibble::tibble(as.data.frame(y_lag))
  colnames(y) <- c(paste0("release_", (e - 1):0, "_lag_", (e - 1):0))
  colnames(y_lag) <- c(paste0("release_", (e - 1):0, "_lag_", e:1))

  data <- cbind(z, y, y_lag)
  rownames(data) <- dates
  data <- data |> tidyr::drop_na()

  return(data)
}

#' @title Estimate Generalized Kishor-Koenig (KK) Models via OLS
#'
#' @description
#' This function estimates the parameters of generalized KK models
#' using ordinary least squares (OLS). It processes a set of equations, extracts
#' necessary information, and fits linear models. This function is intended for
#' internal use within the package.
#'
#' @param equations A list of formula objects representing the equations of the
#'   KK model.
#' @param data A data frame containing the variables used in the equations.
#' @param model A character string specifying the model type. Must be one of
#'   "KK", "Kishor-Koenig", "Howrey", or "Classical". Defaults to "KK".
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item{\code{params}: A named numeric vector containing the estimated
#'           parameters (coefficients and variances).}
#'     \item{\code{fit}: A list of fitted \code{lm} model objects.}
#'   }
#'
#' @srrstats {G2.4b} convert  via `as.numeric()`
#'
#' @details
#' This function is designed to handle different variations of the KK model,
#' including "Classical", "Howrey", and the "KK" or "Kishor-Koenig" models.
#' It extracts coefficients and variances based on the specified model type.
#'
#' @keywords internal
#' @noRd
kk_ols_estim <- function(equations, data, model = "KK") {
  model <- tolower(model)
  n_eq <- length(equations)
  e <- n_eq - 1

  # Remove all brackets and the term "F0" from eq 1
  eq1 <- gsub("\\(|\\)", "", deparse(equations[[1]]))
  eq1 <- gsub("F0", "", eq1)
  eq1 <- gsub("\\*", "", eq1)
  formula <- paste0(eq1, " -1")

  F0_mod <- stats::lm(
    stats::as.formula(formula),
    data = data
  )

  fit <- list()

  fit[[1]] <- F0_mod

  F0 <- stats::coef(F0_mod)
  var_v <- summary(F0_mod)$sigma^2

  ols_coeffs <- c("F0" = as.numeric(F0))
  ols_vars <- c("v0" = as.numeric(var_v))

  for (ii in 2:n_eq) {
    eq <- deparse(equations[[ii]])
    # Step 1: Extract the signs
    signs <- unlist(regmatches(
      eq,
      gregexpr("(?<=[^\\w])[-+]", eq, perl = TRUE)
    ))

    # Step 2: Extract the variable names
    variables <- unlist(regmatches(
      eq,
      gregexpr("release_[^ )]+", eq)
    ))

    # Step 3: Extract the parameters
    pars <- unlist(regmatches(
      eq,
      gregexpr("G\\d+_\\d+", eq, perl = TRUE)
    ))

    # Step 4: Combine signs and variable names
    # Handle cases where the first term might not have an explicit sign
    if (length(signs) < (length(variables) - 1)) {
      signs <- c("+", signs) # Assume first term has implicit '+'
    }

    if (model == tolower("Classical")) {
      sig2 <- var(data[[variables[1]]] - data[[variables[2]]])
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
    } else if (model == tolower("Howrey")) {
      # Check length of extracted elements
      if (
        length(signs) != (length(variables) - 1) ||
          length(signs) != length(pars) && !ii == n_eq
      ) {
        rlang::abort(
          "Error: Incorrect number of elements extracted from equation"
        )
      }

      uniq_pars <- unique(pars)

      # Create transformed regressors based on gs mapping
      regressors <- stats::setNames(
        vector("list", length(uniq_pars)),
        uniq_pars
      )

      for (i in seq_along(pars)) {
        g <- pars[i]
        var <- variables[i + 1]
        sign <- ifelse(signs[i] == "+", 1, -1)

        if (is.null(regressors[[g]])) {
          regressors[[g]] <- sign * data[[var]]
        } else {
          regressors[[g]] <- regressors[[g]] + sign * data[[var]]
        }
      }

      # Convert to data frame
      df_regressors <- as.data.frame(regressors)

      # Add dependent variable
      if (ii == n_eq) {
        df_regressors[[variables[1]]] <- data[[variables[1]]] -
          data[[variables[length(variables)]]]
      } else {
        df_regressors[[variables[1]]] <- data[[variables[1]]]
      }

      # Construct formula
      formula <- stats::as.formula(paste(
        variables[1],
        "~",
        paste0(
          paste(names(df_regressors)[-ncol(df_regressors)], collapse = " + "),
          " -1"
        )
      ))

      # Estimate model
      modeli <- stats::lm(formula, data = df_regressors)
      ols_coeffs <- c(ols_coeffs, modeli$coefficients)
      sig2 <- summary(modeli)$sigma^2
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
      fit[[ii]] <- modeli
    } else if (model %in% tolower(c("KK", "Kishor-Koenig"))) {
      # Check length of extracted elements
      if (
        length(signs) != (length(variables) - 1) ||
          length(signs) != length(pars) && !ii == n_eq
      ) {
        rlang::abort(
          "Error: Incorrect number of elements extracted from equation"
        )
      }

      # Unique coefficients to estimate
      uniq_pars <- unique(pars)

      # Create transformed regressors based on gs mapping
      regressors <- stats::setNames(
        vector("list", length(uniq_pars)),
        uniq_pars
      )

      for (i in seq_along(pars)) {
        g <- pars[i]
        var <- variables[i + 1]
        sign <- ifelse(signs[i] == "+", 1, -1)

        if (is.null(regressors[[g]])) {
          if (i == 1 && ii == n_eq) {
            regressors[[g]] <- sign * data[[var]] * ols_coeffs["F0"]
          } else {
            regressors[[g]] <- sign * data[[var]]
          }
        } else {
          regressors[[g]] <- regressors[[g]] + sign * data[[var]]
        }
      }

      # Convert to data frame
      df_regressors <- as.data.frame(regressors)
      # Add dependent variable
      if (ii == n_eq) {
        df_regressors[[variables[1]]] <- data[[variables[1]]] -
          data[[variables[2]]] * ols_coeffs["F0"]
      } else {
        df_regressors[[variables[1]]] <- data[[variables[1]]]
      }

      # Construct formula
      formula <- stats::as.formula(paste(
        variables[1],
        "~",
        paste0(
          paste(names(df_regressors)[-length(df_regressors)], collapse = " + "),
          " -1"
        )
      ))

      # Estimate model
      modeli <- stats::lm(formula, data = df_regressors)
      ols_coeffs <- c(ols_coeffs, modeli$coefficients)
      sig2 <- summary(modeli)$sigma^2
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
      fit[[ii]] <- modeli
    }
  }
  params <- c(ols_coeffs, ols_vars)
  return(list(params = params, fit = fit))
}

#' Create Matrices for the generalized Kishor-Koenig (KK) model
#'
#' Constructs the matrices \eqn{I}, \eqn{F}, \eqn{G}, \eqn{V}, and \eqn{W}
#' used in state-space models, specifically for the Kishor-Koenig (KK), Howrey,
#'  or Classical frameworks.
#'
#' @param e Integer. The number of efficiency gaps (lags) in the model. Must
#' be greater than 0.
#' @param model Character. Specifies the type of model to use. Options are:
#'   \describe{
#'     \item{"Kishor-Koenig" or "KK"}{Uses the Kishor-Koenig framework with
#'     \eqn{e \times (e+1)} parameters for the \eqn{G} matrix.}
#'     \item{"Howrey"}{Uses the Howrey framework with \eqn{e \times e}
#'     parameters for the \eqn{G} matrix.}
#'     \item{"Classical"}{Uses a diagonal identity matrix for \eqn{G}.}
#'   }
#' @param params Numeric vector (optional). A vector of parameters to
#' initialize the matrices. If \code{NULL}, default values are used:
#'   \describe{
#'     \item{\code{type = "numeric"}}{A vector of params must be supplied.}
#'     \item{\code{type = "character"}}{Initializes named parameters
#'     as \code{NA_real_}.}
#'   }
#'   If provided, the length of \code{params} must match the number of
#'   parameters required by the specified model.
#' @param type Character. Specifies the type of matrices returned. Options are:
#'   \describe{
#'     \item{"numeric"}{Returns numeric matrices with parameter values.}
#'     \item{"character"}{Returns character matrices with parameter names.
#'     If \code{params} is provided, it is ignored.}
#'   }
#'
#' @return A list containing the following components:
#'   \describe{
#'     \item{\code{FF}}{State transition matrix (\eqn{F}). Size: \eqn{(e+1)
#'      \times (e+1)}.}
#'     \item{\code{GG}}{Control matrix (\eqn{G}). Size depends on the model
#'     and \code{e}.}
#'     \item{\code{V}}{State noise covariance matrix (\eqn{V}).
#'     Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{W}}{Observation noise covariance matrix (\eqn{W}).
#'     Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{params}}{The vector of parameters used to construct the
#'     matrices, including their names.}
#'   }
#'
#' @details The generalized Kishor-Koenig model consists of the following
#' equations:
#'
#' **State Equation:**
#' \deqn{ z_t = F z_{t-1} + \nu_t, \quad \nu_t \sim N(0, V)}
#'
#' **Observation Equation:**
#' \deqn{y_t = (I - G) F y_{t-1} + G z_t +
#'  \epsilon_t, \quad \epsilon_t \sim N(0, W)}
#'
#' where:
#' - \eqn{z_t} is the state vector.
#' - \eqn{y_t} is the observed data.
#' - \eqn{F} is the state transition matrix.
#' - \eqn{G} is the control matrix.
#' - \eqn{V} is the state noise covariance matrix.
#' - \eqn{W} is the observation noise covariance matrix.
#'
#' @srrstats {G2.4b} convert via via `as.numeric()`
#' @srrstats {G2.4c} convert via via `as.character()`
#' @srrstats {G3.0} Uses epsilon for numerical stability (epsilon parameter)
#'
#' @examples
#' # Example 1: Kishor-Koenig model with character matrices
#' matrices <- kk_matrices(e = 3, model = "KK", type = "character")
#' str(matrices)
#'
#' # Example 2: Kishor-Koenig model with e = 2
#' params <- rep(0.1, 17)
#' names(params) <- names(matrices$params)
#' matrices <- kk_matrices(
#'   e = 3, params = params, model = "KK", type = "numeric"
#' )
#' str(matrices)
#'
#' @keywords internal
#' @noRd
kk_matrices <- function(e, model, params = NULL, type = "numeric") {
  model <- tolower(model)

  # Check input e
  if (e == 0) {
    rlang::abort("The initial release is already efficient, 'e' is equal to 0!")
  }

  # Check model input
  if (!model %in% tolower(c("Kishor-Koenig", "KK", "Howrey", "Classical"))) {
    rlang::abort(
      "'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!"
    )
  }

  # Check type input
  if (!type %in% c("numeric", "character")) {
    rlang::abort("'type' must be one of 'numeric' or 'character'!")
  }

  # Check params input
  if (!is.null(params) && type == "character") {
    rlang::warn(
      "If argument 'type' is 'character', argument 'params' is ignored!"
    )
  }

  if (is.null(params) && type == "numeric") {
    rlang::abort(
      "If argument 'type' is 'numeric', argument 'params' must be provided!"
    )
  }

  # Check params input
  n_param_mat <- dplyr::if_else(
    model %in% tolower(c("Kishor-Koenig", "KK")),
    1 + e + e^2,
    dplyr::if_else(
      model == tolower("Howrey"),
      1 + e^2,
      1 # Classical
    )
  )

  n_param_cov <- e + 1

  n_param <- n_param_mat + n_param_cov
  param_names <- kk_param_names(e = e, model = model)

  if (!is.null(params) && length(params) != n_param) {
    rlang::abort(paste0(
      "'params' must have length ",
      n_param,
      ", not ",
      length(params)
    ))
  }

  if (type == "numeric") {
    params <- kk_normalize_params(params = params, param_names = param_names)
  } else if (type == "character") {
    params <- stats::setNames(rep(NA_character_, n_param), param_names)
  }

  # Define F matrix
  FF <- array(0, c(e + 1, e + 1))
  FF[1:(e), 2:(e + 1)] <- diag(e)
  if (type == "numeric") {
    FF[e + 1, e + 1] <- unname(params[["F0"]])
  } else if (type == "character") {
    FF[e + 1, e + 1] <- "F0"
  }

  # Define G matrix
  GG <- diag(e + 1)

  if (model %in% tolower(c("Kishor-Koenig", "KK"))) {
    # e * e+1 params for G
    for (i in 1:e) {
      for (j in 1:(e + 1)) {
        label <- paste0("G", e - i, "_", e - j + 1)
        if (type == "numeric") {
          GG[i + 1, j] <- unname(params[[label]])
        } else if (type == "character") {
          GG[i + 1, j] <- label
        }
      }
    }
  } else if (model == tolower("Howrey")) {
    # e * e params for G
    for (i in 1:e) {
      for (j in 1:e) {
        label <- paste0("G", e - i, "_", e - j + 1)
        if (type == "numeric") {
          GG[i + 1, j] <- unname(params[[label]])
        } else if (type == "character") {
          GG[i + 1, j] <- label
        }
      }
    }
  } else if (model == tolower("Classical")) {
    GG <- diag(e + 1)
  } else {
    rlang::abort("'model' not recognized")
  }

  # Get Variance-covariance matrices
  # State noise covariance
  V <- array(0, c(e + 1, e + 1))
  if (type == "numeric") {
    V[e + 1, e + 1] <- unname(params[["v0"]])
  } else if (type == "character") {
    V[e + 1, e + 1] <- "v0"
  }

  # Observation noise covariance
  W <- array(0, c(e + 1, e + 1))
  for (jj in 2:(e + 1)) {
    label <- paste0("eps", e + 1 - jj)
    if (type == "numeric") {
      W[jj, jj] <- unname(params[[label]])
    } else if (type == "character") {
      W[jj, jj] <- label
    }
  }

  if (type == "character") {
    FF <- apply(FF, c(1, 2), as.character)
    GG <- apply(GG, c(1, 2), as.character)
    V <- apply(V, c(1, 2), as.character)
    W <- apply(W, c(1, 2), as.character)
  } else if (type == "numeric") {
    FF <- apply(FF, c(1, 2), as.numeric)
    GG <- apply(GG, c(1, 2), as.numeric)
    V <- apply(V, c(1, 2), as.numeric)
    W <- apply(W, c(1, 2), as.numeric)
  }

  return(list(FF = FF, GG = GG, V = V, W = W, params = params))
}


#' Cast Generalized Kishor-Koenig Matrices into State-Space Form
#'
#' Transforms the generalized Kishor-Koenig (KK) matrices into the state-space
#' representation required for Kalman filtering and smoothing.
#'
#' @param FF Matrix. The state transition matrix defining how the state
#' evolves over time.
#' @param GG Matrix. The control matrix, representing the influence of the
#' state on observations.
#' @param V Matrix. The state noise covariance matrix.
#' @param W Matrix. The observation noise covariance matrix.
#' @param epsilon Numeric. A small positive number to ensure numerical
#' stability in covariance matrices (default: \code{1e-6}).
#'
#' @return A list containing the state-space matrices:
#'   \describe{
#'     \item{\code{Z}}{The observation matrix.}
#'     \item{\code{Tmat}}{The state transition matrix for the augmented
#'     state-space model.}
#'     \item{\code{H}}{The observation noise covariance matrix.}
#'     \item{\code{Q}}{The state noise covariance matrix.}
#'     \item{\code{R}}{The control matrix.}
#'   }
#' @srrstats {G3.0} Numerical stability with epsilon parameter
#' @details The state space model is represented by the following equations:
#'
#' **State Equation:**
#' \deqn{\alpha_{t+1} = T \alpha_t + R \eta_t, \quad \eta_t \sim N(0, Q)}
#'
#' **Observation Equation:**
#' \deqn{y_t = Z \alpha_t + \epsilon_t, \quad \epsilon_t \sim N(0, H)}
#'
#' where:
#' - \eqn{\alpha_t} is the state vector.
#' - \eqn{y_t} is the observed data.
#' - \eqn{T} is the state transition matrix.
#' - \eqn{R} is the control matrix.
#' - \eqn{Q} is the covariance matrix of the state disturbances \eqn{\eta_t}.
#' - \eqn{Z} is the observation matrix.
#' - \eqn{H} is the covariance matrix of the observation disturbances
#' \eqn{\epsilon_t}.
#'
#' @examples
#' # Define example matrices
#' II <- diag(3)
#' FF <- matrix(c(0.9, 0.1, 0, 0.1, 0.8, 0.1, 0, 0.1, 0.9), nrow = 3)
#' GG <- matrix(c(0.8, 0.2, 0, 0.2, 0.7, 0.1, 0, 0.1, 0.8), nrow = 3)
#' V <- diag(3) * 0.01
#' W <- diag(3) * 0.02
#'
#' # Generate state-space matrices
#' ss_matrices <- kk_to_ss(FF, GG, V, W)
#' str(ss_matrices)
#'
#' @keywords internal
#' @noRd
kk_to_ss <- function(FF, GG, V, W, epsilon = 1e-6) {
  # Get efficient release, e
  e <- nrow(FF) - 1

  II <- diag(e + 1)

  # Observation matrix Z
  Z <- cbind(II, II)

  # State transition matrix
  Tmat <- rbind(
    cbind(FF, array(0, c(e + 1, e + 1))),
    cbind(array(0, c(e + 1, e + 1)), (II - GG) %*% FF)
  )

  # Covariance matrices
  R <- diag(2 * (e + 1))
  H <- array(0, c(e + 1, e + 1))
  Q <- array(0, c(2 * (e + 1), 2 * (e + 1)))
  v_t_2 <- V[1:(e + 1), 1:(e + 1)]
  Q[1:(e + 1), 1:(e + 1)] <- v_t_2
  Q[(1:(e + 1)), ((e + 2):(2 * (e + 1)))] <- -v_t_2 %*% t(II - GG)
  Q[((e + 2):(2 * (e + 1))), 1:(e + 1)] <- -(II - GG) %*% v_t_2
  Q[((e + 2):(2 * (e + 1))), ((e + 2):(2 * (e + 1)))] <- W[
    1:(e + 1),
    1:(e + 1)
  ] +
    (II - GG) %*% v_t_2 %*% t(II - GG)

  for (jj in c(1:e, e + 2)) {
    Q[jj, jj] <- epsilon
  }

  return(list(Z = Z, Tmat = Tmat, H = H, Q = Q, R = R))
}


#' Summary Method for KK Model
#'
#' @description Computes and displays a summary of the results from a
#' Kishor-Koenig (KK) model fit, including convergence status,
#' information criteria, and parameter estimates.
#'
#' @param object An object of class \code{kk_model}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The function returns the input \code{object} invisibly.
#' @method summary kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1
#' h <- 2
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig", method = "MLE")
#' summary(result)
#'
#' @family revision nowcasting
#' @export
summary.kk_model <- function(object, ...) {
  cat("\n=== Kishor-Koenig Model ===\n\n")

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


#' Print Method for KK Model
#'
#' @description Default print method for \code{kk_model} objects.
#' Wraps the \code{summary} method for a consistent output.
#'
#' @param x An object of class \code{kk_model}.
#' @param ... Additional arguments passed to \code{summary.kk_model}.
#'
#' @return The function returns the input \code{x} invisibly.
#' @method print kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1
#' h <- 2
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig", method = "MLE")
#' result
#'
#' @family revision nowcasting
#' @export
print.kk_model <- function(x, ...) {
  summary.kk_model(x, ...)
}


#' Plot Kishor-Koenig Model Results
#'
#' Plot filtered or smoothed estimates for a selected state from a fitted
#' `kk_model`.
#'
#' @param x An object of class `kk_model`.
#' @param state Character scalar giving the state to visualize. If `NULL`, the
#'   first available state is used.
#' @param type Character scalar indicating whether `"filtered"` or `"smoothed"`
#'   estimates should be plotted.
#' @param ... Additional arguments passed to `plot.revision_model()`.
#' @details This method requires `x$states` to be available. If the model was
#'   fitted with `solver_options$return_states = FALSE`, plotting is not
#'   possible.
#'
#' @return A `ggplot2` object visualizing the specified state estimates.
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1 # Number of efficient release
#' h <- 2 # Forecast horizon
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")
#'
#' plot(result)
#'
#' @family revision nowcasting
#' @export
plot.kk_model <- function(x, state = NULL, type = "filtered", ...) {
  if (is.null(state)) {
    state <- x$states[x$states$filter == type, ]$state[1]
  }
  # Forward to the base method with KK defaults
  plot.revision_model(x, state = state, type = type, ...)
}
