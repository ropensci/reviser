#' Jacobs-Van Norden model for data revisions
#'
#' Estimate the Jacobs and Van Norden (2011) state-space model for real-time
#' data revisions, allowing for news and noise components and optional
#' spillovers.
#'
#' @param df A matrix, data frame, or single-ID vintages object. Wide data
#'   should store one vintage per column. Long-format vintages data are also
#'   accepted and are converted internally. If `df` is a matrix, or a data
#'   frame without a `time` column, a synthetic `time` index is created.
#' @param e A single integer giving the number of vintages used in estimation.
#'   The function uses the first `e` vintage columns after `time`, so `e` must
#'   be greater than `0` and no larger than the number of available vintage
#'   columns.
#' @param ar_order A single integer giving the autoregressive order of the
#'   latent true-value process. Must be greater than `0`.
#' @param h A single integer giving the forecast horizon. Must be greater than
#'   or equal to `0`.
#' @param include_news Logical; whether to include a news component.
#' @param include_noise Logical; whether to include a noise component.
#' @param include_spillovers Logical; whether to include spillover effects.
#' @param spillover_news Logical; whether spillovers apply to the news
#'   component.
#' @param spillover_noise Logical; whether spillovers apply to the noise
#'   component.
#' @param method Estimation method. Currently only `"MLE"` is supported.
#' @param alpha Significance level used for confidence intervals. Must lie in
#'   `(0, 1)`.
#' @param standardize Logical; whether to standardize the vintage matrix before
#'   estimation using `jvn_standardize()`. If `TRUE`, scaling metadata are
#'   returned in the `scale` element of the output.
#' @param solver_options A named list of solver options. Valid names are
#'   `trace`, `method`, `maxiter`, `transform_se`, `startvals`, `se_method`,
#'   `n_starts`, `seed`, `return_states`, `qml_eps`, `qml_score_method`,
#'   `qml_scale`, `sigma_lower`, `sigma_upper`, `kfas_init`, and `ic_n`.
#'   Supported entries are:
#'   \itemize{
#'     \item `trace`: integer controlling console output.
#'     \item `method`: optimization method; one of `"L-BFGS-B"`, `"BFGS"`,
#'       `"Nelder-Mead"`, `"nlminb"`, or `"two-step"`.
#'     \item `maxiter`: maximum number of optimizer iterations.
#'     \item `transform_se`: logical; whether standard deviation parameters are
#'       optimized on the log scale.
#'     \item `startvals`: optional numeric vector of starting values. The vector
#'       must have length equal to the number of estimated parameters and must
#'       follow the internal parameter order used by `jvn_param_table()`: AR
#'       coefficients `rho_*`, `sigma_e`, optional `sigma_nu_*`, optional
#'       `sigma_zeta_*`, and optional spillover parameters `T_nu_*` and
#'       `T_zeta_*`.
#'     \item `se_method`: standard-error method; one of `"hessian"`, `"qml"`,
#'       or `"none"`.
#'     \item `n_starts`: number of random starting points for multi-start
#'       optimization.
#'     \item `seed`: optional random seed used for multi-start perturbations.
#'     \item `return_states`: logical; whether filtered and smoothed state
#'       estimates should be returned.
#'     \item `qml_eps`: finite-difference step size used in QML covariance
#'       estimation.
#'     \item `qml_score_method`: score approximation method; `"forward"` or
#'       `"central"`.
#'     \item `qml_scale`: scaling convention for the QML covariance; `"sum"`,
#'       `"mean"`, or `"hc"`.
#'     \item `sigma_lower`: lower bound for standard deviations on the natural
#'       scale.
#'     \item `sigma_upper`: upper bound for standard deviations on the natural
#'       scale.
#'     \item `kfas_init`: initialization used in the KFAS filtering/smoothing
#'       stage; `"stationary"` or `"diffuse"`. This affects state extraction,
#'       not the custom likelihood engine.
#'     \item `ic_n`: sample-size convention used for BIC; `"T"` for the paper
#'       convention or `"Tp"` for `T * n_vint`.
#'   }
#'   For backward compatibility, the legacy aliases `score_method` and
#'   `score_eps` are also accepted and mapped to `qml_score_method` and
#'   `qml_eps`.
#'
#' @return An object of class `"jvn_model"` with components:
#' \describe{
#'   \item{states}{A tibble of filtered and smoothed state estimates. If
#'   `solver_options$return_states = FALSE`, this is `NULL`.}
#'   \item{jvn_model_mat}{A list containing the state-space matrices `Z`,
#'   `Tmat`, `R`, `H`, and `Q`.}
#'   \item{params}{A data frame of parameter estimates and standard errors.}
#'   \item{fit}{The raw optimizer output returned by the selected numerical
#'   optimizer.}
#'   \item{loglik}{The maximized log-likelihood.}
#'   \item{aic}{Akaike information criterion.}
#'   \item{bic}{Bayesian information criterion.}
#'   \item{convergence}{Optimizer convergence code.}
#'   \item{data}{The input data after preprocessing.}
#'   \item{scale}{Scaling metadata returned by `jvn_standardize()` when
#'   `standardize = TRUE`; otherwise `NULL`.}
#'   \item{se_method}{The standard-error method used.}
#'   \item{cov}{Estimated covariance matrix of the parameter estimates, if
#'   available; otherwise `NULL`.}
#' }
#'
#' @references Jacobs, Jan P. A. M. and Van Norden, Simon (2011). Modeling data
#'   revisions: Measurement error and dynamics of "true" values. \emph{Journal
#'   of Econometrics}.
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
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 4,
#'   ar_order = 2,
#'   h = 0,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' summary(result)
#' }
#'
#' @family revision nowcasting
#' @export
jvn_nowcast <- function(
  df,
  e,
  ar_order = 1,
  h = 0,
  include_news = TRUE,
  include_noise = TRUE,
  include_spillovers = FALSE,
  spillover_news = TRUE,
  spillover_noise = TRUE,
  method = "MLE",
  alpha = 0.05,
  standardize = FALSE,
  solver_options = list()
) {
  # Helper utilities ----
  abort_scalar <- function(x, nm, cond, msg_extra = NULL) {
    if (length(x) != 1L || !is.finite(x) || !cond(x)) {
      msg <- paste0("`", nm, "` ", rlang::`%||%`(msg_extra, "is invalid."))
      rlang::abort(msg, call = rlang::caller_env())
    }
  }

  abort_flag <- function(x, nm) {
    if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
      rlang::abort(
        paste0("`", nm, "` must be a single TRUE/FALSE."),
        call = rlang::caller_env()
      )
    }
  }

  if (!include_news && !include_noise) {
    rlang::abort(
      "At least one of `include_news` or `include_noise` must be TRUE.",
      call = rlang::caller_env()
    )
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  infer_forecast_times <- function(time_in, h) {
    if (h <= 0) {
      return(NULL)
    }

    if (inherits(time_in, "Date")) {
      # Best-effort: infer monthly step by approx days/30
      diffs <- diff(time_in)
      step_m <- unique(round(as.numeric(diffs) / 30))
      step_m <- step_m[is.finite(step_m) & step_m > 0]
      if (length(step_m) != 1L) {
        rlang::abort(
          paste(
            "Time index appears irregular; cannot infer monthly step",
            "for forecasts."
          ),
          call = rlang::caller_env()
        )
      }
      seq.Date(
        from = time_in[length(time_in)],
        by = paste0(step_m, " months"),
        length.out = h + 1
      )[-1]
    } else if (inherits(time_in, "POSIXt")) {
      step <- stats::median(diff(as.numeric(time_in)), na.rm = TRUE)
      if (!is.finite(step) || step <= 0) step <- 86400
      as.POSIXct(
        as.numeric(time_in[length(time_in)]) + step * seq_len(h),
        origin = "1970-01-01",
        tz = attr(time_in, "tzone")
      )
    } else if (is.numeric(time_in) || is.integer(time_in)) {
      step <- stats::median(diff(as.numeric(time_in)), na.rm = TRUE)
      if (!is.finite(step) || step == 0) step <- 1
      as.numeric(time_in[length(time_in)]) + step * seq_len(h)
    } else {
      # Last-resort regular index
      seq_len(h) + length(time_in)
    }
  }

  compute_hessian_cov <- function(theta_hat, model_struct, y, transform_se) {
    H <- numDeriv::hessian(
      func = jvn_negloglik,
      x = theta_hat,
      model_struct = model_struct,
      data = y,
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

  transform_params_and_cov <- function(
    params_raw,
    cov_raw,
    model_struct,
    transform_se
  ) {
    params <- params_raw
    cov_used <- cov_raw

    idx_sd <- unlist(c(
      model_struct$param_info$sigma_e_idx,
      model_struct$param_info$sigma_nu_idx,
      model_struct$param_info$sigma_zeta_idx
    ))
    idx_sd <- idx_sd[is.finite(idx_sd)]

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
    } else if (isTRUE(transform_se) && length(idx_sd) > 0) {
      # Diagonal-only fallback if we had raw SEs but no cov.
      # The caller can overwrite this later.
      se <- rep(NA_real_, length(params_raw))
    }

    list(params = params, cov = cov_used, se = se, idx_sd = idx_sd)
  }

  # Validate main arguments ----

  method <- match.arg(method, c("MLE"))

  abort_scalar(ar_order, "ar_order", \(x) x > 0, "must be a single number > 0.")
  ar_order <- as.integer(round(ar_order))

  abort_scalar(e, "e", \(x) x > 0, "must be a single number > 0.")
  e <- as.integer(round(e))

  abort_scalar(h, "h", \(x) x >= 0, "must be a single number >= 0.")
  h <- as.integer(round(h))

  abort_scalar(alpha, "alpha", \(x) x > 0 && x < 1, "must be in (0, 1).")

  abort_flag(include_news, "include_news")
  abort_flag(include_noise, "include_noise")
  abort_flag(include_spillovers, "include_spillovers")
  abort_flag(spillover_news, "spillover_news")
  abort_flag(spillover_noise, "spillover_noise")
  abort_flag(standardize, "standardize")

  if (!is.list(solver_options)) {
    rlang::abort("`solver_options` must be a list.", call = rlang::caller_env())
  }

  # Solver options (defaults + validation + legacy aliases) ----
  default_solver_options <- list(
    trace = 0,
    method = "L-BFGS-B",
    maxiter = 1000,
    transform_se = TRUE,
    startvals = NULL,
    se_method = "hessian",
    n_starts = 1,
    seed = NULL,
    return_states = TRUE,
    qml_eps = 1e-4,
    qml_score_method = "forward",
    qml_scale = "sum",
    sigma_lower = 1e-3,
    sigma_upper = 100,
    kfas_init = "stationary",
    ic_n = "T"
  )

  # Legacy renames (keep but do not document as primary)
  if (!is.null(solver_options$score_method) &&
      is.null(solver_options$qml_score_method)) {
    solver_options$qml_score_method <- solver_options$score_method
    solver_options$score_method <- NULL
  }
  if (!is.null(solver_options$score_eps) && is.null(solver_options$qml_eps)) {
    solver_options$qml_eps <- solver_options$score_eps
    solver_options$score_eps <- NULL
  }

  bad <- setdiff(names(solver_options), names(default_solver_options))
  if (length(bad) > 0) {
    rlang::abort(
      paste0(
        "Invalid `solver_options`: ", paste(bad, collapse = ", "), ".\n",
        "Valid options are: ",
        paste(names(default_solver_options), collapse = ", "),
        "."
      ),
      call = rlang::caller_env()
    )
  }

  for (nm in names(solver_options)) {
    default_solver_options[[nm]] <- solver_options[[nm]]
  }

  default_solver_options$se_method <- match.arg(
    default_solver_options$se_method,
    c("hessian", "qml", "none")
  )

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

  default_solver_options$kfas_init <- match.arg(
    default_solver_options$kfas_init,
    c("stationary", "diffuse")
  )

  default_solver_options$ic_n <- match.arg(
    default_solver_options$ic_n,
    c("T", "Tp")
  )

  abort_scalar(
    default_solver_options$trace, "solver_options$trace", \(x) x >= 0,
    "must be a single number >= 0."
  )
  default_solver_options$trace <- as.integer(
    round(default_solver_options$trace)
  )

  abort_scalar(
    default_solver_options$maxiter, "solver_options$maxiter", \(x) x >= 1,
    "must be a single integer >= 1."
  )
  default_solver_options$maxiter <- as.integer(
    round(default_solver_options$maxiter)
  )

  abort_scalar(
    default_solver_options$n_starts, "solver_options$n_starts", \(x) x >= 1,
    "must be a single integer >= 1."
  )
  default_solver_options$n_starts <- as.integer(
    round(default_solver_options$n_starts)
  )

  abort_scalar(
    default_solver_options$qml_eps, "solver_options$qml_eps", \(x) x > 0,
    "must be > 0."
  )

  abort_scalar(
    default_solver_options$sigma_lower,
    "solver_options$sigma_lower",
    \(x) x > 0,
    "must be > 0."
  )
  abort_scalar(
    default_solver_options$sigma_upper, "solver_options$sigma_upper",
    \(x) x > default_solver_options$sigma_lower,
    "must be > solver_options$sigma_lower."
  )

  # Accept matrix / data.frame without time -> add synthetic time ----
  if (is.matrix(df)) {
    df <- data.frame(
      time = seq_len(nrow(df)),
      as.data.frame(df),
      check.names = FALSE
    )
  } else if (is.data.frame(df) && !"time" %in% names(df)) {
    df <- data.frame(time = seq_len(nrow(df)), df, check.names = FALSE)
  }

  # Preprocess vintages data (long/wide, single ID) ----
  check <- vintages_check(df)

  if (is.list(check)) {
    if (length(check) > 1) {
      rlang::abort(
        paste0(
          "`df` must contain a single ID, but ",
          length(check),
          " IDs were provided: ",
          paste(names(check), collapse = ", "),
          "."
        ),
        call = rlang::caller_env()
      )
    }
    df <- df[[1]]
    check <- check[[1]]
  }

  if (check == "long") {
    if ("id" %in% colnames(df) && length(unique(df$id)) > 1) {
      rlang::abort(
        paste0(
          "`df` contains ", length(unique(df$id)), " different IDs. ",
          "Filter to a single ID first."
        ),
        call = rlang::caller_env()
      )
    }
    df <- suppressWarnings(vintages_wide(df, names_from = "release"))
    if (is.list(df) && !is.data.frame(df)) df <- df[[1]]
  }

  if (!("time" %in% names(df))) {
    rlang::abort("After preprocessing, `df` must contain a `time` column.",
      call = rlang::caller_env()
    )
  }

  # Keep time first (predictable subsetting)
  df <- df[, c("time", setdiff(names(df), "time")), drop = FALSE]

  n_vint_total <- ncol(df) - 1L
  if (n_vint_total < e) {
    rlang::abort(
      paste0(
        "Not enough vintage columns in `df`.\n",
        "Requested `e = ", e, "` but found ", n_vint_total, " vintage columns."
      ),
      call = rlang::caller_env()
    )
  }

  # Ensure time is strictly increasing if comparable
  time_in <- df$time
  if ((inherits(time_in, "Date") ||
        inherits(time_in, "POSIXt") ||
        is.numeric(time_in) ||
        is.integer(time_in)) &&
      any(diff(as.numeric(time_in)) <= 0, na.rm = TRUE)) {
    rlang::abort("`time` must be strictly increasing.",
      call = rlang::caller_env()
    )
  }

  # Internal working subset: time + first e vintages
  df_intern <- df[, 1:(e + 1), drop = FALSE]

  y_mat <- as.matrix(dplyr::select(df_intern, -"time"))
  rownames(y_mat) <- as.character(df_intern$time)

  # Optional standardization
  scale_info <- NULL
  if (isTRUE(standardize)) {
    scale_info <- jvn_standardize(y_mat)
    y_mat <- scale_info$Y
  }

  # Build model structure + initialize parameters ----
  model_struct <- jvn_matrices(
    n_obs = nrow(df_intern),
    n_vint = ncol(df_intern) - 1,
    ar_order = ar_order,
    include_news = include_news,
    include_noise = include_noise,
    include_spillovers = include_spillovers,
    spillover_news = spillover_news,
    spillover_noise = spillover_noise
  )

  state_names <- model_struct$state_names

  if (!is.null(default_solver_options$startvals)) {
    if (length(default_solver_options$startvals) != model_struct$n_params) {
      rlang::abort(
        paste0("`startvals` must have length ", model_struct$n_params, "."),
        call = rlang::caller_env()
      )
    }
    init_params <- default_solver_options$startvals
  } else {
    init_params <- jvn_init_params(
      model_struct,
      y_mat,
      transform_se = default_solver_options$transform_se
    )
  }

  if (default_solver_options$trace > 0) {
    cat("Estimating JVN model with", model_struct$n_params, "parameters...\n")
    cat("AR order:", ar_order, "\n")
    cat(
      "News:", include_news, " Noise:",
      include_noise, " Spillovers:",
      include_spillovers, "\n\n"
    )
  }

  # Multi-start optimization ----
  seed <- default_solver_options$seed
  reviser_with_seed(seed, {
    n_starts <- max(1L, as.integer(default_solver_options$n_starts))
    all_results <- vector("list", n_starts)
    all_values <- rep(NA_real_, n_starts)

    sd_idx <- unlist(c(
      model_struct$param_info$sigma_e_idx,
      model_struct$param_info$sigma_nu_idx,
      model_struct$param_info$sigma_zeta_idx
    ))
    sd_idx <- sd_idx[is.finite(sd_idx)]

    for (start_idx in seq_len(n_starts)) {
      if (start_idx == 1) {
        current_init <- init_params
      } else {
        current_init <- init_params + stats::rnorm(length(init_params), 0, 0.5)

        if (isTRUE(default_solver_options$transform_se) && length(sd_idx) > 0) {
          current_init[sd_idx] <- pmax(
            pmin(current_init[sd_idx], log(default_solver_options$sigma_upper)),
            log(default_solver_options$sigma_lower)
          )
        }

        ar_indices <- model_struct$param_info$ar_coef_idx
        current_init[ar_indices] <- pmax(
          pmin(current_init[ar_indices], 0.9),
          -0.9
        )

        if (!is.null(model_struct$param_info$spill_nu_idx)) {
          idx <- model_struct$param_info$spill_nu_idx
          current_init[idx] <- pmax(pmin(current_init[idx], 0.9), -0.9)
        }
        if (!is.null(model_struct$param_info$spill_zeta_idx)) {
          idx <- model_struct$param_info$spill_zeta_idx
          current_init[idx] <- pmax(pmin(current_init[idx], 0.9), -0.9)
        }
      }

      opt_method <- default_solver_options$method

      if (opt_method == "two-step") {
        opt_nm <- stats::optim(
          par = current_init,
          fn = jvn_negloglik,
          model_struct = model_struct,
          data = y_mat,
          transform_se = default_solver_options$transform_se,
          method = "Nelder-Mead",
          control = list(
            trace = max(0, default_solver_options$trace),
            maxit = 500
          )
        )

        current_result <- stats::optim(
          par = opt_nm$par,
          fn = jvn_negloglik,
          model_struct = model_struct,
          data = y_mat,
          transform_se = default_solver_options$transform_se,
          method = "BFGS",
          control = list(
            trace = max(0, default_solver_options$trace),
            maxit = default_solver_options$maxiter
          ),
          hessian = FALSE
        )
      } else if (opt_method %in% c("L-BFGS-B", "nlminb")) {
        n_params <- model_struct$n_params
        lower_bounds <- rep(-Inf, n_params)
        upper_bounds <- rep(Inf, n_params)

        if (isTRUE(default_solver_options$transform_se) && length(sd_idx) > 0) {
          lower_bounds[sd_idx] <- log(default_solver_options$sigma_lower)
          upper_bounds[sd_idx] <- log(default_solver_options$sigma_upper)
        }

        ar_indices <- model_struct$param_info$ar_coef_idx
        lower_bounds[ar_indices] <- -0.9
        upper_bounds[ar_indices] <- 0.9

        if (!is.null(model_struct$param_info$spill_nu_idx)) {
          idx <- model_struct$param_info$spill_nu_idx
          lower_bounds[idx] <- -0.9
          upper_bounds[idx] <- 0.9
        }
        if (!is.null(model_struct$param_info$spill_zeta_idx)) {
          idx <- model_struct$param_info$spill_zeta_idx
          lower_bounds[idx] <- -0.9
          upper_bounds[idx] <- 0.9
        }

        if (opt_method == "L-BFGS-B") {
          current_result <- stats::optim(
            par = current_init,
            fn = jvn_negloglik,
            model_struct = model_struct,
            data = y_mat,
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
        } else {
          opt_nl <- stats::nlminb(
            start = current_init,
            objective = jvn_negloglik,
            model_struct = model_struct,
            data = y_mat,
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
            par = opt_nl$par,
            value = opt_nl$objective,
            convergence = opt_nl$convergence,
            message = opt_nl$message
          )
        }
      } else {
        current_result <- stats::optim(
          par = current_init,
          fn = jvn_negloglik,
          model_struct = model_struct,
          data = y_mat,
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
    }

    best_idx <- which.min(all_values)
    opt_result <- all_results[[best_idx]]

    params_raw <- opt_result$par
    loglik <- -opt_result$value
  })

  # Covariance / SEs on optimizer scale (then transform once) ----

  se_method_used <- default_solver_options$se_method
  se_warning <- NULL
  cov_raw <- NULL
  se_raw <- rep(NA_real_, length(params_raw))

  if (se_method_used == "hessian") {
    h_res <- tryCatch(
      compute_hessian_cov(
        params_raw,
        model_struct,
        y_mat,
        default_solver_options$transform_se
      ),
      error = function(e) {
        list(
          cov = NULL,
          warning = paste0(
            "Hessian computation failed: ",
            e$message
          )
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
      jvn_qml_covariance(
        theta_hat = params_raw,
        model_struct = model_struct,
        y = y_mat,
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

    if (!is.null(se_warning)) {
      warning(se_warning, call. = FALSE)
    }
  }

  # Transform params + covariance (once)
  tr <- transform_params_and_cov(
    params_raw = params_raw,
    cov_raw = cov_raw,
    model_struct = model_struct,
    transform_se = default_solver_options$transform_se
  )

  params <- tr$params
  cov_used <- tr$cov

  # If cov is missing but we do have raw SEs, apply a diagonal delta fallback
  # for SDs.
  se <- tr$se
  if (is.null(cov_used) && any(is.finite(se_raw))) {
    se <- se_raw
    if (isTRUE(default_solver_options$transform_se) && length(tr$idx_sd) > 0) {
      se[tr$idx_sd] <- exp(params_raw[tr$idx_sd]) * se_raw[tr$idx_sd]
    }
  }

  if (!is.null(se_warning) && default_solver_options$trace > 0) {
    warning(se_warning, call. = FALSE)
  }

  # Update matrices at estimates + parameter table + IC ----
  model_struct <- jvn_update_matrices(model_struct, params)
  param_table <- jvn_param_table(params, se, model_struct$param_info)

  k <- length(params)

  n_ic <- if (default_solver_options$ic_n == "Tp") {
    nrow(y_mat) * ncol(y_mat)
  } else {
    nrow(y_mat)
  }

  aic <- -2 * loglik + 2 * k
  bic <- -2 * loglik + log(n_ic) * k

  # Forecast extension (optional) ----
  time_in <- df_intern$time
  forecast_times <- infer_forecast_times(time_in, h)
  output_times <- time_in
  y_kfas <- y_mat

  if (!is.null(forecast_times)) {
    output_times <- c(time_in, forecast_times)
    y_kfas <- rbind(y_mat, matrix(NA_real_, nrow = h, ncol = ncol(y_mat)))
  }

  # Early return if states not requested
  if (!isTRUE(default_solver_options$return_states)) {
    out <- list(
      states = NULL,
      jvn_model_mat = list(
        Z = model_struct$Z,
        Tmat = model_struct$Tmat,
        R = model_struct$R,
        H = model_struct$H,
        Q = model_struct$Q
      ),
      params = param_table,
      fit = opt_result,
      loglik = loglik,
      aic = aic,
      bic = bic,
      convergence = opt_result$convergence,
      data = df,
      scale = scale_info,
      se_method = se_method_used,
      cov = cov_used
    )
    class(out) <- c("jvn_model", class(out))
    return(out)
  }

  # KFAS: stationary initialization (matches likelihood engine) ----
  P0 <- jvn_stationary_P0(model_struct$Tmat, model_struct$R, model_struct$Q)

  if (default_solver_options$kfas_init == "stationary") {
    a1 <- rep(0, model_struct$m)
    P1inf <- matrix(0, model_struct$m, model_struct$m)
    P1 <- P0
  } else {
    # diffuse init (Marc-style)
    a1 <- c(0.2, rep(0, model_struct$m - 1))
    P1inf <- diag(c(1, rep(0, model_struct$m - 1)), model_struct$m)
    P1 <- diag(c(0, rep(1, model_struct$m - 1)), model_struct$m)
  }

  stopifnot(
    ncol(y_kfas) == nrow(model_struct$Z),
    ncol(y_kfas) == nrow(model_struct$H),
    ncol(y_kfas) == ncol(model_struct$H)
  )

  y_df <- as.data.frame(y_kfas)
  colnames(y_df) <- paste0("y", seq_len(ncol(y_df)))

  resp <- paste(colnames(y_df), collapse = ", ")

  Z_kfas <- array(model_struct$Z, dim = c(ncol(y_kfas), model_struct$m, 1))
  T_kfas <- array(model_struct$Tmat, dim = c(model_struct$m, model_struct$m, 1))
  R_kfas <- array(
    model_struct$R,
    dim = c(model_struct$m, ncol(model_struct$R), 1)
  )
  Q_kfas <- array(
    model_struct$Q,
    dim = c(ncol(model_struct$R), ncol(model_struct$R), 1)
  )
  H_kfas <- array(model_struct$H, dim = c(ncol(y_kfas), ncol(y_kfas), 1))

  a1_kfas <- a1
  P1inf_kfas <- P1inf
  P1_kfas <- P1

  kfas_formula <- stats::as.formula(
    paste0(
      "cbind(", resp, ") ~ -1 + SSMcustom(",
      "Z = Z_kfas, T = T_kfas, R = R_kfas, Q = Q_kfas, ",
      "a1 = a1_kfas, P1inf = P1inf_kfas, P1 = P1_kfas)"
    )
  )
  environment(kfas_formula) <- environment()

  kfas_model <- KFAS::SSModel(
    formula = kfas_formula,
    data = y_df,
    H = H_kfas
  )

  kalman <- KFAS::KFS(kfas_model)

  zcrit <- stats::qnorm(1 - alpha / 2)
  n_states <- length(state_names)

  state_results <- vector("list", n_states)
  for (i in seq_len(n_states)) {
    filtered_est <- kalman$att[, i]
    filtered_se <- sqrt(pmax(kalman$Ptt[i, i, ], 0))

    smoothed_est <- kalman$alphahat[, i]
    smoothed_se <- sqrt(pmax(kalman$V[i, i, ], 0))

    filtered_df <- dplyr::tibble(
      time = output_times,
      state = state_names[i],
      estimate = filtered_est,
      lower = filtered_est - zcrit * filtered_se,
      upper = filtered_est + zcrit * filtered_se,
      filter = "filtered",
      sample = dplyr::if_else(
        !is.null(forecast_times) & (output_times %in% forecast_times),
        "out_of_sample",
        "in_sample"
      )
    )

    smoothed_df <- dplyr::tibble(
      time = output_times,
      state = state_names[i],
      estimate = smoothed_est,
      lower = smoothed_est - zcrit * smoothed_se,
      upper = smoothed_est + zcrit * smoothed_se,
      filter = "smoothed",
      sample = dplyr::if_else(
        !is.null(forecast_times) & (output_times %in% forecast_times),
        "out_of_sample",
        "in_sample"
      )
    )

    state_results[[i]] <- dplyr::bind_rows(filtered_df, smoothed_df)
  }

  states_long <- dplyr::bind_rows(state_results) |>
    dplyr::as_tibble() |>
    dplyr::arrange(.data$filter, .data$state, .data$time)

  out <- list(
    states = states_long,
    jvn_model_mat = list(
      Z = model_struct$Z,
      Tmat = model_struct$Tmat,
      R = model_struct$R,
      H = model_struct$H,
      Q = model_struct$Q
    ),
    params = param_table,
    fit = opt_result,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = opt_result$convergence,
    data = df,
    scale = scale_info,
    se_method = se_method_used,
    cov = cov_used
  )

  class(out) <- c("jvn_model", class(out))
  out
}

#' Build JVN state-space matrices and parameter indices
#'
#' Construct time-invariant state-space matrices for the Jacobs and Van Norden
#' model, together with parameter indexing metadata used by the optimizer and
#' post-estimation utilities.
#'
#' The construction follows the sign convention used in
#' `jvn_update_matrices()`: the true state loads `-sigma_nu` on news shocks,
#' news states load `+sigma_nu` in an upper-triangular pattern, and noise
#' states load `sigma_zeta` on their own shocks.
#'
#' @srrstats {G1.4a} Internal function documented with @noRd tag
#' @srrstats {G2.0} Input assertions on parameter dimensions
#' @srrstats {G2.1} Type checking for parameters
#' @srrstats {G3.0} Numerical stability in matrix construction
#'
#' @keywords internal
#' @noRd
jvn_matrices <- function(
  n_obs,
  n_vint,
  ar_order,
  include_news,
  include_noise,
  include_spillovers,
  spillover_news,
  spillover_noise
) {
  # Dimensions
  news_dim <- if (isTRUE(include_news)) n_vint else 0L
  noise_dim <- if (isTRUE(include_noise)) n_vint else 0L
  m <- ar_order + news_dim + noise_dim

  # Number of shocks: e-shock + (news shocks) + (noise shocks)
  n_eta <- 1L + news_dim + noise_dim

  # ===== Z (measurement) =====
  Z <- matrix(0, n_vint, m)
  Z[, 1] <- 1

  if (isTRUE(include_news)) {
    news_start <- ar_order + 1
    news_end <- ar_order + n_vint
    Z[, news_start:news_end] <- diag(n_vint)
  }

  if (isTRUE(include_noise)) {
    noise_start <- ar_order + news_dim + 1
    noise_end <- noise_start + n_vint - 1
    Z[, noise_start:noise_end] <- diag(n_vint)
  }

  # ===== T (transition) =====
  Tmat <- matrix(0, m, m)

  # AR companion form for lags
  if (ar_order > 1) {
    for (i in 2:ar_order) {
      Tmat[i, i - 1] <- 1
    }
  }
  # (AR coeffs and spillovers are filled by jvn_update_matrices())

  # ===== R (shock loading) =====
  R <- matrix(0, m, n_eta)

  # AR shock enters true state
  R[1, 1] <- 1 # placeholder for sigma_e

  # News shocks: affect true state and (upper-triangular) news states
  if (isTRUE(include_news)) {
    # True state gets all news shocks (sign is set in update_matrices)
    R[1, 2:(n_vint + 1)] <- 1 # placeholders

    news_start <- ar_order + 1
    for (j in 1:n_vint) {
      for (i in j:n_vint) {
        # placeholders; update_matrices overwrites with sigma_nu[i]
        R[news_start + j - 1, 1 + i] <- 1
      }
    }
  }

  # Noise shocks: independent shocks to each noise state
  if (isTRUE(include_noise)) {
    noise_start <- ar_order + news_dim + 1
    shock_start <- 1 + news_dim + 1
    for (i in 1:n_vint) {
      R[noise_start + i - 1, shock_start + i - 1] <- 1 # placeholder
    }
  }

  # ===== H and Q =====
  H <- matrix(0, n_vint, n_vint)
  Q <- diag(n_eta)

  # ===== Parameter indexing =====
  idx <- 1L
  ar_coef_idx <- idx:(idx + ar_order - 1L)
  idx <- idx + ar_order
  sigma_e_idx <- idx
  idx <- idx + 1L

  sigma_nu_idx <- NULL
  if (isTRUE(include_news)) {
    sigma_nu_idx <- idx:(idx + n_vint - 1L)
    idx <- idx + n_vint
  }

  sigma_zeta_idx <- NULL
  if (isTRUE(include_noise)) {
    sigma_zeta_idx <- idx:(idx + n_vint - 1L)
    idx <- idx + n_vint
  }

  spill_nu_idx <- NULL
  if (isTRUE(include_spillovers) &&
      isTRUE(include_news) &&
      isTRUE(spillover_news)) {
    spill_nu_idx <- idx:(idx + n_vint - 1L)
    idx <- idx + n_vint
  }

  spill_zeta_idx <- NULL
  if (isTRUE(include_spillovers) &&
      isTRUE(include_noise) &&
      isTRUE(spillover_noise)) {
    spill_zeta_idx <- idx:(idx + n_vint - 1L)
    idx <- idx + n_vint
  }

  n_params <- idx - 1L

  # ===== State names =====
  true_names <- paste0("true_lag_", 0:(ar_order - 1L))
  news_names <- if (isTRUE(include_news)) {
    paste0("news_vint", 1:n_vint)
  } else {
    NULL
  }
  noise_names <- if (isTRUE(include_noise)) {
    paste0("noise_vint", 1:n_vint)
  } else {
    NULL
  }
  state_names <- c(true_names, news_names, noise_names)

  list(
    Z = Z,
    Tmat = Tmat,
    R = R,
    H = H,
    Q = Q,
    param_info = list(
      ar_order = ar_order,
      n_vint = n_vint,
      include_news = include_news,
      include_noise = include_noise,
      include_spillovers = include_spillovers,
      spillover_news = spillover_news,
      spillover_noise = spillover_noise,
      ar_coef_idx = ar_coef_idx,
      sigma_e_idx = sigma_e_idx,
      sigma_nu_idx = sigma_nu_idx,
      sigma_zeta_idx = sigma_zeta_idx,
      spill_nu_idx = spill_nu_idx,
      spill_zeta_idx = spill_zeta_idx
    ),
    n_params = n_params,
    state_names = state_names,
    m = m
  )
}

#' Update JVN state-space matrices with parameter values
#'
#' Insert parameter values into the transition matrix `Tmat` and the shock
#' loading matrix `R` for the Jacobs and Van Norden model.
#'
#' @srrstats {G1.4a} Internal function documented with @noRd tag
#' @srrstats {G2.1} Parameter type validation
#' @srrstats {G3.0} Numerical operations with appropriate precision
#'
#' @keywords internal
#' @noRd
jvn_update_matrices <- function(model_struct, params) {
  info <- model_struct$param_info
  Tmat <- model_struct$Tmat
  R <- model_struct$R

  # --- AR block ---
  Tmat[1, 1:info$ar_order] <- params[info$ar_coef_idx]
  R[1, 1] <- params[info$sigma_e_idx]

  # --- NEWS block (GAUSS convention) ---
  if (isTRUE(info$include_news)) {
    sigma_nu <- params[info$sigma_nu_idx]

    # True value gets -sigma_nu on news shocks
    R[1, 2:(info$n_vint + 1)] <- -sigma_nu

    # News states: upper triangular with +sigma_nu[i]
    news_start <- info$ar_order + 1L

    # (optional but safe) clear news-state shock loadings first
    R[news_start:(news_start + info$n_vint - 1L), 2:(info$n_vint + 1L)] <- 0

    for (j in 1:info$n_vint) {
      for (i in j:info$n_vint) {
        R[news_start + j - 1L, 1L + i] <- sigma_nu[i]
      }
    }
  }

  # --- NOISE block ---
  if (isTRUE(info$include_noise)) {
    sigma_zeta <- params[info$sigma_zeta_idx]

    news_dim <- if (isTRUE(info$include_news)) info$n_vint else 0L
    noise_start <- info$ar_order + news_dim + 1L
    shock_start <- 1L + news_dim + 1L

    # (optional but safe) clear then fill
    R[
      noise_start:(noise_start + info$n_vint - 1L),
      shock_start:(shock_start + info$n_vint - 1L)
    ] <- 0

    for (i in 1:info$n_vint) {
      R[noise_start + i - 1L, shock_start + i - 1L] <- sigma_zeta[i]
    }
  }

  # --- Spillovers: NEWS (diagonal persistence in news-state block) ---
  if (isTRUE(info$include_news) &&
      isTRUE(info$include_spillovers) &&
      isTRUE(info$spillover_news)) {
    spill_nu <- params[info$spill_nu_idx]
    news_start <- info$ar_order + 1L
    news_end <- info$ar_order + info$n_vint

    blk <- Tmat[news_start:news_end, news_start:news_end, drop = FALSE]
    diag(blk) <- spill_nu
    Tmat[news_start:news_end, news_start:news_end] <- blk
  }

  # --- Spillovers: NOISE (diagonal persistence in noise-state block) ---
  if (isTRUE(info$include_noise) &&
      isTRUE(info$include_spillovers) &&
      isTRUE(info$spillover_noise)) {
    spill_zeta <- params[info$spill_zeta_idx]
    news_dim <- if (isTRUE(info$include_news)) info$n_vint else 0L
    noise_start <- info$ar_order + news_dim + 1L
    noise_end <- noise_start + info$n_vint - 1L

    blk <- Tmat[noise_start:noise_end, noise_start:noise_end, drop = FALSE]
    diag(blk) <- spill_zeta
    Tmat[noise_start:noise_end, noise_start:noise_end] <- blk
  }

  model_struct$Tmat <- Tmat
  model_struct$R <- R
  model_struct
}

#' Compute the stationary initial covariance matrix
#'
#' Compute the stationary state covariance matrix `P0` from the discrete
#' Lyapunov equation
#' \deqn{vec(P) = (I - T \otimes T)^{-1} vec(R Q R').}
#'
#' This is the initialization used by the custom likelihood engine and matches
#' the `InitP0()` logic in the original GAUSS code.
#'
#' @keywords internal
#' @noRd
jvn_stationary_P0 <- function(Tmat, R, Q, ridge = 1e-10, cond_max = 1e12) {
  m <- nrow(Tmat)
  stopifnot(ncol(Tmat) == m)

  S <- R %*% Q %*% t(R)
  A <- diag(m * m) - kronecker(Tmat, Tmat)
  b <- as.vector(S)

  # Try direct solve first
  vecP <- tryCatch(solve(A, b), error = function(e) NULL)

  # If solve failed or matrix is nasty, add ridge (scaled) and retry
  if (is.null(vecP)) {
    # scale ridge to typical magnitude of A (avoids “too small to matter” ridge)
    scaleA <- mean(abs(diag(A)))
    if (!is.finite(scaleA) || scaleA <= 0) scaleA <- 1
    vecP <- solve(A + (ridge * scaleA) * diag(m * m), b)
  } else {
    # optional conditioning check (helps avoid garbage P0 when nearly singular)
    cond <- tryCatch(kappa(A, exact = FALSE), error = function(e) Inf)
    if (!is.finite(cond) || cond > cond_max) {
      scaleA <- mean(abs(diag(A)))
      if (!is.finite(scaleA) || scaleA <= 0) scaleA <- 1
      vecP <- solve(A + (ridge * scaleA) * diag(m * m), b)
    }
  }

  P0 <- matrix(vecP, nrow = m, ncol = m)
  P0 <- (P0 + t(P0)) / 2 # symmetrize
  P0
}


#' Kalman filter log-likelihood contributions for a time-invariant system
#'
#' Compute per-period and total Gaussian log-likelihood contributions for a
#' time-invariant state-space model using the custom Kalman filter employed by
#' the JVN likelihood engine.
#'
#' @keywords internal
#' @noRd
jvn_kalman_loglik <- function(
  y,
  Z,
  Tmat,
  R,
  Q,
  H = NULL,
  a1 = NULL,
  P1 = NULL
) {
  y <- as.matrix(y)
  n <- nrow(y)
  p <- ncol(y)
  m <- nrow(Tmat)

  stopifnot(
    nrow(Z) == p, ncol(Z) == m,
    ncol(Tmat) == m,
    nrow(R) == m,
    nrow(Q) == ncol(Q),
    ncol(R) == nrow(Q)
  )

  if (is.null(H)) H <- matrix(0, p, p)
  if (is.null(a1)) a1 <- rep(0, m)
  if (is.null(P1)) P1 <- diag(m)

  a <- matrix(a1, ncol = 1)
  P <- P1
  P <- (P + t(P)) / 2

  RQR <- R %*% Q %*% t(R)

  ll <- numeric(n)
  log2pi <- log(2 * pi)

  for (t in seq_len(n)) {
    obs <- !is.na(y[t, ])
    k <- sum(obs)

    # Propagate-only when no observation
    if (k == 0L) {
      a <- Tmat %*% a
      P <- Tmat %*% P %*% t(Tmat) + RQR
      P <- (P + t(P)) / 2
      next
    }

    yt <- matrix(y[t, obs], ncol = 1)
    Zt <- Z[obs, , drop = FALSE]
    Ht <- H[obs, obs, drop = FALSE]

    v <- yt - Zt %*% a
    F_mat <- Zt %*% P %*% t(Zt) + Ht
    F_mat <- (F_mat + t(F_mat)) / 2

    # Cholesky with tiny jitter retry for numerical PSD issues
    cholF <- tryCatch(chol(F_mat), error = function(e) NULL)
    if (is.null(cholF)) {
      base <- 1e-10 * max(1, mean(abs(diag(F_mat))))
      for (j in 0:4) {
        cholF <- tryCatch(chol(F_mat + (10^j) * base * diag(k)),
          error = function(e) NULL
        )
        if (!is.null(cholF)) break
      }
    }

    if (is.null(cholF)) {
      ll[t] <- -1e10
      a <- Tmat %*% a
      P <- Tmat %*% P %*% t(Tmat) + RQR
      P <- (P + t(P)) / 2
      next
    }

    logdetF <- 2 * sum(log(diag(cholF)))

    # Finv_v = F^{-1} v via triangular solves, F = t(U)U
    tmp <- backsolve(cholF, v, transpose = TRUE)
    Finv_v <- backsolve(cholF, tmp)
    quad <- drop(t(v) %*% Finv_v)

    ll[t] <- -0.5 * (logdetF + quad + k * log2pi)

    # Finv_Zt = F^{-1} Zt (k x m)
    tmpZ <- backsolve(cholF, Zt, transpose = TRUE)
    Finv_Zt <- backsolve(cholF, tmpZ)

    # Kalman gain: K = P Z' F^{-1}
    K <- P %*% t(Finv_Zt)

    # Update
    a <- a + K %*% v
    P <- P - K %*% F_mat %*% t(K)
    P <- (P + t(P)) / 2

    # Propagate
    a <- Tmat %*% a
    P <- Tmat %*% P %*% t(Tmat) + RQR
    P <- (P + t(P)) / 2
  }

  list(loglik = sum(ll), ll = ll)
}

#' Per-period negative log-likelihood contributions for the JVN model
#'
#' Return the vector of per-period negative log-likelihood contributions used in
#' the custom likelihood engine. These contributions are also used to construct
#' QML sandwich covariance estimates.
#'
#' @keywords internal
#' @noRd
jvn_negloglik_contrib <- function(
  params,
  model_struct,
  data,
  transform_se = TRUE
) {
  info <- model_struct$param_info
  y <- as.matrix(data)
  Tn <- nrow(y)

  # Work on a copy
  theta <- as.numeric(params)

  # Transform SDs from log-scale if used
  if (isTRUE(transform_se)) {
    idx_sd <- unlist(
      c(info$sigma_e_idx, info$sigma_nu_idx, info$sigma_zeta_idx)
    )
    idx_sd <- idx_sd[is.finite(idx_sd)]
    if (length(idx_sd) > 0) theta[idx_sd] <- exp(theta[idx_sd])
  }

  # GAUSS-like “keep AR sane” soft constraint: sum(rho) in (-1,1)
  rho <- theta[info$ar_coef_idx]
  s <- sum(rho)
  if (!is.finite(s) || abs(s) >= 0.999) {
    return(rep(1e8, Tn))
  }

  # Update matrices
  ms <- jvn_update_matrices(model_struct, theta)

  # Stationary P0 (InitP0) — fail-safe
  P0 <- tryCatch(
    jvn_stationary_P0(ms$Tmat, ms$R, ms$Q),
    error = function(e) NULL
  )
  if (is.null(P0)) {
    return(rep(1e8, Tn))
  }

  # Filter to get per-time loglik contributions, then flip sign to
  # objective contributions.
  kf <- jvn_kalman_loglik(
    y    = y,
    Z    = ms$Z,
    Tmat = ms$Tmat,
    R    = ms$R,
    Q    = ms$Q,
    H    = ms$H,
    a1   = rep(0, ms$m),
    P1   = P0
  )

  out <- -as.numeric(kf$ll)
  out[!is.finite(out)] <- 1e8
  out
}


#' Total negative log-likelihood for the JVN model
#'
#' Return the summed negative log-likelihood corresponding to
#' `jvn_negloglik_contrib()`.
#'
#' @srrstats {G1.4a} Internal function documented with @noRd tag
#' @srrstats {G2.15} Handles missing values appropriately
#' @srrstats {G3.0} Numerical stability in likelihood calculation
#'
#' @keywords internal
#' @noRd
jvn_negloglik <- function(params, model_struct, data, transform_se = TRUE) {
  contrib <- jvn_negloglik_contrib(
    params       = params,
    model_struct = model_struct,
    data         = data,
    transform_se = transform_se
  )

  contrib <- as.numeric(contrib)
  if (length(contrib) == 0L || any(!is.finite(contrib))) {
    return(1e10)
  }

  obj <- sum(contrib)
  if (!is.finite(obj)) 1e10 else obj
}

#' Standardize vintage matrix for JVN estimation
#'
#' Standardizes all vintages using the mean and standard deviation of the
#' final vintage, following the convention used in the empirical illustration
#' of Jacobs and Van Norden (2011).
#'
#' @param Y Numeric matrix of vintages, with one column per vintage.
#'
#' @return A list with:
#' \describe{
#'   \item{Y}{Standardized vintage matrix}
#'   \item{center}{Mean of the final vintage}
#'   \item{scale}{Standard deviation of the final vintage}
#' }
#'
#' @keywords internal
#' @noRd
jvn_standardize <- function(Y) {
  Y <- as.matrix(Y)

  if (!is.numeric(Y)) {
    rlang::abort("`Y` must be a numeric matrix.")
  }

  if (ncol(Y) < 1L) {
    rlang::abort("`Y` must contain at least one vintage column.")
  }

  y_final <- Y[, ncol(Y)]
  center <- mean(y_final, na.rm = TRUE)
  scale <- stats::sd(y_final, na.rm = TRUE)

  if (!is.finite(scale) || scale <= 0) {
    rlang::abort(
      paste(
        "Standard deviation of the final vintage is not positive;",
        "cannot standardize."
      )
    )
  }

  Y_std <- (Y - center) / scale

  list(
    Y = Y_std,
    center = center,
    scale = scale
  )
}

#' Create data-driven starting values for the JVN model
#'
#' Construct starting values for the optimizer using simple data-driven
#' heuristics. If the data are too short or the heuristics fail, stable fallback
#' values are returned instead.
#'
#' @keywords internal
#' @noRd
jvn_init_params <- function(model_struct, data, transform_se = TRUE) {
  info <- model_struct$param_info
  n_vint <- info$n_vint
  ar_order <- info$ar_order

  # ---------- fallback (never recurse) ----------
  fallback <- function() {
    p <- numeric(model_struct$n_params)

    # AR coefs: small & stable-ish
    p[info$ar_coef_idx] <- pmin(0.2, 0.8 / max(1, ar_order))

    # sigmas on natural scale
    p[info$sigma_e_idx] <- 0.5
    if (!is.null(info$sigma_nu_idx)) {
      p[info$sigma_nu_idx] <- rep(0.3, length(info$sigma_nu_idx))
    }
    if (!is.null(info$sigma_zeta_idx)) {
      p[info$sigma_zeta_idx] <- rep(0.2, length(info$sigma_zeta_idx))
    }

    # spillovers: allow negative, keep modest
    if (!is.null(info$spill_nu_idx)) {
      p[info$spill_nu_idx] <- rep(0.2, length(info$spill_nu_idx))
    }
    if (!is.null(info$spill_zeta_idx)) {
      p[info$spill_zeta_idx] <- rep(0.2, length(info$spill_zeta_idx))
    }

    # log-transform sigmas if optimizer uses logs
    if (isTRUE(transform_se)) {
      idx_sd <- unlist(c(
        info$sigma_e_idx,
        info$sigma_nu_idx,
        info$sigma_zeta_idx
      ))
      idx_sd <- idx_sd[is.finite(idx_sd)]
      p[idx_sd] <- log(pmax(p[idx_sd], 1e-6))
    }

    p
  }

  data <- as.matrix(data)
  data_clean <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data_clean) < ar_order + 10) {
    warning(
      "Insufficient data for smart initialization. Using fallback values."
    )
    return(fallback())
  }

  params <- numeric(model_struct$n_params)

  # ---------- 1) AR(p) from final vintage ----------
  final_vintage <- data_clean[, n_vint]

  ar_fit <- tryCatch(
    stats::ar(final_vintage, aic = FALSE, order.max = ar_order, method = "ols"),
    error = function(e) NULL
  )

  if (is.null(ar_fit) ||
      length(ar_fit$ar) < ar_order ||
      any(!is.finite(ar_fit$ar))) {
    # OLS fallback for AR coefs
    y <- final_vintage[(ar_order + 1):length(final_vintage)]
    X <- matrix(0, nrow = length(y), ncol = ar_order)
    for (i in 1:ar_order) {
      X[, i] <- final_vintage[(ar_order + 1 - i):(length(final_vintage) - i)]
    }
    lm_fit <- stats::lm(y ~ X - 1)
    rho <- stats::coef(lm_fit)
    sig2 <- summary(lm_fit)$sigma^2
  } else {
    rho <- ar_fit$ar
    sig2 <- ar_fit$var.pred
  }

  rho <- as.numeric(rho)
  if (length(rho) < ar_order) rho <- c(rho, rep(0, ar_order - length(rho)))
  rho[!is.finite(rho)] <- 0

  params[info$ar_coef_idx] <- pmax(pmin(rho, 0.9), -0.9)
  params[info$sigma_e_idx] <- sqrt(pmax(as.numeric(sig2), 1e-6))

  # GAUSS-like constraint spirit used in your likelihood:
  # keep sum(rho) in (-1,1).
  s <- sum(params[info$ar_coef_idx])
  if (is.finite(s) && abs(s) >= 0.95) {
    params[info$ar_coef_idx] <- params[info$ar_coef_idx] * (0.95 / abs(s))
  }

  # ---------- 2) News sigmas from revision variances ----------
  if (isTRUE(info$include_news) && !is.null(info$sigma_nu_idx)) {
    revision_vars <- numeric(n_vint)
    for (v in 1:(n_vint - 1)) {
      revisions <- data_clean[, v + 1] - data_clean[, v]
      revision_vars[v] <- stats::var(revisions, na.rm = TRUE)
    }
    revision_vars[n_vint] <- revision_vars[n_vint - 1] * 0.5

    sigma_nu_init <- sqrt(pmax(revision_vars, 1e-6))
    # (no forced monotone decrease; keep it flexible)
    params[info$sigma_nu_idx] <- pmax(sigma_nu_init, 0.05)
  }

  # ---------- 3) Noise sigmas from cross-vintage deviations ----------
  if (isTRUE(info$include_noise) && !is.null(info$sigma_zeta_idx)) {
    vintage_mean <- rowMeans(data_clean, na.rm = TRUE)
    sigma_zeta_init <- numeric(n_vint)
    for (v in 1:n_vint) {
      deviations <- data_clean[, v] - vintage_mean
      sigma_zeta_init[v] <- stats::sd(deviations, na.rm = TRUE)
    }
    params[info$sigma_zeta_idx] <- pmax(sigma_zeta_init, 0.05)
  }

  # ---------- 4) Spillovers: allow negative ----------
  if (isTRUE(info$include_spillovers)) {
    if (isTRUE(info$include_news) &&
        isTRUE(info$spillover_news) &&
        !is.null(info$spill_nu_idx)) {
      spillover_nu <- numeric(n_vint)
      for (v in 1:(n_vint - 1)) {
        revisions <- data_clean[, v + 1] - data_clean[, v]
        spillover_nu[v] <- tryCatch(
          stats::ar(
            revisions,
            aic = FALSE,
            order.max = 1,
            method = "ols"
          )$ar[1],
          error = function(e) 0.2
        )
      }
      spillover_nu[n_vint] <- mean(spillover_nu[1:(n_vint - 1)], na.rm = TRUE)
      spillover_nu[!is.finite(spillover_nu)] <- 0.2

      params[info$spill_nu_idx] <- pmax(pmin(spillover_nu, 0.9), -0.9)
    }

    if (isTRUE(info$include_noise) &&
        isTRUE(info$spillover_noise) &&
        !is.null(info$spill_zeta_idx)) {
      spillover_zeta <- numeric(n_vint)
      for (v in 1:n_vint) {
        residuals <- diff(data_clean[, v])
        spillover_zeta[v] <- tryCatch(
          stats::ar(
            residuals,
            aic = FALSE,
            order.max = 1,
            method = "ols"
          )$ar[1],
          error = function(e) 0.2
        )
      }
      spillover_zeta[!is.finite(spillover_zeta)] <- 0.2

      params[info$spill_zeta_idx] <- pmax(pmin(spillover_zeta, 0.9), -0.9)
    }
  }

  # ---------- 5) log-transform sigmas if needed ----------
  if (isTRUE(transform_se)) {
    idx_sd <- unlist(
      c(info$sigma_e_idx, info$sigma_nu_idx, info$sigma_zeta_idx)
    )
    idx_sd <- idx_sd[is.finite(idx_sd)]
    params[idx_sd] <- log(pmax(params[idx_sd], 1e-12))
  }

  # ---------- 6) final validation ----------
  if (any(!is.finite(params))) {
    warning("Non-finite starting values detected. Using fallback values.")
    return(fallback())
  }

  params
}


#' QML sandwich covariance for the JVN estimator
#'
#' Compute a quasi-maximum-likelihood sandwich covariance matrix of the form
#' `solve(H) %*% X %*% solve(H)`, where `H` is the Hessian of the negative
#' log-likelihood and `X` is the outer product of per-period score vectors.
#'
#' If the per-period contributions do not sum numerically to the full objective,
#' the score contributions are rescaled accordingly before the sandwich
#' matrix is
#' formed.
#'
#' @keywords internal
#' @noRd
jvn_qml_covariance <- function(
  theta_hat,
  model_struct,
  y,
  transform_se = TRUE,
  score_eps = 1e-4,
  score_method = c("central", "forward"),
  qml_scale = c("sum", "mean", "hc"),
  hess_args = list(method.args = list(
    eps = 1e-4,
    d = 0.01,
    r = 6
  )),
  cond_max = 1e10,
  ridge_factor = 1e-6
) {
  score_method <- match.arg(score_method)
  qml_scale <- match.arg(qml_scale)

  # ---- Hessian of full objective (neg loglik) ----
  H <- do.call(
    numDeriv::hessian,
    c(list(
      func = jvn_negloglik,
      x = theta_hat,
      model_struct = model_struct,
      data = y,
      transform_se = transform_se
    ), hess_args)
  )

  # conditioning check (useful for diagnostics)
  cond_num <- tryCatch(kappa(H, exact = FALSE), error = function(e) Inf)

  # ---- Per-time contributions at theta_hat ----
  base_contrib <- jvn_negloglik_contrib(
    theta_hat, model_struct, y, transform_se
  )
  nT <- length(base_contrib)
  k <- length(theta_hat)

  # ---- Critical sanity check: do contributions sum to total? ----
  total_nll <- jvn_negloglik(theta_hat, model_struct, y, transform_se)
  sum_contrib <- sum(base_contrib)

  # If contribs are averages (or scaled), fix by constant rescaling:
  # scores should be based on contribs that sum to total_nll.
  scale_factor <- 1
  if (is.finite(total_nll) && is.finite(sum_contrib) && sum_contrib != 0) {
    rel_gap <- abs(total_nll - sum_contrib) / max(1, abs(total_nll))
    if (rel_gap > 1e-6) {
      scale_factor <- total_nll / sum_contrib
    }
  }

  # ---- Compute score matrix (nT x k) via finite differences on contribs ----
  scores <- matrix(0, nrow = nT, ncol = k)

  for (j in seq_len(k)) {
    step <- rep(0, k)
    step[j] <- score_eps

    c_plus <- jvn_negloglik_contrib(
      theta_hat + step, model_struct, y, transform_se
    )

    if (length(c_plus) != nT) {
      stop(
        paste(
          "jvn_negloglik_contrib returned different length under",
          "perturbation; cannot form scores."
        )
      )
    }

    if (score_method == "central") {
      c_minus <- jvn_negloglik_contrib(
        theta_hat - step, model_struct, y, transform_se
      )
      if (length(c_minus) != nT) {
        stop(
          paste(
            "jvn_negloglik_contrib returned different length under",
            "perturbation; cannot form scores."
          )
        )
      }
      scores[, j] <- (c_plus - c_minus) / (2 * score_eps)
    } else {
      # forward difference (closer to GAUSS GradMethod=1)
      scores[, j] <- (c_plus - base_contrib) / score_eps
    }
  }

  # enforce contrib scaling if needed
  if (!isTRUE(all.equal(scale_factor, 1))) {
    scores <- scores * scale_factor
  }

  # ---- Xproduct ----
  Xproduct <- crossprod(scores) # sum_t g_t g_t'

  # scaling conventions (optional)
  Tobs <- nrow(scores)
  p <- ncol(scores)

  if (qml_scale == "mean") {
    Xproduct <- Xproduct / Tobs
  } else if (qml_scale == "hc") {
    if (Tobs <= p) stop("hc scaling requires Tobs > number of parameters.")
    Xproduct <- (Tobs / (Tobs - p)) * (Xproduct / Tobs)
  }

  # ---- Robust inversion of Hessian ----
  invH <- tryCatch(
    solve(H),
    error = function(e) {
      ridge <- ridge_factor * mean(abs(diag(H)))
      solve(H + ridge * diag(nrow(H)))
    }
  )

  # Symmetrise for numerical stability
  invH <- (invH + t(invH)) / 2
  cov_qml <- invH %*% Xproduct %*% invH
  cov_qml <- (cov_qml + t(cov_qml)) / 2

  list(
    cov = cov_qml,
    hessian = H,
    invH = invH,
    Xproduct = Xproduct,
    scores = scores,
    diagnostics = list(
      total_nll = total_nll,
      sum_contrib = sum_contrib,
      scale_factor = scale_factor,
      cond_num = cond_num
    )
  )
}


#' Create a parameter table for a fitted JVN model
#'
#' Combine parameter estimates, standard errors, and internally generated
#' parameter names into a data frame.
#'
#' @keywords internal
#' @noRd
jvn_param_table <- function(params, se, param_info) {
  stopifnot(is.numeric(params), is.numeric(se))
  if (length(params) != length(se)) {
    rlang::abort("`params` and `se` must have the same length.")
  }

  param_names <- character(0)

  # AR coefficients
  param_names <- c(param_names, paste0("rho_", seq_len(param_info$ar_order)))

  # AR shock
  param_names <- c(param_names, "sigma_e")

  # News shocks
  if (isTRUE(param_info$include_news)) {
    param_names <- c(
      param_names,
      paste0("sigma_nu_", seq_len(param_info$n_vint))
    )
  }

  # Noise shocks
  if (isTRUE(param_info$include_noise)) {
    param_names <- c(
      param_names,
      paste0("sigma_zeta_", seq_len(param_info$n_vint))
    )
  }

  # News spillovers
  if (isTRUE(param_info$include_spillovers) &&
      isTRUE(param_info$include_news) &&
      isTRUE(param_info$spillover_news)) {
    param_names <- c(param_names, paste0("T_nu_", seq_len(param_info$n_vint)))
  }

  # Noise spillovers
  if (isTRUE(param_info$include_spillovers) &&
      isTRUE(param_info$include_noise) &&
      isTRUE(param_info$spillover_noise)) {
    param_names <- c(param_names, paste0("T_zeta_", seq_len(param_info$n_vint)))
  }

  if (length(param_names) != length(params)) {
    rlang::abort(
      paste0(
        "Parameter-name construction mismatch: got ", length(param_names),
        " names but ", length(params), " parameters. ",
        "Check `jvn_matrices()` / `param_info` indexing."
      )
    )
  }

  data.frame(
    Parameter = param_names,
    Estimate = params,
    Std.Error = se,
    row.names = NULL
  )
}

#' Summary method for JVN model objects
#'
#' Print a compact summary of a fitted `jvn_model`, including convergence
#' status, information criteria, and parameter estimates.
#'
#' @param object An object of class `jvn_model`.
#' @param ... Unused; included for method compatibility.
#'
#' @return The input object, invisibly.
#'
#' @method summary jvn_model
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
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 4,
#'   ar_order = 2,
#'   h = 0,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' summary(result)
#' }
#'
#' @family revision nowcasting
#' @export
summary.jvn_model <- function(object, ...) {
  cat("\n=== Jacobs-Van Norden Model ===\n\n")
  cat(
    "Convergence:",
    ifelse(
      object$convergence == 0,
      "Success",
      "Failed"
    ),
    "\n"
  )
  cat("Log-likelihood:", round(object$loglik, 2), "\n")
  cat("AIC:", round(object$aic, 2), "\n")
  cat("BIC:", round(object$bic, 2), "\n\n")

  cat("Parameter Estimates:\n")
  df_print <- object$params
  df_print$Estimate <- sprintf("%.3f", df_print$Estimate)
  df_print$Std.Error <- sprintf("%.3f", df_print$Std.Error)
  print(df_print, row.names = FALSE, quote = FALSE)

  cat("\n")
  invisible(object)
}


#' Print method for JVN model objects
#'
#' Default print method for `jvn_model` objects. This method dispatches to
#' `summary.jvn_model()` for a consistent console display.
#'
#' @param x An object of class `jvn_model`.
#' @param ... Additional arguments passed to `summary.jvn_model()`.
#'
#' @return The input object, invisibly.
#'
#' @method print jvn_model
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
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 4,
#'   ar_order = 2,
#'   h = 0,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' result
#' }
#'
#' @family revision nowcasting
#' @export
print.jvn_model <- function(x, ...) {
  summary.jvn_model(x, ...)
}

#' Plot JVN model results
#'
#' Plot filtered or smoothed estimates for a selected state from a fitted
#' `jvn_model`.
#'
#' @param x An object of class `jvn_model`.
#' @param state Character scalar giving the state to visualize. Defaults to
#'   `"true_lag_0"`.
#' @param type Character scalar indicating whether `"filtered"` or `"smoothed"`
#'   estimates should be plotted.
#' @param ... Additional arguments passed to `plot.revision_model()`.
#' @details This method requires `x$states` to be available. If the model was
#'   fitted with `solver_options$return_states = FALSE`, plotting is not
#'   possible.
#'
#' @srrstats {TS5.0} Implements default plot methods for class system
#' @srrstats {TS5.1} Time axis labeling (delegates to base method)
#' @srrstats {TS5.2} Time on horizontal axis (delegates to base method)
#' @srrstats {TS5.6} Distributional limits shown (confidence intervals)
#' @srrstats {TS5.7} Includes model and forecast values in plot
#' @srrstats {TS5.8} Visual distinction between model and forecast values
#'
#' @return A `ggplot2` object.
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
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 4,
#'   ar_order = 2,
#'   h = 0,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' plot(result)
#' }
#'
#' @family revision nowcasting
#' @export
plot.jvn_model <- function(x, state = "true_lag_0", type = "filtered", ...) {
  # Forward to the base method with JVN defaults
  plot.revision_model(x, state = state, type = type, ...)
}
