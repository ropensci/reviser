#' Calculate Revisions in Vintage Data
#'
#' Computes revisions in vintage data based on specified reference points:
#' a fixed reference date, the nth release, or a specified interval.
#' This function allows users to analyze differences between data vintages
#' across time.
#'
#' @param df A data frame containing vintage data. The data frame must
#' include at least the following columns:
#'   - `pub_date`: The publication date of each vintage.
#'   - `time`: The reference period (e.g., quarter or month).
#'   - `value`: The observed value for the given vintage and reference period.
#' @param interval A positive integer specifying the lag (in periods) between
#' vintages to compute revisions.
#'   Defaults to `1` if no other parameter is specified.
#' @param nth_release A positive integer or `"latest"`, specifying the release
#' to use as a reference for revisions.
#'   If `"latest"`, the most recent vintage is used.
#' @param ref_date A date specifying the fixed reference publication date to
#' compare all vintages against.
#'
#' @return A data frame (tibble) of class `tbl_revision`, with the
#' following columns:
#'   - `pub_date`: The publication date of the vintage.
#'   - `time`: The reference period (e.g., quarter or month).
#'   - `value`: The calculated revision, i.e., the difference between the
#'   observed value and the reference value.
#'
#' @details
#' The function supports three mutually exclusive methods for
#' calculating revisions:
#'
#' - **Reference date (`ref_date`)**: Computes revisions relative to a
#' fixed publication date.
#' - **Interval (`interval`)**: Computes revisions relative to vintages
#' published `interval` periods earlier.
#' - **Nth release (`nth_release`)**: Computes revisions relative to the
#' nth vintage release for each reference period.
#'
#' If no method is explicitly specified, `interval = 1` is used by default.
#'
#' Input validation ensures that only one of `ref_date`, `nth_release`, or
#' `interval` is specified.
#'
#' @srrstats {G1.3} Statistical terminology for revisions defined
#' @srrstats {G2.0} Implements assertions on lengths of inputs through
#' validation checks
#' @srrstats {G2.1} Implements assertions on types of inputs (e.g., date,
#' numeric)
#' @srrstats {G2.1a} Documents expectations on data types for all inputs
#' @srrstats {G2.3} Uses validation logic to ensure inputs are exclusive
#' @srrstats {G2.3a} Uses validation logic to ensure parameters have expected
#' values
#' @srrstats {G2.3b} use `tolower()`
#' @srrstats {G2.4} convert between different data types
#' @srrstats {G2.4a} Converts input as needed (ref_date converted to Date)
#' @srrstats {G2.7} Accepts both wide and long format tabular data through
#' vintages_check
#' @srrstats {G2.8} Provides conversion routines (vintages_long) to ensure
#' consistent input format
#' @srrstats {G5.8} Handles edge conditions with appropriate error messages
#' @srrstats {TS1.0} Uses explicit class systems for time series data
#' (tbl_revision)
#' @srrstats {TS1.1} Explicitly documents required input data structure
#' @srrstats {TS1.2} Implements validation routines to confirm inputs are
#' acceptable
#' @srrstats {TS1.3} Uses a single pre-processing routine (vintages_check,
#' vintages_long)
#' @srrstats {TS1.4} Maintains time-based components of input data
#' @srrstats {TS1.5} Ensures ordering of time through
#' dplyr::arrange(pub_date, time)
#' @srrstats {TS4.0a} Returns values in same class as input data
#' @srrstats {TS4.2} Type and class of return values  documented
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Calculate revisions using an interval of 1
#' revisions_interval <- get_revisions(df, interval = 1)
#'
#' # Calculate revisions using a fixed reference date
#' revisions_date <- get_revisions(df, ref_date = as.Date("2023-02-01"))
#'
#' # Calculate revisions relative to the nth release (2nd release)
#' revisions_nth <- get_revisions(df, nth_release = 1)
#'
#' @family revision utilities
#' @export
get_revisions <- function(
  df,
  interval = NULL,
  nth_release = NULL,
  ref_date = NULL
) {
  # Validate inputs
  specified_count <- sum(vapply(
    list(interval, nth_release, ref_date),
    function(x) !is.null(x),
    FUN.VALUE = logical(1) # Specify the expected return type
  ))

  # Default interval is 1
  if (specified_count == 0L) {
    interval <- 1L
  }

  if (specified_count > 1L) {
    rlang::abort("Specify only one of 'ref_date', 'nth_release' or 'interval'.")
  }

  if (!is.null(interval)) {
    # check interval is length 1 and integer
    if (length(interval) != 1) {
      rlang::abort("'interval' must be of length 1.")
    }

    if (is.integer(interval)) {
      # Already valid
    } else if (is.numeric(interval)) {
      if (interval %% 1 != 0) {
        rlang::abort("'interval' must be a whole number.")
      }
      interval <- as.integer(interval)
    } else {
      rlang::abort("'interval' must be an integer or numeric of length 1.")
    }
  } else if (!is.null(ref_date)) {
    ref_date <- tryCatch(as.Date(ref_date), error = function(e) e)
    if (!c("Date") %in% class(ref_date)) {
      rlang::abort("The input 'ref_date' must be a date object.")
    }
  } else if (!is.null(nth_release)) {
    if (is.numeric(nth_release) && nth_release < 0) {
      rlang::abort(
        "The input 'nth_release' must be a non-negative integer or 'latest'."
      )
    } else if (is.character(nth_release) && tolower(nth_release) != "latest") {
      rlang::abort(
        "The input 'nth_release' must be a non-negative integer or 'latest'."
      )
    }
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  # Check if id column present
  if ("id" %in% colnames(df) && length(unique(df$id)) > 1) {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$id, .data$pub_date, .data$time)

    if (!is.null(ref_date)) {
      # Calculate revisions against the specified reference date
      revisions <- df |>
        dplyr::inner_join(
          df |>
            dplyr::filter(.data$pub_date == ref_date) |>
            dplyr::select("id", "time", "value_ref" = "value"),
          by = c("id", "time")
        ) |>
        dplyr::group_by(.data$id, .data$time) |>
        dplyr::mutate(value = .data$value_ref - .data$value) |>
        dplyr::ungroup() |>
        dplyr::select("id", "pub_date", "time", "value")
    } else if (!is.null(interval)) {
      # Get revisions relative to estimates published 'interval' periods ago
      revisions <- df |>
        dplyr::group_by(.data$id, .data$time) |>
        dplyr::mutate(
          value_ref = dplyr::lag(
            .data$value,
            n = interval,
            order_by = .data$pub_date
          )
        ) |>
        dplyr::mutate(value = .data$value_ref - .data$value) |>
        dplyr::ungroup() |>
        dplyr::select("id", "pub_date", "time", "value")
    } else if (!is.null(nth_release)) {
      # Calculate revisions relative to the nth release
      revisions <- df |>
        dplyr::inner_join(
          get_nth_release(df, n = nth_release) |>
            dplyr::select("id", "time", "value_ref" = "value"),
          by = c("id", "time")
        ) |>
        dplyr::group_by(.data$id, .data$time) |>
        dplyr::mutate(value = .data$value_ref - .data$value) |>
        dplyr::ungroup() |>
        dplyr::select("id", "pub_date", "time", "value")
    }
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$pub_date, .data$time)

    if (!is.null(ref_date)) {
      # Calculate revisions against the specified reference date
      revisions <- df |>
        dplyr::inner_join(
          df |>
            dplyr::filter(.data$pub_date == ref_date) |>
            dplyr::select("time", "value_ref" = "value"),
          by = "time"
        ) |>
        dplyr::mutate(value = .data$value_ref - .data$value) |>
        dplyr::select("pub_date", "time", "value")
    } else if (!is.null(interval)) {
      # Get revisions relative to estimates published 'interval' periods ago
      revisions <- df |>
        dplyr::group_by(.data$time) |>
        dplyr::mutate(
          value_ref = dplyr::lag(
            .data$value,
            n = interval,
            order_by = .data$pub_date
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(value = .data$value_ref - .data$value) |>
        dplyr::select("pub_date", "time", "value")
    } else if (!is.null(nth_release)) {
      # Calculate revisions relative to the nth release
      revisions <- df |>
        dplyr::inner_join(
          get_nth_release(df, n = nth_release) |>
            dplyr::select("time", "value_ref" = "value"),
          by = "time"
        ) |>
        dplyr::mutate(value = .data$value_ref - .data$value) |>
        dplyr::select("pub_date", "time", "value")
    }
  }

  revisions <- vintages_assign_class(revisions)
  return(revisions)
}

#' Identify the First Efficient Release in Vintage Data
#'
#' Identifies the first release in a sequence of vintages that is "efficient"
#' relative to the final release. A release is deemed efficient if it
#' satisfies specific conditions of unbiasedness and efficiency, tested
#' using a Mincer-Zarnowitz type linear regression and hypothesis testing.
#'
#' @param df A data frame of class `tbl_release` containing the vintage data.
#'   It must include the columns:
#'   - `time`: The reference period (e.g., quarter or month).
#'   - `value`: The observed value for the given release.
#'   - `release`: The release number or identifier.
#' @param final_release A data frame containing the final release data.
#'   This must include the columns:
#'   - `time`: The reference period.
#'   - `value`: The observed final value for the given period.
#' @param significance A numeric value specifying the significance level for
#' the hypothesis test (default is `0.05`).
#' @param test_all A logical value indicating whether to test all releases,
#' even after finding the first efficient release (default is `FALSE`).
#' @param robust A logical value indicating whether to use robust HAC standard
#' errors (default is `TRUE`).
#'
#' @return A list of class `lst_efficient` with the following elements:
#'   - `e`: The index of the first efficient release. (0 indexed)
#'   - `data`: A long-format data frame containing the vintage data with the
#'   final release appended.
#'   - `models`: A list of linear regression models fitted for each release.
#'   - `tests`: A list of hypothesis test results for each release.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates inputs and ensures both `df` and `final_release` are in the
#' correct format.
#' 2. Iteratively tests each release for efficiency using a linear regression
#' model of the form:
#'    \deqn{final = \beta_0 + \beta_1 \cdot release_i + \epsilon}
#'    The null hypothesis for efficiency is:
#'    - \eqn{\beta_0 = 0} (no bias)
#'    - \eqn{\beta_1 = 1} (efficiency)
#'    Uses heteroskedasticity and autocorrelation consistent (HAC) standard
#'    errors for robust hypothesis testing.
#' 3. Stops testing when the first efficient release is found (unless
#' `test_all = TRUE`).
#'
#' If no efficient release is found, a warning is issued.
#'
#' @srrstats {G1.0} reference from published academic literature (Aruoba, 2008).
#' @srrstats {G1.3} Statistical terminology is clearly defined, including the
#' concept of "efficiency" in the context of vintage data releases.
#' @srrstats {G2.0} Function implements assertions on lengths of inputs by
#' ensuring that both df and final_release contain necessary columns.
#' @srrstats {G2.1} Function implements assertions on types of inputs by
#' checking for the 'tbl_release' class.
#' @srrstats {G2.7} Function accepts different standard tabular forms and
#' converts them to a consistent format.
#' @srrstats {G2.8} Function provides appropriate conversion routines as part
#' of initial pre-processing to ensure uniform data type.
#' @srrstats {G2.13} Function implements checks for missing data through
#' stats::na.omit() before passing data to analytical algorithms.
#' @srrstats {G3.1} Robust and standard covariance matrix allowed
#' @srrstats {G3.1a} Shown in example
#' @srrstats {G5.2a} Function implements explicit error behavior for unexpected
#' inputs with unique error messages for different validation failures.
#' @srrstats {G5.8d} Function handles edge cases where no efficient release is
#' found by issuing an appropriate warning.
#' @srrstats {TS1.1} Function explicitly documents the types and classes of
#' input data as 'tbl_release' objects.
#' @srrstats {TS1.2} Function validates that inputs are of acceptable classes
#' through the 'vintages_check' function.
#' @srrstats {TS1.3} Function uses a single pre-processing routine to validate
#' and transform input data to a uniform type.
#' @srrstats {TS1.4} Function maintains time-based components of input data
#' through the transformation process.
#' @srrstats {TS1.5} Function ensures strict ordering of time through the
#' dplyr::arrange(time) operation.
#' @srrstats {TS2.1} Function removes missings before passing data
#' @srrstats {TS2.1a} Function removes missings before passing data
#' @srrstats {TS2.1b} Function removes missings before passing data
#' @srrstats {TS4.0} Return values are in a unique, class-defined format
#' ('lst_efficient').
#' @srrstats {TS4.2} The type and class of all return values are documented
#' @examples
#' # Example data
#' df <- get_nth_release(
#'   tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'   n = 0:3
#' )
#'
#' final_release <- get_nth_release(
#'   tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'   n = 10
#' )
#'
#' # Identify the first efficient release
#' result <- get_first_efficient_release(
#'   df,
#'   final_release,
#'   significance = 0.05,
#'   robust = FALSE
#' )
#'
#' result <- get_first_efficient_release(df, final_release, significance = 0.05)
#'
#' # Access the index of the first efficient release
#' result$e
#'
#' @references Aruoba, S. Boragan, "Revisions Are Not Well Behaved", Journal of
#' Money, Credit and Banking, 40(2-3), 319-340, 2008.
#'
#' @family revision analysis
#' @export
get_first_efficient_release <- function(
  df,
  final_release,
  significance = 0.05,
  test_all = FALSE,
  robust = TRUE
) {
  # Check robust is logical
  if (!is.logical(robust)) {
    rlang::abort("The 'robust' argument must be a logical value.")
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  check <- vintages_check(final_release)
  if (check == "wide") {
    final_release <- vintages_long(final_release)
  }
  final_release <- vintages_assign_class(final_release)

  if (!"tbl_release" %in% class(df)) {
    rlang::abort("The input 'df' must be a 'tbl_release' object.")
  }

  if ("id" %in% colnames(df) && "id" %in% colnames(final_release)) {
    if (!setequal(unique(df$id), unique(final_release$id))) {
      rlang::abort(
        "The 'id' column in 'df' and 'final_release' must have the same values."
      )
    }
  } else if ("id" %in% colnames(df) || "id" %in% colnames(final_release)) {
    rlang::abort(
      "Both or none of 'df' and 'final_release' must contain an 'id' column."
    )
  }

  if (!is.logical(test_all)) {
    rlang::abort("The 'test_all' argument must be a logical value.")
  }

  df_output <- NULL

  has_id <- "id" %in% colnames(df)
  if (has_id) {
    if (length(unique(df$id)) > 1) {
      has_ids <- TRUE
    } else {
      has_ids <- FALSE
    }
  } else {
    has_ids <- FALSE
  }

  if (has_ids) {
    for (iidd in unique(df$id)) {
      models <- list()
      tests <- list()
      final_release_id <- final_release |>
        dplyr::filter(.data$id == iidd) |>
        dplyr::select("time", "value") |>
        dplyr::mutate(release = "final")

      df_id <- df |>
        dplyr::filter(.data$id == iidd) |>
        dplyr::select("time", "value", "release")

      # Ensure data is sorted by pub_date and time
      df_id <- df_id |>
        dplyr::bind_rows(final_release_id) |>
        dplyr::arrange(.data$time) |>
        stats::na.omit()

      es <- unique(df_id$release)
      es <- es[es != "final"]

      df_wide <- vintages_wide(df_id, names_from = "release")

      e_found <- FALSE
      for (i in seq_along(es)) {
        formula <- stats::as.formula(paste0("final ~ ", es[i]))

        model <- stats::lm(formula, data = df_wide)

        # Conditionally set vcov argument
        vcov <- if (robust) sandwich::vcovHAC(model) else NULL

        test <- car::linearHypothesis(
          model,
          c("(Intercept) = 0", paste0(es[i], " = 1")),
          vcov = vcov
        )

        p_value <- test[2, "Pr(>F)"]

        models[[i]] <- model
        tests[[i]] <- test

        if (!test_all) {
          if (p_value > significance) {
            efficient_release <- i - 1
            break
          }
        } else if (test_all && !e_found && p_value > significance) {
          efficient_release <- i - 1
          e_found <- TRUE
        }
      }

      if (i == length(es) && p_value < significance) {
        rlang::warn(paste0(
          "No efficient release found for ",
          iidd,
          ". Please provide further releases!"
        ))
        efficient_release <- NA_real_
      }

      data <- vintages_long(df_wide, names_to = "release")
      data <- vintages_assign_class(data)

      df_output[[iidd]] <- list(
        "e" = efficient_release,
        "data" = data,
        "models" = models,
        "tests" = tests
      )
    }
  } else {
    models <- list()
    tests <- list()

    final_release <- dplyr::select(final_release, "time", "value") |>
      dplyr::mutate(release = "final")

    df <- df |>
      dplyr::select("time", "value", "release")

    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::bind_rows(final_release) |>
      dplyr::arrange(.data$time) |>
      stats::na.omit()

    es <- unique(df$release)
    es <- es[es != "final"]

    df_wide <- vintages_wide(df, names_from = "release")

    models <- list()
    tests <- list()
    e_found <- FALSE
    for (i in seq_along(es)) {
      formula <- stats::as.formula(paste0("final ~ ", es[i]))

      model <- stats::lm(formula, data = df_wide)

      # Conditionally set vcov argument
      vcov <- if (robust) sandwich::vcovHAC(model) else NULL

      test <- car::linearHypothesis(
        model,
        c("(Intercept) = 0", paste0(es[i], " = 1")),
        vcov = vcov
      )

      p_value <- test[2, "Pr(>F)"]

      models[[i]] <- model
      tests[[i]] <- test

      if (!test_all) {
        if (p_value > significance) {
          efficient_release <- i - 1
          break
        }
      } else if (test_all && !e_found && p_value > significance) {
        efficient_release <- i - 1
        e_found <- TRUE
      }
    }

    if (i == length(es) && p_value < significance) {
      rlang::warn(
        "No efficient release found. Please provide further releases!"
      )
      efficient_release <- NA_real_
    }

    data <- vintages_long(df_wide, names_to = "release")
    data <- vintages_assign_class(data)

    df_output <- list(
      "e" = efficient_release,
      "data" = data,
      "models" = models,
      "tests" = tests
    )
  }

  class(df_output) <- c("lst_efficient", class(df_output))
  return(df_output)
}

#' Print Method for Efficient Release Results
#'
#' @param x An object of class \code{lst_efficient}.
#' @param ... Additional arguments (not used).
#'
#' @return The function returns the input \code{x} invisibly.
#' @method print lst_efficient
#' @family revision analysis
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'   n = 0:3
#' )
#'
#' final_release <- get_nth_release(
#'   tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'   n = 10
#' )
#'
#' result <- get_first_efficient_release(df, final_release, significance = 0.05)
#' print(result)
#' @export
print.lst_efficient <- function(x, ...) {
  summary.lst_efficient(x, ...)
}

#' Summary of Efficient Release Models
#'
#' Provides a detailed summary of the regression model and hypothesis test for
#' the first efficient release identified by the
#' `get_first_efficient_release()` function.
#'
#' @param object An output object from the
#' `get_first_efficient_release` function. The object must be of
#' class `lst_efficient`.
#' @param ... Additional arguments (not used).
#'
#' @details
#' This function prints the following information:
#'
#' - The index of the first efficient release.
#' - A summary of the regression model fitted for the efficient release,
#' which includes coefficients,
#'   R-squared values, and other relevant statistics.
#' - The hypothesis test results for the efficient release, showing the
#' test statistic and p-value
#'   for the null hypothesis of unbiasedness and efficiency.
#'
#' The function assumes the object includes:
#'
#' - `e`: The index of the first efficient release (0-based).
#' - `models`: A list of linear regression models for each release.
#' - `tests`: A list of hypothesis test results corresponding to each release.
#'
#' @return Returns a tibble with the following columns:
#' - `id`: The identifier of the time series (if present in input data).
#' - `e`: The index of the first efficient release.
#' - `alpha`: The intercept coefficient of the regression model.
#' - `beta`: The coefficient of the slope.
#' - `p-value`: The p-value for the joint hypothesis (alpha = 0 and beta = 1).
#' - `n_tested`: The number of releases tested.
#'
#' @examples
#' # Example usage
#' df <- get_nth_release(
#'   tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'   n = 1:4
#' )
#'
#' final_release <- get_nth_release(
#'   tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
#'   n = 10
#' )
#'
#' # Identify the first efficient release
#' result <- get_first_efficient_release(df, final_release, significance = 0.05)
#' summary(result)
#'
#' @family revision analysis
#' @export
summary.lst_efficient <- function(object, ...) {
  is_id_list <- is.null(object$e)
  df_out <- NULL
  if (is_id_list) {
    for (iidd in names(object)) {
      cat("id: ", iidd, "\n")
      if (is.na(object[[iidd]]$e)) {
        cat("No efficient release found! \n")
      } else {
        cat("Efficient release: ", object[[iidd]]$e, "\n\n")
        cat("Model summary: \n")
        print(summary(object[[iidd]]$models[[object[[iidd]]$e + 1]]))
        cat("\nTest summary: \n")
        print(object[[iidd]]$tests[[object[[iidd]]$e + 1]])
        cat("\n\n")
      }
      if (!is.na(object[[iidd]]$e)) {
        df_out <- dplyr::bind_rows(
          df_out,
          tibble::tibble(
            id = iidd,
            e = object[[iidd]]$e,
            alpha = stats::coef(summary(object[[iidd]]$models[[
              object[[iidd]]$e + 1
            ]]))[1, 1],
            beta = stats::coef(summary(object[[iidd]]$models[[
              object[[iidd]]$e + 1
            ]]))[2, 1],
            p_value = object[[iidd]]$tests[[object[[iidd]]$e + 1]][2, "Pr(>F)"],
            n_tested = length(object[[iidd]]$tests)
          )
        )
      } else {
        df_out <- dplyr::bind_rows(
          df_out,
          tibble::tibble(
            id = iidd,
            e = NA_real_,
            alpha = stats::coef(summary(object[[iidd]]$models[[length(
              object[[iidd]]$tests
            )]]))[1, 1],
            beta = stats::coef(summary(object[[iidd]]$models[[length(
              object[[iidd]]$tests
            )]]))[2, 1],
            p_value = object[[iidd]]$tests[[length(object[[iidd]]$tests)]][
              2,
              "Pr(>F)"
            ],
            n_tested = length(object[[iidd]]$tests)
          )
        )
      }
    }
  } else {
    if (is.na(object$e)) {
      cat("No efficient release found! \n")
    } else {
      cat("Efficient release: ", object$e, "\n\n")
      cat("Model summary: \n")
      print(summary(object$models[[object$e + 1]]))
      cat("\nTest summary: \n")
      print(object$tests[[object$e + 1]])
    }
    if (!is.na(object$e)) {
      df_out <- tibble::tibble(
        e = object$e,
        alpha = stats::coef(summary(object$models[[object$e + 1]]))[1, 1],
        beta = stats::coef(summary(object$models[[object$e + 1]]))[2, 1],
        p_value = object$tests[[object$e + 1]][2, "Pr(>F)"],
        n_tested = length(object$tests)
      )
    } else {
      df_out <- tibble::tibble(
        e = NA_real_,
        alpha = stats::coef(
          summary(object$models[[length(object$tests)]])
        )[1, 1],
        beta = stats::coef(
          summary(object$models[[length(object$tests)]])
        )[2, 1],
        p_value = object$tests[[length(object$tests)]][2, "Pr(>F)"],
        n_tested = length(object$tests)
      )
    }
  }

  return(invisible(df_out))
}


#' Revision Analysis Summary Statistics
#'
#' Calculates a comprehensive set of summary statistics and hypothesis tests
#' for revisions between initial and final data releases.
#'
#' @param df A data frame containing the initial data releases.
#' Must include columns:
#'
#'   - `time`: The time variable.
#'   - `value`: The observed values in the initial release.
#'   - Optionally, `release` (release identifier) and `id` (grouping variable).
#' @param final_release A data frame containing the final release data.
#' Must include columns:
#'
#'   - `time`: The time variable (matching the initial release data).
#'   - `value`: The observed values in the final release.
#' @param degree An integer between 1 and 5 specifying the level of
#' detail for the output:
#'    1: Default, includes information about revision size.
#'    2: includes correlation statistics of revision.
#'    3: includes news and noise tests.
#'    4: includes sign switches, seasonality analysis and Theil's U.
#'    5: Full set of all statistics and tests.
#' @param grouping_var A character string specifying the grouping variable in
#' the data frame. Defaults to `"pub_date"` or `"release"` if available.
#'
#' @details
#' This function performs a variety of statistical analyses to understand
#' the nature of revisions between the initial and final data releases.
#' The function:
#'
#' - Checks the input data for consistency and transforms it as necessary.
#' - Merges the initial and final release datasets by their time variable and
#' optional grouping variables (`id` or `release`).
#' - Computes summary statistics such as the mean, standard deviation, and
#' range of the revisions.
#' - Performs hypothesis tests for bias, efficiency, and correlation using
#' robust methods (e.g., Newey-West standard errors).
#' - Includes tests for seasonality, noise, and news effects.
#'
#' Key tests include:
#' - **Bias Tests**: Tests for the presence of mean bias and regression bias.
#' - **Autocorrelation and Seasonality**: Tests for serial correlation and
#' seasonal patterns in revisions.
#' - **Theil's U Statistics**: Measures predictive accuracy of the initial
#' releases relative to the final values.
#' - **Noise vs. News**: Differentiates between unpredictable errors (noise)
#' and systematic adjustments (news).
#'
#' The function supports grouped calculations based on the presence
#' of `id` or `release` columns in the input.
#'
#' The following statistics and tests are calculated (See the vignette
#' \code{vignette("revision-analysis")} for more details):
#'
#' \itemize{
#'   \item **N**: The number of observations in the group.
#'   \item **Frequency**: The inferred data frequency (e.g., 12 for monthly
#'   or 4 for quarterly data).
#'   \item **Bias (mean)**: The mean revision, testing whether revisions are
#'   systematically biased.
#'   \item **Bias (p-value)**: p-value from a t-test evaluating the
#'   significance of the mean revision.
#'   \item **Bias (robust p-value)**: Newey-West HAC robust p-value for the
#'   mean revision test.
#'   \item **Minimum**: The minimum revision in the group.
#'   \item **Maximum**: The maximum revision in the group.
#'   \item **10Q**: The 10th percentile revision.
#'   \item **Median**: The median revision.
#'   \item **90Q**: The 90th percentile revision.
#'   \item **MAR**: The mean absolute revision.
#'   \item **Std. Dev.**: The standard deviation of revisions, indicating
#'   their variability.
#'   \item **Noise/Signal**: The ratio of the standard deviation of revisions
#'   to the standard deviation of final values.
#'   \item **Correlation**: The Pearson correlation between revisions and
#'   initial values, testing the relationship.
#'   \item **Correlation (p-value)**: p-value for the significance of
#'   the correlation.
#'   \item **Autocorrelation (1st)**: The first-order autocorrelation of
#'   revisions, measuring persistence.
#'   \item **Autocorrelation (1st p-value)**: p-value for the first-order
#'   autocorrelation test.
#'   \item **Autocorrelation up to 1yr (Ljung-Box p-value)**: p-value for the
#'   Ljung-Box test for higher-order autocorrelation.
#'   \item **Theil's U1**: A normalized measure of forecast accuracy,
#'   comparing the root mean squared error (RMSE) of revisions to the RMSE of
#'   final and initial values.
#'   \item **Theil's U2**: Compares forecast changes to actual changes.
#'   \item **Seasonality (Friedman p-value)**: p-value from the Friedman test
#'   for seasonality in revisions.
#'   \item **News joint test (p-value)**: p-value for the joint news test.
#'   \item **News test Intercept**: The estimated intercept from the news
#'   test regression.
#'   \item **News test Intercept (std.err)**: The standard error of the
#'   intercept in the news test regression.
#'   \item **News test Intercept (p-value)**: p-value for the intercept in
#'   the news test regression.
#'   \item **News test Coefficient**: The estimated coefficient for the
#'   `value` in the news test regression.
#'   \item **News test Coefficient (std.err)**: The standard error of the
#'   coefficient in the news test regression.
#'   \item **News test Coefficient (p-value)**: p-value for the coefficient
#'   in the news test regression.
#'   \item **Noise joint test (p-value)**: p-value for the joint noise test.
#'   \item **Noise test Intercept**: The estimated intercept from the noise
#'   test regression.
#'   \item **Noise test Intercept (std.err)**: The standard error of the
#'   intercept in the noise test regression.
#'   \item **Noise test Intercept (p-value)**: p-value for the intercept in
#'   the noise test regression.
#'   \item **Noise test Coefficient**: The estimated coefficient for the
#'   `final_value` in the noise test regression.
#'   \item **Noise test Coefficient (std.err)**: The standard error of the
#'   coefficient in the noise test regression.
#'   \item **Noise test Coefficient (p-value)**: p-value for the coefficient
#'   in the noise test regression.
#'   \item **Fraction of correct sign**: The fraction of correct sign changes
#'   in revisions.
#'   \item **Fraction of correct growth rate change**: The fraction of correct
#'   sign changes of growth rates in revisions.
#' }
#'
#' @srrstats {G1.3} Terminology explained here and in vignette.
#' @srrstats {G2.14a} Function removes missings before passing data
#' @srrstats {G2.14b} Function removes missings before passing data
#' @srrstats {G2.15} Function removes missings before passing data
#' @srrstats {TS1.6} ordering taken care of
#' @srrstats {TS2.1} Function removes missings before passing data
#' @srrstats {TS2.1a}Function removes missings before passing data
#' @srrstats {TS2.1b} Function removes missings before passing data
#'
#' @return A data frame with one row per grouping (if applicable) and columns
#' for summary statistics and test results. The resulting data frame is of
#' class `revision_summary`.
#'
#' @examples
#' # Example usage:
#' df_small <- dplyr::filter(
#'   reviser::gdp,
#'   id == "US",
#'   time >= as.Date("2018-01-01")
#' )
#'
#' df <- dplyr::select(
#'   get_nth_release(df_small, n = 0:2),
#'   -"pub_date"
#' )
#'
#' final_release <- dplyr::select(
#'   get_nth_release(df_small, n = "latest"),
#'   -"pub_date"
#' )
#'
#' results <- get_revision_analysis(
#'   df,
#'   final_release
#' )
#'
#' @family revision analysis
#' @export
get_revision_analysis <- function(
  df,
  final_release,
  degree = 1,
  grouping_var = NULL
) {
  # Check degree in 1:5
  if (!degree %in% c(1:5)) {
    rlang::abort("The 'degree' must be an integer between 1 and 5.")
  }

  # Check grouping variable present in both df and final_release
  if (!is.null(grouping_var)) {
    if (
      !grouping_var %in% colnames(df) ||
        !grouping_var %in% colnames(final_release)
    ) {
      rlang::abort(
        "The grouping variable must be present in 'df' and 'final_release'."
      )
    }
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  check <- vintages_check(final_release)
  if (check == "wide") {
    final_release <- vintages_long(final_release)
  }
  final_release <- vintages_assign_class(final_release)

  if (!any(c("tbl_release", "tbl_pubdate") %in% class(df))) {
    rlang::abort("'df' must be a 'tbl_release' or 'tbl_pubdate' object.")
  }

  # Check that id is present in both data.frames or neither
  if ("id" %in% colnames(df) && "id" %in% colnames(final_release)) {
    if (!identical(sort(unique(df$id)), sort(unique(final_release$id)))) {
      rlang::abort("The same 'id' must be present in 'df' and 'final_release'.")
    }
  } else if ("id" %in% colnames(df) || "id" %in% colnames(final_release)) {
    rlang::abort(
      "Both or none of 'df' and 'final_release' must have an 'id' column."
    )
  }

  # If both pub_date and release columns are present in df, use release column
  if (
    ("release" %in% colnames(df) && "pub_date" %in% colnames(df)) &&
      is.null(grouping_var)
  ) {
    rlang::warn(
      "Both 'release' and 'pub_date' columns are present in 'df.
      The 'release' column will be used for grouping."
    )
    df <- df |>
      dplyr::select(-"pub_date")
  }

  if (!is.null(grouping_var)) {
    df_var <- grouping_var
  } else if (any(grepl("pub_date", colnames(df)))) {
    df_var <- "pub_date"
  } else if (any(grepl("release", colnames(df)))) {
    df_var <- "release"
  } else {
    rlang::abort("The 'df' object must have a 'release' or 'pub_date' column.")
  }

  final_release <- final_release |>
    dplyr::rename("final_value" = "value")

  # Check if df has id column:
  if (
    "id" %in%
      colnames(df) &&
      "id" %in% colnames(final_release) &&
      length(unique(df$id)) > 0
  ) {
    final_release <- dplyr::select(
      final_release,
      "time",
      "final_value",
      "id"
    )

    df <- df |>
      dplyr::select("time", "value", dplyr::all_of(df_var), "id")

    df <- df |>
      dplyr::left_join(final_release, by = c("time" = "time", "id" = "id")) |>
      dplyr::arrange(.data$id, .data$time) |>
      stats::na.omit()
  } else {
    final_release <- dplyr::select(final_release, "time", "final_value")

    df <- df |>
      dplyr::select("time", "value", dplyr::all_of(df_var))

    df <- df |>
      dplyr::left_join(final_release, by = c("time" = "time")) |>
      dplyr::arrange(.data$time) |>
      stats::na.omit()
  }

  revisions <- df |>
    dplyr::mutate(
      revision = .data$final_value - .data$value,
    )

  # if no id or release column present, create a dummy id column
  if (!any(c("id", "release", "pub_date") %in% colnames(revisions))) {
    revisions <- revisions |>
      dplyr::mutate(id = "release_0")
  }

  # Defing grouping vars
  if (all(c(df_var, "id") %in% colnames(revisions))) {
    grouping_vars <- c("id", df_var)
  } else if ("id" %in% colnames(revisions)) {
    grouping_vars <- c("id")
  } else if (df_var %in% colnames(revisions)) {
    grouping_vars <- c(df_var)
  }

  # Check that there are at least N = 10 observation paires per group
  if (
    any(
      revisions |>
        dplyr::count(dplyr::across(dplyr::all_of(grouping_vars))) |>
        dplyr::pull(.data$n) <
        8
    )
  ) {
    rlang::abort(
      "Need at least 8 observations per group to compute the statistics."
    )
  }

  # Apply the computation to each group and combine the results
  results <- revisions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
    dplyr::group_modify(~ compute_revision_stats(.x)) |>
    dplyr::ungroup()

  results <- results |>
    tidyr::pivot_wider(
      names_from = "Statistic",
      values_from = "Value"
    )

  class(results) <- c("revision_summary", class(results))

  if (degree == 1) {
    # Descriptives
    results <- results |>
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "Bias (mean)",
        "Bias (p-value)",
        "Bias (robust p-value)",
        "Minimum",
        "Maximum",
        "10Q",
        "Median",
        "90Q",
        "MAR",
        "Std. Dev.",
        "Noise/Signal"
      )
    return(results)
  } else if (degree == 2) {
    # Correlation
    results <- results |>
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "Correlation",
        "Correlation (p-value)",
        "Autocorrelation (1st)",
        "Autocorrelation (1st p-value)",
        "Autocorrelation up to 1yr (Ljung-Box p-value)"
      )
    return(results)
  } else if (degree == 3) {
    # News/Noise tests
    results <- results |>
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "News test Intercept",
        "News test Intercept (std.err)",
        "News test Intercept (p-value)",
        "News test Coefficient",
        "News test Coefficient (std.err)",
        "News test Coefficient (p-value)",
        "News joint test (p-value)",
        "Noise test Intercept",
        "Noise test Intercept (std.err)",
        "Noise test Intercept (p-value)",
        "Noise test Coefficient",
        "Noise test Coefficient (std.err)",
        "Noise test Coefficient (p-value)",
        "Noise joint test (p-value)"
      )
    return(results)
  } else if (degree == 4) {
    # News and noise tests
    results <- results |>
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "Fraction of correct sign",
        "Fraction of correct growth rate change",
        "Theil's U1",
        "Theil's U2",
        "Seasonality (Friedman p-value)"
      )
    return(results)
  } else if (degree == 5) {
    return(results)
  }
}

#' Print Method for Revision Summary
#'
#' @param x An object of class \code{revision_summary}.
#' @param interpretation Logical. If TRUE, provides interpretation of key
#' statistics. Default is TRUE.
#' @param digits Integer. Number of digits to display. Default is 3.
#' @param ... Additional arguments (not used).
#'
#' @return The function returns the input \code{x} invisibly.
#' @method print revision_summary
#' @family revision analysis
#' @examples
#' df <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = 0:3
#'   ),
#'   -"pub_date"
#' )
#'
#' final_release <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = "latest"
#'   ),
#'   -"pub_date"
#' )
#'
#' results <- get_revision_analysis(df, final_release, degree = 1)
#' print(results)
#'
#' # Print without interpretation
#' print(results, interpretation = FALSE)
#' @export
print.revision_summary <- function(x, interpretation = TRUE, digits = 3, ...) {
  cat("\n=== Revision Analysis Summary ===\n\n")

  # Determine grouping variables
  grouping_vars <- setdiff(
    colnames(x)[
      !colnames(x) %in%
        c(
          "N",
          "Frequency",
          "Bias (mean)",
          "Bias (p-value)",
          "Bias (robust p-value)",
          "Minimum",
          "Maximum",
          "10Q",
          "Median",
          "90Q",
          "MAR",
          "Std. Dev.",
          "Noise/Signal",
          "Correlation",
          "Correlation (p-value)",
          "Autocorrelation (1st)",
          "Autocorrelation (1st p-value)",
          "Autocorrelation up to 1yr (Ljung-Box p-value)",
          "Theil's U1",
          "Theil's U2",
          "Seasonality (Friedman p-value)",
          "News test Intercept",
          "News test Intercept (std.err)",
          "News test Intercept (p-value)",
          "News test Coefficient",
          "News test Coefficient (std.err)",
          "News test Coefficient (p-value)",
          "News joint test (p-value)",
          "Noise test Intercept",
          "Noise test Intercept (std.err)",
          "Noise test Intercept (p-value)",
          "Noise test Coefficient",
          "Noise test Coefficient (std.err)",
          "Noise test Coefficient (p-value)",
          "Noise joint test (p-value)",
          "Fraction of correct sign",
          "Fraction of correct growth rate change"
        )
    ],
    c()
  )

  # Print the data
  x_print <- x

  # Round numeric columns
  numeric_cols <- vapply(x_print, is.numeric, logical(1))
  x_print[numeric_cols] <- lapply(x_print[numeric_cols], round, digits)

  # Remove class for printing
  class(x_print) <- class(x_print)[class(x_print) != "revision_summary"]
  print(x_print, ...)

  # Add interpretation if requested
  if (interpretation && nrow(x) > 0) {
    cat("\n=== Interpretation ===\n")

    for (i in seq_len(nrow(x))) {
      row <- x[i, ]

      # Header for group
      if (length(grouping_vars) > 0) {
        group_label <- paste(
          vapply(
            grouping_vars,
            function(v) {
              paste0(v, "=", row[[v]])
            },
            FUN.VALUE = character(1)
          ),
          collapse = ", "
        )
        cat("\n", group_label, ":\n", sep = "")
      } else if (nrow(x) > 1) {
        cat("\nGroup", i, ":\n")
      } else {
        cat("\n")
      }

      # Bias interpretation
      if ("Bias (robust p-value)" %in% colnames(row)) {
        bias_p <- row[["Bias (robust p-value)"]]
        bias_mean <- row[["Bias (mean)"]]

        if (!is.na(bias_p) && !is.na(bias_mean)) {
          if (bias_p < 0.05) {
            direction <- if (bias_mean > 0) "upward" else "downward"
            cat(
              "  \u2022 Significant",
              direction,
              "bias detected (p =",
              round(bias_p, 3),
              ")\n"
            )
          } else {
            cat(
              "  \u2022 No significant bias detected (p =",
              round(bias_p, 3),
              ")\n"
            )
          }
        }
      }

      # Noise/Signal ratio interpretation
      if ("Noise/Signal" %in% colnames(row)) {
        ns_ratio <- row[["Noise/Signal"]]
        if (!is.na(ns_ratio)) {
          if (ns_ratio < 0.1) {
            cat(
              "  \u2022 Very low revision volatility (Noise/Signal =",
              round(ns_ratio, 3),
              ")\n"
            )
          } else if (ns_ratio < 0.3) {
            cat(
              "  \u2022 Moderate revision volatility (Noise/Signal =",
              round(ns_ratio, 3),
              ")\n"
            )
          } else {
            cat(
              "  \u2022 High revision volatility (Noise/Signal =",
              round(ns_ratio, 3),
              ")\n"
            )
          }
        }
      }

      # Correlation interpretation
      if ("Correlation (p-value)" %in% colnames(row)) {
        cor_p <- row[["Correlation (p-value)"]]
        cor_val <- row[["Correlation"]]

        if (!is.na(cor_p) && !is.na(cor_val)) {
          if (cor_p < 0.05) {
            direction <- if (cor_val > 0) "positive" else "negative"
            cat(
              "  \u2022 Significant",
              direction,
              "correlation between revisions and initial values (\u03C1 =",
              round(cor_val, 3),
              ", p =",
              round(cor_p, 3),
              ")\n"
            )
          }
        }
      }

      # News/Noise test interpretation
      if ("News joint test (p-value)" %in% colnames(row)) {
        news_p <- row[["News joint test (p-value)"]]
        if (!is.na(news_p)) {
          if (news_p < 0.05) {
            cat(
              "  \u2022 Revisions contain NEWS (p =",
              round(news_p, 3),
              "): systematic information\n"
            )
          } else {
            cat(
              "  \u2022 Revisions do NOT contain news (p =",
              round(news_p, 3),
              ")\n"
            )
          }
        }
      }

      if ("Noise joint test (p-value)" %in% colnames(row)) {
        noise_p <- row[["Noise joint test (p-value)"]]
        if (!is.na(noise_p)) {
          if (noise_p < 0.05) {
            cat(
              "  \u2022 Revisions contain NOISE (p =",
              round(noise_p, 3),
              "): measurement error\n"
            )
          } else {
            cat(
              "  \u2022 Revisions do NOT contain noise (p =",
              round(noise_p, 3),
              ")\n"
            )
          }
        }
      }

      # Autocorrelation interpretation
      if ("Autocorrelation (1st p-value)" %in% colnames(row)) {
        auto_p <- row[["Autocorrelation (1st p-value)"]]
        auto_val <- row[["Autocorrelation (1st)"]]

        if (!is.na(auto_p) && !is.na(auto_val)) {
          if (auto_p < 0.05) {
            cat(
              "  \u2022 Significant autocorrelation in revisions
              (\u03C1\u2081 =", round(auto_val, 3),
              "): revisions are persistent\n"
            )
          }
        }
      }

      # Theil's U interpretation
      if ("Theil's U1" %in% colnames(row)) {
        u1 <- row[["Theil's U1"]]
        if (!is.na(u1)) {
          if (u1 < 0.3) {
            cat(
              "  \u2022 Good forecast accuracy (Theil's U1 =",
              round(u1, 3),
              ")\n"
            )
          } else if (u1 < 0.6) {
            cat(
              "  \u2022 Moderate forecast accuracy (Theil's U1 =",
              round(u1, 3),
              ")\n"
            )
          } else {
            cat(
              "  \u2022 Poor forecast accuracy (Theil's U1 =",
              round(u1, 3),
              ")\n"
            )
          }
        }
      }

      # Sign correctness interpretation
      if ("Fraction of correct sign" %in% colnames(row)) {
        sign_correct <- row[["Fraction of correct sign"]]
        if (!is.na(sign_correct)) {
          pct <- round(sign_correct * 100, 1)
          if (pct > 90) {
            cat(
              "  \u2022 Excellent sign prediction (",
              pct,
              "% correct)\n",
              sep = ""
            )
          } else if (pct > 70) {
            cat(
              "  \u2022 Good sign prediction (",
              pct,
              "% correct)\n",
              sep = ""
            )
          } else {
            cat(
              "  \u2022 Poor sign prediction (",
              pct,
              "% correct)\n",
              sep = ""
            )
          }
        }
      }
    }
  }

  invisible(x)
}

#' Diagnose Revision Quality
#'
#' Provides a quick diagnostic summary of revision quality with color-coded
#' pass/fail indicators for key metrics.
#'
#' @param object An object of class \code{revision_summary}.
#' @param alpha Significance level for hypothesis tests. Default is 0.05.
#' @param ... Additional arguments (not used).
#'
#' @return A tibble with diagnostic results.
#' @examples
#' # Example usage with revision analysis results
#' df <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = 0:3
#'   ),
#'   -"pub_date"
#' )
#'
#' final_release <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = "latest"
#'   ),
#'   -"pub_date"
#' )
#'
#' # Get revision analysis results
#' results <- get_revision_analysis(df, final_release, degree = 5)
#'
#' # Diagnose revision quality
#' diagnose(results)
#' @family revision analysis
#' @export
diagnose.revision_summary <- function(object, alpha = 0.05, ...) {
  cat("\n=== Revision Quality Diagnostics ===\n\n")

  # Determine grouping variables
  grouping_vars <- setdiff(
    colnames(object)[
      !colnames(object) %in%
        c(
          "N",
          "Frequency",
          "Bias (mean)",
          "Bias (p-value)",
          "Bias (robust p-value)",
          "Minimum",
          "Maximum",
          "10Q",
          "Median",
          "90Q",
          "MAR",
          "Std. Dev.",
          "Noise/Signal",
          "Correlation",
          "Correlation (p-value)",
          "Autocorrelation (1st)",
          "Autocorrelation (1st p-value)",
          "Autocorrelation up to 1yr (Ljung-Box p-value)",
          "Theil's U1",
          "Theil's U2",
          "Seasonality (Friedman p-value)",
          "News test Intercept",
          "News test Intercept (std.err)",
          "News test Intercept (p-value)",
          "News test Coefficient",
          "News test Coefficient (std.err)",
          "News test Coefficient (p-value)",
          "News joint test (p-value)",
          "Noise test Intercept",
          "Noise test Intercept (std.err)",
          "Noise test Intercept (p-value)",
          "Noise test Coefficient",
          "Noise test Coefficient (std.err)",
          "Noise test Coefficient (p-value)",
          "Noise joint test (p-value)",
          "Fraction of correct sign",
          "Fraction of correct growth rate change"
        )
    ],
    c()
  )

  results <- list()

  for (i in seq_len(nrow(object))) {
    row <- object[i, ]

    diagnostics <- tibble::tibble(
      Metric = character(),
      Status = character(),
      Value = character(),
      Assessment = character()
    )

    # Group identifier
    if (length(grouping_vars) > 0) {
      group_id <- paste(
        vapply(
          grouping_vars,
          function(v) {
            as.character(row[[v]])
          },
          FUN.VALUE = character(1)
        ),
        collapse = "_"
      )
    } else {
      group_id <- paste0("Group_", i)
    }

    # Test 1: Bias
    if ("Bias (robust p-value)" %in% colnames(row)) {
      bias_p <- row[["Bias (robust p-value)"]]
      bias_mean <- row[["Bias (mean)"]]

      if (!is.na(bias_p)) {
        status <- if (bias_p >= alpha) "\u2713 PASS" else "\u2717 FAIL"
        value <- paste0(
          "p=", round(bias_p, 3),
          ", \u03BC=", round(bias_mean, 3)
        )
        assessment <- if (bias_p >= alpha) {
          "No significant bias"
        } else {
          paste0(
            "Significant ",
            if (bias_mean > 0) "upward" else "downward",
            " bias"
          )
        }

        diagnostics <- dplyr::bind_rows(
          diagnostics,
          tibble::tibble(
            Metric = "Unbiasedness",
            Status = status,
            Value = value,
            Assessment = assessment
          )
        )
      }
    }

    # Test 2: Noise/Signal Ratio
    if ("Noise/Signal" %in% colnames(row)) {
      ns <- row[["Noise/Signal"]]

      if (!is.na(ns)) {
        status <- if (ns < 0.3) {
          "\u2713 GOOD"
        } else if (ns < 0.5) {
          "~ OK"
        } else {
          "\u2717 HIGH"
        }
        value <- round(ns, 3)
        assessment <- if (ns < 0.3) {
          "Low revision volatility"
        } else if (ns < 0.5) {
          "Moderate revision volatility"
        } else {
          "High revision volatility"
        }

        diagnostics <- dplyr::bind_rows(
          diagnostics,
          tibble::tibble(
            Metric = "Noise/Signal",
            Status = status,
            Value = as.character(value),
            Assessment = assessment
          )
        )
      }
    }

    # Test 3: News
    if ("News joint test (p-value)" %in% colnames(row)) {
      news_p <- row[["News joint test (p-value)"]]

      if (!is.na(news_p)) {
        status <- if (news_p >= alpha) "\u2713 PASS" else "\u2717 FAIL"
        value <- paste0("p=", round(news_p, 3))
        assessment <- if (news_p >= alpha) {
          "No news component"
        } else {
          "Contains systematic information"
        }

        diagnostics <- dplyr::bind_rows(
          diagnostics,
          tibble::tibble(
            Metric = "News Test",
            Status = status,
            Value = value,
            Assessment = assessment
          )
        )
      }
    }

    # Test 4: Noise
    if ("Noise joint test (p-value)" %in% colnames(row)) {
      noise_p <- row[["Noise joint test (p-value)"]]

      if (!is.na(noise_p)) {
        status <- if (noise_p >= alpha) "\u2713 PASS" else "\u2717 FAIL"
        value <- paste0("p=", round(noise_p, 3))
        assessment <- if (noise_p >= alpha) {
          "No noise component"
        } else {
          "Contains measurement error"
        }

        diagnostics <- dplyr::bind_rows(
          diagnostics,
          tibble::tibble(
            Metric = "Noise Test",
            Status = status,
            Value = value,
            Assessment = assessment
          )
        )
      }
    }

    # Test 5: Forecast Accuracy
    if ("Theil's U1" %in% colnames(row)) {
      u1 <- row[["Theil's U1"]]

      if (!is.na(u1)) {
        status <- if (u1 < 0.3) {
          "\u2713 GOOD"
        } else if (u1 < 0.6) {
          "~ OK"
        } else {
          "\u2717 POOR"
        }
        value <- round(u1, 3)
        assessment <- if (u1 < 0.3) {
          "Good forecast accuracy"
        } else if (u1 < 0.6) {
          "Moderate forecast accuracy"
        } else {
          "Poor forecast accuracy"
        }

        diagnostics <- dplyr::bind_rows(
          diagnostics,
          tibble::tibble(
            Metric = "Theil's U1",
            Status = status,
            Value = as.character(value),
            Assessment = assessment
          )
        )
      }
    }

    # Test 6: Sign Prediction
    if ("Fraction of correct sign" %in% colnames(row)) {
      sign_pct <- row[["Fraction of correct sign"]] * 100

      if (!is.na(sign_pct)) {
        status <- if (sign_pct > 90) {
          "\u2713 GOOD"
        } else if (sign_pct > 70) {
          "~ OK"
        } else {
          "\u2717 POOR"
        }
        value <- paste0(round(sign_pct, 1), "%")
        assessment <- if (sign_pct > 90) {
          "Excellent sign prediction"
        } else if (sign_pct > 70) {
          "Good sign prediction"
        } else {
          "Poor sign prediction"
        }

        diagnostics <- dplyr::bind_rows(
          diagnostics,
          tibble::tibble(
            Metric = "Sign Accuracy",
            Status = status,
            Value = value,
            Assessment = assessment
          )
        )
      }
    }

    # Print for this group
    cat(group_id, ":\n")
    print(diagnostics)
    cat("\n")

    # Store results
    diagnostics$Group <- group_id
    results[[i]] <- diagnostics
  }

  # Overall summary
  all_results <- dplyr::bind_rows(results)

  if (nrow(all_results) > 0) {
    n_pass <- sum(grepl("PASS|GOOD", all_results$Status))
    n_total <- nrow(all_results)
    pct_pass <- round(100 * n_pass / n_total, 1)

    cat("=== Overall Assessment ===\n")
    cat(
      "Passed:",
      n_pass,
      "of",
      n_total,
      "checks (",
      pct_pass,
      "%)\n",
      sep = " "
    )

    if (pct_pass >= 80) {
      cat("Overall: \u2713 GOOD - Revisions are of high quality\n")
    } else if (pct_pass >= 60) {
      cat("Overall: ~ MODERATE - Some revision quality issues detected\n")
    } else {
      cat("Overall: \u2717 POOR - Significant revision quality issues\n")
    }
  }

  invisible(all_results)
}

#' Diagnose Revision Quality
#'
#' Generic function to provide diagnostic summaries for revision analysis
#' objects.
#'
#' @param object An object for which diagnostics are desired.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method-specific diagnostic output.
#'
#' @examples
#' # Example usage with revision analysis results
#' df <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = 0:3
#'   ),
#'   -"pub_date"
#' )
#'
#' final_release <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = "latest"
#'   ),
#'   -"pub_date"
#' )
#'
#' # Get revision analysis results
#' results <- get_revision_analysis(df, final_release, degree = 5)
#'
#' # Diagnose revision quality
#' diagnose(results)
#'
#' @family revision analysis
#' @export
diagnose <- function(object, ...) {
  UseMethod("diagnose")
}
#' Summary Method for Revision Summary
#'
#' @param object An object of class \code{revision_summary}.
#' @param interpretation Logical. If TRUE, provides interpretation of key
#' statistics. Default is TRUE.
#' @param ... Additional arguments passed to print.
#'
#' @return The function returns the input \code{object} invisibly.
#' @method summary revision_summary
#' @family revision analysis
#' @examples
#' # Example usage with revision analysis results
#' df <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = 0:3
#'   ),
#'   -"pub_date"
#' )
#'
#' final_release <- dplyr::select(
#'   get_nth_release(
#'     na.omit(
#'       tsbox::ts_pc(
#'         dplyr::filter(reviser::gdp, id == "US")
#'       )
#'     ),
#'     n = "latest"
#'   ),
#'   -"pub_date"
#' )
#'
#' # Get revision analysis results
#' results <- get_revision_analysis(df, final_release, degree = 5)
#'
#' # Summarize revision quality
#' summary(results)
#' @export
summary.revision_summary <- function(object, interpretation = TRUE, ...) {
  print.revision_summary(object, interpretation = interpretation, ...)
}

#' Function to compute statistics and tests for each group
#' @param data vector
#' @return A data frame with the computed statistics
#' @srrstats {TS1.8} rounded to whole days
#' @keywords internal
#' @noRd
compute_revision_stats <- function(data) {
  N <- nrow(data)
  freq <- round(
    1 / ((mean(((as.numeric(diff(unique(data$time))) / 360)), na.rm = TRUE)))
  )
  mean_revision <- mean(data$revision)
  mean_abs_revision <- mean(abs(data$revision))
  min_revision <- min(data$revision)
  max_revision <- max(data$revision)
  q10 <- stats::quantile(data$revision, 0.1)
  q50 <- stats::median(data$revision)
  q90 <- stats::quantile(data$revision, 0.9)
  std_dev_revision <- stats::sd(data$revision)
  noise_to_signal <- std_dev_revision / stats::sd(data$final_value)
  correlation <- stats::cor(data$revision, data$value, use = "complete.obs")
  autocorrelation <- stats::cor(
    data$revision[-1],
    data$revision[-length(data$revision)],
    use = "complete.obs"
  )

  # Significance test for the Bias (mean) (Newey-West HAC standard errors)
  mean_test_model <- stats::lm(revision ~ 1, data = data)
  # Conventional t test:
  mean_p_value <- summary(mean_test_model)$coefficients[1, 4]

  # Newey-West HAC standard errors t test:
  # Throws an error if all revisions are 0, return NA in that case
  mean_test_se <- tryCatch(
    {
      nw <- sandwich::NeweyWest(mean_test_model)

      # Check if the result is numeric and finite before taking sqrt
      if (is.numeric(nw) && all(is.finite(nw))) {
        sqrt(nw)
      } else {
        NA_real_
      }
    },
    error = function(e) NA_real_, # Return NA if an error occurs
    warning = function(w) NA_real_ # Also handle warnings safely
  )

  mean_nw_t_stat <- stats::coef(mean_test_model)[1] / mean_test_se
  # Two-sided p-value
  mean_nw_p_value <- 2 * (1 - stats::pt(abs(mean_nw_t_stat), df = N - 1))

  # Significance test for correlation
  cor_t_stat <- correlation * sqrt((N - 2) / (1 - correlation^2))
  # Two-sided p-value
  cor_p_value <- 2 * (1 - stats::pt(abs(cor_t_stat), df = N - 2))

  # Significance test for autocorrelation
  auto_t_stat <- autocorrelation * sqrt((N - 2) / (1 - autocorrelation^2))
  # Two-sided p-value
  auto_p_value <- 2 * (1 - stats::pt(abs(auto_t_stat), df = N - 2))

  # Ljung Box:
  if (freq %in% c(4, 12)) {
    ljung_box <- stats::Box.test(
      data$revision,
      lag = freq,
      type = "Ljung-Box",
      fitdf = 1
    )$p.value
  } else {
    ljung_box <- NA
  }

  theils_u1 <- sqrt(mean((data$final_value - data$value)^2)) /
    (sqrt(mean(data$final_value^2)) + sqrt(mean(data$value^2)))

  theils_u2 <- sqrt(
    sum(
      (data$value[-1] - data$final_value[-1]) /
        data$final_value[-length(data$final_value)]
    )^2
  ) /
    sqrt(
      sum(
        (data$final_value[-1] - data$final_value[-length(data$final_value)]) /
          data$final_value[-length(data$final_value)]
      )^2
    )

  # Seasonality test
  if (freq %in% c(4, 12)) {
    friedman_p_value <- friedman_test(
      data$revision,
      frequency = freq
    )$`p_value`
  } else {
    friedman_p_value <- NA
  }

  # Noise test with error handling (ensures no break if all revisions are 0)
  noise_test_results <- tryCatch(
    {
      # Run the regression
      noise_test <- stats::lm(revision ~ final_value, data = data)

      # Compute HAC standard errors
      hac_se <- sandwich::vcovHAC(noise_test)

      # Wald test
      test <- car::linearHypothesis(
        noise_test,
        c("(Intercept) = 0", "final_value = 0"),
        vcov = hac_se
      )

      # Extract values safely
      noise_wald_p_value <- test[2, "Pr(>F)"]
      noise_intercept <- stats::coef(noise_test)[1]
      noise_intercept_stderr <- sqrt(diag(hac_se))[1]
      noise_intercept_p_value <- summary(noise_test)$coefficients[1, 4]
      noise_coeff <- stats::coef(noise_test)[2]
      noise_coeff_stderr <- sqrt(diag(hac_se))[2]
      noise_coeff_p_value <- summary(noise_test)$coefficients[2, 4]

      # Return results as a named list
      list(
        noise_wald_p_value = noise_wald_p_value,
        noise_intercept = noise_intercept,
        noise_intercept_stderr = noise_intercept_stderr,
        noise_intercept_p_value = noise_intercept_p_value,
        noise_coeff = noise_coeff,
        noise_coeff_stderr = noise_coeff_stderr,
        noise_coeff_p_value = noise_coeff_p_value
      )
    },
    error = function(e) {
      # Return a list with NA values if an error occurs
      list(
        noise_wald_p_value = NA,
        noise_intercept = NA,
        noise_intercept_stderr = NA,
        noise_intercept_p_value = NA,
        noise_coeff = NA,
        noise_coeff_stderr = NA,
        noise_coeff_p_value = NA
      )
    }
  )

  # Extract individual variables
  noise_wald_p_value <- noise_test_results$noise_wald_p_value
  noise_intercept <- noise_test_results$noise_intercept
  noise_intercept_stderr <- noise_test_results$noise_intercept_stderr
  noise_intercept_p_value <- noise_test_results$noise_intercept_p_value
  noise_coeff <- noise_test_results$noise_coeff
  noise_coeff_stderr <- noise_test_results$noise_coeff_stderr
  noise_coeff_p_value <- noise_test_results$noise_coeff_p_value

  # News test with error handling
  news_test_results <- tryCatch(
    {
      # Run the regression
      news_test <- stats::lm(revision ~ value, data = data)

      # Compute HAC standard errors
      hac_se <- sandwich::vcovHAC(news_test)

      # Wald test
      test <- car::linearHypothesis(
        news_test,
        c("(Intercept) = 0", "value = 0"),
        vcov = hac_se
      )

      # Extract values safely
      news_wald_p_value <- test[2, "Pr(>F)"]
      news_intercept <- stats::coef(news_test)[1]
      news_intercept_stderr <- sqrt(diag(hac_se))[1]
      news_intercept_p_value <- summary(news_test)$coefficients[1, 4]
      news_coeff <- stats::coef(news_test)[2]
      news_coeff_stderr <- sqrt(diag(hac_se))[2]
      news_coeff_p_value <- summary(news_test)$coefficients[2, 4]

      # Return results as a named list
      list(
        news_wald_p_value = news_wald_p_value,
        news_intercept = news_intercept,
        news_intercept_stderr = news_intercept_stderr,
        news_intercept_p_value = news_intercept_p_value,
        news_coeff = news_coeff,
        news_coeff_stderr = news_coeff_stderr,
        news_coeff_p_value = news_coeff_p_value
      )
    },
    error = function(e) {
      # Return a list with NA values if an error occurs
      list(
        news_wald_p_value = NA,
        news_intercept = NA,
        news_intercept_stderr = NA,
        news_intercept_p_value = NA,
        news_coeff = NA,
        news_coeff_stderr = NA,
        news_coeff_p_value = NA
      )
    }
  )

  # Extract individual variables
  news_wald_p_value <- news_test_results$news_wald_p_value
  news_intercept <- news_test_results$news_intercept
  news_intercept_stderr <- news_test_results$news_intercept_stderr
  news_intercept_p_value <- news_test_results$news_intercept_p_value
  news_coeff <- news_test_results$news_coeff
  news_coeff_stderr <- news_test_results$news_coeff_stderr
  news_coeff_p_value <- news_test_results$news_coeff_p_value

  # Computes the fraction of sign changes
  correct_sign <- data |>
    dplyr::mutate(
      early_sign = sign(.data$value),
      late_sign = sign(.data$final_value)
    ) |>
    dplyr::summarise(
      fraction_sign_correct = sum(
        (.data$early_sign - .data$late_sign) == 0
      ) /
        dplyr::n(),
      fraction_sign_wrong = 1 - .data$fraction_sign_correct,
      n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::pull(.data$fraction_sign_correct)

  # Get the fraction of changes in the sign of the change in the growth rate
  correct_change <- data |>
    dplyr::mutate(
      diff_value = .data$value - dplyr::lag(.data$value, 1),
      diff_final_value = .data$final_value - dplyr::lag(.data$final_value, 1)
    ) |>
    dplyr::mutate(
      early_sign = sign(.data$diff_value),
      late_sign = sign(.data$diff_final_value)
    ) |>
    dplyr::summarise(
      fraction_sign_correct = sum(
        (.data$early_sign - .data$late_sign) == 0,
        na.rm = TRUE
      ) /
        dplyr::n(),
      fraction_sign_wrong = 1 - .data$fraction_sign_correct,
      n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::pull("fraction_sign_correct")

  tibble::tibble(
    Statistic = c(
      "N",
      "Frequency",
      "Bias (mean)",
      "Bias (p-value)",
      "Bias (robust p-value)",
      "Minimum",
      "Maximum",
      "10Q",
      "Median",
      "90Q",
      "MAR",
      "Std. Dev.",
      "Noise/Signal",
      "Correlation",
      "Correlation (p-value)",
      "Autocorrelation (1st)",
      "Autocorrelation (1st p-value)",
      "Autocorrelation up to 1yr (Ljung-Box p-value)",
      "Theil's U1",
      "Theil's U2",
      "Seasonality (Friedman p-value)",
      "News test Intercept",
      "News test Intercept (std.err)",
      "News test Intercept (p-value)",
      "News test Coefficient",
      "News test Coefficient (std.err)",
      "News test Coefficient (p-value)",
      "News joint test (p-value)",
      "Noise test Intercept",
      "Noise test Intercept (std.err)",
      "Noise test Intercept (p-value)",
      "Noise test Coefficient",
      "Noise test Coefficient (std.err)",
      "Noise test Coefficient (p-value)",
      "Noise joint test (p-value)",
      "Fraction of correct sign",
      "Fraction of correct growth rate change"
    ),
    Value = c(
      N,
      freq,
      mean_revision,
      mean_p_value,
      mean_nw_p_value,
      min_revision,
      max_revision,
      q10,
      q50,
      q90,
      mean_abs_revision,
      std_dev_revision,
      noise_to_signal,
      correlation,
      cor_p_value,
      autocorrelation,
      auto_p_value,
      ljung_box,
      theils_u1,
      theils_u2,
      friedman_p_value,
      news_intercept,
      news_intercept_stderr,
      news_intercept_p_value,
      news_coeff,
      news_coeff_stderr,
      news_coeff_p_value,
      news_wald_p_value,
      noise_intercept,
      noise_intercept_stderr,
      noise_intercept_p_value,
      noise_coeff,
      noise_coeff_stderr,
      noise_coeff_p_value,
      noise_wald_p_value,
      correct_sign,
      correct_change
    )
  )
}


#' Function for Friedman Test used in `get_revision_analysis`
#' @param series vector
#' @param frequency integer
#' @noRd
friedman_test <- function(series, frequency = 12) {
  # First-difference the series
  diff_series <- diff(series)

  # Reshape the series into blocks (years) and treatments (months or quarters)
  n <- length(diff_series)
  n_blocks <- n %/% frequency # Number of full periods (years)

  # Truncate the series to fit into a complete matrix
  diff_series <- diff_series[1:(n_blocks * frequency)]

  # Create block and treatment identifiers
  blocks <- rep(1:n_blocks, each = frequency) # e.g., Year number
  treatments <- rep(1:frequency, times = n_blocks) # e.g., Month number

  # Convert to a data frame
  friedman_data <- data.frame(
    value = diff_series,
    block = factor(blocks),
    treatment = factor(treatments)
  )

  # Perform Friedman test
  test_result <- stats::friedman.test(
    value ~ treatment | block,
    data = friedman_data
  )

  # Return p-value
  return(list(p_value = test_result$p.value))
}

#' Extract the Nth Data Release (Vintage)
#'
#' Filters the input dataset to return the Nth release (or vintage) of data
#' for each time period. The function supports selecting the first, latest, or
#' a specific numbered release.
#'
#' @param df A data frame containing data vintages. The data frame must
#' include the columns:
#'
#' - `pub_date` (publication date of the release)
#' - `time` (thecorresponding time period for the data).
#' @param n The release number to extract. Accepts:
#'
#' - Non-negative integer or vector
#'   (e.g., 0 for first release, 1 for second, etc.)
#' - `"first"` to extract the first release.
#' - `"latest"` to extract the most recent release.
#' Default is 0 (the first release).
#' @param diagonal Logical. If `TRUE`, the function only returns real
#' first releases.
#'
#' @return A filtered data frame containing only the specified release(s).
#' The resulting data frame is assigned the class `tbl_release` to indicate
#' its structure. If diagonal is set to `TRUE`, the function only returns the
#' real first releases. That is historic values for which no vintages exist
#' are not returned.
#'
#' @details
#' The behavior depends on the value of `n`:
#' - **Non-negative integer**: The function retrieves the Nth release for each
#' time period (e.g., 0 = first release, 1 = second release, etc.).
#' - **"first"**: Retrieves the first release for each time
#' period (via `get_first_release`).
#' - **"latest"**: Retrieves the most recent release for each time
#' period (via `get_latest_release`).
#'
#' @srrstats {G2.3b} use `tolower()`
#' @srrstats {G2.6} input 'n' is approproately pre processed
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Get the first release (n = 0)
#' first_release <- get_nth_release(df, n = 0)
#'
#' # Get the latest release
#' latest_release <- get_nth_release(df, n = "latest")
#'
#' # Get the second release (n = 1)
#' second_release <- get_nth_release(df, n = 1)
#'
#' # Get the first and second release (n = 0:1)
#' releases <- get_nth_release(df, n = 0:1)
#'
#' @family revision utilities
#' @export
get_nth_release <- function(df, n = 0, diagonal = FALSE) {
  # Validate inputs
  if (is.numeric(n) && any(n < 0)) {
    rlang::abort("'n' must be a whole number >= 0, 'first', or 'latest'.")
  } else if (is.numeric(n)) {
    if (any(n %% 1 != 0)) {
      rlang::abort("'n' must be a whole number >= 0, 'first', or 'latest'.")
    }
    n <- as.integer(n)
  } else if (is.character(n) && !tolower(n) %in% c("first", "latest")) {
    rlang::abort("'n' must be a whole number >= 0, 'first', or 'latest'.")
  }

  if (!is.logical(diagonal)) {
    rlang::abort("The 'diagonal' argument must be a logical value.")
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  if ("id" %in% colnames(df) && length(unique(df$id)) > 1) {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$id, .data$pub_date, .data$time)

    if (is.numeric(n)) {
      n <- n + 1
      # Get the nth release(s)
      nth_release <- df |>
        dplyr::group_by(.data$time, .data$id) |>
        dplyr::mutate(
          release = paste0("release_", (seq_len(dplyr::n()) - 1))
        ) |>
        dplyr::slice(n) |>
        dplyr::ungroup()
    } else if (tolower(n) == "latest") {
      # Get the latest release
      nth_release <- get_latest_release(df)
    } else if (tolower(n) == "first") {
      # Get the first release
      nth_release <- get_first_release(df)
    }
    if (diagonal) {
      diagonal_thresholds <- df |>
        dplyr::group_by(.data$id) |>
        dplyr::summarise(
          min_pub_date = min(.data$pub_date),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          df |>
            dplyr::group_by(.data$id, .data$pub_date) |>
            dplyr::summarise(max_time = max(.data$time), .groups = "drop"),
          by = c("id", "min_pub_date" = "pub_date")
        ) |>
        dplyr::select("id", "max_time")

      nth_release <- nth_release |>
        dplyr::left_join(diagonal_thresholds, by = "id") |>
        dplyr::filter(.data$time >= .data$max_time) |>
        dplyr::select(-"max_time")
    }
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$pub_date, .data$time)

    if (is.numeric(n)) {
      n <- n + 1
      # Get the nth release(s)
      nth_release <- df |>
        dplyr::group_by(.data$time) |>
        dplyr::mutate(
          release = paste0("release_", (seq_len(dplyr::n()) - 1))
        ) |>
        dplyr::slice(n) |>
        dplyr::ungroup()
    } else if (tolower(n) == "latest") {
      # Get the latest release
      nth_release <- get_latest_release(df)
    } else if (tolower(n) == "first") {
      # Get the first release
      nth_release <- get_first_release(df)
    }
    if (diagonal) {
      min_pub_date <- min(df$pub_date)
      max_time <- max(df$time[df$pub_date == min_pub_date])
      nth_release <- nth_release |>
        dplyr::filter(
          .data$time >= max_time
        )
    }
  }

  # Add the class only if it is not already present
  nth_release <- vintages_assign_class(nth_release)
  return(nth_release)
}

#' Extract the First Data Release (Vintage)
#'
#' Filters the input dataset to return the earliest release (or vintage) for
#' each time period.
#'
#' @param df A data frame containing data vintages. The data frame must
#' include the columns `pub_date` (publication date of the release) and
#' `time` (the corresponding time period for the data).
#' @param diagonal Logical. If `TRUE`, the function only returns real
#' first releases.
#'
#' @return A filtered data frame containing only the first release(s).
#' The resulting data frame is assigned the class `tbl_release` to
#' indicate its structure.
#'
#' @details
#' For each time period, the function identifies the release with the earliest
#' publication date (`pub_date`). A new column `release` is added and labels
#' all rows in the resulting data frame as `release_0`. If diagonal is set
#' to `TRUE`, the function only returns the real first releases. That is
#' historic values for which no  vintages exist are not returned.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Get the first release for each time period
#' first_release <- get_first_release(df)
#'
#' @family revision utilities
#' @export
get_first_release <- function(df, diagonal = FALSE) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  if (!is.logical(diagonal)) {
    rlang::abort("The 'diagonal' argument must be a logical value.")
  }

  if ("id" %in% colnames(df)) {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$id, .data$pub_date, .data$time)

    df <- df |>
      dplyr::group_by(.data$id, .data$time) |>
      dplyr::mutate("release" = "release_0") |>
      dplyr::filter(.data$pub_date == min(.data$pub_date)) |>
      dplyr::ungroup()
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$pub_date, .data$time)

    df <- df |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate("release" = "release_0") |>
      dplyr::filter(.data$pub_date == min(.data$pub_date)) |>
      dplyr::ungroup()
  }

  if (diagonal) {
    if ("id" %in% colnames(df)) {
      diagonal_thresholds <- df |>
        dplyr::group_by(.data$id) |>
        dplyr::summarise(
          min_pub_date = min(.data$pub_date),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          df |>
            dplyr::group_by(.data$id, .data$pub_date) |>
            dplyr::summarise(max_time = max(.data$time), .groups = "drop"),
          by = c("id", "min_pub_date" = "pub_date")
        ) |>
        dplyr::select("id", "max_time")

      df <- df |>
        dplyr::left_join(diagonal_thresholds, by = "id") |>
        dplyr::filter(.data$time >= .data$max_time) |>
        dplyr::select(-"max_time")
    } else {
      min_pub_date <- min(df$pub_date)
      max_time <- max(df$time[df$pub_date == min_pub_date])
      df <- df |>
        dplyr::filter(.data$time >= max_time)
    }
  }

  # Add the class only if it is not already present
  df <- vintages_assign_class(df)
  return(df)
}

#' Extract the Latest Data Release (Vintage)
#'
#' Filters the input dataset to return the most recent release (or vintage)
#' for each time period.
#'
#' @param df A data frame containing data vintages. The data frame must include
#' the columns `pub_date` (publication date of the release) and
#' `time` (the corresponding time period for the data).
#'
#' @return A filtered data frame containing only the most recent release(s).
#' The resulting data frame is assigned the class `tbl_release` to indicate
#' its structure.
#'
#' @details
#' For each time period, the function identifies the release with the latest
#' publication date (`pub_date`)  and adds a column `release` that labels the
#' release as `release_N`, where `N` is the release index (zero indexed).
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Get the latest release for each time period
#' latest_release <- get_latest_release(df)
#'
#' @family revision utilities
#' @export
get_latest_release <- function(df) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  if ("id" %in% colnames(df)) {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$id, .data$pub_date, .data$time)
    df <- df |>
      dplyr::group_by(.data$id, .data$time) |>
      dplyr::mutate("release" = paste0("release_", dplyr::n() - 1)) |>
      dplyr::filter(.data$pub_date == max(.data$pub_date)) |>
      dplyr::ungroup()
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df |>
      dplyr::arrange(.data$pub_date, .data$time)
    df <- df |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate("release" = paste0("release_", dplyr::n() - 1)) |>
      dplyr::filter(.data$pub_date == max(.data$pub_date)) |>
      dplyr::ungroup()
  }

  # Add the class only if it is not already present
  df <- vintages_assign_class(df)
  return(df)
}

#' Extract Vintage Values from a Data Frame
#'
#' Some statistical agencies make a final revision of their data after a certain
#' period of time in a give month in the year. This function extracts values
#' from a given month or quarter a specified  number of years after the
#' initial release.
#'
#' @param df A data frame containing columns `pub_date` (publication date) and `
#' time` (observation date).
#' @param month An optional parameter specifying the target month as a name
#' ("July") or an integer (7). Cannot be used with `quarter`.
#' At least one of `month` or `quarter` must be supplied.
#' @param quarter An optional parameter specifying the target quarter (1-4).
#' Cannot be used with `month`. At least one of `month` or `quarter` must be
#' supplied.
#' @param years A single whole number of years after `pub_date` for which the
#' values should be extracted.
#'
#' @return A filtered data frame containing values matching the
#' specified criteria.
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#' dta <- get_fixed_release(df, month = "July", years = 3)
#' dta <- get_fixed_release(df, month = 7, years = 3)
#' dta <- get_fixed_release(df, quarter = 3, years = 3)
#'
#' @family revision utilities
#' @export
get_fixed_release <- function(df, years, month = NULL, quarter = NULL) {
  # Ensure only one of month or quarter is specified
  if (!is.null(month) && !is.null(quarter)) {
    rlang::abort("Specify either a month or a quarter, not both.")
  }

  # Ensure one target period is specified
  if (is.null(month) && is.null(quarter)) {
    rlang::abort("Specify one of 'month' or 'quarter'.")
  }

  # Ensure years is a single whole number
  if (
    !is.numeric(years) ||
      length(years) != 1 ||
      is.na(years) ||
      years %% 1 != 0
  ) {
    rlang::abort("'years' must be a single whole number.")
  }
  years <- as.integer(years)

  # Ensure the month is in numeric format if provided
  if (!is.null(month)) {
    if (is.character(month)) {
      if (length(month) != 1) {
        rlang::abort("Invalid 'month'. Must be a single month name or integer.")
      }
      month <- match(tolower(month), tolower(month.name))
      if (is.na(month)) rlang::abort("Invalid 'month' name")
    } else if (
      !is.numeric(month) || length(month) != 1 || is.na(month) ||
        month %% 1 != 0 || month < 1 || month > 12
    ) {
      rlang::abort(
        "Invalid 'month'. Must be an integer between 1 and 12 or a month name."
      )
    }
    month <- as.integer(month)
  }

  # Ensure quarter is in numeric format if provided
  if (!is.null(quarter)) {
    if (
      !is.numeric(quarter) || length(quarter) != 1 || is.na(quarter) ||
        quarter %% 1 != 0 || !quarter %in% 1:4
    ) {
      rlang::abort("Invalid quarter number. Must be between 1 and 4.")
    }
    quarter <- as.integer(quarter)
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  # Define quarter months if quarter is specified
  if (!is.null(quarter)) {
    quarter_months <- list(
      `1` = c(1, 2, 3),
      `2` = c(4, 5, 6),
      `3` = c(7, 8, 9),
      `4` = c(10, 11, 12)
    )
    target_month <- quarter_months[[as.character(quarter)]][1]
  } else if (!is.null(month)) {
    target_month <- month
  }

  df <- df |>
    dplyr::mutate(
      target_date = dplyr::if_else(
        lubridate::month(.data$time) > target_month,
        lubridate::make_date(
          lubridate::year(.data$time) + years + 1,
          target_month,
          1
        ),
        lubridate::make_date(
          lubridate::year(.data$time) + years,
          target_month,
          1
        )
      )
    ) |>
    dplyr::filter(
      lubridate::year(.data$pub_date) == lubridate::year(.data$target_date) &
        lubridate::month(.data$pub_date) == lubridate::month(.data$target_date)
    ) |>
    dplyr::select(-"target_date")

  # Add the class only if it is not already present
  df <- vintages_assign_class(df)
  return(df)
}

#' Get Data Releases for a Specific Date
#'
#' Filters the input dataset to return the releases corresponding to a specific
#' time period (date).
#'
#' @param df A data frame containing data vintages. The data frame must
#' include the columns `pub_date` (publication date of the release) and
#' `time` (the corresponding time period for the data).
#' @param date A Date object specifying the time period (date) for which
#' releases should be retrieved.
#'
#' @return A data frame containing the releases for the specified date.
#' The returned data frame will include the same structure as the input,
#' filtered to only include rows matching the `date` in the `time` column.
#'
#' @details
#' This function filters the input data based on the specified `date` in the
#' `time` column. The input dataset must have the `pub_date` and `time` columns,
#' with `time` being the period to match against the given `date`. If the
#' dataset is in wide format, it will first be transformed into long format
#' using the helper function `vintages_long`.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Get releases for a specific date
#' date <- as.Date("2020-04-01")
#' releases_on_date <- get_releases_by_date(df, date)
#'
#' @family revision utilities
#' @export
get_releases_by_date <- function(df, date) {
  date <- tryCatch(as.Date(date), error = function(e) e)

  # Validate inputs
  if (!c("Date") %in% class(date)) {
    rlang::abort("The input 'date' must be a Date object.")
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }

  # Ensure data is sorted by pub_date and time
  df <- df |>
    dplyr::arrange(.data$pub_date, .data$time)

  # Get all releases on the specified date
  releases <- df |>
    dplyr::filter(.data$time == date)

  releases <- vintages_assign_class(releases)

  return(releases)
}


#' Calculate the Number of Days Between Period End and First Release
#'
#' Computes the number of days between the publication date (`pub_date`) of a
#' release and the time period  (`time`) end date for each record in
#' the dataset.
#'
#' @param df A data frame containing data vintages. The data frame must
#' include the columns `pub_date` (publication date of the release) and
#' `time` (the corresponding time period for the data).
#'
#' @return A data frame with an additional column `days_to_release`
#' representing the number of days between the publication date (`pub_date`)
#' and the time period (`time`) for each release.
#'
#' @details
#' The function calculates the difference between `pub_date` and `time`
#' for each row in the dataset. The result is expressed as the number of days
#' between the release publication date and the corresponding time period end.
#' If the dataset is in wide format, it will first be transformed into long
#' format using `vintages_long`.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Calculate days to release
#' df_with_days <- get_days_to_release(df)
#'
#' @family revision utilities
#' @export
get_days_to_release <- function(df) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }

  # Ensure data is sorted by pub_date and time
  df <- df |>
    dplyr::arrange(.data$pub_date, .data$time)

  # Get the number of days between the release date and the period end date
  df <- df |>
    dplyr::mutate(
      days_to_release = as.numeric(
        difftime(.data$pub_date, .data$time, units = "days")
      )
    )

  df <- vintages_assign_class(df)

  return(df)
}
