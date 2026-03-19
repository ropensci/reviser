# Test file for revisions.R
# Tests for get_revisions, get_first_efficient_release,
# get_revision_analysis, and related functions

# ===== Setup Test Data =====

set.seed(789)
n_obs <- 50
n_vintages <- 5

# Generate synthetic vintage data
true_values <- cumsum(rnorm(n_obs, 0, 1))
dates <- seq.Date(
  from = as.Date("2020-01-01"),
  by = "month",
  length.out = n_obs
)

expect_revision_tbl <- function(result) {
  expect_true(
    inherits(result, "tbl_pubdate") || inherits(result, "tbl_revision")
  )
}

expect_release_tbl <- function(result) {
  expect_true(
    inherits(result, "tbl_pubdate") || inherits(result, "tbl_release")
  )
}

add_test_pub_dates <- function(df) {
  df |>
    dplyr::group_by(time) |>
    dplyr::mutate(
      pub_date = seq.Date(
        as.Date("2020-02-01"),
        by = "month",
        length.out = dplyr::n()
      )[dplyr::row_number()]
    ) |>
    dplyr::ungroup()
}

# Create pub_date based long format (for get_revisions and most tests)
df_long_rev <- data.frame(
  time = rep(dates, n_vintages),
  pub_date = rep(
    seq.Date(as.Date("2020-02-01"), by = "month", length.out = n_vintages),
    each = n_obs
  ),
  value = as.vector(vapply(
    seq_len(n_vintages),
    function(v) {
      true_values + rnorm(n_obs, 0, 0.5 / v)
    },
    FUN.VALUE = numeric(n_obs)
  ))
)

# Create release based format (for get_first_efficient_release tests)
df_release <- df_long_rev |>
  dplyr::group_by(time) |>
  dplyr::mutate(release = paste0("release_", 0:(dplyr::n() - 1))) |>
  dplyr::ungroup() |>
  dplyr::select(time, value, release)

# Final release (most accurate)
final_release <- data.frame(
  time = dates,
  value = true_values,
  release = "final"
)

# Small datasets for faster tests (need at least 24 months for Friedman test)
df_small_rev <- df_release |>
  dplyr::filter(release %in% c("release_0", "release_1", "release_2")) |>
  dplyr::slice(1:90)  # 30 obs × 3 releases

final_small <- final_release[1:30, ]

# ===== Tests for get_revisions =====

test_that("get_revisions returns correct structure with interval", {
  result <- get_revisions(df_long_rev, interval = 1)

  expect_revision_tbl(result)
  expect_true("time" %in% colnames(result))
  expect_true("value" %in% colnames(result))
  expect_true("pub_date" %in% colnames(result))
})

test_that("get_revisions default interval is 1", {
  result1 <- get_revisions(df_long_rev)
  result2 <- get_revisions(df_long_rev, interval = 1)

  expect_equal(nrow(result1), nrow(result2))
})

test_that("get_revisions handles interval parameter", {
  result <- get_revisions(df_long_rev, interval = 2)

  expect_revision_tbl(result)
  expect_true(all(is.na(result$value) | is.finite(result$value)))
})

test_that("get_revisions handles ref_date parameter", {
  ref_date <- as.Date("2020-02-01")
  result <- get_revisions(df_long_rev, ref_date = ref_date)

  expect_revision_tbl(result)
  expect_true(nrow(result) > 0)
})

test_that("get_revisions handles nth_release parameter", {
  # Add pub_date to df_release for this test
  df_with_pub <- add_test_pub_dates(df_release)

  result <- get_revisions(df_with_pub, nth_release = 1)

  expect_revision_tbl(result)
  expect_true(nrow(result) > 0)
})

test_that("get_revisions handles nth_release = 'latest'", {
  # Add pub_date to df_release for this test
  df_with_pub <- add_test_pub_dates(df_release)

  result <- get_revisions(df_with_pub, nth_release = "latest")

  expect_revision_tbl(result)
})

test_that("get_revisions validates exclusive parameters", {
  expect_error(
    get_revisions(df_long_rev, interval = 1, ref_date = as.Date("2020-01-01")),
    "Specify only one"
  )
})

test_that("get_revisions validates interval type", {
  expect_error(
    get_revisions(df_long_rev, interval = "not_numeric"),
    "'interval' must be an integer"
  )

  expect_error(
    get_revisions(df_long_rev, interval = 1.5),
    "'interval' must be a whole number"
  )
})

test_that("get_revisions case insensitivity for nth_release", {
  # Add pub_date to df_release for this test
  df_with_pub <- add_test_pub_dates(df_release)

  result1 <- get_revisions(df_with_pub, nth_release = "latest")
  result2 <- get_revisions(df_with_pub, nth_release = "LATEST")

  expect_equal(nrow(result1), nrow(result2))
})

test_that("get_revisions handles wide format data", {
  df_wide <- vintages_wide(df_long_rev, names_from = "pub_date")
  result <- get_revisions(df_wide, interval = 1)

  expect_revision_tbl(result)
})

# ===== Tests for get_first_efficient_release =====

test_that("get_first_efficient_release returns correct structure", {
  result <- get_first_efficient_release(
    df_small_rev,
    final_small,
    significance = 0.05
  )

  expect_s3_class(result, "lst_efficient")
  expect_true("e" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("models" %in% names(result))
  expect_true("tests" %in% names(result))
})

test_that("get_first_efficient_release validates robust parameter", {
  expect_error(
    get_first_efficient_release(df_small_rev, final_small, robust = "yes"),
    "'robust' argument must be a logical"
  )
})

test_that("get_first_efficient_release with robust = FALSE", {
  result <- get_first_efficient_release(
    df_small_rev,
    final_small,
    robust = FALSE
  )

  expect_s3_class(result, "lst_efficient")
})

test_that("get_first_efficient_release with test_all = TRUE", {
  result <- get_first_efficient_release(
    df_small_rev,
    final_small,
    test_all = TRUE
  )

  expect_s3_class(result, "lst_efficient")
  # With test_all, all releases are tested
  expect_gt(length(result$models), 0)
})

test_that("get_first_efficient_release e value is valid", {
  result <- get_first_efficient_release(df_small_rev, final_small)

  expect_true(is.numeric(result$e))

  if (!is.na(result$e)) {
    expect_true(result$e >= 0)
  }
})

# ===== Tests for print/summary methods =====

test_that("print.lst_efficient produces output", {
  result <- get_first_efficient_release(df_small_rev, final_small)

  output <- utils::capture.output(print(result))
  expect_gt(length(output), 0)
})

test_that("summary.lst_efficient produces output", {
  result <- get_first_efficient_release(df_small_rev, final_small)

  output <- utils::capture.output(summary(result))
  expect_gt(length(output), 0)
})

test_that("summary.lst_efficient returns data frame", {
  result <- get_first_efficient_release(df_small_rev, final_small)

  df_out <- summary(result)
  expect_s3_class(df_out, "data.frame")
  expect_true("e" %in% colnames(df_out))
})

# ===== Tests for get_revision_analysis =====

test_that("get_revision_analysis returns correct structure", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 1)

  expect_s3_class(result, "revision_summary")
  expect_true("N" %in% colnames(result))
})

test_that("get_revision_analysis degree = 1 returns descriptive stats", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 1)

  expect_true("Bias (mean)" %in% colnames(result))
  expect_true("Std. Dev." %in% colnames(result))
  expect_true("MAR" %in% colnames(result))
})

test_that("get_revision_analysis degree = 2 returns correlation stats", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 2)

  expect_true("Correlation" %in% colnames(result))
  expect_true("Autocorrelation (1st)" %in% colnames(result))
})

test_that("get_revision_analysis degree = 3 returns news/noise tests", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 3)

  expect_true("News joint test (p-value)" %in% colnames(result))
  expect_true("Noise joint test (p-value)" %in% colnames(result))
})

test_that("get_revision_analysis degree = 5 returns all stats", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 5)

  # Should have many columns
  expect_gt(ncol(result), 30)
  expect_true("Frequency" %in% colnames(result))
})

test_that("get_revision_analysis validates degree parameter", {
  expect_error(
    get_revision_analysis(df_small_rev, final_small, degree = 6),
    "'degree' must be an integer between 1 and 5"
  )
})

test_that("print.revision_summary produces output", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 1)

  output <- utils::capture.output(print(result))
  expect_gt(length(output), 0)
})

test_that("diagnose.revision_summary produces output", {
  result <- get_revision_analysis(df_small_rev, final_small, degree = 5)

  # Suppress warnings about print.tbl formatting
  output <- suppressWarnings(utils::capture.output(diagnose(result)))
  expect_gt(length(output), 0)
})

# ===== Tests for get_nth_release =====

test_that("get_nth_release returns first release (n=0)", {
  result <- get_nth_release(df_long_rev, n = 0)

  expect_release_tbl(result)
  expect_true("release" %in% colnames(result))
})

test_that("get_nth_release returns specific release", {
  result <- get_nth_release(df_long_rev, n = 1)

  expect_release_tbl(result)
  expect_true(all(grepl("release_1", result$release)))
})

test_that("get_nth_release handles 'latest'", {
  result <- get_nth_release(df_long_rev, n = "latest")

  expect_release_tbl(result)
})

test_that("get_nth_release handles 'first'", {
  result <- get_nth_release(df_long_rev, n = "first")

  expect_release_tbl(result)
})

test_that("get_nth_release validates n parameter", {
  expect_error(
    get_nth_release(df_long_rev, n = -1),
    "must be a whole number >= 0"
  )
})

test_that("get_nth_release case insensitivity", {
  result1 <- get_nth_release(df_long_rev, n = "latest")
  result2 <- get_nth_release(df_long_rev, n = "LATEST")

  expect_equal(nrow(result1), nrow(result2))
})

test_that("get_nth_release diagonal filters historical rows", {
  df_diag <- tibble::tibble(
    time = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01", "2020-03-01"
    )),
    pub_date = as.Date(c(
      "2020-02-01", "2020-02-01", "2020-03-01", "2020-03-01", "2020-03-01"
    )),
    value = c(1, 2, 1.1, 2.1, 3.1)
  )

  result_all <- get_nth_release(df_diag, n = 0, diagonal = FALSE)
  result_diag <- get_nth_release(df_diag, n = 0, diagonal = TRUE)

  expect_lt(nrow(result_diag), nrow(result_all))
  expect_true(all(result_diag$time >= as.Date("2020-02-01")))
})

test_that("get_nth_release diagonal works for multiple IDs", {
  df_a <- tibble::tibble(
    id = "A",
    time = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01", "2020-03-01"
    )),
    pub_date = as.Date(c(
      "2020-02-01", "2020-02-01", "2020-03-01", "2020-03-01", "2020-03-01"
    )),
    value = c(1, 2, 1.1, 2.1, 3.1)
  )
  df_b <- tibble::tibble(
    id = "B",
    time = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-02-01", "2020-03-01"
    )),
    pub_date = as.Date(c(
      "2020-02-01", "2020-03-01", "2020-03-01", "2020-03-01"
    )),
    value = c(5, 5.1, 6.1, 7.1)
  )
  df_multi <- dplyr::bind_rows(df_a, df_b)

  result <- get_nth_release(df_multi, n = "first", diagonal = TRUE)
  counts <- result |>
    dplyr::count(id) |>
    dplyr::arrange(id)

  expect_equal(counts$n[counts$id == "A"], 2)
  expect_equal(counts$n[counts$id == "B"], 3)
})

# ===== Tests for get_first_release =====

test_that("get_first_release returns first release", {
  result <- get_first_release(df_long_rev)

  expect_release_tbl(result)
  expect_true(all(result$release == "release_0"))
})

test_that("get_first_release diagonal keeps the real first-release diagonal", {
  df_single_wide <- data.frame(
    time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    `2020-03-01` = c(1, 2, 3),
    check.names = FALSE
  )

  result <- get_first_release(vintages_long(df_single_wide), diagonal = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$time, as.Date("2020-03-01"))
  expect_equal(result$pub_date, as.Date("2020-03-01"))
  expect_true(all(result$release == "release_0"))
})

# ===== Tests for get_latest_release =====

test_that("get_latest_release returns latest release", {
  result <- get_latest_release(df_long_rev)

  expect_release_tbl(result)
  expect_true("release" %in% colnames(result))
  expect_true(all(result$release == "release_4"))
})

# ===== Tests for get_fixed_release =====

test_that("get_fixed_release handles month as name", {
  result <- get_fixed_release(df_long_rev, month = "July", years = 1)

  expect_release_tbl(result)
})

test_that("get_fixed_release handles month as number", {
  result <- get_fixed_release(df_long_rev, month = 7, years = 1)

  expect_release_tbl(result)
})

test_that("get_fixed_release handles quarter", {
  result <- get_fixed_release(df_long_rev, quarter = 3, years = 1)

  expect_release_tbl(result)
})

test_that("get_fixed_release validates exclusive parameters", {
  expect_error(
    get_fixed_release(df_long_rev, month = 7, quarter = 3, years = 1),
    "Specify either a month or a quarter"
  )
})

test_that("get_fixed_release requires month or quarter", {
  expect_error(
    get_fixed_release(df_long_rev, years = 1),
    "Specify one of 'month' or 'quarter'"
  )
})

test_that("get_fixed_release validates years parameter", {
  expect_error(
    get_fixed_release(df_long_rev, month = 7, years = 1.5),
    "whole number"
  )
})

test_that("get_fixed_release handles years = 0", {
  result <- get_fixed_release(df_long_rev, month = 7, years = 0)

  expect_release_tbl(result)
})

# ===== Tests for get_releases_by_date =====

test_that("get_releases_by_date returns releases for specific date", {
  target_date <- dates[5]
  result <- get_releases_by_date(df_long_rev, target_date)

  expect_release_tbl(result)
  expect_true(all(result$time == target_date))
})

test_that("get_releases_by_date validates date parameter", {
  expect_error(
    get_releases_by_date(df_long_rev, "not_a_date"),
    "must be a Date object"
  )
})

test_that("get_releases_by_date handles non-existent date", {
  non_existent <- as.Date("1900-01-01")
  result <- get_releases_by_date(df_long_rev, non_existent)

  expect_equal(nrow(result), 0)
})

# ===== Tests for get_days_to_release =====

test_that("get_days_to_release adds days_to_release column", {
  result <- get_days_to_release(df_long_rev)

  expect_true("days_to_release" %in% colnames(result))
  expect_true(all(is.numeric(result$days_to_release)))
})

test_that("get_days_to_release calculates days correctly", {
  result <- get_days_to_release(df_long_rev)

  expect_true(all(is.finite(result$days_to_release)))
})

# ===== Integration Tests =====

test_that("full revision workflow works", {
  # Add pub_date to df_small_rev for get_revisions
  df_with_pub <- add_test_pub_dates(df_small_rev)

  # Get revisions
  revisions <- get_revisions(df_with_pub, nth_release = 0)

  # Find efficient release
  efficient <- get_first_efficient_release(df_small_rev, final_small)

  # Analyze revisions
  analysis <- get_revision_analysis(df_small_rev, final_small, degree = 5)

  # All should complete successfully
  expect_revision_tbl(revisions)
  expect_s3_class(efficient, "lst_efficient")
  expect_s3_class(analysis, "revision_summary")
})

test_that("nth_release functions are consistent", {
  result_0 <- get_nth_release(df_long_rev, n = 0)
  result_first <- get_first_release(df_long_rev)

  # Both should have same number of rows
  expect_equal(nrow(result_0), nrow(result_first))
})

test_that("revision analysis handles different data frequencies", {
  # Quarterly data
  dates_q <- seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = 20)
  df_q <- data.frame(
    time = dates_q,
    value = rnorm(20),
    release = "release_0"
  )
  final_q <- data.frame(
    time = dates_q,
    value = rnorm(20),
    release = "final"
  )

  result <- get_revision_analysis(df_q, final_q, degree = 5)

  expect_s3_class(result, "revision_summary")
  expect_true("Frequency" %in% colnames(result))
})

# ===== Edge Cases =====

test_that("revision functions handle minimal time periods", {
  # Need at least 2 time periods for frequency detection
  df_two_periods <- df_long_rev[df_long_rev$time %in% dates[1:2], ]

  result <- get_nth_release(df_two_periods, n = 0)

  expect_release_tbl(result)
  expect_equal(nrow(result), 2)
})

test_that("get_revision_analysis handles all zero revisions", {
  df_zero <- df_small_rev
  df_zero$value <- 100
  final_zero <- final_small
  final_zero$value <- 100

  # Should not error even though revisions are all zero
  # Suppress expected warnings about zero standard deviation
  suppressWarnings({
    result <- get_revision_analysis(df_zero, final_zero, degree = 1)
  })

  expect_s3_class(result, "revision_summary")
})

test_that("get_first_efficient_release handles no efficient release", {
  # Create data where no release is efficient
  df_bad <- df_small_rev
  df_bad$value <- df_bad$value + rnorm(nrow(df_bad), 10, 5)

  expect_warning(
    result <- get_first_efficient_release(df_bad, final_small),
    "No efficient release found"
  )

  expect_true(is.na(result$e))
})

test_that("summary.lst_efficient handles NA efficient release", {
  df_bad <- df_small_rev
  df_bad$value <- df_bad$value + rnorm(nrow(df_bad), 10, 5)

  suppressWarnings({
    result <- get_first_efficient_release(df_bad, final_small)
  })

  df_out <- summary(result)

  expect_s3_class(df_out, "data.frame")
  expect_true(is.na(df_out$e))
})
