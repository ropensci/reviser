# Test file for utils.R
# Tests for vintages_long, vintages_wide, vintages_check,
# and related functions

# ===== Setup Test Data =====

# Create test data in various formats
set.seed(789)

# Basic long format data
df_long <- data.frame(
  time = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12), 3),
  pub_date = rep(
    as.Date(c("2020-02-01", "2020-03-01", "2020-04-01")),
    each = 12
  ),
  value = rnorm(36, 100, 10)
)

# Basic wide format data
df_wide <- data.frame(
  time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
  `2020-02-01` = rnorm(12, 100, 10),
  `2020-03-01` = rnorm(12, 100, 10),
  `2020-04-01` = rnorm(12, 100, 10),
  check.names = FALSE
)

# Long format with release column
df_long_release <- data.frame(
  time = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12), 3),
  release = rep(c("release_0", "release_1", "release_2"), each = 12),
  value = rnorm(36, 100, 10)
)

# Wide format with release column names
df_wide_release <- data.frame(
  time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
  release_0 = rnorm(12, 100, 10),
  release_1 = rnorm(12, 100, 10),
  release_2 = rnorm(12, 100, 10)
)

# Long format with id column - ensure unique combinations
df_long_id <- data.frame(
  time = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12), 6),
  pub_date = rep(
    rep(as.Date(c("2020-02-01", "2020-03-01", "2020-04-01")), each = 12),
    2
  ),
  value = rnorm(72, 100, 10),
  id = rep(c("US", "EA"), each = 36)
)

# Wide format with id (as list)
df_wide_list <- list(
  US = data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    `2020-02-01` = rnorm(12, 100, 10),
    `2020-03-01` = rnorm(12, 100, 10),
    check.names = FALSE
  ),
  EA = data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    `2020-02-01` = rnorm(12, 100, 10),
    `2020-03-01` = rnorm(12, 100, 10),
    check.names = FALSE
  )
)

# Data with NAs
df_long_na <- df_long
df_long_na$value[1:5] <- NA

df_wide_na <- df_wide
df_wide_na[1:3, 2] <- NA

# ===== Tests for vintages_long =====

test_that("vintages_long converts wide to long format", {
  result <- vintages_long(df_wide, names_to = "pub_date")

  expect_true("time" %in% colnames(result))
  expect_true("pub_date" %in% colnames(result))
  expect_true("value" %in% colnames(result))
  expect_equal(nrow(result), 36) # 12 time periods * 3 vintages
  expect_s3_class(result$pub_date, "Date")
})

test_that("vintages_long handles release column", {
  result <- vintages_long(df_wide_release, names_to = "release")

  expect_true("release" %in% colnames(result))
  expect_true("value" %in% colnames(result))
  expect_equal(nrow(result), 36)
})

test_that("vintages_long handles list input", {
  result <- vintages_long(df_wide_list, names_to = "pub_date")

  expect_true("id" %in% colnames(result))
  expect_true(all(c("US", "EA") %in% result$id))
  expect_equal(nrow(result), 48) # 12 * 2 vintages * 2 ids
})

test_that("vintages_long keeps ids for long-format list input", {
  long_list <- list(US = df_long, EA = df_long)

  result <- suppressWarnings(vintages_long(long_list, names_to = "pub_date"))

  expect_true("id" %in% colnames(result))
  expect_equal(sort(unique(result$id)), c("EA", "US"))
  expect_equal(
    unname(as.integer(table(result$id))),
    c(nrow(df_long), nrow(df_long))
  )
})

test_that("vintages_long validates names_to parameter", {
  expect_error(
    vintages_long(df_wide, names_to = "invalid"),
    "'names_to' must be one of 'pub_date' or 'release'"
  )
})

test_that("vintages_long validates keep_na parameter", {
  expect_error(
    vintages_long(df_wide, keep_na = "yes"),
    "'keep_na' argument must be logical"
  )
})

test_that("vintages_long keeps NA values when keep_na = TRUE", {
  result_keep <- vintages_long(
    df_wide_na,
    names_to = "pub_date",
    keep_na = TRUE
  )
  result_drop <- vintages_long(
    df_wide_na,
    names_to = "pub_date",
    keep_na = FALSE
  )

  expect_gt(nrow(result_keep), nrow(result_drop))
  expect_true(any(is.na(result_keep$value)))
  expect_false(any(is.na(result_drop$value)))
})

test_that("vintages_long warns when data is already long", {
  expect_warning(
    vintages_long(df_long, names_to = "pub_date"),
    "already in long format"
  )
})

test_that("vintages_long assigns correct class", {
  result_pub <- vintages_long(df_wide, names_to = "pub_date")
  result_rel <- vintages_long(df_wide_release, names_to = "release")

  expect_s3_class(result_pub, "tbl_pubdate")
  expect_s3_class(result_rel, "tbl_release")
})

test_that("vintages_long handles case insensitivity", {
  result <- vintages_long(df_wide, names_to = "PUB_DATE")

  expect_true("pub_date" %in% colnames(result))
})

test_that("vintages_long sorts data appropriately", {
  result <- vintages_long(df_wide, names_to = "pub_date")

  # Check that data is sorted by pub_date and time
  expect_true(all(diff(as.numeric(result$pub_date)) >= 0))
})

# ===== Tests for vintages_wide =====

test_that("vintages_wide converts long to wide format", {
  result <- vintages_wide(df_long, names_from = "pub_date")

  expect_true("time" %in% colnames(result))
  expect_equal(nrow(result), 12)
  expect_equal(ncol(result), 4) # time + 3 vintages
})

test_that("vintages_wide handles release column", {
  result <- vintages_wide(df_long_release, names_from = "release")

  expect_true("release_0" %in% colnames(result))
  expect_equal(nrow(result), 12)
})

test_that("vintages_wide handles id column", {
  result <- vintages_wide(df_long_id, names_from = "pub_date")

  expect_true(is.list(result))
  expect_equal(names(result), c("EA", "US"))
  expect_equal(nrow(result$US), 12)
  expect_equal(nrow(result$EA), 12)
})

test_that("vintages_wide validates names_from parameter", {
  expect_error(
    vintages_wide(df_long, names_from = "invalid"),
    "'names_from' must be one of 'pub_date' or 'release'"
  )
})


test_that("vintages_wide warns about additional columns", {
  df_extra <- df_long
  df_extra$extra_col <- rnorm(nrow(df_extra))

  expect_warning(
    vintages_wide(df_extra, names_from = "pub_date"),
    "Ignoring columns"
  )
})

test_that("vintages_wide warns when data is already wide", {
  expect_warning(
    vintages_wide(df_wide),
    "already in wide format"
  )
})

test_that("vintages_wide assigns correct class", {
  result_pub <- vintages_wide(df_long, names_from = "pub_date")
  result_rel <- vintages_wide(df_long_release, names_from = "release")

  expect_s3_class(result_pub, "tbl_pubdate")
  expect_s3_class(result_rel, "tbl_release")
})

test_that("vintages_wide handles case insensitivity", {
  result <- vintages_wide(df_long, names_from = "PUB_DATE")

  expect_true("time" %in% colnames(result))
})

test_that("vintages_wide handles values vs value column", {
  df_values <- df_long
  colnames(df_values)[colnames(df_values) == "value"] <- "values"

  result <- vintages_wide(df_values, names_from = "pub_date")
  expect_equal(nrow(result), 12)
})

# ===== Tests for vintages_check =====

test_that("vintages_check identifies long format", {
  result <- vintages_check(df_long)
  expect_equal(result, "long")
})

test_that("vintages_check identifies wide format", {
  result <- vintages_check(df_wide)
  expect_equal(result, "wide")
})

test_that("vintages_check handles release columns", {
  result_long <- vintages_check(df_long_release)
  result_wide <- vintages_check(df_wide_release)

  expect_equal(result_long, "long")
  expect_equal(result_wide, "wide")
})

test_that("vintages_check validates data frame input", {
  expect_error(
    vintages_check("not_a_df"),
    "not a data.frame or tibble"
  )
})

test_that("vintages_check requires time column", {
  df_no_time <- df_long[, -1]

  expect_error(
    vintages_check(df_no_time),
    "'time' column is missing"
  )
})


test_that("vintages_check validates pub_date format in long data", {
  df_bad_pubdate <- df_long
  df_bad_pubdate$pub_date <- as.character(df_bad_pubdate$pub_date)
  df_bad_pubdate$pub_date[1] <- "bad_date"

  expect_error(
    vintages_check(df_bad_pubdate),
    "not in '%Y-%m-%d' format"
  )
})

test_that("vintages_check validates wide format column names", {
  df_bad_cols <- df_wide
  colnames(df_bad_cols)[2] <- "invalid_col"

  expect_error(
    vintages_check(df_bad_cols),
    "not labeled correctly"
  )
})

test_that("vintages_check handles list of data frames", {
  result <- vintages_check(df_wide_list)

  expect_true(is.list(result))
  expect_equal(names(result), c("US", "EA"))
  expect_true(all(unlist(result) == "wide"))
})

test_that("vintages_check requires named list", {
  unnamed_list <- list(df_wide, df_wide)

  expect_error(
    vintages_check(unnamed_list),
    "must be named"
  )
})

test_that("vintages_check rejects list columns", {
  df_list_col <- df_long
  df_list_col$list_col <- as.list(seq_len(nrow(df_list_col)))

  expect_error(
    vintages_check(df_list_col),
    "list column"
  )
})

test_that("vintages_check detects implicit missing dates", {
  # Create wide format data with gaps in time column
  df_gaps_wide <- data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    `2020-02-01` = rnorm(12, 100, 10),
    check.names = FALSE
  )
  # Remove some time rows to create gaps
  df_gaps_wide <- df_gaps_wide[
    !df_gaps_wide$time %in% as.Date(c("2020-03-01", "2020-04-01")),
  ]

  expect_warning(
    vintages_check(df_gaps_wide),
    "implicit missing values"
  )
})

test_that("vintages_check rejects non-scalar values", {
  df_bad <- df_long
  df_bad$value <- as.list(df_bad$value)

  expect_error(
    vintages_check(df_bad),
    "list column"
  )
})

# ===== Tests for print methods =====

test_that("print.tbl_pubdate produces output", {
  df_pub <- vintages_wide(df_long, names_from = "pub_date")
  output <- utils::capture.output(print(df_pub))

  expect_gt(length(output), 0)
  expect_true(any(grepl("publication date format", output)))
  expect_true(any(grepl("Time periods", output)))
  expect_true(any(grepl("Vintages", output)))
})

test_that("print.tbl_pubdate returns invisibly", {
  df_pub <- vintages_wide(df_long, names_from = "pub_date")
  result <- print(df_pub)

  # Print returns the object invisibly, but removes the custom class
  expect_true("time" %in% colnames(result))
  expect_equal(nrow(result), nrow(df_pub))
})

test_that("print.tbl_release produces output", {
  df_rel <- vintages_wide(df_long_release, names_from = "release")
  output <- utils::capture.output(print(df_rel))

  expect_gt(length(output), 0)
  expect_true(any(grepl("release format", output)))
})

test_that("print.tbl_release handles long format", {
  df_rel_long <- dplyr::as_tibble(df_long_release)
  class(df_rel_long) <- c("tbl_release", class(df_rel_long))

  output <- utils::capture.output(print(df_rel_long))

  expect_true(any(grepl("long", output)))
})

test_that("print.tbl_release handles wide format", {
  df_rel <- vintages_wide(
    dplyr::as_tibble(df_long_release),
    names_from = "release"
  )
  output <- utils::capture.output(print(df_rel))

  expect_true(any(grepl("wide", output)))
})

# ===== Tests for summary methods =====

test_that("summary.tbl_pubdate produces detailed output", {
  df_pub <- vintages_wide(df_long, names_from = "pub_date")
  output <- utils::capture.output(summary(df_pub))

  expect_gt(length(output), 0)
  expect_true(any(grepl("Vintages Data Summary", output)))
  expect_true(any(grepl("Time range", output)))
  expect_true(any(grepl("Publication dates", output)))
  expect_true(any(grepl("Missing values", output)))
})

test_that("summary.tbl_pubdate returns invisibly", {
  df_pub <- vintages_wide(df_long, names_from = "pub_date")
  result <- summary(df_pub)

  expect_identical(result, df_pub)
})

test_that("summary.tbl_pubdate handles long format", {
  df_pub_long <- vintages_assign_class(dplyr::as_tibble(df_long))

  output <- utils::capture.output(summary(df_pub_long))

  expect_true(any(grepl("Format: long", output)))
  expect_true(any(grepl("Number of vintages: 3", output)))
  # Time periods must count distinct dates, not rows.
  expect_true(any(grepl("Time periods: 12", output)))
  expect_true(any(grepl("Earliest: 2020-02-01", output)))
  expect_true(any(grepl("Latest: 2020-04-01", output)))
})

test_that("summary.tbl_pubdate works on get_revisions() output", {
  # Regression test: get_revisions() always returns a long tbl_pubdate, which
  # previously made summary() fail in as.Date() on the column names.
  df <- dplyr::filter(reviser::gdp, id == "US")

  for (revisions in list(
    get_revisions(df, interval = 1),
    get_revisions(df, nth_release = 1),
    get_revisions(df, ref_date = as.Date("2010-01-01"))
  )) {
    expect_s3_class(revisions, "tbl_pubdate")
    expect_no_error(utils::capture.output(summary(revisions)))
  }
})

test_that("every generic works on every exported vintages constructor", {
  # Guards against method/class combinations that no example happens to cover.
  df <- dplyr::filter(reviser::gdp, id == "US")

  objects <- list(
    wide = vintages_wide(df)$US,
    first = get_first_release(df),
    latest = get_latest_release(df),
    nth = get_nth_release(df, n = 0:3),
    revisions = get_revisions(df, interval = 1)
  )

  for (nm in names(objects)) {
    object <- objects[[nm]]
    expect_no_error(utils::capture.output(print(object)))
    expect_no_error(utils::capture.output(summary(object)))
    expect_s3_class(plot(object), "ggplot")
  }
})

test_that("summary.tbl_release handles long format", {
  df_rel_long <- df_long_release
  class(df_rel_long) <- c("tbl_release", class(df_rel_long))

  output <- utils::capture.output(summary(df_rel_long))

  expect_true(any(grepl("Format: long", output)))
  expect_true(any(grepl("Number of releases", output)))
})

test_that("summary.tbl_release handles wide format", {
  df_rel <- vintages_wide(df_long_release, names_from = "release")
  output <- utils::capture.output(summary(df_rel))

  expect_true(any(grepl("Format: wide", output)))
})

test_that("summary.tbl_pubdate calculates missing values correctly", {
  # Create long data with NAs, convert to wide, then assign class
  long_with_na <- vintages_long(
    df_wide_na,
    names_to = "pub_date",
    keep_na = TRUE
  )
  df_pub <- vintages_wide(long_with_na, names_from = "pub_date")

  output <- utils::capture.output(summary(df_pub))
  expect_true(any(grepl("Missing values", output)))
})

# ===== Tests for internal helper functions =====

test_that("invert_hessian matches solve() on positive definite input", {
  set.seed(101)
  a <- matrix(rnorm(25), 5, 5)
  h <- crossprod(a) + diag(5) # symmetric positive definite

  res <- invert_hessian(h)

  expect_null(res$warning)
  expect_equal(res$cov, solve(h), tolerance = 1e-10)
  # Result must be exactly symmetric, not merely close.
  expect_identical(res$cov, t(res$cov))
  expect_equal(res$cov %*% h, diag(5), tolerance = 1e-10)
})

test_that("invert_hessian reports a non positive definite Hessian", {
  # Symmetric and invertible, but indefinite: a saddle point rather than a
  # maximum. The inverse is still returned, with a diagnostic attached.
  h <- diag(c(2, -3, 4))

  res <- invert_hessian(h)

  expect_false(is.null(res$warning))
  expect_match(res$warning, "not positive definite")
  expect_equal(res$cov, solve(h), tolerance = 1e-10)
})

test_that("invert_hessian reports failure on a singular Hessian", {
  h <- matrix(0, 3, 3)

  res <- invert_hessian(h)

  expect_null(res$cov)
  expect_match(res$warning, "Failed to invert Hessian")
})

test_that("delta-method scaling equals the explicit Jacobian form", {
  # The Jacobian is diagonal, so the covariance transform is an outer-product
  # scaling. This checks the optimised form against the explicit matrix
  # version, including the single-parameter case where diag() on a scalar
  # would silently build a wrongly sized identity matrix.
  set.seed(202)

  for (n in c(1L, 2L, 5L)) {
    a <- matrix(rnorm(n * n), n, n)
    cov_raw <- crossprod(a) + diag(n)
    theta <- rnorm(n)

    jac_diag <- exp(theta)
    optimised <- outer(jac_diag, jac_diag) * cov_raw

    jac <- diag(jac_diag, nrow = n)
    explicit <- jac %*% cov_raw %*% t(jac)

    expect_equal(optimised, explicit, tolerance = 1e-12)
  }
})

test_that("tcrossprod forms match the explicit products they replace", {
  set.seed(303)
  tmat <- matrix(rnorm(9), 3, 3)
  p <- crossprod(matrix(rnorm(9), 3, 3))
  k <- matrix(rnorm(6), 3, 2)
  f <- crossprod(matrix(rnorm(4), 2, 2))

  expect_equal(tcrossprod(tmat %*% p, tmat), tmat %*% p %*% t(tmat))
  expect_equal(tcrossprod(k %*% f, k), k %*% f %*% t(k))
})

test_that("standardize_val_col renames values to value", {
  df_values <- df_long
  colnames(df_values)[colnames(df_values) == "value"] <- "values"

  result <- standardize_val_col(df_values)
  expect_true("value" %in% colnames(result))
  expect_false("values" %in% colnames(result))
})

test_that("vintages_assign_class assigns correct classes", {
  df_with_pub <- df_long
  result <- vintages_assign_class(df_with_pub)
  expect_s3_class(result, "tbl_pubdate")

  df_with_rel <- df_long_release
  result <- vintages_assign_class(df_with_rel)
  expect_s3_class(result, "tbl_release")
})

# ===== Integration Tests =====

test_that("round-trip conversion preserves data (pub_date)", {
  # Start with long format
  wide <- vintages_wide(df_long, names_from = "pub_date")
  long_again <- vintages_long(wide, names_to = "pub_date", keep_na = TRUE)

  # Sort both for comparison
  df_long_sorted <- df_long[order(df_long$time, df_long$pub_date), ]
  long_again_sorted <- long_again[order(long_again$time, long_again$pub_date), ]

  expect_equal(nrow(df_long_sorted), nrow(long_again_sorted))
})

test_that("round-trip conversion preserves data (release)", {
  wide <- vintages_wide(df_long_release, names_from = "release")
  long_again <- vintages_long(wide, names_to = "release", keep_na = TRUE)

  expect_equal(nrow(df_long_release), nrow(long_again))
})

test_that("conversion handles id column correctly", {
  # Convert to wide (should create list)
  wide_list <- vintages_wide(df_long_id, names_from = "pub_date")

  expect_true(is.list(wide_list))
  expect_equal(length(wide_list), 2)

  # Convert back to long
  long_again <- vintages_long(wide_list, names_to = "pub_date")

  expect_true("id" %in% colnames(long_again))
  expect_equal(sort(unique(long_again$id)), c("EA", "US"))
})

test_that("conversion handles NAs consistently", {
  wide <- vintages_wide(df_long_na, names_from = "pub_date")
  long_keep <- vintages_long(wide, names_to = "pub_date", keep_na = TRUE)
  long_drop <- vintages_long(wide, names_to = "pub_date", keep_na = FALSE)

  expect_gt(sum(is.na(long_keep$value)), 0)
  expect_equal(sum(is.na(long_drop$value)), 0)
})

# ===== Edge Cases =====

test_that("functions handle minimal data", {
  df_minimal <- data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 3),
    pub_date = as.Date("2020-02-01"),
    value = c(1, 2, 3)
  )

  wide <- vintages_wide(df_minimal)
  expect_equal(nrow(wide), 3)
  expect_equal(ncol(wide), 2) # time + 1 vintage

  long_again <- vintages_long(wide, names_to = "pub_date")
  expect_equal(nrow(long_again), 3)
})

test_that("functions handle minimal data (2 observations)", {
  df_minimal <- data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 2),
    pub_date = as.Date("2020-02-01"),
    value = c(100, 101)
  )

  wide <- vintages_wide(df_minimal)
  expect_equal(nrow(wide), 2)
  expect_equal(ncol(wide), 2) # time + 1 vintage

  format <- vintages_check(wide)
  expect_equal(format, "wide")
})

test_that("functions handle many vintages", {
  n_vintages <- 50
  df_many <- data.frame(
    time = rep(
      seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
      n_vintages
    ),
    pub_date = rep(
      seq.Date(as.Date("2020-02-01"), by = "month", length.out = n_vintages),
      each = 12
    ),
    value = rnorm(12 * n_vintages, 100, 10)
  )

  wide <- vintages_wide(df_many, names_from = "pub_date")
  expect_equal(ncol(wide), n_vintages + 1) # time + vintages

  long_again <- vintages_long(wide, names_to = "pub_date")
  expect_equal(nrow(long_again), 12 * n_vintages)
})

test_that("functions handle extreme values", {
  df_extreme <- df_long
  df_extreme$value[1] <- 1e10
  df_extreme$value[2] <- -1e10

  wide <- vintages_wide(df_extreme)
  expect_true(all(is.finite(as.matrix(wide[, -1])), na.rm = TRUE))
})

test_that("functions handle all NA values in a vintage", {
  df_all_na <- df_long
  df_all_na$value[df_all_na$pub_date == min(df_all_na$pub_date)] <- NA

  wide <- vintages_wide(df_all_na, names_from = "pub_date")
  expect_true(all(is.na(wide[, 2])))
})

test_that("vintages_check handles quarterly data", {
  df_quarterly <- data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = 12),
    pub_date = as.Date("2020-02-01"),
    value = rnorm(12)
  )

  format <- vintages_check(df_quarterly)
  expect_equal(format, "long")
})

test_that("vintages_check handles annual data", {
  df_annual <- data.frame(
    time = seq.Date(as.Date("2020-01-01"), by = "year", length.out = 10),
    pub_date = as.Date("2020-02-01"),
    value = rnorm(10)
  )

  format <- vintages_check(df_annual)
  expect_equal(format, "long")
})

test_that("conversion preserves date classes", {
  wide <- vintages_wide(df_long, names_from = "pub_date")
  long_again <- vintages_long(wide, names_to = "pub_date")

  expect_s3_class(long_again$time, "Date")
  expect_s3_class(long_again$pub_date, "Date")
})

test_that("conversion handles tibbles", {
  df_tibble <- tibble::as_tibble(df_long)

  wide <- vintages_wide(df_tibble)
  expect_s3_class(wide, "tbl_df")

  long_again <- vintages_long(wide, names_to = "pub_date")
  expect_s3_class(long_again, "tbl_df")
})
