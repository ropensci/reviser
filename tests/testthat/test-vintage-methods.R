# Tests for the shared `tbl_vintage` class system.
#
# `tbl_pubdate` and `tbl_release` inherit their print, summary, plot and
# tbl_sum methods from the common parent class `tbl_vintage`. These tests
# check the inheritance itself and the consistency it is meant to guarantee.
#' @srrstats {TS1.0} Class system for time series vintages data is tested
#' @srrstats {TS1.2} Validation of the class contract is tested
#' @srrstats {TS5.0} Class system and its inherited methods are tested

df_us <- dplyr::filter(reviser::gdp, id == "US")

# Converting long release data to wide drops the pub_date column, which the
# package warns about. That is expected here and not what these tests check.
wide_release_us <- function(x) {
  suppressWarnings(vintages_wide(x, names_from = "release"))$US
}

# ===== Class hierarchy =====

test_that("the package's constructors produce the shared parent class", {
  long_release <- get_nth_release(df_us, n = 0:3)
  wide_pubdate <- vintages_wide(df_us)$US
  wide_release <- wide_release_us(long_release)

  expect_s3_class(long_release, "tbl_vintage")
  expect_s3_class(wide_pubdate, "tbl_vintage")
  expect_s3_class(wide_release, "tbl_vintage")

  # The parent sits behind the specific class but ahead of the tibble
  # classes, so vintages methods win over the tbl_df ones.
  expect_identical(
    class(wide_pubdate),
    c("tbl_pubdate", "tbl_vintage", "tbl_df", "tbl", "data.frame")
  )
  expect_identical(
    class(wide_release),
    c("tbl_release", "tbl_vintage", "tbl_df", "tbl", "data.frame")
  )
})

test_that("a long release table holds both classes, release first", {
  # Long release data carries a pub_date column too, so it is both kinds of
  # vintages object. `tbl_release` must win dispatch.
  long_release <- get_nth_release(df_us, n = 0:3)

  expect_s3_class(long_release, "tbl_release")
  expect_s3_class(long_release, "tbl_pubdate")
  expect_lt(
    match("tbl_release", class(long_release)),
    match("tbl_pubdate", class(long_release))
  )
  expect_true(any(grepl(
    "Release Format",
    utils::capture.output(summary(long_release)),
    fixed = TRUE
  )))
})

test_that("the shared methods are registered on the parent, not the children", {
  for (g in c("print", "summary", "plot", "tbl_sum")) {
    expect_true(
      reviser_has_method(g, "tbl_vintage"),
      info = paste0("no ", g, ".tbl_vintage method")
    )
    for (child in c("tbl_pubdate", "tbl_release")) {
      expect_false(
        reviser_has_method(g, child),
        info = paste0("unexpected ", g, ".", child, " method")
      )
    }
  }
})

test_that("the dimension-specific generics resolve for both representations", {
  for (g in c("vintage_labels", "vintage_value_cols", "vintage_detail")) {
    for (child in c("tbl_pubdate", "tbl_release")) {
      expect_true(
        reviser_has_method(g, child),
        info = paste0("no ", g, ".", child, " method")
      )
    }
    # The parent has no fallback: a new representation must be explicit.
    expect_false(reviser_has_method(g, "tbl_vintage"))
  }
})

# ===== Consistency the shared methods are meant to guarantee =====

test_that("both representations report the same header fields", {
  pubdate <- vintages_wide(df_us)$US
  release <- wide_release_us(get_nth_release(df_us, n = 0:3))

  h_pub <- pillar::tbl_sum(pubdate)
  h_rel <- pillar::tbl_sum(release)

  # Same shape, differing only in the dimension-specific labels.
  expect_true("Format" %in% names(h_pub))
  expect_true("Format" %in% names(h_rel))
  expect_true("Time periods" %in% names(h_pub))
  expect_true("Time periods" %in% names(h_rel))
  expect_true("Vintages" %in% names(h_pub))
  expect_true("Releases" %in% names(h_rel))
})

test_that("time periods count distinct dates, not rows, in the long layout", {
  # Regression test: the publication-date header used to report the row
  # count, which overstates the number of periods for long data.
  long_pubdate <- get_revisions(df_us, interval = 1)
  wide_pubdate <- vintages_wide(df_us)$US

  expect_true(inherits(long_pubdate, "tbl_pubdate"))

  header <- pillar::tbl_sum(long_pubdate)
  expect_identical(
    unname(header[["Time periods"]]),
    as.character(length(unique(long_pubdate$time)))
  )
  expect_lt(
    as.numeric(header[["Time periods"]]),
    nrow(long_pubdate)
  )

  # The wide layout still counts rows, which is correct there.
  expect_identical(
    unname(pillar::tbl_sum(wide_pubdate)[["Time periods"]]),
    as.character(nrow(wide_pubdate))
  )
})

test_that("the inherited plot method uses the class's own dimension", {
  pubdate <- vintages_wide(df_us)$US
  release <- get_nth_release(df_us, n = 0:3)

  expect_s3_class(plot(pubdate), "ggplot")
  expect_s3_class(plot(release), "ggplot")
  # Arguments still reach plot_vintages().
  expect_s3_class(plot(release, type = "point"), "ggplot")
})

test_that("summary reports layout and coverage for both layouts", {
  long_release <- get_nth_release(df_us, n = 0:3)
  wide_release <- wide_release_us(long_release)

  out_long <- utils::capture.output(summary(long_release))
  out_wide <- utils::capture.output(summary(wide_release))

  expect_true(any(grepl("Format: long", out_long)))
  expect_true(any(grepl("Format: wide", out_wide)))
  expect_true(any(grepl("Number of releases", out_long)))
  expect_true(any(grepl("Missing values", out_wide)))
})

test_that("summary returns its input invisibly", {
  release <- get_nth_release(df_us, n = 0:3)

  returned <- utils::capture.output(res <- summary(release))
  expect_identical(res, release)
})

# ===== Validation of the class contract =====

test_that("validate_vintages rejects an object missing the parent class", {
  # Assigning the class attribute by hand skips the parent, so the object
  # would silently not dispatch to the vintages methods.
  hand_made <- dplyr::as_tibble(vintages_wide(df_us)$US)
  class(hand_made) <- c("tbl_pubdate", "tbl_df", "tbl", "data.frame")

  expect_error(validate_vintages(hand_made), "tbl_vintage")
})

test_that("validate_vintages accepts objects from the constructors", {
  expect_silent(validate_vintages(get_nth_release(df_us, n = 0:3)))
  expect_silent(validate_vintages(vintages_wide(df_us)$US))
})
