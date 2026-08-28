# Tests for the multi-series (panel) code paths in revisions.R, for the
# input validation branches of get_revisions(), and for the interpretation
# branches of the revision_summary print and diagnose methods.
#
# Most existing tests use single-series data, which never reaches the
# `id`-aware branches; the interpretation branches depend on the values of
# individual statistics, so they are driven here from constructed
# revision_summary objects rather than from whatever a particular sample
# happens to produce.
#' @srrstats {G5.2} Error and warning behavior tested
#' @srrstats {G5.2b} Tests demonstrate conditions triggering messages
#' @srrstats {G5.4} Correctness tests against fixed test data
#' @srrstats {G5.5} Tests run with fixed random seed

set.seed(2024)

n_obs_panel <- 40
n_vint_panel <- 4
dates_panel <- seq.Date(
  from = as.Date("2015-01-01"), by = "quarter", length.out = n_obs_panel
)

# Two series with different revision behavior: "AA" revises little, "BB" more.
make_panel_series <- function(id, noise) {
  truth <- cumsum(stats::rnorm(n_obs_panel, 0, 1))
  out <- data.frame(
    id = id,
    time = rep(dates_panel, n_vint_panel),
    pub_date = rep(
      seq.Date(
        as.Date("2015-04-01"),
        by = "quarter",
        length.out = n_vint_panel
      ),
      each = n_obs_panel
    ),
    value = as.vector(vapply(
      seq_len(n_vint_panel),
      function(v) truth + stats::rnorm(n_obs_panel, 0, noise / v),
      FUN.VALUE = numeric(n_obs_panel)
    ))
  )
  attr(out, "truth") <- truth
  out
}

panel_aa <- make_panel_series("AA", 0.4)
panel_bb <- make_panel_series("BB", 0.9)
df_panel <- dplyr::bind_rows(panel_aa, panel_bb)

# Release-indexed panel plus its final benchmark.
df_panel_release <- df_panel |>
  dplyr::group_by(.data$id, .data$time) |>
  dplyr::mutate(release = paste0("release_", 0:(dplyr::n() - 1))) |>
  dplyr::ungroup() |>
  dplyr::select("id", "time", "value", "release")

final_panel <- dplyr::bind_rows(
  data.frame(
    id = "AA", time = dates_panel,
    value = attr(panel_aa, "truth"), release = "final"
  ),
  data.frame(
    id = "BB", time = dates_panel,
    value = attr(panel_bb, "truth"), release = "final"
  )
)

# ===== get_revisions: multi-series paths =====

test_that("get_revisions handles multiple ids with interval", {
  result <- get_revisions(df_panel, interval = 1)

  expect_true(inherits(result, "tbl_pubdate"))
  expect_setequal(unique(result$id), c("AA", "BB"))
  # One revision series per id, aligned on the same time grid.
  expect_identical(
    sort(unique(result$time[result$id == "AA"])),
    sort(unique(result$time[result$id == "BB"]))
  )
})

test_that("get_revisions handles multiple ids with ref_date", {
  ref <- as.Date("2015-04-01")
  result <- get_revisions(df_panel, interval = NULL, ref_date = ref)

  expect_true(inherits(result, "tbl_pubdate"))
  expect_setequal(unique(result$id), c("AA", "BB"))
  # Revisions are measured against the reference vintage, so that vintage
  # revises to zero for every period.
  at_ref <- result[result$pub_date == ref, ]
  expect_true(all(abs(at_ref$value) < 1e-12))
})

test_that("get_revisions handles multiple ids with nth_release", {
  result <- get_revisions(df_panel, interval = NULL, nth_release = 0)

  expect_true(inherits(result, "tbl_pubdate"))
  expect_setequal(unique(result$id), c("AA", "BB"))
  expect_gt(nrow(result), 0)
})

test_that("get_revisions accepts nth_release = 'latest' for a panel", {
  result <- get_revisions(df_panel, interval = NULL, nth_release = "latest")

  expect_true(inherits(result, "tbl_pubdate"))
  expect_setequal(unique(result$id), c("AA", "BB"))
})

# ===== get_revisions: input validation =====

test_that("get_revisions rejects malformed interval arguments", {
  expect_error(
    get_revisions(df_panel, interval = c(1, 2)), "must be of length 1"
  )
  expect_error(
    get_revisions(df_panel, interval = 1.5), "must be a whole number"
  )
  expect_error(
    get_revisions(df_panel, interval = "one"), "must be an integer or numeric"
  )
})

test_that("get_revisions rejects malformed nth_release arguments", {
  expect_error(
    get_revisions(df_panel, interval = NULL, nth_release = -1),
    "non-negative integer"
  )
  expect_error(
    get_revisions(df_panel, interval = NULL, nth_release = "final"),
    "non-negative integer"
  )
})

# ===== get_nth_release on a panel =====

test_that("get_nth_release keeps ids separate", {
  first <- get_nth_release(df_panel, n = 0)

  expect_true(inherits(first, "tbl_release"))
  expect_setequal(unique(first$id), c("AA", "BB"))
  # One value per id and time for a single release.
  expect_identical(
    nrow(first),
    length(unique(first$id)) * length(unique(first$time))
  )
})

# ===== get_first_efficient_release: multi-series path =====

test_that("get_first_efficient_release returns one entry per id", {
  res <- suppressWarnings(
    get_first_efficient_release(df_panel_release, final_panel)
  )

  expect_s3_class(res, "lst_efficient")
  expect_setequal(names(res), c("AA", "BB"))

  for (id in c("AA", "BB")) {
    expect_true(all(c("e", "data", "models", "tests") %in% names(res[[id]])))
    expect_gt(length(res[[id]]$models), 0)
    expect_identical(length(res[[id]]$models), length(res[[id]]$tests))
    expect_s3_class(res[[id]]$models[[1]], "lm")
    # The per-id data is a validated vintages object.
    expect_s3_class(res[[id]]$data, "tbl_vintage")
  }
})

test_that("get_first_efficient_release honours test_all on a panel", {
  res <- suppressWarnings(
    get_first_efficient_release(df_panel_release, final_panel, test_all = TRUE)
  )

  # With test_all every release is tested, so no id stops early.
  n_releases <- length(unique(df_panel_release$release))
  for (id in c("AA", "BB")) {
    expect_identical(length(res[[id]]$tests), n_releases)
  }
})

test_that("get_first_efficient_release validates id columns and arguments", {
  no_id <- dplyr::select(df_panel_release, -"id")

  expect_error(
    get_first_efficient_release(df_panel_release, no_id),
    "Both or none"
  )
  expect_error(
    get_first_efficient_release(df_panel_release, final_panel, robust = "yes"),
    "must be a logical"
  )
  expect_error(
    get_first_efficient_release(
      df_panel_release, final_panel, test_all = "yes"
    ),
    "must be a logical"
  )

  mismatched <- final_panel
  mismatched$id[mismatched$id == "BB"] <- "CC"
  expect_error(
    get_first_efficient_release(df_panel_release, mismatched),
    "must have the same values"
  )
})

test_that("summary.lst_efficient reports every id", {
  res <- suppressWarnings(
    get_first_efficient_release(df_panel_release, final_panel)
  )

  out <- utils::capture.output(summ <- suppressWarnings(summary(res)))

  expect_true(any(grepl("id:", out, fixed = TRUE)))
  expect_true(any(grepl("AA", out, fixed = TRUE)))
  expect_true(any(grepl("BB", out, fixed = TRUE)))

  # The returned table carries one row per id with the fitted coefficients.
  expect_s3_class(summ, "tbl_df")
  expect_setequal(summ$id, c("AA", "BB"))
  expect_true(
    all(c("e", "alpha", "beta", "p_value", "n_tested") %in% names(summ))
  )
})

test_that("summary.lst_efficient handles ids with no efficient release", {
  # A series whose early releases never become efficient: pure noise against
  # an unrelated benchmark.
  set.seed(11)
  noisy <- data.frame(
    id = "ZZ",
    time = rep(dates_panel, 2),
    value = stats::rnorm(2 * n_obs_panel, 0, 5),
    release = rep(c("release_0", "release_1"), each = n_obs_panel)
  )
  noisy_final <- data.frame(
    id = "ZZ", time = dates_panel,
    value = stats::rnorm(n_obs_panel, 50, 1), release = "final"
  )

  combined <- dplyr::bind_rows(df_panel_release, noisy)
  combined_final <- dplyr::bind_rows(final_panel, noisy_final)

  res <- suppressWarnings(
    get_first_efficient_release(combined, combined_final)
  )
  out <- utils::capture.output(summ <- suppressWarnings(summary(res)))

  expect_true("ZZ" %in% summ$id)
  expect_true(any(grepl("No efficient release found", out, fixed = TRUE)))
  expect_true(is.na(summ$e[summ$id == "ZZ"]))
})

# ===== get_revision_analysis on a panel =====

test_that("get_revision_analysis groups by id", {
  res <- suppressWarnings(
    get_revision_analysis(df_panel_release, final_panel, degree = 1)
  )

  expect_s3_class(res, "revision_summary")
  expect_true("id" %in% colnames(res))
  expect_setequal(unique(res$id), c("AA", "BB"))
  expect_gt(nrow(res), 2)
})

test_that("print and diagnose report each group of a panel summary", {
  res <- suppressWarnings(
    get_revision_analysis(df_panel_release, final_panel, degree = 1)
  )

  out_print <- suppressWarnings(utils::capture.output(print(res)))
  out_diag <- suppressWarnings(utils::capture.output(diagnose(res)))

  # Group headers name the id being described.
  expect_true(any(grepl("id=AA", out_print, fixed = TRUE)))
  expect_true(any(grepl("id=BB", out_print, fixed = TRUE)))
  expect_true(any(grepl("AA", out_diag, fixed = TRUE)))
})

# ===== Interpretation branches, driven from constructed summaries =====

# A revision_summary is a data frame of statistics; the print and diagnose
# methods turn those numbers into prose. Building the object directly is the
# only reliable way to exercise every branch.
make_summary <- function(...) {
  vals <- list(...)
  base <- list(
    release = "release_0",
    N = 40,
    `Bias (mean)` = 0,
    `Bias (robust p-value)` = 0.5,
    `Noise/Signal` = 0.1,
    Correlation = 0,
    `Correlation (p-value)` = 0.5,
    `Autocorrelation (1st)` = 0,
    `Autocorrelation (1st p-value)` = 0.5,
    `Theil's U1` = 0.1,
    `News joint test (p-value)` = 0.5,
    `Noise joint test (p-value)` = 0.5,
    `Fraction of correct sign` = 0.95
  )
  base[names(vals)] <- vals
  out <- tibble::as_tibble(base)
  class(out) <- c("revision_summary", class(out))
  out
}

test_that("print.revision_summary describes bias in both directions", {
  up <- utils::capture.output(print(make_summary(
    `Bias (mean)` = 0.8, `Bias (robust p-value)` = 0.01
  )))
  down <- utils::capture.output(print(make_summary(
    `Bias (mean)` = -0.8, `Bias (robust p-value)` = 0.01
  )))
  none <- utils::capture.output(print(make_summary(
    `Bias (mean)` = 0.01, `Bias (robust p-value)` = 0.8
  )))

  expect_true(any(grepl("upward", up)))
  expect_true(any(grepl("downward", down)))
  expect_false(any(grepl("upward|downward", none)))
})

test_that("print.revision_summary grades revision volatility", {
  # Note the print method's thresholds (0.1 / 0.3) and wording differ from
  # those used by diagnose() below (0.3 / 0.5).
  low <- utils::capture.output(print(make_summary(`Noise/Signal` = 0.05)))
  mid <- utils::capture.output(print(make_summary(`Noise/Signal` = 0.2)))
  high <- utils::capture.output(print(make_summary(`Noise/Signal` = 0.9)))

  expect_true(any(grepl("Very low revision volatility", low)))
  expect_true(any(grepl("Moderate revision volatility", mid)))
  expect_true(any(grepl("High revision volatility", high)))
})

test_that("print.revision_summary reports significant correlation", {
  pos <- utils::capture.output(print(make_summary(
    Correlation = 0.7, `Correlation (p-value)` = 0.001
  )))
  neg <- utils::capture.output(print(make_summary(
    Correlation = -0.7, `Correlation (p-value)` = 0.001
  )))

  expect_true(any(grepl("positive", pos)))
  expect_true(any(grepl("negative", neg)))
})

test_that("print.revision_summary reports news and noise findings", {
  news <- utils::capture.output(print(make_summary(
    `News joint test (p-value)` = 0.001
  )))
  no_news <- utils::capture.output(print(make_summary(
    `News joint test (p-value)` = 0.9
  )))
  noise <- utils::capture.output(print(make_summary(
    `Noise joint test (p-value)` = 0.001
  )))
  no_noise <- utils::capture.output(print(make_summary(
    `Noise joint test (p-value)` = 0.9
  )))

  expect_true(any(grepl("contain NEWS", news, fixed = TRUE)))
  expect_true(any(grepl("do NOT contain news", no_news, fixed = TRUE)))
  expect_true(any(grepl("contain NOISE", noise, fixed = TRUE)))
  expect_true(any(grepl("do NOT contain noise", no_noise, fixed = TRUE)))
})

test_that("print.revision_summary flags persistent revisions", {
  out <- utils::capture.output(print(make_summary(
    `Autocorrelation (1st)` = 0.6, `Autocorrelation (1st p-value)` = 0.001
  )))

  expect_true(any(grepl("persistent", out)))
})

test_that("print.revision_summary grades forecast accuracy and sign", {
  good <- utils::capture.output(print(make_summary(
    `Theil's U1` = 0.1, `Fraction of correct sign` = 0.95
  )))
  moderate <- utils::capture.output(print(make_summary(
    `Theil's U1` = 0.45, `Fraction of correct sign` = 0.8
  )))
  poor <- utils::capture.output(print(make_summary(
    `Theil's U1` = 0.9, `Fraction of correct sign` = 0.4
  )))

  expect_true(any(grepl("Good forecast accuracy", good)))
  expect_true(any(grepl("Moderate forecast accuracy", moderate)))
  expect_true(any(grepl("Poor forecast accuracy", poor)))

  expect_true(any(grepl("Excellent sign prediction", good)))
  expect_true(any(grepl("Good sign prediction", moderate)))
  expect_true(any(grepl("Poor sign prediction", poor)))
})

test_that("print.revision_summary can suppress interpretation", {
  out <- utils::capture.output(
    print(make_summary(), interpretation = FALSE)
  )

  expect_false(any(grepl("=== Interpretation ===", out, fixed = TRUE)))
})

test_that("diagnose.revision_summary grades each metric", {
  pass <- utils::capture.output(diagnose(make_summary(
    `Bias (robust p-value)` = 0.9,
    `Noise/Signal` = 0.1,
    `News joint test (p-value)` = 0.001,
    `Noise joint test (p-value)` = 0.9,
    `Theil's U1` = 0.1,
    `Fraction of correct sign` = 0.95
  )))
  fail <- utils::capture.output(diagnose(make_summary(
    `Bias (mean)` = 1.5,
    `Bias (robust p-value)` = 0.001,
    `Noise/Signal` = 0.9,
    `News joint test (p-value)` = 0.9,
    `Noise joint test (p-value)` = 0.001,
    `Theil's U1` = 0.9,
    `Fraction of correct sign` = 0.3
  )))
  middling <- utils::capture.output(diagnose(make_summary(
    `Noise/Signal` = 0.4, `Theil's U1` = 0.45,
    `Fraction of correct sign` = 0.8
  )))

  expect_true(any(grepl("Unbiasedness", pass, fixed = TRUE)))
  expect_true(any(grepl("No significant bias", pass, fixed = TRUE)))
  expect_true(any(grepl("Significant upward bias", fail, fixed = TRUE)))
  expect_true(any(grepl("Low revision volatility", pass, fixed = TRUE)))
  expect_true(any(grepl("High revision volatility", fail, fixed = TRUE)))
  expect_true(
    any(grepl("Moderate revision volatility", middling, fixed = TRUE))
  )
})

test_that("diagnose.revision_summary respects the alpha argument", {
  # A p-value of 0.03 passes at alpha = 0.01 but fails at alpha = 0.05.
  s <- make_summary(`Bias (mean)` = 0.5, `Bias (robust p-value)` = 0.03)

  strict <- utils::capture.output(diagnose(s, alpha = 0.05))
  lenient <- utils::capture.output(diagnose(s, alpha = 0.01))

  expect_true(any(grepl("Significant upward bias", strict, fixed = TRUE)))
  expect_true(any(grepl("No significant bias", lenient, fixed = TRUE)))
})
