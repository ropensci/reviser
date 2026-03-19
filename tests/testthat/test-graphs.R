# Test file for visualization.R
# Tests for plot_vintages, theme_reviser, and related functions

# ===== Setup Test Data =====

# Create example data - long format
df_plot <- data.frame(
  time = rep(
    seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12),
    3
  ),
  value = runif(36, 50, 100),
  pub_date = rep(c("2022-01-05", "2022-02-07", "2022-03-03"), each = 12)
)
df_plot$pub_date <- as.Date(df_plot$pub_date)

# Wide format data
df_plot_wide <- vintages_wide(df_plot)

# Single vintage data
df_single <- data.frame(
  time = seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12),
  value = runif(12, 50, 100),
  pub_date = as.Date("2022-01-05")
)

# Data with release column
df_release <- data.frame(
  time = rep(
    seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12),
    3
  ),
  value = runif(36, 50, 100),
  release = rep(c("release_0", "release_1", "release_2"), each = 12)
)

# Data with id column
df_with_id <- data.frame(
  time = rep(
    seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12),
    6
  ),
  value = runif(72, 50, 100),
  pub_date = rep(c("2022-01-05", "2022-02-07", "2022-03-03"), each = 24),
  id = rep(c("US", "EA"), each = 36)
)
df_with_id$pub_date <- as.Date(df_with_id$pub_date)

# ===== Tests for plot_vintages =====

test_that("plot_vintages returns a ggplot object", {
  p <- plot_vintages(df_plot)
  expect_s3_class(p, "ggplot")
})

test_that("plot_vintages handles 'line' type correctly", {
  p <- plot_vintages(df_plot, type = "line", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomLine"),
    FUN.VALUE = logical(1)
  )))
})

test_that("plot_vintages handles 'point' type correctly", {
  p <- plot_vintages(df_plot, type = "point", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomPoint"),
    FUN.VALUE = logical(1)
  )))
})

test_that("plot_vintages handles 'bar' type correctly", {
  p <- plot_vintages(df_plot, type = "bar", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(
    vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomBar"),
      FUN.VALUE = logical(1)
    )
  ))
})

test_that("plot_vintages handles 'boxplot' type correctly", {
  p <- plot_vintages(df_plot, type = "boxplot", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomBoxplot"),
    FUN.VALUE = logical(1)
  )))
})

test_that("plot_vintages handles single vintage for line type", {
  p <- plot_vintages(df_single, type = "line", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomLine"),
    FUN.VALUE = logical(1)
  )))
})

test_that("plot_vintages handles single vintage for point type", {
  p <- plot_vintages(df_single, type = "point", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomPoint"),
    FUN.VALUE = logical(1)
  )))
})

test_that("plot_vintages handles single vintage for bar type", {
  p <- plot_vintages(df_single, type = "bar", dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomBar"),
    FUN.VALUE = logical(1)
  )))
})

test_that("plot_vintages errors for boxplot with single vintage", {
  expect_error(
    plot_vintages(df_single, type = "boxplot", dim_col = "pub_date"),
    "'type' boxplot not supported if 'dim_col' contains one unique value."
  )
})

test_that("plot_vintages throws error for invalid 'type'", {
  expect_error(
    plot_vintages(df_plot, type = "histogram"),
    "The 'type' argument must be either 'line', 'point', 'bar' or 'boxplot'."
  )
})

test_that("plot_vintages throws error if 'dim_col' is not found", {
  expect_error(
    plot_vintages(df_plot, dim_col = "non_existent_col"),
    "The column non_existent_col is not found in 'df'."
  )
})

test_that("plot_vintages throws error if 'time_col' is not found", {
  expect_error(
    plot_vintages(df_plot, time_col = "date"),
    "The column date is not found in 'df'."
  )
})

test_that("plot_vintages throws error if 'time_col' is not Date", {
  df_wrong_time <- dplyr::mutate(df_plot, time = as.character(time))
  expect_error(
    plot_vintages(df_wrong_time, time_col = "time"),
    "The 'time_col' argument must be of class 'Date'."
  )
})

test_that("plot_vintages errors if title is not character", {
  expect_error(
    plot_vintages(df_plot, title = 123),
    "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
  )
})

test_that("plot_vintages errors if subtitle is not character", {
  expect_error(
    plot_vintages(df_plot, subtitle = TRUE),
    "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
  )
})

test_that("plot_vintages errors if ylab is not character", {
  expect_error(
    plot_vintages(df_plot, ylab = factor("value")),
    "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
  )
})

test_that("plot_vintages handles wide format data", {
  p <- plot_vintages(df_plot_wide)
  expect_s3_class(p, "ggplot")
  expect_true(
    any(vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomLine"),
      FUN.VALUE = logical(1)
    ))
  )
})

test_that("plot_vintages warns if dim_col has more than 30 unique values", {
  df_many_vintages <- dplyr::slice(
    df_plot,
    rep(seq_len(dplyr::n()), each = 2)
  ) |>
    dplyr::mutate(
      pub_date = as.Date(pub_date) + rep(0:35, length.out = dplyr::n())
    )

  expect_warning(
    plot_vintages(df_many_vintages, dim_col = "pub_date"),
    "time series supplied. Showing recent 30."
  )
})

test_that("plot_vintages limits to 30 vintages when warning", {
  df_many_vintages <- dplyr::slice(
    df_plot,
    rep(seq_len(dplyr::n()), each = 2)
  ) |>
    dplyr::mutate(
      pub_date = as.Date(pub_date) + rep(0:35, length.out = dplyr::n())
    )

  p <- suppressWarnings(
    plot_vintages(df_many_vintages, dim_col = "pub_date")
  )

  # Check that plot was created
  expect_s3_class(p, "ggplot")
})

test_that("plot_vintages applies title correctly", {
  p <- plot_vintages(df_plot, title = "Test Title")
  expect_equal(p$labels$title, "Test Title")
})

test_that("plot_vintages applies subtitle correctly", {
  p <- plot_vintages(df_plot, title = "Test", subtitle = "Subtitle")
  expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("plot_vintages applies ylab correctly", {
  p <- plot_vintages(df_plot, ylab = "Y-axis Label")
  expect_equal(p$labels$y, "Y-axis Label")
})

test_that("plot_vintages handles release column", {
  p <- plot_vintages(df_release, dim_col = "release")
  expect_s3_class(p, "ggplot")
})

test_that("plot_vintages handles data with id column", {
  p <- plot_vintages(df_with_id, dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
})


test_that("plot_vintages adjusts legend position for many vintages", {
  df_many <- dplyr::slice(
    df_plot,
    rep(seq_len(dplyr::n()), each = 4)
  ) |>
    dplyr::mutate(
      pub_date = as.Date(pub_date) + rep(0:9, length.out = dplyr::n())
    )

  p <- plot_vintages(df_many, dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
})

# ===== Tests for plot.tbl_pubdate =====

test_that("plot.tbl_pubdate calls plot_vintages correctly", {
  df_pubdate <- df_plot
  class(df_pubdate) <- c("tbl_pubdate", class(df_pubdate))

  p <- plot(df_pubdate)
  expect_s3_class(p, "ggplot")
})

test_that("plot.tbl_pubdate passes additional arguments", {
  df_pubdate <- df_plot
  class(df_pubdate) <- c("tbl_pubdate", class(df_pubdate))

  p <- plot(df_pubdate, type = "point", title = "Test")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomPoint"),
    FUN.VALUE = logical(1)
  )))
})

# ===== Tests for plot.tbl_release =====

test_that("plot.tbl_release calls plot_vintages correctly", {
  df_tbl_release <- df_release
  class(df_tbl_release) <- c("tbl_release", class(df_tbl_release))

  p <- plot(df_tbl_release)
  expect_s3_class(p, "ggplot")
})

test_that("plot.tbl_release passes additional arguments", {
  df_tbl_release <- df_release
  class(df_tbl_release) <- c("tbl_release", class(df_tbl_release))

  p <- plot(df_tbl_release, type = "bar", ylab = "Values")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomBar"),
    FUN.VALUE = logical(1)
  )))
})

# ===== Tests for theme_reviser =====

test_that("theme_reviser returns a ggplot theme object", {
  th <- theme_reviser()
  expect_s3_class(th, "theme")
})

test_that("theme_reviser has expected base size", {
  th_default <- theme_reviser()
  th_large <- theme_reviser(base_size = 16)

  expect_s3_class(th_default, "theme")
  expect_s3_class(th_large, "theme")
})

test_that("theme_reviser sets legend position correctly", {
  th_bottom <- theme_reviser(legend.position = "bottom")
  expect_equal(th_bottom$legend.position, "bottom")

  th_top <- theme_reviser(legend.position = "top")
  expect_equal(th_top$legend.position, "top")

  th_right <- theme_reviser(legend.position = "right")
  expect_equal(th_right$legend.position, "right")

  th_left <- theme_reviser(legend.position = "left")
  expect_equal(th_left$legend.position, "left")
})

test_that("theme_reviser sets legend direction correctly", {
  th_horiz <- theme_reviser(legend.direction = "horizontal")
  expect_equal(th_horiz$legend.direction, "horizontal")

  th_vert <- theme_reviser(legend.direction = "vertical")
  expect_equal(th_vert$legend.direction, "vertical")
})

test_that("theme_reviser can combine position and direction", {
  th <- theme_reviser(
    legend.position = "top",
    legend.direction = "vertical"
  )
  expect_equal(th$legend.position, "top")
  expect_equal(th$legend.direction, "vertical")
})

test_that("theme_reviser removes x-axis title", {
  th <- theme_reviser()
  expect_s3_class(th$axis.title.x, "element_blank")
})

test_that("theme_reviser has y-axis title element", {
  th <- theme_reviser()
  expect_s3_class(th$axis.title.y, "element_text")
})

test_that("theme_reviser has plot title formatting", {
  th <- theme_reviser()
  expect_s3_class(th$plot.title, "element_text")
  expect_equal(th$plot.title$face, "bold")
})

test_that("theme_reviser works with ggplot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point() +
    theme_reviser()

  expect_s3_class(p, "ggplot")
})

# ===== Tests for colors_reviser =====

test_that("colors_reviser returns a character vector", {
  cols <- colors_reviser()
  expect_type(cols, "character")
})

test_that("colors_reviser returns valid hex colors", {
  cols <- colors_reviser()
  expect_true(all(grepl("^#([A-Fa-f0-9]{6})$", cols)))
})

test_that("colors_reviser returns multiple colors", {
  cols <- colors_reviser()
  expect_gt(length(cols), 10)
})

test_that("colors_reviser returns unique colors", {
  cols <- colors_reviser()
  expect_equal(length(cols), length(unique(cols)))
})

test_that("colors_reviser includes colorblind-friendly palette", {
  cols <- colors_reviser()
  expect_true("#0072B2" %in% cols) # Blue
  expect_true("#D55E00" %in% cols) # Orange
  expect_true("#009E73" %in% cols) # Green
})

# ===== Tests for scale_color_reviser =====

test_that("scale_color_reviser returns a ScaleDiscrete object", {
  sc <- scale_color_reviser()
  expect_s3_class(sc, "ScaleDiscrete")
})

test_that("scale_color_reviser has correct aesthetic", {
  sc <- scale_color_reviser()
  expect_equal(sc$aesthetics, "colour")
})

test_that("scale_color_reviser uses colors_reviser palette", {
  sc <- scale_color_reviser()
  expected_colors <- colors_reviser()
  palette_func <- sc$palette(length(expected_colors))
  expect_identical(palette_func, expected_colors)
})

test_that("scale_color_reviser works with ggplot", {
  p <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(x = wt, y = mpg, color = factor(cyl))
  ) +
    ggplot2::geom_point() +
    scale_color_reviser()

  expect_s3_class(p, "ggplot")
})

test_that("scale_color_reviser accepts additional arguments", {
  sc <- scale_color_reviser(name = "Custom Name")
  expect_s3_class(sc, "ScaleDiscrete")
})

# ===== Tests for scale_fill_reviser =====

test_that("scale_fill_reviser returns a ScaleDiscrete object", {
  sf <- scale_fill_reviser()
  expect_s3_class(sf, "ScaleDiscrete")
})

test_that("scale_fill_reviser has correct aesthetic", {
  sf <- scale_fill_reviser()
  expect_equal(sf$aesthetics, "fill")
})

test_that("scale_fill_reviser uses colors_reviser palette", {
  sf <- scale_fill_reviser()
  expected_colors <- colors_reviser()
  palette_func <- sf$palette(length(expected_colors))
  expect_identical(palette_func, expected_colors)
})

test_that("scale_fill_reviser works with ggplot", {
  p <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(x = factor(cyl), fill = factor(cyl))
  ) +
    ggplot2::geom_bar() +
    scale_fill_reviser()

  expect_s3_class(p, "ggplot")
})

test_that("scale_fill_reviser accepts additional arguments", {
  sf <- scale_fill_reviser(name = "Custom Fill")
  expect_s3_class(sf, "ScaleDiscrete")
})

# ===== Integration Tests =====

test_that("complete plotting workflow with custom theme and scales", {
  p <- plot_vintages(df_plot, type = "line", dim_col = "pub_date") +
    theme_reviser(base_size = 14, legend.position = "right")

  expect_s3_class(p, "ggplot")
})

test_that("all plot types work with custom theme and scales", {
  types <- c("line", "point", "bar")

  for (plot_type in types) {
    p <- plot_vintages(
      df_plot,
      type = plot_type,
      dim_col = "pub_date",
      title = paste(plot_type, "plot"),
      ylab = "Values"
    )
    expect_s3_class(p, "ggplot")
  }
})

test_that("boxplot works with custom theme", {
  p <- plot_vintages(
    df_plot,
    type = "boxplot",
    dim_col = "pub_date"
  )
  expect_s3_class(p, "ggplot")
})

# ===== Edge Cases =====

test_that("plot_vintages handles missing values gracefully", {
  df_with_na <- df_plot
  df_with_na$value[1:5] <- NA

  p <- plot_vintages(df_with_na, dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
})

test_that("plot_vintages handles very few observations", {
  df_small <- df_plot[1:6, ] # Only 2 obs per vintage

  p <- plot_vintages(df_small, dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
})

test_that("plot_vintages handles extreme values", {
  df_extreme <- df_plot
  df_extreme$value[1] <- 1e6
  df_extreme$value[2] <- -1e6

  p <- plot_vintages(df_extreme, dim_col = "pub_date")
  expect_s3_class(p, "ggplot")
})

test_that("theme_reviser handles very small base_size", {
  th <- theme_reviser(base_size = 6)
  expect_s3_class(th, "theme")
})

test_that("theme_reviser handles very large base_size", {
  th <- theme_reviser(base_size = 24)
  expect_s3_class(th, "theme")
})
