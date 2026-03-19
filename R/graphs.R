#' Plot Vintages Data
#'
#' A flexible function to visualize vintage data using various plot types such
#' as line plots, point plots, bar plots, or boxplots. The function ensures
#' that input data is validated and appropriately transformed before plotting.
#'
#' @param df A data frame containing the vintage data to be plotted. Must
#' include at least two columns:
#' one for time (`time`) and one for value (`value`).
#' @param type A character string specifying the type of plot to create.
#' Options are:
#' \itemize{
#'   \item{ "line": Line plot (default).}
#'   \item{ "point": Scatter plot.}
#'   \item{ "bar": Bar plot.}
#'   \item{ "boxplot": Boxplot.}
#'   }
#' @param dim_col A character string specifying the column name in `df` that
#' represents publication dates or other grouping dimensions (e.g. `"release"`).
#' Defaults to `"pub_date"`.
#' @param time_col A character string specifying the column name in `df` that
#' represents the time variable. Defaults to `"time"`.
#' @param title A character string specifying the title of the plot.
#' Defaults to an empty string.
#' @param subtitle A character string specifying the subtitle of the plot.
#' Defaults to an empty string.
#' @param ylab A character string specifying the label for the y-axis.
#' Defaults to an empty string.
#'
#' @return A ggplot2 plot object representing the specified vintage
#' data visualization.
#'
#' @srrstats {G2.0} Implements assertions on input types and parameters
#' @srrstats {G2.1} Validates data types of inputs through explicit checks
#' @srrstats {G2.3a} Uses explicit validation to only permit expected values
#' for 'type' parameter
#' @srrstats {G2.8} Provides appropriate conversion routines for different data
#' formats
#' @srrstats {G2.9} Issues diagnostic messages for data transformations (warns
#' when limiting to 30 series)
#' @srrstats {G5.8} Includes edge condition tests (e.g., handling single value
#' in dim_col for boxplot)
#' @srrstats {TS1.2} Validates inputs via vintages_check() and explicit column
#' checks
#' @srrstats {TS1.3} Pre-processes data through standardization and conversion
#' from wide to long if needed
#' @srrstats {TS5.0} Implements plotting methods for time series visualization
#' @srrstats {TS5.1} Clearly labels time axis with continuous units (ensures
#' time_col is Date class)
#' @srrstats {TS5.2} time variable on horizontal axis by default
#' @srrstats {TS5.3} Ensures time units are properly formatted as Date objects
#' @srrstats {TS5.5} Handles data with missing values by filtering them out
#' during pre-processing
#'
#' @details
#' The `plot_vintages` function is designed to handle data frames in both
#' wide and long formats. It ensures
#' that the provided data frame includes the necessary columns for plotting.
#' If the `dim_col` column contains
#' more than 30 unique values, only the most recent 30 are plotted.
#' Additionally, the function supports
#' custom themes and color scales using `scale_color_reviser`,
#' `scale_fill_reviser`, and `theme_reviser`.
#'
#' The function raises an error if:
#' \itemize{
#'  \item{The `type` argument is not one of `"line"`, `"point"`, `"bar"`,
#'  or `"boxplot"`.}
#'  \item{The specified `dim_col` is not a column in `df`.}
#'  \item{`title`, `subtitle`, or `ylab` are not character strings.}
#' }
#'
#' @seealso [theme_reviser()], [scale_color_reviser()], [scale_fill_reviser()]
#' @examples
#' # Example data
#' df <- data.frame(
#'   time = rep(seq.Date(from = as.Date("2022-01-01"),
#'   by = "month", length.out = 12), 3),
#'   value = runif(36, 50, 100),
#'   pub_date = rep(c("2022-01-05", "2022-02-07", "2022-03-03"), each = 12)
#' )
#'
#' # Line plot
#' plot_vintages(
#'   df,
#'   type = "line",
#'   dim_col = "pub_date",
#'   title = "Line plot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' # Point plot
#' plot_vintages(
#'   df,
#'   type = "point",
#'   dim_col = "pub_date",
#'   title = "Scatter plot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' # Bar plot
#' plot_vintages(
#'   df,
#'   type = "bar",
#'   dim_col = "pub_date",
#'   title = "Bar plot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' # Boxplot
#' plot_vintages(
#'   df,
#'   type = "boxplot",
#'   dim_col = "pub_date",
#'   title = "Boxplot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' @family revision graphs
#' @export
plot_vintages <- function(
  df,
  type = "line",
  dim_col = "pub_date",
  time_col = "time",
  title = "",
  subtitle = "",
  ylab = ""
) {
  # Check type input
  if (!type %in% c("line", "point", "bar", "boxplot")) {
    rlang::abort(
      "The 'type' argument must be either 'line', 'point', 'bar' or 'boxplot'."
    )
  }

  # Check 'time_col' is column name of 'df'
  if (!time_col %in% colnames(df)) {
    rlang::abort(
      paste0(
        "The column ",
        time_col,
        " is not found in 'df'."
      )
    )
  }

  check <- vintages_check(df, time_col = time_col)
  if (check == "wide") {
    df <- vintages_long(df, keep_na = FALSE)
  }

  # Check 'dim_col' is column name of 'df'
  if (!dim_col %in% colnames(df)) {
    rlang::abort(
      paste0(
        "The column ",
        dim_col,
        " is not found in 'df'."
      )
    )
  }

  # Check that 'time_col' is of date format
  if (!inherits(df[[time_col]], "Date")) {
    rlang::abort(
      "The 'time_col' argument must be of class 'Date'."
    )
  }

  # Check title and subtitle are character strings
  if (!is.character(title) || !is.character(subtitle) || !is.character(ylab)) {
    rlang::abort(
      "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
    )
  }

  dim_col <- as.name(dim_col)
  time_col <- as.name(time_col)

  if (ncol(df) <= 1L) {
    rlang::abort("'df' must have at least two columns.")
  }

  n <- length(unique(df[[dim_col]]))

  df <- df |>
    dplyr::group_by(.data$time) |>
    dplyr::arrange(dplyr::desc(abs(.data$value)), .by_group = TRUE) |>
    dplyr::ungroup()

  if (n > 1) {
    if (class(df[[dim_col]]) %in% c("Date", "integer", "numeric")) {
      df[[dim_col]] <- as.character(df[[dim_col]])
    }

    if (n > 30) {
      rlang::warn(
        paste0(n, " time series supplied. Showing recent 30.")
      )
      df <- df |>
        dplyr::filter(!!dim_col %in% utils::tail(unique(!!dim_col), 30))
    }
  }

  p <- ggplot2::ggplot()
  needs_color_scale <- FALSE
  needs_fill_scale <- FALSE

  if (n == 1L) {
    if (type == "line") {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(x = !!time_col, y = .data$value),
          data = df
        )
    } else if (type == "point") {
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(x = !!time_col, y = .data$value),
          data = df
        )
    } else if (type == "bar") {
      needs_fill_scale <- TRUE
      p <- p +
        ggplot2::geom_bar(
          ggplot2::aes(x = !!time_col, y = .data$value),
          data = df,
          position = "identity",
          stat = "identity"
        )
    } else if (type == "boxplot") {
      rlang::abort(
        "'type' boxplot not supported if 'dim_col' contains one unique value."
      )
    } else {
      rlang::abort("Invalid 'type' argument. Must be either 'line' or 'point'.")
    }
  } else {
    if (type == "line") {
      needs_color_scale <- TRUE
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(x = !!time_col, y = .data$value, color = !!dim_col),
          data = df
        )
    } else if (type == "point") {
      needs_color_scale <- TRUE
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(x = !!time_col, y = .data$value, color = !!dim_col),
          data = df
        )
    } else if (type == "bar") {
      needs_color_scale <- TRUE
      needs_fill_scale <- TRUE
      p <- p +
        ggplot2::geom_bar(
          ggplot2::aes(
            x = !!time_col,
            y = .data$value,
            color = !!dim_col,
            fill = !!dim_col
          ),
          position = "identity",
          stat = "identity",
          data = df
        )
    } else if (type == "boxplot") {
      p <- p +
        ggplot2::geom_boxplot(
          ggplot2::aes(
            x = !!time_col,
            y = .data$value,
            fill = factor(!!time_col)
          ),
          data = df
        ) +
        theme_reviser(legend.position = "none")
    } else {
      rlang::abort("Invalid 'type' argument. Must be either 'line' or 'point'.")
    }
  }

  if (needs_color_scale) {
    p <- p + scale_color_reviser()
  }

  if (needs_fill_scale) {
    p <- p + scale_fill_reviser()
  }

  # labels and title
  p <- p + ggplot2::ylab(ylab)
  if (!missing("title")) {
    if (missing("subtitle")) {
      subtitle <- NULL
    }
    p <- p + ggplot2::ggtitle(label = title, subtitle = subtitle)
  }

  if (!type == "boxplot") {
    if (n > 5) {
      p <- p +
        theme_reviser(legend.position = "right", legend.direction = "vertical")
    } else {
      p <- p +
        theme_reviser(
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
    }
  }
  p
}

#' Plot Method for Publication Date Vintages
#'
#' @param x An object of class \code{tbl_pubdate}.
#' @param ... Additional arguments passed to plot_vintages.
#'
#' @return A ggplot2 object.
#' @srrstats {TS5.0} Implements default plot methods for implemented
#'   class system
#' @srrstats {TS4.2} Explicitly documents the type and class of return values
#' @method plot tbl_pubdate
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#' plot(df)
#' @family revision graphs
#' @export
plot.tbl_pubdate <- function(x, ...) {
  plot_vintages(x, dim_col = "pub_date", ...)
}

#' Plot Method for Release Vintages
#'
#' @param x An object of class \code{tbl_release}.
#' @param ... Additional arguments passed to plot_vintages.
#'
#' @return A ggplot2 object.
#' @srrstats {TS5.0} Implements default plot methods for implemented
#'   class system
#' @srrstats {TS4.2} Explicitly documents the type and class of return values
#' @method plot tbl_release
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#' df <- get_nth_release(df, n = 0:5)
#' plot(df)
#' @family revision graphs
#' @export
plot.tbl_release <- function(x, ...) {
  plot_vintages(x, dim_col = "release", ...)
}

#' Plot Revision Model Results
#'
#' @param x An object of class 'revision_model'
#' @param state String. The name of the state to visualize.
#' @param type String. Type of estimate: "filtered" or "smoothed".
#' @param ... Additional arguments passed to theme_reviser.
#' @srrstats {G1.4a} Internal function documented with @noRd tag
#' @srrstats {TS5.0} Implements plot methods for class system
#' @srrstats {TS5.7} Includes model (input) values in plot with forecast
#'   (output) values (in-sample vs out-of-sample)
#' @srrstats {TS5.8} Provides clear visual distinction between model and
#'   forecast values (different colors/samples)
#' @srrstats {TS5.6} Indicates distributional limits (confidence
#'   intervals) on plot by default
#' @return ggplot object
#'
#' @keywords internal
#' @noRd
plot.revision_model <- function(x, state = NULL, type = "filtered", ...) {
  # Handle defaults if not provided by the child method
  if (is.null(state)) {
    state <- x$states[x$states$filter == type, ]$state[1]
    rlang::warn(paste("No state specified. Defaulting to first state:", state))
  }

  # Filter data
  plot_data <- x$states[x$states$state == state & x$states$filter == type, ]

  if (nrow(plot_data) == 0) {
    rlang::abort(paste("State", state, "not found in model results."))
  }
  # Setup Aesthetics (Unified Legend)
  pal <- colors_reviser()
  col_values <- c("in_sample" = pal[1], "out_of_sample" = pal[2])
  line_values <- c("in_sample" = "solid", "out_of_sample" = "solid")
  label_values <- c(
    "in_sample" = "In-sample",
    "out_of_sample" = "Out-of-sample"
  )

  # Build Plot
  # Split samples
  in_sample_data <- plot_data[plot_data$sample == "in_sample", ]
  oos_data <- plot_data[plot_data$sample == "out_of_sample", ]

  p <- ggplot2::ggplot() +

    # ---- In-sample: always line + ribbon ----
    ggplot2::geom_ribbon(
      data = in_sample_data,
      ggplot2::aes(
        x = .data$time,
        ymin = .data$lower,
        ymax = .data$upper,
        fill = sample
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = in_sample_data,
      ggplot2::aes(
        x = .data$time,
        y = .data$estimate,
        color = sample,
        linetype = sample
      ),
      linewidth = 0.8
    )

  # ---- Out-of-sample: branch on length ----
  if (nrow(oos_data) == 1) {
    p <- p +
      ggplot2::geom_crossbar(
        data = oos_data,
        ggplot2::aes(
          x = .data$time,
          y = .data$estimate,
          ymin = .data$lower,
          ymax = .data$upper,
          color = sample,
          fill = sample
        ),
        width = 0.1,
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = oos_data,
        ggplot2::aes(
          x = .data$time,
          y = .data$estimate,
          color = sample,
          fill = sample
        ),
        size = 2
      )
  } else {
    p <- p +
      ggplot2::geom_ribbon(
        data = oos_data,
        ggplot2::aes(
          x = .data$time,
          ymin = .data$lower,
          ymax = .data$upper,
          fill = sample
        ),
        alpha = 0.2
      ) +
      ggplot2::geom_line(
        data = oos_data,
        ggplot2::aes(
          x = .data$time,
          y = .data$estimate,
          color = sample,
          linetype = sample
        ),
        linewidth = 0.8
      )
  }

  # ---- Scales & theme ----
  p <- p +
    ggplot2::scale_fill_manual(
      values = col_values,
      labels = label_values
    ) +
    ggplot2::scale_linetype_manual(
      values = line_values,
      guide = "none"
    ) +
    ggplot2::scale_color_manual(
      values = col_values,
      labels = label_values
    ) +
    theme_reviser(...) +
    ggplot2::labs(
      title = paste(type, "estimate:", state),
      subtitle = "with 95% confidence intervals",
      color = "Sample",
      fill = "Sample",
      linetype = "Sample"
    ) +
    ggplot2::ylab("")

  p
  return(p)
}


#' Custom Visualization Theme and Color Scales for Reviser
#'
#' These functions provide a custom visualization theme and color scales for
#' use with ggplot2, inspired by the `tsbox` package.
#'
#' @param base_size Numeric. The base font size for the theme. Default is 12.
#' @param legend.position Character. Position of the legend.
#' Default is "bottom".
#' @param legend.direction Character. Direction of the legend.
#' Default is "horizontal".
#' @param ... Additional arguments passed to the ggplot2 scale functions.
#'
#' @return A customized ggplot2 theme, color palette, or scale.
#'
#' @details
#' \itemize{
#' \item{`theme_reviser`: Defines a minimal theme with custom adjustments for
#' axis titles, plot titles, subtitles, captions, and legend positioning.}
#' \item{`colors_reviser`: Provides a predefined set of colors, including a
#' soft black, a palette suitable for colorblind readers, and additional
#' colors for extended use.}
#' \item{`scale_color_reviser`: A ggplot2 color scale that uses the custom
#' `colors_reviser` palette.}
#' \item{`scale_fill_reviser`: A ggplot2 fill scale that uses the custom
#' `colors_reviser` palette.}
#'}
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   theme_reviser() +
#'   scale_color_reviser()
#'
#' @family revision graphs
#' @export
theme_reviser <- function(
  base_size = 12,
  legend.position = "bottom",
  legend.direction = "horizontal"
) {
  half_line <- base_size / 2
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = "grey10",
        margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)
      ),
      plot.title = ggplot2::element_text(
        color = "grey10",
        face = "bold",
        margin = ggplot2::margin(t = half_line * 2, b = half_line * 0.7),
        hjust = 0,
        size = ggplot2::rel(1.2)
      ),
      plot.subtitle = ggplot2::element_text(
        color = "grey10",
        margin = ggplot2::margin(t = 0, b = half_line * 1.2),
        size = ggplot2::rel(0.9),
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        color = "grey50",
        margin = ggplot2::margin(t = 0, b = half_line * 1.2),
        size = ggplot2::rel(0.8)
      ),
      panel.grid = ggplot2::element_line(linewidth = 0.2),
      axis.text = ggplot2::element_text(
        color = "grey10",
        size = ggplot2::rel(0.7)
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        color = "grey10",
        size = ggplot2::rel(0.9)
      ),
      legend.position = legend.position,
      legend.direction = legend.direction
    )
}

#' @family revision graphs
#' @export
#' @name theme_reviser
colors_reviser <- function() {
  c(
    # A soft black
    "#4D4D4D",
    # colorblindr
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#E69F00",
    "#56B4E9",
    "#CC79A7",
    "#F0E442",
    "#999999",
    # Additional Colors
    "#8D0808",
    "#461E78",
    "#4AFFF0",
    "#34BDCC",
    "#4F61A1",
    "#440A4F",
    "#C3FBC4",
    "#85F9D6",
    "#79C7AD",
    "#A6CC7A",
    "#DFFF7B",
    "#8D7B88",
    "#4E414F",
    "#BAADB5",
    "#2D2538",
    "#837A80",
    "#FFF68F",
    "#800080",
    "#F8B1CC",
    "#C29BFF",
    "#FFD700",
    "#FF6347"
  )
}

#' @family revision graphs
#' @export
#' @name theme_reviser
scale_color_reviser <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = scales::manual_pal(colors_reviser()),
    ...
  )
}

#' @family revision graphs
#' @export
#' @name theme_reviser
scale_fill_reviser <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = scales::manual_pal(colors_reviser()),
    ...
  )
}
