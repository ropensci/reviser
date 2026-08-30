#' Vintages Data Objects
#'
#' @description
#' `reviser` represents real-time vintages as tibbles that carry an additional
#' class recording which dimension indexes the vintages. Data indexed by
#' publication date are of class `tbl_pubdate`, data indexed by release number
#' are of class `tbl_release`, and both inherit from the common parent class
#' `tbl_vintage`, ahead of the usual tibble classes. A fitted class attribute
#' therefore reads `c("tbl_pubdate", "tbl_vintage", "tbl_df", "tbl",
#' "data.frame")`. An object that carries both a `pub_date` and a `release`
#' column is classed as both, with `tbl_release` taking precedence.
#'
#' The parent class holds everything the two representations share. The
#' [print()], [summary()], [plot()] and [pillar::tbl_sum()] methods are
#' defined once for `tbl_vintage` and inherited by both, so that the two stay
#' consistent with one another. Only the parts that genuinely depend on the
#' indexing dimension are dispatched on the child classes, through the
#' internal generics `vintage_labels()`, `vintage_value_cols()` and
#' `vintage_detail()`.
#'
#' Both representations may be stored in either a long or a wide layout, and
#' the methods below detect which and report accordingly. The layouts, and the
#' columns each one requires, are specified under "Data contract" in
#' [validate_vintages()], which also checks an object against them. Use
#' [vintages_long()] and [vintages_wide()] to convert between layouts.
#'
#' @return This topic documents a class rather than a function. The functions
#'   that build vintages data, such as [get_nth_release()], [vintages_long()]
#'   and [vintages_wide()], return tibbles whose class attribute is
#'   `c("tbl_pubdate", "tbl_vintage", "tbl_df", "tbl", "data.frame")` or the
#'   same with `"tbl_release"` in place of `"tbl_pubdate"`.
#'
#' @srrstats {TS1.0} Uses explicit class systems for time series data
#' @srrstats {TS4.2} Explicitly documents the type and class of return values
#' @srrstats {TS5.0} Documents the class system implemented for vintages data
#'
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Release vintages carry the shared parent class.
#' releases <- get_nth_release(df, n = 0:3)
#' class(releases)
#' inherits(releases, "tbl_vintage")
#'
#' # So do publication-date vintages, in either layout.
#' class(vintages_wide(df)$US)
#'
#' # The print, summary and plot methods are inherited from the parent.
#' summary(releases)
#'
#' @name tbl_vintage
#' @family helpers
#' @seealso [validate_vintages()], [vintages_long()], [vintages_wide()]
NULL


#' Class attribute for a wide-format vintages tibble
#'
#' The single definition of the vintages class vector, so that the specific
#' class, the shared parent and the tibble classes stay in the right order
#' wherever an object is constructed.
#'
#' @param dim Either `"pub_date"` or `"release"`.
#' @return A character vector of class names.
#' @keywords internal
#' @noRd
vintage_class <- function(dim) {
  specific <- switch(
    dim,
    pub_date = "tbl_pubdate",
    release = "tbl_release",
    rlang::abort(paste0("Unknown vintages dimension: ", dim, "."))
  )

  c(specific, "tbl_vintage", "tbl_df", "tbl", "data.frame")
}


# ---- internal extension points ----------------------------------------------
#
# The parent-class methods below are written once against these three internal
# generics. They are the only points at which the publication-date and release
# representations differ.

#' Names a vintages representation uses when reporting itself
#'
#' @param x A `tbl_vintage`.
#' @return A list with elements `dim` (the column naming the vintage dimension
#'   in the long layout), `header` and `title` (labels used by
#'   `tbl_sum()` and `summary()`), and `unit` (the plural noun for one
#'   vintage).
#' @keywords internal
#' @noRd
vintage_labels <- function(x) {
  UseMethod("vintage_labels")
}

#' Columns holding the observed values in the wide layout
#'
#' @param x A `tbl_vintage` stored in the wide layout.
#' @return A character vector of column names.
#' @keywords internal
#' @noRd
vintage_value_cols <- function(x) {
  UseMethod("vintage_value_cols")
}

#' Representation-specific block printed by `summary.tbl_vintage()`
#'
#' @param x A `tbl_vintage`.
#' @param is_long Whether `x` is stored in the long layout.
#' @return A character vector of complete lines.
#' @keywords internal
#' @noRd
vintage_detail <- function(x, is_long) {
  UseMethod("vintage_detail")
}


# ---- tbl_pubdate ------------------------------------------------------------

#' @keywords internal
#' @noRd
vintage_labels.tbl_pubdate <- function(x) {
  list(
    dim = "pub_date",
    header = "publication date",
    title = "Publication Date",
    unit = "Vintages"
  )
}

#' @keywords internal
#' @noRd
vintage_value_cols.tbl_pubdate <- function(x) {
  # In the wide layout every column that is not an identifier or a long-layout
  # key is a vintage, and the column name is itself the publication date.
  # Names that do not parse as dates are not vintages, which is how a class
  # attribute that no longer matches the columns is detected.
  candidates <- setdiff(colnames(x), vintage_reserved_cols)
  candidates[!is.na(as.Date(candidates, format = "%Y-%m-%d"))]
}

#' @keywords internal
#' @noRd
vintage_detail.tbl_pubdate <- function(x, is_long) {
  pub_dates <- if (is_long) {
    as.Date(unique(x$pub_date))
  } else {
    as.Date(vintage_value_cols(x))
  }

  c(
    paste("\nNumber of vintages:", length(pub_dates)),
    "Publication dates:",
    paste("  Earliest:", as.character(min(pub_dates))),
    paste("  Latest:", as.character(max(pub_dates)))
  )
}


# ---- tbl_release ------------------------------------------------------------

#' @keywords internal
#' @noRd
vintage_labels.tbl_release <- function(x) {
  list(
    dim = "release",
    header = "release",
    title = "Release",
    unit = "Releases"
  )
}

#' @keywords internal
#' @noRd
vintage_value_cols.tbl_release <- function(x) {
  candidates <- setdiff(colnames(x), vintage_reserved_cols)
  grep("release|final", candidates, value = TRUE)
}

#' @keywords internal
#' @noRd
vintage_detail.tbl_release <- function(x, is_long) {
  releases <- if (is_long) unique(x$release) else vintage_value_cols(x)

  c(
    paste("\nNumber of releases:", length(releases)),
    paste("Releases:", paste(sort(releases), collapse = ", "))
  )
}


# ---- shared helpers ---------------------------------------------------------

#' Column names that never hold a vintage's observations
#'
#' Identifiers and long-layout keys. Anything else in a wide object is a
#' candidate vintage column.
#'
#' @keywords internal
#' @noRd
vintage_reserved_cols <- c(
  "time",
  "id",
  "value",
  "values",
  "pub_date",
  "release"
)

#' Layout of a vintages object, or `NA` when it matches neither
#'
#' Long-format objects carry the vintage dimension and the observations in two
#' columns; wide-format objects spread the vintages across columns, one per
#' vintage label. An object can be classed as vintages data and match neither,
#' because the class attribute survives operations that drop or rename the
#' columns the methods rely on. That case is reported rather than guessed at:
#' treating it as wide is what made `summary()` fail inside `as.Date()` on
#' column names that are not dates.
#'
#' @param x A `tbl_vintage`.
#' @return `"long"`, `"wide"`, or `NA_character_`.
#' @keywords internal
#' @noRd
vintage_layout <- function(x) {
  if (vintage_labels(x)$dim %in% colnames(x) && "value" %in% colnames(x)) {
    return("long")
  }

  if (length(vintage_value_cols(x)) > 0) {
    return("wide")
  }

  NA_character_
}

#' Layout of a vintages object under any of the classes it carries
#'
#' `vintage_layout()` resolves the layout under the object's first class,
#' which is what the methods need. Validation asks a different question: are
#' these columns a valid vintages layout under *any* representation? If they
#' are, but not under the class the object claims, the object is mislabelled
#' and should be reported as a class mismatch, naming the offending class. If
#' they are not, under either representation, the object is structurally
#' broken and should be reported as such. So both classes are probed here,
#' whether or not the object carries them.
#'
#' @param x A `tbl_vintage`.
#' @return `"long"`, `"wide"`, or `NA_character_`.
#' @keywords internal
#' @noRd
vintage_layout_any <- function(x) {
  for (cls in c("tbl_release", "tbl_pubdate")) {
    probe <- x
    class(probe) <- c(cls, setdiff(class(probe), cls))

    layout <- vintage_layout(probe)
    if (!is.na(layout)) {
      return(layout)
    }
  }

  NA_character_
}

#' Layout of a vintages object, aborting when it matches neither
#'
#' @param x A `tbl_vintage`.
#' @return A single logical: is `x` stored in the long layout?
#' @keywords internal
#' @noRd
vintage_is_long <- function(x) {
  layout <- vintage_layout(x)

  if (is.na(layout)) {
    labels <- vintage_labels(x)
    rlang::abort(
      paste0(
        "`x` carries the '",
        class(x)[1],
        "' class but its columns match neither documented layout: the long ",
        "layout needs a '",
        labels$dim,
        "' column together with 'value', and the wide layout needs one ",
        "column per vintage. This happens when a vintages object is ",
        "manipulated with tools that keep the class attribute while ",
        "dropping or renaming those columns. See `?validate_vintages` for ",
        "the full contract."
      ),
      call = rlang::caller_env()
    )
  }

  layout == "long"
}

#' Number of distinct time periods in a vintages object
#'
#' @param x A `tbl_vintage`.
#' @param is_long Whether `x` is stored in the long layout.
#' @return A single integer.
#' @keywords internal
#' @noRd
vintage_n_time <- function(x, is_long) {
  # In the long layout each time period appears once per vintage, so the row
  # count would overstate the number of periods.
  if (is_long) length(unique(x$time)) else nrow(x)
}

#' Number of vintages in a vintages object
#'
#' @param x A `tbl_vintage`.
#' @param is_long Whether `x` is stored in the long layout.
#' @return A single integer.
#' @keywords internal
#' @noRd
vintage_n_vintages <- function(x, is_long) {
  if (is_long) {
    length(unique(x[[vintage_labels(x)$dim]]))
  } else {
    length(vintage_value_cols(x))
  }
}


# ---- shared methods ---------------------------------------------------------

#' Tibble Summary for Vintages Data
#'
#' Provides the custom header shown when a vintages object is printed. This
#' method is called automatically by \pkg{pillar}. It is defined once for the
#' parent class [tbl_vintage] and inherited by `tbl_pubdate` and `tbl_release`
#' objects alike, so that both report the same fields.
#'
#' @param x An object inheriting from [tbl_vintage], such as a `tbl_pubdate`
#'   or a `tbl_release`.
#' @param ... Additional arguments (unused).
#'
#' @return A named character vector where names are labels and values are
#'   the corresponding information. The vector is used by pillar to format
#'   the tibble header. An object whose columns no longer match either
#'   documented layout falls back to the plain tibble header, so that a broken
#'   object can still be inspected; [summary()] reports the problem.
#' @method tbl_sum tbl_vintage
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#' release_data <- get_nth_release(df, n = 0:3)
#' pillar::tbl_sum(release_data)
#' @family helpers
#' @export
tbl_sum.tbl_vintage <- function(x, ...) {
  labels <- vintage_labels(x)
  layout <- vintage_layout(x)

  # Printing has to keep working even when the object no longer conforms, or
  # the user cannot look at what went wrong.
  if (is.na(layout)) {
    return(NextMethod())
  }

  is_long <- layout == "long"

  header <- stats::setNames(
    c(
      "",
      if (is_long) "long" else "wide",
      vintage_n_time(x, is_long),
      vintage_n_vintages(x, is_long)
    ),
    c(
      paste0("Vintages data (", labels$header, " format)"),
      "Format",
      "Time periods",
      labels$unit
    )
  )

  if ("id" %in% colnames(x)) {
    header <- c(header, "IDs" = length(unique(x$id)))
  }

  header
}

#' Print Method for Vintages Data
#'
#' Print method for objects inheriting from [tbl_vintage]. Delegates to the
#' tibble print method, which calls [tbl_sum.tbl_vintage()] to generate the
#' custom header.
#'
#' @param x An object inheriting from [tbl_vintage], such as a `tbl_pubdate`
#'   or a `tbl_release`.
#' @param ... Additional arguments passed to the next print method.
#'
#' @return The input `x` is returned invisibly.
#' @method print tbl_vintage
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#' release_data <- get_nth_release(df, n = 0:3)
#' print(release_data)
#' @family helpers
#' @export
print.tbl_vintage <- function(x, ...) {
  NextMethod("print")
  invisible(x)
}

#' Summary Method for Vintages Data
#'
#' Reports the layout, time coverage, number of vintages and missing-value
#' count of a vintages object. Defined once for the parent class
#' [tbl_vintage]; the block describing the vintages themselves is supplied by
#' the concrete class.
#'
#' @param object An object inheriting from [tbl_vintage], such as a
#'   `tbl_pubdate` or a `tbl_release`.
#' @param ... Additional arguments (not used).
#'
#' @return The input `object`, invisibly.
#' @method summary tbl_vintage
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#'
#' # Long format
#' release_data <- get_nth_release(df, n = 0:3)
#' summary(release_data)
#'
#' # Wide format
#' wide_release <- vintages_wide(release_data, names_from = "release")
#' summary(wide_release$US)
#'
#' # Publication-date vintages
#' summary(vintages_wide(df)$US)
#' @family helpers
#' @export
summary.tbl_vintage <- function(object, ...) {
  labels <- vintage_labels(object)
  is_long <- vintage_is_long(object)

  cat(paste0(
    "\n=== Vintages Data Summary (", labels$title, " Format) ===\n\n"
  ))

  cat("Format:", if (is_long) "long" else "wide", "\n")
  cat("Time periods:", vintage_n_time(object, is_long), "\n")
  cat(
    "Time range:",
    as.character(min(object$time)),
    "to",
    as.character(max(object$time)),
    "\n"
  )

  if ("id" %in% colnames(object)) {
    cat("Number of IDs:", length(unique(object$id)), "\n")
    cat("IDs:", paste(unique(object$id), collapse = ", "), "\n")
  }

  for (line in vintage_detail(object, is_long)) {
    cat(line, "\n")
  }

  if (is_long) {
    n_missing <- sum(is.na(object$value))
    total_cells <- nrow(object)
  } else {
    value_cols <- vintage_value_cols(object)
    n_missing <- sum(is.na(object[, value_cols]))
    total_cells <- nrow(object) * length(value_cols)
  }
  pct_missing <- round(100 * n_missing / total_cells, 2)
  cat(
    "\nMissing values:",
    n_missing,
    "of",
    total_cells,
    paste0("(", pct_missing, "%)"),
    "\n"
  )

  invisible(object)
}

#' Plot Method for Vintages Data
#'
#' Plots a vintages object along the dimension implied by its class:
#' publication date for a `tbl_pubdate`, release number for a `tbl_release`.
#' Defined once for the parent class [tbl_vintage] and inherited by both.
#' [plot_vintages()] remains the entry point when the dimension, title or
#' plot type need to be set explicitly.
#'
#' @param x An object inheriting from [tbl_vintage], such as a `tbl_pubdate`
#'   or a `tbl_release`.
#' @param ... Additional arguments passed to [plot_vintages()].
#'
#' @return A ggplot2 object.
#' @srrstats {TS5.0} Implements default plot methods for implemented
#'   class system
#' @srrstats {TS4.2} Explicitly documents the type and class of return values
#' @method plot tbl_vintage
#' @examples
#' df <- dplyr::filter(reviser::gdp, id == "US")
#' plot(df)
#'
#' release_data <- get_nth_release(df, n = 0:5)
#' plot(release_data)
#' @family revision graphs
#' @export
plot.tbl_vintage <- function(x, ...) {
  plot_vintages(x, dim_col = vintage_labels(x)$dim, ...)
}
