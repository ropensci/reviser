#' Vintages Data
#'
#' A collection of real-time datasets.
#'
#' * GDP: Quarterly Vintages (Billions of real dollars, seasonally adjusted)
#' * Timeframe: Q1 1980 - Q4 2024
#' * Real-Time Vintages: Q4 2002 - Q4 2024
#' @format A `tbl_pubdate` vintages object in the long layout: a tibble
#'   carrying the class attribute `c("tbl_pubdate", "tbl_vintage", "tbl_df",
#'   "tbl", "data.frame")`, with quarterly observations and 4 variables:
#' \describe{
#'   \item{time}{Date of the observation}
#'   \item{pub_date}{Publication date of the vintage}
#'   \item{value}{Numeric, real GDP (seasonally adjusted)}
#'   \item{id}{Country code}
#' }
#'
#' Because it is a vintages object rather than a plain tibble, the generics
#' [print()], [summary()] and [plot()] work on it directly. See
#' [validate_vintages()] for the data contract and [tbl_vintage] for the class
#' hierarchy.
#'
#' @section Sources:
#' * All the data is from the realtime database of Indergand and Leist (2014).
#' **Countries**:
#' * CHE:
#'   * Switzerland
#'   * Source: SECO
#'
#' * US:
#'   * United States
#'   * Sources: FRED, OECD
#'
#' * EA:
#'   * Euro Area
#'   * Sources: Eurostat, OECD
#'
#' * JP:
#'   * Japan
#'   * Sources: Cabinet Office (Japan), OECD
#'
#'
#' @srrstats {G1.0} academic literature
#'
#' @references Indergand, R., Leist, S. A Real-Time Data Set for Switzerland.
#' Swiss J Economics Statistics 150, 331--352 (2014).
#' \doi{10.1007/BF03399410}
#'
#' @examples
#' # Load gdp dataset
#' data(gdp)
#' head(gdp)
#'
#' # It is a vintages object, so the generics dispatch on it directly.
#' class(gdp)
#' summary(gdp)
#' @family dataset
#' @seealso [validate_vintages()], [tbl_vintage]
"gdp"
