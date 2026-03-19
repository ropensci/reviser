#' Vintages Data
#'
#' A collection of real-time datasets.
#'
#' * GDP: Quarterly Vintages (Billions of real dollars, seasonally adjusted)
#' * Timeframe: Q1 1980 - Q4 2024
#' * Real-Time Vintages: Q4 2002 - Q4 2024
#' @format A tibble with quarterly observations and 4 variables:
#' \describe{
#'   \item{time}{Date of the observation}
#'   \item{pub_date}{Publication date of the vintage}
#'   \item{value}{Numeric, real GDP (seasonally adjusted)}
#'   \item{id}{Country code}
#' }
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
#' Swiss J Economics Statistics 150, 331–352 (2014).
#' \doi{10.1007/BF03399410}
#'
#' @examples
#' # Load gdp dataset
#' data(gdp)
#' head(gdp)
#' @family dataset
"gdp"
