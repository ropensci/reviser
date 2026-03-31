#' @description
#' **reviser** is an R package designed for working with time-series vintages
#' data. The package provides tools to clean, visualize, and analyze time-series
#' revisions. To learn more about reviser, start with the vignettes:
#' `browseVignettes(package = "reviser")`
#' The package will be actively developed and maintained.
#'
#' Useful links:
#' * <https://docs.ropensci.org/reviser/>
#' * Report bugs at <https://github.com/ropensci/reviser/issues>
#' @srrstats {G1.2} Life Cycle Statement
#' @srrstats {G1.4} Using roxygen2 for function documentation
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(
  ".data"
))

# Import from KFAS necessary to overcome a bug when defining the model
#' @importFrom KFAS SSModel SSMcustom
#' @importFrom calculus %mx% %diff% %sum% %prod%
#' @importFrom pillar tbl_sum
#' @srrstats {G1.4} roxygen2 is used for all documentation.
#' @srrstats {G1.4a} All internal functions are also documented
NULL
