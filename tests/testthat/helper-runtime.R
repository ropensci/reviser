reviser_full_tests <- function() {
  identical(tolower(Sys.getenv("REVISER_FULL_TESTS")), "true")
}

skip_if_not_reviser_full_tests <- function() {
  testthat::skip_if_not(
    reviser_full_tests(),
    "Set REVISER_FULL_TESTS=true to run extended optimizer coverage."
  )
}

cached_fixture <- local({
  cache <- new.env(parent = emptyenv())

  function(key, builder) {
    if (!exists(key, envir = cache, inherits = FALSE)) {
      cache[[key]] <- builder()
    }

    cache[[key]]
  }
})
