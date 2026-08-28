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

# Is an S3 method registered for this generic and class in reviser's
# namespace? Used to assert that shared methods live on the parent class and
# not on the children.
reviser_has_method <- function(generic, cls) {
  !is.null(getS3method(
    generic,
    cls,
    optional = TRUE,
    envir = asNamespace("reviser")
  ))
}
