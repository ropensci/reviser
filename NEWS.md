# reviser (development version)

## Bug fixes

* `summary()` on a long-format `tbl_pubdate` no longer fails with
  "character string is not in a standard unambiguous format". The method
  assumed a wide layout and treated the `pub_date` and `value` column
  names as publication dates, so it failed on every `get_revisions()`
  result. The reported number of time periods and vintages was also wrong
  for long input.
* `print()` and `summary()` on a `kk_model` now report which specification
  was estimated. `model = "Howrey"` and `model = "Classical"` previously
  produced identical headers, because the fitted object never recorded the
  `model` argument. `jvn_model` objects likewise report whether news,
  noise or both were estimated.

## New features

* `kk_model` and `jvn_model` objects gain the standard extractor methods:
  `coef()`, `vcov()`, `logLik()`, `nobs()`, `fitted()`, `residuals()` and
  `predict()`. `AIC()` and `BIC()` therefore work, and reproduce the values
  shown by `summary()`.
* New `states()` generic to access the estimated state paths of a fitted
  revision model, replacing direct use of `fit$states`.
* New `validate_vintages()` checks a `tbl_pubdate` or `tbl_release` object
  against the documented class contract. See `?"reviser-vintages-classes"`
  for the contract itself.

## Internal

* Covariance matrices are obtained through a Cholesky factorization, which
  exploits the symmetry of the Hessian and reports when it is not positive
  definite instead of silently applying a ridge. Delta-method
  transformations exploit the diagonal structure of the Jacobian, and the
  Kalman recursions use `tcrossprod()`. Estimates are unchanged.

# reviser 0.1.1

* Updated repository, issue tracker, and documentation links to the
  rOpenSci organization and docs site.
* Updated package documentation and README badges to use rOpenSci URLs.
* Added rOpenSci R-universe installation instructions to the README.
* Removed the package-specific code of conduct file in favor of the
  rOpenSci project-wide code of conduct.
* Disabled automatic pkgdown deployment to GitHub Pages and replaced the
  legacy website with a redirect page.

# reviser 0.1.0

* Initial CRAN release.
* Added Jacobs-van Norden nowcasting support via `jvn_nowcast()`.
* Improved estimation methods and solver behavior in `kk_nowcast()`.
* Expanded examples, tests, and documentation.
