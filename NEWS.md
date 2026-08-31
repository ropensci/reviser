# reviser 0.3.0

## Breaking changes

* The two fitted-model classes now inherit from a common parent class,
  `revision_model`, and the two vintages classes from a common parent class,
  `tbl_vintage`. A fitted model therefore carries the class attribute
  `c("kk_model", "revision_model", "list")` and a vintages object
  `c("tbl_pubdate", "tbl_vintage", "tbl_df", "tbl", "data.frame")`.
  Ordinary use is unaffected: objects built by the package's own functions
  gain the parent class automatically, and the generics dispatch as before.
  Two situations do change.
    * Code that *replaced* the class attribute wholesale, as in
      `class(x) <- c("tbl_release", "tbl_df", "tbl", "data.frame")`, must now
      include the parent class as well. `validate_vintages()` reports objects
      that do not. Code that *prepended* to the existing attribute, as in
      `class(x) <- c("tbl_release", class(x))`, keeps the parent class and is
      unaffected.
    * Objects **serialized with an earlier version** no longer dispatch to
      the print, summary, plot or extractor methods, because their stored
      class attribute predates the parent class. Re-create them, or add the
      parent class (`tbl_vintage`, and `revision_model` for fitted models)
      to the stored object.
* The pillar header printed for a long-format `tbl_pubdate` changed: it now
  reports the number of distinct dates rather than the number of rows (see
  below), and gained the `Format` row that `tbl_release` already showed.
* The bundled `gdp` data set is now itself a vintages object, with class
  attribute `c("tbl_pubdate", "tbl_vintage", "tbl_df", "tbl", "data.frame")`.
  `print()`, `summary()`, `plot()` and `validate_vintages()` therefore work on
  it as loaded, rather than only after a release-extraction step. The data are
  unchanged; what changes is that printing `gdp` now shows the vintages header
  instead of the plain tibble header.
* `vintages_long()` no longer warns when it is handed long data that carry no
  vintages class. Attaching the class is real work there, and it is the
  documented way to recover the class after an operation that dropped it, so
  the warning made the recommended idiom noisy. Long input that is *already* a
  vintages object still warns, because the call is then a no-op.

## Bug fixes

* The pillar header shown when printing a long-format `tbl_pubdate` reported
  the number of rows as the number of time periods, which overstates it once
  a series has more than one vintage. It now counts distinct dates, matching
  `tbl_release` and the `summary()` method.
* `summary()` on a vintages object whose columns no longer match either
  documented layout -- for example after `dplyr::select()` dropped `value` or
  the vintage key, which leaves the class attribute in place -- failed inside
  `as.Date()` with "character string is not in a standard unambiguous
  format". It now reports what the object is missing and points to
  `?validate_vintages`. `print()` on such an object falls back to the plain
  tibble header instead of failing, so it can still be inspected.
* Standard errors requested with `se_method = "qml"` in `kk_nowcast()`
  inverted the Hessian with a general `solve()` and, on failure, silently
  applied a ridge. This path now uses the same Cholesky-based
  `invert_hessian()` helper as the other standard-error methods, so a Hessian
  that is not positive definite is reported rather than masked. Estimates are
  unchanged; standard errors change only where the previous ridge was
  silently applied.
* Several validation messages contained hard-wrapped newlines and source
  indentation, which appeared verbatim in the console. They are now single
  lines.
* `plot()` on a model fitted with `return_states = FALSE` failed with the base
  error "argument is of length zero", because it read the dropped `states`
  component without checking for it. It now reports the cause in the same
  words as `states()`, `fitted()`, `residuals()` and `predict()`, all of which
  now share a single definition of that message.
* `validate_vintages()` misdiagnosed a vintages object that had lost a
  long-layout column: because the layout was inferred from the column names
  alone, an object missing `value` was reported as a wide object whose column
  names were "not labeled correctly". It now reports what the object is
  actually missing, matching the message `summary()` gives for the same
  object. Objects whose class attribute contradicts their columns are still
  reported as the class mismatch they are.
* `predict()` on a model fitted with `h = 0` returned a zero-row tibble with
  no explanation. It now says which argument decides that.
* The Kalman filter's stationary initial-state covariance in `jvn_nowcast()`
  fell back to a ridge-regularized solve near a non-stationary boundary
  without reporting it, unlike the parameter-covariance fallback added
  earlier in this version. The converged estimate's fitted object now
  carries a `p0_regularized` flag, which `summary()` reports when `TRUE`.
  Trial parameter vectors evaluated during optimization are unaffected and
  still regularize silently, since a momentarily non-stationary trial point
  is expected there and reporting it would be noise, not diagnosis.

## New features

* The standard extractor methods on fitted models -- `coef()`, `vcov()`,
  `logLik()`, `nobs()`, `fitted()`, `residuals()` and `predict()` -- together
  with `states()`, `print()`, `summary()` and `plot()`, are now defined once
  for `revision_model` and inherited by `kk_model` and `jvn_model`, instead
  of being registered separately for each. The same applies to `print()`,
  `summary()`, `plot()` and `pillar::tbl_sum()` on `tbl_vintage`. See
  `?revision_model` and `?tbl_vintage` for the class contracts and the
  extension points a new model family or vintages representation must supply.

## Documentation

* Help page titles now use a consistent title-case style throughout the
  package.
* The Kishor-Koenig and Jacobs-Van Norden vignettes now reach fitted models
  through the extractor generics -- `coef()`, `logLik()`, `AIC()`, `BIC()`,
  `nobs()` and `states()` -- rather than indexing into `fit$params` and
  `fit$states`.
* `?reviser-vintages-classes` and `?tbl_vintage` no longer restate the same
  material: the former documents the data contract and `validate_vintages()`,
  the latter the class hierarchy and the methods the parent provides.
* `inst/CITATION` reports the current version and title.
* The `?kk_nowcast` example reached into the fitted object with
  `result$params`. It now uses `coef()` and `logLik()`, matching the vignettes
  and the rest of the documentation.
* `?validate_vintages` gains an "Operations that drop the class" section,
  which names `tidyr::drop_na()` as the case most likely to be met in a
  vintages workflow and gives the idiom for recovering the class.

## Internal

* The shared behavior of the two model families, and of the two vintages
  representations, is now expressed through S3 inheritance rather than
  through per-class methods forwarding to common helper functions. The model
  families differ only in `model_family()`, `spec_lines()`, `signal_state()`,
  `target_column()` and `default_plot_state()`; the vintages representations
  only in `vintage_labels()`, `vintage_value_cols()` and `vintage_detail()`.
  These are also the methods a new family or representation has to supply.
  Estimates, plots and printed output for `kk_model`, `jvn_model` and
  `tbl_release` objects are unchanged.
* Test coverage of the multi-series (`id`-aware) code paths in
  `revisions.R`, and of the `revision_summary` print and diagnose branches,
  has been substantially extended. Every method that depends on the state
  estimates is now tested to report `return_states = FALSE` as the cause.
* Comments in `jvn.R` no longer contain non-ASCII typographic quotes.
* The stationary initial-state covariance in `jvn_nowcast()` is now obtained
  by diagonalizing the transition matrix and solving the resulting Lyapunov
  equation elementwise in the eigenbasis, rather than by forming and solving
  the dense `m^2 x m^2` linear system `vec(P) = (I - T %x% T)^{-1} vec(S)`.
  This is cheaper (`O(m^3)` versus `O(m^6)`) and exploits the same
  transition-matrix structure -- an AR companion block plus diagonal
  news/noise blocks -- that motivated the earlier Cholesky and `tcrossprod()`
  changes in this version. The dense solve remains as a fallback for the
  rare case of a non-diagonalizable transition matrix. Estimates are
  unchanged.

# reviser 0.2.0

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
* Added Jacobs-Van Norden nowcasting support via `jvn_nowcast()`.
* Improved estimation methods and solver behavior in `kk_nowcast()`.
* Expanded examples, tests, and documentation.
