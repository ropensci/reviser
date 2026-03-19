# Generalized Kishor-Koenig Model for Nowcasting

Implements a generalized Kishor-Koenig (KK) model for nowcasting and
forecasting with state-space models, allowing for multiple vintages of
data, efficient estimation, and Kalman filtering and smoothing.

## Usage

``` r
kk_nowcast(
  df,
  e,
  h = 0,
  model = "Kishor-Koenig",
  method = "MLE",
  alpha = 0.05,
  solver_options = list()
)
```

## Arguments

- df:

  A data frame or single-ID vintages object in either long or wide
  format. Long-format vintages data are converted internally. After
  preprocessing, the data must contain a `time` column and at least
  `e + 1` releases.

- e:

  An integer indicating the number of data vintages to include in the
  model. Must be greater than 0.

- h:

  An integer specifying the forecast horizon. Default is 0, which
  implies no forecasts. Must be greater than or equal to 0.

- model:

  A string specifying the type of model to use. Options are:

  - "Kishor-Koenig" or "KK" (default): Full Kishor-Koenig model.

  - "Howrey": Howrey's simplified framework.

  - "Classical": Classical model without vintage effects.

- method:

  A string specifying the estimation method to use. Options are "MLE"
  (Maximum Likelihood, default), "SUR", and "OLS".

- alpha:

  Significance level for confidence intervals (default = 0.05).

- solver_options:

  A named list controlling the SUR and MLE routines. Valid names are
  `trace`, `maxiter`, `startvals`, `solvtol`, `gradtol`, `steptol`,
  `transform_se`, `method`, `se_method`, `n_starts`, `seed`,
  `return_states`, `qml_eps`, `qml_score_method`, `qml_scale`,
  `sigma_lower`, `sigma_upper`, and `ic_n`. Supported entries are:

  - `trace`: integer controlling console output.

  - `maxiter`: maximum number of optimizer iterations.

  - `startvals`: optional numeric vector of starting values. Unnamed
    vectors are interpreted in the canonical internal order returned by
    `kk_matrices(e, model, type = "character")$params`. Named vectors
    are reordered to that same canonical order.

  - `solvtol`: tolerance passed to
    [`systemfit::nlsystemfit()`](https://rdrr.io/pkg/systemfit/man/nlsystemfit.html)
    in the SUR estimator.

  - `gradtol`: gradient tolerance passed to
    [`systemfit::nlsystemfit()`](https://rdrr.io/pkg/systemfit/man/nlsystemfit.html)
    in the SUR estimator.

  - `steptol`: step-length tolerance passed to
    [`systemfit::nlsystemfit()`](https://rdrr.io/pkg/systemfit/man/nlsystemfit.html)
    in the SUR estimator.

  - `transform_se`: logical; whether variance parameters are optimized
    on the log scale in MLE.

  - `method`: optimization method for MLE; one of `"L-BFGS-B"`,
    `"BFGS"`, `"Nelder-Mead"`, `"nlminb"`, or `"two-step"`.

  - `se_method`: standard-error method for MLE; one of `"hessian"`,
    `"qml"`, or `"none"`.

  - `n_starts`: number of random starting points used by the MLE
    multi-start routine.

  - `seed`: optional random seed used for the MLE multi-start
    perturbations.

  - `return_states`: logical; whether filtered and smoothed states and
    the fitted KFAS model should be returned.

  - `qml_eps`: finite-difference step size used in QML covariance
    estimation.

  - `qml_score_method`: score approximation method; `"forward"` or
    `"central"`.

  - `qml_scale`: scaling convention for the QML covariance; `"sum"`,
    `"mean"`, or `"hc"`.

  - `sigma_lower`: lower bound for variance parameters on the natural
    scale when `transform_se = TRUE`.

  - `sigma_upper`: upper bound for variance parameters on the natural
    scale when `transform_se = TRUE`.

  - `ic_n`: sample-size convention used for BIC; `"T"` or `"Tp"`.

  For backward compatibility, the legacy aliases `score_method` and
  `score_eps` are also accepted and mapped to `qml_score_method` and
  `qml_eps`.

## Value

A list with the following components:

- states:

  A tibble containing filtered and smoothed state estimates. If
  `solver_options$return_states = FALSE`, this is `NULL`.

- kk_model_mat:

  A list of KK model matrices, such as transition and observation
  matrices.

- ss_model_mat:

  A list of state-space model matrices derived from the KK model.

- model:

  The fitted
  [`KFAS::SSModel`](https://rdrr.io/pkg/KFAS/man/SSModel.html) object.
  If `solver_options$return_states = FALSE`, this is `NULL`.

- params:

  Estimated model parameters with standard errors.

- fit:

  The raw fit object returned by the selected estimator.

- loglik:

  Log-likelihood value for MLE fits; otherwise `NULL`.

- aic:

  Akaike information criterion for MLE fits; otherwise `NULL`.

- bic:

  Bayesian information criterion for MLE fits; otherwise `NULL`.

- convergence:

  Convergence status.

- e:

  The number of the efficient release (0-indexed).

- data:

  The input data after preprocessing to wide format.

- se_method:

  The standard-error method used by MLE; otherwise `NULL`.

- cov:

  Estimated covariance matrix of the parameter estimates, if available;
  otherwise `NULL`.

## Details

The function supports multiple models, including the full Kishor-Koenig
framework, Howrey's model, and a classical approach. It handles data
preprocessing, estimation of system equations using Seemingly Unrelated
Regressions (SUR), and application of the Kalman filter. This is the
first openly available implementation of the Kishor-Koenig model (See
the vignette `vignette("nowcasting_revisions")` for more details).

## References

Kishor, N. Kundan and Koenig, Evan F., "VAR Estimation and Forecasting
When Data Are Subject to Revision", Journal of Business and Economic
Statistics, 2012.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`print.kk_model()`](https://p-wegmueller.github.io/reviser/reference/print.kk_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)

## Examples

``` r
# Example usage:
df <- get_nth_release(
  tsbox::ts_span(
    tsbox::ts_pc(
      dplyr::filter(reviser::gdp, id == "US")
    ),
    start = "1980-01-01"
  ),
  n = 0:1
)
df <- dplyr::select(df, -c("id", "pub_date"))
df <- na.omit(df)

e <- 1 # Number of efficient release
h <- 2 # Forecast horizon
result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")

result$params
#>   Parameter    Estimate    Std.Error
#> 1        F0 0.197750674 0.0732240318
#> 2      G0_0 0.990000000 0.0000000000
#> 3      G0_1 0.079813622 0.0757344824
#> 4        v0 1.598006734 0.1708419518
#> 5      eps0 0.006636095 0.0007112013
```
