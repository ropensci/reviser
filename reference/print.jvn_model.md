# Print method for JVN model objects

Default print method for `jvn_model` objects. This method dispatches to
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md)
for a consistent console display.

## Usage

``` r
# S3 method for class 'jvn_model'
print(x, ...)
```

## Arguments

- x:

  An object of class `jvn_model`.

- ...:

  Additional arguments passed to
  [`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md).

## Value

The input object, invisibly.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.kk_model()`](https://p-wegmueller.github.io/reviser/reference/print.kk_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)

## Examples

``` r
# \donttest{
gdp_growth <- dplyr::filter(
  tsbox::ts_pc(reviser::gdp),
  id == "EA",
  time >= min(pub_date),
  time <= as.Date("2020-01-01")
)
gdp_growth <- tidyr::drop_na(gdp_growth)
df <- get_nth_release(gdp_growth, n = 0:3)

result <- jvn_nowcast(
  df = df,
  e = 4,
  ar_order = 2,
  h = 0,
  include_news = TRUE,
  include_noise = TRUE
)
result
#> 
#> === Jacobs-Van Norden Model ===
#> 
#> Convergence: Success 
#> Log-likelihood: 256.22 
#> AIC: -490.44 
#> BIC: -465.7 
#> 
#> Parameter Estimates:
#>     Parameter Estimate Std.Error
#>         rho_1    0.900     0.278
#>         rho_2   -0.236     0.234
#>       sigma_e    0.001     0.497
#>    sigma_nu_1    0.070     0.006
#>    sigma_nu_2    0.052     0.004
#>    sigma_nu_3    0.001     0.036
#>    sigma_nu_4    0.633     0.210
#>  sigma_zeta_1    0.001     0.021
#>  sigma_zeta_2    0.001     0.009
#>  sigma_zeta_3    0.001     0.017
#>  sigma_zeta_4    0.042     0.004
#> 
# }
```
