# Print Method for KK Model

Default print method for `kk_model` objects. Wraps the `summary` method
for a consistent output.

## Usage

``` r
# S3 method for class 'kk_model'
print(x, ...)
```

## Arguments

- x:

  An object of class `kk_model`.

- ...:

  Additional arguments passed to `summary.kk_model`.

## Value

The function returns the input `x` invisibly.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)

## Examples

``` r
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

e <- 1
h <- 2
result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig", method = "MLE")
result
#> 
#> === Kishor-Koenig Model ===
#> 
#> Convergence: Success 
#> Log-likelihood: -100.83 
#> AIC: 211.67 
#> BIC: 230.99 
#> 
#> Parameter Estimates:
#>  Parameter Estimate Std.Error
#>         F0    0.198     0.073
#>       G0_0    0.990     0.000
#>       G0_1    0.080     0.076
#>         v0    1.598     0.171
#>       eps0    0.007     0.001
#> 
```
