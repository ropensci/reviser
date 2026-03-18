# Plot JVN model results

Plot filtered or smoothed estimates for a selected state from a fitted
`jvn_model`.

## Usage

``` r
# S3 method for class 'jvn_model'
plot(x, state = "true_lag_0", type = "filtered", ...)
```

## Arguments

- x:

  An object of class `jvn_model`.

- state:

  Character scalar giving the state to visualize.

- type:

  Character scalar indicating whether `"filtered"` or `"smoothed"`
  estimates should be plotted.

- ...:

  Additional arguments passed to `plot.revision_model()`.

## Value

A `ggplot2` object.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
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
plot(result)

# }
```
