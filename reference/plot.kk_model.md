# Plot Kishor-Koenig Model Results

Plot filtered or smoothed estimates for a selected state from a fitted
`kk_model`.

## Usage

``` r
# S3 method for class 'kk_model'
plot(x, state = NULL, type = "filtered", ...)
```

## Arguments

- x:

  An object of class `kk_model`.

- state:

  Character scalar giving the state to visualize. If `NULL`, the first
  available state is used.

- type:

  Character scalar indicating whether `"filtered"` or `"smoothed"`
  estimates should be plotted.

- ...:

  Additional arguments passed to `plot.revision_model()`.

## Value

A `ggplot2` object visualizing the specified state estimates.

## Details

This method requires `x$states` to be available. If the model was fitted
with `solver_options$return_states = FALSE`, plotting is not possible.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`print.kk_model()`](https://p-wegmueller.github.io/reviser/reference/print.kk_model.md),
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

e <- 1 # Number of efficient release
h <- 2 # Forecast horizon
result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")

plot(result)

```
