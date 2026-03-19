# Revision Analysis Summary Statistics

Calculates a comprehensive set of summary statistics and hypothesis
tests for revisions between initial and final data releases.

## Usage

``` r
get_revision_analysis(df, final_release, degree = 1, grouping_var = NULL)
```

## Arguments

- df:

  A data frame containing the initial data releases. Must include
  columns:

  - `time`: The time variable.

  - `value`: The observed values in the initial release.

  - Optionally, `release` (release identifier) and `id` (grouping
    variable).

- final_release:

  A data frame containing the final release data. Must include columns:

  - `time`: The time variable (matching the initial release data).

  - `value`: The observed values in the final release.

- degree:

  An integer between 1 and 5 specifying the level of detail for the
  output: 1: Default, includes information about revision size. 2:
  includes correlation statistics of revision. 3: includes news and
  noise tests. 4: includes sign switches, seasonality analysis and
  Theil's U. 5: Full set of all statistics and tests.

- grouping_var:

  A character string specifying the grouping variable in the data frame.
  Defaults to `"pub_date"` or `"release"` if available.

## Value

A data frame with one row per grouping (if applicable) and columns for
summary statistics and test results. The resulting data frame is of
class `revision_summary`.

## Details

This function performs a variety of statistical analyses to understand
the nature of revisions between the initial and final data releases. The
function:

- Checks the input data for consistency and transforms it as necessary.

- Merges the initial and final release datasets by their time variable
  and optional grouping variables (`id` or `release`).

- Computes summary statistics such as the mean, standard deviation, and
  range of the revisions.

- Performs hypothesis tests for bias, efficiency, and correlation using
  robust methods (e.g., Newey-West standard errors).

- Includes tests for seasonality, noise, and news effects.

Key tests include:

- **Bias Tests**: Tests for the presence of mean bias and regression
  bias.

- **Autocorrelation and Seasonality**: Tests for serial correlation and
  seasonal patterns in revisions.

- **Theil's U Statistics**: Measures predictive accuracy of the initial
  releases relative to the final values.

- **Noise vs. News**: Differentiates between unpredictable errors
  (noise) and systematic adjustments (news).

The function supports grouped calculations based on the presence of `id`
or `release` columns in the input.

The following statistics and tests are calculated (See the vignette
[`vignette("revision-analysis")`](https://p-wegmueller.github.io/reviser/articles/revision-analysis.md)
for more details):

- **N**: The number of observations in the group.

- **Frequency**: The inferred data frequency (e.g., 12 for monthly or 4
  for quarterly data).

- **Bias (mean)**: The mean revision, testing whether revisions are
  systematically biased.

- **Bias (p-value)**: p-value from a t-test evaluating the significance
  of the mean revision.

- **Bias (robust p-value)**: Newey-West HAC robust p-value for the mean
  revision test.

- **Minimum**: The minimum revision in the group.

- **Maximum**: The maximum revision in the group.

- **10Q**: The 10th percentile revision.

- **Median**: The median revision.

- **90Q**: The 90th percentile revision.

- **MAR**: The mean absolute revision.

- **Std. Dev.**: The standard deviation of revisions, indicating their
  variability.

- **Noise/Signal**: The ratio of the standard deviation of revisions to
  the standard deviation of final values.

- **Correlation**: The Pearson correlation between revisions and initial
  values, testing the relationship.

- **Correlation (p-value)**: p-value for the significance of the
  correlation.

- **Autocorrelation (1st)**: The first-order autocorrelation of
  revisions, measuring persistence.

- **Autocorrelation (1st p-value)**: p-value for the first-order
  autocorrelation test.

- **Autocorrelation up to 1yr (Ljung-Box p-value)**: p-value for the
  Ljung-Box test for higher-order autocorrelation.

- **Theil's U1**: A normalized measure of forecast accuracy, comparing
  the root mean squared error (RMSE) of revisions to the RMSE of final
  and initial values.

- **Theil's U2**: Compares forecast changes to actual changes.

- **Seasonality (Friedman p-value)**: p-value from the Friedman test for
  seasonality in revisions.

- **News joint test (p-value)**: p-value for the joint news test.

- **News test Intercept**: The estimated intercept from the news test
  regression.

- **News test Intercept (std.err)**: The standard error of the intercept
  in the news test regression.

- **News test Intercept (p-value)**: p-value for the intercept in the
  news test regression.

- **News test Coefficient**: The estimated coefficient for the `value`
  in the news test regression.

- **News test Coefficient (std.err)**: The standard error of the
  coefficient in the news test regression.

- **News test Coefficient (p-value)**: p-value for the coefficient in
  the news test regression.

- **Noise joint test (p-value)**: p-value for the joint noise test.

- **Noise test Intercept**: The estimated intercept from the noise test
  regression.

- **Noise test Intercept (std.err)**: The standard error of the
  intercept in the noise test regression.

- **Noise test Intercept (p-value)**: p-value for the intercept in the
  noise test regression.

- **Noise test Coefficient**: The estimated coefficient for the
  `final_value` in the noise test regression.

- **Noise test Coefficient (std.err)**: The standard error of the
  coefficient in the noise test regression.

- **Noise test Coefficient (p-value)**: p-value for the coefficient in
  the noise test regression.

- **Fraction of correct sign**: The fraction of correct sign changes in
  revisions.

- **Fraction of correct growth rate change**: The fraction of correct
  sign changes of growth rates in revisions.

## See also

Other revision analysis:
[`diagnose()`](https://p-wegmueller.github.io/reviser/reference/diagnose.md),
[`diagnose.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/diagnose.revision_summary.md),
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md),
[`print.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md),
[`print.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md),
[`summary.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)

## Examples

``` r
# Example usage:
df_small <- dplyr::filter(
  reviser::gdp,
  id == "US",
  time >= as.Date("2018-01-01")
)

df <- dplyr::select(
  get_nth_release(df_small, n = 0:2),
  -"pub_date"
)

final_release <- dplyr::select(
  get_nth_release(df_small, n = "latest"),
  -"pub_date"
)

results <- get_revision_analysis(
  df,
  final_release
)
```
