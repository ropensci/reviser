# Identifying an Efficient Release in Data Subject to Revisions

Macroeconomic data are often subject to revisions, reflecting the
integration of new information, methodological improvements, and
statistical adjustments. Understanding the optimal properties of a
revision process is essential to ensure that early data releases provide
a reliable foundation for policy decisions and economic forecasting.
This vignette discusses the characteristics of optimal revisions,
formalizes initial estimates as truth measured with noise, and presents
an iterative approach to identify the point at which revisions become
unpredictable also known as the efficient release. Further, examples are
provided to illustrate the application of these concepts to GDP data.

### Optimal Properties of Revisions

Data revisions can be classified into two main types: ongoing revisions,
which incorporate new information as it becomes available, and benchmark
revisions, which reflect changes in definitions, classifications, or
methodologies. Optimally, revisions should satisfy the following
properties ([Aruoba 2008](#ref-aruobaDataRevisionsAre2008)):

- **Unbiasedness**: Revisions should not systematically push estimates
  in one direction. Mathematically, for a revision series
  $r_{t}^{f} = y_{t}^{f} - y_{t}^{h}$ where $h$ denotes the release
  number $y_{t}^{f}$ represents the final release and $y_{t}^{h}$
  represents the series released at time $h$ ($h = 0$ is the initial
  release), unbiasedness requires:
  $$E\left\lbrack r_{t}^{f} \right\rbrack = 0$$

- **Efficiency**: An efficient release incorporates all available
  information optimally, ensuring that subsequent revisions are
  unpredictable. This can be tested using the Mincer-Zarnowitz
  regression:
  $$y_{t}^{f} = \alpha + \beta y_{t}^{h} + \varepsilon_{t},$$ where an
  efficient release satisfies $\alpha = 0$ and $\beta = 1$.

- **Minimal Variance**: Revisions should be as small as possible while
  still improving accuracy. The variance of revisions, defined as
  $\text{Var}\left( r_{t}^{f} \right)$, should decrease over successive
  releases.

### Initial Estimates as Truth Measured with Noise

A fundamental assumption in many revision models is that the first
release of a data point is an imperfect measure of the true value due to
measurement errors. This can be expressed as:
$$y_{t}^{h} = y_{t}^{*} + \varepsilon_{t}^{h},$$ where $y_{t}^{*}$ is
the true value and $\varepsilon_{t}^{h}$ is an error term that
diminishes as $h$ increases. The properties of $r_{t}^{h}$ determine
whether the preliminary release $h$ is an efficient estimator of
$y_{t}^{*}$. If $r_{t}^{h}$ is predictable, the revision process is
inefficient, indicating room for improvement in the initial estimates.
Vice-versa if $r_{t}^{h}$ is unpredictable, the preliminary release is
efficient (i.e $y_{t}^{h} = y_{t}^{e}$).

### The Challenge of Defining a Final Release

One major challenge in identifying an efficient release using
Mincer-Zarnowitz regressions is that it is often unclear which data
release should be considered final. While some revisions occur due to
the incorporation of new information, others result from methodological
changes that redefine past values. Consequently, statistical agencies
may continue revising data for years, making it difficult to pinpoint a
definitive final release. For instance in Germany the final release of
national accounts data is typically published four years after the
initial release. In Switzerland, GDP figures are never finalized. So,
defining a final release is a non-trivial task that requires knowledge
of the revision process.

### Iterative Approach for Identifying $e$

An iterative approach is proposed to determine the optimal number of
revisions, $e$, beyond which further revisions are negligible. This
approach is based on running Mincer-Zarnowitz-style regressions to
assess which release provides an optimal estimate of the final value.
The procedure follows these steps ([Kishor and Koenig
2012](#ref-kishorVAREstimationForecasting2012); [Strohsal and Wolf
2020](#ref-strohsalDataRevisionsGerman2020)):

1.  **Regression Analysis**: Regress the final release $y_{t}^{f}$ on
    the initial releases $y_{t}^{h}$ for $h = 1,2,\ldots,H$:
    $$y_{t}^{f} = \alpha + \beta y_{t}^{h} + \varepsilon_{t}$$ where the
    null hypothesis is that $\alpha = 0$ and $\beta = 1$, indicating
    that $y_{t}^{h}$ is an efficient estimate of $y_{t}^{f}$.

2.  **Determine the Optimal $e$**: Increase $h$ iteratively and test
    whether the efficiency conditions hold. The smallest $e$ for which
    the hypothesis is not rejected is considered the final efficient
    release.

### Importance of an Efficient Release

An efficient release is essential for various economic applications:

- **Macroeconomic Policy Decisions**: Monetary and fiscal policies
  depend on timely and reliable data. If initial releases are biased or
  predictable, policymakers may react inappropriately, leading to
  suboptimal policy outcomes.
- **Nowcasting and Forecasting**: Reliable early estimates improve the
  accuracy of nowcasting models, which are used for real-time assessment
  of economic conditions.
- **Financial Market Stability**: Investors base decisions on official
  economic data. If initial releases are misleading, market volatility
  may increase due to unexpected revisions.
- **International Comparisons**: Cross-country analyses require
  consistent data. If national statistical agencies release inefficient
  estimates, international comparisons become distorted.

An optimal revision process is one that leads to unbiased, efficient,
and minimally variant data revisions. Initial estimates represent the
truth measured with noise, and identifying the efficient release allows
analysts to determine the earliest point at which data can be used
reliably without concern for systematic revisions. The iterative
approach based on Mincer-Zarnowitz regressions provides a robust
framework for achieving this goal, improving the reliability of
macroeconomic data for forecasting and policy analysis.

### Example: Identifying an Efficient Release in GDP Data with `reviser`

In the following example, we use the `reviser` package to identify the
efficient release in quarterly Euro Area GDP data. We test the first 15
data releases and use the 16th release as the benchmark final release.
The sample is restricted to a single series and a shorter time span to
keep the vignette lightweight while preserving a non-trivial
efficient-release result.

``` r
library(reviser)
library(dplyr)

gdp <- reviser::gdp |>
  tsbox::ts_pc() |>
  dplyr::filter(
    id == "EA",
    time >= min(pub_date),
    time <= as.Date("2020-01-01")
  ) |>
  tidyr::drop_na()

df <- get_nth_release(gdp, n = 0:14)

final_release <- get_nth_release(gdp, n = 15)

efficient <- get_first_efficient_release(
  df,
  final_release
)

res <- summary(efficient)
#> Efficient release:  2 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.34873 -0.08185 -0.00706  0.10475  0.31533 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.03276    0.01775   1.846   0.0692 .  
#> release_2    1.01446    0.02440  41.577   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.1428 on 68 degrees of freedom
#> Multiple R-squared:  0.9622, Adjusted R-squared:  0.9616 
#> F-statistic:  1729 on 1 and 68 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_2 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_2
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df     F  Pr(>F)  
#> 1     70                   
#> 2     68  2 2.743 0.07151 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

head(res)
#> # A tibble: 1 × 5
#>       e  alpha  beta p_value n_tested
#>   <dbl>  <dbl> <dbl>   <dbl>    <int>
#> 1     2 0.0328  1.01  0.0715        3
```

Note: The identification of the first efficient release is related to
the news hypothesis testing. Hence, the same conclusion could be reached
by using the function
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md)
(setting `degree=3`, providing results for news and noise tests) as
shown below. An advantage of using the
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md)
function is that it organizes the data to be subsequently used in
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md)
and
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md)
to improve nowcasts of preliminary releases (See vignettes [Nowcasting
revisions using the generalized Kishor-Koenig
family](https://p-wegmueller.github.io/reviser/articles/nowcasting-revisions-kk.md)
and [Nowcasting revisions using the Jacobs-Van Norden
model](https://p-wegmueller.github.io/reviser/articles/nowcasting-revisions-jvn.md)
for more details).

``` r
analysis <- get_revision_analysis(
  df,
  final_release,
  degree = 3
)
head(analysis)
#> 
#> === Revision Analysis Summary ===
#> 
#> # A tibble: 6 × 17
#>   id    release        N `News test Intercept` `News test Intercept (std.err)`
#>   <chr> <chr>      <dbl>                 <dbl>                           <dbl>
#> 1 EA    release_0     70                 0.043                           0.03 
#> 2 EA    release_1     70                 0.041                           0.024
#> 3 EA    release_10    70                 0                               0.008
#> 4 EA    release_11    70                -0.003                           0.006
#> 5 EA    release_12    70                -0.004                           0.007
#> 6 EA    release_13    70                 0.002                           0.005
#> # ℹ 12 more variables: `News test Intercept (p-value)` <dbl>,
#> #   `News test Coefficient` <dbl>, `News test Coefficient (std.err)` <dbl>,
#> #   `News test Coefficient (p-value)` <dbl>, `News joint test (p-value)` <dbl>,
#> #   `Noise test Intercept` <dbl>, `Noise test Intercept (std.err)` <dbl>,
#> #   `Noise test Intercept (p-value)` <dbl>, `Noise test Coefficient` <dbl>,
#> #   `Noise test Coefficient (std.err)` <dbl>,
#> #   `Noise test Coefficient (p-value)` <dbl>, …
#> 
#> === Interpretation ===
#> 
#> id=EA, release=release_0:
#>   • Revisions contain NEWS (p = 0.044 ): systematic information
#>   • Revisions contain NOISE (p = 0.001 ): measurement error
#> 
#> id=EA, release=release_1:
#>   • Revisions contain NEWS (p = 0.042 ): systematic information
#>   • Revisions contain NOISE (p = 0.007 ): measurement error
#> 
#> id=EA, release=release_10:
#>   • Revisions do NOT contain news (p = 0.124 )
#>   • Revisions contain NOISE (p = 0.019 ): measurement error
#> 
#> id=EA, release=release_11:
#>   • Revisions do NOT contain news (p = 0.524 )
#>   • Revisions do NOT contain noise (p = 0.132 )
#> 
#> id=EA, release=release_12:
#>   • Revisions do NOT contain news (p = 0.628 )
#>   • Revisions do NOT contain noise (p = 0.402 )
#> 
#> id=EA, release=release_13:
#>   • Revisions do NOT contain news (p = 0.553 )
#>   • Revisions do NOT contain noise (p = 0.378 )
```

## References

Aruoba, S. Borağan. 2008. “Data Revisions Are Not Well Behaved.”
*Journal of Money, Credit and Banking* 40 (2-3): 319–40.
<https://doi.org/10.1111/j.1538-4616.2008.00115.x>.

Kishor, N. Kundan, and Evan F. Koenig. 2012. “VAR Estimation and
Forecasting When Data Are Subject to Revision.” *Journal of Business and
Economic Statistics*, 1–10. <https://doi.org/10.1198/jbes.2010.08169>.

Strohsal, Till, and Elias Wolf. 2020. “Data Revisions to German National
Accounts: Are Initial Releases Good Nowcasts?” *International Journal of
Forecasting* 36 (4): 1252–59.
<https://doi.org/10.1016/j.ijforecast.2019.12.006>.
