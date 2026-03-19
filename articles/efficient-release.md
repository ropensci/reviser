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
efficient release in quarterly GDP data for Switzerland, the Euro Area,
Japan, and the United States. We test the first 20 data releases, and we
aim to determine the point at which further revisions become negligible.
As the final release, we use the same release for all countries, namely
the data released 5 years after the first release. The tests shows that
the first efficient release occurs at different points for each country,
ranging from the 1st (Japan, US) to the 13th (Switzerland) release.

``` r
library(reviser)
library(dplyr)

gdp <- reviser::gdp |>
  tsbox::ts_pc()

df <- get_nth_release(gdp, n = 0:19)

final_release <- get_nth_release(gdp, n = 20)

efficient <- get_first_efficient_release(
  df,
  final_release
)

res <- summary(efficient)
#> id:  CHE 
#> Efficient release:  13 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.83040 -0.13561 -0.00657  0.15531  0.76670 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.05979    0.02526   2.367   0.0192 *  
#> release_13   0.87766    0.03326  26.384   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2631 on 155 degrees of freedom
#>   (21 observations deleted due to missingness)
#> Multiple R-squared:  0.8179, Adjusted R-squared:  0.8167 
#> F-statistic: 696.1 on 1 and 155 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_13 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_13
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    157                    
#> 2    155  2 2.9999 0.05269 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> id:  EA 
#> Efficient release:  7 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.39144 -0.06507  0.00536  0.07090  0.25743 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.01982    0.01113   1.781   0.0768 .  
#> release_7    1.00311    0.01646  60.934   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.1111 on 156 degrees of freedom
#>   (20 observations deleted due to missingness)
#> Multiple R-squared:  0.9597, Adjusted R-squared:  0.9594 
#> F-statistic:  3713 on 1 and 156 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_7 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_7
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    158                    
#> 2    156  2 3.0071 0.05231 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> id:  JP 
#> Efficient release:  0 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.76370 -0.36739 -0.01023  0.38499  1.72906 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.05437    0.05116   1.063     0.29    
#> release_0    0.83179    0.04878  17.051   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.5765 on 156 degrees of freedom
#>   (20 observations deleted due to missingness)
#> Multiple R-squared:  0.6508, Adjusted R-squared:  0.6486 
#> F-statistic: 290.7 on 1 and 156 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_0 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_0
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    158                 
#> 2    156  2 1.8585 0.1593
#> 
#> 
#> id:  US 
#> Efficient release:  0 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.82880 -0.13760  0.03239  0.12910  0.95887 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 0.005432   0.028360   0.192    0.848    
#> release_0   0.955730   0.029993  31.865   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2564 on 156 degrees of freedom
#>   (20 observations deleted due to missingness)
#> Multiple R-squared:  0.8668, Adjusted R-squared:  0.866 
#> F-statistic:  1015 on 1 and 156 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_0 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_0
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    158                 
#> 2    156  2 2.2919 0.1045

head(res)
#> # A tibble: 4 × 6
#>   id        e   alpha  beta p_value n_tested
#>   <chr> <dbl>   <dbl> <dbl>   <dbl>    <int>
#> 1 CHE      13 0.0598  0.878  0.0527       14
#> 2 EA        7 0.0198  1.00   0.0523        8
#> 3 JP        0 0.0544  0.832  0.159         1
#> 4 US        0 0.00543 0.956  0.104         1
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
to improve nowcasts of preliminary releases (See vignette [Nowcasting
Revisions](https://p-wegmueller.github.io/reviser/articles/nowcasting-revisions.md)
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
#> 1 CHE   release_0    158                 0.18                            0.054
#> 2 CHE   release_1    158                 0.163                           0.054
#> 3 CHE   release_10   157                 0.128                           0.047
#> 4 CHE   release_11   158                 0.126                           0.047
#> 5 CHE   release_12   157                 0.092                           0.041
#> 6 CHE   release_13   157                 0.06                            0.029
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
#> id=CHE, release=release_0:
#>   • Revisions contain NEWS (p = 0.004 ): systematic information
#>   • Revisions contain NOISE (p = 0 ): measurement error
#> 
#> id=CHE, release=release_1:
#>   • Revisions contain NEWS (p = 0.011 ): systematic information
#>   • Revisions contain NOISE (p = 0 ): measurement error
#> 
#> id=CHE, release=release_10:
#>   • Revisions contain NEWS (p = 0.003 ): systematic information
#>   • Revisions contain NOISE (p = 0 ): measurement error
#> 
#> id=CHE, release=release_11:
#>   • Revisions contain NEWS (p = 0.003 ): systematic information
#>   • Revisions contain NOISE (p = 0 ): measurement error
#> 
#> id=CHE, release=release_12:
#>   • Revisions contain NEWS (p = 0.017 ): systematic information
#>   • Revisions contain NOISE (p = 0.001 ): measurement error
#> 
#> id=CHE, release=release_13:
#>   • Revisions do NOT contain news (p = 0.053 )
#>   • Revisions do NOT contain noise (p = 0.121 )
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
