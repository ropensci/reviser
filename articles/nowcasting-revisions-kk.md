# Nowcasting revisions using the generalized Kishor-Koenig model

Having some evidence that the revisions are predictable, we can now move
to the next step: **nowcasting**. Nowcasting refers to the process of
estimating the current state of the economy using timely indicators. In
the context of revisions, nowcasting involves predicting the final (or
efficient) value of an economic variable before all revisions are
available. This vignette demonstrates how to implement a nowcasting
model using the generalized Kishor-Koenig (KK) framework ([Kishor and
Koenig 2012](#ref-kishorVAREstimationForecasting2012)).

## The Generalized Kishor-Koenig Model

The KK model extends the traditional Vector Autoregression (VAR)
framework to account for data revisions explicitly. Unlike conventional
models that treat data as final and accurate, the KK model recognizes
that initial releases are subject to revisions, leading to biased and
inefficient forecasts. By incorporating the revision process into a
state-space framework, the KK model provides a reliable approach to
nowcasting economic variables. Moreover, it nests several nowcasting
models, such as the classical textbook measurement error model or the
Howrey ([1978](#ref-howreyUsePreliminaryData1978)) model.

The procedure is based on the assumption that there exists an efficient
estimate $y_{t}^{e}$ of the final release that becomes available $e$
periods after the initial release $y_{t}^{0}$. We show in vignette
[Efficient Release
Identification](https://p-wegmueller.github.io/reviser/articles/efficient-release.md)
how the number of the efficient release can be empirically tested. We
follow Strohsal and Wolf ([2020](#ref-strohsalDataRevisionsGerman2020))
and assume that $e$th revision data follows an autoregressive model of
order one (AR(1)). The KK model is represented by the following
equations:

**1. The State Equation** $$z_{t} = Fz_{t - 1} + \nu_{t}$$

**2. The Observation Equation**
$$y_{t} = (I - G)Fy_{t - 1} + Gz_{t} + \epsilon_{t}$$

where $$z\prime_{t} = \begin{bmatrix}
{y_{t - e}^{e},y_{t - e + 1}^{e},...,y_{t}^{e}}
\end{bmatrix}\prime$$$$y\prime_{t} = \begin{bmatrix}
{y_{t - e}^{e},y_{t - e + 1}^{e - 1},...,y_{t}^{0}}
\end{bmatrix}\prime$$$$\nu\prime_{t} = \begin{bmatrix}
{0,0,...,\nu_{0,t}}
\end{bmatrix}\prime$$$$\epsilon\prime_{t} = \begin{bmatrix}
{0,\epsilon_{e - 1,t},...,\epsilon_{1,t},\epsilon_{0,t}}
\end{bmatrix}\prime$$$$F = \begin{bmatrix}
0 & 1 & 0 & \ldots & 0 \\
0 & 0 & 1 & \ldots & 0 \\
\vdots & \vdots & \vdots & \ddots & \vdots \\
0 & 0 & 0 & \ldots & 1 \\
0 & 0 & 0 & \ldots & F_{0}
\end{bmatrix}$$$$G = \begin{bmatrix}
1 & 0 & \ldots & 0 \\
G_{e - 1,e} & G_{e - 1,e - 1} & \ldots & G_{e - 1,0} \\
G_{e - 2,e} & G_{e - 2,e - 1} & \ldots & G_{e - 2,0} \\
\vdots & \vdots & \ddots & \vdots \\
G_{0,e} & G_{0,e - 1} & \ldots & G_{0,0}
\end{bmatrix}$$$G$ is a gain matrix capturing the weight placed on new
information, $F$ determines the AR coefficient, and $\nu_{t}$ and
$\epsilon_{t}$ are error vectors. The state-equation and
observation-equation error vectors are assumed to be uncorrelated with
one another at all leads and lags, and to be serially uncorrelated.

Because $\nu_{t}$ and $\epsilon_{t}$ are serially uncorrelated, and
uncorrelated with one another at all leads and lags, estimation of the
model equations is unproblematic apart from the crossequation
restrictions on $F$. Therefore, it is recommended to estimate this
system using seemingly unrelated regression (SUR). The SUR estimation is
based on the \[systemfit::nlsystemfit()\] function. The parameter space
of the KK model increases exponentially with the number of the efficient
release. This leads to long computation times and potential convergence
issues. As a fast, but potentially biased alternative, the package also
allows to estimate the models parameters equation by equation using
ordinary least squares (OLS).

### Howrey and Classical Measurement Error Model

It is easy to see that the KK model nests the classical measurement
error model and the Howrey ([1978](#ref-howreyUsePreliminaryData1978))
model. measurement error model treats published data ($y_{t}$) as a
noisy measure of the true economic variable ($z_{t}$), where
$y_{t} = z_{t} + \eta_{t}$. It addresses uncertainty in interpreting
recent data by optimally updating forecasts, considering both potential
economic shocks and measurement errors. This model is obtained by
setting $G = I$.

As an extension to the classical measurement error mode, Howrey
([1978](#ref-howreyUsePreliminaryData1978)) proposed a model that allows
the revisions to be autocorrelated. Setting the entries
$G_{e - i,0} = 0$ for $i = 1,...,e - 1$ and $G_{0,0} = 1$ results in the
Howrey model.

### State Space Representation

Then, armed with estimates of F and K, one can apply the Kalman filter
to the following state-space model (notation following Durbin and
Koopman ([2012](#ref-durbinTimeSeriesAnalysis2012)))

**1. The Observation Equation**
$$y_{t} = Z\alpha_{t} + \varepsilon_{t}$$

where \$\varepsilon_t \dist N(0,H)\$. Durbin and Koopman
([2012](#ref-durbinTimeSeriesAnalysis2012)) assumed that both error
terms are iid and orthogonal to one another. However, we write the model
in the form where $\varepsilon_{t}$, and therefore also $H$ are equal to
zero and may be omitted, so this equation becomes

$$y_{t} = Z\alpha_{t}$$

**2. The State Equation** $$\alpha_{t + 1} = T\alpha_{t} + R\eta_{t}$$

where $$\alpha_{t} = \begin{bmatrix}
z_{t} \\
{y_{t} - z_{t}}
\end{bmatrix}$$$$Z = \begin{bmatrix}
I & I
\end{bmatrix}$$$$T = \begin{bmatrix}
F & 0 \\
0 & {(I - G)F}
\end{bmatrix}$$$$R = I$$$$\eta_{t} = \begin{bmatrix}
\nu_{t} \\
{\epsilon_{t} - (I - G)\nu_{t}}
\end{bmatrix}$$$$\eta_{t} \sim N(0,Q)$$

The analyst sees truth and the revisions. Both truth and revisions are
included in the state vector. Importantly, when calculating the gain
matrix, the model requires that one allow for the possibility that the
off-diagonal matrix elements of
$Q \equiv E\left\lbrack \eta_{t}\eta\prime_{t} \right\rbrack$ are
nonzero. In particular, our model implies that $$Q = \begin{bmatrix}
{\nu_{t}\nu\prime_{t}} & {- \nu_{t}\nu\prime_{t}(I - G)\prime} \\
{- (I - G)\nu_{t}\nu\prime_{t}} & {\varepsilon_{t}\varepsilon\prime_{t} + (I - G)\nu_{t}\nu\prime_{t}(I - G)\prime}
\end{bmatrix}$$

### Forecasting

Kalman filter forecasts are generated by projecting the estimated state
vector forward in time using the model’s estimated transition equation
$\langle T\rangle$. Given the Kalman filter’s state estimate
$\langle\alpha_{t}\rangle_{T}$ at time $T$, forecasts for future periods
$T + i$ are calculated as:

$$\langle\alpha_{T + i}\rangle_{T} = \langle T\rangle_{T}^{i}\langle\alpha_{T}\rangle_{T}$$

### Nowcasting using the `reviser` package

As a demonstration, we nowcast the Euro Area GDP using the KK model. We
use the
[`reviser::gdp`](https://p-wegmueller.github.io/reviser/reference/gdp.md)
dataset, which contains the Euro Area GDP data. As a first step we test
which release (among the first 15) is efficient. As final release, we
use the value published 4 years after the initial value, allowing for a
4-year revision period. It turns out that $e = 2$ meaning that the third
release is an efficient estimate of the final release.

``` r
library(reviser)
library(magrittr)
library(dplyr)
library(lubridate)

gdp <- reviser::gdp %>%
  tsbox::ts_pc() %>%
    dplyr::filter(id == "EA",
      time >= min(pub_date),
      time <= as.Date("2020-01-01")
  ) %>%
  tidyr::drop_na()

df <- get_nth_release(gdp, n = 0:14)

final_release <- get_nth_release(gdp, n = 15)

efficient_release <- get_first_efficient_release(
    df,
    final_release
  )

data <- efficient_release$data
e <- efficient_release$e

summary(efficient_release)
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
```

We then use the
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md)
function to nowcast the Euro Area GDP using the KK model. The function
requires a `df` with columns `time`, `value`, `release#` and `final`.
The function
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md)
returns a dataset in this format.

``` r
nowcast <- kk_nowcast(
    df = data,
    e = e,
    model = "KK",
    method = "MLE"
  )

# Estimated parameters
nowcast$params
#>    Parameter     Estimate    Std.Error
#> 1         F0  0.632965129 0.1313810220
#> 2       G0_0  0.950076221 0.0109883048
#> 3       G0_1 -0.036659893 0.0915496644
#> 4       G0_2 -0.180788245 0.0612311984
#> 5       G1_0 -0.008913728 0.0308311086
#> 6       G1_1  0.594486477 0.2201158840
#> 7       G1_2  0.194005618 0.1517555758
#> 8         v0  0.379874535 0.0683792003
#> 9       eps0  0.007785909 0.0002435071
#> 10      eps1  0.001397110 0.0014196291

# Filtered states
filt_states <- nowcast$states
tail(filt_states)
#> # A tibble: 6 × 7
#>   time       state           estimate lower upper filter   sample   
#>   <date>     <chr>              <dbl> <dbl> <dbl> <chr>    <chr>    
#> 1 2018-10-01 release_2_lag_2    0.414 0.412 0.416 smoothed in_sample
#> 2 2019-01-01 release_2_lag_2    0.136 0.134 0.138 smoothed in_sample
#> 3 2019-04-01 release_2_lag_2    0.307 0.305 0.309 smoothed in_sample
#> 4 2019-07-01 release_2_lag_2    0.441 0.439 0.443 smoothed in_sample
#> 5 2019-10-01 release_2_lag_2    0.147 0.146 0.149 smoothed in_sample
#> 6 2020-01-01 release_2_lag_2    0.299 0.297 0.301 smoothed in_sample
```

Now we can plot the forecast.

### References

Durbin, James, and Siem Jan Koopman. 2012. *Time Series Analysis by
State Space Methods: Second Edition*. Oxford University Press.
<https://doi.org/10.1093/acprof:oso/9780199641178.001.0001>.

Howrey, E. Philip. 1978. “The Use of Preliminary Data in Econometric
Forecasting.” *The Review of Economics and Statistics* 60 (2): 193.
<https://doi.org/10.2307/1924972>.

Kishor, N. Kundan, and Evan F. Koenig. 2012. “VAR Estimation and
Forecasting When Data Are Subject to Revision.” *Journal of Business and
Economic Statistics*, 1–10. <https://doi.org/10.1198/jbes.2010.08169>.

Strohsal, Till, and Elias Wolf. 2020. “Data Revisions to German National
Accounts: Are Initial Releases Good Nowcasts?” *International Journal of
Forecasting* 36 (4): 1252–59.
<https://doi.org/10.1016/j.ijforecast.2019.12.006>.
