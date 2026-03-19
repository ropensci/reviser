# Vintages Data

A collection of real-time datasets.

## Usage

``` r
gdp
```

## Format

A tibble with quarterly observations and 4 variables:

- time:

  Date of the observation

- pub_date:

  Publication date of the vintage

- value:

  Numeric, real GDP (seasonally adjusted)

- id:

  Country code

## Details

- GDP: Quarterly Vintages (Billions of real dollars, seasonally
  adjusted)

- Timeframe: Q1 1980 - Q4 2024

- Real-Time Vintages: Q4 2002 - Q4 2024

## Sources

- All the data is from the realtime database of Indergand and Leist
  (2014). **Countries**:

- CHE:

  - Switzerland

  - Source: SECO

- US:

  - United States

  - Sources: FRED, OECD

- EA:

  - Euro Area

  - Sources: Eurostat, OECD

- JP:

  - Japan

  - Sources: Cabinet Office (Japan), OECD

## References

Indergand, R., Leist, S. A Real-Time Data Set for Switzerland. Swiss J
Economics Statistics 150, 331–352 (2014).
[doi:10.1007/BF03399410](https://doi.org/10.1007/BF03399410)

## Examples

``` r
# Load gdp dataset
data(gdp)
head(gdp)
#> # A tibble: 6 × 4
#>   time       pub_date    value id   
#>   <date>     <date>      <dbl> <chr>
#> 1 1980-01-01 2002-10-01 64551. CHE  
#> 2 1980-01-01 2003-01-01 64551. CHE  
#> 3 1980-01-01 2003-04-01 64556. CHE  
#> 4 1980-01-01 2003-07-01 64551. CHE  
#> 5 1980-01-01 2003-10-01 64551. CHE  
#> 6 1980-01-01 2004-04-01 75004. CHE  
```
