# Extract the Nth Data Release (Vintage)

Filters the input dataset to return the Nth release (or vintage) of data
for each time period. The function supports selecting the first, latest,
or a specific numbered release.

## Usage

``` r
get_nth_release(df, n = 0, diagonal = FALSE)
```

## Arguments

- df:

  A data frame containing data vintages. The data frame must include the
  columns:

  - `pub_date` (publication date of the release)

  - `time` (thecorresponding time period for the data).

- n:

  The release number to extract. Accepts:

  - Non-negative integer or vector (e.g., 0 for first release, 1 for
    second, etc.)

  - `"first"` to extract the first release.

  - `"latest"` to extract the most recent release. Default is 0 (the
    first release).

- diagonal:

  Logical. If `TRUE`, the function only returns real first releases.

## Value

A filtered data frame containing only the specified release(s). The
resulting data frame is assigned the class `tbl_release` to indicate its
structure. If diagonal is set to `TRUE`, the function only returns the
real first releases. That is historic values for which no vintages exist
are not returned.

## Details

The behavior depends on the value of `n`:

- **Non-negative integer**: The function retrieves the Nth release for
  each time period (e.g., 0 = first release, 1 = second release, etc.).

- **"first"**: Retrieves the first release for each time period (via
  `get_first_release`).

- **"latest"**: Retrieves the most recent release for each time period
  (via `get_latest_release`).

## See also

Other revision utilities:
[`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md),
[`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md),
[`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md),
[`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md),
[`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md),
[`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)

## Examples

``` r
# Example data
df <- dplyr::filter(reviser::gdp, id == "US")

# Get the first release (n = 0)
first_release <- get_nth_release(df, n = 0)

# Get the latest release
latest_release <- get_nth_release(df, n = "latest")

# Get the second release (n = 1)
second_release <- get_nth_release(df, n = 1)

# Get the first and second release (n = 0:1)
releases <- get_nth_release(df, n = 0:1)
```
