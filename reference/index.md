# Package index

### Revision analysis

Functions for analyzing revisions of time series.

- [`diagnose()`](https://p-wegmueller.github.io/reviser/reference/diagnose.md)
  : Diagnose Revision Quality
- [`diagnose(`*`<revision_summary>`*`)`](https://p-wegmueller.github.io/reviser/reference/diagnose.revision_summary.md)
  : Diagnose Revision Quality
- [`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md)
  : Identify the First Efficient Release in Vintage Data
- [`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md)
  : Revision Analysis Summary Statistics
- [`print(`*`<lst_efficient>`*`)`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md)
  : Print Method for Efficient Release Results
- [`print(`*`<revision_summary>`*`)`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md)
  : Print Method for Revision Summary
- [`summary(`*`<lst_efficient>`*`)`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md)
  : Summary of Efficient Release Models
- [`summary(`*`<revision_summary>`*`)`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)
  : Summary Method for Revision Summary

### Revision utilities

Utility functions for working with revisions of time series.

- [`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md)
  : Calculate the Number of Days Between Period End and First Release
- [`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md)
  : Extract the First Data Release (Vintage)
- [`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md)
  : Extract Vintage Values from a Data Frame
- [`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md)
  : Extract the Latest Data Release (Vintage)
- [`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md)
  : Extract the Nth Data Release (Vintage)
- [`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md)
  : Get Data Releases for a Specific Date
- [`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)
  : Calculate Revisions in Vintage Data

### Revision nowcasting

Functions for nowcasting revisions of time series.

- [`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md)
  : Jacobs-Van Norden model for data revisions
- [`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md)
  : Generalized Kishor-Koenig Model for Nowcasting
- [`plot(`*`<jvn_model>`*`)`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md)
  : Plot JVN model results
- [`plot(`*`<kk_model>`*`)`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md)
  : Plot Kishor-Koenig Model Results
- [`print(`*`<jvn_model>`*`)`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md)
  : Print method for JVN model objects
- [`print(`*`<kk_model>`*`)`](https://p-wegmueller.github.io/reviser/reference/print.kk_model.md)
  : Print Method for KK Model
- [`summary(`*`<jvn_model>`*`)`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md)
  : Summary method for JVN model objects
- [`summary(`*`<kk_model>`*`)`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)
  : Summary Method for KK Model

### Revision graphs

Functions for plotting revisions of time series.

- [`plot(`*`<tbl_pubdate>`*`)`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_pubdate.md)
  : Plot Method for Publication Date Vintages
- [`plot(`*`<tbl_release>`*`)`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_release.md)
  : Plot Method for Release Vintages
- [`plot_vintages()`](https://p-wegmueller.github.io/reviser/reference/plot_vintages.md)
  : Plot Vintages Data
- [`theme_reviser()`](https://p-wegmueller.github.io/reviser/reference/theme_reviser.md)
  [`colors_reviser()`](https://p-wegmueller.github.io/reviser/reference/theme_reviser.md)
  [`scale_color_reviser()`](https://p-wegmueller.github.io/reviser/reference/theme_reviser.md)
  [`scale_fill_reviser()`](https://p-wegmueller.github.io/reviser/reference/theme_reviser.md)
  : Custom Visualization Theme and Color Scales for Reviser

### Helpers

Helper functions to convert revisions to a tidy format.

- [`print(`*`<tbl_pubdate>`*`)`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md)
  : Print Method for Publication Date Vintages
- [`print(`*`<tbl_release>`*`)`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md)
  : Print Method for Release Vintages
- [`summary(`*`<tbl_pubdate>`*`)`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md)
  : Summary Method for Publication Date Vintages
- [`summary(`*`<tbl_release>`*`)`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md)
  : Summary Method for Release Vintages
- [`tbl_sum(`*`<tbl_pubdate>`*`)`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md)
  : Tibble Summary for Publication Date Vintages
- [`tbl_sum(`*`<tbl_release>`*`)`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md)
  : Tibble Summary for Release Vintages
- [`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md)
  : Convert Vintages Data to Long Format
- [`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)
  : Convert Vintages Data to Wide Format

### Data

GDP data used in the package.

- [`gdp`](https://p-wegmueller.github.io/reviser/reference/gdp.md) :
  Vintages Data
