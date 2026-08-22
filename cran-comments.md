## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a package update (previous version 0.1.1 is on CRAN).

* checking installed package size ... NOTE
  Installed size is 5.2Mb, with 3.0Mb in `exdata`. This is an example
  real-time dataset (Swiss GDP release vintages) used in the package
  vignettes and documentation.

* checking for future file timestamps ... NOTE
  "unable to verify current time" -- this is an artifact of the local
  check environment (no access to an external time server) and not
  related to the package.

* checking HTML version of manual ... NOTE
  "Error: <main> is not recognized!" -- this is a false positive caused
  by an outdated local `tidy` binary (HTML Tidy for Mac OS X, 2006
  build) that predates HTML5 and does not recognize the `<main>`
  element used by R's own Rd2HTML output. Not reproducible on
  win-builder / R-hub / CRAN's check machines, which use a current
  `tidy`.
