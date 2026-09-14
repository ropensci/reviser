## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a package update (previous version 0.3.0 is on CRAN, published
  2026-09-02).

* This update removes the dependency on `calculus`, which the CRAN team has
  scheduled for archival on 2026-10-05 due to unresolved check problems,
  and asked us to address as a strong reverse dependency. `calculus` was
  used only for four small symbolic-algebra operators in one internal
  function; they have been replaced with equivalent internal helpers, with
  no change to any user-facing behavior.

* checking CRAN incoming feasibility ... NOTE
  "Days since last update". This release follows the previous one by less
  than two weeks because it responds to the CRAN team's notice that
  `calculus` -- a strong dependency of this package -- is scheduled for
  archival on 2026-10-05, and asked maintainers to negotiate the necessary
  actions. This update removes that dependency ahead of the deadline. We do
  not expect a further update soon.

* checking installed package size ... NOTE
  Installed size is 5.1Mb, with 3.0Mb in `exdata`. This is an example
  real-time dataset (Swiss GDP release vintages) used in the package
  vignettes and documentation.

* checking for future file timestamps ... NOTE
  "unable to verify current time" -- this is an artifact of the local
  check environment (no access to an external time server) and not
  related to the package.

## Reverse dependencies

There are no reverse dependencies on CRAN.
