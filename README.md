# dqguard

Public source variant of the dqguard R package.

Provides generic data-quality contracts, guarded checks, survey diagnostics and issue-resolution functions. Client-specific monitoring modules and project catalogues are excluded.

## Installation

Install with `remotes::install_github("GeoffreyManda/dqguard-public")`, or use `R CMD INSTALL` on the source after installing the dependencies declared in `DESCRIPTION`.

## Licence and credits

The original licence and contributor notices are retained in the package metadata and licence files.

## Validation

The public core test suite passed on 24 November 2026. Two tests were skipped: one requires the optional validate package, and one applies to the excluded client-derived forensic catalogue.
