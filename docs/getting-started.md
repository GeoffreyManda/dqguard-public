# Data-quality contracts and survey diagnostics

Use `dqguard` to evaluate explicit rules against a data snapshot and retain the rows and rules behind each finding. The public variant provides the generic validation core; project-specific monitoring modules and client catalogues are excluded.

## Requirements

R 4.1 or later. Runtime dependencies are listed in `DESCRIPTION`. The optional `validate`, database and survey packages support additional integrations. They are not needed for the intake example.

## Worked intake example

The example creates four synthetic records. Two share the ID B and one has a negative measurement. `dq_spec()` defines uniqueness and range rules, `dq_rules()` groups them, and `dq_run()` evaluates them. The result contains `metrics` and `findings` tables. Both duplicate members retain their original row coordinates.

```r
library(dqguard)
rules <- dq_rules(
  dq_spec("id.unique", "unique", "id"),
  dq_spec("value.nonnegative", "range", "value", list(min = 0))
)
result <- dq_run(data.frame(id = c("A", "B", "B"), value = c(12, -2, 8)), rules)
result$findings
dq_gate(result, error = FALSE)
```

The gate returns `FALSE` because the snapshot has findings that prevent release. `dq_gate(result)` raises a gate error by default; use `error = FALSE` when a caller needs a logical decision. A corrected snapshot requires a new evaluation. The executable example includes that second run and checks that it passes the two specified rules.

## Reading results

Inspect the rule identifier, row coordinates, evaluation status and denominators. Missing data and rules that could not be evaluated need their own disposition. A passing gate applies to the supplied rules and snapshot. Rule selection must reflect the intended data use.

Use `dq_partition()` when your workflow needs accepted, held and quarantined routes. Keep the result bound to the exact snapshot: changing the data before partitioning produces a stale-result error.

## Troubleshooting

An unavailable catalogue error means the package must be installed, or that the requested catalogue is outside this public variant. `dq_catalog()` supports controls, standards, tools and the generic checklist. The client-derived forensic catalogue is excluded. Optional integrations report or skip missing optional dependencies; install the dependency for the integration you need.

## Run the complete installed example

```r
path <- system.file("examples", "worked-example.R", package = "dqguard")
stopifnot(nzchar(path))
source(path)
```

## Development checks

GitHub Actions runs on pushes, pull requests and manual dispatch. It installs runtime and test dependencies, installs the package with tests, runs `testthat::test_package()` and executes the installed example. Failed steps fail the job. Optional capability skips remain visible in the log. This workflow covers Linux with the current R release; it does not establish cross-platform package acceptance.

Report an issue with the smallest synthetic example, your R version and the complete error message. Exclude private records and credentials.
