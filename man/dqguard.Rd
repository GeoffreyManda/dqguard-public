\name{dqguard}
\alias{dqguard}
\alias{dq_adapter_rule}
\alias{dq_benford}
\alias{dq_last_digit}
\alias{dq_peer_z}
\alias{dq_percent_match}
\alias{dq_sensitivity}
\alias{dq_whipple}
\alias{dq_assess_controls}
\alias{dq_audit_rule}
\alias{dq_catalog}
\alias{dq_compare_snapshots}
\alias{dq_contract}
\alias{dq_coverage}
\alias{dq_current_issues}
\alias{dq_db_audit}
\alias{dq_db_init}
\alias{dq_db_load_ledger}
\alias{dq_db_read}
\alias{dq_db_route}
\alias{dq_db_save_ledger}
\alias{dq_db_store}
\alias{dq_dictionary}
\alias{dq_dispatch}
\alias{dq_drift}
\alias{dq_export}
\alias{dq_file_audit}
\alias{dq_gate}
\alias{dq_geofence}
\alias{dq_git_audit}
\alias{dq_issues}
\alias{dq_logic_audit}
\alias{dq_missingness}
\alias{dq_partition}
\alias{dq_profile}
\alias{dq_read_contract}
\alias{dq_read_ledger}
\alias{dq_reconcile}
\alias{dq_report}
\alias{dq_resolution_metrics}
\alias{dq_rule}
\alias{dq_rules}
\alias{dq_run}
\alias{dq_run_db}
\alias{dq_save_ledger}
\alias{dq_spec}
\alias{dq_survey_signals}
\alias{dq_sync_issues}
\alias{dq_ticket_payloads}
\alias{dq_transition}
\alias{dq_validate_rule}
\alias{dq_verify_ledger}
\alias{dq_write_contract}
\alias{dq_pv_rule}
\alias{dq_pv_scan}
\alias{dq_pv_rollup}
\alias{dq_pv_band}
\alias{dq_pv_actions}
\alias{dq_pv_alerts}
\alias{dq_speed}
\alias{dq_travel}
\alias{dq_batch_times}
\alias{dq_design_effect}
\alias{dq_interviewer_effect}
\alias{dq_required_n}
\alias{dq_falsification_rank}
\title{Data Quality Contracts, Findings and Evidence-Based Resolution}
\description{
Define explicit quality requirements, evaluate local snapshots, retain precise
findings, separate release routes, and document independently verified incident
resolution. No automatic repair or composite quality score is performed.
}
\usage{
dq_spec(id, type, columns = character(), params = list(), ...)
dq_contract(dataset, version, dictionary, key, rules = list(),
  grain, owner, purpose, allow_extra = FALSE, min_rows = 1L,
  reference_versions = list())
dq_run(data, rules, dataset = "dataset", key = NULL, context = list(),
  trigger = "manual", stage = "ingestion", identity_secret = NULL,
  include_error_messages = FALSE)
dq_gate(result, mode = c("full", "partition"), error = TRUE)
dq_partition(data, result)
dq_report(result, path, max_findings = 200L, overwrite = FALSE)
}
\arguments{
\item{id}{Unique rule identifier.}
\item{type}{A supported declarative check name. See the installed handbook for all 29 types.}
\item{columns}{Explicit ordered column names; composite keys and geographic checks use more than one.}
\item{params}{Named parameters from the closed vocabulary for the chosen check.}
\item{...}{Additional rule metadata passed to \code{dq_rule}, including severity, action, NA policy, thresholds and applicability.}
\item{dataset}{Nonempty dataset identity.}
\item{version}{Approved contract version.}
\item{dictionary}{A data frame accepted by \code{dq_dictionary}, with name, type and required fields.}
\item{key}{Columns defining the intended observation grain. Contract keys must be required.}
\item{rules}{A rule, a list of rules, a \code{dq_rules} object, or, for execution, a \code{dq_contract}.}
\item{grain}{Description of the unit represented by one observation.}
\item{owner}{Accountable data or rule owner.}
\item{purpose}{Approved use of the dataset.}
\item{allow_extra}{Whether undeclared additional columns are permitted.}
\item{min_rows}{Minimum expected number of observations.}
\item{reference_versions}{Named list of required reference versions.}
\item{data}{A local data frame or tibble with original, unique, nonempty column names.}
\item{context}{Versioned external inputs. References go in \code{references}; the evaluation clock is \code{now}, a POSIXct time.}
\item{trigger}{Execution-origin label; recording a label does not install a scheduler.}
\item{stage}{Data-lifecycle stage label.}
\item{identity_secret}{Optional externally managed HMAC secret for stable keyed record identities. Not an anonymization guarantee.}
\item{include_error_messages}{Whether to retain detailed condition messages, which may contain sensitive values.}
\item{result}{A \code{dq_result} from the exact snapshot being considered.}
\item{mode}{Full-snapshot or accepted-partition release policy.}
\item{error}{Whether a blocked release raises a \code{dq_gate_error}.}
\item{path}{Explicit output file path with an existing parent directory.}
\item{max_findings}{Maximum number of findings displayed in the HTML report. Machine-readable results retain all findings.}
\item{overwrite}{Explicit permission to replace a non-ledger output. Ledger files never overwrite existing snapshots.}
}
\details{
Checks return TRUE for valid, FALSE for violated, and NA for unknown. No scalar
recycling is accepted for row rules. Missingness policies and applicability
are explicit. Execution warnings and errors produce error results rather than
passes. Blocking rules without sufficient evidence hold release.

\code{dq_partition} verifies the original snapshot hash and returns disjoint
accepted, quarantine and held tables without modifying the input. Full release
cannot include quarantinable rows even when an aggregate threshold is tolerated.

\code{dq_file_audit} examines CSV and unencrypted OOXML structures. It does not
recalculate formulas or prove access authorization. \code{dq_assess_controls}
requires dated evidence and independent reviewer metadata for non-data controls.

\code{dq_transition} enforces state transitions, evidence, upstream correction,
independent closure and a fresh unchanged-rule recheck. Stable identities are
required to close record-level issues across snapshots. Hash chains detect
content changes relative to trusted retained heads; they do not authenticate
actors or make storage administrator-proof.

Database operations require DBI and a transaction-capable backend. Native schema
inspection is implemented for SQLite; other drivers require reviewed catalogue
evidence. Snapshot execution is in-memory and capped, not database pushdown.

Full function signatures, examples, check parameters, interpretation, operational
requirements and standards mappings are in \code{inst/doc/handbook.md} of the
source package, installed as \code{doc/handbook.md}.
}
\value{
Constructors return rule or contract objects. \code{dq_run} returns a list of
class \code{dq_result} with metrics, findings, hashes and evaluation state.
\code{dq_gate} returns a logical result or raises a typed error.
\code{dq_partition} returns disjoint data frames and row indices.
Audit and profiling functions return data frames or documented named lists.
Ledger functions return \code{dq_ledger} objects; persistence and export functions
return their path, run ID, head or receipt invisibly.
}
\examples{
library(dqguard)
d <- data.frame(id = c("A", "B"), amount = c(1, -1))
r <- dq_rules(
  dq_spec("id.unique", "unique", "id"),
  dq_spec("amount.valid", "range", "amount", list(min = 0))
)
audit <- dq_run(d, r)
dq_gate(audit, error = FALSE)
routes <- dq_partition(d, audit)
nrow(routes$quarantine)
}
\keyword{data}
