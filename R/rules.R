# A rule's check returns TRUE=valid, FALSE=violation, NA=unknown.
# Scope, denominator, missingness policy and release action are explicit.
dq_rule <- function(id, check, columns = character(), dimension = "validity",
                    severity = "error", action = "quarantine", scope = "row",
                    max_fail_rate = 0, max_fail_count = Inf,
                    na_policy = "fail", when = NULL, owner = "unassigned",
                    rationale = "", version = "1", source = "project contract",
                    allow_empty = FALSE) {
  .dq_chr1(id, "id"); .dq_chr1(version, "version")
  .dq_chr1(dimension, "dimension"); .dq_chr1(owner, "owner")
  .dq_chr1(rationale, "rationale", TRUE); .dq_chr1(source, "source")
  if (!is.function(check)) .dq_abort("check must be a function(data, context).")
  if (!is.null(when) && !is.function(when)) .dq_abort("when must be NULL or a function(data, context).")
  if (!is.character(columns) || anyNA(columns) || anyDuplicated(columns) || any(!nzchar(columns)))
    .dq_abort("columns must be unique non-empty names.")
  severity <- match.arg(severity, c("critical", "error", "warning", "info"))
  action <- match.arg(action, c("quarantine", "block", "review", "none"))
  scope <- match.arg(scope, c("row", "dataset"))
  na_policy <- match.arg(na_policy, c("fail", "exclude", "pass"))
  .dq_num1(max_fail_rate, "max_fail_rate", 0, 1)
  .dq_num1(max_fail_count, "max_fail_count", 0, Inf, FALSE)
  .dq_flag(allow_empty, "allow_empty")
  if (scope == "dataset" && !is.null(when)) .dq_abort("Dataset rules cannot use row applicability.")
  if (is.finite(max_fail_count) && max_fail_count != floor(max_fail_count))
    .dq_abort("max_fail_count must be an integer or Inf.")
  structure(list(id = id, check = check, columns = columns, dimension = dimension,
    severity = severity, action = action, scope = scope, max_fail_rate = max_fail_rate,
    max_fail_count = max_fail_count, na_policy = na_policy, when = when, owner = owner,
    rationale = rationale, version = version, source = source, allow_empty = allow_empty,
    specification = NULL), class = "dq_rule")
}

dq_spec <- function(id, type, columns = character(), params = list(), ...) {
  .dq_chr1(type, "type")
  if (!type %in% .dq_check_types()) .dq_abort(paste("Unknown check type:", type))
  if (!is.list(params)) .dq_abort("params must be a list.")
  .dq_validate_params(type, columns, params)
  # A closed parameter vocabulary: no eval(parse()), source(), or YAML code.
  dots <- list(...)
  expected_scope <- if (type %in% c("row_count", "freshness")) "dataset" else "row"
  if (is.null(dots$scope)) dots$scope <- expected_scope
  if (!identical(dots$scope, expected_scope)) .dq_abort("Built-in check scope does not match its return shape.")
  rule <- do.call(dq_rule, c(list(id = id,
    check = function(data, context) .dq_eval_spec(data, context, type, columns, params),
    columns = columns), dots))
  rule$specification <- list(type = type, params = params)
  rule
}

dq_rules <- function(...) {
  rules <- list(...)
  if (length(rules) == 1L && is.list(rules[[1]]) && !inherits(rules[[1]], "dq_rule")) rules <- rules[[1]]
  if (!length(rules) || !all(vapply(rules, inherits, logical(1), "dq_rule")))
    .dq_abort("Supply one or more dq_rule objects.")
  ids <- vapply(rules, `[[`, character(1), "id")
  if (anyDuplicated(ids)) .dq_abort("Rule IDs must be unique.")
  structure(rules, class = c("dq_rules", "list"))
}

.dq_rule_signature <- function(rule) {
  # Custom closures can depend on external state; callers must version such inputs
  # in context and should not use mutable global state in production checks.
  x <- rule[c("id", "columns", "dimension", "severity", "action", "scope", "max_fail_rate",
               "max_fail_count", "na_policy", "owner", "rationale", "version", "source", "allow_empty")]
  x$specification <- rule$specification
  x$inputs_hash <- rule$inputs_hash
  x$check <- if (is.null(rule$specification)) list(formals(rule$check), deparse(body(rule$check))) else NULL
  x$when <- if (!is.null(rule$when)) list(formals(rule$when), deparse(body(rule$when))) else NULL
  x
}
