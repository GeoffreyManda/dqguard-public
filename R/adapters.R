# Reuse existing R engines through explicit, versioned callback contracts.
# External expressions and callbacks are trusted project code, never untrusted file text.
dq_adapter_rule <- function(id, evaluate, columns = character(), engine,
                             engine_version, inputs = list(), ...) {
  .dq_chr1(engine, "engine"); .dq_chr1(engine_version, "engine_version")
  if (!is.function(evaluate)) .dq_abort("evaluate must be a function(data, context, inputs).")
  if (!is.list(inputs)) .dq_abort("inputs must be a list of versioned engine configuration.")
  rule <- dq_rule(id, function(data, context) evaluate(data, context, inputs), columns = columns, ...)
  rule$inputs_hash <- .dq_hash(list(engine, engine_version, inputs,
    formals(evaluate), deparse(body(evaluate))))
  rule
}

dq_validate_rule <- function(id, validator, columns = character(), ...) {
  .dq_need("validate")
  if (!inherits(validator, "validator")) .dq_abort("validator must be created by validate::validator().")
  if (length(validator) != 1L) .dq_abort("Use one validate expression per dq rule to preserve rule-level attribution.")
  validator <- validator[]
  dq_adapter_rule(id, function(data, context, inputs) {
    result <- validate::confront(data, inputs$validator, raise = "all")
    values <- validate::values(result, simplify = TRUE)
    if (is.matrix(values) && ncol(values) == 1L) values <- values[, 1L]
    if (!is.logical(values) || !is.null(dim(values))) .dq_abort("validate returned a non-logical or ambiguous result.")
    values
  }, columns, engine = "validate", engine_version = as.character(utils::packageVersion("validate")),
  inputs = list(validator = validator), ...)
}
