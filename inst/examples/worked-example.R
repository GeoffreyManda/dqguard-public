library(dqguard)

# Synthetic intake table: two duplicated IDs and one invalid measurement.
observations <- data.frame(id = c("A", "B", "B", "D"), value = c(12, -2, 8, 5))
rules <- dq_rules(
  dq_spec("id.unique", "unique", "id"),
  dq_spec("value.nonnegative", "range", "value", list(min = 0))
)
result <- dq_run(observations, rules, dataset = "synthetic-intake", key = "id")
print(result$metrics)
print(result$findings)
stopifnot(!dq_gate(result, error = FALSE))
stopifnot(identical(result$findings$row[result$findings$rule_id == "id.unique"], c(2L, 3L)))

# A reviewed corrected table is evaluated in a new run.
corrected <- data.frame(id = c("A", "B", "C", "D"), value = c(12, 2, 8, 5))
checked <- dq_run(corrected, rules, dataset = "synthetic-intake", key = "id")
stopifnot(dq_gate(checked, error = FALSE))
cat("Initial intake blocked; corrected intake passed the two specified rules.\n")
