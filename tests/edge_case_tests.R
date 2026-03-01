# Edge Case Tests for coxdiag
# Purpose: Test package robustness under non-standard input conditions
# These tests verify that run_diagnostics() handles edge cases gracefully

devtools::load_all()
library(survival)

# ============================================================
# Test 1: Small sample size (n = 15)
# Expected: Model may be unstable, but should run without crashing
# ============================================================

cat("Running Test 1: Small sample size (n = 15)\n")
cat("--------------------------------------------\n")

set.seed(42)
small_data <- data.frame(
  time   = c(5, 10, 3, 8, 12, 1, 7, 15, 2, 9, 4, 11, 6, 13, 14),
  status = c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0),
  age    = c(45, 60, 52, 38, 71, 55, 49, 63, 41, 58, 67, 44, 53, 70, 48)
)

result_small <- run_diagnostics(small_data, Surv(time, status) ~ age)
print_summary(result_small)

# ============================================================
# Test 2: Data with missing values (NA)
# Expected: R should auto-exclude rows with NA (listwise deletion)
# ============================================================

cat("\nRunning Test 2: Missing values in data\n")
cat("--------------------------------------------\n")

set.seed(42)
missing_data <- data.frame(
  time   = c(5, NA, 3, 8, 12, 1, 7, 15, 2, 9, 4, 11, 6, 13, 14),
  status = c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0),
  age    = c(45, 60, NA, 38, 71, 55, NA, 63, 41, 58, 67, NA, 53, 70, 48)
)

result_missing <- run_diagnostics(missing_data, Surv(time, status) ~ age)
print_summary(result_missing)

# ============================================================
# Test 3: Zero variation in a covariate (all ages identical)
# Expected: Model should fail or return a warning/error
# ============================================================

cat("\nRunning Test 3: No variation in covariate\n")
cat("--------------------------------------------\n")

set.seed(42)
no_variation_data <- data.frame(
  time   = c(5, 10, 3, 8, 12, 1, 7, 15, 2, 9, 4, 11, 6, 13, 14),
  status = c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0),
  age    = rep(50, 15)
)

tryCatch({
  result_novar <- run_diagnostics(no_variation_data, Surv(time, status) ~ age)
  print_summary(result_novar)
}, error = function(e) {
  cat("ERROR caught:", conditionMessage(e), "\n")
}, warning = function(w) {
  cat("WARNING caught:", conditionMessage(w), "\n")
})

cat("\nAll edge case tests completed.\n")
