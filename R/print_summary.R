#' Print a Human-Readable Summary of Cox Diagnostic Results
#'
#' Takes the output from \code{run_diagnostics()} and prints a structured,
#' interpretable summary of the model results and diagnostic checks.
#'
#' @param diag_result A list returned by \code{run_diagnostics()}.
#'
#' @return Invisibly returns the input. Called for its side effect of printing.
#' @export
#'
#' @examples
#' library(survival)
#' result <- run_diagnostics(lung, Surv(time, status) ~ age + sex + ph.ecog)
#' print_summary(result)
print_summary <- function(diag_result) {

  model    <- diag_result$model
  ph_test  <- diag_result$ph_test

  # Extract model coefficients table
  coef_table <- summary(model)$coefficients

  cat("========================================\n")
  cat("  Cox Proportional Hazards Model Summary\n")
  cat("========================================\n\n")

  # Section 1: Sample information
  cat("Sample Information:\n")
  cat("  Observations used :", model$n, "\n")
  cat("  Number of events  :", model$nevent, "\n\n")

  # Section 2: Coefficient table
  cat("Coefficient Estimates:\n")
  cat(sprintf("  %-12s %8s %10s %8s\n", "Variable", "HR", "95% CI", "p-value"))
  cat("  ", strrep("-", 46), "\n", sep = "")

  conf <- summary(model)$conf.int

  for (var in rownames(coef_table)) {
    hr    <- round(conf[var, "exp(coef)"], 3)
    lower <- round(conf[var, "lower .95"], 3)
    upper <- round(conf[var, "upper .95"], 3)
    pval  <- round(coef_table[var, "Pr(>|z|)"], 4)
    ci    <- paste0("[", lower, ", ", upper, "]")
    sig   <- ifelse(pval < 0.05, "*", "")
    cat(sprintf("  %-12s %8.3f %12s %8.4f %s\n", var, hr, ci, pval, sig))
  }

  cat("\n  (* = significant at p < 0.05)\n\n")

  # Section 3: Overall model fit
  lrt <- summary(model)$logtest
  cat("Overall Model Fit:\n")
  cat("  Likelihood ratio test: chi-sq =", round(lrt["test"], 2),
      ", df =", lrt["df"],
      ", p =", round(lrt["pvalue"], 4), "\n\n")

  # Section 4: PH assumption test
  cat("Proportional Hazards Assumption (Schoenfeld Test):\n")
  ph_df <- as.data.frame(ph_test$table)

  for (var in rownames(ph_df)) {
    p <- round(ph_df[var, "p"], 4)
    verdict <- ifelse(p < 0.05, "VIOLATED *", "OK")
    cat(sprintf("  %-12s p = %.4f  [%s]\n", var, p, verdict))
  }

  cat("\n  (* = PH assumption may be violated at p < 0.05)\n")
  cat("========================================\n")

  invisible(diag_result)
}
