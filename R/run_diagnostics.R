#' Run Cox Model Diagnostics
#'
#' Fits a Cox proportional hazards model, checks the proportional hazards
#' assumption, and generates diagnostic plots.
#'
#' @param data A data frame containing the survival data.
#' @param model_formula A formula object for the Cox model (e.g., Surv(time, status) ~ age + sex).
#'
#' @return A list with elements: model, ph_test, ph_plot, and martingale_plot.
#' @export
#'
#' @examples
#' library(survival)
#' result <- run_diagnostics(lung, Surv(time, status) ~ age + sex + ph.ecog)
#' print(result$model)
run_diagnostics <- function(data, model_formula) {

  # Step 0: Validate inputs before running anything
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # Extract covariate names from formula and check for zero variation
  covar_names <- all.vars(model_formula)
  covar_names <- covar_names[!covar_names %in% c("Surv", "time", "status")]

  for (var in covar_names) {
    if (var %in% names(data)) {
      col <- data[[var]]
      col_clean <- col[!is.na(col)]
      if (length(unique(col_clean)) == 1) {
        stop(paste0(
          "Variable '", var, "' has no variation (all values are identical). ",
          "Cox model cannot be fitted. Please check your data."
        ))
      }
    }
  }

  # Warn user if sample size is small
  if (nrow(data) < 30) {
    warning(paste0(
      "Sample size is small (n = ", nrow(data), "). ",
      "Model estimates may be unstable."
    ))
  }

  # Step 1: Fit the Cox proportional hazards model
  model <- survival::coxph(model_formula, data = data, x = TRUE)

  # Warn user if rows were dropped due to missing values
  n_used <- model$n
  n_total <- nrow(data)
  if (n_used < n_total) {
    warning(paste0(
      n_total - n_used, " row(s) were removed due to missing values. ",
      "Model was fitted on ", n_used, " of ", n_total, " observations."
    ))
  }

  # Step 2: Test the proportional hazards assumption using Schoenfeld residuals
  ph_test <- survival::cox.zph(model)

  # Step 3: Plot Schoenfeld residuals to visually assess the PH assumption over time
  ph_plot <- survminer::ggcoxzph(ph_test)

  # Step 4: Plot Martingale residuals to check for nonlinearity in continuous covariates
  martingale_plot <- survminer::ggcoxdiagnostics(model, type = "martingale")

  # Step 5: Return all results as a structured list
  result <- list(
    model           = model,
    ph_test         = ph_test,
    ph_plot         = ph_plot,
    martingale_plot = martingale_plot
  )

  return(result)
}
