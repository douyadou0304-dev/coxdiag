#' Run Cox Model Diagnostics
#'
#' Fits a Cox proportional hazards model, checks the proportional hazards
#' assumption, and generates diagnostic plots.
#'
#' @param data A data frame containing the survival data.
#' @param formula A formula object for the Cox model (e.g., Surv(time, status) ~ age + sex).
#'
#' @return A list with three elements: model (coxph fit), ph_test (cox.zph result), and plots.
#' @export
#'
#' @examples
#' library(survival)
#' result <- run_diagnostics(lung, Surv(time, status) ~ age + sex + ph.ecog)
#' print(result$model)
run_diagnostics <- function(data, model_formula) {

  # Step 1: Fit the Cox proportional hazards model
  model <- survival::coxph(model_formula, data = data, x = TRUE)

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
