# Person 2: Problems 2.2--2.6
# Run from the repository root with:
#   Rscript ps1/person2/Problem2_2_6.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("ps1/person2/Problem2_2_6.R", mustWork = TRUE)
}

output_dir <- dirname(script_path)
repo_root <- normalizePath(file.path(output_dir, "..", ".."), mustWork = TRUE)
data_path <- file.path(repo_root, "ps1", "ct_filtered.csv")

raw_data <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns <- c("assessedvalue_true", "saleamount")
missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0L) {
  stop("Missing required column(s): ", paste(missing_columns, collapse = ", "))
}

analysis_data <- raw_data[
  is.finite(raw_data$assessedvalue_true) & is.finite(raw_data$saleamount),
  ,
  drop = FALSE
]
if (nrow(analysis_data) < 2L) stop("At least two complete observations are required.")

x <- analysis_data$assessedvalue_true
y <- analysis_data$saleamount

# 2.2: Predictions from the assigned manual rule.
manual_intercept <- 0
manual_slope <- 0.8
manual_predictions <- manual_intercept + manual_slope * x

# 2.3: Residuals are actual minus predicted sale amount.
manual_residuals <- y - manual_predictions

png(
  file.path(output_dir, "problem2_3_residual_plot.png"),
  width = 1800, height = 1200, res = 180
)
plot(
  manual_predictions,
  manual_residuals,
  pch = 16,
  col = rgb(31 / 255, 119 / 255, 180 / 255, 0.45),
  xlab = "Predicted sale amount (0.8 x assessed value)",
  ylab = "Residual (actual - predicted)",
  main = "Problem 2.3: Residual Plot for Manual Rule"
)
abline(h = 0, lty = 2, lwd = 2, col = "firebrick")
grid(col = "grey85")
dev.off()

# 2.4: Manual OLS using sample covariance, variance, and means.
mean_x <- mean(x)
mean_y <- mean(y)
cov_xy <- cov(x, y)
var_x <- var(x)
if (var_x == 0) stop("Cannot estimate a slope because assessed values have zero variance.")

ols_slope <- cov_xy / var_x
ols_intercept <- mean_y - ols_slope * mean_x
ols_predictions <- ols_intercept + ols_slope * x
ols_residuals <- y - ols_predictions

# 2.5: Numerical minimization of the sum of squared residuals.
sse <- function(parameters) {
  intercept <- parameters[1]
  slope <- parameters[2]
  sum((y - intercept - slope * x)^2)
}

optimization <- optim(
  par = c(intercept = 0, slope = 0.8),
  fn = sse,
  method = "BFGS",
  control = list(reltol = 1e-12, maxit = 10000)
)
if (optimization$convergence != 0L) {
  warning("optim did not report normal convergence; code = ", optimization$convergence)
}

optim_intercept <- unname(optimization$par[1])
optim_slope <- unname(optimization$par[2])

# 2.6: Data with the manual rule and OLS line.
png(
  file.path(output_dir, "problem2_6_manual_vs_ols.png"),
  width = 1800, height = 1200, res = 180
)
plot(
  x,
  y,
  pch = 16,
  col = rgb(0, 0, 0, 0.25),
  xlab = "True assessed value",
  ylab = "Sale amount",
  main = "Problem 2.6: Manual Rule vs. OLS"
)
abline(a = manual_intercept, b = manual_slope, col = "firebrick", lwd = 3, lty = 2)
abline(a = ols_intercept, b = ols_slope, col = "navy", lwd = 3)
legend(
  "topleft",
  legend = c("Manual: y-hat = 0.8x", "OLS prediction line"),
  col = c("firebrick", "navy"),
  lty = c(2, 1),
  lwd = 3,
  bty = "n"
)
grid(col = "grey85")
dev.off()

# Concise results table covering the requested calculations.
results <- data.frame(
  item = c(
    "observations", "mean_assessedvalue_true", "mean_saleamount",
    "covariance_x_y", "variance_x", "manual_intercept", "manual_slope",
    "manual_SSE", "manual_RMSE", "OLS_intercept", "OLS_slope", "OLS_SSE",
    "OLS_RMSE", "optim_intercept", "optim_slope", "optim_SSE",
    "optim_convergence_code", "max_abs_optim_OLS_coefficient_difference"
  ),
  value = c(
    length(x), mean_x, mean_y, cov_xy, var_x, manual_intercept, manual_slope,
    sum(manual_residuals^2), sqrt(mean(manual_residuals^2)),
    ols_intercept, ols_slope, sum(ols_residuals^2), sqrt(mean(ols_residuals^2)),
    optim_intercept, optim_slope, optimization$value, optimization$convergence,
    max(abs(c(optim_intercept - ols_intercept, optim_slope - ols_slope)))
  )
)

write.csv(results, file.path(output_dir, "problem2_results.csv"), row.names = FALSE)

report_lines <- c(
  "Person 2 Results: Problems 2.2--2.6",
  sprintf("Data: ps1/ct_filtered.csv (%d complete observations)", length(x)),
  "",
  sprintf("2.2 Manual rule: saleamount_hat = %.1f * assessedvalue_true", manual_slope),
  sprintf("2.3 Manual-rule SSE = %.6f; RMSE = %.6f", sum(manual_residuals^2), sqrt(mean(manual_residuals^2))),
  sprintf("2.4 mean(x) = %.6f; mean(y) = %.6f", mean_x, mean_y),
  sprintf("    cov(x,y) = %.6f; var(x) = %.6f", cov_xy, var_x),
  sprintf("    OLS intercept = %.6f; OLS slope = %.6f", ols_intercept, ols_slope),
  sprintf("2.5 optim intercept = %.6f; optim slope = %.6f", optim_intercept, optim_slope),
  sprintf("    optim SSE = %.6f; convergence code = %d", optimization$value, optimization$convergence),
  sprintf("    max |optim coefficient - OLS coefficient| = %.10g", max(abs(c(optim_intercept - ols_intercept, optim_slope - ols_slope)))),
  "",
  "2.6 See problem2_6_manual_vs_ols.png."
)
writeLines(report_lines, file.path(output_dir, "problem2_results.txt"))

message("Person 2 analysis complete. Outputs saved to: ", output_dir)
