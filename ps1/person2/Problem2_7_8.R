# Person 2: Problems 2.7--2.8
# Run from the repository root with:
#   Rscript ps1/person2/Problem2_7_8.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("ps1/person2/Problem2_7_8.R", mustWork = TRUE)
}

output_dir <- dirname(script_path)
repo_root <- normalizePath(file.path(output_dir, "..", ".."), mustWork = TRUE)
raw_path <- file.path(repo_root, "ps1", "ct_property_data.csv")
zip_path <- file.path(repo_root, "ps1_export.zip")

# Read the original data without changing or extracting Person 1's files.
if (file.exists(raw_path)) {
  raw_data <- read.csv(raw_path, stringsAsFactors = FALSE)
  source_label <- "ps1/ct_property_data.csv"
} else if (file.exists(zip_path)) {
  archive_files <- unzip(zip_path, list = TRUE)$Name
  member <- archive_files[basename(archive_files) == "ct_property_data.csv"]
  if (length(member) != 1L) stop("Could not uniquely locate ct_property_data.csv in ps1_export.zip.")
  raw_data <- read.csv(unz(zip_path, member), stringsAsFactors = FALSE)
  source_label <- "ct_property_data.csv inside ps1_export.zip"
} else {
  stop("Original Connecticut property data not found.")
}

required_columns <- c(
  "residentialtype", "town", "listyear", "assessedvalue", "saleamount"
)
missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0L) {
  stop("Missing required column(s): ", paste(missing_columns, collapse = ", "))
}

# 2.8: Repeat 2.1 while omitting only the 2008 listing-year restriction.
raw_data$assessedvalue_true <- raw_data$assessedvalue / 0.7 / 1000
raw_data$saleamount_thousands <- raw_data$saleamount / 1000

all_years <- raw_data[
  raw_data$residentialtype == "Single Family" &
    raw_data$town == "New Haven" &
    raw_data$assessedvalue_true < 250 &
    is.finite(raw_data$listyear) &
    is.finite(raw_data$assessedvalue_true) &
    is.finite(raw_data$saleamount_thousands),
  ,
  drop = FALSE
]

split_by_year <- split(all_years, all_years$listyear)
annual_ols <- do.call(
  rbind,
  lapply(split_by_year, function(year_data) {
    x <- year_data$assessedvalue_true
    y <- year_data$saleamount_thousands
    variance_x <- var(x)
    if (length(x) < 2L || !is.finite(variance_x) || variance_x == 0) {
      return(data.frame(
        listyear = year_data$listyear[1], observations = length(x),
        intercept = NA_real_, slope = NA_real_, r_squared = NA_real_
      ))
    }
    slope <- cov(x, y) / variance_x
    intercept <- mean(y) - slope * mean(x)
    fitted <- intercept + slope * x
    sst <- sum((y - mean(y))^2)
    r_squared <- if (sst == 0) NA_real_ else 1 - sum((y - fitted)^2) / sst
    data.frame(
      listyear = year_data$listyear[1], observations = length(x),
      intercept = intercept, slope = slope, r_squared = r_squared
    )
  })
)
rownames(annual_ols) <- NULL
annual_ols <- annual_ols[order(annual_ols$listyear), ]

write.csv(
  annual_ols,
  file.path(output_dir, "problem2_8_annual_ols.csv"),
  row.names = FALSE
)

png(
  file.path(output_dir, "problem2_8_annual_ols_trends.png"),
  width = 1800, height = 1500, res = 180
)
par(mfrow = c(2, 1), mar = c(4.5, 5, 3, 1), oma = c(0, 0, 2, 0))

plot(
  annual_ols$listyear,
  annual_ols$intercept,
  type = "o", pch = 16, lwd = 2.5, col = "navy",
  xlab = "Listing year", ylab = expression(hat(beta)[0]),
  main = "OLS Intercept by Listing Year",
  xaxt = "n"
)
axis(1, at = annual_ols$listyear, labels = annual_ols$listyear)
abline(h = 0, lty = 2, col = "grey45")
grid(col = "grey85")

plot(
  annual_ols$listyear,
  annual_ols$slope,
  type = "o", pch = 16, lwd = 2.5, col = "firebrick",
  xlab = "Listing year", ylab = expression(hat(beta)[1]),
  main = "OLS Slope by Listing Year",
  xaxt = "n"
)
axis(1, at = annual_ols$listyear, labels = annual_ols$listyear)
abline(h = 0.8, lty = 2, lwd = 2, col = "darkgreen")
legend(
  "topleft", legend = "Manual-rule slope = 0.8",
  col = "darkgreen", lty = 2, lwd = 2, bty = "n"
)
grid(col = "grey85")
mtext("Problem 2.8: Evolution of Annual OLS Estimates", outer = TRUE, cex = 1.25, font = 2)
dev.off()

row_2008 <- annual_ols[annual_ols$listyear == 2008, , drop = FALSE]
row_2020 <- annual_ols[annual_ols$listyear == 2020, , drop = FALSE]

# 2.7 is a judgment question. The 2020 OLS rule is preferred for a 2023
# prediction because it is temporally closest and reflects a more recent market.
answer_lines <- c(
  "Person 2 Results: Problems 2.7--2.8",
  "",
  "2.7 Prediction rule for 2023",
  paste(
    "I would use the 2020 OLS prediction rule, saleamount_hat = -4.69 +",
    "1.37 * assessedvalue_true. It is estimated from the most recent sample among",
    "the available choices and is therefore more likely than the 2008 OLS rule or",
    "the manual rule to reflect the relationship between assessed value and sale",
    "price near 2023. This choice assumes that the relationship remained reasonably",
    "stable from 2020 to 2023; a model fitted to reliable 2023 data would be preferable",
    "if such data were available."
  ),
  "",
  "2.8 Annual OLS estimates",
  sprintf("Data source: %s", source_label),
  sprintf(
    "After filtering: %d single-family New Haven sales across %d listing years (%d-%d).",
    nrow(all_years), nrow(annual_ols), min(annual_ols$listyear), max(annual_ols$listyear)
  ),
  if (nrow(row_2008) == 1L) sprintf(
    "2008 check: intercept = %.6f; slope = %.6f; n = %d.",
    row_2008$intercept, row_2008$slope, row_2008$observations
  ) else "2008 check: no estimate available.",
  if (nrow(row_2020) == 1L) sprintf(
    "2020 replication: intercept = %.6f; slope = %.6f; n = %d.",
    row_2020$intercept, row_2020$slope, row_2020$observations
  ) else "2020 replication: no estimate available.",
  paste(
    "The annual estimates vary meaningfully over time, indicating that the relationship",
    "between assessed value and sale price is not constant across listing years.",
    "See problem2_8_annual_ols_trends.png for the time-series figure and",
    "problem2_8_annual_ols.csv for the complete annual estimates."
  )
)

writeLines(answer_lines, file.path(output_dir, "problem2_7_8_results.txt"))
message("Problems 2.7--2.8 complete. Outputs saved to: ", output_dir)
