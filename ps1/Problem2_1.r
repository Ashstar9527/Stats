# =====================================================================
# Problem 2.1: Data Preparation
# =====================================================================
# NOTE FOR 2.2+: assessedvalue_true and saleamount are BOTH in $1,000s
# (e.g., $10,000 -> 10), not raw dollars.
# =====================================================================

# ---- 2.1.1: Load the data ----
ct <- read.csv("ps1/ct_property_data.csv")

# ---- 2.1.2: Create assessedvalue_true ----
# Tax-assessed value in CT is 70% of true market value,
# so dividing by 0.7 recovers the true value.
ct$assessedvalue_true <- ct$assessedvalue / 0.7

# ---- 2.1.3: Express in $1,000s ----
ct$assessedvalue_true <- ct$assessedvalue_true / 1000
ct$saleamount <- ct$saleamount / 1000

# ---- 2.1.4: Filter ----
ct_filtered <- ct[
    ct$residentialtype == "Single Family" &
        ct$town == "New Haven" &
        ct$listyear == 2008 &
        ct$assessedvalue_true < 250, # already in $1,000s, so $250,000 -> 250
]

# ---- Save the cleaned dataset for downstream use (2.2+) ----
write.csv(ct_filtered, "ps1/ct_filtered.csv", row.names = FALSE)

# ---- Sanity check, written to a readable text file ----
sink("ps1/data_prep_check.txt")

cat("Number of rows after filtering:\n")
print(nrow(ct_filtered))

cat("\nPreview of filtered data:\n")
print(head(ct_filtered))

cat("\nSummary statistics of key columns (in $1,000s):\n")
print(summary(ct_filtered[, c("assessedvalue_true", "saleamount")]))

sink() # stop redirecting -- back to normal console output

cat("Filtering complete. Rows remaining:", nrow(ct_filtered), "\n")
