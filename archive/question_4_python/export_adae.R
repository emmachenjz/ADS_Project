# ==============================================================================
# export_adae.R
# ------------------------------------------------------------------------------
# Exports the pharmaverseadam::adae dataset to a CSV file so it can be used
# as the input data file for Question 4 (Python / GenAI Clinical Data Assistant).
#
# Output: question_4_python/adae.csv
# ==============================================================================

library(pharmaverseadam)
library(readr)
library(here)

# Load the ADAE dataset from the pharmaverseadam package
adae <- pharmaverseadam::adae

message("Loaded adae: ", nrow(adae), " rows x ", ncol(adae), " columns")
message("Columns: ", paste(names(adae), collapse = ", "))

# Write to CSV alongside this script
out_path <- here("question_4_python", "adae.csv")
write_csv(adae, out_path)

message("Written to: ", out_path)
