# ==============================================================================
# 00_install_packages.R
# ------------------------------------------------------------------------------
# Installs all R packages required for the Roche ADS Programmer coding
# assessment (Questions 1–3).  Run this script once before sourcing any of the
# question-specific scripts.
#
# R version requirement: 4.2.0+
# ==============================================================================

# ── Helpers ───────────────────────────────────────────────────────────────────

install_if_missing <- function(pkgs, repos = "https://cloud.r-project.org") {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install) == 0L) {
    message("All packages already installed.")
    return(invisible(NULL))
  }
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = repos, dependencies = TRUE)
}

# ── CRAN packages ─────────────────────────────────────────────────────────────

cran_pkgs <- c(
  # --- Pharmaverse (SDTM / ADaM) ---
  "admiral",          # ADaM derivation framework (Q2)
  "admiraldev",       # Developer utilities for admiral
  "sdtm.oak",         # SDTM domain creation (Q1)
  "pharmaverseraw",   # Raw source datasets (Q1)
  "pharmaversesdtm",  # SDTM reference datasets (Q2)
  "pharmaverseadam",  # ADaM reference datasets (Q3)

  # --- TLG / Reporting ---
  "gtsummary",        # Summary tables (Q3)
  "gt",               # Table formatting / export
  "ggplot2",          # Visualizations (Q3)

  # --- Tidyverse core ---
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "forcats",
  "purrr",
  "tibble",
  "readr",

  # --- Data I/O ---
  "haven",            # Read/write SAS (.sas7bdat) and XPT files

  # --- Utilities ---
  "here",             # Robust relative paths
  "glue",             # String interpolation
  "scales",           # ggplot2 axis formatting
  "binom"             # Binomial CIs for Plot 2 in Q3
)

install_if_missing(cran_pkgs)

# ── Verify installation ───────────────────────────────────────────────────────

missing_after <- cran_pkgs[!cran_pkgs %in% rownames(installed.packages())]

if (length(missing_after) == 0L) {
  message("\n✓ All packages installed successfully.")
} else {
  warning(
    "The following packages could not be installed: ",
    paste(missing_after, collapse = ", ")
  )
}

# ── Session info ──────────────────────────────────────────────────────────────

message("\nSession info:")
print(sessionInfo())
