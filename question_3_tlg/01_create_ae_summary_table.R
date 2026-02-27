# ==============================================================================
# 01_create_ae_summary_table.R
# Question 3, Task 1: TEAE Summary Table (FDA Table 10 style)
# ------------------------------------------------------------------------------
# Creates a nested SOC / Preferred Term TEAE summary table using {gtsummary}.
#
# Approach — gtsummary::tbl_hierarchical() (available since gtsummary 2.0.3):
#   This function is the pharmaverse-recommended approach for FDA-style nested
#   AE tables (it is the same approach used by the {cardinal} package's FDA
#   Table 10 template).  It handles:
#     • Subject-level deduplication via id = USUBJID
#     • Per-arm and total denominators via denominator = adsl_treated
#     • Nested SOC → PT rows via variables = c(AESOC, AEDECOD)
#     • An "Any TEAE" overall row via overall_row = TRUE
#     • A total column via add_overall()
#     • Output convertible to {gt} via as_gt() for further styling
#
# Design decisions:
#   - Unit of analysis: unique SUBJECTS per SOC/PT (not AE record count)
#   - Denominator:      all treated subjects per arm in ADSL (excl. Screen Failure)
#   - Sort order:       SOCs and PTs sorted by descending overall (Total) frequency
#                       achieved by pre-computing factor levels from total subject counts
#   - Table package:    {gtsummary} tbl_hierarchical() → as_gt() → styled with {gt}
#
# Inputs:  pharmaverseadam::adae, pharmaverseadam::adsl
# Outputs: output/q3/ae_summary_table.html      (main — FDA Table 10 style)
#          output/q3/ae_simple_summary.html      (gtsummary flat any-TEAE overview)
# ==============================================================================

suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(dplyr)
  library(tidyr)
  library(gtsummary)
  library(gt)
  library(here)
})

message("gtsummary version: ", as.character(packageVersion("gtsummary")))

# ── 1. Load data ──────────────────────────────────────────────────────────────

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

message("adae: ", nrow(adae), " rows x ", ncol(adae), " cols")
message("adsl: ", nrow(adsl), " rows x ", ncol(adsl), " cols")

# ── 2. Denominators: treated subjects per arm (exclude Screen Failure) ────────

arm_order <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

adsl_treated <- adsl %>%
  filter(ACTARM %in% arm_order) %>%
  mutate(ACTARM = factor(ACTARM, levels = arm_order))

arm_n    <- count(adsl_treated, ACTARM, name = "N")
total_n  <- sum(arm_n$N)

message("Safety population: ", total_n, " subjects")
print(arm_n)

# ── 3. Filter to TEAEs ────────────────────────────────────────────────────────

teae <- adae %>%
  filter(TRTEMFL == "Y", ACTARM %in% arm_order)

message("TEAE records: ", nrow(teae),
        " | TEAE subjects: ", n_distinct(teae$USUBJID))

# ── 4. Set factor levels for descending-frequency sort order ──────────────────
#
# tbl_hierarchical() respects factor levels to determine the display order of
# rows.  Pre-computing levels from overall subject counts gives descending-freq
# ordering for both SOC and PT rows.

soc_levels <- teae %>%
  group_by(AESOC) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(desc(n)) %>%
  pull(AESOC)

pt_levels <- teae %>%
  group_by(AEDECOD) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(desc(n)) %>%
  pull(AEDECOD)

teae_factored <- teae %>%
  mutate(
    AESOC   = factor(AESOC,   levels = soc_levels),
    AEDECOD = factor(AEDECOD, levels = pt_levels),
    ACTARM  = factor(ACTARM,  levels = arm_order)
  )

message("SOC levels set (", length(soc_levels), " SOCs), ",
        "PT levels set (", length(pt_levels), " PTs)")

# ── 5. gtsummary flat overview (any-TEAE) ─────────────────────────────────────
#
# Demonstrates tbl_summary() for a simple cross-tabulation: subjects with any
# TEAE vs. total subjects by arm.  The denominator is correctly set to all
# treated ADSL subjects by starting from the full adsl_treated dataset.

teae_subjs <- unique(teae$USUBJID)

tbl_overview <- adsl_treated %>%
  select(USUBJID, ACTARM) %>%
  mutate(any_teae = if_else(USUBJID %in% teae_subjs, "Y", "N")) %>%
  select(-USUBJID) %>%
  tbl_summary(
    by        = ACTARM,
    include   = any_teae,
    value     = list(any_teae ~ "Y"),
    label     = list(any_teae ~ "Any Treatment-Emergent AE"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing   = "no"
  ) %>%
  add_overall(col_label = "**Total**<br>N = {N}") %>%
  modify_header(all_stat_cols(FALSE) ~ "**{level}**<br>N = {n}") %>%
  bold_labels() %>%
  modify_caption("**Any TEAE by Treatment Arm (quick overview)**")

message("Flat overview table created")

# ── 6. Main table: tbl_hierarchical() — nested SOC / PT ───────────────────────
#
# tbl_hierarchical() was introduced in gtsummary 2.0.3 (Oct 2024) and is the
# approach recommended by the {cardinal} FDA Table 10 template.
#
# Key arguments:
#   data        = ADAE filtered to TEAEs (one row per TEAE record)
#   variables   = c(AESOC, AEDECOD)  → two-level hierarchy: SOC then PT
#   id          = USUBJID            → deduplicate: count subjects, not records
#   denominator = adsl_treated       → arm-level Ns from the safety population
#   by          = ACTARM             → stratify columns by treatment arm
#   overall_row = TRUE               → add "Any Treatment-Emergent AE" header row

message("\n── Building tbl_hierarchical ────────────────────────────────────────")

tbl_main <- tbl_hierarchical(
  data        = teae_factored,
  variables   = c(AESOC, AEDECOD),
  id          = USUBJID,
  denominator = adsl_treated,
  by          = ACTARM,
  overall_row = TRUE,
  statistic   = everything() ~ "{n} ({p}%)",
  label       = list(
    "..ard_hierarchical_overall.." ~ "Any Treatment-Emergent AE"
  )
) %>%
  # Add a "Total" column pooled across all arms
  add_overall(
    statistic = everything() ~ "{n} ({p}%)",
    col_label = "**Total**  \nN = {N}"
  ) %>%
  # Column headers: arm name + N for each arm column
  modify_header(
    all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}"
  ) %>%
  bold_labels() %>%
  modify_caption(
    paste0(
      "**Summary of Treatment-Emergent Adverse Events (TEAEs)**  \n",
      "Number (%) of Unique Subjects with \u2265 1 TEAE, by SOC and Preferred Term"
    )
  )

message("tbl_hierarchical built")

# ── 7. Convert to gt and add additional styling ───────────────────────────────

gt_tbl <- tbl_main %>%
  as_gt() %>%

  # Table-level options (Roche-style colours)
  tab_options(
    table.width                    = pct(100),
    column_labels.background.color = "#f5f5f5",
    heading.background.color       = "#eaf0fb",
    table.border.top.color         = "#003865",
    table.border.top.width         = px(2)
  ) %>%

  # Footnote on denominator
  tab_footnote(
    footnote  = "\u2020 Denominator = all treated subjects in ADSL (safety population).",
    locations = cells_column_labels(columns = everything())
  ) %>%

  # Source note
  tab_source_note(md(paste0(
    "*TEAE: Treatment-Emergent Adverse Event (TRTEMFL = 'Y'). ",
    "A subject is counted once per SOC/PT even if multiple events were reported. ",
    "SOCs and preferred terms sorted by descending overall frequency. ",
    "Table built with gtsummary::tbl_hierarchical() (gtsummary \u2265 2.0.3).*"
  ))) %>%

  opt_table_font(font = "Arial") %>%
  opt_horizontal_padding(scale = 1.5)

message("gt styling applied")

# ── 8. Save outputs ───────────────────────────────────────────────────────────

dir.create(here("output", "q3"), showWarnings = FALSE, recursive = TRUE)

# Primary output: full nested HTML table
gtsave(gt_tbl, here("output", "q3", "ae_summary_table.html"))
message("Saved: output/q3/ae_summary_table.html")

# Secondary output: flat overview (any-TEAE, gtsummary tbl_summary)
gtsave(as_gt(tbl_overview), here("output", "q3", "ae_simple_summary.html"))
message("Saved: output/q3/ae_simple_summary.html")

# Optional DOCX export (requires officer; skip gracefully if unavailable)
tryCatch({
  if (!requireNamespace("officer", quietly = TRUE)) {
    install.packages("officer", repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  gtsave(gt_tbl, here("output", "q3", "ae_summary_table.docx"))
  message("Saved: output/q3/ae_summary_table.docx")
}, error = function(e) {
  message("Note: .docx export skipped — ", conditionMessage(e))
})

# ── 9. Verification summary ───────────────────────────────────────────────────

message("\n── Verification ──────────────────────────────────────────────────────")
message("gtsummary version: ", as.character(packageVersion("gtsummary")))
message("Function used: tbl_hierarchical() [gtsummary >= 2.0.3]")
message("Hierarchy: AESOC -> AEDECOD (", length(soc_levels), " SOCs, ",
        length(pt_levels), " PTs)")
message("Overall row: 'Any Treatment-Emergent AE'")
message("Total N (denominator): ", total_n)
message("Arms: ", paste(arm_order, collapse = ", "))
message("Output: output/q3/ae_summary_table.html")

message("\n== STATUS: SUCCESS — all outputs written error-free ==")
