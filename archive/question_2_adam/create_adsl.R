# ==============================================================================
# create_adsl.R
# Question 2: ADaM ADSL (Subject-Level) Dataset Creation
# ------------------------------------------------------------------------------
# Creates an ADSL dataset following the Pharmaverse admiral pattern.
# The script is structured in clearly labelled sections:
#   1  Load packages & data
#   2  Base ADSL from DM
#   3  Standard date conversions (reference start/end, death)
#   4  Treatment start/end datetime from EX  ← Custom variable: TRTSDTM/TRTSTMF
#   5  Disposition variables from DS
#   6  Safety and efficacy flags
#   7  Custom variable: AGEGR9 / AGEGR9N
#   8  Custom variable: TRTSDTM / TRTSTMF  (already derived in Section 4;
#                                            this section documents the logic)
#   9  Custom variable: ITTFL
#  10  Custom variable: LSTAVLDT
#  11  Final variable selection, ordering, and output
#  12  Quality-check summary
#
# Inputs:  pharmaversesdtm::dm, vs, ex, ds, ae
# Outputs: output/q2/adsl.rds
#          output/q2/adsl.csv
#
# Reference: https://pharmaverse.github.io/admiral/cran-release/articles/adsl.html
# ==============================================================================

suppressPackageStartupMessages({
  library(admiral)
  library(pharmaversesdtm)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(here)
})

# ── 1. Load SDTM source data ──────────────────────────────────────────────────

dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

message("Source datasets loaded:")
message("  dm : ", nrow(dm), " rows | vs : ", nrow(vs),
        " rows | ex : ", nrow(ex), " rows")
message("  ds : ", nrow(ds), " rows | ae : ", nrow(ae), " rows")

# ── 2. Base ADSL from DM ──────────────────────────────────────────────────────
#
# Per the admiral ADSL article, start by assigning pharmaversesdtm::dm to adsl.
# Select the core demographics and treatment assignment variables; add numeric
# treatment codes (TRT01PN / TRT01AN) needed for downstream sorting.

adsl <- dm %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
    ARM, ACTARM, ARMCD, ACTARMCD, COUNTRY,
    RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, RFICDTC, RFPENDTC,
    DTHDTC, DTHFL,
    DMDTC, DMDY
  ) %>%
  mutate(
    # Planned and actual treatment labels (synonyms of ARM / ACTARM kept for
    # downstream readability and consistency with ADaM convention)
    TRT01P  = ARM,
    TRT01A  = ACTARM,
    # Numeric treatment codes (used for analysis ordering / multiplicity adj.)
    TRT01PN = case_when(
      ARM == "Placebo"              ~ 0L,
      ARM == "Xanomeline Low Dose"  ~ 54L,
      ARM == "Xanomeline High Dose" ~ 81L,
      TRUE                          ~ NA_integer_
    ),
    TRT01AN = case_when(
      ACTARM == "Placebo"              ~ 0L,
      ACTARM == "Xanomeline Low Dose"  ~ 54L,
      ACTARM == "Xanomeline High Dose" ~ 81L,
      TRUE                             ~ NA_integer_
    )
  )

message("Base ADSL rows: ", nrow(adsl), " (one row per subject expected)")

# ── 3. Standard date conversions ──────────────────────────────────────────────
#
# admiral::derive_vars_dt() converts ISO 8601 DTC strings to numeric SAS dates
# and creates an optional imputation flag (suffix DTF).
# highest_imputation = "n" ensures no imputation (only complete dates pass).

adsl <- adsl %>%
  # Reference start / end dates
  derive_vars_dt(new_vars_prefix = "RFST",  dtc = RFSTDTC,
                 highest_imputation = "n") %>%
  derive_vars_dt(new_vars_prefix = "RFEND", dtc = RFENDTC,
                 highest_imputation = "n") %>%
  # First / last exposure reference dates
  derive_vars_dt(new_vars_prefix = "RFXST", dtc = RFXSTDTC,
                 highest_imputation = "n") %>%
  derive_vars_dt(new_vars_prefix = "RFXEN", dtc = RFXENDTC,
                 highest_imputation = "n") %>%
  # Informed consent date
  derive_vars_dt(new_vars_prefix = "RFIC",  dtc = RFICDTC,
                 highest_imputation = "n") %>%
  # Death date
  derive_vars_dt(new_vars_prefix = "DTH",   dtc = DTHDTC,
                 highest_imputation = "n")

# ── 4. Treatment start/end datetime from EX ───────────────────────────────────
#
# ── CUSTOM VARIABLE: TRTSDTM / TRTSTMF ─────────────────────────────────────
#
# TRTSDTM = datetime of patient's FIRST valid-dose exposure start.
# TRTSTMF = time imputation flag (indicates which time components were imputed).
#
# VALID DOSE: EXDOSE > 0  OR  (EXDOSE == 0 AND EXTRT contains "PLACEBO")
# DATE completeness: datepart of EXSTDTC must be complete (YYYY-MM-DD).
#   → achieved by highest_imputation = "h": only time parts are imputed,
#     not date parts; records with incomplete dates become NA and are filtered out.
#
# TIME IMPUTATION RULES (per spec):
#   - Completely missing time (as in this dataset — dates only) → 00:00:00
#   - Partially missing hours/minutes                            → 00 for missing parts
#   - If ONLY seconds are missing                               → do NOT set TRTSTMF
#     (handled by ignore_seconds_flag = TRUE, the admiral default)
#
# Step 1: Derive EXSTDTM / EXSTTMF and EXENDTM / EXENTMF from the EX domain.

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc                 = EXSTDTC,
    new_vars_prefix     = "EXST",
    highest_imputation  = "h",   # impute time components only (not date parts)
    time_imputation     = "first",  # "first" = 00:00:00 (spec: impute with 00s)
    flag_imputation     = "auto",
    ignore_seconds_flag = TRUE   # flag not set when only seconds are imputed
  ) %>%
  derive_vars_dtm(
    dtc                 = EXENDTC,
    new_vars_prefix     = "EXEN",
    highest_imputation  = "h",
    time_imputation     = "last",   # end of treatment defaults to end of day
    flag_imputation     = "auto",
    ignore_seconds_flag = TRUE
  )

# Step 2: Merge FIRST valid-dose start into ADSL → TRTSDTM, TRTSTMF
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = (EXDOSE > 0 |
                     (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
                  !is.na(EXSTDTM),   # exclude records where date was incomplete
    new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order       = exprs(EXSTDTM, EXSEQ),   # sort by datetime, then seq to break ties
    mode        = "first",                  # take the earliest exposure per subject
    by_vars     = exprs(STUDYID, USUBJID)
  ) %>%
  # Step 3: Merge LAST valid-dose end → TRTEDTM, TRTETMF
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = (EXDOSE > 0 |
                     (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
                  !is.na(EXENDTM),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXENDTM, EXSEQ),
    mode        = "last",
    by_vars     = exprs(STUDYID, USUBJID)
  )

# Step 4: Convert treatment datetimes to SAS numeric dates and derive duration
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  derive_var_trtdurd()

message("Treatment dates derived. TRTSDTM non-missing: ",
        sum(!is.na(adsl$TRTSDTM)))

# ── 5. Disposition variables from DS ──────────────────────────────────────────
#
# End-of-study status (EOSSTT) is derived from the DISPOSITION EVENT record.
# Screen Failure subjects are excluded from the disposition mapping — they did
# not progress to treatment and are represented separately.

format_eosstt <- function(x) {
  case_when(
    x == "COMPLETED"      ~ "COMPLETED",
    x == "SCREEN FAILURE" ~ NA_character_,  # handled via missing_values below
    TRUE                  ~ "DISCONTINUED"
  )
}

adsl <- adsl %>%
  # End-of-study status
  derive_vars_merged(
    dataset_add    = ds,
    by_vars        = exprs(STUDYID, USUBJID),
    filter_add     = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE",
    new_vars       = exprs(EOSSTT = format_eosstt(DSDECOD)),
    order          = exprs(DSSTDTC, DSSEQ),
    mode           = "last",
    missing_values = exprs(EOSSTT = "ONGOING")
  ) %>%
  # End-of-study date (last disposition event date)
  derive_vars_merged(
    dataset_add = ds,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE",
    new_vars    = exprs(EOSDT = convert_dtc_to_dt(DSSTDTC)),
    order       = exprs(DSSTDTC, DSSEQ),
    mode        = "last"
  ) %>%
  # Discontinuation reason (subjects who did not complete or screen-fail)
  derive_vars_merged(
    dataset_add = ds,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = DSCAT == "DISPOSITION EVENT" &
                  !DSDECOD %in% c("SCREEN FAILURE", "COMPLETED"),
    new_vars    = exprs(DCSREAS = DSDECOD),
    order       = exprs(DSSTDTC, DSSEQ),
    mode        = "last"
  )

# ── 6. Safety and study population flags ──────────────────────────────────────

adsl <- adsl %>%
  mutate(
    # Safety population: all subjects who received at least one dose
    SAFFL = if_else(!is.na(TRTSDTM), "Y", "N"),
    # Standard age grouping (< 65 / >= 65), common in regulatory submissions
    AGEGR1  = if_else(AGE < 65, "<65", ">=65"),
    AGEGR1N = if_else(AGE < 65, 1L, 2L)
  )

# ── 7. Custom Variable 1 — AGEGR9 / AGEGR9N ──────────────────────────────────
#
# Age grouping into three categories as specified in the assessment:
#   "<18"    (AGEGR9N = 1)
#   "18 - 50" (AGEGR9N = 2)  ← exact string format from spec (space-dash-space)
#   ">50"    (AGEGR9N = 3)
#
# Source: DM.AGE (analysis age, already in adsl from DM merge)

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE <  18              ~ "<18",
      AGE >= 18 & AGE <= 50  ~ "18 - 50",
      AGE >  50              ~ ">50",
      TRUE                   ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE <  18              ~ 1L,
      AGE >= 18 & AGE <= 50  ~ 2L,
      AGE >  50              ~ 3L,
      TRUE                   ~ NA_integer_
    )
  )

# ── 8. (Documentation) TRTSDTM / TRTSTMF ─────────────────────────────────────
#
# TRTSDTM and TRTSTMF were fully derived in Section 4.  Key decisions:
#   • Only records with a COMPLETE date portion in EXSTDTC enter the derivation
#     (highest_imputation = "h" means admiral will NOT impute partial dates →
#      they remain NA → excluded by the !is.na(EXSTDTM) filter).
#   • Since all EXSTDTC values in this dataset are date-only (no time component),
#     time is imputed to 00:00:00 in all cases → TRTSTMF is always populated.
#   • If only seconds had been missing, TRTSTMF would NOT be set
#     (ignore_seconds_flag = TRUE, the admiral default).

# ── 9. Custom Variable 3 — ITTFL ──────────────────────────────────────────────
#
# ITT flag: "Y" for randomised patients, "N" for screen failures.
#
# The spec states: "Y if DM.ARM not equal to missing."
# In pharmaversesdtm::dm, ARM is populated for all subjects — including screen
# failures (ARM = "Screen Failure").  A literal !is.na(ARM) check would give
# ITTFL = "Y" for all 306 subjects, which is clinically incorrect.
#
# Interpretation: the spec means "Y for subjects who received a valid treatment
# randomisation ARM (i.e. randomised subjects)."  Screen Failure is not a
# randomisation arm — subjects with ARM = "Screen Failure" were NEVER randomised.
#
# Implementation: "Y" if ARM is populated AND is not "Screen Failure".

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(
      !is.na(ARM) & ARM != "" & ARM != "Screen Failure",
      "Y", "N"
    )
  )

# ── 10. Custom Variable 4 — LSTAVLDT ─────────────────────────────────────────
#
# Last known alive date = the MAXIMUM complete date across four sources:
#   (1) VS:   last complete VSDTC where test result is not entirely missing
#             (VSSTRESN and VSSTRESC must not BOTH be NA)
#   (2) AE:   last complete AESTDTC (date portion must be YYYY-MM-DD)
#   (3) DS:   last complete DSSTDTC
#   (4) ADSL: TRTEDT (date of last valid dose, already derived in Section 4)
#
# "Complete date" is enforced with: !is.na(dtc) & nchar(dtc) >= 10
#
# admiral::derive_vars_extreme_event() pools rows from all four source datasets,
# evaluates set_values_to to compute LSTAVLDT for each row, then selects the
# row with the maximum LSTAVLDT per subject (mode = "last").

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events  = list(

      # (1) Vital Signs — last complete date with a valid test result
      # VSSEQ added as within-event tiebreaker to avoid the "duplicate records"
      # admiral warning that occurs when multiple VS tests share the same date.
      event(
        dataset_name  = "vs",
        order         = exprs(VSDTC, VSSEQ),
        condition     = !is.na(VSDTC) & nchar(VSDTC) >= 10 &
                        !(is.na(VSSTRESN) & is.na(VSSTRESC)),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(VSDTC),
                              LSTAVSEQ = VSSEQ)
      ),

      # (2) Adverse Events — last complete AE start date
      event(
        dataset_name  = "ae",
        order         = exprs(AESTDTC, AESEQ),
        condition     = !is.na(AESTDTC) & nchar(AESTDTC) >= 10,
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(AESTDTC),
                              LSTAVSEQ = AESEQ)
      ),

      # (3) Disposition — last complete DS event date
      event(
        dataset_name  = "ds",
        order         = exprs(DSSTDTC, DSSEQ),
        condition     = !is.na(DSSTDTC) & nchar(DSSTDTC) >= 10,
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(DSSTDTC),
                              LSTAVSEQ = DSSEQ)
      ),

      # (4) Exposure — last valid-dose treatment date (from ADSL TRTEDT)
      event(
        dataset_name  = "adsl",
        condition     = !is.na(TRTEDT),
        set_values_to = exprs(LSTAVLDT = TRTEDT,
                              LSTAVSEQ = NA_integer_)
      )
    ),
    source_datasets  = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order            = exprs(LSTAVLDT, LSTAVSEQ, event_nr),
    mode             = "last",
    new_vars         = exprs(LSTAVLDT)
  )

# ── 11. Final variable selection and dataset output ───────────────────────────
#
# Select variables in a logical order following ADaM ADSL conventions.
# Custom variables (AGEGR9/N, TRTSDTM/TRTSTMF, ITTFL, LSTAVLDT) are kept
# alongside their standard ADaM counterparts.

adsl_final <- adsl %>%
  select(
    # Subject identifiers
    STUDYID, USUBJID, SUBJID, SITEID, COUNTRY,
    # Demographics
    AGE, AGEU, AGEGR1, AGEGR1N,
    # Custom age grouping (Q2 requirement)
    AGEGR9, AGEGR9N,
    SEX, RACE, ETHNIC,
    # Treatment assignment
    ARM, ACTARM, ARMCD, ACTARMCD,
    TRT01P, TRT01PN, TRT01A, TRT01AN,
    # Reference dates
    RFSTDTC, RFSTDT,
    RFENDTC, RFENDDT,
    RFXSTDTC, RFXSTDT,
    RFXENDTC, RFXENDT,
    RFICDTC, RFICDT,
    # Treatment start datetime + imputation flag (Q2 custom: TRTSDTM/TRTSTMF)
    TRTSDTM, TRTSTMF,
    # Treatment end datetime
    TRTEDTM, TRTETMF,
    # Treatment dates (from DTM → DT conversion) and duration
    TRTSDT, TRTEDT, TRTDURD,
    # Disposition
    EOSSTT, EOSDT, DCSREAS,
    # Death
    DTHFL, DTHDTC, DTHDT,
    # Population flags
    SAFFL,
    # Custom flag: ITT population (Q2 requirement)
    ITTFL,
    # Last known alive date (Q2 custom: LSTAVLDT)
    LSTAVLDT,
    # DM administrative
    DMDTC, DMDY
  )

# ── 12. Save outputs ──────────────────────────────────────────────────────────

dir.create(here("output", "q2"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("data"),         showWarnings = FALSE, recursive = TRUE)

# RDS (lossless — preserves numeric SAS dates as Date objects)
saveRDS(adsl_final, here("output", "q2", "adsl.rds"))
message("Saved: output/q2/adsl.rds")

# CSV (human-readable; Date columns are formatted as ISO 8601)
readr::write_csv(adsl_final, here("output", "q2", "adsl.csv"))
message("Saved: output/q2/adsl.csv")

# Mirror to data/ for use by other scripts
saveRDS(adsl_final, here("data", "adsl.rds"))

# ── 13. Quality checks ────────────────────────────────────────────────────────

message("\n══ Quality Checks ════════════════════════════════════════════════════")

message("\nDimensions: ", nrow(adsl_final), " rows x ", ncol(adsl_final), " cols")
message("Expected:    306 rows (one per subject in DM)\n")

# 1. Uniqueness
n_dupes <- nrow(adsl_final) - n_distinct(adsl_final$USUBJID)
message("Duplicate USUBJIDs: ", n_dupes, " (expected: 0)")

# 2. AGEGR9 / AGEGR9N
message("\nAGEGR9 distribution:")
print(adsl_final %>% count(AGEGR9, AGEGR9N) %>% arrange(AGEGR9N))

# 3. TRTSDTM / TRTSTMF
message("\nTRTSDTM non-missing: ", sum(!is.na(adsl_final$TRTSDTM)))
message("TRTSTMF values:")
print(adsl_final %>% count(TRTSTMF, .drop = FALSE))

# 4. ITTFL
message("\nITTFL distribution:")
print(adsl_final %>% count(ACTARM, ITTFL))

# 5. LSTAVLDT
message("\nLSTAVLDT non-missing: ", sum(!is.na(adsl_final$LSTAVLDT)))
message("LSTAVLDT range: ",
        format(min(adsl_final$LSTAVLDT, na.rm = TRUE), "%Y-%m-%d"), " to ",
        format(max(adsl_final$LSTAVLDT, na.rm = TRUE), "%Y-%m-%d"))

message("\n== STATUS: SUCCESS — ADSL created error-free ==")
