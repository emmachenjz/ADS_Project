# ==============================================================================
# 01_create_ds_domain.R
# Question 1: SDTM DS Domain Creation using {sdtm.oak}
# ------------------------------------------------------------------------------
# Creates the SDTM Disposition (DS) domain from raw eCRF data.
#
# Approach — follows the Pharmaverse AE example structure (the stated hint):
#   1. generate_oak_id_vars()      — tag raw data with oak linkage keys
#   2. assign_no_ct()              — map verbatim term (DSTERM)
#   3. assign_ct()                 — map to controlled-terminology term (DSDECOD)
#   4. assign_datetime()           — derive ISO 8601 dates (DSDTC, DSSTDTC)
#   5. dplyr::mutate()             — derive identifiers, DSCAT, VISIT, VISITNUM
#   6. derive_study_day()          — compute DSSTDY from RFXSTDTC in DM
#   7. derive_seq()                — compute DSSEQ sequence number per subject
#
# Input:   pharmaverseraw::ds_raw  (850 rows × 13 cols)
# CT spec: study_ct (hardcoded per assessment instructions)
# DM:      pharmaversesdtm::dm     (for RFXSTDTC reference date)
#
# Outputs: data/ds.rds
#          data/ds.csv
#          output/q1/ds_summary.html (optional gt summary)
# ==============================================================================

suppressPackageStartupMessages({
  library(pharmaverseraw)
  library(pharmaversesdtm)
  library(sdtm.oak)
  library(dplyr)
  library(readr)
  library(here)
})

message("sdtm.oak version: ", as.character(packageVersion("sdtm.oak")))

# ── 1. Study controlled terminology ───────────────────────────────────────────
#
# Codelist C66727 maps collected disposition terms (collected_value) to the
# CDISC standard terms (term_value).  assign_ct() uses case-insensitive matching
# against collected_value, term_value, and term_synonyms.

study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = rep("C66727", 10),
  term_code     = c("C41331", "C25250", "C28554", "C48226", "C48227",
                    "C48250", "C142185","C49628", "C49632", "C49634"),
  term_value    = c("ADVERSE EVENT", "COMPLETED", "DEATH",
                    "LACK OF EFFICACY", "LOST TO FOLLOW-UP",
                    "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
                    "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR",
                    "WITHDRAWAL BY SUBJECT"),
  collected_value = c("Adverse Event", "Complete", "Dead",
                      "Lack of Efficacy", "Lost To Follow-Up",
                      "Physician Decision", "Protocol Violation",
                      "Trial Screen Failure", "Study Terminated By Sponsor",
                      "Withdrawal by Subject"),
  term_preferred_term = c("AE", "Completed", "Died", NA, NA, NA,
                          "Violation",
                          "Failure to Meet Inclusion/Exclusion Criteria",
                          NA, "Dropout"),
  term_synonyms = c("ADVERSE EVENT", "COMPLETE", "Death", NA, NA, NA,
                    NA, NA, NA, "Discontinued Participation")
)

message("study_ct loaded: ", nrow(study_ct), " terms in codelist C66727")

# ── 2. Load input data ────────────────────────────────────────────────────────

ds_raw_input <- pharmaverseraw::ds_raw
dm           <- pharmaversesdtm::dm

message("ds_raw: ", nrow(ds_raw_input), " rows x ", ncol(ds_raw_input), " cols")
message("dm:     ", nrow(dm), " rows x ", ncol(dm), " cols")

# ── 3. Visit lookup table ─────────────────────────────────────────────────────
#
# Maps INSTANCE (raw visit name) to SDTM VISIT (uppercase) and VISITNUM
# (study-protocol-defined numeric sequence, per CDISCPILOT01 schedule).

visit_lookup <- data.frame(
  stringsAsFactors = FALSE,
  VISIT    = c("SCREENING 1", "UNSCHEDULED 1.1", "BASELINE",
               "WEEK 2", "UNSCHEDULED 4.1", "WEEK 4",
               "UNSCHEDULED 5.1", "AMBUL ECG REMOVAL", "UNSCHEDULED 6.1",
               "WEEK 6", "WEEK 8", "UNSCHEDULED 8.2",
               "WEEK 12", "WEEK 16", "WEEK 20",
               "WEEK 24", "WEEK 26", "UNSCHEDULED 13.1", "RETRIEVAL"),
  VISITNUM = c(1.0, 1.1, 3.0,
               4.0, 4.1, 5.0,
               5.1, 6.0, 6.1,
               7.0, 8.0, 8.2,
               9.0, 10.0, 11.0,
               12.0, 13.0, 13.1, 201.0)
)

# ── 4. Pre-process raw data ───────────────────────────────────────────────────
#
# ds_raw has two types of disposition records:
#   a) IT.DSTERM populated     → main disposition event (DSTERM from IT.DSTERM)
#   b) OTHERSP populated       → protocol milestone/other event (DSTERM from OTHERSP)
#
# Combine into a single DSTERM_RAW and DSDECOD_RAW column, then filter rows
# that have no disposition information at all.  VISIT/VISITNUM are also derived
# here from INSTANCE via the visit_lookup table.

ds_raw_prep <- ds_raw_input %>%
  mutate(
    # Combine verbatim term: prefer IT.DSTERM, fall back to OTHERSP
    DSTERM_RAW  = coalesce(IT.DSTERM, OTHERSP),
    # Combine decoded term: prefer IT.DSDECOD, fall back to OTHERSP
    DSDECOD_RAW = coalesce(IT.DSDECOD, OTHERSP),
    # VISIT: uppercase INSTANCE (matches SDTM VISIT naming convention)
    VISIT_RAW   = toupper(INSTANCE)
  ) %>%
  # Drop rows where neither IT.DSTERM nor OTHERSP is populated
  filter(!is.na(DSTERM_RAW)) %>%
  # Join VISITNUM from the protocol visit schedule
  left_join(visit_lookup, by = c("VISIT_RAW" = "VISIT"))

message("Rows after filtering to non-missing DSTERM: ", nrow(ds_raw_prep))
message("Unique INSTANCE values: ", n_distinct(ds_raw_prep$INSTANCE))

# ── 5. Generate oak ID variables ──────────────────────────────────────────────
#
# generate_oak_id_vars() adds three linkage columns required by all sdtm.oak
# algorithms:
#   oak_id         — row-level identifier (within raw_src)
#   raw_source     — label for this raw dataset
#   patient_number — extracted from pat_var (PATNUM = "701-1015")

ds_raw_oak <- ds_raw_prep %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

message("oak ID vars generated; patient_number sample: ",
        paste(head(unique(ds_raw_oak$patient_number), 3), collapse = ", "))

# ── 6. Build DS domain via sdtm.oak pipeline ──────────────────────────────────
#
# Each assign_*/hardcode_* call:
#   • First call:  tgt_dat = NULL (default), id_vars not supplied
#   • Subsequent:  tgt_dat piped in, id_vars = oak_id_vars() required

message("\n── Building DS domain (sdtm.oak pipeline) ──────────────────────────")

ds <- # ── DSTERM: verbatim disposition term (no CT lookup) ─────────────────
  assign_no_ct(
    raw_dat = ds_raw_oak,
    raw_var = "DSTERM_RAW",
    tgt_var = "DSTERM"
  ) %>%

  # ── DSDECOD: standardised term via study CT (codelist C66727) ─────────────
  #   assign_ct() performs case-insensitive matching against collected_value,
  #   term_value, and term_synonyms.  This correctly maps:
  #     "Randomized"              → "RANDOMIZED"
  #     "Completed"               → "COMPLETED"
  #     "Screen Failure"          → "SCREEN FAILURE"
  #     "Study Terminated by..."  → "STUDY TERMINATED BY SPONSOR"
  #     "Final Lab Visit"         → "FINAL LAB VISIT"   (toupper fallback)
  #     "Final Retrieval Visit"   → "FINAL RETRIEVAL VISIT"
  assign_ct(
    raw_dat = ds_raw_oak,
    raw_var = "DSDECOD_RAW",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%

  # ── DSDTC: date (+ time when available) of CRF data collection ────────────
  #   DSDTCOL format: "m-d-y" (e.g. "02-18-2013" = month-day-year)
  #   DSTMCOL format: "H:M"   (e.g. "11:45" = hour:minute, 24-hour)
  assign_datetime(
    raw_dat = ds_raw_oak,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    raw_unk = c("UN", "UNK"),
    id_vars = oak_id_vars()
  ) %>%

  # ── DSSTDTC: start date of disposition event (date only) ──────────────────
  #   IT.DSSTDAT format: "m-d-y"
  assign_datetime(
    raw_dat = ds_raw_oak,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = "m-d-y",
    raw_unk = c("UN", "UNK"),
    id_vars = oak_id_vars()
  ) %>%

  # ── VISIT and VISITNUM: from pre-joined visit lookup ──────────────────────
  assign_no_ct(
    raw_dat = ds_raw_oak,
    raw_var = "VISIT_RAW",
    tgt_var = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw_oak,
    raw_var = "VISITNUM",
    tgt_var = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%

  # ── Study-level constants and derived identifiers ─────────────────────────
  #   USUBJID = "01-" + PATNUM  (matches pharmaversesdtm format)
  dplyr::mutate(
    STUDYID = "CDISCPILOT01",
    DOMAIN  = "DS",
    USUBJID = paste0("01-", patient_number),
    # DSTERM: uppercase to match SDTM convention
    DSTERM  = toupper(DSTERM),
    # DSCAT: derived from DSDECOD value
    DSCAT   = dplyr::case_when(
      DSDECOD == "RANDOMIZED"                        ~ "PROTOCOL MILESTONE",
      DSDECOD %in% c("FINAL LAB VISIT",
                     "FINAL RETRIEVAL VISIT")        ~ "OTHER EVENT",
      !is.na(DSDECOD)                                ~ "DISPOSITION EVENT",
      TRUE                                           ~ NA_character_
    )
  ) %>%

  # ── DSSTDY: study day relative to first dose (RFXSTDTC from DM) ──────────
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm,
    tgdt          = "DSSTDTC",
    refdt         = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%

  # ── DSSEQ: within-subject sequence number ─────────────────────────────────
  #   Sort by DSSTDTC (chronological), DSCAT (DISPOSITION EVENT before OTHER
  #   EVENT), then DSTERM as a final tiebreaker.
  derive_seq(
    tgt_dat  = .,
    tgt_var  = "DSSEQ",
    rec_vars = c("STUDYID", "USUBJID", "DSSTDTC", "DSCAT", "DSTERM")
  ) %>%

  # ── Select and order required SDTM DS variables ───────────────────────────
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  )

message("DS domain built: ", nrow(ds), " rows x ", ncol(ds), " cols")

# ── 7. Save outputs ───────────────────────────────────────────────────────────

dir.create(here("data"),        showWarnings = FALSE, recursive = TRUE)
dir.create(here("output","q1"), showWarnings = FALSE, recursive = TRUE)

saveRDS(ds, here("data", "ds.rds"))
message("Saved: data/ds.rds")

readr::write_csv(ds, here("data", "ds.csv"))
message("Saved: data/ds.csv")

# ── 8. Quality checks ─────────────────────────────────────────────────────────

message("\n── Quality Checks ────────────────────────────────────────────────────")
message("dim(ds): ", paste(dim(ds), collapse = " x "))

message("\nRequired variables present:")
required_vars <- c("STUDYID","DOMAIN","USUBJID","DSSEQ",
                   "DSTERM","DSDECOD","DSCAT",
                   "VISITNUM","VISIT","DSDTC","DSSTDTC","DSSTDY")
all_present <- all(required_vars %in% names(ds))
message("  All required: ", all_present)
missing_vars <- setdiff(required_vars, names(ds))
if (length(missing_vars) > 0) stop("Missing: ", paste(missing_vars, collapse = ", "))

message("\nUnique DSDECOD values:")
print(sort(unique(ds$DSDECOD)))

message("\nUnique DSCAT values:")
print(sort(unique(ds$DSCAT)))

message("\nDSCAT × DSDECOD distribution:")
print(
  ds %>%
    dplyr::count(DSCAT, DSDECOD) %>%
    dplyr::arrange(DSCAT, DSDECOD) %>%
    as.data.frame()
)

message("\nKey variable NA counts:")
for (v in required_vars) {
  n_na <- sum(is.na(ds[[v]]))
  if (n_na > 0) message("  ", v, ": ", n_na, " NAs")
}
n_na_critical <- sum(is.na(ds$DSTERM) | is.na(ds$DSDECOD) | is.na(ds$DSCAT))
message("  Rows with NA in DSTERM/DSDECOD/DSCAT: ", n_na_critical)

message("\nDSSTDY range: ", min(ds$DSSTDY, na.rm=TRUE),
        " to ", max(ds$DSSTDY, na.rm=TRUE),
        " (", sum(is.na(ds$DSSTDY)), " NA)")

message("\nFirst 20 rows:")
print(head(ds, 20))

message("\n== STATUS: SUCCESS — DS domain created error-free ==")
