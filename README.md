# Roche Analytical Data Science Programmer — Coding Assessment

Solutions to the Pharmaverse Expertise and Python Coding Assessment for the Roche PD Data Science ADS Programmer position.

---

## Repository Structure

Each question folder is self-contained: the script, execution log, and all outputs live together.

```
.
├── README.md
├── 00_install_packages.R              # Install all required R packages
├── ADS_P_Coding_Assessment.pdf        # Original assessment PDF
│
├── question_1_sdtm/                   # Q1: SDTM DS Domain Creation
│   ├── 01_create_ds_domain.R          # Main script (sdtm.oak pipeline)
│   ├── 01_log.txt                     # Execution log — evidence of error-free run
│   ├── ds.rds                         # Output: DS domain (R binary)
│   └── ds.csv                         # Output: DS domain (850 rows × 12 cols)
│
├── question_2_adam/                   # Q2: ADaM ADSL Dataset Creation
│   ├── create_adsl.R                  # Main script (admiral pipeline)
│   ├── create_adsl_log.txt            # Execution log — evidence of error-free run
│   ├── adsl.rds                       # Output: ADSL dataset (R binary)
│   └── adsl.csv                       # Output: ADSL dataset (306 rows × 50 cols)
│
├── question_3_tlg/                    # Q3: TLG — Adverse Events Reporting
│   ├── 01_create_ae_summary_table.R   # TEAE summary table (gtsummary)
│   ├── 01_log.txt                     # Execution log
│   ├── 02_create_visualizations.R     # ggplot2 visualizations
│   ├── 02_log.txt                     # Execution log
│   ├── ae_summary_table.html          # Output: TEAE nested SOC/PT table (HTML)
│   ├── ae_simple_summary.html         # Output: Any-TEAE flat overview (HTML)
│   ├── ae_severity_distribution.png   # Output: Plot 1 — AE severity by treatment arm
│   └── ae_top10_forest_plot.png       # Output: Plot 2 — Top 10 AEs with 95% CI
│
└── question_4_python/                 # Q4 (Bonus): GenAI Clinical Data Assistant
    ├── clinical_data_agent.py         # Main solution: ClinicalTrialDataAgent class
    ├── run_queries.py                 # Demo script: runs 3 example queries
    ├── run_queries_output.txt         # Sample output from a successful run
    ├── requirements.txt               # Python dependencies (pandas, anthropic)
    ├── export_adae.R                  # R helper: export pharmaverseadam::adae → CSV
    └── data/
        └── adae.csv                   # Input data (1191 rows × 107 cols)
```

---

## Questions Overview

| # | Domain | Key Packages | Status |
|---|--------|-------------|--------|
| 1 | SDTM DS Domain | `{sdtm.oak}`, `{pharmaverseraw}` | Required ✅ |
| 2 | ADaM ADSL | `{admiral}`, `{pharmaversesdtm}` | Required ✅ |
| 3 | TLG – AE Reporting | `{gtsummary}`, `{ggplot2}`, `{pharmaverseadam}` | Required ✅ |
| 4 | GenAI Assistant | Python, Anthropic API, Pandas | Bonus ✅ |

---

## How to Run

### Prerequisites

- **R**: 4.2.0 or higher
- **Python**: 3.9+ (Question 4 only)

### Step 1 — Install R packages

```r
source("00_install_packages.R")
```

### Step 2 — Run each question

**Question 1: SDTM DS Domain**

```r
source("question_1_sdtm/01_create_ds_domain.R")
```

Outputs to: `question_1_sdtm/ds.rds`, `question_1_sdtm/ds.csv`

---

**Question 2: ADaM ADSL**

```r
source("question_2_adam/create_adsl.R")
```

Outputs to: `question_2_adam/adsl.rds`, `question_2_adam/adsl.csv`

---

**Question 3: TLG — AE Reporting**

```r
source("question_3_tlg/01_create_ae_summary_table.R")
source("question_3_tlg/02_create_visualizations.R")
```

Outputs to: `question_3_tlg/ae_summary_table.html`, `question_3_tlg/ae_simple_summary.html`, `question_3_tlg/ae_severity_distribution.png`, `question_3_tlg/ae_top10_forest_plot.png`

---

**Question 4: GenAI Clinical Data Assistant (Bonus)**

```bash
cd question_4_python
pip install -r requirements.txt

# Without API key — keyword-based mock, no setup needed
python run_queries.py

# With Anthropic API key — uses claude-sonnet-4-6 live
export ANTHROPIC_API_KEY="sk-ant-..."
python run_queries.py
```

The script prints results to stdout. To capture output:

```bash
python run_queries.py > run_queries_output.txt
```

---

## Required R Packages

| Package | Purpose |
|---------|---------|
| `sdtm.oak` | SDTM domain creation (Q1) |
| `pharmaverseraw` | Raw source data (Q1) |
| `admiral` | ADaM derivations (Q2) |
| `pharmaversesdtm` | SDTM reference datasets (Q2) |
| `pharmaverseadam` | ADaM reference datasets (Q3) |
| `gtsummary` | Summary tables (Q3) |
| `gt` | Table formatting (Q3) |
| `ggplot2` | Visualizations (Q3) |
| `dplyr` | Data manipulation |
| `lubridate` | Date/time handling |
| `readr` | CSV read/write |

---

## Key Design Decisions

### Q1 — SDTM DS Domain
- Uses the full `{sdtm.oak}` pipeline: `generate_oak_id_vars()` → `assign_no_ct()` → `assign_ct()` → `assign_datetime()` → `derive_study_day()` → `derive_seq()`
- `DSTERM` = `toupper(coalesce(IT.DSTERM, OTHERSP))`
- `DSDECOD` mapped via codelist C66727; case-insensitive matching handles all 10 controlled terms
- `DSCAT` logic: PROTOCOL MILESTONE (RANDOMIZED), OTHER EVENT (final/retrieval visits), DISPOSITION EVENT (all others)
- 52 screen-failure subjects correctly have `DSSTDY = NA` (no first dose date)

### Q2 — ADaM ADSL
- Based on `pharmaversesdtm::dm`; all custom variables derived with `{admiral}` functions
- `TRTSDTM`/`TRTSTMF`: imputed at hour level ("H") since `EX.EXSTDTC` contains dates only
- `ITTFL`: "Y" for all 3 randomised arms, "N" for Screen Failure (ARM is populated but not randomised)
- `LSTAVLDT`: maximum of VS, AE, DS, and EX dates (last known alive date)
- Output: 306 rows × 50 columns

### Q3 — TLG
- **Table**: `tbl_hierarchical()` (gtsummary ≥ 2.0.3) with AESOC → AEDECOD hierarchy, unique subject counts, sorted by descending overall frequency
- **Plot 1**: Stacked bar chart of AE severity (MILD/MODERATE/SEVERE) by treatment arm
- **Plot 2**: Forest plot of top 10 AEs by incidence rate with 95% Clopper–Pearson CIs
- Safety population: Placebo N=86, Xanomeline High Dose N=72, Xanomeline Low Dose N=96

### Q4 — GenAI Assistant
- `ClinicalTrialDataAgent` class with `parse_question()` → `execute_query()` → `ask()` flow
- LLM: `claude-sonnet-4-6` via `anthropic` Python package; system prompt includes full schema
- LLM returns structured JSON: `{"target_column": "...", "filter_value": "..."}`
- Keyword-based mock fallback activates automatically when `ANTHROPIC_API_KEY` is unset
- Verified results: MODERATE severity → 136 subjects, DIZZINESS → 22, CARDIAC DISORDERS → 44
