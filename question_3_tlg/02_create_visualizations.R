# ==============================================================================
# 02_create_visualizations.R
# Question 3, Task 2: Adverse Event Visualizations
# ------------------------------------------------------------------------------
# Produces two ggplot2 figures:
#
#   Plot 1 — AE Severity Distribution by Treatment Arm (stacked bar chart)
#     • X-axis: Treatment arm (ACTARM)
#     • Y-axis: Count of AE records (not unique subjects — sample output is bar
#               height ~400 per arm, consistent with record-level counting)
#     • Fill:   Severity (AESEV): SEVERE at bottom, MODERATE middle, MILD top
#     • Filter: TRTEMFL == "Y"
#
#   Plot 2 — Top 10 Most Frequent AEs with 95% CI (forest / dot plot)
#     • Based on number of unique SUBJECTS with each AE term (AETERM)
#     • Denominator: all treated subjects in ADSL (safety population)
#     • CI method:   95% Clopper-Pearson exact binomial
#     • X-axis:      Percentage of patients
#     • Y-axis:      AE terms ordered by descending frequency (top at top)
#
# Inputs:  pharmaverseadam::adae, pharmaverseadam::adsl
# Outputs: output/q3/ae_severity_distribution.png
#          output/q3/ae_top10_forest_plot.png
# ==============================================================================

suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(here)
})

# ── 1. Load data ──────────────────────────────────────────────────────────────

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Safety population: treated arms only (exclude Screen Failure)
arm_order <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

arm_n <- adsl %>%
  filter(ACTARM %in% arm_order) %>%
  count(ACTARM, name = "N")

total_n <- sum(arm_n$N)   # 254 subjects

message("Safety population: ", total_n, " subjects")
print(arm_n)

# Filter to TEAEs in treated arms
teae <- adae %>%
  filter(TRTEMFL == "Y", ACTARM %in% arm_order)

message("TEAE records: ", nrow(teae),
        " | Subjects: ", n_distinct(teae$USUBJID))

# Output directory
dir.create(here("output", "q3"), showWarnings = FALSE, recursive = TRUE)

# ── 2. Plot 1: AE Severity Distribution by Treatment Arm ─────────────────────
#
# Counts AE *records* (not unique subjects) to match the sample output's y-axis
# scale (~400 per arm).  Severity is ordered so SEVERE stacks at the bottom,
# MODERATE in the middle, and MILD (most common) at the top.

message("\n── Plot 1: Severity Distribution ────────────────────────────────────")

# Count AE records per arm × severity
plot1_data <- teae %>%
  filter(!is.na(AESEV)) %>%
  # Factor severity: first level → bottom of stack in geom_col
  mutate(
    AESEV  = factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD")),
    ACTARM = factor(ACTARM, levels = arm_order)
  ) %>%
  count(ACTARM, AESEV, name = "ae_count")

message("Plot 1 counts:")
print(plot1_data)

# Colour palette matching the sample output (salmon/coral, green, steel blue)
severity_colors <- c(
  "MILD"     = "#FA8072",   # salmon
  "MODERATE" = "#3CB371",   # medium sea green
  "SEVERE"   = "#6495ED"    # cornflower blue
)

p1 <- ggplot(plot1_data, aes(x = ACTARM, y = ae_count, fill = AESEV)) +
  geom_col(position = "stack", width = 0.6) +
  scale_fill_manual(
    values = severity_colors,
    # Legend shows MILD first (matches sample output)
    breaks = c("MILD", "MODERATE", "SEVERE"),
    labels = c("MILD", "MODERATE", "SEVERE"),
    name   = "Severity/Intensity"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "AE severity distribution by treatment",
    x     = "Treatment Arm",
    y     = "Count of AEs"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x     = element_text(angle = 15, hjust = 1),
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    panel.grid.major.y = element_line(colour = "grey90"),
    axis.line       = element_line(colour = "grey40")
  )

ggsave(
  here("output", "q3", "ae_severity_distribution.png"),
  plot   = p1,
  width  = 8,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
message("Saved: output/ae_severity_distribution.png")

# ── 3. Plot 2: Top 10 Most Frequent AEs — Forest / Dot Plot with 95% CI ──────
#
# Uses AETERM (verbatim term) per the assessment specification.
# Clopper-Pearson exact 95% CI is calculated with the base R qbeta() function:
#   lower = qbeta(0.025, x,   n - x + 1)
#   upper = qbeta(0.975, x+1, n - x)
# where x = subjects with the AE, n = total subjects in safety population.
# This is numerically equivalent to the standard binom.test exact CI.

message("\n── Plot 2: Top 10 AE Forest Plot ────────────────────────────────────")

# Count unique subjects per AETERM
ae_subj_counts <- teae %>%
  group_by(AETERM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(desc(n))

message("Unique AE terms (AETERM): ", nrow(ae_subj_counts))

# Select top 10 by subject count
ae_top10 <- ae_subj_counts %>%
  slice_head(n = 10) %>%
  mutate(
    pct      = n / total_n,
    # Clopper-Pearson exact 95% CI (handles edge cases at 0 and total_n)
    ci_lower = qbeta(0.025, n,     total_n - n + 1),
    ci_upper = qbeta(0.975, n + 1, total_n - n),
    ci_lower = ifelse(n == 0,        0, ci_lower),
    ci_upper = ifelse(n == total_n,  1, ci_upper),
    # Order factor so highest-frequency term appears at the top of the y-axis
    AETERM   = factor(AETERM, levels = rev(AETERM))
  )

message("Top 10 AEs:")
print(ae_top10 %>% select(AETERM, n, pct, ci_lower, ci_upper))

p2 <- ggplot(ae_top10, aes(x = pct, y = AETERM)) +
  # CI error bars first (so dot sits on top)
  # Use geom_errorbar() with orientation = "y" (horizontal CIs on a flipped plot)
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    orientation = "y",
    width     = 0.25,
    colour    = "grey40",
    linewidth = 0.7
  ) +
  # Point estimate
  geom_point(size = 3, colour = "black") +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.1)),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", total_n, " subjects; 95% Clopper-Pearson CIs"),
    x        = "Percentage of Patients (%)",
    y        = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, colour = "grey40", size = 10),
    axis.text.y     = element_text(size = 10),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.minor.x = element_blank(),
    axis.line.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )

ggsave(
  here("output", "q3", "ae_top10_forest_plot.png"),
  plot   = p2,
  width  = 8,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
message("Saved: output/ae_top10_forest_plot.png")

# ── 4. Verification summary ───────────────────────────────────────────────────

message("\n── Verification ──────────────────────────────────────────────────────")
message("Plot 1 arms: ", paste(levels(plot1_data$ACTARM), collapse = ", "))
message("Plot 1 severity levels: ", paste(levels(plot1_data$AESEV), collapse = ", "))
message("Plot 2 total N: ", total_n)
message("Plot 2 top 10 AEs (AETERM, n, %):")
print(ae_top10 %>%
        select(AETERM, n, pct) %>%
        mutate(pct = scales::percent(pct, accuracy = 0.1)))

message("\n== STATUS: SUCCESS — all outputs written error-free ==")
