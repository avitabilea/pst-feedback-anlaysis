# Replication Guide

This document describes how to replicate the analysis in the paper. Scripts should be run in the order listed below.

---

## Data

The raw data are publicly available on OpenICPSR:

> https://www.openicpsr.org/openicpsr/project/218861/version/V2/view

Download `PST Data.xlsx` and place it in the `raw data/` directory of this repository.

The reliability and validity checks in Step 6 also require two additional files that should be placed in the corresponding subdirectories:
- `qual coding/Refl_CTO_response_analysis_07082024.dta` — qualitative coding data used for external validity
- `validity/validity_coding - KV.xlsx` — manually coded subsample used for human-LLM agreement checks

---

## Software Requirements

### Python
- Python 3.x
- Required packages: `openai`, `pandas`, `python-dotenv`, `tqdm`

```bash
pip install openai pandas python-dotenv tqdm
```

### R
- R 4.x or higher
- Required packages:

```r
install.packages(c(
  "tidyverse", "openxlsx", "lme4", "haven",
  "fixest", "modelsummary", "tinytable",
  "sandwich", "lmtest", "marginaleffects",
  "broom", "glue", "showtext", "scales",
  "psych", "car"
))
```

---

## Setup

### 1. OpenAI API Key

Steps 1 and 2 require an OpenAI API key. Create a `.env` file in the root of the repository:

```
OPENAI_API_KEY=your_api_key_here
```

### 2. Output Directories

Scripts `02_plots.R`, `03_analyses.R`, `05_reliability_and_validity.R`, and `06_examples.R` write figures and LaTeX tables to a local Overleaf directory. **Before running these scripts, update the output file paths in each script to match your local directory structure.**

---

## Replication Steps

### Step 1 — LLM Coding of Supervisor Feedback

```bash
python scripts/00a_llm_feedback.py
```

Submits each supervisor feedback document from `raw data/PST Data.xlsx` to the GPT-4o-mini API using a structured single-shot prompt. Codes four quality indicators (strengths, specific examples, areas for improvement, next steps) and identifies the primary area for improvement across eight teaching skill categories.

**Output:** `processed data/2025.04.29 - Feedback Analysis.csv`

---

### Step 2 — LLM Coding of PST Reflections

```bash
python scripts/00b_llm_reflections.py
```

Applies the same coding procedure to PST self-reflection documents. Saves intermediate checkpoints every 100 observations.

**Output:** `processed data/2025.04.30 - Reflections Analysis.csv`

---

### Step 3 — Create Analysis Dataset

Run `scripts/01_creating_analysis_data.R` in R.

Merges the raw data with the LLM-coded feedback and reflection files, recodes variables (e.g., collapses "Time Management" into "Classroom Management"), constructs binary skill indicators, and estimates Best Linear Unbiased Predictors (BLUPs) for supervisor, PST, and school effects via a multilevel model. BLUPs are rescaled to standard deviation units at each level.

**Outputs:**
- `processed data/analysis_data.RDS` — main analysis dataset (R format)
- `processed data/analysis_data.dta` — Stata format

---

### Step 4 — Generate Figures

Run `scripts/02_plots.R` in R.

Produces all figures in the paper, including distributions of feedback and reflection length, quality indicator prevalence, area-for-improvement categories, supervisor variation, and evaluation score distributions. Figures are saved as PDFs to the output directory.

---

### Step 5 — Main Analyses

Run `scripts/03_analyses.R` in R.

Estimates all regression models reported in the paper: predictors of feedback characteristics, relationships between supervisor feedback and PST reflections, associations between feedback content and evaluation scores, and links to downstream outcomes (student exam scores, teaching entry, school choice, and teacher attrition). Saves formatted LaTeX tables to the output directory.

This script uses the helper function `bi_and_muti_variate_feols()` defined in `scripts/XX_univar_and_multivar_feols.R`.

---

### Step 6 — Reliability and Validity Checks

Run `scripts/05_reliability_and_validity.R` in R.

Produces the reliability and validity tables reported in the Online-Only Technical Appendix:
- **Table B3:** LLM agreement rates across two independent coding runs
- **Table B4:** Human-LLM agreement rates based on a manually coded random sample of 100 documents
- **Table B5:** Regressions predicting LLM-coded areas for improvement using independent qualitative coding

---

### Step 7 — Coding Examples

Run `scripts/06_examples.R` in R.

Extracts representative examples of each quality indicator and area-for-improvement category for inclusion in the paper. Uses `set.seed(939599)` for reproducible sampling.

---

## Notes on Reproducibility

- **LLM outputs:** The LLM coding uses GPT-4o-mini with `temperature=0.2` and `seed=42`. Despite seeding, OpenAI does not guarantee bit-for-bit deterministic outputs across API calls or model versions, so exact replication of the coded CSVs is not assured. Agreement rates across two independent runs are reported in Table B3 of the Technical Appendix and are above 94% for all tasks.
- **Random sampling:** Example selection in `06_examples.R` uses a fixed R seed (`set.seed(939599)`) and is fully reproducible given the same analysis dataset.
- **Processed data:** The intermediate CSV files produced in Steps 1–2 and the final RDS/DTA files produced in Step 3 are excluded from this repository (due to data use restrictions) and must be generated by running the scripts.
