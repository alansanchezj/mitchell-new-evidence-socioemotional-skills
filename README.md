# Human Capital Development: New Evidence on the Production of Socioemotional Skills

**Mark Mitchell, Marta Favara, Catherine Porter, Alan Sánchez**

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

*Journal of Human Resources*, 60(4), 1175–1216, July 2025.
https://doi.org/10.3368/jhr.1120-11342R1

## Abstract

We estimate a dynamic model of socio-emotional skill development between ages eight and 22 for a Peruvian cohort born in 1994. At age eight there is no wealth gradient, in contrast to cognitive skills. However, by age 12, inequalities emerge and widen through age 19, driven by differential household investments, and cross-productivity with cognitive skills. In early adulthood, we separate socio-emotional skills into two distinct domains—social skills and task effectiveness—that evolve differently and are differently correlated with risky behaviors, such as smoking or taking drugs. Unequal initial household resources perpetuate inequality across generations through cognitive and task effectiveness skills.

## Overview

This repository contains the Stata code to replicate the results in the paper. The data is available separately at the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JWAKKN).

## Folder structure

```
.
├── Dofiles/                  # Main Stata do-files (tables and figures)
├── Dofiles online appendix/  # Online appendix do-files
├── Programs/                 # Helper programs called by the do-files
├── Data/                     # Datasets (not included — see Dataverse)
└── Output/                   # Tables and figures (not included)
```

## Requirements

**Software:** Stata

## Replication steps

1. Create the following folder structure locally:
   ```
   Replication files/
   ├── Dofiles/
   ├── Dofiles online appendix/
   ├── Programs/
   ├── Data/
   └── Output/
   ```

2. Download the data files from the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JWAKKN) and place them in `Replication files/Data/`:
   - `Big5_v13.dta`
   - `HH composition.dta`
   - `oc_measures_peru_R1-5_v13.dta`
   - `Other Outcomes_23Mar2020.dta`
   - `peru_constructed.dta`
   - `sample_postest.dta`
   - `YLIncome_allrounds.dta`

3. Place all do-files in their respective subfolders and update the folder path at the top of each do-file to match your local directory

4. **Run the main do-files in order:**

| Order | Do-file | Output |
|---|---|---|
| 1 | `_1_Descriptives_table1` | Table 1 — descriptive statistics |
| 2 | `_2_Estimates_baseline_tables2345` | Tables 2–5 — Cobb-Douglas production functions |
| 3 | `_3_AdultOutcomes_table6` | Table 6 — skills and risky behaviours at age 22 |
| 4 | `_4_Descriptives_figures12` | Figures 1–2 — skills gradients by family wealth |
| 5 | `_5_Simulations_figures345` | Figures 3–5 — counterfactual income simulations |
| 6 | `_6_Appendix_tablesA12` | Tables A1–A2 — measurement parameters |

## Programs folder

The `Programs/` folder contains helper files called by the main do-files — they do not need to be run directly:

- **`inputs.do`** — defines skill and investment measures used in the production functions
- **`programs.do`** — defines the estimation programs for investment and production functions
- **`initial_conditions.do`** — estimates initial period measurement parameters and the distribution of initial conditions

## Additional notes

**Investment and production functions (`_2_Estimates_baseline_tables2345`):** Loads all demographic, skill, and investment measures from the Peru Young Lives data; calls `inputs.do`, `programs.do`, and `initial_conditions.do`; loops through each round estimating Cobb-Douglas functions; and saves formatted LaTeX-ready tables to `Output/`. Stored estimates are also used downstream in the simulations.

**Adult outcomes (`_3_AdultOutcomes_table6`):** Uses a post-estimation dataset with residualised skill/investment measures. Run `_2_Estimates_baseline_tables2345` first to ensure this dataset is up to date.

**Simulations (`_5_Simulations_figures345`):** First runs `_2_Estimates_baseline_tables2345` to retrieve parameter estimates, then draws a synthetic sample at age 8 using `drawnorm` and forward-simulates human capital trajectories. Also estimates counterfactuals with a one-time 25% income increase (for households below 250 soles — the *Juntos* threshold) at ages 8, 12, and 15.
