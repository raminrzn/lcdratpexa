# lcdratpexa

The **Lung Cancer Death Risk Assessment Tool (LCDRAT)**. It gives an
ever-smoker's probability of dying from lung cancer within 5 years without CT
screening, allowing for death from other causes.

Katki HA, Kovalchik SA, Berg CD, Cheung LC, Chaturvedi AK. Development and
Validation of Risk Models to Select Ever-Smokers for CT Lung Cancer Screening.
*JAMA.* 2016;315(21):2300–2311.
[doi:10.1001/jama.2016.6255](https://doi.org/10.1001/jama.2016.6255)

Only LCDRAT is provided. The LCRAT (lung cancer incidence) and LYFS-CT (life
years gained) models in the same NCI software are not.

## Source

The calculation uses NCI's own code and fitted models from the **lcmodels**
R package, **version 4.1.3** (31 March 2026), from
[dceg.cancer.gov/tools/risk-assessment/lcmodels](https://dceg.cancer.gov/tools/risk-assessment/lcmodels).
Version 4.1.2 gives identical LCDRAT results.

- `R/lcmodels_code.R`: `risk.kovalchik()`, `cuts()` and `checkImputed()`,
  copied unchanged.
- `R/sysdata.rda`: the fitted Cox models `LCDRAT` (lung cancer death) and
  `cox.death` (death from other causes), copied unchanged.
- `.ld_covariates()` in `R/lcdrat.R`: the input coding and imputation
  statements of `lcmodels.main()`, unchanged.

The scripts in `data-raw/` make these copies.

## Inputs

| Field | Meaning |
|---|---|
| `age` | years, 50–80 |
| `sex` | male/female |
| `race` | white (non-Hispanic), black (non-Hispanic), hispanic, other (including Asian) |
| `years_smoked` | years smoked, more than 0 |
| `cigarettes_per_day` | average cigarettes per day, more than 0 |
| `years_quit` | years since quitting; 0 for current smokers |
| `bmi` | kg/m², optional |
| `education` | 1–6: below 12th grade, high school graduate, post high school (no college), associate degree or some college, bachelor's degree, graduate school; optional |
| `emphysema` | self-reported emphysema (lcmodels' label: COPD or emphysema), yes/no; optional |
| `family_history` | first-degree relatives with lung cancer: 0, 1, 2 (two or more); optional |
| `impute_missing` | default true |

**Missing inputs.** As in lcmodels, a missing BMI, education, emphysema or
family history is imputed from models fitted to the 2015 National Health
Interview Survey. Imputed emphysema and family history are probabilities, not
yes/no values. The output lists what was imputed. Set `impute_missing` to
false to refuse incomplete inputs instead.

## Outputs

- `lcdrat_5yr`: the probability, unrounded.
- `deaths_per_1000`: the same, per 1,000 people.
- `imputed`: the inputs that were imputed.
- `inputs`: the values used, including pack-years and any imputed values.

## Validation

- **NCI's lcmodels 4.1.3 (the authors' R package).** 40 cases covering every
  race, sex, education level, family history and emphysema value, current and
  former smokers, ages 50 and 80, pack-years at 30, 40 and 50, and 8
  cases with missing inputs. Largest relative difference: 4 × 10⁻¹⁵.
- **NCI's lcrisks Excel tool** (`lcrisk-tool.xlsx`, an independent
  implementation), the 32 complete cases recalculated in LibreOffice. After
  the correction described below, the largest relative difference was
  4.4 × 10⁻⁶ (largest absolute difference 0.00008 percentage points). The
  workbook stores its tables with fewer digits.
- The Excel tool's five sample people agree to about 10⁻⁶.

**A bug in the Excel tool.** In the calculator sheet's `cox.death.RR` column,
the pack-year 30–40 and 40–50 indicators are written as
`IF(Z2>=30 & Z2<40,1,0)`. `&` joins text in Excel; it is not AND. Microsoft
Excel therefore always returns 0 for these indicators. For people with 30–49.9
pack-years, the tool's other-cause mortality, and so its LCDRAT result, is
wrong. In the 8 such test cases it was 0.2%–4.6% too high (up to 0.87
percentage points). Other pack-years are unaffected. This package follows
lcmodels, which is correct. (The tool's USPSTF eligibility column uses the
same construct.)

## Limitations

- LCDRAT was developed in PLCO ever-smokers aged 55–74 and validated in US
  ever-smokers. NCI recommends it for ever-smokers aged 50–80; other ages are
  refused. Never-smokers are not covered.
- It estimates risk without screening; it does not give the benefit of
  screening.
- Imputed inputs, especially emphysema and family history, make the estimate
  less precise.

## Licence

lcmodels is distributed by NCI under GPL-2 and its
[Non-Proprietary Software Transfer Agreement](https://dceg.cancer.gov/tools/risk-assessment/lcmodels/license).
The software was created by US federal government employees, is provided as
is, and is **not intended for treating or diagnosing human subjects**. NCI
does not endorse this service. This package is GPL-2; lcmodels' DESCRIPTION
is in `inst/`.
