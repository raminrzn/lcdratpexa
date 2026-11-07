row_risk <- function(cs, i, impute = TRUE) {
  na <- function(v) if (is.null(v) || is.na(v)) NA else v
  lcdrat_risk(age = cs$age[i], sex = cs$gender[i], race = cs$race[i],
              years_smoked = cs$smkyears[i], cigarettes_per_day = cs$cpd[i],
              years_quit = cs$qtyears[i], bmi = na(cs$bmi[i]), education = na(cs$edu6[i]),
              emphysema = na(cs$emp[i]), family_history = na(cs$fam[i]),
              impute_missing = impute)$lcdrat_5yr
}
cases <- jsonlite::fromJSON(test_path("lcmodels_cases.json"))

test_that("results equal NCI's lcmodels 4.1.3 (authors' R package)", {
  # 40 cases computed with lcmodels::lcmodels() 4.1.3 (column 2, LCDRAT): every
  # race, sex, education level, family history and emphysema value, BMI around
  # 18.5, pack-years at 30, 40 and 50, current and former smokers, ages 50 and
  # 80, and 8 cases with BMI, education, emphysema or family history missing
  # (imputed as lcmodels does).
  for (i in seq_len(nrow(cases))) {
    expect_equal(row_risk(cases, i), cases$lcdrat[i], tolerance = 1e-12)
  }
})

test_that("results agree with NCI's lcrisks Excel tool (corrected)", {
  # The 32 complete cases recalculated in lcrisk-tool.xlsx (LibreOffice), with
  # the '&' conditions of its cox.death.RR column corrected to AND() (see
  # README). The workbook stores its tables with fewer digits.
  xl <- jsonlite::fromJSON(test_path("lcrisks_excel_corrected.json"))
  full <- cases[stats::complete.cases(cases[, c("bmi", "edu6", "emp", "fam")]), ]
  expect_equal(nrow(full), nrow(xl))
  for (i in seq_len(nrow(full))) {
    expect_equal(row_risk(full, i), xl$lcdrat_excel[i], tolerance = 1e-5)
  }
})

test_that("the Excel tool's own sample people are reproduced", {
  # lcrisk-tool.xlsx 'data' sheet, people 1-5; values from lcmodels 4.1.3. The
  # workbook's saved values agree to 1e-6 (these all have 50+ pack-years).
  people <- list(
    list(66, "male", "white", 43, 36, 0, 23, 3, "no", 0, 0.043166154997762822),
    list(58, "female", "black", 37, 36, 0, 28, 5, "yes", 2, 0.066582185383561879),
    list(75, "male", "hispanic", 45, 40, 9, 26, 4, "yes", 0, 0.034829232410007127),
    list(72, "female", "hispanic", 42, 24, 6, 27, 5, "no", 2, 0.034355530718684746),
    list(56, "male", "other", 29, 40, 6, 24, 5, "yes", 0, 0.0064221493425762072))
  for (p in people) {
    r <- do.call(lcdrat_risk, unname(p[1:10]))
    expect_equal(r$lcdrat_5yr, p[[11]], tolerance = 1e-12)
  }
})

test_that("missing inputs are imputed and reported, or refused", {
  r <- lcdrat_risk(60, "male", "white", 35, 20, 5)
  expect_setequal(r$imputed, c("bmi", "education", "emphysema", "family_history"))
  expect_gt(r$inputs$emphysema, 0)
  expect_lt(r$inputs$emphysema, 1)
  expect_error(lcdrat_risk(60, "male", "white", 35, 20, 5, impute_missing = FALSE), "Missing")
})

test_that("inputs are checked", {
  expect_error(lcdrat_risk(49, "male", "white", 30, 20), "age")
  expect_error(lcdrat_risk(81, "male", "white", 30, 20), "age")
  expect_error(lcdrat_risk(60, "male", "white", 0, 20), "years_smoked")
  expect_error(lcdrat_risk(60, "male", "white", 30, 0), "cigarettes_per_day")
  expect_error(lcdrat_risk(60, "male", "white", 50, 20, 15), "cannot exceed")
  expect_error(lcdrat_risk(60, "male", "asian-american", 30, 20), "race")
  expect_error(lcdrat_risk(60, "male", "white", 30, 20, family_history = 3), "family_history")
  expect_error(lcdrat_risk(60, "male", "white", 30, 20, education = 7), "education")
})

test_that("gateway returns full precision", {
  x <- get_default_input()
  g <- jsonlite::fromJSON(gateway(model_input = x))
  expect_equal(g$lcdrat_5yr, 0.043166154997762822, tolerance = 1e-14)
})
