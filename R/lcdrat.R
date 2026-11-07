#' LCDRAT: 5-year risk of lung cancer death (Katki 2016)
#'
#' Probability of dying from lung cancer within 5 years without CT screening,
#' for an ever-smoker, accounting for death from other causes. Computed with
#' NCI's own code and fitted models from the lcmodels R package (version
#' 4.1.3): the lung cancer death model, the other-cause death model and their
#' absolute-risk calculation.
#'
#' The model was developed in PLCO ever-smokers aged 55-74 and validated for
#' US ever-smokers aged 50-80; ages outside 50-80 are refused.
#'
#' BMI, education, emphysema and family history may be left out (`NA`): as in
#' lcmodels (its default `impute.missing = TRUE`), they are then imputed from
#' models fitted to the NHIS 2015 survey. Imputed emphysema and family history
#' are probabilities, not 0/1 values. Set `impute_missing = FALSE` to require
#' them instead.
#'
#' @param age Age, years (50 to 80).
#' @param sex "male" or "female".
#' @param race "white" (non-Hispanic), "black" (non-Hispanic), "hispanic" or
#'   "other" (including Asian), or 0-3 as in lcmodels.
#' @param years_smoked Years smoked (more than 0).
#' @param cigarettes_per_day Cigarettes per day (more than 0).
#' @param years_quit Years since quitting; 0 for current smokers.
#' @param bmi Body mass index, kg/m2 (optional).
#' @param education Highest level, 1-6: 1 less than 12th grade, 2 high school
#'   graduate, 3 post high school (no college), 4 associate degree or some
#'   college, 5 bachelor's degree, 6 graduate school (optional).
#' @param emphysema Self-reported emphysema (lcmodels: COPD or emphysema),
#'   yes/no (optional).
#' @param family_history First-degree relatives (parents, siblings, children)
#'   with lung cancer: 0, 1 or 2 (two or more) (optional).
#' @param impute_missing Impute missing optional inputs as lcmodels does.
#' @return A list: `lcdrat_5yr` (probability), `deaths_per_1000`, `imputed`
#'   (names of imputed inputs) and `inputs` (values used).
#' @export
lcdrat_risk <- function(age, sex, race, years_smoked, cigarettes_per_day,
                        years_quit = 0, bmi = NA, education = NA, emphysema = NA,
                        family_history = NA, impute_missing = TRUE) {
  age <- .ld_num(age, "age", 50, 80)
  female <- .ld_choice(sex, "sex", c(male = 0, female = 1, m = 0, f = 1, `0` = 0, `1` = 1))
  race <- .ld_choice(race, "race", c(white = 0, black = 1, hispanic = 2, other = 3,
                                     asian = 3, `0` = 0, `1` = 1, `2` = 2, `3` = 3))
  smkyears <- .ld_num(years_smoked, "years_smoked", 0, 100, open_low = TRUE)
  cpd <- .ld_num(cigarettes_per_day, "cigarettes_per_day", 0, 200, open_low = TRUE)
  qtyears <- .ld_num(years_quit, "years_quit", 0, 100)
  if (smkyears + qtyears > age) {
    stop("`years_smoked` plus `years_quit` cannot exceed `age`.", call. = FALSE)
  }
  bmi <- .ld_opt_num(bmi, "bmi", 10, 80)
  edu6 <- .ld_opt_num(education, "education", 1, 6, whole = TRUE)
  emp <- .ld_opt_yesno(emphysema, "emphysema")
  fam <- .ld_opt_num(family_history, "family_history", 0, 2, whole = TRUE)
  missing <- c(bmi = is.na(bmi), education = is.na(edu6), emphysema = is.na(emp),
               family_history = is.na(fam))
  if (!isTRUE(impute_missing) && any(missing)) {
    stop("Missing: ", paste(names(missing)[missing], collapse = ", "),
         ". Supply them or set impute_missing = TRUE.", call. = FALSE)
  }

  # Columns 1-10 of the lcmodels input matrix.
  x <- matrix(c(age, female, smkyears, qtyears, cpd, race, emp, fam, bmi, edu6), nrow = 1)
  covar <- .ld_covariates(x, impute.missing = isTRUE(impute_missing))
  risk <- risk.kovalchik(0, 5, covar[, 1:11], .ld_model("LCDRAT"), .ld_model("cox.death"))
  if (!is.finite(risk)) stop("The risk could not be computed for these inputs.", call. = FALSE)
  list(lcdrat_5yr = risk,
       deaths_per_1000 = 1000 * risk,
       imputed = names(missing)[missing],
       inputs = list(age = age, female = female, race = race, years_smoked = smkyears,
                     cigarettes_per_day = cpd, years_quit = qtyears,
                     pack_years = covar$pkyr.cat, bmi = covar$bmi, education = covar$edu6,
                     emphysema = covar$emp, family_history = covar$fam.lung.trend))
}

# The covariates for LCDRAT, coded and imputed with the statements of
# lcmodels.main() (lcmodels 4.1.3, R/lcmodels.R), unchanged. With
# counterfactual.race = NULL, as in lcmodels()'s first call, the original race
# is used for imputation.
.ld_covariates <- function(x, impute.missing = TRUE) {
  age <- ifelse(is.na(x[,1])==0 & x[,1]>=0,x[,1],NA)
  female <- ifelse(is.na(x[,2])==0 & x[,2] %in% c(0,1),x[,2],NA)
  smkyears <- ifelse(is.na(x[,3])==0 & x[,3]>=0,x[,3],NA)
  qtyears <- ifelse(is.na(x[,4])==0 & x[,4]>=0,x[,4],NA)
  cpd <- ifelse(is.na(x[,5])==0 & x[,5]>=0,x[,5],NA)
  race <- as.factor(ifelse(is.na(x[,6])==0 & x[,6] %in% c(0,1,2,3),x[,6],NA))
  emp <- ifelse(is.na(x[,7])==0 & x[,7] %in% c(0,1),x[,7],NA)
  fam.lung.trend <- ifelse(is.na(x[,8])==0 & x[,8] %in% c(0,1,2),x[,8],NA)
  bmi <- ifelse(is.na(x[,9])==0 & x[,9]>0,x[,9],NA)
  edu6 <- ifelse(is.na(x[,10])==0 & x[,10] %in% c(1,2,3,4,5,6),x[,10],NA)
  pkyr.cat <- smkyears*cpd/20

  if (impute.missing==TRUE){
     bmi[is.na(bmi)==1] <- 29.994-0.1115*age[is.na(bmi)==1]+1.7528*I(race[is.na(bmi)==1]==1)+
       0.9577*I(race[is.na(bmi)==1]==2)-1.6921*I(race[is.na(bmi)==1]==3)+0.1091*female[is.na(bmi)==1]+
       0.0531*cpd[is.na(bmi)==1]-0.1608*log(pkyr.cat[is.na(bmi)==1])+0.9115*log(smkyears[is.na(bmi)==1])+
       0.9508*log(qtyears[is.na(bmi)==1]+1)
     bmi <- checkImputed(bmi, "bmi", warn=1)
     edu6[is.na(edu6)==1] <- 3.6862+0.1192*log(age[is.na(edu6)==1])-0.4907*I(race[is.na(edu6)==1]==1)-
       0.9958*I(race[is.na(edu6)==1]==2)-0.0708*I(race[is.na(edu6)==1]==3)+0.0823*female[is.na(edu6)==1]-
       0.2484*I(cpd[is.na(edu6)==1]>=20)-0.0434*log(pkyr.cat[is.na(edu6)==1])-0.0207*smkyears[is.na(edu6)==1]+
       0.0964*log(qtyears[is.na(edu6)==1]+1)
     edu6 <- checkImputed(edu6, "education", warn=1)
     lp <- -5.2038+1.1218*log(age[is.na(fam.lung.trend)==1])-1.022*I(race[is.na(fam.lung.trend)==1]==1)-
           1.4558*I(race[is.na(fam.lung.trend)==1]==2)-1.3052*I(race[is.na(fam.lung.trend)==1]==3)+
           0.0917*female[is.na(fam.lung.trend)==1]-0.3626*log(cpd[is.na(fam.lung.trend)==1])+
           0.4853*log(pkyr.cat[is.na(fam.lung.trend)==1])-0.3273*sqrt(smkyears[is.na(fam.lung.trend)==1])-
           0.0184*qtyears[is.na(fam.lung.trend)==1]
     lp <- checkImputed(lp, "number of parents with lung cancer", warn=1)
     fam.lung.trend[is.na(fam.lung.trend)==1] <- exp(lp)/(1+exp(lp))
     lp <- -9.2854+0.1946*sqrt(age[is.na(emp)==1])-0.4025*I(race[is.na(emp)==1]==1)-1.139*I(race[is.na(emp)==1]==2)-
           1.0095*I(race[is.na(emp)==1]==3)-0.0142*female[is.na(emp)==1]+0.1818*I(cpd[is.na(emp)==1] >= 20)+
           0.0125*pkyr.cat[is.na(emp)==1]+0.7645*sqrt(smkyears[is.na(emp)==1])+0.0198*sqrt(qtyears[is.na(emp)==1])
     lp <- checkImputed(lp, "lung disease", warn=1)
     emp[is.na(emp)==1] <- exp(lp)/(1+exp(lp))
   }

  data.frame(age=age, bmi=bmi, cpd=cpd, emp=emp, fam.lung.trend=fam.lung.trend,
             female=female, qtyears=qtyears, smkyears=smkyears, race=race,
             edu6=edu6, pkyr.cat=pkyr.cat)
}

# The fitted models refer to cuts() in their formulas; predict() must find
# this package's copy, so their terms point to the package namespace.
.ld_model <- function(name) {
  m <- get(name, envir = asNamespace("lcdratpexa"))
  environment(m$terms) <- asNamespace("lcdratpexa")
  m
}

.ld_num <- function(x, field, lo, hi, open_low = FALSE) {
  if (is.character(x)) x <- suppressWarnings(as.numeric(x))
  bad <- !is.numeric(x) || length(x) != 1L || is.na(x) || x > hi ||
    (if (open_low) x <= lo else x < lo)
  if (bad) {
    stop("`", field, "` must be a number ", if (open_low) "above " else "from ", lo,
         if (open_low) " and up to " else " to ", hi, ".", call. = FALSE)
  }
  as.numeric(x)
}

.ld_opt_num <- function(x, field, lo, hi, whole = FALSE) {
  if (is.null(x) || (length(x) == 1L && (is.na(x) || identical(trimws(as.character(x)), "") ||
                                          tolower(as.character(x)) == "unknown"))) {
    return(NA_real_)
  }
  v <- .ld_num(x, field, lo, hi)
  if (whole && v != round(v)) stop("`", field, "` must be a whole number.", call. = FALSE)
  v
}

.ld_opt_yesno <- function(x, field) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_real_)
  key <- tolower(trimws(as.character(x)))
  if (key %in% c("yes", "1", "true")) return(1)
  if (key %in% c("no", "0", "false")) return(0)
  if (key %in% c("", "unknown")) return(NA_real_)
  stop("`", field, "` must be yes, no or unknown.", call. = FALSE)
}

.ld_choice <- function(x, field, map) {
  key <- tolower(gsub("[^a-z0-9]", "", tolower(as.character(x))))
  if (length(key) != 1L || !key %in% names(map)) {
    stop("Unrecognised `", field, "`: \"", paste(x, collapse = ","), "\".", call. = FALSE)
  }
  map[[key]]
}
