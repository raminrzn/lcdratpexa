.ld_vars <- c("age", "sex", "race", "years_smoked", "cigarettes_per_day", "years_quit",
              "bmi", "education", "emphysema", "family_history", "impute_missing")
.ld_alias <- c(gender = "sex", smkyears = "years_smoked", cpd = "cigarettes_per_day",
               qtyears = "years_quit", edu6 = "education", emp = "emphysema",
               fam = "family_history", fam_lung = "family_history")

#' LCDRAT (ModelsCloud entry point)
#' @param model_input Named list of inputs; see [lcdrat_risk()].
#' @param ... The same fields as named arguments.
#' @return See [lcdrat_risk()].
#' @export
model_run <- function(model_input = NULL, ...) {
  args <- .ld_normalize(model_input, list(...), .ld_vars, .ld_alias)
  if (is.null(args)) stop("No input supplied.", call. = FALSE)
  do.call(lcdrat_risk, args)
}

#' Example LCDRAT inputs: two of the sample people in NCI's lcrisks Excel tool
#' @param n Optional number of rows.
#' @param ... Ignored.
#' @export
get_sample_input <- function(n = NULL, ...) {
  df <- data.frame(age = c(66, 58), sex = c("male", "female"), race = c("white", "black"),
                   years_smoked = c(43, 37), cigarettes_per_day = c(36, 36),
                   years_quit = c(0, 0), bmi = c(23, 28), education = c(3, 5),
                   emphysema = c("no", "yes"), family_history = c(0, 2),
                   stringsAsFactors = FALSE)
  if (!is.null(n)) df <- utils::head(df, n)
  df
}

#' Default LCDRAT input: the first sample person in NCI's lcrisks Excel tool
#' @param ... Ignored.
#' @export
get_default_input <- function(...) {
  list(age = 66, sex = "male", race = "white", years_smoked = 43, cigarettes_per_day = 36,
       years_quit = 0, bmi = 23, education = 3, emphysema = "no", family_history = 0)
}
