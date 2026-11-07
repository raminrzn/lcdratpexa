# Accept wrapped or unwrapped input, map aliases, drop JSON nulls and empty
# strings (the platform's spelling of "not supplied"), keep known fields only.
.ld_normalize <- function(model_input, dots, vars, alias) {
  if (is.null(model_input)) {
    if (length(dots) == 0) return(NULL)
    model_input <- dots
  }
  if (is.list(model_input) && !is.data.frame(model_input)) {
    model_input <- model_input[!vapply(model_input, is.null, logical(1))]
  }
  out <- as.list(model_input)
  if (length(out) == 0) return(NULL)
  names(out) <- tolower(names(out))
  for (a in intersect(names(out), names(alias))) {
    canon <- alias[[a]]
    if (!canon %in% names(out)) names(out)[match(a, names(out))] <- canon
  }
  out <- out[intersect(names(out), vars)]
  out[!vapply(out, function(v) is.character(v) && length(v) == 1L &&
                !nzchar(trimws(v)), logical(1))]
}

.ld_yesno <- function(x, field) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(as.integer(x))
  if (is.numeric(x) && length(x) == 1L && x %in% c(0, 1)) return(as.integer(x))
  key <- tolower(trimws(as.character(x)))
  if (key %in% c("yes", "y", "true", "1")) return(1L)
  if (key %in% c("no", "n", "false", "0")) return(0L)
  stop('Unrecognised `', field, '` value: "', x, '". Use yes/no or 1/0.',
       call. = FALSE)
}

