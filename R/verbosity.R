set_verbosity <- function(verbosity) {
  options(dyncli_verbosity = verbosity)
}

critical <- function(...) {
  if (!is.null(getOption("dyncli_verbosity")) && getOption("dyncli_verbosity") >= 1) {
    cat(..., sep = "")
  }
}

info <- function(...) {
  if (!is.null(getOption("dyncli_verbosity")) && getOption("dyncli_verbosity") >= 2) {
    cat(..., sep = "")
  }
}

debug <- function(...) {
  if (!is.null(getOption("dyncli_verbosity")) && getOption("dyncli_verbosity") >= 3) {
    cat(..., sep = "")
  }
}
