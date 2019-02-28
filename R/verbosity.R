


critical <- function(...) {
  if (getOption("dyncli_verbosity") >= 1) {
    cat(..., sep = "")
  }
}

info <- function(...) {
  if (getOption("dyncli_verbosity") >= 2) {
    cat(..., sep = "")
  }
}

debug <- function(...) {
  if (getOption("dyncli_verbosity") >= 3) {
    cat(..., sep = "")
  }
}
