#' @importFrom stringr str_replace_all
parse_prior <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }
  if (name %in% c("start_n", "end_n", "groups_n")) {
    parse_vec(value, name, type = "integer")
  } else if (name %in% c("start_id", "end_id", "features_id")) {
    if (fs::is_file(value)) {
      read_tsv(value)[[1]]
    } else {
      value %>% strsplit(",") %>% first()
    }
  } else if (name %in% c("groups_id", "timecourse_discrete", "timecourse_continuous")) {
    if (fs::is_file(value)) {
      x <- read_tsv(value) %>% deframe()
    } else {
      x <- value %>% str_replace_all(",", "\n") %>% str_replace_all("=", "\t") %>% read_tsv(col_names = c("a", "b")) %>% deframe()
    }
    if (name %in% c("timecourse_discrete", "timecourse_continuous")) {
      x <- as.numeric(x)
    }
    x
  } else if (name %in% c("groups_network")) {
    if (fs::is_file(value)) {
      read_tsv(value)
    } else {
      value %>% str_replace_all(";", "\n") %>% str_replace_all(",", "\t") %>% read_tsv(col_names = c("from", "to"))
    }
  }
}
