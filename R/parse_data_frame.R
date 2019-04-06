#' @importFrom fs is_file
#' @importFrom stringr str_replace_all str_split
#' @importFrom readr read_tsv
#' @importFrom jsonlite read_json
#' @importFrom yaml read_yaml
#' @importFrom hdf5r H5File
parse_data_frame <- function(x, name, types) {
  assert_that(
    is.character(x),
    length(x) == 1,
    is.character(name),
    !is.null(names(types)),
    types %all_in% c("integer", "numeric", "logical", "character")
  )

  if (!fs::is_file(x)) {
    x2 <- x %>% str_replace_all(";", "\n") %>% str_replace_all(",", "\t")
    header <- paste0(names(types), collapse = "\t")
    col <- c("integer" = "i", "numeric" = "d", "logical" = "l", "character" = "c")[types] %>%
      set_names(names(types))
    read_tsv(paste0(header, "\n", x2), col_types = col)
  } else if (grepl("\\.tsv$", x)) {
    col <- c("integer" = "i", "numeric" = "d", "logical" = "l", "character" = "c")[types] %>%
      set_names(names(types))
    read_tsv(x, col_types = col)
  } else if (grepl("\\.json$", x)) {
    read_json(x, simplifyVector = TRUE) %>% as_tibble()
  } else if (grepl("\\.ya?ml$", x)) {
    read_yaml(x) %>% as_tibble()
  } else if (grepl("\\.h5$", x)) {
    file_h5 <- H5File$new(x, mode = "r")
    out <- file_h5[[name]][]
    file_h5$close_all()
    out
  }
}

#' @importFrom readr write_tsv
#' @importFrom jsonlite write_json
#' @importFrom yaml write_yaml
#' @importFrom hdf5r H5File
write_data_frame <- function(x, file, name) {
  if (is.null(file) || is.na(file)) {
    x %>% pmap(paste, sep = ",") %>% paste(collapse = ";")
  } else if (grepl("\\.tsv$", file)) {
    write_tsv(x, file)
  } else if (grepl("\\.json$", file)) {
    write_json(as.list(x), file)
  } else if (grepl("\\.ya?ml$", file)) {
    write_yaml(as.list(x), file)
  } else if (grepl("\\.h5$", file)) {
    file_h5 <- hdf5r::H5File$new(file, mode = "w")
    file_h5[[name]] <- x
    file_h5$close_all()
  }
}
