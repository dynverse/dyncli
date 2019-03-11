#' @importFrom fs is_file
#' @importFrom stringr str_split
#' @importFrom readr read_tsv
#' @importFrom jsonlite read_json
#' @importFrom yaml read_yaml
#' @importFrom hdf5r H5File
parse_vec <- function(x, name, type) {
  assert_that(
    is.character(x),
    length(x) == 1,
    is.character(name),
    length(name) == 1,
    type %all_in% c("integer", "numeric", "logical", "character")
  )
  assert_that(type %all_in% c("integer", "numeric", "logical", "character"))

  v <-
    if (!fs::is_file(x)) {
      x %>% str_split(",") %>% first()
    } else if (grepl("\\.tsv$", x)) {
      col <- set_names(readr_type_map[type], name)
      read_tsv(x, col_types = col)[[1]]
    } else if (grepl("\\.json$", x)) {
      read_json(x) %>% unlist()
    } else if (grepl("\\.ya?ml$", x)) {
      read_yaml(x) %>% unlist()
    } else if (grepl("\\.h5$", x)) {
      file_h5 <- H5File$new(x, mode = "r")
      out <- file_h5[[name]][]
      file_h5$close_all()
      out
    }

  assert_that(is.vector(v))

  if (type == "integer") {
    as.integer(v)
  } else if (type == "numeric") {
    as.numeric(v)
  } else if (type == "logical") {
    as.logical(v)
  } else if (type == "character") {
    v
  }
}

#' @importFrom readr write_tsv
#' @importFrom jsonlite write_json
#' @importFrom yaml write_yaml
#' @importFrom hdf5r H5File
write_vec <- function(x, file, name) {
  assert_that(length(name) == 1)

  if (is.null(file) || is.na(file)) {
    paste(x, collapse = ",")
  } else if (grepl("\\.tsv$", file)) {
    tibble(x) %>% set_colnames(name) %>% write_tsv(file)
  } else if (grepl("\\.json$", file)) {
    write_json(x, file)
  } else if (grepl("\\.ya?ml$", file)) {
    write_yaml(x, file)
  } else if (grepl("\\.h5$", file)) {
    file_h5 <- H5File$new(file, mode = "w")
    file_h5[[name]] <- x
    file_h5$close_all()
  }
}
