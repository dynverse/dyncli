#' @importFrom fs is_file
#' @importFrom stringr str_replace_all str_split
#' @importFrom readr read_tsv
#' @importFrom jsonlite read_json
#' @importFrom yaml read_yaml
#' @importFrom hdf5r H5File
parse_named_vec <- function(x, names, type) {
  assert_that(
    is.character(x),
    length(x) == 1,
    is.character(names),
    length(names) == 2,
    type %all_in% c("integer", "numeric", "logical", "character")
  )

  v <-
    if (!fs::is_file(x)) {
      vec <- x %>% str_split(",") %>% first()
      names <- vec %>% str_replace_all("=.*", "")
      values <- vec %>% str_replace_all(".*=", "")
      set_names(values, names)
    } else if (grepl("\\.tsv$", x)) {
      col <- set_names(c("c", readr_type_map[type]), names)
      read_tsv(x, col_types = col) %>% deframe()
    } else if (grepl("\\.json$", x)) {
      read_json(x) %>% unlist()
    } else if (grepl("\\.ya?ml$", x)) {
      read_yaml(x) %>% unlist()
    } else if (grepl("\\.h5$", x)) {
      file_h5 <- H5File$new(x, mode = "r")
      out <- file_h5[[names[[2]]]][] %>% deframe()
      file_h5$close_all()
      out
    }

  assert_that(is.vector(v))

  if (type == "integer") {
    set_names(as.integer(v), names(v))
  } else if (type == "numeric") {
    set_names(as.numeric(v), names(v))
  } else if (type == "logical") {
    set_names(as.logical(v), names(v))
  } else if (type == "character") {
    v
  }
}

#' @importFrom readr write_tsv
#' @importFrom jsonlite write_json
#' @importFrom yaml write_yaml
#' @importFrom hdf5r H5File
write_named_vec <- function(x, file, names) {
  assert_that(length(names) == 2)

  if (is.null(file) || is.na(file)) {
    paste(names(x), "=", x, sep = "", collapse = ",")
  } else if (grepl("\\.tsv$", file)) {
    x %>%
      enframe(name = names[[1]], value = names[[2]]) %>%
      write_tsv(file)
  } else if (grepl("\\.json$", file)) {
    write_json(as.list(x), file)
  } else if (grepl("\\.ya?ml$", file)) {
    write_yaml(as.list(x), file)
  } else if (grepl("\\.h5$", file)) {
    file_h5 <- hdf5r::H5File$new(file, mode = "w")
    file_h5[[names[[2]]]] <- x %>% enframe(name = names[[1]], value = names[[2]])
    file_h5$close_all()
  }
}
