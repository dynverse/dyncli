#' @importFrom fs is_file
#' @importFrom stringr str_split
#' @importFrom readr read_tsv read_rds
#' @importFrom hdf5r H5File
#' @importFrom Matrix sparseMatrix Matrix t
parse_matrix <- function(x, name, type) {
  assert_that(fs::is_file(x))

  if (grepl("\\.loom$", x)) {
    file_h5 <- H5File$new(x, mode = "r")

    assert_that(file_h5 %has_names% c("matrix", "row_attrs", "col_attrs"))
    matrix <- file_h5[["matrix"]][,] %>% Matrix(sparse = TRUE)
    feature_ids <- file_h5[["row_attrs/gene_names"]][]
    cell_ids <- file_h5[["col_attrs/cell_names"]][]
    file_h5$close_all()

    dimnames(matrix) <- list(cell_ids, feature_ids)
    matrix
  } else if (grepl("\\.h5$", x)) {
    file_h5 <- H5File$new(x, mode = "r")

    if (file_h5 %has_names% name && file_h5[[name]] %has_names% c("rownames", "colnames", "data")) {
      # own H5 format detected
      subfile <- file_h5[[name]]

      cell_ids <- subfile[["rownames"]][]
      feature_ids <- subfile[["colnames"]][]
      data <- subfile[["data"]][]
      matrix <- Matrix::sparseMatrix(
        i = data$cell_ix,
        j = data$feature_ix,
        x = data$value,
        dims = c(length(cell_ids), length(feature_ids)),
        dimnames = list(cell_ids, feature_ids)
      )
    } else if (file_h5 %has_names% "matrix" && file[["matrix"]] %has_names% c("barcodes", "data", "features", "indices", "indptr", "shape")) {
      # cellranger v3 detected
      subfile <- file_h5[["matrix"]]

      matrix <- Matrix::sparseMatrix(
        i = subfile[["indices"]][],
        p = subfile[["indptr"]][],
        x = subfile[["data"]][],
        dims = subfile[["shape"]][],
        dimnames = list(
          subfile[["features/id"]][],
          subfile[["barcodes"]][]
        )
      ) %>% Matrix::t
    } else if (length(names(file_h5)) == 1 && file_h5[[names(file_h5)]] %has_names% c("barcodes", "data", "genes", "indices", "indptr", "shape")) {
      # cellranger v2 detected
      subfile <- file_h5[[names(file_h5)]]

      matrix <- Matrix::sparseMatrix(
        i = subfile[["indices"]][],
        p = subfile[["indptr"]][],
        x = subfile[["data"]][],
        dims = subfile[["shape"]][],
        dimnames = list(
          subfile[["genes"]][],
          subfile[["barcodes"]][]
        )
      ) %>% Matrix::t
    }
  } else if (grepl("\\.rds$", x)) {
    read_rds(x)
  } else if (grepl("\\.tsv$", x)) {
    header <- read_lines(x, n_max = 1) %>% str_split("\t") %>% first()

    if (identical(header, c("cell_id", "feature_id", "value"))) {
      # long format detected
      long <- read_tsv(x, col_types = cols("cell_id" = "c", "feature_id" = "c", "value" = readr_type_map[type]))

      cell_ids <- unique(long$cell_id)
      feature_ids <- unique(long$feature_id)

      i <- match(long$cell_id, cell_ids)
      j <- match(long$cell_id, feature_ids)

      Matrix::sparseMatrix(
        i = i,
        j = j,
        x = long$value,
        dimnames = list(cell_ids, feature_ids)
      )
    } else {
      # wide format detected
      read_tsv(x, col_types = cols("cell_id" = "c", .default = readr_type_map[type])) %>%
        as.data.frame() %>%
        column_to_rownames("cell_id") %>%
        as.matrix() %>%
        Matrix::Matrix(sparse = TRUE)
    }
  }
}

#' @importFrom readr write_tsv write_rds
#' @importFrom jsonlite write_json
#' @importFrom yaml write_yaml
#' @importFrom hdf5r H5File
write_matrix <- function(x, file, format, name) {




  # assert_that(length(name) == 1)
  #
  # if (is.null(file) || is.na(file)) {
  #   paste(x, collapse = ",")
  # } else if (grepl("\\.tsv$", file)) {
  #   tibble(x) %>% set_colnames(name) %>% write_tsv(file)
  # } else if (grepl("\\.json$", file)) {
  #   write_json(x, file)
  # } else if (grepl("\\.ya?ml$", file)) {
  #   write_yaml(x, file)
  # } else if (grepl("\\.rds$", file)) {
  #   write_rds(x, file)
  # } else if (grepl("\\.h5$", file)) {
  #   file_h5 <- H5File$new(file, mode = "w")
  #   file_h5[[name]] <- x
  #   file_h5$close_all()
  # }
}
