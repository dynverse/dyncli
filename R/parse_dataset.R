#' @importFrom fs is_file
#' @importFrom hdf5r H5File
#' @importFrom Matrix sparseMatrix Matrix t
parse_dataset <- function(x, loom_expression_layer = NULL) {
  assert_that(fs::is_file(x))

  extra_input <- NULL
  expression <- NULL

  ##########################
  ###        LOOM        ###
  ##########################
  if (grepl("\\.loom$", x)) {
    file_h5 <- H5File$new(x, mode = "r")

    assert_that(file_h5 %has_names% c("matrix", "row_attrs", "col_attrs"))

    counts <- file_h5[[path]][,] %>% Matrix(sparse = TRUE)
    feature_ids <- file_h5[["row_attrs/gene_names"]][]
    cell_ids <- file_h5[["col_attrs/cell_names"]][]

    if (!is.null(loom_expression_layer)) {
      expression <- file_h5[[paste0("layers/", loom_expression_layer)]][,] %>% Matrix(sparse = TRUE)
    }

    file_h5$close_all()

    dimnames(counts) <- list(cell_ids, feature_ids)
    dimnames(expression) <- list(cell_ids, feature_ids)
  } else if (grepl("\\.h5$", x)) {
    file_h5 <- H5File$new(x, mode = "r")

    if (file_h5 %has_names% c("expression", "counts", "cell_ids", "feature_ids")) {
      ##########################
      ###       OWN H5       ###
      ##########################
      cell_ids <- file_h5[["cell_ids"]]
      feature_ids <- file_h5[["feature_ids"]]

      dims <- c(length(cell_ids), length(feature_ids))
      dimnames <- list(cell_ids, feature_ids)

      counts_data <- file_h5[["counts"]]
      expression_data <- file_h5[["expression"]]

      counts <- with(counts_data, Matrix::sparseMatrix(
        i = i,
        j = j,
        x = x,
        dims = dims,
        dimnames = dimnames
      ))

      expression <- with(expression_data, Matrix::sparseMatrix(
        i = i,
        j = j,
        x = x,
        dims = dims,
        dimnames = dimnames
      ))

      ## TODO: support other inputs with `extra_input` as well!

    } else if (file_h5 %has_names% "matrix" && file[["matrix"]] %has_names% c("barcodes", "data", "features", "indices", "indptr", "shape")) {
      ##########################
      ###    CELLRANGER V3   ###
      ##########################
      subfile <- file_h5[["matrix"]]

      counts <-
        Matrix::sparseMatrix(
          i = subfile[["indices"]][],
          p = subfile[["indptr"]][],
          x = subfile[["data"]][],
          dims = subfile[["shape"]][],
          dimnames = list(
            subfile[["features/id"]][],
            subfile[["barcodes"]][]
          )
        ) %>%
        Matrix::t()
    } else if (length(names(file_h5)) == 1 && file_h5[[names(file_h5)]] %has_names% c("barcodes", "data", "genes", "indices", "indptr", "shape")) {
      ##########################
      ###    CELLRANGER V2   ###
      ##########################
      subfile <- file_h5[[names(file_h5)]]

      counts <-
        Matrix::sparseMatrix(
          i = subfile[["indices"]][],
          p = subfile[["indptr"]][],
          x = subfile[["data"]][],
          dims = subfile[["shape"]][],
          dimnames = list(
            subfile[["genes"]][],
            subfile[["barcodes"]][]
          )
        ) %>%
        Matrix::t()
    }
  }

  if (is.null(expression)) {
    expression <- normalise(counts)
  }

  out <- lst(counts, expression)

  c(out, extra_input)
}

nornmalise <- function(x) {
  # TODO: provide better normalisation :(
  log2(x + 1)
}

write_matrix <- function(x, file, format, name) {
}
