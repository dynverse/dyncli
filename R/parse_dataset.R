#' @importFrom fs is_file
#' @importFrom hdf5r H5File h5attr_names
#' @importFrom Matrix sparseMatrix Matrix t
parse_dataset <- function(x, loom_expression_layer = NULL) {
  assert_that(
    is.character(x),
    length(x) == 1,
    fs::is_file(x) || fs::is_link(x),
    msg = "--dataset should contain a pathname of a .loom or .h5 file. Add a '-h' flag for help."
  )

  extra_input <- NULL
  expression <- NULL

  ##########################
  ###        LOOM        ###
  ##########################
  if (grepl("\\.loom$", x)) {
    file_h5 <- H5File$new(x, mode = "r")
    on.exit(file_h5$close_all())

    assert_that(file_h5 %has_names% c("matrix", "row_attrs", "col_attrs", "layers"))

    counts <- file_h5[["matrix"]][,] %>% Matrix(sparse = TRUE)

    feature_paths <- paste0("row_attrs/", c("gene_names", "Gene"))
    cell_paths <- paste0("col_attrs/", c("cell_names", "CellID"))

    feature_exists <- map_lgl(feature_paths, file_h5$exists) %>% which()
    cell_exists <- map_lgl(cell_paths, file_h5$exists) %>% which()

    feature_ids <-
      if (length(feature_exists) == 1) {
        file_h5[[feature_paths[[feature_exists]]]][]
      } else {
        warning("feature IDs could not be found in the loom format!")
        paste("Feature", seq_len(ncol(counts)))
      }

    if (any(duplicated(feature_ids))) {
      stop("duplicated feature IDs found!")
    }

    cell_ids <-
      if (length(cell_exists) == 1) {
        file_h5[[cell_paths[[cell_exists]]]][]
      } else {
        warning("cell IDs could not be found in the loom format!")
        paste("Cell", seq_len(nrow(counts)))
      }

    if (any(duplicated(cell_ids))) {
      stop("duplicated cell IDs found!")
    }

    dimnames(counts) <- list(cell_ids, feature_ids)

    if (!is.null(loom_expression_layer)) {
      expression <- file_h5[[paste0("layers/", loom_expression_layer)]][,] %>% Matrix(sparse = TRUE)
      dimnames(expression) <- list(cell_ids, feature_ids)
    }
  } else if (grepl("\\.h5$", x)) {
    file_h5 <- H5File$new(x, mode = "r")
    on.exit(file_h5$close_all())

    if (file_h5 %has_names% c("data", "names") && "object_class" %in% h5attr_names(file_h5)) {
      ##########################
      ###       OWN H5       ###
      ##########################
      tmp <- dynutils::read_h5_(file_h5)
      counts <- tmp$counts
      expression <- tmp$expression
      extra_input <- list()

      if ("parameters" %in% names(tmp)) {
        extra_input$parameters <- tmp$parameters
      }
      if ("priors" %in% names(tmp)) {
        extra_input$priors <- tmp$priors
      }
      if ("prior_information" %in% names(tmp)) {
        extra_input$priors <- tmp$prior_information
      }
      if ("seed" %in% names(tmp)) {
        extra_input$seed <- tmp$seed
      }

      # add dataset prior if given
      if (any(c("milestone_percentages", "divergence_regions", "milestone_network", "progressions") %in% names(tmp))) {
        extra_input$priors$dataset <- tmp[c("milestone_network", "progressions", "milestone_percentages", "divergence_regions")]
      }

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
          ),
          index1 = FALSE
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
          ),
          index1 = FALSE
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

normalise <- function(counts) {
  # TODO: provide better normalisation :(
  # TODO: Also print out warning that better normalisation should be added

  expr <- counts
  expr@x <- log2(expr@x + 1)
  expr
}

