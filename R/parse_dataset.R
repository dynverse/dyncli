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

    file_h5$close_all()
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
        p = p,
        x = x,
        dims = dims,
        dimnames = dimnames
      ))

      expression <- with(expression_data, Matrix::sparseMatrix(
        i = i,
        p = p,
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

normalise <- function(counts) {
  # TODO: provide better normalisation :(
  expr <- counts
  expr@x <- log2(expr@x + 1)
  expr
}

write_h5 <- function(x, file) {
  file_h5 <- H5File$new(file, "w")

  .write_h5(x, file_h5, "")

  file_h5$close_all()
}


.write_h5 <- function(x, file_h5, name) {
  cat("Processing ", name, "\n", sep = "")
  if (any(grepl("^[dlniz]..Matrix$", class(x)))) {
    ipx <- as(x, "dgCMatrix")
    file_h5$create_group(name)
    subfile <- file_h5[[name]]
    h5attr(subfile, "object_class") <- "sparse_matrix"
    subfile[["i"]] <- ipx@i
    subfile[["p"]] <- ipx@p
    subfile[["x"]] <- ipx@x
    subfile[["dims"]] <- dim(ipx)
    if (!is.null(rownames(ipx))) {
      subfile[["rownames"]] <- rownames(ipx)
    }
    if (!is.null(colnames(ipx))) {
      subfile[["colnames"]] <- colnames(ipx)
    }
  } else if (is.matrix(x)) {
    file_h5$create_group(name)
    subfile <- file_h5[[name]]
    h5attr(subfile, "object_class") <- "dense_matrix"
    subfile[["rownames"]] <- rownames(x)
    subfile[["colnames"]] <- colnames(x)
    subfile[["matrix"]] <- x
  } else if (is.data.frame(x)) {
    file_h5$create_group(name)
    subfile <- file_h5[[name]]
    h5attr(subfile, "object_class") <- "data_frame"
    walk2(
      x,
      names(x),
      function(xx, xn) {
        .write_h5(xx, subfile, xn)
      }
    )
  } else if (is.atomic(x)) {
    if (is.null(names(x))) {
      file_h5[[name]] <- x
      h5attr(file_h5[[name]], "object_class") <- "vector"
    } else {
      file_h5$create_group(name)
      subfile <- file_h5[[name]]
      h5attr(subfile, "object_class") <- "named_vector"
      subfile[["names"]] <- names(x)
      subfile[["values"]] <- x
    }
  } else if (is.list(x)) {
    if (name == "") {
      subfile <- file_h5
    } else {
      file_h5$create_group(name)
      subfile <- file_h5[[name]]
    }
    h5attr(file_h5, "object_class") <- "list"
    walk2(
      x,
      names(x),
      function(xx, xn) {
        .write_h5(xx, subfile, xn)
      }
    )
  }
}

read_h5 <- function(file) {
  file_h5 <- H5File$new(file, "r")

  lis <- .read_h5(file_h5)

  file_h5$close_all()

  lis
}

#' @importFrom hdf5r h5attr_names
.read_h5 <- function(file_h5) {
  if ("object_class" %in% h5attr_names(file_h5)) {
    object_class <- h5attr(file_h5, "object_class")

    if (object_class == "list") {
      nms <- names(file_h5)
      out <- map(nms, function(nm) {
        cat("Recursing ", nm, "\n", sep = "")
        .read_h5(file_h5[[nm]])
      })
      names(out) <- nms
      out
    } else if (object_class == "named_vector") {
      nms <- file_h5[["names"]][]
      vls <- file_h5[["values"]][]
      set_names(vls, nms)
    } else if (object_class == "data_frame") {
      nms <- names(file_h5)
      out <- map(nms, ~ .read_h5(file_h5[[.]]))
      names(out) <- nms
      data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
    } else if (object_class == "dense_matrix") {
      mat <- file_h5[["matrix"]][,]
      rownames(mat) <- file_h5[["rownames"]][]
      colnames(mat) <- file_h5[["colnames"]][]
    } else if (object_class == "sparse_matrix") {
      i <- file_h5[["i"]][]
      p <- file_h5[["p"]][]
      x <- file_h5[["x"]][]
      dims <- file_h5[["dims"]][]

      rn <-
        if ("rownames" %in% names(file_h5)) {
          file_h5[["rownames"]][]
        } else {
          NULL
        }

      cn <-
        if ("colnames" %in% names(file_h5)) {
          file_h5[["colnames"]][]
        } else {
          NULL
        }

      Matrix::sparseMatrix(
        i = i,
        p = p,
        x = x,
        dims = dims,
        dimnames = list(rn, cn)
      )
    }
  } else {
    file_h5[]
  }
}
