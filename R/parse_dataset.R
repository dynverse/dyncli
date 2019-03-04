#' @importFrom fs is_file
#' @importFrom hdf5r H5File h5attr_names
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

    if (file_h5 %has_names% c("data", "names") && "object_class" %in% h5attr_names(file_h5)) {
      ##########################
      ###       OWN H5       ###
      ##########################
      .read_h5(file_h5)

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

write_h5 <- function(x, file) {
  file_h5 <- H5File$new(file, "w")

  .write_h5(x, file_h5, "")

  file_h5$close_all()
}

#' @importFrom hdf5r h5attr<-
.write_h5 <- function(x, file_h5, name) {
  cat("Processing ", file_h5$get_obj_name(), "/", name, "\n", sep = "")
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
    if (!is.null(rownames(x))) subfile[["rownames"]] <- rownames(x)
    if (!is.null(colnames(x))) subfile[["colnames"]] <- colnames(x)
    subfile[["data"]] <- x
  } else if (is.data.frame(x)) {
    file_h5$create_group(name)
    subfile <- file_h5[[name]]
    h5attr(subfile, "object_class") <- "data_frame"
    subfile[["rownames"]] <- rownames(x)
    subfile[["colnames"]] <- colnames(x)
    subfile$create_group("data")
    subsubfile <- subfile[["data"]]
    for (xn in names(x)) {
      subsubfile[[xn]] <- x[[xn]]
    }
  } else if (is.atomic(x)) {
    file_h5$create_group(name)
    subfile <- file_h5[[name]]
    h5attr(subfile, "object_class") <- "vector"
    if (!is.null(names(x))) subfile[["names"]] <- names(x)
    subfile[["data"]] <- x
  } else if (is.list(x)) {
    if (name == "") {
      subfile <- file_h5
    } else {
      file_h5$create_group(name)
      subfile <- file_h5[[name]]
    }
    h5attr(subfile, "object_class") <- "list"
    subfile[["class"]] <- class(x)
    if (!is.null(names(x))) subfile[["names"]] <- names(x)

    subfile$create_group("data")
    subsubfile <- subfile[["data"]]

    if (is.null(names(x))) names(x) <- paste0("elem", seq_along(x))

    for (xn in names(x)) {
      .write_h5(x[[xn]], subsubfile, xn)
    }
  }
}

read_h5 <- function(file) {
  file_h5 <- H5File$new(file, "r")

  lis <- .read_h5(file_h5)

  file_h5$close_all()

  lis
}

#' @importFrom hdf5r h5attr_names h5attr
.read_h5 <- function(file_h5) {
  cat("Reading ", file_h5$get_obj_name(), "\n", sep = "")
  if (!"object_class" %in% h5attr_names(file_h5)) {
    stop("Object class not found (path=", file_h5$get_filename(), ", obj_name=", file_h5$get_obj_name())
  }
  object_class <- h5attr(file_h5, "object_class")

  ## VECTOR ##
  if (object_class == "vector") {
    data_file <- file_h5[["data"]]

    # out <- data_file[]
    # if ("names" %in% names(file_h5)) names(out) <- file_h5[["names"]][]

    # workaround
    out <- .read_h5_vec(data_file)
    if ("names" %in% names(file_h5)) names(out) <- .read_h5_vec(file_h5[["names"]])

    out

  ## DENSE MATRIX ##
  } else if (object_class == "dense_matrix") {
    out <- file_h5[["data"]][,]
    # if ("rownames" %in% names(file_h5)) rownames(out) <- file_h5[["rownames"]][]
    # if ("colnames" %in% names(file_h5)) colnames(out) <- file_h5[["colnames"]][]

    # workaround
    if ("rownames" %in% names(file_h5)) rownames(out) <- .read_h5_vec(file_h5[["rownames"]])
    if ("colnames" %in% names(file_h5)) colnames(out) <- .read_h5_vec(file_h5[["colnames"]])
    out

  ## LIST ##
  } else if (object_class == "list") {
    subfile <- file_h5[["data"]]

    has_names <- "names" %in% names(file_h5)
    # nms <- if (has_names) file_h5[["names"]][] else names(subfile)
    # workaround
    nms <- if (has_names) .read_h5_vec(file_h5[["names"]]) else names(subfile)

    out <- map(nms, ~.read_h5(subfile[[.]]))
    if (has_names) names(out) <- nms
    if ("class" %in% names(file_h5)) class(out) <- file_h5[["class"]][]

    out

  ## DATA FRAME ##
  } else if (object_class == "data_frame") {
    # rownames <- file_h5[["rownames"]][]
    # colnames <- file_h5[["colnames"]][]

    # workaround
    rownames <- .read_h5_vec(file_h5[["rownames"]])
    colnames <- .read_h5_vec(file_h5[["colnames"]])

    data <- file_h5[["data"]]
    out <- map(colnames, ~ .read_h5_vec(data[[.]])) %>% data.frame(check.names = FALSE, stringsAsFactors = FALSE)
    rownames(out) <- rownames
    colnames(out) <- colnames
    out

  ## SPARSE MATRIX ##
  } else if (object_class == "sparse_matrix") {
    i <- file_h5[["i"]][]
    p <- file_h5[["p"]][]
    x <- file_h5[["x"]][]
    dims <- file_h5[["dims"]][]

    # rn <- if ("rownames" %in% names(file_h5)) file_h5[["rownames"]][] else NULL
    # cn <- if ("colnames" %in% names(file_h5)) file_h5[["colnames"]][] else NULL

    rn <- if ("rownames" %in% names(file_h5)) .read_h5_vec(file_h5[["rownames"]]) else NULL
    cn <- if ("colnames" %in% names(file_h5)) .read_h5_vec(file_h5[["colnames"]]) else NULL

    Matrix::sparseMatrix(
      i = i,
      p = p,
      x = x,
      dims = dims,
      dimnames = list(rn, cn),
      index1 = FALSE
    )
  }
}

.read_h5_vec <- function(file_h5)
  # workaround for https://github.com/hhoeflin/hdf5r/issues/118
  if (file_h5$dims == 0 && "H5T_STRING" %in% class(file_h5$get_type())) {
    character(0)
  } else {
    file_h5[]
  }
