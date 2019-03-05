#' @export
read_h5 <- function(file) {
  info("Reading object from ", file, "\n")
  file_h5 <- H5File$new(file, "r")

  lis <- .read_h5(file_h5)

  file_h5$close_all()

  lis
}

#' @importFrom hdf5r h5attr_names h5attr
.read_h5 <- function(file_h5) {
  debug("Reading ", file_h5$get_obj_name(), "\n")
  if (!"object_class" %in% h5attr_names(file_h5)) {
    stop("Object class not found (path=", file_h5$get_filename(), ", obj_name=", file_h5$get_obj_name())
  }
  object_class <- h5attr(file_h5, "object_class")

  ## VECTOR ##
  if (object_class == "null") {
    NULL
  } else if (object_class == "vector") {
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
