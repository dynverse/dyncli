#' @export
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

