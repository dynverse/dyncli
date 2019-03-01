context("Test parse_named_vec()")

data_tib <- tribble(
  ~names, ~type, ~data,
  c("cell_id", "cluster"), "character", c("C1" = "A", "C2" = "B", "C3" = "B"),
  c("left", "right"), "character", c("X" = "Y"),
  c("name", "number"), "integer", c("five" = 5L),
  c("names", "numbers"), "integer", c("four" = 4L, "eight" = 8L, "twelve" = 12L),
  c("param", "value"), "numeric", c("zeropointfive" = 0.5),
  c("cell_id", "pseudotime"), "numeric", c("pointone" = 0.1, "pointfive" = 0.5, "pointeight" = 0.8, "onepointtwo" = 1.2),
  c("param", "value"), "logical", c("verbose" = FALSE),
  c("param", "bool"), "logical", c("a" = FALSE, "b" = TRUE, "c" = TRUE, "d" = TRUE)
)

formats <- tribble(
  ~format,
  NA,
  ".tsv",
  ".json",
  ".yaml",
  ".yml",
  ".h5"
)

pwalk(
  crossing(data_tib, formats),
  function(names, type, data, format) {
    test_that(paste0("parse_named_vec works with names=(", paste(names, collapse = ", "), "), type=", type, ", format=", format), {
      if (!is.na(format)) {
        file <- tempfile(fileext = format)

        write_named_vec(data, file = file, names = names)
      } else {
        file <- write_named_vec(data, file = NA, names = names)
      }
      # read_lines(file)

      v <- parse_named_vec(file, names = names, type = type)

      expect_equal(v, data)
    })
  }
)
