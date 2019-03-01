context("Test parse_vec()")

data_tib <- tribble(
  ~name, ~type, ~data,
  "names", "character", c("A", "B", "C"),
  "id", "character", "X",
  "start_n", "integer", 5L,
  "end_n", "integer", c(4L, 8L, 12L),
  "alpha", "numeric", 0.5,
  "betas", "numeric", c(0.1, 0.5, 0.8, 1.2),
  "verbose", "logical", FALSE,
  "switches", "logical", c(FALSE, TRUE, TRUE, TRUE)
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
  function(name, type, data, format) {
    test_that(paste0("parse_vec works with name=", name, ", type=", type, ", format=", format), {
      if (!is.na(format)) {
        file <- tempfile(fileext = format)

        write_vec(data, file = file, name = name)
      } else {
        file <- write_vec(data, file = NA, name = name)
      }

      v <- parse_vec(file, name = name, type = type)

      expect_equal(v, data)
    })
  }
)
