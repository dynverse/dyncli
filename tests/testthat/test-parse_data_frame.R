context("Test parse_data_frame()")

data_tib <- tribble(
  ~name, ~types, ~data,
  "letters", c("letters" = "character"), tibble(letters),
  "lettersLETTERS", c("letters" = "character", "LETTERS" = "character"), tibble(letters, LETTERS),
  "numbers", c("name" = "character", "number" = "integer", "double" = "numeric", "boolean" = "logical"), tibble(name = c("one", "two", "three"), number = c(1L, 2L, 3L), double = c(1.1, 2.2, 3.3), boolean = c(TRUE, FALSE, TRUE))
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
  function(name, types, data, format) {
    test_that(paste0("write_data_frame works with name=", name, ", types=[", paste(names(types), " = ", types, sep = "", collapse = ", "), "], format=", format), {
      if (!is.na(format)) {
        file <- tempfile(fileext = format)

        write_data_frame(data, file = file, name = name)
      } else {
        file <- write_data_frame(data, file = NA, name = name)
      }
      # read_lines(file)
      # file_h5 <- H5File$new(file, mode = "r")
      # out <- file_h5[[name]][]

      v <- parse_data_frame(file, name = name, types = types)

      expect_equal(dimnames(v), dimnames(data))
      expect_true(all(map2_lgl(v, data, all.equal)))
    })
  }
)
