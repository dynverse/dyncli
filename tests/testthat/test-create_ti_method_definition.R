context("testing create_ti_method_definition")

test_that("testing create_ti_method_definition", {
  definition_string <- "method:
  id: comp_1

parameters:
  - id: component
    default: 1
    type: integer
    distribution:
      type: uniform
      lower: 1
      upper: 10
    description: The nth component to use

wrapper:
  input_required: expression
  input_optional: start_id
"

  readr::write_file(definition_string, "definition.yml")

  # can't use /usr/bin/env Rscript in R CMD check,
  # even though it is recommended by the R manual
  shebang <-
    if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "") {
      "#!/usr/bin/env Rscript"
    } else {
      paste0("#!", Sys.getenv("R_HOME"), "/bin/Rscript")
    }
  run_r_string <- paste0(shebang, "

dataset <- dyncli::main()

library(dynwrap)
library(dplyr)

# infer trajectory
pca <- prcomp(dataset$expression)

pseudotime <- pca$x[, dataset$parameters$component]

# flip pseudotimes using start_id
if (!is.null(dataset$priors$start_id)) {
  if(mean(pseudotime[start_id]) > 0.5) {
    pseudotime <- 1-pseudotime
  }
}

# build trajectory
trajectory <- wrap_data(cell_ids = rownames(dataset$expression)) %>%
  add_linear_trajectory(pseudotime = pseudotime)

# save output
dyncli::write_output(trajectory, dataset$output)
")

  readr::write_file(run_r_string, "run.R")
  fs::file_chmod("run.R", "+x")

  method <- create_ti_method_definition("definition.yml", "run.R")

  dataset <- dynwrap::example_dataset

  trajectory <- dynwrap::infer_trajectory(dataset, method())

  expect_true(dynwrap::is_wrapper_with_trajectory(trajectory))

  file.remove("definition.yml")
  file.remove("run.R")
})
