
#' @import optparse
#' @export
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  args <- c("ti", "--seed", "4", "--output", "/ti/output/")
  args <- c("api")

  # definition <- dynmethods::ti_scorpius()
  definition <- dynwrap::create_ti_method_container("dynverse/ti_gng")()

  parser <-
    OptionParser(usage = paste0("docker run ~/SOMEDIR:/ti ", definition$docker_repository)) %>%
    add_option(c("-v", "--verbose"), action = "store_true", default = FALSE, help = "Print extra output") %>%
    add_option(c("--seed"), type = "integer", default = NA, help = "A seed to be set") %>%
    add_option(c("--output"), type = "character", help = "Filename of the output data")

  # forbidden are forbidden combinations of parameters; e.g. xmin > xmax.
  parameter_ids <-
    names(definition$parameters) %>%
    keep(~. != "forbidden")

  # generate documentation per parameter separately
  for (parameter_id in parameter_ids) {
    cat("Processing ", parameter_id, "\n", sep = "")
    parameter <- definition$parameters[[parameter_id]]

    description <-
      parameter$description %>%
      ifelse(!is.null(.), ., "") %>%     # use "" if no description is provided
      str_replace_all("\n", "") %>%      # remove newlines
      Hmisc::capitalize()                # capitalise sentences

    range_text <-
      case_when(
        parameter$type == "discrete" ~ paste0("; values: {", paste0(sapply(parameter$values, deparse), collapse = ", "), "}"),
        parameter$type %in% c("integer", "numeric") ~ paste0("; range: from ", deparse(parameter$lower), " to ", deparse(parameter$upper)),
        TRUE ~ ""
      )

    default_str <-
      case_when(
        parameter$type %in% c("integer", "numeric", "discrete", "logical") ~ paste0(" (default: ", deparse(parameter$default), range_text, ")"),
        TRUE ~ ""
      )

    default_value <-
      if (parameter$type %in% c("discrete", "integer", "logical", "numeric")) {
        parameter$default
      } else {
        deparse(parameter$default)
      }

    parser <- parser %>% add_option(
      opt_str = paste0("--", parameter_id),
      type = case_when(
        parameter$type == "discrete" ~ "character",
        parameter$type %in% c("integer", "logical", "numeric") ~ parameter$type,
        TRUE ~ "character"
      ),
      default = ,
      help = paste0(parameter$type, "; ", description, default_str) %>%
        gsub("\\\\link\\[[a-zA-Z0-9_:]*\\]\\{([^\\}]*)\\}", "\\1", .) # substitute \link[X](Y) with just Y
    )
  }

  # generate documentation per input separately
  definition$input
  dynwrap::allowed_inputs


  args <- parse_args(parser, "-h")

  args <- parse_args(parser)

  #   add_option(c("--expression"), type = "character", help = "Filename of the expression data") %>%

  print(args)

  if (!is.null(args$seed) && !is.na(args$seed)) set.seed(args$seed)

  params <- list(dimred = args$dimred)
  data <- list()

  if ("expression" %in% names(args)) data$expression <- readr::read_rds(args$expression)
  if ("counts" %in% names(args)) data$counts <- readr::read_rds(args$counts)

  source("/code/run.R")

  readr::write_rds(output, args$output)
}

main_ti <- function(args) {

}
