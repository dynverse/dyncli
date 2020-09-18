add_prior_options <- function(parser, inputs) {
  debug("Building method prior parsers\n")

  prior_info <-
    inner_join(
      inputs,
      dynwrap::priors,
      by = c("input_id" = "prior_id")
    )

  if (nrow(prior_info) > 0) {
    parser <- parser %>%
      add_option("--priors", type = "character", help = "A file containing prior information.\n\t\tExample: $MOUNT/prior.yml.", default = NULL) %>%
      add_option("--use_priors", type = "character", help = "Which optional priors to use. Possible values are 'all', 'none' (default), or a comma separated vector or which priors to use.", default = "none")
  }

  for (i in seq_len(nrow(prior_info))) {
    input_id <- prior_info$input_id[[i]]
    # debug("Building parser for prior ", input_id, "\n")
    parser <- parser %>% add_option(
      opt_str = paste0("--", input_id),
      type = "character",
      help = paste0(
        "Prior ", prior_info$name[[i]], ifelse(prior_info$required[[i]], " (Required)", ""), "; ", prior_info$description[[i]], ".\n\t\t",
        "Format: ", prior_info$format[[i]], "\n\t\t",
        "Example: ", prior_info$example[[i]]
      )
    )
  }

  parser
}
