add_prior_options <- function(parser, inputs) {
  debug("Building method prior parsers\n")

  poss_priors <-
    tribble(
      ~input_id, ~description, ~format, ~example,
      "start_n", "The number of start cells", "integer", "1",
      "end_n", "The number of end cells", "integer", "4",
      "groups_n", "The number of states, including the start, end and intermediary states", "integer", "5",

      "start_id", "Start cell(s); one or more start cell identifiers", "character vector", "C1,C2,C3",
      "end_id", "End cell(s); one or more end cell identifiers^", "character vector", "C1,C2,C3",
      "features_id", "A set of features known to be important in the dynamic process", "character vector", "G1,G2,G3",

      "groups_id", "Cell clustering linking cell identifiers to different states", "named character vector", "C1=A,C2=B,C3=B",
      "timecourse_discrete", "Time course (discrete), possibly from a time course experiment", "named integer vector", "C1=1,C2=4,C3=7",
      "timecourse_continuous", "Time course (continuous), possibly from a time course experiment", "named double vector", "C1=0.1,C2=0.4,C3=0.8",

      "groups_network", "State network, the known differentiation network between states", "dataframe(from: character, to: character)", "A,B;B,C;B,D"
    )

  prior_info <-
    inner_join(
      inputs,
      poss_priors,
      by = "input_id"
    )

  if (nrow(prior_info) > 0) {
    parser <- parser %>%
      add_option("--priors", type = "character", help = "A file containing prior information.\n\t\tFormat: See <....website....>.\n\t\tExample: $MOUNT/prior.yml.", default = NULL) %>%
      add_option("--use_priors", type = "character", help = "Which optional priors to use. Possible values are 'all', 'none' (default), or a comma separated vector or which priors to use.", default = "none")
  }

  for (i in seq_len(nrow(prior_info))) {
    input_id <- prior_info$input_id[[i]]
    # debug("Building parser for prior ", input_id, "\n")
    parser <- parser %>% add_option(
      opt_str = paste0("--", input_id),
      type = "character",
      help = paste0(
        "Prior", ifelse(prior_info$required[[i]], " (Required)", ""), ": ", prior_info$description[[i]], ".\n\t\t",
        "Format: ", prior_info$format[[i]], "\n\t\t",
        "Example: ", prior_info$example[[i]]
      )
    )
  }

  parser
}
