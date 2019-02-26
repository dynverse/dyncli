# definition_location <- "~/Workspace/dynverse/methods/ti_paga/definition.yml"
# args <- c("--seed", "4", "--expression", "/ti/input/expression.csv", "--counts", "/ti/input/counts.csv", "--output", "/ti/output/output.h5", "--n_comps", "50")

#' dyncli main function
#'
#' @param args The arguments to be read by dyncli
#' @param definition_location The location of the definition file of the method
#'
#' @import optparse
#' @importFrom dynwrap create_ti_method_definition
#'
#' @export
main <- function(
  args = commandArgs(trailingOnly = TRUE),
  definition_location = "/code/definition.yml"
) {

  definition <- dynwrap::create_ti_method_definition(filename = definition_location, return_function = FALSE)

  ##########################################
  ##        CREATE OPTPARSE PARSER        ##
  ##########################################
  parser <-
    OptionParser(usage = paste0("LOCAL=/path/to/folder; MOUNT=/ti; docker run -v $LOCAL:$MOUNT ", definition$container$docker)) %>%
    add_option("--expression", type = "character", help = "Filename of expression data, example: $MOUNT/expression.(tsv|h5|rds).") %>%
    add_option("--counts", type = "character", help = "Filename of raw counts data, example: $MOUNT/counts.(tsv|h5|rds).") %>%
    add_option("--output", type = "character", help = "Filename of the output trajectory data, example: $MOUNT/output.(h5|rds).") %>%

    # parameters
    add_option("--params", type = "character", help = "A file containing method-specific parameters, example: $MOUNT/params.(h5|yml).", default = NULL) %>%
    dynparam::add_optparse_options(definition$parameters) %>%

    # priors
    add_option("--priors", type = "character", help = "A file containing prior information.\n\t\tFormat: See <....website....>.\n\t\tExample: $MOUNT/prior.(h5|yml).", default = NULL) %>%

    add_option("--start_n", type = "character", help = "The number of start cells.\n\t\tFormat: integer.\n\t\tExample: 1.") %>%
    add_option("--end_n", type = "character", help = "The number of end cells.\n\t\tFormat: integer.\n\t\tExample: 4.") %>%
    add_option("--groups_n", type = "character", help = "The number of states, including the start, end and intermediary states.\n\t\tFormat: integer.\n\t\tExample: 5.") %>%

    add_option("--start_id", type = "character", help = "Start cell(s): one or more start cell identifiers.\n\t\tFormat: comma separated vector, or a path of a tsv text file (with column names).\n\t\tExample: C1,C2,C3.") %>%
    add_option("--end_id", type = "character", help = "End cell(s): one or more end cell identifiers.\n\t\tFormat: comma separated vector, or a path of a tsv text file (with column names).\n\t\tExample: C1,C2,C3.") %>%
    add_option("--features_id", type = "character", help = "A set of features known to be important in the dynamic process.\n\t\tFormat: comma separated vector, or a path of a tsv text file (with column names).\n\t\tExample: G1,G2,G3") %>%

    add_option("--groups_id", type = "character", help = "Cell clustering: a named vector linking cell identifiers to different states.\n\t\tFormat: named comma separated vector, or a path of a tsv text file (with column names).\n\t\tExample: C1=A,C2=A,C3=B,C4=C") %>%
    add_option("--timecourse_discrete", type = "character", help = "Time course (discrete), possibly from a time course experiment.\n\t\tFormat: named comma separated vector, or a path of a tsv text file (with column names).\n\t\tExample: C1=1,C2=3,C3=8,C4=10") %>%
    add_option("--timecourse_continuous", type = "character", help = "Time course (continuous), possibly from a time course experiment.\n\t\tFormat: named comma separated vector, or a path of a tsv text file (with column names).\n\t\tExample: C1=0.1,C2=0.4,C3=0.7,C4=0.95") %>%

    add_option("--groups_network", type = "character", help = "State network, the known differentiation network between states.\n\t\tFormat: A path of a tsv text file containing a 'from' and a 'to' column.") %>%

    # add final parameters
    add_option(c("-v", "--verbose"), action = "store_true", default = FALSE, help = "Print extra output.") %>%
    add_option(c("--seed"), type = "integer", default = NA, help = "A seed to be set to ensure reproducability.")

  ##########################################
  ##            PARSE ARGUMENTS           ##
  ##########################################

  parsed_args <- parse_args(parser, args = args)

  # create a data object
  task <-
    if (!is.null(parsed_args$dataset)) {
      # TODO: support hdf5
      readr::read_rds(parsed_args$dataset)
    } else {
      list()
    }

  # convert parameter values to correct type
  task$params <-
    if (!is.null(parsed_args$params)) {
      # TODO: support hdf5
      yaml::read_yaml(parsed_args$params)
    } else {
      list()
    }
  for (parameter in definition$parameters$parameters) {
    task$params[[parameter$id]] <- dynparam::argparse_trafo(parameter, parsed_args[[parameter$id]])
  }

  # convert prior values to correct type
  task$priors <-
    if (!is.null(parsed_args$priors)) {
      # TODO: fix groups_network
      # TODO: support hdf5
      yaml::read_yaml(parsed_args$priors)
    } else {
      list()
    }
  for (prior_id in dynwrap::priors$prior_id %>% setdiff("dataset")) {
    task$priors[[prior_id]] <- read_prior(prior_id, parsed_args[[prior_id]])
  }

  if (!is.null(parsed_args$verbose)) {
    task$verbose <- parsed_args$verbose
  }
  if (!is.null(parsed_args$seed)) {
    task$seed <- parsed_args$seed
  }

  # read expression / counts
  if (!is.null(parsed_args$expression)) {
    # TODO: add support for h5 / rds
    # TODO: add support for wide vs long
    task$expression <- readr::read_tsv(parsed_args$expression)
  }
  if (!is.null(parsed_args$counts)) {
    # TODO: add support for h5 / rds
    # TODO: add support for wide vs long
    task$counts <- readr::read_tsv(parsed_args$counts)
  }

  task
}

read_prior <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (name %in% c("start_n", "end_n", "groups_n")) {
    as.integer(value)
  } else if (name %in% c("start_id", "end_id", "features_id")) {
    if (grepl("^[a-zA-Z0-9/_\\-\\.]*$", value) && file.exists(value)) {
      read_tsv(value)[[1]]
    } else {
      value %>% strsplit(",") %>% first()
    }
  } else if (name %in% c("groups_id", "timecourse_discrete", "timecourse_continuous")) {
    if (grepl("^[a-zA-Z0-9/_\\-\\.]*$", value) && file.exists(value)) {
      x <- read_tsv(value) %>% deframe()
    } else {
      x <- value %>% str_replace_all(",", "\n") %>% str_replace_all("=", "\t") %>% read_tsv(col_names = c("a", "b")) %>% deframe()
    }
    if (name %in% c("timecourse_discrete", "timecourse_continuous")) {
      x <- as.numeric(x)
    }
    x
  } else if (name %in% c("groups_network")) {
    if (grepl("^[a-zA-Z0-9/_\\-\\.]*$", value) && file.exists(value)) {
      read_tsv(value)
    } else {
      value %>% str_replace_all(";", "\n") %>% str_replace_all(",", "\t") %>% read_tsv(col_names = c("from", "to"))
    }
  }
}
