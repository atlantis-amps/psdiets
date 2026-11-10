#' @title Organize and process Baker UW diet data
#'
#' @description
#' Processes diet data from Baker et al. (University of Washington) by reading
#' predator-prey matrices, assigning Atlantis functional groups, and calculating
#' diet proportions. Exports standardized diet matrix CSV file.
#'
#' @param atlantis_fg_file Path to the atlantis functional groups CSV file
#' @param atlantis_groups_file Path to the atlantis groups CSV file
#' @param data.file Character. Name of the raw CSV diet data file.
#' @param source.name Character. Name of data source.
#' @param cvs.file Character. Name of output file.
#' @param output.path Character. Path where output CSV should be saved.
#'
#' @return Character. Returns "done" when processing is complete.
#'   Writes standardized diet matrix to "outputs/diet_data_files/{csv.file}".
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Standardizes atlantis_fg column names and converts names to lowercase
#'   \item Creates separate lookup tables for predators and prey
#'   \item Reads diet data and converts from wide to long format
#'   \item Joins with Atlantis group assignments for predators and prey
#'   \item Filters out zero values
#'   \item Calculates total diet per predator
#'   \item Normalizes to get diet proportions
#'   \item Creates unique indices for each diet observation
#'   \item Exports to CSV in standard format with columns:
#'     source, atlantis_pred_group, predator_stage, predator,
#'     atlantis_prey_group, prey, data, value, proportion, index
#' }
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate rename filter left_join group_by summarise arrange select
#' @importFrom tidyr gather
#' @importFrom here here
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Process diet data
#' # Basic usage will save output to file
#' organize_baker(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path
#' )
#'
organize_baker <- function(
  atlantis_fg_file,
  atlantis_groups_file,
  data.file,
  source.name,
  csv.file,
  output.path
) {
  # Input validation
  if (!file.exists(here::here("data", data.file))) {
    stop("Diet file not found: ", data.file)
  }
  if (!file.exists(here::here("data", atlantis_fg_file))) {
    stop("Atlantis functional groups file not found: ", atlantis_fg_file)
  }
  if (!file.exists(here::here("data", atlantis_groups_file))) {
    stop("Atlantis groups file not found: ", atlantis_groups_file)
  }

  # Read input data ---------------------------------------------------------

  message("Reading input data files...")

  atlantis_fg <- read_csv(here::here("data", atlantis_fg_file)) %>%
    dplyr::mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    ) %>%
    dplyr::rename(atlantis_group = atlantis_fg)

  atlantis_groups <- read_csv(here::here("data", atlantis_groups_file))

  atlantis_groups_pred <- atlantis_groups %>%
    dplyr::mutate(predator = tolower(species), prey = tolower(species)) %>%
    dplyr::distinct(predator, atlantis_group) # %>%
  #get_dupes(predator) #used to find duplicates

  atlantis_groups_prey <- atlantis_groups %>%
    dplyr::mutate(predator = tolower(species), prey = tolower(species)) %>%
    dplyr::distinct(prey, atlantis_group) # %>%

  diet.predator.cat <- read_csv(here::here("data", data.file)) %>%
    dplyr::mutate(index = 1:nrow(.)) %>%
    dplyr::gather(
      key = prey,
      value = val,
      -predator,
      -predator_stage,
      -data,
      -index
    ) %>%
    dplyr::mutate(predator = tolower(predator), prey = tolower(prey)) %>%
    dplyr::left_join(atlantis_groups_pred, by = "predator") %>%
    dplyr::rename(atlantis_pred_group = atlantis_group) %>%
    dplyr::left_join(atlantis_groups_prey, by = "prey") %>%
    dplyr::rename(atlantis_prey_group = atlantis_group) %>%
    dplyr::filter(val != 0)

  diet.predator.sum <- diet.predator.cat %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(tot_sum = sum(val)) %>%
    dplyr::ungroup()

  #get proportions and normalizing
  diet.cvs.final <- diet.predator.cat %>%
    dplyr::left_join(diet.predator.sum, by = "index") %>%
    dplyr::mutate(proportion = val / tot_sum) %>%
    dplyr::mutate(
      proportion = if_else(is.nan(proportion), 0, proportion),
      source = source.name,
      data = index
    ) %>%
    dplyr::arrange(index, atlantis_prey_group, predator) %>%
    dplyr::rename(value = val) %>%
    dplyr::select(
      source,
      atlantis_pred_group,
      predator_stage,
      predator,
      atlantis_prey_group,
      prey,
      data,
      value,
      proportion,
      index
    ) %>%
    dplyr::mutate(
      index = group_indices_(
        .,
        .dots = c('source', 'data', 'atlantis_pred_group')
      )
    )

  readr::write_csv(diet.cvs.final, here::here(output.path, csv.file))
  message("Diet matrix saved to: ", output.path)

  # Summary statistics ------------------------------------------------------

  n_prey_groups <- length(unique(diet.cvs.final$atlantis_prey_group))
  n_prey_taxa <- length(unique(diet.cvs.final$prey))

  message("Processing complete!")
  message("  - Atlantis prey groups: ", n_prey_groups)
  message("  - Individual prey taxa: ", n_prey_taxa)
  message(
    "  - Predator stages: ",
    paste(unique(diet.cvs.final$predator_stage), collapse = ", ")
  )

  return("done")
}
