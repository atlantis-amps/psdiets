#' @title Clean and organize herring diet data from DFO Cowichan
#'
#' @description Processes herring diet data from multiple years (2013-2015) collected by
#' Fisheries and Oceans Canada (DFO) in the Cowichan region. Reads data from
#' multiple Excel sheets, assigns Atlantis functional groups, and calculates
#' diet proportions.
#'
#' @param atlantis_fg_file Path to the atlantis functional groups CSV file
#' @param atlantis_groups_file Path to the atlantis groups CSV file
#' @param data.file Character. Name of the raw diet data file in XLSX format.
#' @param source.name Character. Name of data source.
#' @param cvs.file Character. Name of output file.
#' @param output.path Character. Path where output CSV should be saved.
#' @param sheet.names Character. Name of sheet to analyze
#'
#' @return A data frame with processed diet matrix containing columns:
#'   source, atlantis_pred_group, predator_stage, predator,
#'   atlantis_prey_group, prey, data, value, proportion, index.
#'
#' @details
#' Diet processing workflow:
#' \itemize{
#'   \item Reads multiple Excel sheets from the same workbook (one per year)
#'   \item Filters out empty stomachs (rows where all prey values sum to 0)
#'   \item Assigns sequential indices across all sheets to maintain unique diet IDs
#'   \item Converts from wide format (prey as columns) to long format
#'   \item Maps prey species to Atlantis functional groups
#'   \item Calculates diet proportions for each stomach sample
#'   \item Handles NaN proportions (sets to 0)
#'   \item Standardizes output format for integration with other diet data sources
#' }
#'
#' Index ranges by sheet:
#' \itemize{
#'   \item Sheet 1 (2013 Puget Sound): starts at 1
#'   \item Sheet 2 (2013 boundary): starts at 1501
#'   \item Sheet 3 (2014): starts at 2117
#'   \item Sheet 4 (2015): starts at 3242
#' }
#'
#' These index offsets ensure each diet observation has a unique identifier
#' across all years and regions.
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis, DFO, Cowichan, herring
#'
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter select rename left_join group_by summarise ungroup bind_rows arrange
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom here here
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage will save output to file
#' organize_cowichan(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path,
#' sheet.names
#' )
#'
organize_cowichan <- function(atlantis_fg_file,
                              atlantis_groups_file,
                              data.file,
                              source.name,
                              cvs.file,
                              output.path,
                              sheet_names) {

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

  # Read reference data
  atlantis_fg <- readr::read_csv(here::here("data",atlantis_fg_file), show_col_types = FALSE) %>%
    dplyr::mutate(atlantis_name = tolower(atlantis_name),
                  atlantis_name = gsub(" ", "_", atlantis_name)) %>%
    dplyr::rename(atlantis_group = atlantis_fg)

  atlantis_groups <- readr::read_csv(here::here("data",atlantis_groups_file), show_col_types = FALSE) %>%
    dplyr::distinct(species, atlantis_group) %>%
    dplyr::mutate(prey = tolower(species)) %>%
    dplyr::select(-species)

  # Define index starting points for each sheet to maintain unique IDs
  # These offsets are based on the approximate number of samples per sheet
  index_starts <- c(1, 1501, 2117, 3242)

  # Process each Excel sheet
  diet_list <- list()

  for (i in seq_along(sheet_names)) {
    sheet_name <- sheet_names[i]
    index_start <- index_starts[i]

    message("Processing sheet: ", sheet_name)

    # Read sheet and process
    diet_sheet <- readxl::read_xlsx(here::here("data",data.file), sheet = sheet_name) %>%
      # Calculate total prey per stomach
      dplyr::mutate(tot_prey = rowSums(.[4:ncol(.)])) %>%
      # Filter out empty stomachs
      dplyr::filter(tot_prey != 0) %>%
      # Remove temporary column and data column if present
      dplyr::select(-tot_prey) %>%
      {if("data" %in% names(.)) dplyr::select(., -data) else .} %>%
      # Assign sequential indices starting from sheet-specific offset
      dplyr::mutate(index = index_start:(index_start + dplyr::n() - 1)) %>%
      # Convert from wide to long format
      tidyr::gather(key = prey, value = prop,
                   -atlantis_predator_group, -predator, -predator_stage, -index) %>%
      # Standardize prey names to lowercase
      dplyr::mutate(prey = tolower(prey)) %>%
      # Join with Atlantis groups
      dplyr::left_join(atlantis_groups, by = "prey")

    diet_list[[i]] <- diet_sheet
  }

  # Combine all sheets
  diet.predator.cat <- dplyr::bind_rows(diet_list)

  # Calculate totals for normalization
  diet.predator.sum <- diet.predator.cat %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(tot_sum = sum(prop), .groups = "drop")

  # Calculate proportions and format output
  diet.cvs.final <- diet.predator.cat %>%
    dplyr::left_join(diet.predator.sum, by = "index") %>%
    dplyr::mutate(proportion = prop / tot_sum) %>%
    # Handle division by zero resulting in NaN
    dplyr::mutate(proportion = ifelse(is.nan(proportion), 0, proportion),
                  source = "dfoCowichan",
                  data = index) %>%
    dplyr::rename(atlantis_prey_group = atlantis_group,
                  value = prop) %>%
    dplyr::arrange(index, atlantis_prey_group, predator) %>%
    dplyr::select(source, atlantis_predator_group, predator_stage, predator,
                  atlantis_prey_group, prey, data, value, proportion, index) %>%
    dplyr::rename(atlantis_pred_group = atlantis_predator_group)

  readr::write_csv(diet.cvs.final, here::here(output.path, csv.file))
  message("Diet matrix saved to: ", output.path)

  # Summary statistics ------------------------------------------------------

  n_prey_groups <- length(unique(diet.cvs.final$atlantis_prey_group))
  n_prey_taxa <- length(unique(diet.cvs.final$prey))

  message("Processing complete!")
  message("  - Atlantis prey groups: ", n_prey_groups)
  message("  - Individual prey taxa: ", n_prey_taxa)
  message("  - Predator stages: ", paste(unique(diet.cvs.final$predator_stage), collapse = ", "))

  return("done")

}
