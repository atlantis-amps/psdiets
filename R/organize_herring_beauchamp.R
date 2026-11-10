#' @title Process and Organize Pacific Herring Diet Data from Beauchamp Lab
#'
#' @description
#' This function processes Pacific herring diet data from Beauchamp lab studies,
#' mapping prey items to Atlantis functional groups and calculating diet proportions.
#' The function handles data from multiple Excel sheets and combines them with
#' sequential indexing.
#'
#' @details
#' The function processes diet data for Pacific herring from Puget Sound. It reads
#' data from two Excel sheets within a single workbook, combines them, and maps
#' prey items to Atlantis functional groups for ecosystem modeling.
#'
#' Each stomach sample is assigned a unique index (starting from 1 for sheet 1,
#' continuing sequentially for sheet 2). Diet proportions are calculated by
#' normalizing prey weights within each stomach sample.
#'
#' The function expects Excel sheets with columns:
#' \itemize{
#'   \item atlantis_predator_group: Atlantis functional group code for predator
#'   \item predator: Common name of predator species
#'   \item predator_stage: Life stage (e.g., juvenile, adult)
#'   \item Remaining columns: Prey taxa with weight values
#' }
#'
#' @param atlantis_fg_file Path to the atlantis functional groups CSV file
#' @param atlantis_groups_file Path to the atlantis groups CSV file
#' @param data.file Character. Name of the raw CSV diet data file.
#' @param source.name Character. Name of data source.
#' @param cvs.file Character. Name of output file.
#' @param output.path Character. Path where output CSV should be saved.
#' @param sheet_name.1 Character. Name of the first sheet to analyze
#' @param sheet_name.2 Character. Name of the second sheet to analyze
#'
#' @return Character string "done" upon successful completion. The function also
#'   writes a CSV file to the specified output path containing processed diet data
#'   with the following columns:
#' \describe{
#'   \item{source}{Data source identifier}
#'   \item{atlantis_pred_group}{Atlantis functional group code for predator}
#'   \item{predator_stage}{Life stage of predator}
#'   \item{predator}{Common name of predator species}
#'   \item{atlantis_prey_group}{Atlantis functional group code for prey}
#'   \item{prey}{Prey taxa name (lowercase)}
#'   \item{value}{Raw weight of prey item}
#'   \item{data}{Unique index for each stomach sample}
#'   \item{proportion}{Proportion of prey item in diet (0-1)}
#'   \item{index}{Same as data column, unique index for each stomach sample}
#' }
#'
#' @references
#' Data from Beauchamp Lab studies on Pacific herring diet composition in Puget Sound.
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis, diet data, herring, Pacific herring
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter select rename left_join
#' @importFrom tidyr separate gather
#' @importFrom magrittr %>%
#' @importFrom here here
#' @importFrom readxl read_xlsx
#' @examples
#' \dontrun{
#' # Basic usage with two Excel sheets
#' will save output to file
#' organize_herring_beauchamp(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path
#' sheet_name.1,
#'sheet_name.2"
#' )
#'
#'
#' @export
organize_herring_beauchamp <- function(atlantis_fg_file,
                                       atlantis_groups_file,
                                       data.file,
                                       source.name,
                                       cvs.file,
                                       output.path,
                                       sheet_name.1,
                                       sheet_name.2) {

  # Input validation --------------------------------------------------------

  if (!file.exists(here::here("data", data.file))) {
    stop("Diet file not found: ", data.file)
  }
  if (!file.exists(here::here("data", atlantis_fg_file))) {
    stop("Atlantis functional groups file not found: ", atlantis_fg_file)
  }
  if (!file.exists(here::here("data", atlantis_groups_file))) {
    stop("Atlantis groups file not found: ", atlantis_groups_file)
  }

  # Validate sheet names exist in the Excel file
  available_sheets <- readxl::excel_sheets(here::here("data", data.file))
  if (!sheet_name.1 %in% available_sheets) {
    stop("Sheet '", sheet_name.1, "' not found in ", data.file,
         "\nAvailable sheets: ", paste(available_sheets, collapse = ", "))
  }
  if (!sheet_name.2 %in% available_sheets) {
    stop("Sheet '", sheet_name.2, "' not found in ", data.file,
         "\nAvailable sheets: ", paste(available_sheets, collapse = ", "))
  }

   # Read input data ---------------------------------------------------------

  message("Reading input data files...")

  # Read Atlantis functional group definitions
  # Standardize names to lowercase with underscores
  atlantis_fg <- readr::read_csv(here::here("data", atlantis_fg_file),
                                  show_col_types = FALSE) %>%
    dplyr::mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    ) %>%
    dplyr::rename(atlantis_group = atlantis_fg)

  # Read Atlantis-Beauchamp species group mappings
  # Extract unique species-to-group mappings
  atlantis_groups <- readr::read_csv(here::here("data", atlantis_groups_file),
                                      show_col_types = FALSE) %>%
    dplyr::distinct(species, atlantis_group) %>%
    dplyr::mutate(prey = tolower(species)) %>%
    dplyr::select(-species)

  message("  - Atlantis functional groups: ", nrow(atlantis_fg))
  message("  - Species mappings: ", nrow(atlantis_groups))

  # Process first Excel sheet -----------------------------------------------

  message("Reading diet data from sheet: '", sheet_name.1, "'...")

  # Read first sheet and process
  diet.predator.cat1 <- readxl::read_xlsx(
    here::here("data", data.file),
    sheet = sheet_name.1
  ) %>%
    # Calculate total prey per stomach
    dplyr::mutate(tot_prey = rowSums(.[5:ncol(.)], na.rm = TRUE)) %>%
    # Remove stomachs with no prey
    dplyr::filter(tot_prey != 0) %>%
    dplyr::select(-tot_prey) %>%
    # Create sequential index starting at 1
    dplyr::mutate(index = 1:dplyr::n()) %>%
    # Convert to long format (each row is one prey item in one stomach)
    tidyr::gather(key = prey, value = weight,
                  -atlantis_predator_group, -predator, -predator_stage, -index) %>%
    # Standardize prey names to lowercase
    dplyr::mutate(prey = tolower(prey)) %>%
    # Map prey species to Atlantis functional groups
    dplyr::left_join(atlantis_groups, by = "prey")

  n_stomachs_1 <- max(diet.predator.cat1$index)
  message("  - Found ", n_stomachs_1, " stomachs with contents")

  # Process second Excel sheet ----------------------------------------------

  message("Reading diet data from sheet: '", sheet_name.2, "'...")

  # Read second sheet and process
  # Index continues from where first sheet ended (174 in original code,
  # now calculated dynamically)
  start_index <- n_stomachs_1 + 1

  diet.predator.cat2 <- readxl::read_xlsx(
    here::here("data", data.file),
    sheet = sheet_name.2
  ) %>%
    # Calculate total prey per stomach
    dplyr::mutate(tot_prey = rowSums(.[5:ncol(.)], na.rm = TRUE)) %>%
    # Remove stomachs with no prey
    dplyr::filter(tot_prey != 0) %>%
    dplyr::select(-tot_prey) %>%
    # Create sequential index continuing from first sheet
    dplyr::mutate(index = start_index:(start_index + dplyr::n() - 1)) %>%
    # Convert to long format
    tidyr::gather(key = prey, value = weight,
                  -atlantis_predator_group, -predator, -predator_stage, -index) %>%
    # Standardize prey names to lowercase
    dplyr::mutate(prey = tolower(prey)) %>%
    # Map prey species to Atlantis functional groups
    dplyr::left_join(atlantis_groups, by = "prey")

  n_stomachs_2 <- max(diet.predator.cat2$index) - n_stomachs_1
  message("  - Found ", n_stomachs_2, " stomachs with contents")

  # Combine data from both sheets -------------------------------------------

  message("Combining data from both sheets...")

  # Combine both datasets into single long-format dataset
  diet.predator.cat <- diet.predator.cat1 %>%
    dplyr::bind_rows(diet.predator.cat2)

  total_stomachs <- n_stomachs_1 + n_stomachs_2
  message("  - Total stomachs: ", total_stomachs)

  # Check for unmapped prey species
  unmapped_prey <- diet.predator.cat %>%
    dplyr::filter(is.na(atlantis_group)) %>%
    dplyr::distinct(prey)

  if (nrow(unmapped_prey) > 0) {
    warning("The following prey taxa are not mapped to Atlantis groups:\n",
            paste(unmapped_prey$prey, collapse = ", "))
  }

  # Calculate total weight per stomach --------------------------------------

  message("Calculating diet proportions...")

  # Sum total prey weight for each stomach sample
  diet.predator.sum <- diet.predator.cat %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(tot_sum = sum(weight, na.rm = TRUE), .groups = "drop")

  # Calculate proportions and format output ---------------------------------

  # Join with totals and calculate proportions
  diet.cvs.final <- diet.predator.cat %>%
    dplyr::left_join(diet.predator.sum, by = "index") %>%
    # Calculate proportion of each prey item in diet
    dplyr::mutate(
      proportion = weight / tot_sum,
      # Handle any NaN values (e.g., if tot_sum is 0)
      proportion = dplyr::if_else(is.nan(proportion), 0, proportion),
      # Add source identifier
      source = source.name,
      # Create data column (duplicate of index for consistency with other functions)
      data = index
    ) %>%
    # Rename columns to standard format
    dplyr::rename(
      atlantis_prey_group = atlantis_group,
      value = weight
    ) %>%
    # Sort for consistent output
    dplyr::arrange(index, atlantis_prey_group, predator) %>%
    # Select and order final columns
    dplyr::select(
      source, atlantis_predator_group, predator_stage, predator,
      atlantis_prey_group, prey, value, data, proportion, index
    ) %>%
    # Rename predator group for consistency
    dplyr::rename(atlantis_pred_group = atlantis_predator_group)

  # Quality checks ----------------------------------------------------------

  # Check that proportions sum to ~1 for each stomach
  prop_check <- diet.cvs.final %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(sum_prop = sum(proportion, na.rm = TRUE), .groups = "drop")

  # Allow small rounding errors
  invalid_sums <- prop_check %>%
    dplyr::filter(abs(sum_prop - 1.0) > 0.01)

  if (nrow(invalid_sums) > 0) {
    warning("Proportions do not sum to 1.0 for ", nrow(invalid_sums),
            " stomach samples. Check indices: ",
            paste(head(invalid_sums$index, 5), collapse = ", "))
  }

  # Save output -------------------------------------------------------------

  # Create output directory if it doesn't exist
  if (!dir.exists(here::here(output.path))) {
    message("Creating output directory: ", output.path)
    dir.create(here::here(output.path), recursive = TRUE)
  }

  # Write CSV file
  readr::write_csv(diet.cvs.final, here::here(output.path, csv.file))
  message("Diet matrix saved to: ", output.path)

  # Summary statistics ------------------------------------------------------

  n_prey_groups <- length(unique(diet.cvs.final$atlantis_prey_group))
  n_prey_taxa <- length(unique(diet.cvs.final$prey))

  message("Processing complete!")
  message("  - Total stomachs processed: ", total_stomachs)
  message("  - Atlantis prey groups: ", n_prey_groups)
  message("  - Individual prey taxa: ", n_prey_taxa)
  message("  - Predator stages: ", paste(unique(diet.cvs.final$predator_stage), collapse = ", "))

  return("done")
}
