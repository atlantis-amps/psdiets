#' @title Clean and organize Hood Canal fish diets from Hans Daubenberger
#'
#' @description
#' Processes diet data for herring, salmon (Chinook, Chum, Pink), and surf smelt
#' from Hood Canal. Assigns life stages based on Von Bertalanffy growth equations
#' and calculates diet proportions by predator and prey.
#'
#' @param atlantis_fg_file Path to the atlantis functional groups CSV file
#' @param atlantis_groups_file Path to the atlantis groups CSV file
#' @param data.file Character. Name of the raw CSV diet data file.
#' @param source.name Character. Name of data source.
#' @param cvs.file Character. Name of output file.
#' @param output.path Character. Path where output CSV should be saved.
#'
#' @return A data frame with processed diet matrix containing columns:
#'   source, atlantis_pred_group, predator_stage, predator,
#'   atlantis_prey_group, prey, data, value, proportion, index.
#'
#' @details
#' Diet processing workflow:
#' \itemize{
#'   \item Filters out empty stomachs (% Full = 0%)
#'   \item Assigns life stages using Von Bertalanffy growth equations:
#'     \itemize{
#'       \item Herring: Linf=26.3cm, K=0.36 (Gunderson 1997)
#'       \item Surf smelt: Linf=24cm, K=0.399
#'       \item Maturity determined at age 2 for both species
#'       \item Fish classified as juvenile if fork length < maturity length
#'     }
#'   \item All Chinook, Chum, and Pink salmon classified as juveniles
#'   \item Marked Chinook (ADNB, ADCW, CWT tags) assigned to CI1_CI2 functional group
#'   \item Removes unidentifiable prey categories: Other, Unid, DM, FISH
#'   \item Calculates diet proportions after removing unidentifiable items
#'   \item Assigns Atlantis functional groups to both predators and prey
#' }
#'
#' Von Bertalanffy growth equation: L(t) = Linf * (1 - exp(-K * (t - T0)))
#' where Linf is asymptotic length, K is growth coefficient, and T0 is
#' theoretical age at length 0.
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis, Hood Canal
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter select rename left_join
#' @importFrom tidyr separate gather
#' @importFrom magrittr %>%
#' @importFrom here here
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage will save output to file
#' organize_daubenberg(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path
#' )
#'
organize_daubenberg <- function(atlantis_fg_file,
                                atlantis_groups_file,
                                data.file,
                                source.name,
                                cvs.file,
                                output.path){

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


  # Define internal helper functions for length-at-maturity calculations

  # Von Bertalanffy growth model for Pacific herring
  # Parameters from Gunderson 1997
  length.maturity.herring <- function(age) {
    Linf <- 26.3  # Asymptotic length in cm
    K <- 0.36     # Growth coefficient
    T_0 <- 0      # Theoretical age at length 0 (assumed 0)

    Linf * (1 - exp(-K * (age - T_0)))
  }

  # Von Bertalanffy growth model for surf smelt
  # Uses parameters for small planktivorous fish
  length.maturity.smelt <- function(age) {
    Linf <- 24    # Asymptotic length in cm
    K <- 0.399    # Growth coefficient
    T_0 <- 0      # Theoretical age at length 0 (assumed 0)

    Linf * (1 - exp(-K * (age - T_0)))
  }

  # Calculate maturity lengths at age 2 (convert cm to mm)
  length.mat.herring <- length.maturity.herring(2) * 10
  length.mat.smelt <- length.maturity.smelt(2) * 10

  # Read reference data
  atlantis_fg <- readr::read_csv(here::here("data",atlantis_fg_file), show_col_types = FALSE) %>%
    dplyr::mutate(atlantis_name = tolower(atlantis_name),
                  atlantis_name = gsub(" ", "_", atlantis_name))

  atlantis_groups <- readr::read_csv(here::here("data",atlantis_groups_file), show_col_types = FALSE) %>%
    dplyr::mutate(atlantis_name = tolower(atlantis_name),
                  atlantis_name = gsub(" ", "_", atlantis_name),
                  species = tolower(species))

  # Read and process diet data
  diet.predator.cat <- readr::read_csv(here::here("data",data.file), show_col_types = FALSE) %>%
    # Filter out empty stomachs
    dplyr::filter(`% Full` != "0%") %>%
    # Assign maturity length thresholds by species
    dplyr::mutate(length_mat = ifelse(Species == "Herring", length.mat.herring, 9999)) %>%
    dplyr::mutate(length_mat = ifelse(Species == "Surf Smelt", length.mat.smelt, length_mat)) %>%
    # Classify as juvenile or adult based on fork length vs maturity length
    dplyr::mutate(class = ifelse(`Fork Length (mm)` < length_mat, "juvenile", "adult")) %>%
    # All salmon species are juveniles in this dataset
    dplyr::mutate(class = ifelse(Species == "Chinook" | Species == "Chum" | Species == "Pink",
                                 "juvenile", class)) %>%
    dplyr::select(class, everything()) %>%
    # Parse date and rename location column
    tidyr::separate(Date, c("mo", "day", "year"), "/") %>%
    dplyr::rename(location = Region) %>%
    # Select relevant columns, removing metadata and unneeded fields
    dplyr::select(year, class, location, everything(),
                  -`% Full`, -`% DM`, -`Fork Length (mm)`, -`Wet Weight (g)`,
                  -mo, -day, -length_mat, -Month, -Site, -`Tow #`, -`Fish #`, -`Diet Type`) %>%
    # Remove unidentifiable prey categories
    dplyr::select(-Other, -Unid, -DM, -FISH) %>%
    # Calculate total diet per fish and create index
    dplyr::mutate(sum_tot = rowSums(.[6:20]), index = 1:dplyr::n()) %>%
    # Only keep diets with non-zero prey
    dplyr::filter(sum_tot != 0) %>%
    # Convert from wide to long format
    tidyr::gather(species, value, -location, -year, -class, -index, -sum_tot, -Species, -Marked) %>%
    dplyr::arrange(index, Species) %>%
    # Handle missing mark data
    dplyr::mutate(Marked = ifelse(is.na(Marked), "None", Marked))

  # Assign Atlantis groups and calculate proportions
  diet.cvs.final <- diet.predator.cat %>%
    dplyr::mutate(source = "Daubenberg_Hood_canal",
                  species = trimws(tolower(species))) %>%
    # Join with Atlantis groups for prey
    dplyr::left_join(atlantis_groups, by = "species") %>%
    dplyr::rename(atlantis_prey_group = atlantis_fg,
                  prey_taxa = species_name,
                  predator_stage = class,
                  prey = species,
                  species = Species) %>%
    dplyr::mutate(species = trimws(tolower(species))) %>%
    # Join with Atlantis groups for predators
    dplyr::left_join(atlantis_groups, by = "species") %>%
    dplyr::rename(atlantis_predator_group = atlantis_fg,
                  predator = species) %>%
    # Calculate diet proportions
    dplyr::mutate(proportion = value / sum_tot) %>%
    dplyr::mutate(data = index,
                  # Marked Chinook assigned to CI1_CI2 group
                  atlantis_predator_group = ifelse(
                    predator == "Chinook" & (Marked == "ADNB" | Marked == "ADCW" | Marked == "CWT"),
                    "CI1_CI2",
                    atlantis_predator_group
                  )) %>%
    dplyr::select(source, atlantis_predator_group, predator_stage, predator,
                  atlantis_prey_group, prey_taxa, data, value, proportion, index) %>%
    dplyr::rename(atlantis_pred_group = atlantis_predator_group,
                  prey = prey_taxa)

  # Save output -------------------------------------------------------------

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
