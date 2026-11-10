#' @title Process and Organize Lingcod Diet Data from Beaudreau
#'
#' @description
#' This function processes lingcod diet data from Beaudreau & Essington studies,
#' classifying fish by life stage (juvenile/adult), mapping prey items to Atlantis
#' functional groups, and calculating diet proportions. The function handles
#' unidentified fish by distributing them proportionally among identified fish groups.
#'
#' @details
#' The function processes diet data for lingcod (Ophiodon elongatus) from Puget Sound.
#' It uses von Bertalanffy growth parameters to estimate length at maturity and
#' classify individuals as juvenile or adult. Prey items are aggregated into
#' Atlantis functional groups for ecosystem modeling.
#'
#' Unidentified fish in the diet are proportionally distributed among identified
#' fish groups based on their relative abundance in each stomach sample.
#'
#' @param atlantis_fg_file Path to the atlantis functional groups CSV file
#' @param atlantis_groups_file Path to the atlantis groups CSV file
#' @param data.file Character. Name of the raw CSV diet data file.
#' @param source.name Character. Name of data source.
#' @param cvs.file Character. Name of output file.
#' @param output.path Character. Path where output CSV should be saved.
#'
#' @return A tibble containing processed diet data with the following columns:
#' \describe{
#'   \item{source}{Data source identifier ("Beaudreau_Lingcod")}
#'   \item{atlantis_pred_group}{Atlantis functional group code for predator ("FVS")}
#'   \item{predator_stage}{Life stage of predator ("adult" or "juvenile")}
#'   \item{predator}{Common name of predator species ("Lingcod")}
#'   \item{atlantis_prey_group}{Atlantis functional group code for prey}
#'   \item{prey}{Prey taxa name}
#'   \item{data}{Fish ID from original dataset}
#'   \item{value}{Raw count or weight of prey item}
#'   \item{proportion}{Proportion of prey item in diet (0-1)}
#'   \item{index}{Unique index for each stomach sample}
#' }
#'
#' @references
#' Beaudreau, A.H., T.E. Essington. 2009. Development of a new field-based
#' approach for estimating consumption rates of fishes and comparison with a
#' bioenergetics model for lingcod (Ophiodon elongatus). Canadian Journal of
#' Fisheries and Aquatic Sciences 66(4):565-578.
#'
#' Beaudreau, A.H., T.E. Essington. 2007. Spatial, temporal, and ontogenetic
#' patterns of predation on rockfishes by lingcod. Transactions of the American
#' Fisheries Society 136:1438-1452.
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis, diet data, lingcod
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter select rename left_join group_by summarise ungroup bind_rows arrange
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' # Basic usage will save output to file
#' organize_beaudreau(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path
#' )
#'
#'
#' @export
#'
organize_beaudreau <- function(atlantis_fg_file,
                                    atlantis_groups_file,
                                    data.file,
                                    source.name,
                                    cvs.file,
                                    output.path) {

  # Input validation --------------------------------------------------------

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

  # Read Atlantis functional group definitions
  atlantis_fg <- readr::read_csv(here::here("data",atlantis_fg_file), show_col_types = FALSE) %>%
    dplyr::mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    )

  # Read Atlantis-Beaudreau species group mappings
  atlantis_groups <- readr::read_csv(here::here("data",atlantis_groups_file), show_col_types = FALSE) %>%
    dplyr::mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    )

  # Calculate maturity lengths ----------------------------------------------

  # Calculate length at maturity for different sexes using von Bertalanffy
  # growth equation. Age at maturity: males = 2.506 years, females = 4.323 years
  length_mat_f <- calculate_length_at_maturity(4.323)  # Females
  length_mat_m <- calculate_length_at_maturity(2.506)  # Males
  length_mat_u <- calculate_length_at_maturity((4.323 + 2.506) / 2)  # Unknown sex

  message("Length at maturity - Males: ", round(length_mat_m, 2),
          " cm, Females: ", round(length_mat_f, 2), " cm")

  # Process diet data -------------------------------------------------------

  message("Processing diet data...")

  # Read and categorize diet data by predator characteristics
  diet_predator_cat <- readr::read_csv(here::here("data",data.file), show_col_types = FALSE) %>%
    # Filter for stomachs with contents
    dplyr::filter(StomachContents == 1) %>%
    # Convert date from Excel numeric format
    dplyr::mutate(date = as.Date(SamplingDate, origin = "1899-12-30")) %>%
    # Assign maturity length based on sex
    dplyr::mutate(
      length_mat = dplyr::if_else(
        Sex == 1, length_mat_m,
        dplyr::if_else(Sex == 2, length_mat_f, length_mat_u)
      )
    ) %>%
    # Classify as adult or juvenile based on length relative to maturity
    dplyr::mutate(class = dplyr::if_else(length_mat > Length, "adult", "juvenile")) %>%
    # Extract year, month, day
    tidyr::separate(date, c("year", "mo", "day"), "-") %>%
    dplyr::rename(location = "GeneralLocation") %>%
    # Select relevant columns
    dplyr::select(
      year, class, location, dplyr::everything(),
      -ProtectionStatus, -StomachContents, -Sex, -Length, -Weight,
      -mo, -day, -SamplingDate, -length_mat
    ) %>%
    # Calculate total prey items per stomach
    dplyr::mutate(sum_tot = rowSums(.[5:86], na.rm = TRUE)) %>%
    # Remove empty stomachs
    dplyr::filter(sum_tot != 0) %>%
    # Convert to long format
    tidyr::gather(species, value, -location, -FishID, -year, -class, -sum_tot)

  message("Found ", length(unique(diet_predator_cat$FishID)), " fish with stomach contents")

  # Aggregate by Atlantis groups --------------------------------------------

  message("Aggregating prey to Atlantis functional groups...")

  diet_predator_sum <- diet_predator_cat %>%
    # Map species to Atlantis groups
    dplyr::left_join(atlantis_groups, by = "species") %>%
    dplyr::mutate(atlantis_name = tolower(atlantis_name)) %>%
    # Remove "other" category
    dplyr::filter(atlantis_name != "other") %>%
    # Sum values by Atlantis group
    dplyr::group_by(year, class, location, FishID, atlantis_name) %>%
    dplyr::summarise(tot_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
    # Convert back to wide format
    tidyr::spread(atlantis_name, tot_val, fill = 0)

  # Distribute unidentified fish --------------------------------------------

  message("Distributing unidentified fish among identified groups...")

  # Calculate total identified fish per stomach
  diet_predator_sum <- diet_predator_sum %>%
    dplyr::mutate(
      tot_fish = demersal_rockfish + piscivorous_flatfish + small_demersal_fish +
        `small-mouthed_flatfish` + herring + large_demersal_predators + perch +
        salmon + small_planktivorous_fish + spiny_dogfish + `hake_/large_gadoids`
    )

  # Distribute unidentified fish proportionally to each identified fish group
  # Only add to groups that already have values (avoid creating false positives)
  fish_groups <- c(
    "demersal_rockfish", "piscivorous_flatfish", "small_demersal_fish",
    "small-mouthed_flatfish", "herring", "large_demersal_predators", "perch",
    "salmon", "small_planktivorous_fish", "spiny_dogfish", "hake_/large_gadoids"
  )

  for (group in fish_groups) {
    diet_predator_sum <- diet_predator_sum %>%
      dplyr::mutate(
        !!group := dplyr::if_else(
          .data[[group]] != 0,
          .data[[group]] + ((.data[[group]] / tot_fish) * fish),
          0
        )
      )
  }

  diet_predator_sum <- diet_predator_sum %>%
    dplyr::select(-fish, -tot_fish)

  # Calculate proportions ---------------------------------------------------

  message("Calculating diet proportions...")

  diet.cvs.final <- diet_predator_sum %>%
    # Calculate total prey per stomach
    dplyr::mutate(sum_tot = rowSums(.[5:23], na.rm = TRUE)) %>%
    # Remove any remaining empty stomachs
    dplyr::filter(sum_tot != 0) %>%
    # Convert to long format
    tidyr::gather(atlantis_name, tot_value, -FishID, -year, -class, -location, -sum_tot) %>%
    # Calculate proportions
    dplyr::mutate(
      proportion = tot_value / sum_tot,
      proportion = dplyr::if_else(is.nan(proportion), 0, proportion)
    ) %>%
    dplyr::select(-sum_tot) %>%
    dplyr::arrange(FishID) %>%
    # Add metadata
    dplyr::mutate(
      source = "Beaudreau_Lingcod",
      predator = "Lingcod",
      atlantis_predator_group = "FVS"
    ) %>%
    # Join with functional group codes
    dplyr::left_join(atlantis_fg, by = "atlantis_name") %>%
    # Rename columns to standard format
    dplyr::rename(
      prey_taxa = atlantis_name,
      atlantis_prey_group = atlantis_fg,
      predator_stage = class,
      value = tot_value,
      data = FishID
    ) %>%
    # Create unique index for each stomach
    dplyr::mutate(index = dplyr::group_indices(., data)) %>%
    # Select and order final columns
    dplyr::select(
      source, atlantis_predator_group, predator_stage, predator,
      atlantis_prey_group, prey_taxa, data, value, proportion, index
    ) %>%
    dplyr::rename(
      atlantis_pred_group = atlantis_predator_group,
      prey = prey_taxa
    )

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


#' Calculate Length at Maturity Using von Bertalanffy Growth Equation
#'
#' @description
#' Helper function to calculate length at a given age using the von Bertalanffy
#' growth equation with parameters specific to lingcod from Puget Sound.
#'
#' @details
#' Uses the von Bertalanffy growth equation: L(a) = Linf * (1 - exp(-K * (a - T0)))
#'
#' Growth parameters from Beaudreau & Essington (2007):
#' \itemize{
#'   \item Linf = 91.8169 cm (asymptotic length)
#'   \item K = 0.149 (growth rate coefficient)
#'   \item T0 = -3.1023 (theoretical age at zero length)
#' }
#'
#' @param age Numeric. Age in years.
#' @param Linf Numeric. Asymptotic length in cm. Default is 91.8169.
#' @param K Numeric. Growth rate coefficient. Default is 0.149.
#' @param T0 Numeric. Theoretical age at zero length. Default is -3.1023.
#'
#' @return Numeric. Predicted length in cm at the given age.
#'
#' @references
#' Beaudreau, A.H., T.E. Essington. 2007. Spatial, temporal, and ontogenetic
#' patterns of predation on rockfishes by lingcod. Transactions of the American
#' Fisheries Society 136:1438-1452.
#'
#' @examples
#' # Calculate length at age 2.506 years (male maturity)
#' calculate_length_at_maturity(2.506)
#'
#' # Calculate length at age 4.323 years (female maturity)
#' calculate_length_at_maturity(4.323)
#'
#' # Calculate length at multiple ages
#' ages <- seq(1, 10, by = 1)
#' lengths <- sapply(ages, calculate_length_at_maturity)
#' plot(ages, lengths, type = "l", xlab = "Age (years)", ylab = "Length (cm)")
#'
#' @export
calculate_length_at_maturity <- function(age,
                                          Linf = 91.8169,
                                          K = 0.149,
                                          T0 = -3.1023) {

  # Input validation
  if (!is.numeric(age) || any(age < 0)) {
    stop("Age must be a non-negative numeric value")
  }

  if (!is.numeric(Linf) || Linf <= 0) {
    stop("Linf must be a positive numeric value")
  }

  if (!is.numeric(K) || K <= 0) {
    stop("K must be a positive numeric value")
  }

  if (!is.numeric(T0)) {
    stop("T0 must be a numeric value")
  }

  # von Bertalanffy growth equation
  length_cm <- Linf * (1 - exp(-K * (age - T0)))

  return(length_cm)
}
