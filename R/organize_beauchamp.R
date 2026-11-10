#' @title Clean and organize salmon diets from Dave Beauchamp
#'
#' @description
#' Data of salmon gut contents from several watersheds sent by Iris Kemp.
#' Reads in values, summarizes and assigns atlantis groups.
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
#'   atlantis_prey_group, prey, data, value, proportion, index
#'
#' @details
#' Data is weight of diet items in grams. The function:
#' \itemize{
#'   \item Filters to processed diets only
#'   \item Distributes unidentified salmon between identified salmon groups
#'   \item Distributes unidentified fish between all fish groups
#'   \item Calculates proportions by predator and prey
#'   \item Assigns predator life stage based on fork length (>300mm = adult)
#' }
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter select rename group_by summarise ungroup left_join
#' @importFrom tidyr gather spread separate
#' @importFrom magrittr %>%
#' @importFrom here here
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage will save output to file
#' organize_beauchamp(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path
#' )
#'
organize_beauchamp <- function(
  atlantis_fg_file,
  atlantis_groups_file,
  data.file,
  source.name,
  cvs.file,
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

  atlantis_fg <- readr::read_csv(
    here::here(
      "data",
      atlantis_fg_file
    ),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    )

  # Data is weight of diet items in grams
  atlantis_groups <- readr::read_csv(
    here::here(
      "data",
      atlantis_groups_file
    ),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    )

  # Read and process diet data
  # Variables read incorrectly because they start as integer and have text
  diet.salmon.cat <- readr::read_csv(
    here::here("data", data.file),
    col_types = readr::cols(
      `Thawed Fork Length (mm)` = readr::col_double(),
      Set_Tow = readr::col_character()
    )
  ) %>%
    # File has 13,000+ entries but only 2000+ diets were processed
    dplyr::filter(`Diet Processed` == 1) %>%
    dplyr::select(
      `Wet Fork length (mm)`,
      `Watershed of Origin (CWT/Otolith/GSI)`,
      Species,
      `Fish Age`,
      `Mark Type (see protocol)`,
      `Diptera Adult`:`Digested Fish-ww`,
      Date_Captured
    ) %>%
    dplyr::rename(location = `Watershed of Origin (CWT/Otolith/GSI)`) %>%
    tidyr::separate(Date_Captured, c("mo", "day", "year"), "/") %>%
    dplyr::select(
      `Wet Fork length (mm)`,
      `Digested Material`,
      `UnID Fish-ww`,
      `UnID Salmonid-ww`,
      `Other Fish-ww`,
      `Digested Fish-ww`,
      -Indigestible,
      everything()
    ) %>%
    # Distribute unidentified salmon between Id'd salmon groups
    dplyr::mutate(tot_salmon = `Pink Salmon-ww` + `Chum Salmon-ww`) %>%
    dplyr::mutate(
      `Pink Salmon-ww` = ifelse(
        `Pink Salmon-ww` != 0,
        `Pink Salmon-ww` +
          ((`Pink Salmon-ww` / tot_salmon) * `UnID Salmonid-ww`),
        0
      )
    ) %>%
    dplyr::mutate(
      `Chum Salmon-ww` = ifelse(
        `Chum Salmon-ww` != 0,
        `Chum Salmon-ww` +
          ((`Chum Salmon-ww` / tot_salmon) * `UnID Salmonid-ww`),
        0
      )
    ) %>%
    dplyr::mutate(
      tot_fish = `Pink Salmon-ww` +
        `Chum Salmon-ww` +
        `Sand Lance-ww` +
        `Herring-ww`
    ) %>%
    # Distribute unidentified fish between all fish groups
    dplyr::mutate(
      tot_other_fish = `UnID Fish-ww` + `Other Fish-ww` + `Digested Fish-ww`
    ) %>%
    dplyr::mutate(
      `Pink Salmon-ww` = ifelse(
        `Pink Salmon-ww` != 0,
        `Pink Salmon-ww` + ((`Pink Salmon-ww` / tot_fish) * tot_other_fish),
        0
      )
    ) %>%
    dplyr::mutate(
      `Chum Salmon-ww` = ifelse(
        `Chum Salmon-ww` != 0,
        `Chum Salmon-ww` + ((`Chum Salmon-ww` / tot_fish) * tot_other_fish),
        0
      )
    ) %>%
    dplyr::mutate(
      `Sand Lance-ww` = ifelse(
        `Sand Lance-ww` != 0,
        `Sand Lance-ww` + ((`Sand Lance-ww` / tot_fish) * tot_other_fish),
        0
      )
    ) %>%
    dplyr::mutate(
      `Herring-ww` = ifelse(
        `Herring-ww` != 0,
        `Herring-ww` + ((`Herring-ww` / tot_fish) * tot_other_fish),
        0
      )
    ) %>%
    dplyr::mutate(
      predator_stage = ifelse(`Wet Fork length (mm)` > 300, "adult", "juvenile")
    ) %>%
    dplyr::mutate(
      predator_stage = ifelse(
        is.na(`Wet Fork length (mm)`),
        "adult",
        predator_stage
      )
    ) %>%
    dplyr::select(
      -`Wet Fork length (mm)`,
      -`UnID Fish-ww`,
      -`UnID Salmonid-ww`,
      -`Other Fish-ww`,
      -`Digested Fish-ww`,
      -Indigestible,
      -tot_salmon,
      -tot_fish,
      -tot_other_fish,
      -mo,
      -day
    ) %>%
    dplyr::mutate(index = 1:dplyr::n())

  # Summarize by atlantis groups
  diet.salmon.sum <- diet.salmon.cat %>%
    tidyr::gather(
      species,
      value,
      -location,
      -Species,
      -`Mark Type (see protocol)`,
      -index,
      -year,
      -`Fish Age`,
      -predator_stage
    ) %>%
    # Join atlantis groups and summarise by them
    dplyr::left_join(atlantis_groups, by = "species") %>%
    dplyr::group_by(
      year,
      index,
      Species,
      location,
      `Fish Age`,
      `Mark Type (see protocol)`,
      atlantis_name,
      predator_stage
    ) %>%
    dplyr::summarise(tot_val = sum(value), .groups = "drop") %>%
    tidyr::spread(atlantis_name, tot_val, fill = 0)

  # Get proportions including insects
  diet.salmon.agg <- diet.salmon.sum %>%
    dplyr::select(-digested) %>%
    dplyr::mutate(sum_tot = rowSums(.[8:16])) %>%
    tidyr::gather(
      atlantis_name,
      tot_value,
      -year,
      -index,
      -Species,
      -location,
      -`Fish Age`,
      -`Mark Type (see protocol)`,
      -sum_tot,
      -predator_stage
    ) %>%
    dplyr::mutate(proportion = tot_value / sum_tot) %>%
    dplyr::mutate(proportion = ifelse(is.nan(proportion), 0, proportion)) %>%
    dplyr::select(-tot_value, -sum_tot) %>%
    tidyr::spread(atlantis_name, proportion, fill = 0) %>%
    dplyr::mutate(sum_tot = rowSums(.[8:16])) %>%
    dplyr::filter(sum_tot != 0) %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%
    dplyr::mutate(
      location = ifelse(
        `Mark Type (see protocol)` == "UM",
        "wild",
        ifelse(is.na(location), "unknown", location)
      )
    ) %>%
    dplyr::mutate(
      origin = ifelse(`Mark Type (see protocol)` == "UM", "wild", "hatchery")
    ) %>%
    dplyr::rename(species = Species) %>%
    dplyr::mutate(
      species = ifelse(species == "CK", "CI", species),
      `Fish Age` = ifelse(is.na(`Fish Age`), "None", `Fish Age`)
    ) %>%
    dplyr::mutate(
      species = ifelse(
        species == "CI" & origin == "hatchery" & `Fish Age` == "0+",
        "CI2",
        ifelse(
          species == "CI" & origin == "hatchery" & `Fish Age` == "1+",
          "CI1",
          ifelse(
            species == "CO" & origin == "hatchery" & `Fish Age` == "1+",
            "CO1",
            species
          )
        )
      )
    ) %>%
    dplyr::select(
      -year,
      -origin,
      -location,
      -`Fish Age`,
      -`Mark Type (see protocol)`
    ) %>%
    tidyr::gather(
      key = variable,
      value = value,
      -index,
      -species,
      -sum_tot,
      -predator_stage
    ) %>%
    dplyr::arrange(index, variable)

  # Create final diet matrix
  diet.cvs.final <- diet.salmon.agg %>%
    dplyr::mutate(proportion = value / sum_tot) %>%
    dplyr::rename(atlantis_name = variable) %>%
    dplyr::left_join(atlantis_fg, by = "atlantis_name") %>%
    dplyr::rename(
      atlantis_prey_group = atlantis_fg,
      prey_taxa = atlantis_name
    ) %>%
    dplyr::mutate(source = "Beauchamp_Puget_Sound", data = index) %>%
    dplyr::rename(atlantis_fg = species) %>%
    dplyr::left_join(atlantis_fg, by = "atlantis_fg") %>%
    dplyr::rename(
      atlantis_predator_group = atlantis_fg,
      predator = atlantis_name
    ) %>%
    dplyr::select(
      source,
      atlantis_predator_group,
      predator_stage,
      predator,
      atlantis_prey_group,
      prey_taxa,
      data,
      value,
      proportion,
      index
    ) %>%
    dplyr::rename(
      atlantis_pred_group = atlantis_predator_group,
      prey = prey_taxa
    ) %>%
    dplyr::mutate(source = source.name)

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
