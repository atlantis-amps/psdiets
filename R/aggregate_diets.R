#' Aggregate diet data for Atlantis ecosystem model
#'
#' Main function to process diet composition data for various functional groups
#' and create aggregated prey categories from detailed species-level diet information.
#' Distributes generic prey categories, normalizes proportions, analyzes insect subsidies,
#' and prepares data for Dirichlet distribution analysis.
#'
#' @param func.groups Data frame. Atlantis functional groups with columns:
#'   OldCode, Code, Name, Long Name.
#' @param un.codes Character vector. Codes to exclude (primary producers,
#'   unassigned groups).
#' @param orca.chinook Data frame. Resident orca Chinook salmon prey proportions.
#' @param fish.groups Data frame. Fish functional groups with atlantis_group,
#'   name, and biomass_mt columns.
#' @param atlantis.diet.groups Data frame. Mapping between diet categories
#'   and Atlantis groups.
#' @param invert.groups Data frame. Invertebrate functional groups.
#' @param agg.groups Character vector. Aggregated group codes to exclude
#'   from prey lists.
#' @param herr.groups Character vector. Herring functional group codes.
#' @param chinook.sal Character vector. Chinook salmon functional group codes.
#' @param chin.coho.sal Character vector. Combined Chinook/Coho codes.
#' @param chum.sal Character vector. Chum salmon groups
#' @param coho.sal Character vector. Coho salmon groups
#' @param sal.groups Character vector. All salmon functional group codes.
#' @param groups.notinmodel Character vector. Groups to exclude from analysis.
#' @param hatchery.sal Character vector. Hatchery salmon codes.
#' @param rockfish.list Character vector. Rockfish functional group codes.
#' @param bird.groups Character vector. Seabird functional group codes.
#' @param invert.codes Character vector. Invertebrate functional group codes.
#' @param bacterial.groups Character vector. Bacterial group codes.
#' @param detritus.groups Character vector. Detritus group codes.
#'
#' @return NULL. Creates multiple output files:
#' \itemize{
#'   \item ps_mean_diet_composition.csv: Mean diet by predator and life stage
#'   \item insect_subsidies.csv: Insect proportions in salmon diets
#'   \item diet_composition_{stage}.csv: Wide-format data for Dirichlet analysis
#'   \item raw_diet_prop_{stage}.csv: Raw diet proportions by life stage
#'   \item {stage}_prey_matrix.csv: Binary prey presence/absence matrices
#' }
#'
#' @details
#' This comprehensive function performs the following workflow:
#' \enumerate{
#'   \item Creates biomass-weighted aggregations for fish, salmon, birds, detritus
#'   \item Extracts and saves individual diet sheets from Excel database
#'   \item Reads and combines all diet matrix files
#'   \item Quality checks for missing predator/prey groups
#'   \item Distributes generic "SALMON" prey to specific salmon species
#'   \item Normalizes diet proportions
#'   \item Analyzes insect subsidies in salmon diets (creates boxplot)
#'   \item Removes insects (terrestrial subsidy not in model)
#'   \item Calculates mean diet composition across observations
#'   \item Prepares separate datasets for juvenile, adult, and invertebrate analyses
#'   \item Exports data in formats suitable for Dirichlet distribution fitting
#' }
#'
#' The function uses global objects created by earlier processing steps and
#' calls helper functions: assign_salmon, organize_fish, organize_fish.pred,
#' save_excel_sheet, read_file_sheet.
#'
#' @importFrom dplyr filter select rename mutate left_join group_by summarise ungroup bind_rows arrange distinct if_else
#' @importFrom tidyr pivot_longer pivot_wider spread gather
#' @importFrom tibble tibble
#' @importFrom readr read_csv write_csv
#' @importFrom ggplot2 ggplot aes geom_boxplot stat_summary theme element_text
#' @importFrom here here
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage output is saved file
#' aggregate_diet(
#' func.groups,
#' un.codes,
#' orca.chinook,
#' fish.groups,
#' atlantis.diet.groups,
#' invert.groups,
#' agg.groups,
#' herr.groups,
#' chinook.sal,
#' chin.coho.sal,
#' chum.sal,
#' coho.sal,
#' sal.groups,
#' groups.notinmodel,
#' hatchery.sal,
#' rockfish.list,
#' bird.groups,
#' invert.codes,
#' bacterial.groups,
#' detritus.groups
#' )
#'
aggregate_diets <- function(
  func.groups,
  un.codes,
  orca.chinook,
  fish.groups,
  atlantis.diet.groups,
  invert.groups,
  agg.groups,
  herr.groups,
  chinook.sal,
  chin.coho.sal,
  chum.sal,
  coho.sal,
  sal.groups,
  groups.notinmodel,
  hatchery.sal,
  rockfish.list,
  bird.groups,
  invert.codes,
  bacterial.groups,
  detritus.groups
) {
  # Prepare functional groups data by renaming and selecting relevant columns
  func.groups <- func.groups %>%
    dplyr::rename(atlantis_pred_group = OldCode) %>%
    dplyr::select(atlantis_pred_group, Code, Name, `Long Name`)

  # Extract consumer codes (predators) by filtering out primary producers and unassigned codes
  consumer.codes <- func.groups %>%
    dplyr::distinct(Code) %>%
    #eliminate primary producers and codes that were unassigned
    dplyr::filter(!Code %in% un.codes)

  # Create list of all valid prey group codes, excluding aggregated groups
  allgroup.codes <- func.groups %>%
    dplyr::distinct(Code) %>%
    dplyr::filter(!Code %in% agg.groups) %>%
    dplyr::rename(atlantis_prey_group = Code)

  # Extract all invertebrate group names
  all.invert.groups <<- invert.groups %>%
    dplyr::distinct(atlantis_group) %>%
    .$atlantis_group

  # Extract all fish group codes as a character vector for read_file_sheet()
  all.fish.groups <<- fish.groups %>%
    dplyr::distinct(atlantis_group) %>%
    .$atlantis_group

  # --- Create aggregated fish groups with biomass-weighted proportions ---

  # Aggregate herring groups into single "HE" prey category
  herring.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% herr.groups) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "HE"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Aggregate Chinook salmon groups into "CI" prey category
  chinook.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% chinook.sal) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "CI"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Aggregate hatchery Chinook (yearling and subyearling) into "CH" prey category
  chinook.hatchery.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% c("CHY", "CHS")) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "CH"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Aggregate Coho salmon groups (hatchery, other, deep south, yearling) into "CO" prey category
  coho.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% coho.sal) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "CO"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Combined Chinook and Coho category for certain diet analyses
  chinook.coho.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% chin.coho.sal) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "CI_CO"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Aggregate Chum salmon (subadult, fall, hatchery) into "CU" prey category
  chum.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% chum.sal) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "CU"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Aggregate all salmon species into general "SALMON" prey category
  salmon.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% sal.groups) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "SALMON"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # Extract list of all salmon group codes
  all.salmon <- salmon.groups %>%
    dplyr::distinct(atlantis_group) %>%
    .$atlantis_group

  # Separate hatchery salmon from wild salmon for specific diet analyses
  hatchery.salmon.groups <<- salmon.groups %>%
    dplyr::filter(atlantis_group %in% hatchery.sal) %>%
    dplyr::mutate(atlantis_prey_group = "HATCHERY_SALMON")

  # Wild salmon category (excludes hatchery salmon)
  # Note: Pink salmon (PIS) not included here because this follows Harvey et al. 2010
  # data structure for raptors, which considers pink salmon separately
  wild.salmon.groups <<- salmon.groups %>%
    dplyr::filter(!atlantis_group %in% hatchery.sal) %>%
    #filter(atlantis_group %in% c("CSY","CSS","CSN","CDS","CNY","CNS","CHC","CYE","CKS","COS","COD","COY","CMF","CMH","SAL","SAF")) %>%
    dplyr::mutate(atlantis_prey_group = "WILD_SALMON")

  # Aggregate all rockfish species into "ROCKFISH" prey category
  rockfish.groups <<- fish.groups %>%
    dplyr::filter(atlantis_group %in% rockfish.list) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum,
      atlantis_prey_group = "ROCKFISH"
    ) %>%
    dplyr::select(-name, -biomass_mt, -tot_sum)

  # --- Create aggregated groups for birds, detritus, and bacteria ---

  # Aggregate seabird groups (piscivorous and non-piscivorous) as predators
  # Biomass values used to weight the proportion each bird group contributes
  bird.groups.pred <<- tibble::tibble(
    atlantis_group = bird.groups,
    biomass_mt = c(99.94683333, 72.99475),
    atlantis_pred_group = "BIRD"
  ) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum
    ) %>%
    dplyr::select(-biomass_mt, -tot_sum)

  # Create bird groups as prey category (for cases where birds are consumed)
  bird.groups.prey <<- bird.groups.pred %>%
    dplyr::rename(atlantis_prey_group = atlantis_pred_group)

  # Aggregate detritus groups (carrion, labile, refractory) into "DETRITUS" prey category
  detritus.prey <<- tibble::tibble(
    atlantis_group = detritus.groups,
    biomass_mt = c(1313, 2626, 2626),
    atlantis_prey_group = "DETRITUS"
  ) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum
    ) %>%
    dplyr::select(-biomass_mt, -tot_sum)

  # Aggregate bacterial groups (pelagic and benthic) into "BB" prey category
  bacteria.prey <<- tibble::tibble(
    atlantis_group = bacterial.groups,
    biomass_mt = c(4203673, 706756),
    atlantis_prey_group = "BB"
  ) %>%
    dplyr::mutate(
      tot_sum = sum(biomass_mt),
      fish_prop = biomass_mt / tot_sum
    ) %>%
    dplyr::select(-biomass_mt, -tot_sum)

  # Convert invertebrate codes to full names
  invert.codes <- tibble::tibble(Code = invert.codes) %>%
    dplyr::left_join(func.groups, by = "Code") %>%
    dplyr::select(`Long Name`) %>%
    .$`Long Name`

  # Create lookup table for atlantis functional groups (Code to Long Name)
  atlantis.fg <- func.groups %>%
    dplyr::select(-atlantis_pred_group) %>%
    dplyr::distinct(Code, `Long Name`)

  # Save sheets from xlsx as individual csv files
  save_excel_sheet("Diet_database_V1_no_figures.xlsx")

  # --- Load and combine all diet matrix files ---
  # Read all diet matrix files from the outputs directory
  diet.matrix.list <- list.files(
    here::here("outputs", "diet_data_files"),
    pattern = "diet_data*.*"
  )

  # Create index for looping through files
  diet.files <- 1:length(diet.matrix.list)

  # Read all diet matrix files and combine into single data frame
  diet.data <- lapply(diet.files, read_file_sheet, diet.matrix.list) %>%
    bind_rows()

  readr::write_csv(
    diet.data,
    here::here("outputs", "combined_raw_diet_data_AMPS.csv")
  )

  # --- Quality checks on diet data ---

  # Check that all expected predator groups are present in the diet data
  diet.pred.groups <- diet.data %>%
    dplyr::distinct(atlantis_pred_group) %>%
    .$atlantis_pred_group

  # Identify any consumer codes missing from diet data
  pred.diet.data <- consumer.codes %>%
    dplyr::filter(!Code %in% diet.pred.groups)

  # Check that all prey groups are represented in the diet data
  diet.prey.groups <- diet.data %>%
    dplyr::distinct(atlantis_prey_group) %>%
    .$atlantis_prey_group

  # Identify any prey groups not consumed by any predator
  prey.diet.data <- allgroup.codes %>%
    dplyr::filter(!atlantis_prey_group %in% diet.prey.groups)

  # Note: Aggregated groups that need to be distributed to constituent species:
  # CI (Chinook), CO (Coho), HE (Herring), CU (Chum), CI1_CI2 (Chinook/Coho combined),
  # CH (hatchery Chinook), ROCKFISH, BIRD

  # --- Process diet data to distribute aggregated prey categories ---
  # Fish prey categories will be allocated to specific identified fish groups
  # This section also estimates insect proportions in diets

  # Create index to uniquely identify each diet observation (combination of source, data, predator)
  diet.data.set <- diet.data %>%
    dplyr::group_by(source, data, atlantis_pred_group) %>%
    mutate(index_cont = dplyr::row_number())

  # --- Handle salmon prey assignments ---
  # Extract diet observations where prey is listed as general "SALMON" category
  # These will be distributed to specific salmon species based on proportions

  diet.data.salmon <<- diet.data.set %>%
    dplyr::filter(atlantis_prey_group == "SALMON")

  # Calculate mean proportions of specific salmon species in predator diets
  # This provides the basis for distributing generic "SALMON" prey to specific species
  salmon.prop.fg <<- diet.data.set %>%
    dplyr::filter(atlantis_prey_group %in% all.salmon) %>%
    dplyr::group_by(atlantis_pred_group, atlantis_prey_group) %>%
    dplyr::summarise(mean_prop = mean(proportion))

  # Get list of predators that consumed generic "SALMON" prey
  salmon.prop.pred <- diet.data.salmon %>%
    dplyr::distinct(atlantis_pred_group) %>%
    .$atlantis_pred_group

  # Get list of predators with specific salmon species diet data
  salmon.prop.group <<- salmon.prop.fg %>%
    dplyr::distinct(atlantis_pred_group) %>%
    .$atlantis_pred_group

  # Assign generic "SALMON" prey to specific salmon species for each predator
  # Uses assign_salmon function (defined elsewhere) applied to each predator
  salmon.indexdata <- lapply(salmon.prop.pred, assign_salmon) %>%
    bind_rows

  # Replace generic "SALMON" entries with species-specific salmon entries
  # Recalculate indices after this substitution
  diet.data.corrected.salmon <- diet.data.set %>%
    dplyr::filter(atlantis_prey_group != "SALMON") %>%
    dplyr::bind_rows(salmon.indexdata) %>%
    dplyr::group_by(source, data, atlantis_pred_group) %>%
    dplyr::mutate(index_cont = dplyr::row_number())

  # --- Sum duplicate entries and normalize diet proportions ---

  # Aggregate diet proportions for duplicate predator-prey combinations
  # (can occur after distributing generic prey categories to specific species)
  diet.correct.prop <- diet.data.corrected.salmon %>%
    #get_dupes(index_cont, atlantis_pred_group,predator_stage, atlantis_prey_group, source, data) %>%
    dplyr::group_by(
      index_cont,
      atlantis_pred_group,
      predator_stage,
      atlantis_prey_group,
      source,
      data
    ) %>%
    dplyr::summarise(proportion = sum(proportion))

  # Normalize diet proportions so they sum to 1 for each diet observation
  # Also correct predator life stage classifications for invertebrates
  diet.correct.norm <- diet.correct.prop %>%
    dplyr::group_by(index_cont) %>%
    dplyr::summarise(tot_prop = sum(proportion)) %>%
    dplyr::left_join(diet.correct.prop, by = c("index_cont")) %>%
    dplyr::mutate(new_prop = proportion / tot_prop) %>%
    dplyr::select(-proportion, -tot_prop) %>%
    dplyr::rename(proportion = new_prop) %>%
    # Standardize life stage for dungeness crab and geoducks
    dplyr::mutate(
      predator_stage = dplyr::if_else(
        atlantis_pred_group == "DUN",
        "INVERT",
        dplyr::if_else(atlantis_pred_group == "GEC", "INVERT", predator_stage)
      )
    ) %>%
    # Set all invertebrate functional groups to "INVERT" life stage
    dplyr::mutate(
      predator_stage = dplyr::if_else(
        atlantis_pred_group %in% invert.codes,
        "INVERT",
        predator_stage
      )
    )

  # --- Analyze insect subsidies in salmon diets ---
  # Insects are an important terrestrial prey subsidy for juvenile salmon in nearshore areas

  # Prepare salmon diet data for visualization
  # Convert codes to full names and calculate mean prey proportions
  diet.salmon.plot <- diet.correct.norm %>%
    dplyr::filter(atlantis_pred_group %in% all.salmon) %>%
    dplyr::rename(Code = atlantis_pred_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::rename(Predator = `Long Name`) %>%
    dplyr::select(-Code) %>%
    dplyr::rename(Code = atlantis_prey_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::rename(prey = `Long Name`) %>%
    # Insects have no Code in atlantis.fg, so label NA prey as "Insects"
    dplyr::mutate(prey = dplyr::if_else(is.na(prey), "Insects", prey)) %>%
    dplyr::group_by(Predator, prey) %>%
    dplyr::summarise(tot_sum_prey = mean(proportion))

  # Calculate total diet proportion for each salmon predator (for normalization)
  diet.correct.plot.tot <- diet.salmon.plot %>%
    dplyr::group_by(Predator) %>%
    dplyr::summarise(tot_sum = sum(tot_sum_prey))

  # Define order for salmon predators in plots (organized by species and run timing)
  pred.order <- c(
    "Chinook Duwamish Subyearling",
    "Chinook Hatch Subyearling",
    "Chinook Hatch Yearling",
    "Chinook Hood Canal Subyearling",
    "Chinook Nisqually Subyearling",
    "Chinook Nisqually Yearling",
    "Chinook other Subyearling",
    "Chinook other Yearling",
    "Chinook Skagit Subyearling",
    "Chinook Skagit Yearling",
    "Chinook Snohomish Subyearling",
    "Chum Fall Subyearling",
    "Chum Hatch Subyearling",
    "Chum Hood Canal summer run Subyearling",
    "Coho Deep South Yearling",
    "Coho Hatch Yearling",
    "Coho other Yearling",
    "Coho Skagit Yearling",
    "Other salmonids",
    "Pink Salmon Subyearling",
    "Strait of Georgia salmonids"
  )

  # Extract and normalize insect proportions in salmon diets
  diet.insect.salmon <- diet.salmon.plot %>%
    dplyr::left_join(diet.correct.plot.tot, by = "Predator") %>%
    dplyr::mutate(proportion = tot_sum_prey / tot_sum) %>%
    dplyr::filter(prey == "Insects")

  # Apply predator ordering for consistent plot display
  diet.insect.salmon$Predator <- factor(
    diet.insect.salmon$Predator,
    levels = pred.order
  )

  # Create boxplot showing insect subsidy proportions across salmon groups
  # Diamond indicates mean, notches indicate 95% CI around median
  diet.insect.salmon %>%
    ggplot2::ggplot(ggplot2::aes(x = Predator, y = proportion)) +
    ggplot2::geom_boxplot(notch = TRUE) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

  # Export insect subsidy data to CSV
  diet.insect.salmon %>%
    dplyr::select(-tot_sum_prey, -tot_sum) %>%
    readr::write_csv(here::here("outputs", "insect_subsidies.csv"))

  # Recreate functional group lookup (redundant but ensures it's available)
  atlantis.fg <- func.groups %>%
    dplyr::select(-atlantis_pred_group) %>%
    dplyr::distinct(Code, `Long Name`)

  # --- Create mean diet composition tables (excluding insects) ---
  # Insects are excluded because they are not part of the Atlantis model
  # (terrestrial subsidy not modeled explicitly)

  # Calculate total proportion for each diet observation after removing insects
  # This is needed for re-normalizing proportions
  diet.correct.norm.noinsects.prey <- diet.correct.norm %>%
    dplyr::filter(atlantis_prey_group != "INSECT") %>%
    dplyr::group_by(index_cont) %>%
    dplyr::summarise(tot_prop = sum(proportion)) %>%
    dplyr::arrange(index_cont)

  # Remove insects and renormalize diet proportions
  # Convert atlantis codes to full descriptive names for reporting
  diet.correct.norm.noinsects <- diet.correct.norm %>%
    dplyr::filter(atlantis_prey_group != "INSECT") %>%
    dplyr::group_by(
      index_cont,
      atlantis_pred_group,
      predator_stage,
      atlantis_prey_group
    ) %>%
    dplyr::summarise(prey_prop = sum(proportion)) %>%
    dplyr::left_join(diet.correct.norm.noinsects.prey, by = c("index_cont")) %>%
    dplyr::mutate(proportion = prey_prop / tot_prop) %>%
    dplyr::select(-prey_prop, -tot_prop) %>%
    # Convert predator codes to names
    dplyr::rename(Code = atlantis_pred_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::ungroup() %>%
    dplyr::select(-Code) %>%
    dplyr::rename(Predator = `Long Name`) %>%
    # Convert prey codes to names
    dplyr::rename(Code = atlantis_prey_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::select(-Code) %>%
    dplyr::rename(prey = `Long Name`)

  # --- Calculate mean diet composition across all diet observations ---
  # Group "juvenileAndadult" with "adult" stage for consistency
  mean.diet.comp <- diet.correct.norm.noinsects %>%
    dplyr::mutate(
      predator_stage = dplyr::if_else(
        predator_stage == "juvenileAndadult",
        "adult",
        predator_stage
      )
    ) %>%
    dplyr::group_by(predator_stage, Predator, prey) %>%
    dplyr::summarise(mean_diet_composition = mean(proportion))

  # Count number of diet observations for each predator-stage combination
  organized.data.count <- diet.correct.norm.noinsects %>%
    dplyr::distinct(predator_stage, Predator, index_cont) %>%
    dplyr::group_by(predator_stage, Predator) %>%
    summarise(tot_diet_composition = length(index_cont))

  # Create wide-format diet matrix with normalized proportions
  # Each row is a predator life stage, columns are prey items
  organized.data.sum <- mean.diet.comp %>%
    #mutate(predator_stage=if_else(predator_stage=="juvenileAndadult","adult",predator_stage)) %>%
    dplyr::group_by(predator_stage, Predator) %>%
    dplyr::summarise(tot_diet_composition = sum(mean_diet_composition)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(mean.diet.comp, by = c("predator_stage", "Predator")) %>%
    dplyr::mutate(
      norm.diet.comp = mean_diet_composition / tot_diet_composition
    ) %>%
    dplyr::select(-mean_diet_composition, -tot_diet_composition) %>%
    dplyr::left_join(
      organized.data.count,
      by = c("Predator", "predator_stage")
    ) %>%
    tidyr::pivot_wider(
      names_from = prey,
      values_from = norm.diet.comp,
      values_fill = 0
    ) %>%
    dplyr::mutate(tot_sum = round(rowSums(.[4:ncol(.)]), 1))

  # Export mean diet composition to CSV
  readr::write_csv(
    organized.data.sum,
    here::here("outputs", "ps_mean_diet_composition.csv")
  )

  # --- Process invertebrate diet data ---
  # Extract diet data for invertebrate predators
  invert.data <- diet.correct.norm.noinsects %>%
    dplyr::filter(predator_stage == "INVERT") %>%
    dplyr::select(index_cont, Predator, prey, proportion)

  # Export raw invertebrate diet proportions
  readr::write_csv(
    invert.data,
    here::here("outputs", "raw_diet_prop_invert.csv")
  )

  # Create binary prey matrix (1 = prey consumed, 0 = not consumed) for invertebrates
  invert.data %>%
    dplyr::distinct(Predator, prey) %>%
    dplyr::mutate(value = 1) %>%
    tidyr::pivot_wider(
      names_from = prey,
      values_from = value,
      values_fill = 0
    ) %>%
    readr::write_csv(here::here("invert_prey_matrix.csv"))

  # Create wide-format diet composition for Dirichlet distribution analysis
  # Format: predator | weight | prey1 | prey2 | ... | preyN
  invert.data %>%
    tidyr::pivot_wider(
      names_from = prey,
      values_from = proportion,
      values_fill = 0
    ) %>%
    dplyr::arrange(Predator) %>%
    dplyr::select(-index_cont) %>%
    dplyr::mutate(wgt = 1) %>%
    dplyr::select(Predator, wgt, everything()) %>%
    dplyr::rename(predator = Predator) %>%
    readr::write_csv(here::here(
      "outputs",
      "dirichelet",
      "diet_composition_invert.csv"
    ))

  # Calculate total diet proportion for each invertebrate (for normalization in plots)
  invert.tot.prop <- invert.data %>%
    dplyr::group_by(Predator, prey) %>%
    dplyr::summarise(mean_prop = mean(proportion)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Predator) %>%
    dplyr::summarise(tot_prop = sum(mean_prop))

  # --- Process juvenile fish diet data ---
  # "Juvenile" category includes both "juvenile" and "juvenileAndadult" life stages

  # Calculate mean prey proportions for juvenile predators (includes insects)
  juvenile.data.mean <- diet.correct.norm %>%
    dplyr::filter(
      predator_stage == "juvenile" | predator_stage == "juvenileAndadult"
    ) %>%
    dplyr::rename(Code = atlantis_pred_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::rename(Predator = `Long Name`) %>%
    dplyr::select(-Code) %>%
    dplyr::rename(Code = atlantis_prey_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::rename(prey = `Long Name`) %>%
    dplyr::mutate(prey = dplyr::if_else(is.na(prey), "Insects", prey)) %>%
    dplyr::group_by(Predator, prey) %>%
    dplyr::summarise(tot_sum_prey = mean(proportion))

  # Normalize juvenile diet proportions to sum to 1
  juvenile.data <- juvenile.data.mean %>%
    dplyr::group_by(Predator) %>%
    dplyr::summarise(tot_sum = sum(tot_sum_prey)) %>%
    dplyr::left_join(juvenile.data.mean, by = c("Predator")) %>%
    dplyr::mutate(proportion = tot_sum_prey / tot_sum) %>%
    dplyr::select(-tot_sum, -tot_sum_prey)

  # Export raw juvenile diet proportions (no insects)
  diet.correct.norm.noinsects %>%
    dplyr::filter(
      predator_stage == "juvenile" | predator_stage == "juvenileAndadult"
    ) %>%
    readr::write_csv(here::here(
      "outputs",
      "dirichelet",
      "raw_diet_prop_juvenile.csv"
    ))

  # Create binary prey matrix for juveniles
  diet.correct.norm.noinsects %>%
    dplyr::filter(
      predator_stage == "juvenile" | predator_stage == "juvenileAndadult"
    ) %>%
    dplyr::distinct(Predator, prey) %>%
    dplyr::mutate(value = 1) %>%
    spread(prey, value, fill = 0) %>%
    readr::write_csv(here::here("outputs", "juvenile_prey_matrix.csv"))

  # Create wide-format diet composition for Dirichlet distribution analysis
  diet.correct.norm.noinsects %>%
    dplyr::filter(
      predator_stage == "juvenile" | predator_stage == "juvenileAndadult"
    ) %>%
    tidyr::pivot_wider(
      names_from = prey,
      values_from = proportion,
      values_fill = 0
    ) %>%
    dplyr::arrange(Predator) %>%
    dplyr::select(-index_cont, -predator_stage) %>%
    dplyr::mutate(wgt = 1) %>%
    dplyr::select(Predator, wgt, everything()) %>%
    dplyr::rename(predator = Predator) %>%
    readr::write_csv(here::here(
      "outputs",
      "dirichelet",
      "diet_composition_juvenile.csv"
    ))

  # --- Process adult fish diet data ---
  # "Adult" category includes both "adult" and "juvenileAndadult" life stages

  # Calculate mean prey proportions for adult predators (includes insects)
  adult.data.mean <- diet.correct.norm %>%
    dplyr::filter(
      predator_stage == "adult" | predator_stage == "juvenileAndadult"
    ) %>%
    dplyr::rename(Code = atlantis_pred_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::rename(Predator = `Long Name`) %>%
    dplyr::select(-Code) %>%
    dplyr::rename(Code = atlantis_prey_group) %>%
    dplyr::left_join(atlantis.fg, by = "Code") %>%
    dplyr::rename(prey = `Long Name`) %>%
    dplyr::mutate(prey = dplyr::if_else(is.na(prey), "Insects", prey)) %>%
    dplyr::group_by(Predator, prey) %>%
    dplyr::summarise(tot_sum_prey = mean(proportion))

  # Normalize adult diet proportions to sum to 1
  adult.data <- adult.data.mean %>%
    dplyr::group_by(Predator) %>%
    dplyr::summarise(tot_sum = sum(tot_sum_prey)) %>%
    dplyr::left_join(adult.data.mean, by = c("Predator")) %>%
    dplyr::mutate(proportion = tot_sum_prey / tot_sum) %>%
    dplyr::select(-tot_sum, -tot_sum_prey) %>%
    droplevels()

  # Export raw adult diet proportions (no insects)
  diet.correct.norm.noinsects %>%
    dplyr::filter(
      predator_stage == "adult" | predator_stage == "adultAndadult"
    ) %>%
    readr::write_csv(here::here(
      "outputs",
      "dirichelet",
      "raw_diet_prop_adult.csv"
    ))

  # Create binary prey matrix for adults
  diet.correct.norm.noinsects %>%
    dplyr::filter(
      predator_stage == "adult" | predator_stage == "adultAndadult"
    ) %>%
    dplyr::distinct(Predator, prey) %>%
    dplyr::mutate(value = 1) %>%
    tidyr::pivot_wider(
      names_from = prey,
      values_from = value,
      values_fill = 0
    ) %>%
    readr::write_csv(here::here("outputs", "adult_prey_matrix.csv"))

  # Create wide-format diet composition for Dirichlet distribution analysis
  diet.correct.norm.noinsects %>%
    dplyr::filter(
      predator_stage == "adult" | predator_stage == "adultAndadult"
    ) %>%
    tidyr::pivot_wider(
      names_from = prey,
      values_from = proportion,
      values_fill = 0
    ) %>%
    dplyr::arrange(Predator) %>%
    dplyr::select(-index_cont, -predator_stage) %>%
    dplyr::mutate(wgt = 1) %>%
    dplyr::select(Predator, wgt, everything()) %>%
    dplyr::rename(predator = Predator) %>%
    readr::write_csv(here::here(
      "outputs",
      "dirichelet",
      "diet_composition_adult.csv"
    ))

  # End of aggregate_diet function
}
