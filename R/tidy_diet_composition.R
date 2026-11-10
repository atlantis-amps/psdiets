#' @title Tidy and extend diet composition data with Atlantis codes
#'
#' @description
#' Converts wide diet composition data to tidy format, adds Atlantis functional
#' group codes, and includes new salmon resident groups that inherit diet from
#' specified donor groups.
#'
#' @param diet.matrix Character. Path to ps_mean_diet_composition.csv
#' @param func.groups Character. Path to atlantis_func_groups.csv with existing codes
#' @param salmon.table Character. Path to res_salmon_table.csv with new groups
#'
#' @return A tibble with columns:
#'   - predator_stage: life stage (INVERT, adult, juvenile)
#'   - predator: original predator name
#'   - func.groups: Atlantis functional group code
#'   - atlantis_name: Atlantis functional group full name
#'   - prey: prey item name
#'   - diet_value: diet proportion
#'
#' @importFrom readr read_csv
#' @importFrom dplyr left_join rename select filter mutate bind_rows all_of
#' @importFrom tidyr pivot_longer
#'
#' @export

tidy_diet_composition <- function(
    diet.matrix,
    func.groups,
    salmon.table){


   diet_wide <- readr::read_csv(diet.matrix, show_col_types = FALSE)
  func_groups <- readr::read_csv(func.groups, show_col_types = FALSE) %>%
  dplyr::select(atlantis_fg = Code, atlantis_name = `Long Name`)
  salmon_table <- readr::read_csv(salmon.table, show_col_types = FALSE) %>%
    dplyr::rename(atlantis_pred_fg = code)

#make frame of expected stages for age structured groups (juvenile and adult) to compare with diet matrix

  unique.age.juv <- readr::read_csv(func.groups, show_col_types = FALSE) %>%
    dplyr::filter(GroupType %in% c("MAMMAL","BIRD","SHARK","FISH")) %>%
    dplyr::select(atlantis_pred_fg = Code, pred = `Long Name`) %>%
    dplyr::mutate(pred = tolower(pred), predator_stage = "juvenile")

  unique.age.ad <- readr::read_csv(func.groups, show_col_types = FALSE) %>%
    dplyr::filter(GroupType %in% c("MAMMAL","BIRD","SHARK","FISH")) %>%
    dplyr::select(atlantis_pred_fg = Code, pred = `Long Name`) %>%
    dplyr::mutate(pred = tolower(pred), predator_stage = "adult")

unique.stages <- dplyr::bind_rows(unique.age.juv, unique.age.ad) %>%
  dplyr::arrange(pred, predator_stage) %>%
  dplyr::mutate(pred=trimws(pred), predator_stage=trimws(predator_stage))

  prey_cols <- colnames(diet_wide)[-c(1, 2, 3, ncol(diet_wide))]

  diet_tidy <- diet_wide %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(prey_cols),
      names_to = "prey",
      values_to = "diet_value"
    ) %>%
    dplyr::filter(diet_value > 0) %>%
    dplyr::mutate(pred=tolower(Predator)) %>%
    dplyr::mutate(prey=tolower(prey)) %>%
    dplyr::select(-Predator)

  fg_prey <- func_groups %>%
    dplyr::select(atlantis_fg, atlantis_name) %>%
    dplyr::rename(atlantis_prey_fg = atlantis_fg, prey = atlantis_name) %>%
    dplyr::mutate(prey=tolower(prey))

  fg_predator <- func_groups %>%
    dplyr::select(atlantis_fg, atlantis_name) %>%
    dplyr::rename(atlantis_pred_fg = atlantis_fg, pred = atlantis_name) %>%
    dplyr::mutate(pred=tolower(pred))

  diet.tidy.fg <- diet_tidy %>%
    dplyr::left_join(fg_prey, by = "prey") %>%
    dplyr::left_join(fg_predator, by = "pred") %>%
    dplyr::select(pred, atlantis_pred_fg, predator_stage, prey, atlantis_prey_fg, diet_value) %>%
    dplyr::group_by(pred, atlantis_pred_fg, predator_stage) %>%
  dplyr::mutate(tot_prey = dplyr::n()) %>%
    dplyr::ungroup()

  #create table for new salmon resident groups predators
  new_groups_diet <- salmon_table %>%
    dplyr::left_join(diet.tidy.fg, by = c("atlantis_pred_fg")) %>%
    dplyr::select(-atlantis_pred_fg) %>%
    dplyr::rename(atlantis_pred_fg = atlantis_fg) %>%
    dplyr::mutate(pred=tolower(long_name)) %>%
    dplyr::select(pred, atlantis_pred_fg, predator_stage, prey, atlantis_prey_fg, diet_value, tot_prey)


  # test <- diet.tidy.fg %>%
  #   dplyr::filter(atlantis_pred_fg=="CHC") %>%
  #   dplyr::left_join(salmon_table, by=("atlantis_pred_fg"))   %>%
  #                      dplyr::select(-atlantis_pred_fg) %>%
  #                      dplyr::rename(atlantis_pred_fg = atlantis_fg) %>%
  #                      dplyr::mutate(pred=tolower(long_name)) %>%
  #                      dplyr::select(pred, atlantis_pred_fg, predator_stage, prey, atlantis_prey_fg, diet_value, tot_prey)



  #create table for new salmon resident groups as prey

  new_groups_prey <- salmon_table %>%
    dplyr::rename(atlantis_prey_fg = atlantis_pred_fg) %>%
    dplyr::right_join(diet.tidy.fg, by="atlantis_prey_fg") %>%
    dplyr::filter(!is.na(atlantis_name)) %>%
    dplyr::select(-atlantis_prey_fg, -prey, -atlantis_name, -follows_distribution, -long_name_base) %>%
    dplyr::rename(atlantis_prey_fg = atlantis_fg, prey=long_name) %>%
    dplyr::mutate(prey=tolower(prey))



  #create table for salmon prey of new salmon resident groups predators
   new_groups_salmon_prey <- salmon_table %>%
     dplyr::rename(atlantis_prey_fg = atlantis_pred_fg) %>%
     dplyr::right_join(new_groups_diet, by="atlantis_prey_fg") %>%
     dplyr::filter(!is.na(atlantis_name)) %>%
     dplyr::select(-prey, -atlantis_prey_fg, -atlantis_name, -follows_distribution, -long_name_base) %>%
     dplyr::rename(prey=long_name, atlantis_prey_fg=atlantis_fg) %>%
     dplyr::mutate(prey=tolower(prey))



  #combine all diet data

  combined.salmon.data <- dplyr::bind_rows(new_groups_prey, new_groups_diet, new_groups_salmon_prey)

  #check for duplicates
  combined.salmon.data %>%
    dplyr::group_by_all() %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup()

  #combine all diet data
  combined.data <- dplyr::bind_rows(diet.tidy.fg, combined.salmon.data) %>%
    dplyr::mutate(pred=trimws(pred), predator_stage=trimws(predator_stage))

 #test for groups missing diet, these groups shoudl have no diet
  # 1 BB          Benthic bacteria
  # 2 PB          Pelagic bacteria
  # 3 PL          Large phytoplankton
  # 4 PS          Small phytoplankton
  # 5 MA          Macroalgae
  # 6 SG          Seagrass
  # 7 DL          Labile detritus
  # 8 DR          Refractory detritus
  # 9 DC          Carrion

  func_groups_missing_diet <- func_groups %>%
    dplyr::filter(!atlantis_fg %in% combined.data$atlantis_pred_fg)

#test for missing age classes

  unique.stages %>% dplyr::left_join(combined.data, by = c("pred", "predator_stage","atlantis_pred_fg")) %>%
    dplyr::filter(is.na(diet_value)) %>%
    print()


  missing.groups <- unique.stages %>% dplyr::left_join(combined.data, by = c("pred", "predator_stage","atlantis_pred_fg")) %>%
    dplyr::filter(is.na(diet_value)) %>%
    dplyr::select(pred, atlantis_pred_fg, predator_stage)

  missing.age.class <- combined.data %>%
    dplyr::ungroup() %>%
    dplyr::filter(atlantis_pred_fg %in% missing.groups$atlantis_pred_fg) %>%
    dplyr::select(-predator_stage) %>%
    dplyr::right_join(missing.groups, by = c("pred","atlantis_pred_fg"))

  #combine data

 corrected.data <-  dplyr::bind_rows(combined.data, missing.age.class) %>%
    dplyr::arrange(pred, predator_stage, prey) %>%
   dplyr::ungroup()

 #double-check no groups are missing age stage data
 unique.stages %>% dplyr::left_join(corrected.data, by = c("pred", "predator_stage","atlantis_pred_fg")) %>%
   dplyr::filter(is.na(diet_value)) %>%
   print()

 #add sediment detritus values

 detritus.vals <- corrected.data %>%
   dplyr::filter(atlantis_prey_fg %in% c("DC","DL","DR")) %>%
   dplyr::mutate(prey=paste0("sediment ",prey)) %>%
   dplyr::mutate(atlantis_prey_fg = dplyr::if_else(atlantis_prey_fg == "DC", "DC_s",
                                                   dplyr::if_else(atlantis_prey_fg == "DR", "DR_s",
                                                                  dplyr::if_else(atlantis_prey_fg == "DL", "DL_s", atlantis_prey_fg))))


 corrected.data.sed <- dplyr::bind_rows(corrected.data, detritus.vals) %>%
   dplyr::arrange(pred, predator_stage, prey)

 #save revised matrix

  write_csv(corrected.data.sed, here::here("outputs","complete_diet_matrix.csv"))

}
