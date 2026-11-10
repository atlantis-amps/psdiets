pprey_maker <- function(eachpredstage, pred.stages, func.groups, diet.matrix, diet.matrix.file.name, prm.file.name, prey.order, pred.names){

  prm.pprey <- read_pprey_matrix(biology.prm)

  readr::write_csv(prm.pprey, here::here("outputs","pprey_biologyprm.csv"))

  # Prepare diet_matrix for comparison
  diet_summary <- diet.matrix %>%
    dplyr::group_by(atlantis_pred_fg, predator_stage) %>%
    dplyr::summarise(
      diet_count_prey = dplyr::n_distinct(prey),
      diet_prey_fgs = paste(sort(unique(atlantis_prey_fg)), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::rename(pred = atlantis_pred_fg)

  # Check for data quality issues in diet.matrix
  diet_stage_issues <- diet_summary %>%
    dplyr::group_by(pred) %>%
    dplyr::summarise(
      stages = paste(sort(unique(predator_stage)), collapse = ", "),
      has_invert = any(predator_stage == "INVERT"),
      has_juvenile = any(predator_stage == "juvenile"),
      has_adult = any(predator_stage == "adult"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      issue = dplyr::case_when(
        has_invert & (has_juvenile | has_adult) ~ "INVERT mixed with juvenile/adult",
        has_juvenile & !has_adult ~ "Missing adult stage",
        has_adult & !has_juvenile ~ "Missing juvenile stage",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(issue))

  if (nrow(diet_stage_issues) > 0) {
    cat("\n=== DATA QUALITY ISSUES IN DIET.MATRIX ===\n")
    print(diet_stage_issues)
    cat("\n")
  }

  # Prepare prm.pprey for comparison
  pprey_summary <- prm.pprey %>%
    dplyr::filter(availability!=0) %>%
    dplyr::group_by(functional_group, pred_stage) %>%
    dplyr::summarise(
      pprey_count_prey = dplyr::first(n_prey),
      pprey_prey_fgs = paste(sort(unique(atlantis_prey_fg)), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::rename(pred = functional_group, predator_stage = pred_stage)

  # Get list of predators that have BOTH juvenile (1) and adult (2) stages in pprey
  preds_with_both_stages <- pprey_summary %>%
    dplyr::group_by(pred) %>%
    dplyr::summarise(has_stage_1 = any(predator_stage == 1), has_stage_2 = any(predator_stage == 2), .groups = "drop") %>%
    dplyr::filter(has_stage_1 & has_stage_2) %>%
    dplyr::pull(pred)

  # Convert predator_stage to numeric for pprey comparison (INVERT=NA, juvenile=1, adult=2)
  diet_with_stage <- diet_summary %>%
    dplyr::mutate(
      predator_stage_numeric = dplyr::case_when(
        predator_stage == "INVERT" ~ NA_real_,
        predator_stage == "juvenile" ~ 1,
        predator_stage == "adult" ~ 2,
        TRUE ~ NA_real_
      ),
      predator_stage_check = dplyr::case_when(
        predator_stage == "INVERT" ~ NA_real_,
        pred %in% preds_with_both_stages ~ predator_stage_numeric,
        TRUE ~ NA_real_
      )
    )

  # Prepare pprey data with stage matching
  pprey_for_comparison <- pprey_summary %>%
    dplyr::mutate(
      predator_stage_check = dplyr::case_when(
        pred %in% preds_with_both_stages & (predator_stage == 1 | predator_stage == 2) ~ predator_stage,
        TRUE ~ NA_real_
      )
    )

  # Compare diet items
  comparison <- dplyr::full_join(
    diet_with_stage %>% dplyr::select(pred, predator_stage, predator_stage_check, diet_count_prey, diet_prey_fgs),
    pprey_for_comparison %>%
    dplyr::select(pred, predator_stage, predator_stage_check, pprey_count_prey, pprey_prey_fgs),
    by = c("pred" = "pred", "predator_stage_check" = "predator_stage_check"),
    suffix = c("_diet", "_pprey")
  ) %>%
    dplyr::select(pred, predator_stage_diet, predator_stage_pprey, predator_stage_check, diet_count_prey, pprey_count_prey, diet_prey_fgs, pprey_prey_fgs) %>%
    dplyr::mutate(
      count_match = dplyr::case_when(
        is.na(diet_count_prey) ~ "Missing in diet.matrix",
        is.na(pprey_count_prey) ~ "Missing in prm.pprey",
        diet_count_prey == pprey_count_prey ~ "Match",
        TRUE ~ paste0("Mismatch: ", diet_count_prey, " vs ", pprey_count_prey)
      ),
      prey_fg_match = dplyr::case_when(
        is.na(diet_prey_fgs) | is.na(pprey_prey_fgs) ~ "N/A",
        diet_prey_fgs == pprey_prey_fgs ~ "Match",
        TRUE ~ "Mismatch"
      )
    ) %>%
    dplyr::arrange(pred, predator_stage_check)

  get_missing_groups <- function(diet_fgs, pprey_fgs) {
    if (is.na(diet_fgs) && is.na(pprey_fgs)) return(NA_character_)
    if (!is.na(diet_fgs) && is.na(pprey_fgs)) return(diet_fgs)
    if (is.na(diet_fgs) && !is.na(pprey_fgs)) return(NA_character_)

    diet_groups <- trimws(strsplit(diet_fgs, ", ")[[1]])
    pprey_groups <- trimws(strsplit(pprey_fgs, ", ")[[1]])
    missing <- diet_groups[!diet_groups %in% pprey_groups]
    if (length(missing) > 0) paste(missing, collapse = ", ") else NA_character_
  }

  get_extra_groups <- function(diet_fgs, pprey_fgs) {
    if (is.na(diet_fgs) && is.na(pprey_fgs)) return(NA_character_)
    if (!is.na(diet_fgs) && is.na(pprey_fgs)) return(pprey_fgs)
    if (is.na(diet_fgs) && !is.na(pprey_fgs)) return(NA_character_)

    diet_groups <- trimws(strsplit(diet_fgs, ", ")[[1]])
    pprey_groups <- trimws(strsplit(pprey_fgs, ", ")[[1]])
    extra <- pprey_groups[!pprey_groups %in% diet_groups]
    if (length(extra) > 0) paste(extra, collapse = ", ") else NA_character_
  }

  comparison.modify <- comparison %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      groups_to_add = get_missing_groups(diet_prey_fgs, pprey_prey_fgs),
      groups_to_remove = get_extra_groups(diet_prey_fgs, pprey_prey_fgs),
      groups_to_add=str_replace_all(groups_to_add,",", ""),
      groups_to_remove = str_replace_all(groups_to_remove,",", ""),
      no_groups_add = stringi::stri_count_words(groups_to_add),
      no_groups_remove = stringi::stri_count_words(groups_to_remove)
    ) %>%
    dplyr::ungroup()

  comparison.mismatches <- comparison.modify %>%
    dplyr::filter(count_match != "Match" | prey_fg_match != "Match") %>%
    dplyr::select(pred, predator_stage_diet, diet_count_prey, pprey_count_prey, groups_to_add, groups_to_remove)

  prm.corr <- prm.pprey %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(tot_avail=sum(availability))

  # Extract groups to remove as a lookup table
  remove_lookup <- comparison.mismatches %>%
    dplyr::filter(!is.na(groups_to_remove)) %>%
    dplyr::select(pred, predator_stage_diet, groups_to_remove) %>%
    dplyr::mutate(
      pred_stage_numeric = dplyr::case_when(
        predator_stage_diet == "INVERT" ~ NA_real_,
        predator_stage_diet == "juvenile" ~ 1,
        predator_stage_diet == "adult" ~ 2,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      prey_group = list(trimws(strsplit(groups_to_remove, ", ")[[1]]))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(prey_group) %>%
    dplyr::select(pred, pred_stage_numeric, prey_group)

  # Update prm.corr: set availability to 0 for groups marked for removal
  prm.corr.rev <- prm.corr %>%
    dplyr::left_join(
      remove_lookup %>% dplyr::distinct() %>% dplyr::mutate(marked_for_removal = TRUE),
      by = c("functional_group" = "pred", "pred_stage" = "pred_stage_numeric", "atlantis_prey_fg" = "prey_group")
    ) %>%
    dplyr::mutate(
      availability = dplyr::coalesce(
        dplyr::if_else(marked_for_removal == TRUE, 0, NA_real_),
        availability
      )
    ) %>%
    dplyr::select(-marked_for_removal)

}

