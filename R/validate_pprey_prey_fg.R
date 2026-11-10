#' Validate prey functional groups in pprey matrix
#'
#' Checks that all atlantis_prey_fg values in the pprey matrix are valid
#' according to the expected prey functional group array.
#'
#' @param prm.pprey Data frame. Output from read_pprey_matrix()
#' @param prey_fg_array Character vector. Standard prey functional group codes
#'
#' @return Invisibly returns a list with validation results
#'
#' @export
validate_pprey_prey_fg <- function(prm.pprey, prey_fg_array) {

  # Check pprey values are in valid prey_fg_array
  pprey_prey_check <- prm.pprey %>%
    dplyr::mutate(
      prey_fg_valid = atlantis_prey_fg %in% prey_fg_array,
      prey_fg_status = dplyr::if_else(prey_fg_valid, "✓ Valid", paste0("✗ INVALID: ", atlantis_prey_fg))
    )

  # Summary by predator/stage
  pprey_summary <- pprey_prey_check %>%
    dplyr::group_by(parameter, functional_group, pred_stage) %>%
    dplyr::summarise(
      n_prey = dplyr::first(n_prey),
      n_valid = sum(prey_fg_valid),
      n_invalid = sum(!prey_fg_valid),
      all_valid = all(prey_fg_valid),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      status = dplyr::if_else(all_valid, "✓ Pass", "✗ FAIL")
    )

  # Extract invalid ones for detailed report
  pprey_invalid <- pprey_prey_check %>%
    dplyr::filter(!prey_fg_valid) %>%
    dplyr::distinct(parameter, functional_group, pred_stage, prey_index, atlantis_prey_fg, prey_fg_status) %>%
    dplyr::arrange(parameter, functional_group, pred_stage, prey_index)

  cat("\n========================================\n")
  cat("PPREY PREY FUNCTIONAL GROUP VALIDATION\n")
  cat("========================================\n\n")

  cat("Summary by Predator/Stage:\n")
  cat("----------------------------\n")
  print(pprey_summary)

  if (nrow(pprey_invalid) > 0) {
    cat("\n⚠️  INVALID PREY FUNCTIONAL GROUPS:\n")
    cat("---------------------------------------\n")
    print(pprey_invalid)
    cat("\n")
  } else {
    cat("\n✓ All prey functional groups are VALID\n")
    cat("\n")
  }

  invisible(list(
    summary = pprey_summary,
    invalid_prey_fgs = pprey_invalid,
    n_valid_preds = sum(pprey_summary$all_valid),
    n_invalid_preds = sum(!pprey_summary$all_valid)
  ))
}
