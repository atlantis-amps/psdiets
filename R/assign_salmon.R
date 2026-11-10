#' Assign diet proportions to specific salmon prey groups
#'
#' Distributes generic "SALMON" prey category into specific salmon species
#' based on observed proportions from diet data. This function handles two cases:
#' predators with detailed salmon species data, and those without.
#'
#' @param eachpred Character. Atlantis functional group code for the predator
#'   being analyzed (e.g., "ROR" for resident orca, "HSH" for harbor seal).
#'
#' @return A data frame with salmon prey distributed to specific species,
#'   containing columns: source, atlantis_pred_group, predator_stage, predator,
#'   atlantis_prey_group, prey, data, value, proportion, index.
#'
#' @details
#' This function requires the following objects in the global environment:
#' \itemize{
#'   \item \code{diet.data.salmon}: Diet data with generic "SALMON" prey
#'   \item \code{salmon.prop.group}: Predators with species-specific salmon data
#'   \item \code{salmon.prop.fg}: Mean proportions of each salmon species by predator
#'   \item \code{salmon.groups}: Default salmon species proportions
#' }
#'
#' For predators with species-specific data, generic "SALMON" is distributed
#' proportionally based on observed consumption of specific salmon species.
#' For predators without species data, default proportions from salmon.groups
#' are used.
#'
#' @importFrom dplyr filter mutate left_join select rename bind_rows
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assign salmon prey for resident orca
#' orca_diet <- assign_salmon("ROR")
#'
#' # Process multiple predators
#' salmon_predators <- c("ROR", "HSH", "SSL")
#' all_diets <- lapply(salmon_predators, assign_salmon)
#' }
assign_salmon <- function(eachpred){

  print(eachpred)

  thispreddata <- diet.data.salmon %>%
    filter(atlantis_pred_group==eachpred)

  if(eachpred %in% salmon.prop.group){


    theseprops <- salmon.prop.fg %>%
      filter(atlantis_pred_group == eachpred) %>%
      mutate(tot_prop = sum(mean_prop), norm_prop = mean_prop / tot_prop) %>%
      dplyr::rename(salmon_group = atlantis_prey_group) %>%
      mutate(atlantis_prey_group = "SALMON")

    thissalmonprop <- thispreddata %>%
      left_join(theseprops, by=c("atlantis_prey_group","atlantis_pred_group")) %>%
      mutate(salmon_prop = norm_prop * proportion) %>%
      dplyr::select(-mean_prop, -tot_prop, -norm_prop, -proportion, -atlantis_prey_group) %>%
      dplyr::rename(proportion = salmon_prop, atlantis_prey_group = salmon_group)

    rev.index.data <- thispreddata %>%
      filter(atlantis_prey_group!="SALMON") %>%
      bind_rows(thissalmonprop)

  } else {

    theseprops <- salmon.groups

    thissalmonprop <- thispreddata %>%
      left_join(theseprops, by=c("atlantis_prey_group")) %>%
      mutate(salmon_prop = fish_prop * proportion) %>%
      dplyr::select(-fish_prop, -proportion, -atlantis_prey_group) %>%
      dplyr::rename(proportion = salmon_prop, atlantis_prey_group = atlantis_group)

    rev.index.data <- thispreddata %>%
      filter(atlantis_prey_group!="SALMON") %>%
      bind_rows(thissalmonprop)

  }

  return(rev.index.data)

}
