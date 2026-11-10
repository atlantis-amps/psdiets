#' Distribute aggregated predator groups to specific functional groups
#'
#' Assigns diet data from aggregate predator categories (e.g., "ROCKFISH",
#' "BIRD", "CI") to specific Atlantis functional groups based on biomass
#' proportions. Used to disaggregate generic predator groupings in diet data.
#'
#' @param thesegroups Character. Name(s) of aggregated predator group(s) to
#'   distribute (e.g., "ROCKFISH", "CI", "BIRD").
#' @param thisdataset Data frame. Lookup table with columns: atlantis_group
#'   (specific functional groups), atlantis_pred_group (aggregate group),
#'   and fish_prop (proportion of each specific group within aggregate).
#' @param thisdata Data frame. Diet data containing atlantis_pred_group column
#'   to be disaggregated.
#'
#' @return Data frame with aggregated predator groups replaced by specific
#'   functional groups, maintaining all other diet data columns.
#'
#' @details
#' Filters diet data into two parts: observations with the aggregated predator
#' group and all other observations. For aggregated predators, joins with the
#' dataset proportions and replaces the aggregate code with specific codes.
#' Both parts are then recombined.
#'
#' This is typically used after reading raw diet files where predators may
#' be identified only to a coarse taxonomic level (e.g., "rockfish" rather
#' than specific rockfish species).
#'
#' @importFrom dplyr filter left_join select rename bind_rows arrange
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Distribute rockfish predators to specific species
#' rockfish_diet <- organize_fish.pred(
#'   thesegroups = "ROCKFISH",
#'   thisdataset = rockfish.groups.pred,
#'   thisdata = diet.data
#' )
#'
#' # Distribute bird predators
#' bird_diet <- organize_fish.pred(
#'   thesegroups = "BIRD",
#'   thisdataset = bird.groups.pred,
#'   thisdata = diet.data
#' )
#' }
organize_fish.pred <- function(thesegroups,thisdataset, thisdata){


  atlantis.fish.unid <- thisdata %>%
    filter(atlantis_pred_group %in% thesegroups)

  atlantis.fish.othersp <- thisdata %>%
    filter(!atlantis_pred_group %in% thesegroups)

  this.data.prop <- atlantis.fish.unid %>%
    left_join(thisdataset, by = "atlantis_pred_group") %>%
    dplyr::select(-atlantis_pred_group, -fish_prop) %>%
    dplyr::rename(atlantis_pred_group = atlantis_group) %>%
    bind_rows(atlantis.fish.othersp) %>%
    arrange(index,data,atlantis_pred_group,atlantis_pred_group)

  return(this.data.prop)


}
