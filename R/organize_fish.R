#' Distribute aggregated prey groups to specific functional groups
#'
#' Assigns diet data from aggregate prey categories (e.g., "FISH", "SALMON",
#' "ROCKFISH") to specific Atlantis functional groups based on biomass
#' proportions. Used to disaggregate generic prey groupings in diet data.
#'
#' @param thesegroups Character. Name(s) of aggregated prey group(s) to
#'   distribute (e.g., "ROCKFISH", "CI", "HE", "FISH").
#' @param thisdataset Data frame. Lookup table with columns: atlantis_group
#'   (specific functional groups), atlantis_prey_group (aggregate group),
#'   and fish_prop (proportion of each specific group within aggregate).
#' @param thisdata Data frame. Diet data containing atlantis_prey_group column
#'   to be disaggregated.
#'
#' @return Data frame with aggregated prey groups replaced by specific
#'   functional groups. Diet values are multiplied by fish_prop to distribute
#'   consumption proportionally across specific prey types.
#'
#' @details
#' Filters diet data into two parts: observations with the aggregated prey
#' group and all other observations. For aggregated prey, joins with the
#' dataset proportions, multiplies values by fish_prop, and replaces the
#' aggregate code with specific codes. Both parts are then recombined.
#'
#' This is typically used after reading raw diet files where prey items may
#' be identified only to a coarse taxonomic level (e.g., "fish" or "salmon"
#' rather than specific species).
#'
#' @importFrom dplyr filter left_join mutate select rename bind_rows
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Distribute generic FISH prey to specific fish species
#' fish_diet <- organize_fish(
#'   thesegroups = "FISH",
#'   thisdataset = fish.groups,
#'   thisdata = diet.data
#' )
#'
#' # Distribute SALMON prey to specific salmon species
#' salmon_diet <- organize_fish(
#'   thesegroups = "SALMON",
#'   thisdataset = salmon.groups,
#'   thisdata = diet.data
#' )
#'
#' # Distribute herring prey
#' herring_diet <- organize_fish(
#'   thesegroups = "HE",
#'   thisdataset = herring.groups,
#'   thisdata = diet.data
#' )
#' }
organize_fish <- function(thesegroups,thisdataset, thisdata){

  atlantis.fish.othersp <- thisdata %>%
    filter(atlantis_prey_group!=thesegroups)

  this.data.prop <- thisdata %>%
    filter(atlantis_prey_group %in% thesegroups) %>%
    left_join(thisdataset, by = "atlantis_prey_group") %>%
    mutate(value = fish_prop * value) %>%
    dplyr::select(-atlantis_prey_group, -fish_prop) %>%
    dplyr::rename(atlantis_prey_group = atlantis_group) %>%
    bind_rows(atlantis.fish.othersp)


  return(this.data.prop)
}

