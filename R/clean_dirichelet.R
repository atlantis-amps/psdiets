#' Clean and process Dirichlet maximum likelihood results
#'
#' Extracts and cleans mode values from Dirichlet distribution fits, filters
#' placeholder filler values, maps species to Atlantis functional groups, and
#' writes cleaned results to CSV.
#'
#' @param stage.name Character. Life stage being processed. One of:
#'   \itemize{
#'     \item "juvenile" — juvenile stage predators
#'     \item "adult" — adult stage predators
#'     \item "invert" — invertebrate stage predators
#'   }
#'   Used to locate input file \code{diet_dirichelet_[stage.name].csv} in
#'   \code{outputs/[stage.name]_dirichelet/} and determine functional group filters.
#'
#' @param func.groups Character. Path to functional groups reference CSV file
#'   (e.g., "data/PugetSoundAtlantisFunctionalGroupsV2.csv"). Must contain columns:
#'   \itemize{
#'     \item Code — functional group abbreviation
#'     \item Long Name — full species/group name for joining
#'     \item GroupType — category for filtering by stage
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads raw Dirichlet maximum likelihood results (output from
#'     \code{\link{do_dirichelet}})
#'   \item Filters to rows with valid 95% confidence intervals (\code{!is.na(lower95)})
#'   \item Identifies and replaces placeholder filler values (\code{< 1e-9}) with 0
#'   \item Scales mode values by dividing by 100
#'   \item Maps prey species names to Atlantis functional group codes
#'   \item Maps predator species names to Atlantis functional group codes
#'   \item Writes cleaned results to \code{outputs/[stage.name]_dirichelet_values.csv}
#' }
#'
#' **Filler value handling:** Dirichlet fitting sometimes returns very small placeholder
#' values (e.g., 1.325233e-10, 1.97e-10) for zero or near-zero diet proportions.
#' These are identified by threshold (\code{1e-9}) and replaced with 0 to avoid
#' spurious downstream effects.
#'
#' **Functional group filtering by stage:**
#' \itemize{
#'   \item juvenile/adult: FISH, SHARK, BIRD, MAMMAL
#'   \item invert: SM_ZOO, LG_ZOO, MOB_EP_OTHER, SED_EP_FF, PWN, FISH
#' }
#'
#' @return Invisibly returns a data frame with columns:
#'   \itemize{
#'     \item atlantis_pred_fg — predator functional group code
#'     \item pred — predator species/group name
#'     \item pred_stage — life stage name
#'     \item prey — prey species name
#'     \item mode_corr — cleaned and scaled mode value (mode / 100)
#'     \item atlantis_prey_fg — prey functional group code
#'   }
#'   Also writes the data frame to CSV at \code{outputs/[stage.name]_dirichelet_values.csv}.
#'
#' @examples
#' \dontrun{
#' # Process juvenile predator Dirichlet results
#' clean_dirichelet("juvenile", "data/PugetSoundAtlantisFunctionalGroupsV2.csv")
#'
#' # Process adult stage results
#' clean_dirichelet("adult", "data/PugetSoundAtlantisFunctionalGroupsV2.csv")
#'
#' # Process invertebrate stage results
#' clean_dirichelet("invert", "data/PugetSoundAtlantisFunctionalGroupsV2.csv")
#' }
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr filter mutate select left_join
#' @importFrom here here
#'
#' @export
clean_dirichelet <- function(stage.name, func.groups){
  raw.dirichelet.data <- read_csv(here::here("outputs","dirichelet_res",paste0(stage.name,"_dirichelet"),paste0("diet_dirichelet_",stage.name,".csv")), skip=1)

  estimated.data <- raw.dirichelet.data %>% dplyr::filter(!is.na(lower95))

  # Identify the common filler value threshold (very small values used as placeholders)
  filler_threshold <- 1e-9

  estimated.data.num <- estimated.data %>%
    dplyr::mutate(
      mode_mod = dplyr::if_else(mode < filler_threshold, 0, mode),
      mode_corr = mode_mod / 100,
      pred_stage = stage.name
    ) %>%
    dplyr::select(pred = PredatorName, prey = PreyName, mode_corr)

  if(stage.name=="adult"|stage.name=="juvenile"){

    group.names <- c("FISH","SHARK","BIRD","MAMMAL")

  }
  if(stage.name=="invert"){

    group.names <- c("SM_ZOO","LG_ZOO","MOB_EP_OTHER","SED_EP_FF","PWN","FISH")

  }

  prey.group.codes <- readr::read_csv(func.groups) %>%
    dplyr::select(atlantis_prey_fg=Code, prey=`Long Name`)

  group.frame <- readr::read_csv(func.groups) %>%
    dplyr::filter(GroupType %in% group.names) %>%
    dplyr::mutate(pred_stage = stage.name) %>%
    dplyr::select(atlantis_pred_fg = Code, pred = `Long Name`, pred_stage) %>%
    dplyr::left_join(estimated.data.num, by = c("pred")) %>%
    dplyr::filter(!is.na(prey)) %>%
    dplyr::left_join(prey.group.codes, by = c("prey"))

  readr::write_csv(group.frame, here::here("outputs", paste0(stage.name, "_dirichelet_values.csv")))

  invisible(group.frame)
}
