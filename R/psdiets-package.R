#' @keywords internal
"_PACKAGE"

#' psdiets: Process and Analyze Diet Data for Puget Sound Atlantis Model
#'
#' @name psdiets-package
#' @aliases psdiets
#'
#' @description
#' The psdiets package provides tools for processing and analyzing diet data
#' from various sources for use in the Puget Sound Atlantis ecosystem model.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{organize_beauchamp}}: Process salmon diet data from Dave Beauchamp
#'   \item \code{organize_baker}: Process diet data from Baker
#'   \item \code{organize_fish}: Organize fish diet data
#'   \item \code{aggregate_diets}: Aggregate diet matrices
#'   \item \code{do_dirichelet}: Perform Dirichlet regression on diet data
#'   \item \code{pprey_maker}: Create prey preference matrices
#' }
#'
#' @section Data Processing Workflow:
#' The typical workflow for using this package:
#' \enumerate{
#'   \item Load raw diet data using organize_* functions
#'   \item Assign prey items to Atlantis functional groups
#'   \item Calculate diet proportions by predator and life stage
#'   \item Aggregate diets across multiple data sources
#'   \item Perform statistical analysis (e.g., Dirichlet regression)
#'   \item Generate prey preference matrices for Atlantis
#' }
#'
#' @section Diet Data Sources:
#' The package handles diet data from multiple sources including:
#' \itemize{
#'   \item Salmon gut content data (Beauchamp, Puget Sound)
#'   \item Fish predator-prey databases (Baker et al.)
#'   \item Various fisheries research programs
#' }
#'
#' @author Hem Nalini Morzaria Luna \email{hmorzarialuna@@gmail.com}
#'
#' @keywords package
#' @seealso
#' \url{https://github.com/Atlantis-Ecosystem-Model} for more information on Atlantis
#'
## usethis namespace: start
## usethis namespace: end
NULL
