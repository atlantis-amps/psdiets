#' @title Parse pPREY matrix from Atlantis biology parameter file
#'
#' Reads the pPREY prey-availability entries from a biology .prm file and
#' returns them as a data frame with prey group codes as column names.
#'
#' @param prm_file Character. Path to the biology .prm file.
#'   Defaults to \code{here::here("data", "AMPSbioparam_mv1_2024_V10.prm")}.
#' @param start_line Integer. First line of the pPREY section (default 28282).
#' @param end_line Integer. Last line of the pPREY section (default 28967).
#'
#' @return A data frame with one row per pPREY parameter and 82 columns named
#'   by prey group code (BB through DC_s). Row names are the pPREY parameter
#'   names (e.g., \code{pPREY1BE1}, \code{pPREYGEC}).
#'
#' @details
#' Fish/vertebrate predators have four pPREY entries per species:
#' \itemize{
#'   \item \code{pPREY1\{CODE\}1}: juvenile prey availability to juvenile predator
#'   \item \code{pPREY2\{CODE\}1}: adult prey availability to juvenile predator
#'   \item \code{pPREY1\{CODE\}2}: juvenile prey availability to adult predator
#'   \item \code{pPREY2\{CODE\}2}: adult prey availability to adult predator
#' }
#' Invertebrate predators have a single \code{pPREY\{CODE\}} entry (no stage suffixes).
#'
#' @importFrom readr read_lines
#' @importFrom here here
#'
#' @export
pprey_modify <- function(prm_file  = here::here("data", "AMPSbioparam_mv1_2024_V10.prm"),
                         start_line = 28282L,
                         end_line   = 28967L) {

  prey_cols <- c(
    "BB",   "PB",   "PL",   "PS",   "MA",   "SG",   "ZS",   "ZM",   "ZL",   "ZG",
    "AUR",  "SQX",  "BMD",  "BD",   "BG",   "BMS",  "DUN",  "BML",  "PWN",  "BFF",
    "BIV",  "GEC",  "BC",   "HEP",  "HEC",  "FPS",  "POP",  "CHY",  "CHS",  "CSY",
    "CSS",  "CSN",  "CDS",  "CNY",  "CNS",  "CHC",  "CYE",  "CKS",  "CRH",  "CRW",
    "CRC",  "COH",  "COS",  "COD",  "COY",  "COR",  "CDR",  "CMS",  "CMF",  "CMH",
    "PIS",  "SAL",  "SAF",  "FMM",  "FVS",  "ROC",  "MRO",  "DVR",  "MVR",  "SMD",
    "FDF",  "HAP",  "DOG",  "SBL",  "SSK",  "RAT",  "SB",   "SP",   "BE",   "HSL",
    "CSL",  "PIN",  "PHR",  "ROR",  "TOR",  "HUW",  "DL",   "DR",   "DC",   "DL_s",
    "DR_s", "DC_s"
  )

  # Read only the relevant section for efficiency
  section <- readr::read_lines(
    prm_file,
    skip  = start_line - 1L,
    n_max = end_line - start_line + 1L
  )

  param_names <- character()
  value_rows  <- list()

  i <- 1L
  while (i <= length(section)) {
    line <- trimws(section[i])

    if (startsWith(line, "pPREY")) {
      # Extract the parameter name (first whitespace-delimited token)
      param_name <- strsplit(line, "\\s+")[[1L]][1L]

      # Advance past any blank lines to the data line
      i <- i + 1L
      while (i <= length(section) && trimws(section[i]) == "") {
        i <- i + 1L
      }

      if (i <= length(section)) {
        values <- as.numeric(strsplit(trimws(section[i]), "\\s+")[[1L]])
        param_names                        <- c(param_names, param_name)
        value_rows[[length(value_rows) + 1L]] <- values
      }
    }

    i <- i + 1L
  }

  result           <- as.data.frame(do.call(rbind, value_rows))
  colnames(result) <- prey_cols
  rownames(result) <- param_names

  result
}
