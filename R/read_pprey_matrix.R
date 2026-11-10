read_pprey_matrix <- function(biology.prm) {

   lines <- readr::read_lines(biology.prm)

  prey_fg <- c(
    "BB", "PB", "PL", "PS", "MA", "SG", "ZS", "ZM", "ZL", "ZG", "AUR", "SQX", "BMD", "BD", "BG", "BMS", "DUN", "BML", "PWN", "BFF", "BIV", "GEC", "BC", "HEP", "HEC", "FPS", "POP", "CHY", "CHS", "CSY", "CSS", "CSN", "CDS", "CNY", "CNS", "CHC", "CYE", "CKS", "CRH", "CRW", "CRC", "COH", "COS", "COD", "COY", "COR", "CDR", "CMS", "CMF", "CMH", "PIS", "SAL", "SAF", "FMM", "FVS", "ROC", "MRO", "DVR", "MVR", "SMD", "FDF", "HAP", "DOG", "SBL", "SSK", "RAT", "SB", "SP", "BE", "HSL", "CSL", "PIN", "PHR", "ROR", "TOR", "HUW", "DL", "DR", "DC", "DL_s", "DR_s", "DC_s"
  )

  pprey_indices <- grep("^pPREY", lines)

  pprey_list <- list()

  for (i in seq_along(pprey_indices)) {
    param_line_idx <- pprey_indices[i]
    param_name <- stringr::str_extract(lines[param_line_idx], "^pPREY\\S+")
    values_line_idx <- param_line_idx + 1

    values <- stringr::str_trim(lines[values_line_idx]) %>%
      stringr::str_split("\\s+") %>%
      .[[1]] %>%
      as.numeric()

    pprey_list[[param_name]] <- list(
      parameter = param_name,
      values = values
    )
  }

  pprey_tibble <- pprey_list %>%
    purrr::map_df(function(x) {
      tibble::tibble(
        parameter = x$parameter,
        prey_index = seq_along(x$values),
        availability = x$values
      )
    }) %>%
    dplyr::mutate(
      param_name = "pPREY",
      suffix = stringr::str_extract(parameter, "(?<=pPREY).+"),
      has_stages = stringr::str_detect(suffix, "^\\d+[A-Za-z]{2,3}\\d+$"),
      prey_stage = dplyr::if_else(
        has_stages,
        as.numeric(stringr::str_extract(suffix, "^\\d+")),
        NA_real_
      ),
      functional_group = dplyr::if_else(
        has_stages,
        stringr::str_extract(suffix, "[A-Za-z]{2,3}(?=\\d+$)"),
        stringr::str_extract(suffix, "^[A-Za-z]{2,3}$")
      ),
      pred_stage = dplyr::if_else(
        has_stages,
        as.numeric(stringr::str_extract(suffix, "\\d+$")),
        NA_real_
      ),
      suffix = NULL,
      has_stages = NULL
    ) %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(
      n_prey = sum(availability > 0, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      atlantis_prey_fg = prey_fg[prey_index]
    )

  return(pprey_tibble)
}
