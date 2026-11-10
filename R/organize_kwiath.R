#' @title Process and Organize Salmon Gut Contents Data from Kwiaht (San Juan Islands)
#'
#' @description
#' This function processes salmon gut contents data from Kwiaht studies in the
#' San Juan Islands, mapping prey items to Atlantis functional groups and
#' calculating diet proportions. The function handles data from multiple years
#' (2009-2017) across multiple Excel sheets and processes multiple salmon species.
#'
#' @details
#' The function processes diet data for multiple salmon species (Chinook, Coho,
#' Chum, Sockeye, Cutthroat) from the San Juan Islands collected by Kwiaht
#' (data sent by Russel Barsh).
#'
#' **Data Structure:**
#' The Excel workbook should contain:
#' \itemize{
#'   \item Sheet 1: Prey group mappings (acronym to species/atlantis_group)
#'   \item Sheet 2: 2017 data
#'   \item Sheet 3: 2016 data
#'   \item Sheet 4: 2015 data
#'   \item Sheet 5: 2014 data
#'   \item Sheet 6: 2013 data
#'   \item Sheet 7: 2012 data
#'   \item Sheet 8: 2011 data
#'   \item Sheet 9: 2010 data
#'   \item Sheet 10: 2009 data
#' }
#'
#' **Processing Steps:**
#' 1. Reads prey group mappings from first sheet
#' 2. Processes each year's data separately (2009-2017)
#' 3. Separates Chinook data from other salmon species
#' 4. Combines all years and creates unique indices
#' 5. Classifies predators by life stage based on fork length (FL > 300mm = adult)
#' 6. Distributes unidentified "FISH" category proportionally among identified fish groups
#' 7. Calculates diet proportions normalized within each stomach sample
#'
#' **Fish Group Allocation:**
#' Unidentified fish are distributed among: FPS (Sand Lance), HE (Herring),
#' SMD (Small demersal), FVS (Lingcod), POP (Perch), FDF (Flatfish)
#'
#' @param atlantis_fg_file Path to the atlantis functional groups CSV file
#' @param atlantis_groups_file Path to the atlantis groups CSV file
#' @param data.file Character. Name of the raw CSV diet data file.
#' @param source.name Character. Name of data source.
#' @param cvs.file Character. Name of output file.
#' @param output.path Character. Path where output CSV should be saved.
#'
#' @return Character string "done" upon successful completion. The function also
#'   writes a CSV file to the specified output path containing processed diet data
#'   with the following columns:
#' \describe{
#'   \item{source}{Data source identifier}
#'   \item{atlantis_pred_group}{Atlantis functional group code for predator (CI, CO, CU, SAL, CI1_CI2)}
#'   \item{predator_stage}{Life stage of predator ("adult" or "juvenile" based on FL > 300mm)}
#'   \item{predator}{Common name of predator species (Chinook, Coho, Chum, Sock, Cutthroat)}
#'   \item{atlantis_prey_group}{Atlantis functional group code for prey}
#'   \item{prey}{Prey taxa name}
#'   \item{data}{Unique index for each stomach sample}
#'   \item{value}{Diet proportion (same as proportion column)}
#'   \item{proportion}{Proportion of prey item in diet (0-1)}
#'   \item{index}{Same as data column, unique index for each stomach sample}
#' }
#'
#' @references
#' Data collected by Kwiaht in the San Juan Islands. Data sent by Russel Barsh.
#' Salmon gut contents analysis from multiple years (2009-2017).
#'
#' @author Hem Nalini Morzaria Luna, \email{hmorzarialuna@@gmail.com}
#' @keywords Puget Sound, Atlantis, diet data, salmon, Chinook, Coho, Chum, San Juan Islands
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter select rename left_join group_by summarise ungroup bind_rows arrange
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom here here
#' @importFrom readxl read_xlsx
#'
#' @examples
#' \dontrun{
#' # Basic usage will save output to file
#' organize_daubenberg(
#' atlantis_fg_file,
#' atlantis_groups_file,
#' data.file,
#' source.name,
#' cvs.file,
#' output.path
#' )
#'
#'
#' @export
organize_kwiaht <- function(
  atlantis_fg_file,
  atlantis_groups_file,
  data.file,
  source.name,
  cvs.file,
  output.path
) {
  # Input validation --------------------------------------------------------

  if (!file.exists(here::here("data", data.file))) {
    stop("Diet file not found: ", data.file)
  }
  if (!file.exists(here::here("data", atlantis_fg_file))) {
    stop("Atlantis functional groups file not found: ", atlantis_fg_file)
  }
  if (!file.exists(here::here("data", atlantis_groups_file))) {
    stop("Atlantis groups file not found: ", atlantis_groups_file)
  }

  # Read Atlantis functional group definitions ------------------------------

  # Read and standardize Atlantis functional group names to lowercase with underscores
  atlantis_fg <- readr::read_csv(here::here("data", atlantis_fg_file)) %>%
    mutate(
      atlantis_name = tolower(atlantis_name),
      atlantis_name = gsub(" ", "_", atlantis_name)
    )

  # Read prey group mappings ------------------------------------------------

  # First sheet contains prey group mappings (acronym -> species -> atlantis_group)
  atlantis.kw.sp <- read_xlsx(here::here("data", data.file), sheet = 1) %>%
    rename(variable = acronym)

  # Define salmon categories for filtering ----------------------------------

  # These categories identify non-Chinook salmon in the "Other" column
  salmon.categories <- c(
    "Chum?",
    "Sock?",
    "Coho",
    "Coho?",
    "Jack or Steel",
    "Cutthroat"
  )

  # Process 2017 data -------------------------------------------------------

  # Process Chinook salmon from 2017 (sheet 2)
  # Chinook are identified by NOT being in salmon.categories
  atlantis.kw.data.ch1 <- read_xlsx(
    here::here("data", data.file),
    sheet = 2
  ) %>%
    # Remove non-prey columns
    dplyr::select(
      -Date,
      -Vial,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    # Select only total length columns (prey counts)
    dplyr::select(
      Marked,
      Other,
      contains("totL"),
      contains("totl"),
      contains("Totl")
    ) %>%
    # Create unique index for each stomach sample
    mutate(index = 1:nrow(.)) %>%
    # Convert to long format (one row per prey type per stomach)
    gather(key = variable, value = value, -Marked, -Other, -index) %>%
    # Keep only Chinook (filter out other salmon)
    filter(!Other %in% salmon.categories) %>%
    # Clean variable names by removing total length indicators
    mutate(variable = gsub(" totL", "", variable)) %>%
    mutate(variable = gsub(" totl", "", variable)) %>%
    mutate(variable = gsub(" Totl", "", variable)) %>%
    mutate(variable = gsub(" Totl", "", variable)) %>%
    mutate(variable = gsub("Totl", "", variable)) %>%
    # Map prey acronyms to Atlantis groups
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2017") # %>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2017 (sheet 2)
  # These are identified by being in salmon.categories
  atlantis.kw.data.oth1 <- read_xlsx(
    here::here("data", data.file),
    sheet = 2
  ) %>%
    # Remove non-prey columns
    dplyr::select(
      -Date,
      -Vial,
      -Marked,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    # Convert to long format
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    # Remove total columns (these were already captured for Chinook)
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    # Keep only other salmon species
    filter(Other %in% salmon.categories) %>%
    # Clean variable names
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    # Map to Atlantis groups
    left_join(atlantis.kw.sp, by = "variable") %>%
    # Use "Other" column as predator name and remove "?" marks
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2017") # %>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2017
  atlantis.kw.2017 <- atlantis.kw.data.ch1 %>% bind_rows(atlantis.kw.data.oth1)

  # Process 2016 data -------------------------------------------------------

  # Process Chinook salmon from 2016 (sheet 3)
  atlantis.kw.data.ch2 <- read_xlsx(
    here::here("data", data.file),
    sheet = 3
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Marked,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2016") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2016 (sheet 3)
  atlantis.kw.data.oth2 <- read_xlsx(
    here::here("data", data.file),
    sheet = 3
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Marked,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2016")

  # Combine Chinook and other salmon for 2016
  atlantis.kw.2016 <- atlantis.kw.data.ch2 %>% bind_rows(atlantis.kw.data.oth2)

  # Process 2015 data -------------------------------------------------------

  # Process Chinook salmon from 2015 (sheet 4)
  atlantis.kw.data.ch3 <- read_xlsx(
    here::here("data", data.file),
    sheet = 4
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Marked,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    # Additional filters for 2015 data
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2015") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2015 (sheet 4)
  atlantis.kw.data.oth3 <- read_xlsx(
    here::here("data", data.file),
    sheet = 4
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Marked,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2015") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2015
  atlantis.kw.2015 <- atlantis.kw.data.ch3 %>% bind_rows(atlantis.kw.data.oth3)

  # Process 2014 data -------------------------------------------------------

  # Process Chinook salmon from 2014 (sheet 5)
  atlantis.kw.data.ch4 <- read_xlsx(
    here::here("data", data.file),
    sheet = 5
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2, # Note: different column name in 2014
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    # Convert FL to numeric (may have non-numeric values)
    mutate(FL = as.numeric(FL)) %>%
    mutate(year = "2014") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2014 (sheet 5)
  atlantis.kw.data.oth4 <- read_xlsx(
    here::here("data", data.file),
    sheet = 5
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2014") %>%
    mutate(FL = as.numeric(FL))
  #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2014
  atlantis.kw.2014 <- atlantis.kw.data.ch4 %>% bind_rows(atlantis.kw.data.oth4)

  # Process 2013 data -------------------------------------------------------

  # Process Chinook salmon from 2013 (sheet 6)
  atlantis.kw.data.ch5 <- read_xlsx(
    here::here("data", data.file),
    sheet = 6
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark, # Note: "Mark" instead of "Marked" in 2013
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2013") %>%
    # Handle non-standard FL values (replace "x" with NA)
    mutate(FL = gsub("x", "NA", FL), FL = as.numeric(FL)) %>%
    # Convert value to numeric
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2013 (sheet 6)
  atlantis.kw.data.oth5 <- read_xlsx(
    here::here("data", data.file),
    sheet = 6
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2013") %>%
    mutate(FL = gsub("x", "NA", FL), FL = as.numeric(FL)) %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2013
  atlantis.kw.2013 <- atlantis.kw.data.ch5 %>% bind_rows(atlantis.kw.data.oth5)

  # Process 2012 data -------------------------------------------------------

  # Process Chinook salmon from 2012 (sheet 7)
  atlantis.kw.data.ch6 <- read_xlsx(
    here::here("data", data.file),
    sheet = 7
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2012") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2012 (sheet 7)
  atlantis.kw.data.oth6 <- read_xlsx(
    here::here("data", data.file),
    sheet = 7
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -Volume,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Location, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2012") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2012
  atlantis.kw.2012 <- atlantis.kw.data.ch6 %>% bind_rows(atlantis.kw.data.oth6)

  # Process 2011 data -------------------------------------------------------

  # Process Chinook salmon from 2011 (sheet 8)
  atlantis.kw.data.ch7 <- read_xlsx(
    here::here("data", data.file),
    sheet = 8
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -`Gut volume`, # Different column name in earlier years
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    # Note: "Site" instead of "Location" in earlier years
    gather(key = variable, value = value, -Site, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    # Additional filters for length-related columns
    filter(!grepl("avL", variable)) %>%
    filter(!grepl("FL", variable)) %>%
    filter(!grepl(" L", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2011") %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2011 (sheet 8)
  atlantis.kw.data.oth7 <- read_xlsx(
    here::here("data", data.file),
    sheet = 8
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -`Gut volume`,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Site, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!grepl("avL", variable)) %>%
    filter(!grepl("FL", variable)) %>%
    filter(!grepl(" L", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2011") %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2011
  atlantis.kw.2011 <- atlantis.kw.data.ch7 %>% bind_rows(atlantis.kw.data.oth7)

  # Process 2010 data -------------------------------------------------------

  # Process Chinook salmon from 2010 (sheet 9)
  atlantis.kw.data.ch8 <- read_xlsx(
    here::here("data", data.file),
    sheet = 9
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -`Gut volume`,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Site, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!grepl("avL", variable)) %>%
    filter(!grepl("FL", variable)) %>%
    filter(!grepl("FLL", variable)) %>%
    filter(!grepl(" L", variable)) %>%
    # Additional filters specific to 2010 data
    filter(!grepl("MidgeL", variable)) %>%
    filter(!grepl("mm", variable)) %>%
    filter(!grepl("Fin", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2010") %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2010 (sheet 9)
  atlantis.kw.data.oth8 <- read_xlsx(
    here::here("data", data.file),
    sheet = 9
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -`Gut volume`,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Site, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!grepl("avL", variable)) %>%
    filter(!grepl("FL", variable)) %>%
    filter(!grepl("FLL", variable)) %>%
    filter(!grepl(" L", variable)) %>%
    filter(!grepl("MidgeL", variable)) %>%
    filter(!grepl("mm", variable)) %>%
    filter(!grepl("Fin", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2010") %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2010
  atlantis.kw.2010 <- atlantis.kw.data.ch8 %>% bind_rows(atlantis.kw.data.oth8)

  # Process 2009 data -------------------------------------------------------

  # Process Chinook salmon from 2009 (sheet 10)
  atlantis.kw.data.ch9 <- read_xlsx(
    here::here("data", data.file),
    sheet = 10
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -`Gut volume`,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Site, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!grepl("avL", variable)) %>%
    filter(!grepl("FL", variable)) %>%
    filter(!grepl("FLL", variable)) %>%
    filter(!grepl(" L", variable)) %>%
    filter(!grepl("MidgeL", variable)) %>%
    filter(!grepl("mm", variable)) %>%
    filter(!grepl("Fin", variable)) %>%
    filter(!Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = "Chinook") %>%
    dplyr::select(-Other) %>%
    mutate(year = "2009") %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Process other salmon species from 2009 (sheet 10)
  atlantis.kw.data.oth9 <- read_xlsx(
    here::here("data", data.file),
    sheet = 10
  ) %>%
    dplyr::select(
      -Date,
      -Vial,
      -Mark,
      -`#Lice`,
      FL,
      Other,
      -`Gut volume`,
      -Other2,
      -Other3
    ) %>%
    mutate(index = 1:nrow(.)) %>%
    gather(key = variable, value = value, -Site, -Other, -index, -FL) %>%
    filter(!grepl("totL", variable)) %>%
    filter(!grepl("totl", variable)) %>%
    filter(!grepl("Totl", variable)) %>%
    filter(!grepl("Total", variable)) %>%
    filter(!grepl("total", variable)) %>%
    filter(!grepl("avL", variable)) %>%
    filter(!grepl("FL", variable)) %>%
    filter(!grepl("FLL", variable)) %>%
    filter(!grepl(" L", variable)) %>%
    filter(!grepl("MidgeL", variable)) %>%
    filter(!grepl("mm", variable)) %>%
    filter(!grepl("Fin", variable)) %>%
    filter(Other %in% salmon.categories) %>%
    mutate(variable = gsub("#", "", variable)) %>%
    mutate(variable = gsub(" min", "", variable)) %>%
    left_join(atlantis.kw.sp, by = "variable") %>%
    mutate(predator = Other, predator = gsub("\\?", "", predator)) %>%
    dplyr::select(-Other) %>%
    mutate(year = "2009") %>%
    mutate(value = as.numeric(value)) # %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

  # Combine Chinook and other salmon for 2009
  atlantis.kw.2009 <- atlantis.kw.data.ch9 %>% bind_rows(atlantis.kw.data.oth9)

  # Combine all years and process ------------------------------------------

  # Combine all years (2009-2017)
  # Eliminates "other" categories like leaf and rock
  atlantis.kw.index <- bind_rows(
    atlantis.kw.2009,
    atlantis.kw.2010,
    atlantis.kw.2011,
    atlantis.kw.2012,
    atlantis.kw.2013,
    atlantis.kw.2014,
    atlantis.kw.2015,
    atlantis.kw.2016,
    atlantis.kw.2017
  ) %>%
    arrange(year, index) %>%
    # Convert year and index to numeric
    mutate(year = as.numeric(year), index = as.numeric(index)) %>%
    # Rename columns to standard format
    rename(atlantis_prey_group = atlantis_group, prey_taxa = Species) %>%
    # Create unique index across all years
    mutate(index_2 = group_indices_(., .dots = c('year', 'index'))) %>%
    # Assign Atlantis predator group codes based on species
    mutate(
      atlantis_predator_group = if_else(
        predator == "Chinook",
        "CI",
        if_else(
          predator == "Coho",
          "CO",
          if_else(
            predator == "Chum",
            "CU",
            if_else(predator == "Sock" | predator == "Cutthroat", "SAL", "NA")
          )
        )
      ),
      # Add source identifier
      source = "Kwiath_San_Juan",
      # Classify life stage based on fork length (>300mm = adult)
      predator_stage = if_else(FL > 300, "adult", "juvenile"),
      # If FL is missing, assume adult
      predator_stage = if_else(is.na(FL), "adult", predator_stage),
      # Handle missing Marked values
      Marked = if_else(is.na(Marked), "NO", Marked)
    ) %>%
    # Identify marked Chinook (hatchery vs wild) as separate group
    mutate(
      atlantis_predator_group = if_else(
        Marked == "Y" & predator == "Chinook",
        "CI1_CI2",
        atlantis_predator_group
      )
    ) %>%
    # Select final columns
    dplyr::select(
      source,
      atlantis_predator_group,
      predator_stage,
      predator,
      atlantis_prey_group,
      prey_taxa,
      value,
      index_2
    ) %>%
    rename(index = index_2) %>%
    # Replace NA values with 0
    mutate(value = if_else(is.na(value), 0, value)) %>%
    # Remove non-biological prey items
    filter(atlantis_prey_group != "Other")

  # Calculate initial proportions -------------------------------------------

  # Calculate total prey per stomach and initial proportions
  atlantis.kw.prop <- atlantis.kw.index %>%
    group_by(
      source,
      atlantis_predator_group,
      predator_stage,
      predator,
      index
    ) %>%
    summarise(sum_tot = sum(value)) %>%
    left_join(
      atlantis.kw.index,
      by = c(
        "source",
        "atlantis_predator_group",
        "predator_stage",
        "predator",
        "index"
      )
    ) %>%
    # Calculate proportion of each prey item
    mutate(proportion = value / sum_tot) %>%
    # Handle NaN values (empty stomachs)
    mutate(proportion = if_else(is.nan(proportion), 0, proportion)) %>%
    dplyr::select(-value)

  # Distribute unidentified fish among identified fish groups ---------------

  # Fish is allocated to identified fish groups:
  # FPS (Sand Lance), HE (Herring), SMD (Small demersal),
  # FVS (Lingcod), POP (Perch), FDF (Flatfish)

  # Calculate total proportion of unidentified "FISH" per stomach
  atlantis.kw.fish <- atlantis.kw.prop %>%
    filter(atlantis_prey_group == "FISH") %>%
    group_by(index) %>%
    summarise(sum_tot_fish = sum(proportion))

  # Count how many identified fish groups are present per stomach
  atlantis.kw.fish.prop <- atlantis.kw.prop %>%
    filter(atlantis_prey_group != "FISH") %>% #eliminate fish
    #select only fish groups
    filter(
      atlantis_prey_group == "FDF" |
        atlantis_prey_group == "FPS" |
        atlantis_prey_group == "POP" |
        atlantis_prey_group == "FVS" |
        atlantis_prey_group == "SMD" |
        atlantis_prey_group == "HE"
    ) %>%
    filter(proportion != 0) %>%
    mutate(index = as.character(index)) %>%
    group_by(index) %>%
    # Count number of fish groups present
    summarise(fish_groups = dplyr::n()) %>%
    ungroup %>%
    mutate(index = as.numeric(index)) %>%
    left_join(atlantis.kw.fish, by = "index") %>%
    # Calculate equal proportion to distribute to each fish group
    mutate(fish_prop = sum_tot_fish / fish_groups)

  # Define identified fish groups
  fish.groups <- c("FDF", "FPS", "POP", "FVS", "SMD", "HE")

  # Add distributed fish proportion to identified fish groups
  atlantis.kw.fish.final <- atlantis.kw.prop %>%
    filter(atlantis_prey_group != "FISH") %>% #eliminate fish
    #select only fish groups
    filter(atlantis_prey_group %in% fish.groups) %>%
    filter(proportion != 0) %>%
    left_join(atlantis.kw.fish.prop, by = "index") %>%
    # Add distributed fish proportion
    mutate(proportion = proportion + fish_prop) %>%
    dplyr::select(-fish_prop, -sum_tot_fish, -fish_groups)

  # Create final dataset ----------------------------------------------------

  # Combine non-fish groups with updated fish groups
  diet.cvs.final <- atlantis.kw.prop %>%
    filter(atlantis_prey_group != "FISH") %>% #eliminate fish
    #select only non-fish groups
    filter(!atlantis_prey_group %in% fish.groups) %>%
    # Add fish groups with distributed proportions
    bind_rows(atlantis.kw.fish.final) %>%
    # Create value column (same as proportion)
    mutate(value = proportion, data = index) %>%
    select(
      source,
      atlantis_predator_group,
      predator_stage,
      predator,
      atlantis_prey_group,
      prey_taxa,
      data,
      value,
      proportion,
      index
    ) %>%
    # Rename columns to standard format
    rename(atlantis_pred_group = atlantis_predator_group, prey = prey_taxa)

  # Save output -------------------------------------------------------------

  readr::write_csv(diet.cvs.final, here::here(output.path, csv.file))
  message("Diet matrix saved to: ", output.path)

  # Summary statistics ------------------------------------------------------

  n_prey_groups <- length(unique(diet.cvs.final$atlantis_prey_group))
  n_prey_taxa <- length(unique(diet.cvs.final$prey))

  message("Processing complete!")
  message("  - Atlantis prey groups: ", n_prey_groups)
  message("  - Individual prey taxa: ", n_prey_taxa)
  message(
    "  - Predator stages: ",
    paste(unique(diet.cvs.final$predator_stage), collapse = ", ")
  )

  return("done")
}
