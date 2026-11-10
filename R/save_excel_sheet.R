#' Extract and save individual sheets from Excel diet database
#'
#' Reads an Excel file containing multiple diet data sheets and exports each
#' sheet (excluding metadata sheets) as a separate CSV file for further processing.
#'
#' @param diet.data.file Character. Path to Excel file containing diet database.
#'   Expected to have metadata in first 4 sheets, with data sheets following.
#'
#' @return NULL. Creates CSV files in "outputs/diet_data_files/" directory,
#'   one for each data sheet. Filenames follow pattern: "diet_matrix_{sheet_name}.csv".
#'   Also creates "data_sheets_fileV1.csv" listing all exported sheet names.
#'
#' @details
#' Skips the first 4 sheets (assumed to be metadata/documentation) and
#' processes remaining sheets as diet data. Each sheet is read, a "source"
#' column is added with the sheet name, and the data is written to CSV.
#'
#' This function is typically used at the beginning of a diet analysis workflow
#' to convert a compiled Excel database into individual CSV files that can be
#' processed separately.
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tibble as_tibble
#' @importFrom readr write_csv
#' @importFrom dplyr mutate
#' @importFrom here here
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract all diet data sheets from Excel database
#' save_excel_sheet("Diet_database_V1_no_figures.xlsx")
#'
#' # Specify full path
#' save_excel_sheet("data/Diet_database_V2.xlsx")
#' }
save_excel_sheet <- function(diet.data.file){

  file.sheets <- readxl::excel_sheets(path = here::here("data",diet.data.file))

  file.data.sheets <- file.sheets[-c(1,2,3,4)]

  file.data.sheets %>% tibble::as_tibble() %>% readr::write_csv(here::here("data","database_sheets_list.cvs"))


  for(thissheet in file.data.sheets){

    print(thissheet)
    this.data <- readxl::read_excel(here::here("data",diet.data.file), sheet=thissheet) %>%
      mutate(source = thissheet)

    this.data %>% readr::write_csv(here::here("outputs","diet_data_files",paste("diet_data_",thissheet,".csv",sep="")))


  }

}
