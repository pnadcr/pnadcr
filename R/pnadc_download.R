#' Download PNADc
#'
#' Downloads the PNADc data and creates a survey design on your computer.
#' It saves an external representation of an R object that can be later read in other functions of PNADc.table.
#'
#' @param year The year of the PNADc edition to download. Must be a number between 2012 and the current year.
#' @param quartile The quartile of the year to download. Must be a number between 1 and 4.
#' @param path Path of the local directory where the PNADc files are stored or where you want to save the data.
#'
#' @importFrom PNADcIBGE get_pnadc
#' @importFrom PNADcIBGE read_pnadc
#' @importFrom PNADcIBGE pnadc_design
#' @importFrom PNADcIBGE pnadc_labeller
#'
#' @examples
#'\dontrun{
#' pnadc_download(2018, 1)
#'}
pnadc_download <- function(year, quartile, path = NULL) {
  if (is.null(path)) {
    file_name <- paste("Design_PNADc", year, quartile, sep = "_")
    if (file.exists(fs::path_home(file_name))) {
      message("This edition of PNADc has already been downloaded and can be used.")
    } else {
      design_PNADc <- PNADcIBGE::get_pnadc(year, quarter = quartile, design = TRUE)
      saveRDS(design_PNADc, file = fs::path_home(file_name))
      message(paste("File saved in directory:", fs::path_home(), sep = ""))
    }
  } else {
      file_name <- paste(path, "/Design_PNADc_", year, "_", quartile, sep = "")
      if (file.exists(file_name)) {
        message("This edition of PNADc has already been downloaded and can be used.")
        saveRDS(path, file = fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_")))
        message(paste("File saved in directory:", path, sep = ""))
      } else {
        pnad_txt_file <- paste(path, "/PNADC_0", quartile, year, ".txt", sep = "")
        input_sas_file <- paste(path, "/input_PNADC_trimestral.sas", sep = "")

        if (file.exists(pnad_txt_file)) {
          pnadc <- PNADcIBGE::read_pnadc(pnad_txt_file, input_sas_file)
          design_PNADc <- PNADcIBGE::pnadc_design(pnadc)
        } else {
          design_PNADc <- PNADcIBGE::get_pnadc(year, quarter = quartile, design = TRUE)
        }

      saveRDS(design_PNADc, file = file_name)
      saveRDS(file_name, file = fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_")))
      message(paste("File saved in directory:", file_name, sep = ""))
    }
  }
}
