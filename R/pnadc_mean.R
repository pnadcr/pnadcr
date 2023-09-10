# Tabela mean ------------------------------------------------------------
#' pnadc_mean
#'
#' @description Creates PNADc mean tables for one or more variables. Tables can be generated in \R or exported using the "export" option
#' @description [Documentation in English](https://github.com/pnadcr/pnadcr/tree/master/documentation)
#' @description [Documentation in Portuguese - BR](https://github.com/pnadcr/pnadcr/tree/master/documentation/PT-BR)
#'
#' @param variable Variable of interest that will be used to calculate the mean.
#' @param by Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation.
#' @param year The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept a vector of quartiles.
#' @param path Location/directory where you want to save the final table. If you have already used the "pnadc_download" function to create the design in a specific path, just use the logical value True.
#' @param export Export the table to "html", "pdf", "png" and "csv" formats. If you want to return a data frame in R space, use "df". If it has not been filled, it returns a gt_table in R space. It must be a string and be enclosed in " ".
#'
#' @importFrom gt tab_header
#' @importFrom gt md
#' @importFrom gt tab_options
#' @importFrom gt tab_source_note
#' @importFrom gt sub_missing
#' @importFrom utils write.csv2
#' @importFrom gt gtsave
#'
#' @return gt table.
#'
#' @examples \donttest{pnadc_mean(~V403312, ~UF+V2010, 2019, 1)}
pnadc_mean <- function(variable, by, year, quartile, path = FALSE, export = FALSE) {

  load_design <- function(year, quartile, path) {
    design_file <- fs::path_home(paste("Design_PNADc", year, quartile, sep = "_"))
    if (file.exists(design_file)) {
      load(design_file)
      return(design_PNADc)
    } else {
      pnadc_download(year = year, quartile = quartile, path = path)
      load(design_file)
      return(design_PNADc)
    }
  }

  get_table <- function(variable, by, design) {
    tabela <- survey::svyby(
      formula = variable,
      by = by,
      design = design,
      FUN = survey::svymean,
      na.rm = TRUE,
      na.rm.by = TRUE,
      na.rm.all = TRUE
    )
    return(as.data.frame(tabela))
  }

  create_table <- function(variable, by, year, quartile, tabela) {
    grupo <- as.character(by)
    grupo <- unlist(strsplit(grupo, split = " "))

    tabela_final <- if (length(grupo) > 3) {
      gt::gt(tabela, groupname_col = grupo[4])
    } else {
      gt::gt(tabela)
    }

    tabela_final <- tabela_final %>%
      gt::tab_header(title = gt::md(paste(variable))) %>%
      gt::tab_options(
        table.align = "left",
        heading.background.color = "lightblue3",
        column_labels.background.color = "lightcyan",
        stub.background.color = "lightcyan",
        source_notes.background.color = "lightcyan"
      ) %>%
      gt::tab_source_note(
        source_note = gt::md(paste("Dados da PNADc ", year, ".", quartile, sep = ""))
      ) %>%
      gt::sub_missing(columns = 1:ncol(tabela), missing_text = "General mean")

    return(tabela_final)
  }

  save_table <- function(tabela_final, path, year, quartile, export) {
    if (export == "csv") {
      utils::write.csv2(
        tabela_final,
        file = fs::path_home(paste("Tabela_pnadc_mean_", year, "_", quartile, ".csv", sep = "")),
        fileEncoding = "UTF-8"
      )
      message(paste("Saved in directory: ", fs::path_home(), sep = ""))
    } else {
      gt::gtsave(
        tabela_final,
        filename = paste("Tabela_pnadc_mean_", year, "_", quartile, ".", export, sep = ""),
        path = path
      )
      message(paste("Saved in directory: ", path, sep = ""))
    }
  }

  if (path == FALSE) {
    design_PNADc <- load_design(year, quartile)
  } else {
    design_PNADc <- load_design(year, quartile, path)
  }

  tabela <- get_table(variable, by, design_PNADc)

  tabela_final <- create_table(variable, by, year, quartile, tabela)

  if (export == FALSE) {
    return(tabela_final)
  }

  if (export == FALSE) {
    return(tabela_final)
  } else if (export == "df") {
    return(tabela)
  } else {
    save_table(tabela_final, path, year, quartile, export)
    return(tabela_final)
  }
  gc()
}
