# PNADc faixas -----------------------------------------------------------
#' pnadc_group
#'
#' @description Regroups a numeric or non-numeric variable from the PNADc for one or more variables. Tables can be generated in \R or exported using the "export" option
#' @description [Documentation in English](https://github.com/pnadcr/pnadcr/tree/master/documentation)
#' @description [Documentation in Portuguese - BR](https://github.com/pnadcr/pnadcr/tree/master/documentation/PT-BR)
#'
#' @param variable Variable of interest that will be used to calculate the data. It must be a formula, that is, have ~ in front of the variable.
#' @param by Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation. It must be a formula, i.e. have ~ in front of the variable.
#' @param year The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept a vector of quartiles.
#' @param calculation calculation what you want to do. Must be mean, total or percentage and come between "". It does not accept a list of calculations.
#' @param group Variable used to group a set of data. It must be one of the variables listed in the "by". Must be a formula, that is, have ~ in front of the variable.
#' @param cluster How the grouping strip will be made. In the numerical case it must be a vector calculation the minimum, maximum and size of the range. In the categorical case it must be a list calculation the old names and the new name that will replace them.
#' @param path Path to the directory where the Design was created through the function "pnadc_download".
#' @param export Export the table to "html", "pdf", "png" and "rtf" formats. If you want to return a data frame in R space, use "df". If it has not been filled, it returns a gt table in R space. It must be a string and be enclosed in " ".
#'
#' @importFrom gt tab_header
#' @importFrom gt md
#' @importFrom gt tab_options
#' @importFrom gt tab_source_note
#' @importFrom gt sub_missing
#' @importFrom utils write.csv2
#' @importFrom gt gtsave
#' @importFrom stats as.formula
#'
#' @return gt table
pnadc_group <- function(variable, by, year, quartile, calculation, group, cluster, path = FALSE, export = FALSE) {
  load_design <- function(year, quartile, path) {
    design_file <- fs::path_home(paste("Design_PNADc", year, quartile, sep = "_"))
    if (!file.exists(design_file)) {
      pnadc_download(year = year, quartile = quartile, path = path)
    }
    load(design_file)
    return(design_PNADc)
  }

  get_table <- function(variable, by, design, calculation) {
    func_svy <- ifelse(calculation %in% c("mean", "percentage"), survey::svymean, survey::svytotal)

    formula <- as.formula(paste("~group_col_df$group_col", sep = " "))

    tabela <- survey::svyby(
      formula = variable,
      by = formula,
      design = design,
      FUN = func_svy,
      na.rm = TRUE,
      na.rm.by = TRUE,
      na.rm.all = TRUE)

    return(as.data.frame(tabela))
  }

  create_table <- function(variable, by, year, quartile, tabela) {
    group_name_col <- if (length(by) > 3) by[4] else NULL
    tabela_final <- gt::gt(tabela, groupname_col = group_name_col) %>%
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
      gt::sub_missing(columns = 1:ncol(tabela), missing_text = "Total")
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

  by_char <- as.character(by)
  group_char <- as.character(group)
  #group_col <- as.factor(group_col)
  #group_col_df <- as.data.frame(group_col)

  if (!is.null(levels(design_PNADc$variables[, group_char[2]]))) {
    group_col <- as.character(design_PNADc$variables[, group_char[2]])
    group_col_df <- as.data.frame(group_col)
    for (i in 1:length(cluster)) {
      group_col_df[group_col_df$group_col %in% cluster[[i]],1] <- names(cluster[i])
    }
  } else {
    group_col <- cut(design_PNADc$variables[, group_char[2]],
                     c(seq(cluster[1], cluster[2], by = cluster[3]), max(design_PNADc$variables[, group_char[2]])),
                     include.lowest = TRUE,
                     right = FALSE)
    group_col_df <- as.data.frame(group_col)
  }

  tabela <- get_table(variable, by, design_PNADc, calculation)
  tabela_final <- create_table(variable, by, year, quartile, tabela)

  if (export == FALSE) {
    return(tabela_final)
  } else if (export == "df") {
    return(tabela)
  } else {
    save_table(tabela_final, path, year, quartile, export)
    return(tabela_final)
  }
}
