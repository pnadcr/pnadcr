# PNADc Grafico -----------------------------------------------------------
#' pnadc_plot
#'
#' @description Creates PNADc bar or dot graphics for one or more variables. Tables can be generated in \R or exported using the "export" option
#' @description [Documentation in English](https://github.com/migux14/PNADc.table/tree/main/vignettes)
#' @description [Documentation in Portuguese - BR](https://github.com/migux14/PNADc.table/tree/main/Documents%20PT-BR)
#'
#' @param variable Variable of interest that will be used to calculate the Total. It can be a vector of variables.
#' @param by Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation.
#' @param year The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept an array of quartiles.
#' @param calculation calculation what you want to do. Must be mean, total or percentage and come between "". It does not accept a list of calculations.
#' @param classifier One of the variables that were used in the 'variable' or 'by' parameter that will serve as a classifier by in the legend and graph. It must come between "".
#' @param path Path to the directory where the Design was created through the function "pnadc_download".
#' @param export Export the chart to "pdf" and "png" formats. If it has not been filled, it returns a ggplot2 graph in R space. It must be a string and be enclosed in " ".
#' @param type Type of chart you want to generate. 1 for Bar Graph and 2 for Dot Graph. Does not accept an array of types.
#'
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 guide_axis
#' @importFrom ggplot2 aes_string
#'
#' @return ggplot2 graphic
#'
#' @examples \donttest{pnadc_plot(~V403312, ~UF, 2019, 1, calculation = "mean")}
pnadc_plot <- function(variable, by, year, quartile, calculation, classifier = FALSE, path = FALSE, export = FALSE, type = 1) {

  load_design <- function(year, quartile, path) {
    design_file <- fs::path_home(paste("Design_PNADc", year, quartile, sep = "_"))

    if (!file.exists(design_file)) {
      pnadc_download(year = year, quartile = quartile, path = path)
    }

    load(design_file)
    return(design_PNADc)
  }

  get_table <- function(variable, by, design) {
    func_svy <- ifelse(calculation %in% c("mean", "percentage"), survey::svymean, survey::svytotal)

    tabela <- survey::svyby(
      formula = variable,
      by = by,
      design = design,
      FUN = func_svy,
      na.rm = TRUE,
      na.rm.by = TRUE,
      na.rm.all = TRUE
    )

    tabela <- as.data.frame(tabela)

    if (fil[2] == classifier) {
      return(tabela)
    } else {
      tabela <- tabela[, c(fil[2], var[2], classifier)]
      return(tabela)
    }
  }

  create_graph <- function(tabela, type) {
    grupo <- as.character(by)
    grupo <- unlist(strsplit(grupo, split = " "))

    if (type == 1) {
      graph_final <- tabela %>%
        ggplot2::ggplot(aes_string(x = fil[2], y = var[2], fill = classifier)) +
        geom_col() +
        facet_grid() +
        guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))
    } else if (type == 2) {
      graph_final <- tabela %>%
        ggplot2::ggplot(aes_string(x = fil[2], y = var[2], color = classifier)) +
        geom_point() +
        facet_grid() +
        guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))
    }

    return(graph_final)
  }

  save_graph <- function(graph_final, export) {
    if (export == "pdf" || export == "png") {
      ggplot2::ggsave(
        filename = paste("Grafico_PNADcTABLE_mean_", year, "_", quartile, ".", export, sep = ""),
        plot = graph_final,
        path = fs::path_home()
      )
      message(paste("Saved in directory: ", fs::path_home(), sep = ""))
    } else {
      message("No export format was selected, the graph was generated in R environment")
    }
  }

  var <- as.character(variable)
  fil <- as.character(by)
  fil <- unlist(strsplit(fil, split = " "))

  if (length(fil) > 3 && classifier == FALSE) {
    classifier <- fil[4]
  }

  if (classifier == FALSE) {
    classifier <- fil[2]
  }

  if (path == FALSE) {
    design_PNADc <- load_design(year, quartile)
  } else {
    design_PNADc <- load_design(year, quartile, path)
  }

  tabela <- get_table(variable, by, design_PNADc)

  graph_final <- create_graph(tabela, type)

  if (export == FALSE) {
    return(graph_final)
  } else {
    save_graph(graph_final, export)
    return(graph_final)
  }
  gc()
}
