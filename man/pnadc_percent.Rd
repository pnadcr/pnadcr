% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pnadc_percent.R
\name{pnadc_percent}
\alias{pnadc_percent}
\title{percent PNADc}
\usage{
pnadc_percent(variable, by, year, quartile, path = FALSE, export = FALSE)
}
\arguments{
\item{variable}{Variable of interest that will be used to calculate the percentage.}

\item{by}{Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation.}

\item{year}{The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.}

\item{quartile}{The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept a vector of quartiles.}

\item{path}{Path to the directory where the Design was created using the "pnadc_download" function.}

\item{export}{Export the table to "html", "pdf", "png" and "rtf" formats. If you want to return a data frame in R space, use "df". If it has not been filled, it returns a gt table in R space. It must be a string and be enclosed in " ".}
}
\value{
gt table.
}
\description{
Creates PNADc percent tables for one or more variables. Tables can be generated in \R or exported using the "export" option

\href{https://github.com/migux14/PNADc.table/tree/main/vignettes}{Documentation in English}

\href{https://github.com/migux14/PNADc.table/tree/main/Documents\%20PT-BR}{Documentation in Portuguese - BR}
}
\examples{
\donttest{pnadc_percent(~V3003A, ~UF + V2007, 2019, 1)}
}
