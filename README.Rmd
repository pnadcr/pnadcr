---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# pnadcr

The goal of pnadcr is to make PNADc tables in an easy and interesting way

## Installation

You can install the development version of pnadcr from [GitHub](https://github.com/) with:
      

``` r
# install.packages("devtools")
devtools::install_github("pnadcr/pnadcr")
```

## Example

```{r example}
library(pnadcr)
pnadc_download(2019,1) #downloading the 2019 PNADc first quartile

pnadc_mean(~V403312, ~UF, 2019, 1) #Creating a table of mean monthly gross income by UF (Federation Unit) from the 2019.1 PNADc survey

```

Now let's make a bar graph of the last table

```{r example}
pnadc_plot(~V403312, ~UF, 2019, 1, calculation = "mean", classifier = "V403312", type = 1)
```

