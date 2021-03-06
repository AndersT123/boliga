% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boliga.R
\name{save_to_csv}
\alias{save_to_csv}
\title{Save to CSV file}
\usage{
save_to_csv(
  list_with_dfs,
  file = paste0("listing-", lubridate::today(), ".csv"),
  complete_cases = T
)
}
\arguments{
\item{list_with_dfs}{a list with a list called result. list_with_dfs$result is a list of tibbles with length equal to the number of pages scraped.}

\item{file}{path to filename.}

\item{complete_cases}{removes the NAs from the data.}
}
\description{
Write data to csv file. Expects a list with a slot named result. This slot contains all the scraped pages from scrape_all_pages with a tibble for each page.
}
