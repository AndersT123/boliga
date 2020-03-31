#' Scrape a single page from boliga
#'
#' The function will scrape a given page by providing the page number as argument.
#'
#' @param page_num 'page_num' should be an integer and is pasted to &page=
#'
#' @return a tibble with nine columns.
#' @importFrom magrittr %>%
#' @export
scrape_page <- function(page_num) {
  #browser()
  url_base <- "https://www.boliga.dk/resultat?propertyType=3,5&area=1&sizeMin=50&sizeMax=150"

  url_page <- paste0(url_base,"&page=",page_num)
  page_pointer <- xml2::read_html(url_page)
  page_content <- page_pointer %>%
    rvest::html_nodes(".text-primary , .primary-value") %>%
    rvest::html_text(trim = T)

  if(length(page_content) == 1) return(NULL)


  page_clean <- page_content %>% stringr::str_remove(stringr::regex("kort", ignore_case = T)) %>% .[. != ""]

  by_listing <- page_clean %>% split(., rep(1:(length(.)/4), each = 4))

  df <- by_listing %>% {
    tibble::tibble(
      address = purrr::map_chr(., `[[`, 2) %>% stringr::str_extract("værelser.+") %>%
        stringr::str_sub(10L,200L) %>%  stringr::str_extract(".+\\d{4}") %>% stringr::str_sub(1L, -7L), #from_1_to_m6(),
      zip_code = purrr::map_chr(., `[[`, 2) %>% stringr::str_extract("\\d{4}") %>% as.integer(),
      area    = purrr::map_chr(., `[[`, 2) %>% stringr::str_extract("\\d{4}.+") %>% stringr::str_sub(5, 100L) %>% stringr::str_trim(),
      type = purrr::map_chr(., `[[`, 2) %>% stringr::str_extract("[:alpha:]+"),
      rooms = purrr::map_chr(., `[[`, 2) %>% stringr::str_extract(".+værelser") %>% stringr::str_extract("\\d+") %>% as.integer(),
      listing_date = purrr::map_chr(., `[[`, 4) %>% stringr::str_extract(".+\\d{4}") %>% stringr::str_sub(10L, 100L) %>%
        stringr::str_replace_all(c("okt" = "oct", "maj" = "may")) %>% lubridate::dmy(),
      listing_price = purrr::map_chr(., `[[`, 3) %>% stringr::str_remove(".+%") %>%
        stringr::str_extract("\\d.+") %>%
        stringr::str_remove("kr\\.") %>%
        stringr::str_remove("[.]") %>% stringr::str_remove("[.]") %>% stringr::str_trim() %>% as.integer(),
      today = lubridate::today()
    ) %>% dplyr::mutate(time_on_market = as.numeric(today - listing_date))
  }
  df
}

#' Scrape pages silently
#' @return A list with four elements. The results slot contains the output from iterations without failure. Check warnings slot for possible problems for scraped pages.
scrape_all_pages_silently <- function() {
  num_pages <- get_num_pages()
  out <- purrr::map(seq_along(1:num_pages), purrr::quietly(scrape_page)) %>% purrr::transpose()
  out
}

#' Scrape pages silently
#' @return A list with four elements. The results slot contains the output from iterations without failure. Check errors for scraped pages where code failed.
scrape_all_pages <- function() {
  num_pages <- get_num_pages()
  out <- purrr::map(seq_along(1:num_pages), purrr::safely(print_iter(num_pages, delay_by(1, scrape_page)), quiet = F)) %>% purrr::transpose()
  out
}
#' Save to CSV file
#'
#' Write data to csv file. Expects a list with a slot named result. This slot contains all the scraped pages from scrape_all_pages with a tibble for each page.
#'
#' @param list_with_dfs a list with a list called result. list_with_dfs$result is a list of tibbles with length equal to the number of pages scraped.
#' @param file path to filename.
#' @param complete_cases removes the NAs from the data.
#' @export
save_to_csv <- function(list_with_dfs, file = paste0("listing-", lubridate::today(), ".csv"), complete_cases = T) {

  dfs <- purrr::reduce(list_with_dfs[["result"]], dplyr::bind_rows)

  if(complete_cases) dfs <- dfs[complete.cases(dfs),]

  readr::write_csv(dfs, file)
}

#' Load CSV files in working directory
#'
#' To plot the mean time on market by dates when data is scraped do something like this:
#' #ggplot(dfs_clean, aes(x = today, y = time_on_market)) + stat_summary(geom = "line", fun.y = "mean", fun.args = list(na.rm = T))
#'
#' @param concat Should the tibbles be concatenated? Default is FALSE and it returns a list.
#'
#'
#' @export
load_files <- function(concat = T) {
  files_to_load <- list.files(pattern = ".csv")
  dfs <- purrr::map(files_to_load, readr::read_csv) %>% purrr::set_names(nm = files_to_load)

  if(concat) dfs <- purrr::reduce(dfs, dplyr::bind_rows)
  dfs
}

#' Number of pages to scrape
#'
#' Find the number of pages to scrape
#'
#' @export
get_num_pages <- function() {
  url_base <- "https://www.boliga.dk/resultat?propertyType=3,5&area=1&sizeMin=50&sizeMax=150"
  num_pages <- xml2::read_html(url_base) %>%
    rvest::html_nodes(".jump-to-page .page-button") %>% rvest::html_text(trim = T) %>% as.integer()

  stopifnot(!is.na(num_pages), length(unique(num_pages)) == 1L)

  unique(num_pages)
}

delay_by <- function(delay, f) {
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}
print_iter <- function(num_pages, f) {
  i <- 1
  function(...) {
    if (i  == 1) {
      print(paste0("Fetching page number: ", i, " of ", num_pages))
    } else {
      print(paste0(i," of ", num_pages))
    }
    i <<- i + 1
    f(...)
  }
}
