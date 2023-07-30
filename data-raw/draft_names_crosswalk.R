## code to prepare `draft_names_crosswalk` dataset goes here

page <- rvest::read_html("https://www.eliteprospects.com/draft/nhl-entry-draft")

get_draft_links_internal_data <- function(option_node) {
  draft_name <- option_node %>%
    rvest::html_text()
  link_component <- option_node %>%
    rvest::html_attr("value") %>%
    stringr::str_split("/") %>%
    magrittr::extract2(1) %>%
    tail(., 1)
  return(data.frame(draft_name, link_component))
}

draft_names_crosswalk <- page %>%
  rvest::html_elements("select") %>%
  Filter(function(x) {  # filtering to the one with draft websites
    rvest::html_children(x) %>%
      magrittr::extract(2) %>%
      rvest::html_attr("value") %>%
      grepl("www.+com", .)
  }, .) %>%
  rvest::html_elements("option[value]") %>%
  purrr::map_df(., get_draft_links_internal_data) %>%
  dplyr::mutate_all(trimws)

rm(page, get_draft_links_internal_data)

usethis::use_data(draft_names_crosswalk, overwrite = TRUE)
