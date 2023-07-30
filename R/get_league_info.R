get_league_info <- function(website) {
  page <- rvest::read_html(website)

  link_component <- gsub(".*/", "", website)

  full_name <- page %>%
    rvest::html_elements("h1") %>%
    rvest::html_text() %>%
    trimws()

  abbreviation <- page %>%
    rvest::html_elements("small") %>%
    rvest::html_text() %>%
    trimws()

  return(data.frame(full_name, abbreviation, link_component))
}
