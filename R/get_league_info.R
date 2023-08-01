get_league_info <- function(website = NULL,
                            league = NULL) {

  function_call <- match.call()

  website <- .website_parameter_check(website, league, "", "league",
                                      function_call, "")

  page <- rvest::read_html(website)

  link_component <- gsub(".*/", "", website)

  full_name <- page %>%
    rvest::html_elements("h1") %>%
    rvest::html_text() %>%
    trimws()

  if (length(full_name) == 0) {
    full_name <- page %>%
      rvest::html_elements("div[class='ep-card']") %>%
      rvest::html_elements("div[class='row']") %>%
      rvest::html_text() %>%
      trimws()
  }

  abbreviation <- page %>%
    rvest::html_elements("small") %>%
    rvest::html_text() %>%
    trimws()
  if (length(abbreviation) != 1) abbreviation <- NA

  return(data.frame(full_name, abbreviation, link_component))
}
