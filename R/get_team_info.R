get_team_info <- function(website) {
  page <- rvest::read_html(website)

  team_id <- .get_website_id(website)

  info_part <- page %>%
    rvest::html_elements("div[id^='name']") %>%
    rvest::html_elements("h1")

  names <- info_part %>%
    rvest::html_text() %>%
    stringr::str_split("\n") %>%
    .[[1]] %>%
    trimws() %>%
    Filter(function(x) x != "", .)

  league_link <- info_part %>%
    rvest::html_element("a[href*='league']") %>%
    rvest::html_attr("href")

  final_table <- data.frame(
    team_id = team_id,
    team_name = names[1],
    league_name = names[2],
    league_link = league_link
  )

  return(final_table)
}
