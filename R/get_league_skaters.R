get_league_skaters <- function(website = NULL,
                               league = NULL,
                               season = NULL,
                               force = F) {

  function_call <- match.call()
  website <- .website_parameter_check(website, league, season, "league",
                                      function_call, "/stats/", force)

  #getting number of pages
  page <- rvest::read_html(website)
  number_of_pages <- page %>%
    rvest::html_elements("div[id='skater-stats']") %>%
    rvest::html_elements("div[class='table-pagination']") %>%
    rvest::html_text() %>%
    gsub(" ", "", .) %>%
    stringr::str_extract("[0-9]+playersfound") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric() %>%
    magrittr::divide_by(100) %>%
    ceiling()

  if (length(number_of_pages) == 0) number_of_pages <- 1

  full_skater_list <- purrr::map_df(1:number_of_pages, .f = function(page_num) {
    suppressWarnings(
      .get_league_players_helper(paste0(website, "?page=", page_num), "skater")
    )
  })

  return(full_skater_list)
}
