get_league_skaters <- function(website = NULL, league = "NHL", season = "2022-2023") {
  #input checks
  if (is.null(website) & is.null(league) & is.null(season)) {
    stop("Please use either the `website` parameter, or the `league` and `season` parameters, not both")
  }
  if (!is.null(website) & (!is.null(league) | !is.null(season))) {
    stop("Please use either the `website` parameter, or the `league` and `season` parameters, not both")
  }
  if (is.null(website) & (is.null(league) | is.null(season))) {
    stop("Please supply both `league` and `season` parameters")
  }

  if (!is.null(league) & !is.null(season)) {
    website <- paste0("https://www.eliteprospects.com/league/",
                      tolower(league), "/stats/", season)
  }

  #getting number of pages
  page <- rvest::read_html(website)
  number_of_pages <- page %>%
    rvest::html_elements("div[id='skater-stats']") %>%
    rvest::html_elements("div[class='table-pagination']") %>%
    rvest::html_text() %>%
    stringr::str_extract("[0-9]+ players found") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric() %>%
    magrittr::divide_by(100) %>%
    ceiling()

  full_player_list <- purrr::map_df(1:number_of_pages, .f = function(page_num) {
    .get_league_skaters_helper(paste0(website, "?page=", page_num))
  })



}



.get_league_skaters_helper <- function(website) {

}
