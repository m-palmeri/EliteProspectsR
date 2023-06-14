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
    suppressWarnings(
      .get_league_skaters_helper(paste0(website, "?page=", page_num))
    )
  })

}



.get_league_skaters_helper <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='skater-stats']") %>%
    rvest::html_elements("table")

  skater_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::rename(rank = `#`,
                  games_played = GP,
                  goals = G,
                  assists = A,
                  points = TP,
                  penalty_minutes = PIM,
                  plus_minus = `+/-`) %>%
    dplyr::rename_with(tolower)

  player_links <- .get_league_skater_links(table_setup, "player")

  team_links <- .get_league_skater_links(table_setup, "team")

  full_skater_table <- skater_table %>%
    dplyr::mutate(player_link = player_links,
                  team_link = team_links) %>%
    dplyr::filter(team != "") %>%
    dplyr::select(rank, player, player_link, team, team_link, tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(player:team_link, as.character),
                  dplyr::across(games_played:plus_minus, as.numeric))

  return(full_skater_table)
}


.get_league_skater_links <- function(table_setup, finder) {
  table_setup %>%
    rvest::html_elements("tbody") %>%
    rvest::html_elements("tr") %>%
    rvest::html_element(glue::glue("td[class='{finder}']")) %>%
    rvest::html_element(glue::glue("a[href*='{finder}']")) %>%
    rvest::html_attr("href")
}
