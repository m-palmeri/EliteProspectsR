get_league_goalies <- function(website = NULL, league = "NHL", season = "2022-2023") {

  function_call <- match.call()
  website <- .league_parameter_check(website, league, season, function_call)

  # getting number of pages
  page <- rvest::read_html(website)
  number_of_pages <- page %>%
    rvest::html_elements("div[id='goalie-stats']") %>%
    rvest::html_elements("div[class='table-pagination']") %>%
    rvest::html_text() %>%
    gsub(" ", "", .) %>%
    stringr::str_extract("[0-9]+goaliesfound") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric() %>%
    magrittr::divide_by(100) %>%
    ceiling()

  full_goalie_list <- purrr::map_df(1:number_of_pages, .f = function(page_num) {
    suppressWarnings(
      .get_league_goalie_helper(paste0(website, "?sort-goalie-stats=svp&page-goalie=", page_num))
    )
  })

  return(full_goalie_list)
}



.get_league_goalie_helper <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='goalie-stats']") %>%
    rvest::html_elements("table")

  goalie_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::rename(rank = `#`,
                  games_played = GP,
                  goals_against_average = GAA,
                  save_percentage = `SV%`,
                  wins = W,
                  loses = L,
                  ties = `T`,
                  shutouts = SO,
                  time_on_ice = TOI,
                  saves = SVS,
                  goals_against = GA) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(games_played != "")

  player_links <- .get_table_links(table_setup, "player")
  goalie_table <- dplyr::mutate(goalie_table, player_link = player_links)
  char_vec <- c("player", "player_link")

  if ("team" %in% names(goalie_table)) {
    team_links <- .get_table_links(table_setup, "team")
    goalie_table <- dplyr::mutate(goalie_table, team_link = team_links)
    char_vec <- c(char_vec, "team", "team_link")
  }

  full_goalie_table <- goalie_table %>%
    dplyr::select(rank, tidyselect::all_of(char_vec), tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(char_vec), as.character),
                  dplyr::across(games_played:tidyselect::last_col(), as.numeric))

  return(full_goalie_table)
}
