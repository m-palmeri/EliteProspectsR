get_league_skaters <- function(website = NULL, league = "NHL", season = "2022-2023") {

  function_call <- match.call()
  # setting league and season to NULL if only website is specified
  if ("website" %in% names(function_call) &
      all(!(c("league", "season") %in% names(function_call)))) {
    league <- NULL
    season <- NULL
  }

  #input checks
  if (is.null(website) & is.null(league) & is.null(season)) {
    stop("Please specify either the full website, or the league and season")
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
    gsub(" ", "", .) %>%
    stringr::str_extract("[0-9]+playersfound") %>%
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
                  penalty_minutes = PIM) %>%
    dplyr::rename_with(tolower)
  names(skater_table) <- gsub("\\+/-", "plus_minus", names(skater_table))

  player_links <- .get_table_links(table_setup, "player")
  skater_table <- dplyr::mutate(skater_table, player_link = player_links)
  char_vec <- c("player", "player_link")

  if ("team" %in% names(skater_table)) {
    team_links <- .get_table_links(table_setup, "team")
    skater_table <- dplyr::mutate(skater_table, team_link = team_links)
    char_vec <- c(char_vec, "team", "team_link")
  }

  full_skater_table <- skater_table %>%
    dplyr::filter(games_played != "") %>%
    dplyr::select(rank, tidyselect::all_of(char_vec), tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(char_vec), as.character),
                  dplyr::across(games_played:tidyselect::last_col(), as.numeric))

  return(full_skater_table)
}
