get_league_skaters <- function(website = NULL, league = "NHL", season = "2022-2023") {

  function_call <- match.call()
  website <- .league_parameter_check(website, league, season, function_call, "/stats/")

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
      .get_league_skaters_helper(paste0(website, "?page=", page_num))
    )
  })

  return(full_skater_list)

}



.get_league_skaters_helper <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='skater-stats']") %>%
    rvest::html_elements("table")

  temp_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1)

  rename_temp <- .rename_df_helper(temp_table)
  numeric_cols <- rename_temp[[2]]

  skater_table <- rename_temp[[1]] %>%
    dplyr::rename(rank = `#`) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(player = gsub("\\(.*\\)", "", player)) %>%
    dplyr::filter(games_played != "")

  player_links <- .get_table_links(table_setup, "player")
  skater_table <- dplyr::mutate(skater_table, player_link = player_links)
  char_vec <- c("player", "player_link")

  if ("team" %in% names(skater_table)) {
    team_links <- .get_table_links(table_setup, "team")
    skater_table <- dplyr::mutate(skater_table, team_link = team_links)
    char_vec <- c(char_vec, "team", "team_link")
  }

  full_skater_table <- skater_table %>%
    dplyr::select(rank, tidyselect::all_of(char_vec), tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(char_vec), as.character),
                  dplyr::across(tidyselect::all_of(char_vec), trimws),
                  dplyr::across(games_played:tidyselect::last_col(), as.numeric))

  return(full_skater_table)
}
